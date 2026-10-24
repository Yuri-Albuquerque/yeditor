;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Enterprise
;;;; Proprietary and Confidential — Yeditor Agentic & Forge Suite (see LICENSE-ENTERPRISE).

(defpackage :yeditor-enterprise/forge
  (:use :cl :lem :yeditor-context)
  (:import-from :yeditor-components/models #:provider-generate)
  (:export :forge-component
           :check-component-source
           :forge-rejected
           :forge-rejected-reason
           :*forge-directory*
           :*max-component-bytes*
           :*reversibility-rounds*
           :*allowed-operators*
           :state-fingerprint
           :yeditor-forge
           :le-forge))
(in-package :yeditor-enterprise/forge)

;;;; le-forge (plan T18): natural language -> a tested, reversible editor component.
;;;; 1. ask the model for one define-component form, given the SDK and allowed effects;
;;;; 2. reject it statically unless it is small, a single define-component, calls only
;;;;    whitelisted operators (effects that carry their inverse, plus a harmless subset
;;;;    of Common Lisp), and gives any raw install-effect an :undo-thunk;
;;;; 3. register it and install/uninstall it several times, checking that the editor's
;;;;    observable state comes back identical every time;
;;;; 4. promote it (saved under ~/.yeditor/components/) or discard it with no residue.

(defparameter *forge-directory* (merge-pathnames ".yeditor/components/" (user-homedir-pathname)))
(defparameter *max-component-bytes* 16384)
(defparameter *reversibility-rounds* 100)

(define-condition forge-rejected (error)
  ((reason :initarg :reason :reader forge-rejected-reason))
  (:report (lambda (c s) (format s "Component rejected: ~A" (forge-rejected-reason c)))))

(defun reject (format-control &rest args)
  (error (quote forge-rejected) :reason (apply (function format) nil format-control args)))

;;; The sandbox package generated code is read into

(defpackage :yeditor-forge-sandbox
  (:use :cl :lem :yeditor-context :yeditor-effects))

(defparameter *allowed-operators*
  (append
   (list (quote lambda) (quote declare) (quote ignore) (quote ignorable) (quote function) (quote quote)
         (quote list) (quote list*) (quote cons) (quote append) (quote let) (quote let*) (quote progn)
         (quote if) (quote when) (quote unless) (quote cond) (quote and) (quote or) (quote not)
         (quote equal) (quote eq) (quote eql) (quote string=) (quote format) (quote concatenate)
         (quote first) (quote rest) (quote length)
         (quote define-component) (quote yeditor-tx:install-effect))
   ;; effects that change editor state only and carry their inverse; deliberately not
   ;; process, server, fdefinition, environment, LSP or grammar effects
   (list (quote yeditor-effects:install-keybinding) (quote yeditor-effects:install-key-removal)
         (quote yeditor-effects:install-variable) (quote yeditor-effects:install-editor-variable)
         (quote yeditor-effects:install-hook) (quote yeditor-effects:install-hook-removal)
         (quote yeditor-effects:install-theme) (quote yeditor-effects:install-timer)
         (quote yeditor-effects:install-buffer)))
  "Operators a generated component may call.")

(defun literal-value-p (form)
  "A self-evaluating datum that cannot smuggle code: string, number, character,
   keyword, T or NIL."
  (or (stringp form) (numberp form) (characterp form) (keywordp form) (member form (list t nil))))

(defun check-function-argument (form operator)
  "Hook and timer functions must be a lambda (checked like any code) or #' of an allowed
   operator; a quoted symbol would call an arbitrary function."
  (cond ((and (consp form) (eq (first form) (quote lambda))) (check-calls form))
        ((and (consp form) (eq (first form) (quote function))) (check-calls form))
        (t (reject "~S needs a lambda or #' of an allowed function, not ~S" operator form))))

(defun check-effect-arguments (form)
  (let ((operator (first form)))
    (cond ((eq operator (quote yeditor-effects:install-hook))
           (check-function-argument (fourth form) operator))
          ((eq operator (quote yeditor-effects:install-timer))
           (check-function-argument (fourth form) operator))
          ((member operator (list (quote yeditor-effects:install-variable)
                                  (quote yeditor-effects:install-editor-variable)))
           (unless (literal-value-p (fourth form))
             (reject "~S values must be literals (string, number, keyword, t, nil), not ~S"
                     operator (fourth form)))))))

(defun operator-allowed-p (symbol)
  (member symbol *allowed-operators*))

(defun check-lambda-list (arguments)
  "Generated callbacks use required arguments and an optional final &REST only."
  (unless (alexandria:proper-list-p arguments) (reject "invalid lambda list"))
  (labels ((variable-p (symbol)
             (and (symbolp symbol) symbol (not (keywordp symbol))
                  (not (eq symbol t))
                  (plusp (length (symbol-name symbol)))
                  (not (char= #\& (char (symbol-name symbol) 0))))))
    (loop :for tail :on arguments
          :for argument := (car tail)
          :do (cond ((eq argument '&rest)
                     (unless (and (= 2 (length tail)) (variable-p (second tail)))
                       (reject "invalid &rest lambda list"))
                     (return))
                    ((not (variable-p argument))
                     (reject "unsupported lambda argument ~S" argument))))))

(defun check-calls (form)
  "Walk FORM; reject any call whose operator is not allowed, and any raw install-effect
   without an :undo-thunk."
  (when (consp form)
    (unless (alexandria:proper-list-p form) (reject "improper or circular code form"))
    (let ((head (first form)))
      (cond ((eq head (quote quote)) nil)
            ((eq head (quote function))
             (let ((target (second form)))
               (if (consp target)
                   (check-calls target)
                   (unless (operator-allowed-p target)
                     (reject "calls ~S, which is not allowed" target)))))
            ((eq head (quote lambda))
             (check-lambda-list (second form))
             (mapc (function check-calls) (cddr form)))
            ((member head (list (quote let) (quote let*)))
             (dolist (binding (second form))
               (when (consp binding) (check-calls (second binding))))
             (mapc (function check-calls) (cddr form)))
            ((eq head (quote declare)) nil)
            ((symbolp head)
             (unless (operator-allowed-p head)
               (reject "calls ~S, which is not allowed" head))
             (when (and (eq head (quote yeditor-tx:install-effect))
                        (not (getf (cddddr form) :undo-thunk)))
               (reject "an effect without an inverse (install-effect with no :undo-thunk)"))
             (check-effect-arguments form)
             (mapc (function check-calls) (rest form)))
            (t (reject "unexpected form ~S" head))))))

(defun read-component-source (source)
  "Read SOURCE into the sandbox package with *read-eval* off; returns the single form."
  (when (> (length source) *max-component-bytes*)
    (reject "~D characters exceeds the ~D limit" (length source) *max-component-bytes*))
  (when (> (length (flexi-streams:string-to-octets source :external-format :utf-8))
           *max-component-bytes*)
    (reject "UTF-8 source exceeds the ~D byte limit" *max-component-bytes*))
  (let ((forms (handler-case
                   (let ((*read-eval* nil)
                         (*package* (find-package :yeditor-forge-sandbox))
                         (*readtable* (copy-readtable nil)))
                     (with-input-from-string (in source)
                       (loop :for form := (read in nil in) :until (eq form in) :collect form)))
                 (error (e) (reject "unreadable source: ~A" e)))))
    (unless (= (length forms) 1)
      (reject "expected exactly one form, got ~D" (length forms)))
    (let ((form (first forms)))
      (unless (and (alexandria:proper-list-p form) (consp form)
                   (eq (first form) (quote define-component)) (symbolp (second form)))
        (reject "the form must be (define-component NAME ...)"))
      (let ((name (second form)))
        (unless (and (eq (symbol-package name) (find-package :yeditor-forge-sandbox))
                     (plusp (length (symbol-name name)))
                     (every (lambda (char) (or (alphanumericp char) (find char "-_")))
                            (symbol-name name)))
          (reject "component name must be a local identifier, not a path or foreign symbol")))
      form)))

(defun check-component-source (source)
  "Validate the SDK schema before evaluating any generated code."
  (let* ((form (read-component-source source)) (options (cddr form)) (seen nil))
    (unless (and (alexandria:proper-list-p options) (evenp (length options)))
      (reject "malformed component options"))
    (loop :for (key value) :on options :by #'cddr
          :do (when (member key seen) (reject "duplicate option ~S" key))
              (push key seen)
              (case key
                ((:version :author)
                 (unless (stringp value) (reject "~S must be a literal string" key)))
                (:config-spec
                 (unless (or (literal-value-p value)
                             (and (alexandria:proper-list-p value) (consp value)
                                  (eq (first value) 'quote) (= 2 (length value))))
                   (reject "config-spec must be literal data")))
                (:install
                 (let ((function (if (and (consp value) (eq (first value) 'function))
                                     (second value) value)))
                   (unless (and (consp function) (eq (first function) 'lambda))
                     (reject "install must be a lambda"))
                   (check-calls value)))
                (otherwise (reject "unsupported option ~S" key))))
    (unless (getf options :install) (reject "no :install function"))
    form))

;;; Reversibility test

(defun state-fingerprint ()
  "Observable editor state that installing then uninstalling a component must restore."
  (let ((bindings nil))
    (traverse-keymap *global-keymap*
                     (lambda (keys command) (push (cons (format nil "~{~A~^ ~}" keys) command) bindings)))
    (list (sort (mapcar (function buffer-name) (buffer-list)) (function string<))
          (sort bindings (function string<) :key (function car))
          (copy-tree *find-file-hook*)
          (copy-tree *switch-to-buffer-hook*)
          (copy-tree *after-init-hook*)
          (current-theme)
          (variable-value (quote modeline-format) :global))))

(defun reversible-p (name)
  "Install and uninstall NAME *reversibility-rounds* times; true if the state comes back
   identical each time. Signals forge-rejected if installing fails."
  (let ((before (state-fingerprint)))
    (loop :repeat *reversibility-rounds*
          :do (handler-case (install-component name)
                (error (e) (reject "installing failed: ~A" e)))
              (uninstall-component name)
          :always (equal (state-fingerprint) before))))

;;; The forge loop

(defparameter *forge-prompt*
  "Write one Yeditor component in Common Lisp as a single form:
  (define-component NAME :version \"0.1.0\" :install (lambda (comp ctx) (declare (ignore ctx)) (list EFFECT...)))
Each EFFECT must be one of these calls (each registers its own inverse):
  (install-keybinding comp *global-keymap* \"C-c x\" 'some-existing-command)
  (install-variable comp 'symbol value)
  (install-editor-variable comp 'editor-variable value)
  (install-hook comp '*hook-variable* (lambda (&rest args) (declare (ignore args)) ...))
  (install-theme comp \"theme-name\")
Variable values must be literals (string, number, keyword, t or nil).
Use only those calls plus list, quote and lambda. Reply with the form only, no prose and no code fences.

Request: ~A")

(defun strip-fences (text)
  (let ((trimmed (string-trim '(#\Newline #\Space) text)))
    (if (uiop:string-prefix-p "```" trimmed)
        (let ((start (or (position #\Newline trimmed) 3))
              (end (or (search "```" trimmed :from-end t) (length trimmed))))
          (subseq trimmed start (max start end)))
        trimmed)))

(defun save-forged-source (source path)
  "Create a new source file; remove a partial file if writing or closing fails."
  (ensure-directories-exist path)
  (let ((created nil) (saved nil))
    (unwind-protect
         (progn
           (with-open-file (out path :direction :output :if-exists :error)
             (setf created t)
             (write-string source out)
             (terpri out))
           (setf saved t))
      (when (and created (not saved)) (delete-file path)))))

(defun forge-component (request provider &key (promote t))
  "Generate, check, test and promote a component for REQUEST using PROVIDER.
   Returns a plist :status (:promoted or :discarded) :name :path :reason :source."
  (forge-source (provider-generate provider (format nil *forge-prompt* request)) :promote promote))

(defun forge-source (generated-source &key (promote t))
  "Validate generated source and evaluate it only on the editor thread."
  (let* ((source (strip-fences generated-source))
         (name nil)
         (created nil))
    (handler-case
        (let ((form (check-component-source source)))
          (setf name (second form))
          (when (gethash name *component-registry*)
            (reject "a component named ~A already exists" name))
          (eval form)
          (setf created t)
          (unless (reversible-p name)
            (reject "the editor state was not restored after uninstalling"))
          (let ((path (merge-pathnames (format nil "~(~A~).lisp" (symbol-name name)) *forge-directory*)))
            (when promote (save-forged-source source path))
            (list :status :promoted :name name :path (and promote path) :source source)))
      (forge-rejected (e)
        (when (and created name (gethash name *component-registry*))
          (ignore-errors (uninstall-component name))
          (unregister-component name))
        (list :status :discarded :name name :reason (forge-rejected-reason e) :source source))
      (error (e)
        (when (and created name (gethash name *component-registry*))
          (uninstall-component name)
          (unregister-component name))
        (list :status :discarded :name name :reason (princ-to-string e) :source source)))))

(defun forge-in-background (request provider success)
  "Generate off the UI thread, then validate/promote and report on the UI thread."
  (bt2:make-thread
   (lambda ()
     (handler-case
         (let ((source (provider-generate provider (format nil *forge-prompt* request))))
           (send-event (lambda () (funcall success (forge-source source)))))
       (error (condition)
         (send-event (lambda () (message "Forge generation failed: ~A" condition))))))
   :name "yeditor-forge-generation"))

(define-command yeditor-forge (request) ((:string "Forge a component: "))
  "Generate a component asynchronously, then validate and promote it on the UI thread."
  (let ((provider (or yeditor-components/ai:*default-provider*
                      (editor-error "No AI provider configured"))))
    (forge-in-background
     request provider
     (lambda (result)
       (if (eq (getf result :status) :promoted)
           (message "Component created: ~(~A~)" (getf result :name))
           (message "Forge discarded the component: ~A" (getf result :reason)))))))

(define-component le-forge
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (yeditor-effects:install-keybinding comp *global-keymap* "C-c a f" (quote yeditor-forge)))))
