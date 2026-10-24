;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Enterprise
;;;; Proprietary and Confidential — Yeditor Agentic & Forge Suite (see LICENSE-ENTERPRISE).

(defpackage :yeditor-enterprise-forge-tests
  (:use :cl :rove :yeditor-context :yeditor-enterprise/forge))
(in-package :yeditor-enterprise-forge-tests)

(defclass canned (yeditor-components/models:provider) ((reply :initarg :reply :reader reply))
  (:default-initargs :name "canned" :base-url "http://127.0.0.1" :default-model "canned"))

(defmethod yeditor-components/models:provider-generate ((p canned) prompt &key model system on-chunk)
  (declare (ignore prompt model system on-chunk))
  (reply p))

(defun temp-dir ()
  (uiop:ensure-directory-pathname
   (merge-pathnames (format nil "yeditor-forge-~D/" (random 1000000)) (uiop:default-temporary-directory))))

(defun forge (source &key (directory (temp-dir)))
  (let ((*forge-directory* directory))
    (forge-component "a request" (make-instance 'canned :reply source))))

(defparameter *valid*
  "(define-component forge-demo :version \"0.1.0\"
     :install (lambda (comp ctx) (declare (ignore ctx))
                (list (install-keybinding comp *global-keymap* \"C-c z z\" 'forward-char)
                      (install-variable comp '*forge-demo-flag* :on)
                      (install-hook comp '*find-file-hook* (lambda (&rest args) (declare (ignore args)) nil)))))")

(deftest test-forge-promotes-a-reversible-component
  (lem-fake-interface:with-fake-interface ()
    (let* ((directory (temp-dir))
           (before (state-fingerprint))
           (result (forge (format nil "```lisp~%~A~%```" *valid*) :directory directory)))
      (unwind-protect
           (progn
             (ok (eq (getf result :status) :promoted) (format nil "promoted (~A)" (getf result :reason)))
             (ok (probe-file (getf result :path)) "source saved under the forge directory")
             (ok (search "forge-demo" (uiop:read-file-string (getf result :path))) "saved without the code fence")
             (ok (gethash (getf result :name) *component-registry*) "registered as a component")
             (ok (equal (state-fingerprint) before) "editor state identical after the reversibility rounds")
             (install-component (getf result :name))
             (ok (eq (let ((p (lem:keymap-find lem:*global-keymap* (lem:parse-keyspec "C-c z z"))))
                       (and p (lem:prefix-suffix p)))
                     'lem:forward-char)
                 "the promoted component works when installed")
             (uninstall-component (getf result :name)))
        (unregister-component (getf result :name))
        (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore)))))

(defparameter *rejected-sources*
  (list (cons "without an inverse"
              "(define-component bad-a :install (lambda (comp ctx) (list (yeditor-tx:install-effect comp :x (lambda () nil)))))")
        (cons "not allowed"
              "(define-component bad-b :install (lambda (comp ctx) (delete-file \"/tmp/x\") (list)))")
        (cons "not allowed"
              "(define-component bad-c :install (lambda (comp ctx) (list (install-process comp (list \"rm\" \"-rf\" \"/\")))))")
        (cons "needs a lambda"
              "(define-component bad-d :install (lambda (comp ctx) (list (install-hook comp '*find-file-hook* 'delete-file))))")
        (cons "literals"
              "(define-component bad-e :install (lambda (comp ctx) (list (install-variable comp '*find-file-hook* '((delete-file . 0))))))")
        (cons "unreadable"
              "(define-component bad-f :install #.(delete-file \"/tmp/x\"))")
        (cons "exactly one form"
              "(define-component bad-g :install (lambda (comp ctx) (list))) (delete-file \"/tmp/x\")")
        (cons "must be (define-component"
              "(defun bad-h () (delete-file \"/tmp/x\"))")))

(deftest test-forge-rejects-unsafe-components
  (lem-fake-interface:with-fake-interface ()
    (loop :for (expected . source) :in *rejected-sources*
          :for directory := (temp-dir)
          :do (let ((result (forge source :directory directory)))
                (ok (eq (getf result :status) :discarded) (format nil "discarded: ~A" expected))
                (ok (search expected (or (getf result :reason) "")) (format nil "reason mentions ~S: ~A" expected (getf result :reason)))
                (ok (not (uiop:directory-exists-p directory)) "nothing saved")
                (ok (not (and (getf result :name) (gethash (getf result :name) *component-registry*)))
                    "nothing left registered")))
    (let ((*max-component-bytes* 20))
      (ok (search "limit" (getf (forge *valid*) :reason)) "oversized source rejected"))))

(defvar *leaked* nil)

(define-component irreversible-probe
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (yeditor-tx:install-effect comp :x
                                              (lambda () (push (lem:make-buffer (format nil "leak-~D" (random 100000))) *leaked*))
                                              :undo-thunk (lambda () nil)))))

(deftest test-reversibility-check-detects-leaks
  (lem-fake-interface:with-fake-interface ()
    (ok (not (yeditor-enterprise/forge::reversible-p 'irreversible-probe))
        "a component whose undo leaves a buffer behind fails the reversibility test")
    (mapc #'lem:kill-buffer *leaked*)
    (unregister-component 'irreversible-probe)))

(deftest rejected-duplicate-preserves-the-existing-component
  (lem-fake-interface:with-fake-interface ()
    (let* ((source "(define-component duplicate-forge :install (lambda (comp ctx) (declare (ignore comp ctx)) (list)))")
           (form (check-component-source source))
           (name (second form)))
      (eval form)
      (install-component name)
      (let ((entry (gethash name *component-registry*)))
        (unwind-protect
             (let ((result (forge source)))
               (ok (eq :discarded (getf result :status)))
               (ok (eq entry (gethash name *component-registry*)))
               (ok (eq :active (getf entry :status))))
          (when (gethash name *component-registry*)
            (uninstall-component name) (unregister-component name)))))))

(deftest reject-hidden-code-in-lambda-lists
  (ok (signals
       (check-component-source
        "(define-component hidden-code :install (lambda (comp ctx &aux (x (delete-file \"never-run\"))) (list)))")
       'forge-rejected)))

(deftest component-names-cannot-be-file-paths
  (dolist (source '("(define-component |../escape| :install (lambda (comp ctx) (list)))"
                    "(define-component cl:car :install (lambda (comp ctx) (list)))"))
    (ok (signals (check-component-source source) 'forge-rejected))))

(deftest metadata-is-data-and-promotion-never-overwrites
  (ok (signals
       (check-component-source
        "(define-component bad-metadata :version (install-variable 'x '*x* t) :install (lambda (comp ctx) (list)))")
       'forge-rejected))
  (lem-fake-interface:with-fake-interface ()
    (let* ((directory (temp-dir))
           (path (merge-pathnames "forge-demo.lisp" directory)))
      (ensure-directories-exist path)
      (with-open-file (out path :direction :output) (write-string "existing user source" out))
      (unwind-protect
           (let ((result (forge *valid* :directory directory)))
             (ok (eq :discarded (getf result :status)))
             (ok (string= "existing user source" (uiop:read-file-string path)))
             (ok (null (gethash (getf result :name) *component-registry*))))
        (uiop:delete-directory-tree directory :validate t)))))

(defclass thread-probe (canned)
  ((ui-thread :initarg :ui-thread :reader ui-thread)
   (generated-off-ui :initform nil :accessor generated-off-ui)))

(defmethod yeditor-components/models:provider-generate ((provider thread-probe) prompt &key model system on-chunk)
  (declare (ignore prompt model system on-chunk))
  (setf (generated-off-ui provider) (not (eq (ui-thread provider) (bt2:current-thread))))
  (reply provider))

(deftest generation-is-async-and-promotion-runs-on-ui
  (lem-fake-interface:with-fake-interface ()
    (let* ((*forge-directory* (temp-dir))
           (ui (bt2:current-thread))
           (provider (make-instance 'thread-probe :ui-thread ui :reply *valid*))
           (result nil) (callback-on-ui nil))
      (unwind-protect
           (progn
             (yeditor-enterprise/forge::forge-in-background
              "fixture" provider
              (lambda (value) (setf result value callback-on-ui (eq ui (bt2:current-thread)))))
             (ok (loop :repeat 500 :thereis result :do (lem:receive-event 0.01)))
             (ok (generated-off-ui provider))
             (ok callback-on-ui)
             (ok (eq :promoted (getf result :status))))
        (when (and result (gethash (getf result :name) *component-registry*))
          (uninstall-component (getf result :name)) (unregister-component (getf result :name)))
        (uiop:delete-directory-tree *forge-directory* :validate t :if-does-not-exist :ignore)))))

(deftest http-llm-promotes-valid-source-and-discards-invalid-source
  (lem-fake-interface:with-fake-interface ()
    (let* ((process (uiop:launch-program
                     (list "python3" (namestring (asdf:system-relative-pathname
                                                 "yeditor-components" "test-data/fake-ai-server.py")))
                     :output :stream))
           (line (read-line (uiop:process-info-output process)))
           (provider (make-instance 'yeditor-components/models:ollama-provider
                                   :base-url (format nil "http://127.0.0.1:~A" (subseq line (1+ (position #\Space line))))))
           (*forge-directory* (temp-dir)) (valid nil))
      (unwind-protect
           (progn
             (setf valid (forge-component "FORGE_FIXTURE_VALID" provider))
             (ok (eq :promoted (getf valid :status)))
             (ok (probe-file (getf valid :path)))
             (let ((invalid (forge-component "FORGE_FIXTURE_INVALID" provider)))
               (ok (eq :discarded (getf invalid :status)))
               (ok (search "without an inverse" (getf invalid :reason)))))
        (when (and valid (gethash (getf valid :name) *component-registry*))
          (uninstall-component (getf valid :name)) (unregister-component (getf valid :name)))
        (uiop:terminate-process process :urgent t)
        (uiop:wait-process process)
        (close (uiop:process-info-output process))
        (uiop:delete-directory-tree *forge-directory* :validate t :if-does-not-exist :ignore)))))
