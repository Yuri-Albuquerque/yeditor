;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Enterprise
;;;; Proprietary and Confidential — Yeditor Agentic & Forge Suite (see LICENSE-ENTERPRISE).

(defpackage :yeditor-agent-ui-tests (:use :cl :rove :lem))
(in-package :yeditor-agent-ui-tests)

(deftest indentation-guides-only-change-display
  (let ((source "        value = 1"))
    ;; Exercise the renderer helper, including expanded tabs, without editing a buffer.
    (multiple-value-bind (display attributes) (lem-core::add-indentation-guides source nil 4)
      (ok (string= display "│   │   value = 1"))
      (ok (= 2 (length attributes)))
      (ok (string= source "        value = 1")))
    (ok (string= "  x" (lem-core::add-indentation-guides "  x" nil 4)))
    (multiple-value-bind (expanded attributes) (lem-core::expand-tab (format nil "~Cvalue" #\Tab) nil 4)
      (ok (string= "│   value" (lem-core::add-indentation-guides expanded attributes 4))))))

(deftest indentation-guides-follow-code-mode-and-toggle
  (lem-fake-interface:with-fake-interface ()
    (let* ((buffer (current-buffer))
           (point (buffer-start-point buffer))
           (mode (make-instance 'lem/language-mode:language-mode))
           (before (variable-value 'indentation-guides :global)))
      (insert-string point "        code")
      (setf (variable-value 'lem/language-mode:indent-size :buffer buffer) 4)
      (unwind-protect
           (progn
             (setf (variable-value 'indentation-guides :global) t)
             ;; White-box renderer integration: there is no public logical-line API.
             (ok (string= "│   │   code"
                          (lem-core::logical-line-string
                           (lem-core::create-logical-line point nil (list mode)))))
             (ok (string= "        code"
                          (lem-core::logical-line-string
                           (lem-core::create-logical-line point nil nil))))
             (lem-core::toggle-indentation-guides)
             (ok (string= "        code"
                          (lem-core::logical-line-string
                           (lem-core::create-logical-line point nil (list mode)))))
             (ok (string= "        code" (buffer-text buffer))))
        (setf (variable-value 'indentation-guides :global) before)))))

(deftest hermes-agent-opens-and-refocuses
  (lem-fake-interface:with-fake-interface ()
    (unwind-protect
         (progn
           (yeditor-enterprise/hermes-chat:hermes-agent)
           (let ((window (frame-bottomside-window (current-frame))))
             (ok window)
             (dotimes (i 100)
               (yeditor-enterprise/hermes-chat:hermes-agent))
             (ok (eq window (frame-bottomside-window (current-frame))))
             (ok (eq window (current-window)))))
      (yeditor-enterprise/hermes-chat:yeditor-hermes-chat-quit)
      (yeditor-context:uninstall-component 'yeditor-enterprise/hermes-chat:le-hermes-chat))))

(deftest claude-cli-reads-batched-events-and-reports-stderr
  (let* ((root (merge-pathnames (format nil "yeditor-cli-~D/" (random 10000000)) (uiop:temporary-directory)))
         (binary (merge-pathnames "claude" root)))
    (ensure-directories-exist binary)
    (with-open-file (out binary :direction :output)
      (write-line "#!/usr/bin/env python3" out)
      (write-line "import json, sys" out)
      (write-line "if 'empty' in sys.argv: sys.exit(0)" out)
      (write-line "if 'error' in sys.argv: print('login required', file=sys.stderr); sys.exit(2)" out)
      (write-line "sys.stdout.write(''.join(json.dumps({'type': t})+'\\n' for t in ['system','assistant','result']))" out))
    (uiop:run-program (list "chmod" "+x" (namestring binary)))
    (let ((lem-claude-code/claude-code-sdk:*claude-binary* (namestring binary)))
      (unwind-protect
           (progn
             (let ((events nil) (failure nil))
               (bt2:join-thread
                (lem-claude-code/claude-code-sdk::session-thread
                 (lem-claude-code/claude-code-sdk:query
                  "hello" :callback (lambda (value) (push (gethash "type" value) events))
                  :error-callback (lambda (condition) (setf failure condition)))))
               (ok (null failure))
               (ok (equal '("system" "assistant" "result") (reverse events))))
             (let ((failure nil))
               (bt2:join-thread
                (lem-claude-code/claude-code-sdk::session-thread
                 (lem-claude-code/claude-code-sdk:query
                  "error" :error-callback (lambda (condition) (setf failure condition)))))
               (ok (search "login required" (princ-to-string failure))))
             (let ((failure nil))
               (bt2:join-thread
                (lem-claude-code/claude-code-sdk::session-thread
                 (lem-claude-code/claude-code-sdk:query
                  "empty" :error-callback (lambda (condition) (setf failure condition)))))
               (ok (search "without a result" (princ-to-string failure)))))
        (uiop:delete-directory-tree root :validate t)))))
