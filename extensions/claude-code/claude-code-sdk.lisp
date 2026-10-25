;; https://docs.anthropic.com/ja/docs/claude-code/sdk

(uiop:define-package :lem-claude-code/claude-code-sdk
  (:use :cl
        :alexandria)
  (:export :*claude-binary*
           :make-options
           :query
           :receive))
(in-package :lem-claude-code/claude-code-sdk)

(defparameter *claude-binary* "claude" "Path to the Claude Code CLI executable.")

(defstruct (claude-code-options (:constructor make-options))
  allowed-tools
  max-thinking-tokens
  system-prompt
  append-system-prompt
  mcp-servers
  permission-mode
  continue-conversation
  resume
  max-turns
  disallowed-tools
  model
  permission-prompt-tool-name
  cwd
  settings
  add-dirs)

(defun json-encode (object &key (encode #'yason:encode))
  (with-output-to-string (out) (funcall encode object out)))

(defun construct-command (prompt options)
  `(,*claude-binary*
    "--output-format" "stream-json"
    "--verbose"
    "--print" ,prompt
    ,@(when (claude-code-options-system-prompt options)
        (list "--system-prompt" (claude-code-options-system-prompt options)))
    ,@(when (claude-code-options-append-system-prompt options)
        (list "--append-system-prompt" (claude-code-options-append-system-prompt options)))
    ,@(when (claude-code-options-allowed-tools options)
        (list "--allowedTools" (format nil "~{~A~^,~}" (claude-code-options-allowed-tools options))))
    ,@(when (claude-code-options-max-turns options)
        (list "--max-turns" (write-to-string (claude-code-options-max-turns options))))
    ,@(when (claude-code-options-disallowed-tools options)
        (list "--disallowedTools"
              (format nil "~{~A~^,~}" (claude-code-options-disallowed-tools options))))
    ,@(when (claude-code-options-model options)
        (list "--model" (claude-code-options-model options)))
    ,@(when (claude-code-options-permission-prompt-tool-name options)
        (list "--permission-prompt-tool" (claude-code-options-permission-prompt-tool-name options)))
    ,@(when (claude-code-options-permission-mode options)
        (list "--permission-mode" (claude-code-options-permission-mode options)))
    ,@(when (claude-code-options-continue-conversation options)
        (list "--continue"))
    ,@(when (claude-code-options-resume options)
        (list "--resume" (claude-code-options-resume options)))
    ,@(when (claude-code-options-settings options)
        (list "--settings" (claude-code-options-settings options)))
    ,@(when (claude-code-options-add-dirs options)
        (mapcan (lambda (dir) (list "--add-dir" (princ-to-string dir)))
                (claude-code-options-add-dirs options)))
    ,@(when (claude-code-options-mcp-servers options)
        (cond
          ((consp (claude-code-options-mcp-servers options))
           (list "--mcp-config"
                 (let ((yason:*parse-object-as-alist* t))
                   (json-encode
                    (acons "mcpServers"
                           (claude-code-options-mcp-servers options)
                           nil)
                    :encode #'yason:encode-alist))))
          (t
           (list "--mcp-config" (princ-to-string (claude-code-options-mcp-servers options))))))))

(defstruct session
  process
  thread)

(defun query (prompt &key (options (make-options)) callback error-callback)
  "Read newline-delimited CLI events on a worker, retaining every event in a batch."
  (let ((session (make-session))
        (command (construct-command prompt options)))
    (setf (session-thread session)
          (bt2:make-thread
           (lambda ()
             (handler-case
                 (let* ((process (uiop:launch-program command :directory (claude-code-options-cwd options)
                                                              :input nil :output :stream :error-output :stream))
                        (stderr "")
                        (completed nil)
                        (error-reader nil))
                   (setf (session-process session) process)
                   (unwind-protect
                        (progn
                          (setf error-reader
                                (bt2:make-thread
                                 (lambda ()
                                   (with-open-stream (stream (uiop:process-info-error-output process))
                                     (setf stderr (uiop:slurp-stream-string stream))))))
                          (with-open-stream (stream (uiop:process-info-output process))
                            (loop :for line := (read-line stream nil)
                                  :while line
                                  :unless (zerop (length line))
                                    :do (let ((value (yason:parse line)))
                                          (when (equal "result" (gethash "type" value))
                                            (setf completed t))
                                          (when callback (funcall callback value)))))
                          (let ((code (uiop:wait-process process)))
                            (bt2:join-thread error-reader)
                            (unless (zerop code)
                              (error "Claude exited (~D): ~A" code stderr))
                            (unless completed (error "Claude ended without a result: ~A" stderr))))
                     (when (uiop:process-alive-p process) (uiop:terminate-process process :urgent t))
                     (uiop:wait-process process)
                     (when error-reader (bt2:join-thread error-reader))))
               (error (condition)
                 (when error-callback (funcall error-callback condition)))))
           :name "Claude Code thread"))
    session))

;; example
(eval-when ()
  (let ((session (query "hello")))
    (loop :for message := (receive session)
          :until (equal "result" (gethash "type" message))
          :do (print message))))

#+(or)
(defmethod print-object ((object hash-table) stream)
  (print-unreadable-object (object stream :type t)
    (prin1 (alexandria:hash-table-alist object) stream)))
