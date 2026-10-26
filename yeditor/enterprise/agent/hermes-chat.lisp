;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Enterprise
;;;; Proprietary and Confidential — Yeditor Agentic & Forge Suite (see LICENSE-ENTERPRISE).

;;;; le-hermes-chat: chat panel talking to the local `hermes` CLI.
;;;; Each message runs `hermes chat -q <text> --format stream-json` in the
;;;; editor's working directory (project root preferred), streaming events into
;;;; the chat buffer, and resumes the same session on follow-ups so Hermes keeps
;;;; its context. Layout: every message stacks above a '> ' prompt that is
;;;; always the buffer's last line (so you can keep typing while the agent
;;;; works); the '...' thinking marker is replaced by the first output of the
;;;; turn; agent text is labeled [hermes]; tool call lines are dimmed.

(defpackage :yeditor-enterprise/hermes-chat
  (:use :cl :lem :yeditor-context :yeditor-effects)
  (:import-from :yeditor-enterprise/journal
                #:record-agent-event)
  (:import-from :alexandria
                #:appendf #:when-let)
  (:export :hermes-agent
           :le-hermes-chat
           :yeditor-hermes-chat-toggle
           :yeditor-hermes-chat-send
           :yeditor-hermes-chat-focus
           :yeditor-hermes-chat-quit
           :submit-external-intent
           :*hermes-chat-buffer-name*
           :*hermes-binary*))
(in-package :yeditor-enterprise/hermes-chat)

(defparameter *hermes-chat-buffer-name* "*yeditor-hermes*"
  "Name of the Hermes Agent chat panel buffer.")

(defparameter *hermes-binary* "hermes"
  "The Hermes Agent CLI binary.")

(lem:define-attribute hermes-tool-attribute
  (:light :foreground "#7c6f64")
  (:dark :foreground "#a89984"))

(defparameter *hermes-chat-session* nil
  "Hermes session id to resume on the next message, or nil.")

(defvar *hermes-chat-keymap* (make-keymap :description '*hermes-chat-keymap*
                                            :base *global-keymap*)
  "Keymap active in the Hermes chat buffer.")

(setf (documentation '*hermes-chat-keymap* 'variable)
      "Keymap in the Hermes chat panel. Return sends the line, C-j inserts a
newline for multiline prompts, C-c C-k closes the panel and C-c C-e jumps
back to the last editor buffer without closing the panel.")

(define-key *hermes-chat-keymap* "Return" 'yeditor-hermes-chat-send)
(define-key *hermes-chat-keymap* "C-j" 'newline)
(define-key *hermes-chat-keymap* "C-c C-k" 'yeditor-hermes-chat-quit)
(define-key *hermes-chat-keymap* "C-c C-e" 'yeditor-hermes-chat-focus-editor)

(defun hermes-chat-active-p ()
  (alexandria:when-let (win (frame-bottomside-window (current-frame)))
    (eq (window-buffer win) (get-buffer *hermes-chat-buffer-name*))))

(defun make-hermes-chat-buffer ()
  (let ((buffer (make-buffer *hermes-chat-buffer-name*)))
    (change-buffer-mode buffer 'hermes-chat-mode)
    buffer))

(defun ensure-hermes-chat-buffer ()
  "Create or reuse the Hermes chat buffer."
  (or (get-buffer *hermes-chat-buffer-name*)
      (make-hermes-chat-buffer)))

(define-major-mode hermes-chat-mode ()
    (:name "Hermes Chat"
     :keymap *hermes-chat-keymap*)
  (setf (variable-value 'line-wrap :buffer (current-buffer)) t))

(defun chat-insert (buffer text &key (attribute nil))
  "Insert one chat message line above the prompt (or at the buffer end when
there is no prompt yet, e.g. the greeting)."
  (when (and text (plusp (length text)))
    (with-buffer-read-only buffer nil
      (alexandria:if-let (out (chat-output-point buffer))
          (insert-string out (format nil "~A~%" text) :attribute attribute)
          (let ((point (buffer-end-point buffer)))
            (insert-string point (format nil "~A~%" text) :attribute attribute))))))

(defun chat-insert-running (buffer text)
  "Insert an activity line (tool call) with the dimmed tool attribute."
  (chat-insert buffer (format nil "  | ~A" text)
               :attribute 'hermes-tool-attribute))

;;;; prompt line handling: the '> ' prompt is always the last line of the
;;;; buffer. Every chat message is inserted at chat-output-point (the start of
;;;; the prompt line), which pushes the prompt down while keeping it last, so
;;;; the user can keep typing after '> ' while the agent streams its output.

(defun chat-prompt-point (buffer)
  "Point right after the '> ' prompt, where the user types, or nil."
  (buffer-value buffer 'prompt-marker))

(defun (setf chat-prompt-point) (point buffer)
  (setf (buffer-value buffer 'prompt-marker) point))

(defun chat-output-point (buffer)
  "Point at the start of the prompt line where chat messages are inserted.
Both markers are live :left-inserting points: inserts at the output point
keep the prompt line (and any in-progress typing) intact below them."
  (buffer-value buffer 'output-marker))

(defun (setf chat-output-point) (point buffer)
  (setf (buffer-value buffer 'output-marker) point))

(defun setup-prompt (buffer)
  "Add a '> ' prompt as the last line and record the live input/output points.
The input marker is :right-inserting (typing at its position leaves it before
the text), while the output marker is :left-inserting (inserting there leaves
it after the inserted line, so messages land in chronological order)."
  (alexandria:when-let (old (chat-prompt-point buffer))
    (delete-point old))
  (alexandria:when-let (old (chat-output-point buffer))
    (delete-point old))
  (let ((point (buffer-end-point buffer)))
    (unless (start-line-p point)
      (insert-character point #\newline))
    (insert-string point "> ")
    (let ((prompt (copy-point point :left-inserting)))
      (line-start prompt)
      (setf (chat-prompt-point buffer) (copy-point point :right-inserting)
            (chat-output-point buffer) prompt))))

(defun chat-input-text (buffer)
  "Return the typed input after the '> ' prompt, or nil."
  (alexandria:when-let ((marker (chat-prompt-point buffer)))
    (string-trim '(#\Space #\Tab #\Newline)
                 (points-to-string marker (buffer-end-point buffer)))))

(defun chat-clear-input (buffer)
  (let ((marker (chat-prompt-point buffer)))
    (when marker
      (with-buffer-read-only buffer nil
        (with-point ((s marker)
                     (e (buffer-end-point buffer)))
          (delete-between-points s e))))))

(defun chat-redraw-prompt (buffer)
  "Ensure a '> ' prompt line exists. The prompt, once set, is never removed
and slides along as messages are inserted above it, so this is only a real
setup for fresh buffers."
  (unless (and (chat-prompt-point buffer)
               (chat-output-point buffer))
    (setup-prompt buffer)))

(defun chat-start-pending (buffer)
  "Show the '...' turn-in-progress line above the prompt and record a point
at its start, so the first output of the turn can replace the line. While a
turn is already pending this does nothing (one marker at a time)."
  (unless (buffer-value buffer 'pending-marker)
    (with-buffer-read-only buffer nil
      (alexandria:if-let (out (chat-output-point buffer))
          ;; a :right-inserting copy taken at the insertion position does not
          ;; move when the text is inserted there, so PENDING stays at the
          ;; start of the '...' line while the prompt slides down.
          (let ((pending (copy-point out :right-inserting)))
            (insert-string out (format nil "...~%"))
            (setf (buffer-value buffer 'pending-marker) pending))
          (let ((point (buffer-end-point buffer)))
            (insert-string point (format nil "...~%")))))))

(defun chat-clear-pending (buffer)
  "Remove the '...' turn-in-progress line: the first real output of the turn
takes its place. No-op when the line was removed meanwhile."
  (alexandria:when-let (pending (buffer-value buffer 'pending-marker))
    (setf (buffer-value buffer 'pending-marker) nil)
    (when (alive-point-p pending)
      (with-buffer-read-only buffer nil
        (with-point ((s pending)
                     (e pending))
          (line-end e)
          (when (string= (points-to-string s e) "...")
            (when (line-offset e 1)
              (delete-between-points s e))))))))

;;;; hermes process

(defun hermes-chat-directory ()
  "Run hermes in the project root when known, else the buffer dir."
  (or (ignore-errors (namestring (lem-core/commands/project:find-root (buffer-directory))))
      (uiop:getcwd)))

(defun parse-stream-json-line (line buffer)
  "Turn one stream-json line into chat output."
  (alexandria:when-let ((event (ignore-errors (yason:parse line))))
    (let ((type (gethash "type" event)))
      (cond ((string= type "system")
             ;; the init system event carries an empty session_id; the real id
             ;; arrives on the "result" event, so only remember non-empty ids.
             (let ((sid (gethash "session_id" event)))
               (when (and sid (plusp (length sid)))
                 (setf *hermes-chat-session* sid))))
            ((string= type "text")
             (let ((text (gethash "text" event)))
               (when (and text (plusp (length text)))
                 ;; the first real output replaces the '...' marker
                 (chat-clear-pending buffer)
                 (chat-insert buffer (format nil "[hermes] ~A" text)))))
            ((string= type "tool")
             (chat-clear-pending buffer)
             (chat-insert-running buffer (or (gethash "summary" event)
                                             (gethash "name" event)
                                             "tool call")))
            ((string= type "result")
             (let ((sid (gethash "session_id" event)))
               (when (and sid (plusp (length sid)))
                 (setf *hermes-chat-session* sid)))
             (let ((text (gethash "text" event))
                   (exit (gethash "exit_code" event)))
               ;; a turn that emitted no text events ends silently here:
               ;; replace the thinking marker with the final text the result
               ;; event carries, if any.
               (when (buffer-value buffer 'pending-marker)
                 (chat-clear-pending buffer)
                 (if (and text (plusp (length text)))
                     (chat-insert buffer (format nil "[hermes] ~A" text))
                     (unless (and exit (not (zerop exit)))
                       (chat-insert buffer "(no output)"))))
               (unless (and exit (zerop exit))
                 (chat-insert buffer (format nil "(hermes exited with code ~A)" exit)))))))))

(defun run-hermes-query (buffer query)
  "Run one hermes chat turn, streaming output into BUFFER as it arrives."
  (let ((dir (hermes-chat-directory))
        (args (list "chat" "-q" query "--oneshot" "-Q" "--format" "stream-json")))
    (when *hermes-chat-session*
      (setf args (append args (list "--resume" *hermes-chat-session*))))
    (uiop:with-current-directory (dir)
      (let* ((process (uiop:launch-program (cons *hermes-binary* args)
                                           :output :stream
                                           :error-output :stream))
             (out (uiop:process-info-output process))
             (err (uiop:process-info-error-output process))
             ;; Drain stderr on a background thread so a full stderr pipe can
             ;; never block the stdout streaming loop below.
             (err-thread (bt2:make-thread (lambda () (uiop:slurp-stream-string err)))))
        ;; Stream stdout line-by-line, dispatching each event to the UI thread
        ;; as soon as it arrives instead of buffering the whole run.
        (loop :for line := (read-line out nil)
              :while line
              :do (let ((l line))
                    (send-event (lambda () (parse-stream-json-line l buffer)))))
        (uiop:wait-process process)
        (let ((stderr-text (bt2:join-thread err-thread)))
          (when (and stderr-text
                     (plusp (length (string-trim '(#\Space #\Newline) stderr-text))))
            (send-event
             (lambda ()
               (chat-insert buffer
                            (format nil "(stderr) ~A"
                                    (string-trim '(#\Newline) stderr-text)))))))))))

(defun run-hermes-query-async (buffer query)
  "Run hermes in a thread so the editor stays responsive."
  (bt2:make-thread
   (lambda ()
     (handler-case (run-hermes-query buffer query)
       (error (e)
         (send-event (lambda ()
                       (chat-clear-pending buffer)
                       (chat-insert buffer (format nil "[error] ~A" e))))))
     (send-event (lambda ()
                   (with-buffer-read-only buffer nil
                     (chat-redraw-prompt buffer))))
     (record-agent-event *hermes-chat-session* query
                         :plan (format nil "user asked: ~A" query)
                         :tool-calls nil
                         :observation "done"))
   :name "yeditor-hermes-chat"))

(defun submit-hermes-message (text)
  "Send TEXT to hermes and stream the response into the chat buffer."
  (let ((buffer (ensure-hermes-chat-buffer)))
    (chat-redraw-prompt buffer)
    (chat-insert buffer (format nil "[you] ~A" text) :attribute 'lem:modeline-name-attribute)
    (chat-start-pending buffer)
    (run-hermes-query-async buffer text)))

(defun submit-external-intent (prompt)
  "Director Mode fallback: no MCP worker is on the bus, so run the hermes CLI
directly with the intent phrased to use the editor's MCP tools, and show the
result in the chat panel."
  (let* ((buffer (ensure-hermes-chat-buffer))
         (full (format nil
                       "Operate the Yeditor editor (repo at the current directory) using its MCP tools (yeditor_*). Then summarize what you changed. Intent: ~A"
                       prompt)))
    (make-bottomside-window buffer :height 14)
    (unless (buffer-value buffer 'greeted)
      (setf (buffer-value buffer 'greeted) t))
    (chat-redraw-prompt buffer)
    (chat-insert buffer (format nil "[director] ~A" prompt) :attribute 'lem:modeline-name-attribute)
    (chat-start-pending buffer)
    (run-hermes-query-async buffer full)))

(define-command yeditor-hermes-chat-send () ()
  "Send the current input line in the Hermes chat buffer."
  (let ((buffer (current-buffer))
        (text (chat-input-text (current-buffer))))
    (when (plusp (length text))
      (chat-clear-input buffer)
      ;; no prompt redraw on send: the '> ' line the user typed on slides down
      ;; as the message and the streamed output are inserted above the prompt,
      ;; so a fresh '> ' is always ready at the very bottom. Redrawing here
      ;; would stack an empty prompt line above every message.
      (submit-hermes-message text))))

(defun focus-last-editor-buffer ()
  (alexandria:when-let (win (frame-bottomside-window (current-frame)))
    (let ((windows (window-list)))
      (let ((main (find-if (lambda (w)
                             (and (not (eq w win))
                                  (not (typep w 'lem-core:side-window))))
                           windows)))
        (when main
          (switch-to-window main))))))

(define-command yeditor-hermes-chat-focus () ()
  "Focus the Hermes chat panel without toggling it closed."
  (alexandria:when-let (win (frame-bottomside-window (current-frame)))
    (switch-to-window win)))

(define-command yeditor-hermes-chat-focus-editor () ()
  "Jump back to the main editor window, leaving the chat panel open."
  (focus-last-editor-buffer))

(define-command yeditor-hermes-chat-quit () ()
  "Close the Hermes chat panel."
  (ignore-errors (delete-bottomside-window))
  (setf (frame-bottomside-window (current-frame)) nil)
  (message "Hermes chat closed"))

(define-command yeditor-hermes-chat-toggle () ()
  "Toggle the Hermes Agent chat panel (C-c h)."
  (if (hermes-chat-active-p)
      (yeditor-hermes-chat-quit)
      (let ((buffer (ensure-hermes-chat-buffer)))
        (make-bottomside-window buffer :height 14)
        (switch-to-window (frame-bottomside-window (current-frame)))
        (unless (buffer-value buffer 'greeted)
          (setf (buffer-value buffer 'greeted) t)
          (chat-insert buffer "Hermes Agent chat. Type after '> ' and press Return to send. C-c C-k closes, C-c C-e returns to the editor.")
          (chat-redraw-prompt buffer)))))

(define-command hermes-agent () ()
  "Open and focus the Hermes Agent chat panel."
  (install-component 'le-hermes-chat)
  (unless (hermes-chat-active-p) (yeditor-hermes-chat-toggle))
  (yeditor-hermes-chat-focus))

(define-component le-hermes-chat
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (install-keybinding comp *global-keymap* "C-c h" (quote yeditor-hermes-chat-toggle))
                   ;; F1 intents go to Hermes while this is installed; uninstall restores the previous dispatcher
                   (install-variable comp (quote yeditor-components/director-cockpit:*intent-dispatcher*)
                                     (function submit-external-intent)))))
