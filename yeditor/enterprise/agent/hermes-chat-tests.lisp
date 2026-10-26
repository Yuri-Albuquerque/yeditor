;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Enterprise
;;;; Proprietary and Confidential — Yeditor Agentic & Forge Suite (see LICENSE-ENTERPRISE).

(defpackage :yeditor-enterprise-tests/hermes-chat
  (:use :cl :rove :yeditor-context))
(in-package :yeditor-enterprise-tests/hermes-chat)

(deftest test-hermes-chat-dispatcher-is-reversible
  (lem-fake-interface:with-fake-interface ()
    (let ((before yeditor-components/director-cockpit:*intent-dispatcher*))
      (install-component (quote yeditor-enterprise/hermes-chat:le-hermes-chat))
      (ok (functionp yeditor-components/director-cockpit:*intent-dispatcher*)
          "F1 intents go to Hermes while the chat is installed")
      (uninstall-component (quote yeditor-enterprise/hermes-chat:le-hermes-chat))
      (ok (eq before yeditor-components/director-cockpit:*intent-dispatcher*)
          "uninstall restores the previous dispatcher")
      (let ((keys nil))
        (lem:traverse-keymap yeditor-enterprise/hermes-chat::*hermes-chat-keymap*
                             (lambda (sequence command)
                               (declare (ignore command))
                               (push (format nil "~{~A~^ ~}" sequence) keys)))
        (ok (not (member "C-c C-a" keys :test (function string=)))
            "C-c C-a is left to the HUD commit binding")
        (ok (member "C-c C-e" keys :test (function string=)) "C-c C-e returns to the editor")))))

(deftest test-hermes-stream-json-parsing-inserts-and-captures-session
  (let ((yeditor-enterprise/hermes-chat::*hermes-chat-session* nil))
    (lem-fake-interface:with-fake-interface ()
      (let ((buffer (lem:make-buffer "*hermes-parse-test*")))
        (flet ((parse (line)
                 (yeditor-enterprise/hermes-chat::parse-stream-json-line line buffer)))
          ;; the init "system" event carries an empty session_id: it must NOT
          ;; clobber the session, else the next turn resumes with --resume "".
          (parse "{\"type\":\"system\",\"session_id\":\"\"}")
          (ok (null yeditor-enterprise/hermes-chat::*hermes-chat-session*)
              "empty system session_id is ignored")
          ;; a text event is rendered into the buffer
          (parse "{\"type\":\"text\",\"text\":\"hello agent\"}")
          (ok (search "hello agent" (lem:buffer-text buffer))
              "text event is inserted into the buffer")
          ;; the real session id arrives on the result event and is captured
          (parse "{\"type\":\"result\",\"session_id\":\"local-abc123\",\"exit_code\":0}")
          (ok (string= "local-abc123" yeditor-enterprise/hermes-chat::*hermes-chat-session*)
              "real session_id is captured from the result event"))))))

(deftest test-hermes-query-renders-response-into-buffer
  (let* ((root (merge-pathnames (format nil "yeditor-hermes-mock-~D/" (random 10000000))
                                (uiop:temporary-directory)))
         (binary (merge-pathnames "hermes" root)))
    (ensure-directories-exist binary)
    (with-open-file (out binary :direction :output :if-exists :supersede)
      (write-line "#!/usr/bin/env python3" out)
      (write-line "import json" out)
      (write-line "print(json.dumps({'type':'system','session_id':''}))" out)
      (write-line "print(json.dumps({'type':'text','text':'hello agent'}))" out)
      (write-line "print(json.dumps({'type':'result','session_id':'local-abc123','exit_code':0}))" out))
    (uiop:run-program (list "chmod" "+x" (namestring binary)))
    (lem-fake-interface:with-fake-interface ()
      (let ((buffer (lem:make-buffer "*hermes-run-test*"))
            (yeditor-enterprise/hermes-chat::*hermes-binary* (namestring binary))
            (yeditor-enterprise/hermes-chat::*hermes-chat-session* nil))
        (yeditor-enterprise/hermes-chat::run-hermes-query buffer "hi")
        ;; the query defers the buffer mutation to the UI thread via
        ;; send-event; drain the queue so the enqueued lambda runs.
        (loop repeat 20
              while (plusp (lem:event-queue-length))
              do (lem:receive-event 0.001))
        (ok (search "hello agent" (lem:buffer-text buffer))
            "response text is rendered into the buffer")
        (ok (string= "local-abc123" yeditor-enterprise/hermes-chat::*hermes-chat-session*)
            "session id captured after the turn")))))

(defun hermes-test-lines (buffer)
  (ppcre:split "\\n" (lem:buffer-text buffer)))

(defun hermes-test-prompt-line-p (line)
  (and (plusp (length line)) (char= (char line 0) #\>)))

(deftest test-hermes-chat-lines-stack-above-the-prompt
  (lem-fake-interface:with-fake-interface ()
    (let ((buffer (lem:make-buffer "*hermes-layout-test*")))
      (yeditor-enterprise/hermes-chat::chat-insert buffer "greeting")
      (yeditor-enterprise/hermes-chat::chat-redraw-prompt buffer)
      (yeditor-enterprise/hermes-chat::chat-insert buffer "[you] hi")
      ;; the user may keep typing at the prompt while the agent streams
      (lem:insert-string (lem:buffer-end-point buffer) "next question")
      (yeditor-enterprise/hermes-chat::chat-insert buffer "[hermes] hello")
      (let ((lines (hermes-test-lines buffer)))
        (ok (equal (first (last lines)) "> next question")
            "the prompt stays the last line")
        (ok (= 1 (count-if #'hermes-test-prompt-line-p lines))
            "no duplicate prompt lines stack up")
        (ok (member "[you] hi" lines :test (function equal))
            "user messages are labeled")
        (ok (member "[hermes] hello" lines :test (function equal))
            "agent messages are labeled"))
      (ok (equal "next question"
                 (yeditor-enterprise/hermes-chat::chat-input-text buffer))
          "user typing survives streamed inserts"))))

(deftest test-hermes-chat-input-reads-text-after-the-prompt
  (lem-fake-interface:with-fake-interface ()
    (let ((buffer (lem:make-buffer "*hermes-input-test*")))
      (yeditor-enterprise/hermes-chat::chat-redraw-prompt buffer)
      (lem:insert-string (lem:buffer-end-point buffer) "hello world")
      (ok (equal "hello world"
                 (yeditor-enterprise/hermes-chat::chat-input-text buffer))
          "input text does not include the '> ' prefix")
      (yeditor-enterprise/hermes-chat::chat-clear-input buffer)
      (ok (equal (list "> ")
                 (last (hermes-test-lines buffer)))
          "the '> ' prompt itself survives clearing"))))

(deftest test-hermes-chat-turn-output-replaces-thinking-marker
  (let ((yeditor-enterprise/hermes-chat::*hermes-chat-session* nil))
    (lem-fake-interface:with-fake-interface ()
      (let ((buffer (lem:make-buffer "*hermes-pending-test*")))
        (flet ((parse (line)
                 (yeditor-enterprise/hermes-chat::parse-stream-json-line line buffer)))
          (yeditor-enterprise/hermes-chat::chat-redraw-prompt buffer)
          (yeditor-enterprise/hermes-chat::chat-insert buffer "[you] hi")
          (yeditor-enterprise/hermes-chat::chat-start-pending buffer)
          ;; tool activity is real output: the first event replaces the marker
          (parse "{\"type\":\"tool\",\"summary\":\"running grep\"}")
          (let ((lines (hermes-test-lines buffer)))
            (ok (member "  | running grep" lines :test (function equal))
                "tool call lines are shown dimmed with the pipe prefix")
            (ok (not (member "..." lines :test (function equal)))
                "the thinking marker is replaced by the first output"))
          (parse "{\"type\":\"text\",\"text\":\"hello agent\"}")
          (ok (member "[hermes] hello agent"
                      (hermes-test-lines buffer) :test (function equal))
              "agent text is labeled [hermes]")
          ;; a turn that emits no text events: the result's final text is used
          (yeditor-enterprise/hermes-chat::chat-insert buffer "[you] again")
          (yeditor-enterprise/hermes-chat::chat-start-pending buffer)
          (parse "{\"type\":\"result\",\"session_id\":\"local-x\",\"exit_code\":0,\"text\":\"final answer\"}")
          (let ((lines (hermes-test-lines buffer)))
            (ok (member "[hermes] final answer" lines :test (function equal))
                "result-only turns render the final text")
            (ok (not (member "..." lines :test (function equal)))
                "no stale thinking markers"))
          (ok (string= "local-x" yeditor-enterprise/hermes-chat::*hermes-chat-session*)
              "session id captured")
          ;; a failed result with an empty session id must not clobber
          ;; the session, and still reports the failure to the user.
          (yeditor-enterprise/hermes-chat::chat-start-pending buffer)
          (parse "{\"type\":\"result\",\"session_id\":\"\",\"exit_code\":7,\"text\":\"\"}")
          (ok (search "(hermes exited with code 7)" (lem:buffer-text buffer))
              "nonzero exit codes are reported")
          (ok (string= "local-x" yeditor-enterprise/hermes-chat::*hermes-chat-session*)
              "empty result session_id does not clobber the session")
          (ok (not (member "..." (hermes-test-lines buffer) :test (function equal)))
              "the thinking marker is cleared on failure too"))))))
