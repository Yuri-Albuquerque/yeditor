;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Enterprise
;;;; Proprietary and Confidential — Yeditor Agentic & Forge Suite (see LICENSE-ENTERPRISE).

(load ".qlot/setup.lisp")
(ql:quickload "yeditor-enterprise-tests")
(lem/common/timer:with-timer-manager (make-instance 'lem/common/timer:timer-manager)
  (dolist (test '(yeditor-agent-ui-tests::indentation-guides-only-change-display
                  yeditor-agent-ui-tests::indentation-guides-follow-code-mode-and-toggle
                  yeditor-enterprise-tests/hermes-chat::test-hermes-chat-dispatcher-is-reversible
                  yeditor-enterprise-tests/hermes-chat::test-hermes-stream-json-parsing-inserts-and-captures-session
                  yeditor-enterprise-tests/hermes-chat::test-hermes-query-renders-response-into-buffer
                  yeditor-agent-ui-tests::hermes-agent-opens-and-refocuses
                  yeditor-agent-ui-tests::claude-cli-reads-batched-events-and-reports-stderr))
    (unless (rove:run-test test) (uiop:quit 1))))

(load "tests/display-cache.lisp")
(load "tests/attribute.lisp")
(lem/common/timer:with-timer-manager (make-instance 'lem/common/timer:timer-manager)
  (dolist (package '("LEM-TESTS/DISPLAY-CACHE" "LEM-TESTS/ATTRIBUTE"))
    (do-symbols (symbol (find-package package))
      (when (and (eq (symbol-package symbol) (find-package package)) (rove:get-test symbol))
        (unless (rove:run-test symbol) (uiop:quit 1))))))

(uiop:quit 0)
