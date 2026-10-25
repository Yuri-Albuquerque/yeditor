(defsystem "lem-claude-code"
  :depends-on ("lem/core"
               "lem-lisp-mode" "frugal-uuid" "yason" "bordeaux-threads")
  :serial t
  :components ((:file "claude-code-sdk")
               (:file "claude-code")))
