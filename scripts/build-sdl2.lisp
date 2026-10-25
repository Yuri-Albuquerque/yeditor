(ql:quickload :lem-sdl2)
(ql:quickload :lem-terminal)

;; Include Yeditor agent commands in the standalone application.
(ql:quickload :yeditor-enterprise)

(lem:init-at-build-time)

(ensure-directories-exist "bin/")

(sb-ext:save-lisp-and-die "bin/yeditor"
                          :toplevel #'lem:main
                          :executable t)
