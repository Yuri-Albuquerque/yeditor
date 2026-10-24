(ql:quickload :lem-sdl2)
(ql:quickload :lem-terminal)

(lem:init-at-build-time)

(ensure-directories-exist "bin/")

(sb-ext:save-lisp-and-die "bin/yeditor"
                          :toplevel #'lem:main
                          :executable t)
