(ql:quickload :lem-ncurses)

(lem:init-at-build-time)

(ensure-directories-exist "bin/")

(sb-ext:save-lisp-and-die "bin/yeditor"
                          :toplevel #'lem:main
                          :executable t)
