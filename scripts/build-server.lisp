(ql:quickload :lem-server)

(lem:init-at-build-time)

(ensure-directories-exist "bin/")

(sb-ext:save-lisp-and-die "bin/yeditor-server"
                          :toplevel #'lem-server:main
                          :executable t)
