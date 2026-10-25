(ql:quickload :lem-webview)

(ensure-directories-exist "bin/")

(sb-ext:save-lisp-and-die "bin/yeditor-client"
                          :toplevel #'lem-webview:webview-main
                          :executable t)
