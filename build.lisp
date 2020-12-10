(ql:quickload :goalkeeper)
(clack.util:find-handler :hunchentoot)
(sb-ext:save-lisp-and-die #P"goalkeeper"
                          :toplevel #'goalkeeper::start-loop
                          :executable t)
