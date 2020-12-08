(defpackage #:cffi-proxy
  (:use #:cl))

(cffi:load-foreign-library #P"~/.guix-profile/lib/libssl.so")
