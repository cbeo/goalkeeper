(asdf:defsystem #:cffi-proxy
  :description "loads cffi and loads up some dependencies from custom locations"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cffi)
  :components ((:file "cffi-proxy")))
