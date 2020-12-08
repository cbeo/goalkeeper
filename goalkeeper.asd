;;;; goalkeeper.asd

(asdf:defsystem #:goalkeeper
  :description "Describe goalkeeper here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cffi-proxy
               #:lazybones
               #:bknr.datastore
               #:jonathan
               #:spinneret
               #:lass
               #:ironclad
               #:cl-base64
               #:local-time)
  :components ((:file "package")
               (:file "goalkeeper")))
