;;;; goalkeeper.asd

(asdf:defsystem #:goalkeeper
  :description "Describe goalkeeper here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"

  ;; :defsystem-depends-on (:deploy)
  ;; :build-operation "deploy-op"
  ;; :build-pathname "goalkeeper"
  ;; :entry-point "goalkeeper::start"

  :serial t
  :depends-on (#:lazybones
               #:bknr.datastore
               #:jonathan
               #:spinneret
               #:lass
               #:ironclad
               #:cl-base64
               #:local-time)
  :components ((:file "package")
               (:file "goalkeeper")))
