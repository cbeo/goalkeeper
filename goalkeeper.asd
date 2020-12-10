;;;; goalkeeper.asd

(asdf:defsystem #:goalkeeper
  :description "Friendly app to keep on track"
  :author "Colin Okay <okay@toyful.space>"
  :license  "AGPLv3"
  :version "0.0.1"

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
