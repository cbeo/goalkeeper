;;;; package.lisp

(defpackage #:goalkeeper
  (:use #:cl)
  (:local-nicknames  (#:store #:bknr.datastore))
  (:local-nicknames (#:idx #:bknr.indices))
  (:local-nicknames (#:html #:spinneret))
  (:import-from #:alexandria
                #:if-let
                #:when-let)
  (:import-from #:lazybones
                #:defroute
                #:*req*
                #:*body*
                #:http-ok
                #:http-err))
