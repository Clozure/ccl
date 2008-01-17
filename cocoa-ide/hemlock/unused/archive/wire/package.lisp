(defpackage :hemlock.wire
  (:use :common-lisp)
  (:nicknames :wire)
  (:export
   ;; wire.lisp
   #:remote-object-p
   #:remote-object
   #:remote-object-local-p
   #:remote-object-eq
   #:remote-object-value
   #:make-remote-object
   #:forget-remote-translation
   #:make-wire
   #:wire-p
   #:wire-fd
   #:wire-listen
   #:wire-get-byte
   #:wire-get-number
   #:wire-get-string
   #:wire-get-object
   #:wire-force-output
   #:wire-output-byte
   #:wire-output-number
   #:wire-output-string
   #:wire-output-object
   #:wire-output-funcall
   #:wire-error
   #:wire-eof
   #:wire-io-error
   #:*current-wire*
   #:wire-get-bignum
   #:wire-output-bignum
   ;; remote.lisp
   #:remote
   #:remote-value
   #:remote-value-bind
   #:create-request-server
   #:destroy-request-server
   #:connect-to-remote-server))

