;;; -*- Log: hemlock.log; Package: Hemlock-*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
#+CMU (ext:file-comment
  "$Header$")
;;;
;;; **********************************************************************
;;;
;;; Structures used by constucts in the HEMLOCK package.
;;;

(in-package "HEMLOCK")

;;; The server-info structure holds information about the connection to a
;;; particular eval server.  For now, we don't separate the background I/O and
;;; random compiler output.  The Notifications port and Terminal_IO will be the
;;; same identical object.  This separation in the interface may be just
;;; gratuitous pseudo-generality, but it doesn't hurt.
;;;
(defstruct (server-info
	    (:print-function
	     (lambda (s stream d)
	       (declare (ignore d))
	       (format stream "#<Server-Info for ~A>" (server-info-name s)))))
  name			      ; String name of this server.
  port			      ; Port we send requests to.
			      ;  NullPort if no connection. 
  notifications		      ; List of notification objects for operations
			      ;  which have not yet completed.
  ts-info		      ; Ts-Info structure of typescript we use in
			      ;  "background" buffer.
  buffer		      ; Buffer "background" typescript is in.
  slave-ts		      ; Ts-Info used in "Slave Lisp" buffer
			      ;  (formerly the "Lisp Listener" buffer).
  slave-buffer		      ; "Slave Lisp" buffer for slave's *terminal-io*.
  errors		      ; List of structures describing reported errors.
  error-mark)		      ; Pointer after last error edited. 
