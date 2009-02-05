;;; -*- Log: code.log; Package: wire -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
#+NIL
(ext:file-comment
  "$Header$")
;;;
;;; **********************************************************************
;;;
;;; This file implements a simple remote procedure call mechanism on top
;;; of wire.lisp.
;;;
;;; Written by William Lott.
;;;

(defpackage :hemlock.wire
  (:use :common-lisp))

(in-package :hemlock.wire)

(defstruct remote-wait
  value1 value2 value3 value4 value5
  abort
  finished)

(defvar *pending-returns* nil
  "AList of wire . remote-wait structs")

;;; MAYBE-NUKE-REMOTE-WAIT -- internal
;;;
;;; If the remote wait has finished, remove the external translation.
;;; Otherwise, mark the remote wait as finished so the next call to
;;; MAYBE-NUKE-REMOTE-WAIT will really nuke it.
;;;
(defun maybe-nuke-remote-wait (remote)
  (cond ((remote-wait-finished remote)
	 (forget-remote-translation remote)
	 t)
	(t
	 (setf (remote-wait-finished remote)
	       t)
	 nil)))

;;; REMOTE -- public
;;;
;;; Execute the body remotly. Subforms are executed locally in the lexical
;;; envionment of the macro call. No values are returned.
;;;
(defmacro remote (wire-form &body forms)
  "Evaluates the given forms remotly. No values are returned, as the remote
evaluation is asyncronus."
  (let ((wire (gensym)))
    `(let ((,wire ,wire-form))
       ,@(mapcar #'(lambda (form)
		     `(wire-output-funcall ,wire
					   ',(car form)
					   ,@(cdr form)))
	   forms)
       (values))))

;;; REMOTE-VALUE-BIND -- public
;;;
;;; Send to remote forms. First, a call to the correct dispatch routine based
;;; on the number of args, then the actual call. The dispatch routine will get
;;; the second funcall and fill in the correct number of arguments.
;;; Note: if there are no arguments, we don't even wait for the function to
;;; return, cause we can kind of guess at what the currect results would be.
;;;
(defmacro remote-value-bind (wire-form vars form &rest body)
  "Bind vars to the multiple values of form (which is executed remotly). The
forms in body are only executed if the remote function returned as apposed
to aborting due to a throw."
  (cond
   ((null vars)
    `(progn
       (remote ,wire-form ,form)
       ,@body))
   (t
    (let ((remote (gensym))
	  (wire (gensym)))
      `(let* ((,remote (make-remote-wait))
	      (,wire ,wire-form)
	      (*pending-returns* (cons (cons ,wire ,remote)
				       *pending-returns*)))
	 (unwind-protect
	     (let ,vars
	       (remote ,wire
		 (,(case (length vars)
		     (1 'do-1-value-call)
		     (2 'do-2-value-call)
		     (3 'do-3-value-call)
		     (4 'do-4-value-call)
		     (5 'do-5-value-call)
		     (t 'do-n-value-call))
		  (make-remote-object ,remote))
		 ,form)
	       (wire-force-output ,wire)
	       (loop
                 #+:hemlock.serve-event
                 (serve-all-events)
                 #-:hemlock.serve-event
                 (wire-get-object ,wire)
		 (when (remote-wait-finished ,remote)
		   (return)))
	       (unless (remote-wait-abort ,remote)
		 ,(case (length vars)
		    (1 `(setf ,(first vars) (remote-wait-value1 ,remote)))
		    (2 `(setf ,(first vars) (remote-wait-value1 ,remote)
			      ,(second vars) (remote-wait-value2 ,remote)))
		    (3 `(setf ,(first vars) (remote-wait-value1 ,remote)
			      ,(second vars) (remote-wait-value2 ,remote)
			      ,(third vars) (remote-wait-value3 ,remote)))
		    (4 `(setf ,(first vars) (remote-wait-value1 ,remote)
			      ,(second vars) (remote-wait-value2 ,remote)
			      ,(third vars) (remote-wait-value3 ,remote)
			      ,(fourth vars) (remote-wait-value4 ,remote)))
		    (5 `(setf ,(first vars) (remote-wait-value1 ,remote)
			      ,(second vars) (remote-wait-value2 ,remote)
			      ,(third vars) (remote-wait-value3 ,remote)
			      ,(fourth vars) (remote-wait-value4 ,remote)
			      ,(fifth vars) (remote-wait-value5 ,remote)))
		    (t
		     (do ((remaining-vars vars (cdr remaining-vars))
			  (form (list 'setf)
				(nconc form
				       (list (car remaining-vars)
					     `(pop values)))))
			 ((null remaining-vars)
			  `(let ((values (remote-wait-value1 ,remote)))
			     ,form)))))
		 ,@body))
	   (maybe-nuke-remote-wait ,remote)))))))


;;; REMOTE-VALUE -- public
;;;
;;; Alternate interface to getting the single return value of a remote
;;; function. Works pretty much just the same, except the single value is
;;; returned.
;;;
(defmacro remote-value (wire-form form &optional
				  (on-server-unwind
				   `(error "Remote server unwound")))
  "Execute the single form remotly. The value of the form is returned.
  The optional form on-server-unwind is only evaluated if the server unwinds
  instead of returning."
  (let ((remote (gensym))
	(wire (gensym)))
    `(let* ((,remote (make-remote-wait))
	    (,wire ,wire-form)
	    (*pending-returns* (cons (cons ,wire ,remote)
				     *pending-returns*)))
       (unwind-protect
	   (progn
	     (remote ,wire
	       (do-1-value-call (make-remote-object ,remote))
	       ,form)
	     (wire-force-output ,wire)
	     (loop
               #+:hemlock.serve-event
	       (serve-all-events)
               #-:hemlock.serve-event
               (wire-get-object ,wire)
	       (when (remote-wait-finished ,remote)
		 (return))))
	 (maybe-nuke-remote-wait ,remote))
       (if (remote-wait-abort ,remote)
	 ,on-server-unwind
	 (remote-wait-value1 ,remote)))))

;;; DEFINE-FUNCTIONS -- internal
;;;
;;;   Defines two functions, one that the client runs in the server, and one
;;; that the server runs in the client:
;;;
;;; DO-n-VALUE-CALL -- internal
;;;
;;;   Executed by the remote process. Reads the next object off the wire and
;;; sends the value back. Unwind-protect is used to make sure we send something
;;; back so the requestor doesn't hang.
;;;
;;; RETURN-n-VALUE -- internal
;;;
;;;   The remote procedure returned the given value, so fill it in the
;;; remote-wait structure. Note, if the requestor has aborted, just throw
;;; the value away.
;;;
(defmacro define-functions (values)
  (let ((do-call (intern (format nil "~:@(do-~D-value-call~)" values)))
	(return-values (intern (format nil "~:@(return-~D-value~:P~)" values)))
	(vars nil))
    (dotimes (i values)
      (push (gensym) vars))
    (setf vars (nreverse vars))
    `(progn
       (defun ,do-call (result)
	 (let (worked ,@vars)
	   (unwind-protect
	       (progn
		 (multiple-value-setq ,vars
		   (wire-get-object *current-wire*))
		 (setf worked t))
	     (if worked
	       (remote *current-wire*
		 (,return-values result ,@vars))
	       (remote *current-wire*
		 (remote-return-abort result)))
	     (wire-force-output *current-wire*))))
       (defun ,return-values (remote ,@vars)
	 (let ((result (remote-object-value remote)))
	   (unless (maybe-nuke-remote-wait result)
	     ,@(let ((setf-forms nil))
		 (dotimes (i values)
		   (push `(setf (,(intern (format nil
						  "~:@(remote-wait-value~D~)"
						  (1+ i)))
				 result)
				,(nth i vars))
			 setf-forms))
		 (nreverse setf-forms))))
	 nil))))

(define-functions 1)
(define-functions 2)
(define-functions 3)
(define-functions 4)
(define-functions 5)


;;; DO-N-VALUE-CALL -- internal
;;; 
;;; For more values then 5, all the values are rolled into a list and passed
;;; back as the first value, so we use RETURN-1-VALUE to return it.
;;;
(defun do-n-value-call (result)
  (let (worked values)
    (unwind-protect
	(progn
	  (setf values
		(multiple-value-list (wire-get-object *current-wire*)))
	  (setf worked t))
      (if worked
	(remote *current-wire*
	  (return-1-values result values))
	(remote *current-wire*
	  (remote-return-abort result)))
      (wire-force-output *current-wire*))))

;;; REMOTE-RETURN-ABORT -- internal
;;;
;;; The remote call aborted instead of returned.
;;;
(defun remote-return-abort (result)
  (setf result (remote-object-value result))
  (unless (maybe-nuke-remote-wait result)
    (setf (remote-wait-abort result) t)))

#+:hemlock.serve-event
;;; SERVE-REQUESTS -- internal
;;;
;;; Serve all pending requests on the given wire.
;;;
(defun serve-requests (wire on-death)
  (handler-bind
      ((wire-eof #'(lambda (condition)
		     (declare (ignore condition))
                     (close (wire-stream wire))
		     #+NILGB(system:invalidate-descriptor (wire-fd wire))
		     #+NILGB(unix:unix-close (wire-fd wire))
		     (dolist (pending *pending-returns*)
		       (when (eq (car pending)
				 wire)
			 (unless (maybe-nuke-remote-wait (cdr pending))
			   (setf (remote-wait-abort (cdr pending))
				 t))))
		     (when on-death
		       (funcall on-death))
		     (return-from serve-requests (values))))
       (wire-error #'(lambda (condition)
		       (declare (ignore condition))
                       #+NILGB
		       (system:invalidate-descriptor (wire-fd wire)))))
    (progn #+NILGB loop
        #+NILGB
        (unless (wire-listen wire)
          (return))
      (wire-get-object wire)))
  (values))

;;; NEW-CONNECTION -- internal
;;;
;;;   Maybe build a new wire and add it to the servers list of fds. If the user
;;; Supplied a function, close the socket if it returns NIL. Otherwise, install
;;; the wire.
;;;
(defun new-connection (socket addr on-connect)
  (let ((wire (make-wire socket))
	(on-death nil))
    (if (or (null on-connect)
	    (multiple-value-bind (okay death-fn)
                (funcall on-connect wire addr)
	      (setf on-death death-fn)
	      okay))
        #+:hemlock.serve-event
        (add-fd-handler socket :input
                        #'(lambda (socket)
                            (declare (ignore socket))
                            (serve-requests wire on-death)))
        #-:hemlock.serve-event
        (make-process (lambda ()
                        (loop (wire-get-object wire)))
                      :name (format nil "Wire process for ~S." wire))
        (ext-close-connection socket))))

;;; REQUEST-SERVER structure
;;;
;;; Just a simple handle on the socket and system:serve-event handler that make
;;; up a request server.
;;;
(defstruct (request-server
	    (:print-function %print-request-server))
  socket
  handler)

(defun %print-request-server (rs stream depth)
  (declare (ignore depth))
  (print-unreadable-object (rs stream :type t)
    (format stream "for ~D" (request-server-socket rs))))

;;; CREATE-REQUEST-SERVER -- Public.
;;;
;;; Create a TCP/IP listener on the given port.  If anyone tries to connect to
;;; it, call NEW-CONNECTION to do the connecting.
;;;
#+:hemlock.serve-event
(defun create-request-server (port &optional on-connect)
  "Create a request server on the given port.  Whenever anyone connects to it,
   call the given function with the newly created wire and the address of the
   connector.  If the function returns NIL, the connection is destroyed;
   otherwise, it is accepted.  This returns a manifestation of the server that
   DESTROY-REQUEST-SERVER accepts to kill the request server."
  (let* ((socket (ext-create-inet-listener port))
	 (handler (add-fd-handler socket :input
                                  #'(lambda (socket)
                                      (multiple-value-bind
                                            (newconn addr)
                                          (ext-accept-tcp-connection socket)
                                        (new-connection newconn addr on-connect))))))
    (make-request-server :socket socket
			 :handler handler)))

#-:hemlock.serve-event
(defun create-request-server (port &optional on-connect)
  "Create a request server on the given port.  Whenever anyone connects to it,
   call the given function with the newly created wire and the address of the
   connector.  If the function returns NIL, the connection is destroyed;
   otherwise, it is accepted.  This returns a manifestation of the server that
   DESTROY-REQUEST-SERVER accepts to kill the request server."
  (let* ((socket (ext-create-inet-listener port))
	 (handler (make-process
                   (lambda ()
                     (loop
                         (multiple-value-bind
                               (newconn addr)
                             (ext-accept-tcp-connection socket)
                           (new-connection newconn addr on-connect)))))))
    (make-request-server :socket socket
			 :handler handler)))

;;; DESTROY-REQUEST-SERVER -- Public.
;;;
;;; Removes the request server from SERVER's list of file descriptors and
;;; closes the socket behind it.
;;;
(defun destroy-request-server (server)
  "Quit accepting connections to the given request server."
  #+:hemlock.serve-event
  (remove-fd-handler (request-server-handler server))
  ;;
  (ext-close-socket (request-server-socket server))
  nil)

;;; CONNECT-TO-REMOTE-SERVER -- Public.
;;;
;;; Just like the doc string says, connect to a remote server. A handler is
;;; installed to handle return values, etc.
;;; 
#-NIL
(defun connect-to-remote-server (hostname port &optional on-death)
  "Connect to a remote request server addressed with the given host and port
   pair.  This returns the created wire."
  (let* ((socket (ext-connect-to-inet-socket hostname port))
	 (wire (make-wire socket)))
    #+:hemlock.serve-event
    ;; hmm, what exactly should this accomplish?
    (add-fd-handler socket :input
      #'(lambda (socket)
	  (declare (ignore socket))
	  (serve-requests wire on-death)))
    wire))
