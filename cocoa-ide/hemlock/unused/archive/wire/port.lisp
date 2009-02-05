(defpackage :hemlock.wire
  (:use :common-lisp))

(in-package :hemlock.wire)

(defun ext-create-inet-listener (port)
  #+CMU
  (ext:create-inet-listener port)
  #+EXCL
  (socket:make-socket :connect :passive
                      :local-port port
                      :format :text)
  #+CLISP
  (socket:socket-server port)
  #+SBCL
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream
                               :protocol (sb-bsd-sockets:get-protocol-by-name "tcp"))))
    (sb-bsd-sockets:socket-bind socket (sb-bsd-sockets:make-inet-address "0.0.0.0") port)
    (sb-bsd-sockets:socket-listen socket 2)
    socket)
  #-(OR CMU EXCL CLISP SBCL)
  #.(error "Configure"))

(defun ext-accept-tcp-connection (socket)
  #+CMU (ext:accept-tcp-connection socket)
  #+EXCL
  (values
   (socket:accept-connection socket :wait t)
   (socket:remote-host socket))
  #+CLISP
  (let ((stream (socket:socket-accept socket)))
    #+NIL (setf (stream-element-type stream) '(unsigned-byte 8))
    (values
     stream
     (multiple-value-list (socket:socket-stream-peer stream))))
  #+SBCL
  (multiple-value-bind (socket peer-host peer-port)
      (sb-bsd-sockets:socket-accept socket)
    (values (sb-bsd-sockets:socket-make-stream socket :element-type 'character :input t :output t)
            peer-host))
  #-(OR CMU EXCL CLISP SBCL)
  #.(error "Configure")
  )

(defun ext-connect-to-inet-socket (host port)
  #+CMU (ext:connect-to-inet-socket host port)
  #+EXCL
  (progn
    #+(and allegro-version>= (version>= 5))
    (socket:make-socket :remote-host host
                        :remote-port port
                        :format :text)
    #-(and allegro-version>= (version>= 5))
    (ipc:open-network-stream 
     :host host :port port
     :element-type 'character
     ;; :class EXCL::BIDIRECTIONAL-BINARY-SOCKET-STREAM
     ))
  #+SBCL
  (sb-bsd-sockets:socket-make-stream 
   (let ((host (car (sb-bsd-sockets:host-ent-addresses
		     (sb-bsd-sockets:get-host-by-name host)))))
     (when host
       (let ((s (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream :protocol :tcp)))
         (sb-bsd-sockets:socket-connect s host port)
         s)))
   :element-type 'character             ;(unsigned-byte 8)
   :input t :output t)
  #+CLISP
  (socket:socket-connect port host)
  #-(OR CMU EXCL CLISP SBCL)
  #.(error "Configure"))

(defun ext-close-socket (socket)
  #+CMU   (ext:close-socket socket)
  #+EXCL  (close socket)
  #+CLISP (socket:socket-server-close socket)
  #+SBCL  (sb-bsd-sockets:socket-close socket)
  #-(OR CMU EXCL CLISP SBCL)
  #.(error "Configure"))

(defun ext-close-connection (connection)
  #+CMU   (ext:close-socket connection)
  #+EXCL  (close connection)
  #+CLISP (close connection)
  #+SBCL  (close connection)
  #-(OR CMU EXCL CLISP SBCL)
  #.(error "Configure"))

(defun unix-gethostid ()
  #.(or
     #+CMU '(unix:unix-gethostid)
     398792))

(defun unix-getpid ()
  #.(or 
     #+CMU   '(unix:unix-getpid)
     #+SBCL  '(sb-unix:unix-getpid)
     #+ACL   '(excl::getpid)
     #+CLISP '(system::program-id)))

#+(OR CLISP)
(eval-when (compile load eval)
  (pushnew :hemlock.serve-event *features*) )

#-:hemlock.serve-event
(defun make-process (function &key name)
  #+CMU  (mp:make-process function :name name)
  #+EXCL (mp:process-run-function name function)
  #+SBCL (sb-thread:make-thread function)
  #-(OR CMU EXCL SBCL)
  #.(error "Configure"))

#+:hemlock.serve-event
(progn

  (defstruct handler
    predicate
    function)

  (defvar *event-handlers* nil)

  ;; Sigh. CLISP barfs on (typep (ext-create-inet-listener 8981) 'SOCKET:SOCKET-SERVER)
  ;; Bad!
  
  (defun add-fd-handler (fd direction handler-function)
    (let (handler)
      (setf handler
            (make-handler
             :predicate
             (cond ((eql 'socket:socket-server
                         (type-of fd))
                    (lambda () (socket:socket-wait fd 0)))
                   ((typep fd 'xlib:display)
                    (lambda ()
                      (xlib:display-force-output fd)
                      (xlib:event-listen fd)))
                   (t
                    (lambda ()
                      (cond ((open-stream-p fd)
                             (let ((c (read-char-no-hang fd nil :eof)))
                               #+NIL (progn (print `(read-char-no-hang ,fd -> ,c)) (finish-output))
                               (if (characterp c) (unread-char c fd))
                               c))
                            (t
                             (setf *event-handlers* (delete handler *event-handlers*))
                             nil)))))
             :function
             (lambda () (funcall handler-function fd))))
      (push handler *event-handlers*)
      handler))

  (defun remove-fd-handler (handler)
    (setf *event-handlers*
          (delete handler *event-handlers*)))

  (defun serve-all-events ()
    (loop
        (let ((handler (find-if #'funcall *event-handlers* :key #'handler-predicate)))
          (cond (handler
                 (funcall (handler-function handler))
                 (return))
                (t
                 (sleep .01))))))

  (defun serve-event (&optional timeout)
    (let ((waited 0))
      (loop
          (let ((handler (find-if #'funcall *event-handlers* :key #'handler-predicate)))
            (cond (handler
                   (funcall (handler-function handler))
                   (return t))
                  ((>= waited timeout)
                   (return nil))
                  (t
                   (incf waited .01)
                   (sleep .01)))))))
  )

||#