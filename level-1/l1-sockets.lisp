;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2001 Clozure Associates
;;;   This file is part of OpenMCL.  
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(in-package "CCL")



;;; basic socket API
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(MAKE-SOCKET
	    ACCEPT-CONNECTION
	    DOTTED-TO-IPADDR
	    IPADDR-TO-DOTTED
	    IPADDR-TO-HOSTNAME
	    LOOKUP-HOSTNAME
	    LOOKUP-PORT
	    ;;with-pending-connect
	    RECEIVE-FROM
	    SEND-TO
	    SHUTDOWN
	    ;;socket-control
	    SOCKET-OS-FD
	    REMOTE-HOST
	    REMOTE-PORT
	    REMOTE-FILENAME
	    LOCAL-HOST
	    LOCAL-PORT
	    LOCAL-FILENAME
	    SOCKET-ADDRESS-FAMILY
	    SOCKET-CONNECT
	    SOCKET-FORMAT
	    SOCKET-TYPE
	    SOCKET-ERROR
	    SOCKET-ERROR-CODE
	    SOCKET-ERROR-IDENTIFIER
	    SOCKET-ERROR-SITUATION
	    WITH-OPEN-SOCKET)))

;;; The PPC is big-endian (uses network byte order), which makes
;;; things like #_htonl and #_htonl no-ops.  These functions aren't
;;; necessarily defined as functions in some header files (I'm sure
;;; that that either complies with or violates some C standard), and
;;; it doesn't seem to make much sense to fight that to do ff-calls
;;; to a couple of identity functions.

#+big-endian-target
(progn
  (defmacro HTONL (x) x)
  (defmacro HTONS (x) x)
  (defmacro NTOHL (x) x)
  (defmacro NTOHS (x) x))

#+little-endian-target
(progn
  (declaim (inline %bswap32 %bswap16))
  (defun %bswap32 (x)
    (declare (type (unsigned-byte 32) x))
    (%swap-u32 x))
  (defun %bswap16 (x)
    (declare (type (unsigned-byte 16) x))
    (%swap-u16 x))
  (defmacro HTONL (x) `(%bswap32 ,x))
  (defmacro HTONS (x) `(%bswap16 ,x))
  (defmacro NTOHL (x) `(%bswap32 ,x))
  (defmacro NTOHS (x) `(%bswap16 ,x)))

(defparameter *default-socket-character-encoding*
  nil)

(defmethod default-character-encoding ((domain (eql :socket)))
  *default-socket-character-encoding*)
  

;;; On some (hypothetical) little-endian platform, we might want to
;;; define HTONL and HTONS to actually swap bytes around.

(defpackage "OPENMCL-SOCKET"
  (:use "CL")
  (:import-from "CCL"
		"MAKE-SOCKET"
		"ACCEPT-CONNECTION"
		"DOTTED-TO-IPADDR"
		"IPADDR-TO-DOTTED"
		"IPADDR-TO-HOSTNAME"
		"LOOKUP-HOSTNAME"
		"LOOKUP-PORT"
		;;with-pending-connect
		"RECEIVE-FROM"
		"SEND-TO"
		"SHUTDOWN"
		;;socket-control
		"SOCKET-OS-FD"
		"REMOTE-HOST"
		"REMOTE-PORT"
		"REMOTE-FILENAME"
		"LOCAL-HOST"
		"LOCAL-PORT"
		"LOCAL-FILENAME"
		"SOCKET-ADDRESS-FAMILY"
		"SOCKET-CONNECT"
		"SOCKET-FORMAT"
		"SOCKET-TYPE"
		"SOCKET-ERROR"
		"SOCKET-ERROR-CODE"
		"SOCKET-ERROR-IDENTIFIER"
		"SOCKET-ERROR-SITUATION"
		"WITH-OPEN-SOCKET")
  (:export  "MAKE-SOCKET"
	    "ACCEPT-CONNECTION"
	    "DOTTED-TO-IPADDR"
	    "IPADDR-TO-DOTTED"
	    "IPADDR-TO-HOSTNAME"
	    "LOOKUP-HOSTNAME"
	    "LOOKUP-PORT"
	    ;;with-pending-connect
	    "RECEIVE-FROM"
	    "SEND-TO"
	    "SHUTDOWN"
	    ;;socket-control
	    "SOCKET-OS-FD"
	    "REMOTE-HOST"
	    "REMOTE-PORT"
	    "REMOTE-FILENAME"
	    "LOCAL-HOST"
	    "LOCAL-PORT"
	    "LOCAL-FILENAME"
	    "SOCKET-ADDRESS-FAMILY"
	    "SOCKET-CONNECT"
	    "SOCKET-FORMAT"
	    "SOCKET-TYPE"
	    "SOCKET-ERROR"
	    "SOCKET-ERROR-CODE"
	    "SOCKET-ERROR-IDENTIFIER"
	    "SOCKET-ERROR-SITUATION"
	    "WITH-OPEN-SOCKET"))

(eval-when (:compile-toplevel :execute)
  #+linuxppc-target
  (require "PPC-LINUX-SYSCALLS")
  #+linuxx8664-target
  (require "X8664-LINUX-SYSCALLS")
  #+darwinppc-target
  (require "DARWINPPC-SYSCALLS")
  #+darwinx8664-target
  (require "DARWINX8664-SYSCALLS")
  #+freebsdx8664-target
  (require "X8664-FREEBSD-SYSCALLS")
  #+solarisx8664-target
  (require "X8664-SOLARIS-SYSCALLS")
  )

(define-condition socket-error (simple-stream-error)
  ((code :initarg :code :reader socket-error-code)
   (identifier :initform :unknown :initarg :identifier :reader socket-error-identifier)
   (Situation :initarg :situation :reader socket-error-situation)))

(define-condition socket-creation-error (simple-error)
  ((code :initarg :code :reader socket-creation-error-code)
   (identifier :initform :unknown :initarg :identifier :reader socket-creation-error-identifier)
   (situation :initarg :situation :reader socket-creation-error-situation)))

(defvar *socket-error-identifiers*
  (list #$EADDRINUSE :address-in-use
	#$ECONNABORTED :connection-aborted
	#$ENOBUFS :no-buffer-space
	#$ENOMEM :no-buffer-space
	#$ENFILE :no-buffer-space
	#$ETIMEDOUT :connection-timed-out
	#$ECONNREFUSED :connection-refused
	#$ENETUNREACH :host-unreachable
	#$EHOSTUNREACH :host-unreachable
	#$EHOSTDOWN :host-down
	#$ENETDOWN :network-down
	#$EADDRNOTAVAIL :address-not-available
	#$ENETRESET :network-reset
	#$ECONNRESET :connection-reset
	#$ESHUTDOWN :shutdown
	#$EACCES :access-denied
	#$EPERM :access-denied))


(declaim (inline socket-call))
(defun socket-call (stream where res)
  (if (< res 0)
    (socket-error stream where res)
    res))

(defun %hstrerror (h_errno)
  (with-macptrs ((p (#_hstrerror (abs h_errno))))
    (if p
      (%get-cstring p)
      (format nil "Nameserver error ~d" (abs h_errno)))))
    



(defun socket-error (stream where errno &optional nameserver-p)
  "Creates and signals (via error) one of two socket error 
conditions, based on the state of the arguments."
  (when (< errno 0)
    (setq errno (- errno)))
  (if stream
    (error (make-condition 'socket-error
			   :stream stream
			   :code errno
			   :identifier (getf *socket-error-identifiers* errno :unknown)
			   :situation where
			   ;; TODO: this is a constant arg, there is a way to put this
			   ;; in the class definition, just need to remember how...
			   :format-control "~a (error #~d) during ~a"
			   :format-arguments (list
					      (if nameserver-p
						(%hstrerror errno)
						(%strerror errno))
					      errno where)))
    (error (make-condition 'socket-creation-error
			   :code errno
			   :identifier (getf *socket-error-identifiers* errno :unknown)
			   :situation where
			   ;; TODO: this is a constant arg, there is a way to put this
			   ;; in the class definition, just need to remember how...
			   :format-control "~a (error #~d) during socket creation in ~a"
			   :format-arguments (list
					      (if nameserver-p
						(%hstrerror errno)
						(%strerror errno))
					      errno where)))))
    


;; If true, this will try to allow other cooperative processes to run
;; while socket io is happening.  Since CCL threads are preemptively
;; scheduled, this isn't particularly meaningful.
(defvar *multiprocessing-socket-io* nil)

(defclass socket ()
  ())

(defmacro with-open-socket ((var . args) &body body
			    &aux (socket (make-symbol "socket"))
			         (done (make-symbol "done")))
  "Execute body with var bound to the result of applying make-socket to
make-socket-args. The socket gets closed on exit."
  `(let (,socket ,done)
     (unwind-protect
	 (multiple-value-prog1
	   (let ((,var (setq ,socket (make-socket ,@args))))
	     ,@body)
	   (setq ,done t))
       (when ,socket (close ,socket :abort (not ,done))))))

(defgeneric socket-address-family (socket)
  (:documentation "Return :internet or :file, as appropriate."))

(defclass ip-socket (socket)
  ())

(defmethod socket-address-family ((socket ip-socket)) :internet)

(defclass file-socket (socket)
  ())

(defmethod socket-address-family ((socket file-socket)) :file)

(defclass tcp-socket (ip-socket)
  ())

(defgeneric socket-type (socket)
  (:documentation
   "Return :stream for tcp-stream and listener-socket, and :datagram
for udp-socket."))

(defmethod socket-type ((socket tcp-socket)) :stream)

(defclass stream-file-socket (file-socket)
  ())

(defmethod socket-type ((socket stream-file-socket)) :stream)


;;; An active TCP socket is an honest-to-goodness stream.
(defclass tcp-stream (tcp-socket)
  ())

(defclass fundamental-tcp-stream (tcp-stream
                                  fd-stream
                                  buffered-binary-io-stream-mixin
                                  buffered-character-io-stream-mixin)
    ())

(make-built-in-class 'basic-tcp-stream
                     'tcp-stream
                     'basic-binary-io-stream
                     'basic-character-io-stream)

(defgeneric socket-connect (stream)
 (:documentation
   "Return :active for tcp-stream, :passive for listener-socket, and NIL
for udp-socket"))

(defmethod socket-connect ((stream tcp-stream)) :active)

(defgeneric socket-format (stream)
  (:documentation
   "Return the socket format as specified by the :format argument to
make-socket."))

(defmethod socket-format ((stream tcp-stream))
  (if (eq (stream-element-type stream) 'character)
    :text
    ;; Should distinguish between :binary and :bivalent, but hardly
    ;; seems worth carrying around an extra slot just for that.
    :bivalent))

(defmethod socket-device ((stream tcp-stream))
  (let ((ioblock (stream-ioblock stream nil)))
    (and ioblock (ioblock-device ioblock))))

(defmethod select-stream-class ((class tcp-stream) in-p out-p char-p)
  (declare (ignore char-p)) ; TODO: is there any real reason to care about this?
  ;; Yes, in general.  There is.
  (assert (and in-p out-p) () "Non-bidirectional tcp stream?")
  'fundamental-tcp-stream)

(defmethod map-to-basic-stream-class-name ((name (eql 'tcp-stream)))
  'basic-tcp-stream)

(defmethod select-stream-class ((s (eql 'basic-tcp-stream)) in-p out-p char-p)
  (declare (ignore char-p))
  (assert (and in-p out-p) () "Non-bidirectional tcp stream?")
  'basic-tcp-stream)

;;; A FILE-SOCKET-STREAM is also honest. To goodness.
(defclass file-socket-stream (stream-file-socket)
  ())

(defclass fundamental-file-socket-stream (file-socket-stream
                                          fd-stream
                                          buffered-binary-io-stream-mixin
                                          buffered-character-io-stream-mixin)
    ())

(make-built-in-class 'basic-file-socket-stream
                     'file-socket-stream
                     'basic-binary-io-stream
                     'basic-character-io-stream)


(defmethod map-to-basic-stream-class-name ((name (eql 'file-socket-stream)))
  'basic-file-socket-stream)

(defmethod select-stream-class ((class file-socket-stream) in-p out-p char-p)
  (declare (ignore char-p)) ; TODO: is there any real reason to care about this?
  (assert (and in-p out-p) () "Non-bidirectional file-socket stream?")
  'fundamental-file-socket-stream)

(defmethod select-stream-class ((s (eql 'basic-file-socket-stream)) in-p out-p char-p)
  (declare (ignore char-p))
  (assert (and in-p out-p) () "Non-bidirectional file-socket stream?")
  'basic-file-socket-stream)

(defclass unconnected-socket (socket)
  ((device :initarg :device :accessor socket-device)
   (keys :initarg :keys :reader socket-keys)))

(defmethod socket-format ((socket unconnected-socket))
  (or (getf (socket-keys socket) :format) :text))

(defgeneric close (socket &key abort)
  (:documentation
   "The close generic function can be applied to sockets. It releases the
operating system resources associated with the socket."))

(defmethod close ((socket unconnected-socket) &key abort)
  (declare (ignore abort))
  (when (socket-device socket)
    (fd-close (socket-device socket))
    (setf (socket-device socket) nil)
    t))

;; A passive tcp socket just generates connection streams
(defclass listener-socket (tcp-socket unconnected-socket) ())

(defmethod SOCKET-CONNECT ((stream listener-socket)) :passive)

(defclass file-listener-socket (stream-file-socket unconnected-socket) ())

(defmethod SOCKET-CONNECT ((stream file-listener-socket)) :passive)

;;; A FILE-LISTENER-SOCKET should try to delete the filesystem
;;; entity when closing.

(defmethod close :before ((s file-listener-socket) &key abort)
  (declare (ignore abort))
  (let* ((path (local-socket-filename (socket-device s) s)))
    (when path (%delete-file path))))


;; A udp socket just sends and receives packets.
(defclass udp-socket (ip-socket unconnected-socket) ())

(defmethod socket-type ((stream udp-socket)) :datagram)
(defmethod socket-connect ((stream udp-socket)) nil)

(defgeneric socket-os-fd (socket)
  (:documentation
   "Return the native OS's representation of the socket, or NIL if the
socket is closed. On Unix, this is the Unix 'file descriptor', a small
non-negative integer. Note that it is rather dangerous to mess around
with tcp-stream fd's, as there is all sorts of buffering and asynchronous
I/O going on above the OS level. listener-socket and udp-socket fd's are
safer to mess with directly as there is less magic going on."))

;; Returns nil for closed stream...
(defmethod socket-os-fd ((socket socket))
  (socket-device socket))

;; Returns nil for closed stream
(defun local-socket-info (fd type socket)
  (and fd
       (rlet ((sockaddr :sockaddr_in)
	      (namelen :signed))
	     (setf (pref namelen :signed) (record-length :sockaddr_in))
	     (socket-call socket "getsockname" (c_getsockname fd sockaddr namelen))
	     (when (= #$AF_INET (pref sockaddr :sockaddr_in.sin_family))
	       (ecase type
		 (:host (ntohl (pref sockaddr
                                     #-solaris-target :sockaddr_in.sin_addr.s_addr
                                     #+solaris-target #>sockaddr_in.sin_addr.S_un.S_addr)))
		 (:port (ntohs (pref sockaddr :sockaddr_in.sin_port))))))))

(defun path-from-unix-address (addr)
  (when (= #$AF_UNIX (pref addr :sockaddr_un.sun_family))
    #+darwin-target
    (%str-from-ptr (pref addr :sockaddr_un.sun_path)
		   (- (pref addr :sockaddr_un.sun_len) 2))
    #-darwin-target
    (%get-cstring (pref addr :sockaddr_un.sun_path))))

(defun local-socket-filename (fd socket)
  (and fd
       (rlet ((addr :sockaddr_un)
              (namelen :signed))
         (setf (pref namelen :signed) (record-length :sockaddr_un))
         (socket-call socket "getsockname" (c_getsockname fd addr namelen))
	 (path-from-unix-address addr))))

(defmacro with-if ((var expr) &body body)
  `(let ((,var ,expr))
     (if ,var
	 (progn
	   ,@body))))     

(defun remote-socket-info (socket type)
  (with-if (fd (socket-device socket))
    (rlet ((sockaddr :sockaddr_in)
	   (namelen :signed))
	  (setf (pref namelen :signed) (record-length :sockaddr_in))
	  (let ((err (c_getpeername fd sockaddr namelen)))
	    (cond ((eql err (- #$ENOTCONN)) nil)
		  ((< err 0) (socket-error socket "getpeername" err))
		  (t
		   (when (= #$AF_INET (pref sockaddr :sockaddr_in.sin_family))
		     (ecase type
		       (:host (ntohl (pref sockaddr
                                           #-solaris-target :sockaddr_in.sin_addr.s_addr
                                           #+solaris-target #>sockaddr_in.sin_addr.S_un.S_addr)))
		       (:port (ntohs  (pref sockaddr :sockaddr_in.sin_port)))))))))))

(defun remote-socket-filename (socket)
  (with-if (fd (socket-device socket))
    (rlet ((addr :sockaddr_un)
	   (namelen :signed))
	  (setf (pref namelen :signed) (record-length :sockaddr_un))
	  (let* ((err (c_getsockname fd addr namelen)))
	    (cond ((eql err (- #$ENOTCONN)) nil)
		  ((< err 0) (socket-error socket "getpeername" err))
		  (t (path-from-unix-address addr)))))))

(defgeneric local-port (socket)
  (:documentation "Return the local port number."))

(defmethod local-port ((socket socket))
  (local-socket-info (socket-device socket) :port socket))

(defgeneric local-host (socket)
  (:documentation
   "Return 32-bit unsigned IP address of the local host."))

(defmethod local-host ((socket socket))
  (local-socket-info (socket-device socket) :host socket))

(defmethod local-filename ((socket socket))
  (local-socket-filename (socket-device socket) socket))

(defgeneric remote-host (socket)
  (:documentation
   "Return the 32-bit unsigned IP address of the remote host, or NIL if
the socket is not connected."))

;; Returns NIL if socket is not connected
(defmethod remote-host ((socket socket))
  (remote-socket-info socket :host))

(defgeneric remote-port (socket)
  (:documentation
   "Return the remote port number, or NIL if the socket is not connected."))

(defmethod remote-port ((socket socket))
  (remote-socket-info socket :port))

(defmethod remote-filename ((socket socket))
  (remote-socket-filename socket))
  
(defun set-socket-options (fd-or-socket &key 
			   keepalive
			   reuse-address
			   nodelay
			   broadcast
			   linger
			   address-family
			   local-port
			   local-host
			   local-filename
			   type
			   connect
			   out-of-band-inline
			   &allow-other-keys)
  ;; see man socket(7) tcp(7) ip(7)
  (multiple-value-bind (socket fd) (etypecase fd-or-socket
				     (socket (values fd-or-socket (socket-device fd-or-socket)))
				     (integer (values nil fd-or-socket)))
    
    (if (null address-family)
	(setq address-family :internet))
    (when keepalive
      (int-setsockopt fd #$SOL_SOCKET #$SO_KEEPALIVE 1))
    (when reuse-address
      (int-setsockopt fd #$SOL_SOCKET #$SO_REUSEADDR 1))
    (when broadcast
      (int-setsockopt fd #$SOL_SOCKET #$SO_BROADCAST 1))
    (when out-of-band-inline
      (int-setsockopt fd #$SOL_SOCKET #$SO_OOBINLINE 1))
    (rlet ((plinger :linger))
	  (setf (pref plinger :linger.l_onoff) (if linger 1 0)
		(pref plinger :linger.l_linger) (or linger 0))
	  (socket-call socket "setsockopt"
		       (c_setsockopt fd #$SOL_SOCKET #$SO_LINGER plinger (record-length :linger))))
    (when (eq address-family :internet)
      (when nodelay
	(int-setsockopt fd
			#+linux-target #$SOL_TCP
			#-linux-target #$IPPROTO_TCP
			#$TCP_NODELAY 1))
      (when (or local-port local-host)
	(let* ((proto (if (eq type :stream) "tcp" "udp"))
	       (port-n (if local-port (port-as-inet-port local-port proto) 0))
	       (host-n (if local-host (host-as-inet-host local-host) #$INADDR_ANY)))
	  ;; Darwin includes the SIN_ZERO field of the sockaddr_in when
	  ;; comparing the requested address to the addresses of configured
	  ;; interfaces (as if the zeros were somehow part of either address.)
	  ;; "rletz" zeros out the stack-allocated structure, so those zeros
	  ;; will be 0.
	  (rletz ((sockaddr :sockaddr_in))
		 (setf (pref sockaddr :sockaddr_in.sin_family) #$AF_INET
		       (pref sockaddr :sockaddr_in.sin_port) port-n
		       (pref sockaddr
                             #-solaris-target :sockaddr_in.sin_addr.s_addr
                             #+solaris-target #>sockaddr_in.sin_addr.S_un.S_addr
                             ) host-n)
		 (socket-call socket "bind" (c_bind fd sockaddr (record-length :sockaddr_in)))))))
    (when (and (eq address-family :file)
	       (eq connect :passive)
	       local-filename)
      (bind-unix-socket fd local-filename))    
    (when (and nil *multiprocessing-socket-io*)
      (socket-call socket "fcntl" (fd-set-flag fd #$O_NONBLOCK)))))

;; I hope the inline declaration makes the &rest/apply's go away...
(declaim (inline make-ip-socket))
(defun make-ip-socket (&rest keys &key type &allow-other-keys)
  (declare (dynamic-extent keys))
  (ecase type
    ((nil :stream) (apply #'make-tcp-socket keys))
    ((:datagram) (apply #'make-udp-socket keys))))

(declaim (inline make-file-socket))
(defun make-file-socket (&rest keys &key type &allow-other-keys)
  (declare (dynamic-extent keys))
  (ecase type
    ((nil :stream) (apply #'make-stream-file-socket keys))
    (:datagram (apply #'make-datagram-file-socket keys))))

(defun make-socket (&rest keys
		    &key address-family
		    ;; List all keys here just for error checking...
		    ;; &allow-other-keys
		    type connect remote-host remote-port eol format
		    keepalive reuse-address nodelay broadcast linger
		    local-port local-host backlog class out-of-band-inline
		    local-filename remote-filename sharing basic
                    external-format (auto-close t)
                    connect-timeout input-timeout output-timeout deadline)
  "Create and return a new socket."
  (declare (dynamic-extent keys))
  (declare (ignore type connect remote-host remote-port eol format
		   keepalive reuse-address nodelay broadcast linger
		   local-port local-host backlog class out-of-band-inline
		   local-filename remote-filename sharing basic external-format
                   auto-close connect-timeout input-timeout output-timeout deadline))
  (ecase address-family
    ((:file) (apply #'make-file-socket keys))
    ((nil :internet) (apply #'make-ip-socket keys))))



(defun make-udp-socket (&rest keys &aux (fd -1))
  (unwind-protect
    (let (socket)
      (setq fd (socket-call nil "socket"
			    (c_socket #$AF_INET #$SOCK_DGRAM #$IPPROTO_UDP)))
      (apply #'set-socket-options fd keys)
      (setq socket (make-instance 'udp-socket
				  :device fd
				  :keys keys))
      (setq fd -1)
      socket)
    (unless (< fd 0)
      (fd-close fd))))

(defun make-tcp-socket (&rest keys &key connect &allow-other-keys &aux (fd -1))
  (unwind-protect
    (let (socket)
      (setq fd (socket-call nil "socket"
			    (c_socket #$AF_INET #$SOCK_STREAM #$IPPROTO_TCP)))
      (apply #'set-socket-options fd keys)
      (setq socket
	    (ecase connect
	      ((nil :active) (apply #'make-tcp-stream-socket fd keys))
	      ((:passive) (apply #'make-tcp-listener-socket fd keys))))
      (setq fd -1)
      socket)
    (unless (< fd 0)
      (fd-close fd))))

(defun make-stream-file-socket (&rest keys &key connect &allow-other-keys &aux (fd -1))
  (unwind-protect
    (let (socket)
      (setq fd (socket-call nil "socket" (c_socket #$PF_UNIX #$SOCK_STREAM 0)))
      (apply #'set-socket-options fd keys)
      (setq socket
	    (ecase connect
	      ((nil :active) (apply #'make-file-stream-socket fd keys))
	      ((:passive) (apply #'make-file-listener-socket fd keys))))
      (setq fd -1)
      socket)
    (unless (< fd 0)
      (fd-close fd))))

(defun %socket-connect (fd addr addrlen &optional timeout-in-milliseconds)
  (let* ((err (c_connect fd addr addrlen timeout-in-milliseconds)))
    (declare (fixnum err))
    (unless (eql err 0) (fd-close fd) (socket-error nil "connect" err))))
    
(defun inet-connect (fd host-n port-n &optional timeout-in-milliseconds)
  (rlet ((sockaddr :sockaddr_in))
    (setf (pref sockaddr :sockaddr_in.sin_family) #$AF_INET
          (pref sockaddr :sockaddr_in.sin_port) port-n
          (pref sockaddr
                #-solaris-target :sockaddr_in.sin_addr.s_addr
                #+solaris-target #>sockaddr_in.sin_addr.S_un.S_addr
                ) host-n)
    (%socket-connect fd sockaddr (record-length :sockaddr_in) timeout-in-milliseconds)))
               
(defun file-socket-connect (fd remote-filename)
  (rletz ((sockaddr :sockaddr_un))
    (init-unix-sockaddr sockaddr remote-filename)
    (%socket-connect fd sockaddr (record-length :sockaddr_un))))
         
  
(defun make-tcp-stream-socket (fd &rest keys
                                  &key remote-host
				  remote-port
                                  connect-timeout
                                  deadline
				  &allow-other-keys)
  (let* ((timeout-in-milliseconds
          (if deadline
            (max (round (- deadline (get-internal-real-time))
                        (/ internal-time-units-per-second 1000))
                 0)
            (if connect-timeout
              (round (* connect-timeout 1000))))))
    (inet-connect fd
                  (host-as-inet-host remote-host)
                  (port-as-inet-port remote-port "tcp")
                  timeout-in-milliseconds)
    (apply #'make-tcp-stream fd keys)))

(defun make-file-stream-socket (fd &rest keys
                                   &key remote-filename
                                   &allow-other-keys)
  (file-socket-connect fd remote-filename)
  (apply #'make-file-socket-stream fd keys))


(defun make-tcp-stream (fd
                        &key (format :bivalent)
                             external-format
                             (class 'tcp-stream)
                             sharing
                             (basic t)
                             (auto-close t)
                             input-timeout
                             output-timeout
                             deadline
                        &allow-other-keys)
  (let* ((external-format (normalize-external-format :socket external-format)))
    (let ((element-type (ecase format
                          ((nil :text) 'character)
                          ((:binary :bivalent) '(unsigned-byte 8)))))
      ;; TODO: check out fd-stream-advance, -listen, -eofp, -force-output, -close
      ;; See if should specialize any of 'em.
      (make-fd-stream fd
                      :class class
                      :direction :io
                      :element-type element-type
                      :sharing sharing
                      :character-p (not (eq format :binary))
                      :encoding (external-format-character-encoding external-format)
                      :line-termination (external-format-line-termination external-format)
                      :basic basic
                      :auto-close auto-close
                      :input-timeout input-timeout
                      :output-timeout output-timeout
                      :deadline deadline))))

(defun make-file-socket-stream (fd
                                &key (format :bivalent)
                                external-format
                                (class 'file-socket-stream)
                                sharing
                                basic
                                (auto-close t)
                                input-timeout
                                output-timeout
                                deadline
                                &allow-other-keys)
  (let* ((external-format (normalize-external-format :socket external-format)))
  
    (let ((element-type (ecase format
                          ((nil :text) 'character)
                          ((:binary :bivalent) '(unsigned-byte 8)))))
      ;; TODO: check out fd-stream-advance, -listen, -eofp, -force-output, -close
      ;; See if should specialize any of 'em.
      (make-fd-stream fd
                      :class class
                      :direction :io
                      :element-type element-type
                      :encoding (external-format-character-encoding external-format)
                      :line-termination (external-format-line-termination external-format)
                      :sharing sharing
                      :character-p (not (eq format :binary))
                      :basic basic
                      :auto-close auto-close
                      :input-timeout input-timeout
                      :output-timeout output-timeout
                      :deadline deadline))))

(defun make-tcp-listener-socket (fd &rest keys &key backlog &allow-other-keys)
  (socket-call nil "listen" (c_listen fd (or backlog 5)))
  (make-instance 'listener-socket
		 :device fd
		 :keys keys))

(defun make-file-listener-socket (fd &rest keys &key backlog &allow-other-keys)
  (socket-call nil "listen" (c_listen fd (or backlog 5)))
  (make-instance 'file-listener-socket
		 :device fd
		 :keys keys))

(defun socket-accept (fd wait socket)
  (flet ((_accept (fd async)
	   (let ((res (c_accept fd (%null-ptr) (%null-ptr))))
	     (declare (fixnum res))
	     ;; See the inscrutable note under ERROR HANDLING in
	     ;; man accept(2). This is my best guess at what they mean...
	     (if (and async (< res 0)
		      (or (eql res (- #$ENETDOWN))
			  (eql res (- #+linux-target #$EPROTO
				      #-linux-target  #$EPROTOTYPE))
			  (eql res (- #$ENOPROTOOPT))
			  (eql res (- #$EHOSTDOWN))
			  (eql res (- #+linux-target #$ENONET
				      #-linux-target #$ENETDOWN))
			  (eql res (- #$EHOSTUNREACH))
			  (eql res (- #$EOPNOTSUPP))
			  (eql res (- #$ENETUNREACH))))
	       (- #$EAGAIN)
	       res))))
    (cond (wait
	    (with-eagain fd :input
	      (_accept fd *multiprocessing-socket-io*)))
	  (*multiprocessing-socket-io*
	    (_accept fd t))
	  (t
	    (let ((old (socket-call socket "fcntl" (fd-get-flags fd))))
	      (unwind-protect
		  (progn
		    (socket-call socket "fcntl" (fd-set-flags fd (logior old #$O_NONBLOCK)))
		    (_accept fd t))
		(socket-call socket "fcntl" (fd-set-flags fd old))))))))

(defun accept-socket-connection (socket wait stream-create-function)
  (let ((listen-fd (socket-device socket))
	(fd -1))
    (unwind-protect
      (progn
	(setq fd (socket-accept listen-fd wait socket))
	(cond ((>= fd 0)
	       (prog1 (apply stream-create-function fd (socket-keys socket))
		 (setq fd -1)))
	      ((eql fd (- #$EAGAIN)) nil)
	      (t (socket-error socket "accept" fd))))
      (when (>= fd 0)
	(fd-close fd)))))

(defgeneric accept-connection (socket &key wait)
  (:documentation
  "Extract the first connection on the queue of pending connections,
accept it (i.e. complete the connection startup protocol) and return a new
tcp-stream or file-socket-stream representing the newly established
connection.  The tcp stream inherits any properties of the listener socket
that are relevant (e.g. :keepalive, :nodelay, etc.) The original listener
socket continues to be open listening for more connections, so you can call
accept-connection on it again."))

(defmethod accept-connection ((socket listener-socket) &key (wait t))
  (accept-socket-connection socket wait #'make-tcp-stream))

(defmethod accept-connection ((socket file-listener-socket) &key (wait t))
  (accept-socket-connection socket wait #'make-file-socket-stream))

(defun verify-socket-buffer (buf offset size)
  (unless offset (setq offset 0))
  (unless (<= (+ offset size) (length buf))
    (report-bad-arg size `(integer 0 ,(- (length buf) offset))))
  (multiple-value-bind (arr start) (array-data-and-offset buf)
    (setq buf arr offset (+ offset start)))
  ;; TODO: maybe should allow any raw vector
  (let ((subtype (typecode buf)))
    (unless #+ppc32-target (and (<= ppc32::min-8-bit-ivector-subtag subtype)
                                (<= subtype ppc32::max-8-bit-ivector-subtag))
            #+ppc64-target (= (the fixnum (logand subtype ppc64::fulltagmask))
                              ppc64::ivector-class-8-bit)
            #+x8632-target (and (<= x8632::min-8-bit-ivector-subtag subtype)
                                (<= subtype x8632::max-8-bit-ivector-subtag))
            #+x8664-target (and (>= subtype x8664::min-8-bit-ivector-subtag)
                                (<= subtype x8664::max-8-bit-ivector-subtag))
      (report-bad-arg buf `(or (array character)
			       (array (unsigned-byte 8))
			       (array (signed-byte 8))))))
  (values buf offset))

(defmethod send-to ((socket udp-socket) msg size
		    &key remote-host remote-port offset)
  "Send a UDP packet over a socket."
  (let ((fd (socket-device socket)))
    (multiple-value-setq (msg offset) (verify-socket-buffer msg offset size))
    (unless remote-host
      (setq remote-host (or (getf (socket-keys socket) :remote-host)
			    (remote-socket-info socket :host))))
    (unless remote-port
      (setq remote-port (or (getf (socket-keys socket) :remote-port)
			    (remote-socket-info socket :port))))
    (rlet ((sockaddr :sockaddr_in))
      (setf (pref sockaddr :sockaddr_in.sin_family) #$AF_INET)
      (setf (pref sockaddr
                  #-solaris-target :sockaddr_in.sin_addr.s_addr
                  #+solaris-target #>sockaddr_in.sin_addr.S_un.S_addr)
	    (if remote-host (host-as-inet-host remote-host) #$INADDR_ANY))
      (setf (pref sockaddr :sockaddr_in.sin_port)
	    (if remote-port (port-as-inet-port remote-port "udp") 0))
      (%stack-block ((bufptr size))
        (%copy-ivector-to-ptr msg offset bufptr 0 size)
	(socket-call socket "sendto"
	  (with-eagain fd :output
	    (c_sendto fd bufptr size 0 sockaddr (record-length :sockaddr_in))))))))

(defmethod receive-from ((socket udp-socket) size &key buffer extract offset)
  "Read a UDP packet from a socket. If no packets are available, wait for
a packet to arrive. Returns four values:
  The buffer with the data
  The number of bytes read
  The 32-bit unsigned IP address of the sender of the data
  The port number of the sender of the data."
  (let ((fd (socket-device socket))
	(vec-offset offset)
	(vec buffer)
	(ret-size -1))
    (when vec
      (multiple-value-setq (vec vec-offset)
	(verify-socket-buffer vec vec-offset size)))
    (rlet ((sockaddr :sockaddr_in)
	   (namelen :signed))
      (setf (pref sockaddr :sockaddr_in.sin_family) #$AF_INET)
      (setf (pref sockaddr
                  #-solaris-target :sockaddr_in.sin_addr.s_addr
                  #+solaris-target #>sockaddr_in.sin_addr.S_un.S_addr)
            #$INADDR_ANY)
      (setf (pref sockaddr :sockaddr_in.sin_port) 0)
      (setf (pref namelen :signed) (record-length :sockaddr_in))
      (%stack-block ((bufptr size))
	(setq ret-size (socket-call socket "recvfrom"
			 (with-eagain fd :input
			   (c_recvfrom fd bufptr size 0 sockaddr namelen))))
	(unless vec
	  (setq vec (make-array ret-size
				:element-type
				(ecase (socket-format socket)
				  ((:text) 'base-char)
				  ((:binary :bivalent) '(unsigned-byte 8))))
		vec-offset 0))
	(%copy-ptr-to-ivector bufptr 0 vec vec-offset ret-size))
      (values (cond ((null buffer)
		     vec)
		    ((or (not extract)
			 (and (eql 0 (or offset 0))
			      (eql ret-size (length buffer))))
		     buffer)
		    (t 
		     (subseq vec vec-offset (+ vec-offset ret-size))))
	      ret-size
	      (ntohl (pref sockaddr
                           #-solaris-target :sockaddr_in.sin_addr.s_addr
                           #+solaris-target #>sockaddr_in.sin_addr.S_un.S_addr))
	      (ntohs (pref sockaddr :sockaddr_in.sin_port))))))

(defgeneric shutdown (socket &key direction)
  (:documentation
   "Shut down part of a bidirectional connection. This is useful if e.g.
you need to read responses after sending an end-of-file signal."))

(defmethod shutdown (socket &key direction)
  ;; TODO: should we ignore ENOTCONN error?  (at least make sure it
  ;; is a distinct, catchable error type).
  (let ((fd (socket-device socket)))
    (socket-call socket "shutdown"
      (c_shutdown fd (ecase direction
		       (:input 0)
		       (:output 1))))))

;; Accepts port as specified by user, returns port number in network byte
;; order.  Protocol should be one of "tcp" or "udp".  Error if not known.
(defun port-as-inet-port (port proto)
  (or (etypecase port
	(fixnum (htons port))
	(string (_getservbyname port proto))
	(symbol (_getservbyname (string-downcase (symbol-name port)) proto)))
      (socket-error nil "getservbyname" (- #$ENOENT))))

(defun lookup-port (port proto)
  "Find the port number for the specified port and protocol."
  (if (fixnump port)
    port
    (ntohs (port-as-inet-port port proto))))

;; Accepts host as specified by user, returns host number in network byte
;; order.
(defun host-as-inet-host (host)
  (etypecase host
    (integer (htonl host))
    (string (or (and (every #'(lambda (c) (position c ".0123456789")) host)
		     (_inet_aton host))
		(multiple-value-bind (addr err) (c_gethostbyname host)
		  (or addr
		      (socket-error nil "gethostbyname" err t)))))))


(defun dotted-to-ipaddr (name &key (errorp t))
  "Convert a dotted-string representation of a host address to a 32-bit
unsigned IP address."
  (let ((addr (_inet_aton name)))
    (if addr (ntohl addr)
      (and errorp (error "Invalid dotted address ~s" name)))))
    
(defun lookup-hostname (host)
  "Convert a host spec in any of the acceptable formats into a 32-bit
unsigned IP address."
  (if (typep host 'integer)
    host
    (ntohl (host-as-inet-host host))))

(defun ipaddr-to-dotted (addr &key values)
  "Convert a 32-bit unsigned IP address into octets."
  (if values
      (values (ldb (byte 8 24) addr)
	      (ldb (byte 8 16) addr)
	      (ldb (byte 8  8) addr)
	      (ldb (byte 8  0) addr))
    (_inet_ntoa (htonl addr))))

(defun ipaddr-to-hostname (ipaddr &key ignore-cache)
  "Convert a 32-bit unsigned IP address into a host name string."
  (declare (ignore ignore-cache))
  (multiple-value-bind (name err) (c_gethostbyaddr (htonl ipaddr))
    (or name (socket-error nil "gethostbyaddr" err t))))
  

(defun int-getsockopt (socket level optname)
  (rlet ((valptr :signed)
         (vallen :signed))
    (setf (pref vallen :signed) 4)
    (let* ((err (c_getsockopt socket level optname valptr vallen)))
      (if (and (eql 0 err)
               (eql 4 (pref vallen :signed)))
        (pref valptr :signed)
	(socket-error socket "getsockopt" err)))))

(defun timeval-setsockopt (socket level optname timeout)
    (multiple-value-bind (seconds micros)
        (microseconds timeout)
      (rlet ((valptr :timeval :tv_sec seconds :tv_usec micros))
        (socket-call socket "setsockopt"
          (c_setsockopt socket level optname valptr (record-length :timeval))))))
                   
(defun int-setsockopt (socket level optname optval)
  (rlet ((valptr :signed))
    (setf (pref valptr :signed) optval)
    (socket-call socket "setsockopt"
      (c_setsockopt socket level optname valptr (record-length :signed)))))

#+(or darwin-target linux-target)
(defloadvar *h-errno-variable-address* nil)
#+linux-target
(defloadvar *h-errno-function-address* nil)

(defun h-errno-location ()
  #+darwin-target
  ;; As of Tiger, Darwin doesn't seem to have grasped the concept
  ;; of thread-local storage for h_errno.
  (or *h-errno-variable-address*
      (setq *h-errno-variable-address* (foreign-symbol-address "_h_errno")))
  ;; Supported versions of FreeBSD seem to have grasped that concept.
  #+freebsd-target
  (#_ __h_error)
  #+linux-target
  ;; Current versions of Linux support thread-specific h_errno,
  ;; but older versions may not.
  (if *h-errno-function-address*
    (ff-call *h-errno-function-address* :address)
    (or *h-errno-variable-address*
        (let* ((entry (foreign-symbol-entry "__h_errno_location")))
          (if entry
            (ff-call (setq *h-errno-function-address* entry) :address)
            (setq *h-errno-variable-address*
                  (foreign-symbol-address  "h_errno")))))))
            

#+(or darwin-target freebsd-target)
(defun c_gethostbyaddr (addr)
  (rlet ((addrp :unsigned))
    (setf (pref addrp :unsigned) addr)
    (without-interrupts
     (let* ((hp (#_gethostbyaddr addrp (record-length :unsigned) #$AF_INET)))
       (declare (dynamic-extent hp))
       (if (not (%null-ptr-p hp))
	 (%get-cstring (pref hp :hostent.h_name))
	 (values nil (pref (h-errno-location) :signed)))))))

#+linux-target
(defun c_gethostbyaddr (addr)
  (rlet ((hostent :hostent)
	 (hp (* (struct :hostent)))
	 (herr :signed)
	 (addrp :unsigned))
    (setf (pref addrp :unsigned) addr)
    (do* ((buflen 1024 (+ buflen buflen))) ()
      (declare (fixnum buflen))
      (%stack-block ((buf buflen))
	(let* ((res (#_gethostbyaddr_r addrp (record-length :unsigned) #$AF_INET
				       hostent buf buflen hp herr)))
	  (declare (fixnum res))
	  (unless (eql res #$ERANGE)
	    (return
	     (if (and (eql res 0) (not (%null-ptr-p (%get-ptr hp))))
		 (%get-cstring (pref (%get-ptr hp) :hostent.h_name))
	       (values nil (- (pref herr :signed)))))))))))

#+solaris-target
(defun c_gethostbyaddr (addr)
  (rlet ((hostent :hostent)
	 (herr :signed)
	 (addrp :unsigned))
    (setf (pref addrp :unsigned) addr)
    (do* ((buflen 1024 (+ buflen buflen))) ()
      (declare (fixnum buflen))
      (%stack-block ((buf buflen))
	(let* ((res (#_gethostbyaddr_r addrp (record-length :unsigned) #$AF_INET
				       hostent buf buflen herr)))
          (if (%null-ptr-p res)
            (unless (eql (%get-errno) (- #$ERANGE))
              (return (values nil (- (pref herr :signed)))))
            (return (%get-cstring (pref res :hostent.h_name)))))))))

#+(or darwin-target freebsd-target)
(defun c_gethostbyname (name)
  (with-cstrs ((name (string name)))
    (without-interrupts
     (let* ((hp (#_gethostbyname  name)))
       (declare (dynamic-extent hp))
       (if (not (%null-ptr-p hp))
	 (%get-unsigned-long
	  (%get-ptr (pref hp :hostent.h_addr_list)))
	 (values nil (pref (h-errno-location) :signed)))))))

#+linux-target
(defun c_gethostbyname (name)
  (with-cstrs ((name (string name)))
    (rlet ((hostent :hostent)
           (hp (* (struct :hostent)))
           (herr :signed 0))
       (do* ((buflen 1024 (+ buflen buflen))) ()
         (declare (fixnum buflen))
         (%stack-block ((buf buflen))
           (let* ((res (#_gethostbyname_r name hostent buf buflen hp herr)))
             (declare (fixnum res))
             (unless (eql res #$ERANGE)
	       (return
                 (let* ((err (pref herr :signed)))
		 (if (and (eql res 0) (eql err 0))
		   (%get-unsigned-long
		    (%get-ptr (pref (%get-ptr hp) :hostent.h_addr_list)))
		   (values nil (- err))))))))))))

#+solaris-target
(defun c_gethostbyname (name)
  (with-cstrs ((name (string name)))
    (rlet ((hostent :hostent)
           (herr :signed 0))
       (do* ((buflen 1024 (+ buflen buflen))) ()
         (declare (fixnum buflen))
         (%stack-block ((buf buflen))
           (setf (pref herr :signed) 0)
           (let* ((res (#_gethostbyname_r name hostent buf buflen herr)))
             (if (%null-ptr-p res)
               (unless (eql (%get-errno) (- #$ERANGE))
                 (return (values nil (- (pref herr :signed)))))
               (return
                 (%get-unsigned-long
                  (%get-ptr (pref res :hostent.h_addr_list)))))))))))
  

(defun _getservbyname (name proto)
  (with-cstrs ((name (string name))
	       (proto (string proto)))
    (let* ((servent-ptr (%null-ptr)))
      (declare (dynamic-extent servent-ptr))
      (%setf-macptr servent-ptr (#_getservbyname name proto))
      (unless (%null-ptr-p servent-ptr)
	(pref servent-ptr :servent.s_port)))))

#+linuxppc-target
(defun _inet_ntoa (addr)
  (rlet ((addrp :unsigned))
    (setf (pref addrp :unsigned) addr)
    (with-macptrs ((p))
      (%setf-macptr p (#_inet_ntoa addrp))
      (unless (%null-ptr-p p) (%get-cstring p)))))

;;; On all of these platforms, the argument is a (:struct :in_addr),
;;; a single word that should be passed by value.  The FFI translator
;;; seems to lose the :struct, so just using #_ doesn't work (that
;;; sounds like a bug in the FFI translator.)
#+(or darwin-target linuxx8664-target freebsd-target solaris-target)
(defun _inet_ntoa (addr)
  (with-macptrs ((p))
    (%setf-macptr p (external-call #+darwin-target "_inet_ntoa"
                                   #-darwin-target "inet_ntoa"
				   :unsigned-fullword addr
				   :address))
    (unless (%null-ptr-p p) (%get-cstring p))))				   


(defun _inet_aton (string)
  (with-cstrs ((name string))
    (rlet ((addr :in_addr))
      (let* ((result #+freebsd-target (#___inet_aton name addr)
                     #-freebsd-target (#_inet_aton name addr)))
	(unless (eql result 0)
	  (pref addr
                #-solaris-target :in_addr.s_addr
                #+solaris-target #>in_addr.S_un.S_addr
                ))))))

(defun c_socket_1 (domain type protocol)
  #-(or linuxppc-target solaris-target)
  (syscall syscalls::socket domain type protocol)
  #+linuxppc-target
  (rlet ((params (:array :unsigned-long 3)))
    (setf (paref params (:* :unsigned-long) 0) domain
          (paref params (:* :unsigned-long) 1) type
          (paref params (:* :unsigned-long) 2) protocol)
    (syscall syscalls::socketcall 1 params))
  #+solaris-target
  (syscall syscalls::so_socket domain type protocol +null-ptr+ #$SOV_DEFAULT))

(defun c_socket (domain type protocol)
  (let* ((fd (c_socket_1 domain type protocol)))
    (when (or (eql fd (- #$EMFILE))
              (eql fd (- #$ENFILE)))
      (gc)
      (drain-termination-queue)
      (setq fd (c_socket_1 domain type protocol)))
    fd))
      

(defun init-unix-sockaddr (addr path)
  (macrolet ((sockaddr_un-path-len ()
               (/ (ensure-foreign-type-bits
                   (foreign-record-field-type 
                    (%find-foreign-record-type-field
                     (parse-foreign-type '(:struct :sockaddr_un)) :sun_path)))
                  8)))
    (let* ((name (native-translated-namestring path))
           (namelen (length name))
           (pathlen (sockaddr_un-path-len))
           (copylen (min (1- pathlen) namelen)))
      (setf (pref addr :sockaddr_un.sun_family) #$AF_UNIX)
      (let* ((sun-path (pref addr :sockaddr_un.sun_path)))
        (dotimes (i copylen)
          (setf (%get-unsigned-byte sun-path i)
                (let* ((code (char-code (schar name i))))
                  (if (> code 255)
                    (char-code #\Sub)
                    code))))))))

(defun bind-unix-socket (socketfd path)
  (rletz ((addr :sockaddr_un))
    (init-unix-sockaddr addr path)
    (socket-call
     nil
     "bind"
     (c_bind socketfd
             addr
             (+ 2
                (#_strlen
                 (pref addr :sockaddr_un.sun_path)))))))
      

(defun c_bind (sockfd sockaddr addrlen)
  #-linuxppc-target
  (progn
    #+(or darwin-target freebsd-target)
    (setf (pref sockaddr :sockaddr_in.sin_len) addrlen)
    (syscall syscalls::bind sockfd sockaddr addrlen))
  #+linuxppc-target
  (progn
    #+ppc32-target
    (%stack-block ((params 12))
      (setf (%get-long params 0) sockfd
            (%get-ptr params 4) sockaddr
            (%get-long params 8) addrlen)
      (syscall syscalls::socketcall 2 params))
    #+ppc64-target
    (%stack-block ((params 24))
      (setf (%%get-unsigned-longlong params 0) sockfd
            (%get-ptr params 8) sockaddr
            (%%get-unsigned-longlong params 16) addrlen)
      (syscall syscalls::socketcall 2 params))))


;;; If attempts to connnect are interrupted, we basically have to
;;; wait in #_select (or the equivalent).  There's a good rant
;;; about these issues in:
;;; <http://www.madore.org/~david/computers/connect-intr.html>
(defun c_connect (sockfd addr len &optional timeout-in-milliseconds)
  (let* ((flags (fd-get-flags sockfd)))
    (unwind-protect
         (progn
           (fd-set-flags sockfd (logior flags #$O_NONBLOCK))
           (let* ((err 
                   #-linuxppc-target
                   (syscall syscalls::connect sockfd addr len)
                   #+linuxppc-target
                   (progn
                     #+ppc32-target
                     (%stack-block ((params 12))
                       (setf (%get-long params 0) sockfd
                             (%get-ptr params 4) addr
                             (%get-long params 8) len)
                       (syscall syscalls::socketcall 3 params))
                     #+ppc64-target
                     (%stack-block ((params 24))
                       (setf (%%get-unsigned-longlong params 0) sockfd
                             (%get-ptr params 8) addr
                             (%%get-unsigned-longlong params 16) len)
                       (syscall syscalls::socketcall 3 params)))))
             (cond ((or (eql err (- #$EINPROGRESS)) (eql err (- #$EINTR)))
                    (if (process-output-wait sockfd timeout-in-milliseconds)
                      (- (int-getsockopt sockfd #$SOL_SOCKET #$SO_ERROR))
                      (- #$ETIMEDOUT)))
                   (t err))))
      (fd-set-flags sockfd flags))))

(defun c_listen (sockfd backlog)
  #-linuxppc-target
  (syscall syscalls::listen sockfd backlog)
  #+linuxppc-target
  (progn
    #+ppc32-target
    (%stack-block ((params 8))
      (setf (%get-long params 0) sockfd
            (%get-long params 4) backlog)
      (syscall syscalls::socketcall 4 params))
    #+ppc64-target
    (%stack-block ((params 16))
      (setf (%%get-unsigned-longlong params 0) sockfd
            (%%get-unsigned-longlong params 8) backlog)
      (syscall syscalls::socketcall 4 params))))

(defun c_accept (sockfd addrp addrlenp)
  (ignoring-eintr 
   #-linuxppc-target
   (syscall syscalls::accept sockfd addrp addrlenp)
   #+linuxppc-target
   (progn
     #+ppc32-target
     (%stack-block ((params 12))
       (setf (%get-long params 0) sockfd
             (%get-ptr params 4) addrp
             (%get-ptr params 8) addrlenp)
       (syscall syscalls::socketcall 5 params))
     #+ppc64-target
     (%stack-block ((params 24))
       (setf (%%get-unsigned-longlong params 0) sockfd
             (%get-ptr params 8) addrp
             (%get-ptr params 16) addrlenp)
       (syscall syscalls::socketcall 5 params)))))

(defun c_getsockname (sockfd addrp addrlenp)
  #-linuxppc-target
  (syscall syscalls::getsockname sockfd addrp addrlenp)
  #+linuxppc-target
  (progn
    #+ppc32-target
    (%stack-block ((params 12))
      (setf (%get-long params 0) sockfd
            (%get-ptr params 4) addrp
            (%get-ptr params 8) addrlenp)
      (syscall syscalls::socketcall 6 params))
    #+ppc64-target
    (%stack-block ((params 24))
      (setf (%%get-unsigned-longlong params 0) sockfd
            (%get-ptr params 8) addrp
            (%get-ptr params 16) addrlenp)
      (syscall syscalls::socketcall 6 params))))

(defun c_getpeername (sockfd addrp addrlenp)
  #-linuxppc-target
  (syscall syscalls::getpeername sockfd addrp addrlenp)
  #+linuxppc-target
  (progn
    #+ppc32-target
    (%stack-block ((params 12))
      (setf (%get-long params 0) sockfd
            (%get-ptr params 4) addrp
            (%get-ptr params 8) addrlenp)
      (syscall syscalls::socketcall 7 params))
    #+ppc64-target
    (%stack-block ((params 24))
      (setf (%%get-unsigned-longlong params 0) sockfd
            (%get-ptr params 8) addrp
            (%get-ptr params 16) addrlenp)
      (syscall syscalls::socketcall 7 params))))

(defun c_socketpair (domain type protocol socketsptr)
  #-(or linuxppc-target solaris-target)
  (syscall syscalls::socketpair domain type protocol socketsptr)
  #+linuxppc-target
  (progn
    #+ppc32-target
    (%stack-block ((params 16))
      (setf (%get-long params 0) domain
            (%get-long params 4) type
            (%get-long params 8) protocol
            (%get-ptr params 12) socketsptr)
      (syscall syscalls::socketcall 8 params))
    #+ppc64-target
    (%stack-block ((params 32))
      (setf (%%get-unsigned-longlong params 0) domain
            (%%get-unsigned-longlong params 8) type
            (%%get-unsigned-longlong params 16) protocol
            (%get-ptr params 24) socketsptr)
      (syscall syscalls::socketcall 8 params)))
  #+solaris-target
  (let* ((fd1 (syscall syscalls::so_socket domain type protocol +null-ptr+ #$SOV_DEFAULT)))
    (if (>= fd1 0)
      (let* ((fd2 (syscall syscalls::so_socket domain type protocol +null-ptr+ #$SOV_DEFAULT)))
        (if (>= fd2 0)
          (progn
            (setf (paref socketsptr (:* :int) 0) fd1
                  (paref socketsptr (:* :int) 1) fd2)
            (let* ((res (syscall syscalls::so_socketpair socketsptr)))
              (when (< res 0)
                (fd-close fd1)
                (fd-close fd2))
              res))
          (progn
            (fd-close fd1)
            fd2)))
      fd1)))




(defun c_sendto (sockfd msgptr len flags addrp addrlen)
  #-linuxppc-target
  (syscall syscalls::sendto sockfd msgptr len flags addrp addrlen)
  #+linuxppc-target
  (progn
    #+ppc32-target
    (%stack-block ((params 24))
      (setf (%get-long params 0) sockfd
            (%get-ptr params  4) msgptr
            (%get-long params 8) len
            (%get-long params 12) flags
            (%get-ptr params  16) addrp
            (%get-long params 20) addrlen)
      (syscall syscalls::socketcall 11 params))
    #+ppc64-target
    (%stack-block ((params 48))
      (setf (%%get-unsigned-longlong params 0) sockfd
            (%get-ptr params  8) msgptr
            (%%get-unsigned-longlong params 16) len
            (%%get-unsigned-longlong params 24) flags
            (%get-ptr params  32) addrp
            (%%get-unsigned-longlong params 40) addrlen)
      (syscall syscalls::socketcall 11 params))))

(defun c_recvfrom (sockfd bufptr len flags addrp addrlenp)
  #-linuxppc-target
  (syscall syscalls::recvfrom sockfd bufptr len flags addrp addrlenp)
  #+linuxppc-target
  (progn
    #+ppc32-target
    (%stack-block ((params 24))
      (setf (%get-long params 0) sockfd
            (%get-ptr params  4) bufptr
            (%get-long params 8) len
            (%get-long params 12) flags
            (%get-ptr params  16) addrp
            (%get-ptr params  20) addrlenp)
      (syscall syscalls::socketcall 12 params))
    #+ppc64-target
    (%stack-block ((params 48))
      (setf (%get-long params 0) sockfd
            (%get-ptr params  8) bufptr
            (%get-long params 16) len
            (%get-long params 24) flags
            (%get-ptr params  32) addrp
            (%get-ptr params  40) addrlenp)
      (syscall syscalls::socketcall 12 params))))

(defun c_shutdown (sockfd how)
  #-linuxppc-target
  (syscall syscalls::shutdown sockfd how)
  #+linuxppc-target
  (progn
    #+ppc32-target
    (%stack-block ((params 8))
      (setf (%get-long params 0) sockfd
            (%get-long params 4) how)
      (syscall syscalls::socketcall 13 params))
    #+ppc64-target
    (%stack-block ((params 16))
      (setf (%%get-unsigned-longlong params 0) sockfd
            (%%get-unsigned-longlong params 8) how)
      (syscall syscalls::socketcall 13 params))))

(defun c_setsockopt (sockfd level optname optvalp optlen)
  #-linux-ppc-target
  (syscall syscalls::setsockopt sockfd level optname optvalp optlen)
  #+linuxppc-target
  (progn
    #+ppc32-target
    (%stack-block ((params 20))
      (setf (%get-long params 0) sockfd
            (%get-long params 4) level
            (%get-long params 8) optname
            (%get-ptr params 12) optvalp
            (%get-long params 16) optlen)
      (syscall syscalls::socketcall 14 params))
    #+ppc64-target
    (%stack-block ((params 40))
      (setf (%%get-unsigned-longlong params 0) sockfd
            (%%get-unsigned-longlong params 8) level
            (%%get-unsigned-longlong params 16) optname
            (%get-ptr params 24) optvalp
            (%%get-unsigned-longlong params 32) optlen)
      (syscall syscalls::socketcall 14 params))))

(defun c_getsockopt (sockfd level optname optvalp optlenp)
  #-linuxppc-target
  (syscall syscalls::getsockopt sockfd level optname optvalp optlenp)
  #+linuxppc-target
  (progn
    #+ppc32-target
    (%stack-block ((params 20))
      (setf (%get-long params 0) sockfd
            (%get-long params 4) level
            (%get-long params 8) optname
            (%get-ptr params 12) optvalp
            (%get-ptr params 16) optlenp)
      (syscall syscalls::socketcall 15 params))
    #+ppc64-target
    (%stack-block ((params 40))
      (setf (%%get-unsigned-longlong params 0) sockfd
            (%%get-unsigned-longlong params 8) level
            (%%get-unsigned-longlong params 16) optname
            (%get-ptr params 24) optvalp
            (%get-ptr params 32) optlenp)
      (syscall syscalls::socketcall 15 params))))

(defun c_sendmsg (sockfd msghdrp flags)
  #-linuxppc-target
  (syscall syscalls::sendmsg sockfd msghdrp flags)
  #+linuxppc-target
  (progn
    #+ppc32-target
    (%stack-block ((params 12))
      (setf (%get-long params 0) sockfd
            (%get-ptr params 4) msghdrp
            (%get-long params 8) flags)
      (syscall syscalls::socketcall 16 params))
    #+ppc64-target
    (%stack-block ((params 24))
      (setf (%%get-unsigned-longlong params 0) sockfd
            (%get-ptr params 8) msghdrp
            (%%get-unsigned-longlong params 16) flags)
      (syscall syscalls::socketcall 16 params))))

(defun c_recvmsg (sockfd msghdrp flags)
  #-linuxppc-target
  (syscall syscalls::recvmsg sockfd msghdrp flags)
  #+linuxppc-target
  (progn
    #+ppc32-target
    (%stack-block ((params 12))
      (setf (%get-long params 0) sockfd
            (%get-ptr params 4) msghdrp
            (%get-long params 8) flags)
      (syscall syscalls::socketcall 17 params))
    #+ppc64-target
    (%stack-block ((params 24))
      (setf (%%get-unsigned-longlong params 0) sockfd
            (%get-ptr params 8) msghdrp
            (%%get-unsigned-longlong params 16) flags)
      (syscall syscalls::socketcall 17 params))))

;;; Return a list of currently configured interfaces, a la ifconfig.
(defstruct ip-interface
  name
  addr
  netmask
  flags
  address-family)

(defun dump-buffer (p n)
  (dotimes (i n (progn (terpri) (terpri)))
    (unless (logtest i 15)
      (format t "~&~8,'0x: " (%ptr-to-int (%inc-ptr p i))))
    (format t " ~2,'0x" (%get-byte p i))))

#-solaris-target
(defun %get-ip-interfaces ()
  (rlet ((p :address (%null-ptr)))
    (if (zerop (#_getifaddrs p))
      (unwind-protect
           (do* ((q (%get-ptr p) (pref q :ifaddrs.ifa_next))
                 (res ()))
                ((%null-ptr-p q) (nreverse res))
             (let* ((addr (pref q :ifaddrs.ifa_addr)))
               (when (and (not (%null-ptr-p addr))
                          (eql (pref addr :sockaddr.sa_family) #$AF_INET))
                 (push (make-ip-interface
                        :name (%get-cstring (pref q :ifaddrs.ifa_name))
                        :addr (pref addr :sockaddr_in.sin_addr.s_addr)
                        :netmask (pref (pref q :ifaddrs.ifa_netmask)
                                       :sockaddr_in.sin_addr.s_addr)
                        :flags (pref q :ifaddrs.ifa_flags)
                        :address-family #$AF_INET)
                       res))))
        (#_freeifaddrs (pref p :address))))))

#+solaris-target
(progn
  ;;; Interface translator has trouble with a lot of ioctl constants.
  (eval-when (:compile-toplevel :execute)
    (defconstant os::|SIOCGLIFNUM| #xc00c6982)
    (defconstant os::|SIOCGLIFCONF| #xc01069a5)
    (defconstant os::|SIOCGLIFADDR| #xc0786971)
    (defconstant os::|SIOCGLIFFLAGS| #xc0786975)
    (defconstant os::|SIOCGLIFNETMASK| #xc078697d)
    )

(defun %get-ip-interfaces ()
  (let* ((sock (c_socket #$AF_INET #$SOCK_DGRAM #$IPPROTO_UDP))
         (res nil))
    (when (>= sock 0)
      (unwind-protect
           (let* ((flags (logior #$LIFC_NOXMIT #$LIFC_TEMPORARY #$LIFC_ALLZONES))
                  (ninterfaces (rlet ((lifnum :lifnum
                                        :lifn_flags flags
                                        :lifn_family #$AF_INET
                                        :lifn_count 0))
                                 (#_ioctl sock os::SIOCGLIFNUM :address lifnum)
                                 (pref lifnum :lifnum.lifn_count))))
             (declare (fixnum ninterfaces))
             (when (> ninterfaces 0)
               (let* ((bufsize (* ninterfaces (record-length :lifreq))))
                 (%stack-block ((buf bufsize :clear t))
                   (rlet ((lifc :lifconf
                            :lifc_family #$AF_INET
                            :lifc_flags flags
                            :lifc_len bufsize
                            :lifc_lifcu.lifcu_buf buf))
                     (when (>= (#_ioctl sock os::SIOCGLIFCONF :address lifc) 0)
                       (do* ((i 0 (1+ i))
                             (p (pref lifc :lifconf.lifc_lifcu.lifcu_buf)
                                (%inc-ptr p (record-length :lifreq))))
                            ((= i ninterfaces))
                         (let* ((name (%get-cstring (pref p :lifreq.lifr_name)))
                                (address-family (pref p :lifreq.lifr_lifru.lifru_addr.ss_family))
                                (if-flags nil)
                                (address nil)
                                (netmask nil))
                           (if (>= (#_ioctl sock os::SIOCGLIFFLAGS :address p)
                                   0)
                             (setq if-flags (pref p :lifreq.lifr_lifru.lifru_flags)))
                           (if (>= (#_ioctl sock os::SIOCGLIFADDR :address p)
                                   0)
                             (setq address (pref
                                            (pref p :lifreq.lifr_lifru.lifru_addr)
                                            #>sockaddr_in.sin_addr.S_un.S_addr)))
                           (if (>= (#_ioctl sock os::SIOCGLIFNETMASK :address p)
                                   0)
                             (setq netmask (pref
                                            (pref p :lifreq.lifr_lifru.lifru_subnet)
                                            #>sockaddr_in.sin_addr.S_un.S_addr)))
                             
                           (push (make-ip-interface
                                  :name name
                                  :addr address
                                  :netmask netmask
                                  :flags if-flags
                                  :address-family address-family)
                                 res)))))))))
        (fd-close sock)))
    res))
)

             

      


(defloadvar *ip-interfaces* ())

(defun ip-interfaces ()
  (or *ip-interfaces*
      (setq *ip-interfaces* (%get-ip-interfaces))))

;;; This should presumably happen after a configuration change.
;;; How do we detect a configuration change ?
(defun %reset-ip-interfaces ()
  (setq *ip-interfaces* ()))

;;; Return the first non-loopback interface that's up and whose address
;;; family is #$AF_INET.  If no such interface exists, return
;;; the loopback interface.
(defun primary-ip-interface ()
  (let* ((ifaces (ip-interfaces)))
    (or (find-if #'(lambda (i)
		     (and (eq #$AF_INET (ip-interface-address-family i))
                          (ip-interface-addr i)
			  (let* ((flags (ip-interface-flags i)))
			    (and (not (logtest #$IFF_LOOPBACK flags))
				 (logtest #$IFF_UP flags)))))
		 ifaces)
	(car ifaces))))

(defun primary-ip-interface-address ()
  (let* ((iface (primary-ip-interface)))
    (if iface
      (ip-interface-addr iface)
      (error "Can't determine primary IP interface"))))
	  
	  
(defmethod stream-io-error ((stream socket) errno where)
  (socket-error stream where errno))
