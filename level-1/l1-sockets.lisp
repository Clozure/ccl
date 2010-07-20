;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2001-2009 Clozure Associates
;;;   This file is part of Clozure CL.  
;;;
;;;   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with Clozure CL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with Clozure CL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   Clozure CL is referenced in the preamble as the "LIBRARY."
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
	    SOCKET-CREATION-ERROR
	    SOCKET-CREATION-ERROR-CODE
	    SOCKET-CREATION-ERROR-IDENTIFIER
	    SOCKET-CREATION-ERROR-SITUATION
	    WITH-OPEN-SOCKET))
  #+windows-target
  (defmacro check-winsock-error (form)
    (let* ((val (gensym)))
      `(let* ((,val ,form))
        (if (< ,val 0)
          (%get-winsock-error)
          ,val))))
  (defmacro check-socket-error (form)
    #+windows-target `(check-winsock-error ,form)
    #-windows-target `(int-errno-call ,form))
  )


#+windows-target
(defun %get-winsock-error ()
  (- (#_WSAGetLastError)))

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
		"SOCKET-CREATION-ERROR"
		"SOCKET-CREATION-ERROR-CODE"
		"SOCKET-CREATION-ERROR-IDENTIFIER"
		"SOCKET-CREATION-ERROR-SITUATION"
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
	    "SOCKET-CREATION-ERROR"
	    "SOCKET-CREATION-ERROR-CODE"
	    "SOCKET-CREATION-ERROR-IDENTIFIER"
	    "SOCKET-CREATION-ERROR-SITUATION"
	    "WITH-OPEN-SOCKET"))

(define-condition socket-error (simple-stream-error)
  ((code :initarg :code :reader socket-error-code)
   (identifier :initform :unknown :initarg :identifier :reader socket-error-identifier)
   (situation :initarg :situation :reader socket-error-situation)))

(define-condition socket-creation-error (simple-error)
  ((code :initarg :code :reader socket-creation-error-code)
   (identifier :initform :unknown :initarg :identifier :reader socket-creation-error-identifier)
   (situation :initarg :situation :reader socket-creation-error-situation)))

(defparameter *gai-error-identifiers*
  (list #$EAI_AGAIN :try-again
	#$EAI_FAIL :no-recovery
	#$EAI_NONAME :host-not-found))

(defvar *socket-error-identifiers*
  #-windows-target
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
	#$EPERM :access-denied)
  #+windows-target
  (list #$WSAEADDRINUSE :address-in-use
	#$WSAECONNABORTED :connection-aborted
	#$WSAENOBUFS :no-buffer-space
	#$ENOMEM :no-buffer-space
	#$ENFILE :no-buffer-space
	#$WSAETIMEDOUT :connection-timed-out
	#$WSAECONNREFUSED :connection-refused
	#$WSAENETUNREACH :host-unreachable
	#$WSAEHOSTUNREACH :host-unreachable
	#$WSAEHOSTDOWN :host-down
	#$WSAENETDOWN :network-down
	#$WSAEADDRNOTAVAIL :address-not-available
	#$WSAENETRESET :network-reset
	#$WSAECONNRESET :connection-reset
	#$WSAESHUTDOWN :shutdown
	#$EACCES :access-denied
	#$EPERM :access-denied)
  )


(declaim (inline socket-call))
(defun socket-call (stream where res)
  (if (< res 0)
    (socket-error stream where res)
    res))

#-windows-target
(defun %gai-strerror (err)
  (let ((p (#_gai_strerror err)))
    (if (%null-ptr-p p)
      (format nil "Unknown nameserver error ~d" err)
      (%get-cstring p))))

(defun socket-error (stream where errno &optional nameserver-p)
  "Creates and signals (via error) one of two socket error 
conditions, based on the state of the arguments."
  (unless nameserver-p
    (setq errno (abs errno)))
  (if stream
    (error (make-condition 'socket-error
			   :stream stream
			   :code errno
			   :identifier (getf *socket-error-identifiers* errno :unknown)
			   :situation where
			   :format-control "~a (error #~d) during ~a"
			   :format-arguments (list
					      #+windows-target
					      (%windows-error-string errno)
					      #-windows-target
					      (%strerror errno)
					      errno where)))
    (let ((identifiers (if nameserver-p
			 *gai-error-identifiers*
			 *socket-error-identifiers*)))
      (error (make-condition 'socket-creation-error
			     :code errno
			     :identifier (getf identifiers errno :unknown)
			     :situation where
			     :format-control "~a (error #~d) during socket creation or nameserver operation in ~a"
			     :format-arguments (list
						#+windows-target
						(%windows-error-string errno)
						#-windows-target
						(if nameserver-p
						  (%gai-strerror errno)
						  (%strerror errno))
						errno where))))))

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

#-windows-target
(defmethod close :before ((s file-listener-socket) &key abort)
  (declare (ignore abort))
  (let* ((path (local-socket-filename (socket-device s) s)))
    (when path (%delete-file path))))


;; A udp socket just sends and receives packets.
(defclass udp-socket (ip-socket unconnected-socket) ())

(defmethod socket-type ((stream udp-socket)) :datagram)
(defmethod socket-connect ((stream udp-socket)) nil)
(defmethod socket-format ((stream udp-socket)) :binary)

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
                                     #-(or solaris-target windows-target) :sockaddr_in.sin_addr.s_addr
                                     #+(or solaris-target windows-target) #>sockaddr_in.sin_addr.S_un.S_addr)))
		 (:port (ntohs (pref sockaddr :sockaddr_in.sin_port))))))))

#-windows-target
(defun path-from-unix-address (addr)
  (when (= #$AF_UNIX (pref addr :sockaddr_un.sun_family))
    #+darwin-target
    (%str-from-ptr (pref addr :sockaddr_un.sun_path)
		   (- (pref addr :sockaddr_un.sun_len) 2))
    #-darwin-target
    (%get-cstring (pref addr :sockaddr_un.sun_path))))

#-windows-target
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
	    (cond ((eql err (- #+windows-target #$WSAENOTCONN #-windows-target #$ENOTCONN)) nil)
		  ((< err 0) (socket-error socket "getpeername" err))
		  (t
		   (when (= #$AF_INET (pref sockaddr :sockaddr_in.sin_family))
		     (ecase type
		       (:host (ntohl (pref sockaddr
                                           #-(or solaris-target windows-target) :sockaddr_in.sin_addr.s_addr
                                           #+(or solaris-target windows-target)  #>sockaddr_in.sin_addr.S_un.S_addr)))
		       (:port (ntohs  (pref sockaddr :sockaddr_in.sin_port)))))))))))

#-windows-target
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

#-windows-target
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

#-windows-target
(defmethod remote-filename ((socket socket))
  (remote-socket-filename socket))
  
(defun set-socket-fd-blocking (fd block-flag)
  #+windows-target
  (rlet ((argp :u_long (if block-flag 0 1)))
    (#_ioctlsocket fd #$FIONBIO argp))
  #-windows-target
  (if block-flag
    (fd-clear-flag fd #$O_NONBLOCK)
    (fd-set-flag fd #$O_NONBLOCK)))

(defun get-socket-fd-blocking (fd)
  "returns T iff socket is in blocking mode"
  #+windows-target (declare (ignore fd))
  #+windows-target t
  #-windows-target
  (not (logtest #$O_NONBLOCK (fd-get-flags fd))))

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
    (when (eq address-family :internet)
      (when (eq type :stream)
	(rlet ((plinger :linger))
	  (setf (pref plinger :linger.l_onoff) (if linger 1 0)
		(pref plinger :linger.l_linger) (or linger 0))
	  (socket-call socket "setsockopt"
		       (c_setsockopt fd #$SOL_SOCKET #$SO_LINGER
				     plinger (record-length :linger)))))
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
                             #-(or solaris-target windows-target) :sockaddr_in.sin_addr.s_addr
                             #+(or solaris-target windows-target) #>sockaddr_in.sin_addr.S_un.S_addr
                             ) host-n)
		 (socket-call socket "bind" (c_bind fd sockaddr (record-length :sockaddr_in)))))))
    (when (and (eq address-family :file)
	       (eq connect :passive)
	       local-filename)
      #+windows-target (error "can't create file socket on Windows")
      #-windows-target (bind-unix-socket fd local-filename))))

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
                    connect-timeout input-timeout output-timeout deadline
                    fd)
  "Create and return a new socket."
  (declare (dynamic-extent keys))
  (declare (ignore type connect remote-host remote-port eol format
		   keepalive reuse-address nodelay broadcast linger
		   local-port local-host backlog class out-of-band-inline
		   local-filename remote-filename sharing basic external-format
                   auto-close connect-timeout input-timeout output-timeout deadline fd))
  (ecase address-family
    ((:file) (apply #'make-file-socket keys))
    ((nil :internet) (apply #'make-ip-socket keys))))



(defun make-udp-socket (&rest keys &key (fd -1) &allow-other-keys)
  (unwind-protect
    (let (socket)
      (when (< fd 0)
        (setq fd (socket-call nil "socket"
                              (c_socket #$AF_INET #$SOCK_DGRAM #$IPPROTO_UDP))))
      (apply #'set-socket-options fd keys)
      (setq socket (make-instance 'udp-socket
				  :device fd
				  :keys keys))
      (setq fd -1)
      socket)
    (unless (< fd 0)
      (fd-close fd))))

(defun make-tcp-socket (&rest keys &key connect (fd -1) &allow-other-keys)
  (unwind-protect
       (let (socket)
         (when (< fd 0)
           (setq fd (socket-call nil "socket"
                                 (c_socket #$AF_INET #$SOCK_STREAM #$IPPROTO_TCP))))
         (apply #'set-socket-options fd keys)
         (setq socket
               (ecase connect
                 ((nil :active) (apply #'make-tcp-stream-socket fd keys))
                 ((:passive) (apply #'make-tcp-listener-socket fd keys))))
         (setq fd -1)
         socket)
    (unless (< fd 0)
      (fd-close fd))))

(defun make-stream-file-socket (&rest keys &key connect (fd -1) &allow-other-keys)
  (unwind-protect
       (let (socket)
         (when (< fd 0)
           (setq fd (socket-call nil "socket" (c_socket #$PF_UNIX #$SOCK_STREAM 0))))
         (apply #'set-socket-options fd keys)
         (setq socket
               (ecase connect
                 ((nil :active) (apply #'make-file-stream-socket fd keys))
                 ((:passive) (apply #'make-file-listener-socket fd keys))))
         (setq fd -1)
         socket)
    (unless (< fd 0)
      (fd-close fd))))

(defun make-datagram-file-socket (&rest keys)
  (declare (ignore keys))
  (error "Datagram file sockets aren't implemented."))


(defun %socket-connect (fd addr addrlen &optional timeout-in-milliseconds)
  (let* ((err (c_connect fd addr addrlen timeout-in-milliseconds)))
    (declare (fixnum err))
    (unless (eql err 0) (fd-close fd) (socket-error nil "connect" err))))
    
(defun inet-connect (fd host-n port-n &optional timeout-in-milliseconds)
  (rlet ((sockaddr :sockaddr_in))
    (setf (pref sockaddr :sockaddr_in.sin_family) #$AF_INET
          (pref sockaddr :sockaddr_in.sin_port) port-n
          (pref sockaddr
                #-(or solaris-target windows-target) :sockaddr_in.sin_addr.s_addr
                #+(or solaris-target windows-target)  #>sockaddr_in.sin_addr.S_un.S_addr
                ) host-n)
    (%socket-connect fd sockaddr (record-length :sockaddr_in) timeout-in-milliseconds)))

#-windows-target
(defun file-socket-connect (fd remote-filename)
  (rletz ((sockaddr :sockaddr_un))
    (init-unix-sockaddr sockaddr remote-filename)
    (%socket-connect fd sockaddr (record-length :sockaddr_un))))

#+windows-target
(defun file-socket-connect (fd remote-filename)
  (declare (ignore fd))
  (error "Can't create file socket to ~s on Windows" remote-filename))
  
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

(defun socket-accept (fd wait)
  (flet ((_accept (fd async)
	   (let ((res (c_accept fd (%null-ptr) (%null-ptr))))
	     (declare (fixnum res))
	     ;; See the inscrutable note under ERROR HANDLING in
	     ;; man accept(2). This is my best guess at what they mean...
	     (if (and async (< res 0)
                      #+windows-target
                      (= res #$WSAEWOULDBLOCK)
                      #-windows-target
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
	    (let ((was-blocking (get-socket-fd-blocking fd)))
	      (unwind-protect
		  (progn
                    (set-socket-fd-blocking fd nil)
		    (_accept fd t))
		(set-socket-fd-blocking fd was-blocking)))))))

(defun accept-socket-connection (socket wait stream-create-function &optional stream-args)
  (let ((listen-fd (socket-device socket))
	(fd -1))
    (unwind-protect
      (let ((keys (append stream-args (socket-keys socket))))
	(setq fd (socket-accept listen-fd wait))
	(cond ((>= fd 0)
	       (prog1 (apply stream-create-function fd keys)
		 (setq fd -1)))
	      ((eql fd (- #$EAGAIN)) nil)
	      (t (socket-error socket "accept" fd))))
      (when (>= fd 0)
	(fd-close fd)))))

(defgeneric accept-connection (socket &key wait stream-args)
  (:documentation
  "Extract the first connection on the queue of pending connections,
accept it (i.e. complete the connection startup protocol) and return a new
tcp-stream or file-socket-stream representing the newly established
connection.  The tcp stream inherits any properties of the listener socket
that are relevant (e.g. :keepalive, :nodelay, etc.) Additional arguments
may be specified using STREAM-ARGS. The original listener
socket continues to be open listening for more connections, so you can call
accept-connection on it again."))

(defmethod accept-connection ((socket listener-socket) &key (wait t) stream-args)
  (accept-socket-connection socket wait #'make-tcp-stream stream-args))

(defmethod accept-connection ((socket file-listener-socket) &key (wait t) stream-args)
  (accept-socket-connection socket wait #'make-file-socket-stream stream-args))

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
      (report-bad-arg buf '(or (array (unsigned-byte 8))
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
                  #-(or solaris-target windows-target) :sockaddr_in.sin_addr.s_addr
                  #+(or solaris-target windows-target)  #>sockaddr_in.sin_addr.S_un.S_addr)
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
                  #-(or solaris-target windows-target) :sockaddr_in.sin_addr.s_addr
                  #+(or solaris-target windows-target) #>sockaddr_in.sin_addr.S_un.S_addr)
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
				  ((:binary) '(unsigned-byte 8))))
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
                           #-(or solaris-target windows-target) :sockaddr_in.sin_addr.s_addr
                           #+(or solaris-target windows-target) #>sockaddr_in.sin_addr.S_un.S_addr))
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
  (let* ((a (ldb (byte 8 24) addr))
	 (b (ldb (byte 8 16) addr))
	 (c (ldb (byte 8  8) addr))
	 (d (ldb (byte 8  0) addr)))
    (if values
      (values a b c d)
      (format nil "~d.~d.~d.~d" a b c d))))

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



            
(defun c_gethostbyaddr (addr-in-net-byte-order)
  (rletZ ((sin #>sockaddr_in))
    (setf (pref sin :sockaddr_in.sin_family) #$AF_INET
          (pref sin
                #+(or windows-target solaris-target) #>sockaddr_in.sin_addr.S_un.S_addr
                #-(or windows-target solaris-target) #>sockaddr_in.sin_addr.s_addr) addr-in-net-byte-order)
    #+darwin-target (setf (pref sin :sockaddr_in.sin_len) (record-length :sockaddr_in))
    (%stack-block ((namep #$NI_MAXHOST))
      (let* ((err (#_getnameinfo sin (record-length #>sockaddr_in) namep #$NI_MAXHOST (%null-ptr) 0 #$NI_NAMEREQD)))
        (if (eql 0 err)
          (%get-cstring namep)
          (values nil err))))))
                
(defun c_gethostbyname (name)
  (with-cstrs ((name (string name)))
    (rletZ ((hints #>addrinfo)
            (results :address))
      (setf (pref hints #>addrinfo.ai_family) #$AF_INET)
      (let* ((err (#_getaddrinfo name (%null-ptr) hints results)))
        (if (eql 0 err)
          (let* ((info (pref results :address))
                 (sin (pref info #>addrinfo.ai_addr)))
            (prog1
                #+(or windows-target solaris-target)
                (pref sin #>sockaddr_in.sin_addr.S_un.S_addr)
                #-(or windows-target solaris-target)
                (pref sin #>sockaddr_in.sin_addr.s_addr)
                (#_freeaddrinfo info)))
          (values nil err))))))
      
  

  

(defun _getservbyname (name proto)
  (with-cstrs ((name (string name))
	       (proto (string proto)))
    (let* ((servent-ptr (%null-ptr)))
      (declare (dynamic-extent servent-ptr))
      (%setf-macptr servent-ptr (#_getservbyname name proto))
      (unless (%null-ptr-p servent-ptr)
	(pref servent-ptr :servent.s_port)))))

(defun _inet_aton (string)
  (with-cstrs ((name string))
    #-windows-target
    (rlet ((addr :in_addr))
      (let* ((result #+freebsd-target (#___inet_aton name addr)
                     #-freebsd-target (#_inet_aton name addr)))
	(unless (eql result 0)
	  (pref addr
                #-solaris-target :in_addr.s_addr
                #+solaris-target #>in_addr.S_un.S_addr
                ))))
    #+windows-target
    (rlet ((addr :sockaddr_in)
           (addrlenp :int (record-length :sockaddr_in)))
      (setf (pref addr :sockaddr_in.sin_family) #$AF_INET)
      (when (zerop (#_WSAStringToAddressA name #$AF_INET (%null-ptr)  addr addrlenp))
        (pref addr #>sockaddr_in.sin_addr.S_un.S_addr)))))

(defun c_socket_1 (domain type protocol)
  #-windows-target (int-errno-call (#_socket domain type protocol))
  #+windows-target (let* ((handle (#_socket domain type protocol)))
                     (if (< handle 0)
                       (%get-winsock-error)
                       handle)))



(defun c_socket (domain type protocol)
  (let* ((fd (c_socket_1 domain type protocol)))
    (when (or (eql fd (- #$EMFILE))
              (eql fd (- #$ENFILE)))
      (gc)
      (drain-termination-queue)
      (setq fd (c_socket_1 domain type protocol)))
    fd))
      

#-windows-target
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

#-windows-target
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
  (check-socket-error (#_bind sockfd sockaddr addrlen)))


#+windows-target
(defun windows-connect-wait (sockfd timeout-in-milliseconds)
  (if (and timeout-in-milliseconds
           (< timeout-in-milliseconds 0))
    (setq timeout-in-milliseconds nil))
  (rlet ((writefds :fd_set)
         (exceptfds :fd_set)
         (tv :timeval :tv_sec 0 :tv_usec 0))
    (fd-zero writefds)
    (fd-zero exceptfds)
    (fd-set sockfd writefds)
    (fd-set sockfd exceptfds)
    (when timeout-in-milliseconds
      (multiple-value-bind (seconds milliseconds)
          (floor timeout-in-milliseconds 1000)
        (setf (pref tv :timeval.tv_sec) seconds
              (pref tv :timeval.tv_usec) (* 1000 milliseconds))))
    (> (#_select 1 (%null-ptr) writefds exceptfds (if timeout-in-milliseconds tv (%null-ptr))) 0)))
      
      
;;; If attempts to connnect are interrupted, we basically have to
;;; wait in #_select (or the equivalent).  There's a good rant
;;; about these issues in:
;;; <http://www.madore.org/~david/computers/connect-intr.html>
(defun c_connect (sockfd addr len &optional timeout-in-milliseconds)
  (let* ((was-blocking (get-socket-fd-blocking sockfd)))
    (unwind-protect
         (progn
           (set-socket-fd-blocking sockfd nil)
           (let* ((err (check-socket-error (#_connect sockfd addr len))))
             (cond ((or (eql err (- #+windows-target #$WSAEINPROGRESS
                                    
                                    #-windows-target #$EINPROGRESS))
                        #+windows-target (eql err (- #$WSAEWOULDBLOCK))
                        (eql err (- #$EINTR)))
                    (if
                      #+windows-target (windows-connect-wait sockfd timeout-in-milliseconds)
                      #-windows-target (process-output-wait sockfd timeout-in-milliseconds)
                      (- (int-getsockopt sockfd #$SOL_SOCKET #$SO_ERROR))
                      (- #+windows-target #$WSAETIMEDOUT #-windows-target #$ETIMEDOUT)))
                   (t err))))
      (set-socket-fd-blocking sockfd was-blocking))))

(defun c_listen (sockfd backlog)
  (check-socket-error (#_listen sockfd backlog)))

(defun c_accept (sockfd addrp addrlenp)
  (ignoring-eintr
   (check-socket-error (#_accept sockfd addrp addrlenp))))

(defun c_getsockname (sockfd addrp addrlenp)
  (check-socket-error (#_getsockname sockfd addrp addrlenp)))

(defun c_getpeername (sockfd addrp addrlenp)
  (check-socket-error (#_getpeername sockfd addrp addrlenp)))

#-windows-target
(defun c_socketpair (domain type protocol socketsptr)
  (check-socket-error (#_socketpair domain type protocol socketsptr)))


(defun c_sendto (sockfd msgptr len flags addrp addrlen)
  (ignoring-eintr (check-socket-error (#_sendto sockfd msgptr len flags addrp addrlen))))

(defun c_recvfrom (sockfd bufptr len flags addrp addrlenp)
  (ignoring-eintr (check-socket-error (#_recvfrom sockfd bufptr len flags addrp addrlenp))))

(defun c_shutdown (sockfd how)
  (check-socket-error (#_shutdown sockfd how)))

(defun c_setsockopt (sockfd level optname optvalp optlen)
  (check-socket-error (#_setsockopt sockfd level optname optvalp optlen)))

(defun c_getsockopt (sockfd level optname optvalp optlenp)
  (check-socket-error (#_getsockopt sockfd level optname optvalp optlenp)))

#-windows-target
(defun c_sendmsg (sockfd msghdrp flags)
  (check-socket-error (#_sendmsg sockfd msghdrp flags)))

#-windows-target
(defun c_recvmsg (sockfd msghdrp flags)
  (check-socket-error   (#_recvmsg sockfd msghdrp flags)))

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

#-(or windows-target solaris-target)
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
                        :addr (ntohl (pref addr :sockaddr_in.sin_addr.s_addr))
                        :netmask (ntohl
                                  (pref (pref q :ifaddrs.ifa_netmask)
                                       :sockaddr_in.sin_addr.s_addr))
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
                                  :addr (ntohl address)
                                  :netmask (ntohl netmask)
                                  :flags if-flags
                                  :address-family address-family)
                                 res)))))))))
        (fd-close sock)))
    res))
)




#+windows-target
(defun %get-ip-interfaces ()
  (let* ((socket (#_socket #$AF_INET #$SOCK_DGRAM #$IPPROTO_IP)))
    (unwind-protect
         (rlet ((realoutlen #>DWORD 0))
           (do* ((reservedlen (* 4 (record-length #>INTERFACE_INFO))
                              (* 2 reservedlen)))
                ()
             (%stack-block ((buf reservedlen))
               (if (eql 0  (#_WSAIoctl
                            socket
                            #$SIO_GET_INTERFACE_LIST
                            (%null-ptr)
                            0
                            buf
                            reservedlen
                            realoutlen
                            (%null-ptr)
                            (%null-ptr)))
               (let* ((noutbytes (pref realoutlen #>DWORD)))
                 (when (< noutbytes reservedlen)
                   (let* ((interfaces nil))

                     (do* ((offset 0 (+ offset (record-length #>INTERFACE_INFO)))
                           (nameidx 0 (1+ nameidx)))
                          ((>= offset noutbytes))
                       (with-macptrs ((p (%inc-ptr buf offset)))
                         (push (make-ip-interface 
                                :name (format nil "ip~d" nameidx)
                                :addr (ntohl
                                       (pref (pref p #>INTERFACE_INFO.iiAddress)
                                             #>sockaddr_gen.AddressIn.sin_addr.S_un.S_addr))
                                :netmask (ntohl
                                          (pref (pref p #>INTERFACE_INFO.iiNetmask)
                                                #>sockaddr_gen.AddressIn.sin_addr.S_un.S_addr))
                                :flags (pref p #>INTERFACE_INFO.iiFlags)
                                :address-family #$AF_INET)
                               interfaces)))
                     (return interfaces))))
               (let* ((err (#_WSAGetLastError)))
                 (unless (eql err #$WSAEFAULT)
                   (return)))))))
      (#_closesocket socket))))

      


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
