;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2001-2014 Clozure Associates
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
            REMOTE-SOCKET-ADDRESS
	    LOCAL-HOST
	    LOCAL-PORT
	    LOCAL-FILENAME
            LOCAL-SOCKET-ADDRESS
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
	    WITH-OPEN-SOCKET
            RESOLVE-ADDRESS
            SOCKET-ADDRESS-HOST
            SOCKET-ADDRESS-PORT
            SOCKET-ADDRESS-PATH))
  #+windows-target
  (defmacro check-winsock-error (form)
    (let* ((val (gensym)))
      `(let* ((,val ,form))
        (if (or (< ,val 0) (eql ,val #xffffffff))
          (%get-winsock-error)
          ,val))))
  (defmacro check-socket-error (form)
    #+windows-target `(check-winsock-error ,form)
    #-windows-target `(int-errno-call ,form))
  )

#+android-target
(eval-when (:compile-toplevel :execute)
  (def-foreign-type :sockaddr_storage (:struct :__kernel_sockaddr_storage)))


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
                "REMOTE-SOCKET-ADDRESS"
		"LOCAL-HOST"
		"LOCAL-PORT"
		"LOCAL-FILENAME"
                "LOCAL-SOCKET-ADDRESS"
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
		"WITH-OPEN-SOCKET"
                "RESOLVE-ADDRESS"
                "SOCKET-ADDRESS-HOST"
                "SOCKET-ADDRESS-PORT"
                "SOCKET-ADDRESS-PATH")
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
            "REMOTE-SOCKET-ADDRESS"
	    "LOCAL-HOST"
	    "LOCAL-PORT"
	    "LOCAL-FILENAME"
            "LOCAL-SOCKET-ADDRESS"
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
	    "WITH-OPEN-SOCKET"
            "RESOLVE-ADDRESS"
            "SOCKET-ADDRESS-HOST"
            "SOCKET-ADDRESS-PORT"
            "SOCKET-ADDRESS-PATH"))

(define-condition socket-error (simple-stream-error)
  ((code :initarg :code :reader socket-error-code)
   (identifier :initform :unknown :initarg :identifier :reader socket-error-identifier)
   (situation :initarg :situation :reader socket-error-situation)))

(define-condition socket-creation-error (simple-error)
  ((code :initarg :code :reader socket-creation-error-code)
   (identifier :initform :unknown :initarg :identifier :reader socket-creation-error-identifier)
   (situation :initarg :situation :reader socket-creation-error-situation)
   (remote-address :initform nil :initarg :remote-address :accessor socket-creation-error-remote-address)))

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
    (socket-error stream where res nil)
    res))

#-windows-target
(defun %gai-strerror (err)
  (let ((p (#_gai_strerror err)))
    (if (%null-ptr-p p)
      (format nil "Unknown nameserver error ~d" err)
      (%get-cstring p))))

(defun socket-error (stream where errno nameserver-p &key connect-address)
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
    (let* ((identifiers (if nameserver-p
                          *gai-error-identifiers*
                          *socket-error-identifiers*))
           (format-control (if nameserver-p
                             "~A (error #~D) during nameserver operation in ~A"
                             (if connect-address
                               "~A (error #~D) during attempt to connect to ~A"
                               "~A (error #~D) during socket creation operation in ~A")))
           (format-arguments (if connect-address
                               (list
                                #+windows-target
                                (%windows-error-string errno)
                                #-windows-target
                                (if nameserver-p
                                  (%gai-strerror errno)
                                  (%strerror errno))
                                errno
                                (socket-address-as-string connect-address))
                               (list
                                #+windows-target
                                (%windows-error-string errno)
                                #-windows-target
                                (if nameserver-p
                                  (%gai-strerror errno)
                                  (%strerror errno))
                                errno where))))
                              
      (error (make-condition 'socket-creation-error
			     :code errno
			     :identifier (getf identifiers errno :unknown)
			     :situation where
			     :format-control format-control
			     :format-arguments format-arguments
                             :remote-address connect-address)))))

;; If true, this will try to allow other cooperative processes to run
;; while socket io is happening.  Since CCL threads are preemptively
;; scheduled, this isn't particularly meaningful.
(defvar *multiprocessing-socket-io* nil)

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
  (:documentation "Return :internet, :internet6 or :file, as appropriate."))

(defmethod socket-address-family ((socket ip-socket))
  :internet)

(defmethod socket-address-family ((socket file-socket))
  :file)

(defgeneric socket-type (socket)
  (:documentation
   "Return :stream for tcp-stream and listener-socket, and :datagram
for udp-socket."))

(defmethod socket-type ((socket tcp-socket)) :stream)

(defmethod socket-type ((socket stream-file-socket)) :stream)

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

(defmethod socket-device ((stream file-socket-stream))
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

(defmethod socket-connect ((stream listener-socket)) :passive)

(defmethod socket-connect ((stream file-listener-socket)) :passive)

(defun %socket-connect (fd socket-address &optional timeout-in-milliseconds)
  (let ((err (c_connect fd (sockaddr socket-address) (sockaddr-length socket-address) timeout-in-milliseconds)))
    (declare (fixnum err))
    (unless (eql err 0)
      #||(fd-close fd)||#
      (socket-error nil "connect" err nil :connect-address socket-address))))

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

#-windows-target
(defmethod local-socket-filename ((socket file-socket))
  (let ((address (local-socket-address socket)))
    (when address
      (socket-address-path address))))

#-windows-target
(defmethod remote-socket-filename ((socket file-socket))
  (let ((address (remote-socket-address socket)))
    (when address
      (socket-address-path address))))

#-windows-target
(defmethod local-filename ((socket file-socket))
  (local-socket-filename socket))

#-windows-target
(defmethod remote-filename ((socket file-socket))
  (remote-socket-filename socket))
  
(defgeneric remote-host (socket)
  (:documentation
   "Return the 32-bit unsigned IP address of the remote host, or NIL if
the socket is not connected."))

;; Returns NIL if socket is not connected
(defmethod remote-host ((socket socket))
  (host (remote-socket-address socket)))

(defgeneric remote-port (socket)
  (:documentation
   "Return the remote port number, or NIL if the socket is not connected."))

(defmethod remote-port ((socket socket))
  (port (remote-socket-address socket)))

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

(defun set-socket-options (socket
                           &key 
                             keepalive
                             reuse-address
                             nodelay
                             broadcast
                             linger
                             (address-family :internet)
                             local-port
                             local-host
                             local-address
                             local-filename
                             type
                             connect
                             out-of-band-inline
			   &allow-other-keys)
  ;; see man socket(7) tcp(7) ip(7)
  (let ((fd (socket-device socket)))
    (when keepalive
      (int-setsockopt fd #$SOL_SOCKET #$SO_KEEPALIVE 1))
    (when reuse-address
      (int-setsockopt fd #$SOL_SOCKET #$SO_REUSEADDR 1))
    (when broadcast
      (int-setsockopt fd #$SOL_SOCKET #$SO_BROADCAST 1))
    (when out-of-band-inline
      (int-setsockopt fd #$SOL_SOCKET #$SO_OOBINLINE 1))
    (when (member address-family '(:internet :internet6))
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
      (when (or local-port local-host local-address)
        (socket-bind-local socket (or local-address
                                      (resolve-address :host local-host
                                                       :port local-port
                                                       :connect connect
                                                       :address-family address-family
                                                       :socket-type type)))))
    (when (and (eq address-family :file)
	       (eq connect :passive))
      (unless local-filename
        (error "need :local-filename argument to create passive file socket"))
      #+windows-target (error "can't create file socket on Windows")
      #-windows-target (socket-bind-local socket (make-instance 'unix-socket-address :path local-filename)))))

;; I hope the inline declaration makes the &rest/apply's go away...
(declaim (inline make-ip-socket))
(defun make-ip-socket (&rest keys &key (type :stream) &allow-other-keys)
  (declare (dynamic-extent keys))
  (ecase type
    (:stream (apply #'make-tcp-socket keys))
    (:datagram (apply #'make-udp-socket keys))))

#-windows-target
(declaim (inline make-file-socket))
#-windows-target
(defun make-file-socket (&rest keys &key type &allow-other-keys)
  (declare (dynamic-extent keys))
  (ecase type
    ((nil :stream) (apply #'make-stream-file-socket keys))
    (:datagram (apply #'make-datagram-file-socket keys))))

(defun make-socket (&rest keys
		    &key
                      (address-family :internet)
                      (type :stream)
                      (connect :active)
                      remote-host remote-port remote-address
                      eol format
                      keepalive reuse-address nodelay broadcast linger
                      local-port local-host local-address
                      backlog class out-of-band-inline
                      local-filename remote-filename sharing basic
                      external-format (auto-close t)
                      connect-timeout input-timeout output-timeout deadline
                      fd)
  "Create and return a new socket."
  (declare (dynamic-extent keys))
  (declare (ignore type connect remote-host remote-port remote-address
		   eol format keepalive reuse-address nodelay
		   broadcast linger local-port local-host
		   local-address backlog class out-of-band-inline
		   local-filename remote-filename sharing basic
		   external-format auto-close connect-timeout
		   input-timeout output-timeout deadline fd))
  (ecase address-family
    #-windows-target
    ((:file) (apply #'make-file-socket keys))
    ((:internet :internet6) (apply #'make-ip-socket keys))))

(defun make-udp-socket (&rest keys
                        &key
                          (fd -1)
                          (connect :active)
                          (address-family :internet)
                          remote-host remote-port
                        &allow-other-keys)
  (unwind-protect
    (let (socket)
      (when (< fd 0)
        (setq fd (socket-call nil "socket"
                              (c_socket (ecase address-family
                                          (:internet #$PF_INET)
                                          (:internet6 #$PF_INET6))
                                        #$SOCK_DGRAM #$IPPROTO_UDP))))
      (setq socket (make-instance 'udp-socket
				  :device fd
				  :keys keys))
      (apply #'set-socket-options socket keys)
      (when (and (eql connect :active)
                 remote-host remote-port)
        (%socket-connect fd
                         (apply #'resolve-address
				:connect connect
				:address-family address-family
                                :host remote-host
                                :port remote-port
                                :allow-other-keys t
                                keys)
                         nil))
      (setq fd -1)
      socket)
    (unless (< fd 0)
      (fd-close fd))))

(defun make-tcp-socket (&rest keys
                        &key
                          (connect :active)
                          (fd -1)
                          (address-family :internet)
                          remote-host remote-port remote-address
                          backlog connect-timeout deadline
                        &allow-other-keys)
  (unwind-protect
       (let ((timeout-in-milliseconds
               (cond
                 (deadline
                  (max (round (- deadline (get-internal-real-time))
                              (/ internal-time-units-per-second 1000))
                       0))
                 (connect-timeout
		  (check-io-timeout connect-timeout)
                  (round (* connect-timeout 1000)))))
             (socket-address (or remote-address
                                 (apply #'resolve-address
					:connect connect
					:address-family address-family
                                        :host remote-host
                                        :port remote-port
                                        :allow-other-keys t
                                        keys))))
         (when (< fd 0)
           (setq fd (socket-call nil "socket"
                                 (c_socket (ecase address-family
                                             (:internet #$PF_INET)
                                             (:internet6 #$PF_INET6))
                                           #$SOCK_STREAM #$IPPROTO_TCP))))
         (let ((socket (apply (ecase connect
                                (:active #'make-tcp-stream-socket)
                                (:passive #'make-tcp-listener-socket))
                              fd
                              keys)))
           (apply #'set-socket-options socket keys)
           (if (eql connect :passive)
               (socket-call nil "listen" (c_listen fd (or backlog 5)))
               (%socket-connect fd socket-address timeout-in-milliseconds))
           (setq fd -1)
           socket))
    (unless (< fd 0)
      (fd-close fd))))

;;; A FILE-LISTENER-SOCKET should try to delete the filesystem
;;; entity when closing.

#-windows-target
(defmethod close :before ((s file-listener-socket) &key abort)
  (declare (ignore abort))
  (let* ((path (local-socket-filename s)))
    (when path (%delete-file path))))

#-windows-target
(defun make-stream-file-socket (&rest keys
                                &key
                                  (connect :active)
                                  backlog
                                  (fd -1)
                                &allow-other-keys)
  (unwind-protect
       (let (socket)
         (when (< fd 0)
           (setq fd (socket-call nil "socket" (c_socket #$PF_LOCAL #$SOCK_STREAM 0))))
         (setq socket
                (apply (ecase connect
                         (:active #'make-file-stream-socket)
                         (:passive #'make-file-listener-socket))
                       fd keys))
         (apply #'set-socket-options socket keys)
         (when (eql connect :passive)
           (socket-call nil "listen" (c_listen fd (or backlog 5))))
         (setq fd -1)
         socket)
    (unless (< fd 0)
      (fd-close fd))))

(defun make-datagram-file-socket (&rest keys)
  (declare (ignore keys))
  (error "Datagram file sockets aren't implemented."))

#-windows-target
(defun file-socket-connect (fd remote-filename)
  (%socket-connect fd (make-instance 'unix-socket-address :path remote-filename)))

#+windows-target
(defun file-socket-connect (fd remote-filename)
  (declare (ignore fd))
  (error "Can't create file socket to ~s on Windows" remote-filename))
  
(defun make-tcp-stream-socket (fd &rest keys &key &allow-other-keys)
  (apply #'make-tcp-stream fd keys))

#-windows-target
(defun make-file-stream-socket (fd &rest keys
                                   &key remote-filename
                                   &allow-other-keys)
  (unless remote-filename
    (error "need :remote-filename argument when creating file stream socket"))
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

#-windows-target
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

(defun make-tcp-listener-socket (fd &rest keys &key &allow-other-keys)
  (make-instance 'listener-socket
                 :device fd
                 :keys keys))

(defun make-file-listener-socket (fd &rest keys &key &allow-other-keys)
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
                      (= res (- #$WSAEWOULDBLOCK))
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
	      (t (socket-error socket "accept" fd nil))))
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

#-windows-target
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
            #+arm-target (and (<= arm::min-8-bit-ivector-subtag subtype)
                                (<= subtype arm::max-8-bit-ivector-subtag))
      (report-bad-arg buf `(or (array (unsigned-byte 8))
			       (array (signed-byte 8))))))
  (values buf offset))

(defmethod send-to ((socket udp-socket) msg size
		    &key remote-host remote-port remote-address offset)
  "Send a UDP packet over a socket."
  (let ((socket-address (or remote-address
                            (remote-socket-address socket)
                            (resolve-address :host (or remote-host
                                                       (getf (socket-keys socket) :remote-host))
                                             :port (or remote-port
                                                       (getf (socket-keys socket) :remote-port))
                                             :connect :active
                                             :address-family (socket-address-family socket)
                                             :socket-type :datagram))))
    (multiple-value-setq (msg offset) (verify-socket-buffer msg offset size))
    (%stack-block ((bufptr size))
      (%copy-ivector-to-ptr msg offset bufptr 0 size)
      (socket-call socket "sendto"
        (with-eagain (socket-device socket) :output
          (c_sendto (socket-device socket)
                    bufptr size 0
                    (sockaddr socket-address)
                    (sockaddr-length socket-address)))))))

(defmethod receive-from ((socket udp-socket) size &key buffer extract offset want-socket-address-p)
  "Read a UDP packet from a socket. If no packets are available, wait for
a packet to arrive. Returns three or four values:

If want-socket-address-p is false:

  The buffer with the data
  The number of bytes read
  The 32-bit unsigned IPv4 address or the 16 byte IPv6 address of the sender of the data
  The port number of the sender of the data.

If want-socket-address-p is true:

  The buffer with the data
  The number of bytes read
  The socket-address describing the sender
"
  (let ((fd (socket-device socket))
	(vec-offset offset)
	(vec buffer)
	(ret-size -1)
        (socket-address (make-instance 'socket-address)))
    (when vec
      (multiple-value-setq (vec vec-offset)
	(verify-socket-buffer vec vec-offset size)))
    (rletz ((namelen :signed))
      (setf (pref namelen :signed) (sockaddr-length socket-address))
      (%stack-block ((bufptr size))
	(setq ret-size (socket-call socket "recvfrom"
			 (with-eagain fd :input
			   (c_recvfrom fd bufptr size 0 (sockaddr socket-address) namelen))))
        (upgrade-socket-address-from-sockaddr (pref (sockaddr socket-address) :sockaddr_storage.ss_family)
                                              socket-address)
	(unless vec
	  (setq vec (make-array ret-size
				:element-type
				(ecase (socket-format socket)
				  ((:binary) '(unsigned-byte 8))))
		vec-offset 0))
	(%copy-ptr-to-ivector bufptr 0 vec vec-offset ret-size))
      (let* ((buffer (cond ((null buffer)
                            vec)
                           ((or (not extract)
                                (and (eql 0 (or offset 0))
                                     (eql ret-size (length buffer))))
                            buffer)
                           (t 
                            (subseq vec vec-offset (+ vec-offset ret-size))))))
        (if want-socket-address-p
            (values buffer ret-size socket-address)
            (values buffer ret-size (host socket-address) (port socket-address)))))))

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

(defun dotted-to-ipaddr (name &key (errorp t))
  "Convert a dotted-string representation of a host address to a 32-bit
unsigned IP address."
  (let ((addr (_inet_aton name)))
    (if addr (ntohl addr)
      (and errorp (error "Invalid dotted address ~s" name)))))
    
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
	(socket-error socket "getsockopt" err nil)))))

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
    #+(or darwin-target freebsd-target)
    (setf (pref sin :sockaddr_in.sin_len) (record-length :sockaddr_in))
    (%stack-block ((namep #$NI_MAXHOST))
      (let* ((err (#_getnameinfo sin (record-length #>sockaddr_in) namep #$NI_MAXHOST (%null-ptr) 0 #$NI_NAMEREQD)))
        (if (eql 0 err)
          (%get-cstring namep)
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
      (when (zerop (#_WSAStringToAddressA name #$AF_INET (%null-ptr) addr addrlenp))
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
      
      
;;; If attempts to connect are interrupted, we basically have to
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
                    (if #+windows-target (windows-connect-wait sockfd timeout-in-milliseconds)
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
  #-android-target
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
  (let* ((sock (c_socket #$PF_INET #$SOCK_DGRAM #$IPPROTO_UDP))
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
  (socket-error stream where errno nil))

(defclass socket-address ()
  ((sockaddr :reader sockaddr :initform (make-gcable-record :sockaddr_storage))
   (sockaddr-length :reader sockaddr-length :initform (record-length :sockaddr_storage))))

(defmethod initialize-instance :after ((socket-address socket-address) &key)
  (loop for i below (sockaddr-length socket-address)
        do (setf (paref (sockaddr socket-address) :uint8_t i) 0)))

(defgeneric upgrade-socket-address-from-sockaddr (socket-address-family socket-address)
  (:documentation "Upgrade the class of the given socket-address
  instance to be the class of the socket-address subclass that
  corresponds to the given socket-address-family, which is expected to
  be an address family as encoded by the sockets API (i.e. #$AF_UNIX,
  #$AF_INET or #$AF_INET6).  This generic function is eql-specialized
  for all supported address families.  It is called after a system
  function like getaddrinfo() or getpeername() has been called to
  determine a sockaddr to turn it into a lisp object that can be
  conveniently manipulated."))

(defgeneric socket-address-as-string (socket-address)
  (:documentation "Return printable representation of the
  IP-SOCKET-ADDRESS"))

(defmethod socket-address-as-string ((socket-address socket-address))
  (format nil "[unparsed, AF ~A]" (pref (sockaddr socket-address) :sockaddr_storage.ss_family)))

(defmethod print-object ((socket-address socket-address) stream)
  (print-unreadable-object (socket-address stream :type t)
    (write-string (socket-address-as-string socket-address) stream)))

(defgeneric af (socket-address)
  (:documentation "Return the socket address family associated with
  the socket address object"))

(defun in6-addr-to-vec (in6-addr)
  (loop with result = (make-array 16 :element-type '(unsigned-byte 8))
        for i below 16
        do (setf (aref result i) (paref in6-addr :uint8_t i))
        finally (return result)))

(defun vec-to-in6-addr (vec in6-addr)
  (loop for i below 16
        do (setf (paref in6-addr :uint8_t i) (aref vec i))))

(defclass ip-socket-address (socket-address)
  ((host :initarg :host :reader host :reader socket-address-host)
   (port :initarg :port :reader port :reader socket-address-port)))

(defconstant +host-address-string-len+
  #-windows-target #$INET6_ADDRSTRLEN
  #+windows-target 64)

(defun host-address-as-string (ip-socket-address)
  (%stack-block ((namep +host-address-string-len+)
		 #+windows-target
		 (namelenp (record-length :int)))
    #-windows-target
    (multiple-value-bind (result errno)
        ;; Needs to be ccl:external-call because FreeBSD, for unclear
        ;; reasons, does not have #_inet_ntop, but rather
        ;; #___inet_ntop
        (ccl:external-call "inet_ntop"
                           :int (af ip-socket-address)
                           :address (ecase (af ip-socket-address)
                                      (#.#$AF_INET (pref (sockaddr ip-socket-address) :sockaddr_in.sin_addr))
                                      (#.#$AF_INET6 (pref (sockaddr ip-socket-address) :sockaddr_in6.sin6_addr)))
                           :address namep
                           :socklen_t +host-address-string-len+
                           :address)
      (if (%null-ptr-p result)
	  (error "could not convert address to string, error ~S" errno)
	  (%get-cstring namep)))
    #+windows-target
    (setf (pref namelenp :int) +host-address-string-len+)
    #+windows-target
    (if (zerop (#_WSAAddressToStringA (sockaddr ip-socket-address)
                                      (sockaddr-length ip-socket-address)
                                      (%null-ptr) namep namelenp))
        (%str-from-ptr namep (pref namelenp :int))
        (error "cannot convert host address to string, error ~A" (%get-winsock-error)))))

(defun ensure-string (port)
  (if (stringp port)
      port
      (princ-to-string port)))

(defun resolve-address (&key
                        host
                        port
                        (socket-type :stream)
                        (connect :active)
                        address-family
                        numeric-host-p
                        #-windows-target numeric-service-p
                        (singlep t)
                        (errorp t))
  "Resolve a host and/or port string to one or more socket-address
  instances.  Either host or port may be unspecified.  Calls
  getaddrinfo() underneath.
  
  singlep may be passed as NIL to make the function return a list of
  host addresses matching the specified query terms.  The default is to
  return the first matching address.
  
  errorp may be passed as NIL to return NIL if no match was found."
  
  ;; We have historically supported the use of an (unsigned-byte 32)
  ;; value to represent an IPv4 address. If existing code does that to
  ;; avoid overhead (name resolution, consing, what-have-you), then
  ;; that code may not appreciate the consing/mallocing we do here
  ;; to support that.
  (when (typep host '(unsigned-byte 32))
    (let* ((proto (ecase socket-type
		    ((nil :stream) "tcp")
		    (:datagram "udp")))
	   (inet-port (typecase port
			(fixnum (htons port))
			(string (_getservbyname port proto))
			(symbol (_getservbyname (string-downcase
						 (string port)) proto)))))
      (if (null inet-port)
          (when errorp
            (error "can't resolve port ~s with getservbyname" port))
          (let* ((socket-address (make-instance 'socket-address))
                 (sin (sockaddr socket-address)))
            (setf (pref sin :sockaddr_in.sin_family) #$AF_INET)
            (setf (pref sin
                        #+(or windows-target solaris-target) #>sockaddr_in.sin_addr.S_un.S_addr
                        #-(or windows-target solaris-target) :sockaddr_in.sin_addr.s_addr) (htonl host))
            (setf (pref sin :sockaddr_in.sin_port) inet-port)
            (upgrade-socket-address-from-sockaddr #$AF_INET socket-address)
            (return-from resolve-address socket-address)))))
  
  (with-cstrs ((host-buf (or host ""))
               (port-buf (string-downcase (or (ensure-string port) ""))))
    (rletZ ((hints #>addrinfo)
            (results :address))
      (setf (pref hints #>addrinfo.ai_socktype) (ecase socket-type
                                                  ((nil :stream) #$SOCK_STREAM)
                                                  (:datagram #$SOCK_DGRAM)))
      (when address-family
        (setf (pref hints #>addrinfo.ai_family) (ecase address-family
                                                  (:internet #$AF_INET)
                                                  (:internet6 #$AF_INET6))))
      (let ((flags 0))
        (when numeric-host-p
          (incf flags #$AI_NUMERICHOST))
	#-windows-target
        (when numeric-service-p
          (incf flags #$AI_NUMERICSERV))
        (when (eql connect :passive)
          (incf flags #$AI_PASSIVE))
        (setf (pref hints #>addrinfo.ai_flags) flags))
      (let* ((err (#_getaddrinfo (if host host-buf (%null-ptr))
                                 (if port port-buf (%null-ptr))
                                 hints
                                 results)))
        (if (eql 0 err)
            (prog1
              (or (loop for info = (pref results :address) then (pref info #>addrinfo.ai_next)
                    until (%null-ptr-p info)
                    for sockaddr = (pref info #>addrinfo.ai_addr)
                    for socket-address = (make-instance 'socket-address)
                    do (loop for i below (pref info #>addrinfo.ai_addrlen)
                         do (setf (paref (sockaddr socket-address) :uint8_t i)
                                  (paref sockaddr :uint8_t i)))
                    (upgrade-socket-address-from-sockaddr (pref (sockaddr socket-address) :sockaddr_storage.ss_family)
                                                          socket-address)
                    if singlep
                    do (return socket-address)
                    else
                    collect socket-address)
                  (when errorp
                    (error "cannot resolve local service host ~A port ~A connect ~S type ~S"
                           host port connect socket-type)))
              (#_freeaddrinfo (pref results :address)))
            (if errorp 
                (socket-error nil "getaddrinfo" err t) 
                (values nil err)))))))

(defclass ip4-socket-address (ip-socket-address)
  ())

(defmethod socket-address-family ((socket-address ip4-socket-address))
  :internet)

(defmethod socket-address-as-string ((socket-address ip4-socket-address))
  (format nil "~A:~A" (host-address-as-string socket-address) (port socket-address)))

(defmethod af ((socket-address ip4-socket-address))
  #$AF_INET)

(defmethod upgrade-socket-address-from-sockaddr ((address-family (eql #$AF_INET)) socket-address)
  (setf (slot-value socket-address 'sockaddr-length) (record-length #:sockaddr_in))
  (change-class socket-address 'ip4-socket-address
                :host (ntohl (pref (sockaddr socket-address)
                                   #-(or solaris-target windows-target) :sockaddr_in.sin_addr.s_addr
                                   #+(or solaris-target windows-target) #>sockaddr_in.sin_addr.S_un.S_addr))
                :port (ntohs (pref (sockaddr socket-address) :sockaddr_in.sin_port))))

(defclass ip6-socket-address (ip-socket-address)
  ())

(defmethod socket-address-family ((socket-address ip6-socket-address))
  :internet6)

(defmethod socket-address-as-string ((socket-address ip6-socket-address))
  (format nil "[~A]:~A" (host-address-as-string socket-address) (port socket-address)))

(defmethod af ((socket-address ip6-socket-address))
  #$AF_INET6)

(defmethod upgrade-socket-address-from-sockaddr ((address-family (eql #$AF_INET6)) socket-address)
  (setf (slot-value socket-address 'sockaddr-length) (record-length #:sockaddr_in6))
  (change-class socket-address 'ip6-socket-address
                :host (in6-addr-to-vec (pref (sockaddr socket-address) :sockaddr_in6.sin6_addr))
                :port (ntohs (pref (sockaddr socket-address) :sockaddr_in6.sin6_port))))

#-windows-target
(defclass unix-socket-address (socket-address)
  ((path :initarg :path :reader host :reader socket-address-path)))

#-windows-target
(defmethod initialize-instance :after ((socket-address unix-socket-address) &key path)
  (setf (pref (sockaddr socket-address) :sockaddr_un.sun_family) #$AF_UNIX
        (slot-value socket-address 'sockaddr-length) (copy-string-to-sockaddr_un path
                                                                                 (sockaddr socket-address)))
  #+(or darwin-target freebsd-target)
  (setf (pref (sockaddr socket-address) :sockaddr_un.sun_len) (sockaddr-length socket-address)))


#-windows-target
(defmethod socket-address-family ((socket-address unix-socket-address))
  :file)

#-windows-target
(defmethod af ((socket-address unix-socket-address))
  #$AF_UNIX)

#-windows-target
(defmethod port ((socket-address unix-socket-address))
  nil)

#-windows-target
(defmethod socket-address-as-string ((socket-address unix-socket-address))
  (format nil "~S" (socket-address-path socket-address)))

#-windows-target
(defconstant +socketaddr_un-sock-path-lan+ (/ (ensure-foreign-type-bits
                                               (foreign-record-field-type 
                                                (%find-foreign-record-type-field
                                                 (parse-foreign-type '(:struct :sockaddr_un)) :sun_path)))
                                              8))

#-windows-target
(defun copy-string-to-sockaddr_un (name sockaddr)
  "Copy a pathname to a sockaddr_un object, returning the length of
the resulting sockaddr."
  (let* ((namelen (length name))
         (copylen (min (1- +socketaddr_un-sock-path-lan+) namelen))
         (sun-path (pref sockaddr :sockaddr_un.sun_path)))
    (dotimes (i copylen)
      (setf (%get-unsigned-byte sun-path i)
            (let* ((code (char-code (schar name i))))
              (if (> code 255)
                  (char-code #\Sub)
                  code))))
    (setf (%get-unsigned-byte sun-path copylen) 0)
    (+ (record-length :sockaddr_un) (- +socketaddr_un-sock-path-lan+) 1 copylen)))

#-windows-target
(defmethod upgrade-socket-address-from-sockaddr ((address-family (eql #$AF_UNIX)) socket-address)
  (let ((sockaddr (sockaddr socket-address)))
    (change-class socket-address 'unix-socket-address
                  :path (when (and #+(or darwin-target freebsd-target) (plusp (pref sockaddr :sockaddr_un.sun_len))
                                   (not (zerop (paref (pref sockaddr :sockaddr_un.sun_path) :uint8_t 0))))
                          #+darwin-target
                          (%str-from-ptr (pref sockaddr :sockaddr_un.sun_path)
                                         (- (pref sockaddr :sockaddr_un.sun_len)
                                            (- (record-length :sockaddr_un) +socketaddr_un-sock-path-lan+)
                                            1))
                          #-darwin-target
                          (%get-cstring (pref sockaddr :sockaddr_un.sun_path))))))

(defmethod socket-bind-local (socket socket-address)
  (socket-call socket "bind" (c_bind (socket-device socket)
                                     (sockaddr socket-address)
                                     (sockaddr-length socket-address))))

(defun get-socket-address-from-call (socket function call-name)
  (let ((socket-address (make-instance 'socket-address)))
    (rlet ((namelen :signed (sockaddr-length socket-address)))
      (let ((err (funcall function (socket-device socket) (sockaddr socket-address) namelen)))
        (cond
          ((eql err (- #+windows-target #$WSAENOTCONN #-windows-target #$ENOTCONN))
           nil)
          ((< err 0)
           (socket-error socket call-name err nil))
          (t
           (upgrade-socket-address-from-sockaddr (pref (sockaddr socket-address) :sockaddr_storage.ss_family)
                                                 socket-address)))))))

(defmethod remote-socket-address ((socket socket))
  (when (socket-device socket)
    (get-socket-address-from-call socket #'c_getpeername "getpeername")))

(defmethod local-socket-address ((socket socket))
  (when (socket-device socket)
    (get-socket-address-from-call socket #'c_getsockname "getsockname")))

(defgeneric local-port (socket)
  (:documentation "Return the local port number."))

(defmethod local-port ((socket socket))
  (port (local-socket-address socket)))

(defgeneric local-host (socket)
  (:documentation
   "Return internal representation of the local IP address of the
   socket.  For IPv4 addresses, this is a 32 bit integer.  For IPv6
   addresses, it is a vector of 16 bytes."))

(defmethod local-host ((socket socket))
  (host (local-socket-address socket)))

(defun lookup-port (port proto)
  "Find the port number for the specified port and protocol."
  (if (fixnump port)
      port
      (let ((socket-address (resolve-address :port port
                                             :socket-type (if (stringp proto)
                                                              (case (find-symbol (string-upcase proto) :keyword)
                                                                (:udp :datagram)
                                                                (:tcp :stream))
                                                              proto)
                                             :errorp nil)))
        (when socket-address
          (port socket-address)))))

(defun lookup-hostname (host)
  "Convert a host spec in any of the acceptable formats into a 32-bit
unsigned IP address."
  (if (typep host 'integer)
      host
      (let ((socket-address (resolve-address :host host :address-family :internet :errorp nil)))
        (when socket-address
          (host socket-address)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun snarf-url (url &key max-redirects (user-agent "CCL") &aux conn)
  "GET the contents of the url as a (VECTOR (UNSIGNED-BYTE 8))"
  (labels ((is-prefix (prefix string) (eql (length prefix) (string-lessp prefix string)))
	   (header (prefix lines)
	     (let ((line (find prefix lines :test #'is-prefix)))
	       (and line (string-trim ccl::wsp (subseq line (length prefix))))))
	   (header-value (prefix lines)
	     (let ((line (find prefix lines :test #'is-prefix)))
	       (and line (parse-integer line :start (length prefix)))))
	   (split-url (string)
             (if (is-prefix "/" string)
               (list nil 80 string)
               (if (not (is-prefix "http://" string))
                 (error "Unknown scheme in ~s" string)
                 (let* ((start (length "http://"))
                        (end (length string))
                        (ppos (or (position #\/ string :start start) end))
                        (hend (or (position #\: string :start start :end ppos) ppos)))
                   (list (subseq string start hend)
                         (if (< hend ppos) (parse-integer string :start (1+ hend) :end ppos) 80)
                         (if (< ppos end) (subseq string ppos) "/"))))))
	   (read-header (conn)
	     (loop as lines = (loop for line = (read-line conn nil)
				    until (= 0 (length line)) ; eof or empty line
				    collect line)
		   as status = (let ((status-line (pop lines)))
				 (or (parse-integer status-line
						    :start (position #\Space status-line)
						    :junk-allowed t)
				     0))
		   while (= status 100)
		   finally (return (values lines status)))))
    (unwind-protect
       (loop with original-url = url
	     with redirects = (or max-redirects 20)
	     with (host port path) = (split-url original-url)
	     do (setq conn (make-socket :remote-host host
					:remote-port port
					:external-format '(:character-encoding :us-ascii
							   :line-termination :crlf)))
	     do (format conn "GET ~a HTTP/1.1~%Host: ~a:~d~%Connection: close~%User-Agent: ~a~2%"
			path host port user-agent)
	     do (finish-output conn)
	     do (multiple-value-bind (header-lines status) (read-header conn)
		  (when (= status 200)
		    (let ((encoding (header "transfer-encoding:" header-lines)))
		      ;; Here would recognize chunked encoding if cared about that...
		      (when (and encoding (not (string-equal encoding "identity")))
			(error "Unsupported encoding ~s" encoding)))
		    (return
		      (let* ((count (header-value "content-length:" header-lines)))
			(if count
			    (let ((vec (make-array count :element-type '(unsigned-byte 8))))
			      (loop for i from 0 below count
				    do (setf (aref vec i) (read-byte conn)))
			      vec)
			    (let ((vec (make-array 1000
						   :element-type '(unsigned-byte 8)
						   :fill-pointer 0
						   :adjustable t)))
			      (loop for byte = (read-byte conn nil) while byte
				    do (vector-push-extend byte vec))
			      (subseq vec 0 (length vec)))))))
		  (unless (and (<= 300 status 399) (<= 0 (decf redirects)))
		    (if (<= 300 status 399)
			(error "Too many redirects")
			(error "Unknown response ~s" status)))
		  (let* ((new (or (header "location:" header-lines)
				  (error "Missing Location: header"))))
		    (destructuring-bind (new-host new-port new-path) (split-url new)
		      (when new-host
			(setq host new-host port new-port))
		      (setq path new-path))
		    (close conn)
		    (setq conn nil)))
      (when conn (close conn))))))
