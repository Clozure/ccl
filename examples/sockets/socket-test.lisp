;; -*- Lisp -*-

(defpackage :socket-test
  (:use :cl :ccl))

(in-package :socket-test)

(defconstant +receive-timeout+ 0.2)

(defun set-receive-timeout (socket)
  (ccl::timeval-setsockopt (ccl::socket-device socket) #$SOL_SOCKET #$SO_RCVTIMEO +receive-timeout+))

(defun say (format &rest args)
  (format t "~&; ~?~%" format args)
  (finish-output))

(defun make-stream-server (address-family &rest args &key &allow-other-keys)
  (let ((server-socket (apply #'make-socket :address-family address-family :connect :passive args)))
    (process-run-function (format nil "~A stream server ~{~A~^ ~}" address-family args)
                          (lambda ()
                            (say "waiting for connection on ~A" server-socket)
                            (let ((client-socket (accept-connection server-socket)))
                              (say "got connection from ~A to ~A, client socket is ~A"
                                   (remote-socket-address client-socket)
                                   (local-socket-address client-socket)
                                   client-socket)
                              (unwind-protect
                                   (handler-case
                                       (loop for command = (read-line client-socket)
                                             do (say "got command ~S" command)
                                             while (not (string= command "quit"))
                                             do (format client-socket "yes ~A~%" command)
                                                (finish-output client-socket))
                                     (error (e)
                                       (say "error serving client: ~A" e)))
                                (say "server exiting")
                                (close client-socket)
                                (close server-socket)))))))

(defun run-stream-test (address-family &key host port path)
  (multiple-value-bind (server-args client-args)
      (ecase address-family
        ((:internet :internet6)
         (assert (and host port) ()
                 "need :host and :port argument for internet socket test")
         (values (list :local-host host :local-port port :reuse-address t)
                 (list :remote-host host :remote-port port)))
        (:file
         (assert path ()
                 "nett :path argument for file socket test")
         (values (list :local-filename path)
                 (list :remote-filename path))))
    (let* ((server-process (apply #'make-stream-server address-family server-args))
           (client-socket (apply #'make-socket :address-family address-family client-args)))
      (set-receive-timeout client-socket)
      (unwind-protect
           (flet ((send-server (command &rest args)
                    (say "sending ~S to server" command)
                    (format client-socket command args)
                    (terpri client-socket)
                    (force-output client-socket)))
             (send-server "hello")
             (say "got reply ~S" (read-line client-socket))
             (send-server "quit")
             (say "sent quit")
             (join-process server-process))
        (ignore-errors
         (process-kill server-process))))))

(defun receive-message (socket)
  (multiple-value-bind (buffer len socket-address)
      (receive-from socket 100 :want-socket-address-p t)
    (values (decode-string-from-octets buffer :end len) socket-address)))

(defun send-message (socket string socket-address)
  (let ((reply (encode-string-to-octets string)))
    (send-to socket reply (length reply) :remote-address socket-address)))

(defun run-datagram-test (address-family &rest args &key host port)
  (with-open-socket (server-socket :address-family address-family :type :datagram :local-host host :local-port port)
    (with-open-socket (client-socket :address-family address-family :type :datagram)
      (let ((server-process (process-run-function (format nil "~A datagram ~{~A~^ ~}" address-family args)
                                                  (lambda ()
                                                    (loop
                                                      (multiple-value-bind (message client-address) (receive-message server-socket)
                                                        (say "received ~S from ~A" message client-address)
                                                        (when (string-equal message "quit")
                                                          (return))
                                                        (send-message server-socket (format nil "yes ~A" message) client-address))))))
            (server-address (apply #'resolve-address :address-family address-family args)))
        (set-receive-timeout client-socket)
        (send-message client-socket "hello" server-address)
        (say "server says ~S" (receive-message client-socket))
        (send-message client-socket "quit" server-address)
        (join-process server-process)))))

(defun run-trivial-test (&key (port 3994))
  (run-stream-test :internet :host "127.0.0.1" :port port)
  (run-stream-test :internet6 :host "::1" :port port)
  #-windows-target
  (run-stream-test :file :path "/tmp/ccl-socktest")
  (run-datagram-test :internet :host "127.0.0.1" :port port)
  (run-datagram-test :internet6 :host "::1" :port port))
