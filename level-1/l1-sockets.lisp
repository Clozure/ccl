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

(defclass socket ()
  ())

(defclass ip-socket (socket)
  ())

(defclass file-socket (socket)
  ())

(defclass tcp-socket (ip-socket)
  ())

(defclass stream-file-socket (file-socket)
  ())

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

(defclass unconnected-socket (socket)
  ((device :initarg :device :accessor socket-device)
   (keys :initarg :keys :reader socket-keys)))

;; A passive tcp socket just generates connection streams
(defclass listener-socket (tcp-socket unconnected-socket) ())

(defclass file-listener-socket (stream-file-socket unconnected-socket) ())

;; A udp socket just sends and receives packets.
(defclass udp-socket (ip-socket unconnected-socket) ())

