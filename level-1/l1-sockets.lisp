;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 2001-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

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

(defclass device-mixin ()
  ((device :initarg :device :accessor socket-device)
   (keys :initarg :keys :reader socket-keys)))

;; A passive tcp socket just generates connection streams
(defclass listener-socket (tcp-socket device-mixin) ())

(defclass file-listener-socket (stream-file-socket device-mixin) ())

;; A udp socket just sends and receives packets.
(defclass udp-socket (ip-socket device-mixin)
  ((connected :initarg :connected :initform nil :accessor socket-connected
              :documentation "True if the socket is connected in the UDP sense,
              i.e. it has had #_connect successfully called on it.")))


