;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Jan 29 22:06:28 2004
;;;; Contains: Tests of BROADCAST-STREAM-STREAMS

(in-package :cl-test)

(deftest broadcast-stream-streams.1
  (broadcast-stream-streams (make-broadcast-stream))
  nil)

(deftest broadcast-stream-streams.2
  (equalt
   (broadcast-stream-streams (make-broadcast-stream *standard-output*))
   (list *standard-output*))
  t)

(deftest broadcast-stream-streams.error.1
  (signals-error (broadcast-stream-streams) program-error)
  t)

(deftest broadcast-stream-streams.error.2
  (signals-error (broadcast-stream-streams (make-broadcast-stream) nil)
		 program-error)
  t)





