;;; -*- Log: hemlock.log; Package: Hemlock-Internals -*-
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
;;; This file contatins the stream operations for pop-up-displays.
;;;
;;; Written by Blaine Burks.
;;;

(in-package :hemlock-internals)


(defmethod stream-write-char ((stream random-typeout-stream) char)
  (insert-character (random-typeout-stream-mark stream) char))

(defmethod stream-write-string ((stream random-typeout-stream) string &optional start end)
  (setf start (or start 0))
  (setf end (or end (length string)))
  (unless (and (eql start 0) (eql end (length string)))
    (setq string (subseq string start end)))
  (insert-string (random-typeout-stream-mark stream) string))

(defmethod stream-finish-output ((stream random-typeout-stream))
  nil)

(defmethod stream-force-output ((stream random-typeout-stream))
  (stream-finish-output stream))

(defmethod stream-line-column ((stream random-typeout-stream))
  (mark-charpos (random-typeout-stream-mark stream)))
