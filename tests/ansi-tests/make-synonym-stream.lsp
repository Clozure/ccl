;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jan 28 06:54:33 2004
;;;; Contains: Tests of MAKE-SYNONYM-STREAM

(in-package :cl-test)

(deftest make-synonym-stream.1
  (with-input-from-string
   (*s* "abcde")
   (declare (special *s*))
   (let ((ss (make-synonym-stream '*s*)))
     (assert (typep ss 'stream))
     (assert (typep ss 'synonym-stream))
     (assert (input-stream-p ss))
     (assert (not (output-stream-p ss)))
     (assert (open-stream-p ss))
     (assert (streamp ss))
     (assert (stream-element-type ss))
     (values
      (read-char *s*)
      (read-char ss)
      (read-char *s*)
      (read-char ss)
      (read-char ss))))
  #\a #\b #\c #\d #\e)


;;; This test was wrong (section 21.1.4)
#|
(deftest make-synonym-stream.2
   (let ((ss (make-synonym-stream '*s*)))
     (with-input-from-string
      (*s* "z")
      (declare (special *s*))
      (assert (typep ss 'stream))
      (assert (typep ss 'synonym-stream))
      (assert (input-stream-p ss))
      (assert (not (output-stream-p ss)))
      (assert (open-stream-p ss))
      (assert (streamp ss))
      (assert (stream-element-type ss))
      (read-char ss)))
   #\z)
|#

(deftest make-synonym-stream.3
  (with-output-to-string
   (*s*)
   (declare (special *s*))
   (let ((ss (make-synonym-stream '*s*)))
     (assert (typep ss 'stream))
     (assert (typep ss 'synonym-stream))
     (assert (output-stream-p ss))
     (assert (not (input-stream-p ss)))
     (assert (open-stream-p ss))
     (assert (streamp ss))
     (assert (stream-element-type ss))
     (write-char #\a *s*)
     (write-char #\b ss)
     (write-char #\x *s*)
     (write-char #\y ss)))
  "abxy")

(deftest make-synonym-stream.4
  (let ((ss (make-synonym-stream '*terminal-io*)))
     (assert (typep ss 'stream))
     (assert (typep ss 'synonym-stream))
     (assert (output-stream-p ss))
     (assert (input-stream-p ss))
     (assert (open-stream-p ss))
     (assert (streamp ss))
     (assert (stream-element-type ss))
     nil)
  nil)


;;; FIXME
;;; Add tests for: close,
;;;  peek-char, read-char-no-hang, terpri, fresh-line, unread-char,
;;;  read-line, write-line, write-string, read-sequence, write-sequence,
;;;  read-byte, write-byte, listen, clear-input, finish-output, force-output,
;;;  clear-output, format, print, prin1, princ

;;; Error cases

(deftest make-synonym-stream.error.1
  (signals-error (make-synonym-stream) program-error)
  t)

(deftest make-synonym-stream.error.2
  (signals-error (make-synonym-stream '*standard-input* nil) program-error)
  t)

(deftest make-synonym-stream.error.3
  (check-type-error #'make-synonym-stream #'symbolp)
  nil)
