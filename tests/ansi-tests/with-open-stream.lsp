;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Dec 13 01:42:59 2004
;;;; Contains: Tests of WITH-OPEN-STREAM

(in-package :cl-test)

(deftest with-open-stream.1
  (with-open-stream (os (make-string-output-stream)))
  nil)

(deftest with-open-stream.2
  (with-open-stream (os (make-string-output-stream))
		    (declare (ignore os)))
  nil)

(deftest with-open-stream.3
  (with-open-stream (os (make-string-output-stream))
		    (declare (ignore os))
		    (declare (type string-stream os)))
  nil)

(deftest with-open-stream.4
  (with-open-stream (os (make-string-output-stream))
		    (declare (ignore os))
		    (values)))

(deftest with-open-stream.5
  (with-open-stream (os (make-string-output-stream))
		    (declare (ignore os))
		    (values 'a 'b))
  a b)

(deftest with-open-stream.6
  (let ((s (make-string-output-stream)))
    (values
     (with-open-stream (os s))
     (notnot (typep s 'string-stream))
     (open-stream-p s)))
  nil t nil)

(deftest with-open-stream.7
  (let ((s (make-string-input-stream "123")))
    (values
     (with-open-stream (is s) (read-char s))
     (notnot (typep s 'string-stream))
     (open-stream-p s)))
  #\1 t nil)

(deftest with-open-stream.8
  (let ((s (make-string-output-stream)))
    (values
     (block done
      (with-open-stream (os s) (return-from done nil)))
     (notnot (typep s 'string-stream))
     (open-stream-p s)))
  nil t nil)

(deftest with-open-stream.9
  (let ((s (make-string-output-stream)))
    (values
     (catch 'done
      (with-open-stream (os s) (throw 'done nil)))
     (notnot (typep s 'string-stream))
     (open-stream-p s)))
  nil t nil)

;;; Free declaration scope

(deftest with-open-stream.10
  (block done
    (let ((x :bad))
      (declare (special x))
      (let ((x :good))
	(with-open-stream (s (return-from done x))
			  (declare (special x))))))
  :good)
