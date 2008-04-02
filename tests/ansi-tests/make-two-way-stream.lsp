;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Jan 30 05:39:56 2004
;;;; Contains: Tests for MAKE-TWO-WAY-STREAM

(in-package :cl-test)

(deftest make-two-way-stream.1
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-two-way-stream is os)))
    (assert (typep s 'stream))
    (assert (typep s 'two-way-stream))
    (assert (streamp s))
    (assert (open-stream-p s))
    (assert (input-stream-p s))
    (assert (output-stream-p s))
    (assert (stream-element-type s))
    (values
     (read-char s)
     (write-char #\b s)
     (read-char s)
     (write-char #\a s)
     (read-char s)
     (write-char #\r s)
     (get-output-stream-string os)))
  #\f #\b #\o #\a #\o #\r "bar")

(deftest make-two-way-stream.2
   (let* ((is (make-string-input-stream "foo"))
	  (os (make-string-output-stream))
	  (s (make-two-way-stream is os)))
     (values
      (close s)
      (open-stream-p s)
      (notnot (open-stream-p is))
      (notnot (open-stream-p os))
      (write-char #\8 os)
      (get-output-stream-string os)))
   t nil t t #\8 "8")

(deftest make-two-way-stream.3
   (let* ((is (make-string-input-stream "foo"))
	  (os (make-string-output-stream))
	  (s (make-two-way-stream is os)))
     (values
      (peek-char nil s)
      (read-char s)
      (get-output-stream-string os)))
   #\f #\f "")

(deftest make-two-way-stream.4
   (let* ((is (make-string-input-stream "foo"))
	  (os (make-string-output-stream))
	  (s (make-two-way-stream is os)))
     (values
      (read-char-no-hang s)
      (read-char-no-hang s nil)
      (read-char-no-hang s t :eof)
      (read-char-no-hang s nil :eof)
      (get-output-stream-string os)))
   #\f #\o #\o :eof "")

(deftest make-two-way-stream.5
   (let* ((is (make-string-input-stream "foo"))
	  (os (make-string-output-stream))
	  (s (make-two-way-stream is os)))
     (values
      (terpri s)
      (get-output-stream-string os)))
   nil #.(string #\Newline))

(deftest make-two-way-stream.6
   (let* ((is (make-string-input-stream "foo"))
	  (os (make-string-output-stream))
	  (s (make-two-way-stream is os)))
     (values
      (write-char #\+ s)
      (notnot (fresh-line s))
      (read-char s)
      (get-output-stream-string os)))
   #\+ t #\f #.(coerce (list #\+ #\Newline) 'string))

(deftest make-two-way-stream.7
   (let* ((is (make-string-input-stream "foo"))
	  (os (make-string-output-stream))
	  (s (make-two-way-stream is os)))
     (values
      (read-char s)
      (unread-char #\f s)
      (read-char s)
      (read-char s)
      (unread-char #\o s)
      (get-output-stream-string os)))
   #\f nil #\f #\o nil "")

(deftest make-two-way-stream.8
   (let* ((is (make-string-input-stream "foo"))
	  (os (make-string-output-stream))
	  (s (make-two-way-stream is os)))
     (values
      (read-line s)
      (get-output-stream-string os)))
   "foo" "")

(deftest make-two-way-stream.9
   (let* ((is (make-string-input-stream "foo"))
	  (os (make-string-output-stream))
	  (s (make-two-way-stream is os)))
     (values
      (write-string "bar" s)
      (get-output-stream-string os)))
   "bar" "bar")

(deftest make-two-way-stream.10
   (let* ((is (make-string-input-stream "foo"))
	  (os (make-string-output-stream))
	  (s (make-two-way-stream is os)))
     (values
      (write-line "bar" s)
      (get-output-stream-string os)))
   "bar" #.(concatenate 'string "bar" '(#\Newline)))

(deftest make-two-way-stream.11
  (let* ((is (make-string-input-stream "foo"))
	  (os (make-string-output-stream))
	  (s (make-two-way-stream is os)))
    (let ((x (vector nil nil nil)))
     (values
      (read-sequence x s)
      x
      (get-output-stream-string os))))
  3 #(#\f #\o #\o) "")

(deftest make-two-way-stream.12
  (let ((pn1 #p"tmp.dat")
	(pn2 #p"tmp2.dat")
	(element-type '(unsigned-byte 8)))
    (with-open-file (s pn1 :direction :output :if-exists :supersede
		       :element-type element-type)
		    (dolist (b '(3 8 19 41)) (write-byte b s)))
    (with-open-file
     (is pn1 :direction :input :element-type element-type)
     (with-open-file
      (os pn2 :direction :output :element-type element-type
	  :if-exists :supersede)
      (let ((s (make-two-way-stream is os))
	    (x (vector nil nil nil nil)))
	(assert (eql (read-sequence x s) 4))
	(assert (equalp x #(3 8 19 41)))
	(let ((y #(100 5 18 211 0 178)))
	  (assert (eql (write-sequence y s) y))
	  (close s)))))
    (with-open-file
     (s pn2 :direction :input :element-type element-type)
     (let ((x (vector nil nil nil nil nil nil nil)))
       (values
	(read-sequence x s)
	x))))
  6
  #(100 5 18 211 0 178 nil))

(deftest make-two-way-stream.13
  (let ((pn1 #p"tmp.dat")
	(pn2 #p"tmp2.dat")
	(element-type '(unsigned-byte 32)))
    (with-open-file (s pn1 :direction :output :if-exists :supersede
		       :element-type element-type)
		    (dolist (b '(3 8 19 41)) (write-byte b s)))
    (with-open-file
     (is pn1 :direction :input :element-type element-type)
     (with-open-file
      (os pn2 :direction :output :element-type element-type
	  :if-exists :supersede)
      (let ((s (make-two-way-stream is os))
	    (x (vector nil nil nil nil)))
	(assert (eql (read-sequence x s) 4))
	(assert (equalp x #(3 8 19 41)))
	(let ((y #(100 5 18 211 0 178)))
	  (assert (eql (write-sequence y s) y))
	  (close s)))))
    (with-open-file
     (s pn2 :direction :input :element-type element-type)
     (let ((x (vector nil nil nil nil nil nil nil)))
       (values
	(read-sequence x s)
	x))))
  6
  #(100 5 18 211 0 178 nil))

(deftest make-two-way-stream.14
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-two-way-stream is os)))
    (values
     (write-string "abc" s)
     (clear-input s)
     (write-string "def" s)
     (get-output-stream-string os)))
  "abc" nil "def" "abcdef")

;;; Error tests

(deftest make-two-way-stream.error.1
  (signals-error (make-two-way-stream) program-error)
  t)

(deftest make-two-way-stream.error.2
  (signals-error (make-two-way-stream (make-string-input-stream "foo"))
		 program-error)
  t)

(deftest make-two-way-stream.error.3
  (signals-error (let ((os (make-string-output-stream)))
		   (make-two-way-stream (make-string-input-stream "foo")
					os nil))
		 program-error)
  t)

(deftest make-two-way-stream.error.4
  (check-type-error #'(lambda (x) (make-two-way-stream x (make-string-output-stream)))
		    #'(lambda (x) (and (streamp x) (input-stream-p x))))
  nil)

(deftest make-two-way-stream.error.5
  (check-type-error #'(lambda (x) (make-two-way-stream x (make-string-output-stream)))
		    #'(lambda (x) (and (streamp x) (input-stream-p x)))
		    *streams*)
  nil)

(deftest make-two-way-stream.error.6
  (check-type-error #'(lambda (x) (make-two-way-stream (make-string-input-stream "foo") x))
		    #'(lambda (x) (and (streamp x) (output-stream-p x))))
  nil)

(deftest make-two-way-stream.error.7
  (check-type-error #'(lambda (x) (make-two-way-stream (make-string-input-stream "foo") x))
		    #'(lambda (x) (and (streamp x) (output-stream-p x)))
		    *streams*)
  nil)



						