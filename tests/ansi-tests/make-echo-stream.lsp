;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Feb 12 04:34:42 2004
;;;; Contains: Tests of MAKE-ECHO-STREAM

(in-package :cl-test)

(deftest make-echo-stream.1
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-echo-stream is os)))
    (values
     (read-char s)
     (get-output-stream-string os)))
  #\f "f")

(deftest make-echo-stream.2
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-echo-stream is os)))
    (get-output-stream-string os))
  "")

(deftest make-echo-stream.3
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-echo-stream is os)))
    (values (read-line s nil)
	    (get-output-stream-string os)))
  "foo" "foo")

;;; Tests of READ-BYTE on echo streams

(deftest make-echo-stream.4
  (let ((pn #p"tmp.dat")
	(pn2 #p"tmp2.dat")
	(element-type '(unsigned-byte 8)))
    (with-open-file (os pn
			:direction :output
			:element-type element-type
			:if-exists :supersede)
		    (loop for x in '(2 3 5 7 11)
			  do (write-byte x os)))
    (with-open-file
     (is pn :direction :input :element-type element-type)
     (values
      (with-open-file
       (os pn2 :direction :output :if-exists :supersede
	   :element-type element-type)
       (let ((s (make-echo-stream is os)))
	 (loop repeat 6 collect (read-byte s nil :eof1))))
      (with-open-file
       (s pn2 :direction :input :element-type element-type)
       (loop repeat 6 collect (read-byte s nil :eof2))))))
  (2 3 5 7 11 :eof1)
  (2 3 5 7 11 :eof2))

(deftest make-echo-stream.5
  (let ((pn #p"tmp.dat")
	(pn2 #p"tmp2.dat")
	(element-type '(unsigned-byte 8)))
    (with-open-file (os pn
			:direction :output
			:element-type element-type
			:if-exists :supersede)
		    (loop for x in '(2 3 5 7 11)
			  do (write-byte x os)))
    (with-open-file
     (is pn :direction :input :element-type element-type)
     (values
      (with-open-file
       (os pn2 :direction :output :if-exists :supersede
	   :element-type element-type)
       (let ((s (make-echo-stream is os)))
	 (loop repeat 6 collect (read-byte s nil 100))))
      (with-open-file
       (s pn2 :direction :input :element-type element-type)
       (loop repeat 6 collect (read-byte s nil 200))))))
  (2 3 5 7 11 100)
  (2 3 5 7 11 200))

(deftest make-echo-stream.6
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-echo-stream is os)))
    (values (coerce (loop repeat 3 collect (read-char-no-hang s)) 'string)
	    (get-output-stream-string os)))
  "foo" "foo")

(deftest make-echo-stream.7
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-echo-stream is os)))
    (values (coerce (loop repeat 4 collect (read-char-no-hang s nil '#\z))
		    'string)
	    (get-output-stream-string os)))
  "fooz" "foo")

;;; peek-char + echo streams is tested in peek-char.lsp
;;; unread-char + echo streams is tested in unread-char.lsp

(deftest make-echo-stream.8
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-echo-stream is os))
	 (x (copy-seq "xxxxxx")))
    (values
     (read-sequence x s)
     x
     (get-output-stream-string os)))
  3
  "fooxxx"
  "foo")

(deftest make-echo-stream.9
  (let ((pn #p"tmp.dat")
	(pn2 #p"tmp2.dat")
	(element-type '(unsigned-byte 8)))
    (with-open-file (os pn
			:direction :output
			:element-type element-type
			:if-exists :supersede)
		    (loop for x in '(2 3 5 7 11)
			  do (write-byte x os)))
    (with-open-file
     (is pn :direction :input :element-type element-type)
     (values
      (with-open-file
       (os pn2 :direction :output :if-exists :supersede
	   :element-type element-type)
       (let ((s (make-echo-stream is os))
	     (x (vector 0 0 0 0 0 0 0 0)))
	 (list (read-sequence x s)
	       x)))
      (with-open-file
       (s pn2 :direction :input :element-type element-type)
       (loop repeat 8 collect (read-byte s nil nil))))))
  (5 #(2 3 5 7 11 0 0 0))
  (2 3 5 7 11 nil nil nil))

(deftest make-echo-stream.10
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-echo-stream is os)))
    (values
     (notnot (open-stream-p s))
     (close s)
     (open-stream-p s)
     (notnot (open-stream-p is))
     (notnot (open-stream-p os))))
  t t nil t t)

(deftest make-echo-stream.11
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-echo-stream is os)))
    (values
     (notnot (listen s))
     (read-char s)
     (notnot (listen s))
     (read-char s)
     (notnot (listen s))
     (read-char s)
     (listen s)))
  t #\f t #\o t #\o nil)

(deftest make-echo-stream.12
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-echo-stream is os)))
    (values
     (notnot (streamp s))
     (notnot (typep s 'stream))
     (notnot (typep s 'echo-stream))
     (notnot (input-stream-p s))
     (notnot (output-stream-p s))
     (notnot (stream-element-type s))))
  t t t t t t)

;;; FIXME
;;; Add tests for clear-input, file-position(?)
;;;  Also, add tests for output operations (since echo-streams are
;;;   bidirectional)

(deftest make-echo-stream.13
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-echo-stream is os)))
    (values
     (write-char #\0 s)
     (close s)
     (get-output-stream-string os)))
  #\0 t "0")

(deftest make-echo-stream.14
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-echo-stream is os)))
    (values
     (terpri s)
     (close s)
     (get-output-stream-string os)))
  nil t #.(string #\Newline))

(deftest make-echo-stream.15
  (let ((pn #p"tmp.dat")
	(pn2 #p"tmp2.dat")
	(element-type '(unsigned-byte 8)))
    (with-open-file (os pn
			:direction :output
			:element-type element-type
			:if-exists :supersede))
    (with-open-file
     (is pn :direction :input :element-type element-type)
     (values
      (with-open-file
       (os pn2 :direction :output :if-exists :supersede
	   :element-type element-type)
       (let ((s (make-echo-stream is os))
	     (x (mapcar #'char-code (coerce "abcdefg" 'list))))
	 (loop for b in x do
	       (assert (equal (list b)
			      (multiple-value-list (write-byte b s)))))
	 (close s)))))
    (with-open-file
     (is pn2 :direction :input :element-type element-type)
     (let ((x (vector 0 0 0 0 0 0 0)))
       (read-sequence x is)
       (values
	(read-byte is nil :done)
	(map 'string #'code-char x)))))
  :done
  "abcdefg")

(deftest make-echo-stream.16
  (let ((pn #p"tmp.dat")
	(pn2 #p"tmp2.dat")
	(element-type '(unsigned-byte 8)))
    (with-open-file (os pn
			:direction :output
			:element-type element-type
			:if-exists :supersede))
    (with-open-file
     (is pn :direction :input :element-type element-type)
     (values
      (with-open-file
       (os pn2 :direction :output :if-exists :supersede
	   :element-type element-type)
       (let ((s (make-echo-stream is os))
	     (x (map 'vector #'char-code "abcdefg")))
	 (assert (equal (multiple-value-list (write-sequence x s)) (list x)))
	 (close s)))))
    (with-open-file
     (is pn2 :direction :input :element-type element-type)
     (let ((x (vector 0 0 0 0 0 0 0)))
       (read-sequence x is)
       (values
	(read-byte is nil :done)
	(map 'string #'code-char x)))))
  :done
  "abcdefg")

(deftest make-echo-stream.17
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-echo-stream is os)))
    (values
     (write-char #\X s)
     (notnot (fresh-line s))
     (finish-output s)
     (force-output s)
     (close s)
     (get-output-stream-string os)))
 #\X t nil nil t #.(coerce '(#\X #\Newline) 'string))

(deftest make-echo-stream.18
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-echo-stream is os)))
    (values
     (write-string "159" s)
     (close s)
     (get-output-stream-string os)))
  "159" t "159")

(deftest make-echo-stream.20
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-echo-stream is os)))
    (values
     (write-string "0159X" s :start 1 :end 4)
     (close s)
     (get-output-stream-string os)))
  "0159X" t "159")

(deftest make-echo-stream.21
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-echo-stream is os)))
    (values
     (write-line "159" s)
     (close s)
     (get-output-stream-string os)))
  "159" t #.(concatenate 'string "159" (string #\Newline)))

(deftest make-echo-stream.22
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-echo-stream is os)))
    (values
     (write-char #\0 s)
     (clear-output s)))
  #\0 nil)

;;; Error tests

(deftest make-echo-stream.error.1
  (signals-error (make-echo-stream) program-error)
  t)

(deftest make-echo-stream.error.2
  (signals-error (make-echo-stream *standard-input*) program-error)
  t)

(deftest make-echo-stream.error.3
  (signals-error (make-echo-stream *standard-input* *standard-output* nil)
		 program-error)
  t)




