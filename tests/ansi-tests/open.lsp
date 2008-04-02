;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Jan 23 05:36:55 2004
;;;; Contains: Tests of OPEN

(in-package :cl-test)

;;; Input streams

(defun generator-for-element-type (type)
  (etypecase type
   ((member character base-char)
    #'(lambda (i) (aref "abcdefghijklmnopqrstuvwxyz" (mod i 26))))
   ((member signed-byte unsigned-byte bit)
    #'(lambda (i) (logand i 1)))
   (cons
    (let ((op (car type))
	  (arg1 (cadr type))
	  (arg2 (caddr type)))
      (ecase op
	(unsigned-byte
	 (let ((mask (1- (ash 1 arg1))))
	   #'(lambda (i) (logand i mask))))
	(signed-byte
	 (let ((mask (1- (ash 1 (1- arg1)))))
	   #'(lambda (i) (logand i mask))))
	(integer
	 (let* ((lo arg1)
		(hi arg2)
	       (lower-bound
		(etypecase lo
		  (integer lo)
		  (cons (1+ (car lo)))))
	       (upper-bound
		(etypecase hi
		  (integer hi)
		  (cons (1- (car hi)))))
	       (range (1+ (- upper-bound lower-bound))))
	   #'(lambda (i) (+ lower-bound (mod i range))))))))))

(compile 'generator-for-element-type)

(defmacro def-open-test (name args form expected
			      &key
			      (notes nil notes-p)
			      (build-form nil build-form-p)
			      (element-type 'character element-type-p)
			      (pathname #p"tmp.dat"))
	  
  (when element-type-p
    (setf args (append args (list :element-type `',element-type))))

  (unless build-form-p
    (let ((write-element-form
	   (cond
	    ((subtypep element-type 'integer)
	     `(write-byte
	       (funcall (the function
			  (generator-for-element-type ',element-type)) i)
	       os))
	    ((subtypep element-type 'character)
	     `(write-char
	       (funcall (the function
			  (generator-for-element-type ',element-type)) i)
	       os)))))
      (setq build-form
	    `(with-open-file
	      (os pn :direction :output
		  ,@(if element-type-p
			`(:element-type ',element-type))
		  :if-exists :supersede)
	      (assert (open-stream-p os))
	      (dotimes (i 10) ,write-element-form)
	      (finish-output os)
	    ))))
			      
  `(deftest ,name
     ,@(when notes-p `(:notes ,notes))
     (let ((pn ,pathname))
       (delete-all-versions pn)
       ,build-form
       (let ((s (open pn ,@args)))
	 (unwind-protect
	     (progn
	       (assert (open-stream-p s))
	       (assert (typep s 'file-stream))
	       ,@
	       (unless (member element-type '(signed-byte unsigned-byte))
		 #-allegro
		 `((assert (subtypep ',element-type
				     (stream-element-type s))))
		 #+allegro nil
		 )
	       ,form)
	   (close s))))
     ,@expected))

;; (compile 'def-open-test)

(def-open-test open.1 () (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.2 (:direction :input)
  (values (read-line s nil)) ("abcdefghij") :element-type character)
(def-open-test open.3 (:direction :input)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.4 (:direction :input)
  (values (read-line s nil)) ("abcdefghij") :element-type base-char)
(def-open-test open.5 (:if-exists :error)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.6 (:if-exists :error :direction :input)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.7 (:if-exists :new-version)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.8 (:if-exists :new-version :direction :input)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.9 (:if-exists :rename)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.10 (:if-exists :rename :direction :input)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.11 (:if-exists :rename-and-delete)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.12 (:if-exists :rename-and-delete :direction :input)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.13 (:if-exists :overwrite)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.14 (:if-exists :overwrite :direction :input)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.15 (:if-exists :append)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.16 (:if-exists :append :direction :input)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.17 (:if-exists :supersede)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.18 (:if-exists :supersede :direction :input)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.19 (:if-exists nil)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.20 (:if-exists nil :direction :input)
  (values (read-line s nil)) ("abcdefghij"))

(def-open-test open.21 (:if-does-not-exist nil)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.22 (:if-does-not-exist nil :direction :input)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.23 (:if-does-not-exist :error)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.24 (:if-does-not-exist :error :direction :input)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.25 (:if-does-not-exist :create)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.26 (:if-does-not-exist :create :direction :input)
  (values (read-line s nil)) ("abcdefghij"))

(def-open-test open.27 (:external-format :default)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.28 (:external-format :default :direction :input)
  (values (read-line s nil)) ("abcdefghij"))

(def-open-test open.29 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 0 1 0 1 0 1 0 1)) :element-type (unsigned-byte 1))
(def-open-test open.30 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 0 1 0 1 0 1 0 1)) :element-type (unsigned-byte 1))

(def-open-test open.31 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 0 1 2 3 0 1)) :element-type (unsigned-byte 2))
(def-open-test open.32 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 0 1 2 3 0 1)) :element-type (unsigned-byte 2))

(def-open-test open.33 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 0 1)) :element-type (unsigned-byte 3))
(def-open-test open.34 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 0 1)) :element-type (unsigned-byte 3))

(def-open-test open.35 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 4))
(def-open-test open.36 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 4))

(def-open-test open.37 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 5))
(def-open-test open.38 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 5))

(def-open-test open.39 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 6))
(def-open-test open.40 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 6))

(def-open-test open.41 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 7))
(def-open-test open.42 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 7))

(def-open-test open.43 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 8))
(def-open-test open.44 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 8))

(def-open-test open.45 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 9))
(def-open-test open.46 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 9))

(def-open-test open.47 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 10))
(def-open-test open.48 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 10))

(def-open-test open.49 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 20))
(def-open-test open.50 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 20))

(def-open-test open.51 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 25))
(def-open-test open.52 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 25))

(def-open-test open.53 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 30))
(def-open-test open.54 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 30))

(def-open-test open.55 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 32))
(def-open-test open.56 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 32))

(def-open-test open.57 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 33))
(def-open-test open.58 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 33))

(def-open-test open.59 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 0 1 0 1 0 1 0 1)) :element-type unsigned-byte)
(def-open-test open.60 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 0 1 0 1 0 1 0 1)) :element-type unsigned-byte)

(def-open-test open.61 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 0 1 0 1 0 1 0 1)) :element-type signed-byte)
(def-open-test open.62 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 0 1 0 1 0 1 0 1)) :element-type signed-byte)


(def-open-test open.63 ()
  (values (read-line s nil)) ("abcdefghij")
  :pathname "tmp.dat")

(def-open-test open.64 ()
  (values (read-line s nil)) ("abcdefghij")
  :pathname (logical-pathname "CLTEST:TMP.DAT"))

;;; It works on recognizable subtypes.
(deftest open.65
  (let ((type '(or (integer 0 1) (integer 100 200)))
	(pn #p"tmp.dat")
	(vals '(0 1 100 120 130 190 200 1 0 150)))
    (or
     (not (subtypep type 'integer))
     (progn
       (with-open-file
	(os pn :direction :output
	    :element-type type
	    :if-exists :supersede)
	(dolist (e vals) (write-byte e os)))
       (let ((s (open pn :direction :input
		      :element-type type))
	     (seq (make-array 10)))
	 (unwind-protect
	     (progn (read-sequence seq s) seq)
	   (close s))
	 (notnot (every #'eql seq vals))))))
  t)

;;; FIXME: Add -- tests for when the filespec is a stream

(deftest open.66
  (let ((pn #p"tmp.dat"))
    (delete-all-versions pn)
    (with-open-file
     (s pn :direction :io :if-exists :rename-and-delete
	:if-does-not-exist :create)
     (format s "some stuff~%")
     (finish-output s)
     (let ((is (open s :direction :input)))
       (unwind-protect
	   (values
	    (read-char is)
	    (notnot (file-position s :start))
	    (read-line is)
	    (read-line s))
	 (close is)))))
  #\s
  t
  "ome stuff"
  "some stuff")

(deftest open.67
  (let ((pn #p"tmp.dat"))
    (delete-all-versions pn)
    (let ((s (open pn :direction :output)))
      (unwind-protect
	  (progn
	    (format s "some stuff~%")
	    (finish-output s)
	    (close s)
	    (let ((is (open s :direction :input)))
	      (unwind-protect
		  (values (read-line is))
		(close is))))
	(when (open-stream-p s) (close s)))))
  "some stuff")

;;; FIXME: Add -- tests for when element-type is :default

;;; Tests of file creation

(defmacro def-open-output-test
  (name args form expected
	&rest keyargs
	&key
	(element-type 'character)
	(build-form
	 `(dotimes (i 10)
	    ,(cond
	      ((subtypep element-type 'integer)
	       `(write-byte
		 (funcall (the function
			    (generator-for-element-type ',element-type)) i)
		 s))
	      ((subtypep element-type 'character)
	       `(write-char
		 (funcall (the function
			    (generator-for-element-type ',element-type)) i)
		 s)))))
	&allow-other-keys)
  `(def-open-test ,name (:direction :output ,@args)
     (progn
       ,build-form
       (assert (output-stream-p s))
       ,form)
     ,expected
     :build-form nil
     ,@keyargs))

;; (compile 'def-open-output-test)

(def-open-output-test open.output.1 ()
  (progn (close s)
	 (with-open-file (is #p"tmp.dat") (values (read-line is nil))))
  ("abcdefghij"))

(def-open-output-test open.output.2 ()
  (progn (close s)
	 (with-open-file (is "tmp.dat") (values (read-line is nil))))
  ("abcdefghij")
  :pathname "tmp.dat")

(def-open-output-test open.output.3
  ()
  (progn (close s)
	 (with-open-file (is (logical-pathname "CLTEST:TMP.DAT"))
			 (values (read-line is nil))))
  ("abcdefghij")
  :pathname (logical-pathname "CLTEST:TMP.DAT"))

(def-open-output-test open.output.4 ()
  (progn (close s)
	 (with-open-file (is #p"tmp.dat" :element-type 'character)
			 (values (read-line is nil))))
  ("abcdefghij")
  :element-type character)

(def-open-output-test open.output.5 ()
  (progn (close s) (with-open-file (is #p"tmp.dat"
				       :element-type 'base-char)
				   (values (read-line is nil))))
  ("abcdefghij")
  :element-type base-char)

(def-open-output-test open.output.6 ()
  (progn (close s) (with-open-file (is #p"tmp.dat"
				       :element-type '(integer 0 1))
				   (let ((seq (make-array 10)))
				     (read-sequence seq is)
				     seq)))
  (#(0 1 0 1 0 1 0 1 0 1))
  :element-type (integer 0 1))

(def-open-output-test open.output.7 ()
  (progn (close s) (with-open-file (is #p"tmp.dat"
				       :element-type 'bit)
				   (let ((seq (make-array 10)))
				     (read-sequence seq is)
				     seq)))
  (#(0 1 0 1 0 1 0 1 0 1))
  :element-type bit)

(def-open-output-test open.output.8 ()
  (progn (close s) (with-open-file (is #p"tmp.dat"
				       :element-type '(unsigned-byte 1))
				   (let ((seq (make-array 10)))
				     (read-sequence seq is)
				     seq)))
  (#(0 1 0 1 0 1 0 1 0 1))
  :element-type (unsigned-byte 1))

(def-open-output-test open.output.9 ()
  (progn (close s) (with-open-file (is #p"tmp.dat"
				       :element-type '(unsigned-byte 2))
				   (let ((seq (make-array 10)))
				     (read-sequence seq is)
				     seq)))
  (#(0 1 2 3 0 1 2 3 0 1))
  :element-type (unsigned-byte 2))

(def-open-output-test open.output.10 ()
  (progn (close s) (with-open-file (is #p"tmp.dat"
				       :element-type '(unsigned-byte 3))
				   (let ((seq (make-array 10)))
				     (read-sequence seq is)
				     seq)))
  (#(0 1 2 3 4 5 6 7 0 1))
  :element-type (unsigned-byte 3))

(def-open-output-test open.output.11 ()
  (progn (close s) (with-open-file (is #p"tmp.dat"
				       :element-type '(unsigned-byte 4))
				   (let ((seq (make-array 10)))
				     (read-sequence seq is)
				     seq)))
  (#(0 1 2 3 4 5 6 7 8 9))
  :element-type (unsigned-byte 4))


(def-open-output-test open.output.12 ()
  (progn (close s) (with-open-file (is #p"tmp.dat"
				       :element-type '(unsigned-byte 6))
				   (let ((seq (make-array 10)))
				     (read-sequence seq is)
				     seq)))
  (#(0 1 2 3 4 5 6 7 8 9))
  :element-type (unsigned-byte 6))

(def-open-output-test open.output.13 ()
  (progn (close s) (with-open-file (is #p"tmp.dat"
				       :element-type '(unsigned-byte 8))
				   (let ((seq (make-array 10)))
				     (read-sequence seq is)
				     seq)))
  (#(0 1 2 3 4 5 6 7 8 9))
  :element-type (unsigned-byte 8))

(def-open-output-test open.output.14 ()
  (progn (close s) (with-open-file (is #p"tmp.dat"
				       :element-type '(unsigned-byte 12))
				   (let ((seq (make-array 10)))
				     (read-sequence seq is)
				     seq)))
  (#(0 1 2 3 4 5 6 7 8 9))
  :element-type (unsigned-byte 12))

(def-open-output-test open.output.15 ()
  (progn (close s) (with-open-file (is #p"tmp.dat"
				       :element-type '(unsigned-byte 16))
				   (let ((seq (make-array 10)))
				     (read-sequence seq is)
				     seq)))
  (#(0 1 2 3 4 5 6 7 8 9))
  :element-type (unsigned-byte 16))

(def-open-output-test open.output.16 ()
  (progn (close s) (with-open-file (is #p"tmp.dat"
				       :element-type '(unsigned-byte 24))
				   (let ((seq (make-array 10)))
				     (read-sequence seq is)
				     seq)))
  (#(0 1 2 3 4 5 6 7 8 9))
  :element-type (unsigned-byte 24))

(def-open-output-test open.output.17 ()
  (progn (close s) (with-open-file (is #p"tmp.dat"
				       :element-type '(unsigned-byte 32))
				   (let ((seq (make-array 10)))
				     (read-sequence seq is)
				     seq)))
  (#(0 1 2 3 4 5 6 7 8 9))
  :element-type (unsigned-byte 32))

(def-open-output-test open.output.18 ()
  (progn (close s) (with-open-file (is #p"tmp.dat"
				       :element-type '(unsigned-byte 64))
				   (let ((seq (make-array 10)))
				     (read-sequence seq is)
				     seq)))
  (#(0 1 2 3 4 5 6 7 8 9))
  :element-type (unsigned-byte 64))

(def-open-output-test open.output.19 ()
  (progn (close s) (with-open-file (is #p"tmp.dat"
				       :element-type '(unsigned-byte 100))
				   (let ((seq (make-array 10)))
				     (read-sequence seq is)
				     seq)))
  (#(0 1 2 3 4 5 6 7 8 9))
  :element-type (unsigned-byte 100))

(deftest open.output.20
  (let ((pn #p"tmp.dat"))
    (with-open-file (s pn :direction :output :if-exists :supersede))
    (open pn :direction :output :if-exists nil))
  nil)

(def-open-test open.output.21 (:if-exists :new-version :direction :output)
  (progn (write-sequence "wxyz" s)
	 (close s)
	 (with-open-file
	  (s pn :direction :input)
	  (values (read-line s nil))))
  ("wxyz")
  :notes (:open-if-exists-new-version-no-error)
  )

(def-open-test open.output.22 (:if-exists :rename :direction :output)
  (progn (write-sequence "wxyz" s)
	 (close s)
	 (with-open-file
	  (s pn :direction :input)
	  (values (read-line s nil))))
  ("wxyz"))

(def-open-test open.output.23 (:if-exists :rename-and-delete
					  :direction :output)
  (progn (write-sequence "wxyz" s)
	 (close s)
	 (with-open-file
	  (s pn :direction :input)
	  (values (read-line s nil))))
  ("wxyz"))

(def-open-test open.output.24 (:if-exists :overwrite
					  :direction :output)
  (progn (write-sequence "wxyz" s)
	 (close s)
	 (with-open-file
	  (s pn :direction :input)
	  (values (read-line s nil))))
  ("wxyzefghij"))

(def-open-test open.output.25 (:if-exists :append
					  :direction :output)
  (progn (write-sequence "wxyz" s)
	 (close s)
	 (with-open-file
	  (s pn :direction :input)
	  (values (read-line s nil))))
  ("abcdefghijwxyz"))

(def-open-test open.output.26 (:if-exists :supersede
					  :direction :output)
  (progn (write-sequence "wxyz" s)
	 (close s)
	 (with-open-file
	  (s pn :direction :input)
	  (values (read-line s nil))))
  ("wxyz"))

(def-open-output-test open.output.27 (:if-does-not-exist :create
							 :direction :output)
  (progn (close s)
	 (with-open-file
	  (is pn :direction :input)
	  (values (read-line is nil))))
  ("abcdefghij"))

(deftest open.output.28
  (let ((pn #p"tmp.dat"))
    (delete-all-versions pn)
    (open pn :direction :output :if-does-not-exist nil))
  nil)

(def-open-output-test open.output.28a (:external-format :default)
  (progn (close s)
	 (with-open-file (is #p"tmp.dat") (values (read-line is nil))))
  ("abcdefghij"))

(def-open-output-test open.output.29
  (:external-format (prog1
		      (with-open-file (s "foo.dat" :direction :output
					 :if-exists :supersede)
				      (stream-external-format s))
		      (delete-all-versions "foo.dat")
		      ))
  (progn (close s)
	 (with-open-file (is #p"tmp.dat") (values (read-line is nil))))
  ("abcdefghij"))

;;; Default behavior of open :if-exists is :create when the version
;;; of the filespec is :newest

(deftest open.output.30
  :notes (:open-if-exists-new-version-no-error)
  (let ((pn (make-pathname :name "tmp" :type "dat" :version :newest)))
    (or (not (eql (pathname-version pn) :newest))
	(progn
	  ;; Create file
	  (let ((s1 (open pn :direction :output :if-exists :overwrite
			  :if-does-not-exist :create)))
	    (unwind-protect
		;; Now try again
		(let ((s2 (open pn :direction :output)))
		  (unwind-protect
		      (write-line "abcdef" s2)
		    (close s2))
		  (unwind-protect
		      (progn
			(setq s2 (open s1 :direction :input))
			(equalt (read-line s2 nil) "abcdef"))
		    (close s2)))
	      (close s1)
	      (delete-all-versions pn)
	      )))))
  t)

(def-open-output-test open.output.31 (:if-exists :rename
				      :direction :output)
  (progn (close s)
	 (with-open-file
	  (is pn :direction :input)
	  (values (read-line is nil))))
  ("abcdefghij"))

(def-open-output-test open.output.32 (:if-exists :rename-and-delete
				      :direction :output)
  (progn (close s)
	 (with-open-file
	  (is pn :direction :input)
	  (values (read-line is nil))))
  ("abcdefghij"))

(def-open-output-test open.output.33 (:if-exists :new-version
				      :direction :output)
  (progn (close s)
	 (with-open-file
	  (is pn :direction :input)
	  (values (read-line is nil))))
  ("abcdefghij"))

(def-open-output-test open.output.34 (:if-exists :supersede
				      :direction :output)
  (progn (close s)
	 (with-open-file
	  (is pn :direction :input)
	  (values (read-line is nil))))
  ("abcdefghij"))

(def-open-output-test open.output.35 (:if-exists nil
				      :direction :output)
  (progn (close s)
	 (with-open-file
	  (is pn :direction :input)
	  (values (read-line is nil))))
  ("abcdefghij"))	    

;;; Add -- tests for when the filespec is a stream


;;; Tests of bidirectional IO

(defmacro def-open-io-test
  (name args form expected
	&rest keyargs
	&key
	(element-type 'character)
	(build-form
	 `(dotimes (i 10)
	    ,(cond
	      ((subtypep element-type 'integer)
	       `(write-byte
		 (funcall (the function
			    (generator-for-element-type ',element-type)) i)
		 s))
	      ((subtypep element-type 'character)
	       `(write-char
		 (funcall (the function
			    (generator-for-element-type ',element-type)) i)
		 s)))))
	&allow-other-keys)
  `(def-open-test ,name (:direction :io ,@args)
     (progn
       ,build-form
       (assert (input-stream-p s))
       (assert (output-stream-p s))
       ,form)
     ,expected
     :build-form nil
     ,@keyargs))

;; (compile 'def-open-io-test)

(def-open-io-test open.io.1 ()
  (progn (file-position s :start)
	 (values (read-line s nil)))
  ("abcdefghij"))

(def-open-io-test open.io.2 ()
  (progn (file-position s :start)
	 (values (read-line s nil)))
  ("abcdefghij")
  :pathname "tmp.dat")

(def-open-io-test open.io.3
  ()
  (progn (file-position s :start)
	 (values (read-line s nil)))
  ("abcdefghij")
  :pathname (logical-pathname "CLTEST:TMP.DAT"))

(def-open-io-test open.io.4 ()
  (progn (file-position s :start)
	 (values (read-line s nil)))
  ("abcdefghij")
  :element-type character)

(def-open-io-test open.io.5 ()
  (progn (file-position s :start)
	 (values (read-line s nil)))
  ("abcdefghij")
  :element-type base-char)

(def-open-io-test open.io.6 ()
  (progn (file-position s :start)
	 (let ((seq (make-array 10)))
	   (read-sequence seq s)
	   seq))
  (#(0 1 0 1 0 1 0 1 0 1))
  :element-type (integer 0 1))

(def-open-io-test open.io.7 ()
  (progn (file-position s :start)
	 (let ((seq (make-array 10)))
	   (read-sequence seq s)
	   seq))
  (#(0 1 0 1 0 1 0 1 0 1))
  :element-type bit)

(def-open-io-test open.io.8 ()
  (progn (file-position s :start)
	 (let ((seq (make-array 10)))
	   (read-sequence seq s)
	   seq))
  (#(0 1 0 1 0 1 0 1 0 1))
  :element-type (unsigned-byte 1))

(def-open-io-test open.io.9 ()
  (progn (file-position s :start)
	 (let ((seq (make-array 10)))
	   (read-sequence seq s)
	   seq))
  (#(0 1 2 3 0 1 2 3 0 1))
  :element-type (unsigned-byte 2))

(def-open-io-test open.io.10 ()
  (progn (file-position s :start)
	 (let ((seq (make-array 10)))
	   (read-sequence seq s)
	   seq))
  (#(0 1 2 3 4 5 6 7 0 1))
  :element-type (unsigned-byte 3))

(def-open-io-test open.io.11 ()
  (progn (file-position s :start)
	 (let ((seq (make-array 10)))
	   (read-sequence seq s)
	   seq))
  (#(0 1 2 3 4 5 6 7 8 9))
  :element-type (unsigned-byte 4))


(def-open-io-test open.io.12 ()
  (progn (file-position s :start)
	 (let ((seq (make-array 10)))
	   (read-sequence seq s)
	   seq))
  (#(0 1 2 3 4 5 6 7 8 9))
  :element-type (unsigned-byte 6))

(def-open-io-test open.io.13 ()
  (progn (file-position s :start)
	 (let ((seq (make-array 10)))
	   (read-sequence seq s)
	   seq))
  (#(0 1 2 3 4 5 6 7 8 9))
  :element-type (unsigned-byte 8))

(def-open-io-test open.io.14 ()
  (progn (file-position s :start)
	 (let ((seq (make-array 10)))
	   (read-sequence seq s)
	   seq))
  (#(0 1 2 3 4 5 6 7 8 9))
  :element-type (unsigned-byte 12))

(def-open-io-test open.io.15 ()
  (progn (file-position s :start)
	 (let ((seq (make-array 10)))
	   (read-sequence seq s)
	   seq))
  (#(0 1 2 3 4 5 6 7 8 9))
  :element-type (unsigned-byte 16))

(def-open-io-test open.io.16 ()
  (progn (file-position s :start)
	 (let ((seq (make-array 10)))
	   (read-sequence seq s)
	   seq))
  (#(0 1 2 3 4 5 6 7 8 9))
  :element-type (unsigned-byte 24))

(def-open-io-test open.io.17 ()
  (progn (file-position s :start)
	 (let ((seq (make-array 10)))
	   (read-sequence seq s)
	   seq))
  (#(0 1 2 3 4 5 6 7 8 9))
  :element-type (unsigned-byte 32))

(def-open-io-test open.io.18 ()
  (progn (file-position s :start)
	 (let ((seq (make-array 10)))
	   (read-sequence seq s)
	   seq))
  (#(0 1 2 3 4 5 6 7 8 9))
  :element-type (unsigned-byte 64))

(def-open-io-test open.io.19 ()
  (progn (file-position s :start)
	 (let ((seq (make-array 10)))
	   (read-sequence seq s)
	   seq))
  (#(0 1 2 3 4 5 6 7 8 9))
  :element-type (unsigned-byte 100))

(deftest open.io.20
  (let ((pn #p"tmp.dat"))
    (with-open-file (s pn :direction :io :if-exists :supersede))
    (open pn :direction :io :if-exists nil))
  nil)

(def-open-test open.io.21 (:if-exists :new-version :direction :io)
  (progn (write-sequence "wxyz" s)
	 (file-position s :start)
	 (values (read-line s nil)))
  ("wxyz")
  :notes (:open-if-exists-new-version-no-error)
  )

(def-open-test open.io.22 (:if-exists :rename :direction :io)
  (progn (write-sequence "wxyz" s)
	 (file-position s :start)
	 (values (read-line s nil)))
  ("wxyz"))

(def-open-test open.io.23 (:if-exists :rename-and-delete
			   :direction :io)
  (progn (write-sequence "wxyz" s)
	 (file-position s :start)
	 (values (read-line s nil)))
  ("wxyz"))

(def-open-test open.io.24 (:if-exists :overwrite
			   :direction :io)
  (progn (write-sequence "wxyz" s)
	 (file-position s :start)
	 (values (read-line s nil)))
  ("wxyzefghij"))

(def-open-test open.io.25 (:if-exists :append
			   :direction :io)
  (progn (write-sequence "wxyz" s)
	 (file-position s :start)
	 (values (read-line s nil)))
  ("abcdefghijwxyz"))

(def-open-test open.io.26 (:if-exists :supersede
			   :direction :io)
  (progn (write-sequence "wxyz" s)
	 (file-position s :start)
	 (values (read-line s nil)))
  ("wxyz"))

(def-open-io-test open.io.27 (:if-does-not-exist :create
			      :direction :io)
  (progn (file-position s :start)
	 (values (read-line s nil)))
  ("abcdefghij"))

(deftest open.io.28
  (let ((pn #p"tmp.dat"))
    (delete-all-versions pn)
    (open pn :direction :io :if-does-not-exist nil))
  nil)

(def-open-io-test open.io.28a (:external-format :default)
  (progn (file-position s :start)
	 (values (read-line s nil)))
  ("abcdefghij"))

(def-open-io-test open.io.29
  (:external-format (prog1
		      (with-open-file (s "foo.dat" :direction :io
					 :if-exists :supersede)
				      (stream-external-format s))
		      (delete-all-versions "foo.dat")
		      ))
  (progn (file-position s :start)
	 (values (read-line s nil)))
  ("abcdefghij"))

;;; Default behavior of open :if-exists is :create when the version
;;; of the filespec is :newest

(deftest open.io.30
  :notes (:open-if-exists-new-version-no-error)
  (let ((pn (make-pathname :name "tmp" :type "dat" :version :newest)))
    (or (not (eql (pathname-version pn) :newest))
	(progn
	  ;; Create file
	  (let ((s1 (open pn :direction :io :if-exists :overwrite
			  :if-does-not-exist :create)))
	    (unwind-protect
		;; Now try again
		(let ((s2 (open pn :direction :io)))
		  (unwind-protect
		      (write-line "abcdef" s2)
		    (close s2))
		  (unwind-protect
		      (progn
			(setq s2 (open s1 :direction :input))
			(equalt (read-line s2 nil) "abcdef"))
		    (close s2)))
	      (close s1)
	      (delete-all-versions pn)
	      )))))
  t)

(def-open-io-test open.io.31 (:if-exists :rename
			      :direction :io)
  (progn (file-position s :start)
	 (values (read-line s nil)))
  ("abcdefghij"))

(def-open-io-test open.io.32 (:if-exists :rename-and-delete
			      :direction :io)
  (progn (file-position s :start)
	 (values (read-line s nil)))
  ("abcdefghij"))

(def-open-io-test open.io.33 (:if-exists :new-version
			      :direction :io)
  (progn (file-position s :start)
	 (values (read-line s nil)))
  ("abcdefghij"))

(def-open-io-test open.io.34 (:if-exists :supersede
			      :direction :io)
  (progn (file-position s :start)
	 (values (read-line s nil)))
  ("abcdefghij"))

(def-open-io-test open.io.35 (:if-exists nil
			      :direction :io)
  (progn (file-position s :start)
	 (values (read-line s nil)))
  ("abcdefghij"))

;;;; :PROBE tests

(defmacro def-open-probe-test
  (name args form
	&key (build-form nil build-form-p)
	(pathname #p"tmp.dat"))
  (unless build-form-p
    (setf build-form
	  `(with-open-file (s pn :direction :output
			      :if-exists :supersede))))
  `(deftest ,name
     (let ((pn ,pathname))
       (delete-all-versions pn)
       ,build-form
       (let ((s (open pn :direction :probe ,@args)))
	 (values
	  ,(if build-form
	       `(and
		 (typep s 'file-stream)
		 (not (open-stream-p s))
		 )
	     `(not s))
	  ,form)))
     t t))

(def-open-probe-test open.probe.1 () t)
(def-open-probe-test open.probe.2 (:if-exists :error) t)
(def-open-probe-test open.probe.3 (:if-exists :new-version) t)
(def-open-probe-test open.probe.4 (:if-exists :rename) t)
(def-open-probe-test open.probe.5 (:if-exists :rename-and-delete) t)
(def-open-probe-test open.probe.6 (:if-exists :overwrite) t)
(def-open-probe-test open.probe.7 (:if-exists :append) t)
(def-open-probe-test open.probe.8 (:if-exists :supersede) t)

(def-open-probe-test open.probe.9 (:if-does-not-exist :error) t)
(def-open-probe-test open.probe.10 (:if-does-not-exist nil) t)
(def-open-probe-test open.probe.11 (:if-does-not-exist :create) t)

(def-open-probe-test open.probe.12 () t :build-form nil)
(def-open-probe-test open.probe.13 (:if-exists :error) t :build-form nil)
(def-open-probe-test open.probe.14 (:if-exists :new-version) t :build-form nil)
(def-open-probe-test open.probe.15 (:if-exists :rename) t :build-form nil)
(def-open-probe-test open.probe.16 (:if-exists :rename-and-delete) t
  :build-form nil)
(def-open-probe-test open.probe.17 (:if-exists :overwrite) t
  :build-form nil)
(def-open-probe-test open.probe.18 (:if-exists :append) t
  :build-form nil)
(def-open-probe-test open.probe.19 (:if-exists :supersede) t
  :build-form nil)

(def-open-probe-test open.probe.20 (:if-does-not-exist nil) t
  :build-form nil)

(deftest open.probe.21
  (let ((pn #p"tmp.dat"))
    (delete-all-versions pn)
    (let ((s (open pn :direction :probe :if-does-not-exist :create)))
      (values
       (notnot s)
       (notnot (probe-file pn)))))
  t t)

(deftest open.probe.22
  (let ((pn #p"tmp.dat"))
    (delete-all-versions pn)
    (let ((s (open pn :direction :probe :if-does-not-exist :create
		   :if-exists :error)))
      (values
       (notnot s)
       (notnot (probe-file pn)))))
  t t)

(def-open-probe-test open.probe.23 (:external-format :default) t)
(def-open-probe-test open.probe.24 (:element-type 'character) t)
(def-open-probe-test open.probe.25 (:element-type 'bit) t)
(def-open-probe-test open.probe.26 (:element-type '(unsigned-byte 2)) t)
(def-open-probe-test open.probe.27 (:element-type '(unsigned-byte 4)) t)
(def-open-probe-test open.probe.28 (:element-type '(unsigned-byte 8)) t)
(def-open-probe-test open.probe.29 (:element-type '(unsigned-byte 9)) t)
(def-open-probe-test open.probe.30 (:element-type '(unsigned-byte 15)) t)
(def-open-probe-test open.probe.31 (:element-type '(unsigned-byte 16)) t)
(def-open-probe-test open.probe.32 (:element-type '(unsigned-byte 17)) t)
(def-open-probe-test open.probe.33 (:element-type '(unsigned-byte 31)) t)
(def-open-probe-test open.probe.34 (:element-type '(unsigned-byte 32)) t)
(def-open-probe-test open.probe.35 (:element-type '(unsigned-byte 33)) t)
(def-open-probe-test open.probe.36 (:element-type '(integer -1002 13112)) t)

;;;; Error tests

(deftest open.error.1
  (signals-error (open) program-error)
  t)

(deftest open.error.2
  (signals-error-always
   (let ((pn #p"tmp.dat"))
     (close (open pn :direction :output :if-does-not-exist :create))
     (open pn :if-exists :error :direction :output))
   file-error)
  t t)

(deftest open.error.3
  (signals-error-always
   (let ((pn #p"tmp.dat"))
     (close (open pn :direction :output :if-does-not-exist :create))
     (open pn :if-exists :error :direction :io))
   file-error)
  t t)

(deftest open.error.4
  (signals-error-always
   (let ((pn #p"tmp.dat"))
     (delete-all-versions pn)
     (open pn))
   file-error)
  t t)

(deftest open.error.5
  (signals-error-always
   (let ((pn #p"tmp.dat"))
     (delete-all-versions pn)
     (open pn :if-does-not-exist :error))
   file-error)
  t t)

(deftest open.error.6
  (signals-error-always
   (let ((pn #p"tmp.dat"))
     (delete-all-versions pn)
     (open pn :direction :input))
   file-error)
  t t)

(deftest open.error.7
  (signals-error-always
   (let ((pn #p"tmp.dat"))
     (delete-all-versions pn)
     (open pn :if-does-not-exist :error :direction :input))
   file-error)
  t t)

(deftest open.error.8
  (signals-error-always
   (let ((pn #p"tmp.dat"))
     (delete-all-versions pn)
     (open pn :direction :output :if-does-not-exist :error))
   file-error)
  t t)

(deftest open.error.9
  (signals-error-always
   (let ((pn #p"tmp.dat"))
     (delete-all-versions pn)
     (open pn :direction :io :if-does-not-exist :error))
   file-error)
  t t)

(deftest open.error.10
  (signals-error-always
   (let ((pn #p"tmp.dat"))
     (delete-all-versions pn)
     (open pn :direction :probe :if-does-not-exist :error))
   file-error)
  t t)

(deftest open.error.11
  (signals-error-always
   (let ((pn #p"tmp.dat"))
     (delete-all-versions pn)
     (open pn :direction :output :if-exists :overwrite))
   file-error)
  t t)

(deftest open.error.12
  (signals-error-always
   (let ((pn #p"tmp.dat"))
     (delete-all-versions pn)
     (open pn :direction :output :if-exists :append))
   file-error)
  t t)

(deftest open.error.13
  (signals-error-always
   (let ((pn #p"tmp.dat"))
     (delete-all-versions pn)
     (open pn :direction :io :if-exists :overwrite))
   file-error)
  t t)

(deftest open.error.14
  (signals-error-always
   (let ((pn #p"tmp.dat"))
     (delete-all-versions pn)
     (open pn :direction :io :if-exists :append))
   file-error)
  t t)

(deftest open.error.15
  (signals-error-always
   (open (make-pathname :name :wild :type "lsp"))
   file-error)
  t t)

(deftest open.error.16
  (signals-error-always
   (open (make-pathname :name "open" :type :wild))
   file-error)
  t t)

(deftest open.error.17
  (signals-error-always
   (let ((pn (make-pathname :name "open" :type "lsp" :version :wild)))
     (if (wild-pathname-p pn) (open pn)
       (error 'file-error)))
   file-error)
  t t)

(deftest open.error.18
  (signals-error-always
   (open #p"tmp.dat" :direction :output :if-exists :supersede
	 :external-form (gensym))
   error)
  t t)


;;; FIXME -- add tests for :element-type :default

;;; FIXME -- add tests for filespec being a specialized string
