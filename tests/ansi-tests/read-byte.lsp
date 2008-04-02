;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jan 17 17:30:49 2004
;;;; Contains: Tests of READ-BYTE, WRITE-BYTE

(in-package :cl-test)

(deftest read-byte.1
  (let ((s (open "foo.txt"
		 :direction :output
		 :if-exists :supersede
		 :element-type '(unsigned-byte 8))))
    (values
     (write-byte 17 s)
     (close s)
     (progn
       (setq s (open "foo.txt"
		     :direction :input
		     :element-type '(unsigned-byte 8)))
       (read-byte s))
     (close s)))
  17 t 17 t)

(deftest read-byte.2
  (let ((s (open "foo.txt"
		 :direction :output
		 :if-exists :supersede
		 :element-type '(unsigned-byte 8))))
    (values
     (close s)
     (progn
        (setq s (open "foo.txt"
		     :direction :input
		     :element-type '(unsigned-byte 8)))
	(read-byte s nil 'foo))
     (read-byte s nil)
     (close s)))
  t foo nil t)

(deftest read-byte.3
  (loop with b1 = 0
	and b2 = 0
	for i from 1 to 32
	do (let ((s (open "foo.txt"
			  :direction :output
			  :if-exists :supersede
			  :element-type `(unsigned-byte ,i))))
	     (write-byte (1- (ash 1 i)) s)
	     (write-byte 1 s)
	     (close s))
	unless (let ((s (open "foo.txt"
			      :direction :input
			      :element-type `(unsigned-byte ,i))))
		 (prog1
		   (and (eql (setq b1 (read-byte s)) (1- (ash 1 i)))
			(eql (setq b2 (read-byte s)) 1))
		   (close s)))
	collect (list i b1 b2))
  nil)

(deftest read-byte.4
  (loop with b1 = 0
	and b2 = 0
	for i from 33 to 200 by 7
	do (let ((s (open "foo.txt"
			  :direction :output
			  :if-exists :supersede
			  :element-type `(unsigned-byte ,i))))
	     (write-byte (1- (ash 1 i)) s)
	     (write-byte 1 s)
	     (close s))
	unless (let ((s (open "foo.txt"
			      :direction :input
			      :element-type `(unsigned-byte ,i))))
		 (prog1
		     (and (eql (setq b1 (read-byte s)) (1- (ash 1 i)))
			  (eql (setq b2 (read-byte s)) 1))
		   (close s)))
	collect (list i b1 b2))
  nil)

;;; Error tests

(deftest read-byte.error.1
  (signals-error (read-byte) program-error)
  t)

(deftest read-byte.error.2
  (progn
    (let ((s (open "foo.txt"
		   :direction :output
		   :if-exists :supersede
		  :element-type `(unsigned-byte 8))))
      (close s))
    (signals-error
     (let ((s (open "foo.txt"
		   :direction :input
		   :element-type '(unsigned-byte 8))))
       (read-byte s))
     end-of-file))
  t)

(deftest read-byte.error.3
  (progn
    (let ((s (open "foo.txt"
		   :direction :output
		   :if-exists :supersede)))
      (close s))
    (signals-error
     (let ((s (open "foo.txt" :direction :input)))
       (unwind-protect
	   (read-byte s)
	 (close s)))
     error))
  t)

(deftest read-byte.error.4
  (signals-error-always
   (progn
     (let ((s (open "foo.txt"
		    :direction :output
		    :if-exists :supersede
		    :element-type '(unsigned-byte 8))))
       (close s))
     (let ((s (open "foo.txt"
		    :direction :input
		    :element-type '(unsigned-byte 8))))
       (unwind-protect
	   (read-byte s t)
	 (close s))))
   end-of-file)
  t t)

(deftest read-byte.error.5
  (check-type-error #'read-byte #'streamp)
  nil)

(deftest read-byte.error.6
  (progn
    (let ((s (open "foo.txt"
		   :direction :output
		   :if-exists :supersede
		  :element-type '(unsigned-byte 8))))
      (close s))
    (signals-error
     (let ((s (open "foo.txt"
		   :direction :input
		   :element-type '(unsigned-byte 8))))
       (unwind-protect
	   (read-byte s t t nil)
	 (close s)))
     program-error))
  t)

       
(deftest write-byte.error.1
  (signals-error (write-byte) program-error)
  t)

(deftest write-byte.error.2
  (signals-error (write-byte 0) program-error)
  t)

(deftest write-byte.error.3
  (signals-error
   (let ((s (open "foo.txt"
		  :direction :output
		  :if-exists :supersede
		  :element-type '(unsigned-byte 8))))
     (unwind-protect
	 (write 1 s nil)
       (close s)))
   program-error)
  t)

(deftest write-byte.error.4
  (check-type-error #'(lambda (x) (write-byte 0 x)) #'streamp)
  nil)

(deftest write-byte.error.5
   (signals-error
    (let ((s (open "foo.txt"
		   :direction :output
		   :if-exists :supersede)))
      (unwind-protect
	  (write 1 s)
	(close s)))
    error)
   t)



    
    
