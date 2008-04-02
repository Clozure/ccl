;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jun 12 13:14:53 2004
;;;; Contains: Tests of PPRINT-DISPATCH, SET-PPRINT-DISPATCH

(in-package :cl-test)

(deftest pprint-dispatch.1
  (loop for x in (append *universe* *cl-symbols*)
	for vals = (multiple-value-list (pprint-dispatch x))
	for vals2 = (multiple-value-list (pprint-dispatch
					  x
					  *print-pprint-dispatch*))
	unless
	(and (= (length vals) 2)
	     (= (length vals2) 2)
	     (destructuring-bind (fun foundp)
		 vals
	       (if foundp
		   (and (or (typep fun 'function)
			    (and (symbolp fun)
				 (symbol-function fun)))
			(destructuring-bind (fun2 foundp2)
			    vals2
			  (and (equal fun fun2)
			       foundp2)))
		 (not (cadr vals2)))))
	collect (list x vals vals2))
  nil)
#|
(deftest pprint-dispatch.2
  (loop for sym in *cl-symbols*
	for x = (list sym nil nil)
	for vals = (multiple-value-list (pprint-dispatch x))
	for vals2 = (multiple-value-list (pprint-dispatch
					  x
					  *print-pprint-dispatch*))
	unless
	(and (= (length vals) 2)
	     (= (length vals2) 2)
	     (destructuring-bind (fun foundp)
		 vals
	       (if foundp
		   (and (or (typep fun 'function)
			    (and (symbolp fun)
				 (symbol-function fun)))
			(destructuring-bind (fun2 foundp2)
			    vals2
			  (and (equal fun fun2)
			       foundp2)))
		 (not (cadr vals2)))))
	collect (list x vals vals2))
  nil)
|#

;;; Test that setting the pprint dispatch of a symbol causes
;;; the printing to change, and that it can be unset.
(deftest pprint-dispatch.3
  (my-with-standard-io-syntax
   (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil))
	 (*print-readably* nil)
	 (*print-escape* nil)
	 (*print-pretty* t))
     (let ((f #'(lambda (stream obj)
		  (declare (ignore obj))
		  (write "ABC" :stream stream))))
       (values
	(write-to-string '|X|)
	(set-pprint-dispatch '(eql |X|) f)
	(write-to-string '|X|)
	(set-pprint-dispatch '(eql |X|) nil)
	(write-to-string '|X|)))))
  "X" nil "ABC" nil "X")
       
;;; Test that setting the pprint dispatch of a symbol causes
;;; the printing to change for any real weight, and that it can be unset.
(deftest pprint-dispatch.4
  (my-with-standard-io-syntax
   (loop for v1 in (remove-if-not #'realp *universe*)
	 unless
	 (equal
	  (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil))
		(*print-readably* nil)
		(*print-escape* nil)
		(*print-pretty* t))
	    (let ((f #'(lambda (stream obj)
			 (declare (ignore obj))
			 (write "ABC" :stream stream))))
	      (list
	       (write-to-string '|X|)
	       (set-pprint-dispatch '(eql |X|) f v1)
	       (write-to-string '|X|)
	       (set-pprint-dispatch '(eql |X|) nil)
	       (write-to-string '|X|))))
	  '("X" nil "ABC" nil "X"))
	 collect v1))
  nil)
  
;;; Test that setting the pprint dispatch of a symbol causes
;;; the printing to change, and that it can be unset with any real weight
(deftest pprint-dispatch.5
  (my-with-standard-io-syntax
   (loop for v1 in (remove-if-not #'realp *universe*)
	 unless
	 (equal
	  (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil))
		(*print-readably* nil)
		(*print-escape* nil)
		(*print-pretty* t))
	    (let ((f #'(lambda (stream obj)
			 (declare (ignore obj))
			 (write "ABC" :stream stream))))
	      (list
	       (write-to-string '|X|)
	       (set-pprint-dispatch '(eql |X|) f)
	       (write-to-string '|X|)
	      (set-pprint-dispatch '(eql |X|) nil v1)
	      (write-to-string '|X|))))
	  '("X" nil "ABC" nil "X"))
	 collect v1))
  nil)

;;; Check that specifying the pprint-dispatch table argument to set-pprint-dispatch
;;; causes that table to be changed, not *print-pprint-dispatch*.
(deftest pprint-dispatch.6
  (my-with-standard-io-syntax
   (let ((other-ppd-table (copy-pprint-dispatch nil))
	 (*print-pprint-dispatch* (copy-pprint-dispatch nil))
	 (*print-readably* nil)
	 (*print-escape* nil)
	 (*print-pretty* t))
     (let ((f #'(lambda (stream obj)
		  (declare (ignore obj))
		  (write "ABC" :stream stream))))
       (values
	(write-to-string '|X|)
	(set-pprint-dispatch '(eql |X|) f 0 other-ppd-table)
	(write-to-string '|X|)
	(let ((*print-pprint-dispatch* other-ppd-table))
	  (write-to-string '|X|))
	(set-pprint-dispatch '(eql |X|) f)
	(write-to-string '|X|)
	(set-pprint-dispatch '(eql |X|) nil)
	(write-to-string '|X|)))))
  "X" nil "X" "ABC" nil "ABC" nil "X")

;;; Test that the default weight of set-pprint-dispatch is 0

(deftest pprint-dispatch.7
  (my-with-standard-io-syntax
   (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil))
	 (*print-readably* nil)
	 (*print-escape* nil)
	 (*print-pretty* t))
     (let ((f #'(lambda (stream obj)
		  (declare (ignore obj))
		  (write "ABC" :stream stream)))
	   (g #'(lambda (stream obj)
		  (declare (ignore obj))
		  (write "DEF" :stream stream))))
       (values
	(write-to-string '|X|)
	(set-pprint-dispatch '(eql |X|) f)
	(write-to-string '|X|)
	(set-pprint-dispatch '(member |X| |Y|) g .0001)
	(write-to-string '|X|)
	(write-to-string '|Y|)))))
  "X" nil "ABC" nil "DEF" "DEF")

(deftest pprint-dispatch.8
  (my-with-standard-io-syntax
   (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil))
	 (*print-readably* nil)
	 (*print-escape* nil)
	 (*print-pretty* t))
     (let ((f #'(lambda (stream obj)
		  (declare (ignore obj))
		  (write "ABC" :stream stream)))
	   (g #'(lambda (stream obj)
		  (declare (ignore obj))
		  (write "DEF" :stream stream))))
       (values
	(write-to-string '|X|)
	(set-pprint-dispatch '(eql |X|) f)
	(write-to-string '|X|)
	(set-pprint-dispatch '(member |X| |Y|) g -.0001)
	(write-to-string '|X|)
	(write-to-string '|Y|)))))
  "X" nil "ABC" nil "ABC" "DEF")

;;; Funtion designators in pprint-dispatch

(defun pprint-dispatch-test-fn.1 (stream obj) (declare (ignore obj)) (write "ABC" :stream stream))
(defun pprint-dispatch-test-fn.2 (stream obj) (declare (ignore obj)) (write "DEF" :stream stream))

(deftest pprint-dispatch.9
  (my-with-standard-io-syntax
   (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil))
	 (*print-readably* nil)
	 (*print-escape* nil)
	 (*print-pretty* t))
     (values
      (write-to-string '|X|)
      (multiple-value-list (set-pprint-dispatch '(eql |X|) 'pprint-dispatch-test-fn.1))
      (write-to-string '|X|)
      (multiple-value-list (set-pprint-dispatch '(eql |X|) 'pprint-dispatch-test-fn.2))
      (write-to-string '|X|))))
  "X" (nil) "ABC" (nil) "DEF")

#|
(deftest pprint-dispatch.10
  (my-with-standard-io-syntax
   (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil))
	 (*print-readably* nil)
	 (*print-escape* nil)
	 (*print-pretty* t))
     (let ((f #'(lambda (stream obj)
		  (declare (ignore obj))
		  (write "ABC" :stream stream)))
	   (g #'(lambda (stream obj)
		  (declare (ignore obj))
		  (write "DEF" :stream stream)))
	   (sym (gensym)))
       (setf (symbol-function sym) f)
       (values
	(write-to-string '|X|)
	(set-pprint-dispatch '(eql |X|) sym)
	(write-to-string '|X|)
	(progn
	  (setf (symbol-function sym) g)
	  (write-to-string '|X|))))))
  "X" nil "ABC" "DEF")
|#

;;; Error tests

(deftest pprint-dispatch.error.1
  (signals-error (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil)))
		   (pprint-dispatch))
		 program-error)
  t)

(deftest pprint-dispatch.error.2
  (signals-error (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil)))
		   (pprint-dispatch nil nil nil))
		 program-error)
  t)

(deftest set-pprint-dispatch.error.1
  (signals-error (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil)))
		   (set-pprint-dispatch))
		 program-error)
  t)

(deftest set-pprint-dispatch.error.2
  (signals-error (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil)))
		   (set-pprint-dispatch t))
		 program-error)
  t)

(deftest set-pprint-dispatch.error.3
  (signals-error (let ((table (copy-pprint-dispatch nil)))
		   (set-pprint-dispatch t 'identity 0 table nil))
		 program-error)
  t)


(deftest set-pprint-dispatch.error.4
  (loop for x in *mini-universe*
	unless (or (typep x 'real)
		   (eval `(signals-error (let ((table (copy-pprint-dispatch nil)))
					   (set-pprint-dispatch t 'identity ',x))
					 error)))
	collect x)
  nil)

(deftest set-pprint-dispatch.error.4-unsafe
  (loop for x in *mini-universe*
	unless (or (typep x 'real)
		   (eval `(signals-error (let ((table (copy-pprint-dispatch nil)))
					   (declare (optimize (safety 0)))
					   (set-pprint-dispatch t 'identity ',x))
					 error)))
	collect x)
  nil)

