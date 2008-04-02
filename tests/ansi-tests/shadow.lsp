;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 25 08:01:20 1998
;;;; Contains: Tests of SHADOW

(in-package :cl-test)
(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shadow

(deftest shadow.1
  (prog1
      (progn
	(safely-delete-package "TEST5")
	(safely-delete-package "TEST4")
	(handler-case
	 (let* ((p1 (prog1
			(make-package "TEST4" :use nil)
		      (export (intern "A" "TEST4") "TEST4")))
		(p2 (make-package "TEST5" :use '("TEST4")))
		(r1 (package-shadowing-symbols "TEST4"))
		(r2 (package-shadowing-symbols "TEST5")))
	   (multiple-value-bind* (s1 kind1)
	       (find-symbol "A" p1)
	     (multiple-value-bind* (s2 kind2)
		 (find-symbol "A" p2)
	       (let ((r3 (shadow "A" p2)))
		 (multiple-value-bind* (s3 kind3)
		     (find-symbol "A" p2)
		   (list 
		    (package-name p1)
		    (package-name p2)
		    r1 r2
		    (symbol-name s1)
		    (package-name (symbol-package s1))
		    kind1
		    (symbol-name s2)
		    (package-name (symbol-package s2))
		    kind2
		    r3
		    (symbol-name s3)
		    (package-name (symbol-package s3))
		    kind3))))))
	 (error (c) c)))
    (safely-delete-package "TEST5")
    (safely-delete-package "TEST4"))
  ("TEST4" "TEST5" nil nil "A" "TEST4" :external
   "A" "TEST4" :inherited
   t
   "A" "TEST5" :internal))

(deftest shadow.2
  (progn
    (safely-delete-package "H")
    (safely-delete-package "G")
    (handler-case
     (let* ((p1 (prog1
		    (make-package "G" :use nil)
		  (export (intern "A" "G") "G")))
	    (p2 (make-package "H" :use '("G")))
	    (r1 (package-shadowing-symbols "G"))
	    (r2 (package-shadowing-symbols "H")))
       (multiple-value-bind* (s1 kind1)
	   (find-symbol "A" p1)
	 (multiple-value-bind* (s2 kind2)
	     (find-symbol "A" p2)
	   (let ((r3 (shadow "A" "H")))
	     (multiple-value-bind* (s3 kind3)
		 (find-symbol "A" p2)
	       (prog1
		   (list (package-name p1) (package-name p2)
			 r1 r2 (symbol-name s1) (package-name (symbol-package s1))
			 kind1 (symbol-name s2) (package-name (symbol-package s2))
			 kind2 r3 (symbol-name s3) (package-name (symbol-package s3))
			 kind3)
		 (safely-delete-package p2)
		 (safely-delete-package p1)
		 ))))))
     (error (c)
	    (safely-delete-package "H")
	    (safely-delete-package "G")
	    c)))
  ("G" "H" nil nil "A" "G" :external
   "A" "G" :inherited
   t
   "A" "H" :internal))

;; shadow in which the package is given
;; by a character
(deftest shadow.3
  (progn
    (safely-delete-package "H")
    (safely-delete-package "G")
    (handler-case
     (let* ((p1 (prog1
		    (make-package "G" :use nil)
		  (export (intern "A" "G") "G")))
	    (p2 (make-package "H" :use '("G")))
	    (r1 (package-shadowing-symbols "G"))
	    (r2 (package-shadowing-symbols "H")))
       (multiple-value-bind* (s1 kind1)
	   (find-symbol "A" p1)
	 (multiple-value-bind* (s2 kind2)
	     (find-symbol "A" p2)
	   (let ((r3 (shadow "A" #\H)))
	     (multiple-value-bind* (s3 kind3)
		 (find-symbol "A" p2)
	       (prog1
		   (list (package-name p1) (package-name p2)
			 r1 r2 (symbol-name s1) (package-name (symbol-package s1))
			 kind1 (symbol-name s2) (package-name (symbol-package s2))
			 kind2 r3 (symbol-name s3) (package-name (symbol-package s3))
			 kind3)
		 (safely-delete-package p2)
		 (safely-delete-package p1)
		 ))))))
     (error (c)
	    (safely-delete-package "H")
	    (safely-delete-package "G")
	    c)))
  ("G" "H" nil nil "A" "G" :external
   "A" "G" :inherited
   t
   "A" "H" :internal))


;; shadow on an existing internal symbol returns the existing symbol
(deftest shadow.4
  (prog1
      (handler-case
       (progn
	 (safely-delete-package :G)
	 (make-package :G :use nil)
	 (let ((s1 (intern "X" :G)))
	   (shadow "X" :G)
	   (multiple-value-bind* (s2 kind)
	       (find-symbol "X" :G)
	     (list (eqt s1 s2)
		   (symbol-name s2)
		   (package-name (symbol-package s2))
		   kind))))
       (error (c) c))
    (safely-delete-package "G"))
  (t "X" "G" :internal))


;; shadow of an existing shadowed symbol returns the symbol
(deftest shadow.5
  (prog1
      (handler-case
       (progn
	 (safely-delete-package :H)
	 (safely-delete-package :G)
	 (make-package :G :use nil)
	 (export (intern "X" :G) :G)
	 (make-package :H :use '("G"))
	 (shadow "X" :H)
	 (multiple-value-bind* (s1 kind1)
	     (find-symbol "X" :H)
	   (shadow "X" :H)
	   (multiple-value-bind* (s2 kind2)
	       (find-symbol "X" :H)
	     (list (eqt s1 s2) kind1 kind2))))
       (error (c) c))
    (safely-delete-package :H)
    (safely-delete-package :G))
  (t :internal :internal))

;; Shadow several names simultaneously

(deftest shadow.6
  (prog1
      (handler-case
       (progn
	 (safely-delete-package :G)
	 (make-package :G :use nil)
	 (shadow '("X" "Y" |Z|) :G)
	 (let ((results
		(append (multiple-value-list
			 (find-symbol "X" :G))
			(multiple-value-list
			 (find-symbol "Y" :G))
			(multiple-value-list
			 (find-symbol "Z" :G))
			nil)))
	   (list
	    (symbol-name (first results))
	    (second results)
	    (symbol-name (third results))
	    (fourth results)
	    (symbol-name (fifth results))
	    (sixth results)
	    (length (package-shadowing-symbols :G)))))
       (error (c) c))
    (safely-delete-package :G))
  ("X" :internal "Y" :internal "Z" :internal 3))

;; Same, but shadow character string designators
(deftest shadow.7
  (prog1
      (handler-case
       (let ((i 0) x y)
	 (safely-delete-package :G)
	 (make-package :G :use nil)
	 (shadow (progn (setf x (incf i)) '(#\X #\Y))
		 (progn (setf y (incf i)) :G))
	 (let ((results
		(append (multiple-value-list
			 (find-symbol "X" :G))
			(multiple-value-list
			 (find-symbol "Y" :G))
			nil)))
	   (list
	    i x y
	    (symbol-name (first results))
	    (second results)
	    (symbol-name (third results))
	    (fourth results)
	    (length (package-shadowing-symbols :G)))))
       (error (c) c))
    (safely-delete-package :G))
  (2 1 2 "X" :internal "Y" :internal 2))

;;; Specialized string tests

(deftest shadow.8
  (prog1
      (handler-case
       (progn
	 (safely-delete-package :G)
	 (make-package :G :use nil)
	 (let* ((name (make-array '(1) :initial-contents "X"
				  :element-type 'base-char))
		(s1 (intern name :G)))
	   (shadow name :G)
	   (multiple-value-bind* (s2 kind)
	       (find-symbol "X" :G)
	     (list (eqt s1 s2)
		   (symbol-name s2)
		   (package-name (symbol-package s2))
		   kind))))
       (error (c) c))
    (safely-delete-package "G"))
  (t "X" "G" :internal))

(deftest shadow.9
  (prog1
      (handler-case
       (progn
	 (safely-delete-package :G)
	 (make-package :G :use nil)
	 (let* ((name (make-array '(3) :initial-contents "XYZ"
				  :fill-pointer 1
				  :element-type 'character))
		(s1 (intern name :G)))
	   (shadow name :G)
	   (multiple-value-bind* (s2 kind)
	       (find-symbol "X" :G)
	     (list (eqt s1 s2)
		   (symbol-name s2)
		   (package-name (symbol-package s2))
		   kind))))
       (error (c) c))
    (safely-delete-package "G"))
  (t "X" "G" :internal))

(deftest shadow.10
  (prog1
      (handler-case
       (progn
	 (safely-delete-package :G)
	 (make-package :G :use nil)
	 (let* ((name (make-array '(1) :initial-contents "X"
				  :adjustable t
				  :element-type 'base-char))
		(s1 (intern name :G)))
	   (shadow name :G)
	   (multiple-value-bind* (s2 kind)
	       (find-symbol "X" :G)
	     (list (eqt s1 s2)
		   (symbol-name s2)
		   (package-name (symbol-package s2))
		   kind))))
       (error (c) c))
    (safely-delete-package "G"))
  (t "X" "G" :internal))




(deftest shadow.error.1
  (signals-error (shadow) program-error)
  t)

(deftest shadow.error.2
  (signals-error (shadow "X" "CL-USER" nil) program-error)
  t)
