();-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 25 08:04:56 1998
;;;; Contains: Tests of UNINTERN

(in-package :cl-test)
(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; unintern

;; Simple unintern of an internal symbol, package explicitly
;; given as a package object
(deftest unintern.1
  (progn
    (safely-delete-package "H")
    (prog1
	(let ((p (make-package "H" :use nil))
	      (i 0) x y)
	  (intern "FOO" p)
	  (multiple-value-bind*
	   (sym access)
	   (find-symbol "FOO" p)
	   (and
	    (eqt access :internal)
	    (unintern (progn (setf x (incf i)) sym)
		      (progn (setf y (incf i)) p))
	    (eql i 2) (eql x 1) (eql y 2)
	    (null (symbol-package sym))
	    (not (find-symbol "FOO" p)))))
      (safely-delete-package "H")))
  t)

;; Simple unintern, package taken from the *PACKAGES*
;; special variable (should this have unwind protect?)
(deftest unintern.2
  (progn
    (safely-delete-package "H")
    (prog1
	(let ((*PACKAGE* (make-package "H" :use nil)))
	  (intern "FOO")
	  (multiple-value-bind* (sym access)
	      (find-symbol "FOO")
	    (and
	     (eqt access :internal)
	     (unintern sym)
	     (null (symbol-package sym))
	     (not (find-symbol "FOO")))))
      (safely-delete-package "H")))
  t)

;; Simple unintern, package given as string
(deftest unintern.3
  (progn
    (safely-delete-package "H")
    (prog1
	(let ((p (make-package "H" :use nil)))
	  (intern "FOO" p)
	  (multiple-value-bind* (sym access)
	      (find-symbol "FOO" p)
	    (and
	     (eqt access :internal)
	     (unintern sym "H")
	     (null (symbol-package sym))
	     (not (find-symbol "FOO" p)))))
      (safely-delete-package "H")))
  t)

;; Simple unintern, package given as symbol
(deftest unintern.4
  (progn
    (safely-delete-package "H")
    (prog1
	(let ((p (make-package "H" :use nil)))
	  (intern "FOO" p)
	  (multiple-value-bind* (sym access)
	      (find-symbol "FOO" p)
	    (and
	     (eqt access :internal)
	     (unintern sym '#:|H|)
	     (null (symbol-package sym))
	     (not (find-symbol "FOO" p)))))
      (safely-delete-package "H")))
  t)

;; Simple unintern, package given as character
(deftest unintern.5
  (handler-case
   (progn
     (safely-delete-package "H")
     (prog1
	 (let ((p (make-package "H" :use nil)))
	   (intern "FOO" p)
	   (multiple-value-bind* (sym access)
	       (find-symbol "FOO" p)
	     (and
	      (eqt access :internal)
	      (unintern sym #\H)
	      (null (symbol-package sym))
	      (not (find-symbol "FOO" p)))))
       (safely-delete-package "H")))
   (error (c) c))
  t)


;; Test more complex examples of unintern

;; Unintern an external symbol that is also inherited

(deftest unintern.6
  (handler-case
   (progn
     (safely-delete-package "H")
     (safely-delete-package "G")
     (make-package "G" :use nil)
     (export (intern "FOO" "G") "G")
     (make-package "H" :use '("G"))
     (export (intern "FOO" "H") "H")
     ;; At this point, G:FOO is also an external
     ;; symbol of H.
     (multiple-value-bind* (sym1 access1)
	 (find-symbol "FOO" "H")
       (and sym1
	    (eqt access1 :external)
	    (equal "FOO" (symbol-name sym1))
	    (eqt (find-package "G")
		 (symbol-package sym1))
	    (unintern sym1 "H")
	    (multiple-value-bind* (sym2 access2)
		(find-symbol "FOO" "H")
	      (and (eqt sym1 sym2)
		   (eqt (symbol-package sym1)
			(find-package "G"))
		   (eqt access2 :inherited))))))
   (error (c) c))
  t)

;; unintern a symbol that is shadowing another symbol

(deftest unintern.7
    (block failed
      (safely-delete-package "H")
      (safely-delete-package "G")
      (let* ((pg (make-package "G" :use nil))
	     (ph (make-package "H" :use (list pg))))
	(handler-case
	   (shadow "FOO" ph)
	   (error (c) (return-from failed (list :shadow-error c))))
	(export (intern "FOO" pg) pg)
	;; At this point, H::FOO shadows G:FOO
	(multiple-value-bind* (sym1 access1)
	    (find-symbol "FOO" ph)
	  (and
	   sym1
	   (eqt (symbol-package sym1) ph)
	   (eqt access1 :internal)
	   (equal (list sym1) (package-shadowing-symbols ph))
	   (unintern sym1 ph)
	   (multiple-value-bind* (sym2 access2)
	       (find-symbol "FOO" ph)
	     (and (not (eqt sym1 sym2))
		  (eqt access2 :inherited)
		  (null (symbol-package sym1))
		  (eqt (symbol-package sym2) pg)))))))
  t)

;; Error situation: when the symbol is uninterned, creates
;; a name conflict from two used packages
(deftest unintern.8
  (block failed
    (safely-delete-package "H")
    (safely-delete-package "G1")
    (safely-delete-package "G2")
    (let* ((pg1 (make-package "G1" :use nil))
	   (pg2 (make-package "G2" :use nil))
	   (ph (make-package "H" :use (list pg1 pg2))))
      (handler-case
       (shadow "FOO" ph)
       (error (c) (return-from failed (list :shadow-error c))))
      (let ((gsym1 (intern "FOO" pg1))
	    (gsym2 (intern "FOO" pg2)))
	(export gsym1 pg1)
	(export gsym2 pg2)
	(multiple-value-bind* (sym1 access1)
	    (find-symbol "FOO" ph)
	  (and
	   (equal (list sym1) (package-shadowing-symbols ph))
	   (not (eqt sym1 gsym1))
	   (not (eqt sym1 gsym2))
	   (eqt (symbol-package sym1) ph)
	   (eqt access1 :internal)
	   (equal (symbol-name sym1) "FOO")
	   (handler-case
	    (progn
	      (unintern sym1 ph)
	      nil)
	    (error (c) 
		   (format t "Properly threw an error: ~S~%" c)
		   t)))))))
  t)

;; Now, inherit the same symbol through two intermediate
;; packages.  No error should occur when the shadowing
;; is removed
(deftest unintern.9
  (block failed
    (safely-delete-package "H")
    (safely-delete-package "G1")
    (safely-delete-package "G2")
    (safely-delete-package "G3")
    (let* ((pg3 (make-package "G3" :use nil))
	   (pg1 (make-package "G1" :use (list pg3)))
	   (pg2 (make-package "G2" :use (list pg3)))
	   (ph  (make-package "H"  :use (list pg1 pg2))))
      (handler-case
       (shadow "FOO" ph)
       (error (c) (return-from failed (list :shadow-error c))))
      (let ((gsym (intern "FOO" pg3)))
	(export gsym pg3)
	(export gsym pg1)
	(export gsym pg2)
	(multiple-value-bind* (sym access)
	    (find-symbol "FOO" ph)
	  (and
	   (equal (list sym) (package-shadowing-symbols ph))
	   (not (eqt sym gsym))
	   (equal (symbol-name sym) "FOO")
	   (equal (symbol-package sym) ph)
	   (eqt access :internal)
	   (handler-case
	    (and (unintern sym ph)
		 (multiple-value-bind* (sym2 access2)
		     (find-symbol "FOO" ph)
		   (and (eqt gsym sym2)
			(eqt access2 :inherited))))
	    (error (c) c)))))))
  t)

;;; Specialized sequence tests

(defmacro def-unintern-test (test-name name-form)
  `(deftest ,test-name
     (let ((name ,name-form))
       (safely-delete-package name)
       (prog1
	   (let ((p (make-package name :use nil)))
	     (intern "FOO" p)
	     (multiple-value-bind*
	      (sym access)
	      (find-symbol "FOO" p)
	      (and
	       (eqt access :internal)
	       (unintern sym name)
	       (null (symbol-package sym))
	       (not (find-symbol "FOO" p)))))
	 (safely-delete-package name)))
     t))

(def-unintern-test unintern.10
  (make-array 5 :initial-contents "TEST1" :element-type 'base-char))

(def-unintern-test unintern.11
  (make-array 10 :initial-contents "TEST1ABCDE"
	      :fill-pointer 5 :element-type 'base-char))

(def-unintern-test unintern.12
  (make-array 10 :initial-contents "TEST1ABCDE"
	      :fill-pointer 5 :element-type 'character))

(def-unintern-test unintern.13
  (make-array 5 :initial-contents "TEST1"
	      :adjustable t :element-type 'base-char))

(def-unintern-test unintern.14
  (make-array 5 :initial-contents "TEST1"
	      :adjustable t :element-type 'character))

(def-unintern-test unintern.15
  (let* ((etype 'base-char)
	 (name0 (make-array 10 :element-type etype
			    :initial-contents "xxxxxTEST1")))
    (make-array 5 :element-type etype
		:displaced-to name0
		:displaced-index-offset 5)))

(def-unintern-test unintern.16
  (let* ((etype 'character)
	 (name0 (make-array 10 :element-type etype
			    :initial-contents "xxxxxTEST1")))
    (make-array 5 :element-type etype
		:displaced-to name0
		:displaced-index-offset 5)))


(deftest unintern.error.1
  (signals-error (unintern) program-error)
  t)

(deftest unintern.error.2
  (signals-error (unintern '#:x "CL-TEST" nil) program-error)
  t)
