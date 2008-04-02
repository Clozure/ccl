;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 25 08:09:18 1998
;;;; Contains: Tests of DEFPACKAGE

(in-package :cl-test)

(compile-and-load "package-aux.lsp")

(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; defpackage

;; Test basic defpackage call, with no options
;; The use-list is implementation dependent, so
;; we don't examine it here.
;; Try several ways of specifying the package name.
(deftest defpackage.1
  (loop
   for n in '("H" #:|H| #\H) count
   (not
    (progn
      (safely-delete-package "H")
      (let ((p (ignore-errors (eval `(defpackage ,n)))))
	(and
	 (packagep p)
	 (equal (package-name p)              "H")
	 ;; (equal (package-use-list p)          nil)
	 (equal (package-used-by-list p)      nil)
	 (equal (package-nicknames p)         nil)
	 (equal (package-shadowing-symbols p) nil)
	 (null (documentation p t))
	 )))))
  0)

;; Test :nicknames option
;; Do not check use-list, because it is implementation dependent
;; Try several ways of specifying a nickname.
(deftest defpackage.2
  (loop
   for n in '("I" #:|I| #\I) count
   (not
    (ignore-errors
      (progn
	(safely-delete-package "H")
	(let ((p (ignore-errors
		   (eval `(defpackage "H" (:nicknames ,n "J"))))))
	  (and
	   (packagep p)
	   (equal (package-name p)              "H")
	   ;; (equal (package-use-list p)          nil)
	   (equal (package-used-by-list p)      nil)
	   (equal (sort (copy-list (package-nicknames p))
			#'string<)
		  '("I" "J"))
	   (equal (package-shadowing-symbols p) nil)
	   (null (documentation p t))
	   ))))))
  0)

;; Test defpackage with documentation option
;; Do not check use-list, because it is implementation dependent
(deftest defpackage.3
  (let ()
    (safely-delete-package "H")
    (ignore-errors
      (let ((p (eval '(defpackage "H" (:documentation "This is a doc string")))))
	(and
	 (packagep p)
	 (equal (package-name p)              "H")
	 ;; (equal (package-use-list p)          nil)
	 (equal (package-used-by-list p)      nil)
	 (equal (package-nicknames p)	nil)
	 (equal (package-shadowing-symbols p) nil)
	 ;; The spec says implementations are free to discard
	 ;; documentations, so this next form was wrong.
	 ;; Instead, we'll just computation DOCUMENTATION
	 ;; and throw away the value.
	 ;; (equal (documentation p t) "This is a doc string")
	 (progn (documentation p t) t)
	 ))))
  t)

;; Check use argument
;; Try several ways of specifying the package to be used
(deftest defpackage.4
  (progn
    (set-up-packages)
    (loop
     for n in '("A" :|A| #\A) count
     (not
      (ignore-errors
	(progn
	  (safely-delete-package "H")
	  (let ((p (ignore-errors (eval `(defpackage "H" (:use ,n))))))
	    (and
	     (packagep p)
	     (equal (package-name p)              "H")
	     (equal (package-use-list p)          (list (find-package "A")))
	     (equal (package-used-by-list p)      nil)
	     (equal (package-nicknames p)         nil)
	     (equal (package-shadowing-symbols p) nil)
	     (eql (num-symbols-in-package p)
		  (num-external-symbols-in-package "A"))
	     (equal (documentation p t)             nil)
	     )))))))
  0)

;; Test defpackage shadow option, and null use
(deftest defpackage.5
  (let ()
    (safely-delete-package "H")
    (ignore-errors
      (let ((p (ignore-errors (eval `(defpackage "H" (:use) 
				       (:shadow "foo"))))))
	(mapcar
	 #'notnot
	 (list
	  (packagep p)
	  (equal (package-name p)              "H")
	  (equal (package-use-list p)          nil)
	  (equal (package-used-by-list p)      nil)
	  (equal (package-nicknames p)         nil)
	  (eql (num-symbols-in-package p) 1)
	  (multiple-value-bind* (sym access)
	      (find-symbol "foo" p)
	    (and (eqt access :internal)
		 (equal (symbol-name sym) "foo")
		 (equal (symbol-package sym) p)
		 (equal (package-shadowing-symbols p)
			(list sym))))
	  (equal (documentation p t)             nil)
	 )))))
  (t t t t t t t t))

;; Test defpackage shadow and null use, with several ways
;; of specifying the name of the shadowed symbol
(deftest defpackage.6
  (loop
   for s in '(:|f| #\f)
   collect
   (ignore-errors
     (safely-delete-package "H")
     (let ((p (ignore-errors (eval `(defpackage "H" 
				      (:use)
				      (:shadow ,s))))))
       (mapcar
	#'notnot
	(list
	 (packagep p)
	 (equal (package-name p)              "H")
	 (equal (package-use-list p)          nil)
	 (equal (package-used-by-list p)      nil)
	 (equal (package-nicknames p)         nil)
	 (eql (num-symbols-in-package p) 1)
	 (multiple-value-bind* (sym access)
	     (find-symbol "f" p)
	   (and (eqt access :internal)
		(equal (symbol-name sym) "f")
		(equal (symbol-package sym) p)
		(equal (package-shadowing-symbols p)
		       (list sym))))
	 (equal (documentation p t)             nil)
	 )))))
  ((t t t t t t t t)
   (t t t t t t t t)))


;; Testing defpackage with shadowing-import-from.
;; Test several ways of specifying the symbol name
(deftest defpackage.7
  (progn
    (safely-delete-package "H")
    (safely-delete-package "G")
    (let ((pg (make-package "G" :use nil)))
      ;; Populate package G with several symbols
      (export (intern "A" pg) pg)
      (export (intern "foo" pg) pg)
      (intern "bar" pg)
      ;; Do test with several ways of specifying the
      ;; shadowing-imported symbol
      (loop
       for n in '("A" :|A| #\A)
       collect
       (ignore-errors
	 (safely-delete-package "H")
	 (let ((p (ignore-errors
		    (eval
		     `(defpackage "H"
			(:use)
			(:shadowing-import-from "G" ,n))))))
	   (mapcar
	    #'notnot
	    (list
	     (packagep p)
	     (equal (package-name p)              "H")
	     (equal (package-use-list p)          nil)
	     (equal (package-used-by-list p)      nil)
	     (equal (package-nicknames p)         nil)
	     (eql (num-symbols-in-package p) 1)
	     (multiple-value-bind* (sym access)
		 (find-symbol "A" p)
	       (and (eqt access :internal)
		    (equal (symbol-name sym) "A")
		    (equal (symbol-package sym) pg)
		    (equal (package-shadowing-symbols p)
			   (list sym))))
	     (equal (documentation p t)             nil)
	     )))))))
  ((t t t t t t t t)
   (t t t t t t t t)
   (t t t t t t t t)))

;; Test import-from option
;; Test for each way of specifying the imported symbol name,
;;  and for each way of specifying the package name from which
;;   the symbol is imported
(deftest defpackage.8
    (progn
      (safely-delete-package "H")
      (safely-delete-package "G")
      (let ((pg (eval '(defpackage "G" (:use) (:intern "A" "B" "C")))))
	(loop
	  for pn in '("G" #:|G| #\G)
	  collect
	  (loop
	   for n in '("B" #:|B| #\B)
	   collect
	   (ignore-errors
	     (safely-delete-package "H")
	     (let ((p (ignore-errors
			(eval `(defpackage
				 "H" (:use)
				 (:import-from ,pn ,n "A"))))))
	       (mapcar
		#'notnot
		(list
		 (packagep p)
		 (equal (package-name p)              "H")
		 (equal (package-use-list p)          nil)
		 (equal (package-used-by-list p)      nil)
		 (equal (package-nicknames p)         nil)
		 (equal (package-shadowing-symbols p) nil)
		 (eql (num-symbols-in-package p) 2)
		 (multiple-value-bind* (sym access)
		     (find-symbol "A" p)
		   (and (eqt access :internal)
			(equal (symbol-name sym) "A")
			(equal (symbol-package sym) pg)))
		 (multiple-value-bind* (sym access)
		     (find-symbol "B" p)
		   (and (eqt access :internal)
			(equal (symbol-name sym) "B")
			(equal (symbol-package sym) pg)))
		 (equal (documentation p t)             nil)
		 ))))))))
    (((t t t t t t t t t t) (t t t t t t t t t t) (t t t t t t t t t t))
     ((t t t t t t t t t t) (t t t t t t t t t t) (t t t t t t t t t t))
     ((t t t t t t t t t t) (t t t t t t t t t t) (t t t t t t t t t t))))

;; Test defpackage with export option

(deftest defpackage.9
  (progn
    (loop
     for n in '("Z" #:|Z| #\Z)
     collect
     (ignore-errors
       (safely-delete-package "H")
       (let ((p (ignore-errors
		  (eval `(defpackage
			   "H"
			   (:export "Q" ,n "R") (:use))))))
	 (mapcar
	  #'notnot
	  (list
	   (packagep p)
	   (equal (package-name p)              "H")
	   (equal (package-use-list p)          nil)
	   (equal (package-used-by-list p)      nil)
	   (equal (package-nicknames p)         nil)
	   (equal (package-shadowing-symbols p) nil)
	   (eql (num-symbols-in-package p) 3)
	   (loop
	    for s in '("Q" "Z" "R") do
	    (unless
		(multiple-value-bind* (sym access)
		    (find-symbol s p)
		  (and (eqt access :external)
		       (equal (symbol-name sym) s)
		       (equal (symbol-package sym) p)))
	      (return nil))
	    finally (return t))
	   ))))))
  ((t t t t t t t t)(t t t t t t t t)(t t t t t t t t)))

;; Test defpackage with the intern option

(deftest defpackage.10
  (progn
    (loop
     for n in '("Z" #:|Z| #\Z)
     collect
     (ignore-errors
       (safely-delete-package "H")
       (let ((p (ignore-errors
		  (eval `(defpackage
			   "H"
			   (:intern "Q" ,n "R") (:use))))))
	 (mapcar
	  #'notnot
	  (list
	   (packagep p)
	   (equal (package-name p)              "H")
	   (equal (package-use-list p)          nil)
	   (equal (package-used-by-list p)      nil)
	   (equal (package-nicknames p)         nil)
	   (equal (package-shadowing-symbols p) nil)
	   (eql (num-symbols-in-package p) 3)
	   (loop
	    for s in '("Q" "Z" "R") do
	    (unless
		(multiple-value-bind* (sym access)
		    (find-symbol s p)
		  (and (eqt access :internal)
		       (equal (symbol-name sym) s)
		       (equal (symbol-package sym) p)))
	      (return nil))
	    finally (return t))
	   ))))))
  ((t t t t t t t t) (t t t t t t t t) (t t t t t t t t)))

;; Test defpackage with size

(deftest defpackage.11
  (let ()
    (ignore-errors
      (safely-delete-package "H")
      (let ((p (ignore-errors
		 (eval '(defpackage "H" (:use) (:size 0))))))
	(mapcar
	 #'notnot
	 (list
	  (packagep p)
	  (equal (package-name p)              "H")
	  (equal (package-use-list p)          nil)
	  (equal (package-used-by-list p)      nil)
	  (equal (package-nicknames p)         nil)
	  (equal (package-shadowing-symbols p) nil)
	  (zerop (num-symbols-in-package p)))))))
  (t t t t t t t))

(deftest defpackage.12
  (let ()
    (ignore-errors
      (safely-delete-package "H")
      (let ((p (ignore-errors
		 (eval '(defpackage "H" (:use) (:size 10000))))))
	(mapcar
	 #'notnot
	 (list
	  (packagep p)
	  (equal (package-name p)              "H")
	  (equal (package-use-list p)          nil)
	  (equal (package-used-by-list p)      nil)
	  (equal (package-nicknames p)         nil)
	  (equal (package-shadowing-symbols p) nil)
	  (zerop (num-symbols-in-package p)))))))
  (t t t t t t t))

;; defpackage error handling

;; Repeated size field should cause a program-error
(deftest defpackage.13
  (progn
    (safely-delete-package "H")
    (signals-error
     (defpackage "H" (:use) (:size 10) (:size 20))
     program-error))
  t)

;; Repeated documentation field should cause a program-error
(deftest defpackage.14
  (progn
    (safely-delete-package "H")
    (signals-error
     (defpackage "H" (:use)
       (:documentation "foo")
       (:documentation "bar"))
     program-error))
  t)

;; When a nickname refers to an existing package or nickname,
;; signal a package-error

(deftest defpackage.15
  (progn
    (safely-delete-package "H")
    (signals-error
     (defpackage "H" (:use) (:nicknames "A"))
     package-error))
  t)

(deftest defpackage.16
  (progn
    (safely-delete-package "H")
    (signals-error
     (defpackage "H" (:use) (:nicknames "Q"))
     package-error))
  t)

;; Names in :shadow, :shadowing-import-from, :import-from, and :intern
;; must be disjoint, or a package-error is signalled.

;; :shadow and :shadowing-import-from
(deftest defpackage.17
  (progn
    (safely-delete-package "H")
    (safely-delete-package "G")
    (eval '(defpackage "G" (:use) (:export "A")))
    (signals-error
     (defpackage "H" (:use)
       (:shadow "A")
       (:shadowing-import-from "G" "A"))
     program-error))
  t)

;; :shadow and :import-from
(deftest defpackage.18
  (progn
    (safely-delete-package "H")
    (safely-delete-package "G")
    (eval '(defpackage "G" (:use) (:export "A")))
    (signals-error
     (defpackage "H" (:use)
       (:shadow "A")
       (:import-from "G" "A"))
     program-error))
  t)

;; :shadow and :intern
(deftest defpackage.19
  (progn
    (safely-delete-package "H")
    (signals-error
     (defpackage "H" (:use)
       (:shadow "A")
       (:intern "A"))
     program-error))
  t)

;; :shadowing-import-from and :import-from
(deftest defpackage.20
  (progn
    (safely-delete-package "H")
    (safely-delete-package "G")
    (eval '(defpackage "G" (:use) (:export "A")))
    (signals-error
     (defpackage "H" (:use)
       (:shadowing-import-from "G" "A")
       (:import-from "G" "A"))
     program-error))
  t)

;; :shadowing-import-from and :intern
(deftest defpackage.21
  (progn
    (safely-delete-package "H")
    (safely-delete-package "G")
    (eval '(defpackage "G" (:use) (:export "A")))
    (signals-error
     (defpackage "H" (:use)
       (:shadowing-import-from "G" "A")
       (:intern "A"))
     program-error))
  t)

;; :import-from and :intern
(deftest defpackage.22
  (progn
    (safely-delete-package "H")
    (safely-delete-package "G")
    (eval '(defpackage "G" (:use) (:export "A")))
    (signals-error
     (defpackage "H" (:use)
       (:import-from "G" "A")
       (:intern "A"))
     program-error))
  t)

;; Names given to :export and :intern must be disjoint,
;;  otherwise signal a program-error
(deftest defpackage.23
  (progn
    (safely-delete-package "H")
    (signals-error
     (defpackage "H" (:use)
       (:export "A")
       (:intern "A"))
     program-error))
  t)

;; :shadowing-import-from signals a correctable package-error
;;  if the symbol is not accessible in the named package
(deftest defpackage.24
  (progn
    (safely-delete-package "H")
    (safely-delete-package "G")
    (eval '(defpackage "G" (:use)))
    (handle-non-abort-restart
     (eval '(defpackage "H" (:shadowing-import-from
			     "G" "NOT-THERE")))))
  success)

;; :import-from signals a correctable package-error if a symbol with
;; the indicated name is not accessible in the package indicated

(deftest defpackage.25
  (progn
    (safely-delete-package "H")
    (safely-delete-package "G")
    (eval '(defpackage "G" (:use)))
    (handle-non-abort-restart
     (eval '(defpackage "H" (:import-from "G" "NOT-THERE")))))
  success)

;; A big test that combines all the options to defpackage

(deftest defpackage.26
  (let ()
    (ignore-errors
      (flet
	  ((%do-it%
	    (args)
	    (safely-delete-package "H")
	    (safely-delete-package "G1")
	    (safely-delete-package "G2")
	    (safely-delete-package "G3")
	    (let ((pg1 
		   (progn
		     (format t "Making G1...~%")
		     (eval '(defpackage "G1"
			      (:use)
			      (:export "A" "B" "C")
			      (:intern "D" "E" "F")))))
		  (pg2
		   (progn
		     (format t "Making G2...~%")
		     (eval '(defpackage "G2" 
			      (:use)
			      (:export "A" "D" "G")
			      (:intern "E" "H" "I")))))
		  (pg3 
		   (progn
		     (format t "Making G3...~%")
		     (eval '(defpackage "G3" 
			      (:use)
			      (:export "J" "K" "L")
			      (:intern "M" "N" "O"))))))
	      (let ((p (eval (list* 'defpackage "H" (copy-tree args)))))
		(prog
		 ()
		 (unless (packagep p) (return 1))
		 (unless (equal (package-name p) "H") (return 2))
		 (unless (equal (package-name pg1) "G1") (return 3))
		 (unless (equal (package-name pg2) "G2") (return 4))
		 (unless (equal (package-name pg3) "G3") (return 5))
		 (unless
		     (equal (sort (copy-list (package-nicknames p)) #'string<)
			    '("H1" "H2"))
		   (return 6))
		 (unless
		     (or
		      (equal (package-use-list p) (list pg1 pg2))
		      (equal (package-use-list p) (list pg2 pg1)))
		   (return 7))
		 (unless (equal (package-used-by-list pg1) (list p))
		   (return 8))
		 (unless (equal (package-used-by-list pg2) (list p))
		   (return 9))
		 (when (package-used-by-list pg3) (return 10))
		 (unless (equal (sort (mapcar #'symbol-name
					      (package-shadowing-symbols p))
				      #'string<)
				'("A" "B"))
		   (return 10))
		 (let ((num 11))
		   (unless
		       (every
			#'(lambda (str acc pkg)
			    (multiple-value-bind*
			     (sym access)
			     (find-symbol str p)
			     (or
			      (and (or (not acc) (equal (symbol-name sym) str))
				   (or (not acc) (equal (symbol-package sym) pkg))
				   (equal access acc)
				   (incf num))
			      (progn
				(format t
					"Failed on str = ~S, acc = ~S, pkg = ~S, sym = ~S, access = ~S~%"
					str acc pkg sym access)
				nil))))
			(list "A" "B" "C" "D" "E" "F" "G"
			      "H" "I" "J" "K" "L" "M" "N" "O")
			(list :internal :internal
			      :external :inherited
			      nil nil
			      :inherited :internal
			      nil nil
			      nil :external
			      nil nil
			      :internal)
			(list pg2 p pg1 pg2 nil nil
			      pg2 p nil nil nil pg3
			      nil nil pg3))
		     (return num)))
		 (return 'success))))))
	(let ((args '((:nicknames "H1" "H2")
		      (:use "G1" "G2")
		      (:shadow "B")
		      (:shadowing-import-from "G2" "A")
		      (:import-from "G3" "L" "O")
		      (:intern "D" "H")
		      (:export "L" "C")
		      (:size 20)
		      (:documentation "A test package"))))
	  (list (%do-it% args)
		(%do-it% (reverse args)))))))
  (success success))

(def-macro-test defpackage.error.1
  (defpackage :nonexistent-package (:use)))