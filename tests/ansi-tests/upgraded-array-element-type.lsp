;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jan 22 20:43:55 2003
;;;; Contains: Tests of UPGRADED-ARRAY-ELEMENT-TYPE

(in-package :cl-test)

(deftest upgraded-array-element-type.1
  (let ((upgraded-bit (upgraded-array-element-type 'bit)))
    (and (empirical-subtypep 'bit upgraded-bit)
	 (empirical-subtypep upgraded-bit 'bit)))
  t)

(deftest upgraded-array-element-type.2
  (let ((upgraded-base-char (upgraded-array-element-type 'base-char)))
    (and (empirical-subtypep 'base-char upgraded-base-char)
	 (empirical-subtypep upgraded-base-char 'base-char)))
  t)

(deftest upgraded-array-element-type.3
  (let ((upgraded-character (upgraded-array-element-type 'character)))
    (and (empirical-subtypep 'character upgraded-character)
	 (empirical-subtypep upgraded-character 'character)))
  t)

(defparameter *upgraded-array-types-to-check*
  `(boolean
    base-char
    character
    t
    ,@(loop for i from 0 to 32 collect `(eql ,(ash 1 i)))
    ,@(loop for i from 0 to 32 collect `(eql ,(1- (ash 1 i))))
    (eql -1)
    ,@(loop for i from 0 to 32
	    collect `(integer 0 (,(ash 1 i))))
    symbol
    ,@(loop for i from 0 to 32
	    collect `(integer ,(- (ash 1 i)) (,(ash 1 i))))
    (integer -10000000000000000000000000000000000
	     10000000000000000000000000000000000)
    float
    short-float
    single-float
    double-float
    complex
    rational
    fixnum
    function
    sequence
    list
    cons
    atom
    symbol))

(deftest upgraded-array-element-type.4
  (loop for type in *upgraded-array-types-to-check*
	for upgraded-type = (upgraded-array-element-type type)
	unless (empirical-subtypep type upgraded-type)
	collect (list type upgraded-type))
  nil)

;; Include an environment (NIL, denoting the default null lexical
;; environment)

(deftest upgraded-array-element-type.5
  (loop for type in *upgraded-array-types-to-check*
	for upgraded-type = (upgraded-array-element-type type nil)
	unless (empirical-subtypep type upgraded-type)
	collect (list type upgraded-type))
  nil)

(deftest upgraded-array-element-type.6
  (macrolet
      ((%foo (&environment env)
	     (empirical-subtypep
	      'bit
	      (upgraded-array-element-type 'bit env))))
    (%foo))
  t)

(deftest upgraded-array-element-type.7
  (let ((upgraded-types (mapcar #'upgraded-array-element-type
				*upgraded-array-types-to-check*)))
    (loop for type in *upgraded-array-types-to-check*
	  for upgraded-type in upgraded-types
	  append
	  (loop for type2 in *upgraded-array-types-to-check*
		for upgraded-type2 in upgraded-types
		when (and (subtypep type type2)
			  (equal (subtypep* upgraded-type upgraded-type)
				 '(nil t)))
		collect (list type type2))))
  nil)

;;; Tests that if Tx is a subtype of Ty, then UAET(Tx) is a subtype
;;;  of UAET(Ty)  (see section 15.1.2.1, paragraph 3)

(deftest upgraded-array-element-type.8
  (let ((upgraded-types (mapcar #'upgraded-array-element-type
				*upgraded-array-types-to-check*)))
    (loop for type1 in *upgraded-array-types-to-check*
	  for uaet1 in upgraded-types
	  append
	  (loop for type2 in *upgraded-array-types-to-check*
		for uaet2 in upgraded-types
		when (and (subtypep type1 type2)
			(not (empirical-subtypep uaet1 uaet2)))
		collect (list type1 type2))))
  nil)

;;; Tests of upgrading NIL (it should be type equivalent to NIL)

(deftest upgraded-array-element-type.nil.1
  (let ((uaet-nil (upgraded-array-element-type nil)))
    (check-predicate (typef `(not ,uaet-nil))))
  nil)
    
;;; Error tests

(deftest upgraded-array-element-type.error.1
  (signals-error (upgraded-array-element-type) program-error)
  t)

(deftest upgraded-array-element-type.error.2
  (signals-error (upgraded-array-element-type 'bit nil nil) program-error)
  t)
