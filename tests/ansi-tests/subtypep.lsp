;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jan 29 17:28:19 2003
;;;; Contains: Tests of SUBTYPEP

(in-package :cl-test)

(compile-and-load "types-aux.lsp")

;;; More subtypep tests are in types-and-class.lsp

(deftest subtypep.order.1
  (let ((i 0) x y)
    (values
     (notnot (subtypep (progn (setf x (incf i)) t)
		       (progn (setf y (incf i)) t)))
     i x y))
  t 2 1 2)

(deftest simple-base-string-is-sequence
    (subtypep* 'simple-base-string 'sequence)
  t t)

(deftest subtype.env.1
  (mapcar #'notnot
	  (multiple-value-list (subtypep 'bit 'integer nil)))
  (t t))

(deftest subtype.env.2
  (macrolet
      ((%foo (&environment env)
	     (list 'quote
		   (mapcar #'notnot
			   (multiple-value-list
			    (subtypep 'bit 'integer env))))))
    (%foo))
  (t t))

(deftest subtype.env.3
  (macrolet
      ((%foo (&environment env)
	     (multiple-value-bind (sub good)
		 (subtypep nil (type-of env))
	       (or (not good) (notnot sub)))))
    (%foo))
  t)

(deftest subtype.env.4
  (macrolet
      ((%foo (&environment env)
	     (multiple-value-bind (sub good)
		 (subtypep (type-of env) (type-of env))
	       (or (not good) (notnot sub)))))
    (%foo))
  t)

(deftest subtype.env.5
  (macrolet
      ((%foo (&environment env)
	     (multiple-value-bind (sub good)
		 (subtypep (type-of env) t)
	       (or (not good) (notnot sub)))))
    (%foo))
  t)

(deftest subtypep.error.1
  (signals-error (subtypep) program-error)
  t)

(deftest subtypep.error.2
  (signals-error (subtypep t) program-error)
  t)

(deftest subtypep.error.3
  (signals-error (subtypep t t nil nil) program-error)
  t)

;;; Special cases of types-6 that are/were causing problems in CMU CL

(deftest keyword-is-subtype-of-atom
  (subtypep* 'keyword 'atom)
  t t)

(deftest ratio-is-subtype-of-atom
  (subtypep* 'ratio 'atom)
  t t)

(deftest extended-char-is-subtype-of-atom
  (subtypep* 'extended-char 'atom)
  t t)

(deftest string-is-not-simple-vector
  (subtypep* 'string 'simple-vector)
  nil t)

(deftest base-string-is-not-simple-vector
  (subtypep* 'base-string 'simple-vector)
  nil t)

(deftest simple-string-is-not-simple-vector
  (subtypep* 'simple-string 'simple-vector)
  nil t)

(deftest simple-base-string-is-not-simple-vector
  (subtypep* 'simple-base-string 'simple-vector)
  nil t)

(deftest bit-vector-is-not-simple-vector
  (subtypep* 'bit-vector 'simple-vector)
  nil t)

(deftest simple-bit-vector-is-not-simple-vector
  (subtypep* 'simple-bit-vector 'simple-vector)
  nil t)

;;; Extended characters

(deftest subtypep.extended-char.1
  (if (subtypep* 'character 'base-char)
      (subtypep* 'extended-char nil)
    (values t t))
  t t)

(deftest subtypep.extended-char.2
  (if (subtypep* 'extended-char nil)
      (subtypep* 'character 'base-char)
    (values t t))
  t t)

(deftest subtypep.extended-char.3
  (check-equivalence 'extended-char '(and character (not base-char)))
  nil)


;;; Some and, or combinations

(deftest subtypep.and/or.1
  (check-equivalence
   '(and (or symbol (integer 0 15))
	 (or symbol (integer 10 25)))
   '(or symbol (integer 10 15)))
  nil)

(deftest subtypep.and/or.2
  (check-equivalence
   '(and (or (not symbol) (integer 0 10))
	 (or symbol (integer 11 25)))
   '(integer 11 25))
  nil)

(deftest subtypep.and.1
  (loop for type in *types-list3*
	append (check-equivalence `(and ,type ,type) type))
  nil)

(deftest subtypep.or.1
  (loop for type in *types-list3*
	append (check-equivalence `(or ,type ,type) type))
  nil)

(deftest subtypep.and.2
  (check-equivalence t '(and))
  nil)

(deftest subtypep.or.2
  (check-equivalence nil '(or))
  nil)

(deftest subtypep.and.3
  (loop for type in *types-list3*
	append (check-equivalence `(and ,type) type))
  nil)

(deftest subtypep.or.3
  (loop for type in *types-list3*
	append (check-equivalence `(or ,type) type))
  nil)

(deftest subtypep.and.4
  (let* ((n (length *types-list3*))
	 (a (make-array n :initial-contents *types-list3*)))
    (trim-list
     (loop for i below 1000
	   for tp1 = (aref a (random n))
	   for tp2 = (aref a (random n))
	   append (check-equivalence `(and ,tp1 ,tp2)
				     `(and ,tp2 ,tp1)))
     100))
  nil)

(deftest subtypep.or.4
  (let* ((n (length *types-list3*))
	 (a (make-array n :initial-contents *types-list3*)))
    (trim-list
     (loop for i below 1000
	   for tp1 = (aref a (random n))
	   for tp2 = (aref a (random n))
	   append (check-equivalence `(or ,tp1 ,tp2)
				     `(or ,tp2 ,tp1)))
     100))
  nil)

;;; Check that types that are supposed to be nonempty are
;;; not subtypes of NIL

(deftest subtypep.nil.1
  (loop for (type) in *subtype-table*
	unless (member type '(nil extended-char))
	append (check-all-not-subtypep type nil))
  nil)

(deftest subtypep.nil.2
  (loop for (type) in *subtype-table*
	for class = (find-class type nil)
	unless (or (not class) (member type '(nil extended-char)))
	append (check-all-not-subtypep class nil))
  nil)
