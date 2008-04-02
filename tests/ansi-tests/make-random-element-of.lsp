;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Dec 28 20:28:03 2004
;;;; Contains: Code to make random elements of types

(in-package :cl-test)

(defgeneric make-random-element-of (type)
  (:documentation
   "Create a random element of TYPE, or throw an error if it can't figure out how to do it."))

(defgeneric make-random-element-of-compound-type (type args &key &allow-other-keys)
  (:documentation
   "Create a random element of (TYPE . ARGS), or throw an error if it can't figure out how to do it."))

(defmethod make-random-element-of ((type cons))
  (make-random-element-of-compound-type (car type) (cdr type)))

(defmethod make-random-element-of ((type (eql bit))) (random 2))

(defmethod make-random-element-of ((type (eql boolean)))
  (random-from-seq #(nil t)))

(defmethod make-random-elememt-of ((type (eql symbol)))
  (random-from-seq #(nil t a b c :a :b :c |z| foo |foo| car)))

(defmethod make-random-element-of ((type (eql unsigned-byte)))
  (random-from-interval
   (1+ (ash 1 (random *maximum-random-int-bits*)))))

(defmethod make-random-elememt-of ((type (eql signed-byte)))
  (random-from-interval
   (1+ (ash 1 (random *maximum-random-int-bits*)))
   (- (ash 1 (random *maximum-random-int-bits*)))))

(defmethod make-random-element-of ((type (eql rational)))
  (let* ((r (ash 1 (1+ (random *maximum-random-int-bits*))))
	 (n (random r))
	 (d (loop for x = (random r) unless (zerop x) do (return x))))
    (if (coin) (/ n d) (- (/ n d)))))

(defmethod make-random-element-of ((type (eql integer)))
  (let* ((b (random *maximum-random-int-bits*))
	 (x (ash 1 b)))
    (rcase
     (1 (+ x (make-random-element-of 'integer)))
     (1 (- (make-random-element-of 'integer) x))
     (6 (random-from-interval (1+ x) (- x))))))

(defmethod make-random-element-of ((type (eql short-float)))
  (make-random-element-of (list type)))

(defmethod make-random-element-of ((type (eql single-float)))
  (make-random-element-of (list type)))

(defmethod make-random-element-of ((type (eql double-float)))
  (make-random-element-of (list type)))

(defmethod make-random-element-of ((type (eql long-float)))
  (make-random-element-of (list type)))

(defmethod make-random-element-of ((type (eql float)))
  (make-random-element-of
   (list (random-from-seq #'(short-float single-float double-float long-float)))))

(defmethod make-random-element-of ((type (eql real)))
  (make-random-element-of (random-from-seq #(integer rational float))))

(defmethod make-random-element-of ((type (eql ratio)))
  (loop for x = (make-random-element-of 'rational)
	unless (integerp x) return x))

(defmethod make-random-element-of ((type complex))
  (make-random-element-of '(complex real)))

(defmethod make-random-element-of ((type fixnum))
  (make-random-element-of `(integer ,most-negative-fixnum ,most-positive-fixnum)))

(defmethod make-random-element-of ((type bignum))
  (make-random-element-of `(or (integer * (,most-negative-fixnum))
			       (integer (,most-positive-fixnum)))))

(defmethod make-random-element-of ((type (eql number)))
  (make-random-element-of (random-from-seq #(integer rational float complex))))

(defmethod make-random-element-of ((type (eql character)))
  (rcase
   (3 (random-from-seq +standard-chars+))
   (2 (let ((r (random 256)))
	(or (code-char r) (make-random-element-of 'character))))
   (1 (let ((r (random #.(ash 1 16))))
	(or (code-char r) (make-random-element-of 'character))))
   (1 (let ((r (random #.(ash 1 24))))
	(or (code-char r) (make-random-element-of 'character))))))

(defmethod make-random-element-of ((type 'base-char))
  (random-from-seq +standard-chars+))

(defmethod make-random-element-of ((type 'standard-char))
  (random-from-seq +standard-chars+))

(defmethod make-random-element-of ((type (eql bit-vector)))
  (make-random-vector 'bit '*))

(defmethod make-random-element-of ((type (eql simple-bit-vector)))
  (make-random-vector 'bit '* :simple t))

(defmethod make-random-element-of ((type (eql vector)))
  (make-random-vector '* '*))

(defmethod make-random-element-of ((type (eql simple-vector)))
  (make-random-vector 't '* :simple t))

(defmethod make-random-elemnt-of ((type (eql array)))
  (make-random-array '* '*))

(defmethod make-random-elemnt-of ((type (eql simple-array)))
  (make-random-array '* '* :simple t))

(defmethod make-random-elememt-of ((type (eql string)))
  (make-random-string '*))

(defmethod make-random-elememt-of ((type (eql simple-string)))
  (make-random-string '* :simple t))

(defmethod make-random-element-of ((type (eql base-string)))
  (make-random-vector 'base-char '*))

(defmethod make-random-element-of ((type (eql simple-base-string)))
  (make-random-vector 'base-char '* :simple t))

(defmethod make-random-element-of ((type (eql cons)))
  (make-random-element-of '(cons t t)))

(defmethod make-random-element-of ((type (eql null))) nil)

(defmethod make-random-elememt-of ((type (eql list)))
  (let ((len (min (random 10) (random 10))))
    (loop repeat len collect (make-random-element-of-type t))))

(defmethod make-random-element-of ((type (eql sequence)))
  (make-random-element-of '(or list vector)))

;;;;

(defun make-random-vector (length &key simple (element-type '*))
  (setq element-type (make-random-array-element-type element-type))
  (make-random-element-of `(,(if simple 'simple-vector 'vector) ,element-type ,length)))

(defun make-random-array (dimensions &key simple (element-type '*))
  (setq element-type (make-random-array-element-type element-type))
  (make-random-element-of `(,(if simple 'simple-array 'array) ,element-type ,length)))

(defun make-random-array-element-type (elememt-type)
  (if (eq element-type '*)
    (rcase
     (1 'bit)
     (1 `(unsigned-byte (1+ (random *maximum-random-int-bits*))))
     (1 `(signed-byte (1+ (random *maximum-random-int-bits*))))
     (2 (random-from-seq #(character base-char standard-char)))
     ;; Put float, complex types here also
     (4 t))
    element-type))

;;;;

(defmethod make-random-element-of-compound-type ((type-op (eql or)) (args cons))
  (make-random-element-of (random-from-seq args)))

(defmethod make-random-element-of-compound-type ((type-op (eql and)) (args cons))
  (loop for e = (make-random-element-of (car args))
	repeat 100
	when (or (null (cdr args)) (typep e (cons 'and (cdr args))))
	return x
	finally (error "Cannot generate a random element of ~A"
		       (cons 'and args))))

(defmethod make-random-element-of-compound-type ((type-op (eql integer)) (args t))
  (let ((lo (let ((lo (car args)))
	      (cond
	       ((consp lo) (1+ (car lo)))
	       ((eq lo nil) '*)
	       (t lo))))
	(hi (let ((hi (cadr args)))
	      (cond
	       ((consp hi) (1- (car hi)))
	       ((eq hi nil) '*)
	       (t hi)))))
    (if (eq lo '*)
	(if (eq hi '*)
	    (let ((x (ash 1 (random *maximum-random-int-bits*))))
	      (random-from-interval x (- x)))
	  (random-from-interval (1+ hi)
				(- hi (random (ash 1 *maximum-random-int-bits*)))))
      
      (if (eq hi '*)
	  (random-from-interval (+ lo (random (ash 1 *maximum-random-int-bits*)) 1)
				lo)
	;; May generalize the next case to increase odds
	;; of certain integers (near 0, near endpoints, near
	;; powers of 2...)
	(random-from-interval (1+ hi) lo)))))

(defmethod make-random-element-of-compound-type ((type-op (eql short-float)) (args t))
  (make-random-element-of-float-type type args))

(defmethod make-random-element-of-compound-type ((type-op (eql single-float)) (args t))
  (make-random-element-of-float-type type args))

(defmethod make-random-element-of-compound-type ((type-op (eql double-float)) (args t))
  (make-random-element-of-float-type type args))

(defmethod make-random-element-of-compound-type ((type-op (eql long-float)) (args t))
  (make-random-element-of-float-type type args))

(defun make-random-element-of-float-type (type-op args)
  (let ((lo (car args))
	(hi (cadr args))
	lo= hi=)
    (cond
     ((consp lo) nil)
     ((member lo '(* nil))
      (setq lo (most-negative-float type-op))
      (setq lo= t))
     (t
      (assert (typep lo type-op))
      (setq lo= t)))
    (cond
     ((consp hi) nil)
     ((member hi '(* nil))
      (setq hi (most-positive-float type-op))
      (setq hi= t))
     (t
      (assert (typep hi type-op))
      (setq hi= t)))
    (assert (<= lo hi))
    (assert (or (< lo hi) (and lo= hi=)))
    (let ((limit 100000))
      (cond
       ((or (<= hi 0)
	    (>= lo 0)
	    (and (<= (- limit) hi limit) (<= (- limit) lo limit)))
	(loop for x = (+ (random (- hi lo)) lo)
	      do (when (or lo= (/= x lo)) (return x))))
       (t
	(rcase
	 (1 (random (min hi (float limit hi))))
	 (1 (- (random (min (float limit lo) (- lo)))))))))))

(defmethod make-random-element-of-compound-type ((type-op (eql mod)) (args cons))
  (let ((modulus (car args)))
    (assert (integerp modulus))
    (assert (plusp modulus))
    (make-random-element-of `(integer 0 (,modulus)))))

(defmethod make-random-element-of-compound-type ((type-op (eql unsigned-byte)) (args t))
  (if (null args)
      (make-random-element-of '(integer 0 *))
    (let ((bits (car args)))
      (if (eq bits'*)
	  (make-random-element-of '(integer 0 *))
	(progn
	  (assert (and (integerp bits) (>= bits 1)))
	  (make-random-element-of `(integer 0 ,(1- (ash 1 bits)))))))))

(defmethod make-random-element-of-compound-type ((type-op (eql eql)) (args cons))
  (assert (null (cdr args)))
  (car args))

(defmethod make-random-element-of-compound-type ((type-op (eql member)) (args cons))
  (random-from-seq args))



