;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Mar 13 15:33:55 2005
;;;; Contains: Random type prop tests, part 6 (arrays)

(in-package :cl-test)

(def-type-prop-test adjustable-array-p 'adjustable-array-p '(array) 1)

(def-type-prop-test aref.0 'aref '((array * nil)) 1)
(def-type-prop-test aref.1 'aref (list '(array * (*)) (index-type-for-dim 0)) 2)
(def-type-prop-test aref.2 'aref (list '(array * (* *)) (index-type-for-dim 0) (index-type-for-dim 1)) 3)
(def-type-prop-test aref.3 'aref (list '(array * (* * *))
				       (index-type-for-dim 0) (index-type-for-dim 1) (index-type-for-dim 2))
  4)

(def-type-prop-test array-dimension 'array-dimension
  (list 'array #'(lambda (x) (let ((r (array-rank x))) (and (> r 0) `(integer 0 (,r))))))
  2)
(def-type-prop-test array-dimensions 'array-dimensions '(array) 1)
(def-type-prop-test array-element-type 'array-element-type '(array) 1)
(def-type-prop-test array-has-fill-pointer-p.1 'array-has-fill-pointer-p '(array) 1)
(def-type-prop-test array-has-fill-pointer-p.2 'array-has-fill-pointer-p '(vector) 1)
(def-type-prop-test array-displacement.1 'array-displacement '(array) 1)
(def-type-prop-test array-displacement.2 'array-displacement '(vector) 1)

(def-type-prop-test array-in-bounds-p.0 'array-in-bounds-p '((array * nil)) 1)
(def-type-prop-test array-in-bounds-p.1 'array-in-bounds-p (list '(array * (*)) (index-type-for-dim 0)) 2)
(def-type-prop-test array-in-bounds-p.2 'array-in-bounds-p
  (list '(array * (* *)) (index-type-for-dim 0) (index-type-for-dim 1)) 3)
(def-type-prop-test array-in-bounds-p.3 'array-in-bounds-p
  (list '(array * (* * *)) (index-type-for-dim 0) (index-type-for-dim 1) (index-type-for-dim 2))
  4)
(def-type-prop-test array-in-bounds-p.4 'array-in-bounds-p '((array * (*)) integer) 2)
(def-type-prop-test array-in-bounds-p.5 'array-in-bounds-p '((array * (* *)) integer integer) 3)
(def-type-prop-test array-in-bounds-p.6 'array-in-bounds-p '((array * (* * *)) integer integer integer) 4)
(def-type-prop-test array-rank 'array-rank '(array) 1)

(def-type-prop-test array-row-major-index.0 'array-row-major-index '((array * nil)) 1)
(def-type-prop-test array-row-major-index.1 'array-row-major-index (list '(array * (*)) (index-type-for-dim 0)) 2)
(def-type-prop-test array-row-major-index.2 'array-row-major-index
  (list '(array * (* *)) (index-type-for-dim 0) (index-type-for-dim 1)) 3)
(def-type-prop-test array-row-major-index.3 'array-row-major-index
  (list '(array * (* * *)) (index-type-for-dim 0) (index-type-for-dim 1) (index-type-for-dim 2))
  4)
(def-type-prop-test array-total-size 'array-total-size '(array) 1)

(def-type-prop-test arrayp 'arrayp '(t) 1)

(def-type-prop-test fill-pointer '(lambda (x) (and (array-has-fill-pointer-p x) (fill-pointer x))) '(vector) 1)

(def-type-prop-test row-major-aref 'row-major-aref
  (list 'array #'(lambda (a) (let ((s (array-total-size a))) (and (> s 0) `(integer 0 (,s))))))
  2)

(def-type-prop-test upgraded-array-element-type 'upgraded-array-element-type
  (list #'(lambda () (let ((x (make-random-element-of-type t)))
		       `(eql ,(make-random-type-containing x)))))
  1)

(def-type-prop-test simple-vector-p.1 'simple-vector-p '(t) 1)
(def-type-prop-test simple-vector-p.2 'simple-vector-p '(vector) 1)

(def-type-prop-test svref 'svref (list 'simple-vector (index-type-for-dim 0)) 2)
(def-type-prop-test vector 'vector nil 1 :rest-type t :maxargs 10)
(def-type-prop-test vectorp.1 'vectorp '(t) 1)
(def-type-prop-test vectorp.2 'vectorp '(array) 1)

(def-type-prop-test bit.1 'bit (list '(array bit (*)) (index-type-for-dim 0)) 2)
(def-type-prop-test bit.2 'bit (list '(array bit (* *)) (index-type-for-dim 0) (index-type-for-dim 1)) 3)
(def-type-prop-test bit.3 'bit
  (list '(array bit (* * *)) (index-type-for-dim 0) (index-type-for-dim 1) (index-type-for-dim 2))
  4)

(def-type-prop-test sbit.1 'sbit (list '(simple-array bit (*)) (index-type-for-dim 0)) 2)
(def-type-prop-test sbit.2 'sbit (list '(simple-array bit (* *)) (index-type-for-dim 0) (index-type-for-dim 1)) 3)
(def-type-prop-test sbit.3 'sbit
  (list '(simple-array bit (* * *)) (index-type-for-dim 0) (index-type-for-dim 1) (index-type-for-dim 2))
  4)


(def-type-prop-test bit-and.1 'bit-and (list '(array bit) #'(lambda (a) (let ((dims (array-dimensions a)))
									  `(array bit ,dims))))
  2)
(def-type-prop-test bit-and.2 'bit-and
  (list '(array bit) #'(lambda (a) (let ((dims (array-dimensions a))) `(array bit ,dims))) 'null)
  3)
(def-type-prop-test bit-andc1.1 'bit-andc1 (list '(array bit) #'(lambda (a) (let ((dims (array-dimensions a)))
									  `(array bit ,dims))))
  2)
(def-type-prop-test bit-andc1.2 'bit-andc1
  (list '(array bit) #'(lambda (a) (let ((dims (array-dimensions a))) `(array bit ,dims))) 'null)
  3)
(def-type-prop-test bit-andc2.1 'bit-andc2 (list '(array bit) #'(lambda (a) (let ((dims (array-dimensions a)))
									  `(array bit ,dims))))
  2)
(def-type-prop-test bit-andc2.2 'bit-andc2
  (list '(array bit) #'(lambda (a) (let ((dims (array-dimensions a))) `(array bit ,dims))) 'null)
  3)
(def-type-prop-test bit-ior.1 'bit-ior (list '(array bit) #'(lambda (a) (let ((dims (array-dimensions a)))
									  `(array bit ,dims))))
  2)
(def-type-prop-test bit-ior.2 'bit-ior
  (list '(array bit) #'(lambda (a) (let ((dims (array-dimensions a))) `(array bit ,dims))) 'null)
  3)
(def-type-prop-test bit-orc1.1 'bit-orc1 (list '(array bit) #'(lambda (a) (let ((dims (array-dimensions a)))
									  `(array bit ,dims))))
  2)
(def-type-prop-test bit-orc1.2 'bit-orc1
  (list '(array bit) #'(lambda (a) (let ((dims (array-dimensions a))) `(array bit ,dims))) 'null)
  3)
(def-type-prop-test bit-orc2.1 'bit-orc2 (list '(array bit) #'(lambda (a) (let ((dims (array-dimensions a)))
									  `(array bit ,dims))))
  2)
(def-type-prop-test bit-orc2.2 'bit-orc2
  (list '(array bit) #'(lambda (a) (let ((dims (array-dimensions a))) `(array bit ,dims))) 'null)
  3)
(def-type-prop-test bit-eqv.1 'bit-eqv (list '(array bit) #'(lambda (a) (let ((dims (array-dimensions a)))
									  `(array bit ,dims))))
  2)
(def-type-prop-test bit-eqv.2 'bit-eqv
  (list '(array bit) #'(lambda (a) (let ((dims (array-dimensions a))) `(array bit ,dims))) 'null)
  3)
(def-type-prop-test bit-xor.1 'bit-xor (list '(array bit) #'(lambda (a) (let ((dims (array-dimensions a)))
									  `(array bit ,dims))))
  2)
(def-type-prop-test bit-xor.2 'bit-xor
  (list '(array bit) #'(lambda (a) (let ((dims (array-dimensions a))) `(array bit ,dims))) 'null)
  3)
(def-type-prop-test bit-nand.1 'bit-nand (list '(array bit) #'(lambda (a) (let ((dims (array-dimensions a)))
									  `(array bit ,dims))))
  2)
(def-type-prop-test bit-nand.2 'bit-nand
  (list '(array bit) #'(lambda (a) (let ((dims (array-dimensions a))) `(array bit ,dims))) 'null)
  3)
(def-type-prop-test bit-nor.1 'bit-nor (list '(array bit) #'(lambda (a) (let ((dims (array-dimensions a)))
									  `(array bit ,dims))))
  2)
(def-type-prop-test bit-nor.2 'bit-nor
  (list '(array bit) #'(lambda (a) (let ((dims (array-dimensions a))) `(array bit ,dims))) 'null)
  3)
(def-type-prop-test bit-not.1 'bit-not '((array bit)) 1)
(def-type-prop-test bit-not.2 'bit-not '((array bit) null) 2)

(def-type-prop-test bit-vector-p 'bit-vector-p '(t) 1)
(def-type-prop-test simple-bit-vector-p 'simple-bit-vector-p '(t) 1)
