;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Mar 13 18:31:57 2005
;;;; Contains: Random type prop tests, part 8 (sequences)

(in-package :cl-test)

(def-type-prop-test copy-seq 'copy-seq '((or vector list)) 1)

(def-type-prop-test elt 'elt (list '(or vector list)
				   #'(lambda (x) (let ((len (length x)))
						   (and (> len 0) `(integer 0 (,len))))))
  2)

(defmacro rfill (x y &rest other-args)
  `(fill ,y ,x ,@other-args))

(def-type-prop-test fill.1 'rfill
  (list t #'make-random-sequence-type-containing)
  2 :replicate '(nil t))

(def-type-prop-test fill.2 'rfill
  (list 'integer #'make-random-sequence-type-containing)
  2 :replicate '(nil t))

(def-type-prop-test fill.3 'rfill
  (list 'character #'make-random-sequence-type-containing)
  2 :replicate '(nil t))

(def-type-prop-test fill.4 'rfill
  (list t #'make-random-sequence-type-containing
	'(eql :start)
	#'(lambda (v s k1) (declare (ignore v k1))
	    (let ((len (length s)))
	      `(integer 0 ,len))))
  4 :replicate '(nil t nil nil))

(def-type-prop-test fill.5 'rfill
  (list t #'make-random-sequence-type-containing
	'(eql :end)
	#'(lambda (v s k1) (declare (ignore v k1))
	    (let ((len (length s)))
	      `(integer 0 ,len))))
  4 :replicate '(nil t nil nil))

(def-type-prop-test fill.6 'rfill
  (list t #'make-random-sequence-type-containing
	'(eql :start)
	#'(lambda (v s k1) (declare (ignore v k1))
	    (let ((len (length s)))
	      `(integer 0 ,len)))
	'(eql :end)
	#'(lambda (v s k1 start k2)
	    (declare (ignore v k1 k2))
	    (let ((len (length s)))
	      `(integer ,start ,len))))
  6 :replicate '(nil t nil nil nil nil))

;;; make-sequence tests here

(def-type-prop-test subseq.1 'subseq
  (list 'sequence #'(lambda (s) `(integer 0 ,(length s))))
  2)

(def-type-prop-test subseq.2 'subseq
  (list 'sequence #'(lambda (s) `(integer 0 ,(length s)))
	#'(lambda (s start) `(integer ,start ,(length s))))
  3)

;;; map tests here

(def-type-prop-test map.1 'map
  (list '(member list vector)
	'(member list #.#'list)
	'(or list vector))
  3)

(def-type-prop-test map.2 'map
  (list '(member list vector)
	'(member list #.#'list)
	'(or list vector)
	'(or list vector))
  4)

(def-type-prop-test map.3 'map
  (list '(member list vector)
	'(member list #.#'list)
	'(or list vector)
	'(or list vector)
	'(or list vector))
  5)

(def-type-prop-test map.4 'map
  (list '(member list vector (vector (unsigned-byte 32)))
	'(member 1+ #.#'1+)
	`(or ,@(loop for i from 1 to 31 collect `(vector (unsigned-byte ,i)))))
  3)

(def-type-prop-test map.5 'map
  (list `(member ,@(loop for i from 1 to 32 collect `(vector (unsigned-byte ,i))))
	'(member 1+ #.#'1+)
	#'(lambda (type fun)
	    (declare (ignore fun))
	    (let ((i (cadadr type)))
	      `(or ,@(loop for j from i to 32 collect `(vector (integer 0 ,(- (ash 1 i) 2))))))))
  3)



;;; map-into tests here

(def-type-prop-test map-into.1 'map-into
  (list '(or list (vector t))
	'(member list #.#'list)
	'(or list vector))
  3 :replicate '(t nil nil))

(def-type-prop-test map-into.2 'map-into
  (list '(or list (vector t))
	'(member list #.#'list)
	'(or list vector)
	'(or list vector))
  4 :replicate '(t nil nil nil))

;;; reduce tests here

(def-type-prop-test count.1 'count '(t sequence) 2)
(def-type-prop-test count.2 'count
  (list t #'make-random-sequence-type-containing)
  2)
(def-type-prop-test count.3 'count
  (list t #'make-random-sequence-type-containing
	'(eql :start)
	#'(lambda (x s k1) (declare (ignore x k1))
	    `(integer 0 ,(length s))))
  4)
(def-type-prop-test count.4 'count
  (list t #'make-random-sequence-type-containing
	'(eql :end)
	#'(lambda (x s k1) (declare (ignore x k1))
	    `(integer 0 ,(length s))))
  4)
(def-type-prop-test count.5 'count
  (list t #'make-random-sequence-type-containing
	'(eql :start)
	#'(lambda (x s k1) (declare (ignore x k1))
	    `(integer 0 ,(length s)))
	'(eql :end)
	#'(lambda (x s k1 start k2) (declare (ignore x k1 k2))
	    `(integer ,start ,(length s))))
  6)

(def-type-prop-test count.6 'count
  (list '(or short-float single-float double-float long-float)
	#'(lambda (f) `(vector (or ,(typecase f
				      (short-float 'short-float)
				      (single-float 'single-float)
				      (double-float 'double-float)
				      (long-float 'long-float)
				      (t 'float))
				   (eql ,f)))))
  2)

(def-type-prop-test count.7 'count '(bit (vector bit)) 2)
(def-type-prop-test count.8 'count '((unsigned-byte 2) (vector (unsigned-byte 2))) 2)
(def-type-prop-test count.9 'count '((unsigned-byte 4) (vector (unsigned-byte 4))) 2)
(def-type-prop-test count.10 'count '((unsigned-byte 8) (vector (unsigned-byte 8))) 2)
  

;;; count-if tests

(def-type-prop-test count-if.1 'count-if
  (list (let ((funs '(numberp rationalp realp floatp complexp
		      symbolp identity null functionp listp consp
                      arrayp vectorp simple-vector-p
		      stringp simple-string-p
		      bit-vector-p simple-bit-vector-p)))
	  `(member ,@funs ,@(mapcar #'symbol-function funs)))
	'(or list vector))
  2)

(def-type-prop-test count-if.2 'count-if
  (list (let ((funs '(numberp rationalp realp floatp complexp
		      symbolp identity null functionp listp consp
                      arrayp vectorp simple-vector-p
		      stringp simple-string-p
		      bit-vector-p simple-bit-vector-p)))
	  `(member ,@funs ,@(mapcar #'symbol-function funs)))
	'(or list vector)
	'(eql :key)
	(let ((key-funs '(identity not null))) 
	  `(member ,@key-funs ,@(mapcar #'symbol-function key-funs))))
  4)


;;; Put count-if-not tests here
	

(def-type-prop-test length.1 'length '(sequence) 1)

(def-type-prop-test reverse.1 'reverse '(sequence) 1)
(def-type-prop-test nreverse.1 'nreverse '(sequence) 1 :replicate '(t))

(def-type-prop-test sort.1 'sort
  `((vector bit) (member < <= > >= ,#'< ,#'<= ,#'> ,#'>=)) 2
  :replicate '(t nil))

(def-type-prop-test sort.2 'sort
  `((or (vector (unsigned-byte 2))
	(vector (unsigned-byte 3))
	(vector (unsigned-byte 4))
	(vector (unsigned-byte 5))
	(vector (unsigned-byte 6))
	(vector (unsigned-byte 7))
	(vector (unsigned-byte 8)))
    (member < <= > >= ,#'< ,#'<= ,#'> ,#'>=))
  2 :replicate '(t nil))
    
(def-type-prop-test sort.3 'sort
  `((or (vector (unsigned-byte 10))
	(vector (unsigned-byte 13))
	(vector (unsigned-byte 15))
	(vector (unsigned-byte 16)))
    (member < <= > >= ,#'< ,#'<= ,#'> ,#'>=))
  2 :replicate '(t nil))

(def-type-prop-test sort.4 'sort
  `((or (vector (unsigned-byte 20))
	(vector (unsigned-byte 24))
	(vector (unsigned-byte 28))
	(vector (unsigned-byte 31))
	(vector (unsigned-byte 32)))
    (member < <= > >= ,#'< ,#'<= ,#'> ,#'>=))
  2 :replicate '(t nil))

(def-type-prop-test sort.5 'sort
  `((or (vector (signed-byte 2))
	(vector (signed-byte 3))
	(vector (signed-byte 4))
	(vector (signed-byte 5))
	(vector (signed-byte 6))
	(vector (signed-byte 7))
	(vector (signed-byte 8)))
    (member < <= > >= ,#'< ,#'<= ,#'> ,#'>=))
  2 :replicate '(t nil))

(def-type-prop-test sort.6 'sort
  `((or (vector (signed-byte 10))
	(vector (signed-byte 13))
	(vector (signed-byte 15))
	(vector (signed-byte 16)))
    (member < <= > >= ,#'< ,#'<= ,#'> ,#'>=))
  2 :replicate '(t nil))

(def-type-prop-test sort.7 'sort
  `((or (vector (signed-byte 20))
	(vector (signed-byte 24))
	(vector (signed-byte 28))
	(vector (signed-byte 31))
	(vector (signed-byte 32)))
    (member < <= > >= ,#'< ,#'<= ,#'> ,#'>=))
  2 :replicate '(t nil))

(def-type-prop-test sort.8 'sort
  `((or (vector short-float)
	(vector single-float)
	(vector double-float)
	(vector long-float))
    (member < <= > >= ,#'< ,#'<= ,#'> ,#'>=))
  2 :replicate '(t nil))

;;; Stable sort

(def-type-prop-test stable-sort.1 'stable-sort
  `((vector bit) (member < <= > >= ,#'< ,#'<= ,#'> ,#'>=)) 2
  :replicate '(t nil))

(def-type-prop-test stable-sort.2 'stable-sort
  `((or (vector (unsigned-byte 2))
	(vector (unsigned-byte 3))
	(vector (unsigned-byte 4))
	(vector (unsigned-byte 5))
	(vector (unsigned-byte 6))
	(vector (unsigned-byte 7))
	(vector (unsigned-byte 8)))
    (member < <= > >= ,#'< ,#'<= ,#'> ,#'>=))
  2 :replicate '(t nil))
    
(def-type-prop-test stable-sort.3 'stable-sort
  `((or (vector (unsigned-byte 10))
	(vector (unsigned-byte 13))
	(vector (unsigned-byte 15))
	(vector (unsigned-byte 16)))
    (member < <= > >= ,#'< ,#'<= ,#'> ,#'>=))
  2 :replicate '(t nil))

(def-type-prop-test stable-sort.4 'stable-sort
  `((or (vector (unsigned-byte 20))
	(vector (unsigned-byte 24))
	(vector (unsigned-byte 28))
	(vector (unsigned-byte 31))
	(vector (unsigned-byte 32)))
    (member < <= > >= ,#'< ,#'<= ,#'> ,#'>=))
  2 :replicate '(t nil))

(def-type-prop-test stable-sort.5 'stable-sort
  `((or (vector (signed-byte 2))
	(vector (signed-byte 3))
	(vector (signed-byte 4))
	(vector (signed-byte 5))
	(vector (signed-byte 6))
	(vector (signed-byte 7))
	(vector (signed-byte 8)))
    (member < <= > >= ,#'< ,#'<= ,#'> ,#'>=))
  2 :replicate '(t nil))

(def-type-prop-test stable-sort.6 'stable-sort
  `((or (vector (signed-byte 10))
	(vector (signed-byte 13))
	(vector (signed-byte 15))
	(vector (signed-byte 16)))
    (member < <= > >= ,#'< ,#'<= ,#'> ,#'>=))
  2 :replicate '(t nil))

(def-type-prop-test stable-sort.7 'stable-sort
  `((or (vector (signed-byte 20))
	(vector (signed-byte 24))
	(vector (signed-byte 28))
	(vector (signed-byte 31))
	(vector (signed-byte 32)))
    (member < <= > >= ,#'< ,#'<= ,#'> ,#'>=))
  2 :replicate '(t nil))

(def-type-prop-test stable-sort.8 'stable-sort
  `((or (vector short-float)
	(vector single-float)
	(vector double-float)
	(vector long-float))
    (member < <= > >= ,#'< ,#'<= ,#'> ,#'>=))
  2 :replicate '(t nil))

(def-type-prop-test stable-sort.9 'stable-sort
  `((vector (cons (integer 0 4) (eql nil)))
    (member < <= > >= ,#'< ,#'<= ,#'> ,#'>=)
    (eql :key)
    (member car ,#'car))
  4 :replicate '(t nil nil nil)
  :test #'equalp-and-eql-elements)
