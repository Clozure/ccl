;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Contains: Random type prop tests, part 9 (sequences)

(in-package :cl-test)

;;; FIND

(def-type-prop-test find.1 'find
  (list t #'make-random-sequence-type-containing)
  2)

(def-type-prop-test find.2 'find
  (list 'integer #'make-random-sequence-type-containing)
  2)

(def-type-prop-test find.3 'find
  (list 'character #'make-random-sequence-type-containing)
  2)

(def-type-prop-test find.4 'find
  (list t
	#'make-random-sequence-type-containing
	'(eql :start)
	#'(lambda (v s k1) (declare (ignore v k1))
	    (let ((len (length s)))
	      `(integer 0 ,len))))
  4)

(def-type-prop-test find.5 'find
  (list t
	#'make-random-sequence-type-containing
	'(eql :end)
	#'(lambda (v s k1) (declare (ignore v k1))
	    (let ((len (length s)))
	      `(integer 0 ,len))))
  4)

(def-type-prop-test find.6 'find
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
  6)

(def-type-prop-test find.7 'find
  (list 'integer #'(lambda (x) (declare (ignore x))
		     (make-sequence-type
		      (random 10)
		      (random-from-seq #(bit integer float rational real number))))
	'(eql :key)
	'(member 1+ #.#'1+ 1- #.#'1- - #.#'-))
  4)

(def-type-prop-test find.8 'find
  (list 'character
	#'(lambda (x) (declare (ignore x))
	    (make-sequence-type
	     (random 10)
	     (random-from-seq #(character base-char standard-char))))
	'(eql :key)
	'(member char-upcase #.#'char-upcase
		 char-downcase #.#'char-downcase
		 upper-case-p #.#'upper-case-p
		 lower-case-p #.#'lower-case-p
		 both-case-p #.#'both-case-p
		 char-code #.#'char-code
		 char-int #.#'char-int
		 alpha-char-p #.#'alpha-char-p
		 digit-char-p #.#'digit-char-p
		 alphanumericp #.#'alphanumericp))
  4)

(def-type-prop-test find.9 'find
  (list t #'make-random-sequence-type-containing
	'(eql :from-end)
	'(or null t))
  4)

(def-type-prop-test find.10 'find
  (list 'real #'(lambda (x) (make-sequence-type
			     (random 10)
			     (random-from-seq #(bit integer float rational real))))
	'(eql :from-end)
	'(or null t)
	'(member :test :test-not)
	(list 'member '< #'< '> #'> '<= #'<= '>= #'>= '= #'= '/= #'/=
	      'equal #'equal 'eql #'eql))
  6)

;;; FIND-IF

(def-type-prop-test find-if.1 'find-if
  (list
   (append '(member) *cl-safe-predicates*
	   (mapcar 'symbol-function *cl-safe-predicates*))
   'sequence)
  2)

(def-type-prop-test find-if.2 'find-if
  (list
   (let ((char-predicates
	  '(alpha-char-p digit-char-p upper-case-p lower-case-p
	    both-case-p alphanumericp graphic-char-p
	    standard-char-p)))
     (append '(member) char-predicates
	     (mapcar #'symbol-function char-predicates)))
   #'(lambda (x) (declare (ignore x))
       (make-sequence-type (random 10)
			   '(or standard-char base-char character))))
  2)

(def-type-prop-test find-if.3 'find-if
  (list
   (let ((integer-predicates '(zerop plusp minusp evenp oddp)))
     (append '(member) integer-predicates
	     (mapcar #'symbol-function integer-predicates)))
   #'(lambda (x) (declare (ignore x))
       (make-sequence-type (random 10)
			   `(or bit bit bit bit bit bit bit
				,@(loop for x from 2 to 32
					collect `(unsigned-byte ,x))
				,@(loop for x from 2 to 32
					collect `(signed-byte ,x))))))
  2)

(def-type-prop-test find-if.4 'find-if
  (list
   (append '(member) *cl-safe-predicates*
	   (mapcar 'symbol-function *cl-safe-predicates*))
   'sequence
   '(eql :start)
   #'(lambda (f s k1) (declare (ignore f k1))
       (let ((len (length s)))
	 `(integer 0 ,len))))
  4)

(def-type-prop-test find-if.5 'find-if
  (list
   (append '(member) *cl-safe-predicates*
	   (mapcar 'symbol-function *cl-safe-predicates*))
   'sequence
   '(eql :end)
   #'(lambda (f s k1) (declare (ignore f k1))
       (let ((len (length s)))
	 `(integer 0 ,len))))
  4)

(def-type-prop-test find-if.6 'find-if
  (list
   (append '(member) *cl-safe-predicates*
	   (mapcar 'symbol-function *cl-safe-predicates*))
   'sequence
   '(eql :start)
   #'(lambda (f s k1) (declare (ignore f k1))
       (let ((len (length s)))
	 `(integer 0 ,len)))
   '(eql :end)
   #'(lambda (f s k1 start k2)
       (declare (ignore f k1 k2))
       (let ((len (length s)))
	 `(integer ,start ,len))))
  6)

(def-type-prop-test find-if.7 'find-if
  (list
   (let ((integer-predicates '(zerop plusp minusp evenp oddp)))
     (append '(member) integer-predicates
	     (mapcar #'symbol-function integer-predicates)))
   #'(lambda (x) (declare (ignore x))
       (make-sequence-type (random 10)
			   `(or bit bit bit bit bit bit bit
				,@(loop for x from 2 to 32
					collect `(unsigned-byte ,x))
				,@(loop for x from 2 to 32
					collect `(signed-byte ,x)))))
   '(eql :key)
   (list 'member '1+ '1- 'identity '-
	 #'1+ #'1- #'identity #'-))
  4)

(def-type-prop-test find-if.8 'find-if
  (list
   (let ((integer-predicates '(zerop plusp minusp evenp oddp)))
     (append '(member) integer-predicates
	     (mapcar #'symbol-function integer-predicates)))
   #'(lambda (x) (declare (ignore x))
       (make-sequence-type (random 10)
			   `(or bit bit bit bit bit bit bit
				,@(loop for x from 2 to 32
					collect `(unsigned-byte ,x))
				,@(loop for x from 2 to 32
					collect `(signed-byte ,x)))))
   '(eql :from-end)
   '(or null t))
  4)

(def-type-prop-test find-if.9 'find-if
  (list
   (append '(member) *cl-safe-predicates*
	   (mapcar 'symbol-function *cl-safe-predicates*))
   'sequence
   '(eql :start)
   #'(lambda (f s k1) (declare (ignore f k1))
       (let ((len (length s)))
	 `(integer 0 ,len)))
   '(eql :from-end)
   '(or null t))
  6)

(def-type-prop-test find-if.10 'find-if
  (list
   (append '(member) *cl-safe-predicates*
	   (mapcar 'symbol-function *cl-safe-predicates*))
   'sequence
   '(eql :end)
   #'(lambda (f s k1) (declare (ignore f k1))
       (let ((len (length s)))
	 `(integer 0 ,len)))
   '(eql :from-end)
   '(or null t))
  6)

;;; FIND-IF-NOT

(def-type-prop-test find-if-not.1 'find-if-not
  (list
   (append '(member) *cl-safe-predicates*
	   (mapcar 'symbol-function *cl-safe-predicates*))
   'sequence)
  2)

(def-type-prop-test find-if-not.2 'find-if-not
  (list
   (let ((char-predicates
	  '(alpha-char-p digit-char-p upper-case-p lower-case-p
	    both-case-p alphanumericp graphic-char-p
	    standard-char-p)))
     (append '(member) char-predicates
	     (mapcar #'symbol-function char-predicates)))
   #'(lambda (x) (declare (ignore x))
       (make-sequence-type (random 10)
			   '(or standard-char base-char character))))
  2)

(def-type-prop-test find-if-not.3 'find-if-not
  (list
   (let ((integer-predicates '(zerop plusp minusp evenp oddp)))
     (append '(member) integer-predicates
	     (mapcar #'symbol-function integer-predicates)))
   #'(lambda (x) (declare (ignore x))
       (make-sequence-type (random 10)
			   `(or bit bit bit bit bit bit bit
				,@(loop for x from 2 to 32
					collect `(unsigned-byte ,x))
				,@(loop for x from 2 to 32
					collect `(signed-byte ,x))))))
  2)

(def-type-prop-test find-if-not.4 'find-if-not
  (list
   (append '(member) *cl-safe-predicates*
	   (mapcar 'symbol-function *cl-safe-predicates*))
   'sequence
   '(eql :start)
   #'(lambda (f s k1) (declare (ignore f k1))
       (let ((len (length s)))
	 `(integer 0 ,len))))
  4)

(def-type-prop-test find-if-not.5 'find-if-not
  (list
   (append '(member) *cl-safe-predicates*
	   (mapcar 'symbol-function *cl-safe-predicates*))
   'sequence
   '(eql :end)
   #'(lambda (f s k1) (declare (ignore f k1))
       (let ((len (length s)))
	 `(integer 0 ,len))))
  4)

(def-type-prop-test find-if-not.6 'find-if-not
  (list
   (append '(member) *cl-safe-predicates*
	   (mapcar 'symbol-function *cl-safe-predicates*))
   'sequence
   '(eql :start)
   #'(lambda (f s k1) (declare (ignore f k1))
       (let ((len (length s)))
	 `(integer 0 ,len)))
   '(eql :end)
   #'(lambda (f s k1 start k2)
       (declare (ignore f k1 k2))
       (let ((len (length s)))
	 `(integer ,start ,len))))
  6)

(def-type-prop-test find-if-not.7 'find-if-not
  (list
   (let ((integer-predicates '(zerop plusp minusp evenp oddp)))
     (append '(member) integer-predicates
	     (mapcar #'symbol-function integer-predicates)))
   #'(lambda (x) (declare (ignore x))
       (make-sequence-type (random 10)
			   `(or bit bit bit bit bit bit bit
				,@(loop for x from 2 to 32
					collect `(unsigned-byte ,x))
				,@(loop for x from 2 to 32
					collect `(signed-byte ,x)))))
   '(eql :key)
   (list 'member '1+ '1- 'identity '-
	 #'1+ #'1- #'identity #'-))
  4)

(def-type-prop-test find-if-not.8 'find-if-not
  (list
   (let ((integer-predicates '(zerop plusp minusp evenp oddp)))
     (append '(member) integer-predicates
	     (mapcar #'symbol-function integer-predicates)))
   #'(lambda (x) (declare (ignore x))
       (make-sequence-type (random 10)
			   `(or bit bit bit bit bit bit bit
				,@(loop for x from 2 to 32
					collect `(unsigned-byte ,x))
				,@(loop for x from 2 to 32
					collect `(signed-byte ,x)))))
   '(eql :from-end)
   '(or null t))
  4)

(def-type-prop-test find-if-not.9 'find-if-not
  (list
   (append '(member) *cl-safe-predicates*
	   (mapcar 'symbol-function *cl-safe-predicates*))
   'sequence
   '(eql :start)
   #'(lambda (f s k1) (declare (ignore f k1))
       (let ((len (length s)))
	 `(integer 0 ,len)))
   '(eql :from-end)
   '(or null t))
  6)

(def-type-prop-test find-if-not.10 'find-if-not
  (list
   (append '(member) *cl-safe-predicates*
	   (mapcar 'symbol-function *cl-safe-predicates*))
   'sequence
   '(eql :end)
   #'(lambda (f s k1) (declare (ignore f k1))
       (let ((len (length s)))
	 `(integer 0 ,len)))
   '(eql :from-end)
   '(or null t))
  6)

;;; POSITION

(def-type-prop-test position.1 'position
  (list t #'make-random-sequence-type-containing)
  2)

(def-type-prop-test position.2 'position
  (list 'integer #'make-random-sequence-type-containing)
  2)

(def-type-prop-test position.3 'position
  (list 'character #'make-random-sequence-type-containing)
  2)

(def-type-prop-test position.4 'position
  (list t
	#'make-random-sequence-type-containing
	'(eql :start)
	#'(lambda (v s k1) (declare (ignore v k1))
	    (let ((len (length s)))
	      `(integer 0 ,len))))
  4)

(def-type-prop-test position.5 'position
  (list t
	#'make-random-sequence-type-containing
	'(eql :end)
	#'(lambda (v s k1) (declare (ignore v k1))
	    (let ((len (length s)))
	      `(integer 0 ,len))))
  4)

(def-type-prop-test position.6 'position
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
  6)

(def-type-prop-test position.7 'position
  (list 'integer #'(lambda (x) (declare (ignore x))
		     (make-sequence-type
		      (random 10)
		      (random-from-seq #(bit integer float rational real number))))
	'(eql :key)
	'(member 1+ #.#'1+ 1- #.#'1- - #.#'-))
  4)

(def-type-prop-test position.8 'position
  (list 'character
	#'(lambda (x) (declare (ignore x))
	    (make-sequence-type
	     (random 10)
	     (random-from-seq #(character base-char standard-char))))
	'(eql :key)
	'(member char-upcase #.#'char-upcase
		 char-downcase #.#'char-downcase
		 upper-case-p #.#'upper-case-p
		 lower-case-p #.#'lower-case-p
		 both-case-p #.#'both-case-p
		 char-code #.#'char-code
		 char-int #.#'char-int
		 alpha-char-p #.#'alpha-char-p
		 digit-char-p #.#'digit-char-p
		 alphanumericp #.#'alphanumericp))
  4)

(def-type-prop-test position.9 'position
  (list t #'make-random-sequence-type-containing
	'(eql :from-end)
	'(or null t))
  4)

(def-type-prop-test position.10 'position
  (list 'real #'(lambda (x) (make-sequence-type
			     (random 10)
			     (random-from-seq #(bit integer float rational real))))
	'(eql :from-end)
	'(or null t)
	'(member :test :test-not)
	(list 'member '< #'< '> #'> '<= #'<= '>= #'>= '= #'= '/= #'/=
	      'equal #'equal 'eql #'eql))
  6)

;;; POSITION-IF

(def-type-prop-test position-if.1 'position-if
  (list
   (append '(member) *cl-safe-predicates*
	   (mapcar 'symbol-function *cl-safe-predicates*))
   'sequence)
  2)

(def-type-prop-test position-if.2 'position-if
  (list
   (let ((char-predicates
	  '(alpha-char-p digit-char-p upper-case-p lower-case-p
	    both-case-p alphanumericp graphic-char-p
	    standard-char-p)))
     (append '(member) char-predicates
	     (mapcar #'symbol-function char-predicates)))
   #'(lambda (x) (declare (ignore x))
       (make-sequence-type (random 10)
			   '(or standard-char base-char character))))
  2)

(def-type-prop-test position-if.3 'position-if
  (list
   (let ((integer-predicates '(zerop plusp minusp evenp oddp)))
     (append '(member) integer-predicates
	     (mapcar #'symbol-function integer-predicates)))
   #'(lambda (x) (declare (ignore x))
       (make-sequence-type (random 10)
			   `(or bit bit bit bit bit bit bit
				,@(loop for x from 2 to 32
					collect `(unsigned-byte ,x))
				,@(loop for x from 2 to 32
					collect `(signed-byte ,x))))))
  2)

(def-type-prop-test position-if.4 'position-if
  (list
   (append '(member) *cl-safe-predicates*
	   (mapcar 'symbol-function *cl-safe-predicates*))
   'sequence
   '(eql :start)
   #'(lambda (f s k1) (declare (ignore f k1))
       (let ((len (length s)))
	 `(integer 0 ,len))))
  4)

(def-type-prop-test position-if.5 'position-if
  (list
   (append '(member) *cl-safe-predicates*
	   (mapcar 'symbol-function *cl-safe-predicates*))
   'sequence
   '(eql :end)
   #'(lambda (f s k1) (declare (ignore f k1))
       (let ((len (length s)))
	 `(integer 0 ,len))))
  4)

(def-type-prop-test position-if.6 'position-if
  (list
   (append '(member) *cl-safe-predicates*
	   (mapcar 'symbol-function *cl-safe-predicates*))
   'sequence
   '(eql :start)
   #'(lambda (f s k1) (declare (ignore f k1))
       (let ((len (length s)))
	 `(integer 0 ,len)))
   '(eql :end)
   #'(lambda (f s k1 start k2)
       (declare (ignore f k1 k2))
       (let ((len (length s)))
	 `(integer ,start ,len))))
  6)

(def-type-prop-test position-if.7 'position-if
  (list
   (let ((integer-predicates '(zerop plusp minusp evenp oddp)))
     (append '(member) integer-predicates
	     (mapcar #'symbol-function integer-predicates)))
   #'(lambda (x) (declare (ignore x))
       (make-sequence-type (random 10)
			   `(or bit bit bit bit bit bit bit
				,@(loop for x from 2 to 32
					collect `(unsigned-byte ,x))
				,@(loop for x from 2 to 32
					collect `(signed-byte ,x)))))
   '(eql :key)
   (list 'member '1+ '1- 'identity '-
	 #'1+ #'1- #'identity #'-))
  4)

(def-type-prop-test position-if.8 'position-if
  (list
   (let ((integer-predicates '(zerop plusp minusp evenp oddp)))
     (append '(member) integer-predicates
	     (mapcar #'symbol-function integer-predicates)))
   #'(lambda (x) (declare (ignore x))
       (make-sequence-type (random 10)
			   `(or bit bit bit bit bit bit bit
				,@(loop for x from 2 to 32
					collect `(unsigned-byte ,x))
				,@(loop for x from 2 to 32
					collect `(signed-byte ,x)))))
   '(eql :from-end)
   '(or null t))
  4)

(def-type-prop-test position-if.9 'position-if
  (list
   (append '(member) *cl-safe-predicates*
	   (mapcar 'symbol-function *cl-safe-predicates*))
   'sequence
   '(eql :start)
   #'(lambda (f s k1) (declare (ignore f k1))
       (let ((len (length s)))
	 `(integer 0 ,len)))
   '(eql :from-end)
   '(or null t))
  6)

(def-type-prop-test position-if.10 'position-if
  (list
   (append '(member) *cl-safe-predicates*
	   (mapcar 'symbol-function *cl-safe-predicates*))
   'sequence
   '(eql :end)
   #'(lambda (f s k1) (declare (ignore f k1))
       (let ((len (length s)))
	 `(integer 0 ,len)))
   '(eql :from-end)
   '(or null t))
  6)

;;; POSITION-IF-NOT

(def-type-prop-test position-if-not.1 'position-if-not
  (list
   (append '(member) *cl-safe-predicates*
	   (mapcar 'symbol-function *cl-safe-predicates*))
   'sequence)
  2)

(def-type-prop-test position-if-not.2 'position-if-not
  (list
   (let ((char-predicates
	  '(alpha-char-p digit-char-p upper-case-p lower-case-p
	    both-case-p alphanumericp graphic-char-p
	    standard-char-p)))
     (append '(member) char-predicates
	     (mapcar #'symbol-function char-predicates)))
   #'(lambda (x) (declare (ignore x))
       (make-sequence-type (random 10)
			   '(or standard-char base-char character))))
  2)

(def-type-prop-test position-if-not.3 'position-if-not
  (list
   (let ((integer-predicates '(zerop plusp minusp evenp oddp)))
     (append '(member) integer-predicates
	     (mapcar #'symbol-function integer-predicates)))
   #'(lambda (x) (declare (ignore x))
       (make-sequence-type (random 10)
			   `(or bit bit bit bit bit bit bit
				,@(loop for x from 2 to 32
					collect `(unsigned-byte ,x))
				,@(loop for x from 2 to 32
					collect `(signed-byte ,x))))))
  2)

(def-type-prop-test position-if-not.4 'position-if-not
  (list
   (append '(member) *cl-safe-predicates*
	   (mapcar 'symbol-function *cl-safe-predicates*))
   'sequence
   '(eql :start)
   #'(lambda (f s k1) (declare (ignore f k1))
       (let ((len (length s)))
	 `(integer 0 ,len))))
  4)

(def-type-prop-test position-if-not.5 'position-if-not
  (list
   (append '(member) *cl-safe-predicates*
	   (mapcar 'symbol-function *cl-safe-predicates*))
   'sequence
   '(eql :end)
   #'(lambda (f s k1) (declare (ignore f k1))
       (let ((len (length s)))
	 `(integer 0 ,len))))
  4)

(def-type-prop-test position-if-not.6 'position-if-not
  (list
   (append '(member) *cl-safe-predicates*
	   (mapcar 'symbol-function *cl-safe-predicates*))
   'sequence
   '(eql :start)
   #'(lambda (f s k1) (declare (ignore f k1))
       (let ((len (length s)))
	 `(integer 0 ,len)))
   '(eql :end)
   #'(lambda (f s k1 start k2)
       (declare (ignore f k1 k2))
       (let ((len (length s)))
	 `(integer ,start ,len))))
  6)

(def-type-prop-test position-if-not.7 'position-if-not
  (list
   (let ((integer-predicates '(zerop plusp minusp evenp oddp)))
     (append '(member) integer-predicates
	     (mapcar #'symbol-function integer-predicates)))
   #'(lambda (x) (declare (ignore x))
       (make-sequence-type (random 10)
			   `(or bit bit bit bit bit bit bit
				,@(loop for x from 2 to 32
					collect `(unsigned-byte ,x))
				,@(loop for x from 2 to 32
					collect `(signed-byte ,x)))))
   '(eql :key)
   (list 'member '1+ '1- 'identity '-
	 #'1+ #'1- #'identity #'-))
  4)

(def-type-prop-test position-if-not.8 'position-if-not
  (list
   (let ((integer-predicates '(zerop plusp minusp evenp oddp)))
     (append '(member) integer-predicates
	     (mapcar #'symbol-function integer-predicates)))
   #'(lambda (x) (declare (ignore x))
       (make-sequence-type (random 10)
			   `(or bit bit bit bit bit bit bit
				,@(loop for x from 2 to 32
					collect `(unsigned-byte ,x))
				,@(loop for x from 2 to 32
					collect `(signed-byte ,x)))))
   '(eql :from-end)
   '(or null t))
  4)

(def-type-prop-test position-if-not.9 'position-if-not
  (list
   (append '(member) *cl-safe-predicates*
	   (mapcar 'symbol-function *cl-safe-predicates*))
   'sequence
   '(eql :start)
   #'(lambda (f s k1) (declare (ignore f k1))
       (let ((len (length s)))
	 `(integer 0 ,len)))
   '(eql :from-end)
   '(or null t))
  6)

(def-type-prop-test position-if-not.10 'position-if-not
  (list
   (append '(member) *cl-safe-predicates*
	   (mapcar 'symbol-function *cl-safe-predicates*))
   'sequence
   '(eql :end)
   #'(lambda (f s k1) (declare (ignore f k1))
       (let ((len (length s)))
	 `(integer 0 ,len)))
   '(eql :from-end)
   '(or null t))
  6)
