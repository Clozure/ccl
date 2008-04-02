;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Mar  6 20:37:57 2005
;;;; Contains: Tests that invoke the random type prop infrastructure, part 2

(in-package :cl-test)

(def-type-prop-test =.1 '= '(number number) 2)
(def-type-prop-test =.2 '= '(number number number) 3)
(def-type-prop-test =.3 '= nil 4 :maxargs 10 :rest-type 'number)
(def-type-prop-test =.4 '= '(integer integer) 2)
(def-type-prop-test =.5 '= (list 'number #'(lambda (x) (if (coin) 'number
							`(eql ,x)))) 2)
(def-type-prop-test =.6 '= (list 'number 'number
				 #'(lambda (x y) (rcase
						  (2 'number)
						  (1 `(eql ,x))
						  (1 `(eql ,y)))))
  3)
(def-type-prop-test /=.1 '/= '(number number) 2)
(def-type-prop-test /=.2 '/= '(number number number) 3)
(def-type-prop-test /=.3 '/= nil 4 :maxargs 10 :rest-type 'number)
(def-type-prop-test /=.4 '/= '(integer integer) 2)
(def-type-prop-test /=.5 '/= (list 'number #'(lambda (x) (if (coin) 'number
							   `(eql ,x)))) 2)
(def-type-prop-test /=.6 '/= (list 'number 'number
				   #'(lambda (x y) (rcase
						    (2 'number)
						    (1 `(eql ,x))
						    (1 `(eql ,y)))))
  3)
(def-type-prop-test <.1 '< '(real real) 2)
(def-type-prop-test <.2 '< '(real real real) 3)
(def-type-prop-test <.3 '< nil 4 :maxargs 10 :rest-type 'real)
(def-type-prop-test <.4 '< '(integer integer) 2)
(def-type-prop-test <.5 '< (list 'real #'(lambda (x) (if (coin) 'real
                                                        `(eql ,x)))) 2)
(def-type-prop-test <.6 '< (list 'real 'real
                                 #'(lambda (x y) (rcase
                                                  (2 'real)
                                                  (1 `(eql ,x))
                                                  (1 `(eql ,y)))))
  3)
(def-type-prop-test >.1 '> '(real real) 2)
(def-type-prop-test >.2 '> '(real real real) 3)
(def-type-prop-test >.3 '> nil 4 :maxargs 10 :rest-type 'real)
(def-type-prop-test >.4 '> '(integer integer) 2)
(def-type-prop-test >.5 '> (list 'real #'(lambda (x) (if (coin) 'real
                                                        `(eql ,x)))) 2)
(def-type-prop-test >.6 '> (list 'real 'real
                                 #'(lambda (x y) (rcase
                                                  (2 'real)
                                                  (1 `(eql ,x))
                                                  (1 `(eql ,y)))))
  3)
(def-type-prop-test <=.1 '<= '(real real) 2)
(def-type-prop-test <=.2 '<= '(real real real) 3)
(def-type-prop-test <=.3 '<= nil 4 :maxargs 10 :rest-type 'real)
(def-type-prop-test <=.4 '<= '(integer integer) 2)
(def-type-prop-test <=.5 '<= (list 'real #'(lambda (x) (if (coin) 'real
							 `(eql ,x)))) 2)
(def-type-prop-test <=.6 '<= (list 'real 'real
				   #'(lambda (x y) (rcase
						    (2 'real)
						    (1 `(eql ,x))
						    (1 `(eql ,y)))))
  3)
(def-type-prop-test >=.1 '>= '(real real) 2)
(def-type-prop-test >=.2 '>= '(real real real) 3)
(def-type-prop-test >=.3 '>= nil 4 :maxargs 10 :rest-type 'real)
(def-type-prop-test >=.4 '>= '(integer integer) 2)
(def-type-prop-test >=.5 '>= (list 'real #'(lambda (x) (if (coin) 'real
							 `(eql ,x)))) 2)
(def-type-prop-test >=.6 '>= (list 'real 'real
				   #'(lambda (x y) (rcase
						    (2 'real)
						    (1 `(eql ,x))
						    (1 `(eql ,y)))))
  3)

(def-type-prop-test min.1 'min nil 2 :maxargs 6 :rest-type 'integer)
(def-type-prop-test min.2 'min nil 2 :maxargs 6 :rest-type 'rational)
(def-type-prop-test min.3 'min nil 2 :maxargs 6 :rest-type 'real)
(def-type-prop-test max.1 'max nil 2 :maxargs 6 :rest-type 'integer)
(def-type-prop-test max.2 'max nil 2 :maxargs 6 :rest-type 'rational)
(def-type-prop-test max.3 'max nil 2 :maxargs 6 :rest-type 'real)

(def-type-prop-test minusp 'minusp '(real) 1)
(def-type-prop-test plusp 'plusp '(real) 1)
(def-type-prop-test zerop 'zerop '(number) 1)

(def-type-prop-test floor.1 'floor '(real) 1)
(def-type-prop-test floor.2 'floor '(real (and integer (not (satisfies zerop)))) 2)
(def-type-prop-test floor.3 'floor '(real (and real (not (satisfies zerop)))) 2)
(def-type-prop-test ffloor.1 'ffloor '(real) 1)
(def-type-prop-test ffloor.2 'ffloor '(real (and integer (not (satisfies zerop)))) 2)
(def-type-prop-test ffloor.3 'ffloor '(real (and real (not (satisfies zerop)))) 2)
(def-type-prop-test ceiling.1 'ceiling '(real) 1)
(def-type-prop-test ceiling.2 'ceiling '(real (and integer (not (satisfies zerop)))) 2)
(def-type-prop-test ceiling.3 'ceiling '(real (and real (not (satisfies zerop)))) 2)
(def-type-prop-test fceiling.1 'fceiling '(real) 1)
(def-type-prop-test fceiling.2 'fceiling '(real (and integer (not (satisfies zerop)))) 2)
(def-type-prop-test fceiling.3 'fceiling '(real (and real (not (satisfies zerop)))) 2)
(def-type-prop-test truncate.1 'truncate '(real) 1)
(def-type-prop-test truncate.2 'truncate '(real (and integer (not (satisfies zerop)))) 2)
(def-type-prop-test truncate.3 'truncate '(real (and real (not (satisfies zerop)))) 2)
(def-type-prop-test ftruncate.1 'ftruncate '(real) 1)
(def-type-prop-test ftruncate.2 'ftruncate '(real (and integer (not (satisfies zerop)))) 2)
(def-type-prop-test ftruncate.3 'ftruncate '(real (and real (not (satisfies zerop)))) 2)
(def-type-prop-test round.1 'round '(real) 1)
(def-type-prop-test round.2 'round '(real (and integer (not (satisfies zerop)))) 2)
(def-type-prop-test round.3 'round '(real (and real (not (satisfies zerop)))) 2)
(def-type-prop-test fround.1 'fround '(real) 1)
(def-type-prop-test fround.2 'fround '(real (and integer (not (satisfies zerop)))) 2)
(def-type-prop-test fround.3 'fround '(real (and real (not (satisfies zerop)))) 2)
