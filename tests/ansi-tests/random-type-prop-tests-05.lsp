;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Mar  8 20:31:08 2005
;;;; Contains: Random type prop tests, part 5 (Cons)

(in-package :cl-test)

(def-type-prop-test list.1 'list nil 1 :rest-type 't :maxargs 10)
(def-type-prop-test list.2 '(lambda (x) (car (list x))) '(t) 1)
(def-type-prop-test list.3 '(lambda (x y) (cdr (list x y))) '(t t) 2)
(def-type-prop-test list.4 '(lambda (x y z) (cadr (list x y z))) '(t t t) 3)
(def-type-prop-test list.5 '(lambda (x) (let ((z (list x))) (declare (dynamic-extent z)) (car z)))  '(t) 1)

(def-type-prop-test list* 'list* () 1 :rest-type t :maxargs 10)

(def-type-prop-test null 'null '(t) 1)
(def-type-prop-test cons.1 'cons '(t t) 2)
(def-type-prop-test cons.2 '(lambda (x y) (car (cons y x))) '(t t) 2)
(def-type-prop-test cons.3 '(lambda (x y) (cdr (cons x y))) '(t t) 2)

(def-type-prop-test consp 'consp '(t) 1)
(def-type-prop-test atom 'atom '(t) 1)

(def-type-prop-test rplaca 'rplaca '(cons t) 2 :replicate '(t nil))
(def-type-prop-test rplacd 'rplacd '(cons t) 2 :replicate '(t nil))

(def-type-prop-test car 'car '((cons t t)) 1)
(def-type-prop-test first 'first '((cons t t)) 1)
(def-type-prop-test cdr 'cdr '((cons t t)) 1)
(def-type-prop-test rest 'rest '((cons t t)) 1)
(def-type-prop-test caar 'caar '((cons (cons t t) t)) 1)
(def-type-prop-test cdar 'cdar '((cons (cons t t) t)) 1)
(def-type-prop-test cadr 'cadr '((cons t (cons t t))) 1)
(def-type-prop-test second 'second '((cons t (cons t t))) 1)
(def-type-prop-test cddr 'cddr '((cons t (cons t t))) 1)
(def-type-prop-test caaar 'caaar '((cons (cons (cons t t) t) t)) 1)
(def-type-prop-test cdaar 'cdaar '((cons (cons (cons t t) t) t)) 1)
(def-type-prop-test cadar 'cadar '((cons (cons t (cons t t)) t)) 1)
(def-type-prop-test cddar 'cddar '((cons (cons t (cons t t)) t)) 1)
(def-type-prop-test caadr 'caadr '((cons t (cons (cons t t) t))) 1)
(def-type-prop-test cdadr 'cdadr '((cons t (cons (cons t t) t))) 1)
(def-type-prop-test caddr 'caddr '((cons t (cons t (cons t t)))) 1)
(def-type-prop-test third 'third '((cons t (cons t (cons t t)))) 1)
(def-type-prop-test cdddr 'cdddr '((cons t (cons t (cons t t)))) 1)

(def-type-prop-test caaaar'caaaar '((cons (cons (cons (cons t t) t) t) t)) 1)
(def-type-prop-test cdaaar 'cdaaar '((cons (cons (cons (cons t t) t) t) t)) 1)
(def-type-prop-test cadaar 'cadaar '((cons (cons (cons t (cons t t)) t) t)) 1)
(def-type-prop-test cddaar 'cddaar '((cons (cons (cons t (cons t t)) t) t)) 1)
(def-type-prop-test caadar 'caadar '((cons (cons t (cons (cons t t) t)) t)) 1)
(def-type-prop-test cdadar 'cdadar '((cons (cons t (cons (cons t t) t)) t)) 1)
(def-type-prop-test caddar 'caddar '((cons (cons t (cons t (cons t t))) t)) 1)
(def-type-prop-test cdddar 'cdddar '((cons (cons t (cons t (cons t t))) t)) 1)
(def-type-prop-test caaadr 'caaadr '((cons t (cons (cons (cons t t) t) t))) 1)
(def-type-prop-test cdaadr 'cdaadr '((cons t (cons (cons (cons t t) t) t))) 1)
(def-type-prop-test cadadr 'cadadr '((cons t (cons (cons t (cons t t)) t))) 1)
(def-type-prop-test cddadr 'cddadr '((cons t (cons (cons t (cons t t)) t))) 1)
(def-type-prop-test caaddr 'caaddr '((cons t (cons t (cons (cons t t) t)))) 1)
(def-type-prop-test cdaddr 'cdaddr '((cons t (cons t (cons (cons t t) t)))) 1)
(def-type-prop-test cadddr 'cadddr '((cons t (cons t (cons t (cons t t))))) 1)
(def-type-prop-test fourth 'fourth '((cons t (cons t (cons t (cons t t))))) 1)
(def-type-prop-test cddddr 'cddddr '((cons t (cons t (cons t (cons t t))))) 1)

(def-type-prop-test fifth 'fifth '((cons t (cons t (cons t (cons t (cons t t)))))) 1)
(def-type-prop-test sixth 'sixth '((cons t (cons t (cons t (cons t (cons t (cons t t))))))) 1)
(def-type-prop-test seventh 'seventh '((cons t (cons t (cons t (cons t (cons t (cons t (cons t t)))))))) 1)
(def-type-prop-test eighth 'eighth
  '((cons t (cons t (cons t (cons t (cons t (cons t (cons t (cons t t)))))))))
  1)
(def-type-prop-test ninth 'ninth
  '((cons t (cons t (cons t (cons t (cons t (cons t (cons t (cons t (cons t t))))))))))
  1)
(def-type-prop-test tenth 'tenth
  '((cons t (cons t (cons t (cons t (cons t (cons t (cons t (cons t (cons t (cons t t)))))))))))
  1)

(def-type-prop-test pop '(lambda (x) (list (pop x) x)) '((cons t t)) 1)
(def-type-prop-test push '(lambda (x y) (list (push x y) x y)) '(t t) 2)

(def-type-prop-test copy-tree.1 'copy-tree '((cons t t)) 1)
(def-type-prop-test copy-tree.2 'copy-tree '((cons (cons t t) (cons t t))) 1)
(def-type-prop-test copy-tree.3 'copy-tree '((cons t (cons (cons t (cons t t)) t))) 1)
(def-type-prop-test copy-tree.4 'copy-tree '(list) 1)

(def-type-prop-test sublis.1 'sublis '((cons (cons symbol t) null) list) 2)
(def-type-prop-test sublis.2 'sublis '((cons (cons (integer 0 7) t) null) list) 2)
(def-type-prop-test sublis.3 'sublis '(null list) 2)
(def-type-prop-test sublis.4 'sublis `((cons (cons boolean t) null) list
				       (eql :key)
				       (or null (eql not) (eql ,#'not))) 4)
(def-type-prop-test sublis.5 'sublis `((cons (cons t t) null) list (eql :test) (or (eql equal) (eql ,#'equal))) 4)
(def-type-prop-test sublis.6 'sublis `((cons (cons t t) null) list (eql :test-not) (or (eql eql) (eql ,#'eql))) 4)

(def-type-prop-test subst.1 'subst '(t t t) 3)
(def-type-prop-test subst.2 'subst '(t t (cons t t)) 3)
(def-type-prop-test subst.3 'subst '(t t list) 3)
(def-type-prop-test subst.4 'subst '(t t (cons (cons t t) (cons t t))) 3)
(def-type-prop-test subst.5 'subst `(boolean t (cons (cons t t) (cons t t))
					     (eql :key)
					     (or null (eql not) (eql ,#'not))) 5)
(def-type-prop-test subst.6 'subst `(t t (cons (cons t t) (cons t t)) (eql :test) (or (eql equal) (eql ,#'equal))) 5)
(def-type-prop-test subst.7 'subst `(t t (cons (cons t t) (cons t t)) (eql :test-not) (or (eql equal) (eql ,#'equal))) 5)
(def-type-prop-test subst.8 'subst `(t t (cons (cons t t) (cons t t))
				       (eql :key) (or null (eql not) (eql ,#'not))
				       (eql :test) (or (eql equal) (eql ,#'equal))) 7)

(def-type-prop-test nsubst.1 'nsubst '(t t t) 3 :replicate '(nil nil t))
(def-type-prop-test nsubst.2 'nsubst '(t t (cons t t)) 3 :replicate '(nil nil t))
(def-type-prop-test nsubst.3 'nsubst '(t t list) 3 :replicate '(nil nil t))
(def-type-prop-test nsubst.4 'nsubst '(t t (cons (cons t t) (cons t t))) 3 :replicate '(nil nil t))
(def-type-prop-test nsubst.5 'nsubst `(boolean t (cons (cons t t) (cons t t))
					       (eql :key)
					       (or null (eql not) (eql ,#'not))) 5
					       :replicate '(nil nil t nil nil))
(def-type-prop-test nsubst.6 'nsubst `(t t (cons (cons t t) (cons t t)) (eql :test) (or (eql equal) (eql ,#'equal))) 5 :replicate '(nil nil t nil nil))
(def-type-prop-test nsubst.7 'nsubst `(t t (cons (cons t t) (cons t t)) (eql :test-not) (or (eql equal) (eql ,#'equal))) 5 :replicate '(nil nil t nil nil))
(def-type-prop-test nsubst.8 'nsubst `(t t (cons (cons t t) (cons t t))
					 (eql :key) (or null (eql not) (eql ,#'not))
					 (eql :test) (or (eql equal) (eql ,#'equal))) 7
					 :replicate '(nil nil t nil nil nil nil))


(def-type-prop-test subst-if.1 'subst-if `(t (or (eql not) (eql ,#'not)) list) 3)
(def-type-prop-test subst-if.2 'subst-if `(t (or (eql not) (eql ,#'not)) (cons (or null t) (or null t))) 3)
(def-type-prop-test subst-if.3 'subst-if `(t (eql identity)
					     (cons (cons (cons t t) (cons t t)) (cons (cons t t) (cons t t)))
					     (eql :key) (or null (eql not) (eql ,#'not))) 5)

(def-type-prop-test nsubst-if.1 'nsubst-if `(t (or (eql not) (eql ,#'not)) list) 3 :replicate '(nil nil t))
(def-type-prop-test nsubst-if.2 'nsubst-if `(t (or (eql not) (eql ,#'not)) (cons (or null t) (or null t))) 3 :replicate '(nil nil t))
(def-type-prop-test nsubst-if.3 'nsubst-if `(t (eql identity)
					       (cons (cons (cons t t) (cons t t)) (cons (cons t t) (cons t t)))
					       (eql :key) (or null (eql not) (eql ,#'not))) 5
					       :replicate '(nil nil t nil nil))

(def-type-prop-test subst-if-not.1 'subst-if-not `(t (or (eql not) (eql ,#'not)) list) 3)
(def-type-prop-test subst-if-not.2 'subst-if-not `(t (or (eql not) (eql ,#'not)) (cons (or null t) (or null t))) 3)
(def-type-prop-test subst-if-not.3 'subst-if-not `(t (eql identity)
						     (cons (cons (cons t t) (cons t t)) (cons (cons t t) (cons t t)))
						     (eql :key) (or null (eql not) (eql ,#'not))) 5)

(def-type-prop-test nsubst-if-not.1 'nsubst-if-not `(t (or (eql not) (eql ,#'not)) list) 3 :replicate '(nil nil t))
(def-type-prop-test nsubst-if-not.2 'nsubst-if-not `(t (or (eql not) (eql ,#'not)) (cons (or null t) (or null t))) 3 :replicate '(nil nil t))
(def-type-prop-test nsubst-if-not.3 'nsubst-if-not `(t (eql identity)
						       (cons (cons (cons t t) (cons t t)) (cons (cons t t) (cons t t)))
						       (eql :key) (or null (eql not) (eql ,#'not))) 5
						       :replicate '(nil nil t nil nil))

(def-type-prop-test tree-equal.1 'tree-equal (list t #'(lambda (x) `(or t (eql ,(copy-tree x))))) 2)
(def-type-prop-test tree-equal.2 'tree-equal (list 'list #'(lambda (x) `(or list (eql ,(copy-tree t))))) 2)
(def-type-prop-test tree-equal.3 'tree-equal (list '(cons t t)
						   #'(lambda (x) `(or (cons t t) (eql ,(copy-tree x))))
						   '(eql :test)
						   `(or (eql equal) (eql ,#'equal)))
  4)
(def-type-prop-test tree-equal.4 'tree-equal (list t #'(lambda (x) `(or t (eql ,(copy-tree x))))
						   '(eql :test-not) '(eql eql))
  4)

(def-type-prop-test copy-list.1 'copy-list '(list) 1)
(def-type-prop-test copy-list.2 'copy-list '((cons t t)) 1)
(def-type-prop-test copy-list.3 'copy-list '((cons t (cons t (or t (cons t (or t (cons t t))))))) 1)

(def-type-prop-test list-length.1 'list-length '(list) 1)
(def-type-prop-test list-length.2 'list-length '((cons t list)) 1)

(def-type-prop-test listp 'listp '(t) 1)

(def-type-prop-test make-list.1 'make-list '((integer 0 100)) 1)
(def-type-prop-test make-list.2 '(lambda (x) (length (make-list x))) '((integer 0 100)) 1)
(def-type-prop-test make-list.3 'make-list '((integer 0 100) (eql :initial-element) t) 3)

(def-type-prop-test nth.1 'nth '((integer 0 12) list) 2)

(def-type-prop-test endp.1 'endp '((or null (cons t t))) 1)

(def-type-prop-test append.1 'append nil 1 :maxargs 10 :rest-type 'list)
(def-type-prop-test append.2 'append '(list t) 2)
(def-type-prop-test append.3 'append '(list list t) 3)
(def-type-prop-test append.4 'append '(list list list t) 4)

(def-type-prop-test nconc.1 'nconc '(list) 1)
(def-type-prop-test nconc.2 'nconc '(list list) 2 :replicate '(t nil))
(def-type-prop-test nconc.3 'nconc '(list list list) 3 :replicate '(t t nil))
(def-type-prop-test nconc.4 'nconc '(list list list list) 4 :replicate '(t t t nil))

(def-type-prop-test revappend 'revappend '(list t) 2)
(def-type-prop-test nreconc 'nreconc '(list t) 2 :replicate '(t nil))

(def-type-prop-test butlast.1 'butlast '(list) 1)
(def-type-prop-test butlast.2 'butlast '(list (integer 0 20)) 2)

(def-type-prop-test nbutlast.1 'nbutlast '(list) 1 :replicate '(t))
(def-type-prop-test nbutlast.2 'nbutlast '(list (integer 0 20)) 2 :replicate '(t nil))

(def-type-prop-test last.1 'last '(list) 1)
(def-type-prop-test last.2 'last '(list (integer 0 15)) 2)
(def-type-prop-test last.3 'last '((cons t (or t (cons t (or t (cons t t)))))) 1)
(def-type-prop-test last.4 'last '((cons t (or t (cons t (or t (cons t t))))) (integer 0 5)) 2)

(def-type-prop-test ldiff.1 'ldiff '(list t) 2)
(def-type-prop-test ldiff.2 'ldiff (list 'list
					 #'(lambda (x)
					     (if (consp x)
						 `(or t (eql ,(nthcdr (random (length x)) x)))
					       t)))
  2)

(def-type-prop-test tailp.1 'tailp '(t list) 2)
(def-type-prop-test tailp.2 'tailp (list t #'(lambda (x) (make-list-type (1+ (random 10)) `(eql ,x)))) 2)

(def-type-prop-test nthcdr 'nthcdr '((integer 0 20) list) 2)

(def-type-prop-test member.1 'member '(t list) 2)
(def-type-prop-test member.2 'member
  (list t #'(lambda (x) (make-list-type (random 5) `(cons (eql ,x) ,(make-list-type (random 5))))))
  2)
(def-type-prop-test member.3 'member `(t list (eql :key) (or (eql not) (eql ,#'not))) 4)
(def-type-prop-test member.4 'member `(t list (eql :test) (or (eql equalp) (eql ,#'equalp))) 4)
(def-type-prop-test member.5 'member `(t list (eql :test-not) (or (eql eql) (eql ,#'eql))) 4)
(def-type-prop-test member.6 'member `(t list (eql :allow-other-keys) (and t (not null)) (eql :foo) t) 6)

(def-type-prop-test member-if.1 'member-if `((or (eql symbolp) (eql ,#'symbolp)) list) 2)
(def-type-prop-test member-if.2 'member-if
  (list '(eql zerop) #'(lambda (x) (make-list-type (random 10) 'null '(integer 0 10))))
  2)
(def-type-prop-test member-if.3 'member-if
  (list '(eql zerop) #'(lambda (x) (make-list-type (random 10) 'null '(integer 0 10)))
	'(eql :key)`(or (eql 1-) (eql ,#'1-)))
  4)

(def-type-prop-test member-if-not.1 'member-if-not `((or (eql symbolp) (eql ,#'symbolp)) list) 2)
(def-type-prop-test member-if-not.2 'member-if-not
  (list '(eql plusp) #'(lambda (x) (make-list-type (random 10) 'null '(integer 0 10))))
  2)
(def-type-prop-test member-if-not.3 'member-if-not
  (list '(eql plusp)
	#'(lambda (x) (make-list-type (random 10) 'null '(integer 0 10)))
	'(eql :key)
	`(or (eql 1-) (eql ,#'1-)))
  4)
(def-type-prop-test member-if-not.4 'member-if-not
  `((eql identity) list
    (eql :allow-other-keys) (and t (not null))
    (member :foo :bar #:xyz) t)
  6)

(def-type-prop-test mapc.1 'mapc '((eql list)) 2 :rest-type 'list :maxargs 10)
(def-type-prop-test mapc.2 'mapc `((eql ,#'values)) 2 :rest-type 'list :maxargs 10)

(def-type-prop-test mapcar.1 'mapcar '((eql list)) 2 :rest-type 'list :maxargs 10)
(def-type-prop-test mapcar.2 'mapcar `((eql ,#'vector)) 2 :rest-type 'list :maxargs 10)

(def-type-prop-test maplist.1 'maplist '((eql list)) 2 :rest-type 'list :maxargs 10)
(def-type-prop-test maplist.2 'maplist `((eql ,#'vector)) 2 :rest-type 'list :maxargs 10)

(def-type-prop-test mapl.1 'mapl '((eql list)) 2 :rest-type 'list :maxargs 10)
(def-type-prop-test mapl.2 'mapl `((eql ,#'vector)) 2 :rest-type 'list :maxargs 10)

(def-type-prop-test mapcan.1 'mapcan '((eql list)) 2 :rest-type 'list :maxargs 10)

(def-type-prop-test mapcon.1 'mapcon '((eql copy-list) list) 2)

(def-type-prop-test acons 'acons 
  (list t t #'(lambda (x y) (make-list-type (random 5) 'null '(or null (cons t t)))))
  3)

(def-type-prop-test assoc.1 'assoc (list t #'(lambda (x) (make-list-type (random 6) 'null '(or null (cons t t))))) 2)
(def-type-prop-test assoc.2 'assoc
  (list t #'(lambda (x) (make-list-type (random 6) 'null `(or null (cons t t) (cons (eql ,x) t)))))
  2)
(def-type-prop-test assoc.3 'assoc
  (list t #'(lambda (x) (make-list-type (random 6) 'null `(or null (cons t t) (cons (eql ,x) t))))
	'(eql :key) `(or (eql not) (eql ,#'not)))
  4)
(def-type-prop-test assoc.4 'assoc
  (list 'real
	#'(lambda (x) (make-list-type (random 6) 'null `(or null (cons real t) (cons (eql ,x) t))))
	`(member :test :test-not) `(member <= < = /= > >= ,#'<= ,#'< ,#'= ,#'/= ,#'> ,#'>=))
  4)

(def-type-prop-test assoc-if.1 'assoc-if
  (list `(member identity not symbolp numberp arrayp ,#'identity ,#'not ,#'symbolp ,#'numberp ,#'arrayp)
	(make-list-type (random 8) 'null '(or null (cons t t))))
  2)
(def-type-prop-test assoc-if.2 'assoc-if
  (list `(member plusp minusp zerop ,#'plusp ,#'minusp ,#'zerop)
	(make-list-type (random 8) 'null '(or null (cons real t)))
	'(eql :key) `(member 1+ 1- - abs signum ,#'1+ ,#'1- ,#'- ,#'abs ,#'signum))
  2)

(def-type-prop-test assoc-if-not.1 'assoc-if-not
  (list `(member identity not symbolp numberp arrayp ,#'identity ,#'not ,#'symbolp ,#'numberp ,#'arrayp)
	(make-list-type (random 8) 'null '(or null (cons t t))))
  2)
(def-type-prop-test assoc-if-not.2 'assoc-if-not
  (list `(member plusp minusp zerop ,#'plusp ,#'minusp ,#'zerop)
	(make-list-type (random 8) 'null '(or null (cons real t)))
	'(eql :key) `(member 1+ 1- - abs signum ,#'1+ ,#'1- ,#'- ,#'abs ,#'signum))
  2)

(def-type-prop-test copy-alist 'copy-alist
  (list #'(lambda () (make-list-type (random 10) 'null '(or null (cons t t)))))
  1)

(def-type-prop-test pairlis.1 'pairlis
  (list 'list #'(lambda (x) (make-list-type (length x) 'null t)))
  2)

(def-type-prop-test pairlis.2 'pairlis
  (list 'list #'(lambda (x) (make-list-type (length x) 'null t))
	#'(lambda (x y) (make-list-type (random 6) 'null '(or null (cons t t)))))
  3)

(def-type-prop-test rassoc.1 'rassoc (list t #'(lambda (x) (make-list-type (random 6) 'null '(or null (cons t t))))) 2)
(def-type-prop-test rassoc.2 'rassoc
  (list t #'(lambda (x) (make-list-type (random 6) 'null `(or null (cons t t) (cons t (eql ,x))))))
  2)
(def-type-prop-test rassoc.3 'rassoc
  (list t #'(lambda (x) (make-list-type (random 6) 'null `(or null (cons t t) (cons t (eql ,x)))))
	'(eql :key) `(or (eql not) (eql ,#'not)))
  4)
(def-type-prop-test rassoc.4 'rassoc
  (list 'real
	#'(lambda (x) (make-list-type (random 6) 'null `(or null (cons t real) (cons t (eql ,x)))))
	`(member :test :test-not) `(member <= < = /= > >= ,#'<= ,#'< ,#'= ,#'/= ,#'> ,#'>=))
  4)

(def-type-prop-test rassoc-if.1 'rassoc-if
  (list `(member identity not symbolp numberp arrayp ,#'identity ,#'not ,#'symbolp ,#'numberp ,#'arrayp)
	(make-list-type (random 8) 'null '(or null (cons t t))))
  2)
(def-type-prop-test rassoc-if.2 'rassoc-if
  (list `(member plusp minusp zerop ,#'plusp ,#'minusp ,#'zerop)
	(make-list-type (random 8) 'null '(or null (cons t real)))
	'(eql :key) `(member 1+ 1- - abs signum ,#'1+ ,#'1- ,#'- ,#'abs ,#'signum))
  2)

(def-type-prop-test rassoc-if-not.1 'rassoc-if-not
  (list `(member identity not symbolp numberp arrayp ,#'identity ,#'not ,#'symbolp ,#'numberp ,#'arrayp)
	(make-list-type (random 8) 'null '(or null (cons t t))))
  2)
(def-type-prop-test rassoc-if-not.2 'rassoc-if-not
  (list `(member plusp minusp zerop ,#'plusp ,#'minusp ,#'zerop)
	(make-list-type (random 8) 'null '(or null (cons t real)))
	'(eql :key) `(member 1+ 1- - abs signum ,#'1+ ,#'1- ,#'- ,#'abs ,#'signum))
  2)

;;; We don't use numbers or characters as indicators, since the test is EQ,
;;; which is not well-behaved on these types.

(def-type-prop-test get-properties.1 'get-properties
  (list #'(lambda () (make-list-type (* 2 (random 5)) 'null '(not (or number character)))) 'list)
  2)
(def-type-prop-test get-properties.2 'get-properties
  (list #'(lambda () (make-list-type (* 2 (random 5)) 'null '(not (or number character))))
	#'(lambda (plist) (let ((len (length plist)))
			    (if (= len 0) '(cons t null)
			      (let ((ind (elt plist (* 2 (random (floor len 2))))))
				`(cons (eql ,ind) null))))))
  2)

(def-type-prop-test getf.1 'getf
  (list #'(lambda () (make-list-type (* 2 (random 5)) 'null '(not (or number character)))) t)
  2)
(def-type-prop-test getf.2 'getf
  (list #'(lambda () (make-list-type (* 2 (random 5)) 'null '(not (or number character))))
	#'(lambda (plist) (let ((len (length plist)))
			    (if (= len 0) t
			      (let ((ind (elt plist (* 2 (random (floor len 2))))))
				`(eql ,ind))))))
  2)
(def-type-prop-test getf.3 'getf
  (list #'(lambda () (make-list-type (* 2 (random 5)) 'null '(not (or number character))))
	t t)
  3)

(def-type-prop-test intersection.1 'intersection '(list list) 2 :test #'same-set-p)
(def-type-prop-test intersection.2 'intersection '(list list (eql :key) (eql identity))  4 :test #'same-set-p)
(def-type-prop-test intersection.3 'intersection
  (list #'(lambda () (make-list-type (random 10) 'null 'integer))
	#'(lambda (x) (make-list-type (random 10) 'null 'integer))
	'(eql :key)
	`(member 1+ ,#'1+))
  4
  :test #'same-set-p)
(def-type-prop-test intersection.4 'intersection
  (list #'(lambda () (make-list-type (random 10) 'null '(cons integer null)))
	#'(lambda (x) (make-list-type (random 10) 'null '(cons integer null)))
	'(eql :key)
	`(member car ,#'car))
  4
  :test #'(lambda (x y) (same-set-p x y :key #'car)))
(def-type-prop-test intersection.5 'intersection
  (list #'(lambda () (make-list-type (random 10) 'null '(cons integer null)))
	#'(lambda (x) (make-list-type (random 10) 'null '(cons integer null)))
	'(eql :test)
	`(member equal ,#'equal))
  4
  :test #'(lambda (x y) (same-set-p x y :key #'car)))

(def-type-prop-test nintersection.1 'nintersection '(list list) 2 :test #'same-set-p :replicate '(t t))
(def-type-prop-test nintersection.2 'nintersection '(list list (eql :key) (eql identity))  4 :test #'same-set-p :replicate '(t t nil nil))
(def-type-prop-test nintersection.3 'nintersection
  (list #'(lambda () (make-list-type (random 10) 'null 'integer))
	#'(lambda (x) (make-list-type (random 10) 'null 'integer))
	'(eql :key)
	`(member 1+ ,#'1+))
  4
  :test #'same-set-p
  :replicate '(t t nil nil))
(def-type-prop-test nintersection.4 'nintersection
  (list #'(lambda () (make-list-type (random 10) 'null '(cons integer null)))
	#'(lambda (x) (make-list-type (random 10) 'null '(cons integer null)))
	'(eql :key)
	`(member car ,#'car))
  4
  :test #'(lambda (x y) (same-set-p x y :key #'car))
  :replicate '(t t nil nil))
(def-type-prop-test nintersection.5 'nintersection
  (list #'(lambda () (make-list-type (random 10) 'null '(cons integer null)))
	#'(lambda (x) (make-list-type (random 10) 'null '(cons integer null)))
	'(eql :test)
	`(member equal ,#'equal))
  4
  :test #'(lambda (x y) (same-set-p x y :key #'car))
  :replicate '(t t nil nil))


(def-type-prop-test adjoin.1 'adjoin '(t list) 2)
(def-type-prop-test adjoin.2 'adjoin '((integer 0 1) list) 2)
(def-type-prop-test adjoin.3 'adjoin `((integer 0 10) (cons number (cons number (cons number null)))
				       (eql :test) (or (eql =) (eql ,#'=)))
  4)
(def-type-prop-test adjoin.4 'adjoin `(number
				       (cons number (cons number (cons number (cons number null))))
				       (eql :test-not) (or (eql /=) (eql ,#'/=)))
  4)
(def-type-prop-test adjoin.5 'adjoin `(number
				       (cons number (cons number (cons number (cons number null))))
				       (eql :key) (or (member 1+ 1- ,#'1+ ,#'1-)))
  4)

(def-type-prop-test pushnew.1 '(lambda (x y) (list (pushnew x y) y)) '(t list) 2)
(def-type-prop-test pushnew.2 '(lambda (x y) (list (pushnew x y) y)) '((integer 0 1) list) 2)
(def-type-prop-test pushnew.3 '(lambda (x y) (list (pushnew x y :test #'=) y))
  `((integer 0 10) (cons number (cons number (cons number null))))
  2)
(def-type-prop-test pushnew.4 '(lambda (x y) (list (pushnew x y :test-not #'/=) y))
  `((integer 0 10) (cons number (cons number (cons number null))))
  2)
(def-type-prop-test pushnew.5 '(lambda (x y) (list (pushnew x y :key #'1+) y))
  `(number (cons number (cons number (cons number (cons number null)))))
  2)

(def-type-prop-test set-difference.1 'set-difference '(list list) 2)
(def-type-prop-test set-difference.2 'set-difference '((cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null)))
						       (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null))))
  2)
(def-type-prop-test set-difference.3 'set-difference `((cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null)))
						       (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null)))
						       (eql :test) (member = ,#'=))
  4)
(def-type-prop-test set-difference.4 'set-difference `((cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null)))
						       (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null)))
						       (eql :test-not) (member /= ,#'/=))
  4)
(def-type-prop-test set-difference.5 'set-difference `((cons (unsigned-byte 3) (cons (unsigned-byte 3) null))
						       (cons (unsigned-byte 3) (cons (unsigned-byte 3) null))
						       (eql :key) (member evenp oddp ,#'evenp ,#'oddp))
  4)

(def-type-prop-test nset-difference.1 'nset-difference '(list list) 2 :replicate '(t t))
(def-type-prop-test nset-difference.2 'nset-difference '((cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null)))
						       (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null))))
  2 :replicate '(t t))
(def-type-prop-test nset-difference.3 'nset-difference `((cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null)))
						       (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null)))
						       (eql :test) (member = ,#'=))
  4 :replicate '(t t nil nil))
(def-type-prop-test nset-difference.4 'nset-difference `((cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null)))
						       (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null)))
						       (eql :test-not) (member /= ,#'/=))
  4 :replicate '(t t nil nil))
(def-type-prop-test nset-difference.5 'nset-difference `((cons (unsigned-byte 3) (cons (unsigned-byte 3) null))
						       (cons (unsigned-byte 3) (cons (unsigned-byte 3) null))
						       (eql :key) (member evenp oddp ,#'evenp ,#'oddp))
  4 :replicate '(t t nil nil))


(def-type-prop-test set-exclusive-or.1 'set-exclusive-or '(list list) 2)
(def-type-prop-test set-exclusive-or.2 'set-exclusive-or '((cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null)))
						       (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null))))
  2)
(def-type-prop-test set-exclusive-or.3 'set-exclusive-or `((cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null)))
						       (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null)))
						       (eql :test) (member = ,#'=))
  4)
(def-type-prop-test set-exclusive-or.4 'set-exclusive-or `((cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null)))
						       (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null)))
						       (eql :test-not) (member /= ,#'/=))
  4)
(def-type-prop-test set-exclusive-or.5 'set-exclusive-or `((cons (unsigned-byte 3) (cons (unsigned-byte 3) null))
						       (cons (unsigned-byte 3) (cons (unsigned-byte 3) null))
						       (eql :key) (member evenp oddp ,#'evenp ,#'oddp))
  4)

(def-type-prop-test nset-exclusive-or.1 'nset-exclusive-or '(list list) 2 :replicate '(t t))
(def-type-prop-test nset-exclusive-or.2 'nset-exclusive-or '((cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null)))
						       (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null))))
  2 :replicate '(t t))
(def-type-prop-test nset-exclusive-or.3 'nset-exclusive-or `((cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null)))
						       (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null)))
						       (eql :test) (member = ,#'=))
  4 :replicate '(t t nil nil))
(def-type-prop-test nset-exclusive-or.4 'nset-exclusive-or `((cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null)))
						       (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null)))
						       (eql :test-not) (member /= ,#'/=))
  4 :replicate '(t t nil nil))
(def-type-prop-test nset-exclusive-or.5 'nset-exclusive-or `((cons (unsigned-byte 3) (cons (unsigned-byte 3) null))
						       (cons (unsigned-byte 3) (cons (unsigned-byte 3) null))
						       (eql :key) (member evenp oddp ,#'evenp ,#'oddp))
  4 :replicate '(t t nil nil))

(def-type-prop-test subsetp.1 'subsetp '(list list) 2)
(def-type-prop-test subsetp.2 'subsetp '((cons integer null)
					 (cons integer (cons integer (cons integer (cons integer null)))))
  2)
