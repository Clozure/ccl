;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon May 30 15:38:09 2005
;;;; Contains: Tests of non-ANSI exceptional situations from CLHS section 5, part 1

(in-package :ba-test)

(compile-and-load "ba-aux.lsp")

;;; APPLY

(def-all-error-test apply.1 'function-designator-p '(apply x nil))
(def-all-error-test apply.2 'function-designator-p '(apply x '(1 2 3)))
(def-error-test apply.3 (apply 'cons . 1))
(def-all-error-test apply.4 'listp '(apply 'cons '1 x))

;;; DEFUN

(def-error-test defun.1 (defun))
(def-error-test defun.2 (defun #.(gensym)))
(def-error-test defun.3 (defun . foo))
(def-error-test defun.4 (defun #.(gensym) #.(gensym)))
(def-error-test defun.5 (defun #.(gensym) () . foo))

(def-error-test defun.6 (defun #.(gensym) () "foo" "bar" (declare)))
(def-error-test defun.7 (defun #.(gensym) () nil (declare)))

;;; FIXME  Add lambda list tests

;;; FLET

(def-error-test flet.1 (flet . foo))
(def-error-test flet.2 (flet foo))
(def-error-test flet.3 (flet (foo)))
(def-error-test flet.4 (flet ((foo))))
(def-error-test flet.5 (flet ((foo . bar))))
(def-error-test flet.6 (flet () . foo))
(def-error-test flet.7 (flet ((foo () . bar))))
(def-error-test flet.8 (flet ((foo z))))
(def-error-test flet.9 (flet ((foo ((x y))))))
(def-all-error-test flet.10 'symbolp
  #'(lambda (x) (subst x 'x '(flet ((foo (&rest x)))))))
(def-all-error-test flet.11 (typef '(or symbol cons))
  #'(lambda (x) (subst x 'x '(flet ((foo (&optional x)))))))
(def-all-error-test flet.12 (typef '(or symbol cons))
  #'(lambda (x) (subst x 'x '(flet ((foo (&key x)))))))

(def-error-test flet.13 (flet ((foo (&optional (x . bar)) nil))))
(def-error-test flet.14 (flet ((foo (&optional (x nil . bar)) nil))))
(def-error-test flet.15 (flet ((foo (&optional (x nil x-p . bar)) nil))))
(def-error-test flet.16 (flet ((foo (&optional (x nil x-p nil)) nil))))

(def-error-test flet.17 (flet ((foo (&key (x . bar)) nil))))
(def-error-test flet.18 (flet ((foo (&key (x nil . bar)) nil))))
(def-error-test flet.19 (flet ((foo (&key (x nil x-p . bar)) nil))))
(def-error-test flet.20 (flet ((foo (&key (x nil x-p nil)) nil))))

(def-error-test flet.21 (flet ((foo (&key ((x . bar))) nil))))
(def-error-test flet.22 (flet ((foo (&key ((x y . z))) nil))))
(def-error-test flet.23 (flet ((foo (&key ((x y z))) nil))))

(def-all-error-test flet.24 'symbolp
  #'(lambda (x) `(flet ((foo (&key ((,x y))) nil)))))

(def-all-error-test flet.25 'symbolp
  #'(lambda (x) `(flet ((foo (&key ((y ,x))) nil)))))

(def-error-test flet.26 (flet ((foo (&aux . bar)))))
(def-error-test flet.27 (flet ((foo (&aux (x . bar))))))
(def-error-test flet.28 (flet ((foo (&aux (x nil . bar))))))
(def-error-test flet.29 (flet ((foo (&aux (x nil nil))))))

(def-error-test flet.30 (flet ((foo () "x" "y" (declare))) (foo)))
(def-error-test flet.31 (flet ((foo () :bad1) (foo () :bad2)) (foo)))

;;; FIXME Add tests for disallowed lambda list keywords

;;; LABELS

(def-error-test labels.1 (labels . foo))
(def-error-test labels.2 (labels foo))
(def-error-test labels.3 (labels (foo)))
(def-error-test labels.4 (labels ((foo))))
(def-error-test labels.5 (labels ((foo . bar))))
(def-error-test labels.6 (labels () . foo))
(def-error-test labels.7 (labels ((foo () . bar))))
(def-error-test labels.8 (labels ((foo z))))
(def-error-test labels.9 (labels ((foo ((x y))))))
(def-all-error-test labels.10 'symbolp
  #'(lambda (x) (subst x 'x '(labels ((foo (&rest x)))))))
(def-all-error-test labels.11 (typef '(or symbol cons))
  #'(lambda (x) (subst x 'x '(labels ((foo (&optional x)))))))
(def-all-error-test labels.12 (typef '(or symbol cons))
  #'(lambda (x) (subst x 'x '(labels ((foo (&key x)))))))

(def-error-test labels.13 (labels ((foo (&optional (x . bar)) nil))))
(def-error-test labels.14 (labels ((foo (&optional (x nil . bar)) nil))))
(def-error-test labels.15 (labels ((foo (&optional (x nil x-p . bar)) nil))))
(def-error-test labels.16 (labels ((foo (&optional (x nil x-p nil)) nil))))

(def-error-test labels.17 (labels ((foo (&key (x . bar)) nil))))
(def-error-test labels.18 (labels ((foo (&key (x nil . bar)) nil))))
(def-error-test labels.19 (labels ((foo (&key (x nil x-p . bar)) nil))))
(def-error-test labels.20 (labels ((foo (&key (x nil x-p nil)) nil))))

(def-error-test labels.21 (labels ((foo (&key ((x . bar))) nil))))
(def-error-test labels.22 (labels ((foo (&key ((x y . z))) nil))))
(def-error-test labels.23 (labels ((foo (&key ((x y z))) nil))))

(def-all-error-test labels.24 'symbolp
  #'(lambda (x) `(labels ((foo (&key ((,x y))) nil)))))

(def-all-error-test labels.25 'symbolp
  #'(lambda (x) `(labels ((foo (&key ((y ,x))) nil)))))

(def-error-test labels.26 (labels ((foo (&aux . bar)))))
(def-error-test labels.27 (labels ((foo (&aux (x . bar))))))
(def-error-test labels.28 (labels ((foo (&aux (x nil . bar))))))
(def-error-test labels.29 (labels ((foo (&aux (x nil nil))))))

(def-error-test labels.30 (labels ((foo () "x" "y" (declare))) (foo)))
(def-error-test labels.31 (labels ((foo () :bad1) (foo () :bad2)) (foo)))

;;; FIXME Add tests for disallowed lambda list keywords

;;; MACROLET
;;; FIXME: add these
