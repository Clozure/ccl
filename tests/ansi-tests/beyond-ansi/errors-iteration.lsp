;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Contains: Tests of non-ANSI exceptions sutation from CLHS section 6 (Iteration)

(in-package :ba-test)

(compile-and-load "ba-aux.lsp")

;;; DO tests

(def-all-error-test do.1 'listp #'(lambda (x) `(do . ,x)))
(def-all-error-test do.2 'listp #'(lambda (x) `(do () . ,x)))
(def-all-error-test do.3 #'(lambda (x) (or (symbolp x) (listp x))) #'(lambda (x) `(do (,x))))
(def-all-error-test do.4 'listp #'(lambda (x) `(do ((a 1 (1+ a)) . ,x))))
(def-all-error-test do.5 'listp #'(lambda (x) `(do () ,x)))
(def-all-error-test do.6 'listp #'(lambda (x) `(do () (t . ,x))))
(def-all-error-test do.7 'listp #'(lambda (x) `(do () (t) . ,x)))
(def-all-error-test do.8 'listp #'(lambda (x) `(do ((a . ,x)) (t))))
(def-all-error-test do.9 'listp #'(lambda (x) `(do ((a 1 . ,x)) (t))))
(def-all-error-test do.10 'listp #'(lambda (x) `(do ((a 1 (1+ a) . ,x)) (t))))
(def-error-test do.11 (do))

;;; DO* tests

(def-all-error-test do*.1 'listp #'(lambda (x) `(do* . ,x)))
(def-all-error-test do*.2 'listp #'(lambda (x) `(do* () . ,x)))
(def-all-error-test do*.3 #'(lambda (x) (or (symbolp x) (listp x))) #'(lambda (x) `(do* (,x))))
(def-all-error-test do*.4 'listp #'(lambda (x) `(do* ((a 1 (1+ a)) . ,x))))
(def-all-error-test do*.5 'listp #'(lambda (x) `(do* () ,x)))
(def-all-error-test do*.6 'listp #'(lambda (x) `(do* () (t . ,x))))
(def-all-error-test do*.7 'listp #'(lambda (x) `(do* () (t) . ,x)))
(def-all-error-test do*.8 'listp #'(lambda (x) `(do* ((a . ,x)) (t))))
(def-all-error-test do*.9 'listp #'(lambda (x) `(do* ((a 1 . ,x)) (t))))
(def-all-error-test do*.10 'listp #'(lambda (x) `(do* ((a 1 (1+ a) . ,x)) (t))))
(def-error-test do*.11 (do*))

;;; DOTIMES tests

(def-error-test dotimes.1 (dotimes))
(def-all-error-test dotimes.2 'listp #'(lambda (x) `(dotimes . ,x)))
(def-all-error-test dotimes.3 'symbolp #'(lambda (x) `(dotimes (,x 1))))
(def-all-error-test dotimes.4 (constantly nil) #'(lambda (x) `(dotimes (,x))))
(def-all-error-test dotimes.5 'integerp #'(lambda (x) `(dotimes (i ',x))))
(def-all-error-test dotimes.6 'listp #'(lambda (x) `(dotimes (i . ,x))))
(def-all-error-test dotimes.7 'listp #'(lambda (x) `(dotimes (i 1 . ,x))))
(def-all-error-test dotimes.8 'listp #'(lambda (x) `(dotimes (i 1) . ,x)))
(def-all-error-test dotimes.9 'listp #'(lambda (x) `(dotimes (i 1 nil . ,x))))
(def-all-error-test dotimes.10 'listp #'(lambda (x) `(dotimes (i 1 nil ,x))))

;;; DOLIST tests

(def-error-test dolist.1 (dolist))
(def-all-error-test dolist.2 'listp #'(lambda (x) `(dolist . ,x)))
(def-all-error-test dolist.3 'symbolp #'(lambda (x) `(dolist (,x nil))))
(def-all-error-test dolist.4 'listp #'(lambda (x) `(dolist (e . ,x))))
(def-all-error-test dolist.5 'listp #'(lambda (x) `(dolist (e nil . ,x))))
(def-all-error-test dolist.6 'listp #'(lambda (x) `(dolist (e nil nil . ,x))))
(def-all-error-test dolist.7 'listp #'(lambda (x) `(dolist (e nil nil ,x))))
(def-all-error-test dolist.8 'listp #'(lambda (x) `(dolist (e ',x nil))))
(def-all-error-test dolist.9 'listp #'(lambda (x) `(dolist (e nil nil) . ,x)))
