;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 21 18:27:22 2004
;;;; Contains: Tests of DO-ALL-SYMBOLS

(in-package :cl-test)

(def-macro-test do-all-symbols.error.1
  (do-all-symbols (x)))

;;; FIXME  Add tests for non-error cases

(deftest do-all-symbols.1
  (let ((symbols nil))
    (do-all-symbols (sym) (push sym symbols))
    (let ((hash (make-hash-table :test 'eq)))
      (with-package-iterator
       (f (list-all-packages) :internal :external :inherited)
       (loop
	(multiple-value-bind (found sym) (f)
	  (unless found (return))
	  (setf (gethash sym hash) t))))
      ;; hash now contains all symbols accessible in any package
      ;; Check that all symbols from DO-ALL-SYMBOLS are in this
      ;; package
      (loop for s in symbols unless (gethash s hash) collect s)))
  nil)

;; This is the converse of do-all-symbols.1
(deftest do-all-symbols.2
  (let ((symbols nil))
    (with-package-iterator
     (f (list-all-packages) :internal :external :inherited)
     (loop
      (multiple-value-bind (found sym) (f)
	(unless found (return))`
	(push sym symbols))))
    (let ((hash (make-hash-table :test 'eq)))
      (do-all-symbols (s) (setf (gethash s hash) t))
      (loop for s in symbols unless (gethash s hash) collect s)))
  nil)

(deftest do-all-symbols.3
  (let ((sym (gensym)))
    (do-all-symbols (s t) (assert (not (eq s sym)))))
  t)

(deftest do-all-symbols.4
  (let ((x :bad))
    (do-all-symbols (x x)))
  nil)

(deftest do-all-symbols.5
  (block nil
    (do-all-symbols (x (return :bad)))
    :good)
  :good)

(deftest do-all-symbols.6
  (do-all-symbols (x :bad) (return :good))
  :good)

(deftest do-all-symbols.7
  (block done
    (tagbody
     (do-all-symbols (x (return-from done :good))
       (go 1)
       (return-from done :bad1)
       1)
     1
     (return-from done :bad2)))
  :good)

(deftest do-all-symbols.8
  (block done
    (tagbody
     (do-all-symbols (x (return-from done :good))
       (go tag)
       (return-from done :bad1)
       tag)
     tag
     (return-from done :bad2)))
  :good)

;;; Test that do-all-symbols accepts declarations

(deftest do-all-symbols.9
  (let ((x 0)
	(y 1))
    (do-all-symbols (z nil)
      (declare (type (integer * 0) x))
      (declare (type (integer 1 *) y))
      (declare (ignore z))
      (when (< x y) (return :good))))
  :good)


;;; Default return is NIL

(deftest do-all-symbols.10
  (do-all-symbols (s) (declare (ignore s)))
  nil)

;;; Free declaration scope tests

(deftest do-all-symbols.11
  (let ((x :good))
    (declare (special x))
    (let ((x :bad))
      (do-all-symbols (s x)
	(declare (special x)))))
  :good)

;;; Executing a return actually terminates the loop

(deftest do-all-symbols.12
  (let ((should-have-returned nil))
    (block done
      (do-all-symbols (s :bad1)
	(when should-have-returned
	  (return-from done :bad2))
	(setq should-have-returned t)
	(return :good))))
  :good)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest do-all-symbols.13
  (macrolet
   ((%m (z) z))
   (do-all-symbols (s (expand-in-current-env (%m :good)))))
  :good)
