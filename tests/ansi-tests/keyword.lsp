;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Feb 22 06:53:55 2004
;;;; Contains: Tests of the KEYWORD package

(in-package :cl-test)

;; Check that each keyword satisfies keywordp

(deftest keyword.1
  (do-symbols (s "KEYWORD" t)
    (unless (keywordp s)
      (return (list s nil))))
  t)

;; Check that symbols that are interned in the KEYWORD
;; package, but do not have KEYWORD as their home package,
;; are in fact keywords.
;;
;; This came up on the #lisp irc channel

;;;
;;; The following two tests are improper, since (see the page for SYMBOL)
;;; "The consequences are undefined if an attempt is made to alter the home
;;;  package of a symbol external in the COMMON-LISP package or the KEYWORD package."
;;;
;;; They could be rewritten to search for a name that is not interned in KEYWORD.
;;;

#|
(deftest keyword.4
  (let ((name "SYMBOL-NAME-FOR-KEYWORD.4")
	(kwp (find-package "KEYWORD")))
    (let ((s (find-symbol name kwp)))
      (when s (unintern s kwp))
      ;; Now, create a symbol with this name
      ;; and import it into the keyword package
      (setq s (make-symbol name))
      (import s kwp)
      ;; Check that it's a keyword
      (values
       (eqlt (symbol-package s) kwp)
       (eqlt (find-symbol name kwp) s)
       (nth-value 1 (find-symbol name kwp))
       (notnot (typep s 'keyword))
       (if (boundp s) (eqlt s (symbol-value s)) :not-bound)
       (notnot (constantp s)))))
  t t :external t t t)

(deftest keyword.5
  (let* ((name "SYMBOL-NAME-FOR-KEYWORD.5")
	 (pkg-name "PACKAGE-FOR-KEYWORD.5")
	 (kwp (find-package "KEYWORD")))
    (safely-delete-package pkg-name)
    (let* ((pkg (make-package pkg-name :use nil))
	   (s (find-symbol name kwp)))
      (when s (unintern s kwp))
      ;; Now, create a symbol with this name
      ;; and import it into the keyword package
      (setq s (intern name pkg))
      (import s kwp)
      ;; Check that it's a keyword
      (values
       (eqlt (symbol-package s) pkg)
       (eqlt (find-symbol name kwp) s)
       (nth-value 1 (find-symbol name kwp))
       (notnot (typep s 'keyword))
       (if (boundp s) (eqlt s (symbol-value s)) :not-bound)
       (notnot (constantp s)))))
  t t :external t t t)

(deftest keyword.6
  (let* ((name "SYMBOL-NAME-FOR-KEYWORD.6")
	 (pkg-name "PACKAGE-FOR-KEYWORD.6")
	 (kwp (find-package "KEYWORD")))
    (safely-delete-package pkg-name)
    (let* ((pkg (make-package pkg-name :use nil))
	   (s (find-symbol name kwp))
	   s2)
      (when s (unintern s kwp))
      ;; Recreate a symbol with this name in the keyword package
      ;; shadowing-import will displace this symbol
      (setq s2 (intern name kwp))
      ;; Now, create a symbol with this name
      ;; and shadowing-import it into the keyword package
      (setq s (intern name pkg))
      (shadowing-import s kwp)
      ;; Check that it's a keyword
      (values
       (eqt s s2)
       (symbol-package s2)
       (eqlt (symbol-package s) pkg)
       (eqlt (find-symbol name kwp) s)
       (nth-value 1 (find-symbol name kwp))
       (notnot (typep s 'keyword))
       (if (boundp s) (eqlt s (symbol-value s)) :not-bound)
       (notnot (constantp s)))))
  nil nil t t :external t t t)
|#


;;; Note that the case of a symbol inherited into KEYWORD cannot arise
;;; standardly from user actions, since USE-PACKAGE disallows KEYWORD
;;; as the package designated by its second argument.

;; Every keyword is external
(deftest keyword.2
  (do-symbols (s "KEYWORD" t)
    (multiple-value-bind (s2 access)
	(find-symbol (symbol-name s) "KEYWORD")
      (unless (and (eqt s s2)
		   (eqt access :external))
	(return (list s2 access)))))
  t)

;; Every keyword evaluates to itself
(deftest keyword.3
  (do-symbols (s "KEYWORD" t)
    (cond
     ((not (boundp s))
      (return (list s "NOT-BOUND")))
     ((not (eqt s (eval s)))
      (return (list s (eval s))))))
  t)

    

