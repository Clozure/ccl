;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jun 21 20:59:17 2004
;;;; Contains: Aux. functions for package tests

(in-package :cl-test)

(defmacro test-with-package-iterator (package-list-expr &rest symbol-types)
  "Build an expression that tests the with-package-iterator form."
  (let ((name (gensym))
	(cht-var (gensym))
	(pkg-list-var (gensym)))
    `(let ((,cht-var (make-hash-table))
	   (,pkg-list-var ,package-list-expr)
	   (fail-count 0))
	 (with-package-iterator (,name ,pkg-list-var
				       ,@(copy-list symbol-types))
	   ;; For each symbol, check that name is returning appropriate
	   ;; things
	   (loop
	     (block fail
	       (multiple-value-bind (more sym access pkg)
		   (,name)
		 (unless more (return nil))
		 (setf (gethash sym ,cht-var) t)  ;; note presence of symbol
		 ;; Check that its access status is in the list,
		 ;;  that pkg is a package,
		 ;;  that the symbol is in the package,
		 ;;  and that (in the package) it has the correct access type
		 (unless (member access (quote ,(copy-list symbol-types)))
		   (unless (> fail-count +fail-count-limit+)
		     (format t "Bad access type: ~S ==> ~A~%" sym access))
		   (when (= fail-count +fail-count-limit+)
		     (format t "Further messages suppressed~%"))
		   (incf fail-count)
		   (return-from fail nil))
		 
		 (unless (packagep pkg)
		   (unless (> fail-count +fail-count-limit+)
		     (format t "Not a package: ~S ==> ~S~%" sym pkg))
		   (when (= fail-count +fail-count-limit+)
		     (format t "Further messages suppressed~%"))
		   (incf fail-count)
		   (return-from fail nil))
		 (multiple-value-bind (sym2 access2)
		     (find-symbol (symbol-name sym) pkg)
		   (unless (or (eqt sym sym2)
			       (member sym2 (package-shadowing-symbols pkg)))
		     (unless (> fail-count +fail-count-limit+)
		       (format t "Not same symbol: ~S ~S~%" sym sym2))
		     (when (= fail-count +fail-count-limit+)
		       (format t "Further messages suppressed~%"))
		     (incf fail-count)
		     (return-from fail nil))
		   (unless  (eqt access access2)
		     (unless (> fail-count +fail-count-limit+)
		       (format t "Not same access type: ~S ~S ~S~%"
			       sym access access2))
		     (when (= fail-count +fail-count-limit+)
		       (format t "Further messages suppressed~%"))
		     (incf fail-count)
		     (return-from fail nil)))))))
	 ;; now, check that each symbol in each package has
	 ;; been properly found
	 (loop
	     for p in ,pkg-list-var do
	       (block fail
		 (do-symbols (sym p)
		   (multiple-value-bind (sym2 access)
		       (find-symbol (symbol-name sym) p)
		     (unless (eqt sym sym2)
		       (unless (> fail-count +fail-count-limit+)
			 (format t "Not same symbol (2): ~S ~S~%"
				 sym sym2))
		       (when (= fail-count +fail-count-limit+)
			 (format t "Further messages suppressed~%"))
		       (incf fail-count)
		       (return-from fail nil))
		     (unless (or (not (member access
					      (quote ,(copy-list symbol-types))))
				 (gethash sym ,cht-var))
		       (format t "Symbol not found: ~S~%" sym)
		       (incf fail-count)
		       (return-from fail nil))))))
	 (or (zerop fail-count) fail-count))))

(defun with-package-iterator-internal (packages)
  (test-with-package-iterator packages :internal))

(defun with-package-iterator-external (packages)
  (test-with-package-iterator packages :external))

(defun with-package-iterator-inherited (packages)
  (test-with-package-iterator packages :inherited))

(defun with-package-iterator-all (packages)
  (test-with-package-iterator packages :internal :external :inherited))

(defun num-external-symbols-in-package (p)
  (let ((num 0))
    (declare (fixnum num))
    (do-external-symbols (s p num)
      (declare (ignorable s))			 
      (incf num))))

(defun external-symbols-in-package (p)
  (let ((symbols nil))
    (do-external-symbols (s p)
      (push s symbols))
    (sort symbols #'(lambda (s1 s2) (string< (symbol-name s1)
					     (symbol-name s2))))))

(defun num-symbols-in-package (p)
  (let ((num 0))
    (declare (fixnum num))
    (do-symbols (s p num)
      (declare (ignorable s))			 
      (incf num))))

(defun sort-symbols (sl)
  (sort (copy-list sl)
	#'(lambda (x y)
	    (or
	     (string< (symbol-name x)
		      (symbol-name y))
	     (and (string= (symbol-name x)
			   (symbol-name y))
		  (string< (package-name (symbol-package x))
			   (package-name (symbol-package y))))))))

(defun sort-package-list (x)
  (sort (copy-list x)
	#'string<
	:key #'package-name))
