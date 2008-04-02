;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Dec  4 18:59:27 2004
;;;; Contains: Macro for pattern matching on S-exprs

(in-package :cl-test)

(defmacro pmatch (pattern form)
  (cond
   ((consp pattern)
    (let ((pcar (car pattern))
	  (pcdr (cdr pattern))
	  (v (gensym)))
      (case pcar
	((:or)
	 `(let ((,v ,form)) (or ,@(mapcar (lambda (sub) `(pmatch ,sub ,v))
					  pcdr))))
	((:and)
	 `(let ((,v ,form)) (and ,@(mapcar (lambda (sub) `(pmatch ,sub ,v))
					   pcdr))))
	((:not)
	 (assert (eql (length pcdr) 1))
	 `(not (pmatch ,(car pcdr) ,form)))
	(t
	 `(let ((,v ,form))
	    (and (pmatch ,pcar (car ,v))
		 (pmatch ,pcdr (cdr ,v))))))))
   ((eql pattern '_) t)
   ((null pattern)
    `(null ,form))
   ((symbolp pattern)
    `(eql (quote ,pattern) ,form))
   (t
    `(eql ,pattern ,form))))

(defmacro matchcase (form &body cases)
  (let* ((v (gensym))
	 (cond-cases
	  (mapcar
	   #'(lambda (case)
	       (assert (consp case))
	       (let ((pattern (car case))
		     (body (cdr case)))
		 `((pmatch ,pattern ,v) ,@body)))
	   cases)))
    `(let ((,v ,form))
       (cond ,@cond-cases))))

(defmacro matchcase* (form &body cases)
  (let* ((block-name (gensym "DONE"))
	 (v (gensym)))
    `(block ,block-name
       (let ((,v ,form))
	 (cond
	  ,@(mapcar
	     #'(lambda (case)
		 (assert (consp case))
		 (let ((pat (car case))
		       (forms (cdr case))
		       (fail-name (gensym "FAIL")))
		   `((block ,fail-name
		       (and (pmatch ,pat ,v)
			    (macrolet ((fail () '(return-from ,fail-name nil)))
			      (return-from ,block-name
				(progn ,@forms))))))))
	     cases))))))

		
