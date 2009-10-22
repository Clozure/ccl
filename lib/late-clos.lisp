;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2007-2009 Clozure Associates and contributors.
;;;   This file is part of Clozure CL.  
;;;
;;;   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with Clozure CL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with Clozure CL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

;;; Maybe compile specialized discriminating code (dcode) for generic
;;; functions, if it seems likely that that might perform better than
;;; the general generic-function-dispatch mechanism.


;;; If the GF accepts a fixed number of arguments, return its
;;; lambda list.
(defun gf-fixed-arg-lambda-list (gf)
  (let* ((lambda-list (generic-function-lambda-list gf)))
    (dolist (arg lambda-list lambda-list)
      (when (member arg lambda-list-keywords)
        (return nil)))))

(defun generate-conformance-test (arg-name specializer)
  (cond ((typep specializer 'eql-specializer)
         `(eql ,arg-name ',(eql-specializer-object specializer)))
        ((eq specializer *t-class*))
        ((typep specializer 'standard-class)
         (let* ((wrapper (gensym)))
           `(let* ((,wrapper (if (= (the fixnum (typecode ,arg-name))
                                    target::subtag-instance)
                               (instance.class-wrapper ,arg-name))))
             (and ,wrapper
              (memq ,specializer (or (%wrapper-cpl ,wrapper)
                                                (%inited-class-cpl
                                                 (%wrapper-class ,wrapper))))))))
        (t `(typep ,arg-name ',(class-name specializer)))))

(defun generate-conformance-clause (args method)
  `((and ,@(mapcar #'generate-conformance-test args (method-specializers method)))
     (funcall ,(method-function method) ,@args)))

;;; Generate code to call the single fixed-arg primary method
;;; defined on GF if all args are conformant, or to call
;;; NO-APPLICABLE-METHOD otherwise.
;;; Note that we can often do better than this for accessor
;;; methods (especially reader methods) as a very late (delivery-time)
;;; optimization.
(defun dcode-for-fixed-arg-singleton-gf (gf)
  (let* ((methods (generic-function-methods gf))
         (method (car methods))
         (args (gf-fixed-arg-lambda-list gf)))
    (when (and method
               args
               (null (cdr methods))
               (null (method-qualifiers method))
               (dolist (spec (method-specializers method))
                 (unless (eq spec *t-class*) (return t))))
      (compile nil
               `(lambda ,args
                 (cond ,(generate-conformance-clause args method)
                       (t (no-applicable-method ,gf ,@args))))))))

(register-non-dt-dcode-function #'dcode-for-fixed-arg-singleton-gf)
