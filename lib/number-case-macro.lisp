;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2009 Clozure Associates
;;;   Copyright (C) 1994-2001 Digitool, Inc
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

(in-package "CCL")

;;;;;;;;;
;; support fns and vars for number-case

;;; We don't recognize a very large set of type specifiers - just
;;; a few symbols that can be mapped to primitive types.  (Not
;;; all of those symbols are necessarily valid type specifiers!)
(defun type-name-to-code (name)
  (let* ((arch (backend-target-arch *target-backend*)))
    (case name
      (fixnum (arch::target-fixnum-tag arch))
      ((short-float single-float) (arch::target-single-float-tag arch))
      ((double-float long-float) (arch::target-double-float-tag arch))
      (t (let* ((key (case name
                       (ratio :ratio)
                       (bignum :bignum)
                       (complex-rational :complex)
                       (complex-single-float :complex-single-float)
                       (complex-double-float :complex-double-float))))
           (or (and key (cdr (assoc key (arch::target-uvector-subtags arch))))
               (error "Can't determine typecode of ~s for architecture ~a."
                      name (arch::target-name arch))))))))

(defparameter nd-onions `((integer fixnum bignum) (rational fixnum bignum ratio)
                          (float double-float single-float)
                          (real fixnum bignum ratio double-float single-float)
                          (complex complex-single-float complex-double-float complex-rational)
                          (number fixnum bignum ratio double-float short-float complex-single-float complex-double-float complex-rational)))


;;; This is just used to generate more succint error messages.
(defparameter canonical-numeric-union-types
  '((integer fixnum bignum)
    (rational integer ratio)
    (float short-float double-float)
    (float single-float double-float) 
    (float short-float long-float)
    (float single-float long-float)
    (real float rational)
    (complex complex-single-float complex-double-float complex-rational)
    (number real complex)))

(defun nd-diff (x y) ; things in x that are not in y
  (let ((res))
    (dolist (e x)
      (when (not (memq e y))(push e res)))
    res))


;;; Try to use canonical names for things that're effectively
;;; implicit unions.  Rinse.  Repeat.
(defun nd-type-compose (selectors)
  (dolist (union canonical-numeric-union-types)
    (destructuring-bind  (u &rest members) union
      (if (not (memq u selectors))
        (when (dolist (m members t)
              (unless (memq m selectors)
                (return nil)))
          (push u selectors)))
      (when (memq u selectors)
        (dolist (m members)
          (setq selectors (delete m selectors))))))
  (setq selectors
        (nsubst '(complex single-float) 'complex-single-float
                (nsubst '(complex-double-float) 'complex-double-float
                        (nsubst '(complex rational) 'complex-rational
                                selectors))))
  (if (cdr selectors)
    (cons 'or selectors)
    (car selectors)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Simpler number dispatch. Syntax is just like case.
;;
;; (number-case x                 =>         (case (typecode x)
;;     (fixnum (print 4))		        (target::tag-fixnum (print 4)) ; actually tag value
;;     ((bignum ratio)(print 5)))		((target::tag-bignum target::tag-ratio)(print 5))
;;	                      			(t (require-type x 'rational)))) 
;;						  

(defmacro number-case (var &rest cases)
  (let ((selectors-so-far)
        (t-case nil)
        (tag (gensym))
        (block (gensym)))
    (flet ((maybe-compound (selector)
             (let ((compound (cdr (assq selector nd-onions))))
               (when compound
                 (setq compound (nd-diff compound selectors-so-far))
                 (when (not compound)(error "Unreachable case ~s" selector))
                 (setq selectors-so-far
                       (append compound selectors-so-far))
                 compound))))
      (declare (dynamic-extent #'maybe-compound))
      `(block ,block
         (tagbody 
           ,tag
           (return-from ,block              
             (case (typecode ,var)
               ,@(mapcar 
                  #'(lambda (case)
                      (let ((selector (car case)))
                        (if (atom selector)
                          (cond ((eq selector t)(setq t-case t))
                                ((memq selector selectors-so-far)(error "Unreachable case ~s" selector))
                                ((let ((compound (maybe-compound selector)))
                                   (when compound
                                     (setq selector compound))))
                                (t (push selector selectors-so-far)))
                          (progn
                            (setq selector
                                  (mapcan #'(lambda (item)
                                              (cond ((memq item selectors-so-far))
                                                    ((let ((compound (maybe-compound item)))
                                                       (when compound
                                                         (setq item compound))))
                                                    (t (push item selectors-so-far)))
                                              (if (listp item) item (list item)))
                                          selector))))
                        (setq selector (if (listp selector)
                                         (mapcar #'type-name-to-code selector)
                                         (if (eq selector t) t
                                             (type-name-to-code selector))))
                        `(,selector ,@(cdr case))))
                  cases)
               ,@(if (not t-case)
                   `((t (setq ,var (%kernel-restart $xwrongtype ,var ',(nd-type-compose selectors-so-far)))
                        (go ,tag)))))))))))

(provide "NUMBER-CASE-MACRO")
