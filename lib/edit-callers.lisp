; -*- Mode:Lisp; Package:CCL; -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   This file is part of OpenMCL.  
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

; edit-callers.lisp

(in-package "CCL")

(defun global-function-p (random &optional name)
  (let* ((thing random)
         (name (or name (ignore-errors (function-name thing)))))
    (and name
         (or (not (or (symbolp name)(and (consp name)(eq (car name) 'setf)))) ; maybe its (setf baz)
             (let ((fn  (fboundp name)))
               (and fn
                    (progn
		; maybe this is enough for both cases?
                      (or (eq thing fn)
                          (and (symbolp name)(eq thing (macro-function name))))))))
         name)))

(defvar *function-parent-table* nil)
(defvar *function-parent-pool* (%cons-pool))

(defun copying-gc-p () ; if nz copying gc is on
  nil)

(defun lfun-closure-p (lfun)
  (logbitp $lfbits-trampoline-bit (lfun-bits lfun)))

; make a macro ?
(defun puthash-parent (im fun)
  (when (functionp im) ; was (or (functionp im)(eq imtype $sym.fapply))
    (if (global-function-p fun)
      (setf (gethash im *function-parent-table*) fun)
      (let ((ht (gethash im *function-parent-table*)))
        (if (not ht)
          (setf (gethash im *function-parent-table*) fun)
          (unless (eq ht fun)
            (if (consp ht)
              (when (not (memq fun ht))(nconc ht (list fun)))
              (if (not (global-function-p ht))
                (setf (gethash im *function-parent-table*) (list ht fun))))))))))       


(defun callers (function &aux cfun callers gccount retry)
  ;(declare (special cfun function callers))
  (declare (optimize (speed 3)(safety 0)))

  (let ((*function-parent-table* nil))
    (if (and (symbolp function) (fboundp function))
      (setq cfun (symbol-function function)))
    (if (and (consp function)(eq (car function) 'setf))
      (let ((nm (cadr function)))
        (setq function  (or (%setf-method nm)
                            (and (symbolp nm)
                                 (setq nm (setf-function-name nm))
                                 (fboundp nm)
                                 nm)
                            function))))  
    (when (copying-gc-p) (setq gccount (full-gccount)))
    (flet ((do-it (fun)
                                        ;(declare (special fun))
             (when (and gccount (neq gccount (full-gccount)))
               (throw 'losing :lost))
             (let ((bits (lfun-bits fun)))
               (declare (fixnum bits))
               (unless (or (and (logbitp $lfbits-cm-bit bits)(not (logbitp $lfbits-method-bit bits))) ; combined method
                           (and (logbitp $lfbits-trampoline-bit bits)(lfun-closure-p fun)
                                (not (global-function-p fun)))) ; closure (interp or compiled)
                 (let* ((nm (ignore-errors (lfun-name fun)))
                            (globalp (if nm (global-function-p fun nm))))
                       (flet ((do-imm (im)
                                (when (and (or (eq function im)
                                               (and cfun (eq cfun im)))
                                           (neq im nm))                             
                                  (push fun callers)) 
                                (when (functionp im) ; was (or (functionp im)(eq imtype $sym.fapply))
                                  (if globalp
                                    (setf (gethash im *function-parent-table*) fun)
                                    (let ((ht (gethash im *function-parent-table*)))
                                      (if (not ht)
                                        (setf (gethash im *function-parent-table*) fun)
                                        (unless (eq ht fun)
                                          (if (consp ht)
                                            (when (not (memq fun ht))(nconc ht (list fun)))
                                            (if (not (global-function-p ht))
                                              (setf (gethash im *function-parent-table*) 
                                                    (list ht fun)))))))))))
                         (declare (dynamic-extent #'do-imm))                                
                         (%map-lfimms fun #'do-imm )))))))
      (declare (dynamic-extent #'do-it))
      (unwind-protect
           (progn
             (let* ((pool *function-parent-pool*)
                    (tbl (pool.data pool)))
               (setf (pool.data pool) nil
                     *function-parent-table*
                     (if tbl
                       (clrhash tbl)
                       (make-hash-table :size 700 :test 'eq :weak :value))))
             (loop
               (cond ((eq :lost (catch 'losing      
                                  (%map-lfuns #'do-it)))
                      (when retry (error "Callers is losing"))
                      (setq callers nil)
                      (setq retry t))
                     (t (return))))
             (delete-if #'(lambda (thing)
                            (or (functionp thing)
                                (and (typep thing 'method)
                                     (let ((gf (fboundp (method-name thing))))
                                       (not (and (typep gf 'standard-generic-function)
                                                 (memq thing (%gf-methods gf))))))))
                        (delete-duplicates (mapcar 'top-level-caller callers))))
        (setf (pool.data *function-parent-pool*) *function-parent-table*
              *function-parent-table* nil)))))



(defun top-level-caller (function &optional the-list)
  (or (global-function-p function)
      (pascal-function-p function)
      (let ((name (function-name function)))
        (and name (function-encapsulation name) name))
      (let ((caller function) next)
        (loop
          (setq next (gethash caller *function-parent-table*))
          (if  next           
            (cond ((consp next)
                   (when (null the-list)(push function the-list))
                   (return
                    (dolist (c next)
                      (when (not (memq c the-list))
                        (let ((res (top-level-caller c the-list)))
                          (when (and res (not (functionp res)))
                            (return res)))))))
                  (t (let ((res (global-function-p next)))
                       (when res (return res)))
                     (when (null the-list)(push function the-list))
                     (when (memq next the-list) (return))
                     (push next the-list)
                     (setq caller next)))
            (return caller))))
      function))

; in 3.x the function in pascal-functions calls the actual function
(defun pascal-function-p (function)
  (if (find function %pascal-functions%
            :test #'eq
            :key #'(lambda (elt)
                     (if (consp elt)
                       (let ((one (cdr elt)))
                         (when (and (eq (function-name one)(function-name function))
                                    (block blob
                                      (%map-lfimms one #'(lambda (imm)
                                                           (when (eq imm function)
                                                             (return-from blob function))))))
                           function))
                       (if elt (aref elt 2)))))
    (function-name function)))





                  









;;; Calls function f with args (imm) on each immediate in lfv.

(defun %map-lfimms (function-object f)
  (let* ((lfv (function-to-function-vector function-object))
         (n (- (uvsize lfv) 2)))
    (declare (fixnum n))
    #+ppc-target
    (dotimes (i n)
      (funcall f (%svref lfv (%i+ 1 i))))
    #+x86-target
    (do* ((i (1- (the fixnum (%function-code-words function-object))) (1+ i)))
         ((= i n))
      (declare (fixnum i))
      (funcall f (%svref lfv (%i+ 1 i))))
    ))
         
    


(provide :edit-callers)
