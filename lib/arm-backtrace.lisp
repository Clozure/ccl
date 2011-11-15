;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2010 Clozure Associates
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

(defun cfp-lfun (p)
  (if (fake-stack-frame-p p)
    (let* ((fn (%fixnum-ref p arm::fake-stack-frame.fn))
           (lr (%fixnum-ref p arm::fake-stack-frame.lr)))
      (if (and (typep fn 'function)
               (typep lr 'fixnum))
        (values fn lr)
        (values nil nil)))
    (if (and (typep p 'fixnum)
             (lisp-frame-p p nil))
      (%cfp-lfun p))))

(defun catch-csp-p (p context)
  (let ((catch (if context
                 (bt.top-catch context)
                 (%catch-top (%current-tcr)))))
    (loop
      (when (null catch) (return nil))
      (let ((sp (catch-frame-sp catch)))
        (when (eql sp p)
          (return t)))
      (setq catch (next-catch catch)))))

(defun %stack< (index1 index2 &optional context)
  (let* ((tcr (if context (bt.tcr context) (%current-tcr)))
         (cs-area (%fixnum-ref tcr target::tcr.cs-area)))
    (and (%ptr-in-area-p index1 cs-area)
         (%ptr-in-area-p index2 cs-area)
         (< (the fixnum index1) (the fixnum index2)))))

(defun registers-used-by (lfun &optional at-pc)
  (declare (ignore lfun at-pc))
  (values nil nil))

(defun exception-frame-p (f)
  (fake-stack-frame-p f))


;;; Used for printing only.
(defun index->address (p)
  (when (fake-stack-frame-p p)
    (setq p (%fixnum-ref p arm::fake-stack-frame.sp)))
  (ldb (byte  32 0)  (ash p arm::fixnumshift)))

(defun %raw-frame-ref (cfp context idx bad)
  (declare (fixnum idx))
  (multiple-value-bind (frame base)
      (vsp-limits cfp context)
    (let* ((raw-size (- base frame)))
      (declare (fixnum frame base raw-size))
      (if (and (>= idx 0)
               (< idx raw-size))
        (let* ((addr (- (the fixnum (1- base))
                        idx)))
          (multiple-value-bind (db-count first-db last-db)
              (count-db-links-in-frame frame base context)
            (let* ((is-db-link
                    (unless (zerop db-count)
                      (do* ((last last-db (previous-db-link last first-db)))
                           ((null last))
                        (when (= addr last)                          (return t))))))
              (if is-db-link
                (oldest-binding-frame-value context addr)
                (%fixnum-ref addr)))))
        bad))))

;;; Return two values: the vsp of p and the vsp of p's "parent" frame.
;;; The "parent" frame vsp might actually be the end of p's segment,
;;; if the real "parent" frame vsp is in another segment.
(defun vsp-limits (p context)
  (let* ((vsp (%frame-savevsp p))
         parent)
    (when (eql vsp 0)
      ; This frame is where the code continues after an unwind-protect cleanup form
      (setq vsp (%frame-savevsp (child-frame p context))))
    (flet ((grand-parent (frame)
             (let ((parent (parent-frame frame context)))
               (when (and parent (eq parent (%frame-backlink frame context)))
                 (let ((grand-parent (parent-frame parent context)))
                   (when (and grand-parent (eq grand-parent (%frame-backlink parent context)))
                     grand-parent))))))
      (declare (dynamic-extent #'grand-parent))
      (let* ((frame p)
             grand-parent)
        (loop
          (setq grand-parent (grand-parent frame))
          (when (or (null grand-parent) (not (eql 0 (%frame-savevsp grand-parent))))
            (return))
          (setq frame grand-parent))
        (setq parent (parent-frame frame context)))
      (let* ((parent-vsp (if parent (%frame-savevsp parent) vsp))
             (tcr (if context (bt.tcr context) (%current-tcr)))
             (vsp-area (%fixnum-ref tcr target::tcr.vs-area)))
        (if (eql 0 parent-vsp)
          (values vsp vsp)              ; p is the kernel frame pushed by an unwind-protect cleanup form
          (progn
            (unless vsp-area
              (error "~s is not a stack frame pointer for context ~s" p tcr))
            (unless (%ptr-in-area-p parent-vsp vsp-area)
              (setq parent-vsp (%fixnum-ref vsp-area target::area.high)))
            (values vsp parent-vsp)))))))

(defun %frame-savevsp (p)
  (if (fake-stack-frame-p p)
    (%fixnum-ref p arm::fake-stack-frame.vsp)
    (%%frame-savevsp p)))

;;; Lexprs ?
(defun arg-check-call-arguments (frame function)
  (declare (ignore function))
  (xp-argument-list (%fixnum-ref frame arm::fake-stack-frame.xp)))

;;; Should never be called.
(defun %find-register-argument-value (context csp regval bad)
  (declare (ignore context csp regval))
  bad)

;;; Shouldn't be called.
(defun %set-register-argument-value (context csp regval new)
  (declare (ignore context csp regval))
  new)

(defun %raw-frame-set (frame context idx new)
  (declare (fixnum frame idx))
  (let* ((base (parent-frame frame context))
         (raw-size (- base frame)))
    (declare (fixnum base raw-size))
    (if (and (>= idx 0)
             (< idx raw-size))
      (let* ((addr (- (the fixnum (1- base))
                      idx)))
        (multiple-value-bind (db-count first-db last-db)
            (count-db-links-in-frame frame base context)
          (let* ((is-db-link
                  (unless (zerop db-count)
                    (do* ((last last-db (previous-db-link last first-db)))
                         ((null last))
                      (when (= addr last)
                        (return t))))))
            (if is-db-link
              (setf (oldest-binding-frame-value context addr) new)
              (setf (%fixnum-ref addr) new))))))))

(defun match-local-name (cellno info pc)
  (when info
    (let* ((syms (%car info))
           (ptrs (%cdr info)))
      (dotimes (i (length syms))
        (let ((j (%i+ i (%i+ i i ))))
          (and (eq (uvref ptrs j) (%ilogior (%ilsl (+ 6 target::word-shift) cellno) #o77))
               (%i>= pc (uvref ptrs (%i+ j 1)))
               (%i< pc (uvref ptrs (%i+ j 2)))
               (return (aref syms i))))))))

(defun apply-in-frame (frame function arglist &optional context)
  (declare (ignore frame function arglist context))
  (error "APPLY-IN-FRAME isn't implemented on ARM."))

(defun return-from-frame (frame &rest values)
  (apply-in-frame frame #'values values nil))
