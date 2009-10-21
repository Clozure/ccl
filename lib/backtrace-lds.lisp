;;;-*-Mode: LISP; Package: CCL -*-
;;;
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

;;; backtrace-lds.lisp
;;; low-level support for stack-backtrace dialog (Lisp Development System)

(in-package "CCL")


(defparameter *saved-register-count*
  #+x8632-target 0
  #+x8664-target 4
  #+ppc-target 8)

(defparameter *saved-register-names*
  #+x8632-target nil
  #+x8664-target #(save3 save2 save1 save0)
  #+ppc-target #(save7 save6 save5 save4 save3 save2 save1 save0))

(defun frame-function (frame context)
  "Returns the function using the frame, and pc offset within the function, if known"
  (declare (ignore context))
  (cfp-lfun (require-type frame 'integer)))

(defun frame-supplied-arguments (frame context &key (unknown-marker (%unbound-marker)))
  "Return a list of supplied arguments to the call which opened this frame, as best we can reconstruct it"
  (multiple-value-bind (lfun pc) (cfp-lfun frame)
    (multiple-value-bind (args valid) (supplied-argument-list context frame lfun pc)
      (if (not valid)
        unknown-marker
        (if (eq unknown-marker (%unbound-marker))
          args
          (substitute unknown-marker (%unbound-marker) args))))))

(defun frame-named-variables (frame context &key (unknown-marker (%unbound-marker)))
  "Returns an alist of (NAME . VALUE) of all named variables in this frame."
  (multiple-value-bind (lfun pc) (cfp-lfun frame)
    (multiple-value-bind (args locals) (arguments-and-locals context frame lfun pc unknown-marker)
      (if (eq unknown-marker (%unbound-marker))
        (append args locals)
        (substitute unknown-marker (%unbound-marker) (append args locals))))))


(defun frame-arguments-and-locals (frame context &key unknown-marker)
  "Return two values, the arguments and the locals, known for this frame, as alists of (name . value)"
  (multiple-value-bind (lfun pc) (cfp-lfun frame)
    (arguments-and-locals context frame lfun pc unknown-marker)))

;;; Returns three values: (ARG-VALUES TYPES NAMES), solely for the benefit
;;; of the FRAME-ARGUMENTS function in SLIME's swank-openmcl.lisp.
;;; ARG-VALUES is a list of the values of the args supplied to the function
;;; TYPES is a list of (for bad historical reasons) strings .describing
;;;   whether they're "required", "optional", etc.  SLIME only really
;;;   cares about whether this is equal to "keyword" or not.
;;; NAMES is a list of symbols which name the args.
;; 7/13/2009: This is now deprecated.  Use frame-supplied-arguments.
(defun frame-supplied-args (frame lfun pc child context)
  (declare (ignore child))
  (if (null pc)
    (values nil nil nil)
    (if (<= pc target::arg-check-trap-pc-limit)
      (values (arg-check-call-arguments frame lfun) nil nil)
      (multiple-value-bind (arglist valid) (arglist-from-map lfun)
        (if (not valid)
          (values nil nil nil)
          (let* ((args (arguments-and-locals context frame lfun pc))
                 (state :required))
            (collect ((arg-values)
                      (types)
                      (names))
              (dolist (arg arglist)
                (if (or (member arg lambda-list-keywords)
                        (eq arg '&lexpr))
                  (setq state arg)
                  (let* ((pair (pop args)))
                    (case state
                      (&lexpr
                         (with-list-from-lexpr (rest (cdr pair))
                           (dolist (r rest) (arg-values r) (names nil) (types nil)))
                         (return))
                      (&rest
                         (dolist (r (cdr pair)) (arg-values r) (names nil) (types nil))
                         (return))
                      (&key
                         (arg-values arg)
                         (names nil)
                         (types nil)))
                    (let* ((value (cdr pair)))
                      (if (eq value (%unbound-marker))
                        (return))
                      (names (car pair))
                      (arg-values value)
                      (types nil)))))
              (values (arg-values) (types) (names)))))))))


#|
(setq *save-local-symbols* t)

(defun test (flip flop &optional bar)
  (let ((another-one t)
        (bar 'quux))
    (break)))

(test '(a b c d) #\a)

(defun closure-test (flim flam)
  (labels ((inner (x)
              (let ((ret (list x flam)))
                (break))))
    (inner flim)
    (break)))

(closure-test '(a b c) 'quux)

(defun set-test (a b)
  (break)
  (+ a b))

(set-test 1 'a)

||#


(provide 'backtrace-lds)

; End of backtrace-lds.lisp
