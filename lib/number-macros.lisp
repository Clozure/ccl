;;;-*- Mode: Lisp; Package: CCL -*-
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

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "LISPEQU")
  )

(declare-arch-specific-macro %make-sfloat)

(declare-arch-specific-macro %make-dfloat)

(defmacro require-null-or-double-float-sym (sym)
  (setq sym (require-type sym 'symbol))
  `(when (and ,sym (not (double-float-p ,sym)))
     (setq ,sym (require-type ,sym 'double-float))))


(declare-arch-specific-macro %numerator)

(declare-arch-specific-macro %denominator)

(declare-arch-specific-macro %realpart)

(declare-arch-specific-macro %imagpart)


(defmacro with-stack-double-floats (specs &body body)
  (collect ((binds)
            (inits)
            (names))
    (dolist (spec specs)
      (let ((name (first spec)))
        (binds `(,name (%make-dfloat)))
        (names name)
        (let ((init (second spec)))
          (when init
            (inits `(%double-float ,init ,name))))))
    `(let* ,(binds)
      (declare (dynamic-extent ,@(names))
               (double-float ,@(names)))
      ,@(inits)
      ,@body)))






 ;;; WITH-BIGNUM-BUFFERS  --  Internal.
  ;;;
  ;;; Could do freelisting someday. NAH
  ;;;
(defmacro with-bignum-buffers (specs &body body)  ; <<
  "WITH-BIGNUM-BUFFERS ({(var size [init])}*) Form*"
  (collect ((binds)
	    (inits)
	    (names))
    (dolist (spec specs)
      (let ((name (first spec))
            (size (second spec)))
        (binds `(,name (allocate-typed-vector :bignum ,size)))
        (names name)          
        (let ((init (third spec)))
          (when init
            (inits `(bignum-replace ,name ,init))))))
    `(let* ,(binds)
       (declare (dynamic-extent ,@(names)))
       ,@(inits)
       ,@body)))

;;; call fn on possibly stack allocated negative of a and/or b
;;; args better be vars - we dont bother with once-only
(defmacro with-negated-bignum-buffers (a b fn)
  `(let* ((len-a (%bignum-length ,a))
          (len-b (%bignum-length ,b))
          (a-plusp (bignum-plusp ,a))
          (b-plusp (bignum-plusp ,b)))
     (declare (type bignum-index len-a len-b))
     (if (and a-plusp b-plusp)
       (,fn ,a ,b )
       (if (not a-plusp)
         (with-bignum-buffers ((a1 (1+ len-a)))
           (negate-bignum ,a nil a1)
           (if b-plusp
             (,fn a1 ,b)
             (with-bignum-buffers ((b1 (1+ len-b)))
               (negate-bignum ,b nil b1)
               (,fn a1 b1))))
         (with-bignum-buffers ((b1 (1+ len-b)))
           (negate-bignum ,b nil b1)
           (,fn ,a b1))))))

(defmacro with-one-negated-bignum-buffer (a fn)
  `(if (bignum-plusp ,a)
    (,fn ,a)
    (with-bignum-buffers ((a1 (1+ (%bignum-length ,a))))
      (negate-bignum ,a nil a1)
      (,fn a1))))


(defmacro fixnum-to-bignum-set (big fix)
  `(%fixnum-to-bignum-set ,big ,fix))

(defmacro with-small-bignum-buffers (specs &body body)
  (collect ((binds)
	    (inits)
	    (names))
    (dolist (spec specs)
      (let ((name (first spec)))
	(binds `(,name (allocate-typed-vector :bignum
                        ,(target-word-size-case (32 1)
                                                (64 2)))))
                        
	(names name)
	(let ((init (second spec)))
	  (when init
	    (inits `(fixnum-to-bignum-set ,name ,init))))))
    `(let* ,(binds)
      (declare (dynamic-extent ,@(names)))
      ,@(inits)
      ,@body)))

(provide "NUMBER-MACROS")

;;; end of number-macros.lisp
