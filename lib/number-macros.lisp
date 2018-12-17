;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

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


(defmacro %realpart (x)
  (let* ((thing (gensym))
         (complex-single-float-tag (nx-lookup-target-uvector-subtag :complex-single-float))
         (complex-double-float-tag (nx-lookup-target-uvector-subtag :complex-double-float)))
    `(let* ((,thing ,x))
      (case (ccl::typecode ,thing)
        (,complex-single-float-tag (ccl::%complex-single-float-realpart ,thing))
        (,complex-double-float-tag (ccl::%complex-double-float-realpart ,thing))
        (t (ccl::%svref ,thing 0))))))


(defmacro %imagpart (x)
  (let* ((thing (gensym))
         (complex-single-float-tag (nx-lookup-target-uvector-subtag :complex-single-float))
         (complex-double-float-tag (nx-lookup-target-uvector-subtag :complex-double-float)))
    `(let* ((,thing ,x))
      (case (ccl::typecode ,thing)
        (,complex-single-float-tag (ccl::%complex-single-float-imagpart ,thing))
        (,complex-double-float-tag (ccl::%complex-double-float-imagpart ,thing))
        (t (ccl::%svref ,thing 1))))))

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

;; 
(defmacro with-temporary-bignum-buffers (specs &body body)
  "WITH-TEMPORARY-BIGNUM-BUFFERS ({(var expr)}* form*"
  (collect ((a-vars)
            (a-lens)
            (b-vars)
            (b-lens)
            (buf-lens)
            (buf-vars)
            (vars))
    (loop for (var (operation a b)) in specs
       do (let ((a-var (if (symbolp a) nil (gensym)))
                (a-len (gensym))
                (b-var (if (symbolp b) nil (gensym)))
                (b-len (gensym))
                (buf-len (gensym))
                (buf-var (gensym)))
            (a-vars (if a-var `((,a-var ,a)) nil))
            (a-lens
             `(,a-len (if (fixnump ,(or a-var a)) ,(target-word-size-case (32 1) (64 2)) (%bignum-length ,(or a-var a)))))
            (b-vars (if b-var `((,b-var ,b)) nil))
            (b-lens
             `(,b-len (if (fixnump ,(or b-var b)) ,(target-word-size-case (32 1) (64 2)) (%bignum-length ,(or b-var b)))))
            (buf-lens `(,buf-len ,@(ecase operation
                                     (+-2-into
                                      `((1+ (max ,a-len ,b-len))))
                                     (--2-into
                                      `((1+ (max ,a-len ,b-len))))
                                     (*-2-into
                                      `((+ (* 2 (ceiling (1+ ,a-len) 2)) (* 2 (ceiling (1+ ,b-len) 2)))))
                                     (gcd-2
                                      `((1+ (min ,a-len ,b-len))))
                                     (truncate-no-rem
                                      `((1+ (max ,(target-word-size-case (32 2) (64 3)) (+ (- ,a-len ,b-len) 2)))))
                                     (maybe-truncate-no-rem
                                      `((1+ (max ,(target-word-size-case (32 2) (64 3)) (+ (- ,a-len ,b-len) 2))))))))
            (buf-vars `(,buf-var (allocate-typed-vector :bignum ,buf-len)))
            (vars `(,var (,operation ,(or a-var a) ,(or b-var b) ,buf-var)))))
    `(let* (,@(loop for a-var in (a-vars)
                 for a-len in (a-lens)
                 for b-var in (b-vars)
                 for b-len in (b-lens)
                 for buf-len in (buf-lens)
                 for buf-var in (buf-vars)
                 for var in (vars)
                 append `(,@a-var ,a-len ,@b-var ,b-len ,buf-len ,buf-var ,var)))
       (declare (type bignum-index ,@(mapcar 'car (a-lens))
                                   ,@(mapcar 'car (b-lens))
                                   ,@(mapcar 'car (buf-lens)))
                (dynamic-extent ,@(mapcar 'car (buf-vars))))
       ,@body)))

;;; call fn on possibly stack allocated negative of a and/or b
;;; args better be vars - we dont bother with once-only
(defmacro with-negated-bignum-buffers (a b fn)
  (let ((a1 (gensym))
        (b1 (gensym)))
    `(let* ((len-a (%bignum-length ,a))
            (len-b (%bignum-length ,b))
            (a-plusp (bignum-plusp ,a))
            (b-plusp (bignum-plusp ,b)))
       (declare (type bignum-index len-a len-b))
       (if (and a-plusp b-plusp)
         (,fn ,a ,b )
         (if (not a-plusp)
           (with-bignum-buffers ((,a1 (1+ len-a)))
             (negate-bignum ,a nil ,a1)
             (if b-plusp
               (,fn ,a1 ,b)
               (with-bignum-buffers ((,b1 (1+ len-b)))
                 (negate-bignum ,b nil ,b1)
                 (,fn ,a1 ,b1))))
           (with-bignum-buffers ((,b1 (1+ len-b)))
             (negate-bignum ,b nil ,b1)
             (,fn ,a ,b1)))))))

(defmacro with-one-negated-bignum-buffer (a fn)
  (let ((a1 (gensym)))
    `(if (bignum-plusp ,a)
       (,fn ,a)
       (with-bignum-buffers ((,a1 (1+ (%bignum-length ,a))))
         (negate-bignum ,a nil ,a1)
         (,fn ,a1)))))


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
