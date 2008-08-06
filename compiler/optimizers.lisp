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

; Optimizers.lisp - compiler optimizers

(in-package "CCL")

(eval-when (eval compile)
  (require'backquote)
  (require'lispequ)
  (require "ARCH"))

(declaim (special *nx-can-constant-fold* *nx-synonyms*))

(defvar *dont-find-class-optimize* nil) ; t means dont

#|
;;; can-constant-fold had a bug in the way it called #'proclaim-inline
|#

;;; There seems to be some confusion about what #'proclaim-inline does.
;;; The value of the alist entry in *nx-proclaimed-inline* indicates
;;; whether or not the compiler is allowed to use any special knowledge
;;; about the symbol in question.  That's a necessary but not sufficient
;;; condition to enable inline expansion; that's governed by declarations
;;; in the compile-time environment.
;;; If someone observed a symptom whereby calling CAN-CONSTANT-FOLD
;;; caused unintended inline-expansion, the bug's elsewhere ...
;;; The bug is that nx-declared-inline-p calls proclaimed-inline-p
;;;  which looks at what proclaim-inline sets.  Presumably, that
;;;  means that someone fixed it because it showed evidence of
;;;  being broken.
;;; The two concepts (the compiler should/should not make assumptions about
;;;  the signature of known functions, the compiler should/should not arrange
;;;  to keep the lambda expression around) need to be sorted out.

(defun can-constant-fold (names &aux handler inlines)
  (dolist (name names)
    (if (atom name)
      (setq handler nil)
      (setq handler (cdr name) name (car name)))
    (when (and handler (not (eq handler 'fold-constant-subforms)))
      (warn "Unknown constant-fold handler : ~s" handler)
      (setq handler nil))
    (let* ((bits (%symbol-bits name)))
      (declare (fixnum bits))
      (%symbol-bits name (logior
                          (if handler (logior (ash 1 $sym_fbit_fold_subforms) (ash 1 $sym_fbit_constant_fold))
                              (ash 1 $sym_fbit_constant_fold))
                          bits)))
     (push name inlines))
  '(apply #'proclaim-inline t inlines)
)

;;; There's a bit somewhere.  This is very partial.  Should be a bit
;;; somewhere, there are too many of these to keep on a list.
(can-constant-fold '(specfier-type %ilsl %ilsr 1- 1+ eql eq
                     byte make-point - / (+ . fold-constant-subforms) (* . fold-constant-subforms) ash character
                     char-code code-char lsh
                     (logior . fold-constant-subforms) (logand . fold-constant-subforms)
                     (logxor . fold-constant-subforms) logcount logorc2 listp consp expt
                     logorc1 logtest lognand logeqv lognor lognot logandc2 logandc1
                     numerator denominator ldb-test byte-position byte-size isqrt gcd
                     floor mod truncate rem round boole max min ldb dpb mask-field deposit-field
                     length aref svref char schar bit sbit getf identity list-length
                     car cdr cadr cddr nth nthcdr last load-byte deposit-byte byte-mask
                     member search count position assoc rassoc integer-length
		         float not null char-int expt abs))

(defun %binop-cassoc (call)
  (unless (and (cddr call) (null (cdr (%cddr call))))
    (return-from %binop-cassoc call))
  (let ((func (%car call))
        (arg1 (%cadr call))
        (arg2 (%caddr call))
        (val))
    (cond ((and (fixnump arg1) (fixnump arg2))
           (funcall func arg1 arg2))
          ((or (fixnump arg1) (fixnump arg2))
           (if (fixnump arg2) (psetq arg1 arg2 arg2 arg1))
           (if (and (consp arg2)
                    (eq (%car arg2) func)
                    (cddr arg2)
                    (null (cdr (%cddr arg2)))
                    (or (fixnump (setq val (%cadr arg2)))
                        (fixnump (setq val (%caddr arg2)))))
             (list func
                   (funcall func arg1 val)
                   (if (eq val (%cadr arg2)) (%caddr arg2) (%cadr arg2)))
             call))
          (t call))))

(defun fixnumify (args op &aux (len (length args)))
  (if (eq len 2)
    (cons op args)
    (list op (%car args) (fixnumify (%cdr args) op))))

(defun generic-to-fixnum-n (call env op &aux (args (%cdr call)) targs)
  (block nil
    (if (and (%i> (length args) 1)
             (and (nx-trust-declarations env)
                  (or (neq op '%i+) (subtypep *nx-form-type* 'fixnum))))
      (if (dolist (arg args t)
            (if (nx-form-typep arg 'fixnum env)
              (push arg targs)
              (return)))
        (return
         (fixnumify (nreverse targs) op))))
    call))

;;; True if arg is an alternating list of keywords and args, only
;;; recognizes keywords in keyword package.  Historical note: this
;;; used to try to ensure that the keyword appeared at most once.  Why
;;; ? (Even before destructuring, pl-search/getf would have dtrt.)
;;; Side effects: it's not the right thing to simply pick the value
;;; associated with the first occurrence of a keyword if the value
;;; associated with subsequent occurrence could have a side-effect.
;;; (We -can- ignore a duplicate key if the associated value is
;;; side-effect free.)
(defun constant-keywords-p (keys)
  (when (plistp keys)
    (do* ((seen ())
          (keys keys (cddr keys)))
         ((null keys) t)
      (let* ((key (car keys)))
        (if (or (not (keywordp key))
                (and (memq key seen)
                     (not (constantp (cadr keys)))))
          (return))
        (push key seen)))))


(defun remove-explicit-test-keyword-from-test-testnot-key (item list keys default alist testonly)
  (if (null keys)
    `(,default ,item ,list)
     (if (constant-keywords-p keys)
        (destructuring-bind (&key (test nil test-p)
                                  (test-not nil test-not-p)
                                  (key nil key-p))
                            keys
          (declare (ignore test-not))
          (if (and test-p
                   (not test-not-p)
                   (or (not key-p)
                       (and (consp key)
                            (consp (%cdr key))
                            (null (%cddr key))
                            (or (eq (%car key) 'function)
                                (eq (%car key) 'quote))
                            (eq (%cadr key) 'identity)))
                   (consp test)
                   (consp (%cdr test))
                   (null (%cddr test))
                   (or (eq (%car test) 'function)
                       (eq (%car test) 'quote)))
            (let* ((testname (%cadr test))
                   (reduced (cdr (assoc testname alist))))
              (if reduced
                `(,reduced ,item ,list)
                `(,testonly ,item ,list ,test))))))))


(defun eql-iff-eq-p (thing env)
  (if (quoted-form-p thing)
    (setq thing (%cadr thing))
    (if (not (self-evaluating-p thing))
        (return-from eql-iff-eq-p
          (or (nx-form-typep thing  'symbol env)
              (nx-form-typep thing 'character env)
              (nx-form-typep thing
                             '(or fixnum
                               #+64-bit-target single-float
                               symbol character
                               (and (not number) (not macptr))) env)))))
  (or (fixnump thing) #+64-bit-target (typep thing 'single-float)
      (symbolp thing) (characterp thing)
      (and (not (numberp thing)) (not (macptrp thing)))))

(defun equal-iff-eql-p (thing env)
  (if (quoted-form-p thing)
    (setq thing (%cadr thing))
    (if (not (self-evaluating-p thing))
      (return-from equal-iff-eql-p
        (nx-form-typep thing
                       '(and (not cons) (not string) (not bit-vector) (not pathname)) env))))
  (not (typep thing '(or cons string bit-vector pathname))))


(defun fold-constant-subforms (call env)
    (let* ((constants nil)
           (forms nil))
      (declare (list constants forms))
      (dolist (form (cdr call))
        (setq form (nx-transform form env))
        (if (numberp form)
          (setq constants (%temp-cons form constants))
          (setq forms (%temp-cons form forms))))
      (if constants
        (let* ((op (car call))
               (constant (if (cdr constants) (handler-case (apply op constants)
                                               (error (c) (declare (ignore c))
                                                      (return-from fold-constant-subforms (values call t))))
                             (car constants))))
          (values (if forms (cons op (cons constant (reverse forms))) constant) t))
        (values call nil))))

;;; inline some, etc. in some cases
;;; in all cases, add dynamic-extent declarations
(defun some-xx-transform (call env)
  (destructuring-bind (func predicate sequence &rest args) call
    (multiple-value-bind (func-constant end-value loop-test)
                         (case func
                           (some (values $some nil 'when))
                           (notany (values $notany t 'when))
                           (every (values $every t 'unless))
                           (notevery (values $notevery nil 'unless)))
      (if args
        (let ((func-sym (gensym))
              (seq-sym (gensym))
              (list-sym (gensym)))
          `(let ((,func-sym ,predicate)
                 (,seq-sym ,sequence)
                 (,list-sym (list ,@args)))
             (declare (dynamic-extent ,func-sym ,list-sym ,seq-sym))
             (some-xx-multi ,func-constant ,end-value ,func-sym ,seq-sym ,list-sym)))
        (let ((loop-function (nx-form-sequence-iterator sequence env)))
          ;; inline if we know the type of the sequence and if
          ;; the predicate is a lambda expression
          ;; otherwise, it blows up the code for not much gain
          (if (and loop-function
                   (function-form-p predicate)
                   (lambda-expression-p (second predicate)))
            (let ((elt-var (gensym)))
              (case func
                (some
                 `(,loop-function (,elt-var ,sequence ,end-value)
                                  (let ((result (funcall ,predicate ,elt-var)))
                                    (when result (return result)))))
                ((every notevery notany)
                 `(,loop-function (,elt-var ,sequence ,end-value)
                                  (,loop-test (funcall ,predicate ,elt-var)
                                              (return ,(not end-value)))))))
            (let ((func-sym (gensym))
                  (seq-sym (gensym)))
              `(let ((,func-sym ,predicate)
                     (,seq-sym ,sequence))
                 (declare (dynamic-extent ,func-sym ,seq-sym))
                 (some-xx-one ,func-constant ,end-value ,func-sym ,seq-sym)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The new (roughly alphabetical) order.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Compiler macros on functions can assume that their arguments have
;;; already been transformed.


(defun transform-real-n-ary-comparision (whole binary-name)
  (destructuring-bind (n0 &optional (n1 0 n1-p) &rest more) (cdr whole)
    (if more
      (if (cdr more)
        whole
        (let* ((n2 (car more))
               (n (gensym)))
          `(let* ((,n ,n0))
            (if (,binary-name ,n (setq ,n ,n1))
              (,binary-name ,n ,n2)))))
      (if (not n1-p)
        `(require-type ,n0 'real)
        `(,binary-name ,n0 ,n1)))))



(define-compiler-macro < (&whole whole &rest ignore)
  (declare (ignore ignore))
  (transform-real-n-ary-comparision whole '<-2))

(define-compiler-macro > (&whole whole &rest ignore)
  (declare (ignore ignore))
  (transform-real-n-ary-comparision whole '>-2))

(define-compiler-macro <= (&whole whole &rest ignore)
  (declare (ignore ignore))
  (transform-real-n-ary-comparision whole '<=-2))

(define-compiler-macro >= (&whole whole &rest ignore)
  (declare (ignore ignore))
  (transform-real-n-ary-comparision whole '>=-2))


(define-compiler-macro 1- (x)
  `(- ,x 1))

(define-compiler-macro 1+ (x)
  `(+ ,x 1))

(define-compiler-macro append  (&whole call
                                       &optional arg0
                                       &rest
                                       (&whole tail
                                               &optional (junk nil arg1-p)
                                               &rest more))
  ;(append (list x y z) A) -> (list* x y z A)
  (if (and arg1-p
           (null more)
           (consp arg0)
           (eq (%car arg0) 'list))
    (cons 'list* (append (%cdr arg0) tail))
    (if (and arg1-p (null more))
      `(append-2 ,arg0 ,junk)
      call)))

(define-compiler-macro apply  (&whole call &environment env fn arg0 &rest args)
  ;; Special-case (apply #'make-instance 'name ...)
  ;; Might be good to make this a little more general, e.g., there
  ;; may be other things that can be strength-reduced even if we can't
  ;; get rid of the APPLY.
  (if (and (consp fn)
           (or (eq (car fn) 'quote)
               (eq (car fn) 'function))
           (consp (cdr fn))
           (null (cddr fn))
           (eq (cadr fn) 'make-instance)
           (consp arg0)
           (eq (car arg0) 'quote)
           (consp (cdr arg0))
           (symbolp (cadr arg0)))
    (let* ((name (cadr arg0))
           (class-cell (gensym)))
      `(let* ((,class-cell (load-time-value (find-class-cell ',name t))))
        (apply (class-cell-instantiate ,class-cell) ,class-cell ,@args)))
    (let ((original-fn fn))
      (if (and arg0
               (null args)
               (consp fn)
               (eq (%car fn) 'function)
               (null (cdr (%cdr fn)))
               (consp (setq fn (%cadr fn)))
               (eq (%car fn) 'lambda))
        (destructuring-bind (lambda-list &body body) (%cdr fn)
          `(destructuring-bind ,lambda-list ,arg0 ,@body))
        (let ((last (%car (last (push arg0 args)))))
          (if (and (consp last) (memq (%car last) '(cons list* list)))
            (cons (if (eq (%car last) 'list) 'funcall 'apply)
                  (cons
                   original-fn
                   (nreconc (cdr (reverse args)) (%cdr last))))
            call))))))



(define-compiler-macro assoc (&whole call item list &rest keys)
  (or (remove-explicit-test-keyword-from-test-testnot-key item list keys 'asseql '((eq . assq) (eql . asseql) (equal . assequal)) 'assoc-test)
      call))

(define-compiler-macro assequal (&whole call &environment env item list)
  (if (or (equal-iff-eql-p item env)
          (and (quoted-form-p list)
               (proper-list-p (%cadr list))
               (every (lambda (x) (equal-iff-eql-p (car x) env)) (%cadr list))))
    `(asseql ,item ,list)
    call))

(define-compiler-macro asseql (&whole call &environment env item list)
  (if (or (eql-iff-eq-p item env)
          (and (quoted-form-p list)
               (proper-list-p (%cadr list))
               (every (lambda (x) (eql-iff-eq-p (car x) env)) (%cadr list))))
    `(assq ,item ,list)
    call))

(define-compiler-macro assq (item list)
  (let* ((itemx (gensym))
         (listx (gensym))
         (pair (gensym)))
    `(let* ((,itemx ,item)
            (,listx ,list))
      (dolist (,pair ,listx)
        (when (and ,pair (eq (car ,pair) ,itemx)) (return ,pair))))))

(define-compiler-macro caar (form)
  `(car (car ,form)))

(define-compiler-macro cadr (form)
  `(car (cdr ,form)))

(define-compiler-macro cdar (form)
  `(cdr (car ,form)))

(define-compiler-macro cddr (form)
  `(cdr (cdr ,form)))

(define-compiler-macro caaar (form)
  `(car (caar ,form)))

(define-compiler-macro caadr (form)
  `(car (cadr ,form)))

(define-compiler-macro cadar (form)
  `(car (cdar ,form)))

(define-compiler-macro caddr (form)
  `(car (cddr ,form)))

(define-compiler-macro cdaar (form)
  `(cdr (caar ,form)))

(define-compiler-macro cdadr (form)
  `(cdr (cadr ,form)))

(define-compiler-macro cddar (form)
  `(cdr (cdar ,form)))

(define-compiler-macro cdddr (form)
  `(cdr (cddr ,form)))

(define-compiler-macro caaaar (form)
  `(car (caaar ,form)))

(define-compiler-macro caaadr (form)
  `(car (caadr ,form)))

(define-compiler-macro caadar (form)
  `(car (cadar ,form)))

(define-compiler-macro caaddr (form)
  `(car (caddr ,form)))

(define-compiler-macro cadaar (form)
  `(car (cdaar ,form)))

(define-compiler-macro cadadr (form)
  `(car (cdadr ,form)))

(define-compiler-macro caddar (form)
  `(car (cddar ,form)))

(define-compiler-macro cadddr (form)
  `(car (cdddr ,form)))

(define-compiler-macro cdaaar (form)
  `(cdr (caaar ,form)))

(define-compiler-macro cdaadr (form)
  `(cdr (caadr ,form)))

(define-compiler-macro cdadar (form)
  `(cdr (cadar ,form)))

(define-compiler-macro cdaddr (form)
  `(cdr (caddr ,form)))

(define-compiler-macro cddaar (form)
  `(cdr (cdaar ,form)))

(define-compiler-macro cddadr (form)
  `(cdr (cdadr ,form)))

(define-compiler-macro cdddar (form)
  `(cdr (cddar ,form)))

(define-compiler-macro cddddr (form)
  `(cdr (cdddr ,form)))




(define-compiler-macro cons (&whole call &environment env x y &aux dcall ddcall)
   (if (consp (setq dcall y))
     (cond
      ((or (eq (%car dcall) 'list) (eq (%car dcall) 'list*))
       ;(CONS A (LIST[*] . args)) -> (LIST[*] A . args)
       (list* (%car dcall) x (%cdr dcall)))
      ((or (neq (%car dcall) 'cons) (null (cddr dcall)) (cdddr dcall))
       call)
      ((null (setq ddcall (%caddr dcall)))
       ;(CONS A (CONS B NIL)) -> (LIST A B)
       `(list ,x ,(%cadr dcall)))
      ((and (consp ddcall)
            (eq (%car ddcall) 'cons)
            (eq (list-length ddcall) 3))
       ;(CONS A (CONS B (CONS C D))) -> (LIST* A B C D)
       (list* 'list* x (%cadr dcall) (%cdr ddcall)))
      (t call))
     call))

(define-compiler-macro dotimes (&whole call (i n &optional result)
                                       &body body
                                       &environment env)
  (multiple-value-bind (body decls) (parse-body body env)
    (if (nx-form-typep (setq n (nx-transform n env)) 'fixnum env)
        (let* ((limit (gensym))
               (upper (if (constantp n) n most-positive-fixnum))
               (top (gensym))
               (test (gensym)))
          `(let* ((,limit ,n) (,i 0))
             ,@decls
             (declare (fixnum ,limit)
                      (type (integer 0 ,(if (<= upper 0) 0 `(,upper))) ,i)
                      (unsettable ,i))
             (block nil
               (tagbody
                 (go ,test)
                 ,top
                 ,@body
                 (locally
                   (declare (settable ,i))
                   (setq ,i (1+ ,i)))
                 ,test
                 (when (< ,i ,limit) (go ,top)))
               ,result)))
        call)))

(define-compiler-macro dpb (&whole call &environment env value byte integer)
  (cond ((and (integerp byte) (> byte 0))
         (if (integerp value)
           `(logior ,(dpb value byte 0) (logand ,(lognot byte) ,integer))
           `(deposit-field (ash ,value ,(byte-position byte)) ,byte ,integer)))
        ((and (consp byte)
              (eq (%car byte) 'byte)
              (eq (list-length (%cdr byte)) 2))
         `(deposit-byte ,value ,(%cadr byte) ,(%caddr byte) ,integer))
        (t call)))

(define-compiler-macro eql (&whole call &environment env v1 v2)
  (if (or (eql-iff-eq-p v1 env) (eql-iff-eq-p v2 env))
    `(eq ,v1 ,v2)
    call))

(define-compiler-macro every (&whole call &environment env &rest ignore)
  (declare (ignore ignore))
  (some-xx-transform call env))


(define-compiler-macro identity (form) form)

(define-compiler-macro if (&whole call test true &optional false &environment env)
  (multiple-value-bind (test test-win) (nx-transform test env)
    (multiple-value-bind (true true-win) (nx-transform true env)
      (multiple-value-bind (false false-win) (nx-transform false env)
        (if (or (quoted-form-p test) (self-evaluating-p test))
          (if (eval test)
            true
            false)
          (if (or test-win true-win false-win)
            `(if ,test ,true ,false)
            call))))))

(define-compiler-macro %ilsr (&whole call &environment env shift value)
  (if (eql shift 0)
    value
    (if (eql value 0)
      `(progn ,shift 0)
      call)))

(defun string-designator-p (object)
  (typecase object
    (character t)
    (symbol t)
    (string t)))

(define-compiler-macro ldb (&whole call &environment env byte integer)
   (cond ((and (integerp byte) (> byte 0))
          (let ((size (byte-size byte))
                (position (byte-position byte)))
            (cond ((nx-form-typep integer 'fixnum env)
                   `(logand ,(byte-mask size)
                     (the fixnum (ash ,integer ,(- position)))))
                  (t `(load-byte ,size ,position ,integer)))))
         ((and (consp byte)
               (eq (%car byte) 'byte)
               (eq (list-length (%cdr byte)) 2))
          (let ((size (%cadr byte))
                (position (%caddr byte)))
            (if (and (nx-form-typep integer 'fixnum env) (fixnump position))
              ;; I'm not sure this is worth doing
              `(logand (byte-mask ,size) (the fixnum (ash ,integer ,(- position))))
              ;; this IS worth doing
              `(load-byte ,size ,position ,integer))))
         (t call)))

(define-compiler-macro length (&whole call &environment env seq)
  (if (nx-form-typep seq '(simple-array * (*)) env)
    `(uvsize ,seq)
    call))

(define-compiler-macro let (&whole call (&optional (first nil first-p) &rest rest) &body body)
  (if first-p
    (if rest
      call
      `(let* (,first) ,@body))
    `(locally ,@body)))

(define-compiler-macro let* (&whole call (&rest bindings) &body body)
  (if bindings
    call
    `(locally ,@body)))

(define-compiler-macro list* (&whole call &environment env &rest rest  &aux (n (list-length rest)) last)
  (cond ((%izerop n) nil)
        ((null (setq last (%car (last call))))
         (cons 'list (nreverse (cdr (reverse (cdr call))))))
        ((and (consp last) (memq (%car last) '(list* list cons)))
         (cons (if (eq (%car last) 'cons) 'list* (%car last))
                                 (nreconc (cdr (reverse (cdr call))) (%cdr last))))
        ((eq n 1) (list 'values last))
        ((eq n 2) (cons 'cons (%cdr call)))
        (t call)))



;;;(CONS X NIL) is same size as (LIST X) and faster.
(define-compiler-macro list  (&whole call &optional (first nil first-p) &rest more)
  (if more
    call
    (if first-p
      `(cons ,first nil))))


(define-compiler-macro locally (&whole call &body body &environment env)
  (multiple-value-bind (body decls) (parse-body body env nil)
    (if decls
      call
      `(progn ,@body))))

(defun specifier-type-if-known (typespec &optional env)
  (handler-case (specifier-type typespec env)
    (parse-unknown-type (c) (values nil (parse-unknown-type-specifier c)))
    (error () nil)))

#+debugging-version
(defun specifier-type-if-known (typespec &optional env)
  (handler-bind ((parse-unknown-type (lambda (c)
                                       (break "caught unknown-type ~s" c)
                                       (return-from specifier-type-if-known
                                         (values nil (parse-unknown-type-specifier c)))))
                 (error (lambda (c)
                          (break "caught error ~s" c)
                          (return-from specifier-type-if-known nil))))
    (specifier-type typespec env)))


(defun target-element-type-type-keyword (typespec &optional env)
  (let* ((ctype (specifier-type-if-known `(array ,typespec) env)))
    (if (null ctype)
      (progn
        (nx1-whine :unknown-type-declaration typespec)
        nil)
      (funcall (arch::target-array-type-name-from-ctype-function
                (backend-target-arch *target-backend*))
               ctype))))

(defun infer-array-type (dims element-type element-type-p displaced-to-p fill-pointer-p adjustable-p env)
  (let* ((ctype (make-array-ctype :complexp (or displaced-to-p fill-pointer-p adjustable-p))))
    (if (quoted-form-p dims)
      (let* ((dims (nx-unquote dims)))
        (if (listp dims)
          (progn
            (unless (every #'fixnump dims)
              (warn "Funky-looking array dimensions ~s in MAKE-ARRAY call" dims))
            (setf (array-ctype-dimensions ctype) dims))
          (progn
            (unless (typep dims 'fixnum)
              (warn "Funky-looking array dimensions ~s in MAKE-ARRAY call" dims))
            (setf (array-ctype-dimensions ctype) (list dims)))))
      (if (atom dims)
        (if (nx-form-typep dims 'fixnum env)
          (setf (array-ctype-dimensions ctype)
                (if (typep (setq dims (nx-transform dims env)) 'fixnum)
                  (list dims)
                  (list '*)))
          (setf (array-ctype-dimensions ctype) '*))
        (if (eq (car dims) 'list)
          (setf (array-ctype-dimensions ctype)
                (mapcar #'(lambda (d)
                            (if (typep (setq d (nx-transform d env)) 'fixnum)
                              d
                              '*))
                        (cdr dims)))
          ;; Wimp out
          (setf (array-ctype-dimensions ctype)
                '*))))
    (let* ((typespec (if element-type-p
                       (if (constantp element-type)
                         (nx-unquote element-type)
                         '*)
                       t))
           (element-type (or (specifier-type-if-known typespec env)
                             (make-unknown-ctype :specifier typespec))))
      (setf (array-ctype-element-type ctype) element-type)
      (if (typep element-type 'unknown-ctype)
        (setf (array-ctype-element-type ctype) *wild-type*))
      (specialize-array-type ctype))
    (type-specifier ctype)))



(define-compiler-macro make-array (&whole call &environment env dims &rest keys)
  (if (constant-keywords-p keys)
    (destructuring-bind (&key (element-type t element-type-p)
                              (displaced-to () displaced-to-p)
                              (displaced-index-offset () displaced-index-offset-p)
                              (adjustable () adjustable-p)
                              (fill-pointer () fill-pointer-p)
                              (initial-element () initial-element-p)
                              (initial-contents () initial-contents-p))
        keys
      (declare (ignorable element-type element-type-p
                          displaced-to displaced-to-p
                          displaced-index-offset displaced-index-offset-p
                          adjustable adjustable-p
                          fill-pointer fill-pointer-p
                          initial-element initial-element-p
                          initial-contents initial-contents-p))
      (let* ((element-type-keyword nil)
             (expansion
              (cond ((and initial-element-p initial-contents-p)
                     (nx1-whine 'illegal-arguments call)
                     call)
                    (displaced-to-p
                     (if (or initial-element-p initial-contents-p element-type-p)
                       (comp-make-array-1 dims keys)
                       (comp-make-displaced-array dims keys)))
                    ((or displaced-index-offset-p
                         (not (constantp element-type))
                         (null (setq element-type-keyword
                                     (target-element-type-type-keyword
                                      (eval element-type) env))))
                     (comp-make-array-1 dims keys))
                    ((and (typep element-type-keyword 'keyword)
                          (nx-form-typep dims 'fixnum env)
                          (null (or adjustable fill-pointer initial-contents
                                    initial-contents-p)))
                     (if
                       (or (null initial-element-p)
                           (cond ((eql element-type-keyword :double-float-vector)
                                  (eql initial-element 0.0d0))
                                 ((eql element-type-keyword :single-float-vector)
                                  (eql initial-element 0.0s0))
                                 ((eql element-type :simple-string)
                                  (eql initial-element #\Null))
                                 (t (eql initial-element 0))))
                       `(allocate-typed-vector ,element-type-keyword ,dims)
                       `(allocate-typed-vector ,element-type-keyword ,dims ,initial-element)))
                    (t                        ;Should do more here
                     (comp-make-uarray dims keys (type-keyword-code element-type-keyword)))))
             (type (infer-array-type dims element-type element-type-p displaced-to-p fill-pointer-p adjustable-p env)))
        `(the ,type ,expansion)))

        call))

(defun comp-make-displaced-array (dims keys)
  (let* ((call-list (make-list 4 :initial-element nil))
	 (dims-var (make-symbol "DIMS"))
         (let-list (comp-nuke-keys keys
                                   '((:displaced-to 0)
                                     (:fill-pointer 1)
                                     (:adjustable 2)
                                     (:displaced-index-offset 3))
                                   call-list
				   `((,dims-var ,dims)))))

    `(let ,let-list
       (%make-displaced-array ,dims-var ,@call-list t))))

(defun comp-make-uarray (dims keys subtype)
  (if (null keys)
    `(%make-simple-array ,subtype ,dims)
    (let* ((call-list (make-list 6))
           (dims-var (make-symbol "DIMS"))
           (let-list (comp-nuke-keys keys
                                     '((:adjustable 0)
                                       (:fill-pointer 1)
                                       (:initial-element 2 3)
                                       (:initial-contents 4 5))
                                     call-list
                                     `((,dims-var ,dims)))))
      `(let ,let-list
        (make-uarray-1 ,subtype ,dims-var ,@call-list nil nil)))))

(defun comp-make-array-1 (dims keys)
  (let* ((call-list (make-list 10 :initial-element nil))
	 (dims-var (make-symbol "DIMS"))
         (let-list (comp-nuke-keys keys
                                   '((:element-type 0 1)
                                     (:displaced-to 2)
                                     (:displaced-index-offset 3)
                                     (:adjustable 4)
                                     (:fill-pointer 5)
                                     (:initial-element 6 7)
                                     (:initial-contents 8 9))
                                   call-list
				   `((,dims-var ,dims)))))
    `(let ,let-list
       (make-array-1 ,dims-var ,@call-list nil))))

(defun comp-nuke-keys (keys key-list call-list &optional required-bindings)
  ; side effects call list, returns a let-list
  (let* ((let-list (reverse required-bindings))
         (seen nil))
    (do ((lst keys (cddr lst)))
        ((null lst) nil)
      (let* ((key (car lst))
             (val (cadr lst))
             (ass (assq key key-list))
             (vpos (cadr ass))
             (ppos (caddr ass)))
        (when ass
          (unless (memq vpos seen)
            (push vpos seen)
            (when (not (constantp val))
              (let ((gen (gensym)))
                (setq let-list (cons (list gen val) let-list)) ; reverse him
                (setq val gen)))
            (rplaca (nthcdr vpos call-list) val)
            (if ppos (rplaca (nthcdr ppos call-list) t))))))
    (nreverse let-list)))

(define-compiler-macro make-instance (&whole call class &rest initargs)
  (if (and (listp class)
           (eq (car class) 'quote)
           (symbolp (cadr class))
           (null (cddr class)))
    (let* ((cell (gensym)))
      `(let* ((,cell (load-time-value (find-class-cell ,class t))))
        (funcall (class-cell-instantiate ,cell) ,cell ,@initargs)))
    call))







(define-compiler-macro mapc  (&whole call fn lst &rest more)
  (if more
    call
    (let* ((temp-var (gensym))
           (elt-var (gensym))
           (fn-var (gensym)))
       `(let* ((,fn-var ,fn)
               (,temp-var ,lst))
          (dolist (,elt-var ,temp-var ,temp-var)
            (funcall ,fn-var ,elt-var))
          ))))

(define-compiler-macro mapcar (&whole call fn lst &rest more)
  (if more
    call
    (let* ((temp-var (gensym))
           (result-var (gensym))
           (elt-var (gensym))
           (fn-var (gensym)))
      `(let* ((,temp-var (cons nil nil))
              (,result-var ,temp-var)
              (,fn-var ,fn))
         (declare (dynamic-extent ,temp-var)
                  (type cons ,temp-var ,result-var))
         (dolist (,elt-var ,lst (cdr ,result-var))
           (setq ,temp-var (setf (cdr ,temp-var) (list (funcall ,fn-var ,elt-var)))))))))

(define-compiler-macro member (&whole call item list &rest keys)
  (or (remove-explicit-test-keyword-from-test-testnot-key item list keys 'memeql '((eq . memq) (eql . memeql) (equal . memequal)) 'member-test)
      call))

(define-compiler-macro memequal (&whole call &environment env item list)
  (if (or (equal-iff-eql-p item env)
          (and (quoted-form-p list)
               (proper-list-p (%cadr list))
               (every (lambda (elt) (equal-iff-eql-p elt env)) (%cadr list))))
    `(memeql ,item ,list)
    call))

(define-compiler-macro memeql (&whole call &environment env item list)
  (if (or (eql-iff-eq-p item env)
          (and (quoted-form-p list)
               (proper-list-p (%cadr list))
               (every (lambda (elt) (eql-iff-eq-p elt env)) (%cadr list))))
    `(memq ,item ,list)
    call))

(define-compiler-macro memq (&whole call &environment env item list)
  ;;(memq x '(y)) => (if (eq x 'y) '(y))
  ;;Would it be worth making a two elt list into an OR?  Maybe if
  ;;optimizing for speed...
   (if (and (or (quoted-form-p list)
                (null list))
            (null (cdr (%cadr list))))
     (if list `(if (eq ,item ',(%caadr list)) ,list))
     (let* ((x (gensym))
            (tail (gensym)))
       `(do* ((,x ,item)
              (,tail ,list (cdr (the list ,tail))))
         ((null ,tail))
         (if (eq (car ,tail) ,x) (return ,tail))))))

(define-compiler-macro minusp (x)
  `(< ,x 0))

(define-compiler-macro notany (&whole call &environment env &rest ignore)
  (declare (ignore ignore))
  (some-xx-transform call env))

(define-compiler-macro notevery (&whole call &environment env &rest ignore)
  (declare (ignore ignore))
  (some-xx-transform call env))

(define-compiler-macro nth  (&whole call &environment env count list)
   (if (and (fixnump count)
            (%i>= count 0)
            (%i< count 3))
     `(,(svref '#(car cadr caddr) count) ,list)
     `(car (nthcdr ,count ,list))))

(define-compiler-macro nthcdr (&whole call &environment env count list)
  (if (and (fixnump count)
           (%i>= count 0)
           (%i< count 4))
     (if (%izerop count)
       `(require-type ,list 'list)
       `(,(svref '#(cdr cddr cdddr) (%i- count 1)) ,list))
    (let* ((i (gensym))
           (n (gensym))                 ; evaluation order
           (tail (gensym)))
      `(let* ((,n (require-type ,count 'unsigned-byte))
              (,tail (require-type ,list 'list)))
        (dotimes (,i ,n ,tail)
          (unless (setq ,tail (cdr ,tail))
            (return nil)))))))

(define-compiler-macro plusp (x)
  `(> ,x 0))

(define-compiler-macro progn (&whole call &optional (first nil first-p) &rest rest)
  (if first-p
    (if rest call first)))

;;; This isn't quite right... The idea is that (car (require-type foo
;;; 'list)) ;can become just (<typechecking-car> foo) [regardless of
;;; optimize settings], ;but I don't think this can be done just with
;;; optimizers... For now, at least try to get it to become (%car
;;; (<typecheck> foo)).
(define-compiler-macro require-type (&whole call &environment env arg type &aux ctype)
  (cond ((and (or (eq type t)
                  (and (quoted-form-p type)
                       (setq type (%cadr type))))
              (setq ctype (specifier-type-if-known type env)))
         (cond ((nx-form-typep arg type env) arg)
               ((eq type 'simple-vector)
                `(the simple-vector (require-simple-vector ,arg)))
               ((eq type 'simple-string)
                `(the simple-string (require-simple-string ,arg)))
               ((eq type 'integer)
                `(the integer (require-integer ,arg)))
               ((eq type 'fixnum)
                `(the fixnum (require-fixnum ,arg)))
               ((eq type 'real)
                `(the real (require-real ,arg)))
               ((eq type 'list)
                `(the list (require-list ,arg)))
               ((eq type 'character)
                `(the character (require-character ,arg)))
               ((eq type 'number)
                `(the number (require-number ,arg)))
               ((eq type 'symbol)
                `(the symbol (require-symbol ,arg)))
               ((type= ctype
                       (specifier-type '(signed-byte 8)))
                `(the (signed-byte 8) (require-s8 ,arg)))
               ((type= ctype
                       (specifier-type '(unsigned-byte 8)))
                `(the (unsigned-byte 8) (require-u8 ,arg)))
               ((type= ctype
                       (specifier-type '(signed-byte 16)))
                `(the (signed-byte 16) (require-s16 ,arg)))
               ((type= ctype
                       (specifier-type '(unsigned-byte 16)))
                `(the (unsigned-byte 16) (require-u16 ,arg)))
               ((type= ctype
                       (specifier-type '(signed-byte 32)))
                `(the (signed-byte 32) (require-s32 ,arg)))
               ((type= ctype
                       (specifier-type '(unsigned-byte 32)))
                `(the (unsigned-byte 32) (require-u32 ,arg)))
               ((type= ctype
                       (specifier-type '(signed-byte 64)))
                `(the (signed-byte 64) (require-s64 ,arg)))
               ((type= ctype
                       (specifier-type '(unsigned-byte 64)))
                `(the (unsigned-byte 64) (require-u64 ,arg)))
               #+nil
               ((and (symbolp type)
                     (let ((simpler (type-predicate type)))
                       (if simpler `(the ,type (%require-type ,arg ',simpler))))))
               #+nil
               ((and (symbolp type)(find-class type nil env))
                  `(%require-type-class-cell ,arg (load-time-value (find-class-cell ',type t))))
               (t (let* ((val (gensym)))
                    `(let* ((,val ,arg))
                      (if (typep ,val ',type)
                        ,val
                        (%kernel-restart $xwrongtype ,val ',type)))))))
        (t call)))

(define-compiler-macro proclaim (&whole call decl)
   (if (and (quoted-form-p decl)
            (eq (car (setq decl (%cadr decl))) 'special))
       (do ((vars (%cdr decl) (%cdr vars)) (decls ()))
           ((null vars)
            (cons 'progn (nreverse decls)))
         (unless (and (car vars)
                      (neq (%car vars) t)
                      (symbolp (%car vars)))
            (return call))
         (push (list '%proclaim-special (list 'quote (%car vars))) decls))
       call))


(define-compiler-macro some (&whole call &environment env &rest ignore)
  (declare (ignore ignore))
  (some-xx-transform call env))

(define-compiler-macro struct-ref (&whole call &environment env struct offset)
   (if (nx-inhibit-safety-checking env)
    `(%svref ,struct ,offset)
    call))

;;; expand find-if and find-if-not

(define-compiler-macro find-if (&whole call &environment env
                                       test sequence &rest keys)
  `(find ,test ,sequence
        :test #'funcall
        ,@keys))

(define-compiler-macro find-if-not (&whole call &environment env
                                           test sequence &rest keys)
  `(find ,test ,sequence
        :test-not #'funcall
        ,@keys))

;;; inline some cases, and use a positional function in others

(define-compiler-macro find (&whole call &environment env
                                    item sequence &rest keys)
  (if (constant-keywords-p keys)
    (destructuring-bind (&key from-end test test-not (start 0) end key) keys
      (if (and (eql start 0)
               (null end)
               (null from-end)
               (not (and test test-not)))
        (let ((find-test (or test test-not '#'eql))
              (loop-test (if test-not 'unless 'when))
              (loop-function (nx-form-sequence-iterator sequence env)))
          (if loop-function
            (let ((item-var (unless (or (constantp item)
                                        (and (equal find-test '#'funcall)
                                             (function-form-p item)))
                              (gensym)))
                  (elt-var (gensym)))
              `(let (,@(when item-var `((,item-var ,item))))
                 (,loop-function (,elt-var ,sequence)
                                 (,loop-test (funcall ,find-test ,(or item-var item)
                                                      (funcall ,(or key '#'identity) ,elt-var))
                                             (return ,elt-var)))))
            (let ((find-function (if test-not 'find-positional-test-not-key 'find-positional-test-key))
                  (item-var (gensym))
                  (sequence-var (gensym))
                  (test-var (gensym))
                  (key-var (gensym)))
              `(let ((,item-var ,item)
                     (,sequence-var ,sequence)
                     (,test-var ,(or test test-not))
                     (,key-var ,key))
                 (declare (dynamic-extent ,item-var ,sequence-var ,test-var ,key-var))
                 (,find-function ,item-var ,sequence-var ,test-var ,key-var)))))
        call))
      call))

;;; expand position-if and position-if-not

(define-compiler-macro position-if (&whole call &environment env
                                           test sequence &rest keys)
  `(position ,test ,sequence
             :test #'funcall
             ,@keys))

(define-compiler-macro position-if-not (&whole call &environment env
                                               test sequence &rest keys)
  `(position ,test ,sequence
             :test-not #'funcall
             ,@keys))

;;; inline some cases, and use positional functions for others

(define-compiler-macro position (&whole call &environment env
                                        item sequence &rest keys)
  (if (constant-keywords-p keys)
    (destructuring-bind (&key from-end test test-not (start 0) end key) keys
      (if (and (eql start 0)
               (null end)
               (null from-end)
               (not (and test test-not)))
        (let ((position-test (or test test-not '#'eql))
              (loop-test (if test-not 'unless 'when))
              (sequence-value (if (constantp sequence)
                                (eval-constant sequence)
                                sequence)))
          (cond ((nx-form-typep sequence-value 'list env)
                 (let ((item-var (unless (or (constantp item)
                                             (and (equal position-test '#'funcall)
                                                  (function-form-p item)))
                                   (gensym)))
                       (elt-var (gensym))
                       (position-var (gensym)))
                   `(let (,@(when item-var `((,item-var ,item)))
                          (,position-var 0))
                      (dolist (,elt-var ,sequence)
                        (,loop-test (funcall ,position-test ,(or item-var item)
                                             (funcall ,(or key '#'identity) ,elt-var))
                                    (return ,position-var))
                        (incf ,position-var)))))
                ((nx-form-typep sequence-value 'vector env)
                 (let ((item-var (unless (or (constantp item)
                                             (and (equal position-test '#'funcall)
                                                  (function-form-p item)))
                                   (gensym)))
                       (sequence-var (gensym))
                       (position-var (gensym)))
                   `(let (,@(when item-var `((,item-var ,item)))
                          (,sequence-var ,sequence))
                      ,@(let ((type (nx-form-type sequence env)))
                          (unless (eq type t)
                            `((declare (type ,type ,sequence-var)))))
                      (dotimes (,position-var (length ,sequence-var))
                        (,loop-test (funcall ,position-test ,(or item-var item)
                                             (funcall ,(or key '#'identity)
                                                      (locally (declare (optimize (speed 3) (safety 0)))
                                                        (aref ,sequence ,position-var))))
                                    (return ,position-var))))))
                (t
                 (let ((position-function (if test-not
                                            'position-positional-test-not-key
                                            'position-positional-test-key))
                       (item-var (gensym))
                       (sequence-var (gensym))
                       (test-var (gensym))
                       (key-var (gensym)))
                   `(let ((,item-var ,item)
                          (,sequence-var ,sequence)
                          (,test-var ,(or test test-not))
                          (,key-var ,key))
                      (declare (dynamic-extent ,sequence-var ,test-var ,key-var))
                      (,position-function ,item-var ,sequence-var ,test-var ,key-var))))))
        call))
    call))

;;; inline some cases of remove-if and remove-if-not

(define-compiler-macro remove-if (&whole call &environment env &rest ignore)
  (declare (ignore ignore))
  (remove-if-transform call env))

(define-compiler-macro remove-if-not (&whole call &environment env &rest ignore)
  (declare (ignore ignore))
  (remove-if-transform call env))

(defun remove-if-transform (call env)
  (destructuring-bind (function test sequence &rest keys) call
    (if (constant-keywords-p keys)
      (destructuring-bind (&key from-end (start 0) end count (key '#'identity)) keys
        (if (and (eql start 0)
                 (null end)
                 (null from-end)
                 (null count)
                 (nx-form-typep sequence 'list env))
          ;; only do the list case, since it's hard to collect vector results
          (let ((temp-var (gensym))
                (result-var (gensym))
                (elt-var (gensym))
                (loop-test (ecase function (remove-if 'unless) (remove-if-not 'when))))
            `(the list
               (let* ((,temp-var (cons nil nil))
                      (,result-var ,temp-var))
                 (declare (dynamic-extent ,temp-var))
                 (dolist (,elt-var ,sequence (%cdr ,result-var))
                   (,loop-test (funcall ,test (funcall ,key ,elt-var))
                               (setq ,temp-var
                                     (%cdr
                                      (%rplacd ,temp-var (list ,elt-var)))))))))
          call))
      call)))



(define-compiler-macro struct-set (&whole call &environment env struct offset new)
  (if (nx-inhibit-safety-checking env)
    `(%svset ,struct ,offset ,new)
    call))

(define-compiler-macro zerop (arg &environment env)
  (let* ((z (if (nx-form-typep arg 'float env)
	      (coerce 0 (nx-form-type arg env))
	      0)))
    `(= ,arg ,z)))


(define-compiler-macro = (&whole w n0 &optional (n1 nil n1p) &rest more)
  (if (not n1p)
    `(require-type ,n0 'number)
    (if more
      w
      `(=-2 ,n0 ,n1))))

(define-compiler-macro /= (&whole w n0 &optional (n1 nil n1p) &rest more)
  (if (not n1p)
    `(require-type ,n0 'number)
    (if more
      w
      `(/=-2 ,n0 ,n1))))

(define-compiler-macro + (&whole w  &environment env &optional (n0 nil n0p) (n1 nil n1p) &rest more)
  (if more
    `(+ (+-2 ,n0 ,n1) ,@more)
    (if n1p
      `(+-2 ,n0 ,n1)
      (if n0p
        `(require-type ,n0 'number)
        0))))

(define-compiler-macro - (&whole w &environment env n0 &optional (n1 nil n1p) &rest more)
  (if more
    `(- (--2 ,n0 ,n1) ,@more)
    (if n1p
      `(--2 ,n0 ,n1)
      `(%negate ,n0))))

(define-compiler-macro * (&whole w &environment env &optional (n0 nil n0p) (n1 nil n1p) &rest more)
  (if more
    (let ((type (nx-form-type w env)))
      (if (and type (numeric-type-p type)) ; go pairwise if type known, else not
        `(*-2 ,n0 (* ,n1 ,@more))
        w))
    (if n1p
      `(*-2 ,n0 ,n1)
      (if n0p
        `(require-type ,n0 'number)
        1))))

(define-compiler-macro / (&whole w n0 &optional (n1 nil n1p) &rest more)
  (if more
    w
    (if n1p
      `(/-2 ,n0 ,n1)
      `(%quo-1 ,n0))))

;;; beware of limits - truncate of most-negative-fixnum & -1 ain't a
;;; fixnum - too bad
(define-compiler-macro truncate (&whole w &environment env n0 &optional (n1 nil n1p))
  (let ((*nx-form-type* t))
    (if (nx-form-typep n0 'fixnum env)
      (if (not n1p)
        n0
        (if (nx-form-typep n1 'fixnum env)
          `(%fixnum-truncate ,n0 ,n1)
          w))
      w)))

(define-compiler-macro floor (&whole w &environment env n0 &optional (n1 nil n1p))
  (let ((*nx-form-type* t))
    (if (nx-form-typep n0 'fixnum env)
      (if (not n1p)
        n0
        (if (nx-form-typep n1 'fixnum env)
          `(%fixnum-floor ,n0 ,n1)
          w))
      w)))

(define-compiler-macro round (&whole w &environment env n0 &optional (n1 nil n1p))
  (let ((*nx-form-type* t)) ; it doesn't matter what the result type is declared to be
    (if (nx-form-typep n0 'fixnum env)
      (if (not n1p)
        n0
        (if (nx-form-typep n1 'fixnum env)
          `(%fixnum-round ,n0 ,n1)
          w))
      w)))

(define-compiler-macro ceiling (&whole w &environment env n0 &optional (n1 nil n1p))
  (let ((*nx-form-type* t))
    (if (nx-form-typep n0 'fixnum env)
      (if (not n1p)
        n0
        (if (nx-form-typep n1 'fixnum env)
          `(%fixnum-ceiling ,n0 ,n1)
          w))
      w)))

(define-compiler-macro oddp (&whole w &environment env n0)
  (if (nx-form-typep n0 'fixnum env)
    `(logbitp 0 (the fixnum ,n0))
    w))

(define-compiler-macro evenp (&whole w &environment env n0)
  (if (nx-form-typep n0 'fixnum env)
    `(not (logbitp 0 (the fixnum ,n0)))
    w))


(define-compiler-macro logandc2 (n0 n1)
  (let ((n1var (gensym))
        (n0var (gensym)))
    `(let ((,n0var ,n0)
           (,n1var ,n1))
       (logandc1 ,n1var ,n0var))))

(define-compiler-macro logorc2 (n0 n1)
  (let ((n1var (gensym))
        (n0var (gensym)))
    `(let ((,n0var ,n0)
           (,n1var ,n1))
       (logorc1 ,n1var ,n0var))))

(define-compiler-macro lognand (n0 n1)
  `(lognot (logand ,n0 ,n1)))

(define-compiler-macro lognor (n0 n1)
  `(lognot (logior ,n0 ,n1)))


(defun transform-logop (whole identity binop &optional (transform-complement t))
  (destructuring-bind (op &optional (n0 nil n0p) (n1 nil n1p) &rest more) whole
    (if (and n1p (eql n0 identity))
      `(,op ,n1 ,@more)
      (if (and transform-complement n1p (eql n0 (lognot identity)))
        `(progn
           (,op ,n1 ,@more)
           ,(lognot identity))
        (if more
          (if (cdr more)
            whole
            `(,binop ,n0 (,binop ,n1 ,(car more))))
          (if n1p
            `(,binop ,n0 ,n1)
            (if n0p
              `(require-type ,n0 'integer)
              identity)))))))

(define-compiler-macro logand (&whole w &rest all)
  (declare (ignore all))
  (transform-logop w -1 'logand-2))

(define-compiler-macro logior (&whole w &rest all)
  (declare (ignore all))
  (transform-logop w 0 'logior-2))

(define-compiler-macro logxor (&whole w &rest all)
  (declare (ignore all))
  (transform-logop w 0 'logxor-2 nil))

(define-compiler-macro lognot (&whole w &environment env n1)
  (if (nx-form-typep n1 'fixnum env)
    `(%ilognot ,n1)
    w))

(define-compiler-macro logtest (&whole w &environment env n1 n2)
  (if (and (nx-form-typep n1 'fixnum env)
           (nx-form-typep n2 'fixnum env))
    `(not (eql 0 (logand ,n1 ,n2)))
    w))


(defmacro defsynonym (from to)
  ;Should maybe check for circularities.
  `(progn
     (setf (compiler-macro-function ',from) nil)
     (let ((pair (assq ',from *nx-synonyms*)))
       (if pair (rplacd pair ',to)
           (push (cons ',from ',to)
                 *nx-synonyms*))
       ',to)))

(defsynonym first car)
(defsynonym second cadr)
(defsynonym third caddr)
(defsynonym fourth cadddr)
(defsynonym rest cdr)


(defsynonym functionp lfunp)
(defsynonym null not)
(defsynonym char-int char-code)

;;; Improvemets file by Bob Cassels
;;; Just what are "Improvemets", anyway ?

;;; Optimize some CL sequence functions, mostly by inlining them in
;;; simple cases when the type of the sequence is known.  In some
;;; cases, dynamic-extent declarations are automatically inserted.
;;; For some sequence functions, if the type of the sequence is known
;;; at compile time, the function is inlined.  If the type isn't known
;;; but the call is "simple", a call to a faster (positional-arg)
;;; function is substituted.


(defun nx-form-sequence-iterator (sequence-form env)
  (cond ((nx-form-typep sequence-form 'vector env) 'dovector)
        ((nx-form-typep sequence-form 'list env) 'dolist)))

(defun function-form-p (form)
   ;; c.f. quoted-form-p
   (and (consp form)
        (eq (%car form) 'function)
        (consp (%cdr form))
        (null (%cdr (%cdr form)))))


;; Return a form that checks to see if THING is if type CTYPE, or
;; NIL if we can't do that for some reason.
(defun optimize-ctypep (thing ctype)
  (when (eq *target-backend* *host-backend*)
    (typecase ctype
      (numeric-ctype
       (cond ((eq :real (numeric-ctype-complexp ctype))
              (let* ((low (numeric-ctype-low ctype))
                     (high (numeric-ctype-high ctype))
                     (class (numeric-ctype-class ctype))
                     (format (numeric-ctype-format ctype))
                     (type (if (eq class 'float)
                             (or format class)
                             (or class 'real))))
                (cond ((and low (eql low high) (or (not (eq class 'float))
                                                   format))
                       `(eql ,thing ,low))
                      ((and (eq type 'float)
                            (or low high)
                            (or (null low)
                                (typep low 'single-float)
                                (not (null (ignore-errors
                                             (coerce (if (atom low)
                                                       low
                                                       (car low))
                                                     'single-float)))))
                            (or (null high)
                                (typep high 'single-float)
                                (not (null (ignore-errors
                                             (coerce (if (atom high)
                                                       high
                                                       (car high))
                                                     'single-float))))))
                       (let* ((temp (gensym)))
                         (flet ((bounded-float (type low high)
                                  `(,type
                                    ,(if low
                                         (if (listp low)
                                           (list (coerce (car low) type))
                                           (coerce low type))
                                         '*)
                                    ,(if high
                                         (if (listp high)
                                           (list (coerce (car high) type))
                                           (coerce high type))
                                         '*))))
                         `(let* ((,temp ,thing))
                           (or (typep ,temp ',(bounded-float 'single-float low high))
                            (typep ,temp ',(bounded-float 'double-float low high)))))))
                      (t
                       (let* ((temp (gensym)))
                         (if (and (typep low 'fixnum) (typep high 'fixnum)
                                  (eq class 'integer))
                           (setq type 'fixnum))
                         (if (or low high)
                           `(let* ((,temp ,thing))
                             (and (typep ,temp ',type)
                              ,@(if low `((,(if (consp low) '> '>=) (the ,type ,temp) ,(if (consp low) (car low) low))))
                              ,@(if high `((,(if (consp high) '< '<=) (the ,type ,temp) ,(if (consp high) (car high) high))))))
                           `(typep ,thing ',type)))))))
             (t `(numeric-%%typep ,thing ,ctype))))
      (array-ctype
       (or
        (let* ((typecode (array-ctype-typecode ctype))
               (dims (array-ctype-dimensions ctype)))
          (cond ((and typecode (consp dims) (null (cdr dims)))
                 (case (array-ctype-complexp ctype)
                   ((nil)
                    (if (eq (car dims) '*)
                      `(eql (typecode ,thing) ,typecode)
                      (let* ((temp (gensym)))
                        `(let* ((,temp ,thing))
                          (and (eql (typecode ,temp) ,typecode)
                           (eq (uvsize ,temp) ,(car dims)))))))
                   ((* :maybe)
                    (let* ((temp (gensym))
                           (tempcode (gensym)))
                      `(let* ((,temp ,thing)
                              (,tempcode (typecode ,temp)))
                        (or (and (eql ,tempcode ,typecode)
                             ,@(unless (eq (car dims) '*)
                                       `((eq (uvsize ,temp) ,(car dims)))))
                         (and (eql ,tempcode target::subtag-vectorH)
                          (eql (ldb target::arrayH.flags-cell-subtag-byte (the fixnum (%svref ,temp target::arrayH.flags-cell))) ,typecode)
                          ,@(unless (eq (car dims) '*)
                                    `((eq (%svref ,temp target::vectorH.logsize-cell) ,(car dims)))))))))))))
        `(values (array-%%typep ,thing ,ctype)))))))



(defun optimize-typep (thing type env)
  ;; returns a new form, or nil if it can't optimize
  (let* ((ctype (specifier-type-if-known type env)))
    (when ctype
      (let* ((type (type-specifier ctype))
             (predicate (if (typep type 'symbol) (type-predicate type))))
        (if (and predicate (symbolp predicate))
          `(,predicate ,thing)
          (or (optimize-ctypep thing ctype)
              (cond ((symbolp type)
                     (cond ((%deftype-expander type)
                            ;; recurse here, rather than returning the
                            ;; partially-expanded form mostly since it doesn't
                            ;; seem to further optimize the result otherwise
                            (let ((expanded-type (type-expand type)))
                              (or (optimize-typep thing expanded-type env)
                                  ;; at least do the first expansion
                                  `(typep ,thing ',expanded-type))))
                           ((structure-class-p type env)
                            `(structure-typep ,thing ',type))
                           ((find-class type nil env)
                            `(class-cell-typep ,thing (load-time-value (find-class-cell ',type t))))
                           ((info-type-builtin type) ; bootstrap troubles here?
                            `(builtin-typep ,thing (load-time-value (find-builtin-cell ',type))))
                           (t nil)))
                    ((consp type)
                     (cond
                       ((info-type-builtin type) ; byte types
                        `(builtin-typep ,thing (load-time-value (find-builtin-cell ',type))))
                       (t
                        (case (%car type)
                          (satisfies `(funcall ',(cadr type) ,thing))
                          (eql `(eql ,thing ',(cadr type)))
                          (member `(not (null (member ,thing ',(%cdr type)))))
                          (not `(not (typep ,thing ',(cadr type))))
                          ((or and)
                           (let ((thing-sym (gensym)))
                             `(let ((,thing-sym ,thing))
                               (,(%car type)
                                ,@(mapcar #'(lambda (type-spec)
                                              (or (optimize-typep thing-sym type-spec env)
                                                  `(typep ,thing-sym ',type-spec)))
                                          (%cdr type))))))
                          ((signed-byte unsigned-byte integer mod) ; more byte types
                           `(builtin-typep ,thing (load-time-value (find-builtin-cell ',type))))
                          (t nil)))))
                    (t nil))))))))

(define-compiler-macro typep  (&whole call &environment env thing type &optional e)
  (if (quoted-form-p type)
    (if (and (constantp thing) (specifier-type-if-known type env))
      (typep (if (quoted-form-p thing) (%cadr thing) thing) (%cadr type) env)
      (or (and (null e) (optimize-typep thing (%cadr type) env))
          call))
    (if (eq type t)
      `(progn ,thing t)
      call)))

(define-compiler-macro true (&rest args)
  `(progn
    ,@args
    t))


(define-compiler-macro false (&rest args)
  `(progn
    ,@args
    nil))

(define-compiler-macro find-class (&whole call type &optional (errorp t) env)
  (if (and (quoted-form-p type)(not *dont-find-class-optimize*)(not env))
      `(class-cell-find-class (load-time-value (find-class-cell ,type t)) ,errorp)
    call))


(define-compiler-macro gcd (&whole call &optional (n0 nil n0-p) (n1 nil n1-p) &rest rest)
  (if rest
    call
    (if n1-p
      `(gcd-2 ,n0 ,n1)
      (if n0-p
        `(%integer-abs ,n0)
        0))))

(define-compiler-macro lcm (&whole call &optional (n0 nil n0-p) (n1 nil n1-p) &rest rest)
  (if rest
    call
    (if n1-p
      `(lcm-2 ,n0 ,n1)
      (if n0-p
        `(%integer-abs ,n0)
        1))))

(define-compiler-macro max (&whole call &environment env n0 &optional (n1 nil n1-p) &rest rest)
  (if rest
    call
    (if n1-p
      (if (and (nx-form-typep n0 'fixnum env)(nx-form-typep n1 'fixnum env))
        `(imax-2 ,n0 ,n1)
        `(max-2 ,n0 ,n1))
      `(require-type ,n0 'real))))

(define-compiler-macro max-2 (n0 n1)
  (let* ((g0 (gensym))
         (g1 (gensym)))
   `(let* ((,g0 ,n0)
           (,g1 ,n1))
      (if (> ,g0 ,g1) ,g0 ,g1))))

(define-compiler-macro imax-2 (n0 n1)
  (let* ((g0 (gensym))
         (g1 (gensym)))
   `(let* ((,g0 ,n0)
           (,g1 ,n1))
      (if (%i> ,g0 ,g1) ,g0 ,g1))))




(define-compiler-macro min (&whole call &environment env n0 &optional (n1 nil n1-p) &rest rest)
  (if rest
    call
    (if n1-p
      (if (and (nx-form-typep n0 'fixnum env)(nx-form-typep n1 'fixnum env))
        `(imin-2 ,n0 ,n1)
        `(min-2 ,n0 ,n1))
      `(require-type ,n0 'real))))

(define-compiler-macro min-2 (n0 n1)
  (let* ((g0 (gensym))
         (g1 (gensym)))
   `(let* ((,g0 ,n0)
           (,g1 ,n1))
      (if (< ,g0 ,g1) ,g0 ,g1))))

(define-compiler-macro imin-2 (n0 n1)
  (let* ((g0 (gensym))
         (g1 (gensym)))
   `(let* ((,g0 ,n0)
           (,g1 ,n1))
      (if (%i< ,g0 ,g1) ,g0 ,g1))))


(defun eq-test-p (test)
  (or (equal test ''eq) (equal test '#'eq)))

(defun eql-test-p (test)
  (or (equal test ''eql) (equal test '#'eql)))

(define-compiler-macro adjoin (&whole whole elt list &rest keys)
  (if (constant-keywords-p keys)
    (destructuring-bind (&key (test ''eql) test-not key) keys
      (or (and (null test-not)
               (null key)
               (cond ((eq-test-p test)
                      `(adjoin-eq ,elt ,list))
                     ((eql-test-p test)
                      `(adjoin-eql ,elt ,list))
                     (t nil)))
          whole))
    whole))

(define-compiler-macro union (&whole whole list1 list2 &rest keys)
  (if (constant-keywords-p keys)
    (destructuring-bind (&key (test ''eql) test-not key) keys
      (or (and (null test-not)
               (null key)
               (cond ((eq-test-p test)
                      `(union-eq ,list1 ,list2))
                     ((eql-test-p test)
                      `(union-eql ,list1 ,list2))
                     (t nil)))
          whole))
    whole))

(define-compiler-macro slot-value (&whole whole &environment env
                                          instance slot-name-form)
  (declare (ignore env))
  (let* ((name (and (quoted-form-p slot-name-form)
                    (typep (cadr slot-name-form) 'symbol)
                    (cadr slot-name-form))))
    (if name
      `(slot-id-value ,instance (load-time-value (ensure-slot-id ',name)))
      whole)))


(define-compiler-macro set-slot-value (&whole whole &environment env
                                          instance slot-name-form value-form)
  (declare (ignore env))
  (let* ((name (and (quoted-form-p slot-name-form)
                    (typep (cadr slot-name-form) 'symbol)
                    (cadr slot-name-form))))
    (if name
      `(set-slot-id-value
        ,instance
        (load-time-value (ensure-slot-id ',name))
        ,value-form)
      whole)))




(defsynonym %get-unsigned-byte %get-byte)
(defsynonym %get-unsigned-word %get-word)
(defsynonym %get-signed-long %get-long)




(define-compiler-macro arrayp (arg)
  `(>= (the fixnum (typecode ,arg))
    ,(nx-lookup-target-uvector-subtag :array-header)))

(define-compiler-macro vectorp (arg)
  `(>= (the fixnum (typecode ,arg))
    ,(nx-lookup-target-uvector-subtag :vector-header)))



(define-compiler-macro fixnump (arg)
  (let* ((fixnum-tag
          (arch::target-fixnum-tag (backend-target-arch *target-backend*))))
    `(eql (lisptag ,arg) ,fixnum-tag)))



(define-compiler-macro double-float-p (n)
  (let* ((tag (arch::target-double-float-tag (backend-target-arch *target-backend*))))
    `(eql (typecode ,n) ,tag)))


(define-compiler-macro short-float-p (n)
  (let* ((arch (backend-target-arch *target-backend*))
         (tag (arch::target-single-float-tag arch))
         (op (if (arch::target-single-float-tag-is-subtag arch)
               'typecode
               'fulltag)))
    `(eql (,op ,n) ,tag)))


(define-compiler-macro floatp (n)
  (let* ((typecode (make-symbol "TYPECODE"))
         (arch (backend-target-arch *target-backend*))
         (single (arch::target-single-float-tag arch))
         (double (arch::target-double-float-tag arch)))
    `(let* ((,typecode (typecode ,n)))
       (declare (fixnum ,typecode))
       (or (= ,typecode ,single)
           (= ,typecode ,double)))))

(define-compiler-macro functionp (n)
  (let* ((arch (backend-target-arch *target-backend*))
         (tag (arch::target-function-tag arch))
         (op (if (arch::target-function-tag-is-subtag arch)
               'typecode
               'fulltag)))
    `(eql (,op  ,n) ,tag)))

(define-compiler-macro symbolp (s)
  (let* ((arch (backend-target-arch *target-backend*))
         (symtag (arch::target-symbol-tag arch))
         (op (if (arch::target-symbol-tag-is-subtag arch)
               'typecode
               'fulltag))
         (niltag (arch::target-null-tag arch)))
    (if (eql niltag symtag)
      `(eql (,op ,s) ,symtag)
      (let* ((sym (gensym)))
        `(let* ((,sym ,s))
          (if ,sym (eql (,op ,sym) ,symtag) t))))))

;;; If NIL isn't tagged as a symbol, assume that LISPTAG only looks
;;; at bits that NIL shares with a cons.
(define-compiler-macro listp (n)
  (let* ((arch (backend-target-arch *target-backend*))
         (cons-tag (arch::target-cons-tag arch))
         (nil-tag  (arch::target-null-tag arch))
         (symbol-tag (arch::target-symbol-tag arch)))
    (if (= nil-tag symbol-tag)
      (let* ((nvar (gensym)))
        `(let* ((,nvar ,n))
          (if ,nvar (consp ,nvar) t)))
      `(eql (lisptag ,n) ,cons-tag))))

(define-compiler-macro consp (&whole call n)
  (let* ((arch (backend-target-arch *target-backend*))
	 (cons-tag (arch::target-cons-tag arch))
	 (nil-tag (arch::target-null-tag arch)))
    (if (= nil-tag cons-tag)
      call
      `(eql (fulltag ,n) ,cons-tag))))

(define-compiler-macro bignump (n)
  `(eql (typecode ,n) ,(nx-lookup-target-uvector-subtag :bignum)))

(define-compiler-macro ratiop (n)
  `(eql (typecode ,n) ,(nx-lookup-target-uvector-subtag :ratio)))

(define-compiler-macro complexp (n)
  `(eql (typecode ,n) ,(nx-lookup-target-uvector-subtag :complex)))

(define-compiler-macro macptrp (n)
  `(eql (typecode ,n) ,(nx-lookup-target-uvector-subtag :macptr)))

(define-compiler-macro basic-stream-p (n)
  `(eql (typecode ,n) ,(nx-lookup-target-uvector-subtag :basic-stream)))

(define-compiler-macro aref (&whole call a &rest subscripts &environment env)
  (let* ((ctype (if (nx-form-typep a 'array env)
                  (specifier-type (nx-form-type a env) env)))
         (type (if ctype (type-specifier (array-ctype-specialized-element-type ctype))))
         (useful (unless (or (eq type *) (eq type t))
                   type)))
    (if (= 2 (length subscripts))
      (setq call `(%aref2 ,a ,@subscripts))
      (if (= 3 (length subscripts))
        (setq call `(%aref3 ,a ,@subscripts))))
    (if useful
      `(the ,useful ,call)
      call)))


(define-compiler-macro aset (&whole call a &rest subs&val)
  (if (= 3 (length subs&val))
    `(%aset2 ,a ,@subs&val)
    (if (= 4 (length subs&val))
      `(%aset3 ,a ,@subs&val)
      call)))


(define-compiler-macro make-sequence (&whole call &environment env typespec len &rest keys &key initial-element)
  (declare (ignore typespec len keys initial-element))
  call)

(define-compiler-macro make-string (&whole call size &rest keys)
  (if (constant-keywords-p keys)
    (destructuring-bind (&key (element-type () element-type-p)
                              (initial-element () initial-element-p))
                        keys
      (if (and element-type-p
               (quoted-form-p element-type))
        (let* ((element-type (cadr element-type)))
          (if (subtypep element-type 'base-char)
            `(allocate-typed-vector :simple-string ,size ,@(if initial-element-p `(,initial-element)))
            call))
        (if (not element-type-p)
          `(allocate-typed-vector :simple-string ,size ,@(if initial-element-p `(,initial-element)))
          call)))
    call))

(define-compiler-macro make-string-output-stream (&whole whole &rest keys)
  (if (null keys)
    '(make-simple-string-output-stream)
    whole))


(define-compiler-macro sbit (&environment env &whole call v &optional sub0 &rest others)
  (if (and sub0 (null others))
    `(aref (the simple-bit-vector ,v) ,sub0)
    call))

(define-compiler-macro %sbitset (&environment env &whole call v sub0 &optional (newval nil newval-p) &rest newval-was-really-sub1)
  (if (and newval-p (not newval-was-really-sub1) )
    `(setf (aref (the simple-bit-vector ,v) ,sub0) ,newval)
    call))

(define-compiler-macro simple-base-string-p (thing)
  `(= (the fixnum (typecode ,thing)) ,(nx-lookup-target-uvector-subtag :simple-string)))

(define-compiler-macro simple-string-p (thing)
  `(simple-base-string-p ,thing))

(define-compiler-macro stringp (thing)
  `(base-string-p  ,thing))

(define-compiler-macro base-string-p (thing)
  (let* ((gthing (gensym))
         (gtype (gensym)))
    `(let* ((,gthing ,thing)
            (,gtype (typecode ,gthing)))
      (declare (type (unsigned-byte 8) ,gtype))
      (if (= ,gtype ,(nx-lookup-target-uvector-subtag :vector-header))
        (= (the (unsigned-byte 8)
             (ldb target::arrayH.flags-cell-subtag-byte (the fixnum (%svref ,gthing target::arrayH.flags-cell))))
           ,(nx-lookup-target-uvector-subtag :simple-string))
        (= ,gtype ,(nx-lookup-target-uvector-subtag :simple-string))))))



(defsetf %misc-ref %misc-set)

(define-compiler-macro lockp (lock)
  (let* ((tag (nx-lookup-target-uvector-subtag :lock)))
    `(eq ,tag (typecode ,lock))))

(define-compiler-macro structurep (s)
  (let* ((tag (nx-lookup-target-uvector-subtag :struct)))
    `(eq ,tag (typecode ,s))))
  

(define-compiler-macro integerp (thing)
  (let* ((typecode (gensym))
         (fixnum-tag (arch::target-fixnum-tag (backend-target-arch *target-backend*)))
         (bignum-tag (nx-lookup-target-uvector-subtag :bignum)))
    `(let* ((,typecode (typecode ,thing)))
      (declare (fixnum ,typecode))
      (if (= ,typecode ,fixnum-tag)
        t
        (= ,typecode ,bignum-tag)))))

(define-compiler-macro %composite-pointer-ref (size pointer offset)
  (if (constantp size)
    `(%inc-ptr ,pointer ,offset)
    `(progn
      ,size
      (%inc-ptr ,pointer ,offset))))


(define-compiler-macro char= (&whole call ch &optional (other nil other-p) &rest others)
  (if (null others)
    (if other-p
      `(eq (char-code ,ch) (char-code ,other))
      `(progn (char-code ,ch) t))
    (if (null (cdr others))
      (let* ((third (car others))
             (code (gensym))
             (code2 (gensym))
             (code3 (gensym)))
        `(let* ((,code (char-code ,ch))
                (,code2 (char-code ,other))
                (,code3 (char-code ,third)))
          (and (eq ,code ,code2)
           (eq ,code2 ,code3))))
      call)))

(define-compiler-macro char-equal (&whole call ch &optional (other nil other-p) &rest others)
  (if (null others)
    (if other-p
      `(eq (%char-code-upcase (char-code ,ch)) (%char-code-upcase (char-code ,other)))
      `(progn (char-code ,ch) t))
    (if (null (cdr others))
      (let* ((third (car others))
             (code (gensym))
             (code2 (gensym))
             (code3 (gensym)))
        `(let* ((,code (%char-code-upcase (char-code ,ch)))
                (,code2 (%char-code-upcase (char-code ,other)))
                (,code3 (%char-code-upcase (char-code ,third))))
          (and (eq ,code ,code2)
           (eq ,code ,code3))))
      call)))

(define-compiler-macro char/= (&whole call ch &optional (other nil other-p) &rest others)
  (if (null others)
    (if other-p
      `(not (eq (char-code ,ch) (char-code ,other)))
      `(progn (char-code ,ch) t))
    call))


(define-compiler-macro char< (&whole call ch &optional (other nil other-p) &rest others)
  (if (null others)
    (if other-p
      `(< (the fixnum (char-code ,ch)) (the fixnum (char-code ,other)))
      `(progn (char-code ,ch) t))
    (if (null (cdr others))
      (let* ((third (car others))
             (code (gensym))
             (code2 (gensym))
             (code3 (gensym)))
        ;; We have to evaluate all forms for side-effects.
        ;; Hopefully, there won't be any
        `(let* ((,code (char-code ,ch))
                (,code2 (char-code ,other))
                (,code3 (char-code ,third)))
          (declare (fixnum ,code ,code2 ,code3))
          (and (< ,code ,code2)
           (< ,code2 ,code3))))
      call)))

(define-compiler-macro char<= (&whole call ch &optional (other nil other-p) &rest others)
  (if (null others)
    (if other-p
      `(<= (the fixnum (char-code ,ch)) (the fixnum (char-code ,other)))
      `(progn (char-code ,ch) t))
    (if (null (cdr others))
      (let* ((third (car others))
             (code (gensym))
             (code2 (gensym))
             (code3 (gensym)))
        `(let* ((,code (char-code ,ch))
                (,code2 (char-code ,other))
                (,code3 (char-code ,third)))
          (declare (fixnum ,code ,code2 ,code3))
          (and (<= ,code ,code2)
           (<= ,code2 ,code3))))
      call)))

(define-compiler-macro char> (&whole call ch &optional (other nil other-p) &rest others)
  (if (null others)
    (if other-p
      `(> (the fixnum (char-code ,ch)) (the fixnum (char-code ,other)))
      `(progn (char-code ,ch) t))
    (if (null (cdr others))
      (let* ((third (car others))
             (code (gensym))
             (code2 (gensym))
             (code3 (gensym)))
        `(let* ((,code (char-code ,ch))
                (,code2 (char-code ,other))
                (,code3 (char-code ,third)))
          (declare (fixnum ,code ,code2 ,code3))
          (and (> ,code ,code2)
           (> ,code2 ,code3))))
      call)))

(define-compiler-macro char>= (&whole call ch &optional (other nil other-p) &rest others)
  (if (null others)
    (if other-p
      `(>= (the fixnum (char-code ,ch)) (the fixnum (char-code ,other)))
      `(progn (char-code ,ch) t))
    (if (null (cdr others))
      (let* ((third (car others))
             (code (gensym))
             (code2 (gensym))
             (code3 (gensym)))
        `(let* ((,code (char-code ,ch))
                (,code2 (char-code ,other))
                (,code3 (char-code ,third)))
          (declare (fixnum ,code ,code2 ,code3))
          (and (>= ,code ,code2)
           (>= ,code2 ,code3))))
      call)))

(define-compiler-macro float (&whole call number &optional (other 0.0f0 other-p) &environment env)

  (cond ((and (typep other 'single-float)
              (nx-form-typep number 'double-float env))
         `(the single-float (%double-to-single ,number)))
        ((and (typep other 'double-float)
              (nx-form-typep number 'single-float env))
         `(the double-float (%single-to-double ,number)))
        ((and other-p (typep other 'single-float))
         `(the single-float (%short-float ,number)))
        ((typep other 'double-float)
         `(the double-float (%double-float ,number)))
        ((null other-p)
         (let* ((temp (gensym)))
           `(let* ((,temp ,number))
             (if (typep ,temp 'double-float)
               ,temp
               (the single-float (%short-float ,temp))))))
        (t call)))

(define-compiler-macro coerce (&whole call thing type)
  (if (quoted-form-p type)
    (setq type (cadr type)))
  (if (ignore-errors (subtypep type 'single-float))
    `(float ,thing 0.0f0)
    (if (ignore-errors (subtypep type 'double-float))
      `(float ,thing 0.0d0)
      call)))

(define-compiler-macro equal (&whole call x y &environment env)
  (if (or (equal-iff-eql-p x env)
          (equal-iff-eql-p y env))
    `(eql ,x ,y)
    call))

(define-compiler-macro instance-slots (&whole w instance)
  (if (and (constantp instance)
           (eql (typecode instance) (nx-lookup-target-uvector-subtag :instance)))
    `(instance.slots ,instance)
    (let* ((itemp (gensym))
           (typecode (gensym)))
      `(let* ((,itemp ,instance)
              (,typecode (typecode ,itemp)))
        (declare (type (unsigned-byte 8) ,typecode))
        (if (eql ,typecode ,(nx-lookup-target-uvector-subtag :instance))
          (instance.slots ,itemp)
          (%non-standard-instance-slots ,itemp ,typecode))))))


(define-compiler-macro instance-class-wrapper (instance)
  (let* ((itemp (gensym)))
    `(let* ((,itemp ,instance))
      (if (eql (the (unsigned-byte 8) (typecode ,itemp))
               ,(nx-lookup-target-uvector-subtag :instance))
        (instance.class-wrapper ,itemp)
        (non-standard-instance-class-wrapper ,itemp)))))

;; Instance must be a standard-instance.
(define-compiler-macro %class-of-instance (instance)
  `(%wrapper-class (instance.class-wrapper ,instance)))

(define-compiler-macro unsigned-byte-p (x)
  (if (typep (nx-unquote x) 'unsigned-byte)
    t
    (let* ((val (gensym)))
      `(let* ((,val ,x))
        (and (integerp ,val) (not (< ,val 0)))))))

(define-compiler-macro string-equal (&whole w s1 s2 &rest keys)
  (if (null keys)
    `(%fixed-string-equal ,s1 ,s2)
    (let* ((s1-arg (gensym))
           (s2-arg (gensym)))
      `(funcall
        (lambda (,s1-arg ,s2-arg &key start1 end1 start2 end2)
          (%bounded-string-equal ,s1-arg ,s2-arg start1 end1 start2 end2))
        ,s1 ,s2 ,@keys))))


;;; Try to use "package-references" to speed up package lookup when
;;; a package name is used as a constant argument to some functions.

(defun package-ref-form (arg)
  (when (and arg (constantp arg) (typep (setq arg (nx-unquote arg))
                                        '(or symbol string)))
    `(load-time-value (register-package-ref ,(string arg)))))



(define-compiler-macro intern (&whole w string &optional package)
  (let* ((ref (package-ref-form package)))
    (if (or ref
            (setq ref (and (consp package)
                           (eq (car package) 'find-package)
                           (consp (cdr package))
                           (null (cddr package))
                           (package-ref-form (cadr package)))))
      `(%pkg-ref-intern ,string ,ref)
      w)))

(define-compiler-macro find-symbol (&whole w string &optional package)
  (let* ((ref (package-ref-form package)))
    (if (or ref
            (setq ref (and (consp package)
                           (eq (car package) 'find-package)
                           (consp (cdr package))
                           (null (cddr package))
                           (package-ref-form (cadr package)))))
      `(%pkg-ref-find-symbol ,string ,ref)
      w)))

(define-compiler-macro find-package (&whole w package)
  (let* ((ref (package-ref-form package)))
    (if ref
      `(package-ref.pkg ,ref)
      w)))

(define-compiler-macro pkg-arg (&whole w package &optional allow-deleted)
  (let* ((ref (unless allow-deleted (package-ref-form package))))
    (if ref
      (let* ((r (gensym)))
        `(let* ((,r ,ref))
          (or (package-ref.pkg ,ref)
           (%kernel-restart $xnopkg (package-ref.pkg ,r)))))
      w)))



(define-compiler-macro %char-code-case-fold (&whole w code vector &environment env)
  (if (nx-open-code-in-line env)
    (let* ((c (gensym))
           (table (gensym)))
      `(let* ((,c ,code)
              (,table ,vector))
        (declare (type (mod #x110000) ,c)
                 (type (simple-array (signed-byte 16) (*)) ,table))
        (if (< ,c (length ,table))
          (the fixnum (+ ,c (the (signed-byte 16)
                              (locally (declare (optimize (speed 3) (safety 0)))
                                (aref ,table ,c)))))
          ,c)))
    w))
        
(define-compiler-macro %char-code-upcase (&whole w code &environment env)
  (if (typep code '(mod #x110000))
    (%char-code-upcase code)
    `(%char-code-case-fold ,code *lower-to-upper*)))

(define-compiler-macro %char-code-downcase (&whole w code &environment env)
  (if (typep code '(mod #x110000))
    (%char-code-downcase code)
    `(%char-code-case-fold ,code *upper-to-lower*)))

(define-compiler-macro char-upcase (char)
  `(code-char (the valid-char-code (%char-code-upcase (char-code ,char)))))

(define-compiler-macro char-downcase (char)
  `(code-char (the valid-char-code (%char-code-downcase (char-code ,char)))))


(define-compiler-macro register-istruct-cell (&whole w arg)
  (if (and (quoted-form-p arg)
           (cadr arg)
           (typep (cadr arg) 'symbol))
    `',(register-istruct-cell (cadr arg))
    w))

(define-compiler-macro get-character-encoding (&whole w name)
  (or (if (typep name 'keyword) (lookup-character-encoding name))
      w))

(define-compiler-macro read-char (&optional stream (eof-error-p t) eof-value recursive-p)
  `(read-char-internal ,stream ,eof-error-p (values ,eof-value ,recursive-p)))


(provide "OPTIMIZERS")

