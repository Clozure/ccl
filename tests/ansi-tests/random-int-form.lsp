;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Sep 10 18:03:52 2003
;;;; Contains: Simple randon form generator/tester

(in-package :cl-test)

(compile-and-load "random-aux.lsp")

;;;
;;; This file contains a routine for generating random legal Common Lisp functions
;;; for differential testing.
;;;
;;; To run the random tests by themselves, start a lisp in the ansi-tests directory
;;; and do the following:
;;;   (load "gclload1.lsp")
;;;   (compile-and-load "random-int-form.lsp")
;;;   (in-package :cl-test)
;;;   (let ((*random-state* (make-random-state t)))
;;;      (test-random-integer-forms 100 4 10000)) ;; or other parameters
;;;
;;; If a test breaks during testing the variables *optimized-fn-src*,
;;; *unoptimized-fn-src*, and *int-form-vals* can be used to get the source
;;; of the optimized/unoptimized lambda forms being compiled, and the arguments
;;; on which they are called.
;;;
;;; If a difference is found between optimized/unoptimized functions the forms,
;;; values, and results are collected.  A list of all these discrepancies is returned
;;; after testing finishes (assuming nothing breaks).
;;;
;;; The variable *compile-unoptimized-form* controls whether the low optimization
;;; form is compiled, or if a form funcalling it is EVALed.  The latter is often
;;; faster, and may find more problems since an interpreter and compiler may evaluate
;;; forms in very different ways.
;;;
;;; The rctest/ subdirectory contains fragments of a more OO random form generator
;;; that will eventually replace this preliminary effort.
;;;
;;; The file misc.lsp contains tests that were mostly for bugs found by this
;;; random tester in various Common Lisp implementations.
;;;

(declaim (special *optimized-fn-src* *unoptimized-fn-src* *int-form-vals*
                  *opt-result* *unopt-result* $x $y $z
                  *compile-unoptimized-form*
                  *make-random-integer-form-cdf*))

;;; Little functions used to run collected tests.
;;; (f i) runs the ith collected optimized test
;;; (g i) runs the ith collected unoptimized test
;;; (p i) prints the ith test (forms, input values, and other information)

(defun f (i) (let ((plist (elt $y i)))
               (apply (compile nil (getf plist :optimized-lambda-form))
                      (getf plist :vals))))

(defun g (i) (let ((plist (elt $y i)))
               (if *compile-unoptimized-form*
                   (apply (compile nil (getf plist :unoptimized-lambda-form))
                          (getf plist :vals))
                 (apply (the function (eval `(function ,(getf plist :unoptimized-lambda-form))))
                        (getf plist :vals)))))

(defun p (i) (write (elt $y i) :pretty t :escape t) (values))

(defun load-failures (&key (pathname "failures.lsp"))
  (length (setq $y (with-open-file (s pathname :direction :input)
                                   (loop for x = (read s nil)
                                         while x collect x)))))

(defun tn (n &optional (size 100))
  (length (setq $y (prune-results (setq $x (test-random-integer-forms size 2 n))))))

(declaim (special *s1* *s2* *s3* *s4* *s5* *s6* *s7* *s8* *s9*))

(defparameter *random-special-vars*
  #(*s1* *s2* *s3* *s4* *s5* *s6* *s7* *s8* *s9*))

(defparameter *loop-random-int-form-period* 2000)

(defmacro cl-handler-bind (&rest args)
  `(cl:handler-bind ,@args))

(defmacro cl-handler-case (&rest args)
  `(cl:handler-case ,@args))

(eval-when
 (:compile-toplevel :load-toplevel :execute)
 (defun cumulate (vec)
   (loop for i from 1 below (length vec)
         do (incf (aref vec i) (aref vec (1- i))))
   vec))

(defparameter *default-make-random-integer-form-cdf*
  (cumulate (copy-seq #(10 5 40 4 5 4 2 2 10 1 1 #-armedbead 1 #-armedbear 1
                       #-allegro 5 5 5 #-(or gcl ecl armedbear) 2
                       2 #-(or cmu allegro poplog) 5 4 30
                       4 20 3 2 2 1 1 5 30 #-poplog 5
                       #-(or allegro poplog) 10
                       50 4 4 10 20 10 10 3
                       20 5 #-(or armedbear) 20
                       2 2 2))))

(defparameter *make-random-integer-form-cdf*
  (copy-seq *default-make-random-integer-form-cdf*))

(eval-when
 (:compile-toplevel :load-toplevel :execute)
 (defmacro with-random-integer-form-params (&body forms)
   (let ((len (gensym "LEN"))
         (vec (gensym "VEC")))
     `(let* ((,len (length *default-make-random-integer-form-cdf*))
             (,vec (make-array ,len)))
        (loop for i from 0 below ,len do (setf (aref ,vec i)
					       (1+ (min (random 100)
							(random 100)))))
        (setq ,vec (cumulate ,vec))
        (let ((*make-random-integer-form-cdf* ,vec))
          ,@forms)))))

;;; Run the random tester, collecting failures into the special
;;; variable $y.

(defun loop-random-int-forms (&optional (size 200) (nvars 3))
  (unless (boundp '$x) (setq $x nil))
  (unless (boundp '$y) (setq $y nil))
  (loop
   for i from 1
   do
   (format t "~6D | " i)
   (finish-output *standard-output*)
   (let ((x (test-random-integer-forms
             size nvars *loop-random-int-form-period*
             :index (* (1- i) *loop-random-int-form-period*))))
     (when x
       (setq $x (append $x x))
       (setq x (prune-results x))
       (terpri) (print x) (finish-output *standard-output*)
       (setq $y (append $y x)))
     (terpri))))

(defvar *random-int-form-blocks* nil)
(defvar *random-int-form-catch-tags* nil)
(defvar *go-tags* nil)

(defvar *random-vals-list-bound* 10)

(defvar *max-compile-time* 0)
(defvar *max-compile-term* nil)

(defvar *print-immediately* nil)

(defvar *compile-unoptimized-form*
  #+(or allegro sbcl) t
  #-(or allegro sbcl) nil)

(declaim (special *vars*))

(defstruct var-desc
  (name nil :type symbol)
  (type t))

(defun test-random-integer-forms
  (size nvars n
        &key ((:random-state *random-state*) (make-random-state t))
        (file-prefix "b")
        (index 0)
        (random-size nil)
        (random-nvars nil)
        )

  "Generate random integer forms of size SIZE with NVARS variables.
   Do this N times, returning all those on which a discrepancy
   is found between optimized and nonoptimize, notinlined code."

  (assert (integerp nvars))
  (assert (<= 1 nvars 26))
  (assert (and (integerp n) (plusp n)))
  (assert (and (integerp n) (plusp size)))

  (loop for i from 1 to n
        do (when (= (mod i 100) 0)
              ;; #+sbcl (print "Do gc...")
              ;; #+sbcl (sb-ext::gc :full t)
              ;; #+lispworks-personal-edition (cl-user::normal-gc)
              (prin1 i) (princ " ") (finish-output *standard-output*))
         nconc (let ((result (test-random-integer-form
                              (if random-size (1+ (random size)) size)
                              (if random-nvars (1+ (random nvars)) nvars)
                              :index (+ index i)
                              :file-prefix file-prefix)))
                 (when result
                   (let ((*print-readably* nil))
                     (format t "~%~A~%" (format nil "~S" (car result)))
                     (finish-output *standard-output*)))
                 result)))

(defun test-random-integer-form
  (size nvars &key (index 0) (file-prefix "b"))
  (let* ((vars (subseq '(a b c d e f g h i j k l m
                           n o p q r s u v w x y z) 0 nvars))
         (var-ranges (mapcar #'make-random-integer-range vars))
         (var-types (mapcar #'(lambda (range)
                                (let ((lo (car range))
                                      (hi (cadr range)))
                                  (assert (>= hi lo))
                                  `(integer ,lo ,hi)))
                            var-ranges))
         (form (let ((*vars* (loop for v in vars
                                   for tp in var-types
                                   collect (make-var-desc :name v
                                                          :type tp)))
                     (*random-int-form-blocks* nil)
                     (*random-int-form-catch-tags* nil)
                     (*go-tags* nil)
                     )
                 (with-random-integer-form-params
                  (make-random-integer-form (1+ (random size))))))
         (vals-list
          (loop repeat *random-vals-list-bound*
                collect
                (mapcar #'(lambda (range)
                            (let ((lo (car range))
                                  (hi (cadr range)))
                              (random-from-interval (1+ hi) lo)))
                        var-ranges)))
         (opt-decls-1 (make-random-optimize-settings))
         (opt-decls-2 (make-random-optimize-settings)))
    (when *print-immediately*
      (with-open-file
       (s (format nil "~A~A.lsp" file-prefix index)
          :direction :output :if-exists :error)
       (print `(defparameter *x*
                 '(:vars ,vars
                      :var-types ,var-types
                      :vals-list ,vals-list
                      :decls1 ,opt-decls-1
                      :decls2 ,opt-decls-2
                      :form ,form))
              s)
       (print '(load "c.lsp") s)
       (finish-output s))
       ;; (cl-user::gc)
       ;; (make-list 1000000) 
      )
    (test-int-form form vars var-types vals-list opt-decls-1 opt-decls-2)))

(defun make-random-optimize-settings ()
  (loop for settings = (list*
                        (list 'speed (random 4))
                        #+sbcl '(sb-c:insert-step-conditions 0)
                        (loop for s in '(space safety debug compilation-speed)
                              for n = (random 4)
                              collect (list s n)))
        while
        #+allegro (subsetp '((speed 3) (safety 0)) settings :test 'equal)
        #-allegro nil
        finally (return (random-permute settings))))

(defun fn-symbols-in-form (form)
  "Return a list of the distinct standardized lisp function
   symbols occuring ing FORM.  These are used to generate a NOTINLINE
   declaration for the unoptimized form."
  (intersection
   (remove-duplicates (fn-symbols-in-form* form) :test #'eq)
   *cl-function-or-accessor-symbols*))

(defun fn-symbols-in-form* (form)
  (when (consp form)
    (if (symbolp (car form))
        (cons (car form) (mapcan #'fn-symbols-in-form* (cdr form)))
      (mapcan #'fn-symbols-in-form* form))))

(defun fn-arg-name (fn-name arg-index)
  (intern (concatenate 'string
                       (subseq (symbol-name fn-name) 1)
                       (format nil "-~D" arg-index))
          (symbol-package fn-name)))                       

(declaim (special *flet-names*))
(defparameter *flet-names* nil)



(defun random-var-desc ()
  (loop
   (let* ((pos (random (length *vars*)))
          (desc (elt *vars* pos)))
     (when (= pos (position (var-desc-name desc) (the list *vars*)
                            :key #'var-desc-name))
       (return desc)))))

(defun is-zero-rank-integer-array-type (type)
  "This function was introduced because of a bug in ACL 6.2"
  ; (subtypep type '(array integer 0))
  (and (consp type)
       (eq (car type) 'array)
       (cddr type)
       (or (eq (cadr type) '*)
           (subtypep (cadr type) 'integer))
       (or (eql (caddr type) 0)
           (null (caddr type)))))

(defun make-random-integer-form (size)
  "Generate a random legal lisp form of size SIZE (roughly)."

  (if (<= size 1)
      ;; Leaf node -- generate a variable, constant, or flet function call
      (loop
       when
       (rcase
        (10 (make-random-integer))
        (9 (if *vars*
               (let* ((desc (random-var-desc))
                      (type (var-desc-type desc))
                      (name (var-desc-name desc)))
                 (cond
                  ((subtypep type 'integer) name)
                  (; (subtypep type '(array integer 0))
                   (is-zero-rank-integer-array-type type)
                   `(aref ,name))
                  ((subtypep type '(cons integer integer))
                   (rcase (1 `(car ,name))
                          (1 `(cdr ,name))))
                  (t nil)))
             nil))
        (1 (if *go-tags* `(go ,(random-from-seq *go-tags*)) nil))
        (2 (if *flet-names*
               (let* ((flet-entry (random-from-seq *flet-names*))
                      (flet-name (car flet-entry))
                      (flet-minargs (cadr flet-entry))
                      (flet-maxargs (caddr flet-entry))
                      (nargs (random-from-interval (1+ flet-maxargs) flet-minargs))
                      (args (loop repeat nargs
                                  collect (make-random-integer-form 1))))
                 `(,flet-name ,@args))
             nil)))
       return it)
    ;; (> size 1)
    (rselect *make-random-integer-form-cdf*

     ;; flet call
     (make-random-integer-flet-call-form size)
     (make-random-aref-form size)
     ;; Unary ops
     (let ((op (random-from-seq '(- abs signum 1+ 1- conjugate
                                     rational rationalize
                                     numerator denominator
                                     identity progn floor
                                     ;; #-(or armedbear)
                                     ignore-errors
                                     cl:handler-case
                                     restart-case
                                     ceiling truncate round realpart imagpart
                                     integer-length logcount values
                                     locally))))
        `(,op ,(make-random-integer-form (1- size))))

     (make-random-integer-unwind-protect-form size)
     (make-random-integer-mapping-form size)

     ;; prog1, multiple-value-prog1
     (let* ((op (random-from-seq #(prog1 multiple-value-prog1)))
            (nforms (random 4))
            (sizes (random-partition (1- size) (1+ nforms)))
            (args (mapcar #'make-random-integer-form sizes)))
       `(,op ,@args))

     ;; prog2
     (let* ((nforms (random 4))
            (sizes (random-partition (1- size) (+ nforms 2)))
            (args (mapcar #'make-random-integer-form sizes)))
       `(prog2 ,@args))
     
     `(isqrt (abs ,(make-random-integer-form (- size 2))))

     `(the integer ,(make-random-integer-form (1- size)))
      
     `(cl:handler-bind nil ,(make-random-integer-form (1- size)))
     `(restart-bind nil ,(make-random-integer-form (1- size)))
     #-armedbear
     `(macrolet () ,(make-random-integer-form (1- size)))
     #-armedbear
     `(symbol-macrolet () ,(make-random-integer-form (1- size)))

     ;; dotimes
     #-allegro
     (let* ((var (random-from-seq #(iv1 iv2 iv3 iv4)))
            (count (random 4))
            (sizes (random-partition (1- size) 2))
            (body (let ((*vars* (cons (make-var-desc :name var :type nil)
                                      *vars*)))
                    (make-random-integer-form (first sizes))))
            (ret-form (make-random-integer-form (second sizes))))
       (unless (consp body) (setq body `(progn ,body)))
       `(dotimes (,var ,count ,ret-form) ,body))

     ;; loop
     (make-random-loop-form (1- size))

     (make-random-count-form size)

     #-(or gcl ecl armedbear)
     ;; load-time-value
     (let ((arg (let ((*flet-names* nil)
                      (*vars* nil)
                      (*random-int-form-blocks* nil)
                      (*random-int-form-catch-tags* nil)
                      (*go-tags* nil))
                  (make-random-integer-form (1- size)))))
       (rcase
        (4 `(load-time-value ,arg t))
        (2 `(load-time-value ,arg))
        (2 `(load-time-value ,arg nil))))

     ;; eval
     (make-random-integer-eval-form size)
      
     #-(or cmu allegro poplog)
     (destructuring-bind (s1 s2)
        (random-partition (- size 2) 2)
        `(ash ,(make-random-integer-form s1)
              (min ,(random 100)
                   ,(make-random-integer-form s2))))
     
     ;; binary floor, ceiling, truncate, round
     (let ((op (random-from-seq #(floor ceiling truncate round mod rem)))
           (op2 (random-from-seq #(max min))))
       (destructuring-bind (s1 s2)
          (random-partition (- size 2) 2)
          `(,op  ,(make-random-integer-form s1)
                 (,op2  ,(if (eq op2 'max)
                             (1+ (random 100))
                           (- (1+ (random 100))))
                        ,(make-random-integer-form s2)))))
            
     ;; Binary op
     (let* ((op (random-from-seq
                  '(+ - *  logand min max gcd
                      lcm
                      #-:allegro
                      logandc1
                      logandc2 logeqv logior lognand lognor
                      #-:allegro
                      logorc1
                      logorc2
                      logxor
                      ))))
        (destructuring-bind (leftsize rightsize)
            (random-partition (1- size) 2)
          (let ((e1 (make-random-integer-form leftsize))
                (e2 (make-random-integer-form rightsize)))
            `(,op ,e1 ,e2))))

     ;; boole
     (let* ((op (random-from-seq
                  #(boole-1 boole-2 boole-and boole-andc1 boole-andc2
                    boole-c1 boole-c2 boole-clr boole-eqv boole-ior boole-nand
                    boole-nor boole-orc1 boole-orc2 boole-set boole-xor))))
        (destructuring-bind (leftsize rightsize)
            (random-partition (- size 2) 2)
          (let ((e1 (make-random-integer-form leftsize))
                (e2 (make-random-integer-form rightsize)))
            `(boole ,op ,e1 ,e2))))

     ;; n-ary ops
     (let* ((op (random-from-seq #(+ - * logand min max
                                      logior values lcm gcd logxor)))
             (nmax (case op ((* lcm gcd) 4) (t (1+ (random 40)))))
             (nargs (1+ (min (random nmax) (random nmax))))
             (sizes (random-partition (1- size) nargs))
             (args (mapcar #'make-random-integer-form sizes)))
        `(,op ,@args))

     ;; expt
     `(expt ,(make-random-integer-form (1- size)) ,(random 3))

     ;; coerce
     `(coerce ,(make-random-integer-form (1- size)) 'integer)
     
     ;; complex (degenerate case)
     `(complex ,(make-random-integer-form (1- size)) 0)

     ;; quotient (degenerate cases)
     `(/ ,(make-random-integer-form (1- size)) 1)
     `(/ ,(make-random-integer-form (1- size)) -1)

     ;; tagbody
     (make-random-tagbody-and-progn size)

     ;; conditionals
     (let* ((cond-size (random (max 1 (floor size 2))))
            (then-size (random (- size cond-size)))
            (else-size (- size 1 cond-size then-size))
            (pred (make-random-pred-form cond-size))
            (then-part (make-random-integer-form then-size))
            (else-part (make-random-integer-form else-size)))
       `(if ,pred ,then-part ,else-part))
     #-poplog
      (destructuring-bind (s1 s2 s3) (random-partition (1- size) 3)
        `(,(random-from-seq '(deposit-field dpb))
          ,(make-random-integer-form s1)
          ,(make-random-byte-spec-form s2)
          ,(make-random-integer-form s3)))

     #-(or allegro poplog)
      (destructuring-bind (s1 s2) (random-partition (1- size) 2)
          `(,(random-from-seq '(ldb mask-field))
            ,(make-random-byte-spec-form s1)
            ,(make-random-integer-form s2)))

     (make-random-integer-binding-form size)

     ;; progv
     (make-random-integer-progv-form size)
     
     `(let () ,(make-random-integer-form (1- size)))

      (let* ((name (random-from-seq #(b1 b2 b3 b4 b5 b6 b7 b8)))
             (*random-int-form-blocks* (adjoin name *random-int-form-blocks*)))
        `(block ,name ,(make-random-integer-form (1- size))))

      (let* ((tag (list 'quote (random-from-seq #(ct1 ct2 ct2 ct4 ct5 ct6 ct7 ct8))))
             (*random-int-form-catch-tags* (cons tag *random-int-form-catch-tags*)))
        `(catch ,tag ,(make-random-integer-form (1- size))))
     
      ;; setq and similar
      (make-random-integer-setq-form size)

      (make-random-integer-case-form size)

      (if *random-int-form-blocks*
          (let ((name (random-from-seq *random-int-form-blocks*))
                (form (make-random-integer-form (1- size))))
            `(return-from ,name ,form))
        ;; No blocks -- try again
        (make-random-integer-form size))

      (if *random-int-form-catch-tags*
          (let ((tag (random-from-seq *random-int-form-catch-tags*))
                (form (make-random-integer-form (1- size))))
            `(throw ,tag ,form))
        ;; No catch tags -- try again
        (make-random-integer-form size))

      (if *random-int-form-blocks*
          (destructuring-bind (s1 s2 s3) (random-partition (1- size) 3)
            (let ((name (random-from-seq *random-int-form-blocks*))
                  (pred (make-random-pred-form s1))
                  (then (make-random-integer-form s2))
                  (else (make-random-integer-form s3)))
              `(if ,pred (return-from ,name ,then) ,else)))
        ;; No blocks -- try again
        (make-random-integer-form size))

     #-(or armedbear)
     (make-random-flet-form size)

      (let* ((nbits (1+ (min (random 20) (random 20))))
             (bvec (coerce (loop repeat nbits collect (random 2)) 'simple-bit-vector))
             (op (random-from-seq #(bit sbit))))
        `(,op ,bvec (min ,(1- nbits) (max 0 ,(make-random-integer-form (- size 3 nbits))))))

      (let* ((nvals (1+ (min (random 20) (random 20))))
             (lim (ash 1 (+ 3 (random 40))))
             (vec (coerce (loop repeat nvals collect (random lim)) 'simple-vector))
             (op (random-from-seq #(aref svref elt))))
        `(,op ,vec (min ,(1- nvals) (max 0 ,(make-random-integer-form (- size 3 nvals))))))

      (let* ((nvals (1+ (min (random 20) (random 20))))
             (lim (ash 1 (+ 3 (random 40))))
             (vals (loop repeat nvals collect (random lim)))
             (op 'elt))
        `(,op ',vals (min ,(1- nvals) (max 0 ,(make-random-integer-form (- size 3 nvals))))))

     )))

(defun make-random-aref-form (size)
  (or
   (when *vars*
     (let* ((desc (random-var-desc))
            (type (var-desc-type desc))
            (name (var-desc-name desc)))
       (cond
        ((null type) nil)
        ((subtypep type '(array integer (*)))
         `(aref ,name (min ,(1- (first (third type)))
                           (max 0 ,(make-random-integer-form (- size 2))))))
        ((subtypep type '(array integer (* *)))
         (destructuring-bind (s1 s2) (random-partition (max 2 (- size 2)) 2)
           `(aref ,name
                  (min ,(1- (first (third type)))
                       (max 0 ,(make-random-integer-form s1)))
                  (min ,(1- (second (third type)))
                       (max 0 ,(make-random-integer-form s2))))))
        (t nil))))
   (make-random-integer-form size)))

(defun make-random-count-form (size)
  (destructuring-bind (s1 s2)
      (random-partition (1- size) 2)
    (let ((arg1 (make-random-integer-form s1))
          (arg2-args (loop repeat s2 collect (make-random-integer))))
      (let ((op 'count)
            (test (random-from-seq #(eql = /= < > <= >=)))
            (arg2 (rcase
                   (1 (make-array (list s2) :initial-contents arg2-args))
                   (1
                    (let* ((mask (1- (ash 1 (1+ (random 32))))))
                      (make-array (list s2)
                                  :initial-contents
                                  (mapcar #'(lambda (x) (logand x mask)) arg2-args)
                                  :element-type `(integer 0 ,mask))))
                   (1 `(quote ,arg2-args)))))
        `(,op ,arg1 ,arg2 ,@(rcase
                                    (2 nil)
                                    (1 (list :test `(quote ,test)))
                                    (1 (list :test-not `(quote ,test)))))))))

(defun make-random-integer-flet-call-form (size)
  (if *flet-names*
      (let* ((flet-entry (random-from-seq *flet-names*))
             (flet-name (car flet-entry))
             (flet-minargs (cadr flet-entry))
             (flet-maxargs (caddr flet-entry))
             (nargs (random-from-interval (1+ flet-maxargs) flet-minargs))
             )
        (cond
         ((> nargs 0)
          (let* ((arg-sizes (random-partition (1- size) nargs))
                 (args (mapcar #'make-random-integer-form arg-sizes)))
            (rcase
             (1 `(,flet-name ,@args))
             (1 `(multiple-value-call #',flet-name (values ,@args)))
             (1 `(funcall (function ,flet-name) ,@args))
             (1 (let ((r (random (1+ (length args)))))
                  `(apply (function ,flet-name)
                          ,@(subseq args 0 r)
                          (list ,@(subseq args r))))))))
         (t (make-random-integer-form size))))
    (make-random-integer-form size)))

(defun make-random-integer-unwind-protect-form (size)
  (let* ((op 'unwind-protect)
         (nforms (random 4))
         (sizes (random-partition (1- size) (1+ nforms)))
         (arg (make-random-integer-form (first sizes)))
         (unwind-forms
          ;; We have to be careful not to generate code that will
          ;; illegally transfer control to a dead location
          (let ((*flet-names* nil)
                (*go-tags* nil)
                (*random-int-form-blocks* nil)
                (*random-int-form-catch-tags* nil))
            (mapcar #'make-random-integer-form (rest sizes)))))
    `(,op ,arg ,@unwind-forms)))

(defun make-random-integer-eval-form (size)
  (flet ((%arg (size)
               (let ((*flet-names* nil)
                     (*vars* (remove-if-not #'(lambda (s)
                                                (find (var-desc-name s)
                                                      *random-special-vars*))
                                            *vars*))
                     (*random-int-form-blocks* nil)
                     (*go-tags* nil))
                 (make-random-integer-form size))))
    (rcase
     (2 `(eval ',(%arg (1- size))))
     (2 (let* ((nargs (1+ (random 4)))
               (sizes (random-partition (1- size) nargs))
               (args (mapcar #'%arg sizes)))
          `(eval (values ,@args))))
     )))

(defun make-random-type-for-var (var e1)
  (let (desc)
    (values
     (cond
      ((and (find var *random-special-vars*)
            (setq desc (find var *vars* :key #'var-desc-name)))
       (var-desc-type desc))
      (t (rcase
          (4 '(integer * *))
          (1 (setq e1 `(make-array nil :initial-element ,e1
                                   ,@(rcase (1 nil) (1 '(:adjustable t)))))
             '(array integer nil))
          (1 (let ((size (1+ (random 10))))
               (setq e1 `(make-array '(,size):initial-element ,e1
                                     ,@(rcase (1 nil) (1 '(:adjustable t)))))
               `(array integer (,size))))
          #|
          (1 (let ((size1 (1+ (random 10)))
                   (size2 (1+ (random 10))))
               (setq e1 `(make-array '(,size1 ,size2):initial-element ,e1
                                     ,@(rcase (1 nil) (1 '(:adjustable t)))))
               `(array integer (,size1 ,size2))))
          |#
          (1 (setq e1 `(cons ,e1 ,(make-random-integer-form 1)))
             '(cons integer integer))
          (1 (setq e1 `(cons ,(make-random-integer-form 1) ,e1))
             '(cons integer integer)))))
     e1)))

(defun random2 (n)
  (min (random n) (random n)))

(defun random-from-seq2 (seq)
  (elt seq (random2 (length seq))))

(defun make-random-integer-binding-form (size)
  (destructuring-bind (s1 s2) (random-partition (1- size) 2)
    (let* ((var (random-from-seq2 (rcase
                                   (2 #(v1 v2 v3 v4 v5 v6 v7 v8 v9 v10))
                                   #-ecl (2 *random-special-vars*)
                                   )))
           (e1 (make-random-integer-form s1))
           (type (multiple-value-bind (type2 e)
                     (make-random-type-for-var var e1)
                   (setq e1 e)
                   type2))
           (e2 (let ((*vars* (cons (make-var-desc :name var :type type)
                                   *vars*)))
                 (make-random-integer-form s2)))
           (op (random-from-seq #(let let*))))
      ;; for now, avoid shadowing
      (if (member var *vars* :key #'var-desc-name)
          (make-random-integer-form size)
        (rcase
         (8 `(,op ((,var ,e1))
                  ,@(rcase (1 `((declare (dynamic-extent ,var))))
                           (3 nil))
                  ,e2))
         (2 `(multiple-value-bind (,var) ,e1 ,e2)))))))

(defun make-random-integer-progv-form (size)
  (let* ((num-vars (random 4))
         (possible-vars *random-special-vars*)
         (vars nil))
    (loop repeat num-vars
          do (loop for r = (elt possible-vars (random (length possible-vars)))
                   while (member r vars)
                   finally (push r vars)))
    (setq vars (remove-if #'(lambda (var) (let ((desc (find var *vars* :key #'var-desc-name)))
                                            (and desc (not (subtypep (var-desc-type desc) 'integer)))))
                          vars)
          num-vars (length vars))
    (if (null vars)
        `(progv nil nil ,(make-random-integer-form (1- size)))
      (destructuring-bind (s1 s2) (random-partition (1- size) 2)
        (let* ((var-sizes (random-partition s1 num-vars))
               (var-forms (mapcar #'make-random-integer-form var-sizes))
               (*vars* (append (loop for v in vars collect
                                     (make-var-desc :name v :type '(integer * *)))
                               *vars*))
               (body-form (make-random-integer-form s2)))
          `(progv ',vars (list ,@var-forms) ,body-form))))))

(defun make-random-integer-mapping-form (size)
  ;; reduce
  (let ((keyargs nil)
        (nargs (1+ (random (min 10 (max 1 size)))))
        (sequence-op (random-from-seq '(vector list))))
    (when (coin 2) (setq keyargs '(:from-end t)))
    (cond
     ((coin 2)
      (let ((start (random nargs)))
        (setq keyargs `(:start ,start ,@keyargs))
        (when (coin 2)
          (let ((end (+ start 1 (random (- nargs start)))))
            (setq keyargs `(:end ,end ,@keyargs))))))
     (t
      (when (coin 2)
        (let ((end (1+ (random nargs))))
          (setq keyargs `(:end ,end ,@keyargs))))))
    (rcase
     (1
      (let ((sizes (random-partition (1- size) nargs))
            (op (random-from-seq #(+ - * logand logxor logior max min))))
        `(reduce ,(rcase (1 `(function ,op))
                         (1 `(quote ,op)))
                 (,sequence-op
                  ,@(mapcar #'make-random-integer-form sizes))
                 ,@keyargs)))
     #-(or armedbear)
     (1      
      (destructuring-bind (size1 size2) (random-partition (1- size) 2)
        (let* ((vars '(lmv1 lmv2 lmv3 lmv4 lmv5 lmv6))
               (var1 (random-from-seq vars))
               (var2 (random-from-seq (remove var1 vars)))
               (form (let ((*vars* (list*
                                    (make-var-desc :name var1 :type '(integer * *))
                                    (make-var-desc :name var2 :type '(integer * *))
                                    *vars*)))
                       (make-random-integer-form size1)))
               (sizes (random-partition size2 nargs))
               (args (mapcar #'make-random-integer-form sizes)))
          `(reduce (function (lambda (,var1 ,var2) ,form))
                   (,sequence-op ,@args)
                   ,@keyargs)))))))

(defun make-random-integer-setq-form (size)
  (if *vars*
      (let* ((vdesc (random-from-seq *vars*))
             (var (var-desc-name vdesc))
             (type (var-desc-type vdesc))
             (op (random-from-seq #(setq setf shiftf))))
        (cond
         ((subtypep '(integer * *) type)
          (assert (not (member var '(lv1 lv2 lv3 lv4 lv5 lv6 lv7 lv8))))
          (rcase
           (1 (when (find var *random-special-vars*)
                (setq op (random-from-seq #(setf shiftf))
                      var `(symbol-value ',var))))
           (1 (setq op 'multiple-value-setq)
              (setq var (list var)))
           (5 (setf op (random-from-seq #(setq setf shiftf incf decf)))))
          `(,op ,var ,(make-random-integer-form (1- size))))
         ((and (consp type)
               (eq (car type) 'integer)
               (integerp (second type))
               (integerp (third type)))
          (assert (not (member var '(lv1 lv2 lv3 lv4 lv5 lv6 lv7 lv8))))
          (rcase
           (1 (when (find var *random-special-vars*)
                (setq op (random-from-seq #(setf shiftf))
                      var `(symbol-value ',var))))
           (1 (setq op 'multiple-value-setq)
              (setq var (list var)))
           (5 nil))
          `(,op ,var ,(random-from-interval (1+ (third type)) (second type))))
         ((and type (is-zero-rank-integer-array-type type)) ; (subtypep type '(array integer nil))
          (assert (not (member var '(lv1 lv2 lv3 lv4 lv5 lv6 lv7 lv8))))
          (when (eq op 'setq)
            (setq op (random-from-seq #(setf shiftf))))
          `(,op (aref ,var) ,(make-random-integer-form (- size 2))))
         ((and type (subtypep type '(array integer (*))))
          (when (eq op 'setq)
            (setq op (random-from-seq #(setf shiftf))))
          (destructuring-bind (s1 s2) (random-partition (max 2 (- size 2)) 2)
            `(,op (aref ,var (min ,(1- (first (third type)))
                                  (max 0
                                       ,(make-random-integer-form s1))))
                  ,(make-random-integer-form s2))))
         ((and type (subtypep type '(array integer (* *))))
          (when (eq op 'setq)
            (setq op (random-from-seq #(setf shiftf))))
          (destructuring-bind (s1 s2 s3) (random-partition (max 3 (- size 3)) 3)
            `(,op (aref ,var
                        (min ,(1- (first (third type)))
                             (max 0
                                  ,(make-random-integer-form s1)))
                        (min ,(1- (second (third type)))
                             (max 0
                                  ,(make-random-integer-form s2))))
                  ,(make-random-integer-form s3))))
         ;; Abort -- can't assign
         (t (make-random-integer-form size))))
    (make-random-integer-form size)))


(defun make-random-integer-case-form (size)
  (let ((ncases (1+ (random 10))))
    (if (< (+ size size) (+ ncases 2))
        ;; Too small, give up
        (make-random-integer-form size)
      (let* ((sizes (random-partition (1- size) (+ ncases 2)))
             (bound (ash 1 (+ 2 (random 16))))
             (lower-bound (if (coin 3) 0 (- bound)))
             (upper-bound (if (and (< lower-bound 0) (coin 3))
                              1
                            (1+ bound)))
             (cases
              (loop
               for case-size in (cddr sizes)
               for vals = (loop repeat (1+ (min (random 10) (random 10)))
                                collect (random-from-interval
                                         upper-bound lower-bound))
               for result = (make-random-integer-form case-size)
               repeat ncases
               collect `(,vals ,result)))
             (expr (make-random-integer-form (first sizes))))
        `(case ,expr
           ,@cases
           (t ,(make-random-integer-form (second sizes))))))))

(defun make-random-flet-form (size)
  "Generate random flet, labels forms, for now with no arguments
   and a single binding per form."
  (let ((fname (random-from-seq #(%f1 %f2 %f3 %f4 %f5 %f6 %f7 %f8 %f9 %f10
                                  %f11 %f12 %f13 %f14 %f15 %f16 %f17 %f18))))
    (if (assoc fname *flet-names*)
        ;; Fail if the name is in use
        (make-random-integer-form size)
      (let* ((op (random-from-seq #(flet labels)))
             (minargs (random 4))
             (maxargs #+:allegro minargs
                      #-:allegro
                      (rcase
                       (1 minargs)
                       (1 (+ minargs (random 4)))))
             (keyarg-p (coin 2))
             (keyarg-n (if keyarg-p (random 3) 0))
             (arg-names (loop for i from 1 to maxargs
                              collect (fn-arg-name fname i)))
             (key-arg-names (loop for i from 1 to keyarg-n
                                  collect (intern (format nil "KEY~A" i)
                                                  (find-package "CL-TEST"))))
             (allow-other-keys (and keyarg-p (coin 3)))
             )
        (let* ((sizes (random-partition (1- size) (+ 2 keyarg-n (- maxargs minargs))))
               (s1 (car sizes))
               (s2 (cadr sizes))
               (opt-sizes (cddr sizes)))
          (let* ((form1
                  ;; Allow return-from of the flet/labels function
                  (let ((*random-int-form-blocks*
                         (cons fname *random-int-form-blocks*))
                        (*vars* (nconc (loop for var in (append arg-names key-arg-names)
                                             collect (make-var-desc :name var
                                                                    :type '(integer * *)))
                                       *vars*)))
                    (make-random-integer-form s1)))
                 (form2 (let ((*flet-names* (cons (list fname minargs maxargs keyarg-p)
                                                  *flet-names*)))
                          (make-random-integer-form s2)))
                 (opt-forms (mapcar #'make-random-integer-form opt-sizes)
                            ))
            (if opt-forms
                `(,op ((,fname (,@(subseq arg-names 0 minargs)
                                  &optional
                                  ,@(mapcar #'list
                                            (subseq arg-names minargs)
                                            opt-forms)
                                  ,@(when keyarg-p
                                      (append '(&key)
                                              (mapcar #'list
                                                      key-arg-names
                                                      (subseq opt-forms (- maxargs minargs)))
                                              (when allow-other-keys '(&allow-other-keys))
                                              )))
                               ,form1))
                      ,form2)
              `(,op ((,fname (,@arg-names
                              ,@(when keyarg-p
                                  (append '(&key)
                                          (mapcar #'list
                                                  key-arg-names
                                                  opt-forms )
                                          (when allow-other-keys '(&allow-other-keys))
                                          )))
                             ,form1))
                    ,form2))))))))

(defun make-random-tagbody (size)
  (let* ((num-forms (random 6))
         (tags nil))
    (loop for i below num-forms
          do (loop for tag = (rcase
                              #-allegro (1 (random 8))
                              (1 (random-from-seq #(tag1 tag2 tag3 tag4
                                                         tag5 tag6 tag7 tag8))))
                   while (member tag tags)
                   finally (push tag tags)))
    (assert (= (length (remove-duplicates tags)) (length tags)))
    (let* ((*go-tags* (set-difference *go-tags* tags))
           (sizes (if (> num-forms 0) (random-partition (1- size) num-forms) nil))
           (forms
            (loop for tag-list on tags
                  for i below num-forms
                  for size in sizes
                  collect (let ((*go-tags* (append tag-list *go-tags*)))
                            (make-random-integer-form size)))))
      `(tagbody ,@(loop for tag in tags
                        for form in forms
                        when (atom form) do (setq form `(progn ,form))
                        append `(,form ,tag))))))

(defun make-random-tagbody-and-progn (size)
  (let* ((final-size (random (max 1 (floor size 5))))
         (tagbody-size (- size final-size)))
    (let ((final-form (make-random-integer-form final-size))
          (tagbody-form (make-random-tagbody tagbody-size)))
      `(progn ,tagbody-form ,final-form))))

(defun make-random-pred-form (size)
  "Make a random form whose value is to be used as a generalized boolean."
  (if (<= size 1)
      (rcase
        (1 (if (coin) t nil))
        (2
         `(,(random-from-seq '(< <= = > >= /= eql equal))
           ,(make-random-integer-form size)
           ,(make-random-integer-form size))))
    (rcase
      (1 (if (coin) t nil))
      (3 `(not ,(make-random-pred-form (1- size))))
      (12 (destructuring-bind (leftsize rightsize)
             (random-partition (1- size) 2)
           `(,(random-from-seq '(and or))
             ,(make-random-pred-form leftsize)
             ,(make-random-pred-form rightsize))))
      (1 (let* ((nsizes (+ 1 (random 3)))
                (sizes (random-partition (1- size) nsizes)))
           `(,(random-from-seq (if (= nsizes 2) #(< <= > >= = /= eql equal)
                                 #(< <= > >= = /=)))
             ,@(mapcar #'make-random-integer-form sizes))))
      (3 (let* ((cond-size (random (max 1 (floor size 2))))
                (then-size (random (- size cond-size)))
                (else-size (- size 1 cond-size then-size))
                (pred (make-random-pred-form cond-size))
                (then-part (make-random-pred-form then-size))
                (else-part (make-random-pred-form else-size)))
           `(if ,pred ,then-part ,else-part)))
      #-poplog
      (1 (destructuring-bind (s1 s2)
             (random-partition (1- size) 2)
           `(ldb-test ,(make-random-byte-spec-form s1)
                      ,(make-random-integer-form s2))))
      (2 (let ((form (make-random-integer-form (1- size)))
               (op (random-from-seq #(evenp oddp minusp plusp zerop))))
           `(,op ,form)))
      (2 (destructuring-bind (s1 s2)
             (random-partition (1- size) 2)
           (let ((arg1 (make-random-integer-form s1))
                 (arg2-args (loop repeat s2 collect (make-random-integer))))
             (let ((op (random-from-seq #(find position)))
                   (test (random-from-seq #(eql = /= < > <= >=)))
                   (arg2 (rcase
                          (1 (make-array (list s2) :initial-contents arg2-args))
                          (1
                           (let* ((mask (1- (ash 1 (1+ (random 32))))))
                             (make-array (list s2)
                                         :initial-contents
                                         (mapcar #'(lambda (x) (logand x mask)) arg2-args)
                                        :element-type `(integer 0 ,mask))))
                          (1 `(quote ,arg2-args)))))
               `(,op ,arg1 ,arg2 ,@(rcase
                                    (2 nil)
                                    (1 (list :test `(quote ,test)))
                                    (1 (list :test-not `(quote ,test)))))))))
      
      (1 (let ((index (random (1+ (random *maximum-random-int-bits*))))
               (form (make-random-integer-form (1- size))))
           `(logbitp ,index ,form)))
      (1 ;; typep form
       (let ((subform (make-random-integer-form (- size 2)))
             (type
              (rcase
               (1 `(real ,@(make-random-integer-range)))
               (1 `(rational ,@(make-random-integer-range)))
               (1 `(rational ,(+ 1/2 (make-random-integer))))
               (1 `(rational * ,(+ 1/2 (make-random-integer))))
               (1 `(integer ,@(make-random-integer-range)))
               (1 `(integer ,(make-random-integer)))
               (1 `(integer * ,(make-random-integer)))
               (1 'fixnum)
               (1 'bignum)
               (1 `(integer)))))
         `(typep ,subform ',type)))
      )))

(defun make-random-loop-form (size)
  (if (<= size 2)
      (make-random-integer-form size)
    (let* ((var (random-from-seq #(lv1 lv2 lv3 lv4)))
           (count (random 4))
           (*vars* (cons (make-var-desc :name var :type nil)
                         *vars*)))
      (rcase
       (1 `(loop for ,var below ,count count ,(make-random-pred-form (- size 2))))
       (1 `(loop for ,var below ,count sum ,(make-random-integer-form (- size 2))))
       ))))

(defun make-random-byte-spec-form (size)
  (declare (ignore size))
  (let* ((pform (random 33))
         (sform (1+ (random 33))))
    `(byte ,sform ,pform)))

(defgeneric make-random-element-of-type (type)
  (:documentation "Create a random element of a lisp type."))

(defgeneric make-random-element-of-compound-type (type-op type-args)
  (:documentation "Create a random element of type `(,TYPE-OP ,@TYPE-ARGS)")
  (:method ((type-op (eql 'or)) type-args)
	   (assert type-args)
	   (make-random-element-of-type (random-from-seq type-args)))
  (:method ((type-op (eql 'and)) type-args)
	   (assert type-args)
	   (loop for x = (make-random-element-of-type (car type-args))
	     repeat 100
	     when (typep x (cons 'and (cdr type-args)))
	     return x
	     finally (error "Cannot generate random element of ~A"
			    (cons type-op type-args))))
  (:method ((type-op (eql 'not)) type-args)
	   (assert (eql (length type-args) 1))
	   (make-random-element-of-type `(and t (not ,(car type-args)))))
  (:method ((type-op (eql 'integer)) type-args)
	   (let ((lo (let ((lo (car type-args)))
		       (cond
			((consp lo) (1+ (car lo)))
			((eq lo nil) '*)
			(t lo))))
		 (hi (let ((hi (cadr type-args)))
		       (cond
			((consp hi) (1- (car hi)))
			((eq hi nil) '*)
			(t hi)))))
	     (if (eq lo '*)
		 (if (eq hi '*)
		     (let ((x (ash 1 (random *maximum-random-int-bits*))))
		       (random-from-interval x (- x)))
		   (random-from-interval (1+ hi)
					 (- hi (random (ash 1 *maximum-random-int-bits*)))))
	   
	       (if (eq hi '*)
		   (random-from-interval (+ lo (random (ash 1 *maximum-random-int-bits*)) 1)
					 lo)
		 ;; May generalize the next case to increase odds
		 ;; of certain integers (near 0, near endpoints, near
		 ;; powers of 2...)
		 (random-from-interval (1+ hi) lo)))))
  (:method ((type-op (eql 'rational)) type-args)
	   (let ((type (cons type-op type-args)))
	     (or
	      (let ((r (make-random-element-of-type 'rational)))
		(and (typep r type) r))
	      (let ((lo (car type-args))
		    (hi (cadr type-args))
		    lo= hi=)
		(cond
		 ((consp lo) nil)
		 ((member lo '(* nil))
		  (setq lo nil)
		  (setq lo= nil))
		 (t
		  (assert (typep lo 'rational))
		  (setq lo= t)))
		(cond
		 ((consp hi) nil)
		 ((member hi '(* nil))
		  (setq hi nil)
		  (setq hi= nil))
		 (t
		  (assert (typep hi 'rational))
		  (setq hi= t)))
		(assert (or (null lo) (null hi) (<= lo hi)))
		(assert (or (null lo) (null hi) (< lo hi) (and lo= hi=)))
		(cond
		 ((null lo)
		  (cond
		   ((null hi) (make-random-rational))
		   (hi= (- hi (make-random-nonnegative-rational)))
		   (t (- hi (make-random-positive-rational)))))
		 ((null hi)
		  (cond
		   (lo= (+ lo (make-random-nonnegative-rational)))
		   (t (+ lo (make-random-positive-rational)))))
		 (t
		  (+ lo (make-random-bounded-rational (- hi lo) lo= hi=))))))))
  
  (:method ((type-op (eql 'ratio)) type-args)
	   (let ((r 0))
	     (loop
	      do (setq r (make-random-element-of-compound-type 'rational type-args))
	      while (integerp r))
	     r))
  
  (:method ((type-op (eql 'real)) type-args)
	   (rcase
	    (1 (let ((lo (and (numberp (car type-args))
			      (rational (car type-args))))
		     (hi (and (numberp (cadr type-args))
			      (rational (cadr type-args)))))
		 (make-random-element-of-compound-type 'rational
						       `(,(or lo '*)
							 ,(or hi '*)))))
	    (1 (make-random-element-of-compound-type 'float
						     `(,(or (car type-args) '*)
						       ,(or (cadr type-args) '*))))))
  
  (:method ((type-op (eql 'float)) type-args)
	   (let* ((new-type-op (random-from-seq #(single-float double-float long-float short-float)))
		  (lo (car type-args))
		  (hi (cadr type-args))
		  (most-neg (most-negative-float new-type-op))
		  (most-pos (most-positive-float new-type-op)))
	     (cond
	      ((or (and (realp lo) (< lo most-neg))
		   (and (realp hi) (> hi most-pos)))
	       ;; try again
	       (make-random-element-of-compound-type type-op type-args))
	      (t
	       (when (and (realp lo) (not (typep lo new-type-op)))
		 (cond
		  ((< lo most-neg) (setq lo '*))
		  (t (setq lo (coerce lo new-type-op)))))
	       (when (and (realp hi) (not (typep hi new-type-op)))
		 (cond
		  ((> hi most-pos) (setq hi '*))
		  (t (setq hi (coerce hi new-type-op)))))
	       (make-random-element-of-compound-type new-type-op `(,(or lo '*) ,(or hi '*)))))))
  
  (:method ((type-op (eql 'short-float)) type-args)
	   (assert (<= (length type-args) 2))
	   (apply #'make-random-element-of-float-type type-op type-args))
  (:method ((type-op (eql 'single-float)) type-args)
	   (assert (<= (length type-args) 2))
	   (apply #'make-random-element-of-float-type type-op type-args))
  (:method ((type-op (eql 'double-float)) type-args)
	   (assert (<= (length type-args) 2))
	   (apply #'make-random-element-of-float-type type-op type-args))
  (:method ((type-op (eql 'long-float)) type-args)
	   (assert (<= (length type-args) 2))
	   (apply #'make-random-element-of-float-type type-op type-args))
  
  (:method ((type-op (eql 'mod)) type-args)
	   (let ((modulus (second type-args)))
	     (assert (integerp modulus))
	     (assert (plusp modulus))
	     (make-random-element-of-compound-type 'integer `(0 (,modulus)))))
  
  (:method ((type-op (eql 'unsigned-byte)) type-args)
	   (assert (<= (length type-args) 1))
	   (if (null type-args)
	       (make-random-element-of-type '(integer 0 *))
	     (let ((bits (first type-args)))
	       (if (eq bits '*)
		   (make-random-element-of-type '(integer 0 *))
		 (progn
		   (assert (and (integerp bits) (>= bits 1)))
		   (make-random-element-of-type
		    `(integer 0 ,(1- (ash 1 bits)))))))))
  
  (:method ((type-op (eql 'signed-byte)) type-args)
	   (assert (<= (length type-args) 1))
	   (if (null type-args)
	       (make-random-element-of-type 'integer)
	     (let ((bits (car type-args)))
	       (if (eq bits'*)
		   (make-random-element-of-type 'integer)
		 (progn
		   (assert (and (integerp bits) (>= bits 1)))
		   (make-random-element-of-type
		    `(integer ,(- (ash 1 (1- bits))) ,(1- (ash 1 (1- bits))))))))))

  (:method ((type-op (eql 'eql)) type-args)
	   (assert (= (length type-args) 1))
	   (car type-args))

  (:method ((type-op (eql 'member)) type-args)
	   (assert type-args)
	   (random-from-seq type-args))
  
  (:method ((type-op (eql 'vector)) type-args)
	   (assert (<= (length type-args) 2))
	   (let ((etype-spec (if type-args (car type-args) '*))
		 (size-spec (if (cdr type-args) (cadr type-args) '*)))
	     (make-random-vector etype-spec size-spec)))
  
  (:method ((type-op (eql 'aimple-vector)) type-args)
	   (assert (<= (length type-args) 1))
	   (let ((size-spec (if type-args (car type-args) '*)))
	     (make-random-vector t size-spec :simple t)))
  
  (:method ((type-op (eql 'array)) type-args)
	   (assert (<= (length type-args) 2))
	   (let ((etype-spec (if type-args (car type-args) '*))
		 (size-spec (if (cdr type-args) (cadr type-args) '*)))
	     (make-random-array etype-spec size-spec)))
  
  (:method ((type-op (eql 'simple-array)) type-args)
	   (assert (<= (length type-args) 2))
	   (let ((etype-spec (if type-args (car type-args) '*))
		 (size-spec (if (cdr type-args) (cadr type-args) '*)))
	     (make-random-array etype-spec size-spec :simple t)))

  (:method ((type-op (eql 'string)) type-args)
	   (assert (<= (length type-args) 1))
	   (let ((size-spec (if type-args (car type-args) '*)))
	     (make-random-string size-spec)))

  (:method ((type-op (eql 'simple-string)) type-args)
	   (assert (<= (length type-args) 1))
	   (let ((size-spec (if type-args (car type-args) '*)))
	     (make-random-string size-spec :simple t)))
  
  (:method ((type-op (eql 'base-string)) type-args)
	   (assert (<= (length type-args) 1))
	   (let ((size-spec (if type-args (car type-args) '*)))
	     (make-random-vector 'base-char size-spec)))
  
  (:method ((type-op (eql 'simple-base-string)) type-args)
	   (assert (<= (length type-args) 1))
	   (let ((size-spec (if type-args (car type-args) '*)))
	     (make-random-vector 'base-char size-spec :simple t)))
  
  (:method ((type-op (eql 'bit-vector)) type-args)
	   (assert (<= (length type-args) 1))
	   (let ((size-spec (if type-args (car type-args) '*)))
	     (make-random-vector 'bit size-spec)))
  
  (:method ((type-op (eql 'simple-bit-vector)) type-args)
	   (assert (<= (length type-args) 1))
	   (let ((size-spec (if type-args (car type-args) '*)))
	     (make-random-vector 'bit size-spec :simple t)))
  
  (:method ((type-op (eql 'cons)) type-args)
	   (assert (<= (length type-args) 2))
	   (cons (make-random-element-of-type (if type-args (car type-args) t))
		 (make-random-element-of-type (if (cdr type-args) (cadr type-args) t))))
  
  (:method ((type-op (eql 'complex)) type-args)
	   (cond
	    ((null type-args)
	     (make-random-element-of-type 'complex))
	    (t
	     (assert (null (cdr type-args)))
	     (let ((etype (car type-args)))
	       (loop for v1 = (make-random-element-of-type etype)
		     for v2 = (make-random-element-of-type etype)
		     for c = (complex v1 v2)
		     when (typep c (cons 'complex type-args))
		     return c)))))
  )

(defmethod make-random-element-of-type ((type cons))
  (make-random-element-of-compound-type (car type) (cdr type)))

(defun make-random-element-of-float-type (type-op &optional lo hi)
  (let (lo= hi=)
    (cond
     ((consp lo) nil)
     ((member lo '(* nil))
      (setq lo (most-negative-float type-op))
      (setq lo= t))
     (t
      (assert (typep lo type-op))
      (setq lo= t)))
    (cond
     ((consp hi) nil)
     ((member hi '(* nil))
      (setq hi (most-positive-float type-op))
      (setq hi= t))
     (t
      (assert (typep hi type-op))
      (setq hi= t)))
    (assert (<= lo hi))
    (assert (or (< lo hi) (and lo= hi=)))
    (let ((limit 100000))
      (cond
       ((or (<= hi 0)
	    (>= lo 0)
	    (and (<= (- limit) hi limit) (<= (- limit) lo limit)))
	(loop for x = (+ (random (- hi lo)) lo)
	      do (when (or lo= (/= x lo)) (return x))))
       (t
	(rcase
	 (1 (random (min hi (float limit hi))))
	 (1 (- (random (min (float limit lo) (- lo)))))))))))
 
#|
(defmethod make-random-element-of-type ((type cons))
  (let ((type-op (first type)))
    (ecase type-op
      (or
       (assert (cdr type))
       (make-random-element-of-type (random-from-seq (cdr type))))
      (and
       (assert (cdr type))
       (loop for x = (make-random-element-of-type (cadr type))
	     repeat 100
	     when (typep x (cons 'and (cddr type)))
	     return x
	     finally (error "Cannot generate random element of ~A" type)))
      (not
       (assert (cdr type))
       (assert (not (cddr type)))
       (make-random-element-of-type `(and t ,type)))
      (integer
       (let ((lo (let ((lo (cadr type)))
		   (cond
		    ((consp lo) (1+ (car lo)))
		    ((eq lo nil) '*)
		    (t lo))))
	     (hi (let ((hi (caddr type)))
		   (cond
		    ((consp hi) (1- (car hi)))
		    ((eq hi nil) '*)
		    (t hi)))))
         (if (eq lo '*)
             (if (eq hi '*)
                 (let ((x (ash 1 (random *maximum-random-int-bits*))))
                   (random-from-interval x (- x)))
               (random-from-interval (1+ hi)
                                     (- hi (random (ash 1 *maximum-random-int-bits*)))))
	   
           (if (eq hi '*)
               (random-from-interval (+ lo (random (ash 1 *maximum-random-int-bits*)) 1)
                                     lo)
             ;; May generalize the next case to increase odds
             ;; of certain integers (near 0, near endpoints, near
             ;; powers of 2...)
             (random-from-interval (1+ hi) lo)))))
      
      (rational
       (or
	(let ((r (make-random-element-of-type 'rational)))
	  (and (typep r type) r))
	(let ((lo (cadr type))
	      (hi (caddr type))
	      lo= hi=)
	  (cond
	   ((consp lo) nil)
	   ((member lo '(* nil))
	    (setq lo nil)
	    (setq lo= nil))
	   (t
	    (assert (typep lo 'rational))
	    (setq lo= t)))
	  (cond
	   ((consp hi) nil)
	   ((member hi '(* nil))
	    (setq hi nil)
	    (setq hi= nil))
	   (t
	    (assert (typep hi 'rational))
	    (setq hi= t)))
	  (assert (or (null lo) (null hi) (<= lo hi)))
	  (assert (or (null lo) (null hi) (< lo hi) (and lo= hi=)))
	  (cond
	   ((null lo)
	    (cond
	     ((null hi) (make-random-rational))
	     (hi= (- hi (make-random-nonnegative-rational)))
	     (t (- hi (make-random-positive-rational)))))
	   ((null hi)
	    (cond
	     (lo= (+ lo (make-random-nonnegative-rational)))
	     (t (+ lo (make-random-positive-rational)))))
	   (t
	    (+ lo (make-random-bounded-rational (- hi lo) lo= hi=)))))))
      
      (ratio
       (let ((r 0))
	 (loop
	  do (setq r (make-random-element-of-type `(rational ,@(cdr type))))
	  while (integerp r))
	 r))
      
      (real
       (rcase
	(1 (let ((lo (and (numberp (cadr type))
			  (rational (cadr type))))
		 (hi (and (numberp (caddr type))
			  (rational (caddr type)))))
	     (make-random-element-of-type `(rational ,(or lo '*)
						     ,(or hi '*)))))
	(1 (make-random-element-of-type `(float ,(or (cadr type) '*)
						,(or (caddr type) '*))))))
      
      ((float)
       (let* ((new-type-op (random-from-seq #(single-float double-float
							   long-float short-float)))
	      (lo (cadr type))
	      (hi (caddr type))
	      (most-neg (most-negative-float new-type-op))
	      (most-pos (most-positive-float new-type-op)))
	 (cond
	  ((or (and (realp lo) (< lo most-neg))
	       (and (realp hi) (> hi most-pos)))
	   ;; try again
	   (make-random-element-of-type type))
	  (t
	   (when (and (realp lo) (not (typep lo new-type-op)))
	     (cond
	      ((< lo most-neg) (setq lo '*))
	      (t (setq lo (coerce lo new-type-op)))))
	   (when (and (realp hi) (not (typep hi new-type-op)))
	     (cond
	      ((> hi most-pos) (setq hi '*))
	      (t (setq hi (coerce hi new-type-op)))))
	   (make-random-element-of-type
	    `(,new-type-op ,(or lo '*) ,(or hi '*)))))))
      
      ((single-float double-float long-float short-float)
       (let ((lo (cadr type))
	     (hi (caddr type))
	     lo= hi=)
	 (cond
	  ((consp lo) nil)
	  ((member lo '(* nil))
	   (setq lo (most-negative-float type-op))
	   (setq lo= t))
	  (t
	   (assert (typep lo type-op))
	   (setq lo= t)))
	 (cond
	  ((consp hi) nil)
	  ((member hi '(* nil))
	   (setq hi (most-positive-float type-op))
	   (setq hi= t))
	  (t
	   (assert (typep hi type-op))
	   (setq hi= t)))
	 (assert (<= lo hi))
	 (assert (or (< lo hi) (and lo= hi=)))
	 (let ((limit 100000))
	   (cond
	    ((or (<= hi 0)
		 (>= lo 0)
		 (and (<= (- limit) hi limit) (<= (- limit) lo limit)))
	     (loop for x = (+ (random (- hi lo)) lo)
		   do (when (or lo= (/= x lo)) (return x))))
	    (t
	     (rcase
	      (1 (random (min hi (float limit hi))))
	      (1 (- (random (min (float limit lo) (- lo)))))))))))
      
      (mod
       (let ((modulus (second type)))
	 (assert (and (integerp modulus)
		      (plusp modulus)))
	 (make-random-element-of-type `(integer 0 (,modulus)))))
      (unsigned-byte
       (if (null (cdr type))
           (make-random-element-of-type '(integer 0 *))
	 (let ((bits (second type)))
	   (if (eq bits'*)
	       (make-random-element-of-type '(integer 0 *))
	     (progn
	       (assert (and (integerp bits) (>= bits 1)))
	       (make-random-element-of-type
		`(integer 0 ,(1- (ash 1 bits)))))))))
      (signed-byte
       (if (null (cdr type))
           (make-random-element-of-type 'integer)
	 (let ((bits (second type)))
	   (if (eq bits'*)
	       (make-random-element-of-type 'integer)
	     (progn
	       (assert (and (integerp bits) (>= bits 1)))
	       (make-random-element-of-type
		`(integer ,(- (ash 1 (1- bits))) ,(1- (ash 1 (1- bits))))))))))
      (eql
       (assert (= (length type) 2))
       (cadr type))
      (member
       (assert (cdr type))
       (random-from-seq (cdr type)))
      ((vector)
       (let ((etype-spec (if (cdr type) (cadr type) '*))
	     (size-spec (if (cddr type) (caddr type) '*)))
	 (make-random-vector etype-spec size-spec)))
      ((simple-vector)
       (let ((size-spec (if (cdr type) (cadr type) '*)))
	 (make-random-vector t size-spec :simple t)))
      ((array simple-array)
       (let ((etype-spec (if (cdr type) (cadr type) '*))
	     (size-spec (if (cddr type) (caddr type) '*)))
	 (make-random-array etype-spec size-spec :simple (eql (car type) 'simple-array))))
      ((string simple-string)
       (let ((size-spec (if (cdr type) (cadr type) '*)))
	 (make-random-string size-spec :simple (eql (car type) 'simple-string))))
      ((base-string simple-base-string)
       (let ((size-spec (if (cdr type) (cadr type) '*)))
	 (make-random-vector 'base-char size-spec :simple (eql (car type) 'simple-base-string))))
      ((bit-vector simple-bit-vector)
       (let ((size-spec (if (cdr type) (cadr type) '*)))
	 (make-random-vector 'bit size-spec :simple (eql (car type) 'simple-bit-vector))))
      ((cons)
       (cons (make-random-element-of-type (if (cdr type) (cadr type) t))
	     (make-random-element-of-type (if (cddr type) (caddr type) t))))
      ((complex)
       (cond
	((null (cdr type))
	 (make-random-element-of-type 'complex))
	(t
	 (assert (null (cddr type)))
	 (let ((etype (cadr type)))
	   (loop for v1 = (make-random-element-of-type etype)
		 for v2 = (make-random-element-of-type etype)
		 for c = (complex v1 v2)
		 when (typep c type)
		 return c)))))
      )))
|#

(defmethod make-random-element-of-type ((type class))
  (make-random-element-of-type (class-name type)))

(defmethod make-random-element-of-type ((type (eql 'bit))) (random 2))
(defmethod make-random-element-of-type ((type (eql 'boolean)))
  (random-from-seq #(nil t)))
(defmethod make-random-element-of-type ((type (eql 'symbol)))
  (random-from-seq #(nil t a b c :a :b :c |z| foo |foo| car)))
(defmethod make-random-element-of-type ((type (eql 'keyword)))
  (random-from-seq #(:a :b :c :d :e :f :g :h :i :j)))
(defmethod make-random-element-of-type ((type (eql 'unsigned-byte)))
  (random-from-interval (1+ (ash 1 (random *maximum-random-int-bits*))) 0))
(defmethod make-random-element-of-type ((type (eql 'signed-byte)))
  (random-from-interval
   (1+ (ash 1 (random *maximum-random-int-bits*)))
   (- (ash 1 (random *maximum-random-int-bits*)))))
(defmethod make-random-element-of-type ((type (eql 'rational)))
  (make-random-rational))
(defmethod make-random-element-of-type ((type (eql 'ratio)))
  (let ((r 0))
    (loop do (setq r (make-random-element-of-type 'rational))
	  while (integerp r))
    r))
(defmethod make-random-element-of-type ((type (eql 'integer)))
  (let ((x (ash 1 (random *maximum-random-int-bits*))))
    (random-from-interval (1+ x) (- x))))
(defmethod make-random-element-of-type ((type (eql 'float)))
  (make-random-element-of-type
   (random-from-seq #(short-float single-float double-float long-float))))
(defmethod make-random-element-of-type ((type (eql 'real)))
  (make-random-element-of-type (random-from-seq #(integer rational float))))
(defmethod make-random-element-of-type ((type (eql 'number)))
  (make-random-element-of-type (random-from-seq #(integer rational float #-ecl complex))))
(defmethod make-random-element-of-type ((type (eql 'bit-vector)))
  (make-random-vector 'bit '*))
(defmethod make-random-element-of-type ((type (eql 'simple-bit-vector)))
  (make-random-vector 'bit '* :simple t))
(defmethod make-random-element-of-type ((type (eql 'vector)))
  (make-random-vector '* '*))
(defmethod make-random-element-of-type ((type (eql 'simple-vector)))
  (make-random-vector 't '* :simple t))
(defmethod make-random-element-of-type ((type (eql 'array)))
  (make-random-array '* '*))
(defmethod make-random-element-of-type ((type (eql 'simple-array)))
  (make-random-array '* '* :simple t))
(defmethod make-random-element-of-type ((type (eql 'string)))
  (make-random-string '*))
(defmethod make-random-element-of-type ((type (eql 'simple-string)))
  (make-random-string '* :simple t))
(defmethod make-random-element-of-type ((type (eql 'base-string)))
  (make-random-vector 'base-char '*))
(defmethod make-random-element-of-type ((type (eql 'simple-base-string)))
  (make-random-vector 'base-char '* :simple t))
(defmethod make-random-element-of-type ((type (eql 'character)))
  (make-random-character))
(defmethod make-random-element-of-type ((type (eql 'extended-char)))
  (loop for x = (make-random-character)
	when (typep x 'extended-char) return x))
(defmethod make-random-element-of-type ((type (eql 'null))) nil)
(defmethod make-random-element-of-type ((type (eql 'fixnum)))
  (random-from-interval (1+ most-positive-fixnum) most-negative-fixnum))
(defmethod make-random-element-of-type ((type (eql 'complex)))
  (make-random-element-of-type '(complex real)))
(defmethod make-random-element-of-type ((type (eql 'cons)))
  (make-random-element-of-type '(cons t t)))
(defmethod make-random-element-of-type ((type (eql 'list)))
  ;; Should modify this to allow non-proper lists?
  (let ((len (min (random 10) (random 10))))
    (loop repeat len collect (make-random-element-of-type t))))
(defmethod make-random-element-of-type ((type (eql 'sequence)))
  (make-random-element-of-type '(or list vector)))
(defmethod make-random-element-of-type ((type (eql 'function)))
  (rcase
   (5 (symbol-function (random-from-seq *cl-function-symbols*)))
   (5 (symbol-function (random-from-seq *cl-accessor-symbols*)))
   (1 #'(lambda (x) (cons x x)))
   (1 (eval '#'(lambda (x) (cons x x))))))

(defmethod make-random-element-of-type ((type symbol))
  (case type
   ((single-float short-float double-float long-float)
    (make-random-element-of-type (list type)))
   ((base-char standard-char)
    (random-from-seq +standard-chars+))
   ;; Default
   ((atom t *) (make-random-element-of-type
		(random-from-seq #(real symbol boolean integer unsigned-byte
					#-ecl complex character
					(string 1) (bit-vector 1)))))
   (t (call-next-method type))
   ))

(defun make-random-character ()
  (loop
   when (rcase
         (3 (random-from-seq +standard-chars+))
         (3 (code-char (random (min 256 char-code-limit))))
         (1 (code-char (random (min (ash 1 16) char-code-limit))))
         (1 (code-char (random (min (ash 1 24) char-code-limit))))
         (1 (code-char (random char-code-limit))))
   return it))

(defun make-random-array-element-type ()
  ;; Create random types for array elements
  (let ((bits 40))
    (rcase
     (2 t)
     (1 'symbol)
     (1 `(unsigned-byte ,(1+ (random bits))))
     (1 `(signed-byte ,(1+ (random bits))))
     (1 'character)
     (1 'base-char)
     (1 'bit)
     (1 (random-from-seq #(short-float single-float double-float long-float))))))

(defun make-random-vector (etype-spec size-spec &key simple)
  (let* ((etype (if (eql etype-spec '*)
                    (make-random-array-element-type)
                  etype-spec))
         (size (if (eql size-spec '*)
                   (random (ash 1 (+ 2 (random 8))))
                 size-spec))
         (displaced? (and (not simple) (coin 4)))
         (displaced-size (+ size (random (max 6 size))))
         (displacement (random (1+ (- displaced-size size))))
         (adjustable (and (not simple) (coin 3)))
         (fill-pointer (and (not simple)
                            (rcase (3 nil) (1 t) (1 (random (1+ size)))))))
    (assert (<= size 1000000))
    (if displaced?
        (let ((displaced-vector (make-array displaced-size :element-type etype
                                            :initial-contents (loop repeat displaced-size
                                                                    collect (make-random-element-of-type etype)))))
          (make-array size :element-type etype :adjustable adjustable
                      :fill-pointer fill-pointer
                      :displaced-to displaced-vector
                      :displaced-index-offset displacement))
      (make-array size
                  :element-type etype
                  :initial-contents (loop repeat size
                                          collect (make-random-element-of-type etype))
                  :adjustable adjustable
                  :fill-pointer fill-pointer
                  ))))

(defun make-random-array (etype-spec dim-specs &key simple)
  (when (eql dim-specs '*)
    (setq dim-specs (random 10)))
  (when (numberp dim-specs)
    (setq dim-specs (make-list dim-specs :initial-element '*)))
  (let* ((etype (if (eql etype-spec '*) t etype-spec))
         (rank (length dim-specs))
         (dims (loop for dim in dim-specs
                     collect (if (eql dim '*)
                                 (1+ (random (ash 1 (floor 9 rank))))
                               dim))))
    (assert (<= (reduce '* dims :initial-value 1) 1000000))
    (assert (<= (reduce 'max dims :initial-value 1) 1000000))
    (make-array dims
                :element-type etype
                :initial-contents
                (labels ((%init (dims)
                                (if (null dims)
                                    (make-random-element-of-type etype)
                                  (loop repeat (car dims)
                                        collect (%init (cdr dims))))))
                  (%init dims))
                :adjustable (and (not simple) (coin))
                ;; Do displacements later
                )))

(defun most-negative-float (float-type-symbol)
  (ecase float-type-symbol
    (short-float most-negative-short-float)
    (single-float most-negative-single-float)
    (double-float most-negative-double-float)
    (long-float most-negative-long-float)
    (float (min most-negative-short-float most-negative-single-float
                most-negative-double-float most-negative-long-float))))

(defun most-positive-float (float-type-symbol)
  (ecase float-type-symbol
    (short-float most-positive-short-float)
    (single-float most-positive-single-float)
    (double-float most-positive-double-float)
    (long-float most-positive-long-float)
    (float (max most-positive-short-float most-positive-single-float
                most-positive-double-float most-positive-long-float))))

(defun make-optimized-lambda-form (form vars var-types opt-decls)
  `(lambda ,vars
     ,@(mapcar #'(lambda (tp var) `(declare (type ,tp ,var)))
               var-types vars)
     (declare (ignorable ,@vars))
     #+cmu (declare (optimize (extensions:inhibit-warnings 3)))
     (declare (optimize ,@opt-decls))
     ,form))

(defun make-unoptimized-lambda-form (form vars var-types opt-decls)
  (declare (ignore var-types))
  `(lambda ,vars
     (declare (notinline ,@(fn-symbols-in-form form)))
     #+cmu (declare (optimize (extensions:inhibit-warnings 3)))
     (declare (optimize ,@opt-decls))
     ,form))

(defvar *compile-using-defun*
  #-(or allegro lispworks) nil
  #+(or allegro lispworks) t)

(defvar *compile-using-defgeneric* nil
  "If true and *COMPILE-USING-DEFUN* is false, then build a defgeneric form
   for the function and compile that.")

(defvar *name-to-use-in-optimized-defun* 'dummy-fn-name1)
(defvar *name-to-use-in-unoptimized-defun* 'dummy-fn-name2)

(defun test-int-form (form vars var-types vals-list opt-decls-1 opt-decls-2)
  ;; Try to compile FORM with associated VARS, and if it compiles
  ;; check for equality of the two compiled forms.
  ;; Return a non-nil list of details if a problem is found,
  ;; NIL otherwise.
  (let ((optimized-fn-src (make-optimized-lambda-form form vars var-types opt-decls-1))
        (unoptimized-fn-src (make-unoptimized-lambda-form form vars var-types opt-decls-2)))
    (setq *int-form-vals* nil
          *optimized-fn-src* optimized-fn-src
          *unoptimized-fn-src* unoptimized-fn-src)
    (flet ((%compile
            (lambda-form opt-defun-name)
            (cl:handler-bind
             (#+sbcl (sb-ext::compiler-note #'muffle-warning)
                     (warning #'muffle-warning)
                     ((or error serious-condition)
                      #'(lambda (c)
                                (format t "Compilation failure~%~A~%"
                                        (format nil "~S" form))
                                (finish-output *standard-output*)
                                (return-from test-int-form
                                  (list (list :vars vars
                                              :form form
                                              :var-types var-types
                                              :vals (first vals-list)
                                              :lambda-form lambda-form
                                              :decls1 opt-decls-1
                                              :decls2 opt-decls-2
                                              :compiler-condition
                                              (with-output-to-string
                                                (s)
                                                (prin1 c s))))))))
             (let ((start-time (get-universal-time))
                   (clf (cdr lambda-form)))
               (prog1
                   (cond
                    (*compile-using-defun*
                     (fmakunbound opt-defun-name)
                     (eval `(defun ,opt-defun-name ,@clf))
                     (compile opt-defun-name)
                     (symbol-function opt-defun-name))
                    (*compile-using-defgeneric*
                     (fmakunbound opt-defun-name)
                     (eval `(defgeneric ,opt-defun-name ,(car clf)))
                     (eval `(defmethod ,opt-defun-name,(mapcar #'(lambda (name) `(,name integer)) (car clf))
                              ,@(cdr clf)))
                     (compile opt-defun-name)
                     (symbol-function opt-defun-name))
                    (t (compile nil lambda-form)))
                 (let* ((stop-time (get-universal-time))
                        (total-time (- stop-time start-time)))
                   (when (> total-time *max-compile-time*)
                     (setf *max-compile-time* total-time)
                     (setf *max-compile-term* lambda-form)))
                 ;; #+:ecl (si:gc t)
                 )))))
      (let ((optimized-compiled-fn (%compile optimized-fn-src
                                             *name-to-use-in-optimized-defun*))
            (unoptimized-compiled-fn
             (if *compile-unoptimized-form*
                 (%compile unoptimized-fn-src *name-to-use-in-unoptimized-defun*)
               (eval `(function ,unoptimized-fn-src)))))
        (declare (type function optimized-compiled-fn unoptimized-compiled-fn))
        (dolist (vals vals-list)
          (setq *int-form-vals* vals)
          (flet ((%eval-error
                  (kind)
                  (let ((*print-circle* t))
                    (format t "~A~%" (format nil "~S" form)))
                  (finish-output *standard-output*)
                  (return
                   (list (list :vars vars
                               :vals vals
                               :form form
                               :var-types var-types
                               :decls1 opt-decls-1
                               :decls2 opt-decls-2
                               :optimized-lambda-form optimized-fn-src
                               :unoptimized-lambda-form unoptimized-fn-src
                               :kind kind)))))
              
            (let ((unopt-result
                   (cl-handler-case
                    (cl-handler-bind
                     (#+sbcl (sb-ext::compiler-note #'muffle-warning)
                             (warning #'muffle-warning))
                     (identity ;; multiple-value-list
                      (apply unoptimized-compiled-fn vals)))
                    ((or error serious-condition)
                     (c)
                     (%eval-error (list :unoptimized-form-error
                                        (with-output-to-string
                                          (s) (prin1 c s)))))))
                  (opt-result
                   (cl-handler-case
                    (cl-handler-bind
                     (#+sbcl (sb-ext::compiler-note #'muffle-warning)
                             (warning #'muffle-warning))
                     (identity ;; multiple-value-list
                      (apply optimized-compiled-fn vals)))
                    ((or error serious-condition)
                     (c)
                     (%eval-error (list :optimized-form-error
                                        (with-output-to-string
                                          (s) (prin1 c s))))))))
              (if (equal opt-result unopt-result)
                  nil
                (progn
                  (format t "Different results: ~A, ~A~%"
                          opt-result unopt-result)
                  (setq *opt-result* opt-result
                        *unopt-result* unopt-result)
                  (%eval-error (list :different-results
                                     opt-result
                                     unopt-result)))))))))))

;;; Interface to the form pruner

(declaim (special *prune-table*))

(defun prune-int-form (input-form vars var-types vals-list opt-decls-1 opt-decls-2)
  "Conduct tests on selected simplified versions of INPUT-FORM.  Return the
   minimal form that still causes some kind of failure."
  (loop do
        (let ((form input-form))
          (flet ((%try-fn (new-form)
                          (when (test-int-form new-form vars var-types vals-list
                                               opt-decls-1 opt-decls-2)
                            (setf form new-form)
                            (throw 'success nil))))
            (let ((*prune-table* (make-hash-table :test #'eq)))
              (loop
               (catch 'success
                 (prune form #'%try-fn)
                 (return form)))))
          (when (equal form input-form) (return form))
          (setq input-form form))))

(defun prune-results (result-list)
  "Given a list of test results, prune their forms down to a minimal set."
  (loop for result in result-list
        collect
        (let* ((form (getf result :form))
               (vars (getf result :vars))
               (var-types (getf result :var-types))
               (vals-list (list (getf result :vals)))
               (opt-decl-1 (getf result :decls1))
               (opt-decl-2 (getf result :decls2))
               (pruned-form (prune-int-form form vars var-types vals-list opt-decl-1 opt-decl-2))
               (optimized-lambda-form (make-optimized-lambda-form
                                       pruned-form vars var-types opt-decl-1))
               (unoptimized-lambda-form (make-unoptimized-lambda-form
                                         pruned-form vars var-types opt-decl-2)))
            `(:vars ,vars
              :var-types ,var-types
              :vals ,(first vals-list)
              :form ,pruned-form
              :decls1 ,opt-decl-1
              :decls2 ,opt-decl-2
              :optimized-lambda-form ,optimized-lambda-form
              :unoptimized-lambda-form ,unoptimized-lambda-form))))

;;;
;;; The call (PRUNE form try-fn) attempts to simplify the lisp form
;;; so that it still satisfies TRY-FN.  The function TRY-FN should
;;; return if the substitution is a failure.  Otherwise, it should
;;; transfer control elsewhere via GO, THROW, etc.
;;;
;;; The return value of PRUNE should be ignored.
;;;
(defun prune (form try-fn)
  (declare (type function try-fn))
  (when (gethash form *prune-table*)
    (return-from prune nil))
  (flet ((try (x) (funcall try-fn x)))
    (cond
     ((keywordp form) nil)
     ((integerp form)
      (unless (zerop form) (try 0)))
     ((consp form)
      (let* ((op (car form))
             (args (cdr form))
             (nargs (length args)))
        (case op

         ((quote) nil)

         ((go)
          (try 0))
         
         ((signum integer-length logcount
                  logandc1 logandc2 lognand lognor logorc1 logorc2
                  realpart imagpart)
          (try 0)
          (mapc try-fn args)
          (prune-fn form try-fn))

         ((make-array)
          (when (and (eq (car args) nil)
                     (eq (cadr args) ':initial-element)
                     ; (null (cdddr args))
                     )
            (prune (caddr args) #'(lambda (form) (try `(make-array nil :initial-element ,form . ,(cdddr args)))))
            (when (cdddr args)
              (try `(make-array nil :initial-element ,(caddr args))))
            ))

         ((cons)
          (prune-fn form try-fn))

         ((dotimes)
          (try 0)
          (let* ((binding-form (first args))
                 (body (rest args))
                 (var (first binding-form))
                 (count-form (second binding-form))
                 (result (third binding-form)))
            (try result)
            (unless (eql count-form 0)
              (try `(dotimes (,var 0 ,result) ,@body)))
            (prune result #'(lambda (form)
                              (try `(dotimes (,var ,count-form ,form) ,@body))))
            (when (= (length body) 1)
              (prune (first body)
                     #'(lambda (form)
                         (when (consp form)
                           (try `(dotimes (,var ,count-form ,result) ,form))))))))
         
         ((abs 1+ 1-)
          (try 0)
          (mapc try-fn args)
          (prune-fn form try-fn))

         ((identity  ignore-errors cl:handler-case restart-case locally)
          (unless (and (consp args)
                       (consp (car args))
                       (eql (caar args) 'tagbody))
            (mapc try-fn args))
          (prune-fn form try-fn))

         ((boole)
          (try (second args))
          (try (third args))
          (prune (second args)
                 #'(lambda (form) (try `(boole ,(first args) ,form ,(third args)))))
          (prune (third args)
                 #'(lambda (form) (try `(boole ,(first args) ,(second args) ,form)))))

         ((unwind-protect prog1 multiple-value-prog1)
          (try (first args))
          (let ((val (first args))
                (rest (rest args)))
            (when rest
              (try `(unwind-protect ,val))
              (when (cdr rest)
                (loop for i from 0 below (length rest)
                      do
                      (try `(unwind-protect ,val
                              ,@(subseq rest 0 i)
                              ,@(subseq rest (1+ i))))))))
          (prune-fn form try-fn))

         ((prog2)
          (assert (>= (length args) 2))
          (let ((val1 (first args))
                (arg2 (second args))
                (rest (cddr args)))
            (try arg2)
            (prune-fn form try-fn)
            (when rest
              (try `(prog2 ,val1 ,arg2))
              (when (cdr rest)
                (loop for i from 0 below (length rest)
                      do
                      (try `(prog2 ,val1 ,arg2
                              ,@(subseq rest 0 i)
                              ,@(subseq rest (1+ i)))))))))

         ((typep)
          (try (car args))
          (prune (car args)
                 #'(lambda (form) `(,op ,form ,@(cdr args)))))

         ((load-time-value)
          (let ((arg (first args)))
            (try arg)
            (cond
             ((cdr args)
              (try `(load-time-value ,arg))
              (prune arg
                     #'(lambda (form)
                         (try `(load-time-value ,form ,(second args))))))
             (t
              (prune arg
                     #'(lambda (form)
                         (try `(load-time-value ,form))))))))

         ((eval)
          (try 0)
          (let ((arg (first args)))
            (cond
             ((consp arg)
              (cond
               ((eql (car arg) 'quote)
                (prune (cadr arg) #'(lambda (form) (try `(eval ',form)))))
               (t
                (try arg)
                (prune arg #'(lambda (form) `(eval ,form))))))
             (t (try arg)))))

         ((the macrolet cl:handler-bind restart-bind)
          (assert (= (length args) 2))
          (try (second args))
          (prune (second args) try-fn))
         
         ((not eq eql equal)
          (when (every #'constantp args)
            (try (eval form)))
          (try t)
          (try nil)
          (mapc try-fn args)
          (prune-fn form try-fn)
          )

         ((and or = < > <= >= /=)
          (when (every #'constantp args)
            (try (eval form)))
          (try t)
          (try nil)
          (mapc try-fn args)
          (prune-nary-fn form try-fn)
          (prune-fn form try-fn))
         
         ((- + * min max logand logior logxor logeqv gcd lcm values)
          (when (every #'constantp args)
            (try (eval form)))
          (try 0)
          (mapc try-fn args)
          (prune-nary-fn form try-fn)
          (prune-fn form try-fn))

         ((/)
          (when (every #'constantp args)
            (try (eval form)))
          (try 0)
          (try (car args))
          (when (cddr args)
            (prune (car args) #'(lambda (form) (try `(/ ,form ,(second args)))))))

         ((expt rationalize rational numberator denominator)
          (try 0)
          (mapc try-fn args)
          (prune-fn form try-fn))
         
         ((coerce)
          (try 0)
          (try (car args))
          (prune (car args) #'(lambda (form) (try `(coerce ,form ,(cadr args))))))
          

         ((multiple-value-call)
          ;; Simplify usual case
          (when (= nargs 2)
            (destructuring-bind (arg1 arg2) args
              (when (and (consp arg1) (consp arg2)
                         (eql (first arg1) 'function)
                         (eql (first arg2) 'values))
                (mapc try-fn (rest arg2))
                (let ((fn (second arg1)))
                  (when (symbolp fn)
                    (try `(,fn ,@(rest arg2)))))
                ;; Prune the VALUES form
                (prune-list (rest arg2)
                            #'prune
                            #'(lambda (args)
                                (try `(multiple-value-call ,arg1 (values ,@args)))))
                )))
          (mapc try-fn (rest args)))

         ((bit sbit elt aref svref)
          (try 0)
          (when (= (length args) 2)
            (let ((arg1 (car args))
                  (arg2 (cadr args)))
              (when (and (consp arg2)
                         (eql (car arg2) 'min)
                         (integerp (cadr arg2)))
                (let ((arg2.2 (caddr arg2)))
                  (try arg2.2)
                  (when (and (consp arg2.2)
                             (eql (car arg2.2) 'max)
                             (integerp (cadr arg2.2)))
                    (prune (caddr arg2.2)
                           #'(lambda (form)
                               (try `(,op ,arg1 (min ,(cadr arg2)
                                                     (max ,(cadr arg2.2) ,form))))))))))))

         ((car cdr)
          (try 0)
          (try 1))

         ((if)
          (let (;; (pred (first args))
                (then (second args))
                (else (third args)))
            (try then)
            (try else)
            (when (every #'constantp args)
              (try (eval form)))
            (prune-fn form try-fn)))

         ((incf decf)
          (try 0)
          (assert (member (length form) '(2 3)))
          (try (first args))
          (when (> (length args) 1)
            (try (second args))
            (try `(,op ,(first args)))
            (unless (integerp (second args))
              (prune (second args)
                     #'(lambda (form)
                         (try `(,op ,(first args) ,form)))))))

         ((setq setf shiftf)
          (try 0)
          ;; Assumes only one assignment
          (assert (= (length form) 3))
          (try (first args))
          (try (second args))
          (unless (integerp (second args))
            (prune (second args)
                   #'(lambda (form)
                       (try `(,op ,(first args) ,form))))))

         ((rotatef)
          (try 0)
          (mapc try-fn (cdr form)))

         ((multiple-value-setq)
          (try 0)
          ;; Assumes only one assignment, and one variable
          (assert (= (length form) 3))
          (assert (= (length (first args)) 1))
          (try `(setq ,(caar args) ,(cadr args)))
          (unless (integerp (second args))
            (prune (second args)
                   #'(lambda (form)
                       (try `(,op ,(first args) ,form))))))

         ((byte)
          (prune-fn form try-fn))

         ((deposit-field dpb)
          (try 0)
          (destructuring-bind (a1 a2 a3)
              args
            (try a1)
            (try a3)
            (when (and (integerp a1)
                       (integerp a3)
                       (and (consp a2)
                            (eq (first a2) 'byte)
                            (integerp (second a2))
                            (integerp (third a2))))
              (try (eval form))))
          (prune-fn form try-fn))

         ((ldb mask-field)
          (try 0)
          (try (second args))
          (when (and (consp (first args))
                     (eq 'byte (first (first args)))
                     (every #'numberp (cdr (first args)))
                     (numberp (second args)))
            (try (eval form)))
          (prune-fn form try-fn))

         ((ldb-test)
          (try t)
          (try nil)
          (prune-fn form try-fn))

         ((let let*)
          (prune-let form try-fn))

         ((multiple-value-bind)
          (assert (= (length args) 3))
          (let ((arg1 (first args))
                (arg2 (second args))
                (body (caddr args)))
            (when (= (length arg1) 1)
              (try `(let ((,(first arg1) ,arg2)) ,body)))
            (prune arg2 #'(lambda (form)
                            (try `(multiple-value-bind ,arg1 ,form ,body))))
            (prune body #'(lambda (form)
                            (try `(multiple-value-bind ,arg1 ,arg2 ,form))))))

         ((block)
          (let ((name (second form))
                (body (cddr form)))
            (when (and body (null (cdr body)))
              (let ((form1 (first body)))

                ;; Try removing the block entirely if it is not in use
                (when (not (find-in-tree name body))
                  (try form1))
                
                ;; Try removing the block if its only use is an immediately
                ;; enclosed return-from: (block <n> (return-from <n> <e>))
                (when (and (consp form1)
                           (eq (first form1) 'return-from)
                           (eq (second form1) name)
                           (not (find-in-tree name (third form1))))
                  (try (third form1)))
                
                ;; Otherwise, try to simplify the subexpression
                (prune form1
                       #'(lambda (x)
                           (try `(block ,name ,x))))))))

         ((catch)
          (let* ((tag (second form))
                 (name (if (consp tag) (cadr tag) tag))
                 (body (cddr form)))
            (when (and body (null (cdr body)))
              (let ((form1 (first body)))

                ;; Try removing the catch entirely if it is not in use
                ;; We make assumptions here about what throws can
                ;; be present.
                (when (or (not (find-in-tree 'throw body))
                          (not (find-in-tree name body)))
                  (try form1))
                
                ;; Try removing the block if its only use is an immediately
                ;; enclosed return-from: (block <n> (return-from <n> <e>))
                (when (and (consp form1)
                           (eq (first form1) 'throw)
                           (equal (second form1) name)
                           (not (find-in-tree name (third form1))))
                  (try (third form1)))
                
                ;; Otherwise, try to simplify the subexpression
                (prune form1
                       #'(lambda (x)
                           (try `(catch ,tag ,x))))))))

         ((throw)
          (try (second args))
          (prune (second args)
                 #'(lambda (x) (try `(throw ,(first args) ,x)))))

         ((flet labels)
          (try 0)
          (prune-flet form try-fn))

         ((case)
          (prune-case form try-fn))

         ((isqrt)
          (let ((arg (second form)))
            (assert (null (cddr form)))
            (assert (consp arg))
            (assert (eq (first arg) 'abs))
            (let ((arg2 (second arg)))
              (try arg2)
              ;; Try to fold
              (when (integerp arg2)
                (try (isqrt (abs arg2))))
              ;; Otherwise, simplify arg2
              (prune arg2 #'(lambda (form)
                              (try `(isqrt (abs ,form))))))))

         ((ash)
          (try 0)
          (let ((form1 (second form))
                (form2 (third form)))
            (try form1)
            (try form2)
            (prune form1
                   #'(lambda (form)
                       (try `(ash ,form ,form2))))
            (when (and (consp form2)
                       (= (length form2) 3))
              (when (and (integerp form1)
                         (eq (first form2) 'min)
                         (every #'integerp (cdr form2)))
                (try (eval form)))
              (let ((form3 (third form2)))
                (prune form3
                       #'(lambda (form)
                           (try
                            `(ash ,form1 (,(first form2) ,(second form2)
                                          ,form)))))))))

         ((floor ceiling truncate round mod rem)
          (try 0)
          (let ((form1 (second form))
                (form2 (third form)))
            (try form1)
            (when (cddr form) (try form2))
            (prune form1
                   (if (cddr form)
                       #'(lambda (form)
                           (try `(,op ,form ,form2)))
                     #'(lambda (form) (try `(,op ,form)))))
            (when (and (consp form2)
                       (= (length form2) 3))
              (when (and (integerp form1)
                         (member (first form2) '(max min))
                         (every #'integerp (cdr form2)))
                (try (eval form)))
              (let ((form3 (third form2)))
                (prune form3
                       #'(lambda (form)
                           (try
                            `(,op ,form1 (,(first form2) ,(second form2)
                                          ,form)))))))))

         ((constantly)
          (unless (eql (car args) 0)
            (prune (car args)
                   #'(lambda (arg) (try `(constantly ,arg))))))

         ((funcall)
          (try 0)
          (let ((fn (second form))
                (fn-args (cddr form)))
            (mapc try-fn fn-args)
            (unless (equal fn '(constantly 0))
              (try `(funcall (constantly 0) ,@fn-args)))
            (when (and (consp fn)
                       (eql (car fn) 'function)
                       (symbolp (cadr fn)))
              (try `(,(cadr fn) ,@fn-args)))
            (prune-list fn-args
                        #'prune
                        #'(lambda (args)
                            (try `(funcall ,fn ,@args))))))

         ((reduce)
          (try 0)
          (let ((arg1 (car args))
                (arg2 (cadr args))
                (rest (cddr args)))
            (when (and ;; (null (cddr args))
                       (consp arg1)
                       (eql (car arg1) 'function))
              (let ((arg1.2 (cadr arg1)))
                (when (and (consp arg1.2)
                           (eql (car arg1.2) 'lambda))
                  (let ((largs (cadr arg1.2))
                        (body (cddr arg1.2)))
                    (when (null (cdr body))
                      (prune (car body)
                             #'(lambda (bform)
                                 (try `(reduce (function (lambda ,largs ,bform))
                                               ,arg2 ,@rest)))))))))
            (when (consp arg2)
              (case (car arg2)
                ((list vector)
                 (let ((arg2.rest (cdr arg2)))
                   (mapc try-fn arg2.rest)
                   (prune-list arg2.rest
                               #'prune
                               #'(lambda (args)
                                   (try `(reduce ,arg1
                                                 (,(car arg2) ,@args)
                                                 ,@rest))))))))))

         ((apply)
          (try 0)
          (let ((fn (second form))
                (fn-args (butlast (cddr form)))
                (list-arg (car (last form))))
            (mapc try-fn fn-args)
            (unless (equal fn '(constantly 0))
              (try `(apply (constantly 0) ,@(cddr form))))
            (when (and (consp list-arg)
                       (eq (car list-arg) 'list))
              (mapc try-fn (cdr list-arg)))
            (prune-list fn-args
                        #'prune
                        #'(lambda (args)
                            (try `(apply ,fn ,@args ,list-arg))))
            (when (and (consp list-arg)
                       (eq (car list-arg) 'list))
              (try `(apply ,fn ,@fn-args ,@(cdr list-arg) nil))
              (prune-list (cdr list-arg)
                        #'prune
                        #'(lambda (args)
                            (try `(apply ,fn ,@fn-args
                                         (list ,@args))))))))

         ((progv)
          (try 0)
          (prune-progv form try-fn))

         ((tagbody)
          (try 0)
          (prune-tagbody form try-fn))

         ((progn)
          (when (null args) (try nil))
          (try (car (last args)))
          (loop for i from 0 below (1- (length args))
                for a in args
                do (try `(progn ,@(subseq args 0 i)
                                ,@(subseq args (1+ i))))
                do (when (and (consp a)
                              (or
                               (eq (car a) 'progn)
                               (and (eq (car a) 'tagbody)
                                    (every #'consp (cdr a)))))
                     (try `(progn ,@(subseq args 0 i)
                                  ,@(copy-list (cdr a))
                                  ,@(subseq args (1+ i))))))
          (prune-fn form try-fn))

         ((loop)
          (try 0)
          (when (and (eql (length args) 6)
                     (eql (elt args 0) 'for)
                     (eql (elt args 2) 'below))
            (let ((var (elt args 1))
                  (count (elt args 3))
                  (form (elt args 5)))
              (unless (eql count 0) (try count))
              (case (elt args 4)
                (sum
                 (try `(let ((,(elt args 1) 0)) ,(elt args 5)))
                 (prune form #'(lambda (form)
                                 (try `(loop for ,var below ,count sum ,form)))))
                (count
                 (unless (or (eql form t) (eql form nil))
                   (try `(loop for ,var below ,count count t))
                   (try `(loop for ,var below ,count count nil))
                   (prune form
                          #'(lambda (form)
                              (try `(loop for ,var below ,count count ,form))))))
                ))))

         (otherwise
          (try 0)
          (prune-fn form try-fn))
         
         )))))
  (setf (gethash form *prune-table*) t)
  nil)

(defun find-in-tree (value tree)
  "Return true if VALUE is eql to a node in TREE."
  (or (eql value tree)
      (and (consp tree)
           (or (find-in-tree value (car tree))
               (find-in-tree value (cdr tree))))))

(defun prune-list (list element-prune-fn list-try-fn)
  (declare (type function element-prune-fn list-try-fn))
  "Utility function for pruning in a list."
    (loop for i from 0
          for e in list
          do (funcall element-prune-fn
                      e
                      #'(lambda (form)
                          (funcall list-try-fn
                                   (append (subseq list 0 i)
                                           (list form)
                                           (subseq list (1+ i))))))))

(defun prune-case (form try-fn)
  (declare (type function try-fn))
  (flet ((try (e) (funcall try-fn e)))
    (let* ((op (first form))
           (expr (second form))
           (cases (cddr form)))
      
      ;; Try just the top expression
      (try expr)
      
      ;; Try simplifying the expr
      (prune expr
             #'(lambda (form)
                 (try `(,op ,form ,@cases))))
      
      ;; Try individual cases
      (loop for case in cases
            do (try (first (last (rest case)))))
      
      ;; Try deleting individual cases
      (loop for i from 0 below (1- (length cases))
            do (try `(,op ,expr
                          ,@(subseq cases 0 i)
                          ,@(subseq cases (1+ i)))))
      
      ;; Try simplifying the cases
      ;; Assume each case has a single form
      (prune-list cases
                  #'(lambda (case try-fn)
                      (declare (type function try-fn))
                      (when (and (listp (car case))
                                 (> (length (car case)) 1))
                        ;; try removing constants
                        (loop for i below (length (car case))
                              do (funcall try-fn
                                          `((,@(subseq (car case) 0 i)
                                             ,@(subseq (car case) (1+ i)))
                                            ,@(cdr case)))))
                      (when (eql (length case) 2)
                        (prune (cadr case)
                               #'(lambda (form)
                                   (funcall try-fn
                                            (list (car case) form))))))
                  #'(lambda (cases)
                      (try `(,op ,expr ,@cases)))))))

(defun prune-tagbody (form try-fn)
  (declare (type function try-fn))
  (let (;; (op (car form))
        (body (cdr form)))
    (loop for i from 0
          for e in body
          do
          (cond
           ((atom e)
            ;; A tag
            (unless (find-in-tree e (subseq body 0 i))
              (funcall try-fn `(tagbody ,@(subseq body 0 i)
                                        ,@(subseq body (1+ i))))))
           (t
            (funcall try-fn
                     `(tagbody ,@(subseq body 0 i)
                               ,@(subseq body (1+ i))))
            (prune e
                   #'(lambda (form)
                       ;; Don't put an atom here.
                       (when (consp form)
                         (funcall
                          try-fn
                          `(tagbody ,@(subseq body 0 i)
                                    ,form
                                    ,@(subseq body (1+ i))))))))))))

(defun prune-progv (form try-fn)
  (declare (type function try-fn))
  (let (;; (op (car form))
        (vars-form (cadr form))
        (vals-form (caddr form))
        (body-list (cdddr form)))
    (when (and (null vars-form) (null vals-form))
      (funcall try-fn `(let () ,@body-list)))
    (when (and (consp vals-form) (eql (car vals-form) 'list))
      (when (and (consp vars-form) (eql (car vars-form) 'quote))
        (let ((vars (cadr vars-form))
              (vals (cdr vals-form)))
          (when (eql (length vars) (length vals))
            (let ((let-form `(let () ,@body-list)))
              (mapc #'(lambda (var val)
                        (setq let-form `(let ((,var ,val)) ,let-form)))
                    vars vals)
              (funcall try-fn let-form)))
          ;; Try simplifying the vals forms
          (prune-list vals
                      #'prune
                      #'(lambda (vals)
                          (funcall try-fn
                                   `(progv ,vars-form (list ,@vals) ,@body-list)))))))
    ;; Try simplifying the body
    (when (eql (length body-list) 1)
      (prune (car body-list)
             #'(lambda (form)
                 (funcall try-fn
                          `(progv ,vars-form ,vals-form ,form)))))))

(defun prune-nary-fn (form try-fn)
  ;; Attempt to reduce the number of arguments to the fn
  ;; Do not reduce below 1
  (declare (type function try-fn))
  (let* ((op (car form))
         (args (cdr form))
         (nargs (length args)))
    (when (> nargs 1)
      (loop for i from 1 to nargs
            do (funcall try-fn `(,op ,@(subseq args 0 (1- i))
                                     ,@(subseq args i)))))))

(defun prune-fn (form try-fn)
  "Attempt to simplify a function call form.  It is considered
   acceptable to replace the call by one of its argument forms."
  (declare (type function try-fn))
  (prune-list (cdr form)
              #'prune
              #'(lambda (args)
                  (funcall try-fn (cons (car form) args)))))

(defun prune-let (form try-fn)
  "Attempt to simplify a LET form."
  (declare (type function try-fn))
  (let* ((op (car form))
         (binding-list (cadr form))
         (body (cddr form))
         (body-len (length body))
         (len (length binding-list))
         )

    (when (> body-len 1)
      (funcall try-fn `(,op ,binding-list ,@(cdr body))))

    ;; Try to simplify (let ((<name> <form>)) ...) to <form>
    #|
    (when (and (>= len 1)
               ;; (eql body-len 1)
               ;; (eql (caar binding-list) (car body))
               )
      (let ((val-form (cadar binding-list)))
        (unless (and (consp val-form)
                     (eql (car val-form) 'make-array))
          (funcall try-fn val-form))))
    |#

    (when (>= len 1)
      (let ((val-form (cadar binding-list)))
        (when (consp val-form)
          (case (car val-form)
            ((make-array)
             (let ((init (getf (cddr val-form) :initial-element)))
               (when init
                 (funcall try-fn init))))
            ((cons)
             (funcall try-fn (cadr val-form))
             (funcall try-fn (caddr val-form)))))))

    ;; Try to simplify the forms in the RHS of the bindings
    (prune-list binding-list
                #'(lambda (binding try-fn)
                    (declare (type function try-fn))
                    (prune (cadr binding)
                           #'(lambda (form)
                               (funcall try-fn
                                        (list (car binding)
                                              form)))))
                #'(lambda (bindings)
                    (funcall try-fn `(,op ,bindings ,@body))))

    ;; Prune off unused variable
    (when (and binding-list
               (not (rest binding-list))
               (let ((name (caar binding-list)))
                 (and (symbolp name)
                      (not (find-if-subtree #'(lambda (x) (eq x name)) body)))))
      (funcall try-fn `(progn ,@body)))

    ;; Try to simplify the body of the LET form
    (when body
      (unless binding-list
        (funcall try-fn (car (last body))))
      (when (and (first binding-list)
                 (not (rest binding-list))
                 (not (rest body)))
        (let ((binding (first binding-list)))
          (unless (or (consp (second binding))
                      (has-binding-to-var (first binding) body)
                      (has-assignment-to-var (first binding) body)
                      )
            (funcall try-fn `(let ()
                               ,@(subst (second binding)
                                        (first binding)
                                        (remove-if #'(lambda (x) (and (consp x) (eq (car x) 'declare)))
                                                   body)
                                        ))))))
      (prune (car (last body))
             #'(lambda (form2)
                 (funcall try-fn
                          `(,@(butlast form) ,form2)))))))

(defun has-assignment-to-var (var form)
  (find-if-subtree
   #'(lambda (form)
       (and (consp form)
            (or
             (and (member (car form) '(setq setf shiftf incf decf) :test #'eq)
                  (eq (cadr form) var))
             (and (eql (car form) 'multiple-value-setq)
                  (member var (cadr form))))))
   form))

(defun has-binding-to-var (var form)
  (find-if-subtree
   #'(lambda (form)
       (and (consp form)
            (case (car form)
              ((let let*)
               (loop for binding in (cadr form)
                     thereis (eq (car binding) var)))
              ((progv)
               (and (consp (cadr form))
                    (eq (caadr form) 'quote)
                    (consp (second (cadr form)))
                    (member var (second (cadr form)))))
              (t nil))))
   form))

(defun find-if-subtree (pred tree)
  (declare (type function pred))
  (cond
   ((funcall pred tree) tree)
   ((consp tree)
    (or (find-if-subtree pred (car tree))
        (find-if-subtree pred (cdr tree))))
   (t nil)))

(defun prune-flet (form try-fn)
  "Attempt to simplify a FLET form."
  (declare (type function try-fn))

  (let* ((op (car form))
         (binding-list (cadr form))
         (body (cddr form)))

    ;; Remove a declaration, if any
    (when (and (consp body)
               (consp (car body))
               (eq (caar body) 'declare))
      (funcall try-fn `(,op ,binding-list ,@(cdr body))))

    ;; Try to prune optional arguments
    (prune-list binding-list
                #'(lambda (binding try-fn)
                    (declare (type function try-fn))
                    (let* ((name (car binding))
                           (args (cadr binding))
                           (body (cddr binding))
                           (opt-pos (position-if #'(lambda (e) (member e '(&key &optional)))
                                                 (the list args))))
                      (when opt-pos
                        (incf opt-pos)
                        (let ((normal-args (subseq args 0 (1- opt-pos)))
                              (optionals (subseq args opt-pos)))
                          (prune-list optionals
                                      #'(lambda (opt-lambda-arg try-fn)
                                          (declare (type function try-fn))
                                          (when (consp opt-lambda-arg)
                                            (let ((name (first opt-lambda-arg))
                                                  (form (second opt-lambda-arg)))
                                              (prune form
                                                     #'(lambda (form)
                                                         (funcall try-fn (list name form)))))))
                                      #'(lambda (opt-args)
                                          (funcall try-fn
                                                   `(,name (,@normal-args
                                                              &optional
                                                              ,@opt-args)
                                                           ,@body))))))))
                #'(lambda (bindings)
                    (funcall try-fn `(,op ,bindings ,@body))))
                       
    
    ;; Try to simplify the forms in the RHS of the bindings
    (prune-list binding-list
                #'(lambda (binding try-fn)
                    (declare (type function try-fn))
                      
                    ;; Prune body of a binding
                    (prune (third binding)
                           #'(lambda (form)
                               (funcall try-fn
                                        (list (first binding)
                                              (second binding)
                                              form)))))
                #'(lambda (bindings)
                    (funcall try-fn `(,op ,bindings ,@body))))

    ;; ;; Try to simplify the body of the FLET form
    (when body

      ;; No bindings -- try to simplify to the last form in the body
      (unless binding-list
        (funcall try-fn (first (last body))))

      (when (and (consp binding-list)
                 (null (rest binding-list)))
        (let ((binding (first binding-list)))
          ;; One binding -- match on (flet ((<name> () <body>)) (<name>))
          (when (and (symbolp (first binding))
                     (not (find-in-tree (first binding) (rest binding)))
                     (null (second binding))
                     (equal body (list (list (first binding)))))
            (funcall try-fn `(,op () ,@(cddr binding))))
          ;; One binding -- try to remove it if not used
          (when (and (symbolp (first binding))
                     (not (find-in-tree (first binding) body)))
            (funcall try-fn (first (last body))))
        ))


      ;; Try to simplify (the last form in) the body.
      (prune (first (last body))
             #'(lambda (form2)
                 (funcall try-fn
                          `(,@(butlast form) ,form2)))))))

;;; Routine to walk form, applying a function at each form
;;; The fn is applied in preorder.  When it returns :stop, do
;;; not descend into subforms

#|
(defun walk (form fn)
  (declare (type function fn))
  (unless (eq (funcall fn form) :stop)
    (when (consp form)
      (let ((op (car form)))
        (case op
          ((let let*)
           (walk-let form fn))
          ((cond)
           (dolist (clause (cdr form))
             (walk-implicit-progn clause fn)))
          ((multiple-value-bind)
              (walk (third form) fn)
              (walk-body (cdddr form) fn))
          ((function quote declare) nil)
          ((block the return-from)
           (walk-implicit-progn (cddr form) fn))
          ((case typecase)
           (walk (cadr form) fn)
           (dolist (clause (cddr form))
             (walk-implicit-progn (cdr clause) fn)))
          ((flet labels)
           
          
              
          
|#  

;;;;;;;;;;;;;;;;;;;;;;
;;; Convert pruned results to test cases

(defun produce-test-cases (instances &key
                                     (stream *standard-output*)
                                     (prefix "MISC.")
                                     (index 1))
  (dolist (inst instances)
    (let* (;; (vars (getf inst :vars))
           (vals (getf inst :vals))
           (optimized-lambda-form (getf inst :optimized-lambda-form))
           (unoptimized-lambda-form (getf inst :unoptimized-lambda-form))
           (name (intern
                  (concatenate 'string prefix (format nil "~D" index))
                  "CL-TEST"))
           (test-form
            `(deftest ,name
               (let* ((fn1 ',optimized-lambda-form)
                      (fn2 ',unoptimized-lambda-form)
                      (vals ',vals)
                      (v1 (apply (compile nil fn1) vals))
                      (v2 (apply (compile nil fn2) vals)))
                 (if (eql v1 v2)
                     :good
                   (list v1 v2)))
               :good)))
      (print test-form stream)
      (terpri stream)
      (incf index)))
  (values))
