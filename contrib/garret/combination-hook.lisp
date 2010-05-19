
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Combination hook -- intercept the CCL compiler's treatment of combinations
;;;  to allow extensions.  The two most notable applications are to allow CL to
;;;  support Scheme's ((...) ...) syntax (which lets you do cool pedagogical things
;;;  like compute factorials without defining any functions -- see below) and
;;;  supporting Lexicons handling of calling undefined functions.  See lexicons.lisp
;;;  for details.
;;;
;;;  Written by Ron Garret, released to the public domain 22 February 2010.
;;;

(defpackage :combination-hook
  (:export :combination-hook :undefined-function-hook))

(in-package :combination-hook)

(defun combination-hook (form) form)
(defun undefined-function-hook (form) form)

(defmacro hook-em-dano (callback)                  ; With apologies to Dragnet
  `(destructuring-bind (form env) ccl::arglist
     (if (atom form)
       (:do-it)
       (let* ((op (car form))
              (hook (cond ((and (symbolp op)
                                (not (fboundp op))
                                (not (ccl::function-information op env)))
                           'undefined-function-hook)
                           ((and (consp op)
                                 (not (ccl::lambda-expression-p op)))
                            'combination-hook))))
         (if (null hook)
           (:do-it)
           (let ((new-form (funcall hook form)))
             (if (equalp form new-form)
               (:do-it)
               (,callback new-form env))))))))

(advise ccl::nx1-combination
  (hook-em-dano ccl::nx1-combination)
  :when :around
  :name combination-hook)

(advise ccl::cheap-eval-in-environment
  (hook-em-dano ccl::cheap-eval-in-environment)
  :when :around
  :name combination-hook)


#|
; Test: factorial using the Y combinator

; ((...) ...) ==> (funcall (...) ...)
(defun combination-hook (form) (cons 'funcall form))

(defun convert-args (args)
  (cond ( (null args) nil )
        ( (atom args) (list '&rest args) )
        (t (cons (car args) (convert-args (cdr args))))))

(defmacro λ (args &body body)
  (let ((args (remove-if (lambda (x) (eql #\& (elt (symbol-name x) 0))) args)))
    `(lambda ,(convert-args args)
       (flet ,(mapcar (lambda (arg) `(,arg (&rest args) (apply ,arg args))) args)
         ,@body))))

(defmacro define (name value)
  `(progn
     (define-symbol-macro ,name ,value)
     (defun ,name (&rest args) (apply ,name args))))

(define y (λ (f) ((λ (h) (λ (x) ((f (h h)) x))) (λ (h) (λ (x) ((f (h h)) x))))))

(define y (λ (f) ((λ (g) (g g)) (λ (h) (λ (x) ((f (h h)) x))))))

(define fact* (λ (f) (λ (n) (if (zerop n) 1 (* n (f (1- n)))))))

((y fact*) 15)

((y (λ (f) (λ (n) (if (zerop n) 1 (* n (f (1- n))))))) 15)

(((λ (f) ((λ (g) (g g)) (λ (h) (λ (x) ((f (h h)) x)))))
  (λ (f) (λ (n) (if (zerop n) 1 (* n (f (1- n)))))))
 15)
|#
