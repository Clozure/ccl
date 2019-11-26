;;;-*-Mode: LISP; Package: CCL -*-
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

;; Level-2.lisp

(in-package "CCL")

(eval-when (eval compile)
  (require "LEVEL-2")
  (require "BACKQUOTE")
  (require "DEFSTRUCT-MACROS"))



(eval-when (eval compile)
  (require "LISPEQU"))








; This incredibly essential thing is part of ANSI CL; put it in the
; right package someday.
; Like maybe when it says something about doc strings, or otherwise
; becomes useful.

(defun parse-macro (name arglist body &optional env)
  (values (parse-macro-1 name arglist body env)))

; Return a list containing a special declaration for SYM
; if SYM is declared special in decls.
; This is so we can be pedantic about binding &WHOLE/&ENVIRONMENT args
; that have been scarfed out of a macro-like lambda list.
; The returned value is supposed to be suitable for splicing ...
(defun hoist-special-decls (sym decls)
  (when sym
    (dolist (decl decls)
      (dolist (spec (cdr decl))
        (when (eq (car spec) 'special)
          (dolist (s (%cdr spec))
            (when (eq s sym)
              (return-from hoist-special-decls `((declare (special ,sym)))))))))))

(defun parse-macro-1 (name arglist body &optional env)
  (parse-macro-internal name arglist body env nil))

(defun parse-macro-internal (name arglist body env default-initial-value)
  (unless (verify-lambda-list arglist t t t)
    (error "Invalid lambda list ~s" arglist))
  (multiple-value-bind (lambda-list whole environment)
      (normalize-lambda-list arglist t t)
    (multiple-value-bind (body local-decs doc)
        (parse-body body env t)
      (let ((whole-var (gensym "WHOLE"))
            (env-var (gensym "ENVIRONMENT")))
        (multiple-value-bind (bindings binding-decls)
            (%destructure-lambda-list lambda-list whole-var nil nil
                                      :cdr-p t
                                      :whole-p nil
                                      :use-whole-var t
                                      :default-initial-value default-initial-value)
          (when environment
            (setq bindings (nconc bindings (list `(,environment ,env-var)))))
          (values
            `(lambda (,whole-var ,env-var)
               (declare (ignorable ,whole-var ,env-var))
               (block ,name
                 (,@(cond ((null whole)
                           `(progn))
                          ((symbolp whole)
                           `(let ((,whole ,whole-var))))
                          (t
                           `(destructuring-bind ,whole ,whole-var)))
                  (let* ,(nreverse bindings)
                    ,@(when binding-decls `((declare ,@binding-decls)))
                    ,@local-decs
                    ,@body))))
            doc))))))

(defun lambda-list-bounds (lambda-list)
  (let* ((state :required)
         (min 0)
         (max 0))
    (do* ((lambda-list lambda-list (cdr lambda-list)))
         ((null lambda-list) (values min max))
      (case (car lambda-list)
        ((&rest &key &body) (return (values min nil)))
        (&aux (return (values min max)))
        (&optional (setq state :optional))
        (t (ecase state
             (:required (incf min) (incf max))
             (:optional (incf max))))))))
  
(defun prepare-to-destructure (list lambda-list min max)
  (if (if max
        (and (proper-list-p list)
             (let* ((len (length list)))
               (<= min len max)))
             (do* ((tail list (cdr tail))
                   (n min (1- n)))
                  ((zerop n) t)
               (when (atom tail)
                 (return))))
    list
    (let* ((reason
            (if max
              (if (not (proper-list-p list))
                "it is not a proper list"
                (let* ((len (length list)))
                  (if (eql min max)
                    (format nil "it contains ~d elements, and exactly ~d are expected" len min)
                    (if (< len min)
                      (format nil "it contains ~d elements, and at least ~d are expected" len min)
                      (format nil "it contains ~d elements, and at most ~d are expected" len max)))))
              (format nil "it does not contain at least ~d elements" min))))
      (signal-program-error "~s can't be destructured against the lambda list ~s, because ~a."
                          list lambda-list reason))))
    

(defun %destructure-lambda-list (lambda-list wholeform  lets decls
                                              &key cdr-p (whole-p t) use-whole-var default-initial-value)
  (unless (and (listp lambda-list)
               (verify-lambda-list lambda-list t whole-p))
    (signal-simple-program-error "Invalid lambda list: ~s" lambda-list))
  (multiple-value-bind (normalized whole) (normalize-lambda-list
					   lambda-list whole-p)
    (let* ((argstate :required)
	   (allow-other-keys nil)
	   (rest-arg-name nil)
	   (w (if use-whole-var wholeform (or whole (gensym "WHOLE"))))
	   (argptr (gensym "ARGS"))
	   (has-&key nil)
	   (keywords ())
	   (first-keyword-init ())
	   (restp nil))
      (labels ((simple-var (var &optional (initform `,default-initial-value))
		 (let* ((binding `(,var ,initform)))
		   (push  binding lets)
		   binding))
	       (structured-var (context sub-lambda-list initform)
		 (let* ((v (gensym (string context))))
		   (simple-var v initform)
		   (multiple-value-setq (lets decls)
		     (%destructure-lambda-list
		      sub-lambda-list
		      v
		      lets
		      decls
		      :default-initial-value default-initial-value
                      ))
		   v)))
	(unless use-whole-var
	  (if (atom w)
	    (simple-var w wholeform)
	    (progn
	      (setq w (structured-var "WHOLE" w (if cdr-p `(cdr ,wholeform) wholeform))
		    cdr-p nil))))
        (multiple-value-bind (min max) (lambda-list-bounds normalized)
          (simple-var argptr `(prepare-to-destructure ,@(if cdr-p `((cdr ,w)) `(,w)) ',lambda-list ,min ,max))
          (push `(ignorable ,argptr) decls)
          (when max
            (push `(list ,argptr) decls))
          (do* ((tail normalized (cdr tail)))
               ((null tail)
                (if has-&key
                  (let* ((key-check-form `(check-keywords
                                           ',(nreverse keywords)
                                           ,rest-arg-name ,allow-other-keys)))
                    (if first-keyword-init
                      (rplaca (cdr first-keyword-init)
                              `(progn
                                ,key-check-form
                                ,(cadr first-keyword-init)))
                      (let* ((check-var (gensym "CHECK")))
                        (push `(ignorable ,check-var) decls)
                        (simple-var check-var key-check-form)))))
                (values lets decls))
            (let* ((var (car tail)))
              (cond ((or (eq var '&rest) (eq var '&body))
                     (let* ((r (cadr tail))
                            (init argptr))
                            (if (listp r)
                              (setq rest-arg-name
                                    (structured-var "REST" r init))
                              (progn
                                (setq rest-arg-name (gensym "REST"))
                                (simple-var rest-arg-name init)
                                (simple-var r rest-arg-name ))))
                       (setq restp t)
                       (setq tail (cdr tail)))
                     ((eq var '&optional) (setq argstate :optional))
                     ((eq var '&key)
                      (setq argstate :key)
                      (setq has-&key t)
                      (unless restp
                        (setq restp t
                              rest-arg-name (gensym "KEYS"))
                        (push `(ignorable ,rest-arg-name) decls)
                        (simple-var rest-arg-name
                                    argptr)))
                     ((eq var '&allow-other-keys)
                      (setq allow-other-keys t))
                     ((eq var '&aux)
                      (setq argstate :aux))
                     ((listp var)
                      (case argstate
                        (:required
                         (structured-var "REQ" var `(pop ,argptr)))
                        (:optional
                         (let* ((variable (car var))
                                (initform (if (cdr var)
                                            (cadr var)
                                            `,default-initial-value))
                                (anon-spvar (gensym "OPT-SUPPLIED-P"))
                                (spvar (if (cddr var)
                                         (caddr var)))
                                (varinit `(if ,anon-spvar
                                           (pop ,argptr)
                                           ,initform)))
                           (simple-var anon-spvar
                                       `(not (null  ,argptr)))
                           (if (listp variable)
                             (structured-var "OPT" variable varinit)
                             (simple-var variable varinit))
                           (if spvar
                             (simple-var spvar anon-spvar))))
                        (:key
                         (let* ((explicit-key (consp (car var)))
                                (variable (if explicit-key
                                            (cadar var)
                                            (car var)))
                                (keyword (if explicit-key
                                           (caar var)
                                           (make-keyword variable)))
                                (initform (if (cdr var)
                                            (cadr var)
                                            `,default-initial-value))
                                (anon-spvar (gensym "KEY-SUPPLIED-P"))
                                (spvar (if (cddr var)
                                         (caddr var))))
                           (push keyword keywords)
                           (let* ((sp-init (simple-var anon-spvar
                                                       `(%keyword-present-p
                                                         ,rest-arg-name
                                                         ',keyword)))
                                  (var-init `(if ,anon-spvar
                                              (getf ,rest-arg-name ',keyword)
                                              ,initform)))
                             (unless first-keyword-init
                               (setq first-keyword-init sp-init))
                             (if (listp variable)
                               (structured-var "KEY" variable var-init)
                               (simple-var variable var-init))
                             (if spvar
                               (simple-var spvar anon-spvar)))))
                        (:aux
                         (simple-var (car var) (cadr var)))
                        (t (error "NYI: ~s" argstate))))
                     ((symbolp var)
                      (case argstate
                        (:required
                         (simple-var var `(pop ,argptr)))
                        (:optional
                         (simple-var var `(if ,argptr
                                           (pop ,argptr)
                                           ',default-initial-value)))
                        (:key
                         (let* ((keyword (make-keyword var)))
                           (push keyword keywords)
                           (let* ((init (simple-var
                                         var
                                         `(getf ,rest-arg-name
                                           ',keyword
                                           ,@(if default-initial-value
                                                 `(',default-initial-value))))))
                             (unless first-keyword-init
                               (setq first-keyword-init init)))))
                        (:aux
                         (simple-var var))))))))))))






(defun apply-to-htab-syms (function pkg-vector)
  (let* ((sym nil)
         (foundp nil))
    (dotimes (i (uvsize pkg-vector))
      (declare (fixnum i))
      (multiple-value-setq (sym foundp) (%htab-symbol pkg-vector i))
      (when foundp (funcall function sym)))))

(defun iterate-over-external-symbols (pkg-spec function)
  (apply-to-htab-syms function (car (pkg.etab (pkg-arg (or pkg-spec *package*))))))

(defun iterate-over-present-symbols (pkg-spec function)
  (let ((pkg (pkg-arg (or pkg-spec *package*))))
    (apply-to-htab-syms function (car (pkg.etab pkg)))
    (apply-to-htab-syms function (car (pkg.itab pkg)))))

(defun iterate-over-accessable-symbols (pkg-spec function)
  (let* ((pkg (pkg-arg (or pkg-spec *package*)))
         (used (pkg.used pkg))
         (shadowed (pkg.shadowed pkg)))
    (iterate-over-present-symbols pkg function)
    (when used
      (if shadowed
        (flet ((ignore-shadowed-conflicts (var)
                 (unless (%name-present-in-package-p (symbol-name var) pkg)
                   (funcall function var))))
          (declare (dynamic-extent #'ignore-shadowed-conflicts))
          (dolist (u used) (iterate-over-external-symbols u #'ignore-shadowed-conflicts)))
        (dolist (u used) (iterate-over-external-symbols u function))))))

(defun iterate-over-all-symbols (function)
  (dolist (pkg %all-packages%)
    (iterate-over-present-symbols pkg function)))          



;;;Eval definitions for things open-coded by the compiler.
;;;Don't use DEFUN since it should be illegal to DEFUN compiler special forms...
;;;Of course, these aren't special forms.
(macrolet ((%eval-redef (name vars &rest body)
             (when (null body) (setq body `((,name ,@vars))))
             `(setf (symbol-function ',name)
                    (qlfun ,name ,vars ,@body))))
  (declare (optimize (speed 1) (safety 1)))
  (%eval-redef %ilsl (n x))
  (%eval-redef %ilsr (n x))
  (%eval-redef neq (x y))
  (%eval-redef not (x))
  (%eval-redef null (x))
  (%eval-redef rplaca (x y))
  (%eval-redef rplacd (x y))
  (%eval-redef set-car (x y))
  (%eval-redef set-cdr (x y))
  (%eval-redef int>0-p (x))
  (%eval-redef %get-byte (ptr &optional (offset 0)) (%get-byte ptr offset))
  (%eval-redef %get-word (ptr &optional (offset 0)) (%get-word ptr offset))
  (%eval-redef %get-signed-byte (ptr &optional (offset 0)) (%get-signed-byte ptr offset))
  (%eval-redef %get-signed-word (ptr &optional (offset 0)) (%get-signed-word ptr offset))
  (%eval-redef %get-long (ptr &optional (offset 0)) (%get-long ptr offset))
  (%eval-redef %get-fixnum (ptr &optional (offset 0)) (%get-fixnum ptr offset))
  (%eval-redef %get-signed-long (ptr &optional (offset 0)) (%get-signed-long ptr offset))
  (%eval-redef %get-unsigned-long (ptr &optional (offset 0)) (%get-unsigned-long ptr offset))
  (%eval-redef %get-ptr (ptr &optional (offset 0)) (%get-ptr ptr offset))
  (%eval-redef %get-full-long (ptr &optional (offset 0)) (%get-full-long ptr offset))
  (%eval-redef %int-to-ptr (int))
  (%eval-redef %ptr-to-int (ptr))
  (%eval-redef %ptr-eql (ptr1 ptr2))
  (%eval-redef %setf-macptr (ptr1 ptr2))
  (%eval-redef %null-ptr-p (ptr))
 


  (%eval-redef %iasr (x y))

  
  (%eval-redef %set-byte (p o &optional (new (prog1 o (setq o 0))))
               (%set-byte p o new))
  (%eval-redef %set-unsigned-byte (p o &optional (new (prog1 o (setq o 0))))
               (%set-unsigned-byte p o new))
  (%eval-redef %set-word (p o &optional (new (prog1 o (setq o 0))))
               (%set-word p o new))
  (%eval-redef %set-unsigned-word (p o &optional (new (prog1 o (setq o 0))))
               (%set-unsigned-word p o new))
  (%eval-redef %set-long (p o &optional (new (prog1 o (setq o 0))))
               (%set-long p o new))
  (%eval-redef %set-unsigned-long (p o &optional (new (prog1 o (setq o 0))))
               (%set-unsigned-long p o new))
  (%eval-redef %set-ptr (p o &optional (new (prog1 o (setq o 0))))
               (%set-ptr p o new))

  
  (%eval-redef %word-to-int (word))
  (%eval-redef %inc-ptr (ptr &optional (by 1)) (%inc-ptr ptr by))
  
  (%eval-redef char-code (x))
  (%eval-redef code-char (x))
  (%eval-redef 1- (n))
  (%eval-redef 1+ (n))

  (%eval-redef uvref (x y))
  (%eval-redef uvset (x y z))
  (%eval-redef uvsize (x))

  (%eval-redef svref (x y))
  (%eval-redef svset (x y z))
  
 
  

  (%eval-redef cons (x y))
  (%eval-redef endp (x))

  (progn
    (%eval-redef typecode (x))
    (%eval-redef lisptag (x))
    (%eval-redef fulltag (x))
    (%eval-redef %unbound-marker ())
    (%eval-redef %slot-unbound-marker ())
    (%eval-redef %slot-ref (v i))
    (%eval-redef %alloc-misc (count subtag &optional (initial nil initial-p))
                 (if initial-p
                   (%alloc-misc count subtag initial)
                   (%alloc-misc count subtag)))
    (%eval-redef %setf-double-float (x y))
    (%eval-redef %lisp-word-ref (x y))
    (%eval-redef %temp-cons (x y))
    (%eval-redef require-fixnum (x))
    (%eval-redef require-symbol (x))
    (%eval-redef require-list (x))
    (%eval-redef require-real (x))
    (%eval-redef require-simple-string (x))
    (%eval-redef require-simple-vector (x))
    (%eval-redef require-character (x))
    (%eval-redef require-number (x))
    (%eval-redef require-integer (x))
    (%eval-redef require-s8 (x))
    (%eval-redef require-u8 (x))
    (%eval-redef require-s16 (x))
    (%eval-redef require-u16 (x))
    (%eval-redef require-s32 (x))
    (%eval-redef require-u32 (x))
    (%eval-redef require-s64 (x))
    (%eval-redef require-u64 (x))
    (%eval-redef %reference-external-entry-point (x))    
    )
  
  (%eval-redef %get-bit (ptr offset))
  (%eval-redef %set-bit (ptr offset val))
  (%eval-redef %get-double-float (ptr &optional (offset 0))
	       (%get-double-float ptr offset))
  (%eval-redef %get-single-float (ptr &optional (offset 0))
	       (%get-single-float ptr offset))
  (%eval-redef %set-double-float (p o &optional (new (prog1 o (setq o 0))))
	       (%set-double-float p o new))
  (%eval-redef %set-single-float (p o &optional (new (prog1 o (setq o 0))))
	       (%set-single-float p o new))
  (%eval-redef assq (item list))
  (%eval-redef %fixnum-ref-double-float (base &optional (index 0))
               (%fixnum-ref-double-float base index))
  (%eval-redef %fixnum-set-double-float (base index &optional (new (prog1 index (setq index 0))))
               (%fixnum-set-double-float base index new))
  (%eval-redef ivector-typecode-p (arg))
  (%eval-redef gvector-typecode-p (arg))
)

; In the spirit of eval-redef ...


;; pointer hacking stuff 
;
;



;;; I'd guess that the majority of bitfields in the world whose width is
;;; greater than 1 have a width of two.  If that's true, this is probably
;;; faster than trying to be more clever about it would be.
(defun %get-bitfield (ptr start-bit width)
  (declare (fixnum start-bit width))
  (do* ((bit #+big-endian-target start-bit
             #+little-endian-target (the fixnum (1- (the fixnum (+ start-bit width))))
             #+big-endian-target (1+ bit)
             #+little-endian-target (1- bit))
	(i 0 (1+ i))
	(val 0))
       ((= i width) val)
    (declare (fixnum val i bit))
    (setq val (logior (ash val 1) (%get-bit ptr bit)))))

(defun %set-bitfield (ptr start width val)
  (declare (fixnum val start width))
  (do* ((v val (ash v -1))
	(bit #+big-endian-target (1- (+ start width))
             #+little-endian-target start
             #+big-endian-target (1- bit)
             #+little-endian-target (1+ bit))
	(i 0 (1+ i)))
       ((= i width) val)
    (declare (fixnum v bit i))
    (setf (%get-bit ptr bit) (logand v 1))))

; expands into compiler stuff

(setf (symbol-function '%get-unsigned-byte) (symbol-function '%get-byte))
(setf (symbol-function '%get-unsigned-word) (symbol-function '%get-word))
(setf (symbol-function '%get-signed-long) (symbol-function '%get-long))

(defun decompose-record-accessor (accessor &aux ret)
  (do* ((str (symbol-name accessor) (%substr str (+ i 1) len))
        (len (length str) (length str))
        (i (%str-member #\. str) (%str-member #\. str))
        (field (%substr str 0 (or i len)) (%substr str 0 (or i len))))
       ((not i) (nreverse (cons (make-keyword field) ret)))
    (push (make-keyword field) ret)))




(provide 'level-2)

	


;; end of level-2.lisp

