;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2001 Clozure Associates
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

(defvar *parse-ffi-target-ftd* *target-ftd*)
(defvar *ffi-struct-return-explicit* nil)
(defvar *ffi-lisp-readtable* (copy-readtable nil))
(defvar *ffi-ordinal* -1)
(defpackage "C" (:use))
(defvar *lparen-symbol* (intern "(" (find-package "C")))
(defvar *rparen-symbol* (intern ")" (find-package "C")))
(defvar *leftbracket-symbol* (intern "[" (find-package "C")))
(defvar *rightbracket-symbol* (intern "]" (find-package "C")))
(defvar *sharp-symbol* (intern "#" (find-package "C")))
(defvar *sharp-sharp-symbol* (intern "##" (find-package "C")))
(defvar *comma-symbol* (intern "," (find-package "C")))


(defstruct (ffi-macro (:include ffi-type))
  args
  expansion
  disposition
  tokens
  expression )

(defstruct (ffi-enum (:include ffi-type)))

(defvar *ffi-typedefs*)
(defvar *ffi-global-typedefs* nil)
(defvar *ffi-unions*)
(defvar *ffi-global-unions* nil)
(defvar *ffi-transparent-unions* nil)
(defvar *ffi-global-transparent-unions* nil)
(defvar *ffi-structs*)
(defvar *ffi-global-structs* nil)
(defvar *ffi-functions*)
(defvar *ffi-global-functions* nil)
(defvar *ffi-global-constants* nil)
(defvar *ffi-global-vars* nil)
(defvar *ffi-objc-classes* nil)
(defvar *ffi-global-objc-classes* nil)
(defvar *ffi-global-objc-messages* nil)
(defvar *ffi-macros*)
(defvar *ffi-vars*)

(defvar *ffi-void-reference* '(:primitive :void))



(defun find-or-create-ffi-struct (string)
  (or (gethash string *ffi-structs*)
      (setf (gethash string *ffi-structs*)
            (make-ffi-struct :string string
                             :name (unless (digit-char-p (schar string 0))
                                     (escape-foreign-name string))))))

(defun find-or-create-ffi-union (string)
  (or (gethash string *ffi-unions*)
      (setf (gethash string *ffi-unions*)
            (make-ffi-union :string string
                            :name (unless (digit-char-p (schar string 0))
                                    (escape-foreign-name string))))))

(defun find-or-create-ffi-transparent-union (string)
  (or (gethash string *ffi-transparent-unions*)
      (setf (gethash string *ffi-transparent-unions*)
            (make-ffi-transparent-union :string string
                                        :name (unless (digit-char-p (schar string 0))
                                                (escape-foreign-name string))))))

(defun find-or-create-ffi-objc-class (string)
  (or (gethash string *ffi-objc-classes*)
      (setf (gethash string *ffi-objc-classes*)
            (make-ffi-objc-class :string string
                                 :name (escape-foreign-name string)))))

(defun find-or-create-ffi-objc-message (string)
  (or (gethash string *ffi-global-objc-messages*)
      (setf (gethash string *ffi-global-objc-messages*)
            (make-ffi-objc-message :string string))))

(defun find-or-create-ffi-typedef (string)
  (or (gethash string *ffi-typedefs*)
      (setf (gethash string *ffi-typedefs*)
            (make-ffi-typedef :string string
                              :name (escape-foreign-name string)))))

(defun eval-complex-c-expression (string constant-alist)
  (declare (ignore string constant-alist)))

(defun eval-c-float-string (string)
  (setq string (nstring-upcase string))
  ;; Make the c float string (which may contain trailing garbage)
  ;; look enough like a lisp float string that READ-FROM-STRING will
  ;; work.
  ;; There can be some trailing garbage on the string, or it might
  ;; end in a decimal point.
  ;; The trailing garbage might be a size specifier : #\L or #\F,
  ;; to denote a LONG-DOUBLE or a (single) FLOAT.
  ;; MCL can't deal with LONG-DOUBLEs, and will assume that an
  ;; unqualified float constant is a SINGLE-FLOAT (or whatever
  ;; *READ-DEFAULT-FLOAT-FORMAT* says.  We may have to add or
  ;; change an exponent marker.
  (let* ((lastpos (1- (length string)))
         (lastchar (schar string lastpos))
         (size :double))
    (case lastchar
      (#\L (setq size :long-double) (setf (schar string lastpos) #\Space))
      (#\F (setq size :single) (setf (schar string lastpos) #\Space))
      (#\. (setq string (concatenate 'string string "0"))))
    (unless (eq size :long-double)
      (let* ((epos (position #\E string))
             (dpos (position #\D string)))
        (if (eq size :double)
          (if epos
            (setf (schar string epos) #\d)
            (setq string (concatenate 'string string "d0")))
          (if dpos
            (setf (schar string dpos) #\e))))
      (values (ignore-errors (let* ((*readtable* *ffi-lisp-readtable*))
                               (read-from-string string)))))))

(defun read-c-number (stream char)
  (loop collect char into chars
        with class = :integer
        with hex = nil
        with octal = (eql char #\0)
        do (setq char (read-char stream nil nil))
        while (or (find char "0123456789abcdefABCDEFxulXUL.")
                  (and (find char "+-")
                       (char-equal (car (last chars)) #\e)))   ;signed exponent
        do (cond ((char-equal char #\x) 
                  (setq hex t octal nil))
                 ((and (not hex) (or (char-equal char #\.) (char-equal char #\e)))
                  (setq class :float)))
        finally
        (when char (unread-char char stream))
        (setq chars (coerce chars 'string))
        (if (eq class :integer)
          (return
            (values
             (ignore-errors
               (parse-integer chars
                              :start (if hex 2 0)
                              :radix (if hex 16 (if octal 8 10))
                              :junk-allowed t))))
          (return (eval-c-float-string chars)))))

(defun eval-c-number (string char)
  (loop collect char into chars
        with class = :integer
        with hex = nil
        with octal = (eql char #\0)
        with len = (length string)
        with i = 0
        do (setq char (if (< (incf i) len) (schar string i)))
        while (or (find char "0123456789abcdefABCDEFxulXUL.")
                  (and (find char "+-")
                       (char-equal (car (last chars)) #\e)))   ;signed exponent
        do (cond ((char-equal char #\x) 
                  (setq hex t octal nil))
                 ((and (not hex) (or (char-equal char #\.) (char-equal char #\e)))
                  (setq class :float)))
        finally
          (setq chars (coerce chars 'string))
          (if (eq class :integer)
            (return
              (values
               (ignore-errors
                 (parse-integer chars
                                :start (if hex 2 0)
                                :radix (if hex 16 (if octal 8 10))
                                :junk-allowed t))))
            (return (eval-c-float-string chars)))))

;;; For our purposes (evaluating constant expressions in C macros),
;;; we don't have to get this exactly right (since the result is
;;; only going to be used in a size-of or cast operation.)
;;; All pointer types would therefore look identical.

(defvar *the-ffi-pointer-type* (parse-foreign-type '(* t)))

;;; If we don't get this right the first time, we never will;
;;; if there's nothing better, just return the void type.

(defvar *the-ffi-void-type* (parse-foreign-type :void))

(defun parse-c-ffi-type (spec)
  (flet ((parse-it-or-lose (spec)
           (or (ignore-errors (parse-foreign-type spec))
               *the-ffi-void-type*))
         (make-type-name (name)
	   (escape-foreign-name (string name))))
    (cond ((eq (car (last spec)) 'c::*) *the-ffi-pointer-type*)
          ((member (car spec) '(c::|struct| c::|union|))
           (parse-it-or-lose (mapcar #'make-type-name spec)))
          ((null (cdr spec))
           (parse-it-or-lose (make-type-name (car spec))))
          (t
           ;;; A qualified primitive type
           (let* ((primitive (parse-it-or-lose (make-type-name (car (last spec))))))
             (if (eq primitive *the-ffi-void-type*)
               primitive
               (let* ((long 0)
                      (explicitly-signed nil))
                 (declare (fixnum long))
                 (if
                   (dolist (token (butlast spec) t)
                     (case token
                       (c::|unsigned| (setq explicitly-signed :unsigned))
                       (c::|signed| (setq explicitly-signed :signed))
                       (c::|long| (incf long))
                       (c::|short| (decf long))
                       (t (return nil))))
                   (cond ((typep primitive 'foreign-integer-type)
                          (let* ((prim-bits (foreign-type-bits primitive))
                                 (prim-signed (foreign-integer-type-signed primitive)))
                            (if (> long 1)
                              (make-foreign-integer-type :bits 64
                                                         :signed (or (not explicitly-signed)
                                                                     (eq explicitly-signed :signed)))
                              (if (< long 0)
                                (make-foreign-integer-type :bits 16
                                                           :signed (or (not explicitly-signed)
                                                                       (eq explicitly-signed :signed)))
                                (if (= long 1)
                                  (make-foreign-integer-type :bits 32
                                                             :signed (or (not explicitly-signed)
                                                                         (eq explicitly-signed :signed)))
                                  (make-foreign-integer-type :bits prim-bits
                                                             :signed
                                                             (case explicitly-signed
                                                               (:signed t)
                                                               (:unsigned nil)
                                                               (t prim-signed))))))))
                         ((and (= long 1)
                               (typep primitive 'foreign-double-float-type))
                          (parse-it-or-lose :long-double))
                         (t *the-ffi-void-type*))
                   *the-ffi-void-type*))))))))
                                                               
(defun eval-parsed-c-expression (expression constant-alist)
  (if (atom expression)
    (if (identifierp expression)
      (find-constant expression constant-alist)
      (if (typep expression 'character)
        (char-code expression)
        expression))
    (let* ((operator (car expression))
           (operands (cdr expression))
           (noperands (length operands)))
      (case operator
        (c::resolve-type (let* ((foreign-type  (ignore-errors (parse-c-ffi-type (car operands)))))
                           (when foreign-type
                             (setf (cdr expression) nil
                                   (car expression) foreign-type)
                             )))
        (c::curly-bracketed-list ())
        (t
         (if (typep operator 'foreign-type)
           operator
         (when (do* ((tail (cdr expression) (cdr tail)))
                    ((null tail) t)
                 (let* ((expr (car tail))
                        (value (eval-parsed-c-expression expr constant-alist)))
                   (unless value (return))
                   (unless (eq expr value)
                     (rplaca tail value))))
           (case noperands
             (1
              (let* ((operand (cadr expression)))
                (case operator
                  (c::! (if (zerop operand) 1 0))
                  (c::- (- operand))
		  (c::+ operand)
                  (c::~ (lognot operand))
                  (c::size-of
                   (let* ((bits (ignore-errors (ensure-foreign-type-bits operand))))
                     (when bits
                       (ash (+ bits 7) -3))))
                  (t
                   ;(break "~s" expression)
		   nil))))
             (2
              (let* ((a (car operands))
                     (b (cadr operands)))
                (case operator
                  (c::<< (ash a b))
                  (c::>> (ash a (- b)))
                  (c::* (* a b))
                  (c::/ (if (zerop b) 0 (values (floor a b)))) ; or maybe TRUNCATE ?
                  (c::+ (+ a b))
                  (c::- (- a b))
                  (c::\| (logior a b))
                  (c::\& (logand a b))
                  (c::cast (if (foreign-typep b a) b))
                  (t 
		   ;(break "binary op = ~s ~s ~s" operator a b)
		   nil))))
             (t
              ;(break "expression = ~s" expression)
	      nil)))))))))

(defun eval-c-expression (macro constant-alist macro-table)
  (let* ((string (ffi-macro-expansion macro))
         (len (length string)))
    (if (= len 0)
      1
      (progn
        (unless (ffi-macro-tokens macro)
          (multiple-value-bind (tokens error) (ignore-errors (string-to-tokens string))
            (if error
              (setf (ffi-macro-disposition macro) :bad-tokenize)
              (setf (ffi-macro-tokens macro) tokens))))
        (unless (ffi-macro-expression macro)
          (let* ((tokens (ffi-macro-tokens macro)))
            (when tokens
              (multiple-value-bind (expression error)
                  (ignore-errors (parse-c-expression tokens
                                                     :constants constant-alist
                                                     :expand-macros macro-table ))
                (if (or error (null expression))
                  (progn
                    ;(format t "~& parse failed: ~s ~s" (ffi-macro-name macro)  string)
                    ;(format t "~&  tokens = ~s, error = ~a" tokens error)
                    (setf (ffi-macro-disposition macro) :bad-parse))
                  (setf (ffi-macro-expression macro) expression))))))
        (let* ((expression (ffi-macro-expression macro)))
          (when expression (values (eval-parsed-c-expression expression constant-alist) t)))))))

;;; Repeatedly iterate over the macros until nothing new's defined.
(defun process-defined-macros (ffi-macros constant-alist parameterized-macros)
  (let* ((new-def ()))
    (loop
        (setq new-def nil)
        (dolist (macro ffi-macros)
          (unless (ffi-macro-disposition macro)
            (let* ((expansion (ffi-macro-expansion macro))
                   (name (ffi-macro-name macro))
                   (value nil))
              (if (string= name expansion)
                (setf (ffi-macro-disposition macro) t)
                (when (setq value (eval-c-expression macro constant-alist parameterized-macros))
                  (push (cons name value) constant-alist)
                  (setf (ffi-macro-disposition macro) t)
                  (setq new-def t))))))
        (unless new-def
          (return (values (reverse constant-alist) nil))))))

(defun reference-ffi-type (spec)
  (case (car spec)
    (:typedef (list :typedef (find-or-create-ffi-typedef (cadr spec))))
    (:struct-ref (list :struct (find-or-create-ffi-struct (cadr spec))))
    (:union-ref (list :union (find-or-create-ffi-union (cadr spec))))
    (:transparent-union-ref
     (list :transparent-union (find-or-create-ffi-transparent-union (cadr spec))))
    (:enum-ref `(:primitive :signed))
    (:function `(:primitive (* t)))
    (:pointer (list :pointer (reference-ffi-type (cadr spec))))
    (:array (list :array (cadr spec) (reference-ffi-type (caddr spec))))
    (:void *ffi-void-reference*)
    (t
     (list :primitive
           (ecase (car spec)
	     (:char (if (getf (ftd-attributes *parse-ffi-target-ftd*)
                              :signed-char)
		      '(:signed 8)
		      '(:unsigned 8)))
             (:signed-char  '(:signed 8))
             (:unsigned-char '(:unsigned 8))
             (:short '(:signed 16))
             (:unsigned-short '(:unsigned 16))
             ((:vec128 :unsigned-long-long-long) '(:unsigned 128))
             (:signed-long-long-long '(:signed 128))
             (:int '(:signed 32))
             (:long (ecase (or
                            (getf
                             (ftd-attributes *parse-ffi-target-ftd*)
                             :bits-per-long)
                            (getf
                             (ftd-attributes *parse-ffi-target-ftd*)
                             :bits-per-word))
                      (32 '(:signed 32))
                      (64 '(:signed 64))))
             (:unsigned  '(:unsigned 32))
             (:unsigned-long (ecase (or
                                     (getf
                                      (ftd-attributes *parse-ffi-target-ftd*)
                                      :bits-per-long)
                                     (getf
                                      (ftd-attributes *parse-ffi-target-ftd*)
                                      :bits-per-word))
                               (32 '(:unsigned 32))
                               (64 '(:unsigned 64))))
             (:long-long '(:signed 64))
             ((:vec64 :unsigned-long-long) '(:unsigned 64))
             (:float :float)
             (:double :double)
             (:long-double :long-float)
             (:complex-int :complex-int)
             (:complex-float :complex-float)
             (:complex-double :complex-double)
             (:complex-long-double :complex-long-float)
             (:long-long-long :long-long-long)
             #|(:void :void)|#)))))
             
             
(defun process-ffi-fieldlist (fields)
  (let* ((parsed-fields ()))
    (dolist (field fields (nreverse parsed-fields))
      (let* ((field-name (escape-foreign-name (car field)))
             (field-descr (cadr field)))
        (destructuring-bind (field-type offset width)
            (cdr field-descr)
          (push (cons field-name
                      (ecase (car field-descr)
                        (:field `(,(reference-ffi-type field-type) ,(ash offset 3) ,(ash width 3)))
                        (:bitfield `((:primitive (:unsigned ,width)) ,offset ,width))))
                parsed-fields))))))

(defun process-ffi-union (form)
  (destructuring-bind (source-info string fields &optional alignform)
      (cdr form)
    (declare (ignore source-info))
    (let* ((union (find-or-create-ffi-union string)))
      (setf (ffi-union-ordinal union) (incf *ffi-ordinal*))
      (when alignform
	(setf (ffi-union-alt-alignment-bits union) (cadr alignform)))
      (unless (ffi-union-fields union)
	(setf (ffi-union-fields union)
	      (process-ffi-fieldlist fields)))
      union)))

(defun process-ffi-transparent-union (form)
  (destructuring-bind (source-info string fields &optional alignform)
      (cdr form)
    (declare (ignore source-info))
    (let* ((union (find-or-create-ffi-transparent-union string)))
      (setf (ffi-transparent-union-ordinal union) (incf *ffi-ordinal*))
      (when alignform
	(setf (ffi-transparent-union-alt-alignment-bits union) (cadr alignform)))
      (unless (ffi-transparent-union-fields union)
	(setf (ffi-transparent-union-fields union)
	      (process-ffi-fieldlist fields)))
      union)))

(defun process-ffi-struct (form)
  (destructuring-bind (source-info string fields &optional alignform)
      (cdr form)
    (declare (ignore source-info))
    (let* ((struct (find-or-create-ffi-struct string)))
      (setf (ffi-struct-ordinal struct) (incf *ffi-ordinal*))
      (when alignform
	(setf (ffi-struct-alt-alignment-bits struct) (cadr alignform)))
      (unless (ffi-struct-fields struct)
	(setf (ffi-struct-fields struct)
	      (process-ffi-fieldlist fields)))
      struct)))

(defun process-ffi-objc-class (form)
  (destructuring-bind (source-info class-name superclass-form protocols ivars) (cdr form)
    (declare (ignore source-info))
    (let* ((class (find-or-create-ffi-objc-class class-name)))
      (setf (ffi-objc-class-ordinal class) (incf *ffi-ordinal*))
      (unless (ffi-objc-class-super-foreign-name class)
        (let* ((super-name (car superclass-form)))
          (unless (eq super-name :void)
            (setf (ffi-objc-class-super-foreign-name class)
                  super-name))))
      (unless (ffi-objc-class-protocol-names class)
        (setf (ffi-objc-class-protocol-names class) protocols))
      (unless (ffi-objc-class-own-ivars class)
        (setf (ffi-objc-class-own-ivars class)
              (process-ffi-fieldlist ivars)))
      class)))

(defun process-ffi-objc-method (form)
  (destructuring-bind (method-type source-info class-name category-name message-name arglist result-type) form
    (declare (ignore source-info category-name))
    (let* ((flags ()))
      (if (or (eq method-type :objc-class-method)
              (eq method-type :objc-protocol-class-method))
        (setf (getf flags :class) t))
      (if (or (eq method-type :objc-protocol-class-method)
              (eq method-type :objc-protocol-instance-method))
        (setf (getf flags :protocol) t))
      (let* ((message (find-or-create-ffi-objc-message message-name))
             (class-method-p (getf flags :class))
             (method
              (make-ffi-objc-method :class-name class-name
                                    :arglist (mapcar #'reference-ffi-type
                                                     arglist)
                                    :result-type (reference-ffi-type
                                                  result-type)
                                    :flags flags)))
        (unless (dolist (m (ffi-objc-message-methods message))
                  (when (and (equal (ffi-objc-method-class-name m)
                                    class-name)
                             (eq (getf (ffi-objc-method-flags m) :class)
                                 class-method-p))
                    (return t)))
          (push method (ffi-objc-message-methods message)))))))
      
(defun process-ffi-typedef (form)
  (let* ((string (caddr form))
         (def (find-or-create-ffi-typedef string)))
    (setf (ffi-typedef-ordinal def) (incf *ffi-ordinal*))
    (unless (ffi-typedef-type def)
      (setf (ffi-typedef-type def) (reference-ffi-type (cadddr form))))
    def))


(defun process-ffi-function (form)
  (let* ((name (caddr form))
         (ftype (cadddr form)))
    (make-ffi-function :string name
                       :arglist (mapcar #'reference-ffi-type (cadr ftype))
                       :return-value (reference-ffi-type (caddr ftype)))))

(defun process-ffi-macro (form)
  (let* ((name-form (caddr form))
         (expansion (cadddr form))
         (name name-form)
         (args nil)
         (space-pos (position #\space name-form)))
    (when space-pos
      (setq name (subseq name-form 0 space-pos))
      (let* ((open-pos (position #\( name-form))
             (close-pos (position #\) name-form)))
        (when (and open-pos close-pos (> close-pos open-pos))
          (let* ((arg-string (subseq name-form open-pos close-pos))
                 (arg-tokens (ignore-errors (string-to-tokens arg-string)))
                 (arg-names (let* ((l ()))
                              (dolist (arg-token arg-tokens (nreverse l))
                                (unless (or (eq arg-token 'c::|,|)
                                            (eq arg-token *lparen-symbol*))
                                  (push arg-token l)))))
                 (body-tokens (ignore-errors (string-to-tokens expansion))))
            (when (and arg-names body-tokens)
              (setq args (list arg-names body-tokens)
                    expansion name))))))
    (make-ffi-macro :name name :args args :expansion expansion)))

(defun process-ffi-enum (form)
  (declare (ignore form)))

(defun process-ffi-var (form)
  (let* ((name (caddr form))
         (type (cadddr form)))
    (cons name (reference-ffi-type type))))

(defun process-ffi-enum-ident (form)
  (cons (caddr form) (cadddr form)))

(defun ensure-referenced-type-defined (spec)
  (declare (ignorable spec))
  (when nil
  (ecase (car spec)
    (:primitive)
    (:typedef (define-typedef-from-ffi-info (cadr spec)))
    (:struct (ensure-struct-defined (cadr spec)))
    (:union (ensure-union-defined (cadr spec)))
    (:transparent-union (ensure-transparent-union-defined (cadr spec)))
    (:pointer (ensure-referenced-type-defined (cadr spec)))
    (:array (ensure-referenced-type-defined (caddr spec)))
    (:function (dolist (arg (ffi-function-arglist (cadr spec)))
                 (ensure-referenced-type-defined arg))
               (ensure-referenced-type-defined (ffi-function-return-value (cadr spec))))
    )))

  
(defun ensure-fields-defined (fields)
  (dolist (f fields)
    (let* ((ftype (cadr f)))
      (ensure-referenced-type-defined ftype))))

(defun record-global-objc-class (c)
  (when *ffi-global-objc-classes*
    (setf (gethash (ffi-objc-class-string c) *ffi-global-objc-classes*) c)))

(defun define-objc-class-from-ffi-info (c)
  (unless (ffi-objc-class-defined c)
    (setf (ffi-objc-class-defined c) t)
    (record-global-objc-class c)
    (ensure-fields-defined (ffi-objc-class-own-ivars c))))

(defun record-global-union (u)
  (when *ffi-global-unions*
    (setf (gethash (ffi-union-reference u) *ffi-global-unions*) u)))

(defun record-global-transparent-union (u)
  (when *ffi-global-transparent-unions*
    (setf (gethash (ffi-transparent-union-reference u) *ffi-global-transparent-unions*) u)))

(defun define-union-from-ffi-info (u)
  (unless (ffi-union-defined u)
    (setf (ffi-union-defined u) t)
    (record-global-union u)
    (when (ffi-union-name u)
      (let* ((fields (ffi-union-fields u)))
        (ensure-fields-defined fields)))))

(defun define-transparent-union-from-ffi-info (u)
  (unless (ffi-transparent-union-defined u)
    (setf (ffi-transparent-union-defined u) t)
    (record-global-transparent-union u)
    (when (ffi-transparent-union-name u)
      (let* ((fields (ffi-transparent-union-fields u)))
        (ensure-fields-defined fields)))))

(defun ensure-union-defined (u)
  (let* ((name (ffi-union-name u)))
    (if name
      (define-union-from-ffi-info u)
      (ensure-fields-defined (ffi-union-fields u)))))

(defun ensure-transparent-union-defined (u)
  (let* ((name (ffi-transparent-union-name u)))
    (if name
      (define-transparent-union-from-ffi-info u)
      (ensure-fields-defined (ffi-transparent-union-fields u)))))

(defun record-global-struct (s)
  (when *ffi-global-structs*
    (setf (gethash (ffi-struct-reference s) *ffi-global-structs*) s)))

(defun define-struct-from-ffi-info (s)
  (unless (ffi-struct-defined s)
    (setf (ffi-struct-defined s) t)
    (record-global-struct s)
    (when (typep (ffi-struct-name s) 'keyword)
      (let* ((fields (ffi-struct-fields s)))
        (ensure-fields-defined fields)))))

(defun ensure-struct-defined (s)
  (let* ((name (ffi-struct-name s)))
    (if (typep name 'keyword)
      (define-struct-from-ffi-info s)
      (ensure-fields-defined (ffi-struct-fields s)))))

(defun record-global-typedef (def)
  (when *ffi-global-typedefs*
    (setf (gethash (ffi-typedef-string def) *ffi-global-typedefs*) def)))
  
(defun define-typedef-from-ffi-info (def)
  (unless (ffi-typedef-defined def)
    (setf (ffi-typedef-defined def) t)
    (record-global-typedef def)
    (let* ((target (ffi-typedef-type def)))
      (unless (and (consp target)
		   (member (car target) '(:struct :union :transparent-union :primitive)))
	(ensure-referenced-type-defined target)))))

(defun record-global-constant (name val)
  (when *ffi-global-constants*
    (setf (gethash name *ffi-global-constants*) val)))
      
(defun emit-ffi-constant (name val)
  (record-global-constant name val))

(defun record-global-var (name type)
  (when *ffi-global-vars*
    (setf (gethash name *ffi-global-vars*) type)))

(defun emit-ffi-var (name type)
  (record-global-var name type))


(defun ffi-record-type-p (typeref)
  (case (car typeref)
    ((:struct :union :transparent-union) t)
    (:typedef (ffi-record-type-p (ffi-typedef-type (cadr typeref))))
    (t nil)))

(defun record-global-function (ffi-function)
  (when *ffi-global-functions*
    (setf (gethash (ffi-function-string ffi-function) *ffi-global-functions*)
	  ffi-function)))

(defun emit-function-decl (ffi-function)
  (let* ((args (ffi-function-arglist ffi-function))
         (retval (ffi-function-return-value ffi-function)))
    (if (eq (car (last args)) *ffi-void-reference*)
      (setq args (butlast args)))
    (when (ffi-record-type-p retval)
      (if  *ffi-struct-return-explicit*
        (format t "~&;; Note: explict struct return in function ~s" (ffi-function-string  ffi-function))
        (progn
          (push retval args)
          (push `(:pointer ,retval) (ffi-function-arglist ffi-function))
          (setf (ffi-function-return-value ffi-function) *ffi-void-reference*)
          (setq retval *ffi-void-reference*))))
    (dolist (arg args) (ensure-referenced-type-defined arg))
    (ensure-referenced-type-defined retval)
    (record-global-function ffi-function)))
  
(defun parse-ffi (inpath)
  (let* ((*ffi-typedefs* (make-hash-table :test 'string= :hash-function 'sxhash))
         (*ffi-unions* (make-hash-table :test 'string= :hash-function 'sxhash))
         (*ffi-transparent-unions* (make-hash-table :test 'string= :hash-function 'sxhash))
         (*ffi-structs* (make-hash-table :test 'string= :hash-function 'sxhash))
         (*ffi-objc-classes* (make-hash-table :test 'string= :hash-function 'sxhash)) 
         (argument-macros (make-hash-table :test 'equal)))
    (let* ((defined-types ())
           (defined-constants ())
           (defined-macros ())
           (defined-functions ())
           (defined-vars ()))
      (with-open-file (in inpath)
        (let* ((*ffi-ordinal* -1))
          (let* ((*package* (find-package "KEYWORD")))
            (do* ((form (read in nil :eof) (read in nil :eof)))
                 ((eq form :eof))
              (case (car form)
                (:struct (push (process-ffi-struct form) defined-types))
                (:objc-class (push (process-ffi-objc-class form) defined-types))
                ((:objc-class-method
                  :objc-instance-method
                  :objc-protocol-class-method
                  :objc-protocol-instance-method
                  )
                 (process-ffi-objc-method form))
                (:function (push (process-ffi-function form) defined-functions))
                (:macro (let* ((m (process-ffi-macro form))
                               (args (ffi-macro-args m)))
                          (if args
                            (setf (gethash (string (ffi-macro-name m)) argument-macros) args)
			    (push m defined-macros))))
                (:type (push (process-ffi-typedef form) defined-types))
                (:var (push (process-ffi-var form) defined-vars))
                (:enum-ident (push (process-ffi-enum-ident form) defined-constants))
                (:enum (process-ffi-enum form))
                (:union (push (process-ffi-union form) defined-types))
                (:transparent-union (push (process-ffi-transparent-union form) defined-types)))))
          (multiple-value-bind (new-constants new-macros)
              (process-defined-macros defined-macros (reverse defined-constants) argument-macros)
	    ;; If we're really lucky, we might be able to turn some C macros
	    ;; into lisp macros.  We can probably turn some C macros into
	    ;; lisp constants.
            (declare (ignore new-macros))
            (dolist (x (reverse new-constants))
              (emit-ffi-constant (car x) (cdr x)))
            (dolist (x defined-vars)
              (emit-ffi-var (car x) (cdr x)))
            (dolist (x (sort defined-types #'< :key #'ffi-type-ordinal))
              (typecase x
                (ffi-struct (define-struct-from-ffi-info x))
                (ffi-union (define-union-from-ffi-info x))
                (ffi-transparent-union (define-transparent-union-from-ffi-info x))
                (ffi-typedef (define-typedef-from-ffi-info x))
                (ffi-objc-class (define-objc-class-from-ffi-info x))))
            (dolist (f defined-functions) (emit-function-decl f))))))))

(defun parse-standard-ffi-files (dirname &optional target)
  (let* ((backend (if target (find-backend target) *target-backend*))
         (ftd (backend-target-foreign-type-data backend))
         (*parse-ffi-target-ftd* ftd)
         (*target-ftd* ftd)
         (*target-backend* backend)
         (*ffi-struct-return-explicit* nil)
	 (d (use-interface-dir dirname ftd))
	 (interface-dir (merge-pathnames
			 (interface-dir-subdir d)
			 (ftd-interface-db-directory ftd)))
	 (*prepend-underscores-to-ffi-function-names*
          (getf (ftd-attributes ftd) :prepend-underscores))
	 (*ffi-global-typedefs* (make-hash-table :test 'string= :hash-function 'sxhash))
	 (*ffi-global-unions* (make-hash-table :test 'string= :hash-function 'sxhash))
         (*ffi-global-transparent-unions* (make-hash-table :test 'string= :hash-function 'sxhash))
	 (*ffi-global-structs* (make-hash-table :test 'string= :hash-function 'sxhash))
         (*ffi-global-objc-classes* (make-hash-table :test 'string= :hash-function 'sxhash))
         (*ffi-global-objc-messages* (make-hash-table :test 'string= :hash-function 'sxhash)) 
	 (*ffi-global-functions* (make-hash-table :test 'string= :hash-function 'sxhash))
	 (*ffi-global-constants* (make-hash-table :test 'string= :hash-function 'sxhash))
         (*ffi-global-vars* (make-hash-table :test 'string= :hash-function 'sxhash)))
         
    (dolist (f (directory (merge-pathnames ";C;**;*.ffi"
					   interface-dir)))
      (format t "~&~s ..." f)
      (parse-ffi f )
      (format t "~&"))
    (with-new-db-file (constants-cdbm (merge-pathnames
                                       "new-constants.cdb"
                                       interface-dir))
      (maphash #'(lambda (name def)
                   (db-define-constant constants-cdbm name def))
	       *ffi-global-constants*))
    (with-new-db-file (types-cdbm (merge-pathnames
				       "new-types.cdb"
				       interface-dir))
      (maphash #'(lambda (name def)
		   (declare (ignore name))
		   (save-ffi-typedef types-cdbm def))
	       *ffi-global-typedefs*))
    (with-new-db-file (records-cdbm (merge-pathnames
                                     "new-records.cdb"
                                     interface-dir))
      (maphash #'(lambda (name def)
		   (declare (ignore name))
                   (save-ffi-union records-cdbm def))
	       *ffi-global-unions*)
      (maphash #'(lambda (name def)
                   (declare (ignore name))
                   (save-ffi-transparent-union records-cdbm def))
               *ffi-global-transparent-unions*)
                         
      (maphash #'(lambda (name def)
		   (declare (ignore name))
		   (save-ffi-struct records-cdbm def))
	       *ffi-global-structs*))
    (with-new-db-file (function-cdbm (merge-pathnames
					   "new-functions.cdb"
					   interface-dir))
      (maphash #'(lambda (name def)
		   (declare (ignore name))
		   (save-ffi-function function-cdbm def))
	       *ffi-global-functions*))
    (with-new-db-file (class-cdbm (merge-pathnames
                                   "new-objc-classes.cdb"
                                   interface-dir))
      (maphash #'(lambda (name def)
                   (declare (ignore name))
                   (save-ffi-objc-class class-cdbm def))
               *ffi-global-objc-classes*))
    (with-new-db-file (vars-cdbm (merge-pathnames
                             "new-vars.cdb"
                             interface-dir))
      (maphash #'(lambda (name type)
                   (db-define-var vars-cdbm name type))
               *ffi-global-vars*))
    (with-new-db-file (methods-cdbm  (merge-pathnames
                                      "new-objc-methods.cdb"
                                      interface-dir))
      (maphash #'(lambda (name message)
                   (declare (ignore name))
                   (save-ffi-objc-message methods-cdbm message))
               *ffi-global-objc-messages*))
    (install-new-db-files ftd d)))

(defvar *c-readtable* (copy-readtable nil))
(setf (readtable-case *c-readtable*) :preserve)


;;; Each element of operators can be a symbol or a list of a symbol, a
;;; function, and args All the symbols must start with the character
;;; for which this is the macro-character fcn The entries must be in
;;; the right order, e.g. dictionary order, so any two symbols with a
;;; common prefix are adjacent in the list.  Furthermore each symbol
;;; in the list must be preceded by every non-empty leading substring
;;; of that symbol, since we only have one character of look-ahead in
;;; the stream.
(defun operator-macro (operators)
  ;; The tree is an alist keyed by character (with a nil key at the end for the default)
  ;; The cdr of each entry is either a symbol to produce, another decision tree,
  ;; or a list of a function to call and additional arguments for the function
  (let ((decision-tree (make-decision-tree operators)))
    (labels ((read-c-operator (stream char)
               (declare (ignore char))
               (loop with decision-tree = decision-tree
                     as char = (read-char stream nil nil)   ; eof => nil which works too
                     as elem = (assoc char decision-tree)
                     do (unless elem
                          (unread-char char stream)
                          (setq elem (assoc nil decision-tree)))
                        (setq elem (cdr elem))
                        (cond ((symbolp elem) 
                               (return elem))
                              ((symbolp (car elem)) 
                               (return (apply (car elem) stream (cdr elem))))
                              (t (setq decision-tree elem)))))
             (read-c-singleton-operator (stream char)
               (declare (ignore stream char))
               (first operators))
             (read-c-macro-character (stream char)
               (declare (ignore char))
               (apply (car decision-tree) stream (cdr decision-tree))))
      (cond ((symbolp decision-tree) #'read-c-singleton-operator)
            ((consp (car decision-tree)) #'read-c-operator)
            (t #'read-c-macro-character)))))

(defun make-decision-tree (operators)
  (labels ((recurse (operators chars-so-far) ;returns new operators and decision tree element
             (let ((next-char (aref (key (first operators))
                                    (length chars-so-far)))
                   (alist nil))
               (setq chars-so-far (append chars-so-far (list next-char)))
               (loop while operators
                 as key = (key (first operators))
                 while (every #'char= key chars-so-far)
                 do (if (= (length key) (length chars-so-far))
                      (push (cons nil (val (pop operators))) alist)
                      (multiple-value-bind (remaining-operators elem)
                          (recurse operators chars-so-far)
                        (push elem alist)
                        (setq operators remaining-operators))))
               (values operators 
                       (cons next-char (if (cdr alist) alist (cdar alist))))))
           (key (operator)
             (string (if (atom operator) operator (car operator))))
           (val (operator)
             (if (atom operator) operator (cdr operator))))
    (multiple-value-bind (left-over elem) (recurse operators nil)
      (when left-over
        (error "Malformed operators list ~S:~S" (ldiff operators left-over) left-over))
      (cdr elem))))

;;; Doesn't support the L prefix for wide characters.  What a complete kludge!
(defun c-read-string (stream single-quote)
  (loop with delimiter = (if single-quote #\' #\")
        as char = (read-char stream nil nil)
        do (cond ((null char)
                  (c-parse-error stream "Unmatched ~A" delimiter))
                 ((char= char delimiter)

                  (return (if single-quote
                              (char-code (car chars))
                              (coerce chars 'string))))
                 ((char= char #\\)
                  (setq char (read-char stream nil nil))
                  (unless char (c-parse-error stream "EOF after backslash in string"))
                  (let ((tem (assoc char '((#\n . #\newline)
                                           (#\t . #\tab)
                                           (#\v . #\^K)
                                           (#\b . #\backspace)
                                           (#\r . #\return)
                                           (#\f . #\page)
                                           (#\a . #\bell)
                                           (#\\ . #\\)
                                           (#\? . #\?)
                                           (#\' . #\')
                                           (#\" . #\")))))
                    (cond (tem (setq char (cdr tem)))
                          ((char<= #\0 char #\7)
                           (setq char (loop while (char<= #\0 char #\7) for count from 1
                                            with sum = 0
                                            do (setq sum (+ (* sum 8) (digit-char-p char)))
                                               (setq char (read-char stream nil nil))
                                            until (= count 3)
                                            finally 
                                              (unread-char char stream)
                                              (return (code-char sum)))))
                          ((char= char #\x)
                           (setq char (loop with sum = 0
                                            as char = (read-char stream)
                                            while (or (char<= #\0 char #\9)
                                                      (char<= #\A char #\F)
                                                      (char<= #\a char #\f))
                                            do (setq sum (+ (* sum 16) (digit-char-p char 16)))
                                            finally 
                                              (unread-char char stream)
                                              (return (code-char sum)))))))))
        collect char into chars))

(dolist (char '(#\_))
  (set-syntax-from-char char #\A *c-readtable*))

(dolist (op '( (c::! c::!=)
               ((\" c-read-string nil))
               (|#| |##|)            ; # and ## are pre-processor operators
               (c::% c::%=)
               (c::& c::&= c::&&)
               ((\' c-read-string t))
               (c::\()
               (c::\))
               (c::* c::*=)
               (c::+ c::+= c::++)
               (c::- c::-= c::-- c::->)
               (c::\,)
               (c::|.| c::|.*| c::|..| c::|...|)                 ; .01 will fail to parse as 0.01
               (c::/ c::/= (// c-read-line-comment) (/* c-read-block-comment))
               (c::\: c::\:\:)
               (c::\;)
               (c::< c::<= c::<< c::<<=)
               (c::= c::==)
               (c::> c::>= c::>> c::>>=)
               (c::?)
               (c::[)
               (c::\\)
               (c::])
               (c::^ c::^=)
               (c::{)
               (c::\| c::\|= c::\|\|)
               (c::})
               (c::~)
               ;; C++ doesn't define any meaning for these, treat them as operators
               (c::\$)
               (c::\@)
               (c::\`)
               ))
  (set-macro-character (char (string (if (atom (car op)) (car op) (caar op))) 0)
                       (operator-macro op)
                       nil              ;token-terminating
                       *c-readtable*))

(dolist (char '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
  (set-macro-character char 'read-c-number t *c-readtable*))


(defvar *backslash-symbol* 'c::|\\|)

(defvar *pending-tokens* ())

(defun unread-token (token)
  (push token *pending-tokens*)
  token)

(defun next-token (stream)
  (if *pending-tokens*
    (pop *pending-tokens*)
    (do* ((tok (read-preserving-whitespace stream nil :eof)
                       (read-preserving-whitespace stream nil :eof)))
                 ((or (not (eq tok *backslash-symbol*))
                      (not (eq (peek-char nil stream nil nil) #\Newline)))
                  tok)     
	     ;; Consume the #\newline that followed #\\.  Yecch.
	     (read-char stream nil nil))))
              
(defun string-to-tokens (string)
  (with-input-from-string (stream string)
    (let* ((*package* (find-package "C"))
           (*readtable* *c-readtable*)
           (tokens ()))
      (loop
          (let* ((token (next-token stream)))
            (when (eq token :eof)
              (return (nreverse tokens)))
            (push token tokens))))))


(defun identifierp (token)
  (and (symbolp token)
       (let ((char (char (symbol-name token) 0)))
	 (or (alpha-char-p char) (char= char #\_)))))


(defun evaluate-type-name (x)
  (let* ((name (car x)))
    (if (and (atom name) nil (null (cdr x)))
      name)))
      

(defun find-constant (x constants)
  (when (symbolp x)
    (cdr (assoc (string x) constants :test #'string=))))

(defun find-user-or-primitive-type (x)
  x
  nil)

(defun macro-definition (id table)
  (gethash (string id) table))

(defun expand-c-macro (name parameters arguments body stream macros-not-to-expand macro-table)
  (let ((expansion nil))
    (unless (= (length arguments) (length parameters))
      (c-parse-error stream "Expected ~D argument~:P to macro ~A but got ~D argument~:P."
			 (length parameters) name (length arguments)))
    (loop while body
      as token = (pop body)
      as next = (first body)
      as argno = (position token parameters) do
      (cond ((and argno (eq next *sharp-sharp-symbol*)) ; parameter ## token/parameter
	     (pop body)
	     (setq next (pop body))
	     (let ((next-argno (position next parameters)))
	       (push (intern (concatenate 'string (c-stringize-token-list (nth argno arguments))
					  (if next-argno
					    (c-stringize-token-list (nth next-argno arguments))
					    (c-stringize-token next))))
		     expansion)))
	    (argno			; normal parameter substitution
	     (setq expansion (nreconc (expand-c-macros-in-token-list (nth argno arguments)
                                                                     stream macros-not-to-expand
                                                                     macro-table)
				      expansion)))
	    ((and (eq token *sharp-sharp-symbol*) ; token ## parameter
		  (setq argno (position next parameters)))
	     (pop body)
	     (push (intern (concatenate 'string (c-stringize-token (pop expansion))
					(c-stringize-token-list (nth argno arguments))))
		   expansion))
	    ((and (eq token *sharp-symbol*)	; # parameter
		  (setq argno (position next parameters)))
	     (pop body)
	     (push (c-stringize-token-list (nth argno arguments)) expansion))
	    (t (push token expansion))))
    (expand-c-macros-in-token-list (nreverse expansion) stream
                                   (adjoin name macros-not-to-expand)
                                   macro-table)))

(defun expand-c-macros-in-token-list (tokens stream macros-not-to-expand macro-table)
  (loop
      while tokens
    as token = (pop tokens)
    as macro = (and (symbolp token)
                    (not (member token macros-not-to-expand))
                    (macro-definition token macro-table))
    if macro
    nconc (if (eq (first macro) :none) 
            (expand-c-macros-in-token-list (second macro) stream 
                                           (adjoin token macros-not-to-expand) macro-table)
            (expand-c-macro token (first macro)
                            (let ((open (pop tokens)))
                              (unless (eq open *lparen-symbol*)
                                (c-parse-error
                                 stream
                                 "~A where open parenthesis expected after macro name ~A"
                                 open token))
                              (loop with done = nil
                                    collect
                                    (loop as token = (if tokens (pop tokens)
                                                       (c-parse-error stream
                                                                      "Unexpected impossible EOF"))
                                          with level = 0
                                          do (cond ((eq token *lparen-symbol*) (incf level))
                                                   ((eq token *rparen-symbol*)
                                                    (if (plusp level) (decf level) (setq done t))))
                                                  until (or done (and (zerop level)
                                                                      (eq token *comma-symbol*)))
                                                  collect token)
                                    until done))
                            (second macro) stream macros-not-to-expand macro-table))
    else collect token))

(defun parse-c-expression (token-list &key  constants additional-constants 
                                          expand-macros)
  (labels ((next ()
             (unless token-list
               (fail "Unterminated expression or unbalanced parentheses"))
             (pop token-list))
           (peek ()
             (car token-list))
           (unread (token)
             (push token token-list))
           (collect-parenthesized ()
             (loop with level = 0
                   as token = (next)
                   until (and (eq token *rparen-symbol*) (= level 0))
                   collect token
                   do (case token
                        (#.*lparen-symbol* (incf level))
                        (#.*rparen-symbol* (decf level)))))
           (fail (format-string &rest format-arguments)
             (apply #'c-parse-error nil format-string format-arguments))
           (parse-expression ()
             (parse-assignment))
           (parse-assignment ()
             (let ((left (parse-conditional)))
               (if (eq (peek) 'c::|=|)
                 (let ((right (progn (next) (parse-assignment))))
                   (list 'setf left right))
                 left)))
           (parse-conditional ()
             (let ((left (parse-logical-or)))
               (if (eq (peek) 'c::|?|)
                 (let ((then (progn (next) (parse-expression)))
                       (else (if (eq (peek) '|:|)
                               (progn (next) (parse-conditional))
                               (fail "~A where : was expected" (peek)))))
                   (list 'if left then else))
                 left)))
           (parse-logical-or ()
             (let ((left (parse-logical-and)))
               (loop while (eq (peek) 'c::|\|\||)
                     do (setq left (list (next) left (parse-logical-and))))
               left))
           (parse-logical-and ()
             (let ((left (parse-bitwise-ior)))
               (loop while (eq (peek) 'c::|&&|)
                     do (setq left (list (next) left (parse-bitwise-ior))))
               left))
           (parse-bitwise-ior ()
             (let ((left (parse-bitwise-xor)))
               (loop while (eq (peek) 'c::|\||)
                     do (setq left (list (next) left (parse-bitwise-xor))))
               left))
           (parse-bitwise-xor ()
             (let ((left (parse-bitwise-and)))
               (loop while (eq (peek) 'c::|\^|)
                     do (setq left (list (next) left (parse-bitwise-and))))
               left))
           (parse-bitwise-and ()
             (let ((left (parse-equality)))
               (loop while (eq (peek) 'c::|&|)
                     do (setq left (list (next) left (parse-equality))))
               left))
           (parse-equality ()
             (let ((left (parse-relational)))
               (loop while (member (peek) '(c::|==| c::|!=|))
                     do (setq left (list (next) left (parse-relational))))
               left))
           (parse-relational ()
             (let ((left (parse-shift)))
               (loop while (member (peek) '(c::|<| c::|>| c::|<=| c::|>=|))
                     do (setq left (list (next) left (parse-shift))))
               left))
           (parse-shift ()
             (let ((left (parse-additive)))
               (loop while (member (peek) '(c::|<<| c::|>>|))
                     do (setq left (list (next) left (parse-additive))))
               left))
           (parse-additive ()
             (let ((left (parse-multiplicative)))
               (loop while (member (peek) '(c::|+| c::|-|))
                     do (setq left (list (next) left (parse-multiplicative))))
               left))
           (parse-multiplicative ()
             (let ((left (parse-pointer-to-member)))
               (loop while (member (peek) '(c::|*| c::|/| c::|%|))
                     do (setq left (list (next) left (parse-pointer-to-member))))
               left))
           (parse-pointer-to-member ()
             (let ((left (parse-unary)))
               (loop while (member (peek) '(c::|.*| c::|->*|))
                     do (setq left (list (next) left (parse-unary))))
               left))
           (parse-unary ()              ; subsumes parse-cast, thus accepting some invalid programs
             (let ((token (next)))      ; --- doesn't support new and delete yet
               (cond ((member token '(c::|+| c::|-| c::|!| c::|~| c::|++| c::|--|))
                      ;;--- doesn't yet have special support for calling destructors...
                      (list token (parse-unary)))
                     ((eq token 'c::|*|)
                      (list 'c::indirect (parse-unary)))
                     ((eq token 'c::|&|)
                      (list 'c::address-of (parse-unary)))
                     ((eq token 'c::|sizeof|)
                      (unless (eq (peek) *lparen-symbol*)          ; Require open paren, maybe it's really optional
                        (fail "~A where ( was expected after sizeof" (peek)))
                      (next)            ; Swallow open parenthesis
                      `(c::size-of (c::resolve-type ,(loop as token = (next)
                                                           until (eq token *rparen-symbol*)
                                                           collect token))))
                     (t (parse-postfix token)))))
           (parse-postfix (token)
             (loop with left = (parse-primary token)
                   as right =  (peek) do
                   (setq left
                         (cond ((eq right *leftbracket-symbol*)
                                (next)          ; swallow [
                                (let ((subscript (parse-expression))
                                      (delimiter (next)))
                                  (unless (eq delimiter *rightbracket-symbol*)
                                  (fail "~A where ] expected after subscript" delimiter))
                                  `(c::aref ,left ,subscript)))
                               ((eq right *lparen-symbol*)
                                (next)          ; swallow open parenthesis
                                (let ((macro (and expand-macros
                                                  (identifierp left)
                                                  (macro-definition left expand-macros))))
                                  (cond ((and macro (not (eq (first macro) ':none)))
                                         ;; Function-like macro - constant-like was alraedy handled
                                         (let ((more-tokens 
                                                (expand-c-macro left (first macro)
                                                                (collect-macro-arguments)
                                                                (second macro) nil '()
                                                                expand-macros)))
                                           (setq token-list (append more-tokens token-list))
                                           (parse-expression)))
                                        ((valid-type-name? (list left))
                                         ;; This is an explicit type conversion
                                         `(c::cast ,(evaluate-type-name (list left))
                                           ,@(parse-argument-list)))
                                        (t `(c::call ,left ,@(parse-argument-list))))))
                               ((memq right '(c::|.| c::|->|))
                                (next)          ; swallow operator
                                `(,right ,left ,(parse-primary (next))))  ; parse-name, really
                               ((eq right 'c::|++|)
                                (next)          ; swallow operator
                                `(c::postfix++ ,left))
                               ((eq right 'c::|--|)
                                (next)          ; swallow operator
                                `(c::postfix-- ,left))
                               (t (return left))))))
           (parse-primary (token)
               (cond ((identifierp token)
                        ;; nonqualified name
                        (let ((value (find-constant token constants)))
                          (cond (value 
                                 (setq value (list value) token-list `(,@value #.*rparen-symbol* ,@token-list))
                                 (parse-parenthesized))
                                ((setq value (assoc token additional-constants))
                                 (cdr value))
                                ((and expand-macros
                                      (setq value (macro-definition-of-token token))
                                      (eq (first value) ':none))
                                 (setq token-list (append (expand-c-macros-in-token-list 
                                                           (second value) nil (list token) expand-macros)
                                                          token-list ))
                                 (parse-primary (next)))
                                (t token))))
                     ((eq token *lparen-symbol*)
                      (let* ((save-token-list token-list)
                            (type-name (collect-parenthesized))
                            (type (valid-type-name? type-name)))
                        (cond (type
                               ;; This is a cast
                               ;; Doing cast here is easier but accepts some invalid programs
                               (progn
                                 `(c::cast (,type) ,(parse-unary))))
                              (t
                               ;; These are ordinary grouping parentheses
                               (setq token-list save-token-list)
                               (parse-parenthesized)))))
                     ((eq token 'c::|{|)
                      (cons 'c::curly-bracketed-list
                            (loop as token = (next)
                                  until (eq token 'c::|}|)
                                  do (unread token)
                                  collect (parse-expression)
                                  do (let ((delimiter (peek)))
                                       (case delimiter
                                         (c::|,| (next))
                                         (c::|}| )
                                         (otherwise 
                                          (fail "~A where , or } was expected" delimiter)))))))
                     ((numberp token) token)
                     ((stringp token) token)
                     ((eq token 'c::|::|)
                      (fail "Unary :: is not supported yet"))
                     (t (fail "~A is unrecognized syntax in an expression" token))))
           (parse-parenthesized ()
             (prog1 (parse-expression)
               (let ((close (next)))
                 (unless (eq close *rparen-symbol*)
                   (fail "~A where ) was expected" close)))))
           (parse-argument-list ()
             (if (eq (peek) *rparen-symbol*)
               (progn (next) '())
               (loop as arg = (parse-expression)
                     as delimiter = (next)
                     collect arg
                     do (unless (or (eq delimiter 'c::|,|) (eq delimiter *rparen-symbol*))
                          (fail "~A where , or ) expected in function arguments"
                                delimiter))
                     while (eq delimiter 'c::|,|))))
           (collect-macro-arguments ()
             (loop with done = nil with first = t
                   collect (loop as token = (next) with level = 0
                                 do (cond ((eq token *lparen-symbol*) (incf level))
                                          ((eq token *rparen-symbol*) 
                                           (when first   ; () has to be treated as a special case
                                             (return-from collect-macro-arguments '()))
                                           (if (plusp level) (decf level) (setq done t))))
                                    (setq first nil)
                                 until (or done (and (zerop level) (eq token 'c::|,|)))
                                 collect token)
                   until done))
           
           ;;--- The following type-name routines don't support the full C++ syntax
           ;;--- Maybe we will add ::, arrays, functions, and God knows what later
           (valid-type-name? (token-list &optional tailp)
             (let* ((type (ignore-errors (parse-c-ffi-type token-list))))
               tailp
               (return-from valid-type-name?
                 (if (and type (not (eq type *the-ffi-void-type*)))
                   type)))
                                              
             ;; At least one type-specifier followed by an optional abstract-declarator
             ;; For now the type-specifier cannot contain :: and the only
             ;; abstract-declarators we accept are stars (not functions, arrays)
             (cond ((null token-list) tailp)
                   ((member (car token-list) '(c::|long| c::|short| c::|signed| c::|unsigned|))
                    (valid-type-name? (cdr token-list) t))
                   ((and (identifierp (car token-list))
                         (find-user-or-primitive-type (car token-list)))
                    (valid-type-name? (cdr token-list) t))
                   ;((eq (car token-list) '|::|) (valid-type-name? (cdr token-list)))
                   ((and tailp (eq (car token-list) 'c::|*|))
                    (valid-type-name? (cdr token-list) t))
                   (t nil))))
    (prog1 (parse-expression)
      (when token-list
        (fail "~{~A ~} left over after expression" token-list)))))

(defun c-parse-error (stream format &rest args)
  (declare (ignore stream))
  (apply #'error format args))

(defun macro-definition-of-token (x)
  (declare (ignore x)))

(defun c-stringize-token-list (tokens)
  (apply #'concatenate 'string (mapcar #'c-stringize-token tokens)))

(defun c-stringize-token (token)
  (etypecase token
    (symbol (string token))
    (string token)
    (number (princ-to-string token))))

(defun install-new-db-files (ftd d)
  (let* ((dir (merge-pathnames (interface-dir-subdir d)
			       (ftd-interface-db-directory ftd))))
    (flet ((rename-and-reopen (was-open path newpath)
	     (let* ((path (merge-pathnames path dir))
		    (newpath (merge-pathnames newpath dir)))
	       (when was-open
		 (cdb-close was-open))
	       (when (probe-file path)
		 (rename-file path
			      (concatenate 'string (namestring (truename path)) "-BAK")
			      :if-exists :supersede))
	       (rename-file newpath path)
	       (when was-open
		 (cdb-open path)))))
      (without-interrupts
       (setf (interface-dir-constants-interface-db-file d)
	     (rename-and-reopen
	      (interface-dir-constants-interface-db-file d)
	      "constants.cdb"
	      "new-constants.cdb"))
       (setf (interface-dir-functions-interface-db-file d)
	     (rename-and-reopen
	      (interface-dir-functions-interface-db-file d)
	      "functions.cdb"
	      "new-functions.cdb"))
       (setf (interface-dir-records-interface-db-file d)
	     (rename-and-reopen
	      (interface-dir-records-interface-db-file d)
	      "records.cdb"
	      "new-records.cdb"))
       (setf (interface-dir-types-interface-db-file d)
	     (rename-and-reopen
	      (interface-dir-types-interface-db-file d)
	      "types.cdb"
	      "new-types.cdb"))
       (setf (interface-dir-vars-interface-db-file d)
	     (rename-and-reopen
	      (interface-dir-vars-interface-db-file d)
	      "vars.cdb"
	      "new-vars.cdb"))
       (setf (interface-dir-objc-classes-interface-db-file d)
	     (rename-and-reopen
	      (interface-dir-objc-classes-interface-db-file d)
	      "objc-classes.cdb"
	      "new-objc-classes.cdb"))
       (setf (interface-dir-objc-methods-interface-db-file d)
	     (rename-and-reopen
	      (interface-dir-objc-methods-interface-db-file d)
	      "objc-methods.cdb"
	      "new-objc-methods.cdb")))))
  t)


