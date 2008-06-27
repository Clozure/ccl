;;;-*-Mode: LISP; Package: CCL -*-
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

;;; Record pseudo-arglist info for special operators.
(record-arglist 'catch "tag &body body")
(record-arglist 'progn "&BODY BODY")
(record-arglist 'function "NAME-OR-LAMBDA-EXPRESSION")
(record-arglist 'go "TAG")
(record-arglist 'symbol-macrolet "(&REST BINDINGS) &BODY BODY")
(record-arglist 'locally "DECLARATION* &BODY BODY")
(record-arglist 'setq "[SYMBOL VALUE]*")
(record-arglist 'tagbody "&REST TAGS-OR-FORMS")
(record-arglist 'return-from "BLOCK VALUES")
(record-arglist 'quote '(form))
(record-arglist 'macrolet "(&REST BINDINGS) &BODY BODY")
(record-arglist 'the '(type-specifier form))
(record-arglist 'eval-when "(&REST SITUATIONS) &BODY BODY")
(record-arglist 'let* "(&REST BINDINGS) &BODY BODY")
(record-arglist 'let "(&REST BINDINGS) &BODY BODY")
(record-arglist 'load-time-value '(form))
(record-arglist 'throw '(tag value))
(record-arglist 'unwind-protect "PROTECTED-FORM &BODY CLEANUP-FORMS")
(record-arglist 'flet "(&REST BINDINGS) &BODY BODY")
(record-arglist 'multiple-value-call '(function &rest values-producing-forms))
(record-arglist 'block "NAME &BODY BODY")
(record-arglist 'labels "(&REST BINDINGS) &BODY BODY")
(record-arglist 'multiple-value-prog1 "VALUES-PRODUCING-FORM &BODY FORMS-FOR-EFFECT")
(record-arglist 'if '(test true &optional false))
(record-arglist 'progv "(&REST VARS) (&REST VALUES) &BODY BODY")
(record-arglist 'nfunction '(function-name lambda-expression))


; Returns two values: the arglist & it's functions binding.
; If the second arg is NIL, there was no function binding.
(defun arglist (sym &optional include-bindings)
  (%arglist sym include-bindings))

(defun arglist-string (sym &optional include-bindings)
  (multiple-value-bind (res type)
                       (%arglist-internal sym include-bindings)
    (values
     (if (stringp res)
       res
       (and res (princ-to-string res)))
     type)))

(defun set-arglist (sym arglist)
  (let ((real-sym (arglist-sym-and-def sym)))
    (when (or real-sym (null sym))
      (if (eq arglist t)
        (remhash real-sym %lambda-lists%)
        (setf (gethash real-sym %lambda-lists%) arglist)))))

(defsetf arglist set-arglist)

; Same as ARGLIST, but has the option of using TEMP-CONS instead of CONS
; to cons up the list.
(defun %arglist (sym &optional include-bindings)
  (multiple-value-bind (res type)
                       (%arglist-internal
                        sym include-bindings)
    (when (stringp res)
      (with-input-from-string (stream res)
        (setq res nil)
        (let ((eof (list nil))
              val errorp)
          (declare (dynamic-extent eof))
          (loop
            (multiple-value-setq (val errorp)
              (ignore-errors (values (read stream nil eof))))
            (when errorp
              (push '&rest res)
              (push ':unparseable res)
              (return))
            (when (eq val eof)
              (return))
            (push val res))
          (setq res
                (if (and (null (cdr res)) (listp (car res)))
                  (car res)
                  (nreverse res))))))
    (values res type)))

(defun %arglist-internal (sym include-bindings 
                              &aux def type)
  (multiple-value-setq (sym def) (arglist-sym-and-def sym))
  (if (generic-function-p def)
    (values (generic-function-lambda-list def) :declaration)
    (let ((ll (gethash sym %lambda-lists% *eof-value*))
        (macrop (and (symbolp sym) (eq (macro-function sym) def))))
    (flet ((strip (f) (if (stringp f) f (strip-bindings f include-bindings))))
      (declare (dynamic-extent #'strip))
      (cond ((neq ll *eof-value*) (values (strip ll) :declaration))
            ((consp def)
             ;; Presumably (lambda (... arglist) ...)
             (values (strip (cadr def)) :definition))
            ((neq (setq ll (getf (%lfun-info def) 'arglist *eof-value*)) *eof-value*)
             (values ll :definition))
            ((and (not macrop) (setq ll (uncompile-function def)))
             (values (strip (cadr ll)) (or type :definition)))
            ((lfunp def)
             (multiple-value-bind (arglist gotit) 
                                  (unless macrop (arglist-from-map def))
               (if gotit
                 (values arglist :analysis)
                 (cond  (macrop (values nil :unknown))
                       (t (values (arglist-from-compiled-def def) :analysis))))))
            (t (values nil nil)))))))

            

(defun strip-bindings (arglist include-bindings)
  (if include-bindings
    arglist
    (let ((res nil))
      (do ((args arglist (%cdr args)))
          ((not (consp args)) (nreconc res args))
        (let ((arg (car args)))
          (cond ((atom arg)
                 (push arg res))
                ((atom (car arg))
                 (push (car arg) res))
                (t (push (caar arg) res))))))))

(defun arglist-sym-and-def (sym &aux def)
  (cond ((functionp sym)
         (setq def sym
               sym (function-name def))
         (unless (and (symbolp sym) (eq def (fboundp sym)))
           (setq sym nil)))
        ((listp sym)
         (if (eq (car sym) 'setf)
           (setq sym (setf-function-name (cadr sym))
                 def (find-unencapsulated-definition (fboundp sym)))
           (setq sym nil def nil)))
        ((standard-method-p sym)
         (setq def (closure-function 
                    (find-unencapsulated-definition (%method-function sym)))))
        ((and (macro-function sym))
         (setq def (macro-function sym)))
        ((special-operator-p sym)
         nil)
        (t (setq def (find-unencapsulated-definition (fboundp sym)))))
  (values sym (if (standard-generic-function-p def) def (closure-function def))))

(defun arglist-from-map (lfun)
  (multiple-value-bind (nreq nopt restp nkeys allow-other-keys
                             optinit lexprp
                             ncells nclosed)
      (function-args lfun)
    (declare (ignore optinit))
    (if lexprp
      (setq restp t))
    (let ((map (car (function-symbol-map lfun))))
      (if map
        (let ((total (+ nreq nopt (if restp 1 0) (or nkeys 0)))
              (idx (- (length map) nclosed))
              (res nil))
          (if (%izerop total)
            (values nil t)
            (progn
              (dotimes (x nreq)
                (declare (fixnum x))
                (push (if (> idx 0) (elt map (decf idx)) (make-arg "ARG" x)) res))
              (when (neq nopt 0)
                (push '&optional res)
                (dotimes (x (the fixnum nopt))
                  (push (if (> idx 0) (elt map (decf idx)) (make-arg "OPT" x)) res)))

              (when restp
                (push (if lexprp '&lexpr '&rest) res)
                (push (if (> idx 0) (elt map (decf idx)) 'the-rest) res))                  (when nkeys
                (push '&key res)
                (let ((keyvect (lfun-keyvect lfun)))
                  (dotimes (i (length keyvect))
                    (push (elt keyvect i) res))))
              (when allow-other-keys
                (push '&allow-other-keys res))))
          (values (nreverse res) t))
        (values nil (zerop ncells))))))

(defun arg-names-from-map (lfun pc)
  (multiple-value-bind (nreq nopt restp nkeys allow-other-keys
                             optinit lexprp
                             ncells nclosed)
      (function-args lfun)
    (declare (ignore optinit ncells allow-other-keys))
    (collect ((req)
              (opt)
              (keys))
      (let* ((rest nil)
             (map (if (and pc (> pc target::arg-check-trap-pc-limit))
                    (car (function-symbol-map lfun)))))
        (if (and map pc)
          (let ((total (+ nreq nopt (if (or restp lexprp) 1 0) (or nkeys 0)))
                (idx (- (length map) nclosed)))
            (unless (zerop total)
              (progn
                (dotimes (x nreq)
                  (declare (fixnum x))
                  (req (if (> idx 0) (elt map (decf idx)) (make-arg "ARG" x))))
                (when (neq nopt 0)
                  (dotimes (x (the fixnum nopt))
                    (opt (if (> idx 0) (elt map (decf idx)) (make-arg "OPT" x)))))
                (when (or restp lexprp)
                  (setq rest (if (> idx 0) (elt map (decf idx)) 'the-rest)))                (when nkeys
                                                                                              (dotimes (i (the fixnum nkeys))
                    (keys (if (> idx 0) (elt map (decf idx)) (make-arg "KEY" i)))))))))
        (values (not (null map)) (req) (opt) rest (keys))))))
              
              


(defvar *req-arg-names*
  #(arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9))

(defvar *opt-arg-names*
  #(opt-0 opt-1 opt-2 opt-3 opt-4 opt-5 opt-6 opt-7 opt-8 opt-9))


(defun make-arg (prefix count)
  (cond ((and (string= prefix "ARG") (< count (length *req-arg-names*)))
         (svref *req-arg-names* count))
        ((and (string= prefix "OPT") (< count (length *opt-arg-names*)))
         (svref *opt-arg-names* count))
        (t (intern (format nil "~a-~d" prefix count) :CCL))))

(defun arglist-from-compiled-def (lfun &aux (res nil) argnames)
  (multiple-value-bind (nreq nopt restp nkeys allow-other-keys
                             optinit lexprp
                             ncells nclosed)
      (function-args lfun)
    (declare (ignore optinit ncells nclosed))
    (flet ((push-various-args (prefix count)
             (dotimes (i (the fixnum count))
               (push (make-arg prefix i) res))))
      (declare (dynamic-extent #'push-various-args))
      ;; Treat &LEXPR like &REST.
      (if lexprp (setq restp t lexprp nil))
      (cond ((and (eq 0 (+ nreq nopt (or nkeys 0))) (not restp))
             nil)
            (t 
             (if argnames
               (setq res (reverse (butlast argnames (- (length argnames) nreq))))
               (push-various-args "ARG" nreq))
             (when (> nopt 0)
               (push '&optional res)
               (if argnames
                 (setq res (append (reverse (subseq argnames nreq (+ nreq nopt))) res))
                 (push-various-args "OPT" nopt)))
             (when restp
               (push '&rest res)
               (if argnames
                 (push (nth (+ nreq nopt) argnames) res)
                 (push 'the-rest res)))
             (when nkeys
               (push '&key res)
               (let ((keyvect (lfun-keyvect lfun)))
                 (dotimes (i (length keyvect))
                   (push (elt keyvect i) res))))
             (when allow-other-keys
               (push '&allow-other-keys res))
             (nreverse res))))))

; End of arglist.lisp
