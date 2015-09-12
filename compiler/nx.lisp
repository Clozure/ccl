;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2009 Clozure Associates
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

(in-package "CCL")

(eval-when (:compile-toplevel)
  (require 'nxenv)
  (require 'numbers)
  (require 'sequences)
  (require 'optimizers))

(eval-when (:load-toplevel :execute :compile-toplevel)
  (require 'numbers) ; just calls 'logcount' and 'integer-length'
  (require 'sort)    ; just calls '%sort-list-no-keys'
  (require 'hash))


(%include "ccl:compiler;nx-basic.lisp")

(eval-when (:load-toplevel :execute)
  (require "DEFSTRUCT"))

(defparameter *nx-start* (cons nil nil))





(defun %def-info.lambda (def-info)
  (and def-info
       (let ((data (svref def-info 3)))
         (or (and (consp (car data)) (eq (caar data) 'lambda) (car data))
             (and (eq (car data) 'lambda) data)))))



(defun %def-info.environment (def-info)
  (and def-info
       (let* ((data (svref def-info 3)))
         (and (consp (car data))
              (eq (caar data) 'lambda)
              (cdr data)))))

(defun def-info.function-type (def-info)
  (if (null def-info)
    nil ;; ftype only, for the purposes here, is same as nothing.
    (let ((data (svref def-info 3)))
      (if (and (consp (car data)) (eq 'lambda (caar data)))
        'defun
        (ecase (car data)
          ((nil lambda) 'defun)
          (:methods 'defgeneric)
          (macro 'defmacro)
          (ftype nil)
          (type nil))))))

;;; Return T if and only if the lexical environment contains variable
;;; or function bindings (other than macros or symbol-macros).
(defun binding-free-environment-p (env)
  (do* ((env env (lexenv.parent-env env)))
       ((or (null env) (typep env 'definition-environment)) t)
    (let* ((vars (lexenv.variables env)))
      (unless (or (atom vars)
                  (dolist (var vars t)
                    (let* ((ea (var-ea var)))
                      (unless (and (consp ea)
                                 (eq (car ea) :symbol-macro))
                        (return)))))
        (return)))
    (unless (dolist (f (lexenv.functions env) t)
              (unless (and (consp f)
                           (consp (cdr f))
                           (eq 'macro (cadr f)))
                (return))))))
          

(defun retain-lambda-expression (name lambda-expression env)
  (if (and (let* ((lambda-list (cadr lambda-expression)))
             (and (not (memq '&lap lambda-list))
                  (not (memq '&method lambda-list))
                  (not (memq '&lexpr lambda-list))))
           (nx-declared-inline-p name env)
           (not (gethash name *nx1-alphatizers*))
           (binding-free-environment-p env))
    (cons lambda-expression env)))

(defvar *host-backend*)
(defvar *target-backend*)

(defun find-backend (name)
  (find name *known-backends* :key #'backend-name))

(eval-when (:load-toplevel :execute :compile-toplevel)
  (require "DLL-NODE")
  #+ppc-target
  (require "PPC32-ARCH")
  (require "VREG")
  #+ppc-target
  (require "PPC-ASM")
  (require "VINSN")
  (require "REG")
  (require "SUBPRIMS")
  #+ppc-target
  (require "PPC-LAP")
)
(%include "ccl:compiler;nx0.lisp")
(%include "ccl:compiler;nx1.lisp")

; put this in nx-basic too
;(defvar *lisp-compiler-version* 666 "I lost count.")

; At some point, COMPILE refused to compile things that were defined
; in a non-null lexical environment (or so I remember.)   That seems
; to have been broken when the change of 10/11/93 was made.
; It makes no sense to talk about compiling something that was defined
; in a lexical environment in which there are symbol or function bindings
; present;  I'd thought that the old code checked for this, though it
; may well have botched it.
(defun compile (spec &optional def &aux (macro-p nil))
  "Coerce DEFINITION (by default, the function whose name is NAME)
  to a compiled function, returning (VALUES THING WARNINGS-P FAILURE-P),
  where if NAME is NIL, THING is the result of compilation, and
  otherwise THING is NAME. When NAME is not NIL, the compiled function
  is also set into (MACRO-FUNCTION NAME) if NAME names a macro, or into
  (FDEFINITION NAME) otherwise."
  (unless def
    (setq def (fboundp spec))
    (when (and (symbolp spec) (not (lfunp def)))
      (setq def (setq macro-p (macro-function spec)))))
  #+have-interpreted-functions
  (when (typep def 'interpreted-function)
    (let ((lambda (function-lambda-expression def)))
      (when lambda (setq def lambda))))
  (unless def
    (nx-error "Can't find compilable definition for ~S." spec))
  (multiple-value-bind (lfun warnings)
                       (if (functionp def)
                         def
                         (compile-named-function def
                                                 :name spec
                                                 :keep-lambda *save-definitions*
                                                 :keep-symbols *save-local-symbols*))
    (let ((harsh nil) (some nil) (init t))
      (dolist (w warnings)
        (multiple-value-setq (harsh some) (signal-compiler-warning w init nil harsh some))
        (setq init nil))
      (values
       (if spec
         (progn
           (if macro-p
             (setf (macro-function spec) lfun)
             (setf (fdefinition spec) lfun))
           spec)
         lfun)
       some
       harsh))))

(defparameter *default-compiler-policy* (new-compiler-policy))

(defun current-compiler-policy () *default-compiler-policy*)

(defun set-current-compiler-policy (&optional new-policy)
  (setq *default-compiler-policy* 
        (if new-policy (require-type new-policy 'compiler-policy) (new-compiler-policy))))

#+ppc-target
(defun xcompile-lambda (target def)
  (let* ((*ppc2-debug-mask* (ash 1 ppc2-debug-vinsns-bit))
         (backend (find-backend target))
         (*target-ftd* (if backend
                         (backend-target-foreign-type-data backend)
                         *target-ftd*))
         (*target-backend* (or backend *target-backend*)))
    (multiple-value-bind (xlfun warnings)
        (compile-named-function def :target target)
      (signal-or-defer-warnings warnings nil)
      (ppc-xdisassemble xlfun :target target)
      xlfun)))
  
(defun compile-user-function (def name &optional env)
  (multiple-value-bind (lfun warnings)
                       (compile-named-function def
                                               :name name
                                               :env env
                                               :keep-lambda *save-definitions*
                                               :keep-symbols *save-local-symbols*)
    (signal-or-defer-warnings warnings env)
    lfun))

(defun signal-or-defer-warnings (warnings env)
  (let* ((defenv (definition-environment env))
         (init t)
         (defer (and defenv (cdr (defenv.type defenv)) *outstanding-deferred-warnings*)))
    (dolist (w warnings)
      (if (and defer (typep w 'undefined-reference))
        (push w (deferred-warnings.warnings defer))
        (progn
          (signal-compiler-warning w init nil nil nil)
          (setq init nil))))))

(defparameter *load-time-eval-token* nil)


(defparameter *nx-discard-xref-info-hook* nil)

(defparameter *nx-in-frontend* nil)
(defparameter *nx-rewrite-acode* t)



(defparameter *current-function-name* nil)
(defparameter *nx-current-function* nil)

(defun compile-named-function (def &rest args
                                &key name env policy load-time-eval-token target
                                function-note keep-lambda keep-symbols source-notes
                                (record-pc-mapping *record-pc-mapping*)
                                (force-legacy-backend nil)
                                (compile-code-coverage *compile-code-coverage*))
  ;; SOURCE-NOTES, if not nil, is a hash table mapping source forms to locations,
  ;;   is used to produce and attach a pc/source map to the lfun, also to attach
  ;;   source locations and pc/source maps to inner lfuns.
  ;; FUNCTION-NOTE, if not nil, is a note to attach to the function as the lfun
  ;;   source location in preference to whatever the source-notes table assigns to it.
  (handler-case
      (progn
        (when (and name *nx-discard-xref-info-hook*)
          (funcall *nx-discard-xref-info-hook* name))
        (setq 
         def
         (let* ((*load-time-eval-token* load-time-eval-token)
                (*current-function-name* (or name "an anonymous function"))
                (*backend-use-linear-scan*  (target-arch-case (:x8664 (unless force-legacy-backend *backend-use-linear-scan*)) (t nil)))
                (*force-legacy-backend* force-legacy-backend)
                (*nx-source-note-map* source-notes)
                (*nx-current-note* function-note)
                (*record-pc-mapping* (and source-notes record-pc-mapping))
                (*compile-code-coverage* (and source-notes compile-code-coverage))
                (*nx-current-code-note* (and *compile-code-coverage*
                                             (make-code-note :form def :source-note function-note)))
                (env (new-lexical-environment env)))
           (setf (lexenv.variables env) 'barrier)
           (let* ((*target-backend* (or (if target (find-backend target)) *host-backend*))
                  (*nx-target-fixnum-type*
                   (target-word-size-case
                    (32 *nx-32-bit-fixnum-type*)
                    (64 *nx-64-bit-fixnum-type*)))
                  (*nx-target-half-fixnum-type*
                   (target-word-size-case
                    (32 '(signed-byte 29))
                    (64 '(signed-byte 60))))
                  (*nx-target-natural-type*
                   (target-word-size-case
                    (32 *nx-32-bit-natural-type*)
                    (64 *nx-64-bit-natural-type*)))
                  (*nx-in-frontend* t)
                  (afunc (nx1-compile-lambda 
                          name 
                          def
                          (make-afunc) 
                          nil 
                          env 
                          (or policy *default-compiler-policy*)
                          *load-time-eval-token*)))
             (setq *nx-in-frontend* nil)
             (if (afunc-lfun afunc)
               afunc
               (progn
                 (when (and *nx-rewrite-acode*
                            (afunc-acode afunc))
                   (let* ((*nx-current-function* afunc))
                     (rewrite-acode-form (afunc-acode afunc) t)))
                 (funcall (backend-p2-compile *target-backend*)
                          afunc
                          ;; will also bind *nx-lexical-environment*
                          (if keep-lambda (if (lambda-expression-p keep-lambda) keep-lambda def))
                          keep-symbols))))))
        (values (afunc-lfun def) (afunc-warnings def)))
    (linear-scan-bailout
     ()
     (apply #'compile-named-function def :force-legacy-backend T args))))
                         

(defparameter *compiler-whining-conditions*
  '((:undefined-function . undefined-function-reference)
    (:undefined-type . undefined-type-reference)
    (:deferred-mismatch . undefined-keyword-reference)
    (:invalid-type . invalid-type-warning)
    (:global-mismatch . invalid-arguments-global)
    (:lexical-mismatch . invalid-arguments)
    (:environment-mismatch . invalid-arguments)
    (:ftype-mismatch . invalid-arguments)
    (:unknown-type-in-declaration . style-warning)
    (:ignore . style-warning)
    (:result-ignored . style-warning)
    (:lambda . style-warning)
    (:format-error . style-warning)
    (:unused . style-warning)
    (:type-conflict . style-warning)
    (:special-ignore . style-warning)))



(defun compiler-bug (format &rest args)
  (error (make-condition 'compiler-bug
                         :format-control format
                         :format-arguments args)))


(defparameter *nx-end* (cons nil nil))
(provide 'nx)

