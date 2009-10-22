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

(defun compile-named-function (def &key name env policy load-time-eval-token target
                                function-note keep-lambda keep-symbols source-notes
                                (record-pc-mapping *record-pc-mapping*)
                                (compile-code-coverage *compile-code-coverage*))
  ;; SOURCE-NOTES, if not nil, is a hash table mapping source forms to locations,
  ;;   is used to produce and attach a pc/source map to the lfun, also to attach
  ;;   source locations and pc/source maps to inner lfuns.
  ;; FUNCTION-NOTE, if not nil, is a note to attach to the function as the lfun
  ;;   source location in preference to whatever the source-notes table assigns to it.
  (when (and name *nx-discard-xref-info-hook*)
    (funcall *nx-discard-xref-info-hook* name))
  (setq 
   def
   (let* ((*load-time-eval-token* load-time-eval-token)
	  (*nx-source-note-map* source-notes)
          (*nx-current-note* function-note)
          (*record-pc-mapping* (and source-notes record-pc-mapping))
          (*compile-code-coverage* (and source-notes compile-code-coverage))
	  (*nx-acode-note-map* (and (or *record-pc-mapping* *compile-code-coverage*)
                                    (make-hash-table :test #'eq :shared nil)))
          (*nx-current-code-note* (and *compile-code-coverage*
                                       (make-code-note :form def :source-note function-note)))
          (env (new-lexical-environment env)))
     (setf (lexenv.variables env) 'barrier)
     (let* ((*target-backend* (or (if target (find-backend target)) *host-backend*))
            (afunc (nx1-compile-lambda 
                    name 
                    def
                    (make-afunc) 
                    nil 
                    env 
                    (or policy *default-compiler-policy*)
                    *load-time-eval-token*)))
       (if (afunc-lfun afunc)
         afunc
         (funcall (backend-p2-compile *target-backend*)
                  afunc
                  ;; will also bind *nx-lexical-environment*
                  (if keep-lambda (if (lambda-expression-p keep-lambda) keep-lambda def))
                  keep-symbols)))))
  (values (afunc-lfun def) (afunc-warnings def)))

(defparameter *compiler-whining-conditions*
  '((:undefined-function . undefined-function-reference)
    (:undefined-type . undefined-type-reference)
    (:deferred-mismatch . undefined-keyword-reference)
    (:invalid-type . invalid-type-warning)
    (:global-mismatch . invalid-arguments-global)
    (:lexical-mismatch . invalid-arguments)
    (:environment-mismatch . invalid-arguments)
    (:ftype-mismatch . invalid-arguments)
    (:ignore . style-warning)
    (:result-ignored . style-warning)
    (:lambda . style-warning)
    (:format-error . style-warning)
    (:unused . style-warning)))



(defun compiler-bug (format &rest args)
  (error (make-condition 'compiler-bug
                         :format-control format
                         :format-arguments args)))


(defparameter *nx-end* (cons nil nil))
(provide 'nx)

