;;-*-Mode: LISP; Package: CCL -*-
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

;; :lib:nfcomp.lisp - New fasl compiler.

(eval-when (:compile-toplevel :load-toplevel :execute)
   (require 'level-2))

(require 'optimizers)
(require 'hash)

(eval-when (:compile-toplevel :execute)

(require 'backquote)
(require 'defstruct-macros)

(defmacro short-fixnum-p (fixnum)
  `(and (fixnump ,fixnum) (< (integer-length ,fixnum) 16)))

(require "FASLENV" "ccl:xdump;faslenv")

#+ppc32-target
(require "PPC32-ARCH")
#+ppc64-target
(require "PPC64-ARCH")
#+x8632-target
(require "X8632-ARCH")
#+x8664-target
(require "X8664-ARCH")
) ;eval-when (:compile-toplevel :execute)


;File compiler options.  Not all of these need to be exported/documented, but
;they should be in the product just in case we need them for patches....
(defvar *fasl-save-local-symbols* t)
(defvar *fasl-save-doc-strings*  t)
(defvar *fasl-save-definitions* nil)

(defvar *fasl-deferred-warnings* nil)
(defvar *fasl-non-style-warnings-signalled-p* nil)
(defvar *fasl-warnings-signalled-p* nil)

(defvar *compile-verbose* nil ; Might wind up getting called *compile-FILE-verbose*
  "The default for the :VERBOSE argument to COMPILE-FILE.")
(defvar *compile-file-pathname* nil
  "The defaulted pathname of the file currently being compiled, or NIL if not
  compiling.") ; pathname of src arg to COMPILE-FILE
(defvar *compile-file-truename* nil
  "The TRUENAME of the file currently being compiled, or NIL if not
  compiling.") ; truename ...
(defvar *fasl-target* (backend-name *host-backend*))
(defvar *fasl-backend* *host-backend*)
(defvar *fasl-host-big-endian*
  (arch::target-big-endian (backend-target-arch *host-backend*)))
(defvar *fasl-target-big-endian* *fasl-host-big-endian*)
(defvar *fcomp-external-format* :default)

(defvar *fasl-break-on-program-errors* :defer
  "Controls what happens when the compiler detects PROGRAM-ERROR's during file compilation.

  If T, the compiler signals an error immediately when it detects the program-error.

  If :DEFER, program errors are reported as compiler warnings, and in addition, an error
    is signalled at the end of file compilation.  This allows all warnings for the file
    to be reported, but prevents the creation of a fasl file.

  If NIL, program errors are treated the same as any other error condition detected by
   the compiler, i.e. they are reported as compiler warnings and do not cause any
   error to be signalled at compile time.")
  

(defvar *compile-print* nil ; Might wind up getting called *compile-FILE-print*
  "The default for the :PRINT argument to COMPILE-FILE.")

;Note: errors need to rebind this to NIL if they do any reading without
; unwinding the stack!
(declaim (special *compiling-file*)) ; defined in l1-init.

(defvar *fasl-source-file* nil "Name of file currently being read from.
Will differ from *compiling-file* during an INCLUDE")

(defparameter *fasl-package-qualified-symbols* '(*loading-file-source-file* set-package %define-package)
  "These symbols are always fasdumped with full package qualification.")

(defvar *fasl-setf-name-alias-alist* ())

(defun setup-target-features (backend features)
  (if (eq backend *host-backend*)
    features
    (let* ((new nil)
	   (nope (backend-target-specific-features *host-backend*)))
      (dolist (f features)
	(unless (memq f nope) (pushnew f new)))
      (dolist (f (backend-target-specific-features backend)
	       (progn (pushnew :cross-compiling new) new))
	(pushnew f new)))))

(defun compile-file-pathname (pathname &rest ignore &key output-file &allow-other-keys)
  "Return a pathname describing what file COMPILE-FILE would write to given
   these arguments."
  (declare (ignore ignore))
  (setq pathname (merge-pathnames pathname))
  (merge-pathnames (if output-file
                     (merge-pathnames output-file *.fasl-pathname*)
                     *.fasl-pathname*) 
                   pathname))

(defun compile-file (src &key output-file
                         (verbose *compile-verbose*)
                         (print *compile-print*)
                         load
                         (features nil features-p)
                         (target *fasl-target* target-p)
                         (save-local-symbols *fasl-save-local-symbols*)
                         (save-doc-strings *fasl-save-doc-strings*)
                         (save-definitions *fasl-save-definitions*)
                         (save-source-locations *save-source-locations*)
                         (external-format :default)
                         force
                         ;; src may be a temp file with a section of the real source,
                         ;; then this is the real source file name.
                         compile-file-original-truename
                         (compile-file-original-buffer-offset 0)
                         (break-on-program-errors (if compile-file-original-truename
                                                    t  ;; really SLIME being interactive...
                                                    *fasl-break-on-program-errors*))
                         (load-preserves-optimization-settings *load-preserves-optimization-settings*))
  "Compile SRC, producing a corresponding fasl file and returning its filename."
  (let* ((backend *target-backend*)
         (symbols ())
         (values ())
         (*load-preserves-optimization-settings* load-preserves-optimization-settings))
    (when (and target-p (not (setq backend (find-backend target))))
      (warn "Unknown :TARGET : ~S.  Reverting to ~s ..." target *fasl-target*)
      (setq target *fasl-target*  backend *target-backend*))
    (if (eq *target-backend* *host-backend*)
      (when features-p
        (setq symbols '(*features*)
              values (list (append (if (listp features) features (list features)) *features*))))
      (setq symbols '(*features*)
            values (list (setup-target-features *target-backend* *features*))
            save-source-locations nil))
    (multiple-value-bind (output-file truename warnings-p serious-p)
        (loop
          (restart-case
              (return (progv symbols values
                        (%compile-file src output-file verbose print
                                       save-local-symbols save-doc-strings save-definitions
                                       save-source-locations break-on-program-errors
                                       force backend external-format
                                       compile-file-original-truename compile-file-original-buffer-offset)))
            (retry-compile-file ()
              :report (lambda (stream) (format stream "Retry compiling ~s" src))
              nil)
            (skip-compile-file ()
              :report (lambda (stream)
                        (if load
                          (format stream "Skip compiling and loading ~s" src)
                          (format stream "Skip compiling ~s" src)))
              (return-from compile-file))))
      (when load (load output-file :verbose (or verbose *load-verbose*)))
      (values truename warnings-p serious-p))))


(defvar *fasl-compile-time-env* nil)

(defun %compile-file (src output-file verbose print
                          save-local-symbols save-doc-strings save-definitions
                          save-source-locations break-on-program-errors
                          force target-backend external-format
                          compile-file-original-truename compile-file-original-buffer-offset)
  (let* ((orig-src (merge-pathnames src))
         (output-default-type (backend-target-fasl-pathname target-backend))
         (*fasl-non-style-warnings-signalled-p* nil)
         (*fasl-warnings-signalled-p* nil))
    (setq src (fcomp-find-file orig-src))
    (let* ((newtype (pathname-type src)))
      (when (and newtype (not (pathname-type orig-src)))
        (setq orig-src (merge-pathnames orig-src (make-pathname :type newtype :defaults nil)))))
    (setq output-file (merge-pathnames
		       (if output-file  ; full-pathname in case output-file is relative
			 (full-pathname (merge-pathnames output-file output-default-type) :no-error nil) 
			 output-default-type)
		       orig-src))
    ;; This should not be necessary, but it is.
    (setq output-file (namestring output-file))
    (when (physical-pathname-p orig-src) ; only back-translate to things likely to exist at load time
      (setq orig-src (back-translate-pathname orig-src '("home" "ccl"))))
    (when (and (not force)
               (probe-file output-file)
               (not (fasl-file-p output-file)))
      (cerror "overwrite it anyway"
              "Compile destination ~S is not a ~A file!"
              output-file (pathname-type
                           (backend-target-fasl-pathname
                            *target-backend*))))
    (let* ((*fasl-deferred-warnings* nil) ; !!! WITH-COMPILATION-UNIT ...
           (*fasl-save-local-symbols* save-local-symbols)
           (*save-source-locations* save-source-locations)
           (*fasl-save-doc-strings* save-doc-strings)
           (*fasl-save-definitions* save-definitions)
           (*fasl-break-on-program-errors* break-on-program-errors)
           (*fcomp-warnings-header* nil)
           (*compile-file-pathname* orig-src)
           (*compile-file-truename* (truename src))
           (*package* *package*)
           (*readtable* *readtable*)
           (*compile-print* print)
           (*compile-verbose* verbose)
           (*fasl-target* (backend-name target-backend))
           (*fasl-backend* target-backend)
           (*fasl-target-big-endian* (arch::target-big-endian
                                      (backend-target-arch target-backend)))
           (*target-ftd* (backend-target-foreign-type-data target-backend))
           (defenv (new-definition-environment))
           (lexenv (new-lexical-environment defenv))
           (*fasl-compile-time-env* (new-lexical-environment (new-definition-environment)))
           (*fcomp-external-format* (if (eq external-format :default)
                                      :inferred
                                      external-format))
           (*fasl-setf-name-alias-alist* ())
           (forms nil))
      (let ((current *outstanding-deferred-warnings*) last)
        (when (and current
                   (setq last (deferred-warnings.last-file current))
                   (equalp *compile-file-pathname* (cdr last)))
          ;; Discard previous deferred warnings when recompiling exactly the same file again,
          ;; since most likely this is due to an interactive "retry compilation" request and
          ;; we want to avoid duplicate warnings.
          (setf (deferred-warnings.last-file current) nil)))

      (let* ((*outstanding-deferred-warnings* (%defer-warnings nil)))
        (rplacd (defenv.type defenv) *outstanding-deferred-warnings*)
        (setf (defenv.defined defenv) (deferred-warnings.defs *outstanding-deferred-warnings*))

        (setq forms (fcomp-file src
                                (or compile-file-original-truename (namestring orig-src))
                                compile-file-original-buffer-offset
                                lexenv))

        (setf (deferred-warnings.warnings *outstanding-deferred-warnings*) 
              (append *fasl-deferred-warnings* (deferred-warnings.warnings *outstanding-deferred-warnings*)))
        (when *compile-verbose* (fresh-line))
        (multiple-value-bind (any harsh) (report-deferred-warnings *compile-file-pathname*)
          (setq *fasl-warnings-signalled-p* (or *fasl-warnings-signalled-p* any)
                *fasl-non-style-warnings-signalled-p* (if (eq harsh :very) :very
                                                        (or *fasl-non-style-warnings-signalled-p* harsh)))))
      (when (and *fasl-break-on-program-errors* (eq *fasl-non-style-warnings-signalled-p* :very))
        (cerror "create the output file despite the errors"
                "Serious errors encountered during compilation of ~s"
                src))
      (fasl-scan-forms-and-dump-file forms output-file lexenv)
      (values output-file
              (truename (pathname output-file)) 
              *fasl-warnings-signalled-p* 
              (and *fasl-non-style-warnings-signalled-p* t)))))

(defvar *fcomp-locked-hash-tables*)
(defvar *fcomp-load-forms-environment* nil)

; This is separated out so that dump-forms-to-file can use it
(defun fasl-scan-forms-and-dump-file (forms output-file &optional env)
  (let ((*fcomp-locked-hash-tables* nil)
	(*fcomp-load-forms-environment* env))
    (unwind-protect
      (multiple-value-bind (hash gnames goffsets) (fasl-scan forms)
        (fasl-dump-file gnames goffsets forms hash output-file))
      (fasl-unlock-hash-tables))))

#-bccl
(defun nfcomp (src &optional dest &rest keys)
  (when (keywordp dest) (setq keys (cons dest keys) dest nil))
  (apply #'compile-file src :output-file dest keys))

#-bccl
(%fhave 'fcomp #'nfcomp)

(defparameter *default-file-compilation-policy* (new-compiler-policy))

(defun current-file-compiler-policy ()
  *default-file-compilation-policy*)

(defun set-current-file-compiler-policy (&optional new-policy)
  (setq *default-file-compilation-policy* 
        (if new-policy (require-type new-policy 'compiler-policy) (new-compiler-policy))))

(defparameter *compile-time-evaluation-policy*
  (new-compiler-policy :force-boundp-checks t))

(defun %compile-time-eval (form env)
  (declare (ignore env))
  (let* ((*target-backend* *host-backend*)
         (*loading-toplevel-location* (or (fcomp-source-note form)
                                          *loading-toplevel-location*))
         (lambda `(lambda () ,form)))
    (fcomp-note-source-transformation form lambda)
    ;; The HANDLER-BIND here is supposed to note WARNINGs that're
    ;; signaled during (eval-when (:compile-toplevel) processing; this
    ;; in turn is supposed to satisfy a pedantic interpretation of the
    ;; spec's requirement that COMPILE-FILE's second and third return
    ;; values reflect (all) conditions "detected by the compiler."
    ;; (It's kind of sad that CL language design is influenced so
    ;; strongly by the views of pedants these days.)
    (handler-bind ((warning (lambda (c)
                              (setq *fasl-warnings-signalled-p* t)
                              (unless (typep c 'style-warning)
                                (setq *fasl-non-style-warnings-signalled-p* t))
                              (signal c))))
      (funcall (compile-named-function
                lambda
                :compile-code-coverage nil
                :source-notes *fcomp-source-note-map*
                :env *fasl-compile-time-env*
                :policy *compile-time-evaluation-policy*)))))


;;; No methods by default, not even for structures.  This really sux.
(defgeneric make-load-form (object &optional environment))

;;; Well, no usable methods by default.  How this is better than
;;; getting a NO-APPLICABLE-METHOD error frankly escapes me,
;;; [Hint: this is called even when there is an applicable method]
(defun no-make-load-form-for (object)
  (error "No ~S method is defined for ~s" 'make-load-form object))

(defmethod make-load-form ((s standard-object) &optional environment)
  (declare (ignore environment))
  (no-make-load-form-for s))

(defmethod make-load-form ((s structure-object) &optional environment)
  (declare (ignore environment))
  (no-make-load-form-for s))

(defmethod make-load-form ((c condition) &optional environment)
  (declare (ignore environment))
  (no-make-load-form-for c))

(defmethod make-load-form ((c class) &optional environment)
  (let* ((name (class-name c))
	 (found (if name (find-class name nil environment))))
    (if (eq found c)
      `(find-class ',name)
      (error "Class ~s does not have a proper name." c))))


;;;;          FCOMP-FILE - read & compile file
;;;;          Produces a list of (opcode . args) to run on loading, intermixed
;;;;          with read packages.

(defparameter *fasl-eof-forms* nil)

(defparameter cfasl-load-time-eval-sym (make-symbol "LOAD-TIME-EVAL"))
(%macro-have cfasl-load-time-eval-sym
    #'(lambda (call env) (declare (ignore env)) (list 'eval (list 'quote call))))
;Make it a constant so compiler will barf if try to bind it, e.g. (LET #,foo ...)
(define-constant cfasl-load-time-eval-sym cfasl-load-time-eval-sym)


(defparameter *reading-for-cfasl* nil "Used by the reader for #,")



(declaim (special *nx-compile-time-types*
;The following are the global proclaimed values.  Since compile-file binds
;them, this means you can't ever globally proclaim these things from within a
;file compile (e.g. from within eval-when compile, or loading a file) - the
;proclamations get lost when compile-file exits.  This is sort of intentional
;(or at least the set of things which fall in this category as opposed to
;having a separate compile-time variable is sort of intentional).
                    *nx-proclaimed-inline*    ; inline and notinline
                    *nx-proclaimed-ignore*    ; ignore and unignore
                    *nx-known-declarations*   ; declaration
                    *nx-speed*                ; optimize speed
                    *nx-space*                ; optimize space
                    *nx-safety*               ; optimize safety
                    *nx-cspeed*))             ; optimize compiler-speed

(defvar *fcomp-load-time*)
(defvar *fcomp-inside-eval-always* nil)
(defvar *fcomp-eval-always-functions* nil)   ; used by the LISP package
(defvar *fcomp-output-list*)
(defvar *fcomp-toplevel-forms*)
(defvar *fcomp-source-note-map* nil)
(defvar *fcomp-loading-toplevel-location*)
(defvar *fcomp-warnings-header*)
(defvar *fcomp-stream-position* nil)
(defvar *fcomp-previous-position* nil)
(defvar *fcomp-indentation*)
(defvar *fcomp-print-handler-plist* nil)
(defvar *fcomp-last-compile-print*
  '(INCLUDE (NIL . T)
    DEFSTRUCT ("Defstruct" . T) 
    DEFCONSTANT "Defconstant" 
    DEFSETF "Defsetf" 
    DEFTYPE "Deftype" 
    DEFCLASS "Defclass" 
    DEFGENERIC "Defgeneric"
    DEFMETHOD "Defmethod"
    DEFMACRO "Defmacro" 
    DEFPARAMETER "Defparameter" 
    DEFVAR "Defvar" 
    DEFUN ""))

(setf (getf *fcomp-print-handler-plist* 'defun) ""
      (getf *fcomp-print-handler-plist* 'defvar) "Defvar"
      (getf *fcomp-print-handler-plist* 'defparameter) "Defparameter"
      (getf *fcomp-print-handler-plist* 'defmacro) "Defmacro"
      (getf *fcomp-print-handler-plist* 'defmethod) "Defmethod"  ; really want more than name (use the function option)
      (getf *fcomp-print-handler-plist* 'defgeneric) "Defgeneric"
      (getf *fcomp-print-handler-plist* 'defclass) "Defclass"
      (getf *fcomp-print-handler-plist* 'deftype) "Deftype"
      (getf *fcomp-print-handler-plist* 'defsetf) "Defsetf"
      (getf *fcomp-print-handler-plist* 'defconstant) "Defconstant"
      (getf *fcomp-print-handler-plist* 'defstruct) '("Defstruct" . t)
      (getf *fcomp-print-handler-plist* 'include) '(nil . t))


(defun fcomp-file (filename orig-file orig-offset env)  ; orig-file is back-translated
  (let* ((*package* *package*)
         (*compiling-file* filename)
         (*nx-compile-time-types* *nx-compile-time-types*)
         (*nx-proclaimed-inline* *nx-proclaimed-inline*)
         (*nx-known-declarations* *nx-known-declarations*)
         (*nx-proclaimed-ignore* *nx-proclaimed-ignore*)
         (*nx-speed* *nx-speed*)
         (*nx-space* *nx-space*)
         (*nx-debug* *nx-debug*)
         (*nx-safety* *nx-safety*)
         (*nx-cspeed* *nx-cspeed*)
         (*fcomp-load-time* t)
         (*fcomp-output-list* nil)
         (*fcomp-indentation* 0)
         (*fcomp-last-compile-print* (cons nil (cons nil nil))))
    (push (list $fasl-platform (backend-target-platform *fasl-backend*)) *fcomp-output-list*)
    (fcomp-read-loop filename orig-file orig-offset env :not-compile-time)
    (nreverse *fcomp-output-list*)))

(defun fcomp-find-file (file &aux path)
  (unless (or (and (setq path (probe-file file))
		   (not (directoryp path)))
              (setq path (probe-file (merge-pathnames file *.lisp-pathname*))))
    (error 'file-error :pathname file :error-type "File ~S not found"))
  (namestring path))

;;; orig-file is back-translated when from fcomp-file
;;; when from fcomp-include it's included filename merged with *compiling-file*
;;; which is not back translated
(defun fcomp-read-loop (filename orig-file orig-offset env processing-mode)
  (when *compile-verbose*
    (format t "~&;~A ~S..."
            (if (eq filename *compiling-file*) "Compiling" " Including")
            filename))
  (with-open-file (stream filename
                          :element-type 'base-char
                          :external-format *fcomp-external-format*)
    (let* ((old-file (and (neq filename *compiling-file*) *fasl-source-file*))
           (*fasl-source-file* (or orig-file filename))
           (*fcomp-toplevel-forms* nil)
           (*fasl-eof-forms* nil)
           (*loading-file-source-file* orig-file)
           (*fcomp-source-note-map* (and (or *save-source-locations* *compile-code-coverage*)
                                         (make-hash-table :test #'eq :shared nil)))
           (*loading-toplevel-location* nil)
           (*fcomp-loading-toplevel-location* nil)
           (eofval (cons nil nil))
           (read-package nil)
           form)

      (fcomp-output-form $fasl-src env *loading-file-source-file*)
      (let* ((*fcomp-previous-position* nil))
        (loop
          (let* ((*fcomp-stream-position* (file-position stream))
                 (*nx-warnings* nil)) ;; catch any warnings from :compile-toplevel forms
            (when (and *fcomp-stream-position* orig-offset)
              (incf *fcomp-stream-position* orig-offset))
            (unless (eq read-package *package*)
              (fcomp-compile-toplevel-forms env)
              (setq read-package *package*))
            (let ((*reading-for-cfasl*
                   (and *fcomp-load-time* cfasl-load-time-eval-sym)))
              (declare (special *reading-for-cfasl*))
              (let ((pos (file-position stream)))
                (handler-bind
                    ((error #'(lambda (c) ; we should distinguish read errors from others?
                                (format *error-output* "~&Read error between positions ~a and ~a in ~a." pos (file-position stream) filename)
                                (signal c))))
                  (multiple-value-setq (form *loading-toplevel-location*)
                    (read-recording-source stream
                                           :eofval eofval
                                           :file-name *loading-file-source-file*
                                           :start-offset orig-offset
                                           :map *fcomp-source-note-map*
                                           :save-source-text (neq *save-source-locations* :no-text))))))
            (when (eq eofval form)
	      (require-type *loading-toplevel-location* 'null)
	      (return))
            (fcomp-form form env processing-mode)
            (fcomp-signal-or-defer-warnings *nx-warnings* env)
            (setq *fcomp-previous-position* *fcomp-stream-position*))))
      (when *compile-code-coverage*
	(fcomp-compile-toplevel-forms env)
        (let* ((fns (fcomp-code-covered-functions))
	       (v (nreverse (coerce fns 'vector)))
               (id (fcomp-file-checksum stream)))
	  (map nil #'fcomp-digest-code-notes v)
          (fcomp-random-toplevel-form `(register-code-covered-functions ',v
                                                                        ',*fcomp-external-format*
                                                                        ,id)
                                      env)))
      (while (setq form *fasl-eof-forms*)
        (setq *fasl-eof-forms* nil)
        (fcomp-form-list form env processing-mode))
      (when old-file
        (fcomp-output-form $fasl-src env (namestring *compile-file-pathname*)))
      (fcomp-compile-toplevel-forms env))))

(defvar *crc32-table* (let ((crc-table (make-array 256 :element-type '(unsigned-byte 32))))
                        (loop for i from 0 below 255 as crc = i
                              do (loop for j from 0 below 8
                                       do (setq crc (ash crc -1))
                                       do (when (oddp crc)
                                            (setq crc (logxor crc  #xEDB88320))))
                              do (setf (aref crc-table i) crc))
                        crc-table))
(declaim (type (simple-array (unsigned-byte 32) (256)) *crc32-table*))

(defun fcomp-stream-checksum (stream)
  ;; Could consider crc16 for 32-bit targets, but this is only used with code
  ;; coverage so don't worry about efficiency anyway.
  (file-position stream 0)
  (let ((crc 0))
    (declare (type (unsigned-byte 32) crc))
    (loop for char base-char = (read-char stream nil) while char
          do (setq crc (logxor
                         (%ilogand (ash crc -8) #x00FFFFFF)
                         (aref *crc32-table* (logand (logxor crc (char-code char)) #xFF)))))
    (logior (ash (file-position stream) 32) crc)))

(defun fcomp-file-checksum (filename &key (external-format *fcomp-external-format*))
  (when (setq filename (probe-file filename))
    (with-open-file (stream filename
                            :element-type 'base-char
                            :external-format external-format)
      (fcomp-stream-checksum stream))))

(defun fcomp-code-covered-functions ()
  (loop for op in *fcomp-output-list*
        when (consp op)
          nconc (if (eq (car op) $fasl-lfuncall)
                  ;; Don't collect the toplevel lfun itself, it leads to spurious markings.
                  ;; Instead, descend one level and collect any referenced fns.
                  (destructuring-bind (fn) (cdr op)
                    (lfunloop for imm in fn when (functionp imm) collect imm))
                  (loop for arg in (cdr op) when (functionp arg) collect arg))))


(defun fcomp-form (form env processing-mode
                        &aux print-stuff 
                        (load-time (and processing-mode (neq processing-mode :compile-time)))
                        (compile-time-too (or (eq processing-mode :compile-time) 
                                              (eq processing-mode :compile-time-too))))
  (let* ((*fcomp-indentation* *fcomp-indentation*)
         (*compile-print* *compile-print*))
    (when *compile-print*
      (cond ((and (consp form) (setq print-stuff (getf *fcomp-print-handler-plist* (car form))))
             (rplaca (rplacd (cdr *fcomp-last-compile-print*) nil) nil)
             (rplaca *fcomp-last-compile-print* nil)         
             (let ((print-recurse nil))
               (when (consp print-stuff)
                 (setq print-recurse (cdr print-stuff) print-stuff (car print-stuff)))
               (cond ((stringp print-stuff)
                      (if (equal print-stuff "")
                        (format t "~&~vT~S~%" *fcomp-indentation* (second form))
                        (format t "~&~vT~S [~A]~%" *fcomp-indentation* (second form) print-stuff)))
                     ((not (null print-stuff))
                      (format t "~&~vT" *fcomp-indentation*)
                      (funcall print-stuff form *standard-output*)
                      (terpri *standard-output*)))
               (if print-recurse
                 (setq *fcomp-indentation* (+ *fcomp-indentation* 4))
                 (setq *compile-print* nil))))
            (t (unless (and (eq load-time (car *fcomp-last-compile-print*))
                            (eq compile-time-too (cadr *fcomp-last-compile-print*))
                            (eq *fcomp-indentation* (cddr *fcomp-last-compile-print*)))
                 (rplaca *fcomp-last-compile-print* load-time)
                 (rplaca (rplacd (cdr *fcomp-last-compile-print*) compile-time-too) *fcomp-indentation*)
                 (format t "~&~vTToplevel Forms...~A~%"
                         *fcomp-indentation*
                         (if load-time
                           (if compile-time-too
                             "  (Compiletime, Loadtime)"
                             "")
                           (if compile-time-too
                             "  (Compiletime)"
                             "")))))))
    (fcomp-form-1 form env processing-mode)))

(defun fcomp-form-1 (form env processing-mode &aux sym body)
  (if (consp form) (setq sym (%car form) body (%cdr form)))
  (case sym
    (progn (fcomp-form-list body env processing-mode))
    (eval-when (fcomp-eval-when form env processing-mode))
    (compiler-let (fcomp-compiler-let form env processing-mode))
    (locally (fcomp-locally form env processing-mode))
    (macrolet (fcomp-macrolet form env processing-mode))
    (symbol-macrolet (fcomp-symbol-macrolet form env processing-mode))
    ((%include include) (fcomp-include form env processing-mode))
    (t
     ;;Need to macroexpand to see if get more progn's/eval-when's and so should
     ;;stay at toplevel.  But don't expand if either the evaluator or the
     ;;compiler might not - better safe than sorry... 
     ;; Good advice, but the hard part is knowing which is which.
     (cond 
       ((and (non-nil-symbol-p sym)
             (macro-function sym env)
             (not (compiler-macro-function sym env))
             (not (eq sym '%defvar-init)) ;  a macro that we want to special-case
             (multiple-value-bind (new win) (fcomp-macroexpand-1 form env)
               (if win (setq form new))
               win))
        (fcomp-form form env processing-mode))
       ((and (not *fcomp-inside-eval-always*)
             (memq sym *fcomp-eval-always-functions*))
        (let* ((*fcomp-inside-eval-always* t)
               (new `(eval-when (:execute :compile-toplevel :load-toplevel) ,form)))
          (fcomp-form-1 new env processing-mode)))
       (t
        (when (or (eq processing-mode :compile-time) (eq processing-mode :compile-time-too))
          (%compile-time-eval form env))
        (when (and processing-mode (neq processing-mode :compile-time))
          (case sym
            ((%defconstant) (fcomp-load-%defconstant form env))
            ((%defparameter) (fcomp-load-%defparameter form env))
            ((%defvar %defvar-init) (fcomp-load-defvar form env))
            ((%defun) (fcomp-load-%defun form env))
            ((set-package %define-package)
             (fcomp-random-toplevel-form form env)
             (fcomp-compile-toplevel-forms env))
            ((%macro) (fcomp-load-%macro form env))
            ;; ((%deftype) (fcomp-load-%deftype form))
            ;; ((define-setf-method) (fcomp-load-define-setf-method form))
            (t (fcomp-random-toplevel-form form env)))))))))

(defun fcomp-form-list (forms env processing-mode)
  (let* ((outer *loading-toplevel-location*))
    (dolist (form forms)
      (setq *loading-toplevel-location* (or (fcomp-source-note form) outer))
      (fcomp-form form env processing-mode))
    (setq *loading-toplevel-location* outer)))

(defun fcomp-compiler-let (form env processing-mode &aux vars varinits (body (%cdr form)))
  (fcomp-compile-toplevel-forms env)
  (dolist (pair (car body))
    (push (nx-pair-name pair) vars)
    (push (%compile-time-eval (nx-pair-initform pair) env) varinits))
  (progv (nreverse vars) (nreverse varinits)
    (fcomp-form-list (cdr body) env processing-mode)
    (fcomp-compile-toplevel-forms env)))

(defun fcomp-locally (form env processing-mode &aux (body (%cdr form)))
  (fcomp-compile-toplevel-forms env)
  (multiple-value-bind (body decls) (parse-body body env)
    (let* ((decl-specs (decl-specs-from-declarations decls))
           (env (augment-environment env :declare decl-specs))
           (*fasl-compile-time-env* (augment-environment *fasl-compile-time-env*
                                                         :declare decl-specs)))
      (fcomp-form-list body env processing-mode)
      (fcomp-compile-toplevel-forms env))))

(defun fcomp-macrolet (form env processing-mode &aux (body (%cdr form)))
  (fcomp-compile-toplevel-forms env)
  (flet ((augment-with-macros (e defs)
           (augment-environment e
                                :macro
                                (mapcar #'(lambda (m)
                                            (destructuring-bind (name arglist &body body) m
                                              (list name (enclose (parse-macro name arglist body env)
                                                                  e))))
                                        defs))))
           
    (let* ((macros (car body))
           (outer-env (augment-with-macros env macros)))
      (multiple-value-bind (body decls) (parse-body (cdr body) outer-env)
        (let* ((decl-specs (decl-specs-from-declarations decls))
               (env (augment-environment 
                     outer-env
                     :declare decl-specs))
               (*fasl-compile-time-env* (augment-environment
                                         (augment-with-macros
                                          *fasl-compile-time-env*
                                          macros)
                                         :declare decl-specs)))
          (fcomp-form-list body env processing-mode)
          (fcomp-compile-toplevel-forms env))))))

(defun fcomp-symbol-macrolet (form env processing-mode &aux (body (%cdr form)))
  (fcomp-compile-toplevel-forms env)
  (let* ((defs (car body))
         (outer-env (augment-environment env :symbol-macro defs)))
    (multiple-value-bind (body decls) (parse-body (cdr body) env)
      (let* ((decl-specs (decl-specs-from-declarations decls))
             (env (augment-environment outer-env 
                                       :declare decl-specs))
             (*fasl-compile-time-env* (augment-environment *fasl-compile-time-env*
                                                           :symbol-macro defs
                                                           :declare decl-specs)))
        (fcomp-form-list body env processing-mode)
        (fcomp-compile-toplevel-forms env)))))

(defun fcomp-eval-when (form env processing-mode &aux (body (%cdr form)) (eval-times (pop body)))
  (let* ((compile-time-too  (eq processing-mode :compile-time-too))
         (compile-time-only (eq processing-mode :compile-time))
         (at-compile-time nil)
         (at-load-time nil)
         (at-eval-time nil))
    (dolist (when eval-times)
      (if (or (eq when 'compile) (eq when :compile-toplevel))
        (setq at-compile-time t)
        (if (or (eq when 'eval) (eq when :execute))
          (setq at-eval-time t)
          (if (or (eq when 'load) (eq when :load-toplevel))
            (setq at-load-time t)
            (warn "Unknown EVAL-WHEN time ~s in ~S while compiling ~S."
                  when eval-times *fasl-source-file*)))))
    (fcomp-compile-toplevel-forms env)        ; always flush the suckers
    (cond (compile-time-only
           (if at-eval-time (fcomp-form-list body env :compile-time)))
          (at-load-time
           (fcomp-form-list body env (if (or at-compile-time (and at-eval-time compile-time-too))
                                       :compile-time-too
                                       :not-compile-time)))
          ((or at-compile-time (and at-eval-time compile-time-too))
           (fcomp-form-list body env :compile-time))))
  (fcomp-compile-toplevel-forms env))

(defun fcomp-include (form env processing-mode &aux file)
  (fcomp-compile-toplevel-forms env)
  (verify-arg-count form 1 1)
  (setq file (nx-transform (%cadr form) env))
  (unless (constantp file) (report-bad-arg file '(or string pathname)))
  (let ((actual (merge-pathnames (eval-constant file)
                                 (directory-namestring *compiling-file*))))
    (when *compile-print* (format t "~&~vTIncluding file ~A~%" *fcomp-indentation* actual))
    (let ((*fcomp-indentation* (+ 4 *fcomp-indentation*))
          (*package* *package*))
      (fcomp-read-loop (fcomp-find-file actual) actual 0 env processing-mode)
      (fcomp-output-form $fasl-src env *loading-file-source-file*))
    (when *compile-print* (format t "~&~vTFinished included file ~A~%" *fcomp-indentation* actual))))

(defun define-compile-time-constant (symbol initform env)
  (note-variable-info symbol t env)
  (let ((compile-time-defenv (definition-environment *fasl-compile-time-env*))
        (definition-env (definition-environment env)))
    (when (or compile-time-defenv definition-env)
      (multiple-value-bind (value error) 
                           (ignore-errors (values (%compile-time-eval initform env) nil))
        (when error
          (warn "Compile-time evaluation of DEFCONSTANT initial value form for ~S while ~
                 compiling ~S signalled the error: ~&~A" symbol *fasl-source-file* error))
        (let ((cell (cons symbol (if error (%unbound-marker-8) value))))
          (when definition-env
            (push cell (defenv.constants definition-env)))
          (when compile-time-defenv
            (push cell (defenv.constants compile-time-defenv))))))
    symbol))

(defun fcomp-load-%defconstant (form env)
  (destructuring-bind (sym valform &optional doc) (cdr form)
    (unless *fasl-save-doc-strings*
      (setq doc nil))
    (if (quoted-form-p sym)
      (setq sym (%cadr sym)))
    (if (and (typep sym 'symbol) (or (quoted-form-p valform) (self-evaluating-p valform)))
      (fcomp-output-form $fasl-defconstant env sym (eval-constant valform) (eval-constant doc))
      (fcomp-random-toplevel-form form env))))

(defun fcomp-load-%defparameter (form env)
  (destructuring-bind (sym valform &optional doc) (cdr form)
    (unless *fasl-save-doc-strings*
      (setq doc nil))
    (if (quoted-form-p sym)
      (setq sym (%cadr sym)))
    (let* ((sym-p (typep sym 'symbol))
           (fn (and sym-p (fcomp-function-arg valform env))))
      (if (and sym-p (or fn (constantp valform)))
        (fcomp-output-form $fasl-defparameter env sym (or fn (eval-constant valform)) (eval-constant doc))
        (fcomp-random-toplevel-form form env)))))

; Both the simple %DEFVAR and the initial-value case (%DEFVAR-INIT) come here.
; Only try to dump this as a special fasl operator if the initform is missing
;  or is "harmless" to evaluate whether needed or not (constant or function.)
; Hairier initforms could be handled by another fasl operator that takes a thunk
; and conditionally calls it.
(defun fcomp-load-defvar (form env)
  (destructuring-bind (sym &optional (valform nil val-p) doc) (cdr form)
    (unless *fasl-save-doc-strings*
      (setq doc nil))
    (if (quoted-form-p sym)             ; %defvar quotes its arg, %defvar-init doesn't.
      (setq sym (%cadr sym)))
    (let* ((sym-p (typep sym 'symbol)))
      (if (and sym-p (not val-p))
        (fcomp-output-form $fasl-defvar env sym)
        (let* ((fn (if sym-p (fcomp-function-arg valform env))))
          (if (and sym-p (or fn (constantp valform)))
            (fcomp-output-form $fasl-defvar-init env sym (or fn (eval-constant valform)) (eval-constant doc))
            (fcomp-random-toplevel-form form env)))))))
      
(defun define-compile-time-macro (name lambda-expression env)
  (let ((compile-time-defenv (definition-environment *fasl-compile-time-env*))
        (definition-env (definition-environment env)))
    (when (or definition-env compile-time-defenv)
      (let ((cell (list* name 
                         'macro 
                         (compile-named-function lambda-expression :name name :env env))))
        (when compile-time-defenv
          (push cell (defenv.functions compile-time-defenv)))
        (when definition-env
          (push cell (defenv.functions definition-env))))
      (record-function-info name (%cons-def-info 'defmacro) env))
    name))

(defun define-compile-time-symbol-macro (name expansion env)
  (let ((compile-time-defenv (definition-environment *fasl-compile-time-env*))
        (definition-env (definition-environment env)))
    (let* ((info (variable-information name env)))
      (when (or (eq info :special)
                (eq info :constant))
        (signal-program-error "Can't define ~s as a symbol-macro; already defined as a ~a." name (string-downcase info))))
    (when (or definition-env compile-time-defenv)
      (let ((cell (cons name expansion)))
        (when compile-time-defenv
          (push cell (defenv.symbol-macros compile-time-defenv)))
        (when definition-env
          (push cell (defenv.symbol-macros definition-env)))))
    name))


(defun fcomp-proclaim-type (type syms env)
  (if (every #'symbolp syms)
    (progn
      (specifier-type-if-known type env :whine t)
      (dolist (sym syms)
        (push (cons sym type) *nx-compile-time-types*)))
    (nx-bad-decls `(,type ,@syms))))

(defun compile-time-proclamation (specs env &aux  sym (defenv (definition-environment env)))
  (when defenv
    (dolist (spec specs)
      (setq sym (pop spec))
      (case sym
        (type
         (fcomp-proclaim-type (car spec) (cdr spec) env))
        (special
         (if (every #'symbolp spec)
           (dolist (sym spec)
             (push (cons sym nil) (defenv.specials defenv)))
           (nx-bad-decls `(,sym ,@spec))))
        (notspecial
         (if (every #'symbolp spec)
           (let ((specials (defenv.specials defenv)))
             (dolist (sym spec (setf (defenv.specials defenv) specials))
               (let ((pair (assq sym specials)))
                 (when pair (setq specials (nremove pair specials))))))
           (nx-bad-decls `(,sym ,@spec))))
        (optimize
           (handler-case (%proclaim-optimize spec)
             (program-error () (nx-bad-decls `(,sym ,@spec)))))
        (inline
         (if (every (lambda (v) (or (symbolp v) (setf-function-name-p v))) spec)
           (dolist (sym spec)
             (push (cons (maybe-setf-function-name sym) (cons 'inline 'inline)) (lexenv.fdecls defenv)))
           (nx-bad-decls `(,sym ,@spec))))
        (notinline
         (if (every (lambda (v) (or (symbolp v) (setf-function-name-p v))) spec)
           (dolist (sym spec)
             (unless (compiler-special-form-p sym)
               (push (cons (maybe-setf-function-name sym) (cons 'inline 'notinline)) (lexenv.fdecls defenv))))
           (nx-bad-decls `(,sym ,@spec))))
        (declaration
         (if (every #'symbolp spec)
           (dolist (sym spec)
             (pushnew sym *nx-known-declarations*))
           (nx-bad-decls `(,sym ,@spec))))
        (ignore
         (if (every #'symbolp spec)
           (dolist (sym spec)
             (push (cons sym t) *nx-proclaimed-ignore*))
           (nx-bad-decls `(,sym ,@spec))))
        (unignore
         (if (every #'symbolp spec)
           (dolist (sym spec)
             (push (cons sym nil) *nx-proclaimed-ignore*))
           (nx-bad-decls `(,sym ,@spec))))
        (ftype 
         (let ((ftype (car spec))
               (fnames (cdr spec)))
           (if (every (lambda (v) (or (symbolp v) (setf-function-name-p v))) fnames)
             (when (specifier-type-if-known ftype env :whine t)
               ;; ----- this part may be redundant, now that the lexenv.fdecls part is being done
               (if (and (consp ftype)
                        (consp fnames)
                        (eq (%car ftype) 'function))
                 (dolist (fname fnames)
                   (note-function-info fname nil env)))
               (dolist (fname fnames)
                 (push (list* (maybe-setf-function-name fname) sym ftype) (lexenv.fdecls defenv))))
             (nx-bad-decls `(ftype ,@spec)))))
        (otherwise
	 (unless (memq sym *nx-known-declarations*)
	   ;; Any type name is now (ANSI CL) a valid declaration.
	   (if (specifier-type-if-known sym env)
	     (fcomp-proclaim-type sym spec env)
	     (nx-bad-decls `(,sym ,@spec)))))))))

(defun fcomp-load-%defun (form env)
  (destructuring-bind (fn &optional doc) (cdr form)
    (unless *fasl-save-doc-strings*
      (if (consp doc)
        (if (and (eq (car doc) 'quote) (consp (cadr doc)))
          (setf (car (cadr doc)) nil))
        (setq doc nil)))
    (when (and (consp fn) (eq (%car fn) 'nfunction))
      (note-function-info (cadr fn) (caddr fn) env))
    (if (and (constantp doc)
             (setq fn (fcomp-function-arg fn env)))
      (progn
        (setq doc (eval-constant doc))
        (fcomp-output-form $fasl-defun env fn doc))
      (fcomp-random-toplevel-form form env))))

(defun fcomp-load-%macro (form env &aux fn doc)
  (verify-arg-count form 1 2)
  (if (and (constantp (setq doc (caddr form)))
           (setq fn (fcomp-function-arg (cadr form) env)))
    (progn
      (setq doc (eval-constant doc))
      (fcomp-output-form $fasl-macro env fn doc))
    (fcomp-random-toplevel-form form env)))

(defun define-compile-time-structure (sd refnames predicate env)
  (let ((defenv (definition-environment env))
        (class-name (and (not (sd-type sd)) (symbolp (sd-name sd)) (sd-name sd))))
    (when defenv
      (when class-name
	(note-type-info class-name 'class env)
        (push (make-instance 'compile-time-class :name class-name)
              (defenv.classes defenv)))
      (setf (defenv.structures defenv) (alist-adjoin (sd-name sd) sd (defenv.structures defenv)))
      (let* ((structrefs (defenv.structrefs defenv)))
        (when (and predicate class-name)
          (setq structrefs (alist-adjoin predicate class-name structrefs)))
        (dolist (slot (sd-slots sd))
	  (cond
	    ((fixnump (ssd-name slot)))	;skip
	    ((ssd-inherited slot)
	     (when refnames (pop refnames)))
	    (t
	     (setq structrefs
		   ;; structrefs isn't a proper alist
		   (alist-adjoin (if refnames (pop refnames) (ssd-name slot))
				 (cons (ssd-type-and-refinfo slot) class-name)
				 structrefs)))))
        (setf (defenv.structrefs defenv) structrefs)))))

(defun fcomp-source-note (form &aux (notes *fcomp-source-note-map*))
  (and notes (gethash form notes)))

(defun (setf fcomp-source-note) (note form &aux (notes *fcomp-source-note-map*))
  (and notes (setf (gethash form notes) note)))

(defun fcomp-note-source-transformation (original new)
  (let* ((*nx-source-note-map* *fcomp-source-note-map*))
    (nx-note-source-transformation original new)))

(defun fcomp-macroexpand-1 (form env)
  (handler-bind ((warning (lambda (c)
                            (nx1-whine :program-error c)
                            (muffle-warning c)))
                 (program-error (lambda (c)
                                  (if *fasl-break-on-program-errors*
                                    (cerror "continue compilation ignoring this form" c)
                                    (progn
                                      (when (typep c 'compile-time-program-error)
                                        (setq c (make-condition 'simple-program-error
                                                  :format-control (simple-condition-format-control c)
                                                  :format-arguments (simple-condition-format-arguments c))))
                                      (nx1-whine :program-error c)))
                                  (return-from fcomp-macroexpand-1 (values nil t)))))
    (let* ((*nx-source-note-map* *fcomp-source-note-map*))
      (multiple-value-bind (new win)
          (macroexpand-1 form env)
        (when win
          (nx-note-source-transformation form new))
        (values new win)))))

(defun fcomp-transform (form env)
  (let* ((*nx-source-note-map* *fcomp-source-note-map*))
    (nx-transform form env)))


(defun fcomp-random-toplevel-form (form env)
  (unless (constantp form)
    (unless (or (atom form)
                (compiler-special-form-p (%car form)))
      ;;Pre-compile any lfun args.  This is an efficiency hack, since compiler
      ;;reentering itself for inner lambdas tends to be more expensive than
      ;;top-level compiles.
      ;;This assumes the form has been macroexpanded, or at least none of the
      ;;non-evaluated macro arguments could look like functions.
      (let ((new-form (make-list (length form))))
        (declare (dynamic-extent new-form))
        (loop for arg in (%cdr form) for newptr on (%cdr new-form)
              do (setf (%car newptr)
                       (multiple-value-bind (new win) (fcomp-transform arg env)
                         (let ((lfun (fcomp-function-arg new env)))
                           (when lfun
                             (setq new `',lfun win t)
                             (fcomp-note-source-transformation arg new)))
                         (if win new arg))))
        (unless (every #'eq (%cdr form) (%cdr new-form))
          (setf (%car new-form) (%car form))
          (fcomp-note-source-transformation form (setq form (copy-list new-form))))))
    ;; At some point we will dump the toplevel forms, make sure that when that happens,
    ;;; the loading location for this form is stored in *fcomp-loading-toplevel-location*,
    ;; because *loading-toplevel-location* will be long gone by then.
    (fcomp-ensure-source env)
    (push form *fcomp-toplevel-forms*)))

(defun fcomp-function-arg (expr env)
  (when (consp expr)
    (multiple-value-bind (lambda-expr name win)
        (cond ((and (eq (%car expr) 'nfunction)
                    (lambda-expression-p (cadr (%cdr expr))))
               (values (%caddr expr) (%cadr expr) t))
              ((and (eq (%car expr) 'function)
                    (lambda-expression-p (car (%cdr expr))))
               (values (%cadr expr) nil t)))
      (when win
        (fcomp-named-function lambda-expr name env
                              (or (fcomp-source-note expr)
                                  (fcomp-source-note lambda-expr)
                                  *loading-toplevel-location*))))))

(defun fcomp-compile-toplevel-forms (env)
  (when *fcomp-toplevel-forms*
    (let* ((forms (nreverse *fcomp-toplevel-forms*))
           (*fcomp-stream-position* *fcomp-previous-position*)
	   (*loading-toplevel-location* *fcomp-loading-toplevel-location*)
           (body (if T ;; (null (cdr forms))
                   `(progn ,@forms)
                   `(macrolet ((load-time-value (value)
                                 (declare (ignore value))
                                 (compiler-function-overflow)))
                      ,@forms)))
           (lambda `(lambda () ,body)))
      ;; Don't assign a location to the lambda so it doesn't confuse acode printing, but
      ;; arrange to assign it to any inner lambdas.
      (setf (fcomp-source-note body) *loading-toplevel-location*)
      (setq *fcomp-toplevel-forms* nil)
      ;(format t "~& Random toplevel form: ~s" lambda)
      (handler-case (fcomp-output-form
                     $fasl-lfuncall
                     env
                     (fcomp-named-function lambda nil env #|*loading-toplevel-location*|#))
        (compiler-function-overflow ()
          (if (null (cdr forms))
            (error "Form ~s cannot be compiled - size exceeds compiler limitation"
                   (%car forms))
            ; else compile each half :
            (progn
              (dotimes (i (floor (length forms) 2))
                (declare (fixnum i))
                (push (pop forms) *fcomp-toplevel-forms*))
              (fcomp-compile-toplevel-forms env)
              (setq *fcomp-toplevel-forms* (nreverse forms))
              (fcomp-compile-toplevel-forms env))))))))

(defun fcomp-ensure-source (env)
  ;; if source location saving is off, both values are NIL, so this will do nothing,
  ;; don't need to check explicitly.
  (unless (eq *fcomp-loading-toplevel-location* *loading-toplevel-location*)
    (fcomp-compile-toplevel-forms env)
    (setq *fcomp-loading-toplevel-location* *loading-toplevel-location*)
    (fcomp-output-form $fasl-toplevel-location env *loading-toplevel-location*)))

(defun fcomp-output-form (opcode env &rest args)
  (fcomp-ensure-source env)
  (when *fcomp-toplevel-forms* (fcomp-compile-toplevel-forms env))
  (push (cons opcode args) *fcomp-output-list*))


;;; Compile a lambda expression for the sole purpose of putting it in a fasl
;;; file.  The result will not be funcalled.  This really shouldn't bother
;;; making an lfun, but it's simpler this way...
(defun fcomp-named-function (def name env &optional source-note)
  (let* ((env (new-lexical-environment env))
         (*nx-break-on-program-errors* (not (memq *fasl-break-on-program-errors* '(nil :defer)))))
    (multiple-value-bind (lfun warnings)
        (compile-named-function def
                                :name name
                                :env env
                                :function-note source-note
                                :keep-lambda *fasl-save-definitions*
                                :keep-symbols *fasl-save-local-symbols*
                                :policy *default-file-compilation-policy*
                                :source-notes *fcomp-source-note-map*
                                :load-time-eval-token cfasl-load-time-eval-sym
                                :target *fasl-target*)
      (fcomp-signal-or-defer-warnings warnings env)
      lfun)))


;; Convert parent-notes to immediate indices.  The reason this is necessary is to avoid hitting
;; the fasdumper's 64K limit on multiply-referenced objects.  This removes the reference
;; from parent slots, making notes less likely to be multiply-referenced.
(defun fcomp-digest-code-notes (lfun &optional refs)
  (unless (memq lfun refs)
    (let ((source (function-source-note lfun)))
      (when (and (source-note-p source)
                 (not (equalp (source-note-filename source) *loading-file-source-file*)))
        ;; Function is from a different file than being compiled, so don't digest it.
        ;; This can happen when #. is used to create arbitrary literal constants.
        #+no (warn "Reference to non-externalizable literal ~s" lfun)
        (return-from fcomp-digest-code-notes)))
    (let* ((lfv (function-to-function-vector lfun))
	   (start #-x86-target 0 #+x86-target (%function-code-words lfun))
	   (refs (cons lfun refs)))
      (declare (dynamic-extent refs))
      (loop for i from start below (uvsize lfv) as imm = (uvref lfv i)
	    do (typecase imm
		 (code-note
		  (let* ((parent (code-note-parent-note imm))
			 (pos (when (code-note-p parent)
				(loop for j from start below i
				      do (when (eq parent (uvref lfv j)) (return j))))))
		    (when pos
		      (setf (code-note-parent-note imm) pos))))
		 (function
		  (fcomp-digest-code-notes imm refs)))))))

; For now, defer only UNDEFINED-REFERENCEs, signal all others via WARN.
; Well, maybe not WARN, exactly.
(defun fcomp-signal-or-defer-warnings (warnings env)
  (let ((init (null *fcomp-warnings-header*))
        (some *fasl-warnings-signalled-p*)
        (harsh *fasl-non-style-warnings-signalled-p*))
    (dolist (w warnings)
      (when (and (not (compiler-warning-source-note w)) *fcomp-stream-position*)
        (setf (compiler-warning-source-note w)
              (make-source-note :source nil
                                :filename *fasl-source-file*
                                :start-pos *fcomp-stream-position*
                                :end-pos *fcomp-stream-position*)))
      (if (and (typep w 'undefined-reference) 
               (eq w (setq w (macro-too-late-p w env))))
        (push w *fasl-deferred-warnings*)
        (progn
          (multiple-value-setq (harsh some *fcomp-warnings-header*)
                               (signal-compiler-warning w init *fcomp-warnings-header* harsh some))
          (setq init nil))))
    (setq *fasl-warnings-signalled-p* some
          *fasl-non-style-warnings-signalled-p* harsh)))

; If W is an UNDEFINED-FUNCTION-REFERENCE which refers to a macro (either at compile-time in ENV
; or globally), cons up a MACRO-USED-BEFORE-DEFINITION warning and return it; else return W.

(defun macro-too-late-p (w env)
  (let* ((args (compiler-warning-args w))
         (name (car args)))
    (if (typep w 'undefined-function-reference)
      (if (or (macro-function name)
	      (let* ((defenv (definition-environment env))
		     (info (if defenv (assq name (defenv.functions defenv)))))
		(and (consp (cdr info))
		     (eq 'macro (cadr info)))))
	  (make-instance 'macro-used-before-definition
	    :source-note (compiler-warning-source-note w)
	    :function-name (compiler-warning-function-name w)
	    :warning-type ':macro-used-before-definition
	    :args args)
	  w)
      w)))


              
;;;;          fasl-scan - dumping reference counting
;;;;
;;;;
;These should be constants, but it's too much trouble when need to change 'em.
(defparameter FASL-FILE-ID #xFF00)  ;Overall file format, shouldn't change much

(defvar *fasdump-hash*)
(defvar *fasdump-read-package*)
(defvar *fasdump-global-offsets*)
(defvar *make-load-form-hash*)

;;;Return a hash table containing subexp's which are referenced more than once.
(defun fasl-scan (forms)
  (let* ((*fasdump-hash* (make-hash-table :size (length forms)          ; Crude estimate
                                          :rehash-threshold 0.9
                                          :test 'eq
					  :shared nil))
         (*make-load-form-hash* (make-hash-table :test 'eq :shared nil))
         (*fasdump-read-package* nil)
         (*fasdump-global-offsets* nil)
         (gsymbols nil))
    (dolist (op forms)
      (if (packagep op) ; old magic treatment of *package*
        (setq *fasdump-read-package* op)
        (dolist (arg (cdr op)) (fasl-scan-form arg))))

    #-bccl (when (eq *compile-verbose* :debug)
             (format t "~&~S forms, ~S entries -> "
                     (length forms)
                     (hash-table-count *fasdump-hash*)))
    (maphash #'(lambda (key val)
                 (when (%izerop val) (remhash key *fasdump-hash*)))
             *fasdump-hash*)
    #-bccl (when (eq *compile-verbose* :debug)
             (format t "~S." (hash-table-count *fasdump-hash*)))
    (values *fasdump-hash*
            gsymbols
            *fasdump-global-offsets*)))

;;; During scanning, *fasdump-hash* values are one of the following:
;;;  nil - form hasn't been referenced yet.
;;;   0 - form has been referenced exactly once
;;;   T - form has been referenced more than once
;;;  (load-form scanning-p referenced-p initform)
;;;     form should be replaced by load-form
;;;     scanning-p is true while we're scanning load-form
;;;     referenced-p is nil if unreferenced,
;;;                     T if referenced but not dumped yet,
;;;                     0 if dumped already (fasl-dump-form uses this)
;;;     initform is a compiled version of the user's initform
(defun fasl-scan-form (form)
  (when form
    (let ((info (gethash form *fasdump-hash*)))
      (cond ((null info)
             (fasl-scan-dispatch form))
            ((eql info 0)
             (puthash form *fasdump-hash* t))
            ((listp info)               ; a make-load-form form
             (when (cadr info)
               (error "Circularity in ~S for ~S" 'make-load-form form))
             (let ((referenced-cell (cddr info)))
               (setf (car referenced-cell) t)   ; referenced-p
               (setf (gethash (car info) *fasdump-hash*) t)))))))




(defun fasl-scan-dispatch (exp)
  (when exp
    (let ((type-code (typecode exp)))
      (declare (fixnum type-code))
      (case type-code
        (#.target::tag-fixnum
         (fasl-scan-fixnum exp))
        (#+ppc64-target #.target::fulltag-cons
         #-ppc64-target #.target::tag-list (fasl-scan-list exp))
        #+ppc32-target
        (#.ppc32::tag-imm)
        #+ppc64-target
        ((#.ppc64::fulltag-imm-0
          #.ppc64::fulltag-imm-1
          #.ppc64::fulltag-imm-2
          #.ppc64::fulltag-imm-3))
	#+x8632-target
	(#.x8632::tag-imm)
        #+x8664-target
        ((#.x8664::fulltag-imm-0
          #.x8664::fulltag-imm-1))
        #+arm-target
        (#.arm::tag-imm)
        (t
         (if
           #+ppc32-target
           (= (the fixnum (logand type-code ppc32::full-tag-mask)) ppc32::fulltag-immheader)
           #+ppc64-target
           (= (the fixnum (logand type-code ppc64::lowtagmask)) ppc64::lowtag-immheader)
	   #+x8632-target
	   (= (the fixnum (logand type-code x8632::fulltagmask)) x8632::fulltag-immheader)
           #+x8664-target
           (and (= (the fixnum (lisptag exp)) x8664::tag-misc)
                (logbitp (the (unsigned-byte 16) (logand type-code x8664::fulltagmask))
                         (logior (ash 1 x8664::fulltag-immheader-0)
                                 (ash 1 x8664::fulltag-immheader-1)
                                 (ash 1 x8664::fulltag-immheader-2))))
           #+arm-target
           (= (the fixnum (logand type-code arm::fulltagmask)) arm::fulltag-immheader)
           (case type-code
             (#.target::subtag-dead-macptr (fasl-unknown exp))
             (#.target::subtag-macptr
              ;; Treat untyped pointers to the high/low 64K of the address
              ;; space as constants.  Refuse to dump other pointers.
              (unless (and (zerop (%macptr-type exp))
                           (<= (%macptr-domain exp) 1))
                (error "Can't dump typed pointer ~s" exp))
              (let* ((addr (%ptr-to-int exp)))
                (unless (or (< addr #x10000)
                            (>= addr (- (ash 1 target::nbits-in-word)
                                        #x10000)))
                  (error "Can't dump pointer ~s : address is not in the low or high 64K of the address space." exp))))
             (t (fasl-scan-ref exp)))
           (case type-code
             ((#.target::subtag-pool #.target::subtag-weak #.target::subtag-lock) (fasl-unknown exp))
             (#+ppc-target #.target::subtag-symbol
              #+x8632-target #.target::subtag-symbol
              #+x8664-target #.target::tag-symbol
              #+arm-target #.target::subtag-symbol (fasl-scan-symbol exp))
             ((#.target::subtag-instance #.target::subtag-struct)
              (fasl-scan-user-form exp))
             (#.target::subtag-package (fasl-scan-ref exp))
             (#.target::subtag-istruct
              (if (memq (istruct-type-name exp) *istruct-make-load-form-types*)
                (progn
                  (if (hash-table-p exp)
                    (fasl-lock-hash-table exp))
                  (fasl-scan-user-form exp))
                (fasl-scan-gvector exp)))
	     #+x8632-target
	     (#.target::subtag-function (fasl-scan-clfun exp))
             #+x8664-target
             (#.target::tag-function (fasl-scan-clfun exp))
             (t (fasl-scan-gvector exp)))))))))
              

(defun fasl-scan-ref (form)
  (puthash form *fasdump-hash* 0))

(defun fasl-scan-fixnum (fixnum)
  (unless (short-fixnum-p fixnum) (fasl-scan-ref fixnum)))

(defparameter *istruct-make-load-form-types*
  '(lexical-environment shared-library-descriptor shared-library-entry-point
    external-entry-point foreign-variable
    ctype unknown-ctype class-ctype foreign-ctype union-ctype member-ctype 
    array-ctype numeric-ctype hairy-ctype named-ctype constant-ctype args-ctype
    hash-table package-ref type-cell class-cell slot-id))




(defun fasl-scan-gvector (vec)
  (fasl-scan-ref vec)
  (dotimes (i (uvsize vec)) 
    (declare (fixnum i))
    (fasl-scan-form (%svref vec i))))

#+x86-target
(defun fasl-scan-clfun (f)
  (let* ((fv (function-to-function-vector f))
         (size (uvsize fv))
         (ncode-words (%function-code-words f)))
    (fasl-scan-ref f)
    (do* ((k ncode-words (1+ k)))
         ((= k size))
      (fasl-scan-form (uvref fv k)))))

(defun funcall-lfun-p (form)
  (and (listp form)
       (eq (%car form) 'funcall)
       (listp (%cdr form))
       (or (functionp (%cadr form))
           (eql (typecode (%cadr form)) target::subtag-xfunction))
       (null (%cddr form))))

;;; We currently represent istruct-cells as conses.  That's not
;;; incredibly efficient (among other things, we have to do this
;;; check when scanning/dumping any list), but it's probably not
;;; worth burning a tag on them.  There are currently about 50
;;; entries on the *istruct-cells* list.
(defun istruct-cell-p (x)
  (and (consp x)
       (typep (%car x) 'symbol)
       (atom (%cdr x))
       (not (null (memq x *istruct-cells*)))))

(defun fasl-scan-list (list)
  (cond ((eq (%car list) cfasl-load-time-eval-sym)
         (let ((form (car (%cdr list))))
           (fasl-scan-form (if (funcall-lfun-p form)
                             (%cadr form)
                             form))))
        ((istruct-cell-p list)
         (fasl-scan-form (%car list)))
        (t (when list
             (fasl-scan-ref list)
             (fasl-scan-form (%car list))
             (fasl-scan-form (%cdr list))))))

(defun fasl-scan-user-form (form)
  (when (or (source-note-p form)
            (code-note-p form))
    (return-from fasl-scan-user-form (fasl-scan-gvector form)))
  (multiple-value-bind (load-form init-form) (make-load-form form *fcomp-load-forms-environment*)
    (labels ((simple-load-form (form)
               (or (atom form)
                   (let ((function (car form)))
                     (or (eq function 'quote)
                         (and (symbolp function)
                              ;; using fboundp instead of symbol-function
                              ;; see comments in symbol-function
                              (or (functionp (fboundp function))
                                  (eq function 'progn))
                              ;; (every #'simple-load-form (cdr form))
                              (dolist (arg (cdr form) t)
                                (unless (simple-load-form arg)
                                  (return nil))))))))
             (load-time-eval-form (load-form form type)
               (cond ((quoted-form-p load-form)
                      (%cadr load-form))
                     ((self-evaluating-p load-form)
                      load-form)
                     ((simple-load-form load-form)
                      `(,cfasl-load-time-eval-sym ,load-form))
                     (t (multiple-value-bind (lfun warnings)
                                             (or
                                              (gethash load-form *make-load-form-hash*)
                                              (fcomp-named-function `(lambda () ,load-form) nil nil))
                          (when warnings
                            (cerror "Ignore the warnings"
                                    "Compiling the ~s ~a form for~%~s~%produced warnings."
                                    'make-load-form type form))
                          (setf (gethash load-form *make-load-form-hash*) lfun)
                          `(,cfasl-load-time-eval-sym (funcall ,lfun)))))))
      (declare (dynamic-extent #'simple-load-form #'load-time-eval-form))
      (let* ((compiled-initform
              (and init-form (load-time-eval-form init-form form "initialization")))
             (info (list (load-time-eval-form load-form form "creation")
                         T              ; scanning-p
                         nil            ; referenced-p
                         compiled-initform  ;initform-info
                         )))
        (puthash form *fasdump-hash* info)
        (fasl-scan-form (%car info))
        (setf (cadr info) nil)        ; no longer scanning load-form
        (when init-form
          (fasl-scan-form compiled-initform))))))

(defun fasl-setf-name-inverse-p (sym &optional create)
  (or (cdr (assoc sym *fasl-setf-name-alias-alist*))
      (and create
           (let* ((pname (symbol-name sym))
                  (namelen (length pname))
                  (setf-for 
                   (and (> namelen 2)
                        (eql (schar pname 0) #\()
                        (eql (schar pname (1- namelen)) #\))
                        (gethash sym %setf-function-name-inverses%))))
           (when setf-for
             (let* ((list `(,cfasl-load-time-eval-sym (setf-function-name ',setf-for))))
               (fasl-scan-list list)
               (push (cons sym list) *fasl-setf-name-alias-alist*)
               list))))))
  
(defun fasl-scan-symbol (form)
  (unless (fasl-setf-name-inverse-p form t)
    (fasl-scan-ref form)
    (fasl-scan-form (symbol-package form))))
  


;;;;          Pass 3 - dumping
;;;;
;;;;
(defvar *fasdump-epush*)
(defvar *fasdump-stream*)
(defvar *fasdump-eref*)

(defun fasl-dump-file (gnames goffsets forms hash filename)
  (let ((opened? nil)
        (finished? nil))
    (unwind-protect
      (with-open-file (*fasdump-stream* filename :direction :output
                                        :element-type '(unsigned-byte 8)
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
        (setq opened? t)
        (fasl-set-filepos 0)
        (fasl-out-word 0)             ;Will become the ID word
        (fasl-out-word 1)             ;One block in the file
        (fasl-out-long 12)            ;Block starts at file pos 12
        (fasl-out-long 0)             ;Length will go here
        (fasl-dump-block gnames goffsets forms hash)  ;Write the block
        (let ((pos (fasl-filepos)))
          (fasl-set-filepos 8)        ;Back to length longword
          (fasl-out-long (- pos 12))) ;Write length
        (fasl-set-filepos 0)          ;Seem to have won, make us legal
        (fasl-out-word FASL-FILE-ID)
        (setq finished? t)
        filename)
      (when (and opened? (not finished?))
        (delete-file filename)))))

(defun target-symbol-value (name)  
  (let* ((package (arch::target-package-name (backend-target-arch *target-backend*)))
         (sym (find-symbol name package)))
    (unless (and sym (boundp sym))
      (error "~a not defined in target package ~s." name package))
    (logior #xff00 (logand #xff (symbol-value sym)))))

;;; This is an exported, public interface.
(defun target-fasl-version ()
  (target-symbol-value "FASL-VERSION"))

;; Redefine the versions of these functions used by the fasloader.

(defun target-fasl-min-version ()
  (target-symbol-value "FASL-MIN-VERSION"))

(defun target-fasl-max-version ()
  (target-symbol-value "FASL-MAX-VERSION"))

(defun fasl-dump-block (gnames goffsets forms hash)
  (let ((etab-size (hash-table-count hash)))
    (fasl-out-word (target-fasl-version))          ; Word 0
    (fasl-out-long  0)
    (fasl-out-byte $fasl-vetab-alloc)
    (fasl-out-count etab-size)
    (fasl-dump gnames goffsets forms hash)
    (fasl-out-byte $fasl-end)))

(defun fasl-dump (gnames goffsets forms hash)
  (let* ((*fasdump-hash* hash)
         (*fasdump-read-package* nil)
         (*fasdump-epush* nil)
         (*fasdump-eref* -1)
         (*fasdump-global-offsets* goffsets))
    (when gnames
      (fasl-out-byte $fasl-globals)
      (fasl-dump-form gnames))
    (dolist (op forms)
      (if (packagep op)
        (setq *fasdump-read-package* op)
        (progn
          (fasl-out-byte (car op))
          (dolist (arg (cdr op)) (fasl-dump-form arg)))))))

;;;During dumping, *fasdump-hash* values are one of the following:
;;;   nil - form has no load form, is referenced at most once.
;;;   fixnum - form has already been dumped, fixnum is the etab index.
;;;   T - form hasn't been dumped yet, is referenced more than once.
;;;  (load-form . nil) - form should be replaced by load-form.
(defun fasl-dump-form (form)
  (let ((info (gethash form *fasdump-hash*)))
    (cond ((fixnump info)
           (fasl-out-byte $fasl-veref)
           (fasl-out-count info))
          ((consp info)
           (fasl-dump-user-form form info))
          (t
           (setq *fasdump-epush* info)
           (fasl-dump-dispatch form)))))

(defun fasl-dump-user-form (form info)
  (let* ((load-form (car info))
         (referenced-p (caddr info))
         (initform (cadddr info)))
    (when referenced-p
      (unless (gethash load-form *fasdump-hash*)
        (error "~s was not in ~s.  This shouldn't happen." 'load-form '*fasdump-hash*)))
    (when initform
      (fasl-out-byte $fasl-prog1))      ; ignore the initform
    (fasl-dump-form load-form)
    (when referenced-p
      (setf (gethash form *fasdump-hash*) (gethash load-form *fasdump-hash*)))
    (when initform
      (fasl-dump-form initform))))

(defun fasl-out-opcode (opcode form)
  (if *fasdump-epush*
    (progn
      (setq *fasdump-epush* nil)
      (fasl-out-byte (fasl-epush-op opcode))
      (fasl-dump-epush form))
    (fasl-out-byte opcode)))

(defun fasl-dump-epush (form)
  #-bccl (when (fixnump (gethash form *fasdump-hash*))
           (error "Bug! Duplicate epush for ~S" form))
  (puthash form *fasdump-hash* (setq *fasdump-eref* (1+ *fasdump-eref*))))


(defun fasl-dump-dispatch (exp)
  (etypecase exp
    ((signed-byte 16) (fasl-dump-s16 exp))
    ((signed-byte 32) (fasl-dump-s32 exp))
    ((signed-byte 64) (fasl-dump-s64 exp))
    (bignum (fasl-dump-32-bit-ivector exp $fasl-bignum32))
    (character (fasl-dump-char exp))
    (list (fasl-dump-list exp))
    (immediate (fasl-dump-t_imm exp))
    (double-float (fasl-dump-dfloat exp))
    (single-float (fasl-dump-sfloat exp))
    (simple-string
     (let* ((nextra (utf-8-extra-bytes exp)))
       (cond ((= 0 nextra)
              (fasl-out-opcode $fasl-nvstr exp)
              (fasl-out-nvstring exp))
             (t (fasl-out-opcode $fasl-vstr exp)
                (fasl-out-vstring exp nextra)))))
    (simple-bit-vector (fasl-dump-bit-vector exp))
    ((simple-array (unsigned-byte 8) (*))
     (fasl-dump-8-bit-ivector exp $fasl-u8-vector))
    ((simple-array (signed-byte 8) (*))
     (fasl-dump-8-bit-ivector exp $fasl-s8-vector))
    ((simple-array (unsigned-byte 16) (*))
     (fasl-dump-16-bit-ivector exp $fasl-u16-vector))
    ((simple-array (signed-byte 16) (*))
     (fasl-dump-16-bit-ivector exp $fasl-s16-vector))
    ((simple-array (unsigned-byte 32) (*))
     (fasl-dump-32-bit-ivector exp $fasl-u32-vector))
    ((simple-array (signed-byte 32) (*))
     (fasl-dump-32-bit-ivector exp $fasl-s32-vector))
    ((simple-array single-float (*))
     (fasl-dump-32-bit-ivector exp $fasl-single-float-vector))
    ((simple-array double-float (*))
     (fasl-dump-double-float-vector exp))
    (symbol (fasl-dump-symbol exp))
    (package (fasl-dump-package exp))
    (function (fasl-dump-function exp))
    (xfunction (fasl-dump-function exp))
    (code-vector (fasl-dump-codevector exp))
    (xcode-vector (fasl-dump-codevector exp))
    (simple-vector (fasl-dump-gvector exp $fasl-t-vector))
    (ratio (fasl-dump-ratio exp))
    (complex (fasl-dump-complex exp))
    #+(and 64-bit-target (not cross-compiling))
    ((simple-array (unsigned-byte 64) (*))
     (fasl-dump-64-bit-ivector exp $fasl-u64-vector))
    #+(and 64-bit-target (not cross-compiling))
    ((simple-array (signed-byte 64) (*))
     (fasl-dump-64-bit-ivector exp $fasl-s64-vector))
    (ivector
     (unless (eq (backend-target-arch-name *target-backend*)
                 (backend-target-arch-name *host-backend*))
       (error "can't cross-compile constant reference to ~s" exp))
     (let* ((typecode (typecode exp))
            (n (uvsize exp))
            (nb (subtag-bytes typecode n)))
       (declare (fixnum n nb typecode))
       (fasl-out-opcode $fasl-vivec exp)
       (fasl-out-byte typecode)
       (fasl-out-count n)
       (fasl-out-ivect exp 0 nb)))
    (vector (fasl-dump-gvector exp $fasl-vector-header))
    (array (fasl-dump-gvector exp $fasl-array-header))

    (gvector
     (if (= (typecode exp) target::subtag-istruct)
       (fasl-dump-gvector exp $fasl-istruct)
       (progn
         (unless (eq (backend-target-arch-name *target-backend*)
                     (backend-target-arch-name *host-backend*))
           (error "can't cross-compile constant reference to ~s" exp))
         (let* ((typecode (typecode exp))
                (n (uvsize exp)))
           (declare (fixnum n typecode))
           (fasl-out-opcode $fasl-vgvec exp)
           (fasl-out-byte typecode)
           (fasl-out-count n)
           (dotimes (i n)
             (fasl-dump-form (%svref exp i)))))))))

(defun fasl-dump-gvector (v op)
  (let* ((n (uvsize v)))
    (fasl-out-opcode op v)
    (fasl-out-count n)
    (dotimes (i n)
      (fasl-dump-form (%svref v i)))))

(defun fasl-dump-ratio (v)
  (fasl-out-opcode $fasl-ratio v)
  (fasl-dump-form (%svref v target::ratio.numer-cell))
  (fasl-dump-form (%svref v target::ratio.denom-cell)))

(defun fasl-dump-complex (v)
  (fasl-out-opcode $fasl-complex v)  
  (fasl-dump-form (realpart v))
  (fasl-dump-form (imagpart v)))

(defun fasl-dump-bit-vector (v)
  (let* ((n (uvsize v)))
    (fasl-out-opcode $fasl-bit-vector v)
    (fasl-out-count n)
    (if (eq *fasl-host-big-endian* *fasl-target-big-endian*)
      (let* ((nb (ash (+ n 7) -3)))
        (fasl-out-ivect v 0 nb))
      (compiler-bug "need to byte-swap ~a" v))))

(defun fasl-dump-8-bit-ivector (v op)
  (let* ((n (uvsize v)))
    (fasl-out-opcode op v)
    (fasl-out-count n)
    (let* ((nb n))
      (fasl-out-ivect v 0 nb))))

(defun fasl-dump-16-bit-ivector (v op)
  (let* ((n (uvsize v)))
    (fasl-out-opcode op v)
    (fasl-out-count n)
    (if (eq *fasl-host-big-endian* *fasl-target-big-endian*)
      (let* ((nb (ash n 1)))
        (fasl-out-ivect v 0 nb))
      (dotimes (i n)
        (let* ((k (uvref v i)))
          (fasl-out-byte (ldb (byte 8 0) k))
          (fasl-out-byte (ldb (byte 8 8) k)))))))

(defun fasl-dump-32-bit-ivector (v op)
  (let* ((n (uvsize v)))
    (fasl-out-opcode op v)
    (fasl-out-count n)
    (if (eq *fasl-host-big-endian* *fasl-target-big-endian*)
      (let* ((nb (ash n 2)))
        (fasl-out-ivect v 0 nb))
      (dotimes (i n)
        (let* ((k (uvref v i)))
          (fasl-out-byte (ldb (byte 8 0) k))
          (fasl-out-byte (ldb (byte 8 8) k))
          (fasl-out-byte (ldb (byte 8 16) k))
          (fasl-out-byte (ldb (byte 8 24) k)))))))


(defun fasl-dump-64-bit-ivector (v op)
  (let* ((n (uvsize v)))
    (fasl-out-opcode op v)
    (fasl-out-count n)
    (if (eq *fasl-host-big-endian* *fasl-target-big-endian*)
      (let* ((nb (ash n 3)))
        (fasl-out-ivect v 0 nb))
      (compiler-bug "need to byte-swap ~a" v))))

(defun fasl-dump-double-float-vector (v)
  (let* ((n (uvsize v)))
    (fasl-out-opcode $fasl-double-float-vector v)
    (fasl-out-count n)
    (if (eq *fasl-host-big-endian* *fasl-target-big-endian*)
      (let* ((nb (ash n 3)))
        (fasl-out-ivect v (- target::misc-dfloat-offset
                             target::misc-data-offset) nb))
      (compiler-bug "need to byte-swap ~a" v))))

;;; This is used to dump functions and "xfunctions".
;;; If we're cross-compiling, we shouldn't reference any
;;; (host) functions as constants; try to detect that
;;; case.
#-x86-target
(defun fasl-dump-function (f)
  (if (and (not (eq *fasl-backend* *host-backend*))
           (typep f 'function))
    (compiler-bug "Dumping a native function constant ~s during cross-compilation." f))
  (if (and (= (typecode f) target::subtag-xfunction)
           (= (typecode (uvref f 0)) target::subtag-u8-vector))
    (fasl-xdump-clfun f)
    (let* ((n (uvsize f)))
      (fasl-out-opcode $fasl-function f)
      (fasl-out-count n)
      (dotimes (i n)
        (if (= i 0)
          (target-arch-case
           (:arm (fasl-dump-form 0))
           (t (fasl-dump-form (%svref f i))))
          (fasl-dump-form (%svref f i)))))))

#+x86-target
(defun fasl-dump-function (f)
  (if (and (not (eq *fasl-backend* *host-backend*))
           (typep f 'function))
    (compiler-bug "Dumping a native function constant ~s during cross-compilation." f))
  (if (and (= (typecode f) target::subtag-xfunction)
           (= (typecode (uvref f 0)) target::subtag-u8-vector))
    (fasl-xdump-clfun f)
    (if (= (typecode f) target::subtag-xfunction)
      (let* ((n (uvsize f)))
        (fasl-out-opcode $fasl-function f)
        (fasl-out-count n)
        (dotimes (i n)
          (fasl-dump-form (%svref f i))))        

      (let* ((code-size (%function-code-words f))
             (function-vector (function-to-function-vector f))
             (function-size (uvsize function-vector)))
        (fasl-out-opcode $fasl-clfun f)
        (fasl-out-count function-size)
        (fasl-out-count code-size)
        (fasl-out-ivect function-vector 0 (ash code-size target::word-shift))
        (do* ((k code-size (1+ k)))
             ((= k function-size))
          (declare (fixnum k))
          (fasl-dump-form (uvref function-vector k)))))))
        

  

;;; Write a "concatenated function".
(defun fasl-xdump-clfun (f)
  (target-arch-case
   (:x8632
    (let* ((code (uvref f 0))
	   (function-size (ash (uvsize code) -2))
	   (encoded-imm-words (dpb (uvref code 1) (byte 8 8) (uvref code 0)))
	   (imm-words (if (logbitp 15 encoded-imm-words)
			(- function-size (ldb (byte 15 0) encoded-imm-words))
			encoded-imm-words))
	   (imm-bytes (ash imm-words 2))
	   (other-words (- function-size imm-words)))
      (assert (= other-words (1- (uvsize f))))
      (fasl-out-opcode $fasl-clfun f)
      (fasl-out-count function-size)
      (fasl-out-count imm-words)
      (fasl-out-ivect code 0 imm-bytes)
      (do ((i 1 (1+ i))
	   (n (uvsize f)))
	  ((= i n))
	(declare (fixnum i n))
	(fasl-dump-form (%svref f i)))))
   (:x8664
    (let* ((code (uvref f 0))
	   (code-size (dpb (uvref code 3)
			   (byte 8 24)
			   (dpb (uvref code 2)
				(byte 8 16)
				(dpb (uvref code 1)
				     (byte 8 8)
				     (uvref code 0)))))
	   (function-size (ash (uvsize code) -3)))
      (assert (= (- function-size code-size) (1- (uvsize f))))
      (fasl-out-opcode $fasl-clfun f)
      (fasl-out-count function-size)
      (fasl-out-count code-size)
      (fasl-out-ivect code 0 (ash code-size 3))
      (do* ((i 1 (1+ i))
	    (n (uvsize f)))
	   ((= i n))
	(declare (fixnum i n))
	(fasl-dump-form (%svref f i)))))))

(defun fasl-dump-codevector (c)
  (if (and (not (eq *fasl-backend* *host-backend*))
           (typep c 'code-vector))
    (compiler-bug "Dumping a native code-vector constant ~s during cross-compilation." c))
  (let* ((n (uvsize c)))
    (fasl-out-opcode $fasl-code-vector c)
    (fasl-out-count n)
    (fasl-out-ivect c)))

(defun fasl-dump-t_imm (imm)
  (fasl-out-opcode $fasl-timm imm)
  (fasl-out-long (%address-of imm)))

(defun fasl-dump-char (char)     ; << maybe not
  (let ((code (%char-code char)))
    (fasl-out-opcode $fasl-char char)
    (fasl-out-count code)))

;;; Always write big-endian.
(defun fasl-dump-s16 (s16)
  (fasl-out-opcode $fasl-word-fixnum s16)
  (fasl-out-word s16))

;;; Always write big-endian
(defun fasl-dump-s32 (s32)
  (fasl-out-opcode $fasl-s32 s32)
  (fasl-out-word (ldb (byte 16 16) s32))
  (fasl-out-word (ldb (byte 16 0) s32)))

;;; Always write big-endian
(defun fasl-dump-s64 (s64)
  (fasl-out-opcode $fasl-s64 s64)
  (fasl-out-word (ldb (byte 16 48) s64))
  (fasl-out-word (ldb (byte 16 32) s64))
  (fasl-out-word (ldb (byte 16 16) s64))
  (fasl-out-word (ldb (byte 16 0) s64)))



(defun fasl-dump-dfloat (float)
  (fasl-out-opcode $fasl-dfloat float)
  (multiple-value-bind (high low) (double-float-bits float)
    (fasl-out-long high)
    (fasl-out-long low)))

(defun fasl-dump-sfloat (float)
  (fasl-out-opcode $fasl-sfloat float)
  (fasl-out-long (single-float-bits float)))


(defun fasl-dump-package (pkg)
  (let* ((name (package-name pkg))
         (nextra (utf-8-extra-bytes name)))
    (cond ((eql nextra 0)
           (fasl-out-opcode $fasl-nvpkg pkg)
           (fasl-out-nvstring name))
          (t
           (fasl-out-opcode $fasl-vpkg pkg)
           (fasl-out-vstring name nextra)))))



(defun fasl-dump-list (list)
  (cond ((null list) (fasl-out-opcode $fasl-nil list))
        ((eq (%car list) cfasl-load-time-eval-sym)
         (let* ((form (car (%cdr list)))
                (opcode $fasl-eval))
           (when (funcall-lfun-p form)
             (setq opcode $fasl-lfuncall
                   form (%cadr form)))
           (if *fasdump-epush*
             (progn
               (fasl-out-byte (fasl-epush-op opcode))
               (fasl-dump-form form)
               (fasl-dump-epush list))
             (progn
               (fasl-out-byte opcode)
               (fasl-dump-form form)))))
        ((istruct-cell-p list)
         (fasl-out-opcode $fasl-istruct-cell (car list))
         (fasl-dump-symbol (car list)))
        (t (fasl-dump-cons list))))

(defun fasl-dump-cons (cons &aux (end cons) (cdr-len 0))
  (declare (fixnum cdr-len))
  (while (and (consp (setq end (%cdr end)))
              (null (gethash end *fasdump-hash*)))
    (incf cdr-len))
  (if (eql 0 cdr-len)
    (fasl-out-opcode $fasl-cons cons)
    (progn
      (fasl-out-opcode (if end $fasl-vlist* $fasl-vlist) cons)
      (fasl-out-count cdr-len)))
  (dotimes (i (the fixnum (1+ cdr-len)))
    (fasl-dump-form (%car cons))
    (setq cons (%cdr cons)))
  (when (or (eql 0 cdr-len) end)      ;cons or list*
    (fasl-dump-form end)))



(defun fasl-dump-symbol (sym)
  (let* ((inverse (fasl-setf-name-inverse-p sym)))
    (if inverse
      (fasl-dump-form inverse)
      (let* ((pkg (symbol-package sym))
             (name (symbol-name sym))
             (nextra (utf-8-extra-bytes name))
             (ascii (eql nextra 0))
             (idx (let* ((i (%svref (symptr->symvector (%symbol->symptr sym)) target::symbol.binding-index-cell)))
                    (declare (fixnum i))
                    (unless (zerop i) i))))
        (cond ((null pkg) 
               (progn 
                 (fasl-out-opcode (if idx
                                    (if ascii $fasl-nvmksym-special $fasl-vmksym-special)
                                    (if ascii $fasl-nvmksym $fasl-vmksym))
                                  sym)
                 (if ascii
                   (fasl-out-nvstring name)
                   (fasl-out-vstring name nextra))))
              (*fasdump-epush*
               (progn
                 (fasl-out-byte (fasl-epush-op (if idx
                                                 (if ascii $fasl-nvpkg-intern-special $fasl-vpkg-intern-special)
                                                 (if ascii $fasl-nvpkg-intern $fasl-vpkg-intern))))
                 (fasl-dump-form pkg)
                 (fasl-dump-epush sym)
                 (if ascii
                   (fasl-out-nvstring name)
                   (fasl-out-vstring name nextra))))
              (t
               (progn
                 (fasl-out-byte (if idx
                                  (if ascii $fasl-nvpkg-intern-special $fasl-vpkg-intern-special)
                                  (if ascii $fasl-nvpkg-intern $fasl-vpkg-intern)))
                 (fasl-dump-form pkg)
                 (if ascii
                   (fasl-out-nvstring name)
                   (fasl-out-vstring name nextra)))))))))


(defun fasl-unknown (exp)
  (error "Can't dump ~S - unknown type" exp))

(defun fasl-out-simple-string (str start end)
  (declare (simple-string str) (fixnum start end))
  (do* ((k start (1+ k)))
       ((= k end))
    (declare (fixnum k))
    (fasl-out-byte (char-code (schar str k)))))

(defun fasl-out-nvstring (str)
  (fasl-out-count (length str))
  (fasl-out-simple-string str 0 (length str)))

(defun utf-8-extra-bytes (string)
  (declare (simple-string string))
  (let* ((extra 0))
    (declare (fixnum extra))
    (dotimes (i (length string) extra)
      (let* ((code (%scharcode string i)))
        (declare ((mod #x110000) code))
        (cond ((>= code #x10000) (incf extra 3))
              ((>= code #x800) (incf extra 2))
              ((>= code #x80) (incf extra 1)))))))

(defun fasl-out-vstring (str nextra)
  (declare (fixnum nextra))
  (let* ((len (length str)))
    (declare (fixnum len))
    (fasl-out-count len)
    (fasl-out-count nextra)
    (dotimes (i len)
      (let* ((code (%scharcode str i)))
        (declare ((mod #x110000) code))
        (cond ((< code #x80) (fasl-out-byte code))
              ((< code #x800)
               (let* ((y (ldb (byte 5 6) code))
                      (z (ldb (byte 6 0) code)))
                 (declare (fixnum y z))
                 (fasl-out-byte (logior #xc0 y))
                 (fasl-out-byte (logior #x80 z))))
              ((< code #x10000)
               (let* ((x (ldb (byte 4 12) code))
                      (y (ldb (byte 6 6) code))
                      (z (ldb (byte 6 0) code)))
                 (declare (fixnum x y z))
                 (fasl-out-byte (logior #xe0 x))
                 (fasl-out-byte (logior #x80 y))
                 (fasl-out-byte (logior #x80 z))))
              (t
                (let* ((w (ldb (byte 3 18) code))
                       (x (ldb (byte 6 12) code))
                       (y (ldb (byte 6 6) code))
                       (z (ldb (byte 6 0) code)))
                  (declare (fixnum w x y z))
                  (fasl-out-byte (logior #xf0 w))
                  (fasl-out-byte (logior #x80 x))
                  (fasl-out-byte (logior #x80 y))
                  (fasl-out-byte (logior #x80 z)))))))))


(defun fasl-out-ivect (iv &optional 
                          (start 0) 
                          (nb 
			   (subtag-bytes (typecode iv) (uvsize iv))))
  (stream-write-ivector *fasdump-stream* iv start nb))


(defun fasl-out-long (long)
  (fasl-out-word (ash long -16))
  (fasl-out-word (logand long #xFFFF)))

(defun fasl-out-word (word)
  (fasl-out-byte (ash word -8))
  (fasl-out-byte word))

(defun fasl-out-byte (byte)
  (write-byte (%ilogand2 byte #xFF) *fasdump-stream*))

;;; Write an unsigned integer in 7-bit chunks.
(defun fasl-out-count (val)
  (do* ((b (ldb (byte 7 0) val) (ldb (byte 7 0) val))
        (done nil))
       (done)
    (when (zerop (setq val (ash val -7)))
      (setq b (logior #x80 b) done t))
    (fasl-out-byte b)))

(defun fasl-filepos ()
  (file-position *fasdump-stream*))

(defun fasl-set-filepos (pos)
  (file-position *fasdump-stream* pos)
  #-bccl (unless (eq (file-position *fasdump-stream*) pos)
           (error "Unable to set file position to ~S" pos)))

;;; Concatenate fasl files.

;;; Format of a fasl file as expected by the fasloader.
;;;
;;; #xFF00         2 bytes - File version
;;; Block Count    2 bytes - Number of blocks in the file
;;; addr[0]        4 bytes - address of 0th block
;;; length[0]      4 bytes - length of 0th block
;;; addr[1]        4 bytes - address of 1st block
;;; length[1]      4 bytes - length of 1st block
;;; ...
;;; addr[n-1]      4 bytes
;;; length[n-1]    4 bytes
;;; length[0] + length[1] + ... + length [n-1] bytes of data

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (fasl-concatenate out-file fasl-files &key :if-exists)
;;
;; out-file     name of file in which to store the concatenation
;; fasl-files   list of names of fasl files to concatenate
;; if-exists    as for OPEN, defaults to :error
;;
;; function result: pathname to the output file.
;; It works to use the output of one invocation of fasl-concatenate
;; as an input of another invocation.
;;
(defun fasl-concatenate (out-file fasl-files &key (if-exists :error))
  (%fasl-concatenate out-file fasl-files if-exists (pathname-type *.fasl-pathname*)))

(defun %fasl-concatenate (out-file fasl-files if-exists file-ext)
  (let ((count 0)
        (created? nil)
        (finished? nil)
	(ext-pathname (make-pathname :type file-ext)))
    (declare (fixnum count))
    (flet ((fasl-read-halfword (f)
	     (dpb (read-byte f) (byte 8 8) (read-byte f)))
	   (fasl-write-halfword (h f)
	     (write-byte (ldb (byte 8 8) h) f)
	     (write-byte (ldb (byte 8 0) h) f)
	     h))
      (flet ((fasl-read-fullword (f)
	       (dpb (fasl-read-halfword f) (byte 16 16) (fasl-read-halfword f)))
	     (fasl-write-fullword (w f)
	       (fasl-write-halfword (ldb (byte 16 16) w) f)
	       (fasl-write-halfword (ldb (byte 16 0) w) f)
	       w))
	(dolist (file fasl-files)
	  (setq file (merge-pathnames file ext-pathname))
	  (unless (equal (pathname-type file) file-ext)
	    (error "Not a ~A file: ~s" file-ext file))
	  (with-open-file (instream file :element-type '(unsigned-byte 8))
	    (unless (eql fasl-file-id (fasl-read-halfword instream))
	      (error "Bad ~A file ID in ~s" file-ext file))
	    (incf count (fasl-read-halfword instream))))
	(unwind-protect
	     (with-open-file (outstream
			      (setq out-file (merge-pathnames out-file ext-pathname))
			      :element-type '(unsigned-byte 8)
			      :direction :output
			      :if-does-not-exist :create
			      :if-exists if-exists)
	       (setq created? t)
	       (let ((addr-address 4)
		     (data-address (+ 4 (* count 8))))
		 (fasl-write-halfword 0 outstream) ;  will be $fasl-id
		 (fasl-write-halfword count outstream)
		 (dotimes (i (* 2 count))
		   (fasl-write-fullword 0 outstream)) ; for addresses/lengths
		 (dolist (file fasl-files)
		   (with-open-file (instream (merge-pathnames file ext-pathname)
					     :element-type '(unsigned-byte 8))
		     (fasl-read-halfword instream) ; skip ID
		     (let* ((fasl-count (fasl-read-halfword instream))
			    (addrs (make-array fasl-count))
			    (sizes (make-array fasl-count))
			    addr0)
		       (declare (fixnum fasl-count)
				(dynamic-extent addrs sizes))
		       (dotimes (i fasl-count)
			 (setf (svref addrs i) (fasl-read-fullword instream)
			       (svref sizes i) (fasl-read-fullword instream)))
		       (setq addr0 (svref addrs 0))
		       (file-position outstream addr-address)
		       (dotimes (i fasl-count)
			 (fasl-write-fullword
			  (+ data-address (- (svref addrs i) addr0))
			  outstream)
			 (fasl-write-fullword (svref sizes i) outstream)
			 (incf addr-address 8))
		       (file-position outstream data-address)
		       (dotimes (i fasl-count)
			 (file-position instream (svref addrs i))
			 (let ((fasl-length (svref sizes i)))
			   (dotimes (j fasl-length)
			     (write-byte (read-byte instream) outstream))
			   (incf data-address fasl-length))))))
		 (stream-length outstream data-address)
		 (file-position outstream 0)
		 (fasl-write-halfword fasl-file-id outstream)
		 (setq finished? t)))
	  (when (and created? (not finished?))
	    (delete-file out-file))))
      out-file)))

;;; Cross-compilation environment stuff.  Some of this involves
;;; setting up the TARGET and OS packages.
(defun ensure-package-nickname (name package)
  (let* ((old (find-package name)))
    (unless (eq old package)
      (rename-package old (package-name old) (delete name (package-nicknames old) :test #'string=))
      (rename-package package (package-name package) (cons name (package-nicknames package)))
      old)))

(defmacro with-cross-compilation-package ((name target) &body body)
  (let* ((old-package (gensym))
         (name-var (gensym))
         (target-var (gensym)))
    `(let* ((,name-var ,name)
            (,target-var ,target)
            (,old-package (ensure-package-nickname ,name-var ,target-var)))
      (unwind-protect
           (progn ,@body)
        (when ,old-package (ensure-package-nickname ,name-var
                                                          ,old-package))))))

(defun %with-cross-compilation-target (target thunk)
  (let* ((backend (find-backend target)))
    (if (null backend)
      (error "No known compilation target named ~s." target)
      (let* ((arch (backend-target-arch backend))
             (arch-package-name (arch::target-package-name arch))
             (ftd (or (backend-target-foreign-type-data backend)
                      (error "No foreign type data loaded for target named ~s."
                             target)))
             (ftd-package-name (ftd-interface-package-name ftd)))
        (or (find-package arch-package-name)
            (make-package arch-package-name))
        (or (find-package ftd-package-name)
            (make-package ftd-package-name :use "COMMON-LISP"))
        (with-cross-compilation-package ("OS" ftd-package-name)
          (with-cross-compilation-package ("TARGET" arch-package-name)
            (let* ((*target-ftd* ftd))
               (funcall thunk))))))))

(defmacro with-cross-compilation-target ((target) &body body)
  `(%with-cross-compilation-target ,target #'(lambda () ,@body)))
             

  

(provide 'nfcomp)

