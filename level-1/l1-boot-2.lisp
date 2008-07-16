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

;; l1-boot-2.lisp
;; Second part of l1-boot

(in-package "CCL")

(macrolet ((l1-load (name)
	     (let* ((namestring
		     (concatenate 'simple-base-string
                                  "./l1-fasls/"
				  (string name)
                                  (namestring (backend-target-fasl-pathname
                                               *target-backend*)))))
       `(let* ((*loading-file-source-file* *loading-file-source-file*))
                 (%fasload ,namestring))))
	   (bin-load (name)
	     (let* ((namestring
		     (concatenate 'simple-base-string
                                  "./bin/"
				  (string name)
                                  (namestring (backend-target-fasl-pathname
                                               *target-backend*)))))
               `(let* ((*loading-file-source-file* *loading-file-source-file*))
                 (%fasload ,namestring)))))


(catch :toplevel
    #+ppc-target
    (l1-load "ppc-error-signal")
    #+x86-target
    (l1-load "x86-error-signal")
    (l1-load "l1-error-signal")
    (l1-load "l1-sockets")
    (setq *LEVEL-1-LOADED* t))

#+ppc-target
(defun altivec-available-p ()
  "Return non-NIL if AltiVec is available."
  (not (eql (%get-kernel-global 'ppc::altivec-present) 0)))

#+ppc-target
(defloadvar *altivec-available* (altivec-available-p)
  "This variable is intitialized each time an OpenMCL session starts based
on information provided by the lisp kernel. Its value is true if AltiVec is
present and false otherwise. This variable shouldn't be set by user code.")

       
(defstatic *auto-flush-streams* ())
(def-ccl-pointers *auto-flush-streams* () (setq *auto-flush-streams* nil))
(defstatic *auto-flush-streams-lock* (make-lock))


(defloadvar *batch-flag* (not (eql (%get-kernel-global 'batch-flag) 0)))
(defloadvar *quiet-flag* nil)
(defvar *terminal-input* ())
(defvar *terminal-output* ())
(defvar *stdin* ())
(defvar *stdout* ())
(defvar *stderr* ())


(defun set-basic-stream-prototype (class)
  (when (subtypep class 'basic-stream)
    (setf (%class.prototype class) (or (%class.prototype class)
                                       (allocate-basic-stream class)))
    (dolist (subclass (class-direct-subclasses class))
      (set-basic-stream-prototype subclass))))

(set-basic-stream-prototype (find-class 'basic-stream))


;;; The hard parts here have to do with setting up *TERMINAL-IO*.
;;; Note that opening /dev/tty can fail, and that failure would
;;; be reported as a negative return value from FD-OPEN.
;;; It's pretty important that nothing signals an error here,
;;; since there may not be any valid streams to write an error
;;; message to.

(defloadvar *interactive-streams-initialized* nil)

(defun initialize-interactive-streams ()
  (let* ((encoding (lookup-character-encoding *terminal-character-encoding-name*))
         (encoding-name (if encoding (character-encoding-name encoding))))
    (setq *stdin* (make-fd-stream 0
                                  :basic t
                                  :sharing :lock
                                  :direction :input
                                  :interactive (not *batch-flag*)
                                  :encoding encoding-name))
    (setq *stdout* (make-fd-stream 1 :basic t :direction :output :sharing :lock :encoding encoding-name))

    (setq *stderr* (make-fd-stream 2 :basic t :direction :output :sharing :lock :encoding encoding-name))
    (if *batch-flag*
      (let* ((tty-fd (let* ((fd (fd-open "/dev/tty" #$O_RDWR)))
                       (if (>= fd 0) fd)))
             (can-use-tty (and tty-fd (eql (tcgetpgrp tty-fd) (getpid)))))
        (if can-use-tty
          (setq
           *terminal-input* (make-fd-stream tty-fd
                                            :basic t
                                            :direction :input
                                            :interactive t
                                            :sharing :lock
                                            :encoding encoding-name)
           *terminal-output* (make-fd-stream tty-fd :basic t :direction :output :sharing :lock :encoding encoding-name)
           *terminal-io* (make-echoing-two-way-stream
                          *terminal-input* *terminal-output*))
          (progn
            (when tty-fd (fd-close tty-fd))
            (setq *terminal-input* *stdin*
                  *terminal-output* *stdout*
                  *terminal-io* (make-two-way-stream
                                 *terminal-input* *terminal-output*))))
        (setq *standard-input* *stdin*
              *standard-output* *stdout*))
      (progn
        (setq *terminal-input* *stdin*
              *terminal-output* *stdout*
              *terminal-io* (make-echoing-two-way-stream
                             *terminal-input* *terminal-output*))
        (setq *standard-input* (make-synonym-stream '*terminal-io*)
              *standard-output* (make-synonym-stream '*terminal-io*))))
    (setq *error-output* (if *batch-flag*
                           (make-synonym-stream '*stderr*)
                           (make-synonym-stream '*terminal-io*)))
    (setq *query-io* (make-synonym-stream '*terminal-io*))
    (setq *debug-io* *query-io*)
    (setq *trace-output* *standard-output*)
    (push *stdout* *auto-flush-streams*)
    (setf (input-stream-shared-resource *terminal-input*)
          (make-shared-resource "Shared Terminal Input")))
  (setq *interactive-streams-initialized* t))

(initialize-interactive-streams)

(def-standard-initial-binding *standard-input*)
(def-standard-initial-binding *standard-output*)
(def-standard-initial-binding *error-output*)
(def-standard-initial-binding *trace-output*)
(def-standard-initial-binding *debug-io*)
(def-standard-initial-binding *query-io*)




(catch :toplevel
    (macrolet ((l1-load-provide (module path)
		 `(let* ((*package* *package*))
		   (l1-load ,path)
		   (provide ,module)))
	       (bin-load-provide (module path)
		 `(let* ((*package* *package*))
		   (bin-load ,path)
		   (provide ,module))))
      (bin-load-provide "SORT" "sort")
      (bin-load-provide "NUMBERS" "numbers")
      
      (bin-load-provide "SUBPRIMS" "subprims")
      #+ppc32-target
      (bin-load-provide "PPC32-ARCH" "ppc32-arch") 
      #+ppc64-target
      (bin-load-provide "PPC64-ARCH" "ppc64-arch")
      #+x8664-target
      (bin-load-provide "X8664-ARCH" "x8664-arch")
      (bin-load-provide "VREG" "vreg")
      
      #+ppc-target
      (bin-load-provide "PPC-ASM" "ppc-asm")
      
      (bin-load-provide "VINSN" "vinsn")
      (bin-load-provide "REG" "reg")
      
      #+ppc-target
      (bin-load-provide "PPC-LAP" "ppc-lap")
      (bin-load-provide "BACKEND" "backend")
     
      #+ppc-target
      (provide "PPC2")                  ; Lie, load the module manually

      #+x86-target
      (provide "X862")
      
      (l1-load-provide "NX" "nx")
      
      #+ppc-target
      (bin-load "ppc2")

      #+x86-target
      (bin-load "x862")
      
      (bin-load-provide "LEVEL-2" "level-2")
      (bin-load-provide "MACROS" "macros")
      (bin-load-provide "SETF" "setf")
      (bin-load-provide "SETF-RUNTIME" "setf-runtime")
      (bin-load-provide "FORMAT" "format")
      (bin-load-provide "STREAMS" "streams")
      (bin-load-provide "OPTIMIZERS" "optimizers")      
      (bin-load-provide "DEFSTRUCT-MACROS" "defstruct-macros")
      (bin-load-provide "DEFSTRUCT-LDS" "defstruct-lds")
      (bin-load-provide "NFCOMP" "nfcomp")
      (bin-load-provide "BACKQUOTE" "backquote")
      (bin-load-provide "BACKTRACE-LDS" "backtrace-lds")
      (bin-load-provide "BACKTRACE" "backtrace")
      (bin-load-provide "READ" "read")
      (bin-load-provide "ARRAYS-FRY" "arrays-fry")
      (bin-load-provide "APROPOS" "apropos")
      
      #+ppc-target
      (progn
	(bin-load-provide "PPC-DISASSEMBLE" "ppc-disassemble")
	(bin-load-provide "PPC-LAPMACROS" "ppc-lapmacros"))

      #+x86-target
      (progn
	(bin-load-provide "X86-DISASSEMBLE" "x86-disassemble")
	(bin-load-provide "X86-LAPMACROS" "x86-lapmacros"))
      

      (bin-load-provide "FOREIGN-TYPES" "foreign-types")
      (install-standard-foreign-types *host-ftd*)
      
      #+(and ppc32-target linux-target)
      (bin-load-provide "FFI-LINUXPPC32" "ffi-linuxppc32")
      #+(and ppc32-target darwin-target)
      (bin-load-provide "FFI-DARWINPPC32" "ffi-darwinppc32")
      #+(and ppc64-target darwin-target)
      (bin-load-provide "FFI-DARWINPPC64" "ffi-darwinppc64")
      #+(and ppc64-target linux-target)
      (bin-load-provide "FFI-LINUXPPC64" "ffi-linuxppc64")
      #+(and x8664-target linux-target)  
      (bin-load-provide "FFI-LINUXX8664" "ffi-linuxx8664")
      #+(and x8664-target darwin-target)  
      (bin-load-provide "FFI-DARWINX8664" "ffi-darwinx8664")
      #+(and x8664-target freebsd-target)  
      (bin-load-provide "FFI-FREEBSDX8664" "ffi-freebsdx8664")
      #+(and x8664-target solaris-target)
      (bin-load-provide "FFI-SOLARISX8664" "ffi-solarisx8664")
      
      (bin-load-provide "DB-IO" "db-io")

      (canonicalize-foreign-type-ordinals *host-ftd*)
      
      (bin-load-provide "CASE-ERROR" "case-error")
      (bin-load-provide "ENCAPSULATE" "encapsulate")
      (bin-load-provide "METHOD-COMBINATION" "method-combination")
      (bin-load-provide "MISC" "misc")
      (bin-load-provide "PPRINT" "pprint")
      (bin-load-provide "DUMPLISP" "dumplisp")
      (bin-load-provide "PATHNAMES" "pathnames")
      (bin-load-provide "TIME" "time")
      (bin-load-provide "COMPILE-CCL" "compile-ccl")
      (bin-load-provide "ARGLIST" "arglist")
      (bin-load-provide "EDIT-CALLERS" "edit-callers")
      (bin-load-provide "DESCRIBE" "describe")
      (bin-load-provide "SOURCE-FILES" "source-files")
      (bin-load-provide "MCL-COMPAT" "mcl-compat")
      (require "LOOP")
      (bin-load-provide "CCL-EXPORT-SYMS" "ccl-export-syms")
      (l1-load-provide "VERSION" "version")
      (require "LISPEQU") ; Shouldn't need this at load time ...
      )
    (setq *%fasload-verbose* nil)
    )
)






