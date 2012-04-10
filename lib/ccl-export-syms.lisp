;;-*-Mode: LISP; Package: CCL -*-
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export				;remember "CCL" at end of list
					;setq %ccl-package-export-syms
   '(
     local
     set-local
     @
     *elements-per-buffer*
     save-application
     def-load-pointers
     *save-exit-functions*
     *restore-lisp-functions*
     *lisp-cleanup-functions*
     *lisp-startup-functions*
     defloadvar
     defstatic
     defstaticvar
     *break-on-warnings*
					; misc
     record-source-file
     get-source-files
     edit-definition
     edit-definition-p
     *loading-file-source-file*
     find-definition-sources
     define-definition-type
     definition-type
     definition-type-name
     *save-source-locations*
     function-source-note
     source-note
     source-note-p
     source-note-filename
     source-note-start-pos
     source-note-end-pos
     source-note-text
     ensure-source-note-text
     *record-pc-mapping*
     find-source-note-at-pc
     caller-functions
     *svn-program*
     watch
     unwatch

     show-documentation
     %set-toplevel
     toplevel-loop
     toplevel-function
     repl-function-name
     toplevel
     *listener-prompt-format*
     cancel
     catch-cancel
     throw-cancel
     *backtrace-on-break*
     *show-restarts-on-break*
     print-call-history
     dbg-form
     *backtrace-print-level*
     *backtrace-print-length*
     *backtrace-show-internal-frames*
     *backtrace-format*
     map-call-frames
     frame-function
     frame-supplied-arguments
     frame-named-variables
     apply-in-frame
     *quit-on-eof*
     *quit-interrupt-hook*
     *break-hook*
     *top-error-frame*
     *select-interactive-process-hook*
     interrupt-signal-condition
     macroexpand-all
     compiler-macroexpand
     compiler-macroexpand-1
     compile-user-function
     uncompile-function
     report-compiler-warning
     compiler-warning
     style-warning
     compiler-warning-source-note
     compiler-warning-function-name
     *merge-compiler-warnings*
     abort-break
     *trace-print-level*
     *trace-print-length*
     *trace-bar-frequency*
     trace-function
     *ignore-extra-close-parenthesis*
     advise
     unadvise
     advisedp
     nfunction
     function-name
     setf-function-p
     setf-function-spec-name
     name-of

     assq
     bignump
     bitp
     constant-symbol-p
     proclaimed-special-p
     delq
     fixnump
     quit
     include
     memq
     nremove
					;put
     ratiop
     structure-typep
     structurep
     type-specifier-p
     displaced-array-p
     without-interrupts
     with-interrupts-enabled
     true
     false
     neq
     whitespacep
     *print-structure*
     *print-simple-vector*
     *print-simple-bit-vector*
     *print-string-length*
     *print-abbreviate-quote*
     *signal-printing-errors*
     unignore
     *warn-if-redefine-kernel*
     without-duplicate-definition-warnings
     require-type
     dovector
     debugging-function-name
     *make-package-use-defaults*
     *autoload-lisp-package*
     tyo
     tyi
     untyi
     compiled-lexical-closure		; the type name
     lsh

     ;; Arguments, image name, etc.
     *command-line-argument-list*
     *unprocessed-command-line-arguments*
     *heap-image-name*

					; The MOP
     accessor-method-slot-definition
     add-dependent
     add-direct-method
     add-direct-subclass
     add-method
     class-default-initargs
     class-direct-default-initargs
     class-direct-slots
     class-direct-subclasses
     class-direct-superclasses
     class-finalized-p
     class-precedence-list
     class-prototype
     class-slots
     compute-applicable-methods
     compute-applicable-methods-using-classes
     compute-class-precedence-list
     compute-default-initargs
     compute-discriminating-function
     compute-effective-method
     compute-effective-slot-definition
     compute-slots
     direct-slot-definition-class
     effective-slot-definition-class
     ensure-class
     ensure-class-using-class
     ensure-generic-function-using-class
     eql-specializer
     eql-specializer-object
     extract-lambda-list
     extract-specializer-names
     finalize-inheritance
     find-method-combination
     funcallable-standard-instance-access
     generic-function-argument-precedence-order
     generic-function-declarations
     generic-function-lambda-list
     generic-function-method-class
     generic-function-method-combination
     generic-function-methods
     generic-function-name
     intern-eql-specializer
     make-method-lambda
     map-dependents
     method-function
     method-generic-function
     method-lambda-list
     method-name
     method-specializers
     method-qualifiers
     slot-definition-documentation
     slot-definition-allocation
     slot-definition-initargs
     slot-definition-initform
     slot-definition-initfunction
     slot-definition-name
     slot-definition-type
     slot-definition-readers
     slot-definition-writers
     slot-definition-location
     reader-method-class
     remove-dependent
     remove-direct-method
     remove-direct-subclass
     remove-method
     set-funcallable-instance-function
     slot-boundp-using-class
     slot-makunbound-using-class
     slot-value-using-class
     specializer-direct-generic-functions
     specializer-direct-methods
     standard-instance-access
     update-dependent
     validate-superclass
     writer-method-class
     
     metaobject
     long-method-combination
     short-method-combination
     standard-accessor-method
     standard-reader-method
     standard-writer-method
     specializer

     funcallable-standard-class
     funcallable-standard-object
     forward-referenced-class
     standard-direct-slot-definition
     standard-effective-slot-definition

     standard-slot-definition
     slot-definition
     effective-slot-definition
     direct-slot-definition
     
     clear-specializer-direct-methods-caches
     *check-call-next-method-with-args*
     clear-gf-cache
     clear-all-gf-caches
     clear-clos-caches

     method-exists-p
     method-specializers
     class-own-wrapper
     specializer-direct-methods
     specializer-direct-generic-functions
     copy-instance

     override-one-method-one-arg-dcode
     optimize-generic-function-dispatching

     ;; Not MOP
     no-applicable-method-exists
     string-studlify			;** DO NOT REMOVE, DO NOT DOCUMENT
     nstring-studlify			;** DO NOT REMOVE, DO NOT DOCUMENT

					; User Options
     *compile-definitions*
     *record-source-file*
     *save-doc-strings*
     *fasl-save-doc-strings* 
     *warn-if-redefine*
     *break-on-errors* 
     *save-definitions*
     *fasl-save-definitions* 
     *save-local-symbols*
     *fasl-save-local-symbols*
     *save-arglist-info*
     *always-eval-user-defvars*
     *disassemble-verbose*
     target-fasl-version

					;These 3 need to be set by the user in order for the correspondingly named
					;functions to return something other than "unspecified".
     *short-site-name*
     *long-site-name*
     machine-owner

     init-list-default
     fset

					; Files.
     mac-default-directory
     current-directory
     directory-pathname-p
     full-pathname
     temp-pathname
     create-file
     create-directory
     file-create-date
     set-file-write-date
     set-file-create-date
     copy-file
     lock-file
     unlock-file
     file-locked-p
     directoryp
     delete-directory
     *trust-paths-from-environment*


     *module-search-path*
     *module-provider-functions*
     *.lisp-pathname*
     *.fasl-pathname*
     *pathname-translations-pathname*
     *default-external-format*
     *default-line-termination*
     pathname-encoding-name
     with-filename-cstrs
     get-foreign-namestring
     native-translated-namestring
     native-to-pathname
     fasl-concatenate
     event-ticks
     set-event-ticks
     event-dispatch
     *ticks-per-second*
     encoding-problem
     decoding-problem
     with-encoding-problems-as-errors
     with-decoding-problems-as-errors

     *application*
     arglist
     arglist-string
     arglist-to-stream
     function-args


     get-string-from-user
     with-terminal-input
     *request-terminal-input-via-break*
     add-auto-flush-stream
     remove-auto-flush-stream
     select-item-from-list


					; Low-level
     %stack-block
     %vstack-block
     %get-byte
     %get-signed-byte
     %get-unsigned-byte
     %get-word
     %get-signed-word
     %get-unsigned-word
     %get-long
     %get-unsigned-long
     %get-signed-long
     %%get-signed-longlong
     %%get-unsigned-longlong
     %get-fixnum
     %get-point
     %get-ptr
     %get-string
     %get-cstring
     %str-from-ptr
     %get-double-float
     %get-single-float
     %inc-ptr
     %incf-ptr
     %setf-macptr
     %null-ptr
     %null-ptr-p
     %ptr-eql
     %ptr-to-int
     %int-to-ptr
     %word-to-int
     %address-of
     ensure-simple-string
     %copy-float
     with-macptrs
     pointerp
     macptrp
     macptr
     rlet
     rletz
     make-record
     pref
     rref
     paref
     dparef
     sparef
     with-cstrs
     with-encoded-cstrs
     with-string-vector
     with-pointer-to-ivector
     get-encoded-string
     +null-ptr+
     free
     define-entry-point
     define-callback
     defcallback
     ff-call
     %ff-call
     %reference-external-entry-point
     foreign-symbol-entry
     foreign-symbol-address
     def-foreign-type

     uvref
     uvectorp
     uvsize

     ;;Streams (should be made more complete sometime)
     input-stream
     output-stream
     stream-eofp

     open-file-streams
     note-open-file-stream
     remove-open-file-stream
     clear-open-file-streams
     stream-line-length
     string-output-stream
     truncating-string-stream
     make-truncating-string-stream
     stream-rubout-handler


					; Tools
     gc
     egc
     egc-enabled-p
     egc-active-p
     configure-egc
     egc-configuration
     gccounts
     gctime
     lisp-heap-gc-threshold
     use-lisp-heap-gc-threshold
     set-lisp-heap-gc-threshold
     gc-retain-pages
     gc-retaining-pages
     gc-verbose
     gc-verbose-p
     weak-gc-method
     *trace-max-indent* 
     *trace-level* 
     static-cons
     free-static-conses
     reserved-static-conses
     get-gc-notification-threshold
     set-gc-notification-threshold
     *pending-gc-notification-hook*
     
     population
     make-population
     population-type
     population-contents

     hash-table-weak-p

     compiler-let


     COMPILER-POLICY
     CURRENT-COMPILER-POLICY
     CURRENT-FILE-COMPILER-POLICY
     FIND-MACTYPE
     NEW-COMPILER-POLICY
     SET-CURRENT-COMPILER-POLICY
     SET-CURRENT-FILE-COMPILER-POLICY
     STANDARD-METHOD-COMBINATION
     STREAM-DEVICE
     STREAM-DIRECTION
     *current-process*
     PROCESS
     all-processes
     process-preset
     process-reset
     process-reset-and-enable
     process-enable
     process-abort
     process-kill
     process-interrupt
     process-name
     process-plist
     process-run-function
     make-process
     process-suspend-count
     process-serial-number
     process-initial-form
     process-whostate
     process-priority
     process-total-run-time
     process-creation-time
     clear-process-run-time
     process-resume
     process-suspend
     process-exhausted-p
     let-globally
     process-wait
     process-wait-with-timeout
     process-allow-schedule
     process-kill-issued
     process-termination-semaphore
     process-allocation-quantum
     default-allocation-quantum
     current-process-allocation-quantum
     join-process

     *HOST-PAGE-SIZE*
     
     make-lock
     lock-name
     with-lock-grabbed
     grab-lock
     release-lock
     try-lock
     lock
     read-write-lock
     lock-not-owner

     lock-acquisition-status
     clear-lock-acquisition-status
     lock-acquisition
     make-lock-acquisition

     semaphore-notification-status
     clear-semaphore-notification-status
     semaphore-notification
     make-semaphore-notification
     
     make-read-write-lock
     with-read-lock
     with-write-lock
     symbol-value-in-process

     make-semaphore
     wait-on-semaphore
     timed-wait-on-semaphore
     signal-semaphore
     semaphore

     process-input-wait
     process-output-wait
     wait-for-signal
                                        ; termination
     terminate-when-unreachable
     terminate
     drain-termination-queue
     cancel-terminate-when-unreachable
     termination-function
     *enable-automatic-termination*

     get-fpu-mode
     set-fpu-mode

					; There's more. Like...

     *listener-indent*
     *error-print-circle*
     *break-loop-when-uninterruptable*

     application-error
     application-name
     application-init-file

     cwd

     ;; Old CLtL2 stuff:

     *applyhook*
     *evalhook*
     applyhook
     augment-environment
     declaration-information
     define-declaration
     define-setf-method
     evalhook
     enclose
     function-information
     generic-flet
     generic-labels
     get-setf-method
     get-setf-method-multiple-value
     parse-macro
     variable-information
     with-added-methods

     ;; Gray Streams
     fundamental-stream
     fundamental-input-stream
     fundamental-output-stream
     fundamental-character-stream
     fundamental-character-input-stream
     fundamental-character-output-stream
     fundamental-binary-stream
     fundamental-binary-input-stream
     fundamental-binary-output-stream

     stream-read-char
     stream-unread-char
     stream-read-char-no-hang
     stream-peek-char
     stream-listen
     stream-read-line
     stream-clear-input

     stream-write-char
     stream-line-column
     stream-start-line-p
     stream-write-string
     stream-terpri
     stream-fresh-line
     stream-force-output
     stream-clear-output
     stream-advance-to-column

     stream-read-byte
     stream-write-byte

     stream-read-ivector
     stream-write-ivector

     stream-read-list
     stream-write-list
     stream-read-vector
     stream-write-vector

     stream-input-timeout
     stream-output-timeout
     with-input-timeout
     with-output-timeout
     stream-deadline

     input-timeout
     output-timeout
     communication-deadline-expired

     make-heap-ivector
     dispose-heap-ivector
     ;;
     external
     external-call
     open-shared-library
     close-shared-library
     shlib
     external-entry-point
     use-interface-dir
     unuse-interface-dir
     create-interfaces
     ;;
     run-program
     external-process
     signal-external-process
     external-process-id
     external-process-input-stream
     external-process-output-stream
     external-process-error-stream
     external-process-status
     ;;
     *altivec-available*
     altivec-available-p
     *altivec-lapmacros-maintain-vrsave-p*
     ;;
     *alternate-line-terminator*
     ;;
     set-user-environment
     set-development-environment
     *resident-editor-hook*
     cpu-count
     *report-time-function*
     ;;
     compile-ccl
     xcompile-ccl
     xload-level-0
     rebuild-ccl
     update-ccl
     test-ccl
     defglobal

     getenv
     setenv

     external-format
     make-external-format
     external-format-character-encoding
     external-format-line-termination
     character-encoding
     define-character-encoding
     describe-character-encoding
     describe-character-encodings
     get-character-encoding
     lookup-character-encoding
     string-size-in-octets
     encode-string-to-octets
     count-characters-in-octet-vector
     decode-string-from-octets
     *terminal-character-encoding-name*
     *default-file-character-encoding*
     *default-socket-character-encoding*
     ;; Mapped files.
     map-file-to-ivector
     map-file-to-octet-vector
     unmap-ivector
     unmap-octet-vector
     ;; Miscellany
     heap-utilization
     collect-heap-utilization
     parse-unsigned-integer
     parse-signed-integer
     pui-stream
     psi-stream
     with-output-to-vector
     with-input-from-vector
     make-vector-output-stream
     make-vector-input-stream
     unsigned-integer-to-binary
     signed-integer-to-binary
     vector-input-stream
     vector-output-stream
     get-output-stream-vector  
     *vector-output-stream-default-initial-allocation*   
     external-process-creation-failure
     object-direct-size
     add-feature
     remove-feature
     ;; Disabling heap allocation (to detect unexpected consing.)
     allow-heap-allocaton
     heap-allocation-allowed-p
     allocation-disabled

     ) "CCL"
   )
  )

;;; Define a package for MOP extensions.
(defpackage "OPENMCL-MOP"
  (:use)
  (:import-from
   "CCL"
   "ACCESSOR-METHOD-SLOT-DEFINITION"
   "ADD-DEPENDENT"
   "ADD-DIRECT-METHOD"
   "ADD-DIRECT-SUBCLASS"
   "ADD-METHOD"
   "CLASS-DEFAULT-INITARGS"
   "CLASS-DIRECT-DEFAULT-INITARGS"
   "CLASS-DIRECT-SLOTS"
   "CLASS-DIRECT-SUBCLASSES"
   "CLASS-DIRECT-SUPERCLASSES"
   "CLASS-FINALIZED-P"
   "CLASS-PRECEDENCE-LIST"
   "CLASS-PROTOTYPE"
   "CLASS-SLOTS"
   "COMPUTE-APPLICABLE-METHODS"
   "COMPUTE-APPLICABLE-METHODS-USING-CLASSES"
   "COMPUTE-CLASS-PRECEDENCE-LIST"
   "COMPUTE-DEFAULT-INITARGS"
   "COMPUTE-DISCRIMINATING-FUNCTION"
   "COMPUTE-EFFECTIVE-METHOD"
   "COMPUTE-EFFECTIVE-SLOT-DEFINITION"
   "COMPUTE-SLOTS"
   "DIRECT-SLOT-DEFINITION-CLASS"
   "EFFECTIVE-SLOT-DEFINITION-CLASS"
   "ENSURE-CLASS"
   "ENSURE-CLASS-USING-CLASS"
   "ENSURE-GENERIC-FUNCTION-USING-CLASS"
   "EQL-SPECIALIZER"
   "EQL-SPECIALIZER-OBJECT"
   "EXTRACT-LAMBDA-LIST"
   "EXTRACT-SPECIALIZER-NAMES"
   "FINALIZE-INHERITANCE"
   "FIND-METHOD-COMBINATION"
   "FUNCALLABLE-STANDARD-INSTANCE-ACCESS"
   "GENERIC-FUNCTION-ARGUMENT-PRECEDENCE-ORDER"
   "GENERIC-FUNCTION-DECLARATIONS"
   "GENERIC-FUNCTION-LAMBDA-LIST"
   "GENERIC-FUNCTION-METHOD-CLASS"
   "GENERIC-FUNCTION-METHOD-COMBINATION"
   "GENERIC-FUNCTION-METHODS"
   "GENERIC-FUNCTION-NAME"
   "INTERN-EQL-SPECIALIZER"
   "MAKE-METHOD-LAMBDA"
   "MAP-DEPENDENTS"
   "METHOD-FUNCTION"
   "METHOD-GENERIC-FUNCTION"
   "METHOD-LAMBDA-LIST"
   "METHOD-NAME"
   "METHOD-SPECIALIZERS"
   "METHOD-QUALIFIERS"
   "SLOT-DEFINITION-DOCUMENTATION"
   "SLOT-DEFINITION-ALLOCATION"
   "SLOT-DEFINITION-INITARGS"
   "SLOT-DEFINITION-INITFORM"
   "SLOT-DEFINITION-INITFUNCTION"
   "SLOT-DEFINITION-NAME"
   "SLOT-DEFINITION-TYPE"
   "SLOT-DEFINITION-READERS"
   "SLOT-DEFINITION-WRITERS"
   "SLOT-DEFINITION-LOCATION"
   "READER-METHOD-CLASS"
   "REMOVE-DEPENDENT"
   "REMOVE-DIRECT-METHOD"
   "REMOVE-DIRECT-SUBCLASS"
   "REMOVE-METHOD"
   "SET-FUNCALLABLE-INSTANCE-FUNCTION"
   "SLOT-BOUNDP-USING-CLASS"
   "SLOT-MAKUNBOUND-USING-CLASS"
   "SLOT-VALUE-USING-CLASS"
   "SPECIALIZER-DIRECT-GENERIC-FUNCTIONS"
   "SPECIALIZER-DIRECT-METHODS"
   "STANDARD-DIRECT-SLOT-DEFINITION"
   "STANDARD-EFFECTIVE-SLOT-DEFINITION"
   "STANDARD-INSTANCE-ACCESS"
   "UPDATE-DEPENDENT"
   "VALIDATE-SUPERCLASS"
   "WRITER-METHOD-CLASS"
     
   "METAOBJECT"
   "LONG-METHOD-COMBINATION"
   "SHORT-METHOD-COMBINATION"
   "STANDARD-ACCESSOR-METHOD"
   "STANDARD-READER-METHOD"
   "STANDARD-WRITER-METHOD"
   "SPECIALIZER"

   "FUNCALLABLE-STANDARD-CLASS"
   "FUNCALLABLE-STANDARD-OBJECT"
   "FORWARD-REFERENCED-CLASS"

   "CLEAR-SPECIALIZER-DIRECT-METHODS-CACHES"
   "*CHECK-CALL-NEXT-METHOD-WITH-ARGS*"
   "CLEAR-GF-CACHE"
   "CLEAR-ALL-GF-CACHES"
   "CLEAR-CLOS-CACHES"

   "METHOD-EXISTS-P"
   "METHOD-SPECIALIZERS"
   "CLASS-OWN-WRAPPER"
   "SPECIALIZER-DIRECT-METHODS"
   "SPECIALIZER-DIRECT-GENERIC-FUNCTIONS"
   "COPY-INSTANCE"
   "STANDARD-SLOT-DEFINITION"
   "SLOT-DEFINITION"
   "EFFECTIVE-SLOT-DEFINITION"
   "DIRECT-SLOT-DEFINITION"
   )
  (:export
   "ACCESSOR-METHOD-SLOT-DEFINITION"
   "ADD-DEPENDENT"
   "ADD-DIRECT-METHOD"
   "ADD-DIRECT-SUBCLASS"
   "ADD-METHOD"
   "CLASS-DEFAULT-INITARGS"
   "CLASS-DIRECT-DEFAULT-INITARGS"
   "CLASS-DIRECT-SLOTS"
   "CLASS-DIRECT-SUBCLASSES"
   "CLASS-DIRECT-SUPERCLASSES"
   "CLASS-FINALIZED-P"
   "CLASS-PRECEDENCE-LIST"
   "CLASS-PROTOTYPE"
   "CLASS-SLOTS"
   "COMPUTE-APPLICABLE-METHODS"
   "COMPUTE-APPLICABLE-METHODS-USING-CLASSES"
   "COMPUTE-CLASS-PRECEDENCE-LIST"
   "COMPUTE-DEFAULT-INITARGS"
   "COMPUTE-DISCRIMINATING-FUNCTION"
   "COMPUTE-EFFECTIVE-METHOD"
   "COMPUTE-EFFECTIVE-SLOT-DEFINITION"
   "COMPUTE-SLOTS"
   "DIRECT-SLOT-DEFINITION-CLASS"
   "EFFECTIVE-SLOT-DEFINITION-CLASS"
   "ENSURE-CLASS"
   "ENSURE-CLASS-USING-CLASS"
   "ENSURE-GENERIC-FUNCTION-USING-CLASS"
   "EQL-SPECIALIZER"
   "EQL-SPECIALIZER-OBJECT"
   "EXTRACT-LAMBDA-LIST"
   "EXTRACT-SPECIALIZER-NAMES"
   "FINALIZE-INHERITANCE"
   "FIND-METHOD-COMBINATION"
   "FUNCALLABLE-STANDARD-INSTANCE-ACCESS"
   "GENERIC-FUNCTION-ARGUMENT-PRECEDENCE-ORDER"
   "GENERIC-FUNCTION-DECLARATIONS"
   "GENERIC-FUNCTION-LAMBDA-LIST"
   "GENERIC-FUNCTION-METHOD-CLASS"
   "GENERIC-FUNCTION-METHOD-COMBINATION"
   "GENERIC-FUNCTION-METHODS"
   "GENERIC-FUNCTION-NAME"
   "INTERN-EQL-SPECIALIZER"
   "MAKE-METHOD-LAMBDA"
   "MAP-DEPENDENTS"
   "METHOD-FUNCTION"
   "METHOD-GENERIC-FUNCTION"
   "METHOD-LAMBDA-LIST"
   "METHOD-NAME"
   "METHOD-SPECIALIZERS"
   "METHOD-QUALIFIERS"
   "SLOT-DEFINITION-DOCUMENTATION"
   "SLOT-DEFINITION-ALLOCATION"
   "SLOT-DEFINITION-INITARGS"
   "SLOT-DEFINITION-INITFORM"
   "SLOT-DEFINITION-INITFUNCTION"
   "SLOT-DEFINITION-NAME"
   "SLOT-DEFINITION-TYPE"
   "SLOT-DEFINITION-READERS"
   "SLOT-DEFINITION-WRITERS"
   "SLOT-DEFINITION-LOCATION"
   "READER-METHOD-CLASS"
   "REMOVE-DEPENDENT"
   "REMOVE-DIRECT-METHOD"
   "REMOVE-DIRECT-SUBCLASS"
   "REMOVE-METHOD"
   "SET-FUNCALLABLE-INSTANCE-FUNCTION"
   "SLOT-BOUNDP-USING-CLASS"
   "SLOT-MAKUNBOUND-USING-CLASS"
   "SLOT-VALUE-USING-CLASS"
   "SPECIALIZER-DIRECT-GENERIC-FUNCTIONS"
   "SPECIALIZER-DIRECT-METHODS"
   "STANDARD-DIRECT-SLOT-DEFINITION"
   "STANDARD-EFFECTIVE-SLOT-DEFINITION"
   "STANDARD-INSTANCE-ACCESS"
   "UPDATE-DEPENDENT"
   "VALIDATE-SUPERCLASS"
   "WRITER-METHOD-CLASS"
     
   "METAOBJECT"
   "LONG-METHOD-COMBINATION"
   "SHORT-METHOD-COMBINATION"
   "STANDARD-ACCESSOR-METHOD"
   "STANDARD-READER-METHOD"
   "STANDARD-WRITER-METHOD"
   "SPECIALIZER"

   "FUNCALLABLE-STANDARD-CLASS"
   "FORWARD-REFERENCED-CLASS"


   "CLEAR-SPECIALIZER-DIRECT-METHODS-CACHES"
   "*CHECK-CALL-NEXT-METHOD-WITH-ARGS*"
   "CLEAR-GF-CACHE"
   "CLEAR-ALL-GF-CACHES"
   "CLEAR-CLOS-CACHES"

   "METHOD-EXISTS-P"
   "METHOD-SPECIALIZERS"
   "CLASS-OWN-WRAPPER"
   "SPECIALIZER-DIRECT-METHODS"
   "SPECIALIZER-DIRECT-GENERIC-FUNCTIONS"
   "COPY-INSTANCE"
   "STANDARD-SLOT-DEFINITION"
   "SLOT-DEFINITION"
   "EFFECTIVE-SLOT-DEFINITION"
   "DIRECT-SLOT-DEFINITION"
   ))

(unless (eq %lisp-system-fixups% T)
  (while %lisp-system-fixups%
    (let* ((fn.source (car %lisp-system-fixups%))
           (*loading-toplevel-location* (and (source-note-p (cdr fn.source)) (cdr fn.source)))
           (*loading-file-source-file* (source-note-filename (cdr fn.source)))
           )
      (funcall (car fn.source)))
    (setq %lisp-system-fixups% (cdr %lisp-system-fixups%)))
  (setq %lisp-system-fixups% T))




