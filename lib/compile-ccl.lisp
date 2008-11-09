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

(require 'systems)

; Interim PPC support
; sequences is here since l1-typesys REQUIREs it
(defparameter *level-1-modules*
  '(level-1
    l1-cl-package
    l1-boot-1 l1-boot-2 l1-boot-3
    l1-utils l1-init l1-symhash l1-numbers l1-aprims 
    l1-sort l1-dcode l1-clos-boot l1-clos
    l1-unicode l1-streams l1-files l1-io 
    l1-format l1-readloop l1-reader
    l1-sysio l1-pathnames l1-events
    l1-boot-lds  l1-readloop-lds 
    l1-lisp-threads  l1-application l1-processes
    l1-typesys sysutils l1-error-system
    l1-error-signal version l1-callbacks
    l1-sockets linux-files

    ))

(defparameter *compiler-modules*
      '(nx optimizers dll-node arch vreg vinsn 
	reg subprims  backend))


(defparameter *ppc-compiler-modules*
  '(ppc32-arch
    ppc64-arch
    ppc-arch
    ppcenv
    ppc-asm
    risc-lap
    ppc-lap
    ppc-backend
))

(defparameter *x86-compiler-modules*
  '(x8632-arch
    x8664-arch
    x86-arch
    x8632env
    x8664env
    x86-asm
    x86-lap
    x86-backend
))

(defparameter *ppc32-compiler-backend-modules*
  '(ppc32-backend ppc32-vinsns))

(defparameter *ppc64-compiler-backend-modules*
  '(ppc64-backend ppc64-vinsns))


(defparameter *ppc-compiler-backend-modules*
  '(ppc2))


(defparameter *x8632-compiler-backend-modules*
  '(x8632-backend x8632-vinsns))

(defparameter *x8664-compiler-backend-modules*
  '(x8664-backend x8664-vinsns))

(defparameter *x86-compiler-backend-modules*
  '(x862))




(defparameter *ppc-xload-modules* '(xppcfasload xfasload heap-image ))
(defparameter *x8632-xload-modules* '(xx8632fasload xfasload heap-image ))
(defparameter *x8664-xload-modules* '(xx8664fasload xfasload heap-image ))


;;; Not too OS-specific.
(defparameter *ppc-xdev-modules* '(ppc-lapmacros ))
(defparameter *x86-xdev-modules* '(x86-lapmacros ))

(defun target-xdev-modules (&optional (target
				       (backend-target-arch-name
					*host-backend*)))
  (case target
    ((:ppc32 :ppc64) *ppc-xdev-modules*)
    ((:x8632 :x8664) *x86-xdev-modules*)))

(defun target-xload-modules (&optional (target
					(backend-target-arch-name *host-backend*)))
  (case target
    ((:ppc32 :ppc64) *ppc-xload-modules*)
    (:x8632 *x8632-xload-modules*)
    (:x8664 *x8664-xload-modules*)))






(defparameter *env-modules*
  '(hash backquote lispequ  level-2 macros
    defstruct-macros lists chars setf setf-runtime
    defstruct defstruct-lds 
    foreign-types
    db-io
    nfcomp
    ))

(defun target-env-modules (&optional (target
				      (backend-name *host-backend*)))
  (append *env-modules*
          (list
           (ecase target
             (:linuxppc32 'ffi-linuxppc32)
             (:darwinppc32 'ffi-darwinppc32)
             (:darwinppc64 'ffi-darwinppc64)
             (:linuxppc64 'ffi-linuxppc64)
	     (:darwinx8632 'ffi-darwinx8632)
             (:linuxx8664 'ffi-linuxx8664)
             (:darwinx8664 'ffi-darwinx8664)
             (:freebsdx8664 'ffi-freebsdx8664)
             (:solarisx8664 'ffi-solarisx8664)
             (:win64 'ffi-win64)
             (:linuxx8632 'ffi-linuxx8632)
             (:win32 'ffi-win32)
             (:solarisx8632 'ffi-solarisx8632)
             (:freebsdx8632 'ffi-freebsdx8632)))))


(defun target-compiler-modules (&optional (target
					   (backend-target-arch-name
					    *host-backend*)))
  (case target
    (:ppc32 (append *ppc-compiler-modules*
                    *ppc32-compiler-backend-modules*
                    *ppc-compiler-backend-modules*))
    (:ppc64 (append *ppc-compiler-modules*
                    *ppc64-compiler-backend-modules*
                    *ppc-compiler-backend-modules*))
    (:x8632 (append *x86-compiler-modules*
                    *x8632-compiler-backend-modules*
                    *x86-compiler-backend-modules*))
    (:x8664 (append *x86-compiler-modules*
                    *x8664-compiler-backend-modules*
                    *x86-compiler-backend-modules*))))

(defparameter *other-lib-modules*
  '(streams pathnames backtrace
    apropos
    numbers 
    dumplisp   source-files))

(defun target-other-lib-modules (&optional (target
					    (backend-target-arch-name
					     *host-backend*)))
  (append *other-lib-modules*
	  (case target
	    ((:ppc32 :ppc64) '(ppc-backtrace ppc-disassemble))
            ((:x8632 :x8664) '(x86-backtrace x86-disassemble)))))
	  

(defun target-lib-modules (&optional (backend-name
                                      (backend-name *host-backend*)))
  (let* ((backend (or (find-backend backend-name) *host-backend*))
         (arch-name (backend-target-arch-name backend)))
    (append (target-env-modules backend-name) (target-other-lib-modules arch-name))))


(defparameter *code-modules*
      '(encapsulate
        read misc  arrays-fry
        sequences sort 
        method-combination
        case-error pprint 
        format time 
;        eval step
        backtrace-lds  ccl-export-syms prepare-mcl-environment))



(defparameter *aux-modules*
      '(systems compile-ccl 
        lisp-package
        number-macros number-case-macro
        loop
	runtime
	mcl-compat
	arglist
	edit-callers
        describe
        leaks
	asdf
	defsystem
))







(defun target-level-1-modules (&optional (target (backend-name *host-backend*)))
  (append *level-1-modules*
	  (case target
	    ((:linuxppc32 :darwinppc32 :linuxppc64 :darwinppc64)
	     '(ppc-error-signal ppc-trap-support
	       ppc-threads-utils ppc-callback-support))
            ((:linuxx8664 :freebsdx8664 :darwinx8664 :solarisx8664
	      :darwinx8632 :win64  :linuxx8632 :win32 :solarisx8632
              :freebsdx8632)
             '(x86-error-signal x86-trap-support
               x86-threads-utils x86-callback-support)))))

		  




;





; Needed to cross-dump an image



(unless (fboundp 'xload-level-0)
  (%fhave 'xload-level-0
          #'(lambda (&rest rest)
	      (in-development-mode
	       (require-modules (target-xload-modules)))
              (apply 'xload-level-0 rest))))

(defun find-module (module &optional (target (backend-name *host-backend*))  &aux data fasl sources)
  (if (setq data (assoc module *ccl-system*))
    (let* ((backend (or (find-backend target) *host-backend*)))
      (setq fasl (cadr data) sources (caddr data))      
      (setq fasl (merge-pathnames (backend-target-fasl-pathname
				   backend) fasl))
      (values fasl (if (listp sources) sources (list sources))))
    (error "Module ~S not defined" module)))

;compile if needed.
(defun target-compile-modules (modules target force-compile)
  (if (not (listp modules)) (setq modules (list modules)))
  (in-development-mode
   (dolist (module modules t)
     (multiple-value-bind (fasl sources) (find-module module target)
      (if (needs-compile-p fasl sources force-compile)
        (progn
          (require'nfcomp)
          (compile-file (car sources)
			:output-file fasl
			:verbose t
			:target target)))))))






(defun needs-compile-p (fasl sources force-compile)
  (if fasl
    (if (eq force-compile t)
      t
      (if (not (probe-file fasl))
        t
        (let ((fasldate (file-write-date fasl)))
          (if (if (integerp force-compile) (> force-compile fasldate))
            t
            (dolist (source sources nil)
              (if (> (file-write-date source) fasldate)
                (return t)))))))))



;compile if needed, load if recompiled.

(defun update-modules (modules &optional force-compile)
  (if (not (listp modules)) (setq modules (list modules)))
  (in-development-mode
   (dolist (module modules t)
     (multiple-value-bind (fasl sources) (find-module module)
       (if (needs-compile-p fasl sources force-compile)
	 (progn
	   (require'nfcomp)
	   (let* ((*warn-if-redefine* nil))
	     (compile-file (car sources) :output-file fasl :verbose t :load t))
	   (provide module)))))))

(defun compile-modules (modules &optional force-compile)
  (target-compile-modules modules (backend-name *host-backend*) force-compile)
)

(defun compile-ccl (&optional force-compile)
 (with-compilation-unit ()
  (update-modules 'nxenv force-compile)
  (update-modules *compiler-modules* force-compile)
  (update-modules (target-compiler-modules) force-compile)
  (update-modules (target-xdev-modules) force-compile)
  (update-modules (target-xload-modules)  force-compile)
  (let* ((env-modules (target-env-modules))
	 (other-lib (target-other-lib-modules)))
    (require-modules env-modules)
    (update-modules env-modules force-compile)
    (compile-modules (target-level-1-modules)  force-compile)
    (update-modules other-lib force-compile)
    (require-modules other-lib)
    (require-update-modules *code-modules* force-compile))
  (compile-modules *aux-modules* force-compile)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun require-env (&optional force-load)
  (require-modules  (target-env-modules)
                   force-load))

(defun compile-level-1 (&optional force-compile)
  (require-env)
  (compile-modules (target-level-1-modules (backend-name *host-backend*))
                   force-compile))





(defun compile-lib (&optional force-compile)
  (compile-modules (target-lib-modules)
                   force-compile))

(defun compile-code (&optional force-compile)
  (compile-modules *code-modules* force-compile))


;Compile but don't load

(defun xcompile-ccl (&optional force)
 (with-compilation-unit ()
  (compile-modules 'nxenv force)
  (compile-modules *compiler-modules* force)
  (compile-modules (target-compiler-modules) force)
  (compile-modules (target-xdev-modules) force)
  (compile-modules (target-xload-modules)  force)
  (compile-modules (target-env-modules) force)
  (compile-modules (target-level-1-modules) force)
  (compile-modules (target-other-lib-modules) force)
  (compile-modules *code-modules* force)
  (compile-modules *aux-modules* force)))

(defun require-update-modules (modules &optional force-compile)
  (if (not (listp modules)) (setq modules (list modules)))
  (in-development-mode
    (dolist (module modules)
    (require-modules module)
    (update-modules module force-compile))))


(defun target-xcompile-ccl (target &optional force)
  (let* ((backend (or (find-backend target) *target-backend*))
	 (arch (backend-target-arch-name backend))
	 (*defstruct-share-accessor-functions* nil))
    (target-compile-modules 'nxenv target force)
    (target-compile-modules *compiler-modules* target force)
    (target-compile-modules (target-compiler-modules arch) target force)
    (target-compile-modules (target-level-1-modules target) target force)
    (target-compile-modules (target-lib-modules target) target force)
    (target-compile-modules *aux-modules* target force)
    (target-compile-modules *code-modules* target force)
    (target-compile-modules (target-xdev-modules arch) target force)))

(defun cross-compile-ccl (target &optional force)
  (with-cross-compilation-target (target)
    (let* ((*target-backend* (find-backend target)))
      (target-xcompile-ccl target force))))


(defun require-module (module force-load)
  (multiple-value-bind (fasl source) (find-module module)
      (setq source (car source))
      (if (if fasl (probe-file fasl))
        (if force-load
          (progn
            (load fasl)
            (provide module))
          (require module fasl))
        (if (probe-file source)
          (progn
            (if fasl (format t "~&Can't find ~S so requiring ~S instead"
                             fasl source))
            (if force-load
              (progn
                (load source)
                (provide module))
              (require module source)))
          (error "Can't find ~S or ~S" fasl source)))))

(defun require-modules (modules &optional force-load)
  (if (not (listp modules)) (setq modules (list modules)))
  (let ((*package* (find-package :ccl)))
    (dolist (m modules t)
      (require-module m force-load))))


(defun target-xcompile-level-1 (target &optional force)
  (target-compile-modules (target-level-1-modules target) target force))

(defun standard-boot-image-name (&optional (target (backend-name *host-backend*)))
  (ecase target
    (:darwinppc32 "ppc-boot.image")
    (:linuxppc32 "ppc-boot")
    (:darwinppc64 "ppc-boot64.image")
    (:linuxppc64 "ppc-boot64")
    (:darwinx8632 "x86-boot32.image")
    (:linuxx8664 "x86-boot64")
    (:freebsdx8664 "fx86-boot64")
    (:darwinx8664 "x86-boot64.image")
    (:solarisx8664 "sx86-boot64")
    (:win64 "wx86-boot64.image")
    (:linuxx8632 "x86-boot32")
    (:win32 "wx86-boot32.image")
    (:solarisx8632 "sx86-boot32")
    (:freebsdx8632 "fx86-boot32")))

(defun standard-kernel-name (&optional (target (backend-name *host-backend*)))
  (ecase target
    (:darwinppc32 "dppccl")
    (:linuxppc32 "ppccl")
    (:darwinppc64 "dppccl64")
    (:darwinx8632 "dx86cl")
    (:linuxppc64 "ppccl64")
    (:linuxx8664 "lx86cl64")
    (:freebsdx8664 "fx86cl64")
    (:darwinx8664 "dx86cl64")
    (:solarisx8664 "sx86cl64")
    (:win64 "wx86cl64.exe")
    (:linuxx8632 "lx86cl")
    (:win32 "wx86cl.exe")
    (:solarisx8632 "sx86cl")
    (:freebsdx8632 "fx86cl")))

(defun standard-image-name (&optional (target (backend-name *host-backend*)))
  (concatenate 'string (pathname-name (standard-kernel-name target)) ".image"))

(defun kernel-build-directory (&optional (target (backend-name *host-backend*)))
  (ecase target
    (:darwinppc32 "darwinppc")
    (:linuxppc32 "linuxppc")
    (:darwinppc64 "darwinppc64")
    (:linuxppc64 "linuxppc64")
    (:darwinx8632 "darwinx8632")
    (:linuxx8664 "linuxx8664")
    (:freebsdx8664 "freebsdx8664")
    (:darwinx8664 "darwinx8664")
    (:solarisx8664 "solarisx64")
    (:win64 "win64")
    (:linuxx8632 "linuxx8632")
    (:win32 "win32")
    (:solarisx8632 "solarisx86")
    (:freebsdx8632 "freebsdx8632")))

;;; If we distribute (e.g.) 32- and 64-bit versions for the same
;;; machine and OS in the same svn directory, return the name of the
;;; peer backend, or NIL. For example., the peer of :linuxppc64 is
;;; :linuxppc32.  Note that this may change over time.
;;; Return NIL if the concept doesn't apply.
(defun peer-platform (&optional (target (backend-name *host-backend*)))
  (let* ((pairs '((:darwinppc32 . :darwinppc64)
                  (:linuxppc32 . :linuxppc64)
                  (:darwinx8632 . :darwinx8664)
                  (:linuxx8632 . :linuxx8664)
                  (:win32 . :win64)
                  (:solarisx8632 . :solarisx8664)
                  (:freebsdx8632 . :freebsdx8664))))
    (or (cdr (assoc target pairs))
        (car (rassoc target pairs)))))

(defun make-program (&optional (target (backend-name *host-backend*)))
  ;; The Solaris "make" program is too clever to understand -C, so
  ;; use GNU make (installed as "gmake").
  (case target
    ((:solarisx8664 :solarisx8632) "gmake")
    (t "make")))

(defparameter *known-optional-features* '(:count-gf-calls :monitor-futex-wait :unique-dcode))
(defvar *build-time-optional-features* nil)


(defun rebuild-ccl (&key update full clean kernel force (reload t) exit reload-arguments verbose optional-features)
  (let* ((*build-time-optional-features* (intersection *known-optional-features* optional-features))
         (*features* (append *build-time-optional-features* *features*)))
    (when *build-time-optional-features*
      (setq full t))
    (when full
      (setq clean t kernel t reload t))
    (when update (update-ccl :verbose (not (eq update :quiet))))
    (when (or clean force)
      ;; for better bug reports...
      (format t "~&Rebuilding ~a using ~a"
              (lisp-implementation-type)
              (lisp-implementation-version)))
    (let* ((cd (current-directory)))
      (unwind-protect
           (progn
             (setf (current-directory) "ccl:")
             (when clean
               (dolist (f (directory
                           (merge-pathnames
                            (make-pathname :name :wild
                                           :type (pathname-type *.fasl-pathname*))
                            "ccl:**;")))
                 (delete-file f)))
             (when kernel
               (when (or clean force)
                 ;; Do a "make -k clean".
                 (run-program "make"
                              (list "-k"
                                    "-C"
                                    (format nil "lisp-kernel/~a"
                                            (kernel-build-directory))
                                    "clean")))
               (format t "~&;Building lisp-kernel ...")
               (with-output-to-string (s)
                                      (multiple-value-bind
                                          (status exit-code)
                                          (external-process-status 
                                           (run-program (make-program)
                                                        (list "-k" "-C" 
                                                              (format nil "lisp-kernel/~a"
                                                                      (kernel-build-directory))
                                                              "-j"
                                                            
                                                              (format nil "~d" (1+ (cpu-count))))
                                                        :output s
                                                        :error :output))
                                        (if (and (eq :exited status) (zerop exit-code))
                                          (progn
                                            (format t "~&;Kernel built successfully.")
                                            (when verbose
                                              (format t "~&;kernel build output:~%~a"
                                                      (get-output-stream-string s)))
                                            (sleep 1))
                                          (error "Error(s) during kernel compilation.~%~a"
                                                 (get-output-stream-string s))))))
             (compile-ccl (not (null force)))
             (if force (xload-level-0 :force) (xload-level-0))
             (when reload
               (with-input-from-string (cmd (format nil
                                                    "(save-application ~s)"
                                                    (standard-image-name)))
                 (with-output-to-string (output)
                                        (multiple-value-bind (status exit-code)
                                            (external-process-status
                                             (run-program
                                              (format nil "./~a" (standard-kernel-name))
                                              (list* "--image-name" (standard-boot-image-name)
                                                     reload-arguments)
                                              :input cmd
                                              :output output
                                              :error output))
                                          (if (and (eq status :exited)
                                                   (eql exit-code 0))
                                            (progn
                                              (format t "~&;Wrote heap image: ~s"
                                                      (truename (format nil "ccl:~a"
                                                                        (standard-image-name))))
                                              (when verbose
                                                (format t "~&;Reload heap image output:~%~a"
                                                        (get-output-stream-string output))))
                                            (error "Errors (~s ~s) reloading boot image:~&~a"
                                                   status exit-code
                                                   (get-output-stream-string output)))))))
             (when exit
               (quit)))
        (setf (current-directory) cd)))))
                                                  
               
(defun create-interfaces (dirname &key target populate-arg)
  (let* ((backend (if target (find-backend target) *target-backend*))
         (*default-pathname-defaults* nil)
         (ftd (backend-target-foreign-type-data backend))
         (d (use-interface-dir dirname ftd))
         (populate (merge-pathnames "C/populate.sh"
                                    (merge-pathnames
                                     (interface-dir-subdir d)
                                     (ftd-interface-db-directory ftd))))
         (cdir (make-pathname :directory (pathname-directory (translate-logical-pathname populate))))
         (args (list "-c"
                     (format nil "cd ~a && /bin/sh ~a ~@[~a~]"
                             (native-translated-namestring cdir)
                             (native-translated-namestring populate)
                             populate-arg))))
    (format t "~&;[Running interface translator via ~s to produce .ffi file(s) from headers]~&" populate)
    (force-output t)
    (multiple-value-bind (status exit-code)
        (external-process-status
         (run-program "/bin/sh" args :output t))
      (if (and (eq status :exited)
               (eql exit-code 0))
        (let* ((f 'parse-standard-ffi-files))
          (require "PARSE-FFI")
          (format t "~%~%;[Parsing .ffi files; may create new .cdb files for ~s]" dirname)
          (funcall f dirname target)
          (format t "~%~%;[Parsing .ffi files again to resolve forward-referenced constants]")
          (funcall f dirname target))))))

(defun update-ccl (&key (verbose t))
  (let* ((changed ())
         (conflicts ()))
    (with-output-to-string (out)
      (with-preserved-working-directory ("ccl:")                     
        (when verbose (format t "~&;Running 'svn update'."))
        (multiple-value-bind (status exit-code)
            (external-process-status
             (run-program "svn" '("update" "--non-interactive") :output out :error t))
          (when verbose (format t "~&;'svn update' complete."))
          (if (not (and (eq status :exited)
                        (eql exit-code 0)))
            (error "Running \"svn update\" produced exit status ~s, code ~s." status exit-code)
            (let* ((sout (get-output-stream-string out))
                   (added ())
                   (deleted ())
                   (updated ())
                   (merged ())
                   (binaries (list (standard-kernel-name) (standard-image-name )))
                   (peer (peer-platform)))
              (when peer
                (push (standard-kernel-name peer) binaries)
                (push (standard-image-name peer) binaries))
              (flet ((svn-revert (string)
                       (multiple-value-bind (status exit-code)
                           (external-process-status (run-program "svn" `("revert" ,string)))
                         (when (and (eq status :exited) (eql exit-code 0))
                           (setq conflicts (delete string conflicts :test #'string=))
                           (push string updated)))))
                (with-input-from-string (in sout)
                  (do* ((line (read-line in nil nil) (read-line in nil nil)))
                       ((null line))
                    (when (and (> (length line) 2)
                               (eql #\space (schar line 1)))
                      (let* ((path (string-trim " " (subseq line 2))))
                        (case (schar line 0)
                          (#\A (push path added))
                          (#\D (push path deleted))
                          (#\U (push path updated))
                          (#\G (push path merged))
                          (#\C (push path conflicts)))))))
                ;; If the kernel and/or image conflict, use "svn revert"
                ;; to replace the working copies with the (just updated)
                ;; repository versions.
                (setq changed (if (or added deleted updated merged conflicts) t))
              
                (dolist (f binaries)
                  (when (member f conflicts :test #'string=)
                    (svn-revert f)))
                ;; If there are any remaining conflicts, offer
                ;; to revert them.
                (when conflicts
                  (with-preserved-working-directory ()
                    (cerror "Discard local changes to these files (using 'svn revert'."
                            "'svn update' was unable to merge local changes to the following file~p with the updated versions:~{~&~s~~}" (length conflicts) conflicts)
                    (dolist (c (copy-list conflicts))
                      (svn-revert c))))
                ;; Report other changes, if verbose.
                (when (and verbose
                           (or added deleted updated merged conflicts))
                  (format t "~&;Changes from svn update:")
                  (flet ((show-changes (herald files)
                           (when files
                             (format t "~&; ~a:~{~&;  ~a~}"
                                     herald files))))
                    (show-changes "Conflicting files" conflicts)
                    (show-changes "New files/directories" added)
                    (show-changes "Deleted files/directories" deleted)
                    (show-changes "Updated files" updated)
                    (show-changes "Files with local changes, successfully merged" merged)))))))))
    (values changed conflicts)))

(defmacro with-preserved-working-directory ((&optional dir) &body body)
  (let ((wd (gensym)))
    `(let ((,wd (mac-default-directory)))
       (unwind-protect
	    (progn 
	      ,@(when dir `((cwd ,dir)))
	      ,@body)
	 (cwd ,wd)))))

(defun ensure-tests-loaded (&key force update ansi ccl)
  (unless (and (find-package "REGRESSION-TEST") (not force))
    (if (probe-file "ccl:tests;ansi-tests;")
      (when update
	(cwd "ccl:tests;")
	(run-program "svn" '("update")))
      (let* ((svn (probe-file "ccl:.svn;entries"))
	     (repo (and svn (svn-repository)))
	     (s (make-string-output-stream)))
	(when repo
	  (format t "~&Checking out test suite into ccl:tests;~%")
	  (cwd "ccl:")
	  (multiple-value-bind (status exit-code)
	      (external-process-status
	       (run-program "svn" (list "checkout" (format nil "~a/trunk/tests" repo) "tests")
			    :output s
			    :error s))
	    (unless (and (eq status :exited)
			 (eql exit-code 0))
	      (error "Failed to check out test suite: ~%~a" (get-output-stream-string s)))))))
    (cwd "ccl:tests;ansi-tests;")
    (run-program "make" '("-k" "clean"))
    (map nil 'delete-file (directory "*.*fsl"))
    ;; Muffle the typecase "clause ignored" warnings, since there is really nothing we can do about
    ;; it without making the test suite non-portable across platforms...
    (handler-bind ((warning (lambda (c)
			      (when (let ((w (or (and (typep c 'compiler-warning)
                                                      (eq (compiler-warning-warning-type c) :program-error)
                                                      (car (compiler-warning-args c)))
                                                 c)))
                                      (and (typep w 'simple-warning)
                                           (or 
                                            (string-equal
                                             (simple-condition-format-control w)
                                             "Clause ~S ignored in ~S form - shadowed by ~S .")
                                            ;; Might as well ignore these as well, they're intentional.
                                            (string-equal
                                             (simple-condition-format-control w)
                                             "Duplicate keyform ~s in ~s statement."))))
				(muffle-warning c)))))
      ;; This loads the infrastructure
      (load "ccl:tests;ansi-tests;gclload1.lsp")
      ;; This loads the actual tests
      (let ((redef-var (find-symbol "*WARN-IF-REDEFINE-TEST*" :REGRESSION-TEST)))
	(progv (list redef-var) (list (if force nil (symbol-value redef-var)))
          (when ansi
            (load "ccl:tests;ansi-tests;gclload2.lsp"))
	  ;; And our own tests
          (when ccl
            (load "ccl:tests;ansi-tests;ccl.lsp")))))))

(defun test-ccl (&key force (update t) verbose (catch-errors t) (ansi t) (ccl t)
                      optimization-settings)
  (with-preserved-working-directory ()
    (let* ((*package* (find-package "CL-USER")))
      (ensure-tests-loaded :force force :update update :ansi ansi :ccl ccl)
      (cwd "ccl:tests;ansi-tests;")
      (let ((do-tests (find-symbol "DO-TESTS" "REGRESSION-TEST"))
            (*print-catch-errors* nil))
        (time (funcall do-tests :verbose verbose :compile t
                       :catch-errors catch-errors
                       :optimization-settings (or optimization-settings '((safety 2))))))
      ;; Ok, here we would run any of our own tests.
      )))
