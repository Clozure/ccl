;;-*-Mode: LISP; Package: CCL -*-
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

;;; Load .pfsl files, create a "runtime" (compiler- & development-tools-less)
;;; image.

(in-package "CCL")

(%fasload "./l1-pfsls/l1-cl-package.pfsl")
(%fasload "./l1-pfsls/l1-utils.pfsl")
(%fasload "./l1-pfsls/l1-init.pfsl")
(%fasload "./l1-pfsls/l1-symhash.pfsl")
(%fasload "./l1-pfsls/l1-numbers.pfsl")
(%fasload "./l1-pfsls/l1-aprims.pfsl")
(%fasload "./l1-pfsls/ppc-callback-support.pfsl")
(%fasload "./l1-pfsls/l1-sort.pfsl")
(%fasload "./l1-pfsls/l1-dcode.pfsl")
(%fasload "./l1-pfsls/l1-clos.pfsl")
(%fasload "./binppc/defstruct.pfsl")
(%fasload "./l1-pfsls/l1-streams.pfsl")
(%fasload "./l1-pfsls/linux-files.pfsl")
(%fasload "./binppc/lists.pfsl")
(%fasload "./binppc/sequences.pfsl")
(%fasload "./binppc/chars.pfsl")
(%fasload "./l1-pfsls/l1-files.pfsl")
(provide "SEQUENCES")
(provide "DEFSTRUCT")
(provide "CHARS")
(provide "LISTS")
(%fasload "./l1-pfsls/ppc-stack-groups.pfsl")
(%fasload "./l1-pfsls/l1-stack-groups.pfsl")
(%fasload "./l1-pfsls/l1-processes.pfsl")
(%fasload "./l1-pfsls/l1-io.pfsl")
(%fasload "./l1-pfsls/l1-reader.pfsl")
(%fasload "./l1-pfsls/l1-readloop.pfsl")
(%fasload "./l1-pfsls/l1-readloop-lds.pfsl")
(%fasload "./l1-pfsls/l1-error-system.pfsl")

(%fasload "./l1-pfsls/l1-events.pfsl")
(%fasload "./l1-pfsls/ppc-trap-support.pfsl")
(%fasload "./l1-pfsls/l1-format.pfsl")
(%fasload "./l1-pfsls/l1-sysio.pfsl")
(%fasload "./l1-pfsls/l1-pathnames.pfsl")
(%fasload "./l1-pfsls/version.pfsl")
(%fasload "./l1-pfsls/l1-boot-lds.pfsl")

(%fasload "./l1-pfsls/l1-boot-1.pfsl")
(catch :toplevel
    (%fasload "./l1-pfsls/l1-typesys.pfsl")
    (%fasload "./l1-pfsls/sysutils.pfsl")
    (%fasload "./l1-pfsls/l1-error-signal.pfsl")
    (setq *LEVEL-1-LOADED* t))

(def-ccl-pointers fd-streams ()
  (let* ((in (make-fd-stream 0 :direction :input))
         (out (make-fd-stream 1 :direction :output))
         (error out))
    (setq *terminal-io* (make-echoing-two-way-stream in out))
    (setq *debug-io* (make-echoing-two-way-stream in error)
          *query-io* *debug-io*)
    (setq *standard-input* in
          *standard-output* out
          *error-output* error
          *trace-output* error)))

(catch :toplevel
    (flet ((load-provide (module path)
             (let* ((*package* *package*))
               (%fasload path)
               (provide module))))
      (load-provide "SORT" "./binppc/sort.pfsl")
      (load-provide "NUMBERS" "./binppc/numbers.pfsl")
      (load-provide "HASH" "./binppc/hash.pfsl")
;;;   (load-provide "DLL-NODE" "./binppc/dll-node.pfsl")
;;;   (load-provide "PPC32-ARCH" "./binppc/ppc32-arch.pfsl")
;;;   (load-provide "VREG" "./binppc/vreg.pfsl")
;;;   (load-provide "PPC-ASM" "./binppc/ppc-asm.pfsl")
;;;   (load-provide "VINSN" "./binppc/vinsn.pfsl")
;;;   (load-provide "PPC-VINSNS" "./binppc/ppc-vinsns.pfsl")
;;;   (load-provide "PPC-REG" "./binppc/ppc-reg.pfsl")
;;;   (load-provide "SUBPRIMS" "./binppc/subprims.pfsl")
;;;   (load-provide "PPC-LAP" "./binppc/ppc-lap.pfsl")
;;;   (provide "PPC2")                  ; Lie, load the module manually
;;;   (load-provide "NX" "./l1-pfsls/nx.pfsl")
;;;   (%fasload "./binppc/ppc2.pfsl")
      (load-provide "LEVEL-2" "./binppc/level-2.pfsl")
;;;     (load-provide "SETF" "./binppc/setf.pfsl")
      (load-provide "SETF-RUNTIME" "./binppc/setf-runtime.pfsl")
      (load-provide "FORMAT" "./binppc/format.pfsl")
      (load-provide "STREAMS" "./binppc/streams.pfsl")
;;;   (load-provide "OPTIMIZERS" "./binppc/optimizers.pfsl")
;;;   (load-provide "PPC-OPTIMIZERS" "./binppc/ppc-optimizers.pfsl")
;;;   (load-provide "LISPEQU" "./library/lispequ.pfsl")          ; Shouldn't need this at load time ...
;;;   (load-provide "DEFSTRUCT-MACROS" "./binppc/defstruct-macros.pfsl")        ;  ... but this file thinks it does.
;;;   (load-provide "DEFSTRUCT-LDS" "./binppc/defstruct-lds.pfsl")
;;;   (load-provide "NFCOMP" "./binppc/nfcomp.pfsl")
;;;   (load-provide "BACKQUOTE" "./binppc/backquote.pfsl")
      (load-provide "BACKTRACE-LDS" "./binppc/backtrace-lds.pfsl")
      (load-provide "BACKTRACE" "./binppc/backtrace.pfsl")
      (load-provide "READ" "./binppc/read.pfsl")
      (load-provide "ARRAYS-FRY" "./binppc/arrays-fry.pfsl")
;;;   (load-provide "APROPOS" "./binppc/apropos.pfsl")
;;;   (load-provide "PPC-DISASSEMBLE" "./binppc/ppc-disassemble.pfsl")
;;;   (load-provide "PPC-LAPMACROS" "./binppc/ppc-lapmacros.pfsl")
;;;   (load-provide "MACTYPES" "./binppc/mactypes.pfsl")
;;;   (load-provide "DEFRECORD" "./binppc/defrecord.pfsl")
;;;   (load-provide "LINUX-RECORDS" "./library/linux-records.pfsl")
      (load-provide "CASE-ERROR" "./binppc/case-error.pfsl")
;;;   (load-provide "ENCAPSULATE" "./binppc/encapsulate.pfsl")
      (load-provide "METHOD-COMBINATION" "./binppc/method-combination.pfsl")
      (load-provide "MISC" "./binppc/misc.pfsl")
      (load-provide "PPRINT" "./binppc/pprint.pfsl")
      (load-provide "DUMPLISP" "./binppc/dumplisp.pfsl")
      (load-provide "PATHNAMES" "./binppc/pathnames.pfsl")
      (load-provide "TIME" "./binppc/time.pfsl")
;;;   (load-provide "COMPILE-CCL" "./binppc/compile-ccl.pfsl")
;;;   (load-provide "SOURCE-FILES" "./binppc/source-files.pfsl")
      (load-provide "CCL-EXPORT-SYMS" "./binppc/ccl-export-syms.pfsl")
      )
    (setq *%fasload-verbose* nil)
    )
(catch :toplevel
    (or (find-package "COMMON-LISP-USER")
        (make-package "COMMON-LISP-USER" :use '("COMMON-LISP" "CCL") :NICKNAMES '("CL-USER")))
)

(defvar *LISTENER-PROCESS-STACKSEG-SIZE* (* 4 16384))

(setf (interrupt-level) 0)

(setq *warn-if-redefine* t)

(setq *level-1-loaded* t)

(set-periodic-task-interval 1)

(do-all-symbols (s)
  (setf (symbol-plist s) nil))

(progn (%set-toplevel #'toplevel-loop) (save-application "RUNTIME"))





