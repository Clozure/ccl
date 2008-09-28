;;;-*- Mode: Lisp; Package: CCL -*-

(in-package "CCL")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "BACKEND"))

(eval-when (:compile-toplevel :execute)
  (require "NXENV")
  (require "X8632ENV"))

(defvar *x8632-vinsn-templates* (make-hash-table :test #'eq))

(defvar *known-x8632-backends* ())

#+darwinx86-target
(defvar *darwinx8632-backend*
  (make-backend :lookup-opcode 'lookup-x86-opcode
                :lookup-macro #'false
                :lap-opcodes x86::*x86-opcode-templates*
                :define-vinsn 'define-x86-vinsn
                :p2-dispatch *x862-specials*
                :p2-vinsn-templates *x8632-vinsn-templates*
                :p2-template-hash-name '*x8632-vinsn-templates*
                :p2-compile 'x862-compile
                :platform-syscall-mask (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) 
                :target-specific-features
                '(:x8632 :x86-target :darwin-target :darwinx86-target :x8632-target
                  :darwinx8632-target
                  :little-endian-target
                  :32-bit-target)
                :target-fasl-pathname (make-pathname :type "dx32fsl")
                :target-platform (logior platform-cpu-x86
                                         platform-os-darwin
                                         platform-word-size-32)
                :target-os :darwinx86
                :name :darwinx8632
                :target-arch-name :x8632
                :target-foreign-type-data nil
                :target-arch x8632::*x8632-target-arch*
                :lisp-context-register x8632::fs
		:num-arg-regs 2
                ))


#+darwinx86-target
(pushnew *darwinx8632-backend* *known-x8632-backends* :key #'backend-name)

#+linuxx86-target
(defvar *linuxx8632-backend*
  (make-backend :lookup-opcode 'lookup-x86-opcode
                :lookup-macro #'false
                :lap-opcodes x86::*x86-opcode-templates*
                :define-vinsn 'define-x86-vinsn
                :p2-dispatch *x862-specials*
                :p2-vinsn-templates *x8632-vinsn-templates*
                :p2-template-hash-name '*x8632-vinsn-templates*
                :p2-compile 'x862-compile
                :platform-syscall-mask (logior platform-os-linux platform-cpu-x86 platform-word-size-32) 
                :target-specific-features
                '(:x8632 :x86-target :linux-target :linuxx86-target :x8632-target
                  :linuxx8632-target
                  :little-endian-target
                  :32-bit-target)
                :target-fasl-pathname (make-pathname :type "lx32fsl")
                :target-platform (logior platform-cpu-x86
                                         platform-os-linux
                                         platform-word-size-32)
                :target-os :linuxx86
                :name :linuxx8632
                :target-arch-name :x8632
                :target-foreign-type-data nil
                :target-arch x8632::*x8632-target-arch*
                :lisp-context-register x8632::fs
		:num-arg-regs 2
                ))

#+linuxx86-target
(pushnew *linuxx8632-backend* *known-x8632-backends* :key #'backend-name)

#+windows-target
(defvar *win32-backend*
  (make-backend :lookup-opcode 'lookup-x86-opcode
                :lookup-macro #'false
                :lap-opcodes x86::*x86-opcode-templates*
                :define-vinsn 'define-x86-vinsn
                :p2-dispatch *x862-specials*
                :p2-vinsn-templates *x8632-vinsn-templates*
                :p2-template-hash-name '*x8632-vinsn-templates*
                :p2-compile 'x862-compile
                :platform-syscall-mask (logior platform-os-windows platform-cpu-x86 platform-word-size-32) 
                :target-specific-features
                '(:x8632 :x86-target :windows-target :win32-target :x8632-target
                  :windowsx8632-target
                  :little-endian-target
                  :32-bit-target)
                :target-fasl-pathname (make-pathname :type "wx32fsl")
                :target-platform (logior platform-cpu-x86
                                         platform-os-windows
                                         platform-word-size-32)
                :target-os :win32
                :name :win32
                :target-arch-name :x8632
                :target-foreign-type-data nil
                :target-arch x8632::*x8632-target-arch*
                :lisp-context-register x8632::gs
		:num-arg-regs 2
                ))

#+windows-target
(pushnew *win32-backend* *known-x8632-backends* :key #'backend-name)


(defvar *x8632-backend* (car *known-x8632-backends*))

(defun fixup-x8632-backend ()
  (dolist (b *known-x8632-backends*)
    (setf #| (backend-lap-opcodes b) x86::*x86-opcodes* |#
          (backend-p2-dispatch b) *x862-specials*
          (backend-p2-vinsn-templates b)  *x8632-vinsn-templates*)
    (or (backend-lap-macros b) (setf (backend-lap-macros b)
                                     (make-hash-table :test #'equalp)))))


(fixup-x8632-backend)

#+x8632-target
(setq *host-backend* *x8632-backend* *target-backend* *x8632-backend*)


(defun setup-x8632-ftd (backend)
  (or (backend-target-foreign-type-data backend)
      (let* ((name (backend-name backend))
             (ftd
              (case name
                (:darwinx8632
                 (make-ftd :interface-db-directory "ccl:darwin-x86-headers;"
			   :interface-package-name "X86-DARWIN32"
                           :attributes '(:bits-per-word  32
                                         :signed-char t
                                         :struct-by-value t
                                         :prepend-underscore t)
                           :ff-call-expand-function
                           (intern "EXPAND-FF-CALL" "X86-DARWIN32")
			   :ff-call-struct-return-by-implicit-arg-function
                           (intern "RECORD-TYPE-RETURNS-STRUCTURE-AS-FIRST-ARG"
                                   "X86-DARWIN32")
                           :callback-bindings-function
                           (intern "GENERATE-CALLBACK-BINDINGS" "X86-DARWIN32")
                           :callback-return-value-function
                           (intern "GENERATE-CALLBACK-RETURN-VALUE" "X86-DARWIN32")))
                (:linuxx8632
                 (make-ftd :interface-db-directory "ccl:x86-headers;"
			   :interface-package-name "X86-LINUX32"
                           :attributes '(:bits-per-word  32
                                         :signed-char nil
                                         :struct-by-value t
                                         :float-results-in-x87 t)
                           :ff-call-expand-function
                           (intern "EXPAND-FF-CALL" "X86-LINUX32")
			   :ff-call-struct-return-by-implicit-arg-function
                           (intern "RECORD-TYPE-RETURNS-STRUCTURE-AS-FIRST-ARG"
                                   "X86-LINUX32")
                           :callback-bindings-function
                           (intern "GENERATE-CALLBACK-BINDINGS" "X86-LINUX32")
                           :callback-return-value-function
                           (intern "GENERATE-CALLBACK-RETURN-VALUE" "X86-LINUX32")))
                (:win32
                 (make-ftd :interface-db-directory "ccl:win32-headers;"
			   :interface-package-name "WIN32"
                           :attributes '(:bits-per-word  32
                                         :signed-char nil
                                         :struct-by-value t
                                         :float-results-in-x87 t)
                           :ff-call-expand-function
                           (intern "EXPAND-FF-CALL" "WIN32")
			   :ff-call-struct-return-by-implicit-arg-function
                           (intern "RECORD-TYPE-RETURNS-STRUCTURE-AS-FIRST-ARG"
                                   "WIN32")
                           :callback-bindings-function
                           (intern "GENERATE-CALLBACK-BINDINGS" "WIN32")
                           :callback-return-value-function
                           (intern "GENERATE-CALLBACK-RETURN-VALUE" "WIN32")))
                )))
        (install-standard-foreign-types ftd)
        (use-interface-dir :libc ftd)
        (setf (backend-target-foreign-type-data backend) ftd))))

#-x8632-target
(setup-x8632-ftd *x8632-backend*)

(pushnew *x8632-backend* *known-backends* :key #'backend-name)

#+x8632-target
(require "X8632-VINSNS")

(provide "X8632-BACKEND")

