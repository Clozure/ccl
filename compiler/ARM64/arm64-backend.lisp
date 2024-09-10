(in-package "CCL")

(next-nx-defops)
(defvar *arm642-specials* nil)
(let* ((newsize (%i+ (next-nx-num-ops) 10))
       (old *arm642-specials*)
       (oldsize (length old)))
  (declare (fixnum newsize oldsize))
  (unless (>= oldsize newsize)
    (let* ((v (make-array newsize :initial-element nil)))
      (dotimes (i oldsize (setq *arm642-specials* v))
        (setf (svref v i) (svref old i))))))

(defvar *arm64-vinsn-templates* (make-hash-table :test #'eq))
(defvar *known-arm64-backends* ())

;;; This defines a template.  All expressions in the body must be
;;; evaluable at macroexpansion time.
(defun %define-arm-vinsn (backend vinsn-name results args temps body)
)

#+(or darwinarm64-target (not arm-target))
(defvar *darwinarm64-backend*
  (make-backend :lookup-opcode #'arm64::lookup-instruction
		:lookup-macro #'false
		:lap-opcodes arm64::*instruction-table*
                :define-vinsn '%define-arm-vinsn
                :platform-syscall-mask (logior platform-os-darwin
                                               platform-cpu-arm
                                               platform-word-size-64)
		:p2-dispatch *arm642-specials*
		:p2-vinsn-templates *arm64-vinsn-templates*
		:p2-template-hash-name '*arm64-vinsn-templates*
		:p2-compile 'arm642-compile
		:target-specific-features
		'(:arm64 :arm64-target :darwin-target :darwinarm64-target
                  :64-bit-target :little-endian-target)
		:target-fasl-pathname (make-pathname :type "da64fsl")
		:target-platform (logior platform-os-darwin
                                         platform-cpu-arm
                                         platform-word-size-64)
		:target-os :darwinarm64
		:name :darwinarm64
		:target-arch-name :arm64
		:target-foreign-type-data nil
                :target-arch arm64::*arm64-target-arch*))

#+(or linuxarm64-target (not arm-target))
(defvar *linuxarm64-backend*
  (make-backend :lookup-opcode #'arm64::lookup-instruction
		:lookup-macro #'false
		:lap-opcodes arm64::*instruction-table*
                :define-vinsn '%define-arm-vinsn
                :platform-syscall-mask (logior platform-os-linux
                                               platform-cpu-arm
                                               platform-word-size-64)
		:p2-dispatch *arm642-specials*
		:p2-vinsn-templates *arm64-vinsn-templates*
		:p2-template-hash-name '*arm64-vinsn-templates*
		:p2-compile 'arm642-compile
		:target-specific-features
		'(:arm64 :arm64-target :linux-target :linuxarm64-target
                  :64-bit-target :little-endian-target)
		:target-fasl-pathname (make-pathname :type "la64fsl")
		:target-platform (logior platform-os-linux
                                         platform-cpu-arm
                                         platform-word-size-64)
		:target-os :linuxarm64
		:name :linuxarm64
		:target-arch-name :arm64
		:target-foreign-type-data nil
                :target-arch arm64::*arm64-target-arch*))

#+(or darwinarm64-target (not arm-target))
(pushnew *darwinarm64-backend* *known-arm64-backends* :key #'backend-name)

#+(or linuxarm64-target (not arm-target))
(pushnew *linuxarm64-backend* *known-arm64-backends* :key #'backend-name)

(defvar *arm64-backend* (car *known-arm64-backends*))

(defun fixup-arm64-backend ()
  (dolist (b *known-arm64-backends*)
    (setf (backend-lap-opcodes b) arm64::*instruction-table*
	  (backend-p2-dispatch b) *arm642-specials*
	  (backend-p2-vinsn-templates b)  *arm64-vinsn-templates*)
    (or (backend-lap-macros b) (setf (backend-lap-macros b)
                                     (make-hash-table :test #'equalp)))))

(fixup-arm64-backend)

#+arm64-target
(setq *host-backend* *arm64-backend* *target-backend* *arm64-backend*)

(defun setup-arm64-ftd (backend)
  (or (backend-target-foreign-type-data backend)
      (let* ((name (backend-name backend))
             (ftd
               (case name
                 (:darwinarm64
                  (make-ftd
                   :interface-db-directory "ccl:darwin-arm64-headers;"
		   :interface-package-name "ARM64-DARWIN"
                   :attributes '(:bits-per-word 64
                                 :signed-char t
                                 :struct-by-value t
                                 :natural-alignment t
                                 :prepend-underscore nil)
                   :ff-call-expand-function
                   (intern "EXPAND-FF-CALL" "ARM64-DARWIN")
		   :ff-call-struct-return-by-implicit-arg-function
                   (intern "RECORD-TYPE-RETURNS-STRUCTURE-AS-FIRST-ARG"
                           "ARM64-DARWIN")
                   :callback-bindings-function
                   (intern "GENERATE-CALLBACK-BINDINGS" "ARM64-DARWIN")
                   :callback-return-value-function
                   (intern "GENERATE-CALLBACK-RETURN-VALUE" "ARM64-DARWIN")))
                 (:linuxarm64
                  (make-ftd
                   :interface-db-directory "ccl:arm64-headers;"
		   :interface-package-name "ARM64-LINUX"
                   :attributes '(:bits-per-word 64
                                 :signed-char nil
                                 :natural-alignment t
                                 :struct-by-value t)
                   :ff-call-expand-function
                   (intern "EXPAND-FF-CALL" "ARM64-LINUX")
		   :ff-call-struct-return-by-implicit-arg-function
                   (intern "RECORD-TYPE-RETURNS-STRUCTURE-AS-FIRST-ARG"
                           "ARM64-LINUX")
                   :callback-bindings-function
                   (intern "GENERATE-CALLBACK-BINDINGS" "ARM64-LINUX")
                   :callback-return-value-function
                   (intern "GENERATE-CALLBACK-RETURN-VALUE" "ARM64-LINUX"))))))
        (install-standard-foreign-types ftd)
        (use-interface-dir :libc ftd)
        (setf (backend-target-foreign-type-data backend) ftd))))

(pushnew *arm64-backend* *known-backends*)
#-arm64-target
(progn
  (pushnew *darwinarm64-backend* *known-backends* :key #'backend-name)
  (pushnew *linuxarm64-backend* *known-backends* :key #'backend-name))

;;; FFI stuff

;;; If the type, T, of the result of a function is such that
;;;
;;;   void func(T arg)
;;;
;;; would require that arg be passed as a value in a register (or set
;;; of registers) according to the rules in Parameter passing, then
;;; the result is returned in the same registers as would be used for
;;; such an argument.
;;;
;;; Otherwise, the caller shall reserve a block of memory of
;;; sufficient size and alignment to hold the result. The address of
;;; the memory block shall be passed as an additional argument to the
;;; function in x8. The callee may modify the result memory block at
;;; any point during the execution of the subroutine (there is no
;;; requirement for the callee to preserve the value stored in x8).
;;;
;;;                                - Procedure Call Standard, § 6.9

;;; If a returned struct is more than 16 bytes long, the caller will
;;; reserve memory for the return value and pass a pointer to that
;;; memory in register x8.  (Not as a hidden first argument, despite
;;; the name of this function.)
(defun arm64::record-type-returns-structure-as-first-arg (rtype)
  (when (and rtype
             (not (typep rtype 'unsigned-byte))
             (not (member rtype *foreign-representation-type-keywords*
                          :test #'eq)))
    (let* ((ftype (if (typep rtype 'foreign-type)
                    rtype
                    (parse-foreign-type rtype))))
      (> (ensure-foreign-type-bits ftype) 128))))

(defun arm64::expand-ff-call (callform args
                              &key
                                (arg-coerce #'null-coerce-foreign-arg)
                                (result-coerce #'null-coerce-foreign-result)))

