(in-package "CCL")

;;; On FreeBSD, the C compiler returns small structures in registers
;;; (just like on Darwin, apparently).
(defun x86-freebsd32::record-type-returns-structure-as-first-arg (rtype)
  (when (and rtype
	     (not (typep rtype 'unsigned-byte))
	     (not (member rtype *foreign-representation-type-keywords*
			  :test #'eq)))
    (let* ((ftype (if (typep rtype 'foreign-type)
		    rtype
		    (parse-foreign-type rtype)))
	   (nbits (ensure-foreign-type-bits ftype)))
      (not (member nbits '(8 16 32 64))))))

(defun x86-freebsd32::expand-ff-call (callform args &key (arg-coerce #'null-coerce-foreign-arg) (result-coerce #'null-coerce-foreign-result))
  (x8632::expand-ff-call callform args :arg-coerce arg-coerce :result-coerce result-coerce))

(defun x86-freebsd32::generate-callback-bindings (stack-ptr fp-args-ptr argvars argspecs result-spec struct-result-name)
  (x8632::generate-callback-bindings stack-ptr fp-args-ptr argvars argspecs result-spec struct-result-name))

(defun x86-freebsd32::generate-callback-return-value (stack-ptr fp-args-ptr result return-type struct-return-arg)
  (x8632::generate-callback-return-value stack-ptr fp-args-ptr result return-type struct-return-arg))

