(in-package "CCL")

(defun x86-solaris32::record-type-returns-structure-as-first-arg (rtype)
  (x8632::record-type-returns-structure-as-first-arg rtype))

(defun x86-solaris32::expand-ff-call (callform args &key (arg-coerce #'null-coerce-foreign-arg) (result-coerce #'null-coerce-foreign-result))
  (x8632::expand-ff-call callform args :arg-coerce arg-coerce :result-coerce result-coerce))

(defun x86-solaris32::generate-callback-bindings (stack-ptr fp-args-ptr argvars argspecs result-spec struct-result-name)
  (x8632::generate-callback-bindings stack-ptr fp-args-ptr argvars argspecs result-spec struct-result-name))

(defun x86-solaris32::generate-callback-return-value (stack-ptr fp-args-ptr result return-type struct-return-arg)
  (x8632::generate-callback-return-value stack-ptr fp-args-ptr result return-type struct-return-arg))
