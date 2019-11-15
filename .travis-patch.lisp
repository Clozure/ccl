(in-package :ccl)

(setf *warn-if-redefine-kernel* nil)

;;; Check that something that's supposed to be a proper list of
;;; symbols is; error otherwise.
;;; This is called only by the compiler output of a PROGV form.
;;; It checks for the maximum length that the progvsave subprim
;;; can handle.
(defun %progv-check-symbol-list (list)
  (let ((length (list-length list)))
    (when (not length)
      (error 'simple-program-error
             :format-control "PROGV: ~S is not a proper list."
             :format-arguments (list list)))
    (dolist (symbol list)
      (unless (symbolp symbol)
        (error 'simple-type-error
               :expected-type 'symbol :datum symbol
               :format-control "PROGV: ~S is not a symbol."
               :format-arguments (list symbol)))
      (when (constant-symbol-p symbol)
        (error 'simple-program-error
               :format-control "PROGV: symbol ~S names a constant variable which cannot be rebound."
               :format-arguments (list symbol)))
      (when (logbitp $sym_vbit_global (the fixnum (%symbol-bits symbol)))
        (error 'simple-program-error
               :format-control "PROGV: symbol ~S names a global variable which cannot be rebound."
               :format-arguments (list symbol)))
      (ensure-binding-index symbol))
    list))

(defnx1 nx1-progv progv context (symbols values &body body)
  (make-acode (%nx1-operator progv)
              (nx1-form :value `(%progv-check-symbol-list ,symbols))
              (nx1-form :value values)
              (nx1-catch-body context body)))

(in-package :cl-user)
