(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "ARM64-ARCH")
  (require "DLL-NODE")
  (require "ARM64-ASM"))

(defun %define-arm64-lap-function (name body &optional (bits 0))
  (with-dll-node-freelist (primary arm64::*lap-instruction-freelist*)
    (with-dll-node-freelist (data arm64::*lap-instruction-freelist*)
      (let* ((arm64::*lap-labels* ())
             (name-cell (list name))
             (arm64::*constants* ())
             (current primary)
             (arm64::*register-names* arm64::*standard-register-names*)
             (sections (vector primary data)))
        (dolist (form body)
          (arm64-lap-form form))
        (arm64-lap-generate-code name maxpc *arm64-lfun-bits*)
        (rplacd name-cell (length arm64::*constants*))
        (push name-cell arm64::*constants*)))))

(defun arm64-lap-generate-code (name maxpc &optional (bits 0)))

                                                        
;;; (let ((name val) ...) &body body)
;;; each "val" gets a chance to be treated as an ARM64 register name
;;; before being evaluated.
(defun arm64-lap-equate-form (eqlist body current sections)
  (collect ((symbols)
            (vals))
    (let* ((arm64::*register-names* arm64::*register-names*))
      (dolist (pair eqlist)
        (destructuring-bind (symbol value) pair
          (unless (and symbol
                       (symbolp symbol)
                       (not (constant-symbol-p symbol))
                       (not (arm64::lookup-register symbol)))
            (error "~s is not a bindable symbol name . " symbol))
          (let* ((regval (and value
                              (or (typep value 'symbol)
                                  (typep value 'string))
                              (arm64::lookup-register value))))
            (if regval
              (arm64::define-register symbol regval)
              (progn
                (symbols symbol)
                (vals (eval value)))))))
      (progv (symbols) (vals)
        (dolist (form body current)
          (setq current (arm64-lap-form form current sections)))))))

(defun arm64-lap-form (form current sections)
  (if (and form (symbolp form))
    (arm64::emit-lap-label current form)
    (if (or (atom form) (not (symbolp (car form))))
      (error "~& unknown ARM64-LAP form: ~S" form)
      (multiple-value-bind (expansion expanded)
          (arm64-lap-macroexpand-1 form)
        (if expanded
          (setq current (arm-lap-form expansion current sections))
          (let* ((name (car form)))
            (if (keywordp name)
              (setq current
                    (arm-lap-pseudo-op name (cadr form) current sections))
              (case name
                ((progn) (dolist (f (cdr form))
                           (setq current (arm64-lap-form f current sections))))
                ((let) (setq current (arm64-lap-equate-form (cadr form)
                                                            (cddr form)
                                                            current sections)))
                (t
                 (arm64::assemble-instruction current form)))))))))
  current)

(defmacro defarm64lapfunction (&environment env name arglist &body body
                               &aux doc)
  (if (not (endp body))
    (and (stringp (car body))
         (cdr body)
         (setq doc (car body))
         (setq body (cdr body))))
  `(progn
     (eval-when (:compile-toplevel)
       (note-function-info ',name t ,env))
     #-arm64-target
     (progn
       (eval-when (:load-toplevel)
         (%defun (nfunction ,name
                            (lambda (&lap 0)
                              (arm64-lap-function ,name ,arglist ,@body)))
                 ,doc))
       (eval-when (:execute)
         (%define-arm64-lap-function ',name '((let ,arglist ,@body)))))
     #+arm64-target	; just shorthand for defun
     (%defun (nfunction ,name
                        (lambda (&lap 0)
                          (arm64-lap-function ,name ,arglist ,@body)))
             ,doc)))
