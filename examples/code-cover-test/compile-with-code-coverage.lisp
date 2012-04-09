;; -*- Mode: Lisp; tab-width: 2; indent-tabs-mode: nil -*-

;; Control over whether to compile with code coverage analysis enabled

;; TODO: fall back to cover.lisp if not using CCL 

(in-package :code-cover-test)

(defvar *compile-code-coverage-default-p* nil
  "Set this to true to ASDF compile all systems with code coverage analysis")

(defvar *compile-code-coverage-p* nil
  "Flag indicates whether currently compiling with code coverage analysis")

(defmacro with-code-coverage-compile ((&optional (flag '*compile-code-coverage-p*)) &body body)
  `(let ((*compile-code-coverage-p* ,flag)
         #+ccl
         (ccl:*compile-code-coverage* *compile-code-coverage-p*)
         )
     #-ccl
     (when *compile-code-coverage-p*
       (warn "Code coverage compile is only implemented for CCL")
       (setq *compile-code-coverage-p* nil))
     ;; Continue
     ,@body))

;; ASDF compile methods - these are for all systems and components

(defmethod asdf:perform :around ((op asdf:compile-op) (system asdf:system))
  (with-code-coverage-compile (*compile-code-coverage-default-p*)
    (call-next-method)))

(defmethod asdf:perform :around ((op asdf:compile-op) (component t))
  (declare (ignore component))
  (if asdf:*asdf-verbose*
      (warn "Compiling ~a with code coverage ~@?"
            component "~:[off~;on~]"
            #+ccl
            ccl:*compile-code-coverage*
            #-cll
            *compile-code-coverage-p*))
  (call-next-method))
