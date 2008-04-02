(in-package "CL-TEST")

(defun compile-file-test-fun.2a () nil)

(eval-when (:compile-toplevel)
  (unless (find-class 'compile-file-test-condition.2a nil)
    (define-condition compile-file-test-condition.2a (warning) nil))
  (warn (make-condition 'compile-file-test-condition.2a)))
