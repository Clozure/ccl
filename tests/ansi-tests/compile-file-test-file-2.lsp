(in-package "CL-TEST")

(defun compile-file-test-fun.2 () nil)

(eval-when (:compile-toplevel)
  (unless (find-class 'compile-file-test-condition.2 nil)
    (define-condition compile-file-test-condition.2 (style-warning) nil))
  (warn (make-condition 'compile-file-test-condition.2)))
