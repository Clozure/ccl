(in-package :cl-test)

(declaim (special *load-test-var.1* *load-test-var.2*))
(eval-when (:load-toplevel)
  (setq *load-test-var.1* *load-pathname*)
  (setq *load-test-var.2* *load-truename*))

