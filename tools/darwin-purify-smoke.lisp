;;;; Experimental parent: save with :purify t to /tmp/darm64cl-purify-test.image.
;;;; Invoked by tools/run-darwin-purify-smoke.sh (child is a separate script).
(in-package :ccl)
(defparameter *purify-image* "/tmp/darm64cl-purify-test.image")
(when (probe-file *purify-image*)
  (delete-file *purify-image*))
(setq *outstanding-deferred-warnings* nil)
(format t "~&;; save-application ~s :purify t~%" *purify-image*)
(save-application *purify-image* :purify t)
