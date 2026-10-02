;;;; Interpreted / runtime %ff-call smoke (darwinarm64).
;;;;
;;;; Tests the baked image: FUNCALL of runtime %ff-call, plus
;;;; cheap-eval of #_getpid (macroexpands through ff-call).
;;;;
;;;;   ./darm64cl --no-init --batch < tools/darwin-interp-ff-call-smoke.lisp
(in-package :ccl)

(unless (functionp (fboundp '%ff-call))
  (error "%ff-call not fbound in image (rebuild required)"))

(use-interface-dir :libc)

(defun %interp-ff-call-smoke-wrap (fn a b)
  (funcall fn a b))

(let* ((addr (%reference-external-entry-point (external "getpid")))
       (pid (funcall #'%ff-call addr :signed-fullword))
       (pid-wrap (%interp-ff-call-smoke-wrap #'%ff-call addr :signed-fullword))
       (pid2 (cheap-eval-in-environment '(#_getpid) nil)))
  (unless (and (integerp pid) (> pid 0))
    (error "funcall %ff-call getpid => ~s" pid))
  (unless (eql pid pid-wrap)
    (error "wrap %ff-call => ~s, direct => ~s" pid-wrap pid))
  (unless (eql pid pid2)
    (error "cheap-eval #_getpid => ~s, funcall %ff-call => ~s" pid2 pid))
  (format t "~&DARWIN-INTERP-FF-CALL-SMOKE-OK pid=~d~%" pid))
(quit 0)
