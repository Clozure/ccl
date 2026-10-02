;;;; Smoke: Darwin/arm64 libc math.h CDB (#_sin/#_cos/#_sqrt).
;;;;
;;;;   ./darm64cl --no-init --batch < tools/darwin-math-smoke.lisp
(in-package :ccl)
(use-interface-dir :libc)
(defun %math-sin0 () (#_sin 0.0d0))
(defun %math-cos0 () (#_cos 0.0d0))
(defun %math-sqrt4 () (#_sqrt 4.0d0))
(let ((s (%math-sin0)) (c (%math-cos0)) (r (%math-sqrt4)))
  (unless (and (= s 0.0d0) (= c 1.0d0) (= r 2.0d0))
    (error "math: sin=~s cos=~s sqrt=~s" s c r)))
(format t "~&DARWIN-MATH-SMOKE-OK~%")
(quit)
