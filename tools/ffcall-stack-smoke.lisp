;;;; Smoke: fixed-arity ff-call with >8 GPR args (SPffcall stack bump).
;;;;
;;;;   cc -arch arm64 -shared -o /tmp/libsum9.dylib /tmp/sum9.c
;;;;   # sum9.c: long sum9(long a,b,c,d,e,f,g,h,i){return a+b+c+d+e+f+g+h+i;}
;;;;   ./darm64cl --no-init --batch < tools/ffcall-stack-smoke.lisp
;;;;
;;;; Reloads aapcs64-ff-call (+ Darwin pack helpers) from source when the
;;;; image predates the change.
(in-package :ccl)
(setq *warn-if-redefine-kernel* nil)
;; Darwin natural-pack path uses set-c-arg-*-bytes even for 8-byte overflow.
(load "compiler/ARM64/arm64-vinsns.lisp")
(let* ((src (merge-pathnames "compiler/ARM64/arm642.lisp" (ccl-directory)))
       (helpers ())
       (ffcall nil))
  (with-open-file (s src)
    (loop for f = (read s nil s)
          until (eq f s)
          do (cond ((and (consp f)
                         (member (car f) '(defun defarm642))
                         (member (cadr f)
                                 '(arm642-aapcs64-stack-arg-bytes
                                   arm642-align-up
                                   arm642-aapcs64-ff-call)))
                    (if (eq (cadr f) 'arm642-aapcs64-ff-call)
                      (setq ffcall f)
                      (push f helpers))))))
  (dolist (h (nreverse helpers)) (eval h))
  (unless ffcall (error "aapcs64-ff-call def not found in ~s" src))
  (eval ffcall))
(unless (probe-file "/tmp/libsum9.dylib")
  (error "missing /tmp/libsum9.dylib — build sum9 first"))
(open-shared-library "/tmp/libsum9.dylib")
(defun call-sum9 ()
  (ff-call (foreign-symbol-address "sum9")
           :signed-doubleword 1 :signed-doubleword 2 :signed-doubleword 3
           :signed-doubleword 4 :signed-doubleword 5 :signed-doubleword 6
           :signed-doubleword 7 :signed-doubleword 8 :signed-doubleword 9
           :signed-doubleword))
(let ((n (call-sum9)))
  (unless (eql n 45)
    (error "sum9 => ~s, expected 45" n)))
(format t "~&FFCALL-STACK-SMOKE-OK~%")
(quit)
