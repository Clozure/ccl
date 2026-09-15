;;;; Smoke: Darwin/arm64 natural-size packing for non-variadic stack overflow.
;;;;
;;;;   cc -arch arm64 -shared -o /tmp/libpack_overflow.dylib /tmp/pack_overflow.c
;;;;   # pack_overflow(long×8, char, short, int) → sum; callee loads
;;;;   # char@SP+0, short@SP+2, int@SP+4 (Apple ABI).
;;;;   ./darm64cl --no-init --batch < tools/darwin-pack-overflow-smoke.lisp
;;;;
;;;; Reloads packed-store vinsns + aapcs64-ff-call from source.
(in-package :ccl)
(setq *warn-if-redefine-kernel* nil)
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
(unless (probe-file "/tmp/libpack_overflow.dylib")
  (error "missing /tmp/libpack_overflow.dylib — build pack_overflow first"))
(open-shared-library "/tmp/libpack_overflow.dylib")
(defun call-pack-overflow ()
  ;; 1+…+8 + 1 + 2 + 3 = 42; packed layout must match clang.
  (ff-call (foreign-symbol-address "pack_overflow")
           :signed-doubleword 1 :signed-doubleword 2 :signed-doubleword 3
           :signed-doubleword 4 :signed-doubleword 5 :signed-doubleword 6
           :signed-doubleword 7 :signed-doubleword 8
           :signed-byte 1 :signed-halfword 2 :signed-fullword 3
           :signed-doubleword))
(let ((n (call-pack-overflow)))
  (unless (eql n 42)
    (error "pack_overflow => ~s, expected 42 (8-byte slots would misread short/int)" n)))
(format t "~&DARWIN-PACK-OVERFLOW-SMOKE-OK~%")
(quit)
