;;;; 18 — The ObjC callback error-return trampoline must be installable
;;;; on Darwin: the callback page is MAP_JIT, so the builder assembles
;;;; into heap scratch and blits via kernel C.  (Regression: writing the
;;;; page directly faulted, corrupting every callback error unwind —
;;;; e.g. the xapropos click hang.)
;;;; Marker: 18-CALLBACK-ERROR-RETURN-OK
(in-package :ccl)
(require "OBJC-SUPPORT")

(let ((ptr (%arm64-objc-callback-error-return-trampoline)))
  (assert (macptrp ptr))
  ;; fmov x16, d0 / mov lr, x1 / br x16
  (assert (eql (%get-unsigned-long ptr 0) #x9e670010))
  (assert (eql (%get-unsigned-long ptr 4) #xaa0103fe))
  (assert (eql (%get-unsigned-long ptr 8) #xd61f0200))
  ;; Idempotent.
  (assert (eql (%ptr-to-int ptr)
               (%ptr-to-int (%arm64-objc-callback-error-return-trampoline)))))

(format t "~&18-CALLBACK-ERROR-RETURN-OK~%")
(quit 0)
