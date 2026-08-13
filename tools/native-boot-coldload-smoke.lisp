;;;; Cold-load native arm64-boot.image; expect READ-LOOP, not %FIND-PKG fault.
(in-package "CCL")
(format t "~&;; cold-load ok: ~s ~s~%" (lisp-implementation-version) *features*)
(format t "~&;; (+ 1 2) => ~s~%" (+ 1 2))
(quit 0)
