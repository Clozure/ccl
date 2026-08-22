;;;; Native (no Rosetta) darwinarm64 xload + cold-load smoke.
;;;; Expect: arm64-boot.image with nil=#x20000100b; cold-load reaches READ-LOOP.

(in-package "CCL")

(setq *load-verbose* t *compile-verbose* t *warn-if-redefine-kernel* nil)

(format t "~&;; host=~s nil=#x~x~%"
        (backend-name *host-backend*)
        (arch::target-nil-value (backend-target-arch *host-backend*)))

(load "ccl:compiler;ARM64;arm64-backend.lisp")
(load "ccl:lib;misc.lisp")
(load "ccl:lib;compile-ccl.lisp")
;; Full xload toolchain (registers darwin backend + write-image-file).
(require-modules *arm64-xload-modules*)
(load "ccl:xdump;xfasload.lisp")

(ensure-darwinarm64-target-arch)
(setq *arm64-backend* *darwinarm64-backend*
      *host-backend* *darwinarm64-backend*
      *target-backend* *darwinarm64-backend*)

(format t "~&;; after ensure: host=~s darwin-nil=#x~x linux-nil=#x~x~%"
        (backend-name *host-backend*)
        (arch::target-nil-value (backend-target-arch *darwinarm64-backend*))
        (arch::target-nil-value arm64::*arm64-target-arch*))

(unless (eql (arch::target-nil-value (backend-target-arch *host-backend*))
             +darwinarm64-nil-value+)
  (error "host arch nil not Darwin"))

(unless (find-xload-backend :darwinarm64)
  (error "missing *darwinarm64-xload-backend* (load xarm64fasload)"))

(format t "~&;; xload-default=~s xload-nil will be from target-backend~%"
        (and (boundp '*xload-default-backend*)
             (backend-xload-info-name *xload-default-backend*)))

(%enable-darwinarm64-map-jit-fasls)

(format t "~&;; native xload-level-0 :force~%")
(force-output)
(gc)
(xload-level-0 :force)

(unless (probe-file (standard-boot-image-name))
  (error "missing boot image"))

(format t "~&;; native-xload wrote ~s~%" (truename (standard-boot-image-name)))
(quit 0)
