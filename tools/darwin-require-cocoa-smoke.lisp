;;;; Smoke: tip image can (require :cocoa) on Darwin/arm64.
;;;;
;;;; Expects baked tip (foreign-types id typedef prefer, nfasload htab
;;;; hardening) and cocoa CDB shims (YES/NO, NS*KeyMask, FLT_MAX) via
;;;; tools/darwin-arm64-cdb/inject-objc-bool-constants.lisp / populate.
;;;;
;;;;   ./darm64cl --no-init --batch < tools/darwin-require-cocoa-smoke.lisp
;;;;   ./tools/run-darwin-smoke.sh 300 tools/darwin-require-cocoa-smoke.lisp
(in-package :ccl)

(unless (probe-file "darwin-arm64-headers/cocoa/constants.cdb")
  (error "missing cocoa CDB; run tools/darwin-arm64-cdb/cocoa-populate.sh + parse"))

(require :cocoa)
(unless (member "COCOA" *modules* :test #'string-equal)
  (error "COCOA not in *modules* after require: ~s" *modules*))
(unless (find-package "GUI")
  (error "GUI package missing after require :cocoa"))
(unless (find-package "HI")
  (error "HI package missing after require :cocoa"))
(format t "~&require :cocoa ok; GUI=~s HI=~s~%"
        (find-package "GUI") (find-package "HI"))
(format t "~&DARWIN-REQUIRE-COCOA-SMOKE-OK~%")
(quit 0)
