;;;; Parse cocoa .ffi → *.cdb under ccl:darwin-arm64-headers;cocoa;
;;;;
;;;;   cp darwin-arm64-headers/cocoa/*.cdb /tmp/cocoa-cdb-backup/
;;;;   ./darm64cl --stack-size 16M --thread-stack-size 16M --no-init --batch \
;;;;     < tools/darwin-arm64-cdb/parse-cocoa.lisp
(in-package :ccl)
(setq *warn-if-redefine-kernel* nil)
(load "library/parse-ffi.lisp")
(format t "~&;; parse-standard-ffi-files \"cocoa\"~%")
(parse-standard-ffi-files "cocoa")
(format t "~&;; PARSE-COCOA-OK~%")
(quit 0)
