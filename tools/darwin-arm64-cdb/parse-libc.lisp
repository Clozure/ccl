;;;; Parse libc .ffi → *.cdb under ccl:darwin-arm64-headers;libc;
;;;;
;;;;   cp darwin-arm64-headers/libc/*.cdb /tmp/libc-cdb-backup/
;;;;   ./darm64cl --stack-size 16M --thread-stack-size 16M --no-init --batch \
;;;;     < tools/darwin-arm64-cdb/parse-libc.lisp
;;;;
;;;; Reloads parse-ffi.lisp so Availability/(null) skip fixes apply even
;;;; when the image predates them.  math.h is included via libc-populate.sh
;;;; + filter-ffi.py.
(in-package :ccl)
(setq *warn-if-redefine-kernel* nil)
(load "library/parse-ffi.lisp")
(format t "~&;; parse-standard-ffi-files \"libc\"~%")
(parse-standard-ffi-files "libc")
(format t "~&;; PARSE-LIBC-OK~%")
(quit 0)
