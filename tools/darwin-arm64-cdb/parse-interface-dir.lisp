;;;; Parse $CCL_INTERFACE .ffi → *.cdb under ccl:darwin-arm64-headers;$CCL_INTERFACE;
;;;;
;;;; Shared by gl / carbon / quartz / quartzcore (and any later interface dir).
;;;;
;;;;   mkdir -p /tmp/${CCL_INTERFACE}-cdb-backup
;;;;   cp darwin-arm64-headers/${CCL_INTERFACE}/*.cdb /tmp/${CCL_INTERFACE}-cdb-backup/
;;;;   CCL_INTERFACE=gl ./darm64cl --stack-size 16M --thread-stack-size 16M \
;;;;     --no-init --batch < tools/darwin-arm64-cdb/parse-interface-dir.lisp
;;;;
;;;; Reloads parse-ffi.lisp so Availability/(null)/pending-macro fixes apply.
(in-package :ccl)
(setq *warn-if-redefine-kernel* nil)
(load "library/parse-ffi.lisp")
(let* ((name (getenv "CCL_INTERFACE"))
       (name (and name (string-trim '(#\Space #\Tab #\Newline) name))))
  (unless (and name (plusp (length name)))
    (format t "~&;; PARSE-INTERFACE-DIR requires CCL_INTERFACE (e.g. gl)~%")
    (quit 1))
  (format t "~&;; parse-standard-ffi-files ~S~%" name)
  (parse-standard-ffi-files name)
  (format t "~&;; PARSE-INTERFACE-DIR-OK ~S~%" name)
  (quit 0))
