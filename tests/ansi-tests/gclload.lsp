;;; Uncomment the next line to make MAKE-STRING and MAKE-SEQUENCE
;;; tests require that a missing :initial-element argument defaults
;;; to a single value, rather than leaving the string/sequence filled
;;; with arbitrary legal garbage.
;; (pushnew :ansi-tests-strict-initial-element *features*)

#+allegro (setq *enclose-printer-errors* nil)

;;; Remove compiled files
(let* ((fn (compile-file-pathname "doit.lsp"))
       (type (pathname-type fn))
       (dir-pathname (make-pathname :name :wild :type type))
       (files (directory dir-pathname)))
  (assert type)
  (assert (not (string-equal type "lsp")))
  (mapc #'delete-file files))

(load "gclload1.lsp")
(load "gclload2.lsp")

#+allegro
(progn
  (rt:disable-note :nil-vectors-are-strings)
  (rt:disable-note :standardized-package-nicknames)
  (rt:disable-note :type-of/strict-builtins)
  (rt:disable-note :assume-no-simple-streams)
  (rt:disable-note :assume-no-gray-streams))

#+gcl(si::use-fast-links nil)

(in-package :cl-test)

;;; These two tests will misbehave if the tests are being
;;; invoked from a file that is being loaded, so remove them
(when *load-pathname*
  (mapc #'regression-test:rem-test '(load-pathname.1 load-truename.1)))

(time (regression-test:do-tests))
