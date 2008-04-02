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
