;;; Uncomment the next line to make MAKE-STRING and MAKE-SEQUENCE
;;; tests require that a missing :initial-element argument defaults
;;; to a single value, rather than leaving the string/sequence filled
;;; with arbitrary legal garbage.
;; (pushnew :ansi-tests-strict-initial-element *features*)

#+allegro (run-shell-command "rm -f *.fasl")
#+cmu (run-program "rm -f *.x86f")

(load "gclload1.lsp")
(load "gclload2.lsp")

(setq rt::*compile-tests* t)

#+allegro
(progn
  (rt:disable-note :nil-vectors-are-strings)
  (rt:disable-note :standardized-package-nicknames)
  (rt:disable-note :type-of/strict-builtins)
  (rt:disable-note :assume-no-simple-streams)
  (rt:disable-note :assume-no-gray-streams))

(in-package :cl-test)
(time (regression-test:do-tests))

#+allegro :exit
#+(or cmu sbcl gcl) (quit)
