(defpackage "SPELL"
  (:use "COMMON-LISP")
  (:export #:spell-try-word #:spell-root-word #:spell-collect-close-words
	   #:correct-spelling
           #:+max-entry-length+
	   #:spell-read-dictionary #:spell-add-entry #:spell-root-flags
	   #:spell-remove-entry))