;;;-*- Mode: LISP; Package: CCL -*-

(in-package "CCL")

(require "COMPILE-HEMLOCK")

(format t "~&;;; Compiling Hemlock ...")

(compile-hemlock t)
