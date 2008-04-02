;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 25 08:07:31 1998
;;;; Contains: Package test code (common code)

(in-package :cl-test)
(declaim (optimize (safety 3)))

(report-and-ignore-errors
  (defpackage "A"
    (:use)
    (:nicknames "Q")
    (:export "FOO")))

(report-and-ignore-errors
  (defpackage "B"
    (:use "A")
    (:export "BAR")))

(defun set-up-packages ()
  (safely-delete-package "A")
  (safely-delete-package "B")
  (safely-delete-package "Q")
  (defpackage "A"
    (:use)
    (:nicknames "Q")
    (:export "FOO"))
  (defpackage "B"
    (:use "A")
    (:export "BAR")))

(report-and-ignore-errors
  (defpackage "FS-A"
    (:use)
    (:nicknames "FS-Q")
    (:export "FOO")))

(report-and-ignore-errors
  (defpackage "FS-B"
    (:use "FS-A")
    (:export "BAR")))

(report-and-ignore-errors
  (defpackage "DS1"
    (:use)
    (:intern "C" "D")
    (:export "A" "B")))

(report-and-ignore-errors
  (defpackage "DS2"
    (:use)
    (:intern "E" "F")
    (:export "G" "H" "A")))

(report-and-ignore-errors
  (defpackage "DS3"
    (:shadow "B")
    (:shadowing-import-from "DS1" "A")
    (:use "DS1" "DS2")
    (:export "A" "B" "G" "I" "J" "K")
    (:intern "L" "M")))

(report-and-ignore-errors
  (defpackage "DS4"
    (:shadowing-import-from "DS1" "B")
    (:use "DS1" "DS3")
    (:intern "X" "Y" "Z")
    (:import-from "DS2" "F")))
