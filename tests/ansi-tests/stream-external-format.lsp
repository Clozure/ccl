;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 27 20:53:21 2004
;;;; Contains: Tests of STREAM-EXTERNAL-FORMAT

(in-package :cl-test)

;;; This is tested in open.lsp

;;; Error tests

(deftest stream-external-format.error.1
  (signals-error (stream-external-format) program-error)
  t)

(deftest stream-external-format.error.2
  (signals-error
   (let ((pn #p"tmp.dat"))
     (delete-all-versions pn)
     (with-open-file
      (s pn :direction :output :if-exists :supersede)
      (stream-external-format s nil)))
   program-error)
  t)
