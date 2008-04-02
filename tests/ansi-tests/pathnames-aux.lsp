;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Dec  6 15:05:05 2003
;;;; Contains: Functions associated with pathname tests

(in-package :cl-test)

(defun could-be-pathname-designator (x)
  (or (stringp x)
      (pathnamep x)
      (typep x 'file-stream)
      (and (typep x 'synonym-stream)
	   (could-be-pathname-designator
	    (symbol-value
	     (synonym-stream-symbol x))))))

(defun explode-pathname (pn)
  (list
   :host   (pathname-host pn)
   :device (pathname-device pn)
   :directory (pathname-directory pn)
   :name   (pathname-name pn)
   :type   (pathname-type pn)
   :version (pathname-version pn)))

