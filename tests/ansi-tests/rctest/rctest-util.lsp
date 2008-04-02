;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jun  7 21:42:23 2003
;;;; Contains: Utility functions for RCTEST

(in-package :rctest)

(defun randomly-partition (size &optional (limit 1))
  "Return a randomly generated list of positive integers whose
   sum is SIZE.  Try to make no element be < LIMIT."
  (declare (type unsigned-byte size limit))
  (let ((result nil))
    (loop
     while (> size 0)
     do
     (let* ((e0 (min size (max limit (1+ (min (random size) (random size)))))))
       (push e0 result)
       (decf size e0)))
    (random-permute result)))
