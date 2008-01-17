; -*- Mode:Lisp; Package:CCL; -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   This file is part of OpenMCL.  
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(in-package "CCL")

;I wanted a read that would not error even when given a #<
; and also allow backspace and such.
(defun read-line-no-error (&optional (stream *standard-output*) &aux result)
  (ignore-errors
     (setq result (read-from-string (read-line stream) nil))
     (return-from read-line-no-error (values result t)))
  (values nil nil))



;;;; Assert & Check-Type

;;; Assert-Value-Prompt  --  Internal
;;;
;;;    Prompt for a new value to set a place to.   We do a read-line,
;;; and if there is anything there, we eval it and return the second
;;; value true, otherwise it is false.
;;;
(defun assertion-value-prompt (place)
  (let* ((nvals (length (nth-value 2 (get-setf-method-multiple-value place))))
         (vals nil))
    (dotimes (i nvals)
      (if (eq nvals 1)
        (format *query-io* "Value for ~S: " place)
        (format *query-io* "Value ~D for ~S: " i place))
      (let* ((line (read-line *query-io*))
             (object  (read-from-string line nil *eof-value*)))
        (if (eq object *eof-value*)
            (return)
            (push (eval object) vals))))
    (values (nreverse vals) (not (null vals)))))

(defun %assertion-failure (setf-places-p test-form string &rest condition-args)
  (cerror 
   (if setf-places-p 
     "allow some places to be set and test the assertion again."
     "test the assertion again.")
   (cond
    ((stringp string)
     (make-condition 'simple-error
                     :format-control string
                     :format-arguments  condition-args))
    ((null string)
     (make-condition 'simple-error
                     :format-control "Failed assertion: ~S"
                     :format-arguments (list test-form)))
    ((typep string 'condition)
     (when  condition-args (error "No args ~S allowed with a condition ~S"  condition-args string))
     string)
    (t (apply #'make-condition string  condition-args)))))

