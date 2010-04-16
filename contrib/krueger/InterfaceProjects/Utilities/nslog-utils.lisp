;; nslog-utils.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :ns-string-utils))

(defpackage :interface-utilities
  (:nicknames :iu)
  (:export ns-log ns-error log-rect log-size log-float interleave log-4floats))

(in-package :iu)

(defun ns-log (lisp-str)
  (#_NSLog (lisp-to-temp-nsstring lisp-str)))

(defmacro ns-error (format-string &rest args)
  `(progn
     (ns-log (format nil ,format-string ,@args))
     (error "See console log for information")))

(defun log-rect (r &optional (log-string ""))
  (#_NSLog (lisp-to-temp-nsstring (concatenate 'string 
                                               log-string 
                                               "x = %F y = %F width = %F height = %F"))
           #>CGFloat (ns:ns-rect-x r)
           #>CGFloat (ns:ns-rect-y r)
           #>CGFloat (ns:ns-rect-width r)
           #>CGFloat (ns:ns-rect-height r)))
           
(defun log-size (s &optional (log-string ""))
  (#_NSLog (lisp-to-temp-nsstring (concatenate 'string 
                                               log-string 
                                               "width = %F height = %F"))
           #>CGFloat (ns:ns-size-width s)
           #>CGFloat (ns:ns-size-height s)))

(defun log-float (f &optional (log-string ""))
  (#_NSLog (lisp-to-temp-nsstring (concatenate 'string 
                                               log-string 
                                               "%F"))
           #>CGFloat f))

(defun interleave (l1 l2)
  (let ((lst1 (if (listp l1) l1 (list l1)))
        (lst2 (if (listp l2) l2 (list l2))))
    (if (atom l1)
      (setf (cdr lst1) lst1)
      (if (atom l2)
        (setf (cdr lst2) lst2)))
    (mapcan #'(lambda (el1 el2)
                (list el1 el2))
            lst1
            lst2)))

(defun log-4floats (f1 f2 f3 f4 &optional (log-strings '("" "" "" "")))
  (#_NSLog (lisp-to-temp-nsstring (apply #'concatenate 'string 
                                         (interleave log-strings "%F ")))
           #>CGFloat f1
           #>CGFloat f2
           #>CGFloat f3
           #>CGFloat f4))