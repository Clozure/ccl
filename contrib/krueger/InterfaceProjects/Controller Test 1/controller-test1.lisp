;; controller-test1.lisp

;; Test window that displays lisp lists using an NSTableView

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :lisp-controller)
  (require :ns-string-utils)
  (require :nslog-utils)
  (require :date))

(defpackage :controller-test1
  (:nicknames :ct1)
  (:use :ccl :common-lisp :iu :lc)
  (:export test-controller get-root))

(in-package :ct1)

(defclass lisp-controller-test (ns:ns-window-controller)
   ((lisp-ctrl :foreign-type :id :accessor lisp-ctrl))
  (:metaclass ns:+ns-object))

(defmethod get-root ((self lisp-controller-test))
  (when (lisp-ctrl self)
    (root (lisp-ctrl self))))

(objc:defmethod (#/initWithNibPath: :id)
                ((self lisp-controller-test) (nib-path :id))
  (let* ((init-self (#/initWithWindowNibPath:owner: self nib-path self)))
    init-self))

(defun make-dated-list ()
  (list (now) 0 (random 20) (random 30)))

(defun selected-cell (window controller root row-num col-num obj)
  (declare (ignore window controller root))
  (cond ((and (minusp row-num) (minusp col-num))
         (ns-log "Nothing selected"))
        ((minusp row-num)
         (ns-log (format nil "Selected column ~s with title ~s" col-num obj)))
        ((minusp col-num)
         (ns-log (format nil "Selected row ~s: ~s" row-num obj)))
        (t
         (ns-log (format nil "Selected ~s in row ~s,  col ~s" obj row-num col-num)))))

(defun edited-cell (window controller root row-num col-num obj old-val new-val)
  (declare (ignore window controller root))
  (ns-log (format nil "Changed ~s in row ~s, col ~s: ~s to ~s"
                  old-val row-num col-num obj new-val)))

(defun added-row (window controller root parent new-row)
  (declare (ignore window controller root))
  (ns-log (format nil "Added ~s to ~s" new-row parent)))

(defun removed-row (window controller root parent old-row)
  (declare (ignore window controller root))
  (ns-log (format nil "Removed row ~s from ~s " old-row parent)))

(defun test-controller ()
  (let* ((nib-name (lisp-to-temp-nsstring
                    (namestring (truename "ip:Controller Test 1;lc-test1.nib"))))
         (wc (make-instance 'lisp-controller-test
               :with-nib-path nib-name)))
    (#/window wc)
    wc))
                  
(provide :controller-test1)