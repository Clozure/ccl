;; controller-test2.lisp

;; Test window that implements a class browser using an NSOutlineView to show
;; standard Lisp class objects

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :lisp-controller)
  (require :ns-string-utils))

(defpackage :controller-test2
  (:nicknames :ct2)
  (:use :ccl :common-lisp :iu :lc)
  (:export test-browser))

(in-package :ct2)

(defclass lisp-browser (ns:ns-window-controller)
   ((lisp-ctrl :foreign-type :id :accessor lisp-ctrl))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/initWithNibPath: :id)
                ((self lisp-browser) (nib-path :id))
  (let* ((init-self (#/initWithWindowNibPath:owner: self nib-path self)))
    init-self))

(defun cl-name (cls)
  (symbol-name (class-name cls)))

(defun cl-package (cls)
  (package-name (symbol-package (class-name cls))))

(defun test-browser ()
  (let* ((nib-name (lisp-to-temp-nsstring
                    (namestring (truename "ip:Controller Test 2;lc-test2.nib"))))
         (wc (make-instance 'lisp-browser
               :with-nib-path nib-name)))
    (#/window wc)
    wc))

(provide :controller-test2)