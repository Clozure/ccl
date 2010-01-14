;;; simplesum.lisp

#|
The MIT license.

Copyright (c) 2009 Paul L. Krueger

Permission is hereby granted, free of charge, to any person obtaining a copy of this software 
and associated documentation files (the "Software"), to deal in the Software without restriction, 
including without limitation the rights to use, copy, modify, merge, publish, distribute, 
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is 
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial 
portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT 
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

|#

;;; Sample lisp/Cocoa interface that uses a NIB file defined with interface builder;
;;; A definition is provided for the "Foo" class that was specified to interface builder
;;; so that when the NIB file is loaded an instance of that class will be created and
;;; linked appropriately to the input fields, output sum text field, and Sum button.
;;; When the Sum button is pushed the sum will be computed and placed into the sum 
;;; field in the window.

(require :nib)

(defpackage :simplesum 
  (:nicknames :ss)
  (:use :iu :ccl :common-lisp)
  (:export test-sum))

(in-package :simplesum)

(defvar *debug-sum* nil)

(defclass sum-window-owner (ns:ns-object)
  ((input1 :foreign-type :id
           :accessor input1)
   (input2 :foreign-type :id
           :accessor input2)
   (sum :foreign-type :id
        :accessor sum)
   (nib-objects :accessor nib-objects :initform nil))
  (:metaclass ns:+ns-object))

(defmethod initialize-instance :after ((self sum-window-owner) 
                                       &key &allow-other-keys)
  (setf (nib-objects self)
        (load-nibfile 
         (truename "ip:Simple Sum;sumLisp.nib") 
         :nib-owner self
         :retain-top-objs t))
  ;; we do the following so that ccl:terminate will be called before we are garbage 
  ;; collected and we can release the top-level objects from the NIB that we retained
  ;; when loaded
  (ccl:terminate-when-unreachable self))

(defmethod ccl:terminate ((self sum-window-owner))
  (dolist (top-obj (nib-objects self))
    (unless (eql top-obj (%null-ptr))
      (#/release top-obj))))

;; methods called as a result of button actions

(objc:defmethod (#/doSum: :void) 
                ((self sum-window-owner) (s :id))
  (declare (ignore s))
  (with-slots (input1 input2 sum) self
    (#/setIntValue: sum (+ (#/intValue input1) (#/intValue input2)))))

(objc:defmethod (#/doFirstThing :void) 
                ((self sum-window-owner) (s :id))
  (declare (ignore s))
  ;;; test function for menu tests
  (#/doSum: self (%null-ptr)))

;; test by
(defun test-sum ()
  (make-instance 'sum-window-owner))

(provide :simplesum)