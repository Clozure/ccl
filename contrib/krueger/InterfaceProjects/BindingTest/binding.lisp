;;; binding.lisp

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :nib)
  (require :ns-object-utils)
  (require :nslog-utils)
  (require :lisp-controller))

(defpackage :binding 
  (:nicknames :bnd)
  (:use :iu :ccl :common-lisp :lc)
  (:export test-binding
           random-sum-owner
           input-1
           input-2
           sum
           sum-owner
           controller))

(in-package :bnd)

(defclass sum-owner ()
  ((input-1 :reader input-1 :initarg :input-1)
   (input-2 :reader input-2  :initarg :input-2)
   (sum :reader sum :initform 0))
  (:default-initargs
     :input-1 0
     :input-2 0))

(defmethod (setf input-1) (new-value (self sum-owner))
  ;; we set up the slot writer to note a change in value to make it KVC compliant
  (will-change-value-for-key self 'input-1)
  (setf (slot-value self 'input-1) new-value)
  (did-change-value-for-key self 'input-1))

(defmethod (setf input-2) (new-value (self sum-owner))
  ;; we set up the slot writer to note a change in value to make it KVC compliant
  (will-change-value-for-key self 'input-2)
  (setf (slot-value self 'input-2) new-value)
  (did-change-value-for-key self 'input-2))

(defmethod (setf sum) (new-value (self sum-owner))
  ;; we set up the slot writer to note a change in value to make it KVC compliant
  (will-change-value-for-key self 'sum)
  (setf (slot-value self 'sum) new-value)
  (did-change-value-for-key self 'sum))

(defmethod  initialize-instance :after ((self sum-owner) 
                                       &key &allow-other-keys)
  (setf (sum self) (+ (input-1 self) (input-2 self))))

(defun random-sum-owner ()
  ;; return a sum-owner instance with random initial values
  (make-instance 'sum-owner
    :input-1 (random 100)
    :input-2 (random 100)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass binding-window-owner (ns:ns-object)
  ((controller :foreign-type :id
               :accessor controller)
   (nib-objects :accessor nib-objects :initform nil))
  (:metaclass ns:+ns-object))

(defmethod initialize-instance :after ((self binding-window-owner) 
                                       &key &allow-other-keys)
  (setf (nib-objects self)
        (load-nibfile 
         (truename "ip:BindingTest;binding.nib") 
         :nib-owner self
         :retain-top-objs t))
  ;; we do the following so that ccl:terminate will be called before we are garbage 
  ;; collected and we can release the top-level objects from the NIB that we retained
  ;; when loaded
  (ccl:terminate-when-unreachable self))

(defmethod ccl:terminate ((self binding-window-owner))
  (dolist (top-obj (nib-objects self))
    (unless (eql top-obj (%null-ptr))
      (#/release top-obj))))

;; methods called as a result of button actions

(objc:defmethod (#/doSum: :void) 
                ((self binding-window-owner) (s :id))
  (declare (ignore s))
  (with-slots (input-1 input-2) (root (controller self))
    (setf (sum (root (controller self)))
          (+ input-1 input-2))))

;; test by
(defun test-binding ()
  (make-instance 'binding-window-owner))

(provide :binding)