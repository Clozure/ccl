;; ns-object-utils.lisp
#|
The MIT license.

Copyright (c) 2010 Paul L. Krueger

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


;; These are useful for manipulating various types of NSObjects using lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :ns-string-utils)
  (require :date)
  (require :decimal))


(defpackage :interface-utilities
  (:nicknames :iu)
  (:export 
   lisp-to-ns-object
   lisp-ptr-wrapper
   lpw-lisp-ptr
   lpw-depth
   lpw-parent
   make-ptr-wrapper
   ns-to-lisp-object
   print-ns-object))

(in-package :iu)

(defun print-ns-object (ns-obj)
  ;; default print methods for objects truncate strings at 1024 characters for some reason
  ;; this function doesn't
  (if (ccl::objc-object-p ns-obj)
    (format t "~a" (ns-to-lisp-string (#/description ns-obj)))
    (format t "~s" ns-obj)))

(defun ns-to-lisp-object (old-lisp-obj ns-obj &optional (ns-format nil))
  ;; convert an arbitrary NSObject object to an appropriate lisp object.
  ;; Often done so that it can replace the old-lisp-obj when edited
  ;; An empty string @"" returns nil if old-lisp-obj is not a string
  (cond ((ccl::objc-object-p old-lisp-obj)
         ;; the old value was an NSObject so just return the new value
         ns-obj)
        ((typep ns-obj 'lisp-ptr-wrapper)
         ;; just strip the wrapper and return the original object
         (lpw-lisp-ptr ns-obj))
        ((typep ns-obj 'ns:ns-decimal)
         (if (floatp old-lisp-obj)
           ;; convert the decimal to a float
           (#/doubleValue ns-obj)
           ;; otherwise convert it to an appropriate lisp integer with assumed
           ;; decimals (see ip;Utilities;decimal.lisp)
           (if (eq (first ns-format) :decimal)
             (lisp-from-ns-decimal ns-obj :decimals (second ns-format))
             (lisp-from-ns-decimal ns-obj))))
        ((typep ns-obj 'ns:ns-number)
         (read-from-string (ns-to-lisp-string (#/descriptionWithLocale: ns-obj (%null-ptr)))
                           nil nil))
        ((typep ns-obj 'ns:ns-date)
         (ns-to-lisp-date ns-obj))
        (t
         (let ((str (ns-to-lisp-string ns-obj)))
           (if (stringp old-lisp-obj)
             str
             (read-from-string str nil nil))))))

(defun lisp-to-ns-object (lisp-obj &optional (ns-format nil))
  ;; convert an arbitrary lisp object to an appropriate NSObject so
  ;; that it can be displayed someplace
  (cond ((ccl::objc-object-p lisp-obj)
         ;; it's already an NSObject so just return it
         lisp-obj)
        ((eq ns-format :date)
         ;; assume lisp-obj is an integer representing a lisp date
         (lisp-to-ns-date lisp-obj))
        ((and (consp ns-format) (eq (first ns-format) :decimal))
         (cond ((typep lisp-obj 'fixnum)
                (lisp-to-ns-decimal lisp-obj :decimals (second ns-format)))
               ((typep lisp-obj 'number)
                (lisp-to-ns-decimal (round (* (expt 10 (second ns-format)) lisp-obj))
                                    :decimals (second ns-format)))
               (t
                (lisp-to-ns-decimal 0 :decimals (second ns-format)))))
        ((integerp lisp-obj)
         (#/numberWithInt: ns:ns-number lisp-obj))
        ((typep lisp-obj 'double-float)
         (#/numberWithDouble: ns:ns-number lisp-obj))
        ((floatp lisp-obj)
         (#/numberWithFloat: ns:ns-number lisp-obj))
        ((null lisp-obj)
         #@"")
        (t
         (lisp-to-temp-nsstring (if (stringp lisp-obj)
                                  lisp-obj
                                  (format nil "~s" lisp-obj))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lisp-ptr-wrapper
;;
;; This is a simple class that encapsulates a pointer to a lisp object so we can pass this
;; off to an Objective-C view and know what it points to when we get it back later.

(defclass lisp-ptr-wrapper (ns:ns-object)
  ((lpw-lisp-ptr :accessor lpw-lisp-ptr)
   (lpw-depth :accessor lpw-depth)
   (lpw-parent :accessor lpw-parent))
  (:metaclass ns:+ns-object))

(defun make-ptr-wrapper (ptr &key (depth 1) (parent nil))
  (let ((lpw (make-instance 'lisp-ptr-wrapper)))
    (setf (lpw-lisp-ptr lpw) ptr)
    (setf (lpw-depth lpw) depth)
    (setf (lpw-parent lpw) parent)
    lpw))


(provide :ns-object-utils)