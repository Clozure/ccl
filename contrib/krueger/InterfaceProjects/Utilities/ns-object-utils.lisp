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
  (require :ns-binding-utils)
  (require :nslog-utils)
  (require :date)
  (require :decimal)
  (require :assoc-array))


(defpackage :interface-utilities
  (:nicknames :iu)
  (:export 
   did-change-value-for-key
   lisp-to-ns-array
   lisp-to-ns-dict
   lisp-to-ns-object
   lisp-ptr-wrapper
   lpw-lisp-ptr
   lpw-depth
   lpw-parent
   make-ptr-wrapper
   ns-to-lisp-array
   ns-to-lisp-assoc
   ns-to-lisp-hash-table
   ns-to-lisp-list
   ns-to-lisp-object
   objc-displayable
   print-ns-object
   will-change-value-for-key
   wrapper-for))

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
         (if (or (floatp old-lisp-obj) (eq :float old-lisp-obj))
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
        ((typep ns-obj 'ns:ns-dictionary)
         (if (or (consp old-lisp-obj) (eq old-lisp-obj :cons))
           (ns-to-lisp-assoc ns-obj)
           (ns-to-lisp-hash-table ns-obj)))
        ((typep ns-obj 'ns:ns-array)
         (if (or (consp old-lisp-obj) (eq old-lisp-obj :cons))
           (ns-to-lisp-list ns-obj)
           (ns-to-lisp-array ns-obj)))
        ((typep ns-obj 'ns:ns-string)
         (let ((str (ns-to-lisp-string ns-obj)))
           (if (or (stringp old-lisp-obj) (eq old-lisp-obj :string))
             str
             (read-from-string str nil nil))))
        ((typep ns-obj 'ns:ns-null)
         nil)
        (t
         ;; can't convert so just return ns-obj
         ns-obj)))

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
        ((stringp lisp-obj)
         (lisp-to-temp-nsstring lisp-obj))
        ((hash-table-p lisp-obj)
         (lisp-to-ns-dict lisp-obj))
        ((vectorp lisp-obj)
         (lisp-to-ns-array lisp-obj))
        ((null lisp-obj)
         #@"")
        (t
         (lisp-to-temp-nsstring (format nil "~s" lisp-obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods dealing with NSArray objects

(defmethod ns-to-lisp-array ((ns-arr ns:ns-array) &key (no-convert nil))
  (let* ((count (#/count ns-arr))
         (new-arr (make-array (list count))))
    (dotimes (i count new-arr)
        (setf (aref new-arr i) 
              (if no-convert
                (#/objectAtIndex: ns-arr i)
                (ns-to-lisp-object nil (#/objectAtIndex: ns-arr i)))))))

(defmethod ns-to-lisp-list ((ns-arr ns:ns-array) &key (no-convert nil))
  (let* ((count (#/count ns-arr))
         (new-list nil))
    (dotimes (i count (nreverse new-list))
        (setf new-list 
              (cons (if no-convert
                      (#/objectAtIndex: ns-arr i)
                      (ns-to-lisp-object nil (#/objectAtIndex: ns-arr i)))
                    new-list)))))

(defmethod lisp-to-ns-array ((lst list))
  (let ((new-arr (#/arrayWithCapacity: ns:ns-mutable-array (list-length lst)))
        (count -1))
    (dolist (item lst new-arr)
      (#/insertObject:atIndex: new-arr
                               (lisp-to-ns-object item)
                               (incf count)))))

(defmethod lisp-to-ns-array ((arr array))
  (let* ((max-count (if (array-has-fill-pointer-p arr)
                     (fill-pointer arr)
                     (length arr)))
         (new-arr (#/arrayWithCapacity: ns:ns-mutable-array max-count)))
    (do* ((count 0 (1+ count)))
         ((>= count max-count) new-arr)
      (#/insertObject:atIndex: new-arr
                               (lisp-to-ns-object (aref arr count))
                               count))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods dealing with NSDictionary objects

(defmethod ns-to-lisp-hash-table ((dict ns:ns-dictionary) &key (no-convert nil))
  (let ((ht (make-hash-table))
        (dict-keys (ns-to-lisp-list (#/allKeys dict) :no-convert t)))
    (dolist (key dict-keys ht)
      (setf (gethash (ns-to-lisp-object nil key) ht)
            (if no-convert
              (#/objectForKey: dict key)
              (ns-to-lisp-object nil (#/objectForKey: dict key)))))))

(defmethod ns-to-lisp-assoc ((dict ns:ns-dictionary) &key (no-convert nil))
  (let ((assoc-lst nil)
        (dict-keys (ns-to-lisp-list (#/allKeys dict) :no-convert t)))
    (dolist (key dict-keys assoc-lst)
      (setf assoc-lst
            (acons (ns-to-lisp-object nil key)
                   (if no-convert
                     (#/objectForKey: dict key)
                     (ns-to-lisp-object nil (#/objectForKey: dict key)))
                   assoc-lst)))))

(defmethod lisp-to-ns-dict ((alist list))
  ;; alist must be in the form of an association list
  (let* ((count (list-length alist))
         (new-dict (#/dictionaryWithCapacity: ns:ns-mutable-dictionary count)))
    (dolist (pair alist new-dict)
      (#/setObject:forKey: new-dict 
                           (lisp-to-ns-object nil (cdr pair))
                           (lisp-to-ns-object nil (car pair))))))

(defmethod lisp-to-ns-dict ((ht hash-table))
  (let* ((count (hash-table-count ht))
         (new-dict (#/dictionaryWithCapacity: ns:ns-mutable-dictionary count)))
    (maphash #'(lambda (key val)
                 (#/setObject:forKey: new-dict 
                                      (lisp-to-ns-object nil val)
                                      (lisp-to-ns-object nil key)))
             ht)
    new-dict))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lisp-ptr-wrapper
;;
;; This is a simple class that encapsulates a pointer to a lisp object so we can pass this
;; off to an Objective-C view and know what it points to when we get it back later.
;; Added is the ability to handle bindings 

(deftype objc-displayable () 
  '(and atom 
       (not sequence)
       (not hash-table)
       (not package) 
       (not pathname)
       (not random-state)
       (not readtable)
       (not array)
       (not stream)
       (not class)
       (not structure-object)
       (not standard-object)
       (not macptr)))

(defclass lisp-ptr-wrapper (ns:ns-object)
  ((lpw-lisp-ptr :accessor lpw-lisp-ptr)
   (lpw-controller :accessor lpw-controller)
   (lpw-depth :accessor lpw-depth)
   (lpw-parent :accessor lpw-parent))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/copyWithZone: :id)
                ((self lisp-ptr-wrapper) (zone (* #>NSZone)))
  ;; (ns-log (format nil "Copying wrapper for ~s" (lpw-lisp-ptr self)))
  self)

(let ((kvc-observed (make-instance 'assoc-array :rank 2))
      (obj-wrappers (make-instance 'assoc-array :rank 2)))
  ;; this assoc-array keeps track of paths that are being observed and the
  ;; corresponding lisp-ptr-wrapper object that is ostensibly being observed.

  (defun make-ptr-wrapper (ptr &key (depth 1) (parent nil) (controller nil))
    ;; (ns-log (format nil "Making wrapper for ~s" ptr))
    (let ((lpw (make-instance 'lisp-ptr-wrapper)))
      (setf (lpw-lisp-ptr lpw) ptr)
      (setf (lpw-depth lpw) depth)
      (setf (lpw-parent lpw) parent)
      (setf (lpw-controller lpw) controller)
      (setf (assoc-aref obj-wrappers controller ptr) lpw)
      lpw))

  (defmethod wrapper-for (controller lisp-obj &key (depth 0) (parent nil))
    (or (assoc-aref obj-wrappers controller lisp-obj)
        (setf (assoc-aref obj-wrappers controller lisp-obj)
              (make-ptr-wrapper lisp-obj 
                                :depth depth
                                :parent parent
                                :controller controller))))

  (defmethod note-kvc-observed ((self lisp-ptr-wrapper) lisp-obj path-sym)
    ;; (ns-log (format nil "Observing ~s for ~s" path-sym lisp-obj))
    (pushnew self (assoc-aref kvc-observed lisp-obj path-sym)))

  (defmethod will-change-value-for-key (owner key)
    ;; called from a lisp object to tell us that a value will be changed.
    ;; We find the lisp-ptr-wrapper instances that have been used to access
    ;; the owner via the specified key and call the appropriate
    ;; method to lets KVC know what is going on.
    ;; (ns-log (format nil "Will change ~s for ~s" key owner))
    (let ((owner-lpws (assoc-aref kvc-observed owner key))
          (objc-key (lisp-to-temp-nsstring (lisp-to-objc-keypathname key))))
      (dolist (lpw owner-lpws)
        ;; (ns-log (format nil "#/willChangeValueForKey: ~s ~s" lpw objc-key))
        (#/willChangeValueForKey: lpw objc-key))))

  (defmethod did-change-value-for-key (owner key)
    ;; called from a lisp object to tell us that a value changed.
    ;; We find the lisp-ptr-wrapper instances that have been used to access
    ;; the owner via the specified key and call the appropriate
    ;; method to lets KVC know what is going on.
    ;; (ns-log (format nil "Did change ~s for ~s" key owner))
    (let ((owner-lpws (assoc-aref kvc-observed owner key))
          (objc-key (lisp-to-temp-nsstring (lisp-to-objc-keypathname key))))
      (dolist (lpw owner-lpws)
        ;; (ns-log (format nil "#/didChangeValueForKey: ~s ~s" lpw objc-key))
        (#/didChangeValueForKey: lpw objc-key))))

  (defun kvc-observed ()
    kvc-observed)
)
;; end of definitions with access to kvc-observed assoc-array

(objc:defmethod (#/valueForKey: :id)
                ((self lisp-ptr-wrapper) (path :id))
  (let* ((lisp-path (objc-to-lisp-keypathname (ns-to-lisp-string path)))
         (next-obj (when (and (typep lisp-path 'function-name) (fboundp lisp-path))
                     (funcall lisp-path (lpw-lisp-ptr self)))))
    ;; First track that the path is being observed by somebody
    (note-kvc-observed self (lpw-lisp-ptr self) lisp-path)
    ;; (ns-log (format nil "(~s ~s) returned ~s" lisp-path (lpw-lisp-ptr self) next-obj))
    (cond ((typep next-obj 'objc:objc-object)
           next-obj)
          ((null next-obj)
           (%null-ptr))
          ((typep next-obj 'objc-displayable)
           (lisp-to-ns-object next-obj))
          (t
           (wrapper-for (lpw-controller self) next-obj :parent (lpw-lisp-ptr self))))))

(objc:defmethod (#/setValue:forKey: :void)
                ((self lisp-ptr-wrapper) (new-value :id) (path :id))
  (let* ((lisp-path (objc-to-lisp-keypathname (ns-to-lisp-string path)))
         (prev-obj (when (and (typep lisp-path 'function-name) (fboundp lisp-path))
                     (funcall lisp-path (lpw-lisp-ptr self))))
         (new-lisp-obj (ns-to-lisp-object prev-obj new-value))
         (setf-form `(setf (,lisp-path ,(lpw-lisp-ptr self)) ,new-lisp-obj)))
    ;; (ns-log (format nil "Evaling ~s" setf-form))
    (handler-case (eval setf-form)
      (condition (c)
                 (ns-log (format nil "condition: ~s evaling ~s" c setf-form))))))

(provide :ns-object-utils)