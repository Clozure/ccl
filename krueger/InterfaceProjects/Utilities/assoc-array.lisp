;;; assoc-array.lisp

;; Implements an N-dimensional "array" where indices are arbitrary objects
;; Implementation creates nested hash-tables

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


(defpackage :interface-utilities
  (:nicknames :iu)
  (:export assoc-array
           assoc-aref
           mapcar-assoc-array
           mapcar-hash-keys))

(in-package :iu)

(defclass assoc-array ()
  ((arr-rank :accessor arr-rank :initarg :rank)
   (index1-ht :accessor index1-ht)
   (index-tests :accessor index-tests :initarg :tests)
   (default-value :accessor default-value :initarg :default))
  (:default-initargs
    :rank 2
    :tests nil
    :default nil))

(defmethod initialize-instance :after ((self assoc-array) &key tests &allow-other-keys)
  (setf (index1-ht self) 
        (make-hash-table :test (or (first tests) #'eql))))

(defmethod assoc-aref ((self assoc-array) &rest indices)
  (unless (eql (list-length indices) (arr-rank self))
    (error "Access to ~s requires ~s indices" self (arr-rank self)))
  (do* ((res (index1-ht self))
        (index-list indices (rest index-list))
        (indx (first index-list) (first index-list))
        (found-next t))
       ((null index-list) (if found-next
                            (values res t)
                            (values (default-value self) nil)))
    (if found-next
      (multiple-value-setq (res found-next) (gethash indx res))
      (return-from assoc-aref (values (default-value self) nil)))))

(defmethod (setf assoc-aref) (new-val (self assoc-array) &rest indices)
  (unless (eql (list-length indices) (arr-rank self))
    (error "Access to ~s requires ~s indices" self (arr-rank self)))
  (let* ((ht (index1-ht self))
         (last-indx (do* ((dim 1 (1+ dim))
                          (index-list indices (rest index-list))
                          (indx (first index-list) (first index-list))
                          (tests (rest (index-tests self)) (rest tests))
                          (test (first tests) (first tests)))
                         ((>= dim (arr-rank self)) indx)
                      (multiple-value-bind (next-ht found-next) (gethash indx ht)
                        (unless found-next
                          (setf next-ht (make-hash-table :test (or test #'eql)))
                          (setf (gethash indx ht) next-ht))
                        (setf ht next-ht)))))
    (setf (gethash last-indx ht) new-val)))

(defmethod mapcar-assoc-array ((func function) (self assoc-array) &rest indices)
  ;; collects list of results of applying func to each bound index at
  ;; the next level after the indices provided.
  (unless (<= (list-length indices) (arr-rank self))
    (error "Access to ~s requires ~s or fewer indices" self (arr-rank self)))
  (do* ((res (index1-ht self))
        (index-list indices (rest index-list))
        (indx (first index-list) (first index-list))
        (found-next t))
       ((null index-list) (when found-next
                            ;; apply map function to res
                            (typecase res
                              (hash-table (mapcar-hash-keys func res))
                              (cons (mapcar func res))
                              (sequence (map 'list func res)))))
    (if found-next
      (multiple-value-setq (res found-next) (gethash indx res))
      (return-from mapcar-assoc-array nil))))

(defmethod map-assoc-array ((func function) (self assoc-array) &rest indices)
  ;; collects list of results of applying func of two arguments to 
  ;; a bound index at the next level after the indices provided and to
  ;; the value resulting from indexing the array by appending that index
  ;; to those provided as initial arguments. This would typically be used
  ;; to get a list of all keys and values at the lowest level of an
  ;; assoc-array.
  (unless (<= (list-length indices) (arr-rank self))
    (error "Access to ~s requires ~s or fewer indices" self (arr-rank self)))
  (do* ((res (index1-ht self))
        (index-list indices (rest index-list))
        (indx (first index-list) (first index-list))
        (found-next t))
       ((null index-list) (when found-next
                            ;; apply map function to res
                            (typecase res
                              (hash-table (map-hash-keys func res))
                              (cons (mapcar func res nil))
                              (sequence (map 'list func res nil)))))
    (if found-next
      (multiple-value-setq (res found-next) (gethash indx res))
      (return-from map-assoc-array nil))))

(defun print-last-level (key val)
  (format t "~%Key = ~s   Value = ~s" key val))

(defmethod last-level ((self assoc-array) &rest indices)
  (apply #'map-assoc-array #'print-last-level self indices))

(defmethod map-hash-keys ((func function) (self hash-table))
  (let ((res nil))
    (maphash #'(lambda (key val)
                 (push (funcall func key val) res))
             self)
    (nreverse res)))

(defmethod mapcar-hash-keys ((func function) (self hash-table))
  (let ((res nil))
    (maphash #'(lambda (key val)
                 (declare (ignore val))
                 (push (funcall func key) res))
             self)
    (nreverse res)))

(provide :assoc-array)
      
