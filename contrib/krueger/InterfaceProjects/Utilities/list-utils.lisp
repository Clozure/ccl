;; list-utils.lisp

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
  (:export add-to-list-at
           delete-from-list
           find-cdr
           sort-list-in-place))

(in-package :iu)

(defun add-to-list-at (lst indx value)
  ;; splices value into lst at the designated indx
  ;; if indx = 0 we make sure that the lst pointer remains
  ;; valid.
  (cond ((null lst)
         (setf lst (list value)))
        ((>= indx (list-length lst))
         (nconc lst (list value)))
        (t
         (let* ((nth-ptr (nthcdr indx lst))
                (new-cons (cons (car nth-ptr) (cdr nth-ptr))))
           ;; splice in the new value at the position of the specified indx
           (setf (car nth-ptr) value)
           (setf (cdr nth-ptr) new-cons))))
  lst)

(defun delete-from-list (lst thing)
  (cond ((eql 1 (list-length lst))
         nil)
        ((eq thing (first lst))
         (setf (car lst) (cadr lst))
         (setf (cdr lst) (cddr lst))
         (if (and (null (car lst)) (null (cdr lst)))
           nil
           lst))
        (t 
         (delete thing lst))))

(defun find-cdr (lst cdr-thing)
  ;; return the cons cell with a cdr eq to cdr-thing
  (do* ((cons-cell lst (cdr cons-cell)))
       ((or (null cons-cell) (eq (cdr cons-cell) cdr-thing)) cons-cell)))

(defun sort-list-in-place (lst sort-pred &optional (sort-key nil))
  ;; Sorting a list normally results in a reordering of the cons
  ;; cells. We swap the contents of cons cells around after sorting 
  ;; so that previous references to the list are still valid.
  ;; That way we can sort "in place" without having to copy the whole list.
  (let ((old-first-cons lst)
        (new-first-cons nil))
    (if sort-key
      (setf new-first-cons (sort lst sort-pred :key sort-key))
      (setf new-first-cons (sort lst sort-pred)))
    (let* ((prev-cons (find-cdr new-first-cons old-first-cons))
           (new-first-car (car new-first-cons))
           (new-first-cdr (cdr new-first-cons)))
      (when prev-cons
        ;; exchange the two cons cells: the one that used to 
        ;; be the first (called the old-first-cons) with the 
        ;; first in the sorted list (called the new-first-cons)
        ;; first exchange cars
        (setf (car new-first-cons) (car old-first-cons))
        (setf (car old-first-cons) new-first-car)
        ;; always set cdr of new-first-cons to the cdr of the old-first-cons 
        (setf (cdr new-first-cons) (cdr old-first-cons))  
        ;; have to be more careful about other cdr pointers
        (if (eq prev-cons new-first-cons)
          (setf (cdr old-first-cons) new-first-cons)
          (progn
            (setf (cdr prev-cons) new-first-cons)
            (setf (cdr old-first-cons) new-first-cdr)))))
    lst))

(provide :list-utils)
