;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   This file is part of Clozure CL.  
;;;
;;;   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with Clozure CL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with Clozure CL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

;;; Low-level list sorting routines.  Used by CLOS and SORT.

(in-package "CCL")

(eval-when (:compile-toplevel :execute)

(defmacro apply-key (key value)
  `(if ,key
     (funcall ,key ,value)
     ,value))

)

;; A macro to make predicate & key into lfuns, or maybe NIL.
(defmacro canonicalize-pred-and-key (&optional (pred 'pred) (key 'key))
  `(progn (setq ,pred (coerce-to-function ,pred))
          (unless (null ,key)
            (setq ,key (coerce-to-function ,key))
            (if (eq ,key #'identity) (setq ,key nil)))))


(defun final-cons (p)
  (do* ((drag p lead)
        (lead (cdr p) (cdr lead)))
       ((null lead)
        drag)))

;;; 		   modified to return a pointer to the end of the result
;;; 		      and to not cons header each time its called.
;;; It destructively merges list-1 with list-2.  In the resulting
;;; list, elements of list-2 are guaranteed to come after equal elements
;;; of list-1.
(defun merge-lists* (list-1 list-2 pred key)
  (declare (optimize (speed 3) (safety 0)))
  (if (null key)
    (merge-lists*-no-key list-1 list-2 pred) 
    (cond ((null list-1)
           (values list-2 (final-cons list-2)))
          ((null list-2)
           (values list-1 (final-cons list-1)))
          (t (let* ((result (cons nil nil))
                    (P result)                  ; P points to last cell of result
                    (key-1 (apply-key key (car list-1)))
                    (key-2 (apply-key key (car list-2))))
               (declare (dynamic-extent result))
               (declare (type list p))
               (loop
                 (cond ((funcall pred key-2 key-1)
                        (rplacd P list-2)       ; append the lesser list to last cell of
                        (setq P (cdr P))        ;   result.  Note: test must bo done for
                        (pop list-2)            ;   list-2 < list-1 so merge will be
                        (unless list-2          ;   stable for list-1
                          (rplacd P list-1)
                          (return (values (cdr result) (final-cons p))))
                        (setq key-2 (apply-key key (car list-2))))
                       (T (rplacd P list-1)         
                          (setq P (cdr P))
                          (pop list-1)
                          (unless list-1
                            (rplacd P list-2)
                            (return (values (cdr result) (final-cons p))))
                          (setq key-1 (apply-key key (car list-1)))))))))))

(defun merge-lists*-no-key (list-1 list-2 pred)
  (declare (optimize (speed 3) (safety 0)))
  (cond ((null list-1)
         (values list-2 (final-cons list-2)))
        ((null list-2)
         (values list-1 (final-cons list-1)))
        (t (let* ((result (cons nil nil))
                  (P result)                  ; P points to last cell of result
                  (key-1 (car list-1))
                  (key-2 (car list-2)))
             (declare (dynamic-extent result))
             (declare (type list p))
             (loop
               (cond ((funcall pred key-2 key-1)
                      (rplacd P list-2)        ; append the lesser list to last cell of
                      (setq P (cdr P))         ;   result.  Note: test must bo done for
                      (pop list-2)             ;   list-2 < list-1 so merge will be
                      (unless list-2           ;   stable for list-1
                        (rplacd P list-1)
                        (return (values (cdr result) (final-cons p))))
                      (setq key-2 (car list-2)))
                     (T (rplacd P list-1)
                        (setq P (cdr P))
                        (pop list-1)
                        (unless list-1
                          (rplacd P list-2)
                          (return (values (cdr result) (final-cons p))))
                        (setq key-1 (car list-1)))))))))

(defun sort-list (list pred key)
  (canonicalize-pred-and-key pred key)
  (let ((head (cons nil list))          ; head holds on to everything
	  (n 1)                                ; bottom-up size of lists to be merged
	  unsorted                             ; unsorted is the remaining list to be
                                        ;   broken into n size lists and merged
	  list-1                               ; list-1 is one length n list to be merged
	  last)                                ; last points to the last visited cell
    (declare (fixnum n))
    (declare (dynamic-extent head))
    (loop
      ;; start collecting runs of n at the first element
      (setf unsorted (cdr head))
      ;; tack on the first merge of two n-runs to the head holder
      (setf last head)
      (let ((n-1 (1- n)))
        (declare (fixnum n-1))
        (loop
	    (setf list-1 unsorted)
	    (let ((temp (nthcdr n-1 list-1))
	          list-2)
	      (cond (temp
		       ;; there are enough elements for a second run
		       (setf list-2 (cdr temp))
		       (setf (cdr temp) nil)
		       (setf temp (nthcdr n-1 list-2))
		       (cond (temp
			        (setf unsorted (cdr temp))
			        (setf (cdr temp) nil))
		             ;; the second run goes off the end of the list
		             (t (setf unsorted nil)))
		       (multiple-value-bind (merged-head merged-last)
                                            (merge-lists* list-1 list-2 pred key)
		         (setf (cdr last) merged-head)
		         (setf last merged-last))
		       (if (null unsorted) (return)))
		      ;; if there is only one run, then tack it on to the end
		      (t (setf (cdr last) list-1)
		         (return)))))
        (setf n (ash n 1)) ; (+ n n)
        ;; If the inner loop only executed once, then there were only enough
        ;; elements for two runs given n, so all the elements have been merged
        ;; into one list.  This may waste one outer iteration to realize.
        (if (eq list-1 (cdr head))
	    (return list-1))))))


;; The no-key version of %sort-list
;; list had better be a list.
;; pred had better be functionp.
(defun %sort-list-no-key (list pred)
  (sort-list list pred nil))

(defun sort-list-error ()
  (error "List arg to SORT not a proper list"))



