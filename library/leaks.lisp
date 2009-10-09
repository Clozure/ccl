;;;-*-Mode: LISP; Package: ccl -*-
;;;
;;;   Copyright (C) 2008, Clozure Associates and contributors
;;;   This file is part of OpenMCL.  
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

; leaks.lisp
; A few functions to help in finding memory leaks

(in-package :ccl)

;; Returns all objects that satisfy predicate of one of the types in
;; ccl::*heap-utilization-vector-type-names*
;; Note that these can contain stack-consed objects that are dead.
;; Use pointer-in-some-dynamic-area-p to be sure to follow only real objects
;; (ccl::heap-utilization) prints a useful list of object counts and sizes
;; per type.
(defun all-objects-of-type (type &optional predicate)
  (let ((typecode (position type ccl::*heap-utilization-vector-type-names*))
        (res nil))
    (when typecode
      (flet ((mapper (thing)
               (when (and (eq typecode (ccl::typecode thing))
                          (or (null predicate) (funcall predicate thing)))
                 (push thing res))))
        (declare (dynamic-extent #'mapper))
        (ccl::%map-areas #'mapper))
      res)))

;; Counts objects that satisfy predicate of one of the types in
;; ccl::*heap-utilization-vector-type-names*
(defun count-objects-of-type (type &optional predicate)
  (let ((typecode (position type ccl::*heap-utilization-vector-type-names*))
        (res 0))
    (when typecode
      (flet ((mapper (thing)
               (when (and (eq typecode (ccl::typecode thing))
                          (or (null predicate) (funcall predicate thing)))
                 (incf res))))
        (declare (dynamic-extent #'mapper))
        (ccl::%map-areas #'mapper))
      res)))

(defun count-conses ()
  (let ((res 0))
    (flet ((mapper (thing)
             (when (consp thing) (incf res))))
      (declare (dynamic-extent #'mapper))
      (ccl::%map-areas #'mapper))
    res))

;; Like set-difference, but uses a hash table to go faster.
(defun fast-set-difference (list1 list2 &optional (test #'eq))
  (let ((hash (make-hash-table :test test))
        (res nil))
    (dolist (e1 list1) (setf (gethash e1 hash) t))
    (dolist (e2 list2) (remhash e2 hash))
    (maphash (lambda (k v)
               (declare (ignore v))
               (push k res))
             hash)
    res))

;; Returns all references to object.
;; Note that these can contain stack-consed objects that are dead.
;; Use pointer-in-some-dynamic-area-p to be sure to follow only real objects
(defun find-references (object)
  (let ((res nil))
    (ccl::%map-areas
     (lambda (thing)
       (cond ((and (not (eq thing object))
                   (ccl::uvectorp thing)
                   (not (ccl::ivectorp thing)))
              (dotimes (i (ccl::uvsize thing))
                (when (eq object (ccl::uvref thing i))
                  (push thing res)
                  (return))))
             ((consp thing)
              (when(or (eq object (car thing))
                       (eq object (cdr thing)))
                (push thing res))))))
    res))

;; Return true if P is heap-consed
(defun pointer-in-some-dynamic-area-p (p)
 (block found
   (ccl::do-consing-areas (a)
     (when (eql (%fixnum-ref a target::area.code) ccl::area-dynamic)
       (when (ccl::%ptr-in-area-p p a)
         (return-from found t))))))

;; Find all transitive referencers to object-or-list. If as-object is
;; true, just start with object-or-list. If as-object is false, then if
;; object-or-list is a list, start with its elements, and ignore its
;; cons cells.
;; Returns a hash table with the references as keys.
(defun transitive-referencers (object-or-list &optional as-object)
  (let ((found (make-hash-table :test 'eq)))
    (cond ((or (atom object-or-list) as-object)
           (setf (gethash object-or-list found) t))
          (t (loop for cons on object-or-list
                   do
                (setf (gethash cons found) t
                      (gethash (car cons) found) t))))
    (ccl:gc)
    (format t "Searching") (finish-output)
    (loop
      (let ((added-one nil))
        (format t " ~d" (hash-table-count found)) (finish-output)
        (ccl::%map-areas
         (lambda (thing)
           (unless (or (not (pointer-in-some-dynamic-area-p thing))
                       (gethash thing found))
             (cond ((and (not (eq thing (ccl::nhash.vector found)))
                         (ccl::uvectorp thing)
                         (not (ccl::ivectorp thing))
                         (not (packagep thing)))
                    (dotimes (i (ccl::uvsize thing))
                      (let ((object (ccl::uvref thing i)))
                        (when (gethash object found)
                          (setf (gethash thing found) t
                                added-one t)
                          (return)))))
                   ((and (consp thing)
                         (pointer-in-some-dynamic-area-p (car thing))
                         (pointer-in-some-dynamic-area-p (cdr thing)))
                    (when (or (gethash (car thing) found)
                              (gethash (cdr thing) found))
                      (setf (gethash thing found) t)))))))
        (unless added-one
          (return))))
    (format t " done.~%") (finish-output)
    ;; Eliminate any cons that is referenced by another cons.
    ;; Also eliminate or replace objects that nobody will want to see.
    (let ((cons-refs (make-hash-table :test 'eq))
          (additions nil))
      (loop for cons being the hash-keys of found
            when (consp cons)
              do
           (when (consp (car cons))
             (setf (gethash (car cons) cons-refs) t))
           (when (consp (cdr cons))
             (setf (gethash (cdr cons) cons-refs) t)))
      (loop for key being the hash-keys of found
            when (or (and (consp key) (gethash key cons-refs))
                     (and (consp key) (eq (car key) 'ccl::%function-source-note))
                     (typep key 'ccl::hash-table-vector)
                     (when (and key
				(typep key
				  #+x8664-target 'ccl::symbol-vector
				  #-x8664-target 'symbol
				  ))
                       (push (ccl::symvector->symptr key) additions)
                       t)
                     (when (typep key
				  #+x8664-target 'ccl::function-vector
				  #-x8664-target 'function
				  )
                       (push (ccl::function-vector-to-function key) additions)
                       t))
              do
              (remhash key found))
      (dolist (addition additions)
        (setf (gethash addition found) t))
      (remhash object-or-list found)
      (unless (or (atom object-or-list) as-object)
        (loop for cons on object-or-list
             do
             (remhash cons found)
             (remhash (car cons) found)))
      found)))

;; One convenient way to print the hash table returned by transitive-referencers
(defun print-referencers (hash &key
                          predicate
                          (pause-period 20)
                          (print-circle t)
                          (print-length 20)
                          (print-level 5))
  (let ((cnt 0)
        (*print-circle* print-circle)
        (*print-length* print-length)
        (*print-level* print-level))
    (maphash (lambda (key value)
               (declare (ignore value))
               (when (or (null predicate) (funcall predicate key))
                 (format t "~s~%" key)
                 (when (> (incf cnt) pause-period)
                   (format t "Continue (Y/N)? ")
                   (unless (equalp (read-line) "Y")
                     (return-from print-referencers))
                   (setq cnt 0))))
             hash)))

;; Returns all the obsolete CLOS instances, those whose class has been
;; changed since they were created. Each will be updated as soon as
;; method dispatch is done on it."
(defun obsolete-instances (list)
  (let ((res nil))
    (dolist (i list)
      (when (eq 0 (ccl::%wrapper-hash-index (ccl::instance-class-wrapper i)))
        (push i res)))
    res))

;; Linux-only malloc leak finding
#+linux-target
(progn

;; (ccl::start-mtrace LOGFILE)
;; Do some work.
;; (ccl::stop-mtrace)
;; (ccl::parse-mtrace-log LOGFILE)
(defun start-mtrace (log-file)
  (touch log-file)
  (setf log-file (probe-file log-file))
  (setenv "MALLOC_TRACE" (namestring log-file))
  (gc)
  (#_mtrace))

(defun stop-mtrace ()
  (gc)
  (#_muntrace))

(defun parse-mtrace-log (log-file)
  (with-open-file (s log-file)
    (let ((hash (make-hash-table :test 'equal))
          (eof (list :eof)))
      (loop for line = (read-line s nil eof)
            until (eq line eof)
            when (and (> (length line) 2)
                      (equal "@ " (subseq line 0 2)))
              do
           (setf line (subseq line 2))
           (let ((plus-pos (search " + " line))
                 (minus-pos (search " - " line)))
             (cond (plus-pos
                    (let* ((where (subseq line 0 plus-pos))
                           (addr-and-size (subseq line (+ plus-pos 3)))
                           (space-pos (position #\space addr-and-size))
                           (addr (subseq addr-and-size 0 space-pos))
                           (size (subseq addr-and-size (1+ space-pos))))
                      (setf (gethash addr hash) (list where size))))
                   (minus-pos
                    (let ((addr (subseq line (+ minus-pos 3))))
                      (remhash addr hash))))))
      (let ((res nil))
        (maphash (lambda (key value)
                   (push (append value (list key)) res))
                 hash)
        res))))

;; Return the total number of bytes allocated by malloc()
(defun mallinfo ()
  (ccl:rlet ((mallinfo :mallinfo))
    (#_mallinfo mallinfo)
    (ccl::rref mallinfo :mallinfo.uordblks)))

#||
http://www.gnu.org/s/libc/manual/html_node/Statistics-of-Malloc.html

int arena
    This is the total size of memory allocated with sbrk by malloc, in bytes.
int ordblks
    This is the number of chunks not in use. (The memory allocator internally gets chunks of memory from the operating system, and then carves them up to satisfy individual malloc requests; see Efficiency and Malloc.)
int smblks
    This field is unused.
int hblks
    This is the total number of chunks allocated with mmap.
int hblkhd
    This is the total size of memory allocated with mmap, in bytes.
int usmblks
    This field is unused.
int fsmblks
    This field is unused.
int uordblks
    This is the total size of memory occupied by chunks handed out by malloc.
int fordblks
    This is the total size of memory occupied by free (not in use) chunks.
int keepcost
    This is the size of the top-most releasable chunk that normally borders the end of the heap (i.e., the high end of the virtual address space's data segment).
||#    

(defun show-malloc-info ()
  (rlet ((info :mallinfo))
    (#_mallinfo info)                   ;struct return invisible arg.
    (let* ((arena (pref info :mallinfo.arena))
           (ordblks (pref info :mallinfo.ordblks))
           (hblks (pref info :mallinfo.hblks))
           (hblkhd (pref info :mallinfo.hblkhd))
           (uordblks (pref info :mallinfo.uordblks))
           (fordblks (pref info :mallinfo.fordblks))
           (keepcost (pref info :mallinfo.keepcost)))
      (format t "~& arena size: ~d/#x~x" arena arena)
      (format t "~& number of unused chunks = ~d" ordblks)
      (format t "~& number of mmap'ed chunks = ~d" hblks)
      (format t "~& total size of mmap'ed chunks = ~d/#x~x" hblkhd hblkhd)
      (format t "~& total size of malloc'ed chunks = ~d/#x~x" uordblks uordblks)
      (format t "~& total size of free chunks = ~d/#x~x" fordblks fordblks)
      (format t "~& size of releaseable chunk = ~d/#x~x" keepcost keepcost))))

)  ;; end of linux-only code
