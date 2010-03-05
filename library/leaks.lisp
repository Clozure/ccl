;;;-*-Mode: LISP; Package: ccl -*-
;;;
;;;   Copyright (C) 2008-2009 Clozure Associates and contributors
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
(defun start-mtrace (log-file &key gc-first)
  (delete-file log-file)
  (touch log-file)
  (setenv "MALLOC_TRACE" (native-translated-namestring (truename log-file)))
  (when gc-first (gc))
  (#_mtrace))

(defun stop-mtrace (&key gc-first)
  (when gc-first (gc))
  (#_muntrace))

(defun parse-mtrace-log (log-file &key (duplicate-alloc :show)
                                       (unmatched-free :collect)
                                       (failed-realloc :show)
                                       (hash (make-hash-table :test 'eql))
                                       (id nil))
  (let ((errors nil))
    (with-open-file (stream log-file)
      (loop for line = (read-line stream nil nil) while line
            as pos = (if (and (> (length line) 2) (eql (aref line 0) #\@) (eql (aref line 1) #\space))
                         (1+ (position #\space line :start 2))
                         0)
            as address = (let ((npos (+ pos 2)))
                           (when (and (< (+ npos 2) (length line))
                                      (eql (aref line npos) #\0)
                                      (eql (aref line (1+ npos)) #\x))
                             (parse-integer line :radix 16
                                            :start (+ npos 2)
                                            :end (position #\space line :start npos))))
            as last-data = (gethash address hash)
            do (ecase (aref line pos)
                 ((#\+ #\>)
                    (let ((this-data (if id (cons id line) line)))
                      (if last-data
                          (ecase duplicate-alloc
                            (:collect (push (list :duplicate
                                                  (if (eq (aref line pos) #\+) :alloc :realloc)
                                                  last-data this-data)
                                            errors))
                            ((:show nil) (format t "Duplicate ~a:~%~a~%~a~%"
                                                 (if (eq (aref line pos) #\+) "alloc" "realloc")
                                                 last-data this-data))
                            (:ignore nil))
                          (setf (gethash address hash) this-data))))
                 ((#\- #\<)
                    (if last-data
                        (remhash address hash)
                        (let ((this-data (if id (cons id line) line)))
                          (ecase unmatched-free
                            (:collect (push (list :unmatched
                                                  (if (eq (aref line pos) #\-) :free :realloc)
                                                  this-data)
                                            errors))
                            ((:show nil) (format t "Unmatched ~a: ~a~%"
                                                 (if (eq (aref line pos) #\-) "free" "realloc")
                                                 this-data))
                            (:ignore nil)))))
                 ((#\=) ;; ignore start/end
                    ;; (format t "~&~a" line)
                    nil)
                 ((#\!)
                    (let ((this-data (if id (cons id line) line)))
                      (ecase failed-realloc
                        (:collect (push (list :failed :realloc this-data) errors))
                        ((:show nil) (format t "Failed realloc: ~a" this-data))
                        (:ignore nil)))))))
    (values (nreverse errors) hash)))

(defun pretty-print-mtrace-summary (log-file)
  (multiple-value-bind (errors malloc-hash) (parse-mtrace-log log-file)
    (let* ((malloc-sum 0)
           (malloc-count 0)
           (free-count 0))
      (when (> (hash-table-count malloc-hash) 0)
        (format t "~&Malloced but not freed:~%")
        (loop for line being the hash-value of malloc-hash
              do (let* ((plus-pos (or (search " + " line) (search " > " line)))
                        (size-pos (position #\space line :start (+ plus-pos 3))))
                   (incf malloc-count)
                   (incf malloc-sum (parse-integer line :radix 16 :start (+ size-pos 3)))
                   (format t "~& ~A" line))))
      (when (find :unmatched errors :key #'car)
        (format t "~&Freed but not malloced:~%")
        (loop for (type nil line) in errors
              do (when (eq type :unmatched)
                   (incf free-count)
                   (format t " ~a" line))))
      (format t "~&~aK in ~a mallocs not freed, ~A frees not malloced"
              (/ malloc-sum 1024.0)
              malloc-count
              free-count)))
  (values))


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



;; Parse /proc/<pid>/maps

(defun parse-proc-maps (&optional (pid (ccl::getpid)))
  (let ((perm-cache ())
        (name-cache ()))
    (with-open-file (s (format nil "/proc/~d/maps" pid))
      (loop for line = (read-line s nil) while line
            as low-end = (position #\- line)
            as high-end = (position #\space line :start (1+ low-end))
            as perms-end = (position #\space line :start (1+ high-end))
            as offset-end = (position #\space line :start (1+ perms-end))
            as device-end = (position #\space line :start (1+ offset-end))
            as inode-end = (position #\space line :start (1+ device-end))
            as name-start = (position #\space line :start inode-end :test-not #'eql)
            as low = (parse-integer line :start 0 :end low-end :radix 16)
            as high = (parse-integer line :start (1+ low-end) :end high-end :radix 16)
            as perms = (let ((p (subseq line (1+ high-end) perms-end)))
                         (or (find p perm-cache :test #'equal)
                             (car (setq perm-cache (cons p perm-cache)))))
            as name = (and name-start
                           (let ((f (subseq line name-start)))
                             (or (find f name-cache :test #'equal)
                                 (car (setq name-cache (cons f name-cache))))))
            collect (list low high perms name)))))

(defun proc-maps-diff (map1 map2)
  ;; Compute change from map1 to map2.
  ;;  Remove segment -> (:remove low high ...)
  ;;  Add segment  -> (:add low high ...)
  ;;  grow segment -> (:grow low high new-high ...)
  (let ((added (copy-list map2))
        (changes nil))
    (loop for m1 in map1 as match = (find (car m1) added :key #'car)
          do (when match
               (setq added (delete match added))
               (unless (equal (cddr m1) (cddr match))
                 (warn  "Segment changed ~s -> ~s" m1 match)))
          do (cond ((null match)
                    (push `(:remove ,(- (car m1) (cadr m1)) ,@m1) changes))
                   ((< (cadr m1) (cadr match))
                    (push `(:grow ,(- (cadr match) (cadr m1)) ,@m1) changes)) 
                   ((< (cadr match) (cadr m1))
                    (push `(:shrink ,(- (cadr match) (cadr m1)) ,@m1) changes)) 
                   (t nil)))
    (loop for m in added do (push `(:new ,(- (cadr m) (car m)) ,@m) changes))
    changes))

)  ;; end of linux-only code

(defun get-allocation-sentinel (&key (gc-first t))
  ;; Return the object with the highest address that can be guaranteed to be at a lower
  ;; address than any newer objects.
  ;; If gc-first is true, can also conversely guarantee that all older objects are at a
  ;; lower address than the sentinel.  If gc-first is false, than there may be some
  ;; already-allocated objects at higher addresses, though no more than the size of the
  ;; youngest generation (and usually even less than that). Second value returned is the
  ;; size of the active region above the sentinel.
  (with-other-threads-suspended
    (when gc-first (gc)) ;; get rid of thread allocation chunks.  Wish could just egc...
    ;; This mustn't cons.
    (let* ((first-area (%normalize-areas)) ;; youngest generation
           (min-base (loop with current = (%current-tcr)
                           for tcr = (%fixnum-ref current target::tcr.next)
                             then (%fixnum-ref tcr target::tcr.next)
                           as base fixnum = (%fixnum-ref tcr target::tcr.save-allocbase)
                           when (> base 0)
                             minimize base
                           until (eql tcr current)))
           (active (%fixnum-ref first-area  target::area.active))
           (limit (if (eql min-base 0) active min-base))
           (last-obj nil))
      ;; Normally will find it in the youngest generation, but loop in case limit = area.low.
      (block walk
        (flet ((skip (obj)
                 (declare (optimize (speed 3) (safety 0))) ;; lie
                 (unless (%i< obj limit)
                   (return-from walk))
                 (setq last-obj obj)))
          (declare (dynamic-extent #'skip))
          (loop for area = first-area then (%fixnum-ref area target::area.succ)
                until (neq (%fixnum-ref area target::area.code) area-dynamic)
                when (< (%fixnum-ref area target::area.low) (%fixnum-ref area target::area.active))
                  do (walk-static-area area #'skip))))
      (values last-obj (%i- active limit)))))

