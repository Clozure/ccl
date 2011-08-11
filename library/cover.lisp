;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2008-2009 Clozure Associates
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

;;; Code coverage reporting facility, originally inspired by SBCL's sb-cover API.

(in-package :ccl)

(eval-when (eval load compile)
  (export '(*compile-code-coverage*
            report-coverage
            reset-coverage
            clear-coverage
            save-coverage-in-file
            restore-coverage-from-file
            
            save-coverage  ;; stupid name, here for backward compatibility
            get-coverage
            restore-coverage
            combine-coverage
            read-coverage-from-file
            write-coverage-to-file
            
            reset-incremental-coverage
            get-incremental-coverage
            incremental-coverage-source-matches
            incremental-coverage-svn-matches
            
            coverage-statistics
            coverage-source-file
            coverage-expressions-total
            coverage-expressions-entered
            coverage-expressions-covered
            coverage-unreached-branches
            coverage-code-forms-total
            coverage-code-forms-covered
            coverage-functions-total
            coverage-functions-fully-covered
            coverage-functions-partly-covered
            coverage-functions-not-entered
            
            without-compiling-code-coverage)))

(defconstant $no-style 0)
(defconstant $not-executed-style 1)
(defconstant $totally-covered-style 2)
(defconstant $partially-covered-style 3)

;; These global values are for use in debugging only.  Exported functions always shadow these with thread-local tables.
(defparameter *file-coverage* ())
(defparameter *coverage-tags* nil)
(defparameter *code-note-tags* nil)
(defparameter *coverage-frame-name* "FF0")

(defparameter *code-note-subnotes* (make-hash-table :test #'eq))
(defparameter *code-note-function* (make-hash-table :test #'eq))
(defparameter *entry-note-function* (make-hash-table :test #'eq))
(defparameter *code-note-index* (make-hash-table :test #'eq))
(defparameter *emitted-code-notes* (make-array 10 :adjustable t :fill-pointer 0))

(defparameter *source-note-index* (make-hash-table :test #'eq))
(defparameter *source-code-notes* (make-hash-table :test #'eq))
(defparameter *covered-source-notes* (make-array 10 :adjustable t :fill-pointer 0))


(defmacro with-coverage-decoding ((&key tags (precompute t)) &body body)
  ;; Set up thread-local environment, and decode tags, since those aren't file-specific
  `(let* ((*coverage-tags* nil)
          (*code-note-tags* nil)
          (*file-coverage* nil)
          (*coverage-frame-name* (format nil "FF~x" (random most-positive-fixnum)))
          (*code-note-subnotes* (make-hash-table :test #'eq :shared nil))
          (*code-note-function* (make-hash-table :test #'eq :shared nil))
          (*entry-note-function* (make-hash-table :test #'eq :shared nil))
          (*code-note-index* ,(when precompute `(make-hash-table :test #'eq :shared nil)))
          (*emitted-code-notes* ,(when precompute `(make-array 100 :adjustable t :fill-pointer 0)))
          (*source-note-index* ,(when precompute `(make-hash-table :test #'eq :shared nil)))
          (*source-code-notes* ,(when precompute `(make-hash-table :test #'eq :shared nil)))
          (*covered-source-notes* ,(when precompute `(make-array 100 :adjustable t :fill-pointer 0))))
     ,@(when tags `((decode-coverage-tags ,tags)))
     ,@body))


(defmacro with-decoded-file-coverage ((coveragevar data &key) &body body)
  `(progn
     ;; Wonder if it'd be faster to make new tables instead of clrhash...
     (clrhash *code-note-subnotes*)
     (clrhash *code-note-function*)
     (clrhash *entry-note-function*)
     (when *code-note-index* (clrhash *code-note-index*))
     (when *emitted-code-notes* (setf (fill-pointer *emitted-code-notes*) 0))
     (when *source-note-index* (clrhash *source-note-index*))
     (when *covered-source-notes* (setf (fill-pointer *covered-source-notes*) 0))
     (when *source-code-notes* (clrhash *source-code-notes*))
     (let ((,coveragevar (decode-file-coverage ,data)))
       (push ,coveragevar *file-coverage*)
       ,@body)))


(defstruct (coverage-state (:conc-name "%COVERAGE-STATE-"))
  alist)

(defstruct incremental-coverage
  list)

;; Wrapper in case we ever want to do dwim on raw alists
(defun coverage-state-alist (coverage)
  (etypecase coverage
    (coverage-state (%coverage-state-alist coverage))))


(defstruct (ccl:coverage-statistics (:conc-name "COVERAGE-"))
  source-file
  expressions-total
  expressions-entered
  expressions-covered
  unreached-branches
  code-forms-total
  code-forms-covered
  functions-total
  functions-fully-covered
  functions-partly-covered
  functions-not-entered)


(defun file-coverage-file (entry)
  (car entry))

(defun file-coverage-functions (entry)
  (cadr entry))

(defun file-coverage-toplevel-functions (entry)
  (caddr entry))

(defun file-coverage-statistics (entry)
  (cdddr entry))

(defun file-coverage-index (entry)
  (position entry *file-coverage*))

(defun code-note-subnotes (note) ;; reversed parent chain
  (gethash note *code-note-subnotes*))

(defun emitted-code-note-p (note)
  (gethash note *code-note-function*))

(defun code-note-function (note)
  (gethash note *code-note-function*))

(defun entry-code-note-p (note)
  (gethash note *entry-note-function*))

(defun code-note-index (code-note)
  (gethash code-note *code-note-index*))

(defun code-note-tags (code-note)
  (gethash code-note *code-note-tags*))

(defun source-code-notes (source-note)
  (gethash source-note *source-code-notes*))

(defun source-note-index (source-note)
  (gethash source-note *source-note-index*))

(defun source-coverage (source)
  (loop with entered = nil and covered = t
        for note in  (source-code-notes source)
        do (case (code-note-code-coverage note)
             ((nil) (setq covered nil))
             ((full) (setq entered t))
             (t (setq entered t covered nil)))
        finally (return (and entered (if covered 'full t)))))

(defun map-function-coverage (lfun fn &optional refs)
  (let ((refs (cons lfun refs))
        (source (function-outermost-entry-source lfun)))
    (declare (dynamic-extent refs))
    (lfunloop for imm in lfun
              when (code-note-p imm)
              do (funcall fn imm)
              when (and (functionp imm)
                        (not (memq imm refs))
                        ;; Make sure this fn is in the source we're currently looking at.
                        ;; It might not be, if it is referenced via (load-time-value (foo))
                        ;; where (foo) returns an lfun from some different source entirely.
                        ;; CL-PPCRE does that.
                        (or (null source) (eq source (function-outermost-entry-source imm))))
              do (map-function-coverage imm fn refs))))

(defun collect-coverage-subfunctions (lfun refs)
  (let ((refs (cons lfun refs))
        (source (function-outermost-entry-source lfun)))
    (declare (dynamic-extent refs))
    (assert source) ;; all source-less functions have been eliminated.
    (nconc
     (and (function-entry-code-note lfun) (list lfun))
     (lfunloop for imm in lfun
               when (and (functionp imm)
                         (not (memq imm refs))
                         (eq source (function-outermost-entry-source imm)))
               nconc (collect-coverage-subfunctions imm refs)))))

(defun code-covered-info.file (data) (and (consp data) (car data)))
(defun code-covered-info.fns (data) (and (consp data) (if (consp (cdr data)) (cadr data) (cdr data))))
(defun code-covered-info.ef (data) (and (consp data) (consp (cdr data)) (caddr data)))
(defun code-covered-info.id (data) (and (consp data) (consp (cdr data)) (cadddr data)))

(defun code-covered-info-with-fns (data new-fns)
  (assert (consp data))
  (if (consp (cdr data))
    (let ((new (copy-list data)))
      (setf (cadr new) new-fns)
      new)
    (cons (car data) new-fns)))


(defun decode-file-coverage (data &key (precompute t))
  (let ((file (code-covered-info.file data)))
    (when file
      (let* ((file-name (pathname-name file))
             (file-type (pathname-type file))
             (toplevel-functions (loop for fn across (code-covered-info.fns data)
                                       nconc (iterate flatten ((fn fn))
                                               (let* ((entry (function-entry-code-note fn))
                                                      (source (and entry (nearest-source-note entry))))
                                                 (if source
                                                   (let ((source-file (source-note-filename source)))
                                                     ;; ignore fns from other files, as could happen through '#.(fun).
                                                     ;; Unfortunately, can't do this reliably since source-note-filename can involve
                                                     ;; a logical host not defined in this image, use a heuristic.
                                                     (when (and (equalp (pathname-name source-file) file-name)
                                                                (equalp (pathname-type source-file) file-type))
                                                       (list fn)))
                                                   ;; A top level function without source must be a compiler-generated toplevel
                                                   ;; form, ignore it and treat its subfunctions as top level.
                                                   (lfunloop for imm in fn
                                                     when (functionp imm) nconc (flatten imm)))))))
             (all-functions (delete-duplicates
                             ;; Duplicates are possible if you have multiple instances of
                             ;; (load-time-value (foo)) where (foo) returns an lfun.
                             ;; CL-PPCRE does that.
                             (loop for fn in toplevel-functions
                                   nconc (collect-coverage-subfunctions fn nil))))
             (coverage (list* file
                              all-functions
                              toplevel-functions
                              (make-coverage-statistics :source-file file))))
        ;; record emitted notes
        (loop for fn in all-functions as entry = (function-entry-code-note fn)
              do (assert (eq fn (gethash entry *entry-note-function* fn)))
              do (setf (gethash entry *entry-note-function*) fn)
              do (lfunloop for imm in fn
                   when (code-note-p imm)
                   do (setf (gethash imm *code-note-function*) fn)))
        ;; Now get the emitted subnotes of any note (including emitted subnotes of unemitted notes)
        (loop for note being the hash-key of *code-note-function*
              do (loop for n = note then parent as parent = (code-note-parent-note n)
                       do (push note (gethash parent *code-note-subnotes*));; parent = nil collects toplevel notes
                       while (and parent (not (gethash parent *code-note-function*)))))
        ;; Now get source mapping
        (when precompute
          (precompute-source-coverage coverage)
          ;; bit of overkill, but we end up always wanting them.
          (compute-file-coverage-statistics coverage))
        coverage))))

#+debug
(defun show-notes (note)
  (when (functionp note)
    (setq note (function-entry-code-note note)))
  (labels ((show (note indent label)
             (dotimes (i indent) (write-char #\space))
             (format t "~a ~a" label note)
             (unless (emitted-code-note-p note)
               (format t " [Not Emitted]"))
             (when (entry-code-note-p note)
               (format t " (Entry to ~s)" (entry-code-note-p note)))
             (when (code-note-acode-range note)
               (multiple-value-bind (s e) (decode-file-range (code-note-acode-range note))
                 (format t " [acode ~a:~a]" s e)))
             (format t "~%")
             (when (code-note-p note)
               (loop with subindent = (+ indent 3)
                     for sub in (code-note-subnotes note) as i upfrom 1
                     do (show sub subindent (format nil "~a~d." label i))))))
    (show note 0 "")))

(defun assoc-by-filename (path alist)
  (let* ((true-path (probe-file path)))
    (find-if #'(lambda (data)
                 (or (equalp (car data) path)
                     (and true-path (equalp (probe-file (car data)) true-path))))
             alist)))

(defun ccl:clear-coverage ()
  "Clear all files from the coverage database. The files will be re-entered
into the database when the FASL files (produced by compiling with
CCL:*COMPILE-CODE-COVERAGE* set to true) are loaded again into the
image."
  (setq *code-covered-functions* nil))

(defun reset-function-coverage (lfun)
  (map-function-coverage lfun #'(lambda (note)
                                  (setf (code-note-code-coverage note) nil))))

(defun reset-function-incremental-coverage (lfun)
  (map-function-coverage lfun #'(lambda (note)
                                  (when (code-note-code-coverage note)
                                    (setf (code-note-code-coverage note) :prior)))))

(defun ccl:reset-coverage ()
  "Reset all coverage data back to the `Not executed` state."
  (loop for data in *code-covered-functions*
        do (typecase data
             (cons
                (loop for fn across (code-covered-info.fns data)
                      do (reset-function-coverage fn)))
             (function (reset-function-coverage data)))))


(defun ccl:reset-incremental-coverage ()
  "Mark a starting point for recording incremental coverage.
   Has no effect on regular coverage recording."
  (loop for data in *code-covered-functions*
        do (typecase data
             (cons
                (loop for fn across (code-covered-info.fns data)
                      do (reset-function-incremental-coverage fn)))
             (function (reset-function-incremental-coverage data)))))


;; Name used for consistency checking across file save/restore
(defun function-covered-name (fn)
  (let ((name (function-name fn)))
    (and (symbolp name)
         (symbol-package name)
         name)))
  

(defun coverage-mismatch (why &rest args)
  ;; Throw to somebody who knows what file we're working on.
  (throw 'coverage-mismatch (cons why args)))

(defmacro with-coverage-mismatch-catch ((saved-file) &body body)
  `(let ((file ,saved-file))
     (with-simple-restart (ignore-file "Ignore ~s and continue" file)
       (let ((err (catch 'coverage-mismatch 
                    ,@body
                    nil)))
         (when err
           (error "Mismatched coverage data for ~s, ~?" file (car err) (cdr err)))))))


;; (name . #(i1 i2 ...)) where in is either an index or (index . subfncoverage).
(defun save-function-coverage (fn &optional (refs ()))
  (let ((refs (cons fn refs))
        (source (function-outermost-entry-source fn)))
    (declare (dynamic-extent refs))
    (cons (function-covered-name fn)
          ;; See comments in map-function-coverage
          (lfunloop for imm in fn as i upfrom 0
                    when (and (code-note-p imm)
                              (code-note-code-coverage imm))
                    collect i into list
                    when (and (functionp imm)
                              (not (memq imm refs))
                              (or (null source) (eq source (function-outermost-entry-source imm))))
                    collect (cons i (save-function-coverage imm refs)) into list
                    finally (return (and list (coerce list 'vector)))))))

(defun copy-function-coverage (fn-data)
  (cons (car fn-data)
        (and (cdr fn-data)
             (map 'vector #'(lambda (imm-data)
                              (if (consp imm-data)
                                (cons (car imm-data)
                                      (copy-function-coverage (cdr imm-data)))
                                imm-data))
                  (cdr fn-data)))))

(defun restore-function-coverage (fn saved-fn-data &optional (refs ()))
  (let* ((refs (cons fn refs))
         (source (function-outermost-entry-source fn))
         (saved-name (car saved-fn-data))
         (saved-imms (cdr saved-fn-data))
         (nimms (length saved-imms))
         (n 0))
    (declare (dynamic-extent refs))
    (unless (equalp saved-name (function-covered-name fn))
      (coverage-mismatch "had function ~s now have ~s" saved-name fn))
    ;; See comments in map-function-coverage
    (lfunloop for imm in fn as i upfrom 0
              when (code-note-p imm)
              do (let* ((next (and (< n nimms) (aref saved-imms n))))
                   (when (if (consp next) (<= (car next) i) (and next (< next i)))
                     (coverage-mismatch "in ~s" fn))
                   (when (setf (code-note-code-coverage imm)
                               (and (eql next i) 'restored))
                     (incf n)))
              when (and (functionp imm)
                        (not (memq imm refs))
                        (or (null source) (eq source (function-outermost-entry-source imm))))
              do (let* ((next (and (< n nimms) (aref saved-imms n))))
                   (unless (and (consp next) (eql (car next) i))
                     (coverage-mismatch "in ~s" fn))
                   (restore-function-coverage imm (cdr next) refs)
                   (incf n)))))


(defun add-function-coverage (fn-data new-fn-data)
  (let* ((fn-name (car fn-data))
         (imms (cdr fn-data))
         (new-fn-name (car new-fn-data))
         (new-imms (cdr new-fn-data)))
    (flet ((kar (x) (if (consp x) (%car x) x)))
      (declare (inline kar))
      (unless (equalp fn-name new-fn-name)
        (coverage-mismatch "function ~s vs. ~s" fn-name new-fn-name))
      (when new-imms
        (loop for new across new-imms
              as old = (find (kar new) imms :key #'kar)
              if (and (null old) (fixnump new))
                collect new into extras
              else do (unless (eql old new)
                        (if (and (consp new) (consp old))
                          (add-function-coverage (cdr old) (cdr new))
                          (coverage-mismatch "in function ~s" fn-name)))
              finally (when extras
                        (setf (cdr fn-data)
                              (sort (concatenate 'vector imms extras) #'< :key #'kar))))))
    fn-data))


(defun ccl:get-coverage ()
  "Returns a snapshot of the current coverage state"
  (make-coverage-state
   :alist (loop for data in *code-covered-functions*
                when (consp data)
                  collect (code-covered-info-with-fns
                               data (map 'vector #'save-function-coverage (code-covered-info.fns data))))))

;; Backward compatibility with sbcl name.
(setf (symbol-function 'ccl:save-coverage) #'ccl:get-coverage)

(defun ccl:combine-coverage (coverage-states)
  (let ((result nil))
    (map nil
         (lambda (coverage-state)
           (loop for saved-data in (coverage-state-alist coverage-state)
                 as saved-file = (code-covered-info.file saved-data)
                 as saved-fns = (code-covered-info.fns saved-data)
                 as result-data = (assoc-by-filename saved-file result)
                 as result-fns = (code-covered-info.fns result-data)
                 do (with-coverage-mismatch-catch (saved-file)
                      (cond ((null result-fns)
                             (push (code-covered-info-with-fns
                                    saved-data (map 'vector #'copy-function-coverage saved-fns))
                                   result))
                            ((not (eql (length result-fns) (length saved-fns)))
                             (coverage-mismatch "different function counts"))
                            (t
                             (unless (equal (code-covered-info.id saved-data)
                                            (code-covered-info.id result-data))
                               (cerror "Ignore the mismatch"
                                       "Combining different versions of file ~s (checksum mismatch)"
                                       saved-file))
                             (loop for result-fn across result-fns
                                   for saved-fn across saved-fns
                                   do (add-function-coverage result-fn saved-fn)))))))
         coverage-states)
    (make-coverage-state :alist (nreverse result))))


(defun ccl:restore-coverage (coverage-state)
  "Restore the code coverage data back to an earlier state produced by SAVE-COVERAGE."
  (loop for saved-data in (coverage-state-alist coverage-state)
        for saved-file = (code-covered-info.file saved-data)
        as saved-fns = (code-covered-info.fns saved-data)
        for current-data = (assoc-by-filename saved-file *code-covered-functions*)
        as fns = (and current-data (code-covered-info.fns current-data))
        do (with-coverage-mismatch-catch (saved-file)
             (cond ((null fns)
                    (warn "Couldn't restore saved coverage for ~s, no matching file present"
                          saved-file))
                   ((not (eql (length fns) (length saved-fns)))
                    (coverage-mismatch "had ~s functions, now have ~s"
                                       (length saved-fns) (length fns)))
                   (t 
                    (unless (equal (code-covered-info.id saved-data)
                                   (code-covered-info.id current-data))
                      (cerror "Ignore the mismatch"
                              "Restoring different version of file ~s (checksum mismatch)"
                              saved-file))
                    (map nil #'restore-function-coverage fns saved-fns))))))

(defun ccl:get-incremental-coverage (&key (reset t))
  "Return the delta coverage since the last reset of incremental coverage.
  If RESET is true (the default), it also resets incremental coverage now."
  ;; An incremental coverage snapshot is just a list of covered (i.e. entered) code notes.
  ;; It is not savable in a file.
  (let ((covered nil))
    (flet ((get-fn (note)
             (let ((coverage (code-note-code-coverage note)))
               (when (and coverage (not (eq coverage :prior)))
                 (when reset (setf (code-note-code-coverage note) :prior))
                 (push note covered)))))
      (loop for data in *code-covered-functions*
            when (consp data)
              do (loop for fn across (code-covered-info.fns data)
                       do (map-function-coverage fn #'get-fn)))
      (make-incremental-coverage :list covered))))

(defun decode-coverage-tags (tags)
  (when tags
    (let ((note->tags (make-hash-table :test #'eq :shared nil)))
      (flet ((register (i delta)
               (loop for note in (incremental-coverage-list delta) do (push i (gethash note note->tags)))))
        (etypecase tags
          (hash-table
           (let* ((count (hash-table-count tags))
                  (tags-vector (make-array count)))
             (enumerate-hash-keys-and-values tags tags-vector nil)
             (loop for i from 0 below count
                   do (register i (gethash (aref tags-vector i) tags)))
             (setq *coverage-tags* tags-vector)))
        (list
         (loop for i upfrom 0 as delta in tags do (register i delta)
               finally (setq *coverage-tags* i)))
        (vector
         (loop for i from 0 below (length tags) do (register i (aref tags i))
               finally (setq *coverage-tags* i)))))
      (setq *code-note-tags* note->tags))))


(defun ccl:incremental-coverage-svn-matches (collection &key (directory (current-directory)) (revision :base))
  "Given a hash table COLLECTION whose values are incremental coverage deltas, return a list
  of all keys corresponding to those deltas that intersect any region in a file in DIRECTORY that
  has changed since revision REVISION in subversion."
  (incremental-coverage-source-matches collection (get-svn-changes :directory directory
                                                                   :revision revision
                                                                   :reverse t)))

(defun ccl:incremental-coverage-source-matches (collection sources)
  "Given a hash table COLLECTION whose values are incremental coverage delta, return a list
  of all keys corresponding to deltas that intersect any region in SOURCES.  SOURCES
  should be a list of source notes and/or pathnames"
  (let ((alist ()))
    (loop for source in sources
          as file = (source-note-filename source)
          ;; Typically source notes will have eq filenames since created all at once, so the
          ;; assq will find it after the first time.
          as cell = (or (assq file alist)
                        (assoc-by-filename file alist)
                        (let* ((data (or (assoc-by-filename file *code-covered-functions*)
                                         (error "There is no coverage info for ~s" file)))
                               (cell (list* file data nil)))
                          (push cell alist)
                          cell))
          do (push source (cddr cell)))
    (with-coverage-decoding (:precompute nil)
      (loop for (nil data . sources) in alist
            do (with-decoded-file-coverage (coverage data)
                 (loop for sn in sources
                       as matches = (code-notes-for-region coverage (source-note-start-pos sn) (source-note-end-pos sn))
                       nconc (flet ((matches (delta)
                                      (loop for note in (incremental-coverage-list delta) thereis (memq note matches))))
                               (typecase collection
                                 (hash-table (loop for key being the hash-key of collection using (hash-value delta)
                                                   when (matches delta) collect key))
                                 (sequence (coerce (remove-if-not #'matches collection) 'list))))))))))




(defun nearest-source-note (note)
  (loop for n = note then (code-note-parent-note n)
        thereis (and n (code-note-source-note n))))

(defun code-note-emitted-parent (note)
  (loop while (setq note (code-note-parent-note note))
        when (emitted-code-note-p note) return note))

;; Given a region of a file, find a set of code notes that completely covers it, i.e.
;; a set such that if none of the code notes in the set have been executed, then it's guaranteed
;; that modifying the region is not going to affect execution.  Try to make that set as small
;; as possible.
(defun code-notes-for-region (coverage start-pos end-pos)
  (let* ((notes (loop for fn in (file-coverage-toplevel-functions coverage)
                      as note = (function-entry-code-note fn) as source = (nearest-source-note note)
                      when (and (or (null end-pos) (< (source-note-start-pos source) end-pos))
                                (or (null start-pos) (< start-pos (source-note-end-pos source))))
                        ;; This function intersects the region.  Find the smallest subnote that contains all
                        ;; of this function's part of the region.
                        collect (let ((start (max start-pos (source-note-start-pos source)))
                                      (end (min end-pos (source-note-end-pos source))))
                                  (iterate tighten ((note note))
                                    (loop for subnote in (code-note-subnotes note)
                                          as subsource = (nearest-source-note subnote)
                                          do (when (and (<= (source-note-start-pos subsource) start)
                                                        (<= end (source-note-end-pos subsource)))
                                               (return (tighten subnote)))
                                          finally (return note))))))
         (emitted-notes (iterate splat ((notes notes))
                          (loop for note in notes
                                nconc (if (emitted-code-note-p note)
                                        (list note)
                                        (splat (code-note-subnotes note)))))))
    emitted-notes))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *loading-coverage*)

(defun ccl:write-coverage-to-file (coverage pathname)
  "Write the coverage state COVERAGE in the file designated by PATHNAME"
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (with-standard-io-syntax
      (let ((*package* (pkg-arg "CCL")))
        (format stream "(in-package :ccl)~%~s~%"
                `(setq *loading-coverage* ',(coverage-state-alist coverage)))))
    (values)))
  
(defun ccl:read-coverage-from-file (pathname)
  " Return the coverage state saved in the file.  Doesn't affect the current coverage state."
  (let ((*package* (pkg-arg "CCL"))
        (*loading-coverage* :none))
    (load pathname)
    (when (eq *loading-coverage* :none)
      (error "~s doesn't seem to be a saved coverage file" pathname))
    (make-coverage-state :alist *loading-coverage*)))

(defun ccl:save-coverage-in-file (pathname)
  "Save the current coverage state in the file designed by PATHNAME"
  (write-coverage-to-file (save-coverage) pathname))

(defun ccl:restore-coverage-from-file (pathname)
  "Set the current coverage state from the file designed by PATHNAME"
  (restore-coverage (read-coverage-from-file pathname)))

(defun common-coverage-directory ()
  (let* ((host :unknown)
         (rev-dir ()))
    (loop for data in *code-covered-functions*
          when (consp data)
            do (let ((file (probe-file (code-covered-info.file data))))
                 (when file
                   (cond ((eq host :unknown)
                          (setq host (pathname-host file)
                                rev-dir (reverse (pathname-directory file))))
                         ((not (equalp host (pathname-host file)))
                          (return-from common-coverage-directory nil))
                         (t
                          (let* ((path (pathname-directory file))
                                 (dir-len (length rev-dir))
                                 (len (length path)))
                            (if (< len dir-len)
                              (setq rev-dir (nthcdr (- dir-len len) rev-dir))
                              (setq path (subseq path 0 dir-len)))
                            (loop for pp on (reverse path) until (equalp pp rev-dir)
                                  do (pop rev-dir))))))))
    (unless (eq host :unknown)
      (make-pathname :host host :directory (reverse rev-dir)))))


(defun ccl:coverage-statistics ()
  (with-coverage-decoding ()
    (loop for data in *code-covered-functions*
          do (with-decoded-file-coverage (coverage data)
               (file-coverage-statistics coverage)))))

(defun compute-file-coverage-statistics (coverage)
  (count-unreached-branches coverage)
  (count-covered-aexps coverage)
  (count-covered-sexps coverage))

(defun native-file-namestring (file)
  (native-translated-namestring (make-pathname :name (pathname-name file)
                                               :type (pathname-type file))))


(defun ccl:report-coverage (output-file &key (external-format :default) (statistics t) (html t) (tags nil))
  "If :HTML is non-nil, generate an HTML report, consisting of an index file in OUTPUT-FILE
and, in the same directory, one html file for each instrumented source file that has been
loaded in the current session.
The external format of the source files can be specified with the EXTERNAL-FORMAT parameter.
If :STATISTICS is non-nil, a CSV file is generated with a table.  If
:STATISTICS is a filename, that file is used, else 'statistics.csv' is
written to the output directory.
If :TAGS is non-nil, it must be a hash table whose values are incremental coverage snapshots. This
causes the HTML report to include incremental coverage information"
  ;; TODO: *** How to present incremental coverage info in statistics file?
  (let* ((paths)
         (directory (make-pathname :name nil :type nil :defaults output-file))
         (coverage-dir (common-coverage-directory))
         (frame-file (and html (merge-pathnames output-file "index.html")))
         (index-file (and html (make-pathname :name (%str-cat (pathname-name frame-file) "_html")
                                              :defaults frame-file)))
         (tags-file (and tags (make-pathname :name (%str-cat (pathname-name frame-file) "_tags")
                                             :defaults frame-file)))
         (stats-file (and statistics (merge-pathnames (if (or (stringp statistics)
                                                              (pathnamep statistics))
                                                          (merge-pathnames statistics "statistics.csv")
                                                          "statistics.csv")
                                                      output-file))))
    (ensure-directories-exist directory)
    (with-coverage-decoding (:tags tags)
      (loop for data in *code-covered-functions* as file = (code-covered-info.file data)
            as truename =  (and file (or (probe-file file)
                                         (progn (warn "Cannot find ~s, won't report coverage" file)
                                                nil)))
            do (when truename
                 (let* ((src-name (enough-namestring truename coverage-dir))
                        (html-name (substitute
                                    #\_ #\: (substitute
                                             #\_ #\. (substitute
                                                      #\_ #\/ (namestring-unquote src-name))))))
                   (with-decoded-file-coverage (coverage data)
                     (when html
                       (let* ((checksum (fcomp-file-checksum file :external-format (code-covered-info.ef data))))
                         (unless (eql checksum (code-covered-info.id data))
                           (cerror "Try coloring anyway"
                                   "File ~s has changed since coverage source location info was recorded."
                                   file)))
                       (report-file-coverage frame-file coverage directory html-name external-format))
                     (push (list* src-name html-name coverage) paths)))))
      (when html
        (when tags-file
          (with-open-file (tags-stream tags-file
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
	    ;; have to create a style else changing style.width has no effect
            (format tags-stream "<html><head><style type='text/css'>
#tagsselect {  width: *; }
</style><script type='text/javascript'>
function tags_changed() {
  var file_frame = top.frames.T~a;
  if (file_frame) {
    var sel = document.getElementById('tagsselect');
    var len = sel.length;
    var tags = new Array();
    for (var i = 0; i < len; i++)
      if (sel[i].selected) tags.push(sel[i].value);
    file_frame.colorize(tags);
  }
}

function resize_tags() {
  var sel = document.getElementById('tagsselect');
  sel.style.width = sel.offsetParent.scrollWidth + 'px';
}

function init_tags () {
  var sel = document.getElementById('tagsselect');
  var len = sel.length;
  for (var i = 0; i < len; i++) sel[i].selected = true;
  sel.focus();
  sel.onchange = tags_changed;

  var fs = top.document.getElementById('tagsframeset');
  fs.cols = Math.min(sel.offsetLeft + sel.offsetWidth, 0.3 * fs.scrollWidth) + 'px,*';
  resize_tags();
}

function select_tags (tags) {
  var sel = document.getElementById('tagsselect');
  for (var i = 0; i < sel.length; i++) sel[i].selected = false;
  var len = tags.length;
  for (var i = 0; i < len; i++) sel[tags[i]].selected = true;
}

</script></head><body onload='init_tags()' onresize='resize_tags()'>"
		    *coverage-frame-name*)
            (write-coverage-tag-table tags-stream)
            (format tags-stream "</body></html>")))
        (with-open-file (html-stream frame-file
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
          (format html-stream "<html><head><script type='text/javascript'>~%~
function show_in_target_frame (w, elt) {
  var page_top = w.pageYOffset || w.document.documentElement.scrollTop || w.document.body.scrollTop;
  var page_height = w.innerHeight || w.document.documentElement.clientHeight || w.document.body.clientHeight;
  var elt_bottom = elt.offsetHeight;
  for (var p = elt; p && p.tagName != 'BODY'; p = p.offsetParent) elt_bottom += p.offsetTop;
  //  page height may or may not include the scroll bar, scroll a little extra just in case it does.
  var min_top = elt_bottom - (page_height - 20);
  if (page_top <= min_top) w.scrollTo(0, Math.ceil(min_top));
}

function ensure_target_frame (e) {
  var link = (e ? (e.target ? e.target : e.srcElement) : false);
  if (window.frames.length == 1) {
    var new_frame;~0@*~:[
      new_frame = document.createElement('frame');
      new_frame.name = 'T~1@*~a';
      if (link) new_frame.src = link.href;
~;
      new_frame = document.createElement('frameset');
      new_frame.id = 'tagsframeset';
      var tags_frame = document.createElement('frame');
      tags_frame.src = '~0@*~a';
      tags_frame.name = 'tagsframe';
      file_frame = document.createElement('frame');
      file_frame.name = 'T~1@*~a';
      if (link) file_frame.src = link.href;
      new_frame.appendChild(tags_frame);
      new_frame.appendChild(file_frame);
      // new_frame.cols = '20%,*';
    ~]
    var frameset = document.getElementById('topframeset');
    frameset.appendChild(new_frame);
    frameset.rows = '30%,*';

    if (link) show_in_target_frame(window.frames[0], link);
  }
  return true;
}

function send_links_to_frame (w) {
  for (var i = 0; i < w.document.links.length; i++) {
    var link = w.document.links[i];
    link.target = 'T~1@*~a';
    link.onclick = ensure_target_frame;
  }
}

function close_target_frame () {
  if (window.frames.length > 1) {
    var frameset = document.getElementById('topframeset');
    frameset.removeChild(frameset.childNodes[1]);
    frameset.rows = '*';
  }
  return false;
}
</script></head>
<frameset id='topframeset' rows='*'><frame src='~2@*~a' /></frameset></html>"
                  (and tags-file (native-file-namestring tags-file))
                  *coverage-frame-name*
                  (native-file-namestring index-file))))
      (when (null paths)
        (error "No code coverage data available"))
      (setq paths (sort paths #'(lambda (path1 path2)
                                  (let* ((f1 (car path1))
                                         (f2 (car path2)))
                                    (or (string< (directory-namestring f1)
                                                 (directory-namestring f2))
                                        (and (equal (pathname-directory f1)
                                                    (pathname-directory f2))
                                             (string< (file-namestring f1)
                                                      (file-namestring f2))))))))
      (if html
        (with-open-file (html-stream index-file
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
          (if stats-file
            (with-open-file (stats-stream stats-file
                                          :direction :output
                                          :if-exists :supersede
                                          :if-does-not-exist :create)
              (report-coverage-to-streams paths html-stream stats-stream))
            (report-coverage-to-streams paths html-stream nil)))
        (if stats-file
          (with-open-file (stats-stream stats-file
                                        :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
            (report-coverage-to-streams paths nil stats-stream))
          (error "One of :HTML or :STATISTICS must be non-nil"))))
    (values frame-file stats-file)))


(defun report-coverage-to-streams (paths html-stream stats-stream)
  (when html-stream
    (format html-stream "<html><head>~%")
    (write-coverage-styles html-stream)
    (format html-stream "~%</head>~%<body onload='if (top.send_links_to_frame) top.send_links_to_frame(self)'>"))
  (unless paths
    (warn "No coverage data found for any file, producing an empty report. Maybe you forgot to (SETQ CCL::*COMPILE-CODE-COVERAGE* T) before compiling?")
    (when html-stream (format html-stream "<h3>No code coverage data found.</h3>~%"))
    (when stats-stream (format stats-stream "No code coverage data found.~%"))
    (return-from report-coverage-to-streams))
  (when html-stream (format html-stream "<table class='summary'>"))
  (coverage-stats-head html-stream stats-stream t)
  (loop for prev = nil then src-name
        for (src-name report-name . coverage) in paths
        for even = nil then (not even)
        do (when (or (null prev)
                     (not (equal (pathname-directory (pathname src-name))
                                 (pathname-directory (pathname prev)))))
             (let ((dir (namestring (make-pathname :name nil :type nil :defaults src-name))))
               (when html-stream (format html-stream "<tr class='subheading'><td colspan='17'>~A</td></tr>~%" dir))
               (when stats-stream (format stats-stream "~a~%" dir))))
        do (coverage-stats-data html-stream stats-stream coverage even report-name src-name))
  (when html-stream (format html-stream "</table></body></html>")))

(defun style-for-coverage (coverage)
  (case coverage
    ((full) $totally-covered-style)
    ((nil) $not-executed-style)
    (t $partially-covered-style)))
  
(defun precompute-source-coverage (coverage)
  ;; linearize emitted notes with children preceding parents, and mark up fully covered ones.
  ;; This assumes code notes are never individually reset, so once something is fully
  ;; covered, it stays fully covered, so no need to reinit the setting, just update.
  (let ((subnotes *code-note-subnotes*)
        (vector *emitted-code-notes*)
        (index-hash *code-note-index*))
    (iterate descend ((note nil))
      (let ((full-p (and note (code-note-code-coverage note))))
        (loop for subnote in (gethash note subnotes)
              do (unless (descend subnote) (setq full-p nil))
              do (setf (gethash subnote index-hash) (vector-push-extend subnote vector)))
        (when full-p         ;; return true if full, nil if not.
          (setf (code-note-code-coverage note) 'full)))))
  ;; Find all source notes
  ;; Note that can't compute a source hierarchy because the reader flattens the backpointers
  ;; so each subnote points directly to the toplevel note.
  (labels ((subnotep (a b)
             (or (eq a b) (and a (subnotep (code-note-parent-note a) b))))
           (register (source emitted-notes)
             (assert emitted-notes)
             (let ((prior-notes (gethash source *source-code-notes*)))
               (if prior-notes
                 ;; In some cases, a single source form may be claimed by multiple code notes,
                 (setq emitted-notes
                       (nconc
                        (setq emitted-notes
                              (remove-if (lambda (new)
                                           (some (lambda (old) (subnotep new old)) prior-notes))
                                         emitted-notes))
                        (if emitted-notes
                          (remove-if (lambda (old)
                                       (some (lambda (new) (subnotep old new)) emitted-notes))
                                     prior-notes)
                          prior-notes)))
                 ;; Else this is the first time, record it
                 (vector-push-extend source *covered-source-notes*)))
             (setf (gethash source *source-code-notes*) emitted-notes)))
    (loop for note across *emitted-code-notes*
          as source = (code-note-source-note note)
          when source do (register source (list note))
            ;; want to look at all notes, even unemitted, so can get all source forms
            do (loop while (and (setq note (code-note-parent-note note))
                                (not (emitted-code-note-p note)))
                     when (setq source (code-note-source-note note))
                       do (register source (code-note-subnotes note))))
    (setf *covered-source-notes*
          (sort *covered-source-notes* #'< :key #'source-note-start-pos)) ;; this puts parents before children
    (loop for source across *covered-source-notes* as index upfrom 0
          do (setf (gethash source *source-note-index*) index)))
  (assert (eql (length *covered-source-notes*) (hash-table-count *source-code-notes*)))
  coverage)

(defun file-coverage-html-queue (coverage)
  (declare (ignore coverage)) ;; turns out everything we need is already in global variables
  ;; Collect top-level sources.  *covered-source-notes* is sorted by start address.
  (let ((queue (loop with vector = *covered-source-notes* with len = (length vector)
                     for start = 0 then end while (< start len)
                     as sn = (aref vector start)
                     as end = (loop with limit = (source-note-end-pos sn)
                                    for i from (1+ start) below len
                                    until (<= limit (source-note-start-pos (aref vector i)))
                                    finally (return i))
                     collect (list* end nil (source-note-end-pos sn)))));; (end-index acodes . end-pos)
    ;; Find all acode strings, assign them to appropriate toplevel source form, and collect
    ;; all code notes for each acode.
    (loop for note across *emitted-code-notes*
          when (code-note-acode-range note)
            do (let* ((source (nearest-source-note note))
                      (pos (source-note-start-pos source))
                      (cell (loop for cell in queue while (<= (cddr cell) pos) finally (return cell)))
                      (acode (%function-acode-string (code-note-function note)))
                      (acell (or (assq acode (cadr cell))
                                 (car (push (list* acode nil 0) (cadr cell))))));; (acode notes . src-pos)
                 (assert (and cell acode))
                 (setf (cddr acell) (min (cddr acell) pos));; earliest known source for this acode
                 (push note (cadr acell))))
    ;; Sort acode by source position within source form, sort notes by position within the acode,
    ;; get rid of the end-pos/src-pos fields since no longer needed.
    (loop for cell in queue
          do (setf (cdr cell) (sort (cadr cell) #'< :key #'cddr));; (end-index . acodes)
          do (loop for acell in (cdr cell)
                   do (setf (cdr acell) (sort (cadr acell) #'< :key #'code-note-acode-start-pos)))) ; (acode . notes)
    queue))


(defun function-outermost-entry-source (fn)
  ;; Find the outermost source form containing the fn.
  (loop with sn = nil
        for n = (function-entry-code-note fn) then (code-note-parent-note n)
        do (when (null n) (return nil))
        do (when (setq sn (code-note-source-note n))
             (loop for s = (source-note-source sn) while (source-note-p s)
                   do (setq sn s))
             (return sn))))


(defun report-file-coverage (index-file coverage directory html-name external-format)
  (with-open-file (js-stream (make-pathname :name html-name :type "js" :defaults directory)
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
    (write-coverage-js-file js-stream coverage))
  (with-open-file (html-stream (make-pathname :name html-name :type "html" :defaults directory)
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
    (write-coverage-html-file index-file html-name html-stream coverage external-format)))

(defun write-char-to-html (ch stream)
  (if (or (alphanumericp ch) (find ch "()+-:* ")) ;; common and safe
    (write-char ch stream)
    (format stream "&#~D;" (char-code ch))))


(defun write-coverage-tag-table (html-stream)
  (let* ((tags *coverage-tags*)
         (named-p (not (fixnump tags)))
         (count (if named-p (length tags) tags)))
    (format html-stream "~&<form width='*'><select multiple size='~d' width='*' id='tagsselect' onchange='tags_changed();'>~%" count)
    (loop for i from 0 below count
          do (format html-stream "<option value='~d'>" i)
          do (if named-p
               (let* ((tag (aref tags i))
                      (name (typecase tag
                              (string tag)
                              (symbol (symbol-name tag))
                              (t (princ-to-string tag)))))
                 (loop for ch across name do (write-char-to-html ch html-stream)))
               (format html-stream "[~d]" i))
          do (format html-stream "</option>~%"))
    (format html-stream "</select></form>~%")))

(defun write-coverage-html-file (index-file html-name html-stream coverage source-external-format)
  (let ((*print-case* :downcase))

    (format html-stream "<html><head>")
    (write-coverage-styles html-stream)
    (format html-stream "<script src='~a.js'></script>~%" html-name)
    (format html-stream "</head><body onload='init_file()'>")

    (format html-stream "<h3><a id='backlink' href=~s>Coverage report:</a> ~a <br />~%</h3>~%"
            (native-file-namestring index-file)
            (file-coverage-file coverage))
    (format html-stream "<table class='summary'>")
    (file-coverage-stats-html html-stream)
    (format html-stream "</table>")

    ;;(format html-stream "~2%<a href='javascript:DEBUG_OUT(CodeParents)'>Doit</a><div id='foo'>Debug output here</div>")

    (format html-stream "<div class='key'><b>Key</b><br />~%")
    (format html-stream "<div class='st~a'>Fully covered - every single instruction executed</div>" $totally-covered-style)
    (format html-stream "<div class='st~a'>Partly covered - entered but some subforms not executed</div>" $partially-covered-style)
    (format html-stream "<div class='st~a'>Never entered - not a single instruction executed</div>" $not-executed-style)
    (format html-stream "<div class='stsource'>Uninstrumented - a form whose coverage was not measured</div>")
    (format html-stream "</div><p></p>~%")

    (output-spanned-html html-stream coverage source-external-format)

    (format html-stream "</body></html>")))

#|
var COV = ['unknown', 'not', 'all', 'some'];
function DEBUG_OUT(text) {
  var msg = document.getElementById('foo');
  msg.innerHTML = msg.innerHTML + '<br />' + text;
}
|#

;; This goes in each file.
(defparameter $coverage-javascript "

function init_file () {
  if (top.close_target_frame) {
    var backlink = document.getElementById('backlink');
    backlink.innerHTML = '[Close]<p>';
    backlink.onclick = top.close_target_frame;
  }
  colorize (true);
}

function tags_intersect (tags1, tags2) {   // tags2 = true means all tags.
  var ntags = tags1.length - 1;
  if (tags2 === true)
    return (ntags > 0);
  for (var i = 0; i < ntags; i++) {
    var tag1 = tags1[i];
    for (var j = 0; j < tags2.length; j++)
      if (tag1 == tags2[j]) return true;
  }
  return false;
}

function is_member (elt, vec) {
  for (var i = 0; i < vec.length; i++) {
    if (vec[i] == elt) return true;
  }
  return false;
}

function set_stats_with_pct(name, count, total) {
  var pct;

  if (total > 0) {
    var pct = (count * 100) / total;
    pct = pct.toFixed(1) + '&#37;';
  }
  else {
    pct = '--';
  }
  
  document.getElementById(name).innerHTML = count;

  document.getElementById(name + 'Pct').innerHTML =  pct;
}

function colorize (tags_to_show) {
  var style;

  // Compute acode coverage and colorize acode
  var total = (CodeTags ? CodeTags.length : CodeCoverage.length) - 1;
  var num_entered = 0;
  var coverage = new Array(total);

  for (var cn = 0; cn < total; cn++) {
    var covered = (CodeTags ? tags_intersect(CodeTags[cn], tags_to_show) : CodeCoverage[cn]);
    style = (covered ? ALL_COVERED : NOT_COVERED);

    var sub_style = coverage[cn];
    if (sub_style && (style != sub_style)) style = PARTLY_COVERED;

    coverage[cn] = style; // save for source coloring use below
    if (style != NOT_COVERED) num_entered++;
    var parent = CodeParents[cn];
    if (parent) {
      var sibs_style = coverage[parent];
      if (sibs_style != style) coverage[parent] = (!sibs_style ? style : PARTLY_COVERED);
    }

  var elt = document.getElementById('f~dc' + cn);  // some notes don't have a matched up source.
  if (elt) elt.className = 'st' + style;
  }


  document.getElementById('acodeTotal').innerHTML = total;
  set_stats_with_pct('acodeCovered', num_entered, total);

  // Count unreached branches (aka maximal unentered forms)
  var total = coverage.length;
  var num_branches = 0;
  var parent;
  for (var cn = 0; cn < total; cn++) {
    if ((coverage[cn] == NOT_COVERED) && // not covered
        (parent = CodeParents[cn]) &&  // has a parent
        (coverage[parent] != NOT_COVERED) &&  // that's covered
        (!is_member(cn, FunctionNotes))) // and not an entry note
      num_branches++;
  }

  document.getElementById('branchUnreached').innerHTML = num_branches;


  // Colorize Source
  var total = (SourceCodeNotes ? SourceCodeNotes.length : SourceCoverage.length) - 1;
  var num_all = 0, num_partly = 0;

  for (var sn = 0; sn < total; sn++) {
    if (SourceCodeNotes) {
      var notes = SourceCodeNotes[sn];
      for (var i = 0, style = NO_DATA; i < (notes.length - 1); i++) {
        var note_style = coverage[notes[i]];
        if (style != note_style) style = (style == NO_DATA ? note_style : PARTLY_COVERED);
      }
    }
    else {
      style = SourceCoverage[sn];
    }

    switch (style) {
      case ALL_COVERED: num_all++; break;
      case PARTLY_COVERED: num_partly++; break;
    }

   document.getElementById('f~:*~ds' + sn).className = 'st' + style;

  }
  document.getElementById('srcTotal').innerHTML = total;
  set_stats_with_pct('srcEntered', num_all + num_partly, total);
  set_stats_with_pct('srcCovered', num_all, total);

  var total = FunctionNotes.length - 1;
  var num_all = 0, num_partly = 0, num_not = 0;

  for (var i = 0; i < total; i++) {
    var cn = FunctionNotes[i];
    switch (coverage[FunctionNotes[i]]) {
      case ALL_COVERED: num_all++; break;
      case PARTLY_COVERED: num_partly++; break;
      case NOT_COVERED: num_not++; break;
    }
  }

  document.getElementById('fnTotal').innerHTML = total;
  set_stats_with_pct('fnCovered', num_all, total);
  set_stats_with_pct('fnPartly', num_partly, total);
  set_stats_with_pct('fnUnentered', num_not, total);


}

function show_tags (sn) {
  tags_frame = top.frames.tagsframe;
  if (tags_frame && tags_frame.select_tags) {
    var tags = new Array();
    var outer_notes = SourceCodeNotes[sn].slice(0);
    var total = CodeTags.length - 1;
    for (cn = total - 1; cn >= 0; cn--) {
      if (is_member(CodeParents[cn], outer_notes)) {
         outer_notes.push(cn);
         var new_tags = CodeTags[cn];
         var n = new_tags.length - 1;
         for (i = 0; i < n; i++) {
           var tag = new_tags[i];
           if (!is_member(tag, tags)) tags.push(tag);
         }
      }
    }
    tags_frame.select_tags(tags);
  }
}

")


(defmacro write-js-array (js-stream-expr var-expr data-expr writer)
  (let ((js-stream (gensym))
        (var (gensym))
        (data (gensym)))
    `(let ((,js-stream ,js-stream-expr)
           (,var ,var-expr)
           (,data ,data-expr))
       (when ,var (format ,js-stream "~2&var ~a = " ,var))
       (format ,js-stream "[")
       (loop with len = (and (vectorp ,data) (length ,data))
             for index upfrom 0
             while (if len (< index len) ,data)
             as note = (if len (aref ,data index) (pop ,data))
             do (funcall ,writer ,js-stream note)
             do (write-string (if (eql 49 (mod index 50)) #.(format nil ",~% ") ", ") ,js-stream))
       ;; Add an element at the end because otherwise get the wrong length if last element is empty
       (format ,js-stream "'end']")
       (when ,var (format ,js-stream ";~%")))))

;; output with a line break every 100 entries
(defun write-coverage-js-file (js-stream coverage)
  (flet ((write-code-parent (js-stream cn)
           (let* ((parent (code-note-emitted-parent cn)))
             (when parent
               (format js-stream "~a" (code-note-index parent)))))
         (write-function-note (js-stream fn)
           (format js-stream "~a" (code-note-index (function-entry-code-note fn))))
         (write-source-coverage (js-stream sn)
           (format js-stream "~a" (style-for-coverage (source-coverage sn))))
         (write-code-coverage (js-stream cn)
           (when (code-note-code-coverage cn) (format js-stream "1")))
         (write-source-notes (js-stream sn)
           (write-js-array js-stream nil (source-code-notes sn)
                           (lambda (js-stream cn) (format js-stream "~a" (code-note-index cn)))))
         (write-code-tags (js-stream cn)
           (write-js-array js-stream nil (code-note-tags cn)
                           (lambda (js-stream tag) (format js-stream "~a" tag)))))

    (format js-stream "~&var NO_DATA = ~d, NOT_COVERED = ~d, ALL_COVERED = ~d, PARTLY_COVERED = ~d;~2%"
            $not-executed-style $not-executed-style $totally-covered-style $partially-covered-style)
    (write-js-array js-stream "CodeParents" *emitted-code-notes* #'write-code-parent)
    (write-js-array js-stream "FunctionNotes" (file-coverage-functions coverage) #'write-function-note)
    (cond (*coverage-tags*
           (write-js-array js-stream "CodeTags" *emitted-code-notes* #'write-code-tags)
           (write-js-array js-stream "SourceCodeNotes" *covered-source-notes* #'write-source-notes)
           (format js-stream "~&var CodeCoverage;")
           (format js-stream "~&var SourceCoverage;"))
          (t
           (format js-stream "~&var CodeTags;")
           (format js-stream "~&var SourceCodeNotes;")
           (write-js-array js-stream "CodeCoverage" *emitted-code-notes* #'write-code-coverage)
           (write-js-array js-stream "SourceCoverage" *covered-source-notes* #'write-source-coverage)))
    (format js-stream $coverage-javascript (file-coverage-index coverage))
    (terpri js-stream)))

(defstruct coverage-html-state
  input
  output
  prefix
  (file-pos 0)
  (line-no 0)
  (column 0))

(defun coverage-html-start-line (s)
  (let ((line-no (coverage-html-state-line-no s))
        (output (coverage-html-state-output s)))
    (when line-no
      (setf (coverage-html-state-line-no s) (incf line-no))
      (format output "<span class='line'>~a</span>" line-no))
    (write-char #\space output)))

(defun coverage-html-copy-to (s end &optional end-at-newline-p whitespace-only-p)
  (let ((input (coverage-html-state-input s))
        (output (coverage-html-state-output s))
        (file-pos (coverage-html-state-file-pos s)))
    (assert (<= file-pos end))
    (loop until (eql file-pos end)
          as ch = (read-char input)
          do (when (and whitespace-only-p (not (whitespacep ch)))
               (unread-char ch input)
               (return))
             ;; Source note positions are file positions, not character positions, but assume
             ;; non-control ascii chars are 1 byte so don't have to call stream-position all the time.
          do (setq file-pos (if (< 31 (char-code ch) 127)
                              (1+ file-pos)
                              (let ((newpos (stream-position input)))
                                (assert (<= newpos end))
                                newpos)))
          do (when (eql (coverage-html-state-column s) 0) (coverage-html-start-line s))
          do (case ch
               (#\newline
                  (write-char #\Newline output)
                  (setf (coverage-html-state-column s) 0)
                  (when end-at-newline-p (return)))
               (#\tab
                  (let ((count (- 8 (mod (coverage-html-state-column s) 8))))
                    (write-string "        " output :end count)
                    (incf (coverage-html-state-column s) count)))
               (t
                  (incf (coverage-html-state-column s))
                  (write-char-to-html ch output))))
    (assert (eql file-pos (stream-position input)))
    (setf (coverage-html-state-file-pos s) file-pos)))

(defun output-coverage-html-acode (s note-queue)
  (let* ((output (coverage-html-state-output s))
         (input (coverage-html-state-input s))
         (prefix (coverage-html-state-prefix s))
         (end (stream-length input)))
    (when (< (coverage-html-state-file-pos s) end)
      (iterate output-subnotes ((limit end))
        (loop while (and note-queue (<= (code-note-acode-end-pos (car note-queue)) limit))
              do (let ((note (pop note-queue)))
                   (coverage-html-copy-to s (code-note-acode-start-pos note))
                   ;; skip leading whitespace -- this is necessary for acode, else looks weird.
                   (coverage-html-copy-to s (code-note-acode-end-pos note) nil t)
                   (format output "<span id='~a~d'>" prefix (code-note-index note))
                   (output-subnotes (code-note-acode-end-pos note))
                   (format output "</span>")))
        (coverage-html-copy-to s limit)))))

(defun output-coverage-html-source (s start end)
  (let* ((output (coverage-html-state-output s))
         (input (coverage-html-state-input s))
         (prefix (coverage-html-state-prefix s))
         (vector *covered-source-notes*)
         (len (length vector))
         (outer-note (and (< start end) (aref vector start)))
         (nextpos (if (< end len) (source-note-start-pos (aref vector end)) (stream-length input))))
    (when (< (coverage-html-state-file-pos s) nextpos)
      (format output "<div class='source'><code>")
      (when outer-note
        ;; The first time through this will just do the first note, because that's all that fits.
        (iterate output-subnotes ((outer-note outer-note))
          (loop with outer-end = (source-note-end-pos outer-note)
                as note = (and (< start end) (aref vector start))
                while (and note (<= (source-note-end-pos note) outer-end))
                do (progn
                     (coverage-html-copy-to s (source-note-start-pos note))
                     (format output "<span id='~a~d'>" prefix start)
                     (incf start)
                     (output-subnotes note)
                     (format output "</span>"))
                finally (coverage-html-copy-to s outer-end))))
      ;; Copy the rest of the last line, or to end if called without a note.
      (coverage-html-copy-to s nextpos outer-note)
      (format output "</code></div>~%"))))

(defun output-spanned-html (html-stream coverage external-format)
  (with-open-file (source-stream (file-coverage-file coverage) :external-format external-format)
    (let* ((queue (file-coverage-html-queue coverage))
           (prefix (format nil "f~d" (file-coverage-index coverage)))
           (s (make-coverage-html-state :input source-stream
                                        :output html-stream
                                        :prefix (%str-cat prefix "s"))))
      (loop 
        for start = 0 then end as (end . acodes) in queue
        do (output-coverage-html-source s start end)
        do (format html-stream "<a href=javascript:swap('~at~d')><span class='toggle' id='p~2:*~at~d'>Show expansion</span></a>~%" prefix start)
        do (when *coverage-tags*
             (format html-stream "&nbsp;&nbsp;&nbsp;<a href=javascript:show_tags(~d)><span class='toggle'>Show tags</span></a>~%" start))
        do (format html-stream "<div class='acode' id='a~at~d'><code>" prefix start)
        do (loop for (acode . notes) in acodes
                 do (with-input-from-vector (astream acode :external-format :utf-8)
                      (let ((cs (make-coverage-html-state :input astream
                                                          :output html-stream
                                                          :prefix (%str-cat prefix "c")
                                                          :line-no nil)))
                        (output-coverage-html-acode cs notes)
                        (fresh-line html-stream))))
        do (format html-stream "</code></div><hr/>~%")
           ;; output the rest of file, no notes.
        finally (output-coverage-html-source s start start)))))

(defun coverage-stats-head (html-stream stats-stream include-source-p)
  (when html-stream
    (format html-stream "<tr class='head-row'>")
    (when include-source-p (format html-stream "<td></td>"))
    (format html-stream "<td class='main-head' colspan='5'>Expressions</td>")
    (format html-stream "<td class='main-head' colspan='1'>Branches</td>")
    (format html-stream "<td class='main-head' colspan='3'>Code Forms</td>")
    (format html-stream "<td class='main-head' colspan='7'>Functions</td></tr>")
    (format html-stream "<tr class='head-row'>")
    (let ((fields '(;; Expressions
                    "Total" "Entered" "% entered" "Fully covered" "% fully covered"
                    ;; Branches
                    "total unreached"
                    ;; Code forms
                    "Total" "Covered" "% covered"
                    ;; Functions
                    "Total" "Fully covered" "% fully covered" "Partly covered" "% partly covered" "Not entered" "% not entered")))
      (when include-source-p (push "Source file" fields))
      (format html-stream "~{<td width='60px'>~A</td>~}" fields))
    (format html-stream "</tr>"))
  (when stats-stream
    (format stats-stream "~{~a~^,~}"
            `("Source file"
              "Expressions Total" "Expressions Entered" "% Expressions Entered"
              "Unreached Branches"
              "Code Forms Total" "Code Forms Covered" "% Code Forms Covered"
              "Functions Total" "Functions Fully Covered" "% Functions Fully Covered"
              "Functions Partly Covered" "% Functions Partly Covered"
              "Functions Not Entered" "% Functions Not Entered"))))

(defun file-coverage-stats-html (html-stream)
  (format html-stream "<table class='summary'>")
  (coverage-stats-head html-stream nil nil)
  (format html-stream "<tr class='odd'>")
  (format html-stream "~{<td id='~a'></td>~}"
          '("srcTotal" "srcEntered" "srcEnteredPct" "srcCovered" "srcCoveredPct"
            "branchUnreached"
            "acodeTotal" "acodeCovered" "acodeCoveredPct"
            "fnTotal" "fnCovered" "fnCoveredPct" "fnPartly" "fnPartlyPct" "fnUnentered" "fnUnenteredPct"))
  (format html-stream "</table>"))
  
(defun coverage-stats-data (html-stream stats-stream coverage evenp report-name src-name)
  (when html-stream
    (format html-stream "<tr class='~:[odd~;even~]'>" evenp)
    (format html-stream "<td class='text-cell'><a href='~a.html'>~a</a></td>" report-name src-name))
  (when stats-stream
    (format stats-stream "~a," (file-coverage-file coverage)))

  (let* ((stats (file-coverage-statistics coverage))
         (total (coverage-expressions-total stats))
         (entered (coverage-expressions-entered stats))
         (covered (coverage-expressions-covered stats))
         (exp-counts (list total
                           entered (if (> total 0) (* 100.0d0 (/ entered total)) '--)
                           covered (if (> total 0) (* 100.0d0 (/ covered total)) '--))))
    (when html-stream
      (format html-stream "~{<td>~:[-~;~:*~a~]</td><td>~:[-~;~:*~a~]</td><td>~:[-~;~:*~5,1f%~]</td><td>~:[-~;~:*~a~]</td><td>~:[-~;~:*~5,1f%~]</td>~}" exp-counts))
    (when stats-stream
      (format stats-stream "~{~:[~;~:*~a~],~:[~;~:*~a~],~:[~;~:*~5,1f%~],~:[~;~:*~a~],~:[~;~:*~5,1f%~],~}" exp-counts)))

  (let ((count (coverage-unreached-branches (file-coverage-statistics coverage))))
    (when html-stream
      (format html-stream "<td>~:[-~;~:*~a~]</td>" count))
    (when stats-stream
      (format stats-stream "~:[~;~:*~a~]," count)))

  (let* ((stats (file-coverage-statistics coverage))
         (total (coverage-code-forms-total stats))
         (covered (coverage-code-forms-covered stats))
         (exp-counts (list total covered (if (> total 0) (* 100.0d0 (/ covered total)) '--))))
    (when html-stream
      (format html-stream "~{<td>~:[-~;~:*~a~]</td><td>~:[-~;~:*~a~]</td><td>~:[-~;~:*~5,1f%~]</td>~}" exp-counts))
    (when stats-stream
      (format stats-stream "~{~:[~;~:*~a~],~:[~;~:*~a~],~:[~;~:*~5,1f%~],~}" exp-counts)))

  (let* ((stats (file-coverage-statistics coverage))
         (total (coverage-functions-total stats))
         (fully (coverage-functions-fully-covered stats))
         (partly (coverage-functions-partly-covered stats))
         (never (coverage-functions-not-entered stats))
         (counts (list fully
                       (if (> total 0) (* 100.0 (/ fully total)) '--)
                       partly
                       (if (> total 0) (* 100.0 (/ partly total)) '--)
                       never
                       (if (> total 0) (* 100.0 (/ never total)) '--))))
    (when html-stream
      (format html-stream "<td>~:[-~;~:*~a~]</td>~{<td>~:[-~;~:*~a~]</td><td>~:[-~;~:*~5,1f%~]</td>~}</tr>" total counts))
    (when stats-stream
      (format stats-stream "~:[~;~:*~a~],~{~:[~;~:*~a~],~:[-~;~:*~5,1f%~]~^,~}~%" total counts))))

(defun count-covered-aexps (coverage)
  (let ((covered 0) (total 0)
        (entry-full 0) (entry-part 0) (entry-never 0) (entry-total 0))
    (loop for note across *emitted-code-notes*
          do (incf total)
          do (when (code-note-code-coverage note)
               (incf covered))
          do (when (entry-code-note-p note)
               (incf entry-total)
               (case (code-note-code-coverage note)
                 ((full) (incf entry-full))
                 ((nil) (incf entry-never))
                 (t (incf entry-part)))))
    (let ((stats (file-coverage-statistics coverage)))
      (setf (coverage-code-forms-total stats) total)
      (setf (coverage-code-forms-covered stats) covered)
      (setf (coverage-functions-total stats) entry-total)
      (setf (coverage-functions-fully-covered stats) entry-full)
      (setf (coverage-functions-partly-covered stats) entry-part)
      (setf (coverage-functions-not-entered stats) entry-never))))


(defun count-covered-sexps (coverage)
  ;; Count the number of source expressions that have been entered or covered
  (let ((entered 0) (covered 0) (total (length *covered-source-notes*)))
    (loop for source across *covered-source-notes* as cover = (source-coverage source)
          do (when cover
               (incf entered)
               (when (eq cover 'full) (incf covered))))
    (let ((stats (file-coverage-statistics coverage)))
      (setf (coverage-expressions-total stats) total)
      (setf (coverage-expressions-entered stats) entered)
      (setf (coverage-expressions-covered stats) covered))))

(defun count-unreached-branches (coverage)
  ;; Count the number of maximal unentered code forms, i.e. unentered code forms
  ;; whose parent was entered.
  (let ((count (loop for note across *emitted-code-notes*
                     count (and (null (code-note-code-coverage note));; uncovered
                                (not (entry-code-note-p note));; not entry note
                                (setq note (code-note-emitted-parent note));; has a parent
                                (code-note-code-coverage note)))));; that's covered
    (let ((stats (file-coverage-statistics coverage)))
      (setf (coverage-unreached-branches stats) count))))

(defun write-coverage-styles (html-stream)
  (format html-stream "<style type='text/css'>
*.st~a { background-color: #ffaaaa }
*.st~a { background-color: #aaffaa }
*.st~a { background-color: #44dd44 }
*.stsource { background-color: #eeeeee; }
*.key { margin: 20px; width: 88ex }
*.source { width: 120ex; background-color: #eeeeee; padding-left: 5px;
             /* border-style: solid none none none; border-width: 1px;
             border-color: #dddddd */
             white-space: pre; }

*.acode { border-left: 1px dashed #c0c0c0;
         margin-top: 1ex;
         margin-left: 6ex;
         margin-bottom: 2ex;
         white-space: pre;
         display: none; }

*.line { color: #666666; float: left; width: 6ex; text-align: right; margin-right: 1ex; }

*.toggle { font-size: small; }

table.summary tr.head-row { background-color: #aaaaff }
table.summary tr td.text-cell { text-align: left }
table.summary tr td.main-head { text-align: center }
table.summary tr td { text-align: right }
table.summary tr.even { background-color: #eeeeff }
table.summary tr.subheading { background-color: #aaaaff}
table.summary tr.subheading td { text-align: left; font-weight: bold; padding-left: 5ex; }

</style>

<script type='text/javascript'>
function swap (id) {
  var acode = document.getElementById('a' + id);
  var prompt = document.getElementById('p' + id);
  if (acode.style.display == 'block') {
      acode.style.display = 'none';
      prompt.innerHTML = 'Show expansion';
  } else {
    acode.style.display = 'block';
    prompt.innerHTML = 'Hide expansion';
  }
}
</script>
"
          $not-executed-style
          $partially-covered-style
          $totally-covered-style
          ))
