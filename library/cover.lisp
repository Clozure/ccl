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

(export '(*compile-code-coverage*
          report-coverage
          reset-coverage
          clear-coverage
          save-coverage-in-file
          restore-coverage-from-file

          save-coverage
          restore-coverage
          combine-coverage
          read-coverage-from-file
          write-coverage-to-file

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

          without-compiling-code-coverage))

(defconstant $no-style 0)
(defconstant $not-executed-style 1)
(defconstant $totally-covered-style 2)
(defconstant $partially-covered-style 3)

(defparameter *file-coverage* ())
(defparameter *coverage-subnotes* (make-hash-table :test #'eq))
(defparameter *emitted-code-notes* (make-hash-table :test #'eq))
(defparameter *entry-code-notes* (make-hash-table :test #'eq))

(defstruct (coverage-state (:conc-name "%COVERAGE-STATE-"))
  alist)

;; Wrapper in case we ever want to do dwim on raw alists
(defun coverage-state-alist (coverage)
  (etypecase coverage
    (coverage-state (%coverage-state-alist coverage))))


(defun file-coverage-file (entry)
  (car entry))

(defun file-coverage-functions (entry)
  (cadr entry))

(defun file-coverage-toplevel-functions (entry)
  (cddr entry))

(defun coverage-subnotes (note) ;; reversed parent chain
  (gethash note *coverage-subnotes*))

(defun emitted-code-note-p (note)
  (gethash note *emitted-code-notes*))

(defun entry-code-note-p (note)
  (gethash note *entry-code-notes*))

(defun map-function-coverage (lfun fn &optional refs)
  (let ((refs (cons lfun refs)))
    (declare (dynamic-extent refs))
    (lfunloop for imm in lfun
	      when (code-note-p imm)
	      do (funcall fn imm)
	      when (and (functionp imm)
			(not (memq imm refs)))
	      do (map-function-coverage imm fn refs))))

(defun get-function-coverage (fn refs)
  (let ((entry (function-entry-code-note fn))
	(refs (cons fn refs))
        (source (function-source-form-note fn)))
    (declare (dynamic-extent refs))
    (when entry
      (assert (eq fn (gethash entry *entry-code-notes* fn)))
      (setf (gethash entry *entry-code-notes*) fn))
    (nconc
     (and entry (list fn))
     (lfunloop for imm in fn
       when (code-note-p imm)
       do (setf (gethash imm *emitted-code-notes*) t)
       when (and (functionp imm)
                 (not (memq imm refs))
                 ;; Make sure this fn is in the source we're currently looking at.
                 ;; It might not be, if it is referenced via (load-time-value (foo))
                 ;; where (foo) returns an lfun from some different source entirely.
                 ;; CL-PPCRE does that.
                 (or (null source)
                     (eq source (function-source-form-note imm))))
       nconc (get-function-coverage imm refs)))))

(defun code-covered-info.file (data) (and (consp data) (car data)))
(defun code-covered-info.fns (data) (and (consp data) (if (consp (cdr data)) (cadr data) (cdr data))))
(defun code-covered-info.ef (data) (and (consp data) (consp (cdr data)) (caddr data)))
(defun code-covered-info.id (data) (and (consp data) (consp (cdr data)) (cadddr data)))

(defun code-covered-info-with-fns (data new-fns)
  (assert (consp data))
  (if (consp (cdr data))
    (cons (car data) new-fns)
    (let ((new (copy-list data)))
      (setf (cadr new) new-fns)
      new)))

(defun get-coverage ()
  (setq *file-coverage* nil)
  (clrhash *coverage-subnotes*)
  (clrhash *emitted-code-notes*)
  (clrhash *entry-code-notes*)
  (loop for data in *code-covered-functions*
	do (let* ((file (code-covered-info.file data))
                  (toplevel-functions (code-covered-info.fns data)))
             (when file
               (let* ((all-functions (delete-duplicates
                                      ;; Duplicates are possible if you have multiple instances of
                                      ;; (load-time-value (foo)) where (foo) returns an lfun.
                                      ;; CL-PPCRE does that.
                                      (loop for fn across toplevel-functions
                                            nconc (get-function-coverage fn nil))))
                      (coverage (list* file all-functions toplevel-functions)))
                 (push coverage *file-coverage*)))))
  ;; Now get subnotes, including un-emitted ones.
  (loop for note being the hash-key of *emitted-code-notes*
        do (loop for n = note then parent as parent = (code-note-parent-note n)
                 while parent
                 do (pushnew n (gethash parent *coverage-subnotes*))
                 until (emitted-code-note-p parent))))

(defun file-coverage-acode-queue (coverage)
  (loop with hash = (make-hash-table :test #'eq :shared nil)
        for fn in (file-coverage-functions coverage)
        as acode = (%function-acode-string fn)
        as entry = (function-entry-code-note fn)
        as sn = (entry-note-unambiguous-source entry)
        as toplevel-sn = (function-source-form-note fn)
        do (when sn
             (assert toplevel-sn)
             (let* ((pos (source-note-end-pos sn))
                    (cell (assq acode (gethash toplevel-sn hash))))
               (if cell
                 (setf (cdr cell) (max (cdr cell) pos))
                 (push (cons acode pos) (gethash toplevel-sn hash)))))
        finally (return (sort (loop for sn being the hash-key of hash using (hash-value alist)
                                    collect (cons (source-note-end-pos sn)
                                                  (mapcar #'car (sort alist #'< :key #'cdr))))
                              #'< :key #'car))))

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
		     for sub in (coverage-subnotes note) as i upfrom 1
		     do (show sub subindent (format nil "~a~d." label i))))))
    (show note 0 "")))

(defun assoc-by-filename (path alist)
  (let* ((true-path (probe-file path)))
    (find-if #'(lambda (data)
                 (or (equalp (car data) path)
                     (and true-path (equalp (probe-file (car data)) true-path))))
             alist)))

(defun covered-functions-for-file (path)
  (code-covered-info.fns (assoc-by-filename path *code-covered-functions*)))

(defun clear-coverage ()
  "Clear all files from the coverage database. The files will be re-entered
into the database when the FASL files (produced by compiling with
CCL:*COMPILE-CODE-COVERAGE* set to true) are loaded again into the
image."
  (setq *code-covered-functions* nil))

(defun reset-function-coverage (lfun)
  (map-function-coverage lfun #'(lambda (note)
                                  (setf (code-note-code-coverage note) nil))))

(defun reset-coverage ()
  "Reset all coverage data back to the `Not executed` state."
  (loop for data in *code-covered-functions*
        do (typecase data
             (cons
		(loop for fn across (code-covered-info.fns data)
		      do (reset-function-coverage fn)))
             (function (reset-function-coverage data)))))

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
  (let ((refs (cons fn refs)))
    (declare (dynamic-extent refs))
    (cons (function-covered-name fn)
          (lfunloop for imm in fn as i upfrom 0
                    when (and (code-note-p imm)
                              (code-note-code-coverage imm))
                    collect i into list
                    when (and (functionp imm) (not (memq imm refs)))
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
         (saved-name (car saved-fn-data))
         (saved-imms (cdr saved-fn-data))
         (nimms (length saved-imms))
         (n 0))
    (declare (dynamic-extent refs))
    (unless (equalp saved-name (function-covered-name fn))
      (coverage-mismatch "had function ~s now have ~s" saved-name fn))
    (lfunloop for imm in fn as i upfrom 0
              when (code-note-p imm)
              do (let* ((next (and (< n nimms) (aref saved-imms n))))
                   (when (if (consp next) (<= (car next) i) (and next (< next i)))
                     (coverage-mismatch "in ~s" fn))
                   (when (setf (code-note-code-coverage imm)
                               (and (eql next i) 'restored))
                     (incf n)))
              when (and (functionp imm) (not (memq imm refs)))
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


(defun save-coverage ()
  "Returns a snapshot of the current coverage state"
  (make-coverage-state
   :alist (loop for data in *code-covered-functions*
                when (consp data)
                  collect (code-covered-info-with-fns
                               data (map 'vector #'save-function-coverage (code-covered-info.fns data))))))

(defun combine-coverage (coverage-states)
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


(defun restore-coverage (coverage-state)
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

(defvar *loading-coverage*)

(defun write-coverage-to-file (coverage pathname)
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
  
(defun read-coverage-from-file (pathname)
  " Return the coverage state saved in the file.  Doesn't affect the current coverage state."
  (let ((*package* (pkg-arg "CCL"))
        (*loading-coverage* :none))
    (load pathname)
    (when (eq *loading-coverage* :none)
      (error "~s doesn't seem to be a saved coverage file" pathname))
    (make-coverage-state :alist *loading-coverage*)))

(defun save-coverage-in-file (pathname)
  "Save the current coverage state in the file designed by PATHNAME"
  (write-coverage-to-file (save-coverage) pathname))

(defun restore-coverage-from-file (pathname)
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


(defstruct (coverage-statistics (:conc-name "COVERAGE-"))
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

(defun coverage-statistics ()
  (let* ((*file-coverage* nil)
	 (*coverage-subnotes* (make-hash-table :test #'eq :shared nil))
	 (*emitted-code-notes* (make-hash-table :test #'eq :shared nil))
	 (*entry-code-notes* (make-hash-table :test #'eq :shared nil)))
    (get-coverage) 
    (loop for coverage in *file-coverage*
          as stats = (make-coverage-statistics :source-file (file-coverage-file coverage))
          do (map nil (lambda (fn)
                        (let ((note (function-entry-code-note fn)))
                          (when note (precompute-note-coverage note))))
                  (file-coverage-toplevel-functions coverage))
          do (destructuring-bind (total entered %entered covered %covered)
                 (count-covered-sexps coverage)
               (declare (ignore %entered %covered))
               (setf (coverage-expressions-total stats) total)
               (setf (coverage-expressions-entered stats) entered)
               (setf (coverage-expressions-covered stats) covered))
          do (let ((count (count-unreached-branches coverage)))
               (setf (coverage-unreached-branches stats) count))
          do (destructuring-bind (total covered %covered) (count-covered-aexps coverage)
               (declare (ignore %covered))
               (setf (coverage-code-forms-total stats) total)
               (setf (coverage-code-forms-covered stats) covered))
          do (destructuring-bind (total fully %fully partly %partly never %never)
                 (count-covered-entry-notes coverage)
               (declare (ignore %fully %partly %never))
               (setf (coverage-functions-total stats) total)
               (setf (coverage-functions-fully-covered stats) fully)
               (setf (coverage-functions-partly-covered stats) partly)
               (setf (coverage-functions-not-entered stats) never))
          collect stats)))


(defun report-coverage (output-file &key (external-format :default) (statistics t) (html t))
  "If :HTML is non-nil, generate an HTML report, consisting of an index file in OUTPUT-FILE
and, in the same directory, one html file for each instrumented source file that has been
loaded in the current session.
The external format of the source files can be specified with the EXTERNAL-FORMAT parameter.
If :STATISTICS is non-nil, a CSV file is generated with a table.  If
:STATISTICS is a filename, that file is used, else 'statistics.csv' is
written to the output directory.
"
  (let* ((paths)
         (directory (make-pathname :name nil :type nil :defaults output-file))
         (coverage-dir (common-coverage-directory))
	 (*file-coverage* nil)
	 (*coverage-subnotes* (make-hash-table :test #'eq :shared nil))
	 (*emitted-code-notes* (make-hash-table :test #'eq :shared nil))
	 (*entry-code-notes* (make-hash-table :test #'eq :shared nil))
         (index-file (and html (merge-pathnames output-file "index.html")))
         (stats-file (and statistics (merge-pathnames (if (or (stringp statistics)
                                                              (pathnamep statistics))
                                                        (merge-pathnames statistics "statistics.csv")
                                                        "statistics.csv")
                                                      output-file))))
    (get-coverage)
    (ensure-directories-exist directory)
    (loop for coverage in *file-coverage*
      as truename = (or (probe-file (file-coverage-file coverage))
		    (progn (warn "Cannot find ~s, won't report coverage" (file-coverage-file coverage))
			   nil))
      do (when truename
           (let* ((src-name (enough-namestring truename coverage-dir))
                  (html-name (substitute
                              #\_ #\: (substitute
                                       #\_ #\. (substitute
                                                #\_ #\/ (namestring-unquote src-name)))))
                  (file (file-coverage-file coverage)))
             (when html
               (with-coverage-mismatch-catch (file)
                 (let* ((data (assoc-by-filename file *code-covered-functions*))
                        (checksum (fcomp-file-checksum (code-covered-info.file data)
                                                       :external-format (code-covered-info.ef data))))
                   (unless (eql checksum (code-covered-info.id data))
                     (cerror "Try coloring anyway"
                             "File ~s has changed since coverage source location info was recorded."
                             (code-covered-info.file data))))
                 (with-open-file (stream (make-pathname :name html-name :type "html" :defaults directory)
                                         :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
                   (report-file-coverage index-file coverage stream external-format))))
             (push (list* src-name html-name coverage) paths))))
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
        (error "One of :HTML or :STATISTICS must be non-nil")))
    (values index-file stats-file)))

(defun report-coverage-to-streams (paths html-stream stats-stream)
  (when html-stream (write-coverage-styles html-stream))
  (unless paths
    (warn "No coverage data found for any file, producing an empty report. Maybe you forgot to (SETQ CCL::*COMPILE-CODE-COVERAGE* T) before compiling?")
    (when html-stream (format html-stream "<h3>No code coverage data found.</h3>~%"))
    (when stats-stream (format stats-stream "No code coverage data found.~%"))
    (return-from report-coverage-to-streams))
  (when html-stream (format html-stream "<table class='summary'>"))
  (coverage-stats-head html-stream stats-stream)
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
  (when html-stream (format html-stream "</table>")))

(defun precompute-note-coverage (note &optional refs)
  (when note
    (let ((subnotes (coverage-subnotes note))
	  (refs (cons note refs)))
      (declare (dynamic-extent refs))
      (loop for sub in subnotes
	    when (member sub refs)
	    do (break "Circularity!!")
	    unless (member sub refs)
	    do (precompute-note-coverage sub refs))
      (when (and (or (not (emitted-code-note-p note))
		     (code-note-code-coverage note))
		 (loop for sub in subnotes
		       always (or (eq 'full (code-note-code-coverage sub))
				  (entry-code-note-p sub))))
	(setf (code-note-code-coverage note) 'full)))))


(defun style-for-coverage (coverage)
  (case coverage
    ((full) $totally-covered-style)
    ((nil) $not-executed-style)
    (t $partially-covered-style)))
  
(defun fill-with-text-style (coverage location-note styles)
  (fill styles (style-for-coverage coverage)
        :start (source-note-start-pos location-note)
        :end (source-note-end-pos location-note)))

(defun update-text-styles (note styles)
  (let ((source (code-note-source-note note)))
    (when source
      (fill-with-text-style (code-note-code-coverage note) source styles))
    (unless (and (emitted-code-note-p note)
                 (memq (code-note-code-coverage note) '(nil full))
                 ;; If not a source note, descend in case have some subnotes
                 ;; that can be shown
                 source)
      (loop for sub in (coverage-subnotes note)
            unless (entry-code-note-p sub)
            do (update-text-styles sub styles)))))

(defun entry-note-unambiguous-source (entry-note)
  ;; Return the nearest containing source note provided it can be done unambiguously.
  (loop for n = entry-note then parent until (code-note-source-note n)
	as parent = (code-note-parent-note n)
	do (unless (and parent
			(labels ((no-other-entry-subnotes (n refs)
				   (let ((subs (coverage-subnotes n))
					 (refs (cons n refs)))
				     (declare (dynamic-extent refs))
				     (loop for sub in subs
					   always (or (memq sub refs)
						      (eq sub entry-note)
						      (and (not (entry-code-note-p sub))
							   (no-other-entry-subnotes sub refs)))))))
			  (no-other-entry-subnotes parent ())))
	     (return nil))
	finally (return (code-note-source-note n))))

(defun colorize-source-note (note styles)
  ;; Change coverage flag to 'full if all subforms are covered.
  (precompute-note-coverage note)
  ;; Now actually change text styles, from outside in.
  ;; But first, a special kludge:
  ;; In cases like (setq foo (function (lambda (x) x))), we can colorize "(setq foo (function "
  ;; based on whether the setq got executed, and "(lambda (x) x)" on whether the inner
  ;; function got executed.  However, suppose have a macro "(setq-fun foo (x) x)" that
  ;; expanded into the above, there isn't a clear way to show the distinction between
  ;; just referencing the inner fn and executing it.  In practice, the colorization
  ;; based on the inner function is more interesting -- consider for example DEFUN,
  ;; nobody cares whether the defun form itself got executed.
  ;; So when showing the colorization of an inner function, we usurp the whole nearest source
  ;; form, provided it can be done unambiguously.
  (let ((n (entry-note-unambiguous-source note)))
    (when n
      (fill-with-text-style (code-note-code-coverage note) n styles)))
  (update-text-styles note styles))

(defun function-source-form-note (fn)
  ;; Find the outermost source form containing the fn.
  (loop with sn = nil
        for n = (function-entry-code-note fn) then (code-note-parent-note n)
	do (when (null n) (return nil))
	do (when (setq sn (code-note-source-note n))
	     (loop for s = (source-note-source sn) while (source-note-p s)
		   do (setq sn s))
	     (return sn))))

(defun colorize-acode (fn acode-styles)
  (let* ((acode (%function-acode-string fn))
         (note (function-entry-code-note fn))
         (range (and note (code-note-acode-range note))))
    (when (and acode range)
      (let* ((cell (or (gethash acode acode-styles)
                       (setf (gethash acode acode-styles)
                             (let ((string (decode-string-from-octets acode :external-format :utf-8)))
                               (cons string
                                     (make-array (length string)
                                                 :initial-element $no-style
                                                 :element-type '(unsigned-byte 2)))))))
             (styles (cdr cell)))
        (iterate update ((note note))
          (multiple-value-bind (start end) (decode-file-range (code-note-acode-range note))
            (when (and start
                       (setq start (position-if-not #'whitespacep acode :start start :end end)))
              (fill styles (style-for-coverage (code-note-code-coverage note))
                    :start start
                    :end end)))
          (loop for sub in (coverage-subnotes note)
            unless (entry-code-note-p sub)
            do (update sub)))))))

(defun colorize-function (fn styles acode-styles &optional refs)
  (let* ((note (function-entry-code-note fn))
	 (source (function-source-form-note fn))
	 (refs (cons fn refs)))
    (declare (dynamic-extent refs))
    ;; Colorize the body of the function
    (when note
      (colorize-source-note note styles)
      (colorize-acode fn acode-styles))
    ;; And now any subfunction references
    (lfunloop for imm in fn
	      when (and (functionp imm)
			(not (memq imm refs))
                        ;; See note in get-function-coverage
			(or (null source)
			    (eq source (function-source-form-note imm))
			    #+debug (progn
				      (warn "Ignoring ref to ~s from ~s" imm fn)
				      nil)))
	      do (colorize-function imm styles acode-styles refs))))

(defun report-file-coverage (index-file coverage html-stream external-format)
  "Print a code coverage report of FILE into the stream HTML-STREAM."
  (format html-stream "<html><head>")
  (write-coverage-styles html-stream)
  (format html-stream "</head><body>")
  (let* ((source (with-open-file (s (file-coverage-file coverage) :external-format external-format)
                   (let ((string (make-string (file-length s))))
                     (read-sequence string s)
                     string)))
         (styles (make-array (length source)
                             :initial-element $no-style
                             :element-type '(unsigned-byte 2)))
         (acode-styles (make-hash-table :test #'eq)))
    (map nil #'(lambda (fn) (colorize-function fn styles acode-styles))
         (file-coverage-toplevel-functions coverage))
    (print-file-coverage-report index-file html-stream coverage styles acode-styles source)
    (format html-stream "</body></html>")))

(defun print-file-coverage-report (index-file html-stream coverage styles acode-styles source)
  (let ((*print-case* :downcase))
    (format html-stream "<h3><a href=~s>Coverage report</a>: ~a <br />~%</h3>~%"
            (native-translated-namestring (make-pathname :name (pathname-name index-file)
							 :type (pathname-type index-file)))
            (file-coverage-file coverage))
    (format html-stream "<table class='summary'>")
    (coverage-stats-head html-stream nil)
    (coverage-stats-data html-stream nil coverage)
    (format html-stream "</table>")

    (format html-stream "<div class='key'><b>Key</b><br />~%")
    (format html-stream "<div class='st~a'>Fully covered - every single instruction executed</div>" $totally-covered-style)
    (format html-stream "<div class='st~a'>Partly covered - entered but some subforms not executed</div>" $partially-covered-style)
    (format html-stream "<div class='st~a'>Never entered - not a single instruction executed</div>" $not-executed-style)
    (format html-stream "</div><p></p>~%")

    ;; Output source intertwined with acode
    (iterate output ((start 0) (line 0) (queue (file-coverage-acode-queue coverage)))
      (format html-stream "<div class='source'><code>")
      (let ((next (car queue)))
        (multiple-value-bind (end last-line)
            (output-styled html-stream source styles
                           :start start
                           :line line
                           :limit (car next))
          (format html-stream "</code></div>~%")
          (when (and next end (<= (car next) end))
            (destructuring-bind (pos . strings) next
              (format html-stream "<a href=javascript:swap('~d')><span class='toggle' id='p~:*~d'>Show expansion</span></a>~%~
                                   <div class='acode' id='a~:*~d'><code>" pos)
              (loop for acode in strings as (string . styles) = (gethash acode acode-styles)
                    do (output-styled html-stream string styles)
                    do (fresh-line html-stream))
              (format html-stream "</code></div><hr/>~%")
              (output (1+ end) last-line (cdr queue)))))))))

(defun output-styled (html-stream source styles &key (start 0) line limit)
  (let ((last-style $no-style)
        (col 0)
        (line line))
    (labels ((outch (char)
               (if (eql char #\Tab)
                 (dotimes (i (- 8 (mod col 8)))
                   (incf col)
                   (write-string " " html-stream))
                 (progn
                   (incf col)
                   (if (or (alphanumericp char) (find char "()+-:* ")) ;; common and safe
                     (write-char char html-stream)
                     (format html-stream "&#~D;" (char-code char))))))
             (start-line ()
               (when line
                 (incf line)
                 (format html-stream "<span class='line'>~A</span>" line))
               (write-char #\space html-stream)
               (setq col 0))
             (set-style (new)
               (unless (eq last-style new)
                 (unless (eq last-style $no-style) (format html-stream "</span>"))
                 (unless (eq new $no-style) (format html-stream "<span class='st~a'>" new))
                 (setq last-style new)))
             (end-line ()
               (set-style $no-style)
               (format html-stream "~%")))
      (declare (inline outch start-line end-line))
      (unless limit (setq limit (length source)))
      (start-line)
      (loop
        for pos from start below (length source)
        as char = (aref source pos) as style = (aref styles pos)
        do (set-style style)
        do (case char
             ((#\Newline)
              (end-line)
              (when (<= limit pos)
                (return (values pos line)))
              (start-line))
             (t
              (outch char)))
        finally (end-line)))))


(defun coverage-stats-head (html-stream stats-stream)
  (when html-stream
    (format html-stream "<tr class='head-row'><td></td>")
    (format html-stream "<td class='main-head' colspan='5'>Expressions</td>")
    (format html-stream "<td class='main-head' colspan='1'>Branches</td>")
    (format html-stream "<td class='main-head' colspan='3'>Code Forms</td>")
    (format html-stream "<td class='main-head' colspan='7'>Functions</td></tr>")
    (format html-stream "<tr class='head-row'>~{<td width='60px'>~A</td>~}</tr>"
            '("Source file"
              ;; Expressions
              "Total" "Entered" "% entered" "Fully covered" "% fully covered"
              ;; Branches
              "total unreached"
              ;; Code forms
              "Total" "Covered" "% covered"
              ;; Functions
              "Total" "Fully covered" "% fully covered" "Partly covered" "% partly covered" "Not entered" "% not entered")))
  (when stats-stream
    (format stats-stream "~{~a~^,~}"
	    `("Source file"
              "Expressions Total" "Expressions Entered" "% Expressions Entered"
              "Unreached Branches"
              "Code Forms Total" "Code Forms Covered" "% Code Forms Covered"
              "Functions Total" "Functions Fully Covered" "% Functions Fully Covered"
	      "Functions Partly Covered" "% Functions Partly Covered"
	      "Functions Not Entered" "% Functions Not Entered"))))

(defun coverage-stats-data (html-stream stats-stream coverage &optional evenp report-name src-name)
  (when html-stream
    (format html-stream "<tr class='~:[odd~;even~]'>" evenp)
    (if report-name
      (format html-stream "<td class='text-cell'><a href='~a.html'>~a</a></td>" report-name src-name)
      (format html-stream "<td class='text-cell'>~a</td>" (file-coverage-file coverage))))
  (when stats-stream
    (format stats-stream "~a," (file-coverage-file coverage)))

  (let ((exp-counts (count-covered-sexps coverage)))
    (when html-stream
      (format html-stream "~{<td>~:[-~;~:*~a~]</td><td>~:[-~;~:*~a~]</td><td>~:[-~;~:*~5,1f%~]</td><td>~:[-~;~:*~a~]</td><td>~:[-~;~:*~5,1f%~]</td>~}" exp-counts))
    (when stats-stream
      (format stats-stream "~{~:[~;~:*~a~],~:[~;~:*~a~],~:[~;~:*~5,1f%~],~:[~;~:*~a~],~:[~;~:*~5,1f%~],~}" exp-counts)))

  (let ((count (count-unreached-branches coverage)))
    (when html-stream
      (format html-stream "<td>~:[-~;~:*~a~]</td>" count))
    (when stats-stream
      (format stats-stream "~:[~;~:*~a~]," count)))

  (let ((exp-counts (count-covered-aexps coverage)))
    (when html-stream
      (format html-stream "~{<td>~:[-~;~:*~a~]</td><td>~:[-~;~:*~a~]</td><td>~:[-~;~:*~5,1f%~]</td>~}" exp-counts))
    (when stats-stream
      (format stats-stream "~{~:[~;~:*~a~],~:[~;~:*~a~],~:[~;~:*~5,1f%~],~}" exp-counts)))

  (destructuring-bind (total . counts) (count-covered-entry-notes coverage)
    (when html-stream
      (format html-stream "<td>~:[-~;~:*~a~]</td>~{<td>~:[-~;~:*~a~]</td><td>~:[-~;~:*~5,1f%~]</td>~}</tr>" total counts))
    (when stats-stream
      (format stats-stream "~:[~;~:*~a~],~{~:[~;~:*~a~],~:[-~;~:*~5,1f%~]~^,~}~%" total counts))))

(defun map-coverage-entry-notes (coverage fn)
  (map nil #'(lambda (function)
                 (let ((note (function-entry-code-note function)))
                   (when (and note
			      ;; Ignore toplevel functions created by the compiler.
			      (or (code-note-source-note note)
				  (code-note-parent-note note)))
                     (funcall fn note))))
       (file-coverage-functions coverage)))


(defun count-covered-entry-notes (coverage)
  (let ((fully 0) (partly 0) (never 0) (total 0))
    (map-coverage-entry-notes
     coverage
     #'(lambda (note)
         (incf total)
         (case (code-note-code-coverage note)
           ((full) (incf fully))
           ((nil) (incf never))
           (t (incf partly)))))
    (if (> total 0)
	(list total
	      fully (* 100.0 (/ fully total))
	      partly (* 100.0 (/ partly total))
	      never (* 100.0 (/ never total)))
	'(0 0 -- 0 -- 0 --))))

(defun count-covered-aexps (coverage)
  (let ((covered 0) (total 0))
    (map-coverage-entry-notes
     coverage
     (lambda (note)
       (labels ((rec (note)
		  (when (emitted-code-note-p note)
		    (incf total)
		    (when (code-note-code-coverage note)
		      (incf covered)))
                  (loop for sub in (coverage-subnotes note)
                        unless (entry-code-note-p sub) do (rec sub))))
         (rec note))))
    (list total covered (if (> total 0) (* 100.0d0 (/ covered total)) '--))))

(defun count-covered-sexps (coverage)
  ;; Count the number of source expressions that have been entered (regardless
  ;; of whether or not they are completely covered).
  (let ((entered 0) (covered 0) (total 0))
    (map-coverage-entry-notes
     coverage
     (lambda (note)
       (labels ((rec (note)
                  (when (code-note-source-note note)
                    #+debug (format t "~&~s" note)
                    (incf total)
                    (when (code-note-code-coverage note)
                      (incf entered)
                      (when (eq (code-note-code-coverage note) 'full)
                        (incf covered))))
                  (loop for sub in (coverage-subnotes note)
                        unless (entry-code-note-p sub) do (rec sub))))
         (rec note))))
    (list total
          entered (if (> total 0) (* 100.0d0 (/ entered total)) '--)
          covered (if (> total 0) (* 100.0d0 (/ covered total)) '--))))

(defun count-unreached-branches (coverage)
  ;; Count the number of maximal unentered forms
  (let ((count 0))
    (map-coverage-entry-notes
     coverage
     (lambda (note)
       (labels ((rec (note parent)
                  (case (code-note-code-coverage note)
                    ((full) nil)
                    ((nil) (when parent (incf count)))
                    (t (loop for sub in (coverage-subnotes note)
                             unless (entry-code-note-p sub) do (rec sub note))))))
         (rec note nil))))
    count))

(defun write-coverage-styles (html-stream)
  (format html-stream "<style type='text/css'>
*.st~a { background-color: #ffaaaa }
*.st~a { background-color: #aaffaa }
*.st~a { background-color: #44dd44 }
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

