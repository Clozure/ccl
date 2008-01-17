;;; -*- Log: hemlock.log; Package: dired -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
#+CMU (ext:file-comment
  "$Header$")
;;;
;;; **********************************************************************
;;;
;;; This file contains site dependent code for dired.
;;; Written by Bill Chiles.
;;;

(defpackage "DIRED"
  (:shadow "RENAME-FILE" "DELETE-FILE")
  (:export "COPY-FILE" "RENAME-FILE" "FIND-FILE" "DELETE-FILE"
	   "MAKE-DIRECTORY"
	   "*UPDATE-DEFAULT*" "*CLOBBER-DEFAULT*" "*RECURSIVE-DEFAULT*"
	   "*REPORT-FUNCTION*" "*ERROR-FUNCTION*" "*YESP-FUNCTION*"
	   "PATHNAMES-FROM-PATTERN"))
  
(in-package "DIRED")


;;;; Exported parameters.

(defparameter *update-default* nil
  "Update arguments to utilities default to this value.")

(defparameter *clobber-default* t
  "Clobber arguments to utilities default to this value.")

(defparameter *recursive-default* nil
  "Recursive arguments to utilities default to this value.")



;;;; WILDCARDP

(defconstant wildcard-char #\*
  "Wildcard designator for file names will match any substring.")

(defmacro wildcardp (file-namestring)
  `(position wildcard-char (the simple-string ,file-namestring) :test #'char=))



;;;; User interaction functions, variable declarations, and their defaults.

(defun default-error-function (string &rest args)
  (apply #'error string args))
;;;
(defvar *error-function* #'default-error-function
  "This function is called when an error is encountered in dired code.")

(defun default-report-function (string &rest args)
  (apply #'format t string args))
;;;
(defvar *report-function* #'default-report-function
  "This function is called when the user needs to be informed of something.")

(defun default-yesp-function (string &rest args)
  (apply #'format t string args)
  (let ((answer (nstring-downcase (string-trim '(#\space #\tab) (read-line)))))
    (declare (simple-string answer))
    (or (string= answer "")
	(string= answer "y")
	(string= answer "yes")
	(string= answer "ye"))))
;;;
(defvar *yesp-function* #'default-yesp-function
  "Function to query the user about clobbering an already existent file.")



;;;; Copy-File

;;; WILD-MATCH objects contain information about wildcard matches.  File is the
;;; Sesame namestring of the file matched, and substitute is a substring of the
;;; file-namestring of file.
;;;
(defstruct (wild-match (:print-function print-wild-match)
		       (:constructor make-wild-match (file substitute)))
  file
  substitute)

(defun print-wild-match (obj str n)
  (declare (ignore n))
  (format str "#<Wild-Match  ~S  ~S>"
	  (wild-match-file obj) (wild-match-substitute obj)))


(defun copy-file (spec1 spec2 &key (update *update-default*)
				   (clobber *clobber-default*)
				   (directory () directoryp))
  "Copy file spec1 to spec2.  A single wildcard is acceptable, and directory
   names may be used.  If spec1 and spec2 are both directories, then a
   recursive copy is done of the files and subdirectory structure of spec1;
   if spec2 is in the subdirectory structure of spec1, the recursion will
   not descend into it.  Use spec1/* to copy only the files in spec1 to
   directory spec2.  If spec2 is a directory, and spec1 is a file, then
   spec1 is copied into spec2 with the same pathname-name.  Files are
   copied maintaining the source's write date.  If :update is non-nil, then
   files are only copied if the source is newer than the destination, still
   maintaining the source's write date; the user is not warned if the
   destination is newer (not the same write date) than the source.  If
   :clobber and :update are nil, then if any file spec2 already exists, the
   user will be asked whether it should be overwritten or not."
  (cond
   ((not directoryp)
    (let* ((ses-name1 (ext:unix-namestring spec1 t))
	   (exists1p (unix:unix-file-kind ses-name1))
	   (ses-name2 (ext:unix-namestring spec2 nil))
	   (pname1 (pathname ses-name1))
	   (pname2 (pathname ses-name2))
	   (dirp1 (directoryp pname1))
	   (dirp2 (directoryp pname2))
	   (wildp1 (wildcardp (file-namestring pname1)))
	   (wildp2 (wildcardp (file-namestring pname2))))
      (when (and dirp1 wildp1)
	(funcall *error-function*
		 "Cannot have wildcards in directory names -- ~S." pname1))
      (when (and dirp2 wildp2)
	(funcall *error-function*
		 "Cannot have wildcards in directory names -- ~S." pname2))
      (when (and dirp1 (not dirp2))
	(funcall *error-function*
		 "Cannot handle spec1 being a directory and spec2 a file."))
      (when (and wildp2 (not wildp1))
	(funcall *error-function*
		 "Cannot handle destination having wildcards without ~
		 source having wildcards."))
      (when (and wildp1 (not wildp2) (not dirp2))
	(funcall *error-function*
		 "Cannot handle source with wildcards and destination ~
		 without, unless destination is a directory."))
      (cond ((and dirp1 dirp2)
	     (unless (directory-existsp ses-name1)
	       (funcall *error-function*
			"Directory does not exist -- ~S." pname1))
	     (unless (directory-existsp ses-name2)
	       (enter-directory ses-name2))
	     (recursive-copy pname1 pname2 update clobber pname2
			     ses-name1 ses-name2))
	    (dirp2
	     ;; merge pname2 with pname1 to pick up a similar file-namestring.
	     (copy-file-1 pname1 wildp1 exists1p
			  (merge-pathnames pname2 pname1)
			  wildp1 update clobber))
	    (t (copy-file-1 pname1 wildp1 exists1p
			    pname2 wildp2 update clobber)))))
    (directory
     (when (pathname-directory spec1)
       (funcall *error-function*
		"Spec1 is just a pattern when supplying directory -- ~S."
		spec1))
     (let* ((pname2 (pathname (ext:unix-namestring spec2 nil)))
	    (dirp2 (directoryp pname2))
	    (wildp1 (wildcardp spec1))
	    (wildp2 (wildcardp (file-namestring pname2))))
       (unless wildp1
	 (funcall *error-function*
		  "Pattern, ~S, does not contain a wildcard."
		  spec1))
       (when (and (not wildp2) (not dirp2))
	 (funcall *error-function*
		  "Cannot handle source with wildcards and destination ~
		   without, unless destination is a directory."))
       (copy-wildcard-files spec1 wildp1
			    (if dirp2 (merge-pathnames pname2 spec1) pname2)
			    (if dirp2 wildp1 wildp2)
			    update clobber directory))))
  (values))

;;; RECURSIVE-COPY takes two pathnames that represent directories, and
;;; the files in pname1 are copied into pname2, recursively descending into
;;; subdirectories.  If a subdirectory of pname1 does not exist in pname2,
;;; it is created.  Pname1 is known to exist.  Forbidden-dir is originally
;;; the same as pname2; this keeps us from infinitely recursing if pname2
;;; is in the subdirectory structure of pname1.  Returns t if some file gets
;;; copied.
;;; 
(defun recursive-copy (pname1 pname2 update clobber
		       forbidden-dir ses-name1 ses-name2)
  (funcall *report-function* "~&~S  ==>~%  ~S~%" ses-name1 ses-name2)
  (dolist (spec (directory (directory-namestring pname1)))
    (let ((spec-ses-name (namestring spec)))
      (if (directoryp spec)
	  (unless (equal (pathname spec-ses-name) forbidden-dir)
	    (let* ((dir2-pname (merge-dirs spec pname2))
		   (dir2-ses-name (namestring dir2-pname)))
	      (unless (directory-existsp dir2-ses-name)
		(enter-directory dir2-ses-name))
	      (recursive-copy spec dir2-pname update clobber forbidden-dir
			      spec-ses-name dir2-ses-name)
	      (funcall *report-function* "~&~S  ==>~%  ~S~%" ses-name1
		       ses-name2)))
	  (copy-file-2 spec-ses-name
		       (namestring (merge-pathnames pname2 spec))
		       update clobber)))))

;;; MERGE-DIRS picks out the last directory name in the pathname pname1 and
;;; adds it to the end of the sequence of directory names from pname2, returning
;;; a pathname.
;;;
#|
(defun merge-dirs (pname1 pname2)
  (let* ((dirs1 (pathname-directory pname1))
	 (dirs2 (pathname-directory pname2))
	 (dirs2-len (length dirs2))
	 (new-dirs2 (make-array (1+ dirs2-len))))
    (declare (simple-vector dirs1 dirs2 new-dirs2))
    (replace new-dirs2 dirs2)
    (setf (svref new-dirs2 dirs2-len)
	  (svref dirs1 (1- (length dirs1))))
    (make-pathname :directory new-dirs2 :device :absolute)))
|#

(defun merge-dirs (pname1 pname2)
  (let* ((dirs1 (pathname-directory pname1))
	 (dirs2 (pathname-directory pname2))
	 (dirs2-len (length dirs2))
	 (new-dirs2 (make-list (1+ dirs2-len))))
    (replace new-dirs2 dirs2)
    (setf (nth dirs2-len new-dirs2)
	  (nth (1- (length dirs1)) dirs1))
    (make-pathname :directory new-dirs2 :device :unspecific)))

;;; COPY-FILE-1 takes pathnames which either both contain a single wildcard
;;; or none.  Wildp1 and Wildp2 are either nil or indexes into the
;;; file-namestring of pname1 and pname2, respectively, indicating the position
;;; of the wildcard character.  If there is no wildcard, then simply call
;;; COPY-FILE-2; otherwise, resolve the wildcard and copy those matching files.
;;;
(defun copy-file-1 (pname1 wildp1 exists1p pname2 wildp2 update clobber)
  (if wildp1 
      (copy-wildcard-files pname1 wildp1 pname2 wildp2 update clobber)
      (let ((ses-name1 (namestring pname1)))
	(unless exists1p (funcall *error-function*
				  "~S does not exist." ses-name1))
	(copy-file-2 ses-name1 (namestring pname2) update clobber))))

(defun copy-wildcard-files (pname1 wildp1 pname2 wildp2 update clobber
				   &optional directory)
  (multiple-value-bind (dst-before dst-after)
		       (before-wildcard-after (file-namestring pname2) wildp2)
    (dolist (match (resolve-wildcard pname1 wildp1 directory))
      (copy-file-2 (wild-match-file match)
		   (namestring (concatenate 'simple-string
					    (directory-namestring pname2)
					    dst-before
					    (wild-match-substitute match)
					    dst-after))
		   update clobber))))

;;; COPY-FILE-2 copies ses-name1 to ses-name2 depending on the values of update
;;; and clobber, with respect to the documentation of COPY-FILE.  If ses-name2
;;; doesn't exist, then just copy it; otherwise, if update, then only copy it
;;; if the destination's write date precedes the source's, and if not clobber
;;; and not update, then ask the user before doing the copy.
;;;
(defun copy-file-2 (ses-name1 ses-name2 update clobber)
  (let ((secs1 (get-write-date ses-name1)))
    (cond ((not (probe-file ses-name2))
	   (do-the-copy ses-name1 ses-name2 secs1))
	  (update
	   (let ((secs2 (get-write-date ses-name2)))
	     (cond (clobber
		    (do-the-copy ses-name1 ses-name2 secs1))
		   ((and (> secs2 secs1)
			 (funcall *yesp-function*
				  "~&~S  ==>  ~S~%  ~
				  ** Destination is newer than source.  ~
				  Overwrite it? "
				  ses-name1 ses-name2))
		    (do-the-copy ses-name1 ses-name2 secs1))
		   ((< secs2 secs1)
		    (do-the-copy ses-name1 ses-name2 secs1)))))
	  ((not clobber)
	   (when (funcall *yesp-function*
			  "~&~S  ==>  ~S~%  ** Destination already exists.  ~
			  Overwrite it? "
			  ses-name1 ses-name2)
	     (do-the-copy ses-name1 ses-name2 secs1)))
	  (t (do-the-copy ses-name1 ses-name2 secs1)))))

(defun do-the-copy (ses-name1 ses-name2 secs1)
  (let* ((fd (open-file ses-name1)))
    (unwind-protect
	(multiple-value-bind (data byte-count mode)
			     (read-file fd ses-name1)
	  (unwind-protect (write-file ses-name2 data byte-count mode)
	    (system:deallocate-system-memory data byte-count)))
      (close-file fd)))
  (set-write-date ses-name2 secs1)
  (funcall *report-function* "~&~S  ==>~%  ~S~%" ses-name1 ses-name2))


;;;; Rename-File

(defun rename-file (spec1 spec2 &key (clobber *clobber-default*)
			  (directory () directoryp))
  "Rename file spec1 to spec2.  A single wildcard is acceptable, and spec2 may
   be a directory with the result spec being the merging of spec2 with spec1.
   If clobber is nil and spec2 exists, then the user will be asked to confirm
   the renaming.  As with Unix mv, if you are renaming a directory, don't
   specify the trailing slash."
  (cond
   ((not directoryp)
    (let* ((ses-name1 (ext:unix-namestring spec1 t))
	   (exists1p (unix:unix-file-kind ses-name1))
	   (ses-name2 (ext:unix-namestring spec2 nil))
	   (pname1 (pathname ses-name1))
	   (pname2 (pathname ses-name2))
	   (dirp2 (directoryp pname2))
	   (wildp1 (wildcardp (file-namestring pname1)))
	   (wildp2 (wildcardp (file-namestring pname2))))
      (if (and dirp2 wildp2)
	  (funcall *error-function*
		   "Cannot have wildcards in directory names -- ~S." pname2))
      (if (and wildp2 (not wildp1))
	  (funcall *error-function*
		   "Cannot handle destination having wildcards without ~
		   source having wildcards."))
      (if (and wildp1 (not wildp2) (not dirp2))
	  (funcall *error-function*
		   "Cannot handle source with wildcards and destination ~
		   without, unless destination is a directory."))
      (if dirp2
	  (rename-file-1 pname1 wildp1 exists1p (merge-pathnames pname2
								 pname1)
			 wildp1 clobber)
	  (rename-file-1 pname1 wildp1 exists1p pname2 wildp2 clobber))))
    (directory
     (when (pathname-directory spec1)
       (funcall *error-function*
		"Spec1 is just a pattern when supplying directory -- ~S."
		spec1))

     (let* ((pname2 (pathname (ext:unix-namestring spec2 nil)))
	    (dirp2 (directoryp pname2))
	    (wildp1 (wildcardp spec1))
	    (wildp2 (wildcardp (file-namestring pname2))))
       (unless wildp1
	 (funcall *error-function*
		  "Pattern, ~S, does not contain a wildcard."
		  spec1))
       (when (and (not wildp2) (not dirp2))
	 (funcall *error-function*
		  "Cannot handle source with wildcards and destination ~
		   without, unless destination is a directory."))
       (rename-wildcard-files spec1 wildp1
			      (if dirp2 (merge-pathnames pname2 spec1) pname2)
			      (if dirp2 wildp1 wildp2)
			      clobber directory))))
  (values))

;;; RENAME-FILE-1 takes pathnames which either both contain a single wildcard
;;; or none.  Wildp1 and Wildp2 are either nil or indexes into the
;;; file-namestring of pname1 and pname2, respectively, indicating the position
;;; of the wildcard character.  If there is no wildcard, then simply call
;;; RENAME-FILE-2; otherwise, resolve the wildcard and rename those matching files.
;;;
(defun rename-file-1 (pname1 wildp1 exists1p pname2 wildp2 clobber)
  (if wildp1
      (rename-wildcard-files pname1 wildp1 pname2 wildp2 clobber)
      (let ((ses-name1 (namestring pname1)))
	(unless exists1p (funcall *error-function*
				  "~S does not exist." ses-name1))
	(rename-file-2 ses-name1 (namestring pname2) clobber))))

(defun rename-wildcard-files (pname1 wildp1 pname2 wildp2 clobber
				   &optional directory)
  (multiple-value-bind (dst-before dst-after)
		       (before-wildcard-after (file-namestring pname2) wildp2)
    (dolist (match (resolve-wildcard pname1 wildp1 directory))
      (rename-file-2 (wild-match-file match)
		     (namestring (concatenate 'simple-string
					      (directory-namestring pname2)
					      dst-before
					      (wild-match-substitute match)
					      dst-after))
		     clobber))))

(defun rename-file-2 (ses-name1 ses-name2 clobber)
  (cond ((and (probe-file ses-name2) (not clobber))
	 (when (funcall *yesp-function*
			"~&~S  ==>  ~S~%  ** Destination already exists.  ~
			Overwrite it? "
			ses-name1 ses-name2)
	   (sub-rename-file ses-name1 ses-name2)
	   (funcall *report-function* "~&~S  ==>~%  ~S~%" ses-name1 ses-name2)))
	(t (sub-rename-file ses-name1 ses-name2)
	   (funcall *report-function* "~&~S  ==>~%  ~S~%" ses-name1 ses-name2))))



;;;; Find-File

(defun find-file (file-name &optional (directory "")
			    (find-all-p nil find-all-suppliedp))
  "Find the file with file-namestring file recursively looking in directory.
   If find-all-p is non-nil, then do not stop searching upon finding the first
   occurance of file.  File may contain a single wildcard, which causes
   find-all-p to default to t instead of nil."
  (let* ((file (coerce file-name 'simple-string))
	 (wildp (wildcardp file))
	 (find-all-p (if find-all-suppliedp find-all-p wildp)))
    (declare (simple-string file))
    (catch 'found-file
      (if wildp
	  (multiple-value-bind (before after)
			       (before-wildcard-after file wildp)
	    (find-file-aux file directory find-all-p before after))
	  (find-file-aux file directory find-all-p))))
  (values))

(defun find-file-aux (the-file directory find-all-p &optional before after)
  (declare (simple-string the-file))
  (dolist (spec (directory directory))
    (let* ((spec-ses-name (namestring spec))
	   (spec-file-name (file-namestring spec-ses-name)))
      (declare (simple-string spec-ses-name spec-file-name))
      (if (directoryp spec)
	  (find-file-aux the-file spec find-all-p before after)
	  (when (if before
		    (find-match before after spec-file-name :no-cons)
		    (string-equal the-file spec-file-name))
	    (print spec-ses-name)
	    (unless find-all-p (throw 'found-file t)))))))



;;;; Delete-File

;;; DELETE-FILE
;;;    If spec is a directory, but recursive is nil, just pass the directory
;;; down through, letting LISP:DELETE-FILE signal an error if the directory
;;; is not empty.
;;; 
(defun delete-file (spec &key (recursive *recursive-default*)
			      (clobber *clobber-default*))
  "Delete spec asking confirmation on each file if clobber is nil.  A single
   wildcard is acceptable.  If recursive is non-nil, then a directory spec may
   be given to recursively delete the entirety of the directory and its
   subdirectory structure.  An empty directory may be specified without
   recursive being non-nil.  When specifying a directory, the trailing slash
   must be included."
  (let* ((ses-name (ext:unix-namestring spec t))
	 (pname (pathname ses-name)) 
	 (wildp (wildcardp (file-namestring pname)))
	 (dirp (directoryp pname)))
    (if dirp
	(if recursive
	    (recursive-delete pname ses-name clobber)
	    (delete-file-2 ses-name clobber))
	(delete-file-1 pname ses-name wildp clobber)))
  (values))

(defun recursive-delete (directory dir-ses-name clobber)
  (dolist (spec (directory (directory-namestring directory)))
    (let ((spec-ses-name (namestring spec)))
      (if (directoryp spec)
	  (recursive-delete (pathname spec-ses-name) spec-ses-name clobber)
	  (delete-file-2 spec-ses-name clobber))))
  (delete-file-2 dir-ses-name clobber))

(defun delete-file-1 (pname ses-name wildp clobber)
  (if wildp
      (dolist (match (resolve-wildcard pname wildp))
	(delete-file-2 (wild-match-file match) clobber))
      (delete-file-2 ses-name clobber)))

(defun delete-file-2 (ses-name clobber)
  (when (or clobber (funcall *yesp-function* "~&Delete ~S? " ses-name))
    (if (directoryp ses-name)
	(delete-directory ses-name)
	(lisp:delete-file ses-name))
    (funcall *report-function* "~&~A~%" ses-name)))



;;;; Wildcard resolution

(defun pathnames-from-pattern (pattern files)
  "Return a list of pathnames from files whose file-namestrings match
   pattern.  Pattern must be a non-empty string and contains only one
   asterisk.  Files contains no directories."
  (declare (simple-string pattern))
  (when (string= pattern "")
    (funcall *error-function* "Must be a non-empty pattern."))
  (unless (= (count wildcard-char pattern :test #'char=) 1)
    (funcall *error-function* "Pattern must contain one asterisk."))
  (multiple-value-bind (before after)
		       (before-wildcard-after pattern (wildcardp pattern))
    (let ((result nil))
      (dolist (f files result)
	(let* ((ses-namestring (namestring f))
	       (f-namestring (file-namestring ses-namestring))
	       (match (find-match before after f-namestring)))
	  (when match (push f result)))))))


;;; RESOLVE-WILDCARD takes a pathname with a wildcard and the position of the
;;; wildcard character in the file-namestring and returns a list of wild-match
;;; objects.  When directory is supplied, pname is just a pattern, or a
;;; file-namestring.  It is an error for directory to be anything other than
;;; absolute pathnames in the same directory.  Each wild-match object contains
;;; the Sesame namestring of a file in the same directory as pname, or
;;; directory, and a simple-string representing what the wildcard matched.
;;;
(defun resolve-wildcard (pname wild-pos &optional directory)
  (multiple-value-bind (before after)
		       (before-wildcard-after (if directory
						  pname
						  (file-namestring pname))
					      wild-pos)
    (let (result)
      (dolist (f (or directory (directory (directory-namestring pname)))
		 (nreverse result))
	(unless (directoryp f)
	  (let* ((ses-namestring (namestring f))
		 (f-namestring (file-namestring ses-namestring))
		 (match (find-match before after f-namestring)))
	    (if match
		(push (make-wild-match ses-namestring match) result))))))))

;;; FIND-MATCH takes a "before wildcard" and "after wildcard" string and a
;;; file-namestring.  If before and after match a substring of file-namestring
;;; and are respectively left bound and right bound, then anything left in
;;; between is the match returned.  If no match is found, nil is returned.
;;; NOTE: if version numbers ever really exist, then this code will have to be
;;; changed since the file-namestring of a pathname contains the version number.
;;; 
(defun find-match (before after file-namestring &optional no-cons)
  (declare (simple-string before after file-namestring))
  (let ((before-len (length before))
	(after-len (length after))
	(name-len (length file-namestring)))
    (if (>= name-len (+ before-len after-len))
	(let* ((start (if (string= before file-namestring
				   :end1 before-len :end2 before-len)
			  before-len))
	       (end (- name-len after-len))
	       (matchp (and start
			    (string= after file-namestring :end1 after-len
				     :start2 end :end2 name-len))))
	  (if matchp
	      (if no-cons
		  t
		  (subseq file-namestring start end)))))))

(defun before-wildcard-after (file-namestring wild-pos)
  (declare (simple-string file-namestring))
  (values (subseq file-namestring 0 wild-pos)
	  (subseq file-namestring (1+ wild-pos) (length file-namestring))))



;;;; Miscellaneous Utilities (e.g., MAKEDIR).

(defun make-directory (name)
  "Creates directory name.  If name exists, then an error is signaled."
  (let ((ses-name (ext:unix-namestring name nil)))
    (when (unix:unix-file-kind ses-name)
      (funcall *error-function* "Name already exists -- ~S" ses-name))
    (enter-directory ses-name))
  t)



;;;; Mach Operations

(defun open-file (ses-name)
  (multiple-value-bind (fd err)
		       (unix:unix-open ses-name unix:o_rdonly 0)
    (unless fd
      (funcall *error-function* "Opening ~S failed: ~A." ses-name err))
    fd))

(defun close-file (fd)
  (unix:unix-close fd))

(defun read-file (fd ses-name)
  (multiple-value-bind (winp dev-or-err ino mode nlink uid gid rdev size)
		       (unix:unix-fstat fd)
    (declare (ignore ino nlink uid gid rdev))
    (unless winp (funcall *error-function*
			  "Opening ~S failed: ~A."  ses-name dev-or-err))
    (let ((storage (system:allocate-system-memory size)))
      (multiple-value-bind (read-bytes err)
			   (unix:unix-read fd storage size)
	(when (or (null read-bytes) (not (= size read-bytes)))
	  (system:deallocate-system-memory storage size)
	  (funcall *error-function*
		   "Reading file ~S failed: ~A." ses-name err)))
      (values storage size mode))))

(defun write-file (ses-name data byte-count mode)
  (multiple-value-bind (fd err) (unix:unix-creat ses-name #o644)
    (unless fd
      (funcall *error-function* "Couldn't create file ~S: ~A"
	       ses-name (unix:get-unix-error-msg err)))
    (multiple-value-bind (winp err) (unix:unix-write fd data 0 byte-count)
      (unless winp
	(funcall *error-function* "Writing file ~S failed: ~A"
	       ses-name
	       (unix:get-unix-error-msg err))))
    (unix:unix-fchmod fd (logand mode #o777))
    (unix:unix-close fd)))

(defun set-write-date (ses-name secs)
  (multiple-value-bind (winp dev-or-err ino mode nlink uid gid rdev size atime)
		       (unix:unix-stat ses-name)
    (declare (ignore ino mode nlink uid gid rdev size))
    (unless winp
      (funcall *error-function* "Couldn't stat file ~S failed: ~A."
	       ses-name dev-or-err))
    (multiple-value-bind (winp err)
	(unix:unix-utimes ses-name atime 0 secs 0)
      (unless winp
	(funcall *error-function* "Couldn't set write date of file ~S: ~A"
		 ses-name (unix:get-unix-error-msg err))))))

(defun get-write-date (ses-name)
  (multiple-value-bind (winp dev-or-err ino mode nlink uid gid rdev size
			atime mtime)
 		       (unix:unix-stat ses-name)
    (declare (ignore ino mode nlink uid gid rdev size atime))
    (unless winp (funcall *error-function* "Couldn't stat file ~S failed: ~A."
			  ses-name dev-or-err))
    mtime))

;;; SUB-RENAME-FILE must exist because we can't use Common Lisp's RENAME-FILE.
;;; This is because it merges the new name with the old name to pick up
;;; defaults, and this conflicts with Unix-oid names.  For example, renaming
;;; "foo.bar" to ".baz" causes a result of "foo.baz"!  This routine doesn't
;;; have this problem.
;;;
(defun sub-rename-file (ses-name1 ses-name2)
  (multiple-value-bind (res err) (unix:unix-rename ses-name1 ses-name2)
    (unless res
      (funcall *error-function* "Failed to rename ~A to ~A: ~A."
	       ses-name1 ses-name2 (unix:get-unix-error-msg err)))))

(defun directory-existsp (ses-name)
  (eq (unix:unix-file-kind ses-name) :directory))

(defun enter-directory (ses-name)
  (declare (simple-string ses-name))
  (let* ((length-1 (1- (length ses-name)))
	 (name (if (= (position #\/ ses-name :test #'char= :from-end t)
		      length-1)
		   (subseq ses-name 0 (1- (length ses-name)))
		   ses-name)))
    (multiple-value-bind (winp err) (unix:unix-mkdir name #o755)
      (unless winp
	(funcall *error-function* "Couldn't make directory ~S: ~A"
		 name
		 (unix:get-unix-error-msg err))))))

(defun delete-directory (ses-name)
  (declare (simple-string ses-name))
  (multiple-value-bind (winp err)
		       (unix:unix-rmdir (subseq ses-name 0
						(1- (length ses-name))))
    (unless winp
      (funcall *error-function* "Couldn't delete directory ~S: ~A"
	       ses-name
	       (unix:get-unix-error-msg err)))))



;;;; Misc. Utility Utilities

;;; NSEPARATE-FILES destructively returns a list of file specs from listing.
(defun nseparate-files (listing)
  (do (files hold)
      ((null listing) files)
    (setf hold (cdr listing))
    (unless (directoryp (car listing))
      (setf (cdr listing) files)
      (setf files listing))
    (setf listing hold)))


(defun directoryp (p)
  (not (or (pathname-name p) (pathname-type p))))
