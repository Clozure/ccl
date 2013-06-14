;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   Portions copyright (C) 2001-2009 Clozure Associates
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

;;pathnames.lisp Pathnames for Coral Common LISP
(in-package "CCL")

(eval-when (eval compile)
  (require 'level-2)
  (require 'backquote)
)
;(defconstant $accessDenied -5000) ; put this with other errnos
(defconstant $afpAccessDenied -5000) ; which name to use?


#-BOOTSTRAPPED ;; get rid of this once bootstrapped
(progn
(unless (fboundp 'native-to-filename) (fset 'native-to-filename #'identity))
(unless (fboundp 'native-to-namestring) (fset 'native-to-namestring #'identity)))

;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
;ANSI CL logical pathnames


(defvar *pathname-translations-pathname*
  (make-pathname :host "ccl" :type "pathname-translations"))

(defun load-logical-pathname-translations (host)
  ;(setq host (verify-logical-host-name host))
  (when (not (%str-assoc host %logical-host-translations%))
    (setf (logical-pathname-translations host)
          (with-open-file (file (merge-pathnames (make-pathname :name host :defaults nil)
                                                 *pathname-translations-pathname*)
                                :element-type 'base-char)
            (read file)))
    T))

(defun back-translate-pathname (path &optional hosts)
  (let ((newpath (back-translate-pathname-1 path hosts)))
    (cond ((equalp path newpath)
	   ;; (fcomp-standard-source path)
	   (namestring (pathname path)))
          (t newpath))))


(defun back-translate-pathname-1 (path &optional hosts)
  (dolist (host %logical-host-translations%)
    (when (or (null hosts) (member (car host) hosts :test 'string-equal))
      (dolist (trans (cdr host))
        (when (pathname-match-p path (cadr trans))
          (let* ((src (car trans)) newpath)
            ;; Force host in backtranslated path
            (when (null (pathname-host src))
              (setq src (make-pathname :host (car host) :defaults src)))
            (setq newpath (translate-pathname path (cadr trans) src :reversible t))
            (return-from back-translate-pathname-1 
              (if  (equalp path newpath) path (back-translate-pathname-1 newpath hosts))))))))
  path)



; must be after back-translate-pathname
(defun physical-pathname-p (path)
  (let* ((path (pathname path))
         (dir (pathname-directory path)))
    (and dir
         (or (not (logical-pathname-p path))
             (not (null (memq (pathname-host path) '(nil :unspecific))))))))



;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
;File or directory Manipulations

(defun unix-rename (old-name new-name)
  (with-filename-cstrs ((old old-name)
			(new new-name))
    #+windows-target
    (#__wunlink new)
    (let* ((res #-windows-target (#_rename old new)
		#+windows-target (#__wrename old new)))
      (declare (fixnum res))
      (if (zerop res)
        (values t nil)
        (values nil (%get-errno))))))

(defun rename-file (file new-name &key (if-exists :error))
  "Rename FILE to have the specified NEW-NAME. If FILE is a stream open to a
  file, then the associated file is renamed."
  (let* ((original (truename file))
	 (original-namestring (defaulted-native-namestring original))
	 (new-name (merge-pathnames new-name (merge-pathnames file)))
	 (new-namestring (defaulted-native-namestring new-name)))
    (unless new-namestring
      (error "~S can't be created." new-name))
    (unless (and (probe-file new-name)
		 (not (if-exists if-exists new-name)))
      (multiple-value-bind (res error)
	                   (unix-rename original-namestring
					new-namestring)
	(unless res
	  (error "Failed to rename ~A to ~A: ~A"
		 original new-name error))
	(when (streamp file)
	  (setf (stream-filename file)
		(namestring (native-to-pathname new-namestring))))
	(values new-name original (truename new-name))))))

(defun copy-file (source-path dest-path &key (if-exists :error) (if-does-not-exist :create)
			      (preserve-attributes nil))
  (let* ((original (truename source-path))
	 (new-name (merge-pathnames dest-path original))
         (buffer (make-array 4096 :element-type '(unsigned-byte 8))))
    (with-open-file (in original :direction :input
                        :element-type '(unsigned-byte 8))
      (with-open-file (out new-name :direction :output
                           :if-exists if-exists
                           :if-does-not-exist if-does-not-exist
                           :element-type '(unsigned-byte 8))
        (when (null out) (return-from copy-file nil)) ;; :if-exists nil
        (loop
          as n = (stream-read-vector in buffer 0 4096) until (eql n 0)
          do (stream-write-vector out buffer 0 n))))
    (when preserve-attributes
      (copy-file-attributes original new-name))
    (values new-name original (truename new-name))))

(defun recursive-copy-directory (source-path dest-path &key test (if-exists :error))
  ;; TODO: Support :if-exists :supersede to blow away any files not in source dir
  (assert (directoryp source-path)(source-path)
          "source-path is not a directory in RECURSIVE-COPY-DIRECTORY")
  (setq if-exists (require-type if-exists '(member :overwrite :error)))
  (setq dest-path (ensure-directory-pathname dest-path))
  (when (eq if-exists :error)
    (when (probe-file dest-path)
      (if-exists if-exists dest-path))
    ;; Skip the probe-file in recursive calls, we already know it's ok.
    (setq if-exists :overwrite))
  (let* ((source-dir (ensure-directory-pathname source-path))
         (pattern (make-pathname :name :wild :type :wild :defaults source-dir))
         (source-files (directory pattern :test test :directories t :files t)))
    (ensure-directories-exist dest-path)
    (dolist (f source-files)
      (when (or (null test) (funcall test f))
        (if (directory-pathname-p f)
            (let ((dest-file (make-pathname :name (first (last (pathname-directory f)))
                                            :defaults dest-path)))
              (recursive-copy-directory f dest-file :test test :if-exists if-exists))
            (let* ((dest-file (make-pathname :name (pathname-name f)
                                             :type (pathname-type f)
                                             :defaults dest-path)))
              (copy-file f dest-file :if-exists :supersede :preserve-attributes t)))))))

(defun delete-empty-directory (path)
  (let* ((namestring (defaulted-native-namestring path))
	 (err (%rmdir namestring)))
    (or (eql 0 err) (signal-file-error err path))))

(defun delete-directory (path)
  "Delete specified directory and all its contents."
  (let ((namestring (defaulted-native-namestring path)))
    (if (eq :directory (%unix-file-kind namestring t))
      (let* ((dir (ensure-directory-pathname path))
	     (wild (make-pathname :name :wild :type :wild :defaults dir))
	     (files (directory wild :directories nil :files t
			       :follow-links nil :include-emacs-lockfiles t))
	     (subdirs (directory wild :directories t :files nil
				 :follow-links nil
				 :include-emacs-lockfiles t)))
	(dolist (f files)
	  (delete-file f))
	(dolist (d subdirs)
	  (delete-directory d))
	(delete-empty-directory path))
      (error "~s is not a directory" path))))

;;; use with caution!
;;; blows away a directory and all its contents
(defun recursive-delete-directory (path &key (if-does-not-exist :error))
  (setq path (make-pathname :name nil :type nil
                            :defaults (merge-pathnames (ensure-directory-pathname path))))
  (setq if-does-not-exist (require-type if-does-not-exist '(member :error nil)))
  (when (eq if-does-not-exist :error)
    (unless (probe-file path)
      (if-does-not-exist if-does-not-exist path)))
  (when (probe-file path)
      (if (directoryp path)
	  ;; it's a directory: blow it away
	  (let* ((pattern (make-pathname :name :wild :type :wild :defaults path))
		 (files (directory pattern :directories nil :files t))
		 (subdirs (directory pattern :directories t :files nil))
		 (target-pathname (defaulted-native-namestring path)))
	    (dolist (f files)
	      (delete-file f))
	    (dolist (d subdirs)
	      (recursive-delete-directory d :if-does-not-exist if-does-not-exist))
	    (%rmdir target-pathname))
	  ;; it's not a directory: for safety's sake, signal an error
	  (error "Pathname '~A' is not a directory" path))))

;;; It's not clear that we can support anything stronger than
;;; "advisory" ("you pretend the file's locked & I will too") file
;;; locking under Darwin.




(defun create-directory (path &key (mode #o777))
  (let* ((pathname (translate-logical-pathname (merge-pathnames path)))
	 (created-p nil)
	 (parent-dirs (let* ((pd (pathname-directory pathname)))
			(if (eq (car pd) :relative)
			  (pathname-directory (merge-pathnames
					       pathname
					       (mac-default-directory)))
			  pd)))
	 (nparents (length parent-dirs)))
    (when (wild-pathname-p pathname)
      (error 'file-error :error-type "Inappropriate use of wild pathname ~s"
	     :pathname pathname))
    (do* ((i 1 (1+ i)))
	 ((> i nparents) (values pathname created-p))
      (declare (fixnum i))
      (let* ((parent (make-pathname
		      :name :unspecific
		      :type :unspecific
		      :host (pathname-host pathname)
		      :device (pathname-device pathname)
		      :directory (subseq parent-dirs 0 i)))
	     (parent-name (defaulted-native-namestring parent))
	     (parent-kind (%unix-file-kind parent-name)))

	(if parent-kind
	  (unless (eq parent-kind :directory)
	    (error 'simple-file-error
		   :error-type "Can't create directory ~s, since file ~a exists and is not a directory"
		   :pathname pathname
		   :format-arguments (list parent-name)))
	  (let* ((result (%mkdir parent-name mode)))
	    (declare (fixnum result))
	    (if (< result 0)
	      (signal-file-error result parent-name)
	      (setq created-p t))))))))


(defun ensure-directories-exist (pathspec &key verbose (mode #o777))
  "Test whether the directories containing the specified file
  actually exist, and attempt to create them if they do not.
  The MODE argument is an extension to control the Unix permission
  bits.  Portable programs should avoid using the :MODE keyword
  argument."
  (let ((pathname (let ((pathspec (translate-logical-pathname (merge-pathnames pathspec))))
		    (make-directory-pathname :device (pathname-device pathspec)
					     :directory (pathname-directory pathspec))))
	(created-p nil))
    (when (wild-pathname-p pathname)
      (error 'file-error
	     :error-type "Inappropriate use of wild pathname ~s"
	     :pathname pathname))
    (let ((dir (pathname-directory pathname)))
      (if (eq (car dir) :relative)
	(setq dir (pathname-directory (merge-pathnames
				       pathname
				       (mac-default-directory)))))
      (loop for i from 1 upto (length dir)
	    do (let ((newpath (make-pathname
			       :name :unspecific
			       :type :unspecific
			       :host (pathname-host pathname)
			       :device (pathname-device pathname)
			       :directory (subseq dir 0 i))))
		 (unless (probe-file newpath)
		   (let ((namestring (native-translated-namestring newpath)))
		     (when verbose
		       (format *standard-output* "~&Creating directory: ~A~%"
			       namestring))
		     (%mkdir namestring mode)
		     (unless (probe-file newpath)
		       (error 'file-error
			      :pathname namestring
			      :error-type "Can't create directory ~S."))
		     (setf created-p t)))))
      (values pathspec created-p))))

(defun dirpath-to-filepath (path)
  (setq path (translate-logical-pathname (merge-pathnames path)))
  (let* ((dir (pathname-directory path))
         (super (butlast dir))
         (name (car (last dir))))
    (when (eq name :up)
      (setq dir (remove-up (copy-list dir)))
      (setq super (butlast dir))
      (setq name (car (last dir))))
    (when (null super)
      (signal-file-error $xnocreate path))
    (setq path (make-pathname :directory super :name name :defaults nil))))

(defun filepath-to-dirpath (path)
  (let* ((dir (pathname-directory path))
         (rest (file-namestring path)))
    (make-pathname :directory (append dir (list rest)) :defaults nil)))
  


;Takes a pathname, returns the truename of the directory if the pathname
;names a directory, NIL if it names an ordinary file, error otherwise.
;E.g. (directoryp "ccl;:foo:baz") might return #P"hd:mumble:foo:baz:" if baz
;is a dir. - should we doc this - its exported?
(defun directoryp (path)
  (let* ((native (defaulted-native-namestring path))
	 (realpath (%realpath native)))
    (if realpath (eq (%unix-file-kind realpath) :directory))))
	 

;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
;Wildcards



 
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
;Directory Traversing

(defun %path-cat (device dir subdir)
  (if device
      (%str-cat device ":" dir subdir)
    (%str-cat dir subdir)))

(defmacro with-open-dir ((dirent device dir state follow-links) &body body)
  (let* ((namestring (gensym)))
    `(let* ((,namestring (native-translated-namestring (make-pathname :device ,device :directory ,dir :defaults nil))))
      (when (%new-directory-p ,namestring ,follow-links ,state)
        (let* ((,dirent (%open-dir ,namestring)))
          (when ,dirent
            (unwind-protect
                 (progn ,@body)
              (close-dir ,dirent))))))))

(defun path-is-link (path)
  "Returns T if PATH is a (hard or symbolic) link, NIL otherwise."
  ;; Actually, it's a bit more subtle than that; it basically
  ;; returns information about the last component of PATH.  If
  ;; some enclosing directory name is a link but the last component
  ;; isn't, this'll return false.
  (eq (%unix-file-kind (native-translated-namestring path) t) :link))

(defstruct (directory-result (:constructor %make-directory-result))
  (truenames (make-hash-table :shared nil :test 'string= :hash-function 'sxhash))
  (directories-seen ()))


;;; If no component of the pathname involves a link we could avoid the call to
;;; TRUENAME here.  Later ...
(defun %add-directory-result (path result follow-links &optional followed-some-links)
  (declare (ignore followed-some-links))
  (let* ((truename (if follow-links (truename path) path))
         (namestring (namestring truename))
         (truenames (directory-result-truenames result)))
    (or (gethash namestring truenames)
        (setf (gethash namestring truenames) truename))))
    

(defun %process-directory-result (result)
  (collect ((pairs))
    (maphash (lambda (namestring truename) (pairs (cons namestring truename))) (directory-result-truenames result))
    (mapcar #'cdr (sort (pairs) #'string< :key #'car))))

(defun %new-directory-p (native-namestring follow-links result)
  (multiple-value-bind (win mode size mtime inode uid blocksize rmtime  gid dev)
      (%stat native-namestring (not follow-links))
    (declare (ignore size mtime uid blocksize rmtime gid #+windows-target inode #+windows-target dev))
    (when (and win (= (logand mode #$S_IFMT) #$S_IFDIR))
      #+windows-target
      (let* ((dirname (namestring (truename (native-to-pathname native-namestring)))))
        (unless (member dirname (directory-result-directories-seen result) :test #'string=)
          (push dirname (directory-result-directories-seen result))
          t))
      #-windows-target
      (when (dolist (pair (directory-result-directories-seen result) t)
              (when (and (eql inode (car pair))
                         (eql dev (cdr pair)))
                (return)))
        (push (cons inode dev) (directory-result-directories-seen result))
        t))))

  
(defun directory (path &key (directories t) ;; include subdirectories
                            (files t)         ;; include files
			    (all t)           ;; include Unix dot files (other than dot and dot dot)
			    (directory-pathnames t) ;; return directories as directory-pathname-p's.
                            (include-emacs-lockfiles nil) ;; inculde .#foo
			    test              ;; Only return pathnames matching test
			    (follow-links t)) ;; return truename's of matching files.
  "Return a list of PATHNAMEs, each the TRUENAME of a file that matched the
   given pathname. Note that the interaction between this ANSI-specified
   TRUENAMEing and the semantics of the Unix filesystem (symbolic links..)
   means this function can sometimes return files which don't have the same
   directory as PATHNAME."
  (let* ((keys (list :directories directories ;list defaulted key values
		     :files files
		     :all all
		     :directory-pathnames directory-pathnames
		     :test test
                     :include-emacs-lockfiles include-emacs-lockfiles
		     :follow-links follow-links))
	 (path (full-pathname (merge-pathnames path) :no-error nil))
	 (dir (directory-namestring path)))
    (declare (dynamic-extent keys))
    (if (null (pathname-directory path))
      (setq dir (directory-namestring (setq path
					    (merge-pathnames path
							     (mac-default-directory))))))
    (assert (eq (car (pathname-directory path)) :absolute) ()
	    "full-pathname returned relative path ~s??" path)
    (%process-directory-result (%directory "/" dir path '(:absolute) keys (%make-directory-result)))))

(defun %directory (native-dir rest path so-far keys result)
  (multiple-value-bind (native-sub-dir wild rest) (%split-dir rest)
    (%some-specific native-dir native-sub-dir wild rest path so-far keys result)))

(defun %some-specific (native-dir native-sub-dir wild rest path so-far keys result)
  (let* ((start 1)
	 (end (length native-sub-dir))
	 (native-full-dir (if (eq start end) native-dir (%str-cat native-dir (%substr native-sub-dir start end)))))
    (while (neq start end)
      (let ((pos (position #\/ native-sub-dir :start start :end end)))
	(push (native-to-filename (%substr native-sub-dir start pos)) so-far)
	(setq start (%i+ 1 pos))))
    (cond ((null wild)
	   (%files-in-directory native-full-dir path so-far keys result))
	  ((string= wild "**")
	   (%all-directories native-full-dir rest path so-far keys result))
	  (t (%one-wild native-full-dir wild rest path so-far keys result)))))

; for a * or *x*y
(defun %one-wild (native-dir wild rest path so-far keys result)
  (let ((device (pathname-device path))
	(all (getf keys :all))
        (follow-links (getf keys :follow-links))
        (dir (native-to-namestring native-dir))
	name)
    (with-open-dir (dirent device dir result follow-links)
      (while (setq name (%read-dir dirent))
	(when (and (or all (neq (%schar name 0) #\.))
		   (not (string= name "."))
		   (not (string= name ".."))
		   (%path-str*= name wild)
		   (eq (%unix-file-kind (%path-cat device native-dir name) (not follow-links)) :directory))
	  (let ((native-subdir (%path-cat nil native-dir name))
                (so-far (cons (native-to-filename name) so-far)))
	    (declare (dynamic-extent so-far))
            (%directory (%str-cat native-subdir "/") rest path so-far keys result)))))
    result))

(defun %files-in-directory (native-dir path so-far keys result)
  (let ((device (pathname-device path))
        (name (pathname-name path))
        (type (pathname-type path))
	(directories (getf keys :directories))
	(files (getf keys :files))
	(directory-pathnames (getf keys :directory-pathnames))
	(test (getf keys :test))
	(follow-links (getf keys :follow-links))
	(all (getf keys :all))
        (include-emacs-lockfiles (getf keys :include-emacs-lockfiles))
        (dir (native-to-namestring native-dir))
        native-sub dir-list ans)
    (if (not (or name type))
      (let (full-path)
	(when (and directories
		   (eq (%unix-file-kind (native-translated-namestring (setq full-path (%cons-pathname (reverse so-far) nil nil nil device)))
					(not follow-links))
		       :directory))
	  (setq ans (if directory-pathnames full-path
		      (%cons-pathname (reverse (cdr so-far)) (car so-far) nil nil device)))
	  (when (and ans (or (null test) (funcall test ans)))
            (%add-directory-result ans result follow-links))))
      (with-open-dir (dirent (pathname-device path) dir result follow-links)
	(while (setq native-sub (%read-dir dirent))
	  (when (and (or all (neq (%schar native-sub 0) #\.))
                     (or include-emacs-lockfiles
                         (< (length native-sub) 2)
                         (not (string= native-sub ".#" :end1 2)))
		     (not (string= native-sub "."))
		     (not (string= native-sub ".."))
		     (%file*= name type native-sub))
	    (setq ans
		  (if (eq (%unix-file-kind (%path-cat device native-dir native-sub) (not follow-links)) :directory)
		    (when directories
		      (let* ((std-sub (native-to-filename native-sub)))
			(if directory-pathnames
			  (%cons-pathname (reverse (cons std-sub so-far)) nil nil nil device)
			  (%cons-pathname (or dir-list (setq dir-list (reverse so-far))) std-sub nil nil device))))
		    (when files
		      (multiple-value-bind (name type) (%std-name-and-type native-sub)
			(%cons-pathname (or dir-list (setq dir-list (reverse so-far))) name type nil device)))))
	    (when (and ans (or (null test) (funcall test ans)))
	      (%add-directory-result ans result follow-links))))))
    result))

(defun %all-directories (native-dir rest path so-far keys result)
  (let ((do-files nil)
        (do-dirs nil)
        (device (pathname-device path))
        (name (pathname-name path))
        (type (pathname-type path))
	(all (getf keys :all))
	(test (getf keys :test))
	(directory-pathnames (getf keys :directory-pathnames))
	(follow-links (getf keys :follow-links))
	sub native-sub dir-list ans)
    ;; First process the case that the ** stands for 0 components
    (multiple-value-bind (native-next-dir next-wild next-rest) (%split-dir rest)
      (while (and next-wild ; Check for **/**/ which is the same as **/
		  (string= native-next-dir "/")
		  (string= next-wild "**"))
        (setq rest next-rest)
        (multiple-value-setq (native-next-dir next-wild next-rest) (%split-dir rest)))
      (cond ((not (string= native-next-dir "/"))
	     (%some-specific native-dir native-next-dir next-wild next-rest path so-far keys result))
	    (next-wild
             (%one-wild native-dir next-wild next-rest path so-far keys result))
	    ((or name type)
	     (when (getf keys :files) (setq do-files t))
	     (when (getf keys :directories) (setq do-dirs t)))
	    (t (when (getf keys :directories)
		 (setq sub (if directory-pathnames
			     (%cons-pathname (setq dir-list (reverse so-far)) nil nil nil device)
			     (%cons-pathname (reverse (cdr so-far)) (car so-far) nil nil device)))
		 (when (or (null test) (funcall test sub))
                   (%add-directory-result sub result follow-links))))))
    (unless (or do-dirs do-files) (return-from %all-directories nil))
    ;; now descend doing %all-dirs on dirs and collecting files & dirs
    ;; if do-x is t
    (with-open-dir (dirent device (native-to-namestring native-dir) result follow-links)
      (while (setq native-sub (%read-dir dirent))
        (when (and (or all (neq (%schar native-sub 0) #\.))
                   (not (string= native-sub "."))
		   (not (string= native-sub "..")))
	  (if (eq (%unix-file-kind (%path-cat device native-dir native-sub) (not follow-links)) :directory)
	    (let* ((native-subfile (%path-cat nil native-dir native-sub))
		   (std-sub (native-to-filename native-sub))
		   (so-far (cons std-sub so-far))
		   (native-subdir (%str-cat native-subfile  "/")))
	      (declare (dynamic-extent so-far))
	      (when (and do-dirs (%file*= name type native-sub))
		(setq ans (if directory-pathnames
			    (%cons-pathname (reverse so-far) nil nil nil device)
			    (%cons-pathname (or dir-list (setq dir-list (reverse (cdr so-far))))
					    std-sub nil nil device)))
		(when (or (null test) (funcall test ans))
		  (%add-directory-result ans result follow-links)))
	      (%all-directories native-subdir rest path so-far keys result))
	    (when (and do-files (%file*= name type native-sub))
	      (multiple-value-bind (name type) (%std-name-and-type native-sub)
		(setq ans (%cons-pathname (or dir-list (setq dir-list (reverse so-far))) name type nil device))
		(when (or (null test) (funcall test ans))
		  (%add-directory-result ans result follow-links))))))))
    result))

(defun %split-dir (dir &aux pos)                 ; dir ends in a "/".
  ;"/foo/bar/../x*y/baz/../z*t/"  ->  "/foo/bar/../" "x*y" "/baz/../z*t/",
  ;  where the first value is native, second and third are escaped.
  (if (null (setq pos (%path-mem "*" dir)))
    (if (%path-mem-last-quoted "/" dir)
      (signal-file-error $xbadfilenamechar dir #\/)
      (values (namestring-unquote dir) nil nil))
    (let (epos (len (length dir)))
      (setq pos (if (setq pos (%path-mem-last "/" dir 0 pos)) (%i+ pos 1) 0)
            epos (%path-mem "/" dir pos len))
      (when (%path-mem-last-quoted "/" dir 0 pos)
	(signal-file-error $xbadfilenamechar dir #\/))
      (values (unless (%izerop pos) (namestring-unquote (%substr dir 0 pos)))
              (%substr dir pos epos)
              (%substr dir epos len)))))

(defun %file*= (name-pat type-pat pstr)
  (when (and (eq name-pat :wild)
             (or (null type-pat)
                 (eq type-pat :unspecific)))
    (setq type-pat :wild))
  (if (eq name-pat :wild) (setq name-pat "*"))
  (if (eq type-pat :wild) (setq type-pat "*"))
  (when (and (null name-pat) (null type-pat))
    (return-from %file*= T))
  (let* ((end (length pstr))
	 (pos (position #\. pstr :from-end t))
	 (type (and pos (%substr pstr (%i+ pos 1) end)))
	 (name (unless (eq (or pos end) 0) (if pos (%substr pstr 0 pos) pstr))))
    (and (cond ((or (eq name-pat :unspecific) (null name-pat)) (null name))
	       (t (%path-str*= (or name "") name-pat)))
	 (cond ((or (null type-pat) (eq type-pat :unspecific)) (null type))
	       (t (%path-str*= (or type "") type-pat))))))

(provide "PATHNAMES")
