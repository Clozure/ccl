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

;; L1-files.lisp - Object oriented file stuff

(in-package "CCL")

(defconstant $paramErr -50)   ; put this with the rest when we find the rest

(defconstant pathname-case-type '(member :common :local :studly))
(defconstant pathname-arg-type '(or string pathname stream))

(defmacro signal-file-error (err-num &optional pathname &rest args)
  `(%signal-file-error ,err-num
    ,@(if pathname (list pathname))
              ,@(if args args)))

(defun %signal-file-error (err-num &optional pathname args)
  (declare (fixnum err-num))
  (let* ((err-code (logior (ash 2 16) (the fixnum (logand #xffff (the fixnum err-num))))))
    (funcall (if (< err-num 0) '%errno-disp '%err-disp)
	     err-code
	     pathname
	     args)))


(defvar %logical-host-translations% '())
(defvar *load-pathname* nil
  "the defaulted pathname that LOAD is currently loading")
(defvar *load-truename* nil
  "the TRUENAME of the file that LOAD is currently loading")


(defparameter *default-pathname-defaults*
  (let* ((hide-from-compile-file (%cons-pathname nil nil nil)))
    hide-from-compile-file))

;Right now, the only way it's used is that an explicit ";" expands into it.
;Used to merge with it before going to ROM.  Might be worth to bring that back,
;it doesn't hurt anything if you don't set it.
;(defparameter *working-directory* (%cons-pathname nil nil nil))

;These come in useful...  We should use them consistently and then document them,
;thereby earning the eternal gratitude of any users who find themselves with a
;ton of "foo.CL" files...
(defparameter *.fasl-pathname*
  (%cons-pathname nil nil
                  #.(pathname-type
                     (backend-target-fasl-pathname *target-backend*))))

(defparameter *.lisp-pathname* (%cons-pathname nil nil "lisp"))

(defun if-exists (if-exists filename &optional (prompt "Create ..."))
  (case if-exists
    (:error (signal-file-error (- #$EEXIST) filename))
    ((:dialog) (overwrite-dialog filename prompt))
    ((nil) nil)
    ((:ignored :overwrite :append :supersede :rename-and-delete :new-version :rename) filename)
    (t (report-bad-arg if-exists '(member :error :dialog nil :ignored :overwrite :append :supersede :rename-and-delete)))))

(defun if-does-not-exist (if-does-not-exist filename)
  (case if-does-not-exist 
    (:error (signal-file-error (- #$ENOENT) filename)) ; (%err-disp $err-no-file filename))
    (:create filename)
    ((nil) (return-from if-does-not-exist nil))
    (t (report-bad-arg if-does-not-exist '(member :error :create nil)))))


(defun defaulted-native-namestring (path)
  (native-translated-namestring (merge-pathnames path)))

(defun native-translated-namestring (path)
  (let ((name (let ((*default-pathname-defaults* #P""))
                (translated-namestring path))))
    ;; Check that no quoted /'s
    (when (%path-mem-last-quoted "/" name)
      (signal-file-error $xbadfilenamechar name #\/))
    ;; Check that no unquoted wildcards.
    (when (%path-mem-last "*" name)
      (signal-file-error $xillwild name))
    (namestring-unquote name)))

(defun native-untranslated-namestring (path)
  (let ((name (namestring (translate-logical-pathname path))))
    ;; Check that no quoted /'s
    (when (%path-mem-last-quoted "/" name)
      (signal-file-error $xbadfilenamechar name #\/))
    ;; Check that no unquoted wildcards.
    (when (%path-mem-last "*" name)
      (signal-file-error $xillwild name))
    (namestring-unquote name)))

;; Reverse of above, take native namestring and make a Lisp pathname.
(defun native-to-pathname (name)
  ;; This assumes that NAME is absolute and fully qualified, and
  ;; that there'd be no benefit to (and some risk involved in)
  ;; effectively merging it with whatever random thing may be
  ;; in *DEFAULT-PATHNAME-DEFAULTS*.
  ;; I -think- that that's true for all callers of this function.
  (let* ((*default-pathname-defaults* #p""))
    (pathname (%path-std-quotes name nil
                                   #+windows-target "*;"
                                   #-windows-target "*;:"))))

(defun native-to-directory-pathname (name)
  #+windows-target
  (let* ((len (length name)))
    (when (and (> len 1) (not (or (eql (schar name (1- len)) #\/)
                                  (eql (schar name (1- len)) #\\))))
      (setq name (%str-cat name "/")))
    (string-to-pathname name))
  #-windows-target
  (make-directory-pathname  :device nil :directory (%path-std-quotes name nil "*;:")))

;;; Make a pathname which names the specified directory; use
;;; explict :NAME, :TYPE, and :VERSION components of NIL.
(defun make-directory-pathname (&key host device directory)
  (make-pathname :host host
		 :device device
		 :directory directory
                 :name nil
                 :type nil
                 :version nil))

		   
(defun namestring-unquote (name)
  (let ((esc *pathname-escape-character*))
    (multiple-value-bind (sstr start end) (get-sstring name)
      (declare (simple-string sstr) (fixnum start end))
      (let* ((ncopy 0)
             (skipped nil)
             (quote-next nil))
        (declare (fixnum ncopy))
        (do* ((i start (1+ i)))
             ((= i end))
          (declare (fixnum i))
          (let* ((ch (schar sstr i)))
            (cond ((or quote-next (not (eq ch esc)))
                   (incf ncopy)
                   (setq quote-next nil))
                  ((eq ch esc) (setq skipped t) (setq quote-next t)))))
        (if (not skipped)
          name
          (let ((result (make-string ncopy))
                (dest 0))
            (declare (fixnum dest))
            (setq quote-next nil)
            (do* ((i start (1+ i)))
                 ((= i end) result)
              (declare (fixnum i))
              (let* ((ch (schar sstr i)))
                (cond ((or quote-next (not (eq ch esc)))
                       (setf (schar result dest) ch)
                       (incf dest)
                       (setq quote-next nil))
                      ((eq ch esc) (setq quote-next t)))))))))))

(defun translated-namestring (path)
  (namestring (translate-logical-pathname (merge-pathnames path))))


(defun truename (path)
  "Return the pathname for the actual file described by PATHNAME.
  An error of type FILE-ERROR is signalled if no such file exists,
  or the pathname is wild.

  Under Unix, the TRUENAME of a broken symlink is considered to be
  the name of the broken symlink itself."
  (or (probe-file path)
      (signal-file-error $err-no-file path)))

(defun check-pathname-not-wild (path)
  (when (wild-pathname-p path)
    (error 'file-error :error-type "Inappropriate use of wild pathname ~s"
	   :pathname path))
  path)

(defun probe-file (path)
  "Return a pathname which is the truename of the file if it exists, or NIL
  otherwise. An error of type FILE-ERROR is signaled if pathname is wild."
  (check-pathname-not-wild path)
  (let* ((native (defaulted-native-namestring path))
         (realpath (%realpath native))
         (kind (if realpath (%unix-file-kind realpath))))
    ;; Darwin's #_realpath will happily return non-nil for
    ;; files that don't exist.  I don't think that
    ;; %UNIX-FILE-KIND would do so.
    (when kind
      (if (eq kind :directory)
          (unless (eq (aref realpath (1- (length realpath))) #\/)
            (setq realpath (%str-cat realpath "/"))))
      (if realpath
        (native-to-pathname realpath)
        nil))))

(defun cwd (path)  
  (multiple-value-bind (realpath kind) (%probe-file-x (defaulted-native-namestring path))
    (if kind
      (if (eq kind :directory)
	(let* ((error (%chdir realpath)))
	  (if (eql error 0)
	    (mac-default-directory)
	    (signal-file-error error path)))
	(error "~S is not a directory pathname." path))
      (error "Invalid pathname : ~s." path))))

(defun create-file (path &key (if-exists :error) (create-directory t))
  (let* ((p (%create-file path :if-exists if-exists
				      :create-directory create-directory)))
    (and p
         (native-to-pathname p))))

(defun %create-file (path &key
			 (if-exists :error)
			 (create-directory t))
  (when create-directory
    (create-directory path))
  (when (directory-pathname-p path)
    (return-from %create-file (probe-file-x path)))
  (let* ((unix-name (defaulted-native-namestring path))
	 (fd (fd-open unix-name (logior #$O_WRONLY #$O_CREAT
                                        (if (eq if-exists :overwrite)
                                          #$O_TRUNC
                                          #$O_EXCL)))))
    (when (and (neq if-exists :error)
               (or (eql fd (- #$EEXIST))
                   #+windows-target
                   (and (eql fd (- #$EPERM))
                        (probe-file path))))
      (when (null if-exists)
        (return-from %create-file nil))
      (error "~s ~s not implemented yet" :if-exists if-exists))

    (if (< fd 0)
      (signal-file-error fd path)
      (fd-close fd))
    (%realpath unix-name)))


;; The following assumptions are deeply embedded in all our pathname code:
;; (1) Non-logical pathname host is always :unspecific.
;; (2) Logical pathname host is never :unspecific.
;; (3) Logical pathname host can however be NIL, e.g. "foo;bar;baz".

(defun %pathname-host (pathname)
  (if (logical-pathname-p pathname)
      (%logical-pathname-host pathname)
      :unspecific))

(defun %pathname-version (pathname)
  (if (logical-pathname-p pathname)
    (%logical-pathname-version pathname)
    (%physical-pathname-version pathname)))



(fset 'pathname-host (nfunction bootstrapping-pathname-host   ; redefined later in this file
                                (lambda (thing)
                                  (declare (ignore thing))
                                  :unspecific)))

(fset 'pathname-version (nfunction bootstrapping-pathname-version   ; redefined later in this file
                                   (lambda (thing)
                                     (declare (ignore thing))
                                     nil)))

(defmethod print-object ((pathname pathname) stream)
  (let ((flags (if (logical-pathname-p pathname) 4
                   (%i+ (if (eq (%pathname-type pathname) ':unspecific) 1 0)
                        (if (equal (%pathname-name pathname) "") 2 0))))
        (name (namestring pathname)))
    (if (and (not *print-readably*) (not *print-escape*))
      (write-string name stream)
      (progn
        (format stream (if (or *print-escape* (eql flags 0)) "#P" "#~DP") flags)
        (write-escaped-string name stream #\")))))


(defun mac-default-directory ()
  (let* ((native-name (current-directory-name))
	 (len (length native-name)))
    (declare (fixnum len))
    (when (and (> len 1)
	       (not (eq #\/ (schar native-name (1- len)))))
      (setq native-name (%str-cat native-name "/")))
    (native-to-pathname native-name)))




;;; I thought I wanted to call this from elsewhere but perhaps not
(defun absolute-directory-list (dirlist)
  ; just make relative absolute and remove ups where possible
  (when (or (null dirlist) (eq (car dirlist) :relative))
    (let ((default (mac-default-directory)) default-dir)
      (when default
        (setq default-dir (%pathname-directory default))
        (when default-dir
          (setq dirlist (append default-dir (cdr dirlist)))))))
  (when (memq :up dirlist)
    (setq dirlist (remove-up (copy-list dirlist))))
  dirlist)

; destructively mungs dir
(defun remove-up (dir)
  (setq dir (delete "." dir  :test #'string=))
  (let ((n 0)
        (last nil)
        (sub dir)
        has-abs kept-up)
    ;; from %std-directory-component we get dir with :relative/:absolute stripped
    (when (memq :up dir)
      (when (memq (car dir) '(:relative :absolute))
	(setq sub (cdr dir) n 1 has-abs t))
      (do () ((null sub))
	(cond ((eq (car sub) :up)
	       (cond ((or (eq n 0)
			  (and (stringp last)(string= last "**"))
			  (eq last :wild-inferiors)
			  kept-up
			  (and has-abs (eq n 1)))
		      ;; up after "**" stays, initial :up stays, how bout 2 :ups
		      (setq kept-up t)
		      )
		     ((eq n 1) (setq dir (cddr dir) kept-up nil n -1))
		     (t (rplacd (nthcdr (- n 2) dir) (cdr sub))
			(setq n (- n 2) kept-up nil))))
	      (t (setq kept-up nil)))
	(setq last (car sub)
	      n (1+ n) 
	      sub (cdr sub))))
    dir))

(defun namestring (path)
  "Construct the full (name)string form of the pathname."
  (%str-cat (device-namestring path)
            (host-namestring path)
	    (directory-namestring path)
	    (file-namestring path)))

(defun device-namestring (path)
  (let* ((device (pathname-device path)))
    (if (and device (not (eq device :unspecific)))
      (%str-cat device ":")
      "")))

(defun host-namestring (path)
  "Return a string representation of the name of the host in the pathname."
  (let ((host (pathname-host path)))
    (if (and host (neq host :unspecific)) (%str-cat host ":") "")))

(defun directory-namestring (path)
  "Return a string representation of the directories used in the pathname."
  (%directory-list-namestring (pathname-directory path)
			      (neq (pathname-host path) :unspecific)))

(defun ensure-directory-namestring (string)
  (namestring (ensure-directory-pathname string)))

(defun ensure-directory-pathname (pathname)
  (let ((path (pathname pathname)))
    (if (directory-pathname-p path)
	path
	(cons-pathname (append (or (pathname-directory path)
				   ;; This makes sure "ccl:foo" maps to "ccl:foo;" (not
				   ;; "ccl:;foo;"), but "foo" maps to "foo/" (not "/foo/").
				   (if (eq (pathname-host path) :unspecific)
				       '(:relative)
				       '(:absolute)))
			       ;; Don't use file-namestring, because that
			       ;; includes the version for logical names.
			       (list (file-namestring-from-parts
				      (pathname-name path)
				      (pathname-type path)
				      nil)))
		       nil nil (pathname-host path) nil #+windows-target (pathname-device path)))))

(defun %directory-list-namestring (list &optional logical-p)
  (if (null list)
    ""
    (let ((len (if (eq (car list) (if logical-p :relative :absolute)) 1 0))

          result)
      (declare (fixnum len)(optimize (speed 3)(safety 0)))
      (dolist (s (%cdr list))
        (case s
          (:wild (setq len (+ len 2)))
          (:wild-inferiors (setq len (+ len 3)))
          (:up (setq len (+ len 3)))
          (t ;This assumes that special chars in dir components are escaped,
	     ;otherwise would have to pre-scan for escapes here.
	   (setq len (+ len 1 (length s))))))
      (setq result
	    (make-string len))
      (let ((i 0)
            (sep (if logical-p #\; #\/)))
        (declare (fixnum i))
        (when (eq (%car list) (if logical-p :relative :absolute))
          (setf (%schar result 0) sep)
          (setq i 1))
        (dolist (s (%cdr list))
	  (case s
	    (:wild (setq s "*"))
	    (:wild-inferiors (setq s "**"))
	    ;; There is no :up in logical pathnames, so this must be native
	    (:up (setq s "..")))
	  (let ((len (length s)))
	    (declare (fixnum len))
	    (move-string-bytes s result 0 i len)
	    (setq i (+ i len)))
	  (setf (%schar result i) sep)
	  (setq i (1+ i))))
      result)))

(defun file-namestring (path)
  "Return a string representation of the name used in the pathname."
  (let* ((path (pathname path))
         (name (pathname-name path))
         (type (pathname-type path))
         (version (if (typep path 'logical-pathname) (pathname-version path))))
    (file-namestring-from-parts name type version)))

(defun file-namestring-from-parts (name type version)
  (when (eq version :unspecific) (setq version nil))
  (when (eq type :unspecific) (setq type nil))
  (%str-cat (case name
	      ((nil :unspecific) "")
	      (:wild "*")
	      (t (%path-std-quotes name "*;:" ".")))
	    (if (or type version)
	      (%str-cat (case type
			  ((nil) ".")
			  (:wild ".*")
			  (t (%str-cat "." (%path-std-quotes type "*;:" "."))))
			(case version
			  ((nil) "")
			  (:newest ".newest")
			  (:wild ".*")
			  (t (%str-cat "." (if (fixnump version)
					     (%integer-to-string version)
					     version)))))
	      "")))

(defun enough-namestring (path &optional (defaults *default-pathname-defaults*))
  "Return an abbreviated pathname sufficent to identify the pathname relative
   to the defaults."
  (if (null defaults)
    (namestring path)
    (let* ((dir (pathname-directory path))
           (nam (pathname-name path))
           (typ (pathname-type path))
           (ver (pathname-version path))
           (host (pathname-host path))
           (logical-p (neq host :unspecific))
           (default-dir (pathname-directory defaults)))
      ;; enough-host-namestring
      (setq host (if (and host
                          (neq host :unspecific)
                          (not (equalp host (pathname-host defaults))))
                   (%str-cat host ":")
                   ""))
      ;; enough-directory-namestring
      (cond ((equalp dir default-dir)
             (setq dir '(:relative)))
            ((and dir default-dir
                  (eq (car dir) :absolute) (eq (car default-dir) :absolute))
             ;; maybe make it relative to defaults             
             (do ((p1 (cdr dir) (cdr p1))
                  (p2 (cdr default-dir) (cdr p2)))
                 ((or (null p2) (null p1) (not (equalp (car p1) (car p2))))
                  (when (and (null p2) (or t (neq p1 (cdr dir))))
                    (setq dir (cons :relative p1)))))))
      (setq dir (%directory-list-namestring dir logical-p))
      ;; enough-file-namestring
      (when (or (equalp ver (pathname-version defaults))
                (not logical-p))
        (setq ver nil))
      (when (and (null ver) (equalp typ (pathname-type defaults)))
        (setq typ nil))
      (when (and (null typ) (equalp nam (pathname-name defaults)))
        (setq nam nil))
      (setq nam (file-namestring-from-parts nam typ ver))
      (%str-cat host dir nam))))

(defun cons-pathname (dir name type &optional host version device)
  (if (neq host :unspecific)
    (%cons-logical-pathname dir name type host version)
    (%cons-pathname dir name type version device)))

(defun pathname (path)
  "Convert thing (a pathname, string or stream) into a pathname."
  (etypecase path
    (pathname path)
    (stream (%path-from-stream path))
    (string (string-to-pathname path))))

(defun %path-from-stream (stream)
  (or (pathname (stream-filename stream))
      (error "Can't determine pathname of ~S ." stream)))      ; ???

;Like (pathname stream) except returns NIL rather than error when there's no
;filename associated with the stream.
(defun stream-pathname (stream &aux (path (stream-filename stream)))
  (when path (pathname path)))

(defun get-pathname-sstring (string &optional (start 0) (end (length string)))
  #-windows-target
  (get-sstring string start end)
  #+windows-target
  (multiple-value-bind (sstr start end)
      (get-sstring string start end)
    (declare (fixnum start end)
             (simple-string sstr))
    (if (do* ((i start (1+ i)))
             ((= i end))
          (declare (fixnum i))
          (when (eql (schar sstr i) #\\)
            (return t)))
      (let* ((len (- end start))
             (new (make-string len)))
        (declare (fixnum len) (simple-string new))
        (dotimes (i len)
          (let* ((ch (schar sstr start)))
            (if (eql ch #\\)
              (setf (schar new i) #\/)
              (setf (schar new i) ch)))
          (incf start))
        (values new 0 len))
      (values sstr start end))))
              
(defun string-to-pathname (string &optional (start 0) (end (length string))
                                            (reference-host nil)
                                            (defaults *default-pathname-defaults*))
  (require-type reference-host '(or null string))
  (multiple-value-bind (sstr start end) (get-pathname-sstring string start end)
    (if (and (> end start)
             (eql (schar sstr start) #\~))
      (setq sstr (tilde-expand (subseq sstr start end))
            start 0
            end (length sstr)))
    (let (directory name type host version device (start-pos start) (end-pos end) has-slashes)
      (multiple-value-setq (host start-pos has-slashes) (pathname-host-sstr sstr start-pos end-pos))
      (cond ((and host (neq host :unspecific))
             (when (and reference-host (not (string-equal reference-host host)))
               (error "Host in ~S does not match requested host ~S"
                      (%substr sstr start end) reference-host)))
            ((or reference-host
		 (and defaults
		      (neq (setq reference-host (pathname-host defaults)) :unspecific)))
	     ;;If either a reference-host is specified or defaults is a logical pathname
	     ;; then the string must be interpreted as a logical pathname.
	     (when has-slashes
	       (error "Illegal logical namestring ~S" (%substr sstr start end)))
             (setq host reference-host)))
      #+windows-target
      (when (and (eq host :unspecific)
                 (eql start-pos 0)
                 (eql (position #\: sstr) 1))
        (let* ((ch (schar sstr 0)))
          (when (and (alpha-char-p ch)
                     (standard-char-p ch))
            (setq device (make-string 1 :initial-element ch)
                  start-pos 2))))
      (multiple-value-setq (directory start-pos) (pathname-directory-sstr sstr start-pos end-pos host))
      (unless (eq host :unspecific)
	(multiple-value-setq (version end-pos) (pathname-version-sstr sstr start-pos end-pos)))
      (multiple-value-setq (type end-pos) (pathname-type-sstr sstr start-pos end-pos))
      ;; now everything else is the name
      (unless (eq start-pos end-pos)
        (setq name (%std-name-component (%substr sstr start-pos end-pos))))
      (if (eq host :unspecific)
	(%cons-pathname directory name type (if name :newest) device)
        (%cons-logical-pathname directory name type host version)))))

(defun parse-namestring (thing &optional host (defaults *default-pathname-defaults*)
                               &key (start 0) end junk-allowed)
  (declare (ignore junk-allowed))
  (unless (typep thing 'string)
    (let* ((path (pathname thing))
	   (pathname-host (pathname-host path)))
      (when (and host pathname-host
		 (or (eq pathname-host :unspecific) ;physical
		     (not (string-equal host pathname-host))))
	(error "Host in ~S does not match requested host ~S" path host))
      (return-from parse-namestring (values path start))))
  (when host
    (verify-logical-host-name host))
  (setq end (check-sequence-bounds thing start end))
  (values (string-to-pathname thing start end host defaults) end))



(defun %std-device-component (device host)
  (when (and (or (null host) (eq host :unspecific))
             (and device (not (eq device :unspecific))))
    #+windows-target
    (unless (and (typep device 'string)
                 (eql (length device) 1)
                 (alpha-char-p (char device 0))
                 (standard-char-p (char device 0)))
      (error "Invalid pathname device ~s" device))
    device))
    
(defun make-pathname (&key (host nil host-p) 
                           (device nil device-p)
                           (directory nil directory-p)
                           (name nil name-p)
                           (type nil type-p)
                           (version nil version-p)
                           (defaults nil defaults-p) case
                           &aux path)
  "Makes a new pathname from the component arguments. Note that host is
a host-structure or string."
  (when case (setq case (require-type case pathname-case-type)))
  (if (null host-p)
    (let ((defaulted-defaults (if defaults-p defaults *default-pathname-defaults*)))
      (setq host (if defaulted-defaults
		   (pathname-host defaulted-defaults)
		   :unspecific)))
    (unless host (setq host :unspecific)))
  (if directory-p 
    (setq directory (%std-directory-component directory host)))
  (if (and defaults (not directory-p))
    (setq directory (pathname-directory defaults)))
  (if (and defaults (not device-p))
    (setq device (pathname-device defaults)))
  (setq device (%std-device-component device host))
  (setq name
        (if name-p
             (%std-name-component name)
             (and defaults (pathname-name defaults))))
  (setq type
        (if type-p
             (%std-type-component type)
             (and defaults (pathname-type defaults))))
  (setq version (if version-p
                  (%logical-version-component version)
		  (if name-p
		    nil
		    (and defaults (pathname-version defaults)))))
  (setq path
        (if (eq host :unspecific)
          (%cons-pathname directory name type version device)
          (%cons-logical-pathname
	   (or directory
	       (unless directory-p '(:absolute)))
	   name type host version)))
  (when (and (eq (car directory) :absolute)
	     (member (cadr directory) '(:up :back)))
    (error 'simple-file-error :pathname path :error-type "Second element of absolute directory component in ~s is ~s" :format-arguments (list (cadr directory))))
  (let* ((after-wif (cadr (member :wild-inferiors directory))))
    (when (member after-wif '(:up :back))
          (error 'simple-file-error :pathname path :error-type "Directory component in ~s contains :WILD-INFERIORS followed by ~s" :format-arguments (list after-wif))))
	 
  (when (and case (neq case :local))
    (setf (%pathname-directory path) (%reverse-component-case (%pathname-directory path) case)
          (%pathname-name path) (%reverse-component-case (%pathname-name path) case)
          (%pathname-type path) (%reverse-component-case (%pathname-type path) case)))
  path)

;;;  In portable CL, if the :directory argument to make pathname is a
;;;  string, it should be the name of a top-level directory and should
;;;  not contain any punctuation characters such as "/" or ";".  In
;;;  MCL a string :directory argument with slashes or semi-colons will
;;;  be parsed as a directory in the obvious way.
(defun %std-directory-component (directory host)
  (cond ((null directory) nil)
        ((eq directory :wild) '(:absolute :wild-inferiors))
        ((stringp directory) (%directory-string-list directory 0 (length directory) host))
        ((listp directory)
         ;Standardize the directory list, taking care not to cons if nothing
         ;needs to be changed.
         (let ((names (%cdr directory)) (new-names ()))
           (do ((nn names (%cdr nn)))
               ((null nn) (setq new-names (if new-names (nreverse new-names) names)))
             (let* ((name (car nn))
                    (new-name (%std-directory-part name)))
               (unless (eq name new-name)
                 (unless new-names
                   (do ((new-nn names (%cdr new-nn)))
                       ((eq new-nn nn))
                     (push (%car new-nn) new-names))))
               (when (or new-names (neq name new-name))
                 (push new-name new-names))))
           (when (memq :up (or new-names names))
             (setq new-names (remove-up (copy-list (or new-names names)))))
           (ecase (%car directory)
             (:relative           
                  (cond (new-names         ; Just (:relative) is the same as NIL. - no it isnt
                         (if (eq new-names names)
                           directory
                           (cons ':relative new-names)))
                        (t directory)))
             (:absolute
                  (cond ((null new-names) directory)  ; But just (:absolute) IS the same as NIL
                        ((eq new-names names) directory)
                        (t (cons ':absolute new-names)))))))
        (t (report-bad-arg directory '(or string list (member :wild))))))

(defun %std-directory-part (name)
  (case name
    ((:wild :wild-inferiors :up) name)
    (:back :up)
    (t (cond ((string= name "*") :wild)
             ((string= name "**") :wild-inferiors)
	     ((string= name "..") :up)
             (t (%path-std-quotes name "/:;*" "/:;"))))))

; this will allow creation of garbage pathname "foo:bar;bas:" do we care?
(defun merge-pathnames (path &optional (defaults *default-pathname-defaults*)
                                       (default-version :newest))
  "Construct a filled in pathname by completing the unspecified components
   from the defaults."
  ;(declare (ignore default-version))
  (when (not (pathnamep path))(setq path (pathname path)))
  (when (and defaults (not (pathnamep defaults)))(setq defaults (pathname defaults)))
  (let* ((path-dir (pathname-directory path))
         (path-host (pathname-host path))
         (path-name (pathname-name path))
	 (path-type (pathname-type path))
         (path-device (pathname-device path))
         (default-dir (and defaults (pathname-directory defaults)))
         (default-host (and defaults (pathname-host defaults)))
         (default-device (and defaults (pathname-device defaults)))
         ; take host from defaults iff path-dir is logical or absent - huh? 
         (host (cond ((or (null path-host)  ; added 7/96
                          (and (eq path-host :unspecific)
                               (or (null path-dir)
                                   (null (cdr path-dir))
                                   (and (eq :relative (car path-dir))
                                        (not (memq default-host '(nil :unspecific)))))))
                          
                      default-host)
                     (t  path-host)))
         (dir (cond ((null path-dir) default-dir)
                    ((null default-dir) path-dir)
                    ((eq (car path-dir) ':relative)
                     (let ((the-dir (append default-dir (%cdr path-dir))))
                       (when (memq ':up the-dir)(setq the-dir (remove-up (copy-list the-dir))))
                       the-dir))
                    (t path-dir)))
         (nam (or path-name
                  (and defaults (pathname-name defaults))))
         (typ (or path-type
                  (and defaults (pathname-type defaults))))
         (version (or (pathname-version path)
		      (cond ((not path-name)
			     (or (and defaults (pathname-version defaults))
                                 default-version))
			    (t default-version))))
         (device (or path-device default-device)))
    (if (and (pathnamep path)
             (eq dir (%pathname-directory path))
             (eq nam path-name)
             (eq typ (%pathname-type path))
             (eq host path-host)
             (eq device path-device)
             (eq version (pathname-version path)))
      path 
      (cons-pathname dir nam typ host version device))))

(defun directory-pathname-p (path)
  (let ((name (pathname-name path))(type (pathname-type path)))
    (and  (or (null name) (eq name :unspecific) (%izerop (length name)))
          (or (null type) (eq type :unspecific)))))

;In CCL, a pathname is logical if and only if pathname-host is not :unspecific.
(defun pathname-host (thing &key case)
  "Return PATHNAME's host."
  (when (streamp thing)(setq thing (%path-from-stream thing)))
  (when case (setq case (require-type case pathname-case-type)))
  (let ((name
         (typecase thing    
           (logical-pathname (%logical-pathname-host thing))
           (pathname :unspecific)
           (string (multiple-value-bind (sstr start end) (get-pathname-sstring thing) 
                     (pathname-host-sstr sstr start end)))
           (t (report-bad-arg thing pathname-arg-type)))))
    (if (and case (neq case :local))
      (progn
	(when (and (eq case :common) (neq name :unspecific)) (setq case :logical))
	(%reverse-component-case name case))
      name)))

(defun pathname-host-sstr (sstr start end &optional no-check)
  ;; A pathname with any (unescaped) /'s is always a physical pathname.
  ;; Otherwise, if the pathname has either a : or a ;, then it's always logical.
  ;; Otherwise, it's probably physical.
  ;; Return :unspecific for physical, host string or nil for a logical.
  (let* ((slash (%path-mem "/" sstr start end))
	 (pos (and (not slash) (%path-mem ":;" sstr start end)))
	 (pos-char (and pos (%schar sstr pos)))
	 (host (and (eql pos-char #\:) (%substr sstr start pos))))
    (cond (host
	   (unless (or no-check (logical-host-p host))
	     (error "~S is not a defined logical host" host))
	   (values host (%i+ pos 1) nil))
	  ((eql pos-char #\;) ; logical pathname with missing host
	   (values nil start nil))
	  (t ;else a physical pathname.
	   (values :unspecific start slash)))))


(defun pathname-device (thing &key case)
  "Return PATHNAME's device."
  (declare (ignore case))
  (let* ((p (pathname thing)))
    (etypecase p
      (logical-pathname :unspecific)
      (pathname (%physical-pathname-device p)))))



;A directory is either NIL or a (possibly wildcarded) string ending in "/" or ";"
;Quoted /'s are allowed at this stage, though will get an error when go to the
;filesystem.
(defun pathname-directory (path &key case)
  "Return PATHNAME's directory."
  (when (streamp path) (setq path (%path-from-stream path)))
  (when case (setq case (require-type case pathname-case-type)))
  (let* ((logical-p nil)
	 (names (typecase path
		  (logical-pathname (setq logical-p t) (%pathname-directory path))
		  (pathname (%pathname-directory path))
		  (string
		   (multiple-value-bind (sstr start end) (get-pathname-sstring path)
		     (multiple-value-bind (host pos2) (pathname-host-sstr sstr start end)
		       (unless (eq host :unspecific) (setq logical-p t))
                       #+windows-target
                       (unless logical-p
                         (if (and (> end 1)
                                  (eql (schar sstr 1) #\:))
                           (setq pos2 2)))
                       (pathname-directory-sstr sstr pos2 end host))))
		  (t (report-bad-arg path pathname-arg-type)))))
    (if (and case (neq case :local))
      (progn
	(when (and (eq case :common) logical-p) (setq case :logical))
	(%reverse-component-case names case))
      names)))

;; Must match pathname-directory-end below
(defun pathname-directory-sstr (sstr start end host)
  (if (and (eq host :unspecific)
           (> end start)
           (eql (schar sstr start) #\~))
    (setq sstr (tilde-expand (subseq sstr start end))
          start 0
          end (length sstr)))
  (let ((pos (%path-mem-last (if (eq host :unspecific) "/" ";") sstr start end)))
    (if pos
      (values 
       (%directory-string-list sstr start (setq pos (%i+ pos 1)) host)
       pos)
      (values (and (neq host :unspecific)
                   ;;(neq start end)
		   '(:absolute))
	      start))))

;; Must match pathname-directory-sstr above
(defun pathname-directory-end (sstr start end)
  (multiple-value-bind (host pos2) (pathname-host-sstr sstr start end)
    (let ((pos (%path-mem-last (if (eq host :unspecific) "/" ";") sstr pos2 end)))
      (if pos
	(values (%i+ pos 1) host)
	(values pos2 host)))))

(defun %directory-string-list (sstr start &optional (end (length sstr)) host)
  ;; Should use host to split by / vs. ; but for now suport both for either host,
  ;; like the mac version. It means that ';' has to be quoted in unix pathnames.
  (declare (ignore host))
  ;This must cons up a fresh list, %expand-logical-directory rplacd's it.
  (labels ((std-part (sstr start end)
             (%std-directory-part (if (and (eq start 0) (eq end (length sstr)))
                                    sstr (%substr sstr start end))))
           (split (sstr start end)
	     (unless (eql start end)
	       (let ((pos (%path-mem "/;" sstr start end)))
		 (if (eq pos start)
		   (split sstr (%i+ start 1) end) ;; treat multiple ////'s as one.
                   (cons (std-part sstr start (or pos end))
                         (when pos
                           (split sstr (%i+ pos 1) end))))))))
    (unless (eq start end)
      (let* ((slash-pos (%path-mem "/" sstr start end))
	     (semi-pos (%path-mem ";" sstr start end))
	     (pos (or slash-pos semi-pos)))
	; this never did anything sensible but did not signal an error
        (when (and slash-pos semi-pos)
	  (error "Illegal directory string ~s" (%substr sstr start end)))
        (if (null pos)
	  (list :relative (std-part sstr start end))
	  (let ((pos-char (%schar sstr pos)))
	    (cons (if (eq pos start)
		    (if (eq pos-char #\/) ':absolute ':relative)
		    (if (eq pos-char #\/) ':relative ':absolute))
		  (split sstr start end))))))))

(defun pathname-version (path)
  "Return PATHNAME's version."
  (when (streamp path) (setq path (%path-from-stream path)))
  (typecase path
    (logical-pathname (%logical-pathname-version path))
    (pathname (%physical-pathname-version path))
    (string
     (multiple-value-bind (sstr start end) (get-pathname-sstring path)
       (multiple-value-bind (newstart host) (pathname-directory-end sstr start end)
	 (if (eq host :unspecific)
	   nil
	   (values (pathname-version-sstr sstr newstart end))))))
    (t (report-bad-arg path pathname-arg-type))))

(defun pathname-version-sstr (sstr start end)
  (declare (fixnum start end))
  (let ((pos (%path-mem-last "." sstr start end)))
    (if (and pos (%i> pos start) (%path-mem "." sstr start (%i- pos 1)))
      (values (%std-version-component (%substr sstr (%i+ pos 1) end)) pos)
      (values nil end))))

(defun %std-version-component (v)
  (cond ((or (null v) (eq v :unspecific)) v)
	((eq v :wild) "*")
	((string= v "") :unspecific)
	((string-equal v "newest") :newest)
	((every #'digit-char-p v) (parse-integer v))
	(t (%path-std-quotes v "./:;*" "./:;"))))


;A name is either NIL or a (possibly wildcarded, possibly empty) string.
;Quoted /'s are allowed at this stage, though will get an error if go to the
;filesystem.
(defun pathname-name (path &key case)
  "Return PATHNAME's name."
  (when (streamp path) (setq path (%path-from-stream path)))
  (when case (setq case (require-type case pathname-case-type)))
  (let* ((logical-p nil)
	 (name (typecase path
		 (logical-pathname (setq logical-p t) (%pathname-name path))
		 (pathname (%pathname-name path))
		 (string
		  (multiple-value-bind (sstr start end) (get-pathname-sstring path)
		    (multiple-value-bind (newstart host) (pathname-directory-end sstr start end)
		      (setq start newstart)
		      (unless (eq host :unspecific)
			(setq logical-p t)
			(setq end (nth-value 1 (pathname-version-sstr sstr start end))))
		      ;; TODO: -->> Need to make an exception so that ".emacs" is name with no type.
		      ;;   -->> Need to make an exception so that foo/.. is a directory pathname,
		      ;; for native.
		      (setq end (or (%path-mem-last "." sstr start end) end));; strip off type
		      (unless (eq start end)
			(%std-name-component (%substr sstr start end))))))
		 (t (report-bad-arg path pathname-arg-type)))))
    (if (and case (neq case :local))
      (progn
	(when (and (eq case :common) logical-p) (setq case :logical))
	(%reverse-component-case name case))
      name)))

(defun %std-name-component (name)
  (cond ((or (null name) (eq name :unspecific) (eq name :wild)) name)
        ((equal name "*") :wild)
        (t (%path-std-quotes name "/:;*" "/:;"))))

;A type is either NIL or a (possibly wildcarded, possibly empty) string.
;Quoted :'s are allowed at this stage, though will get an error if go to the
;filesystem.
(defun pathname-type (path &key case)
  "Return PATHNAME's type."
  (when (streamp path) (setq path (%path-from-stream path)))
  (when case (setq case (require-type case pathname-case-type)))
  (let* ((logical-p nil)
	 (name (typecase path
		 (logical-pathname (setq logical-p t) (%pathname-type path))
		 (pathname (%pathname-type path))
		 (string
		  (multiple-value-bind (sstr start end) (get-pathname-sstring path)
		    (multiple-value-bind (newstart host) (pathname-directory-end sstr start end)
		      (setq start newstart)
		      (unless (eq host :unspecific)
			(setq logical-p t)
			(setq end (nth-value 1 (pathname-version-sstr sstr start end))))
		      ;; TODO: -->> Need to make an exception so that ".emacs" is name with no type.
		      ;;   -->> Need to make an exception so that foo/.. is a directory pathname,
		      ;; for native.
		      (pathname-type-sstr sstr start end))))
		 (t (report-bad-arg path pathname-arg-type)))))
    (if (and case (neq case :local))
      (progn
	(when (and (eq case :common) logical-p) (setq case :logical))
	(%reverse-component-case name case))
      name)))

; assumes dir & version if any has been stripped away
(defun pathname-type-sstr (sstr start end)
  (let ((pos (%path-mem-last "." sstr start end)))
    (if pos
      (values (%std-type-component (%substr sstr (%i+ 1 pos) end)) pos)
      (values nil end))))

(defun %std-type-component (type)
  (cond ((or (null type) (eq type :unspecific) (eq type :wild)) type)
        ((equal type "*") :wild)
        (t (%path-std-quotes type "./:;*" "./:;"))))

(defun %std-name-and-type (native)
  (let* ((end (length native))
	 (pos (position #\. native :from-end t))
	 (type (and pos
		    (%path-std-quotes (%substr native (%i+ 1 pos) end)
				      nil "/:;*")))
	 (name (unless (eq (or pos end) 0)
		 (%path-std-quotes (if pos (%substr native 0 pos) native)
				   nil "/:;*"))))
    (values name type)))

(defun %reverse-component-case (name case)
  (cond ((not (stringp name))
         (if (listp name)
           (mapcar #'(lambda (name) (%reverse-component-case name case))  name)
           name))
        #+advanced-studlification-feature
        ((eq case :studly) (string-studlify name))
	((eq case :logical)
	 (if (every #'(lambda (ch) (not (lower-case-p ch))) name)
	   name
	   (string-upcase name)))
        (t ; like %read-idiocy but non-destructive - need it be?
         (let ((which nil)
               (len (length name)))
           (dotimes (i len)
             (let ((c (%schar name i)))
               (if (alpha-char-p c)
                 (if (upper-case-p c)
                   (progn
                     (when (eq which :lower)(return-from %reverse-component-case name))
                     (setq which :upper))
                   (progn
                     (when (eq which :upper)(return-from %reverse-component-case name))
                     (setq which :lower))))))
           (case which
             (:lower (string-upcase name))
             (:upper (string-downcase name))
             (t name))))))

;;;;;;; String-with-quotes utilities
(defun %path-mem-last-quoted (chars sstr &optional (start 0) (end (length sstr)))
  (while (%i< start end)
    (when (and (%%str-member (%schar sstr (setq end (%i- end 1))) chars)
               (%path-quoted-p sstr end start))
      (return-from %path-mem-last-quoted end))))

(defun %path-mem-last (chars sstr &optional (start 0) (end (length sstr)))
  (while (%i< start end)
    (when (and (%%str-member (%schar sstr (setq end (%i- end 1))) chars)
               (not (%path-quoted-p sstr end start)))
      (return-from %path-mem-last end))))

(defun %path-mem (chars sstr &optional (start 0) (end (length sstr)))
  (let ((one-char (when (eq (length chars) 1) (%schar chars 0))))
    (while (%i< start end)
      (let ((char (%schar sstr start)))
        (when (if one-char (eq char one-char)(%%str-member char chars))
          (return-from %path-mem start))
        (when (eq char *pathname-escape-character*)
          (setq start (%i+ start 1)))
        (setq start (%i+ start 1))))))

; these for \:  meaning this aint a logical host. Only legal for top level dir
 
(defun %path-unquote-one-quoted (chars sstr &optional (start 0)(end (length sstr)))
  (let ((pos (%path-mem-last-quoted chars sstr start end)))
    (when (and pos (neq pos 1))
      (cond ((or (%path-mem chars sstr start (1- pos))
                 (%path-mem-last-quoted chars sstr start (1- pos)))
             nil)
            (t (%str-cat (%substr sstr start (1- pos))(%substr sstr  pos end)))))))

(defun %path-one-quoted-p (chars sstr &optional (start 0)(end (length sstr)))
  (let ((pos (%path-mem-last-quoted chars sstr start end)))
    (when (and pos (neq pos 1))
      (not (or (%path-mem-last-quoted chars sstr start (1- pos))
               (%path-mem chars sstr start (1- pos)))))))
 
(defun %path-quoted-p (sstr pos start &aux (esc *pathname-escape-character*) (q nil))
  (while (and (%i> pos start) (eq (%schar sstr (setq pos (%i- pos 1))) esc))
    (setq q (not q)))
  q)



;Standardize pathname quoting, so can do EQUAL.
;; Subtle point: when keep-quoted is NIL, arg is assumed native,
;; and therefore escape characters are made quoted.
;; if keep-quoted is not NIL, e.g. if it's "", arg is assumed
;;   to be escaped already, so escape chars are interpreted as quotes.
;; Note that this can't be used to remove quotes because it
;; always keeps the escape character quoted.
(defun %path-std-quotes (arg keep-quoted make-quoted)
  (when (symbolp arg)
    (error "Invalid pathname component ~S" arg))
  (let* ((str arg)
         (esc *pathname-escape-character*)
         (end (length str))
         res-str char)
    (multiple-value-bind (sstr start)(array-data-and-offset str)
      (setq end (+ start end))
      (let ((i start))
        (until (eq i end)
          (setq char (%schar sstr i))
          (cond ((or (%%str-member char make-quoted)
                     (and (null keep-quoted) (eq char esc)))
                 (unless res-str
                   (setq res-str (make-array (%i- end start)
                                             :element-type (array-element-type sstr)
                                             :adjustable t :fill-pointer 0))
                   (do ((j start (%i+ j 1))) ((eq j i))
                     (vector-push-extend (%schar sstr j) res-str)))
                 (vector-push-extend esc res-str))
                ((neq char esc) nil)
                ((eq (setq i (%i+ i 1)) end)
                 (error "Malformed pathname component string ~S" str))
                ((or (eq (setq char (%schar sstr i)) esc)
                     (%%str-member char keep-quoted))
                 (when res-str (vector-push-extend esc res-str)))
                (t
                 (unless res-str
                   (setq res-str (make-array (%i- end start)
                                             :element-type (array-element-type sstr)
                                             :adjustable t :fill-pointer 0))
                   (do ((j start (%i+ j 1)) (end (%i- i 1))) ((eq j end))
                     (vector-push-extend (%schar sstr j) res-str)))))
          (when res-str (vector-push-extend char res-str))
          (setq i (%i+ i 1)))
        (ensure-simple-string (or res-str str))))))



(defun %%str-member (char string)
  (locally (declare (optimize (speed 3)(safety 0)))
    (dotimes (i (the fixnum (length string)))
      (when (eq (%schar string i) char)
        (return i)))))


(defun file-write-date (path)
  "Return file's creation date, or NIL if it doesn't exist.
  An error of type file-error is signaled if file is a wild pathname"
  (%file-write-date (defaulted-native-namestring path)))

(defun file-author (path)
  "Return the file author as a string, or NIL if the author cannot be
  determined. Signal an error of type FILE-ERROR if FILE doesn't exist,
  or FILE is a wild pathname."
  (%file-author (defaulted-native-namestring path)))

(defun touch (path)
  (if (not (probe-file path))
    (progn
      (ensure-directories-exist path)
      (if (or (pathname-name path)
              (pathname-type path))
        (create-file path)))
    (%utimes (defaulted-native-namestring path)))
  t)


;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
; load, require, provide

(defun find-load-file (file-name)
  (let ((full-name (full-pathname file-name :no-error nil))
        (kind nil))
    (when full-name
      (let ((file-type (pathname-type full-name))
            (merged (pathname (merge-pathnames file-name))))
        (if (and file-type (neq file-type :unspecific))
          (values (probe-file full-name) merged (if (eq (pathname-host file-name) :unspecific) full-name file-name))
          (let* ((source (merge-pathnames file-name *.lisp-pathname*))
                 (fasl   (merge-pathnames file-name *.fasl-pathname*))
                 (true-source (probe-file source))
                 (true-fasl   (probe-file fasl)))
            (cond (true-source
                   (if (and true-fasl
                            (> (file-write-date true-fasl)
                               (file-write-date true-source)))
                     (values true-fasl merged source)
                     (values true-source merged source)))
                  (true-fasl
                   (values true-fasl merged fasl))
                  ((and (multiple-value-setq (full-name kind)
                          (let* ((realpath (%realpath (defaulted-native-namestring full-name))))
                            (if realpath
                              (%probe-file-x realpath ))))
                        (eq kind :file))
                   (values full-name merged file-name)))))))))





(defun load (file-name &key (verbose *load-verbose*)
                       (print *load-print*)
                       (if-does-not-exist :error)
		       (external-format :default)
                       (preserve-optimization-settings *load-preserves-optimization-settings*))
  "Load the file given by FILESPEC into the Lisp environment, returning
   T on success.

   Extension: :PRINT :SOURCE means print source as well as value"
  (loop
    (restart-case
     (return (%load file-name verbose print if-does-not-exist external-format preserve-optimization-settings))
      (retry-load ()
                  :report (lambda (stream) (format stream "Retry loading ~s" file-name)))
      (skip-load ()
                 :report (lambda (stream) (format stream "Skip loading ~s" file-name))
                 (return nil))
      (load-other ()
                  :report (lambda (stream) (format stream "Load other file instead of ~s" file-name))
                  (return
                   (load (choose-file-dialog)
                         :verbose verbose
                         :print print
                         :if-does-not-exist if-does-not-exist))))))


(defun %load (file-name verbose print if-does-not-exist external-format preserve-optimization-settings)
  (let ((*load-pathname* file-name)
        (*load-truename* file-name)
        (source-file file-name)
        (optimization-setting-vars '(*nx-speed* *nx-space* *nx-safety*
                                     *nx-debug* *nx-cspeed*)))
    (declare (special *load-pathname* *load-truename*))
    (progv
        (if preserve-optimization-settings optimization-setting-vars)
        (if preserve-optimization-settings (mapcar #'symbol-value optimization-setting-vars))
    (when (typep file-name 'string-input-stream)
      (when verbose
          (format t "~&;Loading from stream ~S..." file-name)
          (force-output))
      (let ((*package* *package*)
            (*readtable* *readtable*))
        (load-from-stream file-name print))
      (return-from %load file-name))
    (when (and (stringp file-name)
               (eql (length "http://") (string-lessp "http://" file-name)))
      (when verbose
        (format t "~&;Loading from URL ~S..." file-name)
        (force-output))
      (let* ((vec (if if-does-not-exist
                    (snarf-url file-name)
                    (handler-case (snarf-url file-name)
                      (error () (return-from %load nil)))))
             (*package* *package*)
             (*readtable* *readtable*)
             (*loading-file-source-file* file-name)
             (*loading-files* (cons file-name (specialv *loading-files*))))
        (with-input-from-vector (stream vec :external-format external-format)
          (load-from-stream stream print)))
      (return-from %load file-name))
    (unless (streamp file-name)
      (multiple-value-setq (*load-truename* *load-pathname* source-file)
        (find-load-file (merge-pathnames file-name)))
      (when (not *load-truename*)
        (return-from %load (if if-does-not-exist
                             (signal-file-error $err-no-file file-name))))
      (setq file-name *load-truename*))
    (let* ((*package* *package*)
           (*readtable* *readtable*)
           (*loading-files* (cons file-name (specialv *loading-files*)))
           ;;reset by fasload to logical name stored in the file
           (*loading-file-source-file* (namestring source-file))
           (*loading-toplevel-location* nil))
      (declare (special *loading-files* *loading-file-source-file*))
      (when verbose
	(format t "~&;Loading ~S..." *load-pathname*)
	(force-output))
      (cond ((fasl-file-p file-name)
	     (let ((*fasload-print* print)
		   (restart-setup nil)
		   (restart-source nil)
		   (restart-fasl nil))
	       (declare (special *fasload-print*))
	       (flet ((restart-test (c)
			(unless restart-setup
			  (setq restart-setup t)
			  (let ((source *loading-file-source-file*)
				(fasl *load-pathname*))
			    (when (and (not (typep c 'file-error))
				       source
				       fasl
				       (setq source (probe-file source))
				       (setq fasl (probe-file fasl))
				       (not (equalp source fasl)))
			      (setq restart-fasl (namestring *load-pathname*)
				    restart-source *loading-file-source-file*))))
			(not (null restart-fasl)))
		      (fname (p)
			#-versioned-file-system
			(namestring (make-pathname :version :unspecific :defaults p))
			#+versioned-file-system
			(namestring p)))
		 (restart-case (multiple-value-bind (winp err) 
				   (%fasload (defaulted-native-namestring file-name))
				 (if (not winp) 
				   (%err-disp err)))
		   (load-source 
		    ()
		    :test restart-test
		    :report (lambda (s) 
			      (format s "Load ~s instead of ~s" 
				      (fname restart-source) (fname restart-fasl)))
		    (%load source-file verbose print if-does-not-exist external-format preserve-optimization-settings))
		   (recompile
		    ()
		    :test restart-test
		    :report (lambda (s)
			      (let ((*print-circle* NIL))
				(format s
					(if (equalp
					     restart-source
					     (make-pathname :type (pathname-type *.lisp-pathname*)
							    :defaults restart-fasl))
					  "Compile ~s and then load ~s again"
					  "Compile ~s into ~s then load ~:*~s again")
					(fname restart-source) (fname restart-fasl))))
		    (compile-file restart-source :output-file restart-fasl)
		    (%load restart-fasl verbose print if-does-not-exist external-format preserve-optimization-settings))))))
	    (t 
	     (with-open-file (stream file-name
				     :element-type 'base-char
				     :external-format (if (eq external-format :default) :inferred external-format))
	       (load-from-stream stream print)))))))
  file-name)

(defun load-from-stream (stream print &aux (eof-val (list ())) val)
  (with-compilation-unit (:override nil) ; try this for included files
    (let ((env (new-lexical-environment (new-definition-environment 'eval)))
          ;; source note map to use with any compilations.
          (*nx-source-note-map*  (and *save-source-locations*
                                      (make-hash-table :test #'eq :shared nil)))
          (*loading-toplevel-location* nil))
      (%rplacd (defenv.type (lexenv.parent-env env)) *outstanding-deferred-warnings*)
      (loop
        (multiple-value-setq (val *loading-toplevel-location*)
          (read-recording-source stream
                                 :eofval eof-val
                                 :file-name *loading-file-source-file*
                                 :map *nx-source-note-map*
                                 :save-source-text (neq *save-source-locations* :no-text)))
        (when (eq eof-val val)
          (return))
        (when (eq print :source) (format t "~&Source: ~S~%" val))
        (setq val (cheap-eval-in-environment val env))
        (when print
          (format t "~&~A~S~%" (if (eq print :source) "Value: " "") val))))))

(defun include (filename)
  (load
   (if (null *loading-files*)
     filename
     (merge-pathnames filename (directory-namestring (car *loading-files*))))))

(%fhave '%include #'include)

(defun delete-file (path)
  "Delete the specified FILE."
  (let* ((namestring (defaulted-native-namestring path))
	 (err (%delete-file namestring)))
    (or (eql 0 err) (signal-file-error err path))))

(defvar *known-backends* ())

(defun fasl-file-p (pathname)
  (let* ((type (pathname-type pathname)))
    (or (and (null *known-backends*)
	     (equal type (pathname-type *.fasl-pathname*)))
	(dolist (b *known-backends*)
	  (when (equal type (pathname-type (backend-target-fasl-pathname b)))
	    (return t)))
        (ignore-errors
          (with-open-file (f pathname
                             :direction :input
                             :element-type '(unsigned-byte 8))
            ;; Assume that (potential) FASL files start with #xFF00 (big-endian),
            ;; and that source files don't.
            (and (eql (read-byte f nil nil) #xff)
                 (eql (read-byte f nil nil) #x00)))))))

(defun provide (module)
  "Adds a new module name to *MODULES* indicating that it has been loaded.
   Module-name is a string designator"
  (pushnew (string module) *modules* :test #'string=)
  module)

(defparameter *loading-modules* () "Internal. Prevents circularity")
(defparameter *module-provider-functions* '(module-provide-search-path)
  "A list of functions called by REQUIRE to satisfy an unmet dependency.
Each function receives a module name as a single argument; if the function knows how to load that module, it should do so, add the module's name as a string to *MODULES* (perhaps by calling PROVIDE) and return non-NIL."
  )

(defun module-provide-search-path (module)
  ;; (format *debug-io* "trying module-provide-search-path~%")
  (let* ((module-name (string module))
         (pathname (find-module-pathnames module-name)))
    (when pathname
      (if (consp pathname)
        (dolist (path pathname) (load path))
        (load pathname))
      (provide module))))

(defun require (module &optional pathname)
  "Loads a module, unless it already has been loaded. PATHNAMES, if supplied,
   is a designator for a list of pathnames to be loaded if the module
   needs to be. If PATHNAMES is not supplied, functions from the list
   *MODULE-PROVIDER-FUNCTIONS* are called in order with MODULE-NAME
   as an argument, until one of them returns non-NIL.  User code is
   responsible for calling PROVIDE to indicate a successful load of the
   module."
  (let* ((str (string module))
	 (original-modules (copy-list *modules*)))
    (unless (or (member str *modules* :test #'string=)
		(member str *loading-modules* :test #'string=))
      ;; The check of (and binding of) *LOADING-MODULES* is a
      ;; traditional defense against circularity.  (Another
      ;; defense is not having circularity, of course.)  The
      ;; effect is that if something's in the process of being
      ;; REQUIREd and it's REQUIREd again (transitively),
      ;; the inner REQUIRE is a no-op.
      (let ((*loading-modules* (cons str *loading-modules*)))
	(if pathname
	  (dolist (path (if (atom pathname) (list pathname) pathname))
	    (load path))
	  (unless (some (lambda (p) (funcall p module))
			*module-provider-functions*)
	    (error "Module ~A was not provided by any function on ~S." module '*module-provider-functions*)))))
    (values module
	    (set-difference *modules* original-modules))))

(defun find-module-pathnames (module)
  "Returns the file or list of files making up the module"
  (let ((mod-path (make-pathname :name (string-downcase module) :defaults nil)) path)
        (dolist (path-cand *module-search-path* nil)
	  (let ((mod-cand (merge-pathnames mod-path path-cand)))
	    (if (wild-pathname-p path-cand)
		(let* ((untyped-p (member (pathname-type mod-cand) '(nil :unspecific)))
		       (matches (if untyped-p
				    (or (directory (merge-pathnames mod-cand *.lisp-pathname*))
					(directory (merge-pathnames mod-cand *.fasl-pathname*)))
				    (directory mod-cand))))
		  (when (and matches (null (cdr matches)))
		    (return (if untyped-p
				(make-pathname :type nil :defaults (car matches))
				(car matches)))))
		(when (setq path (find-load-file (merge-pathnames mod-path path-cand)))
		  (return path)))))))

(defun wild-pathname-p (pathname &optional field-key)
  "Predicate for determining whether pathname contains any wildcards."
  (flet ((wild-p (name) (or (eq name :wild)
                            (eq name :wild-inferiors)
                            (and (stringp name) (%path-mem "*" name)))))
    (case field-key
      ((nil)
       (or (some #'wild-p (pathname-directory pathname))
           (wild-p (pathname-name pathname))
           (wild-p (pathname-type pathname))
           (wild-p (pathname-version pathname))))
      (:host nil)
      (:device nil)
      (:directory (some #'wild-p (pathname-directory pathname)))
      (:name (wild-p (pathname-name pathname)))
      (:type (wild-p (pathname-type pathname)))
      (:version (wild-p (pathname-version pathname)))
      (t (wild-pathname-p pathname
                          (require-type field-key 
                                        '(member nil :host :device 
                                          :directory :name :type :version)))))))
