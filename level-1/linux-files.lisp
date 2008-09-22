;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
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

(in-package "CCL")

(defconstant unix-to-universal-time 2208988800)

#+windows-target
(progn


            

(defun nbackslash-to-forward-slash (namestring)
  (dotimes (i (length namestring) namestring)
    (when (eql (schar namestring i) #\\)
      (setf (schar namestring i) #\/))))

(defconstant univeral-time-start-in-windows-seconds 9435484800)

(defun windows-filetime-to-universal-time (ft)
  (let* ((100-ns (dpb (pref ft #>FILETIME.dwHighDateTime) (byte 32 32)
                      (pref ft #>FILETIME.dwLowDateTime)))
         (seconds-since-windows-epoch (floor 100-ns 10000000)))
    (- seconds-since-windows-epoch univeral-time-start-in-windows-seconds)))
)

(defun get-foreign-namestring (pointer)
  ;; On Darwin, foreign namestrings are encoded in UTF-8 and
  ;; are canonically decomposed (NFD).  Use PRECOMPOSE-SIMPLE-STRING
  ;; to ensure that the string is "precomposed" (NFC), like the
  ;; rest of the world and most sane people would expect.
  #+darwin-target
  (precompose-simple-string (%get-utf-8-cstring pointer))
  #+windows-target (nbackslash-to-forward-slash
                     (%get-native-utf-16-cstring pointer))
  ;; On some other platforms, the namestring is assumed to
  ;; be encoded according to the current locale's character
  ;; encoding (though FreeBSD seems to be moving towards
  ;; precomposed UTF-8.).
  ;; In any case, the use of %GET-CSTRING here is wrong ...
  #-(or darwin-target windows-target)
  (%get-cstring pointer))

(defun nanoseconds (n)
  (unless (and (typep n 'fixnum)
               (>= (the fixnum n) 0))
    (check-type n (real 0 #.(1- (ash 1 (1- target::nbits-in-word))))))
  (multiple-value-bind (q r)
      (floor n)
    (if (zerop r)
      (setq r 0)
      (setq r (floor (* r 1000000000))))
    (values q r)))

(defun milliseconds (n)
  (unless (and (typep n 'fixnum)
               (>= (the fixnum n) 0))
    (check-type n (real 0 #.(1- (ash 1 (1- target::nbits-in-word))))))
  (multiple-value-bind (q r)
      (floor n)
    (if (zerop r)
      (setq r 0)
      (setq r (floor (* r 1000))))
    (values q r)))

(defun microseconds (n)
  (unless (and (typep n 'fixnum)
               (>= (the fixnum n) 0))
    (check-type n (real 0 #.(1- (ash 1 (1- target::nbits-in-word))))))
  (multiple-value-bind (q r)
      (floor n)
    (if (zerop r)
      (setq r 0)
      (setq r (floor (* r 1000000))))
    (values q r)))

(defun semaphore-value (s)
  (if (istruct-typep s 'semaphore)
    (semaphore.value s)
    (semaphore-value (require-type s 'semaphore))))

(defun %wait-on-semaphore-ptr (s seconds milliseconds &optional flag)
  (if flag
    (if (istruct-typep flag 'semaphore-notification)
      (setf (semaphore-notification.status flag) nil)
      (report-bad-arg flag 'semaphore-notification)))
  (without-interrupts
   (let* ((status (ff-call
                   (%kernel-import target::kernel-import-wait-on-semaphore)
                   :address s
                   :unsigned seconds
                   :unsigned milliseconds
                   :signed))
          (result (zerop status)))     
     (declare (fixnum status))
     (when flag (setf (semaphore-notification.status flag) result))
     (values result status))))

(defun %process-wait-on-semaphore-ptr (s seconds milliseconds &optional
                                         (whostate "semaphore wait") flag)
  (or (%wait-on-semaphore-ptr s 0 0 flag)
      (with-process-whostate  (whostate)
        (loop
          (when (%wait-on-semaphore-ptr s seconds milliseconds flag)
            (return))))))

  
(defun wait-on-semaphore (s &optional flag (whostate "semaphore wait"))
  "Wait until the given semaphore has a positive count which can be
atomically decremented."
  (%process-wait-on-semaphore-ptr (semaphore-value s) #xffffff 0 whostate flag)
  t)


(defun %timed-wait-on-semaphore-ptr (semptr duration notification)
  (or (%wait-on-semaphore-ptr semptr 0 0 notification)
      (with-process-whostate ("Semaphore timed wait")
        (multiple-value-bind (secs millis) (milliseconds duration)
          (let* ((now (get-internal-real-time))
                 (stop (+ now
                          (* secs 1000)
                          millis)))
            (loop
              (multiple-value-bind (success err)
                  (progn
                    (%wait-on-semaphore-ptr semptr secs millis notification))
                (when success
                  (return t))
                (when (or (not (eql err #$EINTR))
                          (>= (setq now (get-internal-real-time)) stop))
                  (return nil))
                (unless (zerop duration)
                  (let* ((diff (- stop now)))
                    (multiple-value-bind (remaining-seconds remaining-millis)
                        (floor diff 1000)
                      (setq secs remaining-seconds
                            millis remaining-millis)))))))))))

(defun timed-wait-on-semaphore (s duration &optional notification)
  "Wait until the given semaphore has a postive count which can be
atomically decremented, or until a timeout expires."
  (%timed-wait-on-semaphore-ptr (semaphore-value s) duration notification))


(defun %signal-semaphore-ptr (p)
  (ff-call
   (%kernel-import target::kernel-import-signal-semaphore)
   :address p
   :signed-fullword))

(defun signal-semaphore (s)
  "Atomically increment the count of a given semaphore."
  (%signal-semaphore-ptr (semaphore-value s)))

(defun %os-getcwd (buf noctets)
  ;; Return N < 0, if error
  ;;        N < noctets: success, string is of length N (octets).
  ;;        N >= noctets: buffer needs to be larger.
  (let* ((p #+windows-target
           (#__wgetcwd buf (ash noctets -1))
           #-windows-target
           (#_getcwd buf noctets)))
    (declare (dynamic-extent p))
    (if (%null-ptr-p p)
      (let* ((err (%get-errno)))
	(if (eql err (- #$ERANGE))
	  (+ noctets noctets)
	  err))
      #+windows-target
      (do* ((i 0 (+ i 2)))
           ((= i noctets) (+ noctets noctets))
        (when (eql (%get-unsigned-word buf i) 0)
          (return i)))
      #-windows-target
      (dotimes (i noctets (+ noctets noctets))
	(when (eql 0 (%get-byte buf i))
	  (return i))))))
    
    
(defun current-directory-name ()
  "Look up the current working directory of the OpenMCL process; unless
it has been changed, this is the directory OpenMCL was started in."
  (flet ((try-getting-dirname (bufsize)
	   (%stack-block ((buf bufsize))
	     (let* ((len (%os-getcwd buf bufsize)))
	       (cond ((< len 0) (%errno-disp len))
		     ((< len bufsize)
		      (values (get-foreign-namestring buf) len))
		     (t (values nil len)))))))
    (do* ((string nil)
	  (len #+windows-target 128 #-windows-target 64)
	  (bufsize len len))
	 ((multiple-value-setq (string len) (try-getting-dirname bufsize))
	  string))))


(defun current-directory ()
  (mac-default-directory))

(defun (setf current-directory) (path)
  (cwd path)
  path)

(defun cd (path)
  (cwd path))




(defun %chdir (dirname)
  (with-filename-cstrs ((dirname dirname))
    (int-errno-call (#+windows-target #__wchdir #-windows-target #_chdir dirname))))

(defun %mkdir (name mode)
  #+windows-target (declare (ignore mode))
  (let* ((name name)
         (len (length name)))
    (when (and (> len 0) (eql (char name (1- len)) #\/))
      (setq name (subseq name 0 (1- len))))
    (with-filename-cstrs ((name name))
      (int-errno-call (#+windows-target #__wmkdir #-windows-target #_mkdir  name #-windows-target mode)))))

(defun %rmdir (name)
  (let* ((last (1- (length name))))
    (with-filename-cstrs ((name name))
      (when (and (>= last 0)
		 (eql (%get-byte name last) (char-code #\/)))
	(setf (%get-byte name last) 0))
      (int-errno-call (#+windows-target #__wrmdir #-windows-target #_rmdir  name)))))


(defun getenv (key)
  "Look up the value of the environment variable named by name, in the
OS environment."
  (with-cstrs ((key (string key)))
    (let* ((env-ptr (%null-ptr)))
      (declare (dynamic-extent env-ptr))
      (%setf-macptr env-ptr (#_getenv key))
      (unless (%null-ptr-p env-ptr)
	(%get-cstring env-ptr))))
  )

(defun setenv (key value &optional (overwrite t))
  "Set the value of the environment variable named by name, in the OS
environment. If there is no such environment variable, create it."
  #+windows-target (declare (ignore overwrite))
  #-windows-target
  (with-cstrs ((ckey key)
	       (cvalue value))
    (#_setenv ckey cvalue (if overwrite 1 0)))
  #+windows-target
  (with-cstrs ((pair (format nil "~a=~a" key value)))
    (#__putenv pair))
  )

#-windows-target                        ; Windows "impersonation" crap ?
(defun setuid (uid)
  "Attempt to change the current user ID (both real and effective);
fails unless the OpenMCL process has super-user privileges or the ID
given is that of the current user."
  (int-errno-call (#_setuid uid)))

#-windows-target
(defun setgid (uid)
  "Attempt to change the current group ID (both real and effective);
fails unless the OpenMCL process has super-user privileges or the ID
given is that of a group to which the current user belongs."
  (int-errno-call (#_setgid uid)))
  

;;; On Linux, "stat" & friends are implemented in terms of deeper,
;;; darker things that need to know what version of the stat buffer
;;; they're talking about.

#-windows-target
(defun %stat-values (result stat)
  (if (eql 0 (the fixnum result)) 
      (values
       t
       (pref stat :stat.st_mode)
       (pref stat :stat.st_size)
       #+(or linux-target solaris-target)
       (pref stat :stat.st_mtim.tv_sec)
       #-(or linux-target solaris-target)
       (pref stat :stat.st_mtimespec.tv_sec)
       (pref stat :stat.st_ino)
       (pref stat :stat.st_uid)
       (pref stat :stat.st_blksize)
       #+(or linux-target solaris-target)
       (round (pref stat :stat.st_mtim.tv_nsec) 1000)
       #-(or linux-target solaris-target)
       (round (pref stat :stat.st_mtimespec.tv_nsec) 1000)
       (pref stat :stat.st_gid))
      (values nil nil nil nil nil nil nil)))

#+win64-target
(defun %stat-values (result stat)
  (if (eql 0 (the fixnum result)) 
      (values
       t
       (pref stat :_stat64.st_mode)
       (pref stat :_stat64.st_size)
       (pref stat :_stat64.st_mtime)
       (pref stat :_stat64.st_ino)
       (pref stat :_stat64.st_uid)
       #$BUFSIZ
       (pref stat :_stat64.st_mtime)     ; ???
       (pref stat :_stat64.st_gid))
      (values nil nil nil nil nil nil nil nil nil)))

#+windows-target
(defun windows-strip-trailing-slash (namestring)
  (do* ((len (length namestring) (length namestring)))
       ((<= len 3) namestring)
    (let* ((p (1- len))
           (ch (char namestring p)))
      (unless (or (eql ch #\\)
                  (eql ch #\/))
        (return namestring))
      (setq namestring (subseq namestring 0 p)))))


(defun %%stat (name stat)
  (with-filename-cstrs ((cname #+windows-target (windows-strip-trailing-slash name) #-windows-target name))
    (%stat-values
     #+linux-target
     (#_ __xstat #$_STAT_VER_LINUX cname stat)
     #-linux-target
     (int-errno-ffcall (%kernel-import target::kernel-import-lisp-stat)
                       :address cname
                       :address stat
                       :int)
     stat)))

(defun %%fstat (fd stat)
  (%stat-values
   #+linux-target
   (#_ __fxstat #$_STAT_VER_LINUX fd stat)
   #-linux-target
   (int-errno-ffcall (%kernel-import target::kernel-import-lisp-fstat)
                     :int fd
                     :address stat
                     :int)
   stat))

#-windows-target
(defun %%lstat (name stat)
  (with-filename-cstrs ((cname name))
    (%stat-values
     #+linux-target
     (#_ __lxstat #$_STAT_VER_LINUX cname stat)
     #-linux-target
     (#_lstat cname stat)
     stat)))


;;; Returns: (values t mode size mtime inode uid blksize) on success,
;;;          (values nil nil nil nil nil nil nil) otherwise
;;; NAME should be a "native namestring", e.g,, have all lisp pathname
;;; escaping removed.
#-windows-target
(defun %stat (name &optional link-p)
  (rlet ((stat :stat))
    (if link-p
      (%%lstat name stat)
      (%%stat name stat))))

#+windows-target
(defun %stat (name &optional link-p)
  (declare (ignore link-p))
  (rlet ((stat  #+win64-target #>_stat64))
    (%%stat name stat)))

(defun %fstat (fd)
  (rlet ((stat #+win64-target #>_stat64 #-win64-target :stat))
    (%%fstat fd stat)))


(defun %file-kind (mode)
  (when mode
    (let* ((kind (logand mode #$S_IFMT)))
      (cond ((eql kind #$S_IFDIR) :directory)
	    ((eql kind #$S_IFREG) :file)
            #-windows-target
	    ((eql kind #$S_IFLNK) :link)
	    ((eql kind #$S_IFIFO) :pipe)
            #-windows-target
	    ((eql kind #$S_IFSOCK) :socket)
	    ((eql kind #$S_IFCHR) :character-special)
	    (t :special)))))

(defun %unix-file-kind (native-namestring &optional check-for-link)
  (%file-kind (nth-value 1 (%stat native-namestring check-for-link))))

(defun %unix-fd-kind (fd)
  (if (isatty fd)
    :tty
    (%file-kind (nth-value 1 (%fstat fd)))))

#-windows-target
(defun %uts-string (result idx buf)
  (if (>= result 0)
    (%get-cstring (%inc-ptr buf (* #+linux-target #$_UTSNAME_LENGTH
				   #+darwin-target #$_SYS_NAMELEN
                                   #+(or freebsd-target solaris-target) #$SYS_NMLN
                                   idx)))
    "unknown"))

#-windows-target
(defun copy-file-attributes (source-path dest-path)
  "Copy the mode, owner, group and modification time of source-path to dest-path.
   Returns T if succeeded, NIL if some of the attributes couldn't be copied due to
   permission problems.  Any other failures cause an error to be signalled"
  (multiple-value-bind (win mode ignore mtime-sec ignore uid ignore mtime-usec gid)
                       (%stat (native-translated-namestring source-path) t)
    (declare (ignore ignore))
    (unless win
      (error "Cannot get attributes of ~s" source-path))
    (with-filename-cstrs ((cnamestr (native-translated-namestring dest-path)))
      (macrolet ((errchk (form)
                   `(let ((err ,form))
                      (unless (eql err 0)
                        (setq win nil)
                        (when (eql err -1)
                          (setq err (- (%get-errno))))
                        (unless (eql err #$EPERM) (%errno-disp err dest-path))))))
        (errchk (#_chmod cnamestr mode))
        (errchk (%stack-block ((times (record-length (:array (:struct :timeval) 2))))
                  (setf (pref times :timeval.tv_sec) mtime-sec)
                  (setf (pref times :timeval.tv_usec) mtime-usec)
                  (%incf-ptr times (record-length :timeval))
                  (setf (pref times :timeval.tv_sec) mtime-sec)
                  (setf (pref times :timeval.tv_usec) mtime-usec)
                  (%incf-ptr times (- (record-length :timeval)))
                  (#_utimes cnamestr times)))
        (errchk (#_chown cnamestr uid gid))))
    win))

#+linux-target
(defun %uname (idx)
  (%stack-block ((buf (* #$_UTSNAME_LENGTH 6)))  
    (%uts-string (#_uname buf) idx buf)))

#+darwin-target
(defun %uname (idx)
  (%stack-block ((buf (* #$_SYS_NAMELEN 5)))
    (%uts-string (#_uname buf) idx buf)))

#+freebsd-target
(defun %uname (idx)
  (%stack-block ((buf (* #$SYS_NMLN 5)))
    (%uts-string (#___xuname #$SYS_NMLN buf) idx buf)))

#+solaris-target
(defun %uname (idx)
  (%stack-block ((buf (* #$SYS_NMLN 5)))
    (%uts-string (#_uname buf) idx buf)))

#-windows-target
(defun fd-dup (fd)
  (int-errno-call (#_dup fd)))

#+windows-target
(defun fd-dup (fd &key direction inheritable)
  (rlet ((handle #>HANDLE))
    (#_DuplicateHandle (#_GetCurrentProcess)
		       (#__get_osfhandle fd)
		       (#_GetCurrentProcess) 
		       handle
		       0
		       (if inheritable #$TRUE #$FALSE)
		       #$DUPLICATE_SAME_ACCESS)
    (#__open_osfhandle (pref handle #>HANDLE) (case direction
						(:input #$O_RDONLY)
						(:output #$O_WRONLY)
						(t #$O_RDWR)))))
		       

(defun fd-fsync (fd)
  #+windows-target (progn fd 0)
  #-windows-target
  (int-errno-call (#_fsync fd)))

#-windows-target
(progn
(defun fd-get-flags (fd)
  (int-errno-call (#_fcntl fd #$F_GETFL)))

(defun fd-set-flags (fd new)
  (int-errno-call (#_fcntl fd #$F_SETFL :int new)))

(defun fd-set-flag (fd mask)
  (let* ((old (fd-get-flags fd)))
    (if (< old 0)
      old
      (fd-set-flags fd (logior old mask)))))

(defun fd-clear-flag (fd mask)
  (let* ((old (fd-get-flags fd)))
    (if (< old 0) 
      old
      (fd-set-flags fd (logandc2 old mask)))))
)

;;; Assume that any quoting's been removed already.
(defun tilde-expand (namestring)
  (let* ((len (length namestring)))
    (if (or (zerop len)
            (not (eql (schar namestring 0) #\~)))
      namestring
      (if (or (= len 1)
              (eql (schar namestring 1) #\/))
        (concatenate 'string (get-user-home-dir (getuid)) (if (= len 1) "/" (subseq namestring 1)))
        (let* ((slash-pos (position #\/ namestring))
               (user-name (subseq namestring 1 slash-pos))
               (uid (or (get-uid-from-name user-name)
                        (error "Unknown user ~s in namestring ~s" user-name namestring))))
          (concatenate 'string (get-user-home-dir uid) (if slash-pos (subseq namestring slash-pos) "/")))))))


#+windows-target
(defun %windows-realpath (namestring)
  (with-filename-cstrs ((path namestring))
    (do* ((bufsize 256))
         ()
      (%stack-block ((buf bufsize))
        (let* ((nchars (#_GetFullPathNameW path (ash bufsize -1) buf +null-ptr+)))
          (if (eql 0 nchars)
            (return nil)
            (let* ((max (+ nchars nchars 2)))
              (if (> max bufsize)
                (setq bufsize max)
                (let* ((real (get-foreign-namestring buf)))
                  (return (and (%stat real) real)))))))))))

    
;;; This doesn't seem to exist on VxWorks.  It's a POSIX
;;; function AFAIK, so the source should be somewhere ...

(defun %realpath (namestring)
  ;; It's not at all right to just return the namestring here.
  (when (zerop (length namestring))
    (setq namestring (current-directory-name)))
  #+windows-target (%windows-realpath namestring)
  #-windows-target
  (%stack-block ((resultbuf #$PATH_MAX))
    (with-filename-cstrs ((name namestring #|(tilde-expand namestring)|#))
      (let* ((result (#_realpath name resultbuf)))
        (declare (dynamic-extent result))
        (unless (%null-ptr-p result)
          (get-foreign-namestring result))))))

;;; Return fully resolved pathname & file kind, or (values nil nil)

(defun %probe-file-x (namestring)
  (let* ((realpath (%realpath namestring))
	 (kind (if realpath (%unix-file-kind realpath))))
    (if kind
      (values realpath kind)
      (values nil nil))))

;;; The mingw headers define timeval.tv_sec and timeval.tv_usec to be
;;; signed 32-bit quantities.
(macrolet ((timeval-ref (ptr accessor)
             #+windows-target `(logand #xfffffffff (pref ,ptr ,accessor))
             #-windows-target `(pref ,ptr ,accessor))
           (set-timeval-ref (ptr accessor new)
           `(setf (pref ,ptr ,accessor)
             #+windows-target (u32->s32 ,new)
             #-windows-target ,new)))
  
(defun timeval->milliseconds (tv)
    (+ (* 1000 (timeval-ref tv :timeval.tv_sec)) (round (timeval-ref tv :timeval.tv_usec) 1000)))

(defun timeval->microseconds (tv)
    (+ (* 1000000 (timeval-ref tv :timeval.tv_sec)) (timeval-ref tv :timeval.tv_usec)))

(defun %add-timevals (result a b)
  (let* ((seconds (+ (timeval-ref a :timeval.tv_sec) (timeval-ref b :timeval.tv_sec)))
	 (micros (+ (timeval-ref a :timeval.tv_usec) (timeval-ref b :timeval.tv_usec))))
    (if (>= micros 1000000)
      (setq seconds (1+ seconds) micros (- micros 1000000)))
    (set-timeval-ref result :timeval.tv_sec seconds)
    (set-timeval-ref result :timeval.tv_usec micros)
    result))

(defun %sub-timevals (result a b)
  (let* ((seconds (- (timeval-ref a :timeval.tv_sec) (timeval-ref b :timeval.tv_sec)))
	 (micros (- (timeval-ref a :timeval.tv_usec) (timeval-ref b :timeval.tv_usec))))
    (if (< micros 0)
      (setq seconds (1- seconds) micros (+ micros 1000000)))
    (set-timeval-ref result :timeval.tv_sec  seconds)
    (set-timeval-ref result :timeval.tv_usec micros)
    result))

;;; Return T iff the time denoted by the timeval a is not later than the
;;; time denoted by the timeval b.
(defun %timeval<= (a b)
  (let* ((asec (timeval-ref a :timeval.tv_sec))
         (bsec (timeval-ref b :timeval.tv_sec)))
    (or (< asec bsec)
        (and (= asec bsec)
             (< (timeval-ref a :timeval.tv_usec)
                (timeval-ref b :timeval.tv_usec))))))

); windows signed nonsense.

#-windows-target
(defun %%rusage (usage &optional (who #$RUSAGE_SELF))
  (int-errno-call (#_getrusage who usage)))




(defun %file-write-date (namestring)
  (let* ((date (nth-value 3 (%stat namestring))))
    (if date
      (+ date unix-to-universal-time))))

#-windows-target
(defun %file-author (namestring)
  (let* ((uid (nth-value 5 (%stat namestring))))
    (if uid
      (with-macptrs ((pw (#_getpwuid uid)))
        (unless (%null-ptr-p pw)
          (without-interrupts
           (%get-cstring (pref pw :passwd.pw_name))))))))

#-windows-target
(defun %utimes (namestring)
  (with-filename-cstrs ((cnamestring namestring))
    (let* ((err (#_utimes cnamestring (%null-ptr))))
      (declare (fixnum err))
      (or (eql err 0)
          (%errno-disp err namestring)))))
         

#-windows-target
(defun get-uid-from-name (name)
  (with-cstrs ((name name))
    (let* ((pwent (#_getpwnam name)))
      (unless (%null-ptr-p pwent)
        (pref pwent :passwd.pw_uid)))))


(defun isatty (fd)
  #+windows-target (declare (ignore fd))
  #+windows-target nil
  #-windows-target
  (= 1 (#_isatty fd)))

(defun %open-dir (namestring)
  (with-filename-cstrs ((name namestring))
    (let* ((DIR (ff-call (%kernel-import target::kernel-import-lisp-opendir)
                         :address name
                         :address)))
      (unless (%null-ptr-p DIR)
	DIR))))

(defun close-dir (dir)
  (ff-call (%kernel-import target::kernel-import-lisp-closedir)
           :address dir
           :int))

(defun %read-dir (dir)
  (let* ((res (ff-call (%kernel-import target::kernel-import-lisp-readdir)
                       :address dir
                       :address)))
    (unless (%null-ptr-p res)
      (get-foreign-namestring (pref res
                                    #+windows-target :_wdirent.d_name
                                    #-windows-target :dirent.d_name)))))


#-windows-target
(defun tcgetpgrp (fd)
  (#_tcgetpgrp fd))

(defun getpid ()
  "Return the ID of the OpenMCL OS process."
  #-windows-target
  (int-errno-call (#_getpid))
  #+windows-target (#_GetCurrentProcessId))


(defun getuid ()
  "Return the (real) user ID of the current user."
  #+windows-target 0
  #-windows-target (int-errno-call (#_getuid)))

(defun get-user-home-dir (userid)
  "Look up and return the defined home directory of the user identified
by uid. This value comes from the OS user database, not from the $HOME
environment variable. Returns NIL if there is no user with the ID uid."
  #+windows-target
  (declare (ignore userid))
  #+windows-target
  (dolist (k '(#||"HOME"||# "USERPROFILE")) 
    (with-native-utf-16-cstrs ((key k))
      (let* ((p (#__wgetenv key)))
        (unless (%null-ptr-p p)
          (return (get-foreign-namestring p))))))
  #-windows-target
  (rlet ((pwd :passwd)
         (result :address))
    (do* ((buflen 512 (* 2 buflen)))
         ()
      (%stack-block ((buf buflen))
        (let* ((err
                #-solaris-target
                 (#_getpwuid_r userid pwd buf buflen result)
                 #+solaris-target
                 (external-call "__posix_getpwuid_r"
                                :uid_t userid
                                :address pwd
                                :address buf
                                :int buflen
                                :address result
                                :int)))
          (if (eql 0 err)
            (return (get-foreign-namestring (pref pwd :passwd.pw_dir)))
            (unless (eql err #$ERANGE)
              (return nil))))))))

(defun %delete-file (name)
  (with-cstrs ((n name))
    (int-errno-call (#+windows-target #__unlink #-windows-target #_unlink n))))

(defun os-command (string)
  "Invoke the Posix function system(), which invokes the user's default
system shell (such as sh or tcsh) as a new process, and has that shell
execute command-line.

If the shell was able to find the command specified in command-line, then
exit-code is the exit code of that command. If not, it is the exit code
of the shell itself."
  (with-cstrs ((s string))
    (#_system s)))

(defun %strerror (errno)
  (declare (fixnum errno))
  (if (< errno 0)
    (setq errno (- errno)))
  (with-macptrs (p)
    (%setf-macptr p (#_strerror errno))
    (if (%null-ptr-p p)
      (format nil "OS Error %d" errno)
      (%get-cstring p))))

#+windows-target
(defun %windows-error-string (error-number)  
  (rlet ((pbuffer :address +null-ptr+))
    (if (eql 0
             (#_FormatMessageW (logior #$FORMAT_MESSAGE_ALLOCATE_BUFFER
                                       #$FORMAT_MESSAGE_FROM_SYSTEM
                                       #$FORMAT_MESSAGE_IGNORE_INSERTS
                                       #$FORMAT_MESSAGE_MAX_WIDTH_MASK)
                               +null-ptr+
                               (abs error-number)
                               0                 ; default langid, more-or-less
                               pbuffer
                               0
                               +null-ptr+))
      (format nil "Windows error ~d" (abs error-number))
      (let* ((p (%get-ptr pbuffer))
             (q (%get-native-utf-16-cstring p)))
        (#_LocalFree p)
        q))))
        
                      

;;; Kind of has something to do with files, and doesn't work in level-0.
#+(or linux-target freebsd-target solaris-target)
(defun close-shared-library (lib &key (completely t))
  "If completely is T, set the reference count of library to 0. Otherwise,
decrements it by 1. In either case, if the reference count becomes 0,
close-shared-library frees all memory resources consumed library and causes
any EXTERNAL-ENTRY-POINTs known to be defined by it to become unresolved."
  (let* ((lib (if (typep lib 'string)
		(or (shared-library-with-name lib)
		    (error "Shared library ~s not found." lib))
		(require-type lib 'shlib)))
	 (handle (shlib.handle lib)))
      (when handle
	(let* ((found nil)
	       (base (shlib.base lib)))
	  (do* ()
	       ((progn		  
		  (#_dlclose handle)
		  (or (not (setq found (shlib-containing-address base)))
		      (not completely)))))
	  (when (not found)
	    (setf (shlib.pathname lib) nil
	      (shlib.base lib) nil
              (shlib.handle lib) nil
	      (shlib.map lib) nil)
            (unload-foreign-variables lib)
	    (unload-library-entrypoints lib))))))

#+darwin-target
;; completely specifies whether to remove it totally from our list
(defun close-shared-library (lib &key (completely nil))
  "If completely is T, set the reference count of library to 0. Otherwise,
decrements it by 1. In either case, if the reference count becomes 0,
close-shared-library frees all memory resources consumed library and causes
any EXTERNAL-ENTRY-POINTs known to be defined by it to become unresolved."
  (let* ((lib (if (typep lib 'string)
		  (or (shared-library-with-name lib)
		      (error "Shared library ~s not found." lib))
		(require-type lib 'shlib))))
    ;; no possible danger closing libsystem since dylibs can't be closed
    (cond
     ((or (not (shlib.map lib)) (not (shlib.base lib)))
      (error "Shared library ~s uninitialized." (shlib.soname lib)))
     ((and (not (%null-ptr-p (shlib.map lib)))
	   (%null-ptr-p (shlib.base lib)))
      (warn "Dynamic libraries cannot be closed on Darwin."))
     ((and (%null-ptr-p (shlib.map lib))
	   (not (%null-ptr-p (shlib.base lib))))
      ;; we have a bundle type library not sure what to do with the
      ;; completely flag when we open the same bundle more than once,
      ;; Darwin gives back a new module address, so we have multiple
      ;; entries on *shared-libraries* the best we can do is unlink
      ;; the module asked for (or our best guess based on name) and
      ;; invalidate any entries which refer to this container
      (if (= 0 (#_NSUnLinkModule (shlib.base lib) #$NSUNLINKMODULE_OPTION_NONE))
	  (error "Unable to close shared library, NSUnlinkModule failed.")
	(progn
	  (setf (shlib.map lib) nil
		(shlib.base lib) nil)
	  (unload-library-entrypoints lib)
	  (when completely
	    (setq *shared-libraries* (delete lib *shared-libraries*)))))))))



;;; Foreign (unix) processes.

(defun call-with-string-vector (function strings)
  (let ((bufsize (reduce #'+ strings
			 :key #'(lambda (s) (1+ (length (string s))))))
	(argvsize (ash (1+ (length strings)) target::word-shift))
	(bufpos 0)
	(argvpos 0))
    (%stack-block ((buf bufsize) (argv argvsize))
      (flet ((init (s)
	     (multiple-value-bind (sstr start end) (get-sstring s)
               (declare (fixnum start end))
	       (let ((len (- end start)))
                 (declare (fixnum len))
                 (do* ((i 0 (1+ i))
                       (start start (1+ start))
                       (bufpos bufpos (1+ bufpos)))
                      ((= i len))
                   (setf (%get-unsigned-byte buf bufpos)
                         (logand #xff (%scharcode sstr start))))
		 (setf (%get-byte buf (%i+ bufpos len)) 0)
		 (setf (%get-ptr argv argvpos) (%inc-ptr buf bufpos))
		 (setq bufpos (%i+ bufpos len 1))
		 (setq argvpos (%i+ argvpos target::node-size))))))
	(declare (dynamic-extent #'init))
	(map nil #'init strings))
      (setf (%get-ptr argv argvpos) (%null-ptr))
      (funcall function argv))))

(defmacro with-string-vector ((var &rest strings) &body body)
  `(call-with-string-vector #'(lambda (,var) ,@body) ,@strings))

(defloadvar *max-os-open-files* #-windows-target (#_getdtablesize) #+windows-target 32)

(defun pipe ()
  ;;  (rlet ((filedes (:array :int 2)))
  (%stack-block ((filedes 8))
    (let* ((status (ff-call (%kernel-import target::kernel-import-lisp-pipe)
                            :address filedes :int))
           (errno (if (eql status 0) 0 (%get-errno))))
      (unless (zerop status)
        (when (or (eql errno (- #$EMFILE))
                  (eql errno (- #$ENFILE)))
          (gc)
          (drain-termination-queue)
          (setq status (ff-call (%kernel-import target::kernel-import-lisp-pipe)
                            :address filedes :int)
                errno (if (zerop status) 0 (%get-errno)))))
      (if (zerop status)
        (values (paref filedes (:array :int)  0) (paref filedes (:array :int)  1))
        (%errno-disp errno)))))

#-windows-target
(progn
(defun %execvp (argv)
  (#_execvp (%get-ptr argv) argv)
  (#_exit #$EX_OSERR))

(defun exec-with-io-redirection (new-in new-out new-err argv)
  (#_setpgid 0 0)
  (if new-in (#_dup2 new-in 0))
  (if new-out (#_dup2 new-out 1))
  (if new-err (#_dup2 new-err 2))
  (do* ((fd 3 (1+ fd)))
       ((= fd *max-os-open-files*) (%execvp argv))
    (declare (fixnum fd))
    (#_close fd)))





(defstruct external-process
  pid
  %status
  %exit-code
  pty
  input
  output
  error
  status-hook
  plist
  token
  core
  args
  (signal (make-semaphore))
  (completed (make-semaphore))
  watched-fd
  watched-stream
  )

(defmethod print-object ((p external-process) stream)
  (print-unreadable-object (p stream :type t :identity t)
    (let* ((status (external-process-%status p)))
      (let* ((*print-length* 3))
	(format stream "~a" (external-process-args p)))
      (format stream "[~d] (~a" (external-process-pid p) status)
      (unless (eq status :running)
	(format stream " : ~d" (external-process-%exit-code p)))
      (format stream ")"))))

(defun get-descriptor-for (object proc close-in-parent close-on-error
				  &rest keys &key direction (element-type 'character)
				  &allow-other-keys)
  (etypecase object
    ((eql t)
     (values nil nil close-in-parent close-on-error))
    (null
     (let* ((null-device #+windows-target "nul" #-windows-target "/dev/null")
	    (fd (fd-open null-device (case direction
				       (:input #$O_RDONLY)
				       (:output #$O_WRONLY)
				       (t #$O_RDWR)))))
       (if (< fd 0)
	 (signal-file-error fd null-device))
       (values fd nil (cons fd close-in-parent) (cons fd close-on-error))))
    ((eql :stream)
     (multiple-value-bind (read-pipe write-pipe) (pipe)
       (case direction
	 (:input
	  (values read-pipe
		  (make-fd-stream write-pipe
				  :direction :output
                                  :element-type element-type
				  :interactive nil
                                  :basic t
                                  :auto-close t)
		  (cons read-pipe close-in-parent)
		  (cons write-pipe close-on-error)))
	 (:output
	  (values write-pipe
		  (make-fd-stream read-pipe
				  :direction :input
                                  :element-type element-type
				  :interactive nil
                                  :basic t
                                  :auto-close t)
		  (cons write-pipe close-in-parent)
		  (cons read-pipe close-on-error)))
	 (t
	  (fd-close read-pipe)
	  (fd-close write-pipe)
	  (report-bad-arg direction '(member :input :output))))))
    ((or pathname string)
     (with-open-stream (file (apply #'open object keys))
       (let* ((fd (fd-dup (ioblock-device (stream-ioblock file t)))))
         (values fd
                 nil
                 (cons fd close-in-parent)
                 (cons fd close-on-error)))))
    (fd-stream
     (let ((fd (fd-dup (ioblock-device (stream-ioblock object t)))))
       (values fd
	       nil
	       (cons fd close-in-parent)
	       (cons fd close-on-error))))
    (stream
     (ecase direction
       (:input
	(with-cstrs ((template "lisp-tempXXXXXX"))
	  (let* ((fd (#_mkstemp template)))
	    (if (< fd 0)
	      (%errno-disp fd))
	    (#_unlink template)
	    (loop
              (multiple-value-bind (line no-newline)
                  (read-line object nil nil)
                (unless line
                  (return))
                (let* ((len (length line)))
                  (%stack-block ((buf (1+ len)))
                    (%cstr-pointer line buf)
                    (fd-write fd buf len)
                    (if no-newline
                      (return))
                    (setf (%get-byte buf) (char-code #\newline))
                    (fd-write fd buf 1)))))
	    (fd-lseek fd 0 #$SEEK_SET)
	    (values fd nil (cons fd close-in-parent) (cons fd close-on-error)))))
       (:output
	(multiple-value-bind (read-pipe write-pipe) (pipe)
          (setf (external-process-watched-fd proc) read-pipe
                (external-process-watched-stream proc) object)
          (incf (car (external-process-token proc)))
	  (values write-pipe
		  nil
		  (cons write-pipe close-in-parent)
		  (cons read-pipe close-on-error))))))))

(let* ((external-processes ())
       (external-processes-lock (make-lock)))
  (defun add-external-process (p)
    (with-lock-grabbed (external-processes-lock)
      (push p external-processes)))
  (defun remove-external-process (p)
    (with-lock-grabbed (external-processes-lock)
      (setq external-processes (delete p external-processes))))
  ;; Likewise
  (defun external-processes ()
    (with-lock-grabbed (external-processes-lock)
      (copy-list external-processes)))
  )


(defmacro wtermsig (status)
  `(ldb (byte 7 0) ,status))

(defmacro wexitstatus (status)
  `(ldb (byte 8 8) (the fixnum ,status)))

(defmacro wstopsig (status)
  `(wexitstatus ,status))

(defmacro wifexited (status)
  `(eql (wtermsig ,status) 0))

(defmacro wifstopped (status)
  `(eql #x7f (ldb (byte 7 0) (the fixnum ,status))))

(defun monitor-external-process (p)
  (let* ((in-fd (external-process-watched-fd p))
         (out-stream (external-process-watched-stream p))
         (token (external-process-token p))
         (terminated))
    (loop
      (when (and terminated (null in-fd))
        (signal-semaphore (external-process-completed p))
        (return))
      (when in-fd
        (when (fd-input-available-p in-fd 1000)
          (%stack-block ((buf 1024))
            (let* ((n (fd-read in-fd buf 1024)))
              (declare (fixnum n))
              (if (<= n 0)
                (progn
                  (without-interrupts
                   (decf (car token))
                   (fd-close in-fd)
                   (setq in-fd nil)))
                (let* ((string (make-string 1024)))
                  (declare (dynamic-extent string))
                  (%str-from-ptr buf n string)
                  (write-sequence string out-stream :end n)))))))
      (let* ((statusflags (check-pid (external-process-pid p)
                                     (logior
                                      (if in-fd #$WNOHANG 0)
                                      #$WUNTRACED)))
             (oldstatus (external-process-%status p)))
        (cond ((null statusflags)
               (remove-external-process p)
               (setq terminated t))
              ((eq statusflags t))	; Running.
              (t
               (multiple-value-bind (status code core)
                   (cond ((wifstopped statusflags)
                          (values :stopped (wstopsig statusflags)))
                         ((wifexited statusflags)
                          (values :exited (wexitstatus statusflags)))
                         (t
                          (let* ((signal (wtermsig statusflags)))
                            (declare (fixnum signal))
                            (values
                             (if (or (= signal #$SIGSTOP)
                                     (= signal #$SIGTSTP)
                                     (= signal #$SIGTTIN)
                                     (= signal #$SIGTTOU))
                               :stopped
                               :signaled)
                             signal
                             (logtest #-solaris-target #$WCOREFLAG
                                      #+solaris-target #$WCOREFLG
                                      statusflags)))))
                 (setf (external-process-%status p) status
                       (external-process-%exit-code p) code
                       (external-process-core p) core)
                 (let* ((status-hook (external-process-status-hook p)))
                   (when (and status-hook (not (eq oldstatus status)))
                     (funcall status-hook p)))
                 (when (or (eq status :exited)
                           (eq status :signaled))
                   (remove-external-process p)
                   (setq terminated t)))))))))
      
(defun run-external-process (proc in-fd out-fd error-fd &optional env)
  ;; type-check the env variable
  (dolist (pair env)
    (destructuring-bind (var . val) pair
      (assert (typep var '(or string symbol character)))
      (assert (typep val 'string)))) 
  (call-with-string-vector
   #'(lambda (argv)
       (let* ((child-pid (#_fork)))
	 (declare (fixnum child-pid))
	 (cond ((zerop child-pid)
		;; Running in the child; do an exec
                (dolist (pair env)
                  (setenv (string (car pair)) (cdr pair)))
		(without-interrupts
		 (exec-with-io-redirection
		  in-fd out-fd error-fd argv)))
	       ((> child-pid 0)
		;; Running in the parent: success
		(setf (external-process-pid proc) child-pid)
		(add-external-process proc)
		(signal-semaphore (external-process-signal proc))
                (monitor-external-process proc))
               (t
                ;; Fork failed
                (setf (external-process-%status proc) :error
                      (external-process-%exit-code proc) (%get-errno))
                (signal-semaphore (external-process-signal proc))))))
   (external-process-args proc)))

		
(defun run-program (program args &key
			    (wait t) pty
			    input if-input-does-not-exist
			    output (if-output-exists :error)
			    (error :output) (if-error-exists :error)
			    status-hook (element-type 'character)
                            env)
  "Invoke an external program as an OS subprocess of lisp."
  (declare (ignore pty))
  (unless (every #'(lambda (a) (typep a 'simple-string)) args)
    (error "Program args must all be simple strings : ~s" args))
  (push (native-untranslated-namestring program) args)
  (let* ((token (list 0))
	 (in-fd nil)
	 (in-stream nil)
	 (out-fd nil)
	 (out-stream nil)
	 (error-fd nil)
	 (error-stream nil)
	 (close-in-parent nil)
	 (close-on-error nil)
	 (proc
          (make-external-process
           :pid nil
           :args args
           :%status :running
           :input nil
           :output nil
           :error nil
           :token token
           :status-hook status-hook)))
    (unwind-protect
	 (progn
	   (multiple-value-setq (in-fd in-stream close-in-parent close-on-error)
	     (get-descriptor-for input proc  nil nil :direction :input
				 :if-does-not-exist if-input-does-not-exist
                                 :element-type element-type))
	   (multiple-value-setq (out-fd out-stream close-in-parent close-on-error)
	     (get-descriptor-for output proc close-in-parent close-on-error
				 :direction :output
				 :if-exists if-output-exists
                                 :element-type element-type))
	   (multiple-value-setq (error-fd error-stream close-in-parent close-on-error)
	     (if (eq error :output)
	       (values out-fd out-stream close-in-parent close-on-error)
	       (get-descriptor-for error proc close-in-parent close-on-error
				   :direction :output
				   :if-exists if-error-exists
                                   :element-type element-type)))
	   (setf (external-process-input proc) in-stream
                 (external-process-output proc) out-stream
                 (external-process-error proc) error-stream)
           (process-run-function
            (list :name
                  (format nil "Monitor thread for external process ~a" args)
                  :stack-size (ash 128 10)
                  :vstack-size (ash 128 10)
                  :tstack-size (ash 128 10))
            #'run-external-process proc in-fd out-fd error-fd env)
           (wait-on-semaphore (external-process-signal proc))
           )
      (dolist (fd close-in-parent) (fd-close fd))
      (unless (external-process-pid proc)
        (dolist (fd close-on-error) (fd-close fd)))
      (when (and wait (external-process-pid proc))
        (with-interrupts-enabled
            (wait-on-semaphore (external-process-completed proc)))))
    (and (or (external-process-pid proc)
             (if (eq (external-process-%status proc) :error)
               (error "Fork failed in ~s: ~s" proc (%strerror (external-process-%exit-code proc)))))
             (external-process-%status proc)) proc))




(defmacro wifsignaled (status)
  (let* ((statname (gensym)))
    `(let* ((,statname ,status))
      (and (not (wifstopped ,statname)) (not (wifexited ,statname))))))


(defun check-pid (pid &optional (flags (logior  #$WNOHANG #$WUNTRACED)))
  (declare (fixnum pid))
  (rlet ((status :signed))
    (let* ((retval (ff-call-ignoring-eintr (#_waitpid pid status flags))))
      (declare (fixnum retval))
      (if (= retval pid)
	(pref status :signed)
	(zerop retval)))))





(defun external-process-wait (proc &optional check-stopped)
  (process-wait "external-process-wait"
		#'(lambda ()
		    (case (external-process-%status proc)
		      (:running)
		      (:stopped
		       (when check-stopped
			 t))
		      (t
		       (when (zerop (car (external-process-token proc)))
			 t))))))

(defun external-process-status (proc)
  "Return information about whether an OS subprocess is running; or, if
not, why not; and what its result code was if it completed."
  (require-type proc 'external-process)
  (values (external-process-%status proc)
	  (external-process-%exit-code proc)))

(defun external-process-input-stream (proc)
  "Return the lisp stream which is used to write input to a given OS
subprocess, if it has one."
  (require-type proc 'external-process)
  (external-process-input proc))

(defun external-process-output-stream (proc)
  "Return the lisp stream which is used to read output from a given OS
subprocess, if there is one."
  (require-type proc 'external-process)
  (external-process-output proc))

(defun external-process-error-stream (proc)
  "Return the stream which is used to read error output from a given OS
subprocess, if it has one."
  (require-type proc 'external-process)
  (external-process-error proc))

(defun external-process-id (proc)
  "Return the process id of an OS subprocess, a positive integer which
identifies it."
  (require-type proc 'external-process)
  (external-process-pid proc))
  
(defun signal-external-process (proc signal)
  "Send the specified signal to the specified external process.  (Typically,
it would only be useful to call this function if the EXTERNAL-PROCESS was
created with :WAIT NIL.) Return T if successful; signal an error otherwise."
  (require-type proc 'external-process)
  (let* ((pid (external-process-pid proc)))
    (when pid
      (int-errno-call (#_kill pid signal)))))

) ; #-windows-target (progn

#+windows-target
(progn
(defun temp-file-name (prefix)
  "Returns a unique name for a temporary file, residing in system temp
space, and prefixed with PREFIX."
  (rlet ((buffer (:array :wchar_t #.#$MAX_PATH)))
    (#_GetTempPathW #$MAX_PATH buffer)
    (with-filename-cstrs ((c-prefix prefix)) 
      (#_GetTempFileNameW buffer c-prefix 0 buffer)
      (%get-native-utf-16-cstring buffer))))
  
(defun get-descriptor-for (object proc close-in-parent close-on-error
                                  &rest keys &key direction (element-type 'character)
                                  &allow-other-keys)
  (etypecase object
    ((eql t)
     (values nil nil close-in-parent close-on-error))
    (null
     (let* ((null-device "nul")
	    (fd (fd-open null-device (case direction
				       (:input #$O_RDONLY)
				       (:output #$O_WRONLY)
				       (t #$O_RDWR)))))
       (if (< fd 0)
	 (signal-file-error fd null-device))
       (values fd nil (cons fd close-in-parent) (cons fd close-on-error))))
    ((eql :stream)
     (multiple-value-bind (read-pipe write-pipe) (pipe)
       (case direction
	 (:input
	  (values read-pipe
		  (make-fd-stream (fd-uninheritable write-pipe :direction :output)
				  :direction :output
                                  :element-type element-type
				  :interactive nil
                                  :basic t
                                  :auto-close t)
		  (cons read-pipe close-in-parent)
		  (cons write-pipe close-on-error)))
	 (:output
	  (values write-pipe
		  (make-fd-stream (fd-uninheritable read-pipe :direction :input)
				  :direction :input
                                  :element-type element-type
				  :interactive nil
                                  :basic t
                                  :auto-close t)
		  (cons write-pipe close-in-parent)
		  (cons read-pipe close-on-error)))
	 (t
	  (fd-close read-pipe)
	  (fd-close write-pipe)
	  (report-bad-arg direction '(member :input :output))))))
    ((or pathname string)
     (with-open-stream (file (apply #'open object keys))
       (let* ((fd (fd-dup (ioblock-device (stream-ioblock file t)) :direction direction :inheritable t)))
         (values fd
                 nil
                 (cons fd close-in-parent)
                 (cons fd close-on-error)))))
    (fd-stream
     (let ((fd (fd-dup (ioblock-device (stream-ioblock object t)))))
       (values fd
	       nil
	       (cons fd close-in-parent)
	       (cons fd close-on-error))))
    (stream
     (ecase direction
       (:input
	(let* ((tempname (temp-file-name "lisp-temp"))
	       (fd (fd-open tempname #$O_RDWR)))
	  (if (< fd 0)
	    (%errno-disp fd))
	  (loop
            (multiple-value-bind (line no-newline)
                (read-line object nil nil)
              (unless line
                (return))
              (let* ((len (length line)))
                (%stack-block ((buf (1+ len)))
                  (%cstr-pointer line buf)
                  (fd-write fd buf len)
                  (if no-newline
                    (return))
                  (setf (%get-byte buf) (char-code #\newline))
                  (fd-write fd buf 1)))))
	  (fd-lseek fd 0 #$SEEK_SET)
	  (values fd nil (cons fd close-in-parent) (cons fd close-on-error))))
       (:output
	(multiple-value-bind (read-pipe write-pipe) (pipe)
          (setf (external-process-watched-fd proc) read-pipe
                (external-process-watched-stream proc) object)
          (incf (car (external-process-token proc)))
	  (values write-pipe
		  nil
		  (cons write-pipe close-in-parent)
		  (cons read-pipe close-on-error))))))))

(defstruct external-process
  pid
  %status
  %exit-code
  pty
  input
  output
  error
  status-hook
  plist
  token
  core
  args
  (signal (make-semaphore))
  (completed (make-semaphore))
  watched-fd
  watched-stream
  )

(defun external-process-status (proc)
  "Return information about whether an OS subprocess is running; or, if
not, why not; and what its result code was if it completed."
  (require-type proc 'external-process)
  (values (external-process-%status proc)
	  (external-process-%exit-code proc)))


(defmethod print-object ((p external-process) stream)
  (print-unreadable-object (p stream :type t :identity t)
    (let* ((status (external-process-%status p)))
      (let* ((*print-length* 3))
        (format stream "~a" (external-process-args p)))
      (format stream "[~d] (~a" (external-process-pid p) status)
      (unless (eq status :running)
        (format stream " : ~d" (external-process-%exit-code p)))
      (format stream ")"))))

(defun run-program (program args &key
                            (wait t) pty
                            input if-input-does-not-exist
                            output (if-output-exists :error)
                            (error :output) (if-error-exists :error)
                            status-hook (element-type 'character)
                            env)
  "Invoke an external program as an OS subprocess of lisp."
  (declare (ignore pty))
  (unless (every #'(lambda (a) (typep a 'simple-string)) args)
    (error "Program args must all be simple strings : ~s" args))
  (push program args)
  (let* ((token (list 0))
         (in-fd nil)
         (in-stream nil)
         (out-fd nil)
         (out-stream nil)
         (error-fd nil)
         (error-stream nil)
         (close-in-parent nil)
         (close-on-error nil)
         (proc
          (make-external-process
           :pid nil
           :args args
           :%status :running
           :input nil
           :output nil
           :error nil
           :token token
           :status-hook status-hook)))
    (unwind-protect
         (progn
           (multiple-value-setq (in-fd in-stream close-in-parent close-on-error)
             (get-descriptor-for input proc  nil nil :direction :input
                                 :if-does-not-exist if-input-does-not-exist
                                 :element-type element-type))
           (multiple-value-setq (out-fd out-stream close-in-parent close-on-error)
             (get-descriptor-for output proc close-in-parent close-on-error
                                 :direction :output
                                 :if-exists if-output-exists
                                 :element-type element-type))
           (multiple-value-setq (error-fd error-stream close-in-parent close-on-error)
             (if (eq error :output)
               (values out-fd out-stream close-in-parent close-on-error)
               (get-descriptor-for error proc close-in-parent close-on-error
                                   :direction :output
                                   :if-exists if-error-exists
                                   :element-type element-type)))
           (setf (external-process-input proc) in-stream
                 (external-process-output proc) out-stream
                 (external-process-error proc) error-stream)
           (process-run-function
            (format nil "Monitor thread for external process ~a" args)
                    
            #'run-external-process proc in-fd out-fd error-fd env)
           (wait-on-semaphore (external-process-signal proc))
           )
      (dolist (fd close-in-parent) (fd-close fd))
      (if (external-process-pid proc)
        (when (and wait (external-process-pid proc))
          (with-interrupts-enabled
              (wait-on-semaphore (external-process-completed proc))))
        (progn
          (dolist (fd close-on-error) (fd-close fd))
          (error "Process execution failed"))))
    proc))

(let* ((external-processes ())
       (external-processes-lock (make-lock)))
  (defun add-external-process (p)
    (with-lock-grabbed (external-processes-lock)
      (push p external-processes)))
  (defun remove-external-process (p)
    (with-lock-grabbed (external-processes-lock)
      (setq external-processes (delete p external-processes))))
  ;; Likewise
  (defun external-processes ()
    (with-lock-grabbed (external-processes-lock)
      (copy-list external-processes)))
  )




(defun run-external-process (proc in-fd out-fd error-fd &optional env)
  (let* ((args (external-process-args proc))
         (child-pid (exec-with-io-redirection in-fd out-fd error-fd args proc)))
    (when child-pid
      (setf (external-process-pid proc) child-pid)
      (add-external-process proc)
      (signal-semaphore (external-process-signal proc))
      (monitor-external-process proc))))

(defun join-strings (strings)
  (reduce (lambda (left right) (concatenate 'string left " " right)) strings))

(defun exec-with-io-redirection (new-in new-out new-err args proc)
  (with-filename-cstrs ((command (join-strings args)))
    (rletz ((proc-info #>PROCESS_INFORMATION)
            (si #>STARTUPINFO))
      (setf (pref si #>STARTUPINFO.cb) (record-length #>STARTUPINFO))
      (setf (pref si #>STARTUPINFO.dwFlags)
            (logior #$STARTF_USESTDHANDLES #$STARTF_USESHOWWINDOW))
      (setf (pref si #>STARTUPINFO.wShowWindow) #$SW_HIDE)
      (setf (pref si #>STARTUPINFO.hStdInput)
            (%int-to-ptr (#__get_osfhandle (or new-in 0))))
      (setf (pref si #>STARTUPINFO.hStdOutput)
            (%int-to-ptr (#__get_osfhandle (or new-out 1))))
      (setf (pref si #>STARTUPINFO.hStdError)
            (%int-to-ptr (#__get_osfhandle (or new-err 2))))
      (if (zerop (#_CreateProcessW (%null-ptr)
                                   command
                                   (%null-ptr)
                                   (%null-ptr)
                                   1
                                   #$CREATE_NEW_CONSOLE
                                   (%null-ptr)
                                   (%null-ptr)
                                   si
                                   proc-info))
        (setf (external-process-%status proc) :error
              (external-process-%exit-code proc) (#_GetLastError))
        (#_CloseHandle (pref proc-info #>PROCESS_INFORMATION.hThread)))
      (pref proc-info #>PROCESS_INFORMATION.hProcess))))

(defun fd-uninheritable (fd &key direction)
  (let ((new-fd (fd-dup fd :direction direction)))
    (fd-close fd)
    new-fd))

(defun monitor-external-process (p)
  (let* ((in-fd (external-process-watched-fd p))
         (out-stream (external-process-watched-stream p))
         (token (external-process-token p))
         (terminated))
    (loop
      (when terminated
        (without-interrupts
         (decf (car token))
         (if in-fd (fd-close in-fd))
         (setq in-fd nil)
         (rlet ((code #>DWORD))
           (loop
             (#_GetExitCodeProcess (external-process-pid p) code)
             (unless (eql (pref code #>DWORD) #$STILL_ACTIVE)
               (return)))
           (#_SleepEx 10 #$TRUE)
           (setf (external-process-%exit-code p) (pref code #>DWORD)))
         (#_CloseHandle (external-process-pid p))
         (setf (external-process-pid p) nil)
         (setf (external-process-%status p) :exited)
         (let ((status-hook (external-process-status-hook p)))
           (when status-hook
             (funcall status-hook p)))
         (remove-external-process p)
         (signal-semaphore (external-process-completed p))
         (return)))	 
      (if in-fd
        (rlet ((handles (:array #>HANDLE 2)))
          (setf (paref handles (:array #>HANDLE) 0) (external-process-pid p))
          (setf (paref handles (:array #>HANDLE) 1) (#__get_osfhandle in-fd))
          (let ((rc (ignoring-eintr
                     (let* ((code (#_WaitForMultipleObjectsEx 2 handles #$FALSE #$INFINITE #$true)))
                       (if (eql code #$WAIT_IO_COMPLETION)
                         (- #$EINTR)
                         code)))))
            (if (eq rc #$WAIT_OBJECT_0)
              (setf terminated t)
              (%stack-block ((buf 1024))
                (let* ((n (fd-read in-fd buf 1024)))
                  (declare (fixnum n))
                  (if (<= n 0)
                    (setf terminated t)
                    (let* ((string (make-string 1024)))
                      (declare (dynamic-extent string))
                      (%str-from-ptr buf n string)
                      (write-sequence string out-stream :end n))))))))
        (progn
          (ignoring-eintr
           (let* ((code (#_WaitForSingleObjectEx (external-process-pid p) #$INFINITE #$true)))
             (if (eql code #$WAIT_IO_COMPLETION)
               (- #$EINTR)
               code)))
          (setf terminated t))))))
  

)                                   ; #+windows-target (progn

;;; EOF on a TTY is transient, but I'm less sure of other cases.
(defun eof-transient-p (fd)
  (case (%unix-fd-kind fd)
    (:tty t)
    #+windows-target (:character-special t)
    (t nil)))


(defstruct (shared-resource (:constructor make-shared-resource (name)))
  (name)
  (lock (make-lock))
  (primary-owner *current-process*)
  (primary-owner-notify (make-semaphore))
  (current-owner nil)
  (requestors (make-dll-header)))

(defstruct (shared-resource-request
	     (:constructor make-shared-resource-request (process))
	     (:include dll-node))
  process
  (signal (make-semaphore)))
	     

;; Returns NIL if already owned by calling thread, T otherwise
(defun %acquire-shared-resource (resource  &optional verbose)
  (let* ((current *current-process*))
    (with-lock-grabbed ((shared-resource-lock resource))
      (let* ((secondary (shared-resource-current-owner resource)))
	(if (or (eq current secondary)
		(and (null secondary)
		     (eq current (shared-resource-primary-owner resource))))
	  (return-from %acquire-shared-resource nil))))
    (let* ((request (make-shared-resource-request *current-process*)))
      (when verbose
	(format t "~%~%;;;~%;;; ~a requires access to ~a~%;;;~%~%"
		*current-process* (shared-resource-name resource)))
      (with-lock-grabbed ((shared-resource-lock resource))
	(append-dll-node request (shared-resource-requestors resource)))
      (wait-on-semaphore (shared-resource-request-signal request))
      (assert (eq current (shared-resource-current-owner resource)))
      (when verbose
	(format t "~%~%;;;~%;;; ~a is now owned by ~a~%;;;~%~%"
		(shared-resource-name resource) current))
      t)))

;;; If we're the primary owner and there is no secondary owner, do nothing.
;;; If we're the secondary owner, cease being the secondary owner.
(defun %release-shared-resource (r)
  (let* ((not-any-owner ()))
    (with-lock-grabbed ((shared-resource-lock r))
      (let* ((current *current-process*)
	     (primary (shared-resource-primary-owner r))
	     (secondary (shared-resource-current-owner r)))
	(unless (setq not-any-owner
		      (not (or (eq current secondary)
                               (and (null secondary)
                                    (eq current primary)))))
	  (when (eq current secondary)
	    (setf (shared-resource-current-owner r) nil)
	    (signal-semaphore (shared-resource-primary-owner-notify r))))))
    (when not-any-owner
      (signal-program-error "Process ~a does not own ~a" *current-process*
			    (shared-resource-name r)))))

;;; The current thread should be the primary owner; there should be
;;; no secondary owner.  Wakeup the specified (or first) requesting
;;; process, then block on our semaphore 
(defun %yield-shared-resource (r &optional to)
  (let* ((request nil))
    (with-lock-grabbed ((shared-resource-lock r))
      (let* ((current *current-process*)
	     (primary (shared-resource-primary-owner r)))
	(when (and (eq current primary)
		   (null (shared-resource-current-owner r)))
	  (setq request
		(let* ((header (shared-resource-requestors r)))
		  (if to 
		    (do-dll-nodes (node header)
		      (when (eq to (shared-resource-request-process node))
			(return node)))
		    (let* ((first (dll-header-first header)))
		      (unless (eq first header)
			first)))))
	  (when request
	    (remove-dll-node request)
            (setf (shared-resource-current-owner r)
                  (shared-resource-request-process request))
	    (signal-semaphore (shared-resource-request-signal request))))))
    (when request
      (wait-on-semaphore (shared-resource-primary-owner-notify r))
      (format t "~%;;;~%;;;control of ~a restored to ~a~%;;;~&"
	      (shared-resource-name r)
	      *current-process*))))


      

(defun %shared-resource-requestor-p (r proc)
  (with-lock-grabbed ((shared-resource-lock r))
    (do-dll-nodes (node (shared-resource-requestors r))
      (when (eq proc (shared-resource-request-process node))
	(return t)))))

(defparameter *resident-editor-hook* nil
  "If non-NIL, should be a function that takes an optional argument
   (like ED) and invokes a \"resident\" editor.")

(defun ed (&optional arg)
  (if *resident-editor-hook*
    (funcall *resident-editor-hook* arg)
    (error "This implementation doesn't provide a resident editor.")))

(defun running-under-emacs-p ()
  (not (null (getenv "EMACS"))))

(defloadvar *cpu-count* nil)

(defun cpu-count ()
  (or *cpu-count*
      (setq *cpu-count*
            #+darwin-target
            (rlet ((info :host_basic_info)
                   (count :mach_msg_type_number_t #$HOST_BASIC_INFO_COUNT))
              (if (eql #$KERN_SUCCESS (#_host_info (#_mach_host_self)
                                                   #$HOST_BASIC_INFO
                                                   info
                                                   count))
                (pref info :host_basic_info.max_cpus)
                1))
            #+(or linux-target solaris-target)
            (or
             (let* ((n (#_sysconf #$_SC_NPROCESSORS_ONLN)))
               (declare (fixnum n))
               (if (> n 0) n))
             #+linux-target
             (ignore-errors
               (with-open-file (p "/proc/cpuinfo")
                 (let* ((ncpu 0)
                        (match "processor")
                        (matchlen (length match)))
                   (do* ((line (read-line p nil nil) (read-line p nil nil)))
                        ((null line) ncpu)
                     (let* ((line-length (length line)))
                       (when (and
                              (> line-length matchlen)
                              (string= match line
                                       :end2 matchlen)
                              (whitespacep (schar line matchlen)))
                         (incf ncpu)))))))
             1)
            #+freebsd-target
            (rlet ((ret :uint))
              (%stack-block ((mib (* (record-length :uint) 2)))
              (setf (paref mib (:array :uint) 0)
                    #$CTL_HW
                    (paref mib (:array :uint) 1)
                    #$HW_NCPU)
              (rlet ((oldsize :uint (record-length :uint)))
                (if (eql 0 (#_sysctl mib 2 ret oldsize (%null-ptr) 0))
                  (pref ret :uint)
                  1))))
            #+windows-target
              (rlet ((bufsize #>DWORD 64))
                (loop
                  (%stack-block ((info (pref bufsize #>DWORD)))
                    (unless (eql #$FALSE (#_GetLogicalProcessorInformation
                                          info bufsize))
                      (let* ((count 0)
                             (nbytes (pref bufsize #>DWORD)))
                        (return
                          (do* ((i 0 (+ i (record-length #>SYSTEM_LOGICAL_PROCESSOR_INFORMATION))))
                               ((>= i nbytes) count)
                            (when (eql (pref info #>SYSTEM_LOGICAL_PROCESSOR_INFORMATION.Relationship) #$RelationProcessorCore)
                              (incf count))
                            (%incf-ptr info (record-length #>SYSTEM_LOGICAL_PROCESSOR_INFORMATION))))))))))))

(def-load-pointers spin-count ()
  (if (eql 1 (cpu-count))
    (%defglobal '*spin-lock-tries* 1)
    (%defglobal '*spin-lock-tries* 1024))
  (%defglobal '*spin-lock-timeouts* 0))

(defun yield ()
  #+windows-target
  (#_Sleep 0)
  #-windows-target  
  (#_sched_yield))

(defloadvar *host-page-size*
    #-windows-target (#_getpagesize)
    #+windows-target
    (rlet ((info #>SYSTEM_INFO))
      (#_GetSystemInfo info)
      (pref info #>SYSTEM_INFO.dwPageSize))
    )

;;(assert (= (logcount *host-page-size*) 1))

(defun get-universal-time ()
  "Return a single integer for the current time of
   day in universal time format."
  (rlet ((tv :timeval))
    (gettimeofday tv)
    (+ (pref tv :timeval.tv_sec) unix-to-universal-time)))

#-windows-target
(progn
(defun map-file-to-ivector (pathname element-type)
  (let* ((upgraded-type (upgraded-array-element-type element-type))
         (upgraded-ctype (specifier-type upgraded-type)))
    (unless (and (typep upgraded-ctype 'numeric-ctype)
                 (eq 'integer (numeric-ctype-class upgraded-ctype)))
      (error "Invalid element-type: ~s" element-type))
    (let* ((bits-per-element (integer-length (- (numeric-ctype-high upgraded-ctype)
                                                (numeric-ctype-low upgraded-ctype))))
           (fd (fd-open (native-translated-namestring pathname) #$O_RDONLY)))
      (if (< fd 0)
        (signal-file-error fd pathname)
        (let* ((len (fd-size fd)))
          (if (< len 0)
            (signal-file-error fd pathname)
            (let* ((nbytes (+ *host-page-size*
                              (logandc2 (+ len
                                           (1- *host-page-size*))
                                        (1- *host-page-size*))))

                   (ndata-elements
                    (ash len
                         (ecase bits-per-element
                           (1 3)
                           (8 0)
                           (16 -1)
                           (32 -2)
                           (64 -3))))
                   (nalignment-elements
                    (ash target::nbits-in-word
                         (ecase bits-per-element
                           (1 0)
                           (8 -3)
                           (16 -4)
                           (32 -5)
                           (64 -6)))))
              (if (>= (+ ndata-elements nalignment-elements)
                      array-total-size-limit)
                (progn
                  (fd-close fd)
                  (error "Can't make a vector with ~s elements in this implementation." (+ ndata-elements nalignment-elements)))
                (let* ((addr (#_mmap (%null-ptr)
                                     nbytes
                                     #$PROT_NONE
                                     (logior #$MAP_ANON #$MAP_PRIVATE)
                                     -1
                                     0)))              
                  (if (eql addr (%int-to-ptr (1- (ash 1 target::nbits-in-word)))) ; #$MAP_FAILED
                    (let* ((errno (%get-errno)))
                      (fd-close fd)
                      (error "Can't map ~d bytes: ~a" nbytes (%strerror errno)))
              ;;; Remap the first page so that we can put a vector header
              ;;; there; use the first word on the first page to remember
              ;;; the file descriptor.
                    (progn
                      (#_mmap addr
                              *host-page-size*
                              (logior #$PROT_READ #$PROT_WRITE)
                              (logior #$MAP_ANON #$MAP_PRIVATE #$MAP_FIXED)
                              -1
                              0)
                      (setf (pref addr :int) fd)
                      (let* ((header-addr (%inc-ptr addr (- *host-page-size*
                                                            (* 2 target::node-size)))))
                        (setf (pref header-addr :unsigned-long)
                              (logior (element-type-subtype upgraded-type)
                                      (ash (+ ndata-elements nalignment-elements) target::num-subtag-bits)))
                        (when (> len 0)
                          (let* ((target-addr (%inc-ptr header-addr (* 2 target::node-size))))
                            (unless (eql target-addr
                                         (#_mmap target-addr
                                                 len
                                                 #$PROT_READ
                                                 (logior #$MAP_PRIVATE #$MAP_FIXED)
                                                 fd
                                                 0))
                              (let* ((errno (%get-errno)))
                                (fd-close fd)
                                (#_munmap addr nbytes)
                                (error "Mapping failed: ~a" (%strerror errno))))))
                        (with-macptrs ((v (%inc-ptr header-addr target::fulltag-misc)))
                          (let* ((vector (rlet ((p :address v)) (%get-object p 0))))
                            ;; Tell some parts of OpenMCL - notably the
                            ;; printer - that this thing off in foreign
                            ;; memory is a real lisp object and not
                            ;; "bogus".
                            (with-lock-grabbed (*heap-ivector-lock*)
                              (push vector *heap-ivectors*))
                            (make-array ndata-elements
                                        :element-type upgraded-type
                                        :displaced-to vector
                                        :adjustable t
                                        :displaced-index-offset nalignment-elements)))))))))))))))

(defun map-file-to-octet-vector (pathname)
  (map-file-to-ivector pathname '(unsigned-byte 8)))

(defun mapped-vector-data-address-and-size (displaced-vector)
  (let* ((v (array-displacement displaced-vector))
         (element-type (array-element-type displaced-vector)))
    (if (or (eq v displaced-vector)
            (not (with-lock-grabbed (*heap-ivector-lock*)
                   (member v *heap-ivectors*))))
      (error "~s doesn't seem to have been allocated via ~s and not yet unmapped" displaced-vector 'map-file-to-ivector))
    (let* ((pv (rlet ((x :address)) (%set-object x 0 v) (pref x :address)))
           (ctype (specifier-type element-type))
           (arch (backend-target-arch *target-backend*)))
      (values (%inc-ptr pv (- (* 2 target::node-size) target::fulltag-misc))
              (- (funcall (arch::target-array-data-size-function arch)
                          (ctype-subtype ctype)
                          (length v))
                 target::node-size)))))

  
;;; Argument should be something returned by MAP-FILE-TO-IVECTOR;
;;; this should be called at most once for any such object.
(defun unmap-ivector (displaced-vector)
  (multiple-value-bind (data-address size-in-octets)
      (mapped-vector-data-address-and-size displaced-vector)
  (let* ((v (array-displacement displaced-vector))
         (base-address (%inc-ptr data-address (- *host-page-size*)))
         (fd (pref base-address :int)))
      (let* ((element-type (array-element-type displaced-vector)))
        (adjust-array displaced-vector 0
                      :element-type element-type
                      :displaced-to (make-array 0 :element-type element-type)
                      :displaced-index-offset 0))
      (with-lock-grabbed (*heap-ivector-lock*)
        (setq *heap-ivectors* (delete v *heap-ivectors*)))
      (#_munmap base-address (+ size-in-octets *host-page-size*))      
      (fd-close fd)
      t)))

(defun unmap-octet-vector (v)
  (unmap-ivector v))

(defun lock-mapped-vector (v)
  (multiple-value-bind (address nbytes)
      (mapped-vector-data-address-and-size v)
    (eql 0 (#_mlock address nbytes))))

(defun unlock-mapped-vector (v)
  (multiple-value-bind (address nbytes)
      (mapped-vector-data-address-and-size v)
    (eql 0 (#_munlock address nbytes))))

(defun bitmap-for-mapped-range (address nbytes)
  (let* ((npages (ceiling nbytes *host-page-size*)))
    (%stack-block ((vec npages))
      (when (eql 0 (#_mincore address nbytes vec))
        (let* ((bits (make-array npages :element-type 'bit)))
          (dotimes (i npages bits)
            (setf (sbit bits i)
                  (logand 1 (%get-unsigned-byte vec i)))))))))

(defun percentage-of-resident-pages (address nbytes)
  (let* ((npages (ceiling nbytes *host-page-size*)))
    (%stack-block ((vec npages))
      (when (eql 0 (#_mincore address nbytes vec))
        (let* ((nresident 0))
          (dotimes (i npages (* 100.0 (/ nresident npages)))
            (when (logbitp 0 (%get-unsigned-byte vec i))
              (incf nresident))))))))

(defun mapped-vector-resident-pages (v)
  (multiple-value-bind (address nbytes)
      (mapped-vector-data-address-and-size v)
    (bitmap-for-mapped-range address nbytes)))

(defun mapped-vector-resident-pages-percentage (v)
  (multiple-value-bind (address nbytes)
      (mapped-vector-data-address-and-size v)
    (percentage-of-resident-pages address nbytes)))
)

#+windows-target
(defun cygpath (winpath)
  "Try to use the Cygwin \"cygpath\" program to map a Windows-style
   pathname to a POSIX-stype Cygwin pathname."
  (let* ((posix-path winpath))
    (with-output-to-string (s)
      (multiple-value-bind (status exit-code)
          (external-process-status
           (run-program "cygpath" (list "-u" winpath) :output s))
        (when (and (eq status :exited)
                   (eql exit-code 0))
          (with-input-from-string (output (get-output-stream-string s))
            (setq posix-path (read-line output nil nil))))))
    posix-path))

#-windows-target (defun cygpath (path) path)
      



#+x86-target
(progn
(defloadvar *last-rdtsc-time* 0)

(defstatic *rdtsc-estimated-increment* 1 "Should be positive ...")

(defun rdtsc-monotonic ()
  "Return monotonically increasing values, partly compensating for
   OSes that don't keep the TSCs of all processorsin synch."
  (loop
    (let* ((old *last-rdtsc-time*)
           (new (rdtsc)))
      (when (< new old)
        ;; We're running on a CPU whose TSC is behind the one
        ;; on the last CPU we were scheduled on.
        (setq new (+ old *rdtsc-estimated-increment*)))
      (when (%store-node-conditional target::symbol.vcell *last-rdtsc-time* old new)
        (return new)))))

(defun estimate-rdtsc-skew (&optional (niter 1000000))
  (do* ((i 0 (1+ i))
        (last (rdtsc) next)
        (next (rdtsc) (rdtsc))
        (skew 1))
       ((>= i niter) (setq *rdtsc-estimated-increment* skew))
    (declare (fixnum last next skew))
    (when (> last next)
      (let* ((s (- last next)))
        (declare (fixnum s))
        (when (> s skew) (setq skew s))))))
)


