;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2009 Clozure Associates
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

(in-package "CCL")

(defconstant unix-to-universal-time 2208988800)

#+windows-target
(progn


            



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
  #-(or darwin-target windows-target)
  (let* ((encoding-name (pathname-encoding-name)))
    (if encoding-name
      (get-encoded-cstring encoding-name pointer)
      (%get-cstring pointer))))

(defun nanoseconds (seconds)
  (when (and (typep seconds 'fixnum)
             (>= (the fixnum seconds) 0))
    (return-from nanoseconds (values seconds 0)))
  (check-type seconds (real 0 #.(1- (ash 1 (1- target::nbits-in-word)))))
  (multiple-value-bind (q r)
      (floor seconds)
    (if (zerop r)
      (setq r 0)
      (setq r (floor (* r 1000000000))))
    (values q r)))

(defun milliseconds (seconds)
  (when (and (typep seconds 'fixnum)
             (>= (the fixnum seconds) 0))
    (return-from milliseconds (values seconds 0)))
  (check-type seconds (real 0 #.(1- (ash 1 (1- target::nbits-in-word)))))
  (multiple-value-bind (q r)
      (floor seconds)
    (if (zerop r)
      (setq r 0)
      (setq r (floor (* r 1000))))
    (values q r)))

(defun microseconds (seconds)
  (when (and (typep seconds 'fixnum)
             (>= (the fixnum seconds) 0))
    (return-from microseconds (values seconds 0)))
  (check-type seconds (real 0 #.(1- (ash 1 (1- target::nbits-in-word)))))
  (multiple-value-bind (q r)
      (floor seconds)
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
        (let* ((now (get-internal-real-time))
               (stop (+ now (floor (* duration internal-time-units-per-second)))))
          (multiple-value-bind (secs millis) (milliseconds duration)
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
                    (multiple-value-bind (remaining-seconds remaining-itus)
                        (floor diff internal-time-units-per-second)
                      (setq secs remaining-seconds
                            millis (floor remaining-itus (/ internal-time-units-per-second 1000)))))))))))))

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

(defun %timed-wait-for-signal (signo seconds millis)
  (let* ((status (ff-call
                  (%kernel-import target::kernel-import-wait-for-signal)
                  :int signo
                  :unsigned seconds
                  :unsigned millis
                  :int)))
    (values (eql status 0) status)))

(defun wait-for-signal (s duration)
  (if duration
    (check-type duration (real 0 #x7fffffff))
    (setq duration #x7fffffff))
  (or (multiple-value-bind (result err)
          (%timed-wait-for-signal s 0 0)
        (or result
            (if (or (eql err #$EINTR) ; probably not possible
                    (eql err #-windows-target #$ETIMEDOUT #+windows-target #$WAIT_TIMEOUT))
              nil
              (error "Error waiting for signal ~d: ~a." s (%strerror err)))))
      (with-process-whostate ("signal wait")
        (let* ((now (get-internal-real-time))
               (stop (+ now (floor (* duration internal-time-units-per-second)))))
          (multiple-value-bind (secs millis) (milliseconds duration)
            (loop
              (multiple-value-bind (success err)
                  (progn
                    (%timed-wait-for-signal s secs millis))
                (when success
                  (return t))
                (if (or (eql err #-windows-target #$ETIMEDOUT #+windows-target #$WAIT_TIMEOUT)
                        (>= (setq now (get-internal-real-time)) stop))
                  (return nil)
                  (unless (eql err #$EINTR)
                    (error "Error waiting for signal ~d: ~a." s (%strerror err))))
                (unless (zerop duration)
                  (let* ((diff (- stop now)))
                    (multiple-value-bind (remaining-seconds remaining-itus)
                        (floor diff internal-time-units-per-second)
                      (setq secs remaining-seconds
                            millis (floor remaining-itus (/ internal-time-units-per-second 1000)))))))))))))
  
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

(defparameter *trust-paths-from-environment* t
  "When true (as it is by default), environment variables can be used
to initialize CCL's notion of some filesystem paths.  This may expose
CCL or your application to greater security risks in some cases; if you're
concerned about that, you may want to save an image with this variable
set to NIL.")


(defun temp-pathname ()
  "Return a suitable pathname for a temporary file.  A different name is returned
each time this is called in a session.  No file by that name existed when last
checked, though no guarantee is given that one hasn't been created since."
  (native-to-pathname
     #-windows-target
     #-android-target (get-foreign-namestring (#_tmpnam (%null-ptr)))
     #+android-target
     ;; Android dutifully implements #_tmpnam and returns a namestring
     ;; in /tmp, but of course they don't usually provide /tmp .
     (let* ((s (get-foreign-namestring (#_tmpnam (%null-ptr)))))
       (if (probe-file (make-pathname :directory (pathname-directory s) :defaults nil))
         s
         (let* ((dirname (or (and *trust-paths-from-environment*
                                  (let* ((p (getenv "TMPDIR")))
                                    (and p
                                         (eq (nth-value 1 (%probe-file-x p))
                                             :directory)
                                         p)))
                             "/data/local/tmp"))
                (filename (make-string 8)))
           (loop
             (flet ((random-char ()
                      (let* ((n (random 62)))
                        (cond ((< n 10) (code-char (+ (char-code #\0) n)))
                              ((< n 36) (code-char (+ (char-code #\A) (- n 10))))
                              (t (code-char (+ (char-code #\a) (- n 36))))))))
               (dotimes (i (length filename))
                 (setf (schar filename i) (random-char)))
               (let* ((path (make-pathname :name filename :directory dirname :defaults nil)))
                 (unless (probe-file path)
                   (return (namestring path)))))))))
     #+windows-target (rlet ((buffer (:array :wchar_t #.#$MAX_PATH)))
                        (#_GetTempPathW #$MAX_PATH buffer)
                        (with-filename-cstrs ((c-prefix "ccl")) 
                            (#_GetTempFileNameW buffer c-prefix 0 buffer)
                              (#_DeleteFileW buffer)
                              (%get-native-utf-16-cstring buffer)))))

(defun current-directory-name ()
  "Look up the current working directory of the Clozure CL process; unless
it has been changed, this is the directory Clozure CL was started in."
  (flet ((try-getting-dirname (bufsize)
	   (%stack-block ((buf bufsize))
	     (let* ((len (%os-getcwd buf bufsize)))
	       (cond ((< len 0) (%errno-disp len))
		     ((< len bufsize)
		      (setf (%get-unsigned-byte buf len) 0)
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
    (when (and (>= last 0)
	       (eql (char name last) #\/))
      (setq name (subseq name 0 last)))
    (with-filename-cstrs ((name name))
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
  (with-cstrs ((pair (concatenate 'string key "=" value)))
    (#__putenv pair))
  )

(defun unsetenv (key)
  #-windows-target
  (with-cstrs ((ckey key))
    (#_unsetenv ckey))
  #+windows-target
  (with-cstrs ((ckey (concatenate 'string key "=")))
    (#__putenv ckey)))

#-windows-target                        ; Windows "impersonation" crap ?
(defun setuid (uid)
  "Attempt to change the current user ID (both real and effective);
fails unless the Clozure CL process has super-user privileges or the ID
given is that of the current user."
  (int-errno-call (#_setuid uid)))

#-windows-target
(defun setgid (uid)
  "Attempt to change the current group ID (both real and effective);
fails unless the Clozure CL process has super-user privileges or the ID
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
       #+android-target (pref stat :stat.st_mtime)
       #+(or (and linux-target (not android-target)) solaris-target)
       (pref stat :stat.st_mtim.tv_sec)
       #-(or linux-target solaris-target)
       (pref stat :stat.st_mtimespec.tv_sec)
       (pref stat :stat.st_ino)
       (pref stat :stat.st_uid)
       (pref stat :stat.st_blksize)
       #+(or linux-target solaris-target)
       (round (pref stat #-android-target :stat.st_mtim.tv_nsec
                         #+android-target :stat.st_mtime_nsec) 1000)
       #-(or linux-target solaris-target)
       (round (pref stat :stat.st_mtimespec.tv_nsec) 1000)
       (pref stat :stat.st_gid)
       (pref stat :stat.st_dev))
      (values nil nil nil nil nil nil nil nil nil nil)))

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
       (pref stat :_stat64.st_gid)
       (pref stat :_stat64.st_dev))
      (values nil nil nil nil nil nil nil nil nil nil)))

#+win32-target
(defun %stat-values (result stat)
  (if (eql 0 (the fixnum result)) 
      (values
       t
       (pref stat :__stat64.st_mode)
       (pref stat :__stat64.st_size)
       (pref stat :__stat64.st_mtime)
       (pref stat :__stat64.st_ino)
       (pref stat :__stat64.st_uid)
       #$BUFSIZ
       (pref stat :__stat64.st_mtime)     ; ???
       (pref stat :__stat64.st_gid)
       (pref stat :__stat64.st_dev))
      (values nil nil nil nil nil nil nil nil nil nil)))

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
     #+(and linux-target (not android-target))
     (#_ __xstat #$_STAT_VER_LINUX cname stat)
     #-(and linux-target (not android-target))
     (int-errno-ffcall (%kernel-import target::kernel-import-lisp-stat)
                       :address cname
                       :address stat
                       :int)
     stat)))

(defun %%fstat (fd stat)
  (%stat-values
   #+(and linux-target (not android-target))
   (#_ __fxstat #$_STAT_VER_LINUX fd stat)
   #-(and linux-target (not android-target))
   (int-errno-ffcall (%kernel-import target::kernel-import-lisp-fstat)
                     :int fd
                     :address stat
                     :int)
   stat))

#-windows-target
(defun %%lstat (name stat)
  (with-filename-cstrs ((cname name))
    (%stat-values
     #+(and linux-target (not android-target))
     (#_ __lxstat #$_STAT_VER_LINUX cname stat)
     #-(and linux-target (not android-target))
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
  (rlet ((stat  #+win64-target #>_stat64 #+win32-target #>__stat64))
    (%%stat name stat)))

(defun %fstat (fd)
  (rlet ((stat #+win64-target #>_stat64 #+win32-target #>__stat64 #-windows-target :stat))
    (%%fstat fd stat)))


(defun %file-kind (mode &optional fd)
  (declare (ignorable fd))
  (when mode
    (let* ((kind (logand mode #$S_IFMT)))
      (cond ((eql kind #$S_IFDIR) :directory)
	    ((eql kind #$S_IFREG) :file)
            #-windows-target
	    ((eql kind #$S_IFLNK) :link)
	    ((eql kind #$S_IFIFO) 
	     #-windows-target :pipe
             ;; Windows doesn't seem to be able to distinguish between
             ;; sockets and pipes.  Since this function is currently
             ;; (mostly) used for printing streams and since we've
             ;; already done something fairly expensive (stat, fstat)
             ;; to get here.  try to distinguish between pipes and
             ;; sockets by calling #_getsockopt.  If that succeeds,
             ;; we've got a socket; otherwise, we've probably got a pipe.
	     #+windows-target (rlet ((ptype :int)
				     (plen :int 4))
				(if (and fd (eql 0 (#_getsockopt fd #$SOL_SOCKET #$SO_TYPE  ptype plen)))
				    :socket
				    :pipe)))
            #-windows-target
	    ((eql kind #$S_IFSOCK) :socket)
	    ((eql kind #$S_IFCHR) :character-special)
	    (t :special)))))

(defun %unix-file-kind (native-namestring &optional check-for-link)
  (%file-kind (nth-value 1 (%stat native-namestring check-for-link))))

(defun %unix-fd-kind (fd)
  (if (isatty fd)
    :tty
    (%file-kind (nth-value 1 (%fstat fd)) fd)))

#-windows-target
(defun %uts-string (result idx buf)
  (if (>= result 0)
    (%get-cstring (%inc-ptr buf (* #+(and linux-target (not android-target)) #$_UTSNAME_LENGTH
                                   #+android-target (1+ #$__NEW_UTS_LEN)
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

#+windows-target
(defun copy-file-attributes (source-path dest-path)
  "could at least copy the file times"
  (declare (ignore source-path dest-path)))


#+(and linux-target (not android-target))
(defun %uname (idx)
  (%stack-block ((buf (* #$_UTSNAME_LENGTH 6)))  
    (%uts-string (#_uname buf) idx buf)))

#+android-target
(defun %uname (idx)
  (%stack-block ((buf (* (1+ #$__NEW_UTS_LEN) 6)))  
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
  (declare (ignore direction))
  (rlet ((handle #>HANDLE))
    (if (eql 0 (#_DuplicateHandle (#_GetCurrentProcess)
                                  (%int-to-ptr fd)
                                  (#_GetCurrentProcess) 
                                  handle
                                  0
                                  (if inheritable #$TRUE #$FALSE)
                                  #$DUPLICATE_SAME_ACCESS))
      (%windows-error-disp (#_GetLastError))
      (pref handle #>DWORD))))


(defun fd-fsync (fd)
  #+windows-target (#_FlushFileBuffers (%int-to-ptr fd))
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
        #+windows-target namestring
        #-windows-target
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
        (let* ((nchars (#_GetFullPathNameW path (ash bufsize -1) buf (%null-ptr))))
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

#+windows-target
(defun %file-author (namestring)
  (declare (ignore namestring))
  nil)

#-windows-target
(defun %utimes (namestring)
  (with-filename-cstrs ((cnamestring namestring))
    (let* ((err (#_utimes cnamestring (%null-ptr))))
      (declare (fixnum err))
      (or (eql err 0)
          (%errno-disp err namestring)))))

#+windows-target
(defun %utimes (namestring)
  (with-filename-cstrs ((cnamestring namestring))
    (let* ((handle (#_CreateFileW
                    cnamestring
                    #$FILE_WRITE_ATTRIBUTES
                    (logior #$FILE_SHARE_READ #$FILE_SHARE_WRITE)
                    (%null-ptr)
                    #$OPEN_EXISTING
                    #$FILE_FLAG_BACKUP_SEMANTICS
                    (%null-ptr))))
      (if (eql handle *windows-invalid-handle*)
        (%windows-error-disp (#_GetLastError))
        (rlet ((st #>SYSTEMTIME)
               (ft #>FILETIME))
          (#_GetSystemTime st)
          (#_SystemTimeToFileTime st ft)
          (let* ((result (#_SetFileTime handle (%null-ptr) (%null-ptr) ft))
                 (err (when (eql 0 result) (#_GetLastError))))
            (#_CloseHandle handle)
            (if err
              (%windows-error-disp err)
              t)))))))


             

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

#-win64-target
(progn
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
)

#+win64-target
(progn
  (eval-when (:compile-toplevel :execute)
    (def-foreign-type nil
        (:struct :win64-dir
                 (:data #>WIN32_FIND_DATAW)
                 (:handle :address)
                 (:state :int))))

(defun %open-dir (namestring)
  ;;; Namestring should end in /*.  Honest.
  (let* ((len (length namestring))
         (lastchar (if (> len 0) (schar namestring (1- len))))
         (penultimatechar (if (> len 1) (schar namestring (- len 2)))))
    (unless (and (eql lastchar #\*)
                 (or (eql penultimatechar #\\)
                     (eql penultimatechar #\/)))
      (if (or (eql lastchar #\\)
              (eql lastchar #\/))
        (setq namestring (concatenate 'string namestring "*"))
        (setq namestring (concatenate 'string namestring "/*")))))
  (let* ((dir (malloc (record-length :win64-dir))))
    (with-filename-cstrs ((name namestring))
      (let* ((handle (#_FindFirstFileW name dir)))
        (cond ((eql handle #$INVALID_HANDLE_VALUE)
               (free dir)
               nil)
              (t
               (setf (pref dir :win64-dir.state) 0
                     (pref dir :win64-dir.handle) handle)
               dir))))))
          
(defun %read-dir (dir)
  (when (eql 0 (pref dir :win64-dir.state))
    (prog1
        (get-foreign-namestring (pref dir  #>WIN32_FIND_DATAW.cFileName))
      (if (eql 0 (#_FindNextFileW (pref dir :win64-dir.handle) dir))
        (setf (pref dir :win64-dir.state) -1)))))

(defun close-dir (dir) 
  (#_FindClose (pref dir :win64-dir.handle))
  (free dir)
  nil)
)
                       
  

                         


#-windows-target
(defun tcgetpgrp (fd)
  (#_tcgetpgrp fd))

(defun getpid ()
  "Return the ID of the Clozure CL OS process."
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
  #+(or windows-target android-target)
  (declare (ignore userid))
  #+windows-target
  (dolist (k '(#||"HOME"||# "USERPROFILE")) 
    (with-native-utf-16-cstrs ((key k))
      (let* ((p (#__wgetenv key)))
        (unless (%null-ptr-p p)
          (return (get-foreign-namestring p))))))
  #-windows-target
  (or (and *trust-paths-from-environment*
           (let* ((p (getenv "HOME")))
             (and p
                  (eq (nth-value 1 (%probe-file-x p)) :directory)
                  p)))
      #+android-target "/data/local" ; for now
      #-android-target
      (rlet ((pwd :passwd)
             (result :address pwd))
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
                (let* ((rp (%get-ptr result))
                       (dir (and (not (%null-ptr-p rp))
                                 (get-foreign-namestring (pref rp :passwd.pw_dir)))))
                  (return (if (and dir (eq (%unix-file-kind dir) :directory))
                            dir)))
                (unless (eql err #$ERANGE)
                  (return nil)))))))))

(defun %delete-file (name)
  (with-filename-cstrs ((n name))
    (int-errno-call (#+windows-target #__wunlink #-windows-target #_unlink n))))

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
      (format nil "OS Error ~d" errno)
      (%get-cstring p))))

#+windows-target
(progn
(defun get-last-windows-error ()
  (#_GetLastError))

(defun %windows-error-string (error-number)
  (rlet ((pbuffer :address (%null-ptr)))
    (if (eql 0
             (#_FormatMessageW (logior #$FORMAT_MESSAGE_ALLOCATE_BUFFER
                                       #$FORMAT_MESSAGE_FROM_SYSTEM
                                       #$FORMAT_MESSAGE_IGNORE_INSERTS
                                       #$FORMAT_MESSAGE_MAX_WIDTH_MASK)
                               (%null-ptr)
                               (abs error-number)
                               0                 ; default langid, more-or-less
                               pbuffer
                               0
                               (%null-ptr)))
      (format nil "Windows error ~d" (abs error-number))
      (let* ((p (%get-ptr pbuffer))
             (q (%get-native-utf-16-cstring p)))
        (#_LocalFree p)
        q))))
)
        
(defun %probe-shared-library (shlib)
  #-(or windows-target android-target freebsd-target)
  (with-cstrs ((name (shlib.pathname shlib)))
    (not (%null-ptr-p (#_dlopen name (logior #$RTLD_NOW #$RTLD_NOLOAD)))))
  ;; FreeBSD may support #$RTLD_NOLOAD in 8.0, and that support may
  ;; have been backported to 7.2.  Until then ...
  #+(or freebsd-target android-target)
  (rlet ((info #>Dl_info))
    (not (eql 0 (#_dladdr (shlib.base shlib) info))))
  #+windows-target
  (with-filename-cstrs ((name (shlib.pathname shlib)))
    (not (%null-ptr-p (#_GetModuleHandleW name)))))


;;; Kind of has something to do with files, and doesn't work in level-0.
(defun close-shared-library (lib &key (completely t))
  "If completely is T, set the reference count of library to 0. Otherwise,
decrements it by 1. In either case, if the reference count becomes 0,
close-shared-library frees all memory resources consumed library and causes
any EXTERNAL-ENTRY-POINTs known to be defined by it to become unresolved."
  (let* ((lib (if (typep lib 'string)
		(or (shared-library-with-name lib)
		    (error "Shared library ~s not found." lib))
		(require-type lib 'shlib)))
	 (handle (shlib.handle lib))
         (opencount (shlib.opencount lib)))
      (when handle
        (dotimes (i (if completely opencount 1))
          (unless #-windows-target (eql 0 (#_dlclose handle))
                  #+windows-target (not (eql 0(#_FreeLibrary handle)))
                  (return))
          (decf (shlib.opencount lib)))
        (when (and (eql 0 (shlib.opencount lib))
                   (not (%probe-shared-library lib)))
          (setf (shlib.pathname lib) nil
                (shlib.base lib) nil
                (shlib.handle lib) nil
                (shlib.map lib) nil)
          (unload-foreign-variables nil)
          (unload-library-entrypoints nil)))))





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

(defmacro with-string-vector ((var strings) &body body)
  `(call-with-string-vector #'(lambda (,var) ,@body) ,strings))

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
    (#_exit #-android-target #$EX_OSERR #+android-target 71))

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
    watched-fds
    watched-streams
    external-format
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
                                    &rest keys
                                    &key direction (element-type 'character)
                                    (sharing :private)
                                    external-format
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
                                    :sharing sharing
                                    :basic t
                                    :encoding (external-format-character-encoding external-format)
                                    :line-termination (external-format-line-termination external-format)
                                    :auto-close t)
                    (cons read-pipe close-in-parent)
                    (cons write-pipe close-on-error)))
           (:output
            (values write-pipe
                    (make-fd-stream read-pipe
                                    :direction :input
                                    :element-type element-type
                                    :interactive t
                                    :basic t
                                    :sharing sharing
                                    :encoding (external-format-character-encoding external-format)
                                    :line-termination (external-format-line-termination external-format)
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
      #||
      ;; What's an FD-STREAM ?
      (fd-stream
       (let ((fd (fd-dup (ioblock-device (stream-ioblock object t)))))
         (values fd
                 nil
                 (cons fd close-in-parent)
                 (cons fd close-on-error))))
      ||#
      (stream
       (ecase direction
         (:input
          (with-cstrs ((template #-android-target "/tmp/lisp-tempXXXXXX"
                                 #+android-target "/data/local/tmp/lisp-tempXXXXXX"))
            (let* ((fd (#_mkstemp template)))
              (if (< fd 0)
                (%errno-disp fd))
              (#_unlink template)
              (let* ((out (make-fd-stream (fd-dup fd)
                                          :direction :output
                                          :encoding (external-format-character-encoding external-format)
                                          :line-termination (external-format-line-termination external-format))))
                (loop
                  (multiple-value-bind (line no-newline)
                      (read-line object nil nil)
                    (unless line
                      (return))
                    (if no-newline
                      (write-string line out)
                      (write-line line out))))
                (close out))
              (fd-lseek fd 0 #$SEEK_SET)
              (values fd nil (cons fd close-in-parent) (cons fd close-on-error)))))
         (:output
          (multiple-value-bind (read-pipe write-pipe) (pipe)
            (push read-pipe (external-process-watched-fds proc))
            (push object (external-process-watched-streams proc))
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
    (let* ((in-fds (external-process-watched-fds p))
           (out-streams (external-process-watched-streams p))
           (token (external-process-token p))
           (terminated)
           (changed)
           (maxfd 0)
           (external-format (external-process-external-format p))
           (encoding (external-format-character-encoding external-format))
           (line-termination (external-format-line-termination external-format))
           (pairs (pairlis
                   (mapcar (lambda (fd)
                             (cons fd
                                   (make-fd-stream fd
                                                   :direction :input
                                                   :sharing :private
                                                   :encoding encoding
                                                   :interactive t
                                                   :line-termination line-termination)))
                                     in-fds) out-streams)))
      (%stack-block ((in-fd-set *fd-set-size*))
        (rlet ((tv #>timeval))
          (loop
            (when changed
              (setq pairs (delete nil pairs :key #'car)
                    changed nil))
            (when (and terminated (null pairs))
              (signal-semaphore (external-process-completed p))
              (return))
            (when pairs
              (fd-zero in-fd-set)
              (setq maxfd 0)
              (dolist (p pairs)
                (let* ((fd (caar p)))
                  (when (> fd maxfd)
                    (setq maxfd fd))
                  (fd-set fd in-fd-set)))
              (setf (pref tv #>timeval.tv_sec) 1
                    (pref tv #>timeval.tv_usec) 0)
              (when (> (#_select (1+ maxfd) in-fd-set (%null-ptr) (%null-ptr) tv)
                       0)
                (dolist (p pairs)
                  (let* ((in-fd (caar p))
                         (in-stream (cdar p))
                         (out-stream (cdr p)))
                    (when (fd-is-set in-fd in-fd-set)
                      (let* ((buf (make-string 1024))
                             (n (ignore-errors (read-sequence buf in-stream))))
                        (declare (dynamic-extent buf))
                        (if (or (null n) (eql n 0))
                          (without-interrupts
                           (decf (car token))
                           (close in-stream)
                           (setf (car p) nil changed t))
                          (write-sequence buf out-stream :end n))))))))
            (let* ((statusflags (check-pid (external-process-pid p)
                                           (logior
                                            (if in-fds #$WNOHANG 0)
                                            #$WUNTRACED)))
                   (oldstatus (external-process-%status p)))
              (cond ((null statusflags)
                     (remove-external-process p)
                     (setq terminated t))
                    ((eq statusflags t)) ; Running.
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
                                   (logtest #-(or solaris-target android-target)
                                            #$WCOREFLAG
                                            #+solaris-target #$WCOREFLG
                                            #+android-target #x80
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
                         (setq terminated t)))))))))))
      
  (defun run-external-process (proc in-fd out-fd error-fd argv &optional env)
    (let* ((signaled nil))
      (unwind-protect
           (let* ((child-pid (#-solaris-target #_fork #+solaris-target #_forkall)))
             (declare (fixnum child-pid))
             (cond ((zerop child-pid)
                    ;; Running in the child; do an exec
                    (setq signaled t)
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
                    (setq signaled t)
                    (monitor-external-process proc))
                   (t
                    ;; Fork failed
                    (setf (external-process-%status proc) :error
                          (external-process-%exit-code proc) (%get-errno))
                    (signal-semaphore (external-process-signal proc))
                    (setq signaled t))))
        (unless signaled
          (setf (external-process-%status proc) :error
                (external-process-%exit-code proc) -1)
          (signal-semaphore (external-process-signal proc))))))

  (defparameter *silently-ignore-catastrophic-failure-in-run-program* nil
    "If NIL, signal an error if run-program is unable to start the program.
If non-NIL, treat failure to start the same as failure from the program
itself, by setting the status and exit-code fields.")

  (defun run-program (program args &key
                              (wait t) pty
                              input if-input-does-not-exist
                              output (if-output-exists :error)
                              (error :output) (if-error-exists :error)
                              status-hook (element-type 'character)
                              env
                              (sharing :private)
                              (external-format `(:character-encoding ,*terminal-character-encoding-name*))
                              (silently-ignore-catastrophic-failures
                               *silently-ignore-catastrophic-failure-in-run-program*))
    "Invoke an external program as an OS subprocess of lisp."
    (declare (ignore pty))
    (unless (every #'(lambda (a) (typep a 'simple-string)) args)
      (error "Program args must all be simple strings : ~s" args))
    (dolist (pair env)
      (destructuring-bind (var . val) pair
        (check-type var (or string symbol character))
        (check-type val string)))
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
             :status-hook status-hook
             :external-format (setq external-format (normalize-external-format t external-format)))))
      (unwind-protect
           (progn
             (multiple-value-setq (in-fd in-stream close-in-parent close-on-error)
               (get-descriptor-for input proc  nil nil :direction :input
                                   :if-does-not-exist if-input-does-not-exist
                                   :element-type element-type
                                   :sharing sharing
                                   :external-format external-format))
             (multiple-value-setq (out-fd out-stream close-in-parent close-on-error)
               (get-descriptor-for output proc close-in-parent close-on-error
                                   :direction :output
                                   :if-exists if-output-exists
                                   :element-type element-type
                                   :sharing sharing
                                   :external-format external-format))
             (multiple-value-setq (error-fd error-stream close-in-parent close-on-error)
               (if (eq error :output)
                 (values out-fd out-stream close-in-parent close-on-error)
                 (get-descriptor-for error proc close-in-parent close-on-error
                                     :direction :output
                                     :if-exists if-error-exists
                                     :sharing sharing
                                     :element-type element-type
                                     :external-format external-format)))
             (setf (external-process-input proc) in-stream
                   (external-process-output proc) out-stream
                   (external-process-error proc) error-stream)
             (call-with-string-vector
              #'(lambda (argv)
                  (process-run-function
                   (list :name
                         (format nil "Monitor thread for external process ~a" args)
                         :stack-size (ash 128 10)
                         :vstack-size (ash 128 10)
                         :tstack-size (ash 128 10))
                   #'run-external-process proc in-fd out-fd error-fd argv env)
                  (wait-on-semaphore (external-process-signal proc)))
              args))
        (dolist (fd close-in-parent) (fd-close fd))
        (unless (external-process-pid proc)
          (dolist (fd close-on-error) (fd-close fd)))
        (when (and wait (external-process-pid proc))
          (with-interrupts-enabled
              (wait-on-semaphore (external-process-completed proc)))))
      (unless (external-process-pid proc)
        ;; something is wrong
        (if (eq (external-process-%status proc) :error)
          ;; Fork failed
          (unless silently-ignore-catastrophic-failures
            (cerror "Pretend the program ran and failed" 'external-process-creation-failure :proc proc))
          ;; Currently can't happen.
          (error "Bug: fork failed but status field not set?")))
      proc))



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
  
  (defun signal-external-process (proc signal &key (error-if-exited t))
    "Send the specified signal to the specified external process.  (Typically,
it would only be useful to call this function if the EXTERNAL-PROCESS was
created with :WAIT NIL.) Return T if successful; NIL if the process wasn't
created successfully, and signal an error otherwise."
    (require-type proc 'external-process)
    (let* ((pid (external-process-pid proc)))
      (when pid
        (let ((error (int-errno-call (#_kill pid signal))))
          (or (eql error 0)
              (unless (and (eql error (- #$ESRCH))
                           (not error-if-exited))
                (%errno-disp error)))))))

  )                                     ; #-windows-target (progn

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
                                    &rest keys
                                    &key
                                    direction (element-type 'character)
                                    (sharing :private)
                                    external-format
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
                                    :sharing sharing
                                    :encoding (external-format-character-encoding external-format)
                                    :line-termination (external-format-line-termination external-format)
                                    :auto-close t)
                    (cons read-pipe close-in-parent)
                    (cons write-pipe close-on-error)))
           (:output
            (values write-pipe
                    (make-fd-stream (fd-uninheritable read-pipe :direction :input)
                                    :direction :input
                                    :element-type element-type
                                    :interactive t
                                    :basic t
                                    :sharing sharing
                                    :encoding (external-format-character-encoding external-format)
                                    :line-termination (external-format-line-termination external-format)
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
      (stream
       (ecase direction
         (:input
          (let* ((tempname (temp-file-name "lisp-temp"))
                 (fd (fd-open tempname #$O_RDWR)))
            (if (< fd 0)
              (%errno-disp fd))
            (let* ((out (make-fd-stream (fd-dup fd)
                                        :direction :output
                                        :encoding (external-format-character-encoding external-format)
                                        :line-termination (external-format-line-termination external-format))))            
              (loop
                (multiple-value-bind (line no-newline)
                    (read-line object nil nil)
                  (unless line
                    (return))
                  (if no-newline
                    (write-string line out)
                    (write-line line out))
                  ))
              (close out))
            (fd-lseek fd 0 #$SEEK_SET)
            (values fd nil (cons fd close-in-parent) (cons fd close-on-error))))
         (:output
          (multiple-value-bind (read-pipe write-pipe) (pipe)
            (push read-pipe (external-process-watched-fds proc))
            (push object (external-process-watched-streams proc))
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
    watched-fds
    watched-streams
    external-format
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

  (defun run-program (program args &key
                              (wait t) pty
                              input if-input-does-not-exist
                              output (if-output-exists :error)
                              (error :output) (if-error-exists :error)
                              status-hook (element-type 'character)
                              (sharing :private)
                              (external-format `(:character-encoding ,*terminal-character-encoding-name* :line-termination :crlf))
                              env)
    "Invoke an external program as an OS subprocess of lisp."
    (declare (ignore pty))
    (push program args)
    (unless (do* ((args args (cdr args)))
                 ((atom args)
                  (or (typep args 'simple-string)
                      (null args)))
              (unless (typep (car args) 'simple-string)
                (return)))
      (error "Program args must all be simple strings : ~s" args))
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
             :external-format (setq external-format (normalize-external-format t external-format))
             :status-hook status-hook)))
      (unwind-protect
           (progn
             (multiple-value-setq (in-fd in-stream close-in-parent close-on-error)
               (get-descriptor-for input proc  nil nil :direction :input
                                   :if-does-not-exist if-input-does-not-exist
                                   :sharing sharing
                                   :element-type element-type
                                   :external-format external-format))
             (multiple-value-setq (out-fd out-stream close-in-parent close-on-error)
               (get-descriptor-for output proc close-in-parent close-on-error
                                   :direction :output
                                   :if-exists if-output-exists
                                   :sharing sharing
                                   :element-type element-type
                                   :external-format external-format))
             (multiple-value-setq (error-fd error-stream close-in-parent close-on-error)
               (if (eq error :output)
                 (values out-fd out-stream close-in-parent close-on-error)
                 (get-descriptor-for error proc close-in-parent close-on-error
                                     :direction :output
                                     :if-exists if-error-exists
                                     :sharing sharing
                                     :element-type element-type
                                     :external-format external-format)))
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
            (dolist (fd close-on-error) (fd-close fd)))))
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
           (child-pid (exec-with-io-redirection in-fd out-fd error-fd args proc env)))
      (when child-pid
        (setf (external-process-pid proc) child-pid)
        (add-external-process proc)
        (signal-semaphore (external-process-signal proc))
        (monitor-external-process proc))))

  (defun make-windows-command-line (strings)
    (with-output-to-string (out)
      (do* ((strings strings (cdr strings)))
           ((atom strings)     
            (if strings (write-string strings out)))
        (let* ((string (car strings))
               (n (length string))
               (quote-backslash 0)
               (literal-backslash 0))
          (declare (fixnum n quote-backslash literal-backslash))
          (dotimes (i n)
            (let* ((c (schar string i)))
              (case c
                (#\\
                 (unless (or (> quote-backslash 0)
                             (> literal-backslash 0))
                   (do* ((j i (1+ j))
                         (k 0))
                        ((= j n) (setq literal-backslash k))
                     (case (schar string j)
                       (#\\ (incf k))
                       ((#\space #\tab #\")
                        (setq quote-backslash k)
                        (return))
                       (t (setq literal-backslash k)
                          (return)))))
                 (if (> quote-backslash 0)
                   (progn
                     (write-char #\\ out)
                     (write-char #\\ out)
                     (decf quote-backslash))
                   (progn
                     (write-char #\\ out)
                     (decf literal-backslash))))
                ((#\space #\tab)
                 (write-char #\" out)
                 (write-char c out)
                 (write-char #\" out))
                (#\"
                 (write-char #\\ out)
                 (write-char #\" out))
                (t (write-char c out)))))
          (when strings (write-char #\space out))))))

  (defun create-windows-process (new-in new-out new-err cmdstring env)
    (declare (ignore env))              ; until we can do better.
    (with-filename-cstrs ((command cmdstring))
      (rletz ((proc-info #>PROCESS_INFORMATION)
              (si #>STARTUPINFO))
        (setf (pref si #>STARTUPINFO.cb) (record-length #>STARTUPINFO))
        (setf (pref si #>STARTUPINFO.dwFlags)
              (logior #$STARTF_USESTDHANDLES #$STARTF_USESHOWWINDOW))
        (setf (pref si #>STARTUPINFO.wShowWindow) #$SW_HIDE)
        (setf (pref si #>STARTUPINFO.hStdInput)
              (if new-in
                (%int-to-ptr new-in)
                (#_GetStdHandle #$STD_INPUT_HANDLE)))
        (setf (pref si #>STARTUPINFO.hStdOutput)
              (if new-out
                (%int-to-ptr new-out)
                (#_GetStdHandle #$STD_OUTPUT_HANDLE)))
        (setf (pref si #>STARTUPINFO.hStdError)
              (if new-err
                (%int-to-ptr new-err)
                (#_GetStdHandle #$STD_ERROR_HANDLE)))
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
          (values nil (#_GetLastError))
          (progn
            (#_CloseHandle (pref proc-info #>PROCESS_INFORMATION.hThread))
            (values t (pref proc-info #>PROCESS_INFORMATION.hProcess)))))))

  (defun exec-with-io-redirection (new-in new-out new-err args proc &optional env)
    (multiple-value-bind (win handle-to-process-or-error)
        (create-windows-process new-in new-out new-err (make-windows-command-line args) env)
      (if win
        handle-to-process-or-error
        (progn
          (setf (external-process-%status proc) :error
                (external-process-%exit-code proc) handle-to-process-or-error)
          (signal-semaphore (external-process-signal proc))
          (signal-semaphore (external-process-completed proc))
          nil))))

  (defun fd-uninheritable (fd &key direction)
    (let ((new-fd (fd-dup fd :direction direction)))
      (fd-close fd)
      new-fd))

  
  (defun data-available-on-pipe-p (hpipe)
    (rlet ((navail #>DWORD 0))
      (unless (eql 0 (#_PeekNamedPipe (if (typep hpipe 'macptr)
                                        hpipe
                                        (%int-to-ptr hpipe))
                                      (%null-ptr)
                                      0
                                      (%null-ptr)
                                      navail
                                      (%null-ptr)))
        (not (eql 0 (pref navail #>DWORD))))))
    

  ;;; There doesn't seem to be any way to wait on input from an
  ;;; anonymous pipe in Windows (that would, after all, make too
  ;;; much sense.)  We -can- check for pending unread data on
  ;;; pipes, and can expect to eventually get EOF on a pipe.
  ;;; So, this tries to loop until the process handle is signaled and
  ;;; all data has been read.
  (defun monitor-external-process (p)
    (let* ((in-fds (external-process-watched-fds p))
           (out-streams (external-process-watched-streams p))
           (token (external-process-token p))
           (terminated)
           (changed)
           (external-format (external-process-external-format p))
           (encoding (external-format-character-encoding external-format))
           (line-termination (external-format-line-termination external-format))
           (pairs (pairlis (mapcar (lambda (fd)
                                     (cons fd
                                           (make-fd-stream fd
                                                           :direction :input
                                                           :sharing :private
                                                           :encoding encoding
                                                           :interactive t
                                                           :line-termination line-termination)))
                                   in-fds)
                           out-streams))
           )
      (loop
        (when changed
          (setq pairs (delete nil pairs :key #'car)
                changed nil))
        (when (and terminated (null pairs))
          (without-interrupts
           (rlet ((code #>DWORD))
             (loop
               (#_GetExitCodeProcess (external-process-pid p) code)
               (unless (eql (pref code #>DWORD) #$STILL_ACTIVE)
                 (return))
               (#_SleepEx 10 #$TRUE))
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
        (dolist (p pairs)
          (let* ((in-fd (caar p))
                 (in-stream (cdar p))
                 (out-stream (cdr p)))
            (when (or terminated (data-available-on-pipe-p in-fd))
              (let* ((buf (make-string 1024)))
                (declare (dynamic-extent buf))
                (let* ((n (ignore-errors (read-sequence buf in-stream))))
                  (if (or (null n) (eql n 0))
                    (progn
                      (without-interrupts
                       (decf (car token))
                       (fd-close in-fd)
                       (setf (car p) nil changed t)))
                    (progn
                      (write-sequence buf out-stream :end n)
                      (force-output out-stream))))))))
        (unless terminated
          (setq terminated (eql (#_WaitForSingleObjectEx
                                 (external-process-pid p)
                                 1000
                                 #$true)
                                #$WAIT_OBJECT_0))))))
  

  (defun signal-external-process (proc signal)
    "Does nothing on Windows"
    (declare (ignore signal))
    (require-type proc 'external-process)
    nil)  


  )
                                        ;#+windows-target (progn


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

(defun external-process-status (proc)
  "Return information about whether an OS subprocess is running; or, if
not, why not; and what its result code was if it completed."
  (require-type proc 'external-process)
  (values (external-process-%status proc)
          (external-process-%exit-code proc)))

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
	(format t "~%~%;;;~%;;; ~a requires access to ~a~%;;; Type (:y ~D) to yield control to this thread.~%;;;~%"
		*current-process* (shared-resource-name resource)
                (process-serial-number *current-process*))
        (force-output t))
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
             (let* ((n (#_sysconf #$_SC_NPROCESSORS_CONF)))
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
            (rlet ((procmask #>DWORD_PTR)
                   (sysmask #>DWORD_PTR))
              (if (eql 0 (#_GetProcessAffinityMask (#_GetCurrentProcess) procmask sysmask))
                1
                (logcount (pref sysmask #>DWORD_PTR)))))))

(def-load-pointers spin-count ()
  (if (eql 1 (cpu-count))
    (%defglobal '*spin-lock-tries* 1)
    (%defglobal '*spin-lock-tries* 1024))
  (%defglobal '*spin-lock-timeouts* 0))

(defun yield ()
  (process-allow-schedule))

(defloadvar *host-page-size*
    #-(or windows-target android-target)
    (#_getpagesize)
    #+windows-target
    (rlet ((info #>SYSTEM_INFO))
      (#_GetSystemInfo info)
      (pref info #>SYSTEM_INFO.dwPageSize))
    #+android-target
    (#_sysconf #$_SC_PAGE_SIZE)
    )

;;(assert (= (logcount *host-page-size*) 1))


(defun same-fd-p (a b)
  (or (eql a b)
      #-windows-target
      (let* ((a-stat (multiple-value-list (%fstat a)))
             (b-stat (multiple-value-list (%fstat b))))
        (declare (dynamic-extent a-stat b-stat))
        (and (car a-stat) (car b-stat)
             (eql (nth 9 a-stat)
                  (nth 9 b-stat))
             (eql (nth 4 a-stat)
                  (nth 4 b-stat))))
      #+windows-target
      (%stack-block ((a-info (record-length #>BY_HANDLE_FILE_INFORMATION))
                     (b-info (record-length #>BY_HANDLE_FILE_INFORMATION)))
        (unless (or (eql 0 (#_GetFileInformationByHandle (%int-to-ptr a) a-info))
                    (eql 0 (#_GetFileInformationByHandle (%int-to-ptr b) b-info)))
          (and (eql (pref a-info #>BY_HANDLE_FILE_INFORMATION.dwVolumeSerialNumber)
                    (pref b-info #>BY_HANDLE_FILE_INFORMATION.dwVolumeSerialNumber))
               (eql (pref a-info #>BY_HANDLE_FILE_INFORMATION.nFileIndexHigh)
                    (pref b-info #>BY_HANDLE_FILE_INFORMATION.nFileIndexHigh))
               (eql (pref a-info #>BY_HANDLE_FILE_INFORMATION.nFileIndexLow)
                    (pref b-info #>BY_HANDLE_FILE_INFORMATION.nFileIndexLow)))))))

  
(defun get-universal-time ()
  "Return a single integer for the current time of
   day in universal time format."
  (rlet ((tv :timeval))
    (gettimeofday tv)
    (+ (pref tv :timeval.tv_sec) unix-to-universal-time)))

#+windows-target
(defloadvar *windows-allocation-granularity*
    (rlet ((info #>SYSTEM_INFO))
      (#_GetSystemInfo info)
      (pref info #>SYSTEM_INFO.dwAllocationGranularity)))

#-windows-target
(defun %memory-map-fd (fd len bits-per-element)
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
              (values header-addr ndata-elements nalignment-elements))))))))

#+windows-target
(defun %memory-map-fd (fd len bits-per-element)
  (let* ((nbytes (+ *windows-allocation-granularity*
                    (logandc2 (+ len
                                 (1- *windows-allocation-granularity*))
                              (1- *windows-allocation-granularity*))))         
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
      (let* ((mapping (#_CreateFileMappingA (%int-to-ptr fd) (%null-ptr) #$PAGE_READONLY 0 0 (%null-ptr))))
        (if (%null-ptr-p mapping)
          (let* ((err (#_GetLastError)))
            (fd-close fd)
            (error "Couldn't create a file mapping - ~a." (%windows-error-string err)))
          (loop
            (let* ((base (#_VirtualAlloc (%null-ptr) nbytes #$MEM_RESERVE #$PAGE_NOACCESS)))
              (if (%null-ptr-p base)
                (let* ((err (#_GetLastError)))
                  (#_CloseHandle mapping)
                  (fd-close fd)
                  (error "Couldn't reserve ~d bytes of address space for mapped file - ~a"
                         nbytes (%windows-error-string err)))
                ;; Now we have to free the memory and hope that we can reallocate it ...
                (progn
                  (#_VirtualFree base 0 #$MEM_RELEASE)
                  (unless (%null-ptr-p (#_VirtualAlloc base *windows-allocation-granularity* #$MEM_RESERVE #$PAGE_NOACCESS))
                    (let* ((fptr (%inc-ptr base *windows-allocation-granularity*)))
                      (if (%null-ptr-p (#_MapViewOfFileEx mapping #$FILE_MAP_READ 0 0 0 fptr))
                        (#_VirtualFree base 0 #$MEM_RELEASE)
                        (let* ((prefix-page (%inc-ptr base (- *windows-allocation-granularity*
                                                              *host-page-size*))))
                          (#_VirtualAlloc prefix-page *host-page-size* #$MEM_COMMIT #$PAGE_READWRITE)
                          (setf (paref prefix-page (:* :address) 0) mapping
                                (paref prefix-page (:* :address) 1) (%int-to-ptr fd))
                          (return (values
                                   (%inc-ptr prefix-page (- *host-page-size*
                                                            (* 2 target::node-size)))
                                   ndata-elements
                                   nalignment-elements)))))))))))))))
                       


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
            (multiple-value-bind (header-address ndata-elements nalignment-elements)
                (%memory-map-fd fd len bits-per-element)
              (setf (%get-natural header-address 0)
                    (logior (element-type-subtype upgraded-type)
                            (ash (+ ndata-elements nalignment-elements) target::num-subtag-bits)))
              (with-macptrs ((v (%inc-ptr header-address target::fulltag-misc)))
                          (let* ((vector (rlet ((p :address v)) (%get-object p 0))))
                            ;; Tell some parts of Clozure CL - notably the
                            ;; printer - that this thing off in foreign
                            ;; memory is a real lisp object and not
                            ;; "bogus".
                            (with-lock-grabbed (*heap-ivector-lock*)
                              (push vector *heap-ivectors*))
                            (make-array ndata-elements
                                        :element-type upgraded-type
                                        :displaced-to vector
                                        :adjustable t
                                        :displaced-index-offset nalignment-elements))))))))))

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
              (- (funcall (locally
			      ;; Don't really care about speed, but need to turn off typechecking for bootstrapping reasons
			      (declare (optimize (speed 3) (safety 0)))
			    (arch::target-array-data-size-function arch))
                          (ctype-subtype ctype)
                          (length v))
                 target::node-size)))))


#-windows-target
(defun %unmap-file (data-address size-in-octets)
  (let* ((base-address (%inc-ptr data-address (- *host-page-size*)))
         (fd (pref base-address :int)))
    (#_munmap base-address (+ *host-page-size* size-in-octets))
    (fd-close fd)))

#+windows-target
(defun %unmap-file (data-address size-in-octets)
  (declare (ignore size-in-octets))
  (let* ((prefix-page (%inc-ptr data-address (- *host-page-size*)))
         (prefix-allocation (%inc-ptr data-address (- *windows-allocation-granularity*)))
         (mapping (paref prefix-page (:* :address) 0))
         (fd (%ptr-to-int (paref prefix-page (:* :address) 1))))
    (#_UnmapViewOfFile data-address)
    (#_CloseHandle mapping)
    (#_VirtualFree prefix-allocation 0 #$MEM_RELEASE)
    (fd-close fd)))

    

;;; Argument should be something returned by MAP-FILE-TO-IVECTOR;
;;; this should be called at most once for any such object.
(defun unmap-ivector (displaced-vector)
  (multiple-value-bind (data-address size-in-octets)
      (mapped-vector-data-address-and-size displaced-vector)
  (let* ((v (array-displacement displaced-vector)))
      (let* ((element-type (array-element-type displaced-vector)))
        (adjust-array displaced-vector 0
                      :element-type element-type
                      :displaced-to (make-array 0 :element-type element-type)
                      :displaced-index-offset 0))
      (with-lock-grabbed (*heap-ivector-lock*)
        (setq *heap-ivectors* (delete v *heap-ivectors*)))
      (%unmap-file data-address size-in-octets)
      t)))

(defun unmap-octet-vector (v)
  (unmap-ivector v))

#-windows-target
(progn
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


