;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2001 Clozure Associates
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

;;; The "CDB" files used here are similar (but not identical) to those
;;; used in the Unix CDB package <http://cr.yp.to/cdb.html>.  The primary
;;; known & intentional differences are:
;;;
;;; a) key values, record positions, and other 32-bit metadata in the
;;;    files are stored in native (vice little-endian) order.
;;; b) hash values are always non-negative fixnums.
;;;
;;; I haven't thought of a compelling reason to attempt full compatibility.
;;;
;;; The basic idea is that the database files are created in a batch
;;; process and are henceforth read-only (e.g., lookup is optimized by
;;; making insertion & deletion impractical or impossible.)  That's
;;; just about exactly what we want here.
;;;
;;; Those of you keeping score may notice that this is the third or forth
;;; database format that OpenMCL has used for its interface database.
;;; As always, this will hopefully be the last format change; the fact
;;; that this code is self-contained (doesn't depend on any Unix database
;;; library) should make it easier to port to other platforms.

(in-package "CCL")

(defparameter *interface-abi-version* 2)
(defparameter *min-interface-abi-version* 1)

(defconstant cdb-hash-mask (1- (ash 1 29)))

(defun cdb-hash (buf len)
  (declare (fixnum len))
  (let* ((h 5381))
    (declare (fixnum h))
    (dotimes (i len (logand h cdb-hash-mask))
      (setq h (+ h (the fixnum (logand cdb-hash-mask (ash h 5)))))
      (setq h (logxor (the (unsigned-byte 8) (%get-unsigned-byte buf i)) h)))))

(defconstant cdbm-hplist 1000)

(defmacro hp-h (v n)
  `(aref ,v (* ,n 2)))

(defmacro hp-p (v n)
  `(aref ,v (1+ (* ,n 2))))

(defstruct cdbm-hplist
  (hp (make-array (* 2 cdbm-hplist)
		  :element-type '(unsigned-byte 32)
		  :initial-element 0))
  (next nil)
  (num 0))





#+openmcl
(progn
  ;;; Given a (possibly logical) PATHNAME, return a corresponding namestring
  ;;; suitable for passing to an OS file-open call.
  (defun cdb-native-namestring (pathname)
    (native-translated-namestring pathname))
  
  ;;; Open the file specified by PATHNAME for output and return a
  ;;; small integer "file id" (fid).
  (defun fid-open-output (pathname)
    (let ((dir (make-pathname :type nil :name nil :defaults pathname)))
      (unless (probe-file dir)
	(error "The directory ~S does not exist, cannot open/create ~S"
	       dir pathname)))
    (let* ((id (fd-open (cdb-native-namestring pathname)
			(logior #$O_WRONLY #$O_CREAT #$O_TRUNC))))
      (if (< id 0)
	(%errno-disp id pathname)
	id)))

  ;;; Open the file specified by PATHNAME for input and return a
  ;;; file id.
  (defun fid-open-input (pathname)
    (let* ((id (fd-open (cdb-native-namestring pathname) #$O_RDONLY)))
      (if (< id 0)
	(%errno-disp id pathname)
	id)))
  
  ;;; Read N octets from FID into BUF.  Return #of octets read or error.
  (defun fid-read (fid buf n)
    (let* ((count (fd-read fid buf n)))
      (if (< count 0)
	(%errno-disp count "reading from file")
	count)))

  ;;; Write N octets to FID from BUF.  Return #of octets written or error.
  (defun fid-write (fid buf n)
    (let* ((count (fd-write fid buf n)))
      (if (< count 0)
	(%errno-disp count "writing to file")
	count)))

  ;;; Return the absolute (octet) position of FID.
  (defun fid-pos (fid)
    (fd-tell fid))

  ;;; Return the current size of the file referenced by FID, in
  ;;; octets.
  (defun fid-size (fid)
    (fd-size fid))
  
  ;;; Seek to specified position (relative to file start.)
  (defun fid-seek (fid pos)
    (fd-lseek fid pos #$SEEK_SET))

  ;;; Guess what this does ?
  (defun fid-close (fid)
    (fd-close fid))

  ;;; Allocate a block of size N bytes (via malloc, #_NewPtr, etc.)
  (defun cdb-alloc (n)
    (malloc n))

  ;;; Free a block allocated by cdb-alloc.
  (defun cdb-free (block)
    (free block))
  )

;;; I suppose that if we wanted to store these things in little-endian
;;; order this'd be the place to swap bytes ...
(defun fid-write-u32 (fid val)
  (%stack-block ((valptr 4))
    (setf (%get-unsigned-long valptr) val)
    (fid-write fid valptr 4)
    val))

(defun fid-read-u32 (fid)
  (%stack-block ((valptr 4))
    (fid-read fid valptr 4)
    (%get-unsigned-long valptr)))



;;; Write N elements of a vector of type (UNSIGNED-BYTE 32) to file-id
;;; FID, starting at element START.  The vector should be a simple
;;; (non-displaced) array.
(defun fid-write-u32-vector (fid v n start)
  (let* ((remaining-octets (* n 4))
	 (start-octet (* start 4))
	 (bufsize 2048))
    (%stack-block ((buf bufsize))
      (do* ()
	   ((zerop remaining-octets))
	(let* ((chunksize (min remaining-octets bufsize)))
	  (%copy-ivector-to-ptr v start-octet buf 0 chunksize)
	  (fid-write fid buf chunksize)
	  (incf start-octet chunksize)
	  (decf remaining-octets chunksize))))))

(defstruct cdbx
  fid					;a small integer denoting a file
  pathname)				;that file's pathname

;;; A CDBM is used to create a database.
(defstruct (cdbm (:include cdbx))
  (final (make-array (* 256 2)
		     :element-type '(unsigned-byte 32)
		     :initial-element 0))
  (count (make-array 256 :element-type '(unsigned-byte 32) :initial-element 0))
  (start (make-array 256 :element-type '(unsigned-byte 32) :initial-element 0))
  (head nil)
  (split nil)
  (hash nil)
  (numentries 0)
  )

(defun cdbm-open (pathname)
  (let* ((fid (fid-open-output pathname))
	 (cdbm (make-cdbm :fid fid :pathname pathname))
	 (final (cdbm-final cdbm)))
    ;;; Write the (empty) final table to the start of the file.  Twice.
    (fid-write-u32-vector fid final (length final) 0)
    (fid-write-u32-vector fid final (length final) 0)
    cdbm))

;;; Note a newly-added <key,value> pair's file position and hash code.
(defun %cdbm-add-hash-pos (cdbm hash pos)
  (let* ((head (cdbm-head cdbm)))
    (when (or (null head)
	      (>= (cdbm-hplist-num head) cdbm-hplist))
      (setq head (make-cdbm-hplist))
      (setf (cdbm-hplist-next head) (cdbm-head cdbm)
	    (cdbm-head cdbm) head))
    (let* ((num (cdbm-hplist-num head))
	   (hp (cdbm-hplist-hp head)))
      (setf (hp-h hp num) hash
	    (hp-p hp num) pos))
    (incf (cdbm-hplist-num head))
    (incf (cdbm-numentries cdbm))))

(defun cdbm-put (cdbm key data)
  (let* ((fid (cdbm-fid cdbm))
	 (pos (fid-pos fid))
	 (keylen (pref key :cdb-datum.size))
	 (keyptr (pref key :cdb-datum.data))
	 (datalen (pref data :cdb-datum.size))
	 (hash (cdb-hash keyptr keylen)))
    (fid-write-u32 fid keylen)
    (fid-write-u32 fid datalen)
    (fid-write fid keyptr keylen)
    (fid-write fid (pref data :cdb-datum.data) datalen)
    (%cdbm-add-hash-pos cdbm hash pos)))

(defun %cdbm-split (cdbm)
  (let* ((count (cdbm-count cdbm))
	 (start (cdbm-start cdbm))
	 (numentries (cdbm-numentries cdbm)))
    (dotimes (i 256) (setf (aref count i) 0))
    (do* ((x (cdbm-head cdbm) (cdbm-hplist-next x)))
	 ((null x))
      (do* ((i (cdbm-hplist-num x))
	    (hp (cdbm-hplist-hp x)))
	   ((zerop i))
	(decf i)
	(incf (aref count (logand 255 (hp-h hp i))))))
    (let* ((memsize 1))
      (dotimes (i 256)
	(let* ((u (* 2 (aref count i))))
	  (if (> u memsize)
	    (setq memsize u))))
      (incf memsize numentries)
      (let* ((split (make-array (the fixnum (* 2 memsize))
				:element-type '(unsigned-byte 32))))
	(setf (cdbm-split cdbm) split)
	(setf (cdbm-hash cdbm)
	      (make-array (- (* 2 memsize)
			     (* 2 numentries))
			  :element-type '(unsigned-byte 32)
			  :displaced-to split
			  :displaced-index-offset (* 2 numentries)))
	(let* ((u 0))
	  (dotimes (i 256)
	    (incf u (aref count i))
	    (setf (aref start i) u)))

	(do* ((x (cdbm-head cdbm) (cdbm-hplist-next x)))
	     ((null x))
	  (do* ((i (cdbm-hplist-num x))
		(hp (cdbm-hplist-hp x)))
	       ((zerop i))
	    (decf i)
	    (let* ((idx (decf (aref start (logand 255 (hp-h hp i))))))
	      (setf (hp-h split idx) (hp-h hp i)
		    (hp-p split idx) (hp-p hp i)))))))))

(defun %cdbm-throw (cdbm pos b)
  (let* ((count (aref (cdbm-count cdbm) b))
	 (len (* 2 count))
	 (hash (cdbm-hash cdbm))
	 (split (cdbm-split cdbm)))
    (let* ((final (cdbm-final cdbm)))
      (setf (aref final (* 2 b)) pos
	    (aref final (1+ (* 2 b))) len))
    (unless (zerop len)
      (dotimes (j len)
	(setf (hp-h hash j) 0
	      (hp-p hash j) 0))
      (let* ((hpi (aref (cdbm-start cdbm) b)))
	(dotimes (j count)
	  (let* ((where (mod (ash (hp-h split hpi) -8) len)))
	    (do* ()
		 ((zerop (hp-p hash where)))
	      (incf where)
	      (if (= where len)
		(setq where 0)))
	    (setf (hp-p hash where) (hp-p split hpi)
		  (hp-h hash where) (hp-h split hpi)
		  hpi (1+ hpi))))))
    len))

;;; Write data structures to the file, then close the file.
(defun cdbm-close (cdbm)
  (when (cdbm-fid cdbm)
    (%cdbm-split cdbm)
    (let* ((hash (cdbm-hash cdbm))
	   (fid (cdbm-fid cdbm))
	   (pos (fid-pos fid)))
      (dotimes (i 256)
	(let* ((len (%cdbm-throw cdbm pos i)))
	  (dotimes (u len)
	    (fid-write-u32 fid (hp-h hash u))
	    (fid-write-u32 fid (hp-p hash u))
	    (incf pos 8))))
      (write-cdbm-trailer cdbm)
      (fid-seek fid (* 256 2 4)) ; skip the empty "final" table, write the new one
      (let* ((final (cdbm-final cdbm)))
	(fid-write-u32-vector fid final (length final) 0))
      (fid-close fid)
      (setf (cdbm-fid cdbm) nil))))

(defun write-cdbm-trailer (cdbm)
  (let* ((string (format nil "~s ~s ~d " "OpenMCL Interface File" (backend-name *target-backend*) *interface-abi-version*)))
    (%stack-block ((buf 512))
      (%cstr-pointer string buf)
      (fid-write (cdbm-fid cdbm) buf 512))))

      
;;; A CDB is used to access a database.
(defstruct (cdb (:include cdbx))
  (lock (make-lock)))

      
;;; Do the bytes on disk match KEY ?
(defun %cdb-match (fid key keylen)
  (%stack-block ((buf keylen))
    (fid-read fid buf keylen)
    (dotimes (i keylen t)
      (unless (= (the fixnum (%get-unsigned-byte key i))
		 (the fixnum (%get-unsigned-byte buf i)))
	(return)))))

;;; Seek to file position of data associated with key.  Return length
;;; of data (or NIL if no matching key.)
(defun %cdb-seek (fid key keylen)
  (let* ((hash (cdb-hash key keylen)))
    (fid-seek fid (+ (* 256 2 4) (* 8 (logand hash 255))))
    (let* ((pos (fid-read-u32 fid))
           (lenhash (fid-read-u32 fid)))
      (unless (zerop lenhash)
        (let* ((h2 (mod (ash hash -8) lenhash)))
          (dotimes (i lenhash)
            (fid-seek fid (+ pos (* 8 h2)))
            (let* ((hashed-key (fid-read-u32 fid))
                   (poskd (fid-read-u32 fid)))
              (when (zerop poskd)
                (return-from %cdb-seek nil))
              (when (= hashed-key hash)
                (fid-seek fid poskd)
                (let* ((hashed-key-len (fid-read-u32 fid))
                       (data-len (fid-read-u32 fid)))
                  (when (= hashed-key-len keylen)
                    (if (%cdb-match fid key keylen)
                      (return-from %cdb-seek data-len)))))
              (if (= (incf h2) lenhash)
                (setq h2 0)))))))))

;;; This should only be called with the cdb-lock of the containing cdb
;;; held.
(defun %cdb-get (fid key value)
  (setf (pref value :cdb-datum.size) 0
	(pref value :cdb-datum.data) (%null-ptr))
  (when fid
    (let* ((datalen (%cdb-seek fid
                               (pref key :cdb-datum.data)
                               (pref key :cdb-datum.size))))
      (when datalen
        (let* ((buf (cdb-alloc datalen)))
          (fid-read fid buf datalen)
          (setf (pref value :cdb-datum.size) datalen
                (pref value :cdb-datum.data) buf)))
      value)))

(defun cdb-get (cdb key value)
  (with-lock-grabbed ((cdb-lock cdb))
    (%cdb-get (cdb-fid cdb) key value)))

(defun cdb-subdirectory-path (&optional (ftd *target-ftd*))
  (let* ((ftd-name (ftd-interface-db-directory ftd))
	 (ftd-dir (pathname-directory ftd-name)))
    (assert (equalp (pathname-host ftd-name) "ccl"))
    (assert (eq (car ftd-dir) :absolute))
    (cdr ftd-dir)))

(defvar *interfaces-root* "ccl:")

(defun open-interface-db-pathname (name d)
  (let* ((db-path (make-pathname :host (pathname-host *interfaces-root*)
				 :directory (append
					     (or (pathname-directory *interfaces-root*)
						 '(:absolute))
					     (cdb-subdirectory-path *target-ftd*))))
	 (path (merge-pathnames name
				(merge-pathnames (interface-dir-subdir d) db-path))))
    (cdb-open path)))

(defun cdb-open (pathname)
  (if (probe-file pathname)
    (let* ((cdb (make-cdb :fid (fid-open-input (cdb-native-namestring pathname))
                          :pathname (namestring pathname))))
      (cdb-check-trailer cdb))
    (progn
      (if (probe-file (make-pathname :name nil :type nil :defaults pathname))
        (warn "Interface file ~s does not exist." pathname)
        (warn "Interface file ~s does not exist, and the containing directory does not exist.~%This may mean that that the \"ccl:\" logical-pathname host has not been properly initialized. " (translate-logical-pathname pathname)))
      (make-cdb :fid nil :pathname (namestring pathname)))))

(defun cdb-check-trailer (cdb)
  (flet ((error-with-cdb (string &rest args)
           (error "Error in interface file at ~s: ~a"
                  (cdb-pathname cdb) (apply #'format nil string args))))
    (let* ((fid (cdb-fid cdb)))
      (fid-seek fid (- (fid-size fid) 512))
      (%stack-block ((buf 512))
        (fid-read fid buf 512)
        (let* ((string (make-string 512)))
          (dotimes (i 512)
            (setf (%scharcode string i) (%get-unsigned-byte buf i)))
          (with-input-from-string (s string)
            (let* ((sig (ignore-errors (read s)))
                   (target (ignore-errors (read s)))
                   (version (ignore-errors (read s))))
              (if (equal sig "OpenMCL Interface File")
                (if (eq target (backend-name *target-backend*))
                  (if (and version
                           (>= version *min-interface-abi-version*)
                           (<=  version *interface-abi-version*))
                    cdb
                    (error-with-cdb "Wrong interface ABI version. Expected ~d, got ~d" *interface-abi-version* version))
                  cdb #+nil(error-with-cdb "Wrong target."))
                (error-with-cdb "Missing interface file signature.  Obsolete version?")))))))))

                  
    
(defun cdb-close (cdb)
  (let* ((fid (cdb-fid cdb)))
    (setf (cdb-fid cdb) nil)
    (when fid
      (fid-close fid))
    t))

(defmethod print-object ((cdb cdbx) stream)
  (print-unreadable-object (cdb stream :type t :identity t)
    (let* ((fid (cdb-fid cdb)))
      (format stream "~s [~a]" (cdb-pathname cdb) (or fid "closed")))))


(defun cdb-enumerate-keys (cdb &optional (predicate #'true))
  "Returns a list of all keys (strings) in the open .cdb file CDB which
satisfy the optional predicate PREDICATE."
  (with-lock-grabbed ((cdb-lock cdb))
    (let* ((keys ())
           (fid (cdb-fid cdb)))
      (dotimes (i 256 keys)
        (fid-seek fid (+ (* 256 2 4) (* 8 i)))
        (let* ((pos (fid-read-u32 fid))
               (n (fid-read-u32 fid)))
          (dotimes (j n)
            (fid-seek fid (+ pos (* 8 j) 4))
            (let* ((posk (fid-read-u32 fid)))
              (unless (zerop posk)
                (fid-seek fid posk)
                (let* ((hashed-key-len (fid-read-u32 fid)))
                  ;; Skip hashed data length
                  (fid-read-u32 fid)
                  (let* ((string (make-string hashed-key-len)))
                    (%stack-block ((buf hashed-key-len))
                      (fid-read fid buf hashed-key-len)
                      (dotimes (k hashed-key-len)
                        (setf (schar string k)
                              (code-char (%get-unsigned-byte buf k)))))
                    (when (funcall predicate string)
                      (push (copy-seq string) keys))))))))))))
                                        ;
                  


(defstruct ffi-type
  (ordinal nil)
  (defined nil)
  (string)
  (name)                                ; a keyword, uppercased or NIL
)

(defmethod print-object ((x ffi-type) out)
  (print-unreadable-object (x out :type t :identity t)
    (format out "~a" (ffi-type-string x))))

(defvar *ffi-prefix* "")

(defstruct (ffi-mem-block (:include ffi-type))
  fields
  (anon-global-id )
  (alt-alignment-bits nil))

(defstruct (ffi-union (:include ffi-mem-block)
                      (:constructor
                       make-ffi-union (&key
                                       string name
                                       &aux
                                       (anon-global-id
                                        (unless name
                                          (concatenate 'string
                                                       *ffi-prefix*
                                                       "-" string)))))))


(defstruct (ffi-struct (:include ffi-mem-block)
                       (:constructor
                       make-ffi-struct (&key
                                       string name
                                       &aux
                                       (anon-global-id
                                        (unless name
                                          (concatenate 'string
                                                       *ffi-prefix*
                                                       "-" string)))))))

(defstruct (ffi-typedef (:include ffi-type))
  (type))

(defstruct (ffi-objc-class (:include ffi-type))
  super-foreign-name
  protocol-names
  own-ivars
  )

(defstruct (ffi-objc-method)
  class-name
  arglist
  result-type
  flags)

(defstruct (ffi-objc-message (:include ffi-type))
  methods)
                            

(defun ffi-struct-reference (s)
  (or (ffi-struct-name s) (ffi-struct-anon-global-id s)))

(defun ffi-union-reference (u)
  (or (ffi-union-name u) (ffi-union-anon-global-id u)))

(defstruct (ffi-function (:include ffi-type))
  arglist
  return-value)
    


(defconstant db-string-constant 0)
(defconstant db-read-string-constant 1)
(defconstant db-s32-constant 2)
(defconstant db-u32-constant 3)
(defconstant db-float-constant 4)
(defconstant db-double-constant 5)
(defconstant db-char-constant 6)

(defparameter *arg-spec-encoding*
  '((#\Space . :void)
    (#\a . :address)
    (#\F . :signed-fullword)
    (#\f . :unsigned-fullword)
    (#\H . :signed-halfword)
    (#\h . :unsigned-halfword)
    (#\B . :signed-byte)
    (#\b . :unsigned-byte)
    (#\s . :single-float)
    (#\d . :double-float)
    (#\L . :signed-doubleword)
    (#\l . :unsigned-doubleword)
    (#\r . :record)))



(defun decode-arguments (string)
  (let* ((result nil))
    (collect ((args))
      (do* ((i 0 (1+ i)))
           ((= i (length string)) (values (args) result))
        (declare (fixnum i))
        (let* ((ch (schar string i))
               (val (if (or (eql ch #\r) (eql ch #\u) (eql ch #\t))
                      (let* ((namelen (char-code (schar string (incf i))))
                             (name (make-string namelen)))
                        (dotimes (k namelen)
                          (setf (schar name k)
                                (schar string (incf i))))
                        (setq name (escape-foreign-name name))
                        (if (eql ch #\r)
                          `(:struct ,name)
                          (if (eql ch #\u)
                            `(:union ,name)
                            name)))
                      (cdr (assoc ch *arg-spec-encoding*)))))
          (if result
            (args val)
            (setq result val)))))))


;;; encoded external function looks like:
;;; byte min-args
;;; byte name-length
;;; name-length bytes of name
;;; result+arg specs

(defun extract-db-function (datum)
  (let* ((val nil)
         (dsize (pref datum :cdb-datum.size)))
    (with-macptrs ((dptr))
      (%setf-macptr dptr (pref datum :cdb-datum.data))
      (unless (%null-ptr-p dptr)
	(let* ((min-args (%get-byte dptr))
	       (name-len (%get-byte dptr 1))
	       (external-name (%str-from-ptr (%inc-ptr dptr 2) name-len))
	       (encoding-len (- dsize (+ 2 name-len)))
	       (encoding (make-string encoding-len)))
	  (declare (dynamic-extent encoding))
          (%str-from-ptr (%inc-ptr dptr (+ 2 name-len)) encoding-len encoding)
	  (cdb-free (pref datum :cdb-datum.data))
	  (multiple-value-bind (args result)
	      (decode-arguments encoding)
	    (setq val (make-external-function-definition
		       :entry-name external-name
		       :arg-specs args
		       :result-spec result
		       :min-args min-args))))))
    val))

(defun db-lookup-function (cdb name)
  (when cdb
    (rletZ ((value :cdb-datum)
            (key :cdb-datum))
      (with-cstrs ((keyname (string name)))
        (setf (pref key :cdb-datum.data) keyname
              (pref key :cdb-datum.size) (length (string name))
              (pref value :cdb-datum.data) (%null-ptr)
              (pref value :cdb-datum.size) 0)
        (cdb-get cdb key value)
        (extract-db-function value)))))




        
(defun extract-db-constant-value (datum)
  (let* ((val nil)
         (dsize (pref datum :cdb-datum.size)))
    (with-macptrs ((dptr))
      (%setf-macptr dptr (pref datum :cdb-datum.data))
      (unless (%null-ptr-p dptr)
	(let* ((class (pref dptr :dbm-constant.class)))
	  (setq val
		(ecase class
                  ((#.db-string-constant #.db-read-string-constant)
                   (let* ((str (%str-from-ptr (%inc-ptr dptr 4) (- dsize 4))))
                     (if (eql class db-read-string-constant)
                       (read-from-string str)
                       str)))
                  (#.db-s32-constant (pref dptr :dbm-constant.value.s32))
                  (#.db-u32-constant (pref dptr :dbm-constant.value.u32))
                  (#.db-float-constant (pref dptr :dbm-constant.value.single-float))
                  (#.db-double-constant (pref dptr :dbm-constant.value.double-float))
                  (#.db-char-constant (code-char (pref dptr :dbm-constant.value.u32)))))
	  (cdb-free (pref datum :cdb-datum.data)))))
    val))



(defun db-lookup-constant (cdb name)
  (when cdb
    (rletZ ((value :cdb-datum)
            (key :cdb-datum))
      (with-cstrs ((keyname (string name)))
        (setf (pref key :cdb-datum.data) keyname
              (pref key :cdb-datum.size) (length (string name))
              (pref value :cdb-datum.data) (%null-ptr)
              (pref value :cdb-datum.size) 0)
        (cdb-get cdb key value)
        (extract-db-constant-value value)))))
    


(defun db-define-string-constant (cdbm name val &optional (class db-string-constant))
  (let* ((dsize (+ 4 (length val))))
    (%stack-block ((valbuf dsize))
      (dotimes (i (length val))
        (setf (%get-unsigned-byte valbuf (the fixnum (+ 4 i)))
              (%scharcode val i)))
      (setf (%get-long valbuf) class)
      (rletZ ((content :cdb-datum)
	      (key :cdb-datum))
        (setf (pref content :cdb-datum.size) dsize
              (pref content :cdb-datum.data) valbuf)
        (with-cstrs ((keyname (string name)))
          (setf (pref key :cdb-datum.size) (length (string name))
                (pref key :cdb-datum.data) keyname)
	  (cdbm-put cdbm key content))))))
      
(defun db-define-constant (cdbm name val)
  (typecase val
    (string (db-define-string-constant cdbm name val))
    ((or (unsigned-byte 32)
         (signed-byte 32)
         short-float
         double-float
         character)
     (rletZ ((constant :dbm-constant)
	     (content :cdb-datum)
	     (key :cdb-datum))
       (etypecase val
         ((signed-byte 32)
          (setf (pref constant :dbm-constant.value.s32) val)
          (setf (pref constant :dbm-constant.class) db-s32-constant))
         ((unsigned-byte 32)
          (setf (pref constant :dbm-constant.value.u32) val)
          (setf (pref constant :dbm-constant.class) db-u32-constant))
         (short-float
          (setf (pref constant :dbm-constant.value.single-float) val)
          (setf (pref constant :dbm-constant.class) db-float-constant))
         (double-float
          (setf (pref constant :dbm-constant.value.double-float) val)
          (setf (pref constant :dbm-constant.class) db-double-constant))
         (character
          (setf (pref constant :dbm-constant.value.u32) (char-code val))
          (setf (pref constant :dbm-constant.class) db-char-constant)))
       (setf (pref content :cdb-datum.data) constant
             (pref content :cdb-datum.size) (record-length :dbm-constant))
       (with-cstrs ((keyname (string name)))
         (setf (pref key :cdb-datum.data) keyname
               (pref key :cdb-datum.size) (length (string name)))
	 (cdbm-put cdbm key content))))
    (t (db-define-string-constant cdbm name (format nil "~a" val) db-read-string-constant))))


  

(defmacro with-new-db-file ((var pathname) &body body)
  (let* ((db (gensym)))
    `(let* (,db)
      (unwind-protect
           (let* ((,var (setq ,db (cdbm-open ,pathname))))
             ,@body)
        (when ,db (cdbm-close ,db))))))



(defun interface-db-pathname (name d &optional (ftd *target-ftd*))
  (merge-pathnames name
		   (merge-pathnames (interface-dir-subdir d)
				    (ftd-interface-db-directory ftd))))

(def-ccl-pointers reset-db-files ()
  (do-interface-dirs (d)
    (setf (interface-dir-constants-interface-db-file d) nil
	  (interface-dir-functions-interface-db-file d) nil
	  (interface-dir-records-interface-db-file d) nil
	  (interface-dir-types-interface-db-file d) nil
          (interface-dir-vars-interface-db-file d) nil
          (interface-dir-objc-classes-interface-db-file d) nil
          (interface-dir-objc-methods-interface-db-file d) nil)))

(defun db-constants (dir)
  (or (interface-dir-constants-interface-db-file dir)
      (setf (interface-dir-constants-interface-db-file dir)
	    (open-interface-db-pathname "constants.cdb" dir))))

(defun db-objc-classes (dir)
  (or (interface-dir-objc-classes-interface-db-file dir)
      (setf (interface-dir-objc-classes-interface-db-file dir)
            (open-interface-db-pathname "objc-classes.cdb" dir))))

(defun db-objc-methods (dir)
  (or (interface-dir-objc-methods-interface-db-file dir)
      (setf (interface-dir-objc-methods-interface-db-file dir)
            (open-interface-db-pathname "objc-methods.cdb" dir))))

(defun db-vars (dir)
  (or (interface-dir-vars-interface-db-file dir)
      (setf (interface-dir-vars-interface-db-file dir)
	    (open-interface-db-pathname "vars.cdb" dir))))

(defun db-types (dir)
  (or (interface-dir-types-interface-db-file dir)
      (setf (interface-dir-types-interface-db-file dir)
	    (open-interface-db-pathname "types.cdb" dir))))

(defun db-records (dir)
  (or (interface-dir-records-interface-db-file dir)
      (setf (interface-dir-records-interface-db-file dir)
	    (open-interface-db-pathname "records.cdb" dir))))

(defun db-functions (dir)
  (or (interface-dir-functions-interface-db-file dir)
      (setf (interface-dir-functions-interface-db-file dir)
	    (open-interface-db-pathname "functions.cdb" dir))))

(defun load-os-constant (sym &optional query)
  (let* ((val (do-interface-dirs (d)
		    (let* ((v (db-lookup-constant (db-constants d) sym)))
		      (when v (return v))))))
    (if query
      (not (null val))
      (if val
        (let* ((*record-source-file* nil))
          (%defconstant sym val)
          val)
        (error "Constant not found: ~s" sym)))))

(defun %load-var (name &optional query-only)
  (let* ((ftd *target-ftd*)
         (string (if (getf (ftd-attributes ftd)
                           :prepend-underscores)
                   (concatenate 'string "_" (string name))
                   (string name)))
         (fv (gethash string (fvs))))
    (unless fv
      (with-cstrs ((cstring string))
        (let* ((type
                (do-interface-dirs (d)
                  (let* ((vars (db-vars d)))
                    (when vars
                      (rletZ ((value :cdb-datum)
                              (key :cdb-datum))
                        (setf (pref key :cdb-datum.data) cstring
                              (pref key :cdb-datum.size) (length string)
                              (pref value :cdb-datum.data) (%null-ptr)
                              (pref value :cdb-datum.size) 0)
                        (cdb-get vars key value)
                        (let* ((vartype (extract-db-type value ftd)))
                          (when vartype (return vartype)))))))))
          (when type
            (setq fv (%cons-foreign-variable string type))
            (resolve-foreign-variable fv nil)
            (setf (gethash string (fvs)) fv)))))
    (if query-only
      (not (null fv))
      (or fv (error "Foreign variable ~s not found" string)))))


(set-dispatch-macro-character 
 #\# #\&
 (qlfun |#&-reader| (stream char arg)
   (declare (ignore char arg))
   (let* ((package (find-package (ftd-interface-package-name *target-ftd*))))
     (multiple-value-bind (sym query)
         (%read-symbol-preserving-case
          stream
          package)
       (unless *read-suppress*
         (let* ((fv (%load-var sym query)))
           (if query
             fv
             (%foreign-access-form `(%reference-external-entry-point (load-time-value ,fv))
                                   (fv.type fv)
                                   0
                                   nil))))))))


              

(defstruct objc-message-info
  message-name
  methods                               ; all methods
  ambiguous-methods                     ; partitioned by signature
  req-args
  flags
  protocol-methods
  lisp-name
  selector)



   
(defstruct objc-method-info
  message-info
  class-name
  class-pointer                         ;canonical, in some sense
  arglist
  result-type
  flags
  signature
  signature-info
  )



(defmethod print-object ((m objc-method-info) stream)
  (print-unreadable-object (m stream :type t :identity t)
    (format stream "~c[~a ~a]"
            (if (getf (objc-method-info-flags m) :class)
              #\+
              #\-)
            (let* ((name (objc-method-info-class-name m)))
              (if (getf (objc-method-info-flags m) :protocol)
                (format nil "<~a>" name)
                name))
            (objc-message-info-message-name
                          (objc-method-info-message-info m)))))

(defun extract-db-objc-message-info (datum message-name info &optional
                                           (ftd *target-ftd*))
  (with-macptrs ((buf))
    (%setf-macptr buf (pref datum :cdb-datum.data))
    (unless (%null-ptr-p buf)
      (unless info
        (setq info
              (make-objc-message-info
               :message-name (string message-name))))
      (let* ((p 0)
             (nmethods 0)
             (nargs 0))
        (multiple-value-setq (nmethods p) (%decode-uint buf p))
        (multiple-value-setq (nargs p) (%decode-uint buf p))
        (dotimes (i nmethods)
          (let* ((flag-byte (prog1 (%get-unsigned-byte buf p)
                              (incf p)))
                 (is-class-method (logbitp 0 flag-byte))
                 (is-protocol-method (logbitp 1 flag-byte))
                 (class-name ())
                 (result-type ())
                 (arg-types ())
                 (arg-type ()))
            (multiple-value-setq (class-name p) (%decode-name buf p t))
            (multiple-value-setq (result-type p) (%decode-type buf p ftd t))
            (dotimes (i nargs)
              (multiple-value-setq (arg-type p) (%decode-type buf p ftd t))
              (push arg-type arg-types))
            (unless (dolist (m (objc-message-info-methods info))
                      (when (and (eq (getf (objc-method-info-flags m) :class)  is-class-method)
                                 (string= (objc-method-info-class-name m)
                                          class-name))
                        (return t)))
              (let* ((flags ()))
                (if is-class-method
                  (setf (getf flags :class) t))
                (if is-protocol-method
                  (setf (getf flags :protocol) t))
                (push (make-objc-method-info
                                     :message-info info
                                     :class-name class-name
                                     :arglist (nreverse arg-types)
                                     :result-type result-type
                                     :flags flags)
                 (objc-message-info-methods info))))))
        (cdb-free (pref datum :cdb-datum.data))))
    info))

(defun db-note-objc-method-info (cdb message-name message-info)
  (when cdb
    (rletZ ((value :cdb-datum)
            (key :cdb-datum))
      (with-cstrs ((keyname (string message-name)))
        (setf (pref key :cdb-datum.data) keyname
              (pref key :cdb-datum.size) (length (string message-name))
              (pref value :cdb-datum.data) (%null-ptr)
              (pref value :cdb-datum.size) 0)
        (cdb-get cdb key value)
        (extract-db-objc-message-info value message-name message-info)))))

(defun lookup-objc-message-info (message-name &optional message-info)
  (do-interface-dirs (d)
    (setq message-info
          (db-note-objc-method-info (db-objc-methods d) message-name message-info)))
  message-info)

(defun %find-objc-class-info (name)
  (do-interface-dirs (d)
    (let* ((info (db-lookup-objc-class (db-objc-classes d) name)))
      (when info (return info)))))

(defun load-external-function (sym query)
  (let* ((def (or (do-interface-dirs (d)
		    (let* ((f (db-lookup-function (db-functions d) sym)))
		      (when f (return f))))
                  (unless query
                    (error "Foreign function not found: ~s" sym)))))
    (if query
      (not (null def))
      (progn
        (setf (gethash sym (ftd-external-function-definitions
                            *target-ftd*)) def)
        (setf (macro-function sym) #'%external-call-expander)
        sym))))

(defun %read-symbol-preserving-case (stream package)
  (let* ((case (readtable-case *readtable*))
         (query nil)
	 (error nil)
	 (sym nil))
    (let* ((*package* package))
      (unwind-protect
	   (progn
	     (setf (readtable-case *readtable*) :preserve)
             (when (eq #\? (peek-char t stream nil nil))
               (setq query t)
               (read-char stream))
	     (multiple-value-setq (sym error)
	       (handler-case (read stream nil nil)
		 (error (condition) (values nil condition)))))
	(setf (readtable-case *readtable*) case)))
    (when error
      (error error))
    (values sym query)))

(set-dispatch-macro-character 
 #\# #\$
 (qlfun |#$-reader| (stream char arg)
   (declare (ignore char))
   (let* ((package (find-package (ftd-interface-package-name *target-ftd*))))
     (multiple-value-bind (sym query)
         (%read-symbol-preserving-case
	    stream
            package)
       (unless *read-suppress*
         (etypecase sym
           (symbol
            (if query
              (load-os-constant sym query)
              (progn
                (when (eq (symbol-package sym) package)
                  (unless arg (setq arg 0))
                  (ecase arg
                    (0
                     (unless (and (constant-symbol-p sym)
                                  (not (eq (%sym-global-value sym)
                                           (%unbound-marker-8))))
                       (load-os-constant sym)))
                    (1 (makunbound sym) (load-os-constant sym))))
                sym)))
           (string
            (let* ((val 0)
                   (len (length sym)))
              (dotimes (i 4 val)
                (let* ((ch (if (< i len) (char sym i) #\space)))
                  (setq val (logior (ash val 8) (char-code ch)))))))))))))

(set-dispatch-macro-character #\# #\_
  (qlfun |#_-reader| (stream char arg)
    (declare (ignore char))
    (unless arg (setq arg 0))
    (multiple-value-bind (sym query)
        (%read-symbol-preserving-case
		 stream
		 (find-package (ftd-interface-package-name *target-ftd*)))
      (unless *read-suppress*
        (unless (and sym (symbolp sym)) (report-bad-arg sym 'symbol))
        (if query
          (load-external-function sym t)
          (let* ((def (if (eql arg 0)
                        (gethash sym (ftd-external-function-definitions
                                      *target-ftd*)))))
            (if (and def (eq (macro-function sym) #'%external-call-expander))
              sym
              (load-external-function sym nil))))))))

(set-dispatch-macro-character
 #\# #\>
 (qlfun |#>-reader| (stream char arg)
    (declare (ignore char arg))
    (if *read-suppress*
      (progn
        (%read-list-expression stream nil)
        nil)
      (let* ((readtable *readtable*)
             (case (readtable-case readtable))
             (string nil)
             (error nil))
        (unwind-protect
             (progn
               (setf (readtable-case readtable) :preserve)
               (multiple-value-setq (string error)
                 (handler-case (read-symbol-token stream)
                   (error (condition) (values nil condition)))))
          (setf (readtable-case *readtable*) case))
        (when error
          (error error))
        (escape-foreign-name string)))))
             



(eval-when (:compile-toplevel :execute)
  (defconstant encoded-type-void 0)
  (defconstant encoded-type-signed-32 1)
  (defconstant encoded-type-unsigned-32 2)
  (defconstant encoded-type-signed-8 3)
  (defconstant encoded-type-unsigned-8 4)
  (defconstant encoded-type-signed-16 5)
  (defconstant encoded-type-unsigned-16 6)
  (defconstant encoded-type-signed-n 7) ;N
  (defconstant encoded-type-unsigned-n 8) ;N
  (defconstant encoded-type-single-float 9)
  (defconstant encoded-type-double-float 10)
  (defconstant encoded-type-pointer 11) ; <type>
  (defconstant encoded-type-array 12) ; <size> <type>
  (defconstant encoded-type-named-struct-ref 13); <tag>
  (defconstant encoded-type-named-union-ref 14) ;<tag>
  (defconstant encoded-type-named-type-ref 15) ; <name>
  (defconstant encoded-type-anon-struct-ref 16) ; <tag>
  (defconstant encoded-type-anon-union-ref 17) ; <tag>
  (defconstant encoded-type-bitfield-marker 18) ; <nbits>
  )


(defconstant encoded-type-type-byte (byte 5 0))
(defconstant encoded-type-align-byte (byte 3 5)
  "alignment in octets, if other than \"natural\" alignment,")

;;; Constants & function names get saved verbatim.
;;; Record, type, and field names get escaped.

(defun encode-name (name &optional verbatim)
  (if (null name)
    (list 0)
    (let* ((string
	    (if (and (typep name 'keyword)
		     (not verbatim))
	      (unescape-foreign-name name)
	      (string name)))
           (length (length string)))
      (cons length (map 'list #'char-code string)))))

(defun encode-ffi-field (field)
  (destructuring-bind (name type offset width) field
  `(,@(encode-name name)
    ,@(encode-ffi-type type)
    ,@(encode-uint offset)
    ,@(encode-uint width))))

(defun encode-ffi-field-list (fields)
  (let* ((len (length fields)))
    (labels ((encode-fields (fields)
               (if fields
                 `(,@(encode-ffi-field (car fields)) ,@(encode-fields (cdr fields))))))
      `(,@(encode-uint len) ,@(encode-fields fields)))))

(defun encode-ffi-union (u)
  (let* ((name (ffi-union-name u))
	 (alt-align-in-bytes-mask (ash (or (ffi-union-alt-alignment-bits u)
				      0)
				  (- 5 3))))
    (if name
      `(,(logior encoded-type-named-union-ref alt-align-in-bytes-mask)
        ,@(encode-name name)
        ,@(encode-ffi-field-list (ffi-union-fields u)))
      `(,(logior encoded-type-anon-union-ref alt-align-in-bytes-mask)
        ,@(encode-ffi-field-list (ffi-union-fields u))))))

(defun encode-ffi-struct (s)
  (let* ((name (ffi-struct-name s))
	 (alt-align-in-bytes-mask (ash (or (ffi-struct-alt-alignment-bits s)
					   0)
				       (- 5 3))))
    (if name
      `(,(logior encoded-type-named-struct-ref alt-align-in-bytes-mask)
        ,@(encode-name (ffi-struct-name s))
        ,@(encode-ffi-field-list (ffi-struct-fields s)))
      `(,(logior encoded-type-anon-struct-ref alt-align-in-bytes-mask)
        ,@(encode-ffi-field-list (ffi-struct-fields s))))))

(defun encode-ffi-objc-class (c)
  (let* ((protocols (ffi-objc-class-protocol-names c)))
    (labels ((encode-name-list (names)
               (if names
                 `(,@(encode-name (car names) t)
                   ,@(encode-name-list (cdr names))))))
      `(,@(encode-name (ffi-objc-class-string c))
        ,@(encode-name (ffi-objc-class-super-foreign-name c))
        ,@(encode-uint (length protocols))
        ,@(encode-name-list protocols)
        ,@(encode-ffi-field-list (ffi-objc-class-own-ivars c))))))


(defstruct db-objc-class-info
  class-name
  superclass-name
  protocols
  ivars
  instance-methods
  class-methods
  )

(defun extract-db-objc-class (datum &optional (ftd *target-ftd*))
  (let* ((val nil))
    (with-macptrs ((buf))
      (%setf-macptr buf (pref datum :cdb-datum.data))
      (unless (%null-ptr-p buf)
	(let* ((p 0)
               (protocol-count 0)
               (class-name ())
               (superclass-name ())
               (protocol-name ())
               (ivars ()))
          (collect ((protocols))
            (multiple-value-setq (class-name p) (%decode-name buf p t))
            (multiple-value-setq (superclass-name p) (%decode-name buf p t))
            (multiple-value-setq (protocol-count p) (%decode-uint buf p))
            (dotimes (i protocol-count)
              (multiple-value-setq (protocol-name p) (%decode-name buf p t))
              (protocols protocol-name))
            (setq ivars (%decode-field-list buf p ftd))
            (cdb-free (pref datum :cdb-datum.data))
            (setq val (make-db-objc-class-info
                       :class-name class-name
                       :superclass-name superclass-name
                       :ivars ivars
                       :protocols (protocols)
                     ))))))
    val))

(defun db-lookup-objc-class (cdb name)
  (when cdb
    (rletZ ((value :cdb-datum)
            (key :cdb-datum))
      (with-cstrs ((keyname (string name)))
        (setf (pref key :cdb-datum.data) keyname
              (pref key :cdb-datum.size) (length (string name))
              (pref value :cdb-datum.data) (%null-ptr)
              (pref value :cdb-datum.size) 0)
        (cdb-get cdb key value)
        (extract-db-objc-class value)))))

(defun encode-u32 (val)
  `(,(ldb (byte 8 24) val)
    ,(ldb (byte 8 16) val)
    ,(ldb (byte 8 8) val)
    ,(ldb (byte 8 0) val)))

(defun encode-uint (val)
  (collect ((bytes))
    (do* ((b (ldb (byte 7 0) val) (ldb (byte 7 0) val))
          (done nil))
         (done (bytes))
      (when (zerop (setq val (ash val -7)))
        (setq b (logior #x80 b) done t))
      (bytes b))))

    

(defun encode-ffi-type (spec)
  (case (car spec)
    (:primitive
     (let ((primtype (cadr spec)))
       (if (atom primtype)
         (case primtype
           (:float `(,encoded-type-single-float))
           (:double `(,encoded-type-double-float))
           (:void `(,encoded-type-void))
           (:signed `(,encoded-type-signed-32))
           (:unsigned `(,encoded-type-unsigned-32))
           ((:long-double :complex-int
                        :complex-float :complex-double :complex-long-double)
            (encode-ffi-type `(:struct ,primtype))))
         (ecase (car primtype)
           (* `(,encoded-type-pointer ,@(encode-ffi-type
                                           (if (eq (cadr primtype) t)
                                             `(:primitive :void)
                                             (cadr primtype)))))
           (:signed
            (case (cadr primtype)
              (32 `(,encoded-type-signed-32))
              (16 `(,encoded-type-signed-16))
              (8 `(,encoded-type-signed-8))
              (t `(,encoded-type-signed-n ,(cadr primtype)))))
           (:unsigned
            (case (cadr primtype)
              (32 `(,encoded-type-unsigned-32))
              (16 `(,encoded-type-unsigned-16))
              (8 `(,encoded-type-unsigned-8))
              (t `(,encoded-type-unsigned-n ,(cadr primtype)))))))))
     (:struct
      (let* ((s (cadr spec))
             (name (ffi-struct-name s))
	     (alt-align-bytes-mask (ash (or (ffi-struct-alt-alignment-bits s)
					    0)
					(- 5 3))))
      `(,(if name
             (logior encoded-type-named-struct-ref alt-align-bytes-mask)
             (logior encoded-type-anon-struct-ref alt-align-bytes-mask))
        ,@(encode-name (ffi-struct-reference s)))))
     (:union
      (let* ((u (cadr spec))
             (name (ffi-union-name u))
	     (alt-align-bytes-mask (ash (or (ffi-union-alt-alignment-bits u)
					    0)
					(- 5 3)))	     )
      `(,(if name
             (logior encoded-type-named-union-ref alt-align-bytes-mask)
             (logior encoded-type-anon-union-ref alt-align-bytes-mask))
        ,@(encode-name (ffi-union-reference u)))))
     (:typedef
      `(,encoded-type-named-type-ref ,@(encode-name (ffi-typedef-name (cadr spec)))))
     (:pointer
      `(,encoded-type-pointer ,@(encode-ffi-type
                                   (if (eq (cadr spec) t)
                                     '(:primitive :void)
                                     (cadr spec)))))
     (:array
      `(,encoded-type-array ,@(encode-uint (cadr spec)) ,@(encode-ffi-type (caddr spec))))
     (t
      (break "Type spec = ~s" spec))))

(defun encode-ffi-arg-type (spec)
  (case (car spec)
    (:primitive
     (let ((primtype (cadr spec)))
       (if (atom primtype)
         (case primtype
           (:float `(#\s))
           (:double `(#\d))
           (:void `(#\Space))
           (:signed `(#\F))
           (:unsigned `(f))
           ((:long-double :complex-int
			  :complex-float :complex-double :complex-long-double)            
            #|(encode-ffi-arg-type `(:struct ,primtype))|#
            `(#\?)))
         (ecase (car primtype)
           (* `(#\a))
           (:signed
            (let* ((nbits (cadr primtype)))
              (if (<= nbits 8)
                '(#\B)
                (if (<= nbits 16)
                  '(#\H)
                  (if (<= nbits 32)
                    '(#\F)
		    (if (<= nbits 64)
		      `(#\L)
		      '(#\?)))))))
           (:unsigned
            (let* ((nbits (cadr primtype)))
              (if (<= nbits 8)
                '(#\b)
                (if (<= nbits 16)
                  '(#\h)
                  (if (<= nbits 32)
                    '(#\f)
		    (if (<= nbits 64)
		      `(#\l)
		      '(#\?)))))))))))
    ((:struct :union)
     `(,(if (eq (car spec) :struct)
                #\r
                #\u)
           ,@(encode-name (ffi-struct-reference (cadr spec)))))
    (:typedef
     `(#\t ,@(encode-name (ffi-typedef-name (cadr spec)))))
    (:pointer
      `(#\a))
    (:array
      `(#\?))))

(defun encode-ffi-arg-list (args)
  (if args
    `(,@(encode-ffi-arg-type (car args)) ,@(encode-ffi-arg-list (cdr args)))))

(defvar *prepend-underscores-to-ffi-function-names* nil)

(defun encode-ffi-function (f)
  (let* ((args (ffi-function-arglist f))
	 (string (ffi-function-string f))
	 (name (if *prepend-underscores-to-ffi-function-names*
		 (concatenate 'string "_" string)
		 string))
         (min-args (length args))
         (result (ffi-function-return-value f)))
    `(,min-args
      ,@(encode-name name t)		; verbatim
      ,@(encode-ffi-arg-type result)
      ,@(encode-ffi-arg-list args))))

(defun encode-ffi-objc-method (m)
  (let* ((flag-byte (logior (if (getf (ffi-objc-method-flags m) :class) 1 0)
                            (if (getf (ffi-objc-method-flags m) :protocol) 2 0))))
  `(,flag-byte
    ,@(encode-name (ffi-objc-method-class-name m) t)
    ,@(encode-ffi-type (ffi-objc-method-result-type m))
    ,@(apply #'append (mapcar #'encode-ffi-type (ffi-objc-method-arglist m))))))

(defun save-ffi-objc-message (cdbm message)
  (let* ((methods (ffi-objc-message-methods message))
         (nmethods (length methods))
         (nargs (length (ffi-objc-method-arglist (car methods)))))
    (labels ((encode-objc-method-list (ml)
               (when ml
                 `(,@(encode-ffi-objc-method (car ml))
                   ,@(encode-objc-method-list (cdr ml))))))
      (db-write-byte-list cdbm
                          (ffi-objc-message-string message)
                          `(,@(encode-uint nmethods)
                            ,@(encode-uint nargs)
                            ,@(encode-objc-method-list methods))
                          t))))
  
    
(defun save-byte-list (ptr l)
  (do* ((l l (cdr l))
        (i 0 (1+ i)))
       ((null l))
    (let* ((b (car l)))
      (if (typep b 'character)
        (setq b (char-code b)))
      (setf (%get-unsigned-byte ptr i) b))))

(defun db-write-byte-list (cdbm keyname bytes &optional verbatim)
  (let* ((len (length bytes)))
    (%stack-block ((p len))
      (save-byte-list p bytes)
      (rletZ ((contents :cdb-datum)
	      (key :cdb-datum))
        (let* ((foreign-name
		(if verbatim
		  keyname
		  (unescape-foreign-name keyname))))
	  (with-cstrs ((keystring foreign-name))
	    (setf (pref contents :cdb-datum.data) p
		  (pref contents :cdb-datum.size) len
		  (pref key :cdb-datum.data) keystring
		  (pref key :cdb-datum.size) (length foreign-name))
	    (cdbm-put cdbm key contents)))))))

(defun save-ffi-function (cdbm fun)
  (let* ((encoding (encode-ffi-function fun)))
    (db-write-byte-list cdbm
			(ffi-function-string fun)
			encoding
			t)))

(defun save-ffi-typedef (cdbm def)
  (db-write-byte-list cdbm
                       (ffi-typedef-string def)
                       (encode-ffi-type (ffi-typedef-type def))
		       t))

(defun save-ffi-struct (cdbm s)
  (db-write-byte-list cdbm (ffi-struct-reference s) (encode-ffi-struct s)))

(defun save-ffi-union (cdbm u)
  (db-write-byte-list cdbm (ffi-union-reference u) (encode-ffi-union u)))



(defun db-define-var (cdbm name type)
  (db-write-byte-list cdbm
                      (if *prepend-underscores-to-ffi-function-names*
                        (concatenate 'string "_" name)
                        name)
  (encode-ffi-type type) t))

(defun save-ffi-objc-class (cdbm c)
  (db-write-byte-list cdbm (ffi-objc-class-name c) (encode-ffi-objc-class c)))


;;; An "uppercase-sequence" is a maximal substring of a string that
;;; starts with an uppercase character and doesn't contain any
;;; lowercase characters.
(defun count-uppercase-sequences (string)
  (let* ((state :lower)
	 (nupper 0))
    (declare (fixnum nupper))
    (dotimes (i (length string) nupper)
      (let* ((ch (char string i)))
	(case state
	  (:lower 
	   (when (upper-case-p ch)
	     (incf nupper)
	     (setq state :upper)))
	  (:upper
	   (unless (upper-case-p ch)
	     (setq state :lower))))))))

(defun escape-foreign-name (in &optional
			       (count (count-uppercase-sequences in)))
  (intern
   (if (zerop count)
     (string-upcase in)
     (let* ((len (length in))
	    (j 0)
	    (out (make-string (+ len (* 2 count))))
	    (state :lower))
       (flet ((outch (ch)
		(setf (schar out j) ch)
		(incf j)
		ch))
	 (dotimes (i len (progn (if (eq state :upper) (outch #\>)) out))
	   (let* ((ch (char in i)))
	     (cond ((and (upper-case-p ch) (eq state :lower))
		    (outch #\<)
		    (setq state :upper))
		   ((and (not (upper-case-p ch)) (eq state :upper))
		    (outch #\>)
		    (setq state :lower)))
	     (outch (char-upcase ch)))))))
   *keyword-package*))

(defun unescape-foreign-name (key)
  (let* ((string (if (typep key 'symbol)
                   (string-downcase key)
                   (string key)))
	 (nleftbrackets (count #\< string))
         (nrightbrackets (count #\> string))
         (nbrackets (+ nleftbrackets nrightbrackets)))
    (declare (fixnum nleftbrackets nrightbrackets nbrackets))
    (if (zerop nbrackets)
      string
      (if (/= nleftbrackets nrightbrackets)
        (error "Mismatched brackets in ~s." key)
        (let* ((len (length string))
               (out (make-string (- len nbrackets)))
               (j 0)
               (state :lower))
          (dotimes (i len out)
            (let* ((ch (schar string i)))
              (if (or (and (eq ch #\<)
                           (eq state :upper))
                      (and (eq ch #\>)
                           (eq state :lower)))
                (error "Mismatched brackets in ~s." key))
              (case ch
                (#\< (setq state :upper))
                (#\> (setq state :lower))
                (t (setf (schar out j) (if (eq state :upper)
                                         (char-upcase ch)
                                         (char-downcase ch))
                         j (1+ j)))))))))))

	
	
(defun %decode-name (buf p &optional verbatim)
  (declare (type macptr buf) (fixnum p))
  (let* ((n (%get-unsigned-byte buf p)))
    (declare (fixnum n))
    (if (zerop n)
      (values nil (1+ p))
      (let* ((pname (%str-from-ptr (%inc-ptr buf (1+ p)) n)))
        (values (if verbatim pname (escape-foreign-name pname))
                (+ p (1+ n)))))))

(defun %decode-u32 (buf p)
  (declare (fixnum p) (type macptr buf))
  (values (dpb
           (%get-unsigned-byte buf p)
           (byte 8 24)
           (dpb
            (%get-unsigned-byte buf (+ p 1))
            (byte 8 16)
            (dpb
             (%get-unsigned-byte buf (+ p 2))
             (byte 8 8)
             (%get-unsigned-byte buf (+ p 3)))))
          (+ p 4)))

(defun %decode-uint (buf p)
  (do* ((val 0)
        (p p (1+ p))
        (shift 0 (+ shift 7))
        (done nil))
       (done (values val p))
    (let* ((b (%get-unsigned-byte buf p)))
      (setq done (logbitp 7 b) val (logior val (ash (logand b #x7f) shift))))))
       
  
;; Should return a FOREIGN-TYPE structure (except if suppress-typedef-expansion is true, may
;; return a symbol for encoded-type-named-type-ref)
(defun %decode-type (buf p ftd &optional suppress-typedef-expansion)
  (declare (type macptr buf) (fixnum p))
  (let* ((q (1+ p)))
    (ecase (ldb encoded-type-type-byte (%get-unsigned-byte buf p))
      (#.encoded-type-void (values (parse-foreign-type :void) q))
      (#.encoded-type-signed-32 (values (svref *signed-integer-types* 32) q))
      (#.encoded-type-unsigned-32 (values (svref *unsigned-integer-types* 32) q))
      (#.encoded-type-signed-8 (values (svref *signed-integer-types* 8) q))
      (#.encoded-type-unsigned-8 (values (svref *unsigned-integer-types* 8) q))
      (#.encoded-type-signed-16 (values (svref *signed-integer-types* 16) q))
      (#.encoded-type-unsigned-16 (values (svref *unsigned-integer-types* 16) q))
      (#.encoded-type-signed-n (values (let* ((bits (%get-unsigned-byte buf q)))
                                         (if (<= bits 32)
                                           (svref *signed-integer-types* bits)
                                           (make-foreign-integer-type
                                            :signed t
                                            :bits bits)))
                                         (1+ q)))
      (#.encoded-type-unsigned-n (values (let* ((bits (%get-unsigned-byte buf q)))
                                         (if (<= bits 32)
                                           (svref *unsigned-integer-types* bits)
                                           (make-foreign-integer-type
                                            :signed nil
                                            :bits bits)))
                                           (1+ q)))
      (#.encoded-type-single-float (values (parse-foreign-type :float) q))
      (#.encoded-type-double-float (values (parse-foreign-type :double) q))
      (#.encoded-type-pointer (multiple-value-bind (target qq)
                                  (%decode-type buf q ftd suppress-typedef-expansion)
                                (values (make-foreign-pointer-type
                                         :to target
                                         :bits (getf (ftd-attributes ftd)
                                                     :bits-per-word)
                                         )
                                          qq)))
      (#.encoded-type-array
       (multiple-value-bind (size qq) (%decode-uint buf q)
         (multiple-value-bind (target qqq) (%decode-type buf qq ftd)
           (let* ((type-alignment (foreign-type-alignment target))
                  (type-bits (foreign-type-bits target)))
             (values (make-foreign-array-type
                      :element-type target
                      :dimensions (list size)
                      :alignment type-alignment
                      :bits (if type-bits
                              (* (align-offset type-bits type-alignment) size)))
                     qqq)))))
      (#.encoded-type-named-type-ref
       (multiple-value-bind (name qq) (%decode-name buf q)         
         (values (if suppress-typedef-expansion
                   name
                   (%parse-foreign-type name))
                 qq)))
      (#.encoded-type-named-struct-ref
       (multiple-value-bind (name qq) (%decode-name buf q)
         (values (or (info-foreign-type-struct name)
                     (setf (info-foreign-type-struct name)
                           (make-foreign-record-type :kind :struct
                                                     :name name)))
                 qq)))
      (#.encoded-type-named-union-ref
       (multiple-value-bind (name qq) (%decode-name buf q)
         (values (or (info-foreign-type-union name)
                     (setf (info-foreign-type-union name)
                           (make-foreign-record-type :kind :union
                                                     :name name)))
                 qq)))
      ((#.encoded-type-anon-struct-ref #.encoded-type-anon-union-ref)
       (multiple-value-bind (tag qq) (%decode-name buf q t)
         (values (load-record tag) qq))))))

(defun extract-db-type (datum ftd)
  (let* ((data (pref datum :cdb-datum.data)))
    (unless (%null-ptr-p data)
      (prog1
	  (%decode-type data 0 ftd)
	(cdb-free data)))))

(defun %load-foreign-type (cdb name ftd)
  (when cdb
    (with-cstrs ((string (string name)))
      (rletZ ((contents :cdb-datum)
              (key :cdb-datum))
        (setf (pref key :cdb-datum.size) (length (string name))
            (pref key :cdb-datum.data) string
            (pref contents :cdb-datum.data) (%null-ptr)
            (pref contents :cdb-datum.size) 0)
      (cdb-get cdb key contents)
      (let* ((type (extract-db-type contents ftd)))
	(if type
	  (%def-foreign-type (escape-foreign-name name) type ftd)))))))

(defun load-foreign-type (name &optional (ftd *target-ftd*))
  (let* ((name (unescape-foreign-name name)))
    (do-interface-dirs (d ftd)
      (let* ((type (%load-foreign-type (db-types d) name ftd)))
	(when type (return type))))))

(defun %decode-field (buf p ftd)
  (declare (type macptr buf) (fixnum p))
  (multiple-value-bind (name p) (%decode-name buf p)
    (multiple-value-bind (type p) (%decode-type buf p ftd)
      (multiple-value-bind (offset p) (%decode-uint buf p)
        (multiple-value-bind (width p) (%decode-uint buf p)
          (values (make-foreign-record-field :type type
                                             :name name
                                             :bits width
                                             :offset offset)
                  p))))))

(defun %decode-field-list (buf p ftd)
  (declare (type macptr buf) (fixnum p))
  (let* ((n nil)
         (fields nil))
    (multiple-value-setq (n p) (%decode-uint buf p))
    (dotimes (i n (values (nreverse fields) p))
      (multiple-value-bind (field q) (%decode-field buf p ftd)
        (push field fields)
        (setq p q)))))

(defun %determine-record-attributes (rtype parsed-fields &optional alt-align)
  (let* ((total-bits 0)
         (overall-alignment 1)
	 #+(and darwinppc-target ppc32-target)
	 (first-field-p t)
         (kind (foreign-record-type-kind rtype)))
    (dolist (field parsed-fields)
      (let* ((field-type (foreign-record-field-type field))
             (bits (ensure-foreign-type-bits field-type))
             (natural-alignment (foreign-type-alignment field-type))
	     (alignment (if alt-align
			  (min natural-alignment alt-align)
			  #+(and darwinppc-target ppc32-target)
			  (if first-field-p
			    (progn
			      (setq first-field-p nil)
			      natural-alignment)
			    (min 32 natural-alignment))
			  #-(and darwinppc-target ppc32-target)
			  natural-alignment)))
        (unless bits
          (error "Unknown size: ~S"
                 (unparse-foreign-type field-type)))
        (unless alignment
          (error "Unknown alignment: ~S"
                 (unparse-foreign-type field-type)))
        (setq overall-alignment (max overall-alignment (if (= alignment 1) 32 alignment)))
        (ecase kind
          (:struct (let* ((imported-offset (foreign-record-field-offset field))
                          (offset (or imported-offset (align-offset total-bits alignment))))
                     (unless imported-offset
                       (setf (foreign-record-field-offset field) offset))
                     (setq total-bits (+ offset bits))))
          (:union (setq total-bits (max total-bits bits))))))
    (setf (foreign-record-type-fields rtype) parsed-fields
          (foreign-record-type-alignment rtype) (or
						 alt-align
						 overall-alignment)
          (foreign-record-type-bits rtype) (align-offset
					    total-bits
					    (or alt-align overall-alignment))
	  (foreign-record-type-alt-align rtype) alt-align)
    rtype))

(defun %decode-record-type (buf p ftd already)
  (declare (type macptr buf) (fixnum p))
  (let* ((rbyte (%get-unsigned-byte buf p))
	 (rcode (ldb encoded-type-type-byte rbyte))
	 (ralign-in-bytes (ldb encoded-type-align-byte rbyte))
	 (alt-align (unless (zerop ralign-in-bytes)
		      (the fixnum (ash ralign-in-bytes 3)))))
    (declare (fixnum rbyte rcode ralign-in-bytes))
    (multiple-value-bind (name q)
        (case rcode
          ((#.encoded-type-anon-struct-ref #.encoded-type-anon-union-ref)
           (values nil (1+ p)))
          (t
           (%decode-name buf (1+ p))))
      (%determine-record-attributes
       (or already
           (if name
             (if (eql rcode encoded-type-named-struct-ref)
               (or (info-foreign-type-struct name)
                   (setf (info-foreign-type-struct name)
                         (make-foreign-record-type :kind :struct :name name)))
               (or (info-foreign-type-union name)
                   (setf (info-foreign-type-union name)
                         (make-foreign-record-type :kind :union :name name))))
             (make-foreign-record-type
              :kind (if (eql rcode encoded-type-anon-struct-ref)
                      :struct
                      :union)
              :name name)))
       (%decode-field-list buf q ftd)
       alt-align))))

(defun extract-db-record (datum ftd already)
  (let* ((data (pref datum :cdb-datum.data)))
    (unless (%null-ptr-p data)
      (prog1
	  (%decode-record-type data 0 ftd already)
	(cdb-free data)))))


(defun %load-foreign-record (cdb name ftd already)
  (when cdb
    (with-cstrs ((string (string name)))
      (rlet ((contents :cdb-datum)
             (key :cdb-datum))
        (setf (pref key :cdb-datum.size) (length (string name))
              (pref key :cdb-datum.data) string
              (pref contents :cdb-datum.data) (%null-ptr)
              (pref contents :cdb-datum.size) 0)
        (cdb-get cdb key contents)
        (extract-db-record contents ftd already)))))

(defun load-record (name &optional (ftd *target-ftd*))
  ;; Try to destructively modify any info we already have.  Use the
  ;; "escaped" name (keyword) for the lookup here.
  (let* ((already (or (info-foreign-type-struct name ftd)
                      (info-foreign-type-union name ftd)))
         (name (unescape-foreign-name name)))
    (do-interface-dirs (d)
      (let* ((r (%load-foreign-record (db-records d) name ftd already)))
	(when r (return r))))))


