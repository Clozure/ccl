;;;-*-Mode: LISP; Package: CCL -*-
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

(defstruct (file-ioblock (:include ioblock))
  (octet-pos 0 )                       ; current io position in octets
  (fileeof 0 )                          ; file length in elements
  )




;;; The file-ioblock-octet-pos field is the (octet) position
;;; at which the next I/O operation will begin (e.g., where the
;;; input came from and/or where the output's going.)  There are
;;; cases (e.g., after a STREAM-CLEAR-INPUT) when this can't be
;;; determined (based on its previous value and the logical size
;;; of the buffer) so we'll have to ask the OS.

(defun file-octet-filepos (file-ioblock)
  (fd-tell (file-ioblock-device file-ioblock)))

(defun synch-file-octet-filepos (file-ioblock)
  (setf (file-ioblock-octet-pos file-ioblock)
	(file-octet-filepos file-ioblock)))

(defun infer-line-termination (file-ioblock)
  (let* ((encoding (or (file-ioblock-encoding file-ioblock)
                       (get-character-encoding nil)))
         (inbuf (file-ioblock-inbuf file-ioblock))
         (buffer (io-buffer-buffer inbuf))
         (n (io-buffer-count inbuf)))
    (when (zerop n)
      (setq n (or (fd-stream-advance (file-ioblock-stream file-ioblock)
                                     file-ioblock
                                     t)
                  0)))
    (multiple-value-bind (nchars last)
        (funcall (character-encoding-length-of-vector-encoding-function encoding)
                 buffer
                 0
                 n)
      (declare (fixnum nchars last))
      (let* ((string (make-string nchars)))
        (declare (dynamic-extent string))
        (decode-character-encoded-vector encoding buffer 0 last string)
        (let* ((line-termination
                (do* ((i 0 (1+ i))
                      (last-was-cr nil))
                     ((= i nchars) (if last-was-cr :cr))
                  (declare (fixnum i))
                  (let* ((char (schar string i)))
                    (if last-was-cr
                      (if (eq char #\Linefeed)
                        (return :crlf)
                        (return :cr))
                      (case char
                        (#\Newline (return nil))
                        (#\Line_Separator (return :unicode))
                        (#\Return (setq last-was-cr t))))))))
          (when line-termination
            (install-ioblock-input-line-termination file-ioblock line-termination)
            (when (file-ioblock-outbuf file-ioblock)
              (install-ioblock-output-line-termination file-ioblock line-termination))))))
    (when (eq (ioblock-owner file-ioblock) *current-process*)
      (setf (ioblock-owner file-ioblock) 0))))



(defvar *default-external-format* :unix)

(defvar *default-file-character-encoding* nil)

(defmethod default-character-encoding ((domain (eql :file)))
  *default-file-character-encoding*)

(defvar *default-line-termination* :unix
  "The value of this variable is used when :EXTERNAL-FORMAT is
unspecified or specified as :DEFAULT. It can meaningfully be given any
of the values :UNIX, :MACOS, :MSDOS, :UNICODE or :INFERRED, each of which is
interpreted as described in the documentation.

Because there's some risk that unsolicited newline translation could have
undesirable consequences, the initial value of this variable in Clozure CL
is :UNIX.")

(defstruct (external-format (:constructor %make-external-format)
                            (:copier nil))
  (character-encoding :default :read-only t)
  (line-termination :default :read-only t))

(defmethod print-object ((ef external-format) stream)
  (print-unreadable-object (ef stream :type t :identity t)
    (format stream "~s/~s" (external-format-character-encoding ef) (external-format-line-termination ef))))



(defvar *external-formats* (make-hash-table :test #'equal))

(defun make-external-format (&key (domain t)
                                  (character-encoding :default)
                                  (line-termination :default))
  (if (eq line-termination :default)
    (setq line-termination *default-line-termination*))
  (unless (assq line-termination *canonical-line-termination-conventions*)
    (error "~S is not a known line-termination format." line-termination))

  (if (eq character-encoding :default)
    (setq character-encoding
          (default-character-encoding domain)))
  (unless (lookup-character-encoding character-encoding)
    (error "~S is not the name of a known characer encoding."
           character-encoding))
  (let* ((pair (cons character-encoding line-termination)))
    (declare (dynamic-extent pair))    
    (or (gethash pair *external-formats*)
        (setf (gethash (cons character-encoding line-termination) *external-formats*)
              (%make-external-format :character-encoding character-encoding
                                     :line-termination line-termination)))))



(defun normalize-external-format (domain external-format)
  (cond ((listp external-format)
         (unless (plistp external-format)
           (error "External-format ~s is not a property list." external-format))
         (normalize-external-format domain (apply #'make-external-format :domain domain  external-format)))
        ((typep external-format 'external-format)
         external-format)
        ((eq external-format :default)
         (normalize-external-format domain *default-external-format*))
        ((lookup-character-encoding external-format)
         (normalize-external-format domain `(:character-encoding ,external-format)))
        ((assq external-format *canonical-line-termination-conventions*)
         (normalize-external-format domain `(:line-termination ,external-format)))
        (t
         (error "Invalid external-format: ~s" external-format))))
               
           
    




;;; Establish a new position for the specified file-stream.
(defun file-ioblock-seek (file-ioblock newoctetpos)
  (let* ((result (fd-lseek
		  (file-ioblock-device file-ioblock) newoctetpos #$SEEK_SET)))
    (if (< result 0)
      (error 'simple-stream-error
	     :stream (file-ioblock-stream file-ioblock)
	     :format-control (format nil "Can't set file position to ~d: ~a"
				     newoctetpos (%strerror result)))
      newoctetpos)))

;;; For input streams, getting/setting the position is fairly simple.
;;; Getting the position is a simple matter of adding the buffer
;;; origin to the current position within the buffer.
;;; Setting the position involves either adjusting the buffer index
;;; (if the new position is within the current buffer) or seeking
;;; to a new position.

(defun %ioblock-input-file-position (file-ioblock newpos)
  (let* ((octet-base (file-ioblock-octet-pos file-ioblock))
	 (element-base (ioblock-octets-to-elements file-ioblock octet-base))
	 (inbuf (file-ioblock-inbuf file-ioblock))
	 (curpos (+ element-base (io-buffer-idx inbuf))))
    (if (null newpos)
      curpos
      (progn
	(if (and (>= newpos element-base)
		 (< newpos (+ element-base (io-buffer-count inbuf))))
	  (setf (io-buffer-idx inbuf) (- newpos element-base))
	  (file-ioblock-seek-and-reset file-ioblock
				       (ioblock-elements-to-octets
					file-ioblock
					newpos)))
	newpos))))

;;; For (pure) output streams, it's a little more complicated.  If we
;;; have to seek to a new origin, we may need to flush the buffer
;;; first.

(defun %ioblock-output-file-position (file-ioblock newpos)
  (let* ((octet-base (file-ioblock-octet-pos file-ioblock))
	 (element-base (ioblock-octets-to-elements file-ioblock octet-base))
	 (outbuf (file-ioblock-outbuf file-ioblock))
	 (curpos (+ element-base (io-buffer-idx outbuf)))
	 (maxpos (+ element-base (io-buffer-count outbuf))))
    (if (null newpos)
      curpos
      (progn
        (unless (= newpos 0)
          (setf (ioblock-pending-byte-order-mark file-ioblock) nil))
	(if (and (>= newpos element-base)
		 (<= newpos maxpos))
	  ;; Backing up is easy.  Skipping forward (without flushing
	  ;; and seeking) would be hard, 'cause we can't tell what
	  ;; we're skipping over.
	  (let* ((newidx (- newpos element-base)))
	    (setf (io-buffer-idx outbuf) newidx))
	  (progn
	    (when (file-ioblock-dirty file-ioblock)
	      (fd-stream-force-output (file-ioblock-stream file-ioblock)
                                      file-ioblock
                                      (io-buffer-count outbuf)
                                      nil)
	      ;; May have just extended the file; may need to update
	      ;; fileeof.
	      (when (> maxpos (file-ioblock-fileeof file-ioblock))
		(setf (file-ioblock-fileeof file-ioblock) maxpos)))
	    (file-ioblock-seek-and-reset file-ioblock
					 (ioblock-elements-to-octets
					  file-ioblock
					  newpos))))
	newpos))))

;;; For I/O file streams, there's an additional complication: if we
;;; back up within the (shared) buffer and the old position was beyond
;;; the buffer's input count, we have to set the input count to the
;;; old position.  (Consider the case of writing a single element at
;;; the end-of-file, backing up one element, then reading the element
;;; we wrote.)  We -can- skip forward over stuff that's been read;
;;; if the buffer's dirty, we'll eventually write it back out.

(defun %ioblock-io-file-position (file-ioblock newpos)
  (let* ((octet-base (file-ioblock-octet-pos file-ioblock))
	 (element-base (ioblock-octets-to-elements file-ioblock octet-base))
	 (outbuf (file-ioblock-outbuf file-ioblock)) ; outbuf = inbuf
	 (curidx (io-buffer-idx outbuf))
	 (curpos (+ element-base curidx)))
    (if (null newpos)
      curpos
      (let* ((incount (io-buffer-count outbuf)))
        (unless (= newpos 0)
          (setf (ioblock-pending-byte-order-mark file-ioblock) nil))        
	(cond 
	  ((and (>= newpos element-base)
		(<= newpos curpos))
	   ;; If we've read less than we've written, make what's
	   ;; been written available for subsequent input.
	   (when (> curidx incount)
	     (setf (io-buffer-count outbuf) curidx))
	   (setf (io-buffer-idx outbuf) (- newpos element-base)))
	  ((and (>= newpos element-base)
		(< newpos (+ element-base incount)))
	   (setf (io-buffer-idx outbuf) (- newpos element-base)))
	  (t
	   (let* ((maxpos (+ element-base (io-buffer-count outbuf))))
	     (when (> maxpos (file-ioblock-fileeof file-ioblock))
	       (setf (file-ioblock-fileeof file-ioblock) maxpos)))
	   (when (file-ioblock-dirty file-ioblock)
	     (file-ioblock-seek file-ioblock octet-base)
	     (fd-stream-force-output (file-ioblock-stream file-ioblock)
                                     file-ioblock
                                     (io-buffer-count outbuf)
                                     nil))
	   (file-ioblock-seek-and-reset file-ioblock
					(ioblock-elements-to-octets
					 file-ioblock newpos))))
	newpos))))

;;; Again, it's simplest to define this in terms of the stream's direction.
;;; Note that we can't change the size of file descriptors open for input
;;; only.

(defun %ioblock-input-file-length (file-ioblock newlen)
  (unless newlen
    (file-ioblock-fileeof file-ioblock)))
 
(defun %ioblock-output-file-length (file-ioblock newlen)
  (let* ((octet-base (file-ioblock-octet-pos file-ioblock))
	 (element-base (ioblock-octets-to-elements file-ioblock octet-base))
	 (outbuf (file-ioblock-outbuf file-ioblock)) 
	 (curidx (io-buffer-idx outbuf))
	 (maxpos (+ element-base (io-buffer-count outbuf)))
	 (curlen (file-ioblock-fileeof file-ioblock)))
    (if (> maxpos curlen)
      (setf (file-ioblock-fileeof file-ioblock) (setq curlen maxpos)))
    (if (null newlen)
      curlen
      (let* ((fd (file-ioblock-device file-ioblock))
	     (new-octet-eof (ioblock-elements-to-octets file-ioblock newlen))
	     (cur-octet-pos (fd-tell fd)))
	(cond ((> newlen curlen)
	       ;; Extend the file; maintain the current position.
	       ;; ftruncate isn't guaranteed to extend a file past
	       ;; its current EOF.  Seeking to the new EOF, then
	       ;; writing, is guaranteed to do so.  Seek to the
	       ;; new EOF, write a random byte, truncate to the
	       ;; specified length, then seek back to where we
	       ;; were and pretend that nothing happened.
	       (file-ioblock-seek file-ioblock new-octet-eof)
	       (%stack-block ((buf 1))
			     (fd-write fd buf 1))
	       (fd-ftruncate fd new-octet-eof)
	       (file-ioblock-seek file-ioblock cur-octet-pos))
	      ((> newlen maxpos)
	       ;; Make the file shorter.  Doesn't affect
	       ;; our position or anything that we have buffered.
	       (fd-ftruncate fd new-octet-eof))
	      ((< newlen element-base)
	       ;; Discard any buffered output.  Truncate the
	       ;; file, then seek to the new EOF.
	       (fd-ftruncate fd new-octet-eof)
	       (file-ioblock-seek-and-reset file-ioblock new-octet-eof))
	      (t
	       (fd-ftruncate fd new-octet-eof)
	       (let* ((newidx (- newlen element-base)))
		 (when (> maxpos newlen)
		   (setf (io-buffer-count outbuf) newidx))
		 (when (> curidx newidx)
		   (setf (io-buffer-idx outbuf) newidx)))))
	(setf (file-ioblock-fileeof file-ioblock) newlen)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass fundamental-file-stream (fd-stream file-stream)
    ((filename :initform nil :initarg :filename :accessor file-stream-filename)
     (actual-filename :initform nil :initarg :actual-filename)
     (external-format :initform :default :initarg :external-format
		      :accessor file-stream-external-format)))

  

(defmethod stream-filename ((s fundamental-file-stream))
  (file-stream-filename s))

(defmethod stream-actual-filename ((s file-stream))
  (slot-value s 'actual-filename))

(defmethod (setf stream-filename) (new (s fundamental-file-stream))
  (setf (file-stream-filename s) new))

(defmethod (setf stream-actual-filename) (new (s fundamental-file-stream))
  (setf (slot-value s 'actual-filename) new))

(defun print-file-stream (s out)
  (print-unreadable-object (s out :type t :identity t)
    (let* ((file-ioblock (stream-ioblock s nil)))
      (format out "(~s/" (stream-filename s))
      (if file-ioblock
	(format out "~d ~a)" (file-ioblock-device file-ioblock) (encoding-name (ioblock-encoding file-ioblock)))
	(format out ":closed")))))
    
(defmethod print-object ((s fundamental-file-stream) out)
  (print-file-stream s out))

(make-built-in-class 'basic-file-stream 'file-stream 'basic-stream)

(defmethod stream-filename ((s basic-file-stream))
  (basic-file-stream.filename s))

(defmethod stream-actual-filename ((s basic-file-stream))
  (basic-file-stream.actual-filename s))

(defmethod (setf stream-filename) (new (s basic-file-stream))
  (setf (basic-file-stream.filename s) new))

(defmethod (setf stream-actual-filename) (new (s basic-file-stream))
  (setf (basic-file-stream.actual-filename s) new))

(defmethod print-object ((s basic-file-stream) out)
  (print-file-stream s out))


(defmethod initialize-basic-stream ((s basic-file-stream) &key element-type external-format &allow-other-keys)
  (setf (getf (basic-stream.info s) :element-type) element-type)
  (setf (basic-file-stream.external-format s) external-format))

(defmethod stream-create-ioblock ((stream fundamental-file-stream) &rest args &key)
  (declare (dynamic-extent args))
  (apply #'make-file-ioblock :stream stream args))

(defmethod stream-create-ioblock ((stream basic-file-stream) &rest args &key)
  (declare (dynamic-extent args))
  (apply #'make-file-ioblock :stream stream args))

(defclass fundamental-file-input-stream (fundamental-file-stream fd-input-stream)
    ())

(make-built-in-class 'basic-file-input-stream 'basic-file-stream 'basic-input-stream)


(defclass fundamental-file-output-stream (fundamental-file-stream fd-output-stream)
    ())

(make-built-in-class 'basic-file-output-stream 'basic-file-stream 'basic-output-stream)

(defclass fundamental-file-io-stream (fundamental-file-stream fd-io-stream)
    ())

(make-built-in-class 'basic-file-io-stream 'basic-file-stream 'basic-io-stream)


(defclass fundamental-file-character-input-stream (fundamental-file-input-stream
					  fd-character-input-stream)
    ())

(make-built-in-class 'basic-file-character-input-stream 'basic-file-input-stream 'basic-character-input-stream)


(defclass fundamental-file-character-output-stream (fundamental-file-output-stream
                                                    fd-character-output-stream)
    ())

(make-built-in-class 'basic-file-character-output-stream 'basic-file-output-stream 'basic-character-output-stream)

(defclass fundamental-file-character-io-stream (fundamental-file-io-stream
				       fd-character-io-stream)
    ())

(make-built-in-class 'basic-file-character-io-stream 'basic-file-io-stream 'basic-character-io-stream)

(defclass fundamental-file-binary-input-stream (fundamental-file-input-stream
                                                fd-binary-input-stream)
    ())

(make-built-in-class 'basic-file-binary-input-stream 'basic-file-input-stream 'basic-binary-input-stream)

(defclass fundamental-file-binary-output-stream (fundamental-file-output-stream
                                                 fd-binary-output-stream)
    ())

(make-built-in-class 'basic-file-binary-output-stream 'basic-file-output-stream 'basic-binary-output-stream)

(defclass fundamental-file-binary-io-stream (fundamental-file-io-stream fd-binary-io-stream)
    ())

(make-built-in-class 'basic-file-binary-io-stream 'basic-file-io-stream 'basic-binary-io-stream)




;;; This stuff is a lot simpler if we restrict the hair to the
;;; case of file streams opened in :io mode (which have to worry
;;; about flushing the shared buffer before filling it, and things
;;; like that.)

(defmethod stream-clear-input ((f fundamental-file-input-stream))
  (with-stream-ioblock-input (file-ioblock f :speedy t)
    (call-next-method)
    (synch-file-octet-filepos file-ioblock)
    nil))


(defmethod stream-clear-input ((f basic-file-input-stream))
  (let* ((file-ioblock (basic-stream-ioblock f)))
    (with-ioblock-input-locked (file-ioblock)
      (call-next-method)
      (synch-file-octet-filepos file-ioblock)
      nil)))

    
(defmethod stream-clear-input ((f fundamental-file-io-stream))
  (with-stream-ioblock-input (file-ioblock f :speedy t)
    (stream-force-output f)		
    (call-next-method)
    (synch-file-octet-filepos file-ioblock)
    nil))

(defmethod stream-clear-input ((f basic-file-io-stream))
  (let* ((file-ioblock (basic-stream-ioblock f)))
    (with-ioblock-input-locked (file-ioblock)
      (call-next-method)
      (synch-file-octet-filepos file-ioblock)
      nil)))

(defmethod stream-clear-output ((f fundamental-file-output-stream))
  (with-stream-ioblock-output (file-ioblock f :speedy t)
    (call-next-method)
    (synch-file-octet-filepos file-ioblock)
    nil))

(defmethod stream-clear-output ((f basic-file-output-stream))
  (let* ((file-ioblock (basic-stream-ioblock f)))
    (with-ioblock-input-locked (file-ioblock)
      (call-next-method)
      (synch-file-octet-filepos file-ioblock)
      nil)))


  
;;; If we've been reading, the file position where we're going
;;; to read this time is (+ where-it-was-last-time what-we-read-last-time.)
(defun input-file-ioblock-advance (stream file-ioblock read-p)
  (let* ((newpos (+ (file-ioblock-octet-pos file-ioblock)
		    (io-buffer-count (file-ioblock-inbuf file-ioblock)))))
    (setf (file-ioblock-octet-pos file-ioblock) newpos)
    (fd-stream-advance stream file-ioblock read-p)))

;;; If the buffer's dirty, we have to back up and rewrite it before
;;; reading in a new buffer.
(defun io-file-ioblock-advance (stream file-ioblock read-p)
  (let* ((curpos (file-ioblock-octet-pos file-ioblock))
	 (count (io-buffer-count (file-ioblock-inbuf file-ioblock)))
	 (newpos (+ curpos 
		    (ioblock-elements-to-octets file-ioblock count))))
    (when (ioblock-dirty file-ioblock)
      (file-ioblock-seek file-ioblock curpos)
      (fd-stream-force-output stream file-ioblock count nil))
    (unless (eql newpos (file-octet-filepos file-ioblock))
      (error "Expected newpos to be ~d, fd is at ~d"
	     newpos (file-octet-filepos file-ioblock)))
    (setf (file-ioblock-octet-pos file-ioblock) newpos)
    (fd-stream-advance stream file-ioblock read-p)))

		    
(defun output-file-force-output (stream file-ioblock count finish-p)
  (let* ((pos (%ioblock-output-file-position file-ioblock nil))
         (n (fd-stream-force-output stream file-ioblock count finish-p)))
    (incf (file-ioblock-octet-pos file-ioblock) (or n 0))
    (%ioblock-output-file-position file-ioblock pos)
    n))

;;; Can't be sure where the underlying fd is positioned, so seek first.
(defun io-file-force-output (stream file-ioblock count finish-p)
  (let* ((pos (%ioblock-io-file-position file-ioblock nil)))
    (file-ioblock-seek file-ioblock (file-ioblock-octet-pos file-ioblock))
    (let* ((n (fd-stream-force-output stream file-ioblock count finish-p)))
      (incf (file-ioblock-octet-pos file-ioblock) (or n 0))
      (%ioblock-io-file-position file-ioblock pos)
      n)))


;;; Invalidate both buffers and seek to the new position.  The output
;;; buffer's been flushed already if it needed to be.

(defun file-ioblock-seek-and-reset (file-ioblock newoctetpos)
  (let* ((inbuf (file-ioblock-inbuf file-ioblock))
	 (outbuf (file-ioblock-outbuf file-ioblock)))
    (setf (file-ioblock-dirty file-ioblock) nil)
    (when inbuf
      (setf (io-buffer-count inbuf) 0
	    (io-buffer-idx inbuf) 0))
    (when outbuf
      (setf (io-buffer-count outbuf) 0
	    (io-buffer-idx outbuf) 0))
    (setf (file-ioblock-octet-pos file-ioblock) newoctetpos)
    (file-ioblock-seek file-ioblock newoctetpos)))

(defmethod stream-position ((stream fundamental-file-input-stream) &optional newpos)
  (with-stream-ioblock-input (file-ioblock stream :speedy t)
    (%ioblock-input-file-position file-ioblock newpos)))


(defmethod stream-position ((stream basic-file-input-stream) &optional newpos)
  (let* ((file-ioblock (basic-stream-ioblock stream)))
    (with-ioblock-input-locked (file-ioblock)
      (%ioblock-input-file-position file-ioblock newpos))))

(defmethod stream-position ((stream fundamental-file-output-stream) &optional newpos)
  (with-stream-ioblock-output (file-ioblock stream :speedy t)
    (%ioblock-output-file-position file-ioblock newpos)))

(defmethod stream-position ((stream basic-file-output-stream) &optional newpos)
  (let* ((file-ioblock (basic-stream-ioblock stream)))
    (with-ioblock-output-locked (file-ioblock)
      (%ioblock-output-file-position file-ioblock newpos))))


(defmethod stream-position ((stream fundamental-file-io-stream) &optional newpos)
  (with-stream-ioblock-input (file-ioblock stream :speedy t)
    (%ioblock-io-file-position file-ioblock newpos)))

(defmethod stream-position ((stream basic-file-io-stream) &optional newpos)
  (let* ((file-ioblock (basic-stream-ioblock stream)))
    (with-ioblock-input-locked (file-ioblock)
      (%ioblock-io-file-position file-ioblock newpos))))


(defmethod stream-length ((stream fundamental-file-input-stream) &optional newlen)
  (with-stream-ioblock-input (file-ioblock stream :speedy t)
    (let* ((res (%ioblock-input-file-length file-ioblock newlen)))
      (and res (>= res 0) res))))


(defmethod stream-length ((stream basic-file-input-stream) &optional newlen)
  (let* ((file-ioblock (basic-stream-ioblock stream)))
    (with-ioblock-input-locked (file-ioblock)
      (let* ((res (%ioblock-input-file-length file-ioblock newlen)))
        (and res (>= res 0) res)))))


(defmethod stream-length ((s fundamental-file-output-stream) &optional newlen)
  (with-stream-ioblock-output (file-ioblock s :speedy t)
    (let* ((res (%ioblock-output-file-length file-ioblock newlen)))
      (and res (>= res 0) res))))


(defmethod stream-length ((stream basic-file-output-stream) &optional newlen)
  (let* ((file-ioblock (basic-stream-ioblock stream)))
    (with-ioblock-output-locked (file-ioblock)
      (let* ((res (%ioblock-output-file-length file-ioblock newlen)))
        (and res (>= res 0) res)))))

(defmethod stream-length ((s fundamental-file-io-stream) &optional newlen)
  (with-stream-ioblock-input (file-ioblock s :speedy t)
    (let* ((res (%ioblock-output-file-length file-ioblock newlen)))
      (and res (>= res 0) res))))

(defmethod stream-length ((stream basic-file-io-stream) &optional newlen)
  (let* ((file-ioblock (basic-stream-ioblock stream)))
    (with-ioblock-input-locked (file-ioblock)
      (let* ((res (%ioblock-output-file-length file-ioblock newlen)))
        (and res (>= res 0) res)))))

(defun close-file-stream (s abort)
  (when (open-stream-p s)
    (let* ((ioblock (stream-ioblock s t))
	   (filename (stream-filename s))
	   (actual-filename (stream-actual-filename s)))
      (when actual-filename ; t => created when opened
	(if abort
	  (progn
	    (setf (ioblock-dirty ioblock) nil)
	    (fd-stream-close s ioblock)
            (if (eq actual-filename t)
              (delete-file filename)
              (unix-rename (namestring actual-filename) (probe-file-x filename))))
	  (unless (eq actual-filename t)
            (delete-file actual-filename))))
      (remove-open-file-stream s))))


(defmethod close ((s fundamental-file-stream) &key abort)
  (close-file-stream s abort)
  (call-next-method))

(defmethod close ((s basic-file-stream) &key abort)
  (close-file-stream s abort)
  (call-next-method))

(defmethod select-stream-class ((class fundamental-file-stream) in-p out-p char-p)
  (if char-p
    (if (and in-p out-p)
      'fundamental-file-character-io-stream
      (if in-p
	'fundamental-file-character-input-stream
	(if out-p
	  'fundamental-file-character-output-stream
	  'fundamental-file-stream)))
    (if (and in-p out-p)
      'fundamental-file-binary-io-stream
      (if in-p
	'fundamental-file-binary-input-stream
	(if out-p
	  'fundamental-file-binary-output-stream
	  'fundamental-file-stream)))))

(defmethod select-stream-class ((class file-stream) in-p out-p char-p)
  (if char-p
    (if (and in-p out-p)
      'fundamental-file-character-io-stream
      (if in-p
	'fundamental-file-character-input-stream
	(if out-p
	  'fundamental-file-character-output-stream
	  'fundamental-file-stream)))
    (if (and in-p out-p)
      'fundamental-file-binary-io-stream
      (if in-p
	'fundamental-file-binary-input-stream
	(if out-p
	  'fundamental-file-binary-output-stream
	  'fundamental-file-stream)))))

(defmethod map-to-basic-stream-class-name ((name (eql 'fundamental-file-stream)))
  'basic-file-stream)

(defmethod map-to-basic-stream-class-name ((name (eql 'file-stream)))
  'basic-file-stream)

(defmethod select-stream-class ((class (eql 'basic-file-stream)) in-p out-p char-p)
  (if char-p
    (if (and in-p out-p)
      'basic-file-character-io-stream
      (if in-p
	'basic-file-character-input-stream
	(if out-p
	  'basic-file-character-output-stream
	  'basic-file-stream)))
    (if (and in-p out-p)
      'basic-file-binary-io-stream
      (if in-p
	'basic-file-binary-input-stream
	(if out-p
	  'basic-file-binary-output-stream
	  'basic-file-stream)))))


(defmethod select-stream-advance-function ((s file-stream) direction)
  (ecase direction
    (:io 'io-file-ioblock-advance)
    (:input 'input-file-ioblock-advance)))

(defmethod select-stream-force-output-function ((s file-stream) direction)
  (ecase direction
    (:io 'io-file-force-output)
    (:output 'output-file-force-output)))

(defmethod select-stream-untyi-function ((s file-stream) (direction t))
  '%file-ioblock-untyi)

;;; Conceptually, decrement the stream's position by the number of octets
;;; needed to encode CHAR.
;;; Since we don't use IOBLOCK-UNTYI-CHAR, it's hard to detect the error
;;; of calling UNREAD-CHAR twice in a row.
(defun %file-ioblock-untyi (ioblock char)
  (let* ((inbuf (ioblock-inbuf ioblock))
         (idx (io-buffer-idx inbuf))
         (encoding (ioblock-encoding ioblock))
         (noctets (if encoding
                    (funcall (character-encoding-character-size-in-octets-function encoding) char)
                    1)))
    (declare (fixnum idx noctets))
    (if (>= idx noctets)
      (setf (io-buffer-idx inbuf) (the fixnum (- idx noctets)))
      (let* ((stream (ioblock-stream ioblock))
             (pos (stream-position stream))
             (newpos (- pos noctets)))
        (if (< newpos 0)
          (error "Invalid attempt to unread ~s on ~s." char (ioblock-stream ioblock))
          (stream-position stream newpos))))
    char))



(defun make-file-stream (filename
			 direction
			 element-type
			 if-exists
			 if-does-not-exist
			 class
			 external-format
                         sharing
                         basic)
  (let* ((temp-name nil)
         (created nil)
         (dir (pathname-directory filename))
         (filename (if (eq (car dir) :relative)
                     (full-pathname filename)
                     filename))
         (pathname (pathname filename))) 
    (block open
      (if (or (memq element-type '(:default character base-char))
	      (subtypep element-type 'character))
	(if (eq element-type :default)(setq element-type 'character))
	(progn
	  (setq element-type (type-expand element-type))
	  (cond ((equal element-type '#.(type-expand 'signed-byte))
		 (setq element-type '(signed-byte 8)))
		((equal element-type '#.(type-expand 'unsigned-byte))
		 (setq element-type '(unsigned-byte 8))))))
      (case direction
	(:probe (setq if-exists :ignored))
	(:input (setq if-exists :ignored))
	((:io :output) nil)
	(t (report-bad-arg direction '(member :input :output :io :probe))))
      (check-pathname-not-wild filename) ;; probe-file-x misses wild versions....
      (multiple-value-bind (native-truename kind)(probe-file-x filename)
	(if native-truename
	  (if (eq kind :directory)
	    (if (eq direction :probe)
	      (return-from open nil)
	      (signal-file-error (- #$EISDIR)  filename))
	    (if (setq filename (if-exists if-exists filename "Open ..."))
	      (progn
		(multiple-value-setq (native-truename kind) (probe-file-x filename))
		(cond 
		  ((not native-truename)
		   (setq native-truename (%create-file filename)
                         created t))
		  ((memq direction '(:output :io))
		   (when (eq if-exists :supersede)
		     (let ((truename (native-to-pathname native-truename)))
		       (setq temp-name (gen-file-name truename))
		       (unix-rename native-truename (native-untranslated-namestring temp-name))
		       (%create-file native-truename))))))
	      (return-from open nil)))
	  (if (setq filename (if-does-not-exist if-does-not-exist filename))
            (progn
              (unless (setq native-truename (%create-file filename :if-exists if-exists))
                (return-from open nil))
              (setq created t))
	    (return-from open nil)))
	(let* ((fd (fd-open native-truename (case direction
					      ((:probe :input) #$O_RDONLY)
					      (:output #$O_WRONLY)
					      (:io #$O_RDWR)))))
	  (when (< fd 0)  (signal-file-error fd filename))
          (let* ((fd-kind (%unix-fd-kind fd)))
            (if (not (eq fd-kind :file))
              (make-fd-stream fd :direction direction
                              :element-type element-type
                              :sharing sharing
                              :basic basic)
              (progn
                (when basic
                  (setq class (map-to-basic-stream-class-name class))
                  (setq basic (subtypep (find-class class) 'basic-stream)))
                (let* ((in-p (member direction '(:io :input)))
                       (out-p (member direction '(:io :output)))
                       (io-p (eq direction :io))
                       (char-p (or (eq element-type 'character)
                                   (subtypep element-type 'character)))
                       (elements-per-buffer (optimal-buffer-size fd element-type))
                       (real-external-format
                        (if char-p
                          (normalize-external-format :file external-format)
                          ))
                       (line-termination (if char-p (external-format-line-termination real-external-format)))
                       (encoding (if char-p (external-format-character-encoding real-external-format)))
                       (class-name (select-stream-class class in-p out-p char-p))
                       (class (find-class class-name))
                       (fstream (make-ioblock-stream
                                 class
                                 :insize (if in-p elements-per-buffer)
                                 :outsize (if (and out-p (not io-p))
                                            elements-per-buffer)
                                 :share-buffers-p io-p
                                 :interactive nil
                                 :direction direction
                                 :element-type element-type
                                 :direction direction
                                 :listen-function 'fd-stream-listen
                                 :close-function 'fd-stream-close
                                 :advance-function
                                 (if in-p (select-stream-advance-function class direction))
                                 :force-output-function
                                 (if out-p (select-stream-force-output-function
                                           class direction))
                                 :device fd
                                 :encoding encoding
                                 :external-format (or real-external-format :binary)
                                 :sharing sharing
                                 :line-termination line-termination
                                 :character-p (or (eq element-type 'character)
                                                  (subtypep element-type 'character))))
                       (ioblock (stream-ioblock fstream t)))
                  (setf (stream-filename fstream) (namestring pathname)
                        (stream-actual-filename fstream) (or temp-name created))
                  (setf (file-ioblock-fileeof ioblock)
                        (ioblock-octets-to-elements ioblock (fd-size fd)))
                  (when (and in-p (eq line-termination :inferred))
                    (infer-line-termination ioblock))
                  (cond ((eq if-exists :append)
                         (file-position fstream :end))
                        ((and (memq direction '(:io :output))
                              (neq if-exists :overwrite))
                         (stream-length fstream 0)))
                  (if (eq direction :probe)
                    (close fstream)
                    (note-open-file-stream fstream))
                  fstream)))))))))






(defmethod stream-external-format ((s broadcast-stream))
  (let* ((last (last-broadcast-stream s)))
    (if last
        (stream-external-format s)
        :default)))

;;; Under the circumstances, this is a very slow way of saying
;;; "we don't support EXTENDED-CHARs".
(defun file-string-length (stream object)
  "Return the delta in STREAM's FILE-POSITION that would be caused by writing
   OBJECT to STREAM. Non-trivial only in implementations that support
   international character sets."
  (if (typep stream 'broadcast-stream)
    (let* ((last (last-broadcast-stream stream)))
      (if last
	(file-string-length last object)
	1))
    (progn
      (unless (and (typep stream 'file-stream)
		   (let* ((eltype (stream-element-type stream)))
		     (or (eq 'character eltype)
			 (eq 'base-char eltype)
			 (subtypep eltype 'character))))
	(error "~S is not a file stream capable of character output" stream))
      (if (typep object 'character)
        (setq object (make-string 1 :initial-element object))
        (progn
          (require-type object 'string)))
      (let* ((start 0)
             (end (length object)))
        (multiple-value-bind (data offset) (array-data-and-offset object)
          (unless (eq data object)
            (setq object data)
            (incf start offset)
            (incf end offset)))
        (let* ((external-format (stream-external-format stream))
               (encoding (get-character-encoding (external-format-character-encoding external-format)))
               (line-termination (external-format-line-termination external-format)))
          (-
           (+ (funcall (character-encoding-octets-in-string-function encoding)
                       object
                       start
                       end)
              (if (eq line-termination :crlf)
                (* (count #\Newline object :start start :end end)
                   (file-string-length stream #\Return))
                0))
           (if (eql (file-position stream) 0)
             0
             (length (character-encoding-bom-encoding encoding)))))))))
  
