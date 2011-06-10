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

(in-package "CCL")

;;;

(defclass stream ()
  ())


(defclass input-stream (stream)
  ())


(defclass output-stream (stream) ())

(defmethod stream-direction ((s stream))
  )

(defmethod stream-domain ((s stream))
  t)


(defmethod stream-direction ((s input-stream))
  (if (typep s 'output-stream)
    :io
    :input))

(defmethod stream-direction ((s output-stream))
  (if (typep s 'input-stream)
    :io
    :output))

(defun check-io-timeout (timeout)
  (when timeout
    (require-type timeout '(real 0 1000000))))

(defmethod stream-input-timeout ((s input-stream))
  nil)

(defmethod (setf input-stream-timeout) (new (s input-stream))
  (check-io-timeout new))

(defmethod stream-output-timeout ((s output-stream))
  nil)

(defmethod (setf stream-output-timeout) (new (s output-stream))
  (check-io-timeout new))

;;; Try to return a string containing characters that're near the
;;; stream's current position, if that makes sense.  Return NIL
;;; if it doesn't make sense.
;;; Some things (SOCKET-ERRORs) are signaled as STREAM-ERRORs
;;; whose STREAM args aren't streams.  That's wrong, but
;;; defining this method on T keeps things from blowing up worse.
(defmethod stream-surrounding-characters ((s t))
  (declare (ignore s))
  nil)


;;; The "direction" argument only helps us dispatch on two-way streams:
;;; it's legal to ask for the :output device of a stream that's only open
;;; for input, and one might get a non-null answer in that case.
(defmethod stream-device ((s stream) direction)
  (declare (ignore direction)))

;;; Some generic stream functions:
(defmethod stream-length ((x t) &optional new)
  (declare (ignore new))
  (report-bad-arg x 'stream))

(defmethod stream-position ((x t) &optional new)
  (declare (ignore new))
  (report-bad-arg x 'stream))

(defmethod stream-element-type ((x t))
  (report-bad-arg x 'stream))

(defmethod stream-force-output ((x t))
  (report-bad-arg x 'stream))

(defmethod stream-position ((s stream) &optional newpos)
  (declare (ignore newpos)))

;;; For input streams:

;; From Shannon Spires, slightly modified.
(defun generic-read-line (s)
  (let* ((str (make-array 20 :element-type 'base-char
			  :adjustable t :fill-pointer 0))
	 (eof nil))
    (do* ((ch (read-char s nil :eof) (read-char s nil :eof)))
	 ((or (eq ch #\newline) (setq eof (eq ch :eof)))
	  (values (ensure-simple-string str) eof))
      (vector-push-extend ch str))))

(defun generic-character-read-list (stream list count)
  (declare (fixnum count))
  (do* ((tail list (cdr tail))
	(i 0 (1+ i)))
       ((= i count) count)
    (declare (fixnum i))
    (let* ((ch (read-char stream nil :eof)))
      (if (eq ch :eof)
	(return i)
	(rplaca tail ch)))))

(defun generic-binary-read-list (stream list count)
  (declare (fixnum count))
  (do* ((tail list (cdr tail))
	(i 0 (1+ i)))
       ((= i count) count)
    (declare (fixnum i))
    (let* ((ch (stream-read-byte stream)))
      (if (eq ch :eof)
	(return i)
	(rplaca tail ch)))))

(defun generic-character-read-vector (stream vector start end)
  (declare (fixnum start end))
  (do* ((i start (1+ i)))
       ((= i end) end)
    (declare (fixnum i))
    (let* ((ch (stream-read-char stream)))
      (if (eq ch :eof)
	(return i)
	(setf (uvref vector i) ch)))))

(defun generic-binary-read-vector (stream vector start end)
  (declare (fixnum start end))
  (do* ((i start (1+ i)))
       ((= i end) end)
    (declare (fixnum i))
    (let* ((byte (stream-read-byte stream)))
      (if (eq byte :eof)
	(return i)
	(setf (uvref vector i) byte)))))


;;; For output streams:

(defun generic-advance-to-column (s col)
  (let* ((current (column s)))
    (unless (null current)
      (when (< current col)
	(do* ((i current (1+ i)))
	     ((= i col))
	  (write-char #\Space s)))
      t)))



(defun generic-stream-write-string (stream string start end)
  (setq end (check-sequence-bounds string start end))
  (locally (declare (fixnum start end))
    (multiple-value-bind (vect offset) (array-data-and-offset string)
      (declare (fixnum offset))
      (unless (zerop offset)
	(incf start offset)
	(incf end offset))
      (do* ((i start (1+ i)))
	   ((= i end) string)
	(declare (fixnum i))
	(write-char (schar vect i) stream)))))












(defstatic *heap-ivectors* ())
(defvar *heap-ivector-lock* (make-lock))



(defun %make-heap-ivector (subtype size-in-bytes size-in-elts)
  (with-macptrs ((ptr (malloc (+ size-in-bytes
                                 #+32-bit-target (+ 4 2 7) ; 4 for header, 2 for delta, 7 for round up
                                 #+64-bit-target (+ 8 2 15) ; 8 for header, 2 for delta, 15 for round up
                                 ))))
    (let ((vect (fudge-heap-pointer ptr subtype size-in-elts))
          (p (%null-ptr)))
      (%vect-data-to-macptr vect p)
      (with-lock-grabbed (*heap-ivector-lock*)
        (push vect *heap-ivectors*))
      (values vect p))))

(defun %heap-ivector-p (v)
  (with-lock-grabbed (*heap-ivector-lock*)
    (not (null (member v *heap-ivectors* :test #'eq)))))


(defun dispose-heap-ivector (v)
  (if (%heap-ivector-p v)
    (with-macptrs (p)
      (with-lock-grabbed (*heap-ivector-lock*)
        (setq *heap-ivectors* (delq v *heap-ivectors*)))
      (%%make-disposable p v)
      (free p))))

(defun %dispose-heap-ivector (v)
  (dispose-heap-ivector v))

(defun make-heap-ivector (element-count element-type)
  (require-type element-count `(unsigned-byte ,(- target::nbits-in-word
						  target::num-subtag-bits)))
  (let* ((subtag (ccl::element-type-subtype element-type)))
    (unless
        #+ppc32-target
        (= (logand subtag ppc32::fulltagmask)
               ppc32::fulltag-immheader)
        #+ppc64-target
        (= (logand subtag ppc64::lowtagmask)
           ppc64::lowtag-immheader)
        #+x8632-target
        (= (logand subtag x8632::fulltagmask)
	   x8632::fulltag-immheader)
        #+x8664-target
        (logbitp (the (mod 16) (logand subtag x8664::fulltagmask))
                 (logior (ash 1 x8664::fulltag-immheader-0)
                         (ash 1 x8664::fulltag-immheader-1)
                         (ash 1 x8664::fulltag-immheader-2)))
        #+arm-target
        (= (logand subtag arm::fulltagmask)
           arm::fulltag-immheader)
      (error "~s is not an ivector subtype." element-type))
    (let* ((size-in-octets (ccl::subtag-bytes subtag element-count)))
      (multiple-value-bind (vector pointer)
          (ccl::%make-heap-ivector subtag size-in-octets element-count)
        (values vector pointer size-in-octets)))))









(defvar *elements-per-buffer* 2048)  ; default buffer size for file io

(defmethod streamp ((x t))
  nil)

(defmethod streamp ((x stream))
  t)

(defmethod stream-io-error ((stream stream) error-number context)
  (error 'simple-stream-error :stream stream
	 :format-control (format nil "~a during ~a"
				 (%strerror error-number) context)))



(defmethod stream-write-char ((stream stream) char)
  (declare (ignore char))
  (error "stream ~S is not capable of output" stream))

(defun stream-write-entire-string (stream string)
  (stream-write-string stream string))


(defmethod stream-read-char ((x t))
  (report-bad-arg x 'stream))

(defmethod stream-read-char ((stream stream))
  (error "~s is not capable of input" stream))

(defmethod stream-unread-char ((x t) char)
  (declare (ignore char))
  (report-bad-arg x 'stream))

(defmethod stream-unread-char ((stream stream) char)
  (declare (ignore char))
  (error "stream ~S is not capable of input" stream))



(defmethod stream-force-output ((stream output-stream)) nil)
(defmethod stream-maybe-force-output ((stream stream))
  (stream-force-output stream))

(defmethod stream-finish-output ((stream output-stream)) nil)



(defmethod stream-clear-output ((stream output-stream)) nil)

(defmethod close ((stream stream) &key abort)
  (declare (ignore abort))
  (open-stream-p stream))

(defmethod close-for-termination ((stream stream) abort)
  (close stream :abort abort))


(defmethod open-stream-p ((x t))
  (report-bad-arg x 'stream))

(defmethod open-stream-p ((stream stream))
  t)

(defmethod stream-external-format ((x t))
  (report-bad-arg x 'stream))

(defmethod stream-external-format ((s stream))
  nil)


(defmethod (setf stream-external-format) (new (s t))
  (normalize-external-format (stream-domain s) new)
  (report-bad-arg s 'stream))



    
(defmethod stream-fresh-line ((stream output-stream))
  (terpri stream)
  t)

(defmethod stream-line-length ((stream stream))
  "This is meant to be shadowed by particular kinds of streams,
   esp those associated with windows."
  *default-right-margin*)

(defmethod interactive-stream-p ((x t))
  (report-bad-arg x 'stream))

(defmethod interactive-stream-p ((stream stream)) nil)

(defmethod stream-clear-input ((x t))
  (report-bad-arg x 'input-stream))

(defmethod stream-clear-input ((stream input-stream)) nil)

(defmethod stream-listen ((stream input-stream))
  (not (eofp stream)))

(defmethod stream-filename ((stream stream))
  (report-bad-arg stream 'file-stream))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; For input streams, the IO-BUFFER-COUNT field denotes the number
;;; of elements read from the underlying input source (e.g., the
;;; file system.)  For output streams, it's the high-water mark of
;;; elements output to the buffer.

(defstruct io-buffer
               ;; This type is too complex during bootstrapping.
  (buffer nil #|:type (or (simple-array * (*)) null)|#)
  (bufptr nil :type (or macptr null))
  (size 0 :type fixnum)			; size (in octets) of buffer
  (idx 0 :type fixnum)			; index of next element
  (count 0 :type fixnum)		; count of active elements
  (limit 0 :type fixnum)		; size (in elements) of buffer
  (translate nil)                       ; newline-translation
  )

(defmethod print-object ((buf io-buffer) out)
  (print-unreadable-object (buf out :identity t :type t)
    (let* ((buffer (io-buffer-buffer buf)))
      (when buffer (format out " ~s " (array-element-type buffer))))
    (format out "~d/~d/~d"
	    (io-buffer-idx buf)
	    (io-buffer-count buf)
	    (io-buffer-limit buf))))

(defstruct ioblock
  stream                                ; the stream being buffered
  untyi-char                            ; nil or last value passed to
                                        ;  stream-unread-char
  (inbuf nil :type (or null io-buffer))
  (outbuf nil :type (or null io-buffer))
  (element-type 'character)
  (element-shift 0 :type fixnum)        ;element shift count
  (charpos 0 :type (or null fixnum))     ;position of cursor
  (device -1 :type (or null fixnum))     ;file descriptor
  (advance-function 'ioblock-advance)
  (listen-function 'ioblock-listen)
  (eofp-function 'ioblock-eofp)
  (force-output-function 'ioblock-force-output)
  (close-function 'ioblock-close)
  (inbuf-lock nil)
  (eof nil)
  (interactive nil)
  (dirty nil)
  (outbuf-lock nil)
  (owner nil)
  (read-char-function 'ioblock-no-char-input)
  (read-byte-function 'ioblock-no-binary-input)
  (write-byte-function 'ioblock-no-binary-output)
  (write-char-function 'ioblock-no-char-output)
  (encoding nil)
  (pending-byte-order-mark nil)
  (decode-literal-code-unit-limit 256)
  (encode-output-function nil)
  (decode-input-function nil)
  (read-char-when-locked-function 'ioblock-no-char-input)
  (write-simple-string-function 'ioblock-no-char-output)
  (character-read-vector-function 'ioblock-no-char-input)
  (read-line-function 'ioblock-no-char-input)
  (write-char-when-locked-function 'ioblock-no-char-output)
  (read-byte-when-locked-function 'ioblock-no-binary-input)
  (write-byte-when-locked-function 'ioblock-no-binary-output)
  (peek-char-function 'ioblock-no-char-input)
  (native-byte-order t)
  (read-char-without-translation-when-locked-function 'ioblock-no-char-input)
  (write-char-without-translation-when-locked-function 'iblock-no-char-output)
  (sharing nil)
  (line-termination nil)
  (unread-char-function 'ioblock-no-char-input)
  (encode-literal-char-code-limit 256)
  (input-timeout nil)
  (output-timeout nil)
  (deadline nil))


;;; Functions on ioblocks.  So far, we aren't saying anything
;;; about how streams use them.

(defun ioblock-no-binary-input (ioblock &rest otters)
  (declare (ignore otters))
  (report-bad-arg (ioblock-stream ioblock) '(and binary-stream input-stream)))

(defun ioblock-no-binary-output (ioblock &rest others)
  (declare (ignore others))
  (report-bad-arg (ioblock-stream ioblock) '(and binary-stream output-stream)))

(defun ioblock-no-char-input (ioblock &rest others)
  (declare (ignore others))
  (report-bad-arg (ioblock-stream ioblock) '(and character-stream input-stream)))

(defun ioblock-no-char-output (ioblock &rest others)
  (declare (ignore others))
  (report-bad-arg (ioblock-stream ioblock) '(and character-stream output-stream)))


(defun ioblock-octets-to-elements (ioblock octets)
  (let* ((shift (ioblock-element-shift ioblock)))
    (declare (fixnum shift))
    (if (zerop shift)
      octets
      (ash octets (- shift)))))

(defun ioblock-elements-to-octets (ioblock elements)
  (let* ((shift (ioblock-element-shift ioblock)))
    (declare (fixnum shift))
    (if (zerop shift)
      elements
      (ash elements shift))))



;;; ioblock must really be an ioblock or you will crash
;;; Also: the expression "ioblock" is evaluated multiple times.

(declaim (inline check-ioblock-owner))
(defun check-ioblock-owner (ioblock)
  (declare (optimize (speed 3)))
  (let* ((owner (ioblock-owner ioblock)))
    (if owner
      (or (eq owner *current-process*)
          (conditional-store (ioblock-owner ioblock) 0 *current-process*)
          (error "Stream ~s is private to ~s" (ioblock-stream ioblock) owner)))))



(declaim (inline %ioblock-advance))
(defun %ioblock-advance (ioblock read-p)
  (funcall (ioblock-advance-function ioblock)
           (ioblock-stream ioblock)
           ioblock
           read-p))


(defun %ioblock-surrounding-characters (ioblock)
  (let* ((inbuf (ioblock-inbuf ioblock)))
    (when inbuf
      (let* ((encoding (or (ioblock-encoding ioblock)
                           (get-character-encoding nil)))
             (size (ash (character-encoding-code-unit-size encoding) -3))
             (buffer (io-buffer-buffer inbuf))
             (idx (io-buffer-idx inbuf))
             (count (io-buffer-count inbuf)))
        (unless (= count 0)
          (let* ((start (max (- idx (* 10 size)) 0))
                 (end (min (+ idx (* 10 size)) count))
                 (string (make-string (funcall (character-encoding-length-of-vector-encoding-function encoding) buffer start end))))
            (funcall (character-encoding-vector-decode-function encoding)
                     buffer
                     start
                     (- end start)
                     string)
            (if (position #\Replacement_Character string)
              (string-trim (string #\Replacement_Character) string)
              string)))))))
             
        


(defun %bivalent-ioblock-read-u8-byte (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (setf (ioblock-untyi-char ioblock) nil)
  (let* ((buf (ioblock-inbuf ioblock))
         (idx (io-buffer-idx buf))
         (limit (io-buffer-count buf)))
    (declare (fixnum idx limit))
    (when (= idx limit)
      (unless (%ioblock-advance ioblock t)
        (return-from %bivalent-ioblock-read-u8-byte :eof))
      (setq idx (io-buffer-idx buf)
            limit (io-buffer-count buf)))
    (setf (io-buffer-idx buf) (the fixnum (1+ idx)))
    (aref (the (simple-array (unsigned-byte 8) (*))
              (io-buffer-buffer buf)) idx)))


(declaim (inline %ioblock-read-u8-byte))
(defun %ioblock-read-u8-byte (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((buf (ioblock-inbuf ioblock))
         (idx (io-buffer-idx buf))
         (limit (io-buffer-count buf)))
    (declare (fixnum idx limit))
    (when (= idx limit)
      (unless (%ioblock-advance ioblock t)
        (return-from %ioblock-read-u8-byte :eof))
      (setq idx (io-buffer-idx buf)))
    (setf (io-buffer-idx buf) (the fixnum (1+ idx)))
    (aref (the (simple-array (unsigned-byte 8) (*))
            (io-buffer-buffer buf)) idx)))

(declaim (inline %ioblock-read-u8-code-unit))
(defun %ioblock-read-u8-code-unit (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((buf (ioblock-inbuf ioblock))
         (idx (io-buffer-idx buf))
         (limit (io-buffer-count buf)))
    (declare (fixnum idx limit))
    (when (= idx limit)
      (unless (%ioblock-advance ioblock t)
        (return-from %ioblock-read-u8-code-unit :eof))
      (setq idx (io-buffer-idx buf)
            limit (io-buffer-count buf)))
    (setf (io-buffer-idx buf) (the fixnum (1+ idx)))
    (aref (the (simple-array (unsigned-byte 8) (*))
              (io-buffer-buffer buf)) idx)))             

(declaim (inline %ioblock-read-s8-byte))
(defun %ioblock-read-s8-byte (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((buf (ioblock-inbuf ioblock))
         (idx (io-buffer-idx buf))
         (limit (io-buffer-count buf)))
    (declare (fixnum idx limit))
    (when (= idx limit)
      (unless (%ioblock-advance ioblock t)
        (return-from %ioblock-read-s8-byte :eof))
      (setq idx (io-buffer-idx buf)
            limit (io-buffer-count buf)))
    (setf (io-buffer-idx buf) (the fixnum (1+ idx)))
    (aref (the (simple-array (signed-byte 8) (*))
            (io-buffer-buffer buf)) idx)))

(defun %private-ioblock-read-s8-byte (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (check-ioblock-owner ioblock)
  (%ioblock-read-s8-byte ioblock))

(defun %locked-ioblock-read-s8-byte (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (with-ioblock-input-lock-grabbed (ioblock)
    (%ioblock-read-s8-byte ioblock)))


(declaim (inline %ioblock-read-u16-byte))
(defun %ioblock-read-u16-byte (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((buf (ioblock-inbuf ioblock))
         (idx (io-buffer-idx buf))
         (limit (io-buffer-count buf)))
    (declare (fixnum idx limit))
    (when (= idx limit)
      (unless (%ioblock-advance ioblock t)
        (return-from %ioblock-read-u16-byte :eof))
      (setq idx (io-buffer-idx buf)
            limit (io-buffer-count buf)))
    (setf (io-buffer-idx buf) (the fixnum (1+ idx)))
    (aref (the (simple-array (unsigned-byte 16) (*))
            (io-buffer-buffer buf)) idx)))

(defun %private-ioblock-read-u16-byte (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (check-ioblock-owner ioblock)
  (%ioblock-read-u16-byte ioblock))

(defun %locked-ioblock-read-u16-byte (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (with-ioblock-input-lock-grabbed (ioblock)
    (%ioblock-read-u16-byte ioblock)))

(declaim (inline %ioblock-read-s16-byte))
(defun %ioblock-read-s16-byte (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((buf (ioblock-inbuf ioblock))
         (idx (io-buffer-idx buf))
         (limit (io-buffer-count buf)))
    (declare (fixnum idx limit))
    (when (= idx limit)
      (unless (%ioblock-advance ioblock t)
        (return-from %ioblock-read-s16-byte :eof))
      (setq idx (io-buffer-idx buf)
            limit (io-buffer-count buf)))
    (setf (io-buffer-idx buf) (the fixnum (1+ idx)))
    (aref (the (simple-array (signed-byte 16) (*))
            (io-buffer-buffer buf)) idx)))

(defun %private-ioblock-read-s16-byte (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (check-ioblock-owner ioblock)
  (%ioblock-read-s16-byte ioblock))

(defun %locked-ioblock-read-s16-byte (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (with-ioblock-input-lock-grabbed (ioblock)
    (%ioblock-read-s16-byte ioblock)))


(declaim (inline %ioblock-read-u32-byte))
(defun %ioblock-read-u32-byte (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((buf (ioblock-inbuf ioblock))
         (idx (io-buffer-idx buf))
         (limit (io-buffer-count buf)))
    (declare (fixnum idx limit))
    (when (= idx limit)
      (unless (%ioblock-advance ioblock t)
        (return-from %ioblock-read-u32-byte :eof))
      (setq idx (io-buffer-idx buf)
            limit (io-buffer-count buf)))
    (setf (io-buffer-idx buf) (the fixnum (1+ idx)))
    (aref (the (simple-array (unsigned-byte 32) (*))
            (io-buffer-buffer buf)) idx)))

(defun %private-ioblock-read-u32-byte (ioblock)
  (check-ioblock-owner ioblock)
  (%ioblock-read-u32-byte ioblock))

(defun %locked-ioblock-read-u32-byte (ioblock)
  (with-ioblock-input-lock-grabbed (ioblock)
    (%ioblock-read-u32-byte ioblock)))

(declaim (inline %ioblock-read-s32-byte))
(defun %ioblock-read-s32-byte (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((buf (ioblock-inbuf ioblock))
         (idx (io-buffer-idx buf))
         (limit (io-buffer-count buf)))
    (declare (fixnum idx limit))
    (when (= idx limit)
      (unless (%ioblock-advance ioblock t)
        (return-from %ioblock-read-s32-byte :eof))
      (setq idx (io-buffer-idx buf)
            limit (io-buffer-count buf)))
    (setf (io-buffer-idx buf) (the fixnum (1+ idx)))
    (aref (the (simple-array (signed-byte 32) (*))
            (io-buffer-buffer buf)) idx)))

(defun %private-ioblock-read-s32-byte (ioblock)
  (check-ioblock-owner ioblock)
  (%ioblock-read-s32-byte ioblock))

(defun %locked-ioblock-read-s32-byte (ioblock)
  (with-ioblock-input-lock-grabbed (ioblock)
    (%ioblock-read-s32-byte ioblock)))

#+64-bit-target
(progn
(declaim (inline %ioblock-read-u64-byte))
(defun %ioblock-read-u64-byte (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((buf (ioblock-inbuf ioblock))
         (idx (io-buffer-idx buf))
         (limit (io-buffer-count buf)))
    (declare (fixnum idx limit))
    (when (= idx limit)
      (unless (%ioblock-advance ioblock t)
        (return-from %ioblock-read-u64-byte :eof))
      (setq idx (io-buffer-idx buf)
            limit (io-buffer-count buf)))
    (setf (io-buffer-idx buf) (the fixnum (1+ idx)))
    (aref (the (simple-array (unsigned-byte 64) (*))
            (io-buffer-buffer buf)) idx)))

(defun %private-ioblock-read-u64-byte (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (check-ioblock-owner ioblock)
  (%ioblock-read-u64-byte ioblock))

(defun %locked-ioblock-read-u64-byte (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (with-ioblock-input-lock-grabbed (ioblock)
    (%ioblock-read-u64-byte ioblock)))

(defun %ioblock-read-s64-byte (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((buf (ioblock-inbuf ioblock))
         (idx (io-buffer-idx buf))
         (limit (io-buffer-count buf)))
    (declare (fixnum idx limit))
    (when (= idx limit)
      (unless (%ioblock-advance ioblock t)
        (return-from %ioblock-read-s64-byte :eof))
      (setq idx (io-buffer-idx buf)
            limit (io-buffer-count buf)))
    (setf (io-buffer-idx buf) (the fixnum (1+ idx)))
    (aref (the (simple-array (signed-byte 64) (*))
            (io-buffer-buffer buf)) idx)))

(defun %private-ioblock-read-s64-byte (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (check-ioblock-owner ioblock)
  (%ioblock-read-s64-byte ioblock))

(defun %locked-ioblock-read-s64-byte (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (with-ioblock-input-lock-grabbed (ioblock)
    (%ioblock-read-s64-byte ioblock)))
)


;;; Read a 16-bit code element from a stream with element-type
;;; (UNSIGNED-BYTE 8), in native byte-order.

(declaim (inline %ioblock-read-u16-code-unit))
(defun %ioblock-read-u16-code-unit (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((buf (ioblock-inbuf ioblock))
         (idx (io-buffer-idx buf))
         (limit (io-buffer-count buf))
         (vector (io-buffer-buffer buf)))
    (declare (fixnum idx limit)
             (type (simple-array (unsigned-byte 8) (*)) vector))
    (if (<= (the fixnum (+ idx 2)) limit)
      (let* ((b0 (aref vector idx))
             (b1 (aref vector (the fixnum (1+ idx)))))
        (declare (type (unsigned-byte 8) b0 b1))
        (setf (io-buffer-idx buf) (the fixnum (+ idx 2)))
        #+big-endian-target
        (logior (the (unsigned-byte 16) (ash b0 8)) b1)
        #+little-endian-target
        (logior (the (unsigned-byte 16) (ash b1 8)) b0))
      (if (< idx limit)
        (let* ((b0 (aref vector idx))
               (n (%ioblock-advance ioblock t)))
          (declare (type (unsigned-byte 8) b0))
          (if (null n)
            :eof
            (let* ((b1 (aref vector 0)))
              (declare (type (unsigned-byte 8) b1))
              (setf (io-buffer-idx buf) 1)
              #+big-endian-target
              (logior (the (unsigned-byte 16) (ash b0 8)) b1)
              #+little-endian-target
              (logior (the (unsigned-byte 16) (ash b1 8)) b0))))
        (let* ((n (%ioblock-advance ioblock t)))
          (if (null n)
            :eof
            (if (eql n 1)
              (progn
                (setf (io-buffer-idx buf) 1)
                :eof)
              (let* ((b0 (aref vector 0))
                     (b1 (aref vector 1)))
                (declare (type (unsigned-byte 8) b0 b1))
                (setf (io-buffer-idx buf) 2)
                #+big-endian-target
                (logior (the (unsigned-byte 16) (ash b0 8)) b1)
                #+little-endian-target
                (logior (the (unsigned-byte 16) (ash b1 8)) b0)))))))))
  
(declaim (inline %ioblock-read-swapped-u16-code-unit))
(defun %ioblock-read-swapped-u16-code-unit (ioblock)
  (declare (optimize (speed 3) (safety 0)))
    (let* ((buf (ioblock-inbuf ioblock))
         (idx (io-buffer-idx buf))
         (limit (io-buffer-count buf))
         (vector (io-buffer-buffer buf)))
    (declare (fixnum idx limit)
             (type (simple-array (unsigned-byte 8) (*)) vector))
    (if (<= (the fixnum (+ idx 2)) limit)
      (let* ((b0 (aref vector idx))
             (b1 (aref vector (the fixnum (1+ idx)))))
        (declare (type (unsigned-byte 8) b0 b1))
        (setf (io-buffer-idx buf) (the fixnum (+ idx 2)))
        #+little-endian-target
        (logior (the (unsigned-byte 16) (ash b0 8)) b1)
        #+big-endian-target
        (logior (the (unsigned-byte 16) (ash b1 8)) b0))
      (if (< idx limit)
        (let* ((b0 (aref vector idx))
               (n (%ioblock-advance ioblock t)))
          (declare (type (unsigned-byte 8) b0))
          (if (null n)
            :eof
            (let* ((b1 (aref vector 0)))
              (declare (type (unsigned-byte 8) b1))
              (setf (io-buffer-idx buf) 1)
              #+little-endian-target
              (logior (the (unsigned-byte 16) (ash b0 8)) b1)
              #+big-endian-target
              (logior (the (unsigned-byte 16) (ash b1 8)) b0))))
        (let* ((n (%ioblock-advance ioblock t)))
          (if (null n)
            :eof
            (if (eql n 1)
              (progn
                (setf (io-buffer-idx buf) 1)
                :eof)
              (let* ((b0 (aref vector 0))
                     (b1 (aref vector 1)))
                (declare (type (unsigned-byte 8) b0 b1))
                (setf (io-buffer-idx buf) 2)
                #+little-endian-target
                (logior (the (unsigned-byte 16) (ash b0 8)) b1)
                #+big-endian-target
                (logior (the (unsigned-byte 16) (ash b1 8)) b0)))))))))


(declaim (inline %ioblock-read-u32-code-unit))
(defun %ioblock-read-u32-code-unit (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((buf (ioblock-inbuf ioblock))
         (idx (io-buffer-idx buf))
         (limit (io-buffer-count buf))
         (vector (io-buffer-buffer buf)))
    (declare (fixnum idx limit)
             (type (simple-array (unsigned-byte 8) (*)) vector))
    (cond ((<= (the fixnum (+ idx 4)) limit)
           (let* ((b0 (aref vector idx))
                  (b1 (aref vector (the fixnum (1+ idx))))
                  (b2 (aref vector (the fixnum (+ idx 2))))
                  (b3 (aref vector (the fixnum (+ idx 3)))))
             (declare (type (unsigned-byte 8) b0 b1 b2 b3))
             (setf (io-buffer-idx buf) (the fixnum (+ idx 4)))
             #+big-endian-target
             (logior (the (unsigned-byte 32) (ash b0 24))
                     (the (unsigned-byte 24) (ash b1 16))
                     (the (unsigned-byte 16) (ash b2 8))
                     b3)
             #+little-endian-target
             (logior (the (unsigned-byte 32) (ash b3 24))
                     (the (unsigned-byte 24) (ash b2 16))
                     (the (unsigned-byte 16) (ash b1 8))
                     b0)))
          ((= (the fixnum (+ idx 3)) limit)
           (let* ((b0 (aref vector idx))
                  (b1 (aref vector (the fixnum (1+ idx))))
                  (b2 (aref vector (the fixnum (+ idx 2))))
                  (n (%ioblock-advance ioblock t)))
             (declare (type (unsigned-byte 8) b0 b1 b2))
             (if (null n)
               :eof
               (let* ((b3 (aref vector 0)))
                 (declare (type (unsigned-byte 8) b3))
                 (setf (io-buffer-idx buf) 1)
                 #+big-endian-target
                 (logior (the (unsigned-byte 32) (ash b0 24))
                         (the (unsigned-byte 24) (ash b1 16))
                         (the (unsigned-byte 16) (ash b2 8))
                         b3)
                 #+little-endian-target
                 (logior (the (unsigned-byte 32) (ash b3 24))
                         (the (unsigned-byte 24) (ash b2 16))
                         (the (unsigned-byte 16) (ash b1 8))
                         b0)))))
          ((= (the fixnum (+ idx 2)) limit)
           (let* ((b0 (aref vector idx))
                  (b1 (aref vector (the fixnum (1+ idx))))
                  (n (%ioblock-advance ioblock t)))
             (declare (type (unsigned-byte 8) b0 b1))
             (if (null n)
               :eof
               (if (eql n 1)
                 (progn
                   (setf (io-buffer-idx buf) 1)
                   :eof)
                 (let* ((b2 (aref vector 0))
                        (b3 (aref vector 1)))
                   (declare (type (unsigned-byte 8) b2 b3))
                   (setf (io-buffer-idx buf) 2)
                   #+big-endian-target
                   (logior (the (unsigned-byte 32) (ash b0 24))
                           (the (unsigned-byte 24) (ash b1 16))
                           (the (unsigned-byte 16) (ash b2 8))
                           b3)
                   #+little-endian-target
                   (logior (the (unsigned-byte 32) (ash b3 24))
                           (the (unsigned-byte 24) (ash b2 16))
                           (the (unsigned-byte 16) (ash b1 8))
                           b0))))))
          ((= (the fixnum (1+ idx)) limit)
           (let* ((b0 (aref vector idx))
                  (n (%ioblock-advance ioblock t)))
             (declare (type (unsigned-byte 8) b0))
             (if (null n)
               :eof
               (if (< n 3)
                 (progn
                   (setf (io-buffer-idx buf) n)
                   :eof)
                 (let* ((b1 (aref vector 0))
                        (b2 (aref vector 1))
                        (b3 (aref vector 2)))
                   (setf (io-buffer-idx buf) 3)
                   #+big-endian-target
                   (logior (the (unsigned-byte 32) (ash b0 24))
                           (the (unsigned-byte 24) (ash b1 16))
                           (the (unsigned-byte 16) (ash b2 8))
                           b3)
                   #+little-endian-target
                   (logior (the (unsigned-byte 32) (ash b3 24))
                           (the (unsigned-byte 24) (ash b2 16))
                           (the (unsigned-byte 16) (ash b1 8))
                           b0))))))
          (t
           (let* ((n (%ioblock-advance ioblock t)))
             (if (null n)
               :eof
               (if (< n 4)
                 (progn
                   (setf (io-buffer-idx buf) n)
                   :eof)
                 (let* ((b0 (aref vector 0))
                        (b1 (aref vector 1))
                        (b2 (aref vector 2))
                        (b3 (aref vector 3)))
                (declare (type (unsigned-byte 8) b0 b1 b2 b3))
                (setf (io-buffer-idx buf) 4)
                #+big-endian-target
                (logior (the (unsigned-byte 32) (ash b0 24))
                        (the (unsigned-byte 24) (ash b1 16))
                        (the (unsigned-byte 16) (ash b2 8))
                        b3)
                #+little-endian-target
                (logior (the (unsigned-byte 32) (ash b3 24))
                        (the (unsigned-byte 24) (ash b2 16))
                        (the (unsigned-byte 16) (ash b1 8))
                        b0)))))))))

(declaim (inline %ioblock-read-swapped-u32-code-unit))
(defun %ioblock-read-swapped-u32-code-unit (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((buf (ioblock-inbuf ioblock))
         (idx (io-buffer-idx buf))
         (limit (io-buffer-count buf))
         (vector (io-buffer-buffer buf)))
    (declare (fixnum idx limit)
             (type (simple-array (unsigned-byte 8) (*)) vector))
    (cond ((<= (the fixnum (+ idx 4)) limit)
           (let* ((b0 (aref vector idx))
                  (b1 (aref vector (the fixnum (1+ idx))))
                  (b2 (aref vector (the fixnum (+ idx 2))))
                  (b3 (aref vector (the fixnum (+ idx 3)))))
             (declare (type (unsigned-byte 8) b0 b1 b2 b3))
             (setf (io-buffer-idx buf) (the fixnum (+ idx 4)))
             #+little-endian-target
             (logior (the (unsigned-byte 32) (ash b0 24))
                     (the (unsigned-byte 24) (ash b1 16))
                     (the (unsigned-byte 16) (ash b2 8))
                     b3)
             #+big-endian-target
             (logior (the (unsigned-byte 32) (ash b3 24))
                     (the (unsigned-byte 24) (ash b2 16))
                     (the (unsigned-byte 16) (ash b1 8))
                     b0)))
          ((= (the fixnum (+ idx 3)) limit)
           (let* ((b0 (aref vector idx))
                  (b1 (aref vector (the fixnum (1+ idx))))
                  (b2 (aref vector (the fixnum (+ idx 2))))
                  (n (%ioblock-advance ioblock t)))
             (declare (type (unsigned-byte 8) b0 b1 b2))
             (if (null n)
               :eof
               (let* ((b3 (aref vector 0)))
                 (declare (type (unsigned-byte 8) b3))
                 (setf (io-buffer-idx buf) 1)
                 #+little-endian-target
                 (logior (the (unsigned-byte 32) (ash b0 24))
                         (the (unsigned-byte 24) (ash b1 16))
                         (the (unsigned-byte 16) (ash b2 8))
                         b3)
                 #+big-endian-target
                 (logior (the (unsigned-byte 32) (ash b3 24))
                         (the (unsigned-byte 24) (ash b2 16))
                         (the (unsigned-byte 16) (ash b1 8))
                         b0)))))
          ((= (the fixnum (+ idx 2)) limit)
           (let* ((b0 (aref vector idx))
                  (b1 (aref vector (the fixnum (1+ idx))))
                  (n (%ioblock-advance ioblock t)))
             (declare (type (unsigned-byte 8) b0 b1))
             (if (null n)
               :eof
               (if (eql n 1)
                 (progn
                   (setf (io-buffer-idx buf) 1)
                   :eof)
                 (let* ((b2 (aref vector 0))
                        (b3 (aref vector 1)))
                   (declare (type (unsigned-byte 8) b2 b3))
                   (setf (io-buffer-idx buf) 2)
                   #+little-endian-target
                   (logior (the (unsigned-byte 32) (ash b0 24))
                           (the (unsigned-byte 24) (ash b1 16))
                           (the (unsigned-byte 16) (ash b2 8))
                           b3)
                   #+big-endian-target
                   (logior (the (unsigned-byte 32) (ash b3 24))
                           (the (unsigned-byte 24) (ash b2 16))
                           (the (unsigned-byte 16) (ash b1 8))
                           b0))))))
          ((= (the fixnum (1+ idx)) limit)
           (let* ((b0 (aref vector idx))
                  (n (%ioblock-advance ioblock t)))
             (declare (type (unsigned-byte 8) b0))
             (if (null n)
               :eof
               (if (< n 3)
                 (progn
                   (setf (io-buffer-idx buf) n)
                   :eof)
                 (let* ((b1 (aref vector 0))
                        (b2 (aref vector 1))
                        (b3 (aref vector 2)))
                   (setf (io-buffer-idx buf) 3)
                   #+little-endian-target
                   (logior (the (unsigned-byte 32) (ash b0 24))
                           (the (unsigned-byte 24) (ash b1 16))
                           (the (unsigned-byte 16) (ash b2 8))
                           b3)
                   #+big-endian-target
                   (logior (the (unsigned-byte 32) (ash b3 24))
                           (the (unsigned-byte 24) (ash b2 16))
                           (the (unsigned-byte 16) (ash b1 8))
                           b0))))))
          (t
           (let* ((n (%ioblock-advance ioblock t)))
             (if (null n)
               :eof
               (if (< n 4)
                 (progn
                   (setf (io-buffer-idx buf) n)
                   :eof)
                 (let* ((b0 (aref vector 0))
                        (b1 (aref vector 1))
                        (b2 (aref vector 2))
                        (b3 (aref vector 3)))
                (declare (type (unsigned-byte 8) b0 b1 b2 b3))
                (setf (io-buffer-idx buf) 4)
                #+little-endian-target
                (logior (the (unsigned-byte 32) (ash b0 24))
                        (the (unsigned-byte 24) (ash b1 16))
                        (the (unsigned-byte 16) (ash b2 8))
                        b3)
                #+big-endian-target
                (logior (the (unsigned-byte 32) (ash b3 24))
                        (the (unsigned-byte 24) (ash b2 16))
                        (the (unsigned-byte 16) (ash b1 8))
                        b0)))))))))


(defun %bivalent-private-ioblock-read-u8-byte (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (check-ioblock-owner ioblock)
  (setf (ioblock-untyi-char ioblock) nil)
    (let* ((buf (ioblock-inbuf ioblock))
	   (idx (io-buffer-idx buf))
	   (limit (io-buffer-count buf)))
      (declare (fixnum idx limit))
      (when (= idx limit)
	(unless (%ioblock-advance ioblock t)
	  (return-from %bivalent-private-ioblock-read-u8-byte :eof))
	(setq idx (io-buffer-idx buf)
	      limit (io-buffer-count buf)))
      (setf (io-buffer-idx buf) (the fixnum (1+ idx)))
      (aref (the (simple-array (unsigned-byte 8) (*))
              (io-buffer-buffer buf)) idx)))

(defun %private-ioblock-read-u8-byte (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (check-ioblock-owner ioblock)
  (%ioblock-read-u8-byte ioblock))

(defun %bivalent-locked-ioblock-read-u8-byte (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (with-ioblock-input-lock-grabbed (ioblock)
    (setf (ioblock-untyi-char ioblock) nil)
    (let* ((buf (ioblock-inbuf ioblock))
           (idx (io-buffer-idx buf))
           (limit (io-buffer-count buf)))
      (declare (fixnum idx limit))
      (when (= idx limit)
        (unless (%ioblock-advance ioblock t)
          (return-from %bivalent-locked-ioblock-read-u8-byte :eof))
        (setq idx (io-buffer-idx buf)
              limit (io-buffer-count buf)))
      (setf (io-buffer-idx buf) (the fixnum (1+ idx)))
      (aref (the (simple-array (unsigned-byte 8) (*))
              (io-buffer-buffer buf)) idx))))

(defun %locked-ioblock-read-u8-byte (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (with-ioblock-input-lock-grabbed (ioblock)
    (%ioblock-read-u8-byte ioblock)))

(defun %general-ioblock-read-byte (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (with-ioblock-input-locked (ioblock)
    (let* ((buf (ioblock-inbuf ioblock))
           (idx (io-buffer-idx buf))
           (limit (io-buffer-count buf)))
      (declare (fixnum idx limit))
      (when (= idx limit)
        (unless (%ioblock-advance ioblock t)
          (return-from %general-ioblock-read-byte :eof))
        (setq idx (io-buffer-idx buf)
              limit (io-buffer-count buf)))
      (setf (io-buffer-idx buf) (the fixnum (1+ idx)))
      (uvref (io-buffer-buffer buf) idx))))


(declaim (inline %ioblock-tyi))
(defun %ioblock-tyi (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((ch (ioblock-untyi-char ioblock)))
    (if ch
      (prog1 ch
        (setf (ioblock-untyi-char ioblock) nil))
      (let* ((buf (ioblock-inbuf ioblock))
             (idx (io-buffer-idx buf))
             (limit (io-buffer-count buf)))
        (declare (fixnum idx limit))
        (when (= idx limit)
          (unless (%ioblock-advance ioblock t)
            (return-from %ioblock-tyi :eof))
          (setq idx 0))
        (setf (io-buffer-idx buf) (the fixnum (1+ idx)))
        (%code-char (aref (the (simple-array (unsigned-byte 8) (*))
                                       (io-buffer-buffer buf)) idx))))))

(defun %private-ioblock-tyi (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (check-ioblock-owner ioblock)
  (%ioblock-tyi ioblock))

(defun %locked-ioblock-tyi (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (with-ioblock-input-lock-grabbed (ioblock)
    (%ioblock-tyi ioblock)))

;;; Read a character composed of one or more 8-bit code-units.
(declaim (inline %ioblock-read-u8-encoded-char))
(defun %ioblock-read-u8-encoded-char (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((ch (ioblock-untyi-char ioblock)))
    (if ch
      (prog1 ch
        (setf (ioblock-untyi-char ioblock) nil))
      (let* ((1st-unit (%ioblock-read-u8-code-unit ioblock)))
        (if (eq 1st-unit :eof)
          1st-unit
          (locally
              (declare (type (unsigned-byte 8) 1st-unit))
            (if (< 1st-unit
                   (the (mod #x110000) (ioblock-decode-literal-code-unit-limit ioblock)))
              (%code-char 1st-unit)
              (funcall (ioblock-decode-input-function ioblock)
                       1st-unit
                       #'%ioblock-read-u8-code-unit
                       ioblock))))))))

(defun %private-ioblock-read-u8-encoded-char (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (check-ioblock-owner ioblock)
  (%ioblock-read-u8-encoded-char ioblock))

(defun %locked-ioblock-read-u8-encoded-char (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (with-ioblock-input-lock-grabbed (ioblock)
    (%ioblock-read-u8-encoded-char ioblock)))

(declaim (inline %ioblock-read-u16-encoded-char))
(defun %ioblock-read-u16-encoded-char (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((ch (ioblock-untyi-char ioblock)))
    (if ch
      (prog1 ch
        (setf (ioblock-untyi-char ioblock) nil))
      (let* ((1st-unit (%ioblock-read-u16-code-unit ioblock)))
        (if (eq 1st-unit :eof)
          1st-unit
          (locally
              (declare (type (unsigned-byte 16) 1st-unit))
            (if (< 1st-unit
                   (the (mod #x110000) (ioblock-decode-literal-code-unit-limit ioblock)))
              (code-char 1st-unit)
              (funcall (ioblock-decode-input-function ioblock)
                       1st-unit
                       #'%ioblock-read-u16-code-unit
                       ioblock))))))))

(defun %private-ioblock-read-u16-encoded-char (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (check-ioblock-owner ioblock)
  (%ioblock-read-u16-encoded-char ioblock))

(defun %locked-ioblock-read-u16-encoded-char (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (with-ioblock-input-lock-grabbed (ioblock)
    (%ioblock-read-u16-encoded-char ioblock)))

(declaim (inline %ioblock-read-swapped-u16-encoded-char))
(defun %ioblock-read-swapped-u16-encoded-char (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((ch (ioblock-untyi-char ioblock)))
    (if ch
      (prog1 ch
        (setf (ioblock-untyi-char ioblock) nil))
      (let* ((1st-unit (%ioblock-read-swapped-u16-code-unit ioblock)))
        (if (eq 1st-unit :eof)
          1st-unit
          (locally
              (declare (type (unsigned-byte 16) 1st-unit))
            (if (< 1st-unit
                   (the (mod #x110000) (ioblock-decode-literal-code-unit-limit ioblock)))
              (code-char 1st-unit)
              (funcall (ioblock-decode-input-function ioblock)
                       1st-unit
                       #'%ioblock-read-swapped-u16-code-unit
                       ioblock))))))))

(defun %private-ioblock-read-swapped-u16-encoded-char (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (check-ioblock-owner ioblock)
  (%ioblock-read-swapped-u16-encoded-char ioblock))

(defun %locked-ioblock-read-swapped-u16-encoded-char (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (with-ioblock-input-lock-grabbed (ioblock)
    (%ioblock-read-swapped-u16-encoded-char ioblock)))

(declaim (inline %ioblock-read-u32-encoded-char))
(defun %ioblock-read-u32-encoded-char (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((ch (ioblock-untyi-char ioblock)))
    (if ch
      (prog1 ch
        (setf (ioblock-untyi-char ioblock) nil))
      (let* ((1st-unit (%ioblock-read-u32-code-unit ioblock)))
        (if (eq 1st-unit :eof)
          1st-unit
          (locally
              (declare (type (unsigned-byte 16) 1st-unit))
            (if (< 1st-unit
                   (the (mod #x110000) (ioblock-decode-literal-code-unit-limit ioblock)))
              (code-char 1st-unit)
              (funcall (ioblock-decode-input-function ioblock)
                       1st-unit
                       #'%ioblock-read-u32-code-unit
                       ioblock))))))))

(defun %private-ioblock-read-u32-encoded-char (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (check-ioblock-owner ioblock)
  (%ioblock-read-u32-encoded-char ioblock))

(defun %locked-ioblock-read-u32-encoded-char (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (with-ioblock-input-lock-grabbed (ioblock)
    (%ioblock-read-u32-encoded-char ioblock)))

(declaim (inline %ioblock-read-swapped-u32-encoded-char))
(defun %ioblock-read-swapped-u32-encoded-char (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((ch (ioblock-untyi-char ioblock)))
    (if ch
      (prog1 ch
        (setf (ioblock-untyi-char ioblock) nil))
      (let* ((1st-unit (%ioblock-read-swapped-u32-code-unit ioblock)))
        (if (eq 1st-unit :eof)
          1st-unit
          (locally
              (declare (type (unsigned-byte 16) 1st-unit))
            (if (< 1st-unit
                   (the (mod #x110000) (ioblock-decode-literal-code-unit-limit ioblock)))
              (code-char 1st-unit)
              (funcall (ioblock-decode-input-function ioblock)
                       1st-unit
                       #'%ioblock-read-swapped-u32-code-unit
                       ioblock))))))))

(defun %private-ioblock-read-swapped-u32-encoded-char (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (check-ioblock-owner ioblock)
  (%ioblock-read-swapped-u32-encoded-char ioblock))

(defun %locked-ioblock-read-swapped-u32-encoded-char (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (with-ioblock-input-lock-grabbed (ioblock)
    (%ioblock-read-swapped-u32-encoded-char ioblock)))

(declaim (inline %ioblock-tyi-no-hang))
(defun %ioblock-tyi-no-hang (ioblock)
  (declare (optimize (speed 3) (safety 0)))
  (if (ioblock-untyi-char ioblock)
    (prog1 (ioblock-untyi-char ioblock)
      (setf (ioblock-untyi-char ioblock) nil))
    (let* ((buf (ioblock-inbuf ioblock))
	   (idx (io-buffer-idx buf))
	   (limit (io-buffer-count buf)))
      (declare (fixnum idx limit))
      (when (= idx limit)
	(unless (%ioblock-advance ioblock nil)
	  (return-from %ioblock-tyi-no-hang (if (ioblock-eof ioblock) :eof))))
      (funcall (ioblock-read-char-when-locked-function ioblock) ioblock))))

;;; :iso-8859-1 only.
(defun %ioblock-peek-char (ioblock)
  (or (ioblock-untyi-char ioblock)
      (let* ((buf (ioblock-inbuf ioblock))
             (idx (io-buffer-idx buf))
             (limit (io-buffer-count buf)))
        (declare (fixnum idx limit))
        (when (= idx limit)
          (unless (%ioblock-advance ioblock t)
            (return-from %ioblock-peek-char :eof))
          (setq idx (io-buffer-idx buf)
                limit (io-buffer-count buf)))
        (%code-char (aref (the (simple-array (unsigned-byte 8) (*)) (io-buffer-buffer buf)) idx)))))

(defun %encoded-ioblock-peek-char (ioblock)
  (or (ioblock-untyi-char ioblock)
      (let* ((ch (funcall (ioblock-read-char-when-locked-function ioblock) ioblock)))
        (unless (eq ch :eof)
          (setf (ioblock-untyi-char ioblock) ch))
        ch)))




(defun %ioblock-clear-input (ioblock)    
    (let* ((buf (ioblock-inbuf ioblock)))
      (setf (io-buffer-count buf) 0
	    (io-buffer-idx buf) 0
	    (ioblock-untyi-char ioblock) nil)))

(defun %ioblock-untyi (ioblock char)
  (if (ioblock-untyi-char ioblock)
    (error "Two UNREAD-CHARs without intervening READ-CHAR on ~s"
	   (ioblock-stream ioblock))
    (setf (ioblock-untyi-char ioblock) char)))

(declaim (inline ioblock-inpos))

(defun ioblock-inpos (ioblock)
  (io-buffer-idx (ioblock-inbuf ioblock)))

(declaim (inline ioblock-outpos))

(defun ioblock-outpos (ioblock)
  (io-buffer-count (ioblock-outbuf ioblock)))



(declaim (inline %ioblock-force-output))

(defun %ioblock-force-output (ioblock finish-p)
  (funcall (ioblock-force-output-function ioblock)
           (ioblock-stream ioblock)
           ioblock
           (ioblock-outpos ioblock)
           finish-p))

;;; ivector should be an ivector.  The ioblock should have an
;;; element-shift of 0; start-octet and num-octets should of course
;;; be sane.  This is mostly to give the fasdumper a quick way to
;;; write immediate data.
(defun %ioblock-out-ivect (ioblock ivector start-octet num-octets)
  (unless (= 0 (the fixnum (ioblock-element-shift ioblock)))
    (error "Can't write vector to stream ~s" (ioblock-stream ioblock)))
  (let* ((written 0)
	 (out (ioblock-outbuf ioblock)))
    (declare (fixnum written))
    (do* ((pos start-octet (+ pos written))
	  (left num-octets (- left written)))
	 ((= left 0) num-octets)
      (declare (fixnum pos left))
      (setf (ioblock-dirty ioblock) t)
      (let* ((index (io-buffer-idx out))
	     (count (io-buffer-count out))
	     (bufsize (io-buffer-size out))
             (avail (- bufsize index))
             (buffer (io-buffer-buffer out)))
	(declare (fixnum index avail count bufsize))
	(cond
	  ((= (setq written avail) 0)
	   (%ioblock-force-output ioblock nil))
	  (t
	   (if (> written left)
	     (setq written left))
	   (%copy-ivector-to-ivector ivector pos buffer index written)
	   (setf (ioblock-dirty ioblock) t)
	   (incf index written)
	   (if (> index count)
	     (setf (io-buffer-count out) index))
	   (setf (io-buffer-idx out) index)
	   (if (= index  bufsize)
	     (%ioblock-force-output ioblock nil))))))))


(defun %ioblock-unencoded-write-simple-string (ioblock string start-char num-chars)
  (declare (fixnum start-char num-chars) (simple-string string))
  (let* ((written 0)
	 (col (ioblock-charpos ioblock))
	 (out (ioblock-outbuf ioblock)))
    (declare (fixnum written col)
	     (optimize (speed 3) (safety 0)))
    (do* ((pos start-char (+ pos written))
	  (left num-chars (- left written)))
	 ((= left 0) (setf (ioblock-charpos ioblock) col)  num-chars)
      (declare (fixnum pos left))
      (setf (ioblock-dirty ioblock) t)
      (let* ((index (io-buffer-idx out))
	     (count (io-buffer-count out))
             (bufsize (io-buffer-size out))
             (buffer (io-buffer-buffer out))
	     (avail (- bufsize index)))
	(declare (fixnum index bufsize avail count)
                 (type (simple-array (unsigned-byte 8) (*)) buffer))
	(cond
	  ((= (setq written avail) 0)
	   (%ioblock-force-output ioblock nil))
	  (t
	   (if (> written left)
	     (setq written left))
	   (do* ((p pos (1+ p))
		 (i index (1+ i))
		 (j 0 (1+ j)))
		((= j written))
	     (declare (fixnum p i j))
	     (let* ((ch (schar string p))
                    (code (char-code ch)))
               (declare (type (mod #x110000) code))
	       (if (eql ch #\newline)
		 (setq col 0)
		 (incf col))
	       (setf (aref buffer i) (if (>= code 256) (char-code #\Sub) code))))
	   (setf (ioblock-dirty ioblock) t)
	   (incf index written)
	   (if (> index count)
	     (setf (io-buffer-count out) index))
	   (setf (io-buffer-idx out) index)
	   (if (= index  bufsize)
	     (%ioblock-force-output ioblock nil))))))))



(defun %ioblock-eofp (ioblock)
  (let* ((buf (ioblock-inbuf ioblock)))
   (and (eql (io-buffer-idx buf)
             (io-buffer-count buf))
         (locally (declare (optimize (speed 3) (safety 0)))
           (with-ioblock-input-locked (ioblock)
             (funcall (ioblock-eofp-function ioblock)
		      (ioblock-stream ioblock)
		      ioblock))))))

(defun %ioblock-listen (ioblock)
  (let* ((buf (ioblock-inbuf ioblock)))
    (or (< (the fixnum (io-buffer-idx buf))
           (the fixnum (io-buffer-count buf)))
	(funcall (ioblock-listen-function ioblock)
		 (ioblock-stream ioblock)
		 ioblock))))



(declaim (inline %ioblock-write-u8-element))
(defun %ioblock-write-u8-element (ioblock element)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((buf (ioblock-outbuf ioblock))
         (idx (io-buffer-idx buf))
	 (count (io-buffer-count buf))
         (limit (io-buffer-limit buf)))
    (declare (fixnum idx limit count))
    (when (= idx limit)
      (%ioblock-force-output ioblock nil)
      (setq idx (io-buffer-idx buf) count (io-buffer-count buf)))
    (setf (aref (the (simple-array (unsigned-byte 8) (*)) (io-buffer-buffer buf)) idx) element)
    (incf idx)
    (setf (io-buffer-idx buf) idx)
    (when (> idx count)
      (setf (io-buffer-count buf) idx))
    (setf (ioblock-dirty ioblock) t)
    element))

(declaim (inline %ioblock-write-s8-element))
(defun %ioblock-write-s8-element (ioblock element)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((buf (ioblock-outbuf ioblock))
         (idx (io-buffer-idx buf))
	 (count (io-buffer-count buf))
         (limit (io-buffer-limit buf)))
    (declare (fixnum idx limit count))
    (when (= idx limit)
      (%ioblock-force-output ioblock nil)
      (setq idx (io-buffer-idx buf) count (io-buffer-count buf)))
    (setf (aref (the (simple-array (signed-byte 8) (*)) (io-buffer-buffer buf)) idx) element)
    (incf idx)
    (setf (io-buffer-idx buf) idx)
    (when (> idx count)
      (setf (io-buffer-count buf) idx))
    (setf (ioblock-dirty ioblock) t)
    element))

(declaim (inline %ioblock-write-u16-element))
(defun %ioblock-write-u16-element (ioblock element)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((buf (ioblock-outbuf ioblock))
         (idx (io-buffer-idx buf))
	 (count (io-buffer-count buf))
         (limit (io-buffer-limit buf)))
    (declare (fixnum idx limit count))
    (when (= idx limit)
      (%ioblock-force-output ioblock nil)
      (setq idx (io-buffer-idx buf) count (io-buffer-count buf)))
    (setf (aref (the (simple-array (unsigned-byte 16) (*)) (io-buffer-buffer buf)) idx) element)
    (incf idx)
    (setf (io-buffer-idx buf) idx)
    (when (> idx count)
      (setf (io-buffer-count buf) idx))
    (setf (ioblock-dirty ioblock) t)
    element))

(declaim (inline %ioblock-write-u16-code-unit))
(defun %ioblock-write-u16-code-unit (ioblock element)
  (declare (optimize (speed 3) (safety 0))
           (type (unsigned-byte 16) element))
  (let* ((buf (ioblock-outbuf ioblock))
         (idx (io-buffer-idx buf))
	 (count (io-buffer-count buf))
         (limit (io-buffer-limit buf))
         (vector (io-buffer-buffer buf))
         (b0 #+big-endian-target (ldb (byte 8 8) element)
             #+little-endian-target (ldb (byte 8 0) element))
         (b1 #+big-endian-target (ldb (byte 8 0) element)
             #+little-endian-target (ldb (byte 8 8) element)))
    (declare (fixnum idx limit count)
             (type (simple-array (unsigned-byte 8) (*)) vector)
             (type (unsigned-byte 8) b0 b1))
   
    (when (= idx limit)
      (%ioblock-force-output ioblock nil)
      (setq idx (io-buffer-idx buf) count (io-buffer-count buf)))
    (setf (aref vector idx) b0)
    (incf idx)
    (when (= idx limit)
      (when (> idx count)
        (setf (io-buffer-count buf) idx))
      (%ioblock-force-output ioblock nil)
      (setq idx (io-buffer-idx buf) count (io-buffer-count buf)))
    (setf (aref vector idx) b1)
    (incf idx)
    (setf (io-buffer-idx buf) idx)
    (when (> idx count)
      (setf (io-buffer-count buf) idx))
    (setf (ioblock-dirty ioblock) t)
    element))

(declaim (inline %ioblock-write-swapped-u16-code-unit))
(defun %ioblock-write-swapped-u16-code-unit (ioblock element)
  (declare (optimize (speed 3) (safety 0)))
(let* ((buf (ioblock-outbuf ioblock))
         (idx (io-buffer-idx buf))
	 (count (io-buffer-count buf))
         (limit (io-buffer-limit buf))
         (vector (io-buffer-buffer buf))
         (b0 #+big-endian-target (ldb (byte 8 8) element)
             #+little-endian-target (ldb (byte 8 0) element))
         (b1 #+big-endian-target (ldb (byte 8 0) element)
             #+little-endian-target (ldb (byte 8 8) element)))
    (declare (fixnum idx limit count)
             (type (simple-array (unsigned-byte 8) (*)) vector)
             (type (unsigned-byte 8) b0 b1))
   
    (when (= idx limit)
      (%ioblock-force-output ioblock nil)
      (setq idx (io-buffer-idx buf)
            count (io-buffer-count buf)
            vector (io-buffer-buffer buf)
            limit (io-buffer-limit buf)))
    (setf (aref vector idx) b1)
    (incf idx)
    (when (= idx limit)
      (when (> idx count)
        (setf (io-buffer-count buf) idx))
      (%ioblock-force-output ioblock nil)
      (setq idx (io-buffer-idx buf)
            count (io-buffer-count buf)
            vector (io-buffer-buffer buf)
            limit (io-buffer-limit buf)))
    (setf (aref vector idx) b0)
    (incf idx)
    (setf (io-buffer-idx buf) idx)
    (when (> idx count)
      (setf (io-buffer-count buf) idx))
    (setf (ioblock-dirty ioblock) t)
    element))

(declaim (inline %ioblock-write-u32-code-unit))
(defun %ioblock-write-u32-code-unit (ioblock element)
  (declare (optimize (speed 3) (safety 0))
           (type (unsigned-byte 16) element))
  (let* ((buf (ioblock-outbuf ioblock))
         (idx (io-buffer-idx buf))
	 (count (io-buffer-count buf))
         (limit (io-buffer-limit buf))
         (vector (io-buffer-buffer buf))
         (b0 #+big-endian-target (ldb (byte 8 24) element)
             #+little-endian-target (ldb (byte 8 0) element))
         (b1 #+big-endian-target (ldb (byte 8 16) element)
             #+little-endian-target (ldb (byte 8 8) element))
         (b2 #+big-endian-target (ldb (byte 8 8) element)
             #+little-endian-target (ldb (byte 8 16) element))
         (b3 #+big-endian-target (ldb (byte 8 0) element)
             #+little-endian-target (ldb (byte 8 24) element)))
    (declare (fixnum idx limit count)
             (type (simple-array (unsigned-byte 8) (*)) vector)
             (type (unsigned-byte 8) b0 b1 b2 b3))
    (when (= idx limit)
      (%ioblock-force-output ioblock nil)
      (setq idx (io-buffer-idx buf)
            count (io-buffer-count buf)
            vector (io-buffer-buffer buf)
            limit (io-buffer-limit buf)))
    (setf (aref vector idx) b0)
    (incf idx)
    (when (= idx limit)
      (when (> idx count)
        (setf (io-buffer-count buf) idx))
      (%ioblock-force-output ioblock nil)
      (setq idx (io-buffer-idx buf)
            count (io-buffer-count buf)
            vector (io-buffer-buffer buf)
            limit (io-buffer-limit buf)))
    (setf (aref vector idx) b1)
    (incf idx)
    (when (= idx limit)
      (when (> idx count)
        (setf (io-buffer-count buf) idx))
      (%ioblock-force-output ioblock nil)
      (setq idx (io-buffer-idx buf)
            count (io-buffer-count buf)
            vector (io-buffer-buffer buf)
            limit (io-buffer-limit buf)))
    (setf (aref vector idx) b2)
    (incf idx)
    (when (= idx limit)
      (when (> idx count)
        (setf (io-buffer-count buf) idx))
      (%ioblock-force-output ioblock nil)
      (setq idx (io-buffer-idx buf)
            count (io-buffer-count buf)
            vector (io-buffer-buffer buf)
            limit (io-buffer-limit buf)))
    (setf (aref vector idx) b3)
    (incf idx)
    (setf (io-buffer-idx buf) idx)
    (when (> idx count)
      (setf (io-buffer-count buf) idx))
    (setf (ioblock-dirty ioblock) t)
    element))

(declaim (inline %ioblock-write-swapped-u32-code-unit))
(defun %ioblock-write-swapped-u32-code-unit (ioblock element)
  (declare (optimize (speed 3) (safety 0))
           (type (unsigned-byte 16) element))
  (let* ((buf (ioblock-outbuf ioblock))
         (idx (io-buffer-idx buf))
	 (count (io-buffer-count buf))
         (limit (io-buffer-limit buf))
         (vector (io-buffer-buffer buf))
         (b0 #+little-endian-target (ldb (byte 8 24) element)
             #+big-endian-target (ldb (byte 8 0) element))
         (b1 #+little-endian-target (ldb (byte 8 16) element)
             #+big-endian-target (ldb (byte 8 8) element))
         (b2 #+little-endian-target (ldb (byte 8 8) element)
             #+big-endian-target (ldb (byte 8 16) element))
         (b3 #+little-endian-target (ldb (byte 8 0) element)
             #+big-endian-target (ldb (byte 8 24) element)))
    (declare (fixnum idx limit count)
             (type (simple-array (unsigned-byte 8) (*)) vector)
             (type (unsigned-byte 8) b0 b1 b2 b3))
    (when (= idx limit)
      (%ioblock-force-output ioblock nil)
      (setq idx (io-buffer-idx buf)
            count (io-buffer-count buf)
            vector (io-buffer-buffer buf)
            limit (io-buffer-limit buf)))
    (setf (aref vector idx) b0)
    (incf idx)
    (when (= idx limit)
      (when (> idx count)
        (setf (io-buffer-count buf) idx))
      (%ioblock-force-output ioblock nil)
      (setq idx (io-buffer-idx buf) count (io-buffer-count buf)))
    (setf (aref vector idx) b1)
    (incf idx)
    (when (= idx limit)
      (when (> idx count)
        (setf (io-buffer-count buf) idx))
      (%ioblock-force-output ioblock nil)
      (setq idx (io-buffer-idx buf)
            count (io-buffer-count buf)
            vector (io-buffer-buffer buf)
            limit (io-buffer-limit buf)))
    (setf (aref vector idx) b2)
    (incf idx)
    (when (= idx limit)
      (when (> idx count)
        (setf (io-buffer-count buf) idx))
      (%ioblock-force-output ioblock nil)
      (setq idx (io-buffer-idx buf)
            count (io-buffer-count buf)
            vector (io-buffer-buffer buf)
            limit (io-buffer-limit buf)))
    (setf (aref vector idx) b3)
    (incf idx)
    (setf (io-buffer-idx buf) idx)
    (when (> idx count)
      (setf (io-buffer-count buf) idx))
    (setf (ioblock-dirty ioblock) t)
    element))

(declaim (inline %ioblock-write-s16-element))
(defun %ioblock-write-s16-element (ioblock element)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((buf (ioblock-outbuf ioblock))
         (idx (io-buffer-idx buf))
	 (count (io-buffer-count buf))
         (limit (io-buffer-limit buf)))
    (declare (fixnum idx limit count))
    (when (= idx limit)
      (%ioblock-force-output ioblock nil)
      (setq idx (io-buffer-idx buf)
            count (io-buffer-count buf)))
    (setf (aref (the (simple-array (signed-byte 16) (*)) (io-buffer-buffer buf)) idx) element)
    (incf idx)
    (setf (io-buffer-idx buf) idx)
    (when (> idx count)
      (setf (io-buffer-count buf) idx))
    (setf (ioblock-dirty ioblock) t)
    element))

(declaim (inline %ioblock-write-u32-element))
(defun %ioblock-write-u32-element (ioblock element)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((buf (ioblock-outbuf ioblock))
         (idx (io-buffer-idx buf))
	 (count (io-buffer-count buf))
         (limit (io-buffer-limit buf)))
    (declare (fixnum idx limit count))
    (when (= idx limit)
      (%ioblock-force-output ioblock nil)
      (setq idx (io-buffer-idx buf)
            count (io-buffer-count buf)))
    (setf (aref (the (simple-array (unsigned-byte 32) (*)) (io-buffer-buffer buf)) idx) element)
    (incf idx)
    (setf (io-buffer-idx buf) idx)
    (when (> idx count)
      (setf (io-buffer-count buf) idx))
    (setf (ioblock-dirty ioblock) t)
    element))

(declaim (inline %ioblock-write-swapped-u32-element))
(defun %ioblock-write-swapped-u32-element (ioblock element)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((buf (ioblock-outbuf ioblock))
         (idx (io-buffer-idx buf))
	 (count (io-buffer-count buf))
         (limit (io-buffer-limit buf)))
    (declare (fixnum idx limit count))
    (when (= idx limit)
      (%ioblock-force-output ioblock nil)
      (setq idx (io-buffer-idx buf)
            count (io-buffer-count buf)))
    (setf (aref (the (simple-array (unsigned-byte 32) (*)) (io-buffer-buffer buf)) idx)
          (%swap-u32 element))
    (incf idx)
    (setf (io-buffer-idx buf) idx)
    (when (> idx count)
      (setf (io-buffer-count buf) idx))
    (setf (ioblock-dirty ioblock) t)
    element))

(declaim (inline %ioblock-write-s32-element))
(defun %ioblock-write-s32-element (ioblock element)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((buf (ioblock-outbuf ioblock))
         (idx (io-buffer-idx buf))
	 (count (io-buffer-count buf))
         (limit (io-buffer-limit buf)))
    (declare (fixnum idx limit count))
    (when (= idx limit)
      (%ioblock-force-output ioblock nil)
      (setq idx (io-buffer-idx buf) count (io-buffer-count buf)))
    (setf (aref (the (simple-array (signed-byte 32) (*)) (io-buffer-buffer buf)) idx) element)
    (incf idx)
    (setf (io-buffer-idx buf) idx)
    (when (> idx count)
      (setf (io-buffer-count buf) idx))
    (setf (ioblock-dirty ioblock) t)
    element))

#+64-bit-target
(progn
(declaim (inline %ioblock-write-u64-element))
(defun %ioblock-write-u64-element (ioblock element)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((buf (ioblock-outbuf ioblock))
         (idx (io-buffer-idx buf))
	 (count (io-buffer-count buf))
         (limit (io-buffer-limit buf)))
    (declare (fixnum idx limit count))
    (when (= idx limit)
      (%ioblock-force-output ioblock nil)
      (setq idx (io-buffer-idx buf) count (io-buffer-count buf)))
    (setf (aref (the (simple-array (unsigned-byte 64) (*)) (io-buffer-buffer buf)) idx) element)
    (incf idx)
    (setf (io-buffer-idx buf) idx)
    (when (> idx count)
      (setf (io-buffer-count buf) idx))
    (setf (ioblock-dirty ioblock) t)
    element))

(declaim (inline %ioblock-write-s64-element))
(defun %ioblock-write-s64-element (ioblock element)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((buf (ioblock-outbuf ioblock))
         (idx (io-buffer-idx buf))
	 (count (io-buffer-count buf))
         (limit (io-buffer-limit buf)))
    (declare (fixnum idx limit count))
    (when (= idx limit)
      (%ioblock-force-output ioblock nil)
      (setq idx (io-buffer-idx buf) count (io-buffer-count buf)))
    (setf (aref (the (simple-array (signed-byte 64) (*)) (io-buffer-buffer buf)) idx) element)
    (incf idx)
    (setf (io-buffer-idx buf) idx)
    (when (> idx count)
      (setf (io-buffer-count buf) idx))
    (setf (ioblock-dirty ioblock) t)
    element))
)

(declaim (inline %ioblock-write-char))
(defun %ioblock-write-char (ioblock char)
  (declare (optimize (speed 3) (safety 0)))
  (if (eq char #\linefeed)
    (setf (ioblock-charpos ioblock) 0)
    (incf (ioblock-charpos ioblock)))
  (let* ((code (char-code char)))
    (declare (type (mod #x110000) code))
    (if (< code 256)
      (%ioblock-write-u8-element ioblock code)
      (%ioblock-write-u8-element ioblock (char-code #\Sub)))))

(defun %private-ioblock-write-char (ioblock char)
  (declare (optimize (speed 3) (safety 0)))
  (check-ioblock-owner ioblock)
  (%ioblock-write-char ioblock char))

(defun %locked-ioblock-write-char (ioblock char)
  (declare (optimize (speed 3) (safety 0)))
  (with-ioblock-output-lock-grabbed (ioblock)
    (%ioblock-write-char ioblock char)))

(declaim (inline %ioblock-write-u8-encoded-char))
(defun %ioblock-write-u8-encoded-char (ioblock char)
  (declare (optimize (speed 3) (safety 0)))
  (if (eq char #\linefeed)
    (setf (ioblock-charpos ioblock) 0)
    (incf (ioblock-charpos ioblock)))
  (let* ((code (char-code char)))
    (declare (type (mod #x110000) code))
    (if (< code (the fixnum (ioblock-encode-literal-char-code-limit ioblock)))
      (%ioblock-write-u8-element ioblock code)
      (funcall (ioblock-encode-output-function ioblock)
               char
               #'%ioblock-write-u8-element
               ioblock))))

(defun %private-ioblock-write-u8-encoded-char (ioblock char)
  (declare (optimize (speed 3) (safety 0)))
  (check-ioblock-owner ioblock)
  (%ioblock-write-u8-encoded-char ioblock char))

(defun %locked-ioblock-write-u8-encoded-char (ioblock char)
  (declare (optimize (speed 3) (safety 0)))
  (with-ioblock-output-lock-grabbed (ioblock) 
    (%ioblock-write-u8-encoded-char ioblock char)))


(defun %ioblock-write-u8-encoded-simple-string (ioblock string start-char num-chars)
  (declare (fixnum start-char num-chars)
           (simple-base-string string)
           (optimize (speed 3) (safety 0)))
  (do* ((i 0 (1+ i))
        (col (ioblock-charpos ioblock))
        (limit (ioblock-encode-literal-char-code-limit ioblock))
        (encode-function (ioblock-encode-output-function ioblock))
        (start-char start-char (1+ start-char)))
       ((= i num-chars) (setf (ioblock-charpos ioblock) col) num-chars)
    (declare (fixnum i start-char limit))
    (let* ((char (schar string start-char))
           (code (char-code char)))
      (declare (type (mod #x110000) code))
      (if (eq char #\newline)
        (setq col 0)
        (incf col))
      (if (< code limit)
        (%ioblock-write-u8-element ioblock code)
        (funcall encode-function char #'%ioblock-write-u8-element ioblock)))))


(declaim (inline %ioblock-write-u16-encoded-char))
(defun %ioblock-write-u16-encoded-char (ioblock char)
  (declare (optimize (speed 3) (safety 0)))
  (when (ioblock-pending-byte-order-mark ioblock)
    (setf (ioblock-pending-byte-order-mark ioblock) nil)
    (%ioblock-write-u16-code-unit ioblock byte-order-mark-char-code))
  (if (eq char #\linefeed)
    (setf (ioblock-charpos ioblock) 0)
    (incf (ioblock-charpos ioblock)))
  (let* ((code (char-code char)))
    (declare (type (mod #x110000) code))
    (if (< code (the fixnum (ioblock-encode-literal-char-code-limit ioblock)))
      (%ioblock-write-u16-code-unit ioblock code)
      (funcall (ioblock-encode-output-function ioblock)
               char
               #'%ioblock-write-u16-code-unit
               ioblock))))

(defun %private-ioblock-write-u16-encoded-char (ioblock char)
  (declare (optimize (speed 3) (safety 0)))
  (check-ioblock-owner ioblock)
  (%ioblock-write-u16-encoded-char ioblock char))

(defun %locked-ioblock-write-u16-encoded-char (ioblock char)
  (declare (optimize (speed 3) (safety 0)))
  (with-ioblock-output-lock-grabbed (ioblock)
    (%ioblock-write-u16-encoded-char ioblock char)))


(defun %ioblock-write-u16-encoded-simple-string (ioblock string start-char num-chars)
  (declare (fixnum start-char num-chars)
           (simple-base-string string)
           (optimize (speed 3) (safety 0)))
  (when (ioblock-pending-byte-order-mark ioblock)
    (setf (ioblock-pending-byte-order-mark ioblock) nil)
    (%ioblock-write-u16-code-unit ioblock byte-order-mark-char-code))
  (do* ((i 0 (1+ i))
        (col (ioblock-charpos ioblock))
        (limit (ioblock-encode-literal-char-code-limit ioblock))
        (encode-function (ioblock-encode-output-function ioblock))
        (start-char start-char (1+ start-char)))
       ((= i num-chars) (setf (ioblock-charpos ioblock) col) num-chars)
    (declare (fixnum i start-char limit))
    (let* ((char (schar string start-char))
           (code (char-code char)))
      (declare (type (mod #x110000) code))
      (if (eq char #\newline)
        (setq col 0)
        (incf col))
      (if (< code limit)
        (%ioblock-write-u16-code-unit ioblock code)
        (funcall encode-function char #'%ioblock-write-u16-code-unit ioblock)))))

(declaim (inline %ioblock-write-swapped-u16-encoded-char))
(defun %ioblock-write-swapped-u16-encoded-char (ioblock char)
  (declare (optimize (speed 3) (safety 0)))
  (if (eq char #\linefeed)
    (setf (ioblock-charpos ioblock) 0)
    (incf (ioblock-charpos ioblock)))
  (let* ((code (char-code char)))
    (declare (type (mod #x110000) code))
    (if (< code (the fixnum (ioblock-encode-literal-char-code-limit ioblock)))
      (%ioblock-write-swapped-u16-code-unit ioblock code)
      (funcall (ioblock-encode-output-function ioblock)
               char
               #'%ioblock-write-swapped-u16-code-unit
               ioblock))))

(defun %private-ioblock-write-swapped-u16-encoded-char (ioblock char)
  (declare (optimize (speed 3) (safety 0)))
  (check-ioblock-owner ioblock)
  (%ioblock-write-swapped-u16-encoded-char ioblock char))

(defun %locked-ioblock-write-swapped-u16-encoded-char (ioblock char)
  (declare (optimize (speed 3) (safety 0)))
  (with-ioblock-output-lock-grabbed (ioblock)
    (%ioblock-write-swapped-u16-encoded-char ioblock char)))

(defun %ioblock-write-swapped-u16-encoded-simple-string (ioblock string start-char num-chars)
  (declare (fixnum start-char num-chars)
           (simple-base-string string)
           (optimize (speed 3) (safety 0)))
  (do* ((i 0 (1+ i))
        (col (ioblock-charpos ioblock))
        (limit (ioblock-encode-literal-char-code-limit ioblock))
        (encode-function (ioblock-encode-output-function ioblock))
        (wcf (ioblock-write-char-when-locked-function ioblock))
        (start-char start-char (1+ start-char)))
       ((= i num-chars) (setf (ioblock-charpos ioblock) col) num-chars)
    (declare (fixnum i start-char limit))
    (let* ((char (schar string start-char))
           (code (char-code char)))
      (declare (type (mod #x110000) code))
      (cond ((eq char #\newline)
             (setq col 0)
             (funcall wcf ioblock char))
            (t
             (incf col)
             (if (< code limit)
               (%ioblock-write-swapped-u16-code-unit ioblock code)
               (funcall encode-function char #'%ioblock-write-swapped-u16-code-unit ioblock)))))))


(declaim (inline %ioblock-write-u32-encoded-char))
(defun %ioblock-write-u32-encoded-char (ioblock char)
  (declare (optimize (speed 3) (safety 0)))
  (when (ioblock-pending-byte-order-mark ioblock)
    (setf (ioblock-pending-byte-order-mark ioblock) nil)
    (%ioblock-write-u32-code-unit ioblock byte-order-mark))
  (if (eq char #\linefeed)
    (setf (ioblock-charpos ioblock) 0)
    (incf (ioblock-charpos ioblock)))
  (let* ((code (char-code char)))
    (declare (type (mod #x110000) code))
    (if (< code (the fixnum (ioblock-encode-literal-char-code-limit ioblock)))
      (%ioblock-write-u32-code-unit ioblock code)
      (funcall (ioblock-encode-output-function ioblock)
               code
               #'%ioblock-write-u32-code-unit
               ioblock))))

(defun %private-ioblock-write-u32-encoded-char (ioblock char)
  (declare (optimize (speed 3) (safety 0)))
  (check-ioblock-owner ioblock)
  (%ioblock-write-u32-encoded-char ioblock char))

(defun %locked-ioblock-write-u32-encoded-char (ioblock char)
  (declare (optimize (speed 3) (safety 0)))  
  (with-ioblock-output-lock-grabbed (ioblock)
    (%ioblock-write-u32-encoded-char ioblock char)))

(defun %ioblock-write-u32-encoded-simple-string (ioblock string start-char num-chars)
  (declare (fixnum start-char num-chars)
           (simple-base-string string)
           (optimize (speed 3) (safety 0)))
  (when (ioblock-pending-byte-order-mark ioblock)
    (setf (ioblock-pending-byte-order-mark ioblock) nil)
    (%ioblock-write-u32-code-unit ioblock byte-order-mark-char-code))
  (do* ((i 0 (1+ i))
        (col (ioblock-charpos ioblock))
        (limit (ioblock-encode-literal-char-code-limit ioblock))
        (encode-function (ioblock-encode-output-function ioblock))
        (start-char start-char (1+ start-char)))
       ((= i num-chars) (setf (ioblock-charpos ioblock) col) num-chars)
    (declare (fixnum i start-char limit))
    (let* ((char (schar string start-char))
           (code (char-code char)))
      (declare (type (mod #x110000) code))
      (if (eq char #\newline)
        (setq col 0)
        (incf col))
      (if (< code limit)
        (%ioblock-write-u32-code-unit ioblock code)
        (funcall encode-function char #'%ioblock-write-u32-code-unit ioblock)))))


(declaim (inline %ioblock-write-swapped-u32-encoded-char))
(defun %ioblock-write-swapped-u32-encoded-char (ioblock char)
  (declare (optimize (speed 3) (safety 0)))
  (if (eq char #\linefeed)
    (setf (ioblock-charpos ioblock) 0)
    (incf (ioblock-charpos ioblock)))
  (let* ((code (char-code char)))
    (declare (type (mod #x110000) code))
    (if (< code (the fixnum (ioblock-encode-literal-char-code-limit ioblock)))
      (%ioblock-write-swapped-u32-code-unit ioblock code)
      (funcall (ioblock-encode-output-function ioblock)
               code
               #'%ioblock-write-swapped-u32-code-unit
               ioblock))))

(defun %private-ioblock-write-swapped-u32-encoded-char (ioblock char)
  (declare (optimize (speed 3) (safety 0)))
  (check-ioblock-owner ioblock)
  (%ioblock-write-swapped-u32-encoded-char ioblock char))

(defun %locked-ioblock-write-swapped-u32-encoded-char (ioblock char)
  (declare (optimize (speed 3) (safety 0)))  
  (with-ioblock-output-lock-grabbed (ioblock)
    (%ioblock-write-swapped-u32-encoded-char ioblock char)))

(defun %ioblock-write-swapped-u32-encoded-simple-string (ioblock string start-char num-chars)
  (declare (fixnum start-char num-chars)
           (simple-base-string string)
           (optimize (speed 3) (safety 0)))
  (do* ((i 0 (1+ i))
        (col (ioblock-charpos ioblock))
        (limit (ioblock-encode-literal-char-code-limit ioblock))
        (encode-function (ioblock-encode-output-function ioblock))
        (start-char start-char (1+ start-char)))
       ((= i num-chars) (setf (ioblock-charpos ioblock) col) num-chars)
    (declare (fixnum i start-char limit))
    (let* ((char (schar string start-char))
           (code (char-code char)))
      (declare (type (mod #x110000) code))
      (if (eq char #\newline)
        (setq col 0)
        (incf col))
      (if (< code limit)
        (%ioblock-write-swapped-u32-code-unit ioblock code)
        (funcall encode-function char #'%ioblock-write-swapped-u32-code-unit ioblock)))))

(declaim (inline %ioblock-write-u8-byte))
(defun %ioblock-write-u8-byte (ioblock byte)
  (declare (optimize (speed 3) (safety 0)))
  (%ioblock-write-u8-element ioblock (require-type byte '(unsigned-byte 8))))

(defun %private-ioblock-write-u8-byte (ioblock byte)
  (declare (optimize (speed 3) (safety 0)))
  (check-ioblock-owner ioblock)
  (%ioblock-write-u8-byte ioblock byte))

(defun %locked-ioblock-write-u8-byte (ioblock byte)
  (declare (optimize (speed 3) (safety 0)))
  (with-ioblock-output-lock-grabbed (ioblock)
    (%ioblock-write-u8-byte ioblock byte)))

(declaim (inline %ioblock-write-s8-byte))
(defun %ioblock-write-s8-byte (ioblock byte)
  (declare (optimize (speed 3) (safety 0)))
  (%ioblock-write-s8-element ioblock (require-type byte '(signed-byte 8))))

(defun %private-ioblock-write-s8-byte (ioblock byte)
  (declare (optimize (speed 3) (safety 0)))
  (check-ioblock-owner ioblock)
  (%ioblock-write-s8-byte ioblock byte))

(defun %locked-ioblock-write-s8-byte (ioblock byte)
  (declare (optimize (speed 3) (safety 0)))
  (with-ioblock-output-lock-grabbed (ioblock)
    (%ioblock-write-s8-byte ioblock byte)))

(declaim (inline %ioblock-write-u16-byte))
(defun %ioblock-write-u16-byte (ioblock byte)
  (declare (optimize (speed 3) (safety 0)))
  (%ioblock-write-u16-element ioblock (require-type byte '(unsigned-byte 16))))

(defun %private-ioblock-write-u16-byte (ioblock byte)
  (declare (optimize (speed 3) (safety 0)))
  (check-ioblock-owner ioblock)
  (%ioblock-write-u16-byte ioblock byte))

(defun %locked-ioblock-write-u16-byte (ioblock byte)
  (declare (optimize (speed 3) (safety 0)))
  (with-ioblock-output-lock-grabbed (ioblock)
    (%ioblock-write-u16-byte ioblock byte)))

(declaim (inline %ioblock-write-s16-byte))
(defun %ioblock-write-s16-byte (ioblock byte)
  (declare (optimize (speed 3) (safety 0)))
  (%ioblock-write-s16-element ioblock (require-type byte '(signed-byte 16))))

(defun %private-ioblock-write-s16-byte (ioblock byte)
  (declare (optimize (speed 3) (safety 0)))
  (check-ioblock-owner ioblock)
  (%ioblock-write-s16-byte ioblock byte))

(defun %locked-ioblock-write-s16-byte (ioblock byte)
  (declare (optimize (speed 3) (safety 0)))
  (with-ioblock-output-lock-grabbed (ioblock)
    (%ioblock-write-s16-byte ioblock byte)))

(declaim (inline %ioblock-write-u32-byte))
(defun %ioblock-write-u32-byte (ioblock byte)
  (declare (optimize (speed 3) (safety 0)))
  (%ioblock-write-u32-element ioblock (require-type byte '(unsigned-byte 32))))

(defun %private-ioblock-write-u32-byte (ioblock byte)
  (declare (optimize (speed 3) (safety 0)))
  (check-ioblock-owner ioblock)
  (%ioblock-write-u32-byte ioblock byte))

(defun %locked-ioblock-write-u32-byte (ioblock byte)
  (declare (optimize (speed 3) (safety 0)))
  (with-ioblock-output-lock-grabbed (ioblock)
    (%ioblock-write-u32-byte ioblock byte)))

(declaim (inline %ioblock-write-s32-byte))
(defun %ioblock-write-s32-byte (ioblock byte)
  (declare (optimize (speed 3) (safety 0)))
  (%ioblock-write-s32-element ioblock (require-type byte '(signed-byte 32))))

(defun %private-ioblock-write-s32-byte (ioblock byte)
  (declare (optimize (speed 3) (safety 0)))
  (check-ioblock-owner ioblock)
  (%ioblock-write-s32-byte ioblock byte))

(defun %locked-ioblock-write-s32-byte (ioblock byte)
  (declare (optimize (speed 3) (safety 0)))
  (with-ioblock-output-lock-grabbed (ioblock)
    (%ioblock-write-s32-byte ioblock byte)))

#+64-bit-target
(progn
(declaim (inline %ioblock-write-u64-byte))
(defun %ioblock-write-u64-byte (ioblock byte)
  (declare (optimize (speed 3) (safety 0)))
  (%ioblock-write-u64-element ioblock (require-type byte '(unsigned-byte 64))))

(defun %private-ioblock-write-u64-byte (ioblock byte)
  (declare (optimize (speed 3) (safety 0)))
  (check-ioblock-owner ioblock)
  (%ioblock-write-u64-byte ioblock byte))

(defun %locked-ioblock-write-u64-byte (ioblock byte)
  (declare (optimize (speed 3) (safety 0)))
  (with-ioblock-output-lock-grabbed (ioblock)
    (%ioblock-write-u64-byte ioblock byte)))

(declaim (inline %ioblock-write-s64-byte))
(defun %ioblock-write-s64-byte (ioblock byte)
  (declare (optimize (speed 3) (safety 0)))
  (%ioblock-write-s64-element ioblock (require-type byte '(signed-byte 64))))

(defun %private-ioblock-write-s64-byte (ioblock byte)
  (declare (optimize (speed 3) (safety 0)))
  (check-ioblock-owner ioblock)
  (%ioblock-write-s64-byte ioblock byte))

(defun %locked-ioblock-write-s64-byte (ioblock byte)
  (declare (optimize (speed 3) (safety 0)))
  (with-ioblock-output-lock-grabbed (ioblock)
    (%ioblock-write-s64-byte ioblock byte)))
)                                       ;#+64-bit-target

(defun %ioblock-clear-output (ioblock)
  (let* ((buf (ioblock-outbuf ioblock)))                      
    (setf (io-buffer-count buf) 0
            (io-buffer-idx buf) 0)))

(defun %ioblock-unencoded-read-line (ioblock)
  (let* ((inbuf (ioblock-inbuf ioblock)))
    (let* ((string "")
           (len 0)
           (eof nil)
           (buf (io-buffer-buffer inbuf))
           (newline (char-code #\newline)))
      (let* ((ch (ioblock-untyi-char ioblock)))
        (when ch
          (setf (ioblock-untyi-char ioblock) nil)
          (if (eql ch #\newline)
            (return-from %ioblock-unencoded-read-line 
              (values string nil))
            (progn
              (setq string (make-string 1)
                    len 1)
              (setf (schar string 0) ch)))))
      (loop
        (let* ((more 0)
               (idx (io-buffer-idx inbuf))
               (count (io-buffer-count inbuf)))
          (declare (fixnum idx count more))
          (if (= idx count)
            (if eof
              (return (values string t))
              (progn
                (setq eof t)
                (%ioblock-advance ioblock t)))
            (progn
              (setq eof nil)
              (let* ((pos (position newline buf :start idx :end count)))
                (when pos
                  (locally (declare (fixnum pos))
                    (setf (io-buffer-idx inbuf) (the fixnum (1+ pos)))
                    (setq more (- pos idx))
                    (unless (zerop more)
                      (setq string
                            (%extend-vector
                             0 string (the fixnum (+ len more)))))
                    (%copy-u8-to-string
                     buf idx string len more)
                    (return (values string nil))))
                ;; No #\newline in the buffer.  Read everything that's
                ;; there into the string, and fill the buffer again.
                (setf (io-buffer-idx inbuf) count)
                (setq more (- count idx)
                      string (%extend-vector
                              0 string (the fixnum (+ len more))))
                (%copy-u8-to-string
                 buf idx string len more)
                (incf len more)))))))))

;;; There are lots of ways of doing better here, but in the most general
;;; case we can't tell (a) what a newline looks like in the buffer or (b)
;;; whether there's a 1:1 mapping between code units and characters.
(defun %ioblock-encoded-read-line (ioblock)
  (let* ((str (make-array 20 :element-type 'base-char
			  :adjustable t :fill-pointer 0))
         (rcf (ioblock-read-char-when-locked-function ioblock))
	 (eof nil))
    (do* ((ch (funcall rcf ioblock) (funcall rcf ioblock)))
	 ((or (eq ch #\newline) (setq eof (eq ch :eof)))
	  (values (ensure-simple-string str) eof))
      (vector-push-extend ch str))))
	 
(defun %ioblock-unencoded-character-read-vector (ioblock vector start end)
  (do* ((i start)
        (in (ioblock-inbuf ioblock))
        (inbuf (io-buffer-buffer in))
        (need (- end start)))
       ((= i end) end)
    (declare (fixnum i need))
    (let* ((ch (%ioblock-tyi ioblock)))
      (if (eq ch :eof)
        (return i))
      (setf (schar vector i) ch)
      (incf i)
      (decf need)
      (let* ((idx (io-buffer-idx in))
             (count (io-buffer-count in))
             (avail (- count idx)))
        (declare (fixnum idx count avail))
        (unless (zerop avail)
          (if (> avail need)
            (setq avail need))
          (%copy-u8-to-string inbuf idx vector i avail)
          (setf (io-buffer-idx in) (+ idx avail))
          (incf i avail)
          (decf need avail))))))

;;; Also used when newline translation complicates things.
(defun %ioblock-encoded-character-read-vector (ioblock vector start end)
  (declare (fixnum start end))
  (do* ((i start (1+ i))
        (rcf (ioblock-read-char-when-locked-function ioblock)))
       ((= i end) end)
    (declare (fixnum i))
    (let* ((ch (funcall rcf ioblock)))
      (if (eq ch :eof)
	(return i))
      (setf (schar vector i) ch))))


(defun %ioblock-binary-read-vector (ioblock vector start end)
  (declare (fixnum start end))
  (let* ((in (ioblock-inbuf ioblock))
	 (inbuf (io-buffer-buffer in))
         (rbf (ioblock-read-byte-when-locked-function ioblock)))
    (setf (ioblock-untyi-char ioblock) nil)
    (if (not (= (the fixnum (typecode inbuf))
		(the fixnum (typecode vector))))
      (do* ((i start (1+ i)))
	   ((= i end) i)
	(declare (fixnum i))
	(let* ((b (funcall rbf ioblock)))
	  (if (eq b :eof)
	    (return i)
	    (setf (uvref vector i) b))))
      (do* ((i start)
	    (need (- end start)))
	   ((= i end) end)
	(declare (fixnum i need))
	(let* ((b (funcall rbf ioblock)))
	  (if (eq b :eof)
	    (return i))
	  (setf (uvref vector i) b)
	  (incf i)
	  (decf need)
	  (let* ((idx (io-buffer-idx in))
		 (count (io-buffer-count in))
		 (avail (- count idx)))
	    (declare (fixnum idx count avail))
	    (unless (zerop avail)
	      (if (> avail need)
		(setq avail need))
	      (%copy-ivector-to-ivector
	       inbuf
	       (ioblock-elements-to-octets ioblock idx)
	       vector
	       (ioblock-elements-to-octets ioblock i)
	       (ioblock-elements-to-octets ioblock avail))
	      (setf (io-buffer-idx in) (+ idx avail))
	      (incf i avail)
	      (decf need avail))))))))

;;; About the same, only less fussy about ivector's element-type.
;;; (All fussiness is about the stream's element-type ...).
;;; Whatever the element-type is, elements must be 1 octet in size.
(defun %ioblock-character-in-ivect (ioblock vector start nb)
  (declare (type (simple-array (unsigned-byte 8) (*)) vector)
	   (fixnum start nb)
	   (optimize (speed 3) (safety 0)))
  (unless (= 0 (the fixnum (ioblock-element-shift ioblock)))
    (error "Can't read vector from stream ~s" (ioblock-stream ioblock)))
  (do* ((i start)
	(in (ioblock-inbuf ioblock))
	(inbuf (io-buffer-buffer in))
	(need nb)
	(end (+ start nb)))
       ((= i end) end)
    (declare (fixnum i end need))
    (let* ((ch (%ioblock-tyi ioblock)))
      (if (eq ch :eof)
	(return (- i start)))
      (setf (aref vector i) (char-code ch))
      (incf i)
      (decf need)
      (let* ((idx (io-buffer-idx in))
	     (count (io-buffer-count in))
	     (avail (- count idx)))
	(declare (fixnum idx count avail))
	(unless (zerop avail)
	  (if (> avail need)
	    (setq avail need))
          (%copy-u8-to-string inbuf idx vector i avail)
	  (setf (io-buffer-idx in) (+ idx avail))
	  (incf i avail)
	  (decf need avail))))))

(defun %ioblock-binary-in-ivect (ioblock vector start nb)
  (declare (type (simple-array (unsigned-byte 8) (*)) vector)
	   (fixnum start nb)
	   (optimize (speed 3) (safety 0)))
  (unless (= 0 (the fixnum (ioblock-element-shift ioblock)))
    (error "Can't read vector from stream ~s" (ioblock-stream ioblock)))
  (setf (ioblock-untyi-char ioblock) nil)
  (do* ((i start)
        (rbf (ioblock-read-byte-when-locked-function ioblock))
	(in (ioblock-inbuf ioblock))
	(inbuf (io-buffer-buffer in))
	(need nb)
	(end (+ start nb)))
       ((= i end) nb)
    (declare (fixnum i end need))
    (let* ((b (funcall rbf ioblock)))
      (if (eq b :eof)
	(return (- i start)))
      (setf (aref vector i) b)
      (incf i)
      (decf need)
      (let* ((idx (io-buffer-idx in))
	     (count (io-buffer-count in))
	     (avail (- count idx)))
	(declare (fixnum idx count avail))
	(unless (zerop avail)
	  (if (> avail need)
	    (setq avail need))
	  (%copy-ivector-to-ivector inbuf idx vector i avail)
	  (setf (io-buffer-idx in) (+ idx avail))
	  (incf i avail)
	  (decf need avail))))))

;;; Thread must own ioblock lock(s).
(defun %%ioblock-close (ioblock)
  (when (ioblock-device ioblock)
    (let* ((stream (ioblock-stream ioblock)))
      (funcall (ioblock-close-function ioblock) stream ioblock)
      (setf (ioblock-device ioblock) nil)
      (setf (stream-ioblock stream) nil)
      (let* ((in-iobuf (ioblock-inbuf ioblock))
             (out-iobuf (ioblock-outbuf ioblock))
             (in-buffer (if in-iobuf (io-buffer-buffer in-iobuf)))
             (in-bufptr (if in-iobuf (io-buffer-bufptr in-iobuf)))
             (out-buffer (if out-iobuf (io-buffer-buffer out-iobuf)))
             (out-bufptr (if out-iobuf (io-buffer-bufptr out-iobuf))))
        (if (and in-buffer in-bufptr)
          (%dispose-heap-ivector in-buffer))
        (unless (eq in-buffer out-buffer)
          (if (and out-buffer out-bufptr)
            (%dispose-heap-ivector out-buffer)))
        (when in-iobuf
          (setf (io-buffer-buffer in-iobuf) nil
                (io-buffer-bufptr in-iobuf) nil
                (ioblock-inbuf ioblock) nil))
        (when out-iobuf
          (setf (io-buffer-buffer out-iobuf) nil
                (io-buffer-bufptr out-iobuf) nil
                (ioblock-outbuf ioblock) nil))
        t))))

(defun %ioblock-close (ioblock)
  (let* ((in-lock (ioblock-inbuf-lock ioblock))
         (out-lock (ioblock-outbuf-lock ioblock)))
    (if in-lock
      (with-lock-grabbed (in-lock)
        (if (and out-lock (not (eq out-lock in-lock)))
          (with-lock-grabbed (out-lock)
            (%%ioblock-close ioblock))
          (%%ioblock-close ioblock)))
      (if out-lock
        (with-lock-grabbed (out-lock)
          (%%ioblock-close ioblock))
        (progn
          (check-ioblock-owner ioblock)
          (%%ioblock-close ioblock))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Character-at-a-time line-termination-translation functions.
;;; It's not always possible to just blast through the buffer, blindly
;;; replacing #xd with #xa (for example), and it's not always desirable
;;; to do that (if we support changing encoding on open streams.)
;;; This is done at a fairly high level; some cases could be done at
;;; a lower level, and some cases are hard even at that lower level.
;;; This approach doesn't slow down the simple case (when no line-termination
;;; translation is used), and hopefully isn't -that- bad.

(declaim (inline %ioblock-read-char-translating-cr-to-newline))
(defun %ioblock-read-char-translating-cr-to-newline (ioblock)
  (let* ((ch (funcall
              (ioblock-read-char-without-translation-when-locked-function
               ioblock)
              ioblock)))
    (if (eql ch #\Return)
      #\Newline
      ch)))

(defun %private-ioblock-read-char-translating-cr-to-newline (ioblock)
  (check-ioblock-owner ioblock)
  (%ioblock-read-char-translating-cr-to-newline ioblock))

(defun %locked-ioblock-read-char-translating-cr-to-newline (ioblock)
  (with-ioblock-input-lock-grabbed (ioblock)
    (%ioblock-read-char-translating-cr-to-newline ioblock)))

(declaim (inline %ioblock-read-char-translating-crlf-to-newline))
(defun %ioblock-read-char-translating-crlf-to-newline (ioblock)
  (let* ((ch (funcall
              (ioblock-read-char-without-translation-when-locked-function
               ioblock)
              ioblock)))
    (if (eql ch #\Return)
      (let* ((next (funcall
                    (ioblock-read-char-without-translation-when-locked-function
                     ioblock)
                    ioblock)))
        (if (eql next #\Linefeed)
          next
          (progn
            (unless (eq next :eof)
              (setf (ioblock-untyi-char ioblock) next))
            ch)))
      ch)))
    
(defun %private-ioblock-read-char-translating-crlf-to-newline (ioblock)
  (check-ioblock-owner ioblock)
  (%ioblock-read-char-translating-crlf-to-newline ioblock))

(defun %locked-ioblock-read-char-translating-crlf-to-newline (ioblock)
  (with-ioblock-input-lock-grabbed (ioblock)
    (%ioblock-read-char-translating-crlf-to-newline ioblock)))

(declaim (inline %ioblock-read-char-translating-line-separator-to-newline))
(defun %ioblock-read-char-translating-line-separator-to-newline (ioblock)
  (let* ((ch (funcall
              (ioblock-read-char-without-translation-when-locked-function
               ioblock)
              ioblock)))
    (if (eql ch #\Line_Separator)
      #\Newline
      ch)))

(defun %private-ioblock-read-char-translating-line-separator-to-newline (ioblock)
  (check-ioblock-owner ioblock)
  (%ioblock-read-char-translating-line-separator-to-newline ioblock))

(defun %locked-ioblock-read-char-translating-line-separator-to-newline (ioblock)
  (with-ioblock-input-lock-grabbed (ioblock)
    (%ioblock-read-char-translating-line-separator-to-newline ioblock)))

(declaim (inline %ioblock-write-char-translating-newline-to-cr))
(defun %ioblock-write-char-translating-newline-to-cr (ioblock char)
  (funcall (ioblock-write-char-without-translation-when-locked-function
            ioblock)
           ioblock
           (if (eql char #\Newline) #\Return char)))

(defun %private-ioblock-write-char-translating-newline-to-cr (ioblock char)
  (check-ioblock-owner ioblock)
  (%ioblock-write-char-translating-newline-to-cr ioblock char))

(defun %locked-ioblock-write-char-translating-newline-to-cr (ioblock char)
  (with-ioblock-output-lock-grabbed (ioblock)
    (%ioblock-write-char-translating-newline-to-cr ioblock char)))

(declaim (inline %ioblock-write-char-translating-newline-to-crlf))
(defun %ioblock-write-char-translating-newline-to-crlf (ioblock char)
  (when (eql char #\Newline)
    (funcall (ioblock-write-char-without-translation-when-locked-function
              ioblock)
             ioblock
             #\Return))    
  (funcall (ioblock-write-char-without-translation-when-locked-function
            ioblock)
           ioblock
           char))

(defun %private-ioblock-write-char-translating-newline-to-crlf (ioblock char)
  (check-ioblock-owner ioblock)
  (%ioblock-write-char-translating-newline-to-crlf ioblock char))

(defun %locked-ioblock-write-char-translating-newline-to-crlf (ioblock char)
  (with-ioblock-output-lock-grabbed (ioblock)
    (%ioblock-write-char-translating-newline-to-crlf ioblock char)))

(declaim (inline %ioblock-write-char-translating-newline-to-line-separator))
(defun %ioblock-write-char-translating-newline-to-line-separator (ioblock char)
  (funcall (ioblock-write-char-without-translation-when-locked-function
            ioblock)
           ioblock
           (if (eql char #\Newline) #\Line_Separator char)))

(defun %private-ioblock-write-char-translating-newline-to-line-separator (ioblock char)
  (check-ioblock-owner ioblock)
  (%ioblock-write-char-translating-newline-to-line-separator ioblock char))

(defun %locked-ioblock-write-char-translating-newline-to-line-separator (ioblock char)
  (with-ioblock-output-lock-grabbed (ioblock)
    (%ioblock-write-char-translating-newline-to-line-separator ioblock char)))

;;; If we do newline translation, we probably can't be too clever about reading/writing
;;; strings.
(defun %ioblock-write-simple-string-with-newline-translation (ioblock string start-pos num-chars)
  (declare (fixnum start-pos num-chars) (simple-string string))
  (let* ((col (ioblock-charpos ioblock))
         (wcf (ioblock-write-char-when-locked-function ioblock)))
    (declare (fixnum col))
    (do* ((i start-pos (1+ i))
          (n 0 (1+ n)))
         ((= n num-chars) (setf (ioblock-charpos ioblock) col) num-chars)
      (let* ((char (schar string i)))
        (if (eql char #\Newline)
          (setq col 0)
          (incf col))
        (funcall wcf ioblock char)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun setup-ioblock-input (ioblock character-p element-type sharing encoding line-termination)
  (setf (ioblock-sharing ioblock) sharing)
  (when character-p
    (setf (ioblock-unread-char-function ioblock) (select-stream-untyi-function (ioblock-stream ioblock) :input))
    (setf (ioblock-decode-literal-code-unit-limit ioblock)
          (if encoding
            (character-encoding-decode-literal-code-unit-limit encoding)
            256))    
    (if encoding
      (let* ((unit-size (character-encoding-code-unit-size encoding)))
        (setf (ioblock-peek-char-function ioblock) '%encoded-ioblock-peek-char)
        (setf (ioblock-read-line-function ioblock)
              '%ioblock-encoded-read-line)
        (setf (ioblock-character-read-vector-function ioblock)
              '%ioblock-encoded-character-read-vector)        
        (setf (ioblock-decode-input-function ioblock)
              (character-encoding-stream-decode-function encoding))
        (setf (ioblock-read-char-function ioblock)
              (ecase unit-size
                (8
                 (setf (ioblock-read-char-when-locked-function ioblock)
                       '%ioblock-read-u8-encoded-char)
                 (case sharing
                   (:private '%private-ioblock-read-u8-encoded-char)
                   (:lock '%locked-ioblock-read-u8-encoded-char)
                   (t '%ioblock-read-u8-encoded-char)))
                (16
                 (if (character-encoding-native-endianness encoding)
                   (progn
                    (setf (ioblock-read-char-when-locked-function ioblock)
                          '%ioblock-read-u16-encoded-char)
                    (case sharing
                      (:private '%private-ioblock-read-u16-encoded-char)
                      (:lock '%locked-ioblock-read-u16-encoded-char)
                      (t '%ioblock-read-u16-encoded-char)))
                   (progn
                     (setf (ioblock-read-char-when-locked-function ioblock)
                           '%ioblock-read-swapped-u16-encoded-char)
                    (case sharing
                      (:private '%private-ioblock-read-swapped-u16-encoded-char)
                      (:lock '%locked-ioblock-read-swapped-u16-encoded-char)
                      (t '%ioblock-read-swapped-u16-encoded-char)))))
                (32
                 (if (character-encoding-native-endianness encoding)
                   (progn
                    (setf (ioblock-read-char-when-locked-function ioblock)
                          #'%ioblock-read-u32-encoded-char)
                    (case sharing
                      (:private #'%private-ioblock-read-u32-encoded-char)
                      (:lock #'%locked-ioblock-read-u32-encoded-char)
                      (t #'%ioblock-read-u32-encoded-char)))
                   (progn
                     (setf (ioblock-read-char-when-locked-function ioblock)
                           #'%ioblock-read-swapped-u32-encoded-char)
                    (case sharing
                      (:private '#'%private-ioblock-read-swapped-u16-encoded-char)
                      (:lock #'%locked-ioblock-read-swapped-u32-encoded-char)
                      (t #'%ioblock-read-swapped-u32-encoded-char))))))))
      (progn
        (setf (ioblock-peek-char-function ioblock) '%ioblock-peek-char)
        (setf (ioblock-read-char-function ioblock)
              (case sharing
                (:private '%private-ioblock-tyi)
                (:lock '%locked-ioblock-tyi)
                (t '%ioblock-tyi)))
        (setf (ioblock-read-char-when-locked-function ioblock)
              '%ioblock-tyi)
        (setf (ioblock-character-read-vector-function ioblock)
              '%ioblock-unencoded-character-read-vector)
        (setf (ioblock-read-line-function ioblock)
              '%ioblock-unencoded-read-line)))
    (when line-termination
      (install-ioblock-input-line-termination ioblock line-termination))
    )

  (unless (or (eq element-type 'character)
              (subtypep element-type 'character))
    (let* ((subtag (element-type-subtype element-type)))
      (declare (type (unsigned-byte 8) subtag))
      (setf (ioblock-read-byte-function ioblock)
            (cond ((= subtag target::subtag-u8-vector)
                   (if character-p
                     ;; The bivalent case, at least for now
                     (progn
                       (setf (ioblock-read-byte-when-locked-function ioblock)
                             '%bivalent-ioblock-read-u8-byte)
                       (case sharing
                         (:private '%bivalent-private-ioblock-read-u8-byte)
                         (:lock '%bivalent-locked-ioblock-read-u8-byte)
                         (t '%bivalent-ioblock-read-u8-byte)))
                     (progn
                       (setf (ioblock-read-byte-when-locked-function ioblock)
                             '%ioblock-read-u8-byte)
                       (case sharing
                         (:private '%private-ioblock-read-u8-byte)
                         (:lock '%locked-ioblock-read-u8-byte)
                         (t '%ioblock-read-u8-byte)))))
                  ((= subtag target::subtag-s8-vector)
                   (setf (ioblock-read-byte-when-locked-function ioblock)
                         '%ioblock-read-s8-byte) 
                   (case sharing
                     (:private '%private-ioblock-read-s8-byte)
                     (:lock '%locked-ioblock-read-s8-byte)
                     (t '%ioblock-read-s8-byte)))
                  ((= subtag target::subtag-u16-vector)
                   (setf (ioblock-read-byte-when-locked-function ioblock)
                         '%ioblock-read-u16-byte)
                   (case sharing
                     (:private '%private-ioblock-read-u16-byte)
                     (:lock '%locked-ioblock-read-u16-byte)
                     (t '%ioblock-read-u16-byte)))
                  ((= subtag target::subtag-s16-vector)
                   (setf (ioblock-read-byte-when-locked-function ioblock)
                         '%ioblock-read-s16-byte)
                   (case sharing
                     (:private '%private-ioblock-read-s16-byte)
                     (:lock '%locked-ioblock-read-s16-byte)
                     (t '%ioblock-read-s16-byte)))
                  ((= subtag target::subtag-u32-vector)
                   (setf (ioblock-read-byte-when-locked-function ioblock)
                         '%ioblock-read-u32-byte)
                   (case sharing
                     (:private '%private-ioblock-read-u32-byte)
                     (:lock '%locked-ioblock-read-u32-byte)
                     (t '%ioblock-read-u32-byte)))
                  ((= subtag target::subtag-s32-vector)
                   (setf (ioblock-read-byte-when-locked-function ioblock)
                         '%ioblock-read-s32-byte)                   
                   (case sharing
                     (:private '%private-ioblock-read-s32-byte)
                     (:lock '%locked-ioblock-read-s32-byte)
                     (t '%ioblock-read-s32-byte)))
                  #+64-bit-target
                  ((= subtag target::subtag-u64-vector)
                   (setf (ioblock-read-byte-when-locked-function ioblock)
                         '%ioblock-read-u64-byte)                   
                   (case sharing
                     (:private '%private-ioblock-read-u64-byte)
                     (:lock '%locked-ioblock-read-u64-byte)
                     (t '%ioblock-read-u64-byte)))
                  #+64-bit-target
                  ((= subtag target::subtag-s64-vector)
                   (setf (ioblock-read-byte-when-locked-function ioblock)
                         '%ioblock-read-s64-byte)
                   (case sharing
                     (:private '%private-ioblock-read-s64-byte)
                     (:lock '%locked-ioblock-read-s64-byte)
                     (t '%ioblock-read-s64-byte)))
                  ;; Not sure what this means, currently.
                  (t
                   (setf (ioblock-read-byte-when-locked-function ioblock)
                         '%general-ioblock-read-byte)
                   '%general-ioblock-read-byte))))))

(defun install-ioblock-input-line-termination (ioblock line-termination)
  (when line-termination
    (let* ((sharing (ioblock-sharing ioblock)))
      (setf (ioblock-read-char-without-translation-when-locked-function ioblock)
            (ioblock-read-char-when-locked-function ioblock)
            (ioblock-character-read-vector-function ioblock)
            '%ioblock-encoded-character-read-vector
            (ioblock-read-line-function ioblock) '%ioblock-encoded-read-line)
      (ecase line-termination
        (:cr (setf (ioblock-read-char-when-locked-function ioblock)
                   '%ioblock-read-char-translating-cr-to-newline
                   (ioblock-read-char-function ioblock)
                   (case sharing
                     (:private
                      '%private-ioblock-read-char-translating-cr-to-newline)
                     (:lock
                      '%locked-ioblock-read-char-translating-cr-to-newline)
                     (t '%ioblock-read-char-translating-cr-to-newline))))
        (:crlf (setf (ioblock-read-char-when-locked-function ioblock)
                     '%ioblock-read-char-translating-crlf-to-newline
                     (ioblock-read-char-function ioblock)
                     (case sharing
                       (:private
                        '%private-ioblock-read-char-translating-crlf-to-newline)
                       (:lock
                        '%locked-ioblock-read-char-translating-crlf-to-newline)
                       (t '%ioblock-read-char-translating-crlf-to-newline))))
        (:unicode (setf (ioblock-read-char-when-locked-function ioblock)
                        '%ioblock-read-char-translating-line-separator-to-newline
                        (ioblock-read-char-function ioblock)
                        (case sharing
                          (:private
                           '%private-ioblock-read-char-translating-line-separator-to-newline)
                          (:lock
                           '%locked-ioblock-read-char-translating-line-separator-to-newline)
                          (t '%ioblock-read-char-translating-line-separator-to-newline)))))
      (setf (ioblock-line-termination ioblock) line-termination))))
  
(defun setup-ioblock-output (ioblock character-p element-type sharing encoding line-termination)
  (or (ioblock-sharing ioblock)
      (setf (ioblock-sharing ioblock) sharing))
  (when character-p
    (setf (ioblock-encode-literal-char-code-limit ioblock)
          (if encoding
            (character-encoding-encode-literal-char-code-limit encoding)
            256))    
    (if encoding
      (let* ((unit-size (character-encoding-code-unit-size encoding)))
        (setf (ioblock-encode-output-function ioblock)
              (character-encoding-stream-encode-function encoding))
        (setf (ioblock-write-char-function ioblock)
              (ecase unit-size
                (8
                 (setf (ioblock-write-char-when-locked-function ioblock)
                       '%ioblock-write-u8-encoded-char) 
                 (case sharing
                   (:private '%private-ioblock-write-u8-encoded-char)
                   (:lock '%locked-ioblock-write-u8-encoded-char)
                   (t '%ioblock-write-u8-encoded-char)))
                (16
                 (if (character-encoding-native-endianness encoding)
                   (progn
                     (setf (ioblock-write-char-when-locked-function ioblock)
                           '%ioblock-write-u16-encoded-char) 
                     (case sharing
                       (:private '%private-ioblock-write-u16-encoded-char)
                       (:lock '%locked-ioblock-write-u16-encoded-char)
                       (t '%ioblock-write-u16-encoded-char)))
                   (progn
                     (setf (ioblock-write-char-when-locked-function ioblock)
                           '%ioblock-write-swapped-u16-encoded-char)
                     (case sharing
                       (:private '%private-ioblock-write-swapped-u16-encoded-char)
                       (:lock '%locked-ioblock-write-swapped-u16-encoded-char)
                       (t '%ioblock-write-swapped-u16-encoded-char)))))
                (32
                 (if (character-encoding-native-endianness encoding)
                   (progn
                     (setf (ioblock-write-char-when-locked-function ioblock)
                           #'%ioblock-write-u32-encoded-char) 
                     (case sharing
                       (:private #'%private-ioblock-write-u32-encoded-char)
                       (:lock #'%locked-ioblock-write-u32-encoded-char)
                       (t #'%ioblock-write-u32-encoded-char)))
                   (progn
                     (setf (ioblock-write-char-when-locked-function ioblock)
                           #'%ioblock-write-swapped-u32-encoded-char)
                     (case sharing
                       (:private #'%private-ioblock-write-swapped-u32-encoded-char)
                       (:lock #'%locked-ioblock-write-swapped-u32-encoded-char)
                       (t #'%ioblock-write-swapped-u32-encoded-char)))))))
        (setf (ioblock-write-simple-string-function ioblock)
              (ecase unit-size
                (8 '%ioblock-write-u8-encoded-simple-string)
                (16
                 (if (character-encoding-native-endianness encoding)
                   '%ioblock-write-u16-encoded-simple-string
                   '%ioblock-write-swapped-u16-encoded-simple-string))
                (32
                 (if (character-encoding-native-endianness encoding)
                   #'%ioblock-write-u32-encoded-simple-string
                   #'%ioblock-write-swapped-u32-encoded-simple-string))))
        (when (character-encoding-use-byte-order-mark encoding)
          (setf (ioblock-pending-byte-order-mark ioblock) t)))
      (progn
        (setf (ioblock-write-simple-string-function ioblock)
              '%ioblock-unencoded-write-simple-string)
        (setf (ioblock-write-char-when-locked-function ioblock)
              '%ioblock-write-char)
        (setf (ioblock-write-char-function ioblock)
              (case sharing
                (:private '%private-ioblock-write-char)
                (:lock '%locked-ioblock-write-char)
                (t '%ioblock-write-char)))))
    (when line-termination
      (install-ioblock-output-line-termination ioblock line-termination)))
  (unless (or (eq element-type 'character)
              (subtypep element-type 'character))
    (let* ((subtag (element-type-subtype element-type)))
      (declare (type (unsigned-byte 8) subtag))
      (setf (ioblock-write-byte-function ioblock)
            (cond ((= subtag target::subtag-u8-vector)
                   (progn
                     (setf (ioblock-write-byte-when-locked-function ioblock)
                           '%ioblock-write-u8-byte)
                     (case sharing
                       (:private '%private-ioblock-write-u8-byte)
                       (:lock '%locked-ioblock-write-u8-byte)
                       (t '%ioblock-write-u8-byte))))
                  ((= subtag target::subtag-s8-vector)
                   (setf (ioblock-write-byte-when-locked-function ioblock)
                         '%ioblock-write-s8-byte)                   
                   (case sharing
                     (:private '%private-ioblock-write-s8-byte)
                     (:lock '%locked-ioblock-write-s8-byte)
                     (t '%ioblock-write-s8-byte)))
                  ((= subtag target::subtag-u16-vector)
                   (setf (ioblock-write-byte-when-locked-function ioblock)
                         '%ioblock-write-u16-byte)                   
                   (case sharing
                     (:private '%private-ioblock-write-u16-byte)
                     (:lock '%locked-ioblock-write-u16-byte)
                     (t '%ioblock-write-u16-byte)))
                  ((= subtag target::subtag-s16-vector)
                   (setf (ioblock-write-byte-when-locked-function ioblock)
                         '%ioblock-write-s16-byte)                                      
                   (case sharing
                     (:private '%private-ioblock-write-s16-byte)
                     (:lock '%locked-ioblock-write-s16-byte)
                     (t '%ioblock-write-s16-byte)))
                  ((= subtag target::subtag-u32-vector)
                   (setf (ioblock-write-byte-when-locked-function ioblock)
                         '%ioblock-write-u32-byte)                                      
                   (case sharing
                     (:private '%private-ioblock-write-u32-byte)
                     (:lock '%locked-ioblock-write-u32-byte)
                     (t '%ioblock-write-u32-byte)))
                  ((= subtag target::subtag-s32-vector)
                   (setf (ioblock-write-byte-when-locked-function ioblock)
                         '%ioblock-write-s32-byte)
                   (case sharing
                     (:private '%private-ioblock-write-s32-byte)
                     (:lock '%locked-ioblock-write-s32-byte)
                     (t '%ioblock-write-s32-byte)))
                  #+64-bit-target
                  ((= subtag target::subtag-u64-vector)
                   (setf (ioblock-write-byte-when-locked-function ioblock)
                         '%ioblock-write-u64-byte)
                   (case sharing
                     (:private '%private-ioblock-write-u64-byte)
                     (:lock '%locked-ioblock-write-u64-byte)
                     (t '%ioblock-write-u64-byte)))
                  #+64-bit-target
                  ((= subtag target::subtag-s64-vector)
                   (setf (ioblock-write-byte-when-locked-function ioblock)
                         '%ioblock-write-u64-byte)
                   (case sharing
                     (:private '%private-ioblock-write-s64-byte)
                     (:lock '%locked-ioblock-write-s64-byte)
                     (t '%ioblock-write-s64-byte)))
                  (t
                   (setf (ioblock-write-byte-when-locked-function ioblock)
                         '%general-ioblock-write-byte)                   
                   '%general-ioblock-write-byte))))))

(defun install-ioblock-output-line-termination (ioblock line-termination)
  (let* ((sharing (ioblock-sharing ioblock)))
        (when line-termination
      (setf (ioblock-write-char-without-translation-when-locked-function ioblock)
            (ioblock-write-char-when-locked-function ioblock)
            (ioblock-write-simple-string-function ioblock)
            '%ioblock-write-simple-string-with-newline-translation)
      (ecase line-termination
        (:cr (setf (ioblock-write-char-when-locked-function ioblock)
                   '%ioblock-write-char-translating-newline-to-cr
                   (ioblock-read-char-function ioblock)
                   (case sharing
                     (:private
                      '%private-ioblock-write-char-translating-newline-to-cr)
                     (:lock
                      '%locked-ioblock-write-char-translating-newline-to-cr)
                     (t '%ioblock-write-char-translating-newline-to-cr))))
        (:crlf (setf (ioblock-write-char-when-locked-function ioblock)
                     '%ioblock-write-char-translating-newline-to-crlf
                     (ioblock-write-char-function ioblock)
                     (case sharing
                       (:private
                        '%private-ioblock-write-char-translating-newline-to-crlf)
                       (:lock
                        '%locked-ioblock-write-char-translating-newline-to-crlf)
                       (t '%ioblock-write-char-translating-newline-to-crlf))))
        (:unicode (setf (ioblock-write-char-when-locked-function ioblock)
                        '%ioblock-write-char-translating-newline-to-line-separator
                        (ioblock-write-char-function ioblock)
                        (case sharing
                          (:private
                           '%private-ioblock-write-char-translating-newline-to-line-separator)
                          (:lock
                           '%locked-ioblock-write-char-translating-newline-to-line-separator)
                          (t '%ioblock-write-char-translating-newline-to-line-separator)))))
      (setf (ioblock-line-termination ioblock) line-termination))))


(defun ensure-reasonable-element-type (element-type)
  (let* ((upgraded (upgraded-array-element-type element-type)))
    (if (eq upgraded 'bit)
      '(unsigned-byte 8)
      (if (eq upgraded 'fixnum)
        #+64-bit-target '(signed-byte 64) #+32-bit-target '(signed-byte 32)
        (if (eq upgraded t)
          (error "Stream element-type ~s can't be reasonably supported." element-type)
          upgraded)))))

(defun init-stream-ioblock (stream
                            &key
                            insize      ; integer to allocate inbuf here, nil
                                        ; otherwise
                            outsize     ; integer to allocate outbuf here, nil
                                        ; otherwise
                            share-buffers-p ; true if input and output
                                        ; share a buffer
                            element-type
                            device
                            advance-function
                            listen-function
                            eofp-function
                            force-output-function
                            close-function
                            element-shift
                            interactive
                            (sharing :private)
                            character-p
                            encoding
                            line-termination
                            input-timeout
                            output-timeout
                            deadline
                            &allow-other-keys)
  (declare (ignorable element-shift))
  (setq line-termination (cdr (assoc line-termination *canonical-line-termination-conventions*)))
  (when encoding
    (unless (typep encoding 'character-encoding)
      (setq encoding (get-character-encoding encoding)))
    (if (eq encoding (get-character-encoding nil))
      (setq encoding nil)))
  (when sharing
    (unless (or (eq sharing :private)
                (eq sharing :lock))
      (if (eq sharing :external)
        (setq sharing nil)
        (report-bad-arg sharing '(member nil :private :lock :external)))))
  (let* ((ioblock (or (let* ((ioblock (stream-ioblock stream nil)))
                        (when ioblock
                          (setf (ioblock-stream ioblock) stream)
                          ioblock))
                      (stream-create-ioblock stream))))
    (when (eq sharing :private)
      (setf (ioblock-owner ioblock) 0))
    (setf (ioblock-encoding ioblock) encoding)
    (when insize
      (unless (ioblock-inbuf ioblock)
        (multiple-value-bind (buffer ptr in-size-in-octets)
            (make-heap-ivector insize
                               (if character-p
                                 '(unsigned-byte 8)
                                 (setq element-type
                                       (ensure-reasonable-element-type element-type))))
          (setf (ioblock-inbuf ioblock)
                (make-io-buffer :buffer buffer
                                :bufptr ptr
                                :size in-size-in-octets
                                :limit insize))
          (when (eq sharing :lock)
            (setf (ioblock-inbuf-lock ioblock) (make-lock)))
          (setf (ioblock-line-termination ioblock) line-termination)

          (setf (ioblock-element-shift ioblock)
                (let* ((octets-per-element (/ in-size-in-octets insize)))
                  (case octets-per-element
                    (1 0)
                    (2 1)
                    (4 2)
                    (8 3)
                    (t (max 0 (ceiling (log octets-per-element 2)))))))
          )))
    (when (ioblock-inbuf ioblock)
      (setup-ioblock-input ioblock character-p element-type sharing encoding line-termination))      
    (if share-buffers-p
      (if insize
        (progn (setf (ioblock-outbuf ioblock)
                     (ioblock-inbuf ioblock))
               (setf (ioblock-outbuf-lock ioblock)
                     (ioblock-inbuf-lock ioblock)))
        (error "Can't share buffers unless insize is non-zero and non-null"))
      (when outsize
        (unless (ioblock-outbuf ioblock)
          (multiple-value-bind (buffer ptr out-size-in-octets)
              (make-heap-ivector outsize
                                 (if character-p
                                   '(unsigned-byte 8)
                                   (setq element-type (ensure-reasonable-element-type element-type))))
            (setf (ioblock-outbuf ioblock)
                  (make-io-buffer :buffer buffer
                                  :bufptr ptr
                                  :count 0
                                  :limit outsize
                                  :size out-size-in-octets))
            (when (eq sharing :lock)
              (setf (ioblock-outbuf-lock ioblock) (make-lock)))
            (setf (ioblock-element-shift ioblock)
                  (let* ((octets-per-element (/ out-size-in-octets outsize)))
                    (case octets-per-element
                      (1 0)
                      (2 1)
                      (4 2)
                      (8 3)
                      (t 
                       (max 0 (ceiling (log octets-per-element 2)))))))
            ))))
    (when (ioblock-outbuf ioblock)
      (setup-ioblock-output ioblock character-p element-type sharing encoding line-termination))
    (when element-type
      (setf (ioblock-element-type ioblock) (if character-p 'character element-type)))
;    (when element-shift
;      (setf (ioblock-element-shift ioblock) element-shift))
    (when device
      (setf (ioblock-device ioblock) device))
    (when advance-function
      (setf (ioblock-advance-function ioblock) advance-function))
    (when listen-function
      (setf (ioblock-listen-function ioblock) listen-function))
    (when eofp-function
      (setf (ioblock-eofp-function ioblock) eofp-function))
    (when force-output-function
      (setf (ioblock-force-output-function ioblock) force-output-function))
    (when close-function
      (setf (ioblock-close-function ioblock) close-function))
    (when interactive
      (setf (ioblock-interactive ioblock) interactive))
    (setf (stream-ioblock stream) ioblock)
    (when encoding
      (setf (ioblock-native-byte-order ioblock)
            (character-encoding-native-endianness encoding)))
    (let* ((bom-info (and insize encoding (character-encoding-use-byte-order-mark encoding))))
      (when bom-info
        (ioblock-check-input-bom ioblock bom-info sharing)))
    (setf (ioblock-input-timeout ioblock) input-timeout)
    (setf (ioblock-output-timeout ioblock) output-timeout)
    (setf (ioblock-deadline ioblock) deadline)
    ioblock))

;;; If there's a byte-order-mark (or a reversed byte-order-mark) at
;;; the beginning of the input stream, deal with it.  If there's any
;;; input present, make sure that we don't write a BOM on output.  If
;;; this is a little-endian machine, input data was present, and there
;;; was no BOM in that data, make things big-endian.  If there's a
;;; leading BOM or swapped BOM, eat it (consume it so that it doesn't
;;; ordinarily appear as input.)
;;;
(defun ioblock-check-input-bom (ioblock swapped-encoding-name sharing)
  (let* ((n (%ioblock-advance ioblock nil))) ; try to read, don't block
    (when n
      (setf (ioblock-pending-byte-order-mark ioblock) nil)
      (let* ((inbuf (ioblock-inbuf ioblock))
             (unit-size (character-encoding-code-unit-size (ioblock-encoding ioblock)))
             (min (ash unit-size -3))
             (buf (io-buffer-buffer inbuf))
             (swapped-encoding
              (and
               (>= n min)
               (case (case unit-size
                       (16 (%native-u8-ref-u16 buf 0))
                       (32 (%native-u8-ref-u32 buf 0)))
                 (#.byte-order-mark-char-code
                  (setf (io-buffer-idx inbuf) min)
                  nil)
                 (#.swapped-byte-order-mark-char-code
                  (setf (io-buffer-idx inbuf) min)
                  t)
                 (t #+little-endian-target t))
               (lookup-character-encoding swapped-encoding-name))))
        (when swapped-encoding
          (let* ((output-p (not (null (ioblock-outbuf ioblock)))))
            (setf (ioblock-native-byte-order ioblock)
                  (character-encoding-native-endianness swapped-encoding))
            (ecase unit-size
              (16
               (setf (ioblock-read-char-when-locked-function ioblock)
                     '%ioblock-read-swapped-u16-encoded-char)
               (case sharing
                 (:private '%private-ioblock-read-swapped-u16-encoded-char)
                 (:lock '%locked-ioblock-read-swapped-u16-encoded-char)
                 (t '%ioblock-read-swapped-u16-encoded-char)))
              (32
               (setf (ioblock-read-char-when-locked-function ioblock)
                     '%ioblock-read-swapped-u32-encoded-char)
               (case sharing
                 (:private '%private-ioblock-read-swapped-u32-encoded-char)
                 (:lock '%locked-ioblock-read-swapped-u32-encoded-char)
                 (t '%ioblock-read-swapped-u16-encoded-char))))
            (when output-p
              (ecase unit-size
                (16
                 (setf (ioblock-write-char-when-locked-function ioblock)
                       '%ioblock-write-swapped-u16-encoded-char)
                 (case sharing
                   (:private '%private-ioblock-write-swapped-u16-encoded-char)
                   (:lock '%locked-ioblock-write-swapped-u16-encoded-char)
                   (t '%ioblock-write-swapped-u16-encoded-char))
                 (setf (ioblock-write-simple-string-function ioblock)
                       '%ioblock-write-swapped-u16-encoded-simple-string))
                (32
                 (setf (ioblock-write-char-when-locked-function ioblock)
                       '%ioblock-write-swapped-u32-encoded-char)
                 (case sharing
                   (:private '%private-ioblock-write-swapped-u32-encoded-char)
                   (:lock '%locked-ioblock-write-swapped-u32-encoded-char)
                   (t '%ioblock-write-swapped-u32-encoded-char))
                 (setf (ioblock-write-simple-string-function ioblock)
                       '%ioblock-write-swapped-u32-encoded-simple-string))))))))))



;;; We can't define a MAKE-INSTANCE method on STRUCTURE-CLASS subclasses
;;; in MCL; of course, calling the structure-class's constructor does
;;; much the same thing (but note that MCL only keeps track of the
;;; default, automatically generated constructor.)
;;; (As fascinating as that may be, that has nothing to do with any
;;; nearby code, though it may have once been relevant.)
(defun make-ioblock-stream (class
			    &rest initargs
			    &key 
			    &allow-other-keys)
  (declare (dynamic-extent initargs))
  (let* ((s
          (if (subtypep class 'basic-stream)
            (apply #'make-basic-stream-instance class :allow-other-keys t initargs)
            (apply #'make-instance class :allow-other-keys t initargs))))
    (apply #'init-stream-ioblock s initargs)
    s))





(defmethod select-stream-class ((s symbol) in-p out-p char-p)
  (select-stream-class (class-prototype (find-class s)) in-p out-p char-p))

(defmethod select-stream-class ((s structure-class) in-p out-p char-p)
  (select-stream-class (class-prototype s) in-p out-p char-p))

(defmethod select-stream-class ((s standard-class) in-p out-p char-p)
  (select-stream-class (class-prototype s) in-p out-p char-p))


(defparameter *canonical-line-termination-conventions*
  '((:unix . nil)
    (:macos . :cr)
    (:cr . :cr)
    (:crlf . :crlf)
    (:cp/m . :crlf)
    (:msdos . :crlf)
    (:dos . :crlf)
    (:windows . :crlf)
    (:inferred . nil)
    (:unicode . :unicode)))

(defun optimal-buffer-size (fd element-type)
  #+windows-target (declare (ignore fd))
  (flet ((scale-buffer-size (octets)
	   (case (subtag-bytes (element-type-subtype element-type) 1)
	     (1 octets)
	     (2 (ash octets -1))
	     (4 (ash octets -2))
	     (8 (ash octets -3)))))
    #+windows-target
    (let ((octets #$BUFSIZ))
      (scale-buffer-size octets))
    #-windows-target
    (let* ((nominal (or (nth-value 6 (%fstat fd)) *elements-per-buffer*))
	   (octets (case (%unix-fd-kind fd)
		     (:pipe (#_fpathconf fd #$_PC_PIPE_BUF))
		     (:socket
		      #+linux-target nominal
		      #-linux-target
		      (int-getsockopt fd #$SOL_SOCKET
				      #+solaris-target #$SO_SNDBUF
				      #-solaris-target #$SO_SNDLOWAT))
		     ((:character-special :tty)
		      (#_fpathconf fd #$_PC_MAX_INPUT))
		     (t nominal))))
      (when (<= octets 0) (setq octets nominal))
      (scale-buffer-size octets))))

(defun milliseconds-until-deadline (deadline ioblock)
  (let* ((now (get-internal-real-time)))
    (if (> now deadline)
      (error 'communication-deadline-expired :stream (ioblock-stream ioblock))
      (values (round (- deadline now) (/ internal-time-units-per-second 1000))))))


;;; Note that we can get "bivalent" streams by specifiying :character-p t
;;; with a reasonable element-type (e.g. (UNSIGNED-BYTE 8))
(defun make-fd-stream (fd &key
			  (direction :input)
			  (interactive t)
			  (element-type 'character)
			  (class 'fd-stream)
                          (sharing :private)
                          (character-p (or (eq element-type 'character)
                                           (subtypep element-type 'character)))
                          (basic nil)
                          encoding
                          line-termination
                          auto-close
                          input-timeout
                          output-timeout
                          deadline)
  (let* ((elements-per-buffer (optimal-buffer-size fd element-type)))
    (when line-termination
      (setq line-termination
            (cdr (assoc line-termination *canonical-line-termination-conventions*))))
    (when basic
      (setq class (map-to-basic-stream-class-name class))
      (setq basic (subtypep (find-class class) 'basic-stream)))
    (let* ((in-p (member direction '(:io :input)))
           (out-p (member direction '(:io :output)))
           (class-name (select-stream-class class in-p out-p character-p))
           (class (find-class class-name))
           (stream
            (make-ioblock-stream class
                                 :insize (if in-p elements-per-buffer)
                                 :outsize (if out-p elements-per-buffer)
                                 :device fd
                                 :interactive interactive
                                 :element-type element-type
                                 :advance-function (if in-p
                                                     (select-stream-advance-function class direction))
                                 :listen-function (if in-p 'fd-stream-listen)
                                 :eofp-function (if in-p 'fd-stream-eofp)
                                 :force-output-function (if out-p
                                                          (select-stream-force-output-function class direction))
                                 :close-function 'fd-stream-close
                                 :sharing sharing
                                 :character-p character-p
                                 :encoding encoding
                                 :line-termination line-termination
                                 :input-timeout input-timeout
                                 :output-timeout output-timeout
                                 :deadline deadline)))
      (if auto-close
        (terminate-when-unreachable stream
                                    (lambda (stream)
                                      (close-for-termination stream t))))
      stream)))

  
;;;  Fundamental streams.

(defclass fundamental-stream (stream)
    ())

(defclass fundamental-input-stream (fundamental-stream input-stream)
    ((shared-resource :initform nil :accessor input-stream-shared-resource)))

(defclass fundamental-output-stream (fundamental-stream output-stream)
    ())

(defmethod input-stream-p ((x t))
  (report-bad-arg x 'stream))
			   
(defmethod input-stream-p ((s input-stream))
  t)

(defmethod output-stream-p ((x t))
  (report-bad-arg x 'stream))

(defmethod output-stream-p ((s input-stream))
  (typep s 'output-stream))

(defmethod output-stream-p ((s output-stream))
  t)

(defmethod input-stream-p ((s output-stream))
  (typep s 'input-stream))

(defclass binary-stream (stream)
    ())

(defclass character-stream (stream)
    ())

(defmethod stream-external-format ((s character-stream))
  (make-external-format :character-encoding #+big-endian-target :utf-32be #+little-endian-target :utf-32le :line-termination :unix))


(defmethod (setf stream-external-format) (new (s character-stream))
  (check-type new external-format)
  (stream-external-format s))


(defclass fundamental-character-stream (fundamental-stream character-stream)
    ())

(defmethod stream-element-type ((s fundamental-character-stream))
  'character)

(defclass fundamental-binary-stream (fundamental-stream binary-stream)
    ())

(defclass character-input-stream (input-stream character-stream)
    ())

(defclass fundamental-character-input-stream (fundamental-input-stream
                                              fundamental-character-stream
                                              character-input-stream)
    ())

(defmethod stream-read-char-no-hang ((s fundamental-character-input-stream))
  (stream-read-char s))

(defmethod stream-peek-char ((s fundamental-character-input-stream))
  (let* ((ch (stream-read-char s)))
    (unless (eq ch :eof)
      (stream-unread-char s ch))
    ch))

(defmethod stream-listen ((s fundamental-character-input-stream))
  (let* ((ch (stream-read-char-no-hang s)))
    (when (and ch (not (eq ch :eof)))
      (stream-unread-char s ch))
    ch))

(defmethod stream-clear-input ((s fundamental-character-input-stream))
  )

(defmethod stream-read-line ((s character-input-stream))
  (generic-read-line s))

(defclass character-output-stream (output-stream character-stream)
    ())

(defclass fundamental-character-output-stream (fundamental-output-stream
                                               fundamental-character-stream
                                               character-output-stream)
    ())

(defclass binary-input-stream (input-stream binary-stream)
    ())

(defclass fundamental-binary-input-stream (fundamental-input-stream
                                           fundamental-binary-stream
                                           binary-input-stream)
    ())

(defclass binary-output-stream (output-stream binary-stream)
    ())

(defclass fundamental-binary-output-stream (fundamental-output-stream
                                            fundamental-binary-stream
                                            binary-output-stream)
    ())



(defmethod stream-read-byte ((s t))
  (report-bad-arg s '(and input-stream binary-stream)))

(defmethod stream-write-byte ((s t) b)
  (declare (ignore b))
  (report-bad-arg s '(and output-stream binary-stream)))

(defmethod stream-length ((s stream) &optional new)
  (declare (ignore new)))

(defmethod stream-start-line-p ((s character-output-stream))
  (eql 0 (stream-line-column s)))

(defmethod stream-terpri ((s character-output-stream))
  (stream-write-char s #\Newline))

(defmethod stream-fresh-line ((s character-output-stream))
  (unless (stream-start-line-p s)
    (stream-terpri s)
    t))

;;; The bad news is that this doesn't even bother to do the obvious
;;; (calling STREAM-WRITE-STRING with a longish string of spaces.)
;;; The good news is that this method is pretty useless to (format "~T" ...)
;;; anyhow.
(defmethod stream-advance-to-column ((s fundamental-character-output-stream)
				     col)
  (generic-advance-to-column s col))

(defmethod stream-write-string ((stream fundamental-character-output-stream) string &optional (start 0) end)
  (generic-stream-write-string stream string start end))


;;; The read-/write-vector methods could be specialized for stream classes
;;; that expose the underlying buffering mechanism.
;;; They can assume that the 'vector' argument is a simple one-dimensional
;;; array and that the 'start' and 'end' arguments are sane.

(defmethod stream-write-vector ((stream character-output-stream)
				vector start end)
  (declare (fixnum start end))
  (do* ((i start (1+ i)))
       ((= i end))
    (declare (fixnum i))
    (write-char (uvref vector i) stream)))

(defmethod stream-write-vector ((stream binary-output-stream)
				vector start end)
  (declare (fixnum start end))
  (do* ((i start (1+ i)))
       ((= i end))
    (declare (fixnum i))
    (write-byte (uvref vector i) stream)))

(defmethod stream-read-vector ((stream character-input-stream)
			       vector start end)
  (generic-character-read-vector stream vector start end))


(defmethod stream-read-vector ((stream binary-input-stream)
			       vector start end)
  (declare (fixnum start end))
  (do* ((i start (1+ i)))
       ((= i end) end)
    (declare (fixnum i))
    (let* ((b (read-byte stream nil :eof)))
      (if (eq b :eof)
	(return i)
	(setf (uvref vector i) b)))))




;;; File streams, in the abstract.

(defclass file-stream (stream)
    ())

(defmethod stream-domain ((s file-stream))
  :file)



;;; "Basic" (non-extensible) streams.


(declaim (inline basic-stream-p))

(defun basic-stream-p (x)
  (= (the fixnum (typecode x)) target::subtag-basic-stream))

(setf (type-predicate 'basic-stream) 'basic-stream-p)

(make-built-in-class 'basic-stream 'stream)
(make-built-in-class 'basic-file-stream 'basic-stream 'file-stream)
(make-built-in-class 'basic-character-stream 'basic-stream 'character-stream)
(make-built-in-class 'basic-binary-stream 'basic-stream 'binary-stream)

(make-built-in-class 'basic-input-stream 'basic-stream 'input-stream)
(make-built-in-class 'basic-output-stream 'basic-stream 'output-stream)
(make-built-in-class 'basic-io-stream 'basic-input-stream 'basic-output-stream)
(make-built-in-class 'basic-character-input-stream 'basic-input-stream 'basic-character-stream 'character-input-stream)
(make-built-in-class 'basic-character-output-stream 'basic-output-stream 'basic-character-stream 'character-output-stream)
(make-built-in-class 'basic-character-io-stream 'basic-character-input-stream 'basic-character-output-stream)
(make-built-in-class 'basic-binary-input-stream 'basic-input-stream 'basic-binary-stream 'binary-input-stream)
(make-built-in-class 'basic-binary-output-stream 'basic-output-stream 'basic-binary-stream 'binary-output-stream)
(make-built-in-class 'basic-binary-io-stream 'basic-binary-input-stream 'basic-binary-output-stream)


(defun %ioblock-external-format (ioblock)
  (let* ((encoding (or (ioblock-encoding ioblock)
                       (get-character-encoding nil)))
         (line-termination (or (ioblock-line-termination ioblock)
                               :unix)))
    (make-external-format :character-encoding (character-encoding-name encoding)
                          :line-termination line-termination)))

(defmethod input-stream-shared-resource ((s basic-input-stream))
  (getf (basic-stream.info s) :shared-resource))

(defmethod (setf input-stream-shared-resource) (new (s basic-input-stream))
  (setf (getf (basic-stream.info s) :shared-resource) new))

(defmethod print-object ((s basic-stream) out)
  (print-unreadable-object (s out :type t :identity t)
    (let* ((ioblock (basic-stream.state s))
           (fd (and ioblock (ioblock-device ioblock)))
           (encoding (and ioblock (encoding-name (ioblock-encoding ioblock)))))
      (if fd
        (format out "~a (~a/~d)" encoding (%unix-fd-kind fd) fd)
        (format out "~s" :closed)))))

(defmethod select-stream-class ((s (eql 'basic-stream)) in-p out-p char-p)
  (if char-p
    (if in-p
      (if out-p
        'basic-character-io-stream
        'basic-character-input-stream)
      'basic-character-output-stream)
    (if in-p
      (if out-p
        'basic-binary-io-stream
        'basic-binary-input-stream)
      'basic-binary-output-stream)))


(defmethod map-to-basic-stream-class-name (name)
  name)

(defmethod map-to-basic-stream-class-name ((name (eql 'fd-stream)))
  'basic-stream)

(defun allocate-basic-stream (class)
  (if (subtypep class 'basic-file-stream)
    (gvector :basic-stream (%class-own-wrapper class) 0 nil nil nil nil nil)
    (gvector :basic-stream (%class-own-wrapper class) 0 nil nil)))


(defmethod initialize-basic-stream ((s basic-stream) &key &allow-other-keys)
  )
  
(defmethod initialize-basic-stream :after  ((s basic-input-stream) &key &allow-other-keys)
  (setf (basic-stream.flags s)
        (logior (ash 1 basic-stream-flag.open-input) (basic-stream.flags s))))

(defmethod initialize-basic-stream :after ((s basic-output-stream) &key &allow-other-keys)
  (setf (basic-stream.flags s)
        (logior (ash 1 basic-stream-flag.open-output) (basic-stream.flags s))))

(defmethod initialize-basic-stream :after ((s basic-binary-stream) &key &allow-other-keys)
  (setf (basic-stream.flags s)
        (logior (ash 1 basic-stream-flag.open-binary) (basic-stream.flags s))))

(defmethod initialize-basic-stream :after ((s basic-character-stream) &key &allow-other-keys)
  (setf (basic-stream.flags s)
        (logior (ash 1 basic-stream-flag.open-character) (basic-stream.flags s))))

(defun make-basic-stream-instance (class &rest initargs)
  (let* ((s (allocate-basic-stream class)))
    (apply #'initialize-basic-stream s initargs)
    s))



(defmethod (setf stream-ioblock) (ioblock (s basic-stream))
  (setf (basic-stream.state s) ioblock))

(defmethod stream-create-ioblock ((stream basic-stream) &rest args &key)
  (declare (dynamic-extent args))
  (apply #'make-ioblock :stream stream args))


(defmethod stream-write-list ((stream fundamental-character-output-stream)
			      list count)
  (declare (fixnum count))
  (dotimes (i count)
    (stream-write-char stream (pop list))))

(defmethod stream-write-list ((stream basic-character-output-stream)
			      list count)
  (declare (fixnum count))
  (dotimes (i count)
    (stream-write-char stream (pop list))))

(defmethod stream-read-list ((stream character-input-stream)
			     list count)
  (generic-character-read-list stream list count))


(defmethod stream-write-list ((stream fundamental-binary-output-stream)
			      list count)
  (declare (fixnum count))
  (dotimes (i count)
    (let* ((element (pop list)))
      (if (typep element 'character)
        (write-char element stream)
        (write-byte element stream)))))

(defmethod stream-write-list ((stream basic-binary-output-stream)
			      list count)
  (declare (fixnum count))
  (dotimes (i count)
    (let* ((element (pop list)))
      (if (typep element 'character)
        (write-char element stream)
        (write-byte element stream)))))

(defmethod stream-read-list ((stream binary-input-stream)
			     list count)
  (declare (fixnum count))
  (do* ((tail list (cdr tail))
	(i 0 (1+ i)))
       ((= i count) count)
    (declare (fixnum i))
    (let* ((b (read-byte stream nil :eof)))
      (if (eq b :eof)
	(return i)
	(rplaca tail b)))))



(defun stream-is-closed (s)
  (error "~s is closed" s))

(defmethod stream-read-char ((s basic-character-input-stream))
  (let* ((ioblock (basic-stream-ioblock s)))
    (funcall (ioblock-read-char-function ioblock) ioblock)))


(defmethod stream-read-char-no-hang ((stream basic-character-input-stream))
  (let* ((ioblock (basic-stream-ioblock stream)))
    (with-ioblock-input-locked (ioblock)
      (values
          (%ioblock-tyi-no-hang ioblock)))))
       
(defmethod stream-peek-char ((stream basic-character-input-stream))
  (let* ((ioblock (basic-stream-ioblock stream)))
    (with-ioblock-input-locked (ioblock)
      (values
       (funcall (ioblock-peek-char-function ioblock) ioblock)))))

(defmethod stream-clear-input ((stream basic-character-input-stream))
  (let* ((ioblock (basic-stream-ioblock stream)))
    (with-ioblock-input-locked (ioblock)
      (values
        (%ioblock-clear-input ioblock)))))

(defmethod stream-unread-char ((s basic-character-input-stream) char)
  (let* ((ioblock (basic-stream-ioblock s)))
    (with-ioblock-input-locked (ioblock)
      (values
       (funcall (ioblock-unread-char-function ioblock) ioblock char)))))

(defmethod stream-read-ivector ((s basic-binary-input-stream)
				iv start nb)
  (let* ((ioblock (basic-stream-ioblock s)))
    (with-ioblock-input-locked (ioblock)
      (values
       (%ioblock-binary-in-ivect ioblock iv start nb)))))

(defmethod stream-read-vector ((stream basic-character-input-stream)
			       vector start end)
  (declare (fixnum start end))
  (if (not (typep vector 'simple-base-string))
    (generic-character-read-vector stream vector start end)
    (let* ((ioblock (basic-stream-ioblock stream)))
      (with-ioblock-input-locked (ioblock)
        (values
         (funcall (ioblock-character-read-vector-function ioblock)
                  ioblock vector start end))))))

(defmethod stream-read-line ((stream basic-character-input-stream))
  (let* ((ioblock (basic-stream-ioblock stream)))
    (with-ioblock-input-locked (ioblock)
      (funcall (ioblock-read-line-function ioblock) ioblock))))

                             
;;; Synonym streams.

(defclass synonym-stream (fundamental-stream)
    ((symbol :initarg :symbol :reader synonym-stream-symbol)))

(defmethod print-object ((s synonym-stream) out)
  (print-unreadable-object (s out :type t :identity t)
    (format out "to ~s" (synonym-stream-symbol s))))

(macrolet ((synonym-method (name &rest args)
            (let* ((stream (make-symbol "STREAM")))
              `(defmethod ,name ((,stream synonym-stream) ,@args)
                (,name (symbol-value (synonym-stream-symbol ,stream)) ,@args)))))
           (synonym-method stream-read-char)
           (synonym-method stream-read-byte)
           (synonym-method stream-unread-char c)
           (synonym-method stream-read-char-no-hang)
           (synonym-method stream-peek-char)
           (synonym-method stream-listen)
           (synonym-method stream-eofp)
           (synonym-method stream-clear-input)
           (synonym-method stream-read-line)
           (synonym-method stream-read-list l c)
           (synonym-method stream-read-vector v start end)
           (synonym-method stream-write-char c)
           ;(synonym-method stream-write-string str &optional (start 0) end)
           (synonym-method stream-write-byte b)
           (synonym-method stream-clear-output)
           (synonym-method stream-line-column)
           (synonym-method stream-line-length)
           (synonym-method stream-set-column new)
           (synonym-method stream-advance-to-column new)
           (synonym-method stream-start-line-p)
           (synonym-method stream-fresh-line)
           (synonym-method stream-terpri)
           (synonym-method stream-force-output)
           (synonym-method stream-finish-output)
           (synonym-method stream-write-list l c)
           (synonym-method stream-write-vector v start end)
           (synonym-method stream-element-type)
           (synonym-method input-stream-p)
           (synonym-method output-stream-p)
           (synonym-method interactive-stream-p)
           (synonym-method stream-direction)
	   (synonym-method stream-device direction)
           (synonym-method stream-surrounding-characters)
           (synonym-method stream-input-timeout)
           (synonym-method stream-output-timeout)
           (synonym-method stream-deadline)
           (synonym-method stream-eof-transient-p))

(defmethod (setf input-stream-timeout) (new (s synonym-stream))
  (setf (input-stream-timeout (symbol-value (synonym-stream-symbol s))) new))

(defmethod (setf output-stream-timeout) (new (s synonym-stream))
  (setf (output-stream-timeout (symbol-value (synonym-stream-symbol s))) new))


(defmethod stream-write-string ((s synonym-stream) string &optional (start 0) end)
  (stream-write-string (symbol-value (synonym-stream-symbol s)) string start end))

(defmethod stream-length ((s synonym-stream) &optional new)
  (stream-length (symbol-value (synonym-stream-symbol s)) new))

(defmethod stream-position ((s synonym-stream) &optional new)
  (stream-position (symbol-value (synonym-stream-symbol s)) new))

(defun make-synonym-stream (symbol)
  (make-instance 'synonym-stream :symbol (require-type symbol 'symbol)))

;;;
(defclass composite-stream-mixin ()
    ((open-p :initform t)))

(defmethod close :after ((stream composite-stream-mixin) &key abort)
  (declare (ignore abort))
  (with-slots (open-p) stream
    (setq open-p nil)))

(defmethod open-stream-p ((stream composite-stream-mixin))
  (slot-value stream 'open-p))


;;; Two-way streams.
(defclass two-way-stream (composite-stream-mixin fundamental-input-stream fundamental-output-stream)
    ((input-stream :initarg :input-stream :accessor two-way-stream-input-stream)
     (output-stream :initarg :output-stream :accessor two-way-stream-output-stream)))

(defmethod stream-eof-transient-p ((stream two-way-stream))
  (stream-eof-transient-p (two-way-stream-input-stream stream)))

(defmethod print-object ((s two-way-stream) out)
  (print-unreadable-object (s out :type t :identity t)
    (format out "input ~s, output ~s" 
            (two-way-stream-input-stream s)
            (two-way-stream-output-stream s))))

(macrolet ((two-way-input-method (name &rest args)
             (let* ((stream (make-symbol "STREAM")))
               `(defmethod ,name ((,stream two-way-stream) ,@args)
                 (,name (two-way-stream-input-stream ,stream) ,@args))))
           (two-way-output-method (name &rest args)
             (let* ((stream (make-symbol "STREAM")))
               `(defmethod ,name ((,stream two-way-stream) ,@args)
                 (,name (two-way-stream-output-stream ,stream) ,@args)))))
  (two-way-input-method stream-read-char)
  (two-way-input-method stream-read-byte)
  (two-way-input-method stream-unread-char c)
  (two-way-input-method stream-read-char-no-hang)
  (two-way-input-method stream-peek-char)
  (two-way-input-method stream-listen)
  (two-way-input-method stream-eofp)
  (two-way-input-method stream-clear-input)
  (two-way-input-method stream-read-line)
  (two-way-input-method stream-read-list l c)
  (two-way-input-method stream-read-vector v start end)
  (two-way-input-method stream-surrounding-characters)
  (two-way-input-method stream-input-timeout)
  (two-way-input-method interactive-stream-p)
  (two-way-output-method stream-write-char c)
  (two-way-output-method stream-write-byte b)
  (two-way-output-method stream-clear-output)
  (two-way-output-method stream-line-column)
  (two-way-output-method stream-line-length)
  (two-way-output-method stream-set-column new)
  (two-way-output-method stream-advance-to-column new)
  (two-way-output-method stream-start-line-p)
  (two-way-output-method stream-fresh-line)
  (two-way-output-method stream-terpri)
  (two-way-output-method stream-force-output)
  (two-way-output-method stream-finish-output)
  (two-way-output-method stream-write-list l c)
  (two-way-output-method stream-write-vector v start end)
  (two-way-output-method stream-output-timeout)
  (two-way-output-method stream-deadline))

(defmethod (setf stream-input-timeout) (new (s two-way-stream))
  (setf (stream-input-timeout (two-way-stream-input-stream s)) new))

(defmethod (setf stream-output-timeout) (new (s two-way-stream))
  (setf (stream-output-timeout (two-way-stream-output-stream s)) new))

(defmethod (setf stream-deadline) (new (s two-way-stream))
  (setf (stream-deadline (two-way-stream-output-stream s)) new))

(defmethod stream-device ((s two-way-stream) direction)
  (case direction
    (:input (stream-device (two-way-stream-input-stream s) direction))
    (:output (stream-device (two-way-stream-output-stream s) direction))))
    
(defmethod stream-write-string ((s two-way-stream) string &optional (start 0) end)
  (stream-write-string (two-way-stream-output-stream s) string start end))

(defmethod stream-element-type ((s two-way-stream))
  (let* ((in-type (stream-element-type (two-way-stream-input-stream s)))
         (out-type (stream-element-type (two-way-stream-output-stream s))))
    (if (equal in-type out-type)
      in-type
      `(and ,in-type ,out-type))))

(defun make-two-way-stream (in out)
  "Return a bidirectional stream which gets its input from INPUT-STREAM and
   sends its output to OUTPUT-STREAM."
  (unless (input-stream-p in)
    (require-type in 'input-stream))
  (unless (output-stream-p out)
    (require-type out 'output-stream))
  (make-instance 'two-way-stream :input-stream in :output-stream out))

;;; This is intended for use with things like *TERMINAL-IO*, where the
;;; OS echoes interactive input.  Whenever we read a character from
;;; the underlying input-stream of such a stream, we need to update
;;; our notion of the underlying output-stream's STREAM-LINE-COLUMN.

(defclass echoing-two-way-stream (two-way-stream)
    ())

(defmethod stream-read-char ((s echoing-two-way-stream))
  (let* ((out (two-way-stream-output-stream s))
         (in (two-way-stream-input-stream s)))
    (force-output out)
    (let* ((ch (stream-read-char in)))
      (unless (eq ch :eof)
        (if (eq ch #\newline)
          (stream-set-column out 0)
          (let* ((cur (stream-line-column out)))
            (when cur
              (stream-set-column out (1+ (the fixnum cur)))))))
      ch)))

(defmethod stream-read-line ((s echoing-two-way-stream))
  (let* ((out (two-way-stream-output-stream s)))
    (force-output out)
    (multiple-value-bind (string eof)
        (call-next-method)
      (unless eof
        (stream-set-column out 0))
      (values string eof))))

(defun make-echoing-two-way-stream (in out)
  (make-instance 'echoing-two-way-stream :input-stream in :output-stream out))

;;;echo streams

(defclass echo-stream (two-way-stream)
    ((did-untyi :initform nil)))

(defmethod echo-stream-input-stream ((s echo-stream))
  (two-way-stream-input-stream s))

(defmethod echo-stream-output-stream ((s echo-stream))
  (two-way-stream-output-stream s))

(defmethod stream-read-char ((s echo-stream))
  (let* ((char (stream-read-char (echo-stream-input-stream s))))
    (unless (eq char :eof)
      (if (slot-value s 'did-untyi)
        (setf (slot-value s 'did-untyi) nil)
        (stream-write-char (echo-stream-output-stream s) char)))
    char))

(defmethod stream-unread-char ((s echo-stream) c)
  (call-next-method s c)
  (setf (slot-value s 'did-untyi) c))

(defmethod stream-read-char-no-hang ((s echo-stream))
  (let* ((char (stream-read-char-no-hang (echo-stream-input-stream s))))
    (unless (eq char :eof)
      (if (slot-value s 'did-untyi)
        (setf (slot-value s 'did-untyi) nil)
        (stream-write-char (echo-stream-output-stream s) char)))
    char))

(defmethod stream-clear-input ((s echo-stream))
  (call-next-method)
  (setf (slot-value s 'did-untyi) nil))

(defmethod stream-read-byte ((s echo-stream))
  (let* ((byte (stream-read-byte (echo-stream-input-stream s))))
    (unless (eq byte :eof)
      (stream-write-byte (echo-stream-output-stream s) byte))
    byte))

(defmethod stream-read-line ((s echo-stream))
  (generic-read-line s))

(defmethod stream-read-vector ((s echo-stream) vector start end)
  (if (subtypep (stream-element-type s) 'character)
      (generic-character-read-vector s vector start end)
    (generic-binary-read-vector s vector start end)))

(defun make-echo-stream (input-stream output-stream)
  "Return a bidirectional stream which gets its input from INPUT-STREAM and
   sends its output to OUTPUT-STREAM. In addition, all input is echoed to
   the output stream."
  (make-instance 'echo-stream
                 :input-stream input-stream
                 :output-stream output-stream))

;;;concatenated-streams

(defclass concatenated-stream (composite-stream-mixin fundamental-input-stream)
    ((streams :initarg :streams :accessor concatenated-stream-streams)))


(defun concatenated-stream-current-input-stream (s)
  (car (concatenated-stream-streams s)))

(defun concatenated-stream-next-input-stream (s)
  (setf (concatenated-stream-streams s)
	(cdr (concatenated-stream-streams s)))
  (concatenated-stream-current-input-stream s))

(defmethod stream-element-type ((s concatenated-stream))
  (let* ((c (concatenated-stream-current-input-stream s)))
    (if c
      (stream-element-type c)
      nil)))



(defmethod stream-read-char ((s concatenated-stream))
  (do* ((c (concatenated-stream-current-input-stream s)
	   (concatenated-stream-next-input-stream s)))
       ((null c) :eof)
    (let* ((ch (stream-read-char c)))
      (unless (eq ch :eof)
	(return ch)))))

(defmethod stream-read-char-no-hang ((s concatenated-stream))
  (do* ((c (concatenated-stream-current-input-stream s)
	   (concatenated-stream-next-input-stream s)))
       ((null c) :eof)
    (let* ((ch (stream-read-char-no-hang c)))
      (unless (eq ch :eof)
	(return ch)))))

(defmethod stream-read-byte ((s concatenated-stream))
  (do* ((c (concatenated-stream-current-input-stream s)
	   (concatenated-stream-next-input-stream s)))
       ((null c) :eof)
    (let* ((b (stream-read-byte c)))
      (unless (eq b :eof)
	(return b)))))

(defmethod stream-peek-char ((s concatenated-stream))
  (do* ((c (concatenated-stream-current-input-stream s)
       (concatenated-stream-next-input-stream s)))
       ((null c) :eof)
    (let* ((ch (stream-peek-char c)))
      (unless (eq ch :eof)
        (return ch)))))

(defmethod stream-read-line ((s concatenated-stream))
  (generic-read-line s))

(defmethod stream-read-list ((s concatenated-stream) list count)
  (generic-character-read-list s list count))

(defmethod stream-read-vector ((s concatenated-stream) vector start end)
  (if (subtypep (stream-element-type s) 'character)
      (generic-character-read-vector s vector start end)
    (generic-binary-read-vector s vector start end)))

(defmethod stream-unread-char ((s concatenated-stream) char)
  (let* ((c (concatenated-stream-current-input-stream s)))
    (if c
      (stream-unread-char c char))))

(defmethod stream-listen ((s concatenated-stream))
  (do* ((c (concatenated-stream-current-input-stream s)
	   (concatenated-stream-next-input-stream s)))
       ((null c))
    (when (stream-listen c)
      (return t))))

(defmethod stream-eofp ((s concatenated-stream))
  (do* ((c (concatenated-stream-current-input-stream s)
	   (concatenated-stream-next-input-stream s)))
       ((null c) t)
    (when (stream-listen c)
      (return nil))))

(defmethod stream-clear-input ((s concatenated-stream))
  (let* ((c (concatenated-stream-current-input-stream s)))
    (when c (stream-clear-input c))))


(defun make-concatenated-stream (&rest streams)
  "Return a stream which takes its input from each of the streams in turn,
   going on to the next at EOF."
  (dolist (s streams (make-instance 'concatenated-stream :streams streams))
    (unless (input-stream-p s)
      (error "~S is not an input stream" s))))

;;;broadcast-streams



(defclass broadcast-stream (fundamental-output-stream)
    ((streams :initarg :streams :reader broadcast-stream-streams)))

(macrolet ((broadcast-method
	       (op (stream &rest others )
                   &optional
                   (args (cons stream others)))
	     (let* ((sub (gensym))
		    (result (gensym)))
               `(defmethod ,op ((,stream broadcast-stream) ,@others)
		 (let* ((,result nil))
		   (dolist (,sub (broadcast-stream-streams ,stream) ,result)
			     (setq ,result (,op ,@(cons sub (cdr args))))))))))
	     (broadcast-method stream-write-char (s c))
	     (broadcast-method stream-write-string
				      (s str &optional (start 0) end)
				      (s str start end))
	     (broadcast-method stream-write-byte (s b))
	     (broadcast-method stream-clear-output (s))
	     (broadcast-method stream-line-column (s))
	     (broadcast-method stream-set-column (s new))
	     (broadcast-method stream-advance-to-column (s new))
	     (broadcast-method stream-start-line-p (s))
	     (broadcast-method stream-terpri (s))
	     (broadcast-method stream-force-output (s))
	     (broadcast-method stream-finish-output (s))
	     (broadcast-method stream-write-list (s l c))
	     (broadcast-method stream-write-vector (s v start end)))

(defun last-broadcast-stream (s)
  (car (last (broadcast-stream-streams s))))

(defmethod stream-fresh-line ((s broadcast-stream))
  (let* ((did-output-newline nil))
    (dolist (sub (broadcast-stream-streams s) did-output-newline)
      (setq did-output-newline (stream-fresh-line sub)))))

(defmethod stream-element-type ((s broadcast-stream))
  (let* ((last (last-broadcast-stream s)))
    (if last
      (stream-element-type last)
      t)))

(defmethod stream-length ((s broadcast-stream) &optional new)
  (unless new
    (let* ((last (last-broadcast-stream s)))
      (if last
	(stream-length last)
	0))))

(defmethod stream-position ((s broadcast-stream) &optional new)
  (unless new
    (let* ((last (last-broadcast-stream s)))
      (if last
	(stream-position last)
	0))))

(defun make-broadcast-stream (&rest streams)
  (dolist (s streams (make-instance 'broadcast-stream :streams streams))
    (unless (output-stream-p s)
      (report-bad-arg s '(satisfies output-stream-p)))))



;;; String streams.
(make-built-in-class 'string-stream 'basic-character-stream)

(defmethod print-object ((s string-stream) out)
  (print-unreadable-object (s out :type t :identity t)
    (unless (open-stream-p s)  (format out " ~s" :closed))))


                 

(defstruct (string-stream-ioblock (:include ioblock))
  string)

(defstruct (string-output-stream-ioblock (:include string-stream-ioblock))
  (index 0)
  freelist
  (line-length 80))

(defstatic *string-output-stream-class* (make-built-in-class 'string-output-stream 'string-stream 'basic-character-output-stream))
(defstatic *string-output-stream-class-wrapper* (%class-own-wrapper *string-output-stream-class*))

(defstatic *fill-pointer-string-output-stream-class* (make-built-in-class 'fill-pointer-string-output-stream 'string-output-stream))

(def-standard-initial-binding %string-output-stream-ioblocks% (%cons-pool nil))

(defmethod stream-force-output ((s string-output-stream))
  nil)

(defmethod stream-finish-output ((s string-output-stream))
  nil)

(defmethod stream-clear-output ((s string-output-stream))
  nil)

(defmethod stream-line-length ((s string-output-stream))
  (let* ((ioblock (basic-stream-ioblock s)))
    (string-output-stream-ioblock-line-length ioblock)))

(defmethod (setf stream-line-length) (newlen (s string-output-stream))
  (let* ((ioblock (basic-stream-ioblock s)))
    (setf (string-output-stream-ioblock-line-length ioblock) newlen)))


;;; Should only be used for a stream whose class is exactly
;;; *string-output-stream-class* 
(defun %close-string-output-stream (stream ioblock)
  (let* ((pool %string-output-stream-ioblocks%))
    (when (and pool
               (eq (basic-stream.wrapper stream)
                   *string-output-stream-class-wrapper*)
               (eq (string-output-stream-ioblock-freelist ioblock) pool))
      (without-interrupts
       (setf (ioblock-stream ioblock) (pool.data pool)
             (pool.data pool) ioblock)))))

;;; If this is the sort of string stream whose ioblock we recycle and
;;; there's a thread-local binding of the variable we use for a freelist,
;;; return the value of that binding.
(defun %string-stream-ioblock-freelist (stream)
  (and stream
       (eq (basic-stream.wrapper stream)
           *string-output-stream-class-wrapper*)
       (let* ((loc (%tcr-binding-location (%current-tcr) '%string-output-stream-ioblocks%)))
         (and loc (%fixnum-ref loc)))))


(defun create-string-output-stream-ioblock (stream string write-char-function write-string-function)
  (let* ((recycled (and stream
                        (eq (basic-stream.wrapper stream)
                            *string-output-stream-class-wrapper*)
                        (without-interrupts
                         (let* ((data (pool.data %string-output-stream-ioblocks%)))
                           (when data
                             (setf (pool.data %string-output-stream-ioblocks%)
                                   (ioblock-stream data)
                                   (ioblock-stream data) stream
                                   (ioblock-device data) -1
                                   (ioblock-charpos data) 0
                                   (string-output-stream-ioblock-index data) 0
                                   (string-output-stream-ioblock-line-length data) 80))
                           data)))))
    (or recycled
        (make-string-output-stream-ioblock :stream stream
                                           :string string
                                           :element-type 'character
                                           :write-char-function write-char-function
                                           :write-char-when-locked-function write-char-function
                                           :write-simple-string-function write-string-function
                                           :force-output-function #'false
                                           :freelist (%string-stream-ioblock-freelist stream)
                                           :close-function #'%close-string-output-stream
                                           :device -1))))
                        


(defun %%make-string-output-stream (class string write-char-function write-string-function)
  (let* ((stream (gvector :basic-stream (%class.own-wrapper class)
                          (logior (ash 1 basic-stream-flag.open-character)
                                  (ash 1 basic-stream-flag.open-output))
                          nil
                          nil))
         (ioblock (create-string-output-stream-ioblock stream string write-char-function write-string-function)))
      (setf (basic-stream.state stream) ioblock)
      stream))

(declaim (inline %string-push-extend))
(defun %string-push-extend (char string)
  (let* ((fill (%svref string target::vectorH.logsize-cell))
         (size (%svref string target::vectorH.physsize-cell)))
    (declare (fixnum fill size))
    (if (< fill size)
      (multiple-value-bind (data offset) (array-data-and-offset string)
        (declare (simple-string data) (fixnum offset))
        (setf (schar data (the fixnum (+ offset fill))) char
              (%svref string target::vectorH.logsize-cell) (the fixnum (1+ fill))))
      (vector-push-extend char string))))
              

(defun fill-pointer-string-output-stream-ioblock-write-char (ioblock char)
  ;; can do better (maybe much better) than VECTOR-PUSH-EXTEND here.
  (if (eql char #\Newline)
    (setf (ioblock-charpos ioblock) 0)
    (incf (ioblock-charpos ioblock)))
  (%string-push-extend char (string-stream-ioblock-string ioblock)))

(defun fill-pointer-string-output-stream-ioblock-write-simple-string (ioblock string start-char num-chars)
  (let* ((end (+ start-char num-chars))
         (nlpos (position #\Newline string :start start-char :end end :from-end t)))
    (if nlpos
      (setf (ioblock-charpos ioblock) (- end nlpos))
      (incf (ioblock-charpos ioblock) num-chars))
    (let* ((out (string-stream-ioblock-string ioblock)))
      (do* ((n 0 (1+ n))
            (i start-char (1+ i)))
           ((= n num-chars) num-chars)
        (%string-push-extend (schar string i) out)))))

(defmethod stream-position ((s fill-pointer-string-output-stream) &optional newpos)
  (let* ((string (string-stream-string s)))
    (if newpos
      (setf (fill-pointer string) newpos)
      (fill-pointer string))))

;;; If the stream's string is adjustable, it doesn't really have a meaningful
;;; "maximum size".
(defmethod stream-length ((s string-output-stream) &optional newlen)
  (unless newlen
    (array-total-size (string-stream-string s))))

;;; This creates a FILL-POINTER-STRING-OUTPUT-STREAM.
(defun %make-string-output-stream (string)
  (unless (and (typep string 'string)
               (array-has-fill-pointer-p string))
    (error "~S must be a string with a fill pointer." string))
  (%%make-string-output-stream *fill-pointer-string-output-stream-class* string 'fill-pointer-string-output-stream-ioblock-write-char 'fill-pointer-string-output-stream-ioblock-write-simple-string))

(defun string-output-stream-ioblock-write-char (ioblock char)
  (let* ((string (string-output-stream-ioblock-string ioblock))
         (index (string-output-stream-ioblock-index ioblock))
         (len (length string)))
    (declare (simple-string string)
             (fixnum index len))
  (if (eql char #\Newline)
    (setf (ioblock-charpos ioblock) 0)
    (incf (ioblock-charpos ioblock)))
  (if (= index len)
      (let* ((newlen (if (zerop len) 20 (+ len len)))      ;non-zero !
             (new (make-string newlen)))
        (%copy-ivector-to-ivector string 0 new 0 (the fixnum (ash len 2)))
        (setq string new)
        (setf (string-output-stream-ioblock-string ioblock) new)))
    (setf (string-output-stream-ioblock-index ioblock) (the fixnum (1+ index))
          (schar string index) char)))

(defun string-output-stream-ioblock-write-simple-string (ioblock string start-char num-chars)
  (declare (simple-string string)
           (fixnum start-char num-chars)
           (optimize (speed 3) (safety 0)))
  (let* ((out (string-output-stream-ioblock-string ioblock))
         (index (string-output-stream-ioblock-index ioblock))
         (len (length out))
         (need (+ index num-chars)))
    (declare (simple-string out)
             (fixnum index len need))
    (if (< len need)
      (let* ((newlen (+ need need))
             (new (make-string newlen)))
        (declare (fixnum newlen) (simple-string new))
        (dotimes (i len)
          (setf (schar new i) (schar out i)))
        (setq out new)
        (setf (string-output-stream-ioblock-string ioblock) new)))
    (do* ((src start-char (1+ src))
          (dest index (1+ dest))
          (nlpos nil)
          (end (+ start-char num-chars)))
         ((= src end)
          (setf (string-output-stream-ioblock-index ioblock) need)
          (if nlpos
            (setf (ioblock-charpos ioblock) (the fixnum (- end (the fixnum nlpos))))
            (incf (ioblock-charpos ioblock) num-chars))
          num-chars)
      (declare (fixnum src dest end))
      (let* ((char (schar string src)))
        (if (eql char #\Newline)
          (setq nlpos (the fixnum (1+ src))))
        (setf (schar out dest) char)))))


(defmethod stream-position ((stream string-output-stream) &optional newpos)
  (let* ((ioblock (basic-stream-ioblock stream)))
    (if (null newpos)
      (string-output-stream-ioblock-index ioblock)
      (if (and (typep newpos 'fixnum)
               (>= (the fixnum newpos) 0)
               (<= (the fixnum newpos) (length (string-output-stream-ioblock-string ioblock))))
        (setf (string-output-stream-ioblock-index ioblock) newpos)))))

(defun make-simple-string-output-stream ()
  ;; There's a good chance that we'll get a recycled ioblock
  ;; that already has a string; if not, we defer actually
  ;; creating a usable string until write-char
  (%%make-string-output-stream *string-output-stream-class*
                               ""
                               'string-output-stream-ioblock-write-char
                               'string-output-stream-ioblock-write-simple-string))

(defun make-string-output-stream (&key (element-type 'character element-type-p))
  "Return an output stream which will accumulate all output given it for
   the benefit of the function GET-OUTPUT-STREAM-STRING."
  (when (and element-type-p
             (not (member element-type '(base-character character
                                         standard-char))))
    (unless (subtypep element-type 'character)
      (error "~S argument ~S is not a subtype of ~S."
             :element-type element-type 'character)))
  (make-simple-string-output-stream))


;;;"Bounded" string output streams.
(defstatic *truncating-string-output-stream-class* (make-built-in-class 'truncating-string-stream 'string-output-stream))

(defun truncating-string-output-stream-ioblock-write-char (ioblock char)
  (let* ((stream (ioblock-stream ioblock))
         (string (string-output-stream-ioblock-string ioblock))
         (index (string-output-stream-ioblock-index ioblock)))
    (declare (fixnum index) (simple-string string))
    (if (< index (the fixnum (length string)))
      (progn
        (setf (schar string index) char
              (string-output-stream-ioblock-index ioblock) (the fixnum (1+ index)))
        (if (eql char #\Newline)
          (setf (ioblock-charpos ioblock) 0)
          (incf (ioblock-charpos ioblock))))
      (setf (getf (basic-stream.info stream) :truncated) t))))

(defun truncating-string-output-stream-ioblock-write-simple-string (ioblock string start-char num-chars)
  (let* ((stream (ioblock-stream ioblock)))
  (do* ((n 0 (1+ n))
        (i start-char (1+ i)))
       ((= n num-chars) num-chars)
    (truncating-string-output-stream-ioblock-write-char ioblock (schar string i))
    (if (getf (basic-stream.info stream) :truncated)
      (return n)))))

(defun truncating-string-output-stream-truncated-p (stream)
  (getf (basic-stream.info stream) :truncated))

(defun make-truncating-string-stream (len)
  (%%make-string-output-stream *truncating-string-output-stream-class*
                               (make-string len)
                               'truncating-string-output-stream-ioblock-write-char
                               'truncating-string-output-stream-ioblock-write-simple-string))
                               

;;;One way to indent on newlines:

(defstatic *indenting-string-output-stream-class* (make-built-in-class 'indenting-string-output-stream 'string-output-stream))
(defstatic *indenting-string-output-stream-class-wrapper* (%class-own-wrapper *indenting-string-output-stream-class*))


(defun indenting-string-stream-ioblock-write-char (ioblock c)
  (string-output-stream-ioblock-write-char ioblock c)
  (if (eql c #\newline)
    (let* ((stream (ioblock-stream ioblock))
           (info (basic-stream.info stream))
           (indent (getf info 'indent))
           (prefixlen 0)
           (prefixchar (getf info 'prefixchar)))
      (when prefixchar
        (if (typep prefixchar 'character)
          (progn
            (setq prefixlen 1)
            (string-output-stream-ioblock-write-char ioblock prefixchar))
          (dotimes (i (setq prefixlen (length prefixchar)))
            (string-output-stream-ioblock-write-char ioblock (schar prefixchar i)))))
      (when indent
        (dotimes (i (the fixnum (- indent prefixlen)))
          (string-output-stream-ioblock-write-char ioblock #\Space)))))
  c)

(defun indenting-string-stream-ioblock-write-simple-string (ioblock string start-char num-chars)
  (do* ((n 0 (1+ n))
        (i start-char (1+ i)))
       ((= n num-chars) num-chars)
    (indenting-string-stream-ioblock-write-char ioblock (schar string i))))

(defun make-indenting-string-output-stream (prefixchar indent)
  (let* ((stream (%%make-string-output-stream
                   *indenting-string-output-stream-class*
                  (make-string 10)
                  'indenting-string-stream-ioblock-write-char
                  'indenting-string-stream-ioblock-write-simple-string)))
    (setf (getf (basic-stream.info stream) 'indent) indent
          (getf (basic-stream.info stream) 'prefixchar) prefixchar)
    stream))

(defun (setf indenting-string-output-stream-indent) (new stream)
  (if (and (typep stream 'basic-stream)
           (eq (basic-stream.wrapper stream) *indenting-string-output-stream-class-wrapper*))
    (setf (getf (basic-stream.info stream) 'indent) new)
    (report-bad-arg stream 'indenting-string-output-stream)))


(defun get-output-stream-string (s)
 (let* ((class (if (typep s 'basic-stream) (%wrapper-class (basic-stream.wrapper s)))))
    (or (eq class *string-output-stream-class*)
        (eq class *truncating-string-output-stream-class*)
        (eq class *indenting-string-output-stream-class*)
        (eq class *fill-pointer-string-output-stream-class*)
        (report-bad-arg s 'string-output-stream))
    (let* ((ioblock (basic-stream-ioblock s))
           (string (string-stream-ioblock-string ioblock)))
      (if (eq class *fill-pointer-string-output-stream-class*)
        (prog1 (ensure-simple-string string)
          (setf (fill-pointer string) 0))
        (let* ((index (string-output-stream-ioblock-index ioblock))
               (result (make-string index)))
          (declare (fixnum index))
          (%copy-ivector-to-ivector string 0 result 0 (the fixnum (ash index 2)))
          (setf (string-output-stream-ioblock-index ioblock) 0)
          result)))))

;;; String input streams.
(defstatic *string-input-stream-class* (make-built-in-class 'string-input-stream 'string-stream 'basic-character-input-stream))
(defstatic *string-input-stream-class-wrapper* (%class-own-wrapper *string-input-stream-class*))
(defstruct (string-input-stream-ioblock (:include string-stream-ioblock))
  (start 0)
  index
  end
  (offset 0))



(defun string-input-stream-index (s)
  (if (and (typep s 'basic-stream)
           (eq *string-input-stream-class-wrapper* (basic-stream.wrapper s)))
    (let* ((ioblock (basic-stream-ioblock s)))
      (- (string-input-stream-ioblock-index ioblock)
         (string-input-stream-ioblock-offset ioblock)))
    (report-bad-arg s 'string-input-stream)))


(defmethod stream-surrounding-characters ((s string-input-stream))
  (let* ((ioblock (basic-stream.state s)))
    (when ioblock
      (let* ((start (string-input-stream-ioblock-start ioblock))
             (idx (string-input-stream-ioblock-index ioblock))
             (end (string-input-stream-ioblock-end ioblock))
             (string (string-stream-ioblock-string ioblock)))
        (subseq string (max (- idx 10) start) (min (+ idx 10) end))))))
    

(defmethod stream-position ((s string-input-stream) &optional newpos)
  (let* ((ioblock (basic-stream-ioblock s))
         (start (string-input-stream-ioblock-start ioblock))
         (idx (string-input-stream-ioblock-index ioblock))
         (end (string-input-stream-ioblock-end ioblock)))
    (declare (fixnum end idx start))
    (if newpos
      (let* ((limit (- end start)))
        (declare (fixnum limit))
        (if (and (typep newpos 'fixnum)
                 (>= (the fixnum newpos) 0)
                 (<= (the fixnum newpos) limit))
          (progn
            (setf (string-input-stream-ioblock-index ioblock)
                  (the fixnum (+ start (the fixnum newpos))))
            newpos)
          (report-bad-arg newpos `(integer 0 ,limit))))
      (the fixnum (- idx start)))))
    
  

(defun string-input-stream-ioblock-read-char (ioblock)
  (let* ((string (string-stream-ioblock-string ioblock))
         (idx (string-input-stream-ioblock-index ioblock))
         (end (string-input-stream-ioblock-end ioblock)))
    (declare (fixnum idx end)
             (simple-string string))
    (if (< idx end)
      (progn (setf (string-input-stream-ioblock-index ioblock)
                   (the fixnum (1+ idx)))
             (schar string idx))
      :eof)))

(defun string-input-stream-ioblock-read-line (ioblock)
  (let* ((string (string-stream-ioblock-string ioblock))
         (idx (string-input-stream-ioblock-index ioblock))
         (end (string-input-stream-ioblock-end ioblock)))
    (declare (fixnum idx end)
             (simple-string string))
    (if (>= idx end)
      (values "" t)
      (let* ((pos (position #\Newline string :start idx :end end)))
        (if pos
          (locally (declare (type index pos))
            (let* ((new (make-string (the fixnum (- pos idx)))))
              (declare (simple-base-string new))
              (setf (string-input-stream-ioblock-index ioblock)
                    (the fixnum (1+ pos)))
              (do* ((src idx (1+ src))
                    (dest 0 (1+ dest)))
                   ((= src pos) (values new nil))
                (declare (fixnum src dest))
                (setf (schar new dest) (schar string src)))))
          (let* ((new (make-string (the fixnum (- end idx)))))
            (declare (simple-base-string new))
              (setf (string-input-stream-ioblock-index ioblock) end)
              (do* ((src idx (1+ src))
                    (dest 0 (1+ dest)))
                   ((= src end) (values new t))
                (declare (fixnum src dest))
                (setf (schar new dest) (schar string src)))))))))


(defun string-input-stream-ioblock-peek-char (ioblock)
  (let* ((string (string-stream-ioblock-string ioblock))
         (idx (string-input-stream-ioblock-index ioblock))
         (end (string-input-stream-ioblock-end ioblock)))
    (declare (fixnum idx end)
             (simple-string string))
    (if (< idx end)
      (schar string idx)
      :eof)))

(defun string-input-stream-ioblock-unread-char (ioblock char)
  (let* ((string (string-stream-ioblock-string ioblock))
         (idx (string-input-stream-ioblock-index ioblock))
         (start (string-input-stream-ioblock-start ioblock)))
    (declare (fixnum idx start)
             (simple-string string))
    (unless (> idx start)
      (error "Nothing has been read from ~s yet." (ioblock-stream ioblock)))
    (decf idx)
    (unless (eq char (schar string idx))
      (error "~a was not the last character read from ~s" char (ioblock-stream ioblock)))
    (setf (string-input-stream-ioblock-index ioblock) idx)
    char))
  
  
(defmethod stream-length ((s string-input-stream) &optional new)
  (unless new
    (let ((ioblock (basic-stream-ioblock s)))
      (%i- (string-input-stream-ioblock-end ioblock)
	   (string-input-stream-ioblock-start ioblock)))))

(defmethod stream-eofp ((s string-input-stream))
  (let* ((ioblock (basic-stream-ioblock s))
         (idx (string-input-stream-ioblock-index ioblock))
         (end (string-input-stream-ioblock-end ioblock)))
    (declare (fixnum idx end))
    (>= idx end)))

(defmethod stream-listen ((s string-input-stream))
  (let* ((ioblock (basic-stream-ioblock s))
         (idx (string-input-stream-ioblock-index ioblock))
         (end (string-input-stream-ioblock-end ioblock)))
    (declare (fixnum idx end))
    (< idx end)))

(defmethod stream-clear-input ((s string-input-stream))
  (basic-stream-ioblock s)
  nil)

(defun string-input-stream-character-read-vector (ioblock vector start end)
  (let* ((string (string-stream-ioblock-string ioblock))
         (idx (string-input-stream-ioblock-index ioblock))
         (limit (string-input-stream-ioblock-end ioblock)))
    (declare (fixnum idx limit))
    (do* ((i start (1+ i)))
         ((= i end) (setf (string-input-stream-ioblock-index ioblock) idx) end)
      (declare (fixnum i))
      (if (< idx limit)
        (setf (uvref vector i) (schar string idx)
              idx (1+ idx))
        (progn
          (setf (string-input-stream-ioblock-index ioblock) idx)
          (return i))))))
         


(defun make-string-input-stream (string &optional (start 0)
                                        (end nil))
  "Return an input stream which will supply the characters of STRING between
  START and END in order."
  (setq end (check-sequence-bounds string start end))
  (multiple-value-bind (data offset) (array-data-and-offset string)
    (unless (typep data 'simple-base-string)
      (report-bad-arg string 'string))
    (incf start offset)
    (incf end offset)
    (let* ((stream (make-basic-stream-instance
                    *string-input-stream-class*
                    :element-type 'character))
           (ioblock (make-string-input-stream-ioblock
                     :stream stream
                     :offset offset
                     :string data
                     :start start
                     :index start
                     :end end
                     :read-char-function 'string-input-stream-ioblock-read-char
                     :read-char-when-locked-function 'string-input-stream-ioblock-read-char
                     :peek-char-function 'string-input-stream-ioblock-peek-char
                     :character-read-vector-function 'string-input-stream-character-read-vector
                     :close-function #'false
                     :unread-char-function 'string-input-stream-ioblock-unread-char
                     :read-line-function 'string-input-stream-ioblock-read-line
                     )))
      (setf (basic-stream.state stream) ioblock)
      stream)))

(defun string-stream-string (s)
  (let* ((class (if (typep s 'basic-stream) (%wrapper-class (basic-stream.wrapper s)))))
    (or (eq class *string-output-stream-class*)
        (eq class *truncating-string-output-stream-class*)
        (eq class *indenting-string-output-stream-class*)
        (report-bad-arg s 'string-output-stream)))
  (string-stream-ioblock-string (basic-stream-ioblock s)))



;;; A mixin to be used with FUNDAMENTAL-STREAMs that want to use ioblocks
;;; to buffer I/O.

(defclass buffered-stream-mixin ()
  ((ioblock :reader %stream-ioblock :writer (setf stream-ioblock) :initform nil)))

(defmethod open-stream-p ((s buffered-stream-mixin))
  (with-slots (ioblock) s
    (not (null ioblock))))

(declaim (inline stream-ioblock))

(defun stream-ioblock (stream error-if-nil)
  (or (if (typep stream 'basic-stream)
        (basic-stream.state stream)
        (%stream-ioblock stream))
      (when error-if-nil
        (stream-is-closed stream))))

(defmethod stream-device ((s buffered-stream-mixin) direction)
  (declare (ignore direction))
  (let* ((ioblock (stream-ioblock s nil)))
    (and ioblock (ioblock-device ioblock))))

(defmethod stream-device ((s basic-stream) direction)
  (declare (ignore direction))
  (let* ((ioblock (basic-stream.state s)))
    (and ioblock (ioblock-device ioblock))))
  
(defmethod stream-element-type ((s buffered-stream-mixin))
  (ioblock-element-type (stream-ioblock s t)))

(defmethod stream-element-type ((s basic-stream))
  (ioblock-element-type (basic-stream-ioblock s)))


(defmethod stream-create-ioblock ((stream buffered-stream-mixin) &rest args &key)
  (declare (dynamic-extent args))
  (apply #'make-ioblock :stream stream args))

(defmethod stream-owner ((stream stream))
  )

(defmethod stream-owner ((stream buffered-stream-mixin))
  (let* ((ioblock (stream-ioblock stream nil)))
    (and ioblock (let* ((owner (ioblock-owner ioblock)))
                   (unless (eql owner 0) owner)))))

(defmethod stream-owner ((stream basic-stream))
  (let* ((ioblock (basic-stream.state stream)))
    (and ioblock (let* ((owner (ioblock-owner ioblock)))
                   (unless (eql owner 0) owner)))))


(defclass buffered-input-stream-mixin
          (buffered-stream-mixin fundamental-input-stream)
  ())

(defclass buffered-output-stream-mixin
          (buffered-stream-mixin fundamental-output-stream)
  ())

(defclass buffered-io-stream-mixin
          (buffered-input-stream-mixin buffered-output-stream-mixin)
  ())

(defclass buffered-character-input-stream-mixin
          (buffered-input-stream-mixin fundamental-character-input-stream)
  ())

(defclass buffered-character-output-stream-mixin
          (buffered-output-stream-mixin fundamental-character-output-stream)
  ())

(defclass buffered-character-io-stream-mixin
          (buffered-character-input-stream-mixin buffered-character-output-stream-mixin)
  ())

(defclass buffered-binary-input-stream-mixin
          (buffered-input-stream-mixin fundamental-binary-input-stream)
  ())

(defclass buffered-binary-output-stream-mixin
          (buffered-output-stream-mixin fundamental-binary-output-stream)
  ())

(defclass buffered-binary-io-stream-mixin
          (buffered-binary-input-stream-mixin
           buffered-binary-output-stream-mixin)
  ())

(defmethod close :after ((stream buffered-stream-mixin) &key abort)
  (declare (ignore abort))
  (let* ((ioblock (stream-ioblock stream nil)))
    (when ioblock
      (%ioblock-close ioblock))))

(defmethod close :before ((stream buffered-output-stream-mixin) &key abort)
  (unless abort
    (when (open-stream-p stream)
      (stream-force-output stream))))

(defmethod close-for-termination ((stream buffered-output-stream-mixin) abort)
  ;; This method should only be invoked via the termination mechanism,
  ;; so it can safely assume that there's no contention for the stream.
  (let* ((ioblock (stream-ioblock stream nil)))
    (when ioblock (setf (ioblock-owner ioblock) nil)))
  (close stream :abort abort))


(defmethod interactive-stream-p ((stream buffered-stream-mixin))
  (let* ((ioblock (stream-ioblock stream nil)))
    (and ioblock (ioblock-interactive ioblock))))

(defmethod interactive-stream-p ((stream basic-stream))
  (let* ((ioblock (basic-stream.state stream)))
    (and ioblock (ioblock-interactive ioblock))))


(defmethod close :after ((stream basic-stream) &key abort)
  (declare (ignore abort))
  (let* ((ioblock (basic-stream.state stream)))
    (when ioblock
      (%ioblock-close ioblock))))

(defmethod close-for-termination  ((stream basic-stream) abort)
  (let* ((ioblock (basic-stream.state stream)))
    (when ioblock (setf (ioblock-owner ioblock) nil)))
  (close stream :abort abort))

  

(defmethod open-stream-p ((stream basic-stream))
  (not (null (basic-stream.state stream))))

(defmethod close :before ((stream basic-output-stream) &key abort)
  (unless abort
    (when (open-stream-p stream)
      (stream-force-output stream))))

(defmethod stream-surrounding-characters ((stream buffered-character-input-stream-mixin))
    (let* ((ioblock (stream-ioblock stream nil)))
      (and ioblock (%ioblock-surrounding-characters ioblock))))

(defmethod stream-surrounding-characters ((stream basic-character-input-stream))
    (let* ((ioblock (basic-stream.state stream)))
      (and ioblock (%ioblock-surrounding-characters ioblock))))


#|
(defgeneric ioblock-advance (stream ioblock readp)
  (:documentation
   "Called when the current input buffer is empty (or non-existent).
    readp true means the caller expects to return a byte now.
    Return value is meaningless unless readp is true, in which case
    it means that there is input ready"))

(defgeneric ioblock-listen (stream ioblock)
  (:documentation
   "Called in response to stream-listen when the current
    input buffer is empty.
    Returns a boolean"))

(defgeneric ioblock-eofp (stream ioblock)
  (:documentation
   "Called in response to stream-eofp when the input buffer is empty.
    Returns a boolean."))

(defgeneric ioblock-force-output (stream ioblock count finish-p)
  (:documentation
   "Called in response to stream-force-output.
    Write count bytes from ioblock-outbuf.
    Finish the I/O if finish-p is true."))

(defgeneric ioblock-close (stream ioblock)
  (:documentation
   "May free some resources associated with the ioblock."))
|#

(defmethod ioblock-close ((stream buffered-stream-mixin) ioblock)
  (declare (ignore ioblock)))

(defmethod ioblock-force-output ((stream buffered-output-stream-mixin)
                                   ioblock
                                   count
                                   finish-p)
  (declare (ignore ioblock count finish-p)))




(defmethod stream-read-char ((stream buffered-character-input-stream-mixin))
  (let* ((ioblock (stream-ioblock stream t)))
    (funcall (ioblock-read-char-function ioblock) ioblock)))

(defmethod stream-read-char-no-hang ((stream buffered-character-input-stream-mixin))
  (with-stream-ioblock-input (ioblock stream :speedy t)
    (%ioblock-tyi-no-hang ioblock)))

(defmethod stream-peek-char ((stream buffered-character-input-stream-mixin))
  (with-stream-ioblock-input (ioblock stream :speedy t)
    (values
        (%ioblock-peek-char ioblock))))

(defmethod stream-clear-input ((stream buffered-input-stream-mixin))
  (with-stream-ioblock-input (ioblock stream :speedy t)
    (values
     (%ioblock-clear-input ioblock))))

(defmethod stream-unread-char ((stream buffered-character-input-stream-mixin) char)
  (with-stream-ioblock-input (ioblock stream :speedy t)
    (funcall (ioblock-unread-char-function ioblock) ioblock char))
  char)

(defmethod stream-read-byte ((stream buffered-binary-input-stream-mixin))
  (let* ((ioblock (stream-ioblock stream t)))
    (funcall (ioblock-read-byte-function ioblock) ioblock)))

(defmethod stream-read-byte ((stream basic-binary-input-stream))
  (let* ((ioblock (basic-stream-ioblock stream)))
    (funcall (ioblock-read-byte-function ioblock) ioblock)))

(defmethod stream-eofp ((stream buffered-input-stream-mixin))
  (with-stream-ioblock-input (ioblock stream :speedy t)
    (values
     (%ioblock-eofp ioblock))))

(defmethod stream-eofp ((stream basic-input-stream))
  (let* ((ioblock (basic-stream-ioblock stream)))
    (with-ioblock-input-locked (ioblock)
      (%ioblock-eofp ioblock))))

(defmethod stream-listen ((stream buffered-input-stream-mixin))
  (with-stream-ioblock-input (ioblock stream :speedy t)
    (values
     (%ioblock-listen ioblock))))

(defmethod stream-listen ((stream basic-input-stream))
  (let* ((ioblock (basic-stream-ioblock stream)))
    (with-ioblock-input-locked (ioblock)
      (values
       (%ioblock-listen ioblock)))))


(defmethod stream-write-byte ((stream buffered-binary-output-stream-mixin)
                              byte)
  (let* ((ioblock (stream-ioblock stream t)))
    (funcall (ioblock-write-byte-function ioblock) ioblock byte)))

(defmethod stream-write-byte ((stream basic-binary-output-stream) byte)
  (let* ((ioblock (basic-stream-ioblock stream)))
    (funcall (ioblock-write-byte-function ioblock) ioblock byte)))

(defmethod stream-write-char ((stream buffered-character-output-stream-mixin) char)
  (let* ((ioblock (stream-ioblock stream t)))
    (funcall (ioblock-write-char-function ioblock) ioblock char)))

(defmethod stream-write-char ((stream basic-character-output-stream) char)
  (let* ((ioblock (basic-stream-ioblock stream)))
    (funcall (ioblock-write-char-function ioblock) ioblock char)))


(defmethod stream-clear-output ((stream buffered-output-stream-mixin))
  (with-stream-ioblock-output (ioblock stream :speedy t)
    (%ioblock-clear-output ioblock))
  nil)

(defmethod stream-clear-output ((stream basic-output-stream))
  (let* ((ioblock (basic-stream-ioblock stream)))
    (with-ioblock-output-locked (ioblock)
      (%ioblock-clear-output ioblock))
    nil))

(defmethod stream-line-column ((stream buffered-character-output-stream-mixin))
  (let* ((ioblock (stream-ioblock stream nil)))
    (and ioblock (ioblock-charpos ioblock))))

(defmethod stream-line-column ((stream basic-character-output-stream))
  (let* ((ioblock (basic-stream.state stream)))
    (and ioblock (ioblock-charpos ioblock))))



(defmethod stream-set-column ((stream buffered-character-output-stream-mixin)
                              new)
  (let* ((ioblock (stream-ioblock stream nil)))
    (and ioblock (setf (ioblock-charpos ioblock) new))))

(defmethod stream-set-column ((stream basic-character-output-stream)
                              new)
  (let* ((ioblock (basic-stream.state stream)))
    (and ioblock (setf (ioblock-charpos ioblock) new))))

(defmethod stream-force-output ((stream buffered-output-stream-mixin))
  (with-stream-ioblock-output (ioblock stream :speedy t)
    (%ioblock-force-output ioblock nil)
    nil))

(defmethod stream-force-output ((stream basic-output-stream))
  (let* ((ioblock (basic-stream-ioblock stream)))
    (with-ioblock-output-locked (ioblock)
      (%ioblock-force-output ioblock nil)
      nil)))

(defmethod maybe-stream-force-output ((stream buffered-output-stream-mixin))
  (with-stream-ioblock-output-maybe (ioblock stream :speedy t)
    (%ioblock-force-output ioblock nil)
    nil))

(defmethod maybe-stream-force-output ((stream basic-output-stream))
  (let* ((ioblock (basic-stream-ioblock stream)))
    (with-ioblock-output-locked-maybe (ioblock)
      (%ioblock-force-output ioblock nil)
      nil)))

(defmethod stream-finish-output ((stream buffered-output-stream-mixin))
  (with-stream-ioblock-output (ioblock stream :speedy t)
    (%ioblock-force-output ioblock t)
    nil))

(defmethod stream-finish-output ((stream basic-output-stream))
  (let* ((ioblock (basic-stream-ioblock stream)))
    (with-ioblock-output-locked (ioblock)
      (%ioblock-force-output ioblock t)
      nil)))


  
(defmethod stream-write-string ((stream buffered-character-output-stream-mixin)
				string &optional (start 0 start-p) end)
				
  (with-stream-ioblock-output (ioblock stream :speedy t)
    (if (and (typep string 'simple-string)
	     (not start-p))
      (funcall (ioblock-write-simple-string-function ioblock)
                   ioblock string 0 (length string))
      (progn
        (setq end (check-sequence-bounds string start end))
        (locally (declare (fixnum start end))
          (multiple-value-bind (arr offset)
              (if (typep string 'simple-string)
                (values string 0)
                (array-data-and-offset (require-type string 'string)))
            (unless (eql 0 offset)
              (incf start offset)
              (incf end offset))
            (funcall (ioblock-write-simple-string-function ioblock)
                     ioblock arr start (the fixnum (- end start))))))))
  string)

(defmethod stream-write-string ((stream basic-character-output-stream)
				string &optional (start 0 start-p) end)

  (let* ((ioblock (basic-stream-ioblock stream)))
    (with-ioblock-output-locked (ioblock) 
      (if (and (typep string 'simple-string)
               (not start-p))
        (values
         (funcall (ioblock-write-simple-string-function ioblock)
                  ioblock string 0 (length string)))
        (progn
          (setq end (check-sequence-bounds string start end))
          (locally (declare (fixnum start end))
            (multiple-value-bind (arr offset)
                (if (typep string 'simple-string)
                  (values string 0)
                  (array-data-and-offset (require-type string 'string)))
              (unless (eql 0 offset)
                (incf start offset)
                (incf end offset))
              (values
                  (funcall (ioblock-write-simple-string-function ioblock)
                           ioblock arr start (the fixnum (- end start))))))))))
  string)


(defmethod stream-write-ivector ((s buffered-output-stream-mixin)
				 iv start length)
  (with-stream-ioblock-output (ioblock s :speedy t)
    (values    
        (%ioblock-out-ivect ioblock iv start length))))

(defmethod stream-write-ivector ((s basic-output-stream)
				 iv start length)
  (let* ((ioblock (basic-stream-ioblock s)))
    (with-ioblock-output-locked (ioblock)
      (values
          (%ioblock-out-ivect ioblock iv start length)))))


#+bad-idea
(defmethod stream-read-ivector ((s buffered-character-input-stream-mixin)
				iv start nb)
  (with-stream-ioblock-input (ioblock s :speedy t)
    (values
     (%ioblock-character-in-ivect ioblock iv start nb))))

(defmethod stream-read-ivector ((s buffered-binary-input-stream-mixin)
				iv start nb)
  (with-stream-ioblock-input (ioblock s :speedy t)
    (values
     (%ioblock-binary-in-ivect ioblock iv start nb))))


(defmethod stream-write-vector ((stream buffered-character-output-stream-mixin)
				vector start end)
  (declare (fixnum start end))
  (if (not (typep vector 'simple-base-string))
    (call-next-method)
    (with-stream-ioblock-output (ioblock stream :speedy t)
      (let* ((total (- end start)))
	(declare (fixnum total))
        (values
            (funcall (ioblock-write-simple-string-function ioblock)
                     ioblock vector start total))))))

(defmethod stream-write-vector ((stream basic-character-output-stream)
				vector start end)
  (declare (fixnum start end))
  (if (not (typep vector 'simple-base-string))
    (call-next-method)
    (let* ((ioblock (basic-stream-ioblock stream))
           (total (- end start)))
      (declare (fixnum total))
      (with-ioblock-output-locked (ioblock)
        (values
            (funcall (ioblock-write-simple-string-function ioblock)
                     ioblock vector start total))))))

;;; bivalence: we don't actually have a "bivalent stream" class;
;;; all actual (potentially) bivalent streams (sockets) include binary streams
;;; before character streams in their CPLs.  That effectively means that
;;; binary-stream methods for reading and writing sequences have to
;;; handle character I/O in some cases.  That may slow some things down
;;; (at least in theory), but the case where the stream's element-type
;;; matches the sequence's element-type isn't affected.
(defun %ioblock-binary-stream-write-vector (ioblock vector start end)
  (declare (fixnum start end))
  (let* ((out (ioblock-outbuf ioblock))
         (buf (io-buffer-buffer out))
         (written 0)
         (limit (io-buffer-limit out))
         (total (- end start))
         (buftype (typecode buf)))
    (declare (fixnum buftype written total limit))
    (if (not (= (the fixnum (typecode vector)) buftype))
      (if (typep vector 'string)
        (funcall (ioblock-write-simple-string-function ioblock)
                 ioblock
                 vector
                 start
                 (- end start))
        (do* ((i start (1+ i))
              (wbf (ioblock-write-byte-when-locked-function ioblock))
              (wcf (ioblock-write-char-when-locked-function ioblock)))
             ((= i end))
          (let ((byte (uvref vector i)))
            (if (characterp byte)
              (funcall wcf ioblock byte)
              (funcall wbf ioblock byte)))))
      (do* ((pos start (+ pos written))
            (left total (- left written)))
           ((= left 0))
        (declare (fixnum pos left))
        (setf (ioblock-dirty ioblock) t)
        (let* ((index (io-buffer-idx out))
               (count (io-buffer-count out))
               (avail (- limit index)))
          (declare (fixnum index avail count))
          (cond
            ((= (setq written avail) 0)
             (%ioblock-force-output ioblock nil))
            (t
             (if (> written left)
               (setq written left))
             (%copy-ivector-to-ivector
              vector
              (ioblock-elements-to-octets ioblock pos)
              buf
              (ioblock-elements-to-octets ioblock index)
              (ioblock-elements-to-octets ioblock written))
             (setf (ioblock-dirty ioblock) t)
             (incf index written)
             (if (> index count)
               (setf (io-buffer-count out) index))
             (setf (io-buffer-idx out) index)
             (if (= index  limit)
               (%ioblock-force-output ioblock nil)))))))))

(defmethod stream-write-vector ((stream buffered-binary-output-stream-mixin)
				vector start end)
  (with-stream-ioblock-output (ioblock stream :speedy t)
    (%ioblock-binary-stream-write-vector ioblock vector start end)))


(defmethod stream-write-vector ((stream basic-binary-output-stream)
				vector start end)
  (declare (fixnum start end))
  (let* ((ioblock (basic-stream-ioblock stream)))
    (with-ioblock-output-locked (ioblock)
      (%ioblock-binary-stream-write-vector ioblock vector start end))))



(defmethod stream-read-vector ((stream basic-binary-input-stream)
			       vector start end)
  (declare (fixnum start end))
  (if (typep vector 'simple-base-string)
    (call-next-method)
    (let* ((ioblock (basic-stream-ioblock stream)))
      (with-ioblock-input-locked (ioblock)
        (values
            (%ioblock-binary-read-vector ioblock vector start end))))))

(defmethod stream-read-vector ((stream buffered-character-input-stream-mixin)
			       vector start end)
  (declare (fixnum start end))
  (if (not (typep vector 'simple-base-string))
    (call-next-method)
    (with-stream-ioblock-input (ioblock stream :speedy t)
      (values
       (funcall (ioblock-character-read-vector-function ioblock)
                ioblock vector start end)))))



(defmethod stream-read-vector ((stream buffered-binary-input-stream-mixin)
			       vector start end)
  (declare (fixnum start end))
  (if (typep vector 'simple-base-string)
    (call-next-method)
    (with-stream-ioblock-input (ioblock stream :speedy t)
      (values
       (%ioblock-binary-read-vector ioblock vector start end)))))



(defloadvar *fd-set-size*
    (ff-call (%kernel-import target::kernel-import-fd-setsize-bytes)
             :unsigned-fullword))

(defun unread-data-available-p (fd)
  #+(or freebsd-target windows-target)
  (fd-input-available-p fd 0)
  #-(or freebsd-target windows-target)
  (rlet ((arg (* :char) (%null-ptr)))
    (when (zerop (int-errno-call (#_ioctl fd #$FIONREAD :address arg)))
      (let* ((avail (pref arg :long)))
	(and (> avail 0) avail)))))

;;; Read and discard any available unread input.
(defun %fd-drain-input (fd)
  (%stack-block ((buf 1024))
    (do* ((avail (unread-data-available-p fd) (unread-data-available-p fd)))
	 ((or (null avail) (eql avail 0)))
      (do* ((max (min avail 1024) (min avail 1024)))
	   ((zerop avail))
	(let* ((count (fd-read fd buf max)))
	  (if (< count 0)
	    (return)
	    (decf avail count)))))))

(defun fd-zero (fdset)
  (ff-call (%kernel-import target::kernel-import-do-fd-zero)
           :address fdset
           :void))

(defun fd-set (fd fdset)
  (ff-call (%kernel-import target::kernel-import-do-fd-set)
           :unsigned-fullword fd
           :address fdset
           :void))

(defun fd-clr (fd fdset)
  (ff-call (%kernel-import target::kernel-import-do-fd-clr)
           :unsigned-fullword fd
           :address fdset
           :void))

(defun fd-is-set (fd fdset)
  (not (= 0 (the fixnum (ff-call (%kernel-import target::kernel-import-do-fd-is-set)
                                 :unsigned-fullword fd
                                 :address fdset
                                 :unsigned-fullword)))))

(defun process-input-would-block (fd)
  #+windows-target (declare (ignore fd))
  #+windows-target t
  #-windows-target
  (if (logtest #$O_NONBLOCK (the fixnum (fd-get-flags fd)))
    (process-input-wait fd)
    (- #$ETIMEDOUT)))
    
(defun process-input-wait (fd &optional timeout)
  "Wait until input is available on a given file-descriptor."
  (rlet ((now :timeval))
    (let* ((wait-end 
            (when timeout
              (gettimeofday now)
              (+ (timeval->milliseconds now) timeout))))
      (loop
        (multiple-value-bind (win error)
            (fd-input-available-p fd (or timeout -1))
          (when win
            (return (values t nil nil)))
          (when (eql error 0)         ;timed out
            (return (values nil t nil)))
          ;; If it returned and a timeout was specified, check
          ;; to see if it's been exceeded.  If so, return NIL;
          ;; otherwise, adjust the remaining timeout.
          ;; If there was no timeout, continue to wait forever.
          (unless (eql error (- #$EINTR))
            (return (values nil nil error)))
          (when timeout
            (gettimeofday now)
            (setq timeout (- wait-end (timeval->milliseconds now)))
            (if (<= timeout 0)
              (return (values nil t nil)))))))))


(defun process-output-would-block (fd)
  #+windows-target (declare (ignore fd))
  #+windows-target t
  #-windows-target
  (if (logtest #$O_NONBLOCK (the fixnum (fd-get-flags fd)))
    (process-output-wait fd)
    (- #$ETIMEDOUT)))

(defun process-output-wait (fd &optional timeout)
  "Wait until output is possible on a given file descriptor."
  (rlet ((now :timeval))
    (let* ((wait-end 
            (when timeout
              (gettimeofday now)
              (+ (timeval->milliseconds now) timeout))))
      (loop
        (multiple-value-bind (win error)
            (fd-ready-for-output-p fd (or timeout -1))
          (when win
            (return (values t nil nil)))
          (when (eql error 0)
            (return (values nil t nil)))
          (unless (eql error (- #$EINTR))
            (return (values nil nil error)))
          ;; If it returned and a timeout was specified, check
          ;; to see if it's been exceeded.  If so, return NIL;
          ;; otherwise, adjust the remaining timeout.
          ;; If there was no timeout, continue to wait forever.
          (when timeout
            (gettimeofday now)
            (setq timeout (- wait-end (timeval->milliseconds now)))
            (if (<= timeout 0)
              (return (values nil t nil)))))))))



(defun ticks-to-timeval (ticks tv)
  (when ticks
    (let* ((total-us (* ticks (/ 1000000 *ticks-per-second*))))
      (multiple-value-bind (seconds us) (floor total-us 1000000)
	(setf (pref tv :timeval.tv_sec) seconds
	      (pref tv :timeval.tv_usec) us)))))

(defun fd-input-available-p (fd &optional milliseconds)
  #+windows-target
  (case (%unix-fd-kind fd)
    (:socket
     (rlet ((infds #>fd_set)
            (tv :timeval :tv_sec 0 :tv_usec 0))
       (fd-zero infds)
       (fd-set fd infds)
       (when milliseconds
         (multiple-value-bind (seconds millis)
             (floor milliseconds 1000)
        (setf (pref tv :timeval.tv_sec) seconds
              (pref tv :timeval.tv_usec) (* 1000 millis))))
       (let* ((result (#_select 1 infds (%null-ptr) (%null-ptr) (if milliseconds tv (%null-ptr)))))
         (cond ((> result 0) (values t 0))
               ((= result 0) (values nil 0))
               (t (values nil (- (#_GetLastError))))))))
    (:pipe (if (data-available-on-pipe-p fd)
             (values t 0)
             (if (and milliseconds (> milliseconds 0))
               (values (process-wait-with-timeout "input-wait" milliseconds #'data-available-on-pipe-p fd) 0)
               (values nil 0))))
    (:file (let* ((curpos (fd-tell fd))
                  (eofpos (%stack-block ((peofpos 8))
                            (#_GetFileSizeEx (%int-to-ptr fd) peofpos)
                            (%%get-unsigned-longlong peofpos 0))))
             (values (< curpos eofpos) 0)))
    ;;(:character-special (windows-tty-input-available-p fd milliseconds))

    (t (values nil 0)))
  #-windows-target
  (rlet ((pollfds (:array (:struct :pollfd) 1)))
    (setf (pref (paref pollfds (:* (:struct :pollfd)) 0) :pollfd.fd) fd
          (pref (paref pollfds (:* (:struct :pollfd)) 0) :pollfd.events) #$POLLIN)
    (let* ((res (int-errno-call (#_poll pollfds 1 (or milliseconds -1)))))
      (declare (fixnum res))
      (values (> res 0) res))))


(defun fd-ready-for-output-p (fd &optional milliseconds)
  #+windows-target
  (case (%unix-fd-kind fd)
    (:socket
     (rlet ((tv :timeval :tv_sec 0 :tv_usec 0)
            (outfds :fd_set))
       (fd-zero outfds)
       (fd-set fd outfds)
       (when milliseconds
         (multiple-value-bind (seconds millis)
             (floor milliseconds 1000)
           (setf (pref tv #>timeval.tv_sec) seconds
                 (pref tv #>timeval.tv_usec) (* millis 1000))))
       (let* ((res (#_select 1 (%null-ptr) outfds (%null-ptr) (if milliseconds tv (%null-ptr)))))
         (cond ((> res 0) (values t 0))
               ((= res 0) (values nil 0))
               (t (values 0 (- (#_GetLastError))))))))
    (t (values t 0)))
  #-windows-target
  (rlet ((pollfds (:array (:struct :pollfd) 1)))
    (setf (pref (paref pollfds (:* (:struct :pollfd)) 0) :pollfd.fd) fd
          (pref (paref pollfds (:* (:struct :pollfd)) 0) :pollfd.events) #$POLLOUT)
    (let* ((res (int-errno-call (#_poll pollfds 1 (or milliseconds -1)))))
      (declare (fixnum res))
      (values (> res 0)  res))))



;;; FD-streams, built on top of the ioblock mechanism.
(defclass fd-stream (buffered-stream-mixin fundamental-stream) ())


(defmethod select-stream-advance-function ((s symbol) direction)
  (select-stream-advance-function (find-class s) direction))

(defmethod select-stream-advance-function ((c class) direction)
  (select-stream-advance-function (class-prototype c) direction))

(defmethod select-stream-advance-function ((s fd-stream) (direction t))
  'fd-stream-advance)

(defmethod select-stream-advance-function ((s basic-stream) (direction t))
  'fd-stream-advance)


(defmethod select-stream-force-output-function ((s symbol) direction)
  (select-stream-force-output-function (find-class s) direction))

(defmethod select-stream-force-output-function ((c class) direction)
  (select-stream-force-output-function (class-prototype c) direction))

(defmethod select-stream-force-output-function ((f fd-stream) (direction t))
  'fd-stream-force-output)

(defmethod select-stream-force-output-function ((f basic-stream) (direction t))
  'fd-stream-force-output)

(defmethod print-object ((s fd-stream) out)
  (print-unreadable-object (s out :type t :identity t)
    (let* ((ioblock (stream-ioblock s nil))
           (fd (and ioblock (ioblock-device ioblock)))
           (encoding (and ioblock (encoding-name (ioblock-encoding ioblock)))))
      (if fd
        (format out "~s (~a/~d)" encoding (%unix-fd-kind fd) fd)
        (format out "~s" :closed)))))

(defclass fd-input-stream (fd-stream buffered-input-stream-mixin)
    ())

(defclass fd-output-stream (fd-stream buffered-output-stream-mixin)
    ())

(defclass fd-io-stream (fd-stream buffered-io-stream-mixin)
    ())

(defclass fd-character-input-stream (fd-input-stream
                                     buffered-character-input-stream-mixin)
    ())

(defclass fd-character-output-stream (fd-output-stream
                                      buffered-character-output-stream-mixin)
    ())

(defclass fd-character-io-stream (fd-io-stream
                                  buffered-character-io-stream-mixin)
    ())

(defclass fd-binary-input-stream (fd-input-stream
                                  buffered-binary-input-stream-mixin)
    ())

(defclass fd-binary-output-stream (fd-output-stream
                                   buffered-binary-output-stream-mixin)
    ())

(defclass fd-binary-io-stream (fd-io-stream buffered-binary-io-stream-mixin)
    ())

(defun fd-stream-advance (s ioblock read-p)
  (let* ((fd (ioblock-device ioblock))
         (buf (ioblock-inbuf ioblock))
         (bufptr (io-buffer-bufptr buf))
         (size (io-buffer-size buf))
         (avail nil))
    (setf (io-buffer-idx buf) 0
          (io-buffer-count buf) 0
          (ioblock-eof ioblock) nil)
      (when (or read-p (setq avail (fd-input-available-p fd 0)))
        (unless avail
          (let* ((deadline (ioblock-deadline ioblock))
                 (timeout
                  (if deadline
                    (milliseconds-until-deadline deadline ioblock)
                    (ioblock-input-timeout ioblock))))
            (when timeout
              (multiple-value-bind (win timedout error)
                  (process-input-wait fd timeout)
                (unless win
                  (if timedout
                    (error (if deadline
                             'communication-deadline-expired
                             'input-timeout)
                           :stream s)
                    (stream-io-error s (- error) "read")))))))
        (let* ((n (with-eagain fd :input
		    (fd-read fd bufptr size))))
          (declare (fixnum n))
          (if (< n 0)
            (stream-io-error s (- n) "read")
            (if (> n 0)
              (setf (io-buffer-count buf)
		    (ioblock-octets-to-elements ioblock n))
              (progn (setf (ioblock-eof ioblock) t)
                     nil)))))))

(defun fd-stream-eofp (s ioblock)
  (declare (ignore s))
  (ioblock-eof ioblock))
  
(defun fd-stream-listen (s ioblock)
  (if (interactive-stream-p s)
    (unread-data-available-p (ioblock-device ioblock))
    (not (fd-stream-eofp s ioblock))))

(defun fd-stream-close (s ioblock)
  (cancel-terminate-when-unreachable s)
  (when (ioblock-dirty ioblock)
    (stream-force-output s))
  (let* ((fd (ioblock-device ioblock)))
    (when fd
      (setf (ioblock-device ioblock) nil)
      (if (>= fd 0) (fd-close fd)))))

(defun fd-stream-force-output (s ioblock count finish-p)
  (when (or (ioblock-dirty ioblock) finish-p)
    (setf (ioblock-dirty ioblock) nil)
    (let* ((fd (ioblock-device ioblock))
	   (io-buffer (ioblock-outbuf ioblock))
	   (buf (%null-ptr))
	   (octets-to-write (ioblock-elements-to-octets ioblock count))
	   (octets octets-to-write))
      (declare (fixnum octets))
      (declare (dynamic-extent buf))
      (%setf-macptr buf (io-buffer-bufptr io-buffer))
      (setf (io-buffer-idx io-buffer) 0
	    (io-buffer-count io-buffer) 0)
      (do* ()
	   ((= octets 0)
	    (when finish-p
	      (case (%unix-fd-kind fd)
		(:file (fd-fsync fd))))
	    octets-to-write)
        (let* ((deadline (ioblock-deadline ioblock))
               (timeout
                (if deadline
                  (milliseconds-until-deadline deadline ioblock)
                  (ioblock-output-timeout ioblock))))
          (when timeout
            (multiple-value-bind (win timedout error)
                (process-output-wait fd timeout)
              (unless win
                (if timedout
                  (error (if deadline
                           'communication-deadline-expired
                           'output-timeout)
                         :stream s)
                  (stream-io-error s (- error) "write"))))))
	(let* ((written (with-eagain fd :output
			  (fd-write fd buf octets))))
	  (declare (fixnum written))
	  (if (< written 0)
	    (stream-io-error s (- written) "write"))
	  (decf octets written)
	  (unless (zerop octets)
	    (%incf-ptr buf written)))))))

(defmethod stream-read-line ((s buffered-input-stream-mixin))
   (with-stream-ioblock-input (ioblock s :speedy t)
     (funcall (ioblock-read-line-function ioblock) ioblock)))

(defmethod stream-clear-input ((s fd-input-stream))
  (call-next-method)
  (with-stream-ioblock-input (ioblock s :speedy t)
    (let* ((fd (ioblock-device ioblock)))
      (when fd (%fd-drain-input fd)))))

(defmethod select-stream-class ((class (eql 'fd-stream)) in-p out-p char-p)
  (if char-p
    (if in-p
      (if out-p
	'fd-character-io-stream
	'fd-character-input-stream)
      'fd-character-output-stream)
    (if in-p
      (if out-p
	'fd-binary-io-stream
	'fd-binary-input-stream)
      'fd-binary-output-stream)))

(defstruct (input-selection (:include dll-node))
  (package nil :type (or null string package))
  (source-file nil :type (or null string pathname))
  (string-stream nil :type (or null string-input-stream)))

(defstruct (input-selection-queue (:include locked-dll-header)))

(defclass selection-input-stream (fd-character-input-stream)
    ((package :initform nil :reader selection-input-stream-package)
     (pathname :initform nil :reader selection-input-stream-pathname)
     (peer-fd  :reader selection-input-stream-peer-fd)))

(defmethod select-stream-class ((class (eql 'selection-input-stream))
                                in-p out-p char-p)
  (if (and in-p char-p (not out-p))
    'selection-input-stream
    (error "Can't create that type of stream.")))

(defun make-selection-input-stream (fd &key peer-fd encoding)
  (let* ((s (make-fd-stream fd
                            :class 'selection-input-stream
                            :sharing :lock
                            :encoding encoding)))
    (setf (slot-value s 'peer-fd) peer-fd)
    s))


;;; Very simple protocol:
;;; ^ppackage-name#\newline
;;; ^vpathname#\newline
;;; ^q quotes next character
;;; else raw data
(defmethod stream-read-char ((s selection-input-stream))
  (with-slots (package pathname) s
    (let* ((quoted nil))
      (loop
        (let* ((ch (call-next-method)))
          (if quoted
            (return ch)
            (case ch
              (#\^p (setq package nil)
                    (let* ((p (read-line s nil nil)))
                      (unless (zerop (length p))
                        (setq package p))))
              (#\^v (setq pathname nil)
                    (let* ((p (read-line s nil nil)))
                      (unless (zerop (length p))
                        (setq pathname p))))
              (#\^q (setq quoted t))
              (t (return ch)))))))))

(defmethod stream-peek-char ((s selection-input-stream))
  (let* ((ch (stream-read-char s)))
    (unless (eq ch :eof)
      (stream-unread-char s ch))
    ch))

(defmethod stream-read-line ((s selection-input-stream))
  (generic-read-line s))

(defmethod stream-read-list ((stream selection-input-stream)
			     list count)
  (generic-character-read-list stream list count))

(defmethod stream-read-vector ((stream selection-input-stream)
			       vector start end)
  (generic-character-read-vector stream vector start end))


;;;File streams.

(let* ((open-file-streams ())
       (open-file-streams-lock (make-lock)))
  (defun open-file-streams ()
    (with-lock-grabbed (open-file-streams-lock)
      (copy-list open-file-streams)))
  (defun note-open-file-stream (f)
    (with-lock-grabbed (open-file-streams-lock)
      (push f open-file-streams))
    t)
  (defun remove-open-file-stream (f)
    (with-lock-grabbed (open-file-streams-lock)
      (setq open-file-streams (nremove f open-file-streams)))
    t)
  (defun clear-open-file-streams ()
    (with-lock-grabbed (open-file-streams-lock)
      (setq open-file-streams nil))))
            

(defun open (filename &key (direction :input)
                      (element-type 'base-char)
                      (if-exists (if (eq (pathname-version filename) :newest)
                                   :new-version
                                   :error))
                      (if-does-not-exist (cond ((eq direction :probe)
                                                nil)
                                               ((or (eq direction :input)
                                                    (eq if-exists :overwrite)
                                                    (eq if-exists :append))
                                                :error)
                                               (t :create)))
                      (external-format :default)
		      (class 'file-stream)
                      (sharing :private)
                      (basic t))
  "Return a stream which reads from or writes to FILENAME.
  Defined keywords:
   :DIRECTION - one of :INPUT, :OUTPUT, :IO, or :PROBE
   :ELEMENT-TYPE - the type of object to read or write, default BASE-CHAR
   :IF-EXISTS - one of :ERROR, :NEW-VERSION, :RENAME, :RENAME-AND-DELETE,
                       :OVERWRITE, :APPEND, :SUPERSEDE or NIL
   :IF-DOES-NOT-EXIST - one of :ERROR, :CREATE or NIL
  See the manual for details."
  (loop
    (restart-case
      (return
	(make-file-stream filename
			  direction
			  element-type
			  if-exists
			  if-does-not-exist
			  class
			  external-format
                          sharing
                          basic))
      (retry-open ()
                  :report (lambda (stream) (format stream "Retry opening ~s" filename))
                  nil))))





(defun gen-file-name (path)
  (let* ((base (random (ash target::target-most-positive-fixnum -1)))
         (tem-path (merge-pathnames (make-pathname :name (%integer-to-string base) :type "tem" :defaults nil) path)))
    (loop
      (when (%create-file tem-path :if-exists nil) (return tem-path))      
      (setf (%pathname-name tem-path) (%integer-to-string (setq base (1+ base)))))))

(defun probe-file-x (path)
  (%probe-file-x (native-translated-namestring path)))

(defun file-length (stream)
  (typecase stream
    ;; Don't use an OR type here
    (file-stream (stream-length stream))
    (synonym-stream (file-length
		     (symbol-value (synonym-stream-symbol stream))))
    (broadcast-stream (let* ((last (last-broadcast-stream stream)))
			(if last
			  (file-length last)
			  0)))
    (otherwise (report-bad-arg stream 'file-stream))))
  
(defun file-position (stream &optional position)
  (when position
    (if (eq position :start)
      (setq position 0)
      (if (eq position :end)
	(setq position (file-length stream))
	(unless (typep position 'unsigned-byte)
	  (report-bad-arg position '(or
				     null
				     (eql :start)
				     (eql :end)
				     unsigned-byte))))))
  (stream-position stream position))


(defun %request-terminal-input ()
  (let* ((shared-resource
	  (if (typep *terminal-io* 'two-way-stream)
	    (input-stream-shared-resource
	     (two-way-stream-input-stream *terminal-io*)))))
    (if shared-resource (%acquire-shared-resource shared-resource t))))




(defun %%yield-terminal-to (&optional process)
  (let* ((stream (if (typep *terminal-io* 'synonym-stream)
                   (symbol-value (synonym-stream-symbol *terminal-io*))
                   *terminal-io*))
         (shared-resource
	  (if (typep stream 'two-way-stream)
	    (input-stream-shared-resource
	     (two-way-stream-input-stream stream)))))
    (when shared-resource (%yield-shared-resource shared-resource process))))

(defun %restore-terminal-input (&optional took-it)
  (let* ((shared-resource
	  (if took-it
	    (if (typep *terminal-io* 'two-way-stream)
	      (input-stream-shared-resource
	       (two-way-stream-input-stream *terminal-io*))))))
    (when shared-resource
      (%release-shared-resource shared-resource))))

;;; Initialize the global streams
;;; These are defparameters because they replace the ones that were in l1-init
;;; while bootstrapping.

(defparameter *terminal-io* nil "terminal I/O stream")
(defparameter *debug-io* nil "interactive debugging stream")
(defparameter *query-io* nil "query I/O stream")
(defparameter *error-output* nil "error output stream")
(defparameter *standard-input* nil "default input stream")
(defparameter *standard-output* nil "default output stream")
(defparameter *trace-output* nil "trace output stream")

(proclaim '(stream 
          *query-io* *debug-io* *error-output* *standard-input* 
          *standard-output* *trace-output*))

;;; Interaction with the REPL.  READ-TOPLEVEL-FORM should return 3
;;; values: a form, a (possibly null) pathname, and a boolean that
;;; indicates whether or not the result(s) of evaluating the form
;;; should be printed.  (The last value has to do with how selections
;;; that contain multiple forms are handled; see *VERBOSE-EVAL-SELECTION*
;;; and the SELECTION-INPUT-STREAM method below.)

(defmethod read-toplevel-form ((stream synonym-stream) &rest keys)
  (apply #'read-toplevel-form (symbol-value (synonym-stream-symbol stream)) keys))

(defmethod read-toplevel-form ((stream two-way-stream) &rest keys)
  (if (typep stream 'echo-stream)
    (call-next-method)
    (apply #'read-toplevel-form (two-way-stream-input-stream stream) keys)))

(defmethod read-toplevel-form :after ((stream echoing-two-way-stream) &key &allow-other-keys)
  (stream-set-column (two-way-stream-output-stream stream) 0))

(defmethod read-toplevel-form ((stream input-stream) &key eof-value file-name start-offset map)
  (loop
    (let* ((*in-read-loop* nil)
           (first-char (peek-char t stream nil eof-value))
           (form
            (let ((*read-suppress* nil))
              (cond ((eq first-char #\:)
                     (read-command-or-keyword stream eof-value))
                    ((eq first-char eof-value) eof-value)
                    (t (multiple-value-bind (form note)
			   (read-recording-source stream :eofval eof-value
						  :file-name file-name
						  :start-offset start-offset
						  :map map
						  :save-source-text t)
			 (setq *loading-toplevel-location* note)
			 form))))))
      (if (eq form eof-value)
        (return (values form nil t))
        (progn
          (let ((ch))                   ;Trim whitespace
            (while (and (listen stream)
                        (setq ch (read-char stream nil nil))
                        (whitespacep cH))
              (setq ch nil))
            (when ch (unread-char ch stream)))
          (when *listener-indent* 
            (write-char #\space stream)
            (write-char #\space stream))
          (return (values (process-single-selection form) nil t)))))))

(defparameter *verbose-eval-selection* nil
  "When true, the results of evaluating all forms in an input selection
are printed.  When false, only the results of evaluating the last form
are printed.")

(defmethod read-toplevel-form ((stream selection-input-stream)
                               &key eof-value &allow-other-keys)
  (if (eq (stream-peek-char stream) :eof)
    (values eof-value nil t)
    (let* ((*package* *package*)
           (pkg-name (selection-input-stream-package stream)))
      (when pkg-name (setq *package* (pkg-arg pkg-name)))
      (let* ((form (call-next-method))
             (last-form-in-selection (not (listen stream))))
        (values form
                (selection-input-stream-pathname stream)
                (or last-form-in-selection *verbose-eval-selection*))))))


(defun (setf %ioblock-external-format) (ef ioblock)
  (let* ((encoding (get-character-encoding (external-format-character-encoding ef)))
         (line-termination (external-format-line-termination ef)))
    (when (eq encoding (get-character-encoding nil))
      (setq encoding nil))
    (setq line-termination (cdr (assoc line-termination
                                       *canonical-line-termination-conventions*)))
    (setf (ioblock-encoding ioblock) encoding)
    (when (ioblock-inbuf ioblock)
      (setup-ioblock-input ioblock t (ioblock-element-type ioblock) (ioblock-sharing ioblock) encoding line-termination))
    (when (ioblock-outbuf ioblock)
      (setup-ioblock-output ioblock t (ioblock-element-type ioblock) (ioblock-sharing ioblock) encoding line-termination))
    ef))

(defmethod stream-external-format ((s basic-character-stream))
  (%ioblock-external-format (basic-stream-ioblock s)))

(defmethod (setf stream-external-format) (new (s basic-character-stream))
  (setf (%ioblock-external-format (basic-stream-ioblock s))
        (normalize-external-format (stream-domain s) new)))

(defmethod stream-external-format ((s buffered-stream-mixin))
  (%ioblock-external-format (stream-ioblock s t)))

(defmethod (setf stream-external-format) (new (s buffered-stream-mixin))
  (setf (%ioblock-external-format (stream-ioblock s t))
        (normalize-external-format (stream-domain s) new)))

(defmethod stream-input-timeout ((s basic-input-stream))
  (let* ((ioblock (basic-stream-ioblock s)))
    (with-ioblock-input-locked (ioblock)
      (let* ((timeout (ioblock-input-timeout ioblock)))
        (when timeout
          (values (floor timeout 1000.0)))))))

(defmethod (setf stream-input-timeout) (new (s basic-input-stream))
  (setq new (check-io-timeout new))
  (let* ((ioblock (basic-stream-ioblock s)))
    (with-ioblock-input-locked (ioblock)
      (setf (ioblock-input-timeout ioblock)
            (if new (round (* new 1000))))
      new)))

(defmethod stream-output-timeout ((s basic-output-stream))
  (let* ((ioblock (basic-stream-ioblock s)))
    (with-ioblock-output-locked (ioblock)
      (let* ((timeout (ioblock-output-timeout ioblock)))
        (when timeout
          (values (floor timeout 1000.0)))))))

(defmethod (setf stream-output-timeout) (new (s basic-output-stream))
  (setq new (check-io-timeout new))
  (let* ((ioblock (basic-stream-ioblock s)))
    (with-ioblock-output-locked (ioblock)
      (setf (ioblock-output-timeout ioblock)
            (if new (round (* new 1000))))
      new)))

(defmethod stream-deadline ((s basic-output-stream))
  (let* ((ioblock (basic-stream-ioblock s)))
    (with-ioblock-output-locked (ioblock)
      (ioblock-deadline ioblock))))
 
(defmethod (setf stream-deadline) (new (s basic-output-stream))
  (let* ((ioblock (basic-stream-ioblock s)))
    (with-ioblock-output-locked (ioblock)
      (setf (ioblock-deadline ioblock) new)
      new)))



(defmethod stream-input-timeout ((s buffered-input-stream-mixin))
  (let* ((ioblock (stream-ioblock s t)))
    (with-ioblock-input-locked (ioblock)
      (let* ((timeout (ioblock-input-timeout ioblock)))
        (when timeout
          (values (floor timeout 1000.0)))))))

(defmethod (setf stream-input-timeout) (new (s buffered-input-stream-mixin))
  (setq new (check-io-timeout new))
  (let* ((ioblock (stream-ioblock s t)))
    (with-ioblock-input-locked (ioblock)
      (setf (ioblock-input-timeout ioblock)
            (if new (round (* new 1000))))
      new)))

(defmethod stream-output-timeout ((s buffered-output-stream-mixin))
  (let* ((ioblock (stream-ioblock s t)))
    (with-ioblock-output-locked (ioblock)
      (let* ((timeout (ioblock-output-timeout ioblock)))
        (when timeout
          (values (floor timeout 1000.0)))))))

(defmethod (setf stream-output-timeout) (new (s buffered-output-stream-mixin))
  (setq new (check-io-timeout new))
  (let* ((ioblock (stream-ioblock s t)))
    (with-ioblock-output-locked (ioblock)
      (setf (ioblock-output-timeout ioblock)
            (if new (round (* new 1000))))
      new)))

(defmethod stream-deadline ((s buffered-output-stream-mixin))
  (let* ((ioblock (stream-ioblock s t)))
    (with-ioblock-output-locked (ioblock)
      (ioblock-deadline ioblock))))
 
(defmethod (setf stream-deadline) (new (s buffered-output-stream-mixin))
  (let* ((ioblock (stream-ioblock s t)))
    (with-ioblock-output-locked (ioblock)
      (setf (ioblock-deadline ioblock) new)
      new)))


(defmethod select-stream-untyi-function ((s symbol) direction)
  (select-stream-untyi-function (find-class s) direction))

(defmethod select-stream-untyi-function ((c class) direction)
  (select-stream-untyi-function (class-prototype c) direction))

(defmethod select-stream-untyi-function ((s fd-stream) (direction t))
  '%ioblock-untyi)

(defmethod select-stream-untyi-function ((s basic-stream) (direction t))
  '%ioblock-untyi)




(defparameter *vector-output-stream-default-initial-allocation* 64 "Default size of the vector created by (MAKE-VECTOR-OUTPUT-STREAM), in octets.")

;;; Bivalent vector streams.
(make-built-in-class 'vector-stream 'basic-binary-stream 'basic-character-stream)

(defmethod print-object ((s vector-stream) out)
  (print-unreadable-object (s out :type t :identity t)
    (unless (open-stream-p s)  (format out " ~s" :closed))))


(defstruct (vector-stream-ioblock (:include ioblock))
  (displacement 0)                      ;displaced-index-offset
  )

(defstruct (vector-output-stream-ioblock (:include vector-stream-ioblock))
  (line-length 80)                      ;for pretty-printer 
  displaced                             ;original vector if fill-pointer case
 )

(defstatic *vector-output-stream-class* (make-built-in-class 'vector-output-stream 'vector-stream 'basic-binary-output-stream 'basic-character-output-stream))
(defstatic *vector-output-stream-class-wrapper* (%class-own-wrapper *vector-output-stream-class*))
(defstatic *vector-input-stream-class* (make-built-in-class 'vector-input-stream 'vector-stream 'basic-binary-input-stream 'basic-character-input-stream))
(defstatic *vector-input-stream-class-wrapper* (%class-own-wrapper *vector-input-stream-class*))

(defmethod initialize-basic-stream :after ((s vector-stream) &key ioblock &allow-other-keys)
  (setf (basic-stream.state s) ioblock))

(defmethod stream-force-output ((s vector-output-stream)))

(defmethod stream-finish-output ((s vector-output-stream)))



(defun %extend-vector-output-stream (s ioblock count finish-p)
  (declare (ignore s count finish-p))
  (check-ioblock-owner ioblock)
  (let* ((displaced (vector-output-stream-ioblock-displaced ioblock))
         (outbuf (ioblock-outbuf ioblock)))
    (cond (displaced
           (let* ((flags (%svref displaced target::arrayH.flags-cell)))
             (declare (fixnum flags))
             (unless (logbitp $arh_adjp_bit flags)
               (%err-disp $XMALADJUST displaced))
             (let* ((len (%svref displaced target::vectorH.physsize-cell))
                    (newlen (max (the fixnum (+ len len)) (+ len *vector-output-stream-default-initial-allocation*)))
                    (new (%alloc-misc newlen target::subtag-u8-vector)))
               (declare (fixnum len newlen)
                        ((simple-array (unsigned-byte 8) (*)) new))
               (multiple-value-bind (data offset)
                   (%array-header-data-and-offset displaced)
                 (declare ((simple-array (unsigned-byte 8) (*)) data)
                          (fixnum offset))
                 (%copy-ivector-to-ivector data 0 new offset len)
                 (setf (vector-output-stream-ioblock-displacement ioblock) 0)
                 (unless (= 0 offset)
                   (setf (io-buffer-idx outbuf) len
                         (io-buffer-count outbuf) len))
                 (setf (io-buffer-limit outbuf) newlen
                       (io-buffer-size outbuf) newlen
                       (io-buffer-buffer outbuf) new)
                 ;; Adjust the displaced vector.
                 (setf (%svref displaced target::vectorH.data-vector-cell) new
                       (%svref displaced target::vectorH.displacement-cell) 0
                       (%svref displaced target::vectorH.physsize-cell) newlen
                       (%svref displaced target::vectorH.flags-cell) (bitclr $arh_exp_disp_bit flags)
                       (%svref displaced target::vectorH.logsize-cell) len)))))
          (t
           ;; Simpler. Honest.
           (let* ((old (io-buffer-buffer outbuf))
                  (len (length old))
                  (newlen (max (the fixnum (+ len len)) 16))
                  (new (%alloc-misc newlen target::subtag-u8-vector)))
             (declare (fixnum len newlen)
                      ((simple-array (unsigned-byte 8) (*)) old new))
             (%copy-ivector-to-ivector old 0 new 0 len)
             (setf (io-buffer-buffer outbuf) new
                   (io-buffer-size outbuf) newlen
                   (io-buffer-limit outbuf) newlen))))))

(defun %vector-output-stream-close (s ioblock)
  (declare (ignore s))
  ;; If there's a displaced vector, fix its fill pointer.
  (let* ((displaced (vector-output-stream-ioblock-displaced ioblock)))
    (when displaced
      (setf (%svref displaced target::vectorH.logsize-cell)
            (the fixnum (- (the fixnum (io-buffer-count (ioblock-outbuf ioblock)))
                           (the fixnum (vector-output-stream-ioblock-displacement ioblock))))))))

(defmethod stream-line-length ((s vector-output-stream))
  (let* ((ioblock (basic-stream-ioblock s)))
    (string-output-stream-ioblock-line-length ioblock)))

(defmethod (setf stream-line-length) (newlen (s vector-output-stream))
  (let* ((ioblock (basic-stream-ioblock s)))
    (setf (vector-output-stream-ioblock-line-length ioblock) newlen)))

(defun get-output-stream-vector (s)
  (unless (and (typep s 'basic-stream)
               (eq *vector-output-stream-class-wrapper*
                   (basic-stream.wrapper s)))
    (report-bad-arg s 'vector-output-stream))
  (let* ((ioblock (basic-stream-ioblock s))
         (outbuf (progn
                   (check-ioblock-owner ioblock)
                   (ioblock-outbuf ioblock)))
         (v (io-buffer-buffer outbuf))
         (offset (vector-output-stream-ioblock-displacement ioblock))
         (len (the fixnum (- (the fixnum (io-buffer-count outbuf)) offset)))
         (new (%alloc-misc len target::subtag-u8-vector)))
    (declare (fixnum offset len))
    (%copy-ivector-to-ivector v offset new 0 len)
    (setf (io-buffer-idx outbuf) offset
          (io-buffer-count outbuf) offset)
    new))


(defmethod unsigned-integer-to-binary (value len (s binary-output-stream))
  (unless (typep value 'unsigned-byte)
    (report-bad-arg value 'unsigned-byte))
  (do* ((shift (ash (1- len) 3) (- shift 8)))
       ((< shift 0) value)
    (write-byte (logand #xff (ash value (- shift))) s)))

(defun %unsigned-integer-to-binary (value len s)
  (declare (fixnum len))
  (unless (and (typep s 'basic-stream)
               (eq *vector-output-stream-class-wrapper*
                   (basic-stream.wrapper s)))
    (report-bad-arg s 'vector-input-stream))
  (let* ((ioblock (basic-stream-ioblock s))
         (outbuf (progn
                   (check-ioblock-owner ioblock)
                   (ioblock-outbuf ioblock)))
         (idx (io-buffer-idx outbuf))
         (limit (io-buffer-limit outbuf))
         (buffer (io-buffer-buffer outbuf)))
    (declare (fixnum idx limit)
             ((simple-array (unsigned-byte 8) (*)) buffer)
             (optimize (speed 3) (safety 0)))
    (etypecase value
      (fixnum
       (if (< (the fixnum value) 0)
         (report-bad-arg value 'unsigned-byte))
       (do* ((shift (ash (the fixnum (1- len)) 3) (- shift 8)))
            ((< shift 0) (progn
                           (setf (io-buffer-idx outbuf) idx)
                           (if (> idx (the fixnum (io-buffer-count outbuf)))
                             (setf (io-buffer-count outbuf) idx))
                           value))
         (declare (fixnum shift))
         (when (= idx limit)
           (%ioblock-force-output ioblock nil)
           (setq limit (io-buffer-limit outbuf)
                 buffer (io-buffer-buffer outbuf)))
         (setf (aref buffer idx) (logand #xff (the fixnum (%iasr shift value))))
         (incf idx)))
      (bignum
       (locally
           (declare ((simple-array (unsigned-byte 8) (*)) value))
         (let* ((nbytes (ash (uvsize value) 2))
                (sign-byte (if (logbitp 7 (the (unsigned-byte 8) (aref value (the fixnum (- nbytes #+big-endian-target 4 #+little-endian-target 1))))) #xff #x00)))
           (declare (fixnum nbytes)
                    ((unsigned-byte 8) sign-byte))
           (unless (zerop sign-byte)
             (report-bad-arg value 'unsigned-byte))
           (do* ((n (1- len) (1- n)))
                ((< n 0) (progn
                           (setf (io-buffer-idx outbuf) idx)
                           (if (> idx (the fixnum (io-buffer-count outbuf)))
                             (setf (io-buffer-count outbuf) idx))
                           value))
             (declare (fixnum n))
             (when (= idx limit)
               (%ioblock-force-output ioblock nil)
               (setq limit (io-buffer-limit outbuf)
                     buffer (io-buffer-buffer outbuf)))
             (setf (aref buffer idx)
                   (if (>= n nbytes)
                     0
                     (aref value #+little-endian-target n #+big-endian-target (the fixnum (logxor n 3)))))
             (incf idx))))))))

(defmethod unsigned-integer-to-binary (value len (s vector-output-stream))
  (%unsigned-integer-to-binary value len s))

(defun %signed-integer-to-binary (value len s)
  (declare (fixnum len))
  (unless (and (typep s 'basic-stream)
               (eq *vector-output-stream-class-wrapper*
                   (basic-stream.wrapper s)))
    (report-bad-arg s 'vector-input-stream))
  (let* ((ioblock (basic-stream-ioblock s))
         (outbuf (progn
                   (check-ioblock-owner ioblock)
                   (ioblock-outbuf ioblock)))
         (idx (io-buffer-idx outbuf))
         (limit (io-buffer-limit outbuf))
         (buffer (io-buffer-buffer outbuf)))
    (declare (fixnum idx limit)
             ((simple-array (unsigned-byte 8) (*)) buffer)
             (optimize (speed 3) (safety 0)))
    (do* ((newidx (+ idx len)))
         ((< newidx limit))
      (declare (fixnum newidx))
      (%ioblock-force-output ioblock nil)
      (setq limit (io-buffer-limit outbuf)
            buffer (io-buffer-buffer outbuf)))
    (etypecase value
      (fixnum
       (do* ((shift (ash (the fixnum (1- len)) 3) (- shift 8)))
            ((< shift 0) (progn
                           (setf (io-buffer-idx outbuf) idx)
                           (if (> idx (the fixnum (io-buffer-count outbuf)))
                             (setf (io-buffer-count outbuf) idx))
                           value))
         (declare (fixnum shift))
         (setf (aref buffer idx) (logand #xff (the fixnum (%iasr shift value))))
         (incf idx)))
      (bignum
       (locally
           (declare ((simple-array (unsigned-byte 8) (*)) value))
         (let* ((nbytes (ash (uvsize value) 2))
                (sign-byte (if (logbitp 7 (the (unsigned-byte 8) (aref value (the fixnum (- nbytes #+big-endian-target 4 #+little-endian-target 1))))) #xff #x00)))
           (declare (fixnum nbytes)
                    ((unsigned-byte 8) sign-byte))
           (do* ((n (1- len) (1- n)))
                ((< n 0) (progn
                           (setf (io-buffer-idx outbuf) idx)
                           (if (> idx (the fixnum (io-buffer-count outbuf)))
                             (setf (io-buffer-count outbuf) idx))
                           value))
             (declare (fixnum n))
             (setf (aref buffer idx)
                   (if (>= n nbytes)
                     sign-byte
                     (aref value #+little-endian-target n #+big-endian-target (the fixnum (logxor n 3)))))
             (incf idx))))))))

(defmethod signed-integer-to-binary (value len (s vector-output-stream))
  (%signed-integer-to-binary value len s))
      
(defmethod signed-integer-to-binary (value len (s binary-output-stream))
  (do* ((shift (ash (1- len) 3) (- shift 8)))
       ((< shift 0) value)
    (write-byte (logand #xff (ash value (- shift))) s)))             
               

         

(defun %make-vector-output-stream (vector external-format)
  (let* ((data nil)
         (len nil)
         (offset 0)
         (start 0)
         (displaced nil)
         (external-format (normalize-external-format t external-format))
         (encoding (external-format-character-encoding external-format))
         (line-termination (external-format-line-termination external-format)))
    (cond ((typep vector '(simple-array (unsigned-byte 8) (*)))
           (setq data vector len (length vector)))
          (t
           (multiple-value-setq (data offset) (array-data-and-offset vector))
           (unless (eql (typecode data) target::subtag-u8-vector)
             (report-bad-arg vector '(vector (unsigned-byte 8))))
           (unless (array-has-fill-pointer-p vector)
             (error "~S must be a vector with a fill pointer." vector))
           (setq start (+ (fill-pointer vector) offset)
                 len (+ (array-total-size vector) offset)
                 displaced vector)))
    (make-ioblock-stream *vector-output-stream-class*
                         :ioblock (make-vector-output-stream-ioblock
                                   :outbuf (make-io-buffer :buffer data
                                                           :idx start
                                                           :count start
                                                           :limit len
                                                           :size len)
                                   :displaced displaced
                                   :displacement offset)
                         :encoding encoding
                         :character-p t
                         :element-type '(unsigned-byte 8)
                         :line-termination line-termination
                         :force-output-function '%extend-vector-output-stream
                         :close-function '%vector-output-stream-close)))

    
(defun make-vector-output-stream (&key (external-format :default))
  (%make-vector-output-stream (make-array *vector-output-stream-default-initial-allocation* :element-type '(unsigned-byte 8))  external-format))

(defmethod stream-position ((s vector-output-stream) &optional newpos)
  (let* ((ioblock (basic-stream-ioblock s))
         (outbuf (ioblock-outbuf ioblock))
         (origin (vector-stream-ioblock-displacement ioblock)))
    (declare (fixnum origin))
    (if newpos
      (if (and (typep newpos 'fixnum)
               (> (the fixnum newpos) -1)
               (< (the fixnum newpos) (the fixnum (+ origin (the fixnum (io-buffer-limit outbuf))))))
        (let* ((scaled-new (+ origin (the fixnum newpos))))
          (declare (fixnum scaled-new))
          (setf (io-buffer-idx outbuf) scaled-new)
          (if (> (the fixnum (io-buffer-count outbuf)) scaled-new)
            (setf (io-buffer-count outbuf) scaled-new))
          (let* ((displaced (vector-output-stream-ioblock-displaced ioblock)))
            (when displaced
              (setf (fill-pointer displaced) newpos)))
          newpos)
        (report-bad-arg newpos `(integer 0 (,(- (the fixnum (io-buffer-limit outbuf)) origin)))))
      (the fixnum (- (the fixnum (io-buffer-idx outbuf)) origin)))))

(defun vector-input-stream-index (s)
  (unless (and (typep s 'basic-stream)
               (eq *vector-input-stream-class-wrapper*
                   (basic-stream.wrapper s)))
    (report-bad-arg s 'vector-input-stream))
  (let* ((ioblock (basic-stream-ioblock s)))
    (check-ioblock-owner ioblock)
    (the fixnum (- (the fixnum (io-buffer-idx (ioblock-inbuf ioblock)))
                   (the fixnum (vector-stream-ioblock-displacement ioblock))))))
            

(defun %vector-input-stream-untyi (ioblock char)
  (check-ioblock-owner ioblock)
  (let* ((inbuf (ioblock-inbuf ioblock))
         (idx (io-buffer-idx inbuf))
         (encoding (ioblock-encoding ioblock))
         (noctets (if encoding
                    (funcall (character-encoding-character-size-in-octets-function encoding) char)
                    1))
         (newidx (- idx noctets)))
    (declare (fixnum idx noctets newidx))
    (if (>= newidx (the fixnum (vector-stream-ioblock-displacement ioblock)))
      (setf (io-buffer-idx inbuf) newidx)
      (error "Invalid attempt to unread ~s on ~s." char (ioblock-stream ioblock)))))

  

(defmethod select-stream-untyi-function ((s vector-input-stream) (direction t))
  '%vector-input-stream-untyi)




(defun %make-vector-input-stream (vector start end external-format)
  (setq end (check-sequence-bounds vector start end))
  (let* ((data nil)
         (offset 0)
         (external-format (normalize-external-format t external-format))
         (encoding (external-format-character-encoding external-format))
         (line-termination (external-format-line-termination external-format)))

      (cond ((typep vector '(simple-array (unsigned-byte 8) (*)))
             (setq data vector                   offset start))
            (t (multiple-value-setq (data offset) (array-data-and-offset vector))
               (unless (typep data '(simple-array (unsigned-byte 8) (*)))
                 (report-bad-arg vector '(vector (unsigned-byte 8))))
               (incf start offset)
               (incf end offset)))
      (make-ioblock-stream *vector-input-stream-class*
                           :ioblock (make-vector-stream-ioblock
                                     :inbuf (make-io-buffer
                                             :buffer data
                                             :idx start
                                             :count end
                                             :limit end
                                             :size end)
                                     :displacement start)
                           :direction :input
                           :character-p t
                           :element-type '(unsigned-byte 8)
                           :encoding encoding
                           :line-termination line-termination
                           :listen-function 'false
                           :eofp-function 'true
                           :advance-function 'false
                           :close-function 'false)))
      
(defun make-vector-input-stream (vector &key (start 0) end external-format)
  (%make-vector-input-stream vector start end external-format))



(defmethod pui-stream ((s binary-input-stream) count)
  "Parse unsigned integer from a stream."
  (declare (fixnum count)               ; any integer that cannot be expressed in fixnum bytes is probably (ahem) too long to worry about
           (optimize (speed 3) (safety 1) (debug 1)))
  (let ((n 0))
    (dotimes (i count n)
      (declare (fixnum i))
      (setq n (+ (the fixnum (read-byte s)) (the integer (ash n 8)))))))

(defun %pui-stream (s count)
  (declare (fixnum count))
  (unless (and (typep s 'basic-stream)
               (eq *vector-input-stream-class-wrapper*
                   (basic-stream.wrapper s)))
    (report-bad-arg s 'vector-input-stream))
  (let* ((ioblock (basic-stream-ioblock s))
         (inbuf (progn
                  (check-ioblock-owner ioblock)
                  (ioblock-inbuf ioblock)))
         (idx (io-buffer-idx inbuf))
         (end (+ idx count))
         (limit (io-buffer-limit inbuf))
         (vector (io-buffer-buffer inbuf)))
    (declare (fixnum idx limit end)
             ((simple-array (unsigned-byte 8) (*)) vector))
    (if (< limit end)
      (error "Integer decoding error"))
    (let* ((result (%parse-unsigned-integer vector idx end)))
      (setf (io-buffer-idx inbuf) end)
      result)))

(defmethod pui-stream ((s vector-input-stream) count)
  (%pui-stream s count))

(defmethod psi-stream ((s binary-input-stream) count)
  (declare (fixnum count))
  (if (zerop count)
    0
    (let* ((n (read-byte s)))
      (if (>= n 128)
        (setq n (- n 256)))
      (dotimes (i (the fixnum (1- count)) n)
        (setq n (logior (read-byte s) (ash n 8)))))))

(defun %psi-stream (s count)
  (declare (fixnum count))
  (unless (and (typep s 'basic-stream)
               (eq *vector-input-stream-class-wrapper*
                   (basic-stream.wrapper s)))
    (report-bad-arg s 'vector-input-stream))
  (let* ((ioblock (basic-stream-ioblock s))
         (inbuf (progn
                  (check-ioblock-owner ioblock)
                  (ioblock-inbuf ioblock)))
         (idx (io-buffer-idx inbuf))
         (end (+ idx count))
         (limit (io-buffer-limit inbuf))
         (vector (io-buffer-buffer inbuf)))
    (declare (fixnum idx limit end))
    (if (< limit end)
      (error "Integer decoding error"))
    (let* ((result (%parse-signed-integer vector idx end)))
      (setf (io-buffer-idx inbuf) end)
      result)))

(defmethod psi-stream ((s vector-input-stream) count)
  (%psi-stream s count))

(defmethod stream-position ((s vector-input-stream) &optional newpos)
  (let* ((ioblock (basic-stream-ioblock s))
         (inbuf (ioblock-inbuf ioblock))
         (origin (vector-stream-ioblock-displacement ioblock)))
    (declare (fixnum origin))
    (if newpos
      (if (and (typep newpos 'fixnum)
               (> (the fixnum newpos) -1)
               (< (the fixnum newpos) (the fixnum (+ origin (the fixnum (io-buffer-limit inbuf))))))
        (progn
          (setf (io-buffer-idx inbuf) (the fixnum (+ origin (the fixnum newpos))))
          newpos)
        (report-bad-arg newpos `(integer 0 (,(- (the fixnum (io-buffer-limit inbuf)) origin)))))
      (the fixnum (- (the fixnum (io-buffer-idx inbuf)) origin)))))

(defmethod stream-length ((s vector-input-stream) &optional new)
  (unless new
    (let ((ioblock (basic-stream-ioblock s)))
      (%i- (io-buffer-limit (ioblock-inbuf ioblock))
	   (vector-stream-ioblock-displacement ioblock)))))


; end of L1-streams.lisp
