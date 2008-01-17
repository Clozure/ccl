;;; -*- Log: hemlock.log; Package: Hemlock-Internals -*-
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
;;;    This file contains definitions of various types of streams used
;;; in Hemlock.  They are implementation dependant, but should be
;;; portable to all implementations based on Spice Lisp with little
;;; difficulty.
;;;
;;; Written by Skef Wholey and Rob MacLachlan.
;;;

(in-package :hemlock-internals)

(defclass hemlock-output-stream (#-scl fundamental-character-output-stream
				 #+scl character-output-stream)
  ((mark
    :initform nil
    :accessor hemlock-output-stream-mark
    :documentation "The mark we insert at.")
   (out
    :accessor old-lisp-stream-out)
   (sout
    :accessor old-lisp-stream-sout)
   ) )

;; this should suffice for now:
(defmethod stream-write-char ((stream hemlock-output-stream) char)
  (funcall (old-lisp-stream-out stream) stream char))

(defmethod stream-write-string ((stream hemlock-output-stream) string
                                &optional
                                (start 0)
                                (end (length string)))
  (funcall (old-lisp-stream-sout stream) stream string start end))
                                

(defmethod print-object ((object hemlock-output-stream) stream)
  (write-string "#<Hemlock output stream>" stream))

(defun make-hemlock-output-stream (mark &optional (buffered :line))
  "Returns an output stream whose output will be inserted at the Mark.
  Buffered, which indicates to what extent the stream may be buffered
  is one of the following:
   :None  -- The screen is brought up to date after each stream operation.
   :Line  -- The screen is brought up to date when a newline is written.
   :Full  -- The screen is not updated except explicitly via Force-Output."
  (modify-hemlock-output-stream (make-instance 'hemlock-output-stream) mark
                                buffered))


(defun modify-hemlock-output-stream (stream mark buffered)
  (unless (and (markp mark)
	       (member (mark-kind mark) '(:right-inserting :left-inserting)))
    (error "~S is not a permanent mark." mark))
  (setf (hemlock-output-stream-mark stream) mark)
  (case buffered
    (:none
     (setf (old-lisp-stream-out stream) #'hemlock-output-unbuffered-out
	   (old-lisp-stream-sout stream) #'hemlock-output-unbuffered-sout))
    (:line
     (setf (old-lisp-stream-out stream) #'hemlock-output-line-buffered-out
	   (old-lisp-stream-sout stream) #'hemlock-output-line-buffered-sout))
    (:full
     (setf (old-lisp-stream-out stream) #'hemlock-output-buffered-out
	   (old-lisp-stream-sout stream) #'hemlock-output-buffered-sout))
    (t
     (error "~S is a losing value for Buffered." buffered)))
  stream)

(defmacro with-left-inserting-mark ((var form) &body forms)
  (let ((change (gensym)))
    `(let* ((,var ,form)
	    (,change (eq (mark-kind ,var) :right-inserting)))
       (unwind-protect
	   (progn
	     (when ,change
	       (setf (mark-kind ,var) :left-inserting))
	     ,@forms)
	 (when ,change
	   (setf (mark-kind ,var) :right-inserting))))))

(defun hemlock-output-unbuffered-out (stream character)
  (with-left-inserting-mark (mark (hemlock-output-stream-mark stream))
    (let* ((buffer (line-%buffer (mark-line mark))))
      (buffer-document-begin-editing buffer)
      (unwind-protect
           (insert-character mark character)
        (buffer-document-end-editing buffer)))))

(defun hemlock-output-unbuffered-sout (stream string start end)
  (with-left-inserting-mark (mark (hemlock-output-stream-mark stream))
    (unless (and (eql start 0)
                 (eql end (length string)))
      (setq string (subseq string start end)))
    (let* ((buffer (line-%buffer (mark-line mark))))
      (buffer-document-begin-editing buffer)
      (unwind-protect
           (insert-string mark string)
        (buffer-document-end-editing buffer)))))

(defun hemlock-output-buffered-out (stream character)
  (hemlock-output-unbuffered-out stream character))


(defun hemlock-output-buffered-sout (stream string start end)
  (hemlock-output-unbuffered-sout stream string start end))

(defun hemlock-output-line-buffered-out (stream character)
  (hemlock-output-unbuffered-out stream character))

(defun hemlock-output-line-buffered-sout (stream string start end)
  (hemlock-output-unbuffered-sout stream string start end))


(defmethod stream-finish-output ((stream hemlock-output-stream)))

(defmethod stream-force-output ((stream hemlock-output-stream)))

(defmethod close ((stream hemlock-output-stream) &key abort)
  (declare (ignore abort))
  (setf (hemlock-output-stream-mark stream) nil))

(defmethod stream-line-column ((stream hemlock-output-stream))
  (mark-charpos (hemlock-output-stream-mark stream)))



(defclass hemlock-region-stream (#-scl fundamental-character-input-stream
				 #+scl character-input-stream)
  ;;
  ;; The region we read from.
  ((region :initarg :region
           :accessor hemlock-region-stream-region)
   ;;
   ;; The mark pointing to the next character to read.
   (mark :initarg :mark
         :accessor hemlock-region-stream-mark)) )

(defmethod print-object ((object hemlock-region-stream) stream)
  (declare (ignorable object))
  (write-string "#<Hemlock region stream>" stream))

(defun make-hemlock-region-stream (region)
  "Returns an input stream that will return successive characters from the
  given Region when asked for input."
  (make-instance 'hemlock-region-stream
                 :region region
                 :mark (copy-mark (region-start region) :right-inserting)))

(defun modify-hemlock-region-stream (stream region)
  (setf (hemlock-region-stream-region stream) region)
  (let* ((mark (hemlock-region-stream-mark stream))
	 (start (region-start region))
	 (start-line (mark-line start)))
    ;; Make sure it's dead.
    (delete-mark mark)
    (setf (mark-line mark) start-line  (mark-charpos mark) (mark-charpos start))
    (push mark (line-marks start-line)))
  stream)

(defmethod stream-read-char ((stream hemlock-region-stream))
  (let ((mark (hemlock-region-stream-mark stream)))
    (cond ((mark< mark
		  (region-end (hemlock-region-stream-region stream)))
	   (prog1 (next-character mark) (mark-after mark)))
	  (t :eof))))

(defmethod stream-listen ((stream hemlock-region-stream))
  (mark< (hemlock-region-stream-mark stream)
         (region-end (hemlock-region-stream-region stream))))

(defmethod stream-unread-char ((stream hemlock-region-stream) char)
  (let ((mark (hemlock-region-stream-mark stream)))
    (unless (mark> mark
                   (region-start (hemlock-region-stream-region stream)))
      (error "Nothing to unread."))
    (unless (char= char (previous-character mark))
      (error "Unreading something not read: ~S" char))
    (mark-before mark)))

(defmethod stream-clear-input ((stream hemlock-region-stream))
  (move-mark
   (hemlock-region-stream-mark stream)
   (region-end (hemlock-region-stream-region stream)))
  nil)

(defmethod close ((stream hemlock-region-stream) &key abort)
  (declare (ignorable abort))
  (delete-mark (hemlock-region-stream-mark stream))
  (setf (hemlock-region-stream-region stream) nil))

#+excl
(defmethod excl:stream-read-char-no-hang ((stream hemlock-region-stream))
  (stream-read-char stream))

#||  
(defmethod excl::stream-file-position ((stream hemlock-output-stream) &optional pos)
  (assert (null pos))
  (mark-charpos (hemlock-output-stream-mark stream)))

(defun region-misc (stream operation &optional arg1 arg2)
  (declare (ignore arg2))
  (case operation

    (:file-position
     (let ((start (region-start (hemlock-region-stream-region stream)))
	   (mark (hemlock-region-stream-mark stream)))
       (cond (arg1
	      (move-mark mark start)
	      (character-offset mark arg1))
	     (t
	      (count-characters (region start mark)))))) ))
||#


;;;; Stuff to support keyboard macros.

#+later
(progn
  
(defstruct (kbdmac-stream
	    (:include editor-input
		      (get #'kbdmac-get)
		      (unget #'kbdmac-unget)
		      (listen #'kbdmac-listen))
	    (:constructor make-kbdmac-stream ()))
  buffer    ; The simple-vector that holds the characters.
  index)    ; Index of the next character.

(defun kbdmac-get (stream ignore-abort-attempts-p)
  (declare (ignore ignore-abort-attempts-p))
  (let ((index (kbdmac-stream-index stream)))
    (setf (kbdmac-stream-index stream) (1+ index))
    (setq *last-key-event-typed*
	  (svref (kbdmac-stream-buffer stream) index))))

(defun kbdmac-unget (ignore stream)
  (declare (ignore ignore))
  (if (plusp (kbdmac-stream-index stream))
      (decf (kbdmac-stream-index stream))
      (error "Nothing to unread.")))

(defun kbdmac-listen (stream)
  (declare (ignore stream))
  t)

;;; MODIFY-KBDMAC-STREAM  --  Internal
;;;
;;;    Bash the kbdmac-stream Stream so that it will return the Input.
;;;
(defun modify-kbdmac-stream (stream input)
  (setf (kbdmac-stream-index stream) 0)
  (setf (kbdmac-stream-buffer stream) input)
  stream)
)