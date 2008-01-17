;;; -*- Package: Hemlock; Log: hemlock.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(hemlock-ext:file-comment
  "$Header$")
;;;
;;; **********************************************************************
;;;
;;; This file implements typescript streams.
;;;
;;; A typescript stream is a bidirectional stream which uses remote
;;; function calls to interact with a Hemlock typescript buffer. That
;;; is: the code in this file is executed on the slave side.
;;;
;;; Written by William Lott.
;;;

(in-package :hemlock)


;;;; Ts-streams.

(defconstant ts-stream-output-buffer-size 512)

(defclass ts-stream (hi::fundamental-character-output-stream
                     hi::fundamental-character-input-stream)
  ((wire
    :initarg  :wire
    :initform nil
    :accessor ts-stream-wire)

   (typescript
    :initarg  :typescript
    :initform nil
    :accessor ts-stream-typescript)

   (output-buffer
    :initarg  :output-buffer
    :initform (make-string ts-stream-output-buffer-size)
    :accessor ts-stream-output-buffer
    :type     simple-string)

   (output-buffer-index
    :initarg  :output-buffer-index
    :initform 0
    :accessor ts-stream-output-buffer-index
    :type     fixnum)
  
   (char-pos
    :initarg  :char-pos
    :initform 0
    :accessor ts-stream-char-pos
    :type     fixnum
    :documentation "The current output character position on the line, returned by the :CHARPOS method.")
  
   (line-length
    :initarg :line-length
    :initform 80
    :accessor ts-stream-line-length
    :documentation "The current length of a line of output.  Returned by STREAM-LINE-LENGTH method.")

   (current-input
    :initarg :current-input
    :initform nil
    :accessor ts-stream-current-input
    :type list
    :documentation "This is a list of strings and stream-commands whose order manifests the
                    input provided by remote procedure calls into the slave of
                    TS-STREAM-ACCEPT-INPUT.")
   
   (input-read-index
    :initarg :input-read-index
    :initform 0
    :accessor ts-stream-input-read-index
    :type fixnum)))

(defun make-ts-stream (wire typescript)
  (make-instance 'ts-stream :wire wire :typescript typescript))


;;;; Conditions.

(define-condition unexpected-stream-command (error)
  ;; Context is a string to be plugged into the report text.
  ((context :reader unexpected-stream-command-context :initarg :context))
  (:report (lambda (condition stream)
	     (format stream "~&Unexpected stream-command while ~A."
		     (unexpected-stream-command-context condition)))))



;;;; Editor remote calls into slave.

;;; TS-STREAM-ACCEPT-INPUT -- Internal Interface.
;;;
;;; The editor calls this remotely in the slave to indicate that the user has
;;; provided input.  Input is a string, symbol, or list.  If it is a list, the
;;; the CAR names the command, and the CDR is the arguments.
;;;
(defun ts-stream-accept-input (remote input)
  (let ((stream (hemlock.wire:remote-object-value remote)))
    (hemlock-ext:without-interrupts
     (hemlock-ext:without-gcing
      (setf (ts-stream-current-input stream)
	    (nconc (ts-stream-current-input stream)
		   (list (etypecase input
			   (string
			    (let ((newline
				   (position #\newline input :from-end t)))
			      (setf (ts-stream-char-pos stream)
				    (if newline
					(- (length input) newline 1)
					(length input)))
			      input))
                           #+NILGB
			   (cons
			    (ext:make-stream-command (car input)
						     (cdr input)))
                           #+NILGB
			   (symbol
			    (ext:make-stream-command input)))))))))
  nil)

;;; TS-STREAM-SET-LINE-LENGTH -- Internal Interface.
;;;
;;; This function is called by the editor to indicate that the line-length for
;;; a TS stream should now be Length.
;;;
(defun ts-stream-set-line-length (remote length)
  (let ((stream (hemlock.wire:remote-object-value remote)))
    (setf (ts-stream-line-length stream) length)))



;;;; Stream methods.

;;; %TS-STREAM-LISTEN -- Internal.
;;;
;;; Determine if there is any input available.  If we don't think so, process
;;; all pending events, and look again.
;;;
(defmethod hi::stream-listen ((stream ts-stream))
  (flet ((check ()
	   (hemlock-ext:without-interrupts
	    (hemlock-ext:without-gcing
	     (loop
	       (let* ((current (ts-stream-current-input stream))
		      (first (first current)))
		 (cond ((null current)
			(return nil))
                       #+NILGB
		       ((ext:stream-command-p first)
			(return t))
		       ((>= (ts-stream-input-read-index stream)
			    (length (the simple-string first)))
			(pop (ts-stream-current-input stream))
			(setf (ts-stream-input-read-index stream) 0))
		       (t
			(return t)))))))))
    (or (check)
	(progn
	  #+NILGB (system:serve-all-events 0)
	  (check)))))

;;; %TS-STREAM-IN -- Internal.
;;;
;;; The READ-CHAR stream method.
;;;
(defmethod hi::stream-read-char ((stream ts-stream))
  (hi::stream-force-output stream)
  (wait-for-typescript-input stream)
  (hemlock-ext:without-interrupts
   (hemlock-ext:without-gcing
    (let ((first (first (ts-stream-current-input stream))))
      (etypecase first
	(string
	 (prog1 (schar first (ts-stream-input-read-index stream))
	   (incf (ts-stream-input-read-index stream))))
        #+NILGB
	(ext:stream-command
	 (error 'unexpected-stream-command
		:context "in the READ-CHAR method")))))))

;;; %TS-STREAM-READ-LINE -- Internal.
;;;
;;; The READ-LINE stream method.  Note: here we take advantage of the fact that
;;; newlines will only appear at the end of strings.
;;;

(defmethod stream-read-line (stream)
  (macrolet
      ((next-str ()
	 '(progn
           (wait-for-typescript-input stream)
           (hemlock-ext:without-interrupts
            (hemlock-ext:without-gcing
             (let ((first (first (ts-stream-current-input stream))))
               (etypecase first
                 (string
                  (prog1 (if (zerop (ts-stream-input-read-index stream))
                             (pop (ts-stream-current-input stream))
                             (subseq (pop (ts-stream-current-input stream))
                                     (ts-stream-input-read-index stream)))
                    (setf (ts-stream-input-read-index stream) 0)))
                 #+NILGB
                 (ext:stream-command
                  (error 'unexpected-stream-command
                         :context "in the READ-CHAR method")))))))))
    (do ((result (next-str) (concatenate 'simple-string result (next-str))))
	((char= (schar result (1- (length result))) #\newline)
	 (values (subseq result 0 (1- (length result)))
		 nil))
      (declare (simple-string result)))))

;;; WAIT-FOR-TYPESCRIPT-INPUT -- Internal.
;;;
;;; Keep calling server until some input shows up.
;;; 
(defun wait-for-typescript-input (stream)
  (unless (hi::stream-listen stream)        ;for some reasons in CLISP CL:LISTEN calls STREAM-READ-CHAR :-/
    (let ((wire (ts-stream-wire stream))
	  (ts (ts-stream-typescript stream)))
      (hemlock-ext:without-interrupts
       (hemlock-ext:without-gcing
	(hemlock.wire:remote wire (ts-buffer-ask-for-input ts))
	(hemlock.wire:wire-force-output wire)))
      (loop
          #+:hemlock.serve-event (hemlock.wire::serve-all-events)
          #-:hemlock.serve-event (hemlock.wire:wire-get-object wire)
          #+NILGB (sleep .1)            ;###
	(when (hi::stream-listen stream)
	  (return))))))

;;; %TS-STREAM-FLSBUF --- internal.
;;;
;;; Flush the output buffer associated with stream.  This should only be used
;;; inside a without-interrupts and without-gcing.
;;; 
(defun %ts-stream-flsbuf (stream)
  (when (and (ts-stream-wire stream)
	     (ts-stream-output-buffer stream)
	     (not (zerop (ts-stream-output-buffer-index stream))))
    (hemlock.wire:remote (ts-stream-wire stream)
      (ts-buffer-output-string
       (ts-stream-typescript stream)
       (subseq (the simple-string (ts-stream-output-buffer stream))
	       0
	       (ts-stream-output-buffer-index stream))))
    (setf (ts-stream-output-buffer-index stream) 0)))

;;; %TS-STREAM-OUT --- internal.
;;;
;;; Output a single character to stream.
;;;
(defmethod hi::stream-write-char ((stream ts-stream) char)
  (declare (base-char char))
  (hemlock-ext:without-interrupts
   (hemlock-ext:without-gcing
    (when (= (ts-stream-output-buffer-index stream)
	     ts-stream-output-buffer-size)
      (%ts-stream-flsbuf stream))
    (setf (schar (ts-stream-output-buffer stream)
		 (ts-stream-output-buffer-index stream))
	  char)
    (incf (ts-stream-output-buffer-index stream))
    (incf (ts-stream-char-pos stream))
    (when (= (char-code char)
	     (char-code #\Newline))
      (%ts-stream-flsbuf stream)
      (setf (ts-stream-char-pos stream) 0)
      (hemlock.wire:wire-force-output (ts-stream-wire stream)))
    char)))

;;; %TS-STREAM-SOUT --- internal.
;;;
;;; Output a string to stream.
;;;
(defmethod hi::stream-write-string ((stream ts-stream) string &optional (start 0) (end (length string)))
  ;; This can't be true generally: --GB
  #+NIL (declare (simple-string string))
  (declare (fixnum start end))
  (let ((wire (ts-stream-wire stream))
	(newline (position #\Newline string :start start :end end :from-end t))
	(length (- end start)))
    (when wire
      (hemlock-ext:without-interrupts
       (hemlock-ext:without-gcing
	(let ((index (ts-stream-output-buffer-index stream)))
	  (cond ((> (+ index length)
		    ts-stream-output-buffer-size)
		 (%ts-stream-flsbuf stream)
		 (hemlock.wire:remote wire
                                      (ts-buffer-output-string (ts-stream-typescript stream)
                                                               (subseq string start end)))
		 (when newline
		   (hemlock.wire:wire-force-output wire)))
		(t
		 (replace (the simple-string (ts-stream-output-buffer stream))
			  string
			  :start1 index
			  :end1 (+ index length)
			  :start2 start
			  :end2 end)
		 (incf (ts-stream-output-buffer-index stream)
		       length)
		 (when newline
		   (%ts-stream-flsbuf stream)
		   (hemlock.wire:wire-force-output wire)))))
	(setf (ts-stream-char-pos stream)
	      (if newline
		  (- end newline 1)
		  (+ (ts-stream-char-pos stream)
		     length))))))))

;;; %TS-STREAM-UNREAD -- Internal.
;;;
;;; Unread a single character.
;;;
(defmethod hi::stream-unread-char ((stream ts-stream) char)
  (hemlock-ext:without-interrupts
   (hemlock-ext:without-gcing
    (let ((first (first (ts-stream-current-input stream))))
      (cond ((and (stringp first)
		  (> (ts-stream-input-read-index stream) 0))
	     (setf (schar first (decf (ts-stream-input-read-index stream)))
		   char))
	    (t
	     (push (string char) (ts-stream-current-input stream))
	     (setf (ts-stream-input-read-index stream) 0)))))))

;;; %TS-STREAM-CLOSE --- internal.
;;;
;;; Can't do much, 'cause the wire is shared.
;;;
(defmethod close ((stream ts-stream) &key abort)
  (unless abort
    (force-output stream))
  #+NILGB (lisp::set-closed-flame stream)       ;Hugh!? what is that? --GB
  )

;;; %TS-STREAM-CLEAR-INPUT -- Internal.
;;;
;;; Pass the request to the editor and clear any buffered input.
;;;
(defmethod hi::stream-clear-input ((stream ts-stream))
  (hemlock-ext:without-interrupts
   (hemlock-ext:without-gcing
    (when (ts-stream-wire stream)
      (hemlock.wire:remote-value (ts-stream-wire stream)
	(ts-buffer-clear-input (ts-stream-typescript stream))))
    (setf (ts-stream-current-input stream) nil
	  (ts-stream-input-read-index stream) 0))))

(defmethod hi::stream-finish-output ((stream ts-stream))
  (when (ts-stream-wire stream)
    (hemlock-ext:without-interrupts
     (hemlock-ext:without-gcing
      (%ts-stream-flsbuf stream)
      ;; Note: for the return value to come back,
      ;; all pending RPCs must have completed.
      ;; Therefore, we know it has synced.
      (hemlock.wire:remote-value (ts-stream-wire stream)
                         (ts-buffer-finish-output (ts-stream-typescript stream))))))
  t)

(defmethod hi::stream-force-output ((stream ts-stream))
  (when (ts-stream-wire stream)
    (hemlock-ext:without-interrupts
     (hemlock-ext:without-gcing
      (%ts-stream-flsbuf stream)
      (hemlock.wire:wire-force-output (ts-stream-wire stream)))))
  t)

(defmethod hi::stream-line-column ((stream ts-stream))
  (ts-stream-char-pos stream))

(defmethod hi::stream-line-length ((stream ts-stream))
  (ts-stream-line-length stream))

#+NILGB ;; -- hmm.
(defmethod interactive-stream-p ((stream ts-stream))
  t)

(defmethod hi::stream-clear-output ((stream ts-stream))
  (setf (ts-stream-output-buffer-index stream) 0))

;;; %TS-STREAM-MISC -- Internal.
;;;
;;; The misc stream method.
;;;
#+NILGB
(defun %ts-stream-misc (stream operation &optional arg1 arg2)
  (case operation
    (:get-command
     (wait-for-typescript-input stream)
     (hemlock-ext:without-interrupts
      (hemlock-ext:without-gcing
       (etypecase (first (ts-stream-current-input stream))
	 (stream-command
	  (setf (ts-stream-input-read-index stream) 0)
	  (pop (ts-stream-current-input stream)))
	 (string nil)))))
    ))

;; $Log$
;; Revision 1.1  2003/10/19 08:57:16  gb
;; Initial revision
;;
;; Revision 1.1.2.1  2003/08/10 19:11:40  gb
;; New files, imported from upstream CVS as of 03/08/09.
;;
;; Revision 1.3  2003/08/05 19:51:13  gilbert
;; initial slave lisp support, still not ready for prime time.
;;
;;
