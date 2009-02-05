;;; -*- Log: hemlock.log; Package: Hemlock -*-
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
;;; Source comparison stuff for Hemlock.
;;;
;;; Written by Skef Wholey and Bill Chiles.
;;;

(in-package :hemlock)

(defhvar "Source Compare Ignore Extra Newlines"
  "If T, Source Compare and Source Merge will treat all groups of newlines
  as if they were a single newline.  The default is T."
  :value t)

(defhvar "Source Compare Ignore Case"
  "If T, Source Compare and Source Merge will treat all letters as if they
  were of the same case.  The default is Nil."
  :value nil)

(defhvar "Source Compare Ignore Indentation"
  "This determines whether comparisons ignore initial whitespace on a line or
   use the whole line."
  :value nil)

(defhvar "Source Compare Number of Lines"
  "This variable controls the number of lines Source Compare and Source Merge
  will compare when resyncronizing after a difference has been encountered.
  The default is 3."
  :value 3)

(defhvar "Source Compare Default Destination"
  "This is a sticky-default buffer name to offer when comparison commands prompt
   for a results buffer."
  :value "Differences")


(defcommand "Buffer Changes" (p)
  "Generate a comparison of the current buffer with its file on disk."
  "Generate a comparison of the current buffer with its file on disk."
  (declare (ignore p))
  (let ((buffer (current-buffer)))
    (unless (buffer-pathname buffer)
      (editor-error "No pathname associated with buffer."))
    (let ((other-buffer (or (getstring "Buffer Changes File" *buffer-names*)
			    (make-buffer "Buffer Changes File")))
	  (result-buffer (or (getstring "Buffer Changes Result" *buffer-names*)
			     (make-buffer "Buffer Changes Result"))))
      (visit-file-command nil (buffer-pathname buffer) other-buffer)
      (delete-region (buffer-region result-buffer))
      (compare-buffers-command nil buffer other-buffer result-buffer)
      (delete-buffer other-buffer))))

;;; "Compare Buffers" creates two temporary buffers when there is a prefix.
;;; These get deleted when we're done.  Buffer-a and Buffer-b are used for
;;; names is banners in either case.
;;; 
(defcommand "Compare Buffers" (p &optional buffer-a buffer-b dest-buffer)
  "Performs a source comparison on two specified buffers.  If the prefix
   argument is supplied, only compare the regions in the buffer."
  "Performs a source comparison on two specified buffers, Buffer-A and
   Buffer-B, putting the result of the comparison into the Dest-Buffer.
   If the prefix argument is supplied, only compare the regions in the
   buffer."
  (srccom-choose-comparison-functions)
  (multiple-value-bind (buffer-a buffer-b dest-point
		        delete-buffer-a delete-buffer-b)
		       (get-srccom-buffers "Compare buffer: " buffer-a buffer-b
					   dest-buffer p)
    (with-output-to-mark (log dest-point)
      (format log "Comparison of ~A and ~A.~%~%"
	      (buffer-name buffer-a) (buffer-name buffer-b))
      (with-mark ((mark-a (buffer-start-mark (or delete-buffer-a buffer-a)))
		  (mark-b (buffer-start-mark (or delete-buffer-b buffer-b))))
	(loop
	  (multiple-value-bind (diff-a diff-b)
			       (srccom-find-difference mark-a mark-b)
	    (when (null diff-a) (return nil))
	    (format log "**** Buffer ~A:~%" (buffer-name buffer-a))
	    (insert-region dest-point diff-a)
	    (format log "**** Buffer ~A:~%" (buffer-name buffer-b))
	    (insert-region dest-point diff-b)
	    (format log "***************~%~%")
	    (move-mark mark-a (region-end diff-a))
	    (move-mark mark-b (region-end diff-b))
	    (unless (line-offset mark-a 1) (return))
	    (unless (line-offset mark-b 1) (return)))))
	(format log "Done.~%"))
    (when delete-buffer-a
      (delete-buffer delete-buffer-a)
      (delete-buffer delete-buffer-b))))


;;; "Merge Buffers" creates two temporary buffers when there is a prefix.
;;; These get deleted when we're done.  Buffer-a and Buffer-b are used for
;;; names is banners in either case.
;;; 
(defcommand "Merge Buffers" (p &optional buffer-a buffer-b dest-buffer)
  "Performs a source merge on two specified buffers.  If the prefix
   argument is supplied, only compare the regions in the buffer."
  "Performs a source merge on two specified buffers, Buffer-A and Buffer-B,
   putting the resulting text into the Dest-Buffer.  If the prefix argument
   is supplied, only compare the regions in the buffer."
  (srccom-choose-comparison-functions)
  (multiple-value-bind (buffer-a buffer-b dest-point
		        delete-buffer-a delete-buffer-b)
		       (get-srccom-buffers "Merge buffer: " buffer-a buffer-b
					   dest-buffer p)
    (with-output-to-mark (stream dest-point)
      (let ((region-a (buffer-region (or delete-buffer-a buffer-a))))
	(with-mark ((temp-a (region-start region-a) :right-inserting)
		    (temp-b dest-point :right-inserting)
		    (mark-a (region-start region-a))
		    (mark-b (region-start
			     (buffer-region (or delete-buffer-b buffer-b)))))
	  (clear-echo-area)
	  (loop
	    (multiple-value-bind (diff-a diff-b)
				 (srccom-find-difference mark-a mark-b)
	      (when (null diff-a)
		(insert-region dest-point (region temp-a (region-end region-a)))
		(return nil))
	      ;; Copy the part that's the same.
	      (insert-region dest-point (region temp-a (region-start diff-a)))
	      ;; Put both versions in the buffer, and prompt for which one to use.
	      (move-mark temp-a dest-point)
	      (format stream "~%**** Buffer ~A (1):~%" (buffer-name buffer-a))
	      (insert-region dest-point diff-a)
	      (move-mark temp-b dest-point)
	      (format stream "~%**** Buffer ~A (2):~%" (buffer-name buffer-b))
	      (insert-region dest-point diff-b)
	      (command-case
		  (:prompt "Merge Buffers: "
		   :help "Type one of these characters to say how to merge:") 
		(#\1 "Use the text from buffer 1."
		     (delete-region (region temp-b dest-point))
		     (delete-characters temp-a)
		     (delete-region
		      (region temp-a
			      (line-start temp-b
					  (line-next (mark-line temp-a))))))
		(#\2 "Use the text from buffer 2."
		     (delete-region (region temp-a temp-b))
		     (delete-characters temp-b)
		     (delete-region
		      (region temp-b
			      (line-start temp-a
					  (line-next (mark-line temp-b))))))
		(#\b "Insert both versions with **** MERGE LOSSAGE **** around them."
		     (insert-string temp-a "
		     **** MERGE LOSSAGE ****")
		     (insert-string dest-point "
		     **** END OF MERGE LOSSAGE ****"))
		(#\a "Align window at start of difference display."
		     (line-start
		      (move-mark
		       (window-display-start
			(car (buffer-windows (line-buffer (mark-line temp-a)))))
		       temp-a))
		     (reprompt))
		(:recursive-edit "Enter a recursive edit."
				 (with-mark ((save dest-point))
				   (do-recursive-edit)
				   (move-mark dest-point save))
				 (reprompt)))
	      (redisplay)
	      (move-mark mark-a (region-end diff-a))
	      (move-mark mark-b (region-end diff-b))
	      (move-mark temp-a mark-a)
	      (unless (line-offset mark-a 1) (return))
	      (unless (line-offset mark-b 1) (return))))))
      (message "Done."))
    (when delete-buffer-a
      (delete-buffer delete-buffer-a)
      (delete-buffer delete-buffer-b))))

(defun get-srccom-buffers (first-prompt buffer-a buffer-b dest-buffer p)
  (unless buffer-a
    (setf buffer-a (prompt-for-buffer :prompt first-prompt
				      :must-exist t
				      :default (current-buffer))))
  (unless buffer-b
    (setf buffer-b (prompt-for-buffer :prompt "With buffer: "
				      :must-exist t
				      :default (previous-buffer))))
  (unless dest-buffer
    (setf dest-buffer
	  (prompt-for-buffer :prompt "Putting results in buffer: "
			     :must-exist nil
			     :default-string
			     (value source-compare-default-destination))))
  (if (stringp dest-buffer)
      (setf dest-buffer (make-buffer dest-buffer))
      (buffer-end (buffer-point dest-buffer)))
  (setf (value source-compare-default-destination) (buffer-name dest-buffer))
  (change-to-buffer dest-buffer)
  (let* ((alt-buffer-a (if p (make-buffer (prin1-to-string (gensym)))))
	 (alt-buffer-b (if alt-buffer-a
			   (make-buffer (prin1-to-string (gensym))))))
    (when alt-buffer-a
      (ninsert-region (buffer-point alt-buffer-a)
		      (copy-region (if (mark< (buffer-point buffer-a)
					      (buffer-mark buffer-a))
				       (region (buffer-point buffer-a)
					       (buffer-mark buffer-a))
				       (region (buffer-mark buffer-a)
					       (buffer-point buffer-a)))))
      (ninsert-region (buffer-point alt-buffer-b)
		      (copy-region (if (mark< (buffer-point buffer-b)
					      (buffer-mark buffer-b))
				       (region (buffer-point buffer-b)
					       (buffer-mark buffer-b))
				       (region (buffer-mark buffer-b)
					       (buffer-point buffer-b))))))
    (values buffer-a buffer-b (current-point) alt-buffer-a alt-buffer-b)))
#|
(defun get-srccom-buffers (first-prompt buffer-a buffer-b dest-buffer p)
  (unless buffer-a
    (setf buffer-a (prompt-for-buffer :prompt first-prompt
				      :must-exist t
				      :default (current-buffer))))
  (unless buffer-b
    (setf buffer-b (prompt-for-buffer :prompt "With buffer: "
				      :must-exist t
				      :default (previous-buffer))))
  (unless dest-buffer
    (let* ((name (value source-compare-default-destination))
	   (temp-default (getstring name *buffer-names*))
	   (default (or temp-default (make-buffer name))))
      (setf dest-buffer (prompt-for-buffer :prompt "Putting results in buffer: "
					   :must-exist nil
					   :default default))
      ;; Delete the default buffer if it did already exist and was not chosen.
      (unless (or (eq dest-buffer default) temp-default)
	(delete-buffer default))))
  (if (stringp dest-buffer)
      (setf dest-buffer (make-buffer dest-buffer))
      (buffer-end (buffer-point dest-buffer)))
  (setf (value source-compare-default-destination) (buffer-name dest-buffer))
  (change-to-buffer dest-buffer)
  (let* ((alt-buffer-a (if p (make-buffer (prin1-to-string (gensym)))))
	 (alt-buffer-b (if alt-buffer-a
			   (make-buffer (prin1-to-string (gensym))))))
    (when alt-buffer-a
      (ninsert-region (buffer-point alt-buffer-a)
		      (copy-region (if (mark< (buffer-point buffer-a)
					      (buffer-mark buffer-a))
				       (region (buffer-point buffer-a)
					       (buffer-mark buffer-a))
				       (region (buffer-mark buffer-a)
					       (buffer-point buffer-a)))))
      (ninsert-region (buffer-point alt-buffer-b)
		      (copy-region (if (mark< (buffer-point buffer-b)
					      (buffer-mark buffer-b))
				       (region (buffer-point buffer-b)
					       (buffer-mark buffer-b))
				       (region (buffer-mark buffer-b)
					       (buffer-point buffer-b))))))
    (values buffer-a buffer-b (current-point) alt-buffer-a alt-buffer-b)))
|#


;;;; Functions that find the differences between two buffers.

(defun srccom-find-difference (mark-a mark-b)
  "Returns as multiple values two regions of text that are different in the
  lines following Mark-A and Mark-B.  If no difference is encountered, Nil
  is returned."
  (multiple-value-bind (diff-a diff-b)
		       (srccom-different-lines mark-a mark-b)
    (when diff-a
      (multiple-value-bind (same-a same-b)
			   (srccom-similar-lines diff-a diff-b)
	(values (region diff-a same-a)
		(region diff-b same-b))))))

;;; These are set by SRCCOM-CHOOSE-COMPARISON-FUNCTIONS depending on something.
;;;
(defvar *srccom-line=* nil)
(defvar *srccom-line-next* nil)

(defun srccom-different-lines (mark-a mark-b)
  "Returns as multiple values two marks pointing to the first different lines
  found after Mark-A and Mark-B.  Nil is returned if no different lines are
  found."
  (do ((line-a (mark-line mark-a) (funcall *srccom-line-next* line-a))
       (mark-a (copy-mark mark-a))
       (line-b (mark-line mark-b) (funcall *srccom-line-next* line-b))
       (mark-b (copy-mark mark-b)))
      (())
    (cond ((null line-a)
	   (return (if line-b
		       (values mark-a mark-b))))
	  ((null line-b)
	   (return (values mark-a mark-b))))
    (line-start mark-a line-a)
    (line-start mark-b line-b)
    (unless (funcall *srccom-line=* line-a line-b)
      (return (values mark-a mark-b)))))

(defun srccom-similar-lines (mark-a mark-b)
  "Returns as multiple values two marks pointing to the first similar lines
  found after Mark-A and Mark-B."
  (do ((line-a (mark-line mark-a) (funcall *srccom-line-next* line-a))
       (cmark-a (copy-mark mark-a))
       (line-b (mark-line mark-b) (funcall *srccom-line-next* line-b))
       (cmark-b (copy-mark mark-b))
       (temp)
       (window-size (value source-compare-number-of-lines)))
      (())
    ;; If we hit the end of one buffer, then the difference extends to the end
    ;; of both buffers.
    (if (or (null line-a) (null line-b))
	(return
	 (values
	  (buffer-end-mark (line-buffer (mark-line mark-a)))
	  (buffer-end-mark (line-buffer (mark-line mark-b))))))
    (line-start cmark-a line-a)
    (line-start cmark-b line-b)
    ;; Three cases:
    ;;  1] Difference will be same length in A and B.  If so, Line-A = Line-B.
    ;;  2] Difference will be longer in A.  If so, Line-A = something in B.
    ;;  3] Difference will be longer in B.  If so, Line-B = something in A.
    (cond ((and (funcall *srccom-line=* line-a line-b)
		(srccom-check-window line-a line-b window-size))
	   (return (values cmark-a cmark-b)))
	  ((and (setq temp (srccom-line-in line-a mark-b cmark-b))
		(srccom-check-window line-a temp window-size))
	   (return (values cmark-a (line-start cmark-b temp))))
	  ((and (setq temp (srccom-line-in line-b mark-a cmark-a))
		(srccom-check-window temp line-b window-size))
	   (return (values (line-start cmark-a temp) cmark-b))))))

(defun srccom-line-in (line start end)
  "Checks to see if there is a Line Srccom-Line= to the given Line in the
  region delimited by the Start and End marks.  Returns that line if so, or
  Nil if there is none."
  (do ((current (mark-line start) (funcall *srccom-line-next* current))
       (terminus (funcall *srccom-line-next* (mark-line end))))
      ((eq current terminus) nil)
    (if (funcall *srccom-line=* line current)
	(return current))))

(defun srccom-check-window (line-a line-b count)
  "Verifies that the Count lines following Line-A and Line-B are Srccom-Line=.
  If so, returns T.  Otherwise returns Nil."
  (do ((line-a line-a (funcall *srccom-line-next* line-a))
       (line-b line-b (funcall *srccom-line-next* line-b))
       (index 0 (1+ index)))
      ((= index count) t)
    (if (not (funcall *srccom-line=* line-a line-b))
	(return nil))))



;;;; Functions that control the comparison of text.

;;; SRCCOM-CHOOSE-COMPARISON-FUNCTIONS -- Internal.
;;;
;;; This initializes utility functions for comparison commands based on Hemlock
;;; variables.
;;;
(defun srccom-choose-comparison-functions ()
  (setf *srccom-line=*
	(if (value source-compare-ignore-case)
	    (if (value source-compare-ignore-indentation)
		#'srccom-ignore-case-and-indentation-line=
		#'srccom-case-insensitive-line=)
	    (if (value source-compare-ignore-indentation)
		#'srccom-ignore-indentation-case-sensitive-line=
		#'srccom-case-sensitive-line=)))
  (setf *srccom-line-next*
	(if (value source-compare-ignore-extra-newlines)
	    #'srccom-line-next-ignoring-extra-newlines
	    #'line-next)))
#|
(defun srccom-choose-comparison-functions ()
  "This function should be called by a ``top level'' source compare utility
  to initialize the lower-level functions that compare text."
  (setf *srccom-line=*
	(if (value source-compare-ignore-case)
	    #'srccom-case-insensitive-line=
	    #'srccom-case-sensitive-line=))
  (setf *srccom-line-next*
	(if (value source-compare-ignore-extra-newlines)
	    #'srccom-line-next-ignoring-extra-newlines
	    #'line-next)))
|#

;;; SRCCOM-LINE-NEXT-IGNORING-EXTRA-NEWLINES -- Internal.
;;;
;;; This is the value of *srccom-line-next* when "Source Compare Ignore Extra
;;; Newlines" is non-nil.
;;;
(defun srccom-line-next-ignoring-extra-newlines (line)
  (if (null line) nil
      (do ((line (line-next line) (line-next line)))
	  ((or (null line) (not (blank-line-p line))) line))))

;;; SRCCOM-IGNORE-CASE-AND-INDENTATION-LINE=	   -- Internal.
;;; SRCCOM-CASE-INSENSITIVE-LINE=		   -- Internal.
;;; SRCCOM-IGNORE-INDENTATION-CASE-SENSITIVE-LINE= -- Internal.
;;; SRCCOM-CASE-SENSITIVE-LINE=			   -- Internal.
;;;
;;; These are the value of *srccom-line-=* depending on the orthogonal values
;;; of "Source Compare Ignore Case" and "Source Compare Ignore Indentation".
;;;
(macrolet ((def-line= (name test &optional ignore-indentation)
	     `(defun ,name (line-a line-b)
		(or (eq line-a line-b)		; if they're both NIL
		    (and line-a
			 line-b
			 (let* ((chars-a (line-string line-a))
				(len-a (length chars-a))
				(chars-b (line-string line-b))
				(len-b (length chars-b)))
			   (declare (simple-string chars-a chars-b))
			   (cond
			    ((and (= len-a len-b)
				  (,test chars-a chars-b)))
			    ,@(if ignore-indentation
				  `((t
				     (flet ((frob (chars len)
					      (dotimes (i len nil)
						(let ((char (schar chars i)))
						  (unless
						      (or (char= char #\space)
							  (char= char #\tab))
						    (return i))))))
				       (let ((i (frob chars-a len-a))
					     (j (frob chars-b len-b)))
					 (if (and i j)
					     (,test chars-a chars-b
						    :start1 i :end1 len-a
						    :start2 j :end2 len-b)
					     )))))))))))))

  (def-line= srccom-ignore-case-and-indentation-line= string-equal t)

  (def-line= srccom-case-insensitive-line= string-equal)

  (def-line= srccom-ignore-indentation-case-sensitive-line= string= t)

  (def-line= srccom-case-sensitive-line= string=))

#|
;;; SRCCOM-CASE-INSENSITIVE-LINE= -- Internal.
;;;
;;; Returns t if line-a and line-b contain STRING-EQUAL text.
;;;
(defun srccom-case-insensitive-line= (line-a line-b)
  (or (eq line-a line-b)		; if they're both NIL
      (and line-a
	   line-b
	   (let ((chars-a (line-string line-a))
		 (chars-b (line-string line-b)))
	     (declare (simple-string chars-a chars-b))
	     (and (= (length chars-a) (length chars-b))
		  (string-equal chars-a chars-b))))))

;;; SRCCOM-CASE-SENSITIVE-LINE= -- Internal.
;;;
;;; Returns t if line-a and line-b contain STRING= text.
;;;
(defun srccom-case-sensitive-line= (line-a line-b)
  (or (eq line-a line-b)		; if they're both NIL
      (and line-a
	   line-b
	   (let ((chars-a (line-string line-a))
		 (chars-b (line-string line-b)))
	     (declare (simple-string chars-a chars-b))
	     (and (= (length chars-a) (length chars-b))
		  (string= chars-a chars-b))))))
|#
