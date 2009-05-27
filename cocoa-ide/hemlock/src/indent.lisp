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
;;; Hemlock indentation commands
;;;
;;; Written by Bill Maddox and Bill Chiles
;;;
(in-package :hemlock)



(defhvar "Spaces per Tab"
  "The number of spaces a tab is equivalent to.  NOTE: This is not incorporated
   everywhere in Hemlock yet, so do not change it."
  :value 8)

(defhvar "Indent with Tabs"
  "If true, indentation is done using a mixture of tabs and spaces.  If false,
   only spaces are used."
  :value nil)


(defun indent-using-tabs (mark column)
  "Inserts at mark a maximum number of tabs and a minimum number of spaces to
   move mark to column.  This assumes mark is at the beginning of a line."
  (multiple-value-bind (tabs spaces) (floor column (value spaces-per-tab))
    (dotimes (i tabs) (insert-character mark #\tab))
    (dotimes (i spaces) (insert-character mark #\space))))

(defun indent-using-spaces (mark column)
  "Inserts some spaces at MARK so that it moves to COLUMN.  This assumes
   mark is at the beginning of a line."
  (insert-string mark (make-string column :initial-element #\space)))


(defun indent-to-column (mark column)
  "Inserts whitespace to move MARK to COLUMN, assuming mark is at column 0"
  (if (value indent-with-tabs)
      (indent-using-tabs mark column)
      (indent-using-spaces mark column)))

(defun indent-to-tab-stop (mark)
  (if (value indent-with-tabs)
      (insert-character mark #\tab)
      (let* ((tab (value spaces-per-tab)))
	(dotimes (i (- tab (mod (mark-column mark) tab)))
	  (insert-character mark #\space)))))

(defhvar "Indent Function"
  "Indentation function which is invoked by \"Indent\" command.
   It takes a :left-inserting mark that may be moved."
  :value #'indent-to-tab-stop)


(defun generic-indent (mark)
  (let* ((line (mark-line mark))
	 (prev (do ((line (line-previous line) (line-previous line)))
		   ((or (null line) (not (blank-line-p line))) line))))
    (unless prev (editor-error))
    (line-start mark prev)
    (find-attribute mark :space #'zerop)
    (let ((indentation (mark-column mark)))
      (line-start mark line)
      (delete-horizontal-space mark)
      (indent-to-column mark indentation))))


(defcommand "Indent New Line" (p)
  "Moves point to a new blank line and indents it.
   Any whitespace before point is deleted.  The value of \"Indent Function\"
   is used for indentation unless there is a Fill Prefix, in which case it is
   used.  Any argument is passed onto \"New Line\"."
  "Moves point to a new blank line and indents it.
   Any whitespace before point is deleted.  The value of \"Indent Function\"
   is used for indentation unless there is a Fill Prefix, in which case it is
   used.  Any argument is passed onto \"New Line\"."
  (let ((point (current-point))
	(prefix (value fill-prefix)))
    (delete-horizontal-space point)
    (new-line-command p)
    (if prefix
	(insert-string point prefix)
	(funcall (value indent-function) point))))


(defcommand "Indent" (p)
  "Invokes function held by the Hemlock variable \"Indent Function\",
   moving point past region if called with argument."
  "Invokes function held by the Hemlock variable \"Indent Function\"
   moving point past region if called with argument."
  (let ((point (current-point)))
    (with-mark ((mark point :left-inserting))
      (cond ((or (not p) (zerop p))
	     (funcall (value indent-function) mark)
             (when (mark< point mark)
               (move-mark point mark)))
	    (t
	     (if (plusp p)
		 (unless (line-offset point (1- p))
		   (buffer-end point))
		 (unless (line-offset mark (1+ p))
		   (buffer-start mark)))
	     (indent-region-for-commands (region mark point))
	     (find-attribute (line-start point) :whitespace #'zerop))))))

(defcommand "Indent Region" (p)
  "Invokes function held by Hemlock variable \"Indent Function\" on every
   line between point and mark, inclusively."
  "Invokes function held by Hemlock variable \"Indent Function\" on every
   line between point and mark, inclusively."
  (declare (ignore p))
  (let* ((region (current-region)))
    (with-mark ((start (region-start region) :left-inserting)
		(end (region-end region) :left-inserting))
      (indent-region-for-commands (region start end)))))

(defun indent-region-for-commands (region)
  "Indents region undoably with INDENT-REGION."
  (let* ((start (region-start region))
	 (end (region-end region))
	 (undo-region (copy-region (region (line-start start) (line-end end)))))
    (indent-region region)
    (make-region-undo :twiddle "Indent"
		      (region (line-start (copy-mark start :left-inserting))
			      (line-end (copy-mark end :right-inserting)))
		      undo-region)))

(defun indent-region (region)
  "Invokes function held by Hemlock variable \"Indent Function\" on every
   line of region."
  (let ((indent-function (value indent-function)))
    (with-mark ((start (region-start region) :left-inserting)
		(end (region-end region)))
      (line-start start)
      (line-start end)
      (loop (when (mark= start end)
	      (funcall indent-function start)
	      (return))
	    (funcall indent-function start)
	    (line-offset start 1 0)))))

(defcommand "Center Line" (p)
  "Centers current line using \"Fill Column\".  If an argument is supplied,
   it is used instead of the \"Fill Column\"."
  "Centers current line using fill-column."
  (let* ((region (if (region-active-p)
		     (current-region)
		     (region (current-point) (current-point))))
	 (end (region-end region)))
    (with-mark ((temp (region-start region) :left-inserting))
      (loop
	(when (mark> temp end) (return))
	(delete-horizontal-space (line-end temp))
	(delete-horizontal-space (line-start temp))
	(let* ((len (line-length (mark-line temp)))
	       (spaces (- (or p (value fill-column)) len)))
	  (if (and (plusp spaces) 
		   (not (zerop len)))
	      (indent-to-column temp (ceiling spaces 2)))
	  (unless (line-offset temp 1) (return))
	  (line-start temp))))))


(defcommand "Quote Tab" (p)
  "Insert tab character."
  "Insert tab character."
  (if (and p (> p 1))
      (insert-string (current-point) (make-string p :initial-element #\tab))
      (insert-character (current-point) #\tab)))


(defcommand "Open Line" (p)
  "Inserts a newline into the buffer without moving the point."
  "Inserts a newline into the buffer without moving the point.
  With argument, inserts p newlines."
  (let ((point (current-point-collapsing-selection))
	(count (if p p 1)))
    (if (not (minusp count))
	(dotimes (i count)
	  (insert-character point #\newline)
	  (mark-before point))
	(editor-error))))


(defcommand "New Line" (p)
  "Moves the point to a new blank line.
  A newline is inserted.
  With an argument, repeats p times."
  "Moves the point to a new blank line."
  (let ((point (current-point-for-insertion))
	(count (if p p 1)))
    (if (not (minusp count))
      (dotimes (i count) (insert-character point #\newline))
      (editor-error))))




(defattribute "Space"
  "This attribute is used by the indentation commands to determine which
  characters are treated as space."
  '(mod 2) 0)

(setf (character-attribute :space #\space) 1)
(setf (character-attribute :space #\tab) 1)

(defun delete-horizontal-space (mark)
  "Deletes all :space characters on either side of mark."
  (with-mark ((start mark))
    (reverse-find-attribute start :space #'zerop)
    (find-attribute mark :space #'zerop)
    (delete-region (region start mark))))



(defcommand "Delete Indentation" (p)
  "Join current line with the previous one, deleting excess whitespace.
  All whitespace is replaced with a single space, unless it is at the beginning
  of a line, immmediately following a \"(\", or immediately preceding a \")\",
  in which case the whitespace is merely deleted.  If the preceeding character
  is a sentence terminator, two spaces are left instead of one.  If a prefix
  argument is given, the following line is joined with the current line."
  "Join current line with the previous one, deleting excess whitespace."
  (with-mark ((m (current-point) :right-inserting))
    (when p (line-offset m 1))
    (line-start m)
    (unless (delete-characters m -1) (editor-error "No previous line."))
    (delete-horizontal-space m)
    (let ((prev (previous-character m)))
      (when (and prev (char/= prev #\newline))
	(cond ((not (zerop (character-attribute :sentence-terminator prev)))
	       (insert-string m "  "))
	      ((not (or (eq (character-attribute :lisp-syntax prev) :open-paren)
			(eq (character-attribute :lisp-syntax (next-character m))
			    :close-paren)))
	       (insert-character m #\space)))))))


(defcommand "Delete Horizontal Space" (p)
  "Delete spaces and tabs surrounding the point."
  "Delete spaces and tabs surrounding the point."
  (declare (ignore p))
  (delete-horizontal-space (current-point)))

(defcommand "Just One Space" (p)
  "Leave one space.
  Surrounding space is deleted, and then one space is inserted.
  with prefix argument insert that number of spaces."
  "Delete surrounding space and insert P spaces."
  (let ((point (current-point)))
    (delete-horizontal-space point)
    (dotimes (i (or p 1)) (insert-character point #\space))))

(defcommand "Back to Indentation" (p)
  "Move point to the first non-whitespace character on the line."
  "Move point to the first non-whitespace character on the line."
  (declare (ignore p))
  (let ((point (current-point)))
    (line-start point)
    (find-attribute point :whitespace #'zerop)))

(defcommand "Indent Rigidly" (p)
  "Indent the region rigidly by p spaces.
   Each line in the region is moved p spaces to the right (left if p is
   negative).  When moving a line to the left, tabs are converted to spaces."
  "Indent the region rigidly p spaces to the right (left if p is negative)."
  (let ((p (or p (value spaces-per-tab)))
	(region (current-region)))
    (with-mark ((mark1 (region-start region) :left-inserting)
		(mark2 (region-end region) :left-inserting))
      (line-start mark1)
      (line-start mark2)
      (do ()
	  ((mark= mark1 mark2))
	(cond ((empty-line-p mark1))
	      ((blank-after-p mark1)
	       (delete-characters mark1 (line-length (mark-line mark1))))
	      (t (find-attribute mark1 :whitespace #'zerop)
		 (let ((new-column (+ p (mark-column mark1))))
		   (delete-characters mark1 (- (mark-charpos mark1)))
		   (if (plusp new-column)
		       (indent-to-column mark1 new-column)))))
	(line-offset mark1 1 0)))))
