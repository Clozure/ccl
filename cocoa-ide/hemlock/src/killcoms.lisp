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
;;; Killing and unkilling things.
;;;
;;; Written by Bill Chiles and Rob MacLachlan.
;;;

(in-package :hemlock)

(defvar *kill-ring* (make-ring 10) "The Hemlock kill ring.")



;;;; Active Regions.

(defhvar "Active Regions Enabled"
  "When set, some commands that affect the current region only work when the
   region is active."
  :value t)

(defhvar "Highlight Active Region"
  "When set, the active region will be highlighted on the display if possible."
  :value t)


(defvar *ephemerally-active-command-types* (list :ephemerally-active)
  "This is a list of command types that permit the current region to be active
   for the immediately following command.")

(declaim (inline activate-region deactivate-region region-active-p))

(defun %buffer-activate-region (buffer)
  (setf (hi::buffer-region-active buffer) (buffer-signature buffer)))

(defun activate-region ()
  "Make the current region active."
  (%buffer-activate-region (current-buffer)))

(defun %buffer-deactivate-region (buffer)
  (setf (hi::buffer-region-active buffer) nil))

(defun deactivate-region ()
  "Make the current region not active, in the current buffer."
  (%buffer-deactivate-region (current-buffer)))

(defun %buffer-region-active-p (b)
  (eql (buffer-signature b)
       (hi::buffer-region-active b)))

(defun region-active-p ()
  "Returns t or nil, depending on whether the current region is active."
  (%buffer-region-active-p (current-buffer)))

(defun check-region-active ()
  "Signals an error when active regions are enabled and the current region
   is not active."
  (when (and (value active-regions-enabled) (not (region-active-p)))
    (editor-error "The current region is not active.")))

(defun current-region (&optional (error-if-not-active t)
				 (deactivate-region t))
  "Returns a region formed by CURRENT-MARK and CURRENT-POINT, optionally
   signalling an editor error if the current region is not active.  A new
   region is cons'ed on each call.  This optionally deactivates the region."
  (when error-if-not-active (check-region-active))
  (when deactivate-region (deactivate-region))
  (let ((point (current-point))
	(mark (current-mark)))
    (if (mark< mark point) (region mark point) (region point mark))))




(defcommand "Activate Region" (p)
  "Make the current region active.  ^G deactivates the region."
  "Make the current region active."
  (declare (ignore p))
  (activate-region))



(defun control-g-deactivate-region ()
  (deactivate-region))
;;;
(add-hook abort-hook 'control-g-deactivate-region)



;;;; Buffer-Mark primitives and commands.

;;; See Command.Lisp for #'hcmd-make-buffer-hook-fun which makes the
;;; stack for each buffer.

(defun current-mark ()
  "Returns the top of the current buffer's mark stack."
  (buffer-mark (current-buffer)))

(defun buffer-mark (buffer)
  "Returns the top of buffer's mark stack."
  (hi::buffer-%mark buffer))

(defun pop-buffer-mark ()
  "Pops the current buffer's mark stack, returning the mark.  If the stack
   becomes empty, a mark is push on the stack pointing to the buffer's start.
   This always makes the current region not active."
  (let* ((ring (value buffer-mark-ring))
         (buffer (current-buffer))
	 (mark (buffer-mark buffer)))
    (deactivate-region)
    (setf (hi::buffer-%mark buffer)
          (if (zerop (ring-length ring))
            (copy-mark
             (buffer-start-mark (current-buffer)) :right-inserting)
            (ring-pop ring)))
    mark))


(defun %buffer-push-buffer-mark (b mark activate-region)
  (cond ((eq (mark-buffer mark) b)
         (setf (mark-kind mark) :right-inserting)
         (let* ((old-mark (hi::buffer-%mark b)))
           (when old-mark
             (ring-push old-mark (variable-value 'buffer-mark-ring :buffer b))))
         (setf (hi::buffer-%mark b) mark))
        (t (error "Mark not in the current buffer.")))
  (when activate-region (%buffer-activate-region b))
  mark)
        

(defun push-buffer-mark (mark &optional (activate-region nil))
  "Pushes mark into buffer's mark ring, ensuring that the mark is in the right
   buffer and :right-inserting.  Optionally, the current region is made active.
   This never deactivates the current region.  Mark is returned."
  (%buffer-push-buffer-mark (current-buffer) mark activate-region))

(defun push-new-buffer-mark (mark &optional (activate-region nil))
  "Pushes a new mark at argument position"
  (push-buffer-mark (copy-mark mark :right-inserting) activate-region))

(defcommand "Set/Pop Mark" (p)
  "Set or Pop the mark ring.
   With no C-U's, pushes point as the mark, activating the current region.
   With one C-U's, pops the mark into point, de-activating the current region.
   With two C-U's, pops the mark and throws it away, de-activating the current
   region."
  "Set or Pop the mark ring."
  (cond ((not p)
	 (push-new-buffer-mark (current-point) t)
	 (message "Mark pushed."))
	((= p (value universal-argument-default))
	 (pop-and-goto-mark-command nil))
	((= p (expt (value universal-argument-default) 2))
	 (delete-mark (pop-buffer-mark)))
	(t (editor-error))))

(defcommand "Pop and Goto Mark" (p)
  "Pop mark into point, de-activating the current region."
  "Pop mark into point."
  (declare (ignore p))
  (let ((mark (pop-buffer-mark)))
    (move-mark (current-point) mark)
    (delete-mark mark)))

(defcommand "Pop Mark" (p)
  "Pop mark and throw it away, de-activating the current region."
  "Pop mark and throw it away."
  (declare (ignore p))
  (delete-mark (pop-buffer-mark)))

(defcommand "Exchange Point and Mark" (p)
  "Swap the positions of the point and the mark, activating region"
  "Swap the positions of the point and the mark."
  (declare (ignore p))
  (let ((point (current-point))
	(mark (current-mark)))
    (with-mark ((temp point))
      (move-mark point mark)
      (move-mark mark temp)))
  (activate-region))

(defcommand "Mark Whole Buffer"  (p)
  "Set the region around the whole buffer, activating the region.
   Pushes the point on the mark ring first, so two pops get it back.
   With prefix argument, put mark at beginning and point at end."
  "Put point at beginning and part at end of current buffer.
  If P, do it the other way around."
  (let* ((region (buffer-region (current-buffer)))
	 (start (region-start region))
	 (end (region-end region))
	 (point (current-point)))
    (push-new-buffer-mark point)
    (cond (p (push-new-buffer-mark start t)
	     (move-mark point end))
	  (t (push-new-buffer-mark end t)
	     (move-mark point start)))))



;;;; KILL-REGION and KILL-CHARACTERS primitives.

(declaim (special *delete-char-region*))

;;; KILL-REGION first checks for any characters that may need to be added to
;;; the region.  If there are some, we possibly push a region onto *kill-ring*,
;;; and we use the top of *kill-ring*.  If there are no characters to deal
;;; with, then we make sure the ring isn't empty; if it is, just push our
;;; region.  If there is some region in *kill-ring*, then see if the last
;;; command type was a region kill.  Otherwise, just push the region.
;;;
(defun kill-region (region current-type)
  "Kills the region saving it in *kill-ring*.  Current-type is either
   :kill-forward or :kill-backward.  When LAST-COMMAND-TYPE is one of these,
   region is appended or prepended, respectively, to the top of *kill-ring*.
   The killing of the region is undo-able with \"Undo\".  LAST-COMMAND-TYPE
   is set to current-type.  This interacts with KILL-CHARACTERS."
  (let ((last-type (last-command-type))
	(insert-mark (copy-mark (region-start region) :left-inserting)))
    (cond ((or (eq last-type :char-kill-forward)
	       (eq last-type :char-kill-backward))
	   (when *delete-char-region*
	     (kill-ring-push *delete-char-region*)
	     (setf *delete-char-region* nil))
	   (setf region (kill-region-top-of-ring region current-type)))
	  ((zerop (ring-length *kill-ring*))
	   (setf region (delete-and-save-region region))
	   (kill-ring-push region))
	  ((or (eq last-type :kill-forward) (eq last-type :kill-backward))
	   (setf region (kill-region-top-of-ring region current-type)))
	  (t
	   (setf region (delete-and-save-region region))
	   (kill-ring-push region)))
    (make-region-undo :insert "kill" (copy-region region) insert-mark)
    (setf (last-command-type) current-type)))

(defun kill-region-top-of-ring (region current-type)
  (let ((r (ring-ref *kill-ring* 0)))
    (ninsert-region (if (eq current-type :kill-forward)
			(region-end r)
			(region-start r))
		    (delete-and-save-region region))
    r))

(defhvar "Character Deletion Threshold"
  "When this many characters are deleted contiguously via KILL-CHARACTERS,
   they are saved on the kill ring -- for example, \"Delete Next Character\",
   \"Delete Previous Character\", or \"Delete Previous Character Expanding
   Tabs\"."
  :value 5)

(defvar *delete-char-region* nil)
(defvar *delete-char-count* 0)

;;; KILL-CHARACTERS makes sure there are count characters with CHARACTER-OFFSET.
;;; If the last command type was a region kill, we just use the top region
;;; in *kill-ring* by making KILL-CHAR-REGION believe *delete-char-count* is
;;; over the threshold.  We don't call KILL-REGION in this case to save making
;;; undo's -- no good reason.  If we were just called, then increment our
;;; global counter.  Otherwise, make an empty region to keep KILL-CHAR-REGION
;;; happy and increment the global counter.
;;;
(defun kill-characters (mark count)
  "Kills count characters after mark if positive, before mark if negative.
   If called multiple times contiguously such that the sum of the count values
   equals \"Character Deletion Threshold\", then the characters are saved on
   *kill-ring*.  This relies on setting LAST-COMMAND-TYPE, and it interacts
   with KILL-REGION.  If there are not count characters in the appropriate
   direction, no characters are deleted, and nil is returned; otherwise, mark
   is returned."
  (if (zerop count)
      mark
      (with-mark ((temp mark :left-inserting))
	(if (character-offset temp count)
	    (let ((current-type (if (plusp count)
				    :char-kill-forward
				    :char-kill-backward))
		  (last-type (last-command-type))
		  (del-region (if (mark< temp mark)
				  (region temp mark)
				  (region mark temp))))
	      (cond ((or (eq last-type :kill-forward)
			 (eq last-type :kill-backward))
		     (setf *delete-char-count*
			   (value character-deletion-threshold))
		     (setf *delete-char-region* nil))
		    ((or (eq last-type :char-kill-backward)
			 (eq last-type :char-kill-forward))
		     (incf *delete-char-count* (abs count)))
		    (t
		     (setf *delete-char-region* (make-empty-region))
		     (setf *delete-char-count* (abs count))))
	      (kill-char-region del-region current-type)
	      mark)
	    nil))))

(defun kill-char-region (region current-type)
  (let ((deleted-region (delete-and-save-region region)))
    (cond ((< *delete-char-count* (value character-deletion-threshold))
	   (ninsert-region (if (eq current-type :char-kill-forward)
			       (region-end *delete-char-region*)
			       (region-start *delete-char-region*))
			   deleted-region)
	   (setf (last-command-type) current-type))
	  (t
	   (when *delete-char-region*
	     (kill-ring-push *delete-char-region*)
	     (setf *delete-char-region* nil))
	   (let ((r (ring-ref *kill-ring* 0)))
	     (ninsert-region (if (eq current-type :char-kill-forward)
				 (region-end r)
				 (region-start r))
			     deleted-region))
	   (setf (last-command-type)
		 (if (eq current-type :char-kill-forward)
		     :kill-forward
		     :kill-backward))))))

(defun kill-ring-push (region)
  (hi::region-to-clipboard region)
  (ring-push region *kill-ring*))


  


;;;; Commands.

(defcommand "Kill Region" (p)
  "Kill the region, pushing on the kill ring.
   If the region is not active nor the last command a yank, signal an error."
  "Kill the region, pushing on the kill ring."
  (declare (ignore p))
  (kill-region (current-region)
		(if (mark< (current-mark) (current-point))
		    :kill-backward
		    :kill-forward)))

(defcommand "Save Region" (p)
  "Insert the region into the kill ring.
   If the region is not active nor the last command a yank, signal an error."
  "Insert the region into the kill ring."
  (declare (ignore p))
  (kill-ring-push (copy-region (current-region))))

(defcommand "Kill Next Word" (p)
  "Kill a word at the point.
  With prefix argument delete that many words.  The text killed is
  appended to the text currently at the top of the kill ring if it was
  next to the text being killed."
  "Kill p words at the point"
  (let ((point (current-point-for-deletion)))
    (when point
      (let* ((num (or p 1)))
        (with-mark ((mark point :temporary))
          (if (word-offset mark num)
            (if (minusp num)
	      (kill-region (region mark point) :kill-backward)
	      (kill-region (region point mark) :kill-forward))
            (editor-error)))))))

(defcommand "Kill Previous Word" (p)
  "Kill a word before the point.
  With prefix argument kill that many words before the point.  The text
  being killed is appended to the text currently at the top of the kill
  ring if it was next to the text being killed."
  "Kill p words before the point"
  (kill-next-word-command (- (or p 1))))


(defcommand "Kill Line" (p)
  "Kills the characters to the end of the current line.
  If the line is empty then the line is deleted.  With prefix argument,
  deletes that many lines past the point (or before if the prefix is negative)."
  "Kills p lines after the point."
  (let* ((point (current-point-for-deletion)))
    (when point
      (let* ((line (mark-line point)))
        (with-mark ((mark point))
          (cond 
            (p
             (when (and (/= (mark-charpos point) 0) (minusp p))
               (incf p))
             (unless (line-offset mark p 0)
               (if (plusp p)
                 (kill-region (region point (buffer-end mark)) :kill-forward)
                 (kill-region (region (buffer-start mark) point) :kill-backward))
               (editor-error))
             (if (plusp p)
               (kill-region (region point mark) :kill-forward)
               (kill-region (region mark point) :kill-backward)))
            (t
             (cond ((not (blank-after-p mark))
                    (line-end mark))
                   ((line-next line)
                    (line-start mark (line-next line)))
                   ((not (end-line-p mark))
                    (line-end mark))
                   (t 
                    (editor-error)))
             (kill-region (region point mark) :kill-forward))))))))

(defcommand "Backward Kill Line" (p)
  "Kill from the point to the beginning of the line.
  If at the beginning of the line, kill the newline and any trailing space
  on the previous line.  With prefix argument, call \"Kill Line\" with
  the argument negated."
  "Kills p lines before the point."
  (if p
      (kill-line-command (- p))
    (let* ((point (current-point-for-deletion)))
      (when point
        (with-mark ((m point))
          (cond ((zerop (mark-charpos m))
                 (mark-before m)
                 (unless (reverse-find-attribute m :space #'zerop)
                   (buffer-start m)))
                (t
                 (line-start m)))
          (kill-region (region m (current-point)) :kill-backward))))))


(defcommand "Delete Blank Lines" (p)
  "On a blank line, deletes all surrounding blank lines, leaving just
  one. On an isolated blank line, deletes that one. On a non-blank line,
  deletes all blank following that one."
  "Kill blank lines around the point"
  (declare (ignore p))
  (let ((point (current-point-for-deletion)))
    (when point
      (with-mark ((beg-mark point :left-inserting)
                  (end-mark point :right-inserting))
        ;; handle case when the current line is blank
        (when (blank-line-p (mark-line point))
          ;; back up to last non-whitespace character
          (reverse-find-attribute beg-mark :whitespace #'zerop)
          (when (previous-character beg-mark)
            ;; that is, we didn't back up to the beginning of the buffer
            (unless (same-line-p beg-mark end-mark)
              (line-offset beg-mark 1 0)))
          ;; if isolated, zap the line else zap the blank ones above
          (cond ((same-line-p beg-mark end-mark)
                 (line-offset end-mark 1 0))
                (t
                 (line-start end-mark)))
          (delete-region (region beg-mark end-mark)))
        ;; always delete all blank lines after the current line
        (move-mark beg-mark point)
        (when (line-offset beg-mark 1 0)
          (move-mark end-mark beg-mark)
          (find-attribute end-mark :whitespace #'zerop)
          (when (next-character end-mark)
            ;; that is, we didn't go all the way to the end of the buffer
            (line-start end-mark))
          (delete-region (region beg-mark end-mark)))))))


(defcommand "Un-Kill" (p)
  "Inserts the top item in the kill-ring at the point.
  The mark is left mark before the insertion and the point after.  With prefix
  argument inserts the prefix'th most recent item."
  "Inserts the item with index p in the kill ring at the point, leaving 
  the mark before and the point after."
  (let ((idx (1- (or p 1))))
    (cond ((> (ring-length *kill-ring*) idx -1)
	   (let* ((region (ring-ref *kill-ring* idx))
		  (point (current-point-for-insertion))
		  (mark (push-new-buffer-mark point)))
	     (insert-region point region)
	     (make-region-undo :delete "Un-Kill"
			       (region (copy-mark mark) (copy-mark point))))
	   (setf (last-command-type) :unkill))
	  (t (editor-error)))))
;;;
(push :unkill *ephemerally-active-command-types*)

(defcommand "Rotate Kill Ring" (p)
  "Replace un-killed text with previously killed text.
  Kills the current region, rotates the kill ring, and inserts the new top
  item.  With prefix argument rotates the kill ring that many times."
  "This function will not behave in any reasonable fashion when
  called as a lisp function."
  (let ((point (current-point))
        (mark (current-mark)))
    (cond ((or (not (eq (last-command-type) :unkill))
	       (zerop (ring-length *kill-ring*)))
	   (editor-error))
	  (t (delete-region (region mark point))
	     (rotate-ring *kill-ring* (or p 1))
	     (insert-region point (ring-ref *kill-ring* 0))
	     (make-region-undo :delete "Un-Kill"
			       (region (copy-mark mark) (copy-mark point)))
	     (setf (last-command-type) :unkill)))))
