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
;;; More Hemlock Text-Manipulation functions.
;;; Written by Skef Wholey and Rob MacLachlan.
;;; Modified by Bill Chiles.
;;; 
;;; The code in this file implements the delete and copy functions in the
;;; "Doing Stuff and Going Places" chapter of the Hemlock Design document.
;;;

(in-package :hemlock-internals)


;;;; DELETE-CHARACTERS.

(defun delete-characters (mark &optional (n 1))
  "Deletes N characters after the mark (or -N before if N is negative)."
  (let* ((line (mark-line mark))
	 (charpos (mark-charpos mark))
	 (length (line-length* line)))
    (check-buffer-modification (line-%buffer line) mark)
    (cond
     ((zerop n) t)
     ;; Deleting chars on one line, just bump the pointers.
     ((<= 0 (+ charpos n) length)
      (let* ((buffer (line-%buffer line)))
        (modifying-buffer buffer
          (modifying-line line mark)
          (cond
           ((minusp n)
            (delete-line-charprops line :start (+ charpos n) :end charpos)
            (setf (current-left-open-pos) (+ (current-left-open-pos) n))
            (move-some-marks (pos line)
                             (if (> pos (current-left-open-pos))
                               (if (<= pos charpos) (current-left-open-pos) (+ pos n))
                               pos)))
           
           (t
            (delete-line-charprops line :start charpos :end (+ charpos n))
            (setf (current-right-open-pos) (+ (current-right-open-pos) n))
            (let ((bound (+ charpos n)))
              (move-some-marks (pos line)
                               (if (> pos charpos)
                                 (if (<= pos bound) (current-left-open-pos) (- pos n))
                                 pos)))))
          (adjust-line-origins-forward line)
          (buffer-note-deletion buffer mark n)
          t)))
     
     ;; Deleting some newlines, punt out to delete-region.
     (t
      (let* ((temp-mark (mark line charpos))
             (other-mark (character-offset temp-mark n))
             (temp-region (make-empty-region)))
        (cond
         (other-mark
          (if (< n 0)
            (setf (region-start temp-region) other-mark
                  (region-end temp-region) mark)
            (setf (region-start temp-region) mark
                  (region-end temp-region) other-mark))
          (delete-region temp-region) t)
         (t nil)))))))

;;;; DELETE-REGION.

(defun delete-region (region)
  "Deletes the Region."
  (let* ((start (region-start region))
	 (end (region-end region))
	 (first-line (mark-line start))
	 (last-line (mark-line end))
	 (first-charpos (mark-charpos start))
	 (last-charpos (mark-charpos end))
	 (buffer (line-%buffer first-line))
         (ndel (count-characters region)))
    (unless (and (eq first-line last-line)
		 (= first-charpos last-charpos))
      (modifying-buffer buffer
	(cond ((eq first-line last-line)
	       ;; Simple case -- just skip over the characters:
	       (modifying-line first-line start)
	       (let ((num (- last-charpos first-charpos)))
		 (setf (current-right-open-pos) (+ (current-right-open-pos) num))
		 ;; and fix up any charprops or marks in there:
                 (delete-line-charprops first-line :start first-charpos
                                        :end last-charpos)
		 (move-some-marks (charpos first-line)
		   (if (> charpos first-charpos)
		       (if (<= charpos last-charpos) 
			   first-charpos
			   (- charpos num))
		       charpos))))
	      (t
	       ;; hairy case -- squish lines together:
	       (close-line)
	       (let* ((first-chars (line-chars first-line))
		      (last-chars (line-chars last-line))
		      (last-length (length last-chars)))
		 (declare (simple-string last-chars first-chars))
		 ;; Cons new chars for the first line.
		 (let* ((length (+ first-charpos (- last-length last-charpos)))
			(new-chars (make-string length)))
		   (%sp-byte-blt first-chars 0 new-chars 0 first-charpos)
		   (%sp-byte-blt last-chars last-charpos new-chars first-charpos
				 length)
		   (setf (line-chars first-line) new-chars))
                 (copy-line-charprops last-line :start last-charpos
                                      :end last-length)
		 ;; fix up the first line's marks:
		 (move-some-marks (charpos first-line)
		   (if (> charpos first-charpos)
		       first-charpos
		       charpos))
		 ;; fix up the marks of the lines in the middle and mash
		 ;;line-%buffer:
		 (do* ((line (line-next first-line) (line-next line))
		       (count (next-disembodied-buffer-counter)))
		      ((eq line last-line)
		       (setf (line-%buffer last-line) count))
		   (setf (line-%buffer line) count)
		   (move-some-marks (ignore line first-line)
		     (declare (ignore ignore))
		     first-charpos))
		 ;; and fix up the last line's marks:
		 (move-some-marks (charpos last-line first-line)
		   (if (<= charpos last-charpos)
		       first-charpos
		       (+ (- charpos last-charpos)
			  first-charpos)))
		 ;; And splice the losers out:
		 (let ((next (line-next last-line)))
		   (setf (line-next first-line) next)
		   (when next (setf (line-previous next) first-line))))))
        (adjust-line-origins-forward first-line)
        (buffer-note-deletion buffer start ndel)))))



;;;; DELETE-AND-SAVE-REGION.

(defun delete-and-save-region (region)
  "Deletes Region and returns a region containing the deleted characters."
  (let* ((start (region-start region))
	 (end (region-end region))
	 (first-line (mark-line start))
	 (last-line (mark-line end))
	 (first-charpos (mark-charpos start))
	 (last-charpos (mark-charpos end))
	 (buffer (line-%buffer first-line))
         (ndel (count-characters region)))
    (check-buffer-modification buffer start)
    (check-buffer-modification buffer end)
    (cond
      ((and (eq first-line last-line)
            (= first-charpos last-charpos))
       (make-empty-region))
      (t
       (modifying-buffer
        buffer
        (prog1
            (cond ((eq first-line last-line)
                   ;; simple case -- just skip over the characters:
                   (modifying-line first-line start)
                   (let* ((num (- last-charpos first-charpos))
                          (new-right (+ (current-right-open-pos) num))
                          (new-chars (make-string num))
                          (new-line (make-line
                                     :chars new-chars  :number 0
                                     :charprops-changes
                                     (copy-line-charprops first-line
                                                          :start first-charpos
                                                          :end last-charpos)
                                     :%buffer (next-disembodied-buffer-counter))))
                     (declare (simple-string new-chars))
                     (%sp-byte-blt (current-open-chars) (current-right-open-pos) new-chars 0 num) 
                     (setf (current-right-open-pos) new-right)
                     ;; and fix up any charprops or marks in there:
                     (delete-line-charprops first-line :start first-charpos
					    :end last-charpos)
                     (move-some-marks (charpos first-line)
                                      (if (> charpos first-charpos)
                                        (if (<= charpos last-charpos)
                                          first-charpos
                                          (- charpos num))
                                        charpos))
                     ;; And return the region with the nuked characters:
                     (internal-make-region (mark new-line 0 :right-inserting)
                                           (mark new-line num :left-inserting))))
                  (t
                   ;; hairy case -- squish lines together:
                   (close-line)
                   (let* ((first-chars (line-chars first-line))
                          (last-chars (line-chars last-line))
                          (first-length (length first-chars))
                          (last-length (length last-chars))
                          (saved-first-length (- first-length first-charpos))
                          (saved-first-chars (make-string saved-first-length))
                          (saved-last-chars (make-string last-charpos))
                          (count (next-disembodied-buffer-counter))
                          (saved-line (make-line :chars saved-first-chars
                                                 :%buffer count)))
                     (declare (simple-string first-chars last-chars
                                             saved-first-chars saved-last-chars))
                     ;; Cons new chars for victim line.
                     (let* ((length (+ first-charpos (- last-length last-charpos)))
                            (new-chars (make-string length)))
                       (%sp-byte-blt first-chars 0 new-chars 0 first-charpos)
                       (%sp-byte-blt last-chars last-charpos new-chars first-charpos
                                     length)
                       (setf (line-chars first-line) new-chars))
                     ;; Make a region with all the lost stuff:
                     (%sp-byte-blt first-chars first-charpos saved-first-chars 0
                                   saved-first-length)
                     (%sp-byte-blt last-chars 0 saved-last-chars 0 last-charpos)
                     ;; Mash the chars and buff of the last line.
                     (setf (line-chars last-line) saved-last-chars
                           (line-%buffer last-line) count)
                     ;; fix up the marks of the lines in the middle and mash
                     ;;line-%buffer:
                     (do ((line (line-next first-line) (line-next line)))
                         ((eq line last-line)
                          (setf (line-%buffer last-line) count))
                       (setf (line-%buffer line) count)
                       (move-some-marks (ignore line first-line)
                                        (declare (ignore ignore))
                                        first-charpos))
                     ;; And splice the losers out:
                     (let ((next (line-next first-line))
                           (after (line-next last-line)))
                       (setf (line-next saved-line) next
                             (line-previous next) saved-line
                             (line-next first-line) after)
                       (when after
                         (setf (line-previous after) first-line
                               (line-next last-line) nil)))
                     
                     ;; fix up the first line's marks:
                     (move-some-marks (charpos first-line)
                                      (if (> charpos first-charpos)
                                        first-charpos
                                        charpos))
                     ;; and fix up the last line's marks:
                     (move-some-marks (charpos last-line first-line)
                                      (if (<= charpos last-charpos)
                                        first-charpos
                                        (+ (- charpos last-charpos)
                                           first-charpos)))
                     ;; And return the region with the nuked characters:
                     (renumber-region
                      (internal-make-region
                       (mark saved-line 0 :right-inserting)
                       (mark last-line last-charpos :left-inserting))))))
          (adjust-line-origins-forward first-line)
          (buffer-note-deletion buffer start ndel)))))))



;;;; COPY-REGION.

(defun copy-region (region)
  "Returns a region containing a copy of the text within Region."
  (let* ((start (region-start region))
	 (end (region-end region))
	 (first-line (mark-line start))
	 (last-line (mark-line end))
	 (first-charpos (mark-charpos start))
	 (last-charpos (mark-charpos end))
	 (count (next-disembodied-buffer-counter)))
    (cond
     ((eq first-line last-line)
      (when (current-open-line-p first-line) (close-line))
      (let* ((length (- last-charpos first-charpos))
	     (chars (make-string length))
	     (line (make-line :chars chars  :%buffer count  :number 0)))
	(%sp-byte-blt (line-chars first-line) first-charpos chars 0 length)
        (setf (line-charprops-changes line)
              (copy-line-charprops line :start first-charpos :end last-charpos))
	(internal-make-region (mark line 0 :right-inserting)
			      (mark line length :left-inserting))))
     (t
      (close-line)
      (let* ((first-chars (line-chars first-line))
	     (length (- (length first-chars) first-charpos))
	     (chars (make-string length))
	     (first-copied-line (make-line :chars chars  :%buffer count
					   :number 0)))
	(declare (simple-string first-chars))
	(%sp-byte-blt first-chars first-charpos chars 0 length)
        (setf (line-charprops-changes first-copied-line)
              (copy-line-charprops first-line :start first-charpos
                                   :end last-charpos))
	(do ((line (line-next first-line) (line-next line))
	     (previous first-copied-line)
	     (number line-increment (+ number line-increment)))
	    ((eq line last-line)
	     (let* ((chars (make-string last-charpos))
		    (last-copied-line (make-line :chars chars
						 :number number
						 :%buffer count
						 :previous previous)))
	       (%sp-byte-blt (line-chars last-line) 0 chars 0 last-charpos)
               (setf (line-charprops-changes last-copied-line)
                     (copy-line-charprops last-line :end last-charpos))
	       (setf (line-next previous) last-copied-line)
	       (internal-make-region
		(mark first-copied-line 0 :right-inserting)
		(mark last-copied-line last-charpos :left-inserting))))
	  (let* ((new-line (%copy-line line :%buffer count
				       :number number
				       :previous previous)))
            ;; note that %copy-line also copies charprops changes
	    (setf (line-next previous) new-line)
	    (setq previous new-line))))))))



;;;; FILTER-REGION.

(eval-when (:compile-toplevel :execute)
(defmacro fcs (fun str)
  `(let ((rs (funcall ,fun ,str)))
     (if (simple-string-p rs) rs
	 (coerce rs 'simple-string))))
); eval-when

;;; FILTER-REGION  --  Public
;;;
;;;    After we deal with the nasty boundry conditions of the first and
;;; last lines, we just scan through lines in the region replacing their
;;; chars with the result of applying the function to the chars.
;;;
(defun filter-region (function region)
  "This function filters the text in a region though a Lisp function.  The
   argument function must map from a string to a string.  It is passed each
   line string from region in order, and each resulting string replaces the
   original.  The function must neither destructively modify its argument nor
   modify the result string after it is returned.  The argument will always be
   a simple-string.  It is an error for any string returned to contain
   newlines."
  (let* ((start (region-start region))
         (count (hemlock::count-characters region))
         (origin (copy-mark start :right-inserting))
	 (start-line (mark-line start))
	 (first (mark-charpos start))
	 (end (region-end region))
	 (end-line (mark-line end))
	 (last (mark-charpos end))
	 (marks ())
         (buffer (line-%buffer start-line)))
    (check-buffer-modification buffer start)
    (check-buffer-modification buffer end)
    (modifying-buffer buffer
      (modifying-line end-line end)
      (cond ((eq start-line end-line)
	     (let* ((res (fcs function (subseq (current-open-chars) first last)))
		    (rlen (length res))
		    (new-left (+ first rlen))
		    (delta (- new-left (current-left-open-pos))))
	       (declare (simple-string res))
	       (when (> new-left (current-right-open-pos))
		 (grow-open-chars (+ new-left (current-line-cache-length))))
	       (%sp-byte-blt res 0 (current-open-chars) first (current-left-open-pos))
	       ;;
	       ;; Move marks to start or end of region, depending on kind.
	       (dolist (m (line-marks start-line))
		 (let ((charpos (mark-charpos m)))
		   (when (>= charpos first)
		     (setf (mark-charpos m)
			   (if (<= charpos last)
			       (if (eq (mark-%kind m) :left-inserting)
				   new-left first)
			       (+ charpos delta))))))
	       (setf (current-left-open-pos) new-left)))
	    (t
	     ;;
	     ;; Do the chars for the first line.
	     (let* ((first-chars (line-chars start-line))
		    (first-len (length first-chars))
		    (res (fcs function (subseq first-chars first first-len)))
		    (rlen (length res))
		    (nlen (+ first rlen))
		    (new (make-string nlen)))
	       (declare (simple-string res first-chars new))
	       (%sp-byte-blt first-chars 0 new 0 first)
	       (%sp-byte-blt res 0 new first nlen)
	       (setf (line-%chars start-line) new))
	     ;;
	     ;; Fix up marks on the first line, saving any within the region
	     ;; to be dealt with later.
	     (let ((outside ()))
	       (dolist (m (line-marks start-line))
		 (if (<= (mark-charpos m) first)
		     (push m outside) (push m marks)))
	       (setf (line-marks start-line) outside))
	     ;;
	     ;; Do chars of intermediate lines in the region, saving their
	     ;; marks.
	     (do ((line (line-next start-line) (line-next line)))
		 ((eq line end-line))
	       (when (line-marks line)
		 (setq marks (nconc (line-marks line) marks))
		 (setf (line-marks line) nil))
	       (setf (line-%chars line) (fcs function (line-chars line))))
	     ;;
	     ;; Do the last line, which is cached.
	     (let* ((res (fcs function (subseq (the simple-string (current-open-chars))
					       0 last)))
		    (rlen (length res))
		    (delta (- rlen last)))
	       (declare (simple-string res))
	       (when (> rlen (current-right-open-pos))
		 (grow-open-chars (+ rlen (current-line-cache-length))))
	       (%sp-byte-blt res 0 (current-open-chars) 0 rlen)
	       (setf (current-left-open-pos) rlen)
	       ;;
	       ;; Adjust marks after the end of the region and save ones in it.
	       (let ((outside ()))
		 (dolist (m (line-marks end-line))
		   (let ((charpos (mark-charpos m)))
		     (cond ((> charpos last)
			    (setf (mark-charpos m) (+ charpos delta))
			    (push m outside))
			   (t
			    (push m marks)))))
		 (setf (line-marks end-line) outside))
	       ;;
	       ;; Scan over saved marks, moving them to the correct end of the
	       ;; region.
	       (dolist (m marks)
		 (cond ((eq (mark-%kind m) :left-inserting)
			(setf (mark-charpos m) rlen)
			(setf (mark-line m) end-line)
			(push m (line-marks end-line)))
		       (t
			(setf (mark-charpos m) first)
			(setf (mark-line m) start-line)
			(push m (line-marks start-line)))))))))
    (hi::buffer-note-modification buffer origin count)
    (delete-mark origin)
    region))
