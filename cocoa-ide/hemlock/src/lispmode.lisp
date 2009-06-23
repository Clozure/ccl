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
;;; Hemlock LISP Mode commands
;;;
;;; Written by Ivan Vazquez and Bill Maddox.
;;;

(in-package :hemlock)

;; (declaim (optimize (speed 2))); turn off byte compilation.


;;;; Variables and lisp-info structure.

;;; These routines are used to define, for standard LISP mode, the start and end
;;; of a block to parse.  If these need to be changed for a minor mode that sits
;;; on top of LISP mode, simply do a DEFHVAR with the minor mode and give the
;;; name of the function to use instead of START-OF-PARSE-BLOCK and 
;;; END-OF-PARSE-BLOCK.
;;; 

(defhvar "Parse Start Function"
  "Take a mark and move it to the top of a block for paren parsing."
  :value 'start-of-parse-block)

(defhvar "Parse End Function"
  "Take a mark and move it to the bottom of a block for paren parsing."
  :value 'end-of-parse-block)

	    
;;; LISP-INFO is the structure used to store the data about the line in its
;;; Plist.
;;;
;;;     -> BEGINS-QUOTED, ENDING-QUOTED are both Boolean slots that tell whether
;;;        or not a line's begining and/or ending are quoted.
;;; 
;;;     -> RANGES-TO-IGNORE is a list of cons cells, each having the form
;;;        ( [begining-charpos] [end-charpos] ) each of these cells indicating
;;;        a range to ignore.  End is exclusive.
;;; 
;;;     -> NET-OPEN-PARENS, NET-CLOSE-PARENS integers that are the number of 
;;;        unmatched opening and closing parens that there are on a line.
;;; 
;;;     -> SIGNATURE-SLOT ...
;;; 

(defstruct (lisp-info (:constructor make-lisp-info ()))
  (begins-quoted nil)		; (or t nil)
  (ending-quoted nil)		; (or t nil)
  (ranges-to-ignore nil)	; (or t nil)
  (net-open-parens 0 :type fixnum)
  (net-close-parens 0 :type fixnum)
  (signature-slot))



;;;; Macros.

;;; The following Macros exist to make it easy to acces the Syntax primitives
;;; without uglifying the code.  They were originally written by Maddox.
;;; 

(defmacro scan-char (mark attribute values)
  `(find-attribute ,mark ',attribute ,(attr-predicate values)))

(defmacro rev-scan-char (mark attribute values)
  `(reverse-find-attribute ,mark ',attribute ,(attr-predicate values)))

(defmacro test-char (char attribute values)
  `(let ((x (character-attribute ',attribute ,char)))
     ,(attr-predicate-aux values)))

(eval-when (:compile-toplevel :execute :load-toplevel)
(defun attr-predicate (values)
  (cond ((eq values 't)
	 '#'plusp)
	((eq values 'nil)
	 '#'zerop)
	(t `#'(lambda (x) ,(attr-predicate-aux values)))))

(defun attr-predicate-aux (values)
  (cond ((eq values t)
	 '(plusp x))
	((eq values nil)
	 '(zerop x))
	((symbolp values)
	 `(eq x ',values))
	((and (listp values) (member (car values) '(and or not)))
	 (cons (car values) (mapcar #'attr-predicate-aux (cdr values))))
	(t (error "Illegal form in attribute pattern - ~S" values))))

); Eval-When

;;; 
;;; FIND-LISP-CHAR

(defmacro find-lisp-char (mark)
  "Move MARK to next :LISP-SYNTAX character, if one isn't found, return NIL."
  `(find-attribute ,mark :lisp-syntax
		   #'(lambda (x)
		       (member x '(:open-paren :close-paren :newline :comment
					       :char-quote :string-quote))))) 
;;; 
;;; PUSH-RANGE

(defmacro push-range (new-range info-struct)
  "Insert NEW-RANGE into the LISP-INFO-RANGES-TO-IGNORE slot of the INFO-STRUCT."
  `(when ,new-range
     (setf (lisp-info-ranges-to-ignore ,info-struct) 
	   (cons ,new-range (lisp-info-ranges-to-ignore ,info-struct)))))
;;; 
;;; SCAN-DIRECTION

(defmacro scan-direction (mark forwardp &rest forms)
  "Expand to a form that scans either backward or forward according to Forwardp."
  (if forwardp
      `(scan-char ,mark ,@forms)
      `(rev-scan-char ,mark ,@forms)))
;;; 
;;; DIRECTION-CHAR

(defmacro direction-char (mark forwardp)
  "Expand to a form that returns either the previous or next character according
  to Forwardp."
  (if forwardp
      `(next-character ,mark)
      `(previous-character ,mark)))

;;; 
;;; NEIGHBOR-MARK

(defmacro neighbor-mark (mark forwardp)
  "Expand to a form that moves MARK either backward or forward one character, 
  depending on FORWARDP."
  (if forwardp
      `(mark-after ,mark)
      `(mark-before ,mark)))

;;; 
;;; NEIGHBOR-LINE

(defmacro neighbor-line (line forwardp)
  "Expand to return the next or previous line, according to Forwardp."
  (if forwardp
      `(line-next ,line)
      `(line-previous ,line)))


;;;; Parsing functions.

;;; PRE-COMMAND-PARSE-CHECK -- Public.
;;;
(defun pre-command-parse-check (mark &optional (fer-sure-parse nil))
  "Parse the area before the command is actually executed."
  (with-mark ((top mark)
	      (bottom mark))
    (funcall (value parse-start-function) top)
    (funcall (value parse-end-function) bottom)
    (parse-over-block (mark-line top) (mark-line bottom) fer-sure-parse)))

;;; PARSE-OVER-BLOCK
;;;
(defun parse-over-block (start-line end-line &optional (fer-sure-parse nil))
  "Parse over an area indicated from END-LINE to START-LINE."
  (let ((test-line start-line)
	prev-line-info)
    
    (with-mark ((mark (mark test-line 0)))
      
      ; Set the pre-begining and post-ending lines to delimit the range
      ; of action any command will take.  This means set the lisp-info of the 
      ; lines immediately before and after the block to Nil.
      
      (when (line-previous start-line)
	(setf (getf (line-plist (line-previous start-line)) 'lisp-info) nil))
      (when (line-next end-line)
	(setf (getf (line-plist (line-next end-line)) 'lisp-info) nil))
      
      (loop
       (let ((line-info (getf (line-plist test-line) 'lisp-info)))
	 
	 ;;    Reparse the line when any of the following are true:
	 ;;
	 ;;      FER-SURE-PARSE is T
	 ;;
	 ;;      LINE-INFO or PREV-LINE-INFO are Nil.
	 ;;
	 ;;      If the line begins quoted and the previous one wasn't 
	 ;;      ended quoted.
	 ;;
	 ;;      The Line's signature slot is invalid (the line has changed).
	 ;;
	 
	 (when (or fer-sure-parse      
		   (not line-info)     
		   (not prev-line-info)
		   
		   (not (eq (lisp-info-begins-quoted line-info) 
			    (lisp-info-ending-quoted prev-line-info)))
		   
		   (not (eql (line-signature test-line)     
			     (lisp-info-signature-slot line-info))))
	   
	   (move-to-position mark 0 test-line)
	   
	   (unless line-info
	     (setf line-info (make-lisp-info))
	     (setf (getf (line-plist test-line) 'lisp-info) line-info))
	   
	   (parse-lisp-line-info mark line-info prev-line-info))
	 
	 (when (eq end-line test-line)
	   (return nil))
	 
	 (setq prev-line-info line-info)
	 
	 (setq test-line (line-next test-line)))))))


;;;; Parse block finders.


(defun start-of-parse-block (mark)
  (buffer-start mark))

(defun end-of-parse-block (mark)
  (buffer-end mark))

;;; 
;;; START-OF-SEARCH-LINE

(defun start-of-search-line (line)
  "Set LINE to the begining line of the block of text to parse."
  (with-mark ((mark (mark line 0)))
    (funcall (value 'Parse-Start-Function) mark)
    (setq line (mark-line mark))))

;;; 
;;; END-OF-SEACH-LINE

(defun end-of-search-line (line)
  "Set LINE to the ending line of the block of text to parse."
  (with-mark ((mark (mark line 0)))
    (funcall (value 'Parse-End-Function) mark)
    (setq line (mark-line mark))))


;;;; PARSE-LISP-LINE-INFO.

;;; PARSE-LISP-LINE-INFO -- Internal.
;;;
;;; This parses through the line doing the following things:
;;;
;;;      Counting/Setting the NET-OPEN-PARENS & NET-CLOSE-PARENS.
;;;
;;;      Making all areas of the line that should be invalid (comments,
;;;      char-quotes, and the inside of strings) and such be in
;;;      RANGES-TO-IGNORE.
;;;
;;;      Set BEGINS-QUOTED and ENDING-QUOTED 
;;;
(defun parse-lisp-line-info (mark line-info prev-line-info)
  "Parse line and set line information like NET-OPEN-PARENS, NET-CLOSE-PARENS,
   RANGES-TO-INGORE, and ENDING-QUOTED."
  (let ((net-open-parens 0)
	(net-close-parens 0))
    (declare (fixnum net-open-parens net-close-parens))
    
    ;; Re-set the slots necessary
    
    (setf (lisp-info-ranges-to-ignore line-info) nil)
    
    ;; The only way the current line begins quoted is when there
    ;; is a previous line and it's ending was quoted.
    
    (setf (lisp-info-begins-quoted line-info)
	  (and prev-line-info 
	       (lisp-info-ending-quoted prev-line-info)))
    
    (if (lisp-info-begins-quoted line-info)
      (deal-with-string-quote mark line-info)
      (setf (lisp-info-ending-quoted line-info) nil))
    
    (assert (eq (hi::mark-buffer mark) (current-buffer)))

    (unless (lisp-info-ending-quoted line-info)
      (loop 

        (unless (find-lisp-char mark)
          (error "Expected at least a newline!"))
        (case (character-attribute :lisp-syntax (next-character mark))
	  
	  (:open-paren
	   (setq net-open-parens (1+ net-open-parens))
	   (mark-after mark))
	  
	  (:close-paren
	   (if (zerop net-open-parens)
	       (setq net-close-parens (1+ net-close-parens))
	       (setq net-open-parens (1- net-open-parens)))
	   (mark-after mark))
	  
	  (:newline
	   (setf (lisp-info-ending-quoted line-info) nil)
	   (return t))
	  
	  (:comment
	   (push-range (cons (mark-charpos mark) (line-length (mark-line mark)))
		       line-info)
	   (setf (lisp-info-ending-quoted line-info) nil)
	   (return t))
	  
	  (:char-quote
	   (mark-after mark)
	   (push-range (cons (mark-charpos mark) (1+ (mark-charpos mark)))
		       line-info)
	   (mark-after mark))
	  
	  (:string-quote
	   (mark-after mark)
	   (unless (deal-with-string-quote mark line-info)
	     (setf (lisp-info-ending-quoted line-info) t)
	     (return t)))
          (t (ERROR "character attribute of: ~s is ~s, at ~s"
                    (next-character mark)
                    (character-attribute :lisp-syntax (next-character mark))
                    mark)))))

    (setf (lisp-info-net-open-parens line-info) net-open-parens)
    (setf (lisp-info-net-close-parens line-info) net-close-parens)
    (setf (lisp-info-signature-slot line-info) 
	  (line-signature (mark-line mark)))))



;;;; String quote utilities.

;;; VALID-STRING-QUOTE-P
;;;
(defmacro valid-string-quote-p (mark forwardp)
  "Return T if the string-quote indicated by MARK is valid."
  (let ((test-mark (gensym)))
    `(with-mark ((,test-mark ,mark))
       ,(unless forwardp
	  ;; TEST-MARK should always be right before the String-quote to be
	  ;; checked.
	  `(mark-before ,test-mark))
       (when (test-char (next-character ,test-mark) :lisp-syntax :string-quote)
	 (let ((slash-count 0))
	   (loop
	     (mark-before ,test-mark)
	     (if (test-char (next-character ,test-mark) :lisp-syntax :char-quote)
		 (incf slash-count)
		 (return t)))
	   (not (oddp slash-count)))))))

;;; 
;;; FIND-VALID-STRING-QUOTE

(defmacro find-valid-string-quote (mark &key forwardp (cease-at-eol nil))
  "Expand to a form that will leave MARK before a valid string-quote character,
  in either a forward or backward direction, according to FORWARDP.  If 
  CEASE-AT-EOL is T then it will return nil if encountering the EOL before a
  valid string-quote."
  (let ((e-mark (gensym)))
    `(with-mark ((,e-mark ,mark))
       
       (loop
	(unless (scan-direction ,e-mark ,forwardp :lisp-syntax 
				,(if cease-at-eol 
				     `(or :newline :string-quote)
				     `:string-quote))
	  (return nil))
	
	,@(if cease-at-eol
	      `((when (test-char (direction-char ,e-mark ,forwardp) :lisp-syntax
				 :newline)
		  (return nil))))
	
	(when (valid-string-quote-p ,e-mark ,forwardp)
	  (move-mark ,mark ,e-mark)
	  (return t))
	
	(neighbor-mark ,e-mark ,forwardp)))))

;;;; DEAL-WITH-STRING-QUOTE.

;;; DEAL-WITH-STRING-QUOTE
;;;
;;; Called when a string is begun (i.e. parse hits a #\").  It checks for a
;;; matching quote on the line that MARK points to, and puts the appropriate
;;; area in the RANGES-TO-IGNORE slot and leaves MARK pointing after this area.
;;; The "appropriate area" is from MARK to the end of the line or the matching
;;; string-quote, whichever comes first.
;;;
(defun deal-with-string-quote (mark info-struct)
  "Alter the current line's info struct as necessary as due to encountering a
   string quote character."
  (with-mark ((e-mark mark))
    (cond ((find-valid-string-quote e-mark :forwardp t :cease-at-eol t)
	   ;; If matching quote is on this line then mark the area between the
	   ;; first quote (MARK) and the matching quote as invalid by pushing
	   ;; its begining and ending into the IGNORE-RANGE.
	   (push-range (cons (mark-charpos mark) (mark-charpos e-mark))
		       info-struct)
	   (setf (lisp-info-ending-quoted info-struct) nil)
	   (mark-after e-mark)
	   (move-mark mark e-mark))
	  ;; If the EOL has been hit before the matching quote then mark the
	  ;; area from MARK to the EOL as invalid.
	  (t
	   (push-range (cons (mark-charpos mark)
			     (line-length (mark-line mark)))
		       info-struct)
	   ;; The Ending is marked as still being quoted. 
	   (setf (lisp-info-ending-quoted info-struct) t)
	   (line-end mark)
	   nil))))



;;;; Character validity checking:

;;; Find-Ignore-Region  --  Internal
;;;
;;;    If the character in the specified direction from Mark is in an ignore
;;; region, then return the region and the line that the region is in as
;;; values.  If there is no ignore region, then return NIL and the Mark-Line.
;;; If the line is not parsed, or there is no character (because of being at
;;; the buffer beginning or end), then return both values NIL.
;;;
(defun find-ignore-region (mark forwardp)
  (flet ((scan (line pos)
	   (declare (fixnum pos))
	   (let ((info (getf (line-plist line) 'lisp-info)))
	     (if info
		 (dolist (range (lisp-info-ranges-to-ignore info)
				(values nil line))
		   (let ((start (car range))
			 (end (cdr range)))
		     (declare (fixnum start end))
		     (when (and (>= pos start) (< pos end))
		       (return (values range line)))))
		 (values nil nil)))))
    (let ((pos (mark-charpos mark))
	  (line (mark-line mark)))
      (declare (fixnum pos))
      (cond (forwardp (scan line pos))
	    ((> pos 0) (scan line (1- pos)))
	    (t
	     (let ((prev (line-previous line)))
	       (if prev
		   (scan prev (line-length prev))
		   (values nil nil))))))))


;;; Valid-Spot  --  Public
;;;
(defun valid-spot (mark forwardp)
  "Return true if the character pointed to by Mark is not in a quoted context,
  false otherwise.  If Forwardp is true, we use the next character, otherwise
  we use the previous."
  (if (and (not forwardp)
	   (null (previous-character mark)))
    t			      ;beginning of buffer always a valid spot
    (multiple-value-bind (region line)
	(find-ignore-region mark forwardp)
      (and line (not region)))))


;;; Scan-Direction-Valid  --  Internal
;;;
;;;    Like scan-direction, but only stop on valid characters.
;;;
(defmacro scan-direction-valid (mark forwardp &rest forms)
  (let ((n-mark (gensym))
	(n-line (gensym))
	(n-region (gensym))
	(n-won (gensym)))
    `(let ((,n-mark ,mark) (,n-won nil))
       (loop
	 (multiple-value-bind (,n-region ,n-line)
			      (find-ignore-region ,n-mark ,forwardp)
	   (unless ,n-line (return nil))
	   (if ,n-region
	       (move-to-position ,n-mark
				 ,(if forwardp
				      `(cdr ,n-region) 
				      `(car ,n-region))
				 ,n-line)
	       (when ,n-won (return t)))
	   ;;
	   ;; Peculiar condition when a quoting character terminates a line.
	   ;; The ignore region is off the end of the line causing %FORM-OFFSET
	   ;; to infinitely loop.
	   (when (> (mark-charpos ,n-mark) (line-length ,n-line))
	     (line-offset ,n-mark 1 0))
	   (unless (scan-direction ,n-mark ,forwardp ,@forms)
	     (return nil))
	   (setq ,n-won t))))))


;;;; List offseting.

;;; %LIST-OFFSET allows for BACKWARD-LIST and FORWARD-LIST to be built
;;; with the same existing structure, with the altering of one variable.
;;; This one variable being FORWARDP.
;;; 
(defmacro %list-offset (actual-mark forwardp &key (extra-parens 0) )
  "Expand to code that will go forward one list either backward or forward, 
   according to the FORWARDP flag."
  (let ((mark (gensym)))
    `(let ((paren-count ,extra-parens))
       (declare (fixnum paren-count))
       (with-mark ((,mark ,actual-mark))
	 (loop
	   (scan-direction ,mark ,forwardp :lisp-syntax
			   (or :close-paren :open-paren :newline))
	   (let ((ch (direction-char ,mark ,forwardp)))
	     (unless ch (return nil))
	     (when (valid-spot ,mark ,forwardp)
	       (case (character-attribute :lisp-syntax ch)
		 (:close-paren
		  (decf paren-count)
		  ,(when forwardp
		     ;; When going forward, an unmatching close-paren means the
		     ;; end of list.
		     `(when (<= paren-count 0)
			(neighbor-mark ,mark ,forwardp)
			(move-mark ,actual-mark ,mark)
			(return t))))
		 (:open-paren
		  (incf paren-count)
		  ,(unless forwardp             ; Same as above only end of list
		     `(when (>= paren-count 0)  ; is opening parens.
			(neighbor-mark ,mark ,forwardp)
			(move-mark ,actual-mark ,mark)
			(return t))))
		 
		 (:newline 
		  ;; When a #\Newline is hit, then the matching paren must lie
		  ;; on some other line so drop down into the multiple line
		  ;; balancing function: QUEST-FOR-BALANCING-PAREN If no paren
		  ;; seen yet, keep going.
		  (cond ((zerop paren-count))
			((quest-for-balancing-paren ,mark paren-count ,forwardp)
			 (move-mark ,actual-mark ,mark)
			 (return t))
			(t
			 (return nil)))))))
	   
	   (neighbor-mark ,mark ,forwardp))))))

;;; 
;;; QUEST-FOR-BALANCING-PAREN

(defmacro quest-for-balancing-paren (mark paren-count forwardp)
  "Expand to a form that finds the the balancing paren for however many opens or
  closes are registered by Paren-Count."
  `(let* ((line (mark-line ,mark)))
     (loop
       (setq line (neighbor-line line ,forwardp))
       (unless line (return nil))
       (let ((line-info (getf (line-plist line) 'lisp-info))
	     (unbal-paren ,paren-count))
	 (unless line-info (return nil))
	 
	 ,(if forwardp
	      `(decf ,paren-count (lisp-info-net-close-parens line-info))
	      `(incf ,paren-count (lisp-info-net-open-parens line-info)))
	 
	 (when ,(if forwardp
		    `(<= ,paren-count 0)
		    `(>= ,paren-count 0))
	   ,(if forwardp
		`(line-start ,mark line)
		`(line-end ,mark line))
	   (return (goto-correct-paren-char ,mark unbal-paren ,forwardp)))

	 ,(if forwardp
	      `(incf ,paren-count (lisp-info-net-open-parens line-info))
	      `(decf ,paren-count (lisp-info-net-close-parens line-info)))))))
		   

;;; 
;;; GOTO-CORRECT-PAREN-CHAR

(defmacro goto-correct-paren-char (mark paren-count forwardp)
  "Expand to a form that will leave MARK on the correct balancing paren matching 
   however many are indicated by COUNT." 
  `(with-mark ((m ,mark))
     (let ((count ,paren-count))
       (loop
	 (scan-direction m ,forwardp :lisp-syntax 
			 (or :close-paren :open-paren :newline))
	 (when (valid-spot m ,forwardp)
	   (ecase (character-attribute :lisp-syntax (direction-char m ,forwardp))
	     (:close-paren 
	      (decf count)
	      ,(when forwardp
		 `(when (zerop count)
		    (neighbor-mark m ,forwardp)
		    (move-mark ,mark m)
		    (return t))))
	     
	     (:open-paren 
	      (incf count)
	      ,(unless forwardp
		 `(when (zerop count)
		    (neighbor-mark m ,forwardp)
		    (move-mark ,mark m)
		    (return t))))))
	 (neighbor-mark m ,forwardp)))))


(defun list-offset (mark offset)
  (if (plusp offset)
      (dotimes (i offset t)
	(unless (%list-offset mark t) (return nil)))
      (dotimes (i (- offset) t)
	(unless (%list-offset mark nil) (return nil)))))

(defun forward-up-list (mark)
  "Moves mark just past the closing paren of the immediately containing list."
  (%list-offset mark t :extra-parens 1))

(defun backward-up-list (mark)
  "Moves mark just before the opening paren of the immediately containing list."
  (%list-offset mark nil :extra-parens -1))



;;;; Top level form location hacks (open parens beginning lines).

;;; NEIGHBOR-TOP-LEVEL is used only in TOP-LEVEL-OFFSET.
;;; 
(eval-when (:compile-toplevel :execute)
(defmacro neighbor-top-level (line forwardp)
  `(loop
     (when (test-char (line-character ,line 0) :lisp-syntax :open-paren)
       (return t))
     (setf ,line ,(if forwardp `(line-next ,line) `(line-previous ,line)))
     (unless ,line (return nil))))
) ;eval-when

(defun top-level-offset (mark offset)
  "Go forward or backward offset number of top level forms.  Mark is
   returned if offset forms exists, otherwise nil."
  (declare (fixnum offset))
  (let* ((line (mark-line mark))
	 (at-start (test-char (line-character line 0) :lisp-syntax :open-paren)))
    (cond ((zerop offset) mark)
	  ((plusp offset)
	   (do ((offset (if at-start offset (1- offset))
			(1- offset)))
	       (nil)
	     (declare (fixnum offset))
	     (unless (neighbor-top-level line t) (return nil))
	     (when (zerop offset) (return (line-start mark line)))
	     (unless (setf line (line-next line)) (return nil))))
	  (t
	   (do ((offset (if (and at-start (start-line-p mark))
			    offset
			    (1+ offset))
			(1+ offset)))
		(nil)
	     (declare (fixnum offset))
	     (unless (neighbor-top-level line nil) (return nil))
	     (when (zerop offset) (return (line-start mark line)))
	     (unless (setf line (line-previous line)) (return nil)))))))


(defun mark-top-level-form (mark1 mark2)
  "Moves mark1 and mark2 to the beginning and end of the current or next defun.
   Mark1 one is used as a reference.  The marks may be altered even if
   unsuccessful.  if successful, return mark2, else nil."
  (let ((winp (cond ((inside-defun-p mark1)
		     (cond ((not (top-level-offset mark1 -1)) nil)
			   ((not (form-offset (move-mark mark2 mark1) 1)) nil)
			   (t mark2)))
		    ((start-defun-p mark1)
		     (form-offset (move-mark mark2 mark1) 1))
		    ((and (top-level-offset (move-mark mark2 mark1) -1)
			  (start-defun-p mark2)
			  (form-offset mark2 1)
			  (same-line-p mark1 mark2))
		     (form-offset (move-mark mark1 mark2) -1)
		     mark2)
		    ((top-level-offset mark1 1)
		     (form-offset (move-mark mark2 mark1) 1)))))
    (when winp
      (when (blank-after-p mark2) (line-offset mark2 1 0))
      mark2)))

(defun inside-defun-p (mark)
  "T if the current point is (supposedly) in a top level form."
  (with-mark ((m mark))
    (when (top-level-offset m -1)
      (form-offset m 1)
      (mark> m mark))))

(defun start-defun-p (mark)
  "Returns t if mark is sitting before an :open-paren at the beginning of a
   line."
  (and (start-line-p mark)
       (test-char (next-character mark) :lisp-syntax :open-paren)))



;;;; Form offseting.

(defmacro %form-offset (mark forwardp)
  `(with-mark ((m ,mark))
     (when (scan-direction-valid m ,forwardp :lisp-syntax
				 (or :open-paren :close-paren
				     :char-quote :string-quote
				     :constituent))
       (ecase (character-attribute :lisp-syntax (direction-char m ,forwardp))
	 (:open-paren
	  (when ,(if forwardp `(list-offset m 1) `(mark-before m))
	    ,(unless forwardp
	       '(scan-direction m nil :lisp-syntax (not :prefix)))
	    (move-mark ,mark m)
	    t))
	 (:close-paren
	  (when ,(if forwardp `(mark-after m) `(list-offset m -1))
	    ,(unless forwardp
	       '(scan-direction m nil :lisp-syntax (not :prefix)))
	    (move-mark ,mark m)
	    t))
	 ((:constituent :char-quote)
	  (scan-direction-valid m ,forwardp :lisp-syntax
				(not (or :constituent :char-quote)))
	  ,(if forwardp
	       `(scan-direction-valid m t :lisp-syntax
				      (not (or :constituent :char-quote)))
	       `(scan-direction-valid m nil :lisp-syntax
				      (not (or :constituent :char-quote
					       :prefix))))
	  (move-mark ,mark m)
	  t)
	 (:string-quote
	  (cond ((valid-spot m ,(not forwardp))
		 (neighbor-mark m ,forwardp)
		 (when (scan-direction-valid m ,forwardp :lisp-syntax
					     :string-quote)
		   (neighbor-mark m ,forwardp)
		   (move-mark ,mark m)
		   t))
		(t (neighbor-mark m ,forwardp)
		   (move-mark ,mark m)
		   t)))))))


(defun form-offset (mark offset)
  "Move mark offset number of forms, after if positive, before if negative.
   Mark is always moved.  If there weren't enough forms, returns nil instead of
   mark."
  (if (plusp offset)
      (dotimes (i offset t)
	(unless (%form-offset mark t) (return nil)))
      (dotimes (i (- offset) t)
	(unless (%form-offset mark nil) (return nil)))))



;;;; Table of special forms with special indenting requirements.

(defhvar "Indent Defanything"
  "This is the number of special arguments implicitly assumed to be supplied
   in calls to functions whose names begin with \"DEF\".  If set to NIL, this
   feature is disabled."
  :value 2)

(defhvar "Indent With-anything"
  "This is the number of special arguments implicitly assumed to be supplied
   in calls to functions whose names begin with \"WITH-\". If set to NIL, this
   feature is disabled."
  :value 1)

(defvar *special-forms* (make-hash-table :test #'equal))

(defun defindent (fname args)
  "Define Fname to have Args special arguments.  If args is null then remove
   any special arguments information."
  (check-type fname string)
  (let ((fname (string-upcase fname)))
    (cond ((null args) (remhash fname *special-forms*))
	  (t
	   (check-type args integer)
	   (setf (gethash fname *special-forms*) args)))))


;;; Hemlock forms.
;;; 
(defindent "defhvar" 1)
(defindent "hlet" 1)
(defindent "defcommand" 2)
(defindent "defattribute" 1)
(defindent "command-case" 1)
(defindent "do-strings" 1)
(defindent "save-for-undo" 1)
(defindent "do-alpha-chars" 1)
(defindent "do-headers-buffers" 1)
(defindent "do-headers-lines" 1)
(defindent "frob" 1) ;cover silly FLET and MACROLET names for Rob and Bill.
(defindent "modifying-buffer" 1)

;;; Common Lisp forms.
;;; 
(defindent "block" 1)
(defindent "case" 1)
(defindent "catch" 1)
(defindent "ccase" 1)			   
(defindent "compiler-let" 1)
(defindent "ctypecase" 1)
(defindent "defconstant" 1)
(defindent "define-compiler-macro" 2)
(defindent "define-setf-method" 2)
(defindent "destructuring-bind" 2)
(defindent "defmacro" 2)
(defindent "defpackage" 1)
(defindent "defparameter" 1)
(defindent "defstruct" 1)
(defindent "deftype" 2)
(defindent "defun" 2)
(defindent "defvar" 1)
(defindent "do" 2)
(defindent "do*" 2)
(defindent "do-all-symbols" 1)
(defindent "do-external-symbols" 1)
(defindent "do-symbols" 1)
(defindent "dolist" 1)
(defindent "dotimes" 1)
(defindent "ecase" 1)
(defindent "etypecase" 1)
(defindent "eval-when" 1)
(defindent "flet" 1)
(defindent "if" 1)
(defindent "labels" 1)
(defindent "lambda" 1)
(defindent "let" 1)
(defindent "let*" 1)
(defindent "locally" 0)
(defindent "loop" 0)
(defindent "macrolet" 1)
(defindent "multiple-value-bind" 2)
(defindent "multiple-value-call" 1)
(defindent "multiple-value-prog1" 1)
(defindent "multiple-value-setq" 1)
(defindent "prog1" 1)
(defindent "progv" 2)
(defindent "progn" 0)
(defindent "typecase" 1)
(defindent "unless" 1)
(defindent "unwind-protect" 1)
(defindent "when" 1)

;;; Error/condition system forms.
;;; 
(defindent "define-condition" 2)
(defindent "handler-bind" 1)
(defindent "handler-case" 1)
(defindent "restart-bind" 1)
(defindent "restart-case" 1)
;;; These are for RESTART-CASE branch formatting.
(defindent "store-value" 1)
(defindent "use-value" 1)
(defindent "muffle-warning" 1)
(defindent "abort" 1)
(defindent "continue" 1)

;;; Debug-internals forms.
;;;
(defindent "do-debug-function-blocks" 1)
(defindent "di:do-debug-function-blocks" 1)
(defindent "do-debug-function-variables" 1)
(defindent "di:do-debug-function-variables" 1)
(defindent "do-debug-block-locations" 1)
(defindent "di:do-debug-block-locations" 1)
;;;
;;; Debug-internals conditions
;;; (define these to make uses of HANDLER-CASE indent branches correctly.)
;;;
(defindent "debug-condition" 1)
(defindent "di:debug-condition" 1)
(defindent "no-debug-info" 1)
(defindent "di:no-debug-info" 1)
(defindent "no-debug-function-returns" 1)
(defindent "di:no-debug-function-returns" 1)
(defindent "no-debug-blocks" 1)
(defindent "di:no-debug-blocks" 1)
(defindent "lambda-list-unavailable" 1)
(defindent "di:lambda-list-unavailable" 1)
(defindent "no-debug-variables" 1)
(defindent "di:no-debug-variables" 1)
(defindent "invalid-value" 1)
(defindent "di:invalid-value" 1)
(defindent "ambiguous-variable-name" 1)
(defindent "di:ambiguous-variable-name" 1)
(defindent "debug-error" 1)
(defindent "di:debug-error" 1)
(defindent "unhandled-condition" 1)
(defindent "di:unhandled-condition" 1)
(defindent "unknown-code-location" 1)
(defindent "di:unknown-code-location" 1)
(defindent "unknown-debug-variable" 1)
(defindent "di:unknown-debug-variable" 1)
(defindent "invalid-control-stack-pointer" 1)
(defindent "di:invalid-control-stack-pointer" 1)
(defindent "frame-function-mismatch" 1)
(defindent "di:frame-function-mismatch" 1)


;;; CLOS forms.
;;; 
(defindent "with-accessors" 2)
(defindent "defclass" 2)
(defindent "print-unreadable-object" 1)
(defindent "defmethod" 2)
(defindent "make-instance" 1)

;;; System forms.
;;;
(defindent "rlet" 1)

;;; Multiprocessing forms.
(defindent "process-wait" 1)



;;;; Indentation.

;;; LISP-INDENTATION -- Internal Interface.

(defun strip-package-prefix (string)
  (let* ((p (position #\: string :from-end t)))
    (if p
      (subseq string (1+ p))
      string)))
;;;
(defun lisp-indentation (mark)
  "Compute number of spaces which mark should be indented according to
   local context and lisp grinding conventions.  This assumes mark is at the
   beginning of the line to be indented."
  (with-mark ((m mark)
	      (temp mark))
    ;; See if we are in a quoted context.
    (unless (valid-spot m nil)
      (return-from lisp-indentation (lisp-generic-indentation m)))
    ;; Look for the paren that opens the containing form.
    (unless (backward-up-list m)
      (return-from lisp-indentation 0))
    ;; Move after the paren, save the start, and find the form name.
    (mark-after m)
    (with-mark ((start m))
      (unless (and (scan-char m :lisp-syntax
			      (not (or :space :prefix :char-quote)))
		   (test-char (next-character m) :lisp-syntax :constituent))
	(return-from lisp-indentation (mark-column start)))
      (with-mark ((fstart m))
	(scan-char m :lisp-syntax (not :constituent))
	(let* ((fname (nstring-upcase
                       (strip-package-prefix (region-to-string (region fstart m)))))
	       (special-args (or (gethash fname *special-forms*)
				 (and (> (length fname) 2)
				      (string= fname "DEF" :end1 3)
				      (value indent-defanything))
                                 (and (> (length fname) 4)
                                      (string= fname "WITH-" :end1 5)
                                      (value indent-with-anything)))))
	  (declare (simple-string fname))
	  ;; Now that we have the form name, did it have special syntax?
	  (cond (special-args
		 (with-mark ((spec m))
		   (cond ((and (form-offset spec special-args)
			       (mark<= spec mark))
			  (1+ (mark-column start)))
			 ((skip-valid-space m)
			  (mark-column m))
			 (t
			  (+ (mark-column start) 3)))))
		;; See if the user seems to have altered the editor's
		;; indentation, and if so, try to adhere to it.  This usually
		;; happens when you type in a quoted list constant that line
		;; wraps.  You want all the items on successive lines to fall
		;; under the first character after the opening paren, not as if
		;; you are calling a function.
		((and (form-offset temp -1)
		      (or (blank-before-p temp) (not (same-line-p temp fstart)))
		      (not (same-line-p temp mark)))
		 (unless (blank-before-p temp)
		   (line-start temp)
		   (find-attribute temp :space #'zerop))
		 (mark-column temp))
		;; Appears to be a normal form.  Is the first arg on the same
		;; line as the form name?
		((skip-valid-space m)
		 (or (lisp-indentation-check-for-local-def
		      mark temp fstart start t)
		     (mark-column m)))
		;; Okay, fall under the first character after the opening paren.
		(t
		 (or (lisp-indentation-check-for-local-def
		      mark temp fstart start nil)
		     (mark-column start)))))))))

(defhvar "Lisp Indentation Local Definers"
  "Forms with syntax like LABELS, MACROLET, etc."
  :value '("LABELS" "MACROLET" "FLET"))

;;; LISP-INDENTATION-CHECK-FOR-LOCAL-DEF -- Internal.
;;;
;;; This is a temporary hack to see how it performs.  When we are indenting
;;; what appears to be a function call, let's look for FLET or MACROLET to see
;;; if we really are indenting a local definition.  If we are, return the
;;; indentation for a DEFUN; otherwise, nil
;;;
;;; Mark is the argument to LISP-INDENTATION.  Start is just inside the paren
;;; of what looks like a function call.  If we are in an FLET, arg-list
;;; indicates whether the local function's arg-list has been entered, that is,
;;; whether we need to normally indent for a DEFUN body or indent specially for
;;; the arg-list.
;;;
(defun lisp-indentation-check-for-local-def (mark temp1 temp2 start arg-list)
  ;; We know this succeeds from LISP-INDENTATION.
  (backward-up-list (move-mark temp1 mark)) ;Paren for local definition.
  (cond ((and (backward-up-list temp1)	    ;Paren opening the list of defs
	      (form-offset (move-mark temp2 temp1) -1)
	      (mark-before temp2)
	      (backward-up-list temp1)	    ;Paren for FLET or MACROLET.
	      (mark= temp1 temp2))	    ;Must be in first arg form.
	 ;; See if the containing form is named FLET or MACROLET.
	 (mark-after temp1)
	 (unless (and (scan-char temp1 :lisp-syntax
				 (not (or :space :prefix :char-quote)))
		      (test-char (next-character temp1) :lisp-syntax
				 :constituent))
	   (return-from lisp-indentation-check-for-local-def nil))
	 (move-mark temp2 temp1)
	 (scan-char temp2 :lisp-syntax (not :constituent))
	 (let ((fname (nstring-upcase (region-to-string (region temp1 temp2)))))
	   (cond ((not (member fname (value lisp-indentation-local-definers)
			       :test #'string=))
		  nil)
		 (arg-list
		  (1+ (mark-column start)))
		 (t
		  (+ (mark-column start) 3)))))))

;;; LISP-GENERIC-INDENTATION -- Internal.
;;;
;;; LISP-INDENTATION calls this when mark is in a invalid spot, or quoted
;;; context.  If we are inside a string, we return the column one greater
;;; than the opening double quote.  Otherwise, we just use the indentation
;;; of the first preceding non-blank line.
;;;
(defun lisp-generic-indentation (mark)
  (with-mark ((m mark))
    (form-offset m -1)
    (cond ((eq (character-attribute :lisp-syntax (next-character m))
	       :string-quote)
	   (1+ (mark-column m)))
	  (t
	   (let* ((line (mark-line mark))
		  (prev (do ((line (line-previous line) (line-previous line)))
			    ((not (and line (blank-line-p line))) line))))
	     (cond (prev
		    (line-start mark prev)
		    (find-attribute mark :space #'zerop)
		    (mark-column mark))
		   (t 0)))))))

;;; Skip-Valid-Space  --  Internal
;;;
;;;    Skip over any space on the line Mark is on, stopping at the first valid
;;; non-space character.  If there is none on the line, return nil.
;;;
(defun skip-valid-space (mark)
  (loop
    (scan-char mark :lisp-syntax (not :space))
    (let ((val (character-attribute :lisp-syntax
				    (next-character mark))))
      (cond ((eq val :newline) (return nil))
	    ((valid-spot mark t) (return mark))))
    (mark-after mark)))

;; (declaim (optimize (speed 0))); byte compile again


;;;; Indentation commands and hook functions.

(defcommand "Defindent" (p)
  "Define the Lisp indentation for the current function.
  The indentation is a non-negative integer which is the number
  of special arguments for the form.  Examples: 2 for Do, 1 for Dolist.
  If a prefix argument is supplied, then delete the indentation information."
  "Do a defindent, man!"
  (with-mark ((m (current-point)))
    (pre-command-parse-check m)
    (unless (backward-up-list m) (editor-error))
    (mark-after m)
    (with-mark ((n m))
      (scan-char n :lisp-syntax (not :constituent))
      (let ((s (region-to-string (region m n))))
	(declare (simple-string s))
	(when (zerop (length s)) (editor-error))
	(if p
	    (defindent s nil)
	    (let ((i (prompt-for-integer
		      :prompt (format nil "Indentation for ~A: " s)
		      :help "Number of special arguments.")))
	      (when (minusp i)
		(editor-error "Indentation must be non-negative."))
	      (defindent s i))))))
  (indent-command nil))

(defcommand "Indent Form" (p)
  "Indent Lisp code in the next form."
  "Indent Lisp code in the next form."
  (declare (ignore p))
  (let ((point (current-point)))
    (pre-command-parse-check point)
    (with-mark ((m point))
      (unless (form-offset m 1) (editor-error))
      (lisp-indent-region (region point m) "Indent Form"))))

;;; LISP-INDENT-REGION -- Internal.
;;;
;;; This indents a region of Lisp code without doing excessive redundant
;;; computation.  We parse the entire region once, then scan through doing
;;; indentation on each line.  We forcibly reparse each line that we indent so
;;; that the list operations done to determine indentation of subsequent lines
;;; will work.  This is done undoably with save1, save2, buf-region, and
;;; undo-region.
;;;
(defun lisp-indent-region (region &optional (undo-text "Lisp region indenting"))  (let* ((start (region-start region))
         (end (region-end region))
         (buffer (hi::line-%buffer (mark-line start))))
    (with-mark ((m1 start)
		(m2 end))
      (funcall (value parse-start-function) m1)
      (funcall (value parse-end-function) m2)
      (parse-over-block (mark-line m1) (mark-line m2)))
    (hi::check-buffer-modification buffer start)
    (hi::check-buffer-modification buffer end)
    (let* ((first-line (mark-line start))
              (last-line (mark-line end))
              (prev (line-previous first-line))
              (prev-line-info
               (and prev (getf (line-plist prev) 'lisp-info)))
              (save1 (line-start (copy-mark start :right-inserting)))
              (save2 (line-end (copy-mark end :left-inserting)))
              (buf-region (region save1 save2))
              (undo-region (copy-region buf-region)))
         (with-mark ((bol start :left-inserting))
           (do ((line first-line (line-next line)))
               (nil)
             (line-start bol line)
             (ensure-lisp-indentation bol)
             (let ((line-info (getf (line-plist line) 'lisp-info)))
               (parse-lisp-line-info bol line-info prev-line-info)
               (setq prev-line-info line-info))
             (when (eq line last-line) (return nil))))
         (make-region-undo :twiddle undo-text buf-region undo-region))))

;;; INDENT-FOR-LISP -- Internal.
;;;
;;; This is the value of "Indent Function" for "Lisp" mode.
;;;
(defun indent-for-lisp (mark)
  (line-start mark)
  (pre-command-parse-check mark)
  (ensure-lisp-indentation mark))

(defun count-leading-whitespace (mark)
  (with-mark ((m mark))
    (line-start m)
    (do* ((p 0)
	  (q 0 (1+ q))
          (tab-width (value spaces-per-tab)))
         ()
      (case (next-character m)
        (#\space (incf p))
        (#\tab (setq p (* tab-width (ceiling (1+ p) tab-width))))
        (t (return (values p q))))
      (character-offset m 1))))

;;; Don't do anything if M's line is already correctly indented.
(defun ensure-lisp-indentation (m)
  (let* ((col (lisp-indentation m)))
    (multiple-value-bind (curcol curpos) (count-leading-whitespace m)
      (cond ((= curcol col) (setf (mark-charpos m) curpos))
	    (t
	     (delete-horizontal-space m)
	     (indent-to-column m col))))))




;;;; Most "Lisp" mode commands.

(defcommand "Beginning of Defun" (p)
  "Move the point to the beginning of a top-level form, collapsing the selection.
  with an argument, skips the previous p top-level forms."
  "Move the point to the beginning of a top-level form, collapsing the selection."
  (let ((point (current-point-collapsing-selection))
	(count (or p 1)))
    (pre-command-parse-check point)
    (if (minusp count)
	(end-of-defun-command (- count))
	(unless (top-level-offset point (- count))
	  (editor-error)))))

(defcommand "Select to Beginning of Defun" (p)
  "Move the point to the beginning of a top-level form, extending the selection.
  with an argument, skips the previous p top-level forms."
  "Move the point to the beginning of a top-level form, extending the selection."
  (let ((point (current-point-extending-selection))
	(count (or p 1)))
    (pre-command-parse-check point)
    (if (minusp count)
	(end-of-defun-command (- count))
	(unless (top-level-offset point (- count))
	  (editor-error)))))

;;; "End of Defun", with a positive p (the normal case), does something weird.
;;; Get a mark at the beginning of the defun, and then offset it forward one
;;; less top level form than we want.  This sets us up to use FORM-OFFSET which
;;; allows us to leave the point immediately after the defun.  If we used
;;; TOP-LEVEL-OFFSET one less than p on the mark at the end of the current
;;; defun, point would be left at the beginning of the p+1'st form instead of
;;; at the end of the p'th form.
;;;
(defcommand "End of Defun" (p)
  "Move the point to the end of a top-level form, collapsing the selection.
   With an argument, skips the next p top-level forms."
  "Move the point to the end of a top-level form, collapsing the selection."
  (let ((point (current-point-collapsing-selection))
	(count (or p 1)))
    (pre-command-parse-check point)
    (if (minusp count)
	(beginning-of-defun-command (- count))
	(with-mark ((m point)
		    (dummy point))
	  (cond ((not (mark-top-level-form m dummy))
		 (editor-error "No current or next top level form."))
		(t 
		 (unless (top-level-offset m (1- count))
		   (editor-error "Not enough top level forms."))
		 ;; We might be one unparsed for away.
		 (pre-command-parse-check m)
		 (unless (form-offset m 1)
		   (editor-error "Not enough top level forms."))
		 (when (blank-after-p m) (line-offset m 1 0))
		 (move-mark point m)))))))

(defcommand "Select to End of Defun" (p)
  "Move the point to the end of a top-level form, extending the selection.
   With an argument, skips the next p top-level forms."
  "Move the point to the end of a top-level form, extending the selection."
  (let ((point (current-point-extending-selection))
	(count (or p 1)))
    (pre-command-parse-check point)
    (if (minusp count)
	(beginning-of-defun-command (- count))
	(with-mark ((m point)
		    (dummy point))
	  (cond ((not (mark-top-level-form m dummy))
		 (editor-error "No current or next top level form."))
		(t 
		 (unless (top-level-offset m (1- count))
		   (editor-error "Not enough top level forms."))
		 ;; We might be one unparsed for away.
		 (pre-command-parse-check m)
		 (unless (form-offset m 1)
		   (editor-error "Not enough top level forms."))
		 (when (blank-after-p m) (line-offset m 1 0))
		 (move-mark point m)))))))

(defcommand "Forward List" (p)
  "Skip over the next Lisp list, collapsing the selection.
  With argument, skips the next p lists."
  "Skip over the next Lisp list, collapsing the selection."
  (or (collapse-if-selection :direction :forward)
      (let ((point (current-point-collapsing-selection))
            (count (or p 1)))
        (pre-command-parse-check point)
        (unless (list-offset point count) (editor-error)))))

(defcommand "Select Forward List" (p)
  "Skip over the next Lisp list, extending the selection.
  With argument, skips the next p lists."
  "Skip over the next Lisp list, extending the selection."
  (let ((point (current-point-extending-selection))
	(count (or p 1)))
    (pre-command-parse-check point)
    (unless (list-offset point count) (editor-error))))

(defcommand "Backward List" (p)
  "Skip over the previous Lisp list, collapsing the selection.
  With argument, skips the previous p lists."
  "Skip over the previous Lisp list, collapsing the selection."
  (or (collapse-if-selection :direction :backward)
   (let ((point (current-point-collapsing-selection))
	(count (- (or p 1))))
    (pre-command-parse-check point)
    (unless (list-offset point count) (editor-error)))))

(defcommand "Select Backward List" (p)
  "Skip over the previous Lisp list, extending the selection.
  With argument, skips the previous p lists."
  "Skip over the previous Lisp list, extending the selection."
  (let ((point (current-point-extending-selection))
	(count (- (or p 1))))
    (pre-command-parse-check point)
    (unless (list-offset point count) (editor-error))))

(defcommand "Forward Form" (p)
    "Skip over the next Form, collapsing the selection.
  With argument, skips the next p Forms."
    "Skip over the next Form, collapsing the selection."
  (or (collapse-if-selection :direction :forward)
      (let ((point (current-point-collapsing-selection))
            (count (or p 1)))
        (pre-command-parse-check point)
        (unless (form-offset point count) (editor-error)))))

(defcommand "Select Forward Form" (p)
  "Skip over the next Form, extending the selection.
  With argument, skips the next p Forms."
  "Skip over the next Form, extending the selection."
  (let ((point (current-point-extending-selection))
	(count (or p 1)))
    (pre-command-parse-check point)
    (unless (form-offset point count) (editor-error))))

(defcommand "Backward Form" (p)
    "Skip over the previous Form, collapsing the selection.
  With argument, skips the previous p Forms."
    "Skip over the previous Form, collaspsing the selection."
  (or (collapse-if-selection :direction :backward)
      (let ((point (current-point-collapsing-selection))
            (count (- (or p 1))))
        (pre-command-parse-check point)
        (unless (form-offset point count) (editor-error)))))

(defcommand "Select Backward Form" (p)
  "Skip over the previous Form, extending the selection.
  With argument, skips the previous p Forms."
  "Skip over the previous Form, extending the selection."
  (let ((point (current-point-extending-selection))
	(count (- (or p 1))))
    (pre-command-parse-check point)
    (unless (form-offset point count) (editor-error))))

(defcommand "Mark Form" (p)
  "Set the mark at the end of the next Form.
   With a positive argument, set the mark after the following p
   Forms. With a negative argument, set the mark before
   the preceding -p Forms."
  "Set the mark at the end of the next Form."
  (with-mark ((m (current-point)))
    (pre-command-parse-check m)
    (let ((count (or p 1))
	  (mark (push-new-buffer-mark m t)))
      (if (form-offset m count)
	  (move-mark mark m)
	  (editor-error)))))

(defcommand "Mark Defun" (p)
  "Puts the region around the next or containing top-level form.
   The point is left before the form and the mark is placed immediately
   after it."
  "Puts the region around the next or containing top-level form."
  (declare (ignore p))
  (let ((point (current-point)))
    (pre-command-parse-check point)
    (with-mark ((start point)
		(end point))
      (cond ((not (mark-top-level-form start end))
	     (editor-error "No current or next top level form."))
	    (t
	     (move-mark point start)
	     (move-mark (push-new-buffer-mark point t) end))))))

(defcommand "Forward Kill Form" (p)
  "Kill the next Form.
   With a positive argument, kills the next p Forms.
   Kills backward with a negative argument."
  "Kill the next Form."
  (with-mark ((m1 (current-point))
	      (m2 (current-point)))
    (pre-command-parse-check m1)
    (let ((count (or p 1)))
      (unless (form-offset m1 count) (editor-error))
      (if (minusp count)
	  (kill-region (region m1 m2) :kill-backward)
	  (kill-region (region m2 m1) :kill-forward)))))

(defcommand "Backward Kill Form" (p)
  "Kill the previous Form.
  With a positive argument, kills the previous p Forms.
  Kills forward with a negative argument."
  "Kill the previous Form."
  (forward-kill-form-command (- (or p 1))))

(defcommand "Extract Form" (p)
  "Replace the current containing list with the next form.  The entire affected
   area is pushed onto the kill ring.  If an argument is supplied, that many
   upward levels of list nesting is replaced by the next form."
  "Replace the current containing list with the next form.  The entire affected
   area is pushed onto the kill ring.  If an argument is supplied, that many
   upward levels of list nesting is replaced by the next form."
  (let ((point (current-point)))
    (pre-command-parse-check point)
    (with-mark ((form-start point :right-inserting)
		(form-end point))
      (unless (form-offset form-end 1) (editor-error))
      (form-offset (move-mark form-start form-end) -1)
      (with-mark ((containing-start form-start :left-inserting)
		  (containing-end form-end :left-inserting))
	(dotimes (i (or p 1))
	  (unless (and (forward-up-list containing-end)
		       (backward-up-list containing-start))
	    (editor-error)))
	(let ((r (copy-region (region form-start form-end))))
	  (ring-push (delete-and-save-region
		      (region containing-start containing-end))
		     *kill-ring*)
	  (ninsert-region point r)
	  (move-mark point form-start))))))

(defcommand "Extract List" (p)
  "Extract the current list.
  The current list replaces the surrounding list.  The entire affected
  area is pushed on the kill-ring.  With prefix argument, remove that
  many surrounding lists."
  "Replace the P containing lists with the current one."
  (let ((point (current-point)))
    (pre-command-parse-check point)
    (with-mark ((lstart point :right-inserting)
		(lend point))
      (if (eq (character-attribute :lisp-syntax (next-character lstart))
	      :open-paren)
	  (mark-after lend)
	  (unless (backward-up-list lstart) (editor-error)))
      (unless (forward-up-list lend) (editor-error))
      (with-mark ((rstart lstart)
		  (rend lend))
	(dotimes (i (or p 1))
	  (unless (and (forward-up-list rend) (backward-up-list rstart))
	    (editor-error)))
	(let ((r (copy-region (region lstart lend))))
	  (ring-push (delete-and-save-region (region rstart rend))
		     *kill-ring*)
	  (ninsert-region point r)
	  (move-mark point lstart))))))

(defcommand "Transpose Forms" (p)
  "Transpose Forms immediately preceding and following the point.
  With a zero argument, tranposes the Forms at the point and the mark.
  With a positive argument, transposes the Form preceding the point
  with the p-th one following it.  With a negative argument, transposes the
  Form following the point with the p-th one preceding it."
  "Transpose Forms immediately preceding and following the point."
  (let ((point (current-point))
	(count (or p 1)))
    (pre-command-parse-check point)
    (if (zerop count)
	(let ((mark (current-mark)))
	  (with-mark ((s1 mark :left-inserting)
		      (s2 point :left-inserting))
	    (scan-char s1 :whitespace nil)
	    (scan-char s2 :whitespace nil)
	    (with-mark ((e1 s1 :right-inserting)
			(e2 s2 :right-inserting))
	      (unless (form-offset e1 1) (editor-error))
	      (unless (form-offset e2 1) (editor-error))
	      (ninsert-region s1 (delete-and-save-region (region s2 e2)))
	      (ninsert-region s2 (delete-and-save-region (region s1 e1))))))
	(let ((fcount (if (plusp count) count 1))
	      (bcount (if (plusp count) 1 count)))
	  (with-mark ((s1 point :left-inserting)
		      (e2 point :right-inserting))
	    (dotimes (i bcount)
	      (unless (form-offset s1 -1) (editor-error)))
	    (dotimes (i fcount)
	      (unless (form-offset e2 1) (editor-error)))
	    (with-mark ((e1 s1 :right-inserting)
			(s2 e2 :left-inserting))
	      (unless (form-offset e1 1) (editor-error))
	      (unless (form-offset s2 -1) (editor-error))
	      (ninsert-region s1 (delete-and-save-region (region s2 e2)))
	      (ninsert-region s2 (delete-and-save-region (region s1 e1)))
	      (move-mark point s2)))))))


(defcommand "Insert ()" (count)
  "Insert a pair of parentheses ().  With positive argument, puts
   parentheses around the next COUNT Forms, or previous COUNT forms, if
   COUNT is negative.  The point is positioned after the open parenthesis."
  "Insert a pair of parentheses ()."
  ;; TODO Form navigation is broken, so this is broken too -- it is
  ;; possible to put parens around more forms than there are in current
  ;; expression.  It works by moving past as many forms as there is, and
  ;; then each delimiting paren also counts as a form.
  (let ((point (current-point)))
    (pre-command-parse-check point)
    (cond (count
	   (when (minusp count)
	     (form-offset point count)
	     (setq count (- count)))
	   (insert-character point #\()
	   (with-mark ((m point))
	     (unless (form-offset m count)
	       (editor-error "Could not find that many forms."))
	     (insert-character m #\))))
	  ;; The simple case with no prefix argument
	  (t
	   (insert-character point #\()
	   (insert-character point #\))
	   (mark-before point)))))


(defcommand "Move Over )" (p)
  "Move past the next close parenthesis, and start a new line.  Any
   indentation preceding the preceding the parenthesis is deleted, and the
   new line is indented.  If there is only whitespace preceding the close
   paren, the paren is moved to the end of the previous line. With prefix
   argument, this command moves past next closing paren and inserts space."
  "Move past the next close parenthesis, and start a new line."
  ;; TODO This is still not complete, because SCAN-CHAR finds the next
  ;; close-paren, but we need to find the next paren that closes current
  ;; expression.  This will have to be updated when form navigation is
  ;; fixed.
  (let ((point (current-point)))
    (pre-command-parse-check point)
    (with-mark ((m point :right-inserting))
      (cond ((scan-char m :lisp-syntax :close-paren)
	     (cond ((same-line-p point m)
		    (delete-horizontal-space m))
		   (t
		    (move-mark point m)
		    (reverse-find-attribute point :whitespace #'zerop)
		    (delete-region (region point m))))
	     (cond ((not p)
		    ;; Move to the previous line if current is empty
		    (when (zerop (mark-charpos m))
		      (delete-characters m -1))
		    (mark-after m)
		    (move-mark point m)
		    (indent-new-line-command 1))
		   (t
		    (mark-after m)
		    (move-mark point m)
		    (insert-character m #\space))))
	    (t 
	     (editor-error "Could not find closing paren."))))))


(defcommand "Forward Up List" (p)
    "Move forward past a one containing )."
    "Move forward past a one containing )."
  (or (collapse-if-selection :direction :forward)
      (let ((point (current-point-collapsing-selection))
            (count (or p 1)))
        (pre-command-parse-check point)
        (if (minusp count)
            (backward-up-list-command (- count))
            (with-mark ((m point))
              (dotimes (i count (move-mark point m))
                (unless (forward-up-list m) (editor-error))))))))

(defcommand "Backward Up List" (p)
    "Move backward past a one containing (."
    "Move backward past a one containing (."
  (or (collapse-if-selection :direction :backward)
      (let ((point (current-point-collapsing-selection))
            (count (or p 1)))
        (pre-command-parse-check point)
        (if (minusp count)
            (forward-up-list-command (- count))
            (with-mark ((m point))
              (dotimes (i count (move-mark point m))
                (unless (backward-up-list m) (editor-error))))))))


(defcommand "Down List" (p)
  "Move down a level in list structure.  With positive argument, moves down
   p levels.  With negative argument, moves down backward, but only one
   level."
  "Move down a level in list structure."
  (let ((point (current-point-collapsing-selection))
	(count (or p 1)))
    (pre-command-parse-check point)
    (with-mark ((m point))
      (cond ((plusp count)
	     (loop repeat count
                   do (unless (and (scan-char m :lisp-syntax :open-paren)
                                   (mark-after m))
                        (editor-error))))
	    (t
	     (unless (and (rev-scan-char m :lisp-syntax :close-paren)
			  (mark-before m))
	       (editor-error))))
      (move-mark point m))))



;;;; Filling Lisp comments, strings, and indented text.

(defhvar "Fill Lisp Comment Paragraph Confirm"
  "This determines whether \"Fill Lisp Comment Paragraph\" will prompt for
   confirmation to fill contiguous lines with the same initial whitespace when
   it is invoked outside of a comment or string."
  :value t)

(defcommand "Fill Lisp Comment Paragraph" (p)
  "This fills a flushleft or indented Lisp comment.
   This also fills Lisp string literals using the proper indentation as a
   filling prefix.  When invoked outside of a comment or string, this tries
   to fill all contiguous lines beginning with the same initial, non-empty
   blankspace.  When filling a comment, the current line is used to determine a
   fill prefix by taking all the initial whitespace on the line, the semicolons,
   and any whitespace following the semicolons."
  "Fills a flushleft or indented Lisp comment."
  (declare (ignore p))
  (let ((point (current-point)))
    (pre-command-parse-check point)
    (with-mark ((start point)
		(end point)
		(m point))
      (let ((commentp (fill-lisp-comment-paragraph-prefix start end)))
	(cond (commentp
	       (fill-lisp-comment-or-indented-text start end))
	      ((and (not (valid-spot m nil))
		    (form-offset m -1)
		    (eq (character-attribute :lisp-syntax (next-character m))
			:string-quote))
	       (fill-lisp-string m))
	      ((or (not (value fill-lisp-comment-paragraph-confirm))
		   (prompt-for-y-or-n
		    :prompt '("Not in a comment or string.  Fill contiguous ~
			       lines with the same initial whitespace? ")))
	       (fill-lisp-comment-or-indented-text start end)))))))

;;; FILL-LISP-STRING -- Internal.
;;;
;;; This fills the Lisp string containing mark as if it had been entered using
;;; Hemlock's Lisp string indentation, "Indent Function" for "Lisp" mode.  This
;;; assumes the area around mark has already been PRE-COMMAND-PARSE-CHECK'ed,
;;; and it ensures the string ends before doing any filling.  This function
;;; is undo'able.
;;;
(defun fill-lisp-string (mark)
  (with-mark ((end mark))
    (unless (form-offset end 1)
      (editor-error "Attempted to fill Lisp string, but it doesn't end?"))
    (let* ((mark (copy-mark mark :left-inserting))
	   (end (copy-mark end :left-inserting))
	   (string-region (region mark end))
	   (undo-region (copy-region string-region))
	   (hack (make-empty-region)))
      ;; Generate prefix.
      (indent-to-column (region-end hack) (1+ (mark-column mark)))
      ;; Skip opening double quote and fill string starting on its own line.
      (mark-after mark)
      (insert-character mark #\newline)
      (line-start mark)
      (setf (mark-kind mark) :right-inserting)
      (fill-region string-region (region-to-string hack))
      ;; Clean up inserted prefix on first line, delete inserted newline, and
      ;; move before the double quote for undo.
      (with-mark ((text mark :left-inserting))
	(find-attribute text :whitespace #'zerop)
	(delete-region (region mark text)))
      (delete-characters mark -1)
      (mark-before mark)
      ;; Save undo.
      (make-region-undo :twiddle "Fill Lisp Comment Paragraph"
			string-region undo-region))))

;;; FILL-LISP-COMMENT-OR-INDENTED-TEXT -- Internal.
;;;
;;; This fills all contiguous lines around start and end containing fill prefix
;;; designated by the region between start and end.  These marks can only be
;;; equal when there is no comment and no initial whitespace.  This is a bad
;;; situation since this function in that situation would fill the entire
;;; buffer into one paragraph.  This function is undo'able.
;;;
(defun fill-lisp-comment-or-indented-text (start end)
  (when (mark= start end)
    (editor-error "This command only fills Lisp comments, strings, or ~
		   indented text, but this line is flushleft."))
  ;;
  ;; Find comment block.
  (let* ((prefix (region-to-string (region start end)))
	 (length (length prefix)))
    (declare (simple-string prefix))
    (flet ((frob (mark direction)
	     (loop
	       (let* ((line (line-string (mark-line mark)))
		      (line-len (length line)))
		 (declare (simple-string line))
		 (unless (string= line prefix :end1 (min line-len length))
		   (when (= direction -1)
		     (unless (same-line-p mark end) (line-offset mark 1 0)))
		   (return)))
	       (unless (line-offset mark direction 0)
		 (when (= direction 1) (line-end mark))
		 (return)))))
      (frob start -1)
      (frob end 1))
    ;;
    ;; Do it undoable.
    (let* ((start1 (copy-mark start :right-inserting))
	   (end2 (copy-mark end :left-inserting))
	   (region (region start1 end2))
	   (undo-region (copy-region region)))
      (fill-region region prefix)
      (make-region-undo :twiddle "Fill Lisp Comment Paragraph"
			region undo-region))))

;;; FILL-LISP-COMMENT-PARAGRAPH-PREFIX -- Internal.
;;;
;;; This sets start and end around the prefix to be used for filling.  We
;;; assume we are dealing with a comment.  If there is no ";", then we try to
;;; find some initial whitespace.  If there is a ";", we make sure the line is
;;; blank before it to eliminate ";"'s in the middle of a line of text.
;;; Finally, if we really have a comment instead of some indented text, we skip
;;; the ";"'s and any immediately following whitespace.  We allow initial
;;; whitespace, so we can fill strings with the same command.
;;;
(defun fill-lisp-comment-paragraph-prefix (start end)
  (line-start start)
  (let ((commentp t)) ; Assumes there's a comment.
    (unless (to-line-comment (line-start end) ";")
      (find-attribute end :whitespace #'zerop)
      #|(when (start-line-p end)
	(editor-error "No comment on line, and no initial whitespace."))|#
      (setf commentp nil))
    (when commentp
      (unless (blank-before-p end)
	(find-attribute (line-start end) :whitespace #'zerop)
	#|(when (start-line-p end)
	  (editor-error "Semicolon preceded by unindented text."))|#
	(setf commentp nil)))
    (when commentp
      (find-attribute end :lisp-syntax #'(lambda (x) (not (eq x :comment))))
      (find-attribute end :whitespace #'zerop))
    commentp))



;;;; "Lisp" mode.

(defcommand "LISP Mode" (p)
  "Put current buffer in LISP mode." 
  "Put current buffer in LISP mode."  
  (declare (ignore p))
  (setf (buffer-major-mode (current-buffer)) "LISP"))


(defmode "Lisp" :major-p t :setup-function 'setup-lisp-mode)


(defun buffer-first-in-package-form (buffer)
  "Returns the package name referenced in the first apparent IN-PACKAGE
   form in buffer, or NIL if it can't find an IN-PACKAGE."
  (let* ((pattern (new-search-pattern :string-insensitive :forward "in-package" nil))
         (mark (copy-mark (buffer-start-mark buffer))))
    (with-mark ((start mark)
                (end mark))
      (loop
        (unless (find-pattern mark pattern)
          (return))
        (pre-command-parse-check mark)
        (when (valid-spot mark t)
          (move-mark end mark)
          (when (form-offset end 1)
            (move-mark start end)
            (when (backward-up-list start)
              (when (scan-char start :lisp-syntax :constituent)
                (let* ((s (nstring-upcase (region-to-string (region start end))))
                       (*package* (find-package "CL-USER")))
                  (unless (eq (ignore-errors (values (read-from-string s)))
                              'in-package)
                    (return)))
                (unless (form-offset end 1) (return))
                (move-mark start end)
                (form-offset start -1)
                (let* ((pkgname (ignore-errors (values (read-from-string (region-to-string (region start end)))))))
                  (return
                    (if pkgname
                      (values (ignore-errors (string pkgname))))))))))))))

(defparameter *previous-in-package-search-pattern*
    (new-search-pattern :string-insensitive :backward "in-package" nil))

(defun package-at-mark (start-mark)
  (let* ((pattern *previous-in-package-search-pattern*)
         (mark (copy-mark start-mark :temporary)))
    (with-mark ((start mark)
                (end mark)
                (list-end mark))
      (loop
        (unless (find-pattern mark pattern)
          (return))
        (pre-command-parse-check mark)
        (when (valid-spot mark t)
          (move-mark end mark)
          (when (form-offset end 1)
            (move-mark start end)
            (when (backward-up-list start)
              (move-mark list-end start)
              (unless (and (list-offset list-end 1)
                           (mark<= list-end start-mark))
                (return))
              (when (scan-char start :lisp-syntax :constituent)
                (unless (or (mark= mark start)
                            (let* ((s (nstring-upcase (region-to-string (region start end))))
                                   (*package* (find-package "CL-USER")))
                              (eq (ignore-errors (values (read-from-string s)))
                                  'in-package)))
                  (return))
                (unless (form-offset end 1) (format t "~& worse") (return 4))
                (move-mark start end)
                (form-offset start -1)
                (return
                  (if (eql (next-character start) #\")
                    (progn
                      (character-offset start 1)
                      (character-offset end -1)
                      (region-to-string (region start end)))
                    (let* ((pkgname (ignore-errors (values (read-from-string (region-to-string (region start end)))))))
                      (if pkgname
                        (values (ignore-errors (string pkgname)))))))))))))))

(defun ensure-buffer-package (buffer)
  (or (variable-value 'current-package :buffer buffer)
      (setf (variable-value 'current-package :buffer buffer)
            (buffer-first-in-package-form buffer))))

(defun buffer-package (buffer)
  (when (hemlock-bound-p 'current-package :buffer buffer)
    (let ((package-name (variable-value 'current-package :buffer buffer)))
      (find-package package-name))))

(defun setup-lisp-mode (buffer)
  (unless (hemlock-bound-p 'current-package :buffer buffer)
    (defhvar "Current Package"
      "The package used for evaluation of Lisp in this buffer."
      :buffer buffer
      :value nil
      :hooks (list 'package-name-change-hook)))
  (unless (hemlock-bound-p 'default-package :buffer buffer)
    (defhvar "Default Package"
      "The package to use if the current package doesn't exist or isn't set."
      :buffer buffer
      :value (package-name *package*))))





;;;; Some mode variables to coordinate with other stuff.

(defhvar "Auto Fill Space Indent"
  "When non-nil, uses \"Indent New Comment Line\" to break lines instead of
   \"New Line\"."
  :mode "Lisp" :value t)

(defhvar "Comment Start"
  "String that indicates the start of a comment."
  :mode "Lisp" :value ";")

(defhvar "Comment Begin"
  "String that is inserted to begin a comment."
  :mode "Lisp" :value "; ")

(defhvar "Indent Function"
  "Indentation function which is invoked by \"Indent\" command.
   It must take one argument that is the prefix argument."
  :value 'indent-for-lisp
  :mode "Lisp")

(defun string-to-arglist (string buffer &optional quiet-if-unknown)
  (multiple-value-bind (name error)
      (let* ((*package* (or
                         (find-package
                          (variable-value 'current-package :buffer buffer))
                         *package*)))
        (ignore-errors (values (read-from-string string))))
    (unless error
      (when (typep name 'symbol)
        (multiple-value-bind (arglist win)
            (ccl::arglist-string name)
          (if (or win (not quiet-if-unknown))
            (format nil "~S : ~A" name (if win (or arglist "()") "(unknown)"))))))))

(defcommand "Current Function Arglist" (p)
  "Show arglist of function whose name precedes point."
  "Show arglist of function whose name precedes point."
  (declare (ignore p))
  (let ((point (current-point)))
    (pre-command-parse-check point)
    (with-mark ((mark1 point)
		(mark2 point))
      (when (backward-up-list mark1)
        (when (form-offset (move-mark mark2 (mark-after mark1)) 1)
          (let* ((fun-name (region-to-string (region mark1 mark2)))
                 (arglist-string (string-to-arglist fun-name (current-buffer))))
            (when arglist-string
              (message "~a" arglist-string))))))))

(defcommand "Arglist On Space" (p)
  "Insert a space, then show the current function's arglist."
  "Insert a space, then show the current function's arglist."
  (declare (ignore p))
  (let ((point (current-point)))
    (insert-character point #\Space)
    (pre-command-parse-check point)
    (with-mark ((mark1 point)
		(mark2 point))
      (when (backward-up-list mark1)
        (when (form-offset (move-mark mark2 (mark-after mark1)) 1)
          (with-mark ((mark3 mark2))
            (do* ()
                 ((mark= mark3 point)
                  (let* ((fun-name (region-to-string (region mark1 mark2)))
                         (arglist-string
                          (string-to-arglist fun-name (current-buffer) t)))
                    (when arglist-string
                      (message "~a" arglist-string))))
              (if (ccl::whitespacep (next-character mark3))
                (mark-after mark3)
                (return nil)))))))))

(hi:defcommand "Show Callers" (p)
  "Display a scrolling list of the callers of the symbol at point.
   Double-click a row to go to the caller's definition."
  (declare (ignore p))
  (with-mark ((mark1 (current-point))
              (mark2 (current-point)))
    (mark-symbol mark1 mark2)
    (with-input-from-region (s (region mark1 mark2))
      (let* ((symbol (read s)))
	(hemlock-ext:open-sequence-dialog
	 :title (format nil "Callers of ~a" symbol)
	 :sequence (ccl::callers symbol)
	 :action #'edit-definition)))))

;; Note this isn't necessarily called from hemlock, e.g. it might be called by cl:ed,
;; from any thread, or it might be called from a sequence dialog, etc.
(defun edit-definition (name)
  (flet ((get-source-alist (name)
           (mapcar #'(lambda (item) (cons name item))
                   (ccl::get-source-files-with-types&classes name))))
    (let* ((info (get-source-alist name)))
      (when (null info)
        (let* ((seen (list name))
               (found ())
               (pname (symbol-name name)))
          (dolist (pkg (list-all-packages))
            (let ((sym (find-symbol pname pkg)))
              (when (and sym (not (member sym seen)))
                (let ((new (get-source-alist sym)))
                  (when new
                    (setq info (nconc new info))
                    (push sym found)))
                (push sym seen))))
          (when found
            ;; Unfortunately, this puts the message in the wrong buffer (would be better in the destination buffer).
            (loud-message "No definitions for ~s, using ~s instead"
                          name (if (cdr found) found (car found))))))
      (if info
        (if (cdr info)
          (hemlock-ext:open-sequence-dialog
           :title (format nil "Definitions of ~s" name)
           :sequence info
           :action #'(lambda (item) (hemlock-ext:edit-single-definition (car item) (cdr item)))
           :printer #'(lambda (item stream) (prin1 (cadr item) stream)))
          (hemlock-ext:edit-single-definition (caar info) (cdar info)))
        (editor-error "No known definitions for ~s" name)))))

#||
(defcommand "Set Package Name" (p)
  (variable-value 'current-package :buffer buffer)
||#                
