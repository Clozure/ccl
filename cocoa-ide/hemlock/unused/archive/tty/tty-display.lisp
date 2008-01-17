;;; -*- Log: hemlock.log; Package: hemlock-internals -*-
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
;;;    Written by Bill Chiles.
;;;

(in-package :hemlock-internals)

(export '(redisplay redisplay-all define-tty-font))



;;;; Macros.

(eval-when (:compile-toplevel :execute)
(defmacro tty-hunk-modeline-pos (hunk)
  `(tty-hunk-text-height ,hunk))
) ;eval-when


(defvar *currently-selected-hunk* nil)
(defvar *hunk-top-line*)

(declaim (fixnum *hunk-top-line*))

(eval-when (:compile-toplevel :execute)
(defmacro select-hunk (hunk)
  `(unless (eq ,hunk *currently-selected-hunk*)
     (setf *currently-selected-hunk* ,hunk)
     (setf *hunk-top-line*
	   (the fixnum
		(1+ (the fixnum
			 (- (the fixnum
				 (tty-hunk-text-position ,hunk))
			    (the fixnum
				 (tty-hunk-text-height ,hunk)))))))))
) ;eval-when


;;; Screen image lines.
;;; 
(defstruct (si-line (:print-function print-screen-image-line)
		    (:constructor %make-si-line (chars)))
  (chars nil :type simple-string)
  (length 0)
  (fonts nil :type list))

(defun make-si-line (n)
  (%make-si-line (make-string n)))

(defun print-screen-image-line (obj str n)
  (declare (ignore n))
  (write-string "#<Screen Image Line \"" str)
  (write-string (si-line-chars obj) str :end (si-line-length obj))
  (write-string "\">" str))


(defun find-identical-prefix (dis-line dis-line-fonts si-line)
  (declare (type dis-line dis-line)
	   (type list dis-line-fonts)
	   (type si-line si-line))
  (let* ((dl-chars (dis-line-chars dis-line))
	 (dl-len (dis-line-length dis-line))
	 (si-chars (si-line-chars si-line))
	 (si-len (si-line-length si-line))
	 (okay-until 0))
    (declare (type simple-string dl-chars si-chars)
	     (type (and unsigned-byte fixnum) dl-len si-len)
	     (type (and unsigned-byte fixnum) okay-until))
    (do ((dl-fonts dis-line-fonts (cdr dis-line-fonts))
	 (si-fonts (si-line-fonts si-line) (cdr si-fonts)))
	((or (null dl-fonts) (null si-fonts))
	 (let ((next-font (car (or dl-fonts si-fonts))))
	   (if next-font
	       (let ((end (min dl-len si-len (cadr next-font))))
		 (or (string/= dl-chars si-chars
			       :start1 okay-until :start2 okay-until
			       :end1 end :end2 end)
		     end))
	       (let ((end (min dl-len si-len)))
		 (or (string/= dl-chars si-chars
			       :start1 okay-until :start2 okay-until
			       :end1 end :end2 end)
		     (if (= dl-len si-len) nil end))))))
      (let ((dl-font (caar dl-fonts))
	    (dl-start (cadar dl-fonts))
	    (dl-stop (cddar dl-fonts))
	    (si-font (caar si-fonts))
	    (si-start (cadar si-fonts))
	    (si-stop (cddar si-fonts)))
	(unless (and (= dl-font si-font)
		     (= dl-start si-start))
	  (let ((font-lossage (min dl-start si-start)))
	    (return (or (string/= dl-chars si-chars
				  :start1 okay-until :start2 okay-until
				  :end1 font-lossage :end2 font-lossage)
			font-lossage))))
	(unless (= dl-stop si-stop)
	  (let ((font-lossage (min dl-stop si-stop)))
	    (return (or (string/= dl-chars si-chars
				  :start1 okay-until :start2 okay-until
				  :end1 font-lossage :end2 font-lossage)
			font-lossage))))
	(let ((mismatch (string/= dl-chars si-chars
				  :start1 okay-until :start2 okay-until
				  :end1 dl-stop :end2 si-stop)))
	  (if mismatch
	      (return mismatch)
	      (setf okay-until dl-stop)))))))


(defun find-identical-suffix (dis-line dis-line-fonts si-line)
  (declare (type dis-line dis-line)
	   (type list dis-line-fonts)
	   (type si-line si-line))
  (let* ((dl-chars (dis-line-chars dis-line))
	 (dl-len (dis-line-length dis-line))
	 (si-chars (si-line-chars si-line))
	 (si-len (si-line-length si-line))
	 (count (dotimes (i (min dl-len si-len) i)
		  (when (char/= (schar dl-chars (- dl-len i 1))
				(schar si-chars (- si-len i 1)))
		    (return i)))))
    (declare (type simple-string dl-chars si-chars)
	     (type (and unsigned-byte fixnum) dl-len si-len))
    (do ((dl-fonts (reverse dis-line-fonts) (cdr dis-line-fonts))
	 (si-fonts (reverse (si-line-fonts si-line)) (cdr si-fonts)))
	((or (null dl-fonts) (null si-fonts))
	 (cond (dl-fonts
		(min (- dl-len (cddar dl-fonts)) count))
	       (si-fonts
		(min (- si-len (cddar si-fonts)) count))
	       (t
		count)))
      (let ((dl-font (caar dl-fonts))
	    (dl-start (- dl-len (cadar dl-fonts)))
	    (dl-stop (- dl-len (cddar dl-fonts)))
	    (si-font (caar si-fonts))
	    (si-start (- si-len (cadar si-fonts)))
	    (si-stop (- si-len (cddar si-fonts))))
	(unless (and (= dl-font si-font)
		     (= dl-stop si-stop))
	  (return (min dl-stop si-stop count)))
	(unless (= dl-start si-start)
	  (return (min dl-start si-start count)))
	(when (<= count dl-start)
	  (return count))))))


(defmacro si-line (screen-image n)
  `(svref ,screen-image ,n))



;;; Font support.

(defvar *tty-font-strings* (make-array font-map-size :initial-element nil)
  "Array of (start-string . end-string) for fonts, or NIL if no such font.")

(defun define-tty-font (font-id &rest stuff)
  (unless (<= 0 font-id (1- font-map-size))
    (error "Bogus font-id: ~S" font-id))
  (cond ((every #'keywordp stuff)
	 (error "Can't extract font strings from the termcap entry yet."))
	((and (= (length stuff) 2)
	      (stringp (car stuff))
	      (stringp (cadr stuff)))
	 (setf (aref *tty-font-strings* font-id)
	       (cons (car stuff) (cadr stuff))))
	(t
	 (error "Bogus font spec: ~S~%Must be either a list of keywords or ~
		 a list of the start string and end string."))))


(defun compute-font-usages (dis-line)
  (do ((results nil)
       (change (dis-line-font-changes dis-line) (font-change-next change))
       (prev nil change))
      ((null change)
       (when prev
	 (let ((font (font-change-font prev)))
	   (when (and (not (zerop font))
		      (aref *tty-font-strings* font))
	     (push (list* (font-change-font prev)
			  (font-change-x prev)
			  (dis-line-length dis-line))
		   results))))
       (nreverse results))
    (when prev
      (let ((font (font-change-font prev)))
	(when (and (not (zerop font))
		   (aref *tty-font-strings* font))
	  (push (list* (font-change-font prev)
		       (font-change-x prev)
		       (font-change-x change))
		results))))))


;;;; Dumb window redisplay.

(defmacro tty-dumb-line-redisplay (device hunk dis-line &optional y)
  (let ((dl (gensym)) (dl-chars (gensym)) (dl-fonts (gensym)) (dl-len (gensym))
	(dl-pos (gensym)) (screen-image-line (gensym)))
    `(let* ((,dl ,dis-line)
	    (,dl-chars (dis-line-chars ,dl))
	    (,dl-fonts (compute-font-usages ,dis-line))
	    (,dl-len (dis-line-length ,dl))
	    (,dl-pos ,(or y `(dis-line-position ,dl))))
       (funcall (tty-device-display-string ,device)
		,hunk 0 ,dl-pos ,dl-chars ,dl-fonts 0 ,dl-len)
       (setf (dis-line-flags ,dl) unaltered-bits)
       (setf (dis-line-delta ,dl) 0)
       (select-hunk ,hunk)
       (let ((,screen-image-line (si-line (tty-device-screen-image ,device)
					  (+ *hunk-top-line* ,dl-pos))))
	 (replace-si-line (si-line-chars ,screen-image-line) ,dl-chars
			  0 0 ,dl-len)
	 (setf (si-line-length ,screen-image-line) ,dl-len)
	 (setf (si-line-fonts ,screen-image-line) ,dl-fonts)))))

(defun tty-dumb-window-redisplay (window)
  (let* ((first (window-first-line window))
	 (hunk (window-hunk window))
	 (device (device-hunk-device hunk))
	 (screen-image (tty-device-screen-image device)))
    (funcall (tty-device-clear-to-eow device) hunk 0 0)
    (do ((i 0 (1+ i))
	 (dl (cdr first) (cdr dl)))
	((eq dl the-sentinel)
	 (setf (window-old-lines window) (1- i))
	 (select-hunk hunk)
	 (do ((last (tty-hunk-text-position hunk))
	      (i (+ *hunk-top-line* i) (1+ i)))
	     ((> i last))
	   (declare (fixnum i last))
	   (let ((si-line (si-line screen-image i)))
	     (setf (si-line-length si-line) 0)
	     (setf (si-line-fonts si-line) nil))))
      (tty-dumb-line-redisplay device hunk (car dl) i))
    (setf (window-first-changed window) the-sentinel
	  (window-last-changed window) first)
    (when (window-modeline-buffer window)
      (let ((dl (window-modeline-dis-line window))
	    (y (tty-hunk-modeline-pos hunk)))
	(unwind-protect
	    (progn
	      (funcall (tty-device-standout-init device) hunk)
	      (funcall (tty-device-clear-to-eol device) hunk 0 y)
	      (tty-dumb-line-redisplay device hunk dl y))
	  (funcall (tty-device-standout-end device) hunk))
	(setf (dis-line-flags dl) unaltered-bits)))))



;;;; Dumb redisplay top n lines of a window.

(defun tty-redisplay-n-lines (window n)
  (let* ((hunk (window-hunk window))
	 (device (device-hunk-device hunk)))
    (funcall (tty-device-clear-lines device) hunk 0 0 n)
    (do ((n n (1- n))
	 (dl (cdr (window-first-line window)) (cdr dl)))
	((or (zerop n) (eq dl the-sentinel)))
      (tty-dumb-line-redisplay device hunk (car dl)))))



;;;; Semi dumb window redisplay

;;; This is for terminals without opening and deleting lines.

;;; TTY-SEMI-DUMB-WINDOW-REDISPLAY is a lot like TTY-SMART-WINDOW-REDISPLAY,
;;; but it calls different line redisplay functions.
;;; 
(defun tty-semi-dumb-window-redisplay (window)
  (let* ((hunk (window-hunk window))
	 (device (device-hunk-device hunk)))
    (let ((first-changed (window-first-changed window))
	  (last-changed (window-last-changed window)))
      ;; Is there anything to do?
      (unless (eq first-changed the-sentinel)
	(if ;; One line-changed.
	    (and (eq first-changed last-changed)
		 (zerop (dis-line-delta (car first-changed))))
	    (tty-semi-dumb-line-redisplay device hunk (car first-changed))
	    ;; More lines changed.
	    (do-semi-dumb-line-writes first-changed last-changed hunk))
	;; Set the bounds so we know we displayed...
	(setf (window-first-changed window) the-sentinel
	      (window-last-changed window) (window-first-line window))))
    ;;
    ;; Clear any extra lines at the end of the window.
    (let ((pos (dis-line-position (car (window-last-line window)))))
      (when (< pos (1- (window-height window)))
	(tty-smart-clear-to-eow hunk (1+ pos)))
      (setf (window-old-lines window) pos))
    ;;
    ;; Update the modeline if needed.
    (when (window-modeline-buffer window)
      (let ((dl (window-modeline-dis-line window)))
	(when (/= (dis-line-flags dl) unaltered-bits)
	  (unwind-protect
	      (progn
		(funcall (tty-device-standout-init device) hunk)
		(tty-smart-line-redisplay device hunk dl
					  (tty-hunk-modeline-pos hunk)))
	    (funcall (tty-device-standout-end device) hunk)))))))

;;; NEXT-DIS-LINE is used in DO-SEMI-DUMB-LINE-WRITES and
;;; COMPUTE-TTY-CHANGES.
;;; 
(eval-when (:compile-toplevel :execute)
(defmacro next-dis-line ()
  `(progn 
    (setf prev dl)
    (setf dl (cdr dl))
    (setf flags (dis-line-flags (car dl)))))
) ;eval-when

;;; DO-SEMI-DUMB-LINE-WRITES does what it says until it hits the last
;;; changed line.  The commented out code was a gratuitous optimization,
;;; especially if the first-changed line really is the first changes line.
;;; Anyway, this had to be removed because of this function's use in
;;; TTY-SMART-WINDOW-REDISPLAY, which was punting line moves due to
;;; "Scroll Redraw Ratio".  However, these supposedly moved lines had their
;;; bits set to unaltered bits in COMPUTE-TTY-CHANGES because it was
;;; assuming TTY-SMART-WINDOW-REDISPLAY guaranteed to do line moves.
;;; 
(defun do-semi-dumb-line-writes (first-changed last-changed hunk)
  (let* ((dl first-changed)
	 flags ;(dis-line-flags (car dl))) flags bound for NEXT-DIS-LINE.
	 prev)
    ;;
    ;; Skip old, unchanged, unmoved lines.
    ;; (loop
    ;;  (unless (zerop flags) (return))
    ;;  (next-dis-line))
    ;;
    ;; Write every remaining line.
    (let* ((device (device-hunk-device hunk))
	   (force-output (device-force-output device)))
      (loop
       (tty-semi-dumb-line-redisplay device hunk (car dl))
       (when force-output (funcall force-output))
       (next-dis-line)
       (when (eq prev last-changed) (return))))))

;;; TTY-SEMI-DUMB-LINE-REDISPLAY finds the first different character
;;; comparing the display line and the screen image line, writes out the
;;; rest of the display line, and clears to end-of-line as necessary.
;;; 
(defun tty-semi-dumb-line-redisplay (device hunk dl
				     &optional (dl-pos (dis-line-position dl)))
  (declare (fixnum dl-pos))
  (let* ((dl-chars (dis-line-chars dl))
	 (dl-len (dis-line-length dl))
	 (dl-fonts (compute-font-usages dl)))
    (declare (fixnum dl-len) (simple-string dl-chars))
    (when (listen-editor-input *editor-input*)
      (throw 'redisplay-catcher :editor-input))
    (select-hunk hunk)
    (let* ((screen-image-line (si-line (tty-device-screen-image device)
				       (+ *hunk-top-line* dl-pos)))
	   (si-line-chars (si-line-chars screen-image-line))
	   (si-line-length (si-line-length screen-image-line))
	   (findex (find-identical-prefix dl dl-fonts screen-image-line)))
      (declare (type (or fixnum null) findex) (simple-string si-line-chars))
      ;;
      ;; When the dis-line and screen chars are not string=.
      (when findex
	(cond
	 ;; See if the screen shows an initial substring of the dis-line.
	 ((= findex si-line-length)
	  (funcall (tty-device-display-string device)
		   hunk findex dl-pos dl-chars dl-fonts findex dl-len)
	  (replace-si-line si-line-chars dl-chars findex findex dl-len))
	 ;; When the dis-line is an initial substring of what's on the screen.
	 ((= findex dl-len)
	  (funcall (tty-device-clear-to-eol device) hunk dl-len dl-pos))
	 ;; Otherwise, blast dl-chars and clear to eol as necessary.
	 (t (funcall (tty-device-display-string device)
		     hunk findex dl-pos dl-chars dl-fonts findex dl-len)
	    (when (< dl-len si-line-length)
	      (funcall (tty-device-clear-to-eol device) hunk dl-len dl-pos))
	    (replace-si-line si-line-chars dl-chars findex findex dl-len)))
	(setf (si-line-length screen-image-line) dl-len)
	(setf (si-line-fonts screen-image-line) dl-fonts)))
    (setf (dis-line-flags dl) unaltered-bits)
    (setf (dis-line-delta dl) 0)))



;;;; Smart window redisplay -- operation queues and internal screen image.

;;; This is used for creating temporary smart redisplay structures.
;;; 
(defconstant tty-hunk-height-limit 100)


;;; Queues for redisplay operations and access macros.
;;; 
(defvar *tty-line-insertions* (make-array (* 2 tty-hunk-height-limit)))

(defvar *tty-line-deletions* (make-array (* 2 tty-hunk-height-limit)))

(defvar *tty-line-writes* (make-array tty-hunk-height-limit))

(defvar *tty-line-moves* (make-array tty-hunk-height-limit))

(eval-when (:compile-toplevel :execute)

(defmacro queue (value queue ptr)
  `(progn
    (setf (svref ,queue ,ptr) ,value)
    (the fixnum (incf (the fixnum ,ptr)))))

(defmacro dequeue (queue ptr)
  `(prog1
    (svref ,queue ,ptr)
    (the fixnum (incf (the fixnum ,ptr)))))

) ;eval-when

;;; INSERT-LINE-COUNT is used in TTY-SMART-WINDOW-REDISPLAY.  The counting is
;;; based on calls to QUEUE in COMPUTE-TTY-CHANGES.
;;; 
(defun insert-line-count (ins)
  (do ((i 1 (+ i 2))
       (count 0 (+ count (svref *tty-line-insertions* i))))
      ((> i ins) count)))


;;; Temporary storage for screen-image lines and accessing macros.
;;; 
(defvar *screen-image-temp* (make-array tty-hunk-height-limit))

(eval-when (:compile-toplevel :execute)

;;; DELETE-SI-LINES is used in DO-LINE-DELETIONS to simulate what's
;;; happening to the screen in a device's screen-image.  At y, num
;;; lines are deleted and saved in *screen-image-temp*; fsil is the
;;; end of the free screen image lines saved here.  Also, we must
;;; move lines up in the screen-image structure.  In the outer loop
;;; we save lines in the temp storage and move lines up at the same
;;; time.  In the termination/inner loop we move any lines that still
;;; need to be moved up.  The screen-length is adjusted by the fsil
;;; because any time a deletion is in progress, there are fsil bogus
;;; lines at the bottom of the screen image from lines being moved
;;; up previously.
;;; 
(defmacro delete-si-lines (screen-image y num fsil screen-length)
  (let ((do-screen-image (gensym)) (delete-index (gensym))
	(free-lines (gensym)) (source-index (gensym)) (target-index (gensym))
	(n (gensym)) (do-screen-length (gensym)) (do-y (gensym)))
    `(let ((,do-screen-image ,screen-image)
	   (,do-screen-length (- ,screen-length fsil))
	   (,do-y ,y))
       (declare (fixnum ,do-screen-length ,do-y))
       (do ((,delete-index ,do-y (1+ ,delete-index))
	    (,free-lines ,fsil (1+ ,free-lines))
	    (,source-index (+ ,do-y ,num) (1+ ,source-index))
	    (,n ,num (1- ,n)))
	   ((zerop ,n)
	    (do ((,target-index ,delete-index (1+ ,target-index))
		 (,source-index ,source-index (1+ ,source-index)))
		((>= ,source-index ,do-screen-length))
	      (declare (fixnum ,target-index ,source-index))
	      (setf (si-line ,do-screen-image ,target-index)
		    (si-line ,do-screen-image ,source-index))))
	 (declare (fixnum ,delete-index ,free-lines ,source-index ,n))
	 (setf (si-line *screen-image-temp* ,free-lines)
	       (si-line ,do-screen-image ,delete-index))
	 (when (< ,source-index ,do-screen-length)
	   (setf (si-line ,do-screen-image ,delete-index)
		 (si-line ,do-screen-image ,source-index)))))))


;;; INSERT-SI-LINES is used in DO-LINE-INSERTIONS to simulate what's
;;; happening to the screen in a device's screen-image.  At y, num free
;;; lines are inserted from *screen-image-temp*; fsil is the end of the
;;; free lines.  When copying lines down in screen-image, we must start
;;; with the lower lines and end with the higher ones, so we don't trash
;;; any lines.  The outer loop does all the copying, and the termination/
;;; inner loop inserts the free screen image lines, setting their length
;;; to zero.
;;; 
(defmacro insert-si-lines (screen-image y num fsil screen-length)
  (let ((do-screen-image (gensym)) (source-index (gensym))
	(target-index (gensym)) (target-terminus (gensym))
	(do-screen-length (gensym)) (temp (gensym)) (do-y (gensym))
	(insert-index (gensym)) (free-lines-index (gensym))
	(n (gensym)))
    `(let ((,do-screen-length ,screen-length)
	   (,do-screen-image ,screen-image)
	   (,do-y ,y))
       (do ((,target-terminus (1- (+ ,do-y ,num)))	 ; (1- target-start)
	    (,source-index (- ,do-screen-length ,fsil 1) ; (1- source-end)
			   (1- ,source-index))
	    (,target-index (- (+ ,do-screen-length ,num)
			      ,fsil 1)			 ; (1- target-end)
		(1- ,target-index)))
	   ((= ,target-index ,target-terminus)
	    (do ((,insert-index ,do-y (1+ ,insert-index))
		 (,free-lines-index (1- ,fsil) (1- ,free-lines-index))
		 (,n ,num (1- ,n)))
		((zerop ,n))
	      (declare (fixnum ,insert-index ,free-lines-index ,n))
	      (let ((,temp (si-line *screen-image-temp* ,free-lines-index)))
		(setf (si-line-length ,temp) 0)
		(setf (si-line-fonts ,temp) nil)
		(setf (si-line ,do-screen-image ,insert-index) ,temp)))
	    (decf ,fsil ,num))
	 (declare (fixnum ,target-terminus ,source-index ,target-index))
	 (setf (si-line ,do-screen-image ,target-index)
	       (si-line ,do-screen-image ,source-index))))))

) ;eval-when



;;;; Smart window redisplay -- the function.

;;; TTY-SMART-WINDOW-REDISPLAY sees if only one line changed after
;;; some preliminary processing.  If more than one line changed,
;;; then we compute changes to make to the screen in the form of
;;; line insertions, deletions, and writes.  Deletions must be done
;;; first, so lines are not lost off the bottom of the screen by
;;; inserting lines.
;;; 
(defun tty-smart-window-redisplay (window)
  (let* ((hunk (window-hunk window))
	 (device (device-hunk-device hunk)))
    (let ((first-changed (window-first-changed window))
	  (last-changed (window-last-changed window)))
      ;; Is there anything to do?
      (unless (eq first-changed the-sentinel)
	(if (and (eq first-changed last-changed)
		 (zerop (dis-line-delta (car first-changed))))
	    ;; One line-changed.
	    (tty-smart-line-redisplay device hunk (car first-changed))
	    ;; More lines changed.
	    (multiple-value-bind (ins outs writes moves)
				 (compute-tty-changes
				  first-changed last-changed
				  (tty-hunk-modeline-pos hunk))
	      (let ((ratio (variable-value 'hemlock::scroll-redraw-ratio)))
		(cond ((and ratio
			    (> (/ (insert-line-count ins)
				  (tty-hunk-text-height hunk))
			       ratio))
		       (do-semi-dumb-line-writes first-changed last-changed
						 hunk))
		      (t
		       (do-line-insertions hunk ins
					   (do-line-deletions hunk outs))
		       (note-line-moves moves)
		       (do-line-writes hunk writes))))))
	;; Set the bounds so we know we displayed...
	(setf (window-first-changed window) the-sentinel
	      (window-last-changed window) (window-first-line window))))
    ;;
    ;; Clear any extra lines at the end of the window.
    (let ((pos (dis-line-position (car (window-last-line window)))))
      (when (< pos (1- (window-height window)))
	(tty-smart-clear-to-eow hunk (1+ pos)))
      (setf (window-old-lines window) pos))
    ;;
    ;; Update the modeline if needed.
    (when (window-modeline-buffer window)
      (let ((dl (window-modeline-dis-line window)))
	(when (/= (dis-line-flags dl) unaltered-bits)
	  (unwind-protect
	      (progn
		(funcall (tty-device-standout-init device) hunk)
		(tty-smart-line-redisplay device hunk dl
					  (tty-hunk-modeline-pos hunk)))
	    (funcall (tty-device-standout-end device) hunk)))))))



;;;; Smart window redisplay -- computing changes to the display.

;;; There is a lot of documentation here to help since this code is not
;;; obviously correct.  The code is not that cryptic, but the correctness
;;; of the algorithm is somewhat.  Most of the complexity is in handling
;;; lines that moved on the screen which the introduction deals with.
;;; Also, the block of documentation immediately before the function
;;; COMPUTE-TTY-CHANGES has its largest portion dedicated to this part of
;;; the function which is the largest block of code in the function.

;;; The window image dis-lines are annotated with the difference between
;;; their current intended locations and their previous locations in the
;;; window.  This delta (distance moved) is negative for an upward move and
;;; positive for a downward move.  To determine what to do with moved
;;; groups of lines, we consider the transition (or difference in deltas)
;;; between two adjacent groups as we look at the window's dis-lines moving
;;; down the window image, disregarding whether they are contiguous (having
;;; moved only by a different delta) or separated by some lines (such as
;;; lines that are new and unmoved).
;;;
;;; Considering the transition between moved groups makes sense because a
;;; given group's delta affects all the lines below it since the dis-lines
;;; reflect the window's buffer's actual lines which are all connected in
;;; series.  Therefore, if the previous group moved up some delta number of
;;; lines because of line deletions, then the lines below this group (down
;;; to the last line of the window image) moved up by the same delta too,
;;; unless one of the following is true:
;;;    1] The lines below the group moved up by a greater delta, possibly
;;;       due to multiple disjoint buffer line deletions.
;;;    2] The lines below the group moved up by a lesser delta, possibly
;;;       due to a number (less than the previous delta) of new line
;;;       insertions below the group that moved up.
;;;    3] The lines below the group moved down, possibly due to a number
;;;       (greater than the previous delta) of new line insertions below
;;;       the group that moved up.
;;; Similarly, if the previous group moved down some delta number of lines
;;; because of new line insertions, then the lines below this group (down
;;; to the last line of the window image not to fall off the window's lower
;;; edge) moved down by the same delta too, unless one of the following is
;;; true:
;;;    1] The lines below the group moved down by a greater delta, possibly
;;;       due to multiple disjoint buffer line insertions.
;;;    2] The lines below the group moved down by a lesser delta, possibly
;;;       due to a number (less than the previous delta) of line deletions
;;;       below the group that moved down.
;;;    3] The lines below the group moved up, possibly due to a number
;;;       (greater than the previous delta) of line deletions below the
;;;       group that moved down.
;;;
;;; Now we can see how the first moved group affects the window image below
;;; it except where there is a lower group of lines that have moved a
;;; different delta due to separate operations on the buffer's lines viewed
;;; through a window.  We can see that this different delta is the expected
;;; effect throughout the window image below the second group, unless
;;; something lower down again has affected the window image.  Also, in the
;;; case of a last group of lines that moved up, the group will never
;;; reflect all of the lines in the window image from the first line to
;;; move down to the bottom of the window image because somewhere down below
;;; the group that moved up are some new lines that have just been drawn up
;;; into the window's image.
;;;

;;; COMPUTE-TTY-CHANGES is used once in TTY-SMART-WINDOW-REDISPLAY.
;;; It goes through all the display lines for a window recording where
;;; lines need to be inserted, deleted, or written to make the screen
;;; consistent with the internal image of the screen.  Pointers to
;;; the insertions, deletions, and writes that have to be done are
;;; returned.
;;; 
;;; If a line is new, then simply queue it to be written.
;;; 
;;; If a line is moved and/or changed, then we compute the difference
;;; between the last block of lines that moved with the same delta and the
;;; current block of lines that moved with the current delta.  If this
;;; difference is positive, then some lines need to be deleted.  Since we
;;; do all the line deletions first to prevent line insertions from
;;; dropping lines off the bottom of the screen, we have to compute the
;;; position of line deletions using the cumulative insertions
;;; (cum-inserts).  Without any insertions, deletions may be done right at
;;; the dis-line's new position.  With insertions needed above a given
;;; deletion point combined with the fact that deletions are all done
;;; first, the location for the deletion is higher than it would be without
;;; the insertions being done above the deletions.  The location of the
;;; deletion is higher by the number of insertions we have currently put
;;; off.  When computing the position of line insertions (a negative delta
;;; transition), we do not need to consider the cumulative insertions or
;;; cumulative deletions since everything above the point of insertion
;;; (both deletions and insertions) has been done.  Because of the screen
;;; state being correct above the point of an insertion, the screen is only
;;; off by the delta transition number of lines.  After determining the
;;; line insertions or deletions, loop over contiguous lines with the same
;;; delta queuing any changed ones to be written.  The delta and flag
;;; fields are initialized according to the need to be written; since
;;; redisplay may be interrupted by more user input after moves have been
;;; done to the screen, we save the changed bit on, so the line will be
;;; queued to be written after redisplay is re-entered.
;;; 
;;; If the line is changed or new, then queue it to be written.  Since we can
;;; abort out of the actual dislpay at any time (due to pending input), we
;;; don't clear the flags or delta here.  A dis-line may be groveled many times
;;; by this function before it actually makes it to the screen, so we may have
;;; odd combinations of bits such as both new and changed.
;;; 
;;; Otherwise, get the next display line, loop, and see if it's
;;; interesting.
;;; 
(defun compute-tty-changes (first-changed last-changed modeline-pos)
  (declare (fixnum modeline-pos))
  (let* ((dl first-changed)
	 (flags (dis-line-flags (car dl)))
	 (ins 0) (outs 0) (writes 0) (moves 0)
	 (prev-delta 0) (cum-deletes 0) (net-delta 0) (cum-inserts 0)
	 prev)
    (declare (fixnum flags ins outs writes moves prev-delta cum-deletes
		     net-delta cum-inserts))
    (loop
      (cond
       ((logtest flags new-bit)
	(queue (car dl) *tty-line-writes* writes)
	(next-dis-line))
       ((logtest flags moved-bit)
	(let* ((start-dl (car dl))
	       (start-pos (dis-line-position start-dl))
	       (curr-delta (dis-line-delta start-dl))
	       (delta-delta (- prev-delta curr-delta))
	       (car-dl start-dl))
	  (declare (fixnum start-pos curr-delta delta-delta))
	  (cond ((plusp delta-delta)
		 (queue (the fixnum (- start-pos cum-inserts))
			*tty-line-deletions* outs)
		 (queue delta-delta *tty-line-deletions* outs)
		 (incf cum-deletes delta-delta)
		 (decf net-delta delta-delta))
		((minusp delta-delta)
		 (let ((eff-pos (the fixnum (+ start-pos delta-delta)))
		       (num (the fixnum (- delta-delta))))
		   (queue eff-pos *tty-line-insertions* ins)
		   (queue num *tty-line-insertions* ins)
		   (incf net-delta num)
		   (incf cum-inserts num))))
	  (loop
	    (if (logtest flags (logior changed-bit new-bit))
		(queue car-dl *tty-line-writes* writes)
		(queue car-dl *tty-line-moves* moves))
	    (next-dis-line)
	    (setf car-dl (car dl))
	    (when (or (eq prev last-changed)
		      (/= (the fixnum (dis-line-delta car-dl)) curr-delta))
	      (setf prev-delta curr-delta)
	      (return)))))
       ((logtest flags (logior changed-bit new-bit))
	(queue (car dl) *tty-line-writes* writes)
	(next-dis-line))
       (t
	(next-dis-line)))

      (when (eq prev last-changed)
	(unless (zerop net-delta)
	  (cond ((plusp net-delta)
		 (queue (the fixnum (- modeline-pos cum-deletes net-delta))
			*tty-line-deletions* outs)
		 (queue net-delta *tty-line-deletions* outs))
		(t (queue (the fixnum (+ modeline-pos net-delta))
			  *tty-line-insertions* ins)
		   (queue (the fixnum (- net-delta))
			  *tty-line-insertions* ins))))
	(return (values ins outs writes moves))))))


;;;; Smart window redisplay -- operation methods.

;;; TTY-SMART-CLEAR-TO-EOW clears lines y through the last text line of hunk.
;;; It takes care not to clear a line unless it really has some characters
;;; displayed on it.  It also maintains the device's screen image lines.
;;; 
(defun tty-smart-clear-to-eow (hunk y)
  (let* ((device (device-hunk-device hunk))
	 (screen-image (tty-device-screen-image device))
	 (clear-to-eol (tty-device-clear-to-eol device)))
    (select-hunk hunk)
    (do ((y y (1+ y))
	 (si-idx (+ *hunk-top-line* y) (1+ si-idx))
	 (last (tty-hunk-text-position hunk)))
	((> si-idx last))
      (declare (fixnum y si-idx last))
      (let ((si-line (si-line screen-image si-idx)))
	(unless (zerop (si-line-length si-line))
	  (funcall clear-to-eol hunk 0 y)
	  (setf (si-line-length si-line) 0)
	  (setf (si-line-fonts si-line) nil))))))

;;; NOTE-LINE-MOVES  --  Internal
;;;
;;;    Clear out the flags and delta of lines that have been moved.
;;;
(defun note-line-moves (moves)
  (let ((i 0))
    (loop
      (when (= i moves) (return))
      (let ((dl (dequeue *tty-line-moves* i)))
	(setf (dis-line-flags dl) unaltered-bits)
	(setf (dis-line-delta dl) 0)))))

;;; DO-LINE-DELETIONS pops elements off the *tty-lines-deletions* queue,
;;; deleting lines from hunk's area of the screen.  The internal screen
;;; image is updated, and the total number of lines deleted is returned.
;;; 
(defun do-line-deletions (hunk outs)
  (declare (fixnum outs))
  (let* ((i 0)
	 (device (device-hunk-device hunk))
	 (fun (tty-device-delete-line device))
	 (fsil 0)) ;free-screen-image-lines
    (declare (fixnum i fsil))
    (loop
     (when (= i outs) (return fsil))
     (let ((y (dequeue *tty-line-deletions* i))
	   (num (dequeue *tty-line-deletions* i)))
       (declare (fixnum y num))
       (funcall fun hunk 0 y num)
       (select-hunk hunk)
       (delete-si-lines (tty-device-screen-image device)
			(+ *hunk-top-line* y) num fsil
			(tty-device-lines device))
       (incf fsil num)))))

;;; DO-LINE-INSERTIONS pops elements off the *tty-line-insertions* queue,
;;; inserting lines into hunk's area of the screen.  The internal screen
;;; image is updated using free screen image lines pointed to by fsil.
;;; 
(defun do-line-insertions (hunk ins fsil)
  (declare (fixnum ins fsil))
  (let* ((i 0)
	 (device (device-hunk-device hunk))
	 (fun (tty-device-open-line device)))
    (declare (fixnum i))
    (loop
     (when (= i ins) (return))
     (let ((y (dequeue *tty-line-insertions* i))
	   (num (dequeue *tty-line-insertions* i)))
       (declare (fixnum y num))
       (funcall fun hunk 0 y num)
       (select-hunk hunk)
       (insert-si-lines (tty-device-screen-image device)
			(+ *hunk-top-line* y) num fsil
			(tty-device-lines device))))))

;;; DO-LINE-WRITES pops elements off the *tty-line-writes* queue, displaying
;;; these dis-lines with TTY-SMART-LINE-REDISPLAY.  We force output after
;;; each line, so the user can see how far we've gotten in case he chooses
;;; to give more editor commands which will abort redisplay until there's no
;;; more input.
;;; 
(defun do-line-writes (hunk writes)
  (declare (fixnum writes))
  (let* ((i 0)
	 (device (device-hunk-device hunk))
	 (force-output (device-force-output device)))
    (declare (fixnum i))
    (loop
     (when (= i writes) (return))
     (tty-smart-line-redisplay device hunk (dequeue *tty-line-writes* i))
     (when force-output (funcall force-output)))))

;;; TTY-SMART-LINE-REDISPLAY uses an auxiliary screen image structure to
;;; try to do minimal character shipping to the terminal.  Roughly, we find
;;; the first different character when comparing what's on the screen and
;;; what should be there; we will start altering the line after this same
;;; initial substring.  Then we find, from the end, the first character
;;; that is different, blasting out characters to the lesser of the two
;;; indexes.  If the dis-line index is lesser, we have some characters to
;;; delete from the screen, and if the screen index is lesser, we have some
;;; additional dis-line characters to insert.  There are a few special
;;; cases that allow us to punt out of the above algorithm sketch.  If the
;;; terminal doesn't have insert mode or delete mode, we have blast out to
;;; the end of the dis-line and possibly clear to the end of the screen's
;;; line, as appropriate.  Sometimes we don't use insert or delete mode
;;; because of the overhead cost in characters; it simply is cheaper to
;;; blast out characters and clear to eol.
;;; 
(defun tty-smart-line-redisplay (device hunk dl
				 &optional (dl-pos (dis-line-position dl)))
  (declare (fixnum dl-pos))
  (let* ((dl-chars (dis-line-chars dl))
	 (dl-len (dis-line-length dl))
	 (dl-fonts (compute-font-usages dl)))
    (declare (fixnum dl-len) (simple-string dl-chars))
    (when (listen-editor-input *editor-input*)
      (throw 'redisplay-catcher :editor-input))
    (select-hunk hunk)
    (let* ((screen-image-line (si-line (tty-device-screen-image device)
				       (+ *hunk-top-line* dl-pos)))
	   (si-line-chars (si-line-chars screen-image-line))
	   (si-line-length (si-line-length screen-image-line))
	   (findex (find-identical-prefix dl dl-fonts screen-image-line)))
      (declare (type (or fixnum null) findex) (simple-string si-line-chars))
      ;;
      ;; When the dis-line and screen chars are not string=.
      (when findex
	(block tslr-main-body
	  ;;
	  ;; See if the screen shows an initial substring of the dis-line.
	  (when (= findex si-line-length)
	    (funcall (tty-device-display-string device)
		     hunk findex dl-pos dl-chars dl-fonts findex dl-len)
	    (replace-si-line si-line-chars dl-chars findex findex dl-len)
	    (return-from tslr-main-body t))
	  ;;
	  ;; When the dis-line is an initial substring of what's on the screen.
	  (when (= findex dl-len)
	    (funcall (tty-device-clear-to-eol device) hunk dl-len dl-pos)
	    (return-from tslr-main-body t))
	  ;;
	  ;; Find trailing substrings that are the same.
	  (multiple-value-bind
	      (sindex dindex)
	      (let ((count (find-identical-suffix dl dl-fonts
						  screen-image-line)))
		(values (- si-line-length count)
			(- dl-len count)))
	    (declare (fixnum sindex dindex))
	    ;;
	    ;; No trailing substrings -- blast and clear to eol.
	    (when (= dindex dl-len)
	      (funcall (tty-device-display-string device)
		       hunk findex dl-pos dl-chars dl-fonts findex dl-len)
	      (when (< dindex sindex)
		(funcall (tty-device-clear-to-eol device)
			 hunk dl-len dl-pos))
	      (replace-si-line si-line-chars dl-chars findex findex dl-len)
	      (return-from tslr-main-body t))
	    (let ((lindex (min sindex dindex)))
	      (cond ((< lindex findex)
		     ;; This can happen in funny situations -- believe me!
		     (setf lindex findex))
		    (t
		     (funcall (tty-device-display-string device)
			      hunk findex dl-pos dl-chars dl-fonts
			      findex lindex)
		     (replace-si-line si-line-chars dl-chars
				      findex findex lindex)))
	      (cond
	       ((= dindex sindex))
	       ((< dindex sindex)
		(let ((delete-char-num (- sindex dindex)))
		  (cond ((and (tty-device-delete-char device)
			      (worth-using-delete-mode
			       device delete-char-num (- si-line-length dl-len)))
			 (funcall (tty-device-delete-char device)
				  hunk dindex dl-pos delete-char-num))
			(t 
			 (funcall (tty-device-display-string device)
				  hunk dindex dl-pos dl-chars dl-fonts
				  dindex dl-len)
			 (funcall (tty-device-clear-to-eol device)
				  hunk dl-len dl-pos)))))
	       (t
		(if (and (tty-device-insert-string device)
			 (worth-using-insert-mode device (- dindex sindex)
						  (- dl-len sindex)))
		    (funcall (tty-device-insert-string device)
			     hunk sindex dl-pos dl-chars sindex dindex)
		    (funcall (tty-device-display-string device)
			     hunk sindex dl-pos dl-chars dl-fonts
			     sindex dl-len))))
	      (replace-si-line si-line-chars dl-chars
			       lindex lindex dl-len))))
	(setf (si-line-length screen-image-line) dl-len)
	(setf (si-line-fonts screen-image-line) dl-fonts)))
    (setf (dis-line-flags dl) unaltered-bits)
    (setf (dis-line-delta dl) 0)))



;;;; Device methods

;;; Initializing and exiting the device (DEVICE-INIT and DEVICE-EXIT functions).
;;; These can be found in Tty-Display-Rt.Lisp.


;;; Clearing the device (DEVICE-CLEAR functions).

(defun clear-device (device)
  (device-write-string (tty-device-clear-string device))
  (cursor-motion device 0 0)
  (setf (tty-device-cursor-x device) 0)
  (setf (tty-device-cursor-y device) 0))


;;; Moving the cursor around (DEVICE-PUT-CURSOR)

;;; TTY-PUT-CURSOR makes sure the coordinates are mapped from the hunk's
;;; axis to the screen's and determines the minimal cost cursor motion
;;; sequence.  Currently, it does no cost analysis of relative motion
;;; compared to absolute motion but simply makes sure the cursor isn't
;;; already where we want it.
;;;
(defun tty-put-cursor (hunk x y)
  (declare (fixnum x y))
  (select-hunk hunk)
  (let ((y (the fixnum (+ *hunk-top-line* y)))
	(device (device-hunk-device hunk)))
    (declare (fixnum y))
    (unless (and (= (the fixnum (tty-device-cursor-x device)) x)
		 (= (the fixnum (tty-device-cursor-y device)) y))
      (cursor-motion device x y)
      (setf (tty-device-cursor-x device) x)
      (setf (tty-device-cursor-y device) y))))

;;; UPDATE-CURSOR is used in device redisplay methods to make sure the
;;; cursor is where it should be.
;;; 
(eval-when (:compile-toplevel :execute)
  (defmacro update-cursor (hunk x y)
    `(funcall (device-put-cursor (device-hunk-device ,hunk)) ,hunk ,x ,y))
) ;eval-when

;;; CURSOR-MOTION takes two coordinates on the screen's axis,
;;; moving the cursor to that location.  X is the column index,
;;; and y is the line index, but Unix and Termcap believe that
;;; the default order of indexes is first the line and then the
;;; column or (y,x).  Because of this, when reversep is non-nil,
;;; we send first x and then y.
;;; 
(defun cursor-motion (device x y)
  (let ((x-add-char (tty-device-cm-x-add-char device))
	(y-add-char (tty-device-cm-y-add-char device))
	(x-condx-add (tty-device-cm-x-condx-char device))
	(y-condx-add (tty-device-cm-y-condx-char device))
	(one-origin (tty-device-cm-one-origin device)))
    (when x-add-char (incf x x-add-char))
    (when (and x-condx-add (> x x-condx-add))
      (incf x (tty-device-cm-x-condx-add-char device)))
    (when y-add-char (incf y y-add-char))
    (when (and y-condx-add (> y y-condx-add))
      (incf y (tty-device-cm-y-condx-add-char device)))
    (when one-origin (incf x) (incf y)))
  (device-write-string (tty-device-cm-string1 device))
  (let ((reversep (tty-device-cm-reversep device))
	(x-pad (tty-device-cm-x-pad device))
	(y-pad (tty-device-cm-y-pad device)))
    (if reversep
	(cm-output-coordinate x x-pad)
	(cm-output-coordinate y y-pad))
    (device-write-string (tty-device-cm-string2 device))
    (if reversep
	(cm-output-coordinate y y-pad)
	(cm-output-coordinate x x-pad))
    (device-write-string (tty-device-cm-string3 device))))

;;; CM-OUTPUT-COORDINATE outputs the coordinate with respect to the pad.  If
;;; there is a pad, then the coordinate needs to be sent as digit-char's (for
;;; each digit in the coordinate), and if there is no pad, the coordinate needs
;;; to be converted into a character.  Using CODE-CHAR here is not really
;;; portable.  With a pad, the coordinate buffer is filled from the end as we
;;; truncate the coordinate by 10, generating ones digits.
;;;
(defconstant cm-coordinate-buffer-len 5)
(defvar *cm-coordinate-buffer* (make-string cm-coordinate-buffer-len))
;;;
(defun cm-output-coordinate (coordinate pad)
  (cond (pad
	 (let ((i (1- cm-coordinate-buffer-len)))
	   (loop
	     (when (= i -1) (error "Terminal has too many lines!"))
	     (multiple-value-bind (tens ones)
				  (truncate coordinate 10)
	       (setf (schar *cm-coordinate-buffer* i) (digit-char ones))
	       (when (zerop tens)
		 (dotimes (n (- pad (- cm-coordinate-buffer-len i)))
		   (decf i)
		   (setf (schar *cm-coordinate-buffer* i) #\0))
		 (device-write-string *cm-coordinate-buffer* i
				      cm-coordinate-buffer-len)
		 (return))
	       (decf i)
	       (setf coordinate tens)))))
	(t (tty-write-char (code-char coordinate)))))


;;; Writing strings (TTY-DEVICE-DISPLAY-STRING functions)

;;; DISPLAY-STRING is used to put a string at (x,y) on the device.
;;; 
(defun display-string (hunk x y string font-info
			    &optional (start 0) (end (strlen string)))
  (declare (fixnum x y start end))
  (update-cursor hunk x y)
  ;; Ignore font info for chars before the start of the string.
  (loop
    (if (or (null font-info)
	    (< start (cddar font-info)))
	(return)
	(pop font-info)))
  (let ((posn start))
    (dolist (next-font font-info)
      (let ((font (car next-font))
	    (start (cadr next-font))
	    (stop (cddr next-font)))
	(when (<= end start)
	  (return))
	(when (< posn start)
	  (device-write-string string posn start)
	  (setf posn start))
	(let ((new-posn (min stop end))
	      (font-strings (aref *tty-font-strings* font)))
	  (unwind-protect
	      (progn
		(device-write-string (car font-strings))
		(device-write-string string posn new-posn))
	    (device-write-string (cdr font-strings)))
	  (setf posn new-posn))))
    (when (< posn end)
      (device-write-string string posn end)))
  (setf (tty-device-cursor-x (device-hunk-device hunk))
	(the fixnum (+ x (the fixnum (- end start))))))

;;; DISPLAY-STRING-CHECKING-UNDERLINES is used for terminals that special
;;; case underlines doing an overstrike when they don't otherwise overstrike.
;;; Note: we do not know in this code whether the terminal can backspace (or
;;; what the sequence is), whether the terminal has insert-mode, or whether
;;; the terminal has delete-mode.
;;; 
(defun display-string-checking-underlines (hunk x y string font-info
						&optional (start 0)
						          (end (strlen string)))
  (declare (ignore font-info))
  (declare (fixnum x y start end) (simple-string string))
  (update-cursor hunk x y)
  (let ((upos (position #\_ string :test #'char= :start start :end end))
	(device (device-hunk-device hunk)))
    (if upos
	(let ((previous start)
	      (after-pos 0))
	  (declare (fixnum previous after-pos))
	  (loop (device-write-string string previous upos)
		(setf after-pos (do ((i (1+ upos) (1+ i)))
				    ((or (= i end)
					 (char/= (schar string i) #\_)) i)
				  (declare (fixnum i))))
		(let ((ulen (the fixnum (- after-pos upos)))
		      (cursor-x (the fixnum (+ x (the fixnum
						      (- after-pos start))))))
		  (declare (fixnum ulen))
		  (dotimes (i ulen) (tty-write-char #\space))
		  (setf (tty-device-cursor-x device) cursor-x)
		  (update-cursor hunk upos y)
		  (dotimes (i ulen) (tty-write-char #\_))
		  (setf (tty-device-cursor-x device) cursor-x))
		(setf previous after-pos)
		(setf upos (position #\_ string :test #'char=
				     :start previous :end end))
		(unless upos
		  (device-write-string string previous end)
		  (return))))
	(device-write-string string start end))
    (setf (tty-device-cursor-x device)
	  (the fixnum (+ x (the fixnum (- end start)))))))
	   

;;; DEVICE-WRITE-STRING is used to shove a string at the terminal regardless
;;; of cursor position.
;;; 
(defun device-write-string (string &optional (start 0) (end (strlen string)))
  (declare (fixnum start end))
  (unless (= start end)
    (tty-write-string string start (the fixnum (- end start)))))


;;; Clearing lines (TTY-DEVICE-CLEAR-TO-EOL, DEVICE-CLEAR-LINES, and
;;; TTY-DEVICE-CLEAR-TO-EOW functions.)

(defun clear-to-eol (hunk x y)
  (update-cursor hunk x y)
  (device-write-string
   (tty-device-clear-to-eol-string (device-hunk-device hunk))))

(defun space-to-eol (hunk x y)
  (declare (fixnum x))
  (update-cursor hunk x y)
  (let* ((device (device-hunk-device hunk))
	 (num (- (the fixnum (tty-device-columns device))
		 x)))
    (declare (fixnum num))
    (dotimes (i num) (tty-write-char #\space))
    (setf (tty-device-cursor-x device) (+ x num))))

(defun clear-lines (hunk x y n)
  (let* ((device (device-hunk-device hunk))
	 (clear-to-eol (tty-device-clear-to-eol device)))
    (funcall clear-to-eol hunk x y)
    (do ((y (1+ y) (1+ y))
	 (count (1- n) (1- count)))
	((zerop count)
	 (setf (tty-device-cursor-x device) 0)
	 (setf (tty-device-cursor-y device) (1- y)))
      (declare (fixnum count y))
      (funcall clear-to-eol hunk 0 y))))

(defun clear-to-eow (hunk x y)
  (declare (fixnum x y))
  (funcall (tty-device-clear-lines (device-hunk-device hunk))
	   hunk x y
	   (the fixnum (- (the fixnum (tty-hunk-text-height hunk)) y))))


;;; Opening and Deleting lines (TTY-DEVICE-OPEN-LINE and TTY-DEVICE-DELETE-LINE)

(defun open-tty-line (hunk x y &optional (n 1))
  (update-cursor hunk x y)
  (dotimes (i n)
    (device-write-string (tty-device-open-line-string (device-hunk-device hunk)))))

(defun delete-tty-line (hunk x y &optional (n 1))
  (update-cursor hunk x y)
  (dotimes (i n)
    (device-write-string (tty-device-delete-line-string (device-hunk-device hunk)))))


;;; Insert and Delete modes (TTY-DEVICE-INSERT-STRING and TTY-DEVICE-DELETE-CHAR)

(defun tty-insert-string (hunk x y string
			   &optional (start 0) (end (strlen string)))
  (declare (fixnum x y start end))
  (update-cursor hunk x y)
  (let* ((device (device-hunk-device hunk))
	 (init-string (tty-device-insert-init-string device))
	 (char-init-string (tty-device-insert-char-init-string device))
	 (char-end-string (tty-device-insert-char-end-string device))
	 (end-string (tty-device-insert-end-string device)))
    (declare (type (or simple-string null) char-init-string char-end-string))
    (when init-string (device-write-string init-string))
    (if char-init-string
	(let ((cis-len (length char-init-string))
	      (ces-len (length char-end-string)))
	  (do ((i start (1+ i)))
	      ((= i end))
	    (device-write-string char-init-string 0 cis-len)
	    (tty-write-char (schar string i))
	    (when char-end-string
	      (device-write-string char-end-string 0 ces-len))))
	(device-write-string string start end))
    (when end-string (device-write-string end-string))
    (setf (tty-device-cursor-x device)
	  (the fixnum (+ x (the fixnum (- end start)))))))

(defun worth-using-insert-mode (device insert-char-num chars-saved)
  (let* ((init-string (tty-device-insert-init-string device))
	 (char-init-string (tty-device-insert-char-init-string device))
	 (char-end-string (tty-device-insert-char-end-string device))
	 (end-string (tty-device-insert-end-string device))
	 (cost 0))
    (when init-string (incf cost (length (the simple-string init-string))))
    (when char-init-string
      (incf cost (* insert-char-num (+ (length (the simple-string
						    char-init-string))
				       (if char-end-string
					   (length (the simple-string
							char-end-string))
					   0)))))
    (when end-string (incf cost (length (the simple-string end-string))))
    (< cost chars-saved)))

(defun delete-char (hunk x y &optional (n 1))
  (declare (fixnum x y n))
  (update-cursor hunk x y)
  (let* ((device (device-hunk-device hunk))
	 (init-string (tty-device-delete-init-string device))
	 (end-string (tty-device-delete-end-string device))
	 (delete-char-string (tty-device-delete-char-string device)))
    (when init-string (device-write-string init-string))
    (dotimes (i n)
      (device-write-string delete-char-string))
    (when end-string (device-write-string end-string))))

(defun worth-using-delete-mode (device delete-char-num clear-char-num)
  (declare (fixnum delete-char-num clear-char-num))
  (let ((init-string (tty-device-delete-init-string device))
	(end-string (tty-device-delete-end-string device))
	(delete-char-string (tty-device-delete-char-string device))
	(clear-to-eol-string (tty-device-clear-to-eol-string device))
	(cost 0))
    (declare (type (or simple-string null) init-string end-string
		   delete-char-string)
	     (fixnum cost))
    (when init-string (incf cost (the fixnum (length init-string))))
    (when end-string (incf cost (the fixnum (length end-string))))
    (incf cost (the fixnum
		    (* (the fixnum (length delete-char-string))
		       delete-char-num)))
    (< cost (+ delete-char-num
	       (if clear-to-eol-string
		   (length clear-to-eol-string)
		   clear-char-num)))))


;;; Standout mode (TTY-DEVICE-STANDOUT-INIT and TTY-DEVICE-STANDOUT-END)

(defun standout-init (hunk)
  (device-write-string
   (tty-device-standout-init-string (device-hunk-device hunk))))

(defun standout-end (hunk)
  (device-write-string
   (tty-device-standout-end-string (device-hunk-device hunk))))
