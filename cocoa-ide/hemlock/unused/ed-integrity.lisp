;;; -*- Package: hemlock; Log: hemlock.log; Mode: Lisp -*-
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
;;; This stuff can be used for testing tty redisplay.  There are four
;;; commands that, given "Setup Tty Buffer", that test
;;; HI::COMPUTE-TTY-CHANGES: "Two Deletes", "Two Inserts", "One Delete One
;;; Insert", and "One Insert One Delete.  Each can be called with an
;;; argument to generate a grand total of eight screen permutations.
;;; "Setup Tty Buffer" numbers the lines of the main window 0 through 19
;;; inclusively.
;;; 
;;; "Setup for Debugging" and "Cleanup for Debugging" were helpful in
;;; conjunction with some alternate versions of COMPUTE-TTY-CHANGES and
;;; TTY-SMART-WINDOW-REDISPLAY.  When something went wrong with on

(in-package "ED")


(declaim (special hemlock-internals::*debugging-tty-redisplay*
		  hemlock-internals::*testing-delete-queue*
		  hemlock-internals::*testing-insert-queue*
		  hemlock-internals::*testing-moved*
		  hemlock-internals::*testing-writes*))


(defcommand "Setup Tty Buffer" (p)
  "Clear buffer and insert numbering strings 0..19."
  "Clear buffer and insert numbering strings 0..19."
  (declare (ignore p))
  (delete-region (buffer-region (current-buffer)))
  (let ((point (current-point)))
    (dotimes (i 20)
      (insert-string point (prin1-to-string i))
      (insert-character point #\newline))
    (buffer-start point)))

(defcommand "Setup for Debugging" (p)
  "Set *debugging-tty-redisplay* to t, and some other stuff to nil."
  "Set *debugging-tty-redisplay* to t, and some other stuff to nil."
  (declare (ignore p))
  (setf hi::*debugging-tty-redisplay* t)
  (setf hi::*testing-delete-queue* nil)
  (setf hi::*testing-insert-queue* nil)
  (setf hi::*testing-moved* nil)
  (setf hi::*testing-writes* nil))

(defcommand "Cleanup for Debugging" (p)
  "Set *debugging-tty-redisplay* to nil."
  "Set *debugging-tty-redisplay* to nil."
  (declare (ignore p))
  (setf hi::*debugging-tty-redisplay* nil))

;;; Given "Setup Tty Buffer", deletes lines numbered 3, 4, 5, 10, 11, 12,
;;; 13, and 14.  With argument, 3..7 and 12..14.
;;; 
(defcommand "Two Deletes" (p)
  "At line 3, delete 3 lines.  At line 3+4, delete 5 lines.
   With an argument, switch the number deleted."
  "At line 3, delete 3 lines.  At line 3+4, delete 5 lines.
   With an argument, switch the number deleted."
  (multiple-value-bind (dnum1 dnum2)
		       (if p (values 5 3) (values 3 5))
    (let ((point (current-point)))
      (move-mark point (window-display-start (current-window)))
      (line-offset point 3)
      (with-mark ((end point :left-inserting))
	(line-offset end dnum1)
	(delete-region (region point end))
 	(line-offset point 4)
	(line-offset (move-mark end point) dnum2)
	(delete-region (region point end))))))


;;; Given "Setup Tty Buffer", opens two blank lines between 2 and 3, and
;;; opens four blank lines between 6 and 7, leaving line numbered 13 at
;;; the bottom.  With argument, four lines between 2 and 3, two lines
;;; between 6 and 7, and line 13 at the bottom of the window.
;;; 
(defcommand "Two Inserts" (p)
  "At line 3, open 2 lines.  At line 3+2+4, open 4 lines.
   With an argument, switch the number opened."
  "At line 3, open 2 lines.  At line 3+2+4, open 4 lines.
   With an argument, switch the number opened."
  (multiple-value-bind (onum1 onum2)
		       (if p (values 4 2) (values 2 4))
    (let ((point (current-point)))
      (move-mark point (window-display-start (current-window)))
      (line-offset point 3)
      (dotimes (i onum1)
	(insert-character point #\newline))
      (line-offset point 4)
      (dotimes (i onum2)
	(insert-character point #\newline)))))


;;; Given "Setup Tty Buffer", deletes lines numbered 3, 4, and 5, and
;;; opens five lines between lines numbered 9 and 10, leaving line numbered
;;; 17 on the bottom.  With an argument, deletes lines numbered 3, 4, 5, 6,
;;; and 7, and opens three lines between 11 and 12, creating two blank lines
;;; at the end of the screen.
;;; 
(defcommand "One Delete One Insert" (p)
  "At line 3, delete 3 lines.  At line 3+4, open 5 lines.
   With an argument, switch the number of lines affected."
  "At line 3, delete 3 lines.  At line 3+4, open 5 lines.
   With an argument, switch the number of lines affected."
  (multiple-value-bind (dnum onum)
		       (if p (values 5 3) (values 3 5))
    (let ((point (current-point)))
      (move-mark point (window-display-start (current-window)))
      (line-offset point 3)
      (with-mark ((end point :left-inserting))
	(line-offset end dnum)
	(delete-region (region point end))
 	(line-offset point 4)
	(dotimes (i onum)
	  (insert-character point #\newline))))))

;;; Given "Setup Tty Buffer", opens three blank lines between lines numbered
;;; 2 and 3, and deletes lines numbered 7, 8, 9, 10, and 11, leaving two
;;; blank lines at the bottom of the window.  With an argument, opens five
;;; blank lines between lines numbered 2 and 3, and deletes lines 7, 8, and
;;; 9, leaving line 17 at the bottom of the window.
;;; 
(defcommand "One Insert One Delete" (p)
  "At line 3, open 3 lines.  At line 3+3+4, delete 5 lines.
   With an argument, switch the number of lines affected."
  "At line 3, open 3 lines.  At line 3+3+4, delete 5 lines.
   With an argument, switch the number of lines affected."
  (multiple-value-bind (onum dnum)
		       (if p (values 5 3) (values 3 5))
    (let ((point (current-point)))
      (move-mark point (window-display-start (current-window)))
      (line-offset point 3)
      (dotimes (i onum)
	(insert-character point #\newline))
      (line-offset point 4)
      (with-mark ((end point :left-inserting))
	(line-offset end dnum)
	(delete-region (region point end))))))


;;; This could be thrown away, but I'll leave it here.  When I was testing
;;; the problem of generating EQ screen image lines due to faulty
;;; COMPUTE-TTY-CHANGES, this was a convenient command to get the editor
;;; back under control.
;;; 
(defcommand "Fix Screen Image Lines" (p)
  ""
  ""
  (declare (ignore p))
  (let* ((device (hi::device-hunk-device (hi::window-hunk (current-window))))
	 (lines (hi::tty-device-lines device))
	 (columns (hi::tty-device-columns device))
	 (screen-image (hi::tty-device-screen-image device)))
    (dotimes (i lines)
      (setf (svref screen-image i) (hi::make-si-line columns)))))
