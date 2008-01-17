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
;;; Hemlock File manipulation functions.
;;; Written by Skef Wholey, Horribly Hacked by Rob MacLachlan.
;;; Unhacked by Gilbert Baumann.
;;;

(in-package :hemlock-internals)


;;;; Utility functions.

;; FIND-CHAR-FROM-SAP was here, deleted --GB


;;; Read-File:

(defun read-file (pathname mark)
  "Inserts the contents of the file named by Pathname at the Mark."
  (with-mark ((mark mark :left-inserting))
    (let* ((first-line (mark-line mark))
           (buffer (line-%buffer first-line)))
      (modifying-buffer buffer)
      (cocoa-read-file pathname mark buffer))))
      



;;; Write-File:

(defun write-file (region pathname &key append
			  (keep-backup (value hemlock::keep-backup-files))
			  access)
  "Writes the characters in region to the file named by pathname.  This writes
   region using a stream opened with :if-exists :rename-and-delete, unless
   either append or keep-backup is supplied.  If append is supplied, this
   writes the file opened with :if-exists :append.  If keep-backup is supplied,
   this writes the file opened with :if-exists :rename.  This signals an error
   if both append and keep-backup are supplied.  Access is an implementation
   dependent value that is suitable for setting pathname's access or protection
   bits."
  (declare (ignorable access))
  (let ((if-exists-action (cond ((and keep-backup append)
				 (error "Cannot supply non-nil values for ~
				         both keep-backup and append."))
				(keep-backup :rename)
				(append :append)
				(t :rename-and-delete))))
    (with-open-file (file pathname :direction :output
			  :element-type 'base-char
			  :if-exists if-exists-action)
      (close-line)
      (fast-write-file region file))
    ;; ### access is always ignored
    #+NIL
    (when access
      (multiple-value-bind
	  (winp code)
	  ;; Must do a TRUENAME in case the file has never been written.
	  ;; It may have Common Lisp syntax that Unix can't handle.
	  ;; If this is ever moved to the beginning of this function to use
	  ;; Unix CREAT to create the file protected initially, they TRUENAME
	  ;; will signal an error, and LISP::PREDICT-NAME will have to be used.
	  (unix:unix-chmod (namestring (truename pathname)) access)
	(unless winp
	  (error "Could not set access code: ~S"
		 (unix:get-unix-error-msg code)))))))

(defun fast-write-file (region file)
  (let* ((start (region-start region))
	 (start-line (mark-line start))
	 (start-charpos (mark-charpos start))
	 (end (region-end region))
	 (end-line (mark-line end))
	 (end-charpos (mark-charpos end)))
    (if (eq start-line end-line)
        ;; just one line (fragment)
        (write-string (line-chars start-line) file
                      :start start-charpos :end end-charpos)
        ;; multiple lines
        (let* ((first-length (- (line-length start-line) start-charpos))
               (length (+ first-length end-charpos 1)))
          ;; count number of octets to be written
          (do ((line (line-next start-line) (line-next line)))
              ((eq line end-line))
            (incf length (1+ (line-length line))))
          ;;
          (macrolet ((chars (line)
                       `(line-%chars ,line)))
            (write-sequence (chars start-line) file :start start-charpos :end (+ start-charpos first-length))
            (write-char #\newline file)
            (let ((offset (1+ first-length)))
              (do ((line (line-next start-line)
                         (line-next line)))
                  ((eq line end-line))
                (let ((end (+ offset (line-length line))))
                  (write-sequence (chars line) file :start 0 :end (- end offset))
                  (write-char #\newline file)      
                  (setf offset (1+ end))))
              (unless (zerop end-charpos)
                (write-sequence (chars end-line) file :start 0 :end end-charpos))))))))
