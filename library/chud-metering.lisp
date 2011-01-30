;;;-*-Mode: LISP; Package: (CHUD (:USE CL CCL)) -*-
;;;
;;;   Copyright (C) 2005,2008,2009 Clozure Associates and contributors
;;;   This file is part of Clozure CL.  
;;;
;;;   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with Clozure CL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with Clozure CL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

;;; Some of this is based on work done by Dan Knapp and Hamilton Link
;;; (and possibly others.)

(defpackage "CHUD"
  (:use "CL" "CCL")
  (:export "METER" "*SHARK-CONFIG-FILE*"))
  
(in-package "CHUD")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
    #-darwin-target
    (error "This code is Darwin/MacOSX-specific.")))


(defparameter *shark-session-path* nil)


(defparameter *shark-session-native-namestring* nil)

(defparameter *shark-config-file* nil "Full pathname of .cfg file to use for profiling, or NIL.")

(defun finder-open-file (namestring)
  "Open the file named by NAMESTRING, as if it was double-clicked on
in the finder"
  (run-program "/usr/bin/open" (list namestring) :output nil))

(defun ensure-shark-session-path ()
  (unless *shark-session-path*
    (multiple-value-bind (second minute hour date month year)
	(decode-universal-time (get-universal-time))
      (let* ((subdir (format nil "profiling-session-~A-~d_~d-~d-~d_~d.~d.~d"
			     (pathname-name
			      (car
			       ccl::*command-line-argument-list*))
			     (ccl::getpid)
			     month
			     date
			     year
			     hour
			     minute
			     second))
	     (dir (make-pathname :directory (append (pathname-directory (user-homedir-pathname)) (list subdir)) :defaults nil))
	     (native-name (ccl::native-untranslated-namestring dir)))
	(ensure-directories-exist dir)
	(setq *shark-session-native-namestring*
	      native-name
	      *shark-session-path* dir))))
  *shark-session-path*)


  

(defloadvar *shark-process* nil)
(defloadvar *sampling* nil)

(defvar *debug-shark-process-output* nil)




(defun terminate-shark-process ()
  (when *shark-process*
    (signal-external-process *shark-process* #$SIGUSR2))
  (setq *shark-process* nil
	*sampling* nil))

(defun toggle-sampling ()
  (if *shark-process*
    (progn
      (signal-external-process *shark-process* (if *sampling* #$SIGUSR2 #$SIGUSR1))
      (setq *sampling* (not *sampling*)))
    (warn "No active shark procsss")))

(defun enable-sampling ()
  (unless *sampling* (toggle-sampling)))

(defun disable-sampling ()
  (when *sampling* (toggle-sampling)))

(defun ensure-shark-process (reset hook)
  (when (or (null *shark-process*) reset)
    (terminate-shark-process)
    (let* ((args (list "-r" "-b" "-1" "-a" (format nil "~d" (ccl::getpid))
                       "-d" *shark-session-native-namestring*)))
      (when *shark-config-file*
	(push (ccl::native-untranslated-namestring *shark-config-file*)
	      args)
	(push "-m" args))
      (setq *shark-process*
	    (run-program "/usr/bin/shark"
			 args
			 :output :stream
			 :status-hook hook
			 :wait nil))
      (let* ((output (external-process-output-stream *shark-process*)))
	(do* ((line (read-line output nil nil) (read-line output nil nil)))
	     ((null line))
	  (when *debug-shark-process-output*
	    (format t "~&~a" line))
	  (when (search "ready." line :key #'char-downcase)
            (sleep 1)
	    (return)))))))

(defun display-shark-session-file (line)
  (let* ((last-quote (position #\' line :from-end t))
	 (first-quote (and last-quote (position #\' line :end (1- last-quote) :from-end t)))
	 (path (and first-quote  (subseq line (1+ first-quote) last-quote))))
    (when path (finder-open-file path))))
    
(defun scan-shark-process-output (p)
  (with-interrupts-enabled 
      (let* ((out (ccl::external-process-output p)))
	(do* ((line (read-line out nil nil) (read-line out nil nil)))
	     ((null line))
	  (when *debug-shark-process-output*
	    (format t "~&~a" line))
	  (when (search "Created session file:" line)
	    (display-shark-session-file line)
	    (return))))))



(defmacro meter (form &key reset debug-output)
  (let* ((hook (gensym))
	 (block (gensym))
	 (process (gensym)))
    `(block ,block
      (flet ((,hook (p)
	       (when (or (eq (external-process-status p) :exited)
			 (eq (external-process-status p) :signaled))
		 (setq *shark-process* nil
		       *sampling* nil))))
	(let* ((*debug-shark-process-output* ,debug-output))
          (ensure-shark-session-path)
	  (ensure-shark-process ,reset #',hook)
	  (unwind-protect
	       (progn
		 (enable-sampling)
		 ,form)
	    (disable-sampling)
	    (let* ((,process *shark-process*))
	      (when ,process
		(scan-shark-process-output ,process)))))))))

;;; Try to clean up after ourselves when the lisp quits.
(pushnew 'terminate-shark-process ccl::*save-exit-functions*)
