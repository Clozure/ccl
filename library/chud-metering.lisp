;;;-*-Mode: LISP; Package: (CHUD (:USE CL CCL)) -*-
;;;
;;; Copyright 2005-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

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
  (format t "~&;;; Waiting for shark to process samples ...")
  (with-interrupts-enabled 
      (let* ((out (ccl::external-process-output p)))
	(do* ((line (read-line out nil nil) (read-line out nil nil)))
	     ((null line))
	  (when *debug-shark-process-output*
	    (format t "~&~a" line))
	  (when (search "Created session file:" line)
            (format t "done.~&")
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
