;;; -*- Log: hemlock.log; Package: Hemlock-Internals -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.

(in-package :hemlock-internals)


;;;; Modelines-field structure support.

(defun print-modeline-field (obj stream ignore)
  (declare (ignore ignore))
  (write-string "#<Hemlock Modeline-field " stream)
  (prin1 (modeline-field-%name obj) stream)
  (write-string ">" stream))

(defun print-modeline-field-info (obj stream ignore)
  (declare (ignore ignore))
  (write-string "#<Hemlock Modeline-field-info " stream)
  (prin1 (modeline-field-%name (ml-field-info-field obj)) stream)
  (write-string ">" stream))


(defvar *modeline-field-names* (make-hash-table))

(defun make-modeline-field (&key name width function)
  "Returns a modeline-field object."
  (unless (or (eq width nil) (and (integerp width) (plusp width)))
    (error "Width must be nil or a positive integer."))
  (when (gethash name *modeline-field-names*)
    (with-simple-restart (continue
			  "Use the new definition for this modeline field.")
      (error "Modeline field ~S already exists."
	     (gethash name *modeline-field-names*))))
  (setf (gethash name *modeline-field-names*)
	(%make-modeline-field name function width)))

(defun modeline-field (name)
  "Returns the modeline-field object named name.  If none exists, return nil."
  (gethash name *modeline-field-names*))


(declaim (inline modeline-field-name modeline-field-width modeline-field-function))

(defun modeline-field-name (ml-field)
  "Returns the name of a modeline field object."
  (modeline-field-%name ml-field))

(defun %set-modeline-field-name (ml-field name)
  (check-type ml-field modeline-field)
  (when (gethash name *modeline-field-names*)
    (error "Modeline field ~S already exists."
	   (gethash name *modeline-field-names*)))
  (remhash (modeline-field-%name ml-field) *modeline-field-names*)
  (setf (modeline-field-%name ml-field) name)
  (setf (gethash name *modeline-field-names*) ml-field))

(defun modeline-field-width (ml-field)
  "Returns the width of a modeline field."
  (modeline-field-%width ml-field))

(declaim (special *buffer-list*))

(defun modeline-field-function (ml-field)
  "Returns the function of a modeline field object.  It returns a string."
  (modeline-field-%function ml-field))


;;;; Default modeline and update hooks.

(make-modeline-field :name :hemlock-literal :width 8
		     :function #'(lambda (buffer)
				   "Returns \"Hemlock \"."
				   (declare (ignore buffer))
				   "Hemlock "))

(make-modeline-field
 :name :external-format
 :function #'(lambda (buffer)
	       "Returns an indication of buffer's external-format, iff it's
other than :DEFAULT"
	       (let* ((line-termination-string
                       (case (buffer-line-termination buffer)
                         ((:lf nil))
                         ((:cr) "CR")
                         ((:crlf) "CRLF")))
                      (encoding-name (or (hemlock-ext:buffer-encoding-name buffer)
					 "Default")))
                 (format nil "[~a~@[ ~a~]] "
                         encoding-name line-termination-string))))


(make-modeline-field
 :name :package
 :function #'(lambda (buffer)
	       "Returns the value of buffer's \"Current Package\" followed
		by a colon and two spaces, or a string with one space."
	       (if (hemlock-bound-p 'hemlock::current-package :buffer buffer)
		   (let ((val (variable-value 'hemlock::current-package
					      :buffer buffer)))
		     (if (stringp val)
                       (if (find-package val)
			 (format nil "~A:  " val)
                         (format nil "?~A?:  " val))
                       " "))
		   " ")))

(make-modeline-field
 :name :modes
 :function #'(lambda (buffer)
	       "Returns buffer's modes followed by one space."
               (let* ((m ()))
                 (dolist (mode (buffer-minor-mode-objects buffer))
                   (unless (mode-object-hidden mode)
                     (push (mode-object-name mode) m)))
                 (format nil "~A  " (cons (buffer-major-mode buffer)
                                          (nreverse m))))))

(make-modeline-field
 :name :modifiedp
 :function #'(lambda (buffer)
	       "Returns \"* \" if buffer is modified, or \"  \"."
	       (let ((modifiedp (buffer-modified buffer)))
		 (if modifiedp
		     "* "
		     "  "))))

(make-modeline-field
 :name :buffer-name
 :function #'(lambda (buffer)
	       "Returns buffer's name followed by a colon and a space if the
		name is not derived from the buffer's pathname, or the empty
		string."
	       (let ((pn (buffer-pathname buffer))
		     (name (buffer-name buffer)))
		 (cond ((not pn)
			(format nil "~A: " name))
		       ((string/= (hemlock::pathname-to-buffer-name pn) name)
			(format nil "~A: " name))
		       (t "")))))

(make-modeline-field
 :name :completion :width 40
 :function #'(lambda (buffer)
               (declare (special hemlock::*completion-mode-possibility*))
	       (declare (ignore buffer))
	       hemlock::*completion-mode-possibility*))




;;; MAXIMUM-MODELINE-PATHNAME-LENGTH-HOOK is called whenever "Maximum Modeline
;;; Pathname Length" is set.
;;;
(defun maximum-modeline-pathname-length-hook (name kind where new-value)
  (declare (ignore name new-value))
  (if (eq kind :buffer)
    (note-modeline-change where)
    (dolist (buffer *buffer-list*)
      (when (buffer-modeline-field-p buffer :buffer-pathname)
	(note-modeline-change buffer)))))

(defun buffer-pathname-ml-field-fun (buffer)
  "Returns the namestring of buffer's pathname if there is one.  When
   \"Maximum Modeline Pathname Length\" is set, and the namestring is too long,
   return a truncated namestring chopping off leading directory specifications."
  (let ((pn (buffer-pathname buffer)))
    (if pn
	(let* ((name (namestring pn))
	       (length (length name))
	       ;; Prefer a buffer local value over the global one.
	       ;; Because variables don't work right, blow off looking for
	       ;; a value in the buffer's modes.  In the future this will
	       ;; be able to get the "current" value as if buffer were current.
	       (max (if (hemlock-bound-p 'hemlock::maximum-modeline-pathname-length
					  :buffer buffer)
			 (variable-value 'hemlock::maximum-modeline-pathname-length
					 :buffer buffer)
			 (variable-value 'hemlock::maximum-modeline-pathname-length
					 :global))))
	  (declare (simple-string name))
	  (if (or (not max) (<= length max))
	      name
	      (let* ((extra-chars (+ (- length max) 3))
		     (slash (or (position #\/ name :start extra-chars)
				;; If no slash, then file-namestring is very
				;; long, and we should include all of it:
				(position #\/ name :from-end t
					  :end extra-chars))))
		(if slash
		    (concatenate 'simple-string "..." (subseq name slash))
		    name))))
	"")))



(make-modeline-field
 :name :buffer-pathname
 :function 'buffer-pathname-ml-field-fun)



(make-modeline-field
 :name :process-info
 :function #'(lambda (buffer)
               (hemlock-ext:buffer-process-description buffer)))

(defparameter *default-modeline-fields*
  (list (modeline-field :modifiedp) ;(modeline-field :hemlock-literal)
	(modeline-field :external-format)
	(modeline-field :package)
	(modeline-field :modes))
  "This is the default value for \"Default Modeline Fields\".")

(defun %init-mode-redisplay ()
  (add-hook hemlock::buffer-major-mode-hook 'note-modeline-change)
  (add-hook hemlock::buffer-minor-mode-hook 'note-modeline-change)
  (add-hook hemlock::buffer-name-hook 'note-modeline-change)
  (add-hook hemlock::buffer-pathname-hook 'note-modeline-change)
  ;; (SETF (BUFFER-MODIFIED ...)) handles updating the modeline;
  ;; it only wants to do so if the buffer's modified state changes.
;  (add-hook hemlock::buffer-modified-hook 'note-modeline-change)
)

(defun note-modeline-change (buffer &rest more)
  (declare (ignore more)) ;; used as hooks some of which pass more info
  (hemlock-ext:invalidate-modeline buffer))

;; Public version
(defun update-modeline-fields (buffer)
  (note-modeline-change buffer))
