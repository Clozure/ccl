(in-package "ELISP-INTERNALS")

(defvar *my-symbols* nil)
(defvar *cl-symbols* nil)
(defvar *cl-kluge-symbols* nil)
(defvar *autoloads* (make-hash-table))

(cl:defun find-lambda-list-variables (list)
  (loop for elem in list
	if (and (symbolp elem)
		(not (member elem '(&optional &rest))))
	collect elem))

(cl:defun generate-cl-package ()
  (when (and (null *my-symbols*)
	     (null *cl-symbols*)
	     (null *cl-kluge-symbols*))
    (setf *my-symbols* (make-hash-table :test 'equal))
    (loop for sym being the present-symbols of (find-package "ELISP")
	  do (cl:let ((name (symbol-name sym)))
	       (setf (gethash name *my-symbols*) name)))
    (setf *cl-kluge-symbols*
	  (loop for sym being the external-symbol
		of (find-package "COMMON-LISP")
		collect sym))
    (setf *cl-symbols*
	  (loop for sym in *cl-kluge-symbols*
		when (and (not (gethash (symbol-name sym) *my-symbols*))
			  (fboundp sym))
		collect (symbol-name sym)))
    (cl:let ((rv (with-output-to-string (s)
		   (format s "(in-package \"ELISP\")~%")
		   (loop for symname in *cl-symbols*
			 do
			 (format s "(cl:defmacro cl-~a (&rest args)~%`(cl:~a ,@args))~%~%~%" symname symname)
			 finally (format s "(export '~a (find-package \"ELISP\"))~%" *cl-kluge-symbols*)))))
      (with-input-from-string (stream rv)
	(load stream)))))

(cl:defun require-load (directory feature filename)
  (if filename
      (cl:let ((fname (format nil "~a/~a" directory filename)))
	(when (cl:probe-file fname)
	  (cl:let ((*package* (cl:find-package "ELISP-USER")))
	    (load fname)
	    (cl:if (member feature elisp::features)
		   feature)))) 
      (cl:let ((fname-1
		(format nil "~a.el" (cl:string-downcase feature)))
	       (fname-2
		(format nil "~a" (cl:string-downcase feature))))
	(or (require-load directory feature fname-1)
	    (require-load directory feature fname-2)))))

;;; Almost there!
;;; Basic thought: "generate a lambda expression that acts as a shim"
;;; NB: Does not handle "*" (read-only buffer signals error) or
;;; "@" (magic find-window-specifying--set-window indicator)
(cl:defun interactive-glue (initform function)
  (if initform	  
      (cl:let ((args (cl:with-input-from-string (s initform)
			(cl:loop for l = (cl:read-line s nil nil)
				 while l collect l))))
	      (multiple-value-bind (types prompt)
		  (cl:loop for l in args
			   collect (aref l 0) into type
			   collect (subseq l 1) into prompt
			   finally (return (values type prompt)))
		`(lambda (p)
		   (funcall #',function
			    ,@(cl:loop for type in types
				       for pr in prompt
				       for extracollect = nil
				       collect
				       (case type
					 (#\a ;; unimplemented -- function
					  )
					 (#\b ;; existing buffer
					  `(hemlock-internals:prompt-for-buffer
					    :prompt :pr
					    :must-exist nil))
					 (#\B	; unimplemented -- buffer name
					; Note, this may need a wrapper to
					; coerce stuff to buffers
					  `(hemlock-internals:prompt-for-buffer
					    :prompt :pr
					    :must-exist nil))
					 (#\c ;; unimplemented -- character
					  )
					 (#\d '(hemlock-internals::current-point))
					 (#\D ;; unimplemented -- directory name
					  )
					 (#\e ;; unimplemented -- event
					  )
					 (#\f ;; existing file
					  `(hemlock-internals:prompt-for-file
					    :prompt ,pr
					    :must-exist t))
					 (#\F ;; file name
					  `(hemlock-internals:prompt-for-file
					    :prompt ,pr
					    :must-exist nil))
					 (#\i nil)
					 (#\k ;; unimplemented -- key sequence
					  )
					 (#\K ;; unimplemented -- key sequence
					  )
					 (#\m '(hemlock::current-mark))
					 (#\M ;; any string
					  `(hemlock-internals:prompt-for-string
					    :prompt ,pr))
					 (#\n ;; number read
					  `(hemlock-internals:prompt-for-integer
					    :prompt ,pr))
					 (#\N ;; raw prefix or #\n
					  `(cl:if p
						  p
						  (hemlock-internals:prompt-for-integer
						   :prompt ,pr)))
					 (#\p ;; raw prefix as number
					  '(cl:if p p 0))
					 (#\P 'p)
					 (#\r
					  (setf extracollect
						'(cl:let ((mark (hemlock::current-mark))
							  (point (hemlock-internals::current-point)))
							 (if (<= (hemlock-internals::mark-charpos mark)
								 (hemlock-internals::mark-charpos point))
							     point
							   mark)))
					  '(cl:let ((mark (hemlock::current-mark))
						    (point (hemlock-internals::current-point)))
						   (if (<= (hemlock-internals::mark-charpos mark)
							   (hemlock-internals::mark-charpos point))
						       mark
						     point)))
					 (#\s ; any string
					  `(hemlock-internals:prompt-for-string
					    :prompt ,pr))
					 (#\S ; any symbol
					  `(intern (hemlock-internals:prompt-for-string
						    :prompt ,pr)
						   *package*))
					 (#\v ; variable name
					  `(hemlock-internals:prompt-for-variable
					    :prompt ,pr)
					  )
					 (#\x ; lisp expr read but not eval
					  `(hemlock-internals:prompt-for-expression
					    :prompt ,pr))
					 (#\X ; lisp expr, read and evalled
					  `(eval (hemlock-internals:prompt-for-expression
						  :prompt ,pr))
					  ))
				       if extracollect
				       collect extracollect
				       )))))
    `(lambda (arg) (declare (ignore arg)) (,function))))
  
(defun get-user-homedir (&optional username)
  (unless username
    (user-homedir-pathname)))
