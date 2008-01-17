;;; The code walker should ideally be in ELISP-INTERNALS, however
;;; getting it there won't be trivial, so ignoring that for now.
(in-package "ELISP")

(cl:defun walk-code (form &optional lexicals)
  (cond ((null form) nil)
	((numberp form) form)
	((stringp form) form)
	((atom form) (if (member form lexicals)
			    form
			  `(elisp-value ',form)))
	(t (cl:let ((head (car form))
		    (rest (cdr form)))
	     (cond ((eq head 'lexical-let)
		    (cl:let ((bindings (append lexicals
					       (mapcar #'(lambda (x)
							   (cl:if (symbolp x)
								  x
								  (car x)))
						       (car rest))))
			     (tail (cdr rest)))
		      (cons head
			    (cons (mapcar #'(lambda (form)
					      (walk-code form lexicals))
					  (car rest))
				  (mapcar #'(lambda (form)
					      (walk-code form bindings))
					  tail)))))
		   ((eq head 'let)
		    (cons head (cons (mapcar #'(lambda (form)
					     (walk-code form lexicals))
					     (car rest))
				     (mapcar #'(lambda (form)
					     (walk-code form lexicals))
					     (cdr rest)))))
		   ((member head '(defun defmacro))
		    (cl:let ((name (car rest))
			     (new-vars
			      (cl:loop for sym in (cadr rest)
				       if (not
					   (member sym '(&optional &rest
							 &aux &key)))
				       collect sym))
			     (forms (cddr rest))
			     (vars (cadr rest)))
		      `(,head ,name ,vars
			,@(mapcar
			   #'(lambda (form)
			       (walk-code form
					  (append lexicals new-vars)))
			   forms))))
		   ((eq head 'cond)
		    (cons head
			  (cl:loop for cond-form in rest
				collect
				(cl:loop for form in cond-form
					 collect (walk-code form lexicals)))))
		   ((eq head 'quote)
		    (cons head rest))
		   ((member head '(setq setf))
		    (cons head
			  (loop for symbol in rest
				for toggle = t then (not toggle)
				if toggle
				collect symbol
				else
				collect (walk-code symbol lexicals))))
		   (t (cons head (mapcar #'(lambda (form)
					     (walk-code form lexicals))
					 rest))))))))
	  
