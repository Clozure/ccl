(in-package "ELISP")

(defvar load-path nil)
(defvar features nil)
(defvar *buffer-locals* (make-hash-table))
(defvar *current-buffer* nil)
(define-symbol-macro major-mode (buffer-major-mode (current-buffer)))


(cl:defun make-sparse-keymap (&optional string)
  (if string
      (list 'keymap string)
    (list 'keymap)))

(cl:defun make-keymap (&optional string)
  (if string
      (list 'keymap string (make-vector 256))
    (list 'keymap (make-vector 256))))

(cl:defun make-sparse-keymap (&optional string)
  (if string
      (list 'keymap string)
    (list 'keymap)))

(cl:defun buffer-local-p (sym)
  (multiple-value-bind (expansion expanded) (macroexpand sym)
    (declare (ignore expansion))
    expanded))

(cl:defun elisp-value (sym)
  (cl:let ((marker (gensym)))
    (multiple-value-bind (value exists)
	(gethash sym *buffer-locals*)
      (if exists
	  (hemlock::variable-value sym)
	  (eval sym)))))

(cl:defun = (a b)
  (cond ((and (characterp a) (characterp b))
	 (char= a b))
	((and (numberp a) (characterp b))
	 (cl:= a (char-code b)))
	((and (characterp a) (numberp b))
	 (cl:= (char-code a) b))
	((and (numberp a) (numberp b))
	 (cl:= a b))
	(t (error "Wrong type argument ~a" (if (or (numberp a) (characterp a))
					       b
					     a)))))

(cl:defun make-variable-buffer-local (sym)
  (make-variable-foo-local sym :buffer))

(cl:defun make-variable-foo-local (sym kind)
  "MAKE-VARIABLES-BUFFER-LOCAL
Arguments SYMBOL

Will make a variable buffer-local UNLESS it has prior special binding,
this may be a grave incompatibility with Emacs Lisp.

In a buffer where no dedicated value has been set, will use the
default-value. The default value is set with SET-DEFAULT."
  (unless (hemlock::hemlock-bound-p sym)
    (setf (gethash sym *buffer-locals*) kind)
    (defhvar sym "Variable automatically set from ELISP" :mode :kind)
    ))


;;; Troublesome? Looks like it IM -- 2003-04-05
(cl:defun set-default (sym value)
  "SET-DEFAULT
Args: SYMBOL VALUE

Will set the default value of (the buffer-local) SYMBOL to VALUE"
  (if (buffer-local-p sym)
      (setf (gethash *buffer-locals* (gethash sym *buffer-locals*)) value)
      (set sym value)))

;;; Troublesome? Looks like it IM -- 2003-04-05
(cl:defun get-default (sym)
  "GET-DEFAULT
Args: SYMBOL

Returns the default value for SYMBOL"
  (if (buffer-local-p sym)
      (gethash *buffer-locals* (gethash sym *buffer-locals*))
      (symbol-value sym)))

(cl:defmacro interactive (&rest spec)
  (declare (ignore spec))
  nil)

;;; This really should generate a glue function to handle the differences
;;; betwen emacs command calling conventions and Hemlock ccc.
;;; Basically, what we need is a layer that does all the prompting that
;;; would've been done on an interactive call in emacs. Probably simplest
;;; to just generate a lambda with the right stuff prompted for, then have
;;; that call the function proper.
(cl:defmacro defun (name args &body body)
  (cl:let ((real-args (elisp-internals:find-lambda-list-variables args))
	   (body (walk-code `(defun ,name ,args ,@body)))
	   (maybe-docstring (car body))
	   (interactive-p (member 'interactive body :key #'(lambda (x) (when (consp x) (car x))))))
    (if interactive-p
	`(prog1
	  (cl:defun ,name ,args
	    (declare (special ,@real-args))
	    ,@(cdddr body))
	  (make-command ,(string-downcase (string name))
	   ,(if (stringp maybe-docstring)
	       maybe-docstring
	       (format nil "This implements the elisp command for function ~a." (string name))) ,(elisp-internals:interactive-glue (cadr (car interactive-p)) name)))
	
	`(cl:defun ,name ,args
	  (declare (special ,@real-args))
	  ,@(cdddr body)))))

(cl:defmacro let (inits &body body)
  (cl:let ((vars (loop for var in inits
		       collect (cl:if (symbolp var) var (car var)))))
    `(cl:let ,inits
      (declare (special ,@vars))
      ,@body)))

(cl:defmacro if (test true &rest falses)
  `(cl:if ,test ,true (progn ,@falses)))

(cl:defmacro lexical-let (&rest body)
  `(cl:let ,@body ))

(cl:defmacro setq (&rest rest)
  `(cl:setf ,@rest))

(cl:defun provide (feature)
  (cl:push feature features))

(cl:defun require (feature &optional filename noerror)
  (let ((*readtable* elisp-internals:*elisp-readtable*))
    (or
     (car (member feature features))
     (loop for directory in load-path
	   if (elisp-internals:require-load directory feature filename)
	   return feature)
     (unless noerror
       (error "Cannot open file ~a." (if filename
					 filename
				       (cl:string-downcase feature)))))))

;; Done via CL:DEFUN since the code walker wreaks havoc with the loop macro.
;; Keep these together for sanity's sake
(cl:defun load-library (library-name)
  (loop for directory in load-path
	do (loop for ext in '(".el" "")
		 for name = (format nil "~a/~a~a" directory library-name ext)
		 if (cl:probe-file name)
		 do (return-from load-library
		      (let (*package* (find-package "ELISP-USER"))
			(let ((*readtable* elisp-internals:*elisp-readtable*))
			  (cl:load name)))))))

(cl:defun load-file (filename)
  (let ((*readtable* elisp-internals:*elisp-readtable*)
	(*package* (find-package "ELISP-USER")))
    (load filename)))

(make-command "load-file" "Load a file, elisp style" #'(lambda (p) (declare (ignore p)) (load-file (hemlock-internals:prompt-for-file :prompt "Load file: "))))
(make-command "load-library" "Load a library, elisp-style" #'(lambda (p) (declare (ignore p)) (load-library (hemlock-internals:prompt-for-string :prompt "Load library: "))))
;; End of things kept together

;; Unfinished, including at least *one* function taht isn't implemented
;; (and will be hell to make portably, I'm afraid)
(cl:defun expand-file-name (name &optional default-directory)
  (cl:let ((result (search "~" name)))
    (if result
      (cl:let ((name (subseq name result)))
	(if (char= (cl:aref name 1) #\/)
	    (merge-pathnames (subseq name 2) (elisp-internals:get-user-homedir))
	  (cl:let ((username (subseq name 1 (search "/" name)))
		   (directory (subseq name (1+ (search "/" name)))))
	    (merge-pathnames directory (elisp-internals:get-user-homedir username)))))
      name
      )))

(cl:defmacro while (test &body body)
  `(cl:do ()
       ((not ,test) nil)
     ,@body))

(cl:defmacro aset (array index new-element)
  `(setf (cl:aref ,array ,index) ,new-element))

(cl:defmacro assq (key list)
  `(cl:assoc ,key ,list :test 'eq))

(cl:defmacro assoc (key list)
  `(cl:assoc ,key ,list :test 'equal))

(cl:defun % (x y)
  "Return the remainder of X divided by Y, both X and Y must be integers"
  (declare (integer x y))
  (mod x y))

(cl:defun car-safe (object)
  (when (consp object)
    (car object)))

(cl:defun cdr-safe (object)
  (when (consp object)
    (cdr object)))

(cl:defun car-less-than-car (a b)
  (< (car a) (car b)))

(cl:defun bool-vector-p (array)
  (and (simple-vector-p array)
       (eq (element-type array) 'bit)))

(cl:defun aref (vector &rest indices)
  (if (bool-vector-p vector)
      (= 1 (apply #'cl:aref vector indices))
    (apply #'cl:aref vector indices)))

(cl:defun make-bool-vector (length init)
  (make-array (list length) :element-type bit :initial-element (if init 1 0)))

(cl:defun delq (element list)
  (cl:delete element list :test #'cl:eq))

(cl:defun fset (symbol function)
  (cl:setf (symbol-function symbol) function))

(cl:defmacro autoload (function file &optional docstring interactive type)
  (cond ((and docstring interactive)
	 `(defun ,function (&rest args)
	    ,docstring
	    (interactive)
	    (unless (gethash ',function elisp-internals::*autoloads* nil)
	      (setf (gethash ',function elisp-internals::*autoloads*) t)
	      (load ,file))
	    (apply ',function args)))
	((and docstring (not interactive))
	 `(defun ,function (&rest args)
	    ,docstring
	    (unless (gethash ',function elisp-internals::*autoloads* nil)
	      (setf (gethash ',function elisp-internals::*autoloads*) t)
	      (load ,file))
	    (apply ',function args)))
	(interactive
	 `(defun ,function (&rest args)
	    (interactive)
	    (unless (gethash ',function elisp-internals::*autoloads* nil)
	      (setf (gethash ',function elisp-internals::*autoloads*) t)
	      (load ,file))
	    (apply ',function args)))
	(t
	 `(defun ,function (&rest args)
	    (unless (gethash ',function elisp-internals::*autoloads* nil)
	      (setf (gethash ',function elisp-internals::*autoloads*) t)
	      (load ,file))
	    (apply ',function args)))))
