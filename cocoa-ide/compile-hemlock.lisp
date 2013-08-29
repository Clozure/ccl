(in-package "CCL")

(defparameter *hemlock-src-dir-pathname* "ccl:cocoa-ide;hemlock;src;")

(defparameter *hemlock-binary-dir-pathname* "ccl:cocoa-ide;hemlock;bin;openmcl;")

(defparameter *hemlock-binary-file-extension*
  (pathname-type (compile-file-pathname "foo.lisp")))

(defun hemlock-source-pathname (name)
  (make-pathname :name name
                 :type "lisp"
                 :defaults *hemlock-src-dir-pathname*))

(defun hemlock-binary-pathname (name)
  (make-pathname :name name
                 :type *hemlock-binary-file-extension*
                 :defaults *hemlock-binary-dir-pathname*))

(defun compile-and-load-hemlock-file (name &optional force)
  (let* ((source-pathname (hemlock-source-pathname name))
	 (binary-pathname (hemlock-binary-pathname name)))
    (when (or force
	      (not (probe-file binary-pathname))
	      (> (file-write-date source-pathname)
		 (file-write-date binary-pathname)))
      (compile-file source-pathname :output-file binary-pathname :verbose t))
    (load binary-pathname :verbose t)))


(defparameter *hemlock-files*
  '("package"

    "hemlock-ext"                     
	       
    "decls"                             ;early declarations of functions and stuff
	       
    "struct"
    "charmacs"
    "key-event" 
    "keysym-defs"
    "cocoa-hemlock"
    "rompsite"

    "macros"

    "views"
    "line"
    "ring"
    "vars"
    "interp"
    "syntax"
    "htext1"
    "buffer"  
    "charprops"
    "htext2"
    "htext3"
    "htext4"
    "files"
    "search1"
    "search2"
    "table"
    "modeline"
    "pop-up-stream"
    "font"
    "streams"
    "main"
    "echo"
    "echocoms"
    "command"
    "indent"
    ;; moved     "comments"
    "morecoms"
    "undo"
    "killcoms"
    "searchcoms"
    "isearchcoms"
    "filecoms"
    "doccoms"
    "fill"
    "text"
    "lispmode"
    "listener"
    "comments"
    "defsyn"
    "edit-defs"
    "register"
    "completion"
    "symbol-completion"
    "bindings"
    ))

(defun compile-hemlock (&optional force)
  (with-compilation-unit ()
    (dolist (name *hemlock-files*)
      (compile-and-load-hemlock-file name force)))
  (fasl-concatenate "ccl:cocoa-ide;hemlock"
                    (mapcar #'hemlock-binary-pathname *hemlock-files*)
                    :if-exists :supersede)
  (provide "HEMLOCK")
  )


(provide "COMPILE-HEMLOCK")
