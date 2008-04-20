;;;; dead letter

#+Old
(defun load-system-definition (sysfile)
  (declare (type pathname sysfile))
  #+asdf
  (when (or (string-equal "asd" (pathname-type sysfile))
            (string-equal "asdf" (pathname-type sysfile)))
    (installer-msg t "Loading system ~S via ASDF." (pathname-name sysfile))
    ;; just load the system definition
    (load sysfile)
    #+Ignore
    (asdf:operate 'asdf:load-op (pathname-name sysfile)))

  #+mk-defsystem
  (when (string-equal "system" (pathname-type sysfile))
    (installer-msg t "Loading system ~S via MK:DEFSYSTEM." (pathname-name sysfile))
    (mk:load-system (pathname-name sysfile))))

#+Old
;; from download-files-for-package
(with-open-file 
    #-(and allegro-version>= (not (version>= 8 0)))
    (o file-name :direction :output
       #+(or :clisp :digitool (and :lispworks :win32))
       :element-type
       #+(or :clisp :digitool (and :lispworks :win32))
       '(unsigned-byte 8)
       #+:sbcl #+:sbcl :external-format :latin1
       :if-exists :supersede)
    ;; for Allegro  versions  < 8.0,  the above  #+sbcl #+sbcl
    ;; will cause an error [2006/01/09:rpg]
    #+(and allegro-version>= (not (version>= 8 0)))
    (o file-name :direction :output :if-exists :supersede))