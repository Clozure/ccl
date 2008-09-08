;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   This file is part of OpenMCL.  
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

;; L1-boot.lisp

(in-package "CCL")

(defparameter *gensym-counter* 0 "counter for generating unique GENSYM symbols")

(defparameter *inhibit-greeting* nil)

;the below 3 variables are expected to be redefined in the user's init file
(defparameter *short-site-name* nil)
(defparameter *long-site-name* nil)
#|
(defparameter *machine-instance* nil)
|#

(defun lisp-implementation-type ()
  #+clozure-common-lisp "Clozure Common Lisp"
  #-clozure-common-lisp "OpenMCL")


(defparameter *platform-os-names*
  `((,platform-os-vxworks . :vxwork)
    (,platform-os-linux . :linux)
    (,platform-os-solaris . :solaris)
    (,platform-os-darwin . :darwin)
    (,platform-os-freebsd . :freebsd)
    (,platform-os-windows . :windows)))

(defparameter *platform-cpu-names*
  `((,platform-cpu-ppc . :ppc)
    (,platform-cpu-sparc . :sparc)
    (,platform-cpu-x86 . :x86)))

(defun host-platform ()
  (let* ((pf (%get-kernel-global 'host-platform)))
    (values
     (or (cdr (assoc (logand pf platform-os-mask)
                     *platform-os-names*))
         :unknown)
     (if (logtest pf platform-word-size-mask)
       64
       32)
     (or (cdr (assoc (logand pf platform-cpu-mask)
                     *platform-cpu-names*))
         :unknown))))


(defun platform-description ()
  (multiple-value-bind (os bits cpu) (host-platform)
    (format nil "~a~a~d" (string-capitalize os) cpu bits)))

(defun lisp-implementation-version ()
  (%str-cat "Version " (format nil *openmcl-version* (platform-description))))




(defun replace-base-translation (host-dir new-base-dir)
  (let* ((host (pathname-host host-dir))
         (device (pathname-device new-base-dir))
         (host-dir (full-pathname host-dir))
         (trans (logical-pathname-translations host))
         (host-wild (merge-pathnames "**/*.*" host-dir)))
    (setq host-dir (pathname-directory host-dir))
    (setq new-base-dir (pathname-directory new-base-dir))
    (setf 
     (logical-pathname-translations host)
     (mapcar
      #'(lambda (pair)
          (let ((rhs (cadr pair)))
            (if (and (physical-pathname-p rhs)
                     (pathname-match-p rhs host-wild))
              (list (car pair)
                    (merge-pathnames 
                     (make-pathname 
                      :defaults nil
                      :device device
                      :directory (append new-base-dir
                                         (nthcdr (length host-dir) 
                                                 (pathname-directory rhs))))
                     rhs))
              pair)))
      trans))))

(defun set-ccl-directory (path)
  (replace-base-translation "ccl:" (translate-logical-pathname path)))




; only do these if exist
(defun init-logical-directories ()
  (replace-base-translation "home:"  (user-homedir-pathname))
  (replace-base-translation "ccl:" (ccl-directory)))

(push #'init-logical-directories *lisp-system-pointer-functions*)


(catch :toplevel
  (setq *loading-file-source-file* nil)  ;Reset from last %fasload...
  (init-logical-directories)
  )






