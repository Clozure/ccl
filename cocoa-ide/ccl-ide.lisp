; ccl-ide.lisp
; 17-Oct-2013 SVS
; Functional version of cocoa.lisp and cocoa-application.lisp.
; Eventually, this function should do the right thing to make an IDE even in a non-Cocoa system.

; (ccl-ide :save-app t)  is the same as the old (require :cocoa-application)
; Creates a ccl-ide environment, saves it as an app, and quits.

; (ccl-ide)  is the same as the old (require :cocoa)
; Creates a ccl-ide environment, but doesn't save it.

; (ccl-ide :save-app "ccl:foo.app;") saves the app with the name "foo"
; (ccl-ide :save-app t :init-file nil) saves an IDE app which does not automatically try to load ccl-ide-init at startup

(in-package "CCL")
#+windows-target
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :cocotron *features*))

(defvar *cocoa-ide-path* nil)
(defvar *cocoa-ide-copy-headers-p* nil)
(defvar *cocoa-ide-install-altconsole* nil)
(defvar *cocoa-ide-bundle-suffix* nil)
(defvar *cocoa-ide-frameworks* nil)
(defvar *cocoa-ide-libraries* nil)

(defun ccl-ide (&key (force-compile nil) (save-app nil) (init-file "home:ccl-ide-init"))
  (multiple-value-bind (os bits cpu) (host-platform)
    (declare (ignore os))
    (setf *cocoa-ide-path* 
          (if save-app
            (if (eq t save-app)
              (format nil "ccl:Clozure CL~a.app;" bits)
              save-app)
            (format nil "ccl:temp bundle~a.app;"
                    bits)))
    (setf *cocoa-ide-install-altconsole* save-app)
    (setf *cocoa-ide-bundle-suffix*
          (if save-app 
            (format nil "Clozure CL-~a~a" (string-downcase cpu) bits)
            (format nil "temp bundle-~a~a" (string-downcase cpu) bits)))
    (setf *cocoa-ide-frameworks*
          #+cocotron '("ccl:cocotron;Foundation.framework;" "ccl:cocotron;AppKit.framework;" "ccl:cocotron;CoreData.framework;")
          #-cocotron nil)
    (setf *cocoa-ide-libraries*
          #+cocotron '("ccl:cocotron;Foundation>.1>.0.dll" "ccl:cocotron;AppKit>.1>.0.dll" "ccl:cocotron;CoreData>.1>.0.dll")
          #-cocotron nil)
    
    (cond ((member "COCOA" *modules* :test #'string-equal)
           (create-ide-bundle *cocoa-ide-path*)
           (fake-cfbundle-path *cocoa-ide-path* "ccl:cocoa-ide;Info.plist-proto" "com.clozure"
                               *cocoa-ide-bundle-suffix*
                               *cocoa-ide-frameworks*
                               *cocoa-ide-libraries*
                               #+windows-target "ccl:cocoa-ide;ide-contents;resources;openmcl-icon.ico"))
          (t (load "ccl:cocoa-ide;defsystem.lisp")
             (load-ide force-compile)))
    
    (setf (symbol-value (intern "*CCL-IDE-INIT-FILE*" (find-package :gui))) (or init-file "")) ;; to prevent loading the user's ccl-ide-init-file
    
    (if save-app
      (build-ide *cocoa-ide-path*)
      (funcall (intern "START-COCOA-IDE" (find-package :gui))))))

; (ccl-ide :save-app t)







