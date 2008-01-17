(cl:in-package :cl-user)

(defpackage :asdf-install
  (:use "CL")
  (:export

   ;; Customizable variables.
   #:*proxy*
   #:*cclan-mirror*
   #:*sbcl-home* ; Deprecated.
   #:asdf-install-dirs
   #:private-asdf-install-dirs

   #:*verify-gpg-signatures*
   #:*locations*
   #:*safe-url-prefixes*
   #:*preferred-location*

   #+(or :win32 :mswindows)
   #:*cygwin-bin-directory*

   #+(or :win32 :mswindows)
   #:*cygwin-bash-command*

   ;; External entry points.   
   #:add-locations
   #+(and asdf (or :win32 :mswindows))
   #:sysdef-source-dir-search
   #:uninstall
   #:install
   ;; proxy authentication
   #:*proxy-user*
   #:*proxy-passwd*))

(defpackage :asdf-install-customize
  (:use "CL" "ASDF-INSTALL"))
