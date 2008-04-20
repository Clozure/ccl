(cl:in-package :cl-user)

(defpackage #:asdf-install
  (:use #:common-lisp)
  
  #+asdf
  (:import-from #:asdf #:*defined-systems*)
  (:export

   ;; Customizable variables.
   #:*shell-path*
   #:*proxy*
   #:*cclan-mirror*
   #:asdf-install-dirs
   #:private-asdf-install-dirs
   #:*tar-extractors*

   #:*shell-search-paths*
   #:*verify-gpg-signatures*
   #:*locations*
   #:*safe-url-prefixes*
   #:*preferred-location*
   #:*temporary-directory*
   
   ;; External entry points.   
   #:add-locations
   #:add-registry-location
   #:uninstall
   #:install
   #:asdf-install-version

   #+(and asdf (or :win32 :mswindows))
   #:sysdef-source-dir-search   
   
   ;; proxy authentication
   #:*proxy-user*
   #:*proxy-passwd*
   
   ;; conditions
   #:download-error
   #:signature-error
   #:gpg-error
   #:gpg-shell-error
   #:key-not-found
   #:key-not-trusted
   #:author-not-trusted
   #:installation-abort

   ;; restarts
   #:install-anyways
   )
  
  #+(or :win32 :mswindows)
  (:export
   #:*cygwin-bin-directory*
   #:*cygwin-bash-command*))

(defpackage #:asdf-install-customize
  (:use #:common-lisp #:asdf-install))
