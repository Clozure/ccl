;;; -*-  Lisp -*-

;;; Portatble ASDF-Install is based on Dan Barlow's ASDF-Install 
;; (see the file COPYRIGHT for details). It is currently maintained
;; by Gary King <gwking@metabang.com>.

(defpackage #:asdf-install-system 
  (:use #:cl #:asdf))

(in-package #:asdf-install-system)

(defsystem asdf-install
  #+:sbcl :depends-on
  #+:sbcl (sb-bsd-sockets)
  :version "0.6.10"
  :author "Dan Barlow <dan@telent.net>, Edi Weitz <edi@agharta.de> and many others. See the file COPYRIGHT for more details."
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :components ((:file "defpackage")
               (:file "split-sequence" :depends-on ("defpackage"))
               
               (:file "port" :depends-on ("defpackage"))
               #+:digitool
               (:file "digitool" :depends-on ("port"))
               
	       (:file "conditions" :depends-on ("defpackage" "variables"))
               (:file "variables" :depends-on ("port"))
	       (:file "installer"
                      :depends-on ("port" "split-sequence" 
					  #+:digitool "digitool"
					  "conditions" "variables"))
               (:file "deprecated" :depends-on ("installer")))
  :in-order-to ((test-op (load-op test-asdf-install)))
  :perform (test-op :after (op c)
		    (funcall
		      (intern (symbol-name '#:run-tests) :lift)
		      :config :generic)))
	   
(defmethod perform :after ((o load-op) (c (eql (find-system :asdf-install))))
  (let ((show-version (find-symbol
                       (symbol-name '#:show-version-information)
		       '#:asdf-install)))
    (when (and show-version (fboundp show-version)) 
      (funcall show-version)))
  (provide 'asdf-install))

(defmethod operation-done-p 
    ((o test-op) (c (eql (find-system :asdf-install))))
  nil)

#+(or)
(defmethod perform ((o test-op) (c (eql (find-system :asdf-install))))
  t)
