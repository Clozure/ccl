;;;-*-Mode: LISP; Package: GUI -*-
;;;
;;;   Copyright (C) 2007 Clozure Associates

(in-package "GUI")

(def-cocoa-default *hyperspec-url-string* :string "http://www.lispworks.com/documentation/HyperSpec/" "HTTP URL for HyperSpec lookup")

(defloadvar *hyperspec-root-url* nil)
(defloadvar *hyperspec-map-sym-hash* nil)
(defloadvar *hyperspec-map-sym-url* nil)

(defvar *lookup-hyperspec-in-browser* nil "True if you want hyperspec lookups to be sent to your browser of choice. Nil if you want them sent to a window within CCL IDE.")

(def-cocoa-default *hyperspec-lookup-enabled* :bool nil "enables hyperspec lookup"
                   (lambda (old new)
                     (unless (eq new old)
                       (if new
                         (setup-hyperspec-root-url)
                         (progn
                           (when *hyperspec-root-url*
                             (#/release *hyperspec-root-url*))
                           (setq *hyperspec-root-url* nil)
                           (when *hyperspec-map-sym-url*
                             (#/release *hyperspec-map-sym-url*))
                           (setq *hyperspec-root-url* nil)
                           (setq *hyperspec-map-sym-hash* nil))))))


(defclass display-document (ns:ns-document)
    ((text-view :foreign-type :id))
  (:metaclass ns:+ns-object))

(defclass url-delegate (ns:ns-object)
    ()
  (:metaclass ns:+ns-object))

(objc:defmethod (#/textView:clickedOnLink:atIndex: :<BOOL>)
    ((self url-delegate)
     textview
     link
     (index :<NSUI>nteger))
  (declare (ignorable link))
  (let* ((attribute (#/attribute:atIndex:effectiveRange:
                     (#/textStorage textview)
                     #&NSLinkAttributeName
                     index
                     +null-ptr+)))
    (if (typep attribute 'ns:ns-url)
      (rlet ((dictp :id +null-ptr+))
        (let* ((data (make-instance 'ns:ns-data :with-contents-of-url attribute))
               (string (unless (%null-ptr-p data)
                         (make-instance 'ns:ns-attributed-string 
                                        :with-html data
                                        :base-url attribute
                                        :document-attributes dictp)))
               (textstorage (#/textStorage textview))
               (dict (pref dictp :id))
               (title
                #-cocotron
                 (unless (%null-ptr-p dict)
                        (#/valueForKey: dict #&NSTitleDocumentAttribute))))
          (when title 
            (#/setTitle: (#/window textview) title))
          (when string
            (#/beginEditing textstorage)
            (#/replaceCharactersInRange:withAttributedString:
             textstorage
             (ns:make-ns-range 0 (#/length textstorage))
             string)
            (#/setSelectedRange: textview (ns:make-ns-range 0 0))
            (#/endEditing textstorage)
            (#/scrollRangeToVisible: textview (ns:make-ns-range 0 0)))))))
  #$YES)

(objc:defmethod (#/textView:shouldChangeTextInRange:replacementString: :<BOOL>)
    ((self url-delegate)
     textview
     (range :<NSR>ange)
     string)
  (declare (ignorable textview range string))
  nil)





(objc:defmethod #/windowNibName ((self display-document))
  #@"displaydoc")

(objc:defmethod (#/windowControllerDidLoadNib: :void)
    ((self display-document) controller)
  (with-slots (text-view) self
    (unless (%null-ptr-p text-view)
      (#/setEditable: text-view t)
      (#/setDelegate: text-view (make-instance 'url-delegate))))
  (call-next-method controller))


(defun hyperspec-root-url ()
  (or *hyperspec-root-url*
      (setq *hyperspec-root-url* (setup-hyperspec-root-url))))

(defun setup-hyperspec-root-url ()
  (make-instance 'ns:ns-url
                 :with-string
                 (%make-nsstring *hyperspec-url-string*)))

(defun hyperspec-map-hash (document)
  (or *hyperspec-map-sym-hash*
      (rlet ((perror :id  +null-ptr+))
        (let* ((map-url (make-instance 'ns:ns-url :with-string #@"Data/Map_Sym.txt" :relative-to-url (hyperspec-root-url)))
               (data (make-instance 'ns:ns-data
                                    :with-contents-of-url map-url
                                    :options 0
                                    :error perror)))
          (let* ((err (pref perror :id)))
            (unless (%null-ptr-p err)
              (#/presentError: document err)
              (return-from hyperspec-map-hash nil)))
          (with-input-from-string (s (%str-from-ptr (#/bytes data) (#/length data)))
            (let* ((hash (make-hash-table :test #'eq))
                   (*package* (find-package "CL"))
                   (eof (cons nil nil)))
              (declare (dynamic-extent eof))
              (loop
                (let* ((sym (read s nil eof))
                       (url (read-line s nil eof)))
                  (when (eq sym eof)
                    (return 
                      (setq *hyperspec-map-sym-url* map-url
                            *hyperspec-map-sym-hash* hash)))
                  (setf (gethash sym hash) url)))))))))

(defun http-url (url)
  "Returns an ns-url object given a string representing an http URL."
  (with-cfstring (s url)
    (#/absoluteURL
     (make-instance 'ns:ns-url
       :with-string s))))

(defun pathname-to-file-url (pathname)
  "Returns an ns-url object given a string representing a file URL."
  (with-cfstring (s (native-translated-namestring pathname))
    (with-autorelease-pool
        (#/retain
         (#/fileURLWithPath: ns:ns-url s)))))

(defun open-cocoa-window-with-url (url &optional (default-title "") (erf nil))
  (rlet ((pdocattrs :id +null-ptr+)
         (perror :id  +null-ptr+))
    (let* ((data (make-instance 'ns:ns-data
                   :with-contents-of-url url
                   :options 0
                   :error perror)))
      (if (not (%null-ptr-p (pref perror :id)))
          (when (functionp erf)
            (funcall erf (pref perror :id)))
          (let* ((string (make-instance 'ns:ns-attributed-string
                           :with-html data
                           :base-url url
                           :document-attributes pdocattrs))
                 (docattrs (pref pdocattrs :id))
                 (title #+cocotron +null-ptr+
                        #-cocotron
                        (if (%null-ptr-p docattrs)
                            +null-ptr+
                            (#/objectForKey: docattrs #&NSTitleDocumentAttribute))))
            (with-cfstring (nsdefault-title default-title)
              (if (%null-ptr-p title)
                  (setq title nsdefault-title))
              (#/newDisplayDocumentWithTitle:content:
               (#/sharedDocumentController ns:ns-document-controller)
               title
               string)))))))

(defun lookup-hyperspec-symbol (symbol doc)
  (let* ((relative-url (gethash symbol (hyperspec-map-hash doc))))
    (when relative-url
      (let* ((url (#/absoluteURL
                   (make-instance 'ns:ns-url
                     :with-string (%make-nsstring relative-url)
                     :relative-to-url *hyperspec-map-sym-url*))))
        
        (if *lookup-hyperspec-in-browser*
            (ccl::%open-url-in-browser url)
            (open-cocoa-window-with-url url (string symbol) (lambda (errptr) (#/presentError: doc errptr))
                                        ))))))
                              
#+IGNORE
(gui::queue-for-gui (lambda () (open-cocoa-window-with-url (http-url "http://www.google.com/"))))

#+IGNORE
(gui::queue-for-gui (lambda () (open-cocoa-window-with-url 
                                (pathname-to-file-url "ccl:doc;ccl-documentation.html"))))
                   
                   
                   
                   
                
