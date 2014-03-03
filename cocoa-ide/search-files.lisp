(in-package "GUI")

(defparameter *search-files-history-limit* 5 "combo box history length")

;;;
;;; Creating the outline view from grep
;;;

;;; Views for the outline view

(defun %image-and-text-table-cell-view ()
  (cg:with-rects ((image-frame 0 0 16 16)
                  (text-field-frame 0 0 100 16))
    (let* ((view (#/initWithFrame: (#/alloc ns:ns-table-cell-view)
                                   #&NSZeroRect))
           (image-view (#/initWithFrame: (#/alloc ns:ns-image-view)
                                         image-frame))
           (text-field (#/initWithFrame: (#/alloc ns:ns-text-field)
                                         text-field-frame))
           (views-dict (#/dictionaryWithObjectsAndKeys: ns:ns-dictionary
                                                        image-view #@"imageView"
                                                        text-field #@"textField"
                                                        +null-ptr+)))
      (#/setNextKeyView: view text-field)
      (#/setImageView: view image-view)
      (#/setTextField: view text-field)
      (#/setDrawsBackground: text-field nil)
      (#/setFont: text-field (#/systemFontOfSize:
                              ns:ns-font
                              (#/smallSystemFontSize ns:ns-font)))
      (#/setBordered: text-field nil)
      (#/setEditable: text-field nil)
      (#/setLineBreakMode: (#/cell text-field)
                           #$NSLineBreakByTruncatingTail)
      (#/setTranslatesAutoresizingMaskIntoConstraints: image-view nil)
      (#/setTranslatesAutoresizingMaskIntoConstraints: text-field nil)
      (#/addSubview: view image-view)
      (#/release image-view)
      (#/addSubview: view text-field)
      (#/release text-field)
      (#/addConstraints: view
                         (#/constraintsWithVisualFormat:options:metrics:views:
                          ns:ns-layout-constraint
                          #@"|-5-[imageView(==16)]-[textField(>=100)]|"
                          0 +null-ptr+ views-dict))
      (#/addConstraint: view
                        (#/constraintWithItem:attribute:relatedBy:toItem:attribute:multiplier:constant:
                         ns:ns-layout-constraint image-view #$NSLayoutAttributeCenterY
                         #$NSLayoutRelationEqual view #$NSLayoutAttributeCenterY 1d0 0d0))
      (#/addConstraint: view
                        (#/constraintWithItem:attribute:relatedBy:toItem:attribute:multiplier:constant:
                         ns:ns-layout-constraint text-field #$NSLayoutAttributeCenterY
                         #$NSLayoutRelationEqual view #$NSLayoutAttributeCenterY 1d0 0d0))
      view)))

(defun %navigator-search-table-cell-view ()
  (%image-and-text-table-cell-view))

(defun %navigator-match-table-cell-view ()
  (cg:with-rects ((text-field-frame 0 0 100 16))
    (let* ((view (#/initWithFrame: (#/alloc ns:ns-table-cell-view)
                                   #&NSZeroRect))
           (text-field (#/initWithFrame: (#/alloc ns:ns-text-field)
                                         text-field-frame))
           (views-dict (#/dictionaryWithObjectsAndKeys: ns:ns-dictionary
                                                        text-field
                                                        #@"textField"
                                                        +null-ptr+)))
      (#/setNextKeyView: view text-field)
      (#/setTextField: view text-field)
      (#/setDrawsBackground: text-field nil)
      (#/setFont: text-field (#/systemFontOfSize:
                              ns:ns-font
                              (#/smallSystemFontSize ns:ns-font)))
      (#/setBordered: text-field nil)
      (#/setEditable: text-field t)
      (#/setLineBreakMode: (#/cell text-field)
                           #$NSLineBreakByTruncatingTail)
      (#/setTranslatesAutoresizingMaskIntoConstraints: text-field nil)
      (#/addSubview: view text-field)
      (#/release text-field)
      (#/addConstraints: view (#/constraintsWithVisualFormat:options:metrics:views:
                               ns:ns-layout-constraint
                               #@"|-5-[textField(>=100)]|"
                               0 +null-ptr+ views-dict))
      (#/addConstraint: view
                        (#/constraintWithItem:attribute:relatedBy:toItem:attribute:multiplier:constant:
                         ns:ns-layout-constraint text-field #$NSLayoutAttributeCenterY
                         #$NSLayoutRelationEqual view #$NSLayoutAttributeCenterY 1d0 0d0))
      view)))



(defun string-to-lines (string)
  (with-input-from-string (s string)
    (loop for line = (read-line s nil nil) while line collect line)))

(defun run-grep (pathnames pattern)
  ;; Don't try to grep files that don't exist.  Even if we use
  ;; grep's -s flag, the exit code will still indicate an error.
  (setq pathnames (remove-if-not #'probe-file pathnames))
  (let* ((s (make-string-output-stream))
         (args (append (list "-nHI" "--null" "--directories=skip" "-F" pattern) pathnames))
         (proc (run-program "/usr/bin/grep" args :input nil :output s)))
    (multiple-value-bind (status exit-code)
                         (external-process-status proc)
      (if (eq status :exited)
        (cond ((= exit-code 0)
               ;; matched one or more lines
               (values (string-to-lines (get-output-stream-string s)) t))
              ((= exit-code 1)
               ;; no matches
               (values nil t))
              ((>= exit-code 2)
               ;; some error occurred
               (values nil nil)))
        (values (ccl::describe-external-process-failure proc "running grep") nil)))))

(defun run-search-files-grep (folder file-pattern pattern
                              &key recursive case-sensitive regex)
  (when (probe-file folder)
    (let* ((s (make-string-output-stream))
           (args (append (list "-snHI" "--null" "--include" file-pattern
                               "--directories=skip")
                         (when recursive
                           (list "-R"))
                         (unless case-sensitive
                           (list "-i"))
                         (unless regex
                           (list "-F"))
                         (list pattern
                               (string-right-trim "/" folder))))
           (proc (run-program "/usr/bin/grep" args :input nil :output s)))
      (multiple-value-bind (status exit-code)
          (external-process-status proc)
        (if (eq status :exited)
            (cond ((= exit-code 0)
                   ;; matched one or more lines
                   (values (string-to-lines (get-output-stream-string s)) t))
                  ((= exit-code 1)
                   ;; no matches
                   (values nil t))
                  ((>= exit-code 2)
                   ;; Return *some* data, even if the exit code
                   ;; indicates an error. This is mainly because grep
                   ;; will still return useful results even if some
                   ;; files were unreadable (e.g. dangling symlinks).
                   (values (string-to-lines (get-output-stream-string s))
                           nil)))
            (values (ccl::describe-external-process-failure proc "running grep") nil))))))

(defclass grep-result ()
  ((file :accessor grep-result-file :initarg :file)
   (matches :accessor grep-result-matches :initarg :matches)))

(defun grep-result-match-count (grep-result)
  (length (grep-result-matches grep-result)))

(defmethod print-object ((g grep-result) stream)
  (print-unreadable-object (g stream :type t)
    (format stream "~s, ~d matched line~:p" (grep-result-file g)
            (length (grep-result-matches g)))))

(defun make-grep-result (filename matches)
  (make-instance 'grep-result :file filename :matches matches))

;; This assumes that grep was called with --null, which prints a null
;; after the filname.  If it printed the usual #\: then we would choke
;; on file names that contain colons.
(defun grep-output-line-values (line)
  (let* ((nul (position #\nul line))
         (colon (position #\: line :start (1+ nul))))
    (values (subseq line 0 nul)
            (parse-integer line :start (1+ nul) :end colon)
            (subseq line (1+ colon)))))

(defun generate-grep-results (lines)
  (let ((current-filename nil)
        (current-matches nil)
        (results nil))
    (dolist (line lines)
      (multiple-value-bind (filename line-number text)
                           (grep-output-line-values line)
        (cond ((null current-filename)
               (setq current-filename filename)
               (push (list line-number text) current-matches))
              ((string= filename current-filename)
               (push (list line-number text) current-matches))
              (t
               (push (make-grep-result current-filename
                                       (nreverse current-matches)) results)
               (setq current-filename filename)
               (setq current-matches nil)
               (push (list line-number text) current-matches)))))
    (when current-matches
      (push (make-grep-result current-filename
                              (nreverse current-matches)) results))
    (nreverse results)))

(defclass navigator-search-node-data (ns:ns-object)
  ((display-string :foreign-type :id)
   (url :foreign-type :id)
   (line-number :foreign-type #>NSInteger)
   (text :foreign-type :id))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/setDisplayString: :void) ((self navigator-search-node-data) string)
  (with-slots (display-string) self
    (unless (eql string display-string)
      (#/release display-string)
      (setq display-string (#/retain string)))))

(objc:defmethod #/URL ((self navigator-search-node-data))
  (slot-value self 'url))

(objc:defmethod (#/setURL: :void) ((self navigator-search-node-data) new-url)
  (with-slots (url) self
    (unless (eql url new-url)
      (#/release url)
      (setq url (#/retain new-url)))))

(objc:defmethod (#/lineNumber #>NSInteger) ((self navigator-search-node-data))
  (slot-value self 'line-number))

(objc:defmethod (#/setLineNumber: :void) ((self navigator-search-node-data)
                                          (number #>NSInteger))
  (setf (slot-value self 'line-number) number))

(objc:defmethod (#/setText: :void) ((self navigator-search-node-data) string)
  (with-slots (text) self
    (unless (eql string text)
      (#/release text)
      (setq text (#/retain string)))))

(objc:defmethod (#/dealloc :void) ((self navigator-search-node-data))
  (#/release (slot-value self 'display-string))
  (#/release (slot-value self 'url))
  (#/release (slot-value self 'text))
  (call-next-method))

(objc:defmethod (#/isContainer #>BOOL) ((self navigator-search-node-data))
  (%null-ptr-p (slot-value self 'text)))

(defun %tree-node-for-grep-result (grep-result)
  (let* ((node-data (#/new navigator-search-node-data))
         (node (#/initWithRepresentedObject: (#/alloc ns:ns-tree-node) node-data))
         (file (grep-result-file grep-result)))
    (#/release node-data)
    (with-cfstring (s file)
      (#/setDisplayString: node-data s))
    (with-cfurl (u file)
      (#/setURL: node-data u))
    (let ((matches (grep-result-matches grep-result)))
      (dolist (match matches)
        (let* ((child-node-data (#/new navigator-search-node-data))
               (child-node (#/initWithRepresentedObject: (#/alloc ns:ns-tree-node)
                                                         child-node-data))
               (line-number (first match))
               (text (second match)))
          (#/release child-node-data)
          (with-cfurl (u file)
            (#/setURL: child-node-data u))
          (#/setLineNumber: child-node-data line-number)
          (with-cfstring (s text)
            (#/setText: child-node-data s))
          (#/addObject: (#/childNodes node) child-node))))
    node))

(defun %tree-node-for-grep-results (grep-results)
  (let* ((root-node (#/initWithRepresentedObject: (#/alloc ns:ns-tree-node)
						  +null-ptr+))
	 (children (#/mutableChildNodes root-node)))
    (dolist (r grep-results)
      (let ((node (%tree-node-for-grep-result r)))
	(#/addObject: children node)
	(#/release node)))
    root-node))

(defclass grep-results-data-source (ns:ns-object)
  ((root-node :foreign-type :id))
  (:metaclass ns:+ns-object))

(objc:defmethod #/init ((self grep-results-data-source))
  (let ((new (call-next-method)))
    (unless (%null-ptr-p new)
      (setf (slot-value self 'root-node)
	    (#/initWithRepresentedObject: (#/alloc ns:ns-tree-node)
					  +null-ptr+)))
    new))

(objc:defmethod (#/dealloc :void) ((self grep-results-data-source))
  (#/release (slot-value self 'root-node))
  (call-next-method))

(objc:defmethod (#/setRootNode: :void) ((self grep-results-data-source) tree-node)
  (with-slots (root-node) self
    (unless (eql root-node tree-node)
      (#/release root-node)
      (setq root-node (#/retain tree-node)))))

(objc:defmethod #/childrenForItem: ((self grep-results-data-source) item)
  (if (%null-ptr-p item)
    (#/childNodes (slot-value self 'root-node))
    (#/childNodes item)))

(objc:defmethod (#/outlineView:numberOfChildrenOfItem: #>NSInteger)
                ((self grep-results-data-source) outline-view item)
  (declare (ignore outline-view))
  (#/count (#/childrenForItem: self item)))

(objc:defmethod (#/outlineView:isItemExpandable: #>BOOL)
                ((self grep-results-data-source) outline-view item)
  (declare (ignore outline-view))
  (let* ((node (if (%null-ptr-p item)
                 (slot-value self 'root-node)
                 item))
         (node-data (#/representedObject node)))
    (#/isContainer node-data)))

(objc:defmethod #/outlineView:child:ofItem: ((self grep-results-data-source)
                                             outline-view (index #>NSInteger)
                                             item)
  (declare (ignore outline-view))
  (let ((children (#/childrenForItem: self item)))
    (#/objectAtIndex: children index)))

(objc:defmethod #/outlineView:viewForTableColumn:item: ((self grep-results-data-source)
                                                       outline-view table-column item)
  (declare (ignore table-column))
  (cond ((zerop (#/count (#/childNodes item)))
         (let ((view (%navigator-match-table-cell-view))
               (node-data (#/representedObject item)))
           (#/setStringValue: (#/textField view) (slot-value node-data 'text))
           (#/setEditable: (#/textField view) nil)
           (#/autorelease view)))
        (t
         (let ((view (#/makeViewWithIdentifier:owner: outline-view
                                                      #@"search-files-cell" self)))
           (when (%null-ptr-p view)
             (setq view (%navigator-search-table-cell-view))
             (#/setIdentifier: view #@"search-files-cell")
             (#/autorelease view))
           (let* ((node-data (#/representedObject item))
                  (url (slot-value node-data 'url))
                  (extension (#/pathExtension url))
                  (name +null-ptr+))
             (unless (%null-ptr-p url)
               (setq name (#/path url)))
             (if (%null-ptr-p name)
               (setq name #@"no url?"))
             (#/setEditable: (#/textField view) nil)
             (#/setTranslatesAutoresizingMaskIntoConstraints: (#/imageView view) nil)
             (#/setTranslatesAutoresizingMaskIntoConstraints: (#/textField view) nil)
             (#/setImage: (#/imageView view)
                          (#/iconForFileType: (#/sharedWorkspace ns:ns-workspace) extension))
             (#/setStringValue: (#/textField view) name))
           view))))

(objc:defmethod (#/outlineView:shouldSelectItem: #>BOOL) ((self grep-results-data-source) ov
                                                         item)
  (declare (ignore ov))
  (let ((node-data (#/representedObject item)))
    (not (#/isContainer node-data))))

;;;
;;; Displaying and updating the interface
;;;


(defclass search-files-window-controller (ns:ns-window-controller)
  ((find-combo-box :foreign-type :id :accessor find-combo-box)
   (folder-combo-box :foreign-type :id :accessor folder-combo-box)
   (file-name-combo-box :foreign-type :id :accessor file-name-combo-box)
   (search-button :foreign-type :id :accessor search-button)
   (browse-button :foreign-type :id :accessor browse-button)
   (outline-view :foreign-type :id :accessor outline-view)
   (recursive-checkbox :foreign-type :id :accessor recursive-checkbox)
   (regex-checkbox :foreign-type :id :accessor regex-checkbox)
   (case-sensitive-checkbox :foreign-type :id :accessor case-sensitive-checkbox)
   (expand-results-checkbox :foreign-type :id :accessor expand-results-checkbox)
   (progress-indicator :foreign-type :id :accessor progress-indicator)
   (status-field :foreign-type :id :accessor status-field)
   (find-string-value :foreign-type :id :reader find-string-value)
   (folder-string-value :foreign-type :id :reader folder-string-value)
   (file-name-string-value :foreign-type :id :reader file-name-string-value)
   (results :initform (make-array 10 :fill-pointer 0 :adjustable t)
	    :accessor search-results) ;contains a vector of search-result-files
   (new-results :accessor new-results)
   (search-dir :initform "" :accessor search-dir) ;the expanded search directory
   (search-str :initform "" :accessor search-str) ;a lisp string
   (recursive-p :initform t :reader recursive-p)
   (regex-p :initform t :reader regex-p)
   (case-sensitive-p :initform nil :reader case-sensitive-p)
   (expand-results-p :initform nil :reader expand-results-p)
   (grep-process :initform nil :accessor grep-process)
   (search-data-source :foreign-type :id :accessor search-data-source))
  (:metaclass ns:+ns-object))

(defmacro def-copying-setter (slot-name class-name)
  (let* ((new (gensym))
	 (obj (gensym)))
    `(defmethod (setf ,slot-name) (,new (,obj ,class-name))
       (with-slots (,slot-name) ,obj
	 (unless (eql ,slot-name ,new)
	   (#/release ,slot-name)
	   (setq ,slot-name (#/copy ,new)))))))

(def-copying-setter find-string-value search-files-window-controller)
(def-copying-setter folder-string-value search-files-window-controller)
(def-copying-setter file-name-string-value search-files-window-controller)

;;; Enable and disable the Search button according to the state of the
;;; search files dialog.

(defun can-search-p (wc)
  (and (plusp (#/length (find-string-value wc)))
       (folder-valid-p wc)
       (plusp (#/length (file-name-string-value wc)))))

(defmethod folder-valid-p ((wc search-files-window-controller))
  (let* ((nsstr (folder-string-value wc)))
    (when (and (typep nsstr ns:ns-string) (plusp (#/length nsstr)))
      (let ((lstr (lisp-string-from-nsstring nsstr)))
        (when (valid-host-p lstr)
          (probe-file (get-full-dir-string lstr)))))))

(objc:defmethod (#/controlTextDidChange: :void) ((wc search-files-window-controller) notification)
  (let* ((object (#/object notification))
	 (info (#/userInfo notification))
	 (field-editor (#/valueForKey: info #@"NSFieldEditor"))
	 (string-ok (plusp (#/length (find-string-value wc))))
	 (folder-ok (folder-valid-p wc))
	 (file-ok (plusp (#/length (file-name-string-value wc)))))
    (cond ((eql object (find-combo-box wc))
	   (setf string-ok (plusp (#/length (#/string field-editor)))))
	  ((eql object (folder-combo-box wc))
	   (setf (folder-string-value wc) (#/string field-editor))
	   (setf folder-ok (folder-valid-p wc)))
	  ((eql object (file-name-combo-box wc))
	   (setf file-ok (#/length (#/string field-editor)))))
    (#/setEnabled: (search-button wc) (and string-ok folder-ok file-ok))))

(objc:defmethod (#/comboBoxSelectionDidChange: :void) ((wc search-files-window-controller) notification)
  (declare (ignore notification))
  (#/setEnabled: (search-button wc) (can-search-p wc)))

(objc:defmethod (#/toggleCheckbox: :void) ((wc search-files-window-controller) checkbox)
  (with-slots (recursive-checkbox regex-checkbox case-sensitive-checkbox expand-results-checkbox
	       recursive-p regex-p case-sensitive-p expand-results-p) wc
    (cond ((eql checkbox recursive-checkbox)
	   (setf recursive-p (not recursive-p)))
          ((eql checkbox regex-checkbox)
           (setf regex-p (not regex-p)))
	  ((eql checkbox case-sensitive-checkbox)
	   (setf case-sensitive-p (not case-sensitive-p)))
	  ((eql checkbox expand-results-checkbox)
	   (setf expand-results-p (not expand-results-p))
	   (if expand-results-p
	     (expand-all-results wc)
	     (collapse-all-results wc))
	   (#/reloadData (outline-view wc)))
	  (t
	   (error "Unknown checkbox ~s" checkbox)))))

;;; For simple strings, it's easier to use the combo box's built-in
;;; list than it is to mess around with a data source.

(defun update-combo-box (combo-box string)
  (check-type string ns:ns-string)
  (unless (#/isEqualToString: string #@"")
    (#/removeItemWithObjectValue: combo-box string)
    (#/insertItemWithObjectValue:atIndex: combo-box string 0)
    (when (> (#/numberOfItems combo-box) *search-files-history-limit*)
      (#/removeItemAtIndex: combo-box *search-files-history-limit*))))

(objc:defmethod (#/updateFindString: :void) ((wc search-files-window-controller)
					     sender)
  (setf (find-string-value wc) (#/stringValue sender))
  (update-combo-box sender (find-string-value wc)))

(objc:defmethod (#/updateFolderString: :void) ((wc search-files-window-controller) sender)
  (setf (folder-string-value wc) (#/stringValue sender))
  (update-combo-box sender (folder-string-value wc)))

(objc:defmethod (#/updateFileNameString: :void) ((wc search-files-window-controller) sender)
  (setf (file-name-string-value wc) (#/stringValue sender))
  (update-combo-box sender (file-name-string-value wc)))

(objc:defmethod #/init ((self search-files-window-controller))
  (prog1
      (#/initWithWindowNibName: self #@"SearchFiles")
    (#/setShouldCascadeWindows: self nil)))

(defloadvar *search-files-cascade-point* (ns:make-ns-point 0 0))

(objc:defmethod (#/windowDidLoad :void) ((wc search-files-window-controller))
  ;; Cascade window from the top left point of the topmost search files window.
  (flet ((good-window-p (w)
           (and (not (eql w (#/window wc)))
                (eql (#/class (#/windowController w))
                     (find-class 'search-files-window-controller)))))
    (let* ((dialogs (remove-if-not #'good-window-p (gui::windows)))
           (top-dialog (car dialogs)))
      (if top-dialog
        (ns:with-ns-point (zp 0 0)
          (setq *search-files-cascade-point*
                (#/cascadeTopLeftFromPoint: top-dialog zp))))))
  (#/cascadeTopLeftFromPoint: (#/window wc) *search-files-cascade-point*))

(defun set-search-files-default-dir (wc)
  (let* ((w (first-window-satisfying-predicate #'window-pathname))
         (path (and w (window-pathname w)))
         (dir (if path
                (namestring (ccl::back-translate-pathname (directory-namestring path)))
                "ccl:"))
         (nsdir (#/autorelease (%make-nsstring dir))))
    (with-slots (folder-combo-box) wc
      (#/setStringValue: folder-combo-box nsdir)
      (#/updateFolderString: wc folder-combo-box))))

(objc:defmethod (#/awakeFromNib :void) ((wc search-files-window-controller))
  (#/setStringValue: (status-field wc) #@"")
  (with-slots (outline-view search-data-source) wc
    (#/setTarget: outline-view search-data-source)
    (#/setDoubleAction: outline-view (@selector #/editLine:))
    (setf search-data-source (#/new grep-results-data-source))
    (#/setDelegate: outline-view search-data-source)
    (#/setDataSource: outline-view search-data-source))
  (setf (find-string-value wc) #@"")
  (setf (folder-string-value wc) #@"")
  (with-slots (file-name-combo-box) wc
    (#/setStringValue: file-name-combo-box #@"*.lisp")
    (#/updateFileNameString: wc file-name-combo-box)))

(defun ns-string-equal (ns1 ns2)
  (and (typep ns1 'ns:ns-string)
       (typep ns2 'ns:ns-string)
       (#/isEqualToString: ns1 ns2)))

(defmethod get-full-dir-string ((str string))
  ;make sure it has a trailing slash
  (let* ((ret (ccl:native-translated-namestring str))
         (len (length ret)))
    (cond ((eql len 0) "./")
          ((eql #\/ (char ret (1- len))) ret)
          (t (concatenate 'string ret "/")))))


;;; nil host is considered valid
(defmethod valid-host-p ((ob t))
  nil)

(defmethod valid-host-p ((str string))
  (let ((colon-pos (position #\: str)))
    (or (not colon-pos)
        (ccl::logical-host-p (subseq str 0 colon-pos)))))

(defmethod valid-host-p ((p pathname))
  (ccl::logical-host-p (pathname-host p)))

(defmethod get-full-dir-string ((nsstring ns:ns-string))
  (get-full-dir-string (lisp-string-from-nsstring nsstring)))

(objc:defmethod (#/doSearch: :void) ((wc search-files-window-controller) sender)
  (declare (ignore sender))
  (set-results-string wc #@"Searching...")
  (#/setEnabled: (search-button wc) nil)
  (#/performSelectorOnMainThread:withObject:waitUntilDone:
   (progress-indicator wc) (@selector #/stopAnimation:) nil t)
  (process-run-function
   'do-search
   (lambda ()
     (let ((result-status #@"Done"))
       (#/performSelectorOnMainThread:withObject:waitUntilDone:
        (progress-indicator wc) (@selector #/startAnimation:) nil t)
       (unwind-protect
            (let* ((pattern (lisp-string-from-nsstring (find-string-value wc)))
                   (folder (get-full-dir-string (folder-string-value wc)))
                   (file-pattern (lisp-string-from-nsstring (file-name-string-value wc)))
                   (lines (run-search-files-grep folder file-pattern pattern
                                                     :recursive (recursive-p wc)
                                                     :case-sensitive (case-sensitive-p wc)
                                                     :regex (regex-p wc)))
                   (results (generate-grep-results lines))
                   (tree-node (%tree-node-for-grep-results results)))
              (setf result-status
                    (if results
                        (#/autorelease
                         (%make-nsstring
                          (format nil "~D line~:P matched in ~D file~:P"
                                  (reduce #'+ results
                                          :key #'grep-result-match-count)
                                  (length results))))
                        #@"No matches"))
              (setf (new-results wc) tree-node))
         (progn
           (#/performSelectorOnMainThread:withObject:waitUntilDone:
            wc
            (@selector #/updateResults:)
            +null-ptr+
            t)
           (set-results-string wc result-status)
           (#/setTitle: (#/window wc)
                        (#/autorelease
                         (%make-nsstring (format nil "Search Files: ~a"
                                                 (lisp-string-from-nsstring (find-string-value wc))))))
           (#/performSelectorOnMainThread:withObject:waitUntilDone:
            (progress-indicator wc) (@selector #/stopAnimation:) nil t)
           (#/setEnabled: (search-button wc) t)))))))

(objc:defmethod (#/windowWillClose: :void) ((wc search-files-window-controller)
					    notification)
  (declare (ignore notification))
  (let* ((proc (grep-process wc)))
    (when proc (process-kill proc))))

(objc:defmethod (#/updateResults: :void) ((wc search-files-window-controller)
					  sender)
  (declare (ignore sender))
  (with-slots (search-data-source outline-view new-results) wc
    (#/setRootNode: search-data-source new-results)
    (#/release new-results)
    (#/reloadData outline-view)))

(defmethod expand-all-results ((wc search-files-window-controller))
  (with-slots (outline-view) wc
    (#/expandItem:expandChildren: outline-view +null-ptr+ t)
    (#/reloadData outline-view)))

(defmethod collapse-all-results ((wc search-files-window-controller))
  (with-slots (outline-view) wc
    (#/collapseItem:collapseChildren: outline-view +null-ptr+ t)
    (#/reloadData outline-view)))

(defun set-results-string (wc str)
  (#/setStringValue: (status-field wc) str))

;;; For choosing the right directory
(objc:defmethod (#/doBrowse: :void) ((wc search-files-window-controller) sender)
  (declare (ignore sender))
  (let ((pathname (cocoa-choose-directory-dialog)))
    (when pathname
      (ccl::with-autoreleased-nsstring
	  (dir (native-translated-namestring pathname))
	(with-slots (folder-combo-box) wc
	  (#/setStringValue: folder-combo-box dir)
	  (#/updateFolderString: wc folder-combo-box))))))

;;; For jumping to a search result
(objc:defmethod (#/editLine: :void) ((wc search-files-window-controller) outline-view)
  (let ((selected-row (#/selectedRow outline-view)))
    (when (plusp selected-row)
      (let* ((item (#/itemAtRow: outline-view selected-row))
             (node-data (#/representedObject item))
             (url (#/URL node-data))
             (line-number (#/lineNumber node-data)))
        (cocoa-edit-grep-line (%get-cfstring (#/path url))
                              (1- line-number))))))
