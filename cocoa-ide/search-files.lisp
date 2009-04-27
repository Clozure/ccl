(in-package "GUI")

(defstruct search-result-file 
  name ;A lisp string that contains the full path of the file
  nsstr  ;An NSString that is shown in the dialog
  lines ;A vector of search-result-lines
  )

(defstruct search-result-line 
  file ;The search-result-file that contains this search-result-line
  number ;An integer that is the line-number of the line
  nsstr ;The NSString used in the dialog
  )

(defmethod print-object ((srl search-result-line) stream)
  (print-unreadable-object (srl stream :type t)
    (format stream "~a ~a ~s" 
            (search-result-line-file srl)
            (search-result-line-number srl)
            (search-result-line-nsstr srl))))

(defconstant $find-combo-box-tag 0)
(defconstant $folder-combo-box-tag 1)
(defconstant $file-name-combo-box-tag 2)

(defparameter *search-files-history-limit* 5 "combo box history length")

(defclass search-files-window-controller (ns:ns-window-controller)
  ((find-combo-box :foreign-type :id :accessor find-combo-box)
   (folder-combo-box :foreign-type :id :accessor folder-combo-box)
   (file-name-combo-box :foreign-type :id :accessor file-name-combo-box)
   (search-button :foreign-type :id :accessor search-button)
   (browse-button :foreign-type :id :accessor browse-button)
   (outline-view :foreign-type :id :accessor outline-view)
   (recursive-checkbox :foreign-type :id :accessor recursive-checkbox)
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
   (case-sensitive-p :initform nil :reader case-sensitive-p)
   (expand-results-p :initform nil :reader expand-results-p))
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
  (let* ((fm (#/defaultManager ns:ns-file-manager))
	 (path (folder-string-value wc)))
    (rlet ((dir-p #>BOOL))
      (and
       (#/fileExistsAtPath:isDirectory: fm path dir-p)
       (plusp (%get-byte dir-p))))))

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
  (with-slots (recursive-checkbox case-sensitive-checkbox expand-results-checkbox
	       recursive-p case-sensitive-p expand-results-p) wc
    (cond ((eql checkbox recursive-checkbox)
	   (setf recursive-p (not recursive-p)))
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
  (#/initWithWindowNibName: self #@"SearchFiles"))

(objc:defmethod (#/awakeFromNib :void) ((wc search-files-window-controller))
  (#/setStringValue: (status-field wc) #@"")
  (with-slots (outline-view) wc
    (#/setTarget: outline-view wc)
    (#/setDoubleAction: outline-view (@selector #/editLine:)))
  (setf (find-string-value wc) #@"")
  (with-slots (file-name-combo-box) wc
    (#/setStringValue: file-name-combo-box #@"*.lisp")
    (#/updateFileNameString: wc file-name-combo-box))
  (with-slots (folder-combo-box) wc
    (let ((dir (ccl::native-translated-namestring (ccl:current-directory))))
    (#/setStringValue: folder-combo-box
		       (#/autorelease (%make-nsstring dir)))
    (#/updateFolderString: wc folder-combo-box))))

(defun ns-string-equal (ns1 ns2)
  (and (typep ns1 'ns:ns-string)
       (typep ns2 'ns:ns-string)
       (#/isEqualToString: ns1 ns2)))

(defmethod get-full-dir-string ((str string))
  ;make sure it has a trailing slash
  (let ((ret (ccl::native-untranslated-namestring str)))
    (unless (eql #\/ (aref str (1- (length str))))
      (setf ret (concatenate 'string ret "/")))
    ret))

(defmethod get-full-dir-string ((nsstring ns:ns-string))
  (get-full-dir-string (lisp-string-from-nsstring nsstring)))

(objc:defmethod (#/doSearch: :void) ((wc search-files-window-controller) sender)
  (declare (ignore sender))
  (set-results-string wc #@"Searching...")
  (setf (find-string-value wc) (#/stringValue (find-combo-box wc))
	(folder-string-value wc) (#/stringValue (folder-combo-box wc))
	(file-name-string-value wc) (#/stringValue (file-name-combo-box wc)))
  (let* ((find-str (lisp-string-from-nsstring (find-string-value wc)))
	 (folder-str (lisp-string-from-nsstring (folder-string-value wc)))
	 (file-str (lisp-string-from-nsstring (file-name-string-value wc)))
	 (grep-args (list "-I" "-s" "-c" "-e" find-str "--include" file-str
			  (get-full-dir-string folder-str))))
    (when (recursive-p wc)
      (push "-r" grep-args))
    (unless (case-sensitive-p wc)
      (push "-i" grep-args))
    (setf (search-dir wc) folder-str
	  (search-str wc) find-str)
    (#/setEnabled: (search-button wc) nil)
    (process-run-function "grep" 'run-grep grep-args wc)
    (#/setTitle: (#/window wc) (#/autorelease
				(%make-nsstring (format nil "Search Files: ~a"
							find-str))))))

(defun auto-expandable-p (results)
  (let ((n 0))
    (dotimes (f (length results) t)
      (dotimes (l (length (search-result-file-lines (aref results f))))
	(incf n)
	(when (> n 20)
	  (return-from auto-expandable-p nil))))))

(objc:defmethod (#/updateResults: :void) ((wc search-files-window-controller)
					  msg)
  (let* ((old-results (search-results wc)))
    (setf (search-results wc) (new-results wc))
    ;; release NSString instances.  sigh.
    (dotimes (f (length old-results))
      (dotimes (l (length (search-result-file-lines f)))
	(and (search-result-line-nsstr l)
	     (#/release (search-result-line-nsstr l))))
      (and (search-result-file-nsstr f)
	   (#/release (search-result-file-nsstr f))))
    (set-results-string wc msg)
    (when (or (auto-expandable-p (search-results wc))
	      (expand-results-p wc))
      (expand-all-results wc))
    (#/reloadData (outline-view wc))
    (#/setEnabled: (search-button wc) t)))
    
;;; This is run in a secondary thread.
(defun run-grep (grep-arglist wc)
  (with-autorelease-pool 
      (#/performSelectorOnMainThread:withObject:waitUntilDone:
       (progress-indicator wc) (@selector #/startAnimation:) nil t)
    (unwind-protect
	 (let* ((grep-output (call-grep grep-arglist)))
	   (multiple-value-bind (results message)
	       (results-and-message grep-output wc)
	     ;; This assumes that only one grep can be running at
	     ;; a time.
	     (setf (new-results wc) results)
	     (#/performSelectorOnMainThread:withObject:waitUntilDone:
	      wc
	      (@selector #/updateResults:)
	      (#/autorelease (%make-nsstring message))
	      t)))
      (#/performSelectorOnMainThread:withObject:waitUntilDone:
       (progress-indicator wc) (@selector #/stopAnimation:) nil t))))

(defun results-and-message (grep-output wc)
  (let* ((results (make-array 10 :fill-pointer 0 :adjustable t))
	 (occurrences 0)
	 (file-count 0)
	 (dir-len (length (search-dir wc))))
    (map-lines
     grep-output
     #'(lambda (start end)
	 (let* ((colon-pos (position #\: grep-output :from-end t :start start
				     :end end))
		(count (and colon-pos
			    (parse-integer grep-output :start (1+ colon-pos)
					   :end end))))
	   (when count
	     (incf file-count)
	     (when (> count 0)
	       (vector-push-extend (make-search-result-file
				    :name (subseq grep-output
						  (+ start dir-len)
						  colon-pos)
				    :lines (make-array count :initial-element nil))
				   results)
	       (incf occurrences count))))))
    (values results
	    (format nil "Found ~a occurrence~:p in ~a file~:p out of ~a ~
                         file~:p searched." occurrences (length results)
			 file-count))))
		   
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
	    
(objc:defmethod (#/doBrowse: :void) ((wc search-files-window-controller) sender)
  (declare (ignore sender))
  (let ((pathname (cocoa-choose-directory-dialog)))
    (when pathname
      (ccl::with-autoreleased-nsstring
	  (dir (native-translated-namestring pathname))
	(with-slots (folder-combo-box) wc
	  (#/setStringValue: folder-combo-box dir)
	  (#/updateFolderString: wc folder-combo-box))))))

(objc:defmethod (#/editLine: :void) ((wc search-files-window-controller) outline-view)
  (let* ((item (get-selected-item outline-view))
         (line-result (and item (nsstring-to-line-result wc item))))
    (unless line-result
      (let ((file-result (and item (nsstring-to-file-result wc item))))
        (when file-result
          (setf line-result (get-line-result wc file-result 0)))))          
    (when line-result
      (cocoa-edit-grep-line (concatenate 'string (search-dir wc) "/" (search-result-line-file line-result))
                      (1- (search-result-line-number line-result))))))

(defun get-selected-item (outline-view)
  (let ((index (#/selectedRow outline-view)))
    (when (> index -1)
      (#/itemAtRow: outline-view (#/selectedRow outline-view)))))

(defun nsstring-to-file-result (wc nsstring)
  (find nsstring (search-results wc) :test #'ns-string-equal :key #'search-result-file-nsstr))

(defun nsstring-to-line-result (wc nsstring)
  (loop for file-result across (search-results wc)
    do (loop for line-result across (search-result-file-lines file-result)
         while line-result
         do (when (ns-string-equal nsstring (search-result-line-nsstr line-result))
              (return-from nsstring-to-line-result line-result)))))

;;NSOutlineView data source protocol
;- (id)outlineView:(NSOutlineView *)outlineView child:(NSInteger)index ofItem:(id)item
(objc:defmethod #/outlineView:child:ofItem: ((wc search-files-window-controller) view (child :<NSI>nteger) item)
  (declare (ignore view))
  (with-slots (results) wc
    (if (eql item +null-ptr+)
      (let ((result (aref results child)))
        (or (search-result-file-nsstr result)
            (setf (search-result-file-nsstr result)
                  (%make-nsstring (format nil "[~a] ~a" 
                                          (length (search-result-file-lines result))
                                          (search-result-file-name result))))))
      (let* ((file-result (nsstring-to-file-result wc item))
             (line-result (get-line-result wc file-result child)))
        (search-result-line-nsstr line-result)))))

(defun get-line-result (wc file-result index)
  (let ((lines (search-result-file-lines file-result)))
    (or (aref lines index)
        (progn
          (compute-line-results wc file-result)
          (aref lines index)))))

(defun compute-line-results (wc file-result)
  (with-slots (search-str search-dir) wc
    (let* ((grep-output (call-grep (nconc (unless (case-sensitive-p wc) (list "-i"))
                                          (list "-n" "-e" search-str 
                                                (concatenate 'string search-dir (search-result-file-name file-result))))))
           (index -1))
      (map-lines grep-output
                 #'(lambda (start end)
                     (let* ((str (subseq grep-output start end))
                            (colon-pos (position #\: str))
                            (num (parse-integer str :end colon-pos)))
                       (setf (aref (search-result-file-lines file-result) (incf index))
                             (make-search-result-line :file (search-result-file-name file-result) 
                                                      :number num 
                                                      :nsstr (%make-nsstring str)))))))))

;- (BOOL)outlineView:(NSOutlineView *)outlineView isItemExpandable:(id)item
(objc:defmethod (#/outlineView:isItemExpandable: :<BOOL>) ((wc search-files-window-controller) view item)
  (declare (ignore view))
  ;;it's expandable if it starts with #\[ (it's a file)
  (and (typep item 'ns:ns-string)
       (= (char-code #\[) (#/characterAtIndex: item 0))))

;- (NSInteger)outlineView:(NSOutlineView *)outlineView numberOfChildrenOfItem:(id)item
(objc:defmethod (#/outlineView:numberOfChildrenOfItem: :<NSI>nteger)
                ((wc search-files-window-controller) view item)
  (declare (ignore view))
  (if (eql item +null-ptr+)
    (length (search-results wc))
    (let ((file-result (nsstring-to-file-result wc item)))
      (if file-result
        (length (search-result-file-lines file-result))
        0))))

;- (id)outlineView:(NSOutlineView *)outlineView objectValueForTableColumn:(NSTableColumn *)tableColumn byItem:(id)item
(objc:defmethod #/outlineView:objectValueForTableColumn:byItem: 
                ((wc search-files-window-controller) outline-view table-column item)
  (declare (ignore outline-view table-column))
  (let ((file-result (nsstring-to-file-result wc item)))
    (if file-result
      (search-result-file-nsstr file-result)
      (let ((line-result (nsstring-to-line-result wc item)))
        (if line-result
          (search-result-line-nsstr line-result)
          #@"ERROR")))))

(defun call-grep (args)
  ;;Calls grep with the strings as arguments, and returns a string containing the output
  (with-output-to-string (stream)
    (let* ((proc (run-program "grep" args :input nil :output stream)))
      (multiple-value-bind (status exit-code) (external-process-status proc)
	(let ((output (get-output-stream-string stream)))
	  (if (eq :exited status)
	    (return-from call-grep output)
            (error "~a returned exit status ~s" *grep-program* exit-code)))))))

(defun map-lines (string fn)
  "For each line in string, fn is called with the start and end of the line"
  (loop with end = (length string)
    for start = 0 then (1+ pos)
    as pos = (or (position #\Newline string :start start :end end) end)
    when (< start pos) do (funcall fn start pos)
    while (< pos end)))


#|
(defun top-search ()
  (#/windowController 
   (first-window-with-controller-type 'search-files-window-controller)))
|#

