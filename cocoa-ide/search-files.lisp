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

(defclass search-files-window-controller (ns:ns-window-controller)
  ((find-combo-box :foreign-type :id :accessor find-combo-box) ;IBOutlet
   (folder-combo-box :foreign-type :id :accessor folder-combo-box) ;IBOutlet
   (file-name-combo-box :foreign-type :id :accessor file-name-combo-box) ;IBOutlet
   (search-button :foreign-type :id :accessor search-button) ;IBOutlet
   (browse-button :foreign-type :id :accessor browse-button) ;IBOutlet
   (outline-view :foreign-type :id :accessor outline-view) ;IBOutlet
   (recursive-menu-item :foreign-type :id :accessor recursive-menu-item) ;IBOutlet
   (case-sensitive-menu-item :foreign-type :id :accessor case-sensitive-menu-item) ;IBOutlet
   (expand-results-menu-item :foreign-type :id :accessor expand-results-menu-item) ;IBOutlet
   ;;the following three vectors contain NSStrings, they're treated as stacks, so the last entry appears first
   ;;They behave as a history so the top of the stack is the most recent
   ;;TODO:  figure out how to save these as a user preference
   (find-strings :initform (make-array 10 :fill-pointer 0 :adjustable t)  :accessor find-strings) ;contains NSStrings
   (folder-strings :initform (make-array 10 :fill-pointer 0 :adjustable t)  :accessor folder-strings) ;contains NSStrings
   (file-name-strings :initform (make-array 10 :fill-pointer 0 :adjustable t)  :accessor file-name-strings) ;contains NSStrings
   (results :initform (make-array 10 :fill-pointer 0 :adjustable t) :accessor search-results) ;contains a vector of search-result-files
   (search-dir :initform "" :accessor search-dir) ;the expanded search directory
   (search-str :initform "")) ;a lisp string
  (:metaclass ns:+ns-object))

(defun recursive-p (wc)
  (not (zerop (#/state (recursive-menu-item wc)))))

(defun case-sensitive-p (wc)
  (not (zerop (#/state (case-sensitive-menu-item wc)))))

(defun expand-results-p (wc)
  (not (zerop (#/state (expand-results-menu-item wc)))))

(objc:defmethod (#/toggleMenuItem :void) ((wc search-files-window-controller) menu-item)
  (#/setState: menu-item (if (zerop (#/state menu-item)) 1 0)))

(objc:defmethod (#/expandResults :void) ((wc search-files-window-controller) menu-item)
  (#/toggleMenuItem wc menu-item)
  (if (expand-results-p wc)
    (expand-all-results wc)
    (collapse-all-results wc)))

(objc:defmethod #/init ((self search-files-window-controller))
  (#/initWithWindowNibName: self #@"SearchFiles"))

;;Lifted from apropos-window.lisp, not sure if it's really needed...
#+later (objc:defmethod (#/automaticallyNotifiesObserversForKey: :<BOOL>) ((self +search-files-window-controller)
                                                                  key)
  (declare (ignore key))
  nil)

(objc:defmethod (#/awakeFromNib :void) ((wc search-files-window-controller))
  (with-slots (search-button browse-button find-combo-box folder-combo-box file-name-combo-box 
			     file-name-strings outline-view recursive-menu-item 
                             case-sensitive-menu-item expand-results-menu-item) wc
    (#/setTarget: search-button wc)
    (#/setKeyEquivalent: search-button (%make-nsstring (string #\return))) ;makes it the default button
    (#/setAction: search-button (@selector #/doSearch))
    (#/setTarget: browse-button wc)
    (#/setAction: browse-button (@selector #/doBrowse))
    (vector-push-extend #@"*.lisp" file-name-strings)
    (#/setUsesDataSource: find-combo-box t)
    (#/setDataSource: find-combo-box wc)
    (#/setUsesDataSource: find-combo-box t)
    (#/setUsesDataSource: folder-combo-box t)
    (#/setDataSource: folder-combo-box wc)
    (#/setUsesDataSource: folder-combo-box t)
    (#/setUsesDataSource: file-name-combo-box t)
    (#/setDataSource: file-name-combo-box wc)
    (#/setUsesDataSource: file-name-combo-box t)
    (#/setDataSource: outline-view wc)
    (#/setTarget: outline-view wc)
    (#/setEditable: (#/objectAtIndex: (#/tableColumns outline-view) 0) nil)
    (#/setDoubleAction: outline-view (@selector #/editLine))
    (#/selectItemAtIndex: file-name-combo-box 0)
    (#/setTarget: recursive-menu-item wc)
    (#/setAction: recursive-menu-item (@selector #/toggleMenuItem))
    (#/setTarget: case-sensitive-menu-item wc)
    (#/setAction: case-sensitive-menu-item (@selector #/toggleMenuItem))
    (#/setTarget: expand-results-menu-item wc)
    (#/setAction: expand-results-menu-item (@selector #/expandResults))
    ))

(defmethod combo-box-to-vector ((wc search-files-window-controller) combo-box)
  (with-slots (find-combo-box folder-combo-box file-name-combo-box 
			      find-strings folder-strings file-name-strings) wc
    (cond ((eql combo-box find-combo-box) find-strings)
          ((eql combo-box file-name-combo-box) file-name-strings)
          ((eql combo-box folder-combo-box) folder-strings)
          (t (error "Unknown combo box: ~s" combo-box)))))

;;; Data source methods for combo box

(objc:defmethod (#/numberOfItemsInComboBox: :<NSI>nteger) ((wc search-files-window-controller)
						   combo-box)
  (length (combo-box-to-vector wc combo-box)))

(objc:defmethod #/comboBox:objectValueForItemAtIndex: ((wc search-files-window-controller)
						       combo-box
						       (index :<NSI>nteger))
  (let ((vec (combo-box-to-vector wc combo-box)))
    (aref vec (- (length vec) index 1))))

(defun ns-string-begins-with (partial-nstr nstr)
  (eql 0 (ns:ns-range-location (#/rangeOfString:options: nstr partial-nstr #$NSAnchoredSearch))))

(defun ns-string-equal (ns1 ns2)
  (and (typep ns1 'ns:ns-string)
       (typep ns2 'ns:ns-string)
       (#/isEqualToString: ns1 ns2)))

(objc:defmethod #/comboBox:completedString: ((wc search-files-window-controller)
					     combo-box
					     partial-nstr)
  (or (find partial-nstr (combo-box-to-vector wc combo-box) :from-end t
            :test #'ns-string-begins-with)
      #@""))

(objc:defmethod (#/comboBox:indexOfItemWithStringValue: :<NSUI>nteger)
    ((wc search-files-window-controller)
     combo-box
     string)
  (let* ((vec (combo-box-to-vector wc combo-box))
         (pos (position string vec :from-end t :test #'ns-string-equal)))
    (if pos
      (1- (length vec))
      #$NSNotFound)))

(defun get-combo-box-nstr (wc combo-box)
  (let* ((vec (combo-box-to-vector wc combo-box))
	 (nstr (#/stringValue combo-box))
         (pos (position nstr vec :test #'eql)))
    (unless pos (#/retain nstr))
    (unless (and pos (= (1+ pos) (length vec))) ;already at top of stack
      (setf vec (delete nstr vec :test #'ns-string-equal)) ;delete string if it's already there
      (vector-push-extend nstr vec))
    nstr))

(defmethod get-full-dir-string ((str string))
  ;make sure it has a trailing slash
  (let ((ret (ccl::native-untranslated-namestring str)))
    (unless (eql #\/ (aref str (1- (length str))))
      (setf ret (concatenate 'string ret "/")))
    ret))

(defmethod get-full-dir-string ((nsstring ns:ns-string))
  (get-full-dir-string (lisp-string-from-nsstring nsstring)))

(objc:defmethod (#/doSearch :void) ((wc search-files-window-controller) sender)
  (declare (ignore sender))
  (queue-for-gui #'(lambda ()
                     (with-slots (outline-view results) wc
                       (setf (fill-pointer results) 0)
                       (set-results-string wc #@"Searching...")
                       (#/reloadData outline-view))))
  (queue-for-gui 
   #'(lambda ()
       (with-slots (find-combo-box folder-combo-box file-name-combo-box
                                   results outline-view search-dir search-str) wc
         (let* ((find-nstr (get-combo-box-nstr wc find-combo-box))
                (folder-nstr (get-combo-box-nstr wc folder-combo-box))
                (file-name-nstr (get-combo-box-nstr wc file-name-combo-box)))
           (setf search-dir (get-full-dir-string folder-nstr)
                 search-str (lisp-string-from-nsstring find-nstr))
           (let* ((grep-output (call-grep (nconc (and (recursive-p wc) (list "-r"))
                                                 (unless (case-sensitive-p wc) (list "-i"))
                                                 (list "-c" "-e" (lisp-string-from-nsstring find-nstr)
                                                  "--include" (lisp-string-from-nsstring file-name-nstr)
                                                  search-dir))))
             (dir-len (length search-dir))
             (occurrences 0)
             (file-count 0))
             (map-lines grep-output
                        #'(lambda (start end)
                            (let* ((colon-pos (position #\: grep-output :from-end t :start start :end end))
                                   (count (parse-integer grep-output :start (1+ colon-pos) :end end)))
                              (incf file-count)
                              (when (> count 0)
                                (vector-push-extend (make-search-result-file 
                                                     :name (subseq grep-output (+ start dir-len) colon-pos)
                                                     :lines (make-array count :initial-element nil))
                                                    results)
                                (incf occurrences count)))))
             (set-results-string wc (%make-nsstring (format nil "Found ~a occurrences in ~a files out of ~a files searched."
                                                            occurrences (length results) file-count)))
             (#/setTitle: (#/window wc) (%make-nsstring (format nil "Search Files: ~a" search-str)))
             (#/reloadData outline-view)
             (when (and (> occurrences 0) (or (<  occurrences 20) (expand-results-p wc)))
               (expand-all-results wc))
             (#/reloadData outline-view)))))))

(defmethod expand-all-results ((wc search-files-window-controller))
  (with-slots (outline-view) wc
    (#/expandItem:expandChildren: outline-view +null-ptr+ t)
    (#/reloadData outline-view)))

(defmethod collapse-all-results ((wc search-files-window-controller))
  (with-slots (outline-view) wc
    (#/collapseItem:collapseChildren: outline-view +null-ptr+ t)
    (#/reloadData outline-view)))

(defun set-results-string (wc str)
  (#/setStringValue: (#/headerCell (#/objectAtIndex: (#/tableColumns (outline-view wc)) 0)) str))
	    
(objc:defmethod (#/doBrowse :void) ((wc search-files-window-controller) sender)
  (declare (ignore sender))
  (let ((dir (choose-directory-dialog)))
    (when dir
      (with-slots (folder-combo-box) wc
        (#/setStringValue: folder-combo-box dir)
        (get-combo-box-nstr wc folder-combo-box)))))

(objc:defmethod (#/editLine :void) ((wc search-files-window-controller) outline-view)
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
           (if (and (eq :exited status) (or (= exit-code 0) (= exit-code 1)))
            (return-from call-grep output)
            (error "Error running ~a, xit code: ~s" *grep-program* exit-code)))))))

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