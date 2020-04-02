;;;
;;; Copyright 2016 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

#| TODO.
    Fix this:
    Example where a regex fails:
    ^\(def(un|method)
    fails when "Search Comments" is unchecked, because
    when "Search Comments" is unchecked, the implicit pattern has already scanned
    past the beginning of the line.
|#

(in-package "GUI")

; search-files.lisp for pre 10.7 (pre-Lion) versions of OSX

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
   (regex-checkbox :foreign-type :id :accessor regex-checkbox)
   (search-comments-checkbox :foreign-type :id :accessor search-comments-checkbox)
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
   (search-comments-p :initform t :reader search-comments-p)
   (grep-process :initform nil :accessor grep-process))
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
          (ignore-errors (probe-file (get-full-dir-string lstr))))))))

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
                                  search-comments-checkbox search-comments-p recursive-p regex-p case-sensitive-p expand-results-p) wc
    (cond ((eql checkbox recursive-checkbox)
	   (setf recursive-p (not recursive-p)))
          ((eql checkbox regex-checkbox)
           (setf regex-p (not regex-p)))
	  ((eql checkbox case-sensitive-checkbox)
	   (setf case-sensitive-p (not case-sensitive-p)))
          ((eql checkbox search-comments-checkbox)
           (setf search-comments-p (not search-comments-p)))
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
      (#/initWithWindowNibName: self #@"SearchFilesPreLion")
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
  (with-slots (outline-view) wc
    (#/setTarget: outline-view wc)
    (#/setDoubleAction: outline-view (@selector #/editLine:)))
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

(defun make-grep-arglist (wc find-str)
  (let ((grep-args nil))
    (flet ((default-args (&rest etc)
                         (list* "-I" "-s" etc)))
      (cond ((search-comments-p wc) ; much simpler when we're searching comments
             (if (regex-p wc)
                 (setf grep-args (default-args "-E" find-str)) ; -E because we need egrep here for full regex generality
                 (setf grep-args (default-args "-e" find-str)))
             ; Ignore binary files, suppress error messages
             (unless (case-sensitive-p wc)
               (push "-i" grep-args))
             (unless (regex-p wc)
               (push "-F" grep-args)))
            (t ; not searching comments. Need a fancy pattern, regardless of whether regex-p or not.
             (unless (regex-p wc) ; treat string as literal. But we still have to use a pattern.
               (setf find-str (concatenate 'string "\\Q" find-str "\\E")))
             (setf find-str (concatenate 'string "(?:^[[:blank:]]*[^[:blank:];].*" find-str ")")) ; NOT a line beginning with ;
             (unless (case-sensitive-p wc)
               (setf find-str (concatenate 'string "(?i)" find-str)))
             (setf find-str (concatenate 'string find-str ""))
             (setf grep-args (default-args "-E" find-str)) 
             ())))
    grep-args))

(objc:defmethod (#/doSearch: :void) ((wc search-files-window-controller) sender)
  (declare (ignore sender))
  (set-results-string wc #@"Searching...")
  (setf (find-string-value wc) (#/stringValue (find-combo-box wc))
	(folder-string-value wc) (#/stringValue (folder-combo-box wc))
	(file-name-string-value wc) (#/stringValue (file-name-combo-box wc)))
  (let* ((find-str (lisp-string-from-nsstring (find-string-value wc)))
	 (folder-str (get-full-dir-string (folder-string-value wc)))
	 (file-str (lisp-string-from-nsstring (file-name-string-value wc)))
	 (grep-args (make-grep-arglist wc find-str)))
    (setf (search-str wc) find-str
          (search-dir wc) folder-str)
    (push "-c" grep-args) ; we just want the count here
    (if (recursive-p wc)
      (setf grep-args (nconc grep-args (list "-r" "--include" file-str folder-str)))
      ; following tells run-grep to use find instead of grep directly
      (setf grep-args (cons :nr (nconc grep-args (list file-str folder-str)))))
    (#/setEnabled: (search-button wc) nil)
    (setf (grep-process wc) (process-run-function "grep" 'run-grep grep-args wc))
    (#/setTitle: (#/window wc) (#/autorelease
				(%make-nsstring (format nil "Search Files: ~a"
							find-str))))))

(objc:defmethod (#/windowWillClose: :void) ((wc search-files-window-controller)
					    notification)
  (declare (ignore notification))
  (let* ((proc (grep-process wc)))
    (when proc (process-kill proc))))

; This was probably an attempt to resist long beachballing when the list to be expanded is long.
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
    (dotimes (idx (length old-results))
      (let* ((file (aref old-results idx))
             (lines (when file (search-result-file-lines file))))
        (dotimes (idx (length lines))
          (let* ((line (aref lines idx))
                 (string (when line (search-result-line-nsstr line))))
            (and string (#/release string))))
        (and (search-result-file-nsstr file)
             (#/release (search-result-file-nsstr file)))))
    (set-results-string wc msg)
;;     (when (or (auto-expandable-p (search-results wc))
;;              (expand-results-p wc))
;;       (expand-all-results wc))
    (setf (grep-process wc) nil)
    (#/reloadData (outline-view wc))
    (#/setEnabled: (search-button wc) t)))

;;; This is run in a secondary thread.
(defun run-grep (grep-arglist wc)
  (let ((progname nil))
    (case (car grep-arglist)
      (:nr ; nonrecursive. Gotta use find and a different arglist format
       ; NOTE: If you're using grep on the command line relative to cwd, nonrecursive grep works fine, and there's
       ;       no need for the find command.
       ;   Where grep doesn't work nonrecursively if if you explicitly hand grep an explicit folder-str to search in,
       ;       like we're doing here.
       (pop grep-arglist)
       ; last two things in arglist are (file-str folder-str) in that order.
       (destructuring-bind (file-str folder-str) (last grep-arglist 2)
         (setf grep-arglist (butlast grep-arglist 2))
         ; need -H because when using find rather than grep, filenames get lost
         (setf grep-arglist (append (list* folder-str "-name" file-str "-type" "f" "-maxdepth" "1" "-exec" "grep" "-H"
                                           grep-arglist)
                                    (list "{}" ";")))
         (setf progname "find")))
      (t
       (setf progname "grep")))
    (with-autorelease-pool 
        (#/performSelectorOnMainThread:withObject:waitUntilDone:
         (progress-indicator wc) (@selector #/startAnimation:) nil t)
      (unwind-protect
          (let* ((grep-output (call-grep grep-arglist progname)))
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
         (progress-indicator wc) (@selector #/stopAnimation:) nil t)))))

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
				    :name (enough-namestring
                                           (parse-namestring
                                            (subseq grep-output
						  start
						  colon-pos))
                                           (search-dir wc))
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
      (cocoa-edit-grep-line (concatenate 'string (search-dir wc) (search-result-line-file line-result))
                      (1- (search-result-line-number line-result))
                      (unless (regex-p wc) ; can't do regex searching in Hemlock. Just go to line.
                        (search-str wc))))))

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
    (if (Eql Item +Null-Ptr+)
      (let ((result (aref results child)))
        (or (search-result-file-nsstr result)
            (setf (search-result-file-nsstr result)
                  (%make-nsstring (format nil "[~a] ~a" 
                                          (length (search-result-file-lines result))
                                          (search-result-file-name result))))))
      (let* ((file-result (nsstring-to-file-result wc item))
             (line-result (get-line-result wc file-result child)))
        (if line-result
          (search-result-line-nsstr line-result)
          #@"[internal error]")))))

(defun get-line-result (wc file-result index)
  (let ((lines (search-result-file-lines file-result)))
    (or (aref lines index)
        (progn
          (compute-line-results wc file-result)
          (aref lines index)))))

(defun compute-line-results (wc file-result)
  (with-slots (search-str search-dir) wc
    (let* ((grep-args (cons "-n" (make-grep-arglist wc search-str))) ; prefix each result with line number
           (grep-output (call-grep (nconc grep-args 
                                          (list (concatenate 'string search-dir (search-result-file-name file-result))))))
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

(defun call-grep (args &optional (progname "grep"))
  ;;Calls grep with the strings as arguments, and returns a string containing the output
  (with-output-to-string (stream)
    (let* ((proc (run-program progname args :input nil :output stream)))
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

