;;;-*-Mode: LISP; Package: GUI -*-
;;;
;;;   Copyright (C) 2007 Clozure Associates

(in-package "GUI")

(def-cocoa-default *listener-input-font* :font #'(lambda ()
						   (#/fontWithName:size:
						    ns:ns-font
                                                    #+darwin-target
						    #@"Monaco"
                                                    #-darwin-target
                                                    #@"Courier"
                                                    10.0))
		   "Default font for listener input")
(def-cocoa-default *listener-output-font* :font #'(lambda ()
						    (#/fontWithName:size:
						     ns:ns-font
                                                     #+darwin-target
						     #@"Monaco"
                                                     #-darwin-target
                                                     #@"Courier"
                                                     10.0))
		   "Default font for listener output")

(def-cocoa-default *listener-rows* :int 16 "Initial height of listener windows, in characters")
(def-cocoa-default *listener-columns* :int 80 "Initial height of listener windows, in characters")

(def-cocoa-default hi::*listener-output-style* :int 1 "Text style index for listener output")

(def-cocoa-default hi::*listener-input-style* :int 0 "Text style index for listener output")

(def-cocoa-default *listener-background-color* :color '(1.0 1.0 1.0 1.0) "Listener default background color")

(def-cocoa-default *read-only-listener* :bool t "Do not allow editing old listener output")

(defun hemlock-ext:read-only-listener-p ()
  *read-only-listener*)


(defclass cocoa-listener-input-stream (fundamental-character-input-stream)
  ((queue :initform ())
   (queue-lock :initform (make-lock))
   (read-lock :initform (make-lock))
   (queue-semaphore :initform (make-semaphore)) ;; total queue count
   (text-semaphore :initform (make-semaphore))  ;; text-only queue count
   (cur-string :initform nil)
   (cur-string-pos :initform 0)
   (cur-env :initform nil)
   (cur-sstream :initform nil)
   (cur-offset :initform nil)
   (source-map :initform nil)
   (reading-line :initform nil :accessor hi:input-stream-reading-line)))

(defmethod interactive-stream-p ((stream cocoa-listener-input-stream))
  t)




(defmethod dequeue-listener-char ((stream cocoa-listener-input-stream) wait-p)
  (with-slots (queue queue-lock read-lock queue-semaphore text-semaphore cur-string cur-string-pos) stream
    (with-lock-grabbed (read-lock)
      (or (with-lock-grabbed (queue-lock)
            (when (< cur-string-pos (length cur-string))
              (prog1 (aref cur-string cur-string-pos) (incf cur-string-pos))))
          (loop
            (unless (if wait-p
                      (wait-on-semaphore text-semaphore nil "Listener Input")
                      (timed-wait-on-semaphore text-semaphore 0))
              (return nil))
            (assert (timed-wait-on-semaphore queue-semaphore 0) () "queue/text mismatch!")
            (with-lock-grabbed (queue-lock)
              (let* ((s (find-if #'stringp queue)))
                (assert s () "queue/semaphore mismatch!")
                (setq queue (delq s queue 1))
                (when (< 0 (length s))
                  (setf cur-string s cur-string-pos 1)
                  (return (aref s 0))))))))))

(defmethod ccl::read-toplevel-form ((stream cocoa-listener-input-stream) &key eof-value)
  (with-slots (queue queue-lock read-lock queue-semaphore text-semaphore cur-string cur-string-pos cur-sstream
               cur-env source-map cur-offset)
    stream
    (with-lock-grabbed (read-lock)
      (loop
        (when cur-sstream
          #+debug (log-debug "About to recursively read from sstring in env: ~s" cur-env)
          (let* ((env cur-env)
                 (form (progv (car env) (cdr env)
                         (ccl::read-toplevel-form cur-sstream
                                                  :eof-value eof-value
                                                  :file-name *loading-file-source-file*
                                                  :start-offset cur-offset
                                                  :map source-map)))
                 (last-form-in-selection (not (listen cur-sstream))))
            #+debug (log-debug " --> ~s" form)
            (when last-form-in-selection
              (setf cur-sstream nil cur-env nil))
            (return (values form env (or last-form-in-selection ccl::*verbose-eval-selection*)))))
        (when (with-lock-grabbed (queue-lock)
                (loop
                  unless (< cur-string-pos (length cur-string)) return nil
                  unless (whitespacep (aref cur-string cur-string-pos)) return t
                  do (incf cur-string-pos)))
          (return (values (call-next-method) nil t)))
        (wait-on-semaphore queue-semaphore nil "Toplevel Read")
        (let ((val (with-lock-grabbed (queue-lock) (pop queue))))
          (cond ((stringp val)
                 (assert (timed-wait-on-semaphore text-semaphore 0) () "text/queue mismatch!")
                 (setq cur-string val cur-string-pos 0))
                (t
                 (destructuring-bind (string package-name pathname offset) val
                   ;; This env is used both for read and eval.  *nx-source-note-map* is for the latter.
                   (let ((env (cons '(*loading-file-source-file* *loading-toplevel-location* ccl::*nx-source-note-map*)
                                    (list pathname nil source-map))))
                     (when package-name
                       (push '*package* (car env))
                       (push (ccl::pkg-arg package-name) (cdr env)))
                     (if source-map
                       (clrhash source-map)
                       (setf source-map (make-hash-table :test 'eq :shared nil)))
                     (setf cur-sstream (make-string-input-stream string) cur-env env cur-offset offset))))))))))

(defmethod enqueue-toplevel-form ((stream cocoa-listener-input-stream) string &key package-name pathname offset)
  (with-slots (queue-lock queue queue-semaphore) stream
    (with-lock-grabbed (queue-lock)
      (setq queue (nconc queue (list (list string package-name pathname offset))))
      (signal-semaphore queue-semaphore))))

(defmethod enqueue-listener-input ((stream cocoa-listener-input-stream) string)
  (with-slots (queue-lock queue queue-semaphore text-semaphore) stream
    (with-lock-grabbed (queue-lock)
      (setq queue (nconc queue (list string)))
      (signal-semaphore queue-semaphore)
      (signal-semaphore text-semaphore))))

(defmethod stream-read-char-no-hang ((stream cocoa-listener-input-stream))
  (dequeue-listener-char stream nil))

(defmethod stream-read-char ((stream cocoa-listener-input-stream))
  (dequeue-listener-char stream t))

(defmethod stream-unread-char ((stream cocoa-listener-input-stream) char)
  ;; Can't guarantee the right order of reads/unreads, just make sure not to
  ;; introduce any internal inconsistencies (and dtrt for the non-conflict case).
  (with-slots (queue queue-lock queue-semaphore text-semaphore cur-string cur-string-pos) stream
    (with-lock-grabbed (queue-lock)
      (cond ((>= cur-string-pos (length cur-string))
             (push (string char) queue)
             (signal-semaphore queue-semaphore)
             (signal-semaphore text-semaphore))
            ((< 0 cur-string-pos)
             (decf cur-string-pos)
             (setf (aref cur-string cur-string-pos) char))
            (t (setf cur-string (concatenate 'string (string char) cur-string)))))))

(defmethod ccl::stream-eof-transient-p ((stream cocoa-listener-input-stream))
  t)

(defmethod stream-clear-input ((stream cocoa-listener-input-stream))
  (with-slots (queue-lock cur-string cur-string-pos cur-sstream cur-env) stream
    (with-lock-grabbed (queue-lock)
      (setf (hi::input-stream-reading-line stream) nil)
      (setf cur-string nil cur-string-pos 0 cur-sstream nil cur-env nil))))

(defmethod stream-read-line ((stream cocoa-listener-input-stream))
  (let* ((old-reading-line (hi:input-stream-reading-line stream)))
    (unwind-protect
         (progn
           (setf (hi::input-stream-reading-line stream) t)
           (call-next-method))
      (setf (hi:input-stream-reading-line stream) old-reading-line))))

(defparameter $listener-flush-limit 4095)

(defclass cocoa-listener-output-stream (fundamental-character-output-stream)
  ((lock :initform (make-lock))
   (hemlock-view :initarg :hemlock-view)
   (data :initform (make-array (1+ $listener-flush-limit)
                               :adjustable t :fill-pointer 0
                               :element-type 'character))
   (limit :initform $listener-flush-limit)))

(defmethod stream-element-type ((stream cocoa-listener-output-stream))
  (with-slots (data) stream
    (array-element-type data)))

(defmethod ccl:stream-write-char ((stream cocoa-listener-output-stream) char)
  (with-slots (data lock limit) stream
    (when (with-lock-grabbed (lock)
	    (>= (vector-push-extend char data) limit))
      (stream-force-output stream))))

;; This isn't really thread safe, but it's not too bad...  I'll take a chance - trying
;; to get it to execute in the gui thread is too deadlock-prone.
(defmethod hemlock-listener-output-mark-column ((view hi::hemlock-view))
  (let* ((output-region (hi::variable-value 'hemlock::current-output-font-region
					    :buffer (hi::hemlock-view-buffer view))))
    (hi::mark-charpos (hi::region-end output-region))))

;; TODO: doesn't do the right thing for embedded tabs (in buffer or data)
(defmethod ccl:stream-line-column ((stream cocoa-listener-output-stream))
  (with-slots (hemlock-view data lock) stream
    (with-lock-grabbed (lock)
      (let* ((n (length data))
             (pos (position #\Newline data :from-end t)))
        (if (null pos)
          (+ (hemlock-listener-output-mark-column hemlock-view) n)
          (- n pos 1))))))

(defmethod ccl:stream-fresh-line  ((stream cocoa-listener-output-stream))
  (with-slots (hemlock-view data lock limit) stream
    (when (with-lock-grabbed (lock)
            (let ((n (length data)))
              (unless (if (= n 0)
                        (= (hemlock-listener-output-mark-column hemlock-view) 0)
                        (eq (aref data (1- n)) #\Newline))
                (>= (vector-push-extend #\Newline data) limit))))
      (stream-force-output stream))))

(defmethod ccl::stream-finish-output ((stream cocoa-listener-output-stream))
  (stream-force-output stream))

(defmethod ccl:stream-force-output ((stream cocoa-listener-output-stream))
  (if (typep *current-process* 'appkit-process)
    (with-slots (hemlock-view data lock) stream
      (with-lock-grabbed (lock)
        (when (> (fill-pointer data) 0)
          (append-output hemlock-view data)
          (setf (fill-pointer data) 0))))
    (with-slots (data) stream
      (when (> (fill-pointer data) 0)
        (queue-for-gui #'(lambda () (stream-force-output stream)))))))

(defmethod ccl:stream-clear-output ((stream cocoa-listener-output-stream))
  (with-slots (data lock) stream
    (with-lock-grabbed (lock)
      (setf (fill-pointer data) 0))))

(defmethod ccl:stream-line-length ((stream cocoa-listener-output-stream))
  ;; TODO: ** compute length from window size **
  80)


(defloadvar *cocoa-listener-count* 0)

(defclass cocoa-listener-process (process)
    ((input-stream :reader cocoa-listener-process-input-stream)
     (output-stream :reader cocoa-listener-process-output-stream)
     (backtrace-contexts :initform nil
                         :accessor cocoa-listener-process-backtrace-contexts)
     (window :reader cocoa-listener-process-window)))
  
(defloadvar *first-listener* t)

(defun new-cocoa-listener-process (procname window)
  (declare (special *standalone-cocoa-ide*))
  (let* ((input-stream (make-instance 'cocoa-listener-input-stream))
         (output-stream (make-instance 'cocoa-listener-output-stream
                          :hemlock-view (hemlock-view window)))
         
         (proc
          (ccl::make-mcl-listener-process 
           procname
           input-stream
           output-stream
           ;; cleanup function
           #'(lambda ()
               (mapcar #'(lambda (buf)
                           (when (eq (buffer-process buf) *current-process*)
                             (let ((doc (hi::buffer-document buf)))
                               (when doc
                                 (setf (hemlock-document-process doc) nil) ;; so #/close doesn't kill it.
                                 (#/performSelectorOnMainThread:withObject:waitUntilDone:
                                  doc
                                  (@selector #/close)
                                  +null-ptr+
                                  nil)))))
                       hi:*buffer-list*))
           :initial-function
           #'(lambda ()
               (setq ccl::*listener-autorelease-pool* (create-autorelease-pool))
               (when (and *standalone-cocoa-ide*
                        (prog1 *first-listener* (setq *first-listener* nil)))
                 (ccl::startup-ccl (ccl::application-init-file ccl::*application*))
                 (ui-object-note-package *nsapp* *package*))
               (ccl::listener-function))
           :echoing nil
           :class 'cocoa-listener-process)))
    (setf (slot-value proc 'input-stream) input-stream)
    (setf (slot-value proc 'output-stream) output-stream)
    (setf (slot-value proc 'window) window)
    proc))
  
(defclass hemlock-listener-frame (hemlock-frame)
    ()
  (:metaclass ns:+ns-object))
(declaim (special hemlock-listener-frame))

(objc:defmethod (#/setDocumentEdited: :void) ((w hemlock-listener-frame)
                                              (edited #>BOOL))
  (declare (ignorable edited)))


(defclass hemlock-listener-window-controller (hemlock-editor-window-controller)
    ()
  (:metaclass ns:+ns-object)
  )
(declaim (special hemlock-listener-window-controller))

;;; Listener documents are never (or always) ediited.  Don't cause their
;;; close boxes to be highlighted.
(objc:defmethod (#/setDocumentEdited: :void)
    ((self hemlock-listener-window-controller) (edited :<BOOL>))
  (declare (ignorable edited)))



(objc:defmethod #/windowTitleForDocumentDisplayName: ((self hemlock-listener-window-controller) name)
  (let* ((doc (#/document self)))
    (if (or (%null-ptr-p doc)
            (not (%null-ptr-p (#/fileURL doc))))
      (call-next-method name)
      (let* ((buffer (hemlock-buffer doc))
             (bufname (if buffer (hi::buffer-name buffer))))
        (if bufname
          (%make-nsstring bufname)
          (call-next-method name))))))


;;; The HemlockListenerDocument class.


(defclass hemlock-listener-document (hemlock-editor-document)
  ((process :reader %hemlock-document-process :writer (setf hemlock-document-process)))
  (:metaclass ns:+ns-object))
(declaim (special hemlock-listener-document))

(defgeneric hemlock-document-process (doc)
  (:method ((unknown t)) nil)
  (:method ((doc hemlock-listener-document)) (%hemlock-document-process doc)))

;; Nowadays this is nil except for listeners.
(defun buffer-process (buffer)
  (hemlock-document-process (hi::buffer-document buffer)))

(defmethod update-buffer-package ((doc hemlock-listener-document) buffer)
  (declare (ignore buffer)))

(defmethod document-encoding-name ((doc hemlock-listener-document))
  "UTF-8")

(defmethod user-input-style ((doc hemlock-listener-document))
  hi::*listener-input-style*)
  
(defmethod textview-background-color ((doc hemlock-listener-document))
  *listener-background-color*)

;; For use with the :process-info listener modeline field
(defmethod hemlock-ext:buffer-process-description (buffer)
  (let ((proc (buffer-process buffer)))
    (when proc
      (format nil "~a(~d) [~a]"
              (ccl:process-name proc)
              (ccl::process-serial-number proc)
              ;; TODO: this doesn't really work as a modeline item, because the modeline
              ;; doesn't get notified when it changes.
              (ccl:process-whostate proc)))))

(objc:defmethod #/topListener ((self +hemlock-listener-document))
  (let* ((all-documents (#/orderedDocuments *NSApp*)))
    (dotimes (i (#/count all-documents) +null-ptr+)
      (let* ((doc (#/objectAtIndex: all-documents i)))
	(when (eql (#/class doc) self)
	  (return doc))))))

(defun symbol-value-in-top-listener-process (symbol)
  (let* ((process (hemlock-document-process (#/topListener hemlock-listener-document))))
     (if process
       (ignore-errors (symbol-value-in-process symbol process))
       (values nil t))))
  
(defun hemlock-ext:top-listener-output-stream ()
  (let* ((process (hemlock-document-process (#/topListener hemlock-listener-document))))
    (when process
      (setq process (require-type process 'cocoa-listener-process))
      (cocoa-listener-process-output-stream process))))

(defun hemlock-ext:top-listener-input-stream ()
  (let* ((process (hemlock-document-process (#/topListener hemlock-listener-document))))
    (when process
      (setq process (require-type process 'cocoa-listener-process))
      (cocoa-listener-process-input-stream process))))



(objc:defmethod (#/isDocumentEdited :<BOOL>) ((self hemlock-listener-document))
  nil)



(objc:defmethod #/init ((self hemlock-listener-document))
  (let* ((doc (call-next-method)))
    (unless (%null-ptr-p doc)
      (let* ((listener-name (if (eql 1 (incf *cocoa-listener-count*))
                              "Listener"
                              (format nil
                                      "Listener-~d" *cocoa-listener-count*)))
	     (buffer (hemlock-buffer doc)))
	(setf (hi::buffer-pathname buffer) nil
	      (hi::buffer-minor-mode buffer "Listener") t
	      (hi::buffer-name buffer) listener-name)
        (hi::set-buffer-modeline-fields buffer hemlock::*listener-modeline-fields*)))
    doc))

(def-cocoa-default *initial-listener-x-pos* :float -100.0f0 "X position of upper-left corner of initial listener")

(def-cocoa-default *initial-listener-y-pos* :float 100.0f0 "Y position of upper-left corner of initial listener")

(defloadvar *next-listener-x-pos* nil) ; set after defaults initialized
(defloadvar *next-listener-y-pos* nil) ; likewise

(objc:defmethod (#/close :void) ((self hemlock-listener-document))
  (if (zerop (decf *cocoa-listener-count*))
    (setq *next-listener-x-pos* nil
          *next-listener-y-pos* nil))
  (let* ((p (shiftf (hemlock-document-process self) nil)))
    (when p
      (process-kill p)))
  (call-next-method))



(objc:defmethod (#/makeWindowControllers :void) ((self hemlock-listener-document))
  (let* ((textstorage (slot-value self 'textstorage))
         (window (%hemlock-frame-for-textstorage
                  hemlock-listener-frame
                  textstorage
                  *listener-columns*
                  *listener-rows*
                  t
                  (textview-background-color self)
                  (user-input-style self)))
	 (listener-styles (#/arrayWithObjects: ns:ns-mutable-array
					       (rme-create-text-attributes
						:font *listener-input-font*)
					       (rme-create-text-attributes
						:font *listener-output-font*)
					       +null-ptr+))
	 (controller (make-instance
		      'hemlock-listener-window-controller
		      :with-window window))
	 (listener-name (hi::buffer-name (hemlock-buffer self)))
         (path (#/windowTitleForDocumentDisplayName: controller (#/displayName self ))))
    (when (slot-exists-p textstorage 'styles)
      (with-slots (styles) textstorage
	;; We probably should be more disciplined about
	;; Cocoa memory management.  Having retain/release in
	;; random places all over the code is going to get
	;; unwieldy.
	(#/release styles)
	(setf styles (#/retain listener-styles))))
    ;; Disabling background layout on listeners is an attempt to work
    ;; around a bug.  The bug's probably gone ...
    #-cocotron                          ;no concept of background layout
    (let* ((layout-managers (#/layoutManagers textstorage)))
      (dotimes (i (#/count layout-managers))
        (let* ((layout (#/objectAtIndex: layout-managers i)))
          (#/setBackgroundLayoutEnabled: layout nil))))
    (#/setDelegate: window controller)
    (#/setDelegate: (text-pane-text-view (slot-value window 'pane)) self)
    (#/setShouldCascadeWindows: controller nil)
    (#/addWindowController: self controller)
    (#/release controller)
    (setf (hemlock-document-process self)
          (new-cocoa-listener-process listener-name window))
    (when path
      (unless (#/setFrameAutosaveName: window path)
        (setq path nil)))
    (unless (and path
                 (when (#/setFrameUsingName: window path)
                   (let* ((frame (#/frame window)))
                     (ns:with-ns-point (current-point
                                        (ns:ns-rect-x frame)
                                        (+ (ns:ns-rect-y frame)
                                           (ns:ns-rect-height frame)))
                        (let* ((next-point (#/cascadeTopLeftFromPoint:
                                            window
                                            current-point)))
                     (setq *next-listener-x-pos*
                           (ns:ns-point-x next-point)
                           *next-listener-y-pos*
                           (ns:ns-point-y next-point)))))
                   t))
      (ns:with-ns-point (current-point
                         (or *next-listener-x-pos*
                             (x-pos-for-window window *initial-listener-x-pos*))
                         (or *next-listener-y-pos*
                             (y-pos-for-window window *initial-listener-y-pos*)))
        (let* ((new-point (#/cascadeTopLeftFromPoint: window current-point)))
          (setf *next-listener-x-pos* (ns:ns-point-x new-point)
                *next-listener-y-pos* (ns:ns-point-y new-point)))))
    (#/synchronizeWindowTitleWithDocumentName controller)
    controller))

(objc:defmethod (#/textView:shouldChangeTextInRange:replacementString: :<BOOL>)
    ((self hemlock-listener-document)
     tv
     (range :<NSR>ange)
     string)
  (declare (ignore tv string))
  (let* ((range-start (ns:ns-range-location range))
         (range-end (+ range-start (ns:ns-range-length range)))
         (buffer (hemlock-buffer self))
         (protected-region (hi::buffer-protected-region buffer)))
    (if protected-region
      (let* ((prot-start (hi:mark-absolute-position (hi::region-start protected-region)))
             (prot-end (hi:mark-absolute-position (hi::region-end protected-region))))
        (not (or (and (>= range-start prot-start)
                      (< range-start prot-end))
                 (and (>= range-end prot-start)
                      (< range-end prot-end)))))
      t)))
    
    
;;; Action methods
(objc:defmethod (#/interrupt: :void) ((self hemlock-listener-document) sender)
  (declare (ignore sender))
  (let* ((process (hemlock-document-process self)))
    (when process
      (ccl::force-break-in-listener process))))



(objc:defmethod (#/exitBreak: :void) ((self hemlock-listener-document) sender)
  (declare (ignore sender))
  (let* ((process (hemlock-document-process self)))
    #+debug (log-debug  "~&exitBreak process ~s" process)
    (when process
      (process-interrupt process #'abort-break))))

(defmethod listener-backtrace-context ((proc cocoa-listener-process))
  (car (cocoa-listener-process-backtrace-contexts proc)))

(objc:defmethod (#/backtrace: :void) ((self hemlock-listener-document) sender)
  (let* ((process (hemlock-document-process self)))
    (when process
      (let* ((context (listener-backtrace-context process)))
        (when context
          (#/makeKeyAndOrderFront: (#/windowForSheet self) nil)
          (#/showWindow: (backtrace-controller-for-context context) sender))))))

(defun restarts-controller-for-context (context)
  (or (car (ccl::bt.restarts context))
      (setf (car (ccl::bt.restarts context))
            (let* ((tcr (ccl::bt.tcr context))
                   (tsp-range (inspector::make-tsp-stack-range tcr context))
                   (vsp-range (inspector::make-vsp-stack-range tcr context))
                   (csp-range (inspector::make-csp-stack-range tcr context))
                   (process (ccl::tcr->process tcr)))
              (make-instance 'sequence-window-controller
                             :sequence (cdr (ccl::bt.restarts context))
                             :result-callback #'(lambda (r)
                                                  (process-interrupt
                                                   process
                                                   #'invoke-restart-interactively
                                                   r))
                             :display #'(lambda (item stream)
                                          (let* ((ccl::*aux-vsp-ranges* vsp-range)
                                                 (ccl::*aux-tsp-ranges* tsp-range)
                                                 (ccl::*aux-csp-ranges* csp-range))
                                          (princ item stream)))
                             :title (format nil "Restarts for ~a(~d), break level ~d"
                                            (process-name process)
                                            (process-serial-number process)
                                            (ccl::bt.break-level context)))))))
                            
(objc:defmethod (#/restarts: :void) ((self hemlock-listener-document) sender)
  (let* ((process (hemlock-document-process self)))
    (when process
      (let* ((context (listener-backtrace-context process)))
        (when context
          (#/showWindow: (restarts-controller-for-context context) sender))))))

(objc:defmethod (#/continue: :void) ((self hemlock-listener-document) sender)
  (declare (ignore sender))
  (let* ((process (hemlock-document-process self)))
    (when process
      (let* ((context (listener-backtrace-context process)))
        (when context
          (process-interrupt process #'invoke-restart-interactively 'continue))))))






;;; Menu item action validation.  It'd be nice if we could distribute this a
;;; bit better, so that this method didn't have to change whenever a new
;;; action was implemented in this class.  For now, we have to do so.

(defmethod document-validate-menu-item ((doc hemlock-listener-document) item)
  ;; Return two values: the first is true if the second is definitive.
  ;; So far, all actions demand that there be an underlying process, so
  ;; check for that first.
  (let* ((process (hemlock-document-process doc)))
    (if process
      (let* ((action (#/action item)))
        (cond
          ((or (eql action (@selector #/revertDocumentToSaved:))
	       (eql action (@selector #/saveDocument:))
	       (eql action (@selector #/saveDocumentAs:)))
           (values t nil))
          ((eql action (@selector #/interrupt:)) (values t t))
          ((eql action (@selector #/continue:))
           (let* ((context (listener-backtrace-context process)))
             (values
              t
              (and context
                   (find 'continue (cdr (ccl::bt.restarts context))
                         :key #'restart-name)))))
          ((or (eql action (@selector #/backtrace:))
               (eql action (@selector #/exitBreak:))
               (eql action (@selector #/restarts:)))
           (values t
                   (not (null (listener-backtrace-context process)))))))
      (values nil nil))))

(objc:defmethod (#/validateMenuItem: :<BOOL>)
    ((self hemlock-listener-document) item)
  (multiple-value-bind (have-opinion opinion)
      (document-validate-menu-item self item)
    (if have-opinion
      opinion
      (call-next-method item))))

(defun shortest-package-name (package)
  (let* ((name (package-name package))
         (len (length name)))
    (dolist (nick (package-nicknames package) name)
      (let* ((nicklen (length nick)))
        (if (< nicklen len)
          (setq name nick len nicklen))))))

(defmethod ui-object-note-package ((app ns:ns-application) package)
  (let ((proc *current-process*)
        (name (shortest-package-name package)))
    (execute-in-gui #'(lambda ()
                        (dolist (buf hi::*buffer-list*)
                          (when (eq proc (buffer-process buf))
                            (setf (hi::variable-value 'hemlock::current-package :buffer buf) name)))))))


(defmethod eval-in-listener-process ((process cocoa-listener-process)
                                     string &key path package offset)
  (enqueue-toplevel-form (cocoa-listener-process-input-stream process) string
                         :package-name package :pathname path :offset offset))

;;; This is basically used to provide INPUT to the listener process, by
;;; writing to an fd which is connected to that process's standard
;;; input.
(defun hemlock-ext:send-string-to-listener (listener-buffer string)
  (let* ((process (buffer-process listener-buffer)))
    (unless process
      (error "No listener process found for ~s" listener-buffer))
    (enqueue-listener-input (cocoa-listener-process-input-stream process) string)))

(defmethod ui-object-choose-listener-for-selection ((app ns:ns-application)
						    selection)
  (declare (ignore selection))
  (#/performSelectorOnMainThread:withObject:waitUntilDone:
   (#/delegate *NSApp*)
   (@selector #/ensureListener:)
   +null-ptr+
   #$YES)
  (hemlock-document-process (#/topListener hemlock-listener-document)))

(defmethod ui-object-eval-selection ((app ns:ns-application)
				     selection)
  (let* ((target-listener (ui-object-choose-listener-for-selection
			   app selection)))
    (when target-listener
      (destructuring-bind (package path string &optional offset) selection
        (eval-in-listener-process target-listener string :package package :path path :offset offset)))))

(defmethod ui-object-load-buffer ((app ns:ns-application) selection)
  (let* ((target-listener (ui-object-choose-listener-for-selection app nil)))
    (when target-listener
      (destructuring-bind (package path) selection
        (let ((string (format nil "(load ~S)" path)))
          (eval-in-listener-process target-listener string :package package))))))

(defmethod ui-object-compile-buffer ((app ns:ns-application) selection)
  (let* ((target-listener (ui-object-choose-listener-for-selection app nil)))
    (when target-listener
      (destructuring-bind (package path) selection
        (let ((string (format nil "(compile-file ~S)" path)))
          (eval-in-listener-process target-listener string :package package))))))

(defmethod ui-object-compile-and-load-buffer ((app ns:ns-application) selection)
  (let* ((target-listener (ui-object-choose-listener-for-selection app nil)))
    (when target-listener
      (destructuring-bind (package path) selection
        (let ((string (format nil "(progn (compile-file ~S)(load ~S))" 
                              path
                              (make-pathname :directory (pathname-directory path)
                                             :name (pathname-name path)
                                             :type (pathname-type path)))))
          (eval-in-listener-process target-listener string :package package))))))

       
 
