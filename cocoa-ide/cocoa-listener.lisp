;;;-*-Mode: LISP; Package: GUI -*-
;;;
;;; Copyright 2007 Clozure Associates
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

(in-package "GUI")

(def-cocoa-default *listener-input-font* :font #'(lambda ()
						   (#/fontWithName:size:
						    ns:ns-font
                                                    #+darwin-target
						    #@"Monaco"
                                                    #-darwin-target
                                                    #@"Courier New"
                                                    (font-size-kludge 10.0)))
		   "Default font for listener input")
(def-cocoa-default *listener-output-font* :font #'(lambda ()
						    (#/fontWithName:size:
						     ns:ns-font
                                                     #+darwin-target
						     #@"Monaco"
                                                     #-darwin-target
                                                     #@"Courier New"
                                                     (font-size-kludge 10.0)))
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



(defmethod queued-listener-char ((stream cocoa-listener-input-stream) wait-p dequeue-p)
  (with-slots (queue queue-lock read-lock queue-semaphore text-semaphore cur-string cur-string-pos) stream
    (with-lock-grabbed (read-lock)
      (or (with-lock-grabbed (queue-lock)
            (when (< cur-string-pos (length cur-string))
              (prog1 (aref cur-string cur-string-pos) (and dequeue-p (incf cur-string-pos)))))
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
                  (setf cur-string s cur-string-pos (if dequeue-p 1 0))
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
        (without-interrupts
         (let ((val (with-lock-grabbed (queue-lock) (pop queue))))
           (cond ((stringp val)
                  (assert (timed-wait-on-semaphore text-semaphore 0) () "text/queue mismatch!")
                  (setq cur-string val cur-string-pos 0))
                 (val
                  (destructuring-bind (string package-name pathname offset) val
                    ;; This env is used both for read and eval.
                    (let ((env (cons '(*loading-file-source-file* *load-pathname* *load-truename* *loading-toplevel-location*
                                                                  ccl::*nx-source-note-map*)
                                     (list pathname pathname (and pathname (or (probe-file pathname) pathname)) nil
                                           source-map))))
                      (when package-name
                        (let ((pkg-arg (ignore-errors (ccl::pkg-arg package-name))))
                          ; sometimes a form in a buffer contains fully-qualified symbols,
                          (when pkg-arg ; and such symbols don't require the buffer's package to be defined
                            (push '*package* (car env))
                            (push pkg-arg (cdr env)))))
                      (if source-map
                        (clrhash source-map)
                        (setf source-map (make-hash-table :test 'eq :shared nil)))
                      (setf cur-sstream (make-string-input-stream string) cur-env env cur-offset offset)))))))))))

(defmethod enqueue-toplevel-form ((stream cocoa-listener-input-stream) string &key package-name pathname offset)
  (with-slots (queue-lock queue queue-semaphore) stream
    (with-lock-grabbed (queue-lock)
      (setq queue (nconc queue (list (list string package-name pathname offset))))
      (signal-semaphore queue-semaphore))))

(defmethod enqueue-listener-input ((stream cocoa-listener-input-stream) string)
  (when (> (length string) 0)
    (with-slots (queue-lock queue queue-semaphore text-semaphore) stream
      (with-lock-grabbed (queue-lock)
        (setq queue (nconc queue (list string)))
        (signal-semaphore queue-semaphore)
        (signal-semaphore text-semaphore)))))

(defmethod stream-read-char-no-hang ((stream cocoa-listener-input-stream))
  (queued-listener-char stream nil t))

(defmethod stream-read-char ((stream cocoa-listener-input-stream))
  (queued-listener-char stream t t))

;; The default implementation of peek-char will lose the character if aborted. This won't.
(defmethod stream-peek-char ((stream cocoa-listener-input-stream))
  (queued-listener-char stream t nil))

(defmethod stream-listen ((stream cocoa-listener-input-stream))
  (queued-listener-char stream nil nil))

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

(defclass double-output-buffer ()
  ((flush-limit :initarg :flush-limit :accessor dob-flush-limit)
   (data :initarg :data :accessor dob-data)
   (other-data :initform nil :accessor dob-other-data)
   (output-data :initarg :output-data :accessor dob-output-data)
   (data-lock :initform (ccl::make-recursive-lock) :accessor dob-data-lock)
   (output-data-lock :initform (ccl::make-recursive-lock) :accessor dob-output-data-lock)
   (semaphore :initform (make-semaphore) :accessor dob-semaphore)))

(defun make-double-output-buffer (&optional (flush-limit $listener-flush-limit))
  (check-type flush-limit (integer 0))
  (flet ((make-buffer ()
	   (make-array (1+ flush-limit)
		       :adjustable t
		       :fill-pointer 0
		       :element-type 'character)))
    (let* ((data (make-buffer))
	   (output-data (make-buffer))
	   (res (make-instance 'double-output-buffer
			       :flush-limit flush-limit
			       :data data
			       :output-data output-data)))
      (dob-return-output-data res)
      res)))

(defmacro with-dob-data ((data dob) &body body)
  (let ((thunk (gensym "THUNK")))
    `(flet ((,thunk (,data)
	      ,@body))
       (declare (dynamic-extent #',thunk))
       (call-with-dob-data #',thunk ,dob))))

;; The GUI thread isn't allowed to print on a listener output-stream,
;; so ignore all attempts.
(defun call-with-dob-data (thunk dob)
  (unless (eq *current-process* *cocoa-event-process*)
    (with-lock-grabbed ((dob-data-lock dob))
      (funcall thunk (dob-data dob)))))

(defmacro with-dob-output-data ((data dob) &body body)
  (let ((thunk (gensym "THUNK")))
    `(flet ((,thunk (,data)
	      ,@body))
       (declare (dynamic-extent #',thunk))
       (call-with-dob-output-data #',thunk ,dob))))

(defun call-with-dob-output-data (thunk dob)
  (with-lock-grabbed ((dob-output-data-lock dob))
    (funcall thunk (dob-output-data dob))))

;; Should be called only in the GUI thread, except when
;; initializing a new double-output-buffer instance (or
;; debugging the semaphore wait code).
(defun dob-return-output-data (dob)
  (with-dob-output-data (output-data dob)
    (when output-data
      (setf (fill-pointer output-data) 0)
      (setf (dob-output-data dob) nil
	    (dob-other-data dob) output-data)
      (signal-semaphore (dob-semaphore dob))
      output-data)))

;; Must be called inside WITH-DOB-DATA
(defun dob-queue-output-data (dob &optional force)
  (unless (and (not force) (eql 0 (length (dob-data dob))))
    (wait-on-semaphore (dob-semaphore dob))
    (when (dob-other-data dob)
      (setf (dob-output-data dob) (dob-data dob)
	    (dob-data dob) (dob-other-data dob)
	    (dob-other-data dob) nil)
      t)))

;; True return means we overflowed the current buffer
(defun dob-push-char (dob char)
  (with-dob-data (data dob)
    (when (>= (vector-push-extend char data) (dob-flush-limit dob))
      (dob-queue-output-data dob t)
      t)))

(defclass cocoa-listener-output-stream (fundamental-character-output-stream)
  ((buffer :initform (make-double-output-buffer $listener-flush-limit))
   (hemlock-view :initarg :hemlock-view)))

(defmethod stream-element-type ((stream cocoa-listener-output-stream))
  (with-slots (buffer) stream
    (array-element-type (dob-data buffer))))

(defun display-cocoa-listener-output-buffer (stream)
  (with-slots (hemlock-view buffer) stream
    (unwind-protect
	 (with-dob-output-data (data buffer)
	   (when (> (fill-pointer data) 0)
	     (append-output hemlock-view data)
	     (setf (fill-pointer data) 0)))
      (dob-return-output-data buffer))))

(defmethod ccl:stream-write-char ((stream cocoa-listener-output-stream) char)
  (with-slots (buffer) stream
    (when (dob-push-char buffer char)
      (queue-for-gui
       (lambda () (display-cocoa-listener-output-buffer stream))))))

;; This isn't really thread safe, but it's not too bad...  I'll take a chance - trying
;; to get it to execute in the gui thread is too deadlock-prone.
(defmethod hemlock-listener-output-mark-column ((view hi::hemlock-view))
  (let* ((output-region (hi::variable-value 'hemlock::current-output-font-region
					    :buffer (hi::hemlock-view-buffer view))))
    (hi::mark-charpos (hi::region-end output-region))))

;; TODO: doesn't do the right thing for embedded tabs (in buffer or data)
(defmethod ccl:stream-line-column ((stream cocoa-listener-output-stream))
  (with-slots (hemlock-view buffer) stream
    (with-dob-data (data buffer)
      (let* ((n (length data))
             (pos (position #\Newline data :from-end t)))
        (if pos
	    (- n pos 1)
	    (with-dob-output-data (output-data buffer)
	      (let* ((output-n (if output-data (length output-data) 0))
		     (output-pos (and (> output-n 0)
				      (position #\Newline output-data :from-end t))))
		(if output-pos
		    (+ n (- output-n output-pos 1))
		    (+ (hemlock-listener-output-mark-column hemlock-view)
		       n output-n)))))))))

(defmethod ccl:stream-fresh-line ((stream cocoa-listener-output-stream))
  (unless (eql 0 (ccl:stream-line-column stream))
    (ccl:stream-write-char stream #\Newline)))

(defmethod ccl::stream-finish-output ((stream cocoa-listener-output-stream))
  (stream-force-output stream))

(defmethod ccl:stream-force-output ((stream cocoa-listener-output-stream))
  (if (typep *current-process* 'appkit-process)
    (display-cocoa-listener-output-buffer stream)
    (with-slots (buffer) stream
      (with-dob-data (data buffer)
	data
	(when (dob-queue-output-data buffer)
	  (queue-for-gui #'(lambda () (stream-force-output stream))))))))

(defmethod ccl:stream-clear-output ((stream cocoa-listener-output-stream))
  (with-slots (buffer) stream
    (with-dob-data (data buffer)
      (setf (fill-pointer data) 0))))

(defmethod ccl:stream-line-length ((stream cocoa-listener-output-stream))
  (with-slots (hemlock-view) stream
    (values (hemlock-view-size hemlock-view))))

(defloadvar *cocoa-listener-count* 0)

(defclass cocoa-listener-process (process)
    ((input-stream :initarg :listener-input-stream :reader cocoa-listener-process-input-stream)
     (output-stream :initarg :listener-output-stream :reader cocoa-listener-process-output-stream)
     (backtrace-contexts :initform nil
                         :accessor cocoa-listener-process-backtrace-contexts)
     (window :initarg :listener-window :initform nil :reader cocoa-listener-process-window)))
  
(defloadvar *first-listener* t)

(defun new-cocoa-listener-process (procname window &key (class 'cocoa-listener-process)
                                                        (initial-function 'ccl::listener-function)
                                                        initargs)
  (declare (special *standalone-cocoa-ide*))
  (let* ((input-stream (make-instance 'cocoa-listener-input-stream))
         (output-stream (make-instance 'cocoa-listener-output-stream
                          :hemlock-view (hemlock-view window))))
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
                           (cocoa-close doc nil)))))
                 hi:*buffer-list*))
     :initial-function
     #'(lambda ()
         (setq ccl::*listener-autorelease-pool* (create-autorelease-pool))
         (when (and *standalone-cocoa-ide*
                    (prog1 *first-listener* (setq *first-listener* nil)))
           (ccl::startup-ccl (ccl::application-init-file ccl::*application*))
           (ui-object-note-package *nsapp* *package*))
         (funcall initial-function))
     :echoing nil
     :class class
     :initargs `(:listener-input-stream ,input-stream
                 :listener-output-stream ,output-stream
                 :listener-window ,window
                 ,@initargs))))
  
(defclass hemlock-listener-frame (hemlock-frame)
    ()
  (:metaclass ns:+ns-object))
(declaim (special hemlock-listener-frame))

(objc:defmethod (#/setDocumentEdited: :void) ((w hemlock-listener-frame)
                                              (edited #>BOOL))
  (declare (ignorable edited)))

(defun window-type (w)
  (type-of w))

(defun maybe-close-all-windows-of-this-class (sender)
  "Maybe forcibly close all windows of same class as window of current event, if any, and
  if the option key was pressed. Returns t if we forcibly 'closed' window(s) here
  (for some definition of 'close'), nil otherwise."
  (let* ((event (#/currentEvent *nsapp*))
         (modifiers (#/modifierFlags event))
         (original-window (#/window event)))
    (when (logtest #$NSAlternateKeyMask modifiers) ; check for option key pressed
      (unless (%null-ptr-p original-window)
        ;(format t "~%About to call map-windows for sender ~S" sender)
        (map-windows #'(lambda (w) (when (and (eq (window-type w)
                                                  (window-type original-window))
                                              (not (#/isDocumentEdited w)))
                                     (do-close-window w sender))))
        ;(print "Called map-windows")
        )
      t)))

(defmethod do-close-window ((w hemlock-listener-frame) sender)
  "Forcibly 'close' this window, with some definition of 'close' that's right for this window class."
  (let* ((doc (#/document (#/windowController w))))
    (if (or (%null-ptr-p doc)
            (null (hemlock-document-process doc)) 
            (perform-close-kills-process-p doc))
      (#/close w)
      (progn
        (#/orderOut: w sender)
        ;(#/close w)
        )))
  nil ; tell system we already closed it
  )

(defmethod do-close-window ((w ns:ns-window) sender)
  "Maybe forcibly 'close' this window, with some definition of 'close' that's right for this window class.
  Return nil if we should let the system close it instead."
  (declare (ignore sender))
  (#/close w)
  nil ; tell system we already closed it
  )

; This is a mess, but a necessary mess.
; The reason this should ALWAYS return nil is because of the case where the option key is held
; down and a window's close box is clicked. The proper behavior for a Lisp IDE is for all windows of
; the same class as the first one to close. But we can't use the #/windowShouldClose mechanism for
; this, because we have to somehow record the window class of the window that triggered the event in
; the first place, and after that window is closed, we cannot then refer back to find out what kind
; it was. And since I didn't want to introduce global variables, this is the next best thing:
; When the option key is held down, forcibly close all the windows of the same class as the first,
;   and tell #/windowShouldClose to always return nil so the system won't ever try to actually
;   close anything. Note that the system does call #/windowShouldClose once for every window that's
;   still open anyway. The first call to #/windowShouldClose establishes whether the option key
;   was held down and if it was, it takes control away from the system and just closes the right
;   windows then and there. If the option key was NOT held down, theoretically we could just return
;   T and the system would take care of it, but because the 'closing' method on hemlock-listener-frames
;   is complicated, we can't do this either. So we have complete lisp control over all window closing.

; Well, almost.
; There's still a slight bug when
;  -- You have multiple open Hemlock windows, AND
;  -- One or more of those windows contains modified but as-yet-unsaved content, and
;  -- You option-close a listener window.
; In that case, you'll get a "Do you want to save" dialog panel for the unsaved Hemlock window,
;   when you shouldn't because you aren't trying to close Hemlock windows--you're trying to close
;   listeners. But I haven't figured out a way to prevent this panel from showing up. Since it shows
;   up BEFORE #/windowShouldClose is called, that function cannot stop it. It's a benign bug because
;   it gives the user a chance to save unsaved files. And it's a rare case (because it's not common
;   to have a bunch of listener windows that you want to close all at once anyway).
;   But it's kind of annoying when it happens.

(objc:defmethod (#/windowShouldClose: #>BOOL) ((w ns:ns-window)
                                               sender)
  (declare (ignorable sender))
  ;(format t "~%In #/windowShouldClose for ~S" (or (window-pathname w) (class-of w) ))
  (if (maybe-close-all-windows-of-this-class sender)
    nil ; because maybe-close-all-windows-of-this-class already closed all that needed closing
    (do-close-window w sender)))

(defclass hemlock-listener-window-controller (hemlock-editor-window-controller)
    ()
  (:metaclass ns:+ns-object)
  )
(declaim (special hemlock-listener-window-controller))

;;; Listener documents are never (or always) edited.  Don't cause their
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
          (let* ((bufname (%make-nsstring bufname))
                 (seq (slot-value self 'sequence)))
            (if (zerop seq)
              bufname
              (#/stringWithFormat: ns:ns-string #@"%@ <%d>" bufname seq)))
          (call-next-method name))))))


;;; The HemlockListenerDocument class.


(defclass hemlock-listener-document (hemlock-editor-document)
  ((process :reader %hemlock-document-process :writer (setf hemlock-document-process) :initform nil))
  (:metaclass ns:+ns-object))
(declaim (special hemlock-listener-document))

(defgeneric hemlock-document-process (doc)
  (:method ((unknown t)) nil)
  (:method ((doc hemlock-listener-document)) (%hemlock-document-process doc)))

;; Nowadays this is nil except for listeners.
(defun buffer-process (buffer)
  (hemlock-document-process (hi::buffer-document buffer)))

(defmethod update-buffer-package ((doc hemlock-listener-document))
  nil)

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
  (let* ((w (car (active-listener-windows))))
    (if w
      (#/document (#/windowController w))
      +null-ptr+)))

(defun top-listener-document ()
  (let* ((doc (#/topListener hemlock-listener-document)))
    (unless (%null-ptr-p doc) doc)))

(defun top-listener-process ()
  (let* ((doc (#/topListener hemlock-listener-document)))
    (unless (%null-ptr-p doc)
      (hemlock-document-process doc))))


(defun symbol-value-in-top-listener-process (symbol)
  (let* ((process (top-listener-process)))
     (if process
       (ignore-errors (symbol-value-in-process symbol process))
       (values nil t))))
  
(defun hemlock-ext:top-listener-output-stream ()
  (let* ((process (top-listener-process)))
    (when process
      (setq process (require-type process 'cocoa-listener-process))
      (cocoa-listener-process-output-stream process))))

(defun hemlock-ext:top-listener-input-stream ()
  (let* ((process (top-listener-process)))
    (when process
      (setq process (require-type process 'cocoa-listener-process))
      (cocoa-listener-process-input-stream process))))



(objc:defmethod (#/isDocumentEdited :<BOOL>) ((self hemlock-listener-document))
  nil)

(defun listener-window-count ()
  (let ((count 0)
        (all-windows (#/windows *NSApp*)))
    (dotimes (i (#/count all-windows) count)
      (let* ((w (#/objectAtIndex: all-windows i))
             (wc (#/windowController w)))
        (when (typep wc 'hemlock-listener-window-controller)
          (incf count))))))

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

(def-cocoa-default *initial-listener-x-pos* :float 100.0f0 "X position of upper-left corner of initial listener")

(def-cocoa-default *initial-listener-y-pos* :float 100.0f0 "Y position of upper-left corner of initial listener")

(defloadvar *next-listener-x-pos* nil) ; set after defaults initialized
(defloadvar *next-listener-y-pos* nil) ; likewise

(objc:defmethod (#/dealloc :void) ((self hemlock-listener-document))
  (when (zerop (listener-window-count))
    (setq *next-listener-x-pos* nil
          *next-listener-y-pos* nil
          *cocoa-listener-count* 0))
  (let* ((p (hemlock-document-process self)))
    (when p
      (setf (hemlock-document-process self) nil)
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
    (#/setDelegate: (text-pane-text-view (slot-value window 'pane)) controller)
    (setf (slot-value controller 'sequence)
          (slot-value self 'dupcount))
    (#/setShouldCascadeWindows: controller nil)
    (#/addWindowController: self controller)
    (#/release controller)
    (unless (hemlock-document-process self)
      (setf (hemlock-document-process self)
            (new-cocoa-listener-process listener-name window)))
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
  (or (backtrace-context-restarts-window context)
      (setf (backtrace-context-restarts-window context) (restarts-dialog context))))

(defmethod restarts-dialog ((context vector))
  (let* ((tcr (ccl::bt.tcr context))
         (tsp-range (ccl::make-tsp-stack-range tcr context))
         (vsp-range (ccl::make-vsp-stack-range tcr context))
         (csp-range (ccl::make-csp-stack-range tcr context))
         (process (ccl::tcr->process tcr)))
    (make-instance 'sequence-window-controller
      :sequence (cdr (ccl::bt.restarts context))
      :before-close-function #'(lambda (wc)
                                 (declare (ignore wc))
                                 (setf (car (ccl::bt.restarts context)) nil))
      :result-callback #'(lambda (r)
                           (execute-in-gui #'(lambda ()
                                               (#/close (car (ccl::bt.restarts context)))))
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
                     (ccl::backtrace-context-break-level context)))))

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
                   (ccl::backtrace-context-continuable-p context)))))
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

(defmethod perform-close-kills-process-p ((self hemlock-listener-document))
  t)

(defmethod ui-object-note-package ((app ns:ns-application) package)
  (let ((proc *current-process*))
    (execute-in-gui #'(lambda ()
                        (dolist (buf hi::*buffer-list*)
                          (when (eq proc (buffer-process buf))
                            (let ((hi::*current-buffer* buf))
                              (hemlock:update-current-package package))))))))


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
  (top-listener-process))

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
        (let ((string (format nil "(cl:load ~S)" path)))
          (eval-in-listener-process target-listener string :package package))))))

(defmethod ui-object-compile-buffer ((app ns:ns-application) selection)
  (let* ((target-listener (ui-object-choose-listener-for-selection app nil)))
    (when target-listener
      (destructuring-bind (package path) selection
        (let ((string (format nil "(cl:compile-file ~S)" path)))
          (eval-in-listener-process target-listener string :package package))))))

(defmethod ui-object-compile-and-load-buffer ((app ns:ns-application) selection)
  (let* ((target-listener (ui-object-choose-listener-for-selection app nil)))
    (when target-listener
      (destructuring-bind (package path) selection
        (let ((string (format nil "(cl:progn (cl:compile-file ~S) (cl:load ~S))" 
                              path
                              (make-pathname :directory (pathname-directory path)
                                             :name (pathname-name path)
                                             :type (pathname-type path)))))
          (eval-in-listener-process target-listener string :package package))))))

       
;;; Support for background processes that acquire listener window/document/
;;; buffer infrastructure iff they try to do I/O to *TERMINAL-IO*.

(defclass hemlock-background-listener-document (hemlock-listener-document)
    ()
  (:metaclass ns:+ns-object))

(defmethod perform-close-kills-process-p ((self hemlock-background-listener-document))
  nil)

(defstruct deferred-cocoa-listener-stream-info
  real-input-stream
  real-output-stream
  process
  window)

    
(defclass deferred-cocoa-listener-stream (fundamental-character-stream)
    ((info :initarg :info :accessor deferred-cocoa-listener-stream-info)))

(defmethod ensure-deferred-stream-info-for-io ((s deferred-cocoa-listener-stream))
  (let* ((info (slot-value s 'info)))
    (when info
      (unless (deferred-cocoa-listener-stream-info-window info)
        (with-autorelease-pool
            (let* ((doc (execute-in-gui (lambda () (make-instance 'hemlock-background-listener-document))))
                   (buffer (hemlock-buffer doc))
                   (process (deferred-cocoa-listener-stream-info-process info)))
              (setf (hi::buffer-name buffer)
                    (format nil "~a(~d)" (process-name process) (process-serial-number process))
                    (hemlock-document-process doc) process)
              (execute-in-gui (lambda () (#/makeWindowControllers doc)))
              (let* ((wc (#/lastObject (#/windowControllers doc)))
                     (window (#/window wc)))
                (setf
                 (deferred-cocoa-listener-stream-info-real-input-stream info)
                 (make-instance 'cocoa-listener-input-stream)
                 (deferred-cocoa-listener-stream-info-real-output-stream info)
                 (make-instance 'cocoa-listener-output-stream
                                :hemlock-view (hemlock-view window))
                 (deferred-cocoa-listener-stream-info-window info)
                 window
                 (slot-value process 'window) window)
                (ui-object-note-package *nsapp* *package*))))))
    info))
                
                      

(defclass deferred-cocoa-listener-output-stream
          (fundamental-character-output-stream deferred-cocoa-listener-stream)
    ())

(defmethod stream-element-type ((s deferred-cocoa-listener-output-stream))
  'character)


(defmethod underlying-output-stream ((s deferred-cocoa-listener-output-stream))
  (let* ((info (ensure-deferred-stream-info-for-io s)))
    (if info
      (progn
        (let* ((window (deferred-cocoa-listener-stream-info-window info)))
          (unless (#/isVisible window)
            (execute-in-gui
             (lambda ()
               (#/makeKeyAndOrderFront: window (%null-ptr)))))
          (deferred-cocoa-listener-stream-info-real-output-stream info)))
      (ccl::stream-is-closed s))))

(defmethod ccl:stream-write-char ((s deferred-cocoa-listener-output-stream)
                                  char)
  (with-autorelease-pool
      (stream-write-char (underlying-output-stream s) char)))

(defmethod ccl:stream-line-column ((s deferred-cocoa-listener-output-stream))
  (stream-line-column (underlying-output-stream s)))

(defmethod ccl:stream-fresh-line ((s deferred-cocoa-listener-output-stream))
  (stream-fresh-line (underlying-output-stream s)))

(defmethod ccl::stream-finish-output ((s deferred-cocoa-listener-output-stream))
  (stream-force-output s))

(defmethod ccl:stream-force-output ((s deferred-cocoa-listener-output-stream))
  (let* ((info (slot-value s 'info)))
    (if info
      (let* ((out (deferred-cocoa-listener-stream-info-real-output-stream info)))
        (if out
          (stream-force-output out)))
      (ccl::stream-is-closed s))))

(defmethod ccl:stream-clear-output ((s deferred-cocoa-listener-output-stream))
  (stream-clear-output (underlying-output-stream s)))

(defmethod ccl:stream-line-length ((s deferred-cocoa-listener-output-stream))
  (stream-line-length (underlying-output-stream s)))

(defmethod close ((s deferred-cocoa-listener-output-stream)
                  &key abort)
  (let* ((info (slot-value s 'info)))
    (when info
      (let* ((out (deferred-cocoa-listener-stream-info-real-output-stream info)))
        (when out
          (stream-force-output out)
          (close out :abort abort)))
      (setf (slot-value s 'info) nil)
      t)))
          

(defclass deferred-cocoa-listener-input-stream
          (fundamental-character-input-stream deferred-cocoa-listener-stream)
    ((reading-line :initform nil :accessor hi:input-stream-reading-line)))


(defmethod underlying-input-stream ((s deferred-cocoa-listener-input-stream))
  (let* ((info (ensure-deferred-stream-info-for-io s)))
    (if info
      (progn
        (let* ((window (deferred-cocoa-listener-stream-info-window info)))
          (unless (#/isVisible window)
            (execute-in-gui
             (lambda ()
               (#/makeKeyAndOrderFront: window (%null-ptr)))))
          (deferred-cocoa-listener-stream-info-real-input-stream info)))
      (ccl::stream-is-closed s))))

(defmethod interactive-stream-p ((s deferred-cocoa-listener-input-stream))
  t)

(defmethod ccl::read-toplevel-form ((s deferred-cocoa-listener-input-stream)
                                    &key eof-value)
  (ccl::read-toplevel-form (underlying-input-stream s) :eof-value eof-value))

(defmethod enqueue-toplevel-form ((s deferred-cocoa-listener-input-stream) string &rest args &key &allow-other-keys)
  (apply #'enqueue-toplevel-form (underlying-input-stream s) string args))

(defmethod enqueue-listener-input ((s deferred-cocoa-listener-input-stream) string)
  (enqueue-listener-input (underlying-input-stream s) string))

(defmethod stream-read-char-no-hang ((s deferred-cocoa-listener-input-stream))
  (stream-read-char-no-hang (underlying-input-stream s)))

(defmethod stream-read-char ((s deferred-cocoa-listener-input-stream))
  (stream-read-char (underlying-input-stream s)))

(defmethod stream-unread-char ((s deferred-cocoa-listener-input-stream) char)
  (stream-unread-char (underlying-input-stream s) char))

(defmethod stream-clear-input ((s deferred-cocoa-listener-input-stream))
  (stream-clear-input (underlying-input-stream s)))

(defmethod stream-read-line ((s deferred-cocoa-listener-input-stream))
  (let* ((old-reading-line (hi:input-stream-reading-line s)))
    (unwind-protect
         (progn
           (setf (hi::input-stream-reading-line s) t)
           (stream-read-line (underlying-input-stream s)))
      (setf (hi:input-stream-reading-line s) old-reading-line))))

(defclass background-cocoa-listener-process (cocoa-listener-process)
    ())

(defun background-process-run-function (keywords function)
  (unless (listp keywords)
    (setf keywords (list :name keywords)))
  (destructuring-bind (&key (name "Anonymous")
                            (priority  0)
			    (stack-size ccl::*default-control-stack-size*)
			    (vstack-size ccl::*default-value-stack-size*)
			    (tstack-size ccl::*default-temp-stack-size*)
			    (initial-bindings ())
                            (persistent nil)
			    (use-standard-initial-bindings t)
                            (termination-semaphore nil)
                            (allocation-quantum (default-allocation-quantum)))
                      keywords
    (setq priority (require-type priority 'fixnum))
    (let* ((process (make-process name
                                  :class 'background-cocoa-listener-process
                                  :priority priority
                                  :stack-size stack-size
				  :vstack-size vstack-size
				  :tstack-size tstack-size
                                  :persistent persistent
				  :use-standard-initial-bindings use-standard-initial-bindings
				  :initial-bindings initial-bindings
                                  :termination-semaphore termination-semaphore
                                  :allocation-quantum allocation-quantum))
           (info (make-deferred-cocoa-listener-stream-info :process process))
           (input-stream (make-instance 'deferred-cocoa-listener-input-stream
                           :info info))
           (output-stream (make-instance 'deferred-cocoa-listener-output-stream
                            :info info)))
      (setf (slot-value process 'input-stream) input-stream
            (slot-value process 'output-stream) output-stream)
      (process-preset process
                      (lambda ()
                        (let* ((*terminal-io* (make-two-way-stream input-stream output-stream)))
                          (ccl::add-auto-flush-stream output-stream)
                          (unwind-protect
                              (funcall function)
                            (remove-auto-flush-stream output-stream)
                            (let* ((w (slot-value process 'window)))
                              (when w
                                (let* ((doc (#/document w)))
                                  (unless (%null-ptr-p doc)
                                    (when (eq *current-process*
                                              (hemlock-document-process doc))
                                      (setf (hemlock-document-process doc) nil))))
                                (cond ((#/isVisible w)
                                       (format output-stream "~%~%{process ~s exiting}~%" *current-process*))
                                      (t
                                       (cocoa-close w t)))
                                (close input-stream)
                                (close output-stream)))))))
      (process-enable process))))
