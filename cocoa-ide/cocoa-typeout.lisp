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
(export 'TYPEOUT-STREAM :GUI)

;;
;; a typeout window is just an ns-window containing a scroll-view
;; which contains a text-view. The text is read only.
;;
;; the window is implicitly bound to a stream, and text written to
;; the stream is written into the text-view object. The stream is 
;; available via the function (gui::typeout-stream)
;;

;; @class typeout-view
;;

(defparameter *default-line-break-mode* :char ":char or :word")

(defclass typeout-view (ns:ns-view)
  ((scroll-view :foreign-type :id :reader typeout-view-scroll-view)
   (text-view :foreign-type :id :reader typeout-view-text-view))
  (:metaclass ns:+ns-object))
(declaim (special typeout-view))

(defclass typeout-text-view (ns:ns-text-view)
    ()
  (:metaclass ns:+ns-object))
(declaim (special typeout-text-view))

(objc:defmethod (#/clearAll: :void) ((self typeout-text-view))
  (#/selectAll: self +null-ptr+)
  (#/delete: self +null-ptr+))

(objc:defmethod (#/insertString: :void) ((self typeout-text-view) text)
  (#/setEditable: self t)
  (#/insertText: self text)
  (#/setEditable: self nil))


(objc:defmethod #/initWithFrame: ((self typeout-view) (frame :<NSR>ect))
  (declare (special *default-font-name* *default-font-size*))
  (call-next-method frame)
  (let* ((scrollview (make-instance 'ns:ns-scroll-view
                                    :with-frame frame))
	 (scroll-content (#/contentView scrollview))) 
    (#/setBorderType: scrollview #$NSBezelBorder)
    (#/setHasVerticalScroller: scrollview t)
    (#/setHasHorizontalScroller: scrollview t)
    (#/setRulersVisible: scrollview nil)
    (#/setAutoresizingMask: scrollview (logior #$NSViewWidthSizable #$NSViewHeightSizable))
    (#/setAutoresizesSubviews: scroll-content t)
    (#/addSubview: self scrollview)
    (setf (slot-value self 'scroll-view) scrollview)
    (let* ((contentsize (#/contentSize scrollview)))
      (ns:with-ns-rect (text-frame 0 0 (ns:ns-size-width contentsize) (ns:ns-size-height contentsize))
        (let* ((text-view (make-instance 'typeout-text-view
                                         :with-frame text-frame)))
          (#/setEditable: text-view nil)
          (#/setHorizontallyResizable: text-view t)
          (#/setAutoresizingMask: text-view #$NSViewWidthSizable)
          (#/setTypingAttributes: text-view (create-text-attributes 
				  :font (default-font :name *default-font-name* :size *default-font-size*)
				  :line-break-mode *default-line-break-mode*))
          (#/setDocumentView: scrollview text-view)
          (ns:with-ns-size (container-size 1.0f7 1.0f7)
          (let* ((layout (#/layoutManager text-view))
                 (container (make-instance 'ns:ns-text-container
                                           :with-container-size container-size)))
            (#/setWidthTracksTextView: container t)
            (#/setHeightTracksTextView: container nil)
            (#/addTextContainer: layout container)))
        
          (setf (slot-value self 'text-view) text-view)))))
  self)

;;
;; @class typeout-panel
;;
(defloadvar *typeout-window* nil)

(defclass typeout-window (ns:ns-window)
  ((typeout-view :foreign-type :id :accessor typeout-window-typeout-view))
  (:metaclass ns:+ns-object))
(declaim (special typeout-window))

(defloadvar *typeout-windows* ())
(defstatic *typeout-windows-lock* (make-lock))

(defun get-typeout-window (title)
  (with-lock-grabbed (*typeout-windows-lock*)
    (when *typeout-windows*
      (let* ((w (pop *typeout-windows*)))
        (set-window-title w title)
        w))))

(objc:defmethod #/typeoutWindowWithTitle: ((self +typeout-window) title)
  (let* ((panel (new-cocoa-window :class self
                                  :title title
                                  :width 600
                                  :activate nil)))
    (#/setReleasedWhenClosed: panel nil)
    (let* ((view (make-instance 'typeout-view :with-frame (#/bounds (#/contentView panel)))))
      (#/setAutoresizingMask: view (logior
                                    #$NSViewWidthSizable
                                    #$NSViewHeightSizable))
      (#/setContentView: panel view)
      (#/setNeedsDisplay: view t)
      (setf (slot-value panel 'typeout-view) view)
      panel)))

(objc:defmethod #/sharedPanel ((self +typeout-window))
   (cond (*typeout-window*)
	 (t
          (setq *typeout-window* (#/typeoutWindowWithTitle: self #@"Typeout")))))


; NOTE: It's okay to send data to a closed window. Next time you
;   do it, it will reopen.
(objc:defmethod (#/close :void) ((self typeout-window))
  (call-next-method)
  (unless (eql self *typeout-window*)
    (with-lock-grabbed (*typeout-windows-lock*)
      (push (%inc-ptr self 0) *typeout-windows*))))

(defclass typeout-stream (fundamental-character-output-stream)
  ((string-stream :initform (make-string-output-stream))
   (window :initform (#/sharedPanel typeout-window) :initarg :window)))

(defun prepare-typeout-stream (stream)
  (declare (ignorable stream))
  (with-slots (window) stream
    (execute-in-gui (lambda ()
                      (unless (#/isVisible window)
                        (activate-window window))))))

;;;
;;;  TYPEOUT-STREAM methods
;;;

(defmethod stream-write-char ((stream typeout-stream) char)
  (prepare-typeout-stream stream)
  (write-char char (slot-value stream 'string-stream)))

(defmethod stream-write-string ((stream typeout-stream) string &optional (start 0) end)
  (prepare-typeout-stream stream)
  (write-string (if (and (eql start 0) (or (null end) (eql end (length string))))
		    string 
		    (subseq string start end))
		(slot-value stream 'string-stream)))

(defmethod stream-fresh-line ((stream typeout-stream))
  (prepare-typeout-stream stream)
  (fresh-line (slot-value stream 'string-stream)))

(defmethod stream-line-column ((stream typeout-stream))
  (stream-line-column (slot-value stream 'string-stream)))

(defmethod stream-clear-output ((stream typeout-stream))
  (prepare-typeout-stream stream)
  (let* ((window (slot-value stream 'window))
         (the-typeout-view (typeout-window-typeout-view window))
         (text-view (slot-value the-typeout-view 'text-view))
         (string-stream (slot-value stream 'string-stream)))
    (get-output-stream-string string-stream)
    (#/performSelectorOnMainThread:withObject:waitUntilDone:
     text-view
     (@selector #/clearAll:)
     +null-ptr+
     t)))

(defmethod stream-force-output ((stream typeout-stream))
  (let* ((window (slot-value stream 'window))
         (the-typeout-view (typeout-window-typeout-view window))
         (text-view (slot-value the-typeout-view 'text-view)))
    (#/performSelectorOnMainThread:withObject:waitUntilDone:
     text-view
     (@selector #/insertString:)
     (%make-nsstring (get-output-stream-string (slot-value stream 'string-stream))) 
     t)))
  
(defloadvar *typeout-stream* nil)

(defun make-typeout-stream (&rest args)
  (let* ((ts (apply #'make-instance 'typeout-stream args)))
    (with-slots (window) ts
      (add-auto-flush-stream ts)
      (execute-in-gui (lambda ()
                        (activate-window window))))
    ts))

(defun typeout-stream (&optional title)
  (if (null title)
    (or *typeout-stream*
        (setq *typeout-stream* (make-typeout-stream)))
    (make-typeout-stream :window (#/typeoutWindowWithTitle: typeout-window (%make-nsstring (format nil "~a" title))))))