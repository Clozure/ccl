(in-package :easygui)

;;; Helper types:

;;; point:
(defclass eg-point ()
     ((x :initarg :x :reader point-x)
      (y :initarg :y :reader point-y)))

(defun point (x y)
  (assert (>= x 0))
  (assert (>= y 0))
  (make-instance 'eg-point :x x :y y))

(defmethod print-object ((o eg-point) s)
  (print-unreadable-object (o s :identity nil :type t)
    (format s "(~,2,F/~,2,F)" (point-x o) (point-y o))))

;;; range:
(defclass eg-range ()
     ((start :initarg :start :reader range-start)
      (end :initarg :end :reader range-end)))

(defun range (start end)
  (assert (>= end start))
  (make-instance 'eg-range :start start :end end))

(defun range-nsrange (range)
  (ns:make-ns-range (range-start range) (range-end range)))

(defclass eg-rectangle ()
     ((x :initarg :x :reader rectangle-x)
      (y :initarg :y :reader rectangle-y)
      (width :initarg :width :reader rectangle-width)
      (height :initarg :height :reader rectangle-height)))

(defun rectangle (x y width height)
  (assert (>= x 0))
  (assert (>= y 0))
  (assert (>= width 0))
  (assert (>= height 0))
  (make-instance 'eg-rectangle :x x :y y :width width :height height))

(defun rectangle-nsrect (r)
  (ns:make-ns-rect (rectangle-x r) (rectangle-y r)
                   (rectangle-width r) (rectangle-height r)))

(defun nsrect-rectangle (r)
  (rectangle (ns:ns-rect-x r) (ns:ns-rect-y r)
             (ns:ns-rect-width r) (ns:ns-rect-height r)))

;;; Base class for all Cocoa-based Easygui objects:
(defclass easy-cocoa-object ()
     ((ref :initarg :cocoa-ref)
      (ref-valid-p :initform t :accessor cocoa-ref-valid-p)))

(defgeneric cocoa-ref (eg-object)
  (:method ((eg-object easy-cocoa-object))
     (if (cocoa-ref-valid-p eg-object)
         (slot-value eg-object 'ref)
         (error "Attempting to access an invalidated Cocoa object on ~A!"
                eg-object))))
  
(defgeneric (setf cocoa-ref) (new eg-object)
  (:method (new (eg-object easy-cocoa-object))
     (setf (cocoa-ref-valid-p eg-object) t
	   (slot-value eg-object 'ref) new)))

(defvar *window-position-default-x* 200)
(defvar *window-position-default-y* 200)
(defvar *window-size-default-x* 200)
(defvar *window-size-default-y* 200)

(defun ns-rect-from-points (posn size)
  (ns:make-ns-rect (point-x posn) (point-y posn)
                   (point-x size) (point-y size)))

(defparameter *flag-to-mask-alist*
              `( ;; (:zoomable-p . #$NSZoomableWindowMask) ; doesn't work
                (:minimizable-p . ,#$NSMiniaturizableWindowMask)
                (:resizable-p . ,#$NSResizableWindowMask)
                (:closable-p . ,#$NSClosableWindowMask)))

(defun flag-mask (keyword enabled-p)
  (if enabled-p
      (or (cdr (assoc keyword *flag-to-mask-alist*)) 0)
      0))

(defparameter *key-to-mask-alist*
              `((:control . ,#$NSControlKeyMask)
                (:alt     . ,#$NSAlternateKeyMask)
                (:command . ,#$NSCommandKeyMask)
                (:shift   . ,#$NSShiftKeyMask)))

(defun key-mask (keyword)
  (or (cdr (assoc keyword *key-to-mask-alist*)) 0))

(defvar *modifier-key-pattern* 0
"Bound to the description of modifier keys pressed at the time of a mouse-down
on a view with an action, also a mouse-down mouse-up mouse-enter or mouse-exit on
a view with an associated method, to permit interrogation using the functions
shift-key-p control-p-key alt-key-p.")

;;; Memory management helpers:

(defmacro maybe-invalidating-object ((eg-object) &body body)
  `(if (= 1 (#/retainCount (cocoa-ref ,eg-object)))
       (multiple-value-prog1 (progn ,@body)
                             (setf (cocoa-ref-valid-p ,eg-object) nil))
       (progn ,@body)))

(defmethod retain-object ((o easy-cocoa-object))
  (#/retain (cocoa-ref o)))

(defmethod release-object ((o easy-cocoa-object))
  (#/release (cocoa-ref o)))

(defmacro retaining-objects ((&rest eg-objects) &body body)
  "Retains EG-OBJECTS, runs BODY forms and releases them after control
has left BODY."
  (let ((objects (gensym)))
    `(let ((,objects (list ,@eg-objects)))
       (mapc #'retain-object ,objects)
       (unwind-protect (progn ,@body)
         (mapc #'release-object ,objects)))))

;;; debug macro for #/ funcalls:

(defvar *debug-cocoa-calls* nil)
;; Default changed to NIL by arthur, March 2009

(defparameter *cocoa-pause* nil
"When *debug-cocoa-calls* is not NIL, then a numeric value of *cocoa-pause* causes
some sleep after every message produced by the DCC macro. Useful if something is
causing a crash. During development it happened to me :-(")

(defmacro dcc (form)
;; Trace output identifies process, and may pause: arthur, March 2009
  `(progn
     (when *debug-cocoa-calls*
       (format *trace-output* "[~a]Calling ~A on ~S~%"
               (ccl::process-serial-number ccl::*current-process*) ',(first form) (list ,@(rest form)))
       (when (and *cocoa-pause* (numberp *cocoa-pause*)) (sleep *cocoa-pause*)))
     ,form))

;;; Running things on the main thread:

(defun run-on-main-thread (waitp thunk)
  (if waitp
    (execute-in-gui thunk)
    (queue-for-gui thunk)))

(defmacro running-on-main-thread ((&key (waitp t)) &body body)
  `(run-on-main-thread ,waitp (lambda () ,@body)))

;;; Getting views from objc objects:

(defgeneric easygui-view-of (cocoa-view))
