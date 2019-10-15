;;;-*- Mode: Lisp; Package: WINDOW-PARKING -*-

;;; ----------------------------------------------------------------------------
;;; 
;;;      window-parking.lisp
;;;
;;;      copyright (c) 2009 Glen Foy
;;;      (Permission is granted to Clozure Associates to distribute this file.)
;;;
;;;      This code provides a Hemlock window manager and is part of the Context-Menu 
;;;      tool set.  See the ReadMe file for details.
;;;
;;;      This software is offered "as is", without warranty of any kind.
;;;
;;;      Mod History (most recent edit first)
;;;      9/17/9 Fix bogus move after #/saveDocument.
;;;      9/16/9 Park new window.
;;;      9/9/9  first cut
;;;
;;; ----------------------------------------------------------------------------


(defpackage "WINDOW-PARKING" (:nicknames "WP") (:use :cl :ccl))
(in-package "WINDOW-PARKING")

(require :context-menu-cm)
(require :list-definitions-cm)

(defparameter *window-parker* nil "The window-parker instance.")
(defparameter *window-parking-menu* nil "The window-parking-menu instance.")
(defParameter *park-p* t "To park or not to park.")

;;; ----------------------------------------------------------------------------
;;;
(defClass WINDOW-PARKING-MENU (ns:ns-menu) 
  ((tool-menu :initform nil :accessor tool-menu)
   (doc-path :initform (merge-pathnames ";ReadMe.rtf" cl-user::*window-parking-directory*) :reader doc-path))
  (:documentation "A menu for adding and deleting parking spots.")
  (:metaclass ns:+ns-object))

;;; This can be called to add a new parking spot or adjust an existing spot.
(objc:defmethod (#/defineAction: :void) ((m window-parking-menu) (sender :id))
  (declare (ignore sender))
  (let* ((window (cmenu:active-hemlock-window))
         (path (when window (cmenu:window-path window)))
         ;; Possibly a re-definition.
         (current-function-key (get-function-key *window-parker* window))
         (defined-function-key
             (when path
               (if current-function-key
                 (open-define-parking-spot-dialog path current-function-key)
                 (open-define-parking-spot-dialog path)))))
    (when defined-function-key
      (cond (current-function-key 
             (cond ((= current-function-key defined-function-key)
                    ;; Adjusting an existing spot.
                    (let ((spot (parking-spot-with-function-key *window-parker* current-function-key)))
                      (init-parking-spot-values spot window current-function-key))
                    (cmenu:echo-msg "Parking spot ~S modified." current-function-key))
                   (t
                    (vacate-current-location *window-parker* window)
                    (add-parking-spot *window-parker* window defined-function-key)
                    (cmenu:echo-msg "Parking spot ~S defined." current-function-key))))
            (t
             (add-parking-spot *window-parker* window defined-function-key))
            (cmenu:echo-msg "Parking spot ~S defined." defined-function-key)))))

(objc:defmethod (#/deleteAction: :void) ((m window-parking-menu) (sender :id))
  (declare (ignore sender))
  (let ((function-key (open-delete-parking-spot-dialog)))
    (when function-key
      (delete-parking-spot *window-parker* function-key))))

(objc:defmethod (#/update :void) ((m window-parking-menu))
  (cmenu:update-tool-menu m (tool-menu m))
  (call-next-method))

(defmethod initialize-instance :after ((m window-parking-menu) &key)
  (setf (tool-menu m) (cmenu:add-default-tool-menu m :doc-file (doc-path m)))
  (flet ((create-menu-item (name action)
           (let ((menu-item (make-instance 'ns:ns-menu-item))
                 (attributed-string (#/initWithString:attributes:
                                     (#/alloc ns:ns-attributed-string) 
                                     (ccl::%make-nsstring name)
                                     cmenu:*hemlock-menu-dictionary*)))
             (#/setAttributedTitle: menu-item attributed-string)
             (#/setAction: menu-item action)
             (#/setTarget: menu-item  m)
             (#/addItem: m menu-item))))
    (create-menu-item "Define Parking Spot..." 
                      (ccl::@selector "defineAction:"))
    (create-menu-item "Delete Parking Spot..." 
                      (ccl::@selector "deleteAction:"))))
  
(setq *window-parking-menu* (make-instance 'window-parking-menu))

(defun get-window-parking-menu (view event) 
  (declare (ignore view event))
  *window-parking-menu*)

(cmenu:register-tool "Window-Parking-CM" #'get-window-parking-menu)


;;; ----------------------------------------------------------------------------
;;;
(defclass PARKABLE-HEMLOCK-FRAME (gui::hemlock-frame)
  ((parked-p :initform nil :accessor parked-p)
   (front-p :initform nil :accessor front-p))
  (:metaclass ns:+ns-object))

(defmethod init-parking ((w parkable-hemlock-frame))
  (setf (parked-p w) nil)
  (setf (front-p w) nil))

(defmethod h-position ((w parkable-hemlock-frame))
  (let ((rect (#/frame w)))
    (pref rect :<NSR>ect.origin.x)))

(defmethod v-position ((w parkable-hemlock-frame))
  (let ((rect (#/frame w)))
    (pref rect :<NSR>ect.origin.y)))

(defmethod h-dimension ((w parkable-hemlock-frame))
  (let ((rect (#/frame w)))
    (pref rect :<NSR>ect.size.width)))

(defmethod v-dimension ((w parkable-hemlock-frame))
  (let ((rect (#/frame w)))
    (pref rect :<NSR>ect.size.height)))

(objc:defmethod (#/close :void) ((w parkable-hemlock-frame))
  (vacate-current-location *window-parker* w)
  (call-next-method))

(defmethod modified-p ((w parkable-hemlock-frame))
  (when w
    (let* ((pane (slot-value w 'gui::pane))
           (hemlock-view (when pane (gui::text-pane-hemlock-view pane)))
           (buffer (when hemlock-view (hi::hemlock-view-buffer hemlock-view))))
      (when buffer
        (hi::buffer-modified buffer)))))

(defmethod print-object ((w parkable-hemlock-frame) stream)
  (format stream "<parkable-hemlock-frame: ~S>" (namestring (cmenu:window-path w))))

;;; This is a work-around for some odd #/saveDocument behavior:
;;; Why is the frame being set on a save operation?
(objc:defmethod (#/saveDocument: :void) ((self gui::hemlock-editor-document) (sender :id))
  (let* ((url (#/fileURL self))
         (path (ccl::lisp-string-from-nsstring (#/path url)))
         (window (cmenu:window-with-path path)))
    (when window (init-parking window))
    (call-next-method sender)
    (when window (setf (parked-p window) t))))

;;; ----------------------------------------------------------------------------
;;; *** redefinition ***
;;; Need the equivalent of: (setf ccl::*default-editor-class* 'parkable-hemlock-frame)
(defun gui::new-hemlock-document-window (class)
  (let* ((w (gui::new-cocoa-window :class (if (or (eq class 'gui::hemlock-listener-frame)
                                                  (eq class (find-class 'gui::hemlock-listener-frame)))
                                            'gui::hemlock-listener-frame
                                            'parkable-hemlock-frame)
                                   :auto-display t
                                   :activate nil))
         (echo-area-height (+ 1 (gui::size-of-char-in-font gui::*editor-font*))))
      (values w (gui::add-pane-to-window w :reserve-below echo-area-height))))

(objc:defmethod (#/makeKeyAndOrderFront: :void) ((w parkable-hemlock-frame) (sender :id))
  (setf (front-p w) t)
  (call-next-method sender))

(objc:defmethod (#/setFrame:display: :void) ((w parkable-hemlock-frame) (rect :<NSR>ect) (display-p :<BOOL>))
 (cond ((parked-p w)
         (call-next-method rect display-p))
        (t
         (when (front-p w) (setf (parked-p w) t))
         (multiple-value-bind (h-position v-position h-dimension v-dimension)
                              (park *window-parker* w)
           (if (and h-position v-position h-dimension v-dimension)
             (ns:with-ns-rect (r h-position v-position h-dimension v-dimension)
               (call-next-method r display-p))
             (call-next-method rect display-p))))))

;;; ----------------------------------------------------------------------------
;;;
(defClass PARKING-SPOT ()
  ((h-dimension :initform nil :initarg :h-dimension :accessor ps-h-dimension)
   (v-dimension :initform nil :initarg :v-dimension :accessor ps-v-dimension)
   (h-position :initform nil :initarg :h-position :accessor ps-h-position)
   (v-position :initform nil :initarg :v-position :accessor ps-v-position)
   (tenant :initform nil :initarg :tenant :accessor ps-tenant)
   (function-key :initform nil :initarg :function-key :accessor ps-function-key))
  (:documentation "Parking spot position, size, tenant and function key information."))

(defMethod initialize-instance :after ((ps parking-spot) &key window 
                                       function-key h-dimension v-dimension
                                       h-position v-position)
  (cond ((and h-dimension v-dimension h-position v-position function-key)
         (setf (ps-tenant ps) window)
         (setf (ps-h-dimension ps) h-dimension)
         (setf (ps-v-dimension ps) v-dimension)
         (setf (ps-h-position ps) h-position)
         (setf (ps-v-position ps) v-position)
         (setf (ps-function-key ps) function-key))
        ((and window function-key)
         (init-parking-spot-values ps window function-key))
        (t
         (error "Bogus condition in parking-spot i-i :after"))))

(defMethod init-parking-spot-values ((ps parking-spot) window function-key)
  (setf (ps-tenant ps) window)
  (setf (ps-h-dimension ps) (h-dimension window))
  (setf (ps-v-dimension ps) (v-dimension window))
  (setf (ps-h-position ps) (h-position window))
  (setf (ps-v-position ps) (v-position window))
  (setf (ps-function-key ps) function-key))

(defMethod parking-spot-on-screen-p ((ps parking-spot) &optional window)
  (let* ((screen (if window 
                   (#/screen window)
                   (#/mainScreen ns:ns-screen)))
         (screen-rect (if (%null-ptr-p screen)
                        (#/visibleFrame (#/mainScreen ns:ns-screen))
                        (#/visibleFrame screen)))
         (screen-left (pref screen-rect :<NSR>ect.origin.x))
         (screen-right (+ screen-left (pref screen-rect :<NSR>ect.size.width)))
         (screen-bottom (pref screen-rect :<NSR>ect.origin.y))
         (screen-top (+ screen-bottom (pref screen-rect :<NSR>ect.size.height))))
    (and (>= (ps-h-position ps) screen-left)
         (<= (+ (ps-h-position ps) (ps-h-dimension ps)) screen-right)
         (>= (ps-v-position ps) screen-bottom)
         (<= (+ (ps-v-position ps) (ps-v-dimension ps)) screen-top))))

(defMethod print-object ((ps parking-spot) stream)
  (format stream "<~a ~a ~a>" (type-of ps) (ps-function-key ps)
          (if (ps-tenant ps) (ps-tenant ps) "empty")))

(defMethod apply-parking-spot-values ((ps parking-spot) window)
  (setf (ps-tenant ps) window)
  (when (or (neq (ps-h-dimension ps) (h-dimension window))
            (neq (ps-v-dimension ps) (v-dimension window))
            (neq (ps-h-position ps) (h-position window))
            (neq (ps-v-position ps) (v-position window)))
    ;; park it
    (init-parking window)
    (ns:with-ns-rect (r (ps-h-position ps) (ps-v-position ps) (ps-h-dimension ps) (ps-v-dimension ps))
      (#/setFrame:display: window r t))
    (#/makeKeyAndOrderFront: window nil))
  (let ((style-screen-function (find-symbol "STYLE-SCREEN" (find-package :sax))))
    (when style-screen-function
      (let* ((hemlock-view (gui::hemlock-view window))
             (text-view (gui::text-pane-text-view (hi::hemlock-view-pane hemlock-view))))
        (when text-view
          (funcall style-screen-function text-view))))))

;;; ----------------------------------------------------------------------------
;;;
(defClass WINDOW-PARKER ()
  ((parking-spots :initform nil :accessor wp-parking-spots)
   (parking-lot-path :initform (merge-pathnames ";Library;Preferences;org.clairvaux;window-parking;parking-lot" 
                                                 (hemlock::user-homedir-pathname))
                      :reader wp-parking-lot-path))
  (:documentation "A window manager."))

(setf *window-parker* (make-instance 'window-parker))

(defMethod park ((wp window-parker) (window parkable-hemlock-frame))
  (when (and (wp-parking-spots wp) *park-p*)
    ;; Already parked?
    (let* ((position (position window (wp-parking-spots wp) :key #'ps-tenant))
           spot)
      (when (null position)
        (or (setf position (get-empty-position wp))
            (setf position (bump-position wp (1- (length (wp-parking-spots wp)))))))
      (cond (position
             (setq spot (nth position (wp-parking-spots wp)))
             (move-position-to-front wp position)
             (setf (ps-tenant spot) window)
             (values (ps-h-position spot) (ps-v-position spot)
                     (ps-h-dimension spot) (ps-v-dimension spot)))
            (t
             ;; only try to park it once
             (setf (parked-p window) t))))))

;;; Test to make sure that POSITION is on screen.  If not, call recursively with
;;; (1- position).  Return POSITION or NIL
(defMethod bump-position ((wp window-parker) position)
  ;; Has the recursive call run out of positions?
  (when (< position 0)
    (cmenu:notify "There are no on-screen parking spots with unmodified buffers.")
    (return-from bump-position nil))
  (let* ((bump-location (nth position (wp-parking-spots wp)))
         (tenant (when bump-location (ps-tenant bump-location))))
    (cond ((and bump-location 
                (parking-spot-on-screen-p bump-location)
                (not (modified-p tenant)))
             (when tenant (#/close tenant))
             position)
          (t ; location is off-screen or not defined, recursive call
           (bump-position wp (1- position))))))

;;; Assumes that WINDOW's buffer is unmodified.
(defMethod bump-location-and-set-location-values ((wp window-parker) location window)
  (let ((tenant (ps-tenant location)))
    (when tenant
      (#/close tenant))
    (apply-parking-spot-values location window)))

(defMethod move-position-to-front ((wp window-parker) position)
  (let ((current-location (nth position (wp-parking-spots wp))))
    (setf (wp-parking-spots wp) 
          (cons current-location (delete current-location (wp-parking-spots wp))))))

(defMethod parking-spot-with-function-key ((wp window-parker) function-key)
  (find  function-key (wp-parking-spots wp) :test #'= :key #'ps-function-key))

;;; Find the lowest number parking-spot that has no tenant.
(defMethod get-empty-position ((wp window-parker))
  (let ((parking-spots (sort (copy-list (wp-parking-spots wp))
                             #'(lambda (s1 s2)
                                 (< (ps-function-key s1) (ps-function-key s2))))))
    (dolist (spot parking-spots)
      (when (and (null (ps-tenant spot))
                 (parking-spot-on-screen-p spot))
        ;; Return the position in the unsorted list. 
        (return (position spot (wp-parking-spots wp)))))))

(defMethod add-parking-spot ((wp window-parker) window function-key)
  (let ((new-parking-spot (make-instance 'parking-spot :window window :function-key function-key)))
    (setf (wp-parking-spots wp) (cons new-parking-spot (wp-parking-spots wp)))
    (cmenu:echo-msg "Parking Spot ~a defined." function-key)))

(defMethod add-parking-spot-2 ((wp window-parker) function-key
                               h-dimension v-dimension h-position v-position)
  (cond ((and (wp-parking-spots wp)
              (find-if #'(lambda (spot) (= function-key (ps-function-key spot)))
                       (wp-parking-spots wp)))
         (cmenu:notify "Duplicate parking-spot ignored."))
        (t
         (let ((new-parking-spot (make-instance 'parking-spot
                                   :function-key function-key
                                   :h-dimension h-dimension :v-dimension v-dimension
                                   :h-position h-position :v-position v-position)))
           (setf (wp-parking-spots wp) (cons new-parking-spot (wp-parking-spots wp)))))))

(defMethod delete-parking-spot ((wp window-parker) function-key)
  (let ((parking-spot (find function-key (wp-parking-spots wp) :key #'ps-function-key)))
    (cond (parking-spot
           (let ((tenant (ps-tenant parking-spot)))
             (cond (tenant
                    (cond ((modified-p tenant)
                           (cmenu:notify (format nil "First save: ~S.  Then try again."
                                                 (cmenu:window-path tenant))))
                          (t
                           (setf (wp-parking-spots wp) (delete parking-spot (wp-parking-spots wp)))  
                           (#/close tenant)
                           (cmenu:echo-msg "Parking Spot ~a deleted." function-key))))
                   (t
                    (setf (wp-parking-spots wp) (delete parking-spot (wp-parking-spots wp)))  
                    (cmenu:echo-msg "Parking Spot ~a deleted." function-key)))))                    
          (t 
           (cmenu:notify (format nil "Parking Spot ~a is not currently defined." function-key))))))

(defMethod get-function-key ((wp window-parker) window)
  (dolist (spot (wp-parking-spots wp))
    (when (eql window (ps-tenant spot)) (return (ps-function-key spot)))))

(defMethod vacate-current-location ((wp window-parker) window)
  (let ((location (find window (wp-parking-spots wp) :key #'ps-tenant)))
    (when location 
      (setf (ps-tenant location) nil)
      t)))

(defMethod clear-parking-lot ((wp window-parker))
  (setf (wp-parking-spots wp) nil))

;;; Move WINDOW to the parking-spot corresponding to the pressed function key,
;;; unless the parking-spot is not on screen or the window is already in that location.
(defMethod move-window-to-position ((wp window-parker) window function-key)
  (when *park-p*
    (let* ((parking-spot (find function-key (wp-parking-spots wp) :key #'ps-function-key))
           (tenant (when parking-spot (ps-tenant parking-spot))))
      (cond ((and parking-spot (parking-spot-on-screen-p parking-spot window))
             (cond (tenant
                    (cond ((eql window tenant)
                           (cmenu:echo-msg "Already in parking-spot ~a." function-key))
                          (t
                           (cond ((modified-p tenant)
                                  (cmenu:notify (format nil "First save: ~S. Then try again." 
                                                        (cmenu:window-path tenant)))
                                  (init-parking tenant))
                                 (t
                                  (vacate-current-location wp window)
                                  (bump-location-and-set-location-values wp parking-spot window)
                                  (#/makeKeyAndOrderFront: window nil)
                                  (cmenu:echo-msg "Moved to parking-spot ~a." function-key))))))
                   (t 
                    (vacate-current-location wp window)
                    (apply-parking-spot-values parking-spot window)
                    (#/makeKeyAndOrderFront: window nil)
                    (cmenu:echo-msg "Moved to parking-spot ~a." function-key))))
            (t
             (if (null parking-spot)
               (cmenu:notify (format nil "Parking-spot ~a is not defined." function-key))
               (cmenu:notify (format nil "Parking-spot ~a is off screen." function-key))))))))

;;; ----------------------------------------------------------------------------
;;; file I/O
;;;
(defMethod read-parking-spot-entries ((wp window-parker) stream)
  (let (length h-dimension v-dimension h-position v-position function-key input)
    (setf input (read stream nil :eof))
    (when (not (numberp input))
      (return-from read-parking-spot-entries))
    (setf length input)
    (dotimes (count length t)
      (setf input (read stream nil :eof))
      ;; *** null ?
      (when (not (or (numberp input) (null input))) (return nil))
      (setf function-key input)
      (setf input (read stream nil :eof))
      (when (not (or (numberp input) (null input))) (return nil))
      (setf h-dimension input)
      (setf input (read stream nil :eof))
      (when (not (or (numberp input) (null input))) (return nil))
      (setf v-dimension input)
      (setf input (read stream nil :eof))
      (when (not (or (numberp input) (null input))) (return nil))
      (setf h-position input)
      (setf input (read stream nil :eof))
      (when (not (or (numberp input) (null input))) (return nil))
      (setf v-position input)
      (add-parking-spot-2 wp function-key h-dimension v-dimension
                            h-position v-position))))

(defMethod write-parking-spot-entries ((wp window-parker) stream)
  (let (;; write the positions in reverse order based on their function key order
        (sorted-parking-spots (sort (copy-list (wp-parking-spots wp)) #'> :key #'ps-function-key)))
    (format stream "~s~%" (length sorted-parking-spots))
    (dolist (entry sorted-parking-spots)
      (format stream "~s~%" (ps-function-key entry))
      (format stream "~s~%" (ps-h-dimension entry))
      (format stream "~s~%" (ps-v-dimension entry))
      (format stream "~s~%" (ps-h-position entry)) 
      (format stream "~s~%" (ps-v-position entry)))))

(defun read-parking-lot-file ()
  "Read the parking-lot file."
  (let ((path (wp-parking-lot-path *window-parker*)))
    (when (probe-file path)
      (with-open-file (stream path :direction :input)
        (unless (read-parking-spot-entries *window-parker* stream)
          (cmenu:notify "There is a problem with the parking-lot file.  You will have to redefine your parking spots.")
          (clear-parking-lot *window-parker*))))))

(defun write-parking-lot-file (&rest args)
  "Writing function pushed into *lisp-cleanup-functions*."
  (declare (ignore args))
  (let ((path (wp-parking-lot-path *window-parker*)))
    (with-open-file (stream path :direction :output :if-exists :supersede)
      (write-parking-spot-entries *window-parker* stream))))

(pushnew 'write-parking-lot-file ccl::*lisp-cleanup-functions*)

;;; To Do:
;;; Heap issues involved in saving an image with the utility loaded.
;;; (pushnew 'read-parking-lot-file ccl::*lisp-startup-functions*)

;;; ----------------------------------------------------------------------------
;;; Commands and bindings:
;;;
(hemlock::defcommand "Move Window to Position 1" (p)
  "Move the front Hemlock window to parking spot 1."
  (declare (ignore p))
  (let ((window (cmenu:active-hemlock-window)))
    (cond (window
           (move-window-to-position *window-parker* window 1))
          (t
           (hi::editor-error "There is no active Hemlock window to move.")))))

(hi::bind-key "Move Window to Position 1" #k"F1")

(hemlock::defcommand "Move Window to Position 2" (p)
  "Move the front Hemlock window to parking spot 2."
  (declare (ignore p))
  (let ((window (cmenu:active-hemlock-window)))
    (cond (window
           (move-window-to-position *window-parker* window 2))
          (t
           (hi::editor-error "There is no active Hemlock window to move.")))))

(hi::bind-key "Move Window to Position 2" #k"F2")

(hemlock::defcommand "Move Window to Position 3" (p)
  "Move the front Hemlock window to parking spot 3."
  (declare (ignore p))
  (let ((window (cmenu:active-hemlock-window)))
    (cond (window
           (move-window-to-position *window-parker* window 3))
          (t
           (hi::editor-error "There is no active Hemlock window to move.")))))

(hi::bind-key "Move Window to Position 3" #k"F3")

(hemlock::defcommand "Move Window to Position 4" (p)
  "Move the front Hemlock window to parking spot 4."
  (declare (ignore p))
  (let ((window (cmenu:active-hemlock-window)))
    (cond (window
           (move-window-to-position *window-parker* window 4))
          (t
           (hi::editor-error "There is no active Hemlock window to move.")))))

(hi::bind-key "Move Window to Position 4" #k"F4")

(hemlock::defcommand "Move Window to Position 5" (p)
  "Move the front Hemlock window to parking spot 5."
  (declare (ignore p))
  (let ((window (cmenu:active-hemlock-window)))
    (cond (window
           (move-window-to-position *window-parker* window 5))
          (t
           (hi::editor-error "There is no active Hemlock window to move.")))))

(hi::bind-key "Move Window to Position 5" #k"F5")

(hemlock::defcommand "Move Window to Position 6" (p)
  "Move the front Hemlock window to parking spot 6."
  (declare (ignore p))
  (let ((window (cmenu:active-hemlock-window)))
    (cond (window
           (move-window-to-position *window-parker* window 6))
          (t
           (hi::editor-error "There is no active Hemlock window to move.")))))

(hi::bind-key "Move Window to Position 6" #k"F6")

(hemlock::defcommand "Move Window to Position 7" (p)
  "Move the front Hemlock window to parking spot 7."
  (declare (ignore p))
  (let ((window (cmenu:active-hemlock-window)))
    (cond (window
           (move-window-to-position *window-parker* window 7))
          (t
           (hi::editor-error "There is no active Hemlock window to move.")))))

(hi::bind-key "Move Window to Position 7" #k"F7")


(read-parking-lot-file)





