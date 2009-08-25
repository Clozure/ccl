(in-package :cl-user)


(defparameter light0 nil)
(defparameter light0-pos (make-array 3 :initial-contents '(5.0 3.0 0.0) ;; default to distant light source
                                     :element-type 'single-float))
(defparameter diffuse0 (make-array 4 :initial-contents '(0.0 0.0 0.0 1.0)
                                   :element-type 'single-float))
(defparameter ambient0 (make-array 4 :initial-contents '(1.0 1.0 1.0 1.0)
                                   :element-type 'single-float))
(defparameter specular0 (make-array 4 :initial-contents '(0.0 0.0 0.0 1.0)
                                   :element-type 'single-float))

(defparameter global-ambient (make-array 4 :initial-contents '(1.0 1.0 1.0 1.0) :element-type 'single-float)) ;; really really dim grey light

(defclass rubix-opengl-view (ns:ns-opengl-view)
  ()
  (:metaclass ns:+ns-object))

(objc:defmethod (#/prepareOpenGL :void) ((self rubix-opengl-view))
  (declare (special *the-origin* *y-axis*))
  (opengl:with-matrix-mode (#$GL_PROJECTION) ;; default is GL_MODELVIEW
    (#_glLoadIdentity)
    (#_glFrustum -0.6d0 0.6d0 -0.6d0 0.6d0 10.0d0 20.0d0))
  (#_glLoadIdentity)
  (mylookat *camera-pos* *the-origin* *y-axis*)

  (#_glShadeModel #$GL_SMOOTH)
  (#_glClearColor 0.05 0.05 0.05 0.0)
  ;; these next three are all needed to enable the z-buffer
  (#_glClearDepth 1.0d0)
  (#_glEnable #$GL_DEPTH_TEST)
  (#_glDepthFunc #$GL_LEQUAL)
  (#_glHint #$GL_PERSPECTIVE_CORRECTION_HINT #$GL_NICEST)

  (setf *cube* (make-instance 'rubix-cube))

  (#_glEnable #$GL_LIGHTING)

  (setf light0 (make-instance 'light :lightid #$GL_LIGHT0))
  (setpointsource light0 t)
  (setlocation light0 light0-pos)
  (setdiffuse light0 diffuse0)
  (setambient light0 ambient0)
  (setspecular light0 specular0)
  (on light0)

  (ccl::%stack-block ((foreign-float-vector (* 4 4))) ; make room for 4 single-floats
    (ccl::%copy-ivector-to-ptr global-ambient ; source
      0     ; offset to first element (alignment padding)
      foreign-float-vector ; destination
      0                    ; byte offset in destination
      (* 4 4))             ; number of bytes to copy
    (#_glLightModelfv #$GL_LIGHT_MODEL_AMBIENT foreign-float-vector)) ;; <- coersion issue

  (#_glFlush))

(objc:defmethod (#/drawRect: :void) ((self rubix-opengl-view) (a-rect :ns-rect))
  (declare (ignorable a-rect))
  ;; drawing callback
  (#_glClear (logior #$GL_COLOR_BUFFER_BIT #$GL_DEPTH_BUFFER_BIT))
  (render *cube*)
  (#_glFlush))

;; want to be able to send keystrokes to the rubix cube
#+ignore
(objc:defmethod (#/acceptsFirstResponder :<BOOL>) ((self rubix-opengl-view))
  t)

;; want to be able to click and start dragging (without moving the window)
(objc:defmethod (#/acceptsFirstMouse: :<BOOL>) ((self rubix-opengl-view)
                                                event)
  (declare (ignore event))
  t)


(defparameter *rubix-face-snap* 8.0) ; degrees

(objc:defmethod (#/mouseDown: :void) ((self rubix-opengl-view) the-event)
  ;; this makes dragging spin the cube
  (cond ((zerop (logand #$NSControlKeyMask (#/modifierFlags the-event))) ; not ctrl-click
	 (let ((dragging-p t))
           (let ((last-loc (#/locationInWindow the-event)))
             (loop while dragging-p do
                   (let ((the-event (#/nextEventMatchingMask:
                                     (#/window self)
                                     (logior #$NSLeftMouseUpMask
                                             #$NSLeftMouseDraggedMask))))
                     (let ((mouse-loc (#/locationInWindow the-event)))
                       (cond ((eq #$NSLeftMouseDragged (#/type the-event))
                              (let ((deltax (float
                                             (- (pref mouse-loc :<NSP>oint.x)
                                                (pref last-loc :<NSP>oint.x))
                                             0.0f0))
                                    (deltay (float
                                             (- (pref last-loc :<NSP>oint.y)
                                                (pref mouse-loc :<NSP>oint.y))
                                             0.0f0))
                                    (vert-rot-axis (cross *y-axis* *camera-pos*)))
                                (setf (pref last-loc :<NSP>oint.x) (pref mouse-loc :<NSP>oint.x)
                                      (pref last-loc :<NSP>oint.y) (pref mouse-loc :<NSP>oint.y))
                                (rotate-relative *cube*
                                                 (mulquats (axis-angle->quat vert-rot-axis deltay)
                                                           (axis-angle->quat *y-axis* deltax))))
                              (#/setNeedsDisplay: self t))
                             (t
                              (setf dragging-p nil))))))
             (#/setNeedsDisplay: self t))))
	(t;; ctrl-click, do what right-click does... note that once
         ;; ctrl-click is done dragging will not require ctrl be held down

	 ;; NOTE THE GRATUITOUS CUT-AND-PASTE, debug the right-mouse-down
	 ;; version preferentially and update this one with fixes as needed
         (let* ((first-loc (#/locationInWindow the-event))
                (pick-loc (#/convertPoint:fromView: self first-loc +null-ptr+)))
           (let ((dragging-p t)
                 (reference-snap 0))
             (setf (turning-face *cube*) (render-for-selection
                                          *cube*
                                          (opengl:unproject (pref pick-loc :<NSP>oint.x)
                                                            (pref pick-loc :<NSP>oint.y)))
                   (face-turning-p *cube*) (when (numberp (turning-face *cube*)) t)
                   (face-theta *cube*) 0.0)
             (loop while (and dragging-p (face-turning-p *cube*)) do
                   (let ((the-event (#/nextEventMatchingMask:
                                               (#/window self)
                                               (logior #$NSLeftMouseUpMask
                                                       #$NSLeftMouseDraggedMask))))
                     (let ((mouse-loc (#/locationInWindow the-event)))
                       (cond ((eq #$NSLeftMouseDragged (#/type the-event))
                              (let ((deltax (float
                                             (- (ns:ns-point-x mouse-loc)
                                                (ns:ns-point-x first-loc))
                                             0.0f0)))
                                (multiple-value-bind (snap-to snap-dist) (round deltax 90.0)
                                  (cond ((>= *rubix-face-snap* (abs snap-dist)) ; snap
                                         ;; update cube structure
                                         (let ((rotations (- snap-to reference-snap)))
                                           (cond ((zerop rotations) nil)
                                                 ((< 0 rotations)
                                                  (dotimes (i rotations)
                                                    (turnfaceclockwise *cube* (turning-face *cube*)))
                                                  (setf reference-snap snap-to))
                                                 ((> 0 rotations)
                                                  (dotimes (i (abs rotations))
                                                    (turnfacecounterclockwise *cube* (turning-face *cube*)))
                                                  (setf reference-snap snap-to))))
                                         ;; determine where face will be drawn
                                         (setf (face-theta *cube*) 0.0))
                                        (t ; no snap
                                         (setf (face-theta *cube*) (- deltax (* 90.0 reference-snap))))
                                        )))
                              (#/setNeedsDisplay: self t))
                             (t
                              (setf (face-turning-p *cube*) nil
                                    (turning-face *cube*) nil
                                    (face-theta *cube*) nil
                                    dragging-p nil))))))
             (#/setNeedsDisplay: self t)))
	 )))

(objc:defmethod (#/rightMouseDown: :void) ((self rubix-opengl-view) the-event)
  ;; this makes dragging left/right turn a face counterclockwise/clockwise
  ;; ... clicked-on face determines face turned
  ;; ... with an n-degree "snap"
  ;; ... with the snap updating the data structure
  ;; ... releasing the mouse clears rotation angle (face will snap to last position)
  (let* ((first-loc (#/locationInWindow the-event))
         (pick-loc (#/convertPoint:fromView: self first-loc +null-ptr+)))
    (let ((dragging-p t)
	  (reference-snap 0))
      (setf (turning-face *cube*) (render-for-selection
                                   *cube*
                                   (opengl:unproject (pref pick-loc :<NSP>oint.x)
                                                     (pref pick-loc :<NSP>oint.y)))
	    (face-turning-p *cube*) (when (numberp (turning-face *cube*)) t)
	    (face-theta *cube*) 0.0)
      (loop while (and dragging-p (face-turning-p *cube*)) do
	    (let ((the-event (#/nextEventMatchingMask:
                              (#/window self)
                              (logior #$NSRightMouseUpMask
                                      #$NSRightMouseDraggedMask))))
	      (let ((mouse-loc (#/locationInWindow the-event)))
		(cond ((eq #$NSRightMouseDragged (#/type the-event))
		       (let ((deltax (float
                                      (- (pref mouse-loc :<NSP>oint.x)
                                         (pref first-loc :<NSP>oint.x))
                                      0.0f0)))
			 (multiple-value-bind (snap-to snap-dist) (round deltax 90.0)
			   (cond ((>= *rubix-face-snap* (abs snap-dist)) ; snap
				  ;; update cube structure
				  (let ((rotations (- snap-to reference-snap)))
				    (cond ((zerop rotations) nil)
					  ((< 0 rotations)
					   (dotimes (i rotations)
					     (turnfaceclockwise *cube* (turning-face *cube*)))
					   (setf reference-snap snap-to))
					  ((> 0 rotations)
					   (dotimes (i (abs rotations))
					     (turnfacecounterclockwise *cube* (turning-face *cube*)))
					   (setf reference-snap snap-to))))
				  ;; determine where face will be drawn
				  (setf (face-theta *cube*) 0.0))
				 (t     ; no snap
				  (setf (face-theta *cube*) (- deltax (* 90.0 reference-snap))))
				 )))
		       (#/setNeedsDisplay: self t))
		      (t
		       (setf (face-turning-p *cube*) nil
			     (turning-face *cube*) nil
			     (face-theta *cube*) nil
			     dragging-p nil))))))
      (#/setNeedsDisplay: self t))))

(defclass rubix-window (ns:ns-window)
  ()
  (:metaclass ns:+ns-object))

(defparameter *aluminum-margin* 5.0f0)

(defun run-rubix-demo ()
  (let* ((w (gui::new-cocoa-window :class (find-class 'rubix-window)
				   :title "Rubix Cube"
				   :height 250
				   :width 250
				   :expandable nil))
	 (w-content-view (#/contentView w)))
    (let ((w-frame (#/frame w-content-view)))
      (ns:with-ns-rect (glview-rect *aluminum-margin*
                                    *aluminum-margin*
                                    (- (pref w-frame :<NSR>ect.size.width)
                                       (* 2 *aluminum-margin*))
                                    (- (pref w-frame :<NSR>ect.size.height)
                                       *aluminum-margin*))
	;; Q: why make-objc-instance here?
	(let ((glview (make-instance 'rubix-opengl-view
			    :with-frame glview-rect
			    :pixel-format #+ignore
			                  (#/defaultPixelFormat nsLns-opengl-view)
					  (opengl:new-pixel-format ;#$NSOpenGLPFADoubleBuffer
								   #$NSOpenGLPFAAccelerated
								   #$NSOpenGLPFAColorSize 32
								   #$NSOpenGLPFADepthSize 32))))
	  (#/addSubview: w-content-view glview)
	  w)))))
