(eval-when (:compile-toplevel :load-toplevel :execute)
  (ccl:use-interface-dir :GL))

;;; Cocoa-based OpenGL package, for handy macros and classes of common
;;; things like transformations, lights, cameras, quaternions, etc.
;;; (note not all of this has been consolidated into this package yet)

#|

To use this functionality from cocoa, subclass NSOpenGLView,
specialize drawRect on your class to render whatever you like,
and make an intance in a window.

|#

#|
;;; Some design notes for OpenGL programming in lisp...

OpenGL is a very stateful API. with-X macros are invaluable for
managing OpenGL's stacks and modes.

The rubix demo is not set up this way, but really the main
objects in a scene need to contain references to their structure,
so that the structure can be reused between identical objects.
For large objects that are not always going to be present, the
structure could be compiled into a fasl and loaded only when
necessary using a sentinel in place of the structure reference.

Objects should capture the instance-specific state of objects in
a scene and be used to parameterize the drawing of skeleton-based
things. This can get tricky, but generic functions that draw
skeleton structures when passed specific data about the object's
state and the object's structure are probably the way to go.

Display lists are handy for static models. Something that loaded
easily-edited display list descriptions and turned them into fasl
data that rebuilt the display lists would be useful... if I can
find some EBNF and regexp forms my parser would build the ASTs
that could be turned into objects easily enough and from there
fasl data is easy to generate and save. If the file created a
fasl that set a hash entry from a structure id to a usable opengl
display list that would be good. A function that requested a
structure by id that loaded a file if there was no hash entry
would be slick.

Since this is lisp, it should be possible to create a display
list or an analogous lexical closure depending on what you want
from the same model information (to be later rendered as a static
object or rendered with a instance-state-driven function). I
don't know how many DLs OpenGL can have at one time or how big
they can be, it'd be good to know.

|#

(defpackage "OPENGL"
  (:nicknames :opengl :gl)
  (:export ;; Cocoa helpers
           "WITH-OPENGL-CONTEXT"
	   "NEW-PIXEL-FORMAT"
	   ;; OpenGL helpers
	   "WITH-MATRIX-MODE"
	   "WITH-RENDER-MODE"
	   "WITH-ROTATION"
	   "WITH-GL"
	   "WITH-CULLING"
	   "WITH-MATRIX"
	   "UNPROJECT"
	   ))

(in-package :opengl)

;; WITH-OPENGL-CONTEXT is not needed in the PREPARE-OPENGL
;; and DRAW-RECT functions of a specialized NS-OPENGL-VIEW
(defparameter *opengl-context* nil)
(defmacro with-opengl-context (context &body body)
  (let ((contextsym (gensym)))
    `(let ((,contextsym ,context))
       (unwind-protect
	   (let ((*opengl-context* ,contextsym))
             (#/makeCurrentContext ,contextsym)
	     ,@body)
	 ;; the following resets the current context to what it was
	 ;; previously as far as the special bindings are concerned
	 (if *opengl-context*
           (#/makeCurrentContext *opengl-context*)
           (#/clearCurrentConext ns:ns-opengl-context))))))

(defun new-pixel-format (&rest attributes)
  ;; take a list of opengl pixel format attributes (enums and other
  ;; small ints), make an array (character array?), and create and
  ;; return an NSOpenGLPixelFormat
  (let* ((attribute-size (ccl::foreign-size :<NSO>pen<GLP>ixel<F>ormat<A>ttribute :bytes))
         (nattributes (length attributes)))
    (ccl::%stack-block ((objc-attributes (* attribute-size (1+ nattributes))))
      (loop for i from 0 to nattributes
	    for attribute in attributes do
	    (setf (ccl:paref objc-attributes (:* :<NSO>pen<GLP>ixel<F>ormat<A>ttribute) i) attribute) ; <- autocoerced?
	    finally (setf (ccl:paref objc-attributes (:* :<NSO>pen<GLP>ixel<F>ormat<A>ttribute) nattributes) 0)) ; <- objc nil = null ptr
      (make-instance ns:ns-opengl-pixel-format :with-attributes objc-attributes))))

#|
(setf pf (opengl:new-pixel-format #$NSOpenGLPFADoubleBuffer #$NSOpenGLPFADepthSize 32))
(%stack-block ((a-long 4))
  (#/getValues:forAttribute:forVirtualScreen: pf a-long #$NSOpenGLPFADepthSize 0)
  (%get-long a-long))
|#

(defparameter *matrix-mode* #$GL_MODELVIEW)
(defmacro with-matrix-mode ((mode) &body body)
  `(unwind-protect
       (let ((*matrix-mode* ,mode))
	 (#_glMatrixMode *matrix-mode*)
	 ,@body)
     (#_glMatrixMode *matrix-mode*)))

(defparameter *render-mode* #$GL_RENDER)
(defmacro with-render-mode ((mode) &body body)
  `(block nil
     (unwind-protect
	 (let ((*render-mode* ,mode))
	   (#_glRenderMode *render-mode*)
	   ,@body)
       (return (#_glRenderMode *render-mode*)))))

(defmacro with-rotation ((angle axis) &body body)
  (let ((anglesym (gensym))
	(axissym (gensym)))
    `(let ((,anglesym ,angle)
	   (,axissym ,axis))
       (unwind-protect
	   (with-matrix-mode (#$GL_MODELVIEW)
	     (#_glPushMatrix)
	     (#_glRotatef ,anglesym (aref ,axissym 0) (aref ,axissym 1) (aref ,axissym 2))
	     ,@body)
	 (#_glPopMatrix)))))

(defmacro with-gl ((value) &body body)
  `(progn (#_glBegin ,value)
          ,@body
          (#_glEnd)))

(defmacro with-culling ((cull-face) &body body)
  `(progn (#_glEnable #$GL_CULL_FACE)
	  (#_glCullFace ,cull-face)
	  ,@body
	  (#_glDisable #$GL_CULL_FACE)))

(defmacro with-matrix ((load-identity-p) &body body)
  `(progn (#_glPushMatrix)
	  ,@(when load-identity-p `((#_glLoadIdentity)))
	  ,@body
	  (#_glPopMatrix)))

(defun unproject (x y)
  (let (;; yeah, yeah... I think I know how big these are...
	(gl-int-size (ccl::foreign-size :<GL>int :bytes))
	(gl-double-size (ccl::foreign-size :<GL>double :bytes)))
    (ccl::%stack-block ((viewport (* gl-int-size 4))
			(modelview-matrix (* gl-double-size 16))
			(projection-matrix (* gl-double-size 16))
			(wx gl-double-size)
			(wy gl-double-size)
			(wz gl-double-size))
      (#_glGetIntegerv #$GL_VIEWPORT viewport)
      (#_glGetDoublev #$GL_MODELVIEW_MATRIX modelview-matrix)
      (#_glGetDoublev #$GL_PROJECTION_MATRIX projection-matrix)
      (#_gluUnProject (ccl::%double-float x) (ccl::%double-float y) 0.0d0
		      modelview-matrix projection-matrix viewport
		      wx wy wz)
      (coerce (list (ccl::%get-double-float wx)
		    (ccl::%get-double-float wy)
		    (ccl::%get-double-float wz))
	      'vector))))
