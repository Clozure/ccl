;;; Example openmcl FFI by hamlink
;;;
;;; 2d Gasket example taken from
;;;  "Interactive Computer Graphics:
;;;   A Top-Down Approach with OpenGL" by Ed Angel

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ccl:use-interface-dir :GL))

(defpackage "2DGASKET")
(defpackage "OPENGL"
    (:nicknames :opengl :gl)
    (:export "INITIALIZE-GLUT"
	     "WITH-MATRIX-MODE"))

;;; Opening "libglut.so" should also open "libGL.so", "libGLU.so",
;;; and other libraries that they depend on.
;;; It seems that it does on some platforms and not on others;
;;; explicitly open what we require here.
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+linux-target
  (dolist (lib '("libGL.so" "libGLU.so" "libglut.so"))
    (open-shared-library lib))
  #+darwin-target
  (open-shared-library "GLUT.framework/GLUT")
  )

(in-package :opengl)

;; glut complains if it's initialized redundantly
(let ((glut-initialized-p nil))
  (defun initialize-glut ()
    (let ((command-line-strings (list "openmcl")))
      (when (not glut-initialized-p)
        (ccl::with-string-vector (argv command-line-strings)
          (rlet ((argvp (* t))    ; glutinit takes (* (:signed 32)) and (* (* (:unsigned 8)))
		 (argcp :signed)) ; so why are these declared as (* t) and :signed?
	    (setf (%get-long argcp) (length command-line-strings)
		  (%get-ptr argvp) argv)
	    (#_glutInit argcp argvp)))
	(setf glut-initialized-p t))))
  ;; When a saved image is restarted, it needs to know that glut
  ;; hasn't been initialized yet.
  (defun uninitialize-glut ()
    (setf glut-initialized-p nil))
  )

(pushnew #'uninitialize-glut ccl::*save-exit-functions*
	 :key #'ccl::function-name)

(defparameter *matrix-mode* #$GL_MODELVIEW)
(defmacro with-matrix-mode (mode &body body)
  `(unwind-protect
       (let ((*matrix-mode* ,mode))
	 (#_glMatrixMode *matrix-mode*)
	 ,@body)
     (#_glMatrixMode *matrix-mode*)))

(in-package :2dgasket)

(defun myinit ()
  (#_glClearColor 1.0 1.0 1.0 0.0) ; white background
  (#_glColor3f 1.0 0.0 0.0) ; red pen color

  (opengl:with-matrix-mode #$GL_PROJECTION
    (#_glLoadIdentity)
    (#_gluOrtho2D 0.0D0 500.0D0 0.0D0 500.0D0))

  ; (#_glEnable #$GL_DEPTH_TEST) ; for 3d only

  (#_srand (#_time (%null-ptr)))
  )

;; 2d gasket using points

(ccl::defcallback display-cb (:void)
  (let ((bounds #2a((0.0 0.0) (250.0 500.0) (500.0 0.0)))
	(point #(75.0 50.0)))
    (#_glClear #$GL_COLOR_BUFFER_BIT)
    (#_glBegin #$GL_POINTS)
    (dotimes (i 5000)
      (let ((j (random 3)))
	(setf (aref point 0) (/ (+ (aref point 0) (aref bounds j 0)) 2.0)
	      (aref point 1) (/ (+ (aref point 1) (aref bounds j 1)) 2.0))
	(#_glVertex2f (aref point 0) (aref point 1))))
    (#_glEnd)
    (#_glFlush)))

(defun main () ; no int argc or char **argv
  (opengl:initialize-glut)
  (#_glutInitDisplayMode (logior #$GLUT_RGB
				 #$GLUT_SINGLE
				 #+ignore #$GLUT_DEPTH))
  (#_glutInitWindowSize 500 500)
  (#_glutInitWindowPosition 0 0)
  (ccl::with-cstrs ((title "simple OpenGL example"))
    (#_glutCreateWindow title))
  (#_glutDisplayFunc display-cb)
  (myinit)
; It appears that glut provides no mechanism for doing the event loop
; yourself -- if you want to do that, you should use some other set of
; libraries and make your own GUI toolkit.
  
  (#_glutMainLoop) ; this never returns and interferes w/scheduling
  )


;;; With native threads, #_glutMainLoop doesn't necessarily interfere
;;; with scheduling: we can just run all of the OpenGL code in a separate
;;; thread (which'll probably spend most of its time blocked in GLUT's
;;; event loop.)  On OSX, we need to use an undocumented API or two
;;; to ensure that the thread we're creating is seen as the "main"
;;; event handling thread (that's what the code that sets the current
;;; thread's CFRunLoop to the main CFRunLoop does.)
#+OpenMCL-native-threads
(ccl:process-run-function
 "OpenGL main thread"
 #'(lambda ()
     #+darwin-target
     (progn
       ;;; In OSX, a "run loop" is a data structure that
       ;;; describes how event-handling code should block
       ;;; for events, timers, and other event sources.
       ;;; Ensure that this thread has a "current run loop".
       ;;; (Under some circumstances, there may not yet be
       ;;; a "main" run loop; setting the "current" run loop
       ;;; ensures that a main run loop exists.)
       (ccl::external-call "_CFRunLoopGetCurrent" :address)
       ;;; Make the current thread's run loop be the "main" one;
       ;;; only the main run loop can interact with the window
       ;;; server.
       (ccl::external-call
        "__CFRunLoopSetCurrent"
        :address (ccl::external-call "_CFRunLoopGetMain" :address))
       ;;; Set the OSX Window Server's notion of the name of the
       ;;; current process.
       (%stack-block ((psn 8))
         (ccl::external-call "_GetCurrentProcess" :address psn)
         (with-cstrs ((name "simple OpenGL example"))
           (ccl::external-call "_CPSSetProcessName" :address psn :address name))))
     (main)))

; (main)

