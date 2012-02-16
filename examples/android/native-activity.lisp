;;;-*-Mode: LISP; Package: CL-USER -*-
;;;
;;;   Copyright (C) 2012 Clozure Associates
;;;   This file is part of Clozure CL.  
;;;
;;;   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with Clozure CL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with Clozure CL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html


(in-package "CL-USER")

;;; This is supposed to be a transliteration of the code from the
;;; file native_activity/jni/main.c  from the Android NDK's samples
;;; directory.  It doesn't do much, but is intended to show how
;;; the native app glue works.

(def-foreign-type nil
  (:struct  :saved_state
            (:angle :float)
            (:x :int32_t)
            (:y :int32_t)))

(def-foreign-type nil
    (:struct :engine
             (#>app (:* (:struct :android_app)))
             (#>sensorManager (:* #>ASensorManager))
             (#>accelerometerSensor (:* #>ASensor))
             (#>sensorEventQueue (:* #>ASensorEventQueue))
             (#>animating :int)
             (#>display #>EGLDisplay)
             (#>surface #>EGLSurface)
             (#>context #>EGLContext)
             (#>width #>int32_t)
             (#>height #>int32_t)
             (#>state (:struct #>saved_state))))

;;;
;;; Initialize an EGL context for the current display.
;;;

(defun engine-init-display (engine)
  ;; initialize OpenGL ES and EGL
  (rletZ ((attribs (:array #>EGLint 9)))
    ;; Not the best way to initialize a foreign array, but
    ;; not the worst ...
    (let* ((i 0))
      (declare (fixnum i))
      (dolist (attrib '(#$EGL_SURFACE_TYPE #$EGL_WINDOW_BIT
                        #$EGL_BLUE_SIZE 8
                        #$EGL_GREEN_SIZE 8
                        #$EGL_RED_SIZE 8
                        #$EGL_NONE))
        (setf (paref attribs #>EGLint i) attrib)))
    (let* ((display (#_eglGetDisplay #$EGL_DEFAULT_DISPLAY)))
      (#_eglInitialize display (%null-ptr) (%null-ptr))
      (rlet ((pconfig #>EGLConfig)
             (pnumconfigs #>EGLint)
             (pformat #>EGLint))
        (#_eglChooseConfig display attribs pconfig 1 pnumconfigs)
        (let* ((config (pref pconfig #>EGLConfig)))
          (#_eglGetConfigAttrib display config #$EGL_NATIVE_VISUAL_ID pformat)
          (let* ((format (pref pformat #>EGLint))
                 (window (pref engine :engine.app.window)))
            (#_ANativeWindow_setBuffersGeometry  window 0 0 format)
            (let* ((surface (#_eglCreateWindowSurface display config window (%null-ptr)))
                   (context (#_eglCreateContext display config (%null-ptr) (%null-ptr))))
              (unless (eql (#_eglMakeCurrent display surface surface context)
                           #$EGL_FALSE)
                (rlet ((pw #>EGLint)
                       (ph #>EGLint))
                  (#_eglQuerySurface display surface #$EGL_WIDTH pw)
                  (#_eglQuerySurface display surface #$EGL_HEIGHT ph)
                  (setf (pref engine :engine.display) display
                        (pref engine :engine.context) context
                        (pref engine :engine.surface) surface
                        (pref engine :engine.width) (pref pw #>EGLint)
                        (pref engine :engine.height) (pref ph #>EGLint)
                        (pref engine :engine.state.angle) 0)
                  (#_glHint #$GL_PERSPECTIVE_CORRECTION_HINT #$GL_FASTEST)
                  (#_glEnable #$GL_CULL_FACE)
                  (#_glShadeModel #$GL_SMOOTH)
                  (#_glDisable #$GL_DEPTH_TEST)
                  t)))))))))

(defun engine-draw-frame (engine)
  (let* ((display (pref engine :engine.display)))
    (unless (%null-ptr-p display)
      (#_glClearColor (/ (float (pref engine :engine.state.x))
                         (float (pref engine :engine.width)))
                      (float (pref engine :engine.state.angle))
                      (/ (float (pref engine :engine.state.y))
                         (float (pref engine :engine.height)))
                      1.0f0)
      (#_glClear #$GL_COLOR_BUFFER_BIT)
      (#_eglSwapBuffers display (pref engine :engine.surface)))))

(defun engine-term-display (engine)
  (let* ((display (pref engine :engine.display))
         (context (pref engine :engine.context))
         (surface (pref engine :engine.surface)))
    (unless (eql display #$EGL_NO_DISPLAY)
      (#_eglMakeCurrent display #$EGL_NO_SURFACE #$EGL_NO_SURFACE #$EGL_NO_CONTEXT)
      (unless (eql context #$EGL_NO_CONTEXT)
        (#_eglDestroyContext display context))
      (unless (eql surface #$EGL_NO_SURFACE)
        (#_eglDestroySurface display surface))
      (#_eglTerminate display))
    (setf (pref engine :engine.animating) 0
          (pref engine :engine.display) #$EGL_NO_DISPLAY
          (pref engine :engine.context) #$EGL_NO_CONTEXT
          (pref engine :engine.surface) #$EGL_NO_SURFACE)))
    

(defcallback engine-handle-input ((:* (:struct #>android_app)) app
                                  (:* #>AInputEvent) event
                                  :int32_t)
  (cond ((eql (#_AInputEvent_getType event) #$AINPUT_EVENT_TYPE_MOTION)
         (let* ((engine (pref app #>android_app.userData)))
           (setf (pref engine :engine.animating) 1
                 (pref engine :engine.state.x) (#_AMotionEvent_getX event 0)
                 (pref engine :engine.state.y) (#_AMotionEvent_getY event 0))
           1))
        (t 0)))

(defcallback engine-handle-cmd ((:* (:struct #>android_app)) app
                                :int32_t cmd)
  (let* ((engine (pref app #>android_app.userData)))
    (case cmd
      (#.#$APP_CMD_SAVE_STATE
       (let* ((new (#_malloc (ccl::record-length :saved_state))))
         (#_memcpy new (pref engine :engine.state) (ccl::record-length :saved_state))
         (setf (pref app #>android_app.savedState) new
               (pref app #>android_app.savedStateSize) (ccl::record-length :saved_state))))
      (#.#$APP_CMD_INIT_WINDOW
       (unless (%null-ptr-p (pref app #>android_app.window))
         (engine-init-display engine)
         (engine-draw-frame engine)))
      (#.#$APP_CMD_TERM_WINDOW
       (engine-term-display engine))
      (#.#$APP_CMD_GAINED_FOCUS
       (unless (%null-ptr-p (pref engine #>engine.accelerometerSensor))
         (#_ASensorEventQueue_enableSensor
          (pref engine #>engine.sensorEventQueue)
          (pref engine #>engine.accelerometerSensor))
         (#_ASensorEventQueue_setEventRate
          (pref engine #>engine.sensorEventQueue)
          (pref engine #>engine.accelerometerSensor)
          (round (* 1000 (/ 1000 60))))))
      (#.#$APP_CMD_LOST_FOCUS 
       (unless (%null-ptr-p (pref engine #>engine.accelerometerSensor))
         (#_ASensorEventQueue_disableSensor
          (pref engine #>engine.sensorEventQueue)
          (pref engine #>engine.accelerometerSensor)))
       (setf (pref engine #>engine.animating) 0)
       (engine-draw-frame engine)))))

;;; This function implements android_main().  It needs to be called by this
;;; distinguished and funny name.  (It'll always need to be called by a
;;; distinguished name, but that name may be less funny in the future.)

(defun ccl::%os-init-function% (state)
  (rletz ((engine :engine))
    (setf (pref state #>android_app.userData) engine
          (pref state #>android_app.onAppCmd) engine-handle-cmd
          (pref state #>android_app.onInputEvent) engine-handle-input
          (pref engine #>engine.app) state
          (pref engine #>engine.sensorManager) (#_ASensorManager_getInstance)
          (pref engine #>engine.accelerometerSensor) (#_ASensorManager_getDefaultSensor (pref engine #>engine.sensorManager) #$ASENSOR_TYPE_ACCELEROMETER)
          (pref engine #>engine.sensorEventQueue) (#_ASensorManager_createEventQueue (pref engine #>engine.sensorManager) (pref state #>android_app.looper) #$LOOPER_ID_USER (%null-ptr) (%null-ptr)))
    (unless (%null-ptr-p (pref state #>android_app.savedState))
      (#_memcpy (pref engine #>engine.state)
                (pref state #>android_app.savedState)
                (ccl::record-length :saved_state)))
    (block event-loop
      (loop
        (let* ((ident -1))
          (rlet ((psource :address)
                 (pevents :int))
            (loop
              (setq ident (#_ALooper_pollAll (if (zerop (pref engine :engine.animating)) -1 0) (%null-ptr) pevents psource))
              (when (< ident 0) (return))
              (let* ((source (pref psource :address)))
                (unless (%null-ptr-p source)
                  (ff-call (pref source :android_poll_source.process)
                           :address state
                           :address source))
                (when (eql ident #$LOOPER_ID_USER)
                  (unless (%null-ptr-p (pref engine #>engine.accelerometerSensor))
                    (rlet ((event #>ASensorEvent))
                      (loop
                        (unless (> (#_ASensorEventQueue_getEvents
                                    (pref engine #>engine.sensorEventQueue)
                                    event
                                    1)
                                   0)
                          (return)))))))
              (unless (eql (pref state #>android_app.destroyRequested) 0)
                (engine-term-display engine)
                (return-from event-loop nil)))
          
    
            (unless (eql 0 (pref engine #>engine.animating))
              (when (> (incf (pref engine :engine.state.angle) 0.1f0) 1)
                (setf (pref engine :engine.state.angle) 0.0f0))
              (engine-draw-frame engine))))))))