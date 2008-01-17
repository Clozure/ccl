;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2006 Clozure Associates
;;;   This file is part of OpenMCL.  
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(in-package "CL-USER")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-interface-dir :gtk2))


;;; Loading "libgnomeui-2.so" seems to be the easiest way to force all of
;;; its dependent libraries to be loaded
(open-shared-library "libgnomeui-2.so")

(defloadvar *gdk-threads-inited* nil)
(defloadvar *gthread-inited* nil)


;;; Set things up so that GDK will use lisp locks internally.
;;; There are a few advantages to this, including the fact
;;; that lisp locks are that newfangled recursive kind (a thread
;;; that owns the lock can lock it agains, which is slightly
;;; better than waiting forever for it to be released.)
(defvar *gdk-lock* (make-lock))


;;; Callbacks called by #_gdk_threads_enter and #_gdk_threads_leave.
(defcallback lock-gdk-lock (:void)
  (grab-lock *gdk-lock*))

(defcallback unlock-gdk-lock (:void)
  (release-lock *gdk-lock*))


(defmacro with-gdk-lock-grabbed (&body body)
  `(with-lock-grabbed (*gdk-lock*)
    ,@body))

;;; gtk_signal_connect is a C macro. Emulate it.
(defmacro gtk-signal-connect (object name function user-data)
  `(external-call "gtk_signal_connect_full"
    :address ,object
    :address ,name
    :address ,function
    :<G>tk<C>allback<M>arshal (%null-ptr)
    :gpointer ,user-data
    :<G>tk<D>estroy<N>otify (%null-ptr)
    :gint 0
    :gint 0
    :gulong))

(defcallback window-destroy-handler (:address window :void)
  (declare (ignore window))
  (#_gtk_main_quit))



(defconstant single-float-pi (coerce pi 'single-float))

;;; A global alist mapping clock windows to their offscreen pixmaps.
(defvar *gtk-clock-window-pixmaps* ())


(defun draw-tick-at (pixmap gc nhour cx cy radius)
  (let* ((radians (/ (* single-float-pi nhour) 6.0))
	 (sin-radians (sin radians))
	 (cos-radians (cos radians))
	 (95%radius (* radius .95)))
    (#_gdk_draw_line pixmap gc
		     (+ cx (floor (* 95%radius sin-radians)))
		     (+ cy (floor (* 95%radius cos-radians)))
		     (+ cx (floor (* radius sin-radians)))
		     (+ cy (floor (* radius cos-radians))))))

;;; It seems like this can get called when the drawing area's in the
;;; process of being destroyed.  Try not to segfault in that case.
(defcallback gtk-clock-repaint (:address data :signed-fullword)
  (if (or (%null-ptr-p data)
	  (%null-ptr-p (pref data :<G>tk<W>idget.style)))
    #$FALSE
    (let* ((drawing-area data)
	   (radius 0)
	   (white-gc (pref drawing-area :<G>tk<W>idget.style.white_gc))
	   (black-gc (pref drawing-area :<G>tk<W>idget.style.black_gc))
	   (area-width  (pref drawing-area :<G>tk<W>idget.allocation.width))
	   (area-height (pref drawing-area :<G>tk<W>idget.allocation.height))
	   (dradians)
	   (midx 0)
	   (midy 0)
	   (vbox (pref drawing-area :<G>tk<W>idget.parent))
	   (window (pref vbox :<G>tk<W>idget.parent))
	   (pixmap (cdr (assoc window *gtk-clock-window-pixmaps*))))
      (rlet ((update-rect :<G>dk<R>ectangle))
	    ;; Clear pixmap (background image)
	    (#_gdk_draw_rectangle
	     pixmap white-gc #$TRUE 0 0 area-width area-height)
	    
	    ;; Calculate midpoint of clock.
	    (setq midx (ash area-width -1)
		  midy (ash area-height -1))
	    
	    ;; Calculate radius
	    (setq radius (min midx midy))

	    ;; Draw circle
	    (#_gdk_draw_arc pixmap black-gc 0 0 0
			    (+ midx midx) (+ midy midy) 0 (* 360 64))
      
	    ;; Draw tickmarks on clock face.
	    (do* ((nhour 1 (1+ nhour)))
		 ((> nhour 12))
	      (draw-tick-at pixmap black-gc nhour midx midy radius))
	    (multiple-value-bind (seconds minutes hours)
                (get-decoded-time)
	      
	      ;; Get radians from seconds
	      (setq dradians (/ (* seconds single-float-pi) 30.0))
	      
	      ;; Draw second hand.
	      (#_gdk_draw_line
	       pixmap black-gc midx midy
	       (+ midx (floor (* 0.9 radius (sin dradians))))
	       (- midy (floor (* 0.9 radius (cos dradians)))))
	      
	      ;; Get radians from minutes & seconds.
	      (setq dradians (+ (/ (* minutes single-float-pi) 30.0)
				(/ (* seconds single-float-pi) 1800.0)))
	      
	      ;; Draw minute hand.
	      (#_gdk_draw_line
	       pixmap black-gc midx midy
	       (+ midx (floor (* 0.7 radius (sin dradians))))
	       (- midy (floor (* 0.7 radius (cos dradians)))))
	      
	      ;; Get radians from hours & minutes.
	      (setq dradians (+ (/ (* (mod hours 12) pi) 6.0)
				(/ (* minutes pi) 360.0)))
	      
	      ;; Draw hour hand.
	      (#_gdk_draw_line
	       pixmap black-gc midx midy
	       (+ midx (floor (* 0.5 radius (sin dradians))))
	       (- midy (floor (* 0.5 radius (cos dradians)))))
	      
	      ;; Setup the update rectangle; this will force an expose event.
	      ;; The expose event handler will then copy the pixmap to the
	      ;; window.
	      
	      (setf (pref update-rect :<G>dk<R>ectangle.x) 0
		    (pref update-rect :<G>dk<R>ectangle.y) 0
		    (pref update-rect :<G>dk<R>ectangle.width) area-width
		    (pref update-rect :<G>dk<R>ectangle.height) area-height)
	      
	      ;; Draw the update rectangle.
	      (#_gtk_widget_draw drawing-area update-rect)
	      #$TRUE)))))


;;; This is called when the window's created and whenever it's
;;; resized.  Create a new pixmap of appropriate
;;; size; free the old one (if it's non-null).
(defcallback gtk-clock-configure-event
    (:address widget :address event :address window :signed-fullword)
  (declare (ignore event))
  (let* ((pair (assoc window *gtk-clock-window-pixmaps*)))
    (if (cdr pair)
      (#_gdk_drawable_unref (cdr pair)))
    (setf (cdr pair)
	  (#_gdk_pixmap_new (pref widget :<G>tk<W>idget.window)
			    (pref widget :<G>tk<W>idget.allocation.width)
			    (pref widget :<G>tk<W>idget.allocation.height)
			    -1)))
  #$TRUE)

;;; Copy the window's pixmap to the exposed region of the window.
(defcallback gtk-clock-expose-event
    (:address widget :address event :address window :signed-fullword)
  (let* ((state (pref widget :<G>tk<W>idget.state))
	 (pixmap (cdr (assoc window *gtk-clock-window-pixmaps*)))
	 (fg-gc (pref widget :<G>tk<W>idget.style.fg_gc))
	 (x (pref event :<G>dk<E>vent<E>xpose.area.x))
	 (y (pref event :<G>dk<E>vent<E>xpose.area.y))
	 (width (pref event :<G>dk<E>vent<E>xpose.area.width))
	 (height (pref event :<G>dk<E>vent<E>xpose.area.height)))
    (#_gdk_draw_drawable
     (pref widget :<G>tk<W>idget.window)
     (%get-ptr fg-gc (ash state target::word-shift))
     pixmap
     x y
     x y
     width height)
    #$FALSE))

;;; When the window's destroyed, delete its entry from the
;;; *gtk-clock-window-pixmaps* alist.

(defcallback gtk-clock-close (:address window :void)
  (let* ((pair (assoc window *gtk-clock-window-pixmaps*)))
    (if pair
      (if (null (setq *gtk-clock-window-pixmaps*
                      (delete pair *gtk-clock-window-pixmaps*)))
        (#_gtk_main_quit))
      (break "No entry for window!"))))

(defun gtk-clock ()
  (let* ((window (#_gtk_window_new #$GTK_WINDOW_TOPLEVEL))
	 (vbox (#_gtk_vbox_new #$FALSE 0)))
    (push (cons window nil) *gtk-clock-window-pixmaps*)
    (#_gtk_container_add window vbox)
    (#_gtk_widget_show vbox)
    (let* ((drawing-area (#_gtk_drawing_area_new)))
      (#_gtk_drawing_area_size drawing-area 200 200)
      (#_gtk_box_pack_start vbox drawing-area #$TRUE #$TRUE 0)
      (#_gtk_widget_show drawing-area)
      (with-cstrs ((expose-name "expose_event")
		   (configure-name "configure_event")
		   (destroy-name "destroy")
		   (window-title
		     "Takes a licking.  Keeps on ticking."))
	(#_gtk_window_set_title window window-title)
	(gtk-signal-connect drawing-area
			      expose-name
			      gtk-clock-expose-event
			      window)
	(gtk-signal-connect drawing-area
                            configure-name
                            gtk-clock-configure-event
                            window)
	(gtk-signal-connect window
                            destroy-name
                            gtk-clock-close
                            (%null-ptr)))
      (#_gtk_widget_show window)
      (#_gtk_timeout_add 1000 gtk-clock-repaint drawing-area)
      (values))))


(defun main (&rest args)
  (unless *gthread-inited*
    (#_g_thread_init (%null-ptr))
    (setq *gthread-inited* t))
  (unless *gdk-threads-inited*
    ;; Tell GDK to use our locks.
    (#_gdk_threads_set_lock_functions lock-gdk-lock unlock-gdk-lock)
    (#_gdk_threads_init)
    (setq *gdk-threads-inited* t))
  (process-run-function "GTK Event thread"
                        #'(lambda ()
                            (#_gdk_threads_enter)
                            (rlet ((argc :int)
                                   (argvp (:* t)))
                              (with-string-vector (argv args)
                                (setf (pref argc :int) (length args)
                                      (%get-ptr argvp ) argv)
                                (#_gtk_init argc argvp)))
                            (gtk-clock)
                            (#_gtk_main)
                            (#_gdk_threads_leave))))

;;; calling (MAIN) starts an event thread and displays a clock.
;;; subsequent calls to (GTK-CLOCK) display additional clocks,
;;;  if/when they can get a word in edgewise ...
