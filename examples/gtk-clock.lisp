;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2001 Clozure Associates
;;;
;;; This is a (loose) translation of the double-buffered clock GTK+
;;; example to OpenMCL.  See p 222 of "Developing Linux Applications
;;; with GDK and GTK+", Eric Harlow, (c) 1999 New Riders Publishing.
;;;
;;; Anyone who wants to use this code for any purpose is free to do so.
;;; In doing so, the user acknowledges that this code is provided "as is",
;;; without warranty of any kind, and that no other party is legally or
;;; otherwise responsible for any consequences of its use.

(in-package "CCL")

;;; 
;;; Make GTK+ interface info available.
(eval-when (:compile-toplevel :execute)
  (use-interface-dir :GTK))

;;; GTK+ "runtime support"; handy to have around at compile time, too.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "OPENMCL-GTK-SUPPORT"))


;;; A global alist mapping clock windows to their offscreen pixmaps.
(defvar *gtk-clock-window-pixmaps* ())


(defun draw-tick-at (pixmap gc nhour cx cy radius)
  (let* ((radians (/ (* pi nhour) 6.0d0))
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
		(decode-universal-time (get-universal-time))
	      
	      ;; Get radians from seconds
	      (setq dradians (/ (* seconds pi) 30.0d0))
	      
	      ;; Draw second hand.
	      (#_gdk_draw_line
	       pixmap black-gc midx midy
	       (+ midx (floor (* 0.9d0 radius (sin dradians))))
	       (- midy (floor (* 0.9d0 radius (cos dradians)))))
	      
	      ;; Get radians from minutes & seconds.
	      (setq dradians (+ (/ (* minutes pi) 30.0d0)
				(/ (* seconds pi) 1800.0d0)))
	      
	      ;; Draw minute hand.
	      (#_gdk_draw_line
	       pixmap black-gc midx midy
	       (+ midx (floor (* 0.7d0 radius (sin dradians))))
	       (- midy (floor (* 0.7d0 radius (cos dradians)))))
	      
	      ;; Get radians from hours & minutes.
	      (setq dradians (+ (/ (* (mod hours 12) pi) 6.0d0)
				(/ (* minutes pi) 360.0d0)))
	      
	      ;; Draw hour hand.
	      (#_gdk_draw_line
	       pixmap black-gc midx midy
	       (+ midx (floor (* 0.5d0 radius (sin dradians))))
	       (- midy (floor (* 0.5d0 radius (cos dradians)))))
	      
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
      (#_gdk_pixmap_unref (cdr pair)))
    
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
    (#_gdk_draw_pixmap
     (pref widget :<G>tk<W>idget.window)
     (%get-ptr fg-gc (ash state 2))
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
      (setq *gtk-clock-window-pixmaps*
	    (delete pair *gtk-clock-window-pixmaps*))
      (break "No entry for window!"))))

(defun gtk-clock ()
  ;; Doesn't hurt to call gtk-init more than once.
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
		     "Takes a lickin' and keeps on tickin'."))
	(#_gtk_window_set_title window window-title)
	(#_gtk_signal_connect drawing-area
			      expose-name
			      gtk-clock-expose-event
			      window)
	(#_gtk_signal_connect drawing-area
			      configure-name
			      gtk-clock-configure-event
			      window)
	(#_gtk_signal_connect window
			      destroy-name
			      gtk-clock-close
			      (%null-ptr)))
      (#_gtk_widget_show window)
      (#_gtk_timeout_add 1000 gtk-clock-repaint drawing-area)
      (values))))

