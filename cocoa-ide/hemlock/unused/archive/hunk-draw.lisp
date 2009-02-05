;;; -*- Log: hemlock.log; Package: Hemlock-Internals -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
#+CMU (ext:file-comment
  "$Header$")
;;;
;;; **********************************************************************
;;;
;;; Written by Bill Chiles and Rob MacLachlan.
;;;
;;; Hemlock screen painting routines for the IBM RT running X.
;;;
(in-package :hemlock-internals)


;;;; TODO

;; . do away with these bogus macros HUNK-PUT-STRING and HUNK-REPLACE-LINE-STRING.

;; . concentrate these in a single point where we draw a string, so that we
;;   can easily introduce foreground and background colors for syntax
;;   highlighting and neater region highlighting.

;; --GB 2003-05-22

(defparameter hunk-height-limit 80 "Maximum possible height for any hunk.")
(defparameter hunk-width-limit 200 "Maximum possible width for any hunk.")
(defparameter hunk-top-border 2 "Clear area at beginning.")
(defparameter hunk-left-border 10 "Clear area before first character.")
(defparameter hunk-bottom-border 3 "Minimum Clear area at end.")
(defparameter hunk-thumb-bar-bottom-border 10
  "Minimum Clear area at end including room for thumb bar." )
(defparameter hunk-modeline-top 2 "Extra black pixels above modeline chars.")
(defparameter hunk-modeline-bottom 2 "Extra black pixels below modeline chars.")



;;;; Character translations for CLX

;;; HEMLOCK-TRANSLATE-DEFAULT.
;;;
;;; CLX glyph drawing routines allow for a character translation function.  The
;;; default one takes a string (any kind) or a vector of numbers and slams them
;;; into the outgoing request buffer.  When the argument is a string, it stops
;;; processing if it sees a character that is not GRAPHIC-CHAR-P.  For each
;;; graphical character, the function ultimately calls CHAR-CODE.
;;;
;;; Hemlock only passes simple-strings in, and these can only contain graphical
;;; characters because of the line image builder, except for one case --
;;; *line-wrap-char* which anyone can set.  Those who want to do evil things
;;; with this should know what they are doing: if they want a funny glyph as
;;; a line wrap char, then they should use CODE-CHAR on the font index.  This
;;; allows the following function to translate everything with CHAR-CODE, and
;;; everybody's happy.
;;;
;;; Actually, Hemlock can passes the line string when doing random-typeout which
;;; does contain ^L's, tabs, etc.  Under X10 these came out as funny glyphs,
;;; and under X11 the output is aborted without this function.
;;;
(defun hemlock-translate-default (src src-start src-end font dst dst-start)
  (declare (simple-string src)
	   (fixnum src-start src-end dst-start)
	   (vector dst)
	   (ignore font))
  (do ((i src-start (1+ i))
       (j dst-start (1+ j)))
      ((>= i src-end) i)
    (declare (fixnum i j))
    (setf (aref dst j) (char-code (schar src i)))))

#+clx
(defvar *glyph-translate-function* #'xlib:translate-default)



;;;; Drawing a line.

;;;; We hack along --GB
#+clx
(defun find-color (window color)
  (let ((ht (or (getf (xlib:window-plist window) :color-hash)
                (setf (getf (xlib:window-plist window) :color-hash)
                      (make-hash-table :test #'equalp)))))
    (or (gethash color ht)
        (setf (gethash color ht) (xlib:alloc-color (xlib:window-colormap window) color)))))

(defparameter *color-map*
  #("black" "white"
    "black" "white"
    "black" "white"
    "black" "cornflower blue"

    "black" "white"
    "black" "white"
    "black" "white"
    "black" "white"

    "blue4" "white"                     ;8 = comments
    "green4" "white"                     ;9 = strings
    "red" "white"                       ;10 = quote
    "black" "white"

    "black" "white"
    "black" "white"
    "black" "white"
    "black" "white"))

;;; HUNK-PUT-STRING takes a character (x,y) pair and computes at which pixel
;;; coordinate to draw string with font from start to end.
;;; 
(defmacro hunk-put-string (x y font string start end)
  (let ((gcontext (gensym)))
    `(let ((,gcontext (bitmap-hunk-gcontext hunk)))
       (xlib:with-gcontext (,gcontext :font ,font)
	 (xlib:draw-image-glyphs
	  (bitmap-hunk-xwindow hunk) ,gcontext
	  (+ hunk-left-border (* ,x (font-family-width font-family)))
	  (+ hunk-top-border (* ,y (font-family-height font-family))
	     (font-family-baseline font-family))
	  ,string :start ,start :end ,end
	  :translate *glyph-translate-function*)))))

(defun hunk-put-string* (hunk x y font-family font string start end)
  (let ((gcontext (bitmap-hunk-gcontext hunk))
        (font (svref (font-family-map font-family) font))
        (fg   (find-color (bitmap-hunk-xwindow hunk) (svref *color-map* (* font 2))))
        (bg   (find-color (bitmap-hunk-xwindow hunk) (svref *color-map* (1+ (* font 2))))))
    (xlib:with-gcontext (gcontext :font font
                                  :foreground fg
                                  :background bg)
      (xlib:draw-image-glyphs
       (bitmap-hunk-xwindow hunk) gcontext
       (+ hunk-left-border (* x (font-family-width font-family)))
       (+ hunk-top-border (* y (font-family-height font-family))
          (font-family-baseline font-family))
       string :start start :end end
       :translate *glyph-translate-function*))))

;;; HUNK-REPLACE-LINE-STRING takes a character (x,y) pair and computes at
;;; which pixel coordinate to draw string with font from start to end. We draw
;;; the text on a pixmap and later blast it out to avoid line flicker since
;;; server on the RT is not very clever; it clears the entire line before
;;; drawing text.

(defun hunk-replace-line-string* (hunk gcontext x y font-family font string start end)
  (declare (ignore y))
  (let ((font (svref (font-family-map font-family) font))
        (fg   (find-color (bitmap-hunk-xwindow hunk) (svref *color-map* (* font 2))))
        (bg   (find-color (bitmap-hunk-xwindow hunk) (svref *color-map* (1+ (* font 2))))))
    (xlib:with-gcontext (gcontext :font font
                                  :foreground fg
                                  :background bg)
      (xlib:draw-image-glyphs
       (hunk-replace-line-pixmap) gcontext
       (+ hunk-left-border (* x (font-family-width font-family)))
       (font-family-baseline font-family)
       string :start start :end end
       :translate *glyph-translate-function*))))

;;; Hunk-Write-Line  --  Internal
;;;
;;;    Paint a dis-line on a hunk, taking font-changes into consideration.
;;; The area of the hunk drawn on is assumed to be cleared.  If supplied,
;;; the line is written at Position, and the position in the dis-line
;;; is ignored.
;;;
(defun hunk-write-line (hunk dl &optional (position (dis-line-position dl)))
  (let* ((font-family (bitmap-hunk-font-family hunk))
	 (chars (dis-line-chars dl))
	 (length (dis-line-length dl)))
    (let ((last 0)
	  (last-font 0))
      (do ((change (dis-line-font-changes dl) (font-change-next change)))
	  ((null change)
           (hunk-put-string* hunk last position font-family last-font chars last length))
	(let ((x (font-change-x change)))
          (hunk-put-string* hunk last position font-family last-font chars last x)
	  (setq last x
                last-font (font-change-font change)) )))))


;;; We hack this since the X11 server's aren't clever about DRAW-IMAGE-GLYPHS;
;;; that is, they literally clear the line, and then blast the new glyphs.
;;; We don't hack replacing the line when reverse video is turned on because
;;; this doesn't seem to work too well.  Also, hacking replace line on the
;;; color Megapel display is SLOW!
;;;
(defvar *hack-hunk-replace-line* t)

;;; Hunk-Replace-Line  --  Internal
;;;
;;;    Similar to Hunk-Write-Line, but the line need not be clear.
;;;
(defun hunk-replace-line (hunk dl &optional
			       (position (dis-line-position dl)))
  (if *hack-hunk-replace-line*
      (hunk-replace-line-on-a-pixmap hunk dl position)
      (old-hunk-replace-line hunk dl position)))

(defun old-hunk-replace-line (hunk dl &optional (position (dis-line-position dl)))
  (let* ((font-family (bitmap-hunk-font-family hunk))
	 (chars (dis-line-chars dl))
	 (length (dis-line-length dl))
	 (height (font-family-height font-family)) )
    (let ((last 0)
	  (last-font 0))
      (do ((change (dis-line-font-changes dl) (font-change-next change)))
	  ((null change)
	   (hunk-put-string* hunk last position font-family last-font chars last length)
	   (let ((dx (+ hunk-left-border
			(* (font-family-width font-family) length))))
	     (xlib:clear-area (bitmap-hunk-xwindow hunk)
			      :x dx
			      :y (+ hunk-top-border (* position height))
			      :width (- (bitmap-hunk-width hunk) dx)
			      :height height)))
	(let ((x (font-change-x change)))
          (hunk-put-string* hunk last position font-family last-font chars last x)
	  (setq last x  last-font (font-change-font change)) )))))

(defvar *hunk-replace-line-pixmap* nil)

(defun hunk-replace-line-pixmap ()
  (if *hunk-replace-line-pixmap*
      *hunk-replace-line-pixmap*
      (let* ((hunk (window-hunk *current-window*))
	     (gcontext (bitmap-hunk-gcontext hunk))
	     (screen (xlib:display-default-screen
		      (bitmap-device-display (device-hunk-device hunk))))
	     (height (font-family-height *default-font-family*))
	     (pixmap (xlib:create-pixmap
		     :width (* hunk-width-limit
			       (font-family-width *default-font-family*))
		     :height height :depth (xlib:screen-root-depth screen)
		     :drawable (xlib:screen-root screen))))
	(xlib:with-gcontext (gcontext :function boole-1
				      :foreground *default-background-pixel*)
	  (xlib:draw-rectangle pixmap gcontext 0 0 hunk-left-border height t))
	(setf *hunk-replace-line-pixmap* pixmap))))

(defun hunk-replace-line-on-a-pixmap (hunk dl position)
  (let* ((font-family (bitmap-hunk-font-family hunk))
	 (chars (dis-line-chars dl))
	 (length (dis-line-length dl))
	 (height (font-family-height font-family))
	 (last 0)
	 (last-font 0)
	 (gcontext (bitmap-hunk-gcontext hunk)))
    (do ((change (dis-line-font-changes dl) (font-change-next change)))
	((null change)
	 (hunk-replace-line-string* hunk gcontext last position font-family last-font chars last length)
	 (let* ((dx (+ hunk-left-border
		       (* (font-family-width font-family) length)))
		(dy (+ hunk-top-border (* position height)))
		(xwin (bitmap-hunk-xwindow hunk)))
	   (xlib:with-gcontext (gcontext :exposures nil)
	     (xlib:copy-area (hunk-replace-line-pixmap) gcontext
			     0 0 dx height xwin 0 dy))
	   (xlib:clear-area xwin :x dx :y dy
			    :width (- (bitmap-hunk-width hunk) dx)
			    :height height)))
      (let ((x (font-change-x change)))
        (hunk-replace-line-string* hunk gcontext last position font-family last-font chars last x)
	(setq last x  last-font (font-change-font change))))))


;;; HUNK-REPLACE-MODELINE sets the entire mode line to the the foreground
;;; color, so the initial bits where no characters go also is highlighted.
;;; Then the text is drawn background on foreground (hightlighted).  This
;;; function assumes that BITMAP-HUNK-MODELINE-POS will not return nil;
;;; that is, there is a modeline.  This function should assume the gcontext's
;;; font is the default font of the hunk.  We must LET bind the foreground and
;;; background values before entering XLIB:WITH-GCONTEXT due to a non-obvious
;;; or incorrect implementation.
;;; 
(defun hunk-replace-modeline (hunk)
  (let* ((dl (bitmap-hunk-modeline-dis-line hunk))
	 (font-family (bitmap-hunk-font-family hunk))
	 (default-font (svref (font-family-map font-family) 0))
	 (modeline-pos (bitmap-hunk-modeline-pos hunk))
	 (xwindow (bitmap-hunk-xwindow hunk))
	 (gcontext (bitmap-hunk-gcontext hunk)))
    (xlib:draw-rectangle xwindow gcontext 0 modeline-pos
			 (bitmap-hunk-width hunk)
			 (+ hunk-modeline-top hunk-modeline-bottom
			    (font-family-height font-family))
			 t)
    (xlib:with-gcontext (gcontext :foreground
				  (xlib:gcontext-background gcontext)
				  :background
				  (xlib:gcontext-foreground gcontext)
				  :font default-font)
      (xlib:draw-image-glyphs xwindow gcontext hunk-left-border
			      (+ modeline-pos hunk-modeline-top
				 (font-family-baseline font-family))
			      (dis-line-chars dl)
			      :end (dis-line-length dl)
			      :translate *glyph-translate-function*))))


;;;; Cursor/Border color manipulation.

;;; *hemlock-listener* is set to t by default because we can't know from X
;;; whether we come up with the pointer in our window.  There is no initial
;;; :enter-window event.  Defaulting this to nil causes the cursor to be hollow
;;; when the window comes up under the mouse, and you have to know how to fix
;;; it.  Defaulting it to t causes the cursor to always come up full, as if
;;; Hemlock is the X listener, but this recovers naturally as you move into the
;;; window.  This also coincides with Hemlock's border coming up highlighted,
;;; even when Hemlock is not the listener.
;;;
(defvar *hemlock-listener* t
  "Highlight border when the cursor is dropped and Hemlock can receive input.")
(defvar *current-highlighted-border* nil
  "When non-nil, the bitmap-hunk with the highlighted border.")

(defvar *hunk-cursor-x* 0 "The current cursor X position in pixels.")
(defvar *hunk-cursor-y* 0 "The current cursor Y position in pixels.")
(defvar *cursor-hunk* nil "Hunk the cursor is displayed on.")
(defvar *cursor-dropped* nil) ; True if the cursor is currently displayed.

;;; HUNK-SHOW-CURSOR locates the cursor at character position (x,y) in hunk.
;;; If the cursor is currently displayed somewhere, then lift it, and display
;;; it at its new location.
;;; 
(defun hunk-show-cursor (hunk x y)
  (unless (and (= x *hunk-cursor-x*)
	       (= y *hunk-cursor-y*)
	       (eq hunk *cursor-hunk*))
    (let ((cursor-down *cursor-dropped*))
      (when cursor-down (lift-cursor))
      (setf *hunk-cursor-x* x)
      (setf *hunk-cursor-y* y)
      (setf *cursor-hunk* hunk)
      (when cursor-down (drop-cursor)))))

;;; FROB-CURSOR is the note-read-wait method for bitmap redisplay.  We
;;; show a cursor and highlight the listening window's border when waiting
;;; for input.
;;; 
(defun frob-cursor (on)
  (if on (drop-cursor) (lift-cursor)))

(declaim (special *default-border-pixmap* *highlight-border-pixmap*))

;;; DROP-CURSOR and LIFT-CURSOR are separate functions from FROB-CURSOR
;;; because they are called a couple places (e.g., HUNK-EXPOSED-REGION
;;; and SMART-WINDOW-REDISPLAY).  When the cursor is being dropped, since
;;; this means Hemlock is listening in the *cursor-hunk*, make sure the
;;; border of the window is highlighted as well.
;;;
(defun drop-cursor ()
  (unless *cursor-dropped*
    (unless *hemlock-listener* (cursor-invert-center))
    (cursor-invert)
    (when *hemlock-listener*
      (cond (*current-highlighted-border*
	     (unless (eq *current-highlighted-border* *cursor-hunk*)
	       (setf (xlib:window-border
		      (window-group-xparent
		       (bitmap-hunk-window-group *current-highlighted-border*)))
		     *default-border-pixmap*)
	       (setf (xlib:window-border
		      (window-group-xparent
		       (bitmap-hunk-window-group *cursor-hunk*)))
		     *highlight-border-pixmap*)
	       ;; For complete gratuitous pseudo-generality, should force
	       ;; output on *current-highlighted-border* device too.
	       (xlib:display-force-output
		(bitmap-device-display (device-hunk-device *cursor-hunk*)))))
	    (t (setf (xlib:window-border
		      (window-group-xparent
		       (bitmap-hunk-window-group *cursor-hunk*)))
		     *highlight-border-pixmap*)
	       (xlib:display-force-output
		(bitmap-device-display (device-hunk-device *cursor-hunk*)))))
      (setf *current-highlighted-border* *cursor-hunk*))
    (setq *cursor-dropped* t)))

;;;
(defun lift-cursor ()
  (when *cursor-dropped*
    (unless *hemlock-listener* (cursor-invert-center))
    (cursor-invert)
    (setq *cursor-dropped* nil)))


(defun cursor-invert-center ()
  (let ((family (bitmap-hunk-font-family *cursor-hunk*))
	(gcontext (bitmap-hunk-gcontext *cursor-hunk*)))
    (xlib:with-gcontext (gcontext :function boole-xor
				  :foreground *foreground-background-xor*)
      (xlib:draw-rectangle (bitmap-hunk-xwindow *cursor-hunk*)
			   gcontext
			   (+ hunk-left-border
			      (* *hunk-cursor-x* (font-family-width family))
			      (font-family-cursor-x-offset family)
			      1)
			   (+ hunk-top-border
			      (* *hunk-cursor-y* (font-family-height family))
			      (font-family-cursor-y-offset family)
			      1)
			   (- (font-family-cursor-width family) 2)
			   (- (font-family-cursor-height family) 2)
			   t)))
  (xlib:display-force-output
   (bitmap-device-display (device-hunk-device *cursor-hunk*))))

(defun cursor-invert ()
  (let ((family (bitmap-hunk-font-family *cursor-hunk*))
	(gcontext (bitmap-hunk-gcontext *cursor-hunk*)))
    (xlib:with-gcontext (gcontext :function boole-xor
				  :foreground *foreground-background-xor*)
      (xlib:draw-rectangle (bitmap-hunk-xwindow *cursor-hunk*)
			   gcontext
			   (+ hunk-left-border
			      (* *hunk-cursor-x* (font-family-width family))
			      (font-family-cursor-x-offset family))
			   (+ hunk-top-border
			      (* *hunk-cursor-y* (font-family-height family))
			      (font-family-cursor-y-offset family))
			   (font-family-cursor-width family)
			   (font-family-cursor-height family)
			   t)))
  (xlib:display-force-output
   (bitmap-device-display (device-hunk-device *cursor-hunk*))))



;;;; Clearing and Copying Lines.

(defun hunk-clear-lines (hunk start count)
  (let ((height (font-family-height (bitmap-hunk-font-family hunk))))
    (xlib:clear-area (bitmap-hunk-xwindow hunk)
		     :x 0 :y (+ hunk-top-border (* start height))
		     :width (bitmap-hunk-width hunk)
		     :height (* count height))))

(defun hunk-copy-lines (hunk src dst count)
  (let ((height (font-family-height (bitmap-hunk-font-family hunk)))
	(xwindow (bitmap-hunk-xwindow hunk)))
    (xlib:copy-area xwindow (bitmap-hunk-gcontext hunk)
		    0 (+ hunk-top-border (* src height))
		    (bitmap-hunk-width hunk) (* height count)
		    xwindow 0 (+ hunk-top-border (* dst height)))))



;;;; Drawing bottom border meter.

;;; HUNK-DRAW-BOTTOM-BORDER assumes eight-character-space tabs.  The LOGAND
;;; calls in the loop are testing for no remainder when dividing by 8, 4,
;;; and other.  This lets us quickly draw longer notches at tab stops and
;;; half way in between.  This function assumes that
;;; BITMAP-HUNK-MODELINE-POS will not return nil; that is, that there is a
;;; modeline.
;;; 
(defun hunk-draw-bottom-border (hunk)
  (when (bitmap-hunk-thumb-bar-p hunk)
    (let* ((xwindow (bitmap-hunk-xwindow hunk))
	   (gcontext (bitmap-hunk-gcontext hunk))
	   (modeline-pos (bitmap-hunk-modeline-pos hunk))
	   (font-family (bitmap-hunk-font-family hunk))
	   (font-width (font-family-width font-family)))
      (xlib:clear-area xwindow :x 0 :y (- modeline-pos
					  hunk-thumb-bar-bottom-border)
		       :width (bitmap-hunk-width hunk)
		       :height hunk-bottom-border)
      (let ((x (+ hunk-left-border (ash font-width -1)))
	    (y7 (- modeline-pos 7))
	    (y5 (- modeline-pos 5))
	    (y3 (- modeline-pos 3)))
	(dotimes (i (bitmap-hunk-char-width hunk))
	  (cond ((zerop (logand i 7))
		 (xlib:draw-rectangle xwindow gcontext
				      x y7 (if (= i 80) 2 1) 7 t))
		((zerop (logand i 3))
		 (xlib:draw-rectangle xwindow gcontext x y5 1 5 t))
		(t
		 (xlib:draw-rectangle xwindow gcontext x y3 1 3 t)))
	  (incf x font-width))))))

;; $Log$
;; Revision 1.1  2003/10/19 08:57:15  gb
;; Initial revision
;;
;; Revision 1.1.2.2  2003/09/18 13:40:16  gb
;; Conditionalize for #-CLX, a little more.
;;
;; Revision 1.1.2.1  2003/08/10 19:11:27  gb
;; New files, imported from upstream CVS as of 03/08/09.
;;
;; Revision 1.4  2003/08/05 19:54:17  gilbert
;; - did away with some macros
;; - invested in a left margin for added readability of hemlock frames.
;;
