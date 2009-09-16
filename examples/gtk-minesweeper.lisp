;;;-*-Mode: LISP; Package: (MINESWEEPER :USE (CL CCL)) -*-
;;;
;;;   Copyright (C) 2001 Clozure Associates
;;; 
;;; This is a GTK+-based MineSweeper game, derived from a C program
;;; developed by Eric Harlow and published in "Developing Linux Programs
;;; with GTK+ and GDK", (c) 1999 New Riders Publishing.
;;;
;;; Anyone who wants to use this code for any purpose is free to do so.
;;; In doing so, the user acknowledges that this code is provided "as is",
;;; without warranty of any kind, and that no other party is legally or
;;; otherwise responsible for any consequences of its use.

(defpackage "MINESWEEPER"
  (:use "CL" "CCL")
  (:export "MINESWEEPER"))

(in-package "MINESWEEPER")

;;; 
;;; Make GTK+ interface info available.
(eval-when (:compile-toplevel :execute)
  (use-interface-dir :GTK2))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "OPENMCL-GTK-SUPPORT"))


(defconstant max-rows 35)
(defconstant max-cols 35)

(defconstant button-width 24)
(defconstant button-height 26)


(defvar *nrows* 10)
(defvar *ncols* 10)
(defvar *ntotalbombs* 0)

(defvar *bgameover* nil)
(defvar *bresetgame* nil)
(defvar *nbombsleft* nil)

(defvar *table* nil)
(defvar *start-button* nil)
(defvar *bombs-label* nil)
(defvar *time-label* nil)
(defvar *vbox* nil)

(defstruct cell
  (buttonstate :button-unknown
	       :type (member :button-down :button-unknown :button-flagged))
  button
  (bombsnearby 0)
  (has-bomb nil)
  row
  col)

;;; The original C Minesweeper example uses GtkToggleButtons to
;;; represent the cells on the grid.  They seem to work reasonably
;;; well except for one minor (but annoying) feature: "enter" and
;;; "leave" events cause the cells under the mouse to be highlighted,
;;; making it difficult to distinguish "unpressed buttons" from "the
;;; button under the mouse".
;;;
;;; This defines a GtkQuietToggleButton class that's exactly like
;;; GtkToggleButton except for the fact that it does nothing on
;;; "enter" and "leave" events.  It's not necessarily the most
;;; interesting example of subclassing a Gtk widget, but it -is- an
;;; example of doing so.
;;;
;;; GtkQuietToggleButtons seem to be better, but there is still some
;;; room for improvement.

(defcallback enter-or-leave-quietly (:address widget :void)
  (let* ((id (with-cstrs ((cell-id "cell-id"))
	       (#_gtk_object_get_data widget cell-id)))
	 (cell (cell-id->cell id))
	 (desired-state 
	  (if (member (cell-buttonstate cell)
		      '(:button-unknown :button-flagged))
	    #$GTK_STATE_NORMAL
	    #$GTK_STATE_ACTIVE))
	 (current-state (pref widget :<G>tk<W>idget.state)))
    (unless (eql current-state desired-state)
      (#_gtk_widget_set_state widget desired-state))))

(defcallback gtk_quiet_toggle_button_class_init (:address classptr :void)
  (setf (pref classptr :<G>tk<B>utton<C>lass.enter) enter-or-leave-quietly
	(pref classptr :<G>tk<B>utton<C>lass.leave) enter-or-leave-quietly))


(defcallback gtk_quiet_toggle_button_init (:address widget :void)
  (declare (ignore widget)))


;;; CCL::DEFLOADVAR behaves like DEFPARAMETER, but arranges to
;;; initialize the variable whenever a saved image start up
;;; as well as when the DEFLOADVAR is executed.
(ccl::defloadvar *gtk-quiet-toggle-button-type-info*
    (let* ((p (#_malloc (ccl::%foreign-type-or-record-size :<G>tk<T>ype<I>nfo :bytes))))
      (setf
       (pref p :<G>tk<T>ype<I>nfo.type_name)
       (with-cstrs ((name "GtkQuietToggleButton")) (#_g_strdup name))
       (pref p :<G>tk<T>ype<I>nfo.object_size)
       (ccl::%foreign-type-or-record-size :<G>tk<T>oggle<B>utton :bytes)
       (pref p :<G>tk<T>ype<I>nfo.class_size)
       (ccl::%foreign-type-or-record-size :<G>tk<T>oggle<B>utton<C>lass :bytes)
       (pref p :<G>tk<T>ype<I>nfo.class_init_func) gtk_quiet_toggle_button_class_init
       (pref p :<G>tk<T>ype<I>nfo.object_init_func) gtk_quiet_toggle_button_init
       (pref p :<G>tk<T>ype<I>nfo.reserved_1) (%null-ptr)
       (pref p :<G>tk<T>ype<I>nfo.reserved_2) (%null-ptr)
       (pref p :<G>tk<T>ype<I>nfo.base_class_init_func) (%null-ptr))
      p))

(ccl::defloadvar *gtk-quiet-toggle-button-type* nil)

(defun gtk-quiet-toggle-button-get-type ()
  (or *gtk-quiet-toggle-button-type*
      (setq *gtk-quiet-toggle-button-type*
	    (#_gtk_type_unique (#_gtk_toggle_button_get_type)
			       *gtk-quiet-toggle-button-type-info*))))

(defcallback gtk_quiet_toggle_button_get_type (:unsigned-fullword)
  (gtk-quiet-toggle-button-get-type))

(defun gtk-quiet-toggle-button-new ()
  (#_gtk_type_new (gtk-quiet-toggle-button-get-type)))

(defcallback gtk_quiet_toggle_button_new (:address)
  (gtk-quiet-toggle-button-new))

(defparameter *minesweeper-use-quiet-toggle-buttons* t)

;;; Display message dialogs (as for the About... box).

;;; A dialog widget has "grabbed" the focus.  Call back here when
;;; the dialog is to be closed; yield the focus.
(defcallback close-show-message
    (:address container :address data :void)
  (declare (ignore container))
  (let* ((dialog-widget data))
    (#_gtk_grab_remove dialog-widget)
    (#_gtk_widget_destroy dialog-widget)))

(defcallback clear-show-message
    (:address widget  :address data :void)
  (declare (ignore data))
  (#_gtk_grab_remove widget))

(defun show-message (title message)
  (let* ((dialog-window (#_gtk_dialog_new)))
    (with-cstrs ((destroy-name "destroy"))
      (#_gtk_signal_connect_full dialog-window destroy-name clear-show-message
			    (%null-ptr) (%null-ptr) (%null-ptr) 0 0))
    (with-cstrs ((title title))
      (#_gtk_window_set_title dialog-window title))
    (#_gtk_container_set_border_width dialog-window 0)

    (let* ((button (with-cstrs ((ok "OK"))
		     (#_gtk_button_new_with_label ok))))
      (with-cstrs ((clicked "clicked"))
	(#_gtk_signal_connect_full button clicked close-show-message (%null-ptr) dialog-window (%null-ptr) 0 0))
      (setf (pref button :<G>tk<O>bject.flags)
	    (logior (pref button :<G>tk<O>bject.flags) #$GTK_CAN_DEFAULT))
      (#_gtk_box_pack_start (pref dialog-window :<G>tk<D>ialog.action_area)
			    button #$TRUE #$TRUE 0)
      (#_gtk_widget_grab_default button)
      (#_gtk_widget_show button))

    (let* ((label (with-cstrs ((message message))
		    (#_gtk_label_new message))))
      (#_gtk_misc_set_padding label 10 10)
      (#_gtk_box_pack_start (pref dialog-window :<G>tk<D>ialog.vbox)
			    label #$TRUE #$TRUE 0)
      (#_gtk_widget_show label))

    (#_gtk_widget_show dialog-window)
    (#_gtk_grab_add dialog-window)))


(defun show-about ()
  (show-message "About ..."
		"Minesweeper OpenMCL GTK+ example
Copyright 2001 Clozure Associates
Derived from Minesweeper v0.6 by Eric Harlow"))

(defvar *win-main* ())
(defvar *accel-group* ())
(defvar *tooltips* ())

(defun reset-minesweeper-globals ()
  (setq *win-main* nil
	*accel-group* nil
	*tooltips* nil
	*vbox* nil
	*time-label* nil
	*bombs-label* nil
	*start-button* nil
	*table* nil
	*bgameover* nil
	*bresetgame* nil))
	
(defun create-widget-from-xpm (window xpm-string-list)
  (rlet ((mask (* :<G>dk<B>itmap)))
   (with-string-vector (xpm-data xpm-string-list)
     (let* ((pixmap-data (#_gdk_pixmap_create_from_xpm_d
			  (pref window :<G>tk<W>idget.window)
			  mask
			  (%null-ptr)
			  xpm-data))
	    (pixmap-widget (#_gtk_pixmap_new pixmap-data (%get-ptr mask))))
       (#_gtk_widget_show pixmap-widget)
       pixmap-widget))))

(defun create-menu-item (menu item-name accel tip func data)
  ;; A null or zero-length item-name indicates a separator.
  (let* ((menuitem nil))
    (if (and item-name (length item-name))
      (with-cstrs ((item-name item-name)
		   (activate "activate"))
	(setq menuitem (#_gtk_menu_item_new_with_label item-name))
	(#_gtk_signal_connect_full menuitem activate func (%null-ptr) (or data (%null-ptr)) (%null-ptr) 0 0))
      (setq menuitem (#_gtk_menu_item_new)))
    (#_gtk_menu_shell_append menu menuitem)
    (#_gtk_widget_show menuitem)

    (unless *accel-group*
      (setq *accel-group*
	    (#_gtk_accel_group_new))
      (#_gtk_window_add_accel_group *win-main* *accel-group*))

    (if (and accel (char= (schar accel 0) #\^))
      (with-cstrs ((activate "activate"))
	(#_gtk_widget_add_accelerator
	 menuitem activate *accel-group* (char-code (schar accel 1))
	 #$GDK_CONTROL_MASK #$GTK_ACCEL_VISIBLE)))

    (if (and tip (length tip))
      (with-cstrs ((tip tip))
	(#_gtk_tooltips_set_tip
	 (or *tooltips*
	     (setq *tooltips* (#_gtk_tooltips_new)))
	 menuitem
	 tip
	 (%null-ptr))))
    menuitem))
    
(defun create-radio-menu-item (menu item-name group-ptr func data)
  (with-cstrs ((item-name item-name)
	       (toggled "toggled"))
    (let* ((menuitem (#_gtk_radio_menu_item_new_with_label
		      (%get-ptr group-ptr)
		      item-name)))
      (setf (%get-ptr group-ptr)
	    (#_gtk_radio_menu_item_get_group menuitem))
      (#_gtk_menu_shell_append menu menuitem)
      (#_gtk_widget_show menuitem)
      (#_gtk_signal_connect_full menuitem toggled func (%null-ptr) (or data (%null-ptr)) (%null-ptr) 0 0)
      menuitem)))

(defun create-bar-sub-menu (menu name)
  (with-cstrs ((name name))
    (let* ((menuitem (#_gtk_menu_item_new_with_label name)))
      (#_gtk_menu_shell_append menu menuitem)
      (#_gtk_widget_show menuitem)
      (let* ((submenu (#_gtk_menu_new)))
	(#_gtk_menu_item_set_submenu menuitem submenu)
	submenu))))

;;; Represent xpm string vectors as lists of strings.  WITH-STRING-VECTOR
;;; will produce a foreign vector of C strings out of such a list.
(defvar *xpm-one*
  '(
    "12 12 2 1"
    "  c None"
    "X c #3333CC"
    "            "
    "     XX     "
    "    XXX     "
    "   X XX     "
    "     XX     "
    "     XX     "
    "     XX     "
    "     XX     "
    "     XX     "
    "   XXXXXX   "
    "            "
    "            "
    ))

(defvar *xpm-two*
  '(
    "12 12 2 1"
    "  c None"
    "X c #009900"
    "            "
    "   XXXXXX   "
    "  X      X  "
    "        XX  "
    "       XX   "
    "      XX    "
    "     XX     "
    "    XX      "
    "   XX       "
    "  XXXXXXXX  "
    "            "
    "            "
    ))


(defvar *xpm-three*
  '(
    "12 12 2 1"
    "  c None"
    "X c #AA0000"
    "            "
    "   XXXXX    "
    "        XX  "
    "        XX  "
    "   XXXXXX   "
    "        XX  "
    "        XX  "
    "        XX  "
    "        XX  "
    "  XXXXXX    "
    "            "
    "            "
    ))


(defvar *xpm-four*
  '(
    "12 12 2 1"
    "  c None"
    "X c #000066"
    "            "
    "  XX    XX  "
    "  XX    XX  "
    "  XX    XX  "
    "  XX    XX  "
    "  XXXXXXXX  "
    "        XX  "
    "        XX  "
    "        XX  "
    "        XX  "
    "            "
    "            "
    ))



(defvar *xpm-five*
  '(
    "12 12 2 1"
    "  c None"
    "X c #992299"
    "            "
    "  XXXXXXXX  "
    "  XX        "
    "  XX        "
    "  XXXXXXX   "
    "        XX  "
    "        XX  "
    "        XX  "
    "  XX    XX  "
    "  XXXXXXX   "
    "            "
    "            "
    ))


(defvar *xpm-six*
  '(
    "12 12 2 1"
    "  c None"
    "X c #550055"
    "            "
    "   XXXXXX   "
    "  XX        "
    "  XX        "
    "  XXXXXXX   "
    "  XX    XX  "
    "  XX    XX  "
    "  XX    XX  "
    "  XX    XX  "
    "   XXXXXX   "
    "            "
    "            "
    ))



(defvar *xpm-seven*
  '(
    "12 12 2 1"
    "  c None"
    "X c #550000"
    "            "
    "  XXXXXXXX  "
    "        XX  "
    "       XX   "
    "       XX   "
    "      XX    "
    "      XX    "
    "     WX     "
    "     XX     "
    "     XX     "
    "            "
    "            "
    ))



(defvar *xpm-eight*
  '(
    "12 12 2 1"
    "  c None"
    "X c #441144"
    "            "
    "   XXXXXX   "
    "  XX    XX  "
    "  XX    XX  "
    "   XXXXXX   "
    "  XX    XX  "
    "  XX    XX  "
    "  XX    XX  "
    "  XX    XX  "
    "   XXXXXX   "
    "            "
    "            "
    ))

(defvar *xpm-flag*
  '(
    "12 12 4 1"
    "  c None"
    "X c #000000"
    "R c #FF0000"
    "r c #AA0000"
    "            "
    "  RRRRRRR   "
    "  RRRRRrr   "
    "  RRRrrrr   "
    "  Rrrrrrr   "
    "        X   "
    "        X   "
    "        X   "
    "        X   "
    "        X   "
    "       XXX  "
    "            "
    ))


;;;
;;; --- A bomb.  Ooops, you're not as smart as you thought.
;;;
(defvar *xpm-bomb*
  '(
    "12 12 4 1"
    "  c None"
    "X c #000000"
    "R c #FF0000"
    "r c #AA0000"
    "            "
    "     X      "
    "  X  X  X   "
    "   XXXXX    "
    "   XXXXX    "
    " XXXXXXXXX  "
    "   XXXXX    "
    "   XXXXX    "
    "  X  X  X   "
    "     X      "
    "            "
    "            "
    ))


;;;
;;; --- Wrong move!
;;;
(defvar *xpm-bigx*
  '(
    "12 12 4 1"
    "  c None"
    "X c #000000"
    "R c #FF0000"
    "r c #AA0000"
    "RRR      RRR"
    " RRR    RRR "
    "  RRR  RRR  "
    "   RRRRRR   "
    "    RRRR    "
    "    RRRR    "
    "    RRRR    "
    "   RRRRRR   "
    "  RRR  RRR  "
    " RRR    RRR "
    "RRR      RRR"
    "            "
    ))


;;;
;;; --- Bitmap of a smile
;;;
(defvar *xpm-smile*
  '(
    "16 16 4 1"
    "  c None"
    ". c #000000"
    "X c #FFFF00"
    "r c #AA0000"
    "     ......     "
    "   ..XXXXXX..   "
    " ..XXXXXXXXXX.  "
    " .XXXXXXXXXXXX. "
    " .XX..XXXX..XX. "
    ".XXX..XXXX..XXX."
    ".XXXXXXXXXXXXXX."
    ".XXXXXXXXXXXXXX."
    ".XXXXXXXXXXXXXX."
    ".XXXXXXXXXXXXXX."
    " .XX.XXXXXX.XX. "
    " .XXX......XXX. "
    "  .XXXXXXXXXX.  "
    "   ..XXXXXX..   "
    "     ......     "
    "                "
    ))


;;;
;;; --- frown.  You lost.
;;;
(defvar *xpm-frown*
  '(
    "16 16 4 1"
    "  c None"
    ". c #000000"
    "X c #FFFF00"
    "r c #AA0000"
    "     ......     "
    "   ..XXXXXX..   "
    " ..XXXXXXXXXX.  "
    " .XXXXXXXXXXXX. "
    " .XX.X.XX.X.XX. "
    ".XXXX.XXXX.XXXX."
    ".XXX.X.XX.X.XXX."
    ".XXXXXXXXXXXXXX."
    ".XXXXXXXXXXXXXX."
    ".XXXXXXXXXXXXXX."
    " .XXX......XXX. "
    " .XX.XXXXXX.XX. "
    "  .XXXXXXXXXX.  "
    "   ..XXXXXX..   "
    "     ......     "
    "                "
    ))


;;;
;;; --- We have a winner
;;; 
(defvar *xpm-winner*
  '(
    "16 16 4 1"
    "  c None"
    ". c #000000"
    "X c #FFFF00"
    "r c #AA0000"
    "     ......     "
    "   ..XXXXXX..   "
    " ..XXXXXXXXXX.  "
    " .XXXXXXXXXXXX. "
    " .XX...XX...XX. "
    ".XX..........XX."
    ".X.X...XX...X.X."
    "..XXXXXXXXXXXX.."
    ".XXXXXXXXXXXXXX."
    ".XXXXXXXXXXXXXX."
    " .XX.XXXXXX.XX. "
    " .XXX......XXX. "
    "  .XXXXXXXXXX.  "
    "   ..XXXXXX..   "
    "     ......     "
    "                "
    ))

(defvar *digits*
  (vector nil *xpm-one* *xpm-two* *xpm-three* *xpm-four* *xpm-five*
	  *xpm-six* *xpm-seven* *xpm-eight*))

(defun set-grid (ncols nrows nbombs)
  (when *table*
    (#_gtk_widget_destroy *table*))
  (setq *table* (#_gtk_table_new ncols nrows #$FALSE))
  (#_gtk_box_pack_start *vbox* *table* #$FALSE #$FALSE 0)
  (#_gtk_widget_realize *table*)
  (reset-game ncols nrows nbombs t)
  (#_gtk_widget_show *table*))


;;; Menu callbacks.

;;; This is called both when the start button is pressed and when
;;; the "New" menu item is selected.
(defcallback start-button-clicked (:address widget :address data :void)
  (declare (ignore widget data))
  (set-start-button-icon *xpm-smile*)
  (reset-game *ncols* *nrows* *ntotalbombs* nil))

(defcallback action-beginner 
    (:address widget :address data :void)
  (declare (ignore data))
  (unless (zerop (pref widget :<G>tk<C>heck<M>enu<I>tem.active))
    (set-grid 10 10 10)))

(defcallback action-intermediate 
    (:address widget :address data :void)
  (declare (ignore data))
  (unless (zerop (pref widget :<G>tk<C>heck<M>enu<I>tem.active))
    (set-grid 20 15 40)))

(defcallback action-advanced
    (:address widget :address data :void)
  (declare (ignore data))
  (unless (zerop (pref widget :<G>tk<C>heck<M>enu<I>tem.active))
    (set-grid 30 20 100)))

(defcallback action-quit (:address widget :address data :void)
  (declare (ignore widget))
  (stop-timer)
  (#_gtk_widget_destroy data)
  (reset-minesweeper-globals))

(defcallback action-about (:void)
  (show-about))

(defun create-menu (window vbox-main)
  (setq *win-main* window)
  (setq *accel-group* (#_gtk_accel_group_new))
  (#_gtk_window_add_accel_group *win-main* *accel-group*)
  (let* ((menubar (#_gtk_menu_bar_new)))
    (#_gtk_box_pack_start vbox-main menubar #$FALSE #$TRUE 0)
    (#_gtk_widget_show menubar)
    (let* ((game-menu (create-bar-sub-menu menubar "Game")))
      (create-menu-item game-menu
			"New" "^N" "New Game" start-button-clicked nil)
      (create-menu-item game-menu nil nil nil nil nil)
      (rlet ((group (* t)))
	(setf (%get-ptr group) (%null-ptr))
	(with-macptrs ((group-ptr group))
	  (create-radio-menu-item game-menu "Beginner" group-ptr
				  action-beginner nil)
	  (create-radio-menu-item game-menu "Intermediate" group-ptr
				  action-intermediate nil)
	  (create-radio-menu-item game-menu "Advanced" group-ptr
				  action-advanced nil)))
      (create-menu-item game-menu nil nil nil nil nil)
      (create-menu-item game-menu "Quit" nil "Quit game"
			action-quit  *win-main*))
    (let* ((help-menu (create-bar-sub-menu menubar "Help")))
      (create-menu-item help-menu "About Minesweeper" nil "Gory Details"
			action-about nil))))
    



(defparameter *cells*
  (let* ((a (make-array (list max-cols max-rows))))
    (dotimes (row max-rows a)
      (dotimes (col max-cols)
	(setf (aref a col row)
	      (make-cell :row row :col col))))))

;;; Callbacks can receive (foreign) pointer arguments.  Since we'd
;;; rather keep information in lisp structures/arrays, that's not
;;; directly helpful.

;;; We can identify a cell by its row and column and
;;; can easily pack the row and column into a fixnum.  This function's
;;; caller can coerce that fixnum into a pointer (via ccl::%int-to-ptr).

(defun cell->cell-id (cell)
  (dpb (cell-row cell)
       (byte 8 8)
       (cell-col cell)))

;;; The inverse operation: the caller (a callback) will generally have
;;; a foreign pointer; it can coerce that to a fixnum and obtain the
;;; corresponding cell by unpacking its indices from that fixnum.

(defun cell-id->cell (cell-id)
  (let* ((id (if (typep cell-id 'macptr)
	       (%ptr-to-int cell-id)
	       cell-id))
	 (row (ldb (byte 8 8) id))
	 (col (ldb (byte 8 0) id)))
    (declare (fixnum id row col))
    (aref *cells* col row)))

;;; Free widget.
(defcallback FreeChildCallback (:address widget :void)
  (#_gtk_widget_destroy widget))

;;; Free all of the widgets contained in this one.
(defun free-children (widget)
  (#_gtk_container_foreach
   (#_g_type_check_instance_cast widget (#_gtk_container_get_type))
   FreeChildCallback (%null-ptr)))

(defun add-image-to-mine (cell xpm-data)
  (let* ((widget (create-widget-from-xpm *table* xpm-data)))
    (#_gtk_container_add (cell-button cell) widget)
    (#_gdk_drawable_unref widget)
    nil))

(defun open-nearby-squares (col row)
  (declare (fixnum col row))
  (let* ((mincol (max (1- col) 0))
	 (maxcol (min (1+ col) (1- *ncols*)))
	 (minrow (max (1- row) 0))
	 (maxrow (min (1+ row) (1- *nrows*))))
    (declare (fixnum mincol maxcol minrow maxrow))
    (do* ((i mincol (1+ i)))
	 ((> i maxcol))
      (declare (fixnum i))
      (do* ((j minrow (1+ j)))
	   ((> j maxrow))
	(declare (fixnum j))
	(display-hidden-info (aref *cells* i j))))))
    
(defun display-hidden-info (cell)
  (case (cell-buttonstate cell)
    (:button-down
     (#_gtk_toggle_button_set_active (cell-button cell) #$TRUE))
    (:button-flagged
     (#_gtk_toggle_button_set_active (cell-button cell) #$FALSE))
    (t
     (setf (cell-buttonstate cell) :button-down)
     (#_gtk_toggle_button_set_active (cell-button cell) #$TRUE)
     (setf (pref (cell-button cell) :<G>tk<B>utton.button_down) #$TRUE)
     (if (cell-has-bomb cell)
       (add-image-to-mine cell *xpm-bomb*)
       (let* ((nearby-bombs (cell-bombsnearby cell)))
	 (declare (fixnum nearby-bombs))
	 (if (> nearby-bombs 0)
	   (add-image-to-mine cell (svref *digits* nearby-bombs))
	   (open-nearby-squares (cell-col cell) (cell-row cell))))))))

(defun show-bombs ()
  (dotimes (i *ncols*)
    (dotimes (j *nrows*)
      (let* ((cell (aref *cells* i j))
	     (buttonstate (cell-buttonstate cell))
	     (has-bomb (cell-has-bomb cell)))
	(if (and (eq buttonstate :button-unknown) has-bomb)
	  (display-hidden-info cell)
	  (when (and (eq buttonstate :button-flagged) (not has-bomb))
	    (free-children (cell-button cell))
	    (add-image-to-mine cell *xpm-bigx*)))))))

	      
  
(defcallback cell-toggled (:address widget :address data :void)
  (let* ((cell (cell-id->cell data))
	 (state (cell-buttonstate cell)))
    (unless (eq state :button-flagged)
      (if *bgameover*
	(#_gtk_toggle_button_set_active widget
					(if (eq state
						:button-down)
					  #$TRUE
					  #$FALSE))
	(unless *bresetgame*
	  (start-timer)
	  (cond ((cell-has-bomb cell)
		 (setq *bgameover* t)
		 (set-start-button-icon *xpm-frown*)
		 (stop-timer)
		 (show-bombs))
		(t
		 (display-hidden-info cell)
		 (check-for-win))))))))



(defcallback button-press (:address widget :address event :address data :void)
  (unless *bgameover*
    (when (and (eql (pref event :<G>dk<E>vent<B>utton.type) #$GDK_BUTTON_PRESS)
	       (eql (pref event :<G>dk<E>vent<B>utton.button) 3))
      (let* ((cell (cell-id->cell data)))
	(case (cell-buttonstate cell)
	  (:button-unknown
	   (free-children widget)
	   (setf (cell-buttonstate cell) :button-flagged)
	   (add-image-to-mine cell *xpm-flag*)
	   (decf *nbombsleft*))
	  (:button-flagged
	   (free-children widget)
	   (setf (cell-buttonstate cell) :button-unknown)
	   (incf *nbombsleft*)))
	(display-bomb-count)
	(check-for-win)))))




(defun set-start-button-icon (xpm-list)
  (let* ((widget (create-widget-from-xpm *start-button* xpm-list)))
    (free-children *start-button*)
    (#_gtk_container_add *start-button* widget)))
    
(defun check-for-win ()
  (let* ((nmines 0))
    (declare (fixnum nmines))
    (dotimes (col *ncols*)
      (declare (fixnum col))
      (dotimes (row *nrows*)
	(declare (fixnum row))
	(when (member (cell-buttonstate (aref *cells* col row))
		      '(:button-unknown :button-flagged))
	  (incf nmines))))
    (when (= nmines (the fixnum *ntotalbombs*))
      (stop-timer)
      (set-start-button-icon *xpm-winner*)
      (setq *bgameover* t))))


(defun create-button (table cell row column)
  (let* ((button
	  (if *minesweeper-use-quiet-toggle-buttons*
	    (let* ((b (gtk-quiet-toggle-button-new))
		   (id (cell->cell-id (aref *cells* column row))))
	      (with-cstrs ((cell-id "cell-id"))
		(#_gtk_object_set_data b cell-id (%int-to-ptr id)))
	      b)
	    (#_gtk_toggle_button_new)))
	 (cell-id (cell->cell-id cell)))
    (with-cstrs ((toggled "toggled")
		 (button-press-event "button_press_event"))
      (#_gtk_signal_connect_full button toggled cell-toggled
                                 (%null-ptr) (%int-to-ptr cell-id) (%null-ptr) 0 0)
      (#_gtk_signal_connect_full button button-press-event
			    button-press (%null-ptr) (%int-to-ptr cell-id) (%null-ptr) 0 0))
    (#_gtk_table_attach table button
			column (1+ column)
			(1+ row) (+ row 2)
			(logior #$GTK_FILL #$GTK_EXPAND)
			(logior #$GTK_FILL #$GTK_EXPAND)
			0 0)
    (#_gtk_widget_set_usize button button-width button-height)
    (#_gtk_widget_show button)
    button))

    
(defun count-nearby-bombs (col row)
  (declare (fixnum col row))
  (let* ((mincol (max (1- col) 0))
	 (maxcol (min (1+ col) (1- *ncols*)))
	 (minrow (max (1- row) 0))
	 (maxrow (min (1+ row) (1- *nrows*)))
	 (ncount 0))
    (declare (fixnum mincol maxcol minrow maxrow ncount))
    (do* ((i mincol (1+ i)))
	 ((> i maxcol) ncount)
      (declare (fixnum i))
      (do* ((j minrow (1+ j)))
	   ((> j maxrow))
	(declare (fixnum j))
	(if (cell-has-bomb (aref *cells* i j))
	  (incf ncount))))))

(defun display-bomb-count ()
  (with-cstrs ((buf (format nil "Bombs: ~d" *nbombsleft*)))
    (#_gtk_label_set_text *bombs-label* buf)))

(defun update-seconds (seconds)
  (with-cstrs ((buf (format nil "Time: ~d" seconds)))
    (#_gtk_label_set_text *time-label* buf)))
  
(defun create-minesweeper-buttons (table ngridcols ngridrows bnewbuttons)
  (setq *nrows* ngridrows
	*ncols* ngridcols
	*bgameover* nil
	*bresetgame* t)
  (display-bomb-count)
  (dotimes (ci *ncols*)
    (declare (fixnum ci))
    (dotimes (ri *nrows*)
      (declare (fixnum ri))
      (let* ((cell (aref *cells* ci ri)))
	(setf (cell-has-bomb cell) nil
	      (cell-buttonstate cell) :button-unknown)
	(if bnewbuttons
	  (setf (cell-button cell) (create-button table cell ri ci))
	  (progn
	    (free-children (cell-button cell))
	    (#_gtk_toggle_button_set_active (cell-button cell) #$FALSE))))))
  (do* ((nbombs *ntotalbombs*)
	(state (make-random-state t)))
       ((zerop nbombs))
    (declare (fixnum nbombs))
    (let* ((cell (aref *cells* (random *ncols* state) (random *nrows* state))))
      (unless (cell-has-bomb cell)
	(setf (cell-has-bomb cell) t)
	(decf nbombs))))
  (dotimes (ci *ncols*)
    (declare (fixnum ci))
    (dotimes (ri *nrows*)
      (declare (fixnum ri))
      (setf (cell-bombsnearby (aref *cells* ci ri))
	    (count-nearby-bombs ci ri))))
  (setq *bresetgame* nil))
		   
(defun reset-game (ncols nrows nbombs bnewbuttons)
  (setq *ntotalbombs* nbombs
	*nbombsleft* nbombs)
  (create-minesweeper-buttons *table* ncols nrows bnewbuttons)
  (stop-timer)
  (update-seconds 0)
  (set-start-button-icon *xpm-smile*))


	     
;;; Timer stuff.

(defvar *timer* nil)
(defvar *nseconds* 0)

(defcallback timer-callback (:address data :void)
  (declare (ignore data))
  (incf *nseconds*)
  (update-seconds *nseconds*))

(defun start-timer ()
  (unless *timer*
    (setq *nseconds* 0
	  *timer* (#_gtk_timeout_add 1000 timer-callback *win-main*))))

(defun stop-timer ()
  (when *timer*
    (#_gtk_timeout_remove *timer*)
    (setq *timer* nil)))


;;; Finally ...

(defun minesweeper ()
  (when *win-main*
    (cerror
     "Close current minesweeper game and start a new one"
     "It seems that a minesweeper game is already active.")
    (do* ()
	 ((null *win-main*))
      (#_gtk_widget_destroy *win-main*)
      (sleep 1)))
  (let* ((window (#_gtk_window_new #$GTK_WINDOW_TOPLEVEL)))
    (#_gtk_window_set_policy window #$FALSE #$FALSE #$TRUE)
    (with-cstrs ((window-title "Minesweeper"))
      (#_gtk_window_set_title window window-title)
      (setq *vbox* (#_gtk_vbox_new #$FALSE 1))
      (#_gtk_widget_show *vbox*)
      (create-menu window *vbox*)
      (let* ((hbox (#_gtk_hbox_new #$TRUE 1)))
	(#_gtk_widget_show hbox)
	(#_gtk_box_pack_start *vbox* hbox #$FALSE #$FALSE 0)
	(with-cstrs ((len0-string ""))
	  (setq *bombs-label* (#_gtk_label_new len0-string)
		*time-label* (#_gtk_label_new len0-string)))
	(#_gtk_box_pack_start hbox *bombs-label* #$FALSE #$FALSE 0)
	(#_gtk_widget_show *bombs-label*)
	(setq *start-button* (#_gtk_button_new))
	(with-cstrs ((clicked "clicked"))
	  (#_gtk_signal_connect_full *start-button* clicked start-button-clicked
				(%null-ptr) (%null-ptr) (%null-ptr) 0 0))
	(#_gtk_box_pack_start hbox *start-button* #$FALSE #$FALSE 0)
	(#_gtk_widget_show *start-button*)
	(#_gtk_box_pack_start hbox *time-label* #$FALSE #$FALSE 0)
	(#_gtk_widget_show *time-label*)
	(#_gtk_widget_show hbox)
	(#_gtk_container_add window *vbox*)
	(with-cstrs ((destroy "destroy"))
	  (#_gtk_signal_connect_full window destroy action-quit (%null-ptr) window (%null-ptr) 0 0))
	(#_gtk_widget_show window)

	(set-start-button-icon *xpm-smile*)
	(set-grid 10 10 10)))))
