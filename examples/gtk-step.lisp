;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2001 Clozure Associates
;;;
;;; Anyone who wants to use this code for any purpose is free to do so.
;;; In doing so, the user acknowledges that this code is provided "as is",
;;; without warranty of any kind, and that no other party is legally or
;;; otherwise responsible for any consequences of its use.

;;; A GTK+-based interface to OpenMCL's stepper.

(in-package "CCL")

;;; 
;;; Make GTK+ interface info available.
(eval-when (:compile-toplevel :execute)
  (ccl::use-interface-dir :GTK))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "OPENMCL-GTK-SUPPORT")
  (require "STEP"))

(eval-when (:compile-toplevel :execute)
  (defconstant gtk-step-window-command-step 1)
  (defconstant gtk-step-window-command-step-over 2)
  (defconstant gtk-step-window-command-go 3)
  (defconstant gtk-step-window-command-eval 4)
  (defconstant gtk-step-window-command-quit 5)
  (defconstant gtk-step-window-command-window-closed 9999))

(defparameter *gtk-step-window-button-commands*
  (list gtk-step-window-command-step
	gtk-step-window-command-step-over
	gtk-step-window-command-go
	gtk-step-window-command-eval
	gtk-step-window-command-quit))

;;; The callback associated with button-clicked events appends one of the
;;; GTK-STEP-WINDOW-COMMAND- constants to a GList whose address is stored
;;; in P.  If the contents of P are non-NULL, remove the first element
;;; from the GList and return the command.
(defun gtk-step-window-command (p)
  (without-interrupts
   (let* ((q (%get-ptr p)))
     (declare (dynamic-extent q))
     (unless (%null-ptr-p q)
       (setf (%get-ptr p) (#_g_list_remove_link q q))
       (let* ((cmd (%ptr-to-int (pref q :<GL>ist.data))))
	 (#_g_list_free_1 q)
	 cmd)))))

;;; Button clicks come here; the "cmd" argument contains
;;; the command specific to this button, encoded as a pointer.
(defcallback gtk-step-window-button-clicked
    (:address button :address cmd :void)
  (with-cstrs ((qptr "qptr"))
    (let* ((p (#_gtk_object_get_data button qptr)))
    (declare (dynamic-extent p))
    (unless (%null-ptr-p p)
      (without-interrupts
       (setf (%get-ptr p)
	     (#_g_list_append (%get-ptr p) cmd)))))))


;;; If the step window gets closed before the stepper's finished, we
;;; want to know that ...
;;; The handler for the window-destroyed signal places a
;;; GTK-STEP-WINDOW-COMMAND-WINDOW-CLOSED command at the front of
;;; the command queue.
(defcallback gtk-step-window-closed (:address window :address cmd :void)
  (with-cstrs ((qptr "qptr"))
    (let* ((p (#_gtk_object_get_data window qptr)))
      (declare (dynamic-extent p))
      (unless (%null-ptr-p p)
	(without-interrupts
	 (setf (%get-ptr p)
	       (#_g_list_prepend (%get-ptr p) cmd)))))))


(defun make-gtk-step-window ()
  (let* ((window (#_gtk_window_new #$GTK_WINDOW_TOPLEVEL)))
    (#_gtk_widget_set_usize window 600 500)
    (#_gtk_window_set_policy window #$TRUE #$TRUE #$FALSE)
    (with-cstrs ((title "Step Window"))
      (#_gtk_window_set_title window title))
    (#_gtk_container_set_border_width window 0)
    (let* ((box1 (#_gtk_vbox_new #$FALSE 0))
	   (box2 (#_gtk_vbox_new #$FALSE 10)))
      (#_gtk_container_add window box1)
      (#_gtk_widget_show box1)
      (#_gtk_container_set_border_width box2 10)
      (#_gtk_box_pack_start box1 box2 #$TRUE #$TRUE 0)
      (#_gtk_widget_show box2)
      (let* ((table (#_gtk_table_new 2 2 #$FALSE))
	     (text (#_gtk_text_new (%null-ptr) (%null-ptr))))
	(#_gtk_text_set_editable text #$TRUE)
	(#_gtk_text_set_line_wrap text #$TRUE)
	(#_gtk_table_set_row_spacing table 0 2)
	(#_gtk_table_set_col_spacing table 0 2)
	(#_gtk_box_pack_start box2 table #$TRUE #$TRUE 0)
	(#_gtk_widget_show table)
	(#_gtk_table_attach table text
			    0		;left
			    1		;right
			    0		;top
			    1		;bottom
			    (logior #$GTK_EXPAND #$GTK_SHRINK #$GTK_FILL)
			    (logior #$GTK_EXPAND #$GTK_SHRINK #$GTK_FILL)
			    0
			    0)
	(#_gtk_widget_show text)
	(let* ((vscrollbar (#_gtk_vscrollbar_new (pref text :<G>tk<T>ext.vadj))))
	  (#_gtk_table_attach table vscrollbar
			      1
			      2
			      0
			      1
			      #$GTK_FILL
			      (logior #$GTK_EXPAND #$GTK_SHRINK #$GTK_FILL)
			      0
			      0)
	  (#_gtk_widget_show vscrollbar))
	(#_gtk_text_thaw text)
	(let* ((separator (#_gtk_hseparator_new)))
	  (#_gtk_box_pack_start box1 separator #$FALSE #$TRUE 0)
	  (#_gtk_widget_show separator))
	(let* ((box3 (#_gtk_hbox_new #$FALSE 10)))
	  (#_gtk_container_set_border_width box3 10)
	  (#_gtk_box_pack_start box1 box3 #$FALSE #$TRUE 0)
	  (#_gtk_widget_show box3)
	  (with-cstrs ((step-name "Step")
		       (step-over-name "Step over")
		       (go-name "Go")
		       (eval-name "Eval ...")
		       (quit-name "Quit")
		       (clicked "clicked")
		       (qptr "qptr"))
	    (let* ((buttons (list 				  
			     (#_gtk_button_new_with_label step-name)
			     (#_gtk_button_new_with_label step-over-name)
			     (#_gtk_button_new_with_label go-name)
			     (#_gtk_button_new_with_label eval-name)
			     (#_gtk_button_new_with_label quit-name)))
		   (commands *gtk-step-window-button-commands*)
		   (tips '("step through evaluation of form"
			   "step over evaluation of form"
			   "continue evaluation without stepping"
			   "evaluate an expression in current env"
			   "exit from the stepper (returning NIL)"))
		   (p (#_g_malloc0 4)))
	      (declare (dynamic-extent buttons))
	      (dolist (b buttons)
		(#_gtk_box_pack_start box3 b #$TRUE #$TRUE 0)
		(#_gtk_object_set_data b qptr p)
		(#_gtk_signal_connect b clicked
				      gtk-step-window-button-clicked
				      (%int-to-ptr (pop commands)))
		(with-cstrs ((tip-text (pop tips)))
		  (let* ((tip (#_gtk_tooltips_new )))
		    (#_gtk_tooltips_set_tip tip b tip-text (%null-ptr))))
		(#_gtk_widget_show b))
	      (let* ((step-button (car buttons)))
		(setf (pref step-button :<G>tk<O>bject.flags)
		      (logior (pref step-button :<G>tk<O>bject.flags)
			      #$GTK_CAN_DEFAULT))
		(#_gtk_widget_grab_default step-button))
	      (with-cstrs ((destroy "destroy"))
		(let* ((close-signal-id
			(#_gtk_signal_connect window destroy
					      gtk-step-window-closed
					      (%int-to-ptr
					       gtk-step-window-command-window-closed))))
		  (#_gtk_widget_show window)
		  (values text p window close-signal-id (reverse buttons)))))))))))

;;; A GTK+ user-interface to OpenMCL's stepper.
(defclass step-gtk-window-ui (step-ui)
    ((text :accessor step-gtk-window-ui-text)
     (queue-ptr :accessor step-gtk-window-ui-queue-ptr)
     (window :accessor step-gtk-window-ui-window)
     (close-signal-id :accessor step-gtk-window-ui-close-signal-id)
     (closed :initform nil :accessor step-gtk-window-ui-closed)
     (finished :initform nil :accessor step-gtk-window-ui-finished)
     (buttons :initform nil :accessor step-gtk-window-ui-buttons)
     (normal-font :initform nil)
     (bold-font :initform nil)))

(defun ui-output-formatted-string (ui string font-id)
  (unless (step-gtk-window-ui-closed ui)
    (let* ((text (step-gtk-window-ui-text ui))
	   (vadj (pref text :<G>tk<T>ext.vadj))
	   (font (if (eql 2 font-id)
		   (slot-value ui 'bold-font)
		   (slot-value ui 'normal-font))))
      (with-cstrs ((string string))
	(#_gtk_text_freeze text)
	(#_gtk_text_insert text font (pref text :<G>tk<W>idget.style.black)
			   (%null-ptr) string -1)
	(#_gtk_text_set_point text (#_gtk_text_get_length text))
	(unless (%null-ptr-p vadj)
	  (#_gtk_adjustment_set_value vadj (pref vadj :<G>tk<A>djustment.upper)))
	(#_gtk_text_thaw text)))))

(defmethod step-prin1 ((ui step-gtk-window-ui) form font &optional prefix)
  (ui-output-formatted-string
   ui
   (with-output-to-string
     (stream)
     (let ((*print-level* *step-print-level*)
	   (*print-length* *step-print-length*)
	   (*print-readably* nil)
	   (*print-array* nil)
	   (*print-case* :downcase))
       (when prefix (princ prefix stream))
       (prin1 form stream)))
   font))

(defmethod step-tab ((ui step-gtk-window-ui))
  (ui-output-formatted-string
   ui
   (with-output-to-string
     (stream)
     (terpri stream)
     (dotimes (i (min *step-level* *trace-max-indent*))
       (write-char #\Space stream)))
   1))

(defmethod step-show-error ((ui step-gtk-window-ui) err)
  (ui-output-formatted-string
   ui
   (with-output-to-string
     (stream)
     (step-tab ui)
     (princ "Error >> " stream)
     (format stream "~A" err))
   1))

(defmethod initialize-instance ((ui step-gtk-window-ui) &key)
  (multiple-value-bind (text ptr window signal-id buttons)
      (make-gtk-step-window)
    (setf (step-gtk-window-ui-text ui) text
	  (step-gtk-window-ui-queue-ptr ui) ptr
	  (step-gtk-window-ui-window ui) window
	  (step-gtk-window-ui-close-signal-id ui) signal-id
	  (step-gtk-window-ui-buttons ui) buttons
	  (step-gtk-window-ui-finished ui) nil
	  (step-gtk-window-ui-closed ui) nil)
    (with-cstrs ((medium "-misc-fixed-medium-r-*-*-*-120-*-*-*-*-*-*")
		 (bold   "-misc-fixed-bold-r-*-*-*-120-*-*-*-*-*"))
      (setf (slot-value ui 'normal-font) (#_gdk_font_load medium)
	    (slot-value ui 'bold-font) (#_gdk_font_load bold)))))

(defmethod step-ask ((ui step-gtk-window-ui))
  (let* ((qptr (step-gtk-window-ui-queue-ptr ui))
	 (cmd nil)
	 (wait-function #'(lambda ()
			    (let* ((c (gtk-step-window-command qptr)))
			      (when c
				(setq cmd c))))))
    (declare (dynamic-extent wait-function))
    (process-wait "step command wait" wait-function)
    (cond
      ((eql cmd gtk-step-window-command-step) :step)
      ((eql cmd gtk-step-window-command-step-over) :step-over)
      ((eql cmd gtk-step-window-command-go) :go)
      ((eql cmd gtk-step-window-command-eval) :eval)
      ((eql cmd gtk-step-window-command-quit) :quit)
      (t :quit))))

(defmethod step-ui-finish ((ui step-gtk-window-ui))
  (unless (step-gtk-window-ui-finished ui)
    (setf (step-gtk-window-ui-finished ui) t)
    (let* ((window (step-gtk-window-ui-window ui)))
      (#_gtk_signal_disconnect window
			       (step-gtk-window-ui-close-signal-id ui))
      (let*  ((buttons (prog1
			   (step-gtk-window-ui-buttons ui)
			 (setf (step-gtk-window-ui-buttons ui) nil)))
	      (parent (pref (car buttons) :<G>tk<W>idget.parent)))
	(with-cstrs ((close "Close")
		     (clicked "clicked"))
	  (let* ((close-button (#_gtk_button_new_with_label close)))
	    (#_gtk_signal_connect_object close-button clicked
					 (foreign-symbol-address
					  "gtk_widget_destroy")
					 window)
	    (#_gtk_box_pack_start parent close-button #$TRUE #$TRUE 0)
	    (dolist (b buttons)
	      (#_gtk_widget_destroy b))
	    (#_gtk_widget_show close-button)))))))



;;; Prompt for a string, via a GtkEntry widget.
;;;

;;; Tell lisp that the dialog's closed (for whatever reason.)
(defcallback gtk-get-string-dialog-closed
    (:address dialog :address info-ptr :void)
  (declare (ignore dialog))
  (setf (%get-ptr info-ptr 0) (%null-ptr)))

;;; String is ready.
(defcallback gtk-get-string-dialog-get-string
    (:address entry :address info-ptr :void)
  (setf (%get-ptr info-ptr 4) (#_g_strdup (#_gtk_entry_get_text entry)))
  ;;; Close the dialog.
  (#_gtk_widget_destroy (%get-ptr info-ptr 0)))

(defun gtk-get-string-from-user (prompt)
  (%stack-block ((info-ptr 12))
    (setf (%get-ptr info-ptr 0) (%null-ptr) ; backptr to window
	  (%get-ptr info-ptr 4) (%null-ptr)) ;string ptr
    (let* ((dialog-window (#_gtk_window_new #$GTK_WINDOW_DIALOG))
	   (vbox (#_gtk_vbox_new #$FALSE 0)))
      (setf (%get-ptr info-ptr 0) dialog-window)
      (#_gtk_container_add dialog-window vbox)
      (#_gtk_widget_show vbox)
      (with-cstrs  ((destroy "destroy")
		    (activate "activate")
		    (prompt prompt))
	(#_gtk_signal_connect dialog-window destroy
			      gtk-get-string-dialog-closed info-ptr)
	(#_gtk_widget_set_usize dialog-window 400 80)
	(let* ((label (#_gtk_label_new prompt))
	       (entry (#_gtk_entry_new)))
	  (#_gtk_box_pack_start vbox label #$TRUE #$TRUE 0)
	  (#_gtk_widget_show label)
	  (#_gtk_entry_set_max_length entry #x0000ffff)
	  (#_gtk_signal_connect entry activate gtk-get-string-dialog-get-string
				info-ptr)
	  (#_gtk_box_pack_end vbox entry #$TRUE #$TRUE 0)
	  (#_gtk_widget_show entry)
	  (#_gtk_widget_show dialog-window))
	(process-wait "text entry" #'(lambda () (%null-ptr-p (%get-ptr info-ptr 0))))
	(let* ((strptr (%get-ptr info-ptr 4))
	       (string ()))
	  (unless (%null-ptr-p strptr)
	    (unless (zerop (%get-byte strptr))
	      (setq string (%get-cstring strptr))
	      (#_g_free strptr)))
	  string)))))

(defmethod step-prompt-for-string ((ui step-gtk-window-ui) prompt)
  (gtk-get-string-from-user prompt))
	

(setq *default-step-ui-class-name* 'step-gtk-window-ui)



