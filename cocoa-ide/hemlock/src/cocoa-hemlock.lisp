;;; -*- Mode: Lisp; Package: Hemlock-Internals -*-
;;;
;;; **********************************************************************
;;; Hemlock was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.

(in-package :hemlock-internals)

(defstruct (frame-event-queue (:include ccl::locked-dll-header))
  (signal (ccl::make-semaphore))
  (quoted-insert nil))

(defstruct (buffer-operation (:include ccl::dll-node))
  (thunk nil))

(defstruct (event-queue-node (:include ccl::dll-node)
                             (:constructor make-event-queue-node (event)))
  event)

(defun event-queue-insert (q node)
  (ccl::locked-dll-header-enqueue node q)
  (ccl::signal-semaphore (frame-event-queue-signal q)))

(defun enqueue-key-event (q event)
  (event-queue-insert q (make-event-queue-node event)))

(defun dequeue-key-event (q)
  (unless (listen-editor-input q)
    (let* ((document (buffer-document (current-buffer))))
      (when document
        (document-set-point-position document))))
  (ccl::wait-on-semaphore (frame-event-queue-signal q))
  (ccl::locked-dll-header-dequeue q))


(defun unget-key-event (event q)
  (ccl::with-locked-dll-header (q)
    (ccl::insert-dll-node-after (make-event-queue-node  event) q))
  (ccl::signal-semaphore (frame-event-queue-signal q)))

(defun timed-wait-for-key-event (q seconds)
  (let* ((signal (frame-event-queue-signal q)))
    (when (ccl:timed-wait-on-semaphore signal seconds)
      (ccl:signal-semaphore signal)
      t)))

(defvar *command-key-event-buffer* nil)

  

(defun buffer-windows (buffer)
  (let* ((doc (buffer-document buffer)))
    (when doc
      (document-panes doc))))

(defvar *current-window* ())

(defvar *window-list* ())
(defun current-window ()
  "Return the current window.  The current window is specially treated by
  redisplay in several ways, the most important of which is that is does
  recentering, ensuring that the Buffer-Point of the current window's
  Window-Buffer is always displayed.  This may be set with Setf."
  *current-window*)

(defun %set-current-window (new-window)
  #+not-yet
  (invoke-hook hemlock::set-window-hook new-window)
  (activate-hemlock-view new-window)
  (setq *current-window* new-window))

;;; This is a public variable.
;;;
(defvar *last-key-event-typed* ()
  "This variable contains the last key-event typed by the user and read as
   input.")

(defvar *input-transcript* ())

(defparameter editor-abort-key-events (list #k"Control-g" #k"Control-G"))

(defmacro abort-key-event-p (key-event)
  `(member (event-queue-node-event ,key-event) editor-abort-key-events))

(defconstant +shift-event-mask+ (hemlock-ext::key-event-modifier-mask "Shift"))
    
(defun get-key-event (q &optional ignore-pending-aborts)
  (do* ((e (dequeue-key-event q) (dequeue-key-event q)))
       ((typep e 'event-queue-node)
        (unless ignore-pending-aborts
          (when (abort-key-event-p e)
            (beep)
            (clear-echo-area)
            (throw 'editor-top-level-catcher nil)))
        (values (setq *last-key-event-typed* (event-queue-node-event e))
                (prog1 (frame-event-queue-quoted-insert q)
                  (setf (frame-event-queue-quoted-insert q) nil))))
    (if (typep e 'buffer-operation)
      (catch 'command-loop-catcher
        (funcall (buffer-operation-thunk e))))))

(defun recursive-get-key-event (q &optional ignore-pending-aborts)
  (let* ((buffer *command-key-event-buffer*)
         (doc (when buffer (buffer-document buffer))))
    (if (null doc)
      (get-key-event q ignore-pending-aborts)
      (unwind-protect
           (progn
             (document-end-editing doc)
             (get-key-event q ignore-pending-aborts))
        (document-begin-editing doc)))))


(defun listen-editor-input (q)
  (ccl::with-locked-dll-header (q)
    (not (eq (ccl::dll-header-first q) q))))

(defun add-buffer-font-region (buffer region)
  (when (typep buffer 'buffer)
    (let* ((header (buffer-font-regions buffer))
           (node (make-font-region-node region)))
      (ccl::append-dll-node node  header)
      (setf (font-region-node region) node)
      region)))

(defun enable-self-insert (q)
  (setf (frame-event-queue-quoted-insert q) t))

(defmethod disable-self-insert ((q frame-event-queue))
  (setf (frame-event-queue-quoted-insert q) nil))

(defun remove-font-region (region)
  (ccl::remove-dll-node (font-region-node region)))

(defun previous-font-region (region)
  (let* ((prev-node (ccl::dll-node-pred (font-region-node region))))
    (if (typep prev-node 'font-region-node)
      (font-region-node-region prev-node))))

(defun next-font-region (region)
  (let* ((next-node (ccl::dll-node-succ (font-region-node region))))
    (if (typep next-node 'font-region-node)
      (font-region-node-region next-node))))

;;; Make the specified font region "active", if it's non-nil and not
;;; already active.   A font region is "active" if it and all of its
;;; successors have "end" marks that're left-inserting, and all of its
;;; predecessors have "end" marks that're right-inserting.
;;; It's assumed that when this is called, no other font region is
;;; active in the buffer.

(defun activate-buffer-font-region (buffer region)
  (let* ((current (buffer-active-font-region buffer)))
    (unless (eq current region)
      (deactivate-buffer-font-region buffer current)
      (when region
        (setf (mark-%kind (region-end region)) :left-inserting
              (mark-%kind (region-start region)) :right-inserting)
        (do* ((r (next-font-region region) (next-font-region r)))
             ((null r)
              current)
          (setf (mark-%kind (region-end r)) :left-inserting
                (mark-%kind (region-start r)) :left-inserting)))
      (setf (buffer-active-font-region buffer) region)
      current)))

(defun deactivate-buffer-font-region (buffer region)
  (when (and region (eq (buffer-active-font-region buffer) region))
    (do* ((r region (next-font-region r)))
         ((null r) (setf (buffer-active-font-region buffer) nil))
      (setf (mark-%kind (region-end r)) :right-inserting
            (mark-%kind (region-start r)) :right-inserting))))


(defmacro with-active-font-region ((buffer region) &body body)
  (let* ((b (gensym))
         (old (gensym)))
    `(let* ((,b ,buffer)
            (,old (activate-buffer-font-region ,b ,region)))
      (unwind-protect
           (progn ,@body)
        (activate-buffer-font-region ,b ,old)))))

    
(defun show-buffer-font-regions (buffer)
  (ccl::do-dll-nodes (node (buffer-font-regions buffer))
    (let* ((r (font-region-node-region node))
           (start (region-start r))
           (end (region-end r)))
      (format t "~& style ~d ~d [~s]/ ~d [~s] ~a"
              (font-mark-font start)
              (ccl::mark-absolute-position start)
              (mark-%kind start)
              (ccl::mark-absolute-position end)
              (mark-%kind end)
              (eq r (buffer-active-font-region buffer))))))

;;; Clipboard
(defun region-to-clipboard (region)
  (string-to-clipboard (region-to-string region)))

;;; Meta-.
(defun hemlock::get-def-info-and-go-to-it (string package)
  (multiple-value-bind (fun-name error)
      (let* ((*package* package))
        (ignore-errors (values (read-from-string string))))
    (if error
      (editor-error)
      (hi::edit-definition fun-name))))

;;; Search highlighting
(defun note-selection-set-by-search (&optional (buffer (current-buffer)))
  (let* ((doc (buffer-document buffer)))
    (when doc (hi::document-note-selection-set-by-search doc))))
