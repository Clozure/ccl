;;; -*- Mode: Lisp; Package: Hemlock-Internals -*-
;;;
;;; **********************************************************************
;;; Hemlock was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.

(in-package :hemlock-internals)

(defun add-buffer-font-region (buffer region)
  (when (typep buffer 'buffer)
    (let* ((header (buffer-font-regions buffer))
           (node (make-font-region-node region)))
      (ccl::append-dll-node node  header)
      (setf (font-region-node region) node)
      region)))

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
              (mark-absolute-position start)
              (mark-%kind start)
              (mark-absolute-position end)
              (mark-%kind end)
              (eq r (buffer-active-font-region buffer))))))

;;; Clipboard
(defun region-to-clipboard (region)
  (hemlock-ext:string-to-clipboard (region-to-string region)))

