;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
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

(in-package "CCL")

(defvar *inhibit-abort* nil)

;;; If any bits in the *periodic-task-mask* are set in the
;;; ptaskstate.flags word of a periodic task, it will not be run
(defvar *periodic-task-mask* 0)

(defmethod print-object ((p periodic-task) stream)
  (print-unreadable-object (p stream :type t :identity t)
    (format stream "~s ~d"
	    (ptask.name p)
	    (ptaskstate.interval (ptask.state p)))))

(defvar *periodic-task-lock* (make-lock))

(defun find-named-periodic-task (name)
  (dolist (task *%periodic-tasks%*)
    (when (eq name (ptask.name task))
      (return task))))

(defun %install-periodic-task (name function interval &optional 
                                    (flags 0)
                                    (privatedata (%null-ptr)))
  (with-lock-grabbed (*periodic-task-lock*)
   (let* ((already (find-named-periodic-task name))
          (state (if already (ptask.state already)
                   (%istruct 'ptaskstate 0 0 0 0)))
          (task (or already (%istruct 'periodic-task state name nil))))
     (setf (ptask.function task) function)
     (setf (ptaskstate.interval state) interval
           (ptaskstate.flags state ) flags
           (ptaskstate.privatedata state) privatedata
           (ptaskstate.nexttick state) (+ (get-tick-count) interval))
     (unless already (push task *%periodic-tasks%*))
     (let* ((interval-in-seconds (/ interval (float *ticks-per-second*))))
       (if (< interval-in-seconds *periodic-task-interval*)
         (set-periodic-task-interval interval-in-seconds)))
     task)))

(defmacro with-periodic-task-mask ((mask) &body body)
  (let ((thunk (gensym)))
    `(let ((,thunk #'(lambda () ,@body)))
       (funcall-with-periodic-task-mask ,mask ,thunk))))

(defvar *periodic-task-masks* nil)

; All this hair is so that multiple processes can vote on the *periodic-task-mask*
(defun funcall-with-periodic-task-mask (mask  thunk)
  (let* ((cell (list mask)))
    (declare (dynamic-extent cell))
    (flet ((logior-list (list)
             (declare (type list list))
             (let ((res 0))
               (declare (fixnum res))
               (loop
                 (when (null list) (return res))
                 (setq res (%ilogior res (pop list)))))))
      (declare (inline logior-list))
      (unwind-protect
        (progn
          (without-interrupts
           (setf (cdr cell) *periodic-task-masks*
                 *periodic-task-masks* cell)
           (setq *periodic-task-mask* (logior-list *periodic-task-masks*))
)
          (funcall thunk))
        (without-interrupts
         (let* ((first *periodic-task-masks*)
                (this first)
                (last nil))
           (declare (type cons first this last))
           (loop
             (when (eq this cell)
               (if last
                 (setf (cdr last) (cdr this))
                 (pop first))
               (return (setq *periodic-task-masks* first)))
             (setq last this
                   this (cdr this))))
         (setq *periodic-task-mask* (logior-list *periodic-task-masks*)))))))

(defparameter *invoke-debugger-hook-on-interrupt* nil)

(define-condition interrupt-signal-condition (condition) ()
  (:report "interrupt signal"))

(defun force-break-in-listener (p)
  (process-interrupt p
		     #'(lambda ()
                         (multiple-value-bind (vars inits old-vals) (%check-error-globals)
                           (progv vars old-vals
                             (mapcar (lambda (v f) (set v (funcall f))) vars inits)
                             (let ((condition (make-condition 'interrupt-signal-condition))
                                   (*top-error-frame* (%current-exception-frame)))
                               (ignoring-without-interrupts
                                 (when *invoke-debugger-hook-on-interrupt*
                                   (let* ((hook *debugger-hook*)
                                          (*debugger-hook* nil))
                                     (when hook
                                       (funcall hook condition hook))))
                                 (%break-in-frame *top-error-frame* condition)
                                 (clear-input *terminal-io*))))))))

(defglobal *quit-interrupt-hook* nil)

(defun force-async-quit (signum)
  (when *quit-interrupt-hook*
    (multiple-value-bind (req opt restp) (function-args *quit-interrupt-hook*)
      (if (and (= req 0) (= opt 0) (not restp))
        (funcall *quit-interrupt-hook*)
        (funcall *quit-interrupt-hook* signum))))
  ;; Exit by resignalling, as per http://www.cons.org/cracauer/sigint.html
  (quit #'(lambda ()
            (ff-call (%kernel-import target::kernel-import-lisp-sigexit) :signed signum)
            ;; Shouldn't get here
            (#__exit 143))))

(defstatic *running-periodic-tasks* nil)

(defun cmain ()
  (thread-handle-interrupts))


(defvar *select-interactive-process-hook* nil)

(defun select-interactive-abort-process ()
  (flet ((maybe-proc (proc) (and proc (process-active-p proc) proc)))
    (or (maybe-proc (and *select-interactive-process-hook*
                         (funcall *select-interactive-process-hook*)))
        (maybe-proc *interactive-abort-process*)
        (let* ((sr (input-stream-shared-resource *terminal-input*)))
          (when sr
            (or (maybe-proc (shared-resource-current-owner sr))
                (maybe-proc (shared-resource-primary-owner sr))))))))

(defun handle-gc-hooks ()
  (let ((bits *gc-event-status-bits*))
    (declare (fixnum bits))
    (cond ((logbitp $gc-postgc-pending-bit bits)
           (setq *gc-event-status-bits*
                 (logand (lognot (ash 1 $gc-postgc-pending-bit))
                         bits))
           (let ((f *post-gc-hook*))
             (when (functionp f) (funcall f)))))))

(defconstant $user-interrupt-break 1)
(defconstant $user-interrupt-quit 2)

(defun housekeeping ()
  (progn
    (handle-gc-hooks)
    (unless *inhibit-abort*
      (let* ((id (pending-user-interrupt))
             (kind (logand #xFF id)))
        (cond ((eql kind $user-interrupt-quit)
               ;; Try to use a process that has a shot at reporting any problems
               ;; in case of bugs in user hook.
               (let* ((proc (or (select-interactive-abort-process)
                                *initial-process*))
                      (signum (ash id -8)))
                 (process-interrupt proc #'force-async-quit signum)))
              ((eql kind $user-interrupt-break)
               (let* ((proc (select-interactive-abort-process)))
                 (if proc
                   (force-break-in-listener proc)))))))
    (flet ((maybe-run-periodic-task (task)
             (let ((now (get-tick-count))
                   (state (ptask.state task)))
               (when (and (>= (- now (ptaskstate.nexttick state))
                              0)
                          (eql 0 (logand (the fixnum (ptaskstate.flags state))
                                         (the fixnum *periodic-task-mask*))))
                 (setf (ptaskstate.nexttick state)
                       (+ now (ptaskstate.interval state)))
                 (funcall (ptask.function task))))))
      (let ((event-dispatch-task *event-dispatch-task*))
        (maybe-run-periodic-task event-dispatch-task)
        (with-lock-grabbed (*periodic-task-lock*)
          (bitclrf $gc-allow-stack-overflows-bit *gc-event-status-bits*)
          (unless *running-periodic-tasks*
            (let-globally ((*running-periodic-tasks* t))
              (dolist (task *%periodic-tasks%*)
                (unless (eq task event-dispatch-task)
                  (maybe-run-periodic-task task))))))))))


(defun %remove-periodic-task (name)
  (with-lock-grabbed (*periodic-task-lock*)
    (let ((task (find-named-periodic-task name)))
      (when task
        (if (setq *%periodic-tasks%* (delete task *%periodic-tasks%*))
          (let* ((min-ticks target::target-most-positive-fixnum))
            (dolist (other *%periodic-tasks%*
                     (set-periodic-task-interval (/ min-ticks (float *ticks-per-second*))))
              (let* ((other-ticks
                      (ptaskstate.interval (ptask.state other))))
                (if (< other-ticks min-ticks)
                  (setq min-ticks other-ticks)))))
          (set-periodic-task-interval 1)))
      task)))


(defun auto-flush-interactive-streams ()
  (with-lock-grabbed (*auto-flush-streams-lock*)
    (dolist (s *auto-flush-streams*)
      (when (open-stream-p s)
        (if (or (typep s 'basic-stream)
                (typep s 'buffered-io-stream-mixin))
          (if (ioblock-outbuf-lock (stream-ioblock s t))
            (force-output s)))
        (force-output s)))))

(defun add-auto-flush-stream (s)
  (with-lock-grabbed (*auto-flush-streams-lock*)
    (when (typep s 'output-stream)
      (pushnew s *auto-flush-streams*))))
      
(defun remove-auto-flush-stream (s)
  (with-lock-grabbed (*auto-flush-streams-lock*)
    (setq *auto-flush-streams* (delete s *auto-flush-streams*))))

; Is it really necessary to keep this guy in a special variable ?
(defloadvar *event-dispatch-task* 
  (%install-periodic-task 
   'auto-flush-interactive-streams
   'auto-flush-interactive-streams
   33
   (+ $ptask_draw-flag $ptask_event-dispatch-flag)))


(defun event-ticks ()
  (let ((task *event-dispatch-task*))
    (when task (ptaskstate.interval (ptask.state task)))))

(defun set-event-ticks (n)
  (setq n (require-type n '(integer 0 32767)))   ;  Why this weird limit ?
  (let ((task *event-dispatch-task*))
    (when task (setf (ptaskstate.interval (ptask.state task)) n))))

;; Making the *initial-process* quit will cause an exit(),
;; though it might be nicer if all processes were shut down
;; in an orderly manner first.  This is the not-so-nice way
;; of quitting ...
(defun %quit ()
  (quit))



; end of L1-events.lisp

