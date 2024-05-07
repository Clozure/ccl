;;; -*- Syntax: Common-Lisp; Base: 10 -*-

(in-package #:ccl)

;;; Wrapper stream which adds a timestamp to each line of output

(defclass timestamped-stream (fundamental-character-output-stream)
  ((stream :accessor timestamped-stream-stream :initarg :stream :initform nil)
   (last-char :initform #\Newline))
  )

(defmethod initialize-instance :after ((ts timestamped-stream) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (check-type (timestamped-stream-stream ts) (or character-output-stream synonym-stream))
  )

(defmethod print-object ((ts timestamped-stream) out)
  (print-unreadable-object (ts out :type t :identity t)
    (print-object (timestamped-stream-stream ts) out)))

(declaim (inline timestamp-string))
(defun timestamp-string ()
  (multiple-value-bind (secs us)
      (floor (get-internal-real-time) internal-time-units-per-second)
    (format nil "~6,'0D.~6,'0D: " secs us)))

(defmethod stream-write-char ((ts timestamped-stream) char)
  (with-slots (stream last-char) ts
    (when (eql (shiftf last-char char) #\Newline)
      (fresh-line stream)
      (stream-write-string stream (timestamp-string)))
    (stream-write-char stream char)))

(defmethod stream-write-string ((ts timestamped-stream) string &optional (start 0) (end (length string)))
  (setf start (or start 0)
        end (or end (length string)))
  (with-slots (stream last-char) ts
    (when (eql (shiftf last-char (if (plusp end) (aref string (1- end)) #\Nul)) #\Newline)
      (fresh-line stream)
      (stream-write-string stream (timestamp-string)))
    (loop with start = start
          while (< start end)
          for position = (position #\Newline string :start start :end end)
          do (stream-write-string stream string start (or position end))
             (when position
               (terpri stream)
               (stream-write-string stream (timestamp-string)))
             (setf start (1+ (or position end)))))
  string)

(defmethod stream-line-column ((ts timestamped-stream))
  (stream-line-column (timestamped-stream-stream ts)))

(defmethod stream-set-column ((ts timestamped-stream) new)
  (declare (ignore new))
  nil)

(defmethod stream-force-output ((ts timestamped-stream))
  (stream-force-output (timestamped-stream-stream ts)))

(defmethod stream-finish-output ((ts timestamped-stream))
  (stream-finish-output (timestamped-stream-stream ts)))

(defmethod close ((ts timestamped-stream) &key abort)
  (declare (ignore abort))
  (when (open-stream-p (timestamped-stream-stream ts))
    (close (timestamped-stream-stream ts))
    t))

(defun make-timestamped-stream (output-stream)
  (make-instance 'timestamped-stream :stream output-stream))

(export 'make-timestamped-stream)
