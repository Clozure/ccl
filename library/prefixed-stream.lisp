;;; -*- Syntax: Common-Lisp; Base: 10 -*-

(in-package #:ccl)

;;; Wrapper stream which adds a prefix to each line of output

(defclass prefixed-stream (fundamental-character-output-stream)
  ((prefix :accessor prefixed-stream-prefix :initarg :prefix :initform nil)
   (stream :accessor prefixed-stream-stream :initarg :stream :initform nil)
   (last-char :initform #\Newline))
  )

(defmethod initialize-instance :after ((ps prefixed-stream) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (check-type (prefixed-stream-prefix ps) string)
  (check-type (prefixed-stream-stream ps) (or character-output-stream synonym-stream))
  )

(defmethod print-object ((ps prefixed-stream) out)
  (print-unreadable-object (ps out :type t :identity t)
    (print-object (prefixed-stream-stream ps) out)))

(defmethod stream-write-char ((ps prefixed-stream) char)
  (with-slots (prefix stream last-char) ps
    (when (eql (shiftf last-char char) #\Newline)
      (fresh-line stream)
      (stream-write-string stream prefix))
    (stream-write-char stream char)))

(defmethod stream-write-string ((ps prefixed-stream) string &optional (start 0) (end (length string)))
  (setf start (or start 0)
        end (or end (length string)))
  (with-slots (prefix stream last-char) ps
    (when (eql (shiftf last-char (if (plusp end) (aref string (1- end)) #\Nul)) #\Newline)
      (fresh-line stream)
      (stream-write-string stream prefix))
    (loop with start = start
          while (< start end)
          for position = (position #\Newline string :start start :end end)
          do (stream-write-string stream string start (or position end))
             (when position
               (terpri stream)
               (stream-write-string stream prefix))
             (setf start (1+ (or position end)))))
  string)

(defmethod stream-line-column ((ps prefixed-stream))
  (stream-line-column (prefixed-stream-stream ps)))

(defmethod stream-set-column ((ps prefixed-stream) new)
  (declare (ignore new))
  nil)

(defmethod stream-force-output ((ps prefixed-stream))
  (stream-force-output (prefixed-stream-stream ps)))

(defmethod stream-finish-output ((ps prefixed-stream))
  (stream-finish-output (prefixed-stream-stream ps)))

(defmethod close ((ps prefixed-stream) &key abort)
  (declare (ignore abort))
  (when (open-stream-p (prefixed-stream-stream ps))
    (close (prefixed-stream-stream ps))
    t))

(defun make-prefixed-stream (prefix output-stream)
  (make-instance 'prefixed-stream :prefix prefix :stream output-stream))

(export 'make-prefixed-stream)
