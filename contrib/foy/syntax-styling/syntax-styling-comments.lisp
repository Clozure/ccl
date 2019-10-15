;;;-*- Mode: Lisp; Package: (SYNTAX-STYLING (CL CCL HEMLOCK-INTERNALS)) -*-

;;; ****************************************************************************
;;; 
;;;      syntax-styling-comments.lisp
;;;
;;;      copyright © 2009 Glen Foy, all rights reserved,
;;;
;;;     These classes support the styling of semi-colon and sharp-stroke comments,
;;;     and strings.  Most unusual cases are correctly handled: strings embedded in 
;;;     comments, comments inside of strings, etc.
;;;
;;;      Mod history, most recent first:
;;;      10/18/9   first cut.
;;; 
;;; ****************************************************************************

(in-package "SAX")


;;; ----------------------------------------------------------------------------
;;;
(defClass STYLED-COMMENT ()
  ((comment-start :initarg :comment-start :initform nil :reader comment-start)
   (comment-end :initform nil :initarg :comment-end :reader comment-end))
  (:documentation "Support for styled comments."))

(defClass STYLED-SEMI-COLON-COMMENT (styled-comment) ())

(defClass STYLED-SHARP-COMMENT (styled-comment) ())

(defMethod style-comment ((comment styled-semi-colon-comment))
  (set-style-attributes (attribute-dictionary *semi-colon-comment-style*)
                        (comment-start comment) (comment-end comment)))

(defMethod style-comment ((comment styled-sharp-comment))
  (set-style-attributes (attribute-dictionary *sharp-comment-style*)
                        (comment-start comment) (comment-end comment)))

;;; ----------------------------------------------------------------------------
;;;
(defClass STYLED-STRING ()
  ((string-start :initarg :string-start :initform nil :reader string-start)
   (string-end :initform nil :initarg :string-end :reader string-end))
  (:documentation "Support for styled strings."))

(defMethod style-string ((string styled-string))
  (cond (*inc-p* ; if dynamic, never style past *inc-pos* 
         (set-style-attributes (attribute-dictionary *string-style*)
                               (string-start string) *inc-pos*))
        (t
         (set-style-attributes (attribute-dictionary *string-style*)
                               (string-start string) (string-end string)))))

;;; ----------------------------------------------------------------------------
;;;
(defClass SEGMENT-ARRAY ()
  ((array :initarg :array :reader segment-array-array)
   (length :initarg :length :accessor segment-array-length))
  (:documentation 
   "A sorted 2d array of the start and end positions for segments  in
a buffer.  There are three segment types: strings, semi-colon comments, 
and sharp-stroke comments.  The method not-embedded-in-segment-p does
 a binary search for the position of a particular char to see if the 
char is embedded."))

(defMethod print-object ((array segment-array) stream)
  (declare (ignore stream))
  #+sax-debug (when *print-object-segment-array-debug*
                (dump-segment-array array))
  #-sax-debug (call-next-method))

(defmethod dump-segment-array ((a segment-array))
  (format t "~%~%segment-array length: ~S" (segment-array-length a))
  (dotimes (idx (segment-array-length a))
    (format t "~%   ~S" (aref (segment-array-array a) idx 0))
    (format t "~%   ~S~%" (aref (segment-array-array a) idx 1))))

(defun unify-segment-lists (segment-list-1 segment-list-2)
  "Merge two lists, discarding segments which are embedded in segments of the other list."
  (do* ((list-1 segment-list-1)
        (list-2 segment-list-2)
        (segment-1 (first list-1) (first list-1))
        (segment-2 (first list-2) (first list-2))
        (unified-list nil))
       ((and (endp list-1) (endp list-2)) (nreverse unified-list))
    (cond ((and list-1 list-2)
           (cond ((mark< (first segment-1) (first segment-2))
                  (cond ((mark< (first segment-2) (second segment-1))
                         (pop list-2))
                        (t 
                         (push segment-1 unified-list)
                         (pop list-1))))
                 (t
                  (cond ((mark< (first segment-1) (second segment-2))
                         (pop list-1))
                        (t 
                         (push segment-2 unified-list)
                         (pop list-2))))))
          (t ; one list is empty - add what's left of the other
           (cond ((endp list-1)
                  (return (append (nreverse unified-list) list-2)))
                 (t
                  (return (append (nreverse unified-list) list-1))))))))

(defun make-segment-array (table)
  "Constructor for the segment-array class."
  (let ((table-length (length table)))
    (make-instance 'segment-array
      :length table-length
      :array (make-array `(,table-length 2)
                         :initial-contents table))))

;;; This is called when constructing the segment array and to get a list of strings
;;; to style. When styling dynamically, cull the string list. When constructing the 
;;; segment array, don't.
;;;
(defun create-string-list (start end  &optional styling-p)
  "Return a list of the form, (start end), for each string in buffer.
The list is in reverse order."
  (flet ((semi-colon-commented-p (pos)
           (do* ((start (mark-move pos 0) (nmark-next start))
                 (char (mark-char start) (mark-char start)))
                ((mark>= start pos))
             (when (char= char #\;) (return-from semi-colon-commented-p t))))
         (sharp-stroke-commented-p (pos)
           (do ((start (clone pos) (nmark-prev start))
                (char (mark-char start) (mark-char start))
                (char-minus-one 
                 (when (>= (mark-charpos start) 1) (mark-char (mark-prev pos)))
                 (when (>= (mark-charpos start) 1) (mark-char (mark-prev pos)))))
               ((or (= (mark-charpos start) 1)
                    (and (char= char #\#) (char= char-minus-one #\|))))
             (when (and (char= char #\|) 
                        (char= char-minus-one #\|))
               (return-from sharp-stroke-commented-p t)))))
    (do* ((position (clone start))
          string-list string-end)
         ((or (null position) (mark>= position end)) string-list)
      (cond ((and (eql (mark-char position) #\") 
                  (not (eql (mark-char (if (> (mark-charpos position) 0)
                                         (mark-prev position)
                                         position)) #\\))
                  ;; Too expensive; may have a rare mis-styled file
                  ;; because of an unmatched quote in a sharp-comment.
                  ;; (not (sharp-stroke-commented-p position))
                  (not (semi-colon-commented-p position)))
             (setf string-end (sexpr-end position))
             (cond ((and string-end (mark<= string-end end))
                    ;; Support for dynamic styling - only cull the string list
                    ;; when styling strings, not when constructing the segment array
                    (if *inc-p* 
                      (if styling-p
                        ;; cull
                        (when (and (mark>= *inc-pos* position)
                                   (mark<= *inc-pos* string-end))
                          (push (list position string-end) string-list))
                        (push (list position string-end) string-list))
                      (push (list position string-end) string-list))
                    (setf position (clone string-end)))
                   (t 
                    (return string-list))))
            (t 
             (nmark-next position))))))

;;; This is only called by get-combined-segment-list, when doing vanilla styling.
(defun create-semi-colon-comment-list (start end )
   "Return a list of the form, (start end), for each comment in buffer."
   (do* ((position (clone start))
         comment-list comment-end)
        ((or (null position) (mark> position end)) (nreverse comment-list))
      (cond ((and (eql (mark-char position) #\;) 
                  (mark> position (buf-start-mark)) ; *** mode line ???
                  (not (eql (mark-char (mark-prev position)) #\\)))
              (setf comment-end (line-end (clone position)))
              (cond ((and comment-end (mark<= comment-end end))
                      (push (list (clone position) (mark-next comment-end)) comment-list)
                      (setf position (mark-next comment-end)))
                     (t ; hum ...
                      (setf position (mark-next position)))))
             (t
              (setf position (mark-next position))))))

;;; This is only called by get-combined-segment-list, when doing vanilla styling.
(defun create-sharp-stroke-comment-list (start end )
  "Return a list of the form, (start end), for each comment in buffer."
  (do* ((position (clone start))
        comment-list comment-end)
       ((or (null position) (mark> position end)) (nreverse comment-list))
    (cond ((and (eql (mark-char position) #\#)
                (eql (mark-char (mark-next position)) #\|)
                (mark> position (buf-start-mark))
                (not (eql (mark-char (mark-prev position)) #\\)))
           (setf comment-end (pattern-search position *stroke-sharp-forward-pattern* end))
           (cond ((and comment-end (mark<= comment-end end))
                  (push (list position comment-end) comment-list)
                  (setf position (mark-next comment-end)))
                 (t 
                  (return (nreverse comment-list)))))
          (t
           (setq position (mark-next position))))))

;;; This is only called by get-combined-segment-list, when doing vanilla styling.
(defun create-cocoa-syntax-list (start end pattern)
  "Return a list of the form, (start end), for each Cocoa function name in buffer."
  (do* ((position (pattern-search (clone start) pattern end)
                  (pattern-search (clone name-end) pattern end))
        (name-end (when position (sexpr-end position)) (when position (sexpr-end position)))
        name-list)
       ((or (null position) (null name-end) (mark> position end)) (nreverse name-list))
    (push (list position name-end) name-list)))

(defMethod not-embedded-in-segment-p ((array segment-array) position)
  ;; Do a binary search of the segment-array to see if the position is embedded.
  #+sax-debug (when *not-embedded-in-segment-p-debug*
               (debug-out "~%~%~S" 'not-embedded-in-segment-p)
               (dump-segment-array array)
               (debug-out "~%position: ~S" position))
  (when (or (zerop (segment-array-length array)) (null position))
    (return-from not-embedded-in-segment-p t))
  (do* ((top (1- (segment-array-length array)))
        (bottom 0)
        (index (truncate (+ bottom top) 2) (truncate (+ bottom top) 2)))
       ((< top bottom) t)
    (when (and (mark< (aref (segment-array-array array) index 0) position)
               (mark> (aref (segment-array-array array) index 1) position))
      ;; embedded - return the end of the containing segment as the second value:
      (return (values nil (aref (segment-array-array array) index 1))))
    (cond ((mark<= position (aref (segment-array-array array) index 0))
           (setf top (1- index)))
          ((mark>= position (aref (segment-array-array array) index 1))
           (setf bottom (1+ index)))
          (t (error "~&Bad value in binary search: ~a" position)))))

(defun embedded-in-segment-p (pos)
  (when *segment-array*
    (multiple-value-bind (not-embedded-p end-of-segment)
                         (not-embedded-in-segment-p *segment-array* pos)
      (values (not not-embedded-p) end-of-segment))))

(defun style-strings (&optional (start (buf-start-mark)) (end (buf-end-mark))
                                &aux string-instances)
  #+sax-debug (when *style-strings-debug*
               (debug-out "~%~%~S" 'style-strings))
  (setf *segment-list* (create-string-list start end *inc-p*))
  (do* ((string-list *segment-list* (rest string-list))
        (start-string (first (first string-list)) (first (first string-list)))
        (end-string (second (first string-list)) (second (first string-list))))
       ((null start-string))
    (push (make-instance 'styled-string
            :string-start start-string
            :string-end end-string)
          string-instances))
  ;; Create the segment array - if styling dynamically.
  ;; Create the inclusive string list for the segment array.
  (setf *segment-array* (make-segment-array 
                         (if *inc-p*
                           (setf *segment-list* (nreverse (create-string-list start end)))
                           (setf *segment-list* (nreverse *segment-list*)))))
  (dolist (string string-instances)
    (style-string string))
  string-instances)

(defun style-semi-colon-comments (&optional (start (buf-start-mark)) (end (buf-end-mark)))
  #+sax-debug (when *style-semi-colon-comments-debug*
                (debug-out "~%~%~S" 'style-semi-colon-comments))
  (let ((comment-instances nil)
        (comment-segment-list nil))
    (do* ((start-comment (pattern-search start *semicolon-forward-pattern* end)
                         (pattern-search end-comment *semicolon-forward-pattern* end))
          (end-comment (when start-comment (line-end (clone start-comment)))
                       (when start-comment (line-end (clone start-comment)))))
         ((or (not start-comment)
              (not end-comment)
              (mark> start-comment end)))
      #+sax-debug (when *style-semi-colon-comments-debug*
                   (debug-out "~%start-comment: ~S" start-comment)
                   (debug-out "~%end-comment: ~S" end-comment))

      ;; The first AND handles the case where a string spans two comments. 
      (when (or (and (mark= start-comment (mark-line-start start-comment))
                     (or (not *inc-p*)
                         (and *inc-p* 
                              (mark>= *inc-pos* start-comment)
                              (mark<= (mark-prev *inc-pos*) end-comment))))
                ;; with dynamically-style-comments *segment-array* may not be there yet.
                (and (not (embedded-in-segment-p start-comment))
                     (not (and (>= (mark-charpos start-comment) 2)
                               (eq (mark-char start-comment -1) #\\)
                               (eq (mark-char start-comment -2) #\#)))))
        ;; Need the entire segment array for accurate parsing, even when
        ;; not styling this comment:
        (push (list start-comment end-comment) comment-segment-list)
        (when (or (not *inc-p*)
                  (and *inc-p* 
                       (mark>= *inc-pos* start-comment)
                       (mark<= (mark-prev *inc-pos*) end-comment)))
          (push (make-instance 'styled-semi-colon-comment 
                  :comment-start start-comment
                  :comment-end end-comment)
                comment-instances))))
    (setf *segment-list* 
          (unify-segment-lists (nreverse comment-segment-list) *segment-list*))
    (setf *segment-array* (make-segment-array *segment-list*))
    (setf comment-instances (nreverse comment-instances))
    (dolist (comment comment-instances)
      (style-comment comment))
    comment-instances))

(defun style-sharp-comments (&optional (start (buf-start-mark)) (end (buf-end-mark)))
  (flet ((find-end-comment (start-comment)
           (do* ((level-count 1)
                 (next-end-comment (pattern-search start-comment *stroke-sharp-forward-pattern* end)
                                   (when next-start-comment
                                     (pattern-search (nmark-offset next-start-comment 2) *stroke-sharp-forward-pattern* end)))
                 (next-start-comment (pattern-search (nmark-offset start-comment 2) *sharp-stroke-forward-pattern* end)
                                     (when next-start-comment
                                       (pattern-search (nmark-offset next-start-comment 2) *sharp-stroke-forward-pattern* end))))
                ((null next-end-comment))
             (when (and next-start-comment (mark< next-start-comment next-end-comment))
               ;; nested
               (incf level-count))
             (decf level-count)
             (when (= level-count 0) (return next-end-comment)))))
    (let ((comment-instances nil)
          (comment-segment-list nil))
      (do* ((start-comment (pattern-search start *sharp-stroke-forward-pattern* end)
                           (pattern-search end-comment *sharp-stroke-forward-pattern* end))
            (end-comment (when (and start-comment (mark<= start-comment end)) ; *** redundant
                           (find-end-comment start-comment))
                         (when (and start-comment (mark<= start-comment end))
                           (find-end-comment start-comment))))
           ((or (not start-comment) 
                (not end-comment)))
        (cond ((and (not-embedded-in-segment-p *segment-array* start-comment)
                    (not-embedded-in-segment-p *segment-array* end-comment)
                    (or (not *inc-p*)
                        (and *inc-p* 
                             (mark>= *inc-pos* start-comment)
                             (mark<= (mark-offset *inc-pos* -3) end-comment))))
               (push (list start-comment end-comment) comment-segment-list)
               (push (make-instance 'styled-sharp-comment 
                       :comment-start (mark-offset start-comment -2)
                       :comment-end (mark-offset end-comment 2))
                     comment-instances))))
      (when comment-instances
        (setf *segment-list* (unify-segment-lists (nreverse comment-segment-list) *segment-list*))
        (setf *segment-array* (make-segment-array *segment-list*))
        (setf comment-instances (nreverse comment-instances))
        (dolist (comment comment-instances)
          (style-comment comment))
        comment-instances))))

(defun style-comments (start end)
  (style-strings start end)
  (style-semi-colon-comments start end)
  (style-sharp-comments start end))

(defun dynamically-style-comments (start end style-strings-p style-semi-colon-comments-p)
  #+sax-debug (when *dynamically-style-comments-debug*
                (debug-out "~%~%~S" 'dynamically-style-comments))
  (let ((hi::*current-buffer* *buf*))
    (hemlock::parse-over-block (mark-line start) (mark-line end))
    (when style-strings-p (style-strings start end))
    (when style-semi-colon-comments-p 
      ;; (style-semi-colon-comments (mark-line-start end) end))))
      ;; Start is necessary to generate an complete segment-array for subsequent styling:
      (style-semi-colon-comments start end))))

;;; *** this needs to use start and end
(defun get-combined-segment-list ()
  (let* ((start (buf-start-mark))
         (end (buf-end-mark))
         (string-list (nreverse (create-string-list start end)))
         (semi-colon-comment-list (create-semi-colon-comment-list start end))
         (sharp-stroke-comment-list (create-sharp-stroke-comment-list start end))
         (cocoa-function-list (create-cocoa-syntax-list start end *sharp-slash-forward-pattern*))
         (cocoa-constant1-list (create-cocoa-syntax-list start end *sharp-dollar-forward-pattern*))
         (cocoa-constant2-list (create-cocoa-syntax-list start end *sharp-ampersand-forward-pattern*))
         (cocoa-constant3-list (create-cocoa-syntax-list start end *colon-lessthan-forward-pattern*))
         (cocoa-constant4-list (create-cocoa-syntax-list start end *sharp-backslash-forward-pattern*)))
    (unify-segment-lists 
     string-list 
     (unify-segment-lists 
      cocoa-constant1-list
      (unify-segment-lists 
       cocoa-constant2-list
       (unify-segment-lists 
        cocoa-constant3-list
        (unify-segment-lists 
         cocoa-constant4-list
         (unify-segment-lists 
          cocoa-function-list
          (unify-segment-lists 
           semi-colon-comment-list
           sharp-stroke-comment-list)))))))))




