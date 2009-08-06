(in-package "HI")

(defun make-empty-charprops-changes (&optional (size 2))
  (make-array size :adjustable t :fill-pointer 0))

(defun insert-charprops-change (changes index new)
  "Insert a new change into the charprops changes vector at index.  Objects
   at and beyond INDEX are shifted right to make room."
  (vector-push-extend nil changes)
  (replace changes changes :start1 (1+ index) :start2 index)
  (setf (aref changes index) new))

(defun delete-charprops-change (changes index)
  "Delete the change at index from the charprops changes vector.  Objects
   beyond INDEX are shifted left to fill the empty spot."
  (unless (= (fill-pointer changes) index)
    (replace changes changes :start1 index :start2 (1+ index)))
  (decf (fill-pointer changes)))

(defun push-charprops-change (change changes)
  (vector-push-extend change changes))

;;; Return the index of the charprops change that applies to
;;; charpos, or else NIL if the charpos has (implicit)
;;; default properties.
(defun charprops-change-index-for-position (changes charpos)
  (do ((i 0 (1+ i))
       (i-1 nil i)
       (change nil))
      ((= i (length changes)) i-1)
    (setq change (aref changes i))
    (when (< charpos (charprops-change-index change))
      (return i-1))))

;;; Real programmers can write assembly in any language.
(defun line-charprops-for-position (line charpos)
  "Return, as multiple values, the plist, and start position and end
   position over which the plist applies."
  (unless (and (>= charpos 0)
               (<= charpos (line-length line)))
    (error "index ~d out of range" charpos))
  (let* ((changes (line-charprops-changes line))
         (change nil)
         (prior-change nil)
         (start-pos 0)
         (end-pos 0))
    (dotimes (i (length changes) (values (and change
                                              (charprops-change-plist change))
                                         start-pos (1+ (line-length line))))
      (setq prior-change change)
      (setq change (aref changes i))
      (setq end-pos (charprops-change-index change))
      (when (< charpos (charprops-change-index change))
        (return (values (and prior-change (charprops-change-plist prior-change))
                        start-pos end-pos)))
      (setq start-pos (charprops-change-index change)))))

(defun squeeze-out-superseded-changes (changes idx)
  (let* ((base-change (aref changes idx)))
    (do* ((i (1+ idx) (1+ i))
          (change nil)
          (n 0))
         ((= i (length changes))
          ;;(format t "~&start1 = ~d start2 = ~d" (1+ idx) (+ n 1 idx))
          (replace changes changes :start1 (1+ idx) :start2 (+ n 1 idx))
          (decf (fill-pointer changes) n)
          changes)
      (setq change (aref changes i))
      (when (<= (charprops-change-index change)
                (charprops-change-index base-change))
        (setf (charprops-change-plist base-change) (charprops-change-plist change))
        (incf n)
        (setf (aref changes i) nil)))))

;;; Set the charprops of the specified LINE between positions START and
;;; END.  If END is NIL, that means the end of the line.  Note that this
;;; doesn't merge in properties: it replaces whatever is there.
(defun set-line-charprops (line charprops &key (start 0) end &aux plist)
  (setq plist (charprops-as-plist charprops))
  (when (and end (= end (line-length line)))
    (setq end nil))
  (when (and (null plist) (= start 0) (null end))
    (setf (line-charprops-changes line) nil)
    (return-from set-line-charprops))
  (let* ((changes (line-charprops-changes line))
         (new-change (make-charprops-change start plist)))
    (if (null changes)
      (let ((new-changes (make-array 2 :adjustable t :fill-pointer 0)))
        (vector-push new-change new-changes)
        (when end
          (vector-push (make-charprops-change end nil) new-changes))
        (setf (line-charprops-changes line) new-changes)
        (line-charprops-changes line))
      ;; Put the new charprops change into the right place in the charprops
      ;; changes vector, making note of its position.
      (do* ((i 0 (1+ i))
            (change nil)
            (prior-change nil change))
           ((= i (length changes))
            (insert-charprops-change changes i new-change)
            (when end
              (let ((prior-plist (and prior-change
                                      (charprops-change-plist prior-change))))
                (insert-charprops-change changes (1+ i)
                                         (make-charprops-change end prior-plist)))))
        (setq change (aref changes i))
        (when (<= start (charprops-change-index change))
          (insert-charprops-change changes i new-change)
          (incf i)
          (if (null end)
            (setf (fill-pointer changes) i)
            (let ((prior-plist (and prior-change
                                    (charprops-change-plist prior-change))))
              (insert-charprops-change changes i (make-charprops-change end prior-plist))
              (squeeze-out-superseded-changes changes i)))
          (return))))))

(defun add-line-charprop-value (line name value &key (start 0) end)
  (let* ((changes (line-charprops-changes line))
         (start-idx (charprops-change-index-for-position changes start))
         (end-idx (charprops-change-index-for-position changes
                                                       (or end
                                                           (setq end (line-length line))))))
    (cond ((or (null changes)
               (and (null start-idx) (null end-idx)))
           ;; Either the line has no existing charprops, or we're within the
           ;; implicit run of default properties at the start of the line.
           ;; Just set the charprops over the relevant range and return.
           (set-line-charprops line (list name value) :start start :end end)
           (return-from add-line-charprop-value changes))
          ((null start-idx)
           ;; The starting position is in the implicit run of default
           ;; properties at the start of the line.
           (let ((new-change (make-charprops-change start (list name value))))
             (insert-charprops-change changes 0 new-change)
             (setq start-idx 0)
             (incf end-idx))
           (let ((end-change (aref changes end-idx)))
             (unless (= (charprops-change-index end-change) end)
               (let ((new-change (copy-charprops-change end-change)))
                 (setf (charprops-change-index new-change) end)
                 (insert-charprops-change changes (1+ end-idx) new-change)
                 (incf end-idx)))))
          ((and start-idx end-idx)
           (let ((start-change (aref changes start-idx)))
             (unless (= (charprops-change-index start-change) start)
               (let ((new-change (copy-charprops-change start-change)))
                 (setf (charprops-change-index new-change) start)
                 (insert-charprops-change  changes (1+ start-idx) new-change)
                 (incf start-idx)
                 (incf end-idx))))
           (let ((end-change (aref changes end-idx))
                 (next-end-idx (charprops-change-index-for-position changes (1+ end))))
             ;; If end-idx and next-end-idx differ, then the end
             ;; position comes at the very end of a run, and we don't
             ;; need to split.  We also don't need to split if end is
             ;; at the very end of the line.
             (when (and (= end-idx next-end-idx)
                        (not (= end (line-length line))))
               (let ((new-change (copy-charprops-change end-change)))
                 (setf (charprops-change-index new-change) end)
                 (insert-charprops-change changes (1+ end-idx) new-change)))))
          (t (error "how did we get here?")))
    (loop for i from start-idx to end-idx
      as change = (aref changes i)
      do (if (null value)
           (remf (charprops-change-plist change) name)
           (setf (getf (charprops-change-plist change) name) value)))))

(defun set-region-charprops (region charprops)
  (let* ((start (region-start region))
         (end (region-end region))
         (first-line (mark-line start))
         (last-line (mark-line end)))
    (cond ((eq first-line last-line)
           (set-line-charprops first-line charprops :start (mark-charpos start)
                               :end (mark-charpos end))
	   (coalesce-line-charprops first-line))
          (t
           (set-line-charprops first-line charprops :start (mark-charpos start))
           (do* ((line (line-next first-line) (line-next line)))
                ((eq line last-line)
                 (set-line-charprops line charprops :end (mark-charpos end)))
             (set-line-charprops line charprops))))))

;;; Returns two values: fresh charprops change vectors for the line's characters
;;; before and after charpos.
(defun split-line-charprops (line charpos)
  (let ((changes (line-charprops-changes line)))
    (when changes
      (let ((left (make-array 2 :adjustable t :fill-pointer 0))
            (right (make-array 2 :adjustable t :fill-pointer 0))
            (pivot nil)
            (prior-change nil))
        (do* ((i 0 (1+ i))
              (change nil))
             ((or pivot
                  (= i (length changes)))
              (if (null pivot)
                ;; The last change extends to the end of line, so that will be the
                ;; charprops in effect at the beginning of the new line.
                (if (null (charprops-change-plist change))
                  (setq right nil)
                  (let* ((c (copy-charprops-change change)))
                    (setf (charprops-change-index c) 0)
                    (push-charprops-change c right)))
                ;; Some charprops changes remain.  First, split the prior change
                ;; if necessary, and then pick up the rest of the shifts.
                (progn
                  (when (and prior-change
                             (> charpos (charprops-change-index prior-change)))
                    ;; need to split change
                    (let* ((c (copy-charprops-change prior-change)))
                      (setf (charprops-change-index c) 0)
                      (push-charprops-change c right)))
                  (loop for i from pivot below (length changes)
                    as change = (aref changes i)
                    do (decf (charprops-change-index change) charpos)
                    (push-charprops-change (aref changes i) right))))
              (values left right pivot))
          (setq change (aref changes i))
          (if (< (charprops-change-index change) charpos)
            (progn
              (push-charprops-change change left)
              (setq prior-change change))
            (setq pivot i)))))))

(defun append-line-charprops (line changes)
  (let* ((left (line-charprops-changes line))
         (len (line-length line))
         (right changes))
    (cond ((and left right)
           (loop for c across right
                 for new-change = (copy-charprops-change c)
                 do (incf (charprops-change-index new-change) len)
                    (push-charprops-change new-change left)))
          ((and (null left) right)
           (setq left (copy-charprops-changes right))
           (adjust-charprops-change-indexes left len)
           (setf (line-charprops-changes line) left))
          ((and left (null right))
           (push-charprops-change (make-charprops-change len nil) left)))
    left))

;;; Append the charprops-changes from line2 onto line1, modifying their
;;; indexes appropriately.
(defun join-line-charprops (line1 line2)
  (let* ((left (line-charprops-changes line1))
         (lidx (1- (length left)))
         (right (line-charprops-changes line2))
         (ridx 0)
         (line1-len (line-length line1)))
    (cond ((and left right)
           ;; If the last change on line1 and the first change on line2
           ;; represent the same properties, then omit line2's first
           ;; change.
           (let* ((lchange (aref left lidx))
                  (lprops (charprops-change-plist lchange))
                  (rchange (aref right ridx))
                  (rprops (charprops-change-plist rchange)))
             (if (> 0 (charprops-change-index rchange))
               ;; There is an implicit run of default charprops at the
               ;; start of the line.
               (unless (null lprops)
                 ;; The last change on line1 represents some non-default
                 ;; set of charprops, so insert an explicit change to the
                 ;; default set before copying over the rest.
                 (push-charprops-change (make-charprops-change (1+ line1-len) nil)
                                        left))
               (when (charprops-equal lprops rprops)
                 (incf ridx)))
             (do* ((i ridx (1+ i))
                   (change nil))
                  ((= i (length right)))
               (setq change (aref right i))
               (incf (charprops-change-index change) (1+ line1-len))
               (push-charprops-change change left))))
          ((and (null left) right)
           (adjust-charprops-change-indexes right line1-len)
           (setf (line-charprops-changes line1) right))
          ((and left (null right))
           (let* ((lchange (aref left lidx)))
             (unless (null (charprops-change-plist lchange))
               (push-charprops-change (make-charprops-change (1+ line1-len) nil)
                                   left))))
          ;; otherwise both nil, so don't need to do anything.
          )
    left))

(defun copy-line-charprops (line &key (start 0) end)
  "Return a freshly-consed vector of charprops changes that applies to the
   characters in the interval [start, end) on the specified line.  If the
   charprops in between start and end are the default charprops, return
   NIL."
  (let ((changes (line-charprops-changes line)))
    ;; some early-out special cases
    (cond ((null changes)
           (return-from copy-line-charprops))
          ((and (= start 0) (or (= 0 end) (null end)))
           (return-from copy-line-charprops (copy-charprops-changes changes))))
    (unless end
      (setq end (line-length line)))
    (let* ((new-changes (make-empty-charprops-changes))
           (start-idx (charprops-change-index-for-position changes start))
           (end-idx (charprops-change-index-for-position changes (1- end))))
      (if (eql start-idx end-idx)
        (if (null start-idx)
          (setq new-changes nil)
          (let* ((change (aref changes start-idx))
                 (plist (charprops-change-plist change)))
            (if (null plist)
              (setq new-changes nil)
              (push-charprops-change (make-charprops-change start plist)
                                     new-changes))))
        (do ((i (or start-idx 0) (1+ i)))
            ((> i end-idx))
          (let* ((change (aref changes i))
                 (index (charprops-change-index change))
                 (plist (charprops-change-plist change)))
          (push-charprops-change (make-charprops-change
                                  (max 0 (- index start)) plist)
                                 new-changes))))
      new-changes)))

(defun delete-line-charprops (line &key (start 0) end)
  (let ((changes (line-charprops-changes line)))
    ;; some early-out special cases
    (cond ((null changes)
           (return-from delete-line-charprops))
          ((and (= start 0) (null end))
           (setf (line-charprops-changes line) nil)
           (return-from delete-line-charprops)))
    (unless end
      (setq end (line-length line)))
    (assert (<= start end) (start end))
    (let* ((start-idx (charprops-change-index-for-position changes start))
           (end-idx (charprops-change-index-for-position changes (1- end))))
      (cond ((null start-idx)
             (if (null end-idx)
               (adjust-charprops-change-indexes changes (- start end) :start 0)
               (progn
                 ;; delete changes before end-idx
                 (replace changes changes :start1 0 :start2 end-idx)
                 (decf (fill-pointer changes) end-idx)
                 (setf (charprops-change-index (aref changes 0)) start)
                 ;; move back start of subsequent changes, if there are any
                 (when (> (length changes) 1)
                   (adjust-charprops-change-indexes changes (- start end)
                                                    :start 1)
                   ;; if the change is now zero-length, remove it
                   (when (= (charprops-change-index (aref changes 0))
                            (charprops-change-index (aref changes 1)))
                     (delete-charprops-change changes 0))))))
            ((eql start-idx end-idx)
             ;; The deletion takes place within the scope of a single
             ;; charprops run.
             ;; Move back start of subsequent changes, if there are any
             (when (> (length changes) (1+ start-idx))
               (adjust-charprops-change-indexes changes (- start end)
                                                :start (1+ start-idx))
               ;; if the change is now zero-length, remove it
               (when (= (charprops-change-index (aref changes start-idx))
                        (charprops-change-index (aref changes (1+ start-idx))))
                 (delete-charprops-change changes start-idx))))
            (t
             ;; Remove changes between start-idx and and end-idx.
             (replace changes changes :start1 (1+ start-idx)
                      :start2 end-idx)
             (decf (fill-pointer changes) (- end-idx (1+ start-idx)))
             (setf (charprops-change-index (aref changes (1+ start-idx))) start)
             (when (> (length changes) (1+ start-idx))
               (adjust-charprops-change-indexes changes (- start end)
                                                :start (+ 2 start-idx))
               ;; if first change is now zero-length, remove it
               (when (= (charprops-change-index (aref changes start-idx))
                        (charprops-change-index (aref changes (1+ start-idx))))
                 (delete-charprops-change changes start-idx))))))
    (coalesce-line-charprops line)))

;;; Coalesce adjacent changes with CHARPROP-EQUAL plists.
;;; Maybe make this remove zero-length changes, too?
(defun coalesce-line-charprops (line)
  (let ((changes (line-charprops-changes line)))
    (do* ((i 0 (1+ i))
          (change nil))
         ((>= i (length changes)))
      (setq change (aref changes i))
      (loop with j = (1+ i)
        while (and (< j (length changes))
                   (charprops-equal (charprops-change-plist change)
                                    (charprops-change-plist (aref changes j))))
        do (delete-charprops-change changes j)))
    ;; Elide any changes with NIL plists at the start of the line.
    (loop
      while (and (> (length changes) 0)
                 (null (charprops-change-plist (aref changes 0))))
      do (delete-charprops-change changes 0))
    (when (zerop (length changes))
      (setf (line-charprops-changes line) nil)))
  (line-charprops-changes line))
      
(defun adjust-charprops-change-indexes (changes delta &key (start 0))
  (do* ((i start (1+ i))
        (change nil))
       ((>= i (length changes))
        changes)
    (setq change (aref changes i))
    (incf (charprops-change-index change) delta)))

;;; Add delta to the starting index of all charprops changes after the one
;;; containing start.
(defun adjust-line-charprops (line delta &key (start 0))
  (let* ((changes (line-charprops-changes line))
         (start-idx (charprops-change-index-for-position changes start)))
    (adjust-charprops-change-indexes changes delta :start (if start-idx
                                                            (1+ start-idx)
                                                            0))))

(defun apply-line-charprops (line changes start-pos end-pos)
  (cond ((null changes)
         (set-line-charprops line nil :start start-pos :end end-pos))
        (t
         (setq changes (copy-charprops-changes changes))
         (do* ((i 0 (1+ i))
               (change nil))
              ((= i (length changes)))
           (setq change (aref changes i))
           (set-line-charprops line (charprops-change-plist change)
                               :start (+ (charprops-change-index change) start-pos)
                               :end end-pos))
         (coalesce-line-charprops line)))
  (line-charprops-changes line))

(defvar *display-properties*
  '(:font-name
    :font-size
    :font-weight
    :font-width
    :font-slant
    :font-underline
    :font-color
    :background-color))

;;; Setting and accessing charprops

(defun next-charprop-value (mark name &key view)
  (let ((props (next-charprops mark :view view)))
    (getf props name)))

(defun previous-charprop-value (mark name &key view)
  (let ((props (previous-charprops mark :view view)))
    (getf props name)))

(defun set-charprop-value (mark name value &key (count 1 count-supplied-p) end view)
  (declare (ignore view))
  (when (and count-supplied-p end)
    (error "Cannot specify both :COUNT and :END"))
  (with-mark ((start-mark mark)
              (end-mark mark))
    (if end
      (move-mark end-mark end)
      (character-offset end-mark count))
    (let* ((start-line (mark-line start-mark))
           (start-charpos (mark-charpos start-mark))
           (end-line (mark-line end-mark))
           (end-charpos (mark-charpos end-mark)))
      (cond ((eq start-line end-line)
	     (add-line-charprop-value start-line name value
				      :start start-charpos
				      :end end-charpos))
	    (t
	     (do* ((line start-line (line-next line))
		   (start start-charpos 0))
		  ((eq line end-line)
		   (add-line-charprop-value end-line name value
					    :start 0
					    :end end-charpos))
	       (add-line-charprop-value line name value :start start))))
      (let ((n (count-characters (region start-mark end-mark)))
	    (buffer (line-%buffer start-line)))
	(buffer-note-modification buffer mark n)))))

(defun find-line-charprop-value (line name value &key (start 0) end)
  (unless end
    (setq end (line-length line)))
  (let* ((changes (line-charprops-changes line))
	 (start-idx (or (charprops-change-index-for-position changes start) 0))
	 (end-idx (or (charprops-change-index-for-position changes end) 0)))
    (when changes
      (loop for i from start-idx to end-idx
	 as change = (aref changes i)
	 as plist = (charprops-change-plist change)
	 as found-value = (getf plist name)
	 do (when (and found-value
		       (charprop-equal found-value value))
	      (return (max start (charprops-change-index change))))))))

(defun find-charprop-value (mark name value &key (count nil count-supplied-p)
			    end view from-end)
  (declare (ignore from-end view))
  (with-mark ((start-mark mark)
	      (end-mark mark))
    (when (and count-supplied-p end)
      (error "Cannot specify both :COUNT and :END"))
    (let* ((buffer (line-buffer (mark-line mark))))
      (unless (bufferp buffer)
	(error "text must be in a buffer"))
      (if count-supplied-p
	(character-offset end-mark count)
	(move-mark end-mark (buffer-end-mark buffer)))
      (let* ((start-line (mark-line start-mark))
	     (start-charpos (mark-charpos start-mark))
	     (end-line (mark-line end-mark))
	     (end-charpos (mark-charpos end-mark)))
	(do* ((line start-line (line-next line))
	      (charpos start-charpos 0))
             ((eq line end-line)
              (let ((pos (find-line-charprop-value end-line name value
                                                   :start charpos
                                                   :end end-charpos)))
                (when pos
                  (move-to-position mark pos end-line)
                  mark)))
	  (let ((pos (find-line-charprop-value line name value :start charpos)))
	    (when pos
	      (move-to-position mark pos line)
	      (return mark))))))))

(defun filter-match (filter name)
  (cond ((functionp filter)
         (funcall filter name))
        ((eq filter :display)
         (member name *display-properties* :test #'eq))
        ((typep filter 'sequence)
         (member name filter))
        (t
         name)))

(defun filter-charprops (filter charprops)
  (if (eq filter t)
    charprops
    (typecase charprops
      ((satisfies ccl::plistp) (loop for (k v) on charprops by #'cddr
                                 when (filter-match filter k)
                                 collect k and collect v))
      (hash-table (loop for k being the hash-keys of charprops using (hash-value v)
                    when (filter-match filter k)
                    collect k and collect v)))))

(defun next-charprops (mark &key view (filter t))
  "Return the properties of the character after MARK."
  (declare (ignore view))
  (when (next-character mark)
    (let* ((props (line-charprops-for-position (mark-line mark) (mark-charpos mark))))
      (filter-charprops filter props))))

(defun previous-charprops (mark &key view (filter t))
  "Return the properties of the character before MARK."
  (with-mark ((m mark))
    (when (mark-before m)
      (next-charprops m :view view :filter filter))))

#|
(defun set-charprops (mark charprops &key (count 1 count-supplied-p)
                           (end nil end-supplied-p) (filter charprops-names charprops))
  (declare (ignore filter end count charprops mark))
  (when (and count-supplied-p end-supplied-p)
    (error "Only one of count or end can be supplied."))
  (setq charprops (charprops-as-plist charprops :filter filter))
  (with-mark ((start-mark mark)
              (end-mark mark))
    (if end
      (move-mark end-mark end)
      (character-offset end-mark count))
    (let* ((start-line (mark-line start-mark))
           (start-charpos (mark-charpos start-mark))
           (end-line (mark-line end-mark))
           (end-charpos (mark-charpos end-mark)))
      (cond ((eq start-line end-line)

|#

;;; Return a list of charprops-change vectors that correspond to the lines
;;; of text in the region defined by the paramaters.
(defun charprops-in-region (region-or-mark &key (count 1 count-supplied-p)
                                           end filter)
  (declare (ignore filter))
  (when (and count-supplied-p end)
    (error "Only one of count or end can be supplied."))
  (let (region result)
    (etypecase region-or-mark
      (mark (with-mark ((m region-or-mark))
	      (when end
		(setq count (- end (mark-absolute-position m))))
	      (character-offset m count)
	      (setq region (region region-or-mark m))))
      (region (when (or count-supplied-p end)
                (error "Can't specify count or end when passing in a region."))
              (setq region region-or-mark)))
    (let* ((start (region-start region))
           (first-line (mark-line start))
           (first-charpos (mark-charpos start))
           (end (region-end region))
           (last-line (mark-line end))
           (last-charpos (mark-charpos end)))
      (cond
       ((eq first-line last-line)
        (list (copy-line-charprops first-line :start first-charpos)))
       (t
        (push (copy-line-charprops first-line :start first-charpos) result)
        (do* ((line (line-next first-line) (line-next line))
              (m (copy-mark start) (line-start m line)))
             ((eq line last-line)
              (push (copy-line-charprops last-line :end last-charpos) result)
              (nreverse result))
          (push (copy-line-charprops line) result)))))))

(defun apply-charprops (mark charprops-range &key filter from-end)
  (declare (ignore from-end filter charprops-range mark)))

#|
  (let* ((start-line (mark-line mark))
         (start-charpos (mark-charpos))
         (nlines (length charprops-range))
         (first-changes (pop charprops-range)))

    ;; do possibly-partial first line
    (let ((left (split-line-charprops start-line start-charpos)))
      (setf (line-charprops start-line) left)
      (append-line-charprops start-line first-changes))
    ;; do some number of whole lines
    (do* ((line (line-next start-line) (line-next line))
          (previous-line start-line (line-next previous-line))
          (cc-list charprops-range (cdr charprops-range))
          (changes (car cc-list) (car cc-list)))
         ((or (null line) (endp cc-list)))
      (setf (line-charprops-changes line) (copy-charprops-changes changes)))
    ;; I don't know what to do about a partial last line.  There's no
    ;; way that I can see to know whether the last charprops change vector
    ;; in the charprops-range list is to apply to an entire line or to end
    ;; at a particular charpos on that line.  Maybe that information needs
    ;; to be stored as part of the charprops-range list.  For example, if the
    ;; element of the charprops-range list is a non-null list, the list could
    ;; be (charprops-change-vector start-charpos end-charpos).

    (multiple-value-bind (left right)
                         (split-line-charprops last-line last-charpos)
      (setf (line-charprops last-line) last-changes)
      (append-line-charprops last-line right)))
|#

(defun find-charprops (mark charprops &key count end view filter from-end)
  (declare (ignore from-end filter view end count charprops mark)))

(defun find-charprops-change (mark &key count end view filter from-end)
  (declare (ignore from-end filter view end count))
  (let* ((line (mark-line mark))
         (charpos (mark-charpos mark))
         (changes (line-charprops-changes line))
         (idx (charprops-change-index-for-position changes charpos)))
    (loop
      (incf idx)
      (if (= idx (length changes))
        (setf line (line-next line)
              charpos 0
              changes (line-charprops-changes line)
              idx (charprops-change-index-for-position changes charpos))
        (return (move-mark mark (charprops-change-index (aref changes idx))))))))

(defun print-line-charprops (line &key (start 0) (end (hi:line-length line)))
  (let* ((string (hi:line-string line))
         (charprops-changes (hi::line-charprops-changes line)))
    (let ((index start)
          (plist nil)
          (x 0))
      (loop for change across charprops-changes
        do (let* ((next-index (charprops-change-index change))
                  (next-plist (charprops-change-plist change))
                  (end (min end next-index)))
             (when (and (>= index start)
                        (< index end))
               (format t "~& ~d: [~d, ~d) ~s: ~s" x index end
                       (subseq string index end) plist))
             (setq index next-index)
             (setq plist next-plist)
             (incf x)))
      ;; final part of line
      (format t "~& ~d: [~d, ~d) ~s: ~s" x index end
              (subseq string index end) plist))))

(defun copy-charprops (charprops)
  (copy-list charprops))


;;; Utility functions

(defun charprop-equal (value1 value2)
  (cond ((and (stringp value1) (stringp value2))
         (string= value1 value2))
        ((and (numberp value1) (numberp value2))
         (= value1 value2))
        (t
         (eql value1 value2))))

(defun charprops-get (charprops name &key (filter t))
  (when (and name (filter-match filter name))
    (etypecase charprops
      ((satisfies ccl::plistp) (getf charprops name))
      (hash-table (gethash name charprops)))))

(defun charprops-set (charprops name value)
  (etypecase charprops
    ((satisfies ccl::plistp) (setf (getf charprops name) value))
    (hash-table (setf (gethash name charprops) value)))
  charprops)

(defun same-sets (s1 s2 &key (test #'eql))
  (and (subsetp s1 s2 :test test)
       (subsetp s2 s1 :test test)))

;; I wonder if this will be a hot spot...
(defun charprops-equal (charprops1 charprops2 &key (filter t))
  (setq charprops1 (charprops-as-plist charprops1 :filter filter)
        charprops2 (charprops-as-plist charprops2 :filter filter))
  (let (keys1 values1 keys2 values2)
    (loop for (k1 v1) on charprops1 by #'cddr
      do (push k1 keys1)
         (push v1 values1))
    (loop for (k2 v2) on charprops2 by #'cddr
      do (push k2 keys2)
         (push v2 values2))
    (and (same-sets keys1 keys2)
         (same-sets values1 values2 :test #'charprop-equal))))

(defun charprops-as-plist (charprops &key (filter t))
  (etypecase charprops
    ((satisfies ccl::plistp) (if (eq filter t)
                               charprops
                               (loop for (k v) on charprops by #'cddr
                                 when (filter-match filter k)
                                 collect k and collect v)))
    (hash-table (loop for k being the hash-keys of charprops using (hash-value v)
                  when (filter-match filter k)
                  collect k and collect v))))

(defun charprops-as-hash (charprops &key (filter t))
  (etypecase charprops
    ((satisfies ccl::plistp) (let ((hash (make-hash-table)))
                               (loop for (k v) on charprops by #'cddr
                                 when (filter-match filter k)
                                 do (setf (gethash k hash) v))
                               hash))
    (hash-table (if (eq filter t)
                  charprops
                  (let ((hash (make-hash-table)))
                    (maphash #'(lambda (k v)
                                 (when (filter-match filter k)
                                   (setf (gethash k hash) v)))
                             charprops))))))

(defun charprops-names (charprops &key (filter t))
  (etypecase charprops
    ((satisfies ccl::plistp) (loop for name in charprops by #'cddr
                               when (filter-match filter name)
                               collect name))
    (hash-table (loop for name being the hash-keys of charprops
                  when (filter-match filter name)
                  collect name))))

;;; From <AppKit/NSAttributedString.h>
(defparameter *cocoa-attributes*
  `((:ns-font . ,#&NSFontAttributeName)
    (:ns-paragraph-style . ,#&NSParagraphStyleAttributeName)
    (:ns-foreground-color . ,#&NSForegroundColorAttributeName)
    (:ns-underline-style . ,#&NSUnderlineStyleAttributeName)
    (:ns-superscript . ,#&NSSuperscriptAttributeName)
    (:ns-background-color . ,#&NSBackgroundColorAttributeName)
    (:ns-attachment . ,#&NSAttachmentAttributeName)
    (:ns-ligature . ,#&NSLigatureAttributeName)
    (:ns-baseline-offset . ,#&NSBaselineOffsetAttributeName)
    (:ns-kern . ,#&NSKernAttributeName)
    (:ns-link . ,#&NSLinkAttributeName)
    (:ns-stroke-width . ,#&NSStrokeWidthAttributeName)
    (:ns-stroke-color . ,#&NSStrokeColorAttributeName)
    (:ns-underline-color . ,#&NSUnderlineColorAttributeName)
    (:ns-strikethrough-style . ,#&NSStrikethroughStyleAttributeName)
    (:ns-strikethrough-color . ,#&NSStrikethroughColorAttributeName)
    (:ns-shadow . ,#&NSShadowAttributeName)
    (:ns-obliqueness . ,#&NSObliquenessAttributeName)
    (:ns-expansion . ,#&NSExpansionAttributeName)
    (:ns-cursor . ,#&NSCursorAttributeName)
    (:ns-tool-tip . ,#&NSToolTipAttributeName)
    #-cocotron
    (:ns-character-shape . ,#&NSCharacterShapeAttributeName)
    #-cocotron
    (:ns-glyph-info . ,#&NSGlyphInfoAttributeName)
    ;;(:ns-marked-clause-segment . #&NSMarkedClauseSegmentAttributeName)
    ;;(:ns-spelling-state . #&NSSpellingStateAttributeName)
    ))

