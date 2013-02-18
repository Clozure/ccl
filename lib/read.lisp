;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2009 Clozure Associates
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


                         


(defun signal-reader-error (input-stream format-string &rest format-args)
  (error 'simple-reader-error :stream input-stream
         :format-control format-string :format-arguments format-args))

#| ; Can't see any reason to leave this in
(defun read-file-to-list (file &aux result)
   ;(print-db (setq file (prepend-default-dir file)))   
   (with-open-file (stream file :direction :input)
       (setq result (read-file-to-list-aux stream)))
   result)

(defun read-file-to-list-aux (stream)
   (if (eofp stream)
        nil
       (let ((form (read stream nil *eof-value* nil)))
            ;(%print "just read " form)
           (if (eq form *eof-value*)
                nil
               (cons form (read-file-to-list-aux stream))))))
|#

(set-dispatch-macro-character #\# #\*
 (qlfun |#*-reader| (input-stream sub-char int 
   &aux list list-length array array-length last-bit)
  (declare (ignore sub-char))
  (do* ((char (read-char input-stream nil nil t)
              (read-char input-stream nil nil t))
        (attr (%character-attribute char (rdtab.ttab *readtable*))
              (%character-attribute char (rdtab.ttab *readtable*))))
       ((or (null char)
            (= $cht_tmac attr)
            (= $cht_wsp attr))
        (if char (unread-char char input-stream)))
    (let ((number (- (char-code char) 48)))
      (if (or (<= 0 number 1) *read-suppress*)
          (setq list (cons number list))
          (signal-reader-error input-stream "reader macro #* got illegal character ~S" char))))
  (setq last-bit (car list))
  (setq list (nreverse list))
  (setq list-length (list-length list))
  (if (not (integerp int))
      (setq int list-length))
  (cond (*read-suppress* nil)
        ((and (= 0 list-length) (> int 0))
         (signal-reader-error input-stream "reader macro #~S* needs something" int))
        ((> list-length int)
         (signal-reader-error input-stream "reader macro #~S* can't fit ~S" int list))
        (t (setq array-length (if int int list-length))
           (setq array (make-array array-length :element-type 'bit))
           (do ((i 0 (1+ i))
                (bit-list list (cdr bit-list)))
               ((>= i array-length))
             (aset array i (if bit-list
                               (car bit-list)
                               last-bit)))
           array))))

(set-dispatch-macro-character #\# #\A
 (qlfun |#A-reader| (stream ignore dimensions)
   (declare (ignore ignore))
   (cond (*read-suppress*
          (read stream () () t)
          nil)
         ((not dimensions)
          (signal-reader-error stream "reader macro #A used without a rank integer"))
         ((eql dimensions 0) ;0 dimensional array
          (make-array nil :initial-contents (read-internal stream t nil t)))
         ((and (integerp dimensions) (> dimensions 0)) 
          (let ((initial-contents (read-internal stream t nil t)))
            (cond ((not (typep initial-contents 'sequence))
                   (signal-reader-error stream "The form following a #~SA reader macro should have been a sequence, but it was: ~S" dimensions initial-contents))
                  ((= (length initial-contents) 0)
                   (make-array (make-list dimensions :initial-element 0)))
                  ((= dimensions 1)
                   (make-array (length initial-contents) :initial-contents initial-contents))
                  (t
                   (let ((dlist (make-list dimensions)))
                     (do ((dl dlist (cdr dl))
                          (il initial-contents (if (> (length il) 0)
                                                 (etypecase il
                                                   (list (car il))
                                                   ((vector t) (aref il 0))))))
                         ((null dl))
                       (rplaca dl (length il)))
                     (make-array dlist :initial-contents initial-contents))))))
         (t (signal-reader-error stream "Dimensions argument to #A not a non-negative integer: ~S" dimensions)))))

(set-dispatch-macro-character #\# #\S
  (qlfun |#S-reader| (input-stream sub-char int &aux list sd)
     (declare (ignore sub-char int))
     (setq list (read-internal input-stream t nil t))
     (unless *read-suppress*
       (unless (and (consp list)
                    (symbolp (%car list))
                    (setq sd (gethash (%car list) %defstructs%))
		    (setq sd (sd-constructor sd)))
         (error "Can't initialize structure from ~S." list))
       (let ((args ()) (plist (cdr list)))
         (unless (plistp plist) (report-bad-arg plist '(satisfies plistp)))
         (while plist
           (push (make-keyword (pop plist)) args)
           (push (pop plist) args))
         (apply sd (nreverse args))))))

;;;from slisp reader2.lisp, and apparently not touched in 20 years.
(defun parse-integer (string &key (start 0) end
                      (radix 10) junk-allowed)
  "Examine the substring of string delimited by start and end
  (default to the beginning and end of the string)  It skips over
  whitespace characters and then tries to parse an integer. The
  radix parameter must be between 2 and 36."
  (flet ((parse-integer-not-integer-string (s)
	   (error 'parse-integer-not-integer-string :string s)))
    (declare (inline parse-integer-not-integer-string))
    (unless (typep string 'string)
      (setq string (require-type string 'string)))
    (setq end (check-sequence-bounds string start end))
    (setq radix (%validate-radix radix))
    (let ((index (do ((i start (1+ i)))
		     ((= i end)
		      (if junk-allowed
                        (return-from parse-integer (values nil end))
                        (parse-integer-not-integer-string string)))
                   (unless (whitespacep (char string i)) (return i))))
        (minusp nil)
        (found-digit nil)
        (result 0))
       (let ((char (char string index)))
            (cond ((char= char #\-)
                   (setq minusp t)
                   (setq index (1+ index)))
                  ((char= char #\+)
                    (setq index (1+ index))
                   )))
       (loop
        (when (= index end) (return nil))
        (let* ((char (char string index))
               (weight (digit-char-p char radix)))
              (cond (weight
                     (setq result (+ weight (* result radix))
                                  found-digit t))
                    (junk-allowed (return nil))
                    ((whitespacep char)
                     (until (eq (setq index (1+ index)) end)
                       (unless (whitespacep (char string index))
                         (parse-integer-not-integer-string string)))
                     (return nil))
                    (t
                     (parse-integer-not-integer-string string))))
         (setq index (1+ index)))
       (values
        (if found-digit
            (if minusp (- result) result)
            (if junk-allowed
                nil
                (parse-integer-not-integer-string string)))
        index))))


(defun get-read-object (arg)
  (if (listp %read-objects%)
    (assoc arg %read-objects%)
    (gethash arg %read-objects%)))

(defparameter *read-objects-hash-threshold* 30)

(defun note-read-object (data)
  (let* ((lab (car data)))
    (if (get-read-object lab)
      (%err-disp $xduprdlbl lab)
      (if (listp %read-objects%)
        (if (< (length %read-objects%) *read-objects-hash-threshold*)
          (push data %read-objects%)
          (let* ((hash (make-hash-table :test #'eql :shared nil :rehash-size 2.0)))
            (dolist (info %read-objects%)
              (let* ((lab (car info)))
                (setf (gethash lab hash) info)))
            (setf (gethash lab hash) data)
            (setq %read-objects% hash)))
        (setf (gethash lab %read-objects%) data)))))


    

(set-dispatch-macro-character #\# #\#
  #'(lambda (stream char arg)
      (declare (ignore stream))
      (if *read-suppress* 
        nil
        (if arg
          (let ((pair (get-read-object arg)))
            (if pair
              (let* ((cell (cdr pair)))
                (setf (car cell) t)
                (cdr cell))
              (%err-disp $xnordlbl arg)))
          (%err-disp $xrdndarg char)))))

(set-dispatch-macro-character 
 #\# 
 #\=
 #'(lambda (stream char arg &aux lab form)
     (cond (*read-suppress* (values))
           ((null arg) (%err-disp $xrdndarg char))
           (t (setq lab (cons arg nil))
              (%rplacd lab (cons nil lab))
              (note-read-object lab)
              (setq form (read stream t nil t))
              (when (eq form lab)       ;#n= #n#.  No can do.
                (%err-disp $xnordlbl (%car lab)))
              (%rplacd (cdr lab) form)
              (let* ((reffed (cadr lab)))
                (if (not reffed)
                  form
                  (let ((scanned nil))
                    (labels ((circle-subst (tree)
                               (if (eq tree lab)
                                 (progn
                                   (unless (memq tree scanned)
                                     (setq scanned (%temp-cons tree scanned))
                                     (circle-subst (cddr tree)))
                                   (cddr tree))
                                 (progn
                                   (cond ((consp tree)
                                          (unless (memq tree scanned)
                                            (push tree scanned)
                                            (locally 
                                                (declare (type cons tree))
                                              (let* ((orig (car tree))
                                                     (new (circle-subst orig)))
                                                (unless (eq orig new)
                                                  (rplaca tree new))
                                                (setq orig (cdr tree)
                                                      new (circle-subst orig))
                                                (unless (eq orig new)
                                                  (rplacd tree new))))))
                                         ((let* ((typecode (typecode tree)))
                                            (declare (type (unsigned-byte 8) typecode))
                                            (or (= typecode target::subtag-simple-vector)
                                                (= typecode target::subtag-struct)
                                                (= typecode target::subtag-arrayH)))
                                          (unless (memq tree scanned)
                                            (push tree scanned)
                                            (let* ((n (uvsize tree)))
                                              (declare (fixnum n))
                                              (do* ((i (if (eql (typecode tree) target::subtag-struct) 1 0) (1+ i)))
                                                   ((= i n))
                                                (declare (fixnum i))
                                                (let* ((old (%svref tree i))
                                                       (new (circle-subst old)))
                                               (unless (eq old new)
                                                 (setf (%svref tree i) new))))))))
                                   tree))))
                      (declare (dynamic-extent #'circle-subst))
                      (circle-subst form)))))))))



