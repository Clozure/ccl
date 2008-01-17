;;; -*- Package: Hemlock -*-
;;;
;;; Copyright (c) 2007 Clozure Associates
;;; This file is part of Clozure Common Lisp.
;;;
;;; Dynamic symbol completion
;;; gz@clozure.com
;;;
;;; This uses wordchar attributes set up in completion.lisp, but otherwise is unrelated.

(in-package :hemlock)

;; Context maintained so repeated M-/'s can walk through all available abbreviations

(defstruct (dabbrev-context (:conc-name "DABBREV."))
  ;; The buffer this context belongs to
  (buffer nil)
  ;; The last expansion
  (expansion nil)
  ;; The thing that was expanded.  This is usually a prefix of expansion, but it might
  ;; be initials (i.e. abbrev = mvb, expansion = multiple-value-bind).
  (abbrev "" :type simple-string)
  ;; The package prefix if any, including the ending colon(s).
  (prefix nil)
  ;; The position of the end of the expansion
  (end-mark nil)
  ;; buffer-signature as of the time the expansion was inserted.
  (signature nil)
  ;; list of expansions already tried and rejected
  (seen ())
  ;; List of places to try next
  (state-path '(:before-point :after-point :other-buffers :this-package :other-packages))
  ;; Sequence of sources to go thru before changing state
  (sources '(:last-used))
  ;; a sequence of expansions to go thru before changing source
  (seq (make-array 10 :fill-pointer 0 :adjustable t)))

(defun symbol-completion-buffer-hook (buffer)
  (defhvar "DAbbrev Context"
    "Internal variable for cycling through symbol completions"
    :buffer buffer
    :value nil)
  (defhvar "DAbbrev Cache"
    "Internal variable for caching symbols in buffer"
    :buffer buffer
    ;; Cons of buffer sig and a vector of all symbols in buffer as of the time
    ;; of the buffer sig.
    :value (cons nil nil))
  )

(add-hook make-buffer-hook #'symbol-completion-buffer-hook)

;; Global table of all abbrevs expanded in this session, and the last value they expanded to.
(defvar *dabbrevs* (make-hash-table :test #'equalp))

(defun dabbrev-package (context)
  (or (dabbrev-package-for-prefix (dabbrev.prefix context))
      ;; TODO: look for in-package form preceeding point...
      (buffer-package (dabbrev.buffer context))))

(defun dabbrev-external-symbol-p (context)
  ;; True if explicitly looking for an external symbol.
  (let* ((prefix (dabbrev.prefix context))
	 (prefix-len (length prefix)))
    (or (eql prefix-len 1)
	(and (>= prefix-len 2)
	     (not (eql (aref prefix (- prefix-len 2)) #\:))))))

(defun dabbrev-package-for-prefix (prefix)
  (when prefix
    (let* ((prefix-len (length prefix)))
      (if (eql prefix-len 1)
	ccl::*keyword-package*
	(find-package (subseq prefix 0 (if (eql (aref prefix (- prefix-len 2)) #\:)
					 (- prefix-len 2)
					 (- prefix-len 1))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State machine support:

(defun dabbrev-next-expansion (context)
  (cond ((> (length (dabbrev.seq context)) 0)
	 (let* ((exp (vector-pop (dabbrev.seq context))))
	   (if (find exp (dabbrev.seen context) :test #'string=)
	     (dabbrev-next-expansion context)
	     exp)))
	((dabbrev.sources context)
	 (dabbrev-collect-expansions (pop (dabbrev.sources context)) context)
	 (dabbrev-next-expansion context))
	((dabbrev.state-path context)
	 (setf (dabbrev.sources context)
	       (dabbrev-sources-in (pop (dabbrev.state-path context)) context))
	 (dabbrev-next-expansion context))
	(t nil)))


;; dabbrev-sources-in: maps state -> sources

(defmethod dabbrev-sources-in ((state t) context)
  (declare (ignore context))
  (list state))

(defmethod dabbrev-sources-in ((state (eql :other-buffers)) context)
  (let* ((buffers (mapcar #'window-buffer (gui::ordered-hemlock-windows))))
    ;; Remove duplicates, always keeping the first occurance (frontmost window)
    (loop for blist on buffers do (setf (cdr blist) (delete (car blist) (cdr blist))))
    (delete (dabbrev.buffer context) buffers)))

(defmethod dabbrev-sources-in ((state (eql :other-packages)) context)
  (let* ((all (copy-list (list-all-packages)))
	 (this-package (dabbrev-package context))
	 (keyword-package ccl::*keyword-package*))
    (setq all (delete this-package all))
    (unless (eq this-package keyword-package)
      (setq all (nconc (delete keyword-package all) (list keyword-package))))
    all))

;; dabbrev-collect-expansion: maps source -> expansions
;; Note that in general these methods don't bother to check for dabbrev.seen
;; or duplicates, even though they could, because there is no reason to spend
;; time up front on checking expansions we might never get to.

(defun dabbrev-match-p (context exp)
  (let* ((abbrev (dabbrev.abbrev context))
	 (abbrev-len (length abbrev)))
    (or (and (< abbrev-len (length exp))
	     (string-equal abbrev exp :end1 abbrev-len :end2 abbrev-len))
	;; Check for initials.
	(loop
	  for char across abbrev
	  for pos = 0 then (and (setq pos (position-if-not #'alphanumericp exp :start pos))
				(position-if #'alphanumericp exp :start (1+ pos)))
	  always (and pos (char-equal char (aref exp pos)))))))

(defmethod dabbrev-collect-expansions ((source (eql :last-used)) context)
  (let* ((abbrev (dabbrev.abbrev context))
	 (prefix (dabbrev.prefix context))
	 (abbrev-len (length abbrev))
	 (prefix-len (length prefix))
	 (string (concatenate 'string abbrev prefix)))
    (loop
      for end from (+ abbrev-len prefix-len) downto prefix-len
      for key = string then (subseq string 0 end)
      as exp = (gethash key *dabbrevs*)
      when (and exp (dabbrev-match-p context exp))
      do (return (vector-push-extend exp (dabbrev.seq context))))))

(defmethod dabbrev-collect-expansions ((buffer buffer) context)
  ;; TODO: need to take prefix into account - give preferences to things
  ;; matching prefix.  For now, ignore the prefix-only case here since can't
  ;; do anything useful.
  (unless (equal (dabbrev.abbrev context) "")
    (let* ((vec (dabbrev-symbols-in-buffer buffer))
	   (seq (dabbrev.seq context)))
      (loop
	for exp across vec
	when (dabbrev-match-p context exp)
	do (vector-push-extend exp seq))
      seq)))

;; TODO: have a background process that does this. (Since the architecture doesn't allow locking
;; against buffer changes, might need to do ignore-errors and just bludgeon through, checking
;; for sig changes at end.  Or perhaps could use the modification hook, if that's reliable)
(defun dabbrev-symbols-in-buffer (buffer)
  (let* ((cache (variable-value 'dabbrev-cache :buffer buffer)))
    (unless (and cache (eql (car cache) (buffer-signature buffer)))
      (let* ((hi::*current-buffer* buffer)
	     (start-mark (buffer-start-mark buffer))
	     (symbols (make-array 100 :adjustable t :fill-pointer 0)))
	(with-mark ((word-start start-mark)
		    (word-end start-mark))
	  (loop
	    (unless (find-attribute word-end :completion-wordchar) (return))
	    (move-mark word-start word-end)
	    (unless (find-not-attribute word-end :completion-wordchar)
	      (buffer-end word-end))
	    (let* ((word (region-to-string (region word-start word-end))))
	      (unless (find word symbols :test #'equal)
		(vector-push-extend word symbols)))))
	(setf (variable-value 'dabbrev-cache :buffer buffer)
	      (setq cache (cons (buffer-signature buffer) (coerce symbols 'simple-vector))))))
    (cdr cache)))

(defun dabbrev-next-in-buffer (mark temp-mark temp-string)
  ;; Leaves temp-mark at start and point-mark at end of next symbol
  (when (find-attribute mark :completion-wordchar)
    (move-mark temp-mark mark)
    (unless (find-not-attribute mark :completion-wordchar)
      (buffer-end mark))
    (region-to-string (region temp-mark mark) temp-string)))

(defun dabbrev-prev-in-buffer (mark temp-mark temp-string)
  (when (reverse-find-attribute mark :completion-wordchar)
    (move-mark temp-mark mark)
    (unless (reverse-find-not-attribute mark :completion-wordchar)
      (buffer-start mark))
    (region-to-string (region mark temp-mark) temp-string)))

(defmethod dabbrev-collect-expansions ((source (eql :before-point)) context)
  (dabbrev-collect-expansions-1 source context))

(defmethod dabbrev-collect-expansions ((source (eql :after-point)) context)
  (dabbrev-collect-expansions-1 source context))

(defun dabbrev-collect-expansions-1 (direction context)
  (let* ((buffer (dabbrev.buffer context))
	 (point (buffer-point buffer))
	 (abbrev (dabbrev.abbrev context))
	 (abbrev-len (length abbrev))
	 (seq (dabbrev.seq context))
	 (temp-string (make-string 30)))
    ;; TODO: need to take prefix into account - give preferences to things
    ;; matching prefix.  For now, ignore the prefix-only case here since can't
    ;; do anything useful.
    (when (eql abbrev-len 0)
      (return-from dabbrev-collect-expansions-1))
    (with-mark ((mark point) (temp-mark point))
      (when (eq direction :before-point) (character-offset mark (- abbrev-len)))
      (loop
	(multiple-value-bind (word word-len)
			     (if (eq direction :before-point)
			       (dabbrev-prev-in-buffer mark temp-mark temp-string)
			       (dabbrev-next-in-buffer mark temp-mark temp-string))
	  (unless word (return))
	  (when (and (< abbrev-len word-len)
		     (string-equal word abbrev :end1 abbrev-len :end2 abbrev-len))
	    (let* ((word (subseq word 0 word-len)))
	      (unless (find word seq :test #'equal)
		(vector-push-extend word seq)))))))
    (nreverse seq)))

(defmethod dabbrev-collect-expansions ((source (eql :this-package)) context)
  (let* ((pkg (dabbrev-package context))
	 (seq (dabbrev.seq context)))
    (when pkg
      (when (dabbrev.prefix context)
	(if (or (dabbrev-external-symbol-p context)
		(eq pkg ccl::*keyword-package*))
	  (do-external-symbols (sym pkg)
	    (when (and (not (find sym seq :test #'eq))
		       (dabbrev-match-p context (symbol-name sym)))
	      (vector-push-extend sym seq)))
	  (ccl::do-present-symbols (sym pkg)
	    (when (and (not (find sym seq :test #'eq))
		       (dabbrev-match-p context (symbol-name sym)))
	      (vector-push-extend sym seq)))))
      (unless (eq pkg ccl::*keyword-package*)
	(do-symbols (sym pkg)
	  (when (and (not (find sym seq :test #'eq))
		     (dabbrev-match-p context (symbol-name sym)))
	    (vector-push-extend sym seq))))
      (stable-sort seq #'(lambda (s1 s2)
			   (and (or (boundp s1) (fboundp s1))
				(not (or (boundp s2) (fboundp s2))))))
      ;; Now convert to strings - and downcase for inserting in buffer.
      (dotimes (i (length seq))
	(setf (aref seq i) (string-downcase (symbol-name (aref seq i))))))
    seq))

(defmethod dabbrev-collect-expansions ((pkg package) context)
  ;; For random packages, only need to do present symbols, since imported ones will be
  ;; shown in their own package.
  (let* ((seq (dabbrev.seq context)))
    (ccl::do-present-symbols (sym pkg)
      (let* ((name (symbol-name sym)))
	(when (dabbrev-match-p context name)
	  (vector-push-extend (string-downcase name) seq))))
    seq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; the command


(defcommand "Expand Dynamic Abbreviation" (p)
  "Treats the symbol before point as an abbreviation and expands it.
It checks the following in order until a suitable expansion is found:
  - last accepted expansion for this abbreviation, if any
  - symbols in current buffer before point
  - symbols in current buffer after point
  - symbols in all other editor windows, front to back
  - symbols visible in the current package, fbound/bound symbols first
  - symbols in all other packages (in no particular order)

If called repeatedly from the same position, replaces the previous expansion
with the next possible one.

A symbol is a suitable expansion for an abbreviation if the abbreviation is
a proper prefix of the symbol, or the abbreviation consists of the initials
of the individual words within the symbol (e.g. mvb => multiple-value-bind).
"
  (declare (ignore p))
  (let* ((buffer (current-buffer))
	 (point (buffer-point buffer))
	 (context (dabbrev-command-init buffer))
	 (abbrev (dabbrev.abbrev context))
	 (abbrev-len (length abbrev))
	 (expansion (dabbrev-next-expansion context))
	 (expansion-len (length expansion)))
    (when (null expansion)
      (editor-error "No~:[ more~] expansions for ~s"
		    (null (dabbrev.expansion context))
		    abbrev))
    (push expansion (dabbrev.seen context))
    (setf (dabbrev.expansion context) expansion)
    (setf (gethash abbrev *dabbrevs*) expansion)
    (if (and (>= expansion-len abbrev-len)
	     (string= abbrev expansion :end2 abbrev-len))
      (insert-string point (subseq expansion abbrev-len))
      (progn
	(delete-characters point (- abbrev-len))
	(insert-string point expansion)))
    (move-mark (dabbrev.end-mark context) point)
    (setf (dabbrev.signature context) (buffer-signature buffer))))

#+gz ;; This tests the generation of completion candidates
;; (time(hemlock::test-completions (cadr hi::*buffer-list*) "dabbrev"))
(defun test-completions (buffer abbrev)
  (let* ((hi::*current-buffer* buffer)
	 (point (buffer-point buffer))
	 (context (make-dabbrev-context
		   :buffer buffer
		   :abbrev abbrev
		   ;; Can use a temp mark (i.e. the kind that doesn't automatically
		   ;; update) because we only use it while buffer is unmodified.
		   :end-mark (copy-mark point :temporary))))
    (loop as expansion = (dabbrev-next-expansion context) while expansion
      do (push expansion (dabbrev.seen context))
      do (setf (dabbrev.expansion context) expansion)
      do (setf (gethash abbrev *dabbrevs*) expansion))
    (dabbrev.seen context)))

;; Reinitialize context to either restart or cycle to next completion.
;; In the latter case, undoes the last completion in the buffer.
(defun dabbrev-command-init (buffer)
  (let* ((point (buffer-point buffer))
	 (context (variable-value 'dabbrev-context :buffer buffer)))
    (if (and context
	     ;; If buffer not modified since last time
	     (eql (dabbrev.signature context) (buffer-signature buffer))
	     ;; and cursor not moved elsewhere
	     (mark= (dabbrev.end-mark context) point))
      ;; This means rejected previous attempt, get rid of it.
      (let* ((abbrev (dabbrev.abbrev context))
	     (abbrev-len (length abbrev))
	     (expansion (dabbrev.expansion context))
	     (expansion-len (length expansion)))
	;; Sanity check, because I don't totally trust buffer-signature ...
	(with-mark ((mark point))
	  (assert (and (character-offset mark (- (length expansion)))
		       (equal (region-to-string (region mark point)) expansion))
		  () "Bug! Buffer changed unexpectedly"))
	(if (and (>= expansion-len abbrev-len)
		 (string= abbrev expansion :end2 abbrev-len))
	  (delete-characters point (- abbrev-len expansion-len))
	  (progn
	    (delete-characters point (- expansion-len))
	    (insert-string point abbrev))))
      ;; Else starting a new attempt, create a new context
      (let* ((mark (copy-mark point :temporary)))
	(multiple-value-bind (abbrev prefix) (dabbrev-get-abbrev mark point)
	  (when (and (equal abbrev "") (equal prefix ""))
	    (editor-error "Nothing to expand"))
	  (setq context (make-dabbrev-context
			 :buffer buffer
			 :abbrev abbrev
			 :prefix prefix
			 ;; Can use a temp mark (i.e. the kind that doesn't automatically
			 ;; update) because we only use it while buffer is unmodified.
			 :end-mark mark)))
	(setf (variable-value 'dabbrev-context :buffer buffer) context)))
    (move-mark (dabbrev.end-mark context) point)
    context))

(defun dabbrev-get-abbrev (mark point)
  (declare (values abbrev package-prefix))
  (move-mark mark point)
  (unless (reverse-find-not-attribute mark :completion-wordchar)
    (buffer-start mark))
  (values (region-to-string (region mark point))
	  (when (eql (previous-character mark) #\:)
	    (with-mark ((temp mark))
	      (character-offset temp -1)
	      (when (eql (previous-character temp) #\:)
		(character-offset temp -1))
	      (unless (reverse-find-not-attribute temp :completion-wordchar)
		(buffer-start temp))
	      (region-to-string (region temp mark))))))


