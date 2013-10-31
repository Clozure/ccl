;;; -*- Mode: Lisp; Package: hemlock -*-
;;;
;;;   Copyright (C) 2007 Clozure Associates

(in-package :hemlock)

(defmode "I-Search" :precedence :highest
  ;; Make anything that's not otherwise overridden exit i-search.
  :default-command "I-Search Exit and Redo")

(add-hook abort-hook 'end-isearch-mode)

(defhvar "Self Insert Command Name"
  "The name of the command to handle quoted input (i.e. after c-q) in I-Search"
  :value "I-Search Self Insert"
  :mode "I-Search")

(defcommand "Incremental Search" (p)
  "Searches for input string as characters are provided.

  These are the default I-Search command characters:
     ^Q quotes the next character typed.
     ^W extends the search string to include the the word after the point. 
     Delete cancels the last key typed.
     ^G during a successful search aborts and returns point to where it started.
       During a failing search, ^G backs up to last non-failing point.
     ^S repeats forward, and ^R repeats backward.
     ^R or ^S with empty string either changes the direction or yanks the previous search string.
     Escape exits the search unless the string is empty.
     Escape with an empty search string calls the non-incremental search command. 

  Other control characters cause exit and execution of the appropriate 
  command.
"
  "Set up Incremental Search mode"
  (declare (ignore p))
  (start-isearch-mode :forward))

(defcommand "Reverse Incremental Search" (p)
  "Searches for input string as characters are provided.

  These are the default I-Search command characters:
     ^Q quotes the next character typed.
     ^W extends the search string to include the the word after the point. 
     Delete cancels the last key typed.
     ^G during a successful search aborts and returns point to where it started.
       During a failing search, ^G backs up to last non-failing point.
     ^S repeats forward, and ^R repeats backward.
     ^R or ^S with empty string either changes the direction or yanks the previous search string.
     Escape exits the search unless the string is empty.
     Escape with an empty search string calls the non-incremental search command. 

  Other control characters cause exit and execution of the appropriate 
  command.
"
  "Set up Incremental Search mode"
  (declare (ignore p))
  (start-isearch-mode :backward))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(defstruct (isearch-state (:conc-name "ISS-"))
  direction
  local-pattern
  failure
  wrapped-p
  history
  start-region)

(defvar *global-search-pattern* nil "Used when *isearch-is-global*") ; can't use *last-search-pattern* because that's
; used elsewhere

(defun iss-pattern (state)
  (if *isearch-is-global*
    (or *global-search-pattern*
        (iss-local-pattern state))
    (iss-local-pattern state)))

(defun iss-string (state)
  (ignore-errors ; because iss-pattern might be nil
   (hi::search-pattern-pattern (iss-pattern state))))

(defun current-region-info ()
  (list (copy-mark (current-point) :temporary)
	(copy-mark (current-mark) :temporary)
	(region-active-p)))

(defun set-current-region-info (info)
  (destructuring-bind (point mark active-p) info
    (move-mark (current-point) point)
    (move-mark (current-mark) mark)
    (if active-p
      (progn
	(activate-region)
	(note-current-selection-set-by-search))
      (deactivate-region))))

(defun %i-search-save-state (iss)
  (push (list* (iss-string iss)
	       (iss-direction iss)
	       (iss-failure iss)
	       (iss-wrapped-p iss)
	       (current-region-info))
	(iss-history iss)))

(defun %i-search-pop-state (iss)
  (destructuring-bind (string direction failure wrapped-p . region-info)
		      (pop (iss-history iss))
    (setf (iss-failure iss) failure)
    (setf (iss-wrapped-p iss) wrapped-p)
    (%i-search-set-pattern iss :string string :direction direction)
    (set-current-region-info region-info)))

(defun %i-search-message (iss)
  (when t ;(interactive)
    (message "~:[~;Failing ~]~:[~;Wrapped ~]~:[Reverse I-Search~;I-Search~]: ~A"
	     (iss-failure iss)
	     (iss-wrapped-p iss)
	     (eq (iss-direction iss) :forward)
	     (or (iss-string iss) ""))))


;; Minor errors that don't cause isearch mode to be exited, except while
;; executing keyboard macros.
(defun %i-search-perhaps-error (message)
  message
  (if t ;(interactive)
      (beep)
      (abort-current-command message)))

;;;;
;;

(defun current-isearch-state ()
  (or (value i-search-state)
      (error "I-Search command invoked outside I-Search")))

(defun start-isearch-mode (direction)
  (let* ((buffer (current-buffer))
         (iss (make-isearch-state :direction direction
				  :start-region (current-region-info))))
    (when (iss-pattern iss)
      (setf (hi::search-pattern-pattern (iss-pattern iss)) nil))
    (setf (buffer-minor-mode buffer "I-Search") t)
    (unless (hemlock-bound-p 'i-search-state :buffer buffer)
      (defhvar "I-Search State"
        "Internal variable containing current state of I-Search"
        :buffer buffer))
    (unless (region-active-p) ; We need the selection (if there is one) to stay put!
      (push-new-buffer-mark (current-point)))
    (setf (value i-search-state) iss)
    (%i-search-message iss)))

(defun end-isearch-mode ()
  (setf (buffer-minor-mode (current-buffer) "I-Search") nil))

(defcommand "I-Search Yank Selection" (p)
   "Pull string from current selection into search string."
  (declare (ignore p))
  (let* ((iss (current-isearch-state)))
    (i-search-extend iss (symbol-at-point (current-buffer)))))

(defun i-search-backup (iss)
  (if (iss-history iss)
    (progn
      (%i-search-pop-state iss)
      (%i-search-message iss))
    (%i-search-perhaps-error "I-Search Backup failed")))

(defun i-search-revert (iss)
  (loop while (iss-failure iss) do (%i-search-pop-state iss))
  (%i-search-message iss))

(defun i-search-repeat (iss)
  (cond ((null (iss-string iss))
	 ;; No search string, so "repeat" really means fetch last successful search string
	 (if (zerop (length *last-search-string*))
	   (%i-search-perhaps-error "No previous search string")
	   (progn
	     (%i-search-save-state iss)
	     (%i-search-set-pattern iss :string *last-search-string*)
	     (%i-search-do-search iss (current-mark)))))
	((iss-failure iss)
	 (%i-search-save-state iss)
	 ;; If failed last time, "repeat" really means try again from the top.
	 (setf (iss-wrapped-p iss) t) ;; start saying "Wrapped i-search" to remind 'em.
	 (%i-search-do-search iss (if (eq (iss-direction iss) :forward)
				    (buffer-start-mark (current-buffer))
				    (buffer-end-mark (current-buffer)))))
	(t
	 (%i-search-save-state iss)
	 ;; Have a non-empty string and a successful search, just find the next one!
	 (%i-search-do-search iss (current-point))))
  (%i-search-message iss))

(defun i-search-reverse (iss)
  (%i-search-save-state iss)
  (%i-search-set-pattern iss :direction (ecase (iss-direction iss)
					  (:forward :backward)
					  (:backward :forward)))
  (let* ((mark (current-mark))
	 (point (current-point)))
    (with-mark ((temp point))
      (move-mark point mark)
      (move-mark mark temp))
    (when (iss-failure iss)
      ;; if we were failing before, search immediately, otherwise wait til asked
      (%i-search-do-search iss mark)))
  (%i-search-message iss))

(defun i-search-extend (iss extension)
  (%i-search-save-state iss)
  (let* ((new-string (concatenate 'simple-string (iss-string iss) extension)))
    (%i-search-set-pattern iss :string new-string))
  (unless (iss-failure iss)  ;; Can't succeed now if failed before, so don't try
    (with-mark ((temp (current-mark)))
      (when (eq (iss-direction iss) :backward)
	(or (character-offset temp (length extension))
	    (buffer-end temp)))
      (%i-search-do-search iss temp)))
  (%i-search-message iss))

(defun i-search-exit (iss)
  (let* ((string (iss-string iss)))
    (when (and string (not (iss-failure iss)))
      (setf *last-search-string* string)))
  (end-isearch-mode)
  (message ""))

(defun %i-search-set-pattern (iss &key (string nil s-p) (direction nil d-p))
  (let ((thisstring (if s-p (or string "") (iss-string iss))))
    (when *isearch-is-global*
      (setf *last-search-string* thisstring))
    (when d-p
      (setf (iss-direction iss) direction))
    (setf *global-search-pattern*
          (setf (iss-local-pattern iss) (new-search-pattern (if (value string-search-ignore-case)
                                                              :string-insensitive
                                                              :string-sensitive)
                                                            (iss-direction iss)
                                                            thisstring
                                                            (iss-pattern iss))))))

;; Do a search for the current pattern starting at START going to
;; end/beginning as per ISS-DIRECTION.  Sets ISS-FAILURE depending on
;; whether found or not.  If successful, moves region to surround the
;; found string (with point at the end for :forward search and at the
;; beginning for :backward) and activates the region.  If failed,
;; leaves region unchanged.  Never modifies START.
(defun %i-search-do-search (iss start)
  (let* ((temp (copy-mark start :temporary))
	 (found-offset (find-pattern temp (iss-pattern iss))))
    (setf (iss-failure iss) (not found-offset))
    (if (iss-failure iss)
      (%i-search-perhaps-error "I-Search failed")
      (let* ((point (current-point))
	     (mark (current-mark)))
	(move-mark point temp)
	(if (eq (iss-direction iss) :forward)
	  (character-offset point found-offset)
	  (character-offset temp found-offset))
	(move-mark mark temp)
	(activate-region)
	(note-current-selection-set-by-search)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(defcommand "I-Search Repeat Forward" (p)
  "Repeat forward incremental search, or reverse direction if currently searching backward"
  (declare (ignore p))
  (let* ((iss (current-isearch-state)))
    (if (eq (iss-direction iss) :forward)
      (i-search-repeat iss)
      (i-search-reverse iss))))

(defcommand "I-Search Repeat Backward" (p)
  "Repeat backward incremental search, or reverse direction if currently searching forward"
  (declare (ignore p))
  (let* ((iss (current-isearch-state)))
    (if (eq (iss-direction iss) :backward)
      (i-search-repeat iss)
      (i-search-reverse iss))))

(defcommand "I-Search Backup" (p)
  "Undo last incremental search command"
  (declare (ignore p))
  (let* ((iss (current-isearch-state)))
    (i-search-backup iss)))

(defcommand "I-Search Yank Word" (p)
  "Extend the search string to include the the word after the point."
  (declare (ignore p))
  (let* ((iss (current-isearch-state))
	(point (current-point)))
    (with-mark ((end point))
      (if (word-offset end 1)
	(i-search-extend iss (region-to-string (region point end)))
	(%i-search-perhaps-error "No more words")))))

(defcommand "I-Search Self Insert" (p)
  "Add character typed to search string"
  (declare (ignore p))
  (let* ((iss (current-isearch-state))
	(char (last-char-typed)))
    (unless char (editor-error "Can't insert that character."))
    (i-search-extend iss (string char))))

(defcommand "I-Search Abort" (p)
  "Abort incremental search mode if search is successful.  Otherwise, revert to last
successful search and continue searching."
  (declare (ignore p))
  (let* ((iss (current-isearch-state)))
    (if (iss-failure iss)
      (i-search-revert iss)
      ;; Else move back to starting point and stop searching
      (progn
	(set-current-region-info (iss-start-region iss))
	(abort-current-command "Search aborted")))))

;; The transparent-p flag takes care of executing the key normally when we're done,
;; as long as we don't take a non-local exit.
(defcommand ("I-Search Exit and Redo" :transparent-p t) (p)
  "Exit Incremental Search and then execute the key normally"
  (declare (ignore p))
  (let* ((iss (current-isearch-state)))
    (i-search-exit iss)))

(defcommand "I-Search Exit or Search" (p)
  "Exit incremental search.  If the search string is empty, switch to non-incremental search,
otherwise just quit"
  (declare (ignore p))
  (let* ((iss (current-isearch-state))
	 (string (iss-string iss))
	 (direction (iss-direction iss)))
    (i-search-exit iss)
    (when (null string)
      (if (eq direction :forward)
	(forward-search-command nil)
	(reverse-search-command nil)))))



