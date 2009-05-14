;;; -*- Log: hemlock.log; Package: hemlock-internals -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
#+CMU (ext:file-comment
  "$Header$")
;;;
;;; **********************************************************************
;;;
;;; Reluctantly written by Christopher Hoover
;;; Supporting cast includes Rob and Bill.
;;;
;;; This file defines a data structure, analogous to a Common Lisp
;;; hashtable, which translates strings to values and facilitates
;;; recognition and completion of these strings.
;;;

(in-package :hemlock-internals)


;;;; Implementation Details

;;; String tables are a data structure somewhat analogous to Common Lisp
;;; hashtables.  String tables are case-insensitive.  Functions are
;;; provided to quickly look up strings, insert strings, disambiguate or
;;; complete strings, and to provide a variety of ``help'' when
;;; disambiguating or completing strings.
;;; 
;;; String tables are represented as a series of word tables which form
;;; a tree.  Four structures are used to implement this data structure.
;;; The first is a STRING-TABLE.  This structure has severals slots one
;;; of which, FIRST-WORD-TABLE, points to the first word table.  This
;;; first word table is also the root of tree.  The STRING-TABLE
;;; structure also contains slots to keep track of the number of nodes,
;;; the string table separator (which is used to distinguish word or
;;; field boundaries), and a pointer to an array of VALUE-NODE's.
;;; 
;;; A WORD-TABLE is simply an array of pointers to WORD-ENTRY's.  This
;;; array is kept sorted by the FOLDED slot in each WORD-ENTRY so that a
;;; binary search can be used.  Each WORD-ENTRY contains a case-folded
;;; string and a pointer to the next WORD-TABLE in the tree.  By
;;; traversing the tree made up by these structures, searching and
;;; completion can easily be done.
;;; 
;;; Another structure, a VALUE-NODE, is used to hold each entry in the
;;; string table and contains both a copy of the original string and a
;;; case-folded version of the original string along with the value.
;;; All of these value nodes are stored in a array (pointed at by the
;;; VALUE-NODES slot of the STRING-TABLE structure) and sorted by the
;;; FOLDED slot in the VALUE-NODE structure so that a binary search may
;;; be used to quickly find existing strings.
;;;


;;;; Structure Definitions

(defparameter initial-string-table-size 20
  "Initial size of string table array for value nodes.")
(defparameter initial-word-table-size 2
  "Inital size of each word table array for each tree node.")

(defstruct (string-table
	    (:constructor %make-string-table (separator))
	    (:print-function print-string-table))
  "This structure is used to implement the Hemlock string-table type."
  ;; Character used to 
  (separator #\Space :type base-char) ; character used for word separator
  (num-nodes 0 :type fixnum)		   ; number of nodes in string table
  (value-nodes (make-array initial-string-table-size)) ; value node array
  (first-word-table (make-word-table)))	   ; pointer to first WORD-TABLE

(defun print-string-table (table stream depth)
  (declare (ignore table depth))
  (format stream "#<String Table>"))

(defun make-string-table (&key (separator #\Space) initial-contents)
  "Creates and returns a Hemlock string-table.  If Intitial-Contents is
  supplied in the form of an A-list of string-value pairs, these pairs
  will be used to initialize the table.  If Separator, which must be a
  base-char, is specified then it will be used to distinguish word
  boundaries."
  (let ((table (%make-string-table separator)))
    (dolist (x initial-contents)
      (setf (getstring (car x) table) (cdr x)))
    table))


(defstruct (word-table
	    (:print-function print-word-table))
  "This structure is a word-table which is part of a Hemlock string-table."
  (num-words 0 :type fixnum)		   ; Number of words
  (words (make-array initial-word-table-size))) ; Array of WORD-ENTRY's

(defun print-word-table (table stream depth)
  (declare (ignore table depth))
  (format stream "#<Word Table>"))


(defstruct (word-entry
	    (:constructor make-word-entry (folded))
	    (:print-function print-word-entry))
  "This structure is an entry in a word table which is part of a Hemlock
  string-table."
  next-table				   ; Pointer to next WORD-TABLE
  folded				   ; Downcased word
  value-node)				   ; Pointer to value node or NIL

(defun print-word-entry (entry stream depth)
  (declare (ignore depth))
  (format stream "#<Word Table Entry: \"~A\">" (word-entry-folded entry)))


(defstruct (value-node
	    (:constructor make-value-node (proper folded value))
	    (:print-function print-value-node))
  "This structure is a node containing a value in a Hemlock string-table."
  folded				   ; Downcased copy of string
  proper				   ; Proper copy of string entry
  value)				   ; Value of entry

(defun print-value-node (node stream depth)
  (declare (ignore depth))
  (format stream "<Value Node \"~A\">" (value-node-proper node)))


;;;; Bi-SvPosition, String-Compare, String-Compare*

;;; Much like the CL function POSITION; however, this is a fast binary
;;; search for simple vectors.  Vector must be a simple vector and Test
;;; must be a function which returns either :equal, :less, or :greater.
;;; (The vector must be sorted from lowest index to highest index by the
;;; Test function.)  Two values are returned: the first is the position
;;; Item was found or if it was not found, where it should be inserted;
;;; the second is a boolean flag indicating whether or not Item was
;;; found.
;;; 
(defun bi-svposition (item vector test &key (start 0) end key)
  (declare (simple-vector vector) (fixnum start))
  (let ((low start)
	(high (if end end (length vector)))
	(mid 0))
    (declare (fixnum low high mid))
    (loop
      (when (< high low) (return (values low nil)))
      (setf mid (+ (the fixnum (ash (the fixnum (- high low)) -1)) low))
      (let* ((array-item (svref vector mid))
	     (test-item (if key (funcall key array-item) array-item)))
	(ecase (funcall test item test-item)
	  (:equal (return (values mid t)))
	  (:less (setf high (1- mid)))
	  (:greater (setf low (1+ mid))))))))

;;; A simple-string comparison appropriate for use with BI-SVPOSITION.
;;; 
(defun string-compare (s1 s2 &key (start1 0) end1 (start2 0) end2)
  (declare (simple-string s1 s2) (fixnum start1 start2))
  (let* ((end1 (or end1 (length s1)))
	 (end2 (or end2 (length s2)))
	 (pos1 (string/= s1 s2
			 :start1 start1 :end1 end1 :start2 start2 :end2 end2)))
    (if (null pos1)
	:equal
	(let ((pos2 (+ (the fixnum pos1) (- start2 start1))))
	  (declare (fixnum pos2))
	  (cond ((= pos1 (the fixnum end1)) :less)
		((= pos2 (the fixnum end2)) :greater)
		((char< (schar s1 (the fixnum pos1)) (schar s2 pos2)) :less)
		(t :greater))))))

;;; Macro to return a closure to call STRING-COMPARE with the given
;;; keys.
;;; 
(defmacro string-compare* (&rest keys)
  `#'(lambda (x y) (string-compare x y ,@keys)))


;;;; Insert-Element, Nconcf

;;; Insert-Element is a macro which encapsulates the hairiness of
;;; inserting an element into a simple vector.  Vector should be a
;;; simple vector with Num elements (which may be less than or equal to
;;; the length of the vector) and Element is the element to insert at
;;; Pos.  The optional argument Grow-Factor may be specified to control
;;; the new size of the array if a new vector is necessary.  The result
;;; of INSERT-ELEMENT must be used as a new vector may be created.
;;; (Note that the arguments should probably be lexicals since some of
;;; them are evaluated more than once.)
;;;
;;; We clear out the old vector so that it won't hold on to garbage if it
;;; happens to be in static space.
;;; 
(defmacro insert-element (vector pos element num &optional (grow-factor 2))
  `(let ((new-num (1+ ,num))
	 (max (length ,vector)))
     (declare (fixnum new-num max))
     (cond ((= ,num max)
	    ;; grow the vector
	    (let ((new (make-array (truncate (* max ,grow-factor)))))
	      (declare (simple-vector new))
	      ;; Blt the new buggers into place leaving a space for
	      ;; the new element
	      (replace new ,vector :end1 ,pos :end2 ,pos)
	      (replace new ,vector :start1 (1+ ,pos) :end1 new-num
		       :start2 ,pos :end2 ,num)
	      (fill ,vector nil)
	      (setf (svref new ,pos) ,element)
	      new))
	   (t
	    ;; move the buggers down a slot
	    (replace ,vector ,vector :start1 (1+ ,pos) :start2 ,pos)
	    (setf (svref ,vector ,pos) ,element)
	    ,vector))))

(define-modify-macro nconcf (&rest args) nconc)


;;;; With-Folded-String, Do-Words

;;; With-Folded-String is a macro which deals with strings from the
;;; user.  First, if the original string is not a simple string then it
;;; is coerced to one.  Next, the string is trimmed using the separator
;;; character and all separators between words are collapsed to a single
;;; separator.  The word boundaries are pushed on to a list so that the
;;; Do-Words macro can be called anywhere within the dynamic extent of a
;;; With-Folded-String to ``do'' over the words.

(defvar *separator-positions* nil)

(defmacro do-words ((start-var end-var) &body body)
  (let ((sep-pos (gensym)))
    `(dolist (,sep-pos *separator-positions*)
       (let ((,start-var (car ,sep-pos))
	     (,end-var (cdr ,sep-pos)))
         (locally
             ,@body)))))

(defmacro with-folded-string ((str-var len-var orig-str separator)
			      &body body)
  `(let* ((,str-var (make-string (length ,orig-str)))
          (*separator-positions* nil))
     (declare (simple-string ,str-var)
              (dynamic-extent ,str-var))
     ;; make the string simple if it isn't already
     (unless (simple-string-p ,orig-str)
       (setq ,orig-str (coerce ,orig-str 'simple-string)))
     ;; munge it into stack-allocated ,str-var and do the body
     (let ((,len-var (with-folded-munge-string ,str-var ,orig-str ,separator)))
       ,@body)))

(defun with-folded-munge-string (buf str separator)
  (declare (simple-string str) (base-char separator))
  (let ((str-len (length str))
	(sep-pos nil)
	(buf-pos 0))
    ;; Bash the spaces out of the string remembering where the words are.
    (let ((start-pos (position separator str :test-not #'char=)))
      (when start-pos
	(loop
	  (let* ((end-pos (position separator str
				    :start start-pos :test #'char=))
		 (next-start-pos (and end-pos (position separator str
							:start end-pos
							:test-not #'char=)))
		 (word-len (- (or end-pos str-len) start-pos))
		 (new-buf-pos (+ buf-pos word-len)))
	    (replace buf str
		     :start1 buf-pos :start2 start-pos :end2 end-pos)
	    (push (cons buf-pos new-buf-pos) sep-pos)
	    (setf buf-pos new-buf-pos)
	    (when (or (null end-pos) (null next-start-pos))
	      (return))
	    (setf start-pos next-start-pos)
	    (setf (schar buf buf-pos) separator)
	    (incf buf-pos)))))
    (nstring-downcase buf :end buf-pos)
    (setf *separator-positions* (nreverse sep-pos))
    buf-pos))


;;;; Getstring, Setf Method for Getstring

(defun getstring (string string-table)
  "Looks up String in String-Table.  Returns two values: the first is
  the value of String or NIL if it does not exist; the second is a
  boolean flag indicating whether or not String was found in
  String-Table."
  (with-folded-string (folded len string (string-table-separator string-table))
    (let ((nodes (string-table-value-nodes string-table))
	  (num-nodes (string-table-num-nodes string-table)))
      (declare (simple-vector nodes) (fixnum num-nodes))
      (multiple-value-bind
	  (pos found-p)
	  (bi-svposition folded nodes (string-compare* :end1 len)
			 :end (1- num-nodes) :key #'value-node-folded)
	(if found-p
	    (values (value-node-value (svref nodes pos)) t)
	    (values nil nil))))))

(defun %set-string-table (string table value)
  "Sets the value of String in Table to Value.  If necessary, creates
  a new entry in the string table."
  (with-folded-string (folded len string (string-table-separator table))
    (when (zerop len)
      (error "An empty string cannot be inserted into a string-table."))
    (let ((nodes (string-table-value-nodes table))
	  (num-nodes (string-table-num-nodes table)))
      (declare (simple-string folded) (simple-vector nodes) (fixnum num-nodes))
      (multiple-value-bind
	  (pos found-p)
	  (bi-svposition folded nodes (string-compare* :end1 len)
			 :end (1- num-nodes) :key #'value-node-folded)
	(cond (found-p
 	       (setf (value-node-value (svref nodes pos)) value))
	      (t
	       ;; Note that a separator collapsed copy of string is NOT
	       ;; used here ...
	       ;; 
	       (let ((node (make-value-node string (subseq folded 0 len) value))
		     (word-table (string-table-first-word-table table)))
		 ;; put in the value nodes array
		 (setf (string-table-value-nodes table)
		       (insert-element nodes pos node num-nodes))
		 (incf (string-table-num-nodes table))
		 ;; insert it into the word tree
		 (%set-insert-words folded word-table node))))))
    value))

(defun %set-insert-words (folded first-word-table value-node)
  (declare (simple-string folded))
  (let ((word-table first-word-table)
	(entry nil))
    (do-words (word-start word-end)
      (let ((word-array (word-table-words word-table))
	    (num-words (word-table-num-words word-table)))
	(declare (simple-vector word-array) (fixnum num-words))
	;; find the entry or create a new one and insert it
	(multiple-value-bind
	    (pos found-p)
	    (bi-svposition folded word-array
			   (string-compare* :start1 word-start :end1 word-end)
			   :end (1- num-words) :key #'word-entry-folded)
	  (declare (fixnum pos))
	  (cond (found-p
		 (setf entry (svref word-array pos)))
		(t
		 (setf entry (make-word-entry
			      (subseq folded word-start word-end)))
		 (setf (word-table-words word-table)
		       (insert-element word-array pos entry num-words))
		 (incf (word-table-num-words word-table)))))
	(let ((next-table (word-entry-next-table entry)))
	  (unless next-table
	    (setf next-table (make-word-table))
	    (setf (word-entry-next-table entry) next-table))
	  (setf word-table next-table))))
    (setf (word-entry-value-node entry) value-node)))


;;;; Find-Bound-Entries

(defun find-bound-entries (word-entries)
  (let ((res nil))
    (dolist (entry word-entries)
      (nconcf res (sub-find-bound-entries entry)))
    res))

(defun sub-find-bound-entries (entry)
  (let ((bound-entries nil))
    (when (word-entry-value-node entry) (push entry bound-entries))
    (let ((next-table (word-entry-next-table entry)))
      (when next-table
	(let ((word-array (word-table-words next-table))
	      (num-words (word-table-num-words next-table)))
	  (declare (simple-vector word-array) (fixnum num-words))
	  (dotimes (i num-words)
	    (declare (fixnum i))
	    (nconcf bound-entries
		    (sub-find-bound-entries (svref word-array i)))))))
    bound-entries))


;;;; Find-Ambiguous

(defun find-ambiguous (string string-table)
  "Returns a list, in alphabetical order, of all the strings in String-Table
  which String matches."
  (with-folded-string (folded len string (string-table-separator string-table))
    (find-ambiguous* folded len string-table)))

(defun find-ambiguous* (folded len table)
  (let ((word-table (string-table-first-word-table table))
	(word-entries nil))
    (cond ((zerop len)
	   (setf word-entries (find-ambiguous-entries "" 0 0 word-table)))
	  (t
	   (let ((word-tables (list word-table)))
	     (do-words (start end)
	       (setf word-entries nil)
	       (dolist (wt word-tables)
		 (nconcf word-entries
			 (find-ambiguous-entries folded start end wt)))
	       (unless word-entries (return))
	       (let ((next-word-tables nil))
		 (dolist (entry word-entries)
		   (let ((next-word-table (word-entry-next-table entry)))
		     (when next-word-table
		       (push next-word-table next-word-tables))))
		 (unless next-word-tables (return))
		 (setf word-tables (nreverse next-word-tables)))))))
    (let ((bound-entries (find-bound-entries word-entries))
	  (res nil))
      (dolist (be bound-entries)
	(push (value-node-proper (word-entry-value-node be)) res))
      (nreverse res))))

(defun find-ambiguous-entries (folded start end word-table)
  (let ((word-array (word-table-words word-table))
	(num-words (word-table-num-words word-table))
	(res nil))
    (declare (simple-vector word-array) (fixnum num-words))
    (unless (zerop num-words)
      (multiple-value-bind
	  (pos found-p)
	  (bi-svposition folded word-array
			 (string-compare* :start1 start :end1 end)
			 :end (1- num-words) :key #'word-entry-folded)
	(declare (ignore found-p))
	;;
	;; Find last ambiguous string, checking for the end of the table.
	(do ((i pos (1+ i)))
	    ((= i num-words))
	  (declare (fixnum i))
	  (let* ((entry (svref word-array i))
		 (str (word-entry-folded entry))
		 (str-len (length str))
		 (index (string/= folded str :start1 start :end1 end
				  :end2 str-len)))
	    (declare (simple-string str) (fixnum str-len))
	    (when (and index (/= index end)) (return nil))
	    (push entry res)))
	(setf res (nreverse res))
	;;
	;; Scan back to the first string, checking for the beginning.
	(do ((i (1- pos) (1- i)))
	    ((minusp i))
	  (declare (fixnum i))
	  (let* ((entry (svref word-array i))
		 (str (word-entry-folded entry))
		 (str-len (length str))
		 (index (string/= folded str :start1 start :end1 end
				  :end2 str-len)))
	    (declare (simple-string str) (fixnum str-len))
	    (when (and index (/= index end)) (return nil))
	    (push entry res)))))
    res))


;;;; Find-Containing

(defun find-containing (string string-table)
  "Return a list in alphabetical order of all the strings in Table which 
  contain String as a substring."
  (with-folded-string (folded len string (string-table-separator string-table))
    (declare (ignore len))
    (let ((word-table (string-table-first-word-table string-table))
	  (words nil))
      ;; cons up a list of the words
      (do-words (start end)
	(push (subseq folded start end) words))
      (setf words (nreverse words))
      (let ((entries (sub-find-containing words word-table))
	    (res nil))
	(dolist (e entries)
	  (push (value-node-proper (word-entry-value-node e)) res))
	(nreverse res)))))

(defun sub-find-containing (words word-table)
  (let ((res nil)
	(word-array (word-table-words word-table))
	(num-words (word-table-num-words word-table)))
    (declare (simple-vector word-array) (fixnum num-words))
    (dotimes (i num-words)
      (declare (fixnum i))
      (let* ((entry (svref word-array i))
	     (word (word-entry-folded entry))
	     (found (find word words
			  :test #'(lambda (y x)
				    (let ((lx (length x))
					  (ly (length y)))
				      (and (<= lx ly)
					   (string= x y :end2 lx))))))
	     (rest-words (if found
			     (remove found words :test #'eq :count 1)
			     words)))
	(declare (simple-string word))
	(cond (rest-words
	       (let ((next-table (word-entry-next-table entry)))
		 (when next-table
		   (nconcf res (sub-find-containing rest-words next-table)))))
	      (t
	       (nconcf res (sub-find-bound-entries entry))))))
    res))


;;;; Complete-String

(defvar *complete-string-buffer-size* 128)
(defvar *complete-string-buffer* (make-string *complete-string-buffer-size*))
(declaim (simple-string *complete-string-buffer*))

(defun complete-string (string tables)
  "Attempts to complete the string String against the string tables in the
   list Tables.  Tables must all use the same separator character.  See the
   manual for details on return values."
  (let ((separator (string-table-separator (car tables))))
    #|(when (member separator (cdr tables)
		  :key #'string-table-separator :test-not #'char=)
      (error "All tables must have the same separator."))|#
    (with-folded-string (folded len string separator)
      (let ((strings nil))
	(dolist (table tables)
	  (nconcf strings (find-ambiguous* folded len table)))
	;; pick off easy case
	(when (null strings)
	  (return-from complete-string (values nil :none nil nil nil)))
	;; grow complete-string buffer if necessary
	(let ((size-needed (1+ len)))
	  (when (> size-needed *complete-string-buffer-size*)
	    (let* ((new-size (* size-needed 2))
		   (new-buffer (make-string new-size)))
	      (setf *complete-string-buffer* new-buffer)
	      (setf *complete-string-buffer-size* new-size))))
	(multiple-value-bind
	    (str ambig-pos unique-p)
	    (find-longest-completion strings separator)
	  (multiple-value-bind (value found-p) (find-values str tables)
	    (let ((field-pos (compute-field-pos string str separator)))
	      (cond ((not found-p)
		     (values str :ambiguous nil field-pos ambig-pos))
		    (unique-p
		     (values str :unique value field-pos nil))
		    (t
		     (values str :complete value field-pos ambig-pos))))))))))

(defun find-values (string tables)
  (dolist (table tables)
    (multiple-value-bind (value found-p) (getstring string table)
      (when found-p
	(return-from find-values (values value t)))))
  (values nil nil))

(defun compute-field-pos (given best separator)
  (declare (simple-string given best) (base-char separator))
  (let ((give-pos 0)
	(best-pos 0))
    (loop
      (setf give-pos (position separator given :start give-pos :test #'char=))
      (setf best-pos (position separator best :start best-pos :test #'char=))
      (unless (and give-pos best-pos) (return best-pos))
      (incf (the fixnum give-pos))
      (incf (the fixnum best-pos)))))


;;;; Find-Longest-Completion

(defun find-longest-completion (strings separator)
  (declare (base-char separator))
  (let ((first (car strings))
	(rest-strings (cdr strings))
	(punt-p nil)
	(buf-pos 0)
	(first-start 0)
	(first-end -1)
	(ambig-pos nil)
	(maybe-unique-p nil))
    (declare (simple-string first) (fixnum buf-pos first-start))
    ;;
    ;; Make room to store each string's next separator index.
    (do ((l rest-strings (cdr l)))
	((endp l))
      (setf (car l) (cons (car l) -1)))
    ;;
    ;; Compare the rest of the strings to the first one.
    ;; It's our de facto standard for how far we can go.
    (loop
      (setf first-start (1+ first-end))
      (setf first-end
	    (position separator first :start first-start :test #'char=))
      (unless first-end
	(setf first-end (length first))
	(setf punt-p t)
	(setf maybe-unique-p t))
      (let ((first-max first-end)
	    (word-ambiguous-p nil))
	(declare (fixnum first-max))
	;;
	;; For each string, store the separator's next index.
	;; If there's no separator, store nil and prepare to punt.
	;; If the string's field is not equal to the first's, shorten the max
	;;   expectation for this field, and declare ambiguity.
	(dolist (s rest-strings)
	  (let* ((str (car s))
		 (str-last-pos (cdr s))
		 (str-start (1+ str-last-pos))
		 (str-end (position separator str
				    :start str-start :test #'char=))
		 (index (string-not-equal first str
					  :start1 first-start :end1 first-max
					  :start2 str-start :end2 str-end)))
	    (declare (simple-string str) (fixnum str-last-pos str-start))
	    (setf (cdr s) str-end)
	    (unless str-end
	      (setf punt-p t)
	      (setf str-end (length str)))
	    (when index
	      (setf word-ambiguous-p t) ; not equal for some reason
	      (when (< index first-max)
		(setf first-max index)))))
	;;
	;; Store what we matched into the result buffer and save the
	;; ambiguous position if its the first ambiguous field.
	(let ((length (- first-max first-start)))
	  (declare (fixnum length))
	  (unless (zerop length)
	    (unless (zerop buf-pos)
	      (setf (schar *complete-string-buffer* buf-pos) separator)
	      (incf buf-pos))
	    (replace *complete-string-buffer* first
		     :start1 buf-pos :start2 first-start :end2 first-max)
	    (incf buf-pos length))
	  (when (and (null ambig-pos) word-ambiguous-p)
	    (setf ambig-pos buf-pos))
	  (when (or punt-p (zerop length)) (return)))))
    (values
     (subseq *complete-string-buffer* 0 buf-pos)
     ;; If every corresponding field in each possible completion was equal,
     ;; our result string is an initial substring of some other completion,
     ;; so we're ambiguous at the end.
     (or ambig-pos buf-pos)
     (and (null ambig-pos)
	  maybe-unique-p
	  (every #'(lambda (x) (null (cdr x))) rest-strings)))))
		 

;;;; Clrstring

(defun clrstring (string-table)
  "Delete all the entries in String-Table."
  (fill (the simple-vector (string-table-value-nodes string-table)) nil)
  (setf (string-table-num-nodes string-table) 0)
  (let ((word-table (string-table-first-word-table string-table)))
    (fill (the simple-vector (word-table-words word-table)) nil)
    (setf (word-table-num-words word-table) 0))
  t)


;;;; Delete-String

(defun delete-string (string string-table)
  (with-folded-string (folded len string (string-table-separator string-table))
    (when (plusp len)
      (let* ((nodes (string-table-value-nodes string-table))
	     (num-nodes (string-table-num-nodes string-table))
	     (end (1- num-nodes)))
	(declare (simple-string folded) (simple-vector nodes)
		 (fixnum num-nodes end))
	(multiple-value-bind
	    (pos found-p)
	    (bi-svposition folded nodes (string-compare* :end1 len)
			   :end end :key #'value-node-folded)
	  (cond (found-p
		 (replace nodes nodes
			  :start1 pos :end1 end :start2 (1+ pos) :end2 num-nodes)
		 (setf (svref nodes end) nil)
		 (setf (string-table-num-nodes string-table) end)
		 (sub-delete-string folded string-table)
		 t)
		(t nil)))))))

(defun sub-delete-string (folded string-table)
  (let ((next-table (string-table-first-word-table string-table))
	(word-table nil)
	(node nil)
	(entry nil)
	(level -1)
	last-table last-table-level last-table-pos
	last-entry last-entry-level)
    (declare (fixnum level))
    (do-words (start end)
      (when node
	(setf last-entry entry)
	(setf last-entry-level level))
      (setf word-table next-table)
      (incf level)
      (let ((word-array (word-table-words word-table))
	    (num-words (word-table-num-words word-table)))
	(declare (simple-vector word-array) (fixnum num-words))
	(multiple-value-bind
	    (pos found-p)
	    (bi-svposition folded word-array
			   (string-compare* :start1 start :end1 end)
			   :end (1- num-words) :key #'word-entry-folded)
	  (declare (fixnum pos) (ignore found-p))
	  (setf entry (svref word-array pos))
	  (setf next-table (word-entry-next-table entry))
	  (setf node (word-entry-value-node entry))
	  (when (or (null last-table) (> num-words 1))
	    (setf last-table word-table)
	    (setf last-table-pos pos)
	    (setf last-table-level level)))))
    (cond (next-table
	   (setf (word-entry-value-node entry) nil))
	  ((and last-entry-level
		(>= last-entry-level last-table-level))
	   (setf (word-entry-next-table last-entry) nil))
	  (t
	   (let* ((del-word-array (word-table-words last-table))
		  (del-num-words (word-table-num-words last-table))
		  (del-end (1- del-num-words)))
	     (declare (simple-vector del-word-array)
		      (fixnum del-num-words del-end))
	     (replace del-word-array del-word-array
		      :start1 last-table-pos :end1 del-end
		      :start2 (1+ last-table-pos)
		      :end2 del-num-words)
	     (setf (svref del-word-array del-end) nil)
	     (setf (word-table-num-words last-table) del-end))))))
