;;; -*- Log: Hemlock.Log; Package: Hemlock-Internals -*-
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
;;; Searching and replacing functions for Hemlock.
;;; Originally written by Skef Wholey, Rewritten by Rob MacLachlan.
;;;

(in-package :hemlock-internals)


;;; The search pattern structure is used only by simple searches, more
;;; complex ones make structures which include it.

(defstruct (search-pattern (:print-function %print-search-pattern)
			   (:constructor internal-make-search-pattern))
  kind			      ; The kind of pattern to search for.
  direction		      ; The direction to search in.
  pattern		      ; The search pattern to use.
  search-function	      ; The function to call to search.
  reclaim-function)	      ; The function to call to reclaim this pattern.

(setf (documentation 'search-pattern-p 'function)
  "Returns true if its argument is a Hemlock search-pattern object,
  Nil otherwise.")

(defun %print-search-pattern (object stream depth)
  (let ((*print-level* (and *print-level* (- *print-level* depth)))
	(*print-case* :downcase))
    (declare (special *print-level* *print-case*))
    (write-string "#<Hemlock " stream)
    (princ (search-pattern-direction object) stream)
    (write-char #\space stream)
    (princ (search-pattern-kind object) stream)
    (write-string " Search-Pattern for ")
    (prin1 (search-pattern-pattern object) stream)
    (write-char #\> stream)
    (terpri stream)))

(defvar *search-pattern-experts* (make-hash-table :test #'eq)
  "Holds an eq hashtable which associates search kinds with the functions
  that know how to make patterns of that kind.")
(defvar *search-pattern-documentation* ()
  "A list of all the kinds of search-pattern that are defined.")

;;; define-search-kind  --  Internal
;;;
;;;    This macro is used to define a new kind of search pattern.  Kind
;;; is the kind of search pattern to define.  Lambda-list is the argument 
;;; list for the expert-function to be built and forms it's body.
;;; The arguments passed are the direction, the pattern, and either
;;; an old search-pattern of the same type or nil.  Documentation
;;; is put on the search-pattern-documentation property of the kind
;;; keyword.
;;;
(defmacro define-search-kind (kind lambda-list documentation &body forms)
  `(progn
     (push ,documentation *search-pattern-documentation*)
     (setf (gethash ,kind *search-pattern-experts*)
           #'(lambda ,lambda-list ,@forms))))

;;; new-search-pattern  --  Public
;;;
;;;    This function deallocates any old search-pattern and then dispatches
;;; to the correct expert.
;;;
(defun new-search-pattern (kind direction pattern &optional
				result-search-pattern)
  "Makes a new Hemlock search pattern of kind Kind to search direction
  using Pattern.  Direction is either :backward or :forward.
  If supplied, result-search-pattern is a pattern to destroy to make
  the new one.  The variable *search-pattern-documentation* contains
  documentation for each kind."
  (unless (or (eq direction :forward) (eq direction :backward))
    (error "~S is not a legal search direction." direction))
  (when result-search-pattern
    (funcall (search-pattern-reclaim-function result-search-pattern)
	     result-search-pattern)
    (unless (eq kind (search-pattern-kind result-search-pattern))
      (setq result-search-pattern nil)))
  (let ((expert (gethash kind *search-pattern-experts*)))
    (unless expert
      (error "~S is not a defined search pattern kind." kind))
    (funcall expert direction pattern result-search-pattern)))

(defun new-search-vector (vec access-fn)
  (let* ((max 0))
    (declare (fixnum max))
    (dotimes (i (length vec))
      (let* ((code (funcall access-fn vec i)))
        (when (> code max)
          (setq max code))))
    (make-array (the fixnum (1+ max)))))

(eval-when (:compile-toplevel :execute)

(defmacro dispose-search-vector (vec)
  vec)
); eval-when (:compile-toplevel :execute)

;;;; macros used by various search kinds:

;;; search-once-forward-macro  --  Internal
;;;
;;;    Passes search-fun strings, starts and lengths to do a forward
;;; search.  The other-args are passed through to the searching
;;; function after after everything else  The search-fun is
;;; expected to return NIL if nothing is found, or it index where the
;;; match ocurred.  Something non-nil is returned if something is
;;; found and line and start are set to where it was found.
;;;
(defmacro search-once-forward-macro (line start search-fun &rest other-args)
  `(do* ((l ,line)
	 (chars (line-chars l) (line-chars l))
	 (len (length chars) (length chars))
	 (start-pos ,start 0)
	 (index 0))
	(())
     (declare (simple-string chars) (fixnum start-pos len)
	      (type (or fixnum null) index))
     (setq index (,search-fun chars start-pos len ,@other-args))
     (when index
       (setq ,start index  ,line l)
       (return t))
     (setq l (line-next l))
     (when (null l) (return nil))))


;;; search-once-backward-macro  --  Internal
;;;
;;;    Like search-once-forward-macro, except it goes backwards.  Length
;;; is not passed to the search function, since it won't need it.
;;;
(defmacro search-once-backward-macro (line start search-fun &rest other-args)
  `(do* ((l ,line)
	 (chars (line-chars l) (line-chars l))
	 (start-pos (1- ,start) (1- (length chars)))
	 (index 0))
	(())
     (declare (simple-string chars) (fixnum start-pos)
	      (type (or fixnum null) index))
     (setq index (,search-fun chars start-pos ,@other-args))
     (when index
       (setq ,start index  ,line l)
       (return t))
     (setq l (line-previous l))
     (when (null l) (return nil))))


;;;; String Searches.
;;;
;;; We use the Boyer-Moore algorithm for string searches.
;;;

;;; sensitive-string-search-macro  --  Internal
;;;
;;;    This macro does a case-sensitive Boyer-Moore string search.
;;;
;;; Args:
;;;    String - The string to search in.
;;;    Start - The place to start searching at.
;;;    Length - NIL if going backward, the length of String if going forward.
;;;    Pattern - A simple-vector of characters.  A simple-vector is used 
;;; rather than a string because it is believed that simple-vector access
;;; will be faster in most implementations.
;;;    Patlen - The length of Pattern.
;;;    Last - (1- Patlen)
;;;    Jumps - The jump vector as given by compute-boyer-moore-jumps
;;;    +/- - The function to increment with, either + (forward) or -
;;; (backward)
;;;    -/+ - Like +/-, only the other way around.
(eval-when (:compile-toplevel :execute)
(defmacro sensitive-string-search-macro (string start length pattern patlen
						last jumps +/- -/+)
  (let* ((jumpslen (gensym))
         (charcode (gensym)))
  `(do ((scan (,+/- ,start ,last))
        (,jumpslen (length ,jumps))
	(patp ,last))
       (,(if length `(>= scan ,length) '(minusp scan)))
     (declare (fixnum scan patp))
     (let ((char (schar ,string scan)))
       (cond
	((char= char (svref ,pattern patp))
	 (if (zerop patp)
	     (return scan)
	     (setq scan (,-/+ scan 1)  patp (1- patp))))
	(t
	 ;; If mismatch consult jump table to find amount to skip.
	 (let* ((,charcode (search-char-code char))
                (jump (if (< ,charcode ,jumpslen)
                        (svref ,jumps ,charcode)
                        ,patlen)))
	   (declare (fixnum jump))
	   (if (> jump (- ,patlen patp))
	       (setq scan (,+/- scan jump))
	       (setq scan (,+/- scan (- ,patlen patp)))))
	 (setq patp ,last)))))))

;;; insensitive-string-search-macro  --  Internal
;;;
;;;    This macro is very similar to the case sensitive one, except that
;;; we do the search for a hashed string, and then when we find a match
;;; we compare the uppercased search string with the found string uppercased
;;; and only say we win when they match too.
;;;
(defmacro insensitive-string-search-macro (string start length pattern
						  folded-string patlen last
						  jumps  +/- -/+)
  (let* ((jumpslen (gensym)))
    `(do ((scan (,+/- ,start ,last))
          (,jumpslen (length ,jumps))
          (patp ,last))
      (,(if length `(>= scan ,length) '(minusp scan)))
      (declare (fixnum scan patp))
      (let ((hash (search-hash-code (schar ,string scan))))
        (declare (fixnum hash))
        (cond
          ((= hash (the fixnum (svref ,pattern patp)))
           (if (zerop patp)
	     (if (do ((i ,last (1- i)))
		     (())
		   (when (char/=
			  (search-char-upcase (schar ,string (,+/- scan i)))
			  (schar ,folded-string i))
		     (return nil))
		   (when (zerop i) (return t)))
               (return scan)
               (setq scan (,+/- scan ,patlen)  patp ,last))
	     (setq scan (,-/+ scan 1)  patp (1- patp))))
          (t
           ;; If mismatch consult jump table to find amount to skip.
           (let ((jump (if (< hash ,jumpslen)
                         (svref ,jumps hash)
                         ,patlen)))
             (declare (fixnum jump))
             (if (> jump (- ,patlen patp))
	       (setq scan (,+/- scan jump))
	       (setq scan (,+/- scan (- ,patlen patp)))))
           (setq patp ,last)))))))

;;;; Searching for strings with newlines in them:
;;;
;;;    Due to the buffer representation, search-strings with embedded 
;;; newlines need to be special-cased.  What we do is break
;;; the search string up into lines and then searching for a line with
;;; the correct prefix.  This is actually a faster search.
;;; For this one we just have one big hairy macro conditionalized for
;;; both case-sensitivity and direction.  Have fun!!

;;; newline-search-macro  --  Internal
;;;
;;;    Do a search for a string containing newlines.  Line is the line
;;; to start on, and Start is the position to start at.  Pattern and
;;; optionally Pattern2, are simple-vectors of things that represent
;;; each line in the pattern, and are passed to Test-Fun.  Pattern
;;; must contain simple-strings so we can take the length.  Test-Fun is a
;;; thing to compare two strings and see if they are equal.  Forward-p
;;; tells whether to go forward or backward.
;;;
(defmacro newline-search-macro (line start test-fun pattern forward-p
				     &optional pattern2)
  `(let* ((patlen (length ,pattern))
	  (first (svref ,pattern 0))
	  (firstlen (length first))
	  (l ,line)
	  (chars (line-chars l))
	  (len (length chars))
	  ,@(if pattern2 `((other (svref ,pattern2 0)))))
     (declare (simple-string first chars) (fixnum firstlen patlen len))
     ,(if forward-p
	  ;; If doing a forward search, go to the next line if we could not
	  ;; match due to the start position.
	  `(when (< (- len ,start) firstlen)
	     (setq l (line-next l)))
	  ;; If doing a backward search, go to the previous line if the current
	  ;; line could not match the last line in the pattern, and then go
	  ;; back the 1- number of lines in the pattern to avoid a possible
	  ;; match across the starting point.
	  `(let ((1-len (1- patlen)))
	     (declare (fixnum 1-len))
	     (when (< ,start (length (the simple-string
					  (svref ,pattern 1-len))))
	       (setq l (line-previous l)))
	     (dotimes (i 1-len)
	       (when (null l) (return nil))
	       (setq l (line-previous l)))))
     (do* ()
	  ((null l))
       (setq chars (line-chars l)  len (length chars))
       ;; If the end of this line is the first line in the pattern then check
       ;; to see if the other lines match.
       (when (and (>= len firstlen)
		  (,test-fun chars first other
			     :start1 (- len firstlen) :end1 len
			     :end2 firstlen))
	 (when
	  (do ((m (line-next l) (line-next m))
	       (i 2 (1+ i))
	       (next (svref ,pattern 1) (svref ,pattern i))
	       ,@(if pattern2
		     `((another (svref ,pattern2 1)
				(svref ,pattern2 i))))
	       (len 0)
	       (nextlen 0)
	       (chars ""))
	      ((null m))
	    (declare (simple-string next chars) (fixnum len nextlen i))
	    (setq chars (line-chars m)  nextlen (length next)
		  len (length chars))
	    ;; When on last line of pattern, check if prefix of line.
	    (when (= i patlen)
	      (return (and (>= len nextlen)
			   (,test-fun chars next another :end1 nextlen
				      :end2 nextlen))))
	    (unless (,test-fun chars next another :end1 len
			       :end2 nextlen)
	      (return nil)))
	  (setq ,line l  ,start (- len firstlen))
	  (return t)))
       ;; If not, try the next line
       (setq l ,(if forward-p '(line-next l) '(line-previous l))))))

;;;; String-comparison macros that are passed to newline-search-macro

;;; case-sensitive-test-fun  --  Internal
;;;
;;;    Just thows away the extra arg and calls string=.
;;;
(defmacro case-sensitive-test-fun (string1 string2 ignore &rest keys)
  (declare (ignore ignore))
  `(string= ,string1 ,string2 ,@keys))

;;; case-insensitive-test-fun  --  Internal
;;;
;;;    First compare the characters hashed with hashed-string2 and then
;;; only if they agree do an actual compare with case-folding.
;;;
(defmacro case-insensitive-test-fun (string1 string2 hashed-string2
					     &key end1 (start1 0) end2)
  `(when (= (- ,end1 ,start1) ,end2)
     (do ((i 0 (1+ i)))
	 ((= i ,end2)
	  (dotimes (i ,end2 t)
	    (when (char/= (search-char-upcase (schar ,string1 (+ ,start1 i)))
			  (schar ,string2 i))
	      (return nil))))
       (when (/= (search-hash-code (schar ,string1 (+ ,start1 i)))
		 (svref ,hashed-string2 i))
	 (return nil)))))
); eval-when (:compile-toplevel :execute)

;;; compute-boyer-moore-jumps  --  Internal
;;;
;;;    Compute return a jump-vector to do a Boyer-Moore search for 
;;; the "string" of things in Vector.  Access-fun is a function
;;; that aref's vector and returns a number.
;;;
(defun compute-boyer-moore-jumps (vec access-fun)
  (declare (simple-vector vec))
  (let ((jumps (new-search-vector vec access-fun))
	(len (length vec)))
    (declare (simple-vector jumps))
    (when (zerop len) (editor-error "Zero length search string not allowed."))
    ;; The default jump is the length of the search string.
    (dotimes (i (length jumps))
      (setf (aref jumps i) len))
    ;; For chars in the string the jump is the distance from the end.
    (dotimes (i len)
      (setf (aref jumps (funcall access-fun vec i)) (- len i 1)))
    jumps))



;;;; Case insensitive searches

;;; In order to avoid case folding, we do a case-insensitive hash of
;;; each character.  We then search for string in this translated
;;; character set, and reject false successes by checking of the found
;;; string is string-equal the the original search string.
;;;

(defstruct (string-insensitive-search-pattern
	    (:include search-pattern)
	    (:conc-name string-insensitive-)
	    (:print-function %print-search-pattern))
  jumps
  hashed-string
  folded-string)

;;;  Search-Hash-String  --  Internal
;;;
;;;    Return a simple-vector containing the search-hash-codes of the
;;; characters in String.
;;;
(defun search-hash-string (string)
  (declare (simple-string string))
  (let* ((len (length string))
	 (result (make-array len)))
    (declare (fixnum len) (simple-vector result))
    (dotimes (i len result)
      (setf (aref result i) (search-hash-code (schar string i))))))

;;; make-insensitive-newline-pattern  -- Internal
;;;
;;;    Make bash in fields in a string-insensitive-search-pattern to
;;; do a search for a string with newlines in it.
;;;
(defun make-insensitive-newline-pattern (pattern folded-string)
  (declare (simple-string folded-string))
  (let* ((len (length folded-string))
	 (num (1+ (count #\newline folded-string :end len)))
	 (hashed (make-array num))
	 (folded (make-array num)))
    (declare (simple-vector hashed folded) (fixnum len num))
    (do ((prev 0 nl)
	 (i 0 (1+ i))
	 (nl (position #\newline folded-string :end len)
	     (position #\newline folded-string :start nl  :end len)))
	((null nl)
	 (let ((piece (subseq folded-string prev len)))
	   (setf (aref folded i) piece)
	   (setf (aref hashed i) (search-hash-string piece))))
      (let ((piece (subseq folded-string prev nl)))
	(setf (aref folded i) piece)
	(setf (aref hashed i) (search-hash-string piece)))
      (incf nl))
    (setf (string-insensitive-folded-string pattern) folded
	  (string-insensitive-hashed-string pattern) hashed)))


(define-search-kind :string-insensitive (direction pattern old)
  ":string-insensitive - Pattern is a string to do a case-insensitive
  search for."
  (unless old (setq old (make-string-insensitive-search-pattern)))
  (setf (search-pattern-kind old) :string-insensitive
	(search-pattern-direction old) direction
	(search-pattern-pattern old) pattern)
  (let* ((folded-string (string-upcase pattern)))
    (declare (simple-string folded-string))
    (cond
     ((find #\newline folded-string)
      (make-insensitive-newline-pattern old folded-string)
      (setf (search-pattern-search-function old)
	    (if (eq direction :forward)
		#'insensitive-find-newline-once-forward-method
		#'insensitive-find-newline-once-backward-method))
      (setf (search-pattern-reclaim-function old) #'identity))
     (t
      (case direction
	(:forward
	 (setf (search-pattern-search-function old)
	       #'insensitive-find-string-once-forward-method))
	(t
	 (setf (search-pattern-search-function old)
	       #'insensitive-find-string-once-backward-method)
	 (setq folded-string (nreverse folded-string))))
      (let ((hashed-string (search-hash-string folded-string)))
	(setf (string-insensitive-hashed-string old) hashed-string
	      (string-insensitive-folded-string old) folded-string)
	(setf (string-insensitive-jumps old)
	      (compute-boyer-moore-jumps hashed-string #'svref))
	(setf (search-pattern-reclaim-function old)
	      #'(lambda (p)
		  (dispose-search-vector (string-insensitive-jumps p))))))))
  old)

(defun insensitive-find-string-once-forward-method (pattern line start)
  (let* ((hashed-string (string-insensitive-hashed-string pattern))
	 (folded-string (string-insensitive-folded-string pattern))
	 (jumps (string-insensitive-jumps pattern))
	 (patlen (length hashed-string))
	 (last (1- patlen)))
    (declare (simple-vector jumps hashed-string) (simple-string folded-string)
	     (fixnum patlen last))
    (when (search-once-forward-macro
	   line start insensitive-string-search-macro
	   hashed-string folded-string patlen last jumps + -)
      (values line start patlen))))

(defun insensitive-find-string-once-backward-method (pattern line start)
  (let* ((hashed-string (string-insensitive-hashed-string pattern))
	 (folded-string (string-insensitive-folded-string pattern))
	 (jumps (string-insensitive-jumps pattern))
	 (patlen (length hashed-string))
	 (last (1- patlen)))
    (declare (simple-vector jumps hashed-string) (simple-string folded-string)
	     (fixnum patlen last))
    (when (search-once-backward-macro
	   line start insensitive-string-search-macro
	   nil hashed-string folded-string patlen last jumps - +)
      (values line (- start last) patlen))))

(eval-when (:compile-toplevel :execute)
(defmacro def-insensitive-newline-search-method (name direction)
  `(defun ,name (pattern line start)
     (let* ((hashed (string-insensitive-hashed-string pattern))
	    (folded-string (string-insensitive-folded-string pattern))
	    (patlen (length (the string (search-pattern-pattern pattern)))))
       (declare (simple-vector hashed folded-string))
       (when (newline-search-macro line start case-insensitive-test-fun
				   folded-string ,direction hashed)
	 (values line start patlen)))))
); eval-when (:compile-toplevel :execute)

(def-insensitive-newline-search-method
  insensitive-find-newline-once-forward-method t)
(def-insensitive-newline-search-method
  insensitive-find-newline-once-backward-method nil)

;;;; And Snore, case sensitive.
;;;
;;;    This is horribly repetitive, but if I introduce another level of
;;; macroexpansion I will go Insaaaane....
;;;
(defstruct (string-sensitive-search-pattern
	    (:include search-pattern)
	    (:conc-name string-sensitive-)
	    (:print-function %print-search-pattern))
  string
  jumps)

;;; make-sensitive-newline-pattern  -- Internal
;;;
;;;    The same, only more sensitive (it hurts when you do that...)
;;;
(defun make-sensitive-newline-pattern (pattern string)
  (declare (simple-vector string))
  (let* ((string (coerce string 'simple-string))
	 (len (length string))
	 (num (1+ (count #\newline string :end len)))
	 (sliced (make-array num)))
    (declare (simple-string string) (simple-vector sliced) (fixnum len num))
    (do ((prev 0 nl)
	 (i 0 (1+ i))
	 (nl (position #\newline string :end len)
	     (position #\newline string :start nl  :end len)))
	((null nl)
	 (setf (aref sliced i) (subseq string prev len)))
      (setf (aref sliced i) (subseq string prev nl))
      (incf nl))
    (setf (string-sensitive-string pattern) sliced)))


(define-search-kind :string-sensitive (direction pattern old)
  ":string-sensitive - Pattern is a string to do a case-sensitive
  search for."
  (unless old (setq old (make-string-sensitive-search-pattern)))
  (setf (search-pattern-kind old) :string-sensitive
	(search-pattern-direction old) direction
	(search-pattern-pattern old) pattern)
  (let* ((string (coerce pattern 'simple-vector)))
    (declare (simple-vector string))
    (cond
     ((find #\newline string)
      (make-sensitive-newline-pattern old string)
      (setf (search-pattern-search-function old)
	    (if (eq direction :forward)
		#'sensitive-find-newline-once-forward-method
		#'sensitive-find-newline-once-backward-method))
      (setf (search-pattern-reclaim-function old) #'identity))
     (t
      (case direction
	(:forward
	 (setf (search-pattern-search-function old)
	       #'sensitive-find-string-once-forward-method))
	(t
	 (setf (search-pattern-search-function old)
	       #'sensitive-find-string-once-backward-method)
	 (setq string (nreverse string))))
      (setf (string-sensitive-string old) string)
      (setf (string-sensitive-jumps old)
	    (compute-boyer-moore-jumps
	     string #'(lambda (v i) (char-code (svref v i)))))
      (setf (search-pattern-reclaim-function old)
	    #'(lambda (p)
		(dispose-search-vector (string-sensitive-jumps p)))))))
  old)


(defun sensitive-find-string-once-forward-method (pattern line start)
  (let* ((string (string-sensitive-string pattern))
	 (jumps (string-sensitive-jumps pattern))
	 (patlen (length string))
	 (last (1- patlen)))
    (declare (simple-vector jumps string) (fixnum patlen last))
    (when (search-once-forward-macro
	   line start sensitive-string-search-macro
	   string patlen last jumps + -)
      (values line start patlen))))

(defun sensitive-find-string-once-backward-method (pattern line start)
  (let* ((string (string-sensitive-string pattern))
	 (jumps (string-sensitive-jumps pattern))
	 (patlen (length string))
	 (last (1- patlen)))
    (declare (simple-vector jumps string) (fixnum patlen last))
    (when (search-once-backward-macro
	   line start sensitive-string-search-macro
	   nil string patlen last jumps - +)
      (values line (- start last) patlen))))

(eval-when (:compile-toplevel :execute)
(defmacro def-sensitive-newline-search-method (name direction)
  `(defun ,name (pattern line start)
     (let* ((string (string-sensitive-string pattern))
	    (patlen (length (the string (search-pattern-pattern pattern)))))
       (declare (simple-vector string))
       (when (newline-search-macro line start case-sensitive-test-fun
				   string ,direction)
	 (values line start patlen)))))
); eval-when (:compile-toplevel :execute)

(def-sensitive-newline-search-method
  sensitive-find-newline-once-forward-method t)
(def-sensitive-newline-search-method
  sensitive-find-newline-once-backward-method nil)

(defun find-pattern (mark search-pattern &optional stop-mark)
  "Find a match of Search-Pattern starting at Mark.  Mark is moved to
  point before the match and the number of characters matched is returned.
  If there is no match for the pattern then Mark is not modified and NIL
  is returned.
  If stop-mark is specified, NIL is returned and mark is not moved if
  the point before the match is after stop-mark for forward search or
  before stop-mark for backward search"
  (close-line)
  (multiple-value-bind (line start matched)
		       (funcall (search-pattern-search-function search-pattern)
				search-pattern (mark-line mark)
				(mark-charpos mark))
    (when (and matched
	       (or (null stop-mark)
                   (if (eq (search-pattern-direction search-pattern) :forward)
                     (or (< (line-number line) (line-number (mark-line stop-mark)))
                         (and (eq line (mark-line stop-mark))
                              (<= start (mark-charpos stop-mark))))
                     (or (< (line-number (mark-line stop-mark)) (line-number line))
                         (and (eq (mark-line stop-mark) line)
                              (<= (mark-charpos stop-mark) start))))))
      (move-to-position mark start line)
      matched)))

;;; replace-pattern  --  Public
;;;
;;;
(defun replace-pattern (mark search-pattern replacement &optional n)
  "Replaces N occurrences of the Search-Pattern with the Replacement string
  in the text starting at the given Mark.  If N is Nil, all occurrences 
  following the Mark are replaced."
  (close-line)
  (do* ((replacement (coerce replacement 'simple-string))
	(new (length (the simple-string replacement)))
	(fun (search-pattern-search-function search-pattern))
	(forward-p (eq (search-pattern-direction search-pattern) :forward))
	(n (if n (1- n) -1) (1- n))
	(m (copy-mark mark :temporary)) line start matched)
       (())
    (multiple-value-setq (line start matched)
      (funcall fun search-pattern (mark-line m) (mark-charpos m)))
    (unless matched (return m))
    (setf (mark-line m) line  (mark-charpos m) start)
    (delete-characters m matched)
    (insert-string m replacement)
    (when forward-p (character-offset m new))
    (when (zerop n) (return m))
    (close-line)))
