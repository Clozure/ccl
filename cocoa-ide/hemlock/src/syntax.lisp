;;; -*- Log: hemlock.log; Package: Hemlock-Internals -*-
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
;;; Hemlock syntax table routines.
;;;
;;; Written by Rob MacLachlan.
;;;

(in-package :hemlock-internals)


;;;; Character attribute caching.
;;;
;;;    In order to permit the %SP-Find-Character-With-Attribute sub-primitive
;;; to be used for a fast implementation of find-attribute and
;;; reverse-find-attribute, there must be some way of translating 
;;; attribute/test-function pairs into a attribute vector and a mask.
;;;    What we do is maintain a eq-hash-cache of attribute/test-function
;;; pairs.  If the desired pair is not in the cache then we reclaim an old
;;; attribute bit in the bucket we hashed to and stuff it by calling the
;;; test function on the value of the attribute for all characters.

(defvar *character-attribute-cache* ()
  "This is the cache used to translate attribute/test-function pairs to
  attribute-vector/mask pairs for find-attribute and reverse-find-attribute.")

(defconstant character-attribute-cache-size 13
  "The number of buckets in the *character-attribute-cache*.")
(defconstant character-attribute-bucket-size 3
  "The number of bits to use in each bucket of the
  *character-attribute-cache*.")


(defconstant character-attribute-cache-size 13
  "The number of buckets in the character-attribute-cache.")
(defconstant character-attribute-bucket-size 3
  "The number of bits to use in each bucket of the character-attribute-cache.")

(defstruct (shadow-syntax (:conc-name "SS-"))
  ;;;    In addition, since a common pattern in code which uses find-attribute
  ;;; is to repeatedly call it with the same function and attribute, we
  ;;; remember the last attribute/test-function pair that was used, and check
  ;;; if it is the same pair beforehand, thus often avoiding the hastable lookup.
  ;; TODO: another common pattern is to use the same attribute but
  ;;       different functions (toggling between zerop and not-zerop), so
  ;;       should use a scheme that handles that - this doesn't.
  ;; The attribute which we last did a find-attribute on
  (last-find-attribute-attribute ())
  ;; The last test-function used for find-attribute.
  (last-find-attribute-function ())
  ;; The %SP-Find-Character-With-Attribute vector corresponding to the last
  ;; attribute/function pair used for find-attribute.
  (last-find-attribute-vector ())
  ;; The the mask to use with *last-find-attribute-vector* to do a search
  ;; for the last attribute/test-function pair.
  (last-find-attribute-mask ())
  ;; The the value of End-Wins for the last attribute/test-function pair.
  (last-find-attribute-end-wins ())

  ;; The last character attribute which was asked for
  (last-character-attribute-requested nil)
  ;; The value of the most recent character attribute
  (value-of-last-character-attribute-requested #() :type (simple-array * (*)))

  ;; list of shadowed bits.
  (shadow-bit-descriptors ())
  ;; List of shadowed attribute vectors
  (shadow-attributes ())
  ;; Syntax tick count at the time shadow info was computed.
  (global-syntax-tick -1))

(defvar *global-syntax-tick* 0 "Tick count noting changes in global syntax settings")

(declaim (special *current-buffer*))


(declaim (inline current-buffer-shadow-syntax))
(defun current-buffer-shadow-syntax ()
  (let ((buffer *current-buffer*))
    (when buffer
      (let ((ss (buffer-shadow-syntax buffer)))
	(if (and ss (eql (ss-global-syntax-tick ss) *global-syntax-tick*))
	  ss
	  (progn
	    (%init-shadow-attributes buffer)
	    (buffer-shadow-syntax buffer)))))))

(defvar *character-attributes* (make-hash-table :test #'eq)
  "A hash table which translates character attributes to their values.")

(declaim (special *character-attribute-names*))


;;; Each bucket contains a list of character-attribute-bucket-size
;;; bit-descriptors.
;;;
(defstruct (bit-descriptor)
  function		      ; The test on the attribute.
  attribute		      ; The attribute this is a test of.
  (mask 0 :type fixnum)	      ; The mask for the corresponding bit.
  vector		      ; The vector the bit is in.
  end-wins)		      ; Is this test true of buffer ends?

;;;
;;; In a descriptor for an unused bit, the function is nil, preventing a
;;; hit.  Whenever we change the value of an attribute for some character,
;;; we need to flush the cache of any entries for that attribute.  Currently
;;; we do this by mapping down the list of all bit descriptors.  Note that
;;; we don't have to worry about GC, since this is just a hint.
;;;
(defvar *all-bit-descriptors* () "The list of all the bit descriptors.")


(defmacro allocate-bit (vec bit-num)
  `(progn
    (when (= ,bit-num 8)
      (setq ,bit-num 0  ,vec (make-array 256 :element-type '(mod 256))))
    (car (push (make-bit-descriptor
		:vector ,vec
		:mask (ash 1 (prog1 ,bit-num (incf ,bit-num))))
	       *all-bit-descriptors*))))
;;;    
(defun %init-syntax-table ()
  (let ((tab (make-array character-attribute-cache-size))
	(bit-num 8) vec)
    (setq *character-attribute-cache* tab)
    (dotimes (c character-attribute-cache-size)
      (setf (svref tab c)
	    (do ((i 0 (1+ i))
		 (res ()))
		((= i character-attribute-bucket-size) res)
	      (push (allocate-bit vec bit-num) res))))))

#+NIL
(defmacro hash-it (attribute function)
  `(abs (rem (logxor (ash (lisp::%sp-make-fixnum ,attribute) -3)
		     (lisp::%sp-make-fixnum ,function))
	     character-attribute-cache-size)))
(defmacro hash-it (attribute function)
  `(abs (rem (logxor (ash (sxhash ,attribute) -3)
		     (sxhash ,function))
	     character-attribute-cache-size)))

;;; CACHED-ATTRIBUTE-LOOKUP  --  Internal
;;;
;;;    Sets Vector and Mask such that they can be used as arguments
;;; to %sp-find-character-with-attribute to effect a search with attribute 
;;; Attribute and test Function.  If the function and attribute
;;; are the same as the last ones then we just set them to that, otherwise
;;; we do the hash-cache lookup and update the *last-find-attribute-<mumble>*
;;;
(defmacro cached-attribute-lookup (attribute function vector mask end-wins)
  `(let ((ss (current-buffer-shadow-syntax)))
     (if (and (eq ,function (ss-last-find-attribute-function ss))
	      (eq ,attribute (ss-last-find-attribute-attribute ss)))
       (setq ,vector (ss-last-find-attribute-vector ss)
	     ,mask (ss-last-find-attribute-mask ss)
	     ,end-wins (ss-last-find-attribute-end-wins ss))
       (let ((b (or (loop for b in (ss-shadow-bit-descriptors ss)
		      when (and (eq (bit-descriptor-attribute b) ,attribute)
				(eq (bit-descriptor-function b) ,function))
		      return b)
		    (loop for b in (svref *character-attribute-cache*
					  (hash-it ,attribute ,function))
		      when (and (eq (bit-descriptor-attribute b) ,attribute)
				(eq (bit-descriptor-function b) ,function))
		      return b))))
	 (cond (b 
		(setq ,vector (bit-descriptor-vector b)
		      ,mask (bit-descriptor-mask b)
		      ,end-wins (bit-descriptor-end-wins b)))
	       (t
		(multiple-value-setq (,vector ,mask ,end-wins)
		  (new-cache-attribute ,attribute ,function))))
	 (setf (ss-last-find-attribute-attribute ss) ,attribute
	       (ss-last-find-attribute-function ss) ,function
	       (ss-last-find-attribute-vector ss) ,vector
	       (ss-last-find-attribute-mask ss) ,mask
	       (ss-last-find-attribute-end-wins ss) ,end-wins)))))

;;; NEW-CACHE-ATTRIBUTE  --  Internal
;;;
;;;    Pick out an old attribute to punt out of the cache and put in the
;;; new one.  We pick a bit off of the end of the bucket and pull it around
;;; to the beginning to get a degree of LRU'ness.
;;;
(defun new-cache-attribute (attribute function)
  (let* ((hash (hash-it attribute function))
	 (values (or (gethash attribute *character-attributes*)
		     (error "~S is not a defined character attribute."
			    attribute)))
	 (bucket (svref *character-attribute-cache* hash))
	 (bit (nthcdr (- character-attribute-bucket-size 2) bucket))
	 (end-wins (funcall function (attribute-descriptor-end-value values))))
    (shiftf bit (cdr bit) nil)
    (setf (svref *character-attribute-cache* hash) bit
	  (cdr bit) bucket  bit (car bit))
    (setf (bit-descriptor-attribute bit) attribute
	  (bit-descriptor-function bit) function
	  (bit-descriptor-end-wins bit) end-wins)
    (incf *global-syntax-tick*)
    (setq values (attribute-descriptor-vector values))
    (do ((mask (bit-descriptor-mask bit))
	 (fun (bit-descriptor-function bit))
	 (vec (bit-descriptor-vector bit))
	 (i 0 (1+ i)))
	((= i syntax-char-code-limit) (values vec mask end-wins))
      (declare (type (simple-array (mod 256)) vec))
      (if (funcall fun (aref (the simple-array values) i))
	(setf (aref vec i) (logior (aref vec i) mask))
	(setf (aref vec i) (logandc2 (aref vec i) mask))))))

(defun %print-attribute-descriptor (object stream depth)
  (declare (ignore depth))
  (format stream "#<Hemlock Attribute-Descriptor ~S>"
	  (attribute-descriptor-name object)))

;;; DEFATTRIBUTE  --  Public
;;;
;;;    Make a new vector of some type and enter it in the table.
;;;
(defun defattribute (name documentation &optional (type '(mod 2))
			  (initial-value 0))
  "Define a new Hemlock character attribute with named Name with
  the supplied Documentation, Type and Initial-Value.  Type
  defaults to (mod 2) and Initial-Value defaults to 0."
  (setq name (coerce name 'simple-string))
  (let* ((attribute (string-to-keyword name))
	 (new (make-attribute-descriptor
	       :vector (make-array syntax-char-code-limit
				   :element-type type
				   :initial-element initial-value)
	       :name name
	       :keyword attribute
	       :documentation documentation
	       :end-value initial-value)))
    (when (gethash attribute *character-attributes*)
      (warn "Character Attribute ~S is being redefined." name))
    (setf (getstring name *character-attribute-names*) attribute)
    (setf (gethash attribute *character-attributes*) new))
    (incf *global-syntax-tick*)
  name)

;;; WITH-ATTRIBUTE  --  Internal
;;;
;;;    Bind obj to the attribute descriptor corresponding to symbol,
;;; giving error if it is not a defined attribute.
;;;
(defmacro with-attribute ((obj symbol) &body forms)
  `(let ((,obj (gethash ,symbol *character-attributes*)))
     (unless ,obj
       (error "~S is not a defined character attribute." ,symbol))
     ,@forms))

(defun character-attribute-name (attribute)
  "Return the string-name of the character-attribute Attribute."
  (with-attribute (obj attribute)
    (attribute-descriptor-name obj)))

(defun character-attribute-documentation (attribute)
  "Return the documentation for the character-attribute Attribute."
  (with-attribute (obj attribute)
    (attribute-descriptor-documentation obj)))

(defun character-attribute-hooks (attribute)
  "Return the hook-list for the character-attribute Attribute.  This can
  be set with Setf."
  (with-attribute (obj attribute)
    (attribute-descriptor-hooks obj)))

(defun %set-character-attribute-hooks (attribute new-value)
  (with-attribute (obj attribute)
    (setf (attribute-descriptor-hooks obj) new-value)))

;;; CHARACTER-ATTRIBUTE  --  Public
;;;
;;;    Return the value of a character attribute for some character.
;;;
(defun character-attribute (attribute character)
  "Return the value of the the character-attribute Attribute for Character.
  If Character is Nil then return the end-value."
  (let ((ss (current-buffer-shadow-syntax)))
    (if (and character ss (eq attribute (ss-last-character-attribute-requested ss)))
      (aref (ss-value-of-last-character-attribute-requested ss) (syntax-char-code character))
      (sub-character-attribute attribute character))))
;;;
(defun sub-character-attribute (attribute character)
  (with-attribute (obj attribute)
    (let* ((ss (current-buffer-shadow-syntax))
	   (cell (and ss (cdr (assoc obj (ss-shadow-attributes ss) :test #'eq)))))
      (if character
	(let ((vec (if cell (car cell) (attribute-descriptor-vector obj))))
	  (when ss
	    (setf (ss-last-character-attribute-requested ss) attribute)
	    (setf (ss-value-of-last-character-attribute-requested ss) vec))
	  (aref (the simple-array vec) (syntax-char-code character)))
	(if cell (cdr cell) (attribute-descriptor-end-value obj))))))

;;; CHARACTER-ATTRIBUTE-P
;;;
;;;    Look up attribute in table.
;;;
(defun character-attribute-p (symbol)
  "Return true if Symbol is the symbol-name of a character-attribute, Nil
  otherwise."
  (not (null (gethash symbol *character-attributes*))))


;;; %SET-CHARACTER-ATTRIBUTE  --  Internal
;;;
;;;    Set the global value of a character attribute.
;;;
(defun %set-character-attribute (attribute character new-value)
  (with-attribute (obj attribute)
    (invoke-hook hemlock::character-attribute-hook attribute character new-value)
    (invoke-hook (attribute-descriptor-hooks obj) attribute character new-value)
    (cond
     ;;
     ;; Setting the value for a real character.
     (character
      (let ((value (attribute-descriptor-vector obj))
	    (code (syntax-char-code character)))
	(declare (type (simple-array *) value))
	(dolist (bit *all-bit-descriptors*)
	  (when (eq (bit-descriptor-attribute bit) attribute)
	    (let ((vec (bit-descriptor-vector bit)))
	      (declare (type (simple-array (mod 256)) vec))
	      (setf (aref vec code)
		    (if (funcall (bit-descriptor-function bit) new-value)
			(logior (bit-descriptor-mask bit) (aref vec code))
			(logandc1 (bit-descriptor-mask bit) (aref vec code)))))))
	(setf (aref value code) new-value)))
     ;;
     ;; Setting the magical end-value.
     (t
      (setf (attribute-descriptor-end-value obj) new-value)
      (dolist (bit *all-bit-descriptors*)
	(when (eq (bit-descriptor-attribute bit) attribute)
	  (setf (bit-descriptor-end-wins bit)
		(funcall (bit-descriptor-function bit) new-value))))))
    (incf *global-syntax-tick*)
    new-value))

;; This is called when change buffer mode.  It used to invoke attribute-descriptor-hooks on
;; all the shadowed attributes.  We don't do that any more, should update doc if any.
(defun invalidate-shadow-attributes (buffer)
  (let ((ss (buffer-shadow-syntax buffer)))
    (when ss (setf (ss-global-syntax-tick ss) -1))))

(defun %init-one-shadow-attribute (ss desc vals)
  ;; Shadow all bits for this attribute
  (loop with key = (attribute-descriptor-keyword desc)
    for bit in *all-bit-descriptors*
    when (eq key (bit-descriptor-attribute bit))
    do (let* ((fun (bit-descriptor-function bit))
	      (b (or (find-if #'(lambda (b)
				  (and (eq (bit-descriptor-function b) fun)
				       (eq (bit-descriptor-attribute b) key)))
			      (ss-shadow-bit-descriptors ss))
		     (let ((new (make-bit-descriptor
				 :attribute key
				 :function fun
				 :vector (copy-seq (bit-descriptor-vector bit))
				 :mask (bit-descriptor-mask bit))))
		       (push new (ss-shadow-bit-descriptors ss))
		       new)))
	      (vec (bit-descriptor-vector b)))
	 (loop for (code . value) in vals
	   ;; Since we don't share the shadow vecs, no need to preserve other bits.
	   do (setf (aref vec code) (if (funcall fun value) #xFF #x00)))))
  ;; Shadow the attribute values
  (let ((vec (cadr (or (assoc desc (ss-shadow-attributes ss) :test #'eq)
		       (let ((new (list* desc 
					 (copy-seq (attribute-descriptor-vector desc))
					 (attribute-descriptor-end-value desc))))
			 (push new (ss-shadow-attributes ss))
			 new)))))
    (loop for (code . value) in vals do (setf (aref vec code) value))))

(defun %init-shadow-attributes (buffer)
  (let* ((mode (buffer-major-mode-object buffer))
	 (ss (or (buffer-shadow-syntax buffer)
		 (setf (buffer-shadow-syntax buffer) (make-shadow-syntax)))))
    (loop for (desc .  vals) in (mode-object-character-attributes mode)
      do (%init-one-shadow-attribute ss desc vals))
    (setf (ss-last-find-attribute-attribute ss) nil)
    (setf (ss-last-find-attribute-function ss) nil)
    (setf (ss-global-syntax-tick ss) *global-syntax-tick*)))

(declaim (special *mode-names*))

;;; SHADOW-ATTRIBUTE  --  Public
;;;
;;;    Stick mode character attribute information in the mode object.
;;;
(defun shadow-attribute (attribute character value mode)
  "Make a mode specific character attribute value.  The value of
  Attribute for Character when we are in Mode will be Value."
  (let ((desc (gethash attribute *character-attributes*))
	(obj (getstring mode *mode-names*)))
    (unless desc
      (error "~S is not a defined Character Attribute." attribute))
    (unless obj (error "~S is not a defined Mode." mode))
    (let* ((current (assoc desc (mode-object-character-attributes obj)))
	   (code (syntax-char-code character))
	   (cons (cons code value)))
      (if current
	  (let ((old (assoc code (cdr current))))
	    (if old
		(setf (cdr old) value  cons old)
		(push cons (cdr current))))
	  (push (list desc cons)
		(mode-object-character-attributes obj)))
      (incf *global-syntax-tick*)
      (invoke-hook hemlock::shadow-attribute-hook attribute character value mode)))
  attribute)

;;; UNSHADOW-ATTRIBUTE  --  Public
;;;
;;;    Nuke a mode character attribute.
;;;
(defun unshadow-attribute (attribute character mode)
  "Make the value of Attribte for Character no longer shadowed in Mode."
  (let ((desc (gethash attribute *character-attributes*))
	(obj (getstring mode *mode-names*)))
    (unless desc
      (error "~S is not a defined Character Attribute." attribute))
    (unless obj
      (error "~S is not a defined Mode." mode))
    (invoke-hook hemlock::shadow-attribute-hook mode attribute character)
    (let* ((current (assoc desc (mode-object-character-attributes obj)))
	   (char (assoc (syntax-char-code character) (cdr current))))
      (unless char
	(error "Character Attribute ~S is not defined for character ~S ~
	       in Mode ~S." attribute character mode))
      (incf *global-syntax-tick*)
      (setf (cdr current) (delete char (the list (cdr current))))))
  attribute)


;;; NOT-ZEROP, the default test function for find-attribute etc.
;;;
(defun not-zerop (n)
  (not (zerop n)))

;;; find-attribute  --  Public
;;;
;;;    Do hairy cache lookup to find a find-character-with-attribute style
;;; vector that we can use to do the search.
;;;
(defmacro normal-find-attribute (line start result vector mask)
  `(let ((chars (line-chars ,line)))
     (setq ,result (%sp-find-character-with-attribute
		   chars ,start (strlen chars) ,vector ,mask))))
;;;
(defmacro cache-find-attribute (start result vector mask)
  `(let ((gap (- (current-right-open-pos) (current-left-open-pos))))
     (declare (fixnum gap))
     (cond
      ((>= ,start (current-left-open-pos))
       (setq ,result
	     (%sp-find-character-with-attribute
	      (current-open-chars) (+ ,start gap) (current-line-cache-length) ,vector ,mask))
       (when ,result (decf ,result gap)))
      ((setq ,result (%sp-find-character-with-attribute
		      (current-open-chars) ,start (current-left-open-pos) ,vector ,mask)))
      (t
       (setq ,result
	     (%sp-find-character-with-attribute
	      (current-open-chars) (current-right-open-pos) (current-line-cache-length) ,vector ,mask))
       (when ,result (decf ,result gap))))))

;;;
(defun find-attribute (mark attribute &optional (test #'not-zerop))
  "Find the next character whose attribute value satisfies test."
  (let ((charpos (mark-charpos mark))
	(line (mark-line mark))
	(mask 0)
	vector end-wins)
    (declare (type (or (simple-array (mod 256)) null) vector) (fixnum mask)
	     (type (or fixnum null) charpos))
    (cached-attribute-lookup attribute test vector mask end-wins)
    (cond
     ((cond
       ((current-open-line-p line)
	(when (cache-find-attribute charpos charpos vector mask)
	  (setf (mark-charpos mark) charpos) mark))
       (t
	(when (normal-find-attribute line charpos charpos vector mask)
	  (setf (mark-charpos mark) charpos) mark))))
     ;; Newlines win and there is one.
     ((and (not (zerop (logand mask (aref vector (char-code #\newline)))))
	   (line-next line))
      (move-to-position mark (line-length line) line))
     ;; We can ignore newlines.
     (t
      (do (prev)
	  (())
	(setq prev line  line (line-next line))
	(cond
	 ((null line)
	  (if end-wins
	      (return (line-end mark prev))
	      (return nil)))
	 ((current-open-line-p line)
	  (when (cache-find-attribute 0 charpos vector mask)
	    (return (move-to-position mark charpos line))))
	 (t
	  (when (normal-find-attribute line 0 charpos vector mask)
	    (return (move-to-position mark charpos line))))))))))

(defun find-not-attribute (mark attribute)
  (find-attribute mark attribute #'zerop))


;;; REVERSE-FIND-ATTRIBUTE  --  Public
;;;
;;;    Line find-attribute, only goes backwards.
;;;
(defmacro rev-normal-find-attribute (line start result vector mask)
  `(let ((chars (line-chars ,line)))
     (setq ,result (%sp-reverse-find-character-with-attribute
		    chars 0 ,(or start '(strlen chars)) ,vector ,mask))))
;;;
(defmacro rev-cache-find-attribute (start result vector mask)
  `(let ((gap (- (current-right-open-pos) (current-left-open-pos))))
     (declare (fixnum gap))
     (cond
      ,@(when start
	  `(((<= ,start (current-left-open-pos))
	     (setq ,result
		   (%sp-reverse-find-character-with-attribute
		    (current-open-chars) 0 ,start ,vector ,mask)))))
      ((setq ,result (%sp-reverse-find-character-with-attribute
		      (current-open-chars) (current-right-open-pos)
		      ,(if start `(+ ,start gap) '(current-line-cache-length))
		      ,vector ,mask))
       (decf ,result gap))
      (t
       (setq ,result
	     (%sp-reverse-find-character-with-attribute
	      (current-open-chars) 0 (current-left-open-pos) ,vector ,mask))))))

;;;
;;; This moves the mark so that previous-character satisfies the test.
(defun reverse-find-attribute (mark attribute &optional (test #'not-zerop))
  "Find the previous character whose attribute value satisfies test."
  (let* ((charpos (mark-charpos mark))
	 (line (mark-line mark)) vector mask end-wins)
    (declare (type (or (simple-array (mod 256)) null) vector)
	     (type (or fixnum null) charpos))
    (cached-attribute-lookup attribute test vector mask end-wins)
    (cond 
     ((cond
       ((current-open-line-p line)
	(when (rev-cache-find-attribute charpos charpos vector mask)
	  (setf (mark-charpos mark) (1+ charpos)) mark))
       (t
	(when (rev-normal-find-attribute line charpos charpos vector mask)
	  (setf (mark-charpos mark) (1+ charpos)) mark))))
     ;; Newlines win and there is one.
     ((and (line-previous line)
	   (not (zerop (logand mask (aref vector (char-code #\newline))))))
      (move-to-position mark 0 line))
     (t
      (do (next)
	  (())
	(setq next line  line (line-previous line))
	(cond
	 ((null line)
	  (if end-wins
	      (return (line-start mark next))
	      (return nil)))
	 ((current-open-line-p line)
	  (when (rev-cache-find-attribute nil charpos vector mask)
	    (return (move-to-position mark (1+ charpos) line))))
	 (t
	  (when (rev-normal-find-attribute line nil charpos vector mask)
	    (return (move-to-position mark (1+ charpos) line))))))))))

(defun reverse-find-not-attribute (mark attribute)
  (reverse-find-attribute mark attribute #'zerop))
