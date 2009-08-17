;;;; TRIVIAL-LDAP v0.91 -- a one file, all lisp client implementation of
;;;; parts of RFC 2261.  

;;;; Please see the trivial-ldap.html file for documentation and limitations.

;;;; Updates are available at:
;;;;    http://homepage.mac.com/ignavusinfo/trivial-ldap/

;;;; TRIVIAL-LDAP is Copyright 2005-2009 Kevin Montuori and is 
;;;; distributed under The Clarified Artistic License, a copy of which
;;;; should have accompanied this file.

;;;; Kevin Montuori <montuori@gmail.com>

(defpackage :trivial-ldap
    (:use :cl-user :common-lisp :usocket)
  (:nicknames :ldap)
  (:shadow :delete :search)
  (:export
   ; mod types.
   delete replace add 
   ; search option symbols
   base sub one never search find always
   ; objects.
   entry ldap
   ; methods.
   #:user #:pass #:base #:debugflag #:host #:port #:rdn #:dn #:attrs #:compare
   #:sslflag #:reuse-connection #:rebind
   #:bind #:unbind #:abandon #:add #:delete #:moddn #:search 
   #:new-entry-from-list #:replace-attr #:del-attr #:add-attr #:modify
   #:attr-list #:attr-value #:new-entry #:new-ldap #:ldif #:change-rdn
   #:response #:results-pending-p #:next-search-result
   ; convenience macros
   #:dosearch #:ldif-search))
	   
(in-package :trivial-ldap)

;;;;
;;;; error conditions
;;;;

(define-condition ldap-error ()
  ((note :initarg :mesg
	 :reader mesg
	 :initform "LDAP transaction resulted in an error."))
  (:report (lambda (c stream)
	     (format stream "~A~%" (mesg c)))))

(define-condition ldap-filter-error (ldap-error)
  ((filter :initarg :filter
	   :reader filter
	   :initform "Not Supplied"))
  (:report (lambda (c stream)
	     (format stream "Filter Error: ~A~%Supplied Filter: ~A~%" 
		     (mesg c) (filter c)))))

(define-condition ldap-connection-error (ldap-error)
  ((host :initarg :host
	 :reader  host)
   (port :initarg :port
	 :reader port))
  (:report (lambda (c stream)
	     (format stream "LDAP Connection Error: ~A~%Host:Port: ~A:~A~%"
		     (mesg c) (host c) (port c)))))

(define-condition ldap-response-error (ldap-error)
    ((dn   :initarg :dn
	   :reader dn
	   :initform "DN not available.")
     (code :initarg :code
	   :reader code
	   :initform "Result code not available")
     (msg  :initarg :msg
	   :reader msg
	   :initform "N/A"))
    (:report (lambda (c stream)
	       (format stream "~A~%DN: ~A~%Code: ~A~%Message: ~A~%"
		       (mesg c) (dn c) (code c) (msg c)))))

;;;;
;;;; utility functions
;;;;


;; to appease sbcl (see http://tinyurl.com/auqmr):
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
    ,@(when doc (list doc))))

(defparameter *hex-print* "~A~%~{~<~%~1,76:;~2,'0,,X~> ~}~%"
  "Format directive to print a list of line wrapped hex numbers.")

(defun base10->base256 (int)
  "Return representation of an integer as a list of base 256 'digits'."
  (assert (and (integerp int) (>= int 0)))
  (or 
   (do ((i 0 (+ i 8))
	(j int (ash j -8))
	(result nil (cons (logand #xFF j) result)))
       ((> i (1- (integer-length int))) result))
   (list 0)))

(defun base256->base10 (list)
  "Given a list of base 256 'digits' return an integer."
  (assert (consp list))
  (let ((len (length list)))
    (do ((i 0 (1+ i))
	 (j (- len 1) (1- j))
	 (int 0 (dpb (pop list) (byte 8 (* 8 j)) int)))
	((= i len) int))))

(defun int->octet-list (int)
  "Return 2s comp. representation of INT."
   (assert (integerp int))
   (do ((i 0 (+ i 8))
	(j int (ash j -8))
	(result nil (cons (logand #xFF j) result)))
       ((> i (integer-length int)) result)))

(defun octet-list->int (octet-list)
  "Convert sequence of twos-complement octets into an integer."
  (assert (consp octet-list))
  (let ((int 0))
    (dolist (value octet-list int) (setq int (+ (ash int 8) value)))))

(defun string->char-code-list (string)
  "Convert a string into a list of bytes."
   (let ((string (etypecase string 
 		  (string string)
 		  (symbol (symbol-name string)))))
     #-(or allegro ccl sbcl)
     (map 'list #'char-code string)
     #+ccl
     (coerce 
      (ccl::encode-string-to-octets string :external-format :utf-8) 'list)
     #+sbcl
     (coerce (sb-ext:string-to-octets string :external-format :utf-8) 'list)
     #+allegro
     (coerce (excl:string-to-octets string :null-terminate nil) 'list)))

(defun char-code-list->string (char-code-list)
  "Convert a list of bytes into a string."
  (assert (or (null char-code-list) (consp char-code-list)))
  #-(or allegro ccl sbcl)
  (map 'string #'code-char char-code-list)
  #+ccl
  (ccl::decode-string-from-octets (make-array (list (length char-code-list))
					      :element-type '(unsigned-byte 8)
					      :initial-contents char-code-list)
				  :external-format :utf-8)
  #+sbcl
  (sb-ext:octets-to-string (make-array (list (length char-code-list))
				       :element-type '(unsigned-byte 8)
				       :initial-contents char-code-list)
			   :external-format :utf-8)
  #+allegro
  (excl:octets-to-string (make-array (list (length char-code-list))
				     :element-type '(unsigned-byte 8)
				     :initial-contents char-code-list)
			 :external-format :utf8))

(defun filter-string->sexp (filter)
  "Convert a filter-string into a sexp."
  (unless (and (char= #\( (char filter 0))
 	       (char= #\) (char filter (1- (length filter)))))
    (setf filter (format nil "(~A)" filter)))
  (unless (balanced-parentheses-p filter)
    (error 'ldap-filter-error :mesg "unbalanced parentheses" :filter filter))
  (let ((*package* (find-package :ldap)))
    (read-from-string (rearrange-filter (quote-verticals filter)) nil nil)))

(defun quote-verticals (string)
  "Backslash quote the character | (for filter strings)"
  (with-output-to-string (stream)
			 (loop for char across string do
			       (when (char= char #\|) (write-char #\\ stream))
			       (write-char char stream))))

(defun balanced-parentheses-p (string)
  (let ((level 0))
    (dotimes (i (length string) (zerop level))
      (case (char string i)
	(#\( (incf level))
	(#\) (if (zerop level) (return nil) (decf level)))))))
  
(defun rearrange-filter (string &optional (beg 0))
  "Prepare a filter to be (read)."
  (let ((pos (position #\= string :start beg)))
    (if pos
	(flet ((trim (x) (string-trim '(#\Space) x)))
	  (let* ((opening-paren (position #\( string :from-end t :end pos))
		 (closing-paren (position #\) string :start pos))
		 (prev (char string (1- pos)))
		 (next (char string (1+ pos)))
		 (op-bounds 
		  (cond
		    ((or (eql #\~ prev) (eql #\< prev) (eql #\> prev)) 
		     (list (1- pos) (1+ pos)))
		    ((eql #\* next) (list pos (+ 2 pos)))
		    (t (list pos (1+ pos)))))
		 (op  (subseq string (first op-bounds) (second op-bounds)))
		 (att (trim (subseq string (1+ opening-paren) 
				    (first op-bounds))))
		 (val (trim (subseq string (second op-bounds) closing-paren)))
		 (pre-part (subseq string 0 (1+ opening-paren)))
		 (post-part (subseq string closing-paren)))
	    (unless (attribute-p att)
	      (error 'ldap-filter-error
		     :mesg (format nil "invalid attribute in filter: '~A'" att)
		     :filter string))
	    (when (and (string= op "=*") (not (string= val "")))
	      (setf op "=" val (format nil "*~A" val)))
	    (let ((this-part (format nil "~A ~A |~A|" op att val)))
	      (rearrange-filter 
	       (format nil "~A~A~A" pre-part this-part post-part)
	       (+ (length pre-part) (length this-part))))))
	string)))

(defun split-substring (string &optional list)
  "Split a substring filter value into a list, retaining the * separators."
  (let ((pos (position #\* string)))
    (if pos
	(let* ((capture (subseq string 0 pos))
	       (vals (if (string= capture "") (list "*") (list "*" capture))))
	  (split-substring (subseq string (1+ pos))(append vals list)))
	(nreverse (if (string= string "") list (push string list))))))

;;;;
;;;; BER encoding constants and constructors.
;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +max-int+ (- (expt 2 31) 1)
    "As defined by the LDAP RFC.")
  
  (define-constant +ber-class-id+
      '((universal   . #b00000000) (application . #b01000000)
	(context     . #b10000000) (private     . #b11000000)))
  
  (define-constant +ber-p/c-bit+
      '((primitive   . #b00000000) (constructed . #b00100000)))
  
  (define-constant +ber-multibyte-tag-number+ #b00011111
    "Flag indicating tag number requires > 1 byte")
  
  (define-constant +ber-long-length-marker+   #b10000000
    "Flag indicating more tag number bytes follow")
  
  (defun ber-class-id (class)
    "Return the bits to construct a BER tag of type class."
    (or (cdr (assoc class +ber-class-id+))
	(error "Attempted to retrieve a non-existent BER class.")))

  (defun ber-p/c-bit (p/c)
    "Return the bit to construct a BER tag of class primitive or constructed."
    (or (cdr (assoc p/c +ber-p/c-bit+))
	(error "Attempted to retrieve a non-existent p/c bit.")))

  (defun ber-tag-type (class p/c)
    "Construct the bits that kicks off a BER tag byte."
    (+ (ber-class-id class) (ber-p/c-bit p/c)))

  (defun ber-tag (class p/c number-or-command)
    "Construct the list of bytes that constitute a BER tag number 0-127.
CLASS should be the symbol universal, applicaiton, context, or private.
P/C should be the symbol primitive or constructed.
NUMBER should be either an integer or LDAP application name as symbol."
    (let ((byte (ber-tag-type class p/c))
	  (number (etypecase number-or-command 
		    (integer number-or-command)
		    (symbol (ldap-command number-or-command)))))
      (cond 
	((< number 31)  (list (+ byte number)))
	((< number 128) (list (+ byte +ber-multibyte-tag-number+) number))
	(t (error "Length of tag exceeds maximum bounds (0-127).")))))

  (defun ber-length (it)
    "Given a sequence or integer, return a BER length."
    (let ((length (etypecase it
		    (sequence (length it))
		    (integer it))))
      (cond
	((< length 128) (list length))
	((< length +max-int+)
	 (let ((output (base10->base256 length)))
	   (append (list (+ (length output) +ber-long-length-marker+)) 
		   output)))
	(t (error "Length exceeds maximum bounds")))))

  (defun ber-msg (tag data)
    "Given a BER tag and a sequence of data, return a message"
    (let ((len (ber-length data)))
      (append tag len data))))


;;;;
;;;; LDAP constants and accessors
;;;;

(define-constant +ldap-version+     #x03 "LDAP version 3.")
(define-constant +ldap-port-no-ssl+ 389  "Default LDAP Port.")
(define-constant +ldap-port-ssl+    636  "Default LDAPS Port.")

(define-constant +ldap-disconnection-response+ "1.3.6.1.4.1.1466.20036"
  "OID of the unsolicited disconnection reponse.")
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +ldap-application-names+
    '((BindRequest           . 0)
      (BindResponse          . 1)
      (UnbindRequest         . 2)
      (SearchRequest	     . 3)
      (SearchResultEntry     . 4)
      (SearchResultReference . 19)
      (SearchResultDone      . 5)
      (ModifyRequest         . 6)
      (ModifyResponse        . 7)
      (AddRequest            . 8)
      (AddResponse           . 9)
      (DelRequest            . 10)
      (DelResponse           . 11)
      (ModifyDNRequest       . 12)
      (ModifyDNResponse      . 13)
      (CompareRequest        . 14)
      (CompareResponse       . 15)
      (AbandonRequest        . 16)
      (ExtendedRequest       . 23)
      (ExtendedResponse      . 24)))
  
  (defun ldap-command (command)
    "Given a symbol naming an ldap command, return the command number."
    (cdr (assoc command +ldap-application-names+)))
  
  (defun ldap-command-sym (number)
    "Given an application number, return the command name as symbol."
    (car (rassoc number +ldap-application-names+)))
  
  (define-constant +ldap-result-codes+
      '((0  . (success			 "Success"))
	(1  . (operationsError		 "Operations Error"))
	(2  . (protocolError		 "Protocol Error"))
	(3  . (timeLimitExceeded	 "Time Limit Exceeded"))
	(4  . (sizeLimitExceeded	 "Size Limit Exceeded"))
	(5  . (compareFalse		 "Compare False"))
	(6  . (compareTrue		 "Compare True"))
	(7  . (authMethodNotSupported	 "Auth Method Not Supported"))
	(8  . (strongAuthRequired	 "Strong Auth Required"))
	(10 . (referral			 "Referral"))
	(11 . (adminLimitExceeded	 "Admin Limit Exceeded"))
	(12 . (unavailableCriticalExtension "Unavailable Critical Extension"))
	(13 . (confidentialityRequired	 "Confidentiality Required"))
	(14 . (saslBindInProgress	 "SASL Bind In Progress"))
	(16 . (noSuchAttribute		 "No Such Attribute"))
	(17 . (undefinedAttributeType	 "Undefined Attribute Type"))
	(18 . (inappropriateMatching	 "Inappropriate Matching"))
	(19 . (constraintViolation	 "Constraint Violation"))
	(20 . (attributeOrValueExists	 "Attribute Or Value Exists"))
	(21 . (invalidAttributeSyntax	 "Invalid Attribute Syntax"))
	(32 . (noSuchObject		 "No Such Object"))
	(33 . (aliasProblem		 "Alias Problem"))
	(34 . (invalidDNSyntax		 "Invalid DN Syntax"))
	(36 . (aliasDereferencingProblem    "Alias Dereferencing Problem"))
	(48 . (inappropriateAuthentication  "Inappropriate Authentication"))
	(49 . (invalidCredentials	 "Invalid Credentials"))
	(50 . (insufficientAccessRights	 "Insufficient Access Rights"))
	(51 . (busy			 "Busy"))
	(52 . (unavailable		 "Unavailable"))
	(53 . (unwillingToPerform	 "Unwilling To Perform"))
	(54 . (loopDetect		 "Loop Detect"))
	(64 . (namingViolation		 "Naming Violation"))
	(65 . (objectClassViolation	 "Object Class Violation"))
	(66 . (notAllowedOnLeaf		 "Not Allowed On Leaf"))
	(67 . (notAllowedOnRDN		 "Not Allowed On RDN"))
	(68 . (entryAlreadyExists	 "Entry Already Exists"))
	(69 . (objectClassModsProhibited "Object Class Mods Prohibited"))
	(71 . (affectsMultipleDSAs	 "Affects Multiple DSAs"))
	(80 . (other			 "Other"))))

  ; export the result code symbols.
  (dolist (i +ldap-result-codes+) (export (second i) :ldap)))

(defun ldap-result-code-string (code)
  (second (cdr (assoc code +ldap-result-codes+))))

(defun ldap-result-code-symbol (code)
  (first (cdr (assoc code +ldap-result-codes+))))


(define-constant +ldap-scope+ 
  '((base . 0)
    (one  . 1)
    (sub  . 2)))

(define-constant +ldap-deref+
  '((never  . 0)
    (search . 1)
    (find   . 2)
    (always . 3)))

(define-constant +ldap-modify-type+
  '((add . 0)
    (delete . 1)
    (replace . 2)))

(define-constant +ldap-filter-comparison-char+
  '((&  . 0)
    (\| . 1)
    (!  . 2)
    (=  . 3)
    (>= . 5)
    (<= . 6)
    (=* . 7)
    (~= . 8)
    (substring . 4)))

(define-constant +ldap-substring+
  '((initial . 0)
    (any     . 1)
    (final   . 2)))

(defun ldap-scope (&optional (scope 'sub))
  "Given a scope symbol return the enumeration int."
    (cdr (assoc scope +ldap-scope+)))

(defun ldap-deref (&optional (deref 'never))
  "Given a deref symbol return the enumeration int."
  (cdr (assoc deref +ldap-deref+)))

(defun ldap-modify-type (type)
  "Given a modify type, return the enumeration int."
  (cdr (assoc type +ldap-modify-type+)))
	
(defun ldap-filter-comparison-char (comparison-char-as-symbol)
  "Given a comparison character, return its integer enum value."
  (cdr (assoc comparison-char-as-symbol +ldap-filter-comparison-char+)))
    
(defun ldap-substring (type)
  "Given a substring type, return its integer choice value."
  (cdr (assoc type +ldap-substring+)))

(defun attribute-p (attribute)
  "Return T if string/symb is legal as an attribute, nil otherwise."
  (let ((string (etypecase attribute 
		  (string attribute)
		  (symbol (symbol-name attribute)))))
    (let ((bad-char-count 
	   (count-if-not #'(lambda (x) 
			     (or
			      (and (char>= x #\0) (char<= x #\9))
			      (and (char>= x #\A) (char<= x #\Z))
			      (and (char>= x #\a) (char<= x #\z))
			      (char= x #\;)
			      (char= x #\-))) string)))
      (if (= 0 bad-char-count) (intern (string-upcase string)) nil))))

;;;;
;;;; BER sequence creators.
;;;;

;;; writers.
(define-constant +ber-bind-tag+ 
  (ber-tag 'application 'constructed 'bindrequest))
(define-constant +ber-add-tag+  
  (ber-tag 'application 'constructed 'addrequest))
(define-constant +ber-del-tag+  
  (ber-tag 'application 'primitive 'delrequest))
(define-constant +ber-moddn-tag+  
  (ber-tag 'application 'constructed 'modifydnrequest))
(define-constant +ber-comp-tag+ 
  (ber-tag 'application 'constructed 'comparerequest))
(define-constant +ber-search-tag+
  (ber-tag 'application 'constructed 'searchrequest))
(define-constant +ber-abandon-tag+
  (ber-tag 'application 'primitive 'abandonrequest))
(define-constant +ber-unbind-tag+
  (ber-tag 'application 'primitive 'unbindrequest))
(define-constant +ber-modify-tag+
  (ber-tag 'application 'constructed 'modifyrequest))

;;;; readers.
(define-constant +ber-tag-extendedresponse+
    (car (ber-tag 'application 'constructed 'extendedresponse)))
(define-constant +ber-tag-ext-name+  
    (car (ber-tag 'context 'primitive 10)))
(define-constant +ber-tag-ext-val+ 
    (car (ber-tag 'context 'primitive 11)))
(define-constant +ber-tag-bool+ 
    (car (ber-tag 'universal 'primitive #x01)))
(define-constant +ber-tag-int+ 
    (car (ber-tag 'universal 'primitive #x02)))
(define-constant +ber-tag-enum+ 
    (car(ber-tag 'universal 'primitive #x0A)))
(define-constant +ber-tag-str+ 
    (car (ber-tag 'universal 'primitive #x04)))
(define-constant +ber-tag-seq+ 
    (car (ber-tag 'universal 'constructed #x10)))
(define-constant +ber-tag-set+ 
    (car (ber-tag 'universal 'constructed #x11)))

(defun seq-null ()
  "BER encode a NULL"
  (append (ber-tag 'universal 'primitive #x05) (ber-length 0)))

(defun seq-boolean (t/f)
  "BER encode a boolean value."
  (let ((value (cond ((eql t/f t)   #xFF)
		     ((eql t/f nil) #x00)
		     (t (error "Unknown boolean value.")))))
    (nconc (ber-tag 'universal 'primitive #x01) (ber-length 1) (list value))))

(defun seq-integer (int)
  "BER encode an integer value."
  (assert (integerp int))
  (let ((bytes (int->octet-list int)))
    (nconc (ber-tag 'universal 'primitive #x02) (ber-length bytes) bytes)))

(defun seq-enumerated (int)
  "BER encode an enumeration value."
  (assert (integerp int))
  (let ((bytes (int->octet-list int)))
    (nconc (ber-tag 'universal 'primitive #x0A) (ber-length bytes) bytes)))

(defun seq-octet-string (string)
  "BER encode an octet string value."
  (let ((bytes (seq-primitive-string string)))
    (nconc (ber-tag 'universal 'primitive #x04) (ber-length bytes) bytes)))

(defun seq-sequence (tlv-seq)
  "BER encode a sequence of TLVs."
  (assert (or (null tlv-seq) (consp tlv-seq)))
  (nconc (ber-tag 'universal 'constructed #x10) (ber-length tlv-seq) tlv-seq))

(defun seq-set (tlv-set)
  "BER encode a set of TLVs."
  (assert (consp tlv-set))
  (nconc (ber-tag 'universal 'constructed #x11) (ber-length tlv-set) tlv-set))

(defun seq-primitive-choice (int &optional data)
  "BER encode a context-specific choice."
  (assert (integerp int))
  (let ((tag (ber-tag 'context 'primitive int)))
    (etypecase data
      (null (append tag (list #x00)))
      (string  (if (string= data "") 
		   (append tag (list #x00))
		   (append tag (ber-length data) 
			   (string->char-code-list data))))
      (integer (seq-integer data))
      (boolean (seq-boolean data))
      (symbol  (let ((str (symbol-name data)))
		 (append tag (ber-length str) 
			 (string->char-code-list str)))))))

(defun seq-constructed-choice (int &optional data)
  "BER encode a context-specific, constructed choice."
  (assert (integerp int))
  (let ((tag (ber-tag 'context 'constructed int)))
    (etypecase data
      (string (let* ((val (seq-octet-string data))
		     (len (ber-length val)))
		(append tag len val)))
      (sequence (let ((len (ber-length data)))
		  (append tag len data))))))
		     
(defun seq-primitive-string (string)
  "BER encode a string/symbol for use in a primitive context."
  (assert (or (stringp string) (symbolp string)))
  (string->char-code-list string))

(defun seq-attribute-alist (atts)
  "BER encode an entry object's attribute alist (for use in add)."
  (seq-sequence (mapcan #'(lambda (i) 
			    (seq-att-and-values (car i) (cdr i))) atts)))
    
(defun seq-attribute-list (att-list)
  "BER encode a list of attributes (for use in search)."
  (seq-sequence (mapcan #'seq-octet-string att-list)))

(defun seq-attribute-assertion (att val)
  "BER encode an ldap attribute assertion (for use in compare)."
  (seq-sequence (nconc (seq-octet-string att) (seq-octet-string val))))
  
(defun seq-attribute-value-assertion (att val)
  "BER encode an ldap attribute value assertion (for use in filters)."
  (nconc (seq-octet-string att) (seq-octet-string val)))

(defun seq-att-and-values (att vals)
  "BER encode an attribute and set of values (for use in modify)."
  (unless (listp vals) (setf vals (list vals)))
  (seq-sequence (nconc (seq-octet-string att) 
		       (seq-set (mapcan #'seq-octet-string vals)))))

(defun seq-filter (filter)
  (let* ((filter (etypecase filter
		   (cons   filter)
		   (symbol filter)
		   (string (filter-string->sexp filter))))
	 (op  (car filter))
	 (sub (if (and (eq '= op)
		       (> (count #\* (symbol-name (third filter))) 0)) t nil)))
    (cond
      ((eq '! op) (seq-constructed-choice (ldap-filter-comparison-char op)
					  (seq-filter (cadr filter))))
      ((or (eq '&  op) (eq '\| op))
       (seq-constructed-choice (ldap-filter-comparison-char op)
			       (mapcan #'seq-filter (cdr filter))))
      ((or (and (eq '= op) (not sub))
	   (eq '<= op) (eq '>= op) (eq '~= op))
       (seq-constructed-choice (ldap-filter-comparison-char op) 
			       (seq-attribute-value-assertion
				(second filter) (third filter))))
      ((eq '=* op) (seq-primitive-choice 
		    (ldap-filter-comparison-char op) (second filter)))
      ((and (eq '= op) sub)
       (seq-constructed-choice (ldap-filter-comparison-char 'substring)
			       (append (seq-octet-string (second filter))
				       (seq-substrings (third filter)))))
      (t (error 'ldap-filter-error 
		:mesg "unable to determine operator." :filter filter)))))


(defun seq-substrings (value)
  "Given a search value with *s in it, return a BER encoded list."
  (let ((list (etypecase value 
		  (symbol (split-substring (symbol-name value)))
		  (string (split-substring value))))
	(initial ()) (any ()) (final ()))
    (when (string/= "*" (car list))   ; initial
      (setf initial (seq-primitive-choice (ldap-substring 'initial)
					  (car list))))
    (setf list (cdr list))            ; last
    (when (and (> (length list) 0) (string/= "*" (car (last list))))
      (setf final (seq-primitive-choice (ldap-substring 'final)
					(car (last list)))))
    (setf list (butlast list))
    (when (> (length list) 0)         ; any
      (dolist (i (remove "*" list :test #'string=))
	(setf any (append any (seq-primitive-choice 
			       (ldap-substring 'any) i)))))
    (seq-sequence (nconc initial any final))))

(defun valid-ldap-response-p (tag-byte)
  "Return T if this is the valid initial tag byte for an LDAP response."
  (if (= tag-byte (car (ber-tag 'universal 'constructed #x10))) t nil))


;;;;
;;;; referrer class & methods.
;;;;

(defclass referrer ()
  ((url :initarg :url 
	:initform (error "No URL specified")
	:type string
	:accessor url)))

(defun new-referrer (url)
  "Instantiate a new referrer object."
  (make-instance 'referrer :url url))

;;;;
;;;; entry class & methods.
;;;;

(defclass entry ()
  ((dn    :initarg :dn     :type string  :accessor dn)
   (rdn   :initarg :rdn    :type string  :accessor rdn)
   (attrs :initarg :attrs  :type cons    :accessor attrs)))

(defmethod dn ((dn string)) dn)

(defun rdn-from-dn (dn)
  "Given a DN, return its RDN and a cons of (att . val)"
  (let* ((eql-pos (position #\= dn))
	 (rdn (subseq dn 0 (position #\, dn)))
	 (rdn-att (subseq rdn 0 eql-pos))
	 (rdn-val (subseq rdn (1+ eql-pos) (length rdn))))
    (values rdn (list (intern (string-upcase rdn-att)) rdn-val))))

(defun new-entry (dn &key (attrs ()) (infer-rdn t))
  "Instantiate a new entry object."
  (multiple-value-bind (rdn rdn-list) (rdn-from-dn dn)
   (when (and infer-rdn
	      (not (assoc (car rdn-list) attrs)))
     (setf attrs (acons (car rdn-list) (cdr rdn-list) attrs)))
   (make-instance 'entry :dn dn :rdn rdn :attrs attrs)))

(defmethod change-rdn ((entry entry) new-rdn)
  "Change the DN and RDN of the specified object, don't touch LDAP."
  (let* ((len-old (length (rdn entry)))
	 (dn (concatenate 'string new-rdn (subseq (dn entry) len-old))))
    (multiple-value-bind (old-rdn old-rdn-parts) (rdn-from-dn (dn entry))
      (declare (ignore old-rdn))
      (del-attr entry (first old-rdn-parts) (second old-rdn-parts)))
    (setf (dn entry) dn  
	  (rdn entry) new-rdn)
    (multiple-value-bind (new-rdn new-rdn-parts) (rdn-from-dn (dn entry))
      (declare (ignore new-rdn))
      (add-attr entry (first new-rdn-parts) (second new-rdn-parts)))))

(defmethod attr-value ((entry entry) attr)
  "Given an entry object and attr name (symbol), return list of values."
  (let ((val (cdr (assoc attr (attrs entry)))))
    (cond 
      ((null val) nil)
      ((consp val) val)
      (t (list val)))))

(defmethod attr-value ((entry entry) (attrs list))
  "Given an entry object and list of attr names (as symbols), 
return list of lists of attributes."
  (mapcar #'(lambda (attr) (attr-value entry attr)) attrs))

(defmethod attr-list ((entry entry))
  "Given an entry object, return a list of its attributes."
  (map 'list #'car (attrs entry)))

(defmethod add-attr ((entry entry) attr vals)
  "Add an attribute to entry object, do not update LDAP."
  (let ((old-val-list (attr-value entry attr))
	(new-val-list (if (consp vals) vals (list vals))))
    (replace-attr entry attr (append old-val-list new-val-list))))

(defmethod del-attr ((entry entry) attr &optional vals)
  "Delete an attribute from entry object, do not update LDAP"
  (let ((old-val (attr-value entry attr))
	(new-val (if (consp vals) vals (list vals))))
    (dolist (val new-val)
      (setf old-val (remove-if #'(lambda (x) (string= val x)) old-val)))
    (if (or (null (car old-val))
	    (null (car new-val)))
	(setf (attrs entry) 
	      (remove-if #'(lambda (x) (eq (car x) attr)) (attrs entry)))
	(replace-attr entry attr old-val))))
	      
(defmethod replace-attr ((entry entry) attr vals)
  "Replace attribute values from entry object, do not update LDAP"
  (let ((vals (remove-if #'null vals)))
    (if (consp (assoc attr (attrs entry)))
	(rplacd (assoc attr (attrs entry)) vals)
	(setf (attrs entry) (acons attr vals (attrs entry))))))

(defmethod ldif ((entry entry))
  "Return an ldif formatted representation of entry."
  (let ((results (format nil "DN: ~A~%" (dn entry))))
    (dolist (att (attr-list entry) results)
      (dolist (val (attr-value entry att))
	(setf results (format nil "~@[~A~]~A: ~A~%" results att val))))))

(defun new-entry-from-list (list)
  "Create an entry object from the list return by search."
  (let ((dn (car list))
	(attrs (mapcar #'(lambda (x) (cons (intern (string-upcase (car x)))
					   (cadr x)))
		       (cadr list))))
    (new-entry dn :attrs attrs)))

;;;;
;;;; LDAP class & methods
;;;;

(defclass ldap () 
  ((host   :initarg :host 
	   :initform "localhost"
	   :type string 
	   :accessor host)
   (port   :initarg :port 
	   :initform +ldap-port-no-ssl+
	   :type integer 
	   :accessor port)
   (sslflag :initarg :sslflag
	    :initform nil
	    :type symbol
	    :accessor sslflag)
   (user   :initarg :user 
	   :initform ""
	   :type string 
	   :accessor user)
   (pass   :initarg :pass 
	   :initform ""
	   :type string 
	   :accessor pass)
   (ldapstream :initarg :ldapstream  
	   :initform nil 
	   :type (or null stream) 
	   :accessor ldapstream)
   (ldapsock :initarg :ldapsock
	   :initform nil 
	   :accessor ldapsock)
   (reuse-connection :initarg :reuse-connection
		     :initform t
		     :type symbol
		     :documentation "nil, t, or bind"
		     :accessor reuse-connection)
   (mesg   :initarg :mesg 
	   :initform 0 
	   :type integer 
	   :accessor mesg)
   (debugflag  :initarg :debugflag
	       :initform nil 
	       :type symbol 
	       :accessor debugflag)
   (base   :initarg :base 
	   :initform nil 
	   :type (or null string) 
	   :accessor base)
   (response :initarg :response
	     :initform ()
	     :type list
	     :accessor response)
   (entry-buffer :initarg :entry-buffer
		 :initform nil
		 :accessor entry-buffer)
   (results-pending-p :initarg :results-pending-p
		      :initform nil
		      :type (boolean)
		      :accessor results-pending-p)))
   

(defun new-ldap (&key (host "localhost") (sslflag nil)
		 (port (if sslflag +ldap-port-ssl+ +ldap-port-no-ssl+))
		 (user "") (pass "") (base nil) (debug nil)
		 (reuse-connection nil))
  "Instantiate a new ldap object."
  (make-instance 'ldap :host host :port port :user user :sslflag sslflag
		 :pass pass :debugflag debug :base base 
		 :reuse-connection reuse-connection))

(defmethod debug-mesg ((ldap ldap)  message)
  "If debugging in T, print a message."
  (when (debugflag ldap) (format *debug-io* "~A~%" message)))

(defmethod mesg-incf ((ldap ldap)) (incf (mesg ldap)))

(defmethod get-stream ((ldap ldap))
  "Open a usocket to the ldap server and set the ldap object's slot.
If the port number is 636 or the SSLflag is not null, the stream
will be made with CL+SSL."
  (let ((existing-stream (ldapstream ldap)))
    (unless (and (streamp existing-stream) 
		 (open-stream-p existing-stream))
      (let* ((sock (usocket:socket-connect (host ldap) (port ldap)
					   :element-type '(unsigned-byte 8)))
	     (stream 
	      (if (or (sslflag ldap) (= (port ldap) 636))
		  (cl+ssl:make-ssl-client-stream (usocket:socket-stream sock))
		  (usocket:socket-stream sock))))
	(debug-mesg ldap "Opening socket and stream.")
	(setf (ldapsock ldap) sock)
	(setf (ldapstream ldap) stream))))
    (ldapstream ldap))

(defmethod close-stream ((ldap ldap))
  "Close an ldap connection if it is currently open."
  (let ((existing-stream (ldapstream ldap))
	(existing-sock (ldapsock ldap)))
    (when (and (streamp existing-stream) (open-stream-p existing-stream))
      (ignore-errors
	(setf (ldapstream ldap) nil)
	(setf (ldapsock ldap) nil)
	(close existing-stream)
	(usocket:socket-close existing-sock)))))

(defmethod possibly-reopen-and-rebind ((ldap ldap) 
				       &optional (absolutely-no-bind nil))
  "Take appropriate reopen or rebind actions based on the reuse-connection attr.
If the attribute is nil, do nothing; if t, reopen; and, if bind, rebind.
This function exists to help the poor saps (read: me) with very fast idletimeout
settings on their LDAP servers."
  (debug-mesg ldap "reusing connection...")
  (let (stream)
    (when (reuse-connection ldap) 
      (close-stream ldap)
      (setf stream (get-stream ldap)))
    (when (and (not absolutely-no-bind)
	       (eq (reuse-connection ldap) 'rebind))
      (debug-mesg ldap "rebinding...")
      (bind ldap))
    stream))
  
(defmethod send-message ((ldap ldap) message &optional (response-expected t))
  "Send a BER encoded message to ldap."
  (let ((mesg (seq-sequence (append (seq-integer (mesg-incf ldap)) message)))
	(stream (get-stream ldap)))
    (debug-mesg ldap (format nil *hex-print* "To LDAP: " mesg))
    (dolist (byte mesg) (write-byte byte stream))
    (handler-case (finish-output stream)
      (error (e) (error 'ldap-connection-error
			:host (host ldap) :port (port ldap) :mesg e)))
    (when response-expected (setf (results-pending-p ldap) t))))

(defmethod receive-length ((ldap ldap))
  "Read length of LDAP message from stream, return length & the bytes read."
  (let* ((stream (ldapstream ldap))
	 (length-byte (read-byte stream))
	 (byte-seq ())
	 (byte-len (- length-byte 128))
	 (length-of-message
	  (cond
	    ((< length-byte 128) length-byte)
	    (t (dotimes (i byte-len) (push (read-byte stream) byte-seq))
	       (base256->base10 (reverse byte-seq)))))
	 (all-bytes-consumed (append (list length-byte) (nreverse byte-seq))))
    (values length-of-message all-bytes-consumed)))

(defmethod receive-message ((ldap ldap))
  "Read incoming LDAP data from the stream, populate LDAP response slot.
The initial tag and length of message bytes will have been consumed already
and will not appear in the response.  Note that this method is executed
only for its side effects."
  (let* (ber-response 
	 (stream (get-stream ldap))
	 (initial-byte (read-byte stream)))
    (unless (or (null initial-byte) (valid-ldap-response-p initial-byte))
      (error "Received unparsable data from LDAP server."))
    (multiple-value-bind (message-length bytes-read) (receive-length ldap)
      (dotimes (i message-length) (push (read-byte stream) ber-response))
      (setf (response ldap) (nreverse ber-response))
      (debug-mesg ldap (format nil *hex-print* "From LDAP:"
			       (append (list initial-byte) bytes-read 
				       (response ldap)))))
    (let ((response-minus-message-number 
	   (check-message-number (response ldap) (mesg ldap))))
      (cond
	((null response-minus-message-number) (receive-message ldap))
	(t (setf (response ldap) response-minus-message-number))))))
  
(defmethod handle-extended-response ((ldap ldap) content)
  "Process an extended response.
Currently this means closing the connection if it is a disconnect request
and throw an error if it's anything else."
  (if (string= (fourth content) +ldap-disconnection-response+)
      (close-stream ldap)
      (error 'ldap-error 
	     :mesg (format nil "Received unhandled extended response: ~A~%"
			   content))))

(defmethod parse-ldap-message ((ldap ldap) &optional (return-entry nil))
  "Parse an ldap object's response slot."
  (let ((received-content ()))
    (multiple-value-bind (content appname) (read-decoder (response ldap))
      (cond
	((eq appname 'searchresultentry)
	 (let ((new-entry (new-entry-from-list content)))
	   (cond
	     ((null return-entry)
	      (setf (entry-buffer ldap) new-entry)
	      (setf received-content t))
	     (t (setf received-content new-entry)))))
	((eq appname 'searchresultreference))
	((eq appname 'searchresultdone)
	 (setf (results-pending-p ldap) nil)
	 (setf received-content nil))
	((eq appname 'extendedresponse) 
	 (handle-extended-response ldap content)
	 (push content received-content)
	 (setf (results-pending-p ldap) nil))
	(t 
	 (push content received-content)
	 (setf (results-pending-p ldap) nil))))
    received-content))
	
(defmethod process-message ((ldap ldap) message &key (success 'success))
  "Send a simple request to LDAP and return three values:
T or NIL, the LDAP response code (as a readable string), and any message
the directory server returned."
  (let ((bind-p (equal (msg-bind ldap) message)))
    (possibly-reopen-and-rebind ldap bind-p))
  (send-message ldap message)
  (receive-message ldap)
  (let* ((results (car (parse-ldap-message ldap)))
	 (code (car results))
	 (msg (third results))
	 (code-sym (ldap-result-code-symbol code))
	 (rc (if (eq code-sym success) t nil)))
    (values rc code-sym msg)))

;;;;  
;;;; ldap user-level commands.
;;;;

(defmethod bind ((ldap ldap))
  "Send a BindRequest."
  (process-message ldap (msg-bind ldap)))

(defmethod unbind ((ldap ldap))
  "Unbind and close the ldap stream."
  (send-message ldap (msg-unbind) nil)
  (setf (mesg ldap) 0)
  (close-stream ldap))

(defmethod abandon ((ldap ldap))
  "Abandon the request and suck any data off the incoming stream.
Because the receive-message will keep receiving messages until it gets
one with the correct message number, no action needs to be taken here to 
clear the incoming data off the line.  It's unclear that's the best 
solution, but (clear-input) doesn't actually work and trying to read non-
existent bytes blocks..."
  (send-message ldap (msg-abandon ldap) nil))


(defmethod add ((ldap ldap) (entry entry))
  "Add an entry to the directory."
  (process-message ldap (msg-add entry)))

(defmethod add ((entry entry) (ldap ldap))
  "Add an entry object to LDAP; error unless successful."
  (multiple-value-bind (res code msg) (add ldap entry)
    (or res (error 'ldap-response-error 
		   :mesg "Cannot add entry to LDAP directory."
		   :dn (dn entry) :code code :msg msg))))

(defmethod delete ((ldap ldap) dn-or-entry)
  "Delete an entry (or DN) from the directory."
  (process-message ldap (msg-delete dn-or-entry)))

(defmethod delete ((entry entry) (ldap ldap))
  "Delete an entry object from ldap; error unless successful."
  (delete (dn entry) ldap))

(defmethod delete ((dn string) (ldap ldap))
  "Delete an entry from LDAP; error unless successful."
  (multiple-value-bind (res code msg) (delete ldap dn)
    (or res (error 'ldap-response-error
		   :mesg "Cannot delete entry from LDAP directory."
		   :dn dn :code code :msg msg))))

(defmethod moddn ((ldap ldap) dn-or-entry new-rdn &key delete-old new-sup)
  "Modify an entry's RDN."
  (process-message ldap (msg-moddn dn-or-entry new-rdn delete-old new-sup)))

(defmethod moddn ((entry entry) (ldap ldap) new-rdn &key delete-old new-sup)
  "Modify the RDN of an LDAP entry; update the entry object as well."
  (when (moddn (dn entry) ldap new-rdn :delete-old delete-old :new-sup new-sup)
    (change-rdn entry new-rdn)))

(defmethod moddn ((dn string) (ldap ldap) new-rdn &key delete-old new-sup)
  "Modify the RDN of an LDAP entry."
  (multiple-value-bind (res code msg)
      (moddn ldap dn new-rdn :delete-old delete-old :new-sup new-sup)
    (or res (error 'ldap-response-error 
		   :mesg "Cannot modify RDN in the LDAP directory."
		   :dn dn :code code :msg msg))))

(defmethod compare ((ldap ldap) dn-or-entry attribute value)
  "Assert DN has attribute with specified value."
  (process-message ldap (msg-compare dn-or-entry attribute value)
		   :success 'comparetrue))

(defmethod compare ((entry entry) (ldap ldap) attribute value)
  "Assert an entry has an att=val; return t or nil, or throw error."
  (compare (dn entry) ldap attribute value))

(defmethod compare ((dn string) (ldap ldap) attribute value)
  "Compare entry's att/val; calle by both entry/compare methods."
  (multiple-value-bind (res code msg) (compare ldap dn attribute value)
    (declare (ignore res))
    (cond ((eq code 'comparetrue) t)
	  ((eq code 'comparefalse) nil)
	  (t (error 'ldap-response-error
		    :mesg "Cannot compare entry's attribute/value."
		    :dn dn :code code :msg msg)))))

(defmethod modify ((ldap ldap) dn-or-entry list-of-mods)
  "Modify and entry's attributes."
  (process-message ldap (msg-modify dn-or-entry list-of-mods)))

(defmethod modify ((entry entry) (ldap ldap) list-of-mods)
  "Modify entry attributes in ldap, update the entry object.
LIST-OF-MODS is a list of (type att val) triples."
  (multiple-value-bind (res code msg) (modify ldap entry list-of-mods)
    (when (null res) 
      (error 'ldap-response-error
	     :mesg "Cannot modify entry in the LDAP directory."
	     :dn (dn entry) :code code :msg msg))
    ; succeeded, so modify the entry.
    (dolist (i list-of-mods t)
      (cond
	((eq (car i) 'delete) (del-attr entry (second i) (third i)))
	((eq (car i) 'add) (add-attr entry (second i) (third i)))
	(t (replace-attr entry (second i) (third i)))))))

(defmethod search ((ldap ldap) filter &key base (scope 'sub) 
		   (deref 'never) (size-limit 0) (time-limit 0) 
		   types-only attributes)
  "Search the LDAP directory."
  (let ((base (if (null base) (base ldap) base))
	(scope (ldap-scope scope))
	(deref (ldap-deref deref)))
    (possibly-reopen-and-rebind ldap)
    (send-message ldap (msg-search filter base scope deref size-limit 
				   time-limit types-only attributes))
    (receive-message ldap)
    (parse-ldap-message ldap)))

(defmethod next-search-result ((ldap ldap))
  "Return the next search result (as entry obj) or NIL if none."
  (if (results-pending-p ldap)
      (let ((pending-entry (entry-buffer ldap)))
	(cond 
	  ((not (null pending-entry))
	   (setf (entry-buffer ldap) nil)
	   pending-entry)
	  (t (receive-message ldap)
	     (parse-ldap-message ldap t))))
      nil))

(defmacro dosearch ((var search-form) &body body)
  (let ((ldap (gensym))
 	(count (gensym)))
    `(let ((,ldap ,(second search-form))
 	   (,count 0))
      ,search-form
      (do ((,var (next-search-result ,ldap) 
 		 (next-search-result ,ldap)))
 	  ((null ,var))
 	(incf ,count)
 	,@body)
      ,count)))

(defmacro ldif-search (&rest search-parameters)
  (let ((ent (gensym)))
    `(dosearch (,ent (search ,@search-parameters))
      (format t "~A~%" (ldif ,ent)))))

;;;;
;;;; ldap message constructors.
;;;;

(defmethod msg-bind ((ldap ldap))
  "Return the sequence of bytes representing a bind message."
  (let ((req (append (seq-integer +ldap-version+)
		     (seq-octet-string (user ldap))
		     (seq-primitive-choice 0 (pass ldap)))))
    (ber-msg +ber-bind-tag+ req)))

(defmethod msg-unbind ()
  (ber-msg +ber-unbind-tag+ (seq-null)))

(defmethod msg-abandon ((ldap ldap))
  "Return the sequence of bytes representing an abandon message"
  (let ((last-message-number (seq-integer (mesg ldap))))
    (ber-msg +ber-abandon-tag+ last-message-number)))

(defmethod msg-add ((entry entry))
  "Return the sequence of bytes representing an add message."
  (let ((dn (seq-octet-string (dn entry)))
	(att (seq-attribute-alist (attrs entry))))
    (ber-msg +ber-add-tag+ (append dn att))))

(defun msg-delete (dn-or-entry)
  "Return the sequence of bytes representing a delete message."
  (let ((dn (seq-primitive-string (dn dn-or-entry))))
    (ber-msg +ber-del-tag+ dn)))
	
(defun msg-moddn (dn-or-entry new-rdn delete-old new-sup)
  "Return the sequence of bytes representing a moddn message."
  (let ((dn  (seq-octet-string (dn dn-or-entry)))
	(rdn (seq-octet-string new-rdn))
	(del (seq-boolean delete-old))
	(new-sup (if new-sup (seq-octet-string new-sup) nil)))
    (ber-msg +ber-moddn-tag+ (append dn rdn del new-sup))))

(defun msg-compare (dn-or-entry attribute value)
  "Return the sequence of bytes representing a compare message."
  (let ((dn (seq-octet-string (dn dn-or-entry)))
	(assertion (seq-attribute-assertion attribute value)))
    (ber-msg +ber-comp-tag+ (append dn assertion))))

(defun msg-modify (dn-or-entry mod-list)
  "Return the sequence of bytes representing a modify message."
  (let ((dn (seq-octet-string (dn dn-or-entry)))
	(mods 
	 (mapcan #'(lambda (x) (seq-sequence 
				(nconc
				 (seq-enumerated (ldap-modify-type (first x)))
				 (seq-att-and-values (second x) (third x)))))
		 mod-list)))
    (ber-msg +ber-modify-tag+ (append dn (seq-sequence mods)))))

(defun msg-search (filter base scope deref size time types attrs)
  "Return the sequence of bytes representing a search message."
  (let ((filter (seq-filter filter))
	(base   (seq-octet-string base))
	(scope  (seq-enumerated scope))
	(deref  (seq-enumerated deref))
	(size   (seq-integer size))
	(time   (seq-integer time))
	(types  (seq-boolean types))
	(attrs  (seq-attribute-list attrs)))
    (ber-msg +ber-search-tag+ 
	     (append base scope deref size time types filter attrs))))

;;;;
;;;; sequence reader & decoder functions
;;;;

(defun read-decoder (response)
  "Decode a BER encoded response (minus initial byte & length) from LDAP."
  (let ((appname (ldap-command-sym (read-app-number (pop response)))))
    (multiple-value-bind (size bytes) (read-length response)
      (declare (ignore size)) 
      (setf response (subseq response bytes)))
    (values (read-generic response) appname)))

(defun read-generic (message &optional (res ()))
  (if (and (consp message) (> (length message) 0))
      (progn
	(let* ((tag-byte (car message))
	       (fn (cond
		     ((= tag-byte +ber-tag-int+)  #'read-integer)
		     ((= tag-byte +ber-tag-enum+) #'read-integer)
		     ((= tag-byte +ber-tag-str+)  #'read-string)
		     ((= tag-byte +ber-tag-ext-name+) #'read-string)
		     ((= tag-byte +ber-tag-ext-val+)  #'read-string)
		     (t nil))))
	  (cond 
	    ((functionp fn)                                   ; primitive.
	     (multiple-value-bind (val bytes) (funcall fn message)
	       (push val res)
	       (setf message (subseq message bytes))))
	    ((or (= tag-byte +ber-tag-set+)                   ; constructed.
		 (= tag-byte +ber-tag-seq+)
		 (= tag-byte +ber-tag-extendedresponse+))
	     (multiple-value-bind (length bytes) 
		 (read-length (subseq message 1))
	       (let* ((start-of-data (+ 1 bytes)) ; tag + bytes
		      (end-of-data   (+ start-of-data length)))
		 (push (read-generic 
			(subseq message start-of-data end-of-data)) res)
		 (setf message (subseq message end-of-data)))))
	    (t (error 'ldap-error :mesg "Unreadable tag value encountered.")))
	  (read-generic message res)))
      (nreverse res)))

(define-constant +ber-app-const-base+
  (car (ber-tag 'application 'constructed 0)))

(defun read-app-number (tag)
  "Given an application tag, return which ldap app number it represents."
  (- (etypecase tag
       (integer tag)
       (cons (car tag))) +ber-app-const-base+))

(defun read-integer (message)
  "Read an int from the message, return int and number of bytes consumed."
  (values (octet-list->int (subseq message 2 (+ 2 (second message))))
	  (+ 2 (second message))))

(defun read-string (message)
  "Read a string from the message, return string and bytes consumed.."
  (pop message) ; lose the tag.
  (multiple-value-bind (len bytes) (read-length message)
    (values (char-code-list->string 
	     (subseq message bytes (+ len bytes))) (+ 1 bytes len))))

(defun read-length (message)
  "Given message starting with length marker, return length and bytes consumed"
  (cond
    ((< (car message) 128) (values (car message) 1))
    (t (let ((bytes (+ 1 (- (car message) 128))))
	 (values (base256->base10 (subseq message 1 bytes)) bytes)))))

(defun read-message-number (response expected-mesg-number)
  "Read message number from the seq, return t or nil and bytes consumed."
  (multiple-value-bind (value bytes) (read-integer response)
    (let ((result (if (or (= value 0) ; 0 is unsolicited notification.
			  (= value expected-mesg-number))
		      t ; msg number matches up
		      nil)))
      (values result bytes))))

(defun check-message-number (response expected-mesg-number)
  "Determine if the  message number of a BER response is correct.
Returns BER response with message number bytes consumed if it is correct
or NIL otherwise."
  (multiple-value-bind (mesg-ok? bytes)
      (read-message-number response expected-mesg-number)
    (if mesg-ok? (subseq response bytes) nil)))

;;; trivial-ldap.lisp ends here.
