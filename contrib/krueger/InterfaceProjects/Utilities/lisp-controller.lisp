;; lisp-controller.lisp
#|
The MIT license.

Copyright (c) 2010 Paul L. Krueger

Permission is hereby granted, free of charge, to any person obtaining a copy of this software 
and associated documentation files (the "Software"), to deal in the Software without restriction, 
including without limitation the rights to use, copy, modify, merge, publish, distribute, 
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is 
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial 
portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT 
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

|#

;; Implements a class that can be used within IB as a content object
;; for various standard view types

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :ns-string-utils)
  (require :ns-object-utils)
  (require :nslog-utils)
  (require :assoc-array)
  (require :list-utils))

(defpackage :lisp-controller
  (:nicknames :lc)
  (:use :ccl :common-lisp :iu)
  (:export 
   added-func
   add-child-func
   children-func
   content-class
   count-func
   delete-func
   edited-func
   gen-root
   lisp-controller 
   lisp-controller-changed
   objects
   reader-func
   removed-func
   root
   root-type
   select-func
   writer-func))

(in-package :lc)

;; lisp-controller functionality
;; Class and methods used to view a lisp object using a NSTableView
;; or NSOutlineView or NSBrowser.
;; The objects slot should initialized as an object to be represented
;; in an associated view table. Unless user-specified reader/writer
;; functions are also specified (as described later), that object 
;; must be a sequence of objects (e.g. a list or vector) or a
;; hast-table which is treated as a sequence of (key . value) pairs.
;; For use with non-hierarchical display views (NSTableView)
;; the objects will be displayed one per row in the table.
;; For use with hierarchical display views (NSOutlineView or 
;; NSBrowser) the objects comprise the set of root objects; each
;; of which may contain children that will be displayed. Children
;; may themselves have children, etc.
;; Objects can be an arbitrary lisp object (e.g. a list,
;; vector, structure, hash-table, or class instance).

;; In the View specification, table column identifiers can be
;; set to provide information about how to access the data to be
;; displayed in that column from the object that represents a
;; given row. 

;; The user has three options for how to convert column identifiers
;; into accessor functions (reader and also writer if the table is
;; editable). The choice is determined by what is found in the column
;; identifier at initialization time and by whether reader/writer
;; functions have been specified.

;; Option 1: Use the column identifier as an index
;; If the column identifier is a number, then it is treated as
;; a numerical index that will be used to access the row-object as
;; a sequence (i.e. as a list or vector)
;; with elements that correspond to numerical values set as
;; column identifiers in IB. For example, a table-column with
;; identifier set to "1" would access the second element of
;; the row. This is the equivalent of:
;;   (elt row-object identifier-value)
;; Also the writer function for such a table identifier
;; is assumed to result in the equivalent of:
;;   (setf (elt row-object identifier-value) new-value)
;; where as before the identifier-value is derived from the
;; identifier-string which is a number.

;; Option 2: Use the column identifier as a key specifier
;; If the column identifier consists of a single symbol,
;; it is treated as a key to use in accessing a value from
;; the row-object. This is the equivalent of:
;;   (funcall (symbol-function identifier-value) row-object)
;; The identifier value should not BE a key (e.g. #'key-function)
;; but rather should be the symbol-name for which symbol-function
;; exists (e.g. key-function).
;; The identifier-value may contain package specifiers for the
;; symbol in the normal way (e.g. my-package::my-symbol).
;; If that column permits editing, then the lisp-controller
;; will execute the equivalent of:
;;  (setf (identifier-value row-object) new-value)
;; So, for example, if the row-object happens to be a list, 
;; the user could specify a key of "first" (quotes not included),
;; since both (first row-object) and (setf (first row-object) new-value)
;; are well-specified for lists. Note that this would, however, be 
;; equivalent to using 0.
;; If editing of a column is permitted, but no appropriate setf
;; method exists for the key specifier, then although the user
;; will apparently be able to change the value of that column in
;; the table, no actual change will be made and the value will 
;; instantly revert back to its former value.
;; Note that a key specifier of "identity" could be used to select
;; the row-object itself for printing. This might be suitable for 
;; single-column tables or multi-column tables where other columns
;; display some transformation of the row-object.

;; Option 3: Use the column-identifier as a symbol or number supplied
;; as a parameter to a separately provided accessor function. 
;; If the user explicitly provides a reader function by setting the
;; reader-function field for a lisp-controller in IB, then
;; that function will be used for all table accesses. It will be passed
;; three arguments: the content of the "objects" slot for the lisp-controller,
;; the requested row number and the value of the column-identifier
;; field for the column. If that is a number, then a numerical
;; value is passed, if it is anything else then whatever lisp object
;; results from doing a (read-from-string column-identifier) is passed
;; as the second parameter. For example, this would permit the user
;; to provide a two-dimensional array as the "objects" to be displayed,
;; provide appropriate numerical indices as the value of various column-
;; identifiers, and to specify "aref" as the reader function.
;; Similarly, if a writer function is specified by the user in IB, then
;; it will be called with four arguments: the new value, the content of
;; the objects slot, the row-number, and the column-identifier as either
;; a number or symbol. Using the same example, this would result in
;; calling (setf (aref contents row-num col-identifier) new-value).
;; Note that if a reader function is specified, then a count function
;; should also be provided. If editing is permitted, then an "edited" 
;; function should also be provided. If row insertion and/or deletion are
;; permitted, then appropriate "added" and "deleted" functions
;; must also be provided.

;; In addition to reader and writer functions, the user may specify
;; functions that are called when a table entry is selected or edited.
;; These should be functions that take 3 arguments: the content of 
;; the lisp-controller's "objects" slot, the row number, 
;; and the column-identifier-string for the affected table entry. 
;; The select function will be invoked whenever a list item is 
;; selected and the edited function will be invoked after an item
;; in the list has been modified.

;; The user may also specify functions to be called when a new row is
;; added or deleted. These will take two arguments, the content of the
;; "objects" slot and the row number of the new object. Note that if 
;; the user has specified their own reader and writer functions that 
;; the lisp-controller will have no way of adding or deleting objects.
;; In this case only, the user supplied "added" and "deleted" functions
;; should actually make the necessary changes to the first argument (i.e.
;; add or delete as appropriate). The table will be re-displayed after
;; this has been done.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global variables

(defvar *lisp-controller-debug* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class and methods specific to hash-tables

(defclass ht-entry ()
  ((ht-key :reader ht-key :initarg :key)
   (ht-value :reader ht-value :initarg :value)
   (ht :accessor ht :initarg :ht))
  (:default-initargs
    :key (gensym "KEY")
    :value nil
    :ht nil))

(defmethod (setf ht-key) (new-key (self ht-entry))
  (block set-it
    (let ((new-key-exists (not (eq :not-found (gethash new-key (ht self) :not-found)))))
      (when new-key-exists
        ;; They are redefining a key to be one that already exists in the hash-table
        ;; first verify they want to do this
        (let* ((alert-str  (lisp-to-temp-nsstring 
                            (format nil 
                                    "Continuing will reset value for existing key: ~s"
                                    new-key)))
               (res (#_NSRunAlertPanel #@"ALERT" 
                                       alert-str
                                       #@"Cancel key change"  
                                       #@"Continue and change key"
                                       (%null-ptr))))
          (unless (eql res #$NSAlertAlternateReturn)
            ;; they don't want to continue
            (return-from set-it))))
      ;; change the value for the existing key
      (setf (gethash new-key (ht self))
            (gethash (ht-key self) (ht self)))
      ;; and then remove the old key that was changed both from the hash table
      ;; and from the list of keys
      ;; new keys are always put at the end of the list unless a sort predicate
      ;; has been specified.
      (remhash (ht-key self) (ht self))
      (setf (slot-value self 'ht-key) new-key))))

(defmethod (setf ht-value) (new-val (self ht-entry))
  (setf (gethash (ht-key self) (ht self)) new-val)
  (setf (slot-value self 'ht-value) new-val))

(let ((ht-hash (make-hash-table)))
  ;; in order to treat hash-tables as containers like lists and vectors we
  ;; need to define a few functions which use a cache of the "children" of
  ;; a hash-table so that we don't need to recreate the whole list every time
  ;; a new child is added

  (defmethod children ((parent hash-table))
    (or (gethash parent ht-hash)
        (setf (gethash parent ht-hash)
              (let ((ht-list nil))
                (maphash #'(lambda (key val)
                             (push (make-instance 'ht-entry
                                     :key key
                                     :value val
                                     :ht parent)
                                   ht-list))
                         parent)
                ht-list))))

  (defmethod (setf children) (new-value (parent hash-table))
    (setf (gethash parent ht-hash) new-value))

  (defmethod add-to-child-seq (parent (seq list) (thing ht-entry))
    (with-slots (ht ht-key ht-value) thing
      (setf (gethash ht-hash parent) (cons thing seq))
      (setf ht parent)
      (setf (gethash parent ht-key) ht-value)))

  (defmethod delete-from-child-seq ((seq list) (thing ht-entry))
    (with-slots (ht ht-key) thing
      (remhash ht-key ht)
      (delete-from-list seq thing)))

) ;; end of hash-table functions within let

;;; Functions to access children for other common types

(defmethod children ((parent sequence))
  ;; root objects that are lists or vectors are
  ;; also the sequence of children
  parent)

(defmethod (setf children) (new-children (parent vector))
  ;; root objects that are lists or vectors are
  ;; also the sequence of children
  (or parent new-children))

(defmethod (setf children) (new-children (parent list))
  ;; root objects that are lists or vectors are
  ;; also the sequence of children
  new-children)

(defmethod children (parent)
  (declare (ignore parent))
  ;; any other unknown type of parent
  nil)

;;; Functions to add/delete items from sequences
;;; See also corresponding methods for sequences of ht-entry items
;;; in section of hash-table methods above

(defmethod add-to-child-seq (parent (seq list) thing)
  (declare (ignore parent))
  (nconc seq (list thing)))

(defmethod add-to-child-seq (parent (seq vector) thing)
  (declare (ignore parent))
  (when (array-has-fill-pointer-p seq)
    (vector-push-extend thing seq)))

(defmethod delete-from-child-seq ((seq vector) thing)
  (let ((pos (position thing seq)))
    (dotimes (i (- (fill-pointer seq) pos 1))
      (setf (aref seq (+ pos i))
            (aref seq (+ pos i 1)))))
  (vector-pop seq))

(defmethod delete-from-child-seq ((seq list) thing)
  (delete-from-list seq thing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some utility functions used later

         
(defun valid-setf-for (read-form)
  (multiple-value-bind (a b c func-form d) (get-setf-expansion read-form)
    (declare (ignore a b c d))
    (or (not (eq (first func-form) 'funcall))
        ;; this must be a built-in function, so assume setf works
        ;; otherwise make sure the function name specified is fboundp
        (let ((func-name (second (second func-form))))
          (and (typep func-name 'function-name) (fboundp func-name))))))

(defun eval-without-errors (form)
  (when *lisp-controller-debug*
    (ns-log (format nil "evaling form: ~s" form)))
  (handler-case (eval form)
    (condition (c)
      (when *lisp-controller-debug*
        (format t "~%condition: ~s" c)))))

(defun reader-writer-pair (typ col-val)
  (let* ((reader-form nil)
         (writer-form nil))
    (cond ((null col-val)
           ;; reader justs return the object itself
           ;; leave the writer-form null
           (setf reader-form 'row))
          ((and (eq col-val :key) (eq typ 'ht-entry))
           ;; used for the key value in a hash table
           (setf reader-form '(ht-key row))
           (setf writer-form '(setf (ht-key row) new-val)))
          ((and (eq col-val :value) (eq typ 'ht-entry))
           ;; used for the value in a hash table
           (setf reader-form '(ht-value row))
           (setf writer-form '(setf (ht-value row) new-val)))
          ((eq col-val :row)
           (setf reader-form 'row)
           (setf writer-form '(setf row new-val)))
          ((numberp col-val)
           (cond ((subtypep typ 'vector)
                  (setf reader-form `(aref row ,col-val))
                  (setf writer-form `(setf (aref row ,col-val) new-val)))
                 ((subtypep typ 'list)
                  (setf reader-form `(nth ,col-val row))
                  (setf writer-form `(setf (nth ,col-val row) new-val)))
                 ((eq typ 'ht-entry)
                  ;; Index if the value is a sequence
                  (setf reader-form `(when (typep (ht-value row) 'sequence)
                                       (elt (ht-value row) ,col-val)))
                  (setf writer-form `(when (typep (ht-value row) 'sequence)
                                       (setf (elt (ht-value row) ,col-val) new-val))))
                 ((subtypep typ 'hash-table)
                  ;; use the number as a key into the hash table and return the value
                  (setf reader-form `(gethash ,col-val row))
                  (setf writer-form `(setf (gethash ,col-val row) new-val)))
                 (t
                  ;; index if row is any other type of sequence
                  (setf reader-form `(when (typep row 'sequence)
                                       (elt row ,col-val)))
                  (setf writer-form `(when (typep row 'sequence)
                                       (setf (elt row ,col-val) new-val))))))
          ((and (symbolp col-val) (fboundp col-val))
           (cond ((eq typ 'ht-entry)
                  ;; Assume the function applies to the value
                  (setf reader-form `(,col-val (ht-value row)))
                  (when (valid-setf-for reader-form)
                    (setf writer-form `(setf (,col-val (ht-value row)) new-val))))
                 (t
                  (setf reader-form `(,col-val row))
                  (when (valid-setf-for reader-form)
                    (setf writer-form `(setf (,col-val row) new-val))))))
          ((symbolp col-val)
           (cond ((subtypep typ 'hash-table)
                  ;; use the symbol as a key into the hash table and return the value
                  (setf reader-form `(gethash ,col-val row))
                  (setf writer-form `(setf (gethash ,col-val row) new-val)))))
          ((and (consp col-val) (eq (first col-val) 'function))
           (let ((col-val (second col-val)))
             (when (and (symbolp col-val) (fboundp col-val))
               (cond ((eq typ 'ht-entry)
                      ;; Assume the function applies to the value
                      (setf reader-form `(,col-val (ht-value row)))
                      (when (valid-setf-for reader-form)
                        (setf writer-form `(setf (,col-val (ht-value row)) new-val))))
                     (t
                      (setf reader-form `(,col-val row))
                      (when (valid-setf-for reader-form)
                        (setf writer-form `(setf (,col-val row) new-val))))))))
          ((consp col-val)
           ;; accessors are lisp forms possibly using keywords :row, :key, and :value
           ;; which are replaced appropriately
           (setf reader-form (nsubst 'row ':row 
                                     (nsubst '(ht-key row) :key
                                             (nsubst '(ht-value row) :value
                                                     col-val))))
           (when (valid-setf-for reader-form)
             (setf writer-form `(setf ,col-val new-val)))))
    (when *lisp-controller-debug*
      (ns-log (format nil "Reader-form: ~s~%Writer-form: ~s" reader-form writer-form)))
    (cons (and reader-form 
               (eval-without-errors `(function (lambda (row) ,reader-form))))
          (and writer-form 
               (eval-without-errors `(function (lambda (new-val row) ,writer-form)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lisp-controller class

(defclass lisp-controller (ns:ns-object)
  ((root :accessor root)
   (root-type :accessor root-type)
   (gen-root :accessor gen-root)
   (objects :accessor objects)
   (types :reader types)
   (reader-func :accessor reader-func)
   (writer-func :accessor writer-func)
   (count-func :accessor count-func)
   (select-func :accessor select-func)
   (edited-func :accessor edited-func)
   (added-func :accessor added-func)
   (removed-func :accessor removed-func)
   (delete-func :accessor delete-func)
   (add-child-func :accessor add-child-func)
   (children-func :accessor children-func)
   (type-info :accessor type-info)
   (column-info :accessor column-info)
   (nib-initialized :accessor nib-initialized)
   (view-class :accessor view-class)
   (can-remove :foreign-type #>BOOL :accessor can-remove)
   (can-insert :foreign-type #>BOOL :accessor can-insert)
   (can-add-child :foreign-type #>BOOL :accessor can-add-child)
   (owner :foreign-type :id :accessor owner)
   (view :foreign-type :id :accessor view))
  (:metaclass ns:+ns-object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; instance initialization methods

(objc:defmethod (#/initWithCoder: :id)
                ((self lisp-controller) (decoder :id))
  ;; This method is called when the Nib is loaded and provides values defined
  ;; when the NIB was created
  (#/init self)
  (with-slots (reader-func writer-func count-func select-func edited-func
               add-child-func root-type gen-root added-func removed-func
               children-func type-info delete-func) self
    (let ((type-info-array (#/decodeObjectForKey: decoder #@"typeInfo")))
      (dotimes (i (#/count type-info-array))
        ;; for each type specified in IB by the user
        (let* ((row-array (#/objectAtIndex: type-info-array i))
               (ns-str-type-name (#/objectAtIndex: row-array 0))
               (type-name (nsstring-to-sym ns-str-type-name))
               (child-type (nsstring-to-sym (#/objectAtIndex: row-array 1)))
               (child-func-str (ns-to-lisp-string (#/objectAtIndex: row-array 2)))
               (child-func (find-func child-func-str))
               (reader-sym (and child-func (function-name child-func)))
               (writer-form `(setf (,reader-sym thing) new-val))
               (child-writer-func (and child-func
                                       (valid-setf-for writer-form)
                                       (eval `(function (lambda (new-val thing)
                                                          ,writer-form))))))
          (when child-type
              (setf (assoc-aref type-info type-name :child-type) child-type))
          (when child-func
            (setf (assoc-aref type-info type-name :child-key) child-func))
          (when child-writer-func
              (setf (assoc-aref type-info type-name :child-setf-key) child-writer-func)))))
    (let ((initform-array (#/decodeObjectForKey: decoder #@"initforms")))
      (dotimes (i (#/count initform-array))
        ;; for each initform specified in IB by the user
        (let* ((row-array (#/objectAtIndex: initform-array i))
               (ns-str-type-name (#/objectAtIndex: row-array 0))
               (type-name (nsstring-to-sym ns-str-type-name))
               (initform (ns-to-lisp-object t (#/objectAtIndex: row-array 1))))
          (when initform
              (setf (assoc-aref type-info type-name :initform) initform)))))
    (let ((sort-info-array (#/decodeObjectForKey: decoder #@"sortInfo")))
      (dotimes (i (#/count sort-info-array))
        ;; for each sort predicate and key specified in IB by the user
        (let* ((row-array (#/objectAtIndex: sort-info-array i))
               (ns-str-type-name (#/objectAtIndex: row-array 0))
               (type-name (nsstring-to-sym ns-str-type-name))
               (sort-key (nsstring-to-func (#/objectAtIndex: row-array 1)))
               (sort-pred (nsstring-to-func (#/objectAtIndex: row-array 2))))
          (when sort-pred
            (setf (assoc-aref type-info type-name :sort-pred) sort-pred))
          (when sort-key
            (setf (assoc-aref type-info type-name :sort-key) sort-key)))))
    (setf root-type (nsstring-to-sym (#/decodeObjectForKey: decoder #@"rootType")))
    (setf (types self) (delete-duplicates (list* root-type 
                                                 'ht-entry
                                                 (mapcar-assoc-array #'identity type-info))))
    (setf reader-func (nsstring-to-func (#/decodeObjectForKey: decoder #@"readerFunc")))
    (setf writer-func (nsstring-to-func (#/decodeObjectForKey: decoder #@"writerFunc")))
    (setf count-func (nsstring-to-func (#/decodeObjectForKey: decoder #@"countFunc")))
    (setf select-func (nsstring-to-func (#/decodeObjectForKey: decoder #@"selectFunc")))
    (setf edited-func (nsstring-to-func (#/decodeObjectForKey: decoder #@"editedFunc")))
    (setf added-func (nsstring-to-func (#/decodeObjectForKey: decoder #@"addedFunc")))
    (setf removed-func (nsstring-to-func (#/decodeObjectForKey: decoder #@"removedFunc")))
    (setf delete-func (nsstring-to-func (#/decodeObjectForKey: decoder #@"deleteFunc")))
    (setf add-child-func (nsstring-to-func (#/decodeObjectForKey: decoder #@"addChildFunc")))
    (setf children-func (nsstring-to-func (#/decodeObjectForKey: decoder #@"childrenFunc")))
    (setf gen-root (#/decodeBoolForKey: decoder #@"genRoot")))
  self)

(objc:defmethod (#/init :id)
                ((self lisp-controller))
  ;; need to do this to initialize default values that are needed when
  ;; this object is instantiated from Objective-C runtime
  (unless (slot-boundp self 'nib-initialized)
    (setf (nib-initialized self) nil))
  (unless (slot-boundp self 'select-func)
    (setf (select-func self) nil))
  (unless (slot-boundp self 'edited-func)
    (setf (edited-func self) nil))
  (unless (slot-boundp self 'added-func)
    (setf (added-func self) nil))
  (unless (slot-boundp self 'removed-func)
    (setf (removed-func self) nil))
  (unless (slot-boundp self 'add-child-func)
    (setf (add-child-func self) nil))
  (unless (slot-boundp self 'delete-func)
    (setf (delete-func self) nil))
  (unless (slot-boundp self 'reader-func)
    (setf (reader-func self) nil))
  (unless (slot-boundp self 'writer-func)
    (setf (writer-func self) nil))
  (unless (slot-boundp self 'count-func)
    (setf (count-func self) nil))
  (unless (slot-boundp self 'objects)
    (setf (objects self) nil))
  (unless (slot-boundp self 'root)
    ;; note that we have to set root slot to avoid
    ;; calling accessor functions. This is a 
    ;; special case and the only place where we
    ;; want to set the root to something that 
    ;; doesn't match the root-type specified in IB
    (setf (slot-value self 'root) nil))
  (unless (slot-boundp self 'root-type)
    (setf (root-type self) t))
  (unless (slot-boundp self 'types)
    (setf (types self) nil))
  (unless (slot-boundp self 'type-info)
    (setf (type-info self) (make-instance 'assoc-array :rank 2)))
  ;; Now set up some default type info for standard types
  ;; These can be overridden in the lisp-controller setup
  ;; within Interface Builder.
  ;; Typically users would define their own types and
  ;; specify values for them rather than overriding these
  ;; but it is permissable to do so.
  (setf (assoc-aref (type-info self) 'hash-table :child-key) #'children)
  (setf (assoc-aref (type-info self) 'list :child-key) #'children)
  (setf (assoc-aref (type-info self) 'vector :child-key) #'children)
  (setf (assoc-aref (type-info self) 'hash-table :child-setf-key) #'(setf children))
  (setf (assoc-aref (type-info self) 'list :child-setf-key) #'(setf children))
  (setf (assoc-aref (type-info self) 'vector :child-setf-key) #'(setf children))
  (setf (assoc-aref (type-info self) 'hash-table :child-type) 'ht-entry)
  (setf (assoc-aref (type-info self) 'list :child-type) 'list)
  (setf (assoc-aref (type-info self) 'vector :child-type) 'vector)
  (setf (assoc-aref (type-info self) 'hash-table :initform)
        '(make-hash-table))
  (setf (assoc-aref (type-info self) 'list :initform)
        nil)
  (setf (assoc-aref (type-info self) 'vector :initform) 
        '(make-array '(10) :adjustable t :fill-pointer 0 :initial-element nil))
  self)

(objc:defmethod (#/awakeFromNib :void)
                ((self lisp-controller))
  (setf (nib-initialized self) t)
  (let ((has-valid-view (and (slot-boundp self 'view)
                             (not (eql (view self) (%null-ptr))))))
    (when has-valid-view
      (setf (view-class self) (#/class (view self)))
      (init-column-info self (view self)))
    (when (gen-root self)
      ;; create the root object
      (setf (root self) (new-object-of-type self (root-type self))))
    (when (and has-valid-view (objects self))
      (setup-accessors self))))

(defmethod setup-accessors ((self lisp-controller))
  ;; This method must be called to initialize the column value
  ;; accessor functions for a lisp-controller.
  ;; It is called after NIB loading has been done.
  (with-slots (reader-func column-info type-info types) self
    (unless reader-func
      (dolist (col (mapcar-assoc-array #'identity column-info))
        (let ((col-id (assoc-aref column-info col :col-val)))
          (dolist (typ types)
            (setf (assoc-aref type-info typ col) 
                  (reader-writer-pair typ col-id))))))))

(defmethod set-can-add-child ((self lisp-controller) row-selected)
  ;; indicates whether new children objects can be placed within
  ;; the object represented in the row specified.
  ;; If we have been given an explict add-child function then we can or
  ;; if we know the child type for the root type and have a valid
  ;; child key for which there is an associated setf function
  ;; then we can also insert a new object.
  (#/willChangeValueForKey: self #@"canAddChild")
  (let* ((obj (object-at-row self row-selected))
         (obj-type (controller-type-of self obj))
         (child-setf-key (assoc-aref (type-info self) obj-type :child-setf-key)))
    (if child-setf-key
      (setf (can-add-child self)  #$YES)
      (setf (can-add-child self) #$NO)))
  (#/didChangeValueForKey: self #@"canAddChild"))

(defmethod set-can-insert :around ((self lisp-controller) new-obj)
  (declare (ignore new-obj))
  (#/willChangeValueForKey: self #@"canInsert")
  (call-next-method)
  (#/didChangeValueForKey: self #@"canInsert"))

(defmethod set-can-insert ((self lisp-controller) new-obj)
  (declare (ignore new-obj))
  ;; indicates whether new children objects can be placed within
  ;; the root object.
  ;; If we have been given an explict insert function then we can or
  ;; if we know the child type for the root type and have a valid
  ;; child key for which there is an associated setf function
  ;; then we can also insert a new object.
  (if (or (add-child-func self)
          (assoc-aref (type-info self) (root-type self) :child-setf-key))
    (setf (can-insert self) #$YES)
    (setf (can-insert self) #$NO)))

(defmethod set-can-insert ((self lisp-controller) (new-obj vector))
  (setf (can-insert self) 
        (if (or (add-child-func self)
                (and (assoc-aref (type-info self) (controller-type-of self new-obj) :child-setf-key)
                     (array-has-fill-pointer-p (objects self))
                     (or (adjustable-array-p (objects self))
                         (< (fill-pointer (objects self)) 
                            (first (array-dimensions (objects self)))))))
          #$YES
          #$NO)))

(defmethod (setf types) (new-types (self lisp-controller))
  ;; sort the types making most specific first
  ;; when we look for a type match using the function controller-type-of
  ;; we'll find the most specific type that matches the given object
  ;; Unfortunately we can't just call sort using #'subtypep as a predicate
  ;; because it may not create a list where it is guaranteed that a appears
  ;; before b whenever a is a subtype of b. So we do our own manual insertion
  ;; sort.
  (let ((res-list nil)
        (pos nil))
    (dolist (typ new-types)
      (setf pos (position-if #'(lambda (check-type)
                                 (subtypep typ check-type))
                             res-list))
      (if pos
        ;; splice type into the res-list at pos
        (setf res-list (add-to-list-at res-list pos typ))
        (setf res-list (nconc res-list (list typ)))))
    (setf (slot-value self 'types) res-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; View-specific methods

(defmethod init-column-info ((self lisp-controller) (view ns:ns-table-view))
  (with-slots (column-info) self
    (let* ((tc-arr (#/tableColumns view))
           (col-obj nil)
           (idc nil)
           (col-count (#/count tc-arr)))
      (unless tc-arr
        (ns-log "#/tableColumns returned nil for view")
        (return-from init-column-info))
      (setf column-info (make-instance 'assoc-array :rank 2))
      (dotimes (i col-count)
        (setf col-obj (#/objectAtIndex: tc-arr i))
        (setf (assoc-aref column-info col-obj :col-indx) i)
        (setf (assoc-aref column-info i :col-obj) col-obj)
        (setf idc (ns-to-lisp-string (#/identifier col-obj)))
        (setf (assoc-aref column-info col-obj :col-string) idc)
        (setf (assoc-aref column-info col-obj :col-val) 
              (read-from-string idc nil nil))
        (setf (assoc-aref column-info col-obj :col-title)
              (ns-to-lisp-string (#/title (#/headerCell col-obj))))
        ;; find any formatter attached to the data cell for this column and 
        ;; use info from it to help us translate to and from lisp objects
        ;; appropriately
        (let ((formatter-object (#/formatter (#/dataCell col-obj))))
          (unless (eql formatter-object (%null-ptr))
            (cond ((typep formatter-object 'ns:ns-date-formatter)
                   (setf (assoc-aref column-info col-obj :col-format) :date))
                  ((typep formatter-object 'ns:ns-number-formatter)
                   (cond ((#/generatesDecimalNumbers formatter-object)
                          (let ((dec-digits (#/maximumFractionDigits formatter-object)))
                            (setf (assoc-aref column-info col-obj :col-format)
                                  (list :decimal dec-digits))))
                         (t
                          (setf (assoc-aref column-info col-obj :col-format)
                                :number)))))))))))
                   

(defmethod object-at-row ((self lisp-controller) row-selected)
  ;; returns two objects: the lisp object represented by the specified 
  ;; row and the parent of that object
  (unless (eql row-selected -1)
    (cond ((eql (view-class self) ns:ns-outline-view)
           (let ((ptr-wrapper (#/itemAtRow: (view self) row-selected)))
             (values (lpw-lisp-ptr ptr-wrapper)
                     (lpw-parent ptr-wrapper))))
          ((eql (view-class self) ns:ns-table-view)
           (values (elt (objects self) row-selected) (root self))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility methods for the lisp-controller class

(defmethod controller-type-of ((self lisp-controller) thing)
  ;; find which of the declared types (if any) matches thing
  ;; if none do, just return its lisp type
  (if (eq thing (root self))
    (root-type self)
    (or (find-if #'(lambda (typ)
                     (typep thing typ))
                 (types self))
        (type-of thing))))

(defmethod controller-types-of ((self lisp-controller) thing)
  ;; finds all of the declared types (if any) that match thing
  ;; in an order from most specific to least specific including
  ;; its lisp type
  (nconc (remove-if-not #'(lambda (typ)
                            (typep thing typ))
                        (types self))
         (list (type-of thing))))

(defmethod most-specific-type-info ((self lisp-controller) type-list info-key)
  (some #'(lambda (typ)
            (assoc-aref (type-info self) typ info-key))
        type-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; methods callable by outside functions within a lisp program

(defmethod lisp-controller-changed ((self lisp-controller))
  ;; program should call this if it changes the contents of the
  ;; list object (but not the pointer to the list itself). In
  ;; the latter case set-lisp-list-controller should be called.
  (#/reloadData (view self)))

(defmethod (setf root) :before (new-obj (self lisp-controller))
  (let ((typ (controller-type-of self new-obj))
        (rt (root-type self)))
    (when (not (subtypep typ rt))
      ;; trying to set root to something other than what was specified in IB
      (error "Type of ~s (~s) is not a subtype of ~s" new-obj typ rt)))
  (#/willChangeValueForKey: self #@"root"))

(defmethod (setf root) :after (new-obj (self lisp-controller))
  ;; cache the children of the root object because they are used so frequently
  (setf (objects self) (children-of-object self new-obj))
  (#/didChangeValueForKey: self #@"root")
  (when (and (nib-initialized self) (not (eql (view self) (%null-ptr))))
    (setup-accessors self)
    (set-can-insert self new-obj)
    (sort-sequence self (objects self))
    (#/reloadData (view self)))
  new-obj)

(defmethod (setf view) :after (new-view (self lisp-controller))
  ;; only used if lisp-list-controller object is not created via a nib load
  ;; and view is set from somewhere else
  (when new-view
    (init-column-info self (view self))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; miscellaneous methods for accessing and transforming objects

(defmethod new-child-initform ((self lisp-controller) parent-type)
  ;; get the initform for the child type of the parent-type parameter
  (let ((child-type (assoc-aref (type-info self) parent-type :child-type)))
    (when child-type
      ;; child-type cannot be nil unless there are no children
      (assoc-aref (type-info self) child-type :initform))))

(defmethod new-object-of-type ((self lisp-controller) typ)
  ;; Create & initialize a new instance of some type
  (eval (assoc-aref (type-info self) typ :initform)))

(defmethod children-of-object ((self lisp-controller) (obj ht-entry))
  ;; We want to handle the children of ht-entry objects a bit differently
  ;; Basically we would like to return the children of the value of the entry
  (children-of-object self (ht-value obj)))

(defmethod children-of-object ((self lisp-controller) obj)
  ;; Get the children of an instance of some type
  (let* ((obj-type (controller-type-of self obj))
         (child-key (assoc-aref (type-info self) obj-type :child-key))
         (children-object nil))
    (if (children-func self)
      (setf children-object (funcall (children-func self) (owner self) self obj))
      (if child-key
        (setf children-object (funcall child-key obj))))
    ;; if the children object is a hash-table, expand it into an ht-entry list
    (when (typep children-object 'hash-table)
      (setf children-object (children children-object)))
    (sort-sequence self children-object)))

(defmethod add-child-to ((self lisp-controller) (parent ht-entry))
  ;; We want to handle the children of ht-entry objects a bit differently
  ;; Basically we would like to add a child to the children of the value of the entry
  (add-child-to self (ht-value parent)))

(defmethod add-child-to ((self lisp-controller) parent)
  (let* ((parent-type (controller-type-of self parent))
         (child-type (assoc-aref (type-info self) parent-type :child-type))
         (child-key (assoc-aref (type-info self) parent-type :child-key))
         (child-initform (and child-type (assoc-aref (type-info self) child-type :initform)))
         (set-child-func (assoc-aref (type-info self) parent-type :child-setf-key))
         (new-children nil)
         (new-child nil))
    (if (and child-type child-key child-initform set-child-func)
      ;; we've got everything we need to set the child ourselves
      (let ((children (children-of-object self parent)))
        (setf new-child (eval child-initform))
        (setf new-children 
              (funcall set-child-func 
                       (add-to-child-seq parent children new-child) 
                       parent))
        (when (subtypep (controller-type-of self parent) 'hash-table)
          (setf (ht new-child) parent)
          (setf (gethash (ht-key new-child) parent) (ht-value new-child))))
      ;; else see if there is a user-specified function to add a child
      (when (add-child-func self)
        (multiple-value-setq (new-children new-child)
          (funcall (add-child-func self) parent))))
    (when (added-func self)
      ;; notify by calling the function specified in IB
      (let ((last-child (if (typep new-child 'ht-entry)
                          (list (ht-key new-child) (ht-value new-child))
                          new-child)))
        (when last-child
          (funcall (added-func self) 
                   (owner self)
                   self
                   (root self)
                   parent last-child))))
    (sort-sequence self new-children)))

(defmethod child-type-of ((self lisp-controller) obj)
  ;; Get the type of child objects for an instance of some type
  (let ((obj-type (controller-type-of self obj)))
    (assoc-aref (type-info self) obj-type :child-type)))

(defmethod col-value ((self lisp-controller) obj col-obj)
  ;; Get the lisp value for some column for an object
  ;; return "" if there isn't one so the display doesn't
  ;; have "nil" for columns without values.
  (let* ((obj-type (controller-type-of self obj))
         (override-reader (reader-func self))
         (reader-func (car (assoc-aref (type-info self) obj-type col-obj))))
    (if override-reader
      (funcall override-reader obj (assoc-aref (column-info self) col-obj :col-val))
      (if reader-func
        (funcall reader-func obj)
        ""))))

(defmethod set-col-value ((self lisp-controller) obj col-obj new-value)
  ;; set the lisp value for some column for an object
  (let* ((obj-type (controller-type-of self obj))
         (override-writer (writer-func self))
         (writer-func (cdr (assoc-aref (type-info self) obj-type col-obj))))
    (if override-writer
      (funcall override-writer new-value obj (assoc-aref (column-info self) col-obj :col-val))
      (if writer-func
        (funcall writer-func new-value obj)))))

(defmethod remove-child-from ((self lisp-controller) parent child)
  (let* ((parent-type (controller-type-of self parent))
         (child-key (assoc-aref (type-info self) parent-type :child-key))
         (set-child-func (assoc-aref (type-info self) parent-type :child-setf-key))
         (parent-is-root (eq parent (root self)))
         (new-children nil))
    (if (delete-func self)
      (setf new-children (funcall (delete-func self) parent child))
      (when (and child-key set-child-func)
        (let ((children (funcall child-key parent)))
          (setf new-children 
                (funcall set-child-func
                         (delete-from-child-seq children child)
                         parent)))))
    (when (and parent-is-root (null new-children))
      ;; The only time this actually does something is when the 
      ;; objects were a list and we just deleted the last child.
      (if (listp parent) (setf (root self) nil))
      (setf (objects self) nil))
    (when (removed-func self)
      (funcall (removed-func self) (owner self) self (root self) parent child))
    (sort-sequence self new-children)))

(defmethod sort-sequence ((self lisp-controller) (seq sequence))
  ;; sort a sequence of objects
  (if (plusp (length seq))
    (let* ((seq-elt-type (controller-type-of self (elt seq 0)))
           (seq-elt-sort-pred (assoc-aref (type-info self) seq-elt-type :sort-pred))
           (seq-elt-sort-key (assoc-aref (type-info self) seq-elt-type :sort-key)))
      (if seq-elt-sort-pred
        (typecase seq
          (cons 
           (sort-list-in-place seq seq-elt-sort-pred seq-elt-sort-key))
          (vector 
           (if seq-elt-sort-key
             (sort seq seq-elt-sort-pred :key seq-elt-sort-key)
             (sort seq seq-elt-sort-pred))))
        seq))
    seq))

(defmethod sort-sequence ((self lisp-controller) thing)
  ;; trying to sort something that isn't a sequence
  ;; just do nothing
  thing)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods invoked by NSTableView objects at runtime.
;; Needed to be a data source for NSTableView

(objc:defmethod (#/tableView:objectValueForTableColumn:row: :id) 
                ((self lisp-controller) 
                 (tab :id)
                 (col :id)
                 (row #>NSInteger))
  (declare (ignore tab))
  (let ((ns-format (assoc-aref (column-info self) col :col-format)))
    (lisp-to-ns-object (col-value self (elt (objects self) row) col) ns-format)))

(objc:defmethod (#/numberOfRowsInTableView: #>NSInteger) 
                ((self lisp-controller) (tab :id))
  ;; Assumes that objects is some type of sequence
  ;; Subclass should override this method if that is not true.
  (declare (ignore tab))
  (with-slots (root objects count-func owner) self
    (if root
      (if count-func
        (funcall count-func owner self root)
        (typecase objects
          (array (if (array-has-fill-pointer-p objects)
                   (fill-pointer objects)
                   (first (array-dimensions objects))))
          (t
           (length objects))))
      0)))
  
(objc:defmethod (#/tableView:setObjectValue:forTableColumn:row: :void)
                ((self lisp-controller) 
                 (tab :id)
                 (val :id)
                 (col :id)
                 (row #>NSInteger))
  ;; We let the user edit the table and something was changed
  ;; try to convert it to the same type as what is already in that
  ;; position in the objects.
  (declare (ignore tab))
  (let* ((row-obj (elt (objects self) row))
         (old-obj (col-value self row-obj col))
         (ns-format (assoc-aref (column-info self) col :col-format))
         (new-val (ns-to-lisp-object old-obj val ns-format)))
    (if (writer-func self)
      (funcall (writer-func self)
               new-val
               (root self) 
               row
               (assoc-aref (column-info self) col :col-val))
      (set-col-value self row-obj col new-val))
    (when (edited-func self)
      (let* ((row-obj (object-at-row self row))
             (edited-obj (if (typep row-obj 'ht-entry)
                           (list (ht-key row-obj) (ht-value row-obj))
                           row-obj)))
        (funcall (edited-func self)
                 (owner self)
                 self
                 (root self)
                 row
                 (assoc-aref (column-info self) col :col-indx)
                 edited-obj
                 old-obj
                 new-val)))
    ;; re-sort and reload the table
    ;; unfortunately we probably have to do this for every change since
    ;; we don't know what affects the sort order
    (sort-sequence self (objects self))
    (#/reloadData (view self))))

(objc:defmethod (#/tableView:shouldEditTableColumn:row: #>BOOL)
                ((self lisp-controller) 
                 (tab :id)
                 (col :id)
                 (row #>NSInteger))
  (declare (ignore tab))
  ;; allow editing if there is a function available to setf a new value
  (if (or (writer-func self)
           (let ((obj-type (controller-type-of self (elt (objects self) row))))
             (cdr (assoc-aref (type-info self) obj-type col))))
    #$YES
    #$NO))

(objc:defmethod (#/tableViewSelectionDidChange: :void) 
                ((self lisp-controller) (notif :id))
  (let* ((tab (#/object notif))
         (row-indx (#/selectedRow tab))
         (col-indx (#/selectedColumn tab)))
    ;; enable/disable buttons that remove current selection
    (#/willChangeValueForKey: self #@"canRemove")
    (if (minusp row-indx)
      (setf (can-remove self) #$NO)
      (setf (can-remove self) #$YES))
    (#/didChangeValueForKey: self #@"canRemove")
    ;; enable/disable buttons that want to add a child to
    ;; the current selection
    (set-can-add-child self row-indx)
    ;; User code to do something when a cell is selected
    (when (select-func self)
      (let* ((row-obj (and (not (eql row-indx -1)) (object-at-row self row-indx)))
             (col (assoc-aref (column-info self) col-indx :col-obj))
             (selected-obj (cond ((and (minusp col-indx) (minusp row-indx))
                                  nil)
                                 ((minusp col-indx)
                                  row-obj)
                                 ((minusp row-indx)
                                  (assoc-aref (column-info self) col :col-title))
                                 (t
                                  (col-value self row-obj col)))))
        (funcall (select-func self)
                 (owner self)
                 self
                 (root self)
                 row-indx
                 col-indx
                 selected-obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods invoked by NSOutlineView objects at runtime.
;; Needed to be a data source for NSOutlineView.

(objc:defmethod (#/outlineView:child:ofItem: :id)
                ((self lisp-controller) 
                 (olview :id)
                 (child #>NSInteger)
                 (item :id))
  (declare (ignore olview))
  (with-slots (objects) self
    (cond ((typep item 'lisp-ptr-wrapper)
           (let* ((parent (lpw-lisp-ptr item))
                  (parent-depth (lpw-depth item))
                  (children (children-of-object self parent))
                  (child-ptr (elt children child)))
             (wrapper-for self child-ptr :depth (1+ parent-depth) :parent parent)))
          ((eql item (%null-ptr))
           (let ((child-ptr (elt objects child)))
             (wrapper-for self child-ptr :depth 1)))
          (t
           (%null-ptr)))))

(objc:defmethod (#/outlineView:isItemExpandable: #>BOOL)
                ((self lisp-controller) 
                 (olview :id)
                 (item :id))
  (declare (ignore olview))
  (cond ((eql item (%null-ptr))
         ;; root object
         #$YES)
        ((typep item 'lisp-ptr-wrapper)
         (if (children-of-object self (lpw-lisp-ptr item))
           #$YES
           #$NO))
         (t
          #$NO)))

(objc:defmethod (#/outlineView:numberOfChildrenOfItem: #>NSInteger)
                ((self lisp-controller) 
                 (olview :id)
                 (item :id))
  (declare (ignore olview))
  (cond ((typep item 'lisp-ptr-wrapper)
         (length (children-of-object self (lpw-lisp-ptr item))))
        ((eql item (%null-ptr))
         (length (objects self)))
        (t
         0)))

(objc:defmethod (#/outlineView:objectValueForTableColumn:byItem: :id)
                ((self lisp-controller) 
                 (olview :id)
                 (col :id)
                 (item :id))
  (declare (ignore olview))
  (let ((ns-format (assoc-aref (column-info self) col :col-format)))
    (lisp-to-ns-object (col-value self (lpw-lisp-ptr item) col) ns-format)))

(objc:defmethod (#/outlineView:setObjectValue:forTableColumn:byItem: :void)
                ((self lisp-controller) 
                 (olview :id)
                 (val :id)
                 (col :id)
                 (item :id))
  (let* ((row-obj (lpw-lisp-ptr item))
         (old-obj (col-value self row-obj col))
         (ns-format (assoc-aref (column-info self) col :col-format))
         (new-val (ns-to-lisp-object old-obj val ns-format)))
    (if (writer-func self)
      (funcall (writer-func self)
               new-val
               (root self) 
               row-obj
               (assoc-aref (column-info self) col :col-val))
      (set-col-value self row-obj col new-val))
    (when (edited-func self)
      (let* ((row (#/rowForItem: olview item))
             (edited-obj (if (typep row-obj 'ht-entry)
                           (list (ht-key row-obj) (ht-value row-obj))
                           row-obj)))
        (funcall (edited-func self)
                 (owner self)
                 self
                 (root self)
                 row
                 (assoc-aref (column-info self) col :col-val)
                 edited-obj
                 old-obj
                 new-val)))))

(objc:defmethod (#/outlineView:shouldEditTableColumn:item: #>BOOL)
                ((self lisp-controller) 
                 (olview :id)
                 (col :id)
                 (item :id))
  (declare (ignore olview))
  ;; allow editing if there is a function available to setf a new value
  (if (or (writer-func self)
           (let ((obj-type (controller-type-of self (lpw-lisp-ptr item))))
             (cdr (assoc-aref (type-info self) obj-type col))))
    #$YES
    #$NO))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods for inserting and removing rows. User interface can trigger these
;; (for example) by setting action methods on buttons 

(objc:defmethod (#/insert: :void)
                ((self lisp-controller) (button :id))
  (declare (ignore button))
  ;; insert a new object into the root object
  (unless (root self)
    ;; need to create a root object
    ;; may still be null if root type is 'list
    (setf (root self)
          (new-object-of-type self (root-type self))))
  (let ((new-children (add-child-to self (root self))))
    (when (null (root self))
      ;; special hack for root objects that are lists and may be null
      (setf (root self) new-children)
      (setf (objects self) new-children)))
  (#/reloadData (view self)))

(objc:defmethod (#/addChild: :void)
                ((self lisp-controller) (button :id))
  (declare (ignore button))
  ;; add a new child to the currently selected item
  (let* ((row-num (#/selectedRow (view self)))
         (parent (object-at-row self row-num)))
    (add-child-to self parent))
  (#/reloadData (view self)))

(objc:defmethod (#/remove: :void)
                ((self lisp-controller) (button :id))
  (declare (ignore button))
  (let ((row-num (#/selectedRow (view self))))
    (multiple-value-bind (child parent) (object-at-row self row-num)
      (when parent
        (remove-child-from self parent child)
        (#/reloadData (view self))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods for using a lisp-controller as an initial binding target
;; Binding paths must start with "root" or "selected" and can use
;; lisp accessors from that point to other lisp targets.
;; Any atomic value found by following the path is converted to an 
;; Objective-C value and returned. If a non-atomic value is found by 
;; following the path it is encapsulated within a lisp-ptr-wrapper object
;; and returned. When that path is subsequently followed the lisp-ptr-wrapper
;; will handle the path reference in the same way.

(objc:defmethod (#/root :id)
                ((self lisp-controller))
  (cond ((typep (root self) 'objc:objc-object)
             (root self))
        ((null (root self))
         (%null-ptr))
        ((typep (root self) 'objc-displayable)
         (lisp-to-ns-object (root self)))
        (t
         (wrapper-for self (root self)))))

(objc:defmethod (#/selection :id)
                ((self lisp-controller))
  (let* ((row-num (#/selectedRow (view self)))
         (obj (object-at-row self row-num)))
    (cond ((typep obj 'objc:objc-object)
           obj)
          ((typep obj 'objc-displayable)
           (lisp-to-ns-object obj))
          (t
           (wrapper-for self obj)))))

(provide :lisp-controller)
      