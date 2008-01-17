;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2003 Clozure Associates
;;;   This file is part of OpenMCL.  
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(in-package "CCL")


;;; A (partial) implementation of SPLAY-TREEs, which are binary trees
;;; that reorganize themselves so that the most recently accessed keys
;;; cluster near the tree's root.

(defstruct (tree-node
             (:constructor make-tree-node (key value)))
  key
  value
  left                                  ; the child < this key, or NIL
  right                                 ; the child > this key, or NIL
  parent                                ; we're the root if NIL.   
  )

(defmethod print-object ((node tree-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (let* ((*print-circle* t))
      (format stream "~s -> ~s" (tree-node-key node) (tree-node-value node)))))


(defun tree-node-is-leaf (n)
  (and (null (tree-node-left n))
       (null (tree-node-right n))))

(defun tree-node-is-root (n)
  (null (tree-node-parent n)))

;;; Is node the left child of its parent ?
(defun tree-node-is-left (n)
  (let* ((parent (tree-node-parent n)))
    (and parent (eq n (tree-node-left parent)))))

(defun tree-node-is-right (n)
  (let* ((parent (tree-node-parent n)))
    (and parent (eq n (tree-node-right parent)))))

(defun tree-node-set-right (node newright)
  (when (setf (tree-node-right node) newright)
    (setf (tree-node-parent newright) node)))

(defun tree-node-set-left (node newleft)
  (when (setf (tree-node-left node) newleft)
    (setf (tree-node-parent newleft) node)))             

(defun tree-node-replace-child (node old new)
  (if (eq old (tree-node-left node))
    (tree-node-set-left node new)
    (tree-node-set-right node new)))

(defstruct (splay-tree (:constructor %make-splay-tree))
  (root nil :type (or null splay-tree-node))
  equal                                 ; true if x = y
  less                                  ; true if x < y
  (count 0)
  )

(defmethod print-object ((tree splay-tree) stream)
  (print-unreadable-object (tree stream :type t :identity t)
    (format stream "count = ~d, root = ~s"
	    (splay-tree-count tree)
	    (splay-tree-root tree))))
	    


;;; Returns tree-node or NIL
(defun binary-tree-get (tree key)
  (do* ((equal (splay-tree-equal tree))
        (less (splay-tree-less tree))
        (node (splay-tree-root tree)))
       ((null node))
    (let* ((node-key (tree-node-key node)))
      (if (funcall equal key node-key)
        (return node)
        (if (funcall less key node-key)
          (setq node (tree-node-left node))
          (setq node (tree-node-right node)))))))

;;; No node with matching key exists in the tree
(defun binary-tree-insert (tree node)
  (let* ((root (splay-tree-root tree)))
    (if (null root)
      (setf (splay-tree-root tree) node)
      (do* ((less (splay-tree-less tree))
            (key (tree-node-key node))
            (current root)
            (parent nil))
           ((null current)
            (if (funcall less key (tree-node-key parent))
              (tree-node-set-left parent node)
              (tree-node-set-right parent node)))
        (setq parent current)
        (if (funcall less key (tree-node-key current))
          (setq current (tree-node-left current))
          (setq current (tree-node-right current))))))
  (incf (splay-tree-count tree)))
    
            
;;; Replace the node's parent with the node itself, updating the
;;; affected children so that the binary tree remains properly
;;; ordered.
(defun binary-tree-rotate (tree node)
  (when (and node (not (tree-node-is-root node)))
    (let* ((parent (tree-node-parent node))
           (grandparent (if parent (tree-node-parent parent)))
           (was-left (tree-node-is-left node)))
      (if grandparent
        (tree-node-replace-child grandparent parent node)
        (setf (splay-tree-root tree) node
              (tree-node-parent node) nil))
      (if was-left
        (progn
          (tree-node-set-left parent (tree-node-right node))
          (tree-node-set-right node parent))
        (progn
          (tree-node-set-right parent (tree-node-left node))
          (tree-node-set-left node parent))))))

;;; Keep rotating the node (and maybe its parent) until the node's the
;;; root of tree.
(defun splay-tree-splay (tree node)
  (when node
    (do* ()
         ((tree-node-is-root node))
      (let* ((parent (tree-node-parent node))
             (grandparent (tree-node-parent parent)))
        (cond ((null grandparent)
               (binary-tree-rotate tree node)) ; node is now root
              ((eq (tree-node-is-left node)
                   (tree-node-is-left parent))
               (binary-tree-rotate tree parent)
               (binary-tree-rotate tree node))
              (t
               (binary-tree-rotate tree node)
               (binary-tree-rotate tree node)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The more-or-less public API follows.
;;;
;;; I suppose that we should support DELETE as well, and perhaps
;;; UPDATE (find the node and modify its key in place.)  For now,
;;; SPLAY-TREE-PUT assumes that no node with a matching key exists.
;;; Access to the tree has to be serialized by the caller.

(defun splay-tree-get (tree key &optional default)
  (let* ((node (binary-tree-get tree key)))
    (if node
      (progn
        (splay-tree-splay tree node)
        (tree-node-value node))
      default)))

(defun splay-tree-put (tree key value)
  (let* ((node (make-tree-node key value)))
    (binary-tree-insert tree node)
    (splay-tree-splay tree node)
    value))

;;; Note that the tree wants two comparison functions.  This may
;;; increase the chance that builtin CL functions can be used; a tree
;;; whose keys are real numbers could use #'= and #'<, for instance.
;;; Using two comparison functions is (at best) only slightly better
;;; than insisting that a single comparison function return (values
;;; equal less), or (member -1 0 1), or some other convention.

(defun make-splay-tree (equal less)
  (check-type equal function)
  (check-type less function)
  (%make-splay-tree :equal equal :less less))

;;; Do an inorder traversal of the splay tree, applying function F
;;; to the value of each node.

(defun map-splay-tree (tree f)
  (labels ((map-tree-node (node)
	     (when node
	       (map-tree-node (tree-node-left node))
	       (funcall f (tree-node-value node))
	       (map-tree-node (tree-node-right node)))))
    (map-tree-node (splay-tree-root tree))))

(defun map-splay-tree-keys-and-values (tree f)
  (labels ((map-tree-node (node)
	     (when node
	       (map-tree-node (tree-node-left node))
	       (funcall f (tree-node-key node) (tree-node-value node))
	       (map-tree-node (tree-node-right node)))))
    (map-tree-node (splay-tree-root tree)))) 
