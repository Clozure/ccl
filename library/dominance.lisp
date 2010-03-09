;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2010 Clozure Associates
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

;(setq *print-array* nil)
;(setq *print-simple-bit-vector* nil)

#+:linuxx8664-target
(progn

(export '(idom-heap-utilization))

#|
(open-core "home:core.28209")
(idom-heap-utilization :unit nil :sort :size)
|#

(defconstant half-fixnum-shift (ash (integer-length most-positive-fixnum) -1))

(defconstant half-fixnum-mask (1- (ash 1 half-fixnum-shift)))

(defstruct (core-graph (:include core-info) (:conc-name "CG.") (:constructor %cons-cg))
  (heap-base 0 :type fixnum)
  (heap-end 0 :type fixnum)
  (stage nil) ;; indication of what has been computed and what hasn't, so can restart.
  (head-p #.(make-array 0 :element-type 'bit) :type simple-bit-vector)
  (ptrs-p #.(make-array 0 :element-type 'bit) :type simple-bit-vector)
  ;; Nodes after eliminating single-entry and leaf objects
  (nodes #() :type simple-vector) ;; map postorder-idx -> dnode
  (revnodes #() :type simple-vector) ;; map dnode -> postorder-idx
  (roots () :type list)
  (predecessors #() :type simple-vector) ;; postorder-idx -> list of postorder indices of predecessors
  (node-doms #() :type simple-vector) ;; postorder-idx of node -> postorder-idx of its immediate dominator
  (idoms #() :type simple-vector) ;; sequence of postorder indices of immediate dominators
  (revidoms #() :type simple-vector) ;; map dnode -> index in idoms
  (logsizes #() :type simple-vector) ;; corresponding sequence of logical sizes (including all owned objects)
  (physizes #() :type simple-vector) ;; corresponding sequence of physical sizes (including all owned objects)
  )

(setq *core-info-class* 'core-graph)

(defparameter *cg-stages* '(nil :objects :leaves :postorder :predecessors :idoms :idom-sizes t))

(defmethod cg-compute :before (stage &aux (cg (current-core)))
  (assert (memq stage *cg-stages*))
  (check-type cg core-graph)
  (when (eql (cg.heap-base cg) 0)
    (let ((area-ptr (core-q (kernel-global-address 'tenured-area))))
      (setf (cg.heap-base cg) (core-q area-ptr target::area.low))
      (setf (cg.heap-end cg) (core-q area-ptr target::area.active))))
  ;; ensure have all the prereqs
  (loop for undone = (cdr (memq (cg.stage cg) *cg-stages*))
        while (memq stage (cdr undone))
        do (format t "~&Computing ~a" (car undone))
        do (cg-compute (car undone))))

(defmethod cg-compute :after (stage &aux (cg (current-core)))
  (setf (cg.stage cg) stage))

(defmethod cg-compute ((stage (eql t))) ;; before method does all the work
  nil)


(declaim (inline core-node-p))
(defun core-node-p (ptr) (or (core-consp  ptr) (core-uvector-p ptr)))

(declaim (inline dnode addr))

(defun dnode (base n) (the fixnum (ash (%i- n base) (- target::dnode-shift))))

(defun addr (base n) (%i+ base (ash (the fixnum n) target::dnode-shift)))

(defun tagged-ptr (ptr)
  (let ((header (core-q ptr)))
    (cond ((uvheader-p header)
           (let ((subtag (uvheader-typecode header)))
             (+ ptr (cond ((eq subtag target::subtag-symbol) target::fulltag-symbol)
                          ((eq subtag target::subtag-function) target::fulltag-function)
                          (t target::fulltag-misc)))) )
          (t
           (+ ptr target::fulltag-cons)))))

(defun core-physsize (obj)
  ;; (assert (core-node-p obj))
  (if (core-consp obj)
    target::dnode-size
    (logandc2 (+ (uvheader-byte-size (core-uvheader obj)) target::node-size (1- target::dnode-size))
              (1- target::dnode-size))))

(defun core-object-sizes (obj)
  (let ((fulltag (logand obj target::fulltagmask)))
    (if (eq fulltag target::fulltag-cons)
      (values target::dnode-size target::dnode-size)
      (if (%i<= target::fulltag-misc fulltag)
        (let* ((header (core-uvheader obj))
               (logsize (uvheader-byte-size header))
               ;; total including header and alignment.
               (total (logandc2 (+ logsize target::node-size (1- target::dnode-size))
                                (1- target::dnode-size))))
          (values logsize total))))))

(defun link-range (ptr)
  (declare (fixnum ptr))
  (let* ((addr (logandc2 ptr target::fulltagmask))
         (header (core-q addr))
         (end addr))
    (declare (fixnum addr end))
    (if (uvheader-p header)
      (let ((subtag (%ilogand header target::fulltagmask)))
        (declare (fixnum subtag))
        (when (or (eq subtag target::fulltag-nodeheader-0)
                  (eq subtag target::fulltag-nodeheader-1))
          (incf addr target::node-size)
          (setq end (+ addr (ash (uvheader-size header) target::word-shift)))
          (when (eql (uvheader-typecode header) target::subtag-function)
            (incf addr (ash (core-l addr) target::word-shift)))))
      (setq end (+ addr target::dnode-size)))
    (values addr end)))

(defmethod cg-compute ((stage (eql :objects)) &aux (cg (current-core)))
  "Compute incoming pointer counts"
  (let* ((base (cg.heap-base cg))
         (high (cg.heap-end cg))
         (ndnodes (dnode base high))
         (ptrs (make-array ndnodes :element-type 'bit :initial-element 0))
         (head (make-array ndnodes :element-type 'bit :initial-element 0)))
    (declare (fixnum base ndnodes)
             (type simple-bit-vector ptrs head))
    (map-core-region base high
                     (lambda (obj)
                       (multiple-value-bind (start end) (link-range obj)
                         (loop for addr from start below end by target::node-size
                               as ptr = (core-q addr)
                               do (when (and (<= base ptr) (< ptr high) (core-node-p ptr))
                                    (let ((dnode (dnode base ptr)))
                                      (setf (aref head dnode) (aref ptrs dnode))
                                      (setf (aref ptrs dnode) 1)))))
                       ;; Mark that have an object even if there are no refs to it.
                       (let ((dnode (dnode base obj)))
                         (when (eql (aref ptrs dnode) 0)
                           (setf (aref head dnode) 1)))))
    ;; head = 0, ptrs = 0  -- not an object (internal dnode)
    ;; head = 0, ptrs = 1  -- single-entry object (exactly one pointer to it)
    ;; head = 1, ptrs = 0  -- root object (no pointers to it)
    ;; head = 1, ptrs = 1  -- multiple-entry object
    (setf (cg.head-p cg) head)
    (setf (cg.ptrs-p cg) ptrs)
    cg))

(defmethod cg-compute ((stage (eql :leaves)) &aux (cg (current-core)))
  "Mark leaf nodes (nodes with no outgoing pointers)"
  (let* ((base (cg.heap-base cg))
	 (high (cg.heap-end cg))
         (ptrs (cg.ptrs-p cg))
         (head (cg.head-p cg)))
    (declare (fixnum base high))
    (loop for dn upfrom 0
          for h bit across head
          for p bit across ptrs
	  do (unless (and (eql h 0) (eql p 0))
	       (unless (multiple-value-bind (start end) (link-range (addr base dn))
			 (loop for addr from start below end by target::node-size
			    as val = (core-q addr)
			    thereis (and (<= base val) (< val high) (core-node-p val))))
		 (setf (aref head dn) 0)
		 (setf (aref ptrs dn) 0))))
    ;; head = 0, ptrs = 0  -- not an object (internal dnode) or a leaf
    ;; head = 0, ptrs = 1  -- single-entry object (exactly one pointer to it), not leaf
    ;; head = 1, ptrs = 0  -- root object (no pointers to it), not leaf
    ;; head = 1, ptrs = 1  -- multiple-entry object, not leaf
    cg))

(defun collect-root-dnodes (cg)
  (let ((head (cg.head-p cg))
	(ptrs (cg.ptrs-p cg)))
    (loop for dn = (position 1 head) then (position 1 head :start (1+ dn)) while dn
          when (eql (aref ptrs dn) 0) collect dn)))

(defmethod cg-compute ((stage (eql :postorder)) &aux (cg (current-core)))
  (let* ((roots (collect-root-dnodes cg))
	 (head (cg.head-p cg))
	 (ptrs (cg.ptrs-p cg))
         (halo-roots ())
         (n (count 1 head))
         (base (cg.heap-base cg))
         (high (cg.heap-end cg))
         (ndnodes (dnode base high))
         (seen (make-array ndnodes :element-type 'bit :initial-element 0))
         (nodes (make-array n))
         (node-count 0))
    (assert (< ndnodes (ash 1 half-fixnum-shift)))
    (flet ((dfs (root-dn)
             (setf (aref seen root-dn) 1)
             (let ((path (multiple-value-bind (start end) (link-range (addr base root-dn))
			   (list (list* start end root-dn)))))
               (loop
		  (destructuring-bind (start end . pred-dn) (car path)
		    (incf (caar path) target::node-size)
		    (if (eql start end)
			(progn
			  (when (eql (aref head pred-dn) 1)
			    (setf (aref nodes node-count) pred-dn)
			    (incf node-count))
			  (setq path (cdr path))
			  (when (null path) (return)))
			(let ((next (core-q start)))
			  (when (and (<= base next) (< next high) (core-node-p next))
			    (let ((next-dn (dnode base next)))
			      (if (eql (aref ptrs next-dn) 0) ;; root or leaf -- ignore leaf
				  (when (eql (aref head next-dn) 1) ;; previously assumed halo root
				    #+debug (warn "REASSIGNING HALO ROOT ~s -> ~d" (assq next-dn halo-roots) node-count)
				    (assert (eql (aref seen next-dn) 1))
				    (setf (aref ptrs next-dn) 1)
				    ;; not actually a root after all. Shift the region containing
				    ;; nodes from previous handling of next-dn to the end, as if
				    ;; just walked it right now.
				    (destructuring-bind (start . end) (cdr (assq next-dn halo-roots))
				      (shift-vector-region nodes start end node-count))
				    (setq halo-roots (delete next-dn halo-roots :key 'car)))
				  ;; non-leaf non-root
				  (when (eq (aref seen next-dn) 0)
				    (setf (aref seen next-dn) 1)
				    (multiple-value-bind (start end) (link-range next)
				      (push (list* start end next-dn) path)))))))))))))
      (map nil #'dfs roots)
      ;; Map through "halo" roots
      (loop until (eql (length nodes) node-count)
	 as circ = (loop for nd = (position 1 head) then (position 1 head :start (1+ nd)) while nd
		      when (eql (aref seen nd) 0) return nd)
	 do (when (null circ)
	      ;; Must have some cycles consisting of just single-entry nodes, since we caught all other ones
	      (setq circ (loop for nd = (position 1 ptrs) then (position 1 ptrs :start (1+ nd)) while nd
			    when (eql (aref seen nd) 0) return nd))
              #+debug (progn (format t "~&Breaking a SINGLE-NODE CYCLE at ") (core-print (tagged-ptr (addr (cg.heap-base cg) circ))))
	      (setf (aref head circ) 1))
	 do (let ((start node-count))
              #+debug (progn (format t "~&Breaking out a HALO ROOT at ") (core-print (tagged-ptr (addr (cg.heap-base cg) circ))))
	      (dfs circ)
	      ;; This just makes it faster to find these in the dfs, it gets undone below.
	      (setf (aref ptrs circ) 0)
	      (push (list* circ start node-count) halo-roots))))
    (setq roots (nconc (mapcar (lambda (x &aux (dn (car x)))
                                 (setf (aref ptrs dn) 1)
                                 dn)
                               halo-roots)
                       roots))
    (setf (cg.roots cg) roots)
    (setf (cg.nodes cg) nodes)
    cg))

(defun shift-vector-region (vector start mid end)
  ;; move the interval from START to MID to after the interval from MID to END.
  (loop as n2 = (- end mid) as n1 = (- mid start)
        while (and (> n2 0) (> n1 0))
        do (if (< n1 n2)
             (loop for i from start below mid
                   do (rotatef (aref vector i) (aref vector (+ i n1)))
                   finally (setq start mid mid (+ mid n1)))
             (loop for i from mid below end
                   do (rotatef (aref vector i) (aref vector (- i n1)))
                   finally (setq start (+ start n2))))))


(declaim (inline make-rev-map))

(defun make-rev-map (arr &optional (fn 'identity))
  (let* ((n (length arr))
         (revarr (make-array n)))
    (loop for i from 0 below n as dn = (funcall fn (aref arr i))
          do (setf (aref revarr i) (+ (ash i half-fixnum-shift) dn))) ;; [pidx ,, dn]
    (sort revarr #'< :key (lambda (i.d) (logand i.d half-fixnum-mask)))))

(defun index-for-dnode (revnodes dn)
  (declare (type simple-vector revnodes) (fixnum dn)
           (optimize (speed 3) (safety 0)))
  (let ((low 0)
        (high (length revnodes)))
    (declare (fixnum low high) )
    (loop
      (when (eq low high) (return nil))
      (let* ((half (ash (%i+ high low) -1))
             (val (%ilogand2 (%svref revnodes half) half-fixnum-mask)))
        (declare (fixnum half val))
        (when (eq val dn)
          (return (the fixnum (ash (the fixnum (%svref revnodes half)) (- half-fixnum-shift)))))
        (if (< val dn)
          (setq low (1+ half))
          (setq high half))))))

(defmacro do-pointers ((child-var addr) &body body)
  (let ((path (gensym))
        (start (gensym))
        (end (gensym)))
    ` (macrolet ((descend-pointers (child)
                   `(multiple-value-bind (start end) (link-range ,child)
                      (push (cons start end) ,',path))))
        (let ((,path nil))
          (descend-pointers ,addr)
          (loop
            (destructuring-bind (,start . ,end) (car ,path)
              (incf (caar ,path) target::node-size)
              (if (eq ,start ,end)
                (unless (setq ,path (cdr ,path)) (return))
                (let ((,child-var (core-q ,start)))
                  (when (core-node-p ,child-var)
                    ,@body)))))))))

(defmethod cg-compute ((stage (eql :predecessors)) &aux (cg (current-core)))
  (let* ((base (cg.heap-base cg))
         (high (cg.heap-end cg))
	 (roots (cg.roots cg))
	 (head (cg.head-p cg))
	 (ptrs (cg.ptrs-p cg))
	 (nodes (cg.nodes cg)) ;; pidx -> dn
         (n (length nodes))
         (revnodes (make-rev-map nodes)) ;; dn -> pidx
         (predecessors (make-array (1+ n) :initial-element 0)))
    (flet ((record-predecessor (dn pred-i)
             (let* ((i (index-for-dnode revnodes dn))
                    (old (aref predecessors i)))
               (cond ((eql old 0)
                      (setf (aref predecessors i) (1+ pred-i)))
                     ((fixnump old)
                      (if (eql (logandc2 old half-fixnum-mask) 0)
                        (setf (aref predecessors i) (+ (ash old half-fixnum-shift) pred-i))
                        ;; could do more here, but most cases are covered by the 2-elt optimization
                        (setf (aref predecessors i)
                              (list pred-i
                                    (logand old half-fixnum-mask) (1- (ash old (- half-fixnum-shift)))))))
                     (t (setf (aref predecessors i) (cons pred-i old)))))))
      (loop for dn across nodes as dn-idx upfrom 0
            do (ASSERT (eql dn-idx (index-for-dnode revnodes dn)))
            do (do-pointers (next (addr base dn))
                 (when (and (<= base next) (< next high))
                   (let ((next-dn (dnode base next)))
                     (when (eq (aref ptrs next-dn) 1) ;; non-leaf
                       (if (eql (aref head next-dn) 1) ;; stop at head node
			 (record-predecessor next-dn dn-idx)
			 (descend-pointers next)))))))
      ;; Pretend there is one single root node which is the predecessor of all our roots.
      (loop for root-dn in roots do (record-predecessor root-dn n)))
    (setf (cg.revnodes cg) revnodes)
    (setf (cg.predecessors cg) predecessors)
    cg))

(defun predecessor-list (predecessors i)
  (let ((p (aref predecessors i)))
    (cond ((eql p 0) '())
          ((fixnump p)
           (let ((p1 (logand p half-fixnum-mask)))
             (if (eql p p1)
               (list (1- p1))
               (list p1 (1- (ash p (- half-fixnum-shift)))))))
          (t p))))

;;; Ok, now on to compute dominance
;; immediate dominators per Cooper, Harvey, Kennedy.
(defmethod cg-compute ((stage (eql :idoms)) &aux (cg (current-core)))
  (let* ((predecessors (cg.predecessors cg))
	 (root-idx (1- (length predecessors)))
         (doms (make-array (1+ root-idx) :initial-element nil)))
    (flet ((intersect (i1 i2)
             (when (and i1 i2)
               (loop until (eq i1 i2)
                     do (loop while (< i1 i2) do (setq i1 (aref doms i1)))
                     do (loop while (< i2 i1) do (setq i2 (aref doms i2)))))
             (or i1 i2))
           (preds (i)
             (predecessor-list predecessors i)))
      (declare (inline intersect preds))
      (setf (aref doms root-idx) root-idx)
      (loop for changed = 0
            do (loop for i from (1- root-idx) downto 0
                     do (let ((new-idom nil))
                          (loop for p in (preds i)
                                do (when (aref doms p) (setq new-idom (intersect p new-idom))))
                          (unless (eql new-idom (aref doms i))
                            (setf (aref doms i) new-idom)
                            (incf changed))))
            DO (progn #+debug (format t "~&Finished loop, changed=~d~%" changed))
            while (> changed 0)))
    (setf (cg.node-doms cg) doms)
    (setf (cg.idoms cg) (sort (delete root-idx (remove-duplicates doms)) #'<))
    (let ((nodes (cg.nodes cg)))
      (setf (cg.revidoms cg) (make-rev-map (cg.idoms cg) (lambda (ni) (aref nodes ni)))))
    cg))

(defmethod cg-compute ((stage (eql :idom-sizes)) &aux (cg (current-core)))
  (let* ((nodes (cg.nodes cg))
         (idom-nodes (cg.idoms cg))
         (idom-revnodes (cg.revidoms cg))
         (seen (make-array (length (cg.head-p cg)) :element-type 'bit :initial-element 0))
         (base (cg.heap-base cg))
         (high (cg.heap-end cg))
	 (nidoms (length idom-nodes))
	 (logsizes (make-array nidoms))
	 (physizes (make-array nidoms)))
    ;; Any object that's not an idom is only reachable by one idom,
    ;; so don't need to reinit SEEN bits between iterations.
    (setf (cg.idoms cg) idom-nodes)
    (loop for i from 0 below nidoms as idom = (aref idom-nodes i)
	 do (let* ((dn (aref nodes idom))
		   (addr (addr base dn))
		   (ptr (tagged-ptr addr)))
	      (multiple-value-bind (logsize physsize) (core-object-sizes ptr)
		(do-pointers (next addr)
		  (when (and (<= base next) (< next high))
		    (let ((next-dn (dnode base next)))
		      (unless (or (index-for-dnode idom-revnodes next-dn)
				  (eql (aref seen next-dn) 1))
			(setf (aref seen next-dn) 1)
			(multiple-value-bind (this-logsize this-physsize) (core-object-sizes next)
			  (incf logsize this-logsize)
			  (incf physsize this-physsize))
			(descend-pointers next)))))
		(setf (aref logsizes i) logsize)
		(setf (aref physizes i) physsize))))
    (setf (cg.logsizes cg) logsizes)
    (setf (cg.physizes cg) physizes)
    cg))

(defun idom-set-heap-range (area)
  (check-type area (member :tenured :dynamic))
  (multiple-value-bind (base end)
      (cond ((eq area :tenured)
             (let ((area-ptr (core-q (kernel-global-address 'tenured-area))))
               (values (core-q area-ptr target::area.low)
                       (core-q area-ptr target::area.active))))
            ((eq area :dynamic)
             (let* ((newest (core-q (core-q (kernel-global-address 'all-areas)) target::area.succ))
                    (oldest (core-q (kernel-global-address 'tenured-area))))
               (assert (loop for this = newest then older as older = (core-q this target::area.succ)
                             until (eql this oldest)
                             always (eql (core-q this target::area.low) (core-q older target::area.active))))
               (values (core-q oldest target::area.low)
                       (core-q newest target::area.active)))))
    (let ((cg (current-core)))
      (unless (and (eq base (cg.heap-base cg))
                   (eq end (cg.heap-end cg)))
        (setf (cg.stage cg) nil)
        (setf (cg.heap-base cg) base)
        (setf (cg.heap-end cg) end)))))
  

(defun report-idom-heap-utilization (type-infos &key unit sort threshold)
  (let ((data  (loop for type being the hash-key of type-infos using (hash-value info)
                     collect (cons (core-type-string type) info))))
    (report-heap-utilization data :unit unit :sort sort :stream *standard-output* :threshold threshold)))

(defun idom-heap-utilization (&key unit (sort :size) (threshold 0.01) (area :tenured))
  (idom-set-heap-range area)
  (cg-compute t)
  (loop with cg = (current-core)
        with nodes = (cg.nodes cg)
        with type-infos = (make-hash-table :test 'eql)
        with base = (cg.heap-base cg)
        for idx across (cg.idoms cg)
        for logsz across (cg.logsizes cg)
        for physz across (cg.physizes cg)
        as type = (core-object-type-key (tagged-ptr (addr base (aref nodes idx))))
        as info = (or (gethash type type-infos) (setf (gethash type type-infos) (list 0 0 0)))
        do (incf (car info))
        do (incf (cadr info) logsz)
        do (incf (caddr info) physz)
        finally (report-idom-heap-utilization type-infos :unit unit :sort sort :threshold threshold)))

(defun idom-frontier-heap-utilization (&key unit (sort :size) (threshold 0.01) (area :tenured) (test nil))
  ;; Compute the heap utilization WITHIN selected idom trees, aggregated.
  (idom-set-heap-range area)
  (cg-compute :idoms)
  (let* ((cg (current-core))
         (nodes (cg.nodes cg))
         (idom-nodes (cg.idoms cg))
         (idom-revnodes (cg.revidoms cg))
         (seen (make-array (length (cg.head-p cg)) :element-type 'bit :initial-element 0))
         (base (cg.heap-base cg))
         (high (cg.heap-end cg))
	 (nidoms (length idom-nodes))
         (type-infos (make-hash-table :test 'eql)))
    (flet ((record (ptr)
             (multiple-value-bind (logsize physsize) (core-object-sizes ptr)
               (let* ((type (core-object-type-key ptr))
                      (info (or (gethash type type-infos) (setf (gethash type type-infos) (list 0 0 0)))))
                 (incf (car info))
                 (incf (cadr info) logsize)
                 (incf (caddr info) physsize)))))
      (loop for i from 0 below nidoms as idom = (aref idom-nodes i)
            do (let* ((dn (aref nodes idom))
                      (addr (addr base dn))
                      (ptr (tagged-ptr addr)))
                 (when (or (null test) (funcall test ptr))
                   ;; Ok, idom of interest.  Walk its subgraph
                   (record ptr)
                   (do-pointers (next addr)
                     (when (and (<= base next) (< next high))
                       (let ((next-dn (dnode base next)))
                         (unless (or (index-for-dnode idom-revnodes next-dn)
                                     (eql (aref seen next-dn) 1))
                           (setf (aref seen next-dn) 1)
                           (record next)
                           (descend-pointers next)))))))
            finally (report-idom-heap-utilization type-infos :unit unit :sort sort :threshold threshold)))))

)
