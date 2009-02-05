(in-package :cl-user)

(defparameter *cube* nil)

(defparameter *camera-pos* #(10.0 5.0 12.0))

(defparameter *selection-buffer-size* 256)

;; some things have no scale or rotation, such as point light sources
;; (note lights use a 4d vector to hold both positoin and pointsourceness)
(defclass positioned-object ()
  ((location :initform nil :initarg :location :accessor location))
  (:default-initargs :location (make-array 3 :initial-element 0.0)))

(defmethod move-relative ((obj positioned-object) v)
  (add-vectors (location obj) v (location obj))
  (location obj))
(defmethod move-relative-3 ((obj positioned-object) dx dy dz)
  (incf (elt (location obj) 0) dx)
  (incf (elt (location obj) 1) dy)
  (incf (elt (location obj) 2) dz)
  (location obj))
(defmethod move-absolute ((obj positioned-object) p)
  (dotimes (i 3) (setf (elt (location obj) i) (elt p i)))
  (location obj))
(defmethod move-absolute-3 ((obj positioned-object) x y z)
  (setf (elt (location obj) 0) x
        (elt (location obj) 1) y
        (elt (location obj) 2) z)
  (location obj))

(defmethod gl-translate ((obj positioned-object))
  (#_glTranslatef (elt (location obj) 0)
                  (elt (location obj) 1)
                  (elt (location obj) 2)))

(defclass rotated-object ()
  ((quaternion :initform nil :initarg :quaternion :accessor quaternion))
  (:default-initargs :quaternion (make-instance 'quaternion)))

(defmethod rotate-relative ((obj rotated-object) quaternion)
  ;; recall mulquats applies q2's rotation first...
  (mulquats quaternion (quaternion obj) (quaternion obj))
  (quaternion obj))
(defmethod rotate-absolute ((obj rotated-object) quaternion)
  (setf (w (quaternion obj)) (w quaternion))
  (dotimes (i 3)
    (setf (elt (xyz (quaternion obj)) i) (elt quaternion i)))
  (quaternion obj))

(defmethod gl-rotate ((obj rotated-object))
  (let ((axis-angle (quat->axis-angle (quaternion obj))))
    (#_glRotatef (cdr axis-angle)
                 (elt (car axis-angle) 0)
                 (elt (car axis-angle) 1)
                 (elt (car axis-angle) 2))))

(defclass scaled-object ()
  ((dilation :initform nil :initarg :dilation :accessor dilation))
  (:default-initargs :dilation (make-array 3 :initial-element 1.0)))

(defmethod gl-scale ((obj scaled-object))
  (#_glScalef (elt (dilation obj) 0)
              (elt (dilation obj) 1)
              (elt (dilation obj) 2)))

(defclass transformed-object (positioned-object
                              rotated-object
                              scaled-object)
  ())

(defmacro with-transformation ((transformed-object) &body body)
  (let ((tobj-sym (gensym)))
    `(let ((,tobj-sym ,transformed-object))
       (#_glPushMatrix)
       (gl-translate ,tobj-sym)
       (gl-rotate ,tobj-sym)
       (gl-scale ,tobj-sym)
       ,@body
       (#_glPopMatrix))))

(defmethod render ((obj transformed-object)) ; should this be on something else?
  (#_glMatrixMode #$GL_MODELVIEW)
  (with-transformation (obj)
    (render-children obj)))

(defclass block (transformed-object)
  (;; need to generate matrices of this form so that copy-ivector-etc will work
   (vertices :initform (coerce
                        (list (make-array 3 :initial-contents '(-0.5 -0.5  0.5)
                                          :element-type 'single-float)
                              (make-array 3 :initial-contents '( 0.5 -0.5  0.5)
                                          :element-type 'single-float)
                              (make-array 3 :initial-contents '( 0.5  0.5  0.5)
                                          :element-type 'single-float)
                              (make-array 3 :initial-contents '(-0.5  0.5  0.5)
                                          :element-type 'single-float)
                              (make-array 3 :initial-contents '(-0.5 -0.5 -0.5)
                                          :element-type 'single-float)
                              (make-array 3 :initial-contents '( 0.5 -0.5 -0.5)
                                          :element-type 'single-float)
                              (make-array 3 :initial-contents '( 0.5  0.5 -0.5)
                                          :element-type 'single-float)
                              (make-array 3 :initial-contents '(-0.5  0.5 -0.5)
                                          :element-type 'single-float))
                        'vector)
             :initarg :vertices :accessor vertices
             ;; :allocation :class
             )))

;; I expect that especially with the FFI overhead, one call to render
;; a static object's prefabbed display list will beat out a lot of
;; calls to render the various portions... this will be an interesting
;; conversionn and test going from code to DL, and good prep for
;; moving from DL-creating code to DL file readers
#+ignore
(defmethod render-children ((obj block))
  (let ((curve-radius 0.1)) ; 90-degree curve in 3 sections for edges and for corners
    ;; strip for faces 0134 and their edges
    ;; strip for face 2 and edges to 0 and 3
    ;; strip for face 5 and edges to 0 and 3
    ;; edges 15, 54, 42, and 21
    ;; corner
    ))

(defmethod render-children ((obj block))
  (flet ((norm (axis) (#_glNormal3f (aref axis 0) (aref axis 1) (aref axis 2)))
         (material (color)
           (ccl::%stack-block ((foreign-float-vector (* 4 4))) ; make room for 4 double-floats
             (ccl::%copy-ivector-to-ptr color
               0 ; offset to first element (alignment padding)
               foreign-float-vector ; destination
               0 ; byte offset in destination
               (* 4 4)) ; number of bytes to copy
             (#_glMaterialfv #$GL_FRONT_AND_BACK
                             #$GL_AMBIENT_AND_DIFFUSE
                             foreign-float-vector)))
         (quad (a b c d)
           (ccl::%stack-block ((ffv (* 4 3)))
             (ccl::%copy-ivector-to-ptr (aref (vertices obj) a) 0 ffv 0 (* 4 3))
             (#_glVertex3fv ffv))
           (ccl::%stack-block ((ffv (* 4 3)))
             (ccl::%copy-ivector-to-ptr (aref (vertices obj) b) 0 ffv 0 (* 4 3))
             (#_glVertex3fv ffv))
           (ccl::%stack-block ((ffv (* 4 3)))
             (ccl::%copy-ivector-to-ptr (aref (vertices obj) c) 0 ffv 0 (* 4 3))
             (#_glVertex3fv ffv))
           (ccl::%stack-block ((ffv (* 4 3)))
             (ccl::%copy-ivector-to-ptr (aref (vertices obj) d) 0 ffv 0 (* 4 3))
             (#_glVertex3fv ffv))
           t))
    (opengl:with-gl (#$GL_QUADS)
      (norm *x-axis*)     (material *hel-orange*) (quad 1 2 6 5)
      (norm *y-axis*)     (material *hel-yellow*) (quad 2 3 7 6)
      (norm *z-axis*)     (material *hel-green*)  (quad 0 3 2 1)
      (norm *neg-x-axis*) (material *hel-red*)    (quad 0 4 7 3)
      (norm *neg-y-axis*) (material *hel-white*)  (quad 0 1 5 4)
      (norm *neg-z-axis*) (material *hel-blue*)   (quad 4 5 6 7))))

(defclass rubix-cube (transformed-object)
  ((blocks :initform nil :initarg :blocks :accessor blocks)
   (faces :initform nil :initarg :faces :accessor faces)
   (faces-axes :initform (coerce (list *neg-x-axis* *neg-y-axis* *neg-z-axis*
                                       *x-axis* *y-axis* *z-axis*) 'vector)
               :initarg :faces-axes :reader faces-axes
               ;; :allocation :class
               )
   (face-turning-p :initform nil :initarg :face-turning-p :accessor face-turning-p)
   (turning-face :initform nil :initarg :turning-face :accessor turning-face)
   (face-theta :initform nil :initarg :face-theta :accessor face-theta)
   ;; vertices for rendering full cube's faces for selection
   (vertices :initform (coerce
                        (list (make-array 3 :initial-contents '(-0.5 -0.5  0.5)
                                          :element-type 'single-float)
                              (make-array 3 :initial-contents '( 0.5 -0.5  0.5)
                                          :element-type 'single-float)
                              (make-array 3 :initial-contents '( 0.5  0.5  0.5)
                                          :element-type 'single-float)
                              (make-array 3 :initial-contents '(-0.5  0.5  0.5)
                                          :element-type 'single-float)
                              (make-array 3 :initial-contents '(-0.5 -0.5 -0.5)
                                          :element-type 'single-float)
                              (make-array 3 :initial-contents '( 0.5 -0.5 -0.5)
                                          :element-type 'single-float)
                              (make-array 3 :initial-contents '( 0.5  0.5 -0.5)
                                          :element-type 'single-float)
                              (make-array 3 :initial-contents '(-0.5  0.5 -0.5)
                                          :element-type 'single-float))
                        'vector)
             :initarg :vertices :reader vertices
             ;; :allocation :class
             ))
  (:default-initargs
      :blocks (let ((list nil))
                (loop for x from -1.0 to 1.0 do
                     (loop for y from -1.0 to 1.0 do
                          (loop for z from -1.0 to 1.0 do
                               (push (make-instance 'block
                                       :location (coerce (list (/ x 3.0)
                                                               (/ y 3.0)
                                                               (/ z 3.0)) 'vector)
                                       :dilation (coerce (list (/ 1.0 3.0)
                                                               (/ 1.0 3.0)
                                                               (/ 1.0 3.0)) 'vector))
                                     list))))
                (coerce list 'vector))))

(defparameter *child-positions* (let ((list nil))
				  (loop for x from -1.0 to 1.0 do
					(loop for y from -1.0 to 1.0 do
					      (loop for z from -1.0 to 1.0 do
						    (push (coerce (list (/ x 3.0)
									(/ y 3.0)
									(/ z 3.0)) 'vector)
							  list))))
				  (coerce list 'vector)))

;; blocks in faces start at a corner, go clockwise around the face,
;; and finish in the center; blocks in the cube are numbered to
;; correspond to *child-positions*; faces that share blocks are
;; associated in faces-neighbors -- all 3 of these variables depend on
;; each other
(defparameter *initial-blocks-in-faces* #2a((0 1 2 5 8 7 6 3 4)
					    (0 9 18 19 20 11 2 1 10)
					    (0 3 6 15 24 21 18 9 12)
					    (26 23 20 19 18 21 24 25 22)
					    (26 25 24 15 6 7 8 17 16)
					    (26 17 8 5 2 11 20 23 14)))

(defmethod shared-initialize :after ((obj rubix-cube) slot-names &key)
  (declare (ignore slot-names))
  (setf (faces obj) (make-array (list 6 9)))
  (dotimes (face 6)
    (dotimes (blok 9)
      (setf (aref (faces obj) face blok)
	    (aref (blocks obj) (aref *initial-blocks-in-faces* face blok))))))

(let ((faces-neighbors #2a((1 5 4 2)
                           (2 3 5 0)
                           (0 4 3 1)
                           (5 1 2 4)
                           (3 2 0 5)
                           (4 0 1 3))))
  (defun faces-neighbor (face neighbor)
    (aref faces-neighbors face neighbor))
  (defun faces-index-from-neighbor (face neighbor)
    (loop for i from 0 to 3 do
      (when (= face (faces-neighbor (faces-neighbor face neighbor) i))
        (return i))))
  )

(defmethod turnfaceclockwise ((cube rubix-cube) face &aux temp)
  (with-slots (faces) cube
    ;; rotate blocks through adjacent faces
    (dotimes (neighbor 4)
      (let* ((neighbors-face (faces-neighbor face neighbor))
             (my-index (faces-index-from-neighbor face neighbor))
             (my-block-index (* 2 my-index))
             (his-new-block-index (* 2 (mod (+ neighbor 3) 4))))
        (setf (aref faces neighbors-face (mod my-block-index 8))
              (aref faces face (mod (+ 2 his-new-block-index) 8)))
        (setf (aref faces neighbors-face (mod (1+ my-block-index) 8))
              (aref faces face (mod (1+ his-new-block-index) 8)))
        (setf (aref faces neighbors-face (mod (+ 2 my-block-index) 8))
              (aref faces face (mod his-new-block-index 8)))))
    ;; rotate blocks in this face
    (setf temp (aref faces face 0)
          (aref faces face 0) (aref faces face 6)
          (aref faces face 6) (aref faces face 4)
          (aref faces face 4) (aref faces face 2)
          (aref faces face 2) temp
          temp (aref faces face 1)
          (aref faces face 1) (aref faces face 7)
          (aref faces face 7) (aref faces face 5)
          (aref faces face 5) (aref faces face 3)
          (aref faces face 3) temp)
    ;; update positions and orientation of blocks in this face
    (dotimes (i 9)
      (move-absolute (aref faces face i)
		     (elt *child-positions* (aref *initial-blocks-in-faces* face i)))
      (rotate-relative (aref faces face i)
		       (axis-angle->quat (aref (faces-axes cube) face)
					 90.0)))
    ))

(defmethod turnfacecounterclockwise ((cube rubix-cube) face &aux temp)
  (with-slots (faces) cube
    ;; rotate blocks through adjacent faces
    (dotimes (neighbor 4)
      (let* ((neighbors-face (faces-neighbor face neighbor))
             (my-index (faces-index-from-neighbor face neighbor))
             (my-block-index (* 2 my-index))
             (his-new-block-index (* 2 (mod (+ neighbor 1) 4))))
        (setf (aref faces neighbors-face (mod my-block-index 8))
              (aref faces face (mod (+ 2 his-new-block-index) 8)))
        (setf (aref faces neighbors-face (mod (1+ my-block-index) 8))
              (aref faces face (mod (1+ his-new-block-index) 8)))
        (setf (aref faces neighbors-face (mod (+ 2 my-block-index) 8))
              (aref faces face (mod his-new-block-index 8)))))
    ;; rotate blocks in this face
    (setf temp (aref faces face 0)
          (aref faces face 0) (aref faces face 2)
          (aref faces face 2) (aref faces face 4)
          (aref faces face 4) (aref faces face 6)
          (aref faces face 6) temp
          temp (aref faces face 1)
          (aref faces face 1) (aref faces face 3)
          (aref faces face 3) (aref faces face 5)
          (aref faces face 5) (aref faces face 7)
          (aref faces face 7) temp)
    ;; update positions and orientation of blocks in this face
    (dotimes (i 9)
      (move-absolute (aref faces face i)
		     (elt *child-positions* (aref *initial-blocks-in-faces* face i)))
      (rotate-relative (aref faces face i)
		       (axis-angle->quat (aref (faces-axes cube) face)
					 -90.0)))
    ))

(defmethod render-children ((obj rubix-cube))
  (flet ((in-face-p (face blok)
	   (dotimes (i 9)
	     (when (eq (aref (blocks obj) blok)
		       (aref (faces obj) face i))
	       (return t)))))
    (cond ((not (face-turning-p obj))
	   (dotimes (blok 27)
	     (render (aref (blocks obj) blok))))
	  (t
	   (dotimes (blok 27)
	     (unless (in-face-p (turning-face obj) blok)
	       (render (aref (blocks obj) blok))))
	   (opengl:with-rotation ((face-theta obj)
				  (aref (faces-axes obj) (turning-face obj)))
	     (dotimes (blok 9)
	       (render (aref (faces obj) (turning-face obj) blok))))))))


(defmethod render-for-selection ((objc rubix-cube) picked-point)
  (let ((gl-uint-size (ccl::foreign-size :<GL>uint :bytes)) ; 4, as it turns out...
	(selection-buffer-size 256))
    (ccl::%stack-block ((selection-buffer (* gl-uint-size selection-buffer-size)))
      (#_glSelectBuffer selection-buffer-size selection-buffer)
      (let (;; FYI - this loses a lot of structure and becomes a lot
	    ;; longer in C++ for lack of macros
	    (hits (opengl:with-render-mode (#$GL_SELECT)
		    (#_glInitNames)
		    (#_glPushName 0)
		    (opengl:with-culling (#$GL_FRONT)
		      ;; set up the modified camera looking around the mouse's region
		      (opengl:with-matrix-mode (#$GL_PROJECTION)
		        (opengl:with-matrix (t)
		          (#_glFrustum -0.01d0 0.01d0 -0.01d0 0.01d0 10.0d0 20.0d0)
			  (opengl:with-matrix-mode (#$GL_MODELVIEW)
			    (opengl:with-matrix (t)
			      (mylookat *camera-pos* picked-point *y-axis*)
			      ;; NOW render the cube like we were doing before
			      (opengl:with-matrix-mode (#$GL_MODELVIEW)
				(with-transformation (objc)
				  (render-children-for-selection objc)))))))
		      (#_glFlush)))))
	(when (and (numberp hits)
		   (< 0 hits))
	  ;; the first hit name is at selectBuf[3], though i don't recall why
	  (ccl::%get-unsigned-long selection-buffer (* 3 4)))))))

(defmethod render-children-for-selection ((objc rubix-cube))
  (flet ((norm (axis) (#_glNormal3f (aref axis 0) (aref axis 1) (aref axis 2)))
         (material (color)
           (ccl::%stack-block ((foreign-float-vector (* 4 4))) ; make room for 4 single-floats
             (ccl::%copy-ivector-to-ptr color
               0 ; offset to first element (alignment padding)
               foreign-float-vector ; destination
               0 ; byte offset in destination
               (* 4 4)) ; number of bytes to copy
             (#_glMaterialfv #$GL_FRONT_AND_BACK
                             #$GL_AMBIENT_AND_DIFFUSE
                             foreign-float-vector)))
         (quad (a b c d)
           (ccl::%stack-block ((ffv (* 4 3)))
             (ccl::%copy-ivector-to-ptr (aref (vertices objc) a) 0 ffv 0 (* 4 3))
             (#_glVertex3fv ffv))
           (ccl::%stack-block ((ffv (* 4 3)))
             (ccl::%copy-ivector-to-ptr (aref (vertices objc) b) 0 ffv 0 (* 4 3))
             (#_glVertex3fv ffv))
           (ccl::%stack-block ((ffv (* 4 3)))
             (ccl::%copy-ivector-to-ptr (aref (vertices objc) c) 0 ffv 0 (* 4 3))
             (#_glVertex3fv ffv))
           (ccl::%stack-block ((ffv (* 4 3)))
             (ccl::%copy-ivector-to-ptr (aref (vertices objc) d) 0 ffv 0 (* 4 3))
             (#_glVertex3fv ffv))
           t))
    (#_glLoadName 0)
    (opengl:with-gl (#$GL_QUADS)
      (norm *x-axis*)     (material *hel-orange*) (quad 1 2 6 5))
    (#_glLoadName 1)
    (opengl:with-gl (#$GL_QUADS)
      (norm *y-axis*)     (material *hel-yellow*) (quad 2 3 7 6))
    (#_glLoadName 2)
    (opengl:with-gl (#$GL_QUADS)
      (norm *z-axis*)     (material *hel-green*)  (quad 0 3 2 1))
    (#_glLoadName 3)
    (opengl:with-gl (#$GL_QUADS)
      (norm *neg-x-axis*) (material *hel-red*)    (quad 0 4 7 3))
    (#_glLoadName 4)
    (opengl:with-gl (#$GL_QUADS)
      (norm *neg-y-axis*) (material *hel-white*)  (quad 0 1 5 4))
    (#_glLoadName 5)
    (opengl:with-gl (#$GL_QUADS)
      (norm *neg-z-axis*) (material *hel-blue*)   (quad 4 5 6 7))))
