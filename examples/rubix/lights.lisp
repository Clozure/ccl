(in-package :cl-user)

;; ah, lights, one of my favorite subjects in OpenGL -- because they way
;; they work when you're using C++ stinks! I seem to recall i have extensive
;; discussions of how i would rather deal with TL&M if i was using lisp in
;; one of my code files somewhere, but first let me get it working then i can
;; get it working properly

(defclass light ()
  ((lightid :initform 0 :initarg :lightid :accessor lightid)
   (on-p :initform nil :accessor on-p)
   (pointsourcep :initform nil :initarg :pointsourcep :accessor pointsourcep)
   (location :initform nil :initarg :location :accessor location)
   (ambient :initform nil :initarg :ambient :accessor ambient)
   (diffuse :initform nil :initarg :diffuse :accessor diffuse)
   (specular :initform nil :initarg :specular :accessor specular))
  (:default-initargs :location (make-array 4 :initial-element 0.0 ; lights are special!
                                           :element-type 'single-float)
                     :ambient (make-array 4 :initial-element 0.0
                                           :element-type 'single-float)
                     :diffuse (make-array 4 :initial-element 0.0
                                           :element-type 'single-float)
                     :specular (make-array 4 :initial-element 0.0
                                           :element-type 'single-float)))

(defmethod on ((light light))
  (#_glEnable (lightid light))
  (setf (on-p light) t))
(defmethod off ((light light))
  (#_glDisable (lightid light))
  (setf (on-p light) nil))

(defmethod setlocation ((light light) pos)
  (dotimes (i 3) (setf (elt (location light) i) (elt pos i)))
  (ccl::%stack-block ((foreign-float-vector (* 4 4))) ; make room for 4 single-floats
    (ccl::%copy-ivector-to-ptr (location light) ; source
      0     ; offset to first element (alignment padding)
      foreign-float-vector ; destination
      0                    ; byte offset in destination
      (* 4 4))             ; number of bytes to copy
    (#_glLightfv (lightid light) #$GL_POSITION foreign-float-vector)))
(defmethod setpointsource ((light light) bool)
  (setf (pointsourcep light) (if bool t nil) ; <- don't hang on to non-nils
        (elt (location light) 3) (if bool 1.0 0.0))
  (ccl::%stack-block ((foreign-float-vector (* 4 4))) ; make room for 4 single-floats
    (ccl::%copy-ivector-to-ptr (location light) ; source
      0     ; offset to first element (alignment padding)
      foreign-float-vector ; destination
      0                    ; byte offset in destination
      (* 4 4))             ; number of bytes to copy
    (#_glLightfv (lightid light) #$GL_POSITION foreign-float-vector)))

(defmethod setambient ((light light) color)
  (dotimes (i 4) (setf (elt (ambient light) i) (elt color i)))
  (ccl::%stack-block ((foreign-float-vector (* 4 4))) ; make room for 4 single-floats
    (ccl::%copy-ivector-to-ptr (ambient light) ; source
      0     ; offset to first element (alignment padding)
      foreign-float-vector ; destination
      0                    ; byte offset in destination
      (* 4 4))             ; number of bytes to copy
    (#_glLightfv (lightid light) #$GL_AMBIENT foreign-float-vector)))
(defmethod setdiffuse ((light light) color)
  (dotimes (i 4) (setf (elt (diffuse light) i) (elt color i)))
  (ccl::%stack-block ((foreign-float-vector (* 4 4))) ; make room for 4 single-floats
    (ccl::%copy-ivector-to-ptr (diffuse light) ; source
      0     ; offset to first element (alignment padding)
      foreign-float-vector ; destination
      0                    ; byte offset in destination
      (* 4 4))             ; number of bytes to copy
    (#_glLightfv (lightid light) #$GL_DIFFUSE foreign-float-vector)))
(defmethod setspecular ((light light) color)
  (dotimes (i 4) (setf (elt (specular light) i) (elt color i)))
  (ccl::%stack-block ((foreign-float-vector (* 4 4))) ; make room for 4 single-floats
    (ccl::%copy-ivector-to-ptr (specular light) ; source
      0     ; offset to first element (alignment padding)
      foreign-float-vector ; destination
      0                    ; byte offset in destination
      (* 4 4))             ; number of bytes to copy
    (#_glLightfv (lightid light) #$GL_SPECULAR foreign-float-vector)))
