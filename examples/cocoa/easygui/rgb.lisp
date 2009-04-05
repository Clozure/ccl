(in-package :easygui)

; --------------------------------------------------------------------------------
; This provides for Clozure CL some RGB functions to match Allegro CL.
; Contributed by AWSC (arthur.cater@ucd.ie) March 2009.
; Permission to disseminate, use and modify is granted.
; --------------------------------------------------------------------------------

(defun make-rgb (&key (red 0) (green 0) (blue 0) (opacity 1.0))
  (assert (typep red     '(integer 0 255)) (red)
          "Value of RED component for make-rgb must be an integer 0-255 inclusive")
  (assert (typep green   '(integer 0 255)) (green)
          "Value of GREEN component for make-rgb must be an integer 0-255 inclusive")
  (assert (typep blue    '(integer 0 255)) (blue)
          "Value of BLUE component for make-rgb must be an integer 0-255 inclusive")
  (assert (typep opacity '(single-float 0.0 1.0)) (opacity)
          "Value of OPACITY component for make-rgb must be a single-float 0.0-1.0 inclusive")
  (#/retain
   (#/colorWithCalibratedRed:green:blue:alpha:
    ns:ns-color
    (/ red 255.0)
    (/ green 255.0)
    (/ blue 255.0)
    opacity)))

(defun rgb-red (color)   (round (* 255 (#/redComponent color))))

(defun rgb-green (color) (round (* 255 (#/greenComponent color))))

(defun rgb-blue (color)  (round (* 255 (#/blueComponent color))))

(defun rgb-opacity (color)  (#/alphaComponent color))

