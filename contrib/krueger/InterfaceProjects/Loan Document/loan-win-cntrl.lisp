;;; loan-win-cntrl.lisp

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

;;; Sample lisp/Cocoa interface that uses a NIB file defined with interface builder;
;;; A definition is provided for the "SpeechController" class that was specified to interface builder
;;; as the class of the NIB file owner.
;;; We manually create an instance of SpeechController and specify it as the owner for the NIB file.

(defpackage :loan-document
  (:nicknames :lnd)
  (:use :iu :ccl :common-lisp))

(in-package :lnd)

;; The loan-win-controller class

(defclass loan-win-controller (ns:ns-window-controller)
  ((loan :foreign-type :id :accessor loan)
   (orig-date-text :foreign-type :id :accessor orig-date-text)
   (loan-text :foreign-type :id :accessor loan-text)
   (int-text :foreign-type :id :accessor int-text)
   (dur-text :foreign-type :id :accessor dur-text)
   (pay-text :foreign-type :id :accessor pay-text)
   (int-slider :foreign-type :id :accessor int-slider)
   (dur-slider :foreign-type :id :accessor dur-slider)
   (pay-slider :foreign-type :id :accessor pay-slider))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/initWithLoan: :id)
                ((self loan-win-controller) (ln :id))
  (setf (loan self) ln)
  (let* ((nib-name (ccl::%make-nsstring 
                    (namestring (truename "ip:Loan Document;loandoc.nib"))))
         (init-self (#/initWithWindowNibPath:owner: self nib-name self)))
    init-self))

;; Action methods that are called when controls do something

(objc:defmethod (#/buttonPushed: :void) 
                ((self loan-win-controller) (button-matrix :id))
  (with-slots (loan loan-text int-text dur-text pay-text int-slider 
                    dur-slider pay-slider) self
    (let ((cm (#/selectedRow button-matrix)))
      (unless (eql cm (compute-mode loan))
        (case (compute-mode loan)
          (0 (#/setEnabled: loan-text #$YES))
          (1 (#/setEnabled: int-text #$YES)
             (#/setEnabled: int-slider #$YES))
          (2 (#/setEnabled: dur-text #$YES)
             (#/setEnabled: dur-slider #$YES))
          (3 (#/setEnabled: pay-text #$YES)
             (#/setEnabled: pay-slider #$YES)))
        (setf (compute-mode loan) cm)
        (case cm
          (0 (#/setEnabled: loan-text #$NO))
          (1 (#/setEnabled: int-text #$NO)
             (#/setEnabled: int-slider #$NO))
          (2 (#/setEnabled: dur-text #$NO)
             (#/setEnabled: dur-slider #$NO))
          (3 (#/setEnabled: pay-text #$NO)
             (#/setEnabled: pay-slider #$NO)))
        (compute-new-loan-values loan)))))

(objc:defmethod (#/awakeFromNib :void) 
                ((self loan-win-controller))
  (#/setEnabled: (loan-text self) #$NO)
  ;; set the sliders to update continuously so that the text boxes reflect the current value
  ;; Note that we can set this in IB for text boxes, but not, apparently, for sliders
  (#/setContinuous: (int-slider self) #$YES)
  (#/setContinuous: (dur-slider self) #$YES)
  (#/setContinuous: (pay-slider self) #$YES)
  ;; tell the text cells not to handle undo
  (#/setAllowsUndo: (#/cell (loan-text self)) #$NO)
  (#/setAllowsUndo: (#/cell (int-text self)) #$NO)
  (#/setAllowsUndo: (#/cell (dur-text self)) #$NO)
  (#/setAllowsUndo: (#/cell (pay-text self)) #$NO)
  (#/setAllowsUndo: (#/cell (orig-date-text self)) #$NO))

(provide :loan-win-cntrl)
