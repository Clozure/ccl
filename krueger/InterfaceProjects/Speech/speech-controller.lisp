;;; speech-controller.lisp

#|
The MIT license.

Copyright (c) 2009 Paul L. Krueger

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

(require :NIB)

(defpackage :speech-controller 
  (:nicknames :spc)
  (:use :iu :ccl :common-lisp)
  (:export test-speech))

(in-package :spc)

(defclass speech-controller (ns:ns-object)
  ((speech-text :foreign-type :id :accessor speech-text)
   (button-matrix :foreign-type :id :accessor button-matrix)
   (speech-synth :accessor speech-synth 
                 :initform (make-instance ns:ns-speech-synthesizer))
   (voices :accessor voices 
           :initform (#/retain (#/availableVoices ns:ns-speech-synthesizer)))
   (nib-objects :accessor nib-objects :initform nil))
  (:metaclass ns:+ns-object))

(defmethod initialize-instance :after ((self speech-controller) 
                                       &key &allow-other-keys)
  (setf (nib-objects self)
        (load-nibfile 
         (truename "ip:Speech;SpeechView.nib") 
         :nib-owner self
         :retain-top-objs t))
  ;; get all of the voice strings and set the names of the radio buttons
  (dotimes (i (#/count (voices self)))
    (multiple-value-bind (col row) (floor i 6)
      (#/setTitle: (#/cellAtRow:column: (button-matrix self) row col)
                   (#/objectForKey: 
                    (#/attributesForVoice: ns:ns-speech-synthesizer 
                                           (#/objectAtIndex: (voices self) i)) 
                    #&NSVoiceName))))
  ;; Make sure that the initial voice selected for the speech syntesizer matches 
  ;; the radio button that is selected at startup. To do that we'll just call our
  ;; own buttonPushed: method.
  (#/buttonPushed: self (button-matrix self))
  ;; we do the following so that ccl:terminate will be called before we are
  ;; garbage collected and we can release the top-level objects from the NIB 
  ;; that we retained when loaded
  (ccl:terminate-when-unreachable self))

(defmethod ccl:terminate ((self speech-controller))
  (when (speech-synth self)
    (#/release (speech-synth self)))
  (when (voices self)
    (#/release (voices self)))
  (dolist (top-obj (nib-objects self))
    (unless (eql top-obj (%null-ptr))
      (#/release top-obj))))

;; Action methods called as a result of pushing some button

(objc:defmethod (#/startSpeaking: :void) 
                ((self speech-controller) (s :id))
  (declare (ignore s))
  (with-slots (speech-text speech-synth) self
    (let ((stxt (#/stringValue speech-text)))
      (when (zerop (#/length stxt))
        (setf stxt #@"I have nothing to say"))
      (#/startSpeakingString: speech-synth stxt))))

(objc:defmethod (#/stopSpeaking: :void) 
                ((self speech-controller) (s :id))
  (declare (ignore s))
  (with-slots (speech-synth) self
    (#/stopSpeaking speech-synth)))

(objc:defmethod (#/buttonPushed: :void) 
                ((self speech-controller) (button-matrix :id))
  (let ((row (#/selectedRow button-matrix))
        (col (#/selectedColumn button-matrix)))
    (#/setVoice: (speech-synth self) 
                 (#/objectAtIndex: (voices self) (+ row (* col 6))))))
  
;; test funtion

(defun test-speech ()
  (make-instance 'speech-controller))

(provide :speech-controller)