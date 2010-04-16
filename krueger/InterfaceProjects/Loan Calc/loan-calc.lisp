;;; loan-calc.lisp

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :nib)
  (require :date)
  (require :decimal))

(defpackage :loan-calc
  (:nicknames :lnc)
  (:use :iu :ccl :common-lisp)
  (:export test-loan))

(in-package :lnc)

;; The loan-controller class

(defclass loan-controller (ns:ns-window-controller)
  ((loan :foreign-type :id :accessor loan)
   (loan-text :foreign-type :id :accessor loan-text)
   (int-text :foreign-type :id :accessor int-text)
   (dur-text :foreign-type :id :accessor dur-text)
   (pay-text :foreign-type :id :accessor pay-text)
   (int-slider :foreign-type :id :accessor int-slider)
   (dur-slider :foreign-type :id :accessor dur-slider)
   (pay-slider :foreign-type :id :accessor pay-slider))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/initWithLoan: :id)
                ((self loan-controller) (ln :id))
  (setf (loan self) ln)
  (let* ((nib-name (ccl::%make-nsstring 
                    (namestring (truename "ip:Loan Calc;loan.nib"))))
         (init-self (#/initWithWindowNibPath:owner: self nib-name self)))
    init-self))

;; Action methods that are called when controls do something

(objc:defmethod (#/buttonPushed: :void) 
                ((self loan-controller) (button-matrix :id))
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
                ((self loan-controller))
  (#/setEnabled: (loan-text self) #$NO)
  ;; set the sliders to update continuously so that the text boxes reflect the current value
  ;; Note that we can set this in IB for text boxes, but not, apparently, for sliders
  (#/setContinuous: (int-slider self) #$YES)
  (#/setContinuous: (dur-slider self) #$YES)
  (#/setContinuous: (pay-slider self) #$YES))

(objc:defmethod (#/windowWillClose: :void) 
                ((self loan-controller) (notif :id))
  (declare (ignore notif))
  (when (loan self)
    ;; Tell the loan that the window is closing
    ;; It will #/autorelease this window-controller)
    (window-closed (loan self))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Everything below is related to the loan data model

(defconstant $max-interest-rate$ .5)

;; Some loan utility functions

;; all equations are derived from a basic loan equation for an N month loan:
;; LoanPrinciple = MonthlyPayment * ( (1 / (1 + MonthlyInterest)) + 
;;                                    (1 / (1 + MonthlyInterest)^2) + ... + 
;;                                    (1 / (1 + MonthlyInterest)^N) )
;; which can be manipulated to derive:
;; MonthlyPayment = LoanPrinciple * (MonthlyInterest + (MonthlyInterest / ((1 + MonthlyInterest)^LoanDuration - 1)))

(defun pay-to-loan-ratio (mo-int loan-dur)
  ;; Just computes the MonthlyPayment/Loan ratio from the basic loan equation given above
  (if (zerop mo-int) 0 
    (+ mo-int (/ mo-int (1- (expt (1+ mo-int) loan-dur))))))

;; The loan class

(defclass loan (ns:ns-object)
  ((loan-amount :accessor loan-amount :initform 0)
   (interest-rate :accessor interest-rate :initform 0)
   (loan-duration :accessor loan-duration :initform 0)
   (monthly-payment :accessor monthly-payment :initform 0)
   (origination-date :accessor origination-date :initform (now))
   (first-payment :accessor first-payment :initform (next-month (now)))
   (pay-schedule :accessor pay-schedule :initform nil)
   (window-controller :accessor window-controller :initform nil)
   (compute-mode :accessor compute-mode :initform 0)
   (max-dur :foreign-type #>BOOL :accessor max-dur)
   (min-dur :foreign-type #>BOOL :accessor min-dur)
   (min-pay :foreign-type #>BOOL :accessor min-pay))
  (:metaclass ns:+ns-object))


;; Normal lisp methods for our class

(defmethod initialize-instance :after ((self loan) 
                                       &key &allow-other-keys)
  (setf (window-controller self)
        (make-instance (find-class 'loan-controller)
          :with-loan self))
  (#/showWindow: (window-controller self) self))

(defmethod window-closed ((self loan))
  ;; called by the window-controller to tell us that the window closed
  (when (window-controller self)
    (#/autorelease (window-controller self))
    (setf (window-controller self) nil)))

(defmethod close-loan ((self loan))
  ;; just tell the window to close
  (when (window-controller self)
    (let ((win (#/window (window-controller self))))
      ;; for reasons mysterious to me, calling #/window seems to
      ;; also #/retain the window, so we'll #/release it
      (#/release win)
      (#/performClose: win self))))

(defmethod set-pay-schedule ((self loan))
  ;; create a detailed payment schedule for the loan using daily compounding of interest 
  ;; Payments are on the same date of each month, but the number of days between payments
  ;; varies because the number of days in each month varies.
  ;; We compute accurate interest compounded daily for the actual number of days.
  (let ((monthly-interest (/ (interest-rate self) 12))
        (payment (monthly-payment self))
        (sched nil)
        (display-min-pay-banner nil))
    (prog1
        (do* ((begin (loan-amount self) end)
              (begin-date (first-payment self) end-date)
              (end-date (next-month begin-date) (next-month begin-date))
              (int (round (* begin monthly-interest))
                   (round (* begin monthly-interest)))
              (end (- (+ begin int) payment) (- (+ begin int) payment)))
             ((not (plusp end)) 
              (progn
                (push (list (short-date-string begin-date) 
                            (/ begin 100)
                            (/ int 100)
                            (/ payment 100)
                            (short-date-string end-date) 
                            (/ end 100)
                            int) 
                      sched)
                (setf (pay-schedule self) (nreverse sched))))
          (when (>= end begin)
            ;; oops, with this combination of values the loan will never 
            ;; be paid off, so let's set a minimum payment required
            ;; Display a field that tells user the minimum payment was reached 
            (setf display-min-pay-banner t)
            (#/willChangeValueForKey: self #@"monthlyPayment")
            (setf (monthly-payment self) (1+ int))
            (#/didChangeValueForKey: self #@"monthlyPayment")
            ;; now patch up our loop variables and keep going
            (setf payment (monthly-payment self))
            (setf end (1- begin)))
          ;; put the last payment into the list
          (push (list (short-date-string begin-date) 
                      (/ begin 100)
                      (/ int 100)
                      (/ payment 100)
                      (short-date-string end-date) 
                      (/ end 100)
                      int)
                sched))
      (#/willChangeValueForKey: self #@"totInterest")
      ;; we'll make the total interest field call our accessor 
      ;; to generate a new amount
      (#/didChangeValueForKey: self #@"totInterest")
      (if display-min-pay-banner
        (progn
          ;; Set a condition that says the minimum payment was reached 
          (setf display-min-pay-banner t)
          (#/willChangeValueForKey: self #@"minPay")
          (setf (min-pay self) #$YES)
          (#/didChangeValueForKey: self #@"minPay"))
        (progn
          ;; otherwise reset that condition
          (#/willChangeValueForKey: self #@"minPay")
          (setf (min-pay self) #$NO)
          (#/didChangeValueForKey: self #@"minPay")))
      ;; If we happen to be computing the interest rate, then 
      ;; the combination of loan-amount and monthly payment will
      ;; determine a maximum interest rate. This, in turn, 
      ;; determines a maximum loan duration. If the duration was set
      ;; longer than this by the user, we will reset the 
      ;; lone duration value to the maximum needed.
      ;; If, on the other hand, the monthly payment is set so low that
      ;; the interest rate approaches 0, then we may have to adjust the
      ;; loan duration up to the minimum needed to pay the loan.
      ;; Let's start by resetting our two "duration" conditions and then we'll
      ;; set them if conditions dictate.
      ;; Reset a condition that indicates the max duration was reached 
      (#/willChangeValueForKey: self #@"maxDur")
      (setf (max-dur self) #$NO)
      (#/didChangeValueForKey: self #@"maxDur")
      ;; Reset a condition that indicates the min duration was reached 
      (#/willChangeValueForKey: self #@"minDur")
      (setf (min-dur self) #$NO)
      (#/didChangeValueForKey: self #@"minDur"))
      (let ((duration-diff (- (loan-duration self) (list-length (pay-schedule self)))))
        (unless (or (eql (compute-mode self) 2) (zerop duration-diff))
          ;; i.e. we're not calling this function just to determine the loan duration
          ;; and we have to adjust the loan duration
          (if (plusp duration-diff)
            (progn
              ;; change the loan-duration value to what it must be
              (#/willChangeValueForKey: self #@"loanDuration")
              (setf (loan-duration self) (list-length (pay-schedule self)))
              (#/didChangeValueForKey: self #@"loanDuration")
              (when (> duration-diff 2)
                ;; If we're one-off just fix it and don't post a message
                ;; This can occur almost anytime because of numerical issues
                ;; Display a field that tells user the max duration was reached 
                (#/willChangeValueForKey: self #@"maxDur")
                (setf (max-dur self) #$YES)
                (#/didChangeValueForKey: self #@"maxDur")))
            (progn
              ;; change the oan-duration value to what it must be
              (#/willChangeValueForKey: self #@"loanDuration")
              (setf (loan-duration self) (list-length (pay-schedule self)))
              (#/didChangeValueForKey: self #@"loanDuration")
              (when (< duration-diff -2)
                ;; If we're one-off just fix it and don't post a message
                ;; This can occur almost anytime because of numerical issues
                ;; Display a field that tells user the min duration was reached 
                (#/willChangeValueForKey: self #@"minDur")
                (setf (min-dur self) #$YES)
                (#/didChangeValueForKey: self #@"minDur"))))))))

(defmethod print-pay-schedule ((self loan) &optional (strm t))
  (format strm 
          "~:{~%On ~a balance = $~$ + interest of $~$ - payment of $~$ = ~a balance of $~$~}"
          (pay-schedule self)))

(defmethod compute-int-rate ((self loan))
  ;; Find a monthly interest rate that makes the rest of the values work.
  ;; There isn't an algebraic solution for the interest rate, so let's search for it.
  ;; Find a suitable search range and do a binary search for it. Even for large interest 
  ;; rates the number of search iterations should be minimal.

  (with-slots (loan-amount monthly-payment loan-duration interest-rate) self
  
    ;; First we'll check to see whether the monthly payment is great than the loan amount.
    ;; If so we'll set the interest rate directly so that the loan is paid off in one month.
    ;; This avoids some ugly arithmetic overflow things that can happen when interest rates
    ;; go off the charts
    (let ((max-monthly-rate (/ $max-interest-rate$ 12)))
      (if (>= monthly-payment loan-amount)
        (min max-monthly-rate (1- (/ monthly-payment loan-amount)))
        (let ((imin (max 0 (min max-monthly-rate
                                (/ (- (* monthly-payment loan-duration) loan-amount) 
                                   (* loan-duration loan-amount)))))
              ;; imin is basically a rate that would result in the first month's interest as 
              ;; the average interest paid for all months. Since we know it must be greater 
              ;; than this, we have a guaranteed lower bound. But we cap it at our allowed 
              ;; monthly maximum interest.
              (imax (min max-monthly-rate 
                         (- (/ monthly-payment loan-amount) .000008333)))
              ;; imax is a rate that would result in the first month's interest being 
              ;; minimally smaller than the payment. Since we must pay off in a finite
              ;; duration, this is a guaranteed maximum. We cap it the allowed maximum 
              ;; monthly rate.
              (target-p-l-ratio (/ monthly-payment loan-amount)))
          (unless (>= imax imin)
            (error "Max int = ~8,4f, Min int = ~8,4f" imax imin))
          (do* ((i (/ (+ imin imax) 2) 
                   (/ (+ imin imax) 2))
                (p-l-ratio (pay-to-loan-ratio i loan-duration) 
                           (pay-to-loan-ratio i loan-duration)))
               ((<= (- imax imin) .000001) imax)
            (if (>= target-p-l-ratio p-l-ratio)
              (setf imin i)
              (setf imax i))))))))

(defmethod compute-new-loan-values ((self loan))
  ;; For the sake of expediency we assume monthly componding
  ;; The basic equation governing these computations is 
  (with-slots (compute-mode interest-rate loan-duration monthly-payment 
                            loan-amount pay-schedule) self
    (case compute-mode
      (0
       ;; compute the loan amount
       (unless (or (zerop interest-rate)
                   (zerop loan-duration)
                   (zerop monthly-payment))
         (#/willChangeValueForKey: self #@"loanAmt")
         (setf loan-amount 
               (round (/ monthly-payment 
                         (pay-to-loan-ratio (/ interest-rate 12)
                                            loan-duration))))
         (set-pay-schedule self)
         (#/didChangeValueForKey: self #@"loanAmt")))
      (1
       ;; compute the interest rate
       (unless (or (zerop loan-amount)
                   (zerop loan-duration)
                   (zerop monthly-payment))
         (#/willChangeValueForKey: self #@"interestRate")
         (setf interest-rate 
               (* 12 (/ (floor (* 1000000 (compute-int-rate self)))
                        1000000)))
         (set-pay-schedule self)
         (#/didChangeValueForKey: self #@"interestRate")))
      (2
       ;; compute the loan duration
       (unless (or (zerop interest-rate)
                   (zerop loan-amount)
                   (zerop monthly-payment))
         (#/willChangeValueForKey: self #@"loanDuration")
         (set-pay-schedule self)
         (setf loan-duration
               (list-length pay-schedule))
         (#/didChangeValueForKey: self #@"loanDuration")))
      (3
       ;; compute the monthly payment
       (unless (or (zerop interest-rate)
                   (zerop loan-amount)
                   (zerop loan-duration))
         (#/willChangeValueForKey: self #@"monthlyPayment")
         (setf monthly-payment
               (round (* loan-amount 
                         (pay-to-loan-ratio (/ interest-rate 12) 
                                            loan-duration))))
         (set-pay-schedule self)
         (#/didChangeValueForKey: self #@"monthlyPayment"))))))

;; Accessor functions used by display objects to retrieve and set values as dictated
;; by the bindings we set up in IB

(objc:defmethod (#/loanAmt :id)
                ((self loan))
  (lisp-to-ns-decimal (loan-amount self)))

(objc:defmethod (#/interestRate :id)
                ((self loan))
  (#/numberWithFloat: ns:ns-number (float (interest-rate self))))

(objc:defmethod (#/loanDuration :id)
                ((self loan))
  (#/numberWithInt: ns:ns-number (loan-duration self)))

(objc:defmethod (#/monthlyPayment :id)
                ((self loan))
  (lisp-to-ns-decimal (monthly-payment self)))

(objc:defmethod (#/originationDate :id)
                ((self loan))
  (lisp-to-ns-date (origination-date self)))

(objc:defmethod (#/firstPayment :id)
                ((self loan))
  (lisp-to-ns-date (first-payment self)))

(objc:defmethod (#/totInterest :id)
                ((self loan))
  (lisp-to-ns-decimal (reduce #'+ (pay-schedule self) 
                              :key #'seventh 
                              :initial-value 0)))

(objc:defmethod (#/setLoanAmt: :void)
                ((self loan) (amt :id))
  (setf (loan-amount self) (lisp-from-ns-decimal amt))
  (compute-new-loan-values self))

(objc:defmethod (#/setInterestRate: :void)
                ((self loan) (rate :id))
  (setf (interest-rate self) (#/floatValue rate))
  (compute-new-loan-values self))

(objc:defmethod (#/setLoanDuration: :void)
                ((self loan) (dur :id))
  (setf (loan-duration self) (#/longValue dur))
  (compute-new-loan-values self))

(objc:defmethod (#/setMonthlyPayment: :void)
                ((self loan) (pay :id))
  (setf (monthly-payment self) (lisp-from-ns-decimal pay))
  (compute-new-loan-values self))

(objc:defmethod (#/setOriginationDate: :void)
                ((self loan) (dt :id))
  (let ((new-dt (ns-to-lisp-date dt)))
    (setf (origination-date self) new-dt)
    (#/willChangeValueForKey: self #@"firstPayment")
    (setf (first-payment self) (next-month new-dt))
    (#/didChangeValueForKey: self #@"firstPayment"))
  (compute-new-loan-values self))

(objc:defmethod (#/setFirstPayment: :void) 
                ((self loan) (pay :id))
  (let ((new-pay (ns-to-lisp-date pay)))
    (setf (first-payment self) new-pay))
  (compute-new-loan-values self))



;; test by
(defun test-loan ()
  ;; up to caller to #/release the returned loan instance
  ;; but only after window is closed or crash will occur
  (make-instance 'loan))

(provide :loan-calc)