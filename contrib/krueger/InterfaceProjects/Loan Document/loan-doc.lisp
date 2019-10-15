;; loan-doc.lisp

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :menu-utils)
  (require :decimal)
  (require :date)
  (require :lisp-doc-controller)
  (require :loan-win-cntrl)
  (require :loan-pr-view)
  (require :ns-string-utils))

(defpackage :loan-document
  (:nicknames :lnd)
  (:use :iu :ccl :common-lisp))

(in-package :lnd)

;; demonstration code for creating a Cocoa document class in lisp that can be required
;; and loaded into a standard running CCL IDE (i.e. does not require a stand-alone program).

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

(defclass loan-doc (ns:ns-document)
  ((loan-amount :accessor loan-amount :initform 0)
   (interest-rate :accessor interest-rate :initform 0)
   (loan-duration :accessor loan-duration :initform 0)
   (monthly-payment :accessor monthly-payment :initform 0)
   (origination-date :accessor origination-date :initform (now))
   (first-payment :accessor first-payment :initform (next-month (now)))
   (pay-schedule :accessor pay-schedule :initform nil)
   (compute-mode :accessor compute-mode :initform 0)
   (last-set-param :accessor last-set-param :initform nil)
   (max-dur :foreign-type #>BOOL :accessor max-dur)
   (min-dur :foreign-type #>BOOL :accessor min-dur)
   (min-pay :foreign-type #>BOOL :accessor min-pay))
  (:default-initargs 
    :doc-class nil
    :menu-name nil
    :file-ext nil)
  (:metaclass ns:+ns-object))

(defvar *loan-doc-controller* (make-instance 'lisp-doc-controller
                                :doc-class (find-class 'loan-doc)
                                :menu-class "Loan"
                                :file-ext "loan"))

;; Normal lisp methods for our class

(defmethod get-loan-state ((self loan-doc))
  ;; returns a list of loan state values suitable for use by set-loan-state
  (list (loan-amount self)
        (interest-rate self)
        (loan-duration self)
        (monthly-payment self)
        (origination-date self)
        (first-payment self)
        (max-dur self)
        (min-dur self)
        (min-pay self)))

(defmethod set-loan-state ((self loan-doc) state-list)
  (setf (last-set-param self) nil)
  (#/willChangeValueForKey: self #@"loanAmt")
  (setf (loan-amount self) (pop state-list))
  (#/didChangeValueForKey: self #@"loanAmt")
  (#/willChangeValueForKey: self #@"interestRate")
  (setf (interest-rate self) (pop state-list))
  (#/didChangeValueForKey: self #@"interestRate")
  (#/willChangeValueForKey: self #@"loanDuration")
  (setf  (loan-duration self) (pop state-list))
  (#/didChangeValueForKey: self #@"loanDuration")
  (#/willChangeValueForKey: self #@"monthlyPayment")
  (setf   (monthly-payment self) (pop state-list))
  (#/didChangeValueForKey: self #@"monthlyPayment")
  (#/willChangeValueForKey: self #@"originationDate")
  (setf  (origination-date self) (pop state-list))
  (#/didChangeValueForKey: self #@"originationDate")
  (#/willChangeValueForKey: self #@"firstPayment")
  (setf  (first-payment self) (pop state-list))
  (#/didChangeValueForKey: self #@"firstPayment")
  (#/willChangeValueForKey: self #@"maxDur")
  (setf   (max-dur self) (pop state-list))
  (#/didChangeValueForKey: self #@"maxDur")
  (#/willChangeValueForKey: self #@"minDur")
  (setf   (min-dur self) (pop state-list))
  (#/didChangeValueForKey: self #@"minDur")
  (#/willChangeValueForKey: self #@"minPay")
  (setf   (min-pay self) (pop state-list))
  (#/didChangeValueForKey: self #@"minPay")
  (#/willChangeValueForKey: self #@"totInterest")
  (setf   (pay-schedule self) nil)
  (#/didChangeValueForKey: self #@"totInterest")
  (compute-new-loan-values self))

(defmethod create-undo ((self loan-doc) undo-name &optional (always-undo nil))
  (when (or always-undo (not (eq (last-set-param self) undo-name)))
    (let ((undo (#/undoManager self))
          (st (lisp-object-to-ns-data (get-loan-state self))))
      (#/setLoanState: (#/prepareWithInvocationTarget: undo self)
                       st)
      (unless (#/isUndoing undo)
        (#/setActionName: undo undo-name))
      (setf (last-set-param self) undo-name))))

(defmethod set-pay-schedule ((self loan-doc))
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

(defmethod print-pay-schedule ((self loan-doc) &optional (strm t))
  (format strm 
          "~:{~%On ~a balance = $~$ + interest of $~$ - payment of $~$ = ~a balance of $~$~}"
          (pay-schedule self)))

(defmethod compute-int-rate ((self loan-doc))
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

(defmethod compute-new-loan-values ((self loan-doc))
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
                ((self loan-doc))
  (lisp-to-ns-decimal (loan-amount self)))

(objc:defmethod (#/interestRate :id)
                ((self loan-doc))
  (#/numberWithFloat: ns:ns-number (float (interest-rate self))))

(objc:defmethod (#/loanDuration :id)
                ((self loan-doc))
  (#/numberWithInt: ns:ns-number (loan-duration self)))

(objc:defmethod (#/monthlyPayment :id)
                ((self loan-doc))
  (lisp-to-ns-decimal (monthly-payment self)))

(objc:defmethod (#/originationDate :id)
                ((self loan-doc))
  (lisp-to-ns-date (origination-date self)))

(objc:defmethod (#/firstPayment :id)
                ((self loan-doc))
  (lisp-to-ns-date (first-payment self)))

(objc:defmethod (#/totInterest :id)
                ((self loan-doc))
  (lisp-to-ns-decimal (reduce #'+ (pay-schedule self) 
                              :key #'seventh 
                              :initial-value 0)))

(objc:defmethod (#/setLoanAmt: :void)
                ((self loan-doc) (amt :id))
  (create-undo self #@"set loan amount")
  (setf (loan-amount self) (lisp-from-ns-decimal amt))
  (compute-new-loan-values self))

(objc:defmethod (#/setInterestRate: :void)
                ((self loan-doc) (rate :id))
  (create-undo self #@"set interest rate")
  (setf (interest-rate self) (#/floatValue rate))
  (compute-new-loan-values self))

(objc:defmethod (#/setLoanDuration: :void)
                ((self loan-doc) (dur :id))
  (create-undo self #@"set loan duration")
  (setf (loan-duration self) (#/longValue dur))
  (compute-new-loan-values self))

(objc:defmethod (#/setMonthlyPayment: :void)
                ((self loan-doc) (pay :id))
  (create-undo self #@"set monthly payment")
  (setf (monthly-payment self) (lisp-from-ns-decimal pay))
  (compute-new-loan-values self))

(objc:defmethod (#/setOriginationDate: :void)
                ((self loan-doc) (dt :id))
  (create-undo self #@"set origination date")
  (let ((new-dt (ns-to-lisp-date dt)))
    (setf (origination-date self) new-dt)
    (#/willChangeValueForKey: self #@"firstPayment")
    (setf (first-payment self) (next-month new-dt))
    (#/didChangeValueForKey: self #@"firstPayment"))
  (compute-new-loan-values self))

(objc:defmethod (#/setFirstPayment: :void) 
                ((self loan-doc) (pay :id))
  (create-undo self #@"set first payment date")
  (let ((new-pay (ns-to-lisp-date pay)))
    (setf (first-payment self) new-pay))
  (compute-new-loan-values self))

;; Necessary overrides of NSDocument methods
;; Methods called to read data from and write data to external files

(objc:defmethod (#/readFromData:ofType:error: #>BOOL) 
                ((self loan-doc) (data :id) (dtype :id) (err (:* :id)))
  (declare (ignore err dtype))
  (set-loan-state self (ns-data-to-lisp-object data))
  #$YES)

(objc:defmethod (#/dataOfType:error: :id) 
                ((self loan-doc) (dtype :id) (err (:* :id)))
  (declare (ignore dtype err))
  (lisp-object-to-ns-data (get-loan-state self)))

(objc:defmethod (#/setLoanState: :void)
                ((self loan-doc) (st :id))
  (create-undo self nil t)
  ;; called when user does an "undo" 
  (set-loan-state self (ns-data-to-lisp-object st)))

;; Methods to manage windows

(objc:defmethod (#/makeWindowControllers :void) 
                ((self loan-doc))
  (let ((lwc (make-instance 'loan-win-controller
               :with-loan self)))
    (#/setShouldCloseDocument: lwc #$YES)
    (#/addWindowController: self lwc)))

(objc:defmethod (#/validateMenuItem: #>BOOL) 
                ((self loan-doc) (item :id))
  (let* ((action (#/action item)))
    (cond ((eql action (ccl::@selector #/saveDocument:))
           (#/isDocumentEdited self))
          (t (call-next-method item)))))

(objc:defmethod (#/prepareSavePanel: #>BOOL) 
                ((self loan-doc) (panel :id))
  (#/setRequiredFileType: panel #@"loan")
  #$YES)

;; Printing Support

(objc:defmethod (#/printLoan: :void)
                ((self loan-doc) (sender :id))
  (declare (ignore sender))
  (#/printDocument: self self))

(objc:defmethod (#/printOperationWithSettings:error: :id)
                ((self loan-doc) (settings :id) (err (:* :id)))
  (declare (ignore err settings))
  (let ((pr-view (make-instance 'loan-print-view
                   :loan self)))
    (#/printOperationWithView:printInfo: ns:ns-print-operation 
                                         pr-view 
                                         (#/printInfo self))))
    
(provide :loan-doc)
