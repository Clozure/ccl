;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2009 Clozure Associates
;;;   Copyright (C) 1994-2001 Digitool, Inc
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

(eval-when (:compile-toplevel :execute)
  (defconstant seconds-in-week (* 60 60 24 7))
  (defconstant weeks-offset 2145)
  (defconstant seconds-offset 432000)
  (defconstant minutes-per-day (* 24 60))
  (defconstant quarter-days-per-year (1+ (* 365 4)))
  (defconstant quarter-days-per-century 146097)
  (defconstant november-17-1858 678882)
  (defconstant weekday-november-17-1858 2)
)

(defun gctime ()
  (let* ((timeval-size (record-length :timeval)))
    (%stack-block ((copy (* timeval-size 5)))
      (#_memmove copy *total-gc-microseconds* (* timeval-size 5))
      (macrolet ((funk (arg)
                   (ecase internal-time-units-per-second 
                    (1000000 `(timeval->microseconds ,arg))
                    (1000 `(timeval->milliseconds ,arg)))))
        (values
         (funk copy)
         (funk (%incf-ptr copy timeval-size))
         (funk (%incf-ptr copy timeval-size))
         (funk (%incf-ptr copy timeval-size))
         (funk (%incf-ptr copy timeval-size)))))))




;;; This should stop using #_localtime_r: not all times can be represented
;;; as a signed natural offset from the start of Unix time.
;;; For now, if the time won't fit in a :time_t, use an arbitrary time
;;; value to get the time zone and assume that DST was -not- in effect.
#-windows-target
(defun get-timezone (time)
  (let* ((toobig (not (typep time '(signed-byte
                                    #+32-bit-target 32
                                    #+64-bit-target 64)))))
    (when toobig
      (setq time 0))
    (rlet ((when :time_t)
           (tm :tm))
      (setf (pref when :time_t) time)
      (with-macptrs ((ltm (#_localtime_r when tm)))
        (if (%null-ptr-p ltm)
          (values 0 nil)
          (progn
            (values (floor #-solaris-target (pref tm :tm.tm_gmtoff)
                           #+solaris-target #&altzone
                           -60)
                    (unless toobig (not (zerop (pref tm :tm.tm_isdst)))))))))))

#+windows-target
(defun get-timezone (time)
  (declare (ignore time))
  (rlet ((tzinfo #>TIME_ZONE_INFORMATION))
    (let* ((id (#_GetTimeZoneInformation tzinfo))
           (minutes-west (pref tzinfo #>TIME_ZONE_INFORMATION.Bias))
           (is-dst (= id #$TIME_ZONE_ID_DAYLIGHT)))
      (values (floor (+ minutes-west
                        (if is-dst
                          (pref tzinfo #>TIME_ZONE_INFORMATION.DaylightBias)
                          0)))
              is-dst))))



(defun decode-universal-time (universal-time &optional time-zone)
  "Converts a universal-time to decoded time format returning the following
   nine values: second, minute, hour, date, month, year, day of week (0 =
   Monday), T (daylight savings time) or NIL (standard time), and timezone.
   Completely ignores daylight-savings-time when time-zone is supplied."
  (let* ((daylight nil)
         (timezone (if (null time-zone)
                     (multiple-value-bind
                         (minwest dst)
                         (get-timezone (- universal-time
                                          unix-to-universal-time))
                       (declare (fixnum minwest))
                       (setf daylight dst)
                       (the fixnum (* minwest 60)))
                     (* time-zone 60 60))))
    (declare (fixnum timezone))
    (multiple-value-bind (weeks secs)
        (truncate (+ (- universal-time timezone) seconds-offset)
                  seconds-in-week)
      (let* ((weeks (+ weeks weeks-offset))
             (second NIL)
             (minute NIL)
             (hour NIL)
             (date NIL)
             (month NIL)
             (year NIL)
             (day NIL))
        (multiple-value-bind (t1 seconds) (truncate secs 60)
          (setq second seconds)
          (let* ((tday (truncate t1 minutes-per-day)))
            (multiple-value-setq (hour minute)
              (truncate (- t1 (* tday minutes-per-day)) 60))
            (let* ((t2 (1- (* (+ (* weeks 7) tday november-17-1858) 4)))
                   (tcent (truncate t2 quarter-days-per-century)))
              (setq t2 (mod t2 quarter-days-per-century))
              (setq t2 (+ (- t2 (mod t2 4)) 3))
              (setq year (+ (* tcent 100) (truncate t2 quarter-days-per-year)))
              (let ((days-since-mar0 (1+ (truncate (mod t2 quarter-days-per-year)
                                                   4))))
                (setq day (mod (+ tday weekday-november-17-1858) 7))
                (let ((t3 (+ (* days-since-mar0 5) 456)))
                  (cond ((>= t3 1989)
                         (setq t3 (- t3 1836))
                         (setq year (1+ year))))
                  (multiple-value-setq (month t3) (truncate t3 153))
                  (setq date (1+ (truncate t3 5))))))))
        (values second minute hour date month year day
                daylight
                (if daylight
		  (1+ (/ timezone 60 60))
		  (/ timezone 60 60)))))))

(defun get-decoded-time ()
  "Return nine values specifying the current time as follows:
   second, minute, hour, date, month, year, day of week (0 = Monday), T
   (daylight savings times) or NIL (standard time), and timezone."
  (decode-universal-time (get-universal-time)))

(defun current-year ()
  (nth-value 5 (get-decoded-time)))

(defun leap-years-before (year)
  (let ((years (- year 1901)))
    (+ (- (truncate years 4)
	  (truncate years 100))
       (truncate (+ years 300) 400))))

(defvar *days-before-month*
  (let* ((results (list nil)))
    (let ((sum 0))
      (dolist (days-per-month '(31 28 31 30 31 30 31 31 30 31 30 31))
	(push sum results)
	(incf sum days-per-month)))
    (coerce (nreverse results) 'vector)))

(defun check-valid-date (year month day)
  (declare (fixnum year month day))
  (let* ((limit (if (and (eql month 2)
                         (not (logtest year 3))
                         (or (eql 0 (mod year 400))
                             (not (eql 0 (mod year 100)))))
                  29
                  (svref #(31 28 31 30 31 30 31 31 30 31 30 31) (1- month)))))
    (declare (fixnum limit))
    (unless (<= day limit)
      (report-bad-arg day `(integer 1 ,limit)))))

(defun encode-universal-time (second minute hour date month year
				     &optional (time-zone nil tz-p))
  "The time values specified in decoded format are converted to
   universal time, which is returned."
  (check-type second (mod 60))
  (check-type minute (mod 60))
  (check-type hour (mod 24))
  (check-type date (integer 1 31))
  (check-type month (integer 1 12))
  (check-type year unsigned-byte)
  (when time-zone
    (check-type time-zone (rational -24 24)))
  (locally
      (declare (type (mod 60) second)
               (type (mod 60) minute)
               (type (mod 24) hour)
               (type (integer 1 31) date)
               (type (integer 1 12) month)
               (type unsigned-byte year)
               (type (or null rational) time-zone))
    (when (< year 100)
      (let* ((this (current-year))
             (past (- this 50))
             (future (+ this 49))
             (maybe-past (+ (- past (mod past 100)) year))
             (maybe-future (+ (- future (mod future 100)) year)))
        (if (>= maybe-past past)
          (setq year maybe-past)
          (setq year maybe-future))))
    ;; 12/31/1899 in some time zones might yield a date after
    ;; the start of the epoch in UTC.
    (check-type year (integer 1899))
    (check-valid-date year month date)
    (let* ((days (+ (1- date)
                    (aref *days-before-month* month)
                    (if (> month 2)
                      (leap-years-before (1+ year))
                      (leap-years-before year))
                    (* (- year 1900) 365)))
           (hours (+ hour (* days 24)))
           (result
            (if time-zone
              (+ second (* (+ minute (* (+ hours time-zone) 60)) 60))
              (let* ((minwest-guess
                      (get-timezone (- (* hours 60 60)
                                       unix-to-universal-time)))
                     (guess (+ minute (* hours 60) minwest-guess))
                     (minwest
                      (get-timezone (- (* guess 60)
                                       unix-to-universal-time))))
                (+ second (* (+ guess (- minwest minwest-guess)) 60))))))
      (if (< result 0)
        (error "Universal time for MM/DD/YYYY ~2,'0d/~2,'0d/~4,'0d ~2,'0d:~2,'0d:~2,'0d ~%with ~a time zone would be negative." month date year hour minute second (if tz-p "specified" "current"))
        result))))


#+windows-target
(defun %windows-sleep (millis)
  (do* ((start (floor (get-internal-real-time)
                      (floor internal-time-units-per-second 1000))
               (floor (get-internal-real-time)
                      (floor internal-time-units-per-second 1000)))
        (millis millis (- stop start))
        (stop (+ start millis)))
       ((or (<= millis 0)
            (not (eql (#_SleepEx millis #$true) #$WAIT_IO_COMPLETION))))))

(defun sleep (seconds)
  "This function causes execution to be suspended for N seconds. N may
  be any non-negative, non-complex number."
  (when (minusp seconds) (report-bad-arg seconds '(real 0 *)))
  #-windows-target
  (multiple-value-bind (secs nanos)
      (nanoseconds seconds)
    (%nanosleep secs nanos))
  #+windows-target
  (%windows-sleep (round (* seconds 1000))))


(defun %internal-run-time ()
  ;; Returns user and system times in internal-time-units as multiple values.
  #-windows-target
  (rlet ((usage :rusage))
    (%%rusage usage)
    (let* ((user-seconds (pref usage :rusage.ru_utime.tv_sec))
           (system-seconds (pref usage :rusage.ru_stime.tv_sec))
           (user-micros (pref usage :rusage.ru_utime.tv_usec))
           (system-micros (pref usage :rusage.ru_stime.tv_usec)))
      (values (+ (* user-seconds internal-time-units-per-second)
                 (round user-micros (floor 1000000 internal-time-units-per-second)))
              (+ (* system-seconds internal-time-units-per-second)
                 (round system-micros (floor 1000000 internal-time-units-per-second))))))
  #+windows-target
  (rlet ((start #>FILETIME)
         (end #>FILETIME)
         (kernel #>FILETIME)
         (user #>FILETIME))
    (#_GetProcessTimes (#_GetCurrentProcess) start end kernel user)
    (let* ((user-100ns (dpb (pref user #>FILETIME.dwHighDateTime)
                            (byte 32 32)
                            (pref user #>FILETIME.dwLowDateTime)))
           (kernel-100ns (dpb (pref kernel #>FILETIME.dwHighDateTime)
                            (byte 32 32)
                            (pref kernel #>FILETIME.dwLowDateTime)))
           (convert (floor 10000000 internal-time-units-per-second)))
      (values (floor user-100ns convert) (floor kernel-100ns convert)))))

(defun get-internal-run-time ()
  "Return the run time in the internal time format. (See
  INTERNAL-TIME-UNITS-PER-SECOND.) This is useful for finding CPU usage."
  (multiple-value-bind (user sys) (%internal-run-time)
    (+ user sys)))





      
