;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Sep 20 09:45:15 2003
;;;; Contains: Miscellaneous tests

;;;
;;; This file contains odds-and-ends, mostly tests that came up as
;;; bug-stimulators in various implementations.
;;;

(in-package :cl-test)

(declaim (special *s1* *s2* *s3* *s4* *s5* *s6* *s7* *s8*))

(deftest misc.1
  (funcall
   (compile nil '(lambda (b)
                   (declare (type (integer 8 22337) b))
                   (+ b 2607688420)))
   100)
  2607688520)

(deftest misc.2
  (funcall (compile nil
                    '(lambda (b) (integer-length (dpb b (byte 4 28) -1005))))
           12800263)
  32)

(deftest misc.3
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (optimize (speed 3) (debug 1)))
       (let ((v7
              (let ((v2 (block b5 (return-from b5 (if t b -4)))))
                a)))
         -65667836)))
   1 2)
  -65667836)

(deftest misc.4
  (funcall
   (compile
    nil
    '(lambda (a b c)
           (declare (type (integer -629491 -333) a)
                    (type (integer -142 1) b)
                    (type (integer 0 12604) c)
                    (optimize (speed 3) (safety 1) (debug 1)))
           (let ((v6 (block b7 (return-from b7 (if (eql b 0) 1358159 a)))))
                b)))
   -1000 -17 6143)
  -17)

(deftest misc.5
  (funcall
   (compile nil
            '(lambda () (* 390 (- (signum (logeqv -8005440 -2310))
                                  -10604863)))))
  4135896180)

(deftest misc.6
  (funcall
   (compile nil
            '(lambda (a c)
                     (declare (optimize (speed 3) (debug 1)))
                     (flet ((%f14 () (if c a -486826646)))
                           (let ((v7 (flet ((%f18 () (%f14))) a)))
                                (let ((v5 (%f14)))
                                     0)))))
   10 20)
  0)

(deftest misc.7
  (funcall (compile nil
                    '(lambda (c) (declare (optimize (speed 3) (debug 1)))
                       (flet ((%f18 () -36))
                         (flet ((%f13 ()
                                      (let () (block b8 (return-from b8 c)))))
                           (%f18)))))
           10)
  -36)

(deftest misc.8
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (optimize (speed 3) (debug 1)))
       (let ((v3 (flet ((%f12 () (min b (block b2 (return-from b2 a)))))
                       a)))
            (block b7
                   (flet ((%f5 () (return-from b7 b)))
                         (%f5))))))
   10 20)
  20)

(deftest misc.9
  (funcall
   (compile
    nil
    '(lambda ()
             (declare (optimize (speed 3) (debug 1)))
             (block b6
                    (flet ((%f3 ()
                                (ldb (byte 19 23)
                                     (block b1
                                            (let ()
                                                 (- (if nil (return-from b6 89627)
                                                        1160)
                                                    (return-from b1 22923)))))))
                          1)))))
  1)

(deftest misc.10
  (funcall
   (compile
    nil
    '(lambda (c)
             (declare (optimize (speed 3) (debug 1))
                      (type (integer -15417757 5816) c))
             (flet ((%f3 () (if nil -3143 c)))
                   (block b5
                          (let ((v7 (if (< 23613642 (%f3)) c -23097977)))
                               (let ((v5
                                      (return-from b5
                                                   (if (eql c v7)
                                                       (let ((v6 (%f3))) 4650813)
                                                       782))))
                                    -4362540))))))
   -10000)
  782)

(deftest misc.11
  (funcall
   (compile
    nil
    '(lambda (a b c)
             (declare (optimize (speed 3) (debug 1)))
             (block b8
                    (logxor
                     (let ((v3 (return-from b8 120789657))) 3690)
                     (block b2
                            (flet ((%f9 ()
                                        (flet ((%f10 () -1))
                                              c)))
                                  (flet ((%f3 () (let () (return-from b2 b))))
                                        a)))))))
   1 2 3)
  120789657)

(deftest misc.12
  (funcall
   (compile
    nil
    '(lambda (c)
             (declare (optimize (speed 3) (safety 1) (debug 1))
                      (type (integer -171067 -5) c))
             (flet ((%f16 ()
                          (flet ((%f12 () 439))
                                3358)))
                   (flet ((%f14 () c))
                         (if (%f14) -1 (%f14))))))
   -100)
  -1)

(deftest misc.13
  (funcall
   (compile
    nil
    '(lambda (b c)
             (declare (optimize (speed 3) (safety 1) (debug 1))
                      (type  (integer -1554410 36086789) b)
                      (type (integer -15033876209 126774299) c)
                      )
             (block b3
                    (flet ((%f9 ()
                                (abs
                                 (flet ((%f5 ()
                                             (return-from b3 -2)))
                                       (if (if (<= 1 c) b (%f5)) -65 -47895812)))))
                          (min
                           (let ((v3 (let ((v8 (%f9))) b))) b)
                           (if (= 1364001 (%f9))
                               (logeqv (block b5 -2713) -247)
                               -19))))))
   0 0)
  -2)

(deftest misc.14
  (funcall
   (compile
    nil
    '(lambda (c)
             (declare (notinline logandc1))
             (block b6
                    (flet ((%f17 () (return-from b6 c)))
                          (logandc1 (%f17)
                                    (if 1
                                        450967818
                                        (let ((v1 (%f17))) -17)))))))
   10)
  10)

(deftest misc.15
  (funcall
   (compile
    nil
    '(lambda (a b)
             (declare (optimize (speed 3) (safety 1) (debug 1)))
             (flet ((%f6 () a))
                   (block b5
                          (flet ((%f14 ()
                                       (min 17593 (block b1 (return-from b1 b)))))
                                (block b7 (if (%f6) (return-from b7 28182012)
                                              (return-from b5 0))))))))
   3 5)
  28182012)

(deftest misc.16
  (funcall
   (compile
    nil
    '(lambda (a c)
             (flet ((%f14 ()
                          (block b6
                                 (flet ((%f7 () (return-from b6 4)))
                                       (if 587793 (if (%f7) c -23086423) (%f7))))))
                   (block b1
                          (flet ((%f18 () a))
                                (logandc1 (return-from b1 -2781)
                                          (if (%f14) 58647578 -396746)))))))
   1 2)
  -2781)

(deftest misc.17
  (funcall
   (compile
    nil
    '(lambda (a b c)
             (declare (optimize (speed 3) (safety 1) (debug 1))
                      (type (integer 4 23363) b)
                      (type (integer -32681 41648) c)
                      )
             (flet ((%f18 ()
                          (if nil c b)))
                   (if (if (> -71810514 a) 102077 465393)
                       (block b3 (if (%f18) (return-from b3 c) c))
                       (%f18)))))
   0 10 1000)
  1000)

(deftest misc.18
  (funcall
   (compile
    nil
    '(lambda (a b c)
             (declare (optimize (speed 3) (safety 1) (debug 1))
                      (type (integer 7 58010860) a)
                      (type (integer -3573280 -1) b)
                      (type (integer -920848 -819) c)
                      )
             (flet ((%f15 () (if (logbitp 5 a) a c)))
                   (min (if (%f15) b -39) (if (> 0 -14756) b (%f15))))))
   8 -1000 -10000)
  -1000)

(deftest misc.19
  (funcall
   (compile
    nil
    '(lambda (a b c)
             (declare (type (integer 54 3862515) a) (type (integer -961325 1539) b)
                      (type (integer 6 31455) c) (ignorable a b c)
                      (optimize (speed 3) (safety 1) (debug 1)))
             (lognor
              (flet ((%f13 () b)) (%f13))
              (flet ((%f1 () (return-from %f1 a)))
                    (labels ((%f3 () (%f1)))
                            -428)))))
   100 0 200)
  427)

(deftest misc.20
  (funcall
   (compile
    nil
    '(lambda (a b c)
             (declare (type (integer -1 31880308) a)
                      (type (integer -11374222037 5331202966) b)
                      (type (integer -483 -1) c)
                      (ignorable a b c)
                      (optimize (speed 3) (safety 1) (debug 1)))
             (labels ((%f6 () a))
                     (if (eql (let ((v9 (%f6))) -50072824) c)
                      28146341
                      (if (< 119937 21304962) 21304962 (%f6))))))
   0 0 -1)
  21304962)

(deftest misc.21
  (funcall
   (compile
    nil
    '(lambda (a b c)
             (declare (type (integer 398 3955) a) (type (integer 233 464963) b)
                      (type (integer -124477 16) c) (ignorable a b c)
                      (optimize (speed 3) (safety 1) (debug 1)))
             (logior
              (flet ((%f18 ()
                           -3584768))
                    (%f18))
              (flet ((%f1 ()
                          (return-from %f1 c)))
                    (flet ((%f9 ()
                                (if (%f1) 24181 7)))
                          56048)))))
   400 300 0)
  -3547152)

(deftest misc.22
  (funcall
   (compile
    nil
    '(lambda (a b c)
             (declare (type (integer -126378 -103) a)
                      (type (integer -1158604975 1) b)
                      (type (integer 502 28036) c)
                      (ignorable a b c)
                      (optimize (speed 3) (safety 1) (debug 1)))
             (labels ((%f13 () c))
                     (labels ((%f3 ()
                                   (logandc1
                                    c
                                    (block b6
                                           (max -73100
                                                (if b (return-from b6 4935) (%f13)))))))
                             (%f13)))))
   -200 0 1000)
  1000)

(deftest misc.23
  (funcall
   (compile
    nil
    '(lambda (a b c)
             (declare (type (integer 1 18911480) a)
                      (type (integer -1 48333) b)
                      (type (integer -3881001767 -1937357) c)
                      (ignorable a b c)
                      (optimize (speed 3) (safety 1) (debug 1)))
             (labels ((%f10 () c))
                     (block b7
                            (logorc2
                             (* (%f10)
                                (if (ldb-test (byte 27 1) -11337)
                                    (return-from b7 -2)
                                    246137101))
                             (min (%f10) (return-from b7 -76114)))))))
   1 0 -2000000)
  -2)

(deftest misc.24
  (funcall
   (compile
    nil
    '(lambda (a b c)
             (declare (type (integer -1477249397 -10697252) a)
                      (type (integer -7 54591) b)
                      (type (integer -102559556 15) c)
                      (ignorable a b c)
                      (optimize (speed 3) (safety 1) (debug 1)))
             (block b8
                    (let ((v1 (return-from b8 a)))
                         (1+
                          (block b3
                                 (flet ((%f10 ()
                                              (min a (return-from b3 -1))))
                                       16776220)))))))
   -11000000 0 0)
  -11000000)

(deftest misc.25
  (funcall
   (compile
    nil
    '(lambda (a b c)
             (declare (type (integer -944 111244) a)
                      (type (integer 100512 3286178) b)
                      (type (integer -2170236 -107) c)
                      (ignorable a b c)
                      (optimize (speed 3) (safety 1) (debug 1)))
             (labels ((%f17 () c))
                     (labels ((%f16 () a))
                             (if (if (logbitp 10 1029643) t 355)
                                 (if (equal (%f17) b) c a)
                                 (if (= 1325844 (%f16)) -50285 (1- (%f17))))))))
   0 200000 -200)
  0)

(deftest misc.26
  (funcall
   (compile
    nil
    '(lambda (c)
             (declare (optimize speed))
             (block b5
                    (if (logbitp 6 -97)
                        (let ((v2 (block b8 -42484))) c)
                        (flet ((%f10 () (return-from b5 -785143)))
                              (let ((v3 (%f10)))
                                   (%f10)))))))
   0)
  -785143)

(deftest misc.27
  (funcall
   (compile
    nil
    '(lambda (a b c)
             (declare (optimize (speed 3) (debug 1)))
             (labels ((%f14 () c))
                     (logand (%f14)
                             (labels ((%f15 () (logeqv (let ((v1 b)) c)
                                                       (return-from %f15 -1740))))
                                     (labels ((%f8 () (%f15)))
                                             a))))))
   5 2 3)
  1)

(deftest misc.28
  (funcall
   (compile
    nil
    '(lambda (a b c)
          (declare
           (type (integer 1948 12024) b)
           (type (integer -104357939 -252) c)
           (optimize (speed 3) (debug 1)))
    (flet ((%f18 () c))
      (logandc1 (if (eql b (%f18)) 0 a)
                (if (ldb-test (byte 30 30) 1) (%f18) 1)
                ))))
   0 2000 -300)
  1)

(deftest misc.29
  (funcall
   (compile
    nil
    '(lambda (a b c)
             (declare (type (integer 661607 10451683348) a)
                      (type (integer -2 -2) b)
                      (type (integer 5996117 18803237) c)
                      (optimize (speed 3) (safety 1) (debug 1)))
             (labels ((%f16 () -29))
               (flet ((%f7 ()
                           (labels ((%f1 () a))
                             (let ()
                               (block b3
                                 (if 37101207
                                     (return-from b3 -5322045)
                                   (let ((v5 b))
                                     146099574)))))))
                 (if (%f16) c c)))))
   1000000 -2 6000000)
  6000000)

(deftest misc.30
  (funcall
   (compile
    nil
    '(lambda (c)
             (declare (type (integer -253 -1) c)
                      (optimize (speed 3) (safety 1) (debug 1)))
             (flet ((%f8 () c))
                   (if (= (%f8) 481) (%f8) 1779465))))
   -100)
  1779465)

(deftest misc.31
  (funcall
   (compile nil
            '(lambda () (let ((v9 (labels ((%f13 () nil)) nil)))
                             (let ((v3 (logandc2 97 3)))
                                  (* v3 (- 37391897 (logand v3 -66))))))))
  3589619040)

(deftest misc.32
  (funcall
   (compile
    nil
    '(lambda (a d)
             (declare (type (integer -8507 26755) a)
                      (type (integer -393314538 2084485) d)
                      (optimize (speed 3) (safety 1) (debug 1)))
             (gcd
              (if (= 0 a) 10 (abs -1))
              (logxor -1
                      (min -7580
                           (max (logand a 31365125) d))))))
   1 1)
  1)

(deftest misc.33
  (funcall
   (compile
    nil
    '(lambda (a b c d)
             (declare (type (integer 240 100434465) a)
                      (optimize (speed 3) (safety 1) (debug 1)))
             (logxor
              (if (ldb-test (byte 27 4) d)
                  -1
                  (max 55546856 -431))
              (logorc2
               (if (>= 0 b)
                   (if (> b c) (logandc2 c d) (if (> d 224002) 0 d))
                   (signum (logior c b)))
               (logior a -1)))))
   256 0 0 0)
  55546856)

(deftest misc.34
  (funcall
   (compile nil
            `(lambda (b c)
                     (declare (type (integer -23228343 2) b)
                              (type (integer -115581022 512244512) c)
                              (optimize (speed 3) (safety 1) (debug 1)))
                     (* (* (logorc2 3 (deposit-field 4667947 (byte 14 26) b))
                           (deposit-field b (byte 25 27) -30424886))
                        (dpb b (byte 23 29) c)
                        )))
   0 0)
  0)

(deftest misc.35
  (funcall
   (compile
    nil
    '(lambda (c)
             (declare (type (integer -5945502333 12668542) c)
                      (optimize (speed 3)))
             (let ((v2 (* c 12)))
                  (- (max (if (/= 109335113 v2) -26479 v2)
                          (deposit-field 311
                                         (byte 14 28)
                                         (min (max 521326 c) -51)))))))
   12668542)
  26479)

(deftest misc.36
  (funcall
   (compile nil
            '(lambda ()
               (declare (notinline + logand)
                        (optimize (speed 0)))
               (logand
                (block b5
                       (flet ((%f1 ()
                                   (return-from b5 -220)))
                             (let ((v7 (%f1)))
                                  (+ 359749 35728422))))
                -24076))))
  -24284)

(deftest misc.37
  (funcall
   (compile
    nil
    '(lambda (b)
       (declare (notinline -) (optimize (speed 0)))
       (- (block b4
                 (flet ((%f4 ()
                             (return-from b4 b)))
                       (%f4))))))
   10)
  -10)

(deftest misc.38
  (funcall
   (compile
    nil
    '(lambda (x) (declare (type (integer 0 100) x)
                          (optimize (speed 3) (safety 1)))
       (logandc1 x x)))
   79)
  0)

(deftest misc.39
  (funcall
   (compile
    nil
    '(lambda (x) (declare (type (integer 0 100) x)
                          (optimize (speed 3) (safety 1)))
       (logandc2 x x)))
   79)
  0)

(deftest misc.40
  (funcall
   (compile
    nil
    '(lambda (x) (declare (type (integer 0 100) x)
                          (optimize (speed 3) (safety 1)))
       (logorc1 x x)))
   79)
  -1)

(deftest misc.41
  (funcall
   (compile
    nil
    '(lambda (x) (declare (type (integer 0 100) x)
                          (optimize (speed 3) (safety 1)))
       (logorc2 x x)))
   79)
  -1)

(deftest misc.42
  (funcall
   (compile
    nil
    '(lambda (x)
       (declare (type (integer -100 100) x))
       (ldb (byte 1 32) x)))
   -1)
  1)

(deftest misc.43
  (funcall (compile nil
        '(lambda () (flet ((%f2 () 288213285))
                      (+ (%f2) (* 13 (%f2)))))))
  4034985990)
  

(deftest misc.44
  (funcall
   (compile
    nil
    '(lambda (a)
             (declare (type (integer -917858 964754309) a)
                      (optimize (speed 3)))
             (* 25 (min (max a 171625820) 171626138))))
   861929141)
  4290653450)

(deftest misc.45
  (funcall
   (compile
    nil
    '(lambda (b)
             (declare (type (integer 21 9673) b)
                      (optimize (speed 3)))
             (* (integer-length -198435631) (+ b 137206182))))
   6027)
  3841941852)

(deftest misc.46
  (funcall
   (compile
    nil
    '(lambda (b c)
             (declare (type (integer 0 1) b) (optimize (speed 3)))
             (flet ((%f2 () (lognor (block b5 138) c)))
                   (if (not (or (= -67399 b) b))
                       (deposit-field (%f2) (byte 11 8) -3)
                       c))))
   0 0)
  0)

(deftest misc.47
  (funcall
   (compile
    nil
    '(lambda (a)
             (declare (type (integer -4005718822 -50081775) a)
                      (optimize (speed 3) (safety 1) (debug 1)))
             (lognor (ash a (min 0 a)) a)))
   -2878148992)
  0)

(deftest misc.48
  (funcall
   (compile
    nil
    '(lambda (a) (declare (notinline ash min)) (lognor (ash a (min 0 a)) a)))
   -2878148992)
  0)

(deftest misc.49
  (let ((body '(truncate (logorc1 -996082 C) -2))
        (arg 25337234))
    (values
     (funcall (compile nil `(lambda (c) ,body)) arg)
     (funcall (compile nil `(lambda (c) (declare (notinline truncate))
                              ,body)) arg)))
  -13099001
  -13099001)

(deftest misc.50
  (funcall (compile nil `(lambda (c)
                           (declare (optimize (speed 3))
                                    (type (integer 23062188 149459656) c))
                           (mod c (min -2 0))))
           95019853)
  -1)

(deftest misc.51
  (funcall (compile nil `(lambda (b)
                           (declare (optimize (speed 3))
                                    (type (integer 2 152044363) b))
                            (rem b (min -16 0))))
           108251912)
  8)
  
(deftest misc.53
  (funcall
   (compile nil '(lambda ()
                   (let (x)
                     (block nil
                       (flet ((%f (y z) (if (> y z) (setq x y) (setq x z))))
                         (%f 1 2)
                         (%f (return 14) 2)))
                     x))))
  2)

(deftest misc.54
  (funcall
   (compile nil '(lambda (a c)
                   (declare (type (integer 8 117873977) a)
                            (type (integer -131828754 234037511) c)
                            (optimize (speed 3) (safety 1) (debug 1)))
                   (* (mod (signum a) (max 50 -358301))
                      (* -2320445737132
                         (* (* a (deposit-field a (byte 32 19) a)) c)))))
   11386 165297671)
  -49725654774521915007942373712)

(deftest misc.55
  (funcall
   (compile nil '(lambda (a b c)
                   (declare (type (integer -5498929 389890) a)
                            (type (integer -5029571274946 48793670) b)
                            (type (integer 9221496 260169518304) c)
                            (ignorable a b c)
                            (optimize (speed 3) (safety 1) (debug 1)))
                   (- (mod 1020122 (min -49 -420))
                      (logandc1
                       (block b2
                         (mod c (min -49 (if t (return-from b2 1582) b))))
                       (labels ((%f14 () (mod a (max 76 8))))
                         b)))))
   -1893077 -2965238893954 30902744890)
  2965238894454)

(deftest misc.56
  (funcall
   (compile nil '(lambda (a c)
                   (declare (type (integer -8691408487404 -9) a)
                            (type (integer 266003133 2112105962) c)
                            (optimize (speed 3) (safety 1) (debug 1)))
                   (truncate (max (round a) c) (* (* a a) a))))
   -10 266003133)
  -266003
  133)

(deftest misc.57
  (funcall
   (compile nil '(lambda (a b c)
                         (declare (type (integer -1907 58388940297) a)
                                  (type (integer -646968358 294016) b)
                                  (type (integer -708435313 89383896) c)
                                  (optimize (speed 3) (safety 1) (debug 1)))
                         (let ((v6 (abs (min a (signum c)))))
                              (if (ceiling v6 (max 77 v6)) b 2))))
   50005747335 -363030456 17382819)
  -363030456)

(deftest misc.58
  (funcall
   (compile nil '(lambda (a)
                   (declare (type (integer -23 66141285) a)
                            (optimize (speed 3)))
                   (logorc2 (setq a 35191330) (* a 107))))
   4099241)
  -3764388885)

(deftest misc.59
  (funcall
   (compile nil '(lambda (a b c)
                   (declare (type (integer -3966039360 -879349) a)
                            (type (integer -62642199164 -8993827395) b)
                            (type (integer -8065934654337 223) c)
                            (optimize (speed 3) (safety 1) (debug 1)))
                   (floor (* (ceiling c) c)
                          (max 78 (* b (* a (* a b)))))))
   -1000000 -10000000000 0)
  0 0)

(deftest misc.60
  (funcall
    (compile nil
             '(lambda ()
                (let ((v5 46660))
                  (setq v5 (signum (rem v5 (max 53 v5))))))))
  0)

(deftest misc.61
  (progn
    (compile nil
             '(lambda (a b)
                (declare (type (integer -1785799651 -2) a)
                         (type (integer -27 614132331) b)
                         (optimize (speed 3) (safety 1) (debug 1)))
                (ceiling (max (floor -733432 (max 84 -20)) 346)
                         (min -10 (* 17592186028032 (* (* a b) a))))))
    :good)
  :good)

(deftest misc.62
  (funcall (compile nil '(lambda (a)
                (if (and (if a t nil) nil)
                    a
                  (min (block b5 -1) a))))
           100)
  -1)

;;; sbcl bug (probably #233)
(deftest misc.63
  (let* ((form '(flet ((%f12 () (setq c -9868204937)))
                  (if (<= c (%f12)) -2 (if (= c c) b c))))
         (form1 `(lambda (b c)
                   (declare (type (integer -80421740610 1395590616) c))
                   ,form))
         (form2 `(lambda (b c) ,form))
         (vals '(-696742851945 686256271)))
    (eqlt (apply (compile nil form1) vals)
          (apply (compile nil form2) vals)))
  t)

;;; sbcl bug (probably #233)
(deftest misc.64
  (let* ((form '(logcount
                 (if (not (> c (let ((v7 (setq c -246180))) -1)))
                     (ldb (byte 24 11) c)
                   c)))
         (form1 `(lambda (c)
                   (declare (type (integer -256128 207636) c))
                   ,form))
         (form2 `(lambda (c) ,form))
         (vals '(11292))
         )
    (eqlt (apply (compile nil form1) vals)
          (apply (compile nil form2) vals)))
  t)

;;; sbcl bug (probably #233)
(deftest misc.65
  (let ((form1 '(lambda (b c)
                  (declare (type (integer -350684427436 -255912007) b))
                  (logandc2 c (if (< b (setq b -25647585550)) b 0))))
        (form2 '(lambda (b c)
                  (logandc2 c (if (< b (setq b -25647585550)) b 0))))
        (vals '(-297090677547 -20121092)))
    (eqlt (apply (compile nil form1) vals)
          (apply (compile nil form2) vals)))
  t)

(deftest misc.66
  (let* ((form '(if (> a (setq a -2198578292))
                    (min b (if (<= a -14866) a -128363))
                  a))
         (form1 `(lambda (a b)
                   (declare (type (integer -3709231882 0) a))
                   (declare (type (integer -562051054 -1) b))
                   ,form))
         (form2 `(lambda (a b) ,form))
         (vals '(-2095414787 -256985442)))
    (eqlt (apply (compile nil form1) vals)
          (apply (compile nil form2) vals)))
  t)

;;; sbcl/cmucl bug (on sparc)
(deftest misc.67
  (funcall
    (compile nil '(lambda (x)
                    (declare (type (integer 10604862 10604862) x)
                             (optimize speed))
                    (* x 390)))
    10604862)
  4135896180)

;;; cmucl bug (cvs, 10/10/2003)
(deftest misc.68
  (funcall
   (compile nil
            '(lambda (b)
               (flet ((%f8 () (rem b (identity (return-from %f8 0)))))
                 (lognor (%f8) 0))))
   0)
  -1)

(deftest misc.69
  (funcall
   (compile nil
            '(lambda (b)
               (flet ((%f11 () (logorc2 (block b1 (let () (return-from b1 b)))
                                        -1984)))
                 b)))
   0)
  0)

(deftest misc.70
  (funcall
   (compile nil '(lambda (c)
                   (declare (type (integer 46156191457 126998564334) c))
                   (truncate c (min -16 186196583))))
   87723029763)
  -5482689360
  3)

(deftest misc.71
  (funcall
   (compile nil
            '(lambda ()
               (block b8
                 (if (identity (return-from b8 30))
                     1
                   (identity
                    (block b5
                      (labels ((%f10 () (min -52 (return-from b5 10))))
                        20))))))))
  30)

(deftest misc.72
  (funcall
   (compile nil '(lambda ()
                   (flet ((%f13 () (rem 1 (min 0 (return-from %f13 17)))))
                     (%f13)))))
  17)

(deftest misc.73
  (funcall
   (compile nil '(lambda (c)
                   (declare (type (integer 46156191457 126998564334) c))
                   (rem c (min -1 0))))
   87723029763)
  0)

(deftest misc.74
  (funcall (compile nil '(lambda ()
                           (declare (optimize (safety 3) (speed 0) (debug 0)))
                           (ash 6916244 (min 42 -185236061640)))))
  0)

;;; Unwind-protect bug, from sbcl:
;;; "The value NIL is not of type SB-C::NODE."

(deftest misc.75
  (funcall (compile nil '(lambda () (flet ((%f12 () (unwind-protect 1))) 0))))
  0)


;;; cmucl (2003-10-12), "NIL is not of type C::REF"
(deftest misc.76
  (funcall
   (compile nil
            '(lambda (a c)
                (if nil (unwind-protect (max 521739 (unwind-protect c)))
                  (logandc2 3942 a))))
   0 0)
  3942)

;;; gcl (2003-10-11)  Miscomputation of (mod 0 -53) in compiled code
(deftest misc.77
  (funcall (compile nil '(lambda () (mod 0 -53))))
  0)


;;; cmucl (2003-10-12)  "NIL is not of type C::BYTE-LAMBDA-INFO"
(deftest misc.78
  (funcall
   (compile nil '(lambda ()
                   (declare (optimize (speed 0) (debug 0)))
                   (let ((v4
                          (case 227
                            ((-11113 -106126) (unwind-protect 8473))
                            (t 43916))))
                     -12))))
  -12)

;;; Same as misc.78, but with no declarations
;;; In cmucl (2003-10-12)  "NIL is not of type C::ENVIRONMENT"
(deftest misc.79
  (funcall
   (compile nil '(lambda ()
                   (let ((v4
                          (case 227
                            ((-11113 -106126) (unwind-protect 8473))
                            (t 43916))))
                     -12))))
  -12)

(deftest misc.79a
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer 72504 351460) a))
       (declare (type (integer 2383 108330) b))
       (declare        (optimize (speed 2) (space 0) (safety 0)
                          (debug 2) (compilation-speed 1)))
       (if (or (or (/= b 0) (logbitp 0 0)) (logbitp 0 a))
           0
         (funcall (constantly 0) b 0 (catch 'ct4 b)))))
   132318 12238)
  0)


;;; cmucl (2003-10-12) "Invalid number of arguments: 2"
(deftest misc.80
  (funcall
   (compile nil
            '(lambda (b c)
               (declare (notinline > logior imagpart))
               (declare (optimize (speed 0) (debug 0)))
               (labels ((%f16 ()
                              (imagpart
                               (block b3
                                 (logeqv (logior -122516 (if (> -1 0) (return-from b3 c) b))
                                         (return-from %f16 32186310))))))
                 (lognor (%f16) b))))
   -123886 -1656)
  57385)

;;; cmucl (2003-10-12) "NIL is not of type C::REF"
(deftest misc.81
  (funcall
   (compile nil '(lambda (b)
                   (block b7
                     (let ((v3 (return-from b7 b)))
                       (unwind-protect b)))))
   17)
  17)

;;; cmucl (2003-10-12) "The assertion C::SUCC failed"
(deftest misc.82
  (funcall
   (compile nil '(lambda (c)
                   (labels ((%f15 ()
                                  (* (unwind-protect c)
                                     (max -5726369
                                          (return-from %f15 3099206)))))
                     c)))
   0)
  0)

;;; cmucl (2003-10-13) "The assertion (NOT (C::BLOCK-DELETE-P BLOCK)) failed."
(deftest misc.83
  (funcall
   (compile nil '(lambda (a c)
                   (flet ((%f8 () (min c (min a (return-from %f8 c)))))
                     c)))
   0 -10)
  -10)

(deftest misc.84
  (funcall
   (compile nil '(lambda (a b)
                   (flet ((%f18 ()
                                (let ()
                                  (let ()
                                    (if (ldb-test (byte 20 23) b) a
                                      (return-from %f18 431))))))
                     -674)))
   0 0)
  -674)

(deftest misc.85
  (funcall
   (compile nil
            '(lambda (c)
               (labels ((%f14 ()
                              (let ()
                                (logandc1 (min -32 (return-from %f14 -69793))
                                          c))))
                     156)))
   0)
  156)

;;; Two tests showing bug(s) in clisp (2.31)
(deftest misc.86
  (funcall (compile nil '(lambda (b)
                           (flet ((%f10 nil :bad))
                             (let ((v7 (let ((v2 (%f10))) b)))
                               (unwind-protect b)))))
           :good)
  :good)

(deftest misc.87
  (apply (compile nil '(lambda (a b c)
                         (let ((v9 a))
                           (let ((v2 (setq v9 c)))
                             (unwind-protect c)))))
         '(x y z))
  z)

;;; cmucl bug (18e+ 10/15/03)
(deftest misc.88
  (eval '(block b3
           (max (return-from b3 1)
                (if (unwind-protect (unwind-protect 2)) 3 4))))
  1)

;;;
;;; cmucl bug (18e+ 10/15/03)
;;; Also occurs in sbcl (0.8.16.20)
;;;  "Too large to be represented as a SINGLE-FLOAT"
;;; (a large bignum is coerced to a single-float in type propagation,
;;;  with unfortunate results.)
;;;

;;; Here, the function were the problem occurs is -
(deftest misc.89
  (funcall
   (compile nil
            '(lambda (c)
               (declare (type (integer 0 130304) c))
               (- (rem -26 (max 25 (load-time-value 505849129)))
                  (* -15718867961526428520296254978781964 c))))
   0)
  -26)

;;; Here, it is MAX
(deftest misc.89a
  (funcall
   (compile
    nil
    '(lambda (a b c d)
       (declare (type (integer -265115792172 -206231862770) a))
       (declare (type (integer 11069 58322510034) b))
       (declare (type (integer -7351 28730) c))
       (declare (type (integer -913299295156 3670905260104) d))
       (declare (ignorable a b c d))
       (declare
        (optimize (safety 1) (space 1) (compilation-speed 2)
                  (debug 0) (speed 2)))
       (- (signum (catch 'ct6 0)) (numerator (* -1303 d -20527703 d c)))))
   -261283766805 41605749408 5110 1269102278886)
  -220139978315039892599545286437019126040)

;;; Here, it is MOD
(deftest misc.89b
  (funcall
   (compile
    nil
    '(lambda (a b c d)
       (declare (type (integer -481454219025 239286093202) a))
       (declare (type (integer -1121405368785 213522) b))
       (declare (type (integer -103720347879 -241) c))
       (declare (type (integer -12830115357 3027711346) d))
       (declare (ignorable a b c d))
       (declare (optimize (speed 2) (compilation-speed 1) (space 1)
                          (safety 3) (debug 2)))
       (floor (load-time-value 0) (min -18 (* a c b -12626)))))
   -78545446876 -460518205737 -38885914099 1598305189)
  0 0)

;;; acl bugs (version 6.2, linux x86 trial)
(deftest misc.90
  (let* ((form '(- 0 (ignore-errors 20763)
                   (logxor b 1 c -7672794) b))
         (fn1 `(lambda (b c)
                 (declare (type (integer -148895 -46982) b))
                 (declare (type (integer 0 1) c))
                 (declare (optimize (speed 3)))
                 (declare (optimize (safety 1)))
                 ,form))
         (fn2 `(lambda (b c) ,form)))
    (let ((v1 (funcall (compile nil fn1) -76071 0))
          (v2 (funcall (compile nil fn2) -76071 0))
          (v3 (funcall (eval `(function ,fn2)) -76071 0)))
      (if (= v1 v2 v3) :good
        (list v1 v2 v3))))
  :good)

(deftest misc.91
  (let ((fn1 '(lambda ()
                (declare (optimize (speed 3) (safety 1)))
                (ash -10 (min 8 -481))))
        (fn2 '(lambda () (ash -10 (min 8 -481)))))
    (let ((v1 (funcall (compile nil fn1)))
          (v2 (funcall (compile nil fn2)))
          (v3 (funcall (eval `(function ,fn2)))))
      (if (= v1 v2 v3)
          :good
        (list v1 v2 v3))))
  :good)

(deftest misc.92
  (let* ((form '(- -16179207 b (lognor (let () 3) (logxor -17567197 c))))
         (fn1 `(lambda (b c)
                 (declare (type (integer -621 30) c))
                 (declare (optimize (speed 3)))
                 (declare (optimize (safety 1)))
                 ,form))
         (fn2 `(lambda (b c) ,form))
         (vals '(26291532469 -21)))
    (let ((v1 (apply (compile nil fn1) vals))
          (v2 (apply (compile nil fn2) vals))
          (v3 (apply (eval `(function ,fn2)) vals)))
      (if (= v1 v2 v3)
          :good
        (list v1 v2 v3))))
  :good)

(deftest misc.93
  (let* ((form '(ash (1+ (flet ((%f5 (f5-1) c)) c))
                     (min 69 (logxor a b))))
         (fn1 `(lambda (a b c)
                 (declare (type (integer -128 -109) a)
                          (type (integer -2 -1) b)
                          (optimize (speed 3) (safety 1)))
                 ,form))
         (fn2 `(lambda (a b c) ,form))
         (vals '(-123 -1 2590941967601)))
    (eqlt (apply (compile nil fn1) vals)
          (apply (compile nil fn2) vals)))
  t)

(deftest misc.94
  (not (funcall
        (compile nil '(lambda ()
                        (declare (optimize (speed 3) (safety 1) (debug 1)))
                        (<= 268435280
                            (load-time-value
                             39763134374436777607194165739302560271120000))))))
  nil)

(deftest misc.95
  (let* ((form '(+ 272 c (if (< b a) -49618 -29042) b))
         (fn1 `(lambda (a b c)
                 (declare (type (integer -1585918 601848636) a))
                 (declare (type (integer -4 16544323) b))
                 (declare (optimize (speed 3)))
                 (declare (optimize (safety 1)))
                 ,form))
         (fn2 `(lambda (a b c) ,form))
         (vals '(601739317 10891850 17452477960)))
    (let ((v1 (apply (compile nil fn1) vals))
          (v2 (apply (compile nil fn2) vals)))
      (if (eql v1 v2)
          :good
        (list v1 v2))))
  :good)

(deftest misc.96
  (let* ((form '(max 26 (ceiling b (min -8 (max -1 c)))))
         (fn1 `(lambda (b c)
                 (declare (type (integer 482134 96074347505) b))
                 (declare (type (integer -4036 -50) c))
                 (declare (optimize (speed 3)))
                 (declare (optimize (safety 1)))
                 ,form))
         (fn2 `(lambda (b c) ,form))
         (vals '(90244278480 -338)))
    (let ((v1 (apply (compile nil fn1) vals))
          (v2 (apply (compile nil fn2) vals)))
      (if (eql v1 v2)
          :good
        (list v1 v2))))
  :good)

(deftest misc.97
  (let* ((form '(- 349708 (gcd c 0) (logand b b (if (> -8543459 c) 83328 1073))))
         (fn1 `(lambda (b c)
                 (declare (type (integer 301653 329907) b))
                 (declare (type (integer 171971491 1073721279) c))
                 (declare (optimize (speed 3)))
                 (declare (optimize (safety 1)))
                 ,form))
         (fn2 `(lambda (b c) ,form))
         (vals '(321769 1073671227)))
    (let ((v1 (apply (compile nil fn1) vals))
          (v2 (apply (compile nil fn2) vals)))
      (if (eql v1 v2)
          :good
        (list v1 v2))))
  :good)

;;; sbcl bugs (0.8.4.40, x86 linux)

(deftest misc.98
  (funcall (compile nil '(lambda (x)
                           (declare (type (integer -1000000 1000000) x))
                           (logand x x 0)))
           12345)
  0)

(deftest misc.99
  (funcall
   (compile nil '(lambda (a)
                   (declare (type (integer 4303063 101130078) a))
                   (mask-field (byte 18 2) (ash a 77))))
   57132532)
  0)

(deftest misc.100
  (funcall (compile nil '(lambda (c)
                           (declare (type (integer -3924 1001809828) c))
                           (declare (optimize (speed 3)))
                           (min 47 (if (ldb-test (byte 2 14) c)
                                       -570344431
                                     (ignore-errors -732893970)))))
           705347625)
  -570344431)

(deftest misc.101
  (funcall
   (compile nil '(lambda (a c)
                   (declare (type (integer 185501219873 303014665162) a))
                   (declare (type (integer -160758 255724) c))
                   (declare (optimize (speed 3)))
                   (let ((v8
                          (- -554046873252388011622614991634432
                             (ignore-errors c)
                             (unwind-protect 2791485))))
                     (max (ignore-errors a)
                          (let ((v6 (- v8 (restart-case 980))))
                            (min v8 v6))))))
   259448422916 173715)
  259448422916)

(deftest misc.102
  (funcall
   (compile nil '(lambda (b)
                   (declare (type (integer -1598566306 2941) b))
                   (declare (optimize (speed 3)))
                   (max -148949 (ignore-errors b))))
   0)
  0)

(deftest misc.103
  (funcall
   (compile nil '(lambda (a b)
                   (min -80
                        (abs
                         (ignore-errors
                           (+
                            (logeqv b
                                    (block b6
                                      (return-from b6
                                        (load-time-value -6876935))))
                            (if (logbitp 1 a) b (setq a -1522022182249))))))))
   -1802767029877 -12374959963)
  -80)

(deftest misc.104
  (funcall
   (compile nil '(lambda (a) (declare (type (integer 55400028 60748067) a))
                   (lognand 1505 (ash a (let () 40)))))
   58194485)
  -1)

(deftest misc.105
  (funcall
   (compile nil '(lambda (b c)
                   (declare (type (integer -4 -3) c))
                   (block b7
                     (flet ((%f1 (f1-1 f1-2 f1-3)
                                 (if (logbitp 0 (return-from b7
                                                  (- -815145138 f1-2)))
                                     (return-from b7 -2611670)
                                   99345)))
                       (let ((v2 (%f1 -2464 (%f1 -1146 c c) -2)))
                         b)))))
   2950453607 -4)
  -815145134)

;;; Gives the error The value NIL is not of type INTEGER.  (in sbcl 0.8.4.40)

(deftest misc.106
  (progn
    (eval '(defun misc.106-fn (a b c)
             (declare (optimize speed))
             (block b6
               (flet ((%f8 (f8-1 f8-2) b))
                 (%f8 (%f8 c 338) (if t (return-from b6 a) c))))))
    (misc.106-fn -30271 -1 -3043))
  -30271)

 ;;; "The value NIL is not of type SB-C::IR2-LVAR." (sbcl 0.8.4.40)
(deftest misc.107
  (funcall
   (compile nil
            '(lambda (b c)
               (declare (type (integer -29742055786 23602182204) b))
               (declare (type (integer -7409 -2075) c))
               (declare (optimize (speed 3)))
               (floor
                (labels ((%f2 ()
                              (block b6
                                (ignore-errors (return-from b6
                                                 (if (= c 8) b 82674))))))
                  (%f2)))))
   22992834060 -5833)
  82674 0)

;;; "The value NIL is not of type SB-C::IR2-LVAR." (sbcl 0.8.10.15)
(deftest misc.107a
  (funcall
   (compile
    nil
    '(lambda ()
       (declare
        (optimize (speed 2) (space 0) (safety 1)
                  (debug 3) (compilation-speed 0)))
       (flet ((%f14
               (f14-1
                &optional (f14-2 (rationalize (catch 'ct4 0))) (f14-3 0)
                (f14-4 0))
               (dotimes (iv2 0 0) (progn f14-2))))
         (apply #'%f14 0 0 0 nil)))))
  0)

;;; "The value NIL is not of type SB-C::IR2-LVAR." (sbcl 0.8.14.18)

(deftest misc.107b
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (type (integer 7215 1030625885) a))
       (declare (type (integer -4361 -6) b))
       (declare (type (integer -3798210806 -898) c))
       (declare (ignorable a b c))
       (declare
        (optimize (speed 2)
                  (space 2)
                  (safety 2)
                  (debug 3)
                  (compilation-speed 1)))
       (block b4
         (let ((*s7* (cons c 0)))
           (declare (special *s7*))
           (return-from b4
             (prog1 0
               (the integer
                 (integer-length
                  (1+
                   (let ()
                     (gcd (cdr *s7*)
                          (case b
                            ((31 38 20 0 5 45) 2)
                            ((34 35 64 61 47) 39)
                            ((58) a)
                            (t 131788)))))))))))))
   734649164 -3343 -2306504518)
  0)

(deftest misc.107c
  (funcall
   (compile
    nil
    '(lambda (c)
       (declare (optimize (speed 2) (space 1) (safety 1) (debug 3)
                          (compilation-speed 0)))
       (let* ((*s6*
               (unwind-protect 0 (the integer (ash 2914825 (min 8 c))))))
         (declare (special *s6*))
         0)))
   -105)
  0)

(deftest misc.107d
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (optimize (speed 1) (space 1) (safety 1) (debug 3)
                          (compilation-speed 1)))
       (catch 'ct4
         (logorc1
          (the integer
            (case (dotimes (iv2 2 2) (progn 203))
              ((-51) -59598)
              ((-31 -150) a)
              (t b)))
          (throw 'ct4 0)))))
   10 20)
  0)

(deftest misc.107e
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (optimize (speed 1) (space 0) (safety 1)
                          (debug 3) (compilation-speed 1)))
       (flet ((%f11 (&key (key1 (the integer (- a 245241933)))) 0))
         (%f11))))
   1)
  0)

;;; cmucl bug (Argument X is not a NUMBER: NIL)

(deftest misc.108
  (funcall
   (compile nil '(lambda (b)
                   (block b7 (- b (ignore-errors (return-from b7 57876))))))
   10)
  57876)

;;; "The assertion (C::CONSTANT-CONTINUATION-P C::CONT) failed." (cmucl)
(deftest misc.109
  (funcall (compile
            nil
            '(lambda ()
               (load-time-value
                (block b4
                  (* (return-from b4 -27)
                     (block b5
                       (return-from b4
                         (return-from b5
                           (ignore-errors (unwind-protect
                                              (return-from b5 0))))))))))))
  -27)

;;; This bug was occuring a lot in sbcl, and now occurs in cmucl too
;;; NIL fell through ETYPECASE expression.  Wanted one of (C:FIXUP X86::EA C:TN).
(deftest misc.110
  (funcall
   (compile nil
            '(lambda (c)
               (declare (type (integer -1441970837 -427) c))
               (declare (optimize (speed 3)))
               (block b7 (abs (min c (ignore-errors (return-from b7 c)))))))
   -500)
  -500)

;;; In sbcl 0.8.10.14
;;;  NIL fell through ETYPECASE expression.
;;;  Wanted one of (SB-C:FIXUP SB-VM::EA SB-C:TN).

(deftest misc.110a
  (funcall
   (compile
    nil
    '(lambda (a b c d e f)
       (declare (type (integer -1294746569 1640996137) a))
       (declare (type (integer 33628514900 90005963619) b))
       (declare (type (integer -807801310 3) c))
       (declare (type (integer 36607 121946) d))
       (declare (type (integer -6669690514043 -1776180885905) e))
       (declare (type (integer -1472 1979) f))
       (declare (ignorable a b c d e f))
       (declare
        (optimize (speed 3)
                  (space 3)
                  (safety 0)
                  (debug 0)
                  (compilation-speed 3)))
       (catch 'ct7
         (if
             (logbitp 0
                      (if (/= 0 a)
                          c
                        (ignore-errors
                          (progn
                            (if
                                (ldb-test (byte 0 0) (rational (throw 'ct7 0)))
                                0
                              0)
                            0))))
             0
           0))))
   391833530 36648101240 -32785211 91893 -4124561581760 1358)
  0)

;;; CLISP (2.31+) compiler bug

(deftest misc.111
  (funcall
   (compile nil
            '(lambda (a c)
               (if (or (ldb-test (byte 12 18) a)
                       (not (and t (not (if (not (and c t)) nil nil)))))
                   170 -110730)))
   3035465333 1919088834)
  170)

;;; sbcl (0.8.5.8) "The value NIL is not of type SB-C::IR2-LVAR."

(deftest misc.112
  (funcall
   (compile nil '(lambda (a)
                   (declare (type (integer -944 -472) a))
                   (declare (optimize (speed 3)))
                   (round
                    (block b3
                      (return-from b3
                        (if (= 55957 a) -117 (ignore-errors
                                               (return-from b3 a))))))))
   -589)
  -589 0)

;;; sbcl (0.8.5.8) "The value NIL is not of type SB-C::CTRAN"

(deftest misc.113
  (funcall
   (compile nil '(lambda (b c)
                   (if (or (ldb-test (byte 8 10) b) t)
                       c
                     (min (if (<= -6467 c) c 6)
                          (flet ((%f3 (f3-1 f3-2)
                                      f3-1))
                            (multiple-value-call #'%f3 (values b 107)))))))
   -238 -23658556)
  -23658556)

;;; clisp (1 Oct 2003 cvs HEAD)  "*** - CAR: #:G7744659 is not a LIST"

(deftest misc.114
  (funcall
   (compile nil
            '(lambda (a b)
               (unwind-protect
                   (block b2
                     (flet ((%f1 nil b))
                       (logior (if a
                                   (if (ldb-test (byte 23 1) 253966182)
                                       (return-from b2 a)
                                     -103275090)
                                 62410)
                               (if (not (not (if (not nil) t (ldb-test (byte 2 27) 253671809))))
                                   (return-from b2 -22)
                                 (%f1))))))))
   777595384624 -1510893868)
  777595384624)

;;; clisp (1 Oct 2003 cvs HEAD) "Compiler bug!! Occurred in OPTIMIZE-LABEL."

(deftest misc.115
  (funcall
   (compile nil
            '(lambda (a b c)
               (declare (type (integer 0 1000) a b c))
               (if (and (if b (not (and (not (or a t)) nil)) nil)
                        (logbitp 6 c))
                   c b)))
   0 100 600)
  600)

(deftest misc.116
  (funcall
   (compile nil
            '(lambda (a c)
               (declare (type (integer 0 1000) a c))
               (if (if (and (not (and (not (or a t)) nil)) t) c nil)
                   91 -1725615)))
   0 0)
  91)

(deftest misc.117
  (funcall
   (compile nil
            '(lambda (a c)
               (declare (type (integer 0 1000) a c))
               (if (or c (not (or nil (not (and (not (or a t)) nil)))))
                   373146181 115)))
   0 0)
  373146181)

(deftest misc.118
  (funcall
   (compile nil '(lambda (a)
                   (declare (type (integer 0 10000) a))
                   (if (or (or nil (not (or (not (or a nil)) t))) a) a 9376)))
   0)
  0)

(deftest misc.119
  (funcall
   (compile
    nil
    '(lambda ()
       (if (and (if (1+ 0) nil (not (and (not (and (<= 3) nil)) nil)))
                (if (= -31) -20 -2371))
           1493 39720))))
  39720)

(deftest misc.120
  (funcall
   (compile
    nil
    '(lambda (c)
       (declare (type (integer 377036 4184626) c))
       (if (or (and t (not (and (not (and c nil)) nil))) nil)
           3470653 c)))
   1000000)
  3470653)

(deftest misc.121
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (if (and (and -92220 (not (and (not (or c nil)) nil))) a) b b)))
   2000000 150000 -1)
  150000)

;;; CAR: #:G243 is not a LIST
(deftest misc.122
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (type (integer 2872749 5754655) a))
       (declare (type (integer 24114340 89504792) b))
       (declare (type (integer 506491 1412971) c))
       (declare (ignorable a b c))
       (declare (optimize (speed 3)))
       (declare (optimize (safety 1)))
       (declare (optimize (debug 1)))
       (- (let ((v7 (ignore-errors a))) -6)
          (logand (if c -13936 c)
                  (block b3 (if (if (or t b) (not nil) c)
                                (return-from b3 -3114)
                              (ignore-errors 7)
                              ))))))
   3000000 30000000 600000)
  15978)

;;; gcl bug (30 Oct 2003)
(deftest misc.123
  (let* ((fn1 '(lambda (b)
                 (declare (optimize (safety 1)))
                 (labels ((%f7 (f7-1 f7-2)
                               (let ((v2 (setq b 723149855)))
                                 25620)))
                   (max b
                        (multiple-value-call #'%f7 (values b 2))))))
         (fn2 '(lambda (b)
                 (labels ((%f7 (f7-1 f7-2)
                               (let ((v2 (setq b 723149855)))
                                 25620)))
                   (max b
                        (multiple-value-call #'%f7 (values b 2))))))
         (vals '(1439719153))
         (v1 (apply (compile nil fn1) vals))
         (v2 (apply (compile nil fn2) vals)))
    (if (eql v1 v2) :good (list v1 v2)))
  :good)

(deftest misc.124
  (let* ((fn1 '(lambda (b)
                 (declare (optimize (safety 1)))
                 (labels ((%f7 (f7-1 f7-2)
                               (let ((v2 (setq b 723149855)))
                                 25620)))
                   (max b
                        (funcall #'%f7 b 2)))))
         (fn2 '(lambda (b)
                 (labels ((%f7 (f7-1 f7-2)
                               (let ((v2 (setq b 723149855)))
                                 25620)))
                   (max b
                        (funcall #'%f7 b 2)))))
         (vals '(1439719153))
         (v1 (apply (compile nil fn1) vals))
         (v2 (apply (compile nil fn2) vals)))
    (if (eql v1 v2) :good (list v1 v2)))
  :good)

;;; This passed in gcl, but I added it for completeness.
(deftest misc.125
  (let* ((fn1 '(lambda (b)
                 (declare (optimize (safety 1)))
                 (labels ((%f7 (f7-1 f7-2)
                               (let ((v2 (setq b 723149855)))
                                 25620)))
                   (max b
                        (%f7 b 2)))))
         (fn2 '(lambda (b)
                 (labels ((%f7 (f7-1 f7-2)
                               (let ((v2 (setq b 723149855)))
                                 25620)))
                   (max b
                        (%f7 b 2)))))
         (vals '(1439719153))
         (v1 (apply (compile nil fn1) vals))
         (v2 (apply (compile nil fn2) vals)))
    (if (eql v1 v2) :good (list v1 v2)))
  :good)


;;; clisp optional argument bug: "SYMBOL-VALUE: 1 is not a SYMBOL"

(deftest misc.126
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (special *should-always-be-true*))
       (labels ((%f10 (f10-1 &optional
                             (f10-2 (cl:handler-bind nil
                                                     (if *should-always-be-true*
                                                         (progn 878)
                                                       (should-never-be-called)
                                                       )))
                             (f10-3 (cl:handler-case 10)))
                      -15))
         (%f10 -144)))))
  -15)

(deftest misc.127
  (funcall
   (compile
    nil
    '(lambda (a c)
       (flet ((%f10 (f10-1 f10-2) 10))
         (flet ((%f4
                 (&optional
                  (f4-1 (ldb (byte 10 6)
                             (* 828
                                (+ 30 (dpb c (byte 9 30) (%f10 1918433 34107)))
                                )))
                  (f4-2 (setq a 0)))
                 2))
           (%f4 -5)))))
   0 0)
  2)

;;; cmucl (22 Oct 2003 build) bug
;;; The assertion (EQ (C::COMPONENT-KIND C:COMPONENT) :INITIAL) failed.

(deftest misc.128
  (flet ((%f14
          (f14-1 f14-2
                 &optional
                 (f14-3 (unwind-protect 13059412))
                 (f14-4 452384)
                 (f14-5 -6714))
          -1))
    (%f14 -2 1 1279896 589726354 -11))
  -1)

(deftest misc.129
  (labels ((%f17 (f17-1 f17-2 &optional (f17-3 (unwind-protect 178)))
                 483633925))
    -661328075)
  -661328075)

(deftest misc.130
  (let* ((fn1
          '(lambda (a c)
             (flet ((%f10 (&optional (f10-1 -6489) (f10-2 (+ c)))
                          a))
               (multiple-value-call #'%f10 (values -178858 a)))))
         (fn2
          '(lambda (a c)
             (declare (notinline values +) (optimize (speed 0) (debug 0)))
             (flet ((%f10 (&optional (f10-1 -6489) (f10-2 (+ c)))
                          a))
               (multiple-value-call #'%f10 (values -178858 a)))))
         (vals '(-13649921 -1813684177409))
         (v1 (apply (compile nil fn1) vals))
         (v2 (apply (compile nil fn2) vals)))
    (if (eql v1 v2) :good (list v1 v2)))
  :good)

(deftest misc.131
  (let* ((fn1
          '(lambda (a b)
             (max
              (block b7
                (abs
                 (ignore-errors
                   (if (ldb-test (byte 33 15) (return-from b7 a))
                       b b)))))))
         (fn2
          '(lambda (a b)
             (declare (notinline abs max))
             (declare (optimize (speed 0)))
             (declare (optimize (debug 0)))
             (max
              (block b7
                (abs
                 (ignore-errors
                   (if (ldb-test (byte 33 15) (return-from b7 a))
                       b b)))))))
         (vals '(-823894140303 -3))
         (v1 (apply (compile nil fn1) vals))
         (v2 (apply (compile nil fn2) vals)))
    (if (eql v1 v2) :good (list v1 v2)))
  :good) 

;;; cmucl (22 Oct 2003)
;;; The assertion (EQ C::ENV
;;;                   (C::LAMBDA-ENVIRONMENT
;;;                      (C::LAMBDA-VAR-HOME C::THING))) failed.

(deftest misc.132
  (funcall
   (compile nil
            '(lambda (b c)
               (declare (type (integer -3358662 7782429) b))
               (declare (type (integer -513018 12740) c))
               (declare (optimize (speed 3)))
               (declare (optimize (safety 1)))
               (declare (optimize (debug 1)))
               (labels ((%f9
                         (&optional
                          (f9-1
                           (labels
                               ((%f5 (f5-1 f5-2)
                                     (floor (ignore-errors f5-1)
                                            (min -67 (if (equal -56 c)
                                                         -11197265 f5-2)))))
                             c))
                          (f9-2 -439518)
                          (f9-3 -2840573))
                         f9-1))
                 (%f9 -193644 b 1368))))
   10 20)
  -193644)

(deftest misc.132a
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (speed 2) (space 0) (safety 0) (debug 2) (compilation-speed 0)))
       (labels ((%f1 () 0))
         (if t 0 (dotimes (iv1 5 (if (%f1) 0 0)) (catch 'ct1 0)))))))
  0)

;;; cmucl (22 Oct 2003)  Default for optional parameter is improperly chosen
(deftest misc.133
  (funcall
   (compile nil
            '(lambda (a b c)
               (declare (notinline values))
               (declare (optimize (speed 0) (debug 0)))
               (flet ((%f15 (&optional (f15-5 c)) f15-5))
                 (multiple-value-call #'%f15 (values -2688612)))))
   1 2 3)
  -2688612)

;;; ACL 6.2 (x86 linux trial) bugs
;;; With optional flet/labels parameters, there's a very high frequency bug
;;; causing the compiler error "Error: `:INFERRED' is not of the expected
;;; type `NUMBER'".  The following tests show this bug.

(deftest misc.134
  (funcall
   (compile nil
            '(lambda (b)
               (labels ((%f5 (f5-1 f5-2 f5-3 &optional (f5-4 0)
                                   (f5-5
                                    (flet ((%f13 (f13-1)
                                                 (return-from %f13 b))) b)))
                             900654472))
                 183301)))
   13775799184)
  183301)

(deftest misc.135
  (funcall
   (compile nil
            '(lambda (a b)
               (labels ((%f4 (&optional (f4-1 (labels ((%f17 nil a)) b)))
                             -14806404))
                 190134)))
   1783745644 268410629)
  190134)

(deftest misc.136
  (funcall
   (compile nil
            '(lambda (c)
               (flet ((%f17 (&optional
                             (f17-1 (flet ((%f9 nil c)) 73574919)))
                            643))
                 1039017546)))
   0)
  1039017546)

;;; And these caused segfaults

(deftest misc.137
  (funcall
   (compile nil
            '(lambda ()
               (declare (optimize (speed 3)))
               (declare (optimize (safety 1)))
               (flet ((%f16 (&optional
                             (f16-2 (lognor -3897747
                                            (if nil -1 -127228378))))
                            10))
                 20))))
  20)

(deftest misc.138
  (funcall
   (compile nil
            '(lambda (c)
               (declare (type (integer 2996 39280) c))
               (declare (optimize (speed 3)))
               (declare (optimize (safety 1)))
               (if (zerop (labels ((%f8 (&optional
                                         (f8-2 (logorc2 c -161957)))
                                        2176))
                            3))
                   c c)))
   3000)
  3000)

;;; Lispworks 4.2 (x86 linux personal edition) failures


(deftest misc.139
  (let* ((fn1
          '(lambda (c)
             (declare (optimize (speed 3)))
             (logior (labels ((%f1 (f1-1 &optional (f1-2 (setq c 7))) f1-1))
                       (%f1 774 3616592)) c)))
         (fn2
          '(lambda (c)
             (logior (labels ((%f1 (f1-1 &optional (f1-2 (setq c 7))) f1-1))
                       (%f1 774 3616592)) c)))
         (vals '(-3))
         (v1 (apply (compile nil fn1) vals))
         (v2 (apply (compile nil fn2) vals)))
    (if (eql v1 v2) :good (list v1 v2)))
  :good)

(deftest misc.140
  (funcall
   (compile nil
            '(lambda (a)
               (ldb (byte 24 20)
                    (labels ((%f12 (&optional (f12-1 149) (f12-2 -3894159)) 34068))
                      (let* ((v4 (%f12))) a)))))
   -1)
  16777215)
 

;;; In Lispworks 4.2 (x86 linux personal edition)
;;; 'Error: *** Ran out of patterns in (MOVE) for (edi NIL)'

(deftest misc.141
  (funcall
   (compile nil
            '(lambda () (labels ((%f11 (&optional (f11-3 (restart-case 0))) f11-3))
                          (%f11 1)))))
  1)

(deftest misc.142
  (funcall
   (compile nil
            '(lambda ()
               (labels ((%f15 (&optional (f15-3 (block b1 (+ 1 (return-from b1 -10)))))
                              f15-3))
                 (%f15)))))
  -10)

;;; cmucl (22 Oct 2003):  NIL is not of type C::REF
(deftest misc.143
  (block b2
      (max (return-from b2 1)
           (let ((v3
                  (unwind-protect
                      (let* ((v1 (ignore-errors -254)))
                        1))))
             -2)))
  1)

;;; (was) The assertion (NOT (C::BLOCK-DELETE-P BLOCK)) failed.
;;; (now) The assertion (NOT (MEMBER C::KIND '(:DELETED :OPTIONAL :TOP-LEVEL))) failed.

(deftest misc.144
  (funcall
   (compile nil
            '(lambda (a b c)
               (declare (type (integer 9739325 14941321) c))
               (labels ((%f7 (f7-1 f7-2 f7-3 &optional (f7-4 b))
                             (return-from %f7 f7-4)))
                 (if (= -76482 c)
                     (if (>= c 10986082) (%f7 a b (%f7 -8088 c -147106 2)) -10502)
                   (%f7 509252 b b)))))
   -200 17 10000000)
  17)

(deftest misc.145
  (funcall
   (compile nil
            '(lambda (a b c)
               (declare (optimize (safety 3)))
               (block b5
                 (return-from b5
                   (logior (if (or c t) b (load-time-value -61)) (return-from b5 -3))))))
   1 2 3)
  -3)

;;; cmucl: order of evaluation error
(deftest misc.146
  (funcall
   (compile nil
            '(lambda (b)
               (declare (optimize (speed 3)))
               (flet ((%f14 (&optional (f14-1 301917227)
                                       (f14-2 (setq b 995196571)))
                            f14-1))
                 (%f14 b (block b3 (%f14 -64))))))
   10)
  10)

;;; cmucl (22 Oct 2003): NIL is not of type C::CLEANUP
(deftest misc.147
  (flet ((%f11 () (if nil (ignore-errors -19884254) (unwind-protect -2)))) :good)
  :good)

;;; The assertion (C::CONSTANT-CONTINUATION-P C::CONT) failed.
(deftest misc.148
  (block b2 (logior (return-from b2 484) (restart-case (ignore-errors 1737021))))
  484)

;;; Argument X is not a NUMBER: NIL.
(deftest misc.149
  (funcall
   (compile nil '(lambda (b)
                   (block b1 (- (logand 0 -34 1026491) (ignore-errors (return-from b1 b))))))
   0)
  0)

(deftest misc.149a
  (funcall
   (compile nil '(lambda (a) (block b1 (- a (ignore-errors (return-from b1 1))))))
   0)
  1)

;;; cmucl (11 2003 image)  "NIL is not of type C::CONTINUATION"
(deftest misc.150
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (flet ((%f17
               (&optional
                (f17-4
                 (labels ((%f13 (f13-1 &optional (f13-2 (multiple-value-prog1 b)))
                                -4))
                   (%f13 b (%f13 190)))))
               -157596))
         (labels ((%f6 () (%f17))) c))))
   10 20 30000)
  30000)

(deftest misc.150a
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (speed 3) (space 2) (safety 3)
                          (debug 3) (compilation-speed 2)))
       (catch 'ct6 (apply (constantly 0) (list))))))
  0)

(deftest misc.150b
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type integer a))
       (declare (optimize (speed 3) (space 0) (safety 3) (debug 2)
                          (compilation-speed 3)))
       (if (= a 0) 0 (truncate a))))
   0)
  0)

(deftest misc.150c
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (optimize (speed 1) (space 3) (safety 2) (debug 3) (compilation-speed 3)))
       (labels ((%f4 (f4-1)
                     0))
         (labels ((%f15
                   (f15-1 f15-2
                          &optional (f15-3 (apply #'%f4 0 nil)) (f15-4 0)
                          (f15-5 (%f4 (%f4 (if (/= 0 0) a 0)))))
                   0))
           (labels ((%f13 (f13-1)
                          (%f15 b 0 0 0)))
             0)))))
   1 2)
  0)

(deftest misc.150d
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer 4146834609223 16403344221223) a))
       (declare (type (integer -35470308180 3523580009) b))
       (declare (optimize (speed 1) (space 3) (safety 3) (debug 0) (compilation-speed 0)))
       (catch 'ct1 (logand b a 0))))
   4146834609223 10)
  0)

;;; cmucl (11 2003 x86 linux)  "NIL is not of type C::ENVIRONMENT"
(deftest misc.151
  (funcall
   (compile
    nil
    '(lambda (b c)
       (declare (type (integer -249 97) b))
       (declare (type (integer 3565969 6559088) c))
       (let* ((v7
               (if (not (= 1030 4))
                   c
                 (logand (if (/= b c) b 34945725) (unwind-protect -12443701)))))
         5520737)))
   -24 5657943)
  5520737)

(deftest misc.151a
  (funcall
   (compile
    nil
    '(lambda ()
       (declare
        (optimize (speed 3) (space 3) (safety 1) (debug 1) (compilation-speed 0)))
       (case 0 ((-12 -9 -12 -2 -5 -2 15) (catch 'ct7 (throw 'ct7 0))) (t 0)))))
  0)  

;;; sbcl bug (0.8.5.19)
;;; "The value NIL is not of type SB-C::REF."

(deftest misc.152
  (funcall
   (compile nil
            '(lambda (a)
               (block b5
                 (let ((v1 (let ((v8 (unwind-protect 9365)))
                             8862008)))
                   (*
                    (return-from b5
                      (labels ((%f11 (f11-1) f11-1))
                        (%f11 87246015)))
                    (return-from b5
                      (setq v1
                            (labels ((%f6 (f6-1 f6-2 f6-3) v1))
                              (dpb (unwind-protect a)
                                   (byte 18 13)
                                   (labels ((%f4 () 27322826))
                                     (%f6 -2 -108626545 (%f4))))))))))))
   -6)
  87246015)

(deftest misc.153
  (funcall
   (compile nil
            '(lambda (a)
               (if (logbitp 3
                            (case -2
                              ((-96879 -1035 -57680 -106404 -94516 -125088)
                               (unwind-protect 90309179))
                              ((-20811 -86901 -9368 -98520 -71594)
                               (let ((v9 (unwind-protect 136707)))
                                 (block b3
                                   (setq v9
                                         (let ((v4 (return-from b3 v9)))
                                           (- (ignore-errors (return-from b3 v4))))))))
                              (t -50)))
                   -20343
                 a)))
   0)
  -20343)

;;; Bug in ecl (cvs head, 4 Nov 2003)
;;; "/tmp/ecl04Coiwc0V.c:48: `lex0' undeclared (first use in this function)"

(deftest misc.154
  (funcall
   (compile nil
            '(lambda (b)
               (labels ((%f8 nil -39011))
                 (flet ((%f4 (f4-1 f4-2 &optional (f4-3 (%f8)) (f4-4 b))
                             (%f8)))
                   (%f4 -260093 -75538 -501684 (let ((v9 (%f8))) -3))))))
   0)
  -39011)

;;; "/tmp/ecl1572CbKzu.c:16: too many arguments to function `APPLY'"

(deftest misc.155
  (funcall
   (compile nil
            '(lambda (a b c)
               (labels ((%f6 (f6-1 f6-2) c))
                 (multiple-value-call #'%f6 (values a c)))))
   0 10 20)
  20)

;;; "The function C::LDB1 is undefined."

(deftest misc.156
  (funcall
   (compile nil
            '(lambda ()
               (let ((v6 (ldb (byte 30 1) 1473))) (let ((v8 v6)) 2395)))))
  2395)

;;; "/tmp/ecl9CEiD1RL5.c:36: `lex0' undeclared (first use in this function)"
   
(deftest misc.157
  (funcall
   (compile nil
            ' (lambda (c)
                (labels ((%f11 nil 1))
                  (flet ((%f9 (f9-1 f9-2)
                              (case 17466182 ((-12) (%f11)) (t c))))
                    (%f9 -9913 c)))))
   17)
  17)

;;; SBCL (0.8.5.24) bug:  "bogus operands to XOR"

(deftest misc.158
  (funcall
   (compile nil
            '(lambda (a b c)
               (declare (type (integer 79828 2625480458) a))
               (declare (type (integer -4363283 8171697) b))
               (declare (type (integer -301 0) c))
               (if (equal 6392154 (logxor a b))
                   1706
                 (let ((v5 (abs c)))
                   (logand v5
                           (logior (logandc2 c v5)
                                   (common-lisp:handler-case
                                    (ash a (min 36 22477)))))))))
   100000 0 0)
  0)

;;; sbcl (0.8.5.24) The value NIL is not of type SB-C::CTRAN.

(deftest misc.159
  (funcall
   (compile nil
            '(lambda ()
               (let ((v8 70696))
                 (if (equal v8 -536145083)
                     (let ((v2 (setq v8 v8)))
                       (flet ((%f9 (f9-1 f9-2)
                                   309257))
                         (multiple-value-call #'%f9 (values v2 v2))))
                   100)))))
  100)

;;; sbcl (0.8.5.37) The value NIL is not of type SB-C::CTRAN.

(deftest misc.159a
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -105680 2104974) a))
       (declare (type (integer -1881 -1134) b))
       (declare (ignorable a b))
       (declare (optimize (speed 3)))
       (declare (optimize (safety 1)))
       (declare (optimize (debug 1)))
       (block b5
         (let ((v2
                (if (or (>= 34 a) 108361696)
                    (return-from b5 -1)
                  (lognand b -16023672))))
           (flet ((%f10
                   (f10-1
                    &optional (f10-2 (if (eql -30 v2) v2 -5)) (f10-3 v2)
                    (f10-4 14))
                   (if (equal a f10-2) f10-4 380663047)))
             (flet ((%f6 (f6-1 f6-2 f6-3)
                         f6-1))
               (multiple-value-call #'%f6
                                    (values a (%f10 -37243) -47691))))))))
   100 -1200)
  -1)

;;; gcl (9 Nov 2003) bug
;;; Error in FUNCALL [or a callee]: Caught fatal error [memory may be damaged]

(deftest misc.160
  (funcall
   (compile nil
            '(lambda (c)
               (declare (notinline + funcall))
               (+ (labels ((%f1 () -14)) (funcall #'%f1))
                  (flet ((%f2 () (floor c))) (funcall #'%f2)))))
   0)
  -14)

;;; cmucl (9 Nov 2003)
;;; The assertion (NOT (MEMBER C::KIND '(:DELETED :OPTIONAL :TOP-LEVEL))) failed.

(deftest misc.161
  (funcall
   (compile nil
            '(lambda (a b c)
               (flet ((%f17 (f17-1 f17-2 f17-3)
                            (flet ((%f2
                                    (f2-1 f2-2
                                          &optional (f2-3 (return-from %f17 f17-1))
                                          (f2-4 (return-from %f17 -57)))
                                    b))
                              (multiple-value-call #'%f2 (values c -588 55101157)))))
                 (if nil
                     (let* ((v6 (%f17 102136 3096194 a)))
                       b)
                   c))))
   -511 -2269809964 250738)
  250738)

(deftest misc.161a
  (funcall
   (compile nil
            '(lambda (a)
               (declare (optimize (speed 3) (space 2) (safety 3) (debug 0) (compilation-speed 0)))
               (progn (abs 0) (- a) 0)))
   1)
  0)

;;; cmucl (9 Nov 2003) Incorrect result at SPEED 0.

(deftest misc.162
  (let* ((fn `(lambda (a c)
                (declare (notinline funcall)
                         (optimize (speed 0) (debug 0)))
                (labels ((%f17 (f17-1 &optional (f17-4 c))
                               (return-from %f17 (if f17-4 f17-1 49572640))))
                  (funcall #'%f17 15128425 a)))))
    (funcall (compile nil fn) 1 3))
  15128425)

;;; gcl (12 Nov 2003)
;;; C compiler failure during compilation (duplicate case value)

(deftest misc.163
  (funcall
   (compile nil
            '(lambda (b)
               (declare (type (integer -15716 3947) b))
               (case b
                 ((-7 -6 -6) :good)
                 ((-5 -6) :bad)
                 )))

   -6)
  :good)

;;; gcl (13 Nov 2003)
;;; Error in FUNCALL [or a callee]: Caught fatal error [memory may be damaged]

(deftest misc.164
  (funcall
   (compile
    nil
    '(lambda (a)
     (labels ((%f6 (f6-1 f6-2)
                   (cl:handler-case
                    (labels ((%f2 nil (logior a)))
                      (if (eql (%f2) (%f2))
                          2829254 -10723))
                    (error (c) (error c))
                    )))
       (funcall #'%f6 10 20)
       )))
   0)
  2829254)

;;; sbcl failures

;;; The value NIL is not of type SB-C::NODE.
(deftest misc.165
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (block b3
         (flet ((%f15
                 (f15-1 f15-2 f15-3
                        &optional
                        (f15-4
                         (flet ((%f17
                                 (f17-1 f17-2 f17-3
                                        &optional (f17-4 185155520) (f17-5 c)
                                        (f17-6 37))
                                 c))
                           (%f17 -1046 a 1115306 (%f17 b -146330 422) -337817)))
                        (f15-5 a) (f15-6 -40))
                 (return-from b3 -16)))
           (multiple-value-call #'%f15 (values -519354 a 121 c -1905))))))
   0 0 -5)
  -16)

;;; failed AVER:
;;;      "(NOT
;;; (AND (NULL (BLOCK-SUCC B))
;;;      (NOT (BLOCK-DELETE-P B))
;;;      (NOT (EQ B (COMPONENT-HEAD #)))))"

(deftest misc.166
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (labels ((%f4
                 (f4-1 f4-2 &optional (f4-3 b) (f4-4 c) (f4-5 -170))
                 (let ((v2
                        (flet ((%f3
                                (f3-1
                                 &optional (f3-2 28476586) (f3-3 c)
                                 (f3-4 -9240))
                                (return-from %f4 1)))
                          (multiple-value-call
                           #'%f3
                           (values -479909 19843799 f4-5 -463858)))))
                       b)))
         c)))
   0 0 -223721124)
  -223721124)

(deftest misc.167
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (flet ((%f5 (f5-1 f5-2)
                   (return-from %f5 604245664)))
         (flet ((%f12
                 (f12-1 f12-2
                        &optional (f12-3 c) (f12-4 -579456)
                        (f12-5
                         (labels ((%f9
                                   (f9-1
                                    &optional
                                    (f9-2
                                     (%f5 1
                                          (let ((v4 (%f5 30732606 a)))
                                            b)))
                                    (f9-3 -29)
                                    (f9-4
                                     (block b4
                                       (labels ((%f14 ()
                                                      (labels ((%f18
                                                                (&optional
                                                                 (f18-1
                                                                  (locally
                                                                   592928))
                                                                 (f18-2 -3)
                                                                 (f18-3
                                                                  (return-from
                                                                      b4 a)))
                                                                f18-1))
                                                        (%f18 74214190 a))))
                                         (%f14)))))
                                   -1))
                           (flet ((%f17
                                   (f17-1 f17-2 &optional (f17-3 -136045032))
                                   -38655))
                             (%f17 43873 -138030706 -1372492)))))
                 (return-from %f12 -15216677)))
           (%f12 (%f5 b 2329383) a)))))
   1 2 3)
  -15216677)

(deftest misc.168
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (block b3
         (flet ((%f11
                 (f11-1 f11-2
                        &optional
                        (f11-3
                         (block b6
                           (labels ((%f11
                                     (f11-1
                                      &optional (f11-2 c)
                                      (f11-3 (return-from b6 -1806)))
                                     (return-from b3 -28432)))
                             (apply #'%f11 (list -114))))))
                 (return-from %f11 f11-2)))
           (%f11 b
                 c
                 (labels ((%f10
                           (f10-1 f10-2
                                  &optional (f10-3 a) (f10-4 (%f11 -3931 170)))
                           -1704759))
                   c))))))
   1 2 3)
  3)

(deftest misc.169
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (if t -21705
         (flet ((%f15 (f15-1 f15-2)
                      b))
           (block b4
             (%f15 -11112264
                   (labels ((%f2
                             (f2-1
                              &optional (f2-2 (if b -5485340 -1534))
                              (f2-3 -6))
                             (return-from b4 f2-1)))
                     (return-from b4
                       (if b (%f2 c -320813) (%f2 b a a))))))))))
   1 2 3)
  -21705)

;;; sbcl (0.8.5.26)
;;; failed AVER: "(FUNCTIONAL-LETLIKE-P CLAMBDA)"

(deftest misc.170
  (funcall
   (compile
    nil
    '(lambda (b)
       (flet ((%f14 (f14-1 f14-2)
                    (if (if (eql b -7) nil nil)
                        (labels ((%f10 (f10-1 f10-2 f10-3)
                                       7466))
                          (return-from %f14
                            (min
                             (multiple-value-call #'%f10 (values 0 492 f14-1))
                             (max 11 f14-1)
                             (multiple-value-call #'%f10
                                                  (values 439171 f14-2 0)))))
                      1)))
         (let ((v6 (%f14 (logcount b) -386283)))
           56211))))
   17)
  56211)

(deftest misc.170a
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -281 30570) a))
       (declare (type (integer -4247786 -199821) b))
       (declare
        (optimize (speed 3)
                  (space 0)
                  (safety 0)
                  (debug 2)
                  (compilation-speed 1)))
       (flet ((%f14 (f14-1 f14-2)
                    (coerce 0 'integer)))
         (labels ((%f3 (f3-1 f3-2 f3-3)
                       (if
                           (if
                               (typep (%f14 -864 -10620) '(integer -11672107617 -2))
                               t
                             (typep
                              (imagpart
                               (lcm 2120258 0 (logandc2 -6222 -1057382553)))
                              '(integer * -113)))
                           (dotimes
                               (iv3 5
                                    (flet ((%f11 (f11-1 f11-2 f11-3)
                                                 b))
                                      (multiple-value-call #'%f11
                                                           (values a a f3-3))))
                             0)
                         0)))
           (case (%f3 a a 0) (t 0))))))
   22087 -1787181)
  0)

;;; The value NIL is not of type SB-C::NODE.

(deftest misc.171
  (funcall
   (compile
    nil
    '(lambda (b)
       (block b6
         (flet ((%f11 (f11-1 f11-2 &optional (f11-3 -2369157) (f11-4 409468))
                      (return-from b6 1)))
           (block b2
             (flet ((%f10 (f10-1 f10-2
                            &optional (f10-3 (return-from b6 (return-from b6 -3))))
                     -8))
               (%f10
                (multiple-value-call #'%f11 (values -5945959 1654846427 -22))
                (return-from b2 b)
                (return-from b2 31258361))))))))
   10)
  1)

  
;;;   segmentation violation at #XA4A0B59

(deftest misc.172
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (notinline list apply))
       (declare (optimize (safety 3)))
       (declare (optimize (speed 0)))
       (declare (optimize (debug 0)))
       (labels ((%f12 (f12-1 f12-2)
                      (labels ((%f2 (f2-1 f2-2)
                                    (flet ((%f6 ()
                                                (flet ((%f18
                                                        (f18-1
                                                         &optional (f18-2 a)
                                                         (f18-3 -207465075)
                                                         (f18-4 a))
                                                        (return-from %f12 b)))
                                                  (%f18 -3489553
                                                        -7
                                                        (%f18 (%f18 150 -64 f12-1)
                                                              (%f18 (%f18 -8531)
                                                                    11410)
                                                              b)
                                                        56362666))))
                                      (labels ((%f7
                                                (f7-1 f7-2
                                                      &optional (f7-3 (%f6)))
                                                7767415))
                                        f12-1))))
                        (%f2 b -36582571))))
         (apply #'%f12 (list 774 -4413)))))
   0 1 2)
  774)

;;; In sbcl 0.8.5.37
;;; "Unreachable code is found or flow graph is not properly depth-first ordered."

(deftest misc.173
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (notinline values))
       (declare (optimize (safety 3)))
       (declare (optimize (speed 0)))
       (declare (optimize (debug 0)))
       (flet ((%f11
               (f11-1 f11-2
                      &optional (f11-3 c) (f11-4 7947114)
                      (f11-5
                       (flet ((%f3 (f3-1 &optional (f3-2 b) (f3-3 5529))
                                   8134))
                         (multiple-value-call #'%f3
                                              (values (%f3 -30637724 b) c)))))
               (setq c 555910)))
         (if (and nil (%f11 a a))
             (if (%f11 a 421778 4030 1)
                 (labels ((%f7
                           (f7-1 f7-2
                                 &optional
                                 (f7-3
                                  (%f11 -79192293
                                        (%f11 c a c -4 214720)
                                        b
                                        b
                                        (%f11 b 985)))
                                 (f7-4 a))
                           b))
                   (%f11 c b -25644))
               54)
           -32326608))))
   1 2 3)
  -32326608)

;;; In sbcl 0.8.5.37
;;; The value NIL is not of type SB-C:COMPONENT.

(deftest misc.174
  (funcall
   (compile
    nil
    '(lambda (a b c)
         (declare (type (integer 10292971433 14459537906) b))
         (declare (optimize (speed 3)))
         (declare (optimize (safety 1)))
         (declare (optimize (debug 1)))
         (if
          (and (and (/= -51885 b) nil)
               (case (1+ b)
                 ((4 4 3 -4)
                  (let* ((v1
                          (flet ((%f16 (f16-1)
                                   -1858366))
                            (apply #'%f16 b (list)))))
                    -1602321))
                 (t 3)))
          19
          c)))
   0 11000000000 0)
  0)

(deftest misc.174a
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer 23 365478242977) a))
       (declare (type (integer -38847 268231) b))
       (declare (optimize (speed 3)))
       (declare (optimize (safety 1)))
       (declare (optimize (debug 1)))
       (if (not (if (/= b 7) t (not (not a))))
           (case (setq b -5880)
             ((8382 3401 2058 39167 62228)
              (flet ((%f7 (f7-1 f7-2 f7-3) f7-1))
                (multiple-value-call #'%f7 (values -135629 a -410168200))))
             (t a))
         15173)))
   30 0)
  15173)

(deftest misc.174b
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -8688 2170) a))
       (declare (type (integer -9938931470 1964967743) b))
       (declare (optimize (speed 3)))
       (declare (optimize (safety 1)))
       (declare (optimize (debug 1)))
       (if
           (and
            (if (if (equal b 9) nil t)
                nil
              (not
               (logbitp 5
                        (labels ((%f5 (f5-1 f5-2 f5-3)
                                      4057223))
                          (let ((v9 (%f5 -42 -27504 45026809)))
                            15011)))))
            (if
                (or a
                    (labels ((%f16 (f16-1)
                                   61))
                      (apply #'%f16 275 (list))))
                a
              t))
           (setq a -4803)
         (rem a (max 47 b)))))
   0 0)
  0)

;;; In sbcl 0.8.5.37
;;; "Unreachable code is found or flow graph is not properly depth-first ordered."

(deftest misc.175
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (notinline list apply values signum funcall))
       (declare (optimize (safety 3)))
       (declare (optimize (speed 0)))
       (declare (optimize (debug 0)))
       (labels ((%f4 (f4-1 f4-2 f4-3)
                     (labels ((%f1 (f1-1 f1-2 f1-3)
                                   2))
                       (labels ((%f11
                                 (f11-1
                                  &optional
                                  (f11-2
                                   (return-from %f4
                                     (labels ((%f8
                                               (f8-1 f8-2 f8-3
                                                     &optional (f8-4 -35)
                                                     (f8-5 f4-2))
                                               f4-1))
                                       (funcall #'%f8 53 b f4-1)))))
                                 (return-from %f4 a)))
                         (signum
                          (let ((v4
                                 (flet ((%f8
                                         (f8-1 f8-2 f8-3
                                               &optional (f8-4 b) (f8-5 -560367))
                                         f8-4))
                                   (%f8 -27 35395 c -69))))
                            (%f11
                             (multiple-value-call #'%f11
                                                  (values (%f1 (%f11 b (%f11 v4 f4-3)) f4-3 77936)
                                                          1628490976))
                             (return-from %f4 (%f1 -9432 f4-1 f4-1)))))))))
         (flet ((%f7 (f7-1 f7-2 f7-3)
                     (%f4 b f7-3 f7-3)))
           (flet ((%f14 (f14-1)
                        (apply #'%f7 -252 -56169265 -7322946 (list))))
             (%f14 a))))))
   -70313091 577425217 28052774417)
  -70313091)

(deftest misc.175a
  (funcall
   (compile
    nil
    '(lambda (a b)
         (declare (notinline values list apply logior))
         (declare (optimize (safety 3)))
         (declare (optimize (speed 0)))
         (declare (optimize (debug 0)))
         (if nil
             (logior (flet ((%f5 (f5-1) b)) (%f5 56288))
              (flet ((%f17 (f17-1 f17-2
                          &optional
                          (f17-3 (let () 6857))
                          (f17-4
                           (labels ((%f3 (f3-1 f3-2 f3-3  &optional (f3-4 a) (f3-5 877))
                                         139))
                             (apply #'%f3 (list -33052082 b a 1572)))))
                           b))
                (multiple-value-call #'%f17 (values 31 b a b))))
             392)))
   0 0)
  392)

(deftest misc.175b
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -1185422977 2286472818) a))
       (declare (type (integer -211381289038 74868) b))
       (declare (ignorable a b))
       (declare (optimize (speed 3)))
       (declare (optimize (safety 1)))
       (declare (optimize (debug 1)))
       (lognor (unwind-protect -1248)
               (flet ((%f7
                       (&optional
                        (f7-1
                         (flet ((%f1 (f1-1 f1-2 f1-3) 121426))
                           (%f1 b 2337452 (%f1 61767 b a))))
                        (f7-2
                         (block b8
                           (logandc1
                            (labels ((%f10 (f10-1 f10-2 f10-3) 323734600))
                              (%f10 (%f10 323734600 323734600 -10165)
                                    -607741 (ignore-errors 971588)))
                            (if (>= b -27) (return-from b8 -2)
                              (ignore-errors 237138926))))))
                       f7-2))
                 (apply #'%f7 (list 761316125 b))))))
   1792769319 -60202244870)
  5)


;;; sbcl 0.8.5.37
;;; failed AVER: "(FUNCTIONAL-LETLIKE-P CLAMBDA)"

(deftest misc.176
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (type (integer 162180298 184143783) a))
       (declare (type (integer 702599480988 725878356286) b))
       (declare (type (integer 168 80719238530) c))
       (declare (optimize (speed 3)))
       (declare (optimize (safety 1)))
       (declare (optimize (debug 1)))
       (block b6
         (flet ((%f10 (f10-1 f10-2)
                      (labels ((%f6 (f6-1 f6-2)
                                    f6-1))
                        (let ((v2
                               (flet ((%f1 (f1-1 f1-2 f1-3)
                                           f1-3))
                                 (let ((v8
                                        (%f1 -11350578
                                             (%f6 10414199 13)
                                             -58931837)))
                                   -239755))))
                          323))))
           (labels ((%f4
                     (f4-1
                      &optional (f4-2 204) (f4-3 -1)
                      (f4-4
                       (flet ((%f2 (f2-1)
                                   (if t (return-from b6 c) a)))
                         (logorc2 (multiple-value-call #'%f2 (values 1))
                                  (let* ((v5 (floor (%f2 -1260))))
                                    (case (abs (logxor 185664 a))
                                      ((-2 5975)
                                       (if (or (< b v5) nil)
                                           (return-from b6
                                             (let ((v10 (%f2 c)))
                                               0))
                                         (multiple-value-call #'%f10
                                                              (values -3 a))))
                                      (t b)))))))
                     1503938))
             (multiple-value-call #'%f4 (values -1 a 1853966)))))))
   173549795 725346738048 993243799)
  993243799)

;;; different results (sbcl 0.8.5.37)
;;; May be that setq side effects bug again?

(deftest misc.177
  (let* ((form '(flet ((%f11
                        (f11-1 f11-2)
                        (labels ((%f4 () (round 200048 (max 99 c))))
                          (logand
                           f11-1
                           (labels ((%f3 (f3-1) -162967612))
                             (%f3 (let* ((v8 (%f4)))
                                    (setq f11-1 (%f4)))))))))
                  (%f11 -120429363 (%f11 62362 b))))
         (vars '(a b c))
         (vals '(6714367 9645616 -637681868))
         (fn1 `(lambda ,vars
                  (declare (type (integer 804561 7640697) a))
                  (declare (type (integer -1 10441401) b))
                  (declare (type (integer -864634669 55189745) c))
                  (declare (ignorable a b c))
                  (declare (optimize (speed 3)))
                  (declare (optimize (safety 1)))
                  (declare (optimize (debug 1)))
                  ,form))
         (fn2 `(lambda ,vars
                 (declare (notinline list apply logand max round))
                 (declare (optimize (safety 3)))
                 (declare (optimize (speed 0)))
                 (declare (optimize (debug 0)))
                 ,form))
         (compiled-fn1 (compile nil fn1))
         (compiled-fn2 (compile nil fn2))
         (results1 (multiple-value-list (apply compiled-fn1 vals)))
         (results2 (multiple-value-list (apply compiled-fn2 vals))))
    (if (equal results1 results2)
        :good
      (values results1 results2)))
  :good)

;;; sbcl 0.8.5.37
;;; The value NIL is not of type INTEGER.

(deftest misc.178
  (funcall
   (compile
    nil
    '(lambda (a b c)
         (declare (ignorable a b c))
         (declare (optimize (speed 3)))
         (declare (optimize (safety 1)))
         (declare (optimize (debug 1)))
         (let ((v9
                (flet ((%f9
                           (f9-1 f9-2 f9-3
                            &optional (f9-4 -40538)
                            (f9-5
                             (flet ((%f10 (f10-1 f10-2)
                                      (labels ((%f11 (f11-1 f11-2)
                                                 (labels ((%f10 (f10-1 f10-2)
                                                            -1422))
                                                   (if
                                                    (< b
                                                       (%f10
                                                        (%f10 28262437 95387)
                                                        f10-2))
                                                    -1562
                                                    f10-2))))
                                        (let* ((v6 (%f11 59 b)))
                                          (return-from %f10
                                            (apply #'%f11
                                                   f10-1
                                                   (list
                                                    (return-from %f10
                                                      2029647))))))))
                               (apply #'%f10 -3067 3854883 (list)))))
                         64066))
                  (%f9 a 2774 0 c))))
           (flet ((%f18 (f18-1 f18-2 &optional (f18-3 66) (f18-4 b))
                    -6939342))
             (%f18 1274880 (%f18 b a 46746370 -1))))))
   0 0 0)
  -6939342)

;;; sbcl 0.8.5.37
;;; failed AVER: "(FUNCTIONAL-LETLIKE-P CLAMBDA)"

(deftest misc.179
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer 1135 16722) a))
       (declare (type (integer -640723637053 -9049) b))
       (declare (optimize (speed 3)))
       (declare (optimize (safety 1)))
       (declare (optimize (debug 1)))
       (block b3
         (return-from b3
           (flet ((%f17 (f17-1 &optional (f17-2 b) (f17-3 b))
                        (+ (if t (return-from b3 -64796) a))))
             (case (%f17 -3908648 -7026139 a)
               ((41771 -113272 -48004 -39699 50691 -13222)
                (multiple-value-call #'%f17 (values -1963404294 -105)))
               (t -7026139)))))))
   2000 -10000)
  -64796)

(deftest misc.180
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer 41 484) a))
       (declare (type (integer -2546947 1008697961708) b))
       (declare (optimize (speed 3)))
       (declare (optimize (safety 1)))
       (declare (optimize (debug 1)))
       (if (and (ldb-test (byte 30 10) b) nil)
           (labels ((%f7 (f7-1 f7-2 &optional (f7-3 -508405733))
                         390004056))
             (let* ((v4 (multiple-value-call #'%f7 (values b (%f7 b b)))))
               (multiple-value-call #'%f7
                                    (values (%f7 80199 a)
                                            (%f7
                                             (%f7 a
                                                  (let* ((v6 (%f7 -226 250)))
                                                    a))
                                             (abs (ceiling v4)))))))
         -6001)))
   50 0)
  -6001)

;;; sbcl 0.8.5.37
;;; The value NIL is not of type SB-C::TAIL-SET.

(deftest misc.181
  (funcall
   (compile
    nil
    '(lambda (a b)
         (declare (type (integer -74233251043 -16478648860) a))
         (declare (type (integer 0 960962) b))
         (declare (optimize (speed 3)))
         (declare (optimize (safety 1)))
         (declare (optimize (debug 1)))
         (flet ((%f14 ()
                  (if 1
                      (return-from %f14 a)
                      (labels ((%f10 (f10-1 f10-2 f10-3
                                            &optional (f10-4 (let* ((v7 a)) 915)))
                                 -1268205049))
                        (labels ((%f18 (f18-1)
                                   (multiple-value-call #'%f10
                                     (values f18-1
                                             (%f10
                                              (%f10 -1495
                                                    (%f10 -384
                                                          -84
                                                          (%f10 -1
                                                                48052
                                                                58909027
                                                                -35812)
                                                          -114)
                                                    (%f10 -391646964
                                                          -28131299
                                                          f18-1
                                                          (%f10 b 368193 a)))
                                              (%f10 f18-1
                                                    -1415811
                                                    f18-1
                                                    267932407)
                                              174)
                                             -58
                                             320))))
                          (let* ((v3 (let ((v7 (return-from %f14 (%f18 -418731))))
                                       (%f10 104871 -1196 -21 a))))
                            (labels ((%f1 () (%f18 (%f18 -794761))))
                              (return-from %f14 b))))))))
           (if (%f14) b 887481))))
   -51967629256 809047)
  809047)

(deftest misc.181a
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -982285129 -90) a))
       (declare (type (integer 1 82987) b))
       (declare (optimize (speed 3)))
       (declare (optimize (safety 1)))
       (declare (optimize (debug 1)))
       (labels ((%f1 (f1-1 &optional (f1-2 -34) (f1-3 3318057) (f1-4 117))
                     (let ((v9 (let ((v9 (if t (return-from %f1 f1-2) 606042)))
                                 f1-1)))
                       (flet ((%f16 (f16-1 f16-2)  292))
                         (labels ((%f2 (f2-1 f2-2 f2-3
                                             &optional (f2-4 f1-3) (f2-5 f1-4)
                                             (f2-6 -418207187))
                                       (%f16 2099 (%f16 f1-2 1157))))
                           (return-from %f1 (%f2 f1-4 -12066040 v9 122107)))))))
         (flet ((%f5
                 (f5-1
                  &optional
                  (f5-2 (labels ((%f13 (f13-1 f13-2 f13-3
                                              &optional (f13-4 a) (f13-5 b))
                                       1054213657))
                          (%f13 b 166441 -3)))
                  (f5-3 20102220)
                  (f5-4 (labels ((%f11 (f11-1 f11-2 f11-3)
                                       (%f1 -110148 (%f1 -12336576 f11-1 -61))))
                          (let ((v1 (apply #'%f11 -29706 a b (list))))
                            a))))
                 b))
           (labels ((%f17 (f17-1 f17-2 f17-3
                                 &optional (f17-4 -107566292) (f17-5 63) (f17-6 -2))
                          105656))
             (%f5
              (%f17 185703492 a a -511
                    (%f1 b b -218142
                         (%f17 -240978 2923208 22 (%f5 1542 68917407 a) b)))
              -2018
              -1))))))
   -100 1)
  1)

;;; sbcl 0.8.5.40
;;; Different results from exprs containing ROUND

(deftest misc.182
  (let* ((form '(labels ((%f14 (f14-1 f14-2)
                               (labels ((%f16
                                         (f16-1 f16-2
                                                &optional
                                                (f16-3 (setq f14-1 (ash f14-1 (min 77 b)))))
                                         (logandc2 c -100)))
                                 (return-from %f14 (* 2 (gcd f14-1 (%f16 c f14-1)))))))
                  (round (%f14 c c)
                         (max 83 (%f14 (multiple-value-call #'%f14 (values 0 2)) 0)))))
         (fn1 `(lambda (a b c)
                 (declare (type (integer 5628 8762) a))
                 (declare (type (integer 778 33310188747) b))
                 (declare (type (integer -6699 4554) c))
                 (declare (optimize (speed 3)))
                 (declare (optimize (safety 1)))
                 (declare (optimize (debug 1)))
                 ,form))
         (fn2 `(lambda (a b c)
                 (declare (notinline values max round gcd * logandc2 min ash))
                 (declare (optimize (safety 3)))
                 (declare (optimize (speed 0)))
                 (declare (optimize (debug 0)))
                 ,form))
         (vals '(7395 1602862793 -2384))
         (cfn1 (compile nil fn1))
         (cfn2 (compile nil fn2))
         (result1 (multiple-value-list (apply cfn1 vals)))
         (result2 (multiple-value-list (apply cfn2 vals))))
    (if (equal result1 result2)
        :good
      (values result1 result2)))
  :good)

;;; sbcl 0.8.5.42
;;; failed AVER: "(NOT POPPING)"
;;; Also occurs in cmucl (11/2003 snapshot)

(deftest misc.183
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (type (integer -368154 377964) a))
       (declare (type (integer 5044 14959) b))
       (declare (type (integer -184859815 -8066427) c))
       (declare (ignorable a b c))
       (declare (optimize (speed 3)))
       (declare (optimize (safety 1)))
       (declare (optimize (debug 1)))
       (block b7
         (flet ((%f3 (f3-1 f3-2 f3-3) 0))
           (apply #'%f3 0 (catch 'foo (return-from b7 (%f3 0 b c))) c nil)))))
   0 6000 -9000000)
  0)

(deftest misc.183a
  (let () (apply #'list 1 (list (catch 'a (throw 'a (block b 2))))))
  (1 2))

;;; sbcl 0.8.5.42
;;; failed AVER: "(FUNCTIONAL-LETLIKE-P CLAMBDA)"

(deftest misc.184
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (type (integer 867934833 3293695878) a))
       (declare (type (integer -82111 1776797) b))
       (declare (type (integer -1432413516 54121964) c))
       (declare (optimize (speed 3)))
       (declare (optimize (safety 1)))
       (declare (optimize (debug 1)))
       (if nil
           (flet ((%f15 (f15-1 &optional (f15-2 c))
                        (labels ((%f1 (f1-1 f1-2) 0))
                          (%f1 a 0))))
             (flet ((%f4 ()
                         (multiple-value-call #'%f15
                                              (values (%f15 c 0) (%f15 0)))))
               (if nil (%f4)
                 (flet ((%f8 (f8-1 &optional (f8-2 (%f4)) (f8-3 0))
                             f8-3))
                   0))))
         0)))
   3040851270 1664281 -1340106197)
  0)

;;; sbcl 0.8.5.42
;;; invalid number of arguments: 1
;;; ("XEP for LABELS CL-TEST::%F10" ...

(deftest misc.185
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (type (integer 5 155656586618) a))
       (declare (type (integer -15492 196529) b))
       (declare (type (integer 7 10) c))
       (declare (optimize (speed 3)))
       (declare (optimize (safety 1)))
       (declare (optimize (debug 1)))
       (flet ((%f3
               (f3-1 f3-2 f3-3
                     &optional (f3-4 a) (f3-5 0)
                     (f3-6
                      (labels ((%f10 (f10-1 f10-2 f10-3)
                                     0))
                        (apply #'%f10
                               0
                               a
                               (- (if (equal a b) b (%f10 c a 0))
                                  (catch 'ct2 (throw 'ct2 c)))
                               nil))))
               0))
         (%f3 (%f3 (%f3 b 0 0 0) a 0) a b b b c))))
   5 0 7)
  0)

(deftest misc.185a
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (type (integer -1304066 1995764) a))
       (declare (type (integer -52262604195 5419515202) b))
       (declare (type (integer -13 94521) c))
       (declare (optimize (speed 3)))
       (declare (optimize (safety 1)))
       (declare (optimize (debug 1)))
       (flet ((%f13 (f13-1 f13-2 f13-3)
                    0))
         (apply #'%f13
                (%f13 b 0 0)
                (catch 'ct1 0)
                (catch 'ct2 (throw 'ct2 c))
                nil))))
   0 0 0)
  0)

;;; sbcl 0.8.5.42
;;; Different results

(deftest misc.186
  (let* ((form '(labels ((%f3 (f3-1 f3-2) f3-1))
                  (apply #'%f3 b (catch 'ct8 (throw 'ct8 (logeqv (%f3 c 0)))) nil)))
         (vars '(b c))
         (fn1 `(lambda ,vars
                 (declare (type (integer -2 19) b)
                          (type (integer -1520 218978) c)
                          (optimize (speed 3) (safety 1) (debug 1)))
                 ,form))
         (fn2 `(lambda ,vars
                 (declare (notinline logeqv apply)
                          (optimize (safety 3) (speed 0) (debug 0)))
                 ,form))
         (cf1 (compile nil fn1))
         (cf2 (compile nil fn2))
         (result1 (multiple-value-list (funcall cf1 2 18886)))
         (result2 (multiple-value-list (funcall cf2 2 18886))))
    (if (equal result1 result2)
        :good
      (values result1 result2)))
  :good)

;;; cmucl (11/2003 snapshot)
;;; The assertion (NOT (EQ (C::FUNCTIONAL-KIND C::LEAF) :ESCAPE)) failed.

(deftest misc.187
  (apply
   (eval '(function
           (lambda (a b c)
             (declare (notinline))
             (declare (optimize (safety 3)))
             (declare (optimize (speed 0)))
             (declare (optimize (debug 0)))
             (flet ((%f7 (&optional (f7-1 (catch (quote ct7) 0)) (f7-2 0))
                         c))
               (let ((v8
                      (flet ((%f14 (f14-1 &optional (f14-2 (%f7 b)))
                                   0))
                        0)))
                 (%f7 b))))))
   '(2374299 70496 -6321798384))
  -6321798384)

;;; ecl bug
;;; Segmentation violation

(deftest misc.188
  (funcall
   (compile
    nil
    '(lambda (a b c)
          (declare (notinline floor min funcall))
          (declare (optimize (safety 3) (speed 0) (debug 0)))
          (floor (flet ((%f10 (f10-1 f10-2) b)) (%f10 (%f10 0 0) a))
                 (min -37
                      (labels ((%f6 (f6-1 f6-2 f6-3) b))
                        (funcall #'%f6 b b b))))))
   7187592 -3970792748407 -14760)
  1 0)

;;; Wrong number of arguments passed to an anonymous function
(deftest misc.189
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (optimize (speed 3) (safety 1) (debug 1)))
       (let* ((v7 (labels ((%f13 (f13-1 f13-2 f13-3) 0))
                    (multiple-value-call #'%f13 (values a a a)))))
         (flet ((%f10 nil v7)) (%f10)))))
   1733 3000 1314076)
  0)

;;; gcl bug
;;; Error in FUNCALL [or a callee]: #<compiled-closure 088ab770> is not of type NUMBER.

(deftest misc.190
  (let* ((form '(flet ((%f15 ()
                             (labels ((%f4 (f4-1) 0))
                               (flet ((%f6 (&optional
                                            (f6-2
                                             (logand (apply #'%f4 (list (%f4 0)))
                                                     (round (* a)))))
                                           -284))
                                 (%f6)))))
                  (funcall #'%f15)))
         (fn `(lambda (a b c)
                (declare (notinline values equal abs isqrt < >= byte
                                    mask-field funcall + * logcount logand logior
                                    round list apply min))
                (declare (optimize (safety 3)))
                (declare (optimize (speed 0)))
                (declare (optimize (debug 0)))
                ,form))
         (vals '(538754530150 -199250645748 105109641)))
    (apply (compile nil fn) vals))
  -284)

;;; gcl
;;; Error in COMPILER::CMP-ANON [or a callee]: 0 is not of type FUNCTION.

(deftest misc.191
  (funcall
   (compile
    nil
    '(lambda (a b c)
          (declare (optimize (speed 3) (safety 1)))
          (labels ((%f1 nil c))
            (flet ((%f12 (f12-1)
                         (labels ((%f9 (f9-1 f9-2 f9-3) (%f1)))
                           (apply #'%f9 (%f9 a b 0) a 0 nil))))
              (apply #'%f12 0 nil)))))
   0 0 0)
  0)

;;; acl 6.2 (trial, x86)
;;; Returns incorrect value

(deftest misc.192
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (optimize (speed 3)))
       (declare (optimize (safety 1)))
       (declare (optimize (debug 1)))
       (flet ((%f8 (f8-1 f8-2 f8-3) f8-2))
         (catch 'ct6 (%f8 0 b (catch 'ct6 (throw 'ct6 a)))))))
   1 2)
  2)

(deftest misc.193
  (let* ((form '(if (if (<= a (truncate c (min -43 b)))
                        (logbitp 0 0) (logbitp 0 -1))
                    0 -36223))
         (fn1 `(lambda (a b c)
                 (declare (type (integer -3 15350342) a))
                 (declare (type (integer -4357 -1555) b))
                 (declare (type (integer 5389300879793 6422214587951) c))
                 (declare (optimize (speed 3)))
                 (declare (optimize (safety 1)))
                 (declare (optimize (debug 1)))
                 ,form))
         (fn2 `(lambda (a b c)
                 (declare (notinline logbitp min truncate <=))
                 (declare (optimize (safety 3)))
                 (declare (optimize (speed 0)))
                 (declare (optimize (debug 0)))
                 ,form))
         (vals '(7792101 -1615 6070931814551))
         (result1 (multiple-value-list (apply (compile nil fn1) vals)))
         (result2 (multiple-value-list (apply (compile nil fn2) vals))))
    (if (equal result1 result2)
        :good
      (values result1 result2)))
  :good)

;;; cmucl (4 Nov 2003 snapshot)
;;;  The assertion (EQ (C::TN-ENVIRONMENT C:TN) C::TN-ENV) failed.

(deftest misc.194
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (notinline funcall))
       (declare (optimize (safety 3) (speed 0) (debug 3)))
       (flet ((%f14
               (f14-1 f14-2
                      &optional (f14-3 0) (f14-4 (catch 'ct8 0))
                      (f14-5 (unwind-protect c)))
               0))
         (funcall #'%f14 0 0))))
   1 2 3)
  0)

;;; incorrect value (in cmucl)
(deftest misc.195
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -5906488825 254936878485) a))
       (declare (type (integer -350857549 -11423) b))
       (declare (ignorable a b))
       (declare (optimize (speed 3) (safety 1) (debug 1)))
       (block b8
         (labels ((%f6 (f6-1 &optional (f6-2 0) (f6-3 0) (f6-4 0))
                       0))
           (multiple-value-call #'%f6 (values 0))))))
   100 -100000)
  0)

;;; NIL is not of type C::ENVIRONMENT
(deftest misc.196
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer 1 46794484349) a))
       (declare (type (integer -627 -2) b))
       (declare (ignorable a b))
       (declare (optimize (speed 3) (safety 1) (debug 1)))
       (if (not (logbitp 0 0))
           0
         (labels ((%f9 (f9-1 f9-2 f9-3)
                       0))
           (%f9 (catch 'ct6 a) (catch 'ct4 0) 0)))))
   1 -200)
  0)

;;; The assertion (EQ (C::TN-ENVIRONMENT C:TN) C::TN-ENV) failed.
(deftest misc.197
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (notinline logcount))
       (declare (optimize (safety 3) (speed 0) (debug 3)))
       (labels ((%f5 (&optional (f5-1 b) (f5-2 0) (f5-3 (catch (quote ct2) 0)))
                     (prog1 (logcount (block b1 f5-1)))))
         (if (%f5 0 0 0) (%f5 a) 0))))
   1 2)
  1)

;;; gcl bug (30-11-2003)
;;; Different results
;;; These tests appear to be for the same bug.

(deftest misc.198
  (let* ((form '(min (catch 'ct4 (throw 'ct4 (setq c 29119897960))) c))
         (fn1 `(lambda (c)
                 (declare (type (integer -70450 39128850560) c))
                 (declare (optimize (speed 3) (safety 1)))
                 ,form)))
    (funcall (compile nil fn1) 3512352656))
  29119897960)

(deftest misc.199
  (let* ((fn '(lambda (b)
                (declare (type (integer 3352138624 13120037248) b))
                (declare (optimize (speed 3) (safety 1) (space 1)))
                (progn (catch 'ct1
                         (progn (setq b 11159349376)
                                (throw 'ct1 0)))
                       b))))
    (funcall (compile nil fn) 4108962100))
  11159349376)

;;; sbcl
;;; "The value 0 is not of type REAL."  (???)

(deftest misc.200
  (funcall
   (compile nil '(lambda ()
                   (declare (optimize (speed 1) (space 0) (safety 3) (debug 3) (compilation-speed 1)))
                   (ceiling
                    (ceiling
                     (flet ((%f16 () 0)) (%f16)))))))
  0 0)

;;; ecl 5 Dec 2003
;;; Wrong number of arguments passed to an anonymous function

(deftest misc.201
  ;; form to be evaluated
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (optimize (speed 1) (space 0) (safety 0) (debug 2)
                          (compilation-speed 1)))
       (flet ((%f10 (f10-1) (return-from %f10 a)))
         (multiple-value-call #'%f10 (values b)))))
   10 -100)
  ;; expected return value
  10)

;;; Does not terminate?
(deftest misc.202
  (funcall
   (compile
    nil
    '(lambda (a b c)
             (declare (type (integer -363953100 5324773015552) a))
             (declare (type (integer -5744998440960 59520311) b))
             (declare (type (integer -1864645998 -14608) c))
             (declare (ignorable a b c))
             (declare (optimize (speed 3) (space 0) (safety 2) (debug 0)
                                (compilation-speed 2)))
             (flet ((%f1 (f1-1 f1-2)
                         (labels ((%f1 (f1-1 f1-2) 0)) (%f1 a f1-2))))
                   (%f1 0 c))))
   10 20 -20000)
  0)

;;; #<illegal pointer bfffe90c>
(deftest misc.203
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (optimize (speed 3) (space 1) (safety 2) (debug 0)
                          (compilation-speed 0)))
       (labels ((%f18 (f18-1 f18-2 &optional (f18-3 a) (f18-4 a))
                      f18-2))
         (multiple-value-call #'%f18 (values a 0)))))
   100)
  0)

;;; `env0' undeclared (first use in this function)
(deftest misc.204
  (funcall
   (compile
    nil
    '(lambda (a b)
          (declare (type (integer -4801373 -50300) a))
          (declare (type (integer -62 -28) b))
          (declare (ignorable a b))
          (declare (optimize (speed 1) (space 3) (safety 3) (debug 2)
                       (compilation-speed 2)))
          (flet ((%f12 (f12-1) 0))
            (labels ((%f3 (f3-1 f3-2 f3-3 &optional (f3-4 b) (f3-5 b)
                                (f3-6 (labels ((%f9 nil b))
                                        (apply #'%f12 (%f9) nil))))
                          (%f12 0)))
              (%f3 b 0 a)))))
   -2224841 -54)
  0)

;;; #<compiled-function 08405fd8> is not of type INTEGER.
(deftest misc.205
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (optimize (speed 3) (space 1) (safety 1) (debug 0)
                          (compilation-speed 3)))
       (labels ((%f1 nil b))
         (flet ((%f11 (f11-1 f11-2 f11-3) 0))
           (apply #'%f11 a (logand (%f1))
                  (flet ((%f13 (f13-1 f13-2) b))
                    (apply #'%f13 0 0 nil))
                  nil)))))
   100 200)
  0)

;;; #<bytecompiled-function %F14> is not of type INTEGER.
(deftest misc.206
  (funcall
   #'(lambda (a b)
       (declare (notinline mask-field byte))
       (declare (optimize (speed 1) (space 1) (safety 2) (debug 1)
                          (compilation-speed 2)))
       (mask-field (byte 0 0)
                   (block b3
                     (labels ((%f14 nil (return-from b3 a))) (%f14)))))
   1 2)
  0)

;;; #<compiled-closure 08405fd8> is not of type INTEGER
(deftest misc.207
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (optimize (speed 3) (space 2) (safety 0) (debug 1)
                          (compilation-speed 0)))
       (labels ((%f3 (f3-1) a))
         (labels ((%f10 (f10-1 f10-2) a))
           (apply #'%f10 0 (logior (%f3 0)) nil)))))
   -10000)
  -10000)

;;; `env0' undeclared (first use in this function)
(deftest misc.208
  (funcall
   (compile
    nil
    '(lambda (b)
       (declare (optimize (speed 3) (space 2) (safety 3) (debug 3)
                          (compilation-speed 0)))
       (flet ((%f6 (f6-1 f6-2 f6-3) f6-3))
         (labels ((%f8 (f8-1) (let* ((v1 (%f6 0 0 0))) 0)))
           (apply #'%f6 b b (%f8 b) nil)))))
   10)
  0)

;;; Wrong value computed
(deftest misc.209
  (funcall
   (compile
    nil
    '(lambda (b)
       (declare (optimize (speed 3) (space 2) (safety 3) (debug 3)
                          (compilation-speed 3)))
       (max (catch 'ct4 (throw 'ct4 (setq b 0))) b)))
   6353)
  0)

;;; Wrong value computed
(deftest misc.210
  (funcall
   (compile
    nil
    '(lambda (c)
       (declare (type (integer 3 65500689) c))
       (declare (optimize (speed 2) (space 1) (safety 3) (debug 3)
                          (compilation-speed 2)))
       (catch 'ct6
         (let ((v10 (truncate (integer-length (throw 'ct6 0))))) c))))
   100)
  0)

(deftest misc.210a
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer -55982525 -1) a))
       (declare (optimize (speed 1) (space 2) (safety 1) (debug 2)
                          (compilation-speed 1)))
       (flet ((%f11 (f11-1 f11-2 f11-3) a))
         (let ((v6 0))
           (flet ((%f12 (f12-1) v6))
             (if (<= 0) (%f11 v6 0 0)
               (multiple-value-call #'%f11
                                    (values 0 0 (%f11 0 0 (apply #'%f12 0 nil))))))))))
   -100)
  -100)

;;; Segmentation violation
(deftest misc.211
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (type (integer -1439706333184 1191686946816) a))
       (declare (type (integer -28 282229324) b))
       (declare (type (integer -108149896 38889958912) c))
       (declare (optimize (speed 3) (space 1) (safety 2) (debug 1)
                          (compilation-speed 3)))
       (let ((v4 (labels ((%f8 (f8-1 &optional (f8-2 0) (f8-3 b)) 0))
                   (logior (%f8 0) (%f8 0 0)))))
         (truncate
          (labels ((%f4 (&optional (f4-1 (ceiling c))) a))
            (%f4 v4))
          (max 38
               (labels ((%f8 (f8-1 &optional (f8-2 (+ c a))) 0))
                 (apply #'%f8 a nil)))))))
   -979021452526 138874383 21099308459)
  -25763722434
  -34)

;;; Wrong value returned
(deftest misc.212
  (funcall #'(lambda ()
               (declare (optimize (speed 2) (space 0) (safety 3) (debug 2)
                                  (compilation-speed 0)))
               (let* ((v9 (unwind-protect 0))) v9)))
  0)

;;; segmentation violation
(deftest misc.213
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -2 -1) b))
       (declare (optimize (speed 1) (space 0) (safety 1) (debug 1)
                          (compilation-speed 3)))
       (max (labels ((%f15 (f15-1) b)) (if (< 0 (%f15 a)) 0 0))
            (labels ((%f11 (f11-1 f11-2 f11-3) b))
              (apply #'%f11 0 0 0 nil)))))
   0 -2)
  0)

(deftest misc.213a
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (optimize (speed 3) (space 3) (safety 0) (debug 1)
                          (compilation-speed 3)))
       (max (labels ((%f7 (f7-1) a)) (%f7 0))
            (flet ((%f12 (f12-1 f12-2) (if a f12-2 0)))
              (apply #'%f12 0 a nil)))))
   123)
  123)

;;; Wrong value
(deftest misc.214
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (optimize (speed 3) (space 1) (safety 2) (debug 0)
                          (compilation-speed 2)))
       (flet ((%f8 nil (setq a 0))) (max a (%f8)))))
   100)
  100)

;;; Wrong value
(deftest misc.215
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (speed 3) (space 3) (safety 3) (debug 0)
                          (compilation-speed 2)))
       (ldb (byte 26 6) -1252))))
  67108844)

(deftest misc.215a
  (funcall
   (compile nil '(lambda ()
                   (declare (optimize (speed 3) (space 2) (safety 2) (debug 1)
                                      (compilation-speed 2)))
                   (ldb (byte 30 0) -407020740))))
  666721084)

;;; Floating point exception
(deftest misc.216
  (truncate 0 -2549795210)
  0 0)

(deftest misc.217
  (ceiling 0 -2549795210)
  0 0)
    
(deftest misc.218
  (floor 0 -2549795210)
  0 0)

;;; Infinite loop
(deftest misc.219
  (funcall
   (compile
    nil
    '(lambda ()
       (labels ((%f (a b)
                    (labels ((%f (c d) 0))
                      (%f 1 2))))
         (%f 3 4)))))
  0)

;;; #\^E is not of type NUMBER.
(deftest misc.220
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -3218770816 9386121) a))
       (declare (type (integer -1 13) b))
       (declare (ignorable a b))
       (declare (optimize (speed 2) (space 1) (safety 1) (debug 0)
                          (compilation-speed 1)))
       (labels ((%f18 (f18-1 f18-2 f18-3) a))
         (apply #'%f18 0 a
                (%f18 b
                      (- (labels ((%f11 (f11-1 f11-2 f11-3) a))
                           (%f11 0 0 0)))
                      a)
                nil))))
   -468614602 3)
  -468614602)

;;; Floating point exception
(deftest misc.221
  (truncate 0 3006819284014656913408)
  0 0)

(deftest misc.222
  (ceiling 0 3006819284014656913408)
  0 0)

(deftest misc.223
  (floor 0 3006819284014656913408)
  0 0)

;;; clisp (10 Dec 2003 cvs head)
;;; *** - SYMBOL-VALUE: 2 is not a SYMBOL

(deftest misc.224
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (optimize (speed 2) (space 3) (safety 0)
                          (debug 1) (compilation-speed 0)))
       (flet ((%f14 (f14-1 f14-2
                           &optional (f14-3 c)
                           (f14-4 (if (not nil)
                                      (labels ((%f9 nil 0)) (%f9))
                                    a)))
                    (flet ((%f17 (f17-1 f17-2) f14-1)) (%f17 0 f14-3))))
         (%f14 (%f14 0 a) 0 b a))))
   248000 5409415 227923)
  0)

;;; Wrong values
(deftest misc.225
  (funcall (compile nil '(lambda () (values (values 'a 'b)))))
  a)

;;; clisp (12 Dec 2003, 2:30AM CST cvs head)
;;; SYMBOL-VALUE: 1 is not a SYMBOL
(deftest misc.226
  (funcall
   (compile nil
            '(lambda (a)
               (flet ((%f (&optional (x (setq a 1))
                                     (y (setq a 2)))
                           0))
                 (%f 0 0))))
   0)
  0)

(deftest misc.227
  (funcall
   (compile
    nil
    '(lambda (b)
       (flet ((%f (&optional x (y (setq b 1))) x))
         (%f 0))))
   0)
  0)

;;; acl (x86 linux 6.2, patched 12 Dec 2003)
;;; No from-creg to move to...
(deftest misc.228
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (optimize (speed 1) (space 2) (safety 0) (debug 2)
                          (compilation-speed 2)))
       (catch 'ct2
         (case 0
           ((-56087 86404 -94716)
            (signum (labels ((%f7 (f7-1 f7-2 f7-3) f7-2)) 0)))
           ((12986)
            (let ((v3 (catch 'ct2 (throw 'ct2 0))))
              (labels ((%f14 (f14-1 f14-2) 0)) (%f14 b c))))
           (t 0)))))
   -3847091255 -13482 -7577750)
  0)

(deftest misc.228a
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (type (integer -249606 2) a))
       (declare (type (integer 125 511) b))
       (declare (type (integer -2 1) c))
       (declare (ignorable a b c))
       (declare (optimize (speed 2) (space 2) (safety 1) (debug 3)
                          (compilation-speed 3)))
       (catch 'ct4
         (rational (case b
                     ((350 244 1059) (prog2 (numerator c) 0))
                     ((1705 493)
                      (unwind-protect
                          (throw 'ct4 c)
                        (loop for lv2 below 2 count (logbitp 0 c))))
                     (t a))))))
   0 200 -1)
  0)

(deftest misc.228b
  (funcall
   (compile
    nil
    '(lambda (c)
       (declare (type (integer -1 412413109) c))
       (declare (optimize (speed 1) (space 2) (safety 2) (debug 1)
                          (compilation-speed 3)))
       (catch 'ct2
         (logior (* (progn (if c 0 (throw 'ct2 0)) 0)
                    (catch 'ct2 (throw 'ct2 0)))
                 (complex c 0)
                 ))))
   62151)
  62151)

;;; Error: `T' is not of the expected type `INTEGER'
(deftest misc.229
  (funcall
   (compile
    nil
    '(lambda nil
       (declare (optimize (speed 2) (space 1) (safety 1) (debug 2)
                          (compilation-speed 3)))
       (labels ((%f15 (f15-1) 0))
         (let ((v4 0))
           (catch 'ct5
             (%f15
              (gcd (catch 'ct5 (let* ((v5 (throw 'ct5 0))) 0)) v4))))))))
  0)

;;; ecl
;;; Wrong result (order of evaluation problem)

(deftest misc.230
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer -6527559920 -247050) a))
       (declare (optimize (speed 1) (space 3) (safety 0) (debug 0)
                          (compilation-speed 3)))
       (labels ((%f10 (&optional (f10-1 0)
                                 (f10-2 (setq a -4456327156)))
                      0))
         (logxor a (%f10 a)))))
   -3444248334)
  -3444248334)

;;; cmucl
;;; Wrong value

(deftest misc.231
  (funcall
   (compile
    nil
    '(lambda (b)
       (declare (type (integer -5209401 3339878) b))
       (declare (optimize (speed 1) (space 2) (safety 0) (debug 2)
                          (compilation-speed 3)))
       (flet ((%f3 (f3-1 f3-2)
                   f3-1))
         (apply #'%f3 0 (logxor (catch 'ct2 b)
                                (catch 'ct5 (throw 'ct5 0))) nil))))
   -2179757)
  0)

;;; Invalid number of arguments: 1
(deftest misc.232
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer 197447754 495807327) a))
       (declare (type (integer -125379462 1863191461) b))
       (declare
        (optimize (speed 2) (space 2) (safety 2) (debug 1) (compilation-speed 2)))
       (flet ((%f8
               (&optional
                (f8-1
                 (max (catch (quote ct4) 0)
                      (catch (quote ct6) (throw (quote ct6) 0)))))
               b))
         (flet ((%f16 (f16-1 f16-2 f16-3)
                      0))
           (apply #'%f16 a 0 (%f8) nil)))))
   348270365 28780966)
  0)

;;;  The assertion (EQ C::CHECK :SIMPLE) failed.
(deftest misc.233
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -2333758327203 -321096206070) a))
       (declare (type (integer -2842843403569 258395684270) b))
       (declare (optimize (speed 2) (space 0) (safety 1) (debug 2)
                          (compilation-speed 2)))
       (flet ((%f18 (f18-1)
                    (the integer
                      (labels ((%f9 (f9-1 f9-2)
                                    (* (- -1 -210032251)
                                       (1+
                                        (floor
                                         (labels ((%f11 (f11-1 f11-2)
                                                        -96773966))
                                           (%f11 b -3440758)))))))
                        (flet ((%f2
                                (f2-1 f2-2 f2-3
                                      &optional (f2-4 (%f9 -429204 -63))
                                      (f2-5 (- (%f9 b 17) a))
                                      (f2-6
                                       (multiple-value-call #'%f9
                                                            (values
                                                             (let () 7127585)
                                                             (flet ((%f1 (f1-1 f1-2 f1-3)
                                                                         (catch 'ct6 -569234)))
                                                               (macrolet () 13))))))
                                1027))
                          (if nil
                              (%f2 b a f18-1 69968 4 -217193265)
                            (catch 'ct1 129548688)))))))
         (max (apply #'%f18 (list 0))))))
   -2067244683733 143879071206)
  129548688)

;;;  NIL is not of type C::TAIL-SET
(deftest misc.234
  (funcall
   (compile
    nil
    '(lambda (b) ;; (a b)
       (declare (type (integer -13583709 -3876310) b))
       (declare (optimize (speed 1) (space 2) (safety 3) (debug 3)
                          (compilation-speed 1)))
       (flet ((%f14 (f14-1 f14-2 f14-3)
                    (flet ((%f2
                            (f2-1
                             &optional (f2-2 0) (f2-3 0)
                             (f2-4
                              (block b8 (if (ldb-test (byte 0 0) 0) (* 0 f14-2) 0))))
                            0))
                      (%f2 b f14-2))))
         (%f14 0 0 (%f14 0 0 0)))))
   ;; -155589
   -5694124)
  0)

;;; sbcl 0.8.6.34
;;; Wrong value
(deftest misc.235
  (funcall
   (compile
    nil
    '(lambda (b)
         (declare (notinline not))
         (declare (optimize (speed 1) (space 0) (safety 0)
                            (debug 2) (compilation-speed 3)))
         (multiple-value-prog1 0 (catch 'ct2 (if (not nil) (throw 'ct2 b) 0)))))
   :wrong)
  0)

(deftest misc.236
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (optimize (speed 1) (space 0) (safety 3)
                          (debug 0) (compilation-speed 1)))
       (flet ((%f8 (f8-1) 0))
         (labels ((%f18 (f18-1 f18-2 &optional (f18-3 0))
                        (%f8 (catch 'ct7 (throw 'ct7 f18-1)))))
           (multiple-value-prog1 (catch 'ct7 a)
                                 0
                                 (multiple-value-call #'%f18 (values 0 (%f8 b))))))))
   :good :bad)
  :good)

(deftest misc.237
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (speed 2) (space 2) (safety 3)
                          (debug 3) (compilation-speed 1)))
       (multiple-value-prog1 0
                             (catch 'ct7 (logandc1 (block b7 0)
                                                   (throw 'ct7 -908543)))))))
  0)

;;; cmucl (11 2003 snapshot)

;;;  NIL is not of type C::CONTINUATION
(deftest misc.238
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer -77145797 -1) a))
       (declare (optimize (speed 1) (space 1) (safety 3) (debug 3) (compilation-speed 1)))
       (flet ((%f5 (f5-1) a))
         (%f5 (unwind-protect 0 (logand (- (catch 'ct5 0))))))))
   -100)
  -100)

(deftest misc.238a
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (speed 3) (space 2) (safety 3)
                          (debug 0) (compilation-speed 0)))
       (min (load-time-value 0))
       0)))
  0)

;;; (in C::MAYBE-LET-CONVERT)
(deftest misc.239
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer -2315418108387 111852261677) a))
       (declare (optimize (speed 2) (space 0) (safety 2) (debug 2) (compilation-speed 1)))
       (labels ((%f4 ()
                     (labels ((%f16 (f16-1 f16-2) 0))
                       (flet ((%f9 () 0))
                         (%f16 0 (%f16 (%f9) a))))))
         (flet ((%f10 (f10-1 f10-2 f10-3)
                      (flet ((%f15 (f15-1 &optional (f15-2 (%f4)) (f15-3 0)) f15-3))
                        0)))
           0))))
   100)
  0)

(deftest misc.239a
  (funcall
   (compile nil '(lambda () (declare (optimize speed safety))
                   (LET ((x (PROG1 0 (ROUND 18916)))) (catch 'ct4 0)))))
  0)

(deftest misc.240
  (funcall
   (compile
    nil
    '(lambda (b)
       (declare (type (integer 4 7) b))
       (declare (optimize (speed 2) (space 3) (safety 3) (debug 1) (compilation-speed 3)))
       (unwind-protect 0
         (common-lisp:handler-case
          (max
           (let ((*s1* b))
             (declare (special *s1*))
             (+ 0 *s1*)))))))
   5)
  0)

;;; clisp (12 Dec 2003 cvs head)
;;; *** - Compiler bug!! Occurred in ASSEMBLE-LAP at ILLEGAL INSTRUCTION.
(deftest misc.241
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (speed 3) (space 3) (safety 0) (debug 0) (compilation-speed 2)))
       (labels ((%f17 (f17-1 f17-2) (multiple-value-prog1 0 0 0 (return-from %f17 0)))) (%f17 0 0)))))
  0)

(deftest misc.242
  (funcall (compile nil '(lambda (a) (block b6 (multiple-value-prog1 a (return-from b6 0))))) :wrong)
  0)

(deftest misc.243
  (funcall (compile nil '(lambda () (block b3 (return-from b3 (multiple-value-prog1 0 (return-from b3 0)))))))
  0)

;;; lispworks 4.3 (personal edition)
(deftest misc.244
  (funcall
   (compile
    nil
    '(lambda (b)
       (declare (optimize (speed 3) (space 1) (safety 2) (debug 3) (compilation-speed 2)))
       (catch 'ct8 (labels ((%f4 (&optional (f4-1 0) (f4-2 (throw 'ct8 0))) f4-1)) (%f4 b)))))
   :wrong)
  0)

(deftest misc.245
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (optimize (speed 2) (space 0) (safety 0) (debug 2) (compilation-speed 1)))
       (catch 'ct2 (labels ((%f11 (&optional (f11-1 (throw 'ct2 0))) a)) (apply #'%f11 0 nil)))))
   20)
  20)

;;; ecl (cvs head, 18 Dec 2003)
(deftest misc.246
  (let ((x (unwind-protect 0))) x)
  0)

(deftest misc.247
  (let ((x (dotimes (i 0 10)))) x)
  10)

;;; acl 6.2 trial
;;; "Error: Attempt to access the plist field of 0 which is not a symbol."

(deftest misc.248
  (funcall (compile nil '(lambda () (dotimes (i 0 0) 0))))
  0)

;;; sbcl
;;;  #<SB-C::LAMBDA-VAR :%SOURCE-NAME IV4 :TYPE # {A67BE29}>
;;;  not found in
;;;  #<SB-C::PHYSENV :LAMBDA # :NLX-INFO # {A680EF1}>

(deftest misc.249
  (funcall
   (compile
    nil
    '(lambda (a b)
         (declare (notinline <=))
         (declare (optimize (speed 2) (space 3) (safety 0)
                            (debug 1) (compilation-speed 3)))
         (if (if (<= 0) nil nil)
             (labels ((%f9 (f9-1 f9-2 f9-3)
                           (ignore-errors 0)))
               (dotimes (iv4 5 a) (%f9 0 0 b)))
           0)))
   1 2)
  0)

;;; cmucl 11/2003
(deftest misc.250
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer -12 14) a))
       (declare        (optimize (speed 1) (space 2) (safety 1) (debug 1) (compilation-speed 3)))
       (let ((v6 0))
         (flet ((%f11
                 (f11-1
                  &optional
                  (f11-2
                   (case (catch 'ct7 0)
                     (t
                      (let* ((v2 (ignore-errors a)))
                        v6)))))
                 0))
           (%f11 0 0)))))
   5)
  0)

;;; NIL is not of type C::CONTINUATION
;;; (C::MAYBE-CONVERT-TO-ASSIGNMENT
;;;  #<LAMBDA #x48A3031D
;;;     NAME= (FLET %F3
;;;             NIL)
;;;     TYPE= #<KERNEL::BUILT-IN-CLASS FUNCTION (read-only) {2803AE6D}>
;;;     WHERE-FROM= :DEFINED
;;;     VARS= (F3-1 F3-2 F3-3)>)

(deftest misc.251
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -186585769 -7483) a))
       (declare (type (integer -550 524) b))
       (declare
        (optimize (speed 2) (space 1) (safety 1) (debug 2) (compilation-speed 3)))
       (flet ((%f3 (f3-1 f3-2 f3-3)
                   0))
         (%f3 0 0
              (flet ((%f13 (f13-1 f13-2)
                           0))
                (if (/= b a)
                    b
                  (deposit-field (%f3 0 b 0) (byte 0 0) (%f3 0 0 (%f13 0 0)))))))))
   -10000 0)
  0)

;;; 8061593093 is not of type (INTEGER -2147483648 4294967295)
(deftest misc.252
  (funcall
   (compile
    nil
    '(lambda (b)
       (declare (type (integer -43443 9126488423) b))
       (declare (optimize (speed 3) (space 1) (safety 1) (debug 3) (compilation-speed 0)))
       (logand (setq b 8061593093) (min b 0))))
   0)
  0)

(deftest misc.252a
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -30189 -6047) a))
       (declare (type (integer -10 16391481067) b))
       (declare (optimize (speed 3) (space 1) (safety 2) (debug 3)
                          (compilation-speed 0)))
       (if
           (<= 0
               (let ((*s1* (setq b 12204309028)))
                 (declare (special *s1*))
                 (truncate b)))
           a
         0)))
   -12618 16130777867)
  -12618)

;;; #<C::ENTRY #x48DCC585> fell through ETYPECASE expression.
;;; Wanted one of (C::BASIC-COMBINATION C::EXIT C::CRETURN C::CSET C::CIF
;;;                (OR C::REF C:BIND)).
;;;    [Condition of type CONDITIONS::CASE-FAILURE]
;;;[...]
;;; (C::SUBSTITUTE-CONTINUATION #<Continuation c1> #<Continuation c2>)

(deftest misc.253
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (speed 2) (space 2) (safety 3) (debug 3) (compilation-speed 2)))
       (flet ((%f17 (f17-1) 0))
         (%f17 (logandc1 0 (catch 'ct2 0)))))))
  0)

(deftest misc.253a
  (funcall
   (compile
    nil
    '(lambda (c)
       (declare (optimize (speed 1) (space 1) (safety 3) (debug 2)
                          (compilation-speed 1)))
       (labels ((%f1 (f1-1 f1-2)
                     (isqrt (abs (complex f1-1 0)))))
         (progn
           (/
            (multiple-value-call #'%f1
                                 (values (1- (restart-bind nil 1416182210))
                                         123337746))
            1)
           (tagbody)
           c))))
   -34661)
  -34661)

;;; Wrong return value
(deftest misc.254
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer -5241 -1159) a))
       (declare (optimize (speed 3) (space 2) (safety 0) (debug 3) (compilation-speed 1)))
       (gcd a
            (let ((*misc.254* (setq a -4929)))  ;; special variable
              (declare (special *misc.254*))
              0))))
   -3000)
  3000)

(deftest misc.255
  (funcall
   (compile
    nil
    '(lambda (b)
       (declare (type (integer -3474321 15089206) b))
       (declare (optimize (speed 3) (space 3) (safety 3) (debug 3) (compilation-speed 0)))
       (- b
          (block b3
            (setq b 9367613)
            0))))
   10)
  10)

;;; clisp (20 Dec 2003)
;;; Bug involving tagbody and go in lexical function

(deftest misc.256
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (speed 3) (space 0) (safety 3) (debug 0) (compilation-speed 0)))
       (tagbody (flet ((%f6 () (go 18))) (%f6))
                18))))
  nil)

;;; clisp (22 Dec 2003)
;;; *** - Compiler bug!! Occurred in ACCESS-IN-STACK at STACKZ-END.

(deftest misc.257
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (speed 1) (space 2) (safety 3) (debug 3) (compilation-speed 1)))
       (declare (special b))
       (tagbody (flet ((%f1 (f1-1)
                            (flet ((%f9 (&optional (f9-1 b) (f9-2 (go tag2)) (f9-3 0)) 0))
                              (%f9 0 0 0))))
                  (%f1 0))
                tag2))))
  nil)

;;; clisp (26 Dec 2003)
;;; PROGV binding is not having the correct effect in compiled code

(deftest misc.258
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (speed 1) (space 2) (safety 1) (debug 1)
                          (compilation-speed 0)))
       (let ((*s4* :right))
         (declare (special *s4*))
         (progv '(*s4*) (list :wrong1) (setq *s4* :wrong2))
         *s4*))))
  :right)

;;; sbcl 0.8.7.5
;;; The value 215067723 is not of type (INTEGER 177547470 226026978).

(deftest misc.259
  (funcall
   (compile
    nil
    '(lambda (a)
         (declare (type (integer 177547470 226026978) a))
         (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)
                            (compilation-speed 1)))
         (logand a (* a 438810))))
   215067723)
  13739018)

(deftest misc.260
  (funcall
   (compile
    nil
    '(lambda (a)
         (declare (type (integer 43369342 45325981) a))
         (declare (optimize (speed 2) (space 0) (safety 2)
                            (debug 0) (compilation-speed 3)))
         (logand 0 (* 5459177 a))))
   44219966)
  0)

(deftest misc.261
  (funcall
   (compile
    nil
    '(lambda (b)
         (declare (type (integer 379442022 806547932) b))
         (declare (optimize (speed 2) (space 0) (safety 0)
                            (debug 3) (compilation-speed 2)))
         (logand b (* 227 b))))
   551173513)
  545263625)

(deftest misc.262
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer 515644 54674673) a))
       (declare (optimize (speed 3) (space 2) (safety 3)
                          (debug 0) (compilation-speed 1)))
       (mask-field (byte 0 0) (* 613783109 a))))
   28831407)
  0)

(deftest misc.263
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer 862944 60462138) a))
       (declare (optimize (speed 3) (space 3) (safety 0)
                          (debug 1) (compilation-speed 1)))
       (logandc2 0 (* a 18094747))))
   36157847)
  0)

(deftest misc.264
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer 896520522 1249309734) a))
       (declare (optimize (speed 3) (space 3) (safety 1)
                          (debug 1) (compilation-speed 2)))
       (lognand 0 (* a 1381212086))))
   1202966173)
  -1)

;;; sbcl 0.8.7.6
;;; Lisp error during constant folding:
;;; The function SB-VM::%LEA-MOD32 is undefined.

(deftest misc.265
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer -19621 11895) a))
       (declare (optimize (speed 3) (space 2) (safety 3)
                          (debug 3) (compilation-speed 3)))
       (* 0 a 103754)))
   1)
  0)

;;; ecl (10 jan 2004)
;;; A bug was found in the compiler.  Contact worm@arrakis.es.
;;; Broken at C::C2GO.

(deftest misc.266
  (funcall
   (compile
    nil
    '(lambda ()
       (tagbody
        (flet ((%f (x) :bad)) (multiple-value-call #'%f (go done)))
        done))))
  nil)

(deftest misc.266a
  (funcall
   (compile
    nil
    '(lambda (b)
       (declare (type (integer -14356828946432 -24266) b))
       (declare (optimize (speed 3) (space 1) (safety 1) (debug 3)
                          (compilation-speed 2)))
       (progn (tagbody (unwind-protect 0 (go 3)) 3) b)))
   -30000)
  -30000)

;;; Broken at C::C2VAR.
(deftest misc.266b
  (funcall
   (compile
    nil
    '(lambda (b)
       (declare (optimize (speed 2) (space 3) (safety 2) (debug 0)
                          (compilation-speed 0)))
       (unwind-protect 0 (catch 'ct7 (prog1 b 0)))))
   1)
  0)

;;; Incorrect return value

(deftest misc.267
  (locally (declare (special *s5*))
           (let ((v8 (progv '(*s5*) (list 0) (if t *s5* *s5*))))
             v8))
  0)

(deftest misc.267a
  (let ((x (progv nil nil 0))) x)
  0)

(deftest misc.268
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (speed 1) (space 1) (safety 3) (debug 2)
                          (compilation-speed 2)))
       (catch 'ct7
         (rationalize (let ((v9 (1+ (throw 'ct7 0)))) 48955))))))
  0)

(deftest misc.269
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer -1 20) a))
       (declare (optimize (speed 3) (space 1) (safety 2) (debug 2)
                          (compilation-speed 3)))
       (if (if a (logbitp 34 a) nil) 0 -230678)))
   14)
  -230678)

(deftest misc.270
  (let ((*s3* (dotimes (iv4 0 10) (if t iv4 8))))
    (declare (special *s3*))
    *s3*)
  10)

(deftest misc.271
  (let ((v2 (unwind-protect 0))) v2)
  0)

;;; wrong number of values passed to anonymous function

(deftest misc.272
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (speed 3) (space 1) (safety 0) (debug 3)
                          (compilation-speed 2)))
       (flet ((%f17 (f17-1) 1))
         (multiple-value-call #'%f17 (values (floor 0)))))))
  1)

;;; clisp (10 jan 2004)
;;; Improper handling of a jump to an exit point from unwind-protect
;;; (see CLHS section 5.2)

(deftest misc.273
  (funcall
   (compile
    nil
    '(lambda (d)
       (declare (optimize (speed 3) (space 0) (safety 3)
                          (debug 2) (compilation-speed 0)))
       (gcd 39
            (catch 'ct2
              (block b7
                (throw 'ct2
                       (unwind-protect
                           (return-from b7 17)
                         (return-from b7
                           (progv '(*s6*) (list 31) d))
                         )))))))
   65)
  13)

;;; sbcl 0.8.7.13
;;; Lexical unwinding of UVL stack is not implemented.

(deftest misc.274
  (funcall
   (compile
    nil
    '(lambda ()
       (declare
        (optimize (speed 2)
                  (space 2)
                  (safety 1)
                  (debug 2)
                  (compilation-speed 0)))
       (multiple-value-prog1
        (ignore-errors 0)
        0
        (catch 'ct7 0)
        (catch 'ct1
          (catch 'ct4
            (complex (throw 'ct4 (dotimes (iv4 0 0) (throw 'ct1 0))) 0)))))))
  0)

(deftest misc.274a
  (funcall
   (compile
    nil
    '(lambda ()
         (declare (optimize (speed 3) (space 1) (safety 3)
                            (debug 1) (compilation-speed 3)))
         (dotimes (iv4 3 0)
           (apply (constantly 0)
                  0
                  (catch 'ct2 (throw 'ct2 (rem 0 (max 46 0))))
                  nil)))))
  0)


;;;  failed AVER: "SUCC"

(deftest misc.275
  (funcall
   (compile
    nil
    '(lambda (b)
       (declare (notinline funcall min coerce))
       (declare
        (optimize (speed 1)
                  (space 2)
                  (safety 2)
                  (debug 1)
                  (compilation-speed 1)))
       (flet ((%f12 (f12-1)
                    (coerce
                     (min
                      (if f12-1 (multiple-value-prog1
                                 b (return-from %f12 0))
                        0))
                     'integer)))
         (funcall #'%f12 0))))
   -33)
  0)

(deftest misc.275a
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (speed 2) (space 0) (safety 2)
                          (debug 1) (compilation-speed 1)))
       (block b4
         (coerce
          (logcount
           (if t 0 (multiple-value-prog1 (identity 0) (return-from b4 0))))
          'integer)))))
  0)

;;; clisp (28 Jan 2004)
;;; Different return values

(deftest misc.276
  (funcall
   (compile
    nil
    `(lambda (b)
       (declare (optimize (speed 2) (space 0) (safety 0)
                          (debug 3) (compilation-speed 3)))
       (labels
           ((%f2 ()
                 (let ((v10 (progn (dotimes (iv2 0 0) iv2)
                                   b)))
                   (unwind-protect b (labels ((%f6 ())) (%f6))
                                   ))))
         (%f2))))
   :good)
  :good)

;;; Lispworks 4.3 linux (personal edition)

;;; Error: In - of (1 NIL) arguments should be of type NUMBER
(deftest misc.277
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (speed 3) (space 1) (safety 0)
                          (debug 3) (compilation-speed 0)))
       (labels ((%f15 (&optional (f15-3
                                  (tagbody (labels ((%f6 () (go tag1))) (%f6)) tag1)))
                      0))
         (%f15)))))
  0)

;;; incorrect return value
(deftest misc.278
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (speed 1) (space 0) (safety 2) (debug 3)
                          (compilation-speed 0)))
       (catch 'ct5 (flet ((%f2 (&optional (f2-4 (throw 'ct5 0))) 1))
                     (%f2 (%f2 0)))))))
  1)

;;; incorrect return value
(deftest misc.279
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (speed 1) (space 1) (safety 3) (debug 0) (compilation-speed 3)))
       (flet ((%f10 ()
                    (if (< 0 (dotimes (iv2 1 -501162)))
                        0 -14)))
         (%f10)))))
  -14)

;;; incorrect return value (may be same bug as misc.278)
(deftest misc.280
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (optimize (speed 1) (space 3) (safety 1) (debug 3) (compilation-speed 2)))
       (catch 'ct6
         (labels ((%f12 ()
                        (labels ((%f14 (&optional (f14-3 (return-from %f12 5))) 4))
                          (funcall (constantly 3)
                                   (let ((v2 (%f14))) 2)
                                   (throw 'ct6 1)
                                   ))))
           (%f12)
           a))))
   :good)
  :good)

;;; incorrect return value
(deftest misc.281
  (funcall
   (compile
    nil
    '(lambda (c)
       (declare (optimize (speed 3) (space 3) (safety 3) (debug 2) (compilation-speed 3)))
       (ldb (byte 24 0) c)))
   -227016367797)
  12919115)

;;; gcl: Error in COMPILER::CMP-ANON [or a callee]: The function COMPILER::LDB1 is undefined.
(deftest misc.282
  (funcall (compile nil '(lambda () (declare (optimize safety))
                           (ldb (byte 13 13) 43710))))
  5)

;;; gcl (2/28/2004)
;;; Error in COMPILER::CMP-ANON [or a callee]: T is not of type INTEGER.
(deftest misc.283
  (funcall
   (compile
    nil
    '(lambda (b d)
       (declare (optimize (speed 2) (space 2) (safety 1) (compilation-speed 3)))
       (expt (logxor (progn
                       (tagbody
                        (multiple-value-prog1 0 (go 7))
                        7)
                       0)
                     0 b
                     (rational d))
             0)))
   2 4)
  1)

;;; Error in COMPILER::CMP-ANON [or a callee]: 3 is not of type FUNCTION.
;;; (possibly the same bug as misc.283)
(deftest misc.284
  (funcall
   (compile
    nil
    '(lambda (c)
       (declare (optimize (speed 1) (space 1) (safety 2) (debug 3)
                          (compilation-speed 2)))
       (progn
         (tagbody
          (multiple-value-prog1 0 (go tag2))
          0
          tag2)
         (funcall (constantly 0)
                  (apply (constantly 0) (signum c) nil)))))
   3)
  0)

;;; ecl 29 Feb 2004
;;; Incorrect constant propagation
(deftest misc.285
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (optimize (speed 2) (space 0) (safety 0) (debug 2)
                          (compilation-speed 3)))
       (block b7 (let* ((v1 (* (return-from b7 0) a))) -4359852))))
   1)
  0)

(deftest misc.286
  (let ((v4 (dotimes (iv4 0 18494910) (progn 0)))) v4)
  18494910)

;;; gcl (found by Camm)
;;; Error in COMPILER::CMP-ANON [or a callee]: The function NIL is undefined.
(deftest misc.287
  (funcall
   (compile
    nil
    '(lambda (e)
       (declare (optimize (speed 1) (space 3) (safety 3) (debug 3)
                          (compilation-speed 1)))
       (flet ((%f11 (f11-2) 0))
         (%f11 (unwind-protect
                   e
                 (tagbody
                  (let* ((v4 (unwind-protect (go 0)))) 0)
                  0)
                 (logand (handler-bind () 0)))))))
   10)
  0)

#|
ecl (6 Mar 2004)
(LAMBDA (C::LOC1 C::LOC2) (IF (AND (CONSP C::LOC1)
                                   (EQ (CAR C::LOC1) 'FIXNUM)
                                   (CONSP (CADR C::LOC1))
                                   (EQ (CAADR C::LOC1) 'C::FIXNUM-VALUE)
                                   (EQ (CADR (CADR C::LOC1)) 2))
                              (PROGN (C::WT1 "(1<<(")
                                     (C::WT1 C::LOC2) (C::WT1 "))"))
                              (PROGN (C::WT1 "fixnum_expt(")
                                     (C::WT1 C::LOC1)
                                     (C::WT1 #\,)
                                     (C::WT1 C::LOC2)
                                     (C::WT1 #\)))))
is not of type STRING.
Broken at C::WT-C-INLINE-LOC.
|#

(deftest misc.288
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (speed 1) (space 2) (safety 3) (debug 3)
                          (compilation-speed 2)))
       (let ((v2 (integer-length (expt 0 0))))
         (dotimes (iv4 0 0) (logand v2))))))
  0)

;;; cmucl
;;; wrong return value

(deftest misc.289
  (funcall
   (compile
    nil
    '(lambda (b)
       (declare (optimize (speed 3) (space 1) (safety 1) (debug 2)
                          (compilation-speed 2)))
       (multiple-value-prog1 (apply (constantly 0) b 0 0 nil)
                             (catch 'ct8 (throw 'ct8 -2)))))
   1)
  0)

;;; sbcl (0.8.8.23.stack.1)
;;; failed AVER: "(TAILP BLOCK2-STACK BLOCK1-STACK)"

(deftest misc.290
  (funcall
   (compile
    nil
    '(lambda ()
       (declare
        (optimize (speed 3) (space 3) (safety 1)
                  (debug 2) (compilation-speed 0)))
       (apply (constantly 0) (catch 'ct2 0) 0 (catch 'ct2 0) nil))))
  0)

(deftest misc.290a
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (speed 2) (space 1) (safety 0)
                          (debug 0) (compilation-speed 0)))
       (boole boole-nor
              (expt
               (let ((v2 (expt (catch 'ct7 0) 0)))
                 0)
               0)
              (expt (apply (constantly 0) 0 0 (catch 'ct6 0) nil) 0)))))
  -2)

;; Allegro CL 6.2 (14 Mar 2004) interpreter bug
;; Error: Cannot go to TAG, its body has been exited.

(deftest misc.291
  (funcall 
   #'(lambda (a)
       (declare (notinline numerator))
       (declare (optimize (speed 2) (space 3) (safety 2) (debug 0)
                          (compilation-speed 2)))
       (tagbody (tagbody (progn a) tag)
                (go tag)
                tag))
   17)
  nil)

;;; sbcl 0.8.8.23.stack.2
;;; The value -1 is not of type (MOD 536870911).

(deftest misc.292
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare
        (optimize (speed 3) (space 2) (safety 3) (debug 0)
                  (compilation-speed 1)))
       (flet ((%f15 (f15-1 f15-2 f15-3)
                    (apply (constantly 0)
                           0
                           0
                           (ignore-errors
                             (let ((v10 (apply (constantly 0)
                                               b a (max 0 c) nil)))
                               0))
                           nil)))
         (flet ((%f14 (f14-1 &optional (f14-2 b) (f14-3 0) (f14-4 0))
                      (%f15 0 0 b)))
           (%f14 0 c)))))
   1 2 3)
  0)

(deftest misc.292a
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (optimize (speed 2) (space 0) (safety 3) (debug 1)
                          (compilation-speed 2)))
       (apply (constantly 0)
              a
              0
              (catch 'ct6
                (apply (constantly 0)
                       0
                       0
                       (let* ((v1
                               (let ((*s7* 0))
                                 b)))
                         0)
                       0
                       nil))
              0
              nil)))
   1 2)
  0)

;;; failed AVER: "(NOT (MEMQ PUSH END-STACK))"

(deftest misc.293
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (optimize (speed 2) (space 1) (safety 3) (debug 3)
                          (compilation-speed 3)))
       (let ((v6
              (labels ((%f9 (f9-1)
                            (multiple-value-prog1 0 (return-from %f9 0) a)))
                (let ((*s4* (%f9 0)))
                  0))))
         0)))
   1)
  0)

(deftest misc.293a
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (optimize (speed 2) (space 3) (safety 1)
                          (debug 2) (compilation-speed 2)))
       (block b6
         (multiple-value-prog1
          0 b 0
          (catch 'ct7
            (return-from b6
              (catch 'ct2
                (complex (cl::handler-bind nil -254932942) 0))))))))
   1 2 3)
  -254932942)

(deftest misc.293b
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (notinline complex))
       (declare (optimize (speed 1) (space 0) (safety 1)
                          (debug 3) (compilation-speed 3)))
       (flet ((%f () (multiple-value-prog1 0 (return-from %f 0))))
         (complex (%f) 0)))))
  0)

(deftest misc.293c
  (funcall
   (compile
    nil
    '(lambda (a b)
           (declare (type (integer -6556 -33) a))
           (declare (type (integer -1973908574551 1125) b))
           (declare (ignorable a b))
           (declare
            (optimize (compilation-speed 0)
                      (space 2)
                      (safety 0)
                      (debug 2)
                      (speed 0)
                      #+sbcl (sb-c:insert-step-conditions 0)
                      ))
           (block b4
             (multiple-value-prog1 0
               (catch 'ct7 (return-from b4 (catch 'ct6 (if a 0 b))))
               0
               0))))
   -237 -1365751422718)
  0)

(deftest misc.293d
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (debug 3) (safety 0) (space 2)
			  (compilation-speed 2) (speed 2)))
       (block b4
	      (multiple-value-prog1
	       0
	       (catch 'ct8
		 (return-from b4 (catch 'ct2 (progn (tagbody) 0)))))))))
  0)
   

;;; failed AVER: "(SUBSETP START START-STACK)"

(deftest misc.294
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (notinline /=))
       (declare (optimize (speed 2) (space 0) (safety 1)
                          (debug 0)(compilation-speed 1)))
         (catch 'ct1
           (flet ((%f1 (f1-1 f1-2 f1-3)
                    (throw 'ct1
                      (if (/= 0) 0 (multiple-value-prog1
                                    0 (throw 'ct1 a) c)))))
             (let ((*s3* (%f1 a a 0)))
               0)))))
   1 2 3)
  0)

(deftest misc.294a
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (notinline expt))
       (declare (optimize (speed 1) (space 2) (safety 3)
                          (debug 0) (compilation-speed 0)))
       (catch 'ct2
         (expt
          (catch 'ct2
            (throw 'ct2 (if a 0 (multiple-value-prog1 0 (throw 'ct2 c) 0))))
          0))))
   1 2 3)
  1)

;;;  The value NIL is not of type SB-C::IR2-BLOCK.

(deftest misc.295
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (type (integer -2858 1050811) a))
       (declare (type (integer -419372 1395833) b))
       (declare (type (integer -4717708 795706) c))
       (declare (ignorable a b c))
       (declare
        (optimize (speed 1)
                  (space 0)
                  (safety 2)
                  (debug 1)
                  (compilation-speed 2)))
       (multiple-value-prog1
        (the integer
          (catch 'ct8
            (catch 'ct5
              (catch 'ct7
                (flet ((%f3 (f3-1 f3-2 &optional (f3-3 a) (f3-4 c)) b))
                  (labels ((%f13 (f13-1 f13-2 f13-3)
                                 (let* ((*s4*
                                         (return-from %f13
                                           (flet ((%f18 (f18-1 f18-2)
                                                        (apply #'%f3
                                                               (progv nil nil f13-2)
                                                               (list (%f3 -460 f18-1 10095 352819651)))))
                                             (flet ((%f5 ()
                                                         (funcall #'%f3
                                                                  f13-2
                                                                  (flet ((%f14 (f14-1 f14-2 &optional (f14-3 f13-2) (f14-4 -15))
                                                                               160080387))
                                                                    -196377)
                                                                  (isqrt
                                                                   (abs
                                                                    (if        (/= 117 (%f18 -14 -46574))
                                                                        (return-from
                                                                            %f13
                                                                          (ignore-errors
                                                                            (flet ((%f12 (f12-1 f12-2 &optional (f12-3 740148786)
                                                                                                (f12-4 -20)
                                                                                                (f12-5 -35261))
                                                                                         f12-3))
                                                                              (%f3
                                                                               (%f3 b (%f12 c b f13-3 f13-1 -1124))
                                                                               0
                                                                               -1003264058
                                                                               f13-1))))
                                                                      (block b3 (labels ((%f15 () f13-2))  -4858377)))))
                                                                  (%f3 793 f13-2 f13-3 a))))
                                               f13-3)))))
                                   (* -420793
                                      (%f3 (%f3 f13-1 f13-3 f13-3 f13-2) 0 8604 f13-1)))))
                    (lognor
                     (progv nil nil
                       (if (< -16 c)
                           15867134
                         (-
                          (throw 'ct5
                                 (prog1 7
                                   (floor
                                    (max (%f13 -4862 -888 -53824112) a -17974 1540006)
                                    (min -74 -473379)))))))
                     (progv nil nil (prog1 b 22 c a)))))))))
        (catch 'ct1 (throw 'ct1 0))
        0)))
   794801 211700 -1246335)
  7)

;;; Tests added by Camm for gcl

(deftest misc.296
  (funcall
   (compile
    nil
    '(lambda (a b c d e f g h)
          (declare (type (integer -2016726144 234357120) a))
          (declare (type (integer -10569521299456 -1307998945280) b))
          (declare (type (integer -45429002240 -17228484608) c))
          (declare (type (integer 228451840 1454976512) d))
          (declare (type (integer -4797 -2609) e))
          (declare (type (integer -21 36300536) f))
          (declare (type (integer -15983530 31646604) g))
          (declare (type (integer -208720272 -357) h))
          (declare (ignorable a b c d e f g h))
          (declare (optimize (speed 1) (space 3) (safety 3) (debug 0)
                       (compilation-speed 3)))
          (expt (labels ((%f14 (f14-1 f14-2)
                               (progn
                                 (tagbody
                                   (+
                                    (unwind-protect
                                      (labels ((%f1 (f1-1) (go tag1)))
                                        (let ((*s6* (%f1 d))) 0))))
                                   tag1
                                   (+
                                    (cl::handler-bind ()
                                      (if (<= -11215713 -819)
                                       (integer-length
                                        (floor (conjugate f14-1)
                                         (max 12
                                          (ceiling
                                           (block b2
                                             (catch 'ct2
                                               (ignore-errors
                                                (flet
                                                 ((%f13 (f13-1)
                                                   (logior 87 f14-2)))
                                                  f14-1))))))))
                                       (progv '(*s8*) (list 472865632)
                                         *s8*)))))
                                 0)))
                  (%f14 0 0))
                0)))
   -28594854 -3859203606860 -40757449218 894599577 -4163 11621230
   29558853 -92216802)
  1)


(deftest misc.297
  (funcall
   (compile
    nil
    '(lambda (a b c d e f g h)
          (declare (type (integer -4354712743936 666241234) a))
          (declare (type (integer -23496787232 13342697120) b))
          (declare (type (integer -6834570 6274788) c))
          (declare (type (integer -1988742 -250650) d))
          (declare (type (integer 10523345 10868247) e))
          (declare (type (integer -489185 -46267) f))
          (declare (type (integer -627627253760 226529) g))
          (declare (type (integer -1039260485 -22498) h))
          (declare (ignorable a b c d e f g h))
          (declare (optimize (speed 1) (space 3) (safety 2) (debug 2)
                       (compilation-speed 0)))
          (labels ((%f7 (f7-1 f7-2 f7-3 &optional (f7-4 0) (f7-5 0)
                              (f7-6 (labels
                                     ((%f6 (f6-1)
                                       (labels ((%f9 (f9-1) 0))
                                         (progn
                                           (tagbody
                                             (unwind-protect
                                               (if (%f9 (go tag4)) 0 0))
                                             tag4
                                             (cl::handler-case
                                              0))
                                           h))))
                                      (apply #'%f6 0 nil))))
                        0))
            (%f7 0 d 0 f d))))
    -4319330882538 -3195059121 -2799927 -1466395 10630639 -224479
    -502579707077 -985908422)
   0)

(deftest misc.298
  (funcall
   (compile
    nil
    '(lambda (a b c d e f g h)
          (declare (type (integer 1296736620544 1680954654720) a))
          (declare (type (integer -2 -2) b))
          (declare (type (integer 1 42303) c))
          (declare (type (integer -38881008000 1333202563072) d))
          (declare (type (integer -435684 1289298) e))
          (declare (type (integer -164302654464 -10150328832) f))
          (declare (type (integer 30759259904 38429537792) g))
          (declare (type (integer -1628949299200 -47724342) h))
          (declare (ignorable a b c d e f g h))
          (declare (optimize (speed 3) (space 1) (safety 0) (debug 0)
                       (compilation-speed 1)))
          (progn
            (tagbody
              (let ((v9 (unwind-protect (go 0)))) 0)
              0
              (numerator (funcall (constantly 0) (logorc2 0 0) 0)))
            0)))
   1451922002679 -2 285 1067997670626 1115209 -37445463114
         36530345360 -80501559891)
   0)

(deftest misc.299
  (funcall
   (compile
    nil
    '(lambda (a b c d e f g h)
          (declare (type (integer -1814 3348) a))
          (declare (type (integer -32239015 12) b))
          (declare (type (integer 128412 101411593) c))
          (declare (type (integer -329076792320 -22) d))
          (declare (type (integer 77651198 86069496) e))
          (declare (type (integer -4616 3453771) f))
          (declare (type (integer -14889981824 53610580608) g))
          (declare (type (integer -1049733788 46605484288) h))
          (declare (ignorable a b c d e f g h))
          (declare (optimize (speed 2) (space 1) (safety 3) (debug 2)
                       (compilation-speed 2)))
          (conjugate
              (progn
                (tagbody
                  (flet ((%f3 nil 0))
                    (unwind-protect
                      (flet ((%f10 (f10-1) (let ((*s6* (%f3))) (go 6))))
                        (funcall #'%f10 f))))
                  6
                  (let ((*s1* (restart-bind ()
                                (labels ((%f1 (f1-1) 3136))
                                  (let ()
                                    (progv '(*s5* *s1*)
                                     (list
                                      (labels
                                       ((%f2 nil (catch 'ct8 -11)))
                                        -70941710)
                                      (if nil (%f1 -1) 87))
                                      (progn
                                        (tagbody
                                          (%f1 *s1*)
                                          3
                                          (block b2 (progn a))
                                          tag3)
                                        h)))))))
                    0))
                0))))
   1555 -22062210 85224215 -161218251003 78463284 730073
   33930166854 37839245921)
   0)

(deftest misc.300
  (funcall
   (compile
    nil
    '(lambda (a b c d e f g h)
          (declare (type (integer -29429 -3320) a))
          (declare (type (integer -407874593 279639852) b))
          (declare (type (integer -542849760256 3344389718016) c))
          (declare (type (integer -2 12012755) d))
          (declare (type (integer -248 -228) e))
          (declare (type (integer 5 15636824592) f))
          (declare (type (integer 21039 21595) g))
          (declare (type (integer -1867743555584 -1621183025152) h))
          (declare (ignorable a b c d e f g h))
          (declare (optimize (speed 1) (space 3) (safety 1) (debug 2)
                       (compilation-speed 3)))
          (labels ((%f12 (f12-1 f12-2 f12-3) 0))
            (labels ((%f17 (f17-1)
                           (progn
                             (tagbody
                               (max (apply (constantly 0)
                                     (list
                                      (%f12 (unwind-protect (go tag1))
                                       0 d)
                                      0 f)))
                               tag1
                               (dpb (realpart
                                     (expt
                                      (round
                                       (return-from %f17
                                         (restart-bind ()
                                           (complex e 0)))
                                       (max 40 0))
                                      0))
                                    (byte 0 0) 0))
                             0)))
              (%f12 0 (%f17 0) 0)))))
   -6416 -274982013 2946309248013 1724720 -228 5782683458 21484
   -1681168611256)
  0)


(deftest misc.301
  (funcall
   (compile
    nil
    '(lambda (a b c d e f g h)
          (declare (type (integer -1814 3348) a))
          (declare (type (integer -32239015 12) b))
          (declare (type (integer 128412 101411593) c))
          (declare (type (integer -329076792320 -22) d))
          (declare (type (integer 77651198 86069496) e))
          (declare (type (integer -4616 3453771) f))
          (declare (type (integer -14889981824 53610580608) g))
          (declare (type (integer -1049733788 46605484288) h))
          (declare (ignorable a b c d e f g h))
          (declare (optimize (speed 2) (space 1) (safety 3) (debug 2)
                       (compilation-speed 2)))
          (conjugate
              (progn
                (tagbody
                  (flet ((%f3 nil 0))
                    (unwind-protect
                      (flet ((%f10 (f10-1) (let ((*s6* (%f3))) (go 6))))
                        (funcall #'%f10 f))))
                  6
                  (let ((*s1* (restart-bind ()
                                (labels ((%f1 (f1-1) 3136))
                                  (let ()
                                    (progv '(*s5* *s1*)
                                     (list
                                      (labels
                                       ((%f2 nil (catch 'ct8 -11)))
                                        -70941710)
                                      (if nil (%f1 -1) 87))
                                      (progn
                                        (tagbody
                                          (%f1 *s1*)
                                          3
                                          (block b2 (progn a))
                                          tag3)
                                        h)))))))
                    0))
                0))))
   1555 -22062210 85224215 -161218251003 78463284 730073
   33930166854 37839245921)
  0)

(deftest misc.302
  (funcall
   (compile
    nil
    '(lambda (a b c d e f g h)
          (declare (type (integer -206837809920 -126404559104) a))
          (declare (type (integer -277874608640 -63724432) b))
          (declare (type (integer -2 0) c))
          (declare (type (integer -5992710 9946878) d))
          (declare (type (integer -4345390743552 -76504514048) e))
          (declare (type (integer -330 3826137) f))
          (declare (type (integer -517792898560 -1193868) g))
          (declare (type (integer 2018 98092396) h))
          (declare (ignorable a b c d e f g h))
          (declare (optimize (speed 2) (space 2) (safety 2) (debug 1)
                       (compilation-speed 1)))
          (flet ((%f12 (f12-1 f12-2 &optional (f12-3 0)
                              (f12-4 (progn
                                       (tagbody
                                         (unwind-protect (go tag6))
                                         tag6)
                                       (flet ((%f1 (f1-1 f1-2) 0))
                                         (apply #'%f1 0 0 (list))))))
                       0))
            (%f12 0 e))))
   -195379170409 -30212852077 -1 -2082141 -1686846623470 360505
   -324299330279 37218064)
  0)

(deftest misc.303
  (funcall
   (compile
    nil
    '(lambda (a b c d e f g h)
          (declare (type (integer -55724018 0) a))
          (declare (type (integer -4929718 2777256) b))
          (declare (type (integer 18939493568 24064422528) c))
          (declare (type (integer -13157393 112210531) d))
          (declare (type (integer -75775 -4883) e))
          (declare (type (integer 5071 1584913674240) f))
          (declare (type (integer -1 -1) g))
          (declare (type (integer -100 7017454141440) h))
          (declare (ignorable a b c d e f g h))
          (declare (optimize (speed 1) (space 3) (safety 3) (debug 1)
                       (compilation-speed 1)))
          (labels ((%f7 (f7-1 &optional (f7-2 0) (f7-3 0) (f7-4 0)) 0))
            (progn
              (denominator
                  (progn
                    (let ((*s6* (progn
                                  (tagbody
                                    (unwind-protect
                                      (%f7 0 0 (go tag6) d))
                                    tag6
                                    (restart-case 0))
                                  0)))
                      0)
                    0))
              0))))
   -23410726 -4342503 20297113275 80145634 -17664 937086103773 -1
   2923877584757)
  0)

(deftest misc.304
  (funcall
   (compile
    nil
    '(lambda (a b c d e f g h)
          (declare (type (integer -11679 1672) a))
          (declare (type (integer -359757 -216048) b))
          (declare (type (integer -46345706880 -1824) c))
          (declare (type (integer -18 18) d))
          (declare (type (integer -70852138 427028370944) e))
          (declare (type (integer -428904547840 535369082368) f))
          (declare (type (integer -4372225 83) g))
          (declare (type (integer -2 0) h))
          (declare (ignorable a b c d e f g h))
          (declare (optimize (speed 2) (space 1) (safety 3) (debug 0)
                       (compilation-speed 1)))
          (labels ((%f1 (f1-1 f1-2 f1-3) 0))
            (rationalize
                (%f1 (progn
                       (tagbody
                         (let ((v3 (%f1 (unwind-protect (go tag2)) b 0)))
                           0)
                         tag2)
                       0)
                     h (cl::handler-case 0))))))
   -7209 -223767 -42093806027 -9 132172281069 138363461574
   -3751010 0)
  0)

(deftest misc.305
  (funcall
   (compile
    nil
    '(lambda (a b c d e f g h)
          (declare (type (integer -438 247) a))
          (declare (type (integer -93662232 112841) b))
          (declare (type (integer 8769 2766606) c))
          (declare (type (integer -33007133760 32531429568) d))
          (declare (type (integer 419 3712) e))
          (declare (type (integer 1628 20513914304) f))
          (declare (type (integer -1347290 47) g))
          (declare (type (integer -12 3030073088) h))
          (declare (ignorable a b c d e f g h))
          (declare (optimize (speed 3) (space 3) (safety 0) (debug 3)
                       (compilation-speed 0)))
          (flet ((%f5 (f5-1 f5-2 &optional (f5-3 0) (f5-4 0) (f5-5 0))
                      (progn
                        (tagbody (unwind-protect (go tag1)) tag1)
                        (coerce (let* ((*s4*
                                        (flet
                                         ((%f1 nil
                                           (let*
                                            ((v7 (dpb 0 (byte 0 0) c)))
                                             a)))
                                          (progv '(*s6* *s7*)
                                           (list (%f1) 0)
                                            g))))
                                  c)
                                'integer))))
            (if (%f5 d 0 e 0 0) h 0))))
   -58 -22237190 2055343 -8144832891 1213 19038103159 -1009345
   929619162)
  929619162)

(deftest misc.306
  (funcall
   (compile
    nil
    '(lambda (a b c d e f g h)
          (declare (type (integer 261 234565) a))
          (declare (type (integer -1454263719936 -3279802168) b))
          (declare (type (integer -1251120498 -49518770) c))
          (declare (type (integer 0 369) d))
          (declare (type (integer -12465203856 -45) e))
          (declare (type (integer -94321486 -91941853) f))
          (declare (type (integer -16528338864 11322249648) g))
          (declare (type (integer -1230549 -1143976) h))
          (declare (ignorable a b c d e f g h))
          (declare (optimize (speed 2) (space 1) (safety 0) (debug 0)
                       (compilation-speed 0)))
          (denominator
              (progn
                (tagbody (unwind-protect (go tag7)) tag7)
                (logxor f
                        (multiple-value-bind (*s4*)
                            (logxor 0 (expt -2 1))
                          (truncate 0)))))))
   130724 -736795298357 -1221747467 326 -9775240900 -94105708
   -2273680158 -1156846)
  1)

(deftest misc.307
  (funcall
   (compile
    nil
    '(lambda (a b c d e f g h)
          (declare (type (integer -2903632 1282236) a))
          (declare (type (integer 7 10741) b))
          (declare (type (integer -249635 214804) c))
          (declare (type (integer -50422 10469) d))
          (declare (type (integer -52337314 10771161) e))
          (declare (type (integer 0 5333060) f))
          (declare (type (integer -1 0) g))
          (declare (type (integer 1595835 4577573) h))
          (declare (ignorable a b c d e f g h))
          (declare (optimize (speed 1) (space 3) (safety 3) (debug 3)
                       (compilation-speed 1)))
          (flet ((%f11 (f11-1 f11-2) 0))
            (%f11 0
                  (unwind-protect
                    e
                    (progn
                      (tagbody
                        (let* ((v4 (progn (unwind-protect (go 0)) 0)))
                          0)
                        0)
                      (logand (cl::handler-bind ()
                                (logand -15 -2 32578787 10349 e
                                        -24781944 -8)))))))))
   60336 1625 124302 -33193 -8095855 4995857 0 4572381)
  0)

(deftest misc.308
  (funcall
   (compile
    nil
    '(lambda (a b c d e f g h)
          (declare (type (integer -2806612475904 8750665416704) a))
          (declare (type (integer -3 10) b))
          (declare (type (integer -94336824 116591592) c))
          (declare (type (integer 456813135872 903636350976) d))
          (declare (type (integer -2364199833600 -172353318912) e))
          (declare (type (integer 717 1760915) f))
          (declare (type (integer -21 105) g))
          (declare (type (integer -3579048169472 -346272903168) h))
          (declare (ignorable a b c d e f g h))
          (declare (optimize (speed 2) (space 1) (safety 0) (debug 0)
                       (compilation-speed 0)))
          (labels ((%f7 (f7-1)
                        (multiple-value-prog1 0
                          0
                          (return-from %f7 (mask-field (byte 0 0) 0)))))
            (unwind-protect (%f7 0)))))
   1951007924893 10 -49879990 614214833752 -1808568999586 1282634
   99 -2783010573143)
  0)

(deftest misc.309
  (funcall
   (compile
    nil
    '(lambda (a b c d e f g h)
          (declare (type (integer -521338 12) a))
          (declare (type (integer -1787856009216 1182078822400) b))
          (declare (type (integer -3313 28535137344) c))
          (declare (type (integer -38914612 -25121536) d))
          (declare (type (integer 403073126400 2632230309888) e))
          (declare (type (integer -39663606528 -1238304) f))
          (declare (type (integer -103560 -70383) g))
          (declare (type (integer -894 -227) h))
          (declare (ignorable a b c d e f g h))
          (declare (optimize (speed 3) (space 1) (safety 1) (debug 3)
                       (compilation-speed 2)))
          (block b8
            (multiple-value-prog1
                (logand (logior 0
                                (if (logbitp 0 0) 0
                                    (multiple-value-bind (v2) 0 0))))
              (gcd (let* ((*s4* 0))
                     (logior 0
                             (return-from b8
                               (let ((*s8* 0)) (round 0))))))
              0
              0))))
   -275760 -565946697213 9650536069 -37585973 1536165173011
   -12895970021 -102192 -534)
  0 0)


(deftest misc.310
  (funcall
   (compile
    nil
    '(lambda (a b c d e f g h)
          (declare (type (integer -2016726144 234357120) a))
          (declare (type (integer -10569521299456 -1307998945280) b))
          (declare (type (integer -45429002240 -17228484608) c))
          (declare (type (integer 228451840 1454976512) d))
          (declare (type (integer -4797 -2609) e))
          (declare (type (integer -21 36300536) f))
          (declare (type (integer -15983530 31646604) g))
          (declare (type (integer -208720272 -357) h))
          (declare (ignorable a b c d e f g h))
          (declare (optimize (speed 1) (space 3) (safety 3) (debug 0)
                       (compilation-speed 3)))
          (expt (labels ((%f14 (f14-1 f14-2)
                               (progn
                                 (tagbody
                                   (+
                                    (unwind-protect
                                      (labels ((%f1 (f1-1) (go tag1)))
                                        (let ((*s6* (%f1 d))) 0))))
                                   tag1
                                   (+
                                    (cl::handler-bind ()
                                      (if (<= -11215713 -819)
                                       (integer-length
                                        (floor (conjugate f14-1)
                                         (max 12
                                          (ceiling
                                           (block b2
                                             (catch 'ct2
                                               (ignore-errors
                                                (flet
                                                 ((%f13 (f13-1)
                                                   (logior 87 f14-2)))
                                                  f14-1))))))))
                                       (progv '(*s8*) (list 472865632)
                                         *s8*)))))
                                 0)))
                  (%f14 0 0))
                0)))
   -28594854 -3859203606860 -40757449218 894599577 -4163 11621230
   29558853 -92216802)
  1)

(deftest misc.311
  (funcall
   (compile
    nil
    '(lambda (a b c d e f g h)
          (declare (type (integer -1203392327680 -3017953) a))
          (declare (type (integer -34222 -1) b))
          (declare (type (integer -871294987 19) c))
          (declare (type (integer 717979131904 3341735845888) d))
          (declare (type (integer -7521858 3) e))
          (declare (type (integer -52 49) f))
          (declare (type (integer 18 43) g))
          (declare (type (integer -503567246 -46) h))
          (declare (ignorable a b c d e f g h))
          (declare (optimize (speed 1) (space 3) (safety 1) (debug 2)
                       (compilation-speed 2)))
          (labels ((%f2 (f2-1 f2-2 f2-3 &optional
                              (f2-4 (let
                                     ((*s6*
                                       (progn
                                         (tagbody
                                           (flet
                                            ((%f17 (f17-1 f17-2 f17-3)
                                              (go 6)))
                                             (%f17 0 b 0))
                                           6)
                                         0)))
                                      (complex
                                       (progn
                                         (tagbody
                                           (labels
                                            ((%f18
                                              (f18-1 f18-2 &optional
                                               (f18-3 0) (f18-4 f))
                                              0))
                                             (apply #'%f18 g 0 0
                                              (list)))
                                           0)
                                         0)
                                       0)))
                              (f2-5 0) (f2-6 0))
                        0))
            (%f2 0 0 f))))
   -738307241633 -25016 -846570136 2181696281793 -983259 24 36
   -185316211)
  0)


(deftest misc.312
  (funcall
   (compile
    nil
    '(lambda (a b c d e f g h)
          (declare (type (integer -18334222 14354736) a))
          (declare (type (integer 11163582 6421184978944) b))
          (declare (type (integer -13690431913984 -64765792960) c))
          (declare (type (integer -12750925 31112834) d))
          (declare (type (integer -5188669232 2246825616) e))
          (declare (type (integer -31235593088 -134) f))
          (declare (type (integer -1 -1) g))
          (declare (type (integer -647589424 12392126736) h))
          (declare (ignorable a b c d e f g h))
          (declare (optimize (speed 3) (space 2) (safety 1) (debug 1)
                       (compilation-speed 1)))
          (let ((*s4* (if (progn
                            (tagbody (unwind-protect (go 2)) 2)
                            0)
                          (numerator
                              (let* ((v1
                                      (let ((*s6* 0))
                                        (logand b
                                         (rationalize
                                          (coerce 0 'integer))))))
                                0))
                          0)))
            0)))
   7112398 3547401482305 -12827294644277 23312291 -444957551
   -5443955020 -1 4998457143)
  0)

(deftest misc.313
  (funcall
   (compile
    nil
    '(lambda (a b c d e f g h)
          (declare (type (integer 55474 401001) a))
          (declare (type (integer -8359558987776 8684176949248) b))
          (declare (type (integer -54332 116292) c))
          (declare (type (integer 0 0) d))
          (declare (type (integer -609311104000 959776553984) e))
          (declare (type (integer -2031580 3834807) f))
          (declare (type (integer -10955 2549) g))
          (declare (type (integer -8362590032 -210369) h))
          (declare (ignorable a b c d e f g h))
          (declare (optimize (speed 1) (space 3) (safety 1) (debug 0)
                       (compilation-speed 1)))
          (dotimes (iv1 3 0)
            (labels ((%f6 (f6-1 f6-2 f6-3 &optional
                                (f6-4 (flet
                                       ((%f3
                                         (f3-1 f3-2 f3-3 &optional
                                          (f3-4 0))
                                         (flet ((%f11 nil 0))
                                           (ash
                                            (progn
                                              (tagbody
                                                (labels
                                                 ((%f3
                                                   (f3-1 &optional
                                                    (f3-2 (go tag4)))
                                                   0))
                                                  (%f3 0))
                                                tag4)
                                              0)
                                            (min 42
                                             (conjugate
                                              (coerce
                                               (conjugate
                                                (let ((v9 (%f11)))
                                                  f3-1))
                                               'integer)))))))
                                        (%f3 c 0 a)))
                                (f6-5 0))
                          0))
              (apply #'%f6 0 0 h nil)))))
   93287 3146418586486 -51786 0 -63479145888 1935918 -10058 -2033798238)
  0)

(deftest misc.314
  (funcall
   (compile
    nil
    '(lambda (a b c d e f g h)
          (declare (type (integer -176150296 698) a))
          (declare (type (integer -62799871488 -56234210816) b))
          (declare (type (integer -1 1) c))
          (declare (type (integer 31 215808) d))
          (declare (type (integer -3 -1) e))
          (declare (type (integer -3 3387651) f))
          (declare (type (integer -14370846720 -56648624) g))
          (declare (type (integer -8316238784 -6221617344) h))
          (declare (ignorable a b c d e f g h))
          (declare (optimize (speed 1) (space 1) (safety 1) (debug 2)
                       (compilation-speed 2)))
          (progn
            (tagbody
              (unwind-protect
                (let ((v10 (let* ((v7 (if (go tag6) 0 0))) 0))) 0))
              tag6
              (let ((v7 (flet ((%f11 nil 0))
                          (flet ((%f13 (f13-1 f13-2 f13-3) f13-2))
                            (funcall #'%f13 0 a (%f11))))))
                0))
            0)))
   -90583503 -61289371485 -1 175888 -3 3257970 -3264725617  -6816839328)
  0)

;;; (misc.315 deleted)

;;; ACL 6.2 interpreter bugs
;;; Error: `NIL' is not of the expected type `NUMBER'
;;; (in COMP::IA-RESOLVE-REFS)

(deftest misc.316
  (funcall
   (compile
    nil
    '(lambda (a c)
       (declare (optimize (speed 2) (space 3) (safety 2) (debug 2)
                          (compilation-speed 0)))
       (unwind-protect
           0
         (progn (tagbody (bit #*000000111 (min 8 (max 0 a)))
                         tag5    (flet ((%f17 (f17-1 f17-2 f17-3)
                                              (complex (numerator (go tag4)) 0)))
                                   c)
                         tag4)
                c))))
   1 2)
  0)

;;; ecl failures (12 April 2004)

;;; wrong value returned
(deftest misc.317
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (speed 1) (space 1) (safety 3) (debug 0)
                          (compilation-speed 3)))
       (catch 'ct4
         (elt '(40760)
              (min 0 (max 0 (let* ((v3 (* (throw 'ct4 0) 0))) 0))))))))
  0)

;;; seg fault
(deftest misc.318
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (type (integer -2050548150 4917) a))
       (declare (type (integer -4 1) b))
       (declare (type (integer 99335934976 442465125376) c))
       (declare (ignorable a b c))
       (declare (optimize (speed 1) (space 1) (safety 1) (debug 0)
                          (compilation-speed 0)))
       (if (rationalize
            (labels ((%f12 (f12-1)
                           (if c 0
                             (bit #*101010011000011
                                  (min 14 (max 0 0))))))
              (if (> 0 c) 0 (%f12 0))))
           (progn
             (expt (flet ((%f18 (f18-1 f18-2 &optional (f18-3 0)
                                       (f18-4 c) (f18-5 b))
                                0))
                     (apply #'%f18 b b 0 0 nil))
                   0)
             a)
         0)))
   10 1 99335934976)
  10)

;;; seg fault
(deftest misc.319
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (type (integer -626615938 3649977016320) a))
       (declare (type (integer -3615553 6013683) b))
       (declare (type (integer -746719 1431737508) c))
       (declare (ignorable a b c))
       (declare (optimize (speed 3) (space 1) (safety 2) (debug 3)
                          (compilation-speed 3)))
       (if (logbitp 0
                    (flet ((%f10 (f10-1 f10-2 f10-3) b))
                      (flet ((%f4 (f4-1 f4-2)
                                  (apply #'%f10 (%f10 0 a 0) 0 c nil)))
                        (complex (%f4 0 0) 0))))
           0 0)))
   2378435476701 1646880 246794654)
  0)

;;; sbcl 0.8.9.35
;;; failed AVER: "(EQL (LAMBDA-COMPONENT FUNCTIONAL) *CURRENT-COMPONENT*)"

(deftest misc.320
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (speed 3) (space 0) (safety 2)
                          (debug 2) (compilation-speed 0)))
       (catch 'ct2
         (elt '(102)
              (flet ((%f12 () (rem 0 -43)))
                (multiple-value-call #'%f12 (values))))))))
  102)

(deftest misc.320a
  (funcall
   (compile
    nil
    '(lambda (b)
       (declare (optimize (speed 3) (space 0) (safety 2)
                            (debug 2) (compilation-speed 0)))
         (reduce '*
                 (list (elt '(10 20 30 40 50) b)
                       (expt (reduce #'(lambda (lmv1 lmv3) (mod lmv3 15))
                                     (vector 0 0))
                             0)
                       (rem 0 -71))
                 )))
   2)
  0)

(deftest misc.320b
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (type (integer -690191 -454473) a))
       (declare (type (integer -459197 -62) b))
       (declare (type (integer 445621505781 8489194559765) c))
       (declare (ignorable a b c))
       (declare
        (optimize (speed 1)
                  (space 0)
                  (safety 2)
                  (debug 3)
                  (compilation-speed 3)))
       (elt '(3327764 3386241)
            (min 1
                 (max 0
                      (reduce #'(lambda (lmv6 lmv5) (mod 0 (min -86 0)))
                              (list 0 0)))))))
   -512398 -156405 1140919327630)
  3327764)  

;;; ecl
;;; Wrong value

(deftest misc.321
  (funcall
   (compile
    nil
    '(lambda (p)
       (declare (optimize (speed 1) (space 3) (safety 2) (debug 1)
                          (compilation-speed 3)))
       (catch 'ct2
         (let* ((v3 (- (if p (throw 'ct2 :good) 0))))
           :bad))))
   t)
  :good)

;;; segfault
(deftest misc.322
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (optimize (speed 2) (space 2) (safety 0) (debug 3)
                          (compilation-speed 2)))
       (logorc2 (labels ((%f14 (f14-1) a)) (%f14 0))
                (reduce #'(lambda (lmv1 lmv2) a) (list 0 0)))))
   3151096069)
  -1)

;; #1# is undefined

(deftest misc.323
  (let* ((tail '(:from-end t))
         (form
          `(lambda ()
            (declare (optimize (speed 3) (space 1) (safety 2) (debug 2)
                               (compilation-speed 2)))
            (eval '(reduce #'logior
                           (vector (reduce #'logand (vector 0 0) . ,tail) 0)
                           . ,tail)))))
    (funcall (compile nil form)))
  0)

;;; Bad value

(deftest misc.324
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (optimize (speed 2) (space 2) (safety 3) (debug 2)
                          (compilation-speed 3)))
       (labels ((%f6 (f6-1) (multiple-value-setq (a) 0)))
         (reduce #'(lambda (lmv4 lmv3) a)
                 (list (%f6 0) 2)))))
   1)
  0)

;;; "A bug was found in the compiler.  Contact worm@arrakis.es."
;;; Broken at C::C2MULTIPLE-VALUE-SETQ.

(deftest misc.325
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -1659358 3099614928896) a))
       (declare (type (integer -492625 197903) b))
       (declare (ignorable a b))
       (declare (optimize (speed 3) (space 1) (safety 3) (debug 0)
                          (compilation-speed 1)))
       (reduce #'(lambda (lmv5 lmv6)
                   (multiple-value-setq (a) 2443855591508))
               (vector b a 0 0) :from-end t)))
   1 2)
  2443855591508)

;;; wrong value
(deftest misc.326
  (funcall
   (compile
    nil
    '(lambda (b)
       (declare (type (integer 155 7955) b))
       (declare (optimize (speed 3) (space 3) (safety 3) (debug 1)
                          (compilation-speed 0)))
       (flet ((%f13 (f13-1) (shiftf b 3019))) (+ b (%f13 0)))))
   200)
  400)

;;; acl 6.2 (x86 linux trial edition, patched, 4/15/04)
;;; Error: `NIL' is not of the expected type `REAL'

(deftest misc.327
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -67668056 -55) a))
       (declare (type (integer -586950907 -10945000) b))
       (declare (ignorable a b))
       (declare (optimize (speed 2) (space 0) (safety 2) (debug 2)
                          (compilation-speed 1)))
       (labels ((%f15 (f15-1)
                      (elt #(1073730663 1073689230 596123606 1073713997
                                        311527378 186184643 1073713230 1316881)
                           (min 7
                                (max 0
                                     (catch 'ct7
                                       (reduce
                                        #'min
                                        (list 0 b (catch 'ct7
                                                    (throw 'ct7 f15-1))
                                              0)
                                        :start 1
                                        :from-end t)))))))
     (%f15 0))))
   -38276611 -11001852)
  1073730663)

;;; wrong return value: T
(deftest misc.327a
  (funcall
   (compile
    nil
    '(lambda (a b c d e)
       (declare (notinline max vector reduce))
       (declare (optimize (speed 1) (space 2) (safety 1) (debug 1)
                          (compilation-speed 2)))
       (reduce #'(lambda (lmv6 lmv3) lmv3)
               (vector 0 (max 0) 0 0
                       (catch 'ct2 (catch 'ct2 (throw 'ct2 0))) 0 e 0)
               
               :end 2
               :from-end t)))
   68664683637 328245 881497115 -303855 311427)
  0)
    
;;; Bugs from abcl
;;; Debugger invoked on condition of type TYPE-ERROR:
;;;   The value org.armedbear.lisp.Symbol@54 is not of type integer.

(deftest misc.328
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -11368047588 14412128900) a))
       (declare (type (integer -10 0) b))
       (declare (ignorable a b))
       (declare (optimize (speed 3) (space 1) (safety 3) (debug 0)
                          (compilation-speed 0)))
       (if (logbitp 0 (if (or t nil) (setf a -2616861879) 0)) 0 0)))
   -4836700955 -1)
  0)

;;; Incorrect value
(deftest misc.329
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -725661427 405092) a))
       (declare (type (integer 84176291516 98216856233) b))
       (declare (ignorable a b))
       (declare (optimize (speed 3) (space 3) (safety 0) (debug 0)
                          (compilation-speed 0)))
       (let ((*s2* (case b ((53651 62711 29537 25305 62250) 0) (t 0))))
         (declare (special *s2*))
         (setq a -688292831))))
   -406606203 84436335326)
  -688292831)

(deftest misc.330
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -12816761394938 -8706928710678) a))
       (declare (type (integer -3683497948554 427) b))
       (declare (ignorable a b))
       (declare (optimize (speed 3) (space 0) (safety 0) (debug 3)
                          (compilation-speed 2)))
       (lcm (block b8 (signum (return-from b8 a))))))
   -12715609319989 -582329850697)
  12715609319989)

(deftest misc.331
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -777352478 239900) a))
       (declare (type (integer -63500163479 -8671) b))
       (declare (ignorable a b))
       (declare (optimize (speed 3) (space 0) (safety 0) (debug 2)
                          (compilation-speed 3)))
       (if (if (>= 0) t t) (setq b -25319949896) b)))
   0 -10000)
  -25319949896)

;;; Debugger invoked on condition of type TYPE-ERROR:
;;;   The value 0 is not of type org.armedbear.lisp.Symbol@80f563d8.
(deftest misc.332
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (notinline max logorc1 numerator rem))
       (declare (optimize (speed 3) (space 1) (safety 1) (debug 1)
                          (compilation-speed 2)))
       (rem (progn (tagbody (numerator (logorc1 0 (go tag5)))
                            tag5)
                   0)
            (max 93 0))))
   -801 17641908)
  0)

;;; Debugger invoked on condition of type TYPE-ERROR:
;;;   The value #<THREAD @ #x15c40eb> is not of type org.armedbear.lisp.Symbol@80f563d8.

(deftest misc.333
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (notinline logxor))
       (declare (optimize (speed 3) (space 0) (safety 0) (debug 3)
                          (compilation-speed 3)))
       (logxor (progn (tagbody (let* ((*s4* (progn (go 1) 0))) 0)
                               1)
                      0)))))
  0)

;;; Debugger invoked on condition of type PROGRAM-ERROR:
;;;  Wrong number of arguments for EXPT.
(deftest misc.334
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (type (integer 1892675246514 8763564964618) a))
       (declare (type (integer -1353 -456) b))
       (declare (type (integer 2010840649 2119165101) c))
       (declare (ignorable a b c))
       (declare (optimize (speed 3) (space 2) (safety 0) (debug 2)
                          (compilation-speed 1)))
       (+ (block b6 (expt (return-from b6 b) 0)))))
   3966745735633 -1123 2030094113)
  -1123)

;;; The value NIL is not of type number.
(deftest misc.335
  (let ((c 10))
    (denominator (progn (tagbody (realpart (loop for lv4 below 2 sum (go 0)))
                                 0)
                        c)))
  1)

(deftest misc.336
  (prog2 (progn (tagbody (- (common-lisp:handler-case (go tag2)))
                         tag2)
                0)
         0)
  0)

;;; Incorrect return value
(deftest misc.337
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (speed 2) (space 2) (safety 2) (debug 0)
                          (compilation-speed 0)))
       (imagpart (block b8
                   (logior (block b7 (return-from b8 225480400))))))))
  0)

;;; Inconsistent stack height 1 != 2
(deftest misc.338
  (let #+armedbear ((jvm::*catch-errors* nil))
       nil
       (funcall
        (compile
         nil
         '(lambda (c)
            (conjugate (block b8 (max (if c (return-from b8 0) 0))))))
        10))
  0)

;;; Inconsistent stack height 4 != 0
(deftest misc.339
  (let
   #+armedbear ((jvm::*catch-errors* nil))
   nil
   (funcall
    (compile
     nil
     '(lambda ()
        (declare (optimize (speed 1) (space 3) (safety 3) (debug 0)
                           (compilation-speed 0)))
        (block b1
          (reduce #'min
                  (list (return-from b1 0))
                  :end    1
                  :start  0
                  :from-end t
                  ))))))
  0)

;;;   The value INTEGER is not of type sequence.
(deftest misc.340
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (type (integer -4379340 -1962) a))
       (declare (type (integer 1304043 3225940) b))
       (declare (type (integer -3229571579853 -180689150012) c))
       (declare (ignorable a b c))
       (declare (optimize (speed 3) (space 1) (safety 0) (debug 2)
                          (compilation-speed 2)))
       (coerce (rationalize (progn (tagbody (reduce #'logand
                                                    (list b 0 (go tag3))
                                                    :from-end
                                                    t)
                                            tag3)
                                   0))
               'integer)))
   -1625211 3052955 -2091182035681)
  0)

;;; Inconsistent stack height 1 != 2
(deftest misc.341
  (let
   #+armedbear ((jvm::*catch-errors* nil))
   nil
   (funcall
    (compile
     nil
     '(lambda (c)
        (declare (optimize (speed 2) (space 1) (safety 1) (debug 2)
                           (compilation-speed 3)))
        (logeqv (block b6
                  (logeqv (case 0
                            ((45293 29462 60403) (return-from b6 0))
                            (t c)))))))
    10))
  10)

;;;  Inconsistent stack height 0 != 1
(deftest misc.342
  (let
   #+armedbear ((jvm::*catch-errors* nil))
   nil
   (funcall
    (compile
     nil
     '(lambda (a)
        (declare (optimize (speed 1) (space 0) (safety 2) (debug 1)
                           (compilation-speed 2)))
        (progn (tagbody (imagpart (dotimes (iv3 0 a) (go 4)))
                        4)
               0)))
    1))
  0)

;;; Expecting to find object/array on stack
(deftest misc.343
  (let
   #+armedbear ((jvm::*catch-errors* nil))
   nil
   (funcall
    (compile
     nil
     '(lambda ()
        (declare (optimize (speed 2) (space 3) (safety 2) (debug 3)
                           (compilation-speed 2)))
        (mask-field (byte 0 0)
                    (block b8
                      (reduce 'logior
                              (list (return-from b8 0) 0 0)
                              :end      3
                              :start    0
                              :from-end t)))))))
  0)

;;; Wrong value
(deftest misc.344
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer -3464434 12316202) a))
       (declare (optimize (speed 1) (space 0) (safety 0) (debug 0)
                          (compilation-speed 2)))
       (progn (tagbody (gcd (expt (setf a -2612809) 0) (go 5))
                       5)
              a)))
   1891348)
  -2612809)

;;; Stack size too large
(deftest misc.345
  (let
   #+armedbear ((jvm::*catch-errors* nil))
   nil
   (funcall
    (compile
     nil
     '(lambda (a b c)
        (declare (type (integer -1968 -1759) a))
        (declare (type (integer 91 2293818743282) b))
        (declare (type (integer -843793650839 -2) c))
        (declare (ignorable a b c))
        (declare (optimize (speed 3) (space 2) (safety 3) (debug 0)
                           (compilation-speed 3)))
        (max (block b1
               (conjugate (dotimes (iv3 0
                                        (bit #*010
                                             (min 2
                                                  (max 0
                                                       (return-from b1 0)))))
                            (progn 0))))
             (sbit #*0001011010010 (min 12 (max 0 0))))))
    -1957 523078358699 -634832888815))
  0)

(deftest misc.345a
  (let
   #+armedbear ((jvm::*catch-errors* nil))
   nil
   (funcall
    (compile
     nil
     '(lambda (c)
        (declare (type (integer -3011346550 1630587670) c))
        (declare (optimize (speed 1) (space 1) (safety 0) (debug 3)
                           (compilation-speed 1)))
        (progn (tagbody (dotimes (iv2 0 (- 0 (go 7))) (progn 0))
                        7
                        (progn (mask-field (byte 0 0) 0) c))
               0)))
    1))
  0)

;;; wrong return value
(deftest misc.346
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (speed 2) (space 2) (safety 2) (debug 2)
                          (compilation-speed 2)))
       (bit #*011100
            (min 5
                 (max 0
                      (block b8
                        (aref #(122010971004 126555236004)
                              (min 1
                                   (max 0
                                        (progn (return-from b8 191438621)
                                               0)))))))))))
  0)

;;; The value 8 is not of type FUNCTION.
(deftest misc.347
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (speed 2) (space 2) (safety 3) (debug 2)
                          (compilation-speed 1)))
       (complex (* (block b2
                     (boole boole-xor (logxor (return-from b2 0)) 0)))
                0))))
  0)

;;; Wrong result
(deftest misc.348
  (funcall
   (compile
    nil
    '(lambda (a c)
       (declare (optimize (speed 1) (space 0) (safety 2) (debug 3)
                          (compilation-speed 1)))
       (max (conjugate (setq a -4178265097)) (if (> c 0) 0 a))))
   -2408319173 -4307532101272)
  -4178265097)

(deftest misc.349
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (speed 3) (space 1) (safety 1) (debug 1)
                          (compilation-speed 2)))
       (mod (let ((*s7* (block b7 (logandc2 (+ (return-from b7 0)) 0))))
              -10)
            (max 26 0)))))
  16)

;;; Inconsistent stack height 0 != 1

(deftest misc.350
  (let
   #+armedbear ((jvm::*catch-errors* nil))
   nil
   (funcall
    (compile
     nil
     '(lambda ()
        (declare (optimize (speed 2) (space 3) (safety 1) (debug 2)
                           (compilation-speed 3)))
        (progn (tagbody (complex (- 0 (if (and t) 0 (go tag1))) 0)
                        tag1)
               0)))))
  0)

(deftest misc.351
  (let
   #+armedbear ((jvm::*catch-errors* nil))
   nil
   (funcall
    (compile
     nil
     '(lambda (c)
        (declare (type (integer -598962457711 -2902) c))
        (declare (optimize (speed 1) (space 0) (safety 1) (debug 0)
                           (compilation-speed 3)))
        (lognor c
                (block b1
                  (loop for lv3 below 1
                        sum (if (/= 0) (return-from b1 0) c))))))
    -392248104420))
  392248104419)

(deftest misc.352
  (let
   #+armedbear ((jvm::*catch-errors* nil))
   nil
   (funcall
    (compile
     nil
     '(lambda ()
        (declare (optimize (speed 1) (space 3) (safety 3) (debug 3)
                           (compilation-speed 1)))
        (progn (tagbody (+ 0 (if (< 0) (go 5) 0))
                        5)
               0)))))
  0)

(deftest misc.353
  (let
   #+armedbear ((jvm::*catch-errors* nil))
   nil
   (funcall
    (compile
     nil
     '(lambda (a b)
        (declare (type (integer -8 -2) a))
        (declare (type (integer -67321 14697029362) b))
        (declare (optimize (speed 3) (space 1) (safety 3) (debug 1)
                           (compilation-speed 2)))
        (expt (block b2
                (loop for lv1 below 3
                      sum (prog2 b
                              0
                            (expt (case 0
                                    ((-13960 -57685 -37843 -34222
                                             -14273 -40931 -2688)
                                     (return-from b2 0))
                                    (t a))
                                  0))))
              0)))
    -7 772373806))
  1)

;;; Incorrect return value
(deftest misc.354
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (type (integer -1309 67082465417) a))
       (declare (type (integer -7824641338734 -832606641) b))
       (declare (type (integer 7473698771 3542216118742) c))
       (declare (ignorable a b c))
       (declare (optimize (speed 3) (space 2) (safety 1) (debug 3)
                          (compilation-speed 2)))
       (+ 0
          (progn (tagbody (if (if (>= b (go 3)) nil t) a c)
                          3)
                 0))))
   29329060987 -4964942044116 512158612507)
  0)

(deftest misc.355
  (funcall
   (compile
    nil
    '(lambda (c)
       (declare (type (integer -1390043946499 -115168466439) c))
       (declare (optimize (speed 2) (space 0) (safety 0) (debug 1)
                          (compilation-speed 2)))
       (+ 0
          (coerce (progn (tagbody (if (<= -1 (go tag1)) 0 c)
                                  tag1)
                         0)
                  'integer))))
   -115168466439)
  0)

(deftest misc.356
  (let
   #+armedbear ((jvm::*catch-errors* nil))
   nil
   (funcall
    (compile
     nil
     '(lambda ()
        (declare (optimize (speed 2) (space 2) (safety 1) (debug 0)
                           (compilation-speed 3)))
        (let ((*s7* 0))
          (dotimes (iv2 0 0)
            (block b3
              (block b3 (block b3 (setq *s7* (return-from b3 0)))))))))))
  0)

(deftest misc.357
  (let
   #+armedbear ((jvm::*catch-errors* nil))
   nil
   (funcall
    (compile
     nil
     '(lambda (b)
        (declare (type (integer -1750881587721 -327383867) b))
        (declare (optimize (speed 1) (space 0) (safety 2) (debug 3)
                           (compilation-speed 3)))
        (denominator (block b2
                       (let* ((*s8* 0))
                         (setq *s8*
                               (case 0
                                 ((-26733 -244 -26253 -50028) 0)
                                 (t (return-from b2 b)))))))))
    -1153135130306))
  1)

(deftest misc.358
  (let
   #+armedbear ((jvm::*catch-errors* nil))
   nil
   (funcall
    (compile
     nil
     '(lambda ()
        (declare (optimize (speed 2) (space 0) (safety 0) (debug 3)
                           (compilation-speed 1)))
        (rationalize (let* ((*s1* 0))
                       (block b3
                         (conjugate (let* ((v10
                                            (if (ldb-test (byte 0 0) 0)
                                                (return-from b3 *s1*)
                                              0)))
                                      (setq *s1* (return-from b3 0)))))))))))
  0)

(deftest misc.359
  (let
   #+armedbear ((jvm::*catch-errors* nil))
   nil
   (funcall
    (compile
     nil
     '(lambda (a b)
        (declare (type (integer -477801566869 432060432661) a))
        (declare (type (integer 366578392 525704751) b))
        (declare (optimize (speed 3) (space 3) (safety 1) (debug 1)
                           (compilation-speed 1)))
        (max (case b
               ((0 -3 -2 -2 -3)
                (progn (tagbody (loop for lv1 below 2
                                      count (let* ((*s1* a))
                                              (setq *s1* (go 4))))
                                4)
                       0))
               (t 0)))))
    287358622300 400248608))
  0)

;;; Wrong return value

(deftest misc.360
  (let ((c :good))
    (tagbody
      (dotimes (j 1 (setf c :bad)) (go done))
      done)
    c)
  :good)

;;; sbcl bugs (0.8.10.4)

;;; failed AVER: "(SUBSETP END END-STACK)"
(deftest misc.361
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (notinline boole values denominator list))
       (declare
        (optimize (speed 2)
                  (space 0)
                  (safety 1)
                  (debug 0)
                  (compilation-speed 2)))
       (catch 'ct6
         (progv
             '(*s8*)
             (list 0)
           (let ((v9 (ignore-errors (throw 'ct6 0))))
             (denominator
              (progv nil nil (values (boole boole-and 0 v9)))))))))
   1 2 3)
  0)

;;; sbcl (0.8.10.15)
;;; Wrong return value: SB-KERNEL:*HANDLER-CLUSTERS*
(deftest misc.362
  (funcall
   (compile
    nil
    '(lambda (b g h)
       (declare (optimize (speed 3) (space 3) (safety 2)
                          (debug 2) (compilation-speed 3)))
       (catch 'ct5
         (unwind-protect
             (labels ((%f15 (f15-1 f15-2 f15-3)
                            (rational (throw 'ct5 0))))
               (%f15 0
                     (apply #'%f15
                            0
                            h
                            (progn
                              (progv '(*s2* *s5*) (list 0 (%f15 0 g 0)) b)
                              0)
                            nil)
                     0))
           (common-lisp:handler-case 0)))))
   1 2 3)
  0)

;;; Wrong value: NIL
(deftest misc.363
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer -17286401550789 15753784105886) a))
       (declare (optimize (speed 2) (space 2) (safety 2)
                          (debug 0) (compilation-speed 3)))
       (if (not (>= 0 (shiftf a 110236462073)))
           0
         (elt '(30 101 13 2 10 52 89 57) (min 7 (max 0 a))))))
   -3647332298473)
  57)

;;; "full call to SB-KERNEL:DATA-VECTOR-REF"
(deftest misc.364
  (dotimes (iv1 2 0)
           (if (> iv1 iv1)
                 (svref #(2002 3778 1998 3466 530 3279 2033 521 4085)
                        (min 8 (max 0 iv1)))
                 0))
  0)

;;; OpenMCL/darwin bug (12 May 2004)
(deftest misc.365
  (let* ((fn1
          '(lambda (a b c)
             (declare (type (integer -2 21) a))
             (declare (type (integer -5651364356 4324101092) b))
             (declare (type (integer -30766087 28182568) c))
             (declare (ignorable a b c))
             (declare (optimize (speed 3) (space 1) (safety 3) (debug 0) (compilation-speed 1)))
             (coerce (logxor b -1) 'integer)))
         (fn2
          '(lambda (a b c)
             (declare (notinline logxor coerce))
             (declare (optimize (speed 3) (space 0) (safety 3) (debug 2) (compilation-speed 2)))
             (coerce (logxor b -1) 'integer)))
         (vals '(9 -328421075 -6406890))
         (v1 (apply (compile nil fn1) vals))
         (v2 (apply (compile nil fn2) vals)))
    (if (eql v1 v2) :good (list v1 v2)))
  :good)

;;; sbcl 0.8.10.24
;;;  Argument X is not a REAL: #<FUNCTION "CLOSURE" {947C46D}>

(deftest misc.366
  (funcall
   (compile
    nil
    '(lambda (a b c d e f g h i)
       (declare (type (integer 10 65866342) a))
       (declare (type (integer 151 702748905609) b))
       (declare (type (integer -60442925 167939283) c))
       (declare (type (integer 7706 10562) d))
       (declare (type (integer -97180326158 17496) e))
       (declare (type (integer -73249 -51989) f))
       (declare (type (integer -12 2718) g))
       (declare (type (integer -37832 591244) h))
       (declare (type (integer -2579781276 2108461452) i))
       (declare (ignorable a b c d e f g h i))
       (declare
        (optimize (speed 3)
                  (space 0)
                  (safety 0)
                  (debug 2)
                  (compilation-speed 2)))
       (elt '(11751 8554 7393 1924 3418)
            (min 4
                 (max 0
                      (block b4
                        (numerator
                         (flet ((%f5
                                 (f5-1 f5-2 f5-3
                                       &optional
                                       (f5-4 (prog1 0 (return-from b4 0) 0))
                                       (f5-5 d) (f5-6 0))
                                 0))
                           (numerator
                            (apply (constantly 0)
                                   0
                                   0
                                   (rationalize
                                    (unwind-protect
                                        (%f5 0
                                             c
                                             (%f5 0
                                                  c
                                                  (%f5 0
                                                       0
                                                       0
                                                       h
                                                       (%f5 0 0 0)
                                                       i)
                                                  a))
                                      (ignore-errors 0)))
                                   0
                                   nil))))))))))
   21956127 524275646496 101890987 8762 -88607922426 -55959 2177 147174
   38469170)
  11751)

;;;  The value #<unknown pointer object, widetag=#x29 {A295F27}>
;;;  is not of type RATIONAL.

(deftest misc.367
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer 11557968 115977463) a))
       (declare (type (integer -89510 -20616) b))
       (declare (optimize (speed 2) (space 3) (safety 1)
                          (debug 0) (compilation-speed 1)))
       (rational
        (flet ((%f17 (f17-1 f17-2) 0))
          (%f17
           (numerator
            (%f17
             (denominator
              (catch 'ct5
                (apply (constantly 0)
                       0
                       (unwind-protect
                           (catch 'ct2 (throw 'ct5 (progn (%f17 a b) a))))
                       nil)))
             0))
           (%f17 0 a))))))
   112475717 -25829)
  0)

;;; sbcl 0.8.10.25
;;; "The value -3 is not of type (INTEGER -5 -2)."
(deftest misc.368
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer -5 -2) a))
       (declare (ignorable a))
       (declare
        (optimize (speed 2) (space 3) (safety 1)
                  (debug 1) (compilation-speed 1)))
       (if
           (and (not (not (> a (numerator (setf a -4)))))
                (logbitp 0 (conjugate a)))
           0
         0)))
   -3)
  0)

;;; acl 6.2 (x86 linux trial edition, patched, 4/15/04)
;;; Error: `T' is not of the expected type `NUMBER'
(deftest misc.369
  (funcall
   (compile
    nil
    '(lambda (a b c d e)
       (declare (type (integer -15256078323 33828721319) a))
       (declare (type (integer -44368 22872) b))
       (declare (type (integer -7623 -7522) c))
       (declare (type (integer -53 289) d))
       (declare (type (integer -1853649832248 2196352552304) e))
       (declare (ignorable a b c d e))
       (declare (optimize (speed 1) (space 2) (safety 0) (debug 0)
                          (compilation-speed 3)))
       (flet ((%f2 (f2-1 &optional &key (key1 0) (key2 e))
                   (labels ((%f5 (f5-1 f5-2 f5-3 &optional &key
                                       (key1
                                        (aref #(397)
                                              (min
                                               0
                                               (max
                                                0
                                                (let ((v7 (make-array nil :initial-element d)))
                                                  (reduce
                                                   #'(lambda (lmv5 lmv6) key1)
                                                   (vector f2-1 0)
                                                   :start 0))))))
                                       &allow-other-keys)
                                 0))
                     0)))
         b)))
   -2821485338 -35420 -7622 135 9592294022)
  -35420)

;;; Lispworks personal edition 4.3 (x86 linux)
;;; Inconsistent return value
(deftest misc.370
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (type (integer -3070433 6) a))
       (declare (type (integer -5 -3) b))
       (declare (type (integer -4433759745778 -1) c))
       (declare (ignorable a b c))
       (declare
        (optimize (speed 3)
                  (space 1)
                  (safety 0)
                  (debug 2)
                  (compilation-speed 3)))
       (flet ((%f15
               (f15-1 f15-2
                      &optional (f15-3 0)
                      (f15-4 (denominator (setq c -4214677583716))) (f15-5 0)
                      &key (key1 c) &allow-other-keys)
               (progv '(*s1* *s5* *s7*) (list f15-2 0 f15-1) key1)))
         (%f15 0 (%f15 c 0) 0))))
   -1233959 -4 -2643533316361)
  -4214677583716)

;;; Armed Bear CL

;;; inconsistent stack height
(deftest misc.371
  (let
   #+armedbear ((jvm::*catch-errors* nil))
   nil
   (funcall
    (compile
     nil
     '(lambda (a b c)
        (declare (type (integer -7288 10764) a))
        (declare (type (integer -7 24) b))
        (declare (type (integer 7951930344 11209871544) c))
        (declare (ignorable a b c))
        (declare (optimize (speed 2) (space 2) (safety 0) (debug 0)
                           (compilation-speed 0)))
        (rationalize (block b1
                       (if b
                           (return-from b1
                             (progn (tagbody (return-from b1
                                               (let* ((*s1*
                                                       (cons (go tag3)
                                                             0)))
                                                 (declare (dynamic-extent
                                                           *s1*))
                                                 0))
                                             tag3)
                                    0))
                         0)))))
    -5566 9 10557204445))
  0)

;;; 0 is not of type LIST
(deftest misc.372
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (type (integer -738508 627) a))
       (declare (type (integer -100241328874 104421) b))
       (declare (type (integer -71651668566 4932238952300) c))
       (declare (ignorable a b c))
       (declare (optimize (speed 3) (space 2) (safety 1) (debug 3)
                          (compilation-speed 2)))
       (sbit #*0
             (min 0
                  (max 0
                       (multiple-value-bind (v1)
                           (cons c
                                 (truncate 0
                                           (min -42 0)))
                         (cdr v1)))))))
   -657195 -10801112339 -4291316763)
  0)

;;; inconsistent stack height
(deftest misc.373
  (let
   #+armedbear ((jvm::*catch-errors* nil))
   nil
   (funcall
    (compile
     nil
     '(lambda (a b c)
        (declare (type (integer 0 179061) a))
        (declare (type (integer -15793 42532) b))
        (declare (type (integer -2 0) c))
        (declare (ignorable a b c))
        (declare (optimize (speed 3) (space 0) (safety 2) (debug 1)
                           (compilation-speed 0)))
        (reduce 'logxor
                (list 0 b 0 0
                      a 0 0 0
                      (block b6
                        (let* ((v6 (cons (if c (return-from b6 0) 0) b)))
                          0))
                      0)
                :end 6
                :from-end t)))
    141814 1445 -2))
  142419)

(deftest misc.374
  (let
   #+armedbear ((jvm::*catch-errors* nil))
   nil  
   (funcall
    (compile
     nil
     '(lambda (a b)
        (declare (type (integer -99 4) a))
        (declare (type (integer 35621436 36172433) b))
        (declare (ignorable a b))
        (declare (optimize (speed 2) (space 1) (safety 3) (debug 1)
                           (compilation-speed 0)))
        (lognand (let ((v6 0)) (declare (dynamic-extent v6)) v6)
                 (block b6
                   (let* ((v10
                           (cons (expt (case 0
                                         ((30207) (return-from b6 0))
                                         (t b))
                                       0)
                                 0)))
                     (declare (dynamic-extent v10))
                     0)))))
    -57 35725118))
  -1)

;;; abcl (23 May 2004)
;;; 0 is not of type LIST
(deftest misc.375
  (funcall
   (compile
    nil
    '(lambda (a b c d e f)
       (declare (type (integer -3172868 25583841) a))
       (declare (type (integer -8176159 1565888775976) b))
       (declare (type (integer -2601325109 147819602) c))
       (declare (type (integer -502316251909 515874281072) d))
       (declare (type (integer 174 2604648) e))
       (declare (type (integer 1627646459 3124243119) f))
       (declare (ignorable a b c d e f))
       (declare (optimize (speed 3) (space 0) (safety 3) (debug 2)
                          (compilation-speed 2)))
       (let* ((*s6* (make-array nil :initial-element 0 :adjustable t)))
         (if (logbitp 0
                      (denominator (prog2 (truncate (dotimes (iv3 0 0)
                                                      (progn 0)))
                                       (multiple-value-bind (*s7*)
                                           (cons d 0)
                                         (cdr *s7*)))))
             0
           0))))
   12851164 182468232812 -2243976802 309299185674 2538150 1855615980)
  0)

;;; abcl (25 May 2004)
;;; 0 is not of type LIST
(deftest misc.376
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (speed 1) (space 1) (safety 2) (debug 1)
                          (compilation-speed 0)))
       (dotimes (iv4 3
                     (multiple-value-bind (*s6*) (cons 0 0)
                       (progn (cdr *s6*) 0)))
         (floor (rational (let ((*s2*
                                 (rational (common-lisp:handler-case 0))))
                            0)))))))
  0)

(deftest misc.377
  (funcall
   (compile
    nil
    '(lambda (e) ; (a b c d e)
       (declare (type (integer -46778182694 512) e))
       (declare (optimize (speed 3) (space 3) (safety 2) (debug 2)
                          (compilation-speed 3)))
       (if (block b3
             (numerator (progn (tagbody (truncate (dotimes (iv3 0 0)
                                                    (block b3 0)))
                                        (multiple-value-bind (*s5*)
                                            (cons 0 e)
                                          (rationalize (cdr *s5*))))
                               0)))
           0
         0)))
   10)
  0)

(deftest misc.378
  (funcall
   (compile
    nil
    '(lambda (c)
       (declare (optimize (speed 1) (space 0) (safety 1) (debug 3)
                          (compilation-speed 2)))
       (dotimes (iv4 3 0)
         (restart-case (round (multiple-value-bind (*s6*) (cons c 0)
                                (car *s6*)))))))
   1)
  0)

(deftest misc.379
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (speed 2) (space 0) (safety 2) (debug 2)
                          (compilation-speed 1)))
       (values (floor 0) (multiple-value-bind (v3) (cons 0 0) (car v3))))))
  0 0)

;;; gcl (31 May 2004, cvs head)
;;; Error in APPLY [or a callee]: Expected a FIXNUM
;;; Also fails in cmucl 1/2003
(deftest misc.380
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer -1397457 1846252) a))
       (declare (optimize (speed 2) (space 2) (safety 1)
                          (debug 3) (compilation-speed 3)))
       (let ((v9 (make-array nil :initial-element 0)))
         (declare (dynamic-extent v9))
         (block b8
           (let ((*s1* 0))
             (let ((*s4* (let
                             ((*s1*
                               (return-from b8
                                 (rational
                                  (setf (aref v9)
                                        (deposit-field -5
                                                       (byte 20 30) a))))))
                           0)))
               (let ((*s8* (cons 0 0))) 0)))))))
   399997)
  1125898833500797)

;; This also fails in cmucl (11/2003 image).  This case has not been fully
;; pruned for cmucl.
;;
;; Error in function LISP::ASSERT-ERROR:  The assertion (NOT C::WIN) failed.
(deftest misc.381
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer -1397457 1846252) a))
       (declare (optimize (speed 2) (space 2) (safety 1)
                          (debug 3) (compilation-speed 3)))
       (let ((v9 (make-array nil :initial-element 0)))
         (declare (dynamic-extent v9))
         (block b8
           (let ((s1 0))
             (let ((s4 (let
                             ((s1
                               (return-from b8
                                 (rational
                                  (setf (aref v9)
                                        (deposit-field -5
                                                       (byte 20 30) a))))))
                           0)))
               (let ((s8 (cons 0 0))) 0)))))))
   399997)
  1125898833500797)

                           
;;; gcl (31 May 2004, cvs head)
;;; Error in SYSTEM:ASET [or a callee]: Expected a FIXNUM

(deftest misc.382
  (funcall
   (compile
    nil
    '(lambda (b)
       (declare (type (integer -65822755520 31689335872) b))
       (declare (optimize (speed 2) (space 2) (safety 3)
                          (debug 0) (compilation-speed 1)))
       (let ((s8 (make-array nil :initial-element
                             (catch 'ct4
                               (complex
                                (dotimes (iv1 1 0)
                                  (rational (throw 'ct4 b)))
                                0)))))
         (elt '(13423701584)
              (min 0
                   (max 0
                        (rational
                         (let ((s3 (make-array nil :initial-element 0)))
                           (if (ldb-test (byte 0 0)
                                         (shiftf (aref s8)
                                                 (aref s8)))
                               0 0)))))))))
   -38169486910)
  13423701584)

;;; cmucl 11/2003
;;; Wrong value
(deftest misc.383
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (type (integer -93650 118967004056) a))
       (declare (type (integer -429173946 -3892) b))
       (declare (type (integer -229669685 -50537386) c))
       (declare (ignorable a b c))
       #+cmu (declare (optimize (extensions:inhibit-warnings 3)))
       (declare
        (optimize (speed 3) (space 1) (safety 0) (debug 3) (compilation-speed 2)))
       (logorc2
        (let* ((*s3* (cons 0 a)))
          (declare (dynamic-extent *s3*))
          (shiftf c -124766263))
        411942919)))
   79909316946 -347537841 -210771963)
  -142606339)

;;; abcl 7 Jun 2004
;;; catch-throw now enabled in the abcl compiler

;;; Inconsistent stack height
(deftest misc.384
  (let
      #+armedbear ((jvm::*catch-errors* nil))
      nil
      (funcall
       (compile
        nil
        '(lambda ()
           (catch 'ct8 (throw 'ct8 (catch 'ct7 0)))))))
  0)

(deftest misc.385
  (let
      #+armedbear ((jvm::*catch-errors* nil))
      nil
      (funcall
       (compile nil '(lambda () (values 1 (catch 'ct2 2))))))
  1 2)

(deftest misc.386
  (let
      #+armedbear ((jvm::*catch-errors* nil))
      nil
      (funcall
       (compile nil '(lambda () (values (rationalize (catch 'ct1 1)) 2)))))
  1 2)

(deftest misc.387
  (let
      #+armedbear ((jvm::*catch-errors* nil))
      nil
      (funcall
       (compile nil '(lambda () (block b1 (catch 'ct1 (throw 'ct1 (return-from b1 0))))))))
  0)

;;; ecl (cvs head, 13 June 2004)
;;; Problems with multiple-value-setq

; NIL cannot be coerced to a C int.

(deftest misc.388
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (type (integer 200077 60836768) a))
       (declare (type (integer 339831915 371006999) b))
       (declare (type (integer -13 5553) c))
       (declare (ignorable a b c))
       (declare (optimize (speed 2) (space 1) (safety 0) (debug 0)
                          (compilation-speed 0)))
       (dotimes (iv4 2 0) (multiple-value-setq (c) 4212))))
   8959928 366395687 5048)
  0)

;;; wrong return value

(deftest misc.389
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (type (integer -49972981888 -48068810368) a))
       (declare (type (integer -452283089 -27620701) b))
       (declare (type (integer -24815 15089) c))
       (declare (ignorable a b c))
       (declare (optimize (speed 2) (space 1) (safety 2) (debug 1)
                          (compilation-speed 0)))
       (multiple-value-setq (c) 8015)))
   -49966124671 -68547159 12944)
  8015)

;;; Evaluation order bug
(deftest misc.390
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (type (integer -257 -140) a))
       (declare (type (integer -1 1069496658) b))
       (declare (type (integer -4 2001960914944) c))
       (declare (ignorable a b c))
       (declare (optimize (speed 2) (space 0) (safety 1) (debug 0)
                          (compilation-speed 1)))
       (labels ((%f12 (f12-1 &optional (f12-2 (setq b 63838027)) &key
                             (key1 0) (key2 0))
                      b))
         (boole boole-orc2 b (let ((*s3* (%f12 0))) -14)))))
   -173 1028908375 1289968133290)
  1028908383)

;;; sbcl 0.8.14.14
;;; "The value NIL is not of type SB-C::LVAR"

(deftest misc.391
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (optimize (speed 2) (space 0) (safety 0)
                          (debug 1) (compilation-speed 3)))
       (let* ((v5 (cons b b)))
         (declare (dynamic-extent v5))
         a)))
   'x 'y)
  x)

;;; sbcl 0.8.14.18
;;; "The value #<SB-C::COMBINATION :FUN # :ARGS (# # #) {C44D1C1}>
;;;   is not of type SB-C::REF."

(deftest misc.392
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (notinline /=))
       (declare (optimize (speed 1) (space 2) (safety 1)
                          (debug 3) (compilation-speed 3)))
       (prog2 0 0 (loop for lv4 below 3 count (or b (/= b))))))
   1 2)
  0)

;;; cmucl (2004-09 snapshot)
;;; "Error in function C::CORE-CALL-TOP-LEVEL-LAMBDA:
;;;   Unresolved forward reference."
;;; (in C::CORE-CALL-TOP-LEVEL-LAMBDA)

(deftest misc.393
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -995205 1035654) a))
       (declare (type (integer 473 114804994247) b))
       (declare (ignorable a b))
       #+cmu (declare (optimize (extensions:inhibit-warnings 3)))
       (declare
        (optimize (debug 3) (speed 2) (compilation-speed 0) (space 3)
                  (safety 3)))
       (labels ((%f7
                 (f7-1 f7-2 f7-3
                       &optional (f7-4 (lcm (if (>= b a) 0 a))) (f7-5 0)
                       &key)
                 0))
         (progn (%f7 (%f7 b a a b) b 0) 0))))
   447930 66120263479)
  0)

(deftest misc.393a
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -76 86) a))
       (declare (type (integer -13771285280 109) b))
       (declare (ignorable a b))
       #+cmu (declare (optimize (extensions:inhibit-warnings 3)))
       (declare
        (optimize (safety 3) (space 1) (debug 2) (compilation-speed 3)
                  (speed 3)))
       (dotimes (iv1 2 0)
         (case (min -3693810 a iv1) ((26 -4) (ldb (byte 13 0) a)) (t b)))))
   56 -1579426331)
  0)

;;; cmucl (2004-09 snapshot)
;;; Wrong values

(deftest misc.394
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -76645001 98715919) a))
       (declare (type (integer 0 856472753903) b))
       (declare (ignorable a b))
       #+cmu (declare (optimize (extensions:inhibit-warnings 3)))
       (declare
        (optimize (speed 2) (space 0) (debug 3) (compilation-speed 0)
                  (safety 3)))
       (logeqv 0 b)))
   -34528661 843541658238)
  -843541658239)

(deftest misc.395
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer 6429252570156 8761983588786) a))
       (declare (type (integer -400378288 4971722) b))
       (declare (ignorable a b))
       #+cmu (declare (optimize (extensions:inhibit-warnings 3)))
       (declare
        (optimize (debug 3) (speed 3) (space 2) (safety 0)
                  (compilation-speed 3)))
       (+ (shiftf a 8496033756259) (min 0 b))))
   8369430915156 -369704905)
  8369061210251)

;;; "The assertion (EQ (CAR C::STACK) C::CONT) failed."
(deftest misc.396
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -1601 485) a))
       (declare (type (integer -190428560464 -1444494) b))
       (declare (ignorable a b))
       #+cmu (declare (optimize (extensions:inhibit-warnings 3)))
       (declare
        (optimize (debug 0) (space 2) (speed 0) (safety 3)
                  (compilation-speed 2)))
       (apply (constantly 0) 0 (list (signum b)))))
   -1365 -46960621335)
  0)

;;; "The assertion (EQ (C::FUNCTIONAL-KIND (C::LAMBDA-HOME C::FUN))
;;;      :TOP-LEVEL) failed."
(deftest misc.397
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -168258525920 -2044) a))
       (declare (type (integer -522 54) b))
       (declare (ignorable a b))
       #+cmu (declare (optimize (extensions:inhibit-warnings 3)))
       (declare
        (optimize (speed 0) (safety 3) (compilation-speed 1) (space 0)
                  (debug 2)))
       (labels ((%f4 (f4-1 f4-2 &key)
                     (flet ((%f7 (f7-1 f7-2 f7-3 &optional &key (key1 a))
                                 (progv '(*s1* *s6* *s2*) (list a 0 key1) f4-1)))
                       f4-2)))
         (apply #'%f4 (list a 0)))))
   -156882103995 -38)
  0)

;;; "Error in function C::CLOSURE-POSITION:
;;;   Can't find #<C::LAMBDA-VAR #x594259BD
;;;                  NAME= IV1
;;;                  TYPE= #<NUMERIC-TYPE (MOD 3)>>"
(deftest misc.398
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -319 7353) a))
       (declare (type (integer 31751 4233916489) b))
       (declare (ignorable a b))
       #+cmu (declare (optimize (extensions:inhibit-warnings 3)))
       (declare
        (optimize (safety 3) (compilation-speed 1) (debug 1) (speed 0)
                  (space 0)))
       (conjugate
        (if t
            (labels ((%f12 (f12-1 f12-2 f12-3)
                           0))
              (%f12 0 b 0))
          (dotimes (iv1 2 0) (catch 'ct2 a))))))
   4430 3476635674)
  0)

;;; "NIL is not of type C::CONTINUATION"
;;; in C::FIND-PUSHED-CONTINUATIONS
(deftest misc.399
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer -3 1) a))
       (declare (ignorable a))
       #+cmu (declare (optimize (extensions:inhibit-warnings 3)))
       (declare
        (optimize (space 0) (debug 0) (speed 3) (compilation-speed 2)
                  (safety 3)))
       (catch 'ct8 (logior a -457019 -1))))
   0)
  -1)

;;; Wrong value
(deftest misc.400
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer 3376 4762) a))
       (declare (ignorable a))
       #+cmu (declare (optimize (extensions:inhibit-warnings 3)))
       (declare
        (optimize (debug 0) (safety 0) (space 0) (compilation-speed 3)
                  (speed 3)))
       (case (lognand 775 a) ((-7) 0) (t 4))))
   4182)
  0)

;;; Invalid number of arguments: 1
(deftest misc.401
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer 7299 257071514003) a))
       (declare (ignorable a))
       #+cmu (declare (optimize (extensions:inhibit-warnings 3)))
       (declare
        (optimize (compilation-speed 2) (space 1) (safety 2) (speed 1)
                  (debug 2)))
       (logeqv (setq a 220250126156) 0)))
   157474319912)
  -220250126157)

;;; "The assertion (EQ (CAR C::NEW-STACK) C::CONT) failed."
(deftest misc.402
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer -19116544 21344004) a))
       (declare (ignorable a))
       #+cmu (declare (optimize (extensions:inhibit-warnings 3)))
       (declare
        (optimize (space 1) (safety 3) (debug 1) (compilation-speed 0)
                  (speed 0)))
       (dotimes (iv3 2 0)
         (progn
           (apply (constantly 0)
                  (list
                   (let* ((*s1* 0))
                     *s1*)))
           0))))
   10)
  0)

;;; "The assertion C::INDIRECT failed."
(deftest misc.403
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer -6456 -32) a))
       (declare (ignorable a))
       #+cmu (declare (optimize (extensions:inhibit-warnings 3)))
       (declare
        (optimize (space 3) (safety 1) (compilation-speed 1) (speed 0)
                  (debug 0)))
       (dotimes (iv1 0 a) (loop for lv4 below 3 sum (catch 'ct8 0)))))
   -1648)
  -1648)

;;; From abcl (cvs, 15 Sept 2004)
;;; Inconsistent stack height

(deftest misc.404
  (let #+armedbear ((jvm::*catch-errors* nil))
       nil
       (funcall
        (compile
         nil
         '(lambda (a b)
            (declare (type (integer -77007578505 7500480849) a))
            (declare (type (integer 211464 53140083) b))
            (declare (ignorable a b))
            (declare (optimize (compilation-speed 0) (speed 2) (debug 3)
                               (safety 1) (space 3)))
            (progn (tagbody (let ((v3
                                   (cons (case a
                                           ((13 5 -9 2 -13) (go tag8))
                                           (t 0))
                                         0)))
                              0)
                            tag8)
                   a)))
        -1068524571 20786758))
  -1068524571)

(deftest misc.405
  (let #+armedbear ((jvm::*catch-errors* nil))
       nil
       (funcall
        (compile
         nil
         '(lambda (a b)
            (declare (type (integer -82196 13938) a))
            (declare (type (integer -44152792 -15846835) b))
            (declare (ignorable a b))
            (declare (optimize (compilation-speed 3) (safety 2) (speed 3)
                               (space 0) (debug 0)))
            (block b5
              (let ((*s7*
                     (cons (if (position (if (eql 0 0)
                                             (return-from b5
                                               (return-from b5
                                                 (let ((*s6* (cons b a))) 0)))
                                           b)
                                         #(23)
                                         :test-not
                                         'eql)
                               0
                             0)
                           b)))
                0))))
        -10305 -26691848))
  0)

(deftest misc.406
  (let #+armedbear ((jvm::*catch-errors* nil))
       nil
       (funcall
        (compile
         nil
         '(lambda (a)
            (declare (type (integer -1 1412366903315) a))
            (declare (ignorable a))
            (declare (optimize (debug 3) (safety 3) (space 3)
                               (compilation-speed 1) (speed 2)))
            (progn (tagbody (case 0 ((1 0 4) (values (go 1) 0)) (t 0))
                            1)
                   0)))
        251841706892))
  0)

;;; Incorrect binding
(deftest misc.407
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer -324 175) a))
       (declare (ignorable a))
       (declare (optimize (safety 0) (space 0) (speed 2) (debug 0)
                          (compilation-speed 0)))
       (multiple-value-bind (v5) (cons (truncate 0) a) (cdr v5))))
   -279)
  -279)

;;; Stack size too large

(deftest misc.408
  (let #+armedbear ((jvm::*catch-errors* nil))
       nil
       (funcall
        (compile
         nil
         '(lambda (a)
            (declare (type (integer 0 0) a))
            (declare (ignorable a))
            (declare (optimize (compilation-speed 0) (safety 3) (speed 0)
                               (debug 1) (space 0)))
            (progn (tagbody (dotimes (iv4 0
                                          (let ((v5 (cons 0 (if (go 3) 0 0)))) 0))
                              (progn 0))
                            3)
                   (ash 0 (min 16 0)))))
        0))
  0)

;;; ecl (07 Oct 2004)
;;; (0 . 0) is not of type REAL

(deftest misc.409
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -40524 53538) a))
       (declare (type (integer -5967075 -235) b))
       (declare (ignorable a b))
       (declare (optimize (speed 2) (safety 1) (space 2)
                          (compilation-speed 3) (debug 0)))
       (labels ((%f2 (f2-1 f2-2 &optional (f2-3 0) (f2-4 a)) 0))
         (apply #'%f2 a
                (%f2 b
                     (flet ((%f12 (f12-1 f12-2 f12-3 &optional &key
                                         (key1 0) (key2 0))
                                  (%f2 0 0)))
                       (reduce #'(lambda (lmv2 lmv1) (%f2 0 0 a))
                               (list 0 0 a 0 0 0 a) :end 7))
                     0)
                nil))))
   -7465 -3590953)
  0)

#|
;;; A bug was found in the compiler.  Contact worm@arrakis.es.

Broken at C::WT-MAKE-CLOSURE.
|#
(deftest misc.410
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (safety 0) (space 1) (compilation-speed 0)
                          (speed 2) (debug 0)))
       (let ((*s2* 0))
         (declare (special *s2*))
         (reduce #'(lambda (lmv1 lmv2) *s2*) (vector 0) :end 1
                 :start 0)))))
  0)

;;; THROW: The catch CT2 is undefined.
(deftest misc.411
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (safety 2) (debug 0) (space 0)
                          (compilation-speed 2) (speed 0)))
       (catch 'ct2 (values 0 (throw 'ct2 0)))
       0)))
  0)

;;; /tmp/eclDD7aumXi8.c: In function `LC3':
;;; /tmp/eclDD7aumXi8.c:9: `env0' undeclared (first use in this function)
(deftest misc.412
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -25409 1946) a))
       (declare (type (integer -215956065 223815244) b))
       (declare (ignorable a b))
       (declare (optimize (compilation-speed 2) (space 3) (debug 2)
                          (safety 1) (speed 3)))
       (complex (flet ((%f15 (f15-1 &optional &key (key1 0)) 0))
                  (reduce #'(lambda (lmv6 lmv1) (%f15 lmv1))
                          (list b 0)))
                0)))
   -21802 -105983932)
  0)

;;; Different resutls: #<a type-error>, 0
(deftest misc.413
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -120206733 37762378) a))
       (declare (type (integer 2777758072 5675328792) b))
       (declare (ignorable a b))
       (declare (optimize (compilation-speed 3) (space 3) (debug 3)
                          (safety 0) (speed 1)))
       (labels ((%f8 (f8-1 f8-2 &optional &key (key1 0))
                     (let* ((v2 (ash f8-1 (min 63 a)))) 0)))
         (ignore-errors
           (logand (apply #'%f8 0 b nil)
                   (unwind-protect
                       0
                     (ash (%f8 0 0)
                          (min 48
                               (flet
                                   ((%f12
                                     (f12-1 f12-2 &optional &key
                                            (key1 a) (key2 b)
                                            &allow-other-keys)
                                     0))
                                 b)))))))))
   -4794909 4095236669)
  0)

;;; sbcl 0.8.14.28
;;; Wrong value computed

(deftest misc.414
  (funcall
   (compile
    nil
    '(lambda (c)
       (declare (optimize (speed 1) (space 3) (compilation-speed 3)
                          (debug 3) (safety 1)))
       (if (setq c 2)
           (case (shiftf c 1) ((2) c) (t 0))
         0)))
   0)
  1)

;;; cmucl
;;; Sept. 2004 snapshot
;;; Wrong return value

(deftest misc.415
  (funcall
   #'(lambda (a c)
       (catch 'ct2
         (flet ((%f17 (&optional x &key)
                        (let* ((y (cons (dotimes (iv3 0)) 0)))
                          a)))
           c)))
   :bad :good)
  :good)

;;; Wrong value
(deftest misc.416
  (funcall
   (compile
    nil
    '(lambda (b)
       (declare (type (integer 12052668 22838464) b))
       (declare (ignorable a b c))
       (declare        (optimize (compilation-speed 3) (debug 2) (speed 1) (space 0)
                          (safety 3)))
       (min (mask-field (byte 2 18) b) 89582)))
   13891743)
  0)

;;; Invalid number of arguments: 3
(deftest misc.417
  (funcall
   (compile
    nil
    '(lambda (c)
     (declare (type (integer 995 22565094) c))
     (declare (optimize (safety 2) (debug 1) (space 0) (compilation-speed 2)
                        (speed 1)))
     (numerator (floor (numerator (deposit-field 0 (byte 0 0) c))))))
   17190042)
  17190042)

;;; Invalid number of arguments: #<Unknown Immediate Object, lowtag=#b110, type=#x66
;;;                               {598FCC66}>
(deftest misc.418
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (type (integer 1670923021 2536883848) a))
       (declare (ignorable a b c))
       (declare (optimize (safety 3) (compilation-speed 3) (speed 1) (debug 1)
                          (space 2)))
       (if (logior (setf c 67) 0 a) a 0)))
   2161404325 -1968715305 83)
  2161404325)

;;;   nil is not of type c::continuation
;;; (c::convert-type-check #<Continuation c1>
;;;                       ((nil #<numeric-type integer> #<numeric-type integer>)))

(deftest misc.419
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (safety 3) (speed 3) (compilation-speed 1) (space 1)
                          (debug 2)))
       (boole boole-set 0 (case 2 ((0) 0) (t (numerator (catch 'ct2 0))))))))
  -1)

;;;  nil is not of type c::continuation
;;; (c::convert-type-check #<Continuation c1>
;;;                       ((nil #<union-type real> #<union-type real>)))

(deftest misc.420
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -65954801 6519292634236) a))
       (declare (type (integer 5721249203 36508717226) b))
       (declare (ignorable a b))
       (declare
        (optimize (space 3) (compilation-speed 2) (safety 3) (speed 0)
                  (debug 2)))
       (flet ((%f14 (f14-1 f14-2 &key)
                    (prog2 0 f14-2 (min (catch 'ct4 (floor 120378948 (max 22 a)))))))
         (reduce #'(lambda (lmv6 lmv5) (%f14 0 0))
                 (vector 0 0 0)
                 :start
                 0
                 :from-end
                 t))))
   6313133774518 10840050742)
  0)

;;; Invalid number of arguments: 1
(deftest misc.421
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (optimize (debug 0) (space 2) (compilation-speed 1) (safety 0)
                          (speed 0)))
       (imagpart (block b8 (logior (catch 'ct7 (return-from b8 a)) -1123785)))))
   -1021899)
  0)

;;; Invalid number of arguments: 2
(deftest misc.422
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer -13 -3) a))
       (declare (optimize (space 2) (debug 1) (safety 1) (speed 2)
                          (compilation-speed 1)))
       (logorc2 (sbit #*0010000011101010 (min 15 (max 0 0))) a)))
   -7)
  6)

;;;   nil is not of type c::continuation
;;; (c::convert-type-check #<Continuation c1>
;;;                        ((t #<member-type null> #<union-type real>)))
(deftest misc.423
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer 0 1) a))
       (declare (type (integer -8031148528 5509023941) b))
       #+cmu (declare (optimize (extensions:inhibit-warnings 3)))
       (declare (optimize (space 2) (safety 3) (debug 1) (compilation-speed 3) (speed 2)))
       (min 0 (ignore-errors (logand 0 b 388)))))
   0 4604112015)
  0)

;;; Argument x is not a real: nil.
;;; (kernel:two-arg-> nil 0)

(deftest misc.424
  (funcall
   (compile
    nil
    '(lambda (a b)
     (declare (type (integer -24 15) a))
     (declare (type (integer -99661829155 16) b))
     (declare (ignorable a b))
     #+cmu (declare (optimize (extensions:inhibit-warnings 3)))
     (declare (optimize (safety 3) (debug 1) (compilation-speed 1) (space 3)
                        (speed 3)))
     (catch 'ct4
       (logandc1 a
                 (ignore-errors
                  (let* ((v8 (complex (throw 'ct4 0) 0)))
                    0))))))
   -18 -47519360453)
  0)

;;; Different results
(deftest misc.425
  (funcall
   (compile
    nil
    '(lambda (a b)
     (declare (type (integer -394128 80657) a))
     (declare (type (integer 13729431 14852298) b))
     (declare (optimize (space 2) (compilation-speed 1) (safety 0) (debug 0)
                        (speed 2)))
     (logorc1 (* a (logior b 0)) 0)))
   -80334 14527920)
  1167085925279)

;;; Unable to display error condition
(deftest misc.426
  (funcall
   (compile
    nil
    '(lambda ()
       #+cmu (declare (optimize (extensions:inhibit-warnings 3)))
       (declare (optimize (safety 3) (space 3) (speed 3) (debug 1)
                          (compilation-speed 3)))
       (dotimes (iv3 1 0) (logxor iv3 1285775)))))
  0)

;;; sbcl 0.8.15.13
;;; NIL is not of type REAL
;;; (This appears to be related to DYNAMIC-EXTENT)

(deftest misc.427
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (notinline list reduce logior))
       (declare (optimize (safety 2) (compilation-speed 1)
                          ; #+sbcl (sb-c:insert-step-conditions 0)
                          (speed 3) (space 2) (debug 2)))
       (logior
        (let* ((v5 (reduce #'+ (list 0 a))))
          (declare (dynamic-extent v5))
          (1- v5)))))
   17)
  16)

(deftest misc.428
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (notinline -))
       (declare        (optimize (compilation-speed 0) (safety 1) (speed 0)
                          (debug 2) (space 3)))
       (let ((v10 (catch 'ct2 1)))
         (declare (dynamic-extent v10))
         (- v10)))))
  -1)

(deftest misc.429
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (safety 1) (debug 1) (space 2)
                          (speed 2) (compilation-speed 1)))
       (let ((v8 (let ((*s3* 0)) *s3*)))
         (declare (dynamic-extent v8))
         (logandc1 v8 28)))))
  28)

;;; poplog 15.53
;;; Excess type specifier(s) in THE special form

(deftest misc.430
  (unwind-protect 0 (the integer 1))
  0)

;;; Wrong return values: T, 0
(deftest misc.431
  (funcall
   (compile
    nil
    '(lambda (a) (declare (notinline > *))
       (declare (optimize (compilation-speed 0) (safety 2) (speed 2) (debug 0) (space 3)))
       (catch 'ct1 (* a (throw 'ct1 (if (> 0) a 0))))))
   5445205692802)
  5445205692802)

;;; Ste: stack empty (missing argument? missing result?)
(deftest misc.432
  (loop for x below 2 count (not (not (typep x t))))
  2)

(deftest misc.433
  (let ((a 1)) (if (not (/= a 0)) a 0))
  0)

;;; sbcl 0.8.16.13
;;;   #<SB-C:TN t1> is not valid as the first argument to VOP:
;;;   SB-VM::FAST-ASH-LEFT-MOD32/UNSIGNED=>UNSIGNED
;;; Primitive type: T
;;; SC restrictions:
;;;   (SB-VM::UNSIGNED-REG)
;;; The primitive type disallows these loadable SCs:
;;;   (SB-VM::UNSIGNED-REG)

(deftest misc.434
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -8431780939320 1571817471932) a))
       (declare (type (integer -4085 0) b))
       (declare (ignorable a b))
       (declare
        (optimize (space 2)
                  (compilation-speed 0)
                  #+sbcl (sb-c:insert-step-conditions 0)
                  (debug 2)
                  (safety 0)
                  (speed 3)))
       (let ((*s5* 0))
         (dotimes (iv1 2 0)
           (let ((*s5*
                  (elt '(1954479092053)
                       (min 0
                            (max 0
                                 (if (< iv1 iv1)
                                     (lognand iv1 (ash iv1 (min 53 iv1)))
                                   iv1))))))
             0)))))
   -7639589303599 -1368)
  0)

;;; failed AVER:
;;;  "(AND (EQ (CTRAN-KIND START) INSIDE-BLOCK) (NOT (BLOCK-DELETE-P BLOCK)))"
(deftest misc.435
  (funcall
   (compile
    nil
    '(lambda (a b c d)
       (declare (notinline aref logandc2 gcd make-array))
       (declare
        (optimize (space 0)
                  (safety 0)
                  (compilation-speed 3)
                  (speed 3)
                  (debug 1)
                  ))
       (progn
         (tagbody
          (let* ((v2
                  (make-array nil :initial-element (catch 'ct1 (go tag2)))))
            (declare (dynamic-extent v2))
            (gcd (go tag2) (logandc2 (catch 'ct2 c) (aref v2))))
          tag2)
         0)))
   3021871717588 -866608 -2 -17194)
  0)

;;; In sbcl 0.8.16.18
;;;   #<SB-C:TN INTEGER!1> is not valid as the first argument to VOP:
;;;   SB-VM::FAST-ASH-LEFT-MOD32/UNSIGNED=>UNSIGNED
;;; Primitive type: T
;;; SC restrictions:
;;;   (SB-VM::UNSIGNED-REG)
;;; The primitive type disallows these loadable SCs:
;;;   (SB-VM::UNSIGNED-REG)

(deftest misc.436
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -2917822 2783884) a))
       (declare (type (integer 0 160159) b))
       (declare (ignorable a b))
       (declare
        (optimize (compilation-speed 1)
                  (speed 3)
                  (safety 3)
                  (space 0)
                  ; #+sbcl (sb-c:insert-step-conditions 0)
                  (debug 0)))
       (if
           (oddp
            (loop for
                  lv1
                  below
                  2
                  count
                  (logbitp 0
                           (1-
                            (ash b
                                 (min 8
                                      (count 0
                                             '(-10197561 486 430631291
                                                         9674068))))))))
           b
         0)))
   1265797 110757)
  0)

;;;  The value NIL is not of type INTEGER.
;;; (in (SB-C::TN-SC-OFFSET 1 #<SB-C:TN #:G27!1>))

(deftest misc.437
  (funcall
   (compile
    nil
    '(lambda (a b c d e)
       (declare (notinline values complex eql))
       (declare
        (optimize (compilation-speed 3)
                  (speed 3)
                  ; #+sbcl (sb-c:insert-step-conditions 0)
                  (debug 1)
                  (safety 1)
                  (space 0)))
       (flet ((%f10
               (f10-1 f10-2 f10-3
                      &optional (f10-4 (ignore-errors 0)) (f10-5 0)
                      &key &allow-other-keys)
               (if (or (eql 0 0) t) 0 (if f10-1 0 0))))
         (complex (multiple-value-call #'%f10 (values a c b 0 0)) 0))))
   80043 74953652306 33658947 -63099937105 -27842393)
  0)

;;;   #<SB-C:TN COUNT!1> is not valid as the second argument to VOP:
;;;   SB-VM::FAST-ASH-LEFT-MOD32/UNSIGNED=>UNSIGNED
;;; Primitive type: T
;;; SC restrictions:
;;;   (SB-VM::UNSIGNED-REG)
;;; The primitive type disallows these loadable SCs:
;;;   (SB-VM::UNSIGNED-REG)

(deftest misc.438
  (funcall
   (compile
    nil
    ' (lambda (a)
        (declare (type (integer 0 1696) a))
        ; (declare (ignorable a))
        (declare (optimize (space 2) (debug 0) (safety 1)
                   (compilation-speed 0) (speed 1)))
        (if (logbitp 0 (ash (1- a) (min 11 a))) 0 0)))
   805)
  0)

;;; "The value -13589 is not of type (INTEGER -15205 18871)"
(deftest misc.439
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer -15205 18871) a))
       (declare (ignorable a))
       (declare
        (optimize (space 2)
                  ; (sb-c:insert-step-conditions 0)
                  (speed 1)
                  (safety 1)
                  (debug 1)
                  (compilation-speed 3)))
       (if (<= a (- (setf a 10305))) a 0)))
   -13589)
  10305)

;;; In ACL 7.0 (sparc, Solaris 8, 11 Nov 2004)
;;; Error: the value of (CAR EXCL::INTEGERS) is NIL, which is not of type INTEGER.

(deftest misc.440
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (notinline logior))
       (declare (optimize (safety 3) (debug 1) (speed 0) (space 1)
                          (compilation-speed 3)))
       (flet ((%f10 (&optional &key
                               (key1
                                (logior (flet ((%f4 (f4-1
                                                     &optional
                                                     &key
                                                     (key1 0)
                                                     (key2 b)
                                                     &allow-other-keys)
                                                 c))
                                          (%f4 0))))
                               &allow-other-keys)
                0))
         (let ((*s8* (%f10)))
           (declare (special *s8*))
           *s8*))))
   13524 4484529434427 8109510572804)
  0)

;;; Error: the value of realpart is nil, which is not of type (or rational float).
(deftest misc.441
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (notinline complex))
       (declare (optimize (compilation-speed 1) (space 1) (speed 3) (safety 2) (debug 3)))
       (flet ((%f8 (f8-1 f8-2 &optional
                         &key (key1 (labels ((%f9 nil a)) (complex (%f9) 0)))
                         (key2 0) &allow-other-keys)
                0))
         (%f8 0 a))))
   1 2)
  0)

;;; Error: the value of excl::x is nil, which is not of type integer.
(deftest misc.442
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (notinline apply evenp))
       (declare (optimize (speed 1) (space 1) (safety 1) (compilation-speed 0) (debug 0)))
       (labels ((%f18 (f18-1 &optional
                             &key
                             (key1 (flet ((%f8 nil b)) (if (evenp (%f8)) 0 a)))
                             (key2 0))
                  0))
         (apply #'%f18 b nil))))
   505808341634 -39752189)
  0)

;;; Error: No from-creg to move to <3:iparam2@(:iparam 2){4=c{s:<3>}}> before (move-throw-tag nil nil -> ({18}) ([18>>:frame :dfr]))

(deftest misc.443
  (funcall
   (compile
    nil
    '(lambda (a b c d e)
       (declare (type (integer -2310674 2) a))
       (declare (type (integer -492505702625 -147091001460) b))
       (declare (type (integer -27638568 52971156) c))
       (declare (type (integer -151 203) d))
       (declare (type (integer -1400301 8173230) e))
       (declare (ignorable a b c d e))
       (declare (optimize (compilation-speed 3) (debug 0) (space 0) (safety 1) (speed 1)))
       (catch 'ct7 (lcm (case 0
                          ((-4557) (let ((*s7* (max d))) 0))
                          ((-15387) c)
                          (t 0))
                        (unwind-protect (throw 'ct7 b) 0)))))
   -1748290 -244489705763 38969920 -90 341977)
  -244489705763)

;;; misc.444
;;; misc.445

;;; gcl 25 Nov 2004
;;; Incorrect return value
(deftest misc.446
  (funcall
   (compile
    nil
    '(lambda (a b c d)
       (declare (type (integer -1254 1868060) a))
       (declare (type (integer -1 0) b))
       (declare (type (integer -424707253248 -82453721088) c))
       (declare (type (integer -252962 3018671) d))
       (declare (ignorable a b c d))
       (declare (optimize (safety 3) (space 3) (speed 3)
                          (compilation-speed 3) (debug 3)))
       (* (labels ((%f8 (&optional (f8-1 0)) (setq b 0)))
            (if (> d 1668249724 (%f8)) 0 (complex a 0)))
          (if (oddp b) 0 c))))
   796131 -1 -338008808923 530637)
  -269099291056676913)

(deftest misc.447
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer 38632397 46632460288) a))
       (declare (optimize (space 0) (safety 0) (debug 1)
                          (compilation-speed 1) (speed 0)))
       (catch 'ct2 (if (= a 0 (throw 'ct2 0)) 1 2289596))))
   18160383912)
  0)

(deftest misc.448
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -3716 1269) a))
       (declare (type (integer -1976579 2312) b))
       (declare (optimize (compilation-speed 1) (safety 0)
                          (speed 0) (space 0) (debug 3)))
       (if (<= 0 b (setq a 117)) 0 a)))
   -1147 -44004)
  117)

;;; gcl 27 Nov 2004
;;; Incorrect return value

(deftest misc.449
  (funcall (compile nil '(lambda (a) (* 10 a (setq a 1000)))) 1)
  10000)

;;; Error in COMPILER::CMP-ANON [or a callee]: The variable MIN is unbound.
(deftest misc.450
  (funcall
   (compile nil '(lambda (a b) (min 0 (reduce #'min (vector a b 0)) 0)))
   -10 -1)
  -10)

;;; gcl 28 Nov 2004
;;; Incorrect return value

(deftest misc.451
  (funcall (compile nil '(lambda (a b) (flet ((%f3 () (setq a -2210)))
                                         (logxor a b (%f3)))))
           -22650 20595)
  171)

(deftest misc.452
  (funcall (compile nil '(lambda (d) (labels ((%f3 () (setf d -1135) -983))
                                       (+ d (%f3) 11267))))
           -2914)
  7370)

(deftest misc.453
  (funcall (compile nil '(lambda (a) (* a (setf a 2) a (identity 5))))
           3)
  60)

(deftest misc.454
  (let* ((form '(let ((v1 0)) (decf v1 (setq v1 -1))))
         (val1 (eval form))
         (val2 (funcall (compile nil `(lambda () ,form)))))
    (if (eql val1 val2) :good
      (list val1 val2)))
  :good)

;;; sbcl 0.8.17.24
;;; Bugs in the just-introduced fixnum arithmetic transforms

;;; LOGAND (?) bug

(deftest misc.455
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -4079701634499 2272876436845) b))
       (declare (optimize (space 0) (compilation-speed 1)
                          (safety 3) (speed 2) (debug 0)))
       (logand (* -775 b) a 37284)))
   -18465060867 832909434173)
  32772)

(deftest misc.456
  (funcall
   (compile
    nil
    '(lambda (b c)
       (declare (type (integer -30606350847 35078064098) b))
       (declare (type (integer -6652 6638) c))
       (declare (optimize (space 3) (safety 0)
                          (speed 0) (compilation-speed 2) (debug 1)))
           (logand (* -9964236 (setq c 6206) 2600) b c)))
    17296668225 -6574)
  4096)

;;; DEPOSIT-FIELD (?) bug

(deftest misc.457
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -455461 343063) a))
       (declare (type (integer -1020097 -12430) b))
       (declare (optimize (speed 3) (space 0) (compilation-speed 3)
                          (debug 0) (safety 3)))
       (deposit-field (* (logeqv a a) b) (byte 6 24) 0)))
   -212811 -985078)
  0)

;;; LDB, *

(deftest misc.458
  (funcall
   (compile
    nil
    ' (lambda (a)
           (declare (type (integer -8175 27760966190) a))
           (declare (optimize
                     ;; The next optimize declaration is necessary
                     ;; for the bug to occur in sbcl 0.8.17.24
                     #+sbcl (sb-c:insert-step-conditions 0)
                     (space 2) (speed 0) (compilation-speed 1)
                     (safety 0) (debug 3)))
           (ldb (byte 29 0) (* a a))))
   14774118941)
  101418825)

;;; LOGAND, +

(deftest misc.459
  (funcall
   (compile
    nil
    '(lambda (a b)
           (declare (type (integer -32933298905 -168011) a))
           (declare (type (integer -190015111797 16) b))
           (declare (optimize (speed 2) (compilation-speed 0) (space 0)
                              (safety 1) (debug 0)))
           (logand (+ b -9255) a 63)))
   -8166030199 -45872222127)
  8)

;;; In sbcl 0.8.17.28-signed-modular-arithmetic.3
;;; Unreachable code is found or flow graph is not properly depth-first ordered.
;;; (This is apparently a different bug from the previous ones that
;;;  were causing this message to be printed.)

(deftest misc.460
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer 50354997 50514623) a))
       (declare (ignorable a))
       (declare
        (optimize (speed 0)
                  (safety 0)
                  (compilation-speed 3)
                  #+sbcl (sb-c:insert-step-conditions 0)
                  (debug 1)
                  (space 1)))
       (loop for lv3 below 2
             sum (if (find 0
                           '(-17604051 126613572 -795198 12037855 127043241 -2 -59
                                       -3458890 1505 -1 -2 107498637 -977489 172087 421813
                                       543299114 12 4311490 569 -3509 -4051770 -1 1 1
                                       216399387 -2482 143297 2 304550 -61 -195904988
                                       57682175 2344 1294831 -247 -2 25779388 -296 -12115
                                       -158487 -15)
                           :test 'eql)
                     (if (find 0 #(4193594) :test '<)
                         (min (catch 'ct6 0) (catch 'ct8 0) 0)
                       (let ((*s1* (cons a 0)))
                         (car *s1*)))
                   0))))
   50395193)
  0)

;;; gcl 16 Dec 2004
;;; Error possibly related to type propagation

(deftest misc.461
  (funcall
   (compile nil '(lambda (a)
                   (declare (type (integer -26657952320 0) a))
                   (declare (optimize (compilation-speed 0) (space 3) (speed 3)
                                      (safety 0) (debug 2)))
                   (- a
                      (ash -1 (min 31 (- a)))
                      -26715477)))
   -26179151369)
  -24004952244)

;;; gcl 18 Dec 2004
;;; Doesn't cause an error, unless -Werror is added to gcc flags
;;; gazonk0.c: In function `L1':
;;; gazonk0.c:5257: warning: assignment makes integer from pointer without a cast

(deftest misc.462
  (funcall
   (compile nil '(lambda (a b)
                   (declare (type (integer -2726808666112 -26532) a))
                   (declare (type (integer 182701814 171137312256) b))
                   (declare (ignorable a b))
                   (declare (optimize (compilation-speed 3) (safety 0)
                                      (speed 3) (space 3) (debug 3)))
                   (ash (let* ((v8 (cons 0 0))) 0) (min 15 a))))
   -1982565461868 46279989780)
  0)

;;; gazonk0.c: In function `L1':
;;; gazonk0.c:5262: warning: assignment makes integer from pointer without a cast
(deftest misc.463
  (funcall
   (compile nil '(lambda (a b)
                   (declare (type (integer 0 0) a))
                   (declare (type (integer -160364747008 264742845184) b))
                   (declare (ignorable a b))
                   (declare (optimize (debug 0) (safety 0)
                                      (compilation-speed 2) (space 0)
                                      (speed 1)))
                   (ash (multiple-value-setq (a) 0) (min 97 13027666096))))
   0 34670845086)
  0)

;;; gcl 21 Dec 2004
;;; Compiler error on ash, rem

(deftest misc.464
  (funcall
   (compile nil '(lambda ()
                   (declare (optimize (debug 1) (safety 2) (compilation-speed 0)
                                      (space 1) (speed 1)))
                   (count (ash (the integer
                                 (macrolet () (rem -197 (min -72 215))))
                               (min 98 442719))
                          #(0 96) :test '=))))
  0)

(deftest misc.465
  (funcall
   (compile nil '(lambda (a)
                   (declare (type (integer -18822 -1280) a))
                   (declare (optimize (debug 0) (speed 1)
                                      (compilation-speed 3) (safety 0)
                                      (space 0)))
                   (ash (the integer
                          (logand a (if t a (imagpart -2607360))))
                        (min 79 (catch 'ct7 0)))))
   -17635)
  -17635)

;;; ACL 6.2 (x86 linux)
;;; Bug in type propagation for ISQRT
;;; Found with the special purpose random tester for type propagation

;;; While compiling (:ANONYMOUS-LAMBDA 22203):
;;; Error: -1 is illegal argument to isqrt

(deftest misc.466
  (funcall (compile nil '(lambda (x)
                           (declare (type (member 4 -1) x)
                                    (optimize speed (safety 1)))
                           (isqrt x)))
           4)
  2)

;;; gcl 24 Dec 2004
;;; Incorrect results (these may all be related)
;;; These are also produced by the special purpose tester in random-type-prop.lsp

(deftest misc.467
  (funcall
   (compile nil '(lambda (p2 p3)
                   (declare (optimize speed (safety 1))
                            (type (integer -990888631320) p2)
                            (type (integer -20346 -19755) p3))
                   (+ -77 (the (integer * -990888630255) p2) p3)))
   -990888630272 -19756)
  -990888650105)

(deftest misc.468
  (funcall
   (compile nil '(lambda (p2 p3)
                   (declare (optimize speed (safety 1))
                            (type (integer * 151075404030) p2)
                            (type (integer 6515518 *) p3))
                   (- 12967657127936 (the (eql 151075403520) p2)
                      (the (member 6515658 -14) p3))))
   151075403520 6515658)
  12816575208758)

(deftest misc.469
  (funcall
   (compile nil '(lambda (p2)
                   (declare (optimize speed (safety 1)) (type integer p2))
                   (+ 30926 (the (integer -4025987543018 *) p2))))
   -4025817763840)
  -4025817732914)

(deftest misc.470
  (funcall
   (compile nil '(lambda (p2)
                   (declare (optimize speed (safety 1))
                            (type (integer 3689224658939 *) p2))
                   (+ -1071 (the (integer * 3689229115390) p2))))
   3689228853248)
  3689228852177)

(deftest misc.471
  (funcall
   (compile nil '(lambda (p1 p2)
                   (declare (optimize speed (safety 1))
                            (type (integer -9024844 230253450) p1)
                            (type (eql 35716681856) p2))
                   (* p1 (the (integer * 35716681856) p2))))
   -9024809 35716681856)
  -322336231864165504)

(deftest misc.472
  (funcall
   (compile nil '(lambda (p1 p2)
                   (declare (optimize speed (safety 1))
                            (type (integer -785238 -80) p1)
                            (type (eql -523213622272) p2))
                   (min p1 (the integer p2))))
   -259 -523213622272)
  -523213622272)

(deftest misc.473
  (funcall
   (compile nil '(lambda (p2)
                   (declare (optimize speed (safety 1))
                            (type (integer * 65861934352) p2))
                   (max 23939 (the (integer 64863825609 65878336765) p2))))
   65861912512)
  65861912512)

(deftest misc.474
  (funcall
   (compile nil '(lambda (p1)
                   (declare (optimize speed (safety 1))
                            (type (integer -6750156308) p1))
                   (logand (the signed-byte p1) -540165229)))
   -6750156304)
  -7289140848)

;;; abcl 25 Dec 2005
;;; Debugger invoked on condition of type UNDEFINED-FUNCTION:
;;; The function %FAILED-AVER is undefined.

(deftest misc.475
  (let #+armedbear ((jvm::*catch-errors* nil))
       nil
       (funcall
        (compile nil '(lambda (p1 p2 p3 p4 p6)
                        (declare (optimize speed (safety 1))
                                 (type (integer -785238 61564048) p1)
                                 (type (integer * 65861934352) p2))
                        (+ P1 (THE (INTEGER -485480 -7019) P2) P3 P4
                           463666373060
                           P6)))
        61564048 -7457 24939545512 51 730))
  488667475944)

(deftest misc.476
  (let #+armedbear ((jvm::*catch-errors* nil))
       nil
       (funcall
        (compile nil '(lambda (p4)
                        (declare (optimize speed (safety 1))
                                 (type (integer -115781893486) p4))
                        (- 1 -35 0 (the (integer -115778245122) p4) -2)))
        -115778114900))
  115778114938)

(deftest misc.477
  (let #+armedbear ((jvm::*catch-errors* nil))
       nil
       (funcall
        (compile nil '(lambda (p4 p5)
                        (declare (optimize speed (safety 1))
                                 (type (integer -126908726190 -126906628448) p4)
                                 (type (integer * 2202) p5))
                        (* -1950 -33610502463 2 p4 p5)))
        -126906629040 1839))
  -30591843552678654213361992000)

(deftest misc.478
  (let #+armedbear ((jvm::*catch-errors* nil))
       nil
       (funcall
        (compile nil '(lambda (p2)
                        (declare (optimize speed (safety 1))
                                 (type (integer * 2343679) p2))
                        (logand 12050257282405 p2 117775123 505354693 -415679150084)))
        -6189))
  33816832)

;;; Bug in CMUCL Snapshot 2004-10
;;; Invalid number of arguments: 370632372

(deftest misc.479
  (let ((r (make-array nil :element-type '(unsigned-byte 32)))
        (fn (compile nil '(lambda (r p2)
                            (declare (optimize speed (safety 1))
                                     (type (simple-array (unsigned-byte 32) nil) r)
                                     (type integer p2))
                            (setf (aref r) (logxor 0 (the (integer 2797513123 2798027357) p2)))
                            (values)))))
    (funcall fn r 2797674503)
    (aref r))
  2797674503)

(deftest misc.480
  (let ((r (make-array nil :element-type 'integer))
        (fn (compile nil '(lambda (r p1)
                            (declare (optimize speed (safety 1))
                                     (type (simple-array integer nil) r)
                                     (type (integer -797971 -797511) p1))
                            (setf (aref r) (logeqv p1 15 1078254884158 -12564176924 0 15096591909))
                            (values)))))
    (funcall fn r -797965)
    (aref r))
  -1075415510532)

(deftest misc.481
  (let ((r (make-array nil :element-type '(unsigned-byte 16)))
        (fn (compile nil '(lambda (r p1)
                            (declare (optimize speed (safety 1))
                                     (type (simple-array (unsigned-byte 16) nil) r)
                                     (type (member 4194309 -123 1692 -4432 -760653 -1741 37) p1))
                            (setf (aref r) (logorc1 (the (eql -4432) p1) 0))
                            (values)))))
    (funcall fn r -4432)
    (aref r))
  4431)

;; Various incorrect results

(deftest misc.482
  (let ((r (make-array nil :element-type '(unsigned-byte 4)))
        (fn (compile nil '(lambda (r p2)
                            (declare (optimize speed (safety 1))
                                     (type (simple-array (unsigned-byte 4) nil) r)
                                     (type (eql -4) p2))
                            (setf (aref r) (logorc2 13 p2))
                            (values)))))
    (funcall fn r -4)
    (aref r))
  15)

(deftest misc.483
  (let ((r (make-array nil :element-type '(unsigned-byte 4)))
        (fn (compile nil '(lambda (r p1 p2)
                            (declare (optimize speed (safety 1))
                                     (type (simple-array (unsigned-byte 4) nil) r)
                                     (type (integer * 28306533) p1)
                                     (type (integer * 1245601) p2))
                            (setf (aref r) (logandc1 p1 (the (integer -3308174) p2)))
                            (values)))))
    (funcall fn r -519 -28180)
    (aref r))
  4)

(deftest misc.484
  (let ((r (make-array nil :element-type '(unsigned-byte 4)))
        (fn (compile nil '(lambda (r p2)
                            (declare (optimize speed (safety 1))
                                     (type (simple-array (unsigned-byte 4) nil) r)
                                     (type (member 260646 -348969 34359738370 -110167) p2))
                            (setf (aref r) (logandc2 9 (the (eql -348969) p2)))
                            (values)))))
    (funcall fn r -348969)
    (aref r))
  8)

(deftest misc.485
  (let ((r (make-array nil :element-type 'bit))
        (fn (compile nil '(lambda (r p2)
                            (declare (optimize speed (safety 1))
                                     (type (simple-array bit nil) r)
                                     (type (integer -108220 256178) p2))
                            (setf (aref r) (logand 1 (the (member -1 2147483652 1 -5 3802) p2)))
                            (values)))))
    (funcall fn r -5)
    (aref r))
  1)

(deftest misc.486
  (let ((r (make-array nil :element-type '(unsigned-byte 4)))
        (fn (compile nil '(lambda (r p1 p2)
                            (declare (optimize speed (safety 1))
                                     (type (simple-array (unsigned-byte 4) nil) r)
                                     (type (integer -9) p1)
                                     (type (integer * 1234117) p2))
                            (setf (aref r) (logior (the (integer -295 *) p1) (the (integer -90 *) p2)))
                            (values)))))
    (funcall fn r 6 6)
    (aref r))
  6)

(deftest misc.487
  (let ((r (make-array nil :element-type '(unsigned-byte 16)))
        (fn (compile nil '(lambda (r p1)
                            (declare (optimize speed (safety 1))
                                     (type (simple-array (unsigned-byte 16) nil) r)
                                     (type (integer 1583040351 1587341394) p1))
                            (setf (aref r) (logandc2 (the (integer 1587211196 1587341392) p1) -166174))
                            (values)))))
    (funcall fn r 1587341392)
    (aref r))
  34832)

(deftest misc.488
  (let ((r (make-array nil :element-type '(unsigned-byte 32)))
        (fn (compile nil '(lambda (r p2)
                            (declare (optimize speed (safety 1))
                                     (type (simple-array (unsigned-byte 32) nil) r)
                                     (type (integer 1960409798 1960426181) p2))
                            (setf (aref r) (logorc1 -1 p2))
                            (values)))))
    (funcall fn r 1960409801)
    (aref r))
  1960409801)

(deftest misc.489
  (let ((r (make-array nil :element-type '(unsigned-byte 32)))
        (fn (compile nil '(lambda (r p2)
                            (declare (optimize speed (safety 1))
                                     (type (simple-array (unsigned-byte 32) nil) r)
                                     (type (integer -55) p2))
                            (setf (aref r) (logorc2 0 (the (member -51) p2)))
                            (values)))))
    (funcall fn r -51)
    (aref r))
  50)

(deftest misc.490
  (let ((r (make-array nil :element-type '(unsigned-byte 32)))
        (fn (compile nil '(lambda (r p1)
                            (declare (optimize speed (safety 1))
                                     (type (simple-array (unsigned-byte 32) nil) r)
                                     (type (integer 761639858 1030075825) p1))
                            (setf (aref r) (logior (the (integer * 35389813668) p1) 0))
                            (values)))))
    (funcall fn r 1030075308)
    (aref r))
  1030075308)

(deftest misc.491
  (let ((r (make-array nil :element-type '(signed-byte 16)))
        (fn (compile nil '(lambda (r p2)
                            (declare (optimize speed (safety 1))
                                     (type (simple-array (signed-byte 16) nil) r)
                                     (type (integer 505774114 573717424) p2))
                            (setf (aref r) (lognand 58539 (the (integer * 910674467) p2)))
                            (values)))))
    (funcall fn r 506608551)
    (aref r))
  -8356)

(deftest misc.492
  (let ((r (make-array nil :element-type '(signed-byte 8)))
        (fn (compile nil '(lambda (r p1)
                            (declare (optimize speed (safety 1))
                                     (type (simple-array (signed-byte 8) nil) r) (type (integer * 22050378) p1))
                            (setf (aref r) (lognand (the (integer 19464371) p1) 2257))
                            (values)))))
    (funcall fn r 19469591)
    (aref r))
  -18)

;;; ABCL (25 Dec 2004)
;;; Class verification failed: (class: org/armedbear/lisp/out, method: execute signature: (Lorg/armedbear/lisp/LispObject;Lorg/armedbear/lisp/LispObject;Lorg/armedbear/lisp/LispObject;)Lorg/armedbear/lisp/LispObject;) Expecting to find integer on stack

(deftest misc.493
  (let #+abcl ((jvm::*catch-errors* nil))
       nil
       (funcall
        (compile nil '(lambda (b)
                        (declare (optimize (speed 2) (debug 1) (safety 3)
                                           (compilation-speed 3) (space 1)))
                        (aref #(41397376227 18660605846 49244777443) (min 2 (max 0 b)))))
        
        -71))
  41397376227)

;;; ABCL (26 Dec 2004)
;;;  Class verification failed: [...] Illegal exception table range

(deftest misc.494
  (let #+abcl ((jvm::*catch-errors* nil))
       nil
       (funcall
        (compile nil '(lambda ()
                        (declare (optimize (safety 0) (space 2) (debug 3) (speed 0)
                                           (compilation-speed 2)))
                        (conjugate (progn (catch 'ct5 (if t 0 0)) 0))))))
  0)

;;; The value 5085 is not of type FUNCTION.

(deftest misc.495
  (funcall
   (compile nil '(lambda (a b)
                   (declare (type (integer -4197 284380207) a))
                   (declare (type (integer -23 5088) b))
                   (declare (ignorable a b))
                   (declare (optimize (speed 1) (space 2) (debug 0)
                                      (compilation-speed 0) (safety 2)))
                   (if (position (progn (1+ b) 0)
                                 '(169496 -726 -13623 53307916 128 -258391 156
                                          7432659 30 20 -11))
                       0
                     a)))
   72179019 5084)
  72179019)

;;; Inconsistent stack height 1 != 2

(deftest misc.496
  (let #+abcl ((jvm::*catch-errors* nil))
       nil
       (funcall
        (compile nil
                 '(lambda (a)
                    (declare (type (integer -54915 -3396) a))
                    (declare (optimize (debug 3) (space 0) (safety 2) (speed 2)
                                       (compilation-speed 3)))
                    (progn (1+ a) (catch 'ct6 (progn 0)))))
        -25986))
  0)

(deftest misc.497
  (let #+abcl ((jvm::*catch-errors* nil))
       nil
       (funcall
        (compile nil
                 '(lambda (b)
                    (declare (type (integer -1 0) b))
                    (declare (optimize (space 3) (compilation-speed 1)
                                       (safety 0) (debug 1) (speed 0)))
                    (if 0 (prog2 0 0 (1+ b)) 0)))
        0))
  0)

;;; Inconsistent stack height 1 != 0

(deftest misc.498
  (let #+abcl ((jvm::*catch-errors* nil))
       nil
       (funcall
        (compile nil
                 '(lambda (a)
                    (declare (type (integer -16191 4) a))
                    (declare (optimize (compilation-speed 2) (space 1) (debug 0)
                                       (safety 0) (speed 2)))
                    (conjugate (dotimes (iv1 0 0)
                                 (let ((v2 (dotimes (iv3 0 0) (1+ a))))
                                   0)))))
        -2840))
  0)

;;; Incompatible object argument for function call

(deftest misc.499
  (let #+abcl ((jvm::*catch-errors* nil))
       nil
       (funcall
        (compile nil
                 '(lambda (a b)
                    (declare (type (integer -31415 133871) a))
                    (declare (type (integer -993 6448) b))
                    (declare (ignorable a b))
                    (declare (optimize (space 0) (debug 2) (safety 0) (speed 0)
                                       (compilation-speed 0)))
                    (progn (ceiling (progn (1+ b) a)) a)))
        -16435 2620))
  -16435)

;;; Stack overflow during compilation

(deftest misc.500
  (funcall
   (compile nil '(lambda nil
                   (declare (optimize (space 2) (debug 2) (compilation-speed 2)
                                      (speed 1) (safety 3)))
                   (the integer (integer-length (dotimes (iv4 2 15790955)))))))
  24)

;;; Inconsistent stack height 1 != 0

(deftest misc.501
  (let #+abcl ((jvm::*catch-errors* nil))
       nil
       (funcall
        (compile nil '(lambda (a)
                        (declare (type (integer -437165353 179983908) a))
                        (declare (optimize (compilation-speed 0) (debug 1) (space 1)
                                           (safety 2) (speed 1)))
                        (dotimes (iv1 0 0) (1+ a))))
        1))
  0)

;;; Ordering problems

(deftest misc.502
  (funcall
   (compile nil '(lambda (a)
                   (declare (type (integer -7 84717795) a))
                   (declare (ignorable a))
                   (declare (optimize (speed 1) (space 1) (debug 1) (safety 2)
                                      (compilation-speed 0)))
                   (+ a (setq a 35035201))))
   29207264)
  64242465)

;;; ABCL 27 Dec 2004
;;; Different results

(deftest misc.503
  (funcall
   (compile nil '(lambda (a)
                   (declare (optimize (space 3) (debug 1) (speed 2) (safety 0)
                                      (compilation-speed 1)))
                   (catch 'ct1
                     (throw 'ct1
                            (catch 'ct5
                              (reduce 'min
                                      (vector 0 0 0 a a 0 0 (values 0 0) (throw 'ct5 -6))
                                      :end 8 :start 6 :from-end t))))))
   17)
  -6)

;;; Inconsistent stack height

(deftest misc.504
  (let #+abcl ((jvm::*catch-errors* nil))
       nil
       (funcall
        (compile nil '(lambda (a)
                        (declare (type (integer 196060 241373941) a))
                        (declare (ignorable a))
                        (declare (optimize (speed 3) (debug 0) (safety 2)
                                           (compilation-speed 3) (space 2)))
                        (prog2 (if 0 (+ a a) 0) 0)))
        200000))
  0)

(deftest misc.505
  (let #+abcl ((jvm::*catch-errors* nil))
       nil
       (funcall
        (compile nil '(lambda (a)
                        (declare (type (integer -6 5) a))
                        (declare (optimize (speed 3) (space 0) (safety 2)
                                           (compilation-speed 2) (debug 3)))
                        (dotimes (iv1 0 0) (+ a a))))
        1))
  0)

(deftest misc.506
  (let #+abcl ((jvm::*catch-errors* nil))
       nil
       (funcall
        (compile nil '(lambda (a)
                        (declare (type (integer -53 49) a))
                        (declare (optimize (debug 0) (compilation-speed 1) (space 2)
                                           (safety 0) (speed 0)))
                        (unwind-protect (+ a a) 0)))
        -38))
  -76)

;;; The value 15390 is not of type FUNCTION.
(deftest misc.507
  (funcall
   (compile nil '(lambda (a)
                   (declare (type (integer 2697 13005) a))
                   (declare (optimize (debug 0) (space 2) (speed 2)
                                      (compilation-speed 3) (safety 3)))
                   (truncate (prog1 0 a (+ a a)))))
   7695)
  0 0)

;;; COMPILE-FORM: unsupported special operator LET*
;;; Associated with 'THE' operator

(deftest misc.508
  (let #+abcl ((jvm::*catch-errors* nil))
       nil
       (funcall
        (compile nil '(lambda (a)
                        (declare (type (integer -57853147 -2) a))
                        (declare (ignorable a))
                        (declare (optimize (debug 2) (space 1) (compilation-speed 3)
                                           (safety 1) (speed 2)))
                        (the integer
                          (mask-field (byte 2 29)
                                      (ash (multiple-value-setq (a) -51781613)
                                           (min 1 a))))))
        -29324754))
  1610612736)

(deftest misc.509
  (let #+abcl ((jvm::*catch-errors* nil))
       nil
       (funcall
        (compile nil '(lambda (a)
                        (declare (type (integer -38984312 657) a))
                        (declare (ignorable a))
                        (declare (optimize (debug 1) (compilation-speed 1) (speed 1)
                                           (safety 2) (space 3)))
                        (the integer
                          (if (> a -27907941364)
                              116871
                            (cl:handler-case
                             (multiple-value-setq (a)
                               -34832621))))))
        -26788929))
  116871)

(deftest misc.510
  (let #+abcl ((jvm::*catch-errors* nil))
       nil
       (funcall
        (compile nil '(lambda (a)
                        (declare (type (integer -2827 3400) a))
                        (declare (optimize (compilation-speed 1) (space 3) (debug 1)
                                           (safety 0) (speed 1)))
                        (logand (the integer (dotimes (iv4 2 a) (progn iv4))))))
        155))
  155)

(deftest misc.511
  (let #+abcl ((jvm::*catch-errors* nil))
       nil
       (funcall
        (compile nil '(lambda (a)
                        (declare (type (integer 18967 23584) a))
                        (declare (ignorable a))
                        (declare (optimize (space 1) (speed 1) (debug 1)
                                           (compilation-speed 3) (safety 1)))
                        (the integer
                          (values (loop for lv4 below 2 count (find a '(16389)))))))
        21352))
  0)

;;; Inconsistent stack height

(deftest misc.512
  (let #+abcl ((jvm::*catch-errors* nil))
       nil
       (funcall
        (compile nil '(lambda (a)
                        (declare (type (integer 1 188902468) a))
                        (declare (ignorable a))
                        (declare (optimize (space 2) (speed 3) (safety 3)
                                           (compilation-speed 0) (debug 2)))
                        (catch 'ct6
                          (the integer
                            (let* ((v3 (signum (ignore-errors a))))
                              (declare (dynamic-extent v3))
                              (throw 'ct6
                                     (round (case (prog2
                                                      (lognor 290171664 v3)
                                                      -3512003993
                                                    -550842867)
                                              ((4) (* 1 4092))
                                              ((21 220 225)
                                               (block b1
                                                 (setf v3
                                                       (let* ((v9 v3))
                                                         a))))
                                              (t -639367819)))))))))
        49008586))
  -639367819
  0)

;;; COMPILE-FORM: unsupported special operator LET*
;;; Associated with 'THE' operator

(deftest misc.513
  (let #+abcl ((jvm::*catch-errors* nil))
       nil
       (funcall
        (compile nil '(lambda (a)
                        (declare (type (integer -2 75025568) a))
                        (declare (ignorable a))
                        (declare (optimize (space 0) (compilation-speed 0) (safety 0)
                                           (speed 2) (debug 2)))
                        (let* ((v8
                                (cons (the integer
                                        (prog2 a
                                            -1558460
                                          a
                                          (ignore-errors (progn (tagbody) -49510826))
                                          a))
                                      0)))
                          0)))
        68043554))
  0)

(deftest misc.514
  (let #+abcl ((jvm::*catch-errors* nil))
       nil
       (funcall
        (compile nil '(lambda (a)
                        (declare (type (integer -6844832476 188341751) a))
                        (declare (optimize (speed 3) (debug 1) (safety 0) (space 3)
                                           (compilation-speed 1)))
                        (the integer (multiple-value-setq (a) -96073358))))
        -3792864899))
  -96073358)

;;; gcl 27 Dec 2004
;;; Issue with dynamic extent

(deftest misc.515
  (funcall
   (compile nil '(lambda (a)
                   (declare (type (integer -1337016312 832159784) a))
                   (declare (optimize speed (safety 1)))
                   (let* ((y 0)
                          (v9 0))
                     (declare (dynamic-extent v9))
                     (setq v9 (+ a a))
                     (setq y (1+ v9)))))
   -1209913207)
  -2419826413)

(deftest misc.516
  (funcall
   (compile nil '(lambda ()
                         (declare (optimize (space 0) (debug 0) (safety 2)
                                            (compilation-speed 3) (speed 1)))
                         (let ((*s2* (* -507991378 14)))
                              (declare (dynamic-extent *s2*))
                              (declare (special *s2*))
                              (1+ *s2*)))))
  -7111879291)

;;; gcl 29 Dec 2004
;;; Interference of special variable bindings?

(deftest misc.517
  (funcall
   (compile nil '(lambda ()
                   (declare (optimize (safety 3) (space 3) (debug 1) (speed 1)
                                      (compilation-speed 0)))
                   (let* ((*s8* (let ((*s8* (make-array nil :initial-element 0)))
                                  (declare (special *s8*))
                                  (progn (shiftf (aref *s8*) 31508066) 0))
                                ))
                     (declare (special *s8*))
                     0))))
  0)

;;; Incorrect return value

(deftest misc.518
  (funcall
   (compile nil '(lambda ()
                   (declare (optimize (compilation-speed 0) (safety 1)
                                      (debug 1) (space 0) (speed 3)))
                   (flet ((%f10 (&optional (f10-1 0) (f10-2 0) &key)
                                (progn
                                  (tagbody
                                   (decf f10-2)
                                   (return-from %f10
                                     (complex (unwind-protect (go tag7))
                                              0))
                                   tag7)
                                  f10-2)))
                     (if (evenp (%f10 0 0)) 0 2140390)))))
  2140390)

;;; Error in APPLY [or a callee]: fixnum or bignum expected
;;; Broken at COMPILER::CMP-ANON.

(deftest misc.519
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (compilation-speed 0) (speed 1) (debug 1)
                          (space 1) (safety 3)))
       (let ((*s3*
              (* (the integer
                   (expt (rationalize
                          (multiple-value-bind (*s3*)
                              (make-array nil :initial-element 0)
                            (shiftf (aref *s3*) 0)))
                         2)))))
         1))))
  1)

;;; sbcl 0.8.18 (sparc solaris)
;;;  identity ASH not transformed away

(deftest misc.520
  (funcall
   (compile
    nil
    '(lambda (a c e)
      (declare (type (integer -44330 64753) c))
      (declare (type (integer -301534047 4291509) e))
      (declare (optimize (safety 3) (debug 2) (speed 3)
                (space 2) (compilation-speed 2)))
      (if (oddp
           (ash (logorc2 c e)
                (min 2 (mask-field (byte 0 0) (mod 0 (max 69 0))))))
       a
       0)))
   1 -8156 -229264929)
  0)

;;; ecl (25 Jan 2005)
;;; Error: In a call to AREF, the type of the form *S6* is FIXNUM, not (ARRAY *).

(deftest misc.521
  (funcall
   (compile
    nil
    '(lambda (b)
       (declare (optimize (speed 0) (safety 1) (debug 1) (compilation-speed 3)
                          (space 0)))
       (if b
           (let ((*s6* 0))
             0)
         (let* ((*s6* (make-array nil :initial-element 0)))
           (aref *s6*)))))
   nil)
  0)

;;; nil is not of type number.

(deftest misc.522
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer -25 38) a))
       (declare (optimize (compilation-speed 3) (safety 3) (debug 1) (space 2) (speed 1)))
       (flet ((%f2 (f2-1 f2-2
                         &optional
                         (f2-3 (labels ((%f6 (&optional (f6-1 0) (f6-2 0)) (max a)))
                                 (%f6 0)))
                         (f2-4 0) (f2-5 0))
                   (flet ((%f4 (f4-1 f4-2 f4-3)
                               (flet ((%f15 () f2-3)) a)))
                     0)))
         (reduce #'(lambda (lmv1 lmv6) a)
                 (vector 0 0 0 (%f2 a a) 0 a 0 a 0)
                 :start 4 :from-end t))))
   35)
  35)

;;; Incorrect return value

(deftest misc.523
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer -1011 978) a))
       (declare (optimize (compilation-speed 1) (safety 3) (debug 0) (speed 2) (space 1)))
       (let ((*s5*
              (cons 0
                    (catch 'ct8
                      (ash
                       (flet ((%f15 (f15-1)
                                    (return-from %f15 a)))
                         0)
                       (min 57
                            (lognor (throw 'ct8 (shiftf a 332))
                                    (let ((v1 (setf a 371)))
                                      a))))))))
         a)))
   99)
  332)

;;; Seg fault

(deftest misc.524
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -2432551 871) a))
       (declare (type (integer -6390 -1) b))
       (declare (ignorable b))
       (declare (optimize (compilation-speed 0) (safety 0) (space 2) (speed 0) (debug 3)))
       (flet ((%f18 (f18-1 f18-2 f18-3 &optional &key (key1 0) &allow-other-keys)
                    (labels ((%f12
                              (f12-1
                               &optional (f12-2 0)
                               &key (key1 (catch 'ct7 (conjugate key1))) (key2 0)
                               &allow-other-keys)
                              0))
                      (%f12 a))))
         (%f18 a 0 0))))
   -925293 -1603)
  0)

;;; Internal error: tried to advance stack.

(deftest misc.525
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer -17179869184 -2147483648) a))
       (declare (ignorable a))
       (declare
        (optimize (space 2) (debug 3) (speed 3) (compilation-speed 3) (safety 1)))
       (catch 'ct4
         (max (conjugate (unwind-protect 0 (catch 'ct4 (values 0))))
              (throw 'ct4 0)))))
   -17179869184)
  0)

;;; integer does not specify a sequence type

(deftest misc.526
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer -4 3025867) a))
       (declare (ignorable a))
       (declare
        (optimize (space 1) (safety 0) (debug 0) (speed 3) (compilation-speed 0)))
       (flet ((%f14 (f14-1 f14-2 f14-3 &key)
                    (let ((v4
                           (return-from %f14
                             (flet ((%f11
                                     (&optional (f11-1 0) (f11-2 0)
                                                (f11-3
                                                 (coerce
                                                  (reduce (function (lambda (lmv2 lmv5) a))
                                                          (vector f14-1 f14-1 0 f14-3 a f14-3 a f14-1
                                                                  0 f14-2))
                                                  (quote integer)))
                                                &key (key1 f14-3) (key2 a))
                                     (flet ((%f8
                                             (f8-1
                                              &optional
                                              (f8-2
                                               (flet ((%f16
                                                       (f16-1 f16-2 f16-3
                                                              &optional
                                                              &key (key1 0)
                                                              (key2 f11-3))
                                                       key1))
                                                 0))
                                              &key (key1 0))
                                             f14-3))
                                       0)))
                               (if (%f11 f14-1 (%f11 0 f14-3) f14-1) 0 0)))))
                      0)))
         (%f14 0 a a))))
   857304)
  0)

;;; sbcl 0.8.19.32
;;; Type propagation problem with BIT-AND

(deftest misc.527
  (let ((v1 (make-array 1 :element-type 'bit
                        :initial-contents '(1)
                        :fill-pointer 0))
        (v2 (make-array 1 :element-type 'bit
                        :initial-contents '(1)
                        :fill-pointer 1))
        (r (make-array nil)))
    (funcall
     (compile
      nil
      `(lambda  (r p2)
         (declare (optimize speed (safety 1))
                  (type (simple-array t nil) r)
                  (type (array *) p2))
         (setf (aref r) (bit-and ,v1 (the (bit-vector *) p2)))
         (values)))
     r v2)
    (let ((result (aref r)))
      (values
       (notnot (simple-bit-vector-p result))
       (=t (array-dimension result 0) 1)
       (=t (aref result 0) 1))))
  t t t)

;;;  The value 22717067 is not of type (INTEGER 22717067 22717067)

(deftest misc.528
  (let* ((x 296.3066f0)
         (y 22717067)
         (form `(lambda (r p2)
                  (declare (optimize speed (safety 1))
                           (type (simple-array single-float nil) r)
                           (type (integer -9369756340 22717335) p2))
                  (setf (aref r) (* ,x (the (eql 22717067) p2)))
                  (values)))
         (r (make-array nil :element-type 'single-float))
         (expected (* x y)))
    (funcall (compile nil form) r y)
    (let ((actual (aref r)))
      (unless (eql expected actual)
        (list expected actual))))
  nil)

;;; The value 46790178 is not of type (INTEGER 46790178 46790178).

(deftest misc.529
  (let* ((x -2367.3296f0)
         (y 46790178)
         (form `(lambda (r p2)
                  (declare (optimize speed (safety 1))
                           (type (simple-array single-float nil) r)
                           (type (eql 46790178) p2))
                  (setf (aref r) (+ ,x (the (integer 45893897) p2)))
                  (values)))
         (r (make-array nil :element-type 'single-float))
         (expected (+ x y)))
    (funcall (compile nil form) r y)
    (let ((actual (aref r)))
      (unless (eql expected actual)
        (list expected actual))))
  nil)

;;; cmucl (Jan 2005 snapshot)

;;; Segmentation fault

(deftest misc.530
  (let* ((v (make-array
             '(11) :element-type 'double-float
             :initial-contents
             '(56826.586316245484d0 -57680.53641925701d0 68651.27735979737d0
               30934.627728043164d0 47252.736017400945d0 35129.46986219467d0
               -57804.412938803005d0 13000.374416975968d0 50263.681826551256d0
               89386.08276072948d0 -89508.77479231959d0)))
         (form
          `(lambda (r)
             (declare (optimize speed (safety 1))
                      (type (simple-array t nil) r))
             (setf (aref r)
                   (array-has-fill-pointer-p ,v))))
         (r (make-array nil)))
    (funcall (compile nil form) r)
    (eqlt (aref r) (array-has-fill-pointer-p v)))
  t)

;;; gcl
;;; Problem with 0-dim char arrays
;;; Produces wrong return value (#\\320).

(deftest misc.532
  (let ((r (make-array nil :element-type 'base-char)))
    (funcall
     (compile
      nil
      '(lambda (r c)
         (declare (optimize speed (safety 1))
                  (type (simple-array base-char nil) r)
                  (type base-char c))
         (setf (aref r) c)
         (values)))
     r #\Z)
    (aref r))
  #\Z)

;;; sbcl 0.8.19.32
;;; Bound is not *, a INTEGER or a list of a INTEGER: -51494/29889

(deftest misc.533
  (let* ((r (make-array nil))
         (c #c(208 -51494/29889))
         (form `(lambda (r p1)
                  (declare (optimize speed (safety 1))
                           (type (simple-array t nil) r)
                           (type number p1))
                  (setf (aref r) (+ (the (eql ,c) p1) -319284))
                  (values)))
         (fn (compile nil form)))
    (funcall fn r c)
    (eqlt (aref r) (+ -319284 c)))
  t)

;;; sbcl 0.8.19.35
;;; Incorrect return value from conditional

(deftest misc.534
  (let ((r0 (make-array nil)))
    (funcall
     (compile
      nil
      '(lambda (r p1 p2 p3)
         (declare (optimize speed (safety 1))
                  (type (eql 4134713351/6105637898) p2)
                  (type (eql 2685) p3))
         (setf (aref r)
               (if p1
                   (the (eql 4134713351/6105637898) p2)
                 (the (integer * 8391301) p3)))))
     r0 t 4134713351/6105637898 2685)
    (aref r0))
  4134713351/6105637898)

#|
  The value
    #<SB-ALIEN:CAST
      :%TYPE-CHECK NIL
      :VALUE #<SB-C::LVAR 1 {DEDE809}>
      :ASSERTED-TYPE #<SB-KERNEL:MEMBER-TYPE (MEMBER
                                              #C(4196.088977268509d0 -15943.3603515625d0))>
      :TYPE-TO-CHECK #<SB-KERNEL:NAMED-TYPE *> {DECFF19}>
  is not of type
    SB-C::REF.
|#

(deftest misc.535
  (let ((c0 #c(4196.088977268509d0 -15943.3603515625d0)))
    (funcall
     (compile
      nil
      `(lambda (p1 p2)
         (declare (optimize speed (safety 1))
                  (type (simple-array t nil) r)
                  (type (eql ,c0) p1)
                  (type number p2))
         (eql (the (complex double-float) p1) p2)))
     c0 #c(12 612/979)))
  nil)

;;; Similar to misc.535
(deftest misc.536
  (funcall
   (compile
    nil
    '(lambda (p1 p2)
       (declare (optimize speed (safety 1))
                (type (eql #c(11963908204 1/6)) p1)
                (type (complex rational) p2))
       (eql p1 (the complex p2))))
   #c(11963908204 1/6) #c(2343315619 5252231066))
  nil)

;;; Comparison of bit vectors in compiled code
(deftest misc.537
  (let ((p1 (make-array '(0) :element-type 'bit
                        :adjustable t)))
    (notnot
     (funcall
      (compile
       nil
       `(lambda (p2)
          (declare (optimize speed (safety 1))
                   (type (simple-array t nil) r)
                   (type (simple-bit-vector 0) p2))
          (equal ,p1 (the (bit-vector 0) p2))))
      #*)))
  t)

;;; abcl (23 Feb 2005)
;;;  The value #C(3 4) is not of type number.

(deftest misc.538
  (notnot (typep (* 2/5 #c(3 4)) 'number))
  t)

;;; Allegro CL (6.2 trial edition, x86)
;;; Error: `#c(0 -8)' is not of the expected type `REAL'

(deftest misc.539
  (notnot-mv
   (complexp
    (funcall
     (compile nil '(lambda (x)
                     (declare (OPTIMIZE SPEED (SAFETY 1))
                              (type (eql #c(0 -8)) x))
                     (sqrt x)))
     #c(0 -8))))
  t)

;;; Illegal instruction

(deftest misc.540
  (let* ((d0 #(a b c d e f g h))
         (d1 (make-array 5
                         :fill-pointer 1
                         :displaced-to d0
                         :displaced-index-offset 2)))
    (find #c(1.0 2.0) d1))
  nil)

;;; A crasher bug of REMOVE on non-simple nibble arrays

(deftest misc.541
  (dotimes (i 1000)
    (let* ((init '(12 11 8 8 11 10 9 1 3 9 6 12 4 3 6 4 7 10 12 6 11 12 4 15 8 10 7 0 0
                    0 12 9 6 1 0 14 2 14 6 4 2 2 11 7 13 11 3 9 0 2 3 4 2 11 8 7 9 0 0 3
                    8 3 10 8 2 8 9 4 9 0 11 4 9 8 12 8 5 2 10 10 1 14 7 8 5 5 7 8 1 13 2
                    13 12 2 5 11 1 12 12 0 2 5 15 2 14 2 3 10 1 0 7 7 11 3 7 6 1 13 8 4 2
                    7 14 9 9 7 3 8 1 15 6 11 15 0 11 9 7 15 12 10 6 4 5 6 10 4 4 4 15 5 1
                    8 9 3 12 11 8 4 10 8 3 15 12 3 4 10 8 12 8 14 2 12 12 14 14 5 14 6 10
                    13 9 6 4 14 9 6 8 4 11 1 6 0 7 7 5 4 12 15 7 4 4 10 7 3 0 11 10 11 1
                    8 9 0 12 14 6 2 15 2 5 11 8 3 4 2 9 9 7 0 7 11 13 5 7 12 8 6 12 11 15
                    3 6 11 0 1 2 7 2 13 14 15 4))
          (d0 (make-array
              '(251) :element-type '(integer 0 15)
              :initial-contents init
              :adjustable t)))
      (assert
       (equalp
        (remove 7 d0)
        (coerce (remove 7 init) '(vector (integer 0 15)))))))
  nil)

;;; Object identity for bit vectors

(deftest misc.542
  (funcall
   (compile
    nil
    (let ((bv1 (copy-seq #*1))
          (bv2 (copy-seq #*1)))
      `(lambda () (eq ,bv1 ,bv2)))))
  nil)

;;; Lispworks personal edition 4.3 (x86 linux)
;;; Error: In PLUSP of (#C(1123113 -260528)) arguments should be of type REAL.

(deftest misc.543
  (funcall
   (compile
    nil
    '(lambda (p1)
        (declare (optimize speed (safety 1))
                 ; (type (simple-array t nil) r)
                 (type (integer 2493220 2495515) p1))
        (* p1 #c(1123113 -260528))))
   2493726)
  #C(2800736089038 -649685447328))

;;; gcl

(deftest misc.544
  (let ((n -1.0l0))
    (notnot-mv
     (complexp
      (funcall
       (compile
        nil
        `(lambda (p1)
           (declare (optimize speed (safety 1))
                    (type (long-float ,n 0.0l0) p1))
           (sqrt p1)))
         n))))
  t)

;;; OpenMCL
;;; 1/2 is not of type integer

(deftest misc.545
  (let ((x #c(-1 1/2)))
    (declare (type (eql #c(-1 1/2)) x))
    x)
  #c(-1 1/2))

;;; SBCL
;;; 0.8.19.39
;;; The function SB-KERNEL:CHARACTER-STRING-P is undefined.

(deftest misc.546
  (funcall
   (compile
    nil
    '(lambda (p1)
       (declare (optimize (speed 0) (safety 1) (debug 2) (space 3))
                (type (eql a) p1))
       (typep p1 (type-of ""))))
   'a)
  nil)

;;; The function SB-KERNEL:SIMPLE-CHARACTER-STRING-P is undefined.

(deftest misc.547
  (funcall
   (compile
    nil
    '(lambda (p1)
       (declare (optimize (speed 3) (safety 3) (debug 0) (space 3))
                (type symbol p1))
       (typep (the (eql :c1) p1) (type-of "b"))))
   :c1)
  nil)

;;; The value NIL is not of type SB-KERNEL:CTYPE.

(deftest misc.548
  (notnot
   (funcall
    (compile
     nil
     '(lambda (p1)
        (declare (optimize (speed 2) (safety 1) (debug 3) (space 2)))
        (atom (the (member f assoc-if write-line t w) p1))))
    t))
  t)

;;; IR2 type checking of unused values in [sic] not implemented. 

(deftest misc.549
  (funcall
   (compile
    nil
    '(lambda (p2)
       (declare (optimize (speed 1) (safety 1) (debug 0) (space 3))
                (type symbol p2))
       (and :a (the (eql t) p2))))
   t)
  t)

(deftest misc.550
  (funcall
   (compile
    nil
    '(lambda (p1 p2)
       (declare (optimize (speed 3) (safety 2) (debug 3) (space 3))
                (type atom p1)
                (type symbol p2))
       (or p1 (the (eql t) p2))))
   nil t)
  t)

(deftest misc.551
  (funcall
   (compile
    nil
    '(lambda (p1 p2)
       (declare (optimize (speed 1) (safety 1) (debug 3) (space 3))
                (type symbol p1)
                (type (integer * 55687) p2))
       (funcall (the (eql +) p1) (the (integer -93015310 16215) p2) 2952)))
   '+ 823)
  3775)

(deftest misc.551a
  (funcall
   (compile nil '(lambda (x) (declare (optimize (speed 2))
                                      (type symbol x))
                   (the (eql t) x)))
   t)
  t)

;;; cmucl (mar 2005 snapshot)

(deftest misc.552
  (funcall
   (compile
    nil
    '(lambda (p1)
       (declare (optimize (speed 3) (safety 1) (debug 2) (space 2))
                (type unsigned-byte p1))
       (logbitp (the (integer -780969457 *) p1) 9)))
   26)
  nil)

;;; ecls
;;; REAL is not of type REAL.

(deftest misc.553
  (funcall (compile nil '(lambda (x) (declare (type (eql #c(1.0 2.0)) x)) x))
           #c(1.0 2.0))
  #c(1.0 2.0))

;;; 1 is not of type SEQUENCE
(deftest misc.554
  (funcall (compile nil '(lambda (x) (declare (type (array t 1) x)) x)) #(a))
  #(a))

;;; sbcl 5 Mar 2005
;;; failed AVER: "(EQ CHECK SIMPLE)"

(deftest misc.555
  (notnot
   (funcall
    (compile nil '(lambda (p1)
                    (declare (optimize (speed 1) (safety 2) (debug 2) (space 0))
                             (type keyword p1))
                    (keywordp p1)))
    :c))
  t)

; Problem with FLOOR
; Wrong return value
(deftest misc.556
  (values
   (funcall
    (compile nil '(lambda (p1 p2)
                    (declare
                     (optimize (speed 1) (safety 0)
                               (debug 0) (space 0))
                     (type (member 8174.8604) p1)
                     (type (member -95195347) p2))
                    (floor p1 p2)))
    8174.8604 -95195347))
  -1)

;  invalid number of arguments: 1
; (possible removal of code due to type fumble)
(deftest misc.557
  (values
   (funcall
    (compile
     nil
     '(lambda (p1)
        (declare (optimize (speed 3) (safety 0) (debug 3) (space 1))
                 (type (member -94430.086f0) p1))
        (floor (the single-float p1) 19311235)))
    -94430.086f0))
  -1)

; FFLOOR
; Wrong return value
(deftest misc.558
  (values
   (funcall
    (compile
     nil
     '(lambda (p1)
              (declare (optimize (speed 1) (safety 2)
                                 (debug 2) (space 3))
                       (type (eql -39466.56f0) p1))
              (ffloor p1 305598613)))
    -39466.56f0))
  -1.0f0)

; CEILING
;  invalid number of arguments: 1
(deftest misc.559
  (values
   (funcall
    (compile
     nil
     '(lambda (p1)
        (declare (optimize (speed 1) (safety 1) (debug 1) (space 2))
                 (type (eql -83232.09f0) p1))
        (ceiling p1 -83381228)))
    -83232.09f0))
  1)

; wrong return value
(deftest misc.560
  (values
   (funcall
    (compile
     nil
     '(lambda (p1)
        (declare (optimize (speed 1) (safety 1)
                           (debug 1) (space 0))
                 (type (member -66414.414f0) p1))
        (ceiling p1 -63019173f0)))
    -66414.414f0))
  1)

; FCEILING
; wrong return value
(deftest misc.561
  (values
   (funcall
    (compile
     nil
     '(lambda (p1)
        (declare (optimize (speed 0) (safety 1)
                           (debug 0) (space 1))
                 (type (eql 20851.398f0) p1))
        (fceiling p1 80839863)))
    20851.398f0))
  1.0f0)

;;; LOG
;;; The value #C(-215549 39/40) is not of type (COMPLEX RATIONAL).

(deftest misc.562
  (let ((fn '(lambda (p1)
               (declare (optimize (speed 0) (safety 0) (debug 0) (space 2))
                        (type (complex rational) p1))
               (log p1))))
    (notnot (complexp (funcall (compile nil fn) #C(-215549 39/40)))))
  t)

;;; CONJUGATE
;;; Wrong result (#c(1 2))

(deftest misc.563
  (funcall (compile nil '(lambda (x)
                           (declare (optimize (speed 1) (safety 0) (debug 3) (space 1))
                                    (type (complex rational) x))
                           (conjugate (the (eql #c(1 2)) x))))
           #c(1 2))
  #c(1 -2))

;;; PHASE
;;; The function SB-KERNEL:%ATAN2 is undefined.

(deftest misc.564
  (notnot
   (typep
    (funcall
     (compile
      nil
      '(lambda (p1)
         (declare (optimize (speed 3) (safety 2) (debug 3) (space 0))
                  (type complex p1))
         (phase (the (eql #c(1.0d0 2.0d0)) p1))))
     #c(1.0d0 2.0d0))
    'double-float))
  t)

;;; ACL 6.2 (trial, x86 linux)
;;; Incorrect return value (t instead of nil)

(deftest misc.565
  (funcall
   (compile
    nil
    '(lambda (x)
       (declare (optimize (speed 2) (safety 1) (debug 3) (space 0))
                (type double-float x))
       (not (the (eql 1.0d0) x))))
   1.0d0)
  nil)

;;; ASH
;;; Incorrect value (59 == (ash p1 -3))

(deftest misc.566
  (funcall
   (compile
    nil
    '(lambda (p1)
                 (declare (optimize (speed 3) (safety 2) (debug 2) (space 0))
                          (type (integer 465 127871) p1))
                 (ash p1 -35)))
   477)
  0)

;;; sbcl
;;; The value -4 is not of type (INTEGER -26794287907 505600792).

(deftest misc.567
  (eqlt
   (funcall
    (compile
     nil
     '(lambda (p2)
        (declare (optimize (speed 3) (safety 1) (debug 0) (space 1))
                 (type (integer -26794287907 505600792) p2))
        (scale-float -15193.341216130497d0 (the (integer * 25) p2))))
    -4)
   (scale-float -15193.341216130497d0 -4))
  t)

;;; ACL 7.0 (x86 linux)
;;; Found by random type prop tests

;;; Error: Attempt to divide 13026.059 by zero.
(deftest misc.568
  (values
   (funcall
   (compile nil '(lambda (p2)
                  (declare (optimize (speed 1) (safety 3) (debug 3) (space 1))
                   (type (rational * 5325/3112) p2))
                  (floor 13026.059 (the (member 5325/3112 0 -2316/167 -449/460) p2))))
   5325/3112))
  7612)

;;; Error: Attempt to take the car of #2\%b which is not listp.
(deftest misc.569
  (funcall
   (compile
    nil
    '(lambda (p2)
      (declare (optimize (speed 3) (safety 2) (debug 1) (space 2))
       (type t p2))
      (ash -2609443 (the (integer -3 0) p2))))
   -1)
  -1304722)

;;; Incorrect return value
(deftest misc.570
  (funcall (compile nil '(lambda ()
                          (declare (optimize (speed 3) (safety 1)))
                          (char-equal #\: #\: #\;))))
  nil)

;;; CODE-CHAR returns incorrect result
;;; (ACL7.0, 8 bit character image)

(deftest misc.571
  (and (< 1000 char-code-limit)
       (let ((c1 (code-char 1000))
             (c2
              (funcall (compile nil '(lambda (x)
                                      (declare (optimize speed (safety 1)))
                                      (code-char x)))
                       1000)))
         (if (not (eql c1 c2))
             (list c1 c2)
             nil)))
  nil)

;;; sbcl 0.8.20.19
;;; The value 22 is not of type (MOD 22).

(deftest misc.572
  (funcall
   (compile
    nil
    '(lambda (p4)
       (declare (optimize (speed 1) (safety 2) (debug 1) (space 1))
                (type (integer -59 65558) p4))
       (string<= #.(coerce "1yapt1l7eeenz72u6xqhdfimcyk" 'base-string)
                 #.(coerce "bababababbbabbabbababb" 'base-string)
                 :start2
                 (the (integer -3735 *) p4))))
   22)
  nil)

;;; The value 0 is not of type NIL.

(deftest misc.573
  (funcall
   (compile
    nil
    '(lambda (p4)
       (declare (optimize (speed 2) (safety 1) (debug 2) (space 2))
                (type unsigned-byte p4))
       (string<= (coerce "pdhd5oeynvqlthz3xrrdycotf" 'base-string)
                 (coerce "" 'base-string)
                 :start1 (the (integer * 81) p4))))
   10)
  nil)

;;; incorrect return value

(deftest misc.574
  (funcall
   (compile
    nil
    '(lambda (p4)
       (declare (optimize (speed 3) (safety 1)
                          (debug 1) (space 2))
                (type (integer * 397079023) p4))
             (string<= (coerce "e99mo7yAJ6oU4" 'base-string)
                       (coerce "aaABAAbaa" 'base-string)
                       :start1
                       (the
                           (member -34
                                   131074
                                   67108872
                                   9
                                   -3305367300
                                   335)
                         p4))))
   9)
  9)

;;; In abcl (14 Mar 2005)
;;; The value T is not of type number.

(deftest misc.575
  (equalp #c(1269346.0 47870.12254712875) t)
  nil)

;;; The value #C(435422075/240892576 373) is not of type NUMBER.

(deftest misc.576
  (* -7023900320 #C(435422075/240892576 373))
  #C(-95573789122736375/7527893 -2619914819360))

;;; The value #C(-555014/122849 -6641556271) is not of type NUMBER.

(deftest misc.577
  (/ -3185994774 #C(-555014/122849 -6641556271))
  #C(217230410502882805764/665706755984253572883257634437
     -319343563321640207257301634954/665706755984253572883257634437))

;;; The value "" is not of type (STRING 1).

(deftest misc.578
  (funcall (compile nil '(lambda (p1) (declare (optimize safety)) (the (string 1) p1)))
           (make-array '(1) :element-type 'base-char :initial-element #\x
                       :fill-pointer 0))
  "")

;;; clisp (11 Jan 2005)

;;; *** - SYSTEM::%RPLACA: NIL is not a pair

(deftest misc.579
  (funcall
   (compile nil '(lambda ()
                   (declare (optimize (speed 3) (safety 3) (debug 3) (space 0)))
                   (member 61 '(432445) :allow-other-keys t :foo t))))
  nil)

;;; sbcl 0.8.20.19
;;;  The component type for COMPLEX is not numeric: (OR RATIO FIXNUM)

(deftest misc.580
  (notnot-mv (typep #c(1 2) '(complex (or ratio fixnum))))
  t)

;;; The value -5067.2056 is not of type (SINGLE-FLOAT -5067.2056 -5067.2056).

(deftest misc.581
  (notnot
   (floatp
    (funcall
     (compile nil '(lambda (x)
                     (declare (type (eql -5067.2056) x))
                     (+ 213734822 x)))
     -5067.2056)))
  t)

(deftest misc.581a
  (notnot
   (typep
    (funcall
     (compile nil '(lambda (x) (declare (type (eql -1.0) x))
                     ;;; Note!  #x1000001 is the least positive integer
                     ;;; for which this fails on x86
                     (+ #x1000001 x)))
     -1.0f0)
    'single-float))
  t)

;;; Incorrect result

(deftest misc.582
  (let ((result
         (funcall
          (compile
           nil
           ' (lambda (p1)
               (declare (optimize (speed 0) (safety 1) (debug 1) (space 1))
                        (type (eql -39887.645) p1))
               (mod p1 382352925)))
          -39887.645)))
    (if (plusp result)
        t
      result))
  t)

;;; Argument X is not a REAL: #<FUNCTION {A39DEDD}>

(deftest misc.583
  (notnot-mv
   (complexp
    (funcall
     (compile
      nil
      '(lambda (p1)
         (declare (optimize (speed 0) (safety 0) (debug 2) (space 3))
                  (type (complex rational) p1))
         (sqrt p1)))
     #c(-9003 -121))))
  t)

;;; The value -27 is not of type (INTEGER -34359738403 -24). 

(deftest misc.584
  (approx=
   (funcall
    (compile
     nil
     '(lambda (p1 p2)
        (declare (optimize (speed 1) (safety 1) (debug 0) (space 1))
                 (type (member -3712.8447) p1)
                 (type (integer -34359738403 -24) p2))
        (scale-float p1 p2)))
    -3712.8447 -27)
   (scale-float -3712.8447 -27))
  t)

;;; IR2 type checking of unused values in not implemented.
;;; (note that this test has no THE form)

(deftest misc.585
  (funcall
   (compile nil '(lambda (p1)
                   (declare (optimize (speed 0) (safety 0) (debug 3) (space 3))
                            (type symbol p1))
                   (copy-list p1)))
   nil)
  nil)

;;; The value 4 is not of type (UNSIGNED-BYTE 2).

(deftest misc.586
  (funcall
   (compile
    nil
    '(lambda (p6)
       (declare (optimize (speed 0) (safety 2) (debug 0) (space 0))
                (type (integer -2 3009181) p6))
       (string> (coerce "ababaaabb" 'base-string)
                (coerce "ubbm" 'base-string)
                :start1 2 :start2 p6 :end1 8)))
   4)
  2)

;;; sbcl 0.8.20.27
;;; Control stack exhausted

(deftest misc.587
  (let ((result (funcall
                 (compile
                  nil
                  '(lambda (p2)
                     (declare (optimize (speed 0) (safety 3) (debug 1) (space 0))
                              (type (eql 33558541) p2))
                     (- 92215.266 p2)))
                 33558541)))
    (notnot (typep result 'single-float)))
  t)

;;; Lispworks 4.3 Personal Edition
;;; Incorrect return value (T instead of NIL)

(deftest misc.588
  (funcall (compile nil '(lambda nil (declare (optimize (speed 2) (safety 1) (debug 1) (space 1)))
                           (functionp 3502843))))
  nil)

;;;  (ARRAY NIL) is an illegal type specifier.

(deftest misc.589
  (typep 1 '(array nil))
  nil)

;;; Segmentation violation

(deftest misc.590
  (funcall (compile nil '(lambda nil (declare (optimize debug)) (symbolp -86755))))
  nil)

;;; parse-integer fails on displaced base strings

(deftest misc.591
  (let* ((s1 (coerce "708553218828630100500" 'base-string))
         (s2 (make-array '(13) :element-type 'base-char
                         :displaced-to s1
                         :displaced-index-offset 5)))
    (parse-integer s2))
  3218828630100
  13)

;;; abcl, 19 Mar 2005
;;; Stack overflow

(deftest misc.592
  (equalp #*0 "0")
  nil)

;;; clisp 21 Mar 2005 (-ansi -q, x86 Linux, gcc 3.2.2)
;;; *** - Compiler bug!! Occurred in SP-DEPTH at <0.

(deftest misc.593
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (ignorable a b))
       (declare (optimize (space 3) (debug 0) (safety 1)
                          (compilation-speed 3) (speed 1)))
       (prog2
           (catch 'ct1 (if (or (and t (not (and (and (or a t) nil) nil))) nil)
                           a
                         (reduce #'(lambda (lmv5 lmv2) 0) (vector b 0 a))))
           0)))
   2212755 3154856)
  0)

;;; OpenMCL 0.14.3
;;; 28192897: value doesn't match constraint :U8CONST in template for CCL::MATCH-VREG

(deftest misc.594
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (ignorable a b c))
       (declare (type (integer -1 0) a)
                (type (integer -1065019672 -181184465) b)
                (type (integer 30074 1948824693) c))
       (declare (optimize (safety 2) (compilation-speed 1) (speed 2)
                          (space 0) (debug 0)))
       (ash c (min 82 -28192897))))
   0 -714979492 1474663829)
  0)

;;; ecl 
;;; 10000000.0d0 is not of type INTEGER.

(deftest misc.595
  (floor 1/2 1.0d0)
  0 #.(float 1/2 1.0d0))

;;; sbcl 0.8.21.45 (x86)
;;; The function SB-KERNEL:VECTOR-NIL-P is undefined.

(deftest misc.596
  (notnot
   (let ((s (coerce "a" 'base-string)))
     (funcall
      (compile
       nil
       `(lambda ()
          (declare (optimize (speed 0) (safety 3) (debug 2) (space 1)))
          (typep ,s '(string 1)))))))
  t)


;;; OpenMCL
;;; Incorrect value

(deftest misc.597
  (funcall (compile nil '(lambda (c)
                           (declare (optimize (speed 1) (compilation-speed 2)
                                              (space 1) (debug 1) (safety 2)))
                           (declare (type (integer 1 41) c))
                           (logxor -1 c)))
           8)
  -9)

;;; SBCL 0.9.1.19
;;; Failure of IMAGPART in compiled code

(deftest misc.598
  (funcall (compile nil '(lambda (p1)
                           (declare (optimize (speed 2) (safety 0) (debug 3)
                                              (space 1))
                                    (type short-float p1))
                           (imagpart (the short-float p1))))
           -79916.61s0)
  -0.0s0)

;;; The value 20408096470 is not of type (INTEGER 19856842407 20640917103)

(deftest misc.599
  (funcall
   (compile
    nil
    '(lambda (b)
       (declare (type (integer 19856842407 20640917103) b))
       (declare (optimize (debug 1) (speed 3) (compilation-speed 2)
                          (safety 3) (space 3)))
       (lognand b (deposit-field b (byte 0 0) 3762528061))))
   20408096470)
  -3225589269)

;;; SBCL 0.9.1.21
;;; The function SB-C::SPECIFER-TYPE is undefined.

(deftest misc.600
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (notinline min ash))
       (declare (optimize (speed 0) (debug 1) (safety 1)
                          (space 1) (compilation-speed 3)))
       (logxor (ash 0 (min 90 0)) 0))))
  0)

(deftest misc.601
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (notinline gcd))
       (declare
        (optimize (debug 3) (space 3) (safety 3)
                  (compilation-speed 2) (speed 3)))
       (logeqv 0 (gcd 0)))))
  -1)

;;; Lispworks 4450
;;; Show sporadic bugs in compiled code

(deftest misc.602
  (let ((form '(lambda () (if (oddp (progn (vector) 3747237)) 'a nil))))
    (loop repeat 10 collect (funcall (compile nil form))))
  (a a a a a a a a a a))

;;; gcl 2.7.0 (12 Jul 2005)
;;; Error in WHEN [or a callee]: The GO tag #:G3614 is missing.

(deftest misc.603
  (funcall (compile nil '(lambda () (let ((x (values 0))) 0))))
  0)

;;; gcl 2.7.0 (23 Jul 2005, experimental cvs HEAD)
;;; Error in COMPILER::T1EXPR [or a callee]:
;;;   LOAD-TIME-VALUE is not of type (OR RATIONAL FLOAT).

(deftest misc.604
  (let ((form '(lambda (p1 p2)
                 (declare (optimize (speed 2) (safety 1) (debug 3) (space 3))
                          (type real p1) (type t p2))
                 (eql (the (rational -55253767/37931089) p1) (the atom p2)))))
    (funcall (compile nil form) -55253767/37931089 'a))
  nil)

;;; Error in FUNCALL [or a callee]: LOAD-TIME-VALUE is not of type NUMBER.

(deftest misc.605
  (let ((form '(lambda (p1 p2)
                 (declare (optimize (speed 3) (safety 1) (debug 0) (space 0))
                          (type number p1) (type (float 0.0 3579.314s0) p2))
                 (eql (the real p1) p2))))
    (not (funcall (compile nil form) 3579.314s0 3579.314s0)))
  nil)

;;; Error in COMPILER::CMP-ANON [or a callee]: #\a is not of type FIXNUM.

(deftest misc.606
  (let ((form '(lambda ()
                 (declare (optimize (speed 3) (safety 2) (debug 3) (space 2)))
                 (equal #\a #c(-1775806.0s0 88367.29s0)))))
    (funcall (compile nil form)))
  nil)

;;; Error in COMPILER::CMP-ANON [or a callee]: #*1 is not of type FIXNUM.

(deftest misc.607
  (funcall (compile nil '(lambda ()
                           (declare (optimize (speed 0) (safety 2) (debug 2) (space 2)))
                           (equal #*1 1))))
  nil)

;;; Error in COMPILER::CMP-ANON [or a callee]: #\& is not of type FIXNUM.

(deftest misc.608
  (funcall (compile nil '(lambda (p1)
                           (declare (optimize (speed 3) (safety 2) (debug 3) (space 3))
                                    (type (integer -62603278 -31187) p1))
                           (equal p1 #\&)))
           -31228)
  nil)

;;; Wrong return value (was returning T)

(deftest misc.609
  (funcall (compile nil '(lambda ()
                           (declare (optimize (speed 0) (safety 0) (debug 0) (space 3)))
                           (equalp "b" #*))))
  nil)

;;; Error in COMPILER::CMP-ANON [or a callee]: 7933992 is not of type SYMBOL.

(deftest misc.610
  (not (funcall (compile nil '(lambda (p2)
                                (declare (optimize (speed 1) (safety 1) (debug 3) (space 2))
                                         (type (cons symbol) p2))
                                (typep -32 p2)))
                '(eql -32)))
  nil)

;;; Error in CAR [or a callee]: -757161859 is not of type LIST.

(deftest misc.611
  (funcall (compile nil '(lambda (p1)
                           (declare (optimize (speed 1) (safety 3) (debug 0) (space 2))
                                    (type (cons atom) p1))
                           (car p1)))
           '(48144509 . a))
  48144509)

;;; gcl (09 Aug 2005)
;;; Error in COMPILER::POSSIBLE-EQ-LIST-SEARCH [or a callee]: COMPILER::POSSIBLE-EQ-LIST-SEARCH does not allow the keyword :B.

(deftest misc.612
  (funcall
   (compile nil '(lambda (p1 p2)
                  ((lambda (x y) (typep x (type-of y))) p1
                   (the (member "foo" #\- :b "bar") p2))))
   #*1 :b)
  nil)

;;; Error in APPLY [or a callee]: The tag CT1 is undefined.

(deftest misc.613
  (funcall
   (compile
    nil
    '(lambda (a)
      (declare (optimize (space 3) (safety 1) (debug 3)
		(speed 1) (compilation-speed 3)))
      (catch 'ct1
	(reduce #'(lambda (lmv6 lmv5) (throw 'ct1 0))
		(list a 0 0) :end 2))))
   1)
  0)

;;; Error in MULTIPLE-VALUE-BIND [or a callee]: Cannot get relocated section contents

(deftest misc.614
  (funcall
   (compile
    nil
    '(lambda (a)
      (declare (type (integer -3873004182 -3717314779) a))
      (declare (ignorable a))
      (declare (optimize (debug 0) (safety 1) (speed 3)
		(space 0) (compilation-speed 0)))
      (let* ((v1 (make-array nil :initial-element
			     (reduce #'logand (list a 0 a)))))
	(declare (dynamic-extent v1))
	0)))
   -3755148485)
  0)

;;; gcl type-prop test failures (10/30/2005)

(deftest misc.615
  (let* ((x -8183.7625s0)
	 (form `(lambda (p1) (eql p1 ,x))))
    (not (not (funcall (compile nil form) x))))
  t)

;;; cmucl 19c
;;; Wrong return value

(deftest misc.616
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (type (integer -153105 -36629) a))
       (declare (type (integer -7811721705 3704985368) b))
       (declare (type (integer 0 15) c))
       (declare (ignorable a b c))
       (declare (optimize (safety 1) (space 0) (compilation-speed 0) (speed 3)
			  (debug 3)))
       (catch 'ct7
	 (labels ((%f12
		   (f12-1 f12-2
			  &optional
			  &key (key1 0)
			  (key2
			   (reduce #'(lambda (lmv2 lmv1) 0)
				   (vector 0 0)
				   :end 2
				   :start 0
				   :from-end t))
			  &allow-other-keys)
		   a))
		 c))))
   -134217 -3699719058 10)
  10)

;;; sbcl 0.9.7.33 (x86)
;;; The value 16561216769 is not of type (INTEGER -2147483648 4294967295).
;;; On sparc solaris, the error message is:
;;;   debugger invoked on a SB-KERNEL:CASE-FAILURE:
;;;  16561216769 fell through ETYPECASE expression.
;;;  Wanted one of (SB-C:FIXUP (OR (SIGNED-BYTE 32) (UNSIGNED-BYTE 32))
;;;                            (SIGNED-BYTE 13)).

(deftest misc.617
  (funcall
   (compile
    nil
    '(lambda (b)
       (declare (optimize (space 3) (safety 2) (debug 1) (speed 3)
              (compilation-speed 2)))
       (let* ((v2 16561216769))
     (lognand (loop for lv3 below 0 sum (setf v2 lv3))
          (if (typep v2 '(integer -39 7))
              b
            0)))))
   -10298)
  -1)

;;;  failed AVER: "(EQ POP (CAR END-STACK))"
;;; (same on sparc solaris)

(deftest misc.618
  (funcall
   (compile
    nil
    '(lambda (c)
       (declare (optimize (space 0) (compilation-speed 2) (debug 0)
              (speed 3) (safety 0)))
       (block b1
          (ignore-errors
           (multiple-value-prog1 0
                     (apply (constantly 0)
                        c
                        (catch 'ct2 (return-from b1 0))
                        nil))))))
   -4951)
  0)

;;; sbcl 0.9.7.33 (sparc solaris)
;;; Incorrect return value

(deftest misc.619
  (funcall
   (compile
    nil
    '(lambda (b)
      (declare (type (integer 75 206) b))
      (declare (optimize (speed 0) (compilation-speed 2) (debug 2)
        (space 2) (safety 2)))
      (mask-field (byte 4 28) (ash b 70))))
   79)
  0)

;;;  The value 64 is not of type (OR SB-C:TN (UNSIGNED-BYTE 6) NULL).

(deftest misc.620
  (funcall (compile nil '(lambda ()
                          (declare (optimize (safety 3) (compilation-speed 3)
                                    (debug 1) (space 3) (speed 1)))
                          (loop for lv2 below 1 sum (ash lv2 64)))))
  0)

;;; sbcl 0.9.8.17, x86 linux
;;;   The value 32 is not of type (OR (INTEGER -67 -67) (INTEGER -63 -63)).

(deftest misc.621
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (debug 1) (space 0) (compilation-speed 3)
			  (speed 1) (safety 3)))
       (loop for lv1 below 2 sum
	     (dotimes (iv2 2 0)
	       (mod (dotimes (iv4 2 0) (progn (count lv1 #*0) 0))
		    (min -63 (rem 0 (min -67 0)))))))))
  
  0)

;;; sbcl 0.9.9.8, x86 linux
;;; TYPE-ERROR: The value 17549.955 is not of type REAL.

(deftest misc.622
  (funcall
   (compile
    nil
    '(lambda (p2)
       (declare (optimize (speed 3) (safety 2) (debug 3) (space 0))
		(type real p2))
       (+ 81535869 (the (member 17549.955 #:g35917) p2))))
   17549.955)
  #.(+ 81535869 17549.955))

;;; sbcl 0.9.9.19
;;;   The function SB-VM::%LOGBITP is undefined.

(deftest misc.623
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (space 2) (speed 0) (debug 2)
			  (compilation-speed 3) (safety 0)))
       (loop for lv3 below 1
	     count (minusp
		    (loop for lv2 below 2
                        count (logbitp 0
				       (bit #*1001101001001
					    (min 12 (max 0 lv3))))))))))
  0)

;;;     failed AVER: "(< Y 29)"

(deftest misc.624
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer 21 28) a))
       (declare	(optimize (compilation-speed 1) (safety 2)
			  (speed 0) (debug 0) (space 1)))
       (let* ((v7 (flet ((%f3 (f3-1 f3-2)
			      (loop for lv2 below 1
				    count
				    (logbitp 29
					     (sbit #*10101111
						   (min 7 (max 0 (eval '0))))))))
                     (%f3 0 a))))
	 0)))
   22)
  0)

;;; sbcl 0.9.9.22 (x86 linux)

;;; The following two errors appear to require the presence
;;; of two ELT forms.  Somehow, the type check for one is
;;; misplaced into the other.

;;; TYPE-ERROR: The value 0 is not of type (INTEGER 3 3).

(deftest misc.625
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer -2 -1) a))
       (declare (optimize (speed 0) (space 0) (safety 1)
                      #+sbcl (sb-c:insert-step-conditions 0)
                      (debug 3) (compilation-speed 1)))
       (elt '(47119 39679 57498 35248 23784 40597 53473 29454)
	    (min 7
		 (max 0
		      (flet ((%f7
			      (f7-1 f7-2
				    &optional
				    &key
				    (key1
				     (elt '(0 25 30 12 27 5)
					  (min 5 (max 0 3)))))
			      0))
                            (flet ((%f6
				    (&optional
				     &key (key1 (progn (%f7 0 a) a))
				     (key2 0))
				    0))
				  (%f7 a a))))))))
   -2)
  47119)

;;; TYPE-ERROR: The value 2 is not of type (INTEGER 12 12)

(deftest misc.625a
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer 1 5) b))
       (declare (optimize (safety 2) (speed 2)
			  (space 0) (compilation-speed 3) (debug 3)))
       (progn
	 (flet ((%f3
		 (f3-1 f3-2 &optional (f3-3 b) f3-4
		       (f3-5 (prog1 0 (elt '(a b c d e f g h i j k l m) 12))))
		 f3-1))
	       (%f3 0 (%f3 0 a 0 a) a 0 a))
	 (elt '(a b c d) (min 3 b))
	 )))
   0 2)
  c)

;;; failed AVER: "(<= Y 29)"

(deftest misc.626
  (funcall
   (compile
    nil
    '(lambda (a)
       (declare (type (integer -902970 2) a))
       (declare (optimize (space 2) (debug 0) (compilation-speed 1)
			  (speed 0) (safety 3)))
       (prog2 (if (logbitp 30 a) 0 (block b3 0)) a)))
   -829253)
  -829253)

;;; The value -93368855 is not of type UNSIGNED-BYTE.
;;; [...]
;;; (LOGBITP -93368855 0)

(deftest misc.628
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (safety 3) (space 3) (compilation-speed 3)
			  (speed 0) (debug 1)))
       (not (not (logbitp 0 (floor 2147483651 (min -23 0))))))))
  t)

;;; sbcl 0.9.9.35
;;; The value #S(MISC-629 :A 1 :B 3) is not of type SB-KERNEL:INSTANCE.

(defstruct misc-629 a b)

(deftest misc.629
  (let* ((s (make-misc-629 :a 1 :b 3))
	 (form  `(lambda (x)
		   (declare (optimize (speed 1) (safety 3) (debug 0) (space 2))
			    (type (member 0 2 ,s) x))
		   (misc-629-a x))))
    (funcall (compile nil form) s))
  1)

;;; sbcl 0.9.10.11
;;; Failures associated with MULTIPLE-VALUE-PROG1
;;; Argument X is not a NUMBER: NIL
;;; (SB-KERNEL:TWO-ARG-/ NIL 1)

(deftest misc.630
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize (speed 1) (debug 0)
			  (space 2) (safety 0) (compilation-speed 0)))
       (unwind-protect 0
	 (* (/ (multiple-value-prog1 -29457482 -5602513511) 1))))))
  0)

;;; Argument X is not a INTEGER: NIL
;;; (SB-KERNEL:TWO-ARG-AND NIL 1)

(deftest misc.631
  (if (flet ((%f17 (&key (key2 (if (evenp (multiple-value-prog1 0)) 0 0)))
		   0))
	    0)
      :a :b)
  :a)


;;; gcl 2.7.0 (7 Mar 2006)
;;; Wrong value -- NIL

(deftest misc.632
  (funcall (compile nil '(lambda () (let (b) (multiple-value-setq (b) 10)))))
  10)

;;; sbcl (x86 linux) 0.9.10.43
;;;  The value -17045.0
;;;  is not of type
;;;    (OR (MEMBER #:|u4m7k0jz6o| 1+)
;;;        (MEMBER #\b)
;;;        (SINGLE-FLOAT -17045.0 -17045.0)).

(deftest misc.633
  (let* ((x -17045.0)
	 (form `(lambda (p3 p4)
		  (declare (optimize (speed 1) (safety 3) (debug 0) (space 1))
			   (type number p3)
			   (type (member -1451.1257 47889 #:|3| ,x #:|aabbaaaaaababa|)
				 p4))
		  (min 1
		       -251.2455
		       (the number p3)
		       (the (member 1+ ,x #\b #:|u4m7k0jz6o|) p4)
		       -1506/1283
		       65681158/19740963))))
    (funcall (compile nil form) 1861 x))
  -17045.0)

;;; sbcl (x86 linux) 0.9.10.48
;;;  The value 35182846 is not of type (INTEGER 35182846 35182846).

(deftest misc.634
  (let ((form '(lambda (p2)
		 (declare (optimize (speed 0) (safety 3) (debug 3) (space 2))
			  (type number p2))
		 (- -83659.0 (the (member 35182846) p2)))))
    (funcall (compile nil form) 35182846))
  #.(- -83659.0 35182846))

;;; sbcl (x86 linux) 0.9.11.4
;;; Different results

(deftest misc.635
  (let* ((form '(lambda (p2)
		 (declare (optimize (speed 0) (safety 1)
				    (debug 2) (space 2))
			  (type (member -19261719) p2))
		 (ceiling -46022.094 p2))))
    (values (funcall (compile nil form) -19261719)))
  1)

;;; TYPE-ERROR: The value 26899.875 is not of type NUMBER.

(deftest misc.636
  (let* ((x 26899.875)
	 (form `(lambda (p2)
		  (declare (optimize (speed 3) (safety 1) (debug 3) (space 1))
			   (type (member ,x #:g5437 char-code #:g5438) p2))
		  (* 104102267 p2))))
    (not (not (floatp (funcall (compile nil form) x)))))
  t)

;;;   attempt to THROW to a tag that does not exist: SB-C::LOCALL-ALREADY-LET-CONVERTED

(deftest misc.637
 (labels ((%f11 (f11-2 &key key1)
		(labels ((%f8 (f8-2 &optional (f8-5 (if nil (return-from %f11 0) 0)))
			      :bad1))
			(%f8 (%f8 0)))
		:bad2))
	 :good)
  :good)

;;;  full call to SB-KERNEL:DATA-VECTOR-REF

(deftest misc.638
  (let* ((codes '(32779 60674 33150 60033 41146 23916 28908 58886 12776 21282 37346 25537 56184
			40736 4845 41954 6663 44378 23466 46903 13661 36445 18784 6114 6266))
	 (chars (loop for code in codes collect (or (code-char code) #\x)))
	 (c (elt chars 21))
	 (s (make-array '(25) :element-type 'character
			:initial-contents chars)))
    (let ((form `(lambda (p1)
		   (declare (optimize (speed 2) (safety 0) (debug 3) (space 1))
			    (type (simple-string 25) p1))
		   (char
		    (the
		     (member ,(let ((s2 "abbbabbaaabbaba"))
				(make-array (length s2) :element-type 'base-char
					    :initial-contents s2))
			     ,s)
		     p1)
		    21))))
      (not (not (eql c (funcall (compile nil form) s))))))
  t)

;;; sbcl 0.9.11.24 (x86 linux)
;;; failed AVER: "(EQ PHYSENV (LAMBDA-PHYSENV (LAMBDA-VAR-HOME THING)))"

(deftest misc.639
  (let ((form '(lambda (a b d)
		 (declare (notinline >= eql))
		 (declare (optimize (debug 2) (speed 3) (safety 0)
				    (compilation-speed 3) (space 0)))
		 (labels ((%f8 (f8-1 &optional (f8-4 (if (if (eql 0 -16) (>= d) nil) 0 0)))
			       a))
			 (%f8 b)))))
    (funcall (compile nil form) :good 18 0))
  :good)

;;; sbcl 0.9.11.45 (x86 linux)
;;; Incorrect value: -32377322164

(deftest misc.640
  (let ((form '(lambda (b g)
		 (declare (type (integer 303184 791836) b))
		 (declare (optimize (compilation-speed 2) (debug 0) (space 1)
				    (speed 1) (safety 2)))
		 (loop for lv1 below 2
		       sum (if (<= g lv1)
			       (labels ((%f7 () (prog1 b 0))) (%f7))
			     (setf g -16188661082))))))
    (funcall (compile nil form) 335562 4655131896))
  -16188325520)

;;; sbcl 0.9.12.27 (x86 linux)
;;;   The value NIL is not of type SB-C::IR2-NLX-INFO.

(deftest misc.641
  (let ((form '(lambda ()
		 (declare (optimize (speed 1) (space 0) (debug 2)
				    (compilation-speed 0) (safety 1)))
		 (flet ((%f3 (f3-1 &key (key1 (count (floor 0 (min -74 0)) #())))
			     0))
		       (apply #'%f3 0 nil)))))
    (funcall (compile nil form)))
  0)

;;; cmucl 19c (x86 linux)
;;; The assertion (NOT (MEMBER C::KIND '(:DELETED :OPTIONAL))) failed.

(deftest misc.642
  (let ((form '   (lambda (a b c d e f g h i j)
		    (declare (type (integer 174130 60165950) a))
		    (declare (type (integer -4076 6783) b))
		    (declare (type (integer -178481569 -1) c))
		    (declare (type (integer 236 954963169) d))
		    (declare (type (integer -1334 407047) e))
		    (declare (type (integer -507 -426) f))
		    (declare (type (integer -1164301 148213922) g))
		    (declare (type (integer -184324 14515) h))
		    (declare (type (integer 258 323) i))
		    (declare (type (integer -11825 109247) j))
		    (declare (ignorable a b c d e f g h i j))
		    #+cmu (declare (optimize (extensions:inhibit-warnings 3)))
		    (declare (optimize (compilation-speed 2) (debug 0) (space 1) (speed 3)
				       (safety 2)))
		    (labels ((%f4 (f4-1)
				  (flet ((%f2 (f2-1 f2-2 f2-3 &key)
					      (progn
						(return-from %f4 0)
						f2-2)))
					(common-lisp:handler-bind nil
					  (/
					   (coerce
					    (unwind-protect
						(reduce
						 #'(lambda (lmv2 lmv4)
						     (reduce #'*
							     (vector
							      (let ()
								h)
							      c
							      (reduce
							       #'(lambda (lmv4 lmv3)
								   (return-from %f4
										(deposit-field lmv4
											       (byte 23 16)
											       (mask-field
												(byte 3 27)
												(elt '(5309746)
												     (min 0
													  (max 0
													       j)))))))
							       (vector
								(%f2 (%f2 12762 f4-1 6646240924) 1501
								     -15)
								277
								(multiple-value-call #'%f2
										     (values -1486981
											     i
											     (%f2 a 16777222 j)))
								1033)
							       :end 4
							       :start 3)
							      (/ 823 -1))
							     :end 3
							     :start 1))
						 (vector
						  (common-lisp:handler-bind nil
									    (- 0 h j b -2539837 28596 d 8161548 h -61))
						  -183768642
						  -1
						  31404552
						  81593)
						 :start 3)
					      (dpb i (byte 14 16) e)
					      (dpb
					       (count f4-1
						      #(524279 8388596 1021351 101986)
						      :test '/=)
					       (byte 4 4)
					       131064)
					      (if (= 524287 f)
						  (prog2
						      (denominator
						       (elt '(1663 120) (min 1 (max 0 -17745))))
						      f
						    (deposit-field e (byte 31 31) 0)
						    (labels ((%f7
							      (f7-1 f7-2 f7-3
								    &optional
								    (f7-4
								     (coerce
								      (coerce
								       (the integer (+ -11045 114))
								       'integer)
								      'integer))
								    (f7-5 h))
							      -2286515))
							    j))
						(macrolet ()
							  (prog2 -2195 1921675 h -183085 a))))
					    'integer)
					   1)))))
			    0))))
    (funcall (compile nil form) 58162926 -3652 -63561386 935157597 63716 -504 108893677 -146677 308 99009))
  0)

;;; Wrong return value

(deftest misc.643
  (let ((form '(lambda (a)
		 (declare (type (integer 6 1273) a))
		 (declare (optimize (space 0) (safety 0) (debug 3) (compilation-speed 2) (speed 3)))
		 (logorc2 0 (restart-bind nil (shiftf a 522))))))
    (funcall (compile nil form) 807))
  -808)

;;;   -1520586839 is not of type INTEGER

(deftest misc.644
  (let ((form '(lambda (a)
		 (declare (type (integer -6568333536 -12667) a))
		 (declare (ignorable a))
		 #+cmu (declare (optimize (extensions:inhibit-warnings 3)))
		 (declare (optimize (compilation-speed 1) (safety 3) (speed 1) (debug 1)
				    (space 3)))
		 (unwind-protect 0
		   (the integer
			(locally
			 (declare (special *s3* *s4*))
			 (progv '(*s4* *s3*) (list a a) (expt *s3* 0))))))))
    (let ((*s3* 0))
      (declare (special *s3*))
      (funcall (compile nil form) -1520586839)))
  0)

;;;    NIL is not of type C::CBLOCK

(deftest misc.645
  (let ((form '(lambda (a)
		 (declare (notinline abs isqrt))
		 #+cmu (declare (optimize (extensions:inhibit-warnings 3)))
		 (declare (optimize (debug 3) (safety 1) (space 2) (compilation-speed 1)
				    (speed 0)))
		 (progn
		   (tagbody
		    (prog2
			a
			0
		      (labels ((%f9 (&key &allow-other-keys)
				    (go 3)))
			      (%f9)))
		    (isqrt (abs (unwind-protect 0)))
		    3)
		   a))))
    (eval `(,form 0)))
  0)

;;; Segmentation violation

(deftest misc.646
  (let ((form '(lambda (a)
		 (declare (type (integer -125 -44) a))
		 (declare (ignorable a))
		 #+cmu (declare (optimize (extensions:inhibit-warnings 3)))
		 (declare (optimize (speed 0) (debug 0) (space 2) (compilation-speed 3)
				    (safety 3)))
		 (mask-field (byte 0 0)
			     (block b3 (isqrt (abs (catch 'ct2 (return-from b3 0)))))))))
    (funcall (compile nil form) -50))
  0)

;;; 1928431123 is not of type (MOD 536870911)

(deftest misc.647
  (let ((form '(lambda (a)
		 (declare (type (integer -2494 534) a))
		 (declare (ignorable a))
		 #+cmu (declare (optimize (extensions:inhibit-warnings 3)))
		 (declare (optimize (speed 0) (space 0) (compilation-speed 3) (safety 1)
				    (debug 1)))
		 (dotimes (iv3 1 0)
		   (block b1
			  (loop for lv1 below 1
				count (logbitp 0
					       (reduce
						#'(lambda (lmv6 lmv2)
						    (if (> 2208446653 lmv6)
							(return-from b1 lmv2)
						      lv1))
						(list 0 0 0 1928431123 iv3 iv3 a a)
						:end 5
						:from-end t))))))))
    (funcall (compile nil form) 1))
  0)

;;;  The assertion (AND C::SUCC (NULL (CDR C::SUCC))) failed.

(deftest misc.648
  (let ((form '(lambda (a)
		 (declare (type (integer -8 11754838336) a))
		 (declare (ignorable a))
		 #+cmu (declare (optimize (extensions:inhibit-warnings 3)))
		 (declare (optimize (space 0) (compilation-speed 0) (speed 3) (debug 3)
				    (safety 0)))
		 (labels ((%f13 ()
				(logorc1 (unwind-protect 0)
					 (prog1 0
					   (prog2
					       (max 0 a)
					       0
					     (progn
					       (return-from %f13 a)
					       a))))))
			 0))))
    (funcall (compile nil form) 2582756596))
  0)

;;; sbcl 0.9.13.8 (x86 linux)
;;; VALUES type illegal in this context:  *

(deftest misc.649
  (let ((form '(lambda (p2)
                 (declare (optimize (speed 0) (safety 0) (debug 2) (space 2))
                          (type (member integer *) p2))
                 (coerce 523242 p2))))
    (funcall (compile nil form) 'integer))
  523242)

;;; The symbol AND is not valid as a type specifier

(deftest misc.650
  (let ((form '(lambda (p2)
                 (declare (optimize (speed 0) (safety 2) (debug 0) (space 2))
                          (type (member integer and) p2))
                 (coerce -12 p2))))
    (funcall (compile nil form) 'integer))
  -12)

;;; The symbol OR is not valid as a type specifier

(deftest misc.651
  (let ((form '(lambda (p2)
                 (declare (optimize (speed 0) (safety 2) (debug 0) (space 2))
                          (type (member integer or) p2))
                 (coerce 1 p2))))
    (funcall (compile nil form) 'integer))
  1)

;;; The symbol NOT is not valid as a type specifier.

(deftest misc.652
  (let ((form '(lambda (p2)
                 (declare (optimize (speed 0) (safety 2) (debug 0) (space 2))
                          (type (member integer not) p2))
                 (coerce 2 p2))))
    (funcall (compile nil form) 'integer))
  2)

;;;   The symbol SATISFIES is not valid as a type specifier.

(deftest misc.653
  (let ((form '(lambda (p2)
                 (declare (optimize (speed 0) (safety 2) (debug 0) (space 2))
                          (type (member integer satisfies) p2))
                 (coerce 2 p2))))
    (funcall (compile nil form) 'integer))
  2)

;;;  error while parsing arguments to DEFTYPE EQL:
;;;    invalid number of elements in
;;;      ()
;;;    to satisfy lambda list
;;;      (SB-KERNEL::N):
;;;    exactly 1 expected, but 0 found

(deftest misc.654
  (let ((form '(lambda (p2)
                 (declare (optimize (speed 0) (safety 2) (debug 0) (space 2))
                          (type (member integer eql) p2))
                 (coerce 2 p2))))
    (funcall (compile nil form) 'integer))
  2)

;;;  The symbol MEMBER is not valid as a type specifier.

(deftest misc.655
  (let ((form '(lambda (p2)
                 (declare (optimize (speed 0) (safety 2) (debug 0) (space 2))
                          (type (member integer member) p2))
                 (coerce 2 p2))))
    (funcall (compile nil form) 'integer))
  2)

;;;  error while parsing arguments to DEFTYPE MOD:
;;;    invalid number of elements in
;;;      ()
;;;    to satisfy lambda list
;;;      (SB-KERNEL::N):
;;;    exactly 1 expected, but 0 found

(deftest misc.656
  (let ((form '(lambda (p2)
                 (declare (optimize (speed 0) (safety 2) (debug 0) (space 2))
                          (type (member integer mod) p2))
                 (coerce 2 p2))))
    (funcall (compile nil form) 'integer))
  2)

;;; The symbol VALUES is not valid as a type specifier.

(deftest misc.657
  (let ((form '(lambda (p2)
                 (declare (optimize (speed 0) (safety 2) (debug 0) (space 2))
                          (type (member integer values) p2))
                 (coerce 2 p2))))
    (funcall (compile nil form) 'integer))
  2)

