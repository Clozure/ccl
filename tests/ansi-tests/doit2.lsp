#+allegro
(progn
  (rt:disable-note :nil-vectors-are-strings)
  (rt:disable-note :standardized-package-nicknames)
  (rt:disable-note :type-of/strict-builtins)
  (rt:disable-note :assume-no-simple-streams)
  (rt:disable-note :assume-no-gray-streams))

#+lispworks
(progn
  (rtest:disable-note :allow-nil-arrays)
  (rtest:disable-note :nil-vectors-are-strings))

(in-package :cl-test)

;;; These two tests will misbehave if the tests are being
;;; invoked from a file that is being loaded, so remove them
(when *load-pathname*
  (mapc #'regression-test:rem-test '(load-pathname.1 load-truename.1)))

(time (regression-test:do-tests))

#+allegro (cl-user::exit)
#+(or cmu sbcl gcl armedbear) (cl-user::quit)
