;;;; Smoke: NSOffState / NSControlStateValue* are constants, not dyld vars.
;;;;
;;;;   ./darm64cl --no-init --batch < tools/darwin-nsoffstate-smoke.lisp
(in-package :ccl)

(setq *warn-if-redefine-kernel* nil)
(load (merge-pathnames "library/parse-ffi.lisp" (ccl-directory))
      :verbose nil)

(assert (ffi-var-static-p
         '(:var ("" 0) "NSOffState" (:typedef "NSControlStateValue") (:static)))
        () "keyword-package (:static) not detected")
(assert (ffi-var-static-p
         '(var ("" 0) "NSOffState" (typedef "NSControlStateValue") (static)))
        () "raw (static) not detected")
(assert (not (ffi-var-static-p
              '(:var ("" 0) "NSControlTintDidChangeNotification"
                (:typedef "NSNotificationName") (:extern))))
        () "(:extern) treated as static")
(assert (null (process-ffi-var
               '(:var ("" 0) "NSOffState" (:typedef "NSControlStateValue") (:static))))
        () "static var was not skipped")

(use-interface-dir :cocoa)
(assert (eql #$NSOffState 0))
(assert (eql #$NSOnState 1))
(assert (eql #$NSMixedState -1))
(assert (eql #$NSControlStateValueOff 0))
(assert (eql #$NSControlStateValueOn 1))
(assert (eql #$NSControlStateValueMixed -1))
(format t "~&#$NSOffState=~s #$NSControlStateValueOff=~s #$NSMixedState=~s~%"
        #$NSOffState #$NSControlStateValueOff #$NSMixedState)

(format t "~&DARWIN-NSOFFSTATE-SMOKE-OK~%")
(quit 0)
