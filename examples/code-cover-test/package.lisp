;; -*- Mode:Lisp; tab-width:2; indent-tabs-mode:nil -*-

(defpackage code-cover-test
  (:use #:cl)
  (:import-from #:cl-ppcre-test "PERL-TEST" "TEST-OPTIMIZED-TEST-FUNCTIONS" "SIMPLE-TESTS")
  (:export
   "INIT-CODE-COVERAGE"
   "RUN-ALL-TESTS-WITH-CODE-COVERAGE"
   "REPORT-CODE-COVERAGE-TEST"
   "INIT-CODE-COVERAGE-TEST-SERVER" "START-CODE-COVERAGE-TEST-SERVER" "STOP-CODE-COVERAGE-TEST-SERVER")
  )

