;; -*- Mode:Text; tab-width:2; indent-tabs-mode:nil -*-

code-cover-test - CCL code coverage test example

To load use QuickLisp and/or ASDF, for example:

   (ql:quickload :code-cover-test)

COMPILE A CL SYSTEM WITH CODE COVERAGE

1. Load the code coverage test system

   (ql:quickload "code-cover-test")
   (in-package :code-cover-test)

2. Define CL systems to test. Ideally the system has a unit test driver.

   Default is CL-PPCRE-TEST.

   TODO: parameterize this - currently the CL-PPCRE systems are hard-coded.

     * Define methods to specify systems and forms
     * Remove CL-PPCRE dependency from package CODE-COVER-TEST

3. Compile and run tests with code coverage 

   (run-all-tests-with-code-coverage :compile-p t)

   To (re)compile and (re)initialize code coverage only (not run tests)

   (init-code-coverage :compile-p t)

GENERATE REPORT ON CODE COVERAGE

1. Specify output directory - default is ~/tmp/code-cover-test

   (setq *output-directory-path* #P"~/tmp/code-cover-test/")

2. Generate code coverage report output files

   (report-code-coverage-test)

VIEW CODE COVERAGE REPORTS

Install the code coverage results into a Web server and view the generated page
"index.html".  With some Web browsers, viewing the files using FILE URLs
(without getting from on a Web server) will not serve the results in frames and
Javascript UI won't work properly.

Following are instructions to view code coverage results via Hunchentoot.

1. Load the code coverage test server

   (ql:quickload "code-cover-test")
   (in-package :code-cover-test)

2. Set the host and port as needed. These default to "localhost" and 9090, respectively.

   (setq *server-port* 9090. *server-host* "localhost")

3. Start the server        

   (start-code-coverage-test-server)

   TODO: Make it easy to load code coverage server in a separate image to avoid
   skewing results. (?)

4. View results in Web browser with appropriate URL. For example:

   http://localhost:9090/code-cover-test

5. To stop the server:

   (stop-code-coverage-test-server)

   Restart as needed:

   (start-code-coverage-test-server)
