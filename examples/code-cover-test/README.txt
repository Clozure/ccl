;; -*- Mode:Text; tab-width:2; indent-tabs-mode:nil -*-

code-cover-test - CCL code coverage test example

COMPILE A CL SYSTEM WITH CODE COVERAGE

In Common Lisp, a system is a code library or application declared
using DEFSYSTEM and containing one or more Lisp source files.
Ideally, each system has its own unit tests.

1. Load the code coverage test system

   (load (translate-logical-pathname "ccl:examples;code-cover-test;code-cover-test.asd"))
   (ql:quickload :code-cover-test)
   (ql:quickload :code-cover-tests)

2. Configure one or more Common Lisp systems to test with code coverage

   See code-cover-test/cl-ppcre-tests.lisp for an example that defines
   methods to run CL-PPCRE unit tests with code coverage analysis
   enabled.

3. Compile and run tests with code coverage 

   (in-package :code-cover-test)

   (do-tests (make-instance 'cl-ppcre-tests))

   To (re)compile and (re)initialize code coverage only (without running tests)

   (init-code-coverage (make-instance 'cl-ppcre-tests))

GENERATE REPORT ON CODE COVERAGE

1. Specify output directory - default is ~/tmp/code-cover-test

   (in-package :code-cover-test)

   (setq *output-directory-path* #P"~/tmp/code-cover-test/")

2. Generate code coverage report output files

   (report-code-coverage-test)

VIEW CODE COVERAGE REPORTS

Use a Web browser to view the generated page "html/index.html".

With some Web browsers, viewing the files using local file URLs
(without getting from on a Web server) will not show the results in
frames and the Javascript UI won't work properly. The recommended
approach is to serve the code coverage results via Web server.

Following are instructions to view code coverage results via Hunchentoot.

1. Load the code coverage test server

   (load (translate-logical-pathname "ccl:examples;code-cover-test;code-cover-test-server.asd"))
   (ql:quickload :code-cover-test-server)

2. Set the host and port as needed. These default to "localhost" and 9090, respectively.

   (in-package :code-cover-test-server)

   (setq *server-port* 9090. *server-host* "localhost")

3. Start the server        

   (in-package :code-cover-test-server)

   (start-server)

   TODO: Make it easy to load code coverage server in a separate image to avoid
   skewing results. (?)

4. View results in Web browser with appropriate URL. For example:

   http://localhost:9090/code-cover-test

5. To stop the server:

   (in-package :code-cover-test-server)

   (stop-server)

   Restart as needed:

   (start-server)
