# Clozure CL

[Clozure CL](http://ccl.clozure.com/docs/ccl.html) is a fast, mature, open source Common Lisp implementation that runs on Linux, Mac OS X, FreeBSD, and Windows. Clozure CL was forked from Macintosh Common Lisp (MCL) in 1998 and the development has been entirely separate since.

When it was forked from MCL in 1998, the new Lisp was named OpenMCL. Subsequently, Clozure renamed its Lisp to Clozure CL, partly because its ancestor MCL has been released as open source. Clozure thought it might be confusing for users if there were two independent open-source projects with such similar names. The new name also reflects Clozure CL's current status as the flagship product of Clozure Associates.

Furthermore, the new name refers to Clozure CL's ancestry: in its early years, MCL was known as Coral Common Lisp, or “CCL”. For years the package that contains most of Clozure CL's implementation-specific symbols has been named “CCL”, an acronym that once stood for the name of the Lisp product. It seems fitting that “CCL” once again stands for the name of the product.

Some commands and source files may still refer to “OpenMCL” instead of Clozure CL.

Clozure CL compiles to native code and supports multithreading using native OS threads. It includes a foreign-function interface, and supports both Lisp code that calls external code, and external code that calls Lisp code. Clozure CL can create standalone executables on all supported platforms.

On Mac OS X, Clozure CL supports building GUI applications that use OS X's native Cocoa frameworks, and the OS X distributions include an IDE written with Cocoa, and distributed with complete sources.

On all supported platforms, Clozure CL can run as a command-line process, or as an inferior Emacs process using either SLIME or ILISP.

Features of Clozure CL include

Very fast compilation speed.

A fast, precise, compacting, generational garbage collector written in hand-optimized C. The sizes of the generations are fully configurable. Typically, a generation can be collected in a millisecond on modern systems.

Fast execution speed, competitive with other Common Lisp implementations on most benchmarks.

Robust and stable. Customers report that their CPU-intensive, multi-threaded applications run for extended periods on Clozure CL without difficulty.

Full native OS threads on all platforms. Threads are automatically distributed across multiple cores. The API includes support for shared memory, locking, and blocking for OS operations such as I/O.

Full Unicode support.

Full SLIME integration.

An IDE on Mac OS X, fully integrated with the Macintosh window system and User Interface standards.

Excellent debugging facilities. The names of all local variables are available in a backtrace.

A complete, mature foreign function interface, including a powerful bridge to Objective-C and Cocoa on Mac OS X.

Many extensions including: files mapped to Common Lisp vectors for fast file I/O; thread-local hash tables and streams to eliminate locking overhead; cons hashing support; and much more

Very efficient use of memory

Although it's an open-source project, available free of charge under a liberal license, Clozure CL is also a fully-supported product of Clozure Associates. Clozure continues to extend, improve, and develop Clozure CL in response to customer and user needs, and offers full support and development services for Clozure CL.

For more information about install CCL, you can visit it's official site:

[http://ccl.clozure.com/docs/ccl.html](http://ccl.clozure.com/docs/ccl.html)


## Install

Clozure CL is distributed via the Internet. Please see [http://ccl.clozure.com/download.html](http://ccl.clozure.com/download.html) for instructions on how to download it.

Then you should read document for how to configure it.

[http://ccl.clozure.com/docs/ccl.html](http://ccl.clozure.com/docs/ccl.html)
