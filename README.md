# Clozure CL

This is the source code for Clozure CL.

Because CCL is written in itself, you need an already-working version of CCL to compile it.

A working CCL consists of three parts.
* The heap image is a file that can be quickly mapped into a process's address space.  The heap image contains the lisp code and (other) data that make up the vast majority of CCL.
* The lisp kernel is a C program with a fair amount of platform-specific assembly language code.  The lisp kernel provides runtime support for lisp code.  When the lisp kernel starts up, it maps the heap image into memory and transfers control to compiled lisp code contained in the heap image.
* The interface database is a set of files derived from the operating system's C header files. The `#_` and `#$` reader macros consult this database to look up the definitions of foreign functions and constants.  The interface database is not required for CCL to start up and run already-compiled code, but it is required to compile CCL itself.

Archives containing a heap image, a pre-compiled lisp kernel, and an interface database are available from https://github.com/Clozure/ccl/releases.

So, to build the development version CCL from source, follow these steps:
* `git clone https://github.com/Clozure/ccl.git`
* Go to https://github.com/Clozure/ccl/releases, and download the appropriate archive of binaries for your platform.
* Unpack the archive, and move the contents into the source directory as needed.  For example, if you have checked out the sources into the `ccl-dev` directory on Linux running on x86, you could do the following:
```
cd ccl-dev
tar --strip-components 1 -xf ../ccl-1.12-dev.0-linuxx86-binaries.tar.bz2
```
Then, start lisp and compile the sources. See http://ccl.clozure.com/docs/ccl.html#building-clozure-cl-from-its-source-code for more detailed information.

In brief, if you have a working C compiler and the m4 macro processor installed, the following commands will compile a new CCL from source:
```
./lx86cl64  # or dx86cl64 or whatever
(rebuild-ccl :full t)
```

If you run into problems, please send mail to openmcl-devel@clozure.com, ask on #ccl on freenode, or create an issue here, especially if you think you have found a bug.

