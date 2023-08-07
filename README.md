# Clozure CL

This is the source code for Clozure CL.

Because CCL is written in itself, you need an already-working version of CCL to compile it.

A working CCL consists of three parts.
* The heap image is a file that can be quickly mapped into a process's address space.  The heap image contains the lisp code and (other) data that make up the vast majority of CCL.
* The lisp kernel is a C program with a fair amount of platform-specific assembly language code.  The lisp kernel provides runtime support for lisp code.  When the lisp kernel starts up, it maps the heap image into memory and transfers control to compiled lisp code contained in the heap image.
* The interface database is a set of files derived from the operating system's C header files. The `#_` and `#$` reader macros consult this database to look up the definitions of foreign functions and constants.  The interface database is not required for CCL to start up and run already-compiled code, but it is required to compile CCL itself.

For the latest released version of CCL, please see https://github.com/Clozure/ccl/releases/latest and download the archive for your platform.  These archives contain not only the necessary binaries, but also a full clone of the source code.  So, to install a complete CCL, all you have to do is download one archive and extract it.

If you want to run the development version of CCL, please see https://github.com/Clozure/ccl/releases.  You will typically want the most recent development snapshot.  Note that archives for a development snapshot include only binaries (heap image, lisp kernel, and interface database).  You must clone the repository first, and then unpack the binaries into your clone.

To report a bug or request an enhancement, please make an issue at https://github.com/Clozure/ccl/issues.

If you run into problems, please send mail to openmcl-devel@clozure.com, ask on #ccl on libera.chat, or create an issue here, especially if you think you have found a bug.
