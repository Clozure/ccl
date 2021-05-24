# Clozure CL

This is the source code for Clozure CL.

## CCL Binaries

For the latest released version of CCL, please see https://github.com/Clozure/ccl/releases/latest and download the archive for your platform.

## Building CCL from Source

> As of version 1.12

Because CCL is written in itself, you need an already-working version of CCL to compile it. The [CCL Releases](https://github.com/Clozure/ccl/releases) contain not only the necessary binaries, but also a full clone of the source code. So, to install a complete CCL, all you have to do is download one archive and extract it.

A working CCL consists of three parts.
* The heap image is a file that can be quickly mapped into a process's address space. The heap image contains the lisp code and (other) data that make up the vast majority of CCL.
* The lisp kernel is a C program with a fair amount of platform-specific assembly language code.  The lisp kernel provides runtime support for lisp code.  When the lisp kernel starts up, it maps the heap image into memory and transfers control to compiled lisp code contained in the heap image.
* The interface database is a set of files derived from the operating system's C header files. The `#_` and `#$` reader macros consult this database to look up the definitions of foreign functions and constants.  The interface database is not required for CCL to start up and run already-compiled code, but it is required to compile CCL itself.

To build CCL from source:

1. First, obtain the source code for CCL by cloning the repository (say, with `git clone https://github.com/Clozure/ccl.git`)
2. The second step is to download preexisting system-specific binaries for purposes of bootstrapping from the [(latest) release](https://github.com/Clozure/ccl/releases/latest). The binary archives also contain a pre-built database derived from the system header files that is used by the FFI.
3. After unpacking the bootstrapping binaries, you can rebuild CCL by evaluating `(rebuild-ccl :full t)`

For example, to get a copy of CCL that runs on macOS, you would say:

```
git clone https://github.com/Clozure/ccl.git ccl-dev
curl -L -O https://github.com/Clozure/ccl/releases/download/v1.12.1/darwinx86.tar.gz
cd ccl-dev
tar xf ../darwinx86.tar.gz
```

Unfortunately, on Windows and macOS Catalina (and Big Sur), `(rebuild-ccl :full t)` will not work. On those systems, you must first build the lisp kernel binary (a program as described above). To do this, use the following commands:

```
cd lisp-kernel/darwinx8664
# or lisp-kernel/win64 or lisp-kernel/win32
make clean
make
```

When that is done, start the lisp and evaluate `(rebuild-ccl :clean t)`. This does the same thing as `:full t`, except it will not attempt to rebuild the lisp kernel for you.

If you have some version of Xcode 11, you must use Xcode 11.4 or later to compile the CCL lisp kernel because of a bug in the assembler (https://github.com/Clozure/ccl/issues/271). Earlier versions of the Xcode tools are fine.

Note that it is not necessary to download a full git clone; appropriate [CCL Releases](https://github.com/Clozure/ccl/releases) also provide the source code associated with the binaries.

## Bugs, Requests or Issues

To report a bug or request an enhancement, please make an issue at https://github.com/Clozure/ccl/issues.

If you run into problems, please send mail to openmcl-devel@clozure.com, ask on #ccl on freenode, or create an issue here, especially if you think you have found a bug.
