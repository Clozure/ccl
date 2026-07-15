# Clozure CL Porting Guide

The words "host" and "target" have their usual compiler-jargon
meanings.

The word "platform" means a specific operating system and processor
architecture combination.  Some architectures have both 32- and
64-bit variants.  The PowerPC architecture looks almost the same for
both variants.  On x86, the 64-bit variant extends the 32-bit
variant with new instructions and more registers with new names.
On the other hand, 32- and 64-bit ARM architectures are totally different
(except in the general sense of being RISC-style load-store architectures).

We treat all fasl files and heap images as platform-specific, even in
cases where a lot of the content is probably the same. For example,
probably 99% or more of the code in an x86-64 image for FreeBSD is
identical to the code in an x86-64 image for Linux.

## Overview
What you're generally going to need to do is to cross-compile a set
of fasl files and a bootstrapping image from some host
machine on which CCL already runs.  Once you've done that, you copy
everything over to the target system, tell the target lisp kernel to
start with the bootstrapping image, watch it load a few dozen FASL
files, save a full heap image, then sit back and congratulate yourself.
(This may take several iterations, and it's sometimes more practical
to NFS-mount the host's CCL directory on the target to avoid having
to copy things around as often.)  In theory, it should be possible
to use any platform on which CCL already runs as the host; in practice,
it may work better if the host and target share the same word size
and endianness.

## Define the architecture description
For a new architecture, define a new architecture description
(for examples, see `ccl:compiler;**.*-arch.lisp`).  The architecture
description contains information about the target and the
tagging scheme (including things like a mapping between keywords
that name vector types and the actual 8-bit tags used to implement
them; the "backend" contains some additional platform stuff
(FFI details, FASL extension, etc.))

### Tagging

Given some arbitrary object reference, we need to be able tell what
kind of an object it is.

CCL allocates lisp objects in memory on double-node (dnode)
boundaries.  Because of this, some of the low bits of an address
are redundant.

On a 32-bit platform, a dnode is 8 bytes (64 bits) long.  That means
that the low three bits of an address are redundant: we only really
need to the upper 29 bits to know the address of a dnode-aligned
object.

On a 64-bit platform, a dnode is 16 bytes (128 bits) long, and the low
four bits are redundant in a similar way: the upper 60 bits are sufficient
to address a dnode-aligned object.

We call the redundant low 3 or 4 bits the "tag bits," and say that
we are using "low tags".

It is very tempting to exploit the arm64 TBI feature (where the
top 8 bits of addresses are ignored) to use a high tag scheme.
There are two main reasons I think it is better (i.e., lower risk,
less effort) to stick to a lowtag scheme.

The first and most important reason is uncertainty over the future
availability of the feature.  As of today, macOS and Linux (at least)
enable the TBI feature.  But I think there is medium-term risk that
memory safety features like ARM's Memory Tagging Extension and
[Apple's Memory Integrity Enforcement](https://security.apple.com/blog/memory-integrity-enforcement/)
will become widely adopted, and they are incompatible with the TBI feature.

By contrast, a lowtag scheme doesn't rely on any special hardware or
operating system support.

The other reason is consistency with the other ports.  This is a
weaker reason, because we generally want to exploit architecture-specific
features wherever we can.  But it is true that lots of low-level
logic is designed to work with a lowtag representation.

Tagging considerations:
 * It's important to quickly recognize fixnums.
 * It's important to quickly recognize lists (for car/cdr);  it's also
   desirable to quickly recognize cons cells.
 * It's desirable for vectorp, arrayp, and specific-array-type-p to be
   fast.  We need at least 12 immediate CL vector types:
     * {signed,unsigned}-byte {8,16,32,64}
     * single-float, double-float
     * bit
     * at least one character type
   As node types, we need:
     * simple-array
     * vector-header
     * array-header

## Define the `backend` structure
A lot of platform-specific attributes (but by no means all) are
encapsulated in a structure called a `backend`.  The existing `backend`
structures 
for x8664 platforms are defined in
"ccl:compiler;X86;X8664;x8664-backend.lisp".  The backend definitions
in that file are conditionalized so that only the native backend is
ordinarily defined; you probably want to add similar conditionalization
when editing that file, but you also want to mouse on the DEFSTRUCT
(e.g., select it and do C-M-x or equivalent in Emacs) while running
the host.  You want to ensure that the new backend is defined in the
host and that it's on `*KNOWN-BACKENDS*` and `*KNOWN-X8664-BACKENDS*`.
Once it is, call

? (ccl::fixup-x8664-backend)

just in case that function actually does something useful.

One attribute of a BACKEND is its "foreign type data" (FTD), which
basically describes how the FFI interacts with the backend.  There's
a large CASE form in the function SETUP-X8664-FTD; add a clause
describing your backend's FTD and call CCL::SETUP-X8664-FTD on the
new backend.

For a new target, it may be necessary to define a couple of interfaces
that describe some of the details of ff-calls and callbacks.  (Most
of the details have to do with how structures are returned and passed
by value; I think that that's very simple on ia32, and the convention
is only used in a couple of socket-related things in libc, so it's a
detail that you can worry about later.

## Create platform interface database

Use the interface translator (see ffigen5) and build appropriate `.cdb`
files for the target.  Note that these files are in native byte order.

## Cross-compiling

Define the target backend in the host.  This may involve evaluating
certain forms that have been conditionalized out.

`(find-backend :my-target)` (where `:my-target` is `:darwinarm64`,
`:linuxriscv64`, or whatever) should find your backend structure.


Once you have the infrastructure set up, it's not that bad; it is
indeed a case of:

- define the backend, possibly by loading a little .lisp file which
does it for you

- load the backend-specific parts of the compiler into the host.

- do `(cross-compile-ccl :<target>)` and then `cross-xload-level-0 :<target>)`

- run the result natively under gdb.  Curse.  Repeat previous
steps.

- when the result (finally) seems to work, you -may- note that
the result is confused about the size of some of the foreign
structures used to read info from .cdb files. (I may have
fixed the problem that caused this very recently, but I'm
not 100% sure that I ever understood what caused it.)  What
I generally have done is to load the freshly cross-compiled
lisp under Emacs and moused on every definition in
ccl:lib;db-io.lisp that referenced a :CDB-DATUM structure.
Once those functions were (re-)defined natively, it's generally
possible to compile natively.  Once that's possible, things
generally get much much simpler.


## Memory Layout

We ultimately want a few kb at #x2000 (the static area from the image,
containing NIL, T, a few dozen other symbols, and a few dozen global
variables used to communicate between lisp and the kernel) and another
kb at #x5000 or thereabouts, for the subprim jump table to be
"otherwise unused"; we'll also want a gig or two of otherwise unused
address space for the lisp heap, but that can be created at runtime.
In MCL, NIL was kept in a register (Classic MacOS programs ran in
a shared address space), and the low-memory area was sometimes called
the "nilreg area"; the symbols (including T and NILSYM) are still
sometimes called "nilreg relative symbols", but we assume that
we can map a few pages at a fixed address and that NIL is just a
constant address in that address range.)

On Linux, we can use linker scripts to try to ensure that the low
memory pages we want are otherwise unmapped (and we can generally
map the jump table at the address we want.)  On the PPC, it's desirable that the the nilreg area be in the low 32K of the
address space (r0+offset is basically 0+offset, so we can use
cheap absolute addressing.)  On x86, all absolute addresses are
equally bloated (and we may have trouble mapping the low 64Kb
on Windows ...)

I used to do other things to persuade PPC Darwin to leave a few pages
alone; some of those other things started breaking in Leopard.  What
happens now (an evil hack, but it seems to work) is to tell the linker to map the text section at #x1000, then to have the first
file (which is ppc-spjump.o on the ppc) start with:

       .org 0x5000-0x2000

and ends with

       .org 0x5000-0x1000

That, the fact that the text section starts at 0x1000, and various
linker artifacts conspire to mean:

- the pages beteen 0x2000 and 0x5000 are part of the text section
but don't have any meaningful contents (they're readonly and full
of 0s or NOPs or whatever.)

- the jump table starts at 0x5000, which happens to be
PPC::*PPC-SUBPRIMS-BASE*.

Sadly, the trailing .org seems to not have the intended effect (of
putting ppc-spentry.o's contents at a predictable place.)

In x86-spjump.s, there's a

       __ifdef([DARWIN])
       .space 0x5000,0
       __endif

which isn't quite as precise as the .org; that's apparently adequate
to leave the pages we want full of 0s.  If we can't get the subprims
jump table to land where we want it to (because of linker lossage),
the idea is to ensure that the page at #x5000 is unused; we map
it read/write at runtime and copy the subprim addresses from whereever
they wound up into that page.)

## Assembler

A port needs an assembler for two reasons.  Some special Lisp functions are
implemented in a notation called LAP.  These are defined using an
architecture-specific macro named something like `defarm64lapfunction`.

The other place we need the assembler is for vinsn templates.  These are
parameterized assembly language fragments that are and "simplified" at
defintion time.  The idea behind simplification is that part of the work
involved in assembing the vinsn body can be done at vinsn definition time.
Then, when the vinsn is turned into machine code (we call that process
"expansion"), we don't have to repeat that work.

The compiler backend emits vinsns as it translates the output of
the compiler front-end into object code.

Other ports have used GNU binutils as a source for instruction
encoding data and assembler structure.  The architecture-specific
directories ccl:compiler;**; contain the assembler and disassembler
files for existing ports.  Look at `*-asm.lisp`, `*-lap.lisp`, and
`*-disassemble.lisp`.

For examples of assembler input, see `*-vinsn.lisp` and the files in
`ccl:level-0;**;*.lisp`.  The `*-bignum.lisp` files are non-trivial, but
not too difficult to follow.

### Disassembler
Although a disassembler (for `cl:disassemble`) is not generally a
commonly-used part of a Common Lisp implementation, it is very helpful
(especially on a new port) for getting an early idea of what compiled
code looks like.

On RISC-style architectures, we can often reuse the assembler's
instruction table and data structures to implement disassembly,
and if the cpu architecture permits it, that is generally worth doing.
On x86, which has a variable-length instruction encoding, the disassembly
job is rather complicated, and we require about 3000 lines of code to
implement it.  By contrast, the RISC-like ports only need about 500 lines.

An assembler could be a project on its own (and so could a
disassembler, for that matter), but it is only a part of CCL.  Design
and implement something reasonable, knowing that it is internal
implementation functionality.

## Integrate LAP into compiler front-end & cross-dumper

Once the assembler and the LAP interface functions are ready,
add the appropriate special form to the compiler front-end.
In nx1.lisp, there are several `nx1-xxx-lap-function` definitions;
follow the example.  Also list the new special form in l1-utils.lisp
by the "Define special forms" comment.

This special form will be used a couple of places in addition to
%define-xxx-lap-function.  The cross dumper x<arch>fasload.lisp


## vinsn templates
We want to be able to write vinsn templates using a (mostly) LAP-like
syntax, but ideally don't want to have to repeatedly expand those
vinsn-definition-time-invariant elements of that syntax.
For example, if DEST is a vinsn parameter and the vinsn body
contains:
  `(ldr DEST (:@ rcontext (:$ arm::tcr.db-link)))`
then we know at definition time:
 1) the opcode of the LDR instruction (obviously)
 2) the fact that the LDR's `:mem12` operand uses indexed
    addressing with an immediate operand and no writeback
 3) in this example, we also know the value of the RB field
    and the value of the immediate operand, which happens
    to be positive (setting the U bit).
 We can apply this knowledge at definition time, and set
 the appropriate bits (U, RN, IMM12) in the opcode.
 We don't, of course, know the value of DEST at vinsn-definition
 time, but we do know that it's the Nth vinsn parameter, so we
 can turn this example into something like:
 ```
 `(,(augmented-opcode-for-LDR) #(rd-field) #(index-of-DEST)
```

## Stacks
Depending on the platform, CCL uses up to three stacks: a control stack,
a value stack, and a temp stack.

Except on the x86 ports, there is a separate value stack and control stack.
The value stack is always unambiguously nodes, from top to bottom.

The control stack (which is typically the architectural stack pointer)
contains frames.  Non-leaf functions need to save a frame.  A leaf
function doesn't clobber its return address or reference any constants.
Building a frame is a multi-instruction sequence, and `pc_luser_xp()`
needs to recognize the case of building a frame on the control stack
and make it look like an atomic operation.

## Memory allocation
Historically, CCL has managed a single dynamic area, which is where allocation takes place.

CCL uses a bump allocator.  The `allocptr` is the current top.  Allocation
groups downward; `allocbase` is the low limit.

Allocation:

A thread tries to allocate an object whose physical size in bytes is X and whose tag is Y by:

1. decrementing the "high" pointer by (- X Y)

1. trapping if the high pointer is less than the low pointer

1. using the (tagged) high pointer to initialize the object, if necessary

1. clearing the low bits of the high pointer

There are a few different states of partial allocation; the GC must be
able to recognize these states and deal with them (either by emulating
the rest of the allocation or backing it out).

# Lisp kernel
The Lisp kernel is C and assembly code that provides GC and other runtime support for Lisp.

C and assembly language files need to agree with the Lisp compiler on
numerous constants and memory layout descriptions.

| Lisp files    | Lisp kernel files |
| ---           | --- |
| arm-arch.lisp | arm-constants.h, arm-constants.s |

Generally, in `lisp-kernel/` there are a handful of files specific to each
architecture, named like `arm-*.[csh]`.  A new architecture will need a
similar set of files.

## Assembler macros and m4
CCL preprocesses `.s` files with m4.  Although m4 is considerably more
capable than assembler macros, there are downsides.
 * it is harder to see the actual source the assembler sees
 * m4 macros do extra work to emitting source line information
 * as part of the source line machinery, the `__` macro has to wrap each
 instruction.  This is ugly.
 * the m4 macros use obsolete stabs directives

The arm64 port attempts to stop using m4 to get better source code
visibility and improved source line information.  The hope is that assembler
macros and the C preprocessor will be expressive enough to get away with this.

One other thing that that the arm64 port will try is to use a single
`arm64-constants.h` that is constructed such that it can be included in
both assembly and C source files.  Having to duplicate information
from `.lisp` files is bad enough: having to duplicate it twice in separate
`arm64-constants.h` for C and `arm64-constants.s` for assembly just makes it
worse.  At least if it's all in one place it should be a little easier
to maintain.

## Subprimitives
Subprims are little assembly-language snippets that run in the Lisp world.
In other words, they use Lisp register and stack conventions, and must be
written in a GC-safe way.

## GC safety
CCL's garbage collector design is such that a GC may happen at any
instruction boundary.  This means that it is essential to keep nodes in
node registers at all times: the GC might need to relocate the objects,
and in that case, it will update any node registers with their new
locations.  This also means that interior pointers
(e.g., registers holding a pointer into the middle of some uvector) are
not allowed.

In other words, all references to garbage-collected memory
must be relative to a node register containing a tagged pointer. This
is sometimes a burden, especially on systems with few registers.

## Lisp calling convention
Lisp's internal calling covention differs completely from the one used by C.

On most ports, the last three arguments to a function are passed in
registers, namely arg_x, arg_y, and arg_z.  Any earlier arguments are
passed on the stack.  The nargs register (which may be an alias for
some other register) contains the actual number of arguments, tagged
as a fixnum.

The registers nfn, fname, and next-method-context are also part
of the calling convention, but they are used only briefly at function
entry and are therefore rather short-lived.  They are typically aliases
for temp registers.  The nfn register ("new function") is used to
establish the new fn register (and be sure that the old value of fn
is stashed somewhere, because otherwise it might get gc'd).

# Calling functions

On a non-x86 port, there is a separate value stack and control stack.
The value stack is always unambiguously nodes, from top to bottom.

The control stack (which is typically the architectural stack pointer)
contains frames.  Non-leaf functions need to save a frame.  A leaf
function doesn't clobber its return address or reference any constants.
Building a frame is a multi-instruction sequence, and `pc_luser_xp()`
needs to recognize the case of building a frame on the control stack
and make it look like an atomic operation.  The gc needs to scan
certain frames on the control stack.

## Returning values
Single values are returned in `arg_z`.  Multiple values are returned
on the stack, in left-to-right order (i.e., for a stack that grows down,
the rightmost value is on the top of the stack).

## Calling external (foreign) functions
The register and stack usage conventions for lisp code and external
(or foreign) are completely different.  For arm64, the AAPCS64 document
describes the standard ABI.  Apple platforms diverge from the
standard ABI in a few places.  See https://developer.apple.com/documentation/xcode/writing-arm64-code-for-apple-platforms for information about that.

## Architecture-specific variants

level-1/xxx/xxx-clos.lisp: GF trampolines


## Error handling
CCL likes to use architecture-specific instructions to signal errors.
It calls these UUOs (unimplemented user operations, terminology
from the PDP-10, I believe).  The details vary, but the main idea is
that a UUO will cause a signal (SIGILL, SIGSEGV, whatever) that the
lisp kernel will catch and handle.

Some UUOs request services from
the lisp kernel (e.g., start a gc, configure gc paramers), and these
are handled in the lisp kernel directly.

Others are caught by the exception/signal handler in the lisp kernel,
which then calls back into Lisp, where the error is ultimately signaled.
You can see some of the Lisp-side code for this in
the files `ccl:level-1;*-error-signal.lisp` and `*-trap-support.lisp`.
These files contain code that knows how to examine a signal context
and decode what happened.


## xload-level-0

One of the things that's been on the to-do list for a long time is to
fix a few aspects of how xload-level-0 works.  Currently, it tries to
compile and cross-load everything in level-0 and platform-specific
subdirectories (rather than a specified set of common and platform-specific
files.)  There are some weird artifacts of this:

- until DIRECTORY was changed to ignore them, xload-level-0 used to
 be confused by Emacs lock files (#.foo.lisp) and can still get confused
 by other cruft.
- there are accidental load-order dependencies; the main application
 startup function (defined in nfasload.lisp) runs after other initialization
 functions run, simply because "nfasload" follows the other files in
 sort order.
- Some "common" files are completely conditionalize (l0-bignum32/64, etc.)

## Function-like things in a symbol's function cell
From a comment in l0-def.lisp:

 There are three kinds of things which can go in the function
 cell of a symbol:
  1. A function
  2. The thing which is the value of %unbound-function%:
    a 1-element vector whose 0th element is a code vector
    which causes an "undefined function" error to be signalled.
  3. A macro or special-form definition,
    which is a 2-element vector whose 0th element is a code vector
    which signals a "can't apply macro or special form" error when
    executed and whose 1st element is a macro or special-operator
    name.  It doesn't matter what type of gvector cases 2 and 3
    are.

This is true for the ppc port.  On the ARM port, it's not possible to
branch to a code-vector directly, so it invents the pseudofunction,
which has basically the same layout as a function object, but isn't one. On
the x86 port, we just cons up a function vector and write a few bytes of
code that start at the entry point.  On the ARM64, I'm thinking cases
2 and 3 above are going to be real functions, but their code vectors
will be distingushed objects.  So, if the code vector of function in the
function cell is eq to %unbound-function%, then the symbol is not fboundp.

For macros and special operators, we detect the sitaution like this:
```
;; Element 0 (past the header) is a code vector; element 1 is a marker:
;; the expander for a macro, or the special-operator name (not a function)
;; for a special form.
(defun special-operator-p (symbol)
  "If the symbol globally names a special form, return T, otherwise NIL."
  (let ((def (fboundp symbol)))
    (and (functionp def)
         (eq (uvref def 0) %macro-code%)      ; the shared macro/special trampoline
         (not (lfunp (uvref def 1))))))        ;name:  special-op; else macro
```
