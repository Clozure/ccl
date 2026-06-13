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

We need an assembler for two reasons.  Some special Lisp functions are
implemented in a notation called LAP.  These are defined using an
architecture-specific macro named something like `defarm64lapfunction`.

The other place we need the assembler is for vinsn templates.  These are
assembly language fragments that are partly pre-assembled.  The compiler
backend emits vinsns as it translates the output of the compiler front-end
into object code.

Other ports have used GNU binutils as a source for instruction
encoding data and assembler structure.  The architecture-specific
directories ccl:compiler;**; contain the assembler and disassembler
files for existing ports.  Look at `*-asm.lisp`, `*-lap.lisp`, and
`*-disassemble.lisp`.

For examples of assembler input, see `*-vinsn.lisp` and the files in
`ccl:level-0;**;*.lisp`.  The `*-bignum.lisp` files are non-trivial, but
not too difficult to follow.

We need a disassembler (for `cl:disassemble` at least). On RISC-style
architectures, we can often reuse the assembler's data structures to
implement disassembly.  On x86, which has a variable-length instruction
encoding, we can't do that, and we have 3000 lines of code to implement
the x86 disassembler. By contrast, we only need about 500 lines for each
of the PowerPC and ARM disassemblers.

An assembler could be a project on its own (and so could a
disassembler for that matter), but it is only a part of CCL.  Make
something reasonable, knowing that it is internal implementation
functionality.

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

The value stack is always unambiguously nodes.

The control stack contains frames.