# Building CCL for Linux/arm64

Notes from getting the linuxarm64 target to build and boot. Assumes this
branch (or the two PRs that produced it) is applied; with those in place the
build is the ordinary CCL build, and what follows is only the parts that are
not obvious.

## You need two machines (or one plus a cross environment)

- **an x86-64 Linux host** to build the heap image, and
- **a Linux/arm64 machine** to build the kernel and run the result.

The image cannot be built natively on arm64: building it runs the
cross-compiler, which needs a working host CCL. The kernel is the only piece
built natively on the target.

## The host CCL must be new enough

The host CCL that runs the cross-compile has to know the `aapcs64-ff-call`
nx1 operator. **A pristine 1.12.2 does not**, and the cross-compile dies
partway through with an obscure failure. A 1.13 built from the `arm64` branch
(`da35a7d` or later) works.

## Kernel

```
cd lisp-kernel/linuxarm64
make
```

The Makefile in this branch has been corrected for aarch64; before that it
was ARM32 inheritance and had never been run. In particular the `.s.o` rule
builds with `$(CC) -x assembler-with-cpp` rather than m4, because the arm64
assembly layer is cpp-based — `imports.s` is the one genuine m4 file and
keeps a rule of its own.

## An install is six parts, not two

This is the step that wastes an afternoon. A runnable install needs:

```
armcl64                 the kernel
arm64-boot-*.image      the heap image
level-1.la64fsl         the startup fasl
bin/                    level-1 fasls
l1-fasls/               more of the same
arm64-headers64/        the FFI interface databases
```

Copy only the kernel and the image and it dies with a **bare SIGSEGV that
looks like a port bug and is not**: startup opens `level-1.la64fsl` relative
to the install, `CDB-OPEN` then needs `arm64-headers64/`, and the failure
happens *inside the error path* —

```
Interface file .../arm64-headers64/libc/records.cdb does not exist
  -> Unknown foreign type: :<D>L_INFO
  -> FATAL (cold load): unbound variable *PRINT-PPRINT-DISPATCH*
```

`strace` names it in one run; a register dump does not.

Run with the install directory as cwd — the image resolves `ccl:` and its
startup fasl relative to itself:

```
cd /your/install && ./armcl64 --image-name ./arm64-boot-.image
echo '(print (list :ok (+ 1 2) (sin 1.0d0)))' | ./armcl64 --image-name ./arm64-boot-.image
```

Expect `(:OK 3 0.8414709848078965D0)`.

## Floating point on this target

AArch64 defines the FPCR trap-enable bits (`IOE`/`DZE`/`OFE`/`UFE`/`IXE`),
but implementing trapped floating-point exceptions is **optional**, and they
are RAZ/WI on at least Neoverse N1 — writing `0x1f00` to FPCR reads back
`0x0`. So there is no SIGFPE to field the way `ppc-exceptions.c` does, and
enabled-exception detection polls the cumulative FPSR flags at checkpoints
instead.

Two consequences worth knowing before debugging anything float-related:

- **FPSR exception flags are cumulative.** Any code that captures them for a
  foreign call must zero FPSR *before* the call, not only after it —
  otherwise the captured word carries every flag raised since the previous
  foreign call, including unrelated inline Lisp arithmetic, and the wrong
  callee gets blamed. That produced spurious `FLOATING-POINT-OVERFLOW` from
  `log`, `exp`, `sin` and `atan`.
- Inline floating-point arithmetic therefore cannot signal on such a part.
  `(expt <float> <integer>)` is repeated multiplication and never reaches
  `pow`, so it needs its own checkpoint if it is to conform.

## Status

With this branch applied, the Dietz ANSI suite runs to completion at **21677
of 21679 passing, no exclusions** — the two remaining failures reproduce
identically on x86-64 CCL 1.12.2.
