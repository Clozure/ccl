# Progress notes on an arm64 port

## May 21 – June 23
I looked a bit at Manfred Bergmann’s code at
https://github.com/mdbergmann/ccl/tree/arm64-arch-foundation. This code is
a Claude-assisted port of CCL using high tags.  It cross-creates a
bootstrapping image and manages to start it and load several fasl files.
This is quite impressive.

After long consideration, I decided that using low tags like the existing
CCL ports is a better choice.  This is mainly due to risk that future
platforms will stop supporting the TBI (top byte ignore) feature that
the high tag scheme depends on.

I used the [ccl-ffigen](https://github.com/Clozure/ccl-ffigen) tool
to process `.h` files.  This worked; the `.ffi` files will need to be
translated by Lisp code into the `.cdb` files that the `#_` and `#$`
reader macros consult.

I wrote out a (low) tagging scheme based on the existing ports.  The
traditional multi-level tagging (tag/lisptag/fulltag) as seen on
ppc64 doesn't really fit, but that's not problematic.  Defined uvector
header subtags, too.  Defined register partitioning.  (Much of this is
found in the file `ccl:compiler;ARM64;arm64-arch.lisp`.)

The majority of the effort over the past few weeks went to the assembler.
Some design points:
 * table-driven, like on other ports (mdbergmann code is driven by code)
 * works by parsing the LAP notation, finding a list of instruction templates
 for the mnemonic in question, and matching the supplied operands with the
 patterns defined in the templates.
 * used Claude Code to generate the instruction templates, avoiding the
 need to type in instruction encodings from the ARM manual, or from binutils
 or LLVM.

The LAP interface is starting to work.  LAP macros work.  The early
milestone I mention on https://github.com/Clozure/ccl/wiki/ARM64-port-draft-milestones now works:

```
(let ((*target-backend* (find-backend :darwinarm64)))
  (%define-arm64-lap-function
   'fact
   '((let ((n arg_z))
       (check-nargs 1)
       @l0
       (cmp n (:$ 0))
       (b.ne @continue)
       (mov arg_z (:$ '1))
       (ret)
       @continue
       (build-lisp-frame imm0)
       (str arg_z (:@! vsp (:$ -8)))
       (sub arg_z arg_z (:$ '1))
       (b.lt @l0)
       (ldr arg_y (:@+ vsp (:$ 8)))
       (restore-lisp-frame)
       (call-subprim .SPbuiltin-times)))))
#<XFUNCTION #x30200224F28D>
?  (uvref * 1)
#<XCODE-VECTOR #x30200224F22D>
? (dotimes (i (uvsize *)) (format t "~&~d: #x~8,'0x" i (uvref * i)))
0: #xF10020DF
1: #x54000040
2: #x0000000F
3: #xF100019F
4: #x54000061
5: #xD280010C
6: #xD65F03C0
7: xD2800B40
8: #xA9BE67E0
9: #xA9017BE7
10: #xF81F8F2C
11: #xD100218C
12: #x54FFFEEB
13: #xF840872B
14: #xA9417BE7
15: #xF94007F9
16: #x910083FF
17: #x910676E0
18: #xD63F0000
NIL
?
```
The Lisp disassembler doesn't work yet, but an arm64 disassembler shows
the following:
```
 0: f10020df subs xzr, x6, #0x8, lsl #0
 4: 54000040 b.eq #0xc
 8: 0000000f udf #0xf
 c: f100019f subs xzr, x12, #0x0, lsl #0
10: 54000061 b.ne #0x1c
14: d280010c movz x12, #0x8, lsl #0
18: d65f03c0 ret x30
1c: d2800b40 movz x0, #0x5a, lsl #0
20: a9be67e0 stp x0, x25, [sp, #-0x20]!
24: a9017be7 stp x7, x30, [sp, #0x10]
28: f81f8f2c str x12, [x25, #-0x8]!
2c: d100218c sub x12, x12, #0x8, lsl #0
30: 54fffeeb b.lt #0xc
34: f840872b ldr x11, [x25], #0x8
38: a9417be7 ldp x7, x30, [sp, #0x10]
3c: f94007f9 ldr x25, [sp, #0x8]
40: 910083ff add sp, sp, #0x20, lsl #0
44: 910676e0 add x0, x23, #0x19d, lsl #0
48: d63f0000 blr x0
```
What this shows:
* LAP macros working (e.g., `check-nargs`, `build-lisp-frame`
* The assembler working and supporting various register names (e.g,
`arg_z`) and operand types
* Generation of a function object (well, an xfunction object, because
we're cross-compiling) with a code-vector object that contains the
machine instructions.

Coming up next: add support for arm64 vinsn notation; define arm64 visns;
start filling in the arm642.lisp file (which is essentially the compiler
backend & code generator).  When that starts working, we'll be able to
cross-compile simple lambdas.


# compiling simple lambdas

At this point, the assembler (and maybe the disassemler) works well
enough to define simple lap functions.
```
CCL> (let ((*target-backend* (find-backend :darwinarm64)))
  (%define-arm64-lap-function
   'foo
   '((let ((offset arg_z))
       (cmp nargs (:$ '3))
       (b.le @done)
       (sub imm0 nargs (:$ '3))
       (add vsp vsp imm0)
       @done
       (mov arg_z rnil)
       (ret)))))
#<XFUNCTION #x302001C7977D>
CCL> (arm64-disassemble-xfunction *)
  (udf (:$ 0))                                              ; 00000000
  (cmp nargs (:$ 24))                                       ; F10060DF
  (b.le L20)                                                ; 5400006D
  (sub imm0 nargs (:$ 24))                                  ; D10060C0
  (add vsp vsp imm0)                                        ; 8B000339
L20
  (mov arg_z rnil)                                          ; AA1703EC
  (ret)                                                     ; D65F03C0
NIL
CCL> 
```

The next step is to cross-compile simple lambdas. 

`(compile-named-function '(lambda (x) x) :target :darwinarm64)`

In order to proceed,
it's necessary to start filling out arm642.lisp, and defining some arm64
vinsns.

Do `(setq *arm642-debug-mask* 2)`, and when the minimum parts of arm642 are filled in, you'll get:
```
CCL> (compile-named-function '(lambda (x) x) :target :darwinarm64)

 vinsns for NIL (after generation)
#<@0 CHECK-EXACT-NARGS 1>
#<@0 SAVE-LISP-CONTEXT-NO-STACK-ARGS>
#<@0 VPUSH-REGISTER #<LREG 2 GPR [12]/LISP>>
#<@0 SAVE-NFP>
#<@0 RESTORE-NFP>
#<@0 POPJ>
```



# Calling functions

On a non-x86 port, there is a separate value stack and control stack.
The value stack is always unambiguously nodes, from top to bottom.

The control stack (which is typically the architectural stack pointer)
contains frames.  Non-leaf functions need to save a frame.  A leaf
function doesn't clobber its return address or reference any constants.
Building a frame is a multi-instruction sequence, and `pc_luser_xp()`
needs to recognize the case of building a frame on the control stack
and make it look like an atomic operation.

# Returning values

Single values are returned in `arg_z`.  Multiple values are returned
on the stack, in left-to-right order (i.e., for a stack that grows down,
the rightmost value is on the top of the stack).

# Calling external (foreign) functions

The register and stack usage conventions for lisp code and external
(or foreign) are completely different.  For arm64, the AAPCS64 document
describes the standard ABI.  Apple platforms diverge from the
standard ABI in a few places.  See https://developer.apple.com/documentation/xcode/writing-arm64-code-for-apple-platforms for information about that.