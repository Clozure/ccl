# arm64 notes

## ISA features
Generally we want to stick to the baseline instruction set.  There's
a fairly common feature, FEAT_LSE, that we may want to require.  The
notable item of interest is a collection CAS instructions.

## LAP notation

### Operand formats

Every operand is written either as a bare symbol (a register or a branch
target) or as a list whose first element is a keyword.
The complete set the assembler accepts:

| Kind | LAP syntax | Example | Meaning |
|------|-----------|---------|---------|
| register | `regname` | `x0`, `d3`, `sp`, `arg_z` | a named register (see below) |
| view override | `(:view r)` | `(:s d0)`, `(:w imm0)` | register `r`'s number at a different width/view |
| vector element | `(:sz r i)` | `(:d q0 1)` | one lane `Vr.Ts[i]` of a SIMD register |
| vector arrangement | `(:arr r)` | `(:8b q0)` | a whole vector `Vr.<T>` |
| shifted/extended reg | `(r mod amt)` | `(x2 :lsl 3)`, `(w2 :uxtw)` | `Rm` with a shift or extend |
| immediate | `(:$ value)` | `(:$ 16)`, `(:$ '2)`, `(:$ 'name)` | a constant, optionally `:lsl`-shifted; quoted forms are special (below) |
| condition | `(:? cc)` / `(:~ cc)` | `(:? eq)`, `(:~ lt)` | a 4-bit condition, or its inverse |
| memory | `(:@ …)` `(:@! …)` `(:@+ …)` | `(:@ x1 (:$ 16))` | a load/store address (see below) |
| label | `symbol` | `@done`, `:ok` | a branch target |

**Registers**: the GPRs `x0`–`x30` / `w0`–`w30`, the
stack pointer `sp`/`wsp`, the zero register `xzr`/`wzr`, and the SIMD&FP
registers at every scalar width: `b0`–`b31` (8-bit), `h0`–`h31` (16),
`s0`–`s31` (32), `d0`–`d31` (64), `q0`–`q31` (128). All the Lisp register
aliases (`arg_z`, `imm0`, `fn`, `temp0`, `vsp`, `rnil`, …) are accepted
wherever a register name is.

**View overrides**: `(:b r)` `(:h r)` `(:s r)` `(:d r)` `(:w r)` `(:x r)`.
These take the *number* of register `r` and name it at the given view: the
`B/H/S/D` scalar-FP widths or the `W/X` GPR widths, regardless of how `r`
was spelled. So `(:s d0)` is `s0`, and `(:w imm0)` is the 32-bit view of the
GPR `imm0`.

**Vector element operands**: `(:b r i)` `(:h r i)` `(:s r i)` `(:d r i)`.
These name one lane of a SIMD register, `Vr.<B|H|S|D>[i]`, and are used by
the lane insert/extract instructions (`ins`, `dup`, `mov` element). The
trailing index is what distinguishes a lane from a view override. Example:
`(ins (:d q0 1) (:d q1 0))` is `INS V0.D[1], V1.D[0]`.

**Vector arrangement operands**: `(:8b r)` `(:16b r)` `(:4h r)` `(:8h r)`
`(:2s r)` `(:4s r)` `(:1d r)` `(:2d r)`. These are a whole SIMD register viewed
as an arrangement of lanes, `Vr.<T>`, for the whole-vector
data-processing instructions (`cnt`, `addv`, …). Example:
`(cnt (:8b q0) (:8b q0))` is `CNT V0.8B, V0.8B`. The arrangement encodes as
the `Q` bit plus the 2-bit element size.

In both element and arrangement operands `r` is any FP/SIMD register name —
only its *number* is used, so the whole-register `q` name is the natural
spelling (there is no `v0` name; `(:d q0 1)` is ARM's `V0.D[1]`).

**Shifted/extended registers** are `(r mod amt)`, where `mod` is a shift
(`:lsl :lsr :asr :ror`) or an extend (`:uxtb`…`:sxtx`) and `amt` is the
constant amount; a bare register is the unshifted `lsl #0` case.

**Immediates** are `(:$ value)`, optionally `(:$ value :lsl shift)`. The
value is an integer, a float (the FP-move immediate), or a constant
expression the assembler evaluates. There are several encodings for
immediates (add/sub imm, logical bitmask, move-wide, branch offset, …);
the appropriate one is selected by the mnemonic's template.  Generally,
you can just write the value you want and the assembler will pick the
right one (or complain that the value is not encodable).

A **quoted** value gets special treatment:
- `(:$ 'n)` for an integer `n` is the *fixnum* representation of `n` — the
  assembler shifts it left by `fixnumshift` (3). So `(:$ '2)` is `16`, the
  boxed fixnum 2.
- `(:$ 'name)` for a symbol is the fn-relative **byte offset** to the
  constant named `name`. The symbol is interned into the list of constants
  on first use. The offset refers directly to the constant in
  question, so it can be used as a load offset from `fn`, e.g.
  `(ldur dest (:@ fn (:$ 'name)))`.

**Conditions** are `(:? cc)` for a condition name (`eq`, `ne`, `lt`, `ge`,
`hi`, `ls`, …) and `(:~ cc)` for its inverse (used by `cbranch-false` and
the `cset`/`csinv`-style aliases).

**Labels** are bare symbols; a symbol that is neither a register nor a known
system register is treated as a branch target.


### Memory operands
Memory operands use three markers — `:@` (plain), `:@!` (pre-index), and
`:@+` (post-index) — over five underlying spec forms. The complete catalog,
with the immediate ranges and some examples, is below.

## Memory operand formats

| # | Spec form | LAP syntax | Meaning | Offset class | Writeback |
|---|-----------|-----------|---------|--------------|-----------|
| 1 | `:mem-scaled` | `(:@ Xn)` or `(:@ Xn (:$ k))` | `[Xn{, #k}]` — base + **scaled** unsigned imm | `:uoffN` | no |
| 2 | `:mem-unscaled` | `(:@ Xn (:$ k))` | `[Xn, #k]` — base + **unscaled** signed imm (the `…ur` mnemonics) | `:simm9` | no |
| 3 | `:mem-regoff` | `(:@ Xn Rm)` / `(:@ Xn (Rm mod amt))` | `[Xn, Rm{, extend #amt}]` — base + index register | `:regoffN` | no |
| 4 | `:mem-pre` | `(:@! Xn (:$ k))` | `[Xn, #k]!` — base + imm, **then** write `Xn←Xn+k` | `:simm9` / `:poffN` | yes (pre) |
| 5 | `:mem-post` | `(:@+ Xn (:$ k))` | `[Xn], #k` — use `Xn`, **then** write `Xn←Xn+k` | `:simm9` / `:poffN` | yes (post) |

The base `Xn` is always an X register or SP (`:x/sp`). The bare `(:@ Xn)` with no offset is `[Xn, #0]`.

## Offset immediate classes (range depends on access size)

| Class | Encoding | Scaled? | Range (per access size `s` = 1/2/4/8 bytes) | Used by |
|-------|----------|---------|---------------------------------------------|---------|
| `:uoffN` | unsigned imm12 @ 21:10 | **yes**, by `s` | `0 … 4095·s`, multiple of `s` | `:mem-scaled` single-reg |
| `:simm9` | signed imm9 @ 20:12 | no (byte offset) | `-256 … 255` | `:mem-unscaled`, pre/post single-reg |
| `:regoffN` | Rm @ 20:16, opt @ 15:13, S @ 12 | optional (S bit) | index reg; shift 0 or `log2 s` only | `:mem-regoff` |
| `:poffN` | signed imm7 @ 21:15 | **yes**, by `s` | `-64·s … 63·s`, multiple of `s` | `ldp`/`stp` pre/post/offset |

Worked examples of the ranges, for a **64-bit** access (`s = 8`):

- `:uoff3` → `0 … 32760` (e.g. `(ldr x0 (:@ x1 (:$ 32760)))`)
- `:simm9` → `-256 … 255` (e.g. `(ldur x0 (:@ x1 (:$ -8)))`)
- `:poff3` → `-512 … 504` (e.g. `(stp x29 x30 (:@! sp (:$ -16)))`)

Because Lisp object pointers carry a tag in their low bits, reaching a slot
of a boxed object loads from `base + (offset - tag)` — the tag subtraction
is folded into the constant displacement. The scaled immediate mode
(`:uoffN`) can only encode displacements that are a **multiple of the access
size**, and the tag adjustment knocks the offset off that boundary: a
uvector's data starts at `misc-data-offset` = 4 bytes past the tagged
pointer (`node-size - fulltag-misc` = 8 - 4), so its 8-byte slots sit at
byte offsets 4, 12, 20, … — never a multiple of 8. Tagged-object access
therefore uses the **unscaled** `ldur`/`stur` (`:simm9`, byte-granular,
±255) instead; the scaled modes stay useful for untagged bases like the
stack pointer.

## Examples

```lisp
(ldr x0 (:@ x1 (:$ 16)))         ;[x1, #16]        scaled offset
(ldr x0 (:@ x1 x2))              ;[x1, x2]         register offset, no scale
(ldr x0 (:@ x1 (x2 :lsl 3)))     ;[x1, x2, lsl #3] scaled index
(ldr x0 (:@ x1 (w2 :uxtw)))      ;[x1, w2, uxtw]   32-bit index
(ldur x0 (:@ x1 (:$ -8)))        ;[x1, #-8]        unscaled, negative
(str x5 (:@! vsp (:$ -8)))       ;[vsp, #-8]!      push (pre-decrement)
(ldr x5 (:@+ vsp (:$ 8)))        ;[vsp], #8        pop (post-increment)
(stp x29 x30 (:@! sp (:$ -16)))  ;[sp, #-16]!      frame push (pair)
(ldp x29 x30 (:@+ sp (:$ 16)))   ;[sp], #16        frame pop (pair)
```

One thing worth noting: `:mem-scaled` (marker `:@`) and `:mem-unscaled` (also `:@`) are disambiguated by *mnemonic*, not by syntax — `ldr` carries the scaled `:uoffN` template, while `ldur` carries the unscaled `:simm9` one. The user picks scaled vs. unscaled by choosing `ldr`/`ldur`, exactly as in real A64.

## Addressing modes
There are two variants of the base + immediate addressing mode.
One variant supports an 9-bit signed offset.  Other than being a
rather small range (-256 to 255), it works in a straightforward
way: the effective address is the base register + the value of the
9-bit signed immediate.

The other variant is a bit tricky.  The immediate offset is encoded as a
12-bit unsigned value, but that value is scaled by the access
size.  However, the assembly language always expresses the offset
as a byte offset.

For example, (ldr x0 (:# x1 (:$ 24))) means (in C-like notation)
"x0 = x1[3]".  Although we write the byte offset "24" in the
assembly language, the assembler actually encodes "3" (the byte
offset divided by the memory access size) into the imm12 field of
the ldr instruction word.

When indexing a uvector, the immediate has to include not only the
index value itself, but also the appropriate offset to subtract
off the tag and account for the uvector header.

So the fulltag for a uvector affects which encoding a constant index
can use.  We picked #b1100 (12) for fulltag-misc so that a tagged
code-vector pointer can be branched to directly (see the note up by
the fulltag definitions).  A side effect is that misc-data-offset is
-4 -- negative -- so element 0 sits just behind the tagged pointer.
That only costs us the scaled encoding for the lowest element(s).
Element 0, at byte offset -4, uses the 9-bit signed form (LDUR); but
every element from index 1 up is at a non-negative multiple of the
access size (0, 4, 8, ... for 32-bit elements) and still uses the
12-bit unsigned scaled encoding.  So we keep scaled addressing for
all but the first element (for 16- and 8-bit types, a couple more
low elements land in the signed-offset window).  The loss is
negligible: LDUR is the same-speed load, and those low elements are
always well within its -256..255 range.

As before, 64-bit elements never get the scaled encoding: that would
need misc-data-offset to be a multiple of 8, i.e. fulltag-misc
#bx000, and those fulltags are used for fixnums.

## UUOs

On arm64 we encode UUOs with `udf`, whose 16-bit immediate carries
all of the payload.  `udf` is architecturally undefined and always raises an
undefined-instruction exception (SIGILL).  The `hlt` instruction could
probably work here, or maybe `brk`, but `udf` doesn't carry any extra semantic
intent, so it seems like the best choice.

The encoders (macros) live in `lisp-kernel/arm64-uuo.s`; the wrong-type
decoder table lives in `level-1/arm64-error-signal.lisp`; the extended type
codes and their collision guard live in `compiler/ARM64/arm64-arch.lisp`.

### Formats

The low two bits of the udf instruction's 16-bit immediate indicate one
of four formats:

| bits 1:0 | format | layout of the upper bits |
|----------|--------|--------------------------|
| 0 | `misc` | 14-bit code in 15:2 (must not be all zero) |
| 1 | `unary` | reg in 6:2, 9-bit info in 15:7 |
| 2 | `binary` | ra in 6:2, rb in 11:7, 4-bit info in 15:12 |
| 3 | `wrong_type` | reg in 6:2, continuable flag in 7, expected type in 15:8 |

`misc` holds the nullary (0-argument) traps: the kernel-handled ones
(allocate, gc-trap, debug-trap, interrupt/suspend, ...) and the nargs
errors that call out to lisp (too-few / too-many / wrong-number of args).
A misc format UUO must not be all zero: `udf #0` is reserved for a
"start-of-code-vector" sentinel.

`unary` holds register-bearing conditions that are not type errors:
not-callable, no-throw-tag, unbound-variable, undefined-function,
undefined-function-call, and the kernel-handled grow-the-TLB-chain request.

`binary` holds two-register errors: vector/array bounds, integer divide by
zero, unresolved eep/fv, FPU exception, array rank/flags.  (We can't rely
on FP exceptions to trap on arm64; the FPU-exception UUO would be emitted
after polling the relevant FPSR bits.)

### The wrong-type format and the xtype namespace

Every type error ("this register doesn't hold the expected type") uses the
`wrong_type` format.  The 8-bit code names the expected type and is one of
four things sharing a single 8-bit-wide namespace:

  * a lisptag value (`tag-fixnum`, `tag-list`, etc.)
  * a fulltag value (`fulltag-misc`, `fulltag-symbol`, etc.)
  * a uvector subtag byte (`subtag-bignum`, `subtag-function`, etc.)
  * a synthetic *xtype* code, for abstract types that aren't a single subtag
    (`integer`, `(signed-byte 32)`, `bit`, a 2-D array, a strict `cons`, ...)

The wrong type errors just barely fit into one UUO format.
The 8-bit code has zero headroom (256 xtypes, 256 slots), so the
type/non-type distinction can't live in the payload: the uuo format itself
encodes it.  That's why `unary` and `wrong_type` have the same layout
(reg + 9 bits) but disjoint roles.

The xtype codes must not collide with any defined subtag byte, since
both share the namespace.  See `arm64-arch.lisp` for the details.

Lisp error handling code indexes `*arm64-xtype-specifiers*`
to recover a type specifier for the `type-error`.

If the continuable bit is set, that means Lisp should signal a cerror.
However, other ports don't always take the trouble to support this:
the x86 port seems to have dropped the idea that a UUO should support
continuing, and 32-bit ARM has cerror UUO formats, but the continuable
flag is decoded and dropped.  Apparently only the PPC port did the full job.

### Errors that need three registers

`slot-unbound` and `array-axis-bounds` want three register operands, and
three 5-bit fields plus a format don't fit in 16 bits.  Rather than invent a
wider mechanism, we emit the primary UUO and follow it with a companion
`binary` UUO (`binary_info_two_registers`) carrying the extra register pair.
The handler reads the companion as data and bumps the PC past both; the
companion is never reached by execution.  If control ever *did* land on a lone
`two_registers` UUO, the handler should treat that as an internal error.

## Functions

The 32-bit ARM port invented a function representation that has an
entrypoint slot, and code-vector slot, and then the usual constants and
function metadata.

The idea is to make calling a function cheap.  The gc
updates the entrypoint slot if the function object moves.  It also
updates lr, because that register is classed as a "pc-locative"; i.e.,
it's a pointer into a code-vector (or else pointing to some text area)

Here's what 32-bit arm does:
```
(define-arm-vinsn (call-known-function :call) (() ())
  (ldr lr (:@ nfn (:$ arm::function.entrypoint)))   ; two-instruction
  (blx lr))                                          ; window

(define-arm-vinsn (jump-known-function :jumplr) (() ())
  (ldr pc (:@ nfn (:$ arm::function.entrypoint))))   ; ONE instruction
```
In the first case, lr needs to be treated specially by the gc; in the
second case, it doesn't matter because control transfer is happening
without having to load the entrypoint into an intermediate register.

On arm64, if we pick the right the tag for a code-vector object, we can
branch right to the tagged pointer.  Like ppc64, we select #b1100 as
fulltag-misc. 

Naturally, a code-vector holds 32-bit elements (instructions).
A fulltag-misc-tagged pointer points 4 bytes past the header, so
it points to element 1 (rather than element 0).  This is fine because
element 0 of a code-vector is always the udf #0 sentinel value.
