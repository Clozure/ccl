/* SPDX-License-Identifier: Apache-2.0 */

/* ARM64-SPECIFIC — rnil-relative ACCESS MACROS for the kernel globals and
   nil-relative symbols, plus a PROPOSED uuo extension.  Used by the
   spentry-A..E drafts; ratify with Matt.

   THE GEOMETRY IS NOT DEFINED HERE.  nrs.*, lisp_globals.* and t_offset
   come from Matt's lisp-kernel/arm64-lisp-globals.s (already in every
   spentry unit via arm64-macros.s -> #include "arm64-lisp-globals.s"),
   as corrected by patches/0104-arm64-lisp-globals-nrs-origin-geometry.patch
   and pinned there by assemble-time .error guards to the compiler's
   canonical-nil-value / canonical-t-value (arm64-arch.lisp:245-246:
   NIL = #x13000 + fulltag_nil, T = #x13020 + fulltag_symbol, so
   nrs.tsym = t_offset = 28 and lisp_globals.get_tcr = -19, rnil-relative).

   16m62: this file used to REDEFINE that whole namespace after his file
   had already defined it — a silent last-.set-wins shadow
   (comms/GLOBALS-SHADOW-16m62.md; corrections.md
   local_dotset_shadows_included_kernel_constant).  Do not add value
   equates for nrs.* / lisp_globals.* / t_offset here again: one drifted
   copy moves T for the assembler while the image, compiler and C runtime
   stay put, with no diagnostic. */

/* Load the VALUE of a kernel global.  The offsets range past ldur's
   +-256, so form the address first (sub imm12 covers all indices). */
.macro ref_global reg, glob
        sub \reg, rnil, #(0 - (lisp_globals.\glob))
        ldr \reg, [\reg]
.endm
/* Address of a kernel global (for stores / atomics). */
.macro ref_global_addr reg, glob
        sub \reg, rnil, #(0 - (lisp_globals.\glob))
.endm

/* symbol slot offsets via the dedicated symbol fulltag (own names to
   avoid colliding with per-file symbol.* equates). */
.set nrs_sym.vcell, (2*node_size - fulltag_symbol)
.set nrs_sym.fcell, (3*node_size - fulltag_symbol)

/* ---- PROPOSED uuo extension (goes with Matt's arm64-uuo.s @ c9e7ffb) ----
   PPC's uuo_interr(errnum, reg): an error trap carrying an errors.s errnum
   plus one register.  Matt's misc format (uuo_format_misc = 0 after the
   c9e7ffb renumber; misc info must not be all-0) has 14 bits of info with
   only values 1-8 assigned; we claim info bit 13 as the "interr" flag:
   bits 12:5 = errnum (8 bits), bits 4:0 = register number.  Used for
   stack-overflow (trllt), too-many-values, object-not-list -- errnum-
   carrying traps with no slot in his sketch.  RATIFY. */
.macro uuo_interr errnum, reg
        udf #((((1<<13) | ((\errnum)<<5) | (R\reg)) << 2) | uuo_format_misc)
.endm
/* (Rsp as a uuo register operand comes from Matt's arm64-constants.h:120
   DEFCONST(Rsp, 31) — formerly proposed here, now upstream.) */

/* The tagged symbol itself. */
.macro ref_nrs_symbol reg, sym
        add \reg, rnil, #nrs.\sym
.endm
/* The symbol's global value (vcell contents). */
.macro ref_nrs_value reg, sym
        add \reg, rnil, #nrs.\sym
        ldur \reg, [\reg, #nrs_sym.vcell]
.endm
/* The symbol's function cell contents. */
.macro ref_nrs_function reg, sym
        add \reg, rnil, #nrs.\sym
        ldur \reg, [\reg, #nrs_sym.fcell]
.endm
