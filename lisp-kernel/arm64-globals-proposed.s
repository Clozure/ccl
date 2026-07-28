/* SPDX-License-Identifier: Apache-2.0 */

/* ARM64-SPECIFIC — justification: access-macro design for AArch64 (no cheap
   absolute addressing => rnil-relative); offsets are NOT invented — they
   mirror the arch-independent lisp-kernel/lisp_globals.h indices and the
   shared vendor lisp_globals.s nrs order, both present in Matt's tree. */

/*
 * PROPOSED (ratify with Matt): ARM64 lisp_globals / nrs anchoring idiom.
 *
 * Matt's arm64-constants.s ALREADY includes the shared generator
 * (vendor lisp_globals.s via `include(lisp_globals.s)' with
 * lisp_globals_limit = -node_size and nrs_origin = node_size), so the
 * SYMBOLIC layout exists in his m4 layer.  This file provides:
 *   (a) cpp/GNU-as equates for the subset our spentry drafts reference,
 *       mirroring the canonical indices in lisp-kernel/lisp_globals.h
 *       (which is arch-independent and present verbatim in his tree), and
 *   (b) the AArch64 ACCESS MACROS: everything is rnil-relative.  rnil
 *       holds nil_value = <static nil address> + fulltag_nil, and the
 *       kernel globals live at negative node offsets below the untagged
 *       nil address, exactly as on every other CCL target.  ARM64 has no
 *       cheap absolute addressing, but rnil is pinned - so:
 *           global N   is at   [rnil - (N*node_size + fulltag_nil)]
 *       (the -fulltag_nil bias makes the address 8-aligned).
 *
 * RATIFY-GEOMETRY: for nrs, Matt's placeholder `nrs_origin = node_size'
 * is INCONSISTENT with his own canonical-t-value: with
 * canonical-nil = #x13000+fulltag_nil and canonical-t = #x13020+
 * fulltag_symbol (arm64-arch.lisp:184-185), T's symbol object begins at
 * untagged-nil + 32, i.e. nrs_origin MUST be 32 (and nrs_symbol_fulltag
 * must be fulltag_symbol) for nrs.tsym to BE canonical T.  The equates
 * below use origin 32; then  rnil + t_offset == rnil + nrs.tsym  holds
 * identically (28 = 32 + 7 - 11).
 */

/* ---- kernel globals (indices: lisp-kernel/lisp_globals.h) ---- */
.set lisp_globals.get_tcr,              (-(1*node_size)  - fulltag_nil)
.set lisp_globals.ret1valn,             (-(9*node_size)  - fulltag_nil)
.set lisp_globals.ret1val_addr,         lisp_globals.ret1valn
.set lisp_globals.refbits,              (-(17*node_size) - fulltag_nil)
.set lisp_globals.oldspace_dnode_count, (-(18*node_size) - fulltag_nil)
.set lisp_globals.all_areas,            (-(29*node_size) - fulltag_nil)
.set lisp_globals.lexpr_return,         (-(30*node_size) - fulltag_nil)
.set lisp_globals.lexpr_return1v,       (-(31*node_size) - fulltag_nil)
.set lisp_globals.ref_base,             (-(41*node_size) - fulltag_nil)
.set lisp_globals.ephemeral_refidx,     (-(52*node_size) - fulltag_nil)

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

/* ---- nil-relative symbols (order: vendor lisp_globals.s _struct(nrs)) ----
   nrs.<name> = tagged symbol, as an offset from rnil:
   32 + k*symbol.size + fulltag_symbol - fulltag_nil = 64k + 28. */
.set nrs_symbol_size, 64
.macro def_nrs name, k
.set nrs.\name, (32 + (\k)*nrs_symbol_size + fulltag_symbol - fulltag_nil)
.endm
        def_nrs tsym,               0
        def_nrs nilsym,             1
        def_nrs errdisp,            2
        def_nrs cmain,              3
        def_nrs eval,               4
        def_nrs appevalfn,          5
        def_nrs error,              6
        def_nrs defun,              7
        def_nrs defvar,             8
        def_nrs defconstant,        9
        def_nrs macrosym,          10
        def_nrs kernelrestart,     11
        def_nrs package,           12
        def_nrs total_bytes_freed, 13
        def_nrs kallowotherkeys,   14
        def_nrs toplcatch,         15
        def_nrs toplfunc,          16
        def_nrs callbacks,         17
        def_nrs restore_lisp_pointers, 18
        def_nrs total_gc_microseconds, 19
        def_nrs builtin_functions, 20
        def_nrs udf,               21
        def_nrs init_misc,         22
        def_nrs macro_code,        23
        def_nrs closure_code,      24
        def_nrs new_gcable_ptr,    25

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
/* PROPOSED: sp as a uuo register operand (stack-overflow traps trap on
   sp, which is not in Matt's R* list). */
.set Rsp, 31

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
