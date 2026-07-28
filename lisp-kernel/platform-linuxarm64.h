/* SPDX-License-Identifier: Apache-2.0 */

/* PPC64 LINE-PORT (source: vendor/ccl/lisp-kernel/ppc-exceptions.c lane;
 * this header modeled line-for-line on Matt Emerson's
 * lisp-kernel/platform-linuxarm.h (ARM-family structure) and
 * lisp-kernel/platform-linuxppc64.h (64-bit values), tree pin d71a5ad.
 *
 * PORT-NOTE / deviations:
 *  - PLATFORM_CPU: RATIFIED by Matt's tree — lisp.h:74 defines
 *    PLATFORM_CPU_ARM64 (4<<3), and the arm64 backends stamp
 *    platform-cpu-arm64 into the image header (darwinarm64 backend @
 *    arm64-backend.lisp:229; our linuxarm64-backend-additions.lisp:36
 *    matches).  Kernel must agree or image.c rejects the image
 *    ("Heap image was saved for another platform", 16j).
 *  - MAXIMUM_MAPPABLE_MEMORY / IMAGE_BASE_ADDRESS: PROPOSED values taken
 *    from platform-linuxx8664.h:26-27 (the other 64-bit low-tag Linux
 *    port; aarch64-linux user VA space is 48-bit like x86-64, and Matt's
 *    low-tag scheme puts no constraint on the top byte).  Ratify.
 *  - xp accessor field layout: aarch64-linux
 *    arch/arm64/include/uapi/asm/sigcontext.h (fault_address; regs[31];
 *    sp; pc; pstate).  Accessor idiom consulted from our high-tag
 *    lisp-kernel/platform-linuxarm64.h (ucontext shapes only; no tag
 *    logic lives here).
 */

#define WORD_SIZE 64
#define PLATFORM_OS PLATFORM_OS_LINUX
#define PLATFORM_CPU PLATFORM_CPU_ARM64    /* see PORT-NOTE above */
#define PLATFORM_WORD_SIZE PLATFORM_WORD_SIZE_64

#include <ucontext.h>

typedef ucontext_t ExceptionInformation;   /* platform-linuxarm.h:31 */

/* PROPOSED (platform-linuxx8664.h:26-27) — ratify */
#define MAXIMUM_MAPPABLE_MEMORY (512L<<30L)
#define IMAGE_BASE_ADDRESS 0x300000000000L

#include "lisptypes.h"
/* arm64-constants.h #includes constants.h itself since f067047 (the fix
 * we'd PROPOSED here landed upstream). */
#include "arm64-constants.h"               /* platform-linuxarm.h:36 pattern */

/* PROPOSED additions to arm64-constants.h / area.h / threads.h (their
 * per-arch #ifdef ladders have no ARM64 branch yet; values cited): */
#ifndef TCR_BIAS
#define TCR_BIAS (0)                       /* = PPC64/ARM32 (ppc-constants64.h:244) */
#endif
#ifndef heap_segment_size
#define heap_segment_size 0x00020000L      /* = PPC64 (ppc-constants64.h:305-306) */
#define log2_heap_segment_size 17L
#endif
/* CS_OVERFLOW_FORCE_LIMIT: now provided by his area.h arm64 branch
   (-(sizeof(lisp_frame)), = our -32).  RECONCILED 556aebe8: dropped. */
#ifndef STATIC_BASE_ADDRESS
#define STATIC_BASE_ADDRESS 0x00012000     /* = X86 shape (x86-constants.h:74):
                                              one page under canonical nil
                                              (0x13000, arm64-arch.lisp:184) */
#endif
/* lisp_globals.h grew a real ARM64 branch @ 93d72a0 (nil-anchored:
   nil_value = the runtime lisp_nil, set by set_nil() at image-load
   time) -- the fixed-address shims that lived here are retired.
   BOOT-CHECK: his nrs base (nil-fulltag+dnode) vs the xload two-cons
   pun layout is unverified geometry; if T/nrs reads come back garbage,
   probe here first. */
/* pmcl-kernel.c:1461-1478's per-platform ladder has no LINUX+ARM64 arm;
   aarch64 support entered mainline in Linux 3.7. */
#ifndef min_os_version
#define min_os_version "3.7"
#endif
/* C alias for the unbound marker (ARM32 precedent, arm-constants.h:247;
   Matt's constants define unbound_marker but not the short name
   pmcl-kernel.c:2583 uses). */
#ifndef unbound
#define unbound unbound_marker
#endif
#ifndef slot_unbound
#define slot_unbound slot_unbound_marker   /* ARM32 precedent (arm-constants.h:251) */
#endif
/* stack-consed-object marker (albt.c stack walker; ARM32:
   arm-constants.h:245 SUBTAG(fulltag_imm,1)).  PROPOSED value in Matt's
   imm_1 subtag space, next free after lisp_frame_marker(5). */
#ifndef stack_alloc_marker
#define stack_alloc_marker SUBTAG(fulltag_imm_1, 6)
#endif
/* Image ABI version (image.c): fresh number for the new target; ARM32
   uses 1045, PPC64 1040.  PROPOSED: 1046 (next free). */
#ifndef ABI_VERSION_CURRENT
#define ABI_VERSION_MIN 1046
#define ABI_VERSION_CURRENT 1046
#define ABI_VERSION_MAX 1046
#endif
/* C-side `struct lisp_frame` is now provided by his arm64-constants.h C
   section (RECONCILED 556aebe8: our duplicate typedef dropped).  He does
   NOT define lisp_frame_size, so keep that (ours-only). */
#ifndef lisp_frame_size
#define lisp_frame_size sizeof(lisp_frame)
#endif

/* fixnum_bitmask: 64-bit form (ppc-constants64.h:237 / x86-constants64.h:272). */
#ifndef fixnum_bitmask
#define fixnum_bitmask(n)  (1LL<<((n)+fixnumshift))
#endif
/* NSAVEREGS: save0-save3 (thread_manager.c:1885-1898 ladder; PPC 8, ARM32 4). */
#ifndef NSAVEREGS
#define NSAVEREGS 4
#endif
/* tcr->single_float_convert.tag (thread_manager.c:1293) wants a boxed
   single-float subtag; singles are IMMEDIATE (fulltag_single_float) in
   this design, so the convert box is vestigial here.  Alias for the
   compile; RATIFY (likely the field or the store goes away). */
#ifndef subtag_single_float
#define subtag_single_float fulltag_single_float
#endif
/* gc.h / macros.h arch ladders have no ARM64 branch — the shims the gc
   draft carries file-locally, hoisted so gc-common.c/image.c see REAL
   definitions instead of implicit decls (would otherwise fail at link).
   arm64 nodeheader fulltags = {nodeheader_0, nodeheader_1}; immheader =
   {immheader_0, immheader_1, immheader_2} (arm64-constants.h:120-135);
   node fulltags = cons|misc|symbol (fulltag_function removed, patch 0055). */
/* nodeheader_tag_p / IMMHEADER_MASK / immheader_tag_p: now provided by his
   macros.h arm64 branch (RECONCILED 556aebe8: our forward-fills dropped). */
#ifndef is_node_fulltag
#define is_node_fulltag(f)  ((1<<(f))&((1<<fulltag_cons)     |                                        (1<<fulltag_misc)     |                                        (1<<fulltag_symbol)))
#endif

/* AArch64 instructions are 32 bits wide; `pc' points at one.
 * (arm-exceptions.h:120 `typedef u_int32_t opcode, *pc;' — ARM64-DEVIATION:
 * hoisted into the platform header because arm64-exceptions.h does not yet
 * define it and the gc also needs it.) */
typedef uint32_t opcode, *pc;

/* xp accessors — aarch64-linux mcontext_t:
 *   struct sigcontext { __u64 fault_address; __u64 regs[31];
 *                       __u64 sp; __u64 pc; __u64 pstate; ... };
 * Same accessor shapes as platform-linuxarm.h:38-46, 64-bit fields. */
#define xpGPRvector(x) ((natural *)&((x)->uc_mcontext.regs[0]))
#define xpGPR(x,gprno) (xpGPRvector(x))[gprno]
#define set_xpGPR(x,gpr,new) xpGPR((x),(gpr)) = (natural)(new)
#define xpPC(x) (*((pc*)(&((x)->uc_mcontext.pc))))
#define set_xpPC(x,new) (xpPC(x) = (pc)(new))
#define xpLR(x) (*((pc*)(&(xpGPR(x,30)))))  /* x30 = LR */
#define xpSP(x) (*((natural*)(&((x)->uc_mcontext.sp))))
#define xpPSR(x) ((x)->uc_mcontext.pstate)
#define xpFaultAddress(x) ((x)->uc_mcontext.fault_address)

#define DarwinSigReturn(context)            /* platform-linuxarm.h:48-49 */
#define SIGRETURN(context)

#include "os-linux.h"
