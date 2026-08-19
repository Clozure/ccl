# Darwin (macOS)

On arm64, macOS prevents us from having a static area at a fixed address.
On x86-64, we used the `pagezero_size` linker option to reserve some low
memory, but this no longer works.

> From https://forums.developer.apple.com/forums/thread/655950
> 
> "Modifying pagezero_size isn't a supportable option in the arm64
> environment. arm64 code must be in an ASLR binary, which using a
> custom pagezero_size is incompatible with. An ASLR binary encodes
> signed pointers using a large random size along with the expected
> page zero size, and this combination is going to extend beyond the
> range of values covered in the lower 32-bits. Further, even if
> that did work, 32-bit pointers are completely incompatible with
> the arm64e architecture, which is available as a preview
> technology.

On an arm64 Mac, building with something like
`cc -Wl,-pagezero_size,0x4000 -g foo.c`, seems to work, but it produces
a binary that won't run: "error: Malformed Mach-o file" is what
the debugger prints out.

On an Intel Mac, that same `cc -Wl,-pagezero_size,0x4000 -g foo.c`
does produce a working binary.

On other ports, `nil` is basically a really popular constant, and when
treated as a pointer, it refers to a fixed address in low-ish memory.
On an Apple Silicon Mac, it looks like we're going to have to keep `nil` in
a register (rnil) because we can't rely on having a fixed address for it.
This means we will access kernel globals and `nil`-relative symbols as
offsets from rnil.

## ARM ABI
Official ARM documentation: https://github.com/ARM-software/abi-aa

As permitted by the standard 64-bit ARM ABI, Apple reserves register x18.
(Linux apparently doesn't.)

The architectural stack pointer SP must be 16-byte aligned whenever it is
used to access memory.  This is hardware-enforced.
As a matter of ABI policy, SP must be 16-byte aligned at public
C function boundaries.

For example, here is a way to lose:
```
str x1, [sp, #-8]! ;OK, but sp now has only 8 byte alignment...
str x0, [sp, #-8]! ;... so this subsequent store fails
```

## MAP_JIT and the W^X policy
On arm64, macOS enforces a policy called W^X.  This means that a memory
region can be either writable or executable, but never both at the same
time.

Apple recognizes that this policy affects dynamic languages.  They
document their accomodation for this at
https://developer.apple.com/documentation/apple-silicon/porting-just-in-time-compilers-to-apple-silicon.

In short, you are supposed to call `mmap` with the `MAP_JIT` flags to
allocate a distinguished region of memory.  Threads can then call
`pthread_jit_write_protect_np` to enable and disable write access.
Note that this operates on a per-thread basis.
Officially, only a single `MAP_JIT` region is supported.

CCL has traditionally managed a single dynamic memory area that contains
code and other data.  Given the W^X policy, I see no alternative to a separate
memory area just for code-vector objects.

* Allocate code with `mmap(..., PROT_READ|PROT_WRITE|PROT_EXEC,
  MAP_PRIVATE|MAP_ANON|MAP_JIT, ...)`.  Despite RWX in the call, the
  region is **not** simultaneously writable and executable.
* Toggle with `pthread_jit_write_protect_np(0)` (write / not exec) and
  `pthread_jit_write_protect_np(1)` (exec / not write); then
  `sys_icache_invalidate`.
* Hardened Runtime needs `com.apple.security.cs.allow-jit` (or
  `allow-unsigned-executable-memory`).  Newer hardening:
  `jit-write-allowlist` → must use `pthread_jit_write_with_callback_np`
  instead of the toggle API.
* Officially **one** `MAP_JIT` region per process under Hardened Runtime
  (practice varies; V8 registers multiple pages via ThreadIsolation).
* `MAP_JIT|MAP_FIXED` is rejected (EINVAL) — cannot pin the JIT region
  at `IMAGE_BASE_ADDRESS`.
* File-backed `mmap`+`MAP_FIXED` with a **nonzero file offset** also
  fails with EINVAL on arm64 Darwin; image load uses anon commit +
  `read` instead (same idea as the Windows `MapFile` path).
* `mrs ctr_el0` for I-cache line size SIGILLs at EL0 on Apple Silicon;
  use `sys_icache_invalidate` instead.

### Cross-runtime survey (not Lisp-only)

| Runtime | Pattern | Takeaway for CCL |
| --- | --- | --- |
| **LuaJIT** (`lj_mcode.c`, `LUAJIT_ENABLE_OSX_HRT`) | Dedicated mcode arena: `MAP_JIT` + `pthread_jit_write_protect_np` around emit; `mprotect` still used in some paths. Hardened Runtime without MAP_JIT → `SIGKILL` / codesign invalid page. | Code-only arena; never MAP_JIT the GC heap. |
| **V8** (`code-memory-access.h`) | APRR/`MAP_JIT` scope: thread defaults RX; `WritableJitAllocation` flips WP for the write window; ThreadIsolation validates writes. Assumes `allow-jit`. | Same: scoped WP around compile, not heap-wide. |
| **JSC / BrowserEngineKit** | `be_memory_inline_jit_restrict_rwx_to_{rw,rx}_with_witness`; arm64e PAC on JIT pointers; write API must be **inlined** into the compiler critical section. | Callback/allowlist path is the long-term Apple direction. |
| **SpiderMonkey / Wasmtime** | Same MAP_JIT + protect/callback; Wasmtime discussion: only mark **code** maps MAP_JIT, not general mmap. | Reinforces separate `AREA_CODE`. |
| **CPython copy-and-patch JIT** (`Python/jit.c`) | `MAP_JIT`+`PROT_EXEC` at alloc; `pthread_jit_write_protect_np(0)` around stitch, `(1)` before mark-exec; skip `mprotect` when MAP_JIT. | Smallest “real” JIT — good template for CCL’s code-vector writer. |
| **PyPy / RPython** (`rlib/rmmap.py`) | `pthread_jit_write_protect_np` **only** on darwin+arm64 (x86 Darwin must not bind the symbol on older macOS). | Gate on `darwin && arm64`, not bare Darwin. |

Common failure modes mirrored across all of them: (1) MAP_JIT without WP
toggle under Hardened Runtime → killed; (2) WP on a mixed code+data
region → store while RX / exec while RW; (3) forgetting `sys_icache_invalidate`;
(4) entitlements missing when codesigned with `--options runtime`.

CCL traditionally mixes code and data in one dynamic area.  That fights
per-thread WP on a single MAP_JIT region (a store from RX code into the
same region needs RW).  Upstream direction (see Clozure/ccl#11 discussion):
a separate code area / code-vector slot (SBCL-style), not “MAP_JIT the
whole heap”.

Do **not** `mprotect` the whole dynamic area RX after image load.  Boot
code runs from pure/readonly (RX is fine there) and from kernel
subprims; heap stores (`SPgvset`, cons init) must keep dynamic RW.  With
dynamic RX, `handle_alloc_trap` succeeds then `SPgvset` takes
`EXC_BAD_ACCESS` and cold-load misreports a nested “read” fault (Darwin
`si_code` ≠ Linux `SEGV_ACCERR` — use ESR.WnR).

### Darwin arm64 executable model

`:purify t` copies MAP_JIT / heap code into `AREA_READONLY` (RX).
Dynamic heap is never executable.  Runtime compile + fasl code-vectors
use a MAP_JIT arena (AREA_CODE stand-in).  Stock path:
`(rebuild-ccl :full t)`.  Details: `doc/porting/progress.md`.  Smokes:
`tools/with-timeout` / `tools/run-darwin-smoke.sh` (exit 124 on timeout).

**Boot path:** map heap RW → fill → purify / `mprotect` RX on pure.
**Runtime compile / fasl load:** MAP_JIT via `%allocate-code-vector`
(`level-0/ARM64/arm64-utils.lisp`); WP only in kernel C
(`darwin_arm64_jit_*`).  On native Darwin, `compile-file` always
allocates MAP_JIT (eval-when `:compile-toplevel` must run while the
heap is NX).

**Dirtying `AREA_READONLY` under W^X:** stock ports `UnProtectMemory` →
RWX so a store into pure leaves the page executable.  Darwin cannot
RWX — `UnProtectMemory` is RW-only.  Kernel oscillates per fault:
write → `mprotect(RW)`; later NX fetch on that page → `mprotect(RX)`.
Do **not** treat NX in `AREA_READONLY` as “non-code heap” FATAL
(FATAL NX = execute from RW **dynamic** heap).

Modern Apple docs push `pthread_jit_write_with_callback_np` + allowlist
(`jit-write-allowlist`); optional later hardening once a first-class
code area exists.

### UUO / SIGILL / Mach exceptions

CCL UUOs are `udf #n` → hardware `EXC_BAD_INSTRUCTION` → Mach exception
port (preferred) or BSD `SIGILL` (XNU `ux_exception.c`).

* **Mach path (current Darwin arm64):** `use_mach_exception_handling = true`.
  `arm64-darwin-mach.c` ports the darwinx8664 server: associate TCR with
  the thread exception port, `mach_exc_server` MIG demux, synthetic
  ucontext → `signal_handler`, return via faulting `pseudo_sigreturn`
  (`udf #0`).  Message buffer is **8192** bytes — ARM64 `THREAD_STATE64`
  messages exceed the historical x86 256-byte `mach_msg_server` limit.
* **lldb:** stops on Mach exception; for Unix-fallback debugging use
  `settings set platform.plugin.darwin.ignored-exceptions EXC_BAD_INSTRUCTION`
  (LLVM/lldb pattern).
* **Unix fallback:** accessors must be Darwin form `uc_mcontext->__ss.__pc`
  / `__x[]` / `__es.__far`, not Linux `mcontext.regs`.
* **Nested fault:** if the handler reads a bad PC/context, cold load
  reports “unhandled read fault” instead of the UUO.

### Fixed addresses / ASLR / rnil

* `-pagezero_size` is unsupported on arm64 (malformed Mach-O / ASLR).
* **Current:** provisional high FIXED RW bases (`STATIC_BASE` /
  `IMAGE_BASE` below).  Code already uses **rnil (x23)** for nil-relative
  *access* to globals/NRS; that is **not** the same as ASLR-relocatable
  statics.  True rnil-relative statics = allocate static/NRS at an
  ASLR-chosen VA, stop baking absolute nil into images/xload/compiler,
  relocate C `STATIC_BASE_ADDRESS` / image headers — a large milestone,
  not a header tweak.
* `MAP_JIT|MAP_FIXED` → EINVAL: cannot pin JIT at `IMAGE_BASE`.
  Floating `MAP_JIT` + reloc.

### Interface `.cdb` databases

* Backend expects `ccl:darwin-arm64-headers;`.  The tree is **gitignored**
  (`/*headers/`) like every other `*headers*` directory.
* **libc** regenerated for arm64 (`tools/darwin-arm64-cdb/libc-populate.sh`,
  current MacOSX.sdk, `-arch arm64`), including **math.h** via
  `filter-ffi.py` (Availability macros + `(null)` / Half types).
* **cocoa** regenerated as ObjC (`FFIGEN_LANG=objective-c`, Foundation +
  AppKit; `:pending` macro parse).  See `doc/porting/darwin-cdb.md`.

### Apple AAPCS64 FFI

* FTD already has `:signed-char t` and `:natural-alignment t`.
* Fixed-arity `ff-call` / `foreign-symbol-address` works under Mach.
* **`_SPffcall` stack args (GPR 9+):** done. Before the `blr`, if the
  c_frame has words above the 8 GP saves, SP advances to
  `c_frame.params+8*node_size` so overflow args sit at the callee's
  incoming SP. Restore state (lr, savedsp, enclosing
  `last_lisp_frame`) is parked on the **vstack** — never re-read from
  the c_frame after return (callee frames clobber below SP). When
  bumping, `last_lisp_frame` is the boundary lisp_frame (above the new
  SP), not the c_frame base.
* **Darwin variadic-on-stack:** done. `%external-call-expander` emits a
  `:variadic` sentinel at the CDB `:void` boundary; `aapcs64-ff-call`
  forces following args onto 8-byte stack slots (Apple ABI). Linux
  ignores the sentinel. Smoke: `tools/darwin-variadic-smoke.lisp`.
* **Darwin natural-size packing (non-variadic overflow):** done.
  `aapcs64-ff-call` packs stack overflow by natural size/alignment
  (char@0, short@2, int@4) on `:darwinarm64`; Linux keeps 8-byte slots.
  Vinsns: `set-c-arg-{byte,halfword,fullword,doubleword-bytes}`.
  Smoke: `tools/darwin-pack-overflow-smoke.lisp`.

### Smaller Darwin arm64 landmines (status)

* **16 KiB pages:** derive `log2_page_size` from `sysconf`; round guards;
  keep **image file** seeks at 4 KiB; short `read` on MapFile OK.
* **File `mmap`+`MAP_FIXED`+nonzero offset:** EINVAL → anon + `read`.
* **`mrs ctr_el0`:** SIGILL → `sys_icache_invalidate`.
* **x18 reserved** (Apple AAPCS64); **SP 16-byte** hardware-aligned.
* **PAC:** strip only if ever on arm64e; plain arm64 PCs are fine.

## Current Darwin arm64 bring-up bases

Provisional fixed bases that `mmap` FIXED RW accepts (low addresses do
not):

* `STATIC_BASE_ADDRESS` = `#x200000000`
* `IMAGE_BASE_ADDRESS` = `#x300000000000`

Nil = static + 4 KiB + `fulltag_nil` (`#x20000100b`).  Longer term this
should move to rnil-relative addressing without fixed static VA.

## Enhanced Security / MIE readiness (macOS 26)

Memory Integrity Enforcement (EMTE tagging, kernel + ~70 system
processes) is always-on **hardware** on A19/M5+ and opt-in per app via
`com.apple.security.hardened-process.*` entitlements; there is no user
toggle.  Measured on macOS 26.6.2 / M4 by re-signing `darm64cl` with
those entitlements (`tools/darwin-fixed-mmap-probe.c` bisects VA
policy per entitlement):

| Entitlement | Effect on this port |
|---|---|
| `hardened-process` + `enhanced-security-version` 2 | boots; no VA change |
| `+ hardened-heap` (guard objects, xzone) | boots; GC stress clean |
| `+ dyld-ro` | **breaks**: `MAP_FIXED` RW at `#x200000000` → EPERM; image load fails at `AREA_STATIC` |
| `+ platform-restrictions` 2 | **killed at exec** (ad-hoc signature; Mach IPC hardening unprobed) |

Consequences:

* The fixed `STATIC_BASE_ADDRESS` is the single point of failure —
  dyld's read-only state region owns that VA in hardened processes.
  `#x400000000` still maps, but address whack-a-mole is fragile
  (`#x1000000000` is denied even unhardened).  The durable fix is
  relocatable statics: bias `AREA_STATIC` references like the dynamic
  area's `image_base` bias (nil already derives from the mapped
  section's actual `a->low` in `load_openmcl_image`).
* EMTE tag checks apply to secure-allocator (`malloc`) memory, not raw
  anonymous `mmap`; the lisp heap is untagged either way.  FFI keeps
  full 64-bit pointers (tag byte intact).  Audit deliberate
  out-of-bounds reads in FFI/string paths before enabling
  `checked-allocations` on M5 hardware (`…soft-mode` gives simulated
  crashes for auditing).
* Mach exception ports may conflict with platform-restrictions Mach
  IPC hardening; the Unix-signal fallback path
  (`DARWIN_USE_PSEUDO_SIGRETURN`) is the escape hatch to validate on
  MIE hardware.
* `load_image_section` / `CommitMemory` / `MapFile` failures now name
  the syscall, VA, length, and errno — a bare
  "Couldn't load lisp heap image: Invalid argument" on a newer
  machine is this policy family until proven otherwise.
