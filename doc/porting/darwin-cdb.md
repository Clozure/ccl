# Darwin arm64 interface databases (`.cdb`)

## Status

`ccl:darwin-arm64-headers;` is **gitignored**.

| Module | Status |
|--------|--------|
| **libc** | Regenerated arm64 (current MacOSX.sdk), includes **math.h** |
| **cocoa** | Regenerated arm64 ObjC: Foundation + AppKit (+ objc runtime) |
| **gl** | Regenerated arm64 (OpenGL + GLU + GLUT; AGL absent from SDK) |
| **carbon** | Regenerated arm64 (Carbon.h umbrella) |
| **quartz** | Regenerated arm64 ObjC (Quartz.h umbrella) |
| **quartzcore** | Regenerated arm64 ObjC (QuartzCore.h; ~340 objc-classes) |
| other | Still x86 bring-up copies |

Helpers: `tools/darwin-arm64-cdb/`. Needs a sibling checkout of
[Clozure/ccl-ffigen](https://github.com/Clozure/ccl-ffigen). Local
ffigen5 patch maps `CXType_Half`/`Float16` → `float`.

## math.h fix

Apple `math.ffi` pulled in ~1.4k Availability macros; expanding them
stack-overflowed `process-defined-macros`. `__fp16` intrinsics became
`(null ())` (unmapped clang kinds).

Fix: `filter-ffi.py` (drop Availability/ptrcheck macros + `(null)`
functions) wired into `h-to-ffi.sh`; `parse-ffi.lisp` skips bad
functions; ffigen5 Half/Float16 → float.

Smoke: `tools/darwin-math-smoke.lisp` (`#_sin`/`#_cos`/`#_sqrt`).

## Cocoa fix

Bring-up used `-x c`, so cocoa CDBs had **0** `objc-class` forms.
Regen uses **`FFIGEN_LANG=objective-c`**, `-F…/Frameworks`, and
Foundation + AppKit (not only the Cocoa umbrella).

Full umbrellas + thousands of macros made `process-defined-macros`
look hung (re-eval every unevaluable macro each pass). Fixes:

* `process-defined-macros`: unevaluable → `:pending`; retry only after
  a pass that defines new constants
* `FILTER_FFI_MACROS=frameworks`: keep Frameworks/ macros only

Typical CDB (~current SDK): ~617 objc-classes, ~11304 objc-methods.
Smoke: `tools/darwin-cocoa-smoke.lisp` (CDB keys; no objc-bridge load).

### Cocoa bridge shims (modern SDK)

`tools/darwin-arm64-cdb/` injects into local cocoa CDBs (also via
`cocoa-populate.sh` `zzz-*.ffi`):

* `YES`/`NO` constants (FILTER_FFI_MACROS drops them)
* `objc_msgSend*` prototypes (`OBJC_OLD_DISPATCH_PROTOTYPES=0`)
* `instancetype` + generics → `id`; `va_list`; `NSConstantString` layout
* complete `struct id` (= `objc_object`) — ffigen emits `(struct-ref "id")`
  which otherwise installs an incomplete record and breaks `record-length`

Inject: `inject-objc-{bool-constants,msgsend-prototypes,bridge-types}.lisp`.
Bridge smoke: `tools/darwin-objc-bridge-smoke.lisp` (poll; may SIGBUS if raced).

## Regenerate libc

```sh
cd $CCL/darwin-arm64-headers/libc/C
$CCL/tools/darwin-arm64-cdb/libc-populate.sh
cd $CCL
./darm64cl --stack-size 16M --thread-stack-size 16M --no-init --batch \
  < tools/darwin-arm64-cdb/parse-libc.lisp
```

## Regenerate cocoa

```sh
mkdir -p /tmp/cocoa-cdb-backup
cp $CCL/darwin-arm64-headers/cocoa/*.cdb /tmp/cocoa-cdb-backup/

cd $CCL/darwin-arm64-headers/cocoa/C
$CCL/tools/darwin-arm64-cdb/cocoa-populate.sh

cd $CCL
./darm64cl --stack-size 16M --thread-stack-size 16M --no-init --batch \
  < tools/darwin-arm64-cdb/parse-cocoa.lisp
```

## Regenerate gl / carbon / quartz / quartzcore

Language mode:

* **gl**, **carbon** — C (`FFIGEN_LANG=c`)
* **quartz**, **quartzcore** — ObjC + `FILTER_FFI_MACROS=frameworks`

Parse uses shared `parse-interface-dir.lisp` with `CCL_INTERFACE=<dir>`.

```sh
# Example: gl (swap name for carbon|quartz|quartzcore)
IFACE=gl
mkdir -p /tmp/${IFACE}-cdb-backup
cp $CCL/darwin-arm64-headers/${IFACE}/*.cdb /tmp/${IFACE}-cdb-backup/

cd $CCL/darwin-arm64-headers/${IFACE}/C
$CCL/tools/darwin-arm64-cdb/${IFACE}-populate.sh

cd $CCL
CCL_INTERFACE=$IFACE ./darm64cl --stack-size 16M --thread-stack-size 16M \
  --no-init --batch < tools/darwin-arm64-cdb/parse-interface-dir.lisp
```

Notes:

* **AGL** is absent from modern SDKs; `gl-populate.sh` still lists it and
  `h-to-ffi.sh` skips missing headers.
* Carbon is deprecated; populate passes `-Wno-deprecated-declarations`.
* After arm64 gl regen, CDBs shrink vs x86 bring-up copies (those were
  oversized shared dumps): empty objc tables (~4608 B), focused CGL/GLU/GLUT
  constants/functions.
