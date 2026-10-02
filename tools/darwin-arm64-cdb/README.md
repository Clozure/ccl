# Darwin/arm64 CDB regeneration helpers

Scripts live here because `*headers*` / `darwin-arm64-headers/` are
**gitignored**.

## Prerequisites

* Built `ccl-ffigen/ffigen5` (`Makefile.darwin` + `include/clang-c`)
* Current MacOSX.sdk (`xcrun --show-sdk-path`)

## FFI filter (required)

`h-to-ffi.sh` runs `filter-ffi.py` after ffigen5:

* drops **Availability\*** / **ptrcheck.h** macros (circular expand →
  control-stack overflow in `process-defined-macros`)
* drops `(function …)` forms containing `(null)` (unmapped clang kinds;
  patched ffigen5 maps Half/Float16 → float)
* `FILTER_FFI_MACROS=all|frameworks|none|default` — cocoa uses
  `frameworks` (keep `/Frameworks/` macros only)

`library/parse-ffi.lisp` also skips functions that still fail type
reference (handler-case), treats leaked `:null` as void, and marks
unevaluable macros `:pending` so Cocoa-scale .ffi does not re-eval
forever.

## libc (including math.h)

```sh
mkdir -p /tmp/libc-cdb-backup
cp $CCL/darwin-arm64-headers/libc/*.cdb /tmp/libc-cdb-backup/

cd $CCL/darwin-arm64-headers/libc/C
$CCL/tools/darwin-arm64-cdb/libc-populate.sh

cd $CCL
./darm64cl --stack-size 16M --thread-stack-size 16M --no-init --batch \
  < tools/darwin-arm64-cdb/parse-libc.lisp
```

Smoke: `tools/darwin-math-smoke.lisp`, `tools/darwin-cdb-stat-smoke.lisp`.

## Cocoa (ObjC)

Must use `-x objective-c` (`FFIGEN_LANG`) — `-x c` yields **0**
objc-classes.

```sh
mkdir -p /tmp/cocoa-cdb-backup
cp $CCL/darwin-arm64-headers/cocoa/*.cdb /tmp/cocoa-cdb-backup/

cd $CCL/darwin-arm64-headers/cocoa/C
$CCL/tools/darwin-arm64-cdb/cocoa-populate.sh
# → objc runtime + Foundation + AppKit; FILTER_FFI_MACROS=frameworks

cd $CCL
./darm64cl --stack-size 16M --thread-stack-size 16M --no-init --batch \
  < tools/darwin-arm64-cdb/parse-cocoa.lisp
```

Smoke: `tools/darwin-cocoa-smoke.lisp` (~600+ classes / ~10k+ methods).
`#$NSOffState` / `NSControlStateValue*`: `tools/darwin-nsoffstate-smoke.lisp`.
Full IDE: `tools/darwin-require-cocoa-smoke.lisp` (after shims below).

### Cocoa constant shims

`cocoa-populate.sh` installs `objc-bool-constants.ffi` (and related
`zzz-*.ffi`).  After parse, or surgically on an existing tree:

```sh
./darm64cl --no-init --batch \
  < tools/darwin-arm64-cdb/inject-objc-bool-constants.lisp
```

Adds: `YES`/`NO`, deprecated `NS*KeyMask` → `NSEventModifierFlag*`
values, `NSControlStateValue*` / `NSOffState` / `NSOnState` /
`NSMixedState`, and `FLT_MAX`/`FLT_MIN`/`DBL_MAX` (modern SDK leaves
these as unlinkable statics / macros).

### libc computed-macro shims

Macros that expand through casts or `sizeof` (`FIONBIO`,
`HOST_BASIC_INFO_COUNT`, `CPU_TYPE_*`) cannot be reduced by the
translator's macro evaluator and are omitted from a fresh
`libc/constants.cdb`; `library/sockets.lisp` and
`level-1/linux-files.lisp` need them:

```sh
./darm64cl --no-init --batch \
  < tools/darwin-arm64-cdb/inject-libc-computed-constants.lisp
```

## gl / carbon / quartz / quartzcore

| Dir | Lang | Populate | Headers |
|-----|------|----------|---------|
| **gl** | C | `gl-populate.sh` | OpenGL.h, glu.h, glut.h (AGL skipped if missing) |
| **carbon** | C | `carbon-populate.sh` | Carbon.h (`-Wno-deprecated-declarations`) |
| **quartz** | ObjC | `quartz-populate.sh` | Quartz.h (`FILTER_FFI_MACROS=frameworks`) |
| **quartzcore** | ObjC | `quartzcore-populate.sh` | QuartzCore.h (`FILTER_FFI_MACROS=frameworks`) |

```sh
IFACE=gl   # or carbon|quartz|quartzcore
mkdir -p /tmp/${IFACE}-cdb-backup
cp $CCL/darwin-arm64-headers/${IFACE}/*.cdb /tmp/${IFACE}-cdb-backup/

cd $CCL/darwin-arm64-headers/${IFACE}/C
$CCL/tools/darwin-arm64-cdb/${IFACE}-populate.sh

cd $CCL
CCL_INTERFACE=$IFACE ./darm64cl --stack-size 16M --thread-stack-size 16M \
  --no-init --batch < tools/darwin-arm64-cdb/parse-interface-dir.lisp
```

## Notes

* `parse-standard-ffi-files` replaces `*.cdb` in place.
* See `doc/porting/darwin-cdb.md`.
