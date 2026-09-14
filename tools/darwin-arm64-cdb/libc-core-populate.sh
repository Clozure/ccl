#!/bin/sh
# Core Darwin/arm64 libc headers → .ffi (Apple Silicon SDK, -arch arm64).
#
# Run from ccl:darwin-arm64-headers;libc;C; (or copy results there):
#   cd $CCL/darwin-arm64-headers/libc/C
#   $CCL/tools/darwin-arm64-cdb/libc-core-populate.sh
#
# Then under a darwinarm64 image:
#   (require "PARSE-FFI")
#   (parse-standard-ffi-files "libc")
#   ;; install new-*.cdb → *.cdb (see install-new-cdb.sh)
#
# Full historical populate.sh lists hundreds of 10.11-era headers; many are
# gone from modern SDKs. This core set is enough to validate arm64 layouts
# (struct stat, stdio, unistd) before a broader regen.

set -e
HERE=$(cd "$(dirname "$0")" && pwd)
PATH="${HERE}:$PATH"
export PATH

if [ -z "${SDK}" ]; then
  SDK=$(xcrun --show-sdk-path)
fi
if [ $# -eq 1 ]; then
  SDK=$1
fi
if [ ! -d "$SDK" ]; then
  echo "SDK not found: $SDK" >&2
  exit 1
fi

# Wipe previous SDK-mirror tree under C/ (ffigen writes ./Applications or ./Library…).
rm -rf Applications Library System usr

CFLAGS="-arch arm64 -isysroot ${SDK}"
# CLT/Xcode clang builtins (stddef/stdint) when SDK omits them
CLANG_INC=$(echo "$(xcrun --find clang)" | sed 's|/bin/clang$|/lib/clang|')
if [ -d "$CLANG_INC" ]; then
  VER=$(ls "$CLANG_INC" | tail -1)
  if [ -n "$VER" ] && [ -d "$CLANG_INC/$VER/include" ]; then
    CFLAGS="$CFLAGS -isystem $CLANG_INC/$VER/include"
  fi
fi
export CFLAGS SDK

h-to-ffi.sh "${SDK}/usr/include/errno.h"
h-to-ffi.sh "${SDK}/usr/include/fcntl.h"
h-to-ffi.sh "${SDK}/usr/include/signal.h"
h-to-ffi.sh "${SDK}/usr/include/stdio.h"
h-to-ffi.sh "${SDK}/usr/include/stdlib.h"
h-to-ffi.sh "${SDK}/usr/include/string.h"
h-to-ffi.sh "${SDK}/usr/include/strings.h"
h-to-ffi.sh "${SDK}/usr/include/time.h"
h-to-ffi.sh "${SDK}/usr/include/unistd.h"
h-to-ffi.sh "${SDK}/usr/include/dlfcn.h"
h-to-ffi.sh "${SDK}/usr/include/pthread.h"
h-to-ffi.sh "${SDK}/usr/include/sys/types.h"
h-to-ffi.sh "${SDK}/usr/include/sys/stat.h"
h-to-ffi.sh "${SDK}/usr/include/sys/mman.h"
h-to-ffi.sh "${SDK}/usr/include/sys/socket.h"
h-to-ffi.sh "${SDK}/usr/include/sys/time.h"
h-to-ffi.sh "${SDK}/usr/include/sys/select.h"
h-to-ffi.sh "${SDK}/usr/include/sys/errno.h"
h-to-ffi.sh "${SDK}/usr/include/sys/fcntl.h"
h-to-ffi.sh "${SDK}/usr/include/sys/unistd.h"
h-to-ffi.sh "${SDK}/usr/include/sys/wait.h"
h-to-ffi.sh "${SDK}/usr/include/sys/param.h"
h-to-ffi.sh "${SDK}/usr/include/mach/mach_time.h"
h-to-ffi.sh "${SDK}/usr/include/mach/mach_init.h"

echo ";; libc-core-populate done under $(pwd)"
