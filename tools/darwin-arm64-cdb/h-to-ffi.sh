#!/bin/sh
# Translate one C/ObjC header to .ffi under ./<sdk-relative-path>/ via ffigen5.
# Used by tools/darwin-arm64-cdb/*-populate.sh.
#
# Env:
#   FFIGEN      — path to ffigen5 binary (default: sibling ccl-ffigen build)
#   SDK         — MacOSX.sdk (default: xcrun --show-sdk-path)
#   CFLAGS      — extra flags (populate scripts set -arch arm64 -isysroot …)
#   FFIGEN_LANG — clang -x language (default: c; cocoa uses objective-c)

set -e
if [ -z "${FFIGEN}" ]; then
  HERE=$(cd "$(dirname "$0")" && pwd)
  if [ -x "${HERE}/../../../ccl-ffigen/ffigen5/ffigen5" ]; then
    FFIGEN="${HERE}/../../../ccl-ffigen/ffigen5/ffigen5"
  elif [ -x "${HERE}/../../ccl-ffigen/ffigen5/ffigen5" ]; then
    FFIGEN="${HERE}/../../ccl-ffigen/ffigen5/ffigen5"
  else
    FFIGEN=ffigen5
  fi
fi

LANG_MODE=${FFIGEN_LANG:-c}

includes=""
other_flags=""
while [ $# -gt 1 ]; do
  case "$1" in
    -include)
      includes="$includes -include $2"
      shift 2
      ;;
    -*)
      other_flags="$other_flags $1"
      shift
      ;;
    *)
      break
      ;;
  esac
done

header=$1
if [ -z "$header" ]; then
  echo "usage: h-to-ffi.sh [-include H]… header.h" >&2
  exit 2
fi
if [ ! -f "$header" ]; then
  echo "skip missing: $header" >&2
  exit 0
fi

output_dir=".`dirname "$header"`"
mkdir -p "$output_dir"
output_file="`basename "$header" .h`.ffi"
output_path="$output_dir/$output_file"
echo "$header"
# shellcheck disable=SC2086
if ! "$FFIGEN" $CFLAGS $other_flags -x "$LANG_MODE" $includes "$header" -o "$output_path"; then
  echo "WARN: ffigen failed: $header" >&2
  rm -f "$output_path"
  exit 0
fi
HERE=$(cd "$(dirname "$0")" && pwd)
if [ -f "$HERE/filter-ffi.py" ] && [ -f "$output_path" ]; then
  python3 "$HERE/filter-ffi.py" "$output_path"
fi
