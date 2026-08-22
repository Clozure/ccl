#!/usr/bin/env python3
"""Filter ffigen5 .ffi for Darwin/arm64 parse-ffi.

Drops:
  * macros from Availability*.h / ptrcheck.h (circular expand → stack overflow)
  * top-level (function …) forms that mention (null) — unmapped clang types
  * optionally all macros, or macros outside Apple frameworks (see env)

Env:
  FILTER_FFI_MACROS=all|frameworks|default
    all         — keep macros (only Availability/ptrcheck dropped)
    frameworks  — keep macros whose source path contains /Frameworks/
    none        — drop every macro (fast cocoa parse; enum-idents remain)
    default     — same as all

Usage:
  filter-ffi.py [file.ffi …]          # rewrite in place
  filter-ffi.py < in.ffi > out.ffi    # stdin/stdout when no args
"""
from __future__ import annotations

import os
import re
import sys
from pathlib import Path

DROP_MACRO_SUBSTR = (
    "Availability",
    "ptrcheck.h",
)

FUNCTION_START = re.compile(r"^\(function\b")
MACRO_START = re.compile(r"^\(macro\b")


def _balanced_form_lines(lines: list[str], start: int) -> tuple[list[str], int]:
    """Return lines of one top-level s-expression starting at start, and next index."""
    depth = 0
    out: list[str] = []
    i = start
    while i < len(lines):
        line = lines[i]
        out.append(line)
        depth += line.count("(") - line.count(")")
        i += 1
        if depth <= 0:
            break
    return out, i


def _keep_macro(blob: str, mode: str) -> bool:
    if any(s in blob for s in DROP_MACRO_SUBSTR):
        return False
    if mode == "none":
        return False
    if mode == "frameworks":
        return "/Frameworks/" in blob
    return True


def filter_text(text: str, macro_mode: str = "default") -> tuple[str, dict[str, int]]:
    lines = text.splitlines(keepends=True)
    if lines and not lines[-1].endswith("\n") and lines[-1] != "":
        lines[-1] += "\n"
    stats = {"macros_dropped": 0, "functions_dropped": 0, "kept": 0}
    out: list[str] = []
    i = 0
    while i < len(lines):
        line = lines[i]
        if MACRO_START.match(line):
            form, i = _balanced_form_lines(lines, i)
            blob = "".join(form)
            if not _keep_macro(blob, macro_mode):
                stats["macros_dropped"] += 1
                continue
            out.extend(form)
            stats["kept"] += 1
            continue
        if FUNCTION_START.match(line):
            form, i = _balanced_form_lines(lines, i)
            blob = "".join(form)
            if "(null)" in blob:
                stats["functions_dropped"] += 1
                continue
            out.extend(form)
            stats["kept"] += 1
            continue
        out.append(line)
        i += 1
        stats["kept"] += 1
    return "".join(out), stats


def main(argv: list[str]) -> int:
    mode = os.environ.get("FILTER_FFI_MACROS", "default").lower()
    if mode not in ("all", "frameworks", "none", "default"):
        mode = "default"
    if len(argv) <= 1:
        text = sys.stdin.read()
        filtered, stats = filter_text(text, mode)
        sys.stdout.write(filtered)
        print(
            f";; filter-ffi macros={mode}: dropped={stats['macros_dropped']} "
            f"functions_dropped={stats['functions_dropped']}",
            file=sys.stderr,
        )
        return 0
    for arg in argv[1:]:
        path = Path(arg)
        text = path.read_text(encoding="utf-8", errors="replace")
        filtered, stats = filter_text(text, mode)
        path.write_text(filtered, encoding="utf-8")
        print(
            f"{path}: macros={mode} dropped={stats['macros_dropped']} "
            f"functions_dropped={stats['functions_dropped']}"
        )
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
