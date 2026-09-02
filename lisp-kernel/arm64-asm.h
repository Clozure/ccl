/* SPDX-License-Identifier: Apache-2.0 */

#pragma once

/*
 * Standalone assembler include file for arm64.
 */

/*
 * C(name) spells a C symbol the way the platform assembler expects it:
 * Mach-O prepends a leading underscore, ELF does not.
 */
#if defined(__APPLE__)
#define C(name) _##name
#else
#define C(name) name
#endif

/*
 * RELRO names the section used for a table of pointers that the loader
 * fills in (rebases/binds) at load time and then makes read-only for the
 * rest of the run.  Write it as the operand of a .section directive:
 *
 *      .section RELRO
 *
 * These are the magic arguments for Mach-O and for ELF.
 */
#if defined(__APPLE__)
#define RELRO __DATA_CONST,__const
#else
#define RELRO .data.rel.ro,"aw",%progbits
#endif
