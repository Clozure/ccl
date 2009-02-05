/* These definitions are taken from the file /usr/include/machine/npx.h,
   which isn't distributed with amd64 versions of FreeBSD */

/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	from: @(#)npx.h	5.3 (Berkeley) 1/18/91
 * $FreeBSD: src/sys/i386/include/npx.h,v 1.29.2.1 2006/07/01 00:57:55 davidxu Exp $
 */

struct  ccl_envxmm {
	u_int16_t	en_cw;		/* control word (16bits) */
	u_int16_t	en_sw;		/* status word (16bits) */
	u_int16_t	en_tw;		/* tag word (16bits) */
	u_int16_t	en_opcode;	/* opcode last executed (11 bits ) */
	u_int32_t	en_fip;		/* floating point instruction pointer */
	u_int16_t	en_fcs;		/* floating code segment selector */
	u_int16_t	en_pad0;	/* padding */
	u_int32_t	en_foo;		/* floating operand offset */
	u_int16_t	en_fos;		/* floating operand segment selector */
	u_int16_t	en_pad1;	/* padding */
	u_int32_t	en_mxcsr;	/* SSE sontorol/status register */
	u_int32_t	en_mxcsr_mask;	/* valid bits in mxcsr */
};

struct  ccl_xmmacc {
	u_char	xmm_bytes[16];
};

struct ccl_fpacc87 {
	u_char	fp_bytes[10];
};

struct  ccl_savexmm {
	struct	ccl_envxmm	sv_env;
	struct {
		struct ccl_fpacc87	fp_acc;
		u_char		fp_pad[6];      /* padding */
	} sv_fp[8];
	struct ccl_xmmacc	sv_xmm[8];
	u_char sv_pad[224];
} __aligned(16);
