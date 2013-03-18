/*-
 * Copyright (c) 2011-2012 Alexander Nasonov.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef _NET_BPFJIT_H_
#define _NET_BPFJIT_H_

#ifndef _KERNEL
#include <stddef.h>
#include <stdint.h>
#endif

#include <sys/types.h>

#ifdef __linux
#include <pcap-bpf.h>
#else
#include <net/bpf.h>
#endif

/*
 * RETURN value and arguments of a function generated by sljit have sljit_uw
 * type which can have a greater width than arguments below. In such cases,
 * we rely on the fact that calling conventions use same registers for
 * smaller types.
 * SLJIT_MOV_UI is passed to sljit_emit_return() to make sure that the
 * return value is truncated to unsigned int.
 */
typedef unsigned int (*bpfjit_function_t)(const uint8_t *p,
    unsigned int wirelen, unsigned int buflen);

bpfjit_function_t bpfjit_generate_code(struct bpf_insn *insns,
    size_t insn_count);

void bpfjit_free_code(bpfjit_function_t code);

#endif /* !_NET_BPFJIT_H_ */
