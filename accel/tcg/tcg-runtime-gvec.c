/*
 *  Generic vectorized operation runtime
 *
 *  Copyright (c) 2017 Linaro
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 */

#include "qemu/osdep.h"
#include "qemu/host-utils.h"
#include "cpu.h"
#include "exec/helper-proto.h"
#include "tcg-gvec-desc.h"


/* Virtually all hosts support 16-byte vectors.  Those that don't can emulate
   them via GCC's generic vector extension.  This turns out to be simpler and
   more reliable than getting the compiler to autovectorize.

   In tcg-op-gvec.c, we asserted that both the size and alignment
   of the data are multiples of 16.  */

typedef uint8_t vec8 __attribute__((vector_size(16)));
typedef uint16_t vec16 __attribute__((vector_size(16)));
typedef uint32_t vec32 __attribute__((vector_size(16)));
typedef uint64_t vec64 __attribute__((vector_size(16)));

typedef int8_t svec8 __attribute__((vector_size(16)));
typedef int16_t svec16 __attribute__((vector_size(16)));
typedef int32_t svec32 __attribute__((vector_size(16)));
typedef int64_t svec64 __attribute__((vector_size(16)));

static inline void clear_high(void *d, intptr_t oprsz, uint32_t desc)
{
    intptr_t maxsz = simd_maxsz(desc);
    intptr_t i;

    if (unlikely(maxsz > oprsz)) {
        for (i = oprsz; i < maxsz; i += sizeof(uint64_t)) {
            *(uint64_t *)(d + i) = 0;
        }
    }
}

void HELPER(gvec_add8)(void *d, void *a, void *b, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec8)) {
        *(vec8 *)(d + i) = *(vec8 *)(a + i) + *(vec8 *)(b + i);
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_add16)(void *d, void *a, void *b, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec16)) {
        *(vec16 *)(d + i) = *(vec16 *)(a + i) + *(vec16 *)(b + i);
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_add32)(void *d, void *a, void *b, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec32)) {
        *(vec32 *)(d + i) = *(vec32 *)(a + i) + *(vec32 *)(b + i);
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_add64)(void *d, void *a, void *b, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec64)) {
        *(vec64 *)(d + i) = *(vec64 *)(a + i) + *(vec64 *)(b + i);
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_sub8)(void *d, void *a, void *b, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec8)) {
        *(vec8 *)(d + i) = *(vec8 *)(a + i) - *(vec8 *)(b + i);
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_sub16)(void *d, void *a, void *b, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec16)) {
        *(vec16 *)(d + i) = *(vec16 *)(a + i) - *(vec16 *)(b + i);
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_sub32)(void *d, void *a, void *b, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec32)) {
        *(vec32 *)(d + i) = *(vec32 *)(a + i) - *(vec32 *)(b + i);
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_sub64)(void *d, void *a, void *b, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec64)) {
        *(vec64 *)(d + i) = *(vec64 *)(a + i) - *(vec64 *)(b + i);
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_mul8)(void *d, void *a, void *b, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec8)) {
        *(vec8 *)(d + i) = *(vec8 *)(a + i) * *(vec8 *)(b + i);
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_mul16)(void *d, void *a, void *b, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec16)) {
        *(vec16 *)(d + i) = *(vec16 *)(a + i) * *(vec16 *)(b + i);
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_mul32)(void *d, void *a, void *b, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec32)) {
        *(vec32 *)(d + i) = *(vec32 *)(a + i) * *(vec32 *)(b + i);
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_mul64)(void *d, void *a, void *b, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec64)) {
        *(vec64 *)(d + i) = *(vec64 *)(a + i) * *(vec64 *)(b + i);
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_neg8)(void *d, void *a, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec8)) {
        *(vec8 *)(d + i) = -*(vec8 *)(a + i);
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_neg16)(void *d, void *a, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec16)) {
        *(vec16 *)(d + i) = -*(vec16 *)(a + i);
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_neg32)(void *d, void *a, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec32)) {
        *(vec32 *)(d + i) = -*(vec32 *)(a + i);
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_neg64)(void *d, void *a, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec64)) {
        *(vec64 *)(d + i) = -*(vec64 *)(a + i);
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_mov)(void *d, void *a, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);

    memcpy(d, a, oprsz);
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_dup64)(void *d, uint32_t desc, uint64_t c)
{
    intptr_t oprsz = simd_oprsz(desc);
    intptr_t i;

    if (c == 0) {
        oprsz = 0;
    } else {
        for (i = 0; i < oprsz; i += sizeof(uint64_t)) {
            *(uint64_t *)(d + i) = c;
        }
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_dup32)(void *d, uint32_t desc, uint32_t c)
{
    intptr_t oprsz = simd_oprsz(desc);
    intptr_t i;

    if (c == 0) {
        oprsz = 0;
    } else {
        for (i = 0; i < oprsz; i += sizeof(uint32_t)) {
            *(uint32_t *)(d + i) = c;
        }
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_dup16)(void *d, uint32_t desc, uint32_t c)
{
    HELPER(gvec_dup32)(d, desc, 0x00010001 * (c & 0xffff));
}

void HELPER(gvec_dup8)(void *d, uint32_t desc, uint32_t c)
{
    HELPER(gvec_dup32)(d, desc, 0x01010101 * (c & 0xff));
}

void HELPER(gvec_not)(void *d, void *a, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec64)) {
        *(vec64 *)(d + i) = ~*(vec64 *)(a + i);
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_and)(void *d, void *a, void *b, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec64)) {
        *(vec64 *)(d + i) = *(vec64 *)(a + i) & *(vec64 *)(b + i);
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_or)(void *d, void *a, void *b, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec64)) {
        *(vec64 *)(d + i) = *(vec64 *)(a + i) | *(vec64 *)(b + i);
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_xor)(void *d, void *a, void *b, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec64)) {
        *(vec64 *)(d + i) = *(vec64 *)(a + i) ^ *(vec64 *)(b + i);
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_andc)(void *d, void *a, void *b, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec64)) {
        *(vec64 *)(d + i) = *(vec64 *)(a + i) &~ *(vec64 *)(b + i);
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_orc)(void *d, void *a, void *b, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec64)) {
        *(vec64 *)(d + i) = *(vec64 *)(a + i) |~ *(vec64 *)(b + i);
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_shl8i)(void *d, void *a, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    int shift = simd_data(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec8)) {
        *(vec8 *)(d + i) = *(vec8 *)(a + i) << shift;
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_shl16i)(void *d, void *a, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    int shift = simd_data(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec16)) {
        *(vec16 *)(d + i) = *(vec16 *)(a + i) << shift;
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_shl32i)(void *d, void *a, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    int shift = simd_data(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec32)) {
        *(vec32 *)(d + i) = *(vec32 *)(a + i) << shift;
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_shl64i)(void *d, void *a, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    int shift = simd_data(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec64)) {
        *(vec64 *)(d + i) = *(vec64 *)(a + i) << shift;
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_shr8i)(void *d, void *a, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    int shift = simd_data(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec8)) {
        *(vec8 *)(d + i) = *(vec8 *)(a + i) >> shift;
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_shr16i)(void *d, void *a, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    int shift = simd_data(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec16)) {
        *(vec16 *)(d + i) = *(vec16 *)(a + i) >> shift;
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_shr32i)(void *d, void *a, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    int shift = simd_data(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec32)) {
        *(vec32 *)(d + i) = *(vec32 *)(a + i) >> shift;
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_shr64i)(void *d, void *a, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    int shift = simd_data(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec64)) {
        *(vec64 *)(d + i) = *(vec64 *)(a + i) >> shift;
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_sar8i)(void *d, void *a, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    int shift = simd_data(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec8)) {
        *(svec8 *)(d + i) = *(svec8 *)(a + i) >> shift;
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_sar16i)(void *d, void *a, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    int shift = simd_data(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec16)) {
        *(svec16 *)(d + i) = *(svec16 *)(a + i) >> shift;
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_sar32i)(void *d, void *a, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    int shift = simd_data(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec32)) {
        *(svec32 *)(d + i) = *(svec32 *)(a + i) >> shift;
    }
    clear_high(d, oprsz, desc);
}

void HELPER(gvec_sar64i)(void *d, void *a, uint32_t desc)
{
    intptr_t oprsz = simd_oprsz(desc);
    int shift = simd_data(desc);
    intptr_t i;

    for (i = 0; i < oprsz; i += sizeof(vec64)) {
        *(svec64 *)(d + i) = *(svec64 *)(a + i) >> shift;
    }
    clear_high(d, oprsz, desc);
}

/* The size of the alloca in the following is currently bounded to 2k.  */

#define DO_ZIP(NAME, TYPE) \
void HELPER(NAME)(void *d, void *a, void *b, uint32_t desc)                  \
{                                                                            \
    intptr_t oprsz = simd_oprsz(desc);                                       \
    intptr_t oprsz_2 = oprsz / 2;                                            \
    intptr_t i;                                                              \
    /* We produce output faster than we consume input.                       \
       Therefore we must be mindful of possible overlap.  */                 \
    if (unlikely((a - d) < (uintptr_t)oprsz)) {                              \
        void *a_new = alloca(oprsz_2);                                       \
        memcpy(a_new, a, oprsz_2);                                           \
        a = a_new;                                                           \
    }                                                                        \
    if (unlikely((b - d) < (uintptr_t)oprsz)) {                              \
        void *b_new = alloca(oprsz_2);                                       \
        memcpy(b_new, b, oprsz_2);                                           \
        b = b_new;                                                           \
    }                                                                        \
    for (i = 0; i < oprsz_2; i += sizeof(TYPE)) {                            \
	*(TYPE *)(d + 2 * i + 0) = *(TYPE *)(a + i);                         \
	*(TYPE *)(d + 2 * i + sizeof(TYPE)) = *(TYPE *)(b + i);              \
    }                                                                        \
    clear_high(d, oprsz, desc);                                              \
}

DO_ZIP(gvec_zip8, uint8_t)
DO_ZIP(gvec_zip16, uint16_t)
DO_ZIP(gvec_zip32, uint32_t)
DO_ZIP(gvec_zip64, uint64_t)

#define DO_UZP(NAME, TYPE) \
void HELPER(NAME)(void *d, void *a, void *b, uint32_t desc)                  \
{                                                                            \
    intptr_t oprsz = simd_oprsz(desc);                                       \
    intptr_t oprsz_2 = oprsz / 2;                                            \
    intptr_t odd_ofs = simd_data(desc);                                      \
    intptr_t i;                                                              \
    if (unlikely((b - d) < (uintptr_t)oprsz)) {                              \
        void *b_new = alloca(oprsz);                                         \
        memcpy(b_new, b, oprsz);                                             \
        b = b_new;                                                           \
    }                                                                        \
    for (i = 0; i < oprsz_2; i += sizeof(TYPE)) {                            \
        *(TYPE *)(d + i) = *(TYPE *)(a + 2 * i + odd_ofs);                   \
    }                                                                        \
    for (i = 0; i < oprsz_2; i += sizeof(TYPE)) {                            \
        *(TYPE *)(d + oprsz_2 + i) = *(TYPE *)(b + 2 * i + odd_ofs);         \
    }                                                                        \
    clear_high(d, oprsz, desc);                                              \
}

DO_UZP(gvec_uzp8, uint8_t)
DO_UZP(gvec_uzp16, uint16_t)
DO_UZP(gvec_uzp32, uint32_t)
DO_UZP(gvec_uzp64, uint64_t)

#define DO_TRN(NAME, TYPE) \
void HELPER(NAME)(void *d, void *a, void *b, uint32_t desc)                  \
{                                                                            \
    intptr_t oprsz = simd_oprsz(desc);                                       \
    intptr_t odd_ofs = simd_data(desc);                                      \
    intptr_t i;                                                              \
    for (i = 0; i < oprsz; i += 2 * sizeof(TYPE)) {                          \
        TYPE ae = *(TYPE *)(a + i + odd_ofs);                                \
        TYPE be = *(TYPE *)(b + i + odd_ofs);                                \
	*(TYPE *)(d + i + 0) = ae;                                           \
	*(TYPE *)(d + i + sizeof(TYPE)) = be;                                \
    }                                                                        \
    clear_high(d, oprsz, desc);                                              \
}

DO_TRN(gvec_trn8, uint8_t)
DO_TRN(gvec_trn16, uint16_t)
DO_TRN(gvec_trn32, uint32_t)
DO_TRN(gvec_trn64, uint64_t)

#define DO_CMP1(NAME, TYPE, OP)                                              \
void HELPER(NAME)(void *d, void *a, void *b, uint32_t desc)                  \
{                                                                            \
    intptr_t oprsz = simd_oprsz(desc);                                       \
    intptr_t i;                                                              \
    for (i = 0; i < oprsz; i += sizeof(vec64)) {                             \
        *(TYPE *)(d + i) = *(TYPE *)(a + i) OP *(TYPE *)(b + i);             \
    }                                                                        \
    clear_high(d, oprsz, desc);                                              \
}

#define DO_CMP2(SZ) \
    DO_CMP1(gvec_eq##SZ, vec##SZ, ==)    \
    DO_CMP1(gvec_ne##SZ, vec##SZ, !=)    \
    DO_CMP1(gvec_lt##SZ, svec##SZ, <)    \
    DO_CMP1(gvec_le##SZ, svec##SZ, <=)   \
    DO_CMP1(gvec_ltu##SZ, vec##SZ, <)    \
    DO_CMP1(gvec_leu##SZ, vec##SZ, <=)

DO_CMP2(8)
DO_CMP2(16)
DO_CMP2(32)
DO_CMP2(64)

#define DO_EXT(NAME, TYPE1, TYPE2) \
void HELPER(NAME)(void *d, void *a, uint32_t desc)                           \
{                                                                            \
    intptr_t oprsz = simd_oprsz(desc);                                       \
    intptr_t oprsz_2 = oprsz / 2;                                            \
    intptr_t i;                                                              \
    /* We produce output faster than we consume input.                       \
       Therefore we must be mindful of possible overlap.  */                 \
    if (unlikely((a - d) < (uintptr_t)oprsz)) {                              \
        void *a_new = alloca(oprsz_2);                                       \
        memcpy(a_new, a, oprsz_2);                                           \
        a = a_new;                                                           \
    }                                                                        \
    for (i = 0; i < oprsz_2; i += sizeof(TYPE1)) {                           \
        *(TYPE2 *)(d + 2 * i) = *(TYPE1 *)(a + i);                           \
    }                                                                        \
    clear_high(d, oprsz, desc);                                              \
}

DO_EXT(gvec_extu8, uint8_t, uint16_t)
DO_EXT(gvec_extu16, uint16_t, uint32_t)
DO_EXT(gvec_extu32, uint32_t, uint64_t)
DO_EXT(gvec_exts8, int8_t, int16_t)
DO_EXT(gvec_exts16, int16_t, int32_t)
DO_EXT(gvec_exts32, int32_t, int64_t)