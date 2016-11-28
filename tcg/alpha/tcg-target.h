/*
 * Tiny Code Generator for QEMU
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
#ifndef TCG_TARGET_ALPHA
#define TCG_TARGET_ALPHA 1

#define TCG_TARGET_INSN_UNIT_SIZE 4
#define TCG_TARGET_TLB_DISPLACEMENT_BITS 16
#define TCG_TARGET_NB_REGS 32

/* Having the zero register ($31) == 0 within TCG simplifies a few things.
   Thus we have a mapping between TCG regno and hardware regno.  */
#define HW_TO_TCG_REGNO(x)      ((x) ^ 0x1f)
#define TCG_TO_HW_REGNO(x)      ((x) ^ 0x1f)

typedef enum TCGReg {
    TCG_REG_V0 = HW_TO_TCG_REGNO(0),

    TCG_REG_T0 = HW_TO_TCG_REGNO(1),
    TCG_REG_T1 = HW_TO_TCG_REGNO(2),
    TCG_REG_T2 = HW_TO_TCG_REGNO(3),
    TCG_REG_T3 = HW_TO_TCG_REGNO(4),
    TCG_REG_T4 = HW_TO_TCG_REGNO(5),
    TCG_REG_T5 = HW_TO_TCG_REGNO(6),
    TCG_REG_T6 = HW_TO_TCG_REGNO(7),
    TCG_REG_T7 = HW_TO_TCG_REGNO(8),

    TCG_REG_S0 = HW_TO_TCG_REGNO(9),
    TCG_REG_S1 = HW_TO_TCG_REGNO(10),
    TCG_REG_S2 = HW_TO_TCG_REGNO(11),
    TCG_REG_S3 = HW_TO_TCG_REGNO(12),
    TCG_REG_S4 = HW_TO_TCG_REGNO(13),
    TCG_REG_S5 = HW_TO_TCG_REGNO(14),
    TCG_REG_S6 = HW_TO_TCG_REGNO(15),

    TCG_REG_A0 = HW_TO_TCG_REGNO(16),
    TCG_REG_A1 = HW_TO_TCG_REGNO(17),
    TCG_REG_A2 = HW_TO_TCG_REGNO(18),
    TCG_REG_A3 = HW_TO_TCG_REGNO(19),
    TCG_REG_A4 = HW_TO_TCG_REGNO(20),
    TCG_REG_A5 = HW_TO_TCG_REGNO(21),

    TCG_REG_T8 = HW_TO_TCG_REGNO(22),
    TCG_REG_T9 = HW_TO_TCG_REGNO(23),
    TCG_REG_T10 = HW_TO_TCG_REGNO(24),
    TCG_REG_T11 = HW_TO_TCG_REGNO(25),

    TCG_REG_RA = HW_TO_TCG_REGNO(26),
    TCG_REG_PV = HW_TO_TCG_REGNO(27),
    TCG_REG_AT = HW_TO_TCG_REGNO(28),
    TCG_REG_GP = HW_TO_TCG_REGNO(29),
    TCG_REG_SP = HW_TO_TCG_REGNO(30),

    TCG_REG_ZERO = HW_TO_TCG_REGNO(31)
} TCGReg;

/* Used for function call generation.  */
#define TCG_REG_CALL_STACK TCG_REG_SP
#define TCG_TARGET_STACK_ALIGN 16
#define TCG_TARGET_CALL_STACK_OFFSET 0

/* We have signed extension instructions.  */
#define TCG_TARGET_HAS_ext8s_i32        1
#define TCG_TARGET_HAS_ext16s_i32       1
#define TCG_TARGET_HAS_ext8s_i64        1
#define TCG_TARGET_HAS_ext16s_i64       1
#define TCG_TARGET_HAS_ext32s_i64       1

/* We have single-output division routines.  */
#define TCG_TARGET_HAS_div_i32          1
#define TCG_TARGET_HAS_rem_i32          1
#define TCG_TARGET_HAS_div_i64          1
#define TCG_TARGET_HAS_rem_i64          1

/* We have conditional move.  */
#define TCG_TARGET_HAS_movcond_i32      1
#define TCG_TARGET_HAS_movcond_i64      1

/* We have optimized bswap routines.  */
#define TCG_TARGET_HAS_bswap16_i32      1
#define TCG_TARGET_HAS_bswap32_i32      1
#define TCG_TARGET_HAS_bswap16_i64      1
#define TCG_TARGET_HAS_bswap32_i64      1
#define TCG_TARGET_HAS_bswap64_i64      1

/* We have NOT via ORNOT.  */
#define TCG_TARGET_HAS_not_i32          1
#define TCG_TARGET_HAS_not_i64          1

/* We have MULUH via UMULQH.  */
#define TCG_TARGET_HAS_muluh_i64        1

/* We have some compound logical instructions.  */
#define TCG_TARGET_HAS_andc_i32         1
#define TCG_TARGET_HAS_andc_i64         1
#define TCG_TARGET_HAS_orc_i32          1
#define TCG_TARGET_HAS_orc_i64          1
#define TCG_TARGET_HAS_eqv_i32          1
#define TCG_TARGET_HAS_eqv_i64          1
#define TCG_TARGET_HAS_nand_i32         0
#define TCG_TARGET_HAS_nand_i64         0
#define TCG_TARGET_HAS_nor_i32          0
#define TCG_TARGET_HAS_nor_i64          0

/* We can do better for specific cases of deposit and extract.  */
#define TCG_TARGET_HAS_deposit_i32      1
#define TCG_TARGET_HAS_deposit_i64      1
#define TCG_TARGET_HAS_extract_i32      1
#define TCG_TARGET_HAS_extract_i64      1
#define TCG_TARGET_HAS_sextract_i32     0
#define TCG_TARGET_HAS_sextract_i64     0

static inline bool TCG_TARGET_deposit_valid(unsigned ofs, unsigned len)
{
    return ofs % 8 == 0 && (len == 8 || len == 16 || len == 32);
}

#define TCG_TARGET_deposit_i32_valid  TCG_TARGET_deposit_valid
#define TCG_TARGET_deposit_i64_valid  TCG_TARGET_deposit_valid

static inline bool TCG_TARGET_extract_valid(unsigned ofs, unsigned len)
{
    /* We can extract 8, 16, or 32 bit quantities, or the 24 bits [8:31].  */
    return ofs % 8 == 0 && (len == 8 || len == 16 || len == 32
                            || ofs + len == 32);
}

#define TCG_TARGET_extract_i32_valid  TCG_TARGET_extract_valid
#define TCG_TARGET_extract_i64_valid  TCG_TARGET_extract_valid

/* We require special attention for truncation.  */
#define TCG_TARGET_HAS_extrl_i64_i32    1
#define TCG_TARGET_HAS_extrh_i64_i32    1

/* The default implementations of these are fine.  */
#define TCG_TARGET_HAS_neg_i32          0
#define TCG_TARGET_HAS_neg_i64          0
#define TCG_TARGET_HAS_ext8u_i32        0
#define TCG_TARGET_HAS_ext16u_i32       0
#define TCG_TARGET_HAS_ext8u_i64        0
#define TCG_TARGET_HAS_ext16u_i64       0
#define TCG_TARGET_HAS_ext32u_i64       0
#define TCG_TARGET_HAS_rot_i32          0
#define TCG_TARGET_HAS_rot_i64          0
#define TCG_TARGET_HAS_add2_i32         0
#define TCG_TARGET_HAS_add2_i64         0
#define TCG_TARGET_HAS_sub2_i32         0
#define TCG_TARGET_HAS_sub2_i64         0
#define TCG_TARGET_HAS_muls2_i32        0
#define TCG_TARGET_HAS_muls2_i64        0
#define TCG_TARGET_HAS_mulu2_i32        0
#define TCG_TARGET_HAS_mulu2_i64        0
#define TCG_TARGET_HAS_mulsh_i32        0
#define TCG_TARGET_HAS_mulsh_i64        0
#define TCG_TARGET_HAS_muluh_i32        0
#define TCG_TARGET_HAS_clz_i32          0
#define TCG_TARGET_HAS_ctz_i32          0
#define TCG_TARGET_HAS_clz_i64          0
#define TCG_TARGET_HAS_ctz_i64          0

#define TCG_TARGET_HAS_GUEST_BASE

#define TCG_AREG0 TCG_REG_S6

static inline void flush_icache_range(uintptr_t start, uintptr_t stop)
{
    __asm__ __volatile__ ("call_pal 0x86");
}

#endif /* TCG_TARGET_ALPHA */
