/*
 * Tiny Code Generator for QEMU on ALPHA platform.
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

#include "tcg-be-ldst.h"

#ifndef NDEBUG
static const char * const tcg_target_reg_names[TCG_TARGET_NB_REGS] = {
    [TCG_REG_V0] = "v0",
    [TCG_REG_T0] = "t0",
    [TCG_REG_T1] = "t1",
    [TCG_REG_T2] = "t2",
    [TCG_REG_T3] = "t3",
    [TCG_REG_T4] = "t4",
    [TCG_REG_T5] = "t5",
    [TCG_REG_T6] = "t6",
    [TCG_REG_T7] = "t7",
    [TCG_REG_T8] = "t8",
    [TCG_REG_T9] = "t9",
    [TCG_REG_T10] = "t10",
    [TCG_REG_T11] = "t11",
    [TCG_REG_S0] = "s0",
    [TCG_REG_S1] = "s1",
    [TCG_REG_S2] = "s2",
    [TCG_REG_S3] = "s3",
    [TCG_REG_S4] = "s4",
    [TCG_REG_S5] = "s5",
    [TCG_REG_S6] = "s6",
    [TCG_REG_A0] = "a0",
    [TCG_REG_A1] = "a1",
    [TCG_REG_A2] = "a2",
    [TCG_REG_A3] = "a3",
    [TCG_REG_A4] = "a4",
    [TCG_REG_A5] = "a5",
    [TCG_REG_RA] = "ra",
    [TCG_REG_PV] = "pv",
    [TCG_REG_AT] = "at",
    [TCG_REG_GP] = "gp",
    [TCG_REG_SP] = "sp",
    [TCG_REG_ZERO] = "zero",
};
#endif

/*
 * $29 is the global pointer,
 * $30 is the stack pointer,
 * $31 is the zero register,
 */
static const int tcg_target_reg_alloc_order[] = {
    /* Call-saved registers.  */
    TCG_REG_S0,
    TCG_REG_S1,
    TCG_REG_S2,
    TCG_REG_S3,
    TCG_REG_S4,
    TCG_REG_S5,
    TCG_REG_S6,
    /* Call-clobbered temporaries.  */
    TCG_REG_T0,
    TCG_REG_T1,
    TCG_REG_T2,
    TCG_REG_T3,
    TCG_REG_T4,
    TCG_REG_T5,
    TCG_REG_T6,
    TCG_REG_T7,
    TCG_REG_T8,
    TCG_REG_T9,
    TCG_REG_T10,
    TCG_REG_T11,
    TCG_REG_RA,
    TCG_REG_PV,
    TCG_REG_AT,
    /* Call-clobbered argument and return registers.  */
    TCG_REG_V0,
    TCG_REG_A0,
    TCG_REG_A1,
    TCG_REG_A2,
    TCG_REG_A3,
    TCG_REG_A4,
    TCG_REG_A5,
};

/*
 * According to alpha calling convention, these 6 registers are used for
 * function parameter passing. if function has more than 6 parameters,
 * remaining arguments are stored on the stack.
 */
static const int tcg_target_call_iarg_regs[6] = {
    TCG_REG_A0,
    TCG_REG_A1,
    TCG_REG_A2,
    TCG_REG_A3,
    TCG_REG_A4,
    TCG_REG_A5,
};

/*
 * According to alpha calling convention, $0 is used for returning function
 * result.
 */
static const int tcg_target_call_oarg_regs[1] = {
    TCG_REG_V0
};

/*
 * Temporary registers used within this translator.  Note that T9 is
 * selected because it is the division return address register.
 */
#define TMP_REG1 TCG_REG_AT
#define TMP_REG2 TCG_REG_T9

/*
 * Pointer to the current TB.  We don't want to clobber the real GP,
 * and we have few spare call-saved registers, so we re-compute it.
 *
 * In user-only mode the code_gen_buffer is statically allocated inside
 * the executable, making this pointer largely redundant with the real GP.
 * In softmmu, the buffer is dynamically allocated, which puts the buffer
 * well away from the executable.  In that case this pointer is useful
 * for addressing most of the dynamically allocated qemu data structures.
 */
#define TCG_REG_TB  TCG_REG_RA
#ifdef CONFIG_SOFTMMU
# define USE_REG_TB 1
#else
# define USE_REG_TB 0
#endif

/*
 * If the guest base gets placed in high memory, it's more efficient
 * to use a register to hold the address.
 */
#if !defined(CONFIG_SOFTMMU)
# define USE_GUEST_BASE_REG (guest_base > 0x7fff0000)
# define TCG_GUEST_BASE_REG TCG_REG_S5
#endif

/*
 * Constant constraint mask values.
 */
#define TCG_CT_CONST_U8     0x100
#define TCG_CT_CONST_ZERO   0x200
#define TCG_CT_CONST_ANDI   0x400
#define TCG_CT_CONST_PN255  0x800

#define INSN_OP(x)     (((x) & 0x3f) << 26)
#define INSN_FUNC1(x)  (((x) & 0x3) << 14)
#define INSN_FUNC2(x)  (((x) & 0x7f) << 5)
#define INSN_RA(x)     (TCG_TO_HW_REGNO(x) << 21)
#define INSN_RB(x)     (TCG_TO_HW_REGNO(x) << 16)
#define INSN_RC(x)     (TCG_TO_HW_REGNO(x))
#define INSN_LIT(x)    (((x) & 0xff) << 13)
#define INSN_DISP16(x) ((x) & 0xffff)
#define INSN_DISP21(x) ((x) & 0x1fffff)
#define INSN_RSVED(x)  ((x) & 0x3fff)

typedef enum AlphaOpcode {
    INSN_ADDL       = INSN_OP(0x10) | INSN_FUNC2(0x00),
    INSN_ADDQ       = INSN_OP(0x10) | INSN_FUNC2(0x20),
    INSN_AND        = INSN_OP(0x11) | INSN_FUNC2(0x00),
    INSN_BEQ        = INSN_OP(0x39),
    INSN_BGE        = INSN_OP(0x3e),
    INSN_BGT        = INSN_OP(0x3f),
    INSN_BIC        = INSN_OP(0x11) | INSN_FUNC2(0x08),
    INSN_BIS        = INSN_OP(0x11) | INSN_FUNC2(0x20),
    INSN_BLE        = INSN_OP(0x3b),
    INSN_BLT        = INSN_OP(0x3a),
    INSN_BNE        = INSN_OP(0x3d),
    INSN_BR         = INSN_OP(0x30),
    INSN_BSR        = INSN_OP(0x34),
    INSN_CMOVEQ     = INSN_OP(0x11) | INSN_FUNC2(0x24),
    INSN_CMOVGE     = INSN_OP(0x11) | INSN_FUNC2(0x46),
    INSN_CMOVGT     = INSN_OP(0x11) | INSN_FUNC2(0x66),
    INSN_CMOVLE     = INSN_OP(0x11) | INSN_FUNC2(0x64),
    INSN_CMOVLT     = INSN_OP(0x11) | INSN_FUNC2(0x44),
    INSN_CMOVNE     = INSN_OP(0x11) | INSN_FUNC2(0x26),
    INSN_CMPEQ      = INSN_OP(0x10) | INSN_FUNC2(0x2d),
    INSN_CMPLE      = INSN_OP(0x10) | INSN_FUNC2(0x6d),
    INSN_CMPLT      = INSN_OP(0x10) | INSN_FUNC2(0x4d),
    INSN_CMPULE     = INSN_OP(0x10) | INSN_FUNC2(0x3d),
    INSN_CMPULT     = INSN_OP(0x10) | INSN_FUNC2(0x1d),
    INSN_CTPOP      = INSN_OP(0x1C) | INSN_FUNC2(0x30),
    INSN_CTLZ       = INSN_OP(0x1C) | INSN_FUNC2(0x32),
    INSN_CTTZ       = INSN_OP(0x1C) | INSN_FUNC2(0x33),
    INSN_EQV        = INSN_OP(0x11) | INSN_FUNC2(0x48),
    INSN_EXTBL      = INSN_OP(0x12) | INSN_FUNC2(0x06),
    INSN_EXTWH      = INSN_OP(0x12) | INSN_FUNC2(0x5a),
    INSN_EXTWL      = INSN_OP(0x12) | INSN_FUNC2(0x16),
    INSN_EXTLL      = INSN_OP(0x12) | INSN_FUNC2(0x26),
    INSN_INSBL      = INSN_OP(0x12) | INSN_FUNC2(0x0b),
    INSN_INSLH      = INSN_OP(0x12) | INSN_FUNC2(0x67),
    INSN_INSLL      = INSN_OP(0x12) | INSN_FUNC2(0x2b),
    INSN_INSWL      = INSN_OP(0x12) | INSN_FUNC2(0x1b),
    INSN_JMP        = INSN_OP(0x1a) | INSN_FUNC1(0),
    INSN_JSR        = INSN_OP(0x1a) | INSN_FUNC1(1),
    INSN_LDA        = INSN_OP(0x08),
    INSN_LDAH       = INSN_OP(0x09),
    INSN_LDBU       = INSN_OP(0x0a),
    INSN_LDL        = INSN_OP(0x28),
    INSN_LDQ        = INSN_OP(0x29),
    INSN_LDWU       = INSN_OP(0x0c),
    INSN_MB         = INSN_OP(0x18) | 0x4400,
    INSN_MSKBL      = INSN_OP(0x12) | INSN_FUNC2(0x02),
    INSN_MSKLL      = INSN_OP(0x12) | INSN_FUNC2(0x22),
    INSN_MSKWL      = INSN_OP(0x12) | INSN_FUNC2(0x12),
    INSN_MULL       = INSN_OP(0x13) | INSN_FUNC2(0x00),
    INSN_MULQ       = INSN_OP(0x13) | INSN_FUNC2(0x20),
    INSN_ORNOT      = INSN_OP(0x11) | INSN_FUNC2(0x28),
    INSN_RET        = INSN_OP(0x1a) | INSN_FUNC1(2),
    INSN_S4ADDL     = INSN_OP(0x10) | INSN_FUNC2(0x02),
    INSN_S8ADDL     = INSN_OP(0x10) | INSN_FUNC2(0x12),
    INSN_SEXTB      = INSN_OP(0x1c) | INSN_FUNC2(0x00),
    INSN_SEXTW      = INSN_OP(0x1c) | INSN_FUNC2(0x01),
    INSN_SLL        = INSN_OP(0x12) | INSN_FUNC2(0x39),
    INSN_SRA        = INSN_OP(0x12) | INSN_FUNC2(0x3c),
    INSN_SRL        = INSN_OP(0x12) | INSN_FUNC2(0x34),
    INSN_STB        = INSN_OP(0x0e),
    INSN_STL        = INSN_OP(0x2c),
    INSN_STQ        = INSN_OP(0x2d),
    INSN_STW        = INSN_OP(0x0d),
    INSN_SUBL       = INSN_OP(0x10) | INSN_FUNC2(0x09),
    INSN_SUBQ       = INSN_OP(0x10) | INSN_FUNC2(0x29),
    INSN_UMULH      = INSN_OP(0x13) | INSN_FUNC2(0x30),
    INSN_WMB        = INSN_OP(0x18) | 0x4000,
    INSN_XOR        = INSN_OP(0x11) | INSN_FUNC2(0x40),
    INSN_ZAPNOT     = INSN_OP(0x12) | INSN_FUNC2(0x31),

    INSN_BUGCHK     = INSN_OP(0x00) | INSN_DISP16(0x81),

    INSN_NOP        = INSN_BIS
                      | INSN_RA(TCG_REG_ZERO)
                      | INSN_RB(TCG_REG_ZERO)
                      | INSN_RC(TCG_REG_ZERO),
} AlphaOpcode;

/*
 * Given a constraint, fill in the available register set or constant range.
 */
static int target_parse_constraint(TCGArgConstraint *ct, const char **pct_str)
{
    const char *ct_str = *pct_str;

    switch (ct_str[0]) {
    case 'r':
        /* Constaint 'r' means any register is okay.  */
        ct->ct |= TCG_CT_REG;
        tcg_regset_set32(ct->u.regs, 0, 0xffffffffu);
        break;

    case 'a':
        /* Constraint 'a' means $24, one of the division inputs.  */
        ct->ct |= TCG_CT_REG;
        tcg_regset_clear(ct->u.regs);
        tcg_regset_set_reg(ct->u.regs, TCG_REG_T10);
        break;

    case 'b':
        /* Constraint 'b' means $25, one of the division inputs.  */
        ct->ct |= TCG_CT_REG;
        tcg_regset_clear(ct->u.regs);
        tcg_regset_set_reg(ct->u.regs, TCG_REG_T11);
        break;

    case 'c':
        /* Constraint 'c' means $27, the call procedure vector,
           as well as the division output.  */
        ct->ct |= TCG_CT_REG;
        tcg_regset_clear(ct->u.regs);
        tcg_regset_set_reg(ct->u.regs, TCG_REG_PV);
        break;

    case 'L':
        /* Constraint for qemu_ld/st.  The extra reserved registers are
           used for passing the parameters to the helper function.  */
        ct->ct |= TCG_CT_REG;
        tcg_regset_set32(ct->u.regs, 0, 0xffffffffu);
        tcg_regset_reset_reg(ct->u.regs, TCG_REG_A0);
        tcg_regset_reset_reg(ct->u.regs, TCG_REG_A1);
        break;

    case 'I':
        /* Constraint 'I' means an immediate 0 ... 255.  */
        ct->ct |= TCG_CT_CONST_U8;
        break;

    case 'J':
        /* Constraint 'J' means the immediate 0.  */
        ct->ct |= TCG_CT_CONST_ZERO;
        break;

    case 'K':
        /* Constraint 'K' means an immediate -255..255.  */
        ct->ct |= TCG_CT_CONST_PN255;
        break;

    case 'M':
        /* Constraint 'M' means constants used with AND/BIC/ZAPNOT.  */
        ct->ct |= TCG_CT_CONST_ANDI;
        break;

    default:
        return -1;
    }

    ct_str++;
    *pct_str = ct_str;
    return 0;
}

static int tcg_match_zapnot(tcg_target_long val)
{
    tcg_target_long mask0, maskff;

    /* Since we know this is an alpha host, speed the check by using
       cmpbge to compare 8 bytes at once, and incidentally also
       produce the zapnot mask.  */
    /* ??? This builtin was implemented sometime in 2002,
       perhaps in the GCC 3.1 timeframe.  */
    mask0 = __builtin_alpha_cmpbge(0, val);
    maskff = __builtin_alpha_cmpbge(val, -1);

    /* Here, mask0 contains the bytes that are 0, maskff contains
       the bytes that are 0xff; that should cover the entire word.  */
    if ((mask0 | maskff) == 0xff) {
        return maskff;
    }
    return 0;
}

static int tcg_match_andi(tcg_target_long val)
{
    if (val == (val & 0xff)) {
        return 1;  /* and */
    } else if (~val == (~val & 0xff)) {
        return 1;  /* bic */
    } else {
        return tcg_match_zapnot(val) != 0;
    }
}

static int tcg_target_const_match(tcg_target_long val, TCGType type,
                                  const TCGArgConstraint *arg_ct)
{
    int ct = arg_ct->ct;
    tcg_debug_assert(type == TCG_TYPE_I64 || val == (int32_t)val);
    if (ct & TCG_CT_CONST) {
        return 1;
    } else if (ct & TCG_CT_CONST_U8) {
        return val == (uint8_t)val;
    } else if (ct & TCG_CT_CONST_ZERO) {
        return val == 0;
    } else if (ct & TCG_CT_CONST_ANDI) {
        return tcg_match_andi(val);
    } else if (ct & TCG_CT_CONST_PN255) {
        return val >= -255 && val <= 255;
    } else {
        return 0;
    }
}

static inline void tcg_out_fmt_br(TCGContext *s, AlphaOpcode opc,
                                  TCGReg ra, int disp)
{
    tcg_out32(s, opc | INSN_RA(ra) | INSN_DISP21(disp));
}

static inline void tcg_out_fmt_mem(TCGContext *s, AlphaOpcode opc,
                                   TCGReg ra, TCGReg rb, tcg_target_long disp)
{
    tcg_debug_assert(disp == (int16_t)disp);
    tcg_out32(s, opc | INSN_RA(ra) | INSN_RB(rb) | INSN_DISP16(disp));
}

static inline void tcg_out_fmt_jmp(TCGContext *s, AlphaOpcode opc,
                                   TCGReg ra, TCGReg rb, int rsved)
{
    tcg_out32(s, opc | INSN_RA(ra) | INSN_RB(rb) | INSN_RSVED(rsved));
}

static inline void tcg_out_fmt_opr(TCGContext *s, AlphaOpcode opc,
                                   TCGReg ra, TCGReg rb, TCGReg rc)
{
    tcg_out32(s, opc | INSN_RA(ra) | INSN_RB(rb) | INSN_RC(rc));
}

static inline void tcg_out_fmt_opi(TCGContext *s, AlphaOpcode opc,
                                   TCGReg ra, tcg_target_ulong lit, TCGReg rc)
{
    tcg_debug_assert(lit <= 0xff);
    tcg_out32(s, opc | INSN_RA(ra) | INSN_LIT(lit) | INSN_RC(rc) | (1<<12));
}

/*
 * Move from one reg to another.  This is called from tcg.c.
 */
static void tcg_out_mov(TCGContext *s, TCGType type, TCGReg rc, TCGReg rb)
{
    if (rb != rc) {
        tcg_out_fmt_opr(s, INSN_BIS, TCG_REG_ZERO, rb, rc);
    }
}

/*
 * Helper function to emit a memory format operation with a displacement
 * that may be larger than the 16 bits accepted by the real instruction.
 */
static void tcg_out_mem_long(TCGContext *s, AlphaOpcode opc, TCGReg ra,
                             TCGReg rb, tcg_target_long orig)
{
    tcg_target_long l0, l1, extra = 0, val = orig;
    TCGReg rs;

    /* Pick a scratch register.  Use the output register, if possible.  */
    switch (opc) {
    default:
        if (ra != rb) {
            rs = ra;
            break;
        }
        /* FALLTHRU */

    case INSN_STB:
    case INSN_STW:
    case INSN_STL:
    case INSN_STQ:
        tcg_debug_assert(ra != TMP_REG1);
        rs = TMP_REG1;
        break;
    }

    /* See if we can turn a large absolute address into an offset from $gp.
       Note that we assert via -msmall-data and --warn-multiple-gp that
       the $gp value is constant everywhere.  Which means that the translated
       code shares the same value as we have loaded right now.  */
    if (rb == TCG_REG_ZERO && orig != (int32_t)orig) {
        register intptr_t gp __asm__("$29");
        intptr_t disp = orig - gp;

        if (disp == (int32_t)disp) {
            orig = val = disp;
            rb = TCG_REG_GP;
        } else if (USE_REG_TB) {
            disp = orig - (intptr_t)s->code_buf;
            if (disp == (int32_t)disp) {
                orig = val = disp;
                rb = TCG_REG_TB;
            }
        }
    }

    l0 = (int16_t)val;
    val = (val - l0) >> 16;
    l1 = (int16_t)val;

    if (orig == (int32_t)orig) {
        /* Now that we're certain we don't need shifts,
           make sure we don't end up with an extra add.  */
        if (opc == INSN_LDA) {
            rs = ra;
        }
        if (l1 < 0 && orig >= 0) {
            extra = 0x4000;
            l1 = (int16_t)(val - 0x4000);
        }
        tcg_debug_assert((l1 + extra) * 0x10000 + l0 == orig);
    } else {
        tcg_target_long l2, l3;
        TCGReg rh = TCG_REG_ZERO;

        val = (val - l1) >> 16;
        l2 = (int16_t)val;
        val = (val - l2) >> 16;
        l3 = (int16_t)val;

        if (l3) {
            tcg_out_fmt_mem(s, INSN_LDAH, rs, rh, l3);
            rh = rs;
        }
        if (l2) {
            tcg_out_fmt_mem(s, INSN_LDA, rs, rh, l2);
            rh = rs;
        }
        tcg_out_fmt_opi(s, INSN_SLL, rh, 32, rs);

        if (rb != TCG_REG_ZERO) {
            tcg_out_fmt_opr(s, INSN_ADDQ, rs, rb, rs);
        }
        rb = rs;
    }

    if (l1) {
        tcg_out_fmt_mem(s, INSN_LDAH, rs, rb, l1);
        rb = rs;
    }
    if (extra) {
        tcg_out_fmt_mem(s, INSN_LDAH, rs, rb, extra);
        rb = rs;
    }

    if (opc != INSN_LDA || rb != ra || l0 != 0) {
        tcg_out_fmt_mem(s, opc, ra, rb, l0);
    }
}

static void tcg_out_movi(TCGContext *s, TCGType type, TCGReg ra,
                         tcg_target_long val)
{
    if (type == TCG_TYPE_I32) {
        val = (int32_t)val;
    }
    tcg_out_mem_long(s, INSN_LDA, ra, TCG_REG_ZERO, val);
}

static void tcg_out_ld(TCGContext *s, TCGType type, TCGReg ra,
                       TCGReg rb, tcg_target_long disp)
{
    tcg_out_mem_long(s, type == TCG_TYPE_I32 ? INSN_LDL : INSN_LDQ,
                     ra, rb, disp);
}

static void tcg_out_st(TCGContext *s, TCGType type, TCGReg ra,
                       TCGReg rb, tcg_target_long disp)
{
    tcg_out_mem_long(s, type == TCG_TYPE_I32 ? INSN_STL : INSN_STQ,
                     ra, rb, disp);
}

static bool tcg_out_sti(TCGContext *s, TCGType type, TCGArg val,
                        TCGReg base, intptr_t ofs)
{
    if (val == 0) {
        tcg_out_st(s, type, TCG_REG_ZERO, base, ofs);
        return true;
    }
    return false;
}

static void tgen_andi(TCGContext *s, TCGReg ra, tcg_target_ulong val, TCGReg rc)
{
    if (val <= 0xff) {
        tcg_out_fmt_opi(s, INSN_AND, ra, val, rc);
    } else if (~val <= 0xff) {
        tcg_out_fmt_opi(s, INSN_BIC, ra, ~val, rc);
    } else {
        int mask = tcg_match_zapnot(val);
        tcg_debug_assert(mask != 0);
        tcg_out_fmt_opi(s, INSN_ZAPNOT, ra, mask, rc);
    }
}

static void tgen_ext8u(TCGContext *s, TCGReg ra, TCGReg rc)
{
    tcg_out_fmt_opi(s, INSN_AND, ra, 0xff, rc);
}

static void tgen_ext8s(TCGContext *s, TCGReg ra, TCGReg rc)
{
    tcg_out_fmt_opr(s, INSN_SEXTB, TCG_REG_ZERO, ra, rc);
}

static void tgen_ext16u(TCGContext *s, TCGReg ra, TCGReg rc)
{
    tcg_out_fmt_opi(s, INSN_ZAPNOT, ra, 0x03, rc);
}

static void tgen_ext16s(TCGContext *s, TCGReg ra, TCGReg rc)
{
    tcg_out_fmt_opr(s, INSN_SEXTW, TCG_REG_ZERO, ra, rc);
}

static void tgen_ext32u(TCGContext *s, TCGReg ra, TCGReg rc)
{
    tcg_out_fmt_opi(s, INSN_ZAPNOT, ra, 0x0f, rc);
}

static void tgen_ext32s(TCGContext *s, TCGReg ra, TCGReg rc)
{
    tcg_out_fmt_opr(s, INSN_ADDL, TCG_REG_ZERO, ra, rc);
}

static void tgen_extend(TCGContext *s, TCGMemOp memop, TCGReg ra, TCGReg rc)
{
    switch (memop) {
    case MO_UB:
        tgen_ext8u(s, ra, rc);
        break;
    case MO_SB:
        tgen_ext8s(s, ra, rc);
        break;
    case MO_UW:
        tgen_ext16u(s, ra, rc);
        break;
    case MO_SW:
        tgen_ext16s(s, ra, rc);
        break;
    case MO_UL:
        tgen_ext32u(s, ra, rc);
        break;
    case MO_SL:
        tgen_ext32s(s, ra, rc);
        break;
    case MO_Q:
        tcg_out_mov(s, TCG_TYPE_I64, ra, rc);
        break;
    default:
        tcg_abort();
    }
}

static void tgen_bswap(TCGContext *s, TCGMemOp memop, TCGReg ra, TCGReg rc)
{
    const TCGReg t0 = TMP_REG1, t1 = TMP_REG2;

    switch (memop & MO_SIZE) {
    case MO_16:
        /* input value =                                   xxxx xxAB */
        tcg_out_fmt_opi(s, INSN_EXTWH, ra, 7, t0);      /* .... ..B. */
        tcg_out_fmt_opi(s, INSN_EXTBL, ra, 1, rc);      /* .... ...A */
        tcg_out_fmt_opr(s, INSN_BIS, rc, t0, rc);       /* .... ..BA */
        if (memop & MO_SIGN) {
            tcg_out_fmt_opr(s, INSN_SEXTW, TCG_REG_ZERO, rc, rc);
        }
        break;

    case MO_32:
        /* input value =                                   xxxx ABCD */
        tcg_out_fmt_opi(s, INSN_INSLH, ra, 7, t0);      /* .... .ABC */
        tcg_out_fmt_opi(s, INSN_INSWL, ra, 3, rc);      /* ...C D... */
        tcg_out_fmt_opr(s, INSN_BIS, t0, rc, rc);       /* ...C DABC */
        tcg_out_fmt_opi(s, INSN_SRL, rc, 16, t0);       /* .... .CDA */
        tcg_out_fmt_opi(s, INSN_ZAPNOT, rc, 0x0A, rc);  /* .... D.B. */
        tcg_out_fmt_opi(s, INSN_ZAPNOT, t0, 0x05, t0);  /* .... .C.A */
        tcg_out_fmt_opr(s, (memop & MO_SIGN ? INSN_ADDL : INSN_BIS),
                        t0, rc, rc);
        break;

    case MO_64:
        /* ??? Consider moving this out-of-line to the prologue block.  */
        /* input value =                                   ABCD EFGH */
        tcg_out_fmt_opi(s, INSN_SRL, ra, 24, t0);       /* ...A BCDE */
        tcg_out_fmt_opi(s, INSN_SLL, ra, 24, t1);       /* DEFG H... */
        tcg_out_fmt_opi(s, INSN_ZAPNOT, t0, 0x11, t0);  /* ...A ...E */
        tcg_out_fmt_opi(s, INSN_ZAPNOT, t1, 0x88, t1);  /* D... H... */
        tcg_out_fmt_opr(s, INSN_BIS, t0, t1, t1);       /* D..A H..E */
        tcg_out_fmt_opi(s, INSN_SRL, ra, 8, t0);        /* .ABC DEFG */
        tcg_out_fmt_opi(s, INSN_ZAPNOT, t0, 0x22, t0);  /* ..B. ..F. */
        tcg_out_fmt_opr(s, INSN_BIS, t0, t1, t1);       /* D.BA H.FE */
        tcg_out_fmt_opi(s, INSN_SLL, ra, 8, t0);        /* BCDE FGH. */
        tcg_out_fmt_opi(s, INSN_ZAPNOT, t1, 0x44, t0);  /* .C.. .G.. */
        tcg_out_fmt_opr(s, INSN_BIS, t1, t1, t1);       /* DCBA HGFE */
        tcg_out_fmt_opi(s, INSN_SRL, t1, 32, t0);       /* .... DCBA */
        tcg_out_fmt_opi(s, INSN_SLL, t1, 32, t1);       /* HGFE .... */
        tcg_out_fmt_opr(s, INSN_BIS, t0, t1, rc);       /* HGFE DCBA */
        break;

    default:
        tcg_abort();
    }
}

static void tgen_ctxz(TCGContext *s, AlphaOpcode opc, TCGReg dest, TCGReg arg1,
                      TCGArg arg2, bool c2)
{
    if (c2 && arg2 == 64) {
        tcg_out_fmt_opr(s, opc, TCG_REG_ZERO, arg1, dest);
    } else if (!c2 && dest == arg2) {
        tcg_out_fmt_opr(s, opc, TCG_REG_ZERO, arg1, TMP_REG1);
        tcg_out_movcond(s, TCG_COND_NE, dest, arg1, 0, 1, TMP_REG1, 0);
    } else if (dest != arg1) {
        tcg_out_fmt_opr(s, opc, TCG_REG_ZERO, arg1, dest);
        tcg_out_movcond(s, TCG_COND_EQ, dest, arg1, 0, 1, arg2, c2);
    } else {
        tcg_out_fmt_opr(s, opc, TCG_REG_ZERO, arg1, TMP_REG1);
        tcg_out_movcond(s, TCG_COND_EQ, TMP_REG1, arg1, 0, 1, arg2, c2);
        tcg_out_mov(s, TCG_TYPE_REG, dest, TMP_REG1);
    }
}

static void tcg_out_ld_sz(TCGContext *s, TCGMemOp memop, TCGReg ra, TCGReg rb,
                          tcg_target_long disp)
{
    static const AlphaOpcode ld_opc[4] = {
        INSN_LDBU, INSN_LDWU, INSN_LDL, INSN_LDQ
    };

    tcg_out_mem_long(s, ld_opc[memop & MO_SIZE], ra, rb, disp);

    switch (memop) {
    case MO_UB:
    case MO_LEUW:
    case MO_LESL:
    case MO_LEQ:
        /* Implemented directly with the opcode.  */
        break;

    case MO_SB:
    case MO_LESW:
    case MO_LEUL:
        tgen_extend(s, memop & MO_SSIZE, ra, ra);
        break;

    case MO_BEUW:
    case MO_BESW:
    case MO_BEUL:
    case MO_BESL:
    case MO_BEQ:
        tgen_bswap(s, memop & MO_SSIZE, ra, ra);
        break;

    default:
        tcg_abort();
    }
}

static void tcg_out_st_sz(TCGContext *s, TCGMemOp memop, TCGReg ra, TCGReg rb,
                          tcg_target_long disp)
{
    static const AlphaOpcode st_opc[4] = {
        INSN_STB, INSN_STW, INSN_STL, INSN_STQ
    };

    tcg_out_mem_long(s, st_opc[memop & MO_SIZE], ra, rb, disp);
}

static inline int valid_direct(intptr_t diff)
{
    return (diff & 3) == 0 && diff >= -0x400000 && diff < 0x400000;
}

static void patch_reloc(tcg_insn_unit *code_ptr, int type,
                        intptr_t value, intptr_t addend)
{
    tcg_debug_assert(type == R_ALPHA_BRADDR);
    tcg_debug_assert(addend == 0);
    value -= (intptr_t)code_ptr + 4;
    tcg_debug_assert(valid_direct(value));
    *code_ptr = (*code_ptr & ~0x1fffff) | INSN_DISP21(value >> 2);
}

static void tcg_out_br_noaddr(TCGContext *s, AlphaOpcode opc, TCGReg ra)
{
    /* We need to keep the offset unchanged for retranslation.
       The field loaded here will be masked in tcg_out_fmt_br.  */
    tcg_out_fmt_br(s, opc, ra, *s->code_ptr);
}

static void tcg_out_br_direct(TCGContext *s, AlphaOpcode opc, TCGReg ra,
                              tcg_insn_unit *target)
{
    ptrdiff_t disp = tcg_ptr_byte_diff(target, s->code_ptr + 1);
    tcg_debug_assert(valid_direct(disp));
    tcg_out_fmt_br(s, opc, ra, disp >> 2);
}

static void tcg_out_br_label(TCGContext *s, AlphaOpcode opc, TCGReg ra,
                             TCGLabel *l)
{
    if (l->has_value) {
        tcg_out_br_direct(s, opc, ra, l->u.value_ptr);
    } else {
        tcg_out_reloc(s, s->code_ptr, R_ALPHA_BRADDR, l, 0);
        tcg_out_br_noaddr(s, opc, ra);
    }
}

static void tcg_out_jbr(TCGContext *s, AlphaOpcode opd, AlphaOpcode opi,
                        TCGReg ra, TCGReg rb, tcg_insn_unit *target)
{
    ptrdiff_t disp = tcg_ptr_byte_diff(target, s->code_ptr + 1);
    tcg_debug_assert(((intptr_t)target & 3) == 0);
    if (valid_direct(disp)) {
        tcg_out_fmt_br(s, opd, ra, disp >> 2);
    } else {
        tcg_out_movi(s, TCG_TYPE_PTR, rb, (intptr_t)target);
        tcg_out_fmt_jmp(s, opi, ra, rb, (intptr_t)target);
    }
}

static void tcg_out_reset_tb(TCGContext *s, TCGReg reg)
{
    if (USE_REG_TB) {
        tcg_out_mem_long(s, INSN_LDA, TCG_REG_TB, reg,
                         -tcg_current_code_size(s));
    }
}

static void tcg_out_call(TCGContext *s, tcg_insn_unit *dest)
{
#if 0
    tcg_insn_unit check0 = dest[0];
    tcg_insn_unit check1 = dest[1];

    /* ??? Ideally we'd have access to Elf64_Sym.st_other, which
       would tell us definitively whether the target function uses
       the incoming PV value.  Make a simplifying assumption here
       that all of the compiler-generated code that we're calling
       either computes the GP from the PV in the first two insns
       or it doesn't use the PV at all.  This assumption holds in
       general for just about anything except some hand-written
       assembly, which we're not calling into.  */
    if ((check0 & 0xffff0000) == 0x27bb0000
        && (check1 & 0xffff0000) == 0x23bd0000) {
        /* Skip the GP computation.  We can do this even if the
           direct branch is out of range.  */
        dest += 2;
    }

    tcg_out_jbr(s, INSN_BSR, INSN_JSR, TCG_REG_RA, TCG_REG_PV, dest);
#else
    tcg_out_movi(s, TCG_TYPE_PTR, TCG_REG_PV, (intptr_t)dest);
    tcg_out_fmt_jmp(s, INSN_JSR, TCG_REG_RA, TCG_REG_PV, (intptr_t)dest);
#endif
    tcg_out_reset_tb(s, TCG_REG_RA);
}

static void tcg_out_deposit(TCGContext *s, TCGReg dest, TCGReg arg1,
                            TCGReg arg2, int ofs, int len, bool is_64)
{
    AlphaOpcode ins_opc, msk_opc;
    int bofs = ofs >> 3;

    switch (len) {
    case 8:
        ins_opc = INSN_INSBL;
        msk_opc = INSN_MSKBL;
        break;
    case 16:
        ins_opc = INSN_INSWL;
        msk_opc = INSN_MSKWL;
        break;
    case 32:
        ins_opc = INSN_INSLL;
        msk_opc = INSN_MSKLL;
        break;
    default:
        tcg_abort();
    }

    if (arg1 == TCG_REG_ZERO) {
        tcg_out_fmt_opi(s, ins_opc, arg2, bofs, dest);
        if (!is_64 && len + ofs >= 32) {
            tgen_ext32s(s, dest, dest);
        }
    } else if (arg2 == TCG_REG_ZERO) {
        if (!is_64 && len + ofs >= 32) {
            int mask = 0xff & ~(0xff << bofs);
            tcg_out_fmt_opi(s, INSN_ZAPNOT, arg1, mask, dest);
        } else {
            tcg_out_fmt_opi(s, msk_opc, arg1, bofs, dest);
        }
    } else {
        tcg_out_fmt_opi(s, ins_opc, arg2, bofs, TMP_REG1);
        tcg_out_fmt_opi(s, msk_opc, arg1, bofs, dest);
        tcg_out_fmt_opr(s, is_64 ? INSN_BIS : INSN_ADDL, dest, TMP_REG1, dest);
    }
}

static void tcg_out_extract(TCGContext *s, TCGReg dest, TCGReg arg,
                            int ofs, int len, bool is_64)
{
    AlphaOpcode opc;
    int bofs = ofs >> 3;

    switch (len) {
    case 8:
        opc = INSN_EXTBL;
        break;
    case 16:
        opc = INSN_EXTWL;
        break;
    case 24:
        tcg_debug_assert(ofs == 8);
        tcg_out_fmt_opi(s, INSN_INSLH, arg, 7, dest);
	return;
    case 32:
	if (!is_64) {
	    tcg_out_mov(s, TCG_TYPE_I32, dest, arg);
	    return;
	}
        opc = INSN_EXTLL;
        break;
    default:
        tcg_abort();
    }

    tcg_out_fmt_opi(s, opc, arg, bofs, dest);
}

/* The low bit of these entries indicates that the result of
   the comparison must be inverted.  This bit should not be
   output with the rest of the instruction.  */
static const int cmp_opc[16] = {
    [TCG_COND_EQ] = INSN_CMPEQ,
    [TCG_COND_NE] = INSN_CMPEQ | 1,
    [TCG_COND_LT] = INSN_CMPLT,
    [TCG_COND_GE] = INSN_CMPLT | 1,
    [TCG_COND_LE] = INSN_CMPLE,
    [TCG_COND_GT] = INSN_CMPLE | 1,
    [TCG_COND_LTU] = INSN_CMPULT,
    [TCG_COND_GEU] = INSN_CMPULT | 1,
    [TCG_COND_LEU] = INSN_CMPULE,
    [TCG_COND_GTU] = INSN_CMPULE | 1
};

static void tcg_out_setcond(TCGContext *s, TCGCond cond, TCGReg dest,
                            TCGReg c1, TCGArg c2, int c2const)
{
    int need_inv = cmp_opc[cond] & 1;
    AlphaOpcode opc = cmp_opc[cond] & ~1;

    if (c2const) {
        tcg_out_fmt_opi(s, opc, c1, c2, dest);
    } else {
        tcg_out_fmt_opr(s, opc, c1, c2, dest);
    }
    if (need_inv) {
        tcg_out_fmt_opi(s, INSN_XOR, dest, 1, dest);
    }
}

static void tcg_out_movcond(TCGContext *s, TCGCond cond, TCGReg dest,
                            TCGReg c1, TCGArg c2, int c2const,
                            TCGArg v1, int v1const)
{
    /* Note that unsigned comparisons are not present here, which means
       that their entries will contain zeros.  */
    static const AlphaOpcode cmov_opc[16] = {
        [TCG_COND_EQ] = INSN_CMOVEQ,
        [TCG_COND_NE] = INSN_CMOVNE,
        [TCG_COND_LT] = INSN_CMOVLT,
        [TCG_COND_GE] = INSN_CMOVGE,
        [TCG_COND_LE] = INSN_CMOVLE,
        [TCG_COND_GT] = INSN_CMOVGT
    };

    AlphaOpcode opc = 0;

    /* Notice signed comparisons vs zero.  These are handled by the
       cmov instructions directly.  */
    if (c2 == 0) {
        opc = cmov_opc[cond];
    }

    /* Otherwise, generate a comparison into a temporary.  */
    if (opc == 0) {
        int need_inv = cmp_opc[cond] & 1;
        opc = cmp_opc[cond] & ~1;

        if (c2const) {
            tcg_out_fmt_opi(s, opc, c1, c2, TMP_REG1);
        } else {
            tcg_out_fmt_opr(s, opc, c1, c2, TMP_REG1);
        }
        opc = (need_inv ? INSN_CMOVEQ : INSN_CMOVNE);
        c1 = TMP_REG1;
    }

    if (v1const) {
        tcg_out_fmt_opi(s, opc, c1, v1, dest);
    } else {
        tcg_out_fmt_opr(s, opc, c1, v1, dest);
    }
}

static void tcg_out_brcond(TCGContext *s, TCGCond cond, TCGReg arg1,
                           TCGArg arg2, int const_arg2, TCGLabel *l)
{
    /* Note that unsigned comparisons are not present here, which means
       that their entries will contain zeros.  */
    static const AlphaOpcode br_opc[16] = {
        [TCG_COND_EQ] = INSN_BEQ,
        [TCG_COND_NE] = INSN_BNE,
        [TCG_COND_LT] = INSN_BLT,
        [TCG_COND_GE] = INSN_BGE,
        [TCG_COND_LE] = INSN_BLE,
        [TCG_COND_GT] = INSN_BGT
    };

    AlphaOpcode opc = 0;

    /* Notice signed comparisons vs zero.  These are handled by the
       branch instructions directly.  */
    if (arg2 == 0) {
        opc = br_opc[cond];
    }

    /* Otherwise, generate a comparison into a temporary.  */
    if (opc == 0) {
        int need_inv = cmp_opc[cond] & 1;
        opc = cmp_opc[cond] & ~1;

        if (const_arg2) {
            tcg_out_fmt_opi(s, opc, arg1, arg2, TMP_REG1);
        } else {
            tcg_out_fmt_opr(s, opc, arg1, arg2, TMP_REG1);
        }
        opc = (need_inv ? INSN_BEQ : INSN_BNE);
        arg1 = TMP_REG1;
    }

    tcg_out_br_label(s, opc, arg1, l);
}

/* Note that these functions don't have normal C calling conventions.  */
typedef long divfn(long, long);
extern divfn __divl, __divlu, __reml, __remlu;
extern divfn __divq, __divqu, __remq, __remqu;

static void tcg_out_div(TCGContext *s, divfn fn)
{
    tcg_out_jbr(s, INSN_BSR, INSN_JSR, TCG_REG_T9, TMP_REG1,
                (tcg_insn_unit *)fn);
}

#if defined(CONFIG_SOFTMMU)
static void * const qemu_ld_helpers[16] = {
    [MO_UB]   = helper_ret_ldub_mmu,
    [MO_LEUW] = helper_le_lduw_mmu,
    [MO_LEUL] = helper_le_ldul_mmu,
    [MO_LEQ]  = helper_le_ldq_mmu,
    [MO_BEUW] = helper_be_lduw_mmu,
    [MO_BEUL] = helper_be_ldul_mmu,
    [MO_BEQ]  = helper_be_ldq_mmu,
};

static void * const qemu_st_helpers[16] = {
    [MO_UB]   = helper_ret_stb_mmu,
    [MO_LEUW] = helper_le_stw_mmu,
    [MO_LEUL] = helper_le_stl_mmu,
    [MO_LEQ]  = helper_le_stq_mmu,
    [MO_BEUW] = helper_be_stw_mmu,
    [MO_BEUL] = helper_be_stl_mmu,
    [MO_BEQ]  = helper_be_stq_mmu,
};

static TCGReg tcg_out_tlb_cmp(TCGContext *s, TCGMemOpIdx oi, TCGReg addr_reg,
                              tcg_insn_unit **label_ptr, bool is_ld)
{
    TCGMemOp memop = get_memop(oi);
    int mem_index = get_mmuidx(oi);
    intptr_t cmp_ofs
        = (is_ld
           ? offsetof(CPUArchState, tlb_table[mem_index][0].addr_read)
           : offsetof(CPUArchState, tlb_table[mem_index][0].addr_write));
    intptr_t add_ofs = offsetof(CPUArchState, tlb_table[mem_index][0].addend);
    uintptr_t addr_mask, tlb_mask;
    int s_bits = memop & MO_SIZE;
    int a_bits = get_alignment_bits(memop);

    /* Mask the page, plus the low bits of the access, into TMP_REG1.  Note
       that the low bits are added in order to catch unaligned accesses,
       as those bits won't be set in the TLB entry.  */
    if (a_bits < s_bits) {
        a_bits = s_bits;
    }
    addr_mask = TARGET_PAGE_MASK | ((1 << a_bits) - 1);

    /* Compute the index into the TLB into R1.  Note that the sign-extended
       high bits of a 32-bit address must be cleared.  */
    tlb_mask = (CPU_TLB_SIZE - 1) << CPU_TLB_ENTRY_BITS;
    if (TARGET_LONG_BITS == 32) {
        tlb_mask &= 0xfffffffful >> (TARGET_PAGE_BITS - CPU_TLB_ENTRY_BITS);
    }

    tcg_out_fmt_opi(s, INSN_SRL, addr_reg,
                    TARGET_PAGE_BITS - CPU_TLB_ENTRY_BITS, TCG_REG_A1);

    tcg_out_movi(s, TCG_TYPE_PTR, TCG_REG_A0, tlb_mask);
    tcg_out_movi(s, TCG_TYPE_PTR, TMP_REG1, addr_mask);

    tcg_out_fmt_opr(s, INSN_AND, TCG_REG_A1, TCG_REG_A0, TCG_REG_A1);
    tcg_out_fmt_opr(s, INSN_AND, TMP_REG1, addr_reg, TMP_REG1);

    tcg_out_fmt_opr(s, INSN_ADDQ, TCG_REG_A1, TCG_AREG0, TCG_REG_A1);

    /* Compensate for very large offsets.  */
    if (add_ofs >= 0x8000) {
        /* Most target env are smaller than 32k; none are larger than 64k.
           Simplify the logic here merely to offset by 0x7ff0, giving us a
           range just shy of 64k.  Check this assumption.  */
        QEMU_BUILD_BUG_ON(offsetof(CPUArchState,
                                   tlb_table[NB_MMU_MODES - 1][1])
                          > 0x7ff0 + 0x7fff);
        tcg_out_fmt_mem(s, INSN_LDA, TCG_REG_A1, TCG_REG_A1, 0x7ff0);
        cmp_ofs -= 0x7ff0;
        add_ofs -= 0x7ff0;
    }

    tcg_out_ld_sz(s, (TARGET_LONG_BITS == 64 ? MO_Q : MO_SL),
                  TMP_REG2, TCG_REG_A1, cmp_ofs);
    tcg_out_ld(s, TCG_TYPE_I64, TCG_REG_A1, TCG_REG_A1, add_ofs);

    /* Zero extend the address into A0.  */
    if (TARGET_LONG_BITS == 32) {
        tgen_ext32u(s, addr_reg, TCG_REG_A0);
        addr_reg = TCG_REG_A0;
    }

    /* Compare TMP1 with the value loaded from the TLB.  */
    tcg_out_fmt_opr(s, INSN_CMPEQ, TMP_REG1, TMP_REG2, TMP_REG1);

    /* Finish the host address into A1.  */
    tcg_out_fmt_opr(s, INSN_ADDQ, TCG_REG_A1, addr_reg, TCG_REG_A1);

    *label_ptr = s->code_ptr;
    tcg_out_fmt_br(s, INSN_BEQ, TMP_REG1, *s->code_ptr);

    return TCG_REG_A1;
}

/* Record the context of a call to the out of line helper code for the slow
   path for a load or store, so that we can later generate the correct
   helper code.  */
static void add_qemu_ldst_label(TCGContext *s, bool is_ld, TCGMemOpIdx oi,
                                TCGReg data_reg, TCGReg addr_reg,
                                tcg_insn_unit *raddr, tcg_insn_unit *label_ptr)
{
    TCGLabelQemuLdst *label = new_ldst_label(s);

    label->is_ld = is_ld;
    label->oi = oi;
    label->datalo_reg = data_reg;
    label->addrlo_reg = addr_reg;
    label->raddr = raddr;
    label->label_ptr[0] = label_ptr;
}

static void tcg_out_qemu_ld_slow_path(TCGContext *s, TCGLabelQemuLdst *lb)
{
    TCGMemOpIdx oi = lb->oi;
    TCGMemOp memop = get_memop(oi);

    patch_reloc(lb->label_ptr[0], R_ALPHA_BRADDR, (intptr_t)s->code_ptr, 0);

    tcg_out_mov(s, TCG_TYPE_PTR, TCG_REG_A0, TCG_AREG0);
    tcg_out_mov(s, TCG_TYPE_TL, TCG_REG_A1, lb->addrlo_reg);
    tcg_out_movi(s, TCG_TYPE_I32, TCG_REG_A2, oi);
    tcg_out_movi(s, TCG_TYPE_PTR, TCG_REG_A3, (intptr_t)lb->raddr);

    tcg_out_call(s, qemu_ld_helpers[memop & (MO_BSWAP | MO_SIZE)]);

    /* Note that the chosen helpers zero-extend.  */
    if (memop & MO_SIGN) {
        tgen_extend(s, memop & MO_SSIZE, TCG_REG_V0, lb->datalo_reg);
    } else {
        tcg_out_mov(s, TCG_TYPE_I64, lb->datalo_reg, TCG_REG_V0);
    }

    tcg_out_br_direct(s, INSN_BR, TCG_REG_ZERO, lb->raddr);
}

static void tcg_out_qemu_st_slow_path(TCGContext *s, TCGLabelQemuLdst *lb)
{
    TCGMemOp oi = lb->oi;
    TCGMemOp memop = get_memop(lb->oi);

    patch_reloc(lb->label_ptr[0], R_ALPHA_BRADDR, (intptr_t)s->code_ptr, 0);

    tcg_out_mov(s, TCG_TYPE_I64, TCG_REG_A0, TCG_AREG0);
    tcg_out_mov(s, TCG_TYPE_TL, TCG_REG_A1, lb->addrlo_reg);
    tcg_out_mov(s, TCG_TYPE_I64, TCG_REG_A2, lb->datalo_reg);
    tcg_out_movi(s, TCG_TYPE_I32, TCG_REG_A3, oi);
    tcg_out_movi(s, TCG_TYPE_PTR, TCG_REG_A4, (intptr_t)lb->raddr);

    tcg_out_call(s, qemu_st_helpers[memop & MO_SIZE]);

    tcg_out_br_direct(s, INSN_BR, TCG_REG_ZERO, lb->raddr);
}
#endif /* SOFTMMU */

static void tcg_out_qemu_ld(TCGContext *s, const TCGReg data,
                            const TCGReg addr, TCGMemOpIdx oi)
{
    TCGMemOp memop = get_memop(oi);
    TCGReg base;
    long ofs;

#if defined(CONFIG_SOFTMMU)
    tcg_insn_unit *label_ptr;
    base = tcg_out_tlb_cmp(s, oi, addr, &label_ptr, 1);
    ofs = 0;
#else
    if (TARGET_LONG_BITS == 32) {
        base = TCG_REG_A1;
        tgen_ext32u(s, addr, base);
    } else {
        base = addr;
    }
    if (USE_GUEST_BASE_REG) {
        tcg_out_fmt_opr(s, INSN_ADDQ, base, TCG_GUEST_BASE_REG, TCG_REG_A1);
        base = TCG_REG_A1;
        ofs = 0;
    } else {
        ofs = guest_base;
    }
#endif

    /* Perform the actual load.  */
    tcg_out_ld_sz(s, memop, data, base, ofs);

#if defined(CONFIG_SOFTMMU)
    add_qemu_ldst_label(s, 1, oi, data, addr, s->code_ptr, label_ptr);
#endif
}

static void tcg_out_qemu_st(TCGContext *s, const TCGReg data,
                            const TCGReg addr, TCGMemOpIdx oi)
{
    TCGMemOp memop = get_memop(oi);
    TCGReg base, out;
    long ofs;

#if defined(CONFIG_SOFTMMU)
    tcg_insn_unit *label_ptr;
    base = tcg_out_tlb_cmp(s, oi, addr, &label_ptr, 0);
    ofs = 0;
#else
    if (TARGET_LONG_BITS == 32) {
        base = TCG_REG_A1;
        tgen_ext32u(s, addr, base);
    } else {
        base = addr;
    }
    if (USE_GUEST_BASE_REG) {
        tcg_out_fmt_opr(s, INSN_ADDQ, base, TCG_GUEST_BASE_REG, TCG_REG_A1);
        base = TCG_REG_A1;
        ofs = 0;
    } else {
        ofs = guest_base;
    }
#endif

    /* Byte swap if necessary.  */
    out = data;
    if ((memop & MO_BSWAP) && data != TCG_REG_ZERO) {
        tgen_bswap(s, memop & MO_SIZE, data, TCG_REG_A0);
        out = TCG_REG_A0;
    }

    /* Perform the actual store.  */
    tcg_out_st_sz(s, memop, out, base, ofs);

#if defined(CONFIG_SOFTMMU)
    add_qemu_ldst_label(s, 0, oi, data, addr, s->code_ptr, label_ptr);
#endif
}

/* Pointer to the epilogue.  */
static tcg_insn_unit *tb_ret_addr;

static void tcg_out_op(TCGContext *s, TCGOpcode opc,
                       const TCGArg *args, const int *const_args)
{
    TCGArg arg0, arg1, arg2;
    AlphaOpcode insn;
    int c, c2;

    arg0 = args[0];
    arg1 = args[1];
    arg2 = args[2];
    c2 = const_args[2];

    switch (opc) {
    case INDEX_op_exit_tb:
        tcg_out_movi(s, TCG_TYPE_PTR, TMP_REG1, (intptr_t)tb_ret_addr);
        tcg_out_movi(s, TCG_TYPE_I64, TCG_REG_V0, arg0);
        tcg_out_fmt_jmp(s, INSN_RET, TCG_REG_ZERO, TMP_REG1, 0);
        break;

    case INDEX_op_goto_tb:
        if (s->tb_jmp_insn_offset) {
            /* Direct jump method.  In the general case we output:
                 ldah $tb,hi($tb|$gp)
                 lda  $tb,lo($tb)
                 jmp  $31,($tb),0
               We need to modify two instructions to set the link.
               We want that modification to be atomic, so we arrange
               for the ldah+lda pair to be 8-byte aligned.  */
            if ((intptr_t)s->code_ptr & 7) {
                tcg_out32(s, INSN_NOP);
            }
            s->tb_jmp_insn_offset[arg0] = tcg_current_code_size(s);
            s->code_ptr += 2;
        } else {
            /* Indirect jump method.  */
            tcg_out_ld(s, TCG_TYPE_PTR, TCG_REG_TB, TCG_REG_ZERO,
                       (intptr_t)(s->tb_jmp_target_addr + arg0));
        }
        tcg_out_fmt_jmp(s, INSN_JMP, TCG_REG_ZERO, TCG_REG_TB, 0);
        s->tb_jmp_reset_offset[arg0] = tcg_current_code_size(s);

        /* The "unlinked" state of a TB has the jump fall through.
           Therefore we need to reset TCG_REG_TB to our top.  */
        tcg_out_reset_tb(s, TCG_REG_TB);
        break;

    case INDEX_op_br:
        tcg_out_br_label(s, INSN_BR, TCG_REG_ZERO, arg_label(arg0));
        break;

    case INDEX_op_ld8u_i32:
    case INDEX_op_ld8u_i64:
        c = MO_UB;
        goto do_load;
    case INDEX_op_ld8s_i32:
    case INDEX_op_ld8s_i64:
        c = MO_SB;
        goto do_load;
    case INDEX_op_ld16u_i32:
    case INDEX_op_ld16u_i64:
        c = MO_UW;
        goto do_load;
    case INDEX_op_ld16s_i32:
    case INDEX_op_ld16s_i64:
        c = MO_SW;
        goto do_load;
    case INDEX_op_ld32u_i64:
        c = MO_UL;
        goto do_load;
    case INDEX_op_ld_i32:
    case INDEX_op_ld32s_i64:
        c = MO_SL;
        goto do_load;
    case INDEX_op_ld_i64:
        c = MO_Q;
    do_load:
        tcg_out_ld_sz(s, c, arg0, arg1, arg2);
        break;

    case INDEX_op_st8_i32:
    case INDEX_op_st8_i64:
        c = MO_8;
        goto do_store;
    case INDEX_op_st16_i32:
    case INDEX_op_st16_i64:
        c = MO_16;
        goto do_store;
    case INDEX_op_st_i32:
    case INDEX_op_st32_i64:
        c = MO_32;
        goto do_store;
    case INDEX_op_st_i64:
        c = MO_64;
    do_store:
        tcg_out_st_sz(s, c, arg0, arg1, arg2);
        break;

    case INDEX_op_sub_i32:
        if (c2) {
            arg2 = -arg2;
        } else {
            insn = INSN_SUBL;
            goto do_arith;
        }
        /* FALLTHRU */

    case INDEX_op_add_i32:
        if (c2) {
            if ((int32_t)arg2 >= 0) {
                tcg_out_fmt_opi(s, INSN_ADDL, arg1, (int32_t)arg2, arg0);
            } else {
                tcg_out_fmt_opi(s, INSN_SUBL, arg1, -(int32_t)arg2, arg0);
            }
        } else {
            insn = INSN_ADDL;
            goto do_arith;
        }
        break;

    case INDEX_op_sub_i64:
        if (c2) {
            arg2 = -arg2;
        } else {
            insn = INSN_SUBQ;
            goto do_arith;
        }
        /* FALLTHRU */

    case INDEX_op_add_i64:
        if (c2) {
            tcg_out_mem_long(s, INSN_LDA, arg0, arg1, arg2);
        } else {
            insn = INSN_ADDQ;
            goto do_arith;
        }
        break;

    case INDEX_op_mul_i32:
        insn = INSN_MULL;
        goto do_arith;

    case INDEX_op_mul_i64:
        insn = INSN_MULQ;
        goto do_arith;

    case INDEX_op_muluh_i64:
        insn = INSN_UMULH;
        goto do_arith;

    case INDEX_op_and_i32:
        arg2 = (int32_t)arg2;
    case INDEX_op_and_i64:
        if (c2) {
            tgen_andi(s, arg1, arg2, arg0);
            break;
        }
        insn = INSN_AND;
        goto do_arith;

    case INDEX_op_andc_i32:
        arg2 = (int32_t)arg2;
    case INDEX_op_andc_i64:
        if (c2) {
            tgen_andi(s, arg1, ~arg2, arg0);
            break;
        }
        insn = INSN_BIC;
        goto do_arith;

    case INDEX_op_or_i32:
    case INDEX_op_or_i64:
        insn = INSN_BIS;
        goto do_arith;

    case INDEX_op_orc_i32:
    case INDEX_op_orc_i64:
        insn = INSN_ORNOT;
        goto do_arith;

    case INDEX_op_xor_i32:
    case INDEX_op_xor_i64:
        insn = INSN_XOR;
        goto do_arith;

    case INDEX_op_eqv_i32:
    case INDEX_op_eqv_i64:
        insn = INSN_EQV;
        goto do_arith;

    case INDEX_op_shl_i32:
        /* Make sure to preserve the sign-extension in the result.
           Thus the special casing of shifts by 1, 2 and 3.  */
        if (c2) {
            arg2 &= 31;
            switch (arg2) {
            case 0:
                tcg_out_mov(s, TCG_TYPE_I32, arg0, arg1);
                break;
            case 1:
                tcg_out_fmt_opr(s, INSN_ADDL, arg1, arg1, arg0);
                break;
            case 2:
                tcg_out_fmt_opr(s, INSN_S4ADDL, arg1, TCG_REG_ZERO, arg0);
                break;
            case 3:
                tcg_out_fmt_opr(s, INSN_S8ADDL, arg1, TCG_REG_ZERO, arg0);
                break;
            default:
                tcg_out_fmt_opi(s, INSN_SLL, arg1, arg2, arg0);
                tgen_ext32s(s, arg0, arg0);
                break;
            }
        } else {
            /* ??? TCG has no requirement to truncate the shift yet.  */
            tcg_out_fmt_opr(s, INSN_SLL, arg1, arg2, arg0);
            tgen_ext32s(s, arg0, arg0);
        }
        break;

    case INDEX_op_shl_i64:
        insn = INSN_SLL;
        goto do_arith;

    case INDEX_op_shr_i32:
        /* Recall that the input is sign-extended, which means that we
           need to mask the high bits that we'll be shifting in.  There
           are three common cases that can perform the shift+mask in
           one instruction.  Otherwise, we'll need a separate mask.  */
        if (c2) {
            arg2 &= 31;
            switch (arg2) {
            case 0:
                tcg_out_mov(s, TCG_TYPE_I32, arg0, arg1);
                break;
            case 8:
                tcg_out_fmt_opi(s, INSN_INSLH, arg1, 7, arg0);
                break;
            case 16:
                tcg_out_fmt_opi(s, INSN_EXTWL, arg1, 2, arg0);
                break;
            case 24:
                tcg_out_fmt_opi(s, INSN_EXTBL, arg1, 3, arg0);
                break;
            case 25 ... 31:
                tcg_out_fmt_opi(s, INSN_SRL, arg1, arg2, arg0);
                tcg_out_fmt_opi(s, INSN_AND, arg0,
                                (1 << (32 - arg2)) - 1, arg0);
                break;
            default:
                tgen_ext32u(s, arg1, arg0);
                tcg_out_fmt_opi(s, INSN_SRL, arg0, arg2, arg0);
                break;
            }
        } else {
            /* Here we need to be careful about a shift of zero,
               for which we'd need to re-sign-extend the output.  */
            tgen_ext32u(s, arg1, TMP_REG1);
            tcg_out_fmt_opr(s, INSN_SRL, TMP_REG1, arg2, arg0);
            tgen_ext32s(s, arg0, arg0);
        }
        break;

    case INDEX_op_shr_i64:
        insn = INSN_SRL;
        goto do_arith;

    case INDEX_op_sar_i32:
        /* Note that since the input is already sign-extended,
           we need not do anything special here.  */
    case INDEX_op_sar_i64:
        insn = INSN_SRA;
        goto do_arith;

    do_arith:
        if (c2) {
            tcg_out_fmt_opi(s, insn, arg1, arg2, arg0);
        } else {
            tcg_out_fmt_opr(s, insn, arg1, arg2, arg0);
        }
        break;

    case INDEX_op_not_i32:
    case INDEX_op_not_i64:
        tcg_out_fmt_opr(s, INSN_ORNOT, TCG_REG_ZERO, arg1, arg0);
        break;

    case INDEX_op_deposit_i32:
        tcg_out_deposit(s, arg0, arg1, arg2, args[3], args[4], 0);
        break;
    case INDEX_op_deposit_i64:
        tcg_out_deposit(s, arg0, arg1, arg2, args[3], args[4], 1);
        break;

    case INDEX_op_extract_i32:
        tcg_out_extract(s, arg0, arg1, arg2, args[3], 0);
        break;
    case INDEX_op_extract_i64:
        tcg_out_extract(s, arg0, arg1, arg2, args[3], 1);
        break;

    case INDEX_op_brcond_i32:
        arg1 = (int32_t)arg1;
    case INDEX_op_brcond_i64:
        tcg_out_brcond(s, arg2, arg0, arg1, const_args[1], arg_label(args[3]));
        break;

    case INDEX_op_setcond_i32:
    case INDEX_op_setcond_i64:
        tcg_out_setcond(s, args[3], arg0, arg1, arg2, c2);
        break;

    case INDEX_op_movcond_i32:
    case INDEX_op_movcond_i64:
        tcg_out_movcond(s, args[5], arg0, arg1, arg2, c2,
                        args[3], const_args[3]);
        break;

    case INDEX_op_ext8s_i32:
    case INDEX_op_ext8s_i64:
        c = MO_SB;
        goto do_sign_extend;
    case INDEX_op_ext16s_i32:
    case INDEX_op_ext16s_i64:
        c = MO_SW;
        goto do_sign_extend;
    case INDEX_op_ext32s_i64:
    case INDEX_op_extrl_i64_i32:
    case INDEX_op_ext_i32_i64:
        c = MO_SL;
    do_sign_extend:
        tgen_extend(s, c, arg1, arg0);
        break;

    case INDEX_op_extu_i32_i64:
        tgen_ext32u(s, arg0, arg1);
        break;
    case INDEX_op_extrh_i64_i32:
        tcg_out_fmt_opi(s, INSN_SRA, arg1, 32, arg0);
        break;

    case INDEX_op_div_i32:
        tcg_out_div(s, __divl);
        break;
    case INDEX_op_rem_i32:
        tcg_out_div(s, __reml);
        break;
    case INDEX_op_divu_i32:
        tcg_out_div(s, __divlu);
        break;
    case INDEX_op_remu_i32:
        tcg_out_div(s, __remlu);
        break;
    case INDEX_op_div_i64:
        tcg_out_div(s, __divq);
        break;
    case INDEX_op_rem_i64:
        tcg_out_div(s, __remq);
        break;
    case INDEX_op_divu_i64:
        tcg_out_div(s, __divqu);
        break;
    case INDEX_op_remu_i64:
        tcg_out_div(s, __remqu);
        break;

    case INDEX_op_bswap16_i32:
    case INDEX_op_bswap16_i64:
        c = MO_UW;
        goto do_bswap;
    case INDEX_op_bswap32_i32:
        c = MO_SL;
        goto do_bswap;
    case INDEX_op_bswap32_i64:
        c = MO_UL;
        goto do_bswap;
    case INDEX_op_bswap64_i64:
        c = MO_Q;
    do_bswap:
        tgen_bswap(s, c, arg1, arg0);
        break;

    case INDEX_op_clz_i64:
        tgen_ctxz(s, INSN_CTLZ, arg0, arg1, arg2, c2);
        break;
    case INDEX_op_ctz_i64:
        tgen_ctxz(s, INSN_CTTZ, arg0, arg1, arg2, c2);
        break;
    case INDEX_op_ctpop_i64:
        tcg_out_fmt_opr(s, INSN_CTPOP, TCG_REG_ZERO, arg1, arg0);
        break;

    case INDEX_op_qemu_ld_i32:
        /* Make sure 32-bit data stays sign-extended.  */
        if ((get_memop(arg2) & MO_SIZE) == MO_32) {
            arg2 |= make_memop_idx(MO_SIGN, 0);
        }
        /* FALLTHRU */
    case INDEX_op_qemu_ld_i64:
        tcg_out_qemu_ld(s, arg0, arg1, arg2);
        break;

    case INDEX_op_qemu_st_i32:
    case INDEX_op_qemu_st_i64:
        tcg_out_qemu_st(s, arg0, arg1, arg2);
        break;

    case INDEX_op_mb:
        tcg_out32(s, (arg2 & TCG_MO_ALL) == TCG_MO_ST_ST ? INSN_WMB : INSN_MB);
        break;

    case INDEX_op_mov_i32:      /* Always emitted via tcg_out_mov. */
    case INDEX_op_mov_i64:
    case INDEX_op_movi_i32:     /* Always emitted via tcg_out_movi. */
    case INDEX_op_movi_i64:
    case INDEX_op_call:         /* Always emitted via tcg_out_call. */
    default:
        tcg_abort();
    }
}

static const TCGTargetOpDef alpha_op_defs[] = {
    { INDEX_op_exit_tb,         { } },
    { INDEX_op_goto_tb,         { } },
    { INDEX_op_br,              { } },
    { INDEX_op_mb,              { } },

    { INDEX_op_ld8u_i32,        { "r", "r" } },
    { INDEX_op_ld8s_i32,        { "r", "r" } },
    { INDEX_op_ld16u_i32,       { "r", "r" } },
    { INDEX_op_ld16s_i32,       { "r", "r" } },
    { INDEX_op_ld_i32,          { "r", "r" } },
    { INDEX_op_st8_i32,         { "rJ", "r" } },
    { INDEX_op_st16_i32,        { "rJ", "r" } },
    { INDEX_op_st_i32,          { "rJ", "r" } },

    { INDEX_op_add_i32,         { "r", "rJ", "rK" } },
    { INDEX_op_mul_i32,         { "r", "rJ", "rI" } },
    { INDEX_op_sub_i32,         { "r", "rJ", "rK" } },
    { INDEX_op_and_i32,         { "r", "rJ", "rM" } },
    { INDEX_op_or_i32,          { "r", "rJ", "rI" } },
    { INDEX_op_xor_i32,         { "r", "rJ", "rI" } },
    { INDEX_op_andc_i32,        { "r", "rJ", "rM" } },
    { INDEX_op_orc_i32,         { "r", "rJ", "rI" } },
    { INDEX_op_eqv_i32,         { "r", "rJ", "rI" } },
    { INDEX_op_not_i32,         { "r", "rI" } },

    { INDEX_op_shl_i32,         { "r", "rJ", "rI" } },
    { INDEX_op_shr_i32,         { "r", "rJ", "rI" } },
    { INDEX_op_sar_i32,         { "r", "rJ", "rI" } },

    { INDEX_op_deposit_i32,     { "r", "rJ", "rJ" } },
    { INDEX_op_extract_i32,     { "r", "r" } },

    { INDEX_op_div_i32,         { "c", "a", "b" } },
    { INDEX_op_rem_i32,         { "c", "a", "b" } },
    { INDEX_op_divu_i32,        { "c", "a", "b" } },
    { INDEX_op_remu_i32,        { "c", "a", "b" } },

    { INDEX_op_brcond_i32,      { "rJ", "rI" } },
    { INDEX_op_setcond_i32,     { "r", "rJ", "rI" } },
    { INDEX_op_movcond_i32,     { "r", "rJ", "rI", "rI", "0" } },

    { INDEX_op_ext_i32_i64,     { "r", "r" } },
    { INDEX_op_extu_i32_i64,    { "r", "r" } },
    { INDEX_op_extrl_i64_i32,   { "r", "r" } },
    { INDEX_op_extrh_i64_i32,   { "r", "r" } },

    { INDEX_op_clz_i64,         { "r", "r", "rI" },
    { INDEX_op_ctz_i64,         { "r", "r", "rI" },
    { INDEX_op_ctpop_i64,       { "r", "r" },

    { INDEX_op_ld8u_i64,        { "r", "r" } },
    { INDEX_op_ld8s_i64,        { "r", "r" } },
    { INDEX_op_ld16u_i64,       { "r", "r" } },
    { INDEX_op_ld16s_i64,       { "r", "r" } },
    { INDEX_op_ld32u_i64,       { "r", "r" } },
    { INDEX_op_ld32s_i64,       { "r", "r" } },
    { INDEX_op_ld_i64,          { "r", "r" } },
    { INDEX_op_st8_i64,         { "rJ", "r" } },
    { INDEX_op_st16_i64,        { "rJ", "r" } },
    { INDEX_op_st32_i64,        { "rJ", "r" } },
    { INDEX_op_st_i64,          { "rJ", "r" } },

    { INDEX_op_add_i64,         { "r", "rJ", "ri" } },
    { INDEX_op_mul_i64,         { "r", "rJ", "rI" } },
    { INDEX_op_muluh_i64,       { "r", "rJ", "rI" } },
    { INDEX_op_sub_i64,         { "r", "rJ", "ri" } },
    { INDEX_op_and_i64,         { "r", "rJ", "rM" } },
    { INDEX_op_or_i64,          { "r", "rJ", "rI" } },
    { INDEX_op_xor_i64,         { "r", "rJ", "rI" } },
    { INDEX_op_andc_i64,        { "r", "rJ", "rM" } },
    { INDEX_op_orc_i64,         { "r", "rJ", "rI" } },
    { INDEX_op_eqv_i64,         { "r", "rJ", "rI" } },
    { INDEX_op_not_i64,         { "r", "rI" } },

    { INDEX_op_shl_i64,         { "r", "rJ", "rI" } },
    { INDEX_op_shr_i64,         { "r", "rJ", "rI" } },
    { INDEX_op_sar_i64,         { "r", "rJ", "rI" } },

    { INDEX_op_deposit_i64,     { "r", "rJ", "rJ" } },
    { INDEX_op_extract_i64,     { "r", "r" } },

    { INDEX_op_div_i64,         { "c", "a", "b" } },
    { INDEX_op_rem_i64,         { "c", "a", "b" } },
    { INDEX_op_divu_i64,        { "c", "a", "b" } },
    { INDEX_op_remu_i64,        { "c", "a", "b" } },

    { INDEX_op_brcond_i64,      { "rJ", "rI" } },
    { INDEX_op_setcond_i64,     { "r", "rJ", "rI" } },
    { INDEX_op_movcond_i64,     { "r", "rJ", "rI", "rI", "0" } },

    { INDEX_op_ext8s_i32,       { "r", "rJ" } },
    { INDEX_op_ext16s_i32,      { "r", "rJ" } },
    { INDEX_op_ext8s_i64,       { "r", "rJ" } },
    { INDEX_op_ext16s_i64,      { "r", "rJ" } },
    { INDEX_op_ext32s_i64,      { "r", "rJ" } },

    { INDEX_op_bswap16_i32,     { "r", "rJ" } },
    { INDEX_op_bswap32_i32,     { "r", "rJ" } },
    { INDEX_op_bswap16_i64,     { "r", "rJ" } },
    { INDEX_op_bswap32_i64,     { "r", "rJ" } },
    { INDEX_op_bswap64_i64,     { "r", "rJ" } },

    { INDEX_op_qemu_ld_i32,     { "r", "L" } },
    { INDEX_op_qemu_ld_i64,     { "r", "L" } },
    { INDEX_op_qemu_st_i32,     { "LJ", "L" } },
    { INDEX_op_qemu_st_i64,     { "LJ", "L" } },
    { -1 },
};


/*
 * Generate global QEMU prologue and epilogue code
 */

/* Compute frame size via macros, to share between tcg_target_qemu_prologue
   and tcg_register_jit.  */

static const TCGReg save_regs[] = {
    TCG_REG_RA,
    TCG_REG_S0,
    TCG_REG_S1,
    TCG_REG_S2,
    TCG_REG_S3,
    TCG_REG_S4,
    TCG_REG_S5,
    TCG_REG_S6,
};

#define REG_SIZE    (ARRAY_SIZE(save_regs) * 8)
#define FRAME_SIZE  ((TCG_STATIC_CALL_ARGS_SIZE \
                      + CPU_TEMP_BUF_NLONGS * sizeof(long) \
                      + REG_SIZE + TCG_TARGET_STACK_ALIGN - 1) \
                     & -TCG_TARGET_STACK_ALIGN)
#define SAVE_OFS    (FRAME_SIZE - REG_SIZE)

void tcg_target_qemu_prologue(TCGContext *s)
{
    long i;

    /* The shape of the stack frame is:
       input sp
         [ Register save area ]
         [ CPU_TEMP_BUF_NLONGS ]
         [ TCG_STATIC_CALL_ARGS_SIZE ]
       sp
    */

    tcg_set_frame(s, TCG_REG_SP, TCG_STATIC_CALL_ARGS_SIZE,
                  CPU_TEMP_BUF_NLONGS * sizeof(long));

    /* TB Prologue.  */

    /* Allocate the stack frame.  */
    tcg_out_fmt_mem(s, INSN_LDA, TCG_REG_SP, TCG_REG_SP, -FRAME_SIZE);

    /* Save all callee saved registers.  */
    for (i = 0; i < ARRAY_SIZE(save_regs); i++) {
        tcg_out_fmt_mem(s, INSN_STQ, save_regs[i], TCG_REG_SP, SAVE_OFS + i*8);
    }

    /* Copy the ENV pointer into place.  */
    tcg_out_mov(s, TCG_TYPE_PTR, TCG_AREG0, TCG_REG_A0);

#if !defined(CONFIG_SOFTMMU)
    /* Setup TCG_GUEST_BASE_REG if desired.  */
    if (USE_GUEST_BASE_REG) {
        tcg_out_movi(s, TCG_TYPE_PTR, TCG_GUEST_BASE_REG, guest_base);
        tcg_regset_set_reg(s->reserved_regs, TCG_GUEST_BASE_REG);
    }
#endif

    /* Invoke the TB.  */
    if (USE_REG_TB) {
        tcg_out_mov(s, TCG_TYPE_PTR, TCG_REG_TB, TCG_REG_A1);
    }
    tcg_out_fmt_jmp(s, INSN_JSR, TCG_REG_ZERO, TCG_REG_A1, 0);

    /* TB epilogue. */
    tb_ret_addr = s->code_ptr;

    /* Restore all callee saved registers.  */
    for (i = 0; i < ARRAY_SIZE(save_regs); i++) {
        tcg_out_fmt_mem(s, INSN_LDQ, save_regs[i], TCG_REG_SP, SAVE_OFS + i*8);
    }

    /* Deallocate the stack frame.  */
    tcg_out_fmt_mem(s, INSN_LDA, TCG_REG_SP, TCG_REG_SP, FRAME_SIZE);

    tcg_out_fmt_jmp(s, INSN_RET, TCG_REG_ZERO, TCG_REG_RA, 0);
}


void tcg_target_init(TCGContext *s)
{
    tcg_regset_set32(tcg_target_available_regs[TCG_TYPE_I32], 0, 0xffffffff);
    tcg_regset_set32(tcg_target_available_regs[TCG_TYPE_I64], 0, 0xffffffff);

    tcg_regset_set32(tcg_target_call_clobber_regs, 0, 0xffffffff);
    tcg_regset_reset_reg(tcg_target_call_clobber_regs, TCG_REG_S0);
    tcg_regset_reset_reg(tcg_target_call_clobber_regs, TCG_REG_S1);
    tcg_regset_reset_reg(tcg_target_call_clobber_regs, TCG_REG_S2);
    tcg_regset_reset_reg(tcg_target_call_clobber_regs, TCG_REG_S3);
    tcg_regset_reset_reg(tcg_target_call_clobber_regs, TCG_REG_S4);
    tcg_regset_reset_reg(tcg_target_call_clobber_regs, TCG_REG_S5);
    tcg_regset_reset_reg(tcg_target_call_clobber_regs, TCG_REG_S6);
    tcg_regset_reset_reg(tcg_target_call_clobber_regs, TCG_REG_GP);
    tcg_regset_reset_reg(tcg_target_call_clobber_regs, TCG_REG_SP);

    tcg_regset_clear(s->reserved_regs);
    tcg_regset_set_reg(s->reserved_regs, TCG_REG_GP);
    tcg_regset_set_reg(s->reserved_regs, TCG_REG_SP);
    tcg_regset_set_reg(s->reserved_regs, TCG_REG_ZERO);
    tcg_regset_set_reg(s->reserved_regs, TMP_REG1);
    tcg_regset_set_reg(s->reserved_regs, TMP_REG2);

    if (USE_REG_TB) {
        tcg_regset_set_reg(s->reserved_regs, TCG_REG_TB);
    }

    tcg_add_target_add_op_defs(alpha_op_defs);
}

#ifdef USE_DIRECT_JUMP
void tb_set_jmp_target2(TranslationBlock *tb, uintptr_t jaddr, uintptr_t addr)
{
    long disp, insn1, insn2;

    /* Try a direct branch first.  */
    disp = addr - (jaddr + 4);
    if (!USE_REG_TB && valid_direct(disp)) {
        insn1 = INSN_BR | INSN_RA(TCG_REG_ZERO) | INSN_DISP21(disp >> 2);
        /* The second insn is dead code, but don't leave the memory totally
           uninitialized.  If the garbage is an illegal insn the prefetch
           unit can flush the pipeline in order to prove the illegal insn
           isn't executed.  */
        insn2 = INSN_NOP;
    } else {
        long hi, lo, base;
        TCGReg basereg;

        if (USE_REG_TB) {
            base = (uintptr_t)tb->tc_ptr;
            basereg = TCG_REG_TB;
        } else {
            register uintptr_t gp __asm__("$29");
            base = gp;
            basereg = TCG_REG_GP;
        }

        /* Failing that, do an ldah+lda pair to make the distance.
           Given that the code buffer is limited to 2G, this should
           always reach.  */
        disp = addr - base;
        lo = (int16_t)disp;
        hi = (int16_t)((disp - lo) >> 16);
        assert((hi << 16) + lo == disp);
        insn1 = INSN_LDAH | INSN_RA(TCG_REG_TB)
                | INSN_RB(basereg) | INSN_DISP16(hi);
        insn2 = INSN_LDA | INSN_RA(TCG_REG_TB)
                | INSN_RB(TCG_REG_TB) | INSN_DISP16(lo);
    }
    *(uint64_t *)jaddr = insn1 + (insn2 << 32);

    flush_icache_range(jaddr, jaddr + 8);
}
#endif /* USE_DIRECT_JUMP */

typedef struct {
    DebugFrameCIE cie;
    DebugFrameFDEHeader fde;
    uint8_t fde_def_cfa[4];
    uint8_t fde_reg_ofs[ARRAY_SIZE(save_regs) * 2];
} DebugFrame;

#define ELF_HOST_MACHINE EM_ALPHA

/* We're expecting a 2 byte uleb128 encoded value.  */
QEMU_BUILD_BUG_ON(FRAME_SIZE >= (1 << 14));

static DebugFrame debug_frame = {
    .cie.len = sizeof(DebugFrameCIE)-4, /* length after .len member */
    .cie.id = -1,
    .cie.version = 1,
    .cie.code_align = 1,
    .cie.data_align = 0x78,             /* sleb128 -8 */
    .cie.return_column = 26,

    /* Total FDE size does not include the "len" member.  */
    .fde.len = sizeof(DebugFrame) - offsetof(DebugFrame, fde.cie_offset),

    .fde_def_cfa = {
        12, 30,                         /* DW_CFA_def_cfa sp, ... */
        (FRAME_SIZE & 0x7f) | 0x80,     /* ... uleb128 FRAME_SIZE */
        (FRAME_SIZE >> 7)
    },
    .fde_reg_ofs = {
        0x8f, 1,                        /* DW_CFA_offset, s6, -8 */
        0x8e, 2,                        /* DW_CFA_offset, s5, -16 */
        0x8d, 3,                        /* DW_CFA_offset, s4, -24 */
        0x8c, 4,                        /* DW_CFA_offset, s3, -32 */
        0x8b, 5,                        /* DW_CFA_offset, s2, -40 */
        0x8a, 6,                        /* DW_CFA_offset, s1, -48 */
        0x89, 7,                        /* DW_CFA_offset, s0, -56 */
        0x9a, 8,                        /* DW_CFA_offset, ra, -64 */
    }
};

void tcg_register_jit(void *buf, size_t buf_size)
{
    debug_frame.fde.func_start = (intptr_t)buf;
    debug_frame.fde.func_len = buf_size;

    tcg_register_jit_int(buf, buf_size, &debug_frame, sizeof(debug_frame));
}
