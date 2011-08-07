/*
 *  Synergistic Processor Unit (SPU) emulation
 *  CPU Translation.
 *
 *  Copyright (c) 2011  Richard Henderson
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

#include "config.h"
#include "cpu.h"
#include "disas.h"
#include "host-utils.h"
#include "tcg-op.h"
#include "qemu-common.h"

#include "helper.h"
#define GEN_HELPER 1
#include "helper.h"

typedef struct {
    struct TranslationBlock *tb;
    uint32_t pc;
    uint32_t lslr;
} DisasContext;

/* Return values from translate_one, indicating the state of the TB.
   Note that zero indicates that we are not exiting the TB.  */
typedef enum {
    NO_INSN,
    NO_EXIT,

    /* We have emitted one or more goto_tb.  No fixup required.  */
    EXIT_GOTO_TB,

    /* We are not using a goto_tb (for whatever reason), but have updated
       the PC (for whatever reason), so there's no need to do it again on
       exiting the TB.  */
    EXIT_PC_UPDATED,

    /* We are exiting the TB, but have neither emitted a goto_tb, nor
       updated the PC for the next instruction to be executed.  */
    EXIT_PC_STALE,

    /* We are ending the TB with a noreturn function call, e.g. longjmp.
       No following code will be executed.  */
    EXIT_NORETURN,
} ExitStatus;

/* Global registers.  */
static TCGv_ptr cpu_env;
static TCGv cpu_pc;
static TCGv cpu_gpr[128][4];

/* register names */
static char cpu_reg_names[128][4][8];

#include "gen-icount.h"

static void spu_translate_init(void)
{
    static int done_init = 0;
    int i, j;

    if (done_init) {
        return;
    }
    done_init = 1;

    cpu_env = tcg_global_reg_new_ptr(TCG_AREG0, "env");
    cpu_pc = tcg_global_mem_new(TCG_AREG0, offsetof(CPUState, pc), "pc");

    for (i = 0; i < 128; i++) {
        for (j = 0; j < 4; ++j) {
            sprintf(cpu_reg_names[i][j], "$%d:%d", i, j);
            cpu_gpr[i][j] = tcg_global_mem_new(TCG_AREG0,
                                               offsetof(CPUState, gpr[i*4+j]),
                                               cpu_reg_names[i][j]);
        }
    }

    /* Register the helpers.  */
#define GEN_HELPER 2
#include "helper.h"
}

static void gen_excp_1(int exception, int error_code)
{
    TCGv tmp1, tmp2;

    tmp1 = tcg_const_tl(exception);
    tmp2 = tcg_const_tl(error_code);
    gen_helper_excp(tmp1, tmp2);
    tcg_temp_free(tmp2);
    tcg_temp_free(tmp1);
}

static ExitStatus gen_excp(DisasContext *ctx, int exception, int error_code)
{
    tcg_gen_movi_tl(cpu_pc, ctx->pc);
    gen_excp_1(exception, error_code);
    return EXIT_NORETURN;
}

static ExitStatus gen_movi(TCGv r[4], int32_t imm)
{
    tcg_gen_movi_i32(r[0], imm);
    tcg_gen_movi_i32(r[1], imm);
    tcg_gen_movi_i32(r[2], imm);
    tcg_gen_movi_i32(r[3], imm);
    return NO_EXIT;
}

static void load_temp_imm(TCGv temp[4], int32_t imm)
{
    temp[0] = tcg_const_tl(imm);
    temp[1] = temp[0];
    temp[2] = temp[0];
    temp[3] = temp[0];
}

static void alloc_temp(TCGv temp[4])
{
    temp[0] = tcg_temp_new();
    temp[1] = tcg_temp_new();
    temp[2] = tcg_temp_new();
    temp[3] = tcg_temp_new();
}

static void free_temp(TCGv temp[4])
{
    tcg_temp_free (temp[0]);
    if (!TCGV_EQUAL (temp[0], temp[1])) {
        tcg_temp_free (temp[1]);
    }
    if (!TCGV_EQUAL (temp[0], temp[2])) {
        tcg_temp_free (temp[2]);
    }
    if (!TCGV_EQUAL (temp[0], temp[3])) {
        tcg_temp_free (temp[3]);
    }
}

static void gen_operate2 (void (*op)(TCGv, TCGv), TCGv rt[4], TCGv ra[4])
{
    op(rt[0], ra[0]);
    op(rt[1], ra[1]);
    op(rt[2], ra[2]);
    op(rt[3], ra[3]);
}

static void gen_operate3 (void (*op)(TCGv, TCGv, TCGv),
                         TCGv rt[4], TCGv ra[4], TCGv rb[4])
{
    op(rt[0], ra[0], rb[0]);
    op(rt[1], ra[1], rb[1]);
    op(rt[2], ra[2], rb[2]);
    op(rt[3], ra[3], rb[3]);
}

static int32_t expand_fsmbi(int32_t imm4)
{
    int32_t i, ret = 0;

    for (i = 3; i >= 0; --i) {
        if ((imm4 >> i) & 1) {
            ret |= 0xff;
        }
        ret <<= 8;
    }
    return ret;
}

static TCGv gen_address_a(DisasContext *ctx, uint32_t a)
{
    return tcg_const_tl(a & ctx->lslr);
}

static TCGv gen_address_x(DisasContext *ctx, TCGv a, TCGv b)
{
    TCGv addr = tcg_temp_new();
    tcg_gen_add_tl(addr, a, b);
    tcg_gen_andi_tl(addr, addr, ctx->lslr);
    return addr;
}

static TCGv gen_address_d(DisasContext *ctx, TCGv a, int32_t disp)
{
    TCGv addr = tcg_temp_new();
    tcg_gen_addi_tl(addr, a, disp & ctx->lslr);
    tcg_gen_andi_tl(addr, addr, ctx->lslr);
    return addr;
}

static ExitStatus gen_loadq(TCGv addr, TCGv reg[4])
{
    tcg_gen_qemu_ld32u(reg[0], addr, 0);
    tcg_gen_addi_tl(addr, addr, 4);
    tcg_gen_qemu_ld32u(reg[1], addr, 0);
    tcg_gen_addi_tl(addr, addr, 4);
    tcg_gen_qemu_ld32u(reg[2], addr, 0);
    tcg_gen_addi_tl(addr, addr, 4);
    tcg_gen_qemu_ld32u(reg[3], addr, 0);
    tcg_temp_free(addr);
    return NO_EXIT;
}

static ExitStatus gen_storeq(TCGv addr, TCGv reg[4])
{
    tcg_gen_qemu_st32(reg[0], addr, 0);
    tcg_gen_addi_tl(addr, addr, 4);
    tcg_gen_qemu_st32(reg[1], addr, 0);
    tcg_gen_addi_tl(addr, addr, 4);
    tcg_gen_qemu_st32(reg[2], addr, 0);
    tcg_gen_addi_tl(addr, addr, 4);
    tcg_gen_qemu_st32(reg[3], addr, 0);
    tcg_temp_free(addr);
    return NO_EXIT;
}

static void gen_addh(TCGv out, TCGv a, TCGv b)
{
    TCGv oh, ah, bh;

    oh = tcg_temp_new();
    ah = tcg_temp_new();
    bh = tcg_temp_new();

    tcg_gen_shri_tl(ah, a, 16);
    tcg_gen_shri_tl(bh, b, 16);
    tcg_gen_add_tl(out, a, b);
    tcg_gen_add_tl(oh, ah, bh);
    tcg_gen_deposit_tl(out, out, oh, 16, 16);

    tcg_temp_free(oh);
    tcg_temp_free(ah);
    tcg_temp_free(bh);
}

static void gen_subh(TCGv out, TCGv a, TCGv b)
{
    TCGv oh, ah, bh;

    oh = tcg_temp_new();
    ah = tcg_temp_new();
    bh = tcg_temp_new();

    tcg_gen_shri_tl(ah, a, 16);
    tcg_gen_shri_tl(bh, b, 16);
    tcg_gen_sub_tl(out, a, b);
    tcg_gen_sub_tl(oh, ah, bh);
    tcg_gen_deposit_tl(out, out, oh, 16, 16);

    tcg_temp_free(oh);
    tcg_temp_free(ah);
    tcg_temp_free(bh);
}

static void gen_addx(TCGv out, TCGv a, TCGv b)
{
    tcg_gen_andi_tl(out, out, 1);
    tcg_gen_add_tl(out, out, a);
    tcg_gen_add_tl(out, out, b);
}

static void gen_cg(TCGv out, TCGv a, TCGv b)
{
#if TCG_TARGET_REG_BITS == 32
    /* For 32-bit hosts, we can re-use the host's hardware carry
       generation by using an ADD2 opcode.  We discard the low
       part of the output.  */
    TCGv out_low = tcg_temp_new();
    TCGv in_zero = tcg_const_tl(0);
    tcg_gen_op6_i32(INDEX_op_add2_i32, out_low, out,
                    a, in_zero, b, in_zero);
    tcg_temp_free(out_low);
    tcg_temp_free(in_zero);
#else
    TCGv_i64 o64, b64;

    o64 = tcg_temp_new_i64();
    b64 = tcg_temp_new_i64();

    tcg_gen_extu_i32_i64(o64, a);
    tcg_gen_extu_i32_i64(b64, b);
    tcg_gen_add_i64(o64, o64, b64);
    tcg_gen_shri_i64(o64, o64, 32);
    tcg_gen_trunc_i64_i32(out, o64);
#endif
}

static void gen_cgx(TCGv out, TCGv a, TCGv b)
{
#if TCG_TARGET_REG_BITS == 32
    /* For 32-bit hosts, we can re-use the host's hardware carry
       generation by using an ADD2 opcode.  We discard the low
       part of the output.  */
    TCGv out_low = tcg_temp_new();
    TCGv in_zero = tcg_const_tl(0);
    TCGv out_lsb = tcg_temp_new();

    tcg_gen_andi_tl(out_lsb, out, 1);
    tcg_gen_op6_i32(INDEX_op_add2_i32, out_low, out,
                    a, in_zero, b, in_zero);
    tcg_gen_op6_i32(INDEX_op_add2_i32, out_low, out,
                    out_low, out, out_lsb, in_zero);

    tcg_temp_free(out_low);
    tcg_temp_free(in_zero);
    tcg_temp_free(out_lsb);
#else
    TCGv_i64 o64, a64, b64;

    o64 = tcg_temp_new_i64();
    a64 = tcg_temp_new_i64();
    b64 = tcg_temp_new_i64();

    tcg_gen_extu_i32_i64(o64, out);
    tcg_gen_extu_i32_i64(a64, a);
    tcg_gen_extu_i32_i64(b64, b);

    tcg_gen_andi_i64(o64, o64, 1);
    tcg_gen_add_i64(o64, o64, a64);
    tcg_gen_add_i64(o64, o64, b64);
    tcg_gen_shri_i64(o64, o64, 32);
    tcg_gen_trunc_i64_i32(out, o64);

    tcg_temp_free_i64(o64);
    tcg_temp_free_i64(a64);
    tcg_temp_free_i64(b64);
#endif
}

static void gen_sfx(TCGv out, TCGv a, TCGv b)
{
    tcg_gen_andi_tl(out, out, 1);
    tcg_gen_add_tl(out, out, b);
    tcg_gen_sub_tl(out, out, a);
}

static void gen_bg(TCGv out, TCGv a, TCGv b)
{
    tcg_gen_setcond_i32(TCG_COND_GTU, out, a, b);
}

static void gen_bgx(TCGv out, TCGv a, TCGv b)
{
    TCGv_i64 o64, a64, b64;

    o64 = tcg_temp_new_i64();
    a64 = tcg_temp_new_i64();
    b64 = tcg_temp_new_i64();

    tcg_gen_extu_i32_i64(o64, out);
    tcg_gen_extu_i32_i64(a64, a);
    tcg_gen_extu_i32_i64(b64, b);

    tcg_gen_andi_i64(o64, o64, 1);
    tcg_gen_add_i64(o64, o64, b64);
    tcg_gen_sub_i64(o64, o64, a64);
    tcg_gen_shri_i64(o64, o64, 63);
    tcg_gen_trunc_i64_i32(out, o64);

    tcg_temp_free_i64(o64);
    tcg_temp_free_i64(a64);
    tcg_temp_free_i64(b64);
}

static void gen_mpy(TCGv out, TCGv a, TCGv b)
{
    TCGv al = tcg_temp_new();
    TCGv bl = tcg_temp_new();

    tcg_gen_ext16s_tl(al, a);
    tcg_gen_ext16s_tl(bl, b);
    tcg_gen_mul_tl(out, al, bl);

    tcg_temp_free(al);
    tcg_temp_free(bl);
}

static void gen_mpyu(TCGv out, TCGv a, TCGv b)
{
    TCGv al = tcg_temp_new();
    TCGv bl = tcg_temp_new();

    tcg_gen_ext16u_tl(al, a);
    tcg_gen_ext16u_tl(bl, b);
    tcg_gen_mul_tl(out, al, bl);

    tcg_temp_free(al);
    tcg_temp_free(bl);
}

static void gen_mpyh(TCGv out, TCGv a, TCGv b)
{
    TCGv ah = tcg_temp_new();
    TCGv bl = tcg_temp_new();

    tcg_gen_sari_tl(ah, a, 16);
    tcg_gen_ext16s_tl(bl, b);
    tcg_gen_mul_tl(out, ah, bl);

    tcg_temp_free(ah);
    tcg_temp_free(bl);
}

static void gen_mpys(TCGv out, TCGv a, TCGv b)
{
    TCGv al = tcg_temp_new();
    TCGv bl = tcg_temp_new();

    tcg_gen_ext16s_tl(al, a);
    tcg_gen_ext16s_tl(bl, b);
    tcg_gen_mul_tl(out, al, bl);
    tcg_gen_sari_tl(out, out, 16);

    tcg_temp_free(al);
    tcg_temp_free(bl);
}

static void gen_mpyhh(TCGv out, TCGv a, TCGv b)
{
    TCGv ah = tcg_temp_new();
    TCGv bh = tcg_temp_new();

    tcg_gen_sari_tl(ah, a, 16);
    tcg_gen_sari_tl(bh, b, 16);
    tcg_gen_mul_tl(out, ah, bh);

    tcg_temp_free(ah);
    tcg_temp_free(bh);
}

static void gen_mpyhhu(TCGv out, TCGv a, TCGv b)
{
    TCGv ah = tcg_temp_new();
    TCGv bh = tcg_temp_new();

    tcg_gen_shri_tl(ah, a, 16);
    tcg_gen_shri_tl(bh, b, 16);
    tcg_gen_mul_tl(out, ah, bh);

    tcg_temp_free(ah);
    tcg_temp_free(bh);
}


#define _(X)  { qemu_log("Unimplemented insn: " #X "\n"); return NO_EXIT; }

static ExitStatus translate_0 (DisasContext *ctx, uint32_t insn, int opwidth)
{
    unsigned op, rt, ra, rb, rc;
    int32_t imm;
    TCGv temp[4];

    /* Normally, RT is on the right, etc.  To be fixed up below as needed.  */
    rt = insn & 0x7f;
    ra = (insn >> 7) & 0x7f;
    rb = (insn >> 14) & 0x7f;
    rc = 0;
    imm = 0;

    /* Up to 11 bits are considered "opcode", depending on the format.
       To make things easy to pull out of the ISA document, we arrange
       the switch statement with a left-aligned 12-bits.  Therefore
       the number in the switch can be read directly from the left
       aligned ISA document, padded on the right with enough bits to
       make up 5 hexidecimal digits.  */
    /* Sadly, it does not appear as if, except for certain cases, that
       there are dedicated opcode bits that indicate the width of the
       opcode.  So we try matching with increasing opcode width until
       we get a match.  */
    op = insn >> 20;
    switch (opwidth) {
    case 4:
        /* RRR Instruction Format (4-bit op).  */
	op &= 0xf00;
        rc = rt;
        rt = (insn >> 21) & 0x7f;
        break;
    case 7:
        /* RI18 Instruction Format (7-bit op).  */
        op &= 0xfe0;
        imm = (uint32_t)(insn << 7) >> 14;
        break;
    case 8:
        /* RI10 Instruction Format (8-bit op).  */
        op &= 0xff0;
        imm = (int32_t)(insn << 8) >> 22;
        break;
    case 9:
        /* RI16 Instruction Format (9-bit op).  */
        op &= 0xff8;
        imm = (int32_t)(insn << 9) >> 16;
        break;
    case 10:
        /* ??? Not specifically documented in section 2.3, but the
           floating-point integer conversions have a 10-bit opcode
           and with an unsigned 8-bit immediate.  */
        op &= 0xffc;
        imm = (insn >> 14) & 0xff;
        break;
    case 11:
        /* RR/RI7 Instruction Format (11-bit op).  */
        op &= 0xffe;
        imm = (rb ^ 0x40) - 0x40;
        break;
    default:
        abort();
    }

    switch (op) {
    /* RRR Instruction Format (4-bit op).  */
    case 0xc00: _(MPYA);
        load_temp_imm(temp, imm);
        gen_operate3(gen_mpy, temp, cpu_gpr[ra], cpu_gpr[rb]);
        gen_operate3(tcg_gen_add_tl, cpu_gpr[rt], cpu_gpr[rc], temp);
        free_temp(temp);
        return NO_EXIT;

    case 0x800: _(SELB);
    case 0xb00: _(SHUFB);
    case 0xe00: _(FMA);
    case 0xd00: _(FNMS);
    case 0xf00: _(FMS);

    /* RI18 Instruction Format (7-bit op).  */
    case 0x420: /* ILA */
        return gen_movi(cpu_gpr[rt], imm);
    case 0x100: _(HBRA);
    case 0x120: _(HBRR);

    /* RI10 Instruction Format (8-bit op).  */
    case 0x340: /* LQD */
        return gen_loadq(gen_address_d(ctx, cpu_gpr[ra][0], imm), cpu_gpr[rt]);
    case 0x240: /* STQD */
        return gen_storeq(gen_address_d(ctx, cpu_gpr[ra][0], imm), cpu_gpr[rt]);

    case 0x1d0: /* AHI */
        imm &= 0xffff;
        imm |= imm << 16;
        load_temp_imm(temp, imm);
        gen_operate3(gen_addh, cpu_gpr[rt], cpu_gpr[ra], temp);
        free_temp(temp);
        return NO_EXIT;
    case 0x1c0: /* AI */
        load_temp_imm(temp, imm);
        gen_operate3(tcg_gen_add_tl, cpu_gpr[rt], cpu_gpr[ra], temp);
        free_temp(temp);
        return NO_EXIT;
    case 0x0d0: /* SFHI */
        imm &= 0xffff;
        imm |= imm << 16;
        load_temp_imm(temp, imm);
        gen_operate3(gen_subh, cpu_gpr[rt], temp, cpu_gpr[ra]);
        free_temp(temp);
        return NO_EXIT;
    case 0x0c0: /* SFI */
        load_temp_imm(temp, imm);
        gen_operate3(tcg_gen_sub_tl, cpu_gpr[rt], temp, cpu_gpr[ra]);
        free_temp(temp);
        return NO_EXIT;

    case 0x740: _(MPYI);
        load_temp_imm(temp, imm);
        gen_operate3(gen_mpy, cpu_gpr[rt], cpu_gpr[ra], temp);
        free_temp(temp);
        return NO_EXIT;
    case 0x750: _(MPYUI);
        load_temp_imm(temp, imm);
        gen_operate3(gen_mpyu, cpu_gpr[rt], cpu_gpr[ra], temp);
        free_temp(temp);
        return NO_EXIT;

    case 0x160: _(ANDBI);
    case 0x150: _(ANDHI);
    case 0x140: _(ANDI);
    case 0x060: _(ORBI);
    case 0x050: _(ORHI);
    case 0x040: _(ORI);
    case 0x460: _(XORBI);
    case 0x450: _(XORHI);
    case 0x440: _(XORI);
    case 0x7f0: _(HEQI);
    case 0x4f0: _(HGTI);
    case 0x5f0: _(HLGTI);
    case 0x7e0: _(CEQBI);
    case 0x7d0: _(CEQHI);
    case 0x7c0: _(CEQI);
    case 0x4e0: _(CGTBI);
    case 0x4d0: _(CGTHI);
    case 0x4c0: _(CGTI);
    case 0x5e0: _(CLGTBI);
    case 0x5d0: _(CLGTHI);
    case 0x5c0: _(CLGTI);

    /* RI16 Instruction Format (9-bit op).  */
    case 0x308: /* LQA */
        return gen_loadq(gen_address_a(ctx, imm), cpu_gpr[rt]);
    case 0x338: /* LQR */
        return gen_loadq(gen_address_a(ctx, ctx->pc + imm), cpu_gpr[rt]);
    case 0x208: /* STQA */
        return gen_storeq(gen_address_a(ctx, imm), cpu_gpr[rt]);
    case 0x238: /* STQR */
        return gen_storeq(gen_address_a(ctx, ctx->pc + imm), cpu_gpr[rt]);

    case 0x418: /* ILH */
        imm &= 0xffff;
        imm |= imm << 16;
	return gen_movi(cpu_gpr[rt], imm);
    case 0x410: /* ILHU */
        imm <<= 16;
        return gen_movi(cpu_gpr[rt], imm);
    case 0x408: /* IL */
        return gen_movi(cpu_gpr[rt], imm);
    case 0x608: /* IOHL */
        load_temp_imm(temp, imm);
        gen_operate3(tcg_gen_or_tl, cpu_gpr[rt], cpu_gpr[rt], temp);
        free_temp(temp);
        return NO_EXIT;
    case 0x328: /* FSMBI */
        tcg_gen_movi_tl(cpu_gpr[rt][0], expand_fsmbi(imm >> 12));
        tcg_gen_movi_tl(cpu_gpr[rt][1], expand_fsmbi(imm >> 8));
        tcg_gen_movi_tl(cpu_gpr[rt][2], expand_fsmbi(imm >> 4));
        tcg_gen_movi_tl(cpu_gpr[rt][3], expand_fsmbi(imm >> 0));
        return NO_EXIT;

    case 0x320: _(BR);
    case 0x300: _(BRA);
    case 0x330: _(BRSL);
    case 0x310: _(BRASL);
    case 0x210: _(BRNZ);
    case 0x200: _(BRZ);
    case 0x230: _(BRHNZ);
    case 0x220: _(BRHZ);

    /* RR/RI7 Instruction Format (11-bit op).  */
    case 0x388: /* LDX */
        return gen_loadq(gen_address_x(ctx, cpu_gpr[ra][0], cpu_gpr[rb][0]),
                         cpu_gpr[rt]);
    case 0x288: /* STQX */
        return gen_storeq(gen_address_x(ctx, cpu_gpr[ra][0], cpu_gpr[rb][0]),
                          cpu_gpr[rt]);

    case 0x3e8: _(CBD);
    case 0x3a8: _(CBX);
    case 0x3ea: _(CHD);
    case 0x3aa: _(CHX);
    case 0x3ec: _(CWD);
    case 0x3ac: _(CWX);
    case 0x3ee: _(CDD);
    case 0x3ae: _(CDX);

    case 0x190: /* AH */
        gen_operate3(gen_addh, cpu_gpr[rt], cpu_gpr[ra], cpu_gpr[rb]);
        return NO_EXIT;
    case 0x180: /* A */
        gen_operate3(tcg_gen_add_tl, cpu_gpr[rt], cpu_gpr[ra], cpu_gpr[rb]);
        return NO_EXIT;
    case 0x090: /* SFH */
        gen_operate3(gen_subh, cpu_gpr[rt], cpu_gpr[rb], cpu_gpr[ra]);
        return NO_EXIT;
    case 0x080: /* SF */
        gen_operate3(tcg_gen_sub_tl, cpu_gpr[rt], cpu_gpr[rb], cpu_gpr[ra]);
        return NO_EXIT;
    case 0x680: /* ADDX */
        gen_operate3(gen_addx, cpu_gpr[rt], cpu_gpr[ra], cpu_gpr[rb]);
        return NO_EXIT;
    case 0x184: /* CG */
        gen_operate3(gen_cg, cpu_gpr[rt], cpu_gpr[ra], cpu_gpr[rb]);
        return NO_EXIT;
    case 0x684: /* CGX */
        gen_operate3(gen_cgx, cpu_gpr[rt], cpu_gpr[ra], cpu_gpr[rb]);
        return NO_EXIT;
    case 0x682: /* SFX */
        gen_operate3(gen_sfx, cpu_gpr[rt], cpu_gpr[ra], cpu_gpr[rb]);
        return NO_EXIT;
    case 0x084: /* BG */
        gen_operate3(gen_bg, cpu_gpr[rt], cpu_gpr[ra], cpu_gpr[rb]);
        return NO_EXIT;
    case 0x686: /* BGX */
        gen_operate3(gen_bgx, cpu_gpr[rt], cpu_gpr[ra], cpu_gpr[rb]);
        return NO_EXIT;

    case 0x788: /* MPY */
        gen_operate3(gen_mpy, cpu_gpr[rt], cpu_gpr[ra], cpu_gpr[rb]);
        return NO_EXIT;
    case 0x798: /* MPYU */
        gen_operate3(gen_mpyu, cpu_gpr[rt], cpu_gpr[ra], cpu_gpr[rb]);
        return NO_EXIT;
    case 0x78a: /* MPYH */
        gen_operate3(gen_mpyh, cpu_gpr[rt], cpu_gpr[ra], cpu_gpr[rb]);
        return NO_EXIT;
    case 0x78e: /* MPYS */
        gen_operate3(gen_mpys, cpu_gpr[rt], cpu_gpr[ra], cpu_gpr[rb]);
        return NO_EXIT;
    case 0x78c: /* MPYHH */
        gen_operate3(gen_mpyhh, cpu_gpr[rt], cpu_gpr[ra], cpu_gpr[rb]);
        return NO_EXIT;
    case 0x68c: _(MPYHHA);
        load_temp_imm(temp, imm);
        gen_operate3(gen_mpyhh, temp, cpu_gpr[ra], cpu_gpr[rb]);
        gen_operate3(tcg_gen_add_tl, cpu_gpr[rt], cpu_gpr[rt], temp);
        free_temp(temp);
        return NO_EXIT;
    case 0x79c: /* MPYHHU */
        gen_operate3(gen_mpyhhu, cpu_gpr[rt], cpu_gpr[ra], cpu_gpr[rb]);
        return NO_EXIT;
    case 0x69c: /* MPYHHAU */
        load_temp_imm(temp, imm);
        gen_operate3(gen_mpyhhu, temp, cpu_gpr[ra], cpu_gpr[rb]);
        gen_operate3(tcg_gen_add_tl, cpu_gpr[rt], cpu_gpr[rt], temp);
        free_temp(temp);
        return NO_EXIT;

    case 0x54a: _(CLZ);
    case 0x568: _(CNTB);
    case 0x36c: _(FSMB);
    case 0x36a: _(FSMH);
    case 0x368: _(FSM);
    case 0x364: _(GBB);
    case 0x362: _(GBH);
    case 0x360: _(GB);
    case 0x1a6: _(AVGB);
    case 0x0a6: _(ABSDB);
    case 0x4a6: _(SUMB);
    case 0x56c: _(XSBH);
    case 0x55c: _(XSHW);
    case 0x54c: _(XSWD);
    case 0x182: _(AND);
    case 0x582: _(ANDC);
    case 0x082: _(OR);
    case 0x592: _(ORC);
    case 0x3e0: _(ORX);
    case 0x482: _(XOR);
    case 0x192: _(NAND);
    case 0x092: _(NOR);
    case 0x492: _(EQV);
    case 0x0be: _(SHLH);
    case 0x0fe: _(SHLHI);
    case 0x0b6: _(SHL);
    case 0x0f6: _(SHLI);
    case 0x3b6: _(SHLQBI);
    case 0x3f6: _(SHLQBII);
    case 0x3be: _(SHLQBY);
    case 0x3fe: _(SHLQBYI);
    case 0x39e: _(SHLQBYBI);
    case 0x0b8: _(ROTH);
    case 0x0f8: _(ROTHI);
    case 0x0b0: _(ROT);
    case 0x0f0: _(ROTI);
    case 0x3b8: _(ROTQBY);
    case 0x3f8: _(ROTBYI);
    case 0x398: _(ROTBYBI);
    case 0x3b0: _(ROTQBI);
    case 0x3f0: _(ROTQBII);
    case 0x0ba: _(ROTHM);
    case 0x0fa: _(ROTHMI);
    case 0x0b2: _(ROTM);
    case 0x0f2: _(ROTMI);
    case 0x3ba: _(ROTQMBY);
    case 0x3fa: _(ROTQMBYI);
    case 0x39a: _(ROTQMBYBI);
    case 0x3b2: _(ROTQMBI);
    case 0x3f2: _(ROTQMBII);
    case 0x0bc: _(ROTMAH);
    case 0x0fc: _(ROTMAHI);
    case 0x0b4: _(ROTMA);
    case 0x0f4: _(ROTMAI);
    case 0x7b0: _(HEQ);
    case 0x2b0: _(HGT);
    case 0x5b0: _(HLGT);
    case 0x7a0: _(CEQB);
    case 0x790: _(CEQH);
    case 0x780: _(CEQ);
    case 0x4a0: _(CGTB);
    case 0x490: _(CGTH);
    case 0x480: _(CGT);
    case 0x5a0: _(CLGTB);
    case 0x590: _(CLGTH);
    case 0x580: _(CLGT);
    case 0x3a0: _(BI);
    case 0x354: _(IRET);
    case 0x356: _(BISLED);
    case 0x352: _(BISL);
    case 0x250: _(BIZ);
    case 0x252: _(BINZ);
    case 0x254: _(BIHZ);
    case 0x256: _(BIHNZ);
    case 0x358: _(HBR);
    case 0x588: _(FA);
    case 0x598: _(DFA);
    case 0x58a: _(FS);
    case 0x59a: _(DFS);
    case 0x58c: _(FM);
    case 0x59c: _(DFM);
    case 0x6b8: _(DFMA);
    case 0x6bc: _(DFNMS);
    case 0x6ba: _(DFMS);
    case 0x6be: _(DFNMA);
    case 0x370: _(FREST);
    case 0x372: _(FRSQEST);
    case 0x7a8: _(FI);
    case 0x768: _(CSFLT);
    case 0x760: _(CFLTS);
    case 0x76c: _(CUFLT);
    case 0x764: _(CFLTU);
    case 0x772: _(FRDS);
    case 0x770: _(FESD);
    case 0x786: _(DFCEQ);
    case 0x796: _(DFCMEQ);
    case 0x586: _(DFCGT);
    case 0x596: _(DFCMGT);
    case 0x77e: _(DFTSV);
    case 0x784: _(FCEQ);
    case 0x794: _(FCMEQ);
    case 0x584: _(FCGT);
    case 0x594: _(FCMGT);
    case 0x774: _(FSCRWR);
    case 0x730: _(FSCRRD);
    case 0x000: _(STOP);
    case 0x280: _(STOPD);
    case 0x002: _(LNOP);
    case 0x402: _(NOP);
    case 0x004: _(SYNC);
    case 0x006: _(DSYNC);
    case 0x018: _(MFSPR);
    case 0x218: _(MTSPR);
    case 0x01a: _(RDCH);
    case 0x01e: _(RCHCNT);
    case 0x21a: _(WRCH);
    default:
        return NO_INSN;
    }
}

static ExitStatus translate_1 (DisasContext *ctx, uint32_t insn)
{
    ExitStatus ret;
    int width;

    if (insn & 0x80000000) {
        ret = translate_0(ctx, insn, 4);
    } else {
        for (width = 7; width <= 11; ++width) {
            ret = translate_0(ctx, insn, width);
            if (ret != NO_INSN) {
                break;
            }
        }
    }
    if (ret == NO_INSN) {
        ret = gen_excp(ctx, EXCP_ILLOPC, 0);
    }
    return ret;
}

static inline void gen_intermediate_code_internal(CPUState *env,
                                                  TranslationBlock *tb,
                                                  int search_pc)
{
    DisasContext ctx, *ctxp = &ctx;
    uint32_t pc_start;
    uint32_t insn;
    uint16_t *gen_opc_end;
    CPUBreakpoint *bp;
    int j, lj = -1;
    ExitStatus ret;
    int num_insns;
    int max_insns;

    gen_opc_end = gen_opc_buf + OPC_MAX_SIZE;

    ctx.tb = tb;
    ctx.pc = pc_start = tb->pc;

    num_insns = 0;
    max_insns = tb->cflags & CF_COUNT_MASK;
    if (max_insns == 0) {
        max_insns = CF_COUNT_MASK;
    }

    gen_icount_start();
    do {
        if (unlikely(!QTAILQ_EMPTY(&env->breakpoints))) {
            QTAILQ_FOREACH(bp, &env->breakpoints, entry) {
                if (bp->pc == ctx.pc) {
                    gen_excp(&ctx, EXCP_DEBUG, 0);
                    break;
                }
            }
        }
        if (search_pc) {
            j = gen_opc_ptr - gen_opc_buf;
            if (lj < j) {
                lj++;
                while (lj < j) {
                    gen_opc_instr_start[lj++] = 0;
                }
            }
            gen_opc_pc[lj] = ctx.pc;
            gen_opc_instr_start[lj] = 1;
            gen_opc_icount[lj] = num_insns;
        }
        if (num_insns + 1 == max_insns && (tb->cflags & CF_LAST_IO)) {
            gen_io_start();
        }
        insn = ldl_code(ctx.pc);
        num_insns++;

	if (unlikely(qemu_loglevel_mask(CPU_LOG_TB_OP))) {
            tcg_gen_debug_insn_start(ctx.pc);
        }

        ret = translate_1(ctxp, insn);
        ctx.pc += 4;

        /* If we reach a page boundary, are single stepping,
           or exhaust instruction count, stop generation.  */
        if (ret == NO_EXIT
            && ((ctx.pc & (TARGET_PAGE_SIZE - 1)) == 0
                || gen_opc_ptr >= gen_opc_end
                || num_insns >= max_insns
                || singlestep
                || env->singlestep_enabled)) {
            ret = EXIT_PC_STALE;
        }
    } while (ret == NO_EXIT);

    if (tb->cflags & CF_LAST_IO) {
        gen_io_end();
    }

    switch (ret) {
    case EXIT_GOTO_TB:
    case EXIT_NORETURN:
        break;
    case EXIT_PC_STALE:
        tcg_gen_movi_tl(cpu_pc, ctx.pc);
        /* FALLTHRU */
    case EXIT_PC_UPDATED:
        if (env->singlestep_enabled) {
            gen_excp_1(EXCP_DEBUG, 0);
        } else {
            tcg_gen_exit_tb(0);
        }
        break;
    default:
        abort();
    }

    gen_icount_end(tb, num_insns);
    *gen_opc_ptr = INDEX_op_end;
    if (search_pc) {
        j = gen_opc_ptr - gen_opc_buf;
        lj++;
        while (lj <= j)
            gen_opc_instr_start[lj++] = 0;
    } else {
        tb->size = ctx.pc - pc_start;
        tb->icount = num_insns;
    }

#ifdef DEBUG_DISAS
    if (qemu_loglevel_mask(CPU_LOG_TB_IN_ASM)) {
        qemu_log("IN: %s\n", lookup_symbol(pc_start));
        log_target_disas(pc_start, ctx.pc - pc_start, 1);
        qemu_log("\n");
    }
#endif
}

void gen_intermediate_code (CPUState *env, struct TranslationBlock *tb)
{
    gen_intermediate_code_internal(env, tb, 0);
}

void gen_intermediate_code_pc (CPUState *env, struct TranslationBlock *tb)
{
    gen_intermediate_code_internal(env, tb, 1);
}

CPUState * cpu_spu_init (const char *cpu_model)
{
    CPUState *env;

    spu_translate_init();

    env = qemu_mallocz(sizeof(CPUState));
    cpu_exec_init(env);

    qemu_init_vcpu(env);
    return env;
}

void restore_state_to_opc(CPUState *env, TranslationBlock *tb, int pc_pos)
{
    env->pc = gen_opc_pc[pc_pos];
}
