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
} DisassContext;

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

/* To be used in "insn_foo", return "foo".  */
#define INSN	(__FUNCTION__ + 5)

#define DISASS_RR			\
    unsigned rt = insn & 0x7f;		\
    unsigned ra = (insn >> 7) & 0x7f;	\
    unsigned rb = (insn >> 14) & 0x7f;	\
    qemu_log_mask(CPU_LOG_TB_IN_ASM, "%s\t$%d,$%d,$%d\n", INSN, rt, ra, rb)

#define DISASS_RR1			\
    unsigned rt = insn & 0x7f;		\
    unsigned ra = (insn >> 7) & 0x7f;	\
    qemu_log_mask(CPU_LOG_TB_IN_ASM, "%s\t$%d,$%d\n", INSN, rt, ra)

#define DISASS_RRR			\
    unsigned rt = (insn >> 21) & 0x7f;	\
    unsigned ra = (insn >> 7) & 0x7f;	\
    unsigned rb = (insn >> 14) & 0x7f;	\
    unsigned rc = insn & 0x7f;		\
    qemu_log_mask(CPU_LOG_TB_IN_ASM, "%s\t$%d,$%d,$%d,$%d\n", \
                  INSN, rt, ra, rb, rc)

#define DISASS_RI7			\
    unsigned rt = insn & 0x7f;		\
    unsigned ra = (insn >> 7) & 0x7f;	\
    int32_t imm = (int32_t)(insn << 11) >> 25; \
    qemu_log_mask(CPU_LOG_TB_IN_ASM, "%s\t$%d,$%d,%d\n", INSN, rt, ra, imm)

#define DISASS_RI10			\
    unsigned rt = insn & 0x7f;		\
    unsigned ra = (insn >> 7) & 0x7f;	\
    int32_t imm = (int32_t)(insn << 8) >> 22; \
    qemu_log_mask(CPU_LOG_TB_IN_ASM, "%s\t$%d,$%d,%d\n", INSN, rt, ra, imm)

#define DISASS_RI16			\
    unsigned rt = insn & 0x7f;		\
    int32_t imm = (int32_t)(insn << 9) >> 16; \
    qemu_log_mask(CPU_LOG_TB_IN_ASM, "%s\t$%d,%d\n", INSN, rt, imm)

#define DISASS_RI18			\
    unsigned rt = insn & 0x7f;		\
    int32_t imm = (uint32_t)(insn << 7) >> 16; \
    qemu_log_mask(CPU_LOG_TB_IN_ASM, "%s\t$%d,%d\n", INSN, rt, imm)


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
    tcg_temp_free(temp[0]);
    if (!TCGV_EQUAL(temp[0], temp[1])) {
        tcg_temp_free(temp[1]);
    }
    if (!TCGV_EQUAL(temp[0], temp[2])) {
        tcg_temp_free(temp[2]);
    }
    if (!TCGV_EQUAL(temp[0], temp[3])) {
        tcg_temp_free(temp[3]);
    }
}

static void foreach_op2 (void (*op)(TCGv, TCGv), TCGv rt[4], TCGv ra[4])
{
    op(rt[0], ra[0]);
    op(rt[1], ra[1]);
    op(rt[2], ra[2]);
    op(rt[3], ra[3]);
}

static void foreach_op3 (void (*op)(TCGv, TCGv, TCGv),
                         TCGv rt[4], TCGv ra[4], TCGv rb[4])
{
    op(rt[0], ra[0], rb[0]);
    op(rt[1], ra[1], rb[1]);
    op(rt[2], ra[2], rb[2]);
    op(rt[3], ra[3], rb[3]);
}

static void foreach_op4 (void (*op)(TCGv, TCGv, TCGv, TCGv),
                         TCGv rt[4], TCGv ra[4], TCGv rb[4], TCGv rc[4])
{
    op(rt[0], ra[0], rb[0], rc[0]);
    op(rt[1], ra[1], rb[1], rc[1]);
    op(rt[2], ra[2], rb[2], rc[2]);
    op(rt[3], ra[3], rb[3], rc[3]);
}

#define FOREACH_RR(NAME, FN)						\
static ExitStatus insn_##NAME(DisassContext *ctx, uint32_t insn)	\
{									\
    DISASS_RR;								\
    foreach_op3(FN, cpu_gpr[rt], cpu_gpr[ra], cpu_gpr[rb]);		\
    return NO_EXIT;							\
}

#define FOREACH_RR1(NAME, FN)						\
static ExitStatus insn_##NAME(DisassContext *ctx, uint32_t insn)	\
{									\
    DISASS_RR1;								\
    foreach_op2(FN, cpu_gpr[rt], cpu_gpr[ra]);				\
    return NO_EXIT;							\
}

#define FOREACH_RRR(NAME, FN)						\
static ExitStatus insn_##NAME(DisassContext *ctx, uint32_t insn)	\
{									\
    DISASS_RRR;								\
    foreach_op4(FN, cpu_gpr[rt], cpu_gpr[ra], cpu_gpr[rb], cpu_gpr[rc]);\
    return NO_EXIT;							\
}

#define FOREACH_RI7_ADJ(NAME, FN, ADJUST_IMM)				\
static ExitStatus insn_##NAME(DisassContext *ctx, uint32_t insn)	\
{									\
    TCGv temp[4];							\
    DISASS_RI7;                                                         \
    ADJUST_IMM;                                                         \
    load_temp_imm(temp, imm);						\
    foreach_op3(FN, cpu_gpr[rt], cpu_gpr[ra], temp);			\
    free_temp(temp);							\
    return NO_EXIT;							\
}

#define FOREACH_RI7(NAME, FN)  FOREACH_RI7_ADJ(NAME, FN, )

#define FOREACH_RI10_ADJ(NAME, FN, ADJUST_IMM)				\
static ExitStatus insn_##NAME(DisassContext *ctx, uint32_t insn)	\
{									\
    TCGv temp[4];							\
    DISASS_RI10;							\
    ADJUST_IMM;                                                         \
    load_temp_imm(temp, imm);						\
    foreach_op3(FN, cpu_gpr[rt], cpu_gpr[ra], temp);			\
    free_temp(temp);							\
    return NO_EXIT;							\
}

#define FOREACH_RI10(NAME, FN)  FOREACH_RI10_ADJ(NAME, FN, )

static void gen_excp_1(int exception, int error_code)
{
    TCGv_i32 tmp1, tmp2;

    tmp1 = tcg_const_i32(exception);
    tmp2 = tcg_const_i32(error_code);
    gen_helper_excp(tmp1, tmp2);
    tcg_temp_free_i32(tmp2);
    tcg_temp_free_i32(tmp1);
}

static ExitStatus gen_excp(DisassContext *ctx, int exception, int error_code)
{
    tcg_gen_movi_tl(cpu_pc, ctx->pc);
    gen_excp_1(exception, error_code);
    return EXIT_NORETURN;
}

/* ---------------------------------------------------------------------- */
/* Section 2: Memory Load/Store Instructions.  */

static TCGv gen_address_a(DisassContext *ctx, uint32_t a)
{
    return tcg_const_tl(a & ctx->lslr);
}

static TCGv gen_address_x(DisassContext *ctx, TCGv a, TCGv b)
{
    TCGv addr = tcg_temp_new();
    tcg_gen_add_tl(addr, a, b);
    tcg_gen_andi_tl(addr, addr, ctx->lslr);
    return addr;
}

static TCGv gen_address_d(DisassContext *ctx, TCGv a, int32_t disp)
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

static ExitStatus insn_lqd(DisassContext *ctx, uint32_t insn)
{
    DISASS_RI10;
    return gen_loadq(gen_address_d(ctx, cpu_gpr[ra][0], imm), cpu_gpr[rt]);
}

static ExitStatus insn_lqx(DisassContext *ctx, uint32_t insn)
{
    DISASS_RR;
    return gen_loadq(gen_address_x(ctx, cpu_gpr[ra][0], cpu_gpr[rb][0]),
                     cpu_gpr[rt]);
}

static ExitStatus insn_lqa(DisassContext *ctx, uint32_t insn)
{
    DISASS_RI16;
    return gen_loadq(gen_address_a(ctx, imm), cpu_gpr[rt]);
}

static ExitStatus insn_lqr(DisassContext *ctx, uint32_t insn)
{
    DISASS_RI16;
    return gen_loadq(gen_address_a(ctx, ctx->pc + imm), cpu_gpr[rt]);
}

static ExitStatus insn_stqd(DisassContext *ctx, uint32_t insn)
{
    DISASS_RI10;
    return gen_storeq(gen_address_d(ctx, cpu_gpr[ra][0], imm), cpu_gpr[rt]);
}

static ExitStatus insn_stqx(DisassContext *ctx, uint32_t insn)
{
    DISASS_RR;
    return gen_storeq(gen_address_x(ctx, cpu_gpr[ra][0], cpu_gpr[rb][0]),
                      cpu_gpr[rt]);
}

static ExitStatus insn_stqa(DisassContext *ctx, uint32_t insn)
{
    DISASS_RI16;
    return gen_storeq(gen_address_a(ctx, imm), cpu_gpr[rt]);
}

static ExitStatus insn_stqr(DisassContext *ctx, uint32_t insn)
{
    DISASS_RI16;
    return gen_storeq(gen_address_a(ctx, ctx->pc + imm), cpu_gpr[rt]);
}

/* ---------------------------------------------------------------------- */
/* Section 3: Constant Formation Instructions.  */

static ExitStatus gen_movi(TCGv r[4], int32_t imm)
{
    tcg_gen_movi_i32(r[0], imm);
    tcg_gen_movi_i32(r[1], imm);
    tcg_gen_movi_i32(r[2], imm);
    tcg_gen_movi_i32(r[3], imm);
    return NO_EXIT;
}

static ExitStatus insn_ilh(DisassContext *ctx, uint32_t insn)
{
    DISASS_RI16;

    imm &= 0xffff;
    imm |= imm << 16;
    return gen_movi(cpu_gpr[rt], imm);
}

static ExitStatus insn_ilhu(DisassContext *ctx, uint32_t insn)
{
    DISASS_RI16;

    imm <<= 16;
    return gen_movi(cpu_gpr[rt], imm);
}

static ExitStatus insn_il(DisassContext *ctx, uint32_t insn)
{
    DISASS_RI16;
    return gen_movi(cpu_gpr[rt], imm);
}

static ExitStatus insn_ila(DisassContext *ctx, uint32_t insn)
{
    DISASS_RI18;
    return gen_movi(cpu_gpr[rt], imm);
}

static ExitStatus insn_iohl(DisassContext *ctx, uint32_t insn)
{
    TCGv temp[4];
    DISASS_RI16;

    load_temp_imm(temp, imm);
    foreach_op3(tcg_gen_or_tl, cpu_gpr[rt], cpu_gpr[rt], temp);
    free_temp(temp);
    return NO_EXIT;
}

static ExitStatus insn_fsmbi(DisassContext *ctx, uint32_t insn)
{
    DISASS_RI16;

    tcg_gen_movi_tl(cpu_gpr[rt][0], helper_fsmb(imm >> 0));
    tcg_gen_movi_tl(cpu_gpr[rt][1], helper_fsmb(imm >> 4));
    tcg_gen_movi_tl(cpu_gpr[rt][2], helper_fsmb(imm >> 8));
    tcg_gen_movi_tl(cpu_gpr[rt][3], helper_fsmb(imm >> 12));
    return NO_EXIT;
}

/* ---------------------------------------------------------------------- */
/* Section 4: Integer and Logical Instructions.  */

static void gen_addh(TCGv out, TCGv a, TCGv b)
{
    TCGv high = tcg_temp_new();
    TCGv low = tcg_temp_new();

    tcg_gen_add_tl(low, a, b);

    /* By zapping low half of A, we guarantee no carry into high
       without having to fiddle B.  That will get done in DEPOSIT.  */
    tcg_gen_andi_tl(high, a, 0xffff0000);
    tcg_gen_add_tl(out, high, b);
    tcg_gen_deposit_tl(out, out, low, 0, 16);

    tcg_temp_free(high);
    tcg_temp_free(low);
}

FOREACH_RR(ah, gen_addh)
FOREACH_RI10(ahi, gen_addh)

FOREACH_RR(a, tcg_gen_add_tl)
FOREACH_RI10(ai, tcg_gen_add_tl)

static void gen_sfh(TCGv out, TCGv a, TCGv b)
{
    TCGv high = tcg_temp_new();
    TCGv low = tcg_temp_new();

    tcg_gen_sub_tl(low, b, a);

    tcg_gen_andi_tl(high, a, 0xffff0000);
    tcg_gen_sub_tl(out, b, high);
    tcg_gen_deposit_tl(out, out, low, 0, 16);

    tcg_temp_free(high);
    tcg_temp_free(low);
}

FOREACH_RR(sfh, gen_sfh)
FOREACH_RI10(sfhi, gen_sfh)

static void gen_sf(TCGv out, TCGv a, TCGv b)
{
    tcg_gen_sub_tl(out, b, a);
}

FOREACH_RR(sf, gen_sf)
FOREACH_RI10(sfi, gen_sf)

static void gen_addx(TCGv out, TCGv a, TCGv b)
{
    tcg_gen_andi_tl(out, out, 1);
    tcg_gen_add_tl(out, out, a);
    tcg_gen_add_tl(out, out, b);
}

FOREACH_RR(addx, gen_addx)

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

    tcg_temp_free_i64(o64);
    tcg_temp_free_i64(b64);
#endif
}

FOREACH_RR(cg, gen_cg)

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

FOREACH_RR(cgx, gen_cgx)

static void gen_sfx(TCGv out, TCGv a, TCGv b)
{
    tcg_gen_andi_tl(out, out, 1);
    tcg_gen_add_tl(out, out, b);
    tcg_gen_sub_tl(out, out, a);
}

FOREACH_RR(sfx, gen_sfx)

static void gen_bg(TCGv out, TCGv a, TCGv b)
{
    tcg_gen_setcond_i32(TCG_COND_GTU, out, a, b);
}

FOREACH_RR(bg, gen_bg)

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

FOREACH_RR(bgx, gen_bgx)

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

FOREACH_RR(mpy, gen_mpy)
FOREACH_RI10(mpyi, gen_mpy)

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

FOREACH_RR(mpyu, gen_mpyu)
FOREACH_RI10(mpyui, gen_mpyu)

static void gen_mpya(TCGv out, TCGv a, TCGv b, TCGv c)
{
    TCGv t = tcg_temp_new();
    gen_mpy(t, a, b);
    tcg_gen_add_tl(out, t, c);
}

FOREACH_RRR(mpya, gen_mpya)

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

FOREACH_RR(mpyh, gen_mpyh)

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

FOREACH_RR(mpys, gen_mpys)

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

FOREACH_RR(mpyhh, gen_mpyhh)

static void gen_mpyhha(TCGv out, TCGv a, TCGv b)
{
    TCGv t = tcg_temp_new();

    gen_mpyhh(t, a, b);
    tcg_gen_add_tl(out, out, t);

    tcg_temp_free(t);
}

FOREACH_RR(mpyhha, gen_mpyhha)

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

FOREACH_RR(mpyhhu, gen_mpyhhu)

static void gen_mpyhhau(TCGv out, TCGv a, TCGv b)
{
    TCGv t = tcg_temp_new();

    gen_mpyhhu(t, a, b);
    tcg_gen_add_tl(out, out, t);

    tcg_temp_free(t);
}

FOREACH_RR(mpyhhau, gen_mpyhhau)

FOREACH_RR1(clz, gen_helper_clz)
FOREACH_RR1(cntb, gen_helper_cntb)

static ExitStatus insn_fsmb(DisassContext *ctx, uint32_t insn)
{
    TCGv temp = tcg_temp_new();
    DISASS_RR1;

    tcg_gen_mov_tl(temp, cpu_gpr[ra][0]);
    gen_helper_fsmb(cpu_gpr[rt][0], temp);
    tcg_gen_shli_tl(temp, temp, 4);
    gen_helper_fsmb(cpu_gpr[rt][1], temp);
    tcg_gen_shli_tl(temp, temp, 4);
    gen_helper_fsmb(cpu_gpr[rt][2], temp);
    tcg_gen_shli_tl(temp, temp, 4);
    gen_helper_fsmb(cpu_gpr[rt][3], temp);
    tcg_temp_free(temp);
    return NO_EXIT;
}

static ExitStatus insn_fsmh(DisassContext *ctx, uint32_t insn)
{
    TCGv temp = tcg_temp_new();
    DISASS_RR1;

    tcg_gen_mov_tl(temp, cpu_gpr[ra][0]);
    gen_helper_fsmh(cpu_gpr[rt][0], temp);
    tcg_gen_shli_tl(temp, temp, 2);
    gen_helper_fsmh(cpu_gpr[rt][1], temp);
    tcg_gen_shli_tl(temp, temp, 2);
    gen_helper_fsmh(cpu_gpr[rt][2], temp);
    tcg_gen_shli_tl(temp, temp, 2);
    gen_helper_fsmh(cpu_gpr[rt][3], temp);

    tcg_temp_free(temp);
    return NO_EXIT;
}

static ExitStatus insn_fsm(DisassContext *ctx, uint32_t insn)
{
    TCGv hold, test;
    int i;
    DISASS_RR1;

    hold = tcg_temp_new();
    test = tcg_temp_new();

    tcg_gen_mov_tl(hold, cpu_gpr[ra][0]);
    for (i = 0; i < 4; ++i) {
        tcg_gen_shri_tl(test, hold, 3 - i);
        tcg_gen_andi_tl(test, test, 1);
        tcg_gen_neg_tl(cpu_gpr[rt][i], test);
    }

    tcg_temp_free(test);
    tcg_temp_free(hold);
    return NO_EXIT;
}

static ExitStatus insn_gbb(DisassContext *ctx, uint32_t insn)
{
    DISASS_RR1;

    gen_helper_gbb(cpu_gpr[rt][0], cpu_gpr[ra][0], cpu_gpr[ra][1],
                   cpu_gpr[ra][2], cpu_gpr[ra][3]);
    tcg_gen_movi_tl(cpu_gpr[rt][1], 0);
    tcg_gen_movi_tl(cpu_gpr[rt][2], 0);
    tcg_gen_movi_tl(cpu_gpr[rt][3], 0);
    return NO_EXIT;
}

static ExitStatus insn_gbh(DisassContext *ctx, uint32_t insn)
{
    DISASS_RR1;

    gen_helper_gbh(cpu_gpr[rt][0], cpu_gpr[ra][0], cpu_gpr[ra][1],
                   cpu_gpr[ra][2], cpu_gpr[ra][3]);
    tcg_gen_movi_tl(cpu_gpr[rt][1], 0);
    tcg_gen_movi_tl(cpu_gpr[rt][2], 0);
    tcg_gen_movi_tl(cpu_gpr[rt][3], 0);
    return NO_EXIT;
}

static ExitStatus insn_gb(DisassContext *ctx, uint32_t insn)
{
    DISASS_RR1;

    gen_helper_gb(cpu_gpr[rt][0], cpu_gpr[ra][0], cpu_gpr[ra][1],
                  cpu_gpr[ra][2], cpu_gpr[ra][3]);
    tcg_gen_movi_tl(cpu_gpr[rt][1], 0);
    tcg_gen_movi_tl(cpu_gpr[rt][2], 0);
    tcg_gen_movi_tl(cpu_gpr[rt][3], 0);
    return NO_EXIT;
}

FOREACH_RR(avgb, gen_helper_avgb)
FOREACH_RR(absdb, gen_helper_absdb)
FOREACH_RR(sumb, gen_helper_sumb)

static void gen_xsbh(TCGv out, TCGv in)
{
    TCGv temp = tcg_temp_new();

    tcg_gen_ext8s_tl(temp, in);
    tcg_gen_shli_tl(out, in, 8);
    tcg_gen_sari_tl(out, out, 8);
    tcg_gen_deposit_tl(out, out, temp, 0, 16);

    tcg_temp_free(temp);
}

FOREACH_RR1(xsbh, gen_xsbh)
FOREACH_RR1(xshw, tcg_gen_ext16s_tl)

static ExitStatus insn_xswd(DisassContext *ctx, uint32_t insn)
{
    DISASS_RR1;

    tcg_gen_sari_tl(cpu_gpr[rt][0], cpu_gpr[ra][1], 31);
    tcg_gen_mov_tl(cpu_gpr[rt][1], cpu_gpr[ra][1]);
    tcg_gen_sari_tl(cpu_gpr[rt][2], cpu_gpr[ra][3], 31);
    tcg_gen_mov_tl(cpu_gpr[rt][3], cpu_gpr[ra][3]);
    return NO_EXIT;
}

FOREACH_RR(and, tcg_gen_and_tl)
FOREACH_RR(andc, tcg_gen_andc_tl)
FOREACH_RI10_ADJ(andbi, tcg_gen_and_tl, imm &= 0xff; imm *= 0x01010101)
FOREACH_RI10_ADJ(andhi, tcg_gen_and_tl, imm &= 0xffff; imm |= imm << 16)
FOREACH_RI10(andi, tcg_gen_and_tl)

FOREACH_RR(or, tcg_gen_or_tl)
FOREACH_RR(orc, tcg_gen_orc_tl)
FOREACH_RI10_ADJ(orbi, tcg_gen_or_tl, imm &= 0xff; imm *= 0x01010101)
FOREACH_RI10_ADJ(orhi, tcg_gen_or_tl, imm &= 0xffff; imm |= imm << 16)
FOREACH_RI10(ori, tcg_gen_or_tl)

static ExitStatus insn_orx(DisassContext *ctx, uint32_t insn)
{
    DISASS_RR1;

    tcg_gen_or_tl(cpu_gpr[rt][0], cpu_gpr[ra][0], cpu_gpr[ra][1]);
    tcg_gen_or_tl(cpu_gpr[rt][0], cpu_gpr[rt][0], cpu_gpr[ra][2]);
    tcg_gen_or_tl(cpu_gpr[rt][0], cpu_gpr[rt][0], cpu_gpr[ra][3]);
    tcg_gen_movi_tl(cpu_gpr[rt][1], 0);
    tcg_gen_movi_tl(cpu_gpr[rt][2], 0);
    tcg_gen_movi_tl(cpu_gpr[rt][3], 0);
    return NO_EXIT;
}

FOREACH_RR(xor, tcg_gen_xor_tl)
FOREACH_RI10_ADJ(xorbi, tcg_gen_xor_tl, imm &= 0xff; imm *= 0x01010101)
FOREACH_RI10_ADJ(xorhi, tcg_gen_xor_tl, imm &= 0xffff; imm |= imm << 16)
FOREACH_RI10(xori, tcg_gen_xor_tl)

FOREACH_RR(nand, tcg_gen_nand_tl)
FOREACH_RR(nor, tcg_gen_nor_tl)
FOREACH_RR(eqv, tcg_gen_eqv_tl)

static void gen_selb(TCGv out, TCGv a, TCGv b, TCGv c)
{
    TCGv temp = tcg_temp_new();

    tcg_gen_and_tl(temp, b, c);
    tcg_gen_andc_tl(out, a, c);
    tcg_gen_or_tl(out, out, temp);

    tcg_temp_free(temp);
}

FOREACH_RRR(selb, gen_selb)

static ExitStatus insn_shufb(DisassContext *ctx, uint32_t insn)
{
    TCGv_ptr pt, pa, pb, pc;
    DISASS_RRR;

    /* The only way to avoid the global state change is to pass three
       complete vectors to a function and return an entire vector.
       Which we cannot do with TCG.  Pass pointers to the vectors instead.  */
    /* ??? We could get away with just passing INSN as a constant and
       re-extracting the register numbers in the helper.  That would be
       more efficient on the TCG side.  */
    pt = tcg_temp_new_ptr();
    pa = tcg_temp_new_ptr();
    pb = tcg_temp_new_ptr();
    pc = tcg_temp_new_ptr();
    tcg_gen_addi_ptr(pt, cpu_env, rt * 16);
    tcg_gen_addi_ptr(pa, cpu_env, ra * 16);
    tcg_gen_addi_ptr(pb, cpu_env, rb * 16);
    tcg_gen_addi_ptr(pc, cpu_env, rc * 16);

    gen_helper_shufb(pt, pa, pb, pc);

    tcg_temp_free_ptr(pt);
    tcg_temp_free_ptr(pa);
    tcg_temp_free_ptr(pb);
    tcg_temp_free_ptr(pc);
    return NO_EXIT;
}

/* ---------------------------------------------------------------------- */
/* Section 6: Shift and Rotate Instructions.  */

FOREACH_RR(shlh, gen_helper_shlh)
FOREACH_RI7_ADJ(shlhi, gen_helper_shlh, imm &= 0x1f; imm |= imm << 16)

static void gen_shl(TCGv out, TCGv a, TCGv b)
{
    TCGv_i64 o64 = tcg_temp_new_i64();
    TCGv_i64 b64 = tcg_temp_new_i64();

    /* Note that the ISA truncates to 6 bits, which means that shift
       values between 32 and 63 have a defined result of 0.  The easy
       way to guarantee that portably is perform a 64-bit shift.  */
    tcg_gen_extu_i32_i64(o64, a);
    tcg_gen_extu_i32_i64(b64, b);
    tcg_gen_shl_i64(o64, o64, b64);
    tcg_gen_trunc_i64_i32(out, o64);

    tcg_temp_free_i64(o64);
    tcg_temp_free_i64(b64);
}

FOREACH_RR(shl, gen_shl)

static ExitStatus insn_shli(DisassContext *ctx, uint32_t insn)
{
    DISASS_RI7;

    imm &= 0x3f;
    if (imm > 31) {
        tcg_gen_movi_tl(cpu_gpr[rt][0], 0);
        tcg_gen_movi_tl(cpu_gpr[rt][1], 0);
        tcg_gen_movi_tl(cpu_gpr[rt][2], 0);
        tcg_gen_movi_tl(cpu_gpr[rt][3], 0);
    } else {
        tcg_gen_shli_tl(cpu_gpr[rt][0], cpu_gpr[ra][0], imm);
        tcg_gen_shli_tl(cpu_gpr[rt][1], cpu_gpr[ra][1], imm);
        tcg_gen_shli_tl(cpu_gpr[rt][2], cpu_gpr[ra][2], imm);
        tcg_gen_shli_tl(cpu_gpr[rt][3], cpu_gpr[ra][3], imm);
    }
    return NO_EXIT;
}

/* Compute OUT = (A||B) << SHL.  SHR is the complement to SHL.  */
/* ??? It would be nice if the x86 SHLD insn was exposed as a primitive.
   We'd be able to use it in expanding normal 64-bit shifts anyway.  */

static void gen_shld(TCGv out, TCGv a, TCGv b, TCGv shl, TCGv shr)
{
    TCGv temp = tcg_temp_new();

    tcg_gen_shr_tl(temp, b, shr);
    tcg_gen_shl_tl(out, a, shl);
    tcg_gen_or_tl(out, out, temp);

    tcg_temp_free(temp);
}

static ExitStatus insn_shlqbi(DisassContext *ctx, uint32_t insn)
{
    TCGv shl, shr;
    int lab_zero, lab_done;
    DISASS_RR;

    lab_zero = gen_new_label();
    lab_done = gen_new_label();

    shl = tcg_temp_local_new();
    tcg_gen_andi_tl(shl, cpu_gpr[rb][0], 7);
    tcg_gen_brcondi_tl(TCG_COND_EQ, shl, 0, lab_zero);

    shr = tcg_const_tl(32);
    tcg_gen_sub_tl(shr, shr, shl);

    gen_shld(cpu_gpr[rt][0], cpu_gpr[ra][0], cpu_gpr[ra][1], shl, shr);
    gen_shld(cpu_gpr[rt][1], cpu_gpr[ra][1], cpu_gpr[ra][2], shl, shr);
    gen_shld(cpu_gpr[rt][2], cpu_gpr[ra][2], cpu_gpr[ra][3], shl, shr);
    tcg_gen_shl_tl(cpu_gpr[rt][3], cpu_gpr[ra][3], shl);

    tcg_temp_free(shr);

    tcg_gen_br(lab_done);
    gen_set_label(lab_zero);

    tcg_gen_mov_tl(cpu_gpr[rt][0], cpu_gpr[ra][0]);
    tcg_gen_mov_tl(cpu_gpr[rt][1], cpu_gpr[ra][1]);
    tcg_gen_mov_tl(cpu_gpr[rt][2], cpu_gpr[ra][2]);
    tcg_gen_mov_tl(cpu_gpr[rt][3], cpu_gpr[ra][3]);

    gen_set_label(lab_done);

    tcg_temp_free(shl);
    return NO_EXIT;
}

static ExitStatus insn_shlqbii(DisassContext *ctx, uint32_t insn)
{
    TCGv shl, shr;
    DISASS_RI7;

    imm &= 7;
    if (imm == 0) {
        tcg_gen_mov_tl(cpu_gpr[rt][0], cpu_gpr[ra][0]);
        tcg_gen_mov_tl(cpu_gpr[rt][1], cpu_gpr[ra][1]);
        tcg_gen_mov_tl(cpu_gpr[rt][2], cpu_gpr[ra][2]);
        tcg_gen_mov_tl(cpu_gpr[rt][3], cpu_gpr[ra][3]);
        return NO_EXIT;
    }

    shl = tcg_const_tl(imm);
    shr = tcg_const_tl(32 - imm);

    gen_shld(cpu_gpr[rt][0], cpu_gpr[ra][0], cpu_gpr[ra][1], shl, shr);
    gen_shld(cpu_gpr[rt][1], cpu_gpr[ra][1], cpu_gpr[ra][2], shl, shr);
    gen_shld(cpu_gpr[rt][2], cpu_gpr[ra][2], cpu_gpr[ra][3], shl, shr);
    tcg_gen_shl_tl(cpu_gpr[rt][3], cpu_gpr[ra][3], shl);

    tcg_temp_free(shl);
    tcg_temp_free(shr);
    return NO_EXIT;
}

static ExitStatus insn_shlqby(DisassContext *ctx, uint32_t insn)
{
    TCGv_ptr pt, pa;
    DISASS_RR;

    pt = tcg_temp_new_ptr();
    pa = tcg_temp_new_ptr();
    tcg_gen_addi_ptr(pt, cpu_env, rt * 16);
    tcg_gen_addi_ptr(pa, cpu_env, ra * 16);

    gen_helper_shlqby(pt, pa, cpu_gpr[rb][0]);

    tcg_temp_free_ptr(pt);
    tcg_temp_free_ptr(pa);
    return NO_EXIT;
}

static ExitStatus insn_shlqbyi(DisassContext *ctx, uint32_t insn)
{
    unsigned shl, shr, i, wofs;
    DISASS_RI7;

    imm &= 0x1f;
    shl = (imm & 3) * 8;
    shr = 32 - shl;
    wofs = imm >> 2;

    if (shl == 0) {
        for (i = 0; i + wofs < 4; ++i) {
            tcg_gen_mov_tl(cpu_gpr[rt][i], cpu_gpr[ra][i + wofs]);
        }
    } else {
        TCGv temp = tcg_temp_new();

        for (i = 0; i + wofs < 4; ++i) {
            tcg_gen_shli_tl(cpu_gpr[rt][i], cpu_gpr[ra][i + wofs], shl);
            if (i > 0) {
                tcg_gen_shri_tl(temp, cpu_gpr[ra][i + wofs], shr);
                tcg_gen_or_tl(cpu_gpr[rt][i - 1], cpu_gpr[rt][i - 1], temp);
            }
        }

        tcg_temp_free(temp);
    }

    for (; i < 4; ++i) {
        tcg_gen_movi_tl(cpu_gpr[rt][i], 0);
    }
    return NO_EXIT;
}

static ExitStatus insn_shlqbybi(DisassContext *ctx, uint32_t insn)
{
    TCGv_ptr pt, pa;
    TCGv temp;
    DISASS_RR;

    pt = tcg_temp_new_ptr();
    pa = tcg_temp_new_ptr();
    tcg_gen_addi_ptr(pt, cpu_env, rt * 16);
    tcg_gen_addi_ptr(pa, cpu_env, ra * 16);

    temp = tcg_temp_new();
    tcg_gen_shri_tl(temp, cpu_gpr[rb][0], 3);

    gen_helper_shlqby(pt, pa, temp);

    tcg_temp_free(temp);
    tcg_temp_free_ptr(pt);
    tcg_temp_free_ptr(pa);
    return NO_EXIT;
}

FOREACH_RR(roth, gen_helper_roth)
FOREACH_RI7_ADJ(rothi, gen_helper_roth, imm &= 0xf; imm |= imm << 16)

FOREACH_RR(rot, tcg_gen_rotl_tl)
FOREACH_RI7_ADJ(roti, tcg_gen_rotl_tl, imm &= 0x1f)

static ExitStatus insn_rotqby(DisassContext *ctx, uint32_t insn)
{
    TCGv_ptr pt, pa;
    DISASS_RR;

    pt = tcg_temp_new_ptr();
    pa = tcg_temp_new_ptr();
    tcg_gen_addi_ptr(pt, cpu_env, rt * 16);
    tcg_gen_addi_ptr(pa, cpu_env, ra * 16);

    gen_helper_rotqby(pt, pa, cpu_gpr[rb][0]);

    tcg_temp_free_ptr(pt);
    tcg_temp_free_ptr(pa);
    return NO_EXIT;
}

static ExitStatus insn_rotqbyi(DisassContext *ctx, uint32_t insn)
{
    TCGv temp[4];
    unsigned shl, shr, i, wofs;
    DISASS_RI7;

    imm &= 15;
    shl = (imm & 3) * 8;
    shr = 32 - shl;
    wofs = imm >> 2;

    alloc_temp(temp);

    if (shl == 0) {
        for (i = 0; i < 4; ++i) {
            tcg_gen_mov_tl(temp[i], cpu_gpr[ra][(i + wofs) & 3]);
        }
    } else {
        TCGv right = tcg_temp_new();

        for (i = 0; i < 4; ++i) {
            tcg_gen_shli_tl(temp[i], cpu_gpr[ra][(i + wofs) & 3], shl);
        }
        for (i = 0; i < 4; ++i) {
            tcg_gen_shri_tl(right, cpu_gpr[ra][(i + 1 + wofs) & 3], shr);
            tcg_gen_or_tl(temp[i], temp[i], right);
        }

        tcg_temp_free(right);
    }

    for (i = 0; i < 4; ++i) {
        tcg_gen_mov_tl(cpu_gpr[rt][i], temp[i]);
    }

    free_temp(temp);
    return NO_EXIT;
}

static ExitStatus insn_rotqbybi(DisassContext *ctx, uint32_t insn)
{
    TCGv_ptr pt, pa;
    TCGv temp;
    DISASS_RR;

    pt = tcg_temp_new_ptr();
    pa = tcg_temp_new_ptr();
    tcg_gen_addi_ptr(pt, cpu_env, rt * 16);
    tcg_gen_addi_ptr(pa, cpu_env, ra * 16);

    temp = tcg_temp_new();
    tcg_gen_shri_tl(temp, cpu_gpr[rb][0], 3);

    gen_helper_rotqby(pt, pa, temp);

    tcg_temp_free(temp);
    tcg_temp_free_ptr(pt);
    tcg_temp_free_ptr(pa);
    return NO_EXIT;
}

static ExitStatus insn_rotqbi(DisassContext *ctx, uint32_t insn)
{
    TCGv shl, shr, hold;
    int lab_zero, lab_done;
    DISASS_RR;

    lab_zero = gen_new_label();
    lab_done = gen_new_label();

    shl = tcg_temp_local_new();
    tcg_gen_andi_tl(shl, cpu_gpr[rb][0], 7);
    tcg_gen_brcondi_tl(TCG_COND_EQ, shl, 0, lab_zero);

    shr = tcg_const_tl(32);
    tcg_gen_sub_tl(shr, shr, shl);

    hold = tcg_temp_new();
    tcg_gen_mov_tl(hold, cpu_gpr[ra][0]);

    gen_shld(cpu_gpr[rt][0], cpu_gpr[ra][0], cpu_gpr[ra][1], shl, shr);
    gen_shld(cpu_gpr[rt][1], cpu_gpr[ra][1], cpu_gpr[ra][2], shl, shr);
    gen_shld(cpu_gpr[rt][2], cpu_gpr[ra][2], cpu_gpr[ra][3], shl, shr);
    gen_shld(cpu_gpr[rt][3], cpu_gpr[ra][3], hold, shl, shr);

    tcg_temp_free(shr);
    tcg_temp_free(hold);

    tcg_gen_br(lab_done);
    gen_set_label(lab_zero);

    tcg_gen_mov_tl(cpu_gpr[rt][0], cpu_gpr[ra][0]);
    tcg_gen_mov_tl(cpu_gpr[rt][1], cpu_gpr[ra][1]);
    tcg_gen_mov_tl(cpu_gpr[rt][2], cpu_gpr[ra][2]);
    tcg_gen_mov_tl(cpu_gpr[rt][3], cpu_gpr[ra][3]);

    gen_set_label(lab_done);

    tcg_temp_free(shl);
    return NO_EXIT;
}

static ExitStatus insn_rotqbii(DisassContext *ctx, uint32_t insn)
{
    TCGv shl, shr, hold;
    DISASS_RI7;

    imm &= 7;
    if (imm == 0) {
        tcg_gen_mov_tl(cpu_gpr[rt][0], cpu_gpr[ra][0]);
        tcg_gen_mov_tl(cpu_gpr[rt][1], cpu_gpr[ra][1]);
        tcg_gen_mov_tl(cpu_gpr[rt][2], cpu_gpr[ra][2]);
        tcg_gen_mov_tl(cpu_gpr[rt][3], cpu_gpr[ra][3]);
        return NO_EXIT;
    }

    shl = tcg_const_tl(imm);
    shr = tcg_const_tl(32 - imm);

    hold = tcg_temp_new();
    tcg_gen_mov_tl(hold, cpu_gpr[ra][0]);

    gen_shld(cpu_gpr[rt][0], cpu_gpr[ra][0], cpu_gpr[ra][1], shl, shr);
    gen_shld(cpu_gpr[rt][1], cpu_gpr[ra][1], cpu_gpr[ra][2], shl, shr);
    gen_shld(cpu_gpr[rt][2], cpu_gpr[ra][2], cpu_gpr[ra][3], shl, shr);
    gen_shld(cpu_gpr[rt][3], cpu_gpr[ra][3], hold, shl, shr);

    tcg_temp_free(shl);
    tcg_temp_free(shr);
    tcg_temp_free(hold);
    return NO_EXIT;
}

FOREACH_RR(rothm, gen_helper_rothm)
FOREACH_RI7_ADJ(rothmi, gen_helper_rothm, imm &= 0x1f; imm |= imm << 16)

static void gen_rotm(TCGv out, TCGv a, TCGv b)
{
    TCGv_i64 o64 = tcg_temp_new_i64();
    TCGv_i64 b64 = tcg_temp_new_i64();
    TCGv binv = tcg_temp_new();

    tcg_gen_neg_tl(binv, b);
    tcg_gen_andi_tl(binv, binv, 63);

    /* Note that the ISA truncates to 6 bits, which means that shift
       values between 32 and 63 have a defined result of 0.  The easy
       way to guarantee that portably is perform a 64-bit shift.  */
    tcg_gen_extu_i32_i64(o64, a);
    tcg_gen_extu_i32_i64(b64, binv);
    tcg_gen_shr_i64(o64, o64, b64);
    tcg_gen_trunc_i64_i32(out, o64);

    tcg_temp_free_i64(o64);
    tcg_temp_free_i64(b64);
    tcg_temp_free(binv);
}

FOREACH_RR(rotm, gen_rotm)

static ExitStatus insn_rotmi(DisassContext *ctx, uint32_t insn)
{
    DISASS_RI7;

    imm = -imm & 0x3f;
    if (imm > 31) {
        tcg_gen_movi_tl(cpu_gpr[rt][0], 0);
        tcg_gen_movi_tl(cpu_gpr[rt][1], 0);
        tcg_gen_movi_tl(cpu_gpr[rt][2], 0);
        tcg_gen_movi_tl(cpu_gpr[rt][3], 0);
    } else {
        tcg_gen_shri_tl(cpu_gpr[rt][0], cpu_gpr[ra][0], imm);
        tcg_gen_shri_tl(cpu_gpr[rt][1], cpu_gpr[ra][1], imm);
        tcg_gen_shri_tl(cpu_gpr[rt][2], cpu_gpr[ra][2], imm);
        tcg_gen_shri_tl(cpu_gpr[rt][3], cpu_gpr[ra][3], imm);
    }
    return NO_EXIT;
}

/* Compute OUT = (A||B) >> SHR.  SHL is the complement to SHR.  */

static void gen_shrd(TCGv out, TCGv a, TCGv b, TCGv shr, TCGv shl)
{
    TCGv temp = tcg_temp_new();

    tcg_gen_shl_tl(temp, a, shl);
    tcg_gen_shr_tl(out, b, shr);
    tcg_gen_or_tl(out, out, temp);

    tcg_temp_free(temp);
}

static ExitStatus insn_rotqmby(DisassContext *ctx, uint32_t insn)
{
    TCGv_ptr pt, pa;
    DISASS_RR;

    pt = tcg_temp_new_ptr();
    pa = tcg_temp_new_ptr();
    tcg_gen_addi_ptr(pt, cpu_env, rt * 16);
    tcg_gen_addi_ptr(pa, cpu_env, ra * 16);

    gen_helper_rotqmby(pt, pa, cpu_gpr[rb][0]);

    tcg_temp_free_ptr(pt);
    tcg_temp_free_ptr(pa);
    return NO_EXIT;
}

static ExitStatus insn_rotqmbyi(DisassContext *ctx, uint32_t insn)
{
    unsigned shl, shr;
    int i, wofs;
    DISASS_RI7;

    imm = -imm & 0x1f;
    shr = (imm & 3) * 8;
    shl = 32 - shr;
    wofs = imm >> 2;

    if (shr == 0) {
        for (i = 3; i - wofs >= 0; --i) {
            tcg_gen_mov_tl(cpu_gpr[rt][i], cpu_gpr[ra][i - wofs]);
        }
    } else {
        TCGv temp = tcg_temp_new();

        for (i = 3; i - wofs >= 0; --i) {
            tcg_gen_shri_tl(cpu_gpr[rt][i], cpu_gpr[ra][i - wofs], shr);
            if (i < 3) {
                tcg_gen_shli_tl(temp, cpu_gpr[ra][i - wofs], shl);
                tcg_gen_or_tl(cpu_gpr[rt][i + 1], cpu_gpr[rt][i + 1], temp);
            }
        }

        tcg_temp_free(temp);
    }

    for (; i >= 0; --i) {
        tcg_gen_movi_tl(cpu_gpr[rt][i], 0);
    }
    return NO_EXIT;
}

static ExitStatus insn_rotqmbybi(DisassContext *ctx, uint32_t insn)
{
    TCGv_ptr pt, pa;
    TCGv temp;
    DISASS_RR;

    pt = tcg_temp_new_ptr();
    pa = tcg_temp_new_ptr();
    tcg_gen_addi_ptr(pt, cpu_env, rt * 16);
    tcg_gen_addi_ptr(pa, cpu_env, ra * 16);

    temp = tcg_temp_new();
    tcg_gen_shri_tl(temp, cpu_gpr[rb][0], 3);

    gen_helper_shlqby(pt, pa, temp);

    tcg_temp_free(temp);
    tcg_temp_free_ptr(pt);
    tcg_temp_free_ptr(pa);
    return NO_EXIT;
}

static ExitStatus insn_rotqmbi(DisassContext *ctx, uint32_t insn)
{
    TCGv shl, shr;
    int lab_zero, lab_done;
    DISASS_RR;

    lab_zero = gen_new_label();
    lab_done = gen_new_label();

    shr = tcg_temp_local_new();
    tcg_gen_neg_tl(shr, cpu_gpr[rb][0]);
    tcg_gen_andi_tl(shr, shr, 7);
    tcg_gen_brcondi_tl(TCG_COND_EQ, shr, 0, lab_zero);

    shl = tcg_const_tl(32);
    tcg_gen_sub_tl(shl, shl, shr);

    gen_shrd(cpu_gpr[rt][3], cpu_gpr[ra][2], cpu_gpr[ra][3], shr, shl);
    gen_shrd(cpu_gpr[rt][2], cpu_gpr[ra][1], cpu_gpr[ra][2], shr, shl);
    gen_shrd(cpu_gpr[rt][1], cpu_gpr[ra][0], cpu_gpr[ra][1], shr, shl);
    tcg_gen_shr_tl(cpu_gpr[rt][0], cpu_gpr[ra][0], shr);

    tcg_temp_free(shl);

    tcg_gen_br(lab_done);
    gen_set_label(lab_zero);

    tcg_gen_mov_tl(cpu_gpr[rt][0], cpu_gpr[ra][0]);
    tcg_gen_mov_tl(cpu_gpr[rt][1], cpu_gpr[ra][1]);
    tcg_gen_mov_tl(cpu_gpr[rt][2], cpu_gpr[ra][2]);
    tcg_gen_mov_tl(cpu_gpr[rt][3], cpu_gpr[ra][3]);

    gen_set_label(lab_done);

    tcg_temp_free(shr);
    return NO_EXIT;
}

static ExitStatus insn_rotqmbii(DisassContext *ctx, uint32_t insn)
{
    TCGv shl, shr;
    DISASS_RI7;

    imm = -imm & 7;
    if (imm == 0) {
        tcg_gen_mov_tl(cpu_gpr[rt][0], cpu_gpr[ra][0]);
        tcg_gen_mov_tl(cpu_gpr[rt][1], cpu_gpr[ra][1]);
        tcg_gen_mov_tl(cpu_gpr[rt][2], cpu_gpr[ra][2]);
        tcg_gen_mov_tl(cpu_gpr[rt][3], cpu_gpr[ra][3]);
        return NO_EXIT;
    }

    shr = tcg_const_tl(imm);
    shl = tcg_const_tl(32 - imm);

    gen_shrd(cpu_gpr[rt][3], cpu_gpr[ra][2], cpu_gpr[ra][3], shr, shl);
    gen_shrd(cpu_gpr[rt][2], cpu_gpr[ra][1], cpu_gpr[ra][2], shr, shl);
    gen_shrd(cpu_gpr[rt][1], cpu_gpr[ra][0], cpu_gpr[ra][1], shr, shl);
    tcg_gen_shr_tl(cpu_gpr[rt][0], cpu_gpr[ra][0], shr);

    tcg_temp_free(shl);
    tcg_temp_free(shr);
    return NO_EXIT;
}

FOREACH_RR(rotmah, gen_helper_rotmah)
FOREACH_RI7_ADJ(rotmahi, gen_helper_rotmah, imm &= 0x1f; imm |= imm << 16)

static void gen_rotma(TCGv out, TCGv a, TCGv b)
{
    TCGv shr = tcg_temp_new();
    TCGv max = tcg_temp_new();

    /* Unlike the other shift operations, we're always replicating the
       most significant bit, which means we can simply bound the shift
       count by 31.  */
    tcg_gen_neg_tl(shr, b);
    tcg_gen_shli_tl(max, shr, 32 - 6);
    tcg_gen_sari_tl(max, max, 31);
    tcg_gen_or_tl(shr, shr, max);
    tcg_gen_andi_tl(shr, shr, 31);

    tcg_gen_sar_tl(out, a, shr);

    tcg_temp_free(shr);
    tcg_temp_free(max);
}

FOREACH_RR(rotma, gen_rotma)

static ExitStatus insn_rotmai(DisassContext *ctx, uint32_t insn)
{
    DISASS_RI7;

    imm = -imm & 0x3f;
    if (imm > 31) {
        imm = 31;
    }
    tcg_gen_shri_tl(cpu_gpr[rt][0], cpu_gpr[ra][0], imm);
    tcg_gen_shri_tl(cpu_gpr[rt][1], cpu_gpr[ra][1], imm);
    tcg_gen_shri_tl(cpu_gpr[rt][2], cpu_gpr[ra][2], imm);
    tcg_gen_shri_tl(cpu_gpr[rt][3], cpu_gpr[ra][3], imm);
    return NO_EXIT;
}

/* ---------------------------------------------------------------------- */
/* Section 7: Compare, Branch, and Halt Instructions.  */

static ExitStatus gen_halt_cond(DisassContext *ctx, TCGCond c, TCGv a, TCGv b)
{
    int lab_over = gen_new_label();

    tcg_gen_brcond_tl(tcg_invert_cond(c), a, b, lab_over);
    gen_excp(ctx, EXCP_HALT, 0);

    gen_set_label(lab_over);
    return NO_EXIT;
}

static ExitStatus insn_heq(DisassContext *ctx, uint32_t insn)
{
    DISASS_RR;

    (void)rt;
    return gen_halt_cond(ctx, TCG_COND_EQ, cpu_gpr[ra][0], cpu_gpr[rb][0]);
}

static ExitStatus insn_heqi(DisassContext *ctx, uint32_t insn)
{
    DISASS_RI10;

    (void)rt;
    return gen_halt_cond(ctx, TCG_COND_EQ, cpu_gpr[ra][0], tcg_const_tl(imm));
}

static ExitStatus insn_hgt(DisassContext *ctx, uint32_t insn)
{
    DISASS_RR;

    (void)rt;
    return gen_halt_cond(ctx, TCG_COND_GT, cpu_gpr[ra][0], cpu_gpr[rb][0]);
}

static ExitStatus insn_hgti(DisassContext *ctx, uint32_t insn)
{
    DISASS_RI10;

    (void)rt;
    return gen_halt_cond(ctx, TCG_COND_GT, cpu_gpr[ra][0], tcg_const_tl(imm));
}

static ExitStatus insn_hlgt(DisassContext *ctx, uint32_t insn)
{
    DISASS_RR;

    (void)rt;
    return gen_halt_cond(ctx, TCG_COND_GTU, cpu_gpr[ra][0], cpu_gpr[rb][0]);
}

static ExitStatus insn_hlgti(DisassContext *ctx, uint32_t insn)
{
    DISASS_RI10;

    (void)rt;
    return gen_halt_cond(ctx, TCG_COND_GTU, cpu_gpr[ra][0], tcg_const_tl(imm));
}

FOREACH_RR(ceqb, gen_helper_ceqb)
FOREACH_RI10_ADJ(ceqbi, gen_helper_ceqb, imm &= 0xff; imm *= 0x01010101)

FOREACH_RR(ceqh, gen_helper_ceqh)
FOREACH_RI10_ADJ(ceqhi, gen_helper_ceqh, imm &= 0xffff; imm |= imm << 16)

static void gen_ceq(TCGv out, TCGv a, TCGv b)
{
    tcg_gen_setcond_tl(TCG_COND_EQ, out, a, b);
    tcg_gen_neg_tl(out, out);
}

FOREACH_RR(ceq, gen_ceq)
FOREACH_RI10(ceqi, gen_ceq)

FOREACH_RR(cgtb, gen_helper_cgtb)
FOREACH_RI10_ADJ(cgtbi, gen_helper_cgtb, imm &=0xff; imm *= 0x01010101)

FOREACH_RR(cgth, gen_helper_cgth)
FOREACH_RI10_ADJ(cgthi, gen_helper_cgth, imm &= 0xffff; imm |= imm << 16)

static void gen_cgt(TCGv out, TCGv a, TCGv b)
{
    tcg_gen_setcond_tl(TCG_COND_GT, out, a, b);
    tcg_gen_neg_tl(out, out);
}

FOREACH_RR(cgt, gen_cgt)
FOREACH_RI10(cgti, gen_cgt)

FOREACH_RR(clgtb, gen_helper_clgtb)
FOREACH_RI10_ADJ(clgtbi, gen_helper_clgtb, imm &=0xff; imm *= 0x01010101)

FOREACH_RR(clgth, gen_helper_clgth)
FOREACH_RI10_ADJ(clgthi, gen_helper_clgth, imm &=0xffff;imm |= imm << 16)

static void gen_clgt(TCGv out, TCGv a, TCGv b)
{
    tcg_gen_setcond_tl(TCG_COND_GTU, out, a, b);
    tcg_gen_neg_tl(out, out);
}

FOREACH_RR(clgt, gen_clgt)
FOREACH_RI10(clgti, gen_clgt)

/* ---------------------------------------------------------------------- */

typedef ExitStatus insn_fn(DisassContext *ctx, uint32_t insn);

/* Up to 11 bits are considered "opcode", depending on the format.
   To make things easy to pull out of the ISA document, we arrange
   the switch statement with a left-aligned 12-bits.  Therefore
   the number in the switch can be read directly from the left
   aligned ISA document, padded on the right with enough bits to
   make up 5 hexidecimal digits.  */
static insn_fn * const translate_table[0x1000] = {
    /* RRR Instruction Format (4-bit op).  */
    [0x800] = insn_selb,
    [0xb00] = insn_shufb,
    [0xc00] = insn_mpya,
//  case 0xd00: _(FNMS);
//  case 0xe00: _(FMA);
//  case 0xf00: _(FMS);

    /* RI18 Instruction Format (7-bit op).  */
    [0x420] = insn_ila,
//  case 0x100: _(HBRA);
//  case 0x120: _(HBRR);

    /* RI10 Instruction Format (8-bit op).  */
    [0x340] = insn_lqd,
    [0x240] = insn_stqd,

    [0x1d0] = insn_ahi,
    [0x1c0] = insn_ai,
    [0x0d0] = insn_sfhi,
    [0x0c0] = insn_sfi,

    [0x740] = insn_mpyi,
    [0x750] = insn_mpyui,

    [0x160] = insn_andbi,
    [0x150] = insn_andhi,
    [0x140] = insn_andi,
    [0x060] = insn_orbi,
    [0x050] = insn_orhi,
    [0x040] = insn_ori,
    [0x460] = insn_xorbi,
    [0x450] = insn_xorhi,
    [0x440] = insn_xori,

    [0x7f0] = insn_heqi,
    [0x4f0] = insn_hgti,
    [0x5f0] = insn_hlgti,
    [0x7e0] = insn_ceqbi,
    [0x7d0] = insn_ceqhi,
    [0x7c0] = insn_ceqi,
    [0x4e0] = insn_cgtbi,
    [0x4d0] = insn_cgthi,
    [0x4c0] = insn_cgti,
    [0x5e0] = insn_clgtbi,
    [0x5d0] = insn_clgthi,
    [0x5c0] = insn_clgti,

    /* RI16 Instruction Format (9-bit op).  */
    [0x308] = insn_lqa,
    [0x338] = insn_lqr,
    [0x208] = insn_stqa,
    [0x238] = insn_stqr,

    [0x418] = insn_ilh,
    [0x410] = insn_ilhu,
    [0x408] = insn_il,
    [0x608] = insn_iohl,
    [0x328] = insn_fsmbi,

//  case 0x320: _(BR);
//  case 0x300: _(BRA);
//  case 0x330: _(BRSL);
//  case 0x310: _(BRASL);
//  case 0x210: _(BRNZ);
//  case 0x200: _(BRZ);
//  case 0x230: _(BRHNZ);
//  case 0x220: _(BRHZ);

    /* RR/RI7 Instruction Format (11-bit op).  */
    [0x388] = insn_lqx,
    [0x288] = insn_stqx,

//  case 0x3e8: _(CBD);
//  case 0x3a8: _(CBX);
//  case 0x3ea: _(CHD);
//  case 0x3aa: _(CHX);
//  case 0x3ec: _(CWD);
//  case 0x3ac: _(CWX);
//  case 0x3ee: _(CDD);
//  case 0x3ae: _(CDX);

    [0x190] = insn_ah,
    [0x180] = insn_a,
    [0x090] = insn_sfh,
    [0x080] = insn_sf,
    [0x680] = insn_addx,
    [0x184] = insn_cg,
    [0x684] = insn_cgx,
    [0x682] = insn_sfx,
    [0x084] = insn_bg,
    [0x686] = insn_bgx,

    [0x788] = insn_mpy,
    [0x798] = insn_mpyu,
    [0x78a] = insn_mpyh,
    [0x78e] = insn_mpys,
    [0x78c] = insn_mpyhh,
    [0x68c] = insn_mpyhha,
    [0x79c] = insn_mpyhhu,
    [0x69c] = insn_mpyhhau,

    [0x54a] = insn_clz,
    [0x568] = insn_cntb,
    [0x36c] = insn_fsmb,
    [0x36a] = insn_fsmh,
    [0x368] = insn_fsm,
    [0x364] = insn_gbb,
    [0x362] = insn_gbh,
    [0x360] = insn_gb,
    [0x1a6] = insn_avgb,
    [0x0a6] = insn_absdb,
    [0x4a6] = insn_sumb,
    [0x56c] = insn_xsbh,
    [0x55c] = insn_xshw,
    [0x54c] = insn_xswd,
    [0x182] = insn_and,
    [0x582] = insn_andc,
    [0x082] = insn_or,
    [0x592] = insn_orc,
    [0x3e0] = insn_orx,
    [0x482] = insn_xor,
    [0x192] = insn_nand,
    [0x092] = insn_nor,
    [0x492] = insn_eqv,

    [0x0be] = insn_shlh,
    [0x0fe] = insn_shlhi,
    [0x0b6] = insn_shl,
    [0x0f6] = insn_shli,
    [0x3b6] = insn_shlqbi,
    [0x3f6] = insn_shlqbii,
    [0x3be] = insn_shlqby,
    [0x3fe] = insn_shlqbyi,
    [0x39e] = insn_shlqbybi,
    [0x0b8] = insn_roth,
    [0x0f8] = insn_rothi,
    [0x0b0] = insn_rot,
    [0x0f0] = insn_roti,
    [0x3b8] = insn_rotqby,
    [0x3f8] = insn_rotqbyi,
    [0x398] = insn_rotqbybi,
    [0x3b0] = insn_rotqbi,
    [0x3f0] = insn_rotqbii,
    [0x0ba] = insn_rothm,
    [0x0fa] = insn_rothmi,
    [0x0b2] = insn_rotm,
    [0x0f2] = insn_rotmi,
    [0x3ba] = insn_rotqmby,
    [0x3fa] = insn_rotqmbyi,
    [0x39a] = insn_rotqmbybi,
    [0x3b2] = insn_rotqmbi,
    [0x3f2] = insn_rotqmbii,
    [0x0bc] = insn_rotmah,
    [0x0fc] = insn_rotmahi,
    [0x0b4] = insn_rotma,
    [0x0f4] = insn_rotmai,

    [0x7b0] = insn_heq,
    [0x2b0] = insn_hgt,
    [0x5b0] = insn_hlgt,
    [0x7a0] = insn_ceqb,
    [0x790] = insn_ceqh,
    [0x780] = insn_ceq,
    [0x4a0] = insn_cgtb,
    [0x490] = insn_cgth,
    [0x480] = insn_cgt,
    [0x5a0] = insn_clgtb,
    [0x590] = insn_clgth,
    [0x580] = insn_clgt,
//  case 0x3a0: _(BI);
//  case 0x354: _(IRET);
//  case 0x356: _(BISLED);
//  case 0x352: _(BISL);
//  case 0x250: _(BIZ);
//  case 0x252: _(BINZ);
//  case 0x254: _(BIHZ);
//  case 0x256: _(BIHNZ);
//  case 0x358: _(HBR);
//  case 0x588: _(FA);
//  case 0x598: _(DFA);
//  case 0x58a: _(FS);
//  case 0x59a: _(DFS);
//  case 0x58c: _(FM);
//  case 0x59c: _(DFM);
//  case 0x6b8: _(DFMA);
//  case 0x6bc: _(DFNMS);
//  case 0x6ba: _(DFMS);
//  case 0x6be: _(DFNMA);
//  case 0x370: _(FREST);
//  case 0x372: _(FRSQEST);
//  case 0x7a8: _(FI);
//  case 0x768: _(CSFLT);
//  case 0x760: _(CFLTS);
//  case 0x76c: _(CUFLT);
//  case 0x764: _(CFLTU);
//  case 0x772: _(FRDS);
//  case 0x770: _(FESD);
//  case 0x786: _(DFCEQ);
//  case 0x796: _(DFCMEQ);
//  case 0x586: _(DFCGT);
//  case 0x596: _(DFCMGT);
//  case 0x77e: _(DFTSV);
//  case 0x784: _(FCEQ);
//  case 0x794: _(FCMEQ);
//  case 0x584: _(FCGT);
//  case 0x594: _(FCMGT);
//  case 0x774: _(FSCRWR);
//  case 0x730: _(FSCRRD);
//  case 0x000: _(STOP);
//  case 0x280: _(STOPD);
//  case 0x002: _(LNOP);
//  case 0x402: _(NOP);
//  case 0x004: _(SYNC);
//  case 0x006: _(DSYNC);
//  case 0x018: _(MFSPR);
//  case 0x218: _(MTSPR);
//  case 0x01a: _(RDCH);
//  case 0x01e: _(RCHCNT);
//  case 0x21a: _(WRCH);
};

static ExitStatus translate_1(DisassContext *ctx, uint32_t insn)
{
    insn_fn *fn;
    uint32_t op;

    /* Sadly, it does not appear as if, except for certain cases, that
       there are dedicated opcode bits that indicate the width of the
       opcode.  So we try matching with increasing opcode width until
       we get a match.  */
    if (insn & 0x80000000) {
        op = (insn >> 20) & 0xf00;
        fn = translate_table[op];
    } else {
        int width;
        for (width = 7; width <= 11; ++width) {
            op = (-1u << (12 - width)) & 0xfff;
            op &= insn >> 20;
            fn = translate_table[op];
            if (fn != NULL) {
                break;
            }
        }
    }
    if (fn == NULL) {
        qemu_log("Unimplemented opcode: 0x%x\n", op);
        gen_excp(ctx, EXCP_ILLOPC, 0);
        return EXIT_NORETURN;
    }

    return fn(ctx, insn);
}

static inline void gen_intermediate_code_internal(CPUState *env,
                                                  TranslationBlock *tb,
                                                  int search_pc)
{
    DisassContext ctx, *ctxp = &ctx;
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
