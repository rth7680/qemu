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

#define FOREACH_RI10(NAME, FN)						\
static ExitStatus insn_##NAME(DisassContext *ctx, uint32_t insn)	\
{									\
    TCGv temp[4];							\
    DISASS_RI10;							\
    load_temp_imm(temp, imm);						\
    foreach_op3(FN, cpu_gpr[rt], cpu_gpr[ra], temp);			\
    free_temp(temp);							\
    return NO_EXIT;							\
}

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
    [0xc00] = insn_mpya,
//  case 0x800: _(SELB);
//  case 0xb00: _(SHUFB);
//  case 0xe00: _(FMA);
//  case 0xd00: _(FNMS);
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

//  case 0x160: _(ANDBI);
//  case 0x150: _(ANDHI);
//  case 0x140: _(ANDI);
//  case 0x060: _(ORBI);
//  case 0x050: _(ORHI);
//  case 0x040: _(ORI);
//  case 0x460: _(XORBI);
//  case 0x450: _(XORHI);
//  case 0x440: _(XORI);
//  case 0x7f0: _(HEQI);
//  case 0x4f0: _(HGTI);
//  case 0x5f0: _(HLGTI);
//  case 0x7e0: _(CEQBI);
//  case 0x7d0: _(CEQHI);
//  case 0x7c0: _(CEQI);
//  case 0x4e0: _(CGTBI);
//  case 0x4d0: _(CGTHI);
//  case 0x4c0: _(CGTI);
//  case 0x5e0: _(CLGTBI);
//  case 0x5d0: _(CLGTHI);
//  case 0x5c0: _(CLGTI);

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
//  case 0x182: _(AND);
//  case 0x582: _(ANDC);
//  case 0x082: _(OR);
//  case 0x592: _(ORC);
//  case 0x3e0: _(ORX);
//  case 0x482: _(XOR);
//  case 0x192: _(NAND);
//  case 0x092: _(NOR);
//  case 0x492: _(EQV);
//  case 0x0be: _(SHLH);
//  case 0x0fe: _(SHLHI);
//  case 0x0b6: _(SHL);
//  case 0x0f6: _(SHLI);
//  case 0x3b6: _(SHLQBI);
//  case 0x3f6: _(SHLQBII);
//  case 0x3be: _(SHLQBY);
//  case 0x3fe: _(SHLQBYI);
//  case 0x39e: _(SHLQBYBI);
//  case 0x0b8: _(ROTH);
//  case 0x0f8: _(ROTHI);
//  case 0x0b0: _(ROT);
//  case 0x0f0: _(ROTI);
//  case 0x3b8: _(ROTQBY);
//  case 0x3f8: _(ROTBYI);
//  case 0x398: _(ROTBYBI);
//  case 0x3b0: _(ROTQBI);
//  case 0x3f0: _(ROTQBII);
//  case 0x0ba: _(ROTHM);
//  case 0x0fa: _(ROTHMI);
//  case 0x0b2: _(ROTM);
//  case 0x0f2: _(ROTMI);
//  case 0x3ba: _(ROTQMBY);
//  case 0x3fa: _(ROTQMBYI);
//  case 0x39a: _(ROTQMBYBI);
//  case 0x3b2: _(ROTQMBI);
//  case 0x3f2: _(ROTQMBII);
//  case 0x0bc: _(ROTMAH);
//  case 0x0fc: _(ROTMAHI);
//  case 0x0b4: _(ROTMA);
//  case 0x0f4: _(ROTMAI);
//  case 0x7b0: _(HEQ);
//  case 0x2b0: _(HGT);
//  case 0x5b0: _(HLGT);
//  case 0x7a0: _(CEQB);
//  case 0x790: _(CEQH);
//  case 0x780: _(CEQ);
//  case 0x4a0: _(CGTB);
//  case 0x490: _(CGTH);
//  case 0x480: _(CGT);
//  case 0x5a0: _(CLGTB);
//  case 0x590: _(CLGTH);
//  case 0x580: _(CLGT);
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
