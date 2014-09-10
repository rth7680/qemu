/*
 * PDP/11 emulation for qemu: main translation routines.
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
#include "cpu.h"
#include "disas/disas.h"
#include "exec/exec-all.h"
#include "exec/cpu_ldst.h"
#include "tcg-op.h"

#include "exec/helper-proto.h"
#include "exec/helper-gen.h"

/*
 * TCG registers
 */
static TCGv_ptr cpu_env;
static TCGv cpu_reg[2][6];
static TCGv cpu_sp[4];
static TCGv cpu_pc;
static TCGv cpu_psw_c;
static TCGv cpu_psw_v;
static TCGv cpu_psw_z;
static TCGv cpu_psw_n;

#include "exec/gen-icount.h"

typedef enum ExitStatus {
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

typedef struct EA {
    TCGv addr;
    int midx;
} EA;

typedef struct DisasContext {
    struct TranslationBlock *tb;
    CPUPDP11State *env;

    uint32_t pc;
    unsigned rs;
    unsigned cm;
    unsigned pm;

    EA dest_ea;
    TCGv tmp[10];
    int ntmp;

    ExitStatus status;
    int singlestep_enabled;
} DisasContext;

static TCGv alloc_tmp(DisasContext *ctx)
{
    TCGv t = tcg_temp_new();
    int i = ctx->ntmp;

    assert(i < ARRAY_SIZE(ctx->tmp));
    ctx->tmp[i] = t;
    ctx->ntmp = i + 1;

    return t;
}

static TCGv raw_reg(DisasContext *ctx, int reg)
{
    switch (reg) {
    default:
        return cpu_reg[ctx->rs][reg];
    case 6:
        return cpu_sp[ctx->cm];
    case 7:
        return cpu_pc;
    }
}

static TCGv src_reg(DisasContext *ctx, int reg)
{
    TCGv ret = raw_reg(ctx, reg);
    if (unlikely(reg == 7)) {
        tcg_gen_movi_i32(ret, ctx->pc);
    }
    return ret;
}

static TCGv dest_reg(DisasContext *ctx, int reg)
{
    if (unlikely(reg == 7)) {
        ctx->status = EXIT_PC_UPDATED;
    }
    return raw_reg(ctx, reg);
}

static uint16_t next_code_word(DisasContext *ctx)
{
    uint16_t word = cpu_lduw_code(ctx->env, ctx->pc);
    ctx->pc = (ctx->pc + 2) & 0xffff;
    return word;
}

static void gen_ibranch(DisasContext *ctx, TCGv dest)
{
    tcg_gen_mov_i32(cpu_pc, dest);
    ctx->status = EXIT_PC_UPDATED;
}

static void gen_branch(DisasContext *ctx, uint16_t dest)
{
    /* ??? Use goto_tb.  */
    tcg_gen_movi_i32(cpu_pc, dest);
    ctx->status = EXIT_PC_UPDATED;
}

static void gen_rbranch(DisasContext *ctx, int ofs)
{
    gen_branch(ctx, ctx->pc + ofs);
}

static void gen_cbranch(DisasContext *ctx, int ofs, TCGCond cond, TCGv val)
{
    TCGv dest = tcg_const_i32(ctx->pc + ofs);
    TCGv next = tcg_const_i32(ctx->pc);
    TCGv zero = tcg_const_i32(0);

    /* ??? Use goto_tb.  */
    tcg_gen_movcond_i32(cond, cpu_pc, val, zero, dest, next);
    ctx->status = EXIT_PC_UPDATED;

    tcg_temp_free(zero);
    tcg_temp_free(next);
    tcg_temp_free(dest);
}

static void gen_excp_nopc(DisasContext *ctx, int code)
{
    TCGv c = tcg_const_i32(code);
    gen_helper_excp(cpu_env, c);
    tcg_temp_free(c);
    ctx->status = EXIT_NORETURN;
}

static void gen_excp(DisasContext *ctx, int code)
{
    tcg_gen_movi_i32(cpu_pc, ctx->pc);
    gen_excp_nopc(ctx, code);
}

static EA load_ea(DisasContext *ctx, int spec, int midx,
                  bool is_byte, bool is_dest)
{
    int regi = spec & 7;
    int size = (is_byte && regi < 6 ? 1 : 2);
    TCGv regv = raw_reg(ctx, regi);
    TCGv addr = alloc_tmp(ctx);
    bool ispace = false;
    EA ea;

    switch (spec) {
    case 010 ... 016: /* Register indirect: (R) */
        tcg_gen_mov_i32(addr, regv);
        goto done;
    case 017:
        tcg_gen_movi_i32(addr, ctx->pc);
        ispace = true;
        goto done;

    case 020 ... 026: /* Autoincrement: (R)+ */
    case 030 ... 036: /* Autoincrement deferred: @(R)+ */
        tcg_gen_mov_i32(addr, regv);
        tcg_gen_addi_i32(regv, regv, size);
        tcg_gen_ext16u_i32(regv, regv);
        break;

    case 027: /* Immediate: #n */
        tcg_gen_movi_i32(addr, ctx->pc);
        ctx->pc = (ctx->pc + 2) & 0xffff;
        ispace = true;
        goto done;

    case 037: /* Absolute: @#n */
        tcg_gen_movi_i32(addr, next_code_word(ctx));
        goto done;

    case 040 ... 046: /* Autodecrement: -(R) */
    case 050 ... 056: /* Autodecrement deferred: @-(R) */
        tcg_gen_subi_i32(regv, regv, size);
        tcg_gen_ext16u_i32(regv, regv);
        tcg_gen_mov_i32(addr, regv);
        /* ??? Kernel stack limit check */
        break;
    case 047:
    case 057:
        ctx->pc = (ctx->pc - 2) & 0xffff;
        tcg_gen_movi_i32(addr, ctx->pc);
        ispace = true;
        break;

    case 060 ... 066: /* Displacement: d(R) */
    case 070 ... 076: /* Displacement deferred: @d(R) */
        tcg_gen_addi_i32(addr, regv, (int16_t)next_code_word(ctx));
        tcg_gen_ext16u_i32(addr, addr);
        break;
    case 067:
    case 077:
        {
            uint16_t disp = ctx->pc;
            disp += next_code_word(ctx);
            tcg_gen_movi_i32(addr, disp);
        }
        break;

    default:
        tcg_abort();
    }

    if (spec & 010) {
        /* All deferred addressing modes.  */
        tcg_gen_qemu_ld_i32(addr, addr, ctx->cm | ispace, MO_LEUW);
    }

 done:
    ea.addr = addr;
    ea.midx = midx | ispace;
    if (is_dest) {
        ctx->dest_ea = ea;
    }
    return ea;
}

static TCGv load_operand(DisasContext *ctx, int spec, bool is_byte, bool rmw)
{
    TCGv val = alloc_tmp(ctx);

    if (spec <= 006) {
        /* Register direct.  */
        TCGv reg = raw_reg(ctx, spec);
        if (is_byte) {
            tcg_gen_ext8s_i32(val, reg);
        } else {
            tcg_gen_mov_i32(val, reg);
        }
    } else if (spec == 007) {
        tcg_gen_movi_i32(val, ctx->pc);
    } else if (spec == 027 && !rmw) {
        /* Immediate.  */
        int imm = next_code_word(ctx);
        if (is_byte) {
            imm = (int8_t)imm;
        }
        tcg_gen_movi_i32(val, imm);
    } else {
        EA ea = load_ea(ctx, spec, ctx->cm, is_byte, rmw);
        tcg_gen_qemu_ld_i32(val, ea.addr, ea.midx, is_byte ? MO_SB : MO_LEUW);
    }

    return val;
}

static void store_operand(DisasContext *ctx, int spec, bool is_byte, TCGv val)
{
    if (spec <= 007) {
        /* Register direct.  */
        TCGv reg = dest_reg(ctx, spec);
        if (is_byte) {
            tcg_gen_deposit_i32(reg, reg, val, 0, 8);
        } else {
            tcg_gen_mov_i32(reg, val);
        }
    } else {
        assert(!TCGV_IS_UNUSED_I32(ctx->dest_ea.addr));
        tcg_gen_qemu_st_i32(val, ctx->dest_ea.addr, ctx->dest_ea.midx,
                            is_byte ? MO_UB : MO_LEUW);
    }
}

static TCGv load_pair(DisasContext *ctx, int reg)
{
    TCGv ret = alloc_tmp(ctx);
    tcg_gen_deposit_i32(ret, src_reg(ctx, reg | 1), src_reg(ctx, reg), 16, 16);
    return ret;
}

static void store_pair(DisasContext *ctx, int reg, TCGv val)
{
    if ((reg & 1) == 0) {
        tcg_gen_shri_i32(dest_reg(ctx, reg), val, 16);
    }
    tcg_gen_ext16u_i32(dest_reg(ctx, reg | 1), val);
}

static void gen_addsub_w(DisasContext *ctx, TCGv dst, TCGv src,
                         bool do_add, bool do_carry, bool is_imm)
{
    TCGv res = tcg_temp_new ();
    TCGv tmp = tcg_temp_new ();

    if (do_add) {
        tcg_gen_add_i32(res, dst, src);
    } else {
        if (do_carry) {
            tcg_gen_setcond_i32(TCG_COND_GEU, cpu_psw_c, dst, src);
        }
        tcg_gen_sub_i32(res, dst, src);
    }

    /* If SRC is an immediate, we can encourage the optimizer to simplify
       the overflow expression by only considering the sign bit.  That will
       cause all non-negative values to be zero, which will lead to the
       XORs used in computation of V going away.  */
    if (is_imm) {
        tcg_gen_andi_i32(src, src, 0x8000);
    }
    tcg_gen_xor_i32(cpu_psw_v, dst, src);
    if (do_add) {
        tcg_gen_xor_i32(tmp, res, src);
        tcg_gen_andc_i32(cpu_psw_v, cpu_psw_v, tmp);
    } else {
        tcg_gen_xor_i32(tmp, res, dst);
        tcg_gen_and_i32(cpu_psw_v, cpu_psw_v, tmp);
    }

    if (do_carry && do_add) {
        tcg_gen_shri_i32(cpu_psw_c, res, 16);
    }
    tcg_gen_ext16u_i32(dst, res);
    tcg_gen_mov_i32(cpu_psw_n, dst);
    tcg_gen_mov_i32(cpu_psw_z, dst);

    tcg_temp_free(tmp);
    tcg_temp_free(res);
}

static void gen_addsub_b(DisasContext *ctx, TCGv dst, TCGv src,
                         bool do_add, bool do_carry, bool is_imm)
{
    TCGv res = tcg_temp_new ();
    TCGv tmp = tcg_temp_new ();

    if (do_add) {
        tcg_gen_add_i32(res, dst, src);
    } else {
        tcg_gen_sub_i32(res, dst, src);
        if (do_carry) {
            tcg_gen_setcond_i32(TCG_COND_GEU, cpu_psw_c, dst, src);
        }
    }

    /* See above re clearing bits from SRC to simplify calculation of V.  */
    if (is_imm) {
        tcg_gen_andi_i32(src, src, -128);
    }
    tcg_gen_xor_i32(cpu_psw_v, dst, src);
    if (do_add) {
        tcg_gen_xor_i32(tmp, res, src);
        tcg_gen_andc_i32(cpu_psw_v, cpu_psw_v, tmp);
    } else {
        tcg_gen_xor_i32(tmp, res, dst);
        tcg_gen_and_i32(cpu_psw_v, cpu_psw_v, tmp);
    }

    tcg_gen_ext8s_i32(cpu_psw_n, res);
    tcg_gen_ext8u_i32(cpu_psw_z, res);
    if (do_carry && do_add) {
        tcg_gen_setcond_i32(TCG_COND_LTU, cpu_psw_c, cpu_psw_n, dst);
    }
    tcg_gen_mov_i32(dst, cpu_psw_z);

    tcg_temp_free(tmp);
    tcg_temp_free(res);
}

static void gen_psw_logic_w(DisasContext *ctx, TCGv dst)
{
    tcg_gen_mov_i32(cpu_psw_n, dst);
    tcg_gen_mov_i32(cpu_psw_z, dst);
    tcg_gen_movi_i32(cpu_psw_v, 0);
}

static void gen_psw_logic_b(DisasContext *ctx, TCGv dst)
{
    tcg_gen_mov_i32(cpu_psw_n, dst);
    tcg_gen_ext8u_i32(cpu_psw_z, dst);
    tcg_gen_movi_i32(cpu_psw_v, 0);
}

static void gen_psw_shift_w(DisasContext *ctx, TCGv dst)
{
    tcg_gen_neg_i32(cpu_psw_v, cpu_psw_c);
    tcg_gen_mov_i32(cpu_psw_n, dst);
    tcg_gen_mov_i32(cpu_psw_z, dst);
    tcg_gen_xor_i32(cpu_psw_v, cpu_psw_v, dst);
}

static void gen_psw_shift_b(DisasContext *ctx, TCGv dst)
{
    tcg_gen_neg_i32(cpu_psw_v, cpu_psw_c);
    tcg_gen_ext8s_i32(cpu_psw_n, dst);
    tcg_gen_ext8u_i32(cpu_psw_z, dst);
    tcg_gen_xor_i32(cpu_psw_v, cpu_psw_v, cpu_psw_n);
}

static void gen_ash(DisasContext *ctx, TCGv src, TCGv shift)
{
    TCGv dl, dr;  /* destination for left and right shift */
    TCGv cl, cr;  /* carry for left and right */
    TCGv sl, sr;  /* shift count for left and right.  */
    TCGv right, zero;

    right = tcg_temp_new();
    sr = tcg_temp_new();
    sl = tcg_temp_new();
    dr = tcg_temp_new();
    dl = tcg_temp_new();
    cr = tcg_temp_new();
    cl = tcg_temp_new();

    /* The sign of the 6-bit shift; negative is right shift.  */
    tcg_gen_andi_i32(right, shift, 32);
    tcg_gen_andi_i32(sl, shift, 31);

    /* Computation of flags:
       N -- Normal.
       Z -- Normal.
       C -- Always the last bit shifted out, or 0 for zero shift.
            R: (x >> (shift-1)) & 1,
            L: ((x << shift) >> 16) & 1.
       V -- Set if the sign bit changes.  This is easy, given that
            psw_v is defined in the sign bit.

       Note: For right shift, since we need shift-1 for C anyway, we
       perform the shift in two steps, trivially handling the case of 32.

    ---- Left --------------------	---- Right ------------------ */
                                        tcg_gen_ext16s_i32(cr, src);
    tcg_gen_shl_i32(dl, src, sl);       tcg_gen_xori_i32(sr, sl, 31);
                                        tcg_gen_sar_i32(cr, cr, sr);
                                        tcg_gen_sari_i32(dr, cr, 1);
    tcg_gen_shri_i32(cl, dl, 16);       tcg_gen_andi_i32(cr, cr, 1);

    /* Merge the shift and carry results.  */
    zero = tcg_const_i32(0);
    tcg_gen_movcond_i32(TCG_COND_NE, dl, right, zero, dr, dl);
    tcg_gen_movcond_i32(TCG_COND_NE, cpu_psw_c, right, zero, cr, cl);
    tcg_temp_free(zero);

    /* Compute final flags and results.  */
    tcg_gen_xor_i32(cpu_psw_v, dl, src);
    tcg_gen_ext16u_i32(src, dl);
    tcg_gen_mov_i32(cpu_psw_n, src);
    tcg_gen_mov_i32(cpu_psw_z, src);

    tcg_temp_free(right);
    tcg_temp_free(sr);
    tcg_temp_free(sl);
    tcg_temp_free(dr);
    tcg_temp_free(dl);
    tcg_temp_free(cr);
    tcg_temp_free(cl);
}

static void gen_ashi(DisasContext *ctx, TCGv src, int shift)
{
    /* Similar to above, but attempt to arrange flags computation
       such that it can all be deleted as dead.  */
    shift = sextract32(shift, 0, 6);
    if (unlikely(shift == 0)) {
        tcg_gen_movi_i32(cpu_psw_v, 0);
        tcg_gen_movi_i32(cpu_psw_c, 0);
    } else if (shift < 0) {
        shift = -shift;
        tcg_gen_ext16s_i32(src, src);
        tcg_gen_sari_i32(cpu_psw_c, src, shift - 1);
        tcg_gen_sari_i32(src, src, shift == 32 ? 31 : shift);
        tcg_gen_andi_i32(cpu_psw_c, cpu_psw_c, 1);
        tcg_gen_ext16u_i32(src, src);
        tcg_gen_movi_i32(cpu_psw_v, 0);
    } else {
        tcg_gen_mov_i32(cpu_psw_v, src);
        tcg_gen_shli_i32(src, src, shift);
        tcg_gen_shri_i32(cpu_psw_c, src, 16);
        tcg_gen_ext16u_i32(src, src);
        tcg_gen_andi_i32(cpu_psw_c, cpu_psw_c, 1);
        tcg_gen_xor_i32(cpu_psw_v, cpu_psw_v, src);
    }
    tcg_gen_mov_i32(cpu_psw_n, src);
    tcg_gen_mov_i32(cpu_psw_z, src);
}

static void gen_ashc(DisasContext *ctx, TCGv src, TCGv shift)
{
    TCGv dl, dr;  /* destination for left and right shift */
    TCGv cl, cr;  /* carry for left and right */
    TCGv sl, sr;  /* shift count for left and right.  */
    TCGv right, zero;

    right = tcg_temp_new();
    sr = tcg_temp_new();
    sl = tcg_temp_new();
    dr = tcg_temp_new();
    dl = tcg_temp_new();
    cr = tcg_temp_new();
    cl = tcg_temp_new();

    /* The sign of the 6-bit shift; negative is right shift.  */
    tcg_gen_andi_i32(right, shift, 32);
    tcg_gen_andi_i32(sl, shift, 31);

    /* Computation of flags:
       N -- Normal.
       Z -- Normal.
       C -- Always the last bit shifted out, or 0 for zero shift.
            R: (x >> (shift-1)) & 1,
            L: (x << (shift-1)) >> 31
             : (x >> (32-shift) & 1
       V -- Set if the (32-bit) sign bit changes.

       Note: For right shift, since we need shift-1 for C anyway, we
       perform the shift in two steps, trivially handling the case of 32.

    ---- Left --------------------	---- Right ------------------ */
    tcg_gen_shl_i32(dl, src, sl);       tcg_gen_xori_i32(sr, sl, 31);
    tcg_gen_neg_i32(sl, sl);            tcg_gen_sar_i32(cr, cr, sr);
    tcg_gen_andi_i32(sl, sl, 31);       tcg_gen_sari_i32(dr, cr, 1);
    tcg_gen_shr_i32(cl, src, sl);       tcg_gen_andi_i32(cr, cr, 1);

    zero = tcg_const_i32(0);
    tcg_gen_setcond_i32(TCG_COND_NE, sl, sl, zero);
    tcg_gen_and_i32(cl, cl, sl);

    /* Merge the shift and carry results.  */
    tcg_gen_movcond_i32(TCG_COND_NE, dl, right, zero, dr, dl);
    tcg_gen_movcond_i32(TCG_COND_NE, cpu_psw_c, right, zero, cr, cl);
    tcg_temp_free(zero);

    /* Compute final flags and results.  */
    tcg_gen_xor_i32(cpu_psw_v, dl, src);
    tcg_gen_sari_i32(cpu_psw_n, src, 31);
    tcg_gen_mov_i32(cpu_psw_z, src);
    tcg_gen_sari_i32(cpu_psw_v, cpu_psw_v, 31);

    tcg_temp_free(right);
    tcg_temp_free(sr);
    tcg_temp_free(sl);
    tcg_temp_free(dr);
    tcg_temp_free(dl);
    tcg_temp_free(cr);
    tcg_temp_free(cl);
}

static void gen_ashci(DisasContext *ctx, TCGv src, int shift)
{
    /* Similar to above, but attempt to arrange flags computation
       such that it can all be deleted as dead.  */
    shift = sextract32(shift, 0, 6);
    if (unlikely(shift == 0)) {
        tcg_gen_movi_i32(cpu_psw_v, 0);
        tcg_gen_movi_i32(cpu_psw_c, 0);
    } else if (shift < 0) {
        shift = -shift;
        tcg_gen_sari_i32(cpu_psw_c, src, shift - 1);
        tcg_gen_sari_i32(src, src, shift == 32 ? 31 : shift);
        tcg_gen_andi_i32(cpu_psw_c, cpu_psw_c, 1);
        tcg_gen_sari_i32(cpu_psw_v, src, 31);
    } else {
        tcg_gen_mov_i32(cpu_psw_v, src);
        tcg_gen_shri_i32(cpu_psw_c, src, 32 - shift);
        tcg_gen_shli_i32(src, src, shift);
        tcg_gen_andi_i32(cpu_psw_c, cpu_psw_c, 1);
        tcg_gen_xor_i32(cpu_psw_v, cpu_psw_v, src);
        tcg_gen_sari_i32(cpu_psw_v, cpu_psw_v, 31);
    }
    tcg_gen_sari_i32(cpu_psw_n, src, 31);
    tcg_gen_mov_i32(cpu_psw_z, src);
}

static void gen_spl(DisasContext *ctx, TCGv val)
{
    TCGv psw = tcg_temp_new();

    tcg_gen_ld_i32(psw, cpu_env, offsetof(CPUPDP11State, psw));
    tcg_gen_andi_i32(val, val, PSW_IPL);
    tcg_gen_andi_i32(psw, psw, ~PSW_IPL);
    tcg_gen_or_i32(psw, psw, val);
    tcg_gen_st_i32(psw, cpu_env, offsetof(CPUPDP11State, psw));

    tcg_temp_free(psw);

    /* Let interrupts arrive now.  */
    ctx->status = EXIT_PC_STALE;
}

static void gen_mfp(DisasContext *ctx, int dstspec, bool ispace)
{
    TCGv t1;
    EA ea;

    if (dstspec <= 007) {
        /* Source uses from PM SP.  */
        t1 = alloc_tmp(ctx);
        if (dstspec == 006) {
            tcg_gen_mov_i32(t1, cpu_sp[ctx->pm]);
        } else {
            tcg_gen_mov_i32(t1, src_reg(ctx, dstspec));
        }
    } else {
        /* Address uses CM SP; load uses PM mmu_idx.  */
        ea = load_ea(ctx, dstspec, ctx->pm | ispace, false, false);
        t1 = ea.addr;
        tcg_gen_qemu_ld_i32(t1, t1, ea.midx, MO_LEUW);
    }
    gen_psw_logic_w(ctx, t1);
    ea = load_ea(ctx, 046, ctx->cm, false, false);
    tcg_gen_qemu_st_i32(t1, ea.addr, ea.midx, MO_LEUW);
}

static void gen_mtp(DisasContext *ctx, int dstspec, bool ispace)
{
    TCGv t1;
    EA ea;

    t1 = load_operand(ctx, 026, false, false);
    gen_psw_logic_w(ctx, t1);
    if (dstspec <= 007) {
        /* Destination uses PM SP.  */
        TCGv reg;
        if (dstspec == 6) {
            reg = cpu_sp[ctx->pm];
        } else {
            reg = dest_reg(ctx, dstspec);
        }
        tcg_gen_mov_i32(reg, t1);
    } else {
        /* Address uses CM SP; store uses PM mmu_idx.  */
        ea = load_ea(ctx, dstspec, ctx->pm | ispace, true, false);
        tcg_gen_qemu_st_i32(t1, ea.addr, ea.midx, MO_LEUW);
    }
}

static void translate_one(DisasContext *ctx)
{
    int insn, dstspec, srcspec, bofs;
    TCGv t0, t1, t2;
    EA ea;

    insn = next_code_word(ctx);

    dstspec = insn & 077;
    srcspec = (insn >> 6) & 077;
    bofs = (int8_t)insn * 2;

    switch (insn >> 12) {
    case 000:
        /* Opcode 0: no operands, specials, branches, JSR, SOPs.  */
        switch (srcspec) {
        case 000: /* No operand.  */
            switch (insn) {
            case 0: /* HALT */
                if (ctx->cm != MMU_KERN_D_IDX) {
                    goto do_illegal;
                }
                gen_excp(ctx, EXCP_HALT);
                return;

            case 1: /* WAIT */
                /* No-op, until there are devices.  */
                return;

            case 6: /* RTT */
            case 2: /* RTI */
                /* Pop new PC and new PSW from stack; delay writeback of SP. */
                t0 = alloc_tmp(ctx);
                t1 = alloc_tmp(ctx);
                t2 = alloc_tmp(ctx);
                tcg_gen_mov_tl(t0, cpu_sp[ctx->cm]);
                tcg_gen_qemu_ld_i32(t1, t0, ctx->cm, MO_LEUW);
                tcg_gen_addi_i32(t0, t0, 2);
                tcg_gen_ext16u_i32(t0, t0);
                tcg_gen_qemu_ld_i32(t2, t0, ctx->cm, MO_LEUW);
                tcg_gen_addi_i32(t0, t0, 2);
                tcg_gen_ext16u_i32(cpu_sp[ctx->cm], t0);
                if (ctx->cm == MMU_KERN_D_IDX) {
                    gen_helper_set_psw(cpu_env, t2);
                } else {
                    gen_helper_set_psw_prot(cpu_env, t2);
                }
                gen_ibranch(ctx, t1);
                /* ??? Take trace trap for RTI, but not RTT.  */
                return;

            case 3: /* BPT */
                gen_excp(ctx, EXCP_BPT);
                return;
            case 4: /* IOT */
                gen_excp(ctx, EXCP_IOT);
                return;
            case 5: /* RESET */
                if (ctx->cm == MMU_KERN_D_IDX) {
                    gen_excp(ctx, EXCP_RESET);
                }
                return;

            case 7: /* MFPT */
                goto do_illegal;
            }
            goto do_illegal;

        case 001: /* JMP */
            if (dstspec <= 7) {
                goto do_illegal;
            } else if (dstspec == 037) {
                /* Absolute branch.  */
                gen_branch(ctx, next_code_word(ctx));
            } else if (dstspec == 067) {
                /* Relative branch.  */
                gen_rbranch(ctx, next_code_word(ctx));
            } else {
                /* Indirect branch.  */
                ea = load_ea(ctx, dstspec, ctx->cm, false, false);
                gen_ibranch(ctx, ea.addr);
            }
            return;

        case 002:
            switch (dstspec & 070) {
            case 000: /* RTS */
                gen_ibranch(ctx, src_reg(ctx, dstspec));
                ea = load_ea(ctx, 026, ctx->cm, false, false);
                tcg_gen_qemu_ld_i32(dest_reg(ctx, dstspec), ea.addr,
                                    ea.midx, MO_LEUW);
                return;

            case 030: /* SPL */
                if (ctx->cm != MMU_KERN_D_IDX) {
                    goto do_illegal;
                }
                t1 = tcg_const_i32((insn & 7) << PSW_V_IPL);
                gen_spl(ctx, t1);
                tcg_temp_free(t1);
                return;

            case 040: case 050: /* Clear CC */
                if (dstspec & 1) {
                    tcg_gen_movi_i32(cpu_psw_c, 0);
                }
                if (dstspec & 2) {
                    tcg_gen_movi_i32(cpu_psw_v, 0);
                }
                if (dstspec & 4) {
                    tcg_gen_movi_i32(cpu_psw_z, 1);
                }
                if (dstspec & 8) {
                    tcg_gen_movi_i32(cpu_psw_n, 0);
                }
                return;

            case 060: case 070: /* Set CC */
                if (dstspec & 1) {
                    tcg_gen_movi_i32(cpu_psw_c, 1);
                }
                if (dstspec & 2) {
                    tcg_gen_movi_i32(cpu_psw_v, -1);
                }
                if (dstspec & 4) {
                    tcg_gen_movi_i32(cpu_psw_z, 0);
                }
                if (dstspec & 8) {
                    tcg_gen_movi_i32(cpu_psw_n, -1);
                }
                return;
            }
            goto do_illegal;

        case 003: /* SWAB */
            t1 = load_operand(ctx, dstspec, 0, 1);
            tcg_gen_mov_i32(cpu_psw_z, t1);
            tcg_gen_mov_i32(cpu_psw_n, t1);
            tcg_gen_movi_i32(cpu_psw_v, 0);
            tcg_gen_movi_i32(cpu_psw_c, 0);
            tcg_gen_bswap16_i32(t1, t1);
            store_operand(ctx, dstspec, 0, t1);
            return;

        case 004 ... 007: /* BR */
            gen_rbranch(ctx, bofs);
            return;

        case 010 ... 013: /* BNE */
            gen_cbranch(ctx, bofs, TCG_COND_NE, cpu_psw_z);
            return;

        case 014 ... 017: /* BEQ */
            gen_cbranch(ctx, bofs, TCG_COND_EQ, cpu_psw_z);
            return;

        case 020 ... 023: /* BGE */
            t1 = alloc_tmp(ctx);
            tcg_gen_xor_i32(t1, cpu_psw_n, cpu_psw_v);
            tcg_gen_andi_i32(t1, t1, 0x8000);
            gen_cbranch(ctx, bofs, TCG_COND_EQ, t1);
            return;

        case 024 ... 027: /* BLT */
            t1 = alloc_tmp(ctx);
            tcg_gen_xor_i32(t1, cpu_psw_n, cpu_psw_v);
            tcg_gen_andi_i32(t1, t1, 0x8000);
            gen_cbranch(ctx, bofs, TCG_COND_NE, t1);
            return;

        case 030 ... 033: /* BGT */
            t1 = alloc_tmp(ctx);
            tcg_gen_xor_i32(t1, cpu_psw_n, cpu_psw_v);
            tcg_gen_andi_i32(t1, t1, 0x8000);
            t2 = alloc_tmp(ctx);
            tcg_gen_setcondi_i32(TCG_COND_EQ, t2, cpu_psw_z, 0);
            tcg_gen_or_i32(t1, t1, t2);
            gen_cbranch(ctx, bofs, TCG_COND_EQ, t1);
            return;

        case 034 ... 037: /* BLE */
            t1 = alloc_tmp(ctx);
            tcg_gen_xor_i32(t1, cpu_psw_n, cpu_psw_v);
            tcg_gen_andi_i32(t1, t1, 0x8000);
            t2 = alloc_tmp(ctx);
            tcg_gen_setcondi_i32(TCG_COND_EQ, t2, cpu_psw_z, 0);
            tcg_gen_or_i32(t1, t1, t2);
            gen_cbranch(ctx, bofs, TCG_COND_NE, t1);
            return;

        case 040 ... 047: /* JSR */
            if (dstspec <= 7) {
                goto do_illegal;
            } else {
                bool direct;
                uint16_t dest;

                if (dstspec == 037) {
                    /* Absolute branch.  */
                    dest = next_code_word(ctx);
                    direct = true;
                } else if (dstspec == 067) {
                    /* Relative branch.  */
                    dest = ctx->pc;
                    dest += next_code_word(ctx);
                    direct = true;
                } else {
                    /* Indirect branch.  */
                    ea = load_ea(ctx, dstspec, ctx->cm, false, false);
                    t1 = ea.addr;
                    dest = 0;
                    direct = false;
                }

                srcspec &= 7;
                ea = load_ea(ctx, 046, ctx->cm, false, false);
                t2 = src_reg(ctx, srcspec);
                tcg_gen_qemu_st_i32(t2, ea.addr, ea.midx, MO_LEUW);
                tcg_gen_movi_i32(t2, ctx->pc);

                if (direct) {
                    gen_branch(ctx, dest);
                } else {
                    gen_ibranch(ctx, t1);
                }
            }
            return;

        case 050: /* CLR */
            tcg_gen_movi_i32(cpu_psw_n, 0);
            tcg_gen_movi_i32(cpu_psw_z, 1);
            tcg_gen_movi_i32(cpu_psw_v, 0);
            tcg_gen_movi_i32(cpu_psw_c, 0);
            if (dstspec <= 7) {
                tcg_gen_movi_i32(dest_reg(ctx, dstspec), 0);
            } else {
                ea = load_ea(ctx, dstspec, ctx->cm, false, true);
                t1 = tcg_const_i32(0);
                store_operand(ctx, dstspec, false, t1);
                tcg_temp_free (t1);
            }
            return;

        case 051: /* COM */
            t1 = load_operand(ctx, dstspec, false, true);
            tcg_gen_xori_i32(t1, t1, 0xffff);
            tcg_gen_mov_i32(cpu_psw_n, t1);
            tcg_gen_mov_i32(cpu_psw_z, t1);
            tcg_gen_movi_i32(cpu_psw_v, 0);
            tcg_gen_movi_i32(cpu_psw_c, 1);
            store_operand(ctx, dstspec, false, t1);
            return;

        case 052: /* INC */
            t1 = load_operand(ctx, dstspec, false, true);
            t2 = tcg_const_i32(1);
            gen_addsub_w(ctx, t1, t2, true, false, true);
            tcg_temp_free(t2);
            store_operand(ctx, dstspec, false, t1);
            return;

        case 053: /* DEC */
            t1 = load_operand(ctx, dstspec, false, true);
            t2 = tcg_const_i32(0xffff);
            gen_addsub_w(ctx, t1, t2, true, false, true);
            tcg_temp_free(t2);
            store_operand(ctx, dstspec, false, t1);
            return;

        case 054: /* NEG */
            t1 = load_operand(ctx, dstspec, false, true);
            t2 = tcg_const_i32(0);
            gen_addsub_w(ctx, t2, t1, false, true, false);
            tcg_temp_free(t2);
            store_operand(ctx, dstspec, false, t1);
            return;

        case 055: /* ADC */
            t1 = load_operand(ctx, dstspec, false, true);
            gen_addsub_w(ctx, t1, cpu_psw_c, true, true, false);
            store_operand(ctx, dstspec, false, t1);
            return;

        case 056: /* SBC */
            t1 = load_operand(ctx, dstspec, false, true);
            gen_addsub_w(ctx, t1, cpu_psw_c, false, true, false);
            store_operand(ctx, dstspec, false, t1);
            return;

        case 057: /* TST */
            t1 = load_operand(ctx, dstspec, false, false);
            gen_psw_logic_w(ctx, t1);
            tcg_gen_movi_i32(cpu_psw_c, 0);
            return;

        case 060: /* ROR */
            t1 = load_operand(ctx, dstspec, false, true);
            t2 = tcg_temp_new();
            tcg_gen_shli_i32(t2, cpu_psw_c, 15);
            tcg_gen_andi_i32(cpu_psw_c, t1, 1);
            tcg_gen_shri_i32(t1, t1, 1);
            tcg_gen_or_i32(t1, t1, t2);
            tcg_temp_free(t2);
            gen_psw_shift_w(ctx, t1);
            store_operand(ctx, dstspec, false, t1);
            return;

        case 061: /* ROL */
            t1 = load_operand(ctx, dstspec, false, true);
            tcg_gen_shli_i32(t1, t1, 1);
            tcg_gen_or_i32(t1, t1, cpu_psw_c);
            tcg_gen_shri_i32(cpu_psw_c, t1, 16);
            tcg_gen_ext16u_i32(t1, t1);
            gen_psw_shift_w(ctx, t1);
            store_operand(ctx, dstspec, false, t1);
            return;

        case 062: /* ASR */
            t1 = load_operand(ctx, dstspec, false, true);
            tcg_gen_andi_i32(cpu_psw_c, t1, 1);
            tcg_gen_ext16s_i32(t1, t1);
            tcg_gen_sari_i32(t1, t1, 1);
            tcg_gen_ext16u_i32(t1, t1);
            gen_psw_shift_w(ctx, t1);
            store_operand(ctx, dstspec, false, t1);
            return;

        case 063: /* ASL */
            t1 = load_operand(ctx, dstspec, false, true);
            tcg_gen_shri_i32(cpu_psw_c, t1, 15);
            tcg_gen_shri_i32(t1, t1, 1);
            tcg_gen_ext16u_i32(t1, t1);
            gen_psw_shift_w(ctx, t1);
            store_operand(ctx, dstspec, false, t1);
            return;

        case 064: /* MARK */
            bofs = (ctx->pc + dstspec * 2) & 0xffff;
            t1 = dest_reg(ctx, 5);
            gen_ibranch(ctx, t1);
            t2 = tcg_const_i32(bofs);
            tcg_gen_qemu_ld_i32(t1, t2, ctx->cm, MO_LEUW);
            tcg_temp_free(t2);
            tcg_gen_movi_i32(dest_reg(ctx, 6), (bofs + 2) & 0xffff);
            return;

        case 065: /* MFPI */
            gen_mfp(ctx, dstspec, true);
            return;
        case 066: /* MTPI */
            gen_mtp(ctx, dstspec, true);
            return;

        case 067: /* SXT */
            tcg_gen_ext16s_i32(cpu_psw_z, cpu_psw_n);
            tcg_gen_movi_i32(cpu_psw_v, 0);
            tcg_gen_sari_i32(cpu_psw_z, cpu_psw_z, 15);
            if (dstspec > 7) {
                ea = load_ea(ctx, dstspec, ctx->cm, false, true);
            }
            store_operand(ctx, dstspec, false, cpu_psw_z);
            return;

        case 070: /* CSM */
        case 072: /* TSTSET */
        case 073: /* WRTLCK */
            goto do_illegal;
        }
        goto do_illegal;

    case 001: /* MOV */
        t1 = load_operand(ctx, srcspec, false, false);
        if (dstspec > 7) {
            ea = load_ea(ctx, dstspec, ctx->cm, false, true);
        }
        gen_psw_logic_w(ctx, t1);
        store_operand(ctx, dstspec, false, t1);
        return;

    case 002: /* CMP */
        t2 = load_operand(ctx, srcspec, false, false);
        t1 = load_operand(ctx, dstspec, false, false);
        gen_addsub_w(ctx, t2, t1, false, true, dstspec == 027);
        return;

    case 003: /* BIT */
        t2 = load_operand(ctx, srcspec, false, false);
        t1 = load_operand(ctx, dstspec, false, false);
        tcg_gen_and_i32(t1, t1, t2);
        gen_psw_logic_w(ctx, t1);
        return;

    case 004: /* BIC */
        t2 = load_operand(ctx, srcspec, false, false);
        t1 = load_operand(ctx, dstspec, false, true);
        tcg_gen_andc_i32(t1, t1, t2);
        gen_psw_logic_w(ctx, t1);
        store_operand(ctx, dstspec, false, t1);
        return;

    case 005: /* BIS */
        t2 = load_operand(ctx, srcspec, false, false);
        t1 = load_operand(ctx, dstspec, false, true);
        tcg_gen_or_i32(t1, t1, t2);
        gen_psw_logic_w(ctx, t1);
        store_operand(ctx, dstspec, false, t1);
        return;

    case 006: /* ADD */
        t2 = load_operand(ctx, srcspec, false, false);
        t1 = load_operand(ctx, dstspec, false, true);
        gen_addsub_w(ctx, t1, t2, true, true, srcspec == 027);
        store_operand(ctx, dstspec, false, t1);
        return;

    case 007: /* EIS, FIS, CIS */
        srcspec &= 7;
        switch ((insn >> 9) & 7) {
        case 0: /* MUL */
            t2 = load_operand(ctx, dstspec, false, false);
            t1 = load_operand(ctx, srcspec, false, false);
            tcg_gen_ext16s_i32(t2, t2);
            tcg_gen_ext16s_i32(t1, t1);
            tcg_gen_mul_i32(t1, t1, t2);
            tcg_gen_sari_i32(cpu_psw_n, t1, 31);
            tcg_gen_mov_i32(cpu_psw_z, t1);
            tcg_gen_ext16s_i32(cpu_psw_c, t1);
            tcg_gen_movi_i32(cpu_psw_v, 0);
            tcg_gen_setcond_i32(TCG_COND_NE, cpu_psw_c, cpu_psw_c, t1);
            store_pair(ctx, srcspec, t1);
            return;

        case 1: /* DIV */
            t2 = load_operand(ctx, dstspec, false, false);
            /* Note that on overflow, the srcspec destination is unchanged.
               Thus we simply let the helper do all of the manipulation of
               the srcspec registers.  */
            tcg_gen_discard_i32(cpu_psw_n);
            tcg_gen_discard_i32(cpu_psw_z);
            tcg_gen_discard_i32(cpu_psw_v);
            tcg_gen_discard_i32(cpu_psw_c);
            t1 = tcg_const_i32(srcspec);
            if (unlikely(srcspec >= 6)) {
                /* OMG.  A division with a conditional store into PC.
                   Update env->pc now so the helper can overwrite it;
                   unconditionally end the TB.  */
                tcg_gen_movi_i32(cpu_pc, ctx->pc);
                ctx->status = EXIT_PC_UPDATED;
            }
            gen_helper_div(cpu_env, t1, t2);
            tcg_temp_free(t1);
            return;

        case 2: /* ASH */
            if (dstspec == 027) {
                int imm = next_code_word(ctx);
                t1 = load_operand(ctx, srcspec, false, false);
                gen_ashi(ctx, t1, imm);
            } else {
                t2 = load_operand(ctx, dstspec, false, false);
                t1 = load_operand(ctx, srcspec, false, false);
                gen_ash(ctx, t1, t2);
            }
            store_operand(ctx, srcspec, false, t1);
            return;

        case 3: /* ASHC */
            if (dstspec == 027) {
                int imm = next_code_word(ctx);
                t1 = load_pair(ctx, srcspec);
                gen_ashci(ctx, t1, imm);
            } else {
                t2 = load_operand(ctx, dstspec, false, false);
                t1 = load_pair(ctx, srcspec);
                gen_ashc(ctx, t1, t2);
            }
            store_pair(ctx, srcspec, t1);
            return;

        case 4: /* XOR */
            t2 = load_operand(ctx, srcspec, false, false);
            t1 = load_operand(ctx, dstspec, false, true);
            tcg_gen_xor_i32(t1, t1, t2);
            gen_psw_logic_w(ctx, t1);
            store_operand(ctx, dstspec, false, t1);
            return;

        case 5: /* FIS */
        case 6: /* CIS */
            goto do_illegal;

        case 7: /* SOB */
            t1 = load_operand(ctx, srcspec, false, false);
            tcg_gen_subi_i32(t1, t1, 1);
            tcg_gen_ext16u_i32(t1, t1);
            store_operand(ctx, srcspec, false, t1);
            gen_cbranch(ctx, dstspec * 2, TCG_COND_NE, t1);
            return;
        }
        goto do_illegal;

    case 010:
        switch (srcspec) {
        case 000 ... 003: /* BPL */
            t1 = alloc_tmp(ctx);
            tcg_gen_andi_i32(t1, cpu_psw_n, 0x8000);
            gen_cbranch(ctx, bofs, TCG_COND_EQ, t1);
            return;

        case 004 ... 007: /* BMI */
            t1 = alloc_tmp(ctx);
            tcg_gen_andi_i32(t1, cpu_psw_n, 0x8000);
            gen_cbranch(ctx, bofs, TCG_COND_NE, t1);
            return;

        case 010 ... 013: /* BHI */
            t1 = alloc_tmp(ctx);
            tcg_gen_or_i32(t1, cpu_psw_z, cpu_psw_c);
            gen_cbranch(ctx, bofs, TCG_COND_EQ, t1);
            return;

        case 014 ... 017: /* BLOS */
            t1 = alloc_tmp(ctx);
            tcg_gen_or_i32(t1, cpu_psw_z, cpu_psw_c);
            gen_cbranch(ctx, bofs, TCG_COND_NE, t1);
            return;

        case 020 ... 023: /* BVC */
            t1 = alloc_tmp(ctx);
            tcg_gen_andi_i32(t1, cpu_psw_v, 0x8000);
            gen_cbranch(ctx, bofs, TCG_COND_EQ, t1);
            return;

        case 024 ... 027: /* BVS */
            t1 = alloc_tmp(ctx);
            tcg_gen_andi_i32(t1, cpu_psw_v, 0x8000);
            gen_cbranch(ctx, bofs, TCG_COND_NE, t1);
            return;

        case 030 ... 033: /* BCC */
            gen_cbranch(ctx, bofs, TCG_COND_EQ, cpu_psw_c);
            return;

        case 034 ... 037: /* BCS */
            gen_cbranch(ctx, bofs, TCG_COND_NE, cpu_psw_c);
            return;

        case 040 ... 043: /* EMT */
            gen_excp(ctx, EXCP_EMT);
            return;
        case 044 ... 047: /* TRAP */
            gen_excp(ctx, EXCP_TRAP);
            return;

        case 050: /* CLRB */
            tcg_gen_movi_i32(cpu_psw_n, 0);
            tcg_gen_movi_i32(cpu_psw_z, 1);
            tcg_gen_movi_i32(cpu_psw_v, 0);
            tcg_gen_movi_i32(cpu_psw_c, 0);
            if (dstspec <= 7) {
                tcg_gen_andi_i32(dest_reg(ctx, dstspec),
                                 src_reg(ctx, dstspec), 0xff00);
            } else {
                ea = load_ea(ctx, dstspec, ctx->cm, true, true);
                t1 = tcg_const_i32(0);
                store_operand(ctx, dstspec, true, t1);
                tcg_temp_free (t1);
            }
            return;

        case 051: /* COMB */
            t1 = load_operand(ctx, dstspec, true, true);
            tcg_gen_not_i32(t1, t1);
            tcg_gen_mov_i32(cpu_psw_n, t1);
            tcg_gen_ext8u_i32(cpu_psw_z, t1);
            tcg_gen_movi_i32(cpu_psw_v, 0);
            tcg_gen_movi_i32(cpu_psw_c, 1);
            store_operand(ctx, dstspec, true, t1);
            return;

        case 052: /* INCB */
            t1 = load_operand(ctx, dstspec, true, true);
            t2 = tcg_const_i32(1);
            gen_addsub_b(ctx, t1, t2, true, false, true);
            tcg_temp_free(t2);
            store_operand(ctx, dstspec, true, t1);
            return;

        case 053: /* DECB */
            t1 = load_operand(ctx, dstspec, true, true);
            t2 = tcg_const_i32(-1);
            gen_addsub_b(ctx, t1, t2, true, false, true);
            tcg_temp_free(t2);
            store_operand(ctx, dstspec, true, t1);
            return;

        case 054: /* NEGB */
            t1 = load_operand(ctx, dstspec, true, true);
            t2 = tcg_const_i32(0);
            gen_addsub_b(ctx, t2, t1, false, true, false);
            tcg_temp_free(t2);
            store_operand(ctx, dstspec, true, t1);
            return;

        case 055: /* ADCB */
            t1 = load_operand(ctx, dstspec, true, true);
            gen_addsub_b(ctx, t1, cpu_psw_c, true, true, false);
            store_operand(ctx, dstspec, true, t1);
            return;

        case 056: /* SBCB */
            t1 = load_operand(ctx, dstspec, true, true);
            gen_addsub_b(ctx, t1, cpu_psw_c, false, true, false);
            store_operand(ctx, dstspec, true, t1);
            return;

        case 057: /* TST */
            t1 = load_operand(ctx, dstspec, true, false);
            gen_psw_logic_b(ctx, t1);
            tcg_gen_movi_i32(cpu_psw_c, 0);
            return;

        case 060: /* RORB */
            t1 = load_operand(ctx, dstspec, true, true);
            tcg_gen_deposit_i32(t1, t1, cpu_psw_c, 8, 24);
            tcg_gen_andi_i32(cpu_psw_c, t1, 1);
            tcg_gen_shri_i32(t1, t1, 1);
            gen_psw_shift_b(ctx, t1);
            store_operand(ctx, dstspec, true, t1);
            return;

        case 061: /* ROLB */
            t1 = load_operand(ctx, dstspec, true, true);
            tcg_gen_shli_i32(t1, t1, 1);
            tcg_gen_or_i32(t1, t1, cpu_psw_c);
            tcg_gen_shri_i32(cpu_psw_c, t1, 31);
            gen_psw_shift_b(ctx, t1);
            store_operand(ctx, dstspec, true, t1);
            return;

        case 062: /* ASRB */
            t1 = load_operand(ctx, dstspec, true, true);
            tcg_gen_andi_i32(cpu_psw_c, t1, 1);
            tcg_gen_sari_i32(t1, t1, 1);
            gen_psw_shift_b(ctx, t1);
            store_operand(ctx, dstspec, true, t1);
            return;

        case 063: /* ASLB */
            t1 = load_operand(ctx, dstspec, true, true);
            tcg_gen_shri_i32(cpu_psw_c, t1, 31);
            tcg_gen_shri_i32(t1, t1, 1);
            gen_psw_shift_b(ctx, t1);
            store_operand(ctx, dstspec, true, t1);
            return;

        case 064: /* MTPS */
            t1 = load_operand(ctx, dstspec, true, false);
            tcg_gen_shli_i32(cpu_psw_n, t1, 15 - PSW_V_N);
            tcg_gen_not_i32(cpu_psw_z, t1);
            tcg_gen_shli_i32(cpu_psw_v, t1, 15 - PSW_V_V);
            tcg_gen_andi_i32(cpu_psw_z, cpu_psw_z, 1 << PSW_V_Z);
            tcg_gen_andi_i32(cpu_psw_c, t1, 1);
            if (ctx->cm == MMU_KERN_D_IDX) {
                gen_spl(ctx, t1);
            }
            return;

        case 065: /* MFPD */
            gen_mfp(ctx, dstspec, false);
            return;
        case 066: /* MTPD */
            gen_mtp(ctx, dstspec, false);
            return;

        case 067: /* MFPS */
            t1 = tcg_temp_new();
            gen_helper_get_psw(t1, cpu_env);
            tcg_gen_ext8s_i32(t1, t1);
            gen_psw_logic_b(ctx, t1);
            if (dstspec > 7) {
                ea = load_ea(ctx, dstspec, ctx->cm, true, true);
            }
            store_operand(ctx, dstspec, true, t1);
            return;
        }
        goto do_illegal;

    case 011: /* MOVB */
        t1 = load_operand(ctx, srcspec, true, false);
        if (dstspec <= 7) {
            tcg_gen_ext16u_i32(t1, t1);
            gen_psw_logic_w(ctx, t1);
            store_operand(ctx, dstspec, false, t1);
        } else {
            ea = load_ea(ctx, dstspec, ctx->cm, false, true);
            gen_psw_logic_b(ctx, t1);
            store_operand(ctx, dstspec, true, t1);
        }
        return;

    case 012: /* CMPB */
        t2 = load_operand(ctx, srcspec, true, false);
        t1 = load_operand(ctx, dstspec, true, false);
        gen_addsub_b(ctx, t1, t2, false, true, srcspec == 027);
        return;

    case 013: /* BITB */
        t2 = load_operand(ctx, srcspec, true, false);
        t1 = load_operand(ctx, dstspec, true, false);
        tcg_gen_and_i32(t1, t1, t2);
        gen_psw_logic_b(ctx, t1);
        return;

    case 014: /* BICB */
        t2 = load_operand(ctx, srcspec, true, false);
        t1 = load_operand(ctx, dstspec, true, true);
        tcg_gen_andc_i32(t1, t1, t2);
        gen_psw_logic_b(ctx, t1);
        store_operand(ctx, dstspec, true, t1);
        return;

    case 015: /* BISB */
        t2 = load_operand(ctx, srcspec, true, false);
        t1 = load_operand(ctx, dstspec, true, true);
        tcg_gen_or_i32(t1, t1, t2);
        gen_psw_logic_b(ctx, t1);
        store_operand(ctx, dstspec, true, t1);
        return;

    case 016: /* SUB */
        t2 = load_operand(ctx, srcspec, false, false);
        t1 = load_operand(ctx, dstspec, false, true);
        gen_addsub_w(ctx, t1, t2, false, true, srcspec == 027);
        store_operand(ctx, dstspec, false, t1);
        return;

    case 017: /* FPP */
        goto do_illegal;
    }

    /* We should have returned or signaled illegal by now.  */
    g_assert_not_reached();

 do_illegal:
    gen_excp(ctx, EXCP_ILL);
}

void
gen_intermediate_code(CPUPDP11State *env, struct TranslationBlock *tb)
{
    PDP11CPU *cpu = pdp11_env_get_cpu(env);
    CPUState *cs = CPU(cpu);
    DisasContext ctx;
    target_ulong pc_start;
    int i, num_insns, max_insns;
    ExitStatus ret;

    pc_start = tb->pc;

    ctx.tb = tb;
    ctx.env = env;
    ctx.pc = pc_start;
    ctx.singlestep_enabled = cs->singlestep_enabled;

    i = tb->flags;
    ctx.rs = (i & PSW_RS) >> PSW_V_RS;
    ctx.cm = (i & PSW_CM) >> PSW_V_CM;
    ctx.pm = (i & PSW_PM) >> PSW_V_PM;

    for (i = 0; i < ARRAY_SIZE(ctx.tmp); ++i) {
        TCGV_UNUSED_I32(ctx.tmp[i]);
    }
    ctx.ntmp = 0;
    ctx.status = NO_EXIT;

    num_insns = 0;
    max_insns = tb->cflags & CF_COUNT_MASK;
    if (max_insns == 0) {
        max_insns = CF_COUNT_MASK;
    }
    if (max_insns > TCG_MAX_INSNS) {
        max_insns = TCG_MAX_INSNS;
    }

    gen_tb_start(tb);
    do {
        target_ulong this_pc = ctx.pc;
        tcg_gen_insn_start(this_pc);
        num_insns++;

        if (unlikely(cpu_breakpoint_test(cs, this_pc, BP_ANY))) {
            gen_excp(&ctx, EXCP_DEBUG);
            ret = EXIT_NORETURN;
            ctx.pc += 2;
            break;
        }
        if (num_insns == max_insns && (tb->cflags & CF_LAST_IO)) {
            gen_io_start();
        }

        tcg_clear_temp_count();
        TCGV_UNUSED_I32(ctx.dest_ea.addr);

        translate_one(&ctx);

        i = ctx.ntmp;
        if (i != 0) {
            for (; --i >= 0;) {
                if (!TCGV_IS_UNUSED_I32(ctx.tmp[i])) {
                    tcg_temp_free_i32(ctx.tmp[i]);
                    TCGV_UNUSED_I32(ctx.tmp[i]);
                }
            }
            ctx.ntmp = 0;
        }

        if (tcg_check_temp_count()) {
            qemu_log("Leak detected at %08x\n", this_pc);
        }

        ret = ctx.status;
        if (ret == NO_EXIT
            && (ctx.pc - pc_start >= TARGET_PAGE_SIZE - 12
                || tcg_op_buf_full()
                || num_insns >= max_insns
                || singlestep
                || ctx.singlestep_enabled)) {
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
        tcg_gen_movi_i32(cpu_pc, ctx.pc);
        /* FALLTHRU */
    case EXIT_PC_UPDATED:
        if (ctx.singlestep_enabled) {
            gen_excp_nopc(&ctx, EXCP_DEBUG);
        } else {
            tcg_gen_exit_tb(0);
        }
        break;
    default:
        tcg_abort();
    }

    gen_tb_end(tb, num_insns);
    tb->size = ctx.pc - pc_start;
    tb->icount = num_insns;
}

void
restore_state_to_opc(CPUPDP11State *env, TranslationBlock *tb,
                     target_ulong *data)
{
    env->pc = data[0];
}

void pdp11_tcg_init(void)
{
    static const char reg_names[2][6][4] = {
        { "r0", "r1", "r2", "r3", "r4", "r5" },
        { "a0", "a1", "a2", "a3", "a4", "a5" },
    };
    static const char sp_names[6][8] = {
        "sp_ker", "sp_sup", "sp_2", "sp_usr"
    };
    static bool done;
    int i, rs;

    if (done) {
        return;
    }
    done = true;

    cpu_env = tcg_global_reg_new_ptr(TCG_AREG0, "env");

    for (rs = 0; rs < 2; ++rs) {
        for (i = 0; i < 6 ; ++i) {
            cpu_reg[rs][i]
                = tcg_global_mem_new(cpu_env,
                                     offsetof(CPUPDP11State, reg[rs][i]),
                                     reg_names[rs][i]);
        }
    }
    for (i = 0; i < 4; ++i) {
        cpu_sp[i] = tcg_global_mem_new(cpu_env,
                                       offsetof(CPUPDP11State, sp[i]),
                                       sp_names[i]);
    }

    cpu_pc = tcg_global_mem_new(cpu_env, offsetof(CPUPDP11State, pc), "pc");
    cpu_psw_c = tcg_global_mem_new(cpu_env,
                                   offsetof(CPUPDP11State, psw_c), "C");
    cpu_psw_v = tcg_global_mem_new(cpu_env,
                                   offsetof(CPUPDP11State, psw_v), "V");
    cpu_psw_z = tcg_global_mem_new(cpu_env,
                                   offsetof(CPUPDP11State, psw_z), "Z");
    cpu_psw_n = tcg_global_mem_new(cpu_env,
                                   offsetof(CPUPDP11State, psw_n), "N");
}
