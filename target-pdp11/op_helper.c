/*
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
#include "exec/helper-proto.h"
#include "exec/cpu_ldst.h"


void cpu_set_psw(CPUPDP11State *env, uint32_t val)
{
    env->psw_c = (val >> PSW_V_C) & 1;
    env->psw_v = -((val >> PSW_V_V) & 1);
    env->psw_z = (~val >> PSW_V_Z) & 1;
    env->psw_n = -((val >> PSW_V_N) & 1);

    val = val & (PSW_TBIT | PSW_IPL | PSW_FPD | PSW_RS | PSW_PM | PSW_CM);
    env->psw = val;
}

void helper_set_psw(CPUPDP11State *env, uint32_t val)
{
    cpu_set_psw(env, val);
}

void helper_set_psw_prot(CPUPDP11State *env, uint32_t val)
{
    /* Not allowed to lower privledges or change IPL.  */
    val &= ~PSW_IPL;
    val |= env->psw & (PSW_RS | PSW_PM | PSW_CM | PSW_IPL);
    cpu_set_psw(env, val);
}

uint32_t cpu_get_psw(CPUPDP11State *env)
{
    uint32_t val = env->psw;

    val |= env->psw_c << PSW_V_C;
    val |= ((env->psw_v >> 15) & 1) << PSW_V_V;
    val |= (env->psw_z == 0) << PSW_V_Z;
    val |= ((env->psw_n >> 15) & 1) << PSW_V_N;

    return val;
}

uint32_t helper_get_psw(CPUPDP11State *env)
{
    return cpu_get_psw(env);
}

void helper_div(CPUPDP11State *env, uint32_t regno, int32_t src2)
{
    int32_t src, dst;
    uint32_t *r1, *r2;
    uint32_t N, Z, V, C;

    if (likely(regno < 6)) {
        uint32_t rs = (env->psw & PSW_RS) >> PSW_V_RS;
        r1 = &env->reg[rs][regno];
        r2 = &env->reg[rs][regno | 1];
    } else {
        /* OMG, what were you thinking... */
        r2 = &env->pc;
        if (regno == 6) {
            uint32_t cm = (env->psw & PSW_CM) >> PSW_V_CM;
            r1 = &env->sp[cm];
        } else {
            r1 = r2;
        }
    }

    src = (*r1 << 16) | *r2;
    src2 = (int16_t)src2;

    V = -1;
    if (src2 == 0) {
        N = 0, Z = 0, C = 1;
    } else {
        C = 0;
        if (src == INT32_MIN && src2 == -1) {
            N = 0, Z = 1;
        } else {
            Z = dst = src / src2;
            N = -(dst < 0);
            if (dst == (int16_t)dst) {
                V = 0;
                *r1 = dst & 0xffff;
                *r2 = (src - src2 * dst) & 0xffff;
            }
        }
    }
    env->psw_n = N;
    env->psw_z = Z;
    env->psw_v = V;
    env->psw_c = C;
}

void QEMU_NORETURN helper_excp(CPUPDP11State *env, uint32_t code)
{
    PDP11CPU *cpu = pdp11_env_get_cpu(env);
    CPUState *cs = CPU(cpu);

    cs->exception_index = code;
    env->error_code = 0;
    cpu_loop_exit(cs);
}
