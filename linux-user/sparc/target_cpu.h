/*
 * SPARC specific CPU ABI and functions for linux-user
 *
 * Copyright (C) 2003 Thomas M. Ogrisegg <tom@fnord.at>
 * Copyright (C) 2003-2005 Fabrice Bellard
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
#ifndef SPARC_TARGET_CPU_H
#define SPARC_TARGET_CPU_H

static inline void cpu_clone_regs_child(CPUSPARCState *env, target_ulong newsp)
{
    /*
     * After cpu_copy, env->regwptr is pointing into old_env.
     * Update the new cpu to use its own register window.
     */
    env->regwptr = env->regbase + (env->cwp * 16);

    /* Set a new stack, if requested.  */
    if (newsp) {
        /* ??? The kernel appears to copy one stack frame to the new stack. */
        /* ??? The kernel force aligns the stack. */
        env->regwptr[WREG_SP] = newsp;
    }

    /*
     * Syscall return for clone child: %o0 = 0 and clear CF since
     * this counts as a success return value.  %o1 = 1 to indicate
     * this is the child.  Advance the PC past the syscall.
     */
    env->regwptr[WREG_O0] = 0;
#if defined(TARGET_SPARC64) && !defined(TARGET_ABI32)
    env->xcc &= ~PSR_CARRY;
#else
    env->psr &= ~PSR_CARRY;
#endif
    env->regwptr[WREG_O1] = 1;
    env->pc = env->npc;
    env->npc = env->npc + 4;
}

static inline void cpu_clone_regs_parent(CPUSPARCState *env)
{
    /* Set the second return value for the parent: %o1 = 0.  */
    env->regwptr[WREG_O1] = 0;
}

static inline void cpu_set_tls(CPUSPARCState *env, target_ulong newtls)
{
    env->gregs[7] = newtls;
}

static inline abi_ulong get_sp_from_cpustate(CPUSPARCState *state)
{
    return state->regwptr[WREG_SP];
}

#endif
