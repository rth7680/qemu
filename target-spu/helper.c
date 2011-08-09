/*
 *  Synergistic Processor Unit (SPU) emulation
 *  Non-opcode helper functions.
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

#include "cpu.h"

int cpu_spu_handle_mmu_fault (CPUState *env, target_ulong address, int rw,
                              int mmu_idx, int is_softmmu)
{
#ifndef CONFIG_USER_ONLY
    address &= TARGET_PAGE_MASK;
    tlb_set_page(env, address, address, PAGE_READ | PAGE_WRITE | PAGE_EXEC,
                 mmu_idx, TARGET_PAGE_SIZE);
#endif
    return 0;
}

target_phys_addr_t cpu_get_phys_page_debug(CPUState *env, target_ulong addr)
{
    return addr;
}

void do_interrupt (CPUState *env)
{
    env->exception_index = -1;
    env->srr0 = env->pc;
    env->pc = 0;
}

void cpu_dump_state (CPUState *env, FILE *f, fprintf_function cpu_fprintf,
                     int flags)
{
    int i;

    cpu_fprintf(f, "  PC %08x\n", env->pc);
    for (i = 0; i < 128; ++i) {
        cpu_fprintf(f, "$%03d %08x %08x %08x %08x%s",
                    i, env->gpr[i*4 + 0], env->gpr[i*4 + 1],
                    env->gpr[i*4 + 2], env->gpr[i*4 + 3],
                    i & 1 ? "  " : "\n");
    }
}

/* ??? Silliness wrt monitor.c.  */
void irq_info(void);
void pic_info(void);
void irq_info(void) { }
void pic_info(void) { }
