/*
 *  Synergistic Processor Unit (SPU) emulation
 *  CPU definitions for qemu.
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

#ifndef QEMU_SPU_CPU_H
#define QEMU_SPU_CPU_H 1

#define TARGET_LONG_BITS 32

/* The SPU does not have virtual memory, but match the PowerPC host.  */
#define TARGET_PAGE_BITS 12

#define TARGET_PHYS_ADDR_SPACE_BITS	32
#define TARGET_VIRT_ADDR_SPACE_BITS	32

#ifndef EM_SPU
#define EM_SPU  23  /* Sony/Toshiba/IBM SPU */
#endif
#define ELF_MACHINE  EM_SPU

#define CPUState struct CPUSPUState

#include "cpu-defs.h"
#include "softfloat.h"

/* The one "mode" is physical addressing.  */
#define NB_MMU_MODES 1

typedef struct CPUSPUState {
    uint32_t gpr[128*4];
    uint32_t pc;
    uint32_t srr0;
    uint32_t inte;
    uint32_t lslr;
#ifndef CONFIG_USER_ONLY
    void *memory_base;
#endif

    /* Those resources are used only in Qemu core.  */
    CPU_COMMON
    int error_code;
} CPUSPUState;

#include "cpu-all.h"

enum {
    EXCP_RESET,
    EXCP_HALT,
    EXCP_STOP,
    EXCP_ILLOPC,
    EXCP_MMFAULT,
    EXCP_RDCH,
    EXCP_WRCH,
};

extern CPUSPUState * cpu_spu_init (const char *cpu_model);
#define cpu_init cpu_spu_init

extern int cpu_spu_exec(CPUSPUState *s);
#define cpu_exec cpu_spu_exec

extern int cpu_spu_signal_handler(int host_signum, void *pinfo, void *puc);
#define cpu_signal_handler cpu_spu_signal_handler

extern int cpu_spu_handle_mmu_fault (CPUSPUState *env, uint32_t address,
				     int rw, int mmu_idx, int is_softmmu);
#define cpu_handle_mmu_fault cpu_spu_handle_mmu_fault

void do_interrupt (CPUState *env);

#define cpu_gen_code cpu_spu_gen_code

static inline void cpu_get_tb_cpu_state(CPUState *env, target_ulong *pc,
                                        target_ulong *cs_base, int *pflags)
{
    *pc = env->pc;
    *cs_base = 0;
    *pflags = 0;
}

static inline bool cpu_has_work(CPUState *env)
{
    return 0;
}

static inline int cpu_mmu_index(CPUState *env)
{
    return 0;
}

#include "exec-all.h"

static inline void cpu_pc_from_tb(CPUState *env, TranslationBlock *tb)
{
    env->pc = tb->pc;
}

#endif
