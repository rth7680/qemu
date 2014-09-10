/*
 *  PDP/11 emulation for qemu: main CPU struct.
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
#ifndef PDP11_CPU_H
#define PDP11_CPU_H

#include "qemu-common.h"

#define TARGET_LONG_BITS 32
#define ALIGNED_ONLY

#define CPUArchState struct CPUPDP11State

#include "exec/cpu-defs.h"

#define ELF_MACHINE       EM_PDP11

/* Ouch.  If we could dynamically change the page size, and adjust the
   computation of the complete physical address to allow tlb_set_page
   to receive a non-aligned destination mapping (i.e. change the final
   address merge from or to add), then we could have a page size as
   high as 8k.  */
#define TARGET_PAGE_BITS  6

#define TARGET_VIRT_ADDR_SPACE_BITS 16
#define TARGET_PHYS_ADDR_SPACE_BITS 32  /* ... or less */

#define NB_MMU_MODES        6
#define MMU_KERN_D_IDX      0
#define MMU_KERN_I_IDX      1
#define MMU_SUPR_D_IDX      2
#define MMU_SUPR_I_IDX      3
#define MMU_USER_D_IDX      4
#define MMU_USER_I_IDX      5

typedef struct CPUPDP11State CPUPDP11State;
struct CPUPDP11State {
    /* Yes, this is a 16-bit target, but TCG doesn't support 16-bit types.  */
    uint32_t reg[2][6];  /* general (and alternate) register set */
    uint32_t sp[4];      /* kernel, supervisor, unused, user */
    uint32_t pc;

    /* The deconstructed Processor Status Word.  */
    uint32_t psw;    /* most of the psw */
    uint32_t psw_n;  /* negative bit (psw_n bit 15) */
    uint32_t psw_z;  /* zero bit (psw_z == 0) */
    uint32_t psw_v;  /* overflow bit (psw_v bit 15) */
    uint32_t psw_c;  /* carry bit (boolean) */

    /* Floating Point Registers */
    /* XXX */

    /* Memory Management Registers */
    /* XXX */

    /* QEMU */
    int error_code;

    CPU_COMMON
};

#include "exec/cpu-all.h"
#include "cpu-qom.h"

/* Bits of the Processor Status Word.  */
enum {
    PSW_V_C = 0,
    PSW_V_V = 1,
    PSW_V_Z = 2,
    PSW_V_N = 3,
    PSW_V_TBIT = 4,
    PSW_V_IPL = 5,
    PSW_V_FPD = 8,
    PSW_V_RS = 11,
    PSW_V_PM = 12,
    PSW_V_CM = 14,

    PSW_CC = 15u,
    PSW_TBIT = 1u << PSW_V_TBIT,
    PSW_FPD = 1u << PSW_V_FPD,
    PSW_IPL = 7u << PSW_V_IPL,
    PSW_RS = 1u << PSW_V_RS,
    PSW_PM = 3u << PSW_V_PM,
    PSW_CM = 3u << PSW_V_CM,
};

enum {
    EXCP_HALT,
    EXCP_RESET,
    EXCP_BPT,
    EXCP_IOT,
    EXCP_ILL,
    EXCP_EMT,
    EXCP_TRAP,
};

static inline int cpu_mmu_index(CPUPDP11State *env, bool ifetch)
{
    switch ((env->psw & PSW_PM) >> PSW_V_CM) {
    case 0:
        return MMU_KERN_D_IDX + ifetch;
    case 1:
        return MMU_SUPR_D_IDX + ifetch;
    default:
        return MMU_USER_D_IDX + ifetch;
    }
}

static inline void cpu_get_tb_cpu_state(CPUPDP11State *env, target_ulong *pc,
                                        target_ulong *cs_base, uint32_t *flags)
{
    *pc = env->pc;
    *cs_base = 0;
    *flags = env->psw & (PSW_TBIT | PSW_RS | PSW_PM | PSW_CM);
}

PDP11CPU *cpu_pdp11_init(const char *cpu_model);

static inline CPUPDP11State *cpu_init(const char *cpu_model)
{
    PDP11CPU *cpu = cpu_pdp11_init(cpu_model);
    return cpu ? &cpu->env : NULL;
}

#define cpu_exec cpu_pdp11_exec
#define cpu_signal_handler cpu_pdp11_signal_handler

int cpu_pdp11_exec(CPUState *s);
void cpu_set_psw(CPUPDP11State *s, uint32_t);
uint32_t cpu_get_psw(CPUPDP11State *s);

void pdp11_tcg_init(void);
int cpu_pdp11_signal_handler(int host_signum, void *pinfo, void *puc);

int cpu_pdp11_handle_mmu_fault(CPUState *cpu, target_ulong address,
                               int rw, int mmu_idx);
#define cpu_handle_mmu_fault cpu_pdp11_handle_mmu_fault

#include "exec/exec-all.h"

#endif /* PDP11_CPU_H */
