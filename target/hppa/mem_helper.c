/*
 *  HPPA memory access helper routines
 *
 *  Copyright (c) 2017 Helge Deller
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
#include "exec/exec-all.h"
#include "exec/helper-proto.h"
#include "qom/cpu.h"

#ifdef CONFIG_USER_ONLY
int hppa_cpu_handle_mmu_fault(CPUState *cs, vaddr address,
                              int size, int rw, int mmu_idx)
{
    HPPACPU *cpu = HPPA_CPU(cs);

    /* ??? Test between data page fault and data memory protection trap,
       which would affect si_code.  */
    cs->exception_index = EXCP_DMP;
    cpu->env.cr[CR_IOR] = address;
    return 1;
}
#else
static hppa_tlb_entry *hppa_find_tlb(CPUHPPAState *env, vaddr addr)
{
    int i;

    for (i = 0; i < ARRAY_SIZE(env->tlb); ++i) {
        hppa_tlb_entry *ent = &env->tlb[i];
        if (ent->va_b <= addr && addr <= ent->va_e && ent->entry_valid) {
            return ent;
        }
    }
    return NULL;
}

int hppa_get_physical_address(CPUHPPAState *env, vaddr addr, int mmu_idx,
                              int type, hwaddr *pphys, int *pprot)
{
    hwaddr phys;
    int prot, r_prot, w_prot, x_prot;
    hppa_tlb_entry *ent;
    int ret = -1;

    /* Virtual translation disabled.  Direct map virtual to physical.  */
    if (mmu_idx == MMU_PHYS_IDX) {
        phys = addr;
        prot = PAGE_READ | PAGE_WRITE | PAGE_EXEC;
        goto egress;
    }

    /* Find a valid tlb entry that matches the virtual address.  */
    ent = hppa_find_tlb(env, addr);
    if (ent == NULL) {
        phys = 0;
        prot = 0;
        ret = (type & PAGE_EXEC ? EXCP_ITLB_MISS : EXCP_DTLB_MISS);
        goto egress;
    }

    /* We now know the physical address.  */
    phys = ent->pa + (addr & ~TARGET_PAGE_MASK);

    /* Map TLB access_rights field to QEMU protection.  */
    r_prot = (mmu_idx <= ent->ar_pl1) * PROT_READ;
    w_prot = (mmu_idx <= ent->ar_pl2) * PROT_WRITE;
    x_prot = (ent->ar_pl2 <= mmu_idx && mmu_idx <= ent->ar_pl1) * PROT_EXEC;
    switch (ent->ar_type) {
    case 0: /* read-only: data page */
        prot = r_prot;
        break;
    case 1: /* read/write: dynamic data page */
        prot = r_prot | w_prot;
        break;
    case 2: /* read/execute: normal code page */
        prot = r_prot | x_prot;
        break;
    case 3: /* read/write/execute: dynamic code page */
        prot = r_prot | w_prot | x_prot;
        break;
    default: /* execute: promote to privilege level type & 3 */
        prot = x_prot;
    }

    /* ??? Check PSW_P and ent->access_prot.  This can remove PROT_WRITE.  */

    /* No guest access type indicates a non-architectural access from
       within QEMU.  Bypass checks for access, D, B and T bits.  */
    if (type == 0) {
        goto egress;
    }

    if (unlikely(!(prot & type))) {
        /* The access isn't allowed -- Inst/Data Memory Protection Fault.  */
        ret = (type & PAGE_EXEC ? EXCP_IMP : EXCP_DMP);
        goto egress;
    }

    /* In reverse priority order, check for conditions which raise faults.
       As we go, remove PROT bits that cover the condition we want to check.
       In this way, the resulting PROT will force a re-check of the
       architectural TLB entry for the next access.  */
    if (unlikely(!ent->d)) {
        if (type & PAGE_WRITE) {
            /* The D bit is not set -- TLB Dirty Bit Fault.  */
            ret = EXCP_TLB_DIRTY;
        }
        prot &= PROT_READ | PROT_EXEC;
    }
    if (unlikely(ent->b)) {
        if (type & PAGE_WRITE) {
            /* The B bit is set -- Data Memory Break Fault.  */
            ret = EXCP_DMB;
        }
        prot &= PROT_READ | PROT_EXEC;
    }
    if (unlikely(ent->t)) {
        if (!(type & PAGE_EXEC)) {
            /* The T bit is set -- Page Reference Fault.  */
            ret = EXCP_PAGE_REF;
        }
        prot &= PROT_EXEC;
    }

 egress:
    *pphys = phys;
    *pprot = prot;
    return ret;
}

hwaddr hppa_cpu_get_phys_page_debug(CPUState *cs, vaddr addr)
{
    HPPACPU *cpu = HPPA_CPU(cs);
    hwaddr phys;
    int prot, excp;

    /* If the (data) mmu is disabled, bypass translation.  */
    /* ??? We really ought to know if the code mmu is disabled too,
       in order to get the correct debugging dumps.  */
    if (!(cpu->env.psw & PSW_D)) {
        return addr;
    }

    excp = hppa_get_physical_address(&cpu->env, addr, MMU_KERNEL_IDX, 0,
                                     &phys, &prot);

    /* Since we're translating for debugging, the only error that is a
       hard error is no translation at all.  Otherwise, while a real cpu
       access might not have permission, the debugger does.  */
    return excp == EXCP_DTLB_MISS ? -1 : phys;
}

void tlb_fill(CPUState *cs, target_ulong addr, int size,
              MMUAccessType type, int mmu_idx, uintptr_t retaddr)
{
    HPPACPU *cpu = HPPA_CPU(cs);
    int prot, excp, a_prot;
    hwaddr phys;

    switch (type) {
    case MMU_INST_FETCH:
        a_prot = PROT_EXEC;
        break;
    case MMU_DATA_STORE:
        a_prot = PROT_WRITE;
        break;
    default:
        a_prot = PROT_READ;
        break;
    }

    excp = hppa_get_physical_address(&cpu->env, addr, mmu_idx,
                                     a_prot, &phys, &prot);
    if (unlikely(excp >= 0)) {
        /* Failure.  Raise the indicated exception.  */
        cs->exception_index = excp;
        if (cpu->env.psw & PSW_Q) {
            /* ??? Needs tweaking for hppa64.  */
            cpu->env.cr[CR_IOR] = addr;
            cpu->env.cr[CR_ISR] = addr >> 32;
        }
        cpu_loop_exit_restore(cs, retaddr);
    }

    /* Success!  Store the translation into the QEMU TLB.  */
    tlb_set_page(cs, addr & TARGET_PAGE_MASK, phys & TARGET_PAGE_MASK,
                 prot, mmu_idx, TARGET_PAGE_SIZE);
}
#endif /* CONFIG_USER_ONLY */
