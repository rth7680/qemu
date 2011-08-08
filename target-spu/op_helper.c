/*
 *  Synergistic Processor Unit (SPU) emulation
 *  Opcode helper functions.
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
#include "dyngen-exec.h"
#include "host-utils.h"
#include "softfloat.h"
#include "helper.h"

/* This should only be called from translate, via gen_excp.
   We expect that ENV->PC has already been updated.  */
void QEMU_NORETURN helper_excp(int excp, int error)
{
    env->exception_index = excp;
    env->error_code = error;
    cpu_loop_exit(env);
}

uint32_t helper_clz(uint32_t arg)
{
    return clz32(arg);
}

uint32_t helper_cntb(uint32_t val)
{
    /* Like ctpop32, but don't fold the bytes together.  */
    val = (val & 0x55555555) + ((val >> 1) & 0x55555555);
    val = (val & 0x33333333) + ((val >> 2) & 0x33333333);
    val = (val & 0x0f0f0f0f) + ((val >> 4) & 0x0f0f0f0f);
    return val;
}

uint32_t helper_fsmb(uint32_t arg)
{
    uint32_t ret = 0;
    ret |= (arg & 0x8000 ? 0xff000000 : 0);
    ret |= (arg & 0x4000 ? 0x00ff0000 : 0);
    ret |= (arg & 0x2000 ? 0x0000ff00 : 0);
    ret |= (arg & 0x1000 ? 0x000000ff : 0);
    return ret;
}

uint32_t helper_fsmh(uint32_t arg)
{
    uint32_t ret = 0;
    ret |= (arg & 0x80 ? 0xffff0000 : 0);
    ret |= (arg & 0x40 ? 0x0000ffff : 0);
    return ret;
}

/*****************************************************************************/
/* Softmmu support */
#if !defined (CONFIG_USER_ONLY)
#include "softmmu_exec.h"

static void do_unaligned_access(target_ulong addr, int is_write,
                                int is_user, void *retaddr)
{
    /* SPU never has unaligned accesses; all loads explicitly
       drop the low bits.  */
    abort();
}

#define MMUSUFFIX _mmu
#define ALIGNED_ONLY

#define SHIFT 0
#include "softmmu_template.h"

#define SHIFT 1
#include "softmmu_template.h"

#define SHIFT 2
#include "softmmu_template.h"

#define SHIFT 3
#include "softmmu_template.h"

void tlb_fill (target_ulong addr, int is_write, int mmu_idx, void *retaddr)
{
    abort();
}
#endif
