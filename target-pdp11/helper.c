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
#include "sysemu/sysemu.h"


void pdp11_cpu_dump_state(CPUState *cs, FILE *f,
                          fprintf_function cpu_fprintf, int flags)
{
    PDP11CPU *cpu = PDP11_CPU(cs);
    CPUPDP11State *env = &cpu->env;
    int i, cm, pm, rs;

    i = cpu_get_psw(env);
    cm = (i & PSW_CM) >> PSW_V_CM;
    pm = (i & PSW_PM) >> PSW_V_PM;
    rs = (i & PSW_RS) >> PSW_V_RS;

    cpu_fprintf(f, "PSW=%04x CM=%d PM=%d RS=%d %c%c%c%c\n",
                i, cm, pm, rs,
                i & PSW_V_N ? 'N' : '-',
                i & PSW_V_Z ? 'Z' : '-',
                i & PSW_V_V ? 'V' : '-',
                i & PSW_V_C ? 'C' : '-');

    cpu_fprintf(f,
                "r0=%04x  r1=%04x  r2=%04x  r3=%04x\n"
                "r4=%04x  r5=%04x  sp=%04x  pc=%04x\n",
                env->reg[rs][0], env->reg[rs][1], env->reg[rs][2],
                env->reg[rs][3], env->reg[rs][4], env->reg[rs][5],
                env->sp[cm], env->pc);
}

void tlb_fill(CPUState *cs, target_ulong address, int is_write,
              int mmu_idx, uintptr_t retaddr)
{
    /* TODO */
    tlb_set_page(cs, address & TARGET_PAGE_MASK,
                 address & 0xffff & TARGET_PAGE_MASK,
                 PAGE_READ | PAGE_WRITE | (mmu_idx & 1) * PAGE_EXEC,
                 mmu_idx, TARGET_PAGE_SIZE);
}

void pdp11_cpu_do_interrupt(CPUState *cs)
{
    PDP11CPU *cpu = PDP11_CPU(cs);
    CPUPDP11State *env = &cpu->env;
    int i = cs->exception_index;

    if (qemu_loglevel_mask(CPU_LOG_INT)) {
        static const char * const names[] = {
            [EXCP_HALT] = "halt",
            [EXCP_RESET] = "reset",
            [EXCP_BPT] = "bpt",
            [EXCP_IOT] = "iot",
            [EXCP_ILL] = "ill",
            [EXCP_EMT] = "emt",
            [EXCP_TRAP] = "trap",
        };
        static int count;
        const char *name = NULL;
        int psw;

        if (i >= 0 && i < ARRAY_SIZE(names)) {
            name = names[i];
        }
        if (name == NULL) {
            name = "unknown";
        }

        psw = cpu_get_psw(env);
        qemu_log("INT %6d: %s pc=%04x psw=%04x sp=%04x\n",
                 ++count, name, env->pc, psw,
                 env->sp[(psw & PSW_CM) >> PSW_V_CM]);
    }
    cs->exception_index = -1;

    /* For now we're treating this like semi-hosting only.  */
    switch (i) {
    case EXCP_HALT:
        /* Graceful shutdown with success exit status.  */
        qemu_system_shutdown_request();
        return;

    case EXCP_RESET:
    case EXCP_BPT:
    case EXCP_IOT:
    case EXCP_EMT:
    case EXCP_TRAP:
        /* Exiting ourselves is the only way to have non-success exit.  */
        exit(EXIT_FAILURE);

    case EXCP_ILL:
        raise(SIGILL);
    }
    g_assert_not_reached();
}

PDP11CPU *cpu_pdp11_init(const char *cpu_model)
{
    return PDP11_CPU(cpu_generic_init(TYPE_PDP11_CPU, cpu_model));
}
