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

#ifndef QEMU_PDP11_CPU_QOM_H
#define QEMU_PDP11_CPU_QOM_H

#include "qom/cpu.h"

#define TYPE_PDP11_CPU "pdp11-cpu"

#define PDP11_CPU_CLASS(klass) \
    OBJECT_CLASS_CHECK(PDP11CPUClass, (klass), TYPE_PDP11_CPU)
#define PDP11_CPU(obj) \
    OBJECT_CHECK(PDP11CPU, (obj), TYPE_PDP11_CPU)
#define PDP11_CPU_GET_CLASS(obj) \
    OBJECT_GET_CLASS(PDP11CPUClass, (obj), TYPE_PDP11_CPU)

typedef struct PDP11CPUClass {
    /*< private >*/
    CPUClass parent_class;
    /*< public >*/

    DeviceRealize parent_realize;
    void (*parent_reset)(CPUState *cpu);
} PDP11CPUClass;

/**
 * PDP11CPU:
 * @env: #CPUPDP11State
 *
 * A PDP11 CPU.
 */
typedef struct PDP11CPU {
    /*< private >*/
    CPUState parent_obj;
    /*< public >*/

    CPUPDP11State env;
} PDP11CPU;

static inline PDP11CPU *pdp11_env_get_cpu(CPUPDP11State *env)
{
    return container_of(env, PDP11CPU, env);
}

#define ENV_GET_CPU(e) CPU(pdp11_env_get_cpu(e))

#define ENV_OFFSET offsetof(PDP11CPU, env)

hwaddr pdp11_cpu_get_phys_page_debug(CPUState *cpu, vaddr addr);
void pdp11_cpu_do_interrupt(CPUState *cpu);
void pdp11_cpu_dump_state(CPUState *cpu, FILE *f,
                            fprintf_function cpu_fprintf, int flags);

#endif /*QEMU_PDP11_CPU_QOM_H */
