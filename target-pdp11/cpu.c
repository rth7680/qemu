/*
 *  PDP/11 emulation for qemu: main translation routines.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see
 * <http://www.gnu.org/licenses/lgpl-2.1.html>
 */

#include "qemu/osdep.h"
#include "qapi/error.h"
#include "cpu.h"
#include "qemu-common.h"

static void pdp11_cpu_set_pc(CPUState *cs, vaddr value)
{
    PDP11CPU *cpu = PDP11_CPU(cs);

    cpu->env.pc = value;
}

static void pdp11_cpu_synchronize_from_tb(CPUState *cs, TranslationBlock *tb)
{
    PDP11CPU *cpu = PDP11_CPU(cs);

    cpu->env.pc = tb->pc;
}

static void pdp11_cpu_reset(CPUState *s)
{
    PDP11CPU *cpu = PDP11_CPU(s);
    PDP11CPUClass *tcc = PDP11_CPU_GET_CLASS(cpu);

    tcc->parent_reset(s);
    tlb_flush(s, 1);

    /* TODO: values on reset?  */
}

static bool pdp11_cpu_has_work(CPUState *cs)
{
    return true;
}

static void pdp11_cpu_realizefn(DeviceState *dev, Error **errp)
{
    CPUState *cs = CPU(dev);
    PDP11CPUClass *tcc = PDP11_CPU_GET_CLASS(dev);

    cpu_reset(cs);
    qemu_init_vcpu(cs);

    tcc->parent_realize(dev, errp);
}

static void pdp11_cpu_initfn(Object *obj)
{
    CPUState *cs = CPU(obj);
    PDP11CPU *cpu = PDP11_CPU(obj);
    CPUPDP11State *env = &cpu->env;

    cs->env_ptr = env;
    cpu_exec_init(cs, &error_abort);
    pdp11_tcg_init();
}

static ObjectClass *pdp11_cpu_class_by_name(const char *cpu_model)
{
    ObjectClass *oc;

    oc = object_class_by_name(TYPE_PDP11_CPU);
    if (!oc || object_class_is_abstract(oc)) {
        return NULL;
    }
    return oc;
}

static void pdp11_cpu_class_init(ObjectClass *c, void *data)
{
    PDP11CPUClass *mcc = PDP11_CPU_CLASS(c);
    CPUClass *cc = CPU_CLASS(c);
    DeviceClass *dc = DEVICE_CLASS(c);

    mcc->parent_realize = dc->realize;
    dc->realize = pdp11_cpu_realizefn;

    mcc->parent_reset = cc->reset;
    cc->reset = pdp11_cpu_reset;
    cc->class_by_name = pdp11_cpu_class_by_name;
    cc->has_work = pdp11_cpu_has_work;

    cc->do_interrupt = pdp11_cpu_do_interrupt;
    cc->dump_state = pdp11_cpu_dump_state;
    cc->set_pc = pdp11_cpu_set_pc;
    cc->synchronize_from_tb = pdp11_cpu_synchronize_from_tb;

}

static const TypeInfo pdp11_cpu_type_info = {
    .name = TYPE_PDP11_CPU,
    .parent = TYPE_CPU,
    .instance_size = sizeof(PDP11CPU),
    .instance_init = pdp11_cpu_initfn,
    .class_size = sizeof(PDP11CPUClass),
    .class_init = pdp11_cpu_class_init,
};

static void pdp11_cpu_register_types(void)
{
    type_register(&pdp11_cpu_type_info);
}

type_init(pdp11_cpu_register_types)
