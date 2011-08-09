/*
 *  Synergistic Processor Unit (SPU) emulation
 *  Simple Simulator.
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

#include "hw.h"
#include "boards.h"
#include "loader.h"
#include "elf.h"

static void spu_sim_init (ram_addr_t ram_size,
                          const char *boot_device,
                          const char *kernel_filename,
                          const char *kernel_cmdline,
                          const char *initrd_filename,
                          const char *cpu_model)
{
    CPUState *env;
    ram_addr_t ram_offset;
    uint64_t entry;
    int kernel_size;

    env = cpu_init(cpu_model);
    if (!env) {
        fprintf(stderr, "qemu: Unable to find SPU CPU definition\n");
        exit(1);
    }

    if (ram_size & (ram_size - 1)) {
        fprintf(stderr, "qemu: RAM size not a power of 2\n");
        exit(1);
    }
    env->lslr = ram_size - 1;

    /* Allocate RAM */
    ram_offset = qemu_ram_alloc(NULL, "spu.ram", ram_size);
    cpu_register_physical_memory(0, ram_size, ram_offset);
    env->memory_base = qemu_get_ram_ptr(ram_offset);

    /* Load the program to run.  */
    if (kernel_filename == NULL) {
        fprintf(stderr, "qemu: No executable specified\n");
        exit(1);
    }
    kernel_size = load_elf(kernel_filename, NULL, NULL,
                           &entry, NULL, NULL, 1, ELF_MACHINE, 0);
    if (kernel_size < 0) {
        fprintf(stderr, "qemu: could not load executable '%s'\n", kernel_filename);
        exit(1);
    }
    env->pc = entry;
}

static QEMUMachine spu_sim_machine = {
    .name = "sim",
    .desc = "Simulator",
    .init = spu_sim_init,
    .max_cpus = 1,
    .is_default = 1,
};

static void spu_sim_machine_init(void)
{
    qemu_register_machine(&spu_sim_machine);
}
machine_init(spu_sim_machine_init);
