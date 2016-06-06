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
#include "qemu-common.h"
#include "cpu.h"
#include "hw/hw.h"
#include "hw/devices.h"
#include "hw/boards.h"
#include "hw/loader.h"
#include "exec/address-spaces.h"
#include "exec/cpu-all.h"
#include "qemu/error-report.h"

/* A.OUT loader */

struct exec
{
  uint16_t a_info;   /* Use macros N_MAGIC, etc for access */
  uint16_t a_text;   /* length of text, in bytes */
  uint16_t a_data;   /* length of data, in bytes */
  uint16_t a_bss;    /* length of uninitialized data area, in bytes */
  uint16_t a_syms;   /* length of symbol table data in file, in bytes */
  uint16_t a_entry;  /* start address */
  uint16_t a_trsize; /* length of relocation info for text, in bytes */
  uint16_t a_drsize; /* length of relocation info for data, in bytes */
};

static void bswap_ahdr(struct exec *e)
{
    tswap16s(&e->a_info);
    tswap16s(&e->a_text);
    tswap16s(&e->a_data);
    tswap16s(&e->a_bss);
    tswap16s(&e->a_syms);
    tswap16s(&e->a_entry);
    tswap16s(&e->a_trsize);
    tswap16s(&e->a_drsize);
}

#define N_MAGIC(exec) ((exec).a_info & 0xffff)
#define OMAGIC 0407
#define NMAGIC 0410
#define ZMAGIC 0413
#define QMAGIC 0314
#define _N_HDROFF(x) (1024 - sizeof (struct exec))

#define TPS  1024

#define N_TXTOFF(x) \
    (N_MAGIC(x) == ZMAGIC ? _N_HDROFF((x)) + sizeof (struct exec)  \
     : N_MAGIC(x) == QMAGIC ? 0                                    \
     : sizeof (struct exec))
#define N_TXTADDR(x, TPS)        (N_MAGIC(x) == QMAGIC ? TPS : 0)
#define _N_SEGMENT_ROUND(x, TPS) (((x) + TPS - 1) & ~(TPS - 1))
#define _N_TXTENDADDR(x, TPS)    (N_TXTADDR(x, TPS) + (x).a_text)

#define N_DATADDR(x, TPS) \
    (N_MAGIC(x)==OMAGIC ? _N_TXTENDADDR(x, TPS) \
     : _N_SEGMENT_ROUND (_N_TXTENDADDR(x, TPS), TPS))

static int pdp11_load_kernel(const char *filename, int max_sz)
{
    int fd;
    ssize_t size, ret;
    struct exec e;
    uint32_t magic;

    fd = open(filename, O_RDONLY | O_BINARY);
    if (fd < 0) {
        return -1;
    }

    size = read(fd, &e, sizeof(e));
    if (size < 0) {
        goto fail;
    }

    bswap_ahdr(&e);

    magic = N_MAGIC(e);
    switch (magic) {
    case ZMAGIC:
    case QMAGIC:
    case OMAGIC:
        if (e.a_text + e.a_data > max_sz) {
            goto fail;
        }
	lseek(fd, N_TXTOFF(e), SEEK_SET);
	size = read_targphys(filename, fd, 0, e.a_text + e.a_data);
	if (size < 0) {
	    goto fail;
        }
	break;
    case NMAGIC:
        if (N_DATADDR(e, TPS) + e.a_data > max_sz) {
            goto fail;
        }
	lseek(fd, N_TXTOFF(e), SEEK_SET);
	size = read_targphys(filename, fd, 0, e.a_text);
	if (size < 0) {
	    goto fail;
        }
        ret = read_targphys(filename, fd, N_DATADDR(e, TPS), e.a_data);
	if (ret < 0) {
	    goto fail;
        }
	size += ret;
	break;
    default:
	goto fail;
    }
    close(fd);
    return e.a_entry;

 fail:
    close(fd);
    return -1;
}

static void sim_init(MachineState *machine)
{
    MemoryRegion *addr_space, *ram;
    PDP11CPU *cpu;
    int entry;

    cpu = cpu_pdp11_init(NULL);

    addr_space = get_system_memory();
    ram = g_new(MemoryRegion, 1);

    /* Main memory.  Since we don't do split I/D yet, there's no point
       in having any more (or less) than 64k.  If we ever get around to
       implemeting the paging bits, we could conceivably allow up to
       22 bit addresses, or 4MB.  */
    memory_region_allocate_system_memory(ram, NULL, "ram", 64*1024);
    memory_region_add_subregion(addr_space, 0, ram);

    if (machine->kernel_filename == NULL) {
        fprintf(stderr, "Guest image must be specified (using -kernel)\n");
        exit(1);
    }

    entry = pdp11_load_kernel(machine->kernel_filename, 50*1024);
    if (entry < 0) {
        fprintf(stderr, "Error loading image\n");
        exit(1);
    }
    cpu->env.pc = entry;
    cpu->env.sp[0] = 60*1024;
    cpu_set_psw(&cpu->env, 0);
}

static void sim_machine_init(MachineClass *mc)
{
    mc->desc = "sim machine";
    mc->init = sim_init;
    mc->max_cpus = 1;
    mc->is_default = 1;
}
DEFINE_MACHINE("sim", sim_machine_init)
