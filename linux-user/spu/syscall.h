/* ??? This is all fake.  Normally there is no stand-alone SPU.  */

struct target_pt_regs {
    abi_ulong gpr[128*4];
    abi_ulong pc;
};

struct target_syscall_block
{
    uint64_t nr_ret;    /* System call nr and return value.  */
    uint64_t parm[6];   /* System call arguments.  */
};


#define UNAME_MACHINE "spu"
