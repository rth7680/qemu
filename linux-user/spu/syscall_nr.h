/* ??? This is partially fake.  Normally there is no stand-alone SPU;
   The main part of the process is controlled by the PPC host.  Some
   of the base PPC syscalls are normally off-limits to the SPU.

   However, lack of syscalls like "exit" are not expeected by the main
   part of the linux-user emulation, and don't really help when it comes
   to simulation testing of "standalone" test cases.  */
#include "../ppc/syscall_nr.h"
