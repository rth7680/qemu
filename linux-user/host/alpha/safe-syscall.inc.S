/*
 * safe-syscall.inc.S : host-specific assembly fragment
 * to handle signals occurring at the same time as system calls.
 * This is intended to be included by linux-user/safe-syscall.S
 *
 * Copyright (C) 2016  Richard Henderson <rth@twiddle.net>
 *
 * This work is licensed under the terms of the GNU GPL, version 2 or later.
 * See the COPYING file in the top-level directory.
 */

#include <asm/regdef.h>
#include <asm/pal.h>

	.global safe_syscall_base
	.global safe_syscall_start
	.global safe_syscall_end

	/* This is the entry point for making a system call. The calling
	 * convention here is that of a C varargs function with the
	 * first argument an 'int *' to the signal_pending flag, the
	 * second one the system call number (as a 'long'), and all further
	 * arguments being syscall arguments (also 'long').
	 * We return a long which is the syscall's return value, which
	 * may be negative-errno on failure. Conversion to the
	 * -1-and-errno-set convention is done by the calling wrapper.
	 */
	 .align	4
	 .ent	safe_syscall_base
safe_syscall_base:
	.frame	sp, 0, ra
	.prologue 0

	/* We enter with a0 == *signal_pending
	 *               a1 == syscall number
	 *               a2 ... a5, (stack), (stack) == syscall arguments
	 *               and return the result in v0
	 * and the syscall instruction needs
	 *               v0 == syscall number
	 *               a0 ... a5 == syscall arguments
	 *               and returns the result in v0
	 * Shuffle everything around appropriately.
	 */
	mov	a0, t0		/* signal_pending pointer */
	mov     a1, v0		/* syscall number */
	mov     a2, a0		/* ... and the syscall arguments */
	mov     a3, a1
	mov     a4, a2
	mov     a5, a3
	ldq	a4, 0(sp)
	ldq	a5, 8(sp)

	/* This next sequence of code works in conjunction with the
	 * rewind_if_safe_syscall_function(). If a signal is taken
	 * and the interrupted PC is anywhere between 'safe_syscall_start'
	 * and 'safe_syscall_end' then we rewind it to 'safe_syscall_start'.
	 * The code sequence must therefore be able to cope with this, and
	 * the syscall instruction must be the final one in the sequence.
	 */
safe_syscall_start:
	/* if signal_pending is non-zero, don't do the call */
	ldl	t1, 0(t0)
	bne	t1, 1f
	call_pal PAL_callsys
safe_syscall_end:
	/* We have executed the syscall.  Convert the v0/a3 error pair
	   to a negative errno.  */
	negq	v0, t0
	cmovne	a3, t0, v0
	ret

1:	/* We did not execute the syscall.  */
	lda	v0, -TARGET_ERESTARTSYS
	ret
	.end	safe_syscall_base
