|
|   $Id: cprt0.as,v 1.1.2.1.2.3 2003/06/18 23:39:33 pierre Exp $
|   This file is part of the Free Pascal run time library.
|   Copyright (c) 2001 by Free Pascal Core Team
|
|   See the file COPYING.FPC, included in this distribution,
|   for details about the copyright.
|
|   This program is distributed in the hope that it will be useful,
|   but WITHOUT ANY WARRANTY;without even the implied warranty of
|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
|
|**********************************************************************}
|
| NetBSD m68k ELF startup code for linking with C lib for Free Pascal
|
        .file   "cprt0.as"
	.text
        .globl  _start
        .type   _start,@function
_start:
        .globl  __entry
        .type   __entry,@function
__entry:
        move.l   8(%sp),%d0
        move.l   %d0,U_SYSBSD_ENVP
        move.l   %d0,__environ
        move.l   4(%sp),%d0
        move.l   %d0,U_SYSBSD_ARGV
        move.l   (%sp),%d0
        move.l   %d0,U_SYSBSD_ARGC
|       The arguments should be in correct order for
|       calling __libc_init
|       This code is untested for now PM
        jsr     __libc_init
|       insert _fini in atexit chain
        move.l   _fini,-(%sp)
        jsr      atexit
        addq.l   #4,%sp
|       call _init function
        jsr      _init

        jsr      PASCALMAIN

|       Used by System_exit procedure
        .globl  _haltproc
_haltproc:
|       Call C exit function
        move.w   U_SYSBSD_EXITCODE,%d1
        move.l   %d1,-(%sp)
        jsr      exit
        moveq.l  #1,%d0
|       No, leave the exit code on stack as NetBSD expects it
|       move.l   (%sp)+,%d1
        trap     #0
        addq.l   #4,%sp
        jmp      _haltproc


|       Is this still needed ??
|        .data
|        .align	4
|        .globl	___fpc_brk_addr
|___fpc_brk_addr:
|       .long	0



| This section is needed for NetBSD to recognize a NetBSD binary as such.
| otherwise it will be startup in Linux emulation mode.

.section ".note.netbsd.ident","a"
.p2align 2

.long 7
.long 4
| ELF NOTE TYPE NETBSD TAG
.long 1
.ascii "NetBSD\0\0"
.long 199905

|
| $Log: cprt0.as,v $
| Revision 1.1.2.1.2.3  2003/06/18 23:39:33  pierre
|  * reinsert note section removed by error
|
| Revision 1.1.2.1.2.2  2003/06/18 14:52:45  pierre
|  * changed 'bra.l' into 'jmp' because 'bra.l' is only for m68020
|
| Revision 1.1.2.1.2.1  2003/06/18 14:46:14  pierre
|  * force 32 bit relative 'bra.l _haltproc' to avoid relocation troubles
|
| Revision 1.1.2.1  2001/08/10 11:00:59  pierre
|  first m68k netbsd files
|
| Revision 1.1.2.2  2001/08/01 13:26:17  pierre
|  * syntax adapted to GNU as
|
| Revision 1.1.2.1  2001/07/13 15:29:32  pierre
|  first version of cprt0.as
|
|

