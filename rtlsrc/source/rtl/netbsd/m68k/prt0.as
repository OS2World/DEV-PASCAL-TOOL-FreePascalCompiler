|
|   $Id: prt0.as,v 1.1.2.1.2.3 2003/06/18 23:39:33 pierre Exp $
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
|   NetBSD m68k ELF startup code for Free Pascal
|
        .file   "prt0.as"
	.text
        .globl  __entry
__entry:
        .globl  _start
_start:
        move.l   (%sp),%d0
        move.l   %d0,U_SYSBSD_ARGC
        lea      4(%sp),%a0
        move.l   %a0,U_SYSBSD_ARGV
        lea      8(%sp,%d0*4),%a0
        move.l   %a0,U_SYSBSD_ENVP
        jsr      PASCALMAIN

        .globl   _haltproc
_haltproc:
        moveq.l  #1,%d0
        clr.l    %d1
        move.w   U_SYSBSD_EXITCODE,%d1
        move.l   %d1,-(%sp)
	bsr	 __call_trap
        addq.l   #4,%sp
        jmp      _haltproc

__call_trap:
	trap	 #0
	rts

        .data
	.align	4
	.globl	___fpc_brk_addr
___fpc_brk_addr:
	.long	0


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
| $Log: prt0.as,v $
| Revision 1.1.2.1.2.3  2003/06/18 23:39:33  pierre
|  * reinsert note section removed by error
|
| Revision 1.1.2.3  2003/06/17 16:55:04  pierre
|  + .note.netbsd.ident section added
|
| Revision 1.1.2.2  2002/09/19 22:27:00  pierre
|  * fix exit trap call
|
| Revision 1.1.2.1  2001/08/10 11:00:59  pierre
|  first m68k netbsd files
|
| Revision 1.1.2.5  2001/08/03 15:13:49  pierre
|  * forgot % prefix in last patch
|
| Revision 1.1.2.4  2001/08/03 14:40:29  pierre
|  * EXITCODE is a word var so we need to load it with move.w
|
| Revision 1.1.2.3  2001/07/30 16:18:31  pierre
|  * convert to normal GNU as syntax
|
| Revision 1.1.2.2  2001/07/17 12:30:53  pierre
|  * fix argv vand envp setting
|
| Revision 1.1.2.1  2001/07/13 15:13:47  pierre
|  + add and fix some comments
|
|

