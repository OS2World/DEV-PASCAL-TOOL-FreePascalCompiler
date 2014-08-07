#
#   $Id: prt0.as,v 1.1.2.3 2001/12/17 02:12:57 carl Exp $
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1999-2000 by Michael Van Canneyt and Peter Vreman
#   members of the Free Pascal development team.
#
#   See the file COPYING.FPC, included in this distribution,
#   for details about the copyright.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY;without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
#**********************************************************************}
#
# BeOS startup code for Free Pascal (with base system)
# (no shared library linking)
#

	.file	"prt0.as"


.text


.globl _start
	.type	 _start,@function
_start:
	pushl %ebp
	movl %esp,%ebp
	movl 16(%ebp),%ecx
	movl 12(%ebp),%ebx
	movl 8(%ebp),%eax
	movl %eax,U_SYSTEM_ARGC
	movl %ebx,U_SYSTEM_ARGV
	movl %ecx,U_SYSTEM_ENVP
	xorl %ebp,%ebp
	call PASCALMAIN
    

/* actual system call interface */
.globl sys_call
sys_call:
    int $0x25
    ret

.globl sys_exit
	.type	 sys_exit,@function
sys_exit:
	xorl %ebx,%ebx
    movw U_SYSTEM_EXITCODE,%bx
	pushl %ebx
    call  exit

.global exit
   .type exit,@function
exit:
    movl $0x3F,%eax
    int $0x25



/* int sys_resize_area */
.globl sys_resize_area
.type sys_resize_area,@function
sys_resize_area:
movl $0x8,%eax
int $0x25
ret


/* int sys_create_area */
.globl sys_create_area
.type sys_create_area,@function
sys_create_area:
movl $0x14,%eax
int $0x25
ret


/* int sys_wait_for_thread */
.globl sys_wait_for_thread
.type sys_wait_for_thread,@function
sys_wait_for_thread:
movl $0x22,%eax
int $0x25
ret


/* int sys_load_image */
.globl sys_load_image
.type sys_load_image,@function
sys_load_image:
movl $0x34,%eax
int $0x25
ret

