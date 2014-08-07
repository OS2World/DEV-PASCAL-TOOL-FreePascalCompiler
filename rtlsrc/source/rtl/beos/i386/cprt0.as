#
#   $Id: cprt0.as,v 1.1.2.2 2001/12/17 02:12:57 carl Exp $
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
# BeOS startup code for Free Pascal (with shared library imports)
#
	.file	"cprt0.s"
.data
	.align 4
default_environ:
	.long 0
.text
.globl _start
	.type	 _start,@function
_start:
	pushl %ebp
	movl %esp,%ebp
	subl $4,%esp
	pushl %ebx
	call .L6
.L6:
	popl %ebx
	addl $_GLOBAL_OFFSET_TABLE_+[.-.L6],%ebx
	movl argv_save@GOT(%ebx),%eax
	movl 12(%ebp),%edi
	movl %edi,(%eax)
	movl environ@GOT(%ebx),%eax
	movl 16(%ebp),%esi
	movl %esi,(%eax)
	test %esi,%esi
	jnz .L4
	movl environ@GOT(%ebx),%eax
	movl %ebx,%ecx
	addl $default_environ@GOTOFF,%ecx
	movl %ecx,%edx
	movl %edx,(%eax)
.L4:
/*	movl %fs:0x4,%eax   this doesn't work on BeOS 4.0, let's use find_thread instead */
	pushl $0x0
	call find_thread
	movl __main_thread_id@GOT(%ebx),%edx
	movl %eax,(%edx)
	pushl %esi
	pushl %edi
	movl 8(%ebp),%eax
	pushl %eax
	call _init_c_library_
	call _call_init_routines_
	movl 8(%ebp),%eax
	movl %eax,U_SYSTEM_ARGC
	movl %edi,U_SYSTEM_ARGV
	movl %esi,U_SYSTEM_ENVP
	xorl %ebp,%ebp
	call PASCALMAIN

.globl  sys_exit
.type   sys_exit,@function
sys_exit:
	call _thread_do_exit_notification
	xorl %ebx,%ebx
    movw U_SYSTEM_EXITCODE,%bx
	pushl %ebx
	call exit

/* actual system call interface */
.globl sys_call
.type sys_call@function
sys_call:
    int $0x25
    ret


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


/* int sys_load_image */
.globl sys_load_image
.type sys_load_image,@function
sys_load_image:
movl $0x34,%eax
int $0x25
ret

