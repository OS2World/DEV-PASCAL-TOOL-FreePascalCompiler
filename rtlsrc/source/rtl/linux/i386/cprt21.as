#
#   $Id: cprt21.as,v 1.1.2.2 2001/06/04 18:04:32 peter Exp $
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
# Linux ELF startup code for Free Pascal
#

        .file   "prt1.as"
        .text
        .globl  _start
        .type   _start,@function
_start:
        /* First locate the start of the environment variables */
        popl    %esi
        movl    %eax,%edi

        movl    %esp,%ebx               /* Points to the arguments */
        movl    %esi,%eax
        incl    %eax
        shll    $2,%eax
        addl    %esp,%eax
        andl    $0xfffffff8,%esp        /* Align stack */

        movl    %eax,U_SYSLINUX_ENVP    /* Move the environment pointer */
        movl    %esi,U_SYSLINUX_ARGC    /* Move the argument counter    */
        movl    %ebx,U_SYSLINUX_ARGV    /* Move the argument pointer    */

        xorl    %ebp,%ebp
        pushl   %edi
        pushl   %esp
        pushl   %edx
        pushl   $_fini_dummy
        pushl   $_init_dummy
        pushl   %ebx
        pushl   %esi
        pushl   $main
        call    __libc_start_main
        hlt

/* fake main routine which will be run from libc */
main:
        /* save return address */
        popl    %eax
        movl    %eax,___fpc_ret
        movl    %ebx,___fpc_ret_ebx
        movl    %ebp,___fpc_ret_ebp
        pushl   %eax

        /* start the program */
        xorl    %ebp,%ebp
        call    PASCALMAIN

        .globl _haltproc
        .type _haltproc,@function
_haltproc:
        xorl    %eax,%eax               /* load and save exitcode */
        movw    U_SYSLINUX_EXITCODE,%ax

        movl    ___fpc_ret,%edx         /* return to libc */
        movl    ___fpc_ret_ebp,%ebp
        movl    ___fpc_ret_ebx,%ebx
        push    %edx
_init_dummy:
_fini_dummy:
        ret

.data
        .align  4

        .globl  ___fpc_brk_addr         /* heap management */
        .type   ___fpc_brk_addr,@object
        .size   ___fpc_brk_addr,4
___fpc_brk_addr:
        .long   0

___fpc_ret:                             /* return address to libc */
        .long   0
___fpc_ret_ebx:
        .long   0
___fpc_ret_ebp:
        .long   0


#
# $Log: cprt21.as,v $
# Revision 1.1.2.2  2001/06/04 18:04:32  peter
#   * use own dummies for _init and _fini
#
# Revision 1.1.2.1  2001/02/14 22:18:45  pierre
#  * fix Sebastian's problem with HeapTrace
#
# Revision 1.1  2000/07/13 06:30:55  michael
# + Initial import
#
# Revision 1.3  2000/01/07 16:41:42  daniel
#   * copyright 2000
#
# Revision 1.2  2000/01/07 16:32:28  daniel
#   * copyright 2000 added
#
# Revision 1.1  1999/05/03 21:29:36  peter
#   + glibc 2.1 support
#
# Revision 1.3  1998/11/04 10:16:25  peter
#   + xorl ebp,ebp to indicate end of backtrace
#
# Revision 1.2  1998/10/14 21:28:46  peter
#   * initialize fpu so sigfpe is finally generated for fpu errors
#
# Revision 1.1  1998/08/12 19:16:09  peter
#   + loader including libc init and exit
#
#
