#
#   $Id: gprt0.as,v 1.1 2000/07/13 06:30:55 michael Exp $
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
# Linux ELF startup code with profiling support for Free Pascal
# Note: Needs linking with -lgmon and -lc
#

        .file   "gprt1.as"
        .text
        .globl _start
        .type _start,@function
_start:
        /* First locate the start of the environment variables */
        popl    %ecx
        movl    %esp,%ebx               /* Points to the arguments */
        movl    %ecx,%eax
        incl    %eax
        shll    $2,%eax
        addl    %esp,%eax
        andl    $0xfffffff8,%esp        /* Align stack */

        movl    %eax,U_SYSLINUX_ENVP    /* Move the environment pointer */
        movl    %ecx,U_SYSLINUX_ARGC    /* Move the argument counter    */
        movl    %ebx,U_SYSLINUX_ARGV    /* Move the argument pointer    */

        finit                           /* initialize fpu */
        fwait
        fldcw   ___fpucw

        pushl   $_etext                 /* Initialize gmon */
        pushl   $_start
        call    monstartup
        addl    $8,%esp
        pushl   $_mcleanup
        call    atexit
        addl    $4,%esp

        xorl    %ebp,%ebp
        call    PASCALMAIN

        .globl _haltproc
        .type _haltproc,@function
_haltproc:
        xorl    %ebx,%ebx               /* load and save exitcode */
        movw    U_SYSLINUX_EXITCODE,%bx
        pushl   %ebx

        call    exit                    /* call libc exit, this will */
                                        /* write the gmon.out */

        movl    $1,%eax                 /* exit call */
        popl    %ebx
        int     $0x80
        jmp     _haltproc

.data
        .align  4
___fpucw:
        .long   0x1332

        .globl  ___fpc_brk_addr         /* heap management */
        .type   ___fpc_brk_addr,@object
        .size   ___fpc_brk_addr,4
___fpc_brk_addr:
        .long   0

#
# $Log: gprt0.as,v $
# Revision 1.1  2000/07/13 06:30:55  michael
# + Initial import
#
# Revision 1.9  2000/02/08 12:39:48  peter
#   * removed curbrk
#
# Revision 1.8  2000/01/07 16:41:42  daniel
#   * copyright 2000
#
# Revision 1.7  2000/01/07 16:32:28  daniel
#   * copyright 2000 added
#
# Revision 1.6  1999/11/08 23:07:48  peter
#   * removed aout entries
#
# Revision 1.5  1998/11/04 10:16:27  peter
#   + xorl ebp,ebp to indicate end of backtrace
#
# Revision 1.4  1998/10/14 21:28:48  peter
#   * initialize fpu so sigfpe is finally generated for fpu errors
#
# Revision 1.3  1998/08/08 14:42:10  peter
#   * added missing ___fpc_sbrk and logs
#
#
