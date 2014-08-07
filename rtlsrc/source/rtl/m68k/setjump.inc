{
    $Id: setjump.inc,v 1.1.2.3 2001/08/01 09:03:03 pierre Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by xxxx
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{**********************************************************************
           Set_Jmp/Long_jmp
 **********************************************************************}

Function SetJmp (Var S : Jmp_buf) : longint;assembler;[Public, alias : 'FPC_SETJMP'];
asm
  move.l   a0,-(sp)          { save register      }
  move.l   s,a0
  move.l   4(a6),d0          { save pc counter    }
  move.l   d0,8(a0)
  move.l   (a6),d0           { save frame pointer }
  move.l   d0,(a0)
  lea      12(a6),a1         { save stack pointer }
  move.l   a1,4(a0)
  movem.l  a2-a5,12(a0)
  move.l   (sp)+,a0          { restore register   }
  clr.l    d0
end;

Procedure longJmp (Var S : Jmp_buf; value : longint); assembler;[Public, alias : 'FPC_LONGJMP'];
asm
  move.l   value,d0
  move.l   s,a0
  move.l   8(a0),d1
  move.l   12(a0),a2
  move.l   16(a0),a3
  move.l   20(a0),a4
  move.l   24(a0),a5
  move.l   (a0),a6
  move.l   4(a0),sp
  move.l   d1,-(sp)
  rts
end;

