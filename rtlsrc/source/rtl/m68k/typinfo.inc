{
    $Id: typinfo.inc,v 1.1.2.2 2003/04/05 21:42:55 carl Exp $
    This file is part of the Free Pascal run time library.

    Copyright (c) 2001 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ This unit provides the same Functionality as the TypInfo Unit }
{ of Delphi                                                     }
{ ---------------------------------------------------------------------
  This include contains cpu-specific Low-level calling of methods.
  Translated from the i386 version
  ---------------------------------------------------------------------}


Function CallIntegerFunc(s: Pointer; Address: Pointer; Index, IValue: LongInt): Int64; assembler;
  asm
     move.l S,a0
     move.l Address,a1
     // ? Indexed Function
     move.l Index,d0
     cmp.l  d0,d0
     beq    @LINoPush
     move.l IValue,d0
     move.l d0,-(sp)
  @LINoPush:
     move.l a0,-(sp)
     jsr (a1)
     // now the result is in d1:d0
  end;

Function CallIntegerProc(s : Pointer;Address : Pointer;Value : Integer; INdex,IValue : Longint) : Integer;assembler;
  asm
     move.l S,a0
     move.l Address,a1
     // ? Indexed Function
     move.l Index,d0
     cmp.l  d0,d0
     beq    @LIPNoPush
     move.l IValue,d0
     move.l d0,-(sp)
  @LIPNoPush:
     move.l a0,-(sp)
     jsr (a1)
  end;
  
  
Function CallDoubleFunc(s : Pointer;Address : Pointer; INdex,IValue : Longint) : Double;assembler;
  asm
     move.l S,a0
     move.l Address,a1
     // ? Indexed Function
     move.l Index,d0
     cmp.l  d0,d0
     beq    @LINoPush
     move.l IValue,d0
     move.l d0,-(sp)
  @LINoPush:
     move.l a0,-(sp)
     jsr (a1)
  end;
  
  
Function CallSingleFunc(s : Pointer;Address : Pointer; INdex,IValue : Longint) : Single;assembler;  
asm
     move.l S,a0
     move.l Address,a1
     // ? Indexed Function
     move.l Index,d0
     cmp.l  d0,d0
     beq    @LINoPush
     move.l IValue,d0
     move.l d0,-(sp)
  @LINoPush:
     move.l a0,-(sp)
     jsr (a1)
  end;
  
  

Function CallExtendedFunc(s : Pointer;Address : Pointer; INdex,IValue : Longint) : Extended;assembler;
  asm
     move.l S,a0
     move.l Address,a1
     // ? Indexed Function
     move.l Index,d0
     cmp.l  d0,d0
     beq    @LINoPush
     move.l IValue,d0
     move.l d0,-(sp)
  @LINoPush:
     move.l a0,-(sp)
     jsr (a1)
  end;

Function CallExtendedProc(s : Pointer;Address : Pointer;Value : Extended; INdex,IVAlue : Longint) : Integer;assembler;
  asm
     // Push value to set
     lea    Value,a0
     move.l (a0),-(sp)
     move.l 4(a0),-(sp)
     move.l 8(a0),-(sp)
     move.l S,a0
     move.l Address,a1
     // ? Indexed Function
     move.l Index,d0
     cmp.l  d0,d0
     beq    @LIPNoPush
     move.l IValue,d0
     move.l d0,-(sp)
  @LIPNoPush:
     move.l a0,-(sp)
     jsr (a1)
  end;

Function CallBooleanFunc(s : Pointer;Address : Pointer; Index,IValue : Longint) : Boolean;assembler;
  asm
     move.l S,a0
     move.l Address,a1
     // ? Indexed Function
     move.l Index,d0
     cmp.l  d0,d0
     beq    @LBNoPush
     move.l IValue,d0
     move.l d0,-(sp)
  @LBNoPush:
     move.l a0,-(sp)
     jsr (a1)
  end;

// Assembler Functions can't have short stringreturn values.
// So we make a Procedure with var parameter.
// That's not true (FK)

Procedure CallSStringFunc(s : Pointer;Address : Pointer; INdex,IValue : Longint;
                        Var Res: Shortstring);assembler;
  asm
     move.l S,a0
     move.l Address,a1
     // ? Indexed Function
     move.l Index,d0
     cmp.l  d0,d0
     beq    @LSSNoPush
     move.l IValue,d0
     move.l d0,-(sp)
  @LSSNoPush:
     move.l Res,-(sp)
     move.l a0,-(sp)
     jsr (a1)
  end;

Procedure CallSStringProc(s : Pointer;Address : Pointer;Const Value : ShortString; INdex,IVAlue : Longint);assembler;
  asm
     move.l S,a0
     move.l Address,a1
     // ? Indexed Function
     move.l Index,d0
     cmp.l  d0,d0
     beq    @LSSPNoPush
     move.l IValue,d0
     move.l d0,-(sp)
  @LSSPNoPush:
     move.l a0,-(sp)
     jsr (a1)
  end;

{
  $Log: typinfo.inc,v $
  Revision 1.1.2.2  2003/04/05 21:42:55  carl
    * Compilation was broken for m68k targets because of last type info patch

  Revision 1.1.2.1  2001/08/02 23:31:13  pierre
   * cpu specific code moved into include and translated to m68k cpu

}
