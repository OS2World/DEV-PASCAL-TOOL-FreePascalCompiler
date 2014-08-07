{
    $Id: m68k.inc,v 1.1.2.20 2002/10/20 12:03:46 carl Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Carl-Eric Codere,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{****************************************************************************

      m68k.inc : Processor dependent implementation of system unit
                 For Motorola 680x0 Processor.

*****************************************************************************}

{****************************************************************************}
{   Credit where credit is due:                                              }
{   -Some of the copy routines taken from the Atari dlib source code:        }
{     Dale Schumacher (alias: Dalnefre')  dal@syntel.uucp                    }
{     399 Beacon Ave. St. Paul, MN 55104,USA                                 }
{   -Some of the routines taken from the freeware ATARI Sozobon C compiler   }
{      1988 by Sozobon, Limited.  Author: Johann Ruegg (freeware)            }
{  Thanks to all these people wherever they maybe today!                     }
{****************************************************************************}




{****************************************************************************
                               Primitives
****************************************************************************}

   {$define FPC_SYSTEM_HAS_FILLCHAR}
   Procedure FillChar(var x;count:longint;value:byte);assembler;
   asm
      move.l x, a0         { destination                   }
      move.l count, d1     { number of bytes to fill       }
      tst.l  d1
      ble    @FillEnd      { if less or equan than zero, do nothing }
      move.b value,d0      { fill data                     }
      cmpi.l #65535, d1    { check, if this is a word move }
      ble    @LMEMSET3     { use fast dbra mode            }
      bra    @LMEMSET2
    @LMEMSET1:
      move.b d0,(a0)+
    @LMEMSET2:
      subq.l #1,d1
      cmp.l #-1,d1
      bne  @LMEMSET1
      bra  @FillEnd        { finished slow mode , exit     }

    @LMEMSET4:             { fast loop mode section 68010+ }
      move.b d0,(a0)+
    @LMEMSET3:
      dbra d1,@LMEMSET4
    @FillEnd:
   end ['d0','d1','a0'];


   {$define FPC_SYSTEM_HAS_MOVE}
   procedure move(const source;var dest;count : longint);assembler;
   asm
      move.l   count,d0   {  number of bytes }
      tst.l    d0
      ble      @MoveEnd   { if less or equan than zero, do nothing }
    @LMOVE0:
      move.l   dest,a1    {  destination          }
      move.l   source,a0  {  source               }

      cmpi.l   #65535, d0     { check, if this is a word move }
      ble      @LMEMSET00     { use fast dbra mode 68010+     }

      cmp.l    a0,a1         {  check copy direction }
      bls      @LMOVE4
      add.l    d0,a0         { move pointers to end  }
      add.l    d0,a1
      bra      @LMOVE2
    @LMOVE1:
      move.b   -(a0),-(a1)   {  (s < d) copy loop }
    @LMOVE2:
      subq.l   #1,d0
      cmpi.l   #-1,d0
      bne      @LMOVE1
      bra      @MoveEnd
    @LMOVE3:
      move.b   (a0)+,(a1)+  { (s >= d) copy loop }
    @LMOVE4:
      subq.l   #1,d0
      cmpi.l   #-1,d0
      bne      @LMOVE3
      bra      @MoveEnd

    @LMEMSET00:            { use fast loop mode 68010+ }
      cmp.l    a0,a1         {  check copy direction }
      bls      @LMOVE04
      add.l    d0,a0         { move pointers to end  }
      add.l    d0,a1
      bra      @LMOVE02
    @LMOVE01:
      move.b   -(a0),-(a1)   {  (s < d) copy loop }
    @LMOVE02:
      dbra     d0,@LMOVE01
      bra      @MoveEnd
    @LMOVE03:
      move.b   (a0)+,(a1)+  { (s >= d) copy loop }
    @LMOVE04:
      dbra     d0,@LMOVE03
    { end fast loop mode }
    @MoveEnd:
   end ['d0','a0','a1'];



{****************************************************************************
                              Object Helpers
****************************************************************************}

 {define FPC_SYSTEM_HAS_FPC_HELP_FAIL
  no we use the generic version of FPC_HELP_FAIL}
(*
procedure int_help_fail;assembler;[public,alias:'FPC_HELP_FAIL'];
{ should be called with a object that needs to be
  freed if VMT field is at -1
  %edi contains VMT offset in object again }
asm
{$warning FPC_HELP_FAIL code is not implemented for m68k CPU}
        orl    %esi,%esi
        je     .LHF_1
        cmpl   $-1,8(%ebp)
        je     .LHF_2
        { reset vmt field to zero for static instances }
        cmpl    $0,8(%ebp)
        je      .LHF_3
        { main constructor, we can zero the VMT field now }
        movl    $0,(%esi,%edi,1)
.LHF_3:
        { we zero esi to indicate failure }
        xorl    %esi,%esi
        jmp    .LHF_1
.LHF_2:
        { get vmt address in eax }
        movl    (%esi,%edi,1),%eax
        movl    %esi,12(%ebp)
        { push object position }
        leal    12(%ebp),%eax
        pushl   %eax
        call    AsmFreeMem
        { set both object places to zero }
        xorl    %esi,%esi
        movl    %esi,12(%ebp)
.LHF_1:
end; *)


{$define FPC_SYSTEM_HAS_FPC_HELP_DESTRUCTOR}
    procedure int_help_destructor;assembler;[public,alias:'FPC_HELP_DESTRUCTOR'];
         asm
            { Stack (relative to %fp):
                12 Self
                8 VMT-Address
                4 Main program-Addr
                0 a6
                d0 contains vmt_offset
            }
            { Save Registers }
            movem.l d0-a7,-(sp)

            move.l 8(a6),d1         { Get the address of the vmt }
            or.l   d1,d1            { Check if there is a vmt    }
            beq    @LHD_3
            { get address of local variable into  }
            { address register                    }
            subq.l #4,sp
            move.l sp,a2
            { Yes, get size from SELF! }
            move.l 12(a6),a0
            { get VMT-pointer (from Self) to %ebx }
            { the offset is in d0 since the calling and has not been changed !! }
            (* move.l (a0,d0),a1
            { And put size on the Stack }
            move.l (a1),-(sp)
            AsmFreeMem does not require the size PM *)
            { SELF }
            { I think for precaution }
            { that we should clear the VMT here }
            clr.l (a0,d0)
            move.l a0,(a2)
            move.l a2,-(sp)
            jsr    FPC_FREEMEM
            addq.l #4,sp
         @LHD_3:
            { Restore all registers in the correct order }
            movem.l (sp)+,d0-a7
            rts
         end;

{$define FPC_SYSTEM_HAS_FPC_NEW_CLASS}
  procedure new_class;assembler;[public,alias:'FPC_NEW_CLASS'];

  asm
     { create class ? }
     move.l 8(a6), d0
     { check for nil... }
     beq    @NEW_CLASS1

     { a5 contains vmt }
     move.l a5,-(sp)
     { call newinstance (class method!) }
     { NEWVMTOFFSET is default now }
     move.l  a5,a0
     move.l  52(a0),a0
     jsr     (a0)
     { new instance returns a pointer to the new created }
     { instance in d0                                    }
     { load a5  and insert self                          }
     move.l d0,8(a6)
     move.l d0,a5
     bra    @end
  @NEW_CLASS1:
     move.l a5,8(a6)
  @end:
  end;


  {$define FPC_SYSTEM_HAS_FPC_DISPOSE_CLASS}
  procedure int_dispose_class;assembler;[public,alias:'FPC_DISPOSE_CLASS'];

  asm
     { destroy class ? }
     move.l 12(a6),d0
     beq    @DISPOSE_CLASS
     { no inherited call  - since flag is non-zero }
     move.l (a5),d0
     { push self }
     move.l a5,-(sp)
     { call freeinstance }
     move.l d0,a0
     move.l 56(a0),a0
     jsr    (a0)
  @DISPOSE_CLASS:
     { load self }
     move.l 8(a6),a5
  end;


  procedure int_help_fail_class;assembler;[public,alias : 'FPC_HELP_FAIL_CLASS'];
{ a non zero class must allways be disposed
  VMT is allways at pos 0 }
asm
        move.l  a5,d0
        or.l    d0,d0
        beq     @LHFC_1
        jsr     INT_DISPOSE_CLASS
        { set both object places to zero }
        move.l  #0,a5
        move.l  a5,8(a6)
        move.l  #0,d0    { should return nil on exit }
@LHFC_1:
end;


{****************************************************************************
                                 String
****************************************************************************}





    {$define FPC_SYSTEM_HAS_GET_FRAME}
    function get_frame : longint; assembler;
      asm
              move.l a6,d0
      end;


  {$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
  function get_caller_addr(framebp:longint):longint;assembler;
  asm
        move.l    framebp,d0
        tst.l     d0
        beq       @Lg_a_null
        move.l    d0,a0
        move.l    4(a0),d0
  @Lg_a_null:
  end ['a0','d0'];


  {$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
  function get_caller_frame(framebp:longint):longint;assembler;
  asm
        move.l    framebp,d0
        tst.l     d0
        beq       @Lgnf_null
        move.l    d0,a0
        move.l    (a0),d0
  @Lgnf_null:
  end ['d0','a0'];




{****************************************************************************
                                 Math
****************************************************************************}



  {$define FPC_SYSTEM_HAS_ABS_LONGINT}
  function abs(l:longint):longint; [internconst:in_const_abs];
   begin
    asm
       move.l l,d0
       tst.l  d0
       bpl @LMABS1
       neg.l d0
    @LMABS1:
       move.l d0,@RESULT
    end ['d0'];
   end;

{$define FPC_SYSTEM_HAS_ODD_LONGINT}
function odd(l:longint):boolean;assembler;[internconst:in_const_odd];
asm
       move.l    l,d0
       and.l     #1,d0
end ['d0'];


  {$define FPC_SYSTEM_HAS_SPTR}
  Function Sptr : Longint;assembler;
   asm
        move.l    sp,d0
   end;



{****************************************************************************
                               Bounds Check
****************************************************************************}

{$define FPC_SYSTEM_HAS_FPC_BOUNDCHECK}

Procedure Int_BoundCheck;assembler;[public,alias:'FPC_BOUNDCHECK'];
 { called by code generator with R+ state to    }
 { determine if a range check occured.          }
 { On Entry:                                    }
 {   A1 = address contaning min and max indexes }
 {   D0 = value of current index to check.      }
 asm
  cmp.l   (A1),D0        { lower bound ...    }
  blt     @rebounderr    { is index lower ... }
  add.l   #4,A1
  cmp.l   (A1),D0
  ble     @reboundend
@rebounderr:
  move.l  A6,-(SP)       { generate a correct frame for stack dump }
  move.l  SP,A6
  move.l  #201,-(SP)     { generate a runtime error }
  jsr     FPC_HANDLEERROR
  move.l  (SP)+,A6       { we shouldn't return here but you never know PM }
@reboundend:
 end;

{ do a thread safe inc/dec }

{$ifndef NO_CAS}
{$define SUPPORT_CAS_INSTRUCTION}
{$endif NO_CAS}

function declocked(var l : longint) : boolean;assembler;
  asm
     move.l     l,a0
     move.l     (a0),d0
     move.l     d0,d1
     subq.l     #1,d0
{$ifdef CPUM68020}     
{$ifdef SUPPORT_CAS_INSTRUCTION}
     { If the program is not multithread, no need to use
       the complex method
     }  
     tst.b      IsMultithread
     beq        @declockednolock
     { repeat until the operand has been updated }
@trylocked:     
     cas.l      d1,d0,(a0)
     bne        @trylocked
     bra        @declockedend
{$endif}
{$endif}
@declockednolock:
     move.l     d0,(a0)
@declockedend:
     tst.l      d0
     seq        d0
     and.l      #1,d0
  end ['d0','d1','a0'];

procedure inclocked(var l : longint);assembler;
  asm
     move.l     l,a0
     move.l     (a0),d0
     move.l     d0,d1
     addq.l     #1,d0
{$ifdef CPUM68020}
{$ifdef SUPPORT_CAS_INSTRUCTION}
     { If the program is not multithread, no need to use
       the complex method
     }  
     tst.b      IsMultithread
     beq        @inclockednolock
     { repeat until the operand has been updated }
@trylocked:     
     cas.l      d1,d0,(a0)
     bne        @trylocked
     bra        @inclockedend
{$endif}     
{$endif}
@inclockednolock:
     move.l     d0,(a0)
@inclockedend:
  end ['d0','d1','a0'];

{
  $Log: m68k.inc,v $
  Revision 1.1.2.20  2002/10/20 12:03:46  carl
    * avoid crashes with negative values for fill / move

  Revision 1.1.2.19  2002/10/12 13:21:36  carl
    * I did a really stupid bug, comparison was wrong, in declocked,
      now its fixed :(

  Revision 1.1.2.18  2002/10/09 11:17:36  pierre
   + added NO_CAS conditional to easily overcome problem of unsupported CAS instruction in older compiler

  Revision 1.1.2.17  2002/10/08 19:21:29  carl
    + inclocked / declocked added

  Revision 1.1.2.16  2002/10/06 13:44:50  carl
    - removed unused strcmp routine (uses generic version instead)

  Revision 1.1.2.15  2002/09/14 13:39:58  carl
    * fpc_dispose_class fixes (classes work now)

  Revision 1.1.2.14  2002/04/01 21:10:49  carl
  * fail class constructor should clear accumulator

  Revision 1.1.2.13  2002/02/28 22:41:37  pierre
   * assembler odd function

  Revision 1.1.2.12  2001/09/14 16:04:25  pierre
   * try to fix class helpers

  Revision 1.1.2.11  2001/09/13 22:46:03  pierre
   * some class related code fixes

  Revision 1.1.2.10  2001/08/01 08:02:52  pierre
   * Use generic fail code, cpu specific code commented out

  Revision 1.1.2.9  2001/07/31 01:43:37  carl
  - removed commented out int_str()
  - removed STRCONCAT (the generic version calls move() which is coded in assembler - fast enough)
  - removed STRCOPY (same remark as above)
  (STRCMP should still be implemented in assembler)

  Revision 1.1.2.8  2001/07/23 19:06:38  pierre
   * fix the boundcheck routine

  Revision 1.1.2.7  2001/07/18 23:53:10  pierre
   * fix the FPC_HELP_DESTRUCTOR code

  Revision 1.1.2.6  2001/07/18 07:21:07  pierre
   + fake FPC_HELP_FAIL inserted

  Revision 1.1.2.5  2001/05/17 01:34:56  carl
  * some bug corrections and added missing routines

  Revision 1.1.2.4  2001/05/09 03:31:24  carl
  * corrected BoundCheck -> FPC_BOUNDCHECK
  - removed some problematic define

  Revision 1.1.2.3  2001/04/24 12:05:58  carl
  * correction of problems with testing directly address registers (not allowed)

  Revision 1.1.2.2  2001/04/22 23:40:03  carl
  + temporarily added FPC_NEW_ CLASS and FPC_DISPOSE_CLASS for compilation

  Revision 1.1.2.1  2001/04/21 05:12:38  carl
  + updates to correct header definitions

  Revision 1.1  2000/07/13 06:30:56  michael
  + Initial import

  Revision 1.17  2000/01/07 16:41:42  daniel
    * copyright 2000

  Revision 1.16  2000/01/07 16:32:29  daniel
    * copyright 2000 added

  Revision 1.15  1998/10/17 14:34:37  carl
   * FillChar and FillObject bugfix, count was compared with byte

  Revision 1.14  1998/10/16 13:37:45  pierre
    * added code for vmt_offset in destructors

  Revision 1.13  1998/10/15 11:35:03  pierre
   + first step of variable vmt offset
     offset is stored in R_EDI (R_D0)
     if objectvmtoffset is defined

  Revision 1.12  1998/10/13 08:00:06  pierre
    * some bugs related to FPC_ prefix fixed
    * problems with pbyte sometimes defined and sometimes not for rttip.inc solved

  Revision 1.11  1998/09/14 10:48:29  peter
    * FPC_ names
    * Heap manager is now system independent

  Revision 1.10  1998/08/17 12:26:04  carl
    + simple cleanup of comments

  Revision 1.9  1998/07/30 13:26:14  michael
  + Added support for ErrorProc variable. All internal functions are required
    to call HandleError instead of runerror from now on.
    This is necessary for exception support.

  Revision 1.8  1998/07/10 11:02:41  peter
    * support_fixed, becuase fixed is not 100% yet for the m68k

  Revision 1.7  1998/07/02 12:20:58  carl
    + Io-Error and overflow print erroraddr in hex now

  Revision 1.6  1998/07/01 14:25:57  carl
    * strconcat was copying one byte too much
    * strcopy bugfix was using signed comparison
    + STRCOPY uses register calling conventions
    * FillChar bugfix was loading a word instead of a byte
}
