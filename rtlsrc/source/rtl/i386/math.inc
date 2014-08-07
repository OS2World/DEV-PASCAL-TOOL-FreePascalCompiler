{
    $Id: math.inc,v 1.1.2.10 2003/02/05 20:01:13 carl Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2001 by the Free Pascal development team

    Implementation of mathematical routines (for extended type)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{****************************************************************************
                       EXTENDED data type routines
 ****************************************************************************}

    {$define FPC_SYSTEM_HAS_PI}
    function pi : extended;[internproc:in_pi];
    {$define FPC_SYSTEM_HAS_ABS}
    function abs(d : extended) : extended;[internproc:in_abs_extended];
    {$define FPC_SYSTEM_HAS_SQR}
    function sqr(d : extended) : extended;[internproc:in_sqr_extended];
    {$define FPC_SYSTEM_HAS_SQRT}
    function sqrt(d : extended) : extended;[internproc:in_sqrt_extended];
    {$define FPC_SYSTEM_HAS_ARCTAN}
    function arctan(d : extended) : extended;[internproc:in_arctan_extended];
    {$define FPC_SYSTEM_HAS_LN}
    function ln(d : extended) : extended;[internproc:in_ln_extended];
    {$define FPC_SYSTEM_HAS_SIN}
    function sin(d : extended) : extended;[internproc:in_sin_extended];
    {$define FPC_SYSTEM_HAS_COS}
    function cos(d : extended) : extended;[internproc:in_cos_extended];

    {$define FPC_SYSTEM_HAS_EXP}
    function exp(d : extended) : extended;assembler;[internconst:in_const_exp];
       asm
            // comes from DJ GPP
            fldt        d
            fldl2e
            fmulp       %st,%st(1)
            fstcw      .LCW1
            fstcw      .LCW2
            andw        $0xf3ff,.LCW2
            orw         $0x0400,.LCW2
            fldcw      .LCW2
            fld         %st(0)
            frndint
            fldcw      .LCW1
            fxch        %st(1)
            fsub        %st(1),%st
            f2xm1
            fld1
            faddp       %st,%st(1)
            fscale
            fstp        %st(1)
            jmp         .LCW3
            // store some help data in the data segment
        .data
        .LCW1:
            .word       0
        .LCW2:
            .word       0
        .text
        .LCW3:
      end;


    {$define FPC_SYSTEM_HAS_FRAC}
    function frac(d : extended) : extended;assembler;[internconst:in_const_frac];
      asm
            subl $16,%esp
            fnstcw -4(%ebp)
            fwait
            movw -4(%ebp),%cx
            orw $0x0c3f,%cx
            movw %cx,-8(%ebp)
            fldcw -8(%ebp)
            fwait
            fldt d
            frndint
            fldt d
            fsub %st(1),%st
            fstp %st(1)
            fclex
            fldcw -4(%ebp)
      end ['ECX'];


    {$define FPC_SYSTEM_HAS_INT}
    function int(d : extended) : extended;assembler;[internconst:in_const_int];
      asm
            subl $16,%esp
            fnstcw -4(%ebp)
            fwait
            movw -4(%ebp),%cx
            orw $0x0c3f,%cx
            movw %cx,-8(%ebp)
            fldcw -8(%ebp)
            fwait
            fldt d
            frndint
            fclex
            fldcw -4(%ebp)
      end ['ECX'];



    {$define FPC_SYSTEM_HAS_TRUNC}
    function trunc(d : extended) : int64;assembler;[internconst:in_const_trunc];
      var
        oldcw,
        newcw : word;
        res   : int64;
      asm
            fnstcw oldcw
            fwait
            movw oldcw,%cx
            orw $0x0c3f,%cx
            movw %cx,newcw
            fldcw newcw
            fwait
            fldt d
            fistpq res
            movl res,%eax
            movl res+4,%edx
            fldcw oldcw
      end ['EAX','ECX','EDX'];


    {$define FPC_SYSTEM_HAS_ROUND}
    function round(d : extended) : longint;assembler;[internconst:in_const_round];
      var
        oldcw,
        newcw : word;
        res   : longint;
      asm
            fnstcw oldcw
            fwait
            movw $0x1372,newcw
            fldcw newcw
            fwait
            fldt d
            fistpl res
            movl res,%eax
            fldcw oldcw
      end ['EAX'];


    {$define FPC_SYSTEM_HAS_POWER}
   function power(bas,expo : extended) : extended;
     begin
        if bas=0 then
          begin
            if expo<>0 then
              power:=0.0
            else
              HandleError(207);
          end
        else if expo=0 then
         power:=1
        else
        { bas < 0 is not allowed }
         if bas<0 then
          handleerror(207)
         else
          power:=exp(ln(bas)*expo);
     end;


{****************************************************************************
                         Fixed data type routines
 ****************************************************************************}

{$ifdef HASFIXED} { Not yet allowed }

    function sqrt(d : fixed) : fixed;

      begin
         asm
            movl d,%eax
            movl %eax,%ebx
            movl %eax,%ecx
            jecxz .L_kl
            xorl %esi,%esi
         .L_it:
            xorl %edx,%edx
            idivl %ebx
            addl %ebx,%eax
            shrl $1,%eax
            subl %eax,%esi
            cmpl $1,%esi
            jbe .L_kl
            movl %eax,%esi
            movl %eax,%ebx
            movl %ecx,%eax
            jmp .L_it
         .L_kl:
            shl $8,%eax
            leave
            ret $4
         end;
      end;


    function int(d : fixed) : fixed;
    {*****************************************************************}
    { Returns the integral part of d                                  }
    {*****************************************************************}
    begin
      int:=d and $ffff0000;       { keep only upper bits      }
    end;


    function trunc(d : fixed) : longint;
    {*****************************************************************}
    { Returns the Truncated integral part of d                        }
    {*****************************************************************}
    begin
      trunc:=longint(integer(d shr 16));   { keep only upper 16 bits  }
    end;

    function frac(d : fixed) : fixed;
    {*****************************************************************}
    { Returns the Fractional part of d                                }
    {*****************************************************************}
    begin
      frac:=d AND $ffff;         { keep only decimal parts - lower 16 bits }
    end;

    function abs(d : fixed) : fixed;
    {*****************************************************************}
    { Returns the Absolute value of d                                 }
    {*****************************************************************}
    begin
       asm
           movl d,%eax
           rol $16,%eax             { Swap high & low word.}
           {Absolute value: Invert all bits and increment when <0 .}
           cwd                      { When ax<0, dx contains $ffff}
           xorw %dx,%ax             { Inverts all bits when dx=$ffff.}
           subw %dx,%ax             { Increments when dx=$ffff.}
           rol $16,%eax             { Swap high & low word.}
           leave
           ret $4
       end;
    end;


    function sqr(d : fixed) : fixed;
    {*****************************************************************}
    { Returns the Absolute squared value of d                         }
    {*****************************************************************}
    begin
            {16-bit precision needed, not 32 =)}
       sqr := d*d;
{       sqr := (d SHR 8 * d) SHR 8; }
    end;


    function Round(x: fixed): longint;
    {*****************************************************************}
    { Returns the Rounded value of d as a longint                     }
    {*****************************************************************}
    var
     lowf:integer;
     highf:integer;
    begin
      lowf:=x and $ffff;       { keep decimal part ... }
      highf :=integer(x shr 16);
      if lowf > 5 then
        highf:=highf+1
      else
      if lowf = 5 then
      begin
        { here we must check the sign ...       }
        { if greater or equal to zero, then     }
        { greater value will be found by adding }
        { one...                                }
         if highf >= 0 then
           Highf:=Highf+1;
      end;
      Round:= longint(highf);
    end;

{$endif HASFIXED}


{
  $Log: math.inc,v $
  Revision 1.1.2.10  2003/02/05 20:01:13  carl
    * round returns longint again for compatibility with 1.0.6

  Revision 1.1.2.9  2003/01/27 19:49:25  carl
    * round bugfix with -Or switch

  Revision 1.1.2.8  2003/01/15 00:48:28  peter
    * power returns int64

  Revision 1.1.2.7  2003/01/14 21:15:23  peter
    * regenerated

  Revision 1.1.2.6  2002/10/14 11:05:39  pierre
   + FPC_SYSTEM_HAS_POWER_LONGINT defined

  Revision 1.1.2.5  2001/12/14 03:31:08  carl
  - remove real2double()

  Revision 1.1.2.4  2001/12/13 20:22:42  michael
  + Added double2real function from main branch

  Revision 1.1.2.3  2001/07/29 23:56:28  carl
  - removed internamth define (always internal)

  Revision 1.1.2.2  2001/04/16 10:56:13  peter
    * fixes for stricter compiler

  Revision 1.1.2.1  2001/04/03 20:33:01  marco
   * Quickfixed trunc to int64 for Sebastian.

  Revision 1.1  2000/07/13 06:30:42  michael
  + Initial import

  Revision 1.23  2000/05/02 10:37:50  pierre
   * 0**n where n<>0 is 0; 0**0 generates RTE 207

  Revision 1.22  2000/04/07 21:29:00  pierre
   changed to get nasm to compile system

  Revision 1.21  2000/02/15 14:37:36  florian
    * disabled FIXED data type per default

  Revision 1.20  2000/02/09 16:59:29  peter
    * truncated log

  Revision 1.19  2000/01/07 16:41:33  daniel
    * copyright 2000

  Revision 1.18  2000/01/07 16:32:24  daniel
    * copyright 2000 added

  Revision 1.17  1999/10/06 17:44:43  peter
    * fixed power(int,int) with negative base
    * power(ext,ext) with negative base gives rte 207

  Revision 1.16  1999/09/15 20:24:11  florian
    * some math functions are now coded inline by the compiler
}
