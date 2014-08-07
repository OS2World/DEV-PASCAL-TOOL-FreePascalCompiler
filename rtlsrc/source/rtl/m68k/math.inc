{
    $Id: math.inc,v 1.1.2.17 2003/02/05 20:03:46 carl Exp $
    Copyright (c) 1998-2001 by the FPC Team

    680x0 specific floating point mathematical routines

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
{$ifdef FPC_FPU_INTERNAL}
{$ifopt E-}
    {$define FPC_SYSTEM_HAS_PI}
    function pi : extended;{[internproc:in_pi];}[internconst: in_const_pi];
      begin
        pi := 3.1415926535897932385;
      end;
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
        fmove.x d,fp0
        fetox.x fp0,fp0
      end;

    {$define FPC_SYSTEM_HAS_FRAC}
    function frac(d : extended) : extended;[internconst:in_const_frac];

      begin
        frac:=d-int(d);
      end;

    {$define FPC_SYSTEM_HAS_INT}
    function int(d : extended) : extended;assembler;[internconst : in_const_int];
      asm
        fintrz.x d,fp0
      end;

    {$define FPC_SYSTEM_HAS_ROUND}
    function round(d : extended) : longint;[internconst: in_const_round];
      var
        i,ctrl,intctrl : longint;
      begin
        asm
          fmove.l fpcr,ctrl
          move.l  ctrl,d0
          { set rounding to nerarest and precision to extended }
          andi.l   #$ff0f,d0
          move.l  d0,intctrl
          fmove.l intctrl,fpcr
          fint.x  d,fp0
          fmove.l fp0,i
          fmove.l ctrl,fpcr
        end;
        round:=i;
      end;

    {$define FPC_SYSTEM_HAS_TRUNC}
    function trunc(d : extended) : int64;[internconst:in_const_trunc];
      var
        i : longint;
      begin
        asm
          fintrz.x d,fp0
          fmove.l  fp0,i
        end;
        trunc:=i;
      end;

    {$define FPC_SYSTEM_HAS_POWER}
    function power(bas,expo : extended) : extended;
      begin
        power:=exp(expo*ln(bas));
      end;

{$endif E-}
{$endif FPC_FPU_INTERNAL}


{$i lowmath.inc}

{ Linux/Unix emulate the complete FPU opcodes }
{ therefore use it in that case               }
{$ifdef UNIX}
{$define USE_FPU_TRUNC}
{$endif UNIX}

{$ifdef USE_FPU_TRUNC}
{$ifndef FPC_SYSTEM_HAS_TRUNC}
{$define FPC_SYSTEM_HAS_TRUNC}
function trunc(d : double) : longint;assembler;[internconst:in_const_trunc];
asm
  fintrz.d d,fp0
  fmove.l  fp0,d0
end;

{$endif ndef FPC_SYSTEM_HAS_TRUNC}
{$endif USE_FPU_TRUNC}



{*
-------------------------------------------------------------------------------
Returns the number of leading 0 bits before the most-significant 1 bit of
`a'.  If `a' is zero, 32 is returned.
-------------------------------------------------------------------------------
*}
Function countLeadingZeros32( a:longword ): shortint;

    const countLeadingZerosHigh:array[0..255] of shortint = (
        8, 7, 6, 6, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4,
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    );
Var
    shiftCount: shortint;
Begin

    shiftCount := 0;
    if ( a < $10000 ) then
    Begin
        shiftCount := shiftcount + 16;
        a := a shl 16;
    End;
    if ( a < $1000000 ) then
    Begin
        shiftCount := shiftcount + 8;
        a := a shl 8;
    end;
    shiftCount := shiftcount + countLeadingZerosHigh[ a shr 24 ];
    countLeadingZeros32:= shiftCount;
End;

{*----------------------------------------------------------------------------
| Returns the number of leading 0 bits before the most-significant 1 bit of
| `a'.  If `a' is zero, 64 is returned.
*----------------------------------------------------------------------------*}

function countLeadingZeros64( a : qword): shortint;
var
 shiftcount : shortint;
Begin
    shiftCount := 0;
    if ( a <  (qword(1)  shl 32 )) then
        shiftCount := shiftcount + 32
    else
        a := a shr 32;
    shiftCount := shiftCount + countLeadingZeros32( a );
    countLeadingZeros64:= shiftCount;
End;


{*
-------------------------------------------------------------------------------
Packs the sign `zSign', exponent `zExp', and significand `zSig' into a
single-precision floating-point value, returning the result.  After being
shifted into the proper positions, the three fields are simply added
together to form the result.  This means that any integer portion of `zSig'
will be added into the exponent.  Since a properly normalized significand
will have an integer portion equal to 1, the `zExp' input should be 1 less
than the desired result exponent whenever `zSig' is a complete, normalized
significand.
-------------------------------------------------------------------------------
*}
Function packFloat32( zSign: byte; zExp : smallint; zSig: longword ): Float32;
 Begin
    packFloat32 := ( ( longword( zSign) ) shl 31 ) + ( ( longword (zExp) ) shl 23 )
      + zSig;
 End;

{*
-------------------------------------------------------------------------------
Shifts `a' right by the number of bits given in `count'.  If any nonzero
bits are shifted off, they are ``jammed'' into the least significant bit of
the result by setting the least significant bit to 1.  The value of `count'
can be arbitrarily large; in particular, if `count' is greater than 32, the
result will be either 0 or 1, depending on whether `a' is zero or nonzero.
The result is stored in the location pointed to by `zPtr'.
-------------------------------------------------------------------------------
*}
Procedure shift32RightJamming( a: longword ; count: smallint ; VAR zPtr :longword);
var
  z: longword;
Begin
    if ( count = 0 ) then
        z := a
   else
    if ( count < 32 ) then
    Begin
        z := ( a shr count ) or longword( (( a shl ( ( - count ) AND 31 )) ) <> 0);
    End
   else
    Begin
        z := longword( a <> 0 );
    End;
    zPtr := z;
End;


{*
-------------------------------------------------------------------------------
Shifts the 64-bit value formed by concatenating `a0' and `a1' right by the
number of bits given in `count'.  Any bits shifted off are lost.  The value
of `count' can be arbitrarily large; in particular, if `count' is greater
than 64, the result will be 0.  The result is broken into two 32-bit pieces
which are stored at the locations pointed to by `z0Ptr' and `z1Ptr'.
-------------------------------------------------------------------------------
*}
Procedure
 shift64Right(
     a0 :longword; a1: longword; count:smallint; VAR z0Ptr:longword; VAR z1Ptr:longword);
Var
  z0, z1: longword;
  negCount : shortint;
Begin
    negCount := ( - count ) AND 31;

    if ( count = 0 ) then
    Begin
        z1 := a1;
        z0 := a0;
    End
    else if ( count < 32 ) then
    Begin
        z1 := ( a0 shl negCount ) OR ( a1 shr count );
        z0 := a0 shr count;
    End
   else
    Begin
        if (count < 64) then
          z1 := ( a0 shr ( count AND 31 ) )
        else
          z1 := 0;
        z0 := 0;
    End;
    z1Ptr := z1;
    z0Ptr := z0;
End;


{*
-------------------------------------------------------------------------------
Shifts the 64-bit value formed by concatenating `a0' and `a1' right by the
number of bits given in `count'.  If any nonzero bits are shifted off, they
are ``jammed'' into the least significant bit of the result by setting the
least significant bit to 1.  The value of `count' can be arbitrarily large;
in particular, if `count' is greater than 64, the result will be either 0
or 1, depending on whether the concatenation of `a0' and `a1' is zero or
nonzero.  The result is broken into two 32-bit pieces which are stored at
the locations pointed to by `z0Ptr' and `z1Ptr'.
-------------------------------------------------------------------------------
*}
Procedure
 shift64RightJamming(
     a0:longword; a1: longword; count:smallint; VAR Z0Ptr :longword;VAR z1Ptr: longword );
VAR
    z0, z1 : longword;
    negCount : shortint;
Begin
    negCount := ( - count ) AND 31;

    if ( count = 0 ) then
    Begin
        z1 := a1;
        z0 := a0;
    End
   else
    if ( count < 32 ) then
    Begin
        z1 := ( a0 shl negCount ) OR ( a1 shr count ) OR longword( ( a1 shl negCount ) <> 0 );
        z0 := a0 shr count;
    End
   else
    Begin
        if ( count = 32 ) then
        Begin
            z1 := a0 OR longword( a1 <> 0 );
        End
       else
        if ( count < 64 ) Then
        Begin
            z1 := ( a0 shr ( count AND 31 ) ) OR longword( ( ( a0 shl negCount ) OR a1 ) <> 0 );
        End
       else
        Begin
            z1 := longword( ( a0 OR a1 ) <> 0 );
        End;
        z0 := 0;
    End;
    z1Ptr := z1;
    z0Ptr := z0;
End;

{*
-------------------------------------------------------------------------------
Takes an abstract floating-point value having sign `zSign', exponent `zExp',
and significand `zSig', and returns the proper single-precision floating-
point value corresponding to the abstract input.  Ordinarily, the abstract
value is simply rounded and packed into the single-precision format, with
the inexact exception raised if the abstract input cannot be represented
exactly.  However, if the abstract value is too large, the overflow and
inexact exceptions are raised and an infinity or maximal finite value is
returned.  If the abstract value is too small, the input value is rounded to
a subnormal number, and the underflow and inexact exceptions are raised if
the abstract input cannot be represented exactly as a subnormal single-
precision floating-point number.
    The input significand `zSig' has its binary point between bits 30
and 29, which is 7 bits to the left of the usual location.  This shifted
significand must be normalized or smaller.  If `zSig' is not normalized,
`zExp' must be 0; in that case, the result returned is a subnormal number,
and it must not require rounding.  In the usual case that `zSig' is
normalized, `zExp' must be 1 less than the ``true'' floating-point exponent.
The handling of underflow and overflow follows the IEC/IEEE Standard for
Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function roundAndPackFloat32( zSign : byte; zExp : smallint; zSig : longword ) : float32;
 Var
   roundIncrement, roundBits : BYTE;
   IsTiny : byte;
 Begin
    roundIncrement := $40;
    roundBits := zSig AND $7F;
    if ($FD <= word (zExp) ) then
     Begin
        if (( $FD < zExp ) OR  ( zExp = $FD ) AND ( longint ( zSig + roundIncrement ) < 0 ) ) then
          Begin
             HandleError(205);
             roundAndPackFloat32:=packFloat32( zSign, $FF, 0 ) - byte( roundIncrement = 0 );
             exit;
          End;
        if ( zExp < 0 ) then
          Begin
            isTiny := Flag(( zExp < -1 ) OR ( (zSig + roundIncrement) < $80000000 ));
            shift32RightJamming( zSig, - zExp, zSig );
            zExp := 0;
            roundBits := zSig AND $7F;
            if ( (isTiny = byte(TRUE)) and (roundBits<>0) ) then
               HandleError(206);
          End;
    End;
    { inexact result if roundbits <> 0 ! }
    { now RTE is generated for inexact results }
    zSig := ( zSig + roundIncrement ) shr 7;
    zSig := zSig AND not longword( longword( ( roundBits XOR $40 ) = 0 ) and 1 );
    if ( zSig = 0 ) then zExp := 0;
    roundAndPackFloat32 := packFloat32( zSign, zExp, zSig );
  End;


{*
-------------------------------------------------------------------------------
Packs the sign `zSign', the exponent `zExp', and the significand formed by
the concatenation of `zSig0' and `zSig1' into a double-precision floating-
point value, returning the result.  After being shifted into the proper
positions, the three fields `zSign', `zExp', and `zSig0' are simply added
together to form the most significant 32 bits of the result.  This means
that any integer portion of `zSig0' will be added into the exponent.  Since
a properly normalized significand will have an integer portion equal to 1,
the `zExp' input should be 1 less than the desired result exponent whenever
`zSig0' and `zSig1' concatenated form a complete, normalized significand.
-------------------------------------------------------------------------------
*}
Procedure
 packFloat64( zSign: byte; zExp: smallint; zSig0: longword; zSig1 : longword; VAR c : float64);
 var
    z: Float64;
 Begin
    z.low := zSig1;
    z.high := ( ( longword (zSign) ) shl 31 ) + ( ( longword (zExp) ) shl 20 ) + zSig0;
    c := z;
 End;



{*----------------------------------------------------------------------------
| Returns the result of converting the 64-bit two's complement integer `a'
| to the single-precision floating-point format.  The conversion is performed
| according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}
function int64_to_float32( a: int64 ): float32;saveregisters;[public,alias:'FPC_INT64_TO_SINGLE'];
var
    zSign : byte;
    absA : qword;
    shiftCount: shortint;
    zSig : longword;
    intval : int64rec;
Begin
    if ( a = 0 ) then
      begin
       int64_to_float32 := 0;
       exit;
      end;
    if a < 0 then
      zSign := byte(TRUE)
    else
      zSign := byte(FALSE);
    if zSign<>0 then
       absA := -a
    else
       absA := a;
    shiftCount := countLeadingZeros64( absA ) - 40;
    if ( 0 <= shiftCount ) then
      begin
        int64_to_float32:= packFloat32( zSign, $95 - shiftCount, absA shl shiftCount );
      end
    else
       begin
        shiftCount := shiftCount + 7;
        if ( shiftCount < 0 ) then
          begin
            intval.low := int64rec(AbsA).low;
            intval.high := int64rec(AbsA).high;
            shift64RightJamming( intval.low, intval.high, - shiftCount,
               intval.low, intval.high);
            int64rec(absA).low := intval.low;
            int64rec(absA).high := intval.high;
          end
        else
            absA := absA shl shiftCount;
        int64_to_float32:=roundAndPackFloat32( zSign, $9C - shiftCount, absA );
      end;
End;


{*----------------------------------------------------------------------------
| Returns the result of converting the 64-bit two's complement integer `a'
| to the double-precision floating-point format.  The conversion is performed
| according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}
procedure int64_to_float64( a: int64; var float_result : float64 );saveregisters;[public,alias:'FPC_INT64_TO_DOUBLE'];
var
 zSign : byte;
 intval : int64rec;
 AbsA : qword;
 shiftcount : shortint;
 zSig0, zSig1 : longword;
Begin
    if ( a = 0 ) then
      Begin
       packFloat64( 0, 0, 0, 0, float_result );
       exit;
      end;
    zSign := byte( a < 0 );
    if ZSign<>0 then
      AbsA := -a
    else
      AbsA := a;
    shiftCount := countLeadingZeros64( absA ) - 11;
    if ( 0 <= shiftCount ) then
      Begin
        absA := absA shl shiftcount;
        zSig0:=int64rec(absA).high;
        zSig1:=int64rec(absA).low;
      End
    else
      Begin
        shift64Right( absA, 0, - shiftCount, zSig0, zSig1 );
      End;
    packFloat64( zSign, $432 - shiftCount, zSig0, zSig1, float_result );
End;



{
 $Log: math.inc,v $
 Revision 1.1.2.17  2003/02/05 20:03:46  carl
   * round returns longint again for compatibility with 1.0.6

 Revision 1.1.2.16  2003/01/15 09:56:30  pierre
  * fix small error in previous commit

 Revision 1.1.2.15  2003/01/14 21:15:23  peter
   * regenerated

 Revision 1.1.2.14  2002/10/22 18:53:29  carl
   - Remove FPU_EMULATION (was very misleading!)

 Revision 1.1.2.13  2002/10/17 20:15:51  pierre
  * fix round function, d0 was not set

 Revision 1.1.2.12  2002/10/14 11:08:58  pierre
  * generate extended math functions for m68k if FPC_FPU_INTERNAL is defined

 Revision 1.1.2.11  2002/10/13 15:28:39  carl
   * just to be sure, use saveregisters directive for int64 to real
     conversion

 Revision 1.1.2.10  2002/10/13 15:19:21  carl
   + int64 to real conversion

 Revision 1.1.2.9  2002/07/16 14:00:00  pierre
  * Use FPU trunc function for Unix systems

 Revision 1.1.2.8  2001/07/31 01:14:35  carl
 - removed FPU_IN_RTL (replaced by ifdef linux)

 Revision 1.1.2.7  2001/07/30 12:16:43  pierre
  * use fintrz for m68k linux as long as the generic code fails

 Revision 1.1.2.6  2001/07/30 00:02:00  carl
 + new file (currently almost empty)

}
