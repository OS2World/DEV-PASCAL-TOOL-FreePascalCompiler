{
    $Id: int64.inc,v 1.1.2.10 2002/10/12 13:20:37 carl Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    This file contains some helper routines for int64 and qword

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$Q- no overflow checking }
{$R- no range checking }

    type
       tqwordrec = packed record
{$ifdef ENDIAN_LITTLE}       
         low : dword;
         high : dword;
{$else not ENDIAN_LITTLE}
{$ifdef ENDIAN_BIG}
         high : dword;
   low  : dword;
{$else not ENDIAN_BIG}
  {$fatal I need to know the endianess for int64.inc code}
{$endif not ENDIAN_BIG}
{$endif not ENDIAN_LITTLE}
       end;

    function count_leading_zeros(q : qword) : longint;

      var
         r,i : longint;

      begin
         r:=0;
         for i:=0 to 31 do
           begin
              if (tqwordrec(q).high and ($80000000 shr i))<>0 then
                begin
                   count_leading_zeros:=r;
                   exit;
                end;
              inc(r);
           end;
         for i:=0 to 31 do
           begin
              if (tqwordrec(q).low and ($80000000 shr i))<>0 then
                begin
                   count_leading_zeros:=r;
                   exit;
                end;
              inc(r);
           end;
         count_leading_zeros:=r;
      end;

    function divqword(n,z : qword) : qword;[public,alias: 'FPC_DIV_QWORD'];

      var
         shift,lzz,lzn : longint;
         { one : qword; }

      begin
         divqword:=0;
         if n=0 then
           HandleErrorFrame(200,get_frame);
         lzz:=count_leading_zeros(z);
         lzn:=count_leading_zeros(n);
         { if the denominator contains less zeros }
         { then the numerator                     }
         { the d is greater than the n            }
         if lzn<lzz then
           exit;
         shift:=lzn-lzz;
         n:=n shl shift;
         repeat
           if z>=n then
             begin
                z:=z-n;
                divqword:=divqword+(qword(1) shl shift);
             end;
           dec(shift);
           n:=n shr 1;
         until shift<0;
      end;

    function modqword(n,z : qword) : qword;[public,alias: 'FPC_MOD_QWORD'];

      var
         shift,lzz,lzn : longint;

      begin
         modqword:=0;
         if n=0 then
           HandleErrorFrame(200,get_frame);
         lzz:=count_leading_zeros(z);
         lzn:=count_leading_zeros(n);
         { if the denominator contains less zeros }
         { then the numerator                     }
         { the d is greater than the n            }
         if lzn<lzz then
           begin
              modqword:=z;
              exit;
           end;
         shift:=lzn-lzz;
         n:=n shl shift;
         repeat
           if z>=n then
             z:=z-n;
           dec(shift);
           n:=n shr 1;
         until shift<0;
         modqword:=z;
      end;

    function divint64(n,z : int64) : int64;[public,alias: 'FPC_DIV_INT64'];

      var
         sign : boolean;
         q1,q2 : qword;
{$ifdef SUPPORT_COMP}
         c : comp;
{$endif}

      begin
         if n=0 then
           HandleErrorFrame(200,get_frame);
         { can the fpu do the work? }
{$ifdef support_comp}
         if fpuint64 then
           begin
              // the c:=comp(...) is necessary to shut up the compiler
              c:=comp(comp(z)/comp(n));
              divint64:=qword(c);
           end
         else
{$endif}
           begin
              sign:=false;
              if z<0 then
                begin
                   sign:=not(sign);
                   q1:=qword(-z);
                end
              else
                q1:=z;
              if n<0 then
                begin
                   sign:=not(sign);
                   q2:=qword(-n);
                end
              else
                q2:=n;

              { the div is coded by the compiler as call to divqword }
              if sign then
                divint64:=-(q1 div q2)
              else
                divint64:=q1 div q2;
           end;
      end;

    function modint64(n,z : int64) : int64;[public,alias: 'FPC_MOD_INT64'];

      var
         signed : boolean;
         r,nq,zq : qword;

      begin
         if n=0 then
           HandleErrorFrame(200,get_frame);
         if n<0 then
           begin
              nq:=-n;
              signed:=true;
           end
         else
           begin
              signed:=false;
              nq:=n;
           end;
         if z<0 then
           begin
              zq:=qword(-z);
              signed:=not(signed);
           end
         else
           zq:=z;
         r:=zq mod nq;
         if signed then
           modint64 := -int64(r)
         else
           modint64 := r;
      end;

    { multiplies two qwords
      the longbool for checkoverflow avoids a misaligned stack
    }
    function mulqword(f1,f2 : qword;checkoverflow : longbool) : qword;[public,alias: 'FPC_MUL_QWORD'];

      var
         _f1,bitpos : qword;
         l : longint;

{$ifdef i386}
         r : qword;
{$endif i386}

      begin
{$ifdef i386}
         if not(checkoverflow) then
           begin
              asm
                 movl f1+4,%edx
                 movl f2+4,%ecx
                 orl %ecx,%edx
                 movl f2,%edx
                 movl f1,%eax
                 jnz .Lqwordmultwomul
                 mull %edx
                 jmp .Lqwordmulready
              .Lqwordmultwomul:
                 imul f1+4,%edx
                 imul %eax,%ecx
                 addl %edx,%ecx
                 mull f2
                 add %ecx,%edx
              .Lqwordmulready:
                 movl %eax,r
                 movl %edx,r+4
              end;
              mulqword:=r;
           end
         else
{$endif i386}
           begin
              mulqword:=0;
              bitpos:=1;

              // store f1 for overflow checking
              _f1:=f1;

              for l:=0 to 63 do
                begin
                   if (f2 and bitpos)<>0 then
                     mulqword:=mulqword+f1;

                   f1:=f1 shl 1;
                   bitpos:=bitpos shl 1;
                end;

              { if one of the operands is greater than the result an }
              { overflow occurs                                      }
              if checkoverflow and (_f1 <> 0) and (f2 <>0) and
                 ((_f1>mulqword) or (f2>mulqword)) then
                HandleErrorFrame(215,get_frame);
           end;
      end;

    {    multiplies two int64 ....
       fpuint64 = false:
         ... using the the qword multiplication
       fpuint64 = true:
         ... using the comp multiplication
       the longbool for checkoverflow avoids a misaligned stack
     }
    function mulint64(f1,f2 : int64;checkoverflow : longbool) : int64;[public,alias: 'FPC_MUL_INT64'];

      var
         sign : boolean;
         q1,q2,q3 : qword;
{$ifdef support_comp}
         c : comp;
{$endif}

      begin
{$ifdef support_comp}
         { can the fpu do the work ? }
         if fpuint64 and not(checkoverflow) then
           begin
              // the c:=comp(...) is necessary to shut up the compiler
              c:=comp(comp(f1)*comp(f2));
              mulint64:=int64(c);
           end
         else
{$endif}
           begin
              sign:=false;
              if f1<0 then
                begin
                   sign:=not(sign);
                   q1:=qword(-f1);
                end
              else
                q1:=f1;
              if f2<0 then
                begin
                   sign:=not(sign);
                   q2:=qword(-f2);
                end
              else
                q2:=f2;
              { the q1*q2 is coded as call to mulqword }
              q3:=q1*q2;

              if checkoverflow and (q1 <> 0) and (q2 <>0) and
              ((q1>q3) or (q2>q3) or
                { the bit 63 can be only set if we have $80000000 00000000 }
                { and sign is true                                         }
                ((tqwordrec(q3).high and $80000000)<>0) and
                 ((q3<>(qword(1) shl 63)) or not(sign))
                ) then
                HandleErrorFrame(215,get_frame);

              if sign then
                mulint64:=-q3
              else
                mulint64:=q3;
           end;
      end;

    procedure qword_str(value : qword;var s : string);

      var
         hs : string;

      begin
         hs:='';
         repeat
           hs:=chr(longint(value mod qword(10))+48)+hs;
           value:=value div qword(10);
         until value=0;
         s:=hs;
      end;

    procedure int64_str(value : int64;var s : string);

      var
         hs : string;
         q : qword;

      begin
         if value<0 then
           begin
              q:=qword(-value);
              qword_str(q,hs);
              s:='-'+hs;
           end
         else
           qword_str(qword(value),s);
      end;

  procedure int_str_qword(v : qword;len : longint;var s : shortstring);[public,alias:'FPC_SHORTSTR_QWORD'];

    begin
       qword_str(v,s);
        if length(s)<len then
          s:=space(len-length(s))+s;
    end;

  procedure int_str_int64(v : int64;len : longint;var s : shortstring);[public,alias:'FPC_SHORTSTR_INT64'];

    begin
       int64_str(v,s);
       if length(s)<len then
         s:=space(len-length(s))+s;
    end;

  procedure int_str_qword(v : qword;len : longint;var s : ansistring);[public,alias:'FPC_ANSISTR_QWORD'];

    var
       ss : shortstring;

    begin
       int_str_qword(v,len,ss);
       s:=ss;
    end;

  procedure int_str_int64(v : int64;len : longint;var s : ansistring);[public,alias:'FPC_ANSISTR_INT64'];

    var
       ss : shortstring;

    begin
       int_str_int64(v,len,ss);
       s:=ss;
    end;

  Function ValInt64(Const S: ShortString; var Code: ValSInt): Int64; [public, alias:'FPC_VAL_INT64_SHORTSTR'];
   type
     QWordRec = packed record
       l1,l2: longint;
     end;

    var
       u, temp, prev, maxint64, maxqword : qword;
       base : byte;
       negative : boolean;

  begin
    ValInt64 := 0;
    Temp:=0;
    Code:=InitVal(s,negative,base);
    if Code>length(s) then
     exit;
    { high(int64) produces 0 in version 1.0 (JM) }
    with qwordrec(maxint64) do
      begin
        l1 := $ffffffff;
        l2 := $7fffffff;
      end;
    with qwordrec(maxqword) do
      begin
        l1 := $ffffffff;
        l2 := $ffffffff;
      end;

    while Code<=Length(s) do
     begin
       case s[Code] of
         '0'..'9' : u:=Ord(S[Code])-Ord('0');
         'A'..'F' : u:=Ord(S[Code])-(Ord('A')-10);
         'a'..'f' : u:=Ord(S[Code])-(Ord('a')-10);
       else
        u:=16;
       end;
       Prev:=Temp;
       Temp:=Temp*Int64(base);
     If (u >= base) or
        ((base = 10) and
         (maxint64-temp+ord(negative) < u)) or
        ((base <> 10) and
         (qword(maxqword-temp) < u)) or
        (prev > maxqword div qword(base)) Then
       Begin
         ValInt64 := 0;
         Exit
       End;
       Temp:=Temp+u;
       inc(code);
     end;
    code:=0;
    ValInt64:=int64(Temp);
    If Negative Then
      ValInt64:=-ValInt64;
  end;


  Function ValQWord(Const S: ShortString; var Code: ValSInt):QWord;[public,alias:'FPC_VAL_QWORD_SHORTSTR'];
    type qwordrec = packed record
      l1,l2: longint;
    end;
    var
       u, prev, maxqword: QWord;
       base : byte;
       negative : boolean;
  begin
    ValQWord:=0;
    Code:=InitVal(s,negative,base);
    If Negative or (Code>length(s)) Then
      Exit;
    with qwordrec(maxqword) do
      begin
        l1 := $ffffffff;
        l2 := $ffffffff;
      end;
    while Code<=Length(s) do
     begin
       case s[Code] of
         '0'..'9' : u:=Ord(S[Code])-Ord('0');
         'A'..'F' : u:=Ord(S[Code])-(Ord('A')-10);
         'a'..'f' : u:=Ord(S[Code])-(Ord('a')-10);
       else
        u:=16;
       end;
       prev := ValQWord;
       If (u>=base) or
         ((QWord(maxqword-u) div QWord(base))<prev) then
         Begin
           ValQWord := 0;
           Exit
         End;
       ValQWord:=ValQWord*QWord(base)+u;
       inc(code);
     end;
    code := 0;
  end;

{
  $Log: int64.inc,v $
  Revision 1.1.2.10  2002/10/12 13:20:37  carl
    * use correct defines

  Revision 1.1.2.9  2002/10/09 11:08:00  pierre
   * respect compiler endianess for qword record

  Revision 1.1.2.8  2001/04/22 00:39:11  carl
  + added $ifdef support_comp to make it portable

  Revision 1.1.2.7  2001/03/03 12:39:09  jonas
    * fixed qword_str for values with bit 63 = 1

  Revision 1.1.2.6  2000/12/09 22:54:06  florian
    * helper name for qword-val fixed

  Revision 1.1.2.5  2000/12/09 22:37:31  florian
    * fixed last commit, under go32v2 it worked

  Revision 1.1.2.4  2000/12/09 20:49:33  florian
    * val for dword and qword didn't handle the max values
      correctly
    * val for qword works again
    + val with int64/qword and ansistring implemented

  Revision 1.1.2.3  2000/11/17 16:56:42  jonas
    * fixed bug for val when processing -2147483648 and low(int64)

  Revision 1.1.2.2  2000/07/28 12:19:21  jonas
    * fixed web bug1069
    * fixed similar (and other) problems in val() for int64 and qword

  Revision 1.1.2.1  2000/07/16 08:55:26  jonas
    * fixed false overflow error if one of the operands of an int64/qword
      multiplication is 0

  Revision 1.1  2000/07/13 06:30:47  michael
  + Initial import

  Revision 1.20  2000/03/17 21:27:56  jonas
    * fixed declaration of val_int64 (removed destsize parameter)
    * fixed val_int64 and val_qword so they reject invalid input
      (u >= base)
    * when reading a number, invalid input is removed from the input
      buffer (+ it should be faster as well)

  Revision 1.19  2000/02/09 22:19:24  florian
    + helper routine for <int64> mod <in64> added

  Revision 1.18  2000/02/09 16:59:30  peter
    * truncated log

  Revision 1.17  2000/01/27 15:43:02  florian
    * improved qword*qword code, if no overflow checking is done

  Revision 1.16  2000/01/23 12:27:39  florian
    * int64/int64 and int64*int64 is now done by the fpu if possible

  Revision 1.15  2000/01/23 12:22:37  florian
    * reading of 64 bit type implemented

  Revision 1.14  2000/01/07 16:41:34  daniel
    * copyright 2000
}
