{
    $Id: system.inc,v 1.1.2.19 2002/10/13 15:18:55 carl Exp $

    This file is part of the Free Pascal Run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    For details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{****************************************************************************
                                Local types
****************************************************************************}

{
  TextRec and FileRec are put in a separate file to make it available to other
  units without putting it explicitly in systemh.
  This way we keep TP compatibility, and the TextRec definition is available
  for everyone who needs it.
}
{$i filerec.inc}
{$i textrec.inc}

Procedure HandleError (Errno : Longint); forward;
Procedure HandleErrorFrame (Errno : longint;frame : longint); forward;

type
  FileFunc = Procedure(var t : TextRec);

{ These types are used throughout FPU routines }
type
  float32 = longword;
{$ifdef ENDIAN_LITTLE}
  float64 = packed record
    low: longword;
    high: longword;
  end;
{$else}
  float64 = packed record
    high: longword;
    low: longword;
  end;
{$endif}
  flag = byte;
  int64rec = float64;


const
  STACK_MARGIN = 16384;    { Stack size margin for stack checking }
{ Random / Randomize constants }
  OldRandSeed : Cardinal = 0;
  InitialSeed : Boolean = TRUE;
  Seed2 : Cardinal = 0;
  Seed3 : Cardinal = 0;

{ For Error Handling.}
  ErrorBase : Longint = 0;

{ Used by the ansistrings and maybe also other things in the future }
var
  emptychar : char;public name 'FPC_EMPTYCHAR';


{****************************************************************************
                     Routines which have compiler magic
****************************************************************************}

{$I innr.inc}

Function  lo(i : Integer) : byte;  [INTERNPROC: In_lo_Word];
Function  lo(w : Word) : byte;     [INTERNPROC: In_lo_Word];
Function  lo(l : Longint) : Word;  [INTERNPROC: In_lo_long];
Function  lo(l : DWord) : Word;    [INTERNPROC: In_lo_long];
Function  hi(i : Integer) : byte;  [INTERNPROC: In_hi_Word];
Function  hi(w : Word) : byte;     [INTERNPROC: In_hi_Word];
Function  hi(l : Longint) : Word;  [INTERNPROC: In_hi_long];
Function  hi(l : DWord) : Word;    [INTERNPROC: In_hi_long];

{$ifdef INT64}
Function  lo(q : QWord) : DWord;  [INTERNPROC: In_lo_qword];
Function  lo(i : Int64) : DWord;  [INTERNPROC: In_lo_qword];
Function  hi(q : QWord) : DWord;  [INTERNPROC: In_hi_qword];
Function  hi(i : Int64) : DWord;  [INTERNPROC: In_hi_qword];
{$endif}

Function chr(b : byte) : Char;      [INTERNPROC: In_chr_byte];
Function Length(s : string) : byte; [INTERNPROC: In_Length_string];
Function Length(c : char) : byte;   [INTERNPROC: In_Length_string];

Procedure Reset(var f : TypedFile);   [INTERNPROC: In_Reset_TypedFile];
Procedure Rewrite(var f : TypedFile); [INTERNPROC: In_Rewrite_TypedFile];


{****************************************************************************
                    Include processor specific routines
****************************************************************************}

{$IFDEF I386}
  {$IFNDEF ENDIAN_LITTLE}
    {$DEFINE ENDIAN_LITTLE}
  {$ENDIF}
  {$IFDEF M68K}
    {$Error Can't determine processor type !}
  {$ENDIF}
  {$I i386.inc}  { Case dependent, don't change }
{$ELSE}
  {$IFDEF M68K}
  {$IFNDEF ENDIAN_BIG}
    {$DEFINE ENDIAN_BIG}
  {$ENDIF}
    {$I m68k.inc}  { Case dependent, don't change }
  {$ELSE}
    {$Error Can't determine processor type !}
  {$ENDIF}
{$ENDIF}

{ Include generic pascal only routines which are not defined in the processor
  specific include file }
{$I generic.inc}


{****************************************************************************
                                Set Handling
****************************************************************************}

{ Include set support which is processor specific}
{$I set.inc}
{ Include generic pascal routines for sets if the processor }
{ specific routines are not available.                      }
{$I genset.inc}


{****************************************************************************
                               Math Routines
****************************************************************************}

{$ifndef RTLLITE}

function Hi(b : byte): byte;
begin
   Hi := b shr 4
end;

function Lo(b : byte): byte;
begin
   Lo := b and $0f
end;

Function swap (X : Word) : Word;[internconst:in_const_swap_word];
Begin
  swap:=(X and $ff) shl 8 + (X shr 8)
End;

Function Swap (X : Integer) : Integer;[internconst:in_const_swap_word];
Begin
  swap:=(X and $ff) shl 8 + (X shr 8)
End;

Function swap (X : Longint) : Longint;[internconst:in_const_swap_long];
Begin
  Swap:=(X and $ffff) shl 16 + (X shr 16)
End;

Function Swap (X : Cardinal) : Cardinal;[internconst:in_const_swap_long];
Begin
  Swap:=(X and $ffff) shl 16 + (X shr 16)
End;

{$ifdef INT64}
Function Swap (X : QWord) : QWord;
Begin
  Swap:=(X and $ffffffff) shl 32 + (X shr 32);
End;

Function swap (X : Int64) : Int64;
Begin
  Swap:=(X and $ffffffff) shl 32 + (X shr 32);
End;
{$endif}

{$endif RTLLITE}

{$ifdef SUPPORT_DOUBLE}
operator := (b:real48) d:double;
begin
 D:=real2double(b);
end;

{$ifdef SUPPORT_EXTENDED}
operator := (b:real48) e:extended;
begin
 e:=real2double(b);
end;
{$endif}
{$endif}


{ Include processor specific routines }
{$I math.inc}
{ Include generic version }
{$I genmath.inc}

{****************************************************************************
                  Subroutines for String handling
****************************************************************************}

{ Needs to be before RTTI handling }

{$i sstrings.inc}

{ requires sstrings.inc for initval }
{$i int64.inc}

{Requires int64.inc, since that contains the VAL functions for int64 and qword}
{$i astrings.inc}

{$ifdef haswidechar}
{$i wstrings.inc}
{$endif haswidechar}

{****************************************************************************
                         Run-Time Type Information (RTTI)
****************************************************************************}

{$i rtti.inc}


{****************************************************************************
                          Random function routines

        This implements a very long cycle random number generator by combining
   three independant generators.  The technique was described in the March
   1987 issue of Byte.
   Taken and modified with permission from the PCQ Pascal rtl code.
****************************************************************************}

{$R-}
{$Q-}

Procedure NewSeed;Forward;


Function Random : Extended;
begin
    if (InitialSeed) OR (RandSeed <> OldRandSeed) then
    Begin
    { This is a pretty complicated affair                             }
    {  Initially we must call NewSeed when RandSeed is initalized     }
    {  We must also call NewSeed each time RandSeed is reinitialized  }
    {  DO NOT CHANGE THE ORDER OF DECLARATIONS IN THIS BLOCK          }
    {  UNLESS YOU WANT RANDON TO CRASH OF COURSE (CEC)                }
      InitialSeed:=FALSE;
      OldRandSeed:=RandSeed;
      NewSeed;
    end;
    Inc(RandSeed);
    RandSeed := (RandSeed * 706) mod 500009;
    OldRandSeed:=RandSeed;
    INC(Seed2);
    Seed2 := (Seed2 * 774) MOD 600011;
    INC(Seed3);
    Seed3 := (Seed3 * 871) MOD 765241;
    Random :=
      frac(RandSeed/500009.0 +
           Seed2/600011.0 +
           Seed3/765241.0);
end;

Function internRandom(l : Cardinal) : Cardinal;
begin
    if (InitialSeed) OR (RandSeed <> OldRandSeed) then
      Begin
      { This is a pretty complicated affair                             }
      {  Initially we must call NewSeed when RandSeed is initalized     }
      {  We must also call NewSeed each time RandSeed is reinitialized  }
      {  DO NOT CHANGE THE ORDER OF DECLARATIONS IN THIS BLOCK          }
      {  UNLESS YOU WANT RANDOM TO CRASH OF COURSE (CEC)                }
        InitialSeed:=FALSE;
        OldRandSeed:=RandSeed;
        NewSeed;
      end;
    Inc(RandSeed);
    RandSeed := (RandSeed * 998) mod 1000003;
    OldRandSeed:=RandSeed;
    if l<>0 then
      begin
        internRandom := RandSeed mod l;
      end
    else internRandom:=0;
end;

function random(l:cardinal): cardinal;
begin
  random := trunc(random()*l);
end;

function random(l:longint): longint;
begin
  random := trunc(random()*l);
end;


Procedure NewSeed;
begin
    randseed := randseed mod 1000003;
    Seed2 := (internRandom(65000) * internRandom(65000)) mod 600011;
    Seed3 := (internRandom(65000) * internRandom(65000)) mod 765241;
end;

{****************************************************************************
                            Memory Management
****************************************************************************}

{$ifndef RTLLITE}

Function Ptr(sel,off : Longint) : pointer;[internconst:in_const_ptr];
Begin
  sel:=0;
  ptr:=pointer(off);
End;

Function CSeg : Word;
Begin
  Cseg:=0;
End;

Function DSeg : Word;
Begin
  Dseg:=0;
End;

Function SSeg : Word;
Begin
  Sseg:=0;
End;

{$endif RTLLITE}


{*****************************************************************************
                             Directory support.
*****************************************************************************}

Procedure getdir(drivenr:byte;Var dir:ansistring);
{ this is needed to also allow ansistrings, the shortstring version is
  OS dependent }
var
  s : shortstring;
begin
  getdir(drivenr,s);
  dir:=s;
end;

{$ifopt R+}
{$define RangeCheckWasOn}
{$R-}
{$endif opt R+}

{$ifopt I+}
{$define IOCheckWasOn}
{$I-}
{$endif opt I+}

{$ifopt Q+}
{$define OverflowCheckWasOn}
{$Q-}
{$endif opt Q+}

{*****************************************************************************
                             Miscellaneous
*****************************************************************************}

procedure int_rangeerror;[public,alias:'FPC_RANGEERROR'];
begin
  HandleErrorFrame(201,get_frame);
end;


procedure int_overflow;[public,alias:'FPC_OVERFLOW'];
begin
  HandleErrorFrame(215,get_frame);
end;


procedure int_iocheck(addr : longint);[saveregisters,public,alias:'FPC_IOCHECK'];
var
  l : longint;
begin
  if InOutRes<>0 then
   begin
     l:=InOutRes;
     InOutRes:=0;
     HandleErrorFrame(l,get_frame);
   end;
end;


Function IOResult:Word;
Begin
  IOResult:=InOutRes;
  InOutRes:=0;
End;


procedure fillchar(var x;count : longint;value : boolean);
begin
  fillchar(x,count,byte(value));
end;


procedure fillchar(var x;count : longint;value : char);
begin
  fillchar(x,count,byte(value));
end;


{*****************************************************************************
                        Initialization / Finalization
*****************************************************************************}

const
  maxunits=1024; { See also files.pas of the compiler source }
type
  TInitFinalRec=record
    InitProc,
    FinalProc : TProcedure;
  end;
  TInitFinalTable=record
    TableCount,
    InitCount  : longint;
    Procs      : array[1..maxunits] of TInitFinalRec;
  end;

var
  InitFinalTable : TInitFinalTable;external name 'INITFINAL';

procedure InitializeUnits;[public,alias:'FPC_INITIALIZEUNITS'];
var
  i : longint;
begin
  with InitFinalTable do
   begin
     for i:=1to TableCount do
      begin
        if assigned(Procs[i].InitProc) then
         Procs[i].InitProc();
        InitCount:=i;
      end;
   end;
end;


procedure FinalizeUnits;[public,alias:'FPC_FINALIZEUNITS'];
begin
  with InitFinalTable do
   begin
     while (InitCount>0) do
      begin
        // we've to decrement the cound before calling the final. code
        // else a halt in the final. code leads to a endless loop
        dec(InitCount);
        if assigned(Procs[InitCount+1].FinalProc) then
         Procs[InitCount+1].FinalProc();
      end;
   end;
end;


{*****************************************************************************
                          Error / Exit / ExitProc
*****************************************************************************}

Procedure system_exit;forward;

Procedure InternalExit;
var
  current_exit : Procedure;
Begin
  while exitProc<>nil Do
   Begin
     InOutRes:=0;
     current_exit:=tProcedure(exitProc);
     exitProc:=nil;
     current_exit();
   End;
  { Finalize units }
  FinalizeUnits;
  { Show runtime error and exit }
  If erroraddr<>nil Then
   Begin
     Writeln(stdout,'Runtime error ',Errorcode,' at 0x',hexstr(Longint(Erroraddr),8));
     { to get a nice symify }
     Writeln(stdout,BackTraceStrFunc(Longint(Erroraddr)));
     dump_stack(stdout,ErrorBase);
     Writeln(stdout,'');
   End;
End;


Procedure do_exit;[Public,Alias:'FPC_DO_EXIT'];
begin
  InternalExit;
  System_exit;
end;


Procedure lib_exit;saveregisters;[Public,Alias:'FPC_LIB_EXIT'];
begin
  InternalExit;
end;


Procedure Internal_Break_Error(Errno : longint;addr : pointer; frame : longint);[public,alias:'FPC_BREAK_ERROR'];
begin
  exitcode:=Errno;
  erroraddr:=addr;
  errorbase:=frame;
  Do_exit;
end;

Procedure Halt(ErrNum: Byte);[alias:'FPC_HALT'];
Begin
  Internal_Break_Error(ErrNum,erroraddr,errorbase);
end;


function SysBackTraceStr (Addr: longint): ShortString;
begin
  SysBackTraceStr:='  0x'+HexStr(addr,8);
end;


Procedure HandleErrorAddrFrame (Errno : longint;addr,frame : longint);
begin
  If pointer(ErrorProc)<>Nil then
    ErrorProc(Errno,pointer(addr),pointer(frame));
  errorcode:=Errno;
  Internal_Break_Error(Errno,pointer(addr),frame);
end;

Procedure HandleErrorFrame (Errno : longint;frame : longint);
{
  Procedure to handle internal errors, i.e. not user-invoked errors
  Internal function should ALWAYS call HandleError instead of RunError.
  Can be used for exception handlers to specify the frame
}
begin
  HandleErrorAddrFrame(Errno,get_caller_addr(frame),get_caller_frame(frame));
end;


Procedure HandleError (Errno : longint);[public,alias : 'FPC_HANDLEERROR'];
{
  Procedure to handle internal errors, i.e. not user-invoked errors
  Internal function should ALWAYS call HandleError instead of RunError.
}
begin
  HandleErrorFrame(Errno,get_frame);
end;


procedure runerror(w : word);[alias: 'FPC_RUNERROR'];
begin
  errorcode:=w;
  Internal_Break_Error(w,pointer(get_caller_addr(get_frame)),get_caller_frame(get_frame));
end;


Procedure RunError;[alias:'FPC_RUNERROR_ZERO'];
Begin
  errorcode:=0;
  Internal_Break_Error(0,pointer(get_caller_addr(get_frame)),get_caller_frame(get_frame));
End;


Procedure Halt;[alias:'FPC_HALT_ZERO'];
Begin
  Internal_Break_Error(0,erroraddr,errorbase);
End;
function do_isdevice(handle:longint):boolean;forward;


Procedure dump_stack(var f : text;bp : Longint);
var
  i, prevbp : Longint;
  is_dev : boolean;
  caller_addr : longint;
Begin
  prevbp:=bp-1;
  i:=0;
  is_dev:=do_isdevice(textrec(f).Handle);
  while bp > prevbp Do
   Begin
     caller_addr := get_caller_addr(bp);
     if caller_addr <> 0 then 
        Writeln(f,BackTraceStrFunc(caller_addr));
     Inc(i);
     If ((i>max_frame_dump) and is_dev) or (i>256) Then
      exit;
     prevbp:=bp;
     bp:=get_caller_frame(bp);
   End;
End;


Type
  PExitProcInfo = ^TExitProcInfo;
  TExitProcInfo = Record
    Next     : PExitProcInfo;
    SaveExit : Pointer;
    Proc     : TProcedure;
  End;
const
  ExitProcList: PExitProcInfo = nil;

Procedure DoExitProc;
var
  P    : PExitProcInfo;
  Proc : TProcedure;
Begin
  P:=ExitProcList;
  ExitProcList:=P^.Next;
  ExitProc:=P^.SaveExit;
  Proc:=P^.Proc;
  DisPose(P);
  Proc();
End;


Procedure AddExitProc(Proc: TProcedure);
var
  P : PExitProcInfo;
Begin
  New(P);
  P^.Next:=ExitProcList;
  P^.SaveExit:=ExitProc;
  P^.Proc:=Proc;
  ExitProcList:=P;
  ExitProc:=@DoExitProc;
End;


{*****************************************************************************
                          Abstract/Assert support.
*****************************************************************************}

procedure AbstractError;[public,alias : 'FPC_ABSTRACTERROR'];
begin
  If pointer(AbstractErrorProc)<>nil then
    AbstractErrorProc();
  HandleErrorFrame(211,get_frame);
end;


Procedure int_assert(Const Msg,FName:Shortstring;LineNo,ErrorAddr:Longint); [SaveRegisters,Public,Alias : 'FPC_ASSERT'];
begin
  if pointer(AssertErrorProc)<>nil then
    AssertErrorProc(Msg,FName,LineNo,ErrorAddr)
  else
    HandleErrorFrame(227,get_frame);
end;


Procedure SysAssert(Const Msg,FName:Shortstring;LineNo,ErrorAddr:Longint);
begin
  If msg='' then
    write(stderr,'Assertion failed')
  else
    write(stderr,msg);
  Writeln(stderr,' (',FName,', line ',LineNo,').');
  Writeln(stderr,'');
  Halt(227);
end;


{*****************************************************************************
                       SetJmp/LongJmp support.
*****************************************************************************}

{$i setjump.inc}


{$ifdef IOCheckWasOn}
{$I+}
{$endif}

{$ifdef RangeCheckWasOn}
{$R+}
{$endif}

{$ifdef OverflowCheckWasOn}
{$Q+}
{$endif}

{*****************************************************************************
                        Object Pascal support
*****************************************************************************}

{$i objpas.inc}

{
  $Log: system.inc,v $
  Revision 1.1.2.19  2002/10/13 15:18:55  carl
    * fixes for int64 to real conversion

  Revision 1.1.2.18  2002/10/09 08:57:34  pierre
   * operator := from real48 to extended only if SUPPORT_EXTENDED is defined

  Revision 1.1.2.17  2002/09/18 18:38:12  carl
    * assert now halts with exitcode 227 (as Delphi does)

  Revision 1.1.2.16  2001/12/14 03:39:20  carl
  + added ifdfef support_double, since we don't support double under m68k!

  Revision 1.1.2.15  2001/12/13 20:22:05  michael
  + Added double2real function from main branch

  Revision 1.1.2.14  2001/11/19 02:37:45  carl
  + don't print stack information if previous frame = 0

  Revision 1.1.2.13  2001/07/29 23:59:10  carl
  - removed ifdef m68k
  + added inclusion of generic mathematical routines

  Revision 1.1.2.12  2001/07/29 13:03:42  carl
  + include lowmath.inc

  Revision 1.1.2.11  2001/07/21 19:19:20  carl
  + added STACK_MARGIN constant

  Revision 1.1.2.10  2001/06/11 02:26:52  carl
  + added endian defines if they don't exist to be able to compile the RTL with FPC v1.0.4

  Revision 1.1.2.9  2001/06/03 15:21:05  peter
    * lib_exit added

  Revision 1.1.2.8  2001/05/09 03:28:14  carl
  + Portable and generic set routines

  Revision 1.1.2.7  2001/04/22 00:42:19  carl
  - removed HASSAVEREGISTERS define

  Revision 1.1.2.6  2001/04/05 15:34:50  pierre
   + Internal_Error_Break to get uniform catch of exit with error for IDE

  Revision 1.1.2.5  2000/12/16 15:56:18  jonas
    - removed all ifdef cardinalmulfix code

  Revision 1.1.2.4  2000/12/06 16:46:30  jonas
    * backported range checking fixes from 1.1 (added range checking for
      conversion between cardinal and longint and for conversion from
      64bit to 32bit types)

  Revision 1.1.2.3  2000/10/13 12:04:28  peter
    * FPC_BREAK_ERROR added

  Revision 1.1.2.2  2000/08/13 19:34:33  peter
    * delphi compatible types added

  Revision 1.1.2.1  2000/08/10 18:53:35  peter
    * int64 ansistring val support

  Revision 1.1  2000/07/13 06:30:49  michael
  + Initial import

  Revision 1.89  2000/05/23 20:35:41  pierre
   * disable check in Exit and Error code

  Revision 1.88  2000/04/24 11:11:50  peter
    * backtraces for exceptions are now only generated from the place of the
      exception
    * frame is also pushed for exceptions
    * raise statement enhanced with [,<frame>]

  Revision 1.87  2000/04/14 12:17:12  pierre
   + get longer backtrace when redirected to file

  Revision 1.86  2000/04/02 09:39:25  florian
    * halt in the finalization statement of a unit lead to an endless loop; fixed

  Revision 1.85  2000/03/14 07:31:57  pierre
   + HandleErrorAddrFrame

  Revision 1.84  2000/02/26 15:49:40  jonas
    + new str_real which is completely TP compatible regarding output
      format and which should have no rounding errors anymore

  Revision 1.83  2000/02/09 16:59:31  peter
    * truncated log

  Revision 1.82  2000/02/09 12:17:51  peter
    * moved halt to system.inc
    * syslinux doesn't use direct asm anymore

  Revision 1.81  2000/02/06 17:19:22  peter
    * lineinfo unit added which uses stabs to get lineinfo for backtraces

  Revision 1.80  2000/01/10 09:54:30  peter
    * primitives added

  Revision 1.79  2000/01/07 16:41:36  daniel
    * copyright 2000

  Revision 1.78  2000/01/07 16:32:25  daniel
    * copyright 2000 added

  Revision 1.77  1999/12/21 11:10:22  pierre
   * allow v09912 to compile system

  Revision 1.76  1999/12/18 14:54:34  florian
    * very basic widestring support

  Revision 1.75  1999/12/12 13:29:34  jonas
    * remove "random(longint): longint" if cardinalmulfixed is defined

  Revision 1.74  1999/12/01 12:37:13  jonas
    + function random(longint): longint

  Revision 1.73  1999/11/20 12:48:09  jonas
    * reinstated old random generator, but modified it so the integer
      one now has a much longer period

  Revision 1.72  1999/11/15 21:49:47  peter
    * exception address fixes

  Revision 1.71  1999/11/09 22:40:12  pierre
   + get also first BackTrace address with '  0x' prefix

  Revision 1.70  1999/11/09 20:14:12  daniel
  * Committed new random generator.

  Revision 1.69  1999/11/06 14:35:39  peter
    * truncated log

  Revision 1.68  1999/10/26 12:31:00  peter
    * *errorproc are not procvars instead of pointers which allows better
      error checking for the parameters (shortstring<->ansistring)

  Revision 1.67  1999/09/18 16:05:12  jonas
    * dump_stack now actually dumps its info to f (was still hardcoded
      to stderr)

  Revision 1.66  1999/08/05 23:45:14  peter
    * saveregister is now working and used for assert and iocheck (which has
      been moved to system.inc because it's now system independent)

  Revision 1.65  1999/07/28 12:58:22  peter
    * fixed assert() to push/pop registers

}
