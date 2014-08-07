{
    $Id: systemh.inc,v 1.1.2.25 2002/10/22 18:52:29 carl Exp $
    This file is part of the Free Pascal Run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    This File contains the OS independent declarations of the system unit

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
   Supported conditionnals:
   ------------------------
   RTLLITE         Create a somewhat smaller RTL
}

{****************************************************************************
                             Needed switches
****************************************************************************}

{$I-,Q-,H-,R-,V-}
{$mode objfpc}

{ don't use FPU registervariables on the i386 }
{$ifdef i386}
  {$maxfpuregisters 0}
{$endif i386}

{ needed for insert,delete,readln }
{$P+}

{ Stack check gives a note under linux }
{$ifndef linux}
  {$S-}
{$endif}

{****************************************************************************
                         Global Types and Constants
****************************************************************************}

Type
  shortint = -128..127;
  SmallInt = -32768..32767;
  Longint  = $80000000..$7fffffff; { $8000000 creates a longint overfow !! }
  byte     = 0..255;
  Word     = 0..65535;
  dword    = cardinal;
  longword = cardinal;

{ at least declare Turbo Pascal real types }
{$ifdef i386}
  StrLenInt = LongInt;

  {$define DEFAULT_EXTENDED}

  {$define SUPPORT_SINGLE}
  {$define SUPPORT_DOUBLE}
  {$define SUPPORT_EXTENDED}
  {$define SUPPORT_COMP}

  { define SUPPORT_FIXED}

  {$define SUPPORT_ANSISTRING}

  ValSInt = Longint;
  ValUInt = Cardinal;
  ValReal = Extended;
{$endif}

{$ifdef m68k}
  StrLenInt = Longint;

  ValSInt = Longint;
  ValUInt = Cardinal;
  ValReal = Extended;

  { Comp type does not exist on fpu }
  Comp    = int64;
  {$define SUPPORT_SINGLE}
{$IFOPT E-}
  {$define SUPPORT_DOUBLE}
  {$define SUPPORT_EXTENDED}
  {$ifdef FPC_FPU_INTERNAL}
    {$define DEFAULT_EXTENDED}
  {$endif FPC_FPU_INTERNAL}
{$ENDIF}
{$endif}

{ Zero - terminated strings }
  PChar       = ^Char;
  PPChar      = ^PChar;

{ Delphi types }
  TAnsiChar   = Char;
  AnsiChar    = TAnsiChar;
  PAnsiChar   = PChar;
  PQWord      = ^QWord;
  PInt64      = ^Int64;

  currency            = int64;
  HRESULT             = Longint;
  TDateTime           = Double;
  Error               = Longint;

  PSmallInt           = ^Smallint;
  PInteger            = ^Longint;
  PSingle             = ^Single;
  PDouble             = ^Double;
  PCurrency           = ^Currency;
  PDate               = ^TDateTime;
  PError              = ^Error;
  PWordBool           = ^WordBool;
  PByte               = ^Byte;
  PWord    = ^word;
  PDWord   = ^DWord;
  PLongint = ^Longint;

{$ifdef HASWIDECHAR}
  PWideChar   = ^WideChar;
  PPWideChar  = ^PWideChar;
{$endif HASWIDECHAR}

{ procedure type }
  TProcedure  = Procedure;

const
{ Maximum value of the biggest signed and unsigned integer type available}
  MaxSIntValue = High(ValSInt);
  MaxUIntValue = High(ValUInt);

{ max. values for longint and int}
  maxLongint  = $7fffffff;
  maxSmallint = 32767;

{ Integer type definition }
type
  Integer  = smallint;
const
  maxint   = maxsmallint;

{ Compatibility With  TP }
const
  { code to use comps in int64mul and div code is commented out! (JM) }
  FPUInt64 : boolean = false; { set this to false if you don't want that }
                              { the fpu does int64*int64 and             }
                              { int64 div int64, if the * is overflow    }
                              { checked, it is done in software          }
{$ifdef i386}
  Test8086 : byte = 2;       { Always i386 or newer }
  Test8087 : byte = 3;       { Always 387 or newer. Emulated if needed. }
{$endif i386}
{$ifdef m68k}
  Test68000 : byte = 0;      { Must be determined at startup for both }
  Test68881 : byte = 0;
{$endif}

{ max level in dumping on error }
  Max_Frame_Dump : Word = 8;

{ Exit Procedure handling consts and types  }
  ExitProc : pointer = nil;
  Erroraddr: pointer = nil;
  Errorcode: Word    = 0;

{ file input modes }
  fmClosed = $D7B0;
  fmInput  = $D7B1;
  fmOutput = $D7B2;
  fmInOut  = $D7B3;
  fmAppend = $D7B4;
  Filemode : byte = 2;
  CmdLine : PChar = nil;

 { Delphi Compatibility }
 { assume that this program will not spawn other threads. }
  IsMultiThread : boolean = FALSE;


var
{ Standard In- and Output }
  Output,
  Input,
  StdOut,
  StdErr      : Text;
  ExitCode,
  InOutRes    : Word;
  StackBottom,
  RandSeed    : Cardinal;
{ Delphi compatible }
  IsLibrary,IsConsole : boolean;
{$ifdef SYSTEMDEBUG}
  LowestStack : Cardinal;
{$endif}



{****************************************************************************
                        Processor specific routines
****************************************************************************}

Procedure Move(const source;var dest;count:Longint);
Procedure FillChar(Var x;count:Longint;Value:Boolean);
Procedure FillChar(Var x;count:Longint;Value:Char);
Procedure FillChar(Var x;count:Longint;Value:Byte);
{$ifndef RTLLITE}
procedure FillByte(var x;count:longint;value:byte);
Procedure FillWord(Var x;count:Longint;Value:Word);
procedure FillDWord(var x;count:longint;value:DWord);
function  IndexChar(var buf;len:longint;b:char):longint;
function  IndexByte(var buf;len:longint;b:byte):longint;
function  Indexword(var buf;len:longint;b:word):longint;
function  IndexDWord(var buf;len:longint;b:DWord):longint;
function  CompareChar(var buf1,buf2;len:longint):longint;
function  CompareByte(var buf1,buf2;len:longint):longint;
function  CompareWord(var buf1,buf2;len:longint):longint;
function  CompareDWord(var buf1,buf2;len:longint):longint;
procedure MoveChar0(var buf1,buf2;len:longint);
function  IndexChar0(var buf;len:longint;b:char):longint;
function  CompareChar0(var buf1,buf2;len:longint):longint;
{$endif}


{****************************************************************************
                          Math Routines
****************************************************************************}

{$ifndef RTLLITE}
Function  lo(w:Word):byte;
Function  lo(l:Longint):Word;
Function  lo(l:DWord):Word;
Function  lo(i:Integer):byte;
Function  lo(B: Byte):Byte;
Function  hi(w:Word):byte;
Function  hi(i:Integer):byte;
Function  hi(l:Longint):Word;
Function  hi(b : Byte) : Byte;
Function  hi(l: DWord): Word;
Function  Swap (X:Word):Word;
Function  Swap (X:Integer):Integer;
Function  Swap (X:Cardinal):Cardinal;
Function  Swap (X:LongInt):LongInt;
{$ifdef INT64}
Function  lo(q : QWord) : DWord;
Function  lo(i : Int64) : DWord;
Function  hi(q : QWord) : DWord;
Function  hi(i : Int64) : DWord;
Function  Swap (X:QWord):QWord;
Function  Swap (X:Int64):Int64;
{$endif}
{$endif RTLLITE}

Function  Random(l:cardinal):cardinal;
Function  Random(l:longint):longint;
Function  Random: extended;
Procedure Randomize;

Function abs(l:Longint):Longint;
Function sqr(l:Longint):Longint;
Function odd(l:Longint):Boolean;
Function odd(l:Cardinal):Boolean;
{$ifdef int64}
Function odd(l:Int64):Boolean;
Function odd(l:QWord):Boolean;
function abs(l: Int64): Int64;
function sqr(l: Int64): Int64;
function sqr(l: QWord): QWord;
{$endif int64}

{ float math routines }
{$I mathh.inc}

{****************************************************************************
                         Addr/Pointer Handling
****************************************************************************}

{$ifndef RTLLITE}
Function  ptr(sel,off:Longint):pointer;
Function  Cseg:Word;
Function  Dseg:Word;
Function  Sseg:Word;
{$endif RTLLITE}

{****************************************************************************
                      PChar and String Handling
****************************************************************************}

function strpas(p:pchar):shortstring;
function strlen(p:pchar):longint;

{ Shortstring functions }
Function  Copy(const s:shortstring;index:StrLenInt;count:StrLenInt):shortstring;
Procedure Delete(Var s:shortstring;index:StrLenInt;count:StrLenInt);
Procedure Insert(const source:shortstring;Var s:shortstring;index:StrLenInt);
Procedure Insert(source:Char;Var s:shortstring;index:StrLenInt);
Function  Pos(const substr:shortstring;const s:shortstring):StrLenInt;
Function  Pos(C:Char;const s:shortstring):StrLenInt;
Procedure SetLength(var s:shortstring;len:StrLenInt);
Procedure SetString (Var S : Shortstring; Buf : PChar; Len : Longint);
Procedure SetString (Var S : AnsiString; Buf : PChar; Len : Longint);
Function  Length(s:string):byte;
Function  upCase(const s:shortstring):shortstring;
{$ifndef RTLLITE}
Function  lowerCase(const s:shortstring):shortstring;
{$endif}
Function  Space(b:byte):shortstring;
{$ifndef RTLLITE}
Function  hexStr(Val:Longint;cnt:byte):shortstring;
Function  OctStr(Val:Longint;cnt:byte):shortstring;
Function  binStr(Val:Longint;cnt:byte):shortstring;
{$ifdef int64}
Function  hexStr(Val:int64;cnt:byte):shortstring;
Function  OctStr(Val:int64;cnt:byte):shortstring;
Function  binStr(Val:int64;cnt:byte):shortstring;
{$endif int64}
{$endif RTLLITE}

{ Char functions }
Function  Chr(b:byte):Char;
Function  upCase(c:Char):Char;
{$ifndef RTLLITE}
Function  lowerCase(c:Char):Char;
{$endif RTLLITE}
function  copy(c:char;index : StrLenInt;count : StrLenInt): shortstring;
function  pos(const substr : shortstring;c:char): StrLenInt;
function  length(c:char):byte;


{****************************************************************************
                             AnsiString Handling
****************************************************************************}

Procedure SetLength (Var S : AnsiString; l : Longint);
Procedure UniqueString (Var S : AnsiString);
Function  Length (Const S : AnsiString) : Longint;
Function  Copy (Const S : AnsiString; Index,Size : Longint) : AnsiString;
Function  Pos (Const Substr : AnsiString; Const Source : AnsiString) : Longint;
Function  Pos (c : Char; Const s : AnsiString) : Longint;
Procedure Insert (Const Source : AnsiString; Var S : AnsiString; Index : Longint);
Procedure Delete (Var S : AnsiString; Index,Size: Longint);
Function StringOfChar(c : char;l : longint) : AnsiString;
Function  Upcase(Const S : AnsiString) : Ansistring;
Function  Lowercase(Const S : AnsiString) : Ansistring;


{****************************************************************************
                          Untyped File Management
****************************************************************************}

Procedure Assign(Var f:File;const Name:string);
Procedure Assign(Var f:File;p:pchar);
Procedure Assign(Var f:File;c:char);
Procedure Rewrite(Var f:File;l:Longint);
Procedure Rewrite(Var f:File);
Procedure Reset(Var f:File;l:Longint);
Procedure Reset(Var f:File);
Procedure Close(Var f:File);
Procedure BlockWrite(Var f:File;Const Buf;Count:Longint;Var Result:Longint);
Procedure BlockWrite(Var f:File;Const Buf;Count:Cardinal;var Result:Cardinal);
Procedure BlockWrite(Var f:File;Const Buf;Count:Word;Var Result:Word);
Procedure BlockWrite(Var f:File;Const Buf;Count:Word;Var Result:Integer);
Procedure BlockWrite(Var f:File;Const Buf;Count:Longint);
Procedure BlockRead(Var f:File;Var Buf;count:Longint;Var Result:Longint);
Procedure BlockRead(var f:File;var Buf;count:Cardinal;var Result:Cardinal);
Procedure BlockRead(Var f:File;Var Buf;count:Word;Var Result:Word);
Procedure BlockRead(Var f:File;Var Buf;count:Word;Var Result:Integer);
Procedure BlockRead(Var f:File;Var Buf;count:Longint);
Function  FilePos(Var f:File):Longint;
Function  FileSize(Var f:File):Longint;
Procedure Seek(Var f:File;Pos:Longint);
Function  EOF(Var f:File):Boolean;
Procedure Erase(Var f:File);
Procedure Rename(Var f:File;const s:string);
Procedure Rename(Var f:File;p:pchar);
Procedure Rename(Var f:File;c:char);
Procedure Truncate (Var F:File);


{****************************************************************************
                           Typed File Management
****************************************************************************}

Procedure Assign(Var f:TypedFile;const Name:string);
Procedure Assign(Var f:TypedFile;p:pchar);
Procedure Assign(Var f:TypedFile;c:char);
Procedure Rewrite(Var f:TypedFile);
Procedure Reset(Var f:TypedFile);


{****************************************************************************
                            Text File Management
****************************************************************************}

Procedure Assign(Var t:Text;const s:string);
Procedure Assign(Var t:Text;p:pchar);
Procedure Assign(Var t:Text;c:char);
Procedure Close(Var t:Text);
Procedure Rewrite(Var t:Text);
Procedure Reset(Var t:Text);
Procedure Append(Var t:Text);
Procedure Flush(Var t:Text);
Procedure Erase(Var t:Text);
Procedure Rename(Var t:Text;const s:string);
Procedure Rename(Var t:Text;p:pchar);
Procedure Rename(Var t:Text;c:char);
Function  EOF(Var t:Text):Boolean;
Function  EOF:Boolean;
Function  EOLn(Var t:Text):Boolean;
Function  EOLn:Boolean;
Function  SeekEOLn (Var t:Text):Boolean;
Function  SeekEOF (Var t:Text):Boolean;
Function  SeekEOLn:Boolean;
Function  SeekEOF:Boolean;
Procedure SetTextBuf(Var f:Text; Var Buf);
Procedure SetTextBuf(Var f:Text; Var Buf; Size:Longint);


{****************************************************************************
                            Directory Management
****************************************************************************}

Procedure chdir(const s:string);
Procedure mkdir(const s:string);
Procedure rmdir(const s:string);
Procedure getdir(drivenr:byte;Var dir:shortstring);
Procedure getdir(drivenr:byte;Var dir:ansistring);


{*****************************************************************************
                             Miscelleaous
*****************************************************************************}

{ os independent calls to allow backtraces }
function get_frame:longint;
function get_caller_addr(framebp:longint):longint;
function get_caller_frame(framebp:longint):longint;

Function IOResult:Word;
Function Sptr:Longint;


{*****************************************************************************
                          Init / Exit / ExitProc
*****************************************************************************}

Function  Paramcount:Longint;
Function  ParamStr(l:Longint):string;
{$ifndef RTLLITE}
Procedure Dump_Stack(var f : text;bp:Longint);
{$endif RTLLITE}
Procedure RunError(w:Word);
Procedure RunError;
Procedure halt(errnum:byte);
{$ifndef RTLLITE}
Procedure AddExitProc(Proc:TProcedure);
{$endif RTLLITE}
Procedure halt;


{*****************************************************************************
                         Abstract/Assert/Error Handling
*****************************************************************************}

procedure AbstractError;
Function  SysBackTraceStr(Addr: Longint): ShortString;
Procedure SysAssert(Const Msg,FName:ShortString;LineNo,ErrorAddr:Longint);

{ Error handlers }
Type
  TBackTraceStrFunc = Function (Addr: Longint): ShortString;
  TErrorProc = Procedure (ErrNo : Longint; Address,Frame : Pointer);
  TAbstractErrorProc = Procedure;
  TAssertErrorProc = Procedure(const msg,fname:ShortString;lineno,erroraddr:longint);



const
  BackTraceStrFunc  : TBackTraceStrFunc = @SysBackTraceStr;
  ErrorProc         : TErrorProc = nil;
  AbstractErrorProc : TAbstractErrorProc = nil;
  AssertErrorProc   : TAssertErrorProc = @SysAssert;


{*****************************************************************************
                          SetJmp/LongJmp
*****************************************************************************}

{$i setjumph.inc}


{*****************************************************************************
                       Object Pascal support
*****************************************************************************}

{$i objpash.inc}

{
  $Log: systemh.inc,v $
  Revision 1.1.2.25  2002/10/22 18:52:29  carl
    - Remove FPU_EMULATION (was very misleading!)

  Revision 1.1.2.24  2002/10/14 11:08:58  pierre
   * generate extended math functions for m68k if FPC_FPU_INTERNAL is defined

  Revision 1.1.2.23  2002/10/09 08:59:55  pierre
   * full m68k extended support

  Revision 1.1.2.22  2002/08/06 20:52:24  michael
    + Added support for octal strings (using &)

  Revision 1.1.2.21  2002/07/14 14:48:43  carl
    - fix compilation problems (revert back to old files)

  Revision 1.1.2.19  2002/01/21 12:33:48  jonas
    + abs(int64), sqr(int64), sqr(qword)

  Revision 1.1.2.18  2001/11/08 10:21:42  michael
  Pathdelim etc should be in sysutils. System contains other vars in system-dependent files

  Revision 1.1.2.17  2001/11/07 14:02:52  michael
  + Added PathDelim,DriveDelim,PathSep

  Revision 1.1.2.16  2001/10/09 02:47:58  carl
  * bugfix #1639 (IsMultiThread variable setting)

  Revision 1.1.2.15  2001/07/30 00:00:12  carl
  + added cechking for $E flag to see if DOUBLE should be supported for m68k

  Revision 1.1.2.14  2001/07/26 02:56:50  carl
  + only LINUX support complete FPU for m68k CPU.

  Revision 1.1.2.13  2001/07/25 16:05:31  pierre
   * define SUPPORT_ANSISTRING

  Revision 1.1.2.12  2001/07/25 16:04:39  pierre
   * define SUPPORT_DOUBLE for m68k

  Revision 1.1.2.11  2001/07/21 19:19:47  carl
  - removed unused variable

  Revision 1.1.2.10  2001/07/14 00:21:08  pierre
   * define comp as int64 for m68k cpu

  Revision 1.1.2.9  2001/06/05 16:01:18  jonas
    * backmerged lowercase/uppercase for ansistrings from 1.1 branch

  Revision 1.1.2.8  2001/06/04 11:43:18  peter
    * Formal const to var fixes
    * Hexstr(int64) added

  Revision 1.1.2.7  2001/05/16 15:45:15  jonas
    + odd() for cardinal, int64 and qword

  Revision 1.1.2.6  2001/05/08 17:41:00  carl
  * It's too complex to support DOUBLE in m68k removed

  Revision 1.1.2.5  2001/04/22 00:41:38  carl
  + m68k will support double
  + fpuint64 is now a portable constant definition

  Revision 1.1.2.4  2001/02/12 12:10:37  pierre
   + cardinal versions of blockread/blockwrite

  Revision 1.1.2.3  2000/12/16 15:56:18  jonas
    - removed all ifdef cardinalmulfix code

  Revision 1.1.2.2  2000/12/08 13:46:37  jonas
    + added pos(char,ansistring), because there is also a pos(char,shortstring)
      and without the ansistring version, the shortstring version is always
      called when calling pos(char,pchar), even when using $h+ (because the
      first parameter matches exactly)

  Revision 1.1.2.1  2000/08/13 19:34:33  peter
    * delphi compatible types added

  Revision 1.1  2000/07/13 06:30:49  michael
  + Initial import

  Revision 1.87  2000/07/07 19:22:27  pierre
   * correct last commit error

  Revision 1.86  2000/07/07 18:23:41  marco
   * Changed move (var source;var dest) to move (const source;var dest)

  Revision 1.85  2000/06/22 18:41:25  peter
    * moved islibrary,isconsole,ismulithread to systemh as they are
      os independent

  Revision 1.84  2000/06/22 18:05:56  michael
  + Modifications for exception support in sysutils. Mainly added
     RaiseList function.

  Revision 1.83  2000/06/11 07:02:30  peter
    * UniqueAnsiString -> UniqueString for Delphi compatibility

  Revision 1.82  2000/05/14 18:46:54  florian
    * TVarRec with In64/QWord stuff extended

  Revision 1.81  2000/04/24 11:11:50  peter
    * backtraces for exceptions are now only generated from the place of the
      exception
    * frame is also pushed for exceptions
    * raise statement enhanced with [,<frame>]

  Revision 1.80  2000/03/26 11:36:28  jonas
    + $maxfpuregisters 0 for i386 in systemh (to avoid requiring too much
      empty FPU registers for sysstem routines
    * fixed bug in str_real when using :x:0
    * str_real now doesn't call exp() anymore at runtime, so it should
      require less free FPU registers now (and be slightly faster)

  Revision 1.79  2000/03/14 10:20:18  michael
  + Added constants and types for Delphi compatibility

  Revision 1.78  2000/02/09 16:59:31  peter
    * truncated log

  Revision 1.77  2000/02/06 17:19:22  peter
    * lineinfo unit added which uses stabs to get lineinfo for backtraces

  Revision 1.76  2000/01/21 15:32:07  jonas
    * set FPUInt64 to false for i386, because comp mul and div code for int64 is
      commented out in int64.inc

  Revision 1.75  2000/01/10 09:54:30  peter
    * primitives added

  Revision 1.74  2000/01/07 16:41:36  daniel
    * copyright 2000

  Revision 1.73  2000/01/07 16:32:25  daniel
    * copyright 2000 added

  Revision 1.72  1999/12/20 11:20:14  peter
    + smallint, maxsmallint
    * integer is redefined as smallint

  Revision 1.71  1999/12/18 14:55:05  florian
    * very basic widestring support

  Revision 1.70  1999/12/12 13:29:34  jonas
    * remove "random(longint): longint" if cardinalmulfixed is defined

  Revision 1.69  1999/12/01 12:37:13  jonas
    + function random(longint): longint

  Revision 1.68  1999/11/25 13:34:57  michael
  + Added Ansistring setstring call

  Revision 1.67  1999/11/20 12:48:09  jonas
    * reinstated old random generator, but modified it so the integer
      one now has a much longer period

  Revision 1.66  1999/11/09 20:14:12  daniel
  * Committed new random generator.

  Revision 1.65  1999/11/06 14:35:39  peter
    * truncated log

  Revision 1.64  1999/10/27 14:19:10  florian
    + StringOfChar

  Revision 1.63  1999/10/26 12:31:00  peter
    * *errorproc are not procvars instead of pointers which allows better
      error checking for the parameters (shortstring<->ansistring)

  Revision 1.62  1999/08/19 11:16:13  peter
    * settextbuf size is now longint

}
