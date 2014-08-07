{
    $Id: dos.pp,v 1.1.2.10 2003/01/19 14:56:58 carl Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998-2001 by Nils Sjoholm and Carl Eric Codere
    members of the Free Pascal development team
      Date conversion routine taken from SWAG

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Unit Dos;


{--------------------------------------------------------------------}
{ LEFT TO DO:                                                        }
{--------------------------------------------------------------------}
{ o DiskFree / Disksize don't work as expected                       }
{ o Implement EnvCount,EnvStr                                        }
{ o FindFirst should only work with correct attributes               }
{--------------------------------------------------------------------}




Interface


Const
  {Bitmasks for CPU Flags}
  fcarry     = $0001;
  fparity    = $0004;
  fauxiliary = $0010;
  fzero      = $0040;
  fsign      = $0080;
  foverflow  = $0800;

  {Bitmasks for file attribute}
  readonly  = $01;
  hidden    = $02;
  sysfile   = $04;
  volumeid  = $08;
  directory = $10;
  archive   = $20;
  anyfile   = $3F;

  {File Status}
  fmclosed = $D7B0;
  fminput  = $D7B1;
  fmoutput = $D7B2;
  fminout  = $D7B3;


Type
  ComStr  = String[255];  { size increased to be more compatible with Unix}
  PathStr = String[255];  { size increased to be more compatible with Unix}
  DirStr  = String[255];  { size increased to be more compatible with Unix}
  NameStr = String[255];  { size increased to be more compatible with Unix}
  ExtStr  = String[255];  { size increased to be more compatible with Unix}



{
  filerec.inc contains the definition of the filerec.
  textrec.inc contains the definition of the textrec.
  It is in a separate file to make it available in other units without
  having to use the DOS unit for it.
}
{$i filerec.inc}
{$i textrec.inc}


Type

  SearchRec = Packed Record
    { watch out this is correctly aligned for all processors }
    { don't modify.                                          }
    { Replacement for Fill }
{0} AnchorPtr : Pointer;    { Pointer to the Anchorpath structure }
{4} Fill: Array[1..15] of Byte; {future use}
    {End of replacement for fill}
    Attr : BYTE;        {attribute of found file}
    Time : LongInt;     {last modify date of found file}
    Size : LongInt;     {file size of found file}
    Name : String[255]; {name of found file}
  End;


  DateTime = packed record
    Year: Word;
    Month: Word;
    Day: Word;
    Hour: Word;
    Min: Word;
    Sec: word;
  End;

  registers = packed record
    case i : integer of
     0 : (ax,f1,bx,f2,cx,f3,dx,f4,bp,f5,si,f51,di,f6,ds,f7,es,f8,flags,fs,gs : word);
     1 : (al,ah,f9,f10,bl,bh,f11,f12,cl,ch,f13,f14,dl,dh : byte);
     2 : (eax,  ebx,  ecx,  edx,  ebp,  esi,  edi : longint);
    end;


Var
  DosError : integer;

{Interrupt}
Procedure Intr(intno: byte; var regs: registers);
Procedure MSDos(var regs: registers);

{Info/Date/Time}
Function  DosVersion: Word;
Procedure GetDate(var year, month, mday, wday: word);
Procedure GetTime(var hour, minute, second, sec100: word);
procedure SetDate(year,month,day: word);
Procedure SetTime(hour,minute,second,sec100: word);
Procedure UnpackTime(p: longint; var t: datetime);
Procedure PackTime(var t: datetime; var p: longint);

{Exec}
Procedure Exec(const path: pathstr; const comline: comstr);
Function  DosExitCode: word;

{Disk}
Function  DiskFree(drive: byte) : longint;
Function  DiskSize(drive: byte) : longint;
Procedure FindFirst(path: pathstr; attr: word; var f: searchRec);
Procedure FindNext(var f: searchRec);
Procedure FindClose(Var f: SearchRec);

{File}
Procedure GetFAttr(var f; var attr: word);
Procedure GetFTime(var f; var time: longint);
Function  FSearch(path: pathstr; dirlist: string): pathstr;
Function  FExpand(const path: pathstr): pathstr;
Procedure FSplit(path: pathstr; var dir: dirstr; var name: namestr; var ext: extstr);

{Environment}
Function  EnvCount: longint;
Function  EnvStr(index: integer): string;
Function  GetEnv(envvar: string): string;

{Misc}
Procedure SetFAttr(var f; attr: word);
Procedure SetFTime(var f; time: longint);
Procedure GetCBreak(var breakvalue: boolean);
Procedure SetCBreak(breakvalue: boolean);
Procedure GetVerify(var verify: boolean);
Procedure SetVerify(verify: boolean);

{Do Nothing Functions}
Procedure SwapVectors;
Procedure GetIntVec(intno: byte; var vector: pointer);
Procedure SetIntVec(intno: byte; vector: pointer);
Procedure Keep(exitcode: word);

implementation

const
  DaysPerMonth :  Array[1..12] of ShortInt =
(031,028,031,030,031,030,031,031,030,031,030,031);
  DaysPerYear  :  Array[1..12] of Integer  =
(031,059,090,120,151,181,212,243,273,304,334,365);
  DaysPerLeapYear :    Array[1..12] of Integer  =
(031,060,091,121,152,182,213,244,274,305,335,366);
  SecsPerYear      : LongInt  = 31536000;
  SecsPerLeapYear  : LongInt  = 31622400;
  SecsPerDay       : LongInt  = 86400;
  SecsPerHour      : Integer  = 3600;
  SecsPerMinute    : ShortInt = 60;
  TICKSPERSECOND    = 50;



Type
    pClockData = ^tClockData;
    tClockData = packed Record
      sec   : Word;
      min   : Word;
      hour  : Word;
      mday  : Word;
      month : Word;
      year  : Word;
      wday  : Word;
    END;

    BPTR     = Longint;
    BSTR     = Longint;

  pMinNode = ^tMinNode;
  tMinNode = Packed Record
    mln_Succ,
    mln_Pred  : pMinNode;
  End;


    pMinList = ^tMinList;
    tMinList = Packed record
    mlh_Head        : pMinNode;
    mlh_Tail        : pMinNode;
    mlh_TailPred    : pMinNode;
    end;
{ *  List Node Structure.  Each member in a list starts with a Node * }

  pNode = ^tNode;
  tNode = Packed Record
    ln_Succ,                { * Pointer to next (successor) * }
    ln_Pred  : pNode;       { * Pointer to previous (predecessor) * }
    ln_Type  : Byte;
    ln_Pri   : Shortint;        { * Priority, for sorting * }
    ln_Name  : PCHAR;       { * ID string, null terminated * }
  End;  { * Note: Integer aligned * }



    pList = ^tList;
    tList = Packed record
    lh_Head     : pNode;
    lh_Tail     : pNode;
    lh_TailPred : pNode;
    lh_Type     : Byte;
    l_pad       : Byte;
    end;


   pMsgPort = ^tMsgPort;
    tMsgPort = Packed record
    mp_Node     : tNode;
    mp_Flags    : Byte;
    mp_SigBit   : Byte;     { signal bit number    }
    mp_SigTask  : Pointer;   { task to be signalled (TaskPtr) }
    mp_MsgList  : tList;     { message linked list  }
    end;


  pTask = ^tTask;
    tTask = Packed record
        tc_Node         : tNode;
        tc_Flags        : Byte;
        tc_State        : Byte;
        tc_IDNestCnt    : Shortint;         { intr disabled nesting         }
        tc_TDNestCnt    : Shortint;         { task disabled nesting         }
        tc_SigAlloc     : longint;        { sigs allocated                }
        tc_SigWait      : longint;        { sigs we are waiting for       }
        tc_SigRecvd     : longint;        { sigs we have received         }
        tc_SigExcept    : longint;        { sigs we will take excepts for }
        tc_TrapAlloc    : Word;        { traps allocated               }
        tc_TrapAble     : Word;        { traps enabled                 }
        tc_ExceptData   : Pointer;      { points to except data         }
        tc_ExceptCode   : Pointer;      { points to except code         }
        tc_TrapData     : Pointer;      { points to trap data           }
        tc_TrapCode     : Pointer;      { points to trap code           }
        tc_SPReg        : Pointer;      { stack pointer                 }
        tc_SPLower      : Pointer;      { stack lower bound             }
        tc_SPUpper      : Pointer;      { stack upper bound + 2         }
        tc_Switch       : Pointer;      { task losing CPU               }
        tc_Launch       : Pointer;      { task getting CPU              }
        tc_MemEntry     : tList;        { allocated memory              }
        tc_UserData     : Pointer;      { per task data                 }
    end;



    TDateStamp = packed record
        ds_Days         : Longint;      { Number of days since Jan. 1, 1978 }
        ds_Minute       : Longint;      { Number of minutes past midnight }
        ds_Tick         : Longint;      { Number of ticks past minute }
    end;
    PDateStamp = ^TDateStamp;



{ Returned by Examine() and ExInfo(), must be on a 4 byte boundary }

    PFileInfoBlock = ^TfileInfoBlock;
    TFileInfoBlock = packed record
        fib_DiskKey     : Longint;
        fib_DirEntryType : Longint;
                        { Type of Directory. If < 0, then a plain file.
                          If > 0 a directory }
        fib_FileName    : Array [0..107] of Char;
                        { Null terminated. Max 30 chars used for now }
        fib_Protection  : Longint;
                        { bit mask of protection, rwxd are 3-0. }
        fib_EntryType   : Longint;
        fib_Size        : Longint;      { Number of bytes in file }
        fib_NumBlocks   : Longint;      { Number of blocks in file }
        fib_Date        : TDateStamp; { Date file last changed }
        fib_Comment     : Array [0..79] of Char;
                        { Null terminated comment associated with file }
        fib_Reserved    : Array [0..35] of Char;
    end;

{ returned by Info(), must be on a 4 byte boundary }

    pInfoData = ^tInfoData;
    tInfoData = packed record
        id_NumSoftErrors        : Longint;      { number of soft errors on disk }
        id_UnitNumber           : Longint;      { Which unit disk is (was) mounted on }
        id_DiskState            : Longint;      { See defines below }
        id_NumBlocks            : Longint;      { Number of blocks on disk }
        id_NumBlocksUsed        : Longint;      { Number of block in use }
        id_BytesPerBlock        : Longint;
        id_DiskType             : Longint;      { Disk Type code }
        id_VolumeNode           : BPTR;         { BCPL pointer to volume node }
        id_InUse                : Longint;      { Flag, zero if not in use }
    end;


{ ------ Library Base Structure ---------------------------------- }
{  Also used for Devices and some Resources  }

    pLibrary = ^tLibrary;
    tLibrary = packed record
        lib_Node     : tNode;
        lib_Flags,
        lib_pad      : Byte;
        lib_NegSize,            {  number of bytes before library  }
        lib_PosSize,            {  number of bytes after library  }
        lib_Version,            {  major  }
        lib_Revision : Word;    {  minor  }
        lib_IdString : PCHAR;   {  ASCII identification  }
        lib_Sum      : LONGINT; {  the checksum itself  }
        lib_OpenCnt  : Word;    {  number of current opens  }
    end;                {  * Warning: size is not a longword multiple ! * }

    PChain = ^TChain;
    TChain = packed record
      an_Child : PChain;
      an_Parent: PChain;
      an_Lock  : BPTR;
      an_info  : TFileInfoBlock;
      an_Flags : shortint;
      an_string: Array[0..0] of char;
    end;


    PAnchorPath = ^TAnchorPath;
    TAnchorPath = packed record
       ap_Base      : PChain;     {* pointer to first anchor *}
       ap_First     : PChain;     {* pointer to last anchor *}
       ap_BreakBits : LONGINT;    {* Bits we want to break on *}
       ap_FondBreak : LONGINT;    {* Bits we broke on. Also returns ERROR_BREAK *}
       ap_Flags     : shortint;   {* New use for extra word. *}
       ap_reserved  : BYTE;
       ap_StrLen    : WORD;
       ap_Info      : TFileInfoBlock;
       ap_Buf       : Array[0..0] of Char; {* Buffer for path name, allocated by user *}
    END;

    pCommandLineInterface = ^TCommandLineInterface;
    TCommandLineInterface = packed record
      cli_result2     : longint;    {* Value of IoErr from last command   *}
      cli_SetName     : BSTR;       {* Name of current directory             *}
      cli_CommandDir  : BPTR;       {* Head of the path locklist             *}
      cli_ReturnCode  : longint;    {* Return code from last command          *}
      cli_CommandName : BSTR;       {* Name of current command              *}
      cli_FailLevel   : longint;    {* Fail level (set by FAILAT)            *}
      cli_Prompt      : BSTR;       {* Current prompt (set by PROMPT)     *}
      cli_StandardInput: BPTR;      {* Default (terminal) CLI input       *}
      cli_CurrentInput : BPTR;      {* Current CLI input                       *}
      cli_CommandFile  : BSTR;      {* Name of EXECUTE command file       *}
      cli_Interactive  : longint;   {* Boolean; True if prompts required  *}
      cli_Background   : longint;   {* Boolean; True if CLI created by RUN*}
      cli_CurrentOutput: BPTR;      {* Current CLI output                   *}
      cli_DefautlStack : longint;   {* Stack size to be obtained in long words *}
      cli_StandardOutput : BPTR;    {* Default (terminal) CLI output          *}
      cli_Module       : BPTR;      {* SegList of currently loaded command*}
    END;

  pDosList = ^tDosList;
   tDosList = packed record
    dol_Next            : BPTR;           {    bptr to next device on list }
    dol_Type            : Longint;        {    see DLT below }
    dol_Task            : Pointer;        {    ptr to handler task }
    dol_Lock            : BPTR;
    dol_Misc            : Array[0..23] of Shortint;
    dol_Name            : BSTR;           {    bptr to bcpl name }
   END;

    TProcess = packed record
        pr_Task         : TTask;
        pr_MsgPort      : TMsgPort;      { This is BPTR address from DOS functions  }
{126}   pr_Pad          : Word;         { Remaining variables on 4 byte boundaries }
{128}   pr_SegList      : Pointer;      { Array of seg lists used by this process  }
{132}   pr_StackSize    : Longint;      { Size of process stack in bytes            }
{136}   pr_GlobVec      : Pointer;      { Global vector for this process (BCPL)    }
{140}   pr_TaskNum      : Longint;      { CLI task number of zero if not a CLI      }
{144}   pr_StackBase    : BPTR;         { Ptr to high memory end of process stack  }
{148}   pr_Result2      : Longint;      { Value of secondary result from last call }
{152}   pr_CurrentDir   : BPTR;         { Lock associated with current directory   }
{156}   pr_CIS          : BPTR;         { Current CLI Input Stream                  }
{160}   pr_COS          : BPTR;         { Current CLI Output Stream                 }
{164}   pr_ConsoleTask  : Pointer;      { Console handler process for current window}
{168}   pr_FileSystemTask : Pointer;    { File handler process for current drive   }
{172}   pr_CLI          : BPTR;         { pointer to ConsoleLineInterpreter         }
        pr_ReturnAddr   : Pointer;      { pointer to previous stack frame           }
        pr_PktWait      : Pointer;      { Function to be called when awaiting msg  }
        pr_WindowPtr    : Pointer;      { Window for error printing }
        { following definitions are new with 2.0 }
        pr_HomeDir      : BPTR;         { Home directory of executing program      }
        pr_Flags        : Longint;      { flags telling dos about process          }
        pr_ExitCode     : Pointer;      { code to call on exit of program OR NULL  }
        pr_ExitData     : Longint;      { Passed as an argument to pr_ExitCode.    }
        pr_Arguments    : PChar;        { Arguments passed to the process at start }
        pr_LocalVars    : TMinList;      { Local environment variables             }
        pr_ShellPrivate : Longint;      { for the use of the current shell         }
        pr_CES          : BPTR;         { Error stream - IF NULL, use pr_COS       }
    end;
    PProcess = ^TProcess;


CONST
    { DOS Lib Offsets }
    _LVOMatchFirst = -822;
    _LVOMatchNext  = -828;
    _LVOMatchEnd   = -834;
    _LVOCli        = -492;
    _LVOExecute    = -222;
    _LVOSystemTagList = -606;
    _LVOSetFileDate = -396;

    LDF_READ   = 1;
    LDF_DEVICES = 4;

    ERROR_NO_MORE_ENTRIES            = 232;
    FIBF_SCRIPT         = 64;  { program is a script              }
    FIBF_PURE           = 32;  { program is reentrant             }
    FIBF_ARCHIVE        = 16;  { cleared whenever file is changed }
    FIBF_READ           = 8;   { ignoed by old filesystem         }
    FIBF_WRITE          = 4;   { ignored by old filesystem        }
    FIBF_EXECUTE        = 2;   { ignored by system, used by shell }
    FIBF_DELETE         = 1;   { prevent file from being deleted  }

    SHARED_LOCK         = -2;

{******************************************************************************
                           --- Internal routines ---
******************************************************************************}




function Date2Amiga(date : pClockData) : Longint;
Begin
  asm
    MOVE.L  A6,-(A7)
    MOVE.L  date,a0
    MOVE.L  _UtilityBase,A6
    JSR -126(A6)
    MOVE.L  (A7)+,A6
    MOVE.L  d0,@RESULT
  end;
end;


procedure Amiga2Date(amigatime : Longint;
                     resultat : pClockData);
Begin
  asm
    MOVE.L  A6,-(A7)
    MOVE.L  amigatime,d0
    MOVE.L  resultat,a0
    MOVE.L  _UtilityBase,A6
    JSR -120(A6)
    MOVE.L  (A7)+,A6
  end;
end;

FUNCTION Examine(lock : BPTR; fileInfoBlock : pFileInfoBlock) : BOOLEAN;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  lock,D1
    MOVE.L  fileInfoBlock,D2
    MOVEA.L _DOSBase,A6
    JSR -102(A6)
    MOVEA.L (A7)+,A6
    TST.L   D0
    BEQ.B   @end
    MOVE.B  #1,D0
    @end: MOVE.B  D0,@RESULT
  END;
END;

function Lock(const name : string;
           accessmode : Longint) : BPTR;
var
 buffer: Array[0..255] of char;
Begin
  move(name[1],buffer,length(name));
  buffer[length(name)]:=#0;
  asm
    MOVEM.L d2/a6,-(A7)
    LEA     buffer,a0
    MOVE.L  a0,d1
    MOVE.L  accessmode,d2
    MOVE.L  _DOSBase,A6
    JSR -084(A6)
    MOVEM.L (A7)+,d2/a6
    MOVE.L  d0,@RESULT
  end;
end;


procedure UnLock(lock : BPTR);
Begin
  asm
    MOVE.L  A6,-(A7)
    MOVE.L  lock,d1
    MOVE.L  _DOSBase,A6
    JSR -090(A6)
    MOVE.L  (A7)+,A6
  end;
end;

FUNCTION Info(lock : BPTR; parameterBlock : pInfoData) : BOOLEAN;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  lock,D1
    MOVE.L  parameterBlock,D2
    MOVEA.L _DOSBase,A6
    JSR -114(A6)
    MOVEA.L (A7)+,A6
    TST.L   D0
    BEQ.B   @end
    MOVE.B  #1,D0
    @end:
     MOVE.B  D0,@RESULT
  END;
END;

FUNCTION NameFromLock(lock : BPTR; buffer : pCHAR; len : LONGINT) : BOOLEAN;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  lock,D1
    MOVE.L  buffer,D2
    MOVE.L  len,D3
    MOVEA.L _DOSBase,A6
    JSR -402(A6)
    MOVEA.L (A7)+,A6
    TST.L   D0
    BEQ.B   @end
    MOVE.B  #1,D0
    @end: MOVE.B  D0,@RESULT
  END;
END;

FUNCTION GetVar(name : pCHAR; buffer : pCHAR; size : LONGINT; flags : LONGINT) : LONGINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  name,D1
    MOVE.L  buffer,D2
    MOVE.L  size,D3
    MOVE.L  flags,D4
    MOVEA.L _DOSBase,A6
    JSR -906(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION FindTask(name : pCHAR) : pTask;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L name,A1
    MOVEA.L _ExecBase,A6
    JSR -294(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION MatchFirst(pat : pCHAR; anchor : pAnchorPath) : LONGINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  pat,D1
    MOVE.L  anchor,D2
    MOVEA.L _DOSBase,A6
    JSR -822(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION MatchNext(anchor : pAnchorPath) : LONGINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  anchor,D1
    MOVEA.L _DOSBase,A6
    JSR -828(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

PROCEDURE MatchEnd(anchor : pAnchorPath);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  anchor,D1
    MOVEA.L _DOSBase,A6
    JSR -834(A6)
    MOVEA.L (A7)+,A6
  END;
END;

FUNCTION Cli : pCommandLineInterface;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L _DOSBase,A6
    JSR -492(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

Function _Execute(p: pchar): longint;
 Begin
   asm
     move.l  a6,-(sp)                 { save base pointer       }
     move.l  d2,-(sp)
     move.l  p,d1                  { command to execute      }
     clr.l   d2                    { No TagList for command  }
     move.l  _DosBase,a6
     jsr     _LVOSystemTagList(a6)
     move.l  (sp)+,d2
     move.l  (sp)+,a6
     move.l  d0,@RESULT
   end;
end;

FUNCTION LockDosList(flags : longint) : pDosList;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  flags,D1
    MOVEA.L _DOSBase,A6
    JSR -654(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;


PROCEDURE UnLockDosList(flags : longint);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  flags,D1
    MOVEA.L _DOSBase,A6
    JSR -660(A6)
    MOVEA.L (A7)+,A6
  END;
END;


FUNCTION NextDosEntry(dlist : pDosList; flags : longint) : pDosList;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  dlist,D1
    MOVE.L  flags,D2
    MOVEA.L _DOSBase,A6
    JSR -690(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;


FUNCTION BADDR(bval : BPTR): POINTER;
BEGIN
    BADDR := POINTER( bval shl 2);
END;

function PasToC(var s: string): Pchar;
var i: integer;
begin
    i := Length(s) + 1;
    if i > 255 then
    begin
        Delete(s, 255, 1);      { ensure there is a spare byte }
        Dec(i)
    end;
    s[i]     := #0;
    PasToC := @s[1]
end;


Procedure AmigaToDt(SecsPast: LongInt; Var Dt: DateTime);
var
  cd : pClockData;
Begin
  New(cd);
  Amiga2Date(SecsPast,cd);
  Dt.sec   := cd^.sec;
  Dt.min   := cd^.min;
  Dt.hour  := cd^.hour;
  Dt.day   := cd^.mday;
  Dt.month := cd^.month;
  Dt.year  := cd^.year;
  Dispose(cd);
End;

Function DtToAmiga(DT: DateTime): LongInt;
var
  cd : pClockData;
  temp : Longint;
Begin
  New(cd);
  cd^.sec   := Dt.sec;
  cd^.min   := Dt.min;
  cd^.hour  := Dt.hour;
  cd^.mday  := Dt.day;
  cd^.month := Dt.month;
  cd^.year  := Dt.year;
  temp := Date2Amiga(cd);
  Dispose(cd);
  DtToAmiga := temp;
end;

Function SetProtection(const name: string; mask:longint): longint;
 var
  buffer : array[0..255] of char;
 Begin
   move(name[1],buffer,length(name));
   buffer[length(name)]:=#0;
   asm
      move.l  a6,-(sp)
      lea     buffer,a0
      move.l  a0,d1
      move.l  mask,d2
      move.l  _DosBase,a6
      jsr     -186(a6)
      move.l  d6,a6
      move.l  (sp)+,a6
      move.l  d0,@RESULT
   end;
 end;


Function IsLeapYear(Source : Word) : Boolean;
Begin
  If (Source Mod 4 = 0) Then
    IsLeapYear := True
  Else
    IsLeapYear := False;
End;


Procedure Amiga2DateStamp(Date : LongInt; Var TotalDays,Minutes,Ticks: longint);
{ Converts a value in seconds past 1978 to a value in AMIGA DateStamp format }
{ Taken from SWAG and modified to work with the Amiga format - CEC           }
Var
  LocalDate : LongInt; Done : Boolean; X : ShortInt; TotDays : Integer;
  Y: Word;
  M: Word;
  D: Word;
  H: Word;
  Min: Word;
  S : Word;
Begin
  Y   := 1978; M := 1; D := 1; H := 0; Min := 0; S := 0;
  TotalDays := 0;
  Minutes := 0;
  Ticks := 0;
  LocalDate := Date;
  Done := False;
  While Not Done Do
  Begin
    If LocalDate >= SecsPerYear Then
    Begin
      Inc(Y,1);
      Dec(LocalDate,SecsPerYear);
      Inc(TotalDays,DaysPerYear[12]);
    End
    Else
      Done := True;
    If (IsLeapYear(Y+1)) And (LocalDate >= SecsPerLeapYear) And
       (Not Done) Then
    Begin
      Inc(Y,1);
      Dec(LocalDate,SecsPerLeapYear);
      Inc(TotalDays,DaysPerLeapYear[12]);
    End;
  End; { END WHILE }
  M := 1; D := 1;
  Done := False;
  TotDays := LocalDate Div SecsPerDay;
  { Total number of days }
  TotalDays := TotalDays + TotDays;
    Dec(LocalDate,TotDays*SecsPerDay);
  { Absolute hours since start of day }
  H := LocalDate Div SecsPerHour;
  { Convert to minutes }
  Minutes := H*60;
    Dec(LocalDate,(H * SecsPerHour));
  { Find the remaining minutes to add }
  Min := LocalDate Div SecsPerMinute;
    Dec(LocalDate,(Min * SecsPerMinute));
  Minutes:=Minutes+Min;
  { Find the number of seconds and convert to ticks }
  S := LocalDate;
  Ticks:=TICKSPERSECOND*S;
End;


  Function SetFileDate(name: string; p : pDateStamp): longint;
  var
    buffer : array[0..255] of char;
  Begin
    move(name[1],buffer,length(name));
    buffer[length(name)]:=#0;
     asm
       move.l a6,-(sp)           { save base pointer }
       move.l d2,-(sp)        { save reserved reg }
       lea    buffer,a0
       move.l a0,d1
       move.l p,d2
       move.l _DosBase,a6
       jsr    _LVOSetFileDate(a6)
       move.l (sp)+,d2        { restore reserved reg }
       move.l (sp)+,a6           { restore base pointer }
       move.l d0,@Result
     end;
  end;





{******************************************************************************
                           --- Dos Interrupt ---
******************************************************************************}

Procedure Intr (intno: byte; var regs: registers);
  Begin
  { Does not apply to Linux - not implemented }
  End;


Procedure SwapVectors;
  Begin
  { Does not apply to Linux - Do Nothing }
  End;


Procedure msdos(var regs : registers);
  Begin
  { ! Not implemented in Linux ! }
  End;


Procedure getintvec(intno : byte;var vector : pointer);
  Begin
  { ! Not implemented in Linux ! }
  End;


Procedure setintvec(intno : byte;vector : pointer);
  Begin
  { ! Not implemented in Linux ! }
  End;

{******************************************************************************
                        --- Info / Date / Time ---
******************************************************************************}

  Function DosVersion: Word;
   var p: pLibrary;
  Begin
    p:=pLibrary(_DosBase);
    DosVersion:= p^.lib_Version or (p^.lib_Revision shl 8);
  End;

{ Here are a lot of stuff just for setdate and settime }

Const

{ unit defintions }
    UNIT_MICROHZ        = 0;
    UNIT_VBLANK         = 1;

    TIMERNAME : PChar   = 'timer.device';

Type

    {***** Device *****************************************************}
     pDevice = ^tDevice;
     tDevice =  record
     dd_Library : tLibrary;
     end;

{***** Unit *******************************************************}
     pUnit = ^tUnit;
     tUnit = record
     unit_MsgPort : tMsgPort;     { queue for unprocessed messages }
                    { instance of msgport is recommended }
     unit_flags,
     unit_pad     : Byte;
     unit_OpenCnt : Word;       { number of active opens }
  end;

{****** Message *****************************************************}

    pMessage = ^tMessage;
    tMessage =  record
    mn_Node       : tNode;
    mn_ReplyPort  : pMsgPort;   { message reply port }
    mn_Length     : Word;      { message len in bytes }
    end;

    pIORequest = ^tIORequest;
    tIORequest =  record
    io_Message  : tMessage;
    io_Device   : pDevice;      { device node pointer  }
    io_Unit     : pUnit;        { unit (driver private)}
    io_Command  : Word;        { device command }
    io_Flags    : Byte;
    io_Error    : Shortint;         { error or warning num }
    end;

    ptimeval = ^ttimeval;
    ttimeval = record
        tv_secs         : longint;
        tv_micro        : longint;
    end;

    ptimerequest = ^ttimerequest;
    ttimerequest = record
        tr_node         : tIORequest;
        tr_time         : ttimeval;
    end;

Const

{ IO_COMMAND to use for adding a timer }
    CMD_NONSTD  = 9;
    TR_ADDREQUEST       = CMD_NONSTD;
    TR_GETSYSTIME       = CMD_NONSTD + 1;
    TR_SETSYSTIME       = CMD_NONSTD + 2;
    MEMF_PUBLIC   = %000000000000000000000001;
    MEMF_CLEAR    = %000000010000000000000000;

{  To use any of the routines below, TimerBase must be set to point
   to the timer.device, either by calling CreateTimer or by pulling
   the device pointer from a valid TimeRequest, i.e.

        TimerBase := TimeRequest.io_Device;

    _after_ you have called OpenDevice on the timer.
 }

var
    TimerBase   : Pointer;


FUNCTION AllocMem(byteSize : longint; requirements : longint) : POINTER;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  byteSize,D0
    MOVE.L  requirements,D1
    MOVEA.L _ExecBase,A6
    JSR -198(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION AllocSignal(signalNum : LONGINT) : shortint;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  signalNum,D0
    MOVEA.L _ExecBase,A6
    JSR -330(A6)
    MOVEA.L (A7)+,A6
    MOVE.B  D0,@RESULT
  END;
END;

PROCEDURE ExecFreeMem(memoryBlock : POINTER; byteSize : longint);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L memoryBlock,A1
    MOVE.L  byteSize,D0
    MOVEA.L _ExecBase,A6
    JSR -210(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE FreeSignal(signalNum : LONGINT);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  signalNum,D0
    MOVEA.L _ExecBase,A6
    JSR -336(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE AddPort(port : pMsgPort);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L port,A1
    MOVEA.L _ExecBase,A6
    JSR -354(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE RemPort(port : pMsgPort);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L port,A1
    MOVEA.L _ExecBase,A6
    JSR -360(A6)
    MOVEA.L (A7)+,A6
  END;
END;

FUNCTION DoIO(ioRequest : pIORequest) : shortint;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L ioRequest,A1
    MOVEA.L _ExecBase,A6
    JSR -456(A6)
    MOVEA.L (A7)+,A6
    MOVE.B  D0,@RESULT
  END;
END;

FUNCTION OpenDevice(const devName : pCHAR; unite : longint; ioRequest : pIORequest; 
flags : longint) : shortint;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L devName,A0
    MOVE.L  unite,D0
    MOVEA.L ioRequest,A1
    MOVE.L  flags,D1
    MOVEA.L _ExecBase,A6
    JSR -444(A6)
    MOVEA.L (A7)+,A6
    MOVE.B  D0,@RESULT
  END;
END;

procedure NewList (list: pList);
begin
    with list^ do
    begin
        lh_Head     := pNode(@lh_Tail);
        lh_Tail     := NIL;
        lh_TailPred := pNode(@lh_Head)
    end
end;

PROCEDURE CloseDevice(ioRequest : pIORequest);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L ioRequest,A1
    MOVEA.L _ExecBase,A6
    JSR -450(A6)
    MOVEA.L (A7)+,A6
  END;
END;

function CreateExtIO (port: pMsgPort; size: Longint): pIORequest;
var
   IOReq: pIORequest;
begin
    IOReq := NIL;
    if port <> NIL then
    begin
        IOReq := AllocMem(size, MEMF_CLEAR or MEMF_PUBLIC);
        if IOReq <> NIL then
        begin
            IOReq^.io_Message.mn_Node.ln_Type   := 7;
            IOReq^.io_Message.mn_Length    := size;
            IOReq^.io_Message.mn_ReplyPort := port;
        end;
    end;
    CreateExtIO := IOReq;
end;

procedure DeleteExtIO (ioReq: pIORequest);
begin
    if ioReq <> NIL then
    begin
        ioReq^.io_Message.mn_Node.ln_Type := $FF;
        ioReq^.io_Message.mn_ReplyPort    := pMsgPort(-1);
        ioReq^.io_Device                  := pDevice(-1);
        ExecFreeMem(ioReq, ioReq^.io_Message.mn_Length);
    end
end;

function Createport(name : PChar; pri : longint): pMsgPort;
var
   sigbit : Byte;
   port    : pMsgPort;
begin
   sigbit := AllocSignal(-1);
   if sigbit = -1 then CreatePort := nil;
   port := Allocmem(sizeof(tMsgPort),MEMF_CLEAR or MEMF_PUBLIC);
   if port = nil then begin
      FreeSignal(sigbit);
      CreatePort := nil;
   end;
   with port^ do begin
       if assigned(name) then
       mp_Node.ln_Name := name
       else mp_Node.ln_Name := nil;
       mp_Node.ln_Pri := pri;
       mp_Node.ln_Type := 4;
       mp_Flags := 0;
       mp_SigBit := sigbit;
       mp_SigTask := FindTask(nil);
   end;
   if assigned(name) then AddPort(port)
   else NewList(addr(port^.mp_MsgList));
   CreatePort := port;
end;

procedure DeletePort (port: pMsgPort);
begin
    if port <> NIL then
    begin
        if port^.mp_Node.ln_Name <> NIL then
            RemPort(port);

        port^.mp_Node.ln_Type     := $FF;
        port^.mp_MsgList.lh_Head  := pNode(-1);
        FreeSignal(port^.mp_SigBit);
        ExecFreeMem(port, sizeof(tMsgPort));
    end;
end;


Function Create_Timer(theUnit : longint) : pTimeRequest;
var
    Error : longint;
    TimerPort : pMsgPort;
    TimeReq : pTimeRequest;
begin
    TimerPort := CreatePort(Nil, 0);
    if TimerPort = Nil then 
  Create_Timer := Nil;
    TimeReq := pTimeRequest(CreateExtIO(TimerPort,sizeof(tTimeRequest)));
    if TimeReq = Nil then begin
  DeletePort(TimerPort);
  Create_Timer := Nil;
    end; 
    Error := OpenDevice(TIMERNAME, theUnit, pIORequest(TimeReq), 0);
    if Error <> 0 then begin
  DeleteExtIO(pIORequest(TimeReq));
  DeletePort(TimerPort);
  Create_Timer := Nil;
    end;
    TimerBase := pointer(TimeReq^.tr_Node.io_Device); 
    Create_Timer := pTimeRequest(TimeReq);
end;

Procedure Delete_Timer(WhichTimer : pTimeRequest);
var
    WhichPort : pMsgPort;
begin
    
    WhichPort := WhichTimer^.tr_Node.io_Message.mn_ReplyPort;
    if assigned(WhichTimer) then begin
        CloseDevice(pIORequest(WhichTimer));
        DeleteExtIO(pIORequest(WhichTimer));
    end;
    if assigned(WhichPort) then
        DeletePort(WhichPort);
end;

function set_new_time(secs, micro : longint): longint;
var
    tr : ptimerequest;
begin
    tr := create_timer(UNIT_MICROHZ);

    { non zero return says error }
    if tr = nil then set_new_time := -1;
  
    tr^.tr_time.tv_secs := secs;
    tr^.tr_time.tv_micro := micro;
    tr^.tr_node.io_Command := TR_SETSYSTIME;
    DoIO(pIORequest(tr));

    delete_timer(tr);
    set_new_time := 0;
end;

function get_sys_time(tv : ptimeval): longint;
var
    tr : ptimerequest;
begin
    tr := create_timer( UNIT_MICROHZ );

    { non zero return says error }
    if tr = nil then get_sys_time := -1;

    tr^.tr_node.io_Command := TR_GETSYSTIME;
    DoIO(pIORequest(tr));

   { structure assignment }
   tv^ := tr^.tr_time;

   delete_timer(tr);
   get_sys_time := 0;
end;

Procedure GetDate(Var Year, Month, MDay, WDay: Word);
Var
  cd    : pClockData;
  oldtime : ttimeval;
begin
  New(cd);
  get_sys_time(@oldtime);
  Amiga2Date(oldtime.tv_secs,cd);
  Year  := cd^.year;
  Month := cd^.month;
  MDay  := cd^.mday;
  WDay  := cd^.wday;
  Dispose(cd);
end;

Procedure SetDate(Year, Month, Day: Word);
var
  cd : pClockData;
  oldtime : ttimeval;
Begin
  new(cd);
  get_sys_time(@oldtime);
  Amiga2Date(oldtime.tv_secs,cd);
  cd^.year := Year;
  cd^.month := Month;
  cd^.mday := Day;
  set_new_time(Date2Amiga(cd),0);
  dispose(cd);
  End;

Procedure GetTime(Var Hour, Minute, Second, Sec100: Word);
Var
  cd      : pClockData;
  oldtime : ttimeval;
begin
  New(cd);
  get_sys_time(@oldtime);
  Amiga2Date(oldtime.tv_secs,cd);
  Hour   := cd^.hour;
  Minute := cd^.min;
  Second := cd^.sec;
  Sec100 := oldtime.tv_micro div 10000;
  Dispose(cd);
END;


Procedure SetTime(Hour, Minute, Second, Sec100: Word);
var
  cd : pClockData;
  oldtime : ttimeval;
Begin
  new(cd);
  get_sys_time(@oldtime);
  Amiga2Date(oldtime.tv_secs,cd);
  cd^.hour := Hour;
  cd^.min := Minute;
  cd^.sec := Second;
  set_new_time(Date2Amiga(cd), Sec100 * 10000);
  dispose(cd);
  End;

Procedure unpacktime(p : longint;var t : datetime);
Begin
  AmigaToDt(p,t);
End;


Procedure packtime(var t : datetime;var p : longint);
Begin
  p := DtToAmiga(t);
end;


{******************************************************************************
                               --- Exec ---
******************************************************************************}


Var
  LastDosExitCode: word;
  Ver : Boolean;


Procedure Exec (Const Path: PathStr; Const ComLine: ComStr);
  var
   p : string;
   buf: array[0..255] of char;
   result : longint;
   MyLock : longint;
   i : Integer;
  Begin
   DosError := 0;
   LastdosExitCode := 0;
   p:=Path+' '+ComLine;
   { allow backslash as slash }
   for i:=1 to length(p) do
       if p[i]='\' then p[i]:='/';
   Move(p[1],buf,length(p));
   buf[Length(p)]:=#0;
   { Here we must first check if the command we wish to execute }
   { actually exists, because this is NOT handled by the        }
   { _SystemTagList call (program will abort!!)                 }

   { Try to open with shared lock                               }
   MyLock:=Lock(path,SHARED_LOCK);
   if MyLock <> 0 then
     Begin
        { File exists - therefore unlock it }
        Unlock(MyLock);
        result:=_Execute(buf);
        { on return of -1 the shell could not be executed }
        { probably because there was not enough memory    }
        if result = -1 then
          DosError:=8
        else
          LastDosExitCode:=word(result);
     end
   else
    DosError:=3;
  End;


Function DosExitCode: Word;
  Begin
    DosExitCode:=LastdosExitCode;
  End;


  Procedure GetCBreak(Var BreakValue: Boolean);
  Begin
   breakvalue := system.BreakOn;
  End;


 Procedure SetCBreak(BreakValue: Boolean);
  Begin
   system.Breakon := BreakValue;
  End;


  Procedure GetVerify(Var Verify: Boolean);
   Begin
     verify:=ver;
   End;


 Procedure SetVerify(Verify: Boolean);
  Begin
    ver:=Verify;
  End;

{******************************************************************************
                               --- Disk ---
******************************************************************************}

{ How to solve the problem with this:       }
{  We could walk through the device list    }
{  at startup to determine possible devices }

const

  not_to_use_devs : array[0..12] of string =(
                   'DF0:',
                   'DF1:',
                   'DF2:',
                   'DF3:',
                   'PED:',
                   'PRJ:',
                   'PIPE:',
                   'RAM:',
                   'CON:',
                   'RAW:',
                   'SER:',
                   'PAR:',
                   'PRT:');

var
   deviceids : array[1..20] of byte;
   devicenames : array[1..20] of string[20];
   numberofdevices : Byte;

Function DiskFree(Drive: Byte): Longint;
Var
  MyLock      : BPTR;
  Inf         : pInfoData;
  Free        : Longint;
  myproc      : pProcess;
  OldWinPtr   : Pointer;
Begin
  Free := -1;
  { Here we stop systemrequesters to appear }
  myproc := pProcess(FindTask(nil));
  OldWinPtr := myproc^.pr_WindowPtr;
  myproc^.pr_WindowPtr := Pointer(-1);
  { End of systemrequesterstop }
  New(Inf);
  MyLock := Lock(devicenames[deviceids[Drive]],SHARED_LOCK);
  If MyLock <> 0 then begin
     if Info(MyLock,Inf) then begin
        Free := (Inf^.id_NumBlocks * Inf^.id_BytesPerBlock) -
                (Inf^.id_NumBlocksUsed * Inf^.id_BytesPerBlock);
     end;
     Unlock(MyLock);
  end;
  Dispose(Inf);
  { Restore systemrequesters }
  myproc^.pr_WindowPtr := OldWinPtr;
  diskfree := Free;
end;



Function DiskSize(Drive: Byte): Longint;
Var
  MyLock      : BPTR;
  Inf         : pInfoData;
  Size        : Longint;
  myproc      : pProcess;
  OldWinPtr   : Pointer;
Begin
  Size := -1;
  { Here we stop systemrequesters to appear }
  myproc := pProcess(FindTask(nil));
  OldWinPtr := myproc^.pr_WindowPtr;
  myproc^.pr_WindowPtr := Pointer(-1);
  { End of systemrequesterstop }
  New(Inf);
  MyLock := Lock(devicenames[deviceids[Drive]],SHARED_LOCK);
  If MyLock <> 0 then begin
     if Info(MyLock,Inf) then begin
        Size := (Inf^.id_NumBlocks * Inf^.id_BytesPerBlock);
     end;
     Unlock(MyLock);
  end;
  Dispose(Inf);
  { Restore systemrequesters }
  myproc^.pr_WindowPtr := OldWinPtr;
  disksize := Size;
end;




Procedure FindFirst(Path: PathStr; Attr: Word; Var f: SearchRec);
var
 buf: Array[0..255] of char;
 Anchor : pAnchorPath;
 Result : Longint;
 index : Integer;
 s     : string;
 j     : integer;
Begin
 DosError:=0;
 New(Anchor);
 {----- allow backslash as slash         -----}
 for index:=1 to length(path) do
   if path[index]='\' then path[index]:='/';
 { remove any dot characters and replace by their current }
 { directory equivalent.                                  }
 if pos('../',path) = 1 then
   begin
     getdir(0,s);
     while pos('../',path) = 1 do
     { look for parent directory }
      Begin
         delete(path,1,3);
         j:=length(s);
         while (s[j] <> '/') AND (s[j] <> ':') AND (j > 0 ) do
           dec(j);
         if j > 0 then
           s:=copy(s,1,j-1);
      end;
     if (length(s) <> 0) and (s[length(s)] <> ':') then
       s:=s + '/';
     path:=s+path;
  end
 else
 if pos('./',path) = 1 then
 { look for current directory }
    Begin
       delete(path,1,2);
       getdir(0,s);
       if (s[length(s)] <> '/') and (s[length(s)] <> ':') then
          s:=s+'/';
       path:=s+path;
    end;
 {----- replace * by #? AmigaOs strings  -----}
 repeat
  index:= pos('*',Path);
  if index <> 0 then
   Begin
     delete(Path,index,1);
     insert('#?',Path,index);
   end;
 until index =0;
 {--------------------------------------------}
 FillChar(Anchor^,sizeof(TAnchorPath),#0);
 move(path[1],buf,length(path));
 buf[length(path)]:=#0;

 Result:=MatchFirst(@buf,Anchor);
 f.AnchorPtr:=Anchor;
 if Result = ERROR_NO_MORE_ENTRIES then
   DosError:=18
 else
 if Result <> 0 then
   DosError:=3;
 { If there is an error, deallocate }
 { the anchorpath structure         }
 if DosError <> 0 then
   Begin
     MatchEnd(Anchor);
     if assigned(Anchor) then
       Dispose(Anchor);
   end
 else
 {-------------------------------------------------------------------}
 { Here we fill up the SearchRec attribute, but we also do check     }
 { something else, if the it does not match the mask we are looking  }
 { for we should go to the next file or directory.                   }
 {-------------------------------------------------------------------}
   Begin
         with Anchor^.ap_Info do
          Begin
             f.Time := fib_Date.ds_Days * (24 * 60 * 60) +
             fib_Date.ds_Minute * 60 +
             fib_Date.ds_Tick div 50;
           {*------------------------------------*}
           {* Determine if is a file or a folder *}
           {*------------------------------------*}
           if fib_DirEntryType > 0 then
               f.attr:=f.attr OR DIRECTORY;

           {*------------------------------------*}
           {* Determine if Read only             *}
           {*  Readonly if R flag on and W flag  *}
           {*   off.                             *}
           {* Should we check also that EXEC     *}
           {* is zero? for read only?            *}
           {*------------------------------------*}
           if   ((fib_Protection and FIBF_READ) <> 0)
            AND ((fib_Protection and FIBF_WRITE) = 0)
           then
              f.attr:=f.attr or READONLY;
           f.Name := strpas(fib_FileName);
           f.Size := fib_Size;
         end; { end with }
   end;
End;


Procedure FindNext(Var f: SearchRec);
var
 Result: longint;
 Anchor : pAnchorPath;
Begin
 DosError:=0;
 Result:=MatchNext(f.AnchorPtr);
 if Result = ERROR_NO_MORE_ENTRIES then
   DosError:=18
 else
 if Result <> 0 then
   DosError:=3;
 { If there is an error, deallocate }
 { the anchorpath structure         }
 if DosError <> 0 then
   Begin
     MatchEnd(f.AnchorPtr);
     if assigned(f.AnchorPtr) then
       {Dispose}FreeMem(f.AnchorPtr);
   end
 else
 { Fill up the Searchrec information     }
 { and also check if the files are with  }
 { the correct attributes                }
   Begin
         Anchor:=pAnchorPath(f.AnchorPtr);
         with Anchor^.ap_Info do
          Begin
             f.Time := fib_Date.ds_Days * (24 * 60 * 60) +
             fib_Date.ds_Minute * 60 +
             fib_Date.ds_Tick div 50;
           {*------------------------------------*}
           {* Determine if is a file or a folder *}
           {*------------------------------------*}
           if fib_DirEntryType > 0 then
               f.attr:=f.attr OR DIRECTORY;

           {*------------------------------------*}
           {* Determine if Read only             *}
           {*  Readonly if R flag on and W flag  *}
           {*   off.                             *}
           {* Should we check also that EXEC     *}
           {* is zero? for read only?            *}
           {*------------------------------------*}
           if   ((fib_Protection and FIBF_READ) <> 0)
            AND ((fib_Protection and FIBF_WRITE) = 0)
           then
              f.attr:=f.attr or READONLY;
           f.Name := strpas(fib_FileName);
           f.Size := fib_Size;
         end; { end with }
   end;
End;

    Procedure FindClose(Var f: SearchRec);
      begin
      end;

{******************************************************************************
                               --- File ---
******************************************************************************}
Procedure FSplit(path: pathstr; var dir: dirstr; var name: namestr; var ext: extstr);
var
  I: Word;
begin
  { allow backslash as slash }
  for i:=1 to length(path) do
    if path[i]='\' then path[i]:='/';

  I := Length(Path);
  while (I > 0) and not ((Path[I] = '/') or (Path[I] = ':'))
     do Dec(I);
  if Path[I] = '/' then
     dir := Copy(Path, 0, I)
  else dir := Copy(Path,0,I);

  if Length(Path) > Length(dir) then
      name := Copy(Path, I + 1, Length(Path)-I)
  else
      name := '';
  { Remove extension }
  if pos('.',name) <> 0 then
   begin
     ext:=copy(name,pos('.',name),length(name));
     delete(name,pos('.',name),length(name));
   end
 else
   ext := '';
end;

{$DEFINE FPC_FEXPAND_VOLUMES} (* Full paths begin with drive specification *)
{$DEFINE FPC_FEXPAND_DRIVESEP_IS_ROOT}
{$DEFINE FPC_FEXPAND_NO_DEFAULT_PATHS}
{$I fexpand.inc}

{$UNDEF FPC_FEXPAND_VOLUMES} (* Full paths begin with drive specification *)
{$UNDEF FPC_FEXPAND_DRIVESEP_IS_ROOT}
{$UNDEF FPC_FEXPAND_NO_DEFAULT_PATHS}



   Function  fsearch(path : pathstr;dirlist : string) : pathstr;
      var
         i,p1   : longint;
         s      : searchrec;
         newdir : pathstr;
      begin
      { No wildcards allowed in these things }
         if (pos('?',path)<>0) or (pos('*',path)<>0) then
           fsearch:=''
         else
           begin
              { allow slash as backslash }
              for i:=1 to length(dirlist) do
                if dirlist[i]='\' then dirlist[i]:='/';
              repeat
                p1:=pos(';',dirlist);
                if p1<>0 then
                 begin
                   newdir:=copy(dirlist,1,p1-1);
                   delete(dirlist,1,p1);
                 end
                else
                 begin
                   newdir:=dirlist;
                   dirlist:='';
                 end;
                if (newdir<>'') and (not (newdir[length(newdir)] in ['/',':'])) then
                 newdir:=newdir+'/';
                findfirst(newdir+path,anyfile,s);
                if doserror=0 then
                 newdir:=newdir+path
                else
                 newdir:='';
              until (dirlist='') or (newdir<>'');
              fsearch:=newdir;
           end;
      end;


Procedure getftime (var f; var time : longint);
{
    This function returns a file's date and time as the number of
    seconds after January 1, 1978 that the file was created.
}
var
    FInfo : pFileInfoBlock;
    FTime : Longint;
    FLock : Longint;
    Str   : String;
    i     : integer;
begin
    DosError:=0;
    FTime := 0;
    Str := StrPas(filerec(f).name);
    for i:=1 to length(Str) do
     if str[i]='\' then str[i]:='/';
    FLock := Lock(Str, SHARED_LOCK);
    IF FLock <> 0 then begin
        New(FInfo);
        if Examine(FLock, FInfo) then begin
             with FInfo^.fib_Date do
             FTime := ds_Days * (24 * 60 * 60) +
             ds_Minute * 60 +
             ds_Tick div 50;
        end else begin
             FTime := 0;
        end;
        Unlock(FLock);
        Dispose(FInfo);
    end
    else
     DosError:=6;
    time := FTime;
end;


  Procedure setftime(var f; time : longint);
   var
    DateStamp: pDateStamp;
    Str: String;
    i: Integer;
    Days, Minutes,Ticks: longint;
    FLock: longint;
  Begin
    new(DateStamp);
    Str := StrPas(filerec(f).name);
    for i:=1 to length(Str) do
     if str[i]='\' then str[i]:='/';
    { Check first of all, if file exists }
    FLock := Lock(Str, SHARED_LOCK);
    IF FLock <> 0 then
      begin
        Unlock(FLock);
        Amiga2DateStamp(time,Days,Minutes,ticks);
        DateStamp^.ds_Days:=Days;
        DateStamp^.ds_Minute:=Minutes;
        DateStamp^.ds_Tick:=Ticks;
        if SetFileDate(Str,DateStamp) <> 0 then
            DosError:=0
        else
            DosError:=6;
      end
    else
      DosError:=2;
    if assigned(DateStamp) then Dispose(DateStamp);
  End;

  Procedure getfattr(var f; var attr : word);
  var
    info : pFileInfoBlock;
    MyLock : Longint;
    flags: word;
    Str: String;
    i: integer;
  Begin
    DosError:=0;
    flags:=0;
    New(info);
    Str := StrPas(filerec(f).name);
    for i:=1 to length(Str) do
     if str[i]='\' then str[i]:='/';
    { open with shared lock to check if file exists }
    MyLock:=Lock(Str,SHARED_LOCK);
    if MyLock <> 0 then
      Begin
        Examine(MyLock,info);
        {*------------------------------------*}
        {* Determine if is a file or a folder *}
        {*------------------------------------*}
        if info^.fib_DirEntryType > 0 then
             flags:=flags OR DIRECTORY;

        {*------------------------------------*}
        {* Determine if Read only             *}
        {*  Readonly if R flag on and W flag  *}
        {*   off.                             *}
        {* Should we check also that EXEC     *}
        {* is zero? for read only?            *}
        {*------------------------------------*}
        if   ((info^.fib_Protection and FIBF_READ) <> 0)
         AND ((info^.fib_Protection and FIBF_WRITE) = 0)
         then
          flags:=flags OR ReadOnly;
        Unlock(mylock);
      end
    else
      DosError:=3;
    attr:=flags;
    Dispose(info);
  End;


Procedure setfattr (var f;attr : word);
  var
   flags: longint;
   MyLock : longint;
   str: string;
   i: integer;
  Begin
    DosError:=0;
    flags:=FIBF_WRITE;
    { open with shared lock }
    Str := StrPas(filerec(f).name);
    for i:=1 to length(Str) do
     if str[i]='\' then str[i]:='/';

    MyLock:=Lock(Str,SHARED_LOCK);

    { By default files are read-write }
    if attr AND ReadOnly <> 0 then
      { Clear the Fibf_write flags }
      flags:=FIBF_READ;


    if MyLock <> 0 then
     Begin
       Unlock(MyLock);
       if SetProtection(Str,flags) = 0 then
         DosError:=5;
     end
    else
      DosError:=3;
  End;



{******************************************************************************
                             --- Environment ---
******************************************************************************}

var
StrofPaths : string[255];

function getpathstring: string;
var
   f : text;
   s : string;
   found : boolean;
   temp : string[255];
begin
   found := true;
   temp := '';
   assign(f,'ram:makepathstr');
   rewrite(f);
   writeln(f,'path >ram:temp.lst');
   close(f);
   exec('c:protect','ram:makepathstr sarwed');
   exec('C:execute','ram:makepathstr');
   exec('c:delete','ram:makepathstr quiet');
   assign(f,'ram:temp.lst');
   reset(f);
   { skip the first line, garbage }
   if not eof(f) then readln(f,s);
   while not eof(f) do begin
      readln(f,s);
      if found then begin
         temp := s;
         found := false;
      end else begin;
         if (length(s) + length(temp)) < 255 then
            temp := temp + ';' + s;
      end;
   end;
   close(f);
   exec('C:delete','ram:temp.lst quiet');
   getpathstring := temp;
end;


 Function EnvCount: Longint;
 { HOW TO GET THIS VALUE:                                }
 {   Each time this function is called, we look at the   }
 {   local variables in the Process structure (2.0+)     }
 {   And we also read all files in the ENV: directory    }
  Begin
   EnvCount := 0;
  End;


 Function EnvStr(Index: Integer): String;
  Begin
    EnvStr:='';
  End;



function GetEnv(envvar : String): String;
var
   bufarr : array[0..255] of char;
   strbuffer : array[0..255] of char;
   temp : Longint;
begin
   if UpCase(envvar) = 'PATH' then begin
       if StrOfpaths = '' then StrOfPaths := GetPathString;
       GetEnv := StrofPaths;
   end else begin
      move(envvar,strbuffer,length(envvar));
      strbuffer[length(envvar)] := #0;
      temp := GetVar(strbuffer,bufarr,255,$100);
      if temp = -1 then
        GetEnv := ''
      else GetEnv := StrPas(bufarr);
   end;
end;


{******************************************************************************
                             --- Not Supported ---
******************************************************************************}

Procedure keep(exitcode : word);
  Begin
  { ! Not implemented in Linux ! }
  End;

procedure AddDevice(str : String);
begin
    inc(numberofdevices);
    deviceids[numberofdevices] := numberofdevices;
    devicenames[numberofdevices] := str;
end;

function MakeDeviceName(str : pchar): string;
var
   temp : string[20];
begin
   temp := strpas(str);
   temp := temp + ':';
   MakeDeviceName := temp;
end;

function IsInDeviceList(str : string): boolean;
var
   i : byte;
   theresult : boolean;
begin
   theresult := false;
   for i := low(not_to_use_devs) to high(not_to_use_devs) do
   begin
       if str = not_to_use_devs[i] then begin
          theresult := true;
          break;
       end;
   end;
   IsInDeviceList := theresult;
end;


function BSTR2STRING(s : BSTR): pchar;
begin
    BSTR2STRING := Pointer(Longint(BADDR(s))+1);
end;

procedure ReadInDevices;
var
   dl : pDosList;
   temp : pchar;
   str  : string[20];
begin
   dl := LockDosList(LDF_DEVICES or LDF_READ );
   repeat
      dl := NextDosEntry(dl,LDF_DEVICES );
      if dl <> nil then begin
         temp := BSTR2STRING(dl^.dol_Name);
         str := MakeDeviceName(temp);
         if not IsInDeviceList(str) then
              AddDevice(str);
      end;
   until dl = nil;
   UnLockDosList(LDF_DEVICES or LDF_READ );
end;

Begin
 DosError:=0;
 ver := TRUE;
 numberofdevices := 0;
 StrOfPaths := '';
 AddDevice('DF0:');
 AddDevice('DF1:');
 AddDevice('DF2:');
 AddDevice('DF3:');
 ReadInDevices;
End.

{
  $Log: dos.pp,v $
  Revision 1.1.2.10  2003/01/19 14:56:58  carl
    * more fixes for fexpand (use correct defines)

  Revision 1.1.2.9  2002/11/04 19:11:50  carl
    * bugfix for FindFirst with ../../../ ..

  Revision 1.1.2.8  2002/11/03 19:05:48  carl
    + use generic version of fexpand.inc

  Revision 1.1.2.7  2002/11/02 16:23:26  carl
    * Patches from Nils : GetDate/SetDate/SetTime/GetTime correct
      implementation

  Revision 1.1.2.6  2002/10/20 16:11:37  carl
    * dos : fsplit / fexpand bugfixes
    * makefile updates for cycle
    * sysamiga : more memory blocks can be allocated (up 16 Mbytes of RAM)

  Revision 1.1.2.5  2002/10/06 18:55:22  carl
    * bugfixes of pointerlist when allocating memory
    * getenv returns something now to avoid crashes

  Revision 1.1.2.4  2002/10/06 13:46:29  carl
    - no longer use D6 as scratch register

  Revision 1.1.2.3  2001/11/23 00:25:21  carl
  * updated behavior of some routines to conform to docs

  Revision 1.1.2.2  2001/07/24 07:32:25  pierre
   * Use FreeMem on untyped pointer instead of dispose

  Revision 1.1.2.1  2001/03/27 03:12:57  carl
  + more routines are implemented (from Nils - thanks!)
  ? Is the problem with illegal memory read fixed?

  Revision 1.8  1998/08/19 14:52:52  carl
    * SearchRec was not aligned!! so BOUM!...

  Revision 1.7  1998/08/17 12:30:42  carl
    * FExpand removes dot characters
    * Findfirst single/double dot expansion
    + SetFtime implemented

  Revision 1.6  1998/08/13 13:18:45  carl
    * FSearch bugfix
    * FSplit bugfix
    + GetFAttr,SetFAttr and GetFTime accept dos dir separators

  Revision 1.5  1998/08/04 13:37:10  carl
    * bugfix of findfirst, was not convberting correctl backslahes

       History (Nils Sjoholm):
       10.02.1998  First version for Amiga.
                   Just GetDate and GetTime.

       11.02.1998  Added AmigaToDt and DtToAmiga
                   Changed GetDate and GetTime to
                   use AmigaToDt and DtToAmiga.

                   Added DiskSize and DiskFree.
                   They are using a string as arg
                   have to try to fix that.

       12.02.1998  Added Fsplit and FExpand.
                   Cleaned up the unit and removed
                   stuff that was not used yet.

       13.02.1998  Added CToPas and PasToC and removed
                   the uses of strings.

       14.02.1998  Removed AmigaToDt and DtToAmiga
                   from public area.
                   Added deviceids and devicenames
                   arrays so now diskfree and disksize
                   is compatible with dos.



}









