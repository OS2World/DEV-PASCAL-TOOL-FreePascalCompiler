{
    $Id: sysamiga.pas,v 1.1.2.17 2002/11/04 19:21:44 carl Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,98 by Carl Eric Codere
    Some parts taken from
       Marcel Timmermans - Modula 2 Compiler
       Nils Sjoholm - Amiga porter
       Matthew Dillon - Dice C (with his kind permission)
    dillon@backplane.com

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit sysamiga;

{
    History:

    Added the global boolean flag BreakOn.
    Now you can disable the ctrl-c checking in
    system. Just put system.BreakOn := false in your
    and the checking is gone. Don't forget that you
    have to do checking by yourself now.
    06 Aug 2000.

    Added Workbench startup.
    _WBenchMsg is global.
    If started from workbench the program will open
    an default conwindow for input and output. This
    conwindow will be closed after your program has
    exit.
    Perhaps make ConName global so we can close the
    con and redirect the output to a file. Not
    fixed yet. Later.
    14 Aug 2000.

    Fixed the linked list of opened files, all
    files will now be closed if the compiler
    gets aborted.
    09 Nov 2000.

    nils.sjoholm@mailbox.swipnet.se
}


{--------------------------------------------------------------------}
{ LEFT TO DO:                                                        }
{--------------------------------------------------------------------}
{ o GetDir with different drive numbers                              }
{--------------------------------------------------------------------}


{$S-}
{ To avoid range check errors in pointerlist checking }
{$B-}
{ AmigaOS uses character #10 as eoln only }
{$DEFINE SHORT_LINEBREAK}

  interface


    {$I systemh.inc}

    { used for single computations }
    { must be after systemh.inc
      as this file contains mode objfpc
      that can only be interpreted before any statement PM }
    const BIAS4 = $7f-1;

    {$I heaph.inc}

{Platform specific information}
const
 LineEnding = #10;
 LFNSupport = true;
 DirectorySeparator = '/';
 DriveSeparator = ':';
 PathSeparator = ';';
 FileNameCaseSensitive = false;

const
  UnusedHandle    : longint = -1;
  StdInputHandle  : longint = 0;
  StdOutputHandle : longint = 0;
  StdErrorHandle  : longint = 0;

 _ExecBase:longint = $4;
 _WBenchMsg : pointer = nil;


 _IntuitionBase : pointer = nil;       { intuition library pointer }
 _DosBase       : pointer = nil;       { DOS library pointer       }
 _UtilityBase   : pointer = nil;       { utiity library pointer    }

  BreakOn : Boolean = true;

 { Required for crt unit }
  function do_read(h,addr,len : longint) : longint;
  function do_write(h,addr,len : longint) : longint;


var
{ C-compatible arguments and environment }
  argc  : longint;
  argv  : ppchar;

  implementation

 const
   tempvar : longint = 0;
   intuitionname : pchar = 'intuition.library';
   dosname : pchar = 'dos.library';
   utilityname : pchar = 'utility.library';
   { To avoid infinite recursivity if stack checking 
     is enabled in the system unit.
   }
   StackError : boolean = false;

   ConName : PChar ='CON:10/30/620/100/FPCPascal ShellWindow/AUTO/CLOSE/WAIT';
   myhandle : longint = 0;
   WBStarted : boolean = false;

   { Maximum number of blocks allocated from OS }
   max_os_allocs = 128;
   { AmigaOS does not autoamtically deallocate memory on program termination }
   { therefore we have to handle this manually. This is a list of allocated  }
   { pointers from the OS, we cannot use a linked list, because the linked   }
   { list itself uses the HEAP!                                              }
   pointerlist : array[1..max_os_allocs] of pointer =
   (
    nil,nil,nil,nil,nil,nil,nil,nil,
    nil,nil,nil,nil,nil,nil,nil,nil,
    nil,nil,nil,nil,nil,nil,nil,nil,
    nil,nil,nil,nil,nil,nil,nil,nil,
    nil,nil,nil,nil,nil,nil,nil,nil,
    nil,nil,nil,nil,nil,nil,nil,nil,
    nil,nil,nil,nil,nil,nil,nil,nil,
    nil,nil,nil,nil,nil,nil,nil,nil,
    nil,nil,nil,nil,nil,nil,nil,nil,
    nil,nil,nil,nil,nil,nil,nil,nil,
    nil,nil,nil,nil,nil,nil,nil,nil,
    nil,nil,nil,nil,nil,nil,nil,nil,
    nil,nil,nil,nil,nil,nil,nil,nil,
    nil,nil,nil,nil,nil,nil,nil,nil,
    nil,nil,nil,nil,nil,nil,nil,nil,
    nil,nil,nil,nil,nil,nil,nil,nil
   );

var
  _HEAP : longint;external name 'HEAP';
  _HEAPSIZE : longint;external name 'HEAPSIZE';

  TYPE
   { from exec.inc}
    BPTR     = Longint;
    ULONG    = Longint;


  pNode = ^tNode;
  tNode =  packed Record
      ln_Succ,                { * Pointer to next (successor) * }
      ln_Pred  : pNode;       { * Pointer to previous (predecessor) * }
      ln_Type  : Byte;
      ln_Pri   : Shortint;        { * Priority, for sorting * }
      ln_Name  : PChar;      { * ID string, null terminated * }
      End;  { * Note: Integer aligned * }

  pMinNode = ^tMinNode;
  tMinNode =  packed Record
    mln_Succ,
    mln_Pred  : pMinNode;
  End;

 pList = ^tList;
    tList =  packed record
    lh_Head     : pNode;
    lh_Tail     : pNode;
    lh_TailPred : pNode;
    lh_Type     : Byte;
    l_pad       : Byte;
    end;

{ minimum list -- no type checking possible }

    pMinList = ^tMinList;
    tMinList =  packed record
    mlh_Head        : pMinNode;
    mlh_Tail        : pMinNode;
    mlh_TailPred    : pMinNode;
    end;

   pMsgPort = ^tMsgPort;
    tMsgPort = packed record
    mp_Node     : tNode;
    mp_Flags    : Byte;
    mp_SigBit   : Byte;     { signal bit number    }
    mp_SigTask  : Pointer;   { task to be signalled (TaskPtr) }
    mp_MsgList  : tList;     { message linked list  }
    end;

    pTask = ^tTask;
    tTask =  packed record
        tc_Node         : tNode;
        tc_Flags        : Byte;
        tc_State        : Byte;
        tc_IDNestCnt    : Shortint;         { intr disabled nesting         }
        tc_TDNestCnt    : Shortint;         { task disabled nesting         }
        tc_SigAlloc     : ULONG;        { sigs allocated                }
        tc_SigWait      : ULONG;        { sigs we are waiting for       }
        tc_SigRecvd     : ULONG;        { sigs we have received         }
        tc_SigExcept    : ULONG;        { sigs we will take excepts for }
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
    { end exec.inc}

  TYPE
    TDateStamp = packed record
      ds_Days         : Longint;      { Number of days since Jan. 1, 1978 }
      ds_Minute       : Longint;      { Number of minutes past midnight }
      ds_Tick         : Longint;      { Number of ticks past minute }
    end;
    PDateStamp = ^TDateStamp;


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

  { AmigaOS does not automatically close opened files on exit back to  }
  { the operating system, therefore as a precuation we close all files }
  { manually on exit.                                                  }
  PFileList = ^TFileList;
  flist = pFileList;
  TFileList = record { no packed, must be correctly aligned }
     Handle: longint;      { Handle to file    }
     next: pfilelist;      { Next file in list }
    { closed: boolean;  }    { TRUE=file already closed }
  end;




    Const
     CTRL_C               = 20;      { Error code on CTRL-C press }
     SIGBREAKF_CTRL_C     = $1000;   { CTRL-C signal flags }

    _LVOFindTask          = -294;
    _LVOWaitPort          = -384;
    _LVOGetMsg            = -372;
    _LVOOpenLibrary       = -552;
    _LVOCloseLibrary      = -414;
    _LVOClose             = -36;
    _LVOOpen              = -30;
    _LVOIoErr             = -132;
    _LVOSeek              = -66;
    _LVODeleteFile        = -72;
    _LVORename            = -78;
    _LVOWrite             = -48;
    _LVORead              = -42;
    _LVOCreateDir         = -120;
    _LVOSetCurrentDirName = -558;
    _LVOGetCurrentDirName = -564;
    _LVOInput             = -54;
    _LVOOutput            = -60;
    _LVOUnLock            = -90;
    _LVOLock              = -84;
    _LVOCurrentDir        = -126;

    _LVONameFromLock      = -402;
    _LVONameFromFH        = -408;
    _LVOGetProgramName    = -576;
    _LVOGetProgramDir     = -600;
    _LVODupLock           =  -96;
    _LVOExamine           = -102;
    _LVOParentDir         = -210;
    _LVOSetFileSize       = -456;
    _LVOSetSignal         = -306;
    _LVOAllocVec          = -684;
    _LVOFreeVec           = -690;
    _LVOForbid            = -132;
    _LVOReplyMsg          = -378;


      { Errors from IoErr(), etc. }
      ERROR_NO_FREE_STORE              = 103;
      ERROR_TASK_TABLE_FULL            = 105;
      ERROR_BAD_TEMPLATE               = 114;
      ERROR_BAD_NUMBER                 = 115;
      ERROR_REQUIRED_ARG_MISSING       = 116;
      ERROR_KEY_NEEDS_ARG              = 117;
      ERROR_TOO_MANY_ARGS              = 118;
      ERROR_UNMATCHED_QUOTES           = 119;
      ERROR_LINE_TOO_LONG              = 120;
      ERROR_FILE_NOT_OBJECT            = 121;
      ERROR_INVALID_RESIDENT_LIBRARY   = 122;
      ERROR_NO_DEFAULT_DIR             = 201;
      ERROR_OBJECT_IN_USE              = 202;
      ERROR_OBJECT_EXISTS              = 203;
      ERROR_DIR_NOT_FOUND              = 204;
      ERROR_OBJECT_NOT_FOUND           = 205;
      ERROR_BAD_STREAM_NAME            = 206;
      ERROR_OBJECT_TOO_LARGE           = 207;
      ERROR_ACTION_NOT_KNOWN           = 209;
      ERROR_INVALID_COMPONENT_NAME     = 210;
      ERROR_INVALID_LOCK               = 211;
      ERROR_OBJECT_WRONG_TYPE          = 212;
      ERROR_DISK_NOT_VALIDATED         = 213;
      ERROR_DISK_WRITE_PROTECTED       = 214;
      ERROR_RENAME_ACROSS_DEVICES      = 215;
      ERROR_DIRECTORY_NOT_EMPTY        = 216;
      ERROR_TOO_MANY_LEVELS            = 217;
      ERROR_DEVICE_NOT_MOUNTED         = 218;
      ERROR_SEEK_ERROR                 = 219;
      ERROR_COMMENT_TOO_BIG            = 220;
      ERROR_DISK_FULL                  = 221;
      ERROR_DELETE_PROTECTED           = 222;
      ERROR_WRITE_PROTECTED            = 223;
      ERROR_READ_PROTECTED             = 224;
      ERROR_NOT_A_DOS_DISK             = 225;
      ERROR_NO_DISK                    = 226;
      ERROR_NO_MORE_ENTRIES            = 232;
      { added for 1.4 }
      ERROR_IS_SOFT_LINK               = 233;
      ERROR_OBJECT_LINKED              = 234;
      ERROR_BAD_HUNK                   = 235;
      ERROR_NOT_IMPLEMENTED            = 236;
      ERROR_RECORD_NOT_LOCKED          = 240;
      ERROR_LOCK_COLLISION             = 241;
      ERROR_LOCK_TIMEOUT               = 242;
      ERROR_UNLOCK_ERROR               = 243;
      
      NT_TASK = 1;



    var
      Initial: boolean;           { Have successfully opened Std I/O   }
      errno : longint;               { AmigaOS IO Error number            }
      FileList : flist;       { Linked list of opened files        }
      old_exit: Pointer;
      OrigDir : Longint;   { Current lock on original startup directory }

    {$I system.inc}

  { ************************ AMIGAOS STUB ROUTINES ************************* }

  procedure DateStamp(var ds : tDateStamp);
  begin
   asm
      MOVE.L  A6,-(A7)
      MOVE.L  ds,d1
      { LAST THING TO SETUP SHOULD BE A6, otherwise you can }
      { not accept local variable, nor any parameters! :)   }
      MOVE.L  _DOSBase,A6
      JSR -192(A6)
      MOVE.L  (A7)+,A6
  end;
 end;



  { UNLOCK the BPTR pointed to in L }
  Procedure Unlock(alock: longint);
  Begin
    asm
     move.l  alock,d1
     move.l  a6,-(sp)           { save base pointer    }
     move.l   _DosBase,a6
     jsr     _LVOUnlock(a6)
     move.l  (sp)+,a6           { restore base pointer }
    end;
  end;

  { Change to the directory pointed to in the lock }
  Function CurrentDir(alock : longint) : longint;
  Begin
    asm
      move.l  alock,d1
      move.l  a6,-(sp)           { save base pointer    }
      move.l  _DosBase,a6
      jsr     _LVOCurrentDir(a6)
      move.l  (sp)+,a6           { restore base pointer }
      move.l  d0,@Result
    end;
  end;

  { Duplicate a lock }
  Function DupLock(alock: longint): Longint;
   Begin
     asm
       move.l  alock,d1
       move.l  a6,-(sp)        { save base pointer    }
       move.l  _DosBase,a6
       jsr     _LVODupLock(a6)
       move.l  (sp)+,a6      { restore base pointer }
       move.l  d0,@Result
     end;
   end;

  { Returns a lock on the directory was loaded from }
  Function GetProgramLock: longint;
  Begin
   asm
       move.l  a6,-(sp)        { save base pointer    }
       move.l  _DosBase,a6
       jsr     _LVOGetProgramDir(a6)
       move.l  (sp)+,a6           { restore base pointer }
       move.l  d0,@Result
   end;
  end;



  Function Examine(alock :longint; var fib: TFileInfoBlock) : Boolean;
  Begin
    asm
       move.l  d2,-(sp)
       move.l  fib,d2         { pointer to FIB        }
       move.l  alock,d1
       move.l  a6,-(sp)      { save base pointer    }
       move.l  _DosBase,a6
       jsr     _LVOExamine(a6)
       move.l  (sp)+,a6           { restore base pointer }
       tst.l   d0
       bne     @success
       bra     @end
    @success:
       move.b  #1,d0
    @end:
       move.b  d0,@Result
       move.l  (sp)+,d2
    end;
  end;

  { Returns the parent directory of a lock }
  Function ParentDir(alock : longint): longint;
   Begin
     asm
       move.l  alock,d1
       move.l  a6,-(sp)       { save base pointer    }
       move.l  _DosBase,a6
       jsr     _LVOParentDir(a6)
       move.l  (sp)+,a6           { restore base pointer }
       move.l  d0,@Result
     end;
   end;


   Function FindTask(p : PChar): PProcess;
   Begin
    asm
   move.l  a6,-(sp)              { Save base pointer    }
   move.l  p,d0
   move.l  d0,a1
   move.l  _ExecBase,a6
   jsr     _LVOFindTask(a6)
   move.l  (sp)+,a6              { Restore base pointer }
   move.l  d0,@Result
    end;
   end;

FUNCTION DOSOpen(name : pCHAR; accessMode : LONGINT) : LONGINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  name,D1
    MOVE.L  accessMode,D2
    MOVEA.L _DOSBase,A6
    JSR -030(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

{$IFOPT S+}
{$DEFINE STACK_CHECK_ON}
{$ENDIF}
{$S-}
    Procedure stack_check(L: longint); assembler;[public,alias : 'FPC_STACKCHECK'];
    { Check for local variable allocation }
     asm
        tst.b   StackError       { Don't call this recursively           }
        bne     @Ok
        move.l  sp,d1            { get value of stack pointer            }

        { We must add some security, because Writing the RunError strings }
        { requires a LOT of stack space (at least 1030 bytes!)            }
        move.l  l,d0
        add.l   #2048,d0
        sub.l   d0,d1            {  sp - stack_size                      }

        move.l  _ExecBase,a0
        move.l  276(A0),A0       { ExecBase.thisTask }
        { if allocated stack_pointer - splower <= 0 then stack_ovf       }
        cmp.l   58(A0),D1        { Task.SpLower      }
        bgt     @Ok
        move.b  #1, StackError
        move.l  #202,-(sp)
        jsr     HandleError      { stack overflow    }
      @Ok:
     end;
{$IFDEF STACK_CHECK_ON}
{$S+}
{$UNDEF STACK_CHECK_ON}
{$ENDIF}

   { This routine from EXEC determines if the Ctrl-C key has }
   { been used since the last call to I/O routines.          }
   { Use to halt the program.                                }
   { Returns the state of the old signals.                   }
   Function SetSignal(newSignal: longint; SignalMask: longint): longint;
   Begin
     asm
       move.l  newSignal,d0
       move.l  SignalMask,d1
       move.l  a6,-(sp)          { save Base pointer into scratch register }
       move.l  _ExecBase,a6
       jsr     _LVOSetSignal(a6)
       move.l  (sp)+,a6
       move.l  d0,@Result
     end;
   end;


   Function AllocVec(bytesize: longint; attributes: longint):pointer;
   Begin
     asm
       move.l  bytesize,d0
       move.l  attributes,d1
       move.l  a6,-(sp)          { save Base pointer into scratch register }
       move.l  _ExecBase,a6
       jsr     _LVOAllocVec(a6)
       move.l  (sp)+,a6
       move.l  d0,@Result
     end;
   end;


   Procedure FreeVec(p: pointer);
   Begin
     asm
       move.l  p,a1
       move.l  a6,-(sp)          { save Base pointer into scratch register }
       move.l  _ExecBase,a6
       jsr     _LVOFreeVec(a6)
       move.l  (sp)+,a6
     end;
   end;

   Function GetCurrentDirName(p: pchar; l: longint): longint;
    begin
      asm
       move.l  p,d1
       move.l  l,d2
       move.l  a6,-(sp)          { save Base pointer into scratch register }
       move.l  _DosBase,a6
       jsr     _LVOGetCurrentDirName(a6)
       move.l  (sp)+,a6
       move.l  d0,@Result
      end;
    end;


    Function IoErr : longint; assembler;
    asm
       move.l  a6,-(sp)          { save Base pointer into scratch register }
       move.l  _DosBase,a6
       jsr     _LVOIOErr(a6)
       move.l  (sp)+,a6
    end;

PROCEDURE Forbid;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L _ExecBase,A6
    JSR -132(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE ReplyMsg(message : pointer);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L message,A1
    MOVEA.L _ExecBase,A6
    JSR -378(A6)
    MOVEA.L (A7)+,A6
  END;
END;

   { Converts an AMIGAOS error code to a TP compatible error code }
   Procedure Error2InOut;
   Begin
     case errno of
       ERROR_BAD_NUMBER,
       ERROR_ACTION_NOT_KNOWN,
       ERROR_NOT_IMPLEMENTED : InOutRes := 1;

       ERROR_OBJECT_NOT_FOUND : InOutRes := 2;
       ERROR_DIR_NOT_FOUND :  InOutRes := 3;

       ERROR_DISK_WRITE_PROTECTED : InOutRes := 150;

       ERROR_OBJECT_WRONG_TYPE : InOutRes := 151;

       ERROR_OBJECT_EXISTS,
       ERROR_DELETE_PROTECTED,
       ERROR_WRITE_PROTECTED,
       ERROR_READ_PROTECTED,
       ERROR_OBJECT_IN_USE,
       ERROR_DIRECTORY_NOT_EMPTY : InOutRes := 5;

       ERROR_NO_MORE_ENTRIES : InOutRes := 18;

       ERROR_RENAME_ACROSS_DEVICES : InOutRes := 17;

       ERROR_DISK_FULL : InOutRes := 101;

       ERROR_INVALID_RESIDENT_LIBRARY : InoutRes := 153;
       ERROR_BAD_HUNK : InOutRes := 153;

       ERROR_NOT_A_DOS_DISK : InOutRes := 157;

       ERROR_NO_DISK,
       ERROR_DISK_NOT_VALIDATED,
       ERROR_DEVICE_NOT_MOUNTED : InOutRes := 152;

       ERROR_SEEK_ERROR : InOutRes := 156;

       ERROR_LOCK_COLLISION,
       ERROR_LOCK_TIMEOUT,
       ERROR_UNLOCK_ERROR,
       ERROR_INVALID_LOCK,
       ERROR_INVALID_COMPONENT_NAME,
       ERROR_BAD_STREAM_NAME,
       ERROR_FILE_NOT_OBJECT : InOutRes := 6;
     else
       InOutres := errno;
     end;
     errno:=0;
   end;


    procedure CloseLibrary(lib : pointer);
    {  Close the library pointed to in lib }
    Begin
      asm
       MOVE.L  A6,-(A7)
       MOVE.L  lib,a1
       MOVE.L  _ExecBase,A6
       JSR     _LVOCloseLibrary(A6)
       MOVE.L  (A7)+,A6
      end;
    end;

PROCEDURE DOSClose(file_ : LONGINT);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  file_,D1
    MOVEA.L _DOSBase,A6
    JSR -036(A6)
    MOVEA.L (A7)+,A6
  END;
END;



   Function KickVersion: word; assembler;
   asm
     move.l  _ExecBase, a0       { Get Exec Base                           }
     move.w  20(a0), d0          { Return version - version at this offset }
   end;


  { ************************ AMIGAOS SUPP ROUTINES ************************* }
procedure EraseList(l:flist);
var
  temp : pFileList;
begin
  while l <> nil do begin
     temp := l;
     l := l^.next;
     dispose(temp);
  end;
end;


  Procedure CloseList(l: flist);
  (***********************************************************************)
  (* PROCEDURE CloseList                                                 *)
  (*  Description: This routine each time the program is about to        *)
  (*  terminate, it closes all opened file handles, as this is not       *)
  (*  handled by the operating system.                                   *)
  (*   p -> Start of linked list of opened files                         *)
  (***********************************************************************)
  var hpek : pFileList;
    h : longint;
  begin
    hpek := l^.next;
    while hpek <> nil do 
     begin
        h := hpek^.handle;
        if (h <> StdInputHandle) and (h <> StdOutputHandle) and (h <> StdErrorHandle) then begin
         { directly close file here, it is faster then doing }
         { it do_close.                                      }
          asm
            move.l  h,d1
            move.l  a6,-(sp)              { save a6 }
            move.l  _DOSBase,a6
            jsr     _LVOClose(a6)
            move.l  (sp)+,a6              { restore a6 }
          end;
      end;
      hpek := hpek^.next;
    end;
    EraseList(l);
  end;

procedure CreateHead(var p : flist);
begin
   New(p);
   p^.next := nil;
end;

procedure InsertIntoList(var l:flist; p:pFileList);
begin
  p^.next:=l^.next;
  l^.next := p;
end;

function IsInlist(h : longint; l:flist):boolean;
var
   p : pFileList;
   found : boolean;
begin
   found := false;
   p := l;
   while (p^.next <> nil) and (not found) do
      if p^.next^.handle = h then found := true
      else p := p^.next;
   IsInList := found;
end;

  (***********************************************************************)
  (* PROCEDURE AddToList                                                 *)
  (*  Description: Adds a node to the linked list of files.              *)
  (*                                                                     *)
  (*   p -> Start of File list linked list, if not allocated allocates   *)
  (*        it for you.                                                  *)
  (*   h -> handle of file to add                                        *)
  (***********************************************************************)
procedure AddToList(var l:flist; h : longint);
var
   p : pFileList;
begin
   if not IsInList(h,l) then begin
      new(p);
      p^.handle := h;
      InsertIntoList(l,p);
   end;
end;



procedure DeletePost( p: pFileList);
var
   t : pFileList;
begin
   t := p^.next;
   if t <> nil then begin
      p^.next := p^.next^.next;
      dispose(t);
   end;
end;

function FindPost(h : longint; l:flist):pFileList;
var
   p : pFileList;
   found : boolean;
begin
   found := false;
   p := l;
   while (p^.next <> nil) and (not found) do
      if p^.next^.handle = h then found := true
      else p := p^.next;
   FindPost := p;
end;



  Procedure SetClosedList(var p: flist; h: longint);
  { Set the file flag to closed if the file is being closed }
  var
   hp: flist;
  Begin
      hp := FindPost(h,p);
      if hp^.next <> nil then
   Deletepost(hp);
  end;


{*****************************************************************************
       System Dependent Exit code
*****************************************************************************}


    Procedure system_exit;
    var
     i: byte;
    Begin
      { We must remove the CTRL-C FALG here because halt }
      { may call I/O routines, which in turn might call  }
      { halt, so a recursive stack crash                 }
      if BreakOn then 
        begin
         IF (SetSignal(0,0) AND SIGBREAKF_CTRL_C) <> 0 THEN
            SetSignal(0,SIGBREAKF_CTRL_C);
        end;
      { Close remaining opened files }
      CloseList(FileList);
      if (OrigDir <> 0) then
       Begin
          Unlock(CurrentDir(OrigDir));
          OrigDir := 0;
       end;
       { close the libraries }
       if myhandle <> 0 then 
          DOSClose(myhandle);
       If _UtilityBase <> nil then
          CloseLibrary(_UtilityBase);
       If _DosBase <> nil then
          CloseLibrary(_DosBase);
       If _IntuitionBase <> nil then
          CloseLibrary(_IntuitionBase);
       _UtilityBase := nil;
       _DosBase := nil;
       _IntuitionBase := nil;
       if _WBenchMsg <> nil then
         begin
           Forbid;
           ReplyMsg(_WBenchMsg);
          _WBenchmsg := nil;
         end;
       { Dispose of extraneous allocated pointers }
       for I:=1 to max_os_allocs do
         Begin
           if assigned(pointerlist[i]) then 
              FreeVec(pointerlist[i]);
         end;
       { return to caller }
       { with correct exit code }
       asm
         clr.l   d0
         move.w  exitcode,d0
         move.l  STKPTR,sp
         rts
      end;
    end;




  { ************************ PARAMCOUNT/PARAMSTR *************************** }

      function paramcount : longint;
      Begin
        if WBStarted then paramcount := 0
          else paramcount := argc - 1;
      end;


      function args : pointer; assembler;
      asm
         move.l __ARGS,d0
      end;

   Function GetParamCount(const p: pchar): longint;
   var
    i: word;
    count: word;
   Begin
    i:=0;
    count:=0;
    while p[count] <> #0 do
     Begin
       if (p[count] <> ' ') and (p[count] <> #9) and (p[count] <> #0) then
         Begin
           i:=i+1;
           while (p[count] <> ' ') and (p[count] <> #9) and (p[count] <> #0) do
               count:=count+1;
         end;
          if p[count] = #0 then break;
       count:=count+1;
     end;
     GetParamCount:=longint(i);
   end;


   Function GetParam(index: word; const p : pchar): string;
   { On Entry: index = string index to correct parameter  }
   { On exit:  = correct character index into pchar array }
   { Returns correct index to command line argument }
   var
    count: word;
    localindex: word;
    l: byte;
    temp: string;
   Begin
     if WBStarted then begin
  GetParam := '';
     end else begin
  temp:='';
  count := 0;
  { first index is one }
  localindex := 1;
  l:=0;
  While p[count] <> #0 do
    Begin
      if (p[count] <> ' ') and (p[count] <> #9) then
        Begin
    if localindex = index then
     Begin
      while (p[count] <> #0) and (p[count] <> ' ') and (p[count] <> #9) and (l < 256) do
       Begin
         temp:=temp+p[count];
         l:=l+1;
         count:=count+1;
       end;
      temp[0]:=char(l);
      GetParam:=temp;
      exit;
     end;
    { Point to next argument in list }
    while (p[count] <> #0) and (p[count] <> ' ') and (p[count] <> #9) do
      Begin
        count:=count+1;
    end;
        localindex:=localindex+1;
       end;
      if p[count] = #0 then break;
      count:=count+1;
    end;
   GetParam:=temp;
      end;
   end;


    Function GetProgramDir : String;
    var
     s1: string;
     alock: longint;
     counter : byte;
    Begin
     FillChar(s1,255,#0);
     { GetLock of program directory }
     asm
      move.l  a6,-(sp)              { save a6 }
      move.l  _DOSBase,a6
      jsr     _LVOGetProgramDir(a6)
      move.l  (sp)+,a6              { restore a6 }
      move.l  d0,alock           { save the lock }
     end;
     if alock <> 0 then
      Begin
        { Get the name from the lock! }
        asm
          movem.l d2/d3,-(sp)        { save used registers             }
          move.l  alock,d1
          lea     s1,a0              { Get pointer to string!          }
          move.l  a0,d2
          add.l   #1,d2              { let  us point past the length byte! }
          move.l  #255,d3
          move.l  a6,-(sp)           { save a6 }
          move.l  _DOSBase,a6
          jsr     _LVONameFromLock(a6)
          move.l  (sp)+,a6           { restore a6 }
          movem.l (sp)+,d2/d3
        end;
        { no check out the length of the string }
        counter := 1;
        while s1[counter] <> #0 do
           Inc(counter);
        s1[0] := char(counter-1);
        GetProgramDir := s1;
     end
    else
      GetProgramDir := '';
    end;


    Function GetProgramName : string;
    { Returns ONLY the program name }
    { There seems to be a bug in v39 since if the program is not }
    { called from its home directory the program name will also  }
    { contain the path!                                          }
    var
     s1: string;
     counter : byte;
    Begin
      FillChar(s1,255,#0);
      asm
        move.l  d2,-(sp)           { Save used register      }
        lea     s1,a0              { Get pointer to string!  }
        move.l  a0,d1
        add.l   #1,d1              { point to correct offset }
        move.l  #255,d2
        move.l  a6,-(sp)           { save a6 }
        move.l  _DOSBase,a6
        jsr     _LVOGetProgramName(a6)
        move.l  (sp)+,a6           { restore a6 }
        move.l  (sp)+,d2           { restore saved register }
      end;
      { no check out and assign the length of the string }
      counter := 1;
      while s1[counter] <> #0 do
         Inc(counter);
      s1[0] := char(counter-1);
      { now remove any component path which should not be there }
      for counter:=length(s1) downto 1 do
          if (s1[counter] = '/') or (s1[counter] = ':') then break;
      { readjust counterv to point to character }
      if counter <> 1 then
          Inc(counter);
      GetProgramName:=copy(s1,counter,length(s1));
    end;


    function paramstr(l : longint) : string;
      var
       p : pchar;
       s1 : string;
      begin
        if WBStarted then begin
            paramstr := '';
        end else begin
        {   -> Call AmigaOS GetProgramName                             }
        if l = 0 then
          Begin
            s1 := GetProgramDir;
            { If this is a root, then simply don't add '/' }
            if s1[length(s1)] = ':' then
               paramstr:=s1+GetProgramName
            else
               { add backslash directory }
               paramstr:=s1+'/'+GetProgramName
          end
        else
          if (l>0) and (l<=paramcount) then
            begin
              p:=args;
              paramstr:=GetParam(word(l),p);
            end
        else paramstr:='';
       end;
      end;

   Procedure GenerateArgs;

   var
     ArgvLen : longint;

     procedure allocarg(idx,len:longint);
     var
        i,oldargvlen : longint;
     begin
       if idx>=argvlen then
        begin
          oldargvlen:=argvlen;
          argvlen:=(idx+8) and (not 7);
          sysreallocmem(argv,argvlen*sizeof(pointer));
          for i:=oldargvlen to argvlen-1 do
            argv[i]:=nil;
        end;
       { use realloc to reuse already existing memory }
        sysreallocmem(argv[idx],len+1);
     end;

   var
    count: word;
    start,_end : word;
    localindex: word;
    p : pchar;
    temp : string;

   Begin
        p:=args;
        argvlen:=0;
        { Set argv[0] }
        temp:=paramstr(0);
        allocarg(0,length(temp));
        move(temp[1],argv[0]^,length(temp));
        argv[0][length(temp)]:=#0;
        { Handle the other args }
        count := 0;
        if WBStarted then
          begin
            argc:=0;
            exit;
          end;
        { first index is one }
        localindex := 1;
        While p[count] <> #0 do
            Begin
              while (p[count] = ' ') or (p[count] = #9) do
                  inc(count);
              start:=count;
              while (p[count] <> #0) and (p[count] <> ' ') and (p[count] <> #9) do
                 count:=count+1;
              if (count-start>0) then
                begin
                  allocarg(localindex,count-start);
                  move(p[start],argv[localindex]^,count-start);
                  argv[localindex][count-start]:=#0;
                end;
              localindex:=localindex+1;
              if p[count] = #0 then break;
                count:=count+1;
            end;
        argc:=localindex;
   end;


  { ************************************************************************ }

    procedure randomize;

      var
       hl : longint;
         time : TDateStamp;
      begin
         DateStamp(time);
         randseed:=time.ds_tick;
      end;


  
  function getheapstart:pointer;
    begin
      getheapstart := @_HEAP;
    end;


  function getheapsize:longint;
    begin
      getheapsize := _HEAPSIZE;
    end;

  { This routine is used to grow the heap.  }
  { But here we do a trick, we say that the }
  { heap cannot be regrown!                 }
  function sbrk( size: longint): longint;
  var
  { on exit -1 = if fails.               }
   p: pointer;
   i: byte;
  Begin
    p:=0;
    { Is the pointer list full }
    if pointerlist[max_os_allocs] <> nil then
    begin
     { yes, then don't allocate and simply exit }
     sbrk:=-1;
     exit;
    end;
    { Allocate best available memory }
    p:=AllocVec(size,0);
    if p = nil then
     sbrk:=-1
    else
    Begin
       i:=1;
       { add it to the list of allocated pointers }
       { first find the last pointer in the list  }
       while (i <= max_os_allocs) and (pointerlist[i] <> nil) do
         i:=i+1;
       if (i > max_os_allocs) then
         begin
           sbrk:= -1;
           exit;
         end;
       pointerlist[i]:=p;
       sbrk:=longint(p);
    end;
  end;



{$I heap.inc}


{****************************************************************************
        Low Level File Routines
 ****************************************************************************}

procedure do_close(h : longint);
{ We cannot check for CTRL-C because this routine will be called }
{ on HALT to close all remaining opened files. Therefore no      }
{ CTRL-C checking otherwise a recursive call might result!       }
{$ifdef debug}
var
  buffer: array[0..255] of char;
{$endif}
begin
  { check if the file handle is in the list }
  { if so the put its field to closed       }
SetClosedList(FileList,h);
{$ifdef debug}
  asm
     move.l  h,d1
     move.l  a6,-(sp)
     move.l  d2,-(sp)
     move.l  d3,-(sp)
     lea     buffer,a0
     move.l  a0,d2
     move.l  #255,d3
     move.l  _DosBase,a6
     jsr     _LVONameFromFH(a6)
     move.l  (sp)+,d3
     move.l  (sp)+,d2
     move.l  (sp)+,a6
  end;
  WriteLn(Buffer);
{$endif debug}
  asm
     move.l  h,d1
     move.l  a6,-(sp)              { save a6 }
     move.l  _DOSBase,a6
     jsr     _LVOClose(a6)
     move.l  (sp)+,a6              { restore a6 }
  end;
end;


function do_isdevice(handle:longint):boolean;
begin
  if (handle=stdoutputhandle) or (handle=stdinputhandle) or
  (handle=stderrorhandle) then
    do_isdevice:=TRUE
  else
    do_isdevice:=FALSE;
end;



procedure do_erase(p : pchar);
begin
  if BreakOn then begin
  if (Setsignal(0,0) AND SIGBREAKF_CTRL_C) <> 0 then
   Begin
     SetSignal(0,SIGBREAKF_CTRL_C);
     Halt(CTRL_C);
   end;
   end;
  asm
     move.l  a6,-(sp)            { save a6 }

     move.l  p,d1
     move.l  _DOSBase,a6
     jsr     _LVODeleteFile(a6)
     tst.l   d0                  { zero = failure }
     bne     @noerror

     jsr     _LVOIoErr(a6)
     move.L  d0,errno

   @noerror:
     move.l  (sp)+,a6            { restore a6 }
  end;
  if errno <> 0 then
     Error2InOut;
end;


procedure do_rename(p1,p2 : pchar);
begin
  if BreakOn then begin
  if (Setsignal(0,0) AND SIGBREAKF_CTRL_C) <> 0 then
   Begin
     SetSignal(0,SIGBREAKF_CTRL_C);
     Halt(CTRL_C);
   end;
   end;
  asm
     move.l  a6,-(sp)               { save a6 }
     move.l  d2,-(sp)               { save d2 }

     move.l  p1,d1
     move.l  p2,d2
     move.l  _DOSBase,a6
     jsr     _LVORename(a6)
     move.l  (sp)+,d2               { restore d2 }
     tst.l   d0
     bne     @dosreend              { if zero = error }
     jsr     _LVOIoErr(a6)
     move.L  d0,errno
   @dosreend:
     move.l  (sp)+,a6                  { restore a6 }
  end;
  if errno <> 0 then
    Error2InOut;
end;


function do_write(h,addr,len : longint) : longint;
begin
  if BreakOn then begin
  if (Setsignal(0,0) AND SIGBREAKF_CTRL_C) <> 0 then
   Begin
     SetSignal(0,SIGBREAKF_CTRL_C);
     Halt(CTRL_C);
   end;
   end;
  if len <= 0 then
   Begin
    do_write:=0;
    exit;
   end;
  asm
      move.l  a6,-(sp)

      movem.l d2/d3,-(sp)
      move.l  h,d1             { we must of course set up the }
      move.l  addr,d2          { parameters BEFORE getting    }
      move.l  len,d3           { _DOSBase                     }
      move.l  _DOSBase,a6
      jsr     _LVOWrite(a6)
      movem.l (sp)+,d2/d3

      cmp.l   #-1,d0
      bne     @doswrend              { if -1 = error }
      jsr     _LVOIoErr(a6)
      move.l  d0,errno
      bra     @doswrend2
    @doswrend:
      { we must restore the base pointer before setting the result }
      move.l  (sp)+,a6
      move.l  d0,@RESULT
      bra     @end
    @doswrend2:
      move.l  (sp)+,a6
    @end:
  end;
  If errno <> 0 then
    Error2InOut;
end;


function do_read(h,addr,len : longint) : longint;
begin
  if BreakOn then begin
  if (Setsignal(0,0) AND SIGBREAKF_CTRL_C) <> 0 then
   Begin
     SetSignal(0,SIGBREAKF_CTRL_C);
     Halt(CTRL_C);
   end;
   end;
  if len <= 0 then
  Begin
     do_read:=0;
     exit;
  end;
  asm
      move.l  a6,-(sp)

      movem.l d2/d3,-(sp)
      move.l  h,d1         { we must set up aparamters BEFORE }
      move.l  addr,d2      { setting up a6 for the OS call    }
      move.l  len,d3
      move.l  _DOSBase,a6
      jsr     _LVORead(a6)
      movem.l (sp)+,d2/d3

      cmp.l   #-1,d0
      bne     @doswrend              { if -1 = error }
      jsr     _LVOIoErr(a6)
      move.l  d0,errno
      bra     @doswrend2
    @doswrend:
      { to store a result for the function  }
      { we must of course first get back the}
      { base pointer!                       }
      move.l  (sp)+,a6
      move.l  d0,@RESULT
      bra     @end
    @doswrend2:
      move.l  (sp)+,a6
    @end:
  end;
  If errno <> 0 then
    Error2InOut;
end;


function do_filepos(handle : longint) : longint;
begin
  if BreakOn then begin
  if (Setsignal(0,0) AND SIGBREAKF_CTRL_C) <> 0 then
   Begin
     { Clear CTRL-C signal }
     SetSignal(0,SIGBREAKF_CTRL_C);
     Halt(CTRL_C);
   end;
   end;
  asm
       move.l  a6,-(sp)

       move.l  handle,d1
       move.l  d2,-(sp)
       move.l  d3,-(sp)              { save registers              }

       clr.l   d2                    { offset 0 }
       move.l  #0,d3                 { OFFSET_CURRENT }
       move.l  _DOSBase,a6
       jsr    _LVOSeek(a6)

       move.l  (sp)+,d3              { restore registers }
       move.l  (sp)+,d2
       cmp.l   #-1,d0                { is there a file access error? }
       bne     @noerr
       jsr     _LVOIoErr(a6)
       move.l  d0,errno
       bra     @fposend
      @noerr:
       move.l  (sp)+,a6            { restore a6 }
       move.l  d0,@Result
       bra     @end
      @fposend:
       move.l  (sp)+,a6                 { restore a6 }
      @end:
  end;
  If errno <> 0 then
    Error2InOut;
end;


procedure do_seek(handle,pos : longint);
begin
  if BreakOn then begin
  if (Setsignal(0,0) AND SIGBREAKF_CTRL_C) <> 0 then
   Begin
     { Clear CTRL-C signal }
     SetSignal(0,SIGBREAKF_CTRL_C);
     Halt(CTRL_C);
   end;
   end;
  asm
       move.l  a6,-(sp)

       move.l  handle,d1
       move.l  d2,-(sp)
       move.l  d3,-(sp)              { save registers              }

       move.l  pos,d2
       { -1 }
       move.l  #$ffffffff,d3          { OFFSET_BEGINNING }
       move.l  _DOSBase,a6
       jsr    _LVOSeek(a6)

       move.l  (sp)+,d3              { restore registers }
       move.l  (sp)+,d2
       cmp.l   #-1,d0                { is there a file access error? }
       bne     @noerr
       jsr     _LVOIoErr(a6)
       move.l  d0,errno
       bra     @seekend
      @noerr:
      @seekend:
       move.l  (sp)+,a6                 { restore a6 }
  end;
  If errno <> 0 then
    Error2InOut;
end;


function do_seekend(handle:longint):longint;
begin
  if BreakOn then begin
  if (Setsignal(0,0) AND SIGBREAKF_CTRL_C) <> 0 then
   Begin
     { Clear CTRL-C signal }
     SetSignal(0,SIGBREAKF_CTRL_C);
     Halt(CTRL_C);
   end;
   end;
  asm
       { seek from end of file }
       move.l  a6,-(sp)

       move.l  handle,d1
       move.l  d2,-(sp)
       move.l  d3,-(sp)              { save registers              }

       clr.l   d2
       move.l  #1,d3                 { OFFSET_END }
       move.l  _DOSBase,a6
       jsr    _LVOSeek(a6)

       move.l  (sp)+,d3              { restore registers }
       move.l  (sp)+,d2
       cmp.l   #-1,d0                { is there a file access error? }
       bne     @noerr
       jsr     _LVOIoErr(a6)
       move.l  d0,errno
       bra     @seekend
      @noerr:
       move.l  (sp)+,a6              { restore a6 }
       move.l  d0,@Result
       bra     @end
      @seekend:
       move.l  (sp)+,a6              { restore a6 }
      @end:
  end;
  If Errno <> 0 then
    Error2InOut;
end;


function do_filesize(handle : longint) : longint;
var
  aktfilepos : longint;
begin
  if BreakOn then begin
  if (Setsignal(0,0) AND SIGBREAKF_CTRL_C) <> 0 then
    Begin
     { Clear CTRL-C signal }
     SetSignal(0,SIGBREAKF_CTRL_C);
     Halt(CTRL_C);
    end;
    end;
   aktfilepos:=do_filepos(handle);
   { We have to do this two times, because seek returns the }
   { OLD position                                           }
   do_filesize:=do_seekend(handle);
   do_filesize:=do_seekend(handle);
   do_seek(handle,aktfilepos);
end;


procedure do_truncate (handle,pos:longint);
begin
      { Point to the end of the file }
      { with the new size            }
      asm
      @noerr_one:                          { Seek a second time            }
       move.l  a6,-(sp)              { Save base pointer             }

       move.l  handle,d1
       move.l  d2,-(sp)
       move.l  d3,-(sp)              { save registers                }

       move.l  pos,d2
       move.l  #-1,d3                { Setup correct move type     }
       move.l  _DOSBase,a6           { from beginning of file      }
       jsr    _LVOSetFileSize(a6)

       move.l  (sp)+,d3              { restore registers }
       move.l  (sp)+,d2
       cmp.l   #-1,d0                { is there a file access error? }
       bne     @noerr
       jsr     _LVOIoErr(a6)
       move.l  d0,errno              { Global variable, so no need    }
      @noerr:                              { to restore base pointer now    }
       move.l  (sp)+,a6              { Restore base pointer           }
      end;
  If Errno <> 0 then
    Error2InOut;
end;


procedure do_open(var f;p:pchar;flags:longint);
{
  filerec and textrec have both handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $100)   the file will be append
  when (flags and $1000)  the file will be truncate/rewritten
  when (flags and $10000) there is no check for close (needed for textfiles)
}
var
  i,j : longint;
  oflags: longint;
  path : string;
  buffer : array[0..255] of char;
  index : integer;
  s : string;
begin
 path:=strpas(p);
 for index:=1 to length(path) do
   if path[index]='\' then path[index]:='/';
 { remove any dot characters and replace by their current }
 { directory equivalent.                                  }
 if pos('../',path) = 1 then
 { look for parent directory }
    Begin
       delete(path,1,3);
       getdir(0,s);
       j:=length(s);
       while (s[j] <> '/') AND (s[j] <> ':') AND (j > 0 ) do
          dec(j);
       if j > 0 then
          s:=copy(s,1,j);
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
  move(path[1],buffer,length(path));
  buffer[length(path)]:=#0;
 { close first if opened }
  if ((flags and $10000)=0) then
   begin
     case filerec(f).mode of
      fminput,fmoutput,fminout : Do_Close(filerec(f).handle);
      fmclosed : ;
     else
      begin
        inoutres:=102; {not assigned}
        exit;
      end;
     end;
   end;
  { reset file handle }
  filerec(f).handle:=UnusedHandle;
  { convert filemode to filerec modes }
  { READ/WRITE on existing file }
  { RESET/APPEND                }
  oflags := 1005;
  case (flags and 3) of
   0 : filerec(f).mode:=fminput;
   1 : filerec(f).mode:=fmoutput;
   2 : filerec(f).mode:=fminout;
  end;
  { rewrite (create a new file) }
  if (flags and $1000)<>0 then
     oflags := 1006;
{ empty name is special }
  if p[0]=#0 then
   begin
     case filerec(f).mode of
       fminput : filerec(f).handle:=StdInputHandle;
      fmappend,
      fmoutput : begin
       filerec(f).handle:=StdOutputHandle;
       filerec(f).mode:=fmoutput; {fool fmappend}
     end;
     end;
     exit;
   end;
   asm
       move.l  a6,-(sp)               { save a6 }
       move.l  d2,-(sp)
       lea     buffer,a0
       move.l  a0,d1
       move.l  oflags,d2               { MODE_READWRITE }
       move.l  _DOSBase,a6
       jsr     _LVOOpen(a6)
       tst.l   d0
       bne     @noopenerror           { on zero an error occured }
       jsr     _LVOIoErr(a6)
       move.l  d0,errno
       bra     @openend
    @noopenerror:
       move.l  (sp)+,d2
       move.l  (sp)+,a6              { restore a6 }
       move.l  d0,i                  { we need the base pointer to access this variable }
       bra     @end
    @openend:
       move.l  (sp)+,d2
       move.l  (sp)+,a6              { restore a6 }
    @end:
   end;
    if Errno = 0 then
    { No error, add file handle to linked list }
    { this must be checked before the call to  }
    { Error2InIOut since it resets Errno to 0  }
    AddToList(FileList,i);
    If Errno <> 0 then
       Error2InOut;

    filerec(f).handle:=i;
   { append mode }
   if (flags and $100)<>0 then
    begin
      do_seekend(filerec(f).handle);
      filerec(f).mode:=fmoutput; {fool fmappend}
    end;
end;

{*****************************************************************************
         UnTyped File Handling
*****************************************************************************}

{$i file.inc}

{*****************************************************************************
         Typed File Handling
*****************************************************************************}

{$i typefile.inc}

{*****************************************************************************
         Text File Handling
*****************************************************************************}

{$i text.inc}

{*****************************************************************************
         Directory Handling
*****************************************************************************}

procedure mkdir(const s : string);[IOCheck];
var
  buffer : array[0..255] of char;
  j: Integer;
  temp : string;
begin
  if BreakOn then begin
  { We must check the Ctrl-C before IOChecking of course! }
  if (Setsignal(0,0) AND SIGBREAKF_CTRL_C) <> 0 then
   Begin
     { Clear CTRL-C signal }
     SetSignal(0,SIGBREAKF_CTRL_C);
     Halt(CTRL_C);
   end;
   end;
  If (s='') or (InOutRes <> 0) then
   exit;
  temp:=s;
  for j:=1 to length(temp) do
    if temp[j] = '\' then temp[j] := '/';
  move(temp[1],buffer,length(temp));
  buffer[length(temp)]:=#0;
  asm
  move.l  a6,-(sp)
  { we must load the parameters BEFORE setting up the }
  { OS call with a6                                   }
  lea     buffer,a0
  move.l  a0,d1
  move.l  _DosBase,a6
  jsr     _LVOCreateDir(a6)
  tst.l   d0
  bne     @noerror
  jsr     _LVOIoErr(a6)
  move.l  d0,errno
  bra     @end
@noerror:
  { Now we must unlock the directory }
  { d0 = lock returned by create dir }
  move.l  d0,d1
  jsr     _LVOUnlock(a6)
@end:
  { restore base pointer }
  move.l  (sp)+,a6
  end;
  If errno <> 0 then
    Error2InOut;
end;


procedure rmdir(const s : string);[IOCheck];
var
  buffer : array[0..255] of char;
  j : Integer;
  temp : string;
begin
  if BreakOn then begin
  { We must check the Ctrl-C before IOChecking of course! }
  if (Setsignal(0,0) AND SIGBREAKF_CTRL_C) <> 0 then
   Begin
     { Clear CTRL-C signal }
     SetSignal(0,SIGBREAKF_CTRL_C);
     Halt(CTRL_C);
   end;
   end;
  if (s = '.' ) then
    InOutRes := 16;
  If (s='') or (InOutRes <> 0) then
   exit;
  temp:=s;
  for j:=1 to length(temp) do
    if temp[j] = '\' then temp[j] := '/';
  move(temp[1],buffer,length(temp));
  buffer[length(temp)]:=#0;
  do_erase(buffer);
end;



procedure chdir(const s : string);[IOCheck];
var
  buffer : array[0..255] of char;
  alock : longint;
  FIB :pFileInfoBlock;
  j: integer;
  temp : string;
begin
  if BreakOn then begin
  if (Setsignal(0,0) AND SIGBREAKF_CTRL_C) <> 0 then
   Begin
     { Clear CTRL-C signal }
     SetSignal(0,SIGBREAKF_CTRL_C);
     Halt(CTRL_C);
   end;
   end;
  If (s='') or (InOutRes <> 0) then
   exit;
  temp:=s;
  for j:=1 to length(temp) do
    if temp[j] = '\' then temp[j] := '/';
  { Return parent directory }
  if s = '..' then
  Begin
       getdir(0,temp);
       j:=length(temp);
       { Look through the previous paths }
       while (temp[j] <> '/') AND (temp[j] <> ':') AND (j > 0 ) do
   dec(j);
       if j > 0 then
   temp:=copy(temp,1,j);
  end;
  alock := 0;
  fib:=nil;
  new(fib);

  move(temp[1],buffer,length(temp));
  buffer[length(temp)]:=#0;
  { Changing the directory is a pretty complicated affair }
  {   1) Obtain a lock on the directory                   }
  {   2) CurrentDir the lock                              }
  asm
    lea      buffer,a0
    move.l   a0,d1      { pointer to buffer in d1  }
    move.l   a6,-(sp)   { Save base pointer        }
    move.l   d2,-(sp)   { save d2 register         }
    move.l   #-2,d2     { ACCESS_READ lock         }
    move.l   _DosBase,a6
    jsr      _LVOLock(a6){ Lock the directory      }
    move.l   (sp)+,d2   { Restore d2 register      }
    tst.l    d0         { zero = error!            }
    bne      @noerror
    jsr      _LVOIoErr(a6)
    move.l   (sp)+,a6   { reset base pointer       }
    move.l   d0,errno
    bra      @End
  @noerror:
    move.l   (sp)+,a6   { reset base pointer       }
    move.l   d0,alock   { save the lock            }
  @End:
  end;
  If errno <> 0 then
   Begin
     Error2InOut;
     exit;
   end;
  if (Examine(alock, fib^) = TRUE) AND (fib^.fib_DirEntryType > 0) then
    Begin
      alock := CurrentDir(alock);
      if OrigDir = 0 then
  Begin
    OrigDir := alock;
    alock := 0;
  end;
    end;
  if alock <> 0 then
    Unlock(alock);
  if assigned(fib) then dispose(fib);
end;




  Procedure GetCwd(var path: string);
    var
      p : array[0..1023] of char;
    Begin
      if GetCurrentDirName(p,1024)=0 then
        begin
           path := '';
           errno := IoErr;
        end
      else
        path:=strpas(p);
    end;


procedure getdir(drivenr : byte;var dir : string);
begin
  if BreakOn then begin
  if (Setsignal(0,0) AND SIGBREAKF_CTRL_C) <> 0 then
    Begin
      { Clear CTRL-C signal }
      SetSignal(0,SIGBREAKF_CTRL_C);
      Halt(CTRL_C);
    end;
    end;
  GetCwd(dir);
  If errno <> 0 then
     Error2InOut;
end;



{*****************************************************************************
       SystemUnit Initialization
*****************************************************************************}
Procedure Startup; Assembler;
asm
    move.l  a6,-(sp)      { save a6             }

    move.l  (4),a6        { get ExecBase pointer }
    move.l  a6,_ExecBase
    suba.l  a1,a1
    jsr     _LVOFindTask(a6)
    move.l  d0,a4
    { Check the stack value }

    {   are we running from a CLI?             }

    tst.l   172(a4)         { 172 = pr_CLI     }
    bne     @fromCLI

    lea     92(a4),a0
    jsr     _LVOWaitPort(a6)
    lea     92(a4),a0
    jsr     _LVOGetMsg(a6)
    move.l  d0,_WBenchMsg
    bra     @openlibs



@fromCLI:
    {  Open the following libraries:            }
    {   Intuition.library                       }
    {   dos.library                             }
    move.l   #0,_WBenchMsg

@openlibs:
    moveq.l  #0,d0
    move.l   intuitionname,a1      { directly since it is a pchar }
    jsr      _LVOOpenLibrary(a6)
    move.l   d0,_IntuitionBase
    beq      @exitprg

    moveq.l  #0,d0
    move.l   utilityname,a1        { directly since it is a pchar }
    jsr      _LVOOpenLibrary(a6)
    move.l   d0,_UtilityBase
    beq      @exitprg

    moveq.l  #0,d0
    move.l   dosname,a1            { directly since it is a pchar }
    jsr      _LVOOpenLibrary(a6)
    move.l   d0,_DOSBase
    beq      @exitprg

    tst.l    _WBenchMsg
    bne      @startupend

    { Find standard input and output               }
    { for CLI                                      }
@OpenFiles:
    move.l  _DOSBase,a6
    jsr     _LVOInput(a6)        { get standard in                   }
    move.l  d0, StdInputHandle   { save standard Input handle        }
{    move.l  d0,d1               }{ set up for next call              }
{   jsr     _LVOIsInteractive(a6)}{ is it interactive?             }
{   move.l  #_Input,a0          }{ get file record again             }
{   move.b  d0,INTERACTIVE(a0)  }{ set flag                          }
{   beq     StdInNotInteractive }{ skip this if not interactive    }
{   move.l  BUFFER(a0),a1       }{ get buffer address                }
{   add.l   #1,a1               }{ make end one byte further on      }
{   move.l  a1,MAX(a0)          }{ set buffer size                   }
{   move.l  a1,CURRENT(a0)      }{ will need a read                  }
    bra     @OpenStdOutput
@StdInNotInteractive
{    jsr _p%FillBuffer     }      { fill the buffer                   }
@OpenStdOutput
    jsr     _LVOOutput(a6)      { get ouput file handle             }
    move.l  d0,StdOutputHandle  { get file record                   }
    bra     @startupend
{    move.l  d0,d1             }  { set up for call                   }
{    jsr _LVOIsInteractive(a6) }  { is it interactive?                }
{    move.l  #_Output,a0       }  { get file record                   }
{    move.b  d0,INTERACTIVE(a0)}  { set flag                          }
@exitprg:
     move.l (sp)+,a6             { restore a6                        }
     move.l #219,-(sp)
     jsr    HandleError
@startupend:
     move.l (sp)+,a6             { restore a6                        }
end;


procedure configwb;
begin
    myhandle := DOSOpen(ConName,1005);
    if myhandle <> 0 then begin
    StdInputHandle := (myhandle);
    StdOutputHandle := (myhandle);
    end else Halt(1);
end;

begin
  StackError := false;
  errno:= 0;
{  Initial state is on -- in case of RunErrors before the i/o handles are }
{  ok.                                                                    }
  Initial:=TRUE;
{ Initialize ExitProc }
  ExitProc:=Nil;
  Startup;
  if _WBenchMsg <> nil then 
   begin
    WBStarted := true;
    IsConsole := false;
   end;
{ Setup heap }
  InitHeap;
{ Setup stdin, stdout and stderr }

  if _WBenchMsg <> nil then configwb;
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);
  { The Amiga does not seem to have a StdError }
  { handle, therefore make the StdError handle }
  { equal to the StdOutputHandle.              }
  StdErrorHandle := StdOutputHandle;
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
{ Now Handles and function handlers are setup }
{ correctly.                                  }
  Initial:=FALSE;
{ Reset IO Error }
  InOutRes:=0;

  { Only AmigaOS v2.04 or greater is supported }
  If KickVersion < 36 then
   Begin
     WriteLn('v36 or greater of Kickstart required.');
     Halt(1);
   end;
   GenerateArgs;
   OrigDir := 0;
   CreateHead(FileList);
   old_Exit:=exitproc;
end.


{
  $Log: sysamiga.pas,v $
  Revision 1.1.2.17  2002/11/04 19:21:44  carl
    * errno is a long not a word!
    + GetCurrentDriname is used for getcwd() : this fixes a problem
       when getting the cwd on a different volume than the one where the
      app. was started.

  Revision 1.1.2.16  2002/11/02 16:25:44  carl
    * Patches from Nils : Console window attributes changed.
    + IsConsole = FALSE when WBench app.

  Revision 1.1.2.15  2002/10/29 20:58:35  carl
    * several updates for Amiga

  Revision 1.1.2.14  2002/10/20 16:11:38  carl
    * dos : fsplit / fexpand bugfixes
    * makefile updates for cycle
    * sysamiga : more memory blocks can be allocated (up 16 Mbytes of RAM)

  Revision 1.1.2.13  2002/10/19 14:45:53  carl
    * fix some problems with argc/argv setup

  Revision 1.1.2.12  2002/10/06 18:55:22  carl
    * bugfixes of pointerlist when allocating memory
    * getenv returns something now to avoid crashes

  Revision 1.1.2.11  2002/10/06 13:48:47  carl
    * fix stupid crash in chdir() (a6 was not saved in the correct location)
    * avoid calling stack checking routine recursively

  Revision 1.1.2.10  2002/10/05 22:40:46  carl
    * bugfix of stack checking (code was completely invalid)

  Revision 1.1.2.9  2002/09/15 16:45:53  carl
    * missing openstdio added (RunError would not work)

  Revision 1.1.2.8  2002/03/10 11:40:26  carl
  * updated with other RTL's
  * InOutRes := 16 with rmdir()

  Revision 1.1.2.7  2001/11/07 15:26:22  michael
  + Added OS describing constants

  Revision 1.1.2.6  2001/07/24 07:30:56  pierre
    + getopt compatible argc and argv

    * HEAP_SIZE to HEAPSIZE

    * STACKCHECK to FPC_STACKCHECK

  Revision 1.1.2.5  2001/07/21 19:18:47  carl
  - removed unused variable

  Revision 1.1.2.4  2001/06/15 15:31:40  pierre
   * change to accept the mode objfpc at start of systemh.inc

  Revision 1.1.2.3  2001/05/17 01:37:41  carl
  * major updates to make it compile with FPC v1.0.4

  Revision 1.1.2.2  2001/04/22 00:36:04  carl
  - removed duplicate routines
  * corrected parameters to some routines

  Revision 1.1.2.1  2001/03/27 03:09:49  carl
  + Merged changes from Nils and Peter and Pierre
  + From Nils : Automatic closing of opened files now works correctly!
  Thanks nils!


  Revision 1.9  1998/08/17 12:34:22  carl
    * chdir accepts .. characters
    + added ctrl-c checking
    + implemented sbrk
    * exit code was never called if no error was found on exit!
    * register was not saved in do_open

  Revision 1.8  1998/07/13 12:32:18  carl
    * do_truncate works, some cleanup

  Revision 1.6  1998/07/02 12:37:52  carl
    * IOCheck for chdir,rmdir and mkdir as in TP

  Revision 1.5  1998/07/01 14:30:56  carl
    * forgot that includes are case sensitive

  Revision 1.4  1998/07/01 14:13:50  carl
    * do_open bugfix
    * correct conversion of Amiga error codes to TP error codes
    * InoutRes word bugfix
    * parameter counting fixed
    * new stack checking implemented
    + IOCheck for chdir,rmdir,getdir and rmdir
    * do_filepos was wrong
    + chdir correctly implemented
    * getdir correctly implemented

  Revision 1.1.1.1  1998/03/25 11:18:47  root
  * Restored version

  Revision 1.14  1998/03/21 04:20:09  carl
    * correct ExecBase pointer (from Nils Sjoholm)
    * correct OpenLibrary vector (from Nils Sjoholm)

  Revision 1.13  1998/03/14 21:34:32  carl
    * forgot to save a6 in Startup routine

  Revision 1.12  1998/02/24 21:19:42  carl
  *** empty log message ***

  Revision 1.11  1998/02/23 02:22:49  carl
    * bugfix if linking problems

  Revision 1.9  1998/02/06 16:34:32  carl
    + do_open is now standard with other platforms

  Revision 1.8  1998/02/02 15:01:45  carl
    * fixed bug with opening library versions (from Nils Sjoholm)

  Revision 1.7  1998/01/31 19:35:19  carl
    + added opening of utility.library

  Revision 1.6  1998/01/29 23:20:54  peter
    - Removed Backslash convert

  Revision 1.5  1998/01/27 10:55:04  peter
    * Amiga uses / not \, so change AllowSlash -> AllowBackSlash

  Revision 1.4  1998/01/25 21:53:20  peter
    + Universal Handles support for StdIn/StdOut/StdErr
    * Updated layout of sysamiga.pas

  Revision 1.3  1998/01/24 21:09:53  carl
    + added missing input/output function pointers

  Revision 1.2  1998/01/24 14:08:25  carl
    * RunError 217 --> RunError 219 (cannot open lib)
    + Standard Handle names implemented

  Revision 1.1  1998/01/24 05:12:15  carl
    + initial revision, some stuff still missing though.
      (and as you might imagine ... untested :))
}
