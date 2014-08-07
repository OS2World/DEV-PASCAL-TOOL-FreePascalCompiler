{
    $Id: fpredir.pas,v 1.2 2002/09/07 15:40:44 peter Exp $
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Unit to redirect output and error to files

    Adapted from code donated to public domain by Schwartz Gabriel 20/03/1993

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
Unit FPRedir;
Interface

{$R-}
{$ifndef Unix}
  {$S-}
{$endif}

{$ifdef TP}
{$define implemented}
{$endif TP}
{$ifdef Go32v2}
{$define implemented}
{$endif}
{$ifdef Win32}
{$define implemented}
{$endif}
{$ifdef Unix}
{$define implemented}
{$endif}

{ be sure msdos is not set for FPC compiler }
{$ifdef FPC}
{$UnDef MsDos}
{$endif FPC}

Var
  IOStatus                   : Integer;
  RedirErrorOut,RedirErrorIn,
  RedirErrorError            : Integer;
  ExecuteResult              : Word;

{------------------------------------------------------------------------------}
procedure InitRedir;
function ExecuteRedir (Const ProgName, ComLine, RedirStdIn, RedirStdOut, RedirStdErr : String) : boolean;
procedure DosExecute(ProgName, ComLine : String);

function  ChangeRedirOut(Const Redir : String; AppendToFile : Boolean) : Boolean;
procedure RestoreRedirOut;
procedure DisableRedirOut;
procedure EnableRedirOut;
function  ChangeRedirIn(Const Redir : String) : Boolean;
procedure RestoreRedirIn;
procedure DisableRedirIn;
procedure EnableRedirIn;
function  ChangeRedirError(Const Redir : String; AppendToFile : Boolean) : Boolean;
procedure RestoreRedirError;
procedure DisableRedirError;
procedure EnableRedirError;
procedure RedirDisableAll;
procedure RedirEnableAll;
{$ifdef win32}
procedure Win32ShowMouse;
{$endif win32}

Implementation

Uses
{$ifdef go32v2}
  go32,
{$endif go32v2}
{$ifdef win32}
  windows,
{$endif win32}
{$ifdef Unix}
  {$ifdef VER1_0}
    linux,
  {$else}
    unix,
  {$endif}
{$endif Unix}
  dos;

var
  FIN,FOUT,FERR     : ^File;
  RedirChangedOut,
  RedirChangedIn    : Boolean;
  RedirChangedError : Boolean;
  InRedirDisabled,OutRedirDisabled,ErrorRedirDisabled : Boolean;

{*****************************************************************************
                                     Dos
*****************************************************************************}

{$ifdef implemented}

{$ifdef TP}

{$ifndef win32}
const
  UnusedHandle    = -1;
  StdInputHandle  = 0;
  StdOutputHandle = 1;
  StdErrorHandle  = 2;
{$endif win32}

Type
  PtrRec = packed record
             Ofs, Seg : Word;
           end;

  PHandles = ^THandles;
  THandles = Array [Byte] of Byte;

  PWord = ^Word;

Var
  MinBlockSize : Word;
  MyBlockSize  : Word;
  Handles      : PHandles;
  PrefSeg      : Word;
  OldHandleOut,OldHandleIn,OldHandleError    : Byte;
{$endif TP}

var
  TempHOut, TempHIn,TempHError : longint;

{ For Unix the following functions exist
Function  Dup(oldfile:longint;var newfile:longint):Boolean;
Function  Dup2(oldfile,newfile:longint):Boolean;
Function  fdClose(fd:longint):boolean;
}
{$ifdef go32v2}

function dup(fh : longint;var nh : longint) : boolean;
  var
    Regs : Registers;

begin
    Regs.ah:=$45;
    Regs.bx:=fh;
    MsDos (Regs);
    Dup:=true;
    If (Regs.Flags and fCarry)=0 then
      nh:=Regs.Ax
    else
      Dup:=false;
end;

function dup2(fh,nh : longint) : boolean;
  var
    Regs : Registers;

begin
    Dup2:=true;
    If fh=nh then
      exit;
    Regs.ah:=$46;
    Regs.bx:=fh;
    Regs.cx:=nh;
    MsDos (Regs);
    If (Regs.Flags and fCarry)<>0 then
      Dup2:=false;
end;

Function FdClose (Handle : Longint) : boolean;
var Regs: registers;
begin
  Regs.Eax := $3e00;
  Regs.Ebx := Handle;
  MsDos(Regs);
  FdClose:=(Regs.Flags and fCarry)=0;
end;

{$endif def go32v2}

{$ifdef win32}
procedure Win32ShowMouse;
begin
  ExecuteRedir(GetEnv('COMSPEC'),'/C rem echo This dummy call gets the mouse to become visible'
    ,'','nul','');
end;

Function FdClose (Handle : Longint) : boolean;
begin
  { Do we need this ?? }
  FdClose:=true;
end;
{$endif}

{$ifdef TP}
Function FdClose (Handle : Longint) : boolean;
begin
  { if executed as under GO32 this hangs the DOS-prompt }
  FdClose:=true;
end;

{$endif}

{$I-}
function FileExist(const FileName : PathStr) : Boolean;
var
  f : file;
  Attr : word;
begin
  Assign(f, FileName);
  GetFAttr(f, Attr);
  FileExist := DosError = 0;
end;


{............................................................................}

function ChangeRedirOut(Const Redir : String; AppendToFile : Boolean) : Boolean;
  begin
    ChangeRedirOut:=False;
    If Redir = '' then Exit;
    Assign (FOUT^, Redir);
    If AppendToFile and FileExist(Redir) then
      Begin
      Reset(FOUT^,1);
      Seek(FOUT^,FileSize(FOUT^));
      End else Rewrite (FOUT^);

    RedirErrorOut:=IOResult;
    IOStatus:=RedirErrorOut;
    If IOStatus <> 0 then Exit;
{$ifndef FPC}
    Handles:=Ptr (prefseg, PWord (Ptr (prefseg, $34))^);
    OldHandleOut:=Handles^[StdOutputHandle];
    Handles^[StdOutputHandle]:=Handles^[FileRec (FOUT^).Handle];
    ChangeRedirOut:=True;
    OutRedirDisabled:=False;
{$else}
{$ifdef win32}
    if SetStdHandle(Std_Output_Handle,FileRec(FOUT^).Handle) then
{$else not win32}
    if dup(StdOutputHandle,TempHOut) and
       dup2(FileRec(FOUT^).Handle,StdOutputHandle) then
{$endif not win32}
      begin
         ChangeRedirOut:=True;
         OutRedirDisabled:=False;
      end;
{$endif def FPC}
     RedirChangedOut:=True;
  end;

function ChangeRedirIn(Const Redir : String) : Boolean;
  begin
    ChangeRedirIn:=False;
    If Redir = '' then Exit;
    Assign (FIN^, Redir);
    Reset(FIN^,1);

    RedirErrorIn:=IOResult;
    IOStatus:=RedirErrorIn;
    If IOStatus <> 0 then Exit;
{$ifndef FPC}
    Handles:=Ptr (prefseg, PWord (Ptr (prefseg, $34))^);
    OldHandleIn:=Handles^[StdInputHandle];
    Handles^[StdInputHandle]:=Handles^[FileRec (FIN^).Handle];
    ChangeRedirIn:=True;
    InRedirDisabled:=False;
{$else}
{$ifdef win32}
    if SetStdHandle(Std_Input_Handle,FileRec(FIN^).Handle) then
{$else not win32}
    if dup(StdInputHandle,TempHIn) and
       dup2(FileRec(FIN^).Handle,StdInputHandle) then
{$endif not win32}
      begin
         ChangeRedirIn:=True;
         InRedirDisabled:=False;
      end;
{$endif def FPC}
     RedirChangedIn:=True;
  end;

function ChangeRedirError(Const Redir : String; AppendToFile : Boolean) : Boolean;
  begin
    ChangeRedirError:=False;
    If Redir = '' then Exit;
    Assign (FERR^, Redir);
    If AppendToFile and FileExist(Redir) then
      Begin
      Reset(FERR^,1);
      Seek(FERR^,FileSize(FERR^));
      End else Rewrite (FERR^);

    RedirErrorError:=IOResult;
    IOStatus:=RedirErrorError;
    If IOStatus <> 0 then Exit;
{$ifndef FPC}
    Handles:=Ptr (prefseg, PWord (Ptr (prefseg, $34))^);
    OldHandleError:=Handles^[StdErrorHandle];
    Handles^[StdErrorHandle]:=Handles^[FileRec (FERR^).Handle];
    ChangeRedirError:=True;
    ErrorRedirDisabled:=False;
{$else}
{$ifdef win32}
    if SetStdHandle(Std_Error_Handle,FileRec(FERR^).Handle) then
{$else not win32}
    if dup(StdErrorHandle,TempHError) and
       dup2(FileRec(FERR^).Handle,StdErrorHandle) then
{$endif not win32}
      begin
         ChangeRedirError:=True;
         ErrorRedirDisabled:=False;
      end;
{$endif}
     RedirChangedError:=True;
  end;


{$IfDef MsDos}
{Set HeapEnd Pointer to Current Used Heapsize}
Procedure SmallHeap;assembler;
asm
                mov     bx,word ptr HeapPtr
                shr     bx,4
                inc     bx
                add     bx,word ptr HeapPtr+2
                mov     ax,PrefixSeg
                sub     bx,ax
                mov     es,ax
                mov     ah,4ah
                int     21h
end;



{Set HeapEnd Pointer to Full Heapsize}
Procedure FullHeap;assembler;
asm
                mov     bx,word ptr HeapEnd
                shr     bx,4
                inc     bx
                add     bx,word ptr HeapEnd+2
                mov     ax,PrefixSeg
                sub     bx,ax
                mov     es,ax
                mov     ah,4ah
                int     21h
end;

{$EndIf MsDos}


  procedure RestoreRedirOut;

  begin
    If not RedirChangedOut then Exit;
{$ifndef FPC}
    Handles^[StdOutputHandle]:=OldHandleOut;
    OldHandleOut:=StdOutputHandle;
{$else}
{$ifdef win32}
    SetStdHandle(Std_Output_Handle,StdOutputHandle);
{$else not win32}
    dup2(TempHOut,StdOutputHandle);
{$endif not win32}
{$endif FPC}
    Close (FOUT^);
    fdClose(TempHOut);
    RedirChangedOut:=false;
  end;

  {............................................................................}

  procedure RestoreRedirIn;

  begin
    If not RedirChangedIn then Exit;
{$ifndef FPC}
    Handles^[StdInputHandle]:=OldHandleIn;
    OldHandleIn:=StdInputHandle;
{$else}
{$ifdef win32}
    SetStdHandle(Std_Input_Handle,StdInputHandle);
{$else not win32}
    dup2(TempHIn,StdInputHandle);
{$endif not win32}
{$endif}
    Close (FIn^);
    fdClose(TempHIn);
    RedirChangedIn:=false;
  end;

  {............................................................................}

  procedure DisableRedirIn;

  begin
    If not RedirChangedIn then Exit;
    If InRedirDisabled then Exit;
{$ifndef FPC}
    Handles^[StdInputHandle]:=OldHandleIn;
{$else}
{$ifdef win32}
    SetStdHandle(Std_Input_Handle,StdInputHandle);
{$else not win32}
    dup2(TempHIn,StdInputHandle);
{$endif not win32}
{$endif}
    InRedirDisabled:=True;
  end;

  {............................................................................}

  procedure EnableRedirIn;

  begin
    If not RedirChangedIn then Exit;
    If not InRedirDisabled then Exit;
{$ifndef FPC}
    Handles:=Ptr (prefseg, PWord (Ptr (prefseg, $34))^);
    Handles^[StdInputHandle]:=Handles^[FileRec (FIn^).Handle];
{$else}
{$ifdef win32}
    SetStdHandle(Std_Input_Handle,FileRec(FIn^).Handle);
{$else not win32}
    dup2(FileRec(FIn^).Handle,StdInputHandle);
{$endif not win32}
{$endif}
    InRedirDisabled:=False;
  end;

  {............................................................................}

  procedure DisableRedirOut;

  begin
    If not RedirChangedOut then Exit;
    If OutRedirDisabled then Exit;
{$ifndef FPC}
    Handles^[StdOutputHandle]:=OldHandleOut;
{$else}
{$ifdef win32}
    SetStdHandle(Std_Output_Handle,StdOutputHandle);
{$else not win32}
    dup2(TempHOut,StdOutputHandle);
{$endif not win32}
{$endif}
    OutRedirDisabled:=True;
  end;

  {............................................................................}

  procedure EnableRedirOut;

  begin
    If not RedirChangedOut then Exit;
    If not OutRedirDisabled then Exit;
{$ifndef FPC}
    Handles:=Ptr (prefseg, PWord (Ptr (prefseg, $34))^);
    Handles^[StdOutputHandle]:=Handles^[FileRec (FOut^).Handle];
{$else}
{$ifdef win32}
    SetStdHandle(Std_Output_Handle,FileRec(FOut^).Handle);
{$else not win32}
    dup2(FileRec(FOut^).Handle,StdOutputHandle);
{$endif not win32}
{$endif}
    OutRedirDisabled:=False;
  end;

  {............................................................................}

  procedure RestoreRedirError;

  begin
    If not RedirChangedError then Exit;
{$ifndef FPC}
    Handles^[StdErrorHandle]:=OldHandleError;
    OldHandleError:=StdErrorHandle;
{$else}
{$ifdef win32}
    SetStdHandle(Std_Error_Handle,StdErrorHandle);
{$else not win32}
    dup2(TempHError,StdErrorHandle);
{$endif not win32}
{$endif}
    Close (FERR^);
    fdClose(TempHError);
    RedirChangedError:=false;
  end;

  {............................................................................}

  procedure DisableRedirError;

  begin
    If not RedirChangedError then Exit;
    If ErrorRedirDisabled then Exit;
{$ifndef FPC}
    Handles^[StdErrorHandle]:=OldHandleError;
{$else}
{$ifdef win32}
    SetStdHandle(Std_Error_Handle,StdErrorHandle);
{$else not win32}
    dup2(TempHError,StdErrorHandle);
{$endif not win32}
{$endif}
    ErrorRedirDisabled:=True;
  end;

  {............................................................................}

  procedure EnableRedirError;

  begin
    If not RedirChangedError then Exit;
    If not ErrorRedirDisabled then Exit;
{$ifndef FPC}
    Handles:=Ptr (prefseg, PWord (Ptr (prefseg, $34))^);
    Handles^[StdErrorHandle]:=Handles^[FileRec (FErr^).Handle];
{$else}
{$ifdef win32}
    SetStdHandle(Std_Error_Handle,FileRec(FErr^).Handle);
{$else not win32}
    dup2(FileRec(FERR^).Handle,StdErrorHandle);
{$endif not win32}
{$endif}
    ErrorRedirDisabled:=False;
  end;

{............................................................................}

function ExecuteRedir (Const ProgName, ComLine, RedirStdIn, RedirStdOut, RedirStdErr : String) : boolean;
Begin
  RedirErrorOut:=0; RedirErrorIn:=0; RedirErrorError:=0;
  ExecuteResult:=0;
  IOStatus:=0;
  if RedirStdIn<>'' then
    ChangeRedirIn(RedirStdIn);
  if RedirStdOut<>'' then
    ChangeRedirOut(RedirStdOut,false);
  if RedirStdErr<>'stderr' then
    ChangeRedirError(RedirStdErr,false);
  DosExecute(ProgName,ComLine);
  RestoreRedirOut;
  RestoreRedirIn;
  RestoreRedirError;
  ExecuteRedir:=(IOStatus=0) and (RedirErrorOut=0) and
                (RedirErrorIn=0) and (RedirErrorError=0) and
                (ExecuteResult=0);
End;

{............................................................................}

procedure RedirDisableAll;
  begin
    If RedirChangedIn and not InRedirDisabled then
      DisableRedirIn;
    If RedirChangedOut and not OutRedirDisabled then
      DisableRedirOut;
    If RedirChangedError and not ErrorRedirDisabled then
      DisableRedirError;
  end;

{............................................................................}

procedure RedirEnableAll;
  begin
    If RedirChangedIn and InRedirDisabled then
      EnableRedirIn;
    If RedirChangedOut and OutRedirDisabled then
      EnableRedirOut;
    If RedirChangedError and ErrorRedirDisabled then
      EnableRedirError;
  end;


procedure InitRedir;
begin
{$ifndef FPC}
  PrefSeg:=PrefixSeg;
{$endif FPC}
end;

{$else not  implemented}


{*****************************************************************************
                                 Fake
*****************************************************************************}

function ExecuteRedir (Const ProgName, ComLine, RedirStdIn, RedirStdOut, RedirStdErr : String) : boolean;
begin
  ExecuteRedir:=false;
end;

function  ChangeRedirOut(Const Redir : String; AppendToFile : Boolean) : Boolean;
begin
  ChangeRedirOut:=false;
end;


procedure RestoreRedirOut;
begin
end;


procedure DisableRedirOut;
begin
end;


procedure EnableRedirOut;
begin
end;


function  ChangeRedirIn(Const Redir : String) : Boolean;
begin
  ChangeRedirIn:=false;
end;


procedure RestoreRedirIn;
begin
end;


procedure DisableRedirIn;
begin
end;


procedure EnableRedirIn;
begin
end;


function  ChangeRedirError(Const Redir : String; AppendToFile : Boolean) : Boolean;
begin
  ChangeRedirError:=false;
end;


procedure RestoreRedirError;
begin
end;


procedure DisableRedirError;
begin
end;


procedure EnableRedirError;
begin
end;


procedure RedirDisableAll;
begin
end;


procedure RedirEnableAll;
begin
end;


procedure InitRedir;
begin
end;
{$endif not implemented}


{............................................................................}

  procedure DosExecute(ProgName, ComLine : String);
{$ifdef win32}
    var
      StoreInherit : BOOL;
{$endif win32}

  Begin
{$IfDef MsDos}
  SmallHeap;
{$EndIf MsDos}
    SwapVectors;
    { Must use shell() for linux for the wildcard expansion (PFV) }
{$ifdef Unix}
    IOStatus:=0;
    ExecuteResult:=Shell(Progname+' '+Comline);
    { Signal that causes the stop of the shell }
    IOStatus:=ExecuteResult and $7F;
    { Exit Code seems to be in the second byte,
      is this also true for BSD ??
      $80 bit is a CoreFlag apparently }
    ExecuteResult:=(ExecuteResult and $ff00) shr 8;
{$else}
{$ifdef win32}
    StoreInherit:=ExecInheritsHandles;
    ExecInheritsHandles:=true;
{$endif win32}
    DosError:=0;
    Dos.Exec (ProgName, ComLine);
{$ifdef win32}
    ExecInheritsHandles:=StoreInherit;
{$endif win32}
    IOStatus:=DosError;
    ExecuteResult:=DosExitCode;
{$endif}
    SwapVectors;
{$IfDef MsDos}
  Fullheap;
{$EndIf MsDos}
  End;

{*****************************************************************************
                                  Initialize
*****************************************************************************}

var oldexit : pointer;

procedure RedirExit; {$ifndef FPC}far;{$endif}
begin
  exitproc:=oldexit;
  Dispose(FIn); Dispose(FOut); Dispose(FErr);
end;

Begin
  oldexit:=exitproc;
  exitproc:=@RedirExit;
  New(FIn); New(FOut); New(FErr);
End.
{
  $Log: fpredir.pas,v $
  Revision 1.2  2002/09/07 15:40:44  peter
    * old logs removed and tabs fixed

}
