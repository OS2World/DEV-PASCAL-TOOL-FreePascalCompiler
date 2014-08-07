{
    $Id: sysutils.pp,v 1.1.2.5 2002/11/02 16:27:03 carl Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2001 by Florian Klaempfl
    member of the Free Pascal development team

    Sysutils unit for POSIX compliant systems

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit sysutils;
interface

{$MODE objfpc}
{ force ansistrings }
{$H+}



{ Include platform independent interface part }
{$i sysutilh.inc}

{ Platform dependent calls }

Procedure AddDisk(const path:string);



implementation

uses dos;

{ Include platform independent implementation part }
{$i sysutils.inc}



{****************************************************************************
                              File Functions
****************************************************************************}
{$I-}{ Required for correct usage of these routines }


Function FileOpen (Const FileName : string; Mode : Integer) : Longint;
Begin
end;


Function FileCreate (Const FileName : String) : Longint;

begin
end;


Function FileRead (Handle : Longint; Var Buffer; Count : longint) : Longint;

begin
end;


Function FileWrite (Handle : Longint; const Buffer; Count : Longint) : Longint;

begin
end;


Function FileSeek (Handle,FOffset,Origin : Longint) : Longint;
begin
end;


Procedure FileClose (Handle : Longint);

begin
end;

Function FileTruncate (Handle,Size: Longint) : boolean;
begin
end;


Function FileAge (Const FileName : String): Longint;

var F: file;
    Time: longint;
begin
end;


Function FileExists (Const FileName : String) : Boolean;


begin
end;







Function FindFirst (Const Path : String; Attr : Longint; Var Rslt : TSearchRec) : Longint;
begin
end;


Function FindNext (Var Rslt : TSearchRec) : Longint;
begin
end;


Procedure FindClose (Var F : TSearchrec);


begin
end;

Function FileGetDate (Handle : Longint) : Longint;

begin
end;


Function FileSetDate (Handle,Age : Longint) : Longint;

begin
  // Impossible under unix from FileHandle !!
  FileSetDate:=-1;
end;


Function FileGetAttr (Const FileName : String) : Longint;

begin
end;


Function FileSetAttr (Const Filename : String; Attr: longint) : Longint;

begin
end;


Function DeleteFile (Const FileName : String) : Boolean;
var
 F: File;
begin
 Assign(F,FileName);
 Erase(F);
 DeleteFile := (IOResult = 0);
end;

Function RenameFile (Const OldName, NewName : String) : Boolean;
var
 F: File;
begin
 Assign(F,OldName);
 Rename(F,NewName);
 RenameFile := (IOResult = 0);
end;


{****************************************************************************
                              Disk Functions
****************************************************************************}

{
  The Diskfree and Disksize functions need a file on the specified drive, since this
  is required for the statfs system call.
  These filenames are set in drivestr[0..26], and have been preset to :
   0 - '.'      (default drive - hence current dir is ok.)
   1 - '/fd0/.'  (floppy drive 1 - should be adapted to local system )
   2 - '/fd1/.'  (floppy drive 2 - should be adapted to local system )
   3 - '/'       (C: equivalent of dos is the root partition)
   4..26          (can be set by you're own applications)
  ! Use AddDisk() to Add new drives !
  They both return -1 when a failure occurs.
}
Const
  FixDriveStr : array[0..3] of pchar=(
    '.',
    '/fd0/.',
    '/fd1/.',
    '/.'
    );
var
  Drives   : byte;
  DriveStr : array[4..26] of pchar;

Procedure AddDisk(const path:string);
begin
  if not (DriveStr[Drives]=nil) then
   FreeMem(DriveStr[Drives],StrLen(DriveStr[Drives])+1);
  GetMem(DriveStr[Drives],length(Path)+1);
  StrPCopy(DriveStr[Drives],path);
  inc(Drives);
  if Drives>26 then
   Drives:=4;
end;



Function DiskFree(Drive: Byte): int64;
Begin
  DiskFree := dos.diskFree(Drive);
End;



Function DiskSize(Drive: Byte): int64;
Begin
  DiskSize := dos.DiskSize(Drive);
End;




Function GetCurrentDir : String;
begin
  GetDir (0,Result);
end;


Function SetCurrentDir (Const NewDir : String) : Boolean;
begin
   ChDir(NewDir);
  result := (IOResult = 0);
end;


Function CreateDir (Const NewDir : String) : Boolean;
begin
   MkDir(NewDir);
  result := (IOResult = 0);
end;


Function RemoveDir (Const Dir : String) : Boolean;
begin
   RmDir(Dir);
  result := (IOResult = 0);
end;


Function DirectoryExists(const Directory: string): Boolean;
begin
end;


{****************************************************************************
                              Misc Functions
****************************************************************************}

procedure Beep;
begin
end;


{****************************************************************************
                              Locale Functions
****************************************************************************}

Procedure GetLocalTime(var SystemTime: TSystemTime);
var
 dayOfWeek: word;
begin
  dos.GetTime(SystemTime.Hour, SystemTime.Minute, SystemTime.Second,SystemTime.Millisecond);
  dos.GetDate(SystemTime.Year, SystemTime.Month, SystemTime.Day, DayOfWeek);
end ;


Procedure InitAnsi;
Var
  i : longint;
begin
  {  Fill table entries 0 to 127  }
  for i := 0 to 96 do
    UpperCaseTable[i] := chr(i);
  for i := 97 to 122 do
    UpperCaseTable[i] := chr(i - 32);
  for i := 123 to 191 do
    UpperCaseTable[i] := chr(i);
  Move (CPISO88591UCT,UpperCaseTable[192],SizeOf(CPISO88591UCT));

  for i := 0 to 64 do
    LowerCaseTable[i] := chr(i);
  for i := 65 to 90 do
    LowerCaseTable[i] := chr(i + 32);
  for i := 91 to 191 do
    LowerCaseTable[i] := chr(i);
  Move (CPISO88591LCT,UpperCaseTable[192],SizeOf(CPISO88591UCT));
end;


Procedure InitInternational;
begin
  InitAnsi;
end;

function SysErrorMessage(ErrorCode: Integer): String;

begin
{  Result:=StrError(ErrorCode);}
end;

{****************************************************************************
                              OS utility functions
****************************************************************************}

Function GetEnvironmentVariable(Const EnvVar : String) : String;

begin
  Result:=Dos.Getenv(shortstring(EnvVar));
end;


{****************************************************************************
                              Initialization code
****************************************************************************}

Initialization
  CommonInitialize;
  InitInternational;    { Initialize internationalization settings }
Finalization
  CommonFinalize;
end.
{
    $Log: sysutils.pp,v $
    Revision 1.1.2.5  2002/11/02 16:27:03  carl
      * more routines implemented

    Revision 1.1.2.4  2002/10/29 21:50:52  michael
    + Added AddTerminateProc

    Revision 1.1.2.3  2002/10/29 20:58:35  carl
      * several updates for Amiga

    Revision 1.1.2.2  2002/10/20 16:11:38  carl
      * dos : fsplit / fexpand bugfixes
      * makefile updates for cycle
      * sysamiga : more memory blocks can be allocated (up 16 Mbytes of RAM)

    Revision 1.1.2.1  2002/10/05 22:41:24  carl
      * first revision

}