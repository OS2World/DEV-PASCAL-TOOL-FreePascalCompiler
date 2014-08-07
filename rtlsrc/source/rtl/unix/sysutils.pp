{
    $Id: sysutils.pp,v 1.1.2.11 2002/10/29 21:50:52 michael Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    Sysutils unit for linux

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

uses
  linux,errors;

{ Include platform independent interface part }
{$i sysutilh.inc}

{ Platform dependent calls }

Procedure AddDisk(const path:string);



implementation

{ Include platform independent implementation part }
{$i sysutils.inc}


{****************************************************************************
                              File Functions
****************************************************************************}

Function FileOpen (Const FileName : string; Mode : Integer) : Longint;

Var LinuxFlags : longint;

BEGIN
  LinuxFlags:=0;
  Case (Mode and 3) of
    0 : LinuxFlags:=LinuxFlags or Open_RdOnly;
    1 : LinuxFlags:=LinuxFlags or Open_WrOnly;
    2 : LinuxFlags:=LinuxFlags or Open_RdWr;
  end;
  FileOpen:=fdOpen (FileName,LinuxFlags);
  //!! We need to set locking based on Mode !!
end;


Function FileCreate (Const FileName : String) : Longint;

begin
  FileCreate:=fdOpen(FileName,Open_RdWr or Open_Creat or Open_Trunc);
end;


Function FileRead (Handle : Longint; Var Buffer; Count : longint) : Longint;

begin
  FileRead:=fdRead (Handle,Buffer,Count);
end;


Function FileWrite (Handle : Longint; const Buffer; Count : Longint) : Longint;

begin
  FileWrite:=fdWrite (Handle,Buffer,Count);
end;


Function FileSeek (Handle,FOffset,Origin : Longint) : Longint;

begin
  FileSeek:=fdSeek (Handle,FOffset,Origin);
end;


Procedure FileClose (Handle : Longint);

begin
  fdclose(Handle);
end;

Function FileTruncate (Handle,Size: Longint) : boolean;

begin
  FileTruncate:=fdtruncate(Handle,Size);
end;

Function UnixToWinAge(UnixAge : Longint): Longint;

Var
  Y,M,D,hh,mm,ss : word;

begin
 EpochToLocal(UnixAge,y,m,d,hh,mm,ss);
 Result:=DateTimeToFileDate(EncodeDate(y,m,d)+EncodeTime(hh,mm,ss,0));
end;

Function FileAge (Const FileName : String): Longint;

Var Info : Stat;

begin
  If not fstat (FileName,Info) then
    exit(-1)
  else
    Result:=UnixToWinAge(Info.MTime);
end;


Function FileExists (Const FileName : String) : Boolean;

Var Info : Stat;

begin
  FileExists:=fstat(filename,Info);
end;


Function LinuxToWinAttr (FN : Pchar; Const Info : Stat) : Longint;

begin
  Result:=faArchive;
  If (Info.Mode and STAT_IFDIR)=STAT_IFDIR then
    Result:=Result or faDirectory ;
  If (FN[0]='.') and (not (FN[1] in [#0,'.']))  then
    Result:=Result or faHidden;
  If (Info.Mode and STAT_IWUSR)=0 Then
     Result:=Result or faReadOnly;
  If (Info.Mode and STAT_IFREG)=0 Then
//      (STAT_IFSOCK or STAT_IFBLK or STAT_IFCHR or STAT_IFIFO))<>0 then
     Result:=Result or faSysFile;
end;

{
 GlobToSearch takes a glob entry, stats the file.
 The glob entry is removed.
 If FileAttributes match, the entry is reused
}

Type
  TGlobSearchRec = Record
    Path       : String;
    GlobHandle : PGlob;
  end;
  PGlobSearchRec = ^TGlobSearchRec;

Function GlobToTSearchRec (Var Info : TSearchRec) : Boolean;

Var SInfo : Stat;
    p     : Pglob;
    GlobSearchRec : PGlobSearchrec;

begin
  GlobSearchRec:=PGlobSearchrec(Info.FindHandle);
  P:=GlobSearchRec^.GlobHandle;
  Result:=P<>Nil;
  If Result then
    begin
    GlobSearchRec^.GlobHandle:=P^.Next;
    Result:=Fstat(GlobSearchRec^.Path+StrPas(p^.name),SInfo);
    If Result then
      begin
      Info.Attr:=LinuxToWinAttr(p^.name,SInfo);
      Result:=(Info.ExcludeAttr and Info.Attr)=0;
      If Result Then
         With Info do
           begin
           Attr:=Info.Attr;
           If P^.Name<>Nil then
           Name:=strpas(p^.name);
           Time:=UnixToWinAge(Sinfo.mtime);
           Size:=Sinfo.Size;
           end;
      end;
    P^.Next:=Nil;
    GlobFree(P);
    end;
end;

Function DoFind(Var Rslt : TSearchRec) : Longint;

Var
  GlobSearchRec : PGlobSearchRec;

begin
  Result:=-1;
  GlobSearchRec:=PGlobSearchRec(Rslt.FindHandle);
  If (GlobSearchRec^.GlobHandle<>Nil) then
    While (GlobSearchRec^.GlobHandle<>Nil) and not (Result=0) do
      If GlobToTSearchRec(Rslt) Then Result:=0;
end;



Function FindFirst (Const Path : String; Attr : Longint; Var Rslt : TSearchRec) : Longint;

Const
  faSpecial = faHidden or faSysFile or faVolumeID or faDirectory;

Var
  GlobSearchRec : PGlobSearchRec;

begin
  New(GlobSearchRec);
  GlobSearchRec^.Path:=ExpandFileName(ExtractFilePath(Path));
  GlobSearchRec^.GlobHandle:=Glob(Path);
  Rslt.ExcludeAttr:=(Not Attr) and faSpecial;
  Rslt.FindHandle:=Longint(GlobSearchRec);
  Result:=DoFind (Rslt);
end;


Function FindNext (Var Rslt : TSearchRec) : Longint;

begin
  Result:=DoFind (Rslt);
end;


Procedure FindClose (Var F : TSearchrec);

Var
  GlobSearchRec : PGlobSearchRec;

begin
  GlobSearchRec:=PGlobSearchRec(F.FindHandle);
  GlobFree (GlobSearchRec^.GlobHandle);
  Dispose(GlobSearchRec);
end;


Function FileGetDate (Handle : Longint) : Longint;

Var Info : Stat;

begin
  If Not(FStat(Handle,Info)) then
    Result:=-1
  else
    Result:=Info.Mtime;
end;


Function FileSetDate (Handle,Age : Longint) : Longint;

begin
  // Impossible under Linux from FileHandle !!
  FileSetDate:=-1;
end;


Function FileGetAttr (Const FileName : String) : Longint;

Var Info : Stat;

begin
  If Not FStat (FileName,Info) then
    Result:=-1
  Else
    Result:=LinuxToWinAttr(Pchar(FileName),Info);
end;


Function FileSetAttr (Const Filename : String; Attr: longint) : Longint;

begin
  Result:=-1;
end;


Function DeleteFile (Const FileName : String) : Boolean;

begin
  Result:=UnLink (FileName);
end;


Function RenameFile (Const OldName, NewName : String) : Boolean;

begin
  RenameFile:=Linux.FRename(OldNAme,NewName);
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


{$ifdef INT64}

Function DiskFree(Drive: Byte): int64;
var
  fs : statfs;
Begin
  if ((Drive<4) and (not (fixdrivestr[Drive]=nil)) and fsstat(StrPas(fixdrivestr[drive]),fs)) or
     ((not (drivestr[Drive]=nil)) and fsstat(StrPas(drivestr[drive]),fs)) then
   Diskfree:=int64(fs.bavail)*int64(fs.bsize)
  else
   Diskfree:=-1;
End;



Function DiskSize(Drive: Byte): int64;
var
  fs : statfs;
Begin
  if ((Drive<4) and (not (fixdrivestr[Drive]=nil)) and fsstat(StrPas(fixdrivestr[drive]),fs)) or
     ((not (drivestr[Drive]=nil)) and fsstat(StrPas(drivestr[drive]),fs)) then
   DiskSize:=int64(fs.blocks)*int64(fs.bsize)
  else
   DiskSize:=-1;
End;

{$else}

Function DiskFree(Drive: Byte): Longint;
var
  fs : statfs;
Begin
  if ((Drive<4) and (not (fixdrivestr[Drive]=nil)) and fsstat(StrPas(fixdrivestr[drive]),fs)) or
     ((not (drivestr[Drive]=nil)) and fsstat(StrPas(drivestr[drive]),fs)) then
   Diskfree:=fs.bavail*fs.bsize
  else
   Diskfree:=-1;
End;


Function DiskSize(Drive: Byte): Longint;
var
  fs : statfs;
Begin
  if ((Drive<4) and (not (fixdrivestr[Drive]=nil)) and fsstat(StrPas(fixdrivestr[drive]),fs)) or
     ((not (drivestr[Drive]=nil)) and fsstat(StrPas(drivestr[drive]),fs)) then
   DiskSize:=fs.blocks*fs.bsize
  else
   DiskSize:=-1;
End;

{$endif INT64}



Function GetCurrentDir : String;
begin
  GetDir (0,Result);
end;


Function SetCurrentDir (Const NewDir : String) : Boolean;
begin
  {$I-}
   ChDir(NewDir);
  {$I+}
  result := (IOResult = 0);
end;


Function CreateDir (Const NewDir : String) : Boolean;
begin
  {$I-}
   MkDir(NewDir);
  {$I+}
  result := (IOResult = 0);
end;


Function RemoveDir (Const Dir : String) : Boolean;
begin
  {$I-}
   RmDir(Dir);
  {$I+}
  result := (IOResult = 0);
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
begin
  linux.GetTime(SystemTime.Hour, SystemTime.Minute, SystemTime.Second);
  linux.GetDate(SystemTime.Year, SystemTime.Month, SystemTime.Day);
  SystemTime.MilliSecond := 0;
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
  Result:=StrError(ErrorCode);
end;

{****************************************************************************
                              OS utility functions
****************************************************************************}

Function GetEnvironmentVariable(Const EnvVar : String) : String;

begin
  Result:=StrPas(Linux.Getenv(PChar(EnvVar)));
end;

Function DirectoryExists(const Directory: string): Boolean;

var
  Info : Stat;
begin
  Result:=FStat(Directory,Info);
  if Result then
    Result:=S_ISDIR(info.mode);
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
    Revision 1.1.2.11  2002/10/29 21:50:52  michael
    + Added AddTerminateProc

    Revision 1.1.2.10  2002/10/22 22:34:38  michael
    + Added missing directoryexists function

    Revision 1.1.2.9  2002/01/22 07:41:12  michael
    + Fixed FileSearch bug in Win32 and made FIleSearch platform independent

    Revision 1.1.2.8  2001/07/04 08:38:10  michael
    + Added adddisk function to interface

    Revision 1.1.2.7  2001/06/03 15:23:33  peter
      * outofmemory and invalidpointer exceptions fixed

    Revision 1.1.2.6  2001/02/22 22:17:41  michael
    + Kylix compatible FindFirst/FindNext

    Revision 1.1.2.5  2001/02/20 21:15:27  michael
    + Added GetEnvironmentVariable function

    Revision 1.1.2.4  2001/01/30 06:29:29  michael
    + Fixed time attribute in FindFIrst/Next TSearchrec

    Revision 1.1.2.3  2000/12/09 13:04:07  michael
    + Hopefully fixed findfirst/findnext forever

    Revision 1.1.2.2  2000/11/28 20:01:22  michael
    + Fixed findfirst/findnext/findclose

    Revision 1.1.2.1  2000/09/14 13:38:26  marco
     * Moved from Linux dir. now start of generic unix dir, from which the
        really exotic features should be moved to the target specific dirs.

    Revision 1.1.2.3  2000/08/22 19:21:48  michael
    + Implemented syserrormessage. Made dummies for go32v2 and OS/2
    * Changed linux/errors.pp so it uses pchars for storage.

    Revision 1.1.2.2  2000/08/20 15:22:57  peter
      * removed beep from interface

    Revision 1.1.2.1  2000/08/20 15:08:32  peter
      * forgot the add command :(

}