{
    $Id: dos.inc,v 1.1.2.2 2002/05/01 14:09:11 carl Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by members of the Free Pascal
    development team

    Operating system specific calls for DOS unit (part of POSIX interface)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$i solaris.inc}


{
  The Diskfree and Disksize functions need a file on the specified drive, since this
  is required for the statfs system call.
  These filenames are set in drivestr[0..26], and have been preset to :
   0 - '.'      (default drive - hence current dir is ok.)
   1 - '/floppy/.'  (either direct or contains dir with volume label of mounted device)
   2 - '/cdrom/.'   (either direct or contains dir with volume label of mounted device)
   3 - '/'       (C: equivalent of dos is the root partition)
   4..26          (can be set by you're own applications)
  ! Use AddDisk() to Add new drives !
  They both return -1 when a failure occurs.
  The drive names are OS specific
}
Const
  FixDriveStr : array[0..3] of pchar=(
    '.',            { the current directory    }
    '/floppy/.',    { manually mounted floppy  }
    '/cdrom/.',     { manually mounted cdrom   }
    '/'             { root partition           }
    );



Function DosVersion:Word;
Var
  Buffer : Array[0..255] of Char;
  Tmp2,
  TmpStr : String[40];
  TmpPos,
  SubRel,
  Rel    : LongInt;
  info   : ^utsname;
Begin
  new(info);
  sys_uname(info^);
  Move(info^.release,buffer[0],40);
  TmpStr:=StrPas(Buffer);
  SubRel:=0;
  TmpPos:=Pos('.',TmpStr);
  if TmpPos>0 then
   begin
     Tmp2:=Copy(TmpStr,TmpPos+1,40);
     Delete(TmpStr,TmpPos,40);
   end;
  TmpPos:=Pos('.',Tmp2);
  if TmpPos>0 then
   Delete(Tmp2,TmpPos,40);
  Val(TmpStr,Rel);
  Val(Tmp2,SubRel);
  DosVersion:=Rel+(SubRel shl 8);
  dispose(info);
End;

Function DiskFree(Drive: Byte): int64;
var
  info : statvfs_t;
Begin
  DiskFree := -1;
  if (Drive < 4) and (FixDriveStr[Drive]<>nil) then
    begin
     if sys_statvfs(FixDriveStr[Drive],info)=0 then
        DiskFree := int64(info.f_frsize)*int64(info.f_bavail);
    end
  else
  if (Drive>4) and (Drive<=MAX_DRIVES) and (drivestr[Drive]<>nil) then
    begin
     if sys_statvfs(DriveStr[Drive],info)=0 then
        DiskFree := int64(info.f_frsize)*int64(info.f_bavail);
    end
  else
    begin
      exit;
    end;
End;



Function DiskSize(Drive: Byte): int64;
var
  info : statvfs_t;
Begin
  DiskSize:= -1;
  if (Drive < 4) and (FixDriveStr[Drive]<>nil) then
    begin
     if sys_statvfs(FixDriveStr[Drive],info)=0 then
        DiskSize := int64(info.f_frsize)*int64(info.f_blocks);
    end
  else
  if (Drive>4) and (Drive<=MAX_DRIVES) and (drivestr[Drive]<>nil) then
    begin
     if sys_statvfs(DriveStr[Drive],info)=0 then
        DiskSize := int64(info.f_frsize)*int64(info.f_blocks);
    end
  else
    begin
      exit;
    end;
End;

function GetTimeZoneString : string;
begin
  GetTimeZoneString:=getenv('TZ');
end;


function GetTimezoneFile:string;
var
  f,len : longint;
  s : string;
  info : stat;
  buffer : array[0..PATH_MAX+1] of char;
begin
  GetTimezoneFile:='';
{ Solaris uses the TZ environment variable }  
end;


{
 $Log: dos.inc,v $
 Revision 1.1.2.2  2002/05/01 14:09:11  carl
 + TZ is now taken from GetTimezoneSitrng instead of getenv

 Revision 1.1.2.1  2001/12/09 03:25:36  carl
 + reinstated

 Revision 1.1.2.2  2001/12/04 02:47:33  carl
 + diskfree, disksize, GetTimeZoneFile and DosVersion implemented
   (untested)

 Revision 1.1.2.1  2001/12/04 00:46:25  carl
 + initial import

}
