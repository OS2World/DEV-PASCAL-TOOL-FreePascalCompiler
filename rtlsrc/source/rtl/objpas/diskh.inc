{
    $Id: diskh.inc,v 1.1.2.1 2002/10/22 22:34:38 michael Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Disk functions from Delphi's sysutils.pas

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifdef Int64}
 Function  DiskFree(drive: byte) : int64;
 Function  DiskSize(drive: byte) : int64;
{$else}
 Function  DiskFree(drive: byte) : longint;
 Function  DiskSize(drive: byte) : longint;
{$endif}
Function GetCurrentDir : String;
Function SetCurrentDir (Const NewDir : String) : Boolean;
Function CreateDir (Const NewDir : String) : Boolean;
Function RemoveDir (Const Dir : String) : Boolean;
function DirectoryExists(const Directory: string): Boolean;

{
 $Log: diskh.inc,v $
 Revision 1.1.2.1  2002/10/22 22:34:38  michael
 + Added missing directoryexists function

 Revision 1.1  2000/07/13 06:30:59  michael
 + Initial import

 Revision 1.5  2000/05/15 19:28:41  peter
   * int64 support for diskfree,disksize

 Revision 1.4  2000/02/09 16:59:32  peter
   * truncated log

 Revision 1.3  2000/01/07 16:41:43  daniel
   * copyright 2000

 Revision 1.2  1999/11/06 14:41:30  peter
   * truncated log

}
