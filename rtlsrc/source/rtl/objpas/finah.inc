{
    *********************************************************************
    $Id: finah.inc,v 1.1.2.3 2002/10/22 21:56:07 michael Exp $
    Copyright (C) 1997, 1998 Gertjan Schouten

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
    *********************************************************************

    System Utilities For Free Pascal
}

Const
  DirSeparators : set of char = ['/','\'];  

function ChangeFileExt(const FileName, Extension: string): string;
function ExtractFilePath(const FileName: string): string;
function ExtractFileDrive(const FileName: string): string;
function ExtractFileName(const FileName: string): string;
function ExtractFileExt(const FileName: string): string; 
function ExtractFileDir(Const FileName : string): string;
function ExpandFileName (Const FileName : string): String;
function ExpandUNCFileName (Const FileName : string): String;
function ExtractRelativepath (Const BaseName,DestNAme : String): String;
function IncludeTrailingPathDelimiter(Const Path : String) : String;
function IncludeTrailingBackslash(Const Path : String) : String;
function ExcludeTrailingBackslash(Const Path: string): string;
function ExcludeTrailingPathDelimiter(Const Path: string): string;
function IsPathDelimiter(Const Path: string; Index: Integer): Boolean;
Procedure DoDirSeparators (Var FileName : String);
Function SetDirSeparators (Const FileName : String) : String;
Function GetDirs (Var DirName : String; Var Dirs : Array of pchar) : Longint; 

{
  $Log: finah.inc,v $
  Revision 1.1.2.3  2002/10/22 21:56:07  michael
  + Added some missing path functions

  Revision 1.1.2.2  2001/11/08 12:49:47  michael
  + Replaced OSDirSeparator with PathDelim

  Revision 1.1.2.1  2001/08/14 20:05:29  carl
  -* replace ifdef linux -> ifdef unix

  Revision 1.1  2000/07/13 06:31:00  michael
  + Initial import

  Revision 1.4  2000/02/09 16:59:32  peter
    * truncated log

}

