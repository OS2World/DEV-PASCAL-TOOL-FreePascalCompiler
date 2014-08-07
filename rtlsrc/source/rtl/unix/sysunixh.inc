{
    $Id: sysunixh.inc,v 1.1.2.3 2001/11/07 15:09:46 michael Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This is the core of the system unit *nix systems (now FreeBSD
     and Unix).

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{$define newsignal}

{$I systemh.inc}
{$I heaph.inc}

{$ifdef m68k}
{ used for single computations }
const
  BIAS4 = $7f-1;
{$endif}

{Platform specific information}
const
 LineEnding = #10;
 LFNSupport = true;
 DirectorySeparator = '/';
 DriveSeparator = ':';
 PathSeparator = ':';
 FileNameCaseSensitive : Boolean = True;

const
  UnusedHandle    = -1;
  StdInputHandle  = 0;
  StdOutputHandle = 1;
  StdErrorHandle  = 2;

var
  argc : longint;
  argv : ppchar;
  envp : ppchar;


{
   $Log: sysunixh.inc,v $
   Revision 1.1.2.3  2001/11/07 15:09:46  michael
   + Added OS describing constants

   Revision 1.1.2.2  2001/06/17 22:54:44  pierre
    * move defintion of BIAS4 for m68k CPU to allow correct compilation

   Revision 1.1.2.1  2000/09/16 11:01:17  marco
    *  Split syslinux into sysunix.inc and sysunixh.inc


}
