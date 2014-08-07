{
    $Id: version.pas,v 1.1.2.16 2003/06/26 19:33:20 michael Exp $
    Copyright (C) 1998-2000 by Florian Klaempfl

    Version/target constants

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

 ****************************************************************************
}
unit version;
interface

    const
       { word version for ppu file }
       wordversion = (1 shl 14)+6;

       { version string }

       version_nr = '1';
       release_nr = '0';
       patch_nr   = '10';
{$ifdef newoptimizations}
       minorpatch = 'OPT';
{$else}
       minorpatch = '';
{$endif}

       { date string }
{$ifdef FPC}
       date_string = {$I %DATE%};
{$else}
       date_string = 'N/A';
{$endif}

       { target cpu string }
{$ifdef i386}
       target_cpu_string = 'i386';
{$endif}
{$ifdef m68k}
       target_cpu_string = 'm68k';
{$endif}
{$ifdef alpha}
       target_cpu_string = 'alpha';
{$endif}
{$ifdef powerpc}
       target_cpu_string = 'powerpc';
{$endif}

       { source cpu string }
{$ifdef cpu86}
        source_cpu_string = 'i386';
{$endif}
{$ifdef cpu68}
        source_cpu_string = 'm68k';
{$endif}

function version_string:string;
function full_version_string:string;


implementation

function version_string:string;
begin
  if patch_nr='0' then
   version_string := version_nr+'.'+release_nr
  else
   version_string := version_nr+'.'+release_nr+'.'+patch_nr;
end;


function full_version_string:string;
begin
  if patch_nr='0' then
   full_version_string := version_nr+'.'+release_nr+minorpatch
  else
   full_version_string := version_nr+'.'+release_nr+'.'+patch_nr+minorpatch;
end;

end.
{
  $Log: version.pas,v $
  Revision 1.1.2.16  2003/06/26 19:33:20  michael
  Version 1.0.10 release

  Revision 1.1.2.15  2003/05/29 16:58:47  peter
    * 1.0.8 release

  Revision 1.1.2.14  2003/04/06 18:12:48  carl
    * Updated to correct -pre1 version

  Revision 1.1.2.13  2003/04/06 15:26:29  carl
    * Update versions to 1.0.8

  Revision 1.1.2.12  2002/05/04 10:31:31  carl
  - revert back to version 1.0.7 for snapshots

  Revision 1.1.2.11  2002/05/04 10:29:27  carl
  * QNX is version 1.0.6a

  Revision 1.1.2.10  2002/05/04 09:28:41  carl
  * QNX is version 1.0.6a

  Revision 1.1.2.9  2002/05/01 09:45:27  michael
  set version number to 1.0.7 for snapshots

  Revision 1.1.2.8  2002/04/23 13:21:18  peter
    * 1.0.6

  Revision 1.1.2.7  2002/03/04 18:01:09  peter
    * add -beta to version number display in logo and info

  Revision 1.1.2.6  2002/02/28 21:30:41  peter
    * regenated

  Revision 1.1.2.5  2000/12/18 21:45:24  michael
  + Version update to 1.0.5

  Revision 1.1.2.4  2000/12/18 09:58:23  florian
    * version 1.0.4

  Revision 1.1.2.3  2000/10/13 12:02:16  michael
  + Changed version no to 1.0.3

  Revision 1.1.2.2  2000/10/10 15:07:20  peter
    * 1.0.2

  Revision 1.1.2.1  2000/07/13 11:36:11  michael
  + Changed version to 1.0.1

  Revision 1.1  2000/07/13 06:30:03  michael
  + Initial import

  Revision 1.20  2000/07/10 09:17:27  pierre
   * fix wordversion

  Revision 1.19  2000/07/09 09:34:50  peter
    * version_string is now a function so it returns 1.00 instead of 1.00.0

  Revision 1.18  2000/07/06 20:08:46  peter
    * version 1.00.0 so the snapshots can test with this version number
      for a few days

  Revision 1.17  2000/03/21 21:35:27  peter
    * add OPT for optimizing compiler

  Revision 1.16  2000/02/09 13:23:09  peter
    * log truncated

  Revision 1.15  2000/01/28 20:47:26  michael
  + Changed patch number to 15

  Revision 1.14  2000/01/14 13:05:54  peter
    * version 0.99.14

  Revision 1.13  2000/01/07 01:14:49  peter
    * updated copyright to 2000

  Revision 1.12  1999/08/04 13:03:18  jonas
    * all tokens now start with an underscore
    * PowerPC compiles!!

  Revision 1.11  1999/08/02 17:17:12  florian
    * small changes for the new code generator

  Revision 1.10  1999/08/01 23:36:42  florian
    * some changes to compile the new code generator

}
