{
    $Id: cpuinfo.pas,v 1.1.2.2 2002/08/18 11:33:22 carl Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    Basic Processor information

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
Unit CPUInfo;

Interface

Type
{$ifdef FPC}
   AWord = dword;
{$else FPC}
   AWord = Longint;
{$endif FPC}

(* Const
   { Size of native extended type }
   extended_size = 8; this is wrong
   correct value is in cpubase unit PM *)

Implementation

end.
{
   $Log: cpuinfo.pas,v $
   Revision 1.1.2.2  2002/08/18 11:33:22  carl
     * readd in fixes branch (the correct version)

   Revision 1.1.2.2  2001/07/25 07:02:25  pierre
    * remove wrong extended_size constant

   Revision 1.1.2.1  2001/05/09 20:29:45  peter
     * renamed to lowercase

   Revision 1.1.2.1  2001/04/17 03:25:15  carl
   + used to compile m68k compiler


}
