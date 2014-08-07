{
    $Id: heapbase.pas,v 1.1.2.1 2002/11/08 09:05:37 pierre Exp $
    Copyright (c) 1998-2000 by Pierre Muller

    Simple unit to add source line and column to each
    memory allocation made with heaptrc unit

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

 ****************************************************************************}
unit heapbase;

interface

uses
  heaptrc;

type
  pextra_info = ^textra_info;
  textra_info = record
    line,
    col,
    fileindex : longint;
  end;


implementation

  const
     heapbase_inited : boolean = false;

  procedure pp_heap_init;
    begin
       if not heapbase_inited then
         begin
            SetExtraInfo(sizeof(textra_info),nil);
         end;
       heapbase_inited:=true;
{$ifndef TRACE_ON_HALT}
       no_trace_if_halt := true;
{$endif TRACE_ON_HALT}
    end;


begin
  pp_heap_init;
end.
{
  $Log: heapbase.pas,v $
  Revision 1.1.2.1  2002/11/08 09:05:37  pierre
   * heapbase added to avoid previous memory allocation on linux

  Revision 1.1.2.4  2002/11/07 18:07:44  pierre
   * only dump on success

  Revision 1.1.2.3  2002/11/07 10:15:03  pierre
   * improove ppheap output

  Revision 1.1.2.2  2002/10/28 23:05:55  pierre
   * update to heaptrc changes

  Revision 1.1.2.1  2001/04/12 18:01:04  peter
    * use the new heaptrc from the mainbranch

  Revision 1.5  2001/03/13 18:43:17  peter
    * made memdebug and heaptrc compilable again

  Revision 1.4  2000/10/14 21:52:56  peter
    * fixed memory leaks

  Revision 1.3  2000/09/24 15:06:24  peter
    * use defines.inc

  Revision 1.2  2000/07/13 11:32:45  michael
  + removed logs

}
