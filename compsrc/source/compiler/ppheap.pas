{
    $Id: ppheap.pas,v 1.1.2.5 2002/11/08 09:05:37 pierre Exp $
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
unit ppheap;

interface

    uses heaptrc;

    { call this function before any memory allocation
      in a unit initialization code (PM) }

    procedure pp_heap_init;
    procedure ppheap_register_file(name : string;index : longint);


implementation

    uses
       heapbase,
       globtype,globals,files,cobjects;

const
   MaxFiles = 1024;
   MaxNameLength = 39;

type
   heapfileinfo = record
     name : string[MaxNameLength];
     index : longint;
   end;

   tfileinfoarray = array [1..MaxFiles] of heapfileinfo;

var
   fileinfoarray : tfileinfoarray;
   last_index : longint;


    procedure ppheap_register_file(name : string;index : longint);
      begin
        inc(last_index);
        if last_index <= MaxFiles then
          begin
            fileinfoarray[last_index].name:=copy(name,1,MaxNameLength);
            fileinfoarray[last_index].index:=index;
          end
        else
          writeln(stderr,'file',name,' has index ',index);
      end;

    procedure set_extra_info(p : pointer);
      begin
        with pextra_info(p)^ do
         begin
           line:=aktfilepos.line;
           col:=aktfilepos.column;
           if assigned(current_module) then
            fileindex:=current_module^.unit_index*100000+aktfilepos.fileindex
           else
            fileindex:=aktfilepos.fileindex;
         end;
      end;

    function getfilename(index : longint) : string;
      var
        i : longint;
      begin
        for i:=1 to last_index do
          begin
            if fileinfoarray[i].index=index then
              begin
                getfilename:=fileinfoarray[i].name;
                exit;
              end;
          end;
        getfilename:=tostr(index);
      end;

    function show_extra_info(p : pointer) : string;
      begin
        with pextra_info(p)^ do
         begin
           show_extra_info:=getfilename(fileindex)+'('+tostr(line)+','+tostr(col)+
             ') ';
         end;
      end;

  const
     pp_heap_inited : boolean = false;

  procedure pp_heap_init;
    begin
       if not pp_heap_inited then
         begin
            SetHeapTraceOutput('heap.log');
            SetExtraInfo(sizeof(textra_info),
                             {$ifdef FPC}@{$endif}set_extra_info);
            SetExtraInfoString({$ifdef FPC}@{$endif}show_extra_info);
         end;
       pp_heap_inited:=true;
    end;


begin
  pp_heap_init;
end.
{
  $Log: ppheap.pas,v $
  Revision 1.1.2.5  2002/11/08 09:05:37  pierre
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
