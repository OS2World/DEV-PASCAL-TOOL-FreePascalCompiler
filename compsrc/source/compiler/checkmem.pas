{
    $Id: checkmem.pas,v 1.1.2.3 2003/01/15 19:27:59 carl Exp $
    Copyright (c) 2002 by Pierre Muller

    This unit is just used to check that all memory is freed

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


Unit CheckMem;

Interface

Const
  PrintMemLost : boolean = true;

Implementation

Const
  EntryMemUsed : longint = 0;
  SaveExit : pointer = nil;



  Procedure CheckMemory(LostMemory : longint);
    var
      st : string;
  begin
    if LostMemory<>0 then
      begin
        str(LostMemory,st);
        Writeln('Memory Lost = ',st);
      end;
  end;

procedure CheckMemExit;

  begin
{$ifndef tp}
    if PrintMemLost then
      CheckMemory(system.HeapSize-MemAvail-EntryMemUsed);
{$endif}
    ExitProc:=SaveExit;
  end;


begin
{$ifndef tp}
  EntryMemUsed:=system.HeapSize-MemAvail;
{$endif}
  SaveExit:=ExitProc;
  ExitProc:=@CheckMemExit;
end.

{
  $Log: checkmem.pas,v $
  Revision 1.1.2.3  2003/01/15 19:27:59  carl
    * fix small compilation problems

  Revision 1.1.2.2  2003/01/12 15:51:45  peter
    * don't print mem lost when exiting from options

  Revision 1.1.2.1  2002/11/08 13:59:43  pierre
   + CheckMem unit added, to get better memory leak infos with -dEXTDEBUG


}
