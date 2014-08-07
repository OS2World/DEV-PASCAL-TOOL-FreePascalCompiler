{
    $Id: ppufiles.pp,v 1.1.2.3 2001/08/17 16:16:52 florian Exp $
    Copyright (c) 1999-2000 by Peter Vreman

    List files needed by PPU

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
Program ppufiles;

uses
  dos,
  ppu;

const
  Version   = 'Version 1.00';
  Title     = 'PPU-Files';
  Copyright = 'Copyright (c) 1999-2000 by the Free Pascal Development Team';

  PPUExt = 'ppu';

type
  poutfile = ^toutfile;
  toutfile = record
    name : string;
    next : poutfile;
  end;

var
  skipdup,
  showstatic,
  showshared,
  showobjects : boolean;

  OutFiles    : poutfile;


{*****************************************************************************
                                 Helpers
*****************************************************************************}

Procedure Error(const s:string;stop:boolean);
{
  Write an error message to stderr
}
begin
{$ifdef FPC}
  writeln(stderr,s);
{$else}
  writeln(s);
{$endif}
  if stop then
   halt(1);
end;


Function AddExtension(Const HStr,ext:String):String;
{
  Return a filename which will have extension ext added if no
  extension is found
}
var
  j : longint;
begin
  j:=length(Hstr);
  while (j>0) and (Hstr[j]<>'.') do
   dec(j);
  if j=0 then
   AddExtension:=Hstr+'.'+Ext
  else
   AddExtension:=HStr;
end;


Function SplitPath(Const HStr:String):String;
var
  i : longint;
begin
  i:=Length(Hstr);
  while (i>0) and not(Hstr[i] in ['\','/']) do
   dec(i);
  SplitPath:=Copy(Hstr,1,i);
end;


Procedure AddFile(const s:string);
var
  p : poutfile;
begin
  p:=nil;
  if skipdup then
   begin
     p:=outfiles;
     while assigned(p) do
      begin
        if s=p^.name then
         break;
        p:=p^.next;
      end;
   end;
  if not assigned(p) then
   begin
     new(p);
     p^.name:=s;
     p^.next:=outfiles;
     outfiles:=p;
   end;
end;


Function DoPPU(const PPUFn:String):Boolean;
{
  Convert one file (in Filename) to library format.
  Return true if successful, false otherwise.
}
Var
  inppu  : pppufile;
  b      : byte;

  procedure showfiles;
  begin
    while not inppu^.endofentry do
     begin
       AddFile(inppu^.getstring);
       inppu^.getlongint;
     end;
  end;

begin
  DoPPU:=false;
  inppu:=new(pppufile,init(PPUFn));
  if not inppu^.openfile then
   begin
     dispose(inppu,done);
     Error('Error: Could not open : '+PPUFn,false);
     Exit;
   end;
{ Check the ppufile }
  if not inppu^.CheckPPUId then
   begin
     dispose(inppu,done);
     Error('Error: Not a PPU File : '+PPUFn,false);
     Exit;
   end;
  if inppu^.GetPPUVersion<CurrentPPUVersion then
   begin
     dispose(inppu,done);
     Error('Error: Wrong PPU Version : '+PPUFn,false);
     Exit;
   end;
{ read until the object files are found }
  repeat
    b:=inppu^.readentry;
    case b of
      ibendinterface,
      ibend :
        break;
      iblinkunitstaticlibs :
        if showstatic then
         showfiles;
      iblinkunitsharedlibs :
        if showshared then
         showfiles;
      iblinkunitofiles :
        if showobjects then
         showfiles;
    end;
  until false;
  dispose(inppu,done);
  DoPPU:=True;
end;



var
  i,parafile : longint;
  dir        : SearchRec;
  s,InFile   : String;
  p          : poutfile;
begin
{ defaults }
  skipdup:=true;
{ options }
  i:=1;
  while (i<=paramcount) do
   begin
     s:=paramstr(i);
     if s[1]<>'-' then
      break;
     case upcase(s[2]) of
      'L' : showshared:=true;
      'S' : showstatic:=true;
      'O' : showobjects:=true;
      'A' : skipdup:=false;
      '?','H' :
        begin
          writeln('usage: ppufiles [options] <files>');
          writeln('options:');
          writeln('  -A  Show all files (don''t remove duplicates)');
          writeln('  -L  Show only shared libraries');
          writeln('  -S  Show only static libraries');
          writeln('  -O  Show only object files');
          writeln('  -H  This helpscreen');
        end;
     end;
     inc(i);
   end;
  { default shows everything }
  if i=1 then
   begin
     showshared:=true;
     showstatic:=true;
     showobjects:=true;
   end;
{ files }
  parafile:=i;
  for i:=parafile to ParamCount do
   begin
     InFile:=AddExtension(ParamStr(i),PPUExt);
     FindFirst(InFile,$20,Dir);
     while (DosError=0) do
      begin
        DoPPU(SplitPath(InFile)+Dir.Name);
        FindNext(Dir);
      end;
{$ifdef fpc}
     FindClose(Dir);
{$endif}
   end;
{ Display the files }
  while assigned(outfiles) do
   begin
     p:=outfiles;
     write(outfiles^.name);
     outfiles:=outfiles^.next;
     dispose(p);
     if assigned(outfiles) then
      write(' ');
   end;
end.
{
  $Log: ppufiles.pp,v $
  Revision 1.1.2.3  2001/08/17 16:16:52  florian
    + support for PalmOS added

  Revision 1.1.2.2  2001/05/18 22:35:02  peter
    * merged endian ppu fixes from mainbranch
    * endian define

  Revision 1.1.2.1  2000/11/06 13:14:48  michael
  + Fixes from Peter for slashes in filenames

  Revision 1.1  2000/07/13 10:16:22  michael
  + Initial import

  Revision 1.4  2000/07/04 19:05:54  peter
    * be optimistic: version 1.00 for some utils

  Revision 1.3  2000/01/24 12:32:22  daniel
    * use a linkedlist instead of ansistring

  Revision 1.2  2000/01/07 16:46:04  daniel
    * copyright 2000

  Revision 1.1  1999/11/23 09:44:41  peter
    * initial version
}
