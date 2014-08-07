{
    $Id: ppumove.pp,v 1.1.2.4 2002/07/14 14:47:31 carl Exp $
    Copyright (c) 1999-2000 by the FPC Development Team

    Add multiple FPC units into a static/shared library

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
{$ifndef TP}
  {$H+}
{$endif}
Program ppumove;
uses
{$ifdef linux}
  linux,
{$else linux}
  dos,
{$endif linux}
  ppu,
  getopts;

const
  Version   = 'Version 1.00';
  Title     = 'PPU-Mover';
  Copyright = 'Copyright (c) 1998-2000 by the Free Pascal Development Team';

  ShortOpts = 'o:e:d:qhsvbw';
  BufSize = 4096;
  PPUExt = 'ppu';
  ObjExt = 'o';
  StaticLibExt ='a';
{$ifdef Linux}
  SharedLibExt ='so';
  BatchExt     ='.sh';
{$else}
  SharedLibExt ='dll';
  BatchExt     ='.bat';
{$endif Linux}

  { link options }
  link_none    = $0;
  link_allways = $1;
  link_static  = $2;
  link_smart   = $4;
  link_shared  = $8;

Type
  PLinkOEnt = ^TLinkOEnt;
  TLinkOEnt = record
    Name : string;
    Next : PLinkOEnt;
  end;

Var
  ArBin,LDBin,StripBin,
  OutputFile,
  OutputFileForLink,  { the name of the output file needed when linking }
  DestPath,
  PPLExt,
  LibExt      : string;
  Batch,
  Quiet,
  MakeStatic  : boolean;
  Buffer      : Pointer;
  ObjFiles    : PLinkOEnt;
  BatchFile   : Text;

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


function Shell(const s:string):longint;
{
  Run a shell commnad and return the exitcode
}
begin
  if Batch then
   begin
     Writeln(BatchFile,s);
     Shell:=0;
     exit;
   end;
{$ifdef Linux}
  Shell:=Linux.shell(s);
{$else}
  exec(getenv('COMSPEC'),'/C '+s);
  Shell:=DosExitCode;
{$endif}
end;


Function FileExists (Const F : String) : Boolean;
{
  Returns True if the file exists, False if not.
}
Var
{$ifdef linux}
  info : Stat;
{$else}
  info : searchrec;
{$endif}
begin
{$ifdef linux}
  FileExists:=FStat (F,Info);
{$else}
  FindFirst (F,anyfile,Info);
  FileExists:=DosError=0;
{$endif}
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


Function ForceExtension(Const HStr,ext:String):String;
{
  Return a filename which certainly has the extension ext
}
var
  j : longint;
begin
  j:=length(Hstr);
  while (j>0) and (Hstr[j]<>'.') do
   dec(j);
  if j=0 then
   j:=255;
  ForceExtension:=Copy(Hstr,1,j-1)+'.'+Ext;
end;


Procedure AddToLinkFiles(const S : String);
{
  Adds a filename to a list of object files to link to.
  No duplicates allowed.
}
Var
  P : PLinKOEnt;
begin
  P:=ObjFiles;
  { Don't add files twice }
  While (P<>nil) and (p^.name<>s) do
    p:=p^.next;
  if p=nil then
   begin
     new(p);
     p^.next:=ObjFiles;
     p^.name:=s;
     ObjFiles:=P;
   end;
end;


Function ExtractLib(const libfn:string):string;
{
  Extract a static library libfn and return the files with a
  wildcard
}
var
  n : namestr;
  d : dirstr;
  e : extstr;
begin
{ create the temp dir first }
  fsplit(libfn,d,n,e);
  {$I-}
   mkdir(n+'.sl');
  {$I+}
  if ioresult<>0 then;
{ Extract }
  if Shell(arbin+' x '+libfn)<>0 then
   Error('Fatal: Error running '+arbin,true);
{ Remove the lib file, it's extracted so it can be created with ease }
  if PPLExt=PPUExt then
   Shell('rm '+libfn);
{$ifdef linux}
  ExtractLib:=n+'.sl/*';
{$else}
  ExtractLib:=n+'.sl\*';
{$endif}
end;


Function DoPPU(const PPUFn,PPLFn:String):Boolean;
{
  Convert one file (in Filename) to library format.
  Return true if successful, false otherwise.
}
Var
  inppu,
  outppu : pppufile;
  b,
  untilb : byte;
  l,m    : longint;
  f      : file;
  s      : string;
begin
  DoPPU:=false;
  If Not Quiet then
   Write ('Processing ',PPUFn,'...');
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
{ No .o file generated for this ppu, just skip }
  if (inppu^.header.flags and uf_no_link)<>0 then
   begin
     dispose(inppu,done);
     If Not Quiet then
      Writeln (' No files.');
     DoPPU:=true;
     Exit;
   end;
{ Already a lib? }
  if (inppu^.header.flags and uf_in_library)<>0 then
   begin
     dispose(inppu,done);
     Error('Error: PPU is already in a library : '+PPUFn,false);
     Exit;
   end;
{ We need a static linked unit }
  if (inppu^.header.flags and uf_static_linked)=0 then
   begin
     dispose(inppu,done);
     Error('Error: PPU is not static linked : '+PPUFn,false);
     Exit;
   end;
{ Create the new ppu }
  if PPUFn=PPLFn then
   outppu:=new(pppufile,init('ppumove.$$$'))
  else
   outppu:=new(pppufile,init(PPLFn));
  outppu^.createfile;
{ Create new header, with the new flags }
  outppu^.header:=inppu^.header;
  outppu^.header.flags:=outppu^.header.flags or uf_in_library;
  if MakeStatic then
   outppu^.header.flags:=outppu^.header.flags or uf_static_linked
  else
   outppu^.header.flags:=outppu^.header.flags or uf_shared_linked;
{ read until the object files are found }
  untilb:=iblinkunitofiles;
  repeat
    b:=inppu^.readentry;
    if b in [ibendinterface,ibend] then
     begin
       dispose(inppu,done);
       dispose(outppu,done);
       Error('Error: No files to be linked found : '+PPUFn,false);
       Exit;
     end;
    if b<>untilb then
     begin
       repeat
         inppu^.getdatabuf(buffer^,bufsize,l);
         outppu^.putdata(buffer^,l);
       until l<bufsize;
       outppu^.writeentry(b);
     end;
  until (b=untilb);
{ we have now reached the section for the files which need to be added,
  now add them to the list }
  case b of
    iblinkunitofiles :
      begin
        { add all o files, and save the entry when not creating a static
          library to keep staticlinking possible }
        while not inppu^.endofentry do
         begin
           s:=inppu^.getstring;
           m:=inppu^.getlongint;
           if not MakeStatic then
            begin
              outppu^.putstring(s);
              outppu^.putlongint(m);
            end;
           AddToLinkFiles(s);
         end;
        if not MakeStatic then
         outppu^.writeentry(b);
      end;
{    iblinkunitstaticlibs :
      begin
        AddToLinkFiles(ExtractLib(inppu^.getstring));
        if not inppu^.endofentry then
         begin
           repeat
             inppu^.getdatabuf(buffer^,bufsize,l);
             outppu^.putdata(buffer^,l);
           until l<bufsize;
           outppu^.writeentry(b);
         end;
       end; }
  end;
{ just add a new entry with the new lib }
  if MakeStatic then
   begin
     outppu^.putstring(outputfileforlink);
     outppu^.putlongint(link_static);
     outppu^.writeentry(iblinkunitstaticlibs)
   end
  else
   begin
     outppu^.putstring(outputfileforlink);
     outppu^.putlongint(link_shared);
     outppu^.writeentry(iblinkunitsharedlibs);
   end;
{ read all entries until the end and write them also to the new ppu }
  repeat
    b:=inppu^.readentry;
  { don't write ibend, that's written automaticly }
    if b<>ibend then
     begin
       repeat
         inppu^.getdatabuf(buffer^,bufsize,l);
         outppu^.putdata(buffer^,l);
       until l<bufsize;
       outppu^.writeentry(b);
     end;
  until b=ibend;
{ write the last stuff and close }
  outppu^.flush;
  outppu^.writeheader;
  dispose(outppu,done);
  dispose(inppu,done);
{ rename }
  if PPUFn=PPLFn then
   begin
     {$I-}
      assign(f,PPUFn);
      erase(f);
      assign(f,'ppumove.$$$');
      rename(f,PPUFn);
     {$I+}
     if ioresult<>0 then;
   end;
{ the end }
  If Not Quiet then
   Writeln (' Done.');
  DoPPU:=True;
end;


Function DoFile(const FileName:String):Boolean;
{
  Process a file, mainly here for wildcard support under Dos
}
{$ifndef linux}
var
  dir : searchrec;
{$endif}
begin
{$ifdef linux}
  DoFile:=DoPPU(FileName,ForceExtension(FileName,PPLExt));
{$else}
  DoFile:=false;
  findfirst(filename,$20,dir);
  while doserror=0 do
   begin
     if not DoPPU(Dir.Name,ForceExtension(Dir.Name,PPLExt)) then
      exit;
     findnext(dir);
   end;
  findclose(dir);
  DoFile:=true;
{$endif}
end;


Procedure DoLink;
{
  Link the object files together to form a (shared) library, the only
  problem here is the 255 char limit of Names
}
Var
  Names : String;
  f     : file;
  Err   : boolean;
  P     : PLinkOEnt;
begin
  if not Quiet then
   Write ('Linking ');
  P:=ObjFiles;
  names:='';
  While p<>nil do
   begin
     if Names<>'' then
      Names:=Names+' '+P^.name
     else
      Names:=p^.Name;
     p:=p^.next;
   end;
  if Names='' then
   begin
     If not Quiet then
      Writeln('Error: no files found to be linked');
     exit;
   end;
  If not Quiet then
   WriteLn(names);
{ Run ar or ld to create the lib }
  If MakeStatic then
   Err:=Shell(arbin+' rs '+outputfile+' '+names)<>0
  else
   begin
     Err:=Shell(ldbin+' -shared -o '+OutputFile+' '+names)<>0;
     if not Err then
      Shell(stripbin+' --strip-unneeded '+OutputFile);
   end;
  If Err then
   Error('Fatal: Library building stage failed.',true);
{ fix permission to 644, so it's not 755 }
{$ifdef Linux}
  ChMod(OutputFile,420);
{$endif}
{ Rename to the destpath }
  if DestPath<>'' then
   begin
     Assign(F, OutputFile);
     Rename(F,DestPath+'/'+OutputFile);
   end;
end;


Procedure usage;
{
  Print usage and exit.
}
begin
  Writeln(paramstr(0),': [-qhwvbs] [-e ext] [-o name] [-d path] file [file ...]');
  Halt(0);
end;



Procedure processopts;
{
  Process command line opions, and checks if command line options OK.
}
var
  C : char;
begin
  if paramcount=0 then
   usage;
{ Reset }
  ObjFiles:=Nil;
  Quiet:=False;
  Batch:=False;
  OutputFile:='';
  PPLExt:='ppu';
  ArBin:='ar';
  LdBin:='ld';
  StripBin:='strip';
  repeat
    c:=Getopt (ShortOpts);
    Case C of
      EndOfOptions : break;
      's' : MakeStatic:=True;
      'o' : OutputFile:=OptArg;
      'd' : DestPath:=OptArg;
      'e' : PPLext:=OptArg;
      'q' : Quiet:=True;
      'w' : begin
              ArBin:='arw';
              LdBin:='ldw';
            end;
      'b' : Batch:=true;
      '?' : Usage;
      'h' : Usage;
    end;
  until false;
{ Test filenames on the commandline }
  if (OptInd>Paramcount) then
   Error('Error: no input files',true);
  if (OptInd<ParamCount) and (OutputFile='') then
   Error('Error: when moving multiple units, specify an output name.',true);
{ alloc a buffer }
  GetMem (Buffer,Bufsize);
  If Buffer=Nil then
   Error('Error: could not allocate memory for buffer.',true);
end;


var
  i : longint;
begin
  ProcessOpts;
{ Write Header }
  if not Quiet then
   begin
     Writeln(Title+' '+Version);
     Writeln(Copyright);
     Writeln;
   end;
{ Check if shared is allowed }
{$ifndef linux}
  if arbin<>'arw' then
   begin
     Writeln('Warning: shared library not supported for Go32, switching to static library');
     MakeStatic:=true;
   end;
{$endif}
{ fix the libext and outputfilename }
  if Makestatic then
   LibExt:=StaticLibExt
  else
   LibExt:=SharedLibExt;
  if OutputFile='' then
   OutPutFile:=Paramstr(OptInd);
{ fix filename }
{$ifdef linux}
  if Copy(OutputFile,1,3)<>'lib' then
   OutputFile:='lib'+OutputFile;
  { For linux skip replacing the extension if a full .so.X.X if specified }
  i:=pos('.so.',Outputfile);
  if i<>0 then
   OutputFileForLink:=Copy(Outputfile,4,i-4)
  else
   begin
     OutputFile:=ForceExtension(OutputFile,LibExt);
     OutputFileForLink:=Copy(Outputfile,4,length(Outputfile)-length(LibExt)-4);
   end;
{$else}
  OutputFile:=ForceExtension(OutputFile,LibExt);
  OutputFileForLink:=OutputFile;
{$endif}
{ Open BatchFile }
  if Batch then
   begin
     Assign(BatchFile,'pmove'+BatchExt);
     Rewrite(BatchFile);
   end;
{ Process Files }
  i:=OptInd;
  While (i<=ParamCount) and Dofile(AddExtension(Paramstr(i),PPUExt)) do
   Inc(i);
{ Do Linking stage }
  DoLink;
{ Close BatchFile }
  if Batch then
   begin
     if Not Quiet then
      Writeln('Writing pmove'+BatchExt);
     Close(BatchFile);
{$ifdef Linux}
     ChMod('pmove'+BatchExt,493);
{$endif}
   end;
{ The End }
  if Not Quiet then
   Writeln('Done.');
end.
{
  $Log: ppumove.pp,v $
  Revision 1.1.2.4  2002/07/14 14:47:31  carl
    - fix compilation problems (revert back to old files)

  Revision 1.1.2.2  2001/05/18 22:35:02  peter
    * merged endian ppu fixes from mainbranch
    * endian define

  Revision 1.1  2000/07/13 10:16:22  michael
  + Initial import

  Revision 1.11  2000/07/04 19:05:54  peter
    * be optimistic: version 1.00 for some utils

  Revision 1.10  2000/05/17 18:30:57  peter
    * libname fixes for linux

  Revision 1.9  2000/02/09 16:44:15  peter
    * log truncated

  Revision 1.8  2000/01/07 16:46:04  daniel
    * copyright 2000

  Revision 1.7  1999/11/25 00:00:39  peter
    * strip created .so file with strip --strip-unneeded

  Revision 1.6  1999/11/23 09:44:15  peter
    * updated

  Revision 1.5  1999/07/29 01:40:21  peter
    * fsplit var type fixes

  Revision 1.4  1999/07/28 16:53:58  peter
    * updated for new linking, but still doesn't work because ld-linux.so.2
      requires some more crt*.o files

}
