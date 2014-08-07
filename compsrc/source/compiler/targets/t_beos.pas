{
    $Id: t_beos.pas,v 1.4.2.5 2003/03/16 15:36:07 carl Exp $
    Based on t_linux.pas - Copyright (c) 1998-2000 by Peter Vreman

    This unit implements support import,export,link routines
    for the (i386) BeOS target

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
unit t_beos;
interface

  uses
    import,export,link,t_linux,dos;

  type
    pimportlibbeos=^timportlibbeos;
    timportlibbeos=object(timportliblinux)
    end;

    pexportlibbeos=^texportliblinux;
    texportlibbeos=object(texportliblinux)
    end;

    plinkerbeos=^tlinkerbeos;
    tlinkerbeos=object(tlinker)
    private
      Function  WriteResponseFile(isdll:boolean;makelib:boolean) : Boolean;
    public
      constructor Init;
      procedure SetDefaultInfo;virtual;
      function  MakeExecutable:boolean;virtual;
    end;


implementation

  uses
    verbose,strings,cobjects,systems,globtype,globals,
    symconst,script,
    files,aasm,cpuasm,cpubase,symtable{$IFDEF NEWST},symbols{$ENDIF NEWST};

{*****************************************************************************
                                  TLINKERBEOS
*****************************************************************************}

Constructor TLinkerBeOS.Init;
var
 s: string;
 i: integer;
begin
  Inherited Init;
  s:=GetEnv('BELIBRARIES');
  { convert to correct format in case under unix system }
  for i:=1 to length(s) do
    if s[i] = ':' then s[i] := ';';
  { just in case we have a single path : add the ending ; }
  { since that is what the compiler expects.              }
  if pos(';',s) = 0 then
    s:=s+';';
  LibrarySearchPath.AddPath(s,true); {format:'path1;path2;...'}
end;


procedure TLinkerBeOS.SetDefaultInfo;
begin
  with Info do
   begin
     ExeCmd[1]:='sh $RES $EXE $OPT $STATIC $STRIP -L.';
{     ExeCmd[1]:='sh $RES $EXE $OPT $DYNLINK $STATIC $STRIP -L.';}
      DllCmd[1]:='sh $RES $EXE $OPT -L.';

{     DllCmd[1]:='sh $RES $EXE $OPT -L. -g -nostart -soname=$EXE';
 }    DllCmd[2]:='strip --strip-unneeded $EXE';
{     DynamicLinker:='/lib/ld-linux.so.2';}
   end;
end;


function TLinkerBeOS.WriteResponseFile(isdll:boolean;makelib:boolean) : Boolean;
Var
  linkres      : TLinkRes;
  i            : longint;
  cprtobj,
  prtobj       : string[80];
{$IFDEF NEWST}
  HPath        : PStringItem;
{$ELSE}
  HPath        : PStringQueueItem;
{$ENDIF NEWST}
  s            : string;
  found,
  linkdynamic,
  linklibc:boolean;
begin
  WriteResponseFile:=False;
{ set special options for some targets }
  linkdynamic:=not(SharedLibFiles.empty);
  linklibc:=SharedLibFiles.Find('root');
  
  prtobj:='prt0';
  cprtobj:='cprt0';
  if (cs_profile in aktmoduleswitches) or
     (not SharedLibFiles.Empty) then begin
    AddSharedLibrary('root');
    linklibc:=true;
  end;
  
  if (not linklibc) and makelib then begin
    linklibc:=true;
    cprtobj:='dllprt.o';
  end;
  
  if linklibc=true then begin
    prtobj:=cprtobj;
  end;

  { Open link.res file }
  LinkRes.Init(outputexedir+Info.ResName);

  if not isdll then LinkRes.Add('ld -o $1 $2 $3 $4 $5 $6 $7 $8 $9 \')
  else LinkRes.Add('ld -o $1 -e 0 $2 $3 $4 $5 $6 $7 $8 $9\');
  
  LinkRes.Add('-m elf_i386_be -shared -Bsymbolic \');
  
  { Write path to search libraries }
  HPath:=current_module^.locallibrarysearchpath.First;
  while assigned(HPath) do
   begin
     LinkRes.Add('-L'+HPath^.Data^+' \');
     HPath:=HPath^.Next;
   end;
  HPath:=LibrarySearchPath.First;
  while assigned(HPath) do
   begin
     LinkRes.Add('-L'+HPath^.Data^+' \');
     HPath:=HPath^.Next;
   end;

  { try to add crti and crtbegin if linking to C }
  if linklibc then
   begin
     s:=librarysearchpath.FindFile('crti.o',found)+'crti.o';
     if found then LinkRes.AddFileName(s+' \');

     s:=librarysearchpath.FindFile('crtbegin.o',found)+'crtbegin.o';
     if found then LinkRes.AddFileName(s+' \');

{      s:=librarysearchpath.FindFile('start_dyn.o',found)+'start_dyn.o';
     if found then LinkRes.AddFileName(s+' \');}

     if prtobj<>'' then LinkRes.AddFileName(FindObjectFile(prtobj,'')+' \');
     
     if isdll then LinkRes.AddFileName(FindObjectFile('func.o','')+' \');

      s:=librarysearchpath.FindFile('init_term_dyn.o',found)+'init_term_dyn.o';
     if found then LinkRes.AddFileName(s+' \');

   end else begin
     if prtobj<>'' then LinkRes.AddFileName(FindObjectFile(prtobj,'')+' \');
   end;


  { main objectfiles }
  while not ObjectFiles.Empty do
   begin
     s:=ObjectFiles.Get;
     if s<>'' then
      LinkRes.AddFileName(s+' \');
   end;

{  LinkRes.Add('-lroot \');
  LinkRes.Add('/boot/develop/tools/gnupro/lib/gcc-lib/i586-beos/2.9-beos-991026/crtend.o \');
  LinkRes.Add('/boot/develop/lib/x86/crtn.o \');}

  { Write staticlibraries }
  if not StaticLibFiles.Empty then
   begin
     While not StaticLibFiles.Empty do
      begin
        S:=StaticLibFiles.Get;
        LinkRes.AddFileName(s+' \')
      end;
   end;

  { Write sharedlibraries like -l<lib> }
  if not SharedLibFiles.Empty then
   begin
     While not SharedLibFiles.Empty do
      begin
        S:=SharedLibFiles.Get;
        if s<>'c' then
         begin
           i:=Pos(target_os.sharedlibext,S);
           if i>0 then
            Delete(S,i,255);
           LinkRes.Add('-l'+s+' \');
         end
        else
         begin
           linklibc:=true;
           linkdynamic:=false; { libc will include the ld-linux for us }
         end;
      end;
     { be sure that libc is the last lib }
{     if linklibc then LinkRes.Add('-lroot');}
{     if linkdynamic and (Info.DynamicLinker<>'') then
      LinkRes.AddFileName(Info.DynamicLinker);}
   end;
   if isdll then LinkRes.Add('-lroot \'); 

  { objects which must be at the end }
  if linklibc then
   begin
     s:=librarysearchpath.FindFile('crtend.o',found)+'crtend.o';
     if found then
      LinkRes.AddFileName(s+' \');
     s:=librarysearchpath.FindFile('crtn.o',found)+'crtn.o';
     if found then
      LinkRes.AddFileName(s+' \');
   end;

{ Write and Close response }
  linkres.Add(' ');
  linkres.writetodisk;
  linkres.done;

  WriteResponseFile:=True;
end;


function TLinkerBeOS.MakeExecutable:boolean;
var
  binstr,
  cmdstr  : string;
  success : boolean;
{  DynLinkStr : string[60];}
  StaticStr,
  StripStr   : string[40];
begin
  if not(cs_link_extern in aktglobalswitches) then
   Message1(exec_i_linking,current_module^.exefilename^);

{ Create some replacements }
  StaticStr:='';
  StripStr:='';
{  DynLinkStr:='';}
  if (cs_link_staticflag in aktglobalswitches) then
   StaticStr:='-static';
  if (cs_link_strip in aktglobalswitches) then
   StripStr:='-s';
{  If (cs_profile in aktmoduleswitches) or
     ((Info.DynamicLinker<>'') and (not SharedLibFiles.Empty)) then
   DynLinkStr:='-dynamic-linker='+Info.DynamicLinker;}

{ Write used files and libraries }
  WriteResponseFile(false,false);

{ Call linker }
  SplitBinCmd(Info.ExeCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$EXE',current_module^.exefilename^);
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  Replace(cmdstr,'$RES',outputexedir+Info.ResName);
  Replace(cmdstr,'$STATIC',StaticStr);
  Replace(cmdstr,'$STRIP',StripStr);
{  Replace(cmdstr,'$DYNLINK',DynLinkStr);}
  success:=DoExec(FindUtil(BinStr),CmdStr,true,false);

{ Remove ReponseFile }
  if (success) and not(cs_link_extern in aktglobalswitches) then
   RemoveFile(outputexedir+Info.ResName);

  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;


(* Currently unsupported
Function TLinkerBeOS.MakeSharedLibrary:boolean;
var
  binstr,
  cmdstr  : string;
  success : boolean;
begin
  MakeSharedLibrary:=false;
  if not(cs_link_extern in aktglobalswitches) then
   Message1(exec_i_linking,current_module^.sharedlibfilename^);

{ Write used files and libraries }
  WriteResponseFile(true,true);

{ Call linker }
  SplitBinCmd(Info.DllCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$EXE',current_module^.sharedlibfilename^);
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  Replace(cmdstr,'$RES',outputexedir+Info.ResName);
  success:=DoExec(FindUtil(binstr),cmdstr,true,false);

{ Strip the library ? }
  if success and (cs_link_strip in aktglobalswitches) then
   begin
     SplitBinCmd(Info.DllCmd[2],binstr,cmdstr);
     Replace(cmdstr,'$EXE',current_module^.sharedlibfilename^);
     success:=DoExec(FindUtil(binstr),cmdstr,true,false);
   end;

{ Remove ReponseFile }
  if (success) and not(cs_link_extern in aktglobalswitches) then
   RemoveFile(outputexedir+Info.ResName);

  MakeSharedLibrary:=success;   { otherwise a recursive call to link method }
end;*)


end.
{
  $Log: t_beos.pas,v $
  Revision 1.4.2.5  2003/03/16 15:36:07  carl
    * Creation of shared libraries now gives out  an error (since they are not supported)

  Revision 1.4.2.4  2001/10/12 16:05:03  peter
    * fixed typos

  Revision 1.4.2.3  2001/10/07 01:17:01  carl
  * Correct searching for system library paths

  Revision 1.4.2.2  2001/08/22 02:42:00  carl
  - remove debug line

  Revision 1.4.2.1  2001/07/13 03:22:32  carl
  + BeOS support


}
