{
    $Id: t_atari.pas,v 1.13.2.1 2002/10/18 21:52:15 carl Exp $
    Copyright (c) 1998-2000 by Pierre Muller

    This unit implements support import,export,link routines
    for the (m68k) Atari target

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
unit t_atari;

  interface
  uses
    link;

  type
    plinkerAtari=^tlinkerAtari;
    tlinkerAtari=object(tlinker)
    private
       Function  WriteResponseFile(isdll:boolean) : Boolean;
    public
       constructor Init;
       procedure SetDefaultInfo;virtual;
       function  MakeExecutable:boolean;virtual;
    end;


  implementation

    uses
       strings,globtype,globals,cobjects,systems,verbose,script,files;


{****************************************************************************
                               TLinkerAmiga
****************************************************************************}

Constructor TLinkerAtari.Init;
begin
  Inherited Init;
  { allow duplicated libs (PM) }
  SharedLibFiles.doubles:=true;
  StaticLibFiles.doubles:=true;
end;


procedure TLinkerAtari.SetDefaultInfo;
begin
  with Info do
   begin
     if (cs_link_on_target in aktglobalswitches) then
        ExeCmd[1]:='ld $OPT $STRIP -o $EXE --script @$RES'
     else
        ExeCmd[1]:='ld $OPT $STRIP -o $EXE --script $RES'
   end;
end;


Function TLinkerAtari.WriteResponseFile(isdll:boolean) : Boolean;
Var
  linkres  : TLinkRes;
  i        : longint;
  HPath    : PStringQueueItem;
  s        : string;
  linklibc : boolean;
begin
  WriteResponseFile:=False;

  { Open link.res file }
  LinkRes.Init(outputexedir+Info.ResName);
  LinkRes.Add('OUTPUT_FORMAT("a.out-mintprg", "a.out-mintprg","a.out-mintprg")');
  LinkRes.Add('OUTPUT_ARCH(m68k)');
  LinkRes.Add('SEARCH_DIR(/usr/m68k-atari-mint/lib)');
  LinkRes.Add('PROVIDE (__stack = 0);');
  LinkRes.Add('SECTIONS');
  LinkRes.Add('{');
  LinkRes.Add('. = 0xe4;');
  LinkRes.Add('.text :');
  LinkRes.Add('{');
  LinkRes.Add('CREATE_OBJECT_SYMBOLS');
    LinkRes.Add('*(.text)');
    LinkRes.Add('*(.dynrel)');
    LinkRes.Add('*(.hash)');
    LinkRes.Add('*(.dynsym)');
    LinkRes.Add('*(.dynstr)');
    LinkRes.Add('*(.rules)');
    LinkRes.Add('*(.need)');
    LinkRes.Add('_etext = .;');
    LinkRes.Add('__etext = .;');
  LinkRes.Add('}');
  LinkRes.Add('. = ALIGN(2);');
  LinkRes.Add('.data :');
  LinkRes.Add('{');
  
  LinkRes.Add('*(.dynamic)');
  LinkRes.Add('  *(.got)');
  LinkRes.Add('  *(.plt)');
  LinkRes.Add('  *(.data)');
  LinkRes.Add('  *(.linux-dynamic) /* For Linux dynamic linking.  */');
  LinkRes.Add('  CONSTRUCTORS');
  LinkRes.Add('  _edata  =  .;');
  LinkRes.Add('  __edata  =  .;');
  LinkRes.Add('}');
  LinkRes.Add('.bss :');
  LinkRes.Add('{');
  LinkRes.Add(' __bss_start = .;');
  LinkRes.Add('*(.bss)');
  LinkRes.Add('*(COMMON)');
  LinkRes.Add('_end = ALIGN(4) ;');
  LinkRes.Add('__end = ALIGN(4) ;');
  LinkRes.Add('}');
  LinkRes.Add('}');
  
  

  { Write path to search libraries }
  HPath:=current_module^.locallibrarysearchpath.First;
  while assigned(HPath) do
   begin
     s:=HPath^.Data^;
     if not (cs_link_on_target in aktglobalswitches) then
       s:=GetShortName(s)
     else
       s:=ScriptFixFileName(s);
     LinkRes.Add('-L'+s);
     HPath:=HPath^.Next;
   end;
  HPath:=LibrarySearchPath.First;
  while assigned(HPath) do
   begin
     s:=HPath^.Data^;
     if not (cs_link_on_target in aktglobalswitches) then
       s:=GetShortName(s);
     LinkRes.Add('-L'+s);
     HPath:=HPath^.Next;
   end;

  LinkRes.Add('INPUT (');
  { add objectfiles, start with prt0 always }
  s:=FindObjectFile('prt0','');
  if not (cs_link_on_target in aktglobalswitches) then
    s:=GetShortName(s);
  LinkRes.AddFileName(s);
  while not ObjectFiles.Empty do
   begin
     s:=ObjectFiles.Get;
     if not (cs_link_on_target in aktglobalswitches) then
       s:=GetShortName(s);
     if s<>'' then
      LinkRes.AddFileName(s);
   end;
  LinkRes.Add(')');

  { Write staticlibraries }
  if not StaticLibFiles.Empty then
   begin
     LinkRes.Add('GROUP(');
     While not StaticLibFiles.Empty do
      begin
        S:=StaticLibFiles.Get;
        if not (cs_link_on_target in aktglobalswitches) then
          s:=GetShortName(s);
        LinkRes.AddFileName(s);
      end;
     LinkRes.Add(')');
   end;

  { Write sharedlibraries like -l<lib>, also add the needed dynamic linker
    here to be sure that it gets linked this is needed for glibc2 systems (PFV) }
  linklibc:=false;
  While not SharedLibFiles.Empty do
   begin
     S:=SharedLibFiles.Get;
     if s<>'c' then
      begin
        i:=Pos(target_os.sharedlibext,S);
        if i>0 then
         Delete(S,i,255);
        LinkRes.Add('-l'+s);
      end
     else
      begin
        LinkRes.Add('-l'+s);
        linklibc:=true;
      end;
   end;
  { be sure that libc&libgcc is the last lib }
  if linklibc then
   begin
     LinkRes.Add('-lc');
     LinkRes.Add('-lgcc');
   end;

{ Write and Close response }
  linkres.writetodisk;
  linkres.done;

  WriteResponseFile:=True;
end;


function TLinkerAtari.MakeExecutable:boolean;
var
  binstr,
  cmdstr  : string;
  success : boolean;
  StripStr : string[40];
begin
  if not(cs_link_extern in aktglobalswitches) then
   Message1(exec_i_linking,current_module^.exefilename^);

{ Create some replacements }
  StripStr:='';
  if (cs_link_strip in aktglobalswitches) then
   StripStr:='-s';

{ Write used files and libraries }
  WriteResponseFile(false);

{ Call linker }
  SplitBinCmd(Info.ExeCmd[1],binstr,cmdstr);
  if pos(' ',current_module^.exefilename^)>0 then
    Replace(cmdstr,'$EXE','"'+ScriptFixFileName(current_module^.exefilename^)+'"')
  else
    Replace(cmdstr,'$EXE',ScriptFixFileName(current_module^.exefilename^));
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  Replace(cmdstr,'$RES',ScriptFixFileName(outputexedir+Info.ResName));
  Replace(cmdstr,'$STRIP',StripStr);
  success:=DoExec(FindUtil(BinStr),cmdstr,true,false);

{ Remove ReponseFile }
  if (success) and not(cs_link_extern in aktglobalswitches) then
   RemoveFile(outputexedir+Info.ResName);

  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;



end.
{
  $Log: t_atari.pas,v $
  Revision 1.13.2.1  2002/10/18 21:52:15  carl
    + atari target

}
