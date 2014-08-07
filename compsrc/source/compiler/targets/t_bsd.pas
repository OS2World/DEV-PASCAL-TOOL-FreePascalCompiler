{
    $Id: t_bsd.pas,v 1.1.2.7 2003/02/02 18:26:34 carl Exp $
    Copyright (c) 1998-2000 by Peter Vreman (original Linux)
              (c) 2000      by Marco van de Voort (FreeBSD mods)

    This unit implements support import,export,link routines
    for the (i386)FreeBSD target

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
unit t_bsd;

interface

  uses
    import,export,link;

  type
    pimportlibbsd=^timportlibbsd;
    timportlibbsd=object(timportlib)
      procedure preparelib(const s:string);virtual;
      procedure importprocedure(const func,module:string;index:longint;const name:string);virtual;
      procedure importvariable(const varname,module:string;const name:string);virtual;
      procedure generatelib;virtual;
    end;

    pexportlibbsd=^texportlibbsd;
    texportlibbsd=object(texportlib)
      procedure preparelib(const s : string);virtual;
      procedure exportprocedure(hp : pexported_item);virtual;
      procedure exportvar(hp : pexported_item);virtual;
      procedure generatelib;virtual;
    end;

    plinkerbsd=^tlinkerbsd;
    tlinkerbsd=object(tlinker)
    private
      LdSupportsNoResponseFile : boolean;
      LibrarySuffix  : char;
      Function  WriteResponseFile(isdll:boolean) : Boolean;
    public
      constructor Init;
      procedure SetDefaultInfo;virtual;
      function  MakeExecutable:boolean;virtual;
      function  MakeSharedLibrary:boolean;virtual;
    end;


implementation

  uses
    verbose,strings,cobjects,systems,globtype,globals,
    symconst,script,
    files,aasm,cpuasm,cpubase,symtable{$IFDEF NEWST},symbols{$ENDIF NEWST};

{*****************************************************************************
                               TIMPORTLIBLINUX
*****************************************************************************}

procedure timportlibbsd.preparelib(const s : string);
begin
end;


procedure timportlibbsd.importprocedure(const func,module : string;index : longint;const name : string);
begin
  { insert sharedlibrary }
{$IFDEF NEWST}
  current_module^.linkothersharedlibs.
   insert(new(Plinkitem,init(SplitName(module),link_allways)));
  { do nothing with the procedure, only set the mangledname }
  if name<>'' then
    aktprocdef^.setmangledname(name)
  else
    message(parser_e_empty_import_name);
{$ELSE}
  current_module^.linkothersharedlibs.
   insert(SplitName(module),link_allways);
  { do nothing with the procedure, only set the mangledname }
  if name<>'' then
    aktprocsym^.definition^.setmangledname(name)
  else
    message(parser_e_empty_import_name);
{$ENDIF NEWST}
end;


procedure timportlibbsd.importvariable(const varname,module:string;const name:string);
begin
  { insert sharedlibrary }
{$IFDEF NEWST}
  current_module^.linkothersharedlibs.
   insert(new(Plinkitem,init(SplitName(module),link_allways)));
{$ELSE}
  current_module^.linkothersharedlibs.
   insert(SplitName(module),link_allways);
{$ENDIF NEWST}
  { reset the mangledname and turn off the dll_var option }
  aktvarsym^.setmangledname(name);
{$IFDEF NEWST}
  exclude(aktvarsym^.properties,vo_is_dll_var);
{$ELSE}
{$ifdef INCLUDEOK}
  exclude(aktvarsym^.varoptions,vo_is_dll_var);
{$else}
  aktvarsym^.varoptions:=aktvarsym^.varoptions-[vo_is_dll_var];
{$endif}
{$ENDIF NEWST}
end;


procedure timportlibbsd.generatelib;
begin
end;


{*****************************************************************************
                               TEXPORTLIBLINUX
*****************************************************************************}

procedure texportlibbsd.preparelib(const s:string);
begin
end;


procedure texportlibbsd.exportprocedure(hp : pexported_item);
var
  hp2 : pexported_item;
begin
  { first test the index value }
  if (hp^.options and eo_index)<>0 then
   begin
     Message1(parser_e_no_export_with_index_for_target,target_os.shortname);
     exit;
   end;
  { now place in correct order }
  hp2:=pexported_item(current_module^._exports^.first);
  while assigned(hp2) and
     (hp^.name^>hp2^.name^) do
    hp2:=pexported_item(hp2^.next);
  { insert hp there !! }
  if assigned(hp2) and (hp2^.name^=hp^.name^) then
    begin
      { this is not allowed !! }
      Message1(parser_e_export_name_double,hp^.name^);
      exit;
    end;
  if hp2=pexported_item(current_module^._exports^.first) then
    current_module^._exports^.insert(hp)
  else if assigned(hp2) then
    begin
       hp^.next:=hp2;
       hp^.previous:=hp2^.previous;
       if assigned(hp2^.previous) then
         hp2^.previous^.next:=hp;
       hp2^.previous:=hp;
    end
  else
    current_module^._exports^.concat(hp);
end;


procedure texportlibbsd.exportvar(hp : pexported_item);
begin
  hp^.is_var:=true;
  exportprocedure(hp);
end;


procedure texportlibbsd.generatelib;
var
  hp2 : pexported_item;
begin
  hp2:=pexported_item(current_module^._exports^.first);
  while assigned(hp2) do
   begin
     if not hp2^.is_var then
      begin
{$ifdef i386}
        { place jump in codesegment }
        codesegment^.concat(new(pai_align,init_op(4,$90)));
        codesegment^.concat(new(pai_symbol,initname_global(hp2^.name^,0)));
        codesegment^.concat(new(paicpu,op_sym(A_JMP,S_NO,newasmsymbol(hp2^.sym^.mangledname))));
        codesegment^.concat(new(pai_symbol_end,initname(hp2^.name^)));
{$endif i386}
{$ifdef m68k}
           { place jump in codesegment }
           codesegment^.concat(new(pai_align,init(4)));
           codesegment^.concat(new(pai_symbol,initname_global(hp2^.name^,0)));
           codesegment^.concat(new(paicpu,op_sym(A_JMP,S_NO,newasmsymbol(hp2^.sym^.mangledname))));
           codesegment^.concat(new(pai_symbol_end,initname(hp2^.name^)));
{$endif m68k}
      end
     else
      Message1(parser_e_no_export_of_variables_for_target,target_os.shortname);
     hp2:=pexported_item(hp2^.next);
   end;
end;


{*****************************************************************************
                                  TLINKERLINUX
*****************************************************************************}

Constructor TLinkerBSD.Init;
begin
  Inherited Init;
  LibrarySearchPath.AddPath('/lib;/usr/lib;/usr/X11R6/lib',true);
end;


procedure TLinkerBSD.SetDefaultInfo;
{
  This will also detect which libc version will be used (used to for
   Linux. I keep that code in place, for a possible next libc major version
   transition.
}
begin
 LibrarySuffix:=' ';
 {If target_os.id=os_m68k_netbsd Then
  LdSupportsNoResponseFile:=true
   that was only a problem for the old aout m68k netbsd PM
 else                     }
  LdSupportsNoResponseFile:=false;
  with Info do
   begin
     if LdSupportsNoResponseFile then
       begin
         ExeCmd[1]:='ld $OPT $DYNLINK $STATIC $STRIP -L. -o $EXE `cat $RES`';
         { We need external linking to interpret the `cat $RES` PM }
         aktglobalswitches:=aktglobalswitches+[cs_link_extern];
       end
     else
       ExeCmd[1]:='ld $OPT $DYNLINK $STATIC $STRIP -L. -o $EXE $RES';
     DllCmd[1]:='ld $OPT -shared -L. -o $EXE $RES';
     DllCmd[2]:='strip --strip-unneeded $EXE';

     if target_os.id=os_i386_netbsd Then
      DynamicLinker:='/usr/libexec/ld.elf_so'
     Else
      DynamicLinker:='';
   end;
end;


Function TLinkerBSD.WriteResponseFile(isdll:boolean) : Boolean;
Var
  linkres      : TLinkRes;
  i            : longint;
  cprtobj,
  gprtobj,
  prtobj       : string[80];
{$IFDEF NEWST}
  HPath        : PStringItem;
{$ELSE}
  HPath        : PStringQueueItem;
{$ENDIF NEWST}
  s            : string;
  found,
  linkdynamic,
  linklibc     : boolean;
begin
  WriteResponseFile:=False;
{ set special options for some targets }
  linkdynamic:=not(SharedLibFiles.empty);
  linklibc:=SharedLibFiles.Find('c');
  prtobj:='prt0';
  cprtobj:='cprt0';
  gprtobj:='gprt0';
  if cs_profile in aktmoduleswitches then
   begin
     prtobj:=gprtobj;
{     if not glibc2 then
      AddSharedLibrary('gmon');}
	AddSharedLibrary('c');
     LibrarySuffix:='p';
     linklibc:=true;
   end
  else
   begin
     if linklibc then
      prtobj:=cprtobj;
   end;

  { Open link.res file }
  LinkRes.Init(outputexedir+Info.ResName);

  { Write path to search libraries }
  HPath:=current_module^.locallibrarysearchpath.First;
  while assigned(HPath) do
   begin
     if LdSupportsNoResponseFile then
       LinkRes.Add('-L'+HPath^.Data^)
     else
       LinkRes.Add('SEARCH_DIR('+HPath^.Data^+')');
     HPath:=HPath^.Next;
   end;
  HPath:=LibrarySearchPath.First;
  while assigned(HPath) do
   begin
     if LdSupportsNoResponseFile then
       LinkRes.Add('-L'+HPath^.Data^)
     else
      LinkRes.Add('SEARCH_DIR('+HPath^.Data^+')');
     HPath:=HPath^.Next;
   end;

  if not LdSupportsNoResponseFile then
    LinkRes.Add('INPUT(');
  { add objectfiles, start with prt0 always }
  if prtobj<>'' then
   LinkRes.AddFileName(FindObjectFile(prtobj,''));
  { try to add crti and crtbegin if linking to C }
  if linklibc then
   begin
     s:=librarysearchpath.FindFile('crtbegin.o',found)+'crtbegin.o';
     if found then
      LinkRes.AddFileName(s);
     s:=librarysearchpath.FindFile('crti.o',found)+'crti.o';
     if found then
      LinkRes.AddFileName(s);
   end;
  { main objectfiles }
  while not ObjectFiles.Empty do
   begin
     s:=ObjectFiles.Get;
     if s<>'' then
      LinkRes.AddFileName(s);
   end;
  { objects which must be at the end }
  if linklibc then
   begin
     s:=librarysearchpath.FindFile('crtend.o',found)+'crtend.o';
     if found then
      LinkRes.AddFileName(s);
     s:=librarysearchpath.FindFile('crtn.o',found)+'crtn.o';
     if found then
      LinkRes.AddFileName(s);
   end;
  if not LdSupportsNoResponseFile then
    LinkRes.Add(')');

  { Write staticlibraries }
  if not StaticLibFiles.Empty then
   begin
     if not LdSupportsNoResponseFile then
       LinkRes.Add('GROUP(');
     While not StaticLibFiles.Empty do
      begin
        S:=StaticLibFiles.Get;
        LinkRes.AddFileName(s)
      end;
     if not LdSupportsNoResponseFile then
       LinkRes.Add(')');
   end;

  { Write sharedlibraries like -l<lib>, also add the needed dynamic linker
    here to be sure that it gets linked this is needed for glibc2 systems (PFV) }
  if not SharedLibFiles.Empty then
   begin
     if not LdSupportsNoResponseFile then
       LinkRes.Add('INPUT(');
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
           linklibc:=true;
           linkdynamic:=false; { libc will include the ld-linux for us }
         end;
      end;
     { be sure that libc is the last lib }
     if linklibc then
      begin	
       If LibrarySuffix=' ' Then
        LinkRes.Add('-lc')
       else  
        LinkRes.Add('-lc_'+LibrarySuffix)
      end;
     { when we have -static for the linker the we also need libgcc }
     if (cs_link_staticflag in aktglobalswitches) then
      LinkRes.Add('-lgcc');
     if linkdynamic and (Info.DynamicLinker<>'') then
      LinkRes.AddFileName(Info.DynamicLinker);
     if not LdSupportsNoResponseFile then
       LinkRes.Add(')');
   end;
{ Write and Close response }
  linkres.writetodisk;
  linkres.done;

  WriteResponseFile:=True;
end;


function TLinkerBSD.MakeExecutable:boolean;
var
  binstr,
  cmdstr  : string;
  success : boolean;
  DynLinkStr : string[60];
  StaticStr,
  StripStr   : string[40];
begin
  if not(cs_link_extern in aktglobalswitches) then
   Message1(exec_i_linking,current_module^.exefilename^);

{ Create some replacements }
  StaticStr:='';
  StripStr:='';
  DynLinkStr:='';
  if (cs_link_staticflag in aktglobalswitches) then
    begin
      if (target_os.id in [os_i386_openbsd,os_m68k_netbsd]) and
         ((cs_link_on_target in aktglobalswitches) or
         (target_os.id=source_os.id)) then
        StaticStr:='-Bstatic'			{ a.out probably}
      else
        StaticStr:='-static';
    end;
  if (cs_link_strip in aktglobalswitches) then
   StripStr:='-s';
  If (cs_profile in aktmoduleswitches) or
     ((Info.DynamicLinker<>'') and (not SharedLibFiles.Empty)) then
   DynLinkStr:='-dynamic-linker='+Info.DynamicLinker;

{ Write used files and libraries }
  WriteResponseFile(false);

{ Call linker }
  SplitBinCmd(Info.ExeCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$EXE',current_module^.exefilename^);
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  Replace(cmdstr,'$RES',outputexedir+Info.ResName);
  Replace(cmdstr,'$STATIC',StaticStr);
  Replace(cmdstr,'$STRIP',StripStr);
  Replace(cmdstr,'$DYNLINK',DynLinkStr);
  success:=DoExec(FindUtil(BinStr),CmdStr,true,false);

{ Remove ReponseFile }
  if (success) and not(cs_link_extern in aktglobalswitches) then
   RemoveFile(outputexedir+Info.ResName);
  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;


Function TLinkerBSD.MakeSharedLibrary:boolean;
var
  binstr,
  cmdstr  : string;
  success : boolean;
begin
  MakeSharedLibrary:=false;
  if not(cs_link_extern in aktglobalswitches) then
   Message1(exec_i_linking,current_module^.sharedlibfilename^);

{ Write used files and libraries }
  WriteResponseFile(true);

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
end;


end.
{
  $Log: t_bsd.pas,v $
  Revision 1.1.2.7  2003/02/02 18:26:34  carl
    * Fix complilation problem under BP

  Revision 1.1.2.6  2003/02/01 22:38:00  marco
   * several fixes, mainly a result of profiling being fixed. glibc21 stuff deleted.

  Revision 1.1.2.5  2002/12/12 00:20:53  pierre
   * add library generation support for m68K

  Revision 1.1.2.4  2002/09/20 22:16:20  pierre
   * m68k netbsd elf does support response files

  Revision 1.1.2.3  2002/07/30 18:30:14  marco
   * Removed forgotten debug measure

  Revision 1.1.2.2  2002/07/30 12:51:16  marco
   * Fixed unit name, oops

  Revision 1.1.2.1  2002/07/30 12:48:03  marco

   * t_fbsd - t_bsd renaming

  Revision 1.1.2.3  2002/01/22 07:58:46  marco
   * Removed some ifdefs, and replaced them with ifdefs. Some netbsd fixes.

  Revision 1.1.2.2  2001/09/14 15:30:18  pierre
   * use `cat link.res` hack to overcome m68k netbsd ld limitations

  Revision 1.1.2.1  2001/02/26 19:47:39  peter
    * moved target units to targets/ subdir

  Revision 1.1.2.3  2000/12/30 22:57:26  peter
    * export with case provided in exports section

  Revision 1.1.2.2  2000/09/24 21:40:19  peter
    * error messages updated
    * if messages not available in message file fallback to the internal
      messages
    * message prefixes (like Note:) can now also be set in the msg file

  Revision 1.1.2.1  2000/09/18 10:59:56  marco
   * Renamed t_freebsd to t_fbsd because of 8.3 convention

  Revision 1.1.2.1  2000/09/13 14:08:28  marco
   Initial FreeBSD version

  Revision 1.1.2.1  2000/09/10 16:11:59  marco
  Dynamic linker name is always empty for BSD

  Revision 1.1  2000/07/13 06:29:57  michael
  + Initial import

  Revision 1.15  2000/07/08 20:43:38  peter
    * findobjectfile gets extra arg with directory where the unit is found
      and the .o should be looked first

  Revision 1.14  2000/03/21 21:36:52  peter
    * only include crtbegin when linking to libc

  Revision 1.13  2000/03/12 08:24:03  daniel
    * Modification for new symtable

  Revision 1.12  2000/03/02 13:12:37  daniel
    * Removed a comment to fix gtk.

  Revision 1.11  2000/02/28 17:23:57  daniel
  * Current work of symtable integration committed. The symtable can be
    activated by defining 'newst', but doesn't compile yet. Changes in type
    checking and oop are completed. What is left is to write a new
    symtablestack and adapt the parser to use it.

  Revision 1.10  2000/02/27 14:46:04  peter
    * check for ld-so.2.0.* then no glibc21 is used, else glibc21 is used

  Revision 1.9  2000/02/09 10:35:48  peter
    * -Xt option to link staticly against c libs

  Revision 1.8  2000/01/11 09:52:07  peter
    * fixed placing of .sl directories
    * use -b again for base-file selection
    * fixed group writing for linux with smartlinking

  Revision 1.7  2000/01/09 00:55:51  pierre
    * GROUP of smartlink units put before the C libraries
      to allow for smartlinking code that uses C code.

  Revision 1.6  2000/01/07 01:14:42  peter
    * updated copyright to 2000

  Revision 1.5  1999/11/16 23:39:04  peter
    * use outputexedir for link.res location

  Revision 1.4  1999/11/12 11:03:50  peter
    * searchpaths changed to stringqueue object

  Revision 1.3  1999/11/05 13:15:00  florian
    * some fixes to get the new cg compiling again

  Revision 1.2  1999/11/04 10:55:31  peter
    * TSearchPathString for the string type of the searchpaths, which is
      ansistring under FPC/Delphi

  Revision 1.1  1999/10/21 14:29:38  peter
    * redesigned linker object
    + library support for linux (only procedures can be exported)

}
