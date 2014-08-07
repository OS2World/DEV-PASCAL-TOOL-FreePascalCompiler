{
    $Id: link.pas,v 1.1.2.32 2003/05/06 18:02:04 peter Exp $
    Copyright (c) 1998-2000 by Peter Vreman

    This unit handles the linker and binder calls for programs and
    libraries

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
Unit link;

Interface

{ Needed for LFN support in path to the executable }
{$ifdef GO32V2}
  { define ALWAYSSHELL, obsolete as go32v2 Dos.Exec
    now handles LFN names and converts them to SFN PM }
{$endif}

uses cobjects,files;

Type
    TLinkerInfo=record
      ExeCmd,
      DllCmd        : array[1..3] of string;
      ResName       : string[12];
      ExtraOptions  : string;
      DynamicLinker : string[100];
    end;

    PLinker=^TLinker;
    TLinker = Object
    public
       Info            : TLinkerInfo;
       ObjectFiles,
       SharedLibFiles,
       StaticLibFiles  : TStringContainer;
     { Methods }
       Constructor Init;
       Destructor Done;
       procedure AddModuleFiles(hp:pmodule);
       function  FindObjectFile(s : string;const unitpath:string) : string;
       function  FindLibraryFile(s:string;const ext:string;var found : boolean) : string;
       Procedure AddObject(const S,unitpath : String);
       Procedure AddStaticLibrary(const S : String);
       Procedure AddSharedLibrary(S : String);
       Function  FindUtil(const s:string):String;
       Function  DoExec(const command,para:string;showinfo,useshell:boolean):boolean;
       {$IFDEF DOTARGETOPENBSDAOUT}
       Function  DoExec(const command:string;para:pchar;showinfo:boolean):boolean;
       {$endif}
     { Virtuals }
       procedure SetDefaultInfo;virtual;
       Function  MakeExecutable:boolean;virtual;
       Function  MakeSharedLibrary:boolean;virtual;
       Function  MakeStaticLibrary:boolean;virtual;
     end;

Var
  Linker : PLinker;

procedure InitLinker;
procedure DoneLinker;


Implementation

uses
{$ifdef Delphi}
  dmisc,
{$else Delphi}
  dos,
{$endif Delphi}
  globtype,systems,
  script,globals,verbose,ppu
{$ifdef i386}
  {$ifndef NOTARGETLINUX}
    ,t_linux
  {$endif}
  {$ifndef NOTARGETFREEBSD}
    ,t_bsd
  {$endif}
  {$ifdef DOTARGETOPENBSDAOUT}
   ,t_bsdaou
  {$endif}
  {$ifndef NOTARGETSUNOS}
    ,t_sunos
  {$endif}
  {$ifndef NOTARGETOS2}
    ,t_os2
  {$endif}
  {$ifndef NOTARGETWIN32}
    ,t_win32
  {$endif}
  {$ifndef NOTARGETGO32V1}
    ,t_go32v1
  {$endif}
  {$ifndef NOTARGETGO32V2}
    ,t_go32v2
  {$endif}
  {$ifndef NOTARGETBEOS}
    ,t_beos
  {$endif}
  {$ifndef NOTARGETQNX}
    ,t_qnx
  {$endif}
{$endif}
{$ifdef m68k}
  {$ifndef NOTARGETLINUX}
    ,t_linux
  {$endif}
  {$ifndef NOTARGETPALMOS}
    ,t_palmos
  {$endif}
  {$ifndef NOTARGETAMIGA}
    ,t_amiga
  {$endif}
  {$ifndef NOTARGETATARI}
    ,t_atari
  {$endif}
  {$ifndef NOTARGETFREEBSD}
    ,t_bsd
  {$endif}
{$endif}
{$ifdef powerpc}
  {$ifndef NOTARGETLINUX}
    ,t_linux
  {$endif}
{$endif}
{$ifdef alpha}
  {$ifndef NOTARGETLINUX}
    ,t_linux
  {$endif}
{$endif}
{$ifdef DOTARGETOPENBSDAOUT} // more openbsd workaround
 ,linux
{$endif}
  ,gendef
  ;

{*****************************************************************************
                                   TLINKER
*****************************************************************************}

Constructor TLinker.Init;
begin
  ObjectFiles.Init_no_double;
  SharedLibFiles.Init_no_double;
  StaticLibFiles.Init_no_double;
{ set generic defaults }
  FillChar(Info,sizeof(Info),0);
  Info.ResName:='link.res';
{ set the linker specific defaults }
  SetDefaultInfo;
{ Allow Parameter overrides for linker info }
  with Info do
   begin
     if ParaLinkOptions<>'' then
      ExtraOptions:=ParaLinkOptions;
     if ParaDynamicLinker<>'' then
      DynamicLinker:=ParaDynamicLinker;
   end;
end;


Destructor TLinker.Done;
begin
  ObjectFiles.Done;
  SharedLibFiles.Done;
  StaticLibFiles.Done;
end;


Procedure TLinker.SetDefaultInfo;
begin
end;


procedure TLinker.AddModuleFiles(hp:pmodule);
var
  mask : longint;
begin
  with hp^ do
   begin
   { link unit files }
     if (flags and uf_no_link)=0 then
      begin
        { create mask which unit files need linking }
        mask:=link_allways;
        { static linking ? }
        if (cs_link_static in aktglobalswitches) then
         begin
           if (flags and uf_static_linked)=0 then
            begin
              { if smart not avail then try static linking }
              if (flags and uf_smart_linked)<>0 then
               begin
                 Message1(exec_t_unit_not_static_linkable_switch_to_smart,modulename^);
                 mask:=mask or link_smart;
               end
              else
               Message1(exec_e_unit_not_smart_or_static_linkable,modulename^);
            end
           else
             mask:=mask or link_static;
         end;
        { smart linking ? }
        if (cs_link_smart in aktglobalswitches) then
         begin
           if (flags and uf_smart_linked)=0 then
            begin
              { if smart not avail then try static linking }
              if (flags and uf_static_linked)<>0 then
               begin
                 Message1(exec_t_unit_not_smart_linkable_switch_to_static,modulename^);
                 mask:=mask or link_static;
               end
              else
               Message1(exec_e_unit_not_smart_or_static_linkable,modulename^);
            end
           else
            mask:=mask or link_smart;
         end;
        { shared linking }
        if (cs_link_shared in aktglobalswitches) then
         begin
           if (flags and uf_shared_linked)=0 then
            begin
              { if shared not avail then try static linking }
              if (flags and uf_static_linked)<>0 then
               begin
                 Message1(exec_t_unit_not_shared_linkable_switch_to_static,modulename^);
                 mask:=mask or link_static;
               end
              else
               Message1(exec_e_unit_not_shared_or_static_linkable,modulename^);
            end
           else
            mask:=mask or link_shared;
         end;
        { unit files }
        while not linkunitofiles.empty do
         AddObject(linkunitofiles.getusemask(mask),path^);
        while not linkunitstaticlibs.empty do
         AddStaticLibrary(linkunitstaticlibs.getusemask(mask));
        while not linkunitsharedlibs.empty do
         AddSharedLibrary(linkunitsharedlibs.getusemask(mask));
      end;
   { Other needed .o and libs, specified using $L,$LINKLIB,external }
     mask:=link_allways;
     while not linkotherofiles.empty do
      AddObject(linkotherofiles.Getusemask(mask),path^);
     while not linkotherstaticlibs.empty do
      AddStaticLibrary(linkotherstaticlibs.Getusemask(mask));
     while not linkothersharedlibs.empty do
      AddSharedLibrary(linkothersharedlibs.Getusemask(mask));
   end;
end;


Function TLinker.FindUtil(const s:string):string;
var
  Found    : boolean;
  FoundBin : string;
  UtilExe  : string;
begin
  if cs_link_on_target in aktglobalswitches then
    begin
      { If linking on target, don't add any path PM }
      FindUtil:=AddExtension(s,target_os.exeext);
      exit;
    end
  else
  UtilExe:=AddExtension(s,source_os.exeext);
  FoundBin:='';
  Found:=false;
  if utilsdirectory<>'' then
   FoundBin:=FindFile(utilexe,utilsdirectory,Found)+utilexe;
  if (not Found) then
   FoundBin:=FindExe(utilexe,Found);
  if (not Found) and not(cs_link_extern in aktglobalswitches) then
   begin
     Message1(exec_e_util_not_found,utilexe);
     aktglobalswitches:=aktglobalswitches+[cs_link_extern];
   end;
  if (FoundBin<>'') then
   Message1(exec_t_using_util,FoundBin);
  FindUtil:=FoundBin;
end;


{ searches an object file }
function TLinker.FindObjectFile(s:string;const unitpath:string) : string;
var
  found : boolean;
  s1 : string;
begin
  findobjectfile:='';
  if s='' then
   exit;
  if pos('.',s)=0 then
   s:=s+target_info.objext
  else
   s:=s;
  s1:=FixFileName(s);
  if FileExists(s1) then
   begin
     Findobjectfile:=ScriptFixFileName(s);
     exit;
   end;
  { find object file
     1. specified unit path (if specified)
     2. cwd
     3. unit search path
     4. local object path
     5. global object path
     6. exepath }
  found:=false;
  if unitpath<>'' then
   findobjectfile:=ScriptFixFileName(FindFile(s,unitpath,found)+s);
  if (not found) then
   findobjectfile:=ScriptFixFileName(FindFile(s,'.'+DirSep,found)+s);
  if (not found) then
   findobjectfile:=ScriptFixFileName(UnitSearchPath.FindFile(s,found)+s);
  if (not found) then
   findobjectfile:=ScriptFixFileName(current_module^.localobjectsearchpath.FindFile(s,found)+s);
  if (not found) then
   findobjectfile:=ScriptFixFileName(objectsearchpath.FindFile(s,found)+s);
  if (not found) then
   findobjectfile:=ScriptFixFileName(FindFile(s,exepath,found)+s);
  if not(cs_link_extern in aktglobalswitches) and (not found) then
   Message1(exec_w_objfile_not_found,s);
end;


{ searches an library file }
function TLinker.FindLibraryFile(s:string;const ext:string;var found : boolean) : string;
begin
  found:=false;
  findlibraryfile:='';
  if s='' then
   exit;
  if pos('.',s)=0 then
   s:=s+ext;
  if FileExists(s) then
   begin
     found:=true;
     FindLibraryFile:=ScriptFixFileName(s);
     exit;
   end;
  { find libary
     1. cwd
     2. local libary dir
     3. global libary dir
     4. exe path of the compiler }
  found:=false;
  findlibraryfile:=ScriptFixFileName(FindFile(s,'.'+DirSep,found)+s);
  if (not found) and (current_module^.outputpath^<>'') then
   findlibraryfile:=ScriptFixFileName(FindFile(s,current_module^.outputpath^,found)+s);
  if (not found) then
   findlibraryfile:=ScriptFixFileName(current_module^.locallibrarysearchpath.FindFile(s,found)+s);
  if (not found) then
   findlibraryfile:=ScriptFixFileName(librarysearchpath.FindFile(s,found)+s);
  if (not found) then
   findlibraryfile:=ScriptFixFileName(FindFile(s,exepath,found)+s);
end;


Procedure TLinker.AddObject(const S,unitpath : String);
begin
  ObjectFiles.Insert(FindObjectFile(s,unitpath));
end;


Procedure TLinker.AddSharedLibrary(S:String);
begin
  if s='' then
   exit;
{ remove prefix 'lib' }
  if Copy(s,1,length(target_os.sharedlibprefix))=target_os.sharedlibprefix then
   Delete(s,1,length(target_os.sharedlibprefix));
{ remove extension if any }
  if Copy(s,length(s)-length(target_os.sharedlibext)+1,length(target_os.sharedlibext))=target_os.sharedlibext then
   Delete(s,length(s)-length(target_os.sharedlibext)+1,length(target_os.sharedlibext)+1);
{ ready to be inserted }
  SharedLibFiles.Insert (S);
end;


Procedure TLinker.AddStaticLibrary(const S:String);
var
  ns : string;
  found : boolean;
begin
  if s='' then
   exit;
  ns:=FindLibraryFile(s,target_os.staticlibext,found);
  if not(cs_link_extern in aktglobalswitches) and (not found) then
   Message1(exec_w_libfile_not_found,s);
  StaticLibFiles.Insert(ns);
end;

{$ifdef DOTARGETOPENBSDAOUT} // OpenBSD 3.2 a.out workaround.
Procedure Execansi (Path: ansistring);
var
  LastDosExitCode,
  pid    : longint;

  // The Error-Checking in the previous Version failed, since halt($7F) gives an WaitPid-status of $7F00
Begin
  LastDosExitCode:=0;
  pid:=Fork;
  if pid=0 then
   begin
    Execl(Path);
    ExitProcess(127);
   end
  else
   if pid=-1 then         {Fork failed}
    begin
      DosError:=8;
      exit
    end;
{We're in the parent, let's wait.}
  LastDosExitCode:=WaitProcess(pid); // WaitPid and result-convert
  { perhaps one time give an better error }
  if (LastDosExitCode>=0) and (LastDosExitCode<>127) then DosError:=0 else DosError:=8;
End;

Function TLinker.DoExec(const command:String;para:pchar;showinfo:boolean):boolean;
{MvdV: to allow cmdlines larger than 255 chars for OpenBSD archaic linker}
begin
  Comment(V_Debug,command+' '+para);
  DoExec:=true;
  swapvectors;
  execansi(command+ansistring(para));
  swapvectors;
  if (doserror<>0) then
    begin
      Message(exec_e_cant_call_linker);
      aktglobalswitches:=aktglobalswitches+[cs_link_extern];
      DoExec:=false;
    end
  else
    if (dosexitcode<>0) then
     begin
       Message(exec_e_error_while_linking);
       aktglobalswitches:=aktglobalswitches+[cs_link_extern];
       DoExec:=false;
     end;

{ Update asmres when externmode is set }
  if cs_link_extern in aktglobalswitches then
   begin
     if showinfo then
       begin
         if DLLsource then
           AsmRes^.AddLinkCommand(Command,Para,current_module^.sharedlibfilename^)
         else
           AsmRes^.AddLinkCommand(Command,Para,current_module^.exefilename^);
       end
     else
      AsmRes^.AddLinkCommand(Command,Para,'');
   end;
end;
{$endif}

Function TLinker.DoExec(const command,para:string;showinfo,useshell:boolean):boolean;


begin
  Comment(V_Debug,command+' '+para);
  DoExec:=true;
  if not(cs_link_extern in aktglobalswitches) then
   begin
     swapvectors;
{$ifdef ALWAYSSHELL}
     shell(command+' '+para);
{$else}
     if useshell then
       begin
        if pos(' ',command)>0 then
          shell('"'+command+'" '+para)
        else
          shell(command+' '+para);
       end
     else
       begin
         exec(command,para);
       end;
{$endif}
     swapvectors;
     if (doserror<>0) then
      begin
         Message(exec_e_cant_call_linker);
         aktglobalswitches:=aktglobalswitches+[cs_link_extern];
         DoExec:=false;
      end
     else
      if (dosexitcode<>0) then
       begin
        Message(exec_e_error_while_linking);
        aktglobalswitches:=aktglobalswitches+[cs_link_extern];
        DoExec:=false;
       end;
   end;
{ Update asmres when externmode is set }
  if cs_link_extern in aktglobalswitches then
   begin
     if showinfo then
       begin
         if DLLsource then
           AsmRes^.AddLinkCommand(Command,Para,current_module^.sharedlibfilename^)
         else
           AsmRes^.AddLinkCommand(Command,Para,current_module^.exefilename^);
       end
     else
      AsmRes^.AddLinkCommand(Command,Para,'');
   end;
end;


function TLinker.MakeExecutable:boolean;
begin
  MakeExecutable:=false;
  Message(exec_e_exe_not_supported);
end;


Function TLinker.MakeSharedLibrary:boolean;
begin
  MakeSharedLibrary:=false;
  Message(exec_e_dll_not_supported);
end;


Function TLinker.MakeStaticLibrary:boolean;
var
  smartpath,
  cmdstr,
  files,
  binstr  : string;
  success : boolean;
begin
  MakeStaticLibrary:=false;
{ remove the library, to be sure that it is rewritten }
  RemoveFile(current_module^.staticlibfilename^);
{ Call AR }
  smartpath:=current_module^.outputpath^+FixPath(FixFileName(current_module^.modulename^)+target_info.smartext,false);
  SplitBinCmd(target_ar.arcmd,binstr,cmdstr);
  Replace(cmdstr,'$LIB',current_module^.staticlibfilename^);
  files:=FixFileName(smartpath+current_module^.asmprefix^+'*'+target_info.objext);
  Replace(files,'\','/');
  Replace(cmdstr,'$FILES',files);
  success:=DoExec(FindUtil(binstr),cmdstr,false,true);
{ Clean up }
  if not(cs_asm_leave in aktglobalswitches) then
   if not(cs_link_extern in aktglobalswitches) then
    begin
      while not SmartLinkOFiles.Empty do
       RemoveFile(SmartLinkOFiles.Get);
      RemoveDir(smartpath);
    end
   else
    begin
      AsmRes^.AddDeleteCommand(FixFileName(smartpath+current_module^.asmprefix^+'*'+target_info.objext));
      AsmRes^.AddDeleteDirCommand(smartpath);
    end;
  MakeStaticLibrary:=success;
end;


{*****************************************************************************
                                 Init/Done
*****************************************************************************}

procedure InitLinker;
begin
  case target_os.id of
{$ifdef i386}
  {$ifndef NOTARGETLINUX}
    os_i386_linux :
      linker:=new(plinkerlinux,Init);
  {$endif}
  {$ifndef NOTARGETFreeBSD}
    os_i386_OpenBSD: {$ifdef DOTARGETOPENBSDAOUT}
                              linker:=new(plinkerBSDaout,Init);
                     {$else}
                              linker:=new(plinkerBSD,Init);
                     {$endif}
    os_i386_FreeBSD,
    os_i386_NetBSD :
      linker:=new(plinkerBSD,Init);
  {$endif}
  {$ifndef NOTARGETSUNOS}
    os_i386_sunos :
      linker:=new(plinkersunos,Init);
  {$endif}
  {$ifndef NOTARGETWIN32}
    os_i386_Win32 :
      linker:=new(plinkerwin32,Init);
  {$endif}
  {$ifndef NOTARGETGO32V1}
    os_i386_Go32v1 :
      linker:=new(plinkergo32v1,Init);
  {$endif}
  {$ifndef NOTARGETGO32V2}
    os_i386_Go32v2 :
      linker:=new(plinkergo32v2,Init);
  {$endif}
  {$ifndef NOTARGETOS2}
    os_i386_os2 :
      linker:=new(plinkeros2,Init);
  {$endif}
  {$ifndef NOTARGETBEOS}
    os_i386_beos :
      linker:=new(plinkerbeos,Init);
  {$endif}
  {$ifndef NOTARGETQNX}
    os_i386_qnx :
      linker:=new(plinkerqnx,Init);
  {$endif}
{$endif i386}
{$ifdef m68k}
  {$ifndef NOTARGETPALMOS}
    os_m68k_palmos:
      linker:=new(plinkerPalmOS,Init);
  {$endif}
  {$ifndef NOTARGETAMIGA}
    os_m68k_amiga:
      linker:=new(plinkeramiga,Init);
  {$endif}
  {$ifndef NOTARGETATARI}
    os_m68k_atari:
      linker:=new(plinkeratari,Init);
  {$endif}
  {$ifndef NOTARGETLINUX}
    os_m68k_linux :
      linker:=new(plinkerlinux,Init);
  {$endif}
  {$ifndef NOTARGETFreeBSD}
    os_m68k_openbsd,
    os_m68k_NetBSD :
      linker:=new(plinkerBSD,Init);
  {$endif}
{$endif m68k}
{$ifdef alpha}
  {$ifndef NOTARGETLINUX}
    os_alpha_linux :
      linker:=new(plinkerlinux,Init);
  {$endif}
{$endif alpha}
{$ifdef powerpc}
  {$ifndef NOTARGETLINUX}
    os_powerpc_linux :
      linker:=new(plinkerlinux,Init);
  {$endif}
{$endif powerpc}
    else
      linker:=new(plinker,Init);
  end;
end;


procedure DoneLinker;
begin
  if assigned(linker) then
   dispose(linker,done);
end;


end.
{
  $Log: link.pas,v $
  Revision 1.1.2.32  2003/05/06 18:02:04  peter
    * also check for .a files in outputpath

  Revision 1.1.2.31  2003/02/24 20:48:52  carl
    * fix comment

  Revision 1.1.2.30  2003/02/20 22:35:59  marco
   * There was a conflict left. Was in wrong dir last time.

  Revision 1.1.2.29  2003/02/20 22:33:11  marco
   * Grr, accidental character

  Revision 1.1.2.28  2003/02/20 22:32:00  marco
   * small fi

  Revision 1.1.2.27  2003/02/20 22:25:42  marco
   * OpenBSD works with old linker but needs DOTARGETOPENBSDAOUT defined.

  Revision 1.1.2.26  2003/01/15 15:45:21  pierre
   * execmd and linkcmd can be more than 100 chars

  Revision 1.1.2.25  2002/12/18 17:00:54  pierre
   * don't use backslash for ar calling

  Revision 1.1.2.24  2002/11/02 11:50:31  carl
    * Assume BNU utils always accept /

  Revision 1.1.2.23  2002/10/18 21:55:38  carl
    * alignment of rtti information (m68k only)
    * bytes are also aligned when target = 68000
    + atari target

  Revision 1.1.2.22  2002/07/30 12:38:50  marco
   * OpenBSD support + t_fbsd -> t_bsd renaming

  Revision 1.1.2.21  2002/07/16 14:27:41  pierre
   * use " to specify command if command contains spaces in TLinker.DoExec

  Revision 1.1.2.20  2002/04/17 17:05:36  carl
  + QNX target

  Revision 1.1.2.19  2001/09/03 16:19:09  pierre
   * implementation of -sh and -st option, for linking at host or target machine

  Revision 1.1.2.18  2001/08/22 20:49:22  peter
    * regenerated

  Revision 1.1.2.17  2001/08/17 16:16:41  florian
    + support for PalmOS added

  Revision 1.1.2.16  2001/08/14 21:07:03  pierre
   ALWAYSSHELL conditional not defined anymore for go32v2

  Revision 1.1.2.15  2001/08/07 18:39:15  peter
    * changed warnings to errors for failed tools

  Revision 1.1.2.14  2001/08/07 15:55:31  pierre
   + new code for NetBSD, behaves like FreeBSD for now

  Revision 1.1.2.13  2001/07/25 13:38:16  pierre
   + include t_amiga

  Revision 1.1.2.12  2001/07/23 06:58:56  pierre
   + AsmRes made pointer to support amiga target

  Revision 1.1.2.11  2001/07/13 03:20:54  carl
  + BeOS support

  Revision 1.1.2.10  2001/06/03 15:18:39  peter
    * linux sharedlib fixes

  Revision 1.1.2.9  2001/02/26 19:46:47  peter
    * more solaris to sunos

  Revision 1.1.2.8  2001/02/08 22:06:43  florian
    * SUNOS to sunos changed

  Revision 1.1.2.7  2001/02/01 22:31:55  florian
    + SUNOS support for the compiler

  Revision 1.1.2.6  2001/01/12 19:19:28  peter
    * fixed searching for utils

  Revision 1.1.2.5  2000/09/24 21:40:18  peter
    * error messages updated
    * if messages not available in message file fallback to the internal
      messages
    * message prefixes (like Note:) can now also be set in the msg file

  Revision 1.1.2.4  2000/09/18 10:59:56  marco
   * Renamed t_freebsd to t_fbsd because of 8.3 convention

  Revision 1.1.2.3  2000/09/13 13:57:41  marco
   * FreeBSD compiler support

  Revision 1.1.2.2  2000/09/04 09:38:39  michael
  + Patch from peter

  Revision 1.1.2.1  2000/07/26 12:54:24  jonas
    * changed V_Hint's to V_Tried's (for attempts to smart/shared/static link)

  Revision 1.1  2000/07/13 06:29:52  michael
  + Initial import

  Revision 1.90  2000/07/08 20:43:37  peter
    * findobjectfile gets extra arg with directory where the unit is found
      and the .o should be looked first

  Revision 1.89  2000/06/28 03:34:06  hajny
    * little corrections for EMX resources

  Revision 1.88  2000/05/17 18:30:35  peter
    * removed wrong warning for library finding

  Revision 1.87  2000/05/03 16:11:57  peter
    * also allow smartlinking for main programs

  Revision 1.86  2000/04/14 11:16:10  pierre
    * partial linklib change
      I could not use Pavel's code because it broke the current way
      linklib is used, which is messy :(
    + add postw32 call if external linking on win32

  Revision 1.85  2000/02/28 17:23:57  daniel
  * Current work of symtable integration committed. The symtable can be
    activated by defining 'newst', but doesn't compile yet. Changes in type
    checking and oop are completed. What is left is to write a new
    symtablestack and adapt the parser to use it.

  Revision 1.84  2000/02/24 18:41:39  peter
    * removed warnings/notes

  Revision 1.83  2000/02/09 13:22:54  peter
    * log truncated

  Revision 1.82  2000/01/14 14:40:37  pierre
   * use ./ instead of . to look into startup dir

  Revision 1.81  2000/01/12 10:38:18  peter
    * smartlinking fixes for binary writer
    * release alignreg code and moved instruction writing align to cpuasm,
      but it doesn't use the specified register yet

  Revision 1.80  2000/01/11 09:52:06  peter
    * fixed placing of .sl directories
    * use -b again for base-file selection
    * fixed group writing for linux with smartlinking

  Revision 1.79  2000/01/07 01:14:27  peter
    * updated copyright to 2000

  Revision 1.78  1999/11/22 22:22:30  pierre
   * Give better info in script

  Revision 1.77  1999/11/12 11:03:50  peter
    * searchpaths changed to stringqueue object

  Revision 1.76  1999/11/06 14:34:21  peter
    * truncated log to 20 revs

  Revision 1.75  1999/10/26 12:25:04  peter
    * fixed os2 linker

  Revision 1.74  1999/10/21 14:29:34  peter
    * redesigned linker object
    + library support for linux (only procedures can be exported)

  Revision 1.72  1999/09/16 23:05:52  florian
    * m68k compiler is again compilable (only gas writer, no assembler reader)

  Revision 1.71  1999/09/16 11:34:56  pierre
   * typo correction

  Revision 1.70  1999/09/15 22:09:16  florian
    + rtti is now automatically generated for published classes, i.e.
      they are handled like an implicit property

  Revision 1.69  1999/09/15 20:24:56  daniel
  + Dw switch now does something.

  Revision 1.68  1999/08/18 17:05:53  florian
    + implemented initilizing of data for the new code generator
      so it should compile now simple programs

  Revision 1.67  1999/08/16 15:35:23  pierre
    * fix for DLL relocation problems
    * external bss vars had wrong stabs for pecoff
    + -WB11000000 to specify default image base, allows to
      load several DLLs with debugging info included
      (relocatable DLL are stripped because the relocation
       of the .Stab section is misplaced by ldw)

  Revision 1.66  1999/08/11 17:26:34  peter
    * tlinker object is now inherited for win32 and dos
    * postprocessexecutable is now a method of tlinker

  Revision 1.65  1999/08/10 12:51:16  pierre
    * bind_win32_dll removed (Relocsection used instead)
    * now relocsection is true by default ! (needs dlltool
      for DLL generation)

  Revision 1.64  1999/07/30 23:19:45  peter
    * fixed placing of dynamiclinker in link.res (should be the last after
      all other libraries)

  Revision 1.63  1999/07/29 01:31:39  peter
    * fixed shared library linking for glibc2 systems

  Revision 1.62  1999/07/27 11:05:51  peter
    * glibc 2.1.2 support

}
