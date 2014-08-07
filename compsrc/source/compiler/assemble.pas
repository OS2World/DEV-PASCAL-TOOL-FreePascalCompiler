{
    $Id: assemble.pas,v 1.1.2.22 2002/11/11 12:14:43 carl Exp $
    Copyright (c) 1998-2000 by Peter Vreman

    This unit handles the assemblerfile write and assembler calls of FPC

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

unit assemble;

interface

uses
{$ifdef Delphi}
  dmisc,
{$else Delphi}
  dos,
{$endif Delphi}
  cobjects,globtype,globals,aasm;

const
{$ifdef tp}
  AsmOutSize=1024;
{$else}
  AsmOutSize=32768;
{$endif}

type
  PAsmList=^TAsmList;
  TAsmList=object
  private
    procedure CreateSmartLinkPath(const s:string);
  public
  {filenames}
    path     : pathstr;
    name     : namestr;
    asmfile,         { current .s and .o file }
    objfile,
    as_bin   : string;
    SmartAsm : boolean;
    smarthcount : longint;
    place    : TCutPlace; { special 'end' file for import dir ? }
  {outfile}
    AsmSize,
    AsmStartSize,
    outcnt   : longint;
    outbuf   : array[0..AsmOutSize-1] of char;
    outfile  : file;
    Constructor Init(smart:boolean);
    Destructor Done;
    Function  FindAssembler:string;
    Function  CallAssembler(const command,para:string):Boolean;
    Function  DoAssemble:boolean;virtual;
    Procedure RemoveAsm;
    procedure NextSmartName;
    Procedure AsmFlush;
    Procedure AsmClear;
    Procedure AsmWrite(const s:string);
    Procedure AsmWritePChar(p:pchar);
    Procedure AsmWriteLn(const s:string);
    Procedure AsmLn;
    procedure AsmCreate(Aplace:tcutplace);
    procedure AsmClose;
    procedure Synchronize;
    procedure WriteTree(p:paasmoutput);virtual;
    procedure WriteAsmList;virtual;
  end;

var
  SmartLinkFilesCnt : longint;

Procedure GenerateAsm(smart:boolean);
Procedure OnlyAsm;


Implementation


uses
  script,files,systems,verbose
{$ifdef hasunix}
  {$ifdef VER1_0} ,linux {$else} ,unix {$endif}
{$endif}
  ,strings
{$ifdef i386}
  {$ifndef NoAg386Bin}
    ,ag386bin
  {$endif}
  {$ifndef NoAg386Att}
    ,aggas
  {$endif NoAg386Att}
  {$ifndef NoAg386Nsm}
    ,ag386nsm
  {$endif NoAg386Nsm}
  {$ifndef NoAg386Int}
    ,ag386int
  {$endif NoAg386Int}
  {$ifdef Ag386Cof}
    ,ag386cof
  {$endif Ag386Cof}
{$endif}
{$ifdef m68k}
  {$ifndef NoAg68kGas}
    ,aggas
  {$endif NoAg68kGas}
  {$ifndef NoAg68kMot}
    ,ag68kmot
  {$endif NoAg68kMot}
  {$ifndef NoAg68kMit}
    ,ag68kmit
  {$endif NoAg68kMit}
  {$ifndef NoAg68kMpw}
    ,ag68kmpw
  {$endif NoAg68kMpw}
{$endif}
  ;


{*****************************************************************************
                                  TAsmList
*****************************************************************************}

Function DoPipe:boolean;
begin
  DoPipe:=(cs_asm_pipe in aktglobalswitches) and
          not(cs_asm_leave in aktglobalswitches)
{$ifdef i386}
          and (aktoutputformat=as_i386_as);
{$endif i386}
{$ifdef m68k}
          and ((aktoutputformat=as_m68k_as) or
               (aktoutputformat=as_m68k_asbsd));
{$endif m68k}
end;


const
  lastas  : byte=255;
var
  LastASBin : pathstr;
Function TAsmList.FindAssembler:string;
var
  asfound : boolean;
  UtilExe  : string;
begin
  asfound:=false;
  if cs_link_on_target in aktglobalswitches then
    begin
      { If linking on target, don't add any path PM }
      FindAssembler:=AddExtension(target_asm.asmbin,target_os.exeext);
      exit;
    end
  else
    UtilExe:=AddExtension(target_asm.asmbin,source_os.exeext);
  if lastas<>ord(target_asm.id) then
   begin
     lastas:=ord(target_asm.id);
     { is an assembler passed ? }
     if utilsdirectory<>'' then
       LastASBin:=FindFile(UtilExe,utilsdirectory,asfound)+UtilExe;
     if not AsFound then
       LastASBin:=FindExe(UtilExe,asfound);
     if (not asfound) and not(cs_asm_extern in aktglobalswitches) then
      begin
        Message1(exec_e_assembler_not_found,LastASBin);
        aktglobalswitches:=aktglobalswitches+[cs_asm_extern];
      end;
     if asfound then
      Message1(exec_t_using_assembler,LastASBin);
   end;
  FindAssembler:=LastASBin;
end;


Function TAsmList.CallAssembler(const command,para:string):Boolean;
begin
  callassembler:=true;
  if not(cs_asm_extern in aktglobalswitches) then
   begin
     swapvectors;
     exec(command,para);
     swapvectors;
     if (doserror<>0) then
      begin
        Message1(exec_e_cant_call_assembler,tostr(doserror));
        aktglobalswitches:=aktglobalswitches+[cs_asm_extern];
        callassembler:=false;
      end
     else
      if (dosexitcode<>0) then
       begin
        Message1(exec_e_error_while_assembling,tostr(dosexitcode));
        callassembler:=false;
       end;
   end
  else
   AsmRes^.AddAsmCommand(command,para,name);
end;


procedure TAsmList.RemoveAsm;
var
  g : file;
begin
  if cs_asm_leave in aktglobalswitches then
   exit;
  if cs_asm_extern in aktglobalswitches then
   AsmRes^.AddDeleteCommand(AsmFile)
  else
   begin
     assign(g,AsmFile);
     {$I-}
      erase(g);
     {$I+}
     if ioresult<>0 then;
   end;
end;


Function TAsmList.DoAssemble:boolean;
var
  s : string;
begin
  DoAssemble:=true;
  if DoPipe then
   exit;
  if not(cs_asm_extern in aktglobalswitches) then
   begin
     if SmartAsm then
      begin
        if (SmartLinkFilesCnt<=1) then
         Message1(exec_i_assembling_smart,name);
      end
     else
     Message1(exec_i_assembling,name);
   end;
  s:=target_asm.asmcmd;
{$ifdef m68k}
  if aktoptprocessor = MC68020 then
    s:='-m68020 '+s
  else
    s:='-m68000 '+s;
{$endif}
  if (cs_link_on_target in aktglobalswitches) then
    begin
      Replace(s,'$ASM',ScriptFixFileName(AsmFile));
      Replace(s,'$OBJ',ScriptFixFileName(ObjFile));
    end
  else
    begin
      Replace(s,'$ASM',AsmFile);
      Replace(s,'$OBJ',ObjFile);
    end;
  if CallAssembler(FindAssembler,s) then
   RemoveAsm
  else
   begin
      DoAssemble:=false;
      GenerateError;
   end;
end;


procedure TAsmList.NextSmartName;
var
  s : string;
begin
  inc(SmartLinkFilesCnt);
  if SmartLinkFilesCnt>999999 then
   Message(asmw_f_too_many_asm_files);
  case place of
    cut_begin :
      begin
        inc(smarthcount);
        s:=current_module^.asmprefix^+tostr(smarthcount)+'h';
      end;
    cut_normal :
      s:=current_module^.asmprefix^+tostr(smarthcount)+'s';
    cut_end :
      s:=current_module^.asmprefix^+tostr(smarthcount)+'t';
  end;
  AsmFile:=Path+FixFileName(s+tostr(SmartLinkFilesCnt)+target_info.asmext);
  ObjFile:=Path+FixFileName(s+tostr(SmartLinkFilesCnt)+target_info.objext);
  { insert in container so it can be cleared after the linking }
  SmartLinkOFiles.Insert(Objfile);
end;


{*****************************************************************************
                       TAsmList AsmFile Writing
*****************************************************************************}

Procedure TAsmList.AsmFlush;
begin
  if outcnt>0 then
   begin
     BlockWrite(outfile,outbuf,outcnt);
     outcnt:=0;
   end;
end;


Procedure TAsmList.AsmClear;
begin
  outcnt:=0;
end;


Procedure TAsmList.AsmWrite(const s:string);
begin
  if OutCnt+length(s)>=AsmOutSize then
   AsmFlush;
  Move(s[1],OutBuf[OutCnt],length(s));
  inc(OutCnt,length(s));
  inc(AsmSize,length(s));
end;


Procedure TAsmList.AsmWriteLn(const s:string);
begin
  AsmWrite(s);
  AsmLn;
end;


Procedure TAsmList.AsmWritePChar(p:pchar);
var
  i,j : longint;
begin
  i:=StrLen(p);
  j:=i;
  while j>0 do
   begin
     i:=min(j,AsmOutSize);
     if OutCnt+i>=AsmOutSize then
      AsmFlush;
     Move(p[0],OutBuf[OutCnt],i);
     inc(OutCnt,i);
     inc(AsmSize,i);
     dec(j,i);
     p:=pchar(@p[i]);
   end;
end;


Procedure TAsmList.AsmLn;
begin
  if OutCnt>=AsmOutSize-2 then
   AsmFlush;
  OutBuf[OutCnt]:=target_os.newline[1];
  inc(OutCnt);
  inc(AsmSize);
  if length(target_os.newline)>1 then
   begin
     OutBuf[OutCnt]:=target_os.newline[2];
     inc(OutCnt);
     inc(AsmSize);
   end;
end;


procedure TAsmList.AsmCreate(Aplace:tcutplace);
begin
  place:=Aplace;
  if SmartAsm then
   NextSmartName;
{$ifdef hasunix}
  if DoPipe then
   begin
     Message1(exec_i_assembling_pipe,asmfile);
     POpen(outfile,'as -o '+objfile,'W');
   end
  else
{$endif}
   begin
     Assign(outfile,asmfile);
     {$I-}
      Rewrite(outfile,1);
     {$I+}
     if ioresult<>0 then
      Message1(exec_d_cant_create_asmfile,asmfile);
   end;
  outcnt:=0;
  AsmSize:=0;
  AsmStartSize:=0;
end;


procedure TAsmList.AsmClose;
var
  f : file;
  l : longint;
begin
  AsmFlush;
{$ifdef hasunix}
  if DoPipe then
   PClose(outfile)
  else
{$endif}
   begin
   {Touch Assembler time to ppu time is there is a ppufilename}
     if Assigned(current_module^.ppufilename) then
      begin
        Assign(f,current_module^.ppufilename^);
        {$I-}
         reset(f,1);
        {$I+}
        if ioresult=0 then
         begin
           getftime(f,l);
           close(f);
           reset(outfile,1);
           setftime(outfile,l);
         end;
      end;
     close(outfile);
   end;
end;


{Touch Assembler and object time to ppu time is there is a ppufilename}
procedure TAsmList.Synchronize;
begin
{Touch Assembler time to ppu time is there is a ppufilename}
  if Assigned(current_module^.ppufilename) then
   begin
     SynchronizeFileTime(current_module^.ppufilename^,asmfile);
     if not(cs_asm_extern in aktglobalswitches) then
       SynchronizeFileTime(current_module^.ppufilename^,objfile);
   end;
end;


procedure TAsmList.WriteTree(p:paasmoutput);
begin
end;


procedure TAsmList.WriteAsmList;
begin
end;


procedure TAsmList.CreateSmartLinkPath(const s:string);
var
  dir : searchrec;
begin
  if PathExists(s) then
   begin
     { the path exists, now we clean only all the .o and .s files }
     { .o files }
     findfirst(s+dirsep+'*'+target_info.objext,anyfile,dir);
     while (doserror=0) do
      begin
        RemoveFile(s+dirsep+dir.name);
        findnext(dir);
      end;
{$ifdef fpc}
     findclose(dir);
{$endif}
     { .s files }
     findfirst(s+dirsep+'*'+target_info.asmext,anyfile,dir);
     while (doserror=0) do
      begin
        RemoveFile(s+dirsep+dir.name);
        findnext(dir);
      end;
{$ifdef fpc}
     findclose(dir);
{$endif}
   end
  else
   begin
     {$I-}
      mkdir(s);
     {$I+}
     if ioresult<>0 then;
   end;
end;


Constructor TAsmList.Init(smart:boolean);
begin
{ load start values }
  asmfile:=current_module^.asmfilename^;
  objfile:=current_module^.objfilename^;
  name:=FixFileName(current_module^.modulename^);
  OutCnt:=0;
  SmartLinkFilesCnt:=0;
  SmartLinkOFiles.Clear;
  place:=cut_normal;
  SmartAsm:=smart;
  SmartHCount:=0;
{ Which path will be used ? }
  if SmartAsm then
   begin
     path:=current_module^.outputpath^+FixFileName(current_module^.modulename^)+target_info.smartext;
     CreateSmartLinkPath(path);
     path:=FixPath(path,false);
   end
  else
   path:=current_module^.outputpath^;
end;


Destructor TAsmList.Done;
begin
end;


{*****************************************************************************
                     Generate Assembler Files Main Procedure
*****************************************************************************}

Procedure GenerateAsm(smart:boolean);
var
  a : PAsmList;
{$ifdef i386}
  {$ifndef NoAg386Bin}
    b : Pi386binasmlist;
  {$endif}
{$endif}
begin
  case aktoutputformat of
     as_none : ;
{$ifdef i386}
  {$ifndef NoAg386Bin}
     as_i386_dbg,
     as_i386_coff,
     as_i386_pecoff :
       begin
         case aktoutputformat of
           as_i386_dbg :
             b:=new(pi386binasmlist,Init(og_dbg,smart));
           as_i386_coff :
             b:=new(pi386binasmlist,Init(og_coff,smart));
           as_i386_pecoff :
             b:=new(pi386binasmlist,Init(og_pecoff,smart));
         end;
         b^.WriteBin;
         dispose(b,done);
         if assigned(current_module^.ppufilename) then
          begin
            if smart then
              SynchronizeFileTime(current_module^.ppufilename^,current_module^.staticlibfilename^)
            else
              SynchronizeFileTime(current_module^.ppufilename^,current_module^.objfilename^);
          end;
         exit;
       end;
  {$endif NoAg386Bin}
  {$ifndef NoAg386Att}
     as_i386_as,
     as_i386_as_aout,
     as_i386_asw :
       a:=new(pi386attasmlist,Init(smart));
  {$endif NoAg386Att}
  {$ifndef NoAg386Nsm}
     as_i386_nasmcoff,
     as_i386_nasmwin32,
     as_i386_nasmelf,
     as_i386_nasmobj :
       a:=new(pi386nasmasmlist,Init(smart));
  {$endif NoAg386Nsm}
  {$ifndef NoAg386Int}
     as_i386_masm,
     as_i386_tasm :
       a:=new(pi386intasmlist,Init(smart));
  {$endif NoAg386Int}
{$endif}
{$ifdef m68k}
  {$ifndef NoAg68kGas}
     as_m68k_palm,
     as_m68k_as,
     as_m68k_gas :
       a:=new(pm68kgasasmlist,Init(smart));
  {$endif NoAg86KGas}
  {$ifndef NoAg68kMot}
     as_m68k_mot :
       a:=new(pm68kmotasmlist,Init(smart));
  {$endif NoAg86kMot}
  {$ifndef NoAg68kMit}
     as_m68k_asbsd,
     as_m68k_mit :
       a:=new(pm68kmitasmlist,Init(smart));
  {$endif NoAg86KMot}
  {$ifndef NoAg68kMpw}
     as_m68k_mpw :
       a:=new(pm68kmpwasmlist,Init(smart));
  {$endif NoAg68kMpw}
{$endif}
  else
{$ifdef TP}
    exit;
{$else}
    Message(asmw_f_assembler_output_not_supported);
{$endif}
  end;
  a^.AsmCreate(cut_normal);
  a^.WriteAsmList;
  a^.AsmClose;
  a^.DoAssemble;
  a^.synchronize;
  dispose(a,Done);
end;


Procedure OnlyAsm;
var
  a : PAsmList;
begin
  a:=new(pasmlist,Init(false));
  a^.DoAssemble;
  dispose(a,Done);
end;

end.
{
  $Log: assemble.pas,v $
  Revision 1.1.2.22  2002/11/11 12:14:43  carl
    - remove unused defines

  Revision 1.1.2.21  2002/09/21 14:20:28  carl
    * default CPU is now 68020+ or higher
    * no longer limited in local stack spacr for 68020+ cpu
    * -m68000 / -m68020 option when assembler is called

  Revision 1.1.2.20  2002/09/16 21:42:34  pierre
   * don't disable m68k mpw assembler generator

  Revision 1.1.2.19  2002/09/16 21:09:37  pierre
   * don't disable motorola assembler generator anymore

  Revision 1.1.2.18  2002/07/31 10:49:26  marco
   * hasunix patches

  Revision 1.1.2.17  2002/07/14 13:17:14  carl
    - revert to old version because it no longer compiles on Be/QNX/Sunos

  Revision 1.1.2.15  2001/09/13 23:03:22  pierre
   + m68k mit assembler output updated and used for netbsd target

  Revision 1.1.2.14  2001/09/05 10:32:02  pierre
     * i386 netbsd uses normal as_i386_as, error was due to
     wrong configuration in cross compilation of as and ld
     as_i386_asbsd removed

  Revision 1.1.2.13  2001/09/03 16:19:08  pierre
   * implementation of -sh and -st option, for linking at host or target machine

  Revision 1.1.2.12  2001/08/29 11:51:28  pierre
   + as_i386_asbsd for Netbsd that needs 'L' local prefix

  Revision 1.1.2.11  2001/08/17 16:16:39  florian
    + support for PalmOS added

  Revision 1.1.2.10  2001/08/07 18:39:14  peter
    * changed warnings to errors for failed tools

  Revision 1.1.2.9  2001/07/25 21:55:25  pierre
   * disable Mit Mot and Mpw m68k assemblers by default

  Revision 1.1.2.8  2001/07/23 06:58:56  pierre
   + AsmRes made pointer to support amiga target

  Revision 1.1.2.7  2001/06/15 12:13:23  pierre
   * make  DoAssemble method virtual needed for masm

  Revision 1.1.2.6  2001/03/13 21:00:59  peter
    * fixes to get it compiled with 1.1 (linux -> unix)

  Revision 1.1.2.5  2001/02/26 08:07:21  michael
  * bug correction: pipes must be closed by pclose (not close);
    There was too many not closed processes under Linux before patch.
    Test this by making a compiler under Linux with command
      OPT="-P" make
    and check a list of processes in another shell with
      ps -xa

  Revision 1.1.2.4  2001/02/26 03:05:35  carl
  + renamed some unit names

  Revision 1.1.2.3  2001/02/20 16:49:17  pierre
   * allow mams output again

  Revision 1.1.2.2  2001/02/09 23:06:49  peter
    * fixed uninited var

  Revision 1.1.2.1  2001/01/12 19:19:28  peter
    * fixed searching for utils

  Revision 1.1  2000/07/13 06:29:44  michael
  + Initial import

  Revision 1.65  2000/06/01 19:11:19  peter
    * added ifdef fpc around findclose

  Revision 1.64  2000/06/01 13:02:45  peter
    * clean .o and .s from smartlinkpath when starting the writer

  Revision 1.63  2000/04/04 15:05:03  pierre
   + accept nasmwin32 output

  Revision 1.62  2000/02/24 18:41:38  peter
    * removed warnings/notes

  Revision 1.61  2000/02/09 13:22:45  peter
    * log truncated

  Revision 1.60  2000/01/11 09:52:06  peter
    * fixed placing of .sl directories
    * use -b again for base-file selection
    * fixed group writing for linux with smartlinking

  Revision 1.59  2000/01/07 01:14:19  peter
    * updated copyright to 2000

  Revision 1.58  1999/11/12 11:03:49  peter
    * searchpaths changed to stringqueue object

  Revision 1.57  1999/11/08 10:37:12  peter
    * filename fixes for win32 imports for units with multiple needed dll's

  Revision 1.56  1999/11/06 14:34:17  peter
    * truncated log to 20 revs

  Revision 1.55  1999/11/02 15:06:57  peter
    * import library fixes for win32
    * alignment works again

  Revision 1.54  1999/09/16 11:34:44  pierre
   * typo correction

  Revision 1.53  1999/09/02 18:47:44  daniel
    * Could not compile with TP, some arrays moved to heap
    * NOAG386BIN default for TP
    * AG386* files were not compatible with TP, fixed.

}
