{
    $Id: script.pas,v 1.1.2.15 2003/02/07 21:22:21 marco Exp $
    Copyright (c) 1998-2000 by Peter Vreman

    This unit handles the writing of script files

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
unit Script;
interface

uses
  CObjects;

type
  PScript=^TScript;
  TScript=object
    fn   : string[80];
    data : TStringQueue;
    executable : boolean;
    constructor Init(const s:string);
    constructor InitExec(const s:string);
    destructor Done;
    procedure AddStart(const s:string);
    procedure Add(const s:string);
    Function  Empty:boolean;
    procedure WriteToDisk;virtual;
  end;

  PAsmScript = ^TAsmScript;
  TAsmScript = Object (TScript)
    Constructor Init (Const ScriptName : String);
    Procedure AddAsmCommand (Const Command, Options,FileName : String);virtual;
    Procedure AddLinkCommand (Const Command, Options, FileName : String);virtual;
    Procedure AddDeleteCommand (Const FileName : String);virtual;
    Procedure AddDeleteDirCommand (Const FileName : String);virtual;
    Procedure WriteToDisk;virtual;
  end;

  PAsmScriptAmiga = ^TAsmScriptAmiga;
  TAsmScriptAmiga = Object (TAsmScript)
    Constructor Init (Const ScriptName : String);
    Procedure AddAsmCommand (Const Command, Options,FileName : String);virtual;
    Procedure AddLinkCommand (Const Command, Options, FileName : String);virtual;
    Procedure AddDeleteCommand (Const FileName : String);virtual;
    Procedure AddDeleteDirCommand (Const FileName : String);virtual;
    Procedure WriteToDisk;virtual;
  end;

  PAsmScriptLinux = ^TAsmScriptLinux;
  TAsmScriptLinux = Object (TAsmScript)
    Constructor Init (Const ScriptName : String);
    Procedure AddAsmCommand (Const Command, Options,FileName : String);virtual;
    Procedure AddLinkCommand (Const Command, Options, FileName : String);virtual;
    Procedure AddDeleteCommand (Const FileName : String);virtual;
    Procedure AddDeleteDirCommand (Const FileName : String);virtual;
    Procedure WriteToDisk;virtual;
  end;

  PLinkRes = ^TLinkRes;
  TLinkRes = Object (TScript)
    procedure Add(const s:string);
    procedure AddFileName(const s:string);
  end;

var
  AsmRes : PAsmScript;
  LinkRes : TLinkRes;

Procedure GenerateAsmRes(const st : string);
Function ScriptFixFileName(const s:string):string;


implementation

uses
{$ifdef hasunix}
  {$ifdef VER1_0} linux, {$else} unix, {$endif}
{$endif}
  globtype,globals,systems;

Function ScriptFixFileName(const s:string):string;
     var
       i      : longint;
       NoPath : boolean;
       Preserve_case_in_path : boolean;
       DirSep : char;
     begin
       if cs_link_on_target in aktglobalswitches then
         begin
           Preserve_case_in_path := target_os.files_case_relevent;
           DirSep:=target_os.DirSep;
         end
       else
         begin
           Preserve_case_in_path := source_os.files_case_relevent;
           DirSep:=source_os.DirSep;
         end;

       if Preserve_case_in_path then
         NoPath:=true;
       for i:=length(s) downto 1 do
        begin
          if Preserve_case_in_path then
            case s[i] of
              '/','\' : begin
                          ScriptFixFileName[i]:=DirSep;
                          NoPath:=false; {Skip lowercasing path: 'X11'<>'x11' }
                        end;
              'A'..'Z' : if NoPath then
                         ScriptFixFileName[i]:=char(byte(s[i])+32)
                        else
                         ScriptFixFileName[i]:=s[i];
            else
             ScriptFixFileName[i]:=s[i];
            end
         else
            case s[i] of
               '\','/' : ScriptFixFileName[i]:=DirSep;
              'A'..'Z' : ScriptFixFileName[i]:=char(byte(s[i])+32);
            else
              ScriptFixFileName[i]:=s[i];
            end;
        end;
       {$ifndef TP}
         {$ifopt H+}
           SetLength(ScriptFixFileName,length(s));
         {$else}
           ScriptFixFileName[0]:=s[0];
         {$endif}
       {$else}
         ScriptFixFileName[0]:=s[0];
       {$endif}
     end;


{****************************************************************************
                                  TScript
****************************************************************************}

constructor TScript.Init(const s:string);
begin
  fn:=FixFileName(s);
  executable:=false;
  data.Init;
end;


constructor TScript.InitExec(const s:string);
begin
  if cs_link_on_target in aktglobalswitches then
    fn:=FixFileName(s)+target_os.scriptext
  else
    fn:=FixFileName(s)+source_os.scriptext;
  executable:=true;
  data.Init;
end;


destructor TScript.Done;
begin
  data.done;
end;


procedure TScript.AddStart(const s:string);
begin
  data.Insert(s);
end;


procedure TScript.Add(const s:string);
begin
  data.Concat(s);
end;


Function TScript.Empty:boolean;
begin
  Empty:=Data.Empty;
end;


procedure TScript.WriteToDisk;
var
  t : Text;
begin
  Assign(t,fn);
  Rewrite(t);
  while not data.Empty do
   Writeln(t,data.Get);
  Close(t);
{$ifdef hasunix}
  if executable then
   ChMod(fn,493);
{$endif}
end;


{****************************************************************************
                                  Asm Response
****************************************************************************}

Constructor TAsmScript.Init (Const ScriptName : String);
begin
  Inherited InitExec(ScriptName);
end;


Procedure TAsmScript.AddAsmCommand (Const Command, Options,FileName : String);
begin
  if FileName<>'' then
   begin
     Add('SET THEFILE='+FileName);
     Add('echo Assembling %THEFILE%');
   end;
  if pos(' ',Command)>0 then
    Add('"'+command+'" '+Options)
  else
    Add(command+' '+Options);
  Add('if errorlevel 1 goto asmend');
end;


Procedure TasmScript.AddLinkCommand (Const Command, Options, FileName : String);
begin
  if FileName<>'' then
   begin
     Add('SET THEFILE='+FileName);
     Add('echo Linking %THEFILE%');
   end;
  if pos(' ',Command)>0 then
    Add('"'+command+'" '+Options)
  else
    Add (Command+' '+Options);
  Add('if errorlevel 1 goto linkend');
end;


Procedure TAsmScript.AddDeleteCommand (Const FileName : String);
begin
 Add('Del '+ScriptFixFileName(FileName));
end;

Procedure TAsmScript.AddDeleteDirCommand (Const FileName : String);
begin
 Add('Rmdir '+ScriptFixFileName(FileName));
end;

Procedure TAsmScript.WriteToDisk;
Begin
  AddStart('@echo off');
  Add('goto end');
  Add(':asmend');
  Add('echo An error occured while assembling %THEFILE%');
  Add('goto end');
  Add(':linkend');
  Add('echo An error occured while linking %THEFILE%');
  Add(':end');
  inherited WriteToDisk;
end;

{****************************************************************************
                                  Amiga Asm Response
****************************************************************************}

Constructor TAsmScriptAmiga.Init (Const ScriptName : String);
begin
  Inherited InitExec(ScriptName);
end;


Procedure TAsmScriptAmiga.AddAsmCommand (Const Command, Options,FileName : String);
begin
  if FileName<>'' then
   begin
     Add('SET THEFILE '+FileName);
     Add('echo Assembling $THEFILE');
   end;
  Add(command+' '+Options);
  { There is a problem here,
    as allways return with a non zero error value PM  }
  Add('if error');
  Add('why');
  Add('skip asmend');
  Add('endif');
end;


Procedure TAsmScriptAmiga.AddLinkCommand (Const Command, Options, FileName : String);
begin
  if FileName<>'' then
   begin
     Add('SET THEFILE '+FileName);
     Add('echo Linking $THEFILE');
   end;
  Add (Command+' '+Options);
  Add('if error');
  Add('skip linkend');
  Add('endif');
end;


Procedure TAsmScriptAmiga.AddDeleteCommand (Const FileName : String);
begin
 Add('Delete '+ScriptFixFileName(FileName));
end;

Procedure TAsmScriptAmiga.AddDeleteDirCommand (Const FileName : String);
begin
 Add('Delete '+ScriptFixFileName(FileName));
end;

Procedure TAsmScriptAmiga.WriteToDisk;
Begin
  Add('skip end');
  Add('lab asmend');
  Add('why');
  Add('echo An error occured while assembling $THEFILE');
  Add('skip end');
  Add('lab linkend');
  Add('why');
  Add('echo An error occured while linking $THEFILE');
  Add('lab end');
  TScript.WriteToDisk;
end;

{****************************************************************************
                                  Linux Asm Response
****************************************************************************}

Constructor TAsmScriptLinux.Init (Const ScriptName : String);
begin
  Inherited InitExec(ScriptName);
end;


Procedure TAsmScriptLinux.AddAsmCommand (Const Command, Options,FileName : String);
begin
  if FileName<>'' then
   Add('echo Assembling '+FileName);
  Add (Command+' '+Options);
  Add('if [ $? != 0 ]; then DoExitAsm '+FileName+'; fi');
end;


Procedure TAsmScriptLinux.AddLinkCommand (Const Command, Options, FileName : String);
begin
  if FileName<>'' then
   Add('echo Linking '+FileName);
  Add (Command+' '+Options);
  Add('if [ $? != 0 ]; then DoExitLink '+FileName+'; fi');
end;


Procedure TAsmScriptLinux.AddDeleteCommand (Const FileName : String);
begin
 Add('rm '+ScriptFixFileName(FileName));
end;

Procedure TAsmScriptLinux.AddDeleteDirCommand (Const FileName : String);
begin
 Add('rmdir '+ScriptFixFileName(FileName));
end;

Procedure TAsmScriptLinux.WriteToDisk;
Begin
  AddStart('{ echo "An error occurred while linking $1"; exit 1; }');
  AddStart('DoExitLink ()');
  AddStart('{ echo "An error occurred while assembling $1"; exit 1; }');
  AddStart('DoExitAsm ()');
  {$ifdef beos}
   AddStart('#!/boot/beos/bin/sh');
  {$else}
   AddStart('#!/bin/sh');
  {$endif}
  TScript.WriteToDisk;
end;

Procedure GenerateAsmRes(const st : string);
var
  choice : tos;
begin
  if cs_link_on_target in aktglobalswitches then
    choice := target_os.id
  else
    choice := source_os.id;
  { MAC type scripts are missing ... PM }
  { don't know about Atari ?? }
  if choice in [os_i386_go32v1,os_i386_go32v2,
                os_i386_win32,os_i386_os2] then
    AsmRes:=New(PAsmScript,Init(st))
  else if choice=os_m68k_amiga then
    AsmRes:=New(PAsmScriptAmiga,Init(st))
  else
    AsmRes:=New(PAsmScriptLinux,Init(st));
end;

{****************************************************************************
                                  Link Response
****************************************************************************}

procedure TLinkRes.Add(const s:string);
begin
  if s<>'' then
   inherited Add(s);
end;

procedure TLinkRes.AddFileName(const s:string);
begin
  if s<>'' then
   begin
     if not(s[1] in ['a'..'z','A'..'Z','/','\','.','"']) then
      begin
        if cs_link_on_target in aktglobalswitches then
          inherited Add('.'+target_os.DirSep+s)
        else
          inherited Add('.'+DirSep+s);
      end
     else
      inherited Add(s);
   end;
end;

end.
{
  $Log: script.pas,v $
  Revision 1.1.2.15  2003/02/07 21:22:21  marco
   * also fix in 1.0 (shell path)

  Revision 1.1.2.14  2003/02/05 22:46:26  marco
   * BeOS fix for scripts. sh is located in /boot/beos/bin it seems

  Revision 1.1.2.13  2003/01/06 11:24:27  pierre
   * don't prepend ./ to quoted file names

  Revision 1.1.2.12  2002/11/02 11:49:59  carl
    * Fix filenames according to script targets (cross-compilation bugfixes)

  Revision 1.1.2.11  2002/08/13 13:38:25  pierre
   * try to fix problem with exec directories containing spaces

  Revision 1.1.2.10  2002/07/31 10:49:27  marco
   * hasunix patches

  Revision 1.1.2.9  2002/07/14 14:47:30  carl
    - fix compilation problems (revert back to old files)

  Revision 1.1.2.7  2001/09/04 15:47:25  pierre
   * fix AddFileName method for cs_link_on_target

  Revision 1.1.2.6  2001/09/03 16:19:10  pierre
   * implementation of -sh and -st option, for linking at host or target machine

  Revision 1.1.2.5  2001/08/07 15:55:31  pierre
   + new code for NetBSD, behaves like FreeBSD for now

  Revision 1.1.2.4  2001/07/30 20:51:49  peter
    * use target script extension
    * fixed chmod call

  Revision 1.1.2.3  2001/07/23 07:35:27  pierre
   * fix soome amiga shell errors

  Revision 1.1.2.2  2001/07/23 06:58:56  pierre
   + AsmRes made pointer to support amiga target

  Revision 1.1.2.1  2001/03/13 21:01:00  peter
    * fixes to get it compiled with 1.1 (linux -> unix)

  Revision 1.1  2000/07/13 06:29:56  michael
  + Initial import

  Revision 1.6  2000/02/09 13:23:04  peter
    * log truncated

  Revision 1.5  2000/02/07 11:52:26  michael
  + Changed bash to sh

  Revision 1.4  2000/01/07 01:14:39  peter
    * updated copyright to 2000

  Revision 1.3  1999/10/21 14:29:37  peter
    * redesigned linker object
    + library support for linux (only procedures can be exported)

}
