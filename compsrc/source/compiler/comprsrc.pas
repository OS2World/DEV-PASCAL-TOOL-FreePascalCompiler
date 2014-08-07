{
    $Id: comprsrc.pas,v 1.1.2.7 2003/01/22 15:42:51 pierre Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    Handles the resource files handling

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
unit comprsrc;

interface

type
   presourcefile=^tresourcefile;
   tresourcefile=object
   private
      fname : string;
   public
      constructor Init(const fn:string);
      destructor Done;
      procedure  Compile;virtual;
   end;

procedure CompileResourceFiles;


implementation

uses
{$ifdef Delphi}
  dmisc,
{$else Delphi}
  dos,
{$endif Delphi}
  Systems,Globtype,Globals,Verbose,Files,
  Script;

{****************************************************************************
                              TRESOURCEFILE
****************************************************************************}

constructor tresourcefile.init(const fn:string);
begin
  fname:=fn;
end;


destructor tresourcefile.done;
begin
end;


procedure tresourcefile.compile;
var
  respath,
  srcfilepath : dirstr;
  n       : namestr;
  e       : extstr;
  s,
  resobj,
  resbin   : string;
  resfound,
  objused  : boolean;
begin
  resbin:='';
  if utilsdirectory<>'' then
   resbin:=FindFile(target_res.resbin+source_os.exeext,utilsdirectory,resfound)+target_res.resbin+source_os.exeext;
  if resbin='' then
   resbin:=FindExe(target_res.resbin,resfound);
  { get also the path to be searched for the windres.h }
  fsplit(resbin,respath,n,e);
  if (not resfound) and not(cs_link_extern in aktglobalswitches) then
   begin
     Message(exec_e_res_not_found);
     aktglobalswitches:=aktglobalswitches+[cs_link_extern];
   end;
  fsplit(current_module^.mainsource^,srcfilepath,n,e);
  if not path_absolute(fname) then
    fname:=srcfilepath+fname;
  resobj:=ForceExtension(fname,target_info.resobjext);
  s:=target_res.rescmd;
  ObjUsed:=(pos('$OBJ',s)>0);
  Replace(s,'$OBJ',resobj);
  Replace(s,'$RES',fname);
  Replace(s,'$INC',respath);
  if (target_info.target = target_i386_win32) and
     (srcfilepath<>'') then
    s:=s+' --include '+srcfilepath;
{ Exec the command }
  if not (cs_link_extern in aktglobalswitches) then
   begin
     Message1(exec_i_compilingresource,fname);
     swapvectors;
     exec(resbin,s);
     swapvectors;
     if (doserror<>0) then
      begin
        Message(exec_e_cant_call_linker);
        aktglobalswitches:=aktglobalswitches+[cs_link_extern];
      end
     else
      if (dosexitcode<>0) then
       begin
         Message(exec_e_error_while_linking);
         aktglobalswitches:=aktglobalswitches+[cs_link_extern];
       end;
    end;
  { Update asmres when externmode is set }
  if cs_link_extern in aktglobalswitches then
    AsmRes^.AddLinkCommand(resbin,s,'');
  if ObjUsed then
    current_module^.linkotherofiles.insert(resobj,link_allways);
end;


procedure CompileResourceFiles;
var
  hr : presourcefile;
begin
(* OS/2 (EMX) must be processed elsewhere (in the linking/binding stage). *)
  if target_info.target <> target_i386_os2 then
   While not Current_module^.ResourceFiles.Empty do
    begin
      case target_info.target of
        target_i386_win32,
        target_m68k_palmos:
          hr:=new(presourcefile,init(Current_module^.ResourceFiles.get));
        else
          Message(scan_e_resourcefiles_not_supported);
      end;
      hr^.compile;
      dispose(hr,done);
    end;
end;


end.
{
  $Log: comprsrc.pas,v $
  Revision 1.1.2.7  2003/01/22 15:42:51  pierre
   * fix problem with resource of files in other dir bug 2073

  Revision 1.1.2.6  2003/01/09 09:29:05  pierre
   * use pilrc for palmos resources

  Revision 1.1.2.5  2001/08/07 18:39:15  peter
    * changed warnings to errors for failed tools

  Revision 1.1.2.4  2001/07/23 08:51:49  pierre
     + AsmRes made pointer to support amiga target

  Revision 1.1.2.3  2001/03/13 21:00:59  peter
    * fixes to get it compiled with 1.1 (linux -> unix)

  Revision 1.1.2.2  2000/08/02 19:37:01  peter
    * fixed resbin and respath generation, previous patch was also wrong

  Revision 1.1.2.1  2000/07/31 20:05:33  peter
    * fixed windres.exewindres.exe filename generation, from mailinglist

  Revision 1.1  2000/07/13 06:29:48  michael
  + Initial import

  Revision 1.12  2000/06/25 19:08:28  hajny
    + $R support for OS/2 (EMX) added

  Revision 1.11  2000/06/23 20:11:05  peter
    * made resourcecompiling object so it can be inherited and replaced
      for other targets if needed

  Revision 1.10  2000/02/09 13:22:50  peter
    * log truncated

  Revision 1.9  2000/01/07 01:14:23  peter
    * updated copyright to 2000

  Revision 1.8  1999/12/01 12:42:32  peter
    * fixed bug 698
    * removed some notes about unused vars

  Revision 1.7  1999/11/12 11:03:50  peter
    * searchpaths changed to stringqueue object

}
