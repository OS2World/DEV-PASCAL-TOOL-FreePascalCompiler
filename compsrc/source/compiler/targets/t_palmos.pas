{
    $Id: t_palmos.pas,v 1.4.2.6 2003/01/15 19:33:06 carl Exp $
    Copyright (c) 1998-2001 by Peter Vreman

    This unit implements support import,export,link routines
    for the PalmOS (m68k) target

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
unit t_palmos;

  interface
  uses
    link;

  type
    plinkerPalmOS=^tlinkerPalmOS;
    tlinkerPalmOS=object(tlinker)
    private
       Function  WriteResponseFile : Boolean;
    public
       constructor Init;
       procedure SetDefaultInfo;virtual;
       function  MakeExecutable:boolean;virtual;
    end;


  implementation

    uses
       strings,globtype,globals,cobjects,systems,verbose,script,files;


{****************************************************************************
                               TLinkerPalmOS
****************************************************************************}

Constructor TLinkerPalmOS.Init;
begin
  Inherited Init;
  { allow duplicated libs (PM) }
  SharedLibFiles.doubles:=true;
  StaticLibFiles.doubles:=true;
end;


procedure TLinkerPalmOS.SetDefaultInfo;
begin
  with Info do
   begin
     { old ExeCmd[1]:='ldpalm $OPT $STRIP -N -dy -T $SCRIPT -o $EXE @$RES'; }
     ExeCmd[1]:='ldpalm --embedded-relocs --no-check-section $OPT $STRIP -N -dy -o $EXE @$RES';
     ExeCmd[2]:='build-prc $EXE.prc "$APPNAME" $APPID $EXE $BINS';
   end;
end;


Function TLinkerPalmOS.WriteResponseFile : Boolean;
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

  { Write path to search libraries }
  HPath:=current_module^.locallibrarysearchpath.First;
  while assigned(HPath) do
   begin
     LinkRes.Add('-L'+GetShortName(HPath^.Data^));
     HPath:=HPath^.Next;
   end;
  HPath:=LibrarySearchPath.First;
  while assigned(HPath) do
   begin
     LinkRes.Add('-L'+GetShortName(HPath^.Data^));
     HPath:=HPath^.Next;
   end;

  { add objectfiles, start with crt0 always  }
  { using crt0, we should stick C compatible }
  LinkRes.AddFileName(GetShortName(FindObjectFile('crt0','')));
  if cs_debuginfo in aktmoduleswitches then
   LinkRes.AddFileName(GetShortName(FindObjectFile('gdbstub','')));
  while not ObjectFiles.Empty do
   begin
     s:=ObjectFiles.Get;
     if s<>'' then
      LinkRes.AddFileName(GetShortName(s));
   end;

  { Write staticlibraries }
  if not StaticLibFiles.Empty then
   begin
     LinkRes.Add('-(');
     While not StaticLibFiles.Empty do
      begin
        S:=StaticLibFiles.Get;
        LinkRes.AddFileName(GetShortName(s))
      end;
     LinkRes.Add('-)');
   end;

  { currently the PalmOS target must be linked always against the C lib }
  LinkRes.Add('-lcrt');
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


function TLinkerPalmOS.MakeExecutable:boolean;
var
  binstr,
  cmdstr  : string;
  success : boolean;
  StripStr : string[40];
  i : longint;
begin
  if not(cs_link_extern in aktglobalswitches) then
    Message1(exec_i_linking,current_module^.exefilename^);

  { Create some replacements }
  StripStr:='';
  if (cs_link_strip in aktglobalswitches) then
   StripStr:='-s';

  { Write used files and libraries }
  WriteResponseFile;

{ Call linker }
  success:=false;
  for i:=1 to 2 do
   begin
     SplitBinCmd(Info.ExeCmd[i],binstr,cmdstr);
     if binstr<>'' then
      begin
        if pos(' ',current_module^.exefilename^)>0 then
          Replace(cmdstr,'$EXE','"'+current_module^.exefilename^+'"')
        else
          Replace(cmdstr,'$EXE',current_module^.exefilename^);

        Replace(cmdstr,'$OPT',Info.ExtraOptions);
        Replace(cmdstr,'$RES',outputexedir+Info.ResName);
        Replace(cmdstr,'$STRIP',StripStr);
        { Replace(cmdstr,'$SCRIPT',FindUtil('palm.ld'));}
        Replace(cmdstr,'$APPNAME',palmos_applicationname);
        Replace(cmdstr,'$APPID',palmos_applicationid);
        If FileExists('*.bin') then
          Replace(cmdstr,'$BINS','*.bin')
        else
          Replace(cmdstr,'$BINS','');


        success:=DoExec(FindUtil(binstr),cmdstr,(i=1),false);
        if not success then
         break;
      end;
   end;

  { Remove ReponseFile }
  if (success) and not(cs_link_extern in aktglobalswitches) then
   RemoveFile(outputexedir+Info.ResName);

  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;

end.
{
  $Log: t_palmos.pas,v $
  Revision 1.4.2.6  2003/01/15 19:33:06  carl
    * fix small compilation problems

  Revision 1.4.2.5  2003/01/15 15:44:18  pierre
   * fix embedded reloc problem

  Revision 1.4.2.4  2003/01/13 10:58:45  pierre
   + include gdbstub if debugging enabled

  Revision 1.4.2.3  2002/12/18 16:59:17  pierre
   * adapt to newer linker

  Revision 1.4.2.2  2002/12/12 00:20:01  pierre
   * adapt ldpalm call to newer version

  Revision 1.4.2.1  2001/08/17 16:16:52  florian
    + support for PalmOS added

}
