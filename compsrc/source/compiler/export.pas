{
    $Id: export.pas,v 1.1.2.15 2002/12/04 20:50:42 pierre Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    This unit implements an uniform export object

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
unit export;

interface

uses
  cobjects{$IFDEF NEWST},objects{$ENDIF NEWST},symtable,aasm;

const
   { export options }
   eo_resident = $1;
   eo_index    = $2;
   eo_name     = $4;

type
   pexported_item = ^texported_item;
   texported_item = object(tlinkedlist_item)
      sym : psym;
      index : longint;
      name : pstring;
      options : word;
      is_var : boolean;
      constructor init;
      destructor done;virtual;
   end;

   pexportlib=^texportlib;
   texportlib=object
   private
      notsupmsg : boolean;
      procedure NotSupported;
   public
      edatalabel : pasmlabel;
      constructor Init;
      destructor Done;
      procedure preparelib(const s : string);virtual;
      procedure exportprocedure(hp : pexported_item);virtual;
      procedure exportvar(hp : pexported_item);virtual;
      procedure generatelib;virtual;
   end;

var
   exportlib : pexportlib;

procedure InitExport;
procedure DoneExport;

implementation

uses
  systems,verbose,globals,files
{$ifdef i386}
  {$ifndef NOTARGETLINUX}
    ,t_linux
  {$endif}
  {$ifndef NOTARGETFREEBSD}
    ,t_bsd
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
  ;

{****************************************************************************
                           TImported_procedure
****************************************************************************}

constructor texported_item.init;
begin
  inherited init;
  sym:=nil;
  index:=-1;
  name:=nil;
  options:=0;
  is_var:=false;
end;


destructor texported_item.done;
begin
  stringdispose(name);
  inherited done;
end;


{****************************************************************************
                              TImportLib
****************************************************************************}

constructor texportlib.Init;
begin
  notsupmsg:=false;
  edatalabel:=nil;
end;


destructor texportlib.Done;
begin
end;


procedure texportlib.NotSupported;
begin
  { show the message only once }
  if not notsupmsg then
   begin
     Message(exec_e_dll_not_supported);
     notsupmsg:=true;
   end;
end;


procedure texportlib.preparelib(const s:string);
begin
  NotSupported;
end;


procedure texportlib.exportprocedure(hp : pexported_item);
begin
  NotSupported;
end;


procedure texportlib.exportvar(hp : pexported_item);
begin
  NotSupported;
end;


procedure texportlib.generatelib;
begin
  NotSupported;
end;


procedure DoneExport;
begin
  if assigned(exportlib) then
    dispose(exportlib,done);
end;


procedure InitExport;
begin
  case target_info.target of
{$ifdef i386}
  {$ifndef NOTARGETLINUX}
    target_i386_Linux :
      exportlib:=new(pexportliblinux,Init);
  {$endif NOTARGETLINUX}
  {$ifndef NOTARGETFREEBSD}
    target_i386_freebsd,
    target_i386_netbsd,
    target_i386_openbsd:
      exportlib:=new(pexportlibbsd,Init);
  {$endif NOTARGETFREEBSD}
  {$ifndef NOTARGETSUNOS}
    target_i386_sunos:
      exportlib:=new(pexportlibsunos,Init);
  {$endif NOTARGETSUNOS}
  {$ifndef NOTARGETWIN32}
    target_i386_Win32 :
      exportlib:=new(pexportlibwin32,Init);
  {$endif NOTARGETWIN32}
  {$ifndef NOTARGETBEOS}
    target_i386_BeOS :
      exportlib:=new(pexportlibbeos,Init);
  {$endif NOTARGETBEOS}
  {$ifndef NOTARGETQNX}
    target_i386_qnx :
      exportlib:=new(pexportlibqnx,Init);
  {$endif NOTARGETQNX}

{
    target_i386_OS2 :
      exportlib:=new(pexportlibos2,Init);
}
{$endif i386}
{$ifdef m68k}
    target_m68k_Linux :
      exportlib:=new(pexportliblinux,Init);
  {$ifndef NOTARGETFREEBSD}
    target_m68k_netbsd,
    target_m68k_openbsd:
      exportlib:=new(pexportlibbsd,Init);
  {$endif NOTARGETFREEBSD}
{$endif m68k}
{$ifdef alpha}
    target_alpha_Linux :
      exportlib:=new(pexportlib,Init);
{$endif alpha}
{$ifdef powerpc}
    target_powerpc_Linux :
      exportlib:=new(pexportlib,Init);
{$endif powerpc}
    else
      exportlib:=new(pexportlib,Init);
  end;
end;


end.
{
  $Log: export.pas,v $
  Revision 1.1.2.15  2002/12/04 20:50:42  pierre
   + allow library generation for m68k linux

  Revision 1.1.2.14  2002/07/30 12:50:06  marco
   * Renaming t_fbsd -> t_bsd, and small OpenBSD fixes

  Revision 1.1.2.13  2002/07/30 11:21:40  marco
   * Small OpenBSD fixes

  Revision 1.1.2.12  2002/04/17 17:03:03  carl
  + QNX target

  Revision 1.1.2.11  2001/08/17 16:16:40  florian
    + support for PalmOS added

  Revision 1.1.2.10  2001/08/07 15:55:30  pierre
   + new code for NetBSD, behaves like FreeBSD for now

  Revision 1.1.2.9  2001/07/13 03:21:27  carl
  + BeOS support

  Revision 1.1.2.8  2001/06/06 22:00:33  peter
    * Win32 fixes

  Revision 1.1.2.7  2001/02/26 19:46:46  peter
    * more solaris to sunos

  Revision 1.1.2.6  2001/02/08 22:06:43  florian
    * SUNOS to sunos changed

  Revision 1.1.2.5  2001/02/02 22:39:23  peter
    * more notarget defines

  Revision 1.1.2.4  2001/02/02 21:06:37  jonas
    * compiler compiles again (with -dnotargetSUNOS)

  Revision 1.1.2.3  2001/02/01 22:31:54  florian
    + SUNOS support for the compiler

  Revision 1.1.2.2  2000/09/18 10:59:56  marco
   * Renamed t_freebsd to t_fbsd because of 8.3 convention

  Revision 1.1.2.1  2000/09/13 13:57:41  marco
   * FreeBSD compiler support

  Revision 1.1  2000/07/13 06:29:50  michael
  + Initial import

  Revision 1.12  2000/02/28 17:23:56  daniel
  * Current work of symtable integration committed. The symtable can be
    activated by defining 'newst', but doesn't compile yet. Changes in type
    checking and oop are completed. What is left is to write a new
    symtablestack and adapt the parser to use it.

  Revision 1.11  2000/02/09 13:22:52  peter
    * log truncated

  Revision 1.10  2000/01/12 10:34:29  peter
    * only give unsupported error once

  Revision 1.9  2000/01/07 01:14:27  peter
    * updated copyright to 2000

  Revision 1.8  1999/11/06 14:34:20  peter
    * truncated log to 20 revs

  Revision 1.7  1999/10/21 14:29:34  peter
    * redesigned linker object
    + library support for linux (only procedures can be exported)

  Revision 1.6  1999/08/04 13:02:41  jonas
    * all tokens now start with an underscore
    * PowerPC compiles!!

  Revision 1.5  1999/08/03 17:09:34  florian
    * the alpha compiler can be compiled now

}
