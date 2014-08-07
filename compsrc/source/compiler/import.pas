{
    $Id: import.pas,v 1.1.2.15 2002/11/19 00:47:02 pierre Exp $
    Copyright (c) 1998-2000 by Peter Vreman

    This unit implements an uniform import object

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
unit import;

{$define GenerateStabs}

interface

uses
{$ifdef GenerateStabs}
    symtable,
{$endif GenerateStabs}
  cobjects{$IFDEF NEWST},objects{$ENDIF NEWST};

type
   pimported_item = ^timported_item;
   timported_item = object(tlinkedlist_item)
      ordnr  : word;
      name,
      func   : pstring;
      lab    : pointer; { should be plabel, but this gaves problems with circular units }
      is_var : boolean;
      constructor init(const n,s : string;o : word);
      constructor init_var(const n,s : string);
      destructor done;virtual;
   end;

   pimportlist = ^timportlist;
   timportlist = object(tlinkedlist_item)
      dllname : pstring;
      imported_items : plinkedlist;
      constructor init(const n : string);
      destructor done;virtual;
   end;

   pimportlib=^timportlib;
   timportlib=object
   private
      notsupmsg : boolean;
      procedure NotSupported;
   public
      constructor Init;
      destructor Done;
      procedure preparelib(const s:string);virtual;
{$ifdef GenerateStabs}
      procedure procsymimportprocedure(aprocsym : pprocsym; const func,module:string;index:longint;const name:string);virtual;
{$endif GenerateStabs}
      procedure importprocedure(const func,module:string;index:longint;const name:string);virtual;
      procedure importvariable(const varname,module:string;const name:string);virtual;
      procedure generatelib;virtual;
      procedure generatesmartlib;virtual;
   end;

var
  importlib : pimportlib;

procedure InitImport;
procedure DoneImport;

implementation

uses
  systems,verbose,globals
{$ifdef i386}
  {$ifndef NOTARGETLINUX}
    ,t_linux
  {$endif}
  {$ifndef NOTARGETBSD}
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
                           Timported_item
****************************************************************************}

constructor timported_item.init(const n,s : string;o : word);
begin
  inherited init;
  func:=stringdup(n);
  name:=stringdup(s);
  ordnr:=o;
  lab:=nil;
  is_var:=false;
end;


constructor timported_item.init_var(const n,s : string);
begin
  inherited init;
  func:=stringdup(n);
  name:=stringdup(s);
  ordnr:=0;
  lab:=nil;
  is_var:=true;
end;


destructor timported_item.done;
begin
  stringdispose(name);
  stringdispose(func);
  inherited done;
end;


{****************************************************************************
                              TImportlist
****************************************************************************}

constructor timportlist.init(const n : string);
begin
  inherited init;
  dllname:=stringdup(n);
  imported_items:=new(plinkedlist,init);
end;


destructor timportlist.done;
begin
  dispose(imported_items,done);
  stringdispose(dllname);
end;


{****************************************************************************
                              TImportLib
****************************************************************************}

constructor timportlib.Init;
begin
  notsupmsg:=false;
end;


destructor timportlib.Done;
begin
end;


procedure timportlib.NotSupported;
begin
  { show the message only once }
  if not notsupmsg then
   begin
     Message(exec_e_dll_not_supported);
     notsupmsg:=true;
   end;
end;


procedure timportlib.preparelib(const s:string);
begin
  NotSupported;
end;

{$ifdef GenerateStabs}
procedure timportlib.procsymimportprocedure(aprocsym : pprocsym; const func,module:string;index:longint;const name:string);
begin
  importprocedure(func, module, index, name);
end;
{$endif GenerateStabs}


procedure timportlib.importprocedure(const func,module:string;index:longint;const name:string);
begin
  NotSupported;
end;


procedure timportlib.importvariable(const varname,module:string;const name:string);
begin
  NotSupported;
end;


procedure timportlib.generatelib;
begin
  NotSupported;
end;


procedure timportlib.generatesmartlib;
begin
  NotSupported;
end;


procedure DoneImport;
begin
  if assigned(importlib) then
    dispose(importlib,done);
end;


procedure InitImport;
begin
  case target_info.target of
{$ifdef i386}
  {$ifndef NOTARGETLINUX}
    target_i386_Linux :
      importlib:=new(pimportliblinux,Init);
  {$endif}
  {$ifndef NOTARGETFREEBSD}
    target_i386_freebsd,
    target_i386_netbsd,
    target_i386_openbsd:
      importlib:=new(pimportlibbsd,Init);
  {$endif}
  {$ifndef NOTARGETSUNOS}
    target_i386_sunos:
      importlib:=new(pimportlibsunos,Init);
  {$endif}
  {$ifndef NOTARGETWIN32}
    target_i386_Win32 :
      importlib:=new(pimportlibwin32,Init);
  {$endif}
  {$ifndef NOTARGETOS2}
    target_i386_OS2 :
      importlib:=new(pimportlibos2,Init);
  {$endif}
  {$ifndef NOTARGETBEOS}
    target_i386_BEOS :
      importlib:=new(pimportlibbeos,Init);
  {$endif}
  {$ifndef NOTARGETQNX}
    target_i386_QNX :
      importlib:=new(pimportlibqnx,Init);
  {$endif}
{$endif i386}
{$ifdef m68k}
    target_m68k_Linux :
      importlib:=new(pimportliblinux,Init);
  {$ifndef NOTARGETFREEBSD}
    target_m68k_netbsd,
    target_m68k_openbsd:
      importlib:=new(pimportlibbsd,Init);
  {$endif}
{$endif m68k}
{$ifdef alpha}
    target_alpha_Linux :
      importlib:=new(pimportliblinux,Init);
{$endif alpha}
{$ifdef powerpc}
    target_alpha_Linux :
      importlib:=new(pimportliblinux,Init);
{$endif powerpc}
    else
      importlib:=new(pimportlib,Init);
  end;
end;


end.
{
  $Log: import.pas,v $
  Revision 1.1.2.15  2002/11/19 00:47:02  pierre
   * GenerateStabs default now

  Revision 1.1.2.14  2002/08/13 13:32:28  pierre
   * commit missing procsymimportprocedure method

  Revision 1.1.2.13  2002/07/30 12:50:07  marco
   * Renaming t_fbsd -> t_bsd, and small OpenBSD fixes

  Revision 1.1.2.12  2002/07/30 11:21:40  marco
   * Small OpenBSD fixes

  Revision 1.1.2.11  2002/04/17 17:03:53  carl
  + QNX target

  Revision 1.1.2.10  2001/08/17 16:16:40  florian
    + support for PalmOS added

  Revision 1.1.2.9  2001/08/07 15:55:30  pierre
   + new code for NetBSD, behaves like FreeBSD for now

  Revision 1.1.2.8  2001/07/13 03:21:11  carl
  + BeOS support

  Revision 1.1.2.7  2001/02/26 19:46:46  peter
    * more solaris to sunos

  Revision 1.1.2.6  2001/02/08 22:06:43  florian
    * SUNOS to sunos changed

  Revision 1.1.2.5  2001/02/02 22:39:23  peter
    * more notarget defines

  Revision 1.1.2.4  2001/02/02 21:06:37  jonas
    * compiler compiles again (with -dnotargetSUNOS)

  Revision 1.1.2.3  2001/02/01 22:31:55  florian
    + SUNOS support for the compiler

  Revision 1.1.2.2  2000/09/18 10:59:56  marco
   * Renamed t_freebsd to t_fbsd because of 8.3 convention

  Revision 1.1.2.1  2000/09/13 13:57:41  marco
   * FreeBSD compiler support

  Revision 1.1  2000/07/13 06:29:52  michael
  + Initial import

  Revision 1.19  2000/02/28 17:23:57  daniel
  * Current work of symtable integration committed. The symtable can be
    activated by defining 'newst', but doesn't compile yet. Changes in type
    checking and oop are completed. What is left is to write a new
    symtablestack and adapt the parser to use it.

  Revision 1.18  2000/02/09 13:22:54  peter
    * log truncated

  Revision 1.17  2000/01/12 10:34:29  peter
    * only give unsupported error once

  Revision 1.16  2000/01/07 01:14:27  peter
    * updated copyright to 2000

  Revision 1.15  1999/11/06 14:34:21  peter
    * truncated log to 20 revs

  Revision 1.14  1999/11/02 15:06:57  peter
    * import library fixes for win32
    * alignment works again

  Revision 1.13  1999/10/21 14:29:34  peter
    * redesigned linker object
    + library support for linux (only procedures can be exported)

  Revision 1.12  1999/08/04 13:02:44  jonas
    * all tokens now start with an underscore
    * PowerPC compiles!!

  Revision 1.11  1999/08/03 13:50:16  michael
  + Changes for alpha

}
