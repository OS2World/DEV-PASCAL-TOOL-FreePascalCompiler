{
    $Id: printer.pas,v 1.1 2000/07/13 06:31:07 michael Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    Printer unit for BP7 compatible RTL

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit printer;
interface

var
  lst : text;

implementation

var
  old_exit : pointer;

procedure printer_exit;
begin
  close(lst);
  exitproc:=old_exit;
end;


begin
  assign(lst,'PRN');
  rewrite(lst);
  old_exit:=exitproc;
  exitproc:=@printer_exit;
end.
{
  $Log: printer.pas,v $
  Revision 1.1  2000/07/13 06:31:07  michael
  + Initial import

  Revision 1.4  2000/02/09 16:59:34  peter
    * truncated log

  Revision 1.3  2000/01/07 16:41:50  daniel
    * copyright 2000

  Revision 1.2  2000/01/07 16:32:33  daniel
    * copyright 2000 added

}
