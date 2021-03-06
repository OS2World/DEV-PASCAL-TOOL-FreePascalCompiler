{
    $Id: custapp.inc,v 1.1 2003/05/29 09:58:30 michael Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Linux version of custom app object routines.
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

function GetEnvironmentStrings : pchar; external 'kernel32' name 'GetEnvironmentStringsA';
function FreeEnvironmentStrings(p : pchar) : longbool; external 'kernel32' name 'FreeEnvironmentStringsA';

Procedure SysGetEnvironmentList(List : TStrings;NamesOnly : Boolean);

var
   s : string;
   i,l : longint;
   hp,p : pchar;

begin
  p:=GetEnvironmentStrings;
  hp:=p;
  while hp^<>#0 do
    begin
    s:=strpas(hp);
    l:=Length(s);
    If NamesOnly then 
      begin
      I:=pos('=',s);
      If (I>0) then
        S:=Copy(S,1,I-1);
      end;
    List.Add(S);
    hp:=hp+l+1;
    end;
   FreeEnvironmentStrings(p);
end;
