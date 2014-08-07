{
    $Id: stringss.inc,v 1.1.2.3 2002/10/22 19:17:26 carl Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Pierre Muller
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

 Function strpas(p: pchar): string;
 { only 255 first characters are actually copied. }
  var
   counter : byte;
   lstr: string;
 Begin
   counter := 0;
   lstr := '';
   if assigned(p) then
     while (ord(p[counter]) <> 0) and (counter < 255) do
       begin
         Inc(counter);
         lstr[counter] := char(p[counter-1]);
       end;
   lstr[0] := char(counter);
   strpas := lstr;
 end;

   Function StrPCopy(D: PChar; Const S: String):PChar;
   var
    counter : byte;
  Begin
    counter := 0;
   { if empty pascal string  }
   { then setup and exit now }
   if S = '' then
   Begin
     D[0] := #0;
     StrPCopy := D;
     exit;
   end;
   for counter:=1 to length(S) do
   begin
     D[counter-1] := S[counter];
   end;
   { terminate the string }
   D[counter] := #0;
   StrPCopy:=D;
 end;



{
  $Log: stringss.inc,v $
  Revision 1.1.2.3  2002/10/22 19:17:26  carl
    - Generic version moved to m68k directory to avoid infinite recursivity

}
