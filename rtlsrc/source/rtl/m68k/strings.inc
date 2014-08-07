{
    $Id: strings.inc,v 1.1.2.2 2002/10/22 19:17:26 carl Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Carl-Eric Codere,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
  {*********************************************************************}
  { Strings include file adapted from                                   }
  { Strings unit, 100% portable.                                        }
  {- COMPILING INFORMATION ---------------------------------------------}
  {   The only difference between this  unit and the one supplied with  }
  {   Turbo Pascal 7.01, are that StrLen returns a longint, and the     }
  {   routines requiring a length now use longints instead of words.    }
  {   This should not influence the behaviour of your programs under    }
  {   Turbo Pascal. (it will even create better error checking for your }
  {   programs).                                                        }
  {*********************************************************************}


 function strlen(p : pchar) : Longint;
  var
   counter : Longint;
 Begin
   counter := 0;
   if assigned(p) then
     while p[counter] <> #0 do
       Inc(counter);
   strlen := counter;
 end;



 Function StrEnd(p: PChar): PChar;
 var
  counter: Longint;
 begin
   counter := 0;
   while p[counter] <> #0 do
      Inc(counter);
   StrEnd := @(p[Counter]);
 end;


 Function StrCopy(Dest, Source:PChar): PChar;
 var
   counter : Longint;
 Begin
   counter := 0;
   while Source[counter] <> #0 do
   begin
     Dest[counter] := char(Source[counter]);
     Inc(counter);
   end;
   { terminate the string }
   Dest[counter] := #0;
   StrCopy := Dest;
 end;


(*
 function StrCat(Dest,Source: PChar): PChar;
 var
  counter: Longint;
  PEnd: PChar;
 begin
   PEnd := StrEnd(Dest);
   counter := 0;
   while (Source[counter] <> #0) do
   begin
     PEnd[counter] := char(Source[counter]);
     Inc(counter);
   end;
   { terminate the string }
   PEnd[counter] := #0;
   StrCat := Dest;
 end;
*)

 function StrUpper(p: PChar): PChar;
 var
  counter: Longint;
 begin
   counter := 0;
   while (p[counter] <> #0) do
   begin
     if p[Counter] in [#97..#122,#128..#255] then
        p[counter] := Upcase(p[counter]);
     Inc(counter);
   end;
   StrUpper := p;
 end;

 function StrLower(p: PChar): PChar;
 var
  counter: Longint;
 begin
   counter := 0;
   while (p[counter] <> #0) do
   begin
     if p[counter] in [#65..#90] then
        p[Counter] := chr(ord(p[Counter]) + 32);
     Inc(counter);
   end;
   StrLower := p;
 end;


(*
  function StrPos(Str1,Str2: PChar): PChar;
 var
  count: Longint;
  oldindex: Longint;
  found: boolean;
  Str1Length: Longint;
  Str2Length: Longint;
  ll: Longint;
 Begin

   Str1Length := StrLen(Str1);
   Str2Length := StrLen(Str2);
   found := true;
   oldindex := 0;

   { If the search string is greater than the string to be searched }
   { it is certain that we will not find it.                        }
   { Furthermore looking for a null will simply give out a pointer, }
   { to the null character of str1 as in Borland Pascal.            }
   if (Str2Length > Str1Length) or (Str2[0] = #0) then
   begin
     StrPos := nil;
     exit;
   end;

   Repeat
     { Find first matching character of Str2 in Str1 }
     { put index of this character in oldindex       }
     for count:= oldindex to Str1Length-1 do
     begin
        if Str2[0] = Str1[count] then
        begin
           oldindex := count;
           break;
        end;
        { nothing found - exit routine }
        if count = Str1Length-1 then
        begin
           StrPos := nil;
           exit;
        end;
     end;

     found := true;
     { Compare the character strings }
     { and check if they match.      }
     for ll := 0 to Str2Length-1 do
     begin
       { no match, stop iteration }
        if (Str2[ll] <> Str1[ll+oldindex]) then
        begin
           found := false;
           break;
        end;
     end;
     { Not found, the index will no point at next character }
     if not found then
       Inc(oldindex);
     { There was a match }
     if found then
     begin
        StrPos := @(Str1[oldindex]);
        exit;
     end;
   { If we have gone through the whole string to search }
   { then exit routine.                                 }
   Until (Str1Length-oldindex) <= 0;
   StrPos := nil;
 end;
*)


 function StrScan(p: PChar; C: Char): PChar;
   Var
     count: Longint;
  Begin

   count := 0;
   { As in Borland Pascal , if looking for NULL return null }
   if c = #0 then
   begin
     StrScan := @(p[StrLen(p)]);
     exit;
   end;
   { Find first matching character of Ch in Str }
   while p[count] <> #0 do
   begin
     if C = p[count] then
      begin
          StrScan := @(p[count]);
          exit;
      end;
     Inc(count);
   end;
   { nothing found. }
   StrScan := nil;
 end;



 function StrRScan(p: PChar; C: Char): PChar;
 Var
  count: Longint;
  index: Longint;
 Begin
   count := Strlen(p);
   { As in Borland Pascal , if looking for NULL return null }
   if c = #0 then
   begin
     StrRScan := @(p[count]);
     exit;
   end;
   Dec(count);
   for index := count downto 0 do
   begin
     if C = p[index] then
      begin
          StrRScan := @(p[index]);
          exit;
      end;
   end;
   { nothing found. }
   StrRScan := nil;
 end;


(*
 function StrNew(p:PChar): PChar;
      var
         len : Longint;
         tmp : pchar;
      begin
         strnew:=nil;
         if (p=nil) or (p^=#0) then
           exit;
         len:=strlen(p)+1;
         getmem(tmp,len);
         if tmp<>nil then
           strmove(tmp,p,len);
         StrNew := tmp;
      end;
*)


  Function StrECopy(Dest, Source: PChar): PChar;
 { Equivalent to the following:                                          }
 {  strcopy(Dest,Source);                                                }
 {  StrECopy := StrEnd(Dest);                                            }
 var
   counter : Longint;
 Begin
   counter := 0;
   while Source[counter] <> #0 do
   begin
     Dest[counter] := char(Source[counter]);
     Inc(counter);
   end;
   { terminate the string }
   Dest[counter] := #0;
   StrECopy:=@(Dest[counter]);
 end;


(*
 procedure strdispose(p : pchar);
 begin
   if p<>nil then
      freemem(p,strlen(p)+1);
 end;
*)

(*
 function strmove(dest,source : pchar;l : Longint) : pchar;
 begin
   move(source^,dest^,l);
   strmove:=dest;
 end;
*)


(*
 function strlcat(dest,source : pchar;l : Longint) : pchar;
 var
   destend : pchar;
 begin
   destend:=strend(dest);
   l:=l-(destend-dest);
   strlcat:=strlcopy(destend,source,l);
 end; *)


 Function StrLCopy(Dest,Source: PChar; MaxLen: Longint): PChar;
  var
   counter: Longint;
 Begin
   counter := 0;
   { To be compatible with BP, on a null string, put two nulls }
   If Source[0] = #0 then
   Begin
     Dest[0]:=Source[0];
     Inc(counter);
   end;
   while (Source[counter] <> #0)  and (counter < MaxLen) do
   Begin
      Dest[counter] := char(Source[counter]);
      Inc(counter);
   end;
   { terminate the string }
   Dest[counter] := #0;
   StrLCopy := Dest;
 end;


 function StrComp(Str1, Str2 : PChar): LongInt;
     var
      counter: Longint;
     Begin
        counter := 0;
       While str1[counter] = str2[counter] do
       Begin
         if (str2[counter] = #0) or (str1[counter] = #0) then
            break;
         Inc(counter);
       end;
       StrComp := ord(str1[counter]) - ord(str2[counter]);
     end;

     function StrIComp(Str1, Str2 : PChar): LongInt;
     var
      counter: Longint;
      c1, c2: char;
     Begin
        counter := 0;
        c1 := upcase(str1[counter]);
        c2 := upcase(str2[counter]);
       While c1 = c2 do
       Begin
         if (c1 = #0) or (c2 = #0) then break;
         Inc(counter);
         c1 := upcase(str1[counter]);
         c2 := upcase(str2[counter]);
      end;
       StrIComp := ord(c1) - ord(c2);
     end;


     function StrLComp(Str1, Str2 : PChar; L: Longint): LongInt;
     var
      counter: Longint;
      c1, c2: char;
     Begin
       counter := 0;
       if L = 0 then
       begin
         StrLComp := 0;
         exit;
       end;
       Repeat
         c1 := str1[counter];
         c2 := str2[counter];
         if (c1 = #0) or (c2 = #0) then break;
         Inc(counter);
      Until (c1 <> c2) or (counter >= L);
       StrLComp := ord(c1) - ord(c2);
     end;



     function StrLIComp(Str1, Str2 : PChar; L: Longint): Longint;
     var
      counter: Longint;
      c1, c2: char;
     Begin
        counter := 0;
       if L = 0 then
       begin
         StrLIComp := 0;
         exit;
       end;
       Repeat
         c1 := upcase(str1[counter]);
         c2 := upcase(str2[counter]);
         if (c1 = #0) or (c2 = #0) then break;
         Inc(counter);
      Until (c1 <> c2) or (counter >= L);
       StrLIComp := ord(c1) - ord(c2);
     end;
{
  $Log: strings.inc,v $
  Revision 1.1.2.2  2002/10/22 19:17:26  carl
    - Generic version moved to m68k directory to avoid infinite recursivity

}
