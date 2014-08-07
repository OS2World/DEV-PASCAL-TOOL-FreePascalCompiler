{
    $Id: cpuswtch.pas,v 1.2.2.2 2001/05/19 21:37:17 peter Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl, Pierre Muller

    interprets the commandline options which are i386 specific

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
unit cpuswtch;
interface

uses
  options;

type
  poption386=^toption386;
  toption386=object(toption)
    procedure interpret_proc_specific_options(const opt:string);virtual;
  end;

implementation

uses
  globtype,systems,globals;

procedure toption386.interpret_proc_specific_options(const opt:string);
var
  j     : longint;
  More  : string;
begin
  More:=Upper(copy(opt,3,length(opt)-2));
  case opt[2] of
   'O' : Begin
           j := 3;
           While (j <= Length(Opt)) Do
             Begin
               case opt[j] of
                 '-' : initglobalswitches:=initglobalswitches-[cs_optimize,cs_fastoptimize,cs_slowoptimize,cs_littlesize,
                           cs_regalloc,cs_uncertainopts,cs_align];
{$ifdef OPTALIGN}
                 'a' : initglobalswitches:=initglobalswitches+[cs_align];
{$endif OPTALIGN}
                 'g' : initglobalswitches:=initglobalswitches+[cs_littlesize];
                 'G' : initglobalswitches:=initglobalswitches-[cs_littlesize];
                 'r' :
                   begin
                     initglobalswitches:=initglobalswitches+[cs_regalloc];
                     simplify_ppu:=false;
                   end;
                 'u' : initglobalswitches:=initglobalswitches+[cs_optimize,cs_uncertainopts];
                 '1' : initglobalswitches:=initglobalswitches-[cs_slowoptimize,cs_uncertainopts]+[cs_optimize,cs_fastoptimize];
                 '2' : initglobalswitches:=initglobalswitches-[cs_uncertainopts]+[cs_optimize,cs_fastoptimize,cs_slowoptimize];
                 '3' : initglobalswitches:=initglobalswitches+[cs_optimize,cs_fastoptimize,cs_slowoptimize,cs_uncertainopts];
                 'p' :
                   Begin
                     If j < Length(Opt) Then
                       Begin
                         Case opt[j+1] Of
                           '1': initoptprocessor := Class386;
                           '2': initoptprocessor := ClassP5;
                           '3': initoptprocessor := ClassP6
                           Else IllegalPara(Opt)
                         End;
                         Inc(j);
                       End
                     Else IllegalPara(opt)
                   End;
{$ifdef USECMOV}
                 's' :
                   Begin
                     If j < Length(Opt) Then
                       Begin
                         Case opt[j+1] Of
                           '3': initspecificoptprocessor:=ClassP6
                           Else IllegalPara(Opt)
                         End;
                         Inc(j);
                       End
                     Else IllegalPara(opt)
                   End
{$endif USECMOV}
                 else IllegalPara(opt);
               End;
               Inc(j)
             end;
         end;
   'R' : begin
           if More='ATT' then
            initasmmode:=asmmode_i386_att
           else
            if More='INTEL' then
             initasmmode:=asmmode_i386_intel
           else
            if More='DIRECT' then
             initasmmode:=asmmode_i386_direct
           else
            IllegalPara(opt);
         end;
  else
   IllegalPara(opt);
  end;
end;

end.
{
  $Log: cpuswtch.pas,v $
  Revision 1.2.2.2  2001/05/19 21:37:17  peter
    * simplify_ppu is default, saves recompiles and makes therefor the
      compiler more stable and faster with recompiles

  Revision 1.2.2.1  2001/03/08 03:27:54  carl
  - renamed optscpu -> cpuswtch

  Revision 1.1.2.1  2001/03/04 02:22:20  carl
  - renamefest!

  Revision 1.1  2000/07/13 06:29:52  michael
  + Initial import

  Revision 1.20  2000/05/31 06:58:50  florian
    * first implementation of -Oa switch

  Revision 1.19  2000/02/09 13:22:55  peter
    * log truncated

  Revision 1.18  2000/01/23 21:29:17  florian
    * CMOV support in optimizer (in define USECMOV)
    + start of support of exceptions in constructors

  Revision 1.17  2000/01/07 01:14:28  peter
    * updated copyright to 2000

  Revision 1.16  1999/08/04 13:02:47  jonas
    * all tokens now start with an underscore
    * PowerPC compiles!!

}