{
    $Id: cgconcmn.pas,v 1.1.2.1 2001/03/02 02:17:57 carl Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate cpu independant assembler for constants

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
unit cgconcmn;

interface

    uses
      tree;

    procedure secondfixconst(var p : ptree);
    procedure secondordconst(var p : ptree);
    procedure secondpointerconst(var p : ptree);
    procedure secondniln(var p : ptree);

implementation

    uses
      globtype,systems,
      cobjects,verbose,globals,
      symconst,symtable,aasm,types,
      hcodegen,temp_gen,pass_2,
      cpubase,cpuasm,
      cga,tgen;

{*****************************************************************************
                             SecondFixConst
*****************************************************************************}

    procedure secondfixconst(var p : ptree);
      begin
         { an fix comma const. behaves as a memory reference }
         p^.location.loc:=LOC_MEM;
         p^.location.reference.is_immediate:=true;
         p^.location.reference.offset:=p^.value_fix;
      end;


{*****************************************************************************
                             SecondOrdConst
*****************************************************************************}

    procedure secondordconst(var p : ptree);
      begin
         { an integer const. behaves as a memory reference }
         p^.location.loc:=LOC_MEM;
         p^.location.reference.is_immediate:=true;
         p^.location.reference.offset:=p^.value;
      end;


{*****************************************************************************
                             SecondPointerConst
*****************************************************************************}

    procedure secondpointerconst(var p : ptree);
      begin
         { an integer const. behaves as a memory reference }
         p^.location.loc:=LOC_MEM;
         p^.location.reference.is_immediate:=true;
         p^.location.reference.offset:=p^.value;
      end;


{*****************************************************************************
                             SecondNilN
*****************************************************************************}

    procedure secondniln(var p : ptree);
      begin
         p^.location.loc:=LOC_MEM;
         p^.location.reference.is_immediate:=true;
         p^.location.reference.offset:=0;
      end;

end.

{
  $Log: cgconcmn.pas,v $
  Revision 1.1.2.1  2001/03/02 02:17:57  carl
  + separated from cpu specific files

}

