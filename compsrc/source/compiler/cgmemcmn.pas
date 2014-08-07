{
    $Id: cgmemcmn.pas,v 1.1.2.3 2003/01/20 15:10:37 pierre Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate cpu independent assembler for in memory related nodes

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
unit cgmemcmn;

interface

    uses
      tree;


    procedure secondhnewn(var p : ptree);
    procedure secondsubscriptn(var p : ptree);
    procedure secondselfn(var p : ptree);

implementation

    uses
{$ifdef GDB}
      strings,gdb,
{$endif GDB}
      globtype,systems,
      cobjects,verbose,globals,
      symconst,symtable,aasm,types,
      hcodegen,temp_gen,pass_2,pass_1,
      cpubase,cpuasm,
      cga,tgen;

{*****************************************************************************
                             SecondHNewN
*****************************************************************************}

    procedure secondhnewn(var p : ptree);
      begin
      end;


{*****************************************************************************
                             SecondSubScriptN
*****************************************************************************}

    procedure secondsubscriptn(var p : ptree);
      var
         hr : tregister;
      begin
         secondpass(p^.left);
         if codegenerror then
           exit;
         { classes must be dereferenced implicit }
         if (p^.left^.resulttype^.deftype=objectdef) and
           pobjectdef(p^.left^.resulttype)^.is_class then
           begin
             reset_reference(p^.location.reference);
             case p^.left^.location.loc of
                LOC_REGISTER:
                 begin
                  if not isaddressregister(p^.left^.location.register) then
                    begin
                     hr := getaddressreg;
                     emit_mov_reg_reg(S_L,p^.left^.location.register,hr);
                     p^.location.reference.base:=hr;
                     ungetregister(p^.left^.location.register);
                   end
                  else
                    p^.location.reference.base:=p^.left^.location.register;
                 end;
                LOC_CREGISTER:
                  begin
                     { ... and reserve one for the pointer }
                     hr:=getaddressreg;
                     emit_mov_reg_reg(S_L,p^.left^.location.register,hr);
                     p^.location.reference.base:=hr;
                  end;
                else
                  begin
                     { free register }
                     del_reference(p^.left^.location.reference);

                     { ... and reserve one for the pointer }
                     hr:=getaddressreg;
                     emit_mov_ref_reg(S_L,
                       newreference(p^.left^.location.reference), hr);
                     p^.location.reference.base:=hr;
                  end;
             end;
           end
         else
           set_location(p^.location,p^.left^.location);

         inc(p^.location.reference.offset,p^.vs^.address);
      end;


{*****************************************************************************
                               SecondSelfN
*****************************************************************************}

    procedure secondselfn(var p : ptree);
      begin
         reset_reference(p^.location.reference);
         getexplicitregister32(self_pointer);
         if (p^.resulttype^.deftype=classrefdef) or
           (
            (p^.resulttype^.deftype=objectdef)
             and
            (pobjectdef(p^.resulttype)^.is_class or
             (po_staticmethod in aktprocsym^.definition^.procoptions)
            )
           ) then
           p^.location.register:=self_pointer
         else
           p^.location.reference.base:=self_pointer;
      end;

end.
{
  $Log: cgmemcmn.pas,v $
  Revision 1.1.2.3  2003/01/20 15:10:37  pierre
   * self for static method is a CREGISTER

  Revision 1.1.2.2  2001/03/03 12:40:19  jonas
    * fixed cvs Log tag

}
