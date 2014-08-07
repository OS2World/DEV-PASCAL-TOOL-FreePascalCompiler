{
    $Id: cgldcmn.pas,v 1.1.2.3 2002/11/15 14:10:06 pierre Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate cpu-independant assembler for load/assignment nodes

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
unit cgldcmn;
interface

    uses
      tree;

    procedure secondfuncret(var p : ptree);


implementation

    uses
      globtype,systems,
      cobjects,verbose,globals,files,
      symconst,symtable,aasm,types,
      hcodegen,temp_gen,pass_2,
      cpubase,cpuasm,
      cga,tgen,cgcnv,cresstr;

{*****************************************************************************
                             SecondFuncRet
*****************************************************************************}

    procedure secondfuncret(var p : ptree);
      var
         hr : tregister;
         hp : preference;
         pp : pprocinfo;
         hr_valid : boolean;
      begin
         reset_reference(p^.location.reference);
         hr_valid:=false;
         if (not inlining_procedure) and
            (procinfo<>pprocinfo(p^.funcretprocinfo)) then
           begin
              hr:=getaddressreg;
              hr_valid:=true;
              hp:=new_reference(procinfo^.framepointer,
                procinfo^.framepointer_offset);
              emit_mov_ref_reg(S_L,hp,hr);
              pp:=procinfo^.parent;
              { walk up the stack frame }
              while pp<>pprocinfo(p^.funcretprocinfo) do
                begin
                   hp:=new_reference(hr,
                     pp^.framepointer_offset);
                   emit_mov_ref_reg(S_L,hp,hr);
                   pp:=pp^.parent;
                end;
              p^.location.reference.base:=hr;
              p^.location.reference.offset:=pp^.return_offset;
           end
         else
           begin
             p^.location.reference.base:=procinfo^.framepointer;
             p^.location.reference.offset:=procinfo^.return_offset;
           end;
         if ret_in_param(p^.rettype.def,
            pprocinfo(p^.funcretprocinfo)^.def^.proccalloptions) then
           begin
              if not hr_valid then
                hr:=getaddressreg;
              emit_mov_ref_reg(S_L,newreference(p^.location.reference),hr);
              p^.location.reference.base:=hr;
              p^.location.reference.offset:=0;
           end;
      end;

end.

{
  $Log: cgldcmn.pas,v $
  Revision 1.1.2.3  2002/11/15 14:10:06  pierre
    + added a calloptions parameter to ret_inacc and ret_in_param
      functions.
      This allows to handle win32 stdcall and m68k cdecl small records
      case.

  Revision 1.1.2.2  2001/03/03 12:40:19  jonas
    * fixed cvs Log tag

}
