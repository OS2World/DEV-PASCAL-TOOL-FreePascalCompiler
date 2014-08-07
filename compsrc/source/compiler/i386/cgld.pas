{
    $Id: cgld.pas,v 1.1.2.14 2003/04/29 07:26:18 michael Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate i386 assembler for load/assignment nodes

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
unit cgld;
interface

    uses
      tree;

    procedure secondload(var p : ptree);
    procedure secondassignment(var p : ptree);
    procedure secondarrayconstruct(var p : ptree);


implementation

    uses
      globtype,systems,
      cobjects,verbose,globals,files,
      symconst,symtable,aasm,types,
      hcodegen,temp_gen,pass_2,
      cpubase,cpuasm,
      cga,tgen,cgcnv,cresstr;

{*****************************************************************************
                             SecondLoad
*****************************************************************************}

    procedure secondload(var p : ptree);
      var
         hregister : tregister;
         symtabletype : tsymtabletype;
         i : longint;
         hp : preference;
         s : pasmsymbol;
         popeax : boolean;
         pushed : tpushed;
         hr : treference;

      begin
         simple_loadn:=true;
         reset_reference(p^.location.reference);
         case p^.symtableentry^.typ of
              { this is only for toasm and toaddr }
              absolutesym :
                 begin
                    p^.location.reference.symbol:=nil;
                    if (pabsolutesym(p^.symtableentry)^.abstyp=toaddr) then
                     begin
                       if pabsolutesym(p^.symtableentry)^.absseg then
                        p^.location.reference.segment:=R_FS;
                       p^.location.reference.offset:=pabsolutesym(p^.symtableentry)^.address;
                     end
                    else
                     p^.location.reference.symbol:=newasmsymbol(p^.symtableentry^.mangledname);
                 end;
              constsym:
                begin
                   if pconstsym(p^.symtableentry)^.consttyp=constresourcestring then
                     begin
                         pushusedregisters(pushed,$ff);
                         emit_const(A_PUSH,S_L,
                           pconstsym(p^.symtableentry)^.resstrindex);
                         emit_sym(A_PUSH,S_L,newasmsymbol(pconstsym(p^.symtableentry)^.owner^.name^+'_RESOURCESTRINGLIST'));
                         emitcall('FPC_GETRESOURCESTRING');

                         hregister:=getexplicitregister32(R_EAX);
                         emit_reg_reg(A_MOV,S_L,R_EAX,hregister);
                         gettempansistringreference(hr);
                         decrstringref(p^.resulttype,hr);
                         emit_reg_ref(A_MOV,S_L,hregister,
                           newreference(hr));
                        ungetregister32(hregister);
                        popusedregisters(pushed);

                        p^.location.loc:=LOC_MEM;
                        p^.location.reference:=hr;
                     end
                   else
                     internalerror(22798);
                end;
              varsym :
                 begin
                    hregister:=R_NO;
                    { C variable }
                    if (vo_is_C_var in pvarsym(p^.symtableentry)^.varoptions) then
                      begin
                         p^.location.reference.symbol:=newasmsymbol(p^.symtableentry^.mangledname);
                      end
                    { DLL variable }
                    else if (vo_is_dll_var in pvarsym(p^.symtableentry)^.varoptions) then
                      begin
                         hregister:=getregister32;
                         p^.location.reference.symbol:=newasmsymbol(p^.symtableentry^.mangledname);
                         emit_ref_reg(A_MOV,S_L,newreference(p^.location.reference),hregister);
                         p^.location.reference.symbol:=nil;
                         p^.location.reference.base:=hregister;
                      end
                    { external variable }
                    else if (vo_is_external in pvarsym(p^.symtableentry)^.varoptions) then
                      begin
                         p^.location.reference.symbol:=newasmsymbol(p^.symtableentry^.mangledname);
                      end
                    { thread variable }
                    else if (vo_is_thread_var in pvarsym(p^.symtableentry)^.varoptions) then
                      begin
                         popeax:=not(R_EAX in unused);
                         if popeax then
                           emit_reg(A_PUSH,S_L,R_EAX);
                         p^.location.reference.symbol:=newasmsymbol(p^.symtableentry^.mangledname);
                         emit_ref(A_PUSH,S_L,newreference(p^.location.reference));
                         { the called procedure isn't allowed to change }
                         { any register except EAX                    }
                         emitcall('FPC_RELOCATE_THREADVAR');

                         reset_reference(p^.location.reference);
                         p^.location.reference.base:=getregister32;
                         emit_reg_reg(A_MOV,S_L,R_EAX,p^.location.reference.base);
                         if popeax then
                           emit_reg(A_POP,S_L,R_EAX);

                      end
                    { normal variable }
                    else
                      begin
                         symtabletype:=p^.symtable^.symtabletype;
                         { in case it is a register variable: }
                         if pvarsym(p^.symtableentry)^.reg<>R_NO then
                           begin
                              if pvarsym(p^.symtableentry)^.reg in [R_ST0..R_ST7] then
                                begin
                                   p^.location.loc:=LOC_CFPUREGISTER;
                                   p^.location.register:=pvarsym(p^.symtableentry)^.reg;
                                end
                              else
                                begin
                                   p^.location.loc:=LOC_CREGISTER;
                                   p^.location.register:=pvarsym(p^.symtableentry)^.reg;
                                   unused:=unused-[pvarsym(p^.symtableentry)^.reg];
                                end;
                           end
                         else
                           begin
                              { first handle local and temporary variables }
                              if (symtabletype in [parasymtable,inlinelocalsymtable,
                                                   inlineparasymtable,localsymtable]) then
                                begin
                                   p^.location.reference.base:=procinfo^.framepointer;
                                   if (symtabletype in [inlinelocalsymtable,
                                                        localsymtable]) then
                                     p^.location.reference.offset:=
                                       pvarsym(p^.symtableentry)^.address-p^.symtable^.address_fixup
                                   else
                                     p^.location.reference.offset:=
                                       pvarsym(p^.symtableentry)^.address+p^.symtable^.address_fixup;

                                   if (symtabletype in [localsymtable,inlinelocalsymtable]) then
                                     begin
                                        if use_esp_stackframe then
                                          dec(p^.location.reference.offset,
                                            pvarsym(p^.symtableentry)^.getvaluesize)
                                        else
                                          p^.location.reference.offset:=-p^.location.reference.offset;
                                     end;
                                   if (lexlevel>(p^.symtable^.symtablelevel)) then
                                     begin
                                        hregister:=getregister32;

                                        { make a reference }
                                        hp:=new_reference(procinfo^.framepointer,
                                          procinfo^.framepointer_offset);

                                        emit_ref_reg(A_MOV,S_L,hp,hregister);

                                        simple_loadn:=false;
                                        i:=lexlevel-1;
                                        while i>(p^.symtable^.symtablelevel) do
                                          begin
                                             { make a reference }
                                             hp:=new_reference(hregister,8);
                                             emit_ref_reg(A_MOV,S_L,hp,hregister);
                                             dec(i);
                                          end;
                                        p^.location.reference.base:=hregister;
                                     end;
                                end
                              else
                                case symtabletype of
                                   unitsymtable,globalsymtable,
                                   staticsymtable :
                                     begin
                                       p^.location.reference.symbol:=newasmsymbol(p^.symtableentry^.mangledname);
                                     end;
                                   stt_exceptsymtable:
                                     begin
                                        p^.location.reference.base:=procinfo^.framepointer;
                                        p^.location.reference.offset:=pvarsym(p^.symtableentry)^.address;
                                     end;
                                   objectsymtable:
                                     begin
                                        getexplicitregister32(R_ESI);
                                        if (sp_static in pvarsym(p^.symtableentry)^.symoptions) then
                                          begin
                                             p^.location.reference.symbol:=newasmsymbol(p^.symtableentry^.mangledname);
                                          end
                                        else
                                          begin
                                             p^.location.reference.base:=R_ESI;
                                             p^.location.reference.offset:=pvarsym(p^.symtableentry)^.address;
                                          end;
                                     end;
                                   withsymtable:
                                     begin
                                        { make a reference }
                                        { symtable datasize field
                                          contains the offset of the temp
                                          stored }
{                                       hp:=new_reference(procinfo^.framepointer,
                                          p^.symtable^.datasize);

                                        emit_ref_reg(A_MOV,S_L,hp,hregister);}

                                        if ptree(pwithsymtable(p^.symtable)^.withnode)^.islocal then
                                         begin
                                           p^.location.reference:=ptree(pwithsymtable(p^.symtable)^.withnode)^.withreference^;
                                         end
                                        else
                                         begin
                                           hregister:=getregister32;
                                           p^.location.reference.base:=hregister;
                                           emit_ref_reg(A_MOV,S_L,
                                             newreference(ptree(pwithsymtable(p^.symtable)^.withnode)^.withreference^),
                                             hregister);
                                         end;
                                        inc(p^.location.reference.offset,pvarsym(p^.symtableentry)^.address);
                                     end;
                                end;
                           end;
                         { in case call by reference, then calculate. Open array
                           is always an reference! }
                         if (pvarsym(p^.symtableentry)^.varspez=vs_var) or
                            is_open_array(pvarsym(p^.symtableentry)^.vartype.def) or
                            (is_array_of_const(pvarsym(p^.symtableentry)^.vartype.def) and
                            { if it is an array of const then
                              the owner is a parasymtable }
                            not (pocall_cdecl in pprocdef(pvarsym(p^.symtableentry)^.owner^.defowner)^.proccalloptions)) or
                            ((pvarsym(p^.symtableentry)^.varspez=vs_const) and
                             push_addr_param(pvarsym(p^.symtableentry)^.vartype.def) and
                             (not (pocall_cdecl in pprocdef(pvarsym(p^.symtableentry)^.owner^.defowner)^.proccalloptions) or
                             (pvarsym(p^.symtableentry)^.vartype.def^.deftype=formaldef))) then
                           begin
                              simple_loadn:=false;
                              if hregister=R_NO then
                                hregister:=getregister32;
                              if p^.location.loc=LOC_CREGISTER then
                                begin
                                   emit_reg_reg(A_MOV,S_L,
                                     p^.location.register,hregister);
                                   p^.location.loc:=LOC_REFERENCE;
                                end
                              else
                                begin
                                   emit_ref_reg(A_MOV,S_L,
                                     newreference(p^.location.reference),
                                     hregister);
                                end;
                              reset_reference(p^.location.reference);
                              p^.location.reference.base:=hregister;
                          end;
                      end;
                 end;
              procsym:
                 begin
                    if assigned(p^.left) then
                      begin
                         secondpass(p^.left);
                         p^.location.loc:=LOC_MEM;
                         gettempofsizereference(8,p^.location.reference);

                         { load class instance address }
                         case p^.left^.location.loc of

                            LOC_CREGISTER,
                            LOC_REGISTER:
                              begin
                                 hregister:=p^.left^.location.register;
                                 ungetregister32(p^.left^.location.register);
                                 if (p^.left^.resulttype^.deftype<>classrefdef) and
                                    (p^.left^.resulttype^.deftype<>objectdef) and
                                    not(pobjectdef(p^.left^.resulttype)^.is_class) then
                                   CGMessage(cg_e_illegal_expression);
                              end;

                            LOC_MEM,
                            LOC_REFERENCE:
                              begin
{$ifndef noAllocEdi}
                                 getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                 hregister:=R_EDI;
                                 if pobjectdef(p^.left^.resulttype)^.is_class then
                                   emit_ref_reg(A_MOV,S_L,
                                     newreference(p^.left^.location.reference),R_EDI)
                                 else
                                   emit_ref_reg(A_LEA,S_L,
                                     newreference(p^.left^.location.reference),R_EDI);
                                 del_reference(p^.left^.location.reference);
                                 ungetiftemp(p^.left^.location.reference);
                              end;
                            else internalerror(26019);
                         end;

                         { store the class instance address }
                         new(hp);
                         hp^:=p^.location.reference;
                         inc(hp^.offset,4);
                         emit_reg_ref(A_MOV,S_L,
                           hregister,hp);

                         { virtual method ? }
                         if (po_virtualmethod in pprocdef(p^.resulttype)^.procoptions) then
                           begin
                              new(hp);
                              reset_reference(hp^);
                              hp^.base:=hregister;
                              { load vmt pointer }
                              emit_ref_reg(A_MOV,S_L,
                                hp,R_EDI);
{$IfDef regallocfix}
                              del_reference(hp^);
{$EndIf regallocfix}
                              { load method address }
                              new(hp);
                              reset_reference(hp^);
                              hp^.base:=R_EDI;
                              hp^.offset:=pprocdef(p^.resulttype)^._class^.vmtmethodoffset(
                                pprocdef(p^.resulttype)^.extnumber);
                              emit_ref_reg(A_MOV,S_L,
                                hp,R_EDI);
                              { ... and store it }
                              emit_reg_ref(A_MOV,S_L,
                                R_EDI,newreference(p^.location.reference));
{$ifndef noAllocEdi}
                              ungetregister32(R_EDI);
{$endif noAllocEdi}
                           end
                         else
                           begin
{$ifndef noAllocEdi}
                              ungetregister32(R_EDI);
{$endif noAllocEdi}
                              s:=newasmsymbol(pprocdef(p^.resulttype)^.mangledname);
                              emit_sym_ofs_ref(A_MOV,S_L,s,0,
                                newreference(p^.location.reference));
                           end;
                      end
                    else
                      begin
                         {!!!!! Be aware, work on virtual methods too }
                         p^.location.reference.symbol:=newasmsymbol(pprocdef(p^.resulttype)^.mangledname);
                      end;
                 end;
              typedconstsym :
                 begin
                    p^.location.reference.symbol:=newasmsymbol(p^.symtableentry^.mangledname);
                 end;
              else internalerror(4);
         end;
      end;


{*****************************************************************************
                             SecondAssignment
*****************************************************************************}

    procedure secondassignment(var p : ptree);
      var
         opsize : topsize;
         otlabel,hlabel,oflabel : pasmlabel;
         fputyp : tfloattype;
         loc : tloc;
         r : preference;
         ai : paicpu;
         op : tasmop;
         pushed : boolean;

      begin
         otlabel:=truelabel;
         oflabel:=falselabel;
         getlabel(truelabel);
         getlabel(falselabel);
         { calculate left sides }
         if not(p^.concat_string) then
           secondpass(p^.left);

         if codegenerror then
           exit;

         if not(p^.left^.location.loc in [LOC_REFERENCE,LOC_CFPUREGISTER,
           LOC_CREGISTER,LOC_CMMXREGISTER]) then
           begin
              CGMessage(cg_e_illegal_expression);
              exit;
           end;


         loc:=p^.left^.location.loc;
         { lets try to optimize this (PM)            }
         { define a dest_loc that is the location      }
         { and a ptree to verify that it is the right }
         { place to insert it                    }
{$ifdef test_dest_loc}
         if (aktexprlevel<4) then
           begin
              dest_loc_known:=true;
              dest_loc:=p^.left^.location;
              dest_loc_tree:=p^.right;
           end;
{$endif test_dest_loc}

         { left can't be never a 64 bit LOC_REGISTER, so the 3. arg }
         { can be false                                             }
         pushed:=maybe_push(p^.right^.registers32,p^.left,false);
         secondpass(p^.right);

         { restoring here is nonsense for LOC_JMP !! }
         { This generated code that was after a jmp and before any
           label => unreachable !!
           Could this be tested somehow ?? PM }
         if pushed and (p^.right^.location.loc <>LOC_JUMP) then
           restore(p^.left,false);

         if codegenerror then
           exit;

{$ifdef test_dest_loc}
         dest_loc_known:=false;
         if in_dest_loc then
           begin
              truelabel:=otlabel;
              falselabel:=oflabel;
              in_dest_loc:=false;
              exit;
           end;
{$endif test_dest_loc}
         if p^.left^.resulttype^.deftype=stringdef then
           begin
              if is_ansistring(p^.left^.resulttype) then
                begin
                  { the source and destinations are released
                    in loadansistring, because an ansi string can
                    also be in a register
                  }
                  loadansistring(p);
                end
              else
              if is_shortstring(p^.left^.resulttype) and
                not (p^.concat_string) then
                begin
                  if is_ansistring(p^.right^.resulttype) then
                    begin
                      if (p^.right^.treetype=stringconstn) and
                         (p^.right^.length=0) then
                        begin
                          emit_const_ref(A_MOV,S_B,
                            0,newreference(p^.left^.location.reference));
                          del_reference(p^.left^.location.reference);
                        end
                      else
                        loadansi2short(p^.right,p^.left);
                    end
                  else
                    begin
                       { we do not need destination anymore }
                       del_reference(p^.left^.location.reference);
                       {del_reference(p^.right^.location.reference);
                        done in loadshortstring }
                       loadshortstring(p);
                       ungetiftemp(p^.right^.location.reference);
                    end;
                end
              else if is_longstring(p^.left^.resulttype) then
                begin
                end
              else
                begin
                  { its the only thing we have to do }
                  del_reference(p^.right^.location.reference);
                end
           end
        else case p^.right^.location.loc of
            LOC_REFERENCE,
            LOC_MEM : begin
                         { extra handling for ordinal constants }
                         if (p^.right^.treetype in [ordconstn,fixconstn,pointerconstn,niln]) or
                            (loc=LOC_CREGISTER) then
                           begin
                              case p^.left^.resulttype^.size of
                                 1 : opsize:=S_B;
                                 2 : opsize:=S_W;
                                 4 : opsize:=S_L;
                                 { S_L is correct, the copy is done }
                                 { with two moves                   }
                                 8 : opsize:=S_L;
                              end;
                              if loc=LOC_CREGISTER then
                                begin
                                  emit_ref_reg(A_MOV,opsize,
                                    newreference(p^.right^.location.reference),
                                    p^.left^.location.register);
                                  if p^.left^.resulttype^.size=8 then
                                    begin
                                       r:=newreference(p^.right^.location.reference);
                                       inc(r^.offset,4);
                                       emit_ref_reg(A_MOV,opsize,r,
                                         p^.left^.location.registerhigh);
                                    end;
{$IfDef regallocfix}
                                  del_reference(p^.right^.location.reference);
{$EndIf regallocfix}
                                end
                              else
                                begin
                                  emit_const_ref(A_MOV,opsize,
                                    p^.right^.location.reference.offset,
                                    newreference(p^.left^.location.reference));
                                  if p^.left^.resulttype^.size=8 then
                                    begin
                                       r:=newreference(p^.left^.location.reference);
                                       inc(r^.offset,4);
                                       emit_const_ref(A_MOV,opsize,
                                         0,r);
                                    end;
{$IfDef regallocfix}
                                  del_reference(p^.left^.location.reference);
{$EndIf regallocfix}
                                {emit_const_loc(A_MOV,opsize,
                                    p^.right^.location.reference.offset,
                                    p^.left^.location);}
                                end;

                           end
                         else if loc=LOC_CFPUREGISTER then
                           begin
                              floatloadops(pfloatdef(p^.right^.resulttype)^.typ,op,opsize);
                              emit_ref(op,opsize,
                                newreference(p^.right^.location.reference));
                              emit_reg(A_FSTP,S_NO,
                                correct_fpuregister(p^.left^.location.register,fpuvaroffset+1));
                           end
                         else
                           begin
                              if (p^.right^.resulttype^.needs_inittable) and
                                ( (p^.right^.resulttype^.deftype<>objectdef) or
                                  not(pobjectdef(p^.right^.resulttype)^.is_class)) then
                                begin
                                   { this would be a problem }
                                   if not(p^.left^.resulttype^.needs_inittable) then
                                     internalerror(3457);
                                   new(r);
                                   { increment source reference counter }
                                   reset_reference(r^);
                                   r^.symbol:=p^.right^.resulttype^.get_inittable_label;
                                   emitpushreferenceaddr(r^);

                                   emitpushreferenceaddr(p^.right^.location.reference);
                                   emitcall('FPC_ADDREF');
                                   { decrement destination reference counter }
                                   r^.symbol:=p^.left^.resulttype^.get_inittable_label;
                                   emitpushreferenceaddr(r^);
                                   dispose(r);
                                   emitpushreferenceaddr(p^.left^.location.reference);
                                   emitcall('FPC_DECREF');
                                end;

{$ifdef regallocfix}
                              concatcopy(p^.right^.location.reference,
                                p^.left^.location.reference,p^.left^.resulttype^.size,true,false);
                              ungetiftemp(p^.right^.location.reference);
{$Else regallocfix}
                              concatcopy(p^.right^.location.reference,
                                p^.left^.location.reference,p^.left^.resulttype^.size,false,false);
                              ungetiftemp(p^.right^.location.reference);
{$endif regallocfix}
                           end;
                      end;
{$ifdef SUPPORT_MMX}
            LOC_CMMXREGISTER,
            LOC_MMXREGISTER:
              begin
                 if loc=LOC_CMMXREGISTER then
                   emit_reg_reg(A_MOVQ,S_NO,
                   p^.right^.location.register,p^.left^.location.register)
                 else
                   emit_reg_ref(A_MOVQ,S_NO,
                     p^.right^.location.register,newreference(p^.left^.location.reference));
              end;
{$endif SUPPORT_MMX}
            LOC_REGISTER,
            LOC_CREGISTER : begin
                              case p^.right^.resulttype^.size of
                                 1 : opsize:=S_B;
                                 2 : opsize:=S_W;
                                 4 : opsize:=S_L;
                                 8 : opsize:=S_L;
                              end;
                              { simplified with op_reg_loc       }
                              if loc=LOC_CREGISTER then
                                begin
                                  emit_reg_reg(A_MOV,opsize,
                                    p^.right^.location.register,
                                    p^.left^.location.register);
                                 ungetregister(p^.right^.location.register);
                                end
                              else
                                Begin
                                  emit_reg_ref(A_MOV,opsize,
                                    p^.right^.location.register,
                                    newreference(p^.left^.location.reference));
                                  ungetregister(p^.right^.location.register);
{$IfDef regallocfix}
                                  del_reference(p^.left^.location.reference);
{$EndIf regallocfix}
                                end;
                              if is_64bitint(p^.right^.resulttype) then
                                begin
                                   { simplified with op_reg_loc  }
                                   if loc=LOC_CREGISTER then
                                     emit_reg_reg(A_MOV,opsize,
                                       p^.right^.location.registerhigh,
                                       p^.left^.location.registerhigh)
                                   else
                                     begin
                                        r:=newreference(p^.left^.location.reference);
                                        inc(r^.offset,4);
                                        emit_reg_ref(A_MOV,opsize,
                                          p^.right^.location.registerhigh,r);
                                     end;
                                end;
                              {emit_reg_loc(A_MOV,opsize,
                                  p^.right^.location.register,
                                  p^.left^.location);      }

                           end;
            LOC_FPU : begin
                              if (p^.left^.resulttype^.deftype=floatdef) then
                               fputyp:=pfloatdef(p^.left^.resulttype)^.typ
                              else
                               if (p^.right^.resulttype^.deftype=floatdef) then
                                fputyp:=pfloatdef(p^.right^.resulttype)^.typ
                              else
                               if (p^.right^.treetype=typeconvn) and
                                  (p^.right^.left^.resulttype^.deftype=floatdef) then
                                fputyp:=pfloatdef(p^.right^.left^.resulttype)^.typ
                              else
                                fputyp:=s32real;
                              case loc of
                                 LOC_CFPUREGISTER:
                                   begin
                                      emit_reg(A_FSTP,S_NO,
                                        correct_fpuregister(p^.left^.location.register,fpuvaroffset));
                                      dec(fpuvaroffset);
                                   end;
                                 LOC_REFERENCE:
                                   floatstore(fputyp,p^.left^.location.reference);
                                 else
                                   internalerror(48991);
                              end;
                           end;
            LOC_CFPUREGISTER: begin
                              if (p^.left^.resulttype^.deftype=floatdef) then
                               fputyp:=pfloatdef(p^.left^.resulttype)^.typ
                              else
                               if (p^.right^.resulttype^.deftype=floatdef) then
                                fputyp:=pfloatdef(p^.right^.resulttype)^.typ
                              else
                               if (p^.right^.treetype=typeconvn) and
                                  (p^.right^.left^.resulttype^.deftype=floatdef) then
                                fputyp:=pfloatdef(p^.right^.left^.resulttype)^.typ
                              else
                                fputyp:=s32real;
                              emit_reg(A_FLD,S_NO,
                                correct_fpuregister(p^.right^.location.register,fpuvaroffset));
                              inc(fpuvaroffset);
                              case loc of
                                 LOC_CFPUREGISTER:
                                   begin
                                      emit_reg(A_FSTP,S_NO,
                                        correct_fpuregister(p^.right^.location.register,fpuvaroffset));
                                      dec(fpuvaroffset);
                                   end;
                                 LOC_REFERENCE:
                                   floatstore(fputyp,p^.left^.location.reference);
                                 else
                                   internalerror(48992);
                              end;
                           end;
            LOC_JUMP     : begin
                              opsize:=def_opsize(p^.left^.resulttype);
                              getlabel(hlabel);
                              emitlab(truelabel);
                              if pushed then
                                restore(p^.left,false);
                              if loc=LOC_CREGISTER then
                                emit_const_reg(A_MOV,opsize,
                                  1,p^.left^.location.register)
                              else
                                emit_const_ref(A_MOV,opsize,
                                  1,newreference(p^.left^.location.reference));
                              {emit_const_loc(A_MOV,opsize,
                                  1,p^.left^.location);}
                              emitjmp(C_None,hlabel);
                              emitlab(falselabel);
                              if pushed then
                                restore(p^.left,false);
                              if loc=LOC_CREGISTER then
                                emit_reg_reg(A_XOR,opsize,
                                  p^.left^.location.register,
                                  p^.left^.location.register)
                              else
                                begin
                                  emit_const_ref(A_MOV,opsize,
                                    0,newreference(p^.left^.location.reference));
{$IfDef regallocfix}
                                  del_reference(p^.left^.location.reference);
{$EndIf regallocfix}
                                 end;
                              emitlab(hlabel);
                           end;
            LOC_FLAGS    : begin
                              if loc=LOC_CREGISTER then
                                emit_flag2reg(p^.right^.location.resflags,p^.left^.location.register)
                              else
                                begin
                                  ai:=new(paicpu,op_ref(A_Setcc,S_B,newreference(p^.left^.location.reference)));
                                  ai^.SetCondition(flag_2_cond[p^.right^.location.resflags]);
                                  exprasmlist^.concat(ai);
                                end;
{$IfDef regallocfix}
                              del_reference(p^.left^.location.reference);
{$EndIf regallocfix}
                           end;
         end;
         truelabel:=otlabel;
         falselabel:=oflabel;
      end;




{*****************************************************************************
                           SecondArrayConstruct
*****************************************************************************}

      const
        vtInteger    = 0;
        vtBoolean    = 1;
        vtChar       = 2;
        vtExtended   = 3;
        vtString     = 4;
        vtPointer    = 5;
        vtPChar      = 6;
        vtObject     = 7;
        vtClass      = 8;
        vtWideChar   = 9;
        vtPWideChar  = 10;
        vtAnsiString = 11;
        vtCurrency   = 12;
        vtVariant    = 13;
        vtInterface  = 14;
        vtWideString = 15;
        vtInt64      = 16;
        vtQWord      = 17;

    procedure secondarrayconstruct(var p : ptree);
      var
        hp    : ptree;
        href  : treference;
        lt    : pdef;
        vaddr : boolean;
        vtype : longint;
        freetemp,
        dovariant : boolean;
        elesize : longint;
      begin
        dovariant:=p^.forcevaria or parraydef(p^.resulttype)^.isvariant;
        if dovariant then
         elesize:=8
        else
         elesize:=parraydef(p^.resulttype)^.elesize;
        if not p^.cargs then
         begin
           reset_reference(p^.location.reference);
           { Allocate always a temp, also if no elements are required, to
             be sure that location is valid (PFV) }
            if parraydef(p^.resulttype)^.highrange=-1 then
              gettempofsizereference(elesize,p^.location.reference)
            else
              gettempofsizereference((parraydef(p^.resulttype)^.highrange+1)*elesize,p^.location.reference);
           href:=p^.location.reference;
         end;
        hp:=p;
        while assigned(hp) do
         begin
           if assigned(hp^.left) then
            begin
              freetemp:=true;
              secondpass(hp^.left);
              if codegenerror then
               exit;
              if dovariant then
               begin
                 { find the correct vtype value }
                 vtype:=$ff;
                 vaddr:=false;
                 lt:=hp^.left^.resulttype;
                 case lt^.deftype of
                   enumdef,
                   orddef :
                     begin
                       if is_64bitint(lt) then
                         begin
                            case porddef(lt)^.typ of
                               s64bit:
                                 vtype:=vtInt64;
                               u64bit:
                                 vtype:=vtQWord;
                            end;
                            if not p^.cargs then
                              begin
                                freetemp:=false;
                                vaddr:=true;
                              end;
                         end
                       else if (lt^.deftype=enumdef) or
                         is_integer(lt) then
                         vtype:=vtInteger
                       else
                         if is_boolean(lt) then
                           vtype:=vtBoolean
                         else
                           if (lt^.deftype=orddef) and (porddef(lt)^.typ=uchar) then
                             vtype:=vtChar;
                     end;
                   floatdef :
                     begin
                       vtype:=vtExtended;
                       if not p^.cargs then
                         begin
                           vaddr:=true;
                           freetemp:=false;
                         end;
                     end;
                   procvardef,
                   pointerdef :
                     begin
                       if is_pchar(lt) then
                         vtype:=vtPChar
                       else
                         vtype:=vtPointer;
                     end;
                   classrefdef :
                     vtype:=vtClass;
                   objectdef :
                     begin
                       vtype:=vtObject;
                     end;
                   stringdef :
                     begin
                       if is_shortstring(lt) then
                        begin
                          vtype:=vtString;
                          vaddr:=true;
                          freetemp:=false;
                        end
                       else
                        if is_ansistring(lt) then
                         begin
                           vtype:=vtAnsiString;
                           freetemp:=false;
                         end;
                     end;
                 end;
                 if vtype=$ff then
                   internalerror(14357);
                 { write C style pushes or an pascal array }
                 if p^.cargs then
                  begin
                    if vaddr then
                     begin
                       emit_to_mem(hp^.left);
                       emit_push_lea_loc(hp^.left^.location,freetemp);
                       del_reference(hp^.left^.location.reference);
                       inc(pushedparasize,4);
                     end
                    else if vtype in [vtInt64,vtQword,vtExtended] then
                      push_value_para(hp^.left,false,true,
                        0,4)
                    else
                     begin
                       emit_push_loc(hp^.left^.location);
                       inc(pushedparasize,4);
                     end;
                  end
                 else
                  begin
                    { write changing field update href to the next element }
                    inc(href.offset,4);
                    if vaddr then
                     begin
                       emit_to_mem(hp^.left);
                       emit_lea_loc_ref(hp^.left^.location,href,freetemp);
                     end
                    else
                     begin
                       emit_mov_loc_ref(hp^.left^.location,href,S_L,freetemp);
                     end;
                    { update href to the vtype field and write it }
                    dec(href.offset,4);
                    emit_const_ref(A_MOV,S_L,vtype,newreference(href));
                    { goto next array element }
                    inc(href.offset,8);
                  end;
               end
              else
              { normal array constructor of the same type }
               begin
                 if is_ansistring(p^.left^.resulttype) or
                    is_widestring(p^.left^.resulttype) then
                   freetemp:=false;
                 case hp^.left^.location.loc of
                   LOC_FPU :
                     floatstore(pfloatdef(p^.left^.resulttype)^.typ,href);
                   else
                     begin
                       case elesize of
                         1 :
                           emit_mov_loc_ref(hp^.left^.location,href,S_B,freetemp);
                         2 :
                           emit_mov_loc_ref(hp^.left^.location,href,S_W,freetemp);
                         4 :
                           emit_mov_loc_ref(hp^.left^.location,href,S_L,freetemp);
                         8 :
                           begin
                             if hp^.left^.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                              begin
                                emit_reg_ref(A_MOV,S_L,hp^.left^.location.registerlow,newreference(href));
                                { update href to the high bytes and write it }
                                inc(href.offset,4);
                                emit_reg_ref(A_MOV,S_L,hp^.left^.location.registerhigh,newreference(href));
                                dec(href.offset,4)
                              end
                             else
                              concatcopy(hp^.left^.location.reference,href,elesize,freetemp,false);
                           end;
                         else
                           begin
                             { concatcopy only supports reference }
                             if not(hp^.left^.location.loc in [LOC_MEM,LOC_REFERENCE]) then
                              internalerror(200108012);
                             concatcopy(hp^.left^.location.reference,href,elesize,freetemp,false);
                           end;
                       end;
                     end;
                 end;
                 inc(href.offset,elesize);
               end;
            end;
           { load next entry }
           hp:=hp^.right;
         end;
      end;


end.
{
  $Log: cgld.pas,v $
  Revision 1.1.2.14  2003/04/29 07:26:18  michael
  + Patch from peter to fix wrong pushing of ansistring function results in open array

  Revision 1.1.2.13  2003/01/13 21:26:29  peter
    * fix for tb0436

  Revision 1.1.2.12  2002/11/12 15:06:27  pierre
   * ensure that all const/value parameters are pushed directly for cdecl'ared procedures

  Revision 1.1.2.11  2002/11/07 22:32:25  pierre
   * correct cdecl check for array of const parameter

  Revision 1.1.2.10  2002/11/07 12:13:32  pierre
   * remove memory leak when calling FPC_ADDREF

  Revision 1.1.2.9  2002/11/07 09:00:45  pierre
   * cdecl array of const is not indirect

  Revision 1.1.2.8  2002/11/05 15:36:42  pierre
   * fix C ellipses parameters for floats and 64bit integers

  Revision 1.1.2.7  2001/10/28 17:18:14  peter
    * allow assignment of overloaded procedures to procvars when we know
      which procedure to take

  Revision 1.1.2.6  2001/08/30 11:42:16  michael
  + Patch for wrong paramsize

  Revision 1.1.2.5  2001/08/05 13:19:27  peter
    * fix for proc of obj=nil

  Revision 1.1.2.4  2001/08/01 21:48:52  peter
    * fixed passing of array of record or shortstring to open array

  Revision 1.1.2.3  2001/07/28 15:10:49  peter
    * fixed opsize for assignment with LOC_JUMP

  Revision 1.1.2.2  2001/03/02 02:21:53  carl
  + separated into non cpu and cpu specific files
  + added isaddressregister() to check if pointer register

  Revision 1.1.2.1  2001/02/25 03:51:16  carl
  + trying again

  Revision 1.1.2.1  2001/02/25 02:33:34  carl
  - moved to i386 directory

  Revision 1.1  2000/07/13 06:29:45  michael
  + Initial import

  Revision 1.109  2000/06/30 22:12:26  peter
    * fix for bug 988

  Revision 1.108  2000/05/18 17:05:15  peter
    * fixed size of const parameters in asm readers

  Revision 1.107  2000/05/14 18:50:35  florian
    + Int64/QWord stuff for array of const added

  Revision 1.106  2000/04/03 12:23:02  pierre
   * fix for bug 909

  Revision 1.105  2000/03/19 11:55:08  peter
    * fixed temp ansi handling within array constructor

  Revision 1.104  2000/03/19 08:14:17  peter
    * small order change for array of const which allows better optimization

  Revision 1.103  2000/03/01 15:36:11  florian
    * some new stuff for the new cg

  Revision 1.102  2000/03/01 13:20:33  pierre
   * fix for bug 859

  Revision 1.101  2000/03/01 00:03:11  pierre
    * fixes for locals in inlined procedures
      fix for bug797
    + stabs generation for inlined paras and locals

  Revision 1.100  2000/02/09 18:08:33  jonas
    * added regallocs for esi

  Revision 1.99  2000/02/09 13:22:47  peter
    * log truncated

  Revision 1.98  2000/02/01 12:54:20  peter
    * cargs must also increase pushedparasize else it won't be 'popped'

  Revision 1.97  2000/01/21 12:17:42  jonas
    * regallocation fixes

  Revision 1.96  2000/01/09 12:35:01  jonas
    * changed edi allocation to use getexplicitregister32/ungetregister
      (adapted tgeni386 a bit for this) and enabled it by default
    * fixed very big and stupid bug of mine in cg386mat that broke the
      include() code (and make cycle :( ) if you compiled without
      -dnewoptimizations

  Revision 1.95  2000/01/09 01:44:20  jonas
    + (de)allocation info for EDI to fix reported bug on mailinglist.
      Also some (de)allocation info for ESI added. Between -dallocEDI
      because at this time of the night bugs could easily slip in ;)

  Revision 1.94  2000/01/07 01:14:21  peter
    * updated copyright to 2000

  Revision 1.93  1999/12/30 15:04:31  peter
    * fixed funcret within inlined procedure

  Revision 1.92  1999/12/22 01:01:47  peter
    - removed freelabel()
    * added undefined label detection in internal assembler, this prevents
      a lot of ld crashes and wrong .o files
    * .o files aren't written anymore if errors have occured
    * inlining of assembler labels is now correct

  Revision 1.91  1999/11/30 10:40:43  peter
    + ttype, tsymlist

  Revision 1.90  1999/11/06 14:34:18  peter
    * truncated log to 20 revs

  Revision 1.89  1999/10/12 22:35:48  florian
    * compiler didn't complain about l1+l2:=l1+l2; it gave only an assembler
      error, fixed

  Revision 1.88  1999/09/27 23:44:47  peter
    * procinfo is now a pointer
    * support for result setting in sub procedure

  Revision 1.87  1999/09/26 13:26:06  florian
    * exception patch of Romio nevertheless the excpetion handling
      needs some corections regarding register saving
    * gettempansistring is again a procedure

  Revision 1.86  1999/09/16 07:56:46  pierre
   * double del_reference removed

  Revision 1.85  1999/09/12 08:48:03  florian
    * bugs 593 and 607 fixed
    * some other potential bugs with array constructors fixed
    * for classes compiled in $M+ and it's childs, the default access method
      is now published
    * fixed copyright message (it is now 1998-2000)

  Revision 1.84  1999/09/11 09:08:31  florian
    * fixed bug 596
    * fixed some problems with procedure variables and procedures of object,
      especially in TP mode. Procedure of object doesn't apply only to classes,
      it is also allowed for objects !!

  Revision 1.83  1999/09/01 09:37:14  peter
    * removed warning

  Revision 1.82  1999/09/01 09:26:21  peter
    * fixed temp allocation for arrayconstructor

  Revision 1.81  1999/08/28 15:34:17  florian
    * bug 519 fixed

  Revision 1.80  1999/08/26 20:24:37  michael
  + Hopefuly last fixes for resourcestrings

  Revision 1.79  1999/08/25 16:41:05  peter
    * resources are working again

}
