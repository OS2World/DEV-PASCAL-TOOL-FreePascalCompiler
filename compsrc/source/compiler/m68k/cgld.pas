{
    $Id: cgld.pas,v 1.1.2.23 2002/11/12 15:06:27 pierre Exp $
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
      cga,tgen,cgcnv,cresstr,cgbase;

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
         popacc : boolean;
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
                      p^.location.reference.offset:=pabsolutesym(p^.symtableentry)^.address
                    else
                      begin
                         p^.location.reference.symbol:=newasmsymbol(p^.symtableentry^.mangledname);
                         if tf_static_a5_based in target_info.flags then
                           p^.location.reference.base:=R_A5;
                      end;
                 end;
              constsym:
                begin
                   if pconstsym(p^.symtableentry)^.consttyp=constresourcestring then
                     begin
                         saveusedregisters(pushed,ALL_REGISTERS);
                         emit_const_reg(A_MOVE,S_L,
                           pconstsym(p^.symtableentry)^.resstrindex,R_SPPUSH);
                         emit_sym(A_PEA,S_L,newasmsymbol(pconstsym(p^.symtableentry)^.owner^.name^+'_RESOURCESTRINGLIST'));
                         emitcall('FPC_GETRESOURCESTRING');

                         hregister:=getexplicitregister32(R_D0);
                         emit_reg_reg(A_MOVE,S_L,R_D0,hregister);
                         gettempansistringreference(hr);
                         decrstringref(p^.resulttype,hr);
                         emit_reg_ref(A_MOVE,S_L,hregister,
                           newreference(hr));
                         ungetregister32(hregister);
                         restoreusedregisters(pushed);

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
                         if tf_static_a5_based in target_info.flags then
                           p^.location.reference.base:=R_A5;
                      end
                    { DLL variable }
                    else if (vo_is_dll_var in pvarsym(p^.symtableentry)^.varoptions) then
                      begin
                         hregister:=getaddressreg;
                         p^.location.reference.symbol:=newasmsymbol(p^.symtableentry^.mangledname);
                         emit_ref_reg(A_MOVE,S_L,newreference(p^.location.reference),hregister);
                         p^.location.reference.symbol:=nil;
                         p^.location.reference.base:=hregister;
                      end
                    { external variable }
                    else if (vo_is_external in pvarsym(p^.symtableentry)^.varoptions) then
                      begin
                         p^.location.reference.symbol:=newasmsymbol(p^.symtableentry^.mangledname);
                         if tf_static_a5_based in target_info.flags then
                           p^.location.reference.base:=R_A5;
                      end
                    { thread variable }
                    else if (vo_is_thread_var in pvarsym(p^.symtableentry)^.varoptions) then
                      begin
                         popacc:=not(R_D0 in unused);
                         if popacc then
                           emit_reg_reg(A_MOVE,S_L,R_D0,R_SPPUSH);
                         p^.location.reference.symbol:=newasmsymbol(p^.symtableentry^.mangledname);
                         emit_ref_reg(A_MOVE,S_L,newreference(p^.location.reference),R_SPPUSH);
                         { the called procedure isn't allowed to change }
                         { any register except D0                       }
                         emitcall('FPC_RELOCATE_THREADVAR');

                         reset_reference(p^.location.reference);
                         p^.location.reference.base:=getaddressreg;
                         emit_reg_reg(A_MOVE,S_L,R_D0,p^.location.reference.base);
                         if popacc then
                           emit_reg_reg(A_MOVE,S_L,R_SPPULL,R_D0);

                      end
                    { normal variable }
                    else
                      begin
                         symtabletype:=p^.symtable^.symtabletype;
                         { in case it is a register variable: }
                         if pvarsym(p^.symtableentry)^.reg<>R_NO then
                           begin
{                              if pvarsym(p^.symtableentry)^.reg in [R_FP0..R_FP7] then
                                begin
                                   p^.location.loc:=LOC_CFPUREGISTER;
                                   p^.location.register:=pvarsym(p^.symtableentry)^.reg;
                                end
                              else}
                                begin
                                   p^.location.loc:=LOC_CREGISTER;
                                   p^.location.register:=pvarsym(p^.symtableentry)^.reg;
                                   unused:=unused-[pvarsym(p^.symtableentry)^.reg];
                                end;
                           end
                         else
                           begin
                              { --------------------- START OF LOCAL AND TEMP VARS ---------------- }
                              { first handle local and temporary variables }
                              if (symtabletype in [parasymtable,inlinelocalsymtable,
                                                   inlineparasymtable,localsymtable]) then
                                begin
                                   p^.location.reference.base:=procinfo^.framepointer;
                                   if (symtabletype in [inlinelocalsymtable,localsymtable]) then
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
                                        hregister:=getaddressreg;

                                        { make a reference }
                                        hp:=new_reference(procinfo^.framepointer,
                                          procinfo^.framepointer_offset);

                                        emit_ref_reg(A_MOVE,S_L,hp,hregister);

                                        simple_loadn:=false;
                                        i:=lexlevel-1;
                                        while i>(p^.symtable^.symtablelevel) do
                                          begin
                                             { make a reference }
                                             hp:=new_reference(hregister,8);
                                             emit_ref_reg(A_MOVE,S_L,hp,hregister);
                                             dec(i);
                                          end;
                                        p^.location.reference.base:=hregister;
                                     end;
                                end
                              { --------------------- END OF LOCAL AND TEMP VARS ---------------- }
                              else
                                case symtabletype of
                                   unitsymtable,globalsymtable,
                                   staticsymtable :
                                     begin
                                       p^.location.reference.symbol:=newasmsymbol(p^.symtableentry^.mangledname);
                                       if tf_static_a5_based in target_info.flags then
                                         p^.location.reference.base:=R_A5;
                                     end;
                                   stt_exceptsymtable:
                                     begin
                                        p^.location.reference.base:=procinfo^.framepointer;
                                        p^.location.reference.offset:=pvarsym(p^.symtableentry)^.address;
                                     end;
                                   objectsymtable:
                                     begin
                                        getexplicitregister32(self_pointer);
                                        if (sp_static in pvarsym(p^.symtableentry)^.symoptions) then
                                          begin
                                             p^.location.reference.symbol:=newasmsymbol(p^.symtableentry^.mangledname);
                                          end
                                        else
                                          begin
                                             p^.location.reference.base:=self_pointer;
                                             p^.location.reference.offset:=pvarsym(p^.symtableentry)^.address;
                                          end;
                                     end;
                                   withsymtable:
                                     begin

                                        if ptree(pwithsymtable(p^.symtable)^.withnode)^.islocal then
                                         begin
                                           p^.location.reference:=ptree(pwithsymtable(p^.symtable)^.withnode)^.withreference^;
                                         end
                                        else
                                         begin
                                           hregister:=getaddressreg;
                                           p^.location.reference.base:=hregister;
                                           emit_ref_reg(A_MOVE,S_L,
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
                                hregister:=getaddressreg;
                              if not isaddressregister(hregister) then
                                begin
                                  ungetregister(hregister);
                                  hregister := getaddressreg;
                                end;
                              if p^.location.loc=LOC_CREGISTER then
                                begin
                                   emit_reg_reg(A_MOVE,S_L,
                                     p^.location.register,hregister);
                                   p^.location.loc:=LOC_REFERENCE;
                                end
                              else
                                begin
                                   emit_ref_reg(A_MOVE,S_L,
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
                         getexplicitregister32(R_A0);

                         { load class instance address }
                         case p^.left^.location.loc of

                            LOC_CREGISTER,
                            LOC_REGISTER:
                              begin
                                 if not isaddressregister(p^.left^.location.register) then
                                   begin
                                     hregister := R_A0;
                                     emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,hregister);
                                   end
                                 else
                                   begin
                                     hregister:=p^.left^.location.register;
                                   end;
                                 ungetregister32(p^.left^.location.register);
                                 if (p^.left^.resulttype^.deftype<>classrefdef) and
                                    (p^.left^.resulttype^.deftype<>objectdef) and
                                    not(pobjectdef(p^.left^.resulttype)^.is_class) then
                                   CGMessage(cg_e_illegal_expression);
                              end;

                            LOC_MEM,
                            LOC_REFERENCE:
                              begin
                                 hregister:=R_A0;
                                 if pobjectdef(p^.left^.resulttype)^.is_class then
                                   emit_ref_reg(A_MOVE,S_L,
                                     newreference(p^.left^.location.reference),R_A0)
                                 else
                                   emit_ref_reg(A_LEA,S_L,
                                     newreference(p^.left^.location.reference),R_A0);
                                 del_reference(p^.left^.location.reference);
                                 ungetiftemp(p^.left^.location.reference);
                              end;
                            else internalerror(26019);
                         end;

                         { store the class instance address }
                         new(hp);
                         hp^:=p^.location.reference;
                         inc(hp^.offset,4);
                         emit_reg_ref(A_MOVE,S_L,
                           hregister,hp);

                         { virtual method ? }
                         if (po_virtualmethod in pprocdef(p^.resulttype)^.procoptions) then
                           begin
                              new(hp);
                              reset_reference(hp^);
                              hp^.base:=hregister;
                              { load vmt pointer }
                              emit_ref_reg(A_MOVE,S_L,
                                hp,R_A0);
                              { load method address }
                              new(hp);
                              reset_reference(hp^);
                              hp^.base:=R_A0;
                              hp^.offset:=pprocdef(p^.resulttype)^._class^.vmtmethodoffset(
                                          pprocdef(p^.resulttype)^.extnumber);
                              emit_ref_reg(A_MOVE,S_L,hp,R_A0);
                              { ... and store it }
                              emit_reg_ref(A_MOVE,S_L,
                                R_A0,newreference(p^.location.reference));
                           end
                         else
                           begin
                              s:=newasmsymbol(pprocdef(p^.resulttype)^.mangledname);
                              emit_sym_ofs_reg(A_LEA,S_L,s,0,R_A0);
                              emit_reg_ref(A_MOVE,S_L,R_A0,newreference(p^.location.reference));
                           end;
                         ungetregister32(R_A0);
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
                    if tf_static_a5_based in target_info.flags then
                      p^.location.reference.base:=R_A5;
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
         saved : tpushed;

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

         if not(p^.left^.location.loc in [LOC_REFERENCE,LOC_CREGISTER]) then
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
                          emit_ref(A_CLR,S_B,newreference(p^.left^.location.reference));
                          del_reference(p^.left^.location.reference);
                        end
                      else
                        loadansi2short(p^.right,p^.left);
                    end
                  else
                    begin
                       { we do not need destination anymore
                       del_reference(p^.left^.location.reference);
                       del_reference(p^.right^.location.reference);
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
                              else
                                internalerror(20021106);
                              end;
                              if loc=LOC_CREGISTER then
                                begin
                                  if  (p^.left^.resulttype^.size=8) then
                                    begin
                                       r:=newreference(p^.right^.location.reference);
                                       emit_ref_reg(A_MOVE,opsize,r,p^.left^.location.registerhigh);
                                       r:=newreference(p^.right^.location.reference);
                                       inc(r^.offset,4);
                                       emit_ref_reg(A_MOVE,opsize,r,p^.left^.location.registerlow);
                                    end
                                  else
                                   emit_ref_reg(A_MOVE,opsize,
                                    newreference(p^.right^.location.reference),
                                    p^.left^.location.register);
                                end
                              else
                                begin
                                  if (p^.left^.resulttype^.size=8) then
                                    begin
                                       r:=newreference(p^.left^.location.reference);
                                       emit_ref(A_CLR,opsize,r);
                                       r:=newreference(p^.left^.location.reference);
                                       inc(r^.offset,4);
                                       emit_const_ref(A_MOVE,opsize,
                                        p^.right^.location.reference.offset,
                                        r);
                                    end
                                  else
                                   emit_const_ref(A_MOVE,opsize,
                                    p^.right^.location.reference.offset,
                                    newreference(p^.left^.location.reference));
                                end;

                           end
(*           Not supported on m68k
                        else if loc=LOC_CFPUREGISTER then
                           begin
                              floatstore(pfloatdef(p^.right^.resulttype)^.typ,
                                p^.right^.location,
                              floatloadops(pfloatdef(p^.right^.resulttype)^.typ,op,opsize);
                              emit_ref(op,opsize,
                                newreference(p^.right^.location.reference));
                              emit_reg(A_FSTP,S_NO,
                                correct_fpuregister(p^.left^.location.register,fpuvaroffset+1));}
                           end*)
                         else
                           begin
                              if (p^.right^.resulttype^.needs_inittable) and
                                ( (p^.right^.resulttype^.deftype<>objectdef) or
                                  not(pobjectdef(p^.right^.resulttype)^.is_class)) then
                                begin
                                   { this would be a problem }
                                   if not(p^.left^.resulttype^.needs_inittable) then
                                     internalerror(3457);

                                   { let us save all used registers }
                                   { compare with i386 where this is done }
                                   { in the routine itself.               }
                                   saveusedregisters(saved, ALL_REGISTERS);
                                   { increment source reference counter }
                                   new(r);
                                   reset_reference(r^);
                                   r^.symbol:=p^.right^.resulttype^.get_inittable_label;
                                   emitpushreferenceaddr(r^);

                                   emitpushreferenceaddr(p^.right^.location.reference);
                                   emitcall('FPC_ADDREF');
                                   restoreusedregisters(saved);
                                   { decrement destination reference counter }
                                   r^.symbol:=p^.left^.resulttype^.get_inittable_label;
                                   emitpushreferenceaddr(r^);
                                   dispose(r);
                                   emitpushreferenceaddr(p^.left^.location.reference);
                                   emitcall('FPC_DECREF');
                                end;

                              concatcopy(p^.right^.location.reference,
                                p^.left^.location.reference,p^.left^.resulttype^.size,false,false);
                              ungetiftemp(p^.right^.location.reference);
                           end;
                      end;
            LOC_REGISTER,
            LOC_CREGISTER : begin
                              case p^.right^.resulttype^.size of
                                 1 : opsize:=S_B;
                                 2 : opsize:=S_W;
                                 4 : opsize:=S_L;
                                 8 : opsize:=S_L;
                              else
                                internalerror(20021106);
                              end;
                              if is_64bitint(p^.right^.resulttype) then
                                begin
                                   { simplified with op_reg_loc  }
                                   if loc=LOC_CREGISTER then
                                     begin
                                       emit_reg_reg(A_MOVE,opsize,
                                         p^.right^.location.registerhigh,
                                         p^.left^.location.registerhigh);
                                       emit_reg_reg(A_MOVE,opsize,
                                         p^.right^.location.registerlow,
                                         p^.left^.location.registerlow);
                                     end
                                   else
                                     begin
                                        r:=newreference(p^.left^.location.reference);
                                        emit_reg_ref(A_MOVE,opsize,
                                          p^.right^.location.registerhigh,r);
                                        r:=newreference(p^.left^.location.reference);
                                        inc(r^.offset,4);
                                        emit_reg_ref(A_MOVE,opsize,
                                          p^.right^.location.registerlow,r);
                                     end;
                                end
                              { simplified with op_reg_loc       }
                              else if loc=LOC_CREGISTER then
                                begin
                                  emit_reg_reg(A_MOVE,opsize,
                                    p^.right^.location.register,
                                    p^.left^.location.register);
                                 ungetregister(p^.right^.location.register);
                                end
                              else
                                Begin
                                  emit_reg_ref(A_MOVE,opsize,
                                    p^.right^.location.register,
                                    newreference(p^.left^.location.reference));
                                  ungetregister(p^.right^.location.register);
                                end;
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
                              if loc = LOC_REFERENCE then
                                   floatstore(fputyp,p^.right^.location,p^.left^.location.reference)
                              else
                                internalerror(48991);
                           end;
            LOC_JUMP     : begin
                              getlabel(hlabel);
                              emitlab(truelabel);
                              if pushed then
                                restore(p^.left,false);
                              case p^.left^.resulttype^.size of
                                 1 : opsize:=S_B;
                                 2 : opsize:=S_W;
                                 4 : opsize:=S_L;
                                 { S_L is correct, the copy is done }
                                 { with two moves                   }
                                 8 : opsize:=S_L;
                              else
                                internalerror(20021106);
                              end;
                              if (p^.left^.resulttype^.size=8) then
                                if loc=LOC_REGISTER then
                                  emit_reg(A_CLR,S_L,p^.left^.location.registerhigh)
                                else
                                  begin
                                    r:=newreference(p^.left^.location.reference);
                                    inc(r^.offset,4);
                                    emit_ref(A_CLR,opsize,r);
                                  end;
                              if loc=LOC_CREGISTER then
                                emit_const_reg(A_MOVE,opsize,1,p^.left^.location.register)
                              else
                                emit_const_ref(A_MOVE,opsize,1,
                                   newreference(p^.left^.location.reference));
                              emitjmp(C_None,hlabel);
                              emitlab(falselabel);
                              if pushed then
                                restore(p^.left,false);
                              if loc=LOC_CREGISTER then
                                emit_reg(A_CLR,opsize,p^.left^.location.register)
                              else
                                begin
                                  emit_ref(A_CLR,opsize,
                                    newreference(p^.left^.location.reference));
                                 end;
                              emitlab(hlabel);
                           end;
            LOC_FLAGS    : begin
                             if loc=LOC_CREGISTER then
                                begin
                                  emit_flag2reg(p^.right^.location.resflags,p^.left^.location.register);
                                  emit_const_reg(A_AND,S_L,$1,p^.left^.location.register);
                                  if (p^.left^.resulttype^.size=8) then
                                    emit_reg(A_CLR,S_L,p^.left^.location.registerhigh);
                                end
                              else
                                begin
                                 emit_ref(flag_2_set[p^.right^.location.resflags],
                                     S_B,newreference(p^.left^.location.reference));
                                 { emit_ref(A_NEG,S_B,newreference(p^.left^.location.reference));
                                   why ???? }
                                 case p^.left^.resulttype^.size of
                                    1 : opsize:=S_B;
                                    2 : opsize:=S_W;
                                    4 : opsize:=S_L;
                                    { FIXME: several things are still worng here for 64 bit integers   }
                                    8 : opsize:=S_L;
                                 else
                                   internalerror(20021106);
                                 end;
                                 emit_const_ref(A_AND,opsize,$1,newreference(p^.left^.location.reference));
                                 if (p^.left^.resulttype^.size=8) then
                                   begin
                                     r:=newreference(p^.left^.location.reference);
                                     inc(r^.offset,4);
                                     emit_ref(A_CLR,S_L,r);
                                   end;
                                end;
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
        copydone,
        dovariant : boolean;
        elesize : longint;
        hreg : tregister;
        opsize : topsize;
      begin
        dovariant:=p^.forcevaria or parraydef(p^.resulttype)^.isvariant;
        if dovariant then
         elesize:=8
        else
         begin
           elesize:=parraydef(p^.resulttype)^.elesize;
           {if elesize>4 then
            internalerror(8765678); }
         end;
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
{ INLINED : }
{                       emit_lea_loc_ref(hp^.left^.location,href,freetemp);}
                        case hp^.left^.location.loc of
                          LOC_MEM,
                          LOC_REFERENCE :
                             Begin
                               if hp^.left^.location.reference.is_immediate then
                                  internalerror(331)
                               else
                                begin
                                  hreg := getaddressreg;
                                  emit_ref_reg(A_LEA,S_L,
                                     newreference(hp^.left^.location.reference),hreg);
                                  emit_reg_ref(A_MOVE,S_L,hreg,newreference(href));
                                  ungetregister(hreg);
                                end;
                                { release the registers }
                                del_reference(hp^.left^.location.reference);
                                if freetemp then
                                  ungetiftemp(hp^.left^.location.reference);
                             end;
                         else
                            internalerror(332);
                         end; { end case }

                     end
                    else
                     begin
{                       emit_mov_loc_ref(hp^.left^.location,href,S_L,freetemp);}
{ INLINED:                                                                     }
                       case hp^.left^.location.loc of
                          LOC_REGISTER,
                          LOC_CREGISTER :
                             begin
                             { the register is not needed anymore }
                               emit_reg_ref(A_MOVE,S_L,
                                   hp^.left^.location.register,newreference(href));
                               ungetregister32(hp^.left^.location.register);
                             end;
                         LOC_MEM,
                         LOC_REFERENCE :
                             begin
                               if hp^.left^.location.reference.is_immediate then
                                 emit_const_ref(A_MOVE,S_L,
                                  hp^.left^.location.reference.offset,newreference(href))
                               else
                                 begin
                                   hreg := getexplicitregister32(accumulator);
                                   emit_ref_reg(A_MOVE,S_L,
                                     newreference(hp^.left^.location.reference),hreg);
                                   del_reference(hp^.left^.location.reference);
                                   emit_reg_ref(A_MOVE,S_L,
                                     hreg,newreference(href));
                                   del_reference(href);
                                   ungetregister(accumulator);
                                 end;
                               if freetemp then
                                 ungetiftemp(hp^.left^.location.reference);
                             end;
                         else
                          internalerror(330);
                       end; { end case }

                     end;
                    { update href to the vtype field and write it }
                    dec(href.offset,4);
                    emit_const_ref(A_MOVE,S_L,vtype,newreference(href));
                    { goto next array element }
                    inc(href.offset,8);
                  end;
               end
              else
              { normal array constructor of the same type }
               begin
{                 case elesize of
                   1 :
                     emit_mov_loc_ref(hp^.left^.location,href,S_B,freetemp);
                   2 :
                     emit_mov_loc_ref(hp^.left^.location,href,S_W,freetemp);
                   4 :
                     emit_mov_loc_ref(hp^.left^.location,href,S_L,freetemp);
                   else
                     internalerror(87656781);
                 end;}
                 copydone:=false;
                 case elesize of
                  1 : opsize := S_B;
                  2 : opsize := S_W;
                  4 : opsize := S_L;
                  8 :
                    begin
                      if hp^.left^.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                       begin
                         emit_reg_ref(A_MOVE,S_L,hp^.left^.location.registerlow,newreference(href));
                         { update href to the high bytes and write it }
                         inc(href.offset,4);
                         emit_reg_ref(A_MOVE,S_L,hp^.left^.location.registerhigh,newreference(href));
                         dec(href.offset,4)
                       end
                      else
                       begin
                         concatcopy(hp^.left^.location.reference,href,elesize,freetemp,false);
                         if freetemp then
                           ungetiftemp(hp^.left^.location.reference);
                       end;
                      copydone:=true;
                    end;
                  else
                    begin
                      { concatcopy only supports reference }
                      if not(hp^.left^.location.loc in [LOC_MEM,LOC_REFERENCE]) then
                       internalerror(200108012);
                      concatcopy(hp^.left^.location.reference,href,elesize,freetemp,false);
                      if freetemp then
                        ungetiftemp(hp^.left^.location.reference);
                      copydone:=true;
                    end;
                 (* else
                     internalerror(87656781); *)
                 end;
{ INLINED:  }

                 if not copydone then
                   case hp^.left^.location.loc of
                     LOC_REGISTER,
                     LOC_CREGISTER :
                      begin
                        { the register is not needed anymore }
                        emit_reg_ref(A_MOVE,opsize,
                           hp^.left^.location.register,newreference(href));
                        ungetregister32(hp^.left^.location.register);
                      end;
                     LOC_MEM,
                     LOC_REFERENCE :
                      begin
                       if hp^.left^.location.reference.is_immediate then
                         emit_const_ref(A_MOVE,opsize,
                           hp^.left^.location.reference.offset,newreference(href))
                       else
                         begin
                           hreg := getexplicitregister32(accumulator);
                           emit_ref_reg(A_MOVE,opsize,
                            newreference(hp^.left^.location.reference),hreg);
                           del_reference(hp^.left^.location.reference);
                           emit_reg_ref(A_MOVE,opsize,
                            hreg,newreference(href));
                           del_reference(href);
                           ungetregister(accumulator);
                         end;
                       if freetemp then
                         ungetiftemp(hp^.left^.location.reference);
                      end;
                     else
                      internalerror(330);
                   end; { end case }
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
  Revision 1.1.2.23  2002/11/12 15:06:27  pierre
   * ensure that all const/value parameters are pushed directly for cdecl'ared procedures

  Revision 1.1.2.22  2002/11/07 22:32:25  pierre
   * correct cdecl check for array of const parameter

  Revision 1.1.2.21  2002/11/07 12:13:32  pierre
   * remove memory leak when calling FPC_ADDREF

  Revision 1.1.2.20  2002/11/07 09:00:45  pierre
   * cdecl array of const is not indirect

  Revision 1.1.2.19  2002/11/06 10:26:44  pierre
   * added a size check in secondassignment

  Revision 1.1.2.18  2002/11/05 15:36:42  pierre
   * fix C ellipses parameters for floats and 64bit integers

  Revision 1.1.2.17  2002/09/20 07:56:38  pierre
   + support any size for arraytoconstruct if not variable

  Revision 1.1.2.16  2002/09/12 19:52:09  carl
    * (m68k): bugfixes for 64-bit arithmetic, 64-bit shifts and 64-bit division
    * (m68k): bugfix for boolean type conversions
    * (m68k): endian problems fixes when src_Size = dst_Size with a different type

  Revision 1.1.2.15  2001/10/28 17:18:14  peter
    * allow assignment of overloaded procedures to procvars when we know
      which procedure to take

  Revision 1.1.2.14  2001/09/14 15:37:41  pierre
   * more int64 fixes

  Revision 1.1.2.13  2001/08/31 02:15:53  carl
  * fix from i386 commit

  Revision 1.1.2.12  2001/08/17 16:16:51  florian
    + support for PalmOS added

  Revision 1.1.2.11  2001/08/05 15:06:56  carl
  * fix for proc of obj=nil

  Revision 1.1.2.10  2001/08/03 08:19:18  pierre
   * fix a wrong pea with second arg

  Revision 1.1.2.9  2001/08/02 13:58:21  pierre
   * convert all move.l symbol into lea sym, probably just bug in aggas...

  Revision 1.1.2.8  2001/07/29 20:35:28  pierre
   * improve fpu handling

  Revision 1.1.2.7  2001/07/26 15:18:37  pierre
   * fix LOC_JUMP and 64bit problems

  Revision 1.1.2.6  2001/07/26 03:08:47  carl
  * LOC_JUMP in secondassignment() was not setting correct values (was always a byte!)

  Revision 1.1.2.5  2001/07/25 12:59:58  pierre
   * simple comment change

  Revision 1.1.2.4  2001/05/21 16:51:13  carl
  * saveregisters added

  Revision 1.1.2.3  2001/04/19 11:37:36  carl
  * m68k updates (code generator compiles)

  Revision 1.1.2.2  2001/04/02 02:20:55  carl
  + ported


}
