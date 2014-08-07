{
    $Id: cgcal.pas,v 1.1.2.27 2003/01/22 00:23:56 pierre Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate i386 assembler for in call nodes

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published bymethodpointer
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
{$ifdef FPC}
  {$goto on}
{$endif FPC}
unit cgcal;
interface

{ $define AnsiStrRef}


    uses
      symtable,tree;

    procedure secondcallparan(var p : ptree;defcoll : pparaitem;
                push_from_left_to_right,inlined,is_cdecl : boolean;para_alignment,para_offset : longint);
    procedure secondcalln(var p : ptree);
    procedure secondprocinline(var p : ptree);


implementation

    uses
      globtype,systems,
      cobjects,verbose,globals,
      symconst,aasm,types,
{$ifdef GDB}
      strings,gdb,
{$endif GDB}
      hcodegen,temp_gen,pass_2,
      cpubase,cpuasm,
      cga,tgen,cgld;

{*****************************************************************************
                             SecondCallParaN
*****************************************************************************}

    procedure secondcallparan(var p : ptree;defcoll : pparaitem;
                push_from_left_to_right,inlined,is_cdecl : boolean;para_alignment,para_offset : longint);

      procedure maybe_push_high;
        begin
           { open array ? }
           { defcoll^.data can be nil for read/write }
           if assigned(defcoll^.paratype.def) and
              assigned(p^.hightree) then
            begin
              secondpass(p^.hightree);
              { this is a longint anyway ! }
              push_value_para(p^.hightree,inlined,false,para_offset,4);
            end;
        end;

      var
         otlabel,oflabel : pasmlabel;
         { temporary variables: }
         tempdeftype : tdeftype;
         r : preference;
      begin
         { set default para_alignment to target_os.stackalignment }
         if para_alignment=0 then
          para_alignment:=target_os.stackalignment;

         { push from left to right if specified }
         if push_from_left_to_right and assigned(p^.right) then
           secondcallparan(p^.right,pparaitem(defcoll^.next),push_from_left_to_right,
             inlined,is_cdecl,para_alignment,para_offset);
         otlabel:=truelabel;
         oflabel:=falselabel;
         getlabel(truelabel);
         getlabel(falselabel);
         secondpass(p^.left);
         { filter array constructor with c styled args }
         if is_array_constructor(p^.left^.resulttype) and p^.left^.cargs then
           begin
             { nothing, everything is already pushed }
           end
         { in codegen.handleread.. defcoll^.data is set to nil }
         else if assigned(defcoll^.paratype.def) and
           (defcoll^.paratype.def^.deftype=formaldef) then
           begin
              { allow @var }
              inc(pushedparasize,4);
              if (p^.left^.treetype=addrn) and
                 (not p^.left^.procvarload) then
                begin
                { always a register }
                  if inlined then
                    begin
                       r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                       emit_reg_ref(A_MOV,S_L,
                         p^.left^.location.register,r);
                    end
                  else
                    emit_reg(A_PUSH,S_L,p^.left^.location.register);
                  ungetregister32(p^.left^.location.register);
                end
              else
                begin
                   if not(p^.left^.location.loc in [LOC_MEM,LOC_REFERENCE]) then
                     CGMessage(type_e_mismatch)
                   else
                     begin
                       if inlined then
                         begin
{$ifndef noAllocEdi}
                           getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                           emit_ref_reg(A_LEA,S_L,
                             newreference(p^.left^.location.reference),R_EDI);
                           r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                           emit_reg_ref(A_MOV,S_L,R_EDI,r);
{$ifndef noAllocEdi}
                           ungetregister32(R_EDI);
{$endif noAllocEdi}
                         end
                      else
                        emitpushreferenceaddr(p^.left^.location.reference);
                        del_reference(p^.left^.location.reference);
                     end;
                end;
           end
         { handle call by reference parameter }
         else if (defcoll^.paratyp=vs_var)  or
                 (is_cdecl and (defcoll^.paratype.def^.deftype=arraydef)) then
           begin
              if not(p^.left^.location.loc in [LOC_REFERENCE,LOC_MEM]) then
                emit_to_mem(p^.left);
              if not push_from_left_to_right then
                maybe_push_high;
              inc(pushedparasize,4);
              if inlined then
                begin
{$ifndef noAllocEdi}
                   getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                   emit_ref_reg(A_LEA,S_L,
                     newreference(p^.left^.location.reference),R_EDI);
                   r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                   emit_reg_ref(A_MOV,S_L,R_EDI,r);
{$ifndef noAllocEdi}
                   ungetregister32(R_EDI);
{$endif noAllocEdi}
                end
              else
                emitpushreferenceaddr(p^.left^.location.reference);
              del_reference(p^.left^.location.reference);
              if push_from_left_to_right then
                maybe_push_high;
           end
         else
           begin
              tempdeftype:=p^.resulttype^.deftype;
              if tempdeftype=filedef then
               CGMessage(cg_e_file_must_call_by_reference);
              { open array must always push the address, this is needed to
                also push addr of small open arrays and with cdecl functions (PFV) }
              if (
                  assigned(defcoll^.paratype.def) and
                  (is_open_array(defcoll^.paratype.def) or
                   is_array_of_const(defcoll^.paratype.def))
                 ) or
                 (
                  push_addr_param(p^.resulttype) and
                  not is_cdecl
                 ) then
                begin
                   if not push_from_left_to_right then
                     maybe_push_high;
                   inc(pushedparasize,4);
                   if inlined then
                     begin
{$ifndef noAllocEdi}
                        getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                        emit_ref_reg(A_LEA,S_L,
                          newreference(p^.left^.location.reference),R_EDI);
                        r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                        emit_reg_ref(A_MOV,S_L,R_EDI,r);
{$ifndef noAllocEdi}
                        ungetregister32(R_EDI);
{$endif noAllocEdi}
                     end
                   else
                     emitpushreferenceaddr(p^.left^.location.reference);
                   del_reference(p^.left^.location.reference);
                   if push_from_left_to_right then
                     maybe_push_high;
                end
              else
                begin
                   push_value_para(p^.left,inlined,is_cdecl,
                     para_offset,para_alignment);
                end;
           end;
         truelabel:=otlabel;
         falselabel:=oflabel;
         { push from right to left }
         if not push_from_left_to_right and assigned(p^.right) then
           secondcallparan(p^.right,pparaitem(defcoll^.next),push_from_left_to_right,
             inlined,is_cdecl,para_alignment,para_offset);
      end;


{*****************************************************************************
                             SecondCallN
*****************************************************************************}

    procedure secondcalln(var p : ptree);
      var
         unusedregisters : tregisterset;
         usablecount : byte;
         pushed : tpushed;
         hr,funcretref : treference;
         hregister,hregister2 : tregister;
         oldpushedparasize : longint;
         { true if ESI must be loaded again after the subroutine }
         loadesi : boolean;
         { true if a virtual method must be called directly }
         no_virtual_call : boolean;
         { true if we produce a con- or destrutor in a call }
         is_con_or_destructor : boolean;
         { true if a constructor is called again }
         extended_new : boolean;
         { adress returned from an I/O-error }
         iolabel : pasmlabel;
         { lexlevel count }
         i : longint;
         { help reference pointer }
         r : preference;
         hp,
         pp,params : ptree;
         inlined : boolean;
         inlinecode : ptree;
         store_parast_fixup : longint;
         para_alignment,
         para_offset : longint;
         { instruction for alignement correction }
{        corr : paicpu;}
         { we must pop this size also after !! }
{        must_pop : boolean; }
         pop_size : longint;
         pop_allowed : boolean;
{$ifdef OPTALIGN}
         pop_esp : boolean;
         push_size : longint;
{$endif OPTALIGN}

      procedure MaybeTestSelf;

      var
        OKLabel : pasmlabel;

        begin
          if (cs_check_object_ext in aktlocalswitches) or
             (cs_check_range in aktlocalswitches) then
            begin
              emit_reg_reg(A_TEST,S_L,R_ESI,R_ESI);
              GetLabel(OKLabel);
              emitjmp(C_NE,OKLabel);
              emit_const(A_PUSH,S_L,210);
              emitcall('FPC_HANDLEERROR');
              emitlab(OKLabel);
            end;
        end;

      procedure MaybeCheckVMT(reg : tregister);
        begin
          if (cs_check_object_ext in aktlocalswitches) then
            begin
              emit_sym(A_PUSH,S_L,
                newasmsymbol(pprocdef(p^.procdefinition)^._class^.vmt_mangledname));
              emit_reg(A_PUSH,S_L,reg);
              emitcall('FPC_CHECK_OBJECT_EXT');
            end
          else if (cs_check_range in aktlocalswitches) then
            begin
              emit_reg(A_PUSH,S_L,reg);
              emitcall('FPC_CHECK_OBJECT');
            end;
        end;

      label
         dont_call;

      begin
         reset_reference(p^.location.reference);
         extended_new:=false;
         iolabel:=nil;
         inlinecode:=nil;
         inlined:=false;
         loadesi:=true;
         no_virtual_call:=false;
         unusedregisters:=unused;
         usablecount:=usablereg32;

         if (pocall_cdecl in p^.procdefinition^.proccalloptions) or
            (pocall_stdcall in p^.procdefinition^.proccalloptions) then
          para_alignment:=4
         else
          para_alignment:=target_os.stackalignment;

         if not assigned(p^.procdefinition) then
          exit;

         { Only work on a copy of the parameters, because they are sometimes }
         { changed (??) and because of that you get problems when inlining   }
         { procedure calls (JM)                                              }
         params:=getcopy(p^.left);

         if (pocall_inline in p^.procdefinition^.proccalloptions) then
           begin
              inlined:=true;
              inlinecode:=p^.right;
              p^.right:=nil;
              { set it to the same lexical level as the local symtable, becuase
                the para's are stored there }
              pprocdef(p^.procdefinition)^.parast^.symtablelevel:=aktprocsym^.definition^.localst^.symtablelevel;
              if assigned(params) then
                begin
                  inlinecode^.para_size:=pprocdef(p^.procdefinition)^.para_size(para_alignment);
                  inlinecode^.para_offset:=gettempofsizepersistant(inlinecode^.para_size);
                end;
              store_parast_fixup:=pprocdef(p^.procdefinition)^.parast^.address_fixup;
              pprocdef(p^.procdefinition)^.parast^.address_fixup:=inlinecode^.para_offset;
{$ifdef extdebug}
             Comment(V_debug,
                'inlined parasymtable is at offset '
               +tostr(pprocdef(p^.procdefinition)^.parast^.address_fixup));
             exprasmlist^.concat(new(pai_asm_comment,init(
               strpnew('inlined parasymtable is at offset '
               +tostr(pprocdef(p^.procdefinition)^.parast^.address_fixup)))));
{$endif extdebug}
              { disable further inlining of the same proc
                in the args }
(* {$ifdef INCLUDEOK}
              exclude(p^.procdefinition^.proccalloptions,pocall_inline);
{$else}
              p^.procdefinition^.proccalloptions:=p^.procdefinition^.proccalloptions-[pocall_inline];
{$endif} *)
           end;
         { only if no proc var }
         if inlined or
            not(assigned(p^.right)) then
           is_con_or_destructor:=(p^.procdefinition^.proctypeoption in [potype_constructor,potype_destructor]);
         { proc variables destroy all registers }
         if (inlined or
            (p^.right=nil)) and
            { virtual methods too }
            not(po_virtualmethod in p^.procdefinition^.procoptions) then
           begin
              if (cs_check_io in aktlocalswitches) and
                 (po_iocheck in p^.procdefinition^.procoptions) and
                 not(po_iocheck in aktprocsym^.definition^.procoptions) then
                begin
                   getlabel(iolabel);
                   emitlab(iolabel);
                end
              else
                iolabel:=nil;

              { save all used registers }
              pushusedregisters(pushed,pprocdef(p^.procdefinition)^.usedregisters);

              { give used registers through }
              usedinproc:=usedinproc or pprocdef(p^.procdefinition)^.usedregisters;
           end
         else
           begin
              pushusedregisters(pushed,$ff);
              usedinproc:=$ff;
              { no IO check for methods and procedure variables }
              iolabel:=nil;
           end;

         { generate the code for the parameter and push them }
         oldpushedparasize:=pushedparasize;
         pushedparasize:=0;
         pop_size:=0;
         { no inc esp for inlined procedure
           and for objects constructors PM }
         if inlined or
            ((p^.procdefinition^.proctypeoption=potype_constructor) and
            { quick'n'dirty check if it is a class or an object }
            (p^.resulttype^.deftype=orddef)) then
           pop_allowed:=false
         else
           pop_allowed:=true;
         if pop_allowed then
          begin
          { Old pushedsize aligned on 4 ? }
            i:=oldpushedparasize and 3;
            if i>0 then
             inc(pop_size,4-i);
          { This parasize aligned on 4 ? }
            i:=p^.procdefinition^.para_size(para_alignment) and 3;
            if i>0 then
             inc(pop_size,4-i);
          { insert the opcode and update pushedparasize }
          { never push 4 or more !! }
            pop_size:=pop_size mod 4;
            if pop_size>0 then
             begin
               inc(pushedparasize,pop_size);
               emit_const_reg(A_SUB,S_L,pop_size,R_ESP);
{$ifdef GDB}
               if (cs_debuginfo in aktmoduleswitches) and
                  (exprasmlist^.first=exprasmlist^.last) then
                 exprasmlist^.concat(new(pai_force_line,init));
{$endif GDB}
             end;
          end;
{$ifdef OPTALIGN}
         if pop_allowed and (cs_align in aktglobalswitches) then
           begin
              pop_esp:=true;
              push_size:=p^.procdefinition^.para_size(para_alignment);
              { !!!! here we have to take care of return type, self
                and nested procedures
              }
              inc(push_size,12);
              emit_reg_reg(A_MOV,S_L,R_ESP,R_EDI);
              if (push_size mod 8)=0 then
                emit_const_reg(A_AND,S_L,$fffffff8,R_ESP)
              else
                begin
                   emit_const_reg(A_SUB,S_L,push_size,R_ESP);
                   emit_const_reg(A_AND,S_L,$fffffff8,R_ESP);
                   emit_const_reg(A_SUB,S_L,push_size,R_ESP);
                end;
              emit_reg(A_PUSH,S_L,R_EDI);
           end
         else
           pop_esp:=false;
{$endif OPTALIGN}
         if (p^.resulttype<>pdef(voiddef)) and
            ret_in_param(p^.resulttype,p^.procdefinition^.proccalloptions) then
           begin
              funcretref.symbol:=nil;
{$ifdef test_dest_loc}
              if dest_loc_known and (dest_loc_tree=p) and
                 (dest_loc.loc in [LOC_REFERENCE,LOC_MEM]) then
                begin
                   funcretref:=dest_loc.reference;
                   if assigned(dest_loc.reference.symbol) then
                     funcretref.symbol:=stringdup(dest_loc.reference.symbol^);
                   in_dest_loc:=true;
                end
              else
{$endif test_dest_loc}
                if inlined then
                  begin
                     reset_reference(funcretref);
                     funcretref.offset:=gettempofsizepersistant(p^.procdefinition^.rettype.def^.size);
                     funcretref.base:=procinfo^.framepointer;
{$ifdef extdebug}
             Comment(V_debug,
                'function return value is at offset '
               +tostr(funcretref.offset));
             exprasmlist^.concat(new(pai_asm_comment,init(
               strpnew('function return value is at offset '
               +tostr(funcretref.offset)))));
{$endif extdebug}
                  end
                else
                  gettempofsizereference(p^.procdefinition^.rettype.def^.size,funcretref);
           end;
         if assigned(params) then
           begin
              { be found elsewhere }
              if inlined then
                para_offset:=pprocdef(p^.procdefinition)^.parast^.address_fixup+
                  pprocdef(p^.procdefinition)^.parast^.datasize
              else
                para_offset:=0;
              if not(inlined) and
                 assigned(p^.right) then
                secondcallparan(params,pparaitem(pabstractprocdef(p^.right^.resulttype)^.para^.first),
                  (pocall_leftright in p^.procdefinition^.proccalloptions),inlined,
                  (pocall_cdecl in p^.procdefinition^.proccalloptions),
                  para_alignment,para_offset)
              else
                secondcallparan(params,pparaitem(p^.procdefinition^.para^.first),
                  (pocall_leftright in p^.procdefinition^.proccalloptions),inlined,
                  (pocall_cdecl in p^.procdefinition^.proccalloptions),
                  para_alignment,para_offset);
           end;
         if inlined and (p^.resulttype<>pdef(voiddef)) then
           inlinecode^.retoffset:=gettempofsizepersistant(Align(p^.resulttype^.size,target_os.stackalignment));
         if ret_in_param(p^.resulttype,p^.procdefinition^.proccalloptions) then
           begin
              { This must not be counted for C code
                complex return address is removed from stack
                by function itself !   }
{$ifdef OLD_C_STACK}
              inc(pushedparasize,4); { lets try without it PM }
{$endif not OLD_C_STACK}
              if inlined then
                begin
                   inlinecode^.retoffset:=gettempofsizepersistant(Align(p^.resulttype^.size,target_os.stackalignment));
{$ifndef noAllocEdi}
                   getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                   emit_ref_reg(A_LEA,S_L,
                     newreference(funcretref),R_EDI);
                   r:=new_reference(procinfo^.framepointer,inlinecode^.retoffset);
                   emit_reg_ref(A_MOV,S_L,R_EDI,r);
{$ifndef noAllocEdi}
                   ungetregister32(R_EDI);
{$endif noAllocEdi}
                end
              else
                emitpushreferenceaddr(funcretref);
           end;
         { procedure variable ? }
         if inlined or
           (p^.right=nil) then
           begin
              { overloaded operator have no symtable }
              { push self }
              if assigned(p^.symtable) and
                (p^.symtable^.symtabletype=withsymtable) then
                begin
                   { dirty trick to avoid the secondcall below }
                   p^.methodpointer:=genzeronode(callparan);
                   p^.methodpointer^.location.loc:=LOC_REGISTER;
{$ifndef noAllocEDI}
                   getexplicitregister32(R_ESI);
{$endif noAllocEDI}
                   p^.methodpointer^.location.register:=R_ESI;
                   { ARGHHH this is wrong !!!
                     if we can init from base class for a child
                     class that the wrong VMT will be
                     transfered to constructor !! }
                   p^.methodpointer^.resulttype:=
                     ptree(pwithsymtable(p^.symtable)^.withnode)^.left^.resulttype;
                   { change dispose type !! }
                   p^.disposetyp:=dt_mbleft_and_method;
                   { make a reference }
                   new(r);
                   reset_reference(r^);
                   { if assigned(ptree(pwithsymtable(p^.symtable)^.withnode)^.pref) then
                     begin
                        r^:=ptree(pwithsymtable(p^.symtable)^.withnode)^.pref^;
                     end
                   else
                     begin
                        r^.offset:=p^.symtable^.datasize;
                        r^.base:=procinfo^.framepointer;
                     end; }
                   r^:=ptree(pwithsymtable(p^.symtable)^.withnode)^.withreference^;
                   if ((not ptree(pwithsymtable(p^.symtable)^.withnode)^.islocal) and
                       (not pwithsymtable(p^.symtable)^.direct_with)) or
                      pobjectdef(p^.methodpointer^.resulttype)^.is_class then
                     emit_ref_reg(A_MOV,S_L,r,R_ESI)
                   else
                     emit_ref_reg(A_LEA,S_L,r,R_ESI);
                end;

              { push self }
              if assigned(p^.symtable) and
                ((p^.symtable^.symtabletype=objectsymtable) or
                (p^.symtable^.symtabletype=withsymtable)) then
                begin
                   if assigned(p^.methodpointer) then
                     begin
                        {
                        if p^.methodpointer^.resulttype=classrefdef then
                          begin
                              two possibilities:
                               1. constructor
                               2. class method

                          end
                        else }
                          begin
                             case p^.methodpointer^.treetype of
                               typen:
                                 begin
                                    { direct call to inherited method }
                                    if (po_abstractmethod in p^.procdefinition^.procoptions) then
                                      begin
                                         CGMessage(cg_e_cant_call_abstract_method);
                                         goto dont_call;
                                      end;
                                    { generate no virtual call }
                                    no_virtual_call:=true;

                                    if (sp_static in p^.symtableprocentry^.symoptions) then
                                      begin
                                         { well lets put the VMT address directly into ESI }
                                         { it is kind of dirty but that is the simplest    }
                                         { way to accept virtual static functions (PM)     }
                                         loadesi:=true;
                                         { if no VMT just use $0 bug0214 PM }
{$ifndef noAllocEDI}
                                         getexplicitregister32(R_ESI);
{$endif noAllocEDI}
                                         if not(oo_has_vmt in pobjectdef(p^.methodpointer^.resulttype)^.objectoptions) then
                                           emit_const_reg(A_MOV,S_L,0,R_ESI)
                                         else
                                           begin
                                             emit_sym_ofs_reg(A_MOV,S_L,
                                               newasmsymbol(pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname),
                                               0,R_ESI);
                                           end;
                                         { emit_reg(A_PUSH,S_L,R_ESI);
                                           this is done below !! }
                                      end
                                    else
                                      { this is a member call, so ESI isn't modfied }
                                      loadesi:=false;

                                    { a class destructor needs a flag }
                                    if pobjectdef(p^.methodpointer^.resulttype)^.is_class and
                                       {assigned(aktprocsym) and
                                       (aktprocsym^.definition^.proctypeoption=potype_destructor)}
                                       (p^.procdefinition^.proctypeoption=potype_destructor) then
                                      begin
                                        push_int(0);
                                        emit_reg(A_PUSH,S_L,R_ESI);
                                      end;

                                    if not(is_con_or_destructor and
                                           pobjectdef(p^.methodpointer^.resulttype)^.is_class and
                                           {assigned(aktprocsym) and
                                          (aktprocsym^.definition^.proctypeoption in [potype_constructor,potype_destructor])}
                                           (p^.procdefinition^.proctypeoption in [potype_constructor,potype_destructor])
                                          ) then
                                      emit_reg(A_PUSH,S_L,R_ESI);
                                    { if an inherited con- or destructor should be  }
                                    { called in a con- or destructor then a warning }
                                    { will be made                                  }
                                    { con- and destructors need a pointer to the vmt }
                                    if is_con_or_destructor and
                                    not(pobjectdef(p^.methodpointer^.resulttype)^.is_class) and
                                    assigned(aktprocsym) then
                                      begin
                                         if not(aktprocsym^.definition^.proctypeoption in
                                                [potype_constructor,potype_destructor]) then
                                          CGMessage(cg_w_member_cd_call_from_method);
                                      end;
                                    { class destructors get there flag above }
                                    { constructor flags ?                    }
                                    if is_con_or_destructor and
                                        not(pobjectdef(p^.methodpointer^.resulttype)^.is_class and
                                        assigned(aktprocsym) and
                                        (aktprocsym^.definition^.proctypeoption=potype_destructor)) then
                                      begin
                                         { a constructor needs also a flag }
                                         if pobjectdef(p^.methodpointer^.resulttype)^.is_class then
                                           push_int(0);
                                         push_int(0);
                                      end;
                                 end;
                               hnewn:
                                 begin
                                    { extended syntax of new }
                                    { ESI must be zero }
{$ifndef noAllocEDI}
                                    getexplicitregister32(R_ESI);
{$endif noAllocEDI}
                                    emit_reg_reg(A_XOR,S_L,R_ESI,R_ESI);
                                    emit_reg(A_PUSH,S_L,R_ESI);
                                    { insert the vmt }
                                    emit_sym(A_PUSH,S_L,
                                      newasmsymbol(pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname));
                                    extended_new:=true;
                                 end;
                               hdisposen:
                                 begin
                                    secondpass(p^.methodpointer);

                                    { destructor with extended syntax called from dispose }
                                    { hdisposen always deliver LOC_REFERENCE          }
{$ifndef noAllocEDI}
                                    getexplicitregister32(R_ESI);
{$endif noAllocEDI}
                                    emit_ref_reg(A_LEA,S_L,
                                      newreference(p^.methodpointer^.location.reference),R_ESI);
                                    del_reference(p^.methodpointer^.location.reference);
                                    emit_reg(A_PUSH,S_L,R_ESI);
                                    emit_sym(A_PUSH,S_L,
                                      newasmsymbol(pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname));
                                 end;
                               else
                                 begin
                                    { call to an instance member }
                                    if (p^.symtable^.symtabletype<>withsymtable) then
                                      begin
                                         secondpass(p^.methodpointer);
{$ifndef noAllocEDI}
                                         getexplicitregister32(R_ESI);
{$endif noAllocEDI}
                                         case p^.methodpointer^.location.loc of
                                            LOC_CREGISTER,
                                            LOC_REGISTER:
                                              begin
                                                 emit_reg_reg(A_MOV,S_L,p^.methodpointer^.location.register,R_ESI);
                                                 ungetregister32(p^.methodpointer^.location.register);
                                              end;
                                            else
                                              begin
                                                 if (p^.methodpointer^.resulttype^.deftype=classrefdef) or
                                                    ((p^.methodpointer^.resulttype^.deftype=objectdef) and
                                                   pobjectdef(p^.methodpointer^.resulttype)^.is_class) then
                                                   emit_ref_reg(A_MOV,S_L,
                                                     newreference(p^.methodpointer^.location.reference),R_ESI)
                                                 else
                                                   emit_ref_reg(A_LEA,S_L,
                                                     newreference(p^.methodpointer^.location.reference),R_ESI);
                                                 del_reference(p^.methodpointer^.location.reference);
                                              end;
                                         end;
                                      end;
                                    { when calling a class method, we have to load ESI with the VMT !
                                      But, not for a class method via self }
                                    if not(po_containsself in p^.procdefinition^.procoptions) then
                                      begin
                                        if ((po_classmethod in p^.procdefinition^.procoptions) and
                                           not(p^.methodpointer^.resulttype^.deftype=classrefdef)) or
                                           (po_staticmethod in p^.procdefinition^.procoptions) then
                                          begin
                                            if not(oo_has_vmt in pobjectdef(p^.symtableproc^.defowner)^.objectoptions) then
                                              emit_const_reg(A_MOV,S_L,0,R_ESI)
                                            else
                                              begin
                                                { class method needs current VMT }
                                                getexplicitregister32(R_ESI);
                                                new(r);
                                                reset_reference(r^);
                                                r^.base:=R_ESI;
                                                r^.offset:= pprocdef(p^.procdefinition)^._class^.vmt_offset;
                                                MaybeTestSelf;
                                                emit_ref_reg(A_MOV,S_L,r,R_ESI);
                                                MaybeCheckVMT(R_ESI);
                                              end;
                                          end;

                                        { direct call to destructor: remove data }
                                        if (p^.procdefinition^.proctypeoption=potype_destructor) and
                                           (p^.methodpointer^.resulttype^.deftype=objectdef) and
                                           (pobjectdef(p^.methodpointer^.resulttype)^.is_class) then
                                          emit_const(A_PUSH,S_L,1);

                                        { direct call to class constructor, don't allocate memory }
                                        if (p^.procdefinition^.proctypeoption=potype_constructor) and
                                           (p^.methodpointer^.resulttype^.deftype=objectdef) and
                                           (pobjectdef(p^.methodpointer^.resulttype)^.is_class) then
                                          begin
                                             emit_const(A_PUSH,S_L,0);
                                             emit_const(A_PUSH,S_L,0);
                                          end
                                        else
                                          begin
                                             { constructor call via classreference => allocate memory }
                                             if (p^.procdefinition^.proctypeoption=potype_constructor) and
                                                (p^.methodpointer^.resulttype^.deftype=classrefdef) and
                                                (pobjectdef(pclassrefdef(p^.methodpointer^.resulttype)^.
                                                   pointertype.def)^.is_class) then
                                                emit_const(A_PUSH,S_L,1);
                                             emit_reg(A_PUSH,S_L,R_ESI);
                                          end;
                                      end;

                                    if is_con_or_destructor then
                                      begin
                                         { classes don't get a VMT pointer pushed }
                                         if (p^.methodpointer^.resulttype^.deftype=objectdef) and
                                           not(pobjectdef(p^.methodpointer^.resulttype)^.is_class) then
                                           begin

                                              if (p^.procdefinition^.proctypeoption=potype_constructor) then
                                                begin
                                                   { it's no bad idea, to insert the VMT }
                                                   emit_sym(A_PUSH,S_L,newasmsymbol(
                                                     pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname));
                                                end
                                              { destructors haven't to dispose the instance, if this is }
                                              { a direct call                                           }
                                              else
                                                push_int(0);
                                           end;
                                      end;
                                 end;
                             end;
                          end;
                     end
                   else
                     begin
                        if ((po_classmethod in p^.procdefinition^.procoptions) and
                          not(
                            assigned(aktprocsym) and
                            (po_classmethod in aktprocsym^.definition^.procoptions)
                          )) or ((po_staticmethod in p^.procdefinition^.procoptions) and
                          not(
                            assigned(aktprocsym) and
                            (po_staticmethod in aktprocsym^.definition^.procoptions)
                          )) then
                          begin
                             if not(oo_has_vmt in pobjectdef(p^.symtableproc^.defowner)^.objectoptions) then
                               emit_const_reg(A_MOV,S_L,0,R_ESI)
                             else
                               begin
                                 { class method needs current VMT }
                                 getexplicitregister32(R_ESI);
                                 new(r);
                                 reset_reference(r^);
                                 r^.base:=R_ESI;
                                 r^.offset:= pprocdef(p^.procdefinition)^._class^.vmt_offset;
                                 MaybeTestSelf;
                                 emit_ref_reg(A_MOV,S_L,r,R_ESI);
                                 MaybeCheckVMT(R_ESI);
                               end;
                          end
                        else
                          begin
                             { member call, ESI isn't modified }
                             loadesi:=false;
                          end;
                        { direct call to destructor: don't remove data! }
                        if procinfo^._class^.is_class then
                          begin
                             if (p^.procdefinition^.proctypeoption=potype_destructor) then
                               begin
                                  emit_const(A_PUSH,S_L,0);
                                  emit_reg(A_PUSH,S_L,R_ESI);
                               end
                             else if (p^.procdefinition^.proctypeoption=potype_constructor) then
                               begin
                                  emit_const(A_PUSH,S_L,0);
                                  emit_const(A_PUSH,S_L,0);
                               end
                             else
                               emit_reg(A_PUSH,S_L,R_ESI);
                          end
                        else
                          begin
                             emit_reg(A_PUSH,S_L,R_ESI);
                             if is_con_or_destructor then
                               begin
                                  (*if (p^.procdefinition^.proctypeoption=potype_constructor) then
                                     begin
                                       { it's no bad idea, to insert the VMT }
                                       emit_sym(A_PUSH,S_L,newasmsymbol(
                                         procinfo^._class^.vmt_mangledname));
                                    end
                                  { destructors haven't to dispose the instance, if this is }
                                  { a direct call                                           }
                                  else *)
                                    push_int(0);
                               end;
                          end;
                     end;
                end;

              { push base pointer ?}
              { never when inlining, since if necessary, the base pointer }
              { can/will be gottten from the current procedure's symtable }
              { (JM)                                                      }
              if not inlined then
                if (lexlevel>=normal_function_level) and assigned(pprocdef(p^.procdefinition)^.parast) and
                  ((pprocdef(p^.procdefinition)^.parast^.symtablelevel)>normal_function_level) then
                  begin
                     { if we call a nested function in a method, we must      }
                     { push also SELF!                                    }
                     { THAT'S NOT TRUE, we have to load ESI via frame pointer }
                     { access                                              }
                     {
                       begin
                          loadesi:=false;
                          emit_reg(A_PUSH,S_L,R_ESI);
                       end;
                     }
                     if lexlevel=pprocdef(p^.procdefinition)^.parast^.symtablelevel then
                       begin
                          new(r);
                          reset_reference(r^);
                          r^.offset:=procinfo^.framepointer_offset;
                          r^.base:=procinfo^.framepointer;
                          emit_ref(A_PUSH,S_L,r)
                       end
                       { this is only true if the difference is one !!
                         but it cannot be more !! }
                     else if (lexlevel=(pprocdef(p^.procdefinition)^.parast^.symtablelevel)-1) then
                       begin
                          emit_reg(A_PUSH,S_L,procinfo^.framepointer)
                       end
                     else if (lexlevel>pprocdef(p^.procdefinition)^.parast^.symtablelevel) then
                       begin
                          hregister:=getregister32;
                          new(r);
                          reset_reference(r^);
                          r^.offset:=procinfo^.framepointer_offset;
                          r^.base:=procinfo^.framepointer;
                          emit_ref_reg(A_MOV,S_L,r,hregister);
                          for i:=(pprocdef(p^.procdefinition)^.parast^.symtablelevel) to lexlevel-1 do
                            begin
                               new(r);
                               reset_reference(r^);
                               {we should get the correct frame_pointer_offset at each level
                               how can we do this !!! }
                               r^.offset:=procinfo^.framepointer_offset;
                               r^.base:=hregister;
                               emit_ref_reg(A_MOV,S_L,r,hregister);
                            end;
                          emit_reg(A_PUSH,S_L,hregister);
                          ungetregister32(hregister);
                       end
                     else
                       internalerror(25000);
                  end;

              if (po_virtualmethod in p^.procdefinition^.procoptions) and
                 not(no_virtual_call) then
                begin
                   { static functions contain the vmt_address in ESI }
                   { also class methods                       }
                   { Here it is quite tricky because it also depends }
                   { on the methodpointer                        PM }
                   getexplicitregister32(R_ESI);
                   if assigned(aktprocsym) then
                     begin
                       if (((sp_static in aktprocsym^.symoptions) or
                        (po_classmethod in aktprocsym^.definition^.procoptions)) and
                        ((p^.methodpointer=nil) or (p^.methodpointer^.treetype=typen)))
                        or
                        (po_staticmethod in p^.procdefinition^.procoptions) or
                        ((p^.procdefinition^.proctypeoption=potype_constructor) and
                        { esi contains the vmt if we call a constructor via a class ref }
                         assigned(p^.methodpointer) and
                         (p^.methodpointer^.resulttype^.deftype=classrefdef)
                        ) or
                        { ESI is loaded earlier }
                        (po_classmethod in p^.procdefinition^.procoptions) then
                         begin
                            new(r);
                            reset_reference(r^);
                            r^.base:=R_ESI;
                         end
                       else
                         begin
                            new(r);
                            reset_reference(r^);
                            r^.base:=R_ESI;
                            { this is one point where we need vmt_offset (PM) }
                            r^.offset:= pprocdef(p^.procdefinition)^._class^.vmt_offset;
{$ifndef noAllocEdi}
                            getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                            MaybeTestSelf;
                            emit_ref_reg(A_MOV,S_L,r,R_EDI);
                            new(r);
                            reset_reference(r^);
                            r^.base:=R_EDI;
                         end;
                     end
                   else
                     { aktprocsym should be assigned, also in main program }
                     internalerror(12345);
                   {
                     begin
                       new(r);
                       reset_reference(r^);
                       r^.base:=R_ESI;
                       emit_ref_reg(A_MOV,S_L,r,R_EDI);
                       new(r);
                       reset_reference(r^);
                       r^.base:=R_EDI;
                     end;
                   }
                   if pprocdef(p^.procdefinition)^.extnumber=-1 then
                     internalerror(44584);
                   r^.offset:=pprocdef(p^.procdefinition)^._class^.vmtmethodoffset(pprocdef(p^.procdefinition)^.extnumber);
                   MaybeCheckVMT(r^.base);
                   emit_ref(A_CALL,S_NO,r);
{$ifndef noAllocEdi}
                   ungetregister32(R_EDI);
{$endif noAllocEdi}
                end
              else if not inlined then
                begin
                  { We can call interrupts from within the smae code
                    by just pushing the flags and CS PM }
                  if (po_interrupt in p^.procdefinition^.procoptions) then
                    begin
                        emit_none(A_PUSHF,S_L);
                        emit_reg(A_PUSH,S_L,R_CS);
                    end;
                  emitcall(pprocdef(p^.procdefinition)^.mangledname);
                end
              else { inlined proc }
                { inlined code is in inlinecode }
                begin
                   (* { set poinline again }
{$ifdef INCLUDEOK}
                   include(p^.procdefinition^.proccalloptions,pocall_inline);
{$else}
                   p^.procdefinition^.proccalloptions:=p^.procdefinition^.proccalloptions+[pocall_inline];
{$endif}              *)
                   { process the inlinecode }
                   secondpass(inlinecode);
                   { free the args }
                   if pprocdef(p^.procdefinition)^.parast^.datasize>0 then
                     ungetpersistanttemp(pprocdef(p^.procdefinition)^.parast^.address_fixup);
                end;
           end
         else
           { now procedure variable case }
           begin
              secondpass(p^.right);
              if (po_interrupt in p^.procdefinition^.procoptions) then
                begin
                    emit_none(A_PUSHF,S_L);
                    emit_reg(A_PUSH,S_L,R_CS);
                end;
              { procedure of object? }
              if (po_methodpointer in p^.procdefinition^.procoptions) then
                begin
                   { method pointer can't be in a register }
                   hregister:=R_NO;

                   { do some hacking if we call a method pointer }
                   { which is a class member                 }
                   { else ESI is overwritten !             }
                   if (p^.right^.location.reference.base=R_ESI) or
                      (p^.right^.location.reference.index=R_ESI) then
                     begin
                        del_reference(p^.right^.location.reference);
{$ifndef noAllocEdi}
                        getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                        emit_ref_reg(A_MOV,S_L,
                          newreference(p^.right^.location.reference),R_EDI);
                        hregister:=R_EDI;
                     end;

                   { load self, but not if it's already explicitly pushed }
                   if not(po_containsself in p^.procdefinition^.procoptions) then
                     begin
                       { load ESI }
                       inc(p^.right^.location.reference.offset,4);
                       getexplicitregister32(R_ESI);
                       emit_ref_reg(A_MOV,S_L,
                         newreference(p^.right^.location.reference),R_ESI);
                       dec(p^.right^.location.reference.offset,4);
                       { push self pointer }
                       emit_reg(A_PUSH,S_L,R_ESI);
                     end;

                   if hregister=R_NO then
                     emit_ref(A_CALL,S_NO,newreference(p^.right^.location.reference))
                   else
                     begin
                       emit_reg(A_CALL,S_NO,hregister);
{$ifndef noAllocEdi}
                       ungetregister32(hregister);
{$else noAllocEdi}
                       { the same code, the previous line is just to       }
                       { indicate EDI actually is deallocated if allocated }
                       { above (JM)                                        }
                       ungetregister32(hregister);
{$endif noAllocEdi}
                     end;

                   del_reference(p^.right^.location.reference);
                   ungetiftemp(p^.right^.location.reference);
                end
              else
                begin
                   case p^.right^.location.loc of
                      LOC_REGISTER,LOC_CREGISTER:
                         begin
                             emit_reg(A_CALL,S_NO,p^.right^.location.register);
                             ungetregister32(p^.right^.location.register);
                         end
                      else
                         begin
                           emit_ref(A_CALL,S_NO,newreference(p^.right^.location.reference));
                           del_reference(p^.right^.location.reference);
                         end;
                   end;
                end;
           end;

           { this was only for normal functions
             displaced here so we also get
             it to work for procvars PM }
           if (not inlined) and (pocall_clearstack in p^.procdefinition^.proccalloptions) then
             begin
                { we also add the pop_size which is included in pushedparasize }
                pop_size:=0;
                { better than an add on all processors }
                if pushedparasize=4 then
                  begin
{$ifndef noAllocEdi}
                    getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                    emit_reg(A_POP,S_L,R_EDI);
{$ifndef noAllocEdi}
                    ungetregister32(R_EDI);
{$endif noAllocEdi}
                  end
                { the pentium has two pipes and pop reg is pairable }
                { but the registers must be different!        }
                else if (pushedparasize=8) and
                  not(cs_littlesize in aktglobalswitches) and
                  (aktoptprocessor=ClassP5) and
                  (procinfo^._class=nil) then
                    begin
{$ifndef noAllocEdi}
                       getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                       emit_reg(A_POP,S_L,R_EDI);
{$ifndef noAllocEdi}
                       ungetregister32(R_EDI);
{$endif noAllocEdi}
{$ifndef noAllocEdi}
                       exprasmlist^.concat(new(pairegalloc,alloc(R_ESI)));
{$endif noAllocEdi}
                       emit_reg(A_POP,S_L,R_ESI);
{$ifndef noAllocEdi}
                       exprasmlist^.concat(new(pairegalloc,alloc(R_ESI)));
{$endif noAllocEdi}
                    end
                else if pushedparasize<>0 then
                  emit_const_reg(A_ADD,S_L,pushedparasize,R_ESP);
             end;
{$ifdef OPTALIGN}
         if pop_esp then
           emit_reg(A_POP,S_L,R_ESP);
{$endif OPTALIGN}
      dont_call:
         pushedparasize:=oldpushedparasize;
         unused:=unusedregisters;
         usablereg32:=usablecount;
{$ifdef TEMPREGDEBUG}
         testregisters32;
{$endif TEMPREGDEBUG}

         { a constructor could be a function with boolean result }
         { if calling constructor called fail we
           must jump directly to quickexitlabel  PM
           but only if it is a call of an inherited constructor }
         if (inlined or
             (p^.right=nil)) and
            (p^.procdefinition^.proctypeoption=potype_constructor) and
            assigned(p^.methodpointer) and
            (p^.methodpointer^.treetype=typen) and
            (aktprocsym^.definition^.proctypeoption=potype_constructor) then
           begin
             emitjmp(C_Z,faillabel);
           end;
         { handle function results }
         { structured results are easy to handle.... }
         { needed also when result_no_used !! }
         if (p^.resulttype<>pdef(voiddef)) and ret_in_param(p^.resulttype,p^.procdefinition^.proccalloptions) then
           begin
              p^.location.loc:=LOC_MEM;
              p^.location.reference.symbol:=nil;
              p^.location.reference:=funcretref;
           end;
         { we have only to handle the result if it is used, but }
         { ansi/widestrings must be registered, so we can dispose them }
         if (p^.resulttype<>pdef(voiddef)) and (p^.return_value_used or
           is_ansistring(p^.resulttype) or is_widestring(p^.resulttype)) then
           begin
              { a contructor could be a function with boolean result }
              if (inlined or
                  (p^.right=nil)) and
                 (p^.procdefinition^.proctypeoption=potype_constructor) and
                 { quick'n'dirty check if it is a class or an object }
                 (p^.resulttype^.deftype=orddef) then
                begin
                   { this fails if popsize > 0 PM }
                   p^.location.loc:=LOC_FLAGS;
                   p^.location.resflags:=F_NE;


                   if extended_new then
                     begin
{$ifdef test_dest_loc}
                        if dest_loc_known and (dest_loc_tree=p) then
                          mov_reg_to_dest(p,S_L,R_EAX)
                        else
{$endif test_dest_loc}
                          begin
                             hregister:=getexplicitregister32(R_EAX);
                             emit_reg_reg(A_MOV,S_L,R_EAX,hregister);
                             p^.location.register:=hregister;
                          end;
                     end;
                end
               { structed results are easy to handle.... }
              else if ret_in_param(p^.resulttype,p^.procdefinition^.proccalloptions) then
                begin
                   {p^.location.loc:=LOC_MEM;
                   stringdispose(p^.location.reference.symbol);
                   p^.location.reference:=funcretref;
                   already done above (PM) }
                end
              else
                begin
                   if (p^.resulttype^.deftype in [orddef,enumdef]) then
                     begin
                        p^.location.loc:=LOC_REGISTER;
                        case p^.resulttype^.size of
                          4 :
                            begin
{$ifdef test_dest_loc}
                               if dest_loc_known and (dest_loc_tree=p) then
                                 mov_reg_to_dest(p,S_L,R_EAX)
                               else
{$endif test_dest_loc}
                                 begin
                                    hregister:=getexplicitregister32(R_EAX);
                                    emit_reg_reg(A_MOV,S_L,R_EAX,hregister);
                                    p^.location.register:=hregister;
                                 end;
                            end;
                          1 :
                            begin
{$ifdef test_dest_loc}
                                 if dest_loc_known and (dest_loc_tree=p) then
                                   mov_reg_to_dest(p,S_B,R_AL)
                                 else
{$endif test_dest_loc}
                                   begin
                                      hregister:=getexplicitregister32(R_EAX);
                                      emit_reg_reg(A_MOV,S_B,R_AL,reg32toreg8(hregister));
                                      p^.location.register:=reg32toreg8(hregister);
                                   end;
                              end;
                          2 :
                            begin
{$ifdef test_dest_loc}
                               if dest_loc_known and (dest_loc_tree=p) then
                                 mov_reg_to_dest(p,S_W,R_AX)
                               else
{$endif test_dest_loc}
                                 begin
                                    hregister:=getexplicitregister32(R_EAX);
                                    emit_reg_reg(A_MOV,S_W,R_AX,reg32toreg16(hregister));
                                    p^.location.register:=reg32toreg16(hregister);
                                 end;
                            end;
                           8 :
                             begin
{$ifdef test_dest_loc}
{$error Don't know what to do here}
{$endif test_dest_loc}
                                if R_EDX in unused then
                                  begin
                                     hregister2:=getexplicitregister32(R_EDX);
                                     hregister:=getexplicitregister32(R_EAX);
                                  end
                                else
                                  begin
                                     hregister:=getexplicitregister32(R_EAX);
                                     hregister2:=getexplicitregister32(R_EDX);
                                  end;
                                emit_reg_reg(A_MOV,S_L,R_EAX,hregister);
                                emit_reg_reg(A_MOV,S_L,R_EDX,hregister2);
                                p^.location.registerlow:=hregister;
                                p^.location.registerhigh:=hregister2;
                             end;
                        else internalerror(7);
                     end

                end
              else if (p^.resulttype^.deftype=floatdef) then
                case pfloatdef(p^.resulttype)^.typ of
                  f32bit:
                    begin
                       p^.location.loc:=LOC_REGISTER;
{$ifdef test_dest_loc}
                       if dest_loc_known and (dest_loc_tree=p) then
                         mov_reg_to_dest(p,S_L,R_EAX)
                       else
{$endif test_dest_loc}
                         begin
                            hregister:=getexplicitregister32(R_EAX);
                            emit_reg_reg(A_MOV,S_L,R_EAX,hregister);
                            p^.location.register:=hregister;
                         end;
                    end;
                  else
                    begin
                       p^.location.loc:=LOC_FPU;
                       inc(fpuvaroffset);
                    end;
                end
              else if is_ansistring(p^.resulttype) or
                is_widestring(p^.resulttype) then
                begin
                   hregister:=getexplicitregister32(R_EAX);
                   emit_reg_reg(A_MOV,S_L,R_EAX,hregister);
                   gettempansistringreference(hr);
                   decrstringref(p^.resulttype,hr);
                   emit_reg_ref(A_MOV,S_L,hregister,
                     newreference(hr));
                   ungetregister32(hregister);
                   p^.location.loc:=LOC_MEM;
                   p^.location.reference:=hr;
                end
              else if p^.resulttype^.deftype in [recorddef,objectdef] then
                begin
                  p^.location.loc:=LOC_REFERENCE;
                  gettempofsizereference(p^.resulttype^.size,
                    p^.location.reference);
                  if p^.resulttype^.size>4 then
                    begin
                      emit_reg_ref(A_MOV,S_L,
                        R_EAX,newreference(p^.location.reference));
                      inc(p^.location.reference.offset,4);
                      {case p^.resulttype^.size of
                       8:
                          emit_reg_ref(A_MOV,S_L,
                            R_EDX,newreference(p^.location.reference))));
                       7:
                          begin
                            emit_reg_ref(A_MOV,S_W,
                              R_DX,newreference(p^.location.reference))));
                            inc(p^.location.reference.offset,2);
                            emit_const_reg(A_SHR,16,R_EDX);
                            emit_reg_ref(A_MOV,S_B,
                              R_DL,newreference(p^.location.reference))));
                            dec(p^.location.reference.offset,2);
                          end;
                       6:
                          emit_reg_ref(A_MOV,S_W,
                            R_DX,newreference(p^.location.reference))));
                       5:
                          emit_reg_ref(A_MOV,S_B,
                            R_DL,newreference(p^.location.reference))));
                      end;
                       We don't need to care because
                       the temp is allocated with a size
                       grown to a multiple of 4 }
                      emit_reg_ref(A_MOV,S_L,
                         R_EDX,newreference(p^.location.reference));
                      dec(p^.location.reference.offset,4);
                    end
                  else
                    begin
                      {case p^.resulttype^.size of
                       4:
                          emit_reg_ref(A_MOV,S_L,
                            R_EAX,newreference(p^.location.reference))));
                       3:
                          begin
                            emit_reg_ref(A_MOV,S_W,
                              R_AX,newreference(p^.location.reference))));
                            inc(p^.location.reference.offset,2);
                            emit_const_reg(A_SHR,16,R_EAX);
                            emit_reg_ref(A_MOV,S_B,
                              R_AL,newreference(p^.location.reference))));
                            dec(p^.location.reference.offset,2);
                          end;
                       2:
                          emit_reg_ref(A_MOV,S_W,
                            R_AX,newreference(p^.location.reference))));
                       1:
                          emit_reg_ref(A_MOV,S_B,
                            R_AL,newreference(p^.location.reference))));
                      end;   }
                      emit_reg_ref(A_MOV,S_L,
                        R_EAX,newreference(p^.location.reference));
                    end;
                end
              else
                begin
                   p^.location.loc:=LOC_REGISTER;
{$ifdef test_dest_loc}
                   if dest_loc_known and (dest_loc_tree=p) then
                     mov_reg_to_dest(p,S_L,R_EAX)
                   else
{$endif test_dest_loc}
                     begin
                       hregister:=getexplicitregister32(R_EAX);
                       emit_reg_reg(A_MOV,S_L,R_EAX,hregister);
                       p^.location.register:=hregister;
                     end;
                end;
             end;
           end;

         { perhaps i/o check ? }
         if iolabel<>nil then
           begin
              emit_sym(A_PUSH,S_L,iolabel);
              emitcall('FPC_IOCHECK');
           end;
         if pop_size>0 then
           emit_const_reg(A_ADD,S_L,pop_size,R_ESP);

         { restore registers }
         popusedregisters(pushed);

         { at last, restore instance pointer (SELF) }
         if loadesi then
           maybe_loadself;
         pp:=params;
         while assigned(pp) do
           begin
              if assigned(pp^.left) then
                begin
                  if (pp^.left^.location.loc in [LOC_REFERENCE,LOC_MEM]) then
                    ungetiftemp(pp^.left^.location.reference);
                { process also all nodes of an array of const }
                  if pp^.left^.treetype=arrayconstructn then
                    begin
                      if assigned(pp^.left^.left) then
                       begin
                         hp:=pp^.left;
                         while assigned(hp) do
                          begin
                            if (hp^.left^.location.loc in [LOC_REFERENCE,LOC_MEM]) then
                              ungetiftemp(hp^.left^.location.reference);
                            hp:=hp^.right;
                          end;
                       end;
                    end;
                end;
              pp:=pp^.right;
           end;
         if inlined then
           begin
             if (p^.resulttype<>pdef(voiddef)) then
               ungetpersistanttemp(inlinecode^.retoffset);
             pprocdef(p^.procdefinition)^.parast^.address_fixup:=store_parast_fixup;
             p^.right:=inlinecode;
           end;
         disposetree(params);


         { from now on the result can be freed normally }
         if inlined and ret_in_param(p^.resulttype,p^.procdefinition^.proccalloptions) then
           persistanttemptonormal(funcretref.offset);

         { if return value is not used }
         if (not p^.return_value_used) and (p^.resulttype<>pdef(voiddef)) then
           begin
              if p^.location.loc in [LOC_MEM,LOC_REFERENCE] then
                begin
                   { data which must be finalized ? }
                   if (p^.resulttype^.needs_inittable) and
                     ( (p^.resulttype^.deftype<>objectdef) or
                       not(pobjectdef(p^.resulttype)^.is_class)) then
                      finalize(p^.resulttype,p^.location.reference,false);
                   { release unused temp }
                   ungetiftemp(p^.location.reference)
                end
              else if p^.location.loc=LOC_FPU then
                begin
                  { release FPU stack }
                  emit_reg(A_FSTP,S_NO,R_ST0);
                  {
                    dec(fpuvaroffset);
                    do NOT decrement as the increment before
                    is not called for unused results PM }
                end;
           end;
      end;


{*****************************************************************************
                             SecondProcInlineN
*****************************************************************************}


    procedure secondprocinline(var p : ptree);
       var st : psymtable;
           oldprocsym : pprocsym;
           para_size,savedstackposition : longint;
           oldprocinfo : pprocinfo;
           store_address_fixup,storesymtablelevel : longint;
           storeparasymtable,storelocalsymtable : tsymtabletype;
           oldinlining_procedure,
           nostackframe,make_global : boolean;
           proc_names : tstringcontainer;
           inlineentrycode,inlineexitcode : paasmoutput;
           oldexitlabel,oldexit2label,oldquickexitlabel:Pasmlabel;
{$ifdef GDB}
           startlabel,endlabel : pasmlabel;
           pp : pchar;
           mangled_length  : longint;
{$endif GDB}
       begin
          oldinlining_procedure:=inlining_procedure;
          oldexitlabel:=aktexitlabel;
          oldexit2label:=aktexit2label;
          oldquickexitlabel:=quickexitlabel;
          getlabel(aktexitlabel);
          getlabel(aktexit2label);
          oldprocsym:=aktprocsym;
          { we're inlining a procedure }
          inlining_procedure:=true;
          { save old procinfo }
          getmem(oldprocinfo,sizeof(tprocinfo));
          move(procinfo^,oldprocinfo^,sizeof(tprocinfo));
          { set the return value }
          aktprocsym:=p^.inlineprocsym;
          procinfo^.returntype:=aktprocsym^.definition^.rettype;
          procinfo^.return_offset:=p^.retoffset;
          procinfo^.para_offset:=p^.para_offset;
          { arg space has been filled by the parent secondcall }
          st:=aktprocsym^.definition^.localst;
          { set it to the same lexical level }
          storesymtablelevel:=st^.symtablelevel;
          st^.symtablelevel:=oldprocsym^.definition^.localst^.symtablelevel;
          if st^.datasize>0 then
            begin
              store_address_fixup:=st^.address_fixup;
              st^.address_fixup:=gettempofsizepersistant(st^.datasize)+st^.datasize;
{$ifdef extdebug}
              Comment(V_debug,'local symtable is at offset '+tostr(st^.address_fixup));
              exprasmlist^.concat(new(pai_asm_comment,init(strpnew(
                'local symtable is at offset '+tostr(st^.address_fixup)))));
{$endif extdebug}
            end;
          exprasmlist^.concat(new(Pai_Marker, Init(InlineStart)));
{$ifdef extdebug}
          exprasmlist^.concat(new(pai_asm_comment,init(strpnew('Start of inlined proc'))));
{$endif extdebug}
{$ifdef GDB}
          if (cs_debuginfo in aktmoduleswitches) then
            begin
              getlabel(startlabel);
              getlabel(endlabel);
              emitlab(startlabel);
              storelocalsymtable:=p^.inlineprocsym^.definition^.localst^.symtabletype;
              storeparasymtable:=p^.inlineprocsym^.definition^.parast^.symtabletype;
              p^.inlineprocsym^.definition^.localst^.symtabletype:=inlinelocalsymtable;
              p^.inlineprocsym^.definition^.parast^.symtabletype:=inlineparasymtable;

              { Here we must include the para and local symtable info }
              p^.inlineprocsym^.concatstabto(withdebuglist);

              { set it back for savety }
              p^.inlineprocsym^.definition^.localst^.symtabletype:=storelocalsymtable;
              p^.inlineprocsym^.definition^.parast^.symtabletype:=storeparasymtable;

              mangled_length:=length(oldprocsym^.definition^.mangledname);
              getmem(pp,mangled_length+50);
              strpcopy(pp,'192,0,0,'+startlabel^.name);
              if (target_os.use_function_relative_addresses) then
                begin
                  strpcopy(strend(pp),'-');
                  strpcopy(strend(pp),oldprocsym^.definition^.mangledname);
                end;
              withdebuglist^.concat(new(pai_stabn,init(strnew(pp))));
            end;
{$endif GDB}
          { takes care of local data initialization }
          inlineentrycode:=new(paasmoutput,init);
          inlineexitcode:=new(paasmoutput,init);
          proc_names.init;
          para_size:=p^.para_size;
          make_global:=false; { to avoid warning }
          genentrycode(inlineentrycode,proc_names,make_global,0,para_size,nostackframe,true,savedstackposition);
          if po_assembler in aktprocsym^.definition^.procoptions then
            inlineentrycode^.insert(new(pai_marker,init(asmblockstart)));
          exprasmlist^.concatlist(inlineentrycode);
          secondpass(p^.inlinetree);
          genexitcode(inlineexitcode,0,false,true,savedstackposition);
          if po_assembler in aktprocsym^.definition^.procoptions then
            inlineexitcode^.concat(new(pai_marker,init(asmblockend)));
          exprasmlist^.concatlist(inlineexitcode);

          dispose(inlineentrycode,done);
          dispose(inlineexitcode,done);
{$ifdef extdebug}
          exprasmlist^.concat(new(pai_asm_comment,init(strpnew('End of inlined proc'))));
{$endif extdebug}
          exprasmlist^.concat(new(Pai_Marker, Init(InlineEnd)));

          {we can free the local data now, reset also the fixup address }
          if st^.datasize>0 then
            begin
              ungetpersistanttemp(st^.address_fixup-st^.datasize);
              st^.address_fixup:=store_address_fixup;
            end;
          { restore procinfo }
          move(oldprocinfo^,procinfo^,sizeof(tprocinfo));
          freemem(oldprocinfo,sizeof(tprocinfo));
{$ifdef GDB}
          if (cs_debuginfo in aktmoduleswitches) then
            begin
              emitlab(endlabel);
              strpcopy(pp,'224,0,0,'+endlabel^.name);
             if (target_os.use_function_relative_addresses) then
               begin
                 strpcopy(strend(pp),'-');
                 strpcopy(strend(pp),oldprocsym^.definition^.mangledname);
               end;
              withdebuglist^.concat(new(pai_stabn,init(strnew(pp))));
              freemem(pp,mangled_length+50);
            end;
{$endif GDB}
          { restore }
          st^.symtablelevel:=storesymtablelevel;
          aktprocsym:=oldprocsym;
          aktexitlabel:=oldexitlabel;
          aktexit2label:=oldexit2label;
          quickexitlabel:=oldquickexitlabel;
          inlining_procedure:=oldinlining_procedure;
       end;



end.
{
  $Log: cgcal.pas,v $
  Revision 1.1.2.27  2003/01/22 00:23:56  pierre
   * fix tcalobjX failure on static methods without vmt

  Revision 1.1.2.26  2003/01/21 11:53:48  pierre
   * fix web reports 2317 and 2318

  Revision 1.1.2.25  2003/01/20 15:47:50  pierre
   * pass vmt or 0 correctly to static object methods

  Revision 1.1.2.24  2003/01/14 21:27:42  peter
    * fixed tw2296

  Revision 1.1.2.23  2003/01/06 21:18:02  peter
    * unget temp for methodpointer

  Revision 1.1.2.22  2002/11/22 16:36:05  pierre
   * fix an error introduced in a previous fix for done inside methods

  Revision 1.1.2.21  2002/11/19 00:48:59  pierre
   * fix tbs/tb0419 cdecl open array problem

  Revision 1.1.2.20  2002/11/18 16:53:53  pierre
   * handle non class objects as recorddefs for calling conventions

  Revision 1.1.2.19  2002/11/15 14:10:13  pierre
    + added a calloptions parameter to ret_inacc and ret_in_param
      functions.
      This allows to handle win32 stdcall and m68k cdecl small records
      case.

  Revision 1.1.2.18  2002/11/12 11:25:04  pierre
   * fix a bug in a previous commit

  Revision 1.1.2.17  2002/11/05 17:49:26  pierre
    * code that returns record of size < 8 for win32 target in registers
      only active in debug mode for now.
    * secondcalln corrected to transfer result into a temp, so
      that secondsubscriptn works correctly.

  Revision 1.1.2.16  2002/11/04 13:03:41  pierre
   * any C array parameter is passed by reference

  Revision 1.1.2.15  2002/10/29 14:58:13  pierre
   * fix bug tw1863

  Revision 1.1.2.14  2002/10/18 11:25:59  pierre
   * fix bug in last commit, retoffset needs to be allocated for all inlined functions

  Revision 1.1.2.13  2002/10/18 10:55:42  pierre
   * recalculate the para_size for inlined procedures

  Revision 1.1.2.12  2002/10/15 19:45:38  pierre
   * fix pascal high parameter passing

  Revision 1.1.2.11  2002/10/15 12:04:01  pierre
   * check for nil self with -Cr or -CR

  Revision 1.1.2.10  2002/10/15 06:12:26  pierre
   * tcalval9 finally fixed, no other change in tests results

  Revision 1.1.2.9  2002/03/07 21:42:26  carl
  * bugfix #1827 (readln() parameter checking... should not give internal error, but simple error)

  Revision 1.1.2.8  2001/10/02 22:30:04  pierre
   + allow multi level inline, fix bug 1623

  Revision 1.1.2.7  2001/09/09 08:51:40  jonas
    * when calling an inline procedure inside a nested procedure, the
      framepointer was being pushed on the stack, but this pushed framepointer
      was never used nor removed from the stack again after the inlining was
      done. It's now simply not pushed anymore, because the inlined procedure
      can get the previous framepointer from the procedure in which it is being
      inlined

  Revision 1.1.2.6  2001/09/01 22:54:46  jonas
    * i386*: call and jmp read their first operand
    * cgcal: deallocate hlper register only after call statement (fixes bug
      with "procedure of object" and optimizer reported to bugrep on
      2001/08/30)

  Revision 1.1.2.5  2001/06/04 11:47:11  peter
    * better const to var checking

  Revision 1.1.2.4  2001/05/19 21:27:17  peter
    * function returning int64 inlining fixed

  Revision 1.1.2.3  2001/05/17 01:28:19  carl
  * added missing begin...end statements

  Revision 1.1.2.2  2001/02/27 02:19:04  carl
  * rename maybe_loadesi to maybe_loadself

  Revision 1.1.2.1  2001/02/25 03:46:27  carl
  + trying again

  Revision 1.1.2.1  2001/02/25 02:33:34  carl
  - moved to i386 directory

  Revision 1.1.2.6  2001/01/08 21:47:39  peter
    * don't push high value for open array with cdecl;external;

  Revision 1.1.2.5  2000/12/03 22:24:44  florian
    * fixed web bug 1275: problem with int64 function results

  Revision 1.1.2.4  2000/11/23 14:08:08  jonas
    * fix for webbug 1066/1126

  Revision 1.1.2.3  2000/11/22 14:07:59  jonas
    * fixed inlining problems

  Revision 1.1.2.2  2000/09/16 12:23:33  peter
    * fixed with and local object

  Revision 1.1.2.1  2000/09/10 20:17:42  peter
    * fixed open array with cdecl
    * fixed finalize call with unused function return

  Revision 1.1  2000/07/13 06:29:44  michael
  + Initial import

  Revision 1.138  2000/07/05 20:39:55  florian
    * virtual contructors weren't handled properly if they were called via a class
      variable

  Revision 1.137  2000/06/29 13:50:30  jonas
    * fixed inline bugs (calling an inlined procedure more than once didn't
      work)

  Revision 1.136  2000/06/04 09:05:05  peter
    * fix addrn with procvar, also detected by testpva2 !

  Revision 1.135  2000/05/31 09:29:15  florian
    * stack alignment to 8 byte boundaries with -Oa switch

  Revision 1.134  2000/05/16 20:19:05  pierre
    + -CR option to enable check for object virtual method

  Revision 1.133  2000/05/15 19:30:27  peter
    * fixed calling of inherited methods from destructors

  Revision 1.132  2000/05/09 14:15:03  pierre
   * also allow interrupt procvars

  Revision 1.131  2000/05/09 10:54:03  pierre
   add code to allow calling interrupt routines

  Revision 1.130  2000/03/31 22:56:45  pierre
    * fix the handling of value parameters in cdecl function

  Revision 1.129  2000/03/19 08:17:36  peter
    * tp7 fix

  Revision 1.128  2000/03/16 15:18:13  pierre
   * avoid wrong ungetpersistanttemp

  Revision 1.127  2000/03/01 00:03:11  pierre
    * fixes for locals in inlined procedures
      fix for bug797
    + stabs generation for inlined paras and locals

  Revision 1.126  2000/02/09 18:08:33  jonas
    * added regallocs for esi

  Revision 1.125  2000/02/09 13:22:45  peter
    * log truncated

  Revision 1.124  2000/02/04 20:00:21  florian
    * an exception in a construcor calls now the destructor (this applies only
      to classes)

  Revision 1.123  2000/01/26 15:03:59  peter
    * fixed pop_size included twice with clearstack

  Revision 1.122  2000/01/26 12:02:29  peter
    * abstractprocdef.para_size needs alignment parameter
    * secondcallparan gets para_alignment size instead of dword_align

  Revision 1.121  2000/01/23 18:50:07  peter
    * fixed missing push esi for constructor calling

  Revision 1.120  2000/01/21 22:06:16  florian
    * fixed for the fix of bug 793
    * fpu variables modified by nested subroutines aren't regable anymore
    * $maxfpuregisters doesn't modify anymore the behavior of a procedure before

  Revision 1.119  2000/01/21 12:17:41  jonas
    * regallocation fixes

  Revision 1.118  2000/01/20 12:14:47  florian
    * bug 793 fixed

  Revision 1.117  2000/01/16 22:17:11  peter
    * renamed call_offset to para_offset

  Revision 1.116  2000/01/09 12:35:00  jonas
    * changed edi allocation to use getexplicitregister32/ungetregister
      (adapted tgeni386 a bit for this) and enabled it by default
    * fixed very big and stupid bug of mine in cg386mat that broke the
      include() code (and make cycle :( ) if you compiled without
      -dnewoptimizations

  Revision 1.115  2000/01/09 01:44:19  jonas
    + (de)allocation info for EDI to fix reported bug on mailinglist.
      Also some (de)allocation info for ESI added. Between -dallocEDI
      because at this time of the night bugs could easily slip in ;)

  Revision 1.114  2000/01/07 01:14:20  peter
    * updated copyright to 2000

  Revision 1.113  1999/12/22 01:01:46  peter
    - removed freelabel()
    * added undefined label detection in internal assembler, this prevents
      a lot of ld crashes and wrong .o files
    * .o files aren't written anymore if errors have occured
    * inlining of assembler labels is now correct

  Revision 1.112  1999/12/13 21:49:54  pierre
   * bug in extdebugg code for inlined procedures

  Revision 1.111  1999/11/30 10:40:42  peter
    + ttype, tsymlist

  Revision 1.110  1999/11/06 14:34:17  peter
    * truncated log to 20 revs

  Revision 1.109  1999/11/04 00:23:58  pierre
   * fix for fpuvaroffset for unused return value

  Revision 1.108  1999/10/26 12:30:40  peter
    * const parameter is now checked
    * better and generic check if a node can be used for assigning
    * export fixes
    * procvar equal works now (it never had worked at least from 0.99.8)
    * defcoll changed to linkedlist with pparaitem so it can easily be
      walked both directions

  Revision 1.107  1999/10/08 15:40:47  pierre
   * use and remember that C functions with complex data results use ret $4

  Revision 1.106  1999/09/27 23:44:46  peter
    * procinfo is now a pointer
    * support for result setting in sub procedure

  Revision 1.105  1999/09/26 13:26:02  florian
    * exception patch of Romio nevertheless the excpetion handling
      needs some corections regarding register saving
    * gettempansistring is again a procedure

}
