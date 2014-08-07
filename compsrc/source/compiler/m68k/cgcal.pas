{
    $Id: cgcal.pas,v 1.1.2.52 2003/03/07 12:14:21 pierre Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate m68k assembler for in call nodes

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
      cga,tgen,cgld,cgbase;

{*****************************************************************************
                             SecondCallParaN
*****************************************************************************}

    { setup a parameter before a call to a subroutine }
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
         hreg : tregister;
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
              inc(pushedparasize,target_os.size_of_pointer);
              if (p^.left^.treetype=addrn) and
                 (not p^.left^.procvarload) then
                begin
                { always a register }
                  if inlined then
                    begin
                       r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                       emit_reg_ref(A_MOVE,S_L,
                         p^.left^.location.register,r);
                    end
                  else
                    emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,R_SPPUSH);
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
                           hreg := getaddressreg;
                           emit_ref_reg(A_LEA,S_L,
                             newreference(p^.left^.location.reference),hreg);
                           r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                           emit_reg_ref(A_MOVE,S_L,hreg,r);
                           ungetregister(hreg);
                         end
                      else
                        emitpushreferenceaddr(p^.left^.location.reference);
                        del_reference(p^.left^.location.reference);
                     end;
                end;
           end
         { handle call by reference parameter }
         else if (defcoll^.paratyp=vs_var) or
                 (is_cdecl and (defcoll^.paratype.def^.deftype=arraydef)) then
           begin
              if not(p^.left^.location.loc in [LOC_REFERENCE,LOC_MEM]) then
                emit_to_mem(p^.left);
              if not push_from_left_to_right then
                maybe_push_high;
              inc(pushedparasize,target_os.size_of_pointer);
              if inlined then
                begin
                   hreg := getaddressreg;
                   emit_ref_reg(A_LEA,S_L,
                     newreference(p^.left^.location.reference),hreg);
                   r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                   emit_reg_ref(A_MOVE,S_L,hreg,r);
                   ungetregister(hreg);
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
                   inc(pushedparasize,target_os.size_of_pointer);
                   if inlined then
                     begin
                        hreg := getaddressreg;
                        emit_ref_reg(A_LEA,S_L,
                          newreference(p^.left^.location.reference),hreg);
                        r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                        emit_reg_ref(A_MOVE,S_L,hreg,r);
                        ungetregister(hreg);
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
         r,r2 : preference;
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
         hreg : tregister;
         href : preference;
         opsize : topsize;
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
              emit_reg(A_TST,S_L,self_pointer);
              GetLabel(OKLabel);
              emitlabeled(A_BNE,OKLabel);
              emit_const_reg(A_MOVE,S_L,210,R_SPPUSH);
              emitcall('FPC_HANDLEERROR');
              emitlab(OKLabel);
            end;
        end;

      procedure MaybeCheckVMT(var reg : tregister);
        begin
          if (cs_check_object_ext in aktlocalswitches) then
            begin
              emit_sym(A_PEA,S_L,
                newasmsymbol(pprocdef(p^.procdefinition)^._class^.vmt_mangledname));
              emit_reg_reg(A_MOVE,S_L,reg,R_SPPUSH);
              emitcall('FPC_CHECK_OBJECT_EXT');
            end
          else if (cs_check_range in aktlocalswitches) then
            begin
              emit_reg_reg(A_MOVE,S_L,reg,R_SPPUSH);
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

         if ((pocall_cdecl in p^.procdefinition^.proccalloptions) or
             (pocall_stdcall in p^.procdefinition^.proccalloptions)) and
             (target_info.target<>target_m68k_PalmOS) then
         { is this true for all m68k platform ?? FK }
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
              (* { disable further inlining of the same proc
                in the args }
              exclude(p^.procdefinition^.proccalloptions,pocall_inline); *)
           end;
         { only if no proc var }
         if inlined or not(assigned(p^.right)) then
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
              saveusedregisters(pushed,pprocdef(p^.procdefinition)^.usedregisters);

              { give used registers through }
              usedinproc:=usedinproc + pprocdef(p^.procdefinition)^.usedregisters;
           end
         else
           begin
              saveusedregisters(pushed,ALL_REGISTERS);
              usedinproc:=ALL_REGISTERS;
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
             { Here we align the stack pointer correctly to the specified }
             { target alignment.                                          }

             { If the number of bytes pushed is not aligned correctly }
             i:=oldpushedparasize mod target_os.stackalignment;
             { align the stack to the correct size }
             if i>0 then
               inc(pop_size,target_os.stackalignment-i);
             { This parasize aligned on target_os.stack_alignment ? }
             i:=p^.procdefinition^.para_size(para_alignment) mod target_os.stackalignment;
             if i>0 then
               inc(pop_size,target_os.stackalignment-i);
             { insert the opcode and update pushedparasize }
             pop_size:=pop_size mod target_os.stackalignment;
             if pop_size>0 then
               begin
                  inc(pushedparasize,pop_size);
                 { actually increment the stack pointer to the proper alignment }
                  emit_const_reg(A_SUB,S_L,pop_size,stack_pointer);
{$ifdef GDB}
                  if (cs_debuginfo in aktmoduleswitches) and
                    (exprasmlist^.first=exprasmlist^.last) then
                  exprasmlist^.concat(new(pai_force_line,init));
{$endif GDB}
               end;
          end;
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
              if inlined then
                begin
{$ifdef OLD_C_STACK}
                   inc(pushedparasize,4); { lets try without it PM }
{$endif not OLD_C_STACK}
                   hreg := getaddressreg;
                   emit_ref_reg(A_LEA,S_L,
                     newreference(funcretref),hreg);
                   r:=new_reference(procinfo^.framepointer,inlinecode^.retoffset);
                   emit_reg_ref(A_MOVE,S_L,hreg,r);
                   ungetregister(hreg);
                end
              else
                begin
                   { C code expects return address in A1 or A0 register }
                   if pocall_cdecl in p^.procdefinition^.proccalloptions then
                     emit_ref_reg(A_LEA,S_L,newreference(funcretref),cdecl_function_return_address_reg)
                   else
                     begin
{$ifdef OLD_C_STACK}
                       inc(pushedparasize,4); { lets try without it PM }
{$endif not OLD_C_STACK}
                       emitpushreferenceaddr(funcretref);
                     end;
                end;
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
                   p^.methodpointer^.location.register:=self_pointer;
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
                     emit_ref_reg(A_MOVE,S_L,r,self_pointer)
                   else
                     emit_ref_reg(A_LEA,S_L,r,self_pointer);
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
                                         if not(oo_has_vmt in pobjectdef(p^.methodpointer^.resulttype)^.objectoptions) then
                                           emit_const_reg(A_MOVE,S_L,0,self_pointer)
                                         else
                                           begin
                                             emit_sym_ofs_reg(A_LEA,S_L,
                                               newasmsymbol(pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname),
                                               0,self_pointer);
                                           end;
                                         { emit_reg_reg(A_MOVE,S_L,self_pointer,R_SPPUSH);
                                           this is done below !! }
                                      end
                                    else
                                      { this is a member call, so SELF_POINTER isn't modfied }
                                      loadesi:=false;

                                    { a class destructor needs a flag }
                                    if pobjectdef(p^.methodpointer^.resulttype)^.is_class and
                                       {assigned(aktprocsym) and
                                       (aktprocsym^.definition^.proctypeoption=potype_destructor)}
                                       (p^.procdefinition^.proctypeoption=potype_destructor) then
                                      begin
                                        push_int(0);
                                        emit_reg_reg(A_MOVE,S_L,self_pointer,R_SPPUSH);
                                      end;

                                    if not(is_con_or_destructor and
                                           pobjectdef(p^.methodpointer^.resulttype)^.is_class and
                                           {assigned(aktprocsym) and
                                          (aktprocsym^.definition^.proctypeoption in [potype_constructor,potype_destructor])}
                                           (p^.procdefinition^.proctypeoption in [potype_constructor,potype_destructor])
                                          ) then
                                      emit_reg_reg(A_MOVE,S_L,self_pointer,R_SPPUSH);
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
                                    { SELF must be zero }
                                    emit_reg(A_CLR,S_L,accumulator);
                                    emit_reg_reg(A_MOVE,S_L,accumulator,self_pointer);
                                    emit_reg_reg(A_MOVE,S_L,accumulator,R_SPPUSH);
                                    { insert the vmt }
                                    emit_sym(A_PEA,S_L,
                                      newasmsymbol(pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname));
                                    extended_new:=true;
                                 end;
                               hdisposen:
                                 begin
                                    secondpass(p^.methodpointer);

                                    { destructor with extended syntax called from dispose }
                                    { hdisposen always deliver LOC_REFERENCE          }
                                    emit_ref_reg(A_LEA,S_L,
                                      newreference(p^.methodpointer^.location.reference),self_pointer);
                                    del_reference(p^.methodpointer^.location.reference);
                                    emit_reg_reg(A_MOVE,S_L,self_pointer,R_SPPUSH);
                                    emit_sym(A_PEA,S_L,
                                      newasmsymbol(pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname));
                                 end;
                               else
                                 begin
                                    { call to an instance member }
                                    if (p^.symtable^.symtabletype<>withsymtable) then
                                      begin
                                         secondpass(p^.methodpointer);
                                         case p^.methodpointer^.location.loc of
                                            LOC_CREGISTER,
                                            LOC_REGISTER:
                                              begin
                                                 emit_reg_reg(A_MOVE,S_L,p^.methodpointer^.location.register,self_pointer);
                                                 ungetregister(p^.methodpointer^.location.register);
                                              end;
                                            else
                                              begin
                                                 if (p^.methodpointer^.resulttype^.deftype=classrefdef) or
                                                    ((p^.methodpointer^.resulttype^.deftype=objectdef) and
                                                   pobjectdef(p^.methodpointer^.resulttype)^.is_class) then
                                                   emit_ref_reg(A_MOVE,S_L,
                                                     newreference(p^.methodpointer^.location.reference),self_pointer)
                                                 else
                                                   emit_ref_reg(A_LEA,S_L,
                                                     newreference(p^.methodpointer^.location.reference),self_pointer);
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
                                              emit_const_reg(A_MOVE,S_L,0,self_pointer)
                                            else
                                              begin
                                                { class method needs current VMT }
                                                new(r);
                                                reset_reference(r^);
                                                r^.base:=self_pointer;
                                                r^.offset:= pprocdef(p^.procdefinition)^._class^.vmt_offset;
                                                MaybeTestSelf;
                                                emit_ref_reg(A_MOVE,S_L,r,self_pointer);
                                                MaybeCheckVMT(self_pointer);
                                              end;
                                          end;

                                        { direct call to destructor: remove data }
                                        if (p^.procdefinition^.proctypeoption=potype_destructor) and
                                           (p^.methodpointer^.resulttype^.deftype=objectdef) and
                                           (pobjectdef(p^.methodpointer^.resulttype)^.is_class) then
                                          emit_const_reg(A_MOVE,S_L,1,R_SPPUSH);

                                        { direct call to class constructor, don't allocate memory }
                                        if (p^.procdefinition^.proctypeoption=potype_constructor) and
                                           (p^.methodpointer^.resulttype^.deftype=objectdef) and
                                           (pobjectdef(p^.methodpointer^.resulttype)^.is_class) then
                                          begin
                                             emit_reg(A_CLR,S_L,R_SPPUSH);
                                             emit_reg(A_CLR,S_L,R_SPPUSH);
                                          end
                                        else
                                          begin
                                             { constructor call via classreference => allocate memory }
                                             if (p^.procdefinition^.proctypeoption=potype_constructor) and
                                                (p^.methodpointer^.resulttype^.deftype=classrefdef) and
                                                (pobjectdef(pclassrefdef(p^.methodpointer^.resulttype)^.
                                                   pointertype.def)^.is_class) then
                                                emit_const_reg(A_MOVE,S_L,1,R_SPPUSH);
                                             emit_reg_reg(A_MOVE,S_L,self_pointer,R_SPPUSH);
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
                                                   emit_sym(A_PEA,S_L,newasmsymbol(
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
                             )
                           ) or
                           ((po_staticmethod in p^.procdefinition^.procoptions) and
                            not(
                              assigned(aktprocsym) and
                              (po_staticmethod in aktprocsym^.definition^.procoptions)
                            )
                           ) then
                          begin
                             if not(oo_has_vmt in pobjectdef(p^.symtableproc^.defowner)^.objectoptions) then
                               emit_const_reg(A_MOVE,S_L,0,self_pointer)
                             else
                               begin
                                 { class method needs current VMT }
                                 new(r);
                                 reset_reference(r^);
                                 r^.base:=self_pointer;
                                 r^.offset:= pprocdef(p^.procdefinition)^._class^.vmt_offset;
                                 MaybeTestSelf;
                                 emit_ref_reg(A_MOVE,S_L,r,self_pointer);
                                 MaybeCheckVMT(self_pointer);
                               end;
                          end
                        else
                          begin
                             { member call, SELF isn't modified }
                             loadesi:=false;
                          end;
                        { direct call to destructor: don't remove data! }
                        if procinfo^._class^.is_class then
                          begin
                             if (p^.procdefinition^.proctypeoption=potype_destructor) then
                               begin
                                  emit_reg(A_CLR,S_L,R_SPPUSH);
                                  emit_reg_reg(A_MOVE,S_L,self_pointer,R_SPPUSH);
                               end
                             else if (p^.procdefinition^.proctypeoption=potype_constructor) then
                               begin
                                  emit_reg(A_CLR,S_L,R_SPPUSH);
                                  emit_reg(A_CLR,S_L,R_SPPUSH);
                               end
                             else
                               emit_reg_reg(A_MOVE,S_L,self_pointer,R_SPPUSH);
                          end
                        else
                          begin
                             emit_reg_reg(A_MOVE,S_L,self_pointer,R_SPPUSH);
                             if is_con_or_destructor then
                               begin
                                  (*if (p^.procdefinition^.proctypeoption=potype_constructor) then
                                     begin
                                       { it's no bad idea, to insert the VMT }
                                       emit_sym(A_PEA,S_L,newasmsymbol(
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
              if not inlined then
                begin
                  if (lexlevel>=normal_function_level) and assigned(pprocdef(p^.procdefinition)^.parast) and
                    ((pprocdef(p^.procdefinition)^.parast^.symtablelevel)>normal_function_level) then
                    begin
                    { if we call a nested function in a method, we must      }
                    { push also SELF!                                    }
                    { THAT'S NOT TRUE, we have to load SELF via frame pointer }
                    { access                                              }
                    {
                     begin
                        loadesi:=false;
                        emit_reg_reg(A_MOVE,S_L,R_self_pointer,R_SPPUSH);
                     end;
                   }
                        if lexlevel=(pprocdef(p^.procdefinition)^.parast^.symtablelevel) then
                           begin
                             new(r);
                             reset_reference(r^);
                             r^.offset:=procinfo^.framepointer_offset;
                             r^.base:=procinfo^.framepointer;
                             emit_ref_reg(A_MOVE,S_L,r,R_SPPUSH)
                           end
                        { this is only true if the difference is one !!
                         but it cannot be more !! }
                         else if (lexlevel=pprocdef(p^.procdefinition)^.parast^.symtablelevel-1) then
                           begin
                             emit_reg_reg(A_MOVE,S_L,procinfo^.framepointer,R_SPPUSH)
                           end
                         else if (lexlevel>pprocdef(p^.procdefinition)^.parast^.symtablelevel) then
                           begin
                             hregister:=getaddressreg;
                             new(r);
                             reset_reference(r^);
                             r^.offset:=procinfo^.framepointer_offset;
                             r^.base:=procinfo^.framepointer;
                             emit_ref_reg(A_MOVE,S_L,r,hregister);
                             for i:=(pprocdef(p^.procdefinition)^.parast^.symtablelevel) to lexlevel-1 do
                               begin
                                 new(r);
                                 reset_reference(r^);
                                 {we should get the correct frame_pointer_offset at each level
                                  how can we do this !!! }
                                 r^.offset:=procinfo^.framepointer_offset;
                                 r^.base:=hregister;
                                 emit_ref_reg(A_MOVE,S_L,r,hregister);
                               end;
                             emit_reg_reg(A_MOVE,S_L,hregister,R_SPPUSH);
                             ungetregister(hregister);
                           end
                         else
                           internalerror(25000);
                   end;
                 end;

              if (po_virtualmethod in p^.procdefinition^.procoptions) and
                 not(no_virtual_call) then
                begin
                   { static functions contain the vmt_address in SELF }
                   { also class methods                       }
                   { Here it is quite tricky because it also depends }
                   { on the methodpointer                        PM }
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
                        { SELF is loaded earlier }
                        (po_classmethod in p^.procdefinition^.procoptions) then
                         begin
                            new(r);
                            reset_reference(r^);
                            r^.base:=self_pointer;
                         end
                       else
                         begin
                            new(r);
                            reset_reference(r^);
                            r^.base:=self_pointer;
                            { this is one point where we need vmt_offset (PM) }
                            r^.offset:= pprocdef(p^.procdefinition)^._class^.vmt_offset;
                            hreg := getaddressreg;
                            MaybeTestSelf;
                            emit_ref_reg(A_MOVE,S_L,r,hreg);
                            new(r);
                            reset_reference(r^);
                            r^.base:=hreg;
                         end;
                     end
                   else
                     { aktprocsym should be assigned, also in main program }
                     internalerror(12345);
                   if pprocdef(p^.procdefinition)^.extnumber=-1 then
                     internalerror(44584);
                   r^.offset:=pprocdef(p^.procdefinition)^._class^.vmtmethodoffset(pprocdef(p^.procdefinition)^.extnumber);
                   MaybeCheckVMT(r^.base);
                   { JSR doesn't seem to consider
                     jsr a2(16) as indirect
                     it jump directly into VMT in that case,
                     so we load the address of the method into
                     the base register PM
                     But that destroyed the self_pointer :(
                     so we need to use another register for doing this PM }
                   new(r2);
                   reset_reference(r2^);
                   if r^.base=self_pointer then
                     r2^.base:=getaddressreg
                   else
                     r2^.base:=r^.base;
                   emit_ref_reg(A_MOVE,S_L,r,r2^.base);
                   emit_ref(A_JSR,S_NO,r2);
                   { don't free self_pointer of course! }
                   { only free if allocated explicitly  }
                   { address register.                  }
                   if r2^.base <> r^.base then
                   ungetregister(r2^.base);
                   if r^.base <> self_pointer then
                      ungetregister(r^.base);
                end
              else if pocall_palmossyscall in p^.procdefinition^.proccalloptions then
                begin
                   emit_const(A_TRAP,S_NO,15);
                   exprasmlist^.concat(new(pai_const,init_16bit(
                     pprocdef(p^.procdefinition)^.extnumber)));
                end
              else if not inlined then
                begin
                  { We can call interrupts from within the smae code
                    by just pushing the flags and CS PM }
                  if (po_interrupt in p^.procdefinition^.procoptions) then
                    begin
                     { do nothing here - contrary to i386 version }
                    end;
                  emitcall(pprocdef(p^.procdefinition)^.mangledname);
                end
              else { inlined proc }
                { inlined code is in inlinecode }
                begin
                   (* { set poinline again }
                   include(p^.procdefinition^.proccalloptions,pocall_inline); *)
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
                     { do nothing here - contrary to i386 version }
                end;
              { procedure of object? }
              if (po_methodpointer in p^.procdefinition^.procoptions) then
                begin
                   { method pointer can't be in a register }
                   hregister:=R_NO;

                   { do some hacking if we call a method pointer }
                   { which is a class member                 }
                   { else SELF is overwritten !              }
                   if (p^.right^.location.reference.base=self_pointer) or
                      (p^.right^.location.reference.index=self_pointer) then
                     begin
                        del_reference(p^.right^.location.reference);
                        hreg := getexplicitregister32(R_A0);
                        emit_ref_reg(A_MOVE,S_L,
                          newreference(p^.right^.location.reference),hreg);
                        hregister:=hreg;
                     end;

                   { load self, but not if it's already explicitly pushed }
                   if not(po_containsself in p^.procdefinition^.procoptions) then
                     begin
                       { load SELF }
                       inc(p^.right^.location.reference.offset,target_os.size_of_pointer);
                       emit_ref_reg(A_MOVE,S_L,
                         newreference(p^.right^.location.reference),self_pointer);
                       dec(p^.right^.location.reference.offset,target_os.size_of_pointer);
                       { push self pointer }
                       emit_reg_reg(A_MOVE,S_L,self_pointer,R_SPPUSH);
                     end;

                   if hregister=R_NO then
                     begin
                       getexplicitregister32(R_A1);
                       emit_ref_reg(A_MOVE,S_L,newreference(p^.right^.location.reference),R_A1);
                       new(href);
                       reset_reference(href^);
                       href^.base := R_A1;
                       emit_ref(A_JSR,S_NO,href);
                       ungetregister(R_A1);
                     end
                   else
                     begin
                       { should always be an address register }
                       if not isaddressregister(hregister) then
                          internalerror(4555);
                       new(href);
                       reset_reference(href^);
                       href^.base := hregister;
                       emit_ref(A_JSR,S_NO,href);
                       ungetregister(hregister);
                     end;

                   del_reference(p^.right^.location.reference);
                   ungetiftemp(p^.right^.location.reference);
                end
              else
                begin
                   case p^.right^.location.loc of
                      LOC_REGISTER,LOC_CREGISTER:
                         begin
                            { make sure it is an address register }
                            if isaddressregister(p^.right^.location.register) then
                               begin
                                new(href);
                                reset_reference(href^);
                                href^.base := p^.right^.location.register;
                                emit_ref(A_JSR,S_NO,href)
                               end
                            else
                               begin
                                 hreg := getaddressreg;
                                 emit_reg_reg(A_MOVE,S_L,p^.right^.location.register,hreg);
                                 new(href);
                                 reset_reference(href^);
                                 href^.base := hreg;
                                 emit_ref(A_JSR,S_NO,href);
                                 ungetregister(hreg);
                               end;
                           ungetregister(p^.right^.location.register);
                         end
                      else
                       begin
                         { we first need to always load the address register value beforehand! }
                         { becase a JSR is not the destination address, but the value address  }
                         { where the destination address is stored!                            }
                         getexplicitregister32(R_A1);
                         emit_ref_reg(A_MOVE,S_L,newreference(p^.right^.location.reference),R_A1);
                         new(href);
                         reset_reference(href^);
                         href^.base := R_A1;
                         emit_ref(A_JSR,S_NO,href);
                         del_reference(p^.right^.location.reference);
                         ungetregister(R_A1);
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
                if pushedparasize<>0 then
                  begin
                    emit_const_reg(A_ADD,S_L,pushedparasize,stack_pointer);
                  end;
             end;
      dont_call:
         pushedparasize:=oldpushedparasize;
         unused:=unusedregisters;
         usablereg32:=usablecount;
{$ifdef TEMPREGDEBUG}
         testregisters32;
{$endif TEMPREGDEBUG}
         { C functions return pointers in A0 }
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{         if (pocall_cdecl in p^.procdefinition^.proccalloptions) and
           (p^.resulttype^.deftype=pointerdef) then
           emit_reg_reg(A_MOVE,S_L,R_A0,R_D0);}
         { This is required at least for Palmos...
           I don't know who removed it above, nor why... PM }
         if (pocall_palmossyscall in p^.procdefinition^.proccalloptions) and
            (p^.resulttype^.deftype=pointerdef) then
           emit_reg_reg(A_MOVE,S_L,R_A0,R_D0);

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
             emitlabeled(A_BEQ,faillabel);
           end;
         { handle function results }
         { structured results are easy to handle.... }
         { needed also when result_no_used !! }
         if (p^.resulttype<>pdef(voiddef)) and
            ret_in_param(p^.resulttype,p^.procdefinition^.proccalloptions) then
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
                   { the result is in the accumulator, contrary to the
                     80x86 where they are in the flags, the flags get
                     corrupt in the m68k because of move
                   }
                   p^.location.loc:=LOC_REGISTER;
{                   p^.location.resflags:=F_NE;}
                   hregister:=getexplicitregister32(accumulator);
                   emit_reg_reg(A_MOVE,S_L,accumulator,hregister);
                   p^.location.register:=hregister;
                end
               { structed results are easy to handle.... }
              else if ret_in_param(p^.resulttype,p^.procdefinition^.proccalloptions) then
                begin
                  { This is not necessary
                  if (pocall_cdecl in p^.procdefinition^.proccalloptions) then
                     ungetregister(R_A1);
                     as Unused has been reset to Unusedregisters
                     after dont_call label,
                     it even lead to an error in the DEBUG compiler version PM }
                   {p^.location.loc:=LOC_MEM;
                   stringdispose(p^.location.reference.symbol);
                   p^.location.reference:=funcretref;
                   already done above (PM) }
                end
              else
                begin
                   if (p^.resulttype^.deftype in [orddef,enumdef])then
                     begin
                        p^.location.loc:=LOC_REGISTER;
                        case p^.resulttype^.size of
                          3,4 :
                            begin
{                              hregister := getexplicitregister32(accumulator);}
                              hregister:=getregister32;
                              emit_reg_reg(A_MOVE,S_L,accumulator,hregister);
                              p^.location.register:=hregister;
                            end;
                          1 :
                            begin
{                              hregister := getexplicitregister32(accumulator);}
                              hregister:=getregister32;
                              emit_reg_reg(A_MOVE,S_B,accumulator,hregister);
                              p^.location.register:=hregister;
                            end;
                          2 :
                            begin
{                              hregister := getexplicitregister32(accumulator);}
                              hregister:=getregister32;
                              emit_reg_reg(A_MOVE,S_W,accumulator,hregister);
                              p^.location.register:=hregister;
                            end;
                           8 :
                             begin
                                hregister:=getregister32;
                                hregister2:=getregister32;
                                emit_reg_reg(A_MOVE,S_L,accumulator,hregister);
                                emit_reg_reg(A_MOVE,S_L,scratch_reg,hregister2);
                                p^.location.registerlow:=hregister2;{ low is d1 }
                                p^.location.registerhigh:=hregister; { high is d0 }
                             end;
                        else internalerror(7);
                     end

                end
              else if (p^.resulttype^.deftype=floatdef) then
                begin
                  case pfloatdef(p^.resulttype)^.typ of
                   s32real :
                      Begin                           { values are always returned in accumulator for 32-bit values }
                       if (cs_fp_emulation in aktmoduleswitches) then
                         begin
                           hregister:=getregister32;
                           emit_reg_reg(A_MOVE,S_L,accumulator,hregister);
                           p^.location.fpuregister:=hregister;
                           p^.location.loc:=LOC_FPU;
                         end
                       else
                         begin
                           { TRUE FPU mode }
                           getexplicitregister32(R_FP0);
                           p^.location.loc:=LOC_FPU;
                           { on exit of function result in R_FP0 }
                           p^.location.fpuregister:=R_FP0;
                         end;
                     end;
                  s64comp :
         { always emulated }
         Begin
         end;
       s64real :
         Begin
                       { TRUE FPU mode }
                       getexplicitregister32(R_FP0);
                       p^.location.loc:=LOC_FPU;
                       { on exit of function result in R_FP0 }
                       p^.location.fpuregister:=R_FP0;
         end;
       s80real :
         Begin
                       { TRUE FPU mode }
                       getexplicitregister32(R_FP0);
                       p^.location.loc:=LOC_FPU;
                       { on exit of function result in R_FP0 }
                       p^.location.fpuregister:=R_FP0;
         End;
       else
         internalerror(1);
       end; { end case }
                end
              else if is_ansistring(p^.resulttype) or
                is_widestring(p^.resulttype) then
                begin
                   hregister:=getexplicitregister32(accumulator);
                   emit_reg_reg(A_MOVE,S_L,accumulator,hregister);
                   gettempansistringreference(hr);
                   decrstringref(p^.resulttype,hr);
                   emit_reg_ref(A_MOVE,S_L,hregister,
                     newreference(hr));
                   ungetregister32(hregister);
                   p^.location.loc:=LOC_MEM;
                   p^.location.reference:=hr;
                end
              else if (p^.resulttype^.deftype in [recorddef,objectdef]) and
                      (p^.resulttype^.size<=4) then
                begin
                  p^.location.loc:=LOC_REFERENCE;
                  gettempofsizereference(p^.resulttype^.size,
                    p^.location.reference);
                  { WARNING, we might need to usee S_B and S_W here ... }
                  case p^.resulttype^.size of
                    1 : opsize:=S_B;
                    2 : opsize:=S_W;
                    else
                      opsize:=S_L;
                  end;
                  emit_reg_ref(A_MOVE,opsize,R_D0,newreference(p^.location.reference));
                end
              else
                begin
                   p^.location.loc:=LOC_REGISTER;
{                   hregister:=getexplicitregister32(accumulator);}
                   hregister:=getregister32;
                   emit_reg_reg(A_MOVE,S_L,accumulator,hregister);
                   p^.location.register:=hregister;
                end;
             end;
           end;

         { perhaps i/o check ? }
         if iolabel<>nil then
           begin
              emit_sym(A_PEA,S_L,iolabel);
              emitcall('FPC_IOCHECK');
           end;
         if pop_size>0 then
           emit_const_reg(A_ADD,S_L,pop_size,stack_pointer);

         { restore registers }
         restoreusedregisters(pushed);

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
                   { do nothing here.... }
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
  Revision 1.1.2.52  2003/03/07 12:14:21  pierre
   + new function cdecl_function_return_address_reg

  Revision 1.1.2.51  2003/01/22 08:58:45  pierre
   * fix wrong code in last commit

  Revision 1.1.2.50  2003/01/22 00:23:56  pierre
   * fix tcalobjX failure on static methods without vmt

  Revision 1.1.2.49  2003/01/21 11:53:48  pierre
   * fix web reports 2317 and 2318

  Revision 1.1.2.48  2003/01/20 15:47:50  pierre
   * pass vmt or 0 correctly to static object methods

  Revision 1.1.2.47  2003/01/14 21:27:42  peter
    * fixed tw2296

  Revision 1.1.2.46  2003/01/13 11:24:30  pierre
   * palmos syscall return pointers only in A0, copy it to D0

  Revision 1.1.2.45  2003/01/11 18:27:18  carl
    * unget temp for method pointers

  Revision 1.1.2.44  2002/12/04 23:43:19  pierre
   * fix tbs/tb0419 problem

  Revision 1.1.2.43  2002/11/26 10:13:26  pierre
   * fix a problem with self pointer in m68k

  Revision 1.1.2.42  2002/11/22 16:36:06  pierre
   * fix an error introduced in a previous fix for done inside methods

  Revision 1.1.2.41  2002/11/19 23:06:40  pierre
   * try to fix stack for cdecl'ared functions

  Revision 1.1.2.40  2002/11/18 16:53:54  pierre
   * handle non class objects as recorddefs for calling conventions

  Revision 1.1.2.39  2002/11/15 15:55:22  pierre
   * fix remaining m68k record with cdecl problems

  Revision 1.1.2.38  2002/11/15 14:37:40  pierre
   * fix cdecl record return values

  Revision 1.1.2.37  2002/11/15 14:10:17  pierre
    + added a calloptions parameter to ret_inacc and ret_in_param
      functions.
      This allows to handle win32 stdcall and m68k cdecl small records
      case.

  Revision 1.1.2.36  2002/11/04 13:03:41  pierre
   * any C array parameter is passed by reference

  Revision 1.1.2.35  2002/10/29 14:58:14  pierre
   * fix bug tw1863

  Revision 1.1.2.34  2002/10/18 11:26:01  pierre
   * fix bug in last commit, retoffset needs to be allocated for all inlined functions

  Revision 1.1.2.33  2002/10/18 10:55:42  pierre
   * recalculate the para_size for inlined procedures

  Revision 1.1.2.32  2002/10/15 19:45:38  pierre
   * fix pascal high parameter passing

  Revision 1.1.2.31  2002/10/15 16:15:32  carl
    * Fix for m68k version (there was 80x86 code in here!)

  Revision 1.1.2.30  2002/10/15 12:04:01  pierre
   * check for nil self with -Cr or -CR

  Revision 1.1.2.29  2002/10/15 06:16:29  pierre
   * final ? fix for tcalval9

  Revision 1.1.2.28  2002/10/07 19:43:53  pierre
   * m68k cdecl'ared function with structured results have address of result in A1

  Revision 1.1.2.27  2002/09/14 13:42:42  carl
    * small fix to previous commit
    * indirect call fixes for methods

  Revision 1.1.2.26  2002/09/13 18:54:04  carl
    * calling conventions related fixes
    * endian fixes with references
    * fixes of invalid emitted opcodes

  Revision 1.1.2.25  2002/09/12 19:52:08  carl
    * (m68k): bugfixes for 64-bit arithmetic, 64-bit shifts and 64-bit division
    * (m68k): bugfix for boolean type conversions
    * (m68k): endian problems fixes when src_Size = dst_Size with a different type

  Revision 1.1.2.24  2002/03/25 16:44:42  pierre
   * fix small error in register usage for C decl functions

  Revision 1.1.2.23  2001/10/02 22:30:04  pierre
   + allow multi level inline, fix bug 1623

  Revision 1.1.2.22  2001/09/12 23:55:11  pierre
   * int64 fixes

  Revision 1.1.2.21  2001/09/09 15:15:52  carl
  * patch for pushing previous framepointer in nested routines (from i386 version)

  Revision 1.1.2.20  2001/08/17 16:16:50  florian
    + support for PalmOS added

  Revision 1.1.2.19  2001/08/04 06:13:26  carl
  * CLR on address register is not valid

  Revision 1.1.2.18  2001/08/02 13:58:20  pierre
   * convert all move.l symbol into lea sym, probably just bug in aggas...

  Revision 1.1.2.17  2001/08/01 12:36:10  pierre
   * fix error in constructor inside constructor

  Revision 1.1.2.16  2001/07/31 23:30:17  pierre
   C functions use A1 register for struct return

  Revision 1.1.2.15  2001/07/29 20:35:29  pierre
   * improve fpu handling

  Revision 1.1.2.14  2001/07/18 23:50:42  pierre
   * correct method calling

  Revision 1.1.2.13  2001/07/17 07:22:16  pierre
   * use accumulator to zero out self pointer

  Revision 1.1.2.12  2001/05/21 03:32:04  carl
  + must explicilty allocate register after return value, otherwise it does not work :(
    (should be fixed sooner or later)

  Revision 1.1.2.11  2001/05/19 21:33:39  peter
    * function returning int64 inlining fixed

  Revision 1.1.2.10  2001/05/18 18:02:42  carl
  * problem with returning FPU values fixed when in non-emulation mode
  - removed FPU modes when in emulation mode

  Revision 1.1.2.9  2001/05/17 01:31:55  carl
  * bugfix of indirect calls, load the address first!

  Revision 1.1.2.8  2001/04/21 05:04:14  carl
  * corrected non-initialized register problem

  Revision 1.1.2.7  2001/04/19 11:37:35  carl
  * m68k updates (code generator compiles)

  Revision 1.1.2.6  2001/04/02 02:19:56  carl
  + Added explicit register allocation

  Revision 1.1.2.5  2001/03/27 02:55:12  carl
  - removed fixed support

  Revision 1.1.2.4  2001/03/24 21:33:45  carl
  + .register -> .fpuregister when LOC_FPU

  Revision 1.1.2.3  2001/03/23 01:17:42  carl
  + ported (uncompiled and untested)
}

