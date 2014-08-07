{
    $Id: cgmem.pas,v 1.1.2.23 2003/06/04 15:08:36 peter Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate i386 assembler for in memory related nodes

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
unit cgmem;
interface

    uses
      tree;

    procedure secondloadvmt(var p : ptree);
    procedure secondnewn(var p : ptree);
    procedure secondhdisposen(var p : ptree);
    procedure secondsimplenewdispose(var p : ptree);
    procedure secondaddr(var p : ptree);
    procedure seconddoubleaddr(var p : ptree);
    procedure secondderef(var p : ptree);
    procedure secondvecn(var p : ptree);
    procedure secondwith(var p : ptree);


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
      cga,tgen,cgbase;

{*****************************************************************************
                             SecondLoadVMT
*****************************************************************************}

    procedure secondloadvmt(var p : ptree);
      begin
         p^.location.register:=getaddressreg;
         emit_sym_ofs_reg(A_LEA,S_L,newasmsymbol
           (pobjectdef(pclassrefdef(p^.resulttype)^.pointertype.def)^.vmt_mangledname),0,
            p^.location.register);
      end;




{*****************************************************************************
                             SecondNewN
*****************************************************************************}

    procedure secondnewn(var p : ptree);
      var
         pushed : tpushed;
         r : preference;
      begin
         if assigned(p^.left) then
           begin
              secondpass(p^.left);
              p^.location.register:=p^.left^.location.register;
           end
         else
           begin
              saveusedregisters(pushed,ALL_REGISTERS);

              gettempofsizereference(target_os.size_of_pointer,p^.location.reference);

              { determines the size of the mem block }
              push_int(ppointerdef(p^.resulttype)^.pointertype.def^.size);
              emit_push_lea_loc(p^.location,false);
              emitcall('FPC_GETMEM');

              if ppointerdef(p^.resulttype)^.pointertype.def^.needs_inittable then
                begin
                   new(r);
                   reset_reference(r^);
                   r^.symbol:=ppointerdef(p^.resulttype)^.pointertype.def^.get_inittable_label;
                   emitpushreferenceaddr(r^);
                   dispose(r);
                   { push pointer we just allocated, we need to initialize the
                     data located at that pointer not the pointer self (PFV) }
                   emit_push_loc(p^.location);
                   emitcall('FPC_INITIALIZE');
                end;
              restoreusedregisters(pushed);
              { may be load self register }
              maybe_loadself;
           end;
         if codegenerror then
           exit;
      end;


{*****************************************************************************
                             SecondDisposeN
*****************************************************************************}

    procedure secondhdisposen(var p : ptree);
      begin
         secondpass(p^.left);
         if codegenerror then
           exit;
         reset_reference(p^.location.reference);
         case p^.left^.location.loc of
            LOC_REGISTER,
            LOC_CREGISTER:
              begin
                 if p^.left^.location.loc = LOC_REGISTER then
                   ungetregister(p^.left^.location.register);
                 p^.location.reference.base:=getaddressreg;
                 emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,
                   p^.location.reference.base);
              end;
            LOC_MEM,LOC_REFERENCE :
              begin
                 del_reference(p^.left^.location.reference);
                 p^.location.reference.base:=getaddressreg;
                 emit_ref_reg(A_MOVE,S_L,newreference(p^.left^.location.reference),
                   p^.location.reference.base);
              end;
         end;
      end;


{*****************************************************************************
                             SecondNewDispose
*****************************************************************************}

    procedure secondsimplenewdispose(var p : ptree);

      var
         pushed : tpushed;
         r : preference;

      begin
         secondpass(p^.left);
         if codegenerror then
           exit;

         saveusedregisters(pushed,ALL_REGISTERS);

         { call the mem handling procedures }
         case p^.treetype of
           simpledisposen:
             begin
                if ppointerdef(p^.left^.resulttype)^.pointertype.def^.needs_inittable then
                  begin
                     new(r);
                     reset_reference(r^);
                     r^.symbol:=ppointerdef(p^.left^.resulttype)^.pointertype.def^.get_inittable_label;
                     emitpushreferenceaddr(r^);
                     dispose(r);
                     { push pointer adress }
                     emit_push_loc(p^.left^.location);
                     emitcall('FPC_FINALIZE');
                  end;
                emit_push_lea_loc(p^.left^.location,true);
                emitcall('FPC_FREEMEM');
             end;
           simplenewn:
             begin
                { determines the size of the mem block }
                push_int(ppointerdef(p^.left^.resulttype)^.pointertype.def^.size);
                emit_push_lea_loc(p^.left^.location,true);
                emitcall('FPC_GETMEM');
                if ppointerdef(p^.left^.resulttype)^.pointertype.def^.needs_inittable then
                  begin
                     new(r);
                     reset_reference(r^);
                     r^.symbol:=ppointerdef(p^.left^.resulttype)^.pointertype.def^.get_inittable_label;
                     emitpushreferenceaddr(r^);
                     dispose(r);
                     emit_push_loc(p^.left^.location);
                     emitcall('FPC_INITIALIZE');
                  end;
             end;
         end;
         restoreusedregisters(pushed);
         { may be load self register }
         maybe_loadself;
      end;


{*****************************************************************************
                             SecondAddr
*****************************************************************************}

    procedure secondaddr(var p : ptree);
      begin
         secondpass(p^.left);

         { when loading procvar we do nothing with this node, so load the
           location of left }
         if p^.procvarload then
          begin
            set_location(p^.location,p^.left^.location);
            exit;
          end;

         p^.location.loc:=LOC_REGISTER;
         del_reference(p^.left^.location.reference);
         p^.location.register:=getregister32;
         {@ on a procvar means returning an address to the procedure that
           is stored in it.}
         { yes but p^.left^.symtableentry can be nil
           for example on @self !! }
         { symtableentry can be also invalid, if left is no tree node }
         if (m_tp_procvar in aktmodeswitches) and
           (p^.left^.treetype=loadn) and
           assigned(p^.left^.symtableentry) and
           (p^.left^.symtableentry^.typ=varsym) and
           (pvarsym(p^.left^.symtableentry)^.vartype.def^.deftype=procvardef) then
           emit_ref_reg(A_MOVE,S_L,
             newreference(p^.left^.location.reference),
             p^.location.register)
         else
     begin
             getexplicitregister32(R_A0);

             emit_ref_reg(A_LEA,S_L,newreference(p^.left^.location.reference),R_A0);
             emit_reg_reg(A_MOVE,S_L,R_A0,p^.location.register);

             ungetregister(R_A0);
     end;
      end;


{*****************************************************************************
                             SecondDoubleAddr
*****************************************************************************}

    procedure seconddoubleaddr(var p : ptree);
      begin
         secondpass(p^.left);
         p^.location.loc:=LOC_REGISTER;
         del_reference(p^.left^.location.reference);
         p^.location.register:=getregister32;

         getexplicitregister32(R_A0);
         emit_ref_reg(A_LEA,S_L,newreference(p^.left^.location.reference),R_A0);
   emit_reg_reg(A_MOVE,S_L,R_A0,p^.location.register);
         ungetregister(R_A0);
      end;


{*****************************************************************************
                             SecondDeRef
*****************************************************************************}

    procedure secondderef(var p : ptree);
      var
         hr : tregister;
      begin
         secondpass(p^.left);
         reset_reference(p^.location.reference);
         case p^.left^.location.loc of
            LOC_REGISTER:
              begin
                 { if we are currently not in an address register, it's a little }
                 { bit more complicated.                                         }
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
                 { ...and reserve one for the pointer }
                 hr:=getaddressreg;
                 emit_mov_ref_reg(S_L,
                   newreference(p^.left^.location.reference), hr);
                 p^.location.reference.base:=hr;
              end;
         end;
         if not ppointerdef(p^.left^.resulttype)^.is_far and
            (cs_gdb_heaptrc in aktglobalswitches) and
            (cs_checkpointer in aktglobalswitches) then
              begin
                 emit_reg_reg(A_MOVE,S_L,p^.location.reference.base,R_SPPUSH);
                 emitcall('FPC_CHECKPOINTER');
              end;
      end;




{*****************************************************************************
                               SecondVecN
*****************************************************************************}

    procedure secondvecn(var p : ptree);
      var
        is_pushed : boolean;
        ind,hr : tregister;
        _p : ptree;

          function get_mul_size:longint;
          begin
            if p^.memindex then
             get_mul_size:=1
            else
             begin
               if (p^.left^.resulttype^.deftype=arraydef) then
                get_mul_size:=parraydef(p^.left^.resulttype)^.elesize
               else
                get_mul_size:=p^.resulttype^.size;
             end
          end;

      procedure calc_emit_mul;

        var
           l1,l2 : longint;

        begin
           l1 := get_mul_size;
           case l1 of
              1     : p^.location.reference.scalefactor:=l1;
              2 : emit_const_reg(A_LSL,S_L,1,ind);
              4 : emit_const_reg(A_LSL,S_L,2,ind);
              8 : emit_const_reg(A_LSL,S_L,3,ind);
           else
             begin
               if ispowerof2(l1,l2) then
                   begin
                     { If the value is zero, don't emit the shift! }
                     if l2 <> 0 then
                       if l2 < 9 then 
                         emit_const_reg(A_LSL,S_L,l2,ind)
                       else begin
                         getexplicitregister32(R_D0);

                         emit_const_reg(A_MOVEQ,S_L,l2,R_D0);
                         emit_reg_reg(A_LSL,S_L,R_D0,ind);

                         ungetregister(R_D0);
                       end;
                   end
                   else
                 begin
                   { use normal MC68000 signed multiply }
                   if (l1 >= -32768) and (l1 <= 32767) then
                     emit_const_reg(A_MULS,S_W,l1,ind)
                   else
                   { use long MC68020 long multiply }
                   if (aktoptprocessor = MC68020) then
                     emit_const_reg(A_MULS,S_L,l1,ind)
                   else
                   { MC68000 long multiply }
                     begin
                       getexplicitregister32(R_D0);
                       getexplicitregister32(R_D1);

                       emit_const_reg(A_MOVE,S_L,l1,R_D0);
                       emit_reg_reg(A_MOVE,S_L,ind,R_D1);
                       emitcall('FPC_MUL_LONGINT');
                       emit_reg_reg(A_MOVE,S_L,R_D0,ind);

                       ungetregister(R_D0);
                       ungetregister(R_D1);
                     end;
                 end;
             end; { else case }
            end; { end case }
        end; { calc_emit_mul }



      var
         extraoffset : longint;
         { rl stores the resulttype of the left node, this is necessary }
         { to detect if it is an ansistring                          }
         { because in constant nodes which constant index              }
         { the left tree is removed                                  }
         t   : ptree;
         hp  : preference;
         href : treference;
         emitcheck : boolean;
         tai : Paicpu;
         pushed : tpushed;
         hightree : ptree;
         hl,otl,ofl : pasmlabel;
         hreg : tregister;
         reg : tregister;
         hregister : tregister;
      begin
         secondpass(p^.left);
         { we load the array reference to p^.location }

         { an ansistring needs to be dereferenced }
         if is_ansistring(p^.left^.resulttype) or
           is_widestring(p^.left^.resulttype) then
           begin
              reset_reference(p^.location.reference);
              if p^.callunique then
                begin
                   if p^.left^.location.loc<>LOC_REFERENCE then
                     begin
                        CGMessage(cg_e_illegal_expression);
                        exit;
                     end;
                   saveusedregisters(pushed,ALL_REGISTERS);
                   emitpushreferenceaddr(p^.left^.location.reference);
                   if is_ansistring(p^.left^.resulttype) then
                     emitcall('FPC_ANSISTR_UNIQUE')
                   else
                     emitcall('FPC_WIDESTR_UNIQUE');
                   maybe_loadself;
                   restoreusedregisters(pushed);
                end;

              if p^.left^.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                begin
                   if not isaddressregister(p^.left^.location.register) then
                     begin
                      p^.location.reference.base:=getaddressreg;
                       emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,
                         p^.location.reference.base);
                     end
                   else
                      p^.location.reference.base:=p^.left^.location.register;
                end
              else
                begin
                   del_reference(p^.left^.location.reference);
                   p^.location.reference.base:=getaddressreg;
                   emit_ref_reg(A_MOVE,S_L,
                     newreference(p^.left^.location.reference),
                     p^.location.reference.base);
                end;

              { check for a zero length string,
                we can use the ansistring routine here }
              if (cs_check_range in aktlocalswitches) then
                begin
                   saveusedregisters(pushed,ALL_REGISTERS);
                   emit_reg_reg(A_MOVE,S_L,p^.location.reference.base,R_SPPUSH);
                   emitcall('FPC_ANSISTR_CHECKZERO');
                   maybe_loadself;
                   restoreusedregisters(pushed);
                end;

              if is_ansistring(p^.left^.resulttype) then
                { in ansistrings S[1] is pchar(S)[0] !! }
                dec(p^.location.reference.offset)
              else
                begin
                   { in widestrings S[1] is pwchar(S)[0] !! }
                   dec(p^.location.reference.offset,2);
                   emit_const_reg(A_LSL,S_L,
                     1,p^.location.reference.base);
                end;

              { we've also to keep left up-to-date, because it is used   }
              { if a constant array index occurs, subject to change (FK) }
              set_location(p^.left^.location,p^.location);
           end
         else
           set_location(p^.location,p^.left^.location);

         { offset can only differ from 0 if arraydef }
         if p^.left^.resulttype^.deftype=arraydef then
           dec(p^.location.reference.offset,
               get_mul_size*parraydef(p^.left^.resulttype)^.lowrange);
         if p^.right^.treetype=ordconstn then
           begin
              { offset can only differ from 0 if arraydef }
              if (p^.left^.resulttype^.deftype=arraydef) then
                begin
                   if not(is_open_array(p^.left^.resulttype)) and
                      not(is_array_of_const(p^.left^.resulttype)) then
                     begin
                        if (p^.right^.value>parraydef(p^.left^.resulttype)^.highrange) or
                           (p^.right^.value<parraydef(p^.left^.resulttype)^.lowrange) then
                           begin
                              if (cs_check_range in aktlocalswitches) then
                                CGMessage(parser_e_range_check_error)
                              else
                                CGMessage(parser_w_range_check_error);
                           end;
                        dec(p^.left^.location.reference.offset,
                            get_mul_size*parraydef(p^.left^.resulttype)^.lowrange);
                     end
                   else
                     begin
                       emitcheck:=true;
                       if (p^.left^.treetype=loadn) and
                          (p^.left^.symtable^.symtabletype in [parasymtable,inlineparasymtable]) and
                          (pocall_cdecl in
                            pprocdef(pvarsym(p^.left^.symtableentry)^.owner^.defowner)^.proccalloptions) then
                          begin
                            { no range checks for C arrays }
                          end
                       else
                         begin
                           reset_reference(href);
                           parraydef(p^.left^.resulttype)^.genrangecheck;
                           href.symbol:=newasmsymbol(parraydef(p^.left^.resulttype)^.getrangecheckstring);
                           href.offset:=4;
                           getsymonlyin(p^.left^.symtable,'high'+pvarsym(p^.left^.symtableentry)^.name);
                           hightree:=genloadnode(pvarsym(srsym),p^.left^.symtable);
                           firstpass(hightree);
                           secondpass(hightree);
    {                        emit_mov_loc_ref(hightree^.location,href,S_L,true);}
    { INLINED:                                                                  }
                            case hightree^.location.loc of
                             LOC_REGISTER,
                             LOC_CREGISTER :
                                     Begin
                                      { the register is not needed anymore }
                                      emit_reg_ref(A_MOVE,S_L,
                                          hightree^.location.register,newreference(href));
                                      ungetregister32(hightree^.location.register);
                                     end;
                             LOC_MEM,
                             LOC_REFERENCE :
                                     Begin
                                       if hightree^.location.reference.is_immediate then
                                          emit_const_ref(A_MOVE,S_L,
                                          hightree^.location.reference.offset,newreference(href))
                                      else
                                        begin
                                          hreg := getexplicitregister32(accumulator);
                                          emit_ref_reg(A_MOVE,S_L,
                                            newreference(hightree^.location.reference),hreg);
                                          del_reference(hightree^.location.reference);
                                          emit_reg_ref(A_MOVE,S_L,
                                              hreg,newreference(href));
                                          del_reference(href);
                                          ungetregister(hreg);
                                        end;
                                        ungetiftemp(hightree^.location.reference);
                                    end;
                            else
                              internalerror(330);
                            end; { end case }
                           disposetree(hightree);
                           p^.right^.location.loc:=LOC_REFERENCE;
                           p^.right^.location.reference.is_immediate:=true;
                           p^.right^.location.reference.offset:=p^.right^.value;
                           emitrangecheck(p^.right,p^.left^.resulttype);
                         end;
                     end;
                end
              else if (p^.left^.resulttype^.deftype=stringdef) then
                begin
                   if (p^.right^.value=0) and not(is_shortstring(p^.left^.resulttype)) then
                     CGMessage(cg_e_can_access_element_zero);

                   if (cs_check_range in aktlocalswitches) then
                     case pstringdef(p^.left^.resulttype)^.string_typ of
                        { it's the same for ansi- and wide strings }
                        st_widestring,
                        st_ansistring:
                          begin
                             saveusedregisters(pushed,ALL_REGISTERS);
                             push_int(p^.right^.value);
                             hp:=newreference(p^.location.reference);
                             dec(hp^.offset,7);
                             emit_ref_reg(A_MOVE,S_L,hp,R_SPPUSH);
                             emitcall('FPC_ANSISTR_RANGECHECK');
                             restoreusedregisters(pushed);
                             maybe_loadself;
                          end;

                        st_shortstring:
                          begin
                             {!!!!!!!!!!!!!!!!!}
                          end;

                        st_longstring:
                          begin
                             {!!!!!!!!!!!!!!!!!}
                          end;
                     end;
                end;
              if not(pocall_inline in aktprocsym^.definition^.proccalloptions) then
                begin
                  inc(p^.left^.location.reference.offset,
                      get_mul_size*p^.right^.value);
                  p^.left^.resulttype:=p^.resulttype;
                  disposetree(p^.right);
                  _p:=p^.left;
                  putnode(p);
                  p:=_p;
                end
              else
                {we need to keep the right constant to avoid bug 1938 }
                begin
                  inc(p^.location.reference.offset,
                      get_mul_size*p^.right^.value);
                end;
           end
         else
         { not treetype=ordconstn }
           begin
              { quick hack, to overcome Delphi 2 }
              if (cs_regalloc in aktglobalswitches) and
              { if we do range checking, we don't }
              { need that fancy code (it would be }
              { buggy)                            }
                not(cs_check_range in aktlocalswitches) and
                (p^.left^.resulttype^.deftype=arraydef) then
                begin
                   extraoffset:=0;
                   if (p^.right^.treetype=addn) then
                     begin
                        if p^.right^.right^.treetype=ordconstn then
                          begin
                             extraoffset:=p^.right^.right^.value;
                             t:=p^.right^.left;
                             { First pass processed this with the assumption   }
                             { that there was an add node which may require an }
                             { extra register. Fake it or die with IE10 (JM)   }
                             t^.registers32 :=  p^.right^.registers32;
                             putnode(p^.right^.right);
                             putnode(p^.right);
                             p^.right:=t;
                          end
                        else if p^.right^.left^.treetype=ordconstn then
                          begin
                             extraoffset:=p^.right^.left^.value;
                             t:=p^.right^.right;
                             t^.registers32 :=  p^.right^.registers32;
                             putnode(p^.right^.left);
                             putnode(p^.right);
                             p^.right:=t;
                          end;
                     end
                   else if (p^.right^.treetype=subn) then
                     begin
                        if p^.right^.right^.treetype=ordconstn then
                          begin
                             extraoffset:=-p^.right^.right^.value;
                             t:=p^.right^.left;
                             t^.registers32 :=  p^.right^.registers32;
                             putnode(p^.right^.right);
                             putnode(p^.right);
                             p^.right:=t;
                          end
                     end;
                   inc(p^.location.reference.offset,
                       get_mul_size*extraoffset);
                end;
              { calculate from left to right }
              if (p^.location.loc<>LOC_REFERENCE) and
                 (p^.location.loc<>LOC_MEM) then
                CGMessage(cg_e_illegal_expression);
              if (p^.right^.location.loc=LOC_JUMP) then
               begin
                 otl:=truelabel;
                 getlabel(truelabel);
                 ofl:=falselabel;
                 getlabel(falselabel);
               end;
              is_pushed:=maybe_push(p^.right^.registers32,p,false);
              secondpass(p^.right);
              if is_pushed then
                restore(p,false);
              { here we change the location of p^.right
                and the update was forgotten so it
                led to wrong code in emitrangecheck later PM
                so make range check before }

              if cs_check_range in aktlocalswitches then
               begin
                 if p^.left^.resulttype^.deftype=arraydef then
                   begin
                     emitcheck:=true;
                     if (p^.left^.treetype=loadn) and
                        (p^.left^.symtable^.symtabletype in [parasymtable,inlineparasymtable]) and
                        (pocall_cdecl in pprocdef(pvarsym(p^.left^.symtableentry)^.
                           owner^.defowner)^.proccalloptions) then
                        begin
                          { no range checks for C arrays }
                          emitcheck:=false;
                        end
                     else if is_open_array(p^.left^.resulttype) or
                          is_array_of_const(p^.left^.resulttype) then
                        begin
                          reset_reference(href);
                          parraydef(p^.left^.resulttype)^.genrangecheck;
                          href.symbol:=newasmsymbol(parraydef(p^.left^.resulttype)^.getrangecheckstring);
                          href.offset:=4;
                          getsymonlyin(p^.left^.symtable,'high'+pvarsym(p^.left^.symtableentry)^.name);
                          hightree:=genloadnode(pvarsym(srsym),p^.left^.symtable);
                          firstpass(hightree);
                          is_pushed:=maybe_push(hightree^.registers32,p,false);
                          secondpass(hightree);
  {                        emit_mov_loc_ref(hightree^.location,href,S_L,true);}
  { INLINED:                                                                  }
                          case hightree^.location.loc of
                           LOC_REGISTER,
                           LOC_CREGISTER :
                                   Begin
                                    { the register is not needed anymore }
                                    emit_reg_ref(A_MOVE,S_L,
                                        hightree^.location.register,newreference(href));
                                    ungetregister32(hightree^.location.register);
                                   end;
                           LOC_MEM,
                           LOC_REFERENCE :
                                   Begin
                                     if hightree^.location.reference.is_immediate then
                                        emit_const_ref(A_MOVE,S_L,
                                        hightree^.location.reference.offset,newreference(href))
                                    else
                                      begin
                                        hreg := getexplicitregister32(accumulator);
                                        emit_ref_reg(A_MOVE,S_L,
                                          newreference(hightree^.location.reference),hreg);
                                        del_reference(hightree^.location.reference);
                                        emit_reg_ref(A_MOVE,S_L,
                                            hreg,newreference(href));
                                        del_reference(href);
                                        ungetregister(hreg);
                                      end;
                                      ungetiftemp(hightree^.location.reference);
                                  end;
                          else
                            internalerror(330);
                          end; { end case }
                          if is_pushed then
                            restore(p,false);
                          disposetree(hightree);
                        end;
                     if emitcheck then
                       emitrangecheck(p^.right,p^.left^.resulttype);
                   end;
               end;

              case p^.right^.location.loc of
                 LOC_REGISTER:
                   begin
                      ind:=p^.right^.location.register;
                      case p^.right^.resulttype^.size of
                         1:
                           begin
                              emit_const_reg(A_AND,S_L,$ff,ind);
                           end;
                         2:
                           begin
                              emit_const_reg(A_AND,S_L,$ffff,ind);
                           end;
                      end;
                   end;
                 LOC_CREGISTER:
                   begin
                      ind:=getregister32;
                      case p^.right^.resulttype^.size of
                         1:
                           Begin
                             emit_reg_reg(A_MOVE,S_B,p^.right^.location.register,ind);
                             { zero extend }
                             emit_const_reg(A_AND,S_L,$FF,ind);
                           End;
                         2:
                           Begin
                             emit_reg_reg(A_MOVE,S_W,p^.right^.location.register,ind);
                             { zero extend }
                             emit_const_reg(A_AND,S_L,$FFFF,ind);
                           End;
                         4:
                           emit_reg_reg(A_MOVE,S_L,p^.right^.location.register,ind);
                      end;
                   end;
                 LOC_FLAGS:
                   begin
                      ind:=getregister32;
                      emit_flag2reg(p^.right^.location.resflags,ind);
                      emit_const_reg(A_AND,S_L,$FF,ind);
                   end;
                 LOC_JUMP :
                   begin
                     ind:=getregister32;
                     emitlab(truelabel);
                     truelabel:=otl;
                     emit_const_reg(A_MOVEQ,S_L,1,ind);
                     getlabel(hl);
                     emitjmp(C_None,hl);
                     emitlab(falselabel);
                     falselabel:=ofl;
                     emit_reg(A_CLR,S_L,ind);
                     emitlab(hl);
                   end;
                 LOC_REFERENCE,LOC_MEM :
                   begin
                      del_reference(p^.right^.location.reference);
                      ind:=getregister32;
                      { Booleans are stored in an 8 bit memory location, so
                        the use of MOVL is not correct }
                      case p^.right^.resulttype^.size of
                       1 :
                       Begin
                         emit_ref_reg(A_MOVE,S_B,newreference(p^.right^.location.reference),ind);
                         emit_const_reg(A_AND,S_L,$FF,ind);
                       End;
                       2 :
                       Begin
                         emit_ref_reg(A_MOVE,S_W,newreference(p^.right^.location.reference),ind);
                         emit_const_reg(A_AND,S_L,$FFFF,ind);
                       End;
                       4 :
                         emit_ref_reg(A_MOVE,S_L,newreference(p^.right^.location.reference),ind);
                      end;
                   end;
                 else
                   internalerror(5913428);
                end;

            { produce possible range check code: }
              if cs_check_range in aktlocalswitches then
               begin
                 if p^.left^.resulttype^.deftype=arraydef then
                   begin
                     { done defore (PM) }
                   end
                 else if (p^.left^.resulttype^.deftype=stringdef) then
                   begin
                      case pstringdef(p^.left^.resulttype)^.string_typ of
                         { it's the same for ansi- and wide strings }
                         st_widestring,
                         st_ansistring:
                           begin
                              saveusedregisters(pushed,ALL_REGISTERS);
                              emit_reg_reg(A_MOVE,S_L,ind,R_SPPUSH);
                              hp:=newreference(p^.location.reference);
                              dec(hp^.offset,7);
                              emit_ref_reg(A_MOVE,S_L,hp,R_SPPUSH);
                              emitcall('FPC_ANSISTR_RANGECHECK');
                              restoreusedregisters(pushed);
                              maybe_loadself;
                           end;
                         st_shortstring:
                           begin
                              {!!!!!!!!!!!!!!!!!}
                           end;
                         st_longstring:
                           begin
                              {!!!!!!!!!!!!!!!!!}
                           end;
                      end;
                   end;
               end;

              { ------------------------ HANDLE INDEXING ----------------------- }
              { In Motorola 680x0 mode, displacement can only be of 64K max.     }
              { Therefore instead of doing a direct displacement, we must first  }
              { load the new address into an address register. Therefore the     }
              { symbol is not used.                                              }
              if assigned(p^.location.reference.symbol) then
               begin
                 if not (tf_static_a5_based in target_info.flags) and
                    (p^.location.reference.base <> R_NO) then
                   CGMessage(cg_f_secondvecn_base_defined_twice);

                 hregister:=getaddressreg;

                 emit_ref_reg(A_LEA,S_L,newreference(p^.location.reference),
                    hregister);
                 p^.location.reference.base := hregister;
                 { we don't need the symbol anymore }
                 p^.location.reference.symbol := nil;
                 p^.location.reference.offset := 0;
               end;


              if p^.location.reference.index=R_NO then
               begin
                 p^.location.reference.index:=ind;
                 calc_emit_mul;
                 { here we must check for the offset      }
                 { and if out of bounds for the motorola  }
                 { eg: out of signed d8 then reload index }
                 { with correct value.                    }
                 if p^.location.reference.offset > 127 then
                   begin
                     emit_const_reg(A_ADD,S_L,p^.location.reference.offset,ind);
                     p^.location.reference.offset := 0;
                   end
                 else if p^.location.reference.offset < -128 then
                   begin
                     emit_const_reg(A_SUB,S_L,-p^.location.reference.offset,ind);
                     p^.location.reference.offset := 0;
                   end;
               end
              else
               begin
                 if p^.location.reference.base=R_NO then
                  begin
                    case p^.location.reference.scalefactor of
                     2 : emit_const_reg(A_LSL,S_L,1,p^.location.reference.index);
                     4 : emit_const_reg(A_LSL,S_L,2,p^.location.reference.index);
                     8 : emit_const_reg(A_LSL,S_L,3,p^.location.reference.index);
                    end;
                    calc_emit_mul;
                    { we must use address register to put index in base }
                    { compare with cgi386.pas                           }

                    reg := getaddressreg;
                    p^.location.reference.base := reg;

                    emit_reg_reg(A_MOVE,S_L,p^.location.reference.index,reg);
                    ungetregister(p^.location.reference.index);

                    p^.location.reference.index:=ind;
                  end
                 else
                  begin
                    { the operand is in memory - reload it into }
                    { a base register.                          }
                    reg := getaddressreg;
                    { Sometimes base is not an address reg, is this a bug ? PM }
                    if not isaddressregister(p^.location.reference.base) then
                      begin
                        {emit_reg_reg(A_MOVE,S_L,p^.location.reference.base,reg);
                        p^.location.reference.base:=reg;}
                        internalerror(998899);
                      end;
                    emit_ref_reg(A_LEA,S_L,newreference(p^.location.reference),reg);

                    ungetregister32(p^.location.reference.base);
                    { the symbol offset is loaded,             }
                    { so release the symbol name and set symbol  }
                    { to nil                                 }
                    p^.location.reference.symbol:=nil;
                    p^.location.reference.offset:=0;
                    calc_emit_mul;
                    p^.location.reference.base:=reg;
                    ungetregister32(p^.location.reference.index);
                    p^.location.reference.index:=ind;
                  end;
               end;

           end;
      end;



{*****************************************************************************
                               SecondWithN
*****************************************************************************}

    procedure secondwith(var p : ptree);
      var
        usetemp,with_expr_in_temp : boolean;
        symtable : pwithsymtable;
        i : longint;
{$ifdef GDB}
        withstartlabel,withendlabel : pasmlabel;
        pp : pchar;
        mangled_length  : longint;

      const
        withlevel : longint = 0;
{$endif GDB}
      begin
         if assigned(p^.left) then
            begin
               secondpass(p^.left);

               new(p^.withreference);

               usetemp:=false;
               if (p^.left^.treetype=loadn) and
                  (p^.left^.symtable=aktprocsym^.definition^.localst) then
                 begin
                    { for locals use the local storage }
                    p^.withreference^:=p^.left^.location.reference;
                    p^.islocal:=true;
                    release_loc(p^.left^.location);
                 end
               else
                { call can have happend with a property }
                if (p^.left^.resulttype^.deftype=objectdef) and
                   pobjectdef(p^.left^.resulttype)^.is_class then
                 begin
                    getexplicitregister32(R_A0);
                    emit_mov_loc_reg(p^.left^.location,R_A0);
                    if p^.left^.location.loc in [LOC_MEM,LOC_REFERENCE] then
                      release_loc(p^.left^.location);
                    usetemp:=true;
                 end
               else
                 begin
                   getexplicitregister32(R_A0);
                   emit_lea_loc_reg(p^.left^.location,R_A0,false);
                   release_loc(p^.left^.location);
                   usetemp:=true;
                 end;

               symtable:=p^.withsymtable;
               for i:=1 to p^.tablecount do
                 begin
                    if (p^.left^.treetype=loadn) and
                       (p^.left^.symtable=aktprocsym^.definition^.localst) then
                      symtable^.direct_with:=true;
                    symtable^.withnode:=p;
                    symtable:=pwithsymtable(symtable^.next);
                  end;

               { if the with expression is stored in a temp    }
               { area we must make it persistent and shouldn't }
               { release it (FK)                               }
               if (p^.left^.location.loc in [LOC_MEM,LOC_REFERENCE]) and
                 istemp(p^.left^.location.reference) then
                 begin
                    with_expr_in_temp:=normaltemptopersistant(p^.left^.location.reference.offset);
                 end
               else
                 with_expr_in_temp:=false;

               { if usetemp is set the value must be in %edi }
               if usetemp then
                begin
                  gettempofsizereference(4,p^.withreference^);
                  normaltemptopersistant(p^.withreference^.offset);
                  { move to temp reference }
                  emit_reg_ref(A_MOVE,S_L,R_A0,newreference(p^.withreference^));
                  ungetregister(R_A0);
{$ifdef GDB}
                  if (cs_debuginfo in aktmoduleswitches) then
                    begin
                      inc(withlevel);
                      getlabel(withstartlabel);
                      getlabel(withendlabel);
                      emitlab(withstartlabel);
                      withdebuglist^.concat(new(pai_stabs,init(strpnew(
                         '"with'+tostr(withlevel)+':'+tostr(symtablestack^.getnewtypecount)+
                         '=*'+p^.left^.resulttype^.numberstring+'",'+
                         tostr(N_LSYM)+',0,0,'+tostr(p^.withreference^.offset)))));
                      mangled_length:=length(aktprocsym^.definition^.mangledname);
                      getmem(pp,mangled_length+50);
                      strpcopy(pp,'192,0,0,'+withstartlabel^.name);
                      if (target_os.use_function_relative_addresses) then
                        begin
                          strpcopy(strend(pp),'-');
                          strpcopy(strend(pp),aktprocsym^.definition^.mangledname);
                        end;
                      withdebuglist^.concat(new(pai_stabn,init(strnew(pp))));
                    end;
{$endif GDB}
                end;

               { p^.right can be optimize out !!! }
               if assigned(p^.right) then
                 secondpass(p^.right);

               if usetemp then
                 begin
                   ungetpersistanttemp(p^.withreference^.offset);
{$ifdef GDB}
                   if (cs_debuginfo in aktmoduleswitches) then
                     begin
                       emitlab(withendlabel);
                       strpcopy(pp,'224,0,0,'+withendlabel^.name);
                      if (target_os.use_function_relative_addresses) then
                        begin
                          strpcopy(strend(pp),'-');
                          strpcopy(strend(pp),aktprocsym^.definition^.mangledname);
                        end;
                       withdebuglist^.concat(new(pai_stabn,init(strnew(pp))));
                       freemem(pp,mangled_length+50);
                       dec(withlevel);
                     end;
{$endif GDB}
                 end;

               if with_expr_in_temp then
                 ungetpersistanttemp(p^.left^.location.reference.offset);

               dispose(p^.withreference);
               p^.withreference:=nil;
            end;
       end;


end.
{
  $Log: cgmem.pas,v $
  Revision 1.1.2.23  2003/06/04 15:08:36  peter
    * lsl patch from Karoly Barlogh

  Revision 1.1.2.22  2002/12/10 23:00:09  pierre
   * palmos specific change for pic code

  Revision 1.1.2.21  2002/11/19 00:48:59  pierre
   * fix tbs/tb0419 cdecl open array problem

  Revision 1.1.2.20  2002/10/29 17:12:42  pierre
   * fix tw1908 bug

  Revision 1.1.2.19  2002/10/29 15:00:34  pierre
   * fix inlined with nodes

  Revision 1.1.2.18  2002/10/22 10:13:51  pierre
   * adapt to changes in aasm and temp_gen units

  Revision 1.1.2.17  2002/09/15 16:41:50  carl
    * alignment fixes for constant dat
    * tabs now correct for m68k
    * many cg fixes (don't remember all)

  Revision 1.1.2.16  2002/09/05 16:41:57  carl
    * bugfix 2072 (also fixed in main)
    * fixes for bug 1938 for m68k version

  Revision 1.1.2.15  2001/09/17 20:59:25  pierre
    one more fix to avoid double unget

  Revision 1.1.2.14  2001/08/04 06:13:55  carl
  * Corrected problem with shifts of zeros with constants

  Revision 1.1.2.13  2001/08/03 11:41:48  pierre
   * check if base is an address reg added

  Revision 1.1.2.12  2001/08/02 13:58:21  pierre
   * convert all move.l symbol into lea sym, probably just bug in aggas...

  Revision 1.1.2.11  2001/07/24 14:32:58  pierre
   * try to fix the internalerror(10) on hightree

  Revision 1.1.2.10  2001/07/14 00:04:40  pierre
   * register better released in secondhdisposen

  Revision 1.1.2.9  2001/07/13 23:57:15  pierre
   * set the reference base instead of index and use an address reg

  Revision 1.1.2.8  2001/06/13 03:07:38  carl
  * fix problems for sign extension with references (CLR cannot be used)

  Revision 1.1.2.7  2001/05/21 17:05:48  carl
  * correct bug with FPC_LONG_MUL (renamed)

  Revision 1.1.2.6  2001/05/17 01:32:51  carl
  * when loading a reference , the symbol was never set to nil.

  Revision 1.1.2.5  2001/05/09 03:47:59  carl
  * corrected problem with address registers (was using data register as base)

  Revision 1.1.2.4  2001/04/19 11:37:37  carl
  * m68k updates (code generator compiles)

  Revision 1.1.2.3  2001/04/02 02:21:22  carl
  + ported


}

