{
    $Id: cga.pas,v 1.1.2.40 2003/05/06 18:22:23 peter Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    Helper routines for the i386 code generator

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

 ****************************************************************************}

unit cga;

  interface

    uses
       cobjects,tree,
       cpubase,cpuasm,
       symconst,symtable,aasm;

{$define TESTGETTEMP to store const that
 are written into temps for later release PM }

    function def_opsize(p1:pdef):topsize;
    function def2def_opsize(p1,p2:pdef):topsize;
    function def_getreg(p1:pdef):tregister;
    function makereg8(r:tregister):tregister;
    function makereg16(r:tregister):tregister;
    function makereg32(r:tregister):tregister;


    procedure locflags2reg(var l:tlocation;opsize:topsize);
    procedure locjump2reg(var l:tlocation;opsize:topsize; otl, ofl: pasmlabel);


    procedure emitlab(var l : pasmlabel);
    procedure emitjmp(c : tasmcond;var l : pasmlabel);
    procedure emit_flag2reg(flag:tresflags;hregister:tregister);

    procedure emit_none(i : tasmop;s : topsize);

    procedure emit_const(i : tasmop;s : topsize;c : longint);
    procedure emit_reg(i : tasmop;s : topsize;reg : tregister);
    procedure emit_ref(i : tasmop;s : topsize;ref : preference);

    procedure emit_const_reg(i : tasmop;s : topsize;c : longint;reg : tregister);
    procedure emit_const_ref(i : tasmop;s : topsize;c : longint;ref : preference);
    procedure emit_ref_reg(i : tasmop;s : topsize;ref : preference;reg : tregister);
    procedure emit_mov_ref_reg(s : topsize;ref : preference;reg : tregister);
    procedure emit_reg_ref(i : tasmop;s : topsize;reg : tregister;ref : preference);
    procedure emit_reg_reg(i : tasmop;s : topsize;reg1,reg2 : tregister);
    procedure emit_mov_reg_reg(s : topsize;reg1,reg2 : tregister);

    procedure emit_const_reg_reg(i : tasmop;s : topsize;c : longint;reg1,reg2 : tregister);
    procedure emit_reg_reg_reg(i : tasmop;s : topsize;reg1,reg2,reg3 : tregister);


    procedure emit_sym(i : tasmop;s : topsize;op : pasmsymbol);
    procedure emit_sym_ofs(i : tasmop;s : topsize;op : pasmsymbol;ofs : longint);
    procedure emit_sym_ofs_reg(i : tasmop;s : topsize;op : pasmsymbol;ofs:longint;reg : tregister);
    procedure emit_sym_ofs_ref(i : tasmop;s : topsize;op : pasmsymbol;ofs:longint;ref : preference);

    procedure emitcall(const routine:string);

    procedure emit_mov_loc_ref(const t:tlocation;const ref:treference;siz:topsize;freetemp:boolean);
    procedure emit_mov_loc_reg(const t:tlocation;reg:tregister);
    procedure emit_mov_ref_reg64(r : treference;rl,rh : tregister);
    procedure emit_lea_loc_ref(const t:tlocation;const ref:treference;freetemp:boolean);
    procedure emit_lea_loc_reg(const t:tlocation;reg:tregister;freetemp:boolean);
    procedure emit_push_loc(const t:tlocation);
    procedure emit_push_mem_size(const t: treference; size: longint);

    { pushes qword location to the stack }
    procedure emit_pushq_loc(const t : tlocation);
    procedure release_qword_loc(const t : tlocation);

    { remove non regvar registers in loc from regs (in the format }
    { pushusedregisters uses)                                     }
    procedure remove_non_regvars_from_loc(const t: tlocation; var regs: byte);
    { releases the registers of a location }
    procedure release_loc(const t : tlocation);

    procedure emit_pushw_loc(const t:tlocation);
    procedure emit_push_lea_loc(const t:tlocation;freetemp:boolean);
    procedure emit_to_mem(var p:ptree);
    procedure emit_to_reg16(var hr:tregister);
    procedure emit_to_reg32(var hr:tregister);
    procedure emit_mov_reg_loc(reg: TRegister; const t:tlocation);
    procedure emit_movq_reg_loc(reghigh,reglow: TRegister;t:tlocation);

    procedure copyshortstring(const dref,sref : treference;len : byte;
                        loadref, del_sref: boolean);
    procedure loadansistring(p : ptree);

    procedure finalize(t : pdef;const ref : treference;is_already_ref : boolean);
    procedure incrstringref(t : pdef;const ref : treference);
    procedure decrstringref(t : pdef;const ref : treference);

    function maybe_push(needed : byte;p : ptree;isint64 : boolean) : boolean;
    function maybe_pushfpu(needed : byte;p : ptree) : boolean;
    procedure push_int(l : longint);
    procedure emit_push_mem(const ref : treference);
    procedure emitpushreferenceaddr(const ref : treference);
    procedure pushsetelement(p : ptree);
    procedure restore(p : ptree;isint64 : boolean);
    procedure push_value_para(p:ptree;inlined,is_cdecl:boolean;
                              para_offset:longint;alignment : longint);

{$ifdef TEMPS_NOT_PUSH}
    { does the same as restore/, but uses temp. space instead of pushing }
    function maybe_push(needed : byte;p : ptree;isint64 : boolean) : boolean;
    procedure restorefromtemp(p : ptree;isint64 : boolean);
{$endif TEMPS_NOT_PUSH}


    procedure floatload(t : tfloattype;const ref : treference);
    procedure floatstore(t : tfloattype;const ref : treference);
    procedure floatloadops(t : tfloattype;var op : tasmop;var s : topsize);
    procedure floatstoreops(t : tfloattype;var op : tasmop;var s : topsize);

    procedure maybe_loadself;
    procedure maketojumpbool(p : ptree);
    procedure emitloadord2reg(const location:Tlocation;orddef:Porddef;destreg:Tregister;delloc:boolean);
    procedure emitoverflowcheck(p:ptree);
    procedure emitrangecheck(p:ptree;todef:pdef);
    procedure concatcopy(source,dest : treference;size : longint;delsource : boolean;loadref:boolean);
    procedure firstcomplex(p : ptree);

    procedure genentrycode(alist : paasmoutput;const proc_names:Tstringcontainer;make_global:boolean;
                           stackframe:longint;
                           var parasize:longint;var nostackframe:boolean;
                           inlined : boolean;
                           var savedstackoffset : longint);
    procedure genexitcode(alist : paasmoutput;parasize:longint;
                          nostackframe,inlined:boolean;savedstackoffset : longint);

    { if a unit doesn't have a explicit init/final code,  }
    { we've to generate one, if the units has ansistrings }
    { in the interface or implementation                  }
    procedure genimplicitunitfinal(alist : paasmoutput);
    procedure genimplicitunitinit(alist : paasmoutput);
{$ifdef test_dest_loc}

const
  { used to avoid temporary assignments }
  dest_loc_known : boolean = false;
  in_dest_loc    : boolean = false;
  dest_loc_tree  : ptree = nil;


var
  dest_loc : tlocation;

procedure mov_reg_to_dest(p : ptree; s : topsize; reg : tregister);

{$endif test_dest_loc}

  implementation

    uses
       strings,globtype,systems,globals,verbose,files,types,pbase,
       tgen,temp_gen,hcodegen,ppu
{$ifdef GDB}
       ,gdb
{$endif}
{$ifndef NOTARGETWIN32}
       ,t_win32
{$endif}
       ;


{*****************************************************************************
                                Helpers
*****************************************************************************}

    function def_opsize(p1:pdef):topsize;
      begin
        case p1^.size of
         1 : def_opsize:=S_B;
         2 : def_opsize:=S_W;
         4 : def_opsize:=S_L;
         { I don't know if we need it (FK) }
         8 : def_opsize:=S_L;
        else
         internalerror(78);
        end;
      end;


    function def2def_opsize(p1,p2:pdef):topsize;
      var
        o1 : topsize;
      begin
        case p1^.size of
         1 : o1:=S_B;
         2 : o1:=S_W;
         4 : o1:=S_L;
         { I don't know if we need it (FK) }
         8 : o1:=S_L;
        else
         internalerror(78);
        end;
        if assigned(p2) then
         begin
           case p2^.size of
            1 : o1:=S_B;
            2 : begin
                  if o1=S_B then
                   o1:=S_BW
                  else
                   o1:=S_W;
                end;
            4,8:
              begin
                 case o1 of
                    S_B : o1:=S_BL;
                    S_W : o1:=S_WL;
                 end;
              end;
           end;
         end;
        def2def_opsize:=o1;
      end;


    function def_getreg(p1:pdef):tregister;
      begin
        case p1^.size of
         1 : def_getreg:=reg32toreg8(getregister32);
         2 : def_getreg:=reg32toreg16(getregister32);
         4 : def_getreg:=getregister32;
        else
         internalerror(78);
        end;
      end;


    function makereg8(r:tregister):tregister;
      begin
        case r of
          R_EAX,R_EBX,R_ECX,R_EDX,R_EDI,R_ESI,R_ESP :
            makereg8:=reg32toreg8(r);
          R_AX,R_BX,R_CX,R_DX,R_DI,R_SI,R_SP :
            makereg8:=reg16toreg8(r);
          R_AL,R_BL,R_CL,R_DL :
            makereg8:=r;
        end;
      end;


    function makereg16(r:tregister):tregister;
      begin
        case r of
          R_EAX,R_EBX,R_ECX,R_EDX,R_EDI,R_ESI,R_ESP :
            makereg16:=reg32toreg16(r);
          R_AX,R_BX,R_CX,R_DX,R_DI,R_SI,R_SP :
            makereg16:=r;
          R_AL,R_BL,R_CL,R_DL :
            makereg16:=reg8toreg16(r);
        end;
      end;


    function makereg32(r:tregister):tregister;
      begin
        case r of
          R_EAX,R_EBX,R_ECX,R_EDX,R_EDI,R_ESI,R_ESP :
            makereg32:=r;
          R_AX,R_BX,R_CX,R_DX,R_DI,R_SI,R_SP :
            makereg32:=reg16toreg32(r);
          R_AL,R_BL,R_CL,R_DL :
            makereg32:=reg8toreg32(r);
        end;
      end;


    procedure locflags2reg(var l:tlocation;opsize:topsize);
      var
        hregister : tregister;
      begin
        if (l.loc=LOC_FLAGS) then
         begin
           hregister:=getregister32;
           case opsize of
            S_W : hregister:=reg32toreg16(hregister);
            S_B : hregister:=reg32toreg8(hregister);
           end;
           emit_flag2reg(l.resflags,hregister);
           l.loc:=LOC_REGISTER;
           l.register:=hregister;
         end
        else internalerror(270720001);
      end;


    procedure locjump2reg(var l:tlocation;opsize:topsize; otl, ofl: pasmlabel);
      var
        hregister : tregister;
        hl : pasmlabel;
      begin
         if l.loc = LOC_JUMP then
           begin
             hregister:=getregister32;
             case opsize of
               S_W : hregister:=reg32toreg16(hregister);
               S_B : hregister:=reg32toreg8(hregister);
             end;
             l.loc:=LOC_REGISTER;
             l.register:=hregister;
             emitlab(truelabel);
             truelabel:=otl;
             emit_const_reg(A_MOV,opsize,1,hregister);
             getlabel(hl);
             emitjmp(C_None,hl);
             emitlab(falselabel);
             falselabel:=ofl;
             emit_reg_reg(A_XOR,S_L,makereg32(hregister),
             makereg32(hregister));
             emitlab(hl);
           end
        else internalerror(270720002);
      end;


{*****************************************************************************
                              Emit Assembler
*****************************************************************************}

    procedure emitlab(var l : pasmlabel);
      begin
         if not l^.is_set then
          exprasmlist^.concat(new(pai_label,init(l)))
         else
          internalerror(7453984);
      end;

{$ifdef nojmpfix}
    procedure emitjmp(c : tasmcond;var l : pasmlabel);
      var
        ai : Paicpu;
      begin
        if c=C_None then
          exprasmlist^.concat(new(paicpu,op_sym(A_JMP,S_NO,l)))
        else
          begin
            ai:=new(paicpu,op_sym(A_Jcc,S_NO,l));
            ai^.SetCondition(c);
            ai^.is_jmp:=true;
            exprasmlist^.concat(ai);
          end;
      end;
{$else nojmpfix}
    procedure emitjmp(c : tasmcond;var l : pasmlabel);
      var
        ai : Paicpu;
      begin
        if c=C_None then
          ai := new(paicpu,op_sym(A_JMP,S_NO,l))
        else
          begin
            ai:=new(paicpu,op_sym(A_Jcc,S_NO,l));
            ai^.SetCondition(c);
          end;
        ai^.is_jmp:=true;
        exprasmlist^.concat(ai);
      end;
{$endif nojmpfix}

    procedure emit_flag2reg(flag:tresflags;hregister:tregister);
      var
        ai : paicpu;
        hreg : tregister;
      begin
         hreg:=makereg8(hregister);
         ai:=new(paicpu,op_reg(A_Setcc,S_B,hreg));
         ai^.SetCondition(flag_2_cond[flag]);
         exprasmlist^.concat(ai);
         if hreg<>hregister then
          begin
            if hregister in regset16bit then
             emit_to_reg16(hreg)
            else
             emit_to_reg32(hreg);
          end;
      end;


    procedure emit_none(i : tasmop;s : topsize);
      begin
         exprasmlist^.concat(new(paicpu,op_none(i,s)));
      end;

    procedure emit_reg(i : tasmop;s : topsize;reg : tregister);
      begin
         exprasmlist^.concat(new(paicpu,op_reg(i,s,reg)));
      end;

    procedure emit_ref(i : tasmop;s : topsize;ref : preference);
      begin
         exprasmlist^.concat(new(paicpu,op_ref(i,s,ref)));
      end;

    procedure emit_const(i : tasmop;s : topsize;c : longint);
      begin
         exprasmlist^.concat(new(paicpu,op_const(i,s,c)));
      end;

    procedure emit_const_reg(i : tasmop;s : topsize;c : longint;reg : tregister);
      begin
         exprasmlist^.concat(new(paicpu,op_const_reg(i,s,c,reg)));
      end;

    procedure emit_const_ref(i : tasmop;s : topsize;c : longint;ref : preference);
      begin
         exprasmlist^.concat(new(paicpu,op_const_ref(i,s,c,ref)));
      end;

    procedure emit_ref_reg(i : tasmop;s : topsize;ref : preference;reg : tregister);
      begin
         exprasmlist^.concat(new(paicpu,op_ref_reg(i,s,ref,reg)));
      end;

    procedure emit_mov_ref_reg(s : topsize;ref : preference;reg : tregister);
      begin
         exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOV,s,ref,reg)));
      end;

    procedure emit_reg_ref(i : tasmop;s : topsize;reg : tregister;ref : preference);
      begin
         exprasmlist^.concat(new(paicpu,op_reg_ref(i,s,reg,ref)));
      end;

    procedure emit_reg_reg(i : tasmop;s : topsize;reg1,reg2 : tregister);
      begin
         if (reg1<>reg2) or (i<>A_MOV) then
           exprasmlist^.concat(new(paicpu,op_reg_reg(i,s,reg1,reg2)));
      end;

    procedure emit_mov_reg_reg(s : topsize;reg1,reg2 : tregister);
      begin
         if (reg1<>reg2) then
           exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOV,s,reg1,reg2)));
      end;

    procedure emit_const_reg_reg(i : tasmop;s : topsize;c : longint;reg1,reg2 : tregister);
      begin
         exprasmlist^.concat(new(paicpu,op_const_reg_reg(i,s,c,reg1,reg2)));
      end;

    procedure emit_reg_reg_reg(i : tasmop;s : topsize;reg1,reg2,reg3 : tregister);
      begin
         exprasmlist^.concat(new(paicpu,op_reg_reg_reg(i,s,reg1,reg2,reg3)));
      end;

    procedure emit_sym(i : tasmop;s : topsize;op : pasmsymbol);
      begin
        exprasmlist^.concat(new(paicpu,op_sym(i,s,op)));
      end;

    procedure emit_sym_ofs(i : tasmop;s : topsize;op : pasmsymbol;ofs : longint);
      begin
        exprasmlist^.concat(new(paicpu,op_sym_ofs(i,s,op,ofs)));
      end;

    procedure emit_sym_ofs_reg(i : tasmop;s : topsize;op : pasmsymbol;ofs:longint;reg : tregister);
      begin
        exprasmlist^.concat(new(paicpu,op_sym_ofs_reg(i,s,op,ofs,reg)));
      end;

    procedure emit_sym_ofs_ref(i : tasmop;s : topsize;op : pasmsymbol;ofs:longint;ref : preference);
      begin
        exprasmlist^.concat(new(paicpu,op_sym_ofs_ref(i,s,op,ofs,ref)));
      end;

    procedure emitcall(const routine:string);
      begin
        exprasmlist^.concat(new(paicpu,op_sym(A_CALL,S_NO,newasmsymbol(routine))));
      end;

    { only usefull in startup code }
    procedure emitinsertcall(const routine:string);
      begin
        exprasmlist^.insert(new(paicpu,op_sym(A_CALL,S_NO,newasmsymbol(routine))));
      end;


    procedure emit_mov_loc_ref(const t:tlocation;const ref:treference;siz:topsize;freetemp:boolean);
      var
        hreg : tregister;
        pushedeax : boolean;

      begin
        pushedeax:=false;
        case t.loc of
          LOC_REGISTER,
         LOC_CREGISTER : begin
                           exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,siz,
                             t.register,newreference(ref))));
                           ungetregister32(t.register); { the register is not needed anymore }
                         end;
               LOC_MEM,
         LOC_REFERENCE : begin
                           if t.reference.is_immediate then
                             emit_const_ref(A_MOV,siz,
                               t.reference.offset,newreference(ref))
                           else
                             begin
                               case siz of
                                 S_B : begin
                                          { we can't do a getregister in the code generator }
                                          { without problems!!!                             }
                                          if usablereg32>0 then
                                            hreg:=reg32toreg8(getregister32)
                                          else
                                            begin
                                               emit_reg(A_PUSH,S_L,R_EAX);
                                               pushedeax:=true;
                                               hreg:=R_AL;
                                            end;
                                       end;
                                 S_W : hreg:=R_DI;
                                 S_L : hreg:=R_EDI;
                               end;
{$ifndef noAllocEdi}
                               if hreg in [R_DI,R_EDI] then
                                 getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                               emit_ref_reg(A_MOV,siz,
                                 newreference(t.reference),hreg);
                               del_reference(t.reference);
                               exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,siz,
                                 hreg,newreference(ref))));
                               if siz=S_B then
                                 begin
                                    if pushedeax then
                                      emit_reg(A_POP,S_L,R_EAX)
                                    else
                                      ungetregister(hreg);
                                 end;
{$ifndef noAllocEdi}
                               if hreg in [R_DI,R_EDI] then
                                 ungetregister32(R_EDI);
{$endif noAllocEdi}
                               { we can release the registers }
                               { but only AFTER the MOV! Important for the optimizer!
                                 (JM)}
                               del_reference(ref);
                             end;
                           if freetemp then
                            ungetiftemp(t.reference);
                         end;
        else
         internalerror(330);
        end;
      end;


    procedure emit_mov_loc_reg(const t:tlocation;reg:tregister);
      begin
        case t.loc of
          LOC_REGISTER,
         LOC_CREGISTER : begin
                           emit_reg_reg(A_MOV,S_L,t.register,reg);
                           ungetregister32(t.register); { the register is not needed anymore }
                         end;
               LOC_MEM,
         LOC_REFERENCE : begin
                           if t.reference.is_immediate then
                             emit_const_reg(A_MOV,S_L,
                               t.reference.offset,reg)
                           else
                             begin
                               emit_ref_reg(A_MOV,S_L,
                                 newreference(t.reference),reg);
                             end;
                         end;
        else
         internalerror(330);
        end;
      end;

    procedure emit_mov_reg_loc(reg: TRegister; const t:tlocation);
      begin
        case t.loc of
          LOC_REGISTER,
         LOC_CREGISTER : begin
                           emit_reg_reg(A_MOV,RegSize(Reg),
                             reg,t.register);
                         end;
               LOC_MEM,
         LOC_REFERENCE : begin
                           if t.reference.is_immediate then
                             internalerror(334)
                           else
                             begin
                               exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,RegSize(Reg),
                                 Reg,newreference(t.reference))));
                             end;
                         end;
        else
         internalerror(330);
        end;
      end;


    procedure emit_lea_loc_reg(const t:tlocation;reg:tregister;freetemp:boolean);
      begin
        case t.loc of
               LOC_MEM,
         LOC_REFERENCE : begin
                           if t.reference.is_immediate then
                             internalerror(331)
                           else
                             begin
                               emit_ref_reg(A_LEA,S_L,
                                 newreference(t.reference),reg);
                             end;
                           if freetemp then
                            ungetiftemp(t.reference);
                         end;
        else
         internalerror(332);
        end;
      end;


    procedure emit_movq_reg_loc(reghigh,reglow: TRegister;t:tlocation);
      begin
        case t.loc of
          LOC_REGISTER,
         LOC_CREGISTER : begin
                           emit_reg_reg(A_MOV,S_L,
                             reglow,t.registerlow);
                           emit_reg_reg(A_MOV,S_L,
                             reghigh,t.registerhigh);
                         end;
               LOC_MEM,
         LOC_REFERENCE : begin
                           if t.reference.is_immediate then
                             internalerror(334)
                           else
                             begin
                               exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,
                                 Reglow,newreference(t.reference))));
                               inc(t.reference.offset,4);
                               exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,
                                 Reghigh,newreference(t.reference))));
                             end;
                         end;
        else
         internalerror(330);
        end;
      end;


   procedure emit_pushq_loc(const t : tlocation);

      var
         hr : preference;

      begin
         case t.loc of
            LOC_REGISTER,
            LOC_CREGISTER:
              begin
                 exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,
                   t.registerhigh)));
                 exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,
                   t.registerlow)));
              end;
            LOC_MEM,
            LOC_REFERENCE:
              begin
                 hr:=newreference(t.reference);
                 inc(hr^.offset,4);
                 exprasmlist^.concat(new(paicpu,op_ref(A_PUSH,S_L,
                   hr)));
                 exprasmlist^.concat(new(paicpu,op_ref(A_PUSH,S_L,
                   newreference(t.reference))));
                 ungetiftemp(t.reference);
              end;
            else internalerror(331);
         end;
      end;

    procedure remove_non_regvars_from_loc(const t: tlocation; var regs: byte);
    begin
      case t.loc of
        LOC_REGISTER:
          begin
            { can't be a regvar, since it would be LOC_CREGISTER then }
            regs := regs and not($80 shr byte(t.register));
            if t.registerhigh <> R_NO then
              regs := regs and not($80 shr byte(t.registerhigh));
          end;
        LOC_MEM,LOC_REFERENCE:
          begin
            if not(cs_regalloc in aktglobalswitches) or
               (t.reference.base in usableregs) then
              regs := regs and
                not($80 shr byte(t.reference.base));
            if not(cs_regalloc in aktglobalswitches) or
               (t.reference.index in usableregs) then
              regs := regs and
                not($80 shr byte(t.reference.index));
          end;
      end;
    end;


    procedure release_loc(const t : tlocation);

      begin
         case t.loc of
            LOC_REGISTER,
            LOC_CREGISTER:
              begin
                 ungetregister32(t.register);
              end;
            LOC_MEM,
            LOC_REFERENCE:
              del_reference(t.reference);
            else internalerror(332);
         end;
      end;

    procedure release_qword_loc(const t : tlocation);
      begin
         case t.loc of
            LOC_REGISTER,
            LOC_CREGISTER:
              begin
                 ungetregister32(t.registerhigh);
                 ungetregister32(t.registerlow);
              end;
            LOC_MEM,
            LOC_REFERENCE:
              del_reference(t.reference);
            else internalerror(331);
         end;
      end;


    procedure emit_push_loc(const t:tlocation);
      begin
        case t.loc of
          LOC_REGISTER,
         LOC_CREGISTER : begin
                           exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,makereg32(t.register))));
                           ungetregister(t.register); { the register is not needed anymore }
                         end;
               LOC_MEM,
         LOC_REFERENCE : begin
                           if t.reference.is_immediate then
                             exprasmlist^.concat(new(paicpu,op_const(A_PUSH,S_L,t.reference.offset)))
                           else
                             exprasmlist^.concat(new(paicpu,op_ref(A_PUSH,S_L,newreference(t.reference))));
                           del_reference(t.reference);
                           ungetiftemp(t.reference);
                         end;
        else
         internalerror(330);
        end;
      end;


    procedure emit_pushw_loc(const t:tlocation);
      var
        opsize : topsize;
      begin
        case t.loc of
          LOC_REGISTER,
         LOC_CREGISTER : begin
                           if target_os.stackalignment=4 then
                             exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,makereg32(t.register))))
                           else
                             exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_W,makereg16(t.register))));
                           ungetregister(t.register); { the register is not needed anymore }
                         end;
               LOC_MEM,
         LOC_REFERENCE : begin
                           if target_os.stackalignment=4 then
                            opsize:=S_L
                           else
                            opsize:=S_W;
                           if t.reference.is_immediate then
                             exprasmlist^.concat(new(paicpu,op_const(A_PUSH,opsize,t.reference.offset)))
                           else
                             exprasmlist^.concat(new(paicpu,op_ref(A_PUSH,opsize,newreference(t.reference))));
                           del_reference(t.reference);
                           ungetiftemp(t.reference);
                         end;
        else
         internalerror(330);
        end;
      end;


    procedure emit_lea_loc_ref(const t:tlocation;const ref:treference;freetemp:boolean);
      begin
        case t.loc of
               LOC_MEM,
         LOC_REFERENCE : begin
                           if t.reference.is_immediate then
                             internalerror(331)
                           else
                             begin
{$ifndef noAllocEdi}
                               getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                               emit_ref_reg(A_LEA,S_L,
                                 newreference(t.reference),R_EDI);
                               exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,
                                 R_EDI,newreference(ref))));
{$ifndef noAllocEdi}
                               ungetregister32(R_EDI);
{$endif noAllocEdi}
                             end;
                            { release the registers }
                            del_reference(t.reference);
                           if freetemp then
                            ungetiftemp(t.reference);
                         end;
        else
         internalerror(332);
        end;
      end;


    procedure emit_push_lea_loc(const t:tlocation;freetemp:boolean);
      begin
        case t.loc of
               LOC_MEM,
         LOC_REFERENCE : begin
                           if t.reference.is_immediate then
                             internalerror(331)
                           else
                             begin
{$ifndef noAllocEdi}
                               getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                               emit_ref_reg(A_LEA,S_L,
                                 newreference(t.reference),R_EDI);
                               exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,R_EDI)));
{$ifndef noAllocEdi}
                               ungetregister32(R_EDI);
{$endif noAllocEdi}
                             end;
                           if freetemp then
                            ungetiftemp(t.reference);
                         end;
        else
         internalerror(332);
        end;
      end;

    procedure emit_push_mem_size(const t: treference; size: longint);

      var
        s: topsize;

      begin
        if t.is_immediate then
          begin
            if (size=4) or
               (target_os.stackalignment=4) then
              exprasmlist^.concat(new(paicpu,op_const(A_PUSH,S_L,t.offset)))
            else
              exprasmlist^.concat(new(paicpu,op_const(A_PUSH,S_W,t.offset)));
          end
        else
          if size < 4 then
            begin
              getexplicitregister32(R_EDI);
              case size of
                1: s := S_BL;
                2: s := S_WL;
                else internalerror(200008071);
              end;
              exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVZX,s,
                newreference(t),R_EDI)));
              if target_os.stackalignment=4 then
                exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,R_EDI)))
              else
                exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_W,R_DI)));
              ungetregister32(R_EDI);
            end
          else
            if size = 4 then
              emit_push_mem(t)
            else
              internalerror(200008072);
      end;


    procedure emit_to_mem(var p:ptree);

      var
         r : treference;

      begin
        case p^.location.loc of
               LOC_FPU : begin
                           reset_reference(p^.location.reference);
                           gettempofsizereference(10,p^.location.reference);
                           floatstore(pfloatdef(p^.resulttype)^.typ,p^.location.reference);
                         end;
               LOC_REGISTER:
                 begin
                    if is_64bitint(p^.resulttype) then
                      begin
                         gettempofsizereference(8,r);
                         emit_reg_ref(A_MOV,S_L,p^.location.registerlow,
                           newreference(r));
                         inc(r.offset,4);
                         emit_reg_ref(A_MOV,S_L,p^.location.registerhigh,
                           newreference(r));
                         dec(r.offset,4);
                      end
                    else
                      begin
                         gettempofsizereference(p^.resulttype^.size,r);
                         emit_reg_ref(A_MOV,def_opsize(p^.resulttype),p^.location.register,
                           newreference(r));
                      end;
                    p^.location.reference:=r;
                 end;
               LOC_MEM,
         LOC_REFERENCE : ;
         LOC_CFPUREGISTER : begin
                           emit_reg(A_FLD,S_NO,correct_fpuregister(p^.location.register,fpuvaroffset));
                           inc(fpuvaroffset);
                           reset_reference(p^.location.reference);
                           gettempofsizereference(10,p^.location.reference);
                           floatstore(pfloatdef(p^.resulttype)^.typ,p^.location.reference);
                         end;
         else
           internalerror(333);
        end;
        p^.location.loc:=LOC_MEM;
      end;


    procedure emit_to_reg16(var hr:tregister);
      begin
        { ranges are a little bit bug sensitive ! }
        case hr of
           R_EAX,R_EBX,R_ECX,R_EDX,R_EDI,R_ESI,R_ESP,R_EBP:
             begin
               hr:=reg32toreg16(hr);
             end;
           R_AL,R_BL,R_CL,R_DL:
             begin
               hr:=reg8toreg16(hr);
               emit_const_reg(A_AND,S_W,$ff,hr);
             end;
           R_AH,R_BH,R_CH,R_DH:
             begin
               hr:=reg8toreg16(hr);
               emit_const_reg(A_AND,S_W,$ff00,hr);
             end;
        end;
      end;


    procedure emit_to_reg32(var hr:tregister);
      begin
        { ranges are a little bit bug sensitive ! }
        case hr of
           R_AX,R_BX,R_CX,R_DX,R_DI,R_SI,R_SP,R_BP:
             begin
                hr:=reg16toreg32(hr);
                emit_const_reg(A_AND,S_L,$ffff,hr);
             end;
           R_AL,R_BL,R_CL,R_DL:
             begin
                hr:=reg8toreg32(hr);
                emit_const_reg(A_AND,S_L,$ff,hr);
             end;
           R_AH,R_BH,R_CH,R_DH:
             begin
                hr:=reg8toreg32(hr);
                emit_const_reg(A_AND,S_L,$ff00,hr);
             end;
        end;
      end;

    procedure emit_mov_ref_reg64(r : treference;rl,rh : tregister);

      var
         hr : preference;

      begin
         { if we load a 64 bit reference, we must be careful because }
         { we could overwrite the registers of the reference by      }
         { accident                                                  }
         getexplicitregister32(R_EDI);
         if r.base=rl then
           begin
              emit_reg_reg(A_MOV,S_L,r.base,
                R_EDI);
              r.base:=R_EDI;
           end
         else if r.index=rl then
           begin
              emit_reg_reg(A_MOV,S_L,r.index,
                R_EDI);
              r.index:=R_EDI;
           end;
         emit_ref_reg(A_MOV,S_L,
           newreference(r),rl);
         hr:=newreference(r);
         inc(hr^.offset,4);
         emit_ref_reg(A_MOV,S_L,
           hr,rh);
         ungetregister32(R_EDI);
      end;

{*****************************************************************************
                           Emit String Functions
*****************************************************************************}

    procedure copyshortstring(const dref,sref : treference;len : byte;
                loadref, del_sref: boolean);
      begin
         emitpushreferenceaddr(dref);
          { if it's deleted right before it's used, the optimizer can move }
          { the reg deallocations to the right places (JM)                 }
         if del_sref then
           del_reference(sref);
         if loadref then
          emit_push_mem(sref)
         else
          emitpushreferenceaddr(sref);
         push_int(len);
         emitcall('FPC_SHORTSTR_COPY');
         maybe_loadself;
      end;

    procedure copylongstring(const dref,sref : treference;len : longint;loadref:boolean);
      begin
         emitpushreferenceaddr(dref);
         if loadref then
          emit_push_mem(sref)
         else
          emitpushreferenceaddr(sref);
         push_int(len);
         emitcall('FPC_LONGSTR_COPY');
         maybe_loadself;
      end;


    procedure incrstringref(t : pdef;const ref : treference);

      var
         pushedregs : tpushed;

      begin
         pushusedregisters(pushedregs,$ff);
         emitpushreferenceaddr(ref);
         if is_ansistring(t) then
           begin
              emitcall('FPC_ANSISTR_INCR_REF');
           end
         else if is_widestring(t) then
           begin
              emitcall('FPC_WIDESTR_INCR_REF');
           end
         else internalerror(1859);
         popusedregisters(pushedregs);
      end;


    procedure decrstringref(t : pdef;const ref : treference);

      var
         pushedregs : tpushed;

      begin
         pushusedregisters(pushedregs,$ff);
         emitpushreferenceaddr(ref);
         if is_ansistring(t) then
           begin
              emitcall('FPC_ANSISTR_DECR_REF');
           end
         else if is_widestring(t) then
           begin
              emitcall('FPC_WIDESTR_DECR_REF');
           end
         else internalerror(1859);
         popusedregisters(pushedregs);
      end;

    procedure loadansistring(p : ptree);
    {
      copies an ansistring from p^.right to p^.left, we
      assume, that both sides are ansistring, firstassignement have
      to take care of that, an ansistring can't be a register variable
    }
      var
         pushed : tpushed;
         regs_to_push: byte;
         ungettemp : boolean;
      begin
         { before pushing any parameter, we have to save all used      }
         { registers, but before that we have to release the       }
         { registers of that node to save uneccessary pushed       }
         { so be careful, if you think you can optimize that code (FK) }

         { nevertheless, this has to be changed, because otherwise the }
         { register is released before it's contents are pushed ->     }
         { problems with the optimizer (JM)                            }
         ungettemp:=false;
         { Find out which registers have to be pushed (JM) }
         regs_to_push := $ff;
         remove_non_regvars_from_loc(p^.right^.location,regs_to_push);
         remove_non_regvars_from_loc(p^.left^.location,regs_to_push);
         { And push them (JM) }
         pushusedregisters(pushed,regs_to_push);
         case p^.right^.location.loc of
            LOC_REGISTER,LOC_CREGISTER:
              begin
                 exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,p^.right^.location.register)));
                 ungetregister32(p^.right^.location.register);
              end;
            LOC_REFERENCE,LOC_MEM:
              begin
                 { First release the registers because emit_push_mem may  }
                 { load the reference in edi before pushing and then the  }
                 { dealloc is too late (and optimizations are missed (JM) }
                 del_reference(p^.right^.location.reference);
                 { This one doesn't need extra registers (JM) }
                 emit_push_mem(p^.right^.location.reference);
                 ungettemp:=true;
              end;
         end;
         emitpushreferenceaddr(p^.left^.location.reference);
         del_reference(p^.left^.location.reference);
         emitcall('FPC_ANSISTR_ASSIGN');
         maybe_loadself;
         popusedregisters(pushed);
         if ungettemp then
           ungetiftemp(p^.right^.location.reference);
      end;


{*****************************************************************************
                           Emit Push Functions
*****************************************************************************}

    function maybe_push(needed : byte;p : ptree;isint64 : boolean) : boolean;
      var
         pushed : boolean;
         {hregister : tregister; }
{$ifdef TEMPS_NOT_PUSH}
         href : treference;
{$endif TEMPS_NOT_PUSH}
      begin
         if needed>usablereg32 then
           begin
              if (p^.location.loc=LOC_REGISTER) then
                begin
                   if isint64 then
                     begin
{$ifdef TEMPS_NOT_PUSH}
                        gettempofsizereference(href,8);
                        p^.temp_offset:=href.offset;
                        href.offset:=href.offset+4;
                        exprasmlist^.concat(new(paicpu,op_reg(A_MOV,S_L,p^.location.registerhigh,href)));
                        href.offset:=href.offset-4;
{$else TEMPS_NOT_PUSH}
                        exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,p^.location.registerhigh)));
{$endif TEMPS_NOT_PUSH}
                        ungetregister32(p^.location.registerhigh);
                     end
{$ifdef TEMPS_NOT_PUSH}
                   else
                     begin
                        gettempofsizereference(href,4);
                        p^.temp_offset:=href.offset;
                     end
{$endif TEMPS_NOT_PUSH}
                     ;
                   pushed:=true;
{$ifdef TEMPS_NOT_PUSH}
                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,p^.location.register,href)));
{$else TEMPS_NOT_PUSH}
                   exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,p^.location.register)));
{$endif TEMPS_NOT_PUSH}
                   ungetregister32(p^.location.register);
                end
              else if (p^.location.loc in [LOC_MEM,LOC_REFERENCE]) and
                      ((p^.location.reference.base<>R_NO) or
                       (p^.location.reference.index<>R_NO)
                      ) then
                  begin
                     del_reference(p^.location.reference);
{$ifndef noAllocEdi}
                    getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                     emit_ref_reg(A_LEA,S_L,newreference(p^.location.reference),
                       R_EDI);
{$ifdef TEMPS_NOT_PUSH}
                     gettempofsizereference(href,4);
                     exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,href)));
                     p^.temp_offset:=href.offset;
{$else TEMPS_NOT_PUSH}
                     exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,R_EDI)));
{$endif TEMPS_NOT_PUSH}
{$ifndef noAllocEdi}
                     ungetregister32(R_EDI);
{$endif noAllocEdi}
                     pushed:=true;
                  end
              else pushed:=false;
           end
         else pushed:=false;
         maybe_push:=pushed;
      end;

{$ifdef TEMPS_NOT_PUSH}
    function maybe_savetotemp(needed : byte;p : ptree;isint64 : boolean) : boolean;

      var
         pushed : boolean;
         href : treference;

      begin
         if needed>usablereg32 then
           begin
              if (p^.location.loc=LOC_REGISTER) then
                begin
                   if isint64(p^.resulttype) then
                     begin
                        gettempofsizereference(href,8);
                        p^.temp_offset:=href.offset;
                        href.offset:=href.offset+4;
                        exprasmlist^.concat(new(paicpu,op_reg(A_MOV,S_L,p^.location.registerhigh,href)));
                        href.offset:=href.offset-4;
                        ungetregister32(p^.location.registerhigh);
                     end
                   else
                     begin
                        gettempofsizereference(href,4);
                        p^.temp_offset:=href.offset;
                     end;
                   pushed:=true;
                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,p^.location.register,href)));
                   ungetregister32(p^.location.register);
                end
              else if (p^.location.loc in [LOC_MEM,LOC_REFERENCE]) and
                      ((p^.location.reference.base<>R_NO) or
                       (p^.location.reference.index<>R_NO)
                      ) then
                  begin
                     del_reference(p^.location.reference);
{$ifndef noAllocEdi}
                     getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                     emit_ref_reg(A_LEA,S_L,newreference(p^.location.reference),
                       R_EDI);
                     gettempofsizereference(href,4);
                     exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,href)));
{$ifndef noAllocEdi}
                     ungetregister32(R_EDI);
{$endif noAllocEdi}
                     p^.temp_offset:=href.offset;
                     pushed:=true;
                  end
              else pushed:=false;
           end
         else pushed:=false;
         maybe_push:=pushed;
      end;
{$endif TEMPS_NOT_PUSH}

    function maybe_pushfpu(needed : byte;p : ptree) : boolean;
      begin
        if needed>=maxfpuregs then
          begin
            if p^.left^.location.loc = LOC_FPU then
              begin
                emit_to_mem(p^.left);
                maybe_pushfpu:=true;
              end
            else
              maybe_pushfpu:=false;
          end
        else
          maybe_pushfpu:=false;

      end;
    procedure push_int(l : longint);
      begin
         if (l = 0) and
            not(aktoptprocessor in [Class386, ClassP6]) and
            not(cs_littlesize in aktglobalswitches)
           Then
             begin
{$ifndef noAllocEdi}
               getexplicitregister32(R_EDI);
{$endif noAllocEdi}
               emit_reg_reg(A_XOR,S_L,R_EDI,R_EDI);
               exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,R_EDI)));
{$ifndef noAllocEdi}
               ungetregister32(R_EDI);
{$endif noAllocEdi}
             end
           else
             exprasmlist^.concat(new(paicpu,op_const(A_PUSH,S_L,l)));
      end;

    procedure emit_push_mem(const ref : treference);

      begin
         if ref.is_immediate then
           push_int(ref.offset)
         else
           begin
             if not(aktoptprocessor in [Class386, ClassP6]) and
                not(cs_littlesize in aktglobalswitches)
               then
                 begin
{$ifndef noAllocEdi}
                   getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                   emit_ref_reg(A_MOV,S_L,newreference(ref),R_EDI);
                   exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,R_EDI)));
{$ifndef noAllocEdi}
                   ungetregister32(R_EDI);
{$endif noAllocEdi}
                 end
               else exprasmlist^.concat(new(paicpu,op_ref(A_PUSH,S_L,newreference(ref))));
           end;
      end;


    procedure emitpushreferenceaddr(const ref : treference);
      var
        href : treference;
      begin
         { this will fail for references to other segments !!! }
         if ref.is_immediate then
         { is this right ? }
           begin
              { push_int(ref.offset)}
              gettempofsizereference(4,href);
              emit_const_ref(A_MOV,S_L,ref.offset,newreference(href));
              emitpushreferenceaddr(href);
              del_reference(href);
           end
         else
           begin
              if ref.segment<>R_NO then
                CGMessage(cg_e_cant_use_far_pointer_there);
              if (ref.base=R_NO) and (ref.index=R_NO) then
                exprasmlist^.concat(new(paicpu,op_sym_ofs(A_PUSH,S_L,ref.symbol,ref.offset)))
              else if (ref.base=R_NO) and (ref.index<>R_NO) and
                 (ref.offset=0) and (ref.scalefactor=0) and (ref.symbol=nil) then
                exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,ref.index)))
              else if (ref.base<>R_NO) and (ref.index=R_NO) and
                 (ref.offset=0) and (ref.symbol=nil) then
                exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,ref.base)))
              else
                begin
{$ifndef noAllocEdi}
                   getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                   emit_ref_reg(A_LEA,S_L,newreference(ref),R_EDI);
                   exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,R_EDI)));
{$ifndef noAllocEdi}
                   ungetregister32(R_EDI);
{$endif noAllocEdi}
                end;
           end;
        end;


     procedure pushsetelement(p : ptree);
     {
       copies p a set element on the stack
     }
      var
         hr,hr16,hr32 : tregister;
      begin
      { copy the element on the stack, slightly complicated }
        if p^.treetype=ordconstn then
         begin
           if target_os.stackalignment=4 then
             exprasmlist^.concat(new(paicpu,op_const(A_PUSH,S_L,p^.value)))
           else
             exprasmlist^.concat(new(paicpu,op_const(A_PUSH,S_W,p^.value)));
         end
        else
         begin
           case p^.location.loc of
             LOC_REGISTER,
             LOC_CREGISTER :
               begin
                 hr:=p^.location.register;
                 case hr of
                   R_EAX,R_EBX,R_ECX,R_EDX,R_EDI,R_ESI,R_ESP :
                     begin
                       hr16:=reg32toreg16(hr);
                       hr32:=hr;
                     end;
                   R_AX,R_BX,R_CX,R_DX,R_DI,R_SI,R_SP :
                     begin
                       hr16:=hr;
                       hr32:=reg16toreg32(hr);
                     end;
                   R_AL,R_BL,R_CL,R_DL :
                     begin
                       hr16:=reg8toreg16(hr);
                       hr32:=reg8toreg32(hr);
                     end;
                 end;
                 if target_os.stackalignment=4 then
                   exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,hr32)))
                 else
                   exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_W,hr16)));
                 ungetregister32(hr32);
               end;
           else
             begin
               { you can't push more bytes than the size of the element, }
               { because this may cross a page boundary and you'll get a }
               { sigsegv (JM)                                            }
               emit_push_mem_size(p^.location.reference,1);
               del_reference(p^.location.reference);
             end;
           end;
         end;
      end;


    procedure restore(p : ptree;isint64 : boolean);
      var
         hregister :  tregister;
{$ifdef TEMPS_NOT_PUSH}
         href : treference;
{$endif TEMPS_NOT_PUSH}
      begin
         hregister:=getregister32;
{$ifdef TEMPS_NOT_PUSH}
         reset_reference(href);
         href.base:=procinfo^.frame_pointer;
         href.offset:=p^.temp_offset;
         emit_ref_reg(A_MOV,S_L,href,hregister);
{$else  TEMPS_NOT_PUSH}
         exprasmlist^.concat(new(paicpu,op_reg(A_POP,S_L,hregister)));
{$endif TEMPS_NOT_PUSH}
         if (p^.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
           begin
              p^.location.register:=hregister;
              if isint64 then
                begin
                   p^.location.registerhigh:=getregister32;
{$ifdef TEMPS_NOT_PUSH}
                   href.offset:=p^.temp_offset+4;
                   emit_ref_reg(A_MOV,S_L,p^.location.registerhigh);
                   { set correctly for release ! }
                   href.offset:=p^.temp_offset;
{$else  TEMPS_NOT_PUSH}
                   exprasmlist^.concat(new(paicpu,op_reg(A_POP,S_L,p^.location.registerhigh)));
{$endif TEMPS_NOT_PUSH}
                end;
           end
         else
           begin
              reset_reference(p^.location.reference);
              { any reasons why this was moved into the index register ? }
              { normally usage of base register is much better (FK)      }
              p^.location.reference.base:=hregister;
              { Why is this done? We can never be sure about p^.left
                because otherwise secondload fails !!!
              set_location(p^.left^.location,p^.location);}
           end;
{$ifdef TEMPS_NOT_PUSH}
         ungetiftemp(href);
{$endif TEMPS_NOT_PUSH}
      end;

{$ifdef TEMPS_NOT_PUSH}
    procedure restorefromtemp(p : ptree;isint64 : boolean);
      var
         hregister :  tregister;
         href : treference;

      begin
         hregister:=getregister32;
         reset_reference(href);
         href.base:=procinfo^.frame_pointer;
         href.offset:=p^.temp_offset;
         emit_ref_reg(A_MOV,S_L,href,hregister);
         if (p^.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
           begin
              p^.location.register:=hregister;
              if isint64 then
                begin
                   p^.location.registerhigh:=getregister32;
                   href.offset:=p^.temp_offset+4;
                   emit_ref_reg(A_MOV,S_L,p^.location.registerhigh);
                   { set correctly for release ! }
                   href.offset:=p^.temp_offset;
                end;
           end
         else
           begin
              reset_reference(p^.location.reference);
              p^.location.reference.base:=hregister;
              { Why is this done? We can never be sure about p^.left
                because otherwise secondload fails PM
              set_location(p^.left^.location,p^.location);}
           end;
         ungetiftemp(href);
      end;
{$endif TEMPS_NOT_PUSH}

      procedure push_value_para(p:ptree;inlined,is_cdecl:boolean;
                                para_offset:longint;alignment : longint);
        var
          tempreference : treference;
          r : preference;
          opsize : topsize;
          op : tasmop;
          hreg : tregister;
          size, helpsize : longint;
          hlabel : pasmlabel;
        begin
          case p^.location.loc of
             LOC_REGISTER,
             LOC_CREGISTER:
               begin
                  if p^.resulttype^.size=8 then
                    begin
                       inc(pushedparasize,8);
                       if inlined then
                         begin
                            r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                            exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,
                              p^.location.registerlow,r)));
                            r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize+4);
                            exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,
                              p^.location.registerhigh,r)));
                         end
                       else
                         begin
                           exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,p^.location.registerhigh)));
                           exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,p^.location.registerlow)));
                         end;
                       ungetregister32(p^.location.registerhigh);
                       ungetregister32(p^.location.registerlow);
                    end
                  else case p^.location.register of
                     R_EAX,R_EBX,R_ECX,R_EDX,R_ESI,
                     R_EDI,R_ESP,R_EBP :
                        begin
                            begin
                               inc(pushedparasize,4);
                               if inlined then
                                 begin
                                    r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                    exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,
                                      p^.location.register,r)));
                                 end
                               else
                                 exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,p^.location.register)));
                               ungetregister32(p^.location.register);
                            end;
                        end;
                     R_AX,R_BX,R_CX,R_DX,R_SI,R_DI:
                        begin
                          if alignment=4 then
                            begin
                              opsize:=S_L;
                              hreg:=reg16toreg32(p^.location.register);
                              inc(pushedparasize,4);
                            end
                          else
                            begin
                              opsize:=S_W;
                              hreg:=p^.location.register;
                              inc(pushedparasize,2);
                            end;
                          if inlined then
                            begin
                              r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                              exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,opsize,hreg,r)));
                            end
                          else
                            exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,opsize,hreg)));
                          ungetregister32(reg16toreg32(p^.location.register));
                        end;
                     R_AL,R_BL,R_CL,R_DL:
                        begin
                          if alignment=4 then
                            begin
                              opsize:=S_L;
                              hreg:=reg8toreg32(p^.location.register);
                              inc(pushedparasize,4);
                            end
                          else
                            begin
                              opsize:=S_W;
                              hreg:=reg8toreg16(p^.location.register);
                              inc(pushedparasize,2);
                            end;
                          { we must push always 16 bit }
                          if inlined then
                            begin
                              r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                              exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,opsize,hreg,r)));
                            end
                          else
                            exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,opsize,hreg)));
                          ungetregister32(reg8toreg32(p^.location.register));
                        end;
                     else internalerror(1899);
                  end;
               end;
             LOC_FPU:
               begin
                  size:=align(pfloatdef(p^.resulttype)^.size,alignment);
                  inc(pushedparasize,size);
                  if not inlined then
                   emit_const_reg(A_SUB,S_L,size,R_ESP);
{$ifdef GDB}
                  if (cs_debuginfo in aktmoduleswitches) and
                     (exprasmlist^.first=exprasmlist^.last) then
                    exprasmlist^.concat(new(pai_force_line,init));
{$endif GDB}
                  r:=new_reference(R_ESP,0);
                  floatstoreops(pfloatdef(p^.resulttype)^.typ,op,opsize);
                  { this is the easiest case for inlined !! }
                  if inlined then
                    begin
                       r^.base:=procinfo^.framepointer;
                       r^.offset:=para_offset-pushedparasize;
                    end;
                  exprasmlist^.concat(new(paicpu,op_ref(op,opsize,r)));
                  dec(fpuvaroffset);
               end;
             LOC_CFPUREGISTER:
               begin
                  exprasmlist^.concat(new(paicpu,op_reg(A_FLD,S_NO,
                    correct_fpuregister(p^.location.register,fpuvaroffset))));
                  size:=align(pfloatdef(p^.resulttype)^.size,alignment);
                  inc(pushedparasize,size);
                  if not inlined then
                   emit_const_reg(A_SUB,S_L,size,R_ESP);
{$ifdef GDB}
                  if (cs_debuginfo in aktmoduleswitches) and
                     (exprasmlist^.first=exprasmlist^.last) then
                    exprasmlist^.concat(new(pai_force_line,init));
{$endif GDB}
                  r:=new_reference(R_ESP,0);
                  floatstoreops(pfloatdef(p^.resulttype)^.typ,op,opsize);
                  { this is the easiest case for inlined !! }
                  if inlined then
                    begin
                       r^.base:=procinfo^.framepointer;
                       r^.offset:=para_offset-pushedparasize;
                    end;
                  exprasmlist^.concat(new(paicpu,op_ref(op,opsize,r)));
               end;
             LOC_REFERENCE,LOC_MEM:
               begin
                  tempreference:=p^.location.reference;
                  del_reference(p^.location.reference);
                  case p^.resulttype^.deftype of
                    enumdef,
                    orddef :
                      begin
                        case p^.resulttype^.size of
                         8 : begin
                               inc(pushedparasize,8);
                               if inlined then
                                 begin
{$ifndef noAllocEdi}
                                   getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                   emit_ref_reg(A_MOV,S_L,
                                     newreference(tempreference),R_EDI);
                                   r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,r)));
{$ifndef noAllocEdi}
                                   ungetregister32(R_EDI);
                                   getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                   inc(tempreference.offset,4);
                                   emit_ref_reg(A_MOV,S_L,
                                     newreference(tempreference),R_EDI);
                                   r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize+4);
                                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,r)));
{$ifndef noAllocEdi}
                                   ungetregister32(R_EDI);
{$endif noAllocEdi}
                                 end
                               else
                                 begin
                                   inc(tempreference.offset,4);
                                   emit_push_mem(tempreference);
                                   dec(tempreference.offset,4);
                                   emit_push_mem(tempreference);
                                 end;
                             end;
                         4 : begin
                               inc(pushedparasize,4);
                               if inlined then
                                 begin
{$ifndef noAllocEdi}
                                   getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                   emit_ref_reg(A_MOV,S_L,
                                     newreference(tempreference),R_EDI);
                                   r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,r)));
{$ifndef noAllocEdi}
                                   ungetregister32(R_EDI);
{$endif noAllocEdi}
                                 end
                               else
                                 emit_push_mem(tempreference);
                             end;
                       1,2 : begin
                               if alignment=4 then
                                begin
                                  opsize:=S_L;
                                  hreg:=R_EDI;
                                  inc(pushedparasize,4);
                                end
                               else
                                begin
                                  opsize:=S_W;
                                  hreg:=R_DI;
                                  inc(pushedparasize,2);
                                end;
                               if inlined then
                                begin
{$ifndef noAllocEdi}
                                  getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                  emit_ref_reg(A_MOV,opsize,
                                    newreference(tempreference),hreg);
                                  r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                  exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,opsize,hreg,r)));
{$ifndef noAllocEdi}
                                  ungetregister32(R_EDI);
{$endif noAllocEdi}
                                end
                               else if alignment=4 then
                                emit_push_mem_size(tempreference,4)
                               else
                                emit_push_mem_size(tempreference,p^.resulttype^.size);
                             end;
                           else
                             internalerror(234231);
                        end;
                      end;
                    floatdef :
                      begin
                        case pfloatdef(p^.resulttype)^.typ of
                          f32bit,
                          s32real :
                            begin
                               inc(pushedparasize,4);
                               if inlined then
                                 begin
{$ifndef noAllocEdi}
                                    getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                    emit_ref_reg(A_MOV,S_L,
                                      newreference(tempreference),R_EDI);
                                    r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                    exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,r)));
{$ifndef noAllocEdi}
                                    ungetregister32(R_EDI);
{$endif noAllocEdi}
                                 end
                               else
                                 emit_push_mem(tempreference);
                            end;
                          s64real,
                          s64comp :
                            begin
                              inc(pushedparasize,4);
                              inc(tempreference.offset,4);
                              if inlined then
                                begin
{$ifndef noAllocEdi}
                                   getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                   emit_ref_reg(A_MOV,S_L,
                                     newreference(tempreference),R_EDI);
                                   r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,r)));
{$ifndef noAllocEdi}
                                   ungetregister32(R_EDI);
{$endif noAllocEdi}
                                end
                              else
                                emit_push_mem(tempreference);
                              inc(pushedparasize,4);
                              dec(tempreference.offset,4);
                              if inlined then
                                begin
{$ifndef noAllocEdi}
                                   getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                   emit_ref_reg(A_MOV,S_L,
                                     newreference(tempreference),R_EDI);
                                   r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,r)));
{$ifndef noAllocEdi}
                                   ungetregister32(R_EDI);
{$endif noAllocEdi}
                                end
                              else
                                emit_push_mem(tempreference);
                            end;
                          s80real :
                            begin
                              inc(pushedparasize,4);
                              if alignment=4 then
                                inc(tempreference.offset,8)
                              else
                                inc(tempreference.offset,6);
                              if inlined then
                                begin
{$ifndef noAllocEdi}
                                   getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                   emit_ref_reg(A_MOV,S_L,
                                     newreference(tempreference),R_EDI);
                                   r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,r)));
{$ifndef noAllocEdi}
                                   ungetregister32(R_EDI);
{$endif noAllocEdi}
                                end
                              else
                                emit_push_mem(tempreference);
                              dec(tempreference.offset,4);
                              inc(pushedparasize,4);
                              if inlined then
                                begin
{$ifndef noAllocEdi}
                                   getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                   emit_ref_reg(A_MOV,S_L,
                                     newreference(tempreference),R_EDI);
                                   r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,r)));
{$ifndef noAllocEdi}
                                   ungetregister32(R_EDI);
{$endif noAllocEdi}
                                end
                              else
                                emit_push_mem(tempreference);
                              if alignment=4 then
                                begin
                                  opsize:=S_L;
                                  hreg:=R_EDI;
                                  inc(pushedparasize,4);
                                  dec(tempreference.offset,4);
                                end
                              else
                                begin
                                  opsize:=S_W;
                                  hreg:=R_DI;
                                  inc(pushedparasize,2);
                                  dec(tempreference.offset,2);
                                end;
                              if inlined then
                                begin
{$ifndef noAllocEdi}
                                   getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                   emit_ref_reg(A_MOV,opsize,
                                     newreference(tempreference),hreg);
                                   r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,opsize,hreg,r)));
{$ifndef noAllocEdi}
                                   ungetregister32(R_EDI);
{$endif noAllocEdi}
                                end
                              else
                                exprasmlist^.concat(new(paicpu,op_ref(A_PUSH,opsize,
                                  newreference(tempreference))));
                          end;
                        end;
                      end;
                    pointerdef,
                    procvardef,
                    classrefdef:
                      begin
                         inc(pushedparasize,4);
                         if inlined then
                           begin
{$ifndef noAllocEdi}
                              getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                              emit_ref_reg(A_MOV,S_L,
                                newreference(tempreference),R_EDI);
                              r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                              exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,r)));
{$ifndef noAllocEdi}
                              ungetregister32(R_EDI);
{$endif noAllocEdi}
                           end
                         else
                           emit_push_mem(tempreference);
                      end;
                    arraydef,
                    recorddef,
                    stringdef,
                    setdef,
                    objectdef :
                      begin
                         { even some structured types are 32 bit }
                         if is_widestring(p^.resulttype) or
                            is_ansistring(p^.resulttype) or
                            is_smallset(p^.resulttype) or
                            ((p^.resulttype^.deftype in [recorddef,arraydef]) and
                             (
                              (p^.resulttype^.deftype<>arraydef) or not
                              (parraydef(p^.resulttype)^.IsConstructor or
                               parraydef(p^.resulttype)^.isArrayOfConst or
                               is_open_array(p^.resulttype))
                             ) and
                             (p^.resulttype^.size<=4)
                            ) or
                            ((p^.resulttype^.deftype=objectdef) and
                             pobjectdef(p^.resulttype)^.is_class) then
                           begin
                              if (p^.resulttype^.size>2) or
                                 ((alignment=4) and (p^.resulttype^.size>0)) then
                                begin
                                  inc(pushedparasize,4);
                                  if inlined then
                                    begin
                                      r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                      concatcopy(tempreference,r^,4,false,false);
                                      dispose(r);
                                    end
                                  else
                                    emit_push_mem(tempreference);
                                end
                              else
                                begin
                                  if p^.resulttype^.size>0 then
                                    begin
                                      inc(pushedparasize,2);
                                      if inlined then
                                        begin
                                          r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                          concatcopy(tempreference,r^,2,false,false);
                                          dispose(r);
                                        end
                                      else
                                        exprasmlist^.concat(new(paicpu,op_ref(A_PUSH,S_W,newreference(tempreference))));
                                    end;
                                end;
                           end
                         { call by value open array ? }
                         else if is_cdecl then
                           begin
                             { push on stack }
                             size:=align(p^.resulttype^.size,alignment);
                             inc(pushedparasize,size);
                             emit_const_reg(A_SUB,S_L,size,R_ESP);
                             r:=new_reference(R_ESP,0);
                             if (target_os.id=os_i386_win32) and
                                (size>=winstackpagesize) then
                               begin
                                 { we need to copy it reversed to avoid GPF on windows PM }
                                 inc(tempreference.offset,size-4);
                                 inc(r^.offset,size-4);
{$ifndef noAllocEdi}
                                 getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                 emit_ref_reg(A_LEA,S_L,r,R_EDI);
{$ifndef noAllocEdi}
                                 exprasmlist^.concat(new(pairegalloc,alloc(R_ESI)));
{$endif noAllocEdi}
                                 emit_ref_reg(A_LEA,S_L,newreference(tempreference),R_ESI);
                                 exprasmlist^.concat(new(paicpu,op_none(A_STD,S_NO)));
                                 helpsize:=size shr 2;
                                 size:=size and 3;
                                 {if helpsize>1 then , always the case here
                                   as size>= winstackpagesize PM }
                                 if not(R_ECX in unused) then
                                   begin
                                    { we cannot push ecx here because that will also
                                      create a GPF ...
                                      Anyhow ECX should be free when pushing parameters, no? PM }
                                     internalerror(20021021);
                                   end
                                 else
                                   getexplicitregister32(R_ECX);
                                 emit_const_reg(A_MOV,S_L,helpsize,R_ECX);
                                 exprasmlist^.concat(new(paicpu,op_none(A_REP,S_NO)));
                                 exprasmlist^.concat(new(paicpu,op_none(A_MOVSD,S_NO)));
                                 if size>1 then
                                   begin
                                     dec(size,2);
                                     exprasmlist^.concat(new(paicpu,op_none(A_MOVSW,S_NO)));
                                   end;
                                 if size=1 then
                                   exprasmlist^.concat(new(paicpu,op_none(A_MOVSB,S_NO)));
{$ifndef noAllocEdi}
                                 ungetregister32(R_EDI);
                                 exprasmlist^.concat(new(pairegalloc,dealloc(R_ESI)));
{$endif noAllocEdi}
                                 ungetregister32(R_ECX);
                                 { reset direction to normal }
                                 exprasmlist^.concat(new(paicpu,op_none(A_CLD,S_NO)));
                                 { loading SELF-reference again }
                                 maybe_loadself;
                               end
                             else
                               begin
                                 concatcopy(tempreference,r^,size,false,false);
                                 dispose(r);
                               end;
                           end
                         else
                           internalerror(8954);
                      end;
                    else
                      CGMessage(cg_e_illegal_expression);
                  end;
               end;
             LOC_JUMP:
               begin
                  getlabel(hlabel);
                  if alignment=4 then
                   begin
                     opsize:=S_L;
                     inc(pushedparasize,4);
                   end
                  else
                   begin
                     opsize:=S_W;
                     inc(pushedparasize,2);
                   end;
                  emitlab(truelabel);
                  if inlined then
                    begin
                       r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                       emit_const_ref(A_MOV,opsize,1,r);
                    end
                  else
                    exprasmlist^.concat(new(paicpu,op_const(A_PUSH,opsize,1)));
                  emitjmp(C_None,hlabel);
                  emitlab(falselabel);
                  if inlined then
                    begin
                       r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                       emit_const_ref(A_MOV,opsize,0,r);
                    end
                  else
                    exprasmlist^.concat(new(paicpu,op_const(A_PUSH,opsize,0)));
                  emitlab(hlabel);
               end;
             LOC_FLAGS:
               begin
                  if not(R_EAX in unused) then
                    begin
{$ifndef noAllocEdi}
                      getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                      emit_reg_reg(A_MOV,S_L,R_EAX,R_EDI);
                    end;
                  emit_flag2reg(p^.location.resflags,R_AL);
                  emit_reg_reg(A_MOVZX,S_BW,R_AL,R_AX);
                  if alignment=4 then
                   begin
                     opsize:=S_L;
                     hreg:=R_EAX;
                     inc(pushedparasize,4);
                   end
                  else
                   begin
                     opsize:=S_W;
                     hreg:=R_AX;
                     inc(pushedparasize,2);
                   end;
                  if inlined then
                    begin
                       r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                       exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,opsize,hreg,r)));
                    end
                  else
                    exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,opsize,hreg)));
                  if not(R_EAX in unused) then
                    begin
                      emit_reg_reg(A_MOV,S_L,R_EDI,R_EAX);
{$ifndef noAllocEdi}
                      ungetregister32(R_EDI);
{$endif noAllocEdi}
                    end;
               end;
{$ifdef SUPPORT_MMX}
             LOC_MMXREGISTER,
             LOC_CMMXREGISTER:
               begin
                  inc(pushedparasize,8); { was missing !!! (PM) }
                  emit_const_reg(
                    A_SUB,S_L,8,R_ESP);
{$ifdef GDB}
                  if (cs_debuginfo in aktmoduleswitches) and
                     (exprasmlist^.first=exprasmlist^.last) then
                    exprasmlist^.concat(new(pai_force_line,init));
{$endif GDB}
                  if inlined then
                    begin
                       r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                       exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOVQ,S_NO,
                         p^.location.register,r)));
                    end
                  else
                     begin
                        r:=new_reference(R_ESP,0);
                        exprasmlist^.concat(new(paicpu,op_reg_ref(
                          A_MOVQ,S_NO,p^.location.register,r)));
                     end;
               end;
{$endif SUPPORT_MMX}
          end;
      end;



{*****************************************************************************
                           Emit Float Functions
*****************************************************************************}

    procedure floatloadops(t : tfloattype;var op : tasmop;var s : topsize);
      begin
         case t of
            s32real : begin
                         op:=A_FLD;
                         s:=S_FS;
                      end;
            s64real : begin
                         op:=A_FLD;
                         { ???? }
                         s:=S_FL;
                      end;
            s80real : begin
                         op:=A_FLD;
                         s:=S_FX;
                      end;
            s64comp : begin
                         op:=A_FILD;
                         s:=S_IQ;
                      end;
            else internalerror(17);
         end;
      end;


    procedure floatload(t : tfloattype;const ref : treference);
      var
         op : tasmop;
         s : topsize;
      begin
         floatloadops(t,op,s);
         exprasmlist^.concat(new(paicpu,op_ref(op,s,
           newreference(ref))));
         inc(fpuvaroffset);
      end;


    procedure floatstoreops(t : tfloattype;var op : tasmop;var s : topsize);
      begin
         case t of
            s32real : begin
                         op:=A_FSTP;
                         s:=S_FS;
                      end;
            s64real : begin
                         op:=A_FSTP;
                         s:=S_FL;
                      end;
            s80real : begin
                         op:=A_FSTP;
                          s:=S_FX;
                      end;
            s64comp : begin
                         op:=A_FISTP;
                         s:=S_IQ;
                      end;
         else
           internalerror(17);
         end;
      end;


    procedure floatstore(t : tfloattype;const ref : treference);
      var
         op : tasmop;
         s : topsize;
      begin
         floatstoreops(t,op,s);
         exprasmlist^.concat(new(paicpu,op_ref(op,s,
           newreference(ref))));
         dec(fpuvaroffset);
      end;


{*****************************************************************************
                           Emit Functions
*****************************************************************************}

    procedure maketojumpbool(p : ptree);
    {
      produces jumps to true respectively false labels using boolean expressions
    }
      var
        opsize : topsize;
        storepos : tfileposinfo;
      begin
         if p^.error then
           exit;
         storepos:=aktfilepos;
         aktfilepos:=p^.fileinfo;
         if is_boolean(p^.resulttype) then
           begin
              if is_constboolnode(p) then
                begin
                   if p^.value<>0 then
                     emitjmp(C_None,truelabel)
                   else
                     emitjmp(C_None,falselabel);
                end
              else
                begin
                   opsize:=def_opsize(p^.resulttype);
                   case p^.location.loc of
                      LOC_CREGISTER,LOC_REGISTER : begin
                                        emit_reg_reg(A_OR,opsize,p^.location.register,
                                          p^.location.register);
                                        ungetregister(p^.location.register);
                                        emitjmp(C_NZ,truelabel);
                                        emitjmp(C_None,falselabel);
                                     end;
                      LOC_MEM,LOC_REFERENCE : begin
                                        emit_const_ref(
                                          A_CMP,opsize,0,newreference(p^.location.reference));
                                        del_reference(p^.location.reference);
                                        emitjmp(C_NZ,truelabel);
                                        emitjmp(C_None,falselabel);
                                     end;
                      LOC_FLAGS : begin
                                     emitjmp(flag_2_cond[p^.location.resflags],truelabel);
                                     emitjmp(C_None,falselabel);
                                  end;
                   end;
                end;
           end
         else
           CGMessage(type_e_mismatch);
         aktfilepos:=storepos;
      end;


    { produces if necessary overflowcode }
    procedure emitoverflowcheck(p:ptree);
      var
         hl : pasmlabel;
      begin
         if not(cs_check_overflow in aktlocalswitches) then
          exit;
         getlabel(hl);
         if not ((p^.resulttype^.deftype=pointerdef) or
                ((p^.resulttype^.deftype=orddef) and
                 (porddef(p^.resulttype)^.typ in [u64bit,u16bit,u32bit,u8bit,uchar,
                                                  bool8bit,bool16bit,bool32bit]))) then
           emitjmp(C_NO,hl)
         else
           emitjmp(C_NB,hl);
         emitcall('FPC_OVERFLOW');
         emitlab(hl);
      end;

    { produces range check code, while one of the operands is a 64 bit
      integer }
    procedure emitrangecheck64(p : ptree;todef : pdef);

      var
        neglabel,
        poslabel,
        endlabel: pasmlabel;
        href   : preference;
        hreg   : tregister;
        hdef   :  porddef;
        fromdef : pdef;
        opcode : tasmop;
        opsize   : topsize;
        oldregisterdef: boolean;
        from_signed,to_signed: boolean;

      begin
         fromdef:=p^.resulttype;
         from_signed := is_signed(fromdef);
         to_signed := is_signed(todef);

         if not is_64bitint(todef) then
           begin
             oldregisterdef := registerdef;
             registerdef := false;

             { get the high dword in a register }
             if p^.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
               hreg := p^.location.registerhigh
             else
               begin
                 hreg := getexplicitregister32(R_EDI);
                 href := newreference(p^.location.reference);
                 inc(href^.offset,4);
                 emit_ref_reg(A_MOV,S_L,href,hreg);
               end;
             getlabel(poslabel);

             { check high dword, must be 0 (for positive numbers) }
             emit_reg_reg(A_TEST,S_L,hreg,hreg);
             emitjmp(C_E,poslabel);

             { It can also be $ffffffff, but only for negative numbers }
             if from_signed and to_signed then
               begin
                 getlabel(neglabel);
                 emit_const_reg(A_CMP,S_L,$ffffffff,hreg);
                 emitjmp(C_E,neglabel);
               end;
             if hreg = R_EDI then
               ungetregister32(hreg);
             { For all other values we have a range check error }
             emitcall('FPC_RANGEERROR');

             { if the high dword = 0, the low dword can be considered a }
             { simple cardinal                                          }
             emitlab(poslabel);
             new(hdef,init(u32bit,0,$ffffffff));
             { the real p^.resulttype is already saved in fromdef }
             p^.resulttype := hdef;
             emitrangecheck(p,todef);
             dispose(hdef,done);
             { restore original resulttype }
             p^.resulttype := todef;

             if from_signed and to_signed then
               begin
                 getlabel(endlabel);
                 emitjmp(C_None,endlabel);
                 { if the high dword = $ffffffff, then the low dword (when }
                 { considered as a longint) must be < 0                    }
                 emitlab(neglabel);
                 if p^.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                   hreg := p^.location.registerlow
                 else
                   begin
                     hreg := getexplicitregister32(R_EDI);
                     emit_ref_reg(A_MOV,S_L,
                       newreference(p^.location.reference),hreg);
                   end;
                 { get a new neglabel (JM) }
                 getlabel(neglabel);
                 emit_reg_reg(A_TEST,S_L,hreg,hreg);
                 if hreg = R_EDI then
                   ungetregister32(hreg);
                 emitjmp(C_L,neglabel);

                 emitcall('FPC_RANGEERROR');

                 { if we get here, the 64bit value lies between }
                 { longint($80000000) and -1 (JM)               }
                 emitlab(neglabel);
                 new(hdef,init(s32bit,$80000000,-1));
                 p^.resulttype := hdef;
                 emitrangecheck(p,todef);
                 dispose(hdef,done);
                 emitlab(endlabel);
               end;
             registerdef := oldregisterdef;
             p^.resulttype := fromdef;
             { restore p's resulttype }
           end
         else
           { todef = 64bit int }
           { no 64bit subranges supported, so only a small check is necessary }

           { if both are signed or both are unsigned, no problem! }
           if (from_signed xor to_signed) and
              { also not if the fromdef is unsigned and < 64bit, since that will }
              { always fit in a 64bit int (todef is 64bit)                       }
              (from_signed or
               (porddef(fromdef)^.typ = u64bit)) then
             begin
               { in all cases, there is only a problem if the higest bit is set }
               if p^.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                 if is_64bitint(fromdef) then
                   hreg := p^.location.registerhigh
                 else
                   hreg := p^.location.register
               else
                 begin
                   hreg := getexplicitregister32(R_EDI);
                   case p^.resulttype^.size of
                     1: opsize := S_BL;
                     2: opsize := S_WL;
                     4,8: opsize := S_L;
                   end;
                   if opsize in [S_BL,S_WL] then
                     if from_signed then
                       opcode := A_MOVSX
                     else opcode := A_MOVZX
                   else
                     opcode := A_MOV;
                   href := newreference(p^.location.reference);
                   if p^.resulttype^.size = 8 then
                     inc(href^.offset,4);
                   emit_ref_reg(opcode,opsize,href,hreg);
                 end;
               getlabel(poslabel);
               emit_reg_reg(A_TEST,regsize(hreg),hreg,hreg);
               if hreg = R_EDI then
                 ungetregister32(hreg);
               emitjmp(C_GE,poslabel);
               emitcall('FPC_RANGEERROR');
               emitlab(poslabel);
             end;
      end;

     { produces if necessary rangecheckcode }
     procedure emitrangecheck(p:ptree;todef:pdef);
     {
       generate range checking code for the value at location t. The
       type used is the checked against todefs ranges. fromdef (p^.resulttype)
       is the original type used at that location, when both defs are
       equal the check is also insert (needed for succ,pref,inc,dec)
     }
      var
        neglabel,
        poslabel : pasmlabel;
        href   : treference;
        rstr   : string;
        hreg   : tregister;
        opsize : topsize;
        op     : tasmop;
        fromdef : pdef;
        lto,hto,
        lfrom,hfrom : longint;
        doublebound,
        is_reg,
        popecx : boolean;
      begin
        { range checking on and range checkable value? }
        if not(cs_check_range in aktlocalswitches) or
           not(todef^.deftype in [orddef,enumdef,arraydef]) then
          exit;
        { only check when assigning to scalar, subranges are different,
          when todef=fromdef then the check is always generated }
        fromdef:=p^.resulttype;
        if is_64bitint(fromdef) or is_64bitint(todef) then
          begin
             emitrangecheck64(p,todef);
             exit;
          end;
        {we also need lto and hto when checking if we need to use doublebound!
        (JM)}
        getrange(todef,lto,hto);
        if todef<>fromdef then
         begin
           getrange(p^.resulttype,lfrom,hfrom);
           { first check for not being u32bit, then if the to is bigger than
             from }
           if (lto<hto) and (lfrom<hfrom) and
              (lto<=lfrom) and (hto>=hfrom) then
            exit;
         end;
        { generate the rangecheck code for the def where we are going to
          store the result }
        doublebound:=false;
        case todef^.deftype of
          orddef :
            begin
              porddef(todef)^.genrangecheck;
              rstr:=porddef(todef)^.getrangecheckstring;
              doublebound:=
                ((porddef(todef)^.typ=u32bit) and (lto>hto)) or
                (is_signed(todef) and (porddef(fromdef)^.typ=u32bit)) or
                (is_signed(fromdef) and (porddef(todef)^.typ=u32bit));
            end;
          enumdef :
            begin
              penumdef(todef)^.genrangecheck;
              rstr:=penumdef(todef)^.getrangecheckstring;
            end;
          arraydef :
            begin
              parraydef(todef)^.genrangecheck;
              rstr:=parraydef(todef)^.getrangecheckstring;
              doublebound:=(lto>hto);
            end;
        end;
      { get op and opsize }
        opsize:=def2def_opsize(fromdef,u32bitdef);
        if opsize in [S_B,S_W,S_L] then
         op:=A_MOV
        else
         if is_signed(fromdef) then
          op:=A_MOVSX
         else
          op:=A_MOVZX;
        is_reg:=(p^.location.loc in [LOC_REGISTER,LOC_CREGISTER]);
        if is_reg then
          hreg:=p^.location.register;
        if not target_os.use_bound_instruction then
         begin
           { FPC_BOUNDCHECK needs to be called with
              %ecx - value
              %edi - pointer to the ranges }
           popecx:=false;
           if not(is_reg) or
              (p^.location.register<>R_ECX) then
            begin
              if not(R_ECX in unused) then
               begin
                 exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,R_ECX)));
                 popecx:=true;
               end
              else
                getexplicitregister32(R_ECX);
              if is_reg then
               emit_reg_reg(op,opsize,p^.location.register,R_ECX)
              else
               emit_ref_reg(op,opsize,newreference(p^.location.reference),R_ECX);
            end;
           if doublebound then
            begin
              getlabel(neglabel);
              getlabel(poslabel);
              emit_reg_reg(A_TEST,S_L,R_ECX,R_ECX);
              emitjmp(C_L,neglabel);
            end;
           { insert bound instruction only }
           getexplicitregister32(R_EDI);
           exprasmlist^.concat(new(paicpu,op_sym_ofs_reg(A_MOV,S_L,newasmsymbol(rstr),0,R_EDI)));
           emitcall('FPC_BOUNDCHECK');
           ungetregister32(R_EDI);
           { u32bit needs 2 checks }
           if doublebound then
            begin
              emitjmp(C_None,poslabel);
              emitlab(neglabel);
              { if a cardinal is > $7fffffff, this is an illegal longint }
              { value (and vice versa)! (JM)                             }
              if ((todef^.deftype = orddef) and
                  ((is_signed(todef) and (porddef(fromdef)^.typ=u32bit)) or
                   (is_signed(fromdef) and (porddef(todef)^.typ=u32bit)))) or
                 { similar for array indexes (JM) }
                 ((todef^.deftype = arraydef) and
                  (((lto < 0) and (porddef(fromdef)^.typ=u32bit)) or
                   ((lto >= 0) and is_signed(fromdef)))) then
                emitcall('FPC_RANGEERROR')
              else
                begin
                  getexplicitregister32(R_EDI);
                  exprasmlist^.concat(new(paicpu,op_sym_ofs_reg(A_MOV,S_L,newasmsymbol(rstr),8,R_EDI)));
                  emitcall('FPC_BOUNDCHECK');
                  ungetregister32(R_EDI);
                end;
              emitlab(poslabel);
            end;
           if popecx then
            exprasmlist^.concat(new(paicpu,op_reg(A_POP,S_L,R_ECX)))
           else
             if (p^.location.register <> R_ECX) then
               ungetregister32(R_ECX);
         end
        else
         begin
           reset_reference(href);
           href.symbol:=newasmsymbol(rstr);
           { load the value in a register }
           if is_reg then
            begin
              { be sure that hreg is a 32 bit reg, if not load it in %edi }
              if p^.location.register in [R_EAX..R_EDI] then
               hreg:=p^.location.register
              else
               begin
                 getexplicitregister32(R_EDI);
                 emit_reg_reg(op,opsize,p^.location.register,R_EDI);
                 hreg:=R_EDI;
               end;
            end
           else
            begin
              getexplicitregister32(R_EDI);
              emit_ref_reg(op,opsize,newreference(p^.location.reference),R_EDI);
              hreg:=R_EDI;
            end;
           if doublebound then
            begin
              getlabel(neglabel);
              getlabel(poslabel);
              emit_reg_reg(A_TEST,S_L,hreg,hreg);
              emitjmp(C_L,neglabel);
            end;
           { insert bound instruction only }
           exprasmlist^.concat(new(paicpu,op_reg_ref(A_BOUND,S_L,hreg,newreference(href))));
           { u32bit needs 2 checks }
           if doublebound then
            begin
              href.offset:=8;
              emitjmp(C_None,poslabel);
              emitlab(neglabel);
              exprasmlist^.concat(new(paicpu,op_reg_ref(A_BOUND,S_L,hreg,newreference(href))));
              emitlab(poslabel);
            end;
           if hreg = R_EDI then
             ungetregister32(R_EDI);
         end;
      end;


    procedure concatcopy(source,dest : treference;size : longint;delsource,loadref : boolean);

      const
         isizes : array[0..3] of topsize=(S_L,S_B,S_W,S_B);
         ishr : array[0..3] of byte=(2,0,1,0);

      var
         ecxpushed : boolean;
         helpsize : longint;
         i : byte;
         reg8,reg32 : tregister;
         swap : boolean;
         storeoffset : longint;


         procedure maybepushecx;
         begin
           if not(R_ECX in unused) then
             begin
               exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,R_ECX)));
               ecxpushed:=true;
             end
           else getexplicitregister32(R_ECX);
         end;

      begin
{$IfNDef regallocfix}
        If delsource then
           begin
             del_reference(source);
             delsource:=false;
           end;
{$EndIf regallocfix}
         storeoffset:=source.offset;
         if (not loadref) and
            ((size<=8) or
             (not(cs_littlesize in aktglobalswitches ) and (size<=12))) then
           begin
              helpsize:=size shr 2;
{$ifndef noAllocEdi}
              getexplicitregister32(R_EDI);
{$endif noAllocEdi}
              for i:=1 to helpsize do
                begin
                   emit_ref_reg(A_MOV,S_L,newreference(source),R_EDI);
{$ifdef regallocfix}
                   If (size = 4) and delsource then
                     begin
                       del_reference(source);
                       delsource:=false;
                     end;
{$endif regallocfix}
                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,newreference(dest))));
                   inc(source.offset,4);
                   inc(dest.offset,4);
                   dec(size,4);
                end;
              if size>1 then
                begin
                   emit_ref_reg(A_MOV,S_W,newreference(source),R_DI);
{$ifdef regallocfix}
                   If (size = 2) and delsource then
                     begin
                       del_reference(source);
                       delsource:=false;
                     end;
{$endif regallocfix}
                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_W,R_DI,newreference(dest))));
                   inc(source.offset,2);
                   inc(dest.offset,2);
                   dec(size,2);
                end;
{$ifndef noAllocEdi}
              ungetregister32(R_EDI);
{$endif noAllocEdi}
              if size>0 then
                begin
                   { and now look for an 8 bit register }
                   swap:=false;
                   if R_EAX in unused then reg8:=reg32toreg8(getexplicitregister32(R_EAX))
                   else if R_EDX in unused then reg8:=reg32toreg8(getexplicitregister32(R_EDX))
                   else if R_EBX in unused then reg8:=reg32toreg8(getexplicitregister32(R_EBX))
                   else if R_ECX in unused then reg8:=reg32toreg8(getexplicitregister32(R_ECX))
                   else
                      begin
                         swap:=true;
                         { we need only to check 3 registers, because }
                         { one is always not index or base          }
                         if (dest.base<>R_EAX) and (dest.index<>R_EAX) then
                           begin
                              reg8:=R_AL;
                              reg32:=R_EAX;
                           end
                         else if (dest.base<>R_EBX) and (dest.index<>R_EBX) then
                           begin
                              reg8:=R_BL;
                              reg32:=R_EBX;
                           end
                         else if (dest.base<>R_ECX) and (dest.index<>R_ECX) then
                           begin
                              reg8:=R_CL;
                              reg32:=R_ECX;
                           end;
                      end;
                   if swap then
                     { was earlier XCHG, of course nonsense }
                     begin
{$ifndef noAllocEdi}
                       getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                       emit_reg_reg(A_MOV,S_L,reg32,R_EDI);
                     end;
                   emit_ref_reg(A_MOV,S_B,newreference(source),reg8);
{$ifdef regallocfix}
                   If delsource then
                     begin
                       del_reference(source);
                       del
{$endif regallocfix}
                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_B,reg8,newreference(dest))));
                   if swap then
                     begin
                       emit_reg_reg(A_MOV,S_L,R_EDI,reg32);
{$ifndef noAllocEdi}
                       ungetregister32(R_EDI);
{$endif noAllocEdi}
                     end
                   else
                     ungetregister(reg8);
                end;
           end
         else
           begin
{$ifndef noAllocEdi}
              getexplicitregister32(R_EDI);
{$endif noAllocEdi}
              emit_ref_reg(A_LEA,S_L,newreference(dest),R_EDI);
{$ifdef regallocfix}
             {is this ok?? (JM)}
              del_reference(dest);
{$endif regallocfix}
{$ifndef noAllocEdi}
              exprasmlist^.concat(new(pairegalloc,alloc(R_ESI)));
{$endif noAllocEdi}
              if loadref then
                emit_ref_reg(A_MOV,S_L,newreference(source),R_ESI)
              else
                begin
                  emit_ref_reg(A_LEA,S_L,newreference(source),R_ESI);
{$ifdef regallocfix}
                  if delsource then
                    del_reference(source);
{$endif regallocfix}
                end;

              exprasmlist^.concat(new(paicpu,op_none(A_CLD,S_NO)));
              ecxpushed:=false;
              if cs_littlesize in aktglobalswitches  then
                begin
                   maybepushecx;
                   emit_const_reg(A_MOV,S_L,size,R_ECX);
                   exprasmlist^.concat(new(paicpu,op_none(A_REP,S_NO)));
                   exprasmlist^.concat(new(paicpu,op_none(A_MOVSB,S_NO)));
                end
              else
                begin
                   helpsize:=size shr 2;
                   size:=size and 3;
                   if helpsize>1 then
                    begin
                      maybepushecx;
                      emit_const_reg(A_MOV,S_L,helpsize,R_ECX);
                      exprasmlist^.concat(new(paicpu,op_none(A_REP,S_NO)));
                    end;
                   if helpsize>0 then
                    exprasmlist^.concat(new(paicpu,op_none(A_MOVSD,S_NO)));
                   if size>1 then
                     begin
                        dec(size,2);
                        exprasmlist^.concat(new(paicpu,op_none(A_MOVSW,S_NO)));
                     end;
                   if size=1 then
                     exprasmlist^.concat(new(paicpu,op_none(A_MOVSB,S_NO)));
                end;
{$ifndef noAllocEdi}
              ungetregister32(R_EDI);
              exprasmlist^.concat(new(pairegalloc,dealloc(R_ESI)));
{$endif noAllocEdi}
              if ecxpushed then
                exprasmlist^.concat(new(paicpu,op_reg(A_POP,S_L,R_ECX)))
              else
                ungetregister32(R_ECX);

              { loading SELF-reference again }
              maybe_loadself;
           end;
         if delsource then
           begin
             source.offset:=storeoffset;
             ungetiftemp(source);
           end;
      end;


    procedure emitloadord2reg(const location:Tlocation;orddef:Porddef;
                              destreg:Tregister;delloc:boolean);

    {A lot smaller and less bug sensitive than the original unfolded loads.}

    var tai:Paicpu;
        r:Preference;

    begin
        tai := nil;
        case location.loc of
            LOC_REGISTER,LOC_CREGISTER:
                begin
                    case orddef^.typ of
                        u8bit,uchar,bool8bit:
                            tai:=new(paicpu,op_reg_reg(A_MOVZX,S_BL,location.register,destreg));
                        s8bit:
                            tai:=new(paicpu,op_reg_reg(A_MOVSX,S_BL,location.register,destreg));
                        u16bit,uwidechar,bool16bit:
                            tai:=new(paicpu,op_reg_reg(A_MOVZX,S_WL,location.register,destreg));
                        s16bit:
                            tai:=new(paicpu,op_reg_reg(A_MOVSX,S_WL,location.register,destreg));
                        u32bit,bool32bit,s32bit:
                            if location.register <> destreg then
                              tai:=new(paicpu,op_reg_reg(A_MOV,S_L,location.register,destreg));
                        else
                          internalerror(330);
                    end;
                    if delloc then
                        ungetregister(location.register);
                end;
            LOC_MEM,
            LOC_REFERENCE:
                begin
                    if location.reference.is_immediate then
                     tai:=new(paicpu,op_const_reg(A_MOV,S_L,location.reference.offset,destreg))
                    else
                     begin
                       r:=newreference(location.reference);
                       case orddef^.typ of
                         u8bit,uchar,bool8bit:
                            tai:=new(paicpu,op_ref_reg(A_MOVZX,S_BL,r,destreg));
                         s8bit:
                            tai:=new(paicpu,op_ref_reg(A_MOVSX,S_BL,r,destreg));
                         u16bit,uwidechar,bool16bit:
                            tai:=new(paicpu,op_ref_reg(A_MOVZX,S_WL,r,destreg));
                         s16bit:
                            tai:=new(paicpu,op_ref_reg(A_MOVSX,S_WL,r,destreg));
                         u32bit,bool32bit:
                            tai:=new(paicpu,op_ref_reg(A_MOV,S_L,r,destreg));
                         s32bit:
                            tai:=new(paicpu,op_ref_reg(A_MOV,S_L,r,destreg));
                        else
                          internalerror(330);
                       end;
                     end;
                    if delloc then
                        del_reference(location.reference);
                end
            else
                internalerror(6);
        end;
        if assigned(tai) then
          exprasmlist^.concat(tai);
    end;

    { if necessary the self register is reloaded after a call}
    procedure maybe_loadself;

      var
         hp : preference;
         p : pprocinfo;
         i : longint;

      begin
         if assigned(procinfo^._class) then
           begin
{$ifndef noAllocEdi}
              exprasmlist^.concat(new(pairegalloc,alloc(R_ESI)));
{$endif noAllocEdi}
              if lexlevel>normal_function_level then
                begin
                   new(hp);
                   reset_reference(hp^);
                   hp^.offset:=procinfo^.framepointer_offset;
                   hp^.base:=procinfo^.framepointer;
                   emit_ref_reg(A_MOV,S_L,hp,R_ESI);
                   p:=procinfo^.parent;
                   for i:=3 to lexlevel-1 do
                     begin
                        new(hp);
                        reset_reference(hp^);
                        hp^.offset:=p^.framepointer_offset;
                        hp^.base:=R_ESI;
                        emit_ref_reg(A_MOV,S_L,hp,R_ESI);
                        p:=p^.parent;
                     end;
                   new(hp);
                   reset_reference(hp^);
                   hp^.offset:=p^.selfpointer_offset;
                   hp^.base:=R_ESI;
                   emit_ref_reg(A_MOV,S_L,hp,R_ESI);
                end
              else
                begin
                   new(hp);
                   reset_reference(hp^);
                   hp^.offset:=procinfo^.selfpointer_offset;
                   hp^.base:=procinfo^.framepointer;
                   emit_ref_reg(A_MOV,S_L,hp,R_ESI);
                end;
           end;
      end;


   { DO NOT RELY on the fact that the ptree is not yet swaped
     because of inlining code PM }
    procedure firstcomplex(p : ptree);
      var
         hp : ptree;
      begin
         { always calculate boolean AND and OR from left to right }
         if (p^.treetype in [orn,andn]) and
            (p^.left^.resulttype^.deftype=orddef) and
            (porddef(p^.left^.resulttype)^.typ in [bool8bit,bool16bit,bool32bit]) then
           begin
             { p^.swaped:=false}
             if p^.swaped then
               internalerror(234234);
           end
         else
           if (((p^.location.loc=LOC_FPU) and
                (p^.right^.registersfpu > p^.left^.registersfpu)) or
               ((((p^.left^.registersfpu = 0) and
                  (p^.right^.registersfpu = 0)) or
                 (p^.location.loc<>LOC_FPU)) and
                (p^.left^.registers32<p^.right^.registers32))) and
           { the following check is appropriate, because all }
           { 4 registers are rarely used and it is thereby   }
           { achieved that the extra code is being dropped   }
           { by exchanging not commutative operators     }
              (p^.right^.registers32<=4) then
            begin
              hp:=p^.left;
              p^.left:=p^.right;
              p^.right:=hp;
              p^.swaped:=not p^.swaped;
            end;
         {else
           p^.swaped:=false; do not modify }
      end;


{*****************************************************************************
                            Entry/Exit Code Functions
*****************************************************************************}

  procedure genprofilecode;
    var
      pl : pasmlabel;
    begin
      if (po_assembler in aktprocsym^.definition^.procoptions) then
       exit;
      case target_info.target of
         target_i386_freebsd,
         target_i386_openbsd,
         target_i386_netbsd,
         target_i386_linux:
           begin
              getlabel(pl);
              case target_info.target of
                target_i386_freebsd: emitinsertcall('.mcount');
                target_i386_netbsd : emitinsertcall('__mcount');
                target_i386_openbsd: emitinsertcall('.mcount'); // not sure yet
                target_i386_linux  : emitinsertcall('mcount');
                end;
              usedinproc:=usedinproc or ($80 shr byte(R_EDX));
              exprasmlist^.insert(new(paicpu,op_sym_ofs_reg(A_MOV,S_L,pl,0,R_EDX)));
              exprasmlist^.insert(new(pai_section,init(sec_code)));
              exprasmlist^.insert(new(pai_const,init_32bit(0)));
              exprasmlist^.insert(new(pai_label,init(pl)));
              exprasmlist^.insert(new(pai_align,init(4)));
              exprasmlist^.insert(new(pai_section,init(sec_data)));
           end;

         target_i386_win32:
           begin
              getlabel(pl);
              emitinsertcall('_mcount');
              usedinproc:=usedinproc or ($80 shr byte(R_EDX));
              exprasmlist^.insert(new(paicpu,op_sym_ofs_reg(A_MOV,S_L,pl,0,R_EDX)));
              exprasmlist^.insert(new(pai_section,init(sec_code)));
              exprasmlist^.insert(new(pai_const,init_32bit(0)));
              exprasmlist^.insert(new(pai_label,init(pl)));
              exprasmlist^.insert(new(pai_align,init(4)));
              exprasmlist^.insert(new(pai_section,init(sec_data)));
           end;

         target_i386_go32v2:
           begin
              emitinsertcall('MCOUNT');
           end;
      end;
    end;


    procedure generate_interrupt_stackframe_entry;
      begin
         { save the registers of an interrupt procedure }
         exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_L,R_EAX)));
         exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_L,R_EBX)));
         exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_L,R_ECX)));
         exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_L,R_EDX)));
         exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_L,R_ESI)));
         exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_L,R_EDI)));

         { .... also the segment registers }
         exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_W,R_DS)));
         exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_W,R_ES)));
         exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_W,R_FS)));
         exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_W,R_GS)));
      end;


    procedure generate_interrupt_stackframe_exit;
      begin
         { restore the registers of an interrupt procedure }
         { this was all with entrycode instead of exitcode !!}
         procinfo^.aktexitcode^.concat(new(paicpu,op_reg(A_POP,S_L,R_EAX)));
         procinfo^.aktexitcode^.concat(new(paicpu,op_reg(A_POP,S_L,R_EBX)));
         procinfo^.aktexitcode^.concat(new(paicpu,op_reg(A_POP,S_L,R_ECX)));
         procinfo^.aktexitcode^.concat(new(paicpu,op_reg(A_POP,S_L,R_EDX)));
         procinfo^.aktexitcode^.concat(new(paicpu,op_reg(A_POP,S_L,R_ESI)));
         procinfo^.aktexitcode^.concat(new(paicpu,op_reg(A_POP,S_L,R_EDI)));

         { .... also the segment registers }
         procinfo^.aktexitcode^.concat(new(paicpu,op_reg(A_POP,S_W,R_DS)));
         procinfo^.aktexitcode^.concat(new(paicpu,op_reg(A_POP,S_W,R_ES)));
         procinfo^.aktexitcode^.concat(new(paicpu,op_reg(A_POP,S_W,R_FS)));
         procinfo^.aktexitcode^.concat(new(paicpu,op_reg(A_POP,S_W,R_GS)));

        { this restores the flags }
         procinfo^.aktexitcode^.concat(new(paicpu,op_none(A_IRET,S_NO)));
      end;


  { generates the code for threadvar initialisation }
  procedure initialize_threadvar(p : pnamedindexobject);{$ifndef FPC}far;{$endif}

    var
       hr : treference;

    begin
       if (psym(p)^.typ=varsym) and
          (vo_is_thread_var in pvarsym(p)^.varoptions) then
         begin
            exprasmlist^.concat(new(paicpu,op_const(A_PUSH,S_L,pvarsym(p)^.getsize)));
            reset_reference(hr);
            hr.symbol:=newasmsymbol(pvarsym(p)^.mangledname);
            emitpushreferenceaddr(hr);
            emitcall('FPC_INIT_THREADVAR');
         end;
    end;

    { initilizes data of type t                           }
    { if is_already_ref is true then the routines assumes }
    { that r points to the data to initialize             }
    procedure initialize(t : pdef;const ref : treference;is_already_ref : boolean);

      var
         hr : treference;

      begin
         if is_ansistring(t) or
           is_widestring(t) then
           begin
              emit_const_ref(A_MOV,S_L,0,
                newreference(ref));
           end
         else
           begin
              reset_reference(hr);
              hr.symbol:=t^.get_inittable_label;
              emitpushreferenceaddr(hr);
              if is_already_ref then
                exprasmlist^.concat(new(paicpu,op_ref(A_PUSH,S_L,
                  newreference(ref))))
              else
                emitpushreferenceaddr(ref);
              emitcall('FPC_INITIALIZE');
           end;
      end;

    { finalizes data of type t                            }
    { if is_already_ref is true then the routines assumes }
    { that r points to the data to finalizes              }
    procedure finalize(t : pdef;const ref : treference;is_already_ref : boolean);

      var
         r : treference;

      begin
         if is_ansistring(t) or
           is_widestring(t) then
           begin
              decrstringref(t,ref);
           end
         else
           begin
              reset_reference(r);
              r.symbol:=t^.get_inittable_label;
              emitpushreferenceaddr(r);
              if is_already_ref then
                exprasmlist^.concat(new(paicpu,op_ref(A_PUSH,S_L,
                  newreference(ref))))
              else
                emitpushreferenceaddr(ref);
              emitcall('FPC_FINALIZE');
           end;
      end;


  { generates the code for initialisation of local data }
  procedure initialize_data(p : pnamedindexobject);{$ifndef FPC}far;{$endif}

    var
       hr : treference;

    begin
       if (psym(p)^.typ=varsym) and
          assigned(pvarsym(p)^.vartype.def) and
          not((pvarsym(p)^.vartype.def^.deftype=objectdef) and
            pobjectdef(pvarsym(p)^.vartype.def)^.is_class) and
          pvarsym(p)^.vartype.def^.needs_inittable then
         begin
            if assigned(procinfo) then
              procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
            reset_reference(hr);
            if psym(p)^.owner^.symtabletype in [localsymtable,inlinelocalsymtable] then
              begin
                 hr.base:=procinfo^.framepointer;
                 hr.offset:=-pvarsym(p)^.address+pvarsym(p)^.owner^.address_fixup;
              end
            else
              begin
                 hr.symbol:=newasmsymbol(pvarsym(p)^.mangledname);
              end;
            initialize(pvarsym(p)^.vartype.def,hr,false);
         end;
    end;

  { generates the code for incrementing the reference count of parameters }
  procedure incr_data(p : pnamedindexobject);{$ifndef FPC}far;{$endif}

    var
       hr : treference;

    begin
       if (psym(p)^.typ=varsym) and
          not((pvarsym(p)^.vartype.def^.deftype=objectdef) and
            pobjectdef(pvarsym(p)^.vartype.def)^.is_class) and
          pvarsym(p)^.vartype.def^.needs_inittable and
          (pvarsym(p)^.varspez=vs_value) then
         begin
            procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
            reset_reference(hr);
            hr.symbol:=pvarsym(p)^.vartype.def^.get_inittable_label;
            emitpushreferenceaddr(hr);
            reset_reference(hr);
            hr.base:=procinfo^.framepointer;
            if assigned(pvarsym(p)^.localvarsym) then
             hr.offset:=-pvarsym(p)^.localvarsym^.address+pvarsym(p)^.localvarsym^.owner^.address_fixup
            else
             hr.offset:=pvarsym(p)^.address+procinfo^.para_offset;
            emitpushreferenceaddr(hr);
            reset_reference(hr);

            emitcall('FPC_ADDREF');
         end;
    end;

  { generates the code for finalisation of local data }
  procedure finalize_data(p : pnamedindexobject);{$ifndef FPC}far;{$endif}

    var
       hr : treference;

    begin
       if (psym(p)^.typ=varsym) and
          not(vo_is_local_copy in pvarsym(p)^.varoptions) and
          assigned(pvarsym(p)^.vartype.def) and
          not((pvarsym(p)^.vartype.def^.deftype=objectdef) and
          pobjectdef(pvarsym(p)^.vartype.def)^.is_class) and
          pvarsym(p)^.vartype.def^.needs_inittable then
         begin
            { not all kind of parameters need to be finalized  }
            if (psym(p)^.owner^.symtabletype=parasymtable) and
               (pvarsym(p)^.varspez in [vs_var,vs_const]) then
              exit;
            if assigned(procinfo) then
              procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
            reset_reference(hr);
            case psym(p)^.owner^.symtabletype of
               localsymtable,inlinelocalsymtable:
                 begin
                    hr.base:=procinfo^.framepointer;
                    hr.offset:=-pvarsym(p)^.address+pvarsym(p)^.owner^.address_fixup;
                 end;
               parasymtable,inlineparasymtable:
                 begin
                    hr.base:=procinfo^.framepointer;
                    if assigned(pvarsym(p)^.localvarsym) then
                     hr.offset:=-pvarsym(p)^.localvarsym^.address+pvarsym(p)^.localvarsym^.owner^.address_fixup
                    else
                     hr.offset:=pvarsym(p)^.address+procinfo^.para_offset;
                 end;
               else
                 hr.symbol:=newasmsymbol(pvarsym(p)^.mangledname);
            end;
            finalize(pvarsym(p)^.vartype.def,hr,false);
         end;
    end;


  { generates the code to make local copies of the value parameters }
  procedure copyvalueparas(p : pnamedindexobject);{$ifndef fpc}far;{$endif}
    var
      href1,href2 : treference;
      r    : preference;
      power,len,highaddress  : longint;
      opsize : topsize;
      again,ok : pasmlabel;
    begin
       if (psym(p)^.typ=varsym) and
          (pvarsym(p)^.varspez=vs_value) and
          (push_addr_param(pvarsym(p)^.vartype.def)) then
        begin
          if is_open_array(pvarsym(p)^.vartype.def) or
             is_array_of_const(pvarsym(p)^.vartype.def) then
           begin
              { get stack space }
              new(r);
              reset_reference(r^);
              r^.base:=procinfo^.framepointer;
              if assigned(pvarsym(p)^.owner^.defowner) and
                 (pvarsym(p)^.owner^.defowner^.deftype=procdef) and
                 (pocall_leftright in pprocdef(pvarsym(p)^.owner^.defowner)^.proccalloptions) then
                highaddress:=pvarsym(p)^.address-4+procinfo^.para_offset
              else
                highaddress:=pvarsym(p)^.address+4+procinfo^.para_offset;
              r^.offset:=highaddress;
{$ifndef noAllocEdi}
              getexplicitregister32(R_EDI);
{$endif noAllocEdi}
              exprasmlist^.concat(new(paicpu,
                op_ref_reg(A_MOV,S_L,r,R_EDI)));

              exprasmlist^.concat(new(paicpu,
                op_reg(A_INC,S_L,R_EDI)));

              if (parraydef(pvarsym(p)^.vartype.def)^.elesize<>1) then
               begin
                 if ispowerof2(parraydef(pvarsym(p)^.vartype.def)^.elesize, power) then
                   exprasmlist^.concat(new(paicpu,
                     op_const_reg(A_SHL,S_L,
                       power,R_EDI)))
                 else
                   exprasmlist^.concat(new(paicpu,
                     op_const_reg(A_IMUL,S_L,
                     parraydef(pvarsym(p)^.vartype.def)^.elesize,R_EDI)));
               end;
{$ifndef NOTARGETWIN32}
              { windows guards only a few pages for stack growing, }
              { so we have to access every page first              }
              if target_os.id=os_i386_win32 then
                begin
                   getlabel(again);
                   getlabel(ok);
                   emitlab(again);
                   exprasmlist^.concat(new(paicpu,
                     op_const_reg(A_CMP,S_L,winstackpagesize,R_EDI)));
                   emitjmp(C_C,ok);
                   exprasmlist^.concat(new(paicpu,
                     op_const_reg(A_SUB,S_L,winstackpagesize-4,R_ESP)));
                   exprasmlist^.concat(new(paicpu,
                     op_reg(A_PUSH,S_L,R_EAX)));
                   exprasmlist^.concat(new(paicpu,
                     op_const_reg(A_SUB,S_L,winstackpagesize,R_EDI)));
                   emitjmp(C_None,again);

                   emitlab(ok);
                   exprasmlist^.concat(new(paicpu,
                     op_reg_reg(A_SUB,S_L,R_EDI,R_ESP)));
{$ifndef noAllocEdi}
                   ungetregister32(R_EDI);
{$endif noAllocEdi}
                   { now reload EDI }
                   new(r);
                   reset_reference(r^);
                   r^.base:=procinfo^.framepointer;
                   r^.offset:=highaddress;
{$ifndef noAllocEdi}
                   getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                   exprasmlist^.concat(new(paicpu,
                     op_ref_reg(A_MOV,S_L,r,R_EDI)));

                   exprasmlist^.concat(new(paicpu,
                     op_reg(A_INC,S_L,R_EDI)));

                   if (parraydef(pvarsym(p)^.vartype.def)^.elesize<>1) then
                    begin
                      if ispowerof2(parraydef(pvarsym(p)^.vartype.def)^.elesize, power) then
                        exprasmlist^.concat(new(paicpu,
                          op_const_reg(A_SHL,S_L,
                            power,R_EDI)))
                      else
                        exprasmlist^.concat(new(paicpu,
                          op_const_reg(A_IMUL,S_L,
                          parraydef(pvarsym(p)^.vartype.def)^.elesize,R_EDI)));
                    end;
                end
              else
{$endif NOTARGETWIN32}
                exprasmlist^.concat(new(paicpu,
                  op_reg_reg(A_SUB,S_L,R_EDI,R_ESP)));
              { load destination }
              exprasmlist^.concat(new(paicpu,
                op_reg_reg(A_MOV,S_L,R_ESP,R_EDI)));

              { don't destroy the registers! }
              exprasmlist^.concat(new(paicpu,
                op_reg(A_PUSH,S_L,R_ECX)));
              exprasmlist^.concat(new(paicpu,
                op_reg(A_PUSH,S_L,R_ESI)));

              { load count }
              new(r);
              reset_reference(r^);
              r^.base:=procinfo^.framepointer;
              r^.offset:=highaddress;
              exprasmlist^.concat(new(paicpu,
                op_ref_reg(A_MOV,S_L,r,R_ECX)));

              { load source }
              new(r);
              reset_reference(r^);
              r^.base:=procinfo^.framepointer;
              r^.offset:=pvarsym(p)^.address+procinfo^.para_offset;
              exprasmlist^.concat(new(paicpu,
                op_ref_reg(A_MOV,S_L,r,R_ESI)));

              { scheduled .... }
              exprasmlist^.concat(new(paicpu,
                op_reg(A_INC,S_L,R_ECX)));

              { calculate size }
              len:=parraydef(pvarsym(p)^.vartype.def)^.elesize;
              opsize:=S_B;
              if (len and 3)=0 then
               begin
                 opsize:=S_L;
                 len:=len shr 2;
               end
              else
               if (len and 1)=0 then
                begin
                  opsize:=S_W;
                  len:=len shr 1;
                end;

              if ispowerof2(len, power) then
                exprasmlist^.concat(new(paicpu,
                  op_const_reg(A_SHL,S_L,
                    power,R_ECX)))
              else
                exprasmlist^.concat(new(paicpu,
                op_const_reg(A_IMUL,S_L,len,R_ECX)));
              exprasmlist^.concat(new(paicpu,
                op_none(A_REP,S_NO)));
              case opsize of
                S_B : exprasmlist^.concat(new(paicpu,op_none(A_MOVSB,S_NO)));
                S_W : exprasmlist^.concat(new(paicpu,op_none(A_MOVSW,S_NO)));
                S_L : exprasmlist^.concat(new(paicpu,op_none(A_MOVSD,S_NO)));
              end;
{$ifndef noAllocEdi}
              ungetregister32(R_EDI);
{$endif noAllocEdi}
              exprasmlist^.concat(new(paicpu,
                op_reg(A_POP,S_L,R_ESI)));
              exprasmlist^.concat(new(paicpu,
                op_reg(A_POP,S_L,R_ECX)));

              { patch the new address }
              new(r);
              reset_reference(r^);
              r^.base:=procinfo^.framepointer;
              r^.offset:=pvarsym(p)^.address+procinfo^.para_offset;
              exprasmlist^.concat(new(paicpu,
                op_reg_ref(A_MOV,S_L,R_ESP,r)));
           end
          else
           if is_shortstring(pvarsym(p)^.vartype.def) then
            begin
              reset_reference(href1);
              href1.base:=procinfo^.framepointer;
              href1.offset:=pvarsym(p)^.address+procinfo^.para_offset;
              reset_reference(href2);
              href2.base:=procinfo^.framepointer;
              href2.offset:=-pvarsym(p)^.localvarsym^.address+pvarsym(p)^.localvarsym^.owner^.address_fixup;
              copyshortstring(href2,href1,pstringdef(pvarsym(p)^.vartype.def)^.len,true,false);
            end
           else
            begin
              reset_reference(href1);
              href1.base:=procinfo^.framepointer;
              href1.offset:=pvarsym(p)^.address+procinfo^.para_offset;
              reset_reference(href2);
              href2.base:=procinfo^.framepointer;
              href2.offset:=-pvarsym(p)^.localvarsym^.address+pvarsym(p)^.localvarsym^.owner^.address_fixup;
              concatcopy(href1,href2,pvarsym(p)^.vartype.def^.size,true,true);
            end;
        end;
    end;

  procedure inittempansistrings;

    var
       hp : ptemprecord;
       r : preference;

    begin
       hp:=templist;
       while assigned(hp) do
         begin
           if hp^.temptype in [tt_ansistring,tt_freeansistring] then
            begin
              procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
              new(r);
              reset_reference(r^);
              r^.base:=procinfo^.framepointer;
              r^.offset:=hp^.pos;
              emit_const_ref(A_MOV,S_L,0,r);
            end;
            hp:=hp^.next;
         end;
   end;

  procedure finalizetempansistrings;

    var
       hp : ptemprecord;
       hr : treference;
    begin
       hp:=templist;
       while assigned(hp) do
         begin
            if hp^.temptype in [tt_ansistring,tt_freeansistring] then
              begin
                 procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
                 reset_reference(hr);
                 hr.base:=procinfo^.framepointer;
                 hr.offset:=hp^.pos;
                 emitpushreferenceaddr(hr);
                 emitcall('FPC_ANSISTR_DECR_REF');
              end;
            hp:=hp^.next;
         end;
   end;

  var
     ls : longint;

  procedure largest_size(p : pnamedindexobject);{$ifndef FPC}far;{$endif}

    begin
       if (psym(p)^.typ=varsym) and
         (pvarsym(p)^.getvaluesize>ls) then
         ls:=pvarsym(p)^.getvaluesize;
    end;

  procedure alignstack(alist : paasmoutput);

    begin
{$ifdef dummy}
       if (cs_optimize in aktglobalswitches) and
         (aktoptprocessor in [classp5,classp6]) then
         begin
            ls:=0;
            aktprocsym^.definition^.localst^.foreach({$ifndef TP}@{$endif}largest_size);
            if ls>=8 then
              alist^.insert(new(paicpu,op_const_reg(A_AND,S_L,-8,R_ESP)));
         end;
{$endif dummy}
    end;

  procedure genentrycode(alist : paasmoutput;const proc_names:Tstringcontainer;make_global:boolean;
                         stackframe:longint;
                         var parasize:longint;var nostackframe:boolean;
                         inlined : boolean;
                         var savedstackoffset : longint);
  {
    Generates the entry code for a procedure
  }
    var
      hs : string;
{$ifdef GDB}
      stab_function_name : Pai_stab_function_name;
{$endif GDB}
      hr : preference;
      p : psymtable;
      r : treference;
      oldlist,
      oldexprasmlist : paasmoutput;
      again : pasmlabel;
      i : longint;

    begin
       oldexprasmlist:=exprasmlist;
       exprasmlist:=alist;
       if (not inlined) and (aktprocsym^.definition^.proctypeoption=potype_proginit) then
           begin
              emitinsertcall('FPC_INITIALIZEUNITS');
              if (target_info.target=target_I386_WIN32) and
                 (cs_profile in aktmoduleswitches) then
                begin
                  emitinsertcall('_monstartup');
                  exprasmlist^.insert(new(paicpu,op_sym(A_PUSH,S_L,newasmsymbol('__image_base__'))));
                  exprasmlist^.insert(new(paicpu,op_sym(A_PUSH,S_L,newasmsymbol('etext'))));
                end;
              if target_info.target=target_I386_WIN32 then
                begin
                   new(hr);
                   reset_reference(hr^);
                   hr^.symbol:=newasmsymbol('U_SYSWIN32_ISCONSOLE');
                   if apptype=at_cui then
                     exprasmlist^.insert(new(paicpu,op_const_ref(A_MOV,S_B,
                       1,hr)))
                   else
                     exprasmlist^.insert(new(paicpu,op_const_ref(A_MOV,S_B,
                       0,hr)));
                end;

              oldlist:=exprasmlist;
              exprasmlist:=new(paasmoutput,init);
              p:=symtablestack;
              while assigned(p) do
                begin
                   p^.foreach({$ifndef TP}@{$endif}initialize_threadvar);
                   p:=p^.next;
                end;
              oldlist^.insertlist(exprasmlist);
              dispose(exprasmlist,done);
              exprasmlist:=oldlist;
           end;

{$ifdef GDB}
      if (not inlined) and (cs_debuginfo in aktmoduleswitches) then
        exprasmlist^.insert(new(pai_force_line,init));
{$endif GDB}

      { a constructor needs a help procedure }
      if (aktprocsym^.definition^.proctypeoption=potype_constructor) then
        begin
          if procinfo^._class^.is_class then
            begin
              procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
              exprasmlist^.insert(new(paicpu,op_cond_sym(A_Jcc,C_Z,S_NO,faillabel)));
              emitinsertcall('FPC_NEW_CLASS');
            end
          else
            begin
              exprasmlist^.insert(new(paicpu,op_cond_sym(A_Jcc,C_Z,S_NO,faillabel)));
              emitinsertcall('FPC_HELP_CONSTRUCTOR');
{$ifndef noAllocEdi}
              getexplicitregister32(R_EDI);
{$endif noAllocEdi}
              exprasmlist^.insert(new(paicpu,op_const_reg(A_MOV,S_L,procinfo^._class^.vmt_offset,R_EDI)));
            end;
        end;

      { don't load ESI, does the caller }
      { we must do it for local function }
      { that can be called from a foreach }
      { of another object than self !! PM }

         if assigned(procinfo^._class) and
            (lexlevel>normal_function_level) then
           maybe_loadself;

      { When message method contains self as a parameter,
        we must load it into ESI }
      If (po_containsself in aktprocsym^.definition^.procoptions) then
        begin
           new(hr);
           reset_reference(hr^);
           hr^.offset:=procinfo^.selfpointer_offset;
           hr^.base:=procinfo^.framepointer;
           exprasmlist^.insert(new(paicpu,op_ref_reg(A_MOV,S_L,hr,R_ESI)));
{$ifndef noAllocEdi}
           exprasmlist^.insert(new(pairegalloc,alloc(R_ESI)));
{$endif noAllocEdi}
        end;
      if not inlined and (procinfo^.framepointer<>R_ESP) and
         ((po_savestdregs in aktprocsym^.definition^.procoptions) or
         (po_saveregisters in aktprocsym^.definition^.procoptions)) then
        begin
          inc(stackframe,4);
          savedstackoffset:=stackframe;
          new(hr);
          reset_reference(hr^);
          hr^.offset:=-savedstackoffset;
          hr^.base:=procinfo^.framepointer;
          exprasmlist^.insert(new(paicpu,op_reg_ref(A_MOV,S_L,R_ESP,hr)));
        end
      else
        savedstackoffset:=-1;
      { should we save edi,esi,ebx like C ? }
      if (po_savestdregs in aktprocsym^.definition^.procoptions) then
       begin
         if (aktprocsym^.definition^.usedregisters and ($80 shr byte(R_EBX)))<>0 then
           exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_L,R_EBX)));
         exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_L,R_ESI)));
         exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_L,R_EDI)));
       end;

      { for the save all registers we can simply use a pusha,popa which
        push edi,esi,ebp,esp(ignored),ebx,edx,ecx,eax }
      if (po_saveregisters in aktprocsym^.definition^.procoptions) then
        begin
          exprasmlist^.insert(new(paicpu,op_none(A_PUSHA,S_L)));
        end;

      { omit stack frame ? }
      if not inlined then
      if procinfo^.framepointer=stack_pointer then
          begin
              CGMessage(cg_d_stackframe_omited);
              nostackframe:=true;
              if (aktprocsym^.definition^.proctypeoption in [potype_unitinit,potype_proginit,potype_unitfinalize]) then
                parasize:=0
              else
                parasize:=aktprocsym^.definition^.parast^.datasize+procinfo^.para_offset-4;
              if stackframe<>0 then
                exprasmlist^.insert(new(paicpu,
                  op_const_reg(A_SUB,S_L,stackframe,R_ESP)));
          end
      else
          begin
              alignstack(alist);
              if (aktprocsym^.definition^.proctypeoption in [potype_unitinit,potype_proginit,potype_unitfinalize]) then
                parasize:=0
              else
                parasize:=aktprocsym^.definition^.parast^.datasize+procinfo^.para_offset-8;
              nostackframe:=false;
              if stackframe<>0 then
                  begin
                          begin
                            { windows guards only a few pages for stack growing, }
                            { so we have to access every page first              }
                            if (target_os.id=os_i386_win32) and
                              (stackframe>=winstackpagesize) then
                              begin
                                  if stackframe div winstackpagesize<=5 then
                                    begin
{ This was also in reverse order (see below) ! - carl
                                       exprasmlist^.insert(new(paicpu,op_const_reg(A_SUB,S_L,stackframe-4,R_ESP)));
                                       for i:=1 to stackframe div winstackpagesize do
                                         begin
                                            hr:=new_reference(R_ESP,stackframe-i*winstackpagesize);
                                            exprasmlist^.concat(new(paicpu,
                                              op_const_ref(A_MOV,S_L,0,hr)));
                                         end;
                                       exprasmlist^.concat(new(paicpu,
                                         op_reg(A_PUSH,S_L,R_EAX)));
}
                                       exprasmlist^.insert(new(paicpu,
                                         op_reg(A_PUSH,S_L,R_EAX)));


                                       for i:= stackframe div winstackpagesize downto 1 do
                                         begin
                                            hr:=new_reference(R_ESP,stackframe-i*winstackpagesize);
                                            exprasmlist^.insert(new(paicpu,
                                              op_const_ref(A_MOV,S_L,0,hr)));
                                         end;
                                       exprasmlist^.insert(new(paicpu,op_const_reg(A_SUB,S_L,stackframe-4,R_ESP)));


                                    end
                                  else
                                    begin
(*
  This code was completely wrong, no concat here, we must
  use insert - otherwise, we get crashes because the stack
  gets corrupted - carl
                                       getlabel(again);
{$ifndef noAllocEdi}
                                       getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                       exprasmlist^.concat(new(paicpu,
                                         op_const_reg(A_MOV,S_L,stackframe div winstackpagesize,R_EDI)));
                                       emitlab(again);
                                       exprasmlist^.concat(new(paicpu,
                                         op_const_reg(A_SUB,S_L,winstackpagesize-4,R_ESP)));
                                       exprasmlist^.concat(new(paicpu,
                                         op_reg(A_PUSH,S_L,R_EAX)));
                                       exprasmlist^.concat(new(paicpu,
                                         op_reg(A_DEC,S_L,R_EDI)));
                                       emitjmp(C_NZ,again);
{$ifndef noAllocEdi}
                                       ungetregister32(R_EDI);
{$endif noAllocEdi}
                                       exprasmlist^.concat(new(paicpu,
                                         op_const_reg(A_SUB,S_L,stackframe mod winstackpagesize,R_ESP)));
*)
                                       getlabel(again);

{                                       getexplicitregister32(R_EDI);}


                                       exprasmlist^.insert(new(paicpu,
                                         op_const_reg(A_SUB,S_L,stackframe mod winstackpagesize,R_ESP)));

                                       exprasmlist^.insert(new(paicpu,op_cond_sym(A_Jcc,C_NZ,S_NO,again)));

                                       exprasmlist^.insert(new(paicpu,
                                         op_reg(A_DEC,S_L,R_EDI)));

                                       exprasmlist^.insert(new(paicpu,
                                         op_reg(A_PUSH,S_L,R_EAX)));

                                       exprasmlist^.insert(new(paicpu,
                                         op_const_reg(A_SUB,S_L,winstackpagesize-4,R_ESP)));

                                       exprasmlist^.insert(new(pai_label,init(again)));

                                       exprasmlist^.insert(new(paicpu,
                                         op_const_reg(A_MOV,S_L,stackframe div winstackpagesize,R_EDI)));


{                                       ungetregister32(R_EDI);}
                                    end
                              end
                            else
                              exprasmlist^.insert(new(paicpu,op_const_reg(A_SUB,S_L,stackframe,R_ESP)));
                            if (cs_check_stack in aktlocalswitches) and
                              not(target_info.target in [target_i386_freebsd,target_i386_netbsd,
                                                         target_i386_linux,target_i386_win32]) and
                               (aktprocsym^.definition^.proctypeoption<>potype_proginit) then
                              begin
                                 emitinsertcall('FPC_STACKCHECK');
                                 exprasmlist^.insert(new(paicpu,op_const(A_PUSH,S_L,stackframe)));
                              end;
                            if cs_profile in aktmoduleswitches then
                              genprofilecode;
                            exprasmlist^.insert(new(paicpu,op_reg_reg(A_MOV,S_L,R_ESP,R_EBP)));
                            exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_L,R_EBP)));
                          end;
                  end { endif stackframe <> 0 }
              else
                 begin
                   if cs_profile in aktmoduleswitches then
                     genprofilecode;
                   exprasmlist^.insert(new(paicpu,op_reg_reg(A_MOV,S_L,R_ESP,R_EBP)));
                   exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_L,R_EBP)));
                 end;
          end;

      if (po_interrupt in aktprocsym^.definition^.procoptions) then
          generate_interrupt_stackframe_entry;

      { initialize return value }
      if (procinfo^.returntype.def<>pdef(voiddef)) and
        (procinfo^.returntype.def^.needs_inittable) and
        ((procinfo^.returntype.def^.deftype<>objectdef) or
        not(pobjectdef(procinfo^.returntype.def)^.is_class)) then
        begin
           procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
           reset_reference(r);
           r.offset:=procinfo^.return_offset;
           r.base:=procinfo^.framepointer;
           initialize(procinfo^.returntype.def,r,
             ret_in_param(procinfo^.returntype.def,procinfo^.def^.proccalloptions));
        end;

      { initialisize local data like ansistrings }
      case aktprocsym^.definition^.proctypeoption of
         potype_unitinit:
           begin
              { using current_module^.globalsymtable is hopefully      }
              { more robust than symtablestack and symtablestack^.next }
              psymtable(current_module^.globalsymtable)^.foreach({$ifndef TP}@{$endif}initialize_data);
              psymtable(current_module^.localsymtable)^.foreach({$ifndef TP}@{$endif}initialize_data);
           end;
         { units have seperate code for initilization and finalization }
         potype_unitfinalize: ;
         else
           aktprocsym^.definition^.localst^.foreach({$ifndef TP}@{$endif}initialize_data);
      end;

      { generate copies of call by value parameters }
      if not(po_assembler in aktprocsym^.definition^.procoptions) and
         not (pocall_cdecl in aktprocsym^.definition^.proccalloptions) then
        aktprocsym^.definition^.parast^.foreach({$ifndef TP}@{$endif}copyvalueparas);

      { add a reference to all call by value/const parameters }
      aktprocsym^.definition^.parast^.foreach({$ifndef TP}@{$endif}incr_data);

      { initialisizes temp. ansi/wide string data }
      inittempansistrings;

      { do we need an exception frame because of ansi/widestrings ? }
      if not inlined and
         ((procinfo^.flags and pi_needs_implicit_finally)<>0) and
      { but it's useless in init/final code of units }
        not(aktprocsym^.definition^.proctypeoption in [potype_unitfinalize,potype_unitinit]) then
        begin
            usedinproc:=usedinproc or ($80 shr byte(R_EAX));

            { Type of stack-frame must be pushed}
            exprasmlist^.concat(new(paicpu,op_const(A_PUSH,S_L,1)));
            emitcall('FPC_PUSHEXCEPTADDR');
            exprasmlist^.concat(new(paicpu,
              op_reg(A_PUSH,S_L,R_EAX)));
            emitcall('FPC_SETJMP');
            exprasmlist^.concat(new(paicpu,
              op_reg(A_PUSH,S_L,R_EAX)));
            exprasmlist^.concat(new(paicpu,
              op_reg_reg(A_TEST,S_L,R_EAX,R_EAX)));
            emitjmp(C_NE,aktexitlabel);
            { probably we've to reload self here }
            maybe_loadself;
        end;

      if not inlined then
       begin
         if (cs_profile in aktmoduleswitches) or
            (aktprocsym^.definition^.owner^.symtabletype=globalsymtable) or
            (assigned(procinfo^._class) and (procinfo^._class^.owner^.symtabletype=globalsymtable)) then
              make_global:=true;

         hs:=proc_names.get;

{$ifdef GDB}
         if (cs_debuginfo in aktmoduleswitches) and target_os.use_function_relative_addresses then
           stab_function_name := new(pai_stab_function_name,init(strpnew(hs)));
{$EndIf GDB}

         while hs<>'' do
          begin
            if make_global then
              exprasmlist^.insert(new(pai_symbol,initname_global(hs,0)))
            else
              exprasmlist^.insert(new(pai_symbol,initname(hs,0)));

{$ifdef GDB}
            if (cs_debuginfo in aktmoduleswitches) and
               target_os.use_function_relative_addresses then
              exprasmlist^.insert(new(pai_stab_function_name,init(strpnew(hs))));
{$endif GDB}

            hs:=proc_names.get;
          end;

         if make_global or ((procinfo^.flags and pi_is_global) <> 0) then
          aktprocsym^.is_global := True;

{$ifdef GDB}
         if (cs_debuginfo in aktmoduleswitches) then
          begin
            if target_os.use_function_relative_addresses then
             exprasmlist^.insert(stab_function_name);
            exprasmlist^.insert(new(pai_stabs,init(aktprocsym^.stabstring)));
            aktprocsym^.isstabwritten:=true;
          end;
{$endif GDB}

       { Align, gprof uses 16 byte granularity }
         if (cs_profile in aktmoduleswitches) then
          exprasmlist^.insert(new(pai_align,init_op(16,$90)))
         else
          if not(cs_littlesize in aktglobalswitches) then
           exprasmlist^.insert(new(pai_align,init(16)));
       end;
      exprasmlist:=oldexprasmlist;
  end;


  procedure handle_return_value(inlined : boolean;var uses_eax,uses_edx : boolean);
    var
       hr : preference;
       op : Tasmop;
       s : Topsize;
  begin
      if procinfo^.returntype.def<>pdef(voiddef) then
          begin
              {if ((procinfo^.flags and pi_operator)<>0) and
                 assigned(opsym) then
                procinfo^.funcret_is_valid:=
                  procinfo^.funcret_is_valid or (opsym^.refs>0);}
              if (procinfo^.funcret_state<>vs_assigned) and not inlined { and
                ((procinfo^.flags and pi_uses_asm)=0)} then
               CGMessage(sym_w_function_result_not_set);
              hr:=new_reference(procinfo^.framepointer,procinfo^.return_offset);
              if (procinfo^.returntype.def^.deftype in [orddef,enumdef]) then
                begin
                  uses_eax:=true;
                  case procinfo^.returntype.def^.size of
                   8:
                     begin
                        emit_ref_reg(A_MOV,S_L,hr,R_EAX);
                        hr:=new_reference(procinfo^.framepointer,procinfo^.return_offset+4);
                        emit_ref_reg(A_MOV,S_L,hr,R_EDX);
                        uses_edx:=true;
                     end;

                   4:
                     emit_ref_reg(A_MOV,S_L,hr,R_EAX);

                   2:
                     emit_ref_reg(A_MOV,S_W,hr,R_AX);

                   1:
                     emit_ref_reg(A_MOV,S_B,hr,R_AL);
                  end;
                end
              else
                if ret_in_acc(procinfo^.returntype.def,procinfo^.def^.proccalloptions) then
                  begin
                    uses_eax:=true;
                    emit_ref_reg(A_MOV,S_L,hr,R_EAX);
                    { this can happen just for win32 target }
                    if (procinfo^.returntype.def^.size>4) then
                      begin
                        hr:=new_reference(procinfo^.framepointer,procinfo^.return_offset+4);
                        emit_ref_reg(A_MOV,S_L,hr,R_EDX);
                        uses_edx:=true;
                      end;
                  end
              else
                 if (procinfo^.returntype.def^.deftype=floatdef) then
                   begin
                      floatloadops(pfloatdef(procinfo^.returntype.def)^.typ,op,s);
                      exprasmlist^.concat(new(paicpu,op_ref(op,s,hr)))
                   end
              else
                dispose(hr);
          end
  end;


  procedure handle_fast_exit_return_value;
    var
       hr : preference;
       op : Tasmop;
       s : Topsize;
  begin
      if procinfo^.returntype.def<>pdef(voiddef) then
          begin
              hr:=new_reference(procinfo^.framepointer,procinfo^.return_offset);
              if (procinfo^.returntype.def^.deftype in [orddef,enumdef]) then
                begin
                  case procinfo^.returntype.def^.size of
                   8:
                     begin
                        emit_reg_ref(A_MOV,S_L,R_EAX,hr);
                        hr:=new_reference(procinfo^.framepointer,procinfo^.return_offset+4);
                        emit_reg_ref(A_MOV,S_L,R_EDX,hr);
                     end;

                   4:
                     emit_reg_ref(A_MOV,S_L,R_EAX,hr);

                   2:
                     emit_reg_ref(A_MOV,S_W,R_AX,hr);

                   1:
                     emit_reg_ref(A_MOV,S_B,R_AL,hr);
                  end;
                end
              else
                if ret_in_acc(procinfo^.returntype.def,procinfo^.def^.proccalloptions) then
                  begin
                    emit_reg_ref(A_MOV,S_L,R_EAX,hr);
                  end
              else
                 if (procinfo^.returntype.def^.deftype=floatdef) then
                   begin
                      floatstoreops(pfloatdef(procinfo^.returntype.def)^.typ,op,s);
                      exprasmlist^.concat(new(paicpu,op_ref(op,s,hr)))
                   end
              else
                dispose(hr);
          end
  end;


  procedure genexitcode(alist : paasmoutput;parasize:longint;nostackframe,inlined:boolean;savedstackoffset : longint);

    var
{$ifdef GDB}
       mangled_length : longint;
       p : pchar;
       st : string[2];
{$endif GDB}
       stabsendlabel,nofinal,okexitlabel,
       noreraiselabel,nodestroycall : pasmlabel;
       hr : treference;
       uses_eax,uses_edx,uses_esi : boolean;
       oldexprasmlist : paasmoutput;
       ai : paicpu;
       pd : pprocdef;

  begin
      oldexprasmlist:=exprasmlist;
      exprasmlist:=alist;

      if aktexit2label^.is_used and
         ((procinfo^.flags and (pi_needs_implicit_finally or pi_uses_exceptions)) <> 0) then
        begin
          exprasmlist^.concat(new(paicpu,op_sym(A_JMP,S_NO,aktexitlabel)));
          exprasmlist^.concat(new(pai_label,init(aktexit2label)));
          handle_fast_exit_return_value;
        end;

      if aktexitlabel^.is_used then
        exprasmlist^.concat(new(pai_label,init(aktexitlabel)));

      { call the destructor help procedure }
      if (aktprocsym^.definition^.proctypeoption=potype_destructor) and
         assigned(procinfo^._class) then
        begin
          if procinfo^._class^.is_class then
            begin
              emitcall('FPC_DISPOSE_CLASS');
            end
          else
            begin
{$ifndef noAllocEdi}
              getexplicitregister32(R_EDI);
{$endif noAllocEdi}
              { must the object be finalized ? }
              if procinfo^._class^.needs_inittable then
                begin
                   reset_reference(hr);
                   hr.base:=R_EBP;
                   hr.offset:=8;
                   exprasmlist^.concat(new(paicpu,op_const_ref(A_CMP,S_L,0,newreference(hr))));
                   getlabel(nofinal);
                   ai:=new(paicpu,op_sym(A_Jcc,S_NO,nofinal));
                   ai^.SetCondition(C_Z);
                   exprasmlist^.concat(ai);
                   exprasmlist^.concat(new(paicpu,op_sym(A_PUSH,S_L,procinfo^._class^.get_inittable_label)));
                   exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,R_ESI)));
                   emitcall('FPC_FINALIZE');
                   exprasmlist^.concat(new(pai_label,init(nofinal)));
                end;
              exprasmlist^.concat(new(paicpu,op_const_reg(A_MOV,S_L,procinfo^._class^.vmt_offset,R_EDI)));
              emitcall('FPC_HELP_DESTRUCTOR');
{$ifndef noAllocEdi}
              ungetregister32(R_EDI);
{$endif noAllocEdi}
            end;
        end;

      { finalize temporary data }
      finalizetempansistrings;

      { finalize local data like ansistrings}
      case aktprocsym^.definition^.proctypeoption of
         potype_unitfinalize:
           begin
              { using current_module^.globalsymtable is hopefully      }
              { more robust than symtablestack and symtablestack^.next }
              psymtable(current_module^.globalsymtable)^.foreach({$ifndef TP}@{$endif}finalize_data);
              psymtable(current_module^.localsymtable)^.foreach({$ifndef TP}@{$endif}finalize_data);
           end;
         { units have seperate code for initialization and finalization }
         potype_unitinit: ;
         else
           aktprocsym^.definition^.localst^.foreach({$ifndef TP}@{$endif}finalize_data);
      end;

      { finalize paras data }
      if assigned(aktprocsym^.definition^.parast) then
        aktprocsym^.definition^.parast^.foreach({$ifndef TP}@{$endif}finalize_data);

      { do we need to handle exceptions because of ansi/widestrings ? }
      if not inlined and
         ((procinfo^.flags and pi_needs_implicit_finally)<>0) and
      { but it's useless in init/final code of units }
        not(aktprocsym^.definition^.proctypeoption in [potype_unitfinalize,potype_unitinit]) then
        begin
           { the exception helper routines modify all registers }
           aktprocsym^.definition^.usedregisters:=$ff;

           getlabel(noreraiselabel);
           emitcall('FPC_POPADDRSTACK');
           exprasmlist^.concat(new(paicpu,
             op_reg(A_POP,S_L,R_EAX)));
           exprasmlist^.concat(new(paicpu,
             op_reg_reg(A_TEST,S_L,R_EAX,R_EAX)));
           emitjmp(C_E,noreraiselabel);
           if (aktprocsym^.definition^.proctypeoption=potype_constructor) then
             begin
                if assigned(procinfo^._class) then
                  begin
                     pd:=procinfo^._class^.searchdestructor;
                     if assigned(pd) then
                       begin
                          getlabel(nodestroycall);
                          emit_const_ref(A_CMP,S_L,0,new_reference(procinfo^.framepointer,
                            procinfo^.selfpointer_offset+4));
                          emitjmp(C_E,nodestroycall);
                          if procinfo^._class^.is_class then
                            begin
                               emit_const(A_PUSH,S_L,1);
                               emit_reg(A_PUSH,S_L,R_ESI);
                            end
                          else
                            begin
                               emit_reg(A_PUSH,S_L,R_ESI);
                               emit_sym(A_PUSH,S_L,newasmsymbol(procinfo^._class^.vmt_mangledname));
                            end;
                          if (po_virtualmethod in pd^.procoptions) then
                            begin
                               emit_ref_reg(A_MOV,S_L,new_reference(R_ESI,0),R_EDI);
                               emit_ref(A_CALL,S_NO,new_reference(R_EDI,procinfo^._class^.vmtmethodoffset(pd^.extnumber)));
                            end
                          else
                            emitcall(pd^.mangledname);
                          { not necessary because the result is never assigned in the
                            case of an exception (FK)
                          emit_const_reg(A_MOV,S_L,0,R_ESI);
                          emit_const_ref(A_MOV,S_L,0,new_reference(procinfo^.framepointer,8));
                          }
                          emitlab(nodestroycall);
                       end;
                  end
             end
           else
           { must be the return value finalized before reraising the exception? }
           if (procinfo^.returntype.def<>pdef(voiddef)) and
             (procinfo^.returntype.def^.needs_inittable) and
             ((procinfo^.returntype.def^.deftype<>objectdef) or
             not(pobjectdef(procinfo^.returntype.def)^.is_class)) then
             begin
                reset_reference(hr);
                hr.offset:=procinfo^.return_offset;
                hr.base:=procinfo^.framepointer;
                finalize(procinfo^.returntype.def,hr,
                  ret_in_param(procinfo^.returntype.def,procinfo^.def^.proccalloptions));
             end;

           emitcall('FPC_RERAISE');
           emitlab(noreraiselabel);
        end;

      { call __EXIT for main program }
      if (not DLLsource) and (not inlined) and (aktprocsym^.definition^.proctypeoption=potype_proginit) then
       begin
         {if (target_info.target=target_I386_WIN32) and
            (cs_profile in aktmoduleswitches) then
           emitinsertcall('__mcleanup');   }
         emitcall('FPC_DO_EXIT');
       end;

      { handle return value }
      uses_eax:=false;
      uses_edx:=false;
      uses_esi:=false;
      if not(po_assembler in aktprocsym^.definition^.procoptions) then
          if (aktprocsym^.definition^.proctypeoption<>potype_constructor) then
            handle_return_value(inlined,uses_eax,uses_edx)
          else
              begin
                  { successful constructor deletes the zero flag }
                  { and returns self in eax                   }
                  { eax must be set to zero if the allocation failed !!! }
                  getlabel(okexitlabel);
                  emitjmp(C_NONE,okexitlabel);
                  emitlab(faillabel);
                  if procinfo^._class^.is_class then
                    begin
                      emit_ref_reg(A_MOV,S_L,new_reference(procinfo^.framepointer,8),R_ESI);
                      emitcall('FPC_HELP_FAIL_CLASS');
                    end
                  else
                    begin
                      emit_ref_reg(A_MOV,S_L,new_reference(procinfo^.framepointer,12),R_ESI);
{$ifndef noAllocEdi}
                       getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                      emit_const_reg(A_MOV,S_L,procinfo^._class^.vmt_offset,R_EDI);
                      emitcall('FPC_HELP_FAIL');
{$ifndef noAllocEdi}
                      ungetregister32(R_EDI);
{$endif noAllocEdi}
                    end;
                  emitlab(okexitlabel);

                  emit_reg_reg(A_MOV,S_L,R_ESI,R_EAX);
                  emit_reg_reg(A_TEST,S_L,R_ESI,R_ESI);
                  uses_eax:=true;
                  uses_esi:=true;
              end;

      if aktexit2label^.is_used and not aktexit2label^.is_set then
        emitlab(aktexit2label);

      if ((cs_debuginfo in aktmoduleswitches) and not inlined) then
        begin
          getlabel(stabsendlabel);
          emitlab(stabsendlabel);
        end;
      { gives problems for long mangled names }
      {list^.concat(new(pai_symbol,init(aktprocsym^.definition^.mangledname+'_end')));}

      if savedstackoffset<>-1 then
        begin
          reset_reference(hr);
          hr.offset:=-savedstackoffset;
          hr.base:=procinfo^.framepointer;
          emit_ref_reg(A_MOV,S_L,newreference(hr),R_ESP);
        end;
      { for all i386 gcc implementations }
      if (po_savestdregs in aktprocsym^.definition^.procoptions) then
        begin
          if (aktprocsym^.definition^.usedregisters and ($80 shr byte(R_EBX)))<>0 then
           exprasmlist^.concat(new(paicpu,op_reg(A_POP,S_L,R_EBX)));
          exprasmlist^.concat(new(paicpu,op_reg(A_POP,S_L,R_ESI)));
          exprasmlist^.concat(new(paicpu,op_reg(A_POP,S_L,R_EDI)));
          { here we could reset R_EBX
            but that is risky because it only works
            if genexitcode is called after genentrycode
            so lets skip this for the moment PM
          aktprocsym^.definition^.usedregisters:=
            aktprocsym^.definition^.usedregisters or not ($80 shr byte(R_EBX));
          }
        end;

      { for the save all registers we can simply use a pusha,popa which
        push edi,esi,ebp,esp(ignored),ebx,edx,ecx,eax }
      if (po_saveregisters in aktprocsym^.definition^.procoptions) then
        begin
          if uses_esi then
            exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_ESI,new_reference(R_ESP,4))));
          if uses_edx then
            exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDX,new_reference(R_ESP,20))));
          if uses_eax then
            exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EAX,new_reference(R_ESP,28))));
          exprasmlist^.concat(new(paicpu,op_none(A_POPA,S_L)));
          { We add a NOP because of the 386DX CPU bugs with POPAD }
          exprasmlist^.concat(new(paicpu,op_none(A_NOP,S_L)))
        end;
      if not(nostackframe) then
        begin
          if not inlined then
            exprasmlist^.concat(new(paicpu,op_none(A_LEAVE,S_NO)));
        end
      else
        begin
          if (gettempsize<>0) and not inlined then
            exprasmlist^.insert(new(paicpu,
              op_const_reg(A_ADD,S_L,gettempsize,R_ESP)));
        end;

      { parameters are limited to 65535 bytes because }
      { ret allows only imm16                    }
      if (parasize>65535) and not(pocall_clearstack in aktprocsym^.definition^.proccalloptions) then
       CGMessage(cg_e_parasize_too_big);

      { at last, the return is generated }

      if not inlined then
      if (po_interrupt in aktprocsym^.definition^.procoptions) then
          begin
             if uses_esi then
               exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_ESI,new_reference(R_ESP,16))));
             if uses_edx then
               exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDX,new_reference(R_ESP,12))));
             if uses_eax then
               exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EAX,new_reference(R_ESP,0))));
             generate_interrupt_stackframe_exit;
          end
      else
       begin
       {Routines with the poclearstack flag set use only a ret.}
       { also routines with parasize=0     }
         if (pocall_clearstack in aktprocsym^.definition^.proccalloptions) then
           begin
{$ifndef OLD_C_STACK}
             { complex return values are removed from stack in C code PM }
             if ret_in_param(aktprocsym^.definition^.rettype.def,
                  procinfo^.def^.proccalloptions) then
               exprasmlist^.concat(new(paicpu,op_const(A_RET,S_NO,4)))
             else
{$endif not OLD_C_STACK}
               exprasmlist^.concat(new(paicpu,op_none(A_RET,S_NO)));
           end
         else if (parasize=0) then
          exprasmlist^.concat(new(paicpu,op_none(A_RET,S_NO)))
         else
          exprasmlist^.concat(new(paicpu,op_const(A_RET,S_NO,parasize)));
       end;

      if not inlined then
        exprasmlist^.concat(new(pai_symbol_end,initname(aktprocsym^.definition^.mangledname)));

{$ifdef GDB}
      if (cs_debuginfo in aktmoduleswitches) and not inlined  then
          begin
              aktprocsym^.concatstabto(exprasmlist);
              if assigned(procinfo^._class) then
                if (not assigned(procinfo^.parent) or
                   not assigned(procinfo^.parent^._class)) then
                  begin
                    if (po_classmethod in aktprocsym^.definition^.procoptions) or
                       (po_staticmethod in aktprocsym^.definition^.procoptions) then
                      begin
                        exprasmlist^.concat(new(pai_stabs,init(strpnew(
                         '"pvmt:p'+pvmtdef^.numberstring+'",'+
                         tostr(N_PSYM)+',0,0,'+tostr(procinfo^.selfpointer_offset)))));
                      end
                    else
                      begin
                        if not  procinfo^._class^.is_class then
                          st:='v'
                        else
                          st:='p';
                        exprasmlist^.concat(new(pai_stabs,init(strpnew(
                         '"$t:'+st+procinfo^._class^.numberstring+'",'+
                         tostr(N_PSYM)+',0,0,'+tostr(procinfo^.selfpointer_offset)))));
                      end;
                  end
                else
                  begin
                    if not  procinfo^._class^.is_class then
                      st:='*'
                    else
                      st:='';
                    exprasmlist^.concat(new(pai_stabs,init(strpnew(
                    '"$t:r'+st+procinfo^._class^.numberstring+'",'+
                    tostr(N_RSYM)+',0,0,'+tostr(GDB_i386index[R_ESI])))));
                  end;
              { define calling EBP as pseudo local var PM }
              { this enables test if the function is a local one !! }
              if  assigned(procinfo^.parent) and (lexlevel>normal_function_level) then
                exprasmlist^.concat(new(pai_stabs,init(strpnew(
                 '"parent_ebp:'+voidpointerdef^.numberstring+'",'+
                 tostr(N_LSYM)+',0,0,'+tostr(procinfo^.framepointer_offset)))));

              if (pdef(aktprocsym^.definition^.rettype.def) <> pdef(voiddef)) then
                begin
                  if ret_in_param(aktprocsym^.definition^.rettype.def,
                       procinfo^.def^.proccalloptions) then
                    exprasmlist^.concat(new(pai_stabs,init(strpnew(
                     '"'+aktprocsym^.name+':X*'+aktprocsym^.definition^.rettype.def^.numberstring+'",'+
                     tostr(N_PSYM)+',0,0,'+tostr(procinfo^.return_offset)))))
                  else
                    exprasmlist^.concat(new(pai_stabs,init(strpnew(
                     '"'+aktprocsym^.name+':X'+aktprocsym^.definition^.rettype.def^.numberstring+'",'+
                     tostr(N_PSYM)+',0,0,'+tostr(procinfo^.return_offset)))));
                  if (m_result in aktmodeswitches) then
                    if ret_in_param(aktprocsym^.definition^.rettype.def,
                         procinfo^.def^.proccalloptions) then
                      exprasmlist^.concat(new(pai_stabs,init(strpnew(
                       '"RESULT:X*'+aktprocsym^.definition^.rettype.def^.numberstring+'",'+
                       tostr(N_PSYM)+',0,0,'+tostr(procinfo^.return_offset)))))
                    else
                      exprasmlist^.concat(new(pai_stabs,init(strpnew(
                       '"RESULT:X'+aktprocsym^.definition^.rettype.def^.numberstring+'",'+
                       tostr(N_PSYM)+',0,0,'+tostr(procinfo^.return_offset)))));
                end;
              mangled_length:=length(aktprocsym^.definition^.mangledname);
              getmem(p,2*mangled_length+50);
              strpcopy(p,'192,0,0,');
              strpcopy(strend(p),aktprocsym^.definition^.mangledname);
              if (target_os.use_function_relative_addresses) then
                begin
                  strpcopy(strend(p),'-');
                  strpcopy(strend(p),aktprocsym^.definition^.mangledname);
                end;
              exprasmlist^.concat(new(pai_stabn,init(strnew(p))));
              {list^.concat(new(pai_stabn,init(strpnew('192,0,0,'
               +aktprocsym^.definition^.mangledname))));
              p[0]:='2';p[1]:='2';p[2]:='4';
              strpcopy(strend(p),'_end');}
              strpcopy(p,'224,0,0,'+stabsendlabel^.name);
              if (target_os.use_function_relative_addresses) then
                begin
                  strpcopy(strend(p),'-');
                  strpcopy(strend(p),aktprocsym^.definition^.mangledname);
                end;
              exprasmlist^.concatlist(withdebuglist);
              exprasmlist^.concat(new(pai_stabn,init(
                strnew(p))));
               { strpnew('224,0,0,'
               +aktprocsym^.definition^.mangledname+'_end'))));}
              freemem(p,2*mangled_length+50);
          end;
{$endif GDB}
      exprasmlist:=oldexprasmlist;
  end;

    procedure genimplicitunitfinal(alist : paasmoutput);

      begin
         { using current_module^.globalsymtable is hopefully      }
         { more robust than symtablestack and symtablestack^.next }
         psymtable(current_module^.globalsymtable)^.foreach({$ifndef TP}@{$endif}finalize_data);
         psymtable(current_module^.localsymtable)^.foreach({$ifndef TP}@{$endif}finalize_data);
         exprasmlist^.insert(new(pai_symbol,initname_global('FINALIZE$$'+current_module^.modulename^,0)));
         exprasmlist^.insert(new(pai_symbol,initname_global(target_os.cprefix+current_module^.modulename^+'_finalize',0)));
{$ifdef GDB}
         if (cs_debuginfo in aktmoduleswitches) and
           target_os.use_function_relative_addresses then
           exprasmlist^.insert(new(pai_stab_function_name,init(strpnew('FINALIZE$$'+current_module^.modulename^))));
{$endif GDB}
         exprasmlist^.concat(new(paicpu,op_none(A_RET,S_NO)));
         alist^.concatlist(exprasmlist);
      end;

    procedure genimplicitunitinit(alist : paasmoutput);

      begin
         { using current_module^.globalsymtable is hopefully      }
         { more robust than symtablestack and symtablestack^.next }
         psymtable(current_module^.globalsymtable)^.foreach({$ifndef TP}@{$endif}finalize_data);
         psymtable(current_module^.localsymtable)^.foreach({$ifndef TP}@{$endif}finalize_data);
         exprasmlist^.insert(new(pai_symbol,initname_global('INIT$$'+current_module^.modulename^,0)));
         exprasmlist^.insert(new(pai_symbol,initname_global(target_os.cprefix+current_module^.modulename^+'_init',0)));
{$ifdef GDB}
         if (cs_debuginfo in aktmoduleswitches) and
           target_os.use_function_relative_addresses then
           exprasmlist^.insert(new(pai_stab_function_name,init(strpnew('INIT$$'+current_module^.modulename^))));
{$endif GDB}
         exprasmlist^.concat(new(paicpu,op_none(A_RET,S_NO)));
         alist^.concatlist(exprasmlist);
      end;

{$ifdef test_dest_loc}
       procedure mov_reg_to_dest(p : ptree; s : topsize; reg : tregister);

         begin
            if (dest_loc.loc=LOC_CREGISTER) or (dest_loc.loc=LOC_REGISTER) then
              begin
                emit_reg_reg(A_MOV,s,reg,dest_loc.register);
                set_location(p^.location,dest_loc);
                in_dest_loc:=true;
              end
            else
            if (dest_loc.loc=LOC_REFERENCE) or (dest_loc.loc=LOC_MEM) then
              begin
                exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,s,reg,newreference(dest_loc.reference))));
                set_location(p^.location,dest_loc);
                in_dest_loc:=true;
              end
            else
              internalerror(20080);
         end;

{$endif test_dest_loc}

end.
{
  $Log: cga.pas,v $
  Revision 1.1.2.40  2003/05/06 18:22:23  peter
    * don't call destroy in inherited constructor

  Revision 1.1.2.39  2003/03/24 11:02:10  pierre
   * fix bug 2432, was a problem in concatcopy code

  Revision 1.1.2.38  2003/02/04 22:33:52  marco
   * Some preparation for profiling support of NetBSD and OpenBSD.

  Revision 1.1.2.37  2003/02/03 19:05:14  carl
    + fix tabulation

  Revision 1.1.2.36  2003/02/01 16:30:52  marco
  * Fix for FreeBSD profiling support

  Revision 1.1.2.35  2003/01/14 21:27:42  peter
    * fixed tw2296

  Revision 1.1.2.34  2002/11/15 14:10:11  pierre
    + added a calloptions parameter to ret_inacc and ret_in_param
      functions.
      This allows to handle win32 stdcall and m68k cdecl small records
      case.

  Revision 1.1.2.33  2002/11/07 23:21:49  pierre
   * fix a bug specific to win32

  Revision 1.1.2.32  2002/11/07 16:51:03  pierre
   * several memory leaks removed

  Revision 1.1.2.31  2002/11/04 13:02:49  pierre
   * correct code generation bug for args to C functions pusehd by value for go32v2 target

  Revision 1.1.2.30  2002/10/21 12:22:32  pierre
   * relative position of high index is different for po_leftright

  Revision 1.1.2.29  2002/10/15 12:02:56  pierre
   * fix win32 failures of tbs/tb0321 and tb0324

  Revision 1.1.2.28  2002/10/15 06:12:24  pierre
   * tcalval9 finally fixed, no other change in tests results

  Revision 1.1.2.27  2002/10/14 20:08:14  pierre
   * fix error in previous commit

  Revision 1.1.2.26  2002/10/14 19:43:20  pierre
   * try to fix the incompatibility between register saving and parameter copies

  Revision 1.1.2.25  2002/09/24 19:17:02  carl
     * all register saving directives did not work on Win32 where
       there a big return value! (already correct behavior in main branch)

  Revision 1.1.2.24  2002/09/22 13:49:22  carl
    * stack checking cannot be called before system unit is initialized

  Revision 1.1.2.23  2002/09/20 06:59:05  pierre
   * try to get cygwin profiling to work

  Revision 1.1.2.22  2002/07/16 14:29:45  pierre
   * try to update profile support for win32

  Revision 1.1.2.21  2002/02/08 14:45:43  jonas
    * fixed bug reported by deka@ic.tsu.ru (Vladimir Ravodin): in the range
      check code we use ecx,  but it was only allocated by generating a
      pairegalloc instead of by getexplicitregister32 -> it wasn't added to
      the used registers of the procedure and thus not saved if no other
      code in the procedure used it

  Revision 1.1.2.20  2002/01/21 21:30:38  pierre
   * fix bug 1658

  Revision 1.1.2.19  2002/01/19 14:22:08  peter
    * fixed init/final for value parameters

  Revision 1.1.2.18  2002/01/19 11:45:18  peter
    * call addref for call by value arrays

  Revision 1.1.2.17  2001/10/11 14:36:32  jonas
    * fixed internalerror(10) due to previous fpu overflow fixes
    * NOTE: from now on, the location of a binary node must now always be set
       before you call calcregisters() for it

  Revision 1.1.2.16  2001/09/30 21:09:19  peter
    * int64->boolean support

  Revision 1.1.2.15  2001/09/10 23:05:00  pierre
   * avoid fpu stack overflow

  Revision 1.1.2.14  2001/09/09 08:52:09  jonas
    * fixed bug in load_ansistring (left^.location was released too early,
      caused bug reported by Aleksey V. Vaneev in mailing list on 2001/09/07
      regarding 'problems with nested procedures and local vars'

  Revision 1.1.2.13  2001/08/29 14:55:24  jonas
    * backported int64 related fixes from main branch (for m68k: I only
      tested whether the compiler can still be compiled, you should still
      compile/run tests/test/tint64*.pp to verify whether I didn't break
      anything

  Revision 1.1.2.12  2001/08/12 20:23:03  peter
    * netbsd doesn't use stackchecking

  Revision 1.1.2.11  2001/08/09 11:39:04  pierre
   * not stack check for netbsd

  Revision 1.1.2.10  2001/08/07 15:55:31  pierre
   + new code for NetBSD, behaves like FreeBSD for now

  Revision 1.1.2.9  2001/07/20 11:38:52  pierre
   * cleanup emitloardord2reg function

  Revision 1.1.2.8  2001/04/16 22:10:27  carl
  * oops - popal can precede any opcode.

  Revision 1.1.2.7  2001/04/16 20:34:00  carl
  + added note about 386DX bug and popal

  Revision 1.1.2.6  2001/03/31 09:09:40  pierre
   * fix for bug 1433

  Revision 1.1.2.5  2001/03/22 10:26:34  pierre
   * fix bug discovered by Carl when pushing in64 values inlined

  Revision 1.1.2.4  2001/03/03 12:39:34  jonas
    * fixed a typo in emitrangecheck64

  Revision 1.1.2.3  2001/03/02 02:21:52  carl
  + separated into non cpu and cpu specific files
  + added isaddressregister() to check if pointer register

  Revision 1.1.2.2  2001/02/27 02:14:47  carl
  * rename maybe_loadesi to maybe_loadself

  Revision 1.1.2.1  2001/02/25 03:30:44  carl
  no message

  Revision 1.1.2.1  2001/02/25 02:33:33  carl
  - moved to i386 directory

  Revision 1.1.2.19  2000/12/08 17:03:20  jonas
    + added full range checking for 64bit types
    * fixed web bug 1144

  Revision 1.1.2.18  2000/12/07 17:17:25  jonas
    * fixed bug where the original resulttype wasn't restored correctly
      after doing a 64bit rangecheck

  Revision 1.1.2.17  2000/12/06 16:46:30  jonas
    * backported range checking fixes from 1.1 (added range checking for
      conversion between cardinal and longint and for conversion from
      64bit to 32bit types)

  Revision 1.1.2.16  2000/11/20 16:21:53  pierre
   * class class method parameter is not a oclass but a vmt pointer

  Revision 1.1.2.15  2000/11/17 23:17:07  pierre
   * fix static object method and class class method

  Revision 1.1.2.14  2000/10/24 22:22:22  peter
    * emitcall -> emitinsertcall for profiling

  Revision 1.1.2.13  2000/10/23 20:07:02  pierre
   * fix web bug 1153

  Revision 1.1.2.12  2000/10/14 00:40:45  pierre
   * fixes for class debugging

  Revision 1.1.2.11  2000/10/13 20:12:21  florian
    * fixed my previous commit

  Revision 1.1.2.10  2000/10/13 19:50:42  florian
    * the warning about the unimplemented int64 range check is now displayed only once

  Revision 1.1.2.9  2000/10/10 14:52:38  jonas
    * added missing regallocs for edi in emit_mov_ref_reg64

  Revision 1.1.2.8  2000/09/13 13:57:40  marco
   * FreeBSD compiler support

  Revision 1.1.2.7  2000/08/24 19:05:29  peter
    * don't initialize if localvarsym is set because that varsym will
      already be initialized
    * first initialize local data before copy of value para's

  Revision 1.1.2.6  2000/08/19 20:06:15  peter
    * check size after checking openarray in push_value_para

  Revision 1.1.2.5  2000/08/10 18:44:43  peter
    * fixed for constants in emit_push_mem_size for go32v2

  Revision 1.1.2.4  2000/08/07 11:22:11  jonas
    + emit_push_mem_size() which pushes a value in memory of a certain size
    * pushsetelement() and pushvaluepara() use this new procedure, because
      otherwise they could sometimes try to push data past the end of the
      heap, causing a crash

  Revision 1.1.2.3  2000/08/02 08:01:08  jonas
    * fixed web bug1087
    * allocate R_ECX explicitely if it's used

  Revision 1.1.2.2  2000/08/02 07:54:49  jonas
  *** empty log message ***

  Revision 1.1.2.1  2000/07/27 09:21:33  jonas
    * moved locflags2reg() procedure from cg386add to cgai386
    + added locjump2reg() procedure to cgai386
    * fixed internalerror(2002) when the result of a case expression has
      LOC_JUMP

  Revision 1.1  2000/07/13 06:29:47  michael
  + Initial import

  Revision 1.109  2000/06/27 12:17:29  jonas
    * fix for web bug 1011: no exception stack stuff is generated for
      inlined procedures, the entry/exitcode of the parent will do that

  Revision 1.108  2000/06/10 17:31:42  jonas
    * loadord2reg doesn't generate any "movl %reg1,%reg1" anymore

  Revision 1.107  2000/06/05 20:39:05  pierre
   * fix for inline bug

  Revision 1.106  2000/05/26 20:16:00  jonas
    * fixed wrong register deallocations in several ansistring related
      procedures. The IDE's now function fine when compiled with -OG3p3r

  Revision 1.105  2000/05/23 14:20:49  pierre
   * Use stacksize param instead of gettempsize

  Revision 1.104  2000/05/18 17:05:15  peter
    * fixed size of const parameters in asm readers

  Revision 1.103  2000/05/17 11:06:11  pierre
   add a comment about ENTER and linux

  Revision 1.102  2000/05/14 18:49:04  florian
    + Int64/QWord stuff for array of const added

  Revision 1.101  2000/05/09 14:17:33  pierre
   * handle interrupt function correctly

  Revision 1.100  2000/05/04 09:29:31  pierre
   * saveregisters now does not overwrite registers used as return value for functions

  Revision 1.99  2000/04/28 08:53:47  pierre
   * fix my last fix for other targets then win32

  Revision 1.98  2000/04/26 10:03:45  pierre
    * correct bugs for ts010026 and ts010029 in win32 mode
      in copyvaluparas
    + use SHL instead of IMUL if constant is a power of 2 in copyvalueparas

  Revision 1.97  2000/04/24 12:48:37  peter
    * removed unused vars

  Revision 1.96  2000/04/10 12:23:18  jonas
    * modified copyshortstring so it takes an extra paramter which allows it
      to delete the sref itself (so the reg deallocations are put in the
      right place for the optimizer)

  Revision 1.95  2000/04/10 09:01:15  pierre
   * fix for bug 922 in copyvalueparas

  Revision 1.94  2000/04/03 20:51:22  florian
    * initialize/finalize_data checks if procinfo is assigned else
      crashes happend at end of compiling if there were ansistrings in the
      interface/implementation part of units: it was the result of the fix
      of 701 :(

  Revision 1.93  2000/04/02 10:18:18  florian
    * bug 701 fixed: ansistrings in interface and implementation part of the units
      are now finalized correctly even if there are no explicit initialization/
      finalization statements

  Revision 1.92  2000/04/01 14:18:45  peter
    * use arraydef.elesize instead of elementtype.def.size

  Revision 1.91  2000/03/31 22:56:46  pierre
    * fix the handling of value parameters in cdecl function

  Revision 1.90  2000/03/28 22:31:46  pierre
   * fix for problem in tbs0299 for 4 byte stack alignment

  Revision 1.89  2000/03/21 23:36:46  pierre
   fix for bug 312

  Revision 1.88  2000/03/19 11:55:08  peter
    * fixed temp ansi handling within array constructor

  Revision 1.87  2000/03/19 08:17:36  peter
    * tp7 fix

  Revision 1.86  2000/03/01 15:36:11  florian
    * some new stuff for the new cg

  Revision 1.85  2000/03/01 12:35:44  pierre
   * fix for bug 855

  Revision 1.84  2000/03/01 00:03:12  pierre
    * fixes for locals in inlined procedures
      fix for bug797
    + stabs generation for inlined paras and locals

  Revision 1.83  2000/02/18 21:25:48  florian
    * fixed a bug in int64/qword handling was a quite ugly one

  Revision 1.82  2000/02/18 20:53:14  pierre
    * fixes a stabs problem for functions
    + includes a stabs local var for with statements
      the name is with in lowercase followed by an index
      for nested with.
    + Withdebuglist added because the stabs declarations of local
      var are postponed to end of function.

  Revision 1.81  2000/02/10 23:44:43  florian
    * big update for exception handling code generation: possible mem holes
      fixed, break/continue/exit should work always now as expected

  Revision 1.80  2000/02/09 17:36:10  jonas
    * added missing regalloc for ecx in range check code

  Revision 1.79  2000/02/09 13:22:50  peter
    * log truncated

  Revision 1.78  2000/02/04 21:00:31  florian
    * some (small) problems with register saving fixed

  Revision 1.77  2000/02/04 20:00:21  florian
    * an exception in a construcor calls now the destructor (this applies only
      to classes)

  Revision 1.76  2000/02/04 14:29:57  pierre
   + add pseudo local var parent_ebp for local procs

  Revision 1.75  2000/01/25 08:46:03  pierre
   * Range check for int64 produces a warning only

  Revision 1.74  2000/01/24 12:17:22  florian
    * some improvemenst to cmov support
    * disabled excpetion frame generation in cosntructors temporarily

  Revision 1.73  2000/01/23 21:29:14  florian
    * CMOV support in optimizer (in define USECMOV)
    + start of support of exceptions in constructors

  Revision 1.72  2000/01/23 11:11:36  michael
  + Fixes from Jonas.

  Revision 1.71  2000/01/22 16:02:37  jonas
    * fixed more regalloc bugs (for set adding and unsigned
      multiplication)

  Revision 1.70  2000/01/16 22:17:11  peter
    * renamed call_offset to para_offset

  Revision 1.69  2000/01/12 10:38:17  peter
    * smartlinking fixes for binary writer
    * release alignreg code and moved instruction writing align to cpuasm,
      but it doesn't use the specified register yet

  Revision 1.68  2000/01/09 12:35:02  jonas
    * changed edi allocation to use getexplicitregister32/ungetregister
      (adapted tgeni386 a bit for this) and enabled it by default
    * fixed very big and stupid bug of mine in cg386mat that broke the
      include() code (and make cycle :( ) if you compiled without
      -dnewoptimizations

  Revision 1.67  2000/01/09 01:44:21  jonas
    + (de)allocation info for EDI to fix reported bug on mailinglist.
      Also some (de)allocation info for ESI added. Between -dallocEDI
      because at this time of the night bugs could easily slip in ;)

  Revision 1.66  2000/01/07 01:14:22  peter
    * updated copyright to 2000

  Revision 1.65  1999/12/22 01:01:47  peter
    - removed freelabel()
    * added undefined label detection in internal assembler, this prevents
      a lot of ld crashes and wrong .o files
    * .o files aren't written anymore if errors have occured
    * inlining of assembler labels is now correct

  Revision 1.64  1999/12/20 21:42:35  pierre
    + dllversion global variable
    * FPC_USE_CPREFIX code removed, not necessary anymore
      as we use .edata direct writing by default now.

  Revision 1.63  1999/12/01 22:45:54  peter
    * fixed wrong assembler with in-node

  Revision 1.62  1999/11/30 10:40:43  peter
    + ttype, tsymlist

  Revision 1.61  1999/11/20 01:22:18  pierre
    + cond FPC_USE_CPREFIX (needs also some RTL changes)
      this allows to use unit global vars as DLL exports
      (the underline prefix seems needed by dlltool)

  Revision 1.60  1999/11/17 17:04:58  pierre
   * Notes/hints changes

  Revision 1.59  1999/11/15 14:04:00  pierre
   * self pointer stabs for local function was wrong
}
