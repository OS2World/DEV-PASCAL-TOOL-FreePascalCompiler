{
    $Id: cga.pas,v 1.1.2.97 2003/06/18 23:19:42 pierre Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    Helper routines for the m68k code generator

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
       symconst,symtable,aasm,cgbase;


    function def_opsize(p1:pdef):topsize;

{*****************************************************************************
                       GENERIC CODE GENERATION API
*****************************************************************************}

    { Load a source location (LOC_MEM,LOC_REFERENCE,LOC_REGISTER or LOC_CREGISTER)      }
    { into the destination register taking into account the source size and destination }
    { size (sign extension, zero extension - as required).                              }
    procedure emit_load_loc_reg(source:tlocation;srcdef:pdef; destdef : pdef;
                              destreg:tregister);
    { Load a source location (LOC_MEM,LOC_REFERENCE,LOC_REGISTER or LOC_CREGISTER)      }
    { into the destination 32-bit register taking into account the source size          }
    { size (sign extension, zero extension - as required).                              }
{    procedure emit_load_loc_reg32(source: tlocation;
       REPALCES emitloadord2reg()
                                               );}

    { Sets the node location from LOC_JUMP to LOC_REGISTER }
    { and sets the destination register either to zero     }
    { or one depending on the value of the specified flag  }
    { value.                                               }
{   procedure emit_set_flag_reg(var l : tlocation);
       REPLACES locflags2reg(var l:tlocation;opsize:topsize);}

    { emit code which will either set the destination register }
    { to zero or 1 depending on the result in the flags        }
    procedure locjump2reg(var l:tlocation;opsize:topsize; otl, ofl: pasmlabel);


    procedure locflags2reg(var l:tlocation;opsize:topsize);


    procedure emitlab(var l : pasmlabel);
    procedure emitlabeled(op : tasmop;var l : pasmlabel);
    procedure emitjmp(c : tasmcond;var l : pasmlabel);
    procedure emit_flag2reg(flag:tresflags;hregister:tregister);

    procedure emit_mov_ref_reg(s : topsize;ref : preference;reg : tregister);
    procedure emit_mov_reg_reg(s : topsize;reg1,reg2 : tregister);



    procedure emitcall(const routine:string);

    procedure emit_mov_loc_reg(const t:tlocation;reg:tregister);
    procedure emit_mov_ref_reg64(r : treference;rl,rh : tregister);
    procedure emit_lea_loc_reg(const t:tlocation;reg:tregister;freetemp:boolean);

    procedure emit_push_loc(const t:tlocation);
    procedure emit_push_mem_size(const t: treference; size: longint; alignment : longint);

    { pushes qword location to the stack }
    procedure emit_pushq_loc(const t : tlocation);
    procedure release_qword_loc(const t : tlocation);

    { remove non regvar registers in loc from regs (in the format }
    { pushusedregisters uses)                                     }
    procedure remove_non_regvars_from_loc(const t: tlocation; var regs: tregisterset);
    { releases the registers of a location }
    procedure release_loc(const t : tlocation);

    procedure emit_pushw_loc(const t:tlocation);
    procedure emit_push_lea_loc(const t:tlocation;freetemp:boolean);
    procedure emit_to_mem(var p:ptree);
    procedure emit_movq_reg_loc(reghigh,reglow: TRegister;t:tlocation);

    procedure copyshortstring(const dref,sref : treference;len : byte;
                        loadref, del_sref: boolean);
    procedure loadansistring(p : ptree);

    procedure finalize(t : pdef;const ref : treference;is_already_ref : boolean);
    procedure incrstringref(t : pdef;const ref : treference);
    procedure decrstringref(t : pdef;const ref : treference);

    { These two routines are used to save the current tree location }
    { on the stack, when we need more registers.                    }
    { for general purpose registers.                                }
    function maybe_push(needed : byte;p : ptree;isint64 : boolean) : boolean;
    procedure restore(p : ptree;isint64 : boolean);
    { These two routines are used to save the current tree location }
    { on the stack, when we need more registers.                    }
    { for floating point registers.                                 }
    function maybe_push_float(needgeneral : byte; neededfpu: byte;
       p : ptree): boolean;

    procedure push_int(l : longint);
    procedure emit_push_mem(const ref : treference);
    procedure emitpushreferenceaddr(const ref : treference);
    procedure pushsetelement(p : ptree);
    procedure push_value_para(p:ptree;inlined,is_cdecl:boolean;
                              para_offset:longint;alignment : longint);

    procedure maybe_loadself;
    procedure maketojumpbool(p : ptree);

    procedure emitloadord2reg(const location:Tlocation;srcdef:Porddef;destreg:Tregister;delloc:boolean);
    procedure emitoverflowcheck(p:ptree);
    procedure emitrangecheck(p:ptree;todef:pdef);
    procedure concatcopy(source,dest : treference;size : longint;delsource : boolean;loadref:boolean);
    procedure firstcomplex(p : ptree);

    procedure genentrycode(alist : paasmoutput;const proc_names:Tstringcontainer;make_global:boolean;
                           stackframe:longint;
                           var parasize:longint;var nostackframe:boolean;
                           inlined : boolean;var savedstackoffset : longint);
    procedure genexitcode(alist : paasmoutput;parasize:longint;
                          nostackframe,inlined:boolean;savedstackoffset : longint);

    { if a unit doesn't have a explicit init/final code,  }
    { we've to generate one, if the units has ansistrings }
    { in the interface or implementation                  }
    procedure genimplicitunitfinal(alist : paasmoutput);
    procedure genimplicitunitinit(alist : paasmoutput);

{    procedure sendparameter(paramnr : byte;
      const loc: tlocation; size : topsize;
       lea : boolean; signext: boolean);}

    procedure floatload(t : tfloattype;ref : treference; var location:tlocation);
    procedure floatstore(t : tfloattype;var location:tlocation; ref:treference);
    function getfloatsize(typ : tfloattype): topsize;

    function cdecl_function_return_address_reg : tregister;

const
  { used to avoid temporary assignments }
  dest_loc_known : boolean = false;
  in_dest_loc    : boolean = false;
  dest_loc_tree  : ptree = nil;


var
  dest_loc : tlocation;



  implementation

    uses
       strings,globtype,systems,globals,verbose,files,types,pbase,
       tgen,temp_gen,hcodegen,ppu
{$ifdef GDB}
       ,gdb
{$endif}
       ;

{ when calling C functions with complex return values,
  the register used seems to depend on specific target PM }
   function cdecl_function_return_address_reg : tregister;
     begin
       if target_info.target = target_m68k_netbsd then
         cdecl_function_return_address_reg:=R_A0
       else
         cdecl_function_return_address_reg:=R_A1;
     end;



{*****************************************************************************
                                Helpers
*****************************************************************************}

    function def_opsize(p1:pdef):topsize;
      begin
        case p1^.size of
         1 : def_opsize:=S_B;
         2 : def_opsize:=S_W;
         4 : def_opsize:=S_L;
         8 : def_opsize:=S_L;
        else
         internalerror(78);
        end;
      end;


    procedure locflags2reg(var l:tlocation;opsize:topsize);
      var
        hregister : tregister;
      begin
        if (l.loc=LOC_FLAGS) then
         begin
           hregister:=getregister32;
           emit_flag2reg(l.resflags,hregister);
           { emit_const_reg(A_AND,opsize,hregister);
             done already in flag2reg }
           l.loc:=LOC_REGISTER;
           l.register:=hregister;
         end
        else internalerror(270720001);
      end;


    procedure locjump2reg(var l:tlocation;opsize:topsize; otl, ofl: pasmlabel);
      var
        hl : pasmlabel;
      begin
         if l.loc = LOC_JUMP then
           begin
             l.loc:=LOC_REGISTER;
             l.register:=getregister32;
             emitlab(truelabel);
             truelabel:=otl;
             { set the register destination to TRUE }
             if isaddressregister(l.register) then
               emit_const_reg(A_MOVE,S_L,1,l.register)
             else
               emit_const_reg(A_MOVEQ,S_L,1,l.register);
             getlabel(hl);
             emitjmp(C_None,hl);
             emitlab(falselabel);
             falselabel:=ofl;
             { set the register destinatio to FALSE }
             emit_reg(A_CLR,opsize,l.register);
             emitlab(hl);
           end
        else internalerror(270720002);
      end;


{*****************************************************************************
                           Floating point interface
*****************************************************************************}

  (***********************************************************************)
  (* PROCEDURE FLOATLOAD                                                 *)
  (*  Description: This routine is to be called each time a location     *)
  (*   must be set to LOC_FPU and a value loaded into a FPU register.    *)
  (*                                                                     *)
  (*  Remark: The routine sets up the register field of LOC_FPU correctly*)
  (***********************************************************************)
    procedure floatload(t : tfloattype;ref : treference; var location:tlocation);

      var
         op : tasmop;
         s : topsize;

      begin
        { no emulation }
        case t of
            s32real : s := S_FS;
            s64real : s := S_FL;
            s80real : s := S_FX;
         else
           begin
             CGMessage(cg_f_unknown_float_type);
           end;
        end; { end case }
        location.loc := LOC_FPU;
        if not ((cs_fp_emulation) in aktmoduleswitches) then
        begin
            location.fpuregister := getfloatreg;
            emit_ref_reg(A_FMOVE,s,newreference(ref),location.fpuregister);
        end
        else
        { handle emulation }
        begin
          case t of
          s32real:
            Begin
              location.fpuregister := getregister32;
              emit_ref_reg(A_MOVE,S_L,newreference(ref),location.fpuregister);
            end;
          { not supported }
          s64real,s80real:
            Begin
              internalerror(3434);
            end;
          else
             { other floating types are not supported in emulation mode }
            CGMessage(sym_e_type_id_not_defined);
          end; {end case}
        end;
      end;


  (***********************************************************************)
  (* PROCEDURE FLOATSTORE                                                *)
  (*  Description: This routine is to be called when a value located     *)
  (*   in LOC_FPU must be stored into memory.                            *)
  (*                                                                     *)
  (*  Remark: This routine frees the LOC_FPU location.                   *)
  (*  ref : Destination of storage.                                      *)
  (*  location : source of LOC_FPU location                              *)
  (***********************************************************************)
    procedure floatstore(t : tfloattype;var location:tlocation; ref:treference);

      var
         op : tasmop;
         s : topsize;

      begin
        if location.loc <> LOC_FPU then
         InternalError(34);
        { no emulation }
        case t of
            s32real : s := S_FS;
            s64real : s := S_FL;
            s80real : s := S_FX;
         else
             CGMessage(cg_f_unknown_float_type);
        end; { end case }
        if not ((cs_fp_emulation) in aktmoduleswitches) then
         Begin
            { This permits mixing emulation, and non-emulation code }
            { together.                                             }
            if not (location.fpuregister in [R_FP0..R_FP7]) then
             Begin
               case s of
               S_FS :
                 Begin
                   emit_reg_ref(A_MOVE,S_L,location.fpuregister,newreference(ref));
                   ungetregister(location.fpuregister);
                 End;
               S_FL,S_FX:
                 Begin
                   emit_reg_ref(A_MOVE,S_L,location.fpuregister,newreference(ref));
                   ungetregister(location.fpuregister);
                   location.fpuregister:=getfloatreg;
                   emit_ref_reg(A_FMOVE,S_FS,newreference(ref),location.fpuregister);
                   emit_reg_ref(A_FMOVE,s,location.fpuregister,newreference(ref));
                   ungetregister(location.fpuregister);
                   {internalerror(234);}
                 End;
               else
                 internalerror(234);
               end; { end case }
             end
            else
               emit_reg_ref(A_FMOVE,s,location.fpuregister,newreference(ref));
         end
        else
        { handle emulation }
        begin
          case t of
          s32real :
             begin
               emit_reg_ref(A_MOVE,S_L,location.fpuregister,newreference(ref));
               ungetregister(location.fpuregister);
             end;
          { currently not supported }
          s64real,s80real :
             begin
               internalerror(3435);
             end;
          else
             { other floating types are not supported in emulation mode }
            CGMessage(sym_e_type_id_not_defined);
          end; {end case}
        end;
        location.fpuregister:=R_NO;  { no register in LOC_FPU now }
      end;


    { should never be called in emulation mode }
    function getfloatsize(typ : tfloattype): topsize;
     begin
      if ((cs_fp_emulation) in aktmoduleswitches) then
        internalerror(14567);
      case typ of
       s80real:
                begin
                 getfloatsize := S_FX;
                end;
       s32real:
                begin
                 getfloatsize := S_FS;
                end;
       s64real:
                begin
                 getfloatsize := S_FL;
                end;
      else
        internalerror(1212);
      end;
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


    procedure emitlabeled(op : tasmop;var l : pasmlabel);

      begin
         if op=A_LABEL then
            internalerror(1313)
         else
           exprasmlist^.concat(new(pai_labeled,init(op,l)))
      end;

    procedure emitjmp(c : tasmcond;var l : pasmlabel);
      var
        ai : Paicpu;
      begin
        if c=C_None then
          begin
             if tf_code_small in target_info.flags then
               ai := new(paicpu,op_sym(A_BRA,S_NO,l))
             else
               ai := new(paicpu,op_sym(A_JMP,S_NO,l));
          end
        else
          begin
            internalerror(11);
          end;
        ai^.is_jmp:=true;
        exprasmlist^.concat(ai);
      end;


    procedure emit_flag2reg(flag:tresflags;hregister:tregister);
      var
        ai : paicpu;
      begin
         ai:=new(paicpu,op_reg(flag_2_set[flag],S_B,hregister));
         exprasmlist^.concat(ai);
         { only keep the lower 8-bits always }
         { these instructions do set the byte part of the register to $ff
           but we need 1 or 0 for booleans PM
           so only keep the first bit }
         exprasmlist^.concat(new(paicpu,op_const_reg(A_AND,S_L,$1,hregister)));
      end;


    procedure emit_mov_ref_reg(s : topsize;ref : preference;reg : tregister);
      begin
         exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,s,ref,reg)));
      end;



    procedure emit_mov_reg_reg(s : topsize;reg1,reg2 : tregister);
      begin
         if (reg1<>reg2) then
           exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,s,reg1,reg2)));
      end;


    procedure emitcall(const routine:string);
      begin
        if target_info.target=target_m68k_palmos then
           exprasmlist^.concat(new(paicpu,op_sym(A_BSR,S_W,newasmsymbol(routine))))
        else
           exprasmlist^.concat(new(paicpu,op_sym(A_JSR,S_NO,newasmsymbol(routine))));
      end;

    { only useful in startup code }
    procedure emitinsertcall(const routine:string);
      begin
        if (tf_code_small in target_info.flags) or
           (target_info.target=target_m68k_palmos) then
           exprasmlist^.insert(new(paicpu,op_sym(A_BSR,S_W,newasmsymbol(routine))))
        else
           exprasmlist^.insert(new(paicpu,op_sym(A_JSR,S_NO,newasmsymbol(routine))));
      end;



(*
    procedure sendparameter(paramnr : byte;
      const t: tlocation; size : topsize;
       lea : boolean; signext: boolean);
      begin
        case t.loc of
          LOC_REGISTER,
         LOC_CREGISTER : begin
                           emit_reg_reg(A_MOVE,S_L,t.register,R_SPPUSH);
                           ungetregister(t.register); { the register is not needed anymore }
                         end;
               LOC_MEM,
         LOC_REFERENCE : begin
                           if t.reference.is_immediate then
                             emit_const_reg(A_MOVE,S_L,t.reference.offset,R_SPPUSH)
                           else
                             emit_ref_reg(A_MOVE,S_L,newreference(t.reference),R_SPPUSH);
                           del_reference(t.reference);
                           ungetiftemp(t.reference);
                         end;
        else
         internalerror(330);
        end;
      end;*)


    { emit a move from a source type definition }
    { using sign extension as necessary         }
    { depending on source / destination result  }
    { types.                                    }

    procedure emit_load_loc_reg(source:tlocation;srcdef:pdef;
                              destdef : pdef;
                              destreg:tregister);

    var r:preference;
        opsize : topsize;
        hr: preference;

    begin
        { get the correct size operand }
        case srcdef^.size of
         1 : opsize:=S_B;
         2 : opsize:=S_W;
         4 : opsize:=S_L;
         8 : opsize:=S_L;
        else
         internalerror(78);
        end;
        if assigned(destdef) then
         begin
           case destdef^.size of
            1 : opsize:=S_B;
            2 : begin
                  if opsize=S_B then
                   opsize:=S_BW
                  else
                   opsize:=S_W;
                end;
            4,8:
              begin
                 case opsize of
                    S_B : opsize:=S_BL;
                    S_W : opsize:=S_WL;
                 end;
              end;
           end;
         end;
        case source.loc of
            LOC_REGISTER,LOC_CREGISTER:
                begin
                    if opsize in [S_B,S_W,S_L] then
                      begin
                       emit_reg_reg(A_MOVE,opsize,source.register,destreg);
                      end
                    else
                      begin
                        case opsize of
                        S_BW:
                          begin
                           if is_signed(srcdef) then
                             begin
                               emit_reg_reg(A_MOVE,S_B,source.register,destreg);
                               emit_reg(A_EXT,S_W,destreg);
                             end
                           else
                             begin
                               if source.register <> destreg then
                                begin
                                 emit_reg(A_CLR,S_W,source.register);
                                 emit_reg_reg(A_MOVE,S_B,source.register,destreg);
                                end
                               else
                                 emit_const_reg(A_AND,S_L,$FF,destreg);
                             end;
                          end;
                        S_BL:
                          begin
                           if is_signed(srcdef) then
                             begin
                               emit_reg_reg(A_MOVE,S_B,source.register,destreg);
                               if aktoptprocessor = MC68020 then
                                 emit_reg(A_EXTB,S_L,destreg)
                               else
                                 begin
                                   emit_reg(A_EXT,S_W,destreg);
                                   emit_reg(A_EXT,S_L,destreg);
                                 end;
                             end
                           else
                             begin
                               if source.register <> destreg then
                               begin
                                  emit_reg(A_CLR,S_L,destreg);
                                  emit_reg_reg(A_MOVE,S_B,source.register,destreg);
                               end
                               else
                                  emit_const_reg(A_AND,S_L,$FF,destreg);
                             end;
                          end;
                        S_WL:
                          begin
                           if is_signed(srcdef) then
                             begin
                               emit_reg_reg(A_MOVE,S_W,source.register,destreg);
                               emit_reg(A_EXT,S_L,destreg);
                             end
                           else
                             begin
                               if destreg <> source.register then
                               begin
                                 emit_reg(A_CLR,S_L,destreg);
                                 emit_reg_reg(A_MOVE,S_W,source.register,destreg);
                               end
                               else
                                 emit_const_reg(A_AND,S_L,$FFFF,destreg);
                             end;
                          end;
                       end; {end case}
                      end;

                end;
            { in this case we must always load into a register first
              because of endian problems
            }
            LOC_REFERENCE,LOC_MEM:
                begin
                    { for 64-bit values, it should point to the low-order 32-bit value }
                    hr:=newreference(source.reference);
                    inc(hr^.offset,4);
                    case srcdef^.size of
                     1: emit_ref_reg(A_MOVE,S_B,newreference(source.reference),destreg);
                     2: emit_ref_reg(A_MOVE,S_W,newreference(source.reference),destreg);
                     4: emit_ref_reg(A_MOVE,S_L,newreference(source.reference),destreg);
                     8: emit_ref_reg(A_MOVE,S_L,hr,destreg);
                    else
                      internalerror(1212);
                    end;

                    if opsize in [S_B,S_W,S_L] then
                      begin
                        { nothing to do here, already taken care above }
                       {emit_ref_reg(A_MOVE,opsize,newreference(source.reference),destreg);}
                      end
                    else
                        case opsize of
                        S_BW:
                          begin
                           if is_signed(srcdef) then
                             begin
                               emit_reg(A_EXT,S_W,destreg);
                             end
                           else
                             begin
                               emit_const_reg(A_AND,S_W,$FF,destreg);
                             end;
                          end;
                        S_BL:
                          begin
                           if is_signed(srcdef) then
                             begin
                               if aktoptprocessor = MC68020 then
                                     emit_reg(A_EXTB,S_L,destreg)
                               else
                                 begin
                                   emit_reg(A_EXT,S_W,destreg);
                                   emit_reg(A_EXT,S_L,destreg);
                                  end;
                             end
                           else
                             begin
                               emit_const_reg(A_AND,S_L,$FF,destreg);
                             end;
                          end;
                        S_WL:
                          begin
                           if is_signed(srcdef) then
                             begin
                               emit_reg(A_EXT,S_L,destreg);
                             end
                           else
                             begin
                               emit_const_reg(A_AND,S_L,$FFFF,destreg);
                             end;
                          end;
                       end; {end case}
                end
            else
                internalerror(6);
        end;
    end;

    procedure emit_mov_loc_reg(const t:tlocation;reg:tregister);
      begin
        case t.loc of
          LOC_REGISTER,
         LOC_CREGISTER : begin
                           emit_reg_reg(A_MOVE,S_L,t.register,reg);
                           ungetregister32(t.register); { the register is not needed anymore }
                         end;
               LOC_MEM,
         LOC_REFERENCE : begin
                           if t.reference.is_immediate then
                             emit_const_reg(A_MOVE,S_L,
                               t.reference.offset,reg)
                           else
                             begin
                               emit_ref_reg(A_MOVE,S_L,
                                 newreference(t.reference),reg);
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
                               if not isaddressregister(reg) then
                                 begin
                                   internalerror(772);
                                   ungetregister(reg);
                                   reg := getaddressreg;
                                 end;
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
                           emit_reg_reg(A_MOVE,S_L,
                             reglow,t.registerlow);
                           emit_reg_reg(A_MOVE,S_L,
                             reghigh,t.registerhigh);
                         end;
               LOC_MEM,
         LOC_REFERENCE : begin
                           if t.reference.is_immediate then
                             internalerror(334)
                           else
                             begin
                               emit_reg_ref(A_MOVE,S_L,
                                 Reghigh,newreference(t.reference));
                               inc(t.reference.offset,4);
                               emit_reg_ref(A_MOVE,S_L,
                                 reglow,newreference(t.reference));
                               dec(t.reference.offset,4);
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
                 emit_reg_reg(A_MOVE,S_L,t.registerlow,R_SPPUSH);
                 emit_reg_reg(A_MOVE,S_L,t.registerhigh,R_SPPUSH);
              end;
            LOC_MEM,
            LOC_REFERENCE:
              begin
                 hr:=newreference(t.reference);
                 inc(hr^.offset,4);
                 emit_ref_reg(A_MOVE,S_L,hr,R_SPPUSH);
                 emit_ref_reg(A_MOVE,S_L,newreference(t.reference),R_SPPUSH);
                 ungetiftemp(t.reference);
              end;
            else internalerror(331);
         end;
      end;



    procedure remove_non_regvars_from_loc(const t: tlocation; var regs: tregisterset);
    begin
      case t.loc of
        LOC_REGISTER:
          begin
            { can't be a regvar, since it would be LOC_CREGISTER then }
            regs := regs - [t.register];
            if t.registerhigh <> R_NO then
              regs := regs - [t.registerhigh];
          end;
        LOC_MEM,LOC_REFERENCE:
          begin
            if not(cs_regalloc in aktglobalswitches) or
               (t.reference.base in usableregs) then
              regs := regs - [t.reference.base];
            if not(cs_regalloc in aktglobalswitches) or
               (t.reference.index in usableregs) then
              regs := regs - [t.reference.index];
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
                           emit_reg_reg(A_MOVE,S_L,t.register,R_SPPUSH);
                           ungetregister(t.register); { the register is not needed anymore }
                         end;
               LOC_MEM,
         LOC_REFERENCE : begin
                           if t.reference.is_immediate then
                             emit_const_reg(A_MOVE,S_L,t.reference.offset,R_SPPUSH)
                           else
                             emit_ref_reg(A_MOVE,S_L,newreference(t.reference),R_SPPUSH);
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
                             emit_reg_reg(A_MOVE,S_L,t.register,R_SPPUSH)
                           else
                             emit_reg_reg(A_MOVE,S_W,t.register,R_SPPUSH);
                           ungetregister(t.register); { the register is not needed anymore }
                         end;
               LOC_MEM,
         LOC_REFERENCE : begin
                           if target_os.stackalignment=4 then
                            opsize:=S_L
                           else
                            opsize:=S_W;
                           if t.reference.is_immediate then
                             emit_const_reg(A_MOVE,opsize,t.reference.offset,R_SPPUSH)
                           else
                             emit_ref_reg(A_MOVE,opsize,newreference(t.reference),R_SPPUSH);
                           del_reference(t.reference);
                           ungetiftemp(t.reference);
                         end;
        else
         internalerror(330);
        end;
      end;




    procedure emit_push_lea_loc(const t:tlocation;freetemp:boolean);
      var
       hreg : tregister;
      begin
        case t.loc of
               LOC_MEM,
         LOC_REFERENCE : begin
                           if t.reference.is_immediate then
                             internalerror(331)
                           else
                             begin
                               hreg := getaddressreg;
                               emit_ref_reg(A_LEA,S_L,
                                 newreference(t.reference),hreg);
                               emit_reg_reg(A_MOVE,S_L,hreg,R_SPPUSH);
                               ungetregister(hreg);
                             end;
                           if freetemp then
                            ungetiftemp(t.reference);
                         end;
        else
         internalerror(332);
        end;
      end;

    procedure emit_push_mem_size(const t: treference; size: longint; alignment : longint);

      var
        s: topsize;
        hreg : tregister;

      begin
        if t.is_immediate then
          begin
              { everything is pushed as a longint when the stack is
                aligned on a four byte boundary.
              }
              if (alignment=4) then
                  emit_const_reg(A_MOVE,S_L,t.offset,R_SPPUSH)
              else
                begin
                  case size of
                    1 : begin
                          { decrements the SP by 2 }
                          emit_const_reg(A_MOVE,S_B,t.offset,R_SPPUSH);
                        end;
                    2 : emit_const_reg(A_MOVE,S_W,t.offset,R_SPPUSH);
                    3 : emit_const_reg(A_MOVE,S_L,t.offset*256,R_SPPUSH);
                    4 : emit_const_reg(A_MOVE,S_L,t.offset,R_SPPUSH);
                  else
                   internalerror(1516);
                  end;
                end;
          end
        else if (size < 4) then
            begin
              hreg := getregister32;
              case size of
                1:
                 begin
                   emit_ref_reg(A_MOVE,S_B,newreference(t),hreg);
                   emit_const_reg(A_AND,S_L,$FF,hreg);
                 end;
                2:
                 begin
                     emit_ref_reg(A_MOVE,S_W,newreference(t),hreg);
                     emit_const_reg(A_AND,S_L,$FFFF,hreg);
                 end;
                3:
                 begin
                     emit_ref_reg(A_MOVE,S_L,newreference(t),hreg);
                     emit_const_reg(A_LSR,S_L,8,hreg);
                 end;
                else internalerror(200008071);
              end;
              if alignment=4 then
                emit_reg_reg(A_MOVE,S_L,hreg,R_SPPUSH)
              else
              if size = 1 then
                 begin
                   { increments the SP by 2 }
                   emit_reg_reg(A_MOVE,S_B,hreg,R_SPPUSH)
                 end
              else if (size = 2) then
                 emit_reg_reg(A_MOVE,S_W,hreg,R_SPPUSH)
              else { size = 3 }
                 emit_reg_reg(A_MOVE,S_L,hreg,R_SPPUSH);
              ungetregister(hreg);
            end
          else
            if size = 4 then
              emit_push_mem(t)
            else
              internalerror(200008072);
      end;


      { put value on the stack temporary space }
    procedure emit_to_mem(var p:ptree);
      var
         r : treference;

      begin
        case p^.location.loc of
               { normally this should be in registers }
               LOC_FPU : begin
                           { reserve some stack space }
                           gettempofsizereference(extended_size,r);
                           { load it to the reserved stack space }
                           { and free the registers              }
                           floatstore(pfloatdef(p^.resulttype)^.typ,
                              p^.location, r);
                           p^.location.reference:=r;
                         end;
               LOC_REGISTER:
                 begin
                    if is_64bitint(p^.resulttype) then
                      begin
                         gettempofsizereference(8,r);
                         emit_reg_ref(A_MOVE,S_L,p^.location.registerlow,
                           newreference(r));
                         inc(r.offset,4);
                         emit_reg_ref(A_MOVE,S_L,p^.location.registerhigh,
                           newreference(r));
                         dec(r.offset,4);
                      end
                    else
                      begin
                         gettempofsizereference(p^.resulttype^.size,r);
                         emit_reg_ref(A_MOVE,def_opsize(p^.resulttype),p^.location.register,
                           newreference(r));
                      end;
                   p^.location.reference:=r;
                 end;
               LOC_MEM,
         LOC_REFERENCE : ;
         else
         internalerror(333);
        end;
        p^.location.loc:=LOC_MEM;
      end;

    procedure emit_mov_ref_reg64(r : treference;rl,rh : tregister);

      var
         hr : preference;
         hreg : tregister;

      begin
         { if we load a 64 bit reference, we must be careful because }
         { we could overwrite the registers of the reference by      }
         { accident                                                  }
         hreg := R_NO;
         if r.base=rl then
           begin
              hreg := getaddressreg;
              emit_reg_reg(A_MOVE,S_L,r.base,hreg);
              r.base:=hreg;
           end
         else if r.index=rl then
           begin
              getexplicitregister32(accumulator);
              emit_reg_reg(A_MOVE,S_L,r.index,accumulator);
              r.index:=accumulator;
              ungetregister(accumulator);
           end;
         emit_ref_reg(A_MOVE,S_L,newreference(r),rh);
         hr:=newreference(r);
         inc(hr^.offset,4);
         emit_ref_reg(A_MOVE,S_L,hr,rl);
         if hreg <> R_NO then
           ungetregister32(hreg);
      end;

{*****************************************************************************
                           Emit String Functions
*****************************************************************************}

    procedure copyshortstring(const dref,sref : treference;len : byte;
                loadref, del_sref: boolean);
      var
        regs_to_push: tregisterset;
        saved : tpushed;
        t : tlocation;
      begin
         regs_to_push := ALL_REGISTERS;
         t.loc:=LOC_REFERENCE;
         t.reference:=dref;
         remove_non_regvars_from_loc(t,regs_to_push);
         t.reference:=sref;
         remove_non_regvars_from_loc(t,regs_to_push);
         saveusedregisters(saved, regs_to_push);
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
         restoreusedregisters(saved);
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
         saveusedregisters(pushedregs,ALL_REGISTERS);
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
         restoreusedregisters(pushedregs);
      end;


    procedure decrstringref(t : pdef;const ref : treference);

      var
         pushedregs : tpushed;

      begin
         saveusedregisters(pushedregs,ALL_REGISTERS);
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
         restoreusedregisters(pushedregs);
      end;

    procedure loadansistring(p : ptree);
    {
      copies an ansistring from p^.right to p^.left, we
      assume, that both sides are ansistring, firstassignement have
      to take care of that, an ansistring can't be a register variable
    }
      var
         pushed : tpushed;
         regs_to_push : tregisterset;
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
         regs_to_push := ALL_REGISTERS;
         remove_non_regvars_from_loc(p^.right^.location,regs_to_push);
         remove_non_regvars_from_loc(p^.left^.location,regs_to_push);
         { And save them (JM) }
         saveusedregisters(pushed,regs_to_push);
         case p^.right^.location.loc of
            LOC_REGISTER,LOC_CREGISTER:
              begin
                 emit_reg_reg(A_MOVE,S_L,p^.right^.location.register, R_SPPUSH);
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
         restoreusedregisters(pushed);
         if ungettemp then
           ungetiftemp(p^.right^.location.reference);
      end;


{*****************************************************************************
                           Emit Push Functions
*****************************************************************************}

    function maybe_push_float(needgeneral : byte; neededfpu: byte;
       p : ptree): boolean;
      var
         pushed : boolean;
         hreg : tregister;
     begin
         pushed := false;
         if ((needgeneral>= usablereg32) and (cs_fp_emulation in aktmoduleswitches)) or
            ((neededfpu >= usablefloatreg) and not (cs_fp_emulation in aktmoduleswitches)) then
           begin
              if (p^.location.loc = LOC_FPU) then
                  begin
                   { are in emulation mode? }
                   if cs_fp_emulation in aktmoduleswitches then
                     begin
                       pushed:=true;
                       emit_reg_reg(A_MOVE, S_L, p^.location.fpuregister, R_SPPUSH);
                       ungetregister(p^.location.fpuregister);
                     end
                   else
                      begin
                       pushed:=true;
                       emit_reg_reg(A_FMOVE, S_FX, p^.location.fpuregister, R_SPPUSH);
                       ungetregister(p^.location.fpuregister);
                      end;
                  end
              else
               begin
                 pushed:=false;
                 internalerror(35);
               end;
           end
         else
          pushed:=false;
         maybe_push_float:=pushed;
     end;

    function maybe_push(needed : byte;p : ptree;isint64 : boolean) : boolean;
      var
         pushed : boolean;
         hreg : tregister;
      begin
         if (needed> usablereg32) or (needed > usableaddress) or (needed > usablefloatreg) then
           begin
              if (p^.location.loc=LOC_REGISTER) then
                begin
                   if isint64 then
                     begin
                        emit_reg_reg(A_MOVE,S_L,p^.location.registerhigh,R_SPPUSH);
                        ungetregister(p^.location.registerhigh);
                     end;
                   pushed:=true;
                   emit_reg_reg(A_MOVE, S_L, p^.location.register, R_SPPUSH);
                   ungetregister(p^.location.register);
                end
              else if (p^.location.loc in [LOC_MEM,LOC_REFERENCE]) and
                      ((p^.location.reference.base<>R_NO) or
                       (p^.location.reference.index<>R_NO)
                      ) then
                  begin
                     del_reference(p^.location.reference);
                     hreg := getaddressreg;
                     emit_ref_reg(A_LEA,S_L,newreference(p^.location.reference),hreg);
                     emit_reg_reg(A_MOVE,S_L,hreg,R_SPPUSH);
                     ungetregister(hreg);
                     pushed:=true;
                  end
              else if (p^.location.loc = LOC_FPU) then
                  begin
                   { are in emulation mode? }
                   if cs_fp_emulation in aktmoduleswitches then
                     begin
                       pushed:=true;
                       { overlayed with fpuregisterlow }
                       emit_reg_reg(A_MOVE, S_L, p^.location.fpuregister, R_SPPUSH);
                       ungetregister(p^.location.fpuregister);
                     end
                   else
                      begin
                       pushed:=true;
                       { overlayed with fpuregisterlow }
                       emit_reg_reg(A_FMOVE, S_FX, p^.location.fpuregister, R_SPPUSH);
                       ungetregister(p^.location.fpuregister);
                      end;
                  end
              else pushed:=false;
           end
         else pushed:=false;
         maybe_push:=pushed;
      end;


    procedure push_int(l : longint);

      begin
         if (l = 0) and (aktoptprocessor = MC68020) then
           begin
             emit_reg(A_CLR,S_L,accumulator);
             emit_reg_reg(A_MOVE,S_L,accumulator, R_SPPUSH);
           end
         else
         if not(cs_littlesize in aktglobalswitches) and (l >= -128) and (l <= 127) then
           begin
             emit_const_reg(A_MOVEQ, S_L, l, accumulator);
             emit_reg_reg(A_MOVE,S_L, accumulator, R_SPPUSH);
           end
         else
           emit_const_reg(A_MOVE,S_L,l,R_SPPUSH);
      end;


    procedure emit_push_mem(const ref : treference);

      begin
         if ref.is_immediate then
           push_int(ref.offset)
         else
           emit_ref_reg(A_MOVE, S_L,newreference(ref),R_SPPUSH);
      end;


    procedure emitpushreferenceaddr(const ref : treference);
    { Push a pointer to a value on the stack }
       var
        href : treference;
        hreg : tregister;
      begin
         if ref.is_immediate then
          begin
              reset_reference(href);
              { push_int(ref.offset)
              internalerror(12);
              needed for nil procedure of object in fcl PM }
              gettempofsizereference(4,href);
              emit_const_ref(A_MOVE,S_L,ref.offset,newreference(href));
              emitpushreferenceaddr(href);
              del_reference(href);
          end
         else
           begin
              if (ref.base=R_NO) and (ref.index=R_NO) then
                 emit_ref(A_PEA, S_L, newreference(ref))
              else if (ref.base=R_NO) and (ref.index<>R_NO) and
                 (ref.offset=0) and (ref.scalefactor=0) and (ref.symbol=nil) then
                 emit_reg_reg(A_MOVE,S_L,ref.index,R_SPPUSH)
              else if (ref.base<>R_NO) and (ref.index=R_NO) and
                 (ref.offset=0) and (ref.symbol=nil) then
                 emit_reg_reg(A_MOVE,S_L,ref.base,R_SPPUSH)
              else
                begin
                   emit_ref(A_PEA,S_L,newreference(ref));
                end;
           end;
        end;


     procedure pushsetelement(p : ptree);
     {
       copies p a set element on the stack
       the value pushed on the stack must always be converted to a byte, this
       is the prologue to the internal set handling routine calls

     }
     var
       hr : tregister;
      begin
      { copy the element on the stack, slightly complicated }
        if p^.treetype=ordconstn then
         begin
           if target_os.stackalignment=4 then
              emit_const_reg(A_MOVE,S_L,p^.value,R_SPPUSH)
           else
              { this conserves 2 byte alignment }
              emit_const_reg(A_MOVE,S_B,p^.value,R_SPPUSH);
         end
        else
         begin
           case p^.location.loc of
             LOC_REGISTER,
             LOC_CREGISTER :
               begin
                 hr:=p^.location.register;
                 if target_os.stackalignment=4 then
                   emit_reg_reg(A_MOVE,S_L,hr,R_SPPUSH)
                 else
                    { this conserves 2 byte alignment }
                    emit_reg_reg(A_MOVE,S_B,hr,R_SPPUSH)
               end;
           else
             begin
               { only a byte can be pushed, but since   }
               { the system is big-endian, we must      }
               { absolutely load the full size into     }
               { a register.                            }
               { compare with the i386 version          }
               hr := getregister32;
               case p^.resulttype^.size of
                1:
                 begin
                   emit_ref_reg(A_MOVE,S_B,
                      newreference(p^.location.reference),hr);
                   emit_const_reg(A_AND,S_L,$FF,hr);
                 end;
                2:
                 begin
                     emit_ref_reg(A_MOVE,S_W,
                        newreference(p^.location.reference),hr);
                     emit_const_reg(A_AND,S_L,$FFFF,hr);
                 end;
                4:
                 Begin
                     emit_ref_reg(A_MOVE,S_L,
                       newreference(p^.location.reference),hr);
                 end;
               end;

               if target_os.stackalignment=4 then
                 emit_reg_reg(A_MOVE,S_L,hr,R_SPPUSH)
               else
                 { this conserves 2 byte alignment }
                 emit_reg_reg(A_MOVE,S_B,hr,R_SPPUSH);
               ungetregister(hr);
               del_reference(p^.location.reference);
             end;
           end;
         end;
      end;


    { used in conjunction with maybe_push to restore value from the stack }
    procedure restore(p : ptree;isint64 : boolean);
      var
         hregister :  tregister;
      begin
         if (p^.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
           begin
              p^.location.register:=getregister32;
              emit_reg_reg(A_MOVE,S_L,R_SPPULL,p^.location.register);
              if isint64 then
                begin
                   p^.location.registerhigh:=getregister32;
                   emit_reg_reg(A_MOVE,S_L,R_SPPULL,p^.location.registerhigh);
                end;
           end
         else
         if (p^.location.loc in [LOC_REFERENCE,LOC_MEM]) then
           begin
              reset_reference(p^.location.reference);
              p^.location.reference.base:=getaddressreg;
              emit_reg_reg(A_MOVE,S_L,R_SPPULL,p^.location.reference.base);
           end
         else
         { now take care of floating point types }
         if (p^.location.loc = LOC_FPU) then
           begin
             if (cs_fp_emulation in aktmoduleswitches) then
               begin
                 p^.location.fpuregister:=getregister32;
                 emit_reg_reg(A_MOVE,S_L,R_SPPULL,p^.location.fpuregister);
               end
             else
              begin
                 p^.location.fpuregister:=getfloatreg;
                 emit_reg_reg(A_FMOVE,S_FX,R_SPPULL,p^.location.fpuregister);
              end;
           end;
      end;


      procedure push_value_para(p:ptree;inlined,is_cdecl:boolean;
                                para_offset:longint;alignment : longint);

        var
          tempreference : treference;
          r : preference;
          opsize : topsize;
          op : tasmop;
          hreg : tregister;
          size : longint;
          hlabel : pasmlabel;
        begin
          size:=p^.resulttype^.size;
          case p^.location.loc of
             LOC_REGISTER,
             LOC_CREGISTER:
               begin
                  case size of
                     8 :
                        begin
                          inc(pushedparasize,8);
                          if inlined then
                            begin
                               r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                               emit_reg_ref(A_MOVE,S_L,p^.location.registerhigh,r);
                               r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize+4);
                               emit_reg_ref(A_MOVE,S_L,p^.location.registerlow,r);
                            end
                          else
                            begin
                              emit_reg_reg(A_MOVE,S_L,p^.location.registerlow,R_SPPUSH);
                              emit_reg_reg(A_MOVE,S_L,p^.location.registerhigh,R_SPPUSH);
                            end;
                            ungetregister32(p^.location.registerhigh);
                            ungetregister32(p^.location.registerlow);
                        end;
                        4 :
                        begin
                          inc(pushedparasize,4);
                          if inlined then
                           begin
                             r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                             emit_reg_ref(A_MOVE, S_L, p^.location.register,r);
                           end
                          else
                           emit_reg_reg(A_MOVE,S_L,p^.location.register,R_SPPUSH);
                          ungetregister(p^.location.register);
                        end;
                        2 :
                        begin
                          hreg:=p^.location.register;
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
                          if inlined then
                            begin
                              r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                              emit_reg_ref(A_MOVE,opsize,hreg,r);
                            end
                          else
                            emit_reg_reg(A_MOVE,opsize,hreg,R_SPPUSH);
                          ungetregister(p^.location.register);
                        end;
                        1 :
                        begin
                          hreg:=p^.location.register;
                          if alignment=4 then
                            begin
                              opsize:=S_L;
                              inc(pushedparasize,4);
                            end
                          else
                            begin
                              opsize:=S_B;
                              inc(pushedparasize,2);
                            end;
                          { we must push always 16 bit }
                          if inlined then
                            begin
                              r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                              emit_reg_ref(A_MOVE,opsize,hreg,r);
                            end
                          else
                            emit_reg_reg(A_MOVE,opsize,hreg,R_SPPUSH);
                          ungetregister(p^.location.register);
                        end;
                     else internalerror(1899);
                  end;
               end;
             LOC_FPU:
               begin
                  { align the operand correctly to the specified alignment }
                  { requirement.                                           }
                  size:=align(pfloatdef(p^.resulttype)^.size,alignment);
                  inc(pushedparasize,size);
                  if not inlined then
                    begin
                      { optimize a bit }
                      if size < 9 then
                         { subtract quick }
                         emit_const_reg(A_SUBQ,S_L,size,stack_pointer)
                      else
                         emit_const_reg(A_SUB,S_L,size,stack_pointer);
                    end;
{$ifdef GDB}
                  if (cs_debuginfo in aktmoduleswitches) and
                     (exprasmlist^.first=exprasmlist^.last) then
                    exprasmlist^.concat(new(pai_force_line,init));
{$endif GDB}
                  r:=new_reference(stack_pointer,0);
                  if inlined then
                    begin
                       r^.base:=procinfo^.framepointer;
                       r^.offset:=para_offset-pushedparasize;
                    end;
                  floatstore(pfloatdef(p^.resulttype)^.typ,p^.location,r^);
                  { this is the easiest case for inlined !! }
{                  exprasmlist^.concat(new(paicpu,op_ref(op,opsize,r)));}
{                  dec(fpuvaroffset);}
               end;
             LOC_REFERENCE,LOC_MEM:
               begin
                  tempreference:=p^.location.reference;
                  del_reference(p^.location.reference);
                  if (p^.resulttype^.deftype=enumdef) or
                     (p^.resulttype^.deftype=orddef) or
                     ((p^.resulttype^.deftype in [recorddef,objectdef]) and
                      (size<=4) and (size<>3)) then
                      begin
                        case size of
                         8 : begin
                               inc(pushedparasize,8);
                               if inlined then
                                 begin
                                   emit_ref_reg(A_MOVE,S_L,
                                     newreference(tempreference),accumulator);
                                   r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                   emit_reg_ref(A_MOVE,S_L,accumulator,r);
                                   inc(tempreference.offset,4);
                                   emit_ref_reg(A_MOVE,S_L,
                                     newreference(tempreference),accumulator);
                                   r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize+4);
                                   emit_reg_ref(A_MOVE,S_L,accumulator,r);
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
                                   emit_ref_reg(A_MOVE,S_L,
                                     newreference(tempreference),accumulator);
                                   r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                   emit_reg_ref(A_MOVE,S_L,accumulator,r);
                                 end
                               else
                                 emit_push_mem(tempreference);
                             end;
                       1,2 : begin
                               if inlined then
                                begin
                                  if size=1 then
                                    opsize:=S_B
                                  else
                                    opsize:=S_W;

                                  inc(pushedparasize,2);
                                  hreg:=getexplicitregister32(accumulator);
                                  emit_ref_reg(A_MOVE,opsize,
                                    newreference(tempreference),hreg);
                                  if alignment=4 then
                                    begin
                                       inc(pushedparasize,2);
                                       opsize := S_L;
                                    end;
                                  r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                  emit_reg_ref(A_MOVE,opsize,hreg,r);
                                  ungetregister(hreg);
                                end
                               else
                                 begin
                                    if alignment = 2 then
                                      Inc(pushedparasize,2)
                                    else if alignment = 4 then
                                      Inc(pushedparasize,4)
                                    else
                                      internalerror(20021223);
                                    emit_push_mem_size(tempreference,size,alignment);
                                 end;
                             end;
                           0 : ; { record of size zero can exist }
                           else
                             internalerror(234231);
                        end;
                      end
                  else case p^.resulttype^.deftype of
                    { notice here that we do not pass by the floating pointe registers }
                    floatdef :
                      begin
                        case pfloatdef(p^.resulttype)^.typ of
                          f32bit,
                          s32real :
                            begin
                               inc(pushedparasize,4);
                               if inlined then
                                 begin
                                    emit_ref_reg(A_MOVE,S_L,
                                      newreference(tempreference),accumulator);
                                    r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                    emit_reg_ref(A_MOVE,S_L,accumulator,r);
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
                                   emit_ref_reg(A_MOVE,S_L,
                                     newreference(tempreference),accumulator);
                                   r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                   emit_reg_ref(A_MOVE,S_L,accumulator,r);
                                end
                              else
                                emit_push_mem(tempreference);
                              inc(pushedparasize,4);
                              dec(tempreference.offset,4);
                              if inlined then
                                begin
                                   emit_ref_reg(A_MOVE,S_L,
                                     newreference(tempreference),accumulator);
                                   r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                   emit_reg_ref(A_MOVE,S_L,accumulator,r);
                                end
                              else
                                emit_push_mem(tempreference);
                            end;
                          s80real :
                            begin
                              if (cs_fp_emulation in aktmoduleswitches) then
                                begin
                                  inc(pushedparasize,4);
                                  inc(tempreference.offset,4);
                                  if inlined then
                                    begin
                                      emit_ref_reg(A_MOVE,S_L,
                                      newreference(tempreference),accumulator);
                                      r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                      emit_reg_ref(A_MOVE,S_L,accumulator,r);
                                    end
                                  else
                                    emit_push_mem(tempreference);
                                  inc(pushedparasize,4);
                                  dec(tempreference.offset,4);
                                  if inlined then
                                    begin
                                      emit_ref_reg(A_MOVE,S_L,
                                      newreference(tempreference),accumulator);
                                      r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                      emit_reg_ref(A_MOVE,S_L,accumulator,r);
                                    end
                                   else
                                    emit_push_mem(tempreference);
                                end
                              else
                                begin
                                  inc(pushedparasize,4);
                                  if alignment=4 then
                                    inc(tempreference.offset,8)
                                  else
                                    inc(tempreference.offset,6);
                                  if inlined then
                                    begin
                                      emit_ref_reg(A_MOVE,S_L,
                                      newreference(tempreference),accumulator);
                                      r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                      emit_reg_ref(A_MOVE,S_L,accumulator,r);
                                    end
                                  else
                                    emit_push_mem(tempreference);
                                  dec(tempreference.offset,4);
                                  inc(pushedparasize,4);
                                  if inlined then
                                    begin
                                      emit_ref_reg(A_MOVE,S_L,
                                      newreference(tempreference),accumulator);
                                      r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                      emit_reg_ref(A_MOVE,S_L,accumulator,r);
                                    end
                                  else
                                    emit_push_mem(tempreference);
                                  if alignment=4 then
                                    begin
                                      opsize:=S_L;
                                      hreg:=accumulator;
                                      inc(pushedparasize,4);
                                      dec(tempreference.offset,4);
                                    end
                                  else
                                    begin
                                      opsize:=S_W;
                                      hreg:=accumulator;
                                      inc(pushedparasize,2);
                                      dec(tempreference.offset,2);
                                    end;
                                  if inlined then
                                    begin
                                      emit_ref_reg(A_MOVE,opsize,
                                      newreference(tempreference),hreg);
                                      r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                      emit_reg_ref(A_MOVE,opsize,accumulator,r);
                                    end
                                  else
                                      emit_ref_reg(A_MOVE,opsize,newreference(tempreference),
                                        R_SPPUSH);
                                end;
                            end; { end s80real }
                          else
                            internalerror(8954);
                          end; { end case }
                      end;
                    pointerdef,
                    procvardef,
                    classrefdef:
                      begin
                         inc(pushedparasize,4);
                         if inlined then
                           begin
                              emit_ref_reg(A_MOVE,S_L,
                                newreference(tempreference),accumulator);
                              r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                              emit_reg_ref(A_MOVE,S_L,accumulator,r);
                           end
                         else
                           emit_push_mem(tempreference);
                      end;
                    { why we don't do a concatcopy??               }
                    { The reason is very simple - value parameters }
                    { which are bigger then 4 bytes are passed by  }
                    { reference, and the routine itself, copies the }
                    { values to its reserved stack space (see TP manual) }
                    { - Carl }
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
                            ((p^.resulttype^.deftype in [recorddef,objectdef]) and
                             (size=3)) or
                            ((p^.resulttype^.deftype=arraydef) and
                             (parraydef(p^.resulttype)^.IsConstructor or
                              parraydef(p^.resulttype)^.isArrayOfConst or
                              is_open_array(p^.resulttype) or
                             (size<=4))
                            ) or
                            ((p^.resulttype^.deftype=objectdef) and
                             pobjectdef(p^.resulttype)^.is_class) then
                           begin
                              if is_cdecl and (size<=4) then
                                emit_push_mem_size(tempreference,size,alignment)
                              else if (size>2) or
                                 ((alignment=4) and (size>0)) then
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
                                  if size>0 then
                                    begin
                                      inc(pushedparasize,2);
                                      if inlined then
                                        begin
                                          r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                          concatcopy(tempreference,r^,2,false,false);
                                          dispose(r);
                                        end
                                      else
                                        emit_ref_reg(A_MOVE,S_W,newreference(tempreference),R_SPPUSH);
                                    end;
                                end;
                           end
                         { call by value open array ? }
                         else if is_cdecl then
                           begin
                             size:=align(p^.resulttype^.size,alignment);
                             inc(pushedparasize,size);
                             if inlined then
                               begin
                                 r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                 concatcopy(tempreference,r^,size,false,false);
                                 dispose(r);
                               end
                             else
                               begin
                                 { push on stack }
                                 emit_const_reg(A_SUB,S_L,size,stack_pointer);
                                 r:=new_reference(stack_pointer,0);
                                 concatcopy(tempreference,r^,size,false,false);
                                 dispose(r);
                               end;
                           end
                         else
                           internalerror(8954);
                      end
                    else
                      CGMessage(cg_e_illegal_expression);
                  end;
               end;
             LOC_JUMP:
               begin
                  getlabel(hlabel);
                  if (size=4) then
                   begin
                     opsize:=S_L;
                     inc(pushedparasize,4);
                   end
                  else
                   begin
                     if (alignment=4) then
                       begin
                         inc(pushedparasize,2);
                         opsize := S_L;
                       end
                     else
                     if size=2 then
                       opsize:=S_W
                     else
                       opsize:=S_B;
                     inc(pushedparasize,2);
                   end;
                  emitlab(truelabel);
                  if inlined then
                    begin
                       r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                       emit_const_ref(A_MOVE,opsize,1,r);
                    end
                  else
                      emit_const_reg(A_MOVE,opsize,1,R_SPPUSH);
                  emitjmp(C_None,hlabel);
                  emitlab(falselabel);
                  if inlined then
                    begin
                       r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                       emit_ref(A_CLR,opsize,r);
                    end
                  else
                      emit_const_reg(A_MOVE,opsize,0,R_SPPUSH);
                  emitlab(hlabel);
               end;
             LOC_FLAGS:
               begin
                  { why in accumulator always? }
                  if not(accumulator in unused) then
                    begin
                      emit_reg_reg(A_MOVE,S_L,accumulator,scratch_reg);
                    end;
                  emit_flag2reg(p^.location.resflags,accumulator);
                  { zero extend }
                  emit_const_reg(A_AND,S_L,$ff,accumulator);
                  if  (size=4) then
                   begin
                     opsize:=S_L;
                     hreg:=accumulator;
                     inc(pushedparasize,4);
                   end
                  else
                   begin
                     if (alignment=4) then
                       begin
                         opsize := S_L;
                         inc(pushedparasize,2);
                       end
                     else
                     if size=2 then
                       opsize:=S_W
                     else
                       opsize:=S_B;
                     hreg:=accumulator;
                     inc(pushedparasize,2);
                   end;
                  if inlined then
                    begin
                       r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                       emit_reg_ref(A_MOVE,opsize,hreg,r);
                    end
                  else
                    emit_reg_reg(A_MOVE,opsize,hreg,R_SPPUSH);
                  if not(accumulator in unused) then
                    begin
                      emit_reg_reg(A_MOVE,S_L,scratch_reg,accumulator);
                    end;
               end;
          end;
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
                                        if isaddressregister(p^.location.register) then
                                          internalerror(1212);
                                        emit_reg(A_TST,opsize,p^.location.register);
                                        ungetregister(p^.location.register);
                                        emitlabeled(A_BNE,truelabel);
                                        emitjmp(C_None,falselabel);
                                     end;
                      LOC_MEM,LOC_REFERENCE : begin
                                        emit_ref(A_TST,opsize,newreference(p^.location.reference));
                                        del_reference(p^.location.reference);
                                        emitlabeled(A_BNE,truelabel);
                                        emitjmp(C_None,falselabel);
                                     end;
                      LOC_FLAGS : begin
                                     emitlabeled(flag_2_jmp[p^.location.resflags],truelabel);
                                     {if tf_code_small in target_info.flags then
                                       emitlabeled(A_BRA,falselabel)
                                     else
                                       emitlabeled(A_JMP,falselabel); }
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
           emitlabeled(A_BVC,hl)
         else
           emitlabeled(A_BCC,hl);
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
                 hreg := accumulator;
                 href := newreference(p^.location.reference);
                 { inc(href^.offset,4); high part is at offset zero
                   as m68k is a big endian cpu PM }

                 emit_ref_reg(A_MOVE,S_L,href,hreg);
               end;
             getlabel(poslabel);

             { check high dword, must be 0 (for positive numbers) }
             emit_reg(A_TST,S_L,hreg);
             emitlabeled(A_BEQ,poslabel);

             { It can also be $ffffffff, but only for negative numbers }
             if from_signed and to_signed then
               begin
                 getlabel(neglabel);
                 emit_const_reg(A_CMP,S_L,$ffffffff,hreg);
                 emitlabeled(A_BEQ,neglabel);
               end;
             { For all other values we have a range check error }
             emitcall('FPC_RANGEERROR');

             { if the high dword = 0, the low dword can be considered a }
             { simple cardinal                                          }
             emitlab(poslabel);
             new(hdef,init(u32bit,0,$ffffffff));
             { the real p^.resulttype is already saved in fromdef }
             p^.resulttype := hdef;
             if (p^.location.loc in [LOC_MEM,LOC_REFERENCE]) then
               inc(p^.location.reference.offset,4);
             emitrangecheck(p,todef);
             dispose(hdef,done);
             { restore original resulttype }
             if (p^.location.loc in [LOC_MEM,LOC_REFERENCE]) then
               dec(p^.location.reference.offset,4);
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
                     hreg := accumulator;
                     emit_ref_reg(A_MOVE,S_L,
                       newreference(p^.location.reference),hreg);
                   end;
                 { get a new neglabel (JM) }
                 getlabel(neglabel);
                 emit_reg(A_TST,S_L,hreg);
                 emitlabeled(A_BLT,neglabel);

                 emitcall('FPC_RANGEERROR');

                 { if we get here, the 64bit value lies between }
                 { longint($80000000) and -1 (JM)               }
                 emitlab(neglabel);
                 new(hdef,init(s32bit,$80000000,-1));
                 p^.resulttype := hdef;
                 if (p^.location.loc in [LOC_MEM,LOC_REFERENCE]) then
                   inc(p^.location.reference.offset,4);
                 emitrangecheck(p,todef);
                 if (p^.location.loc in [LOC_MEM,LOC_REFERENCE]) then
                   dec(p^.location.reference.offset,4);
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
                   hreg := accumulator;
                   href := newreference(p^.location.reference);
                   {if p^.resulttype^.size = 8 then
                     inc(href^.offset,4); wrong PM }
                   case p^.resulttype^.size of
                     1:
                        Begin
                         emit_ref_reg(A_MOVE,S_B,href,hreg);
                         { sign extend the stuff }
                         if from_signed then
                           Begin
                           { byte to long }
                            if aktoptprocessor = MC68020 then
                                exprasmlist^.concat(new(paicpu,op_reg(A_EXTB,S_L,hreg)))
                            else
                              begin
                                exprasmlist^.concat(new(paicpu,op_reg(A_EXT,S_W,hreg)));
                                exprasmlist^.concat(new(paicpu,op_reg(A_EXT,S_L,hreg)));
                              end;
                           end
                         else
                           Begin
                             { zero extend the stuff }
                             emit_const_reg(A_AND,S_L,$00ff,hreg);
                           End;
                        End;
                     2: Begin
                         emit_ref_reg(A_MOVE,S_W,href,hreg);
                         { sign extend }
                         if from_signed then
                           { word to long }
                           emit_reg(A_EXT,S_L,hreg)
                         else
                           { zero extend the stuff }
                           emit_const_reg(A_AND,S_L,$ffff,hreg);
                         End;
                     4,8: Begin
                            emit_ref_reg(A_MOVE,S_L,href,hreg);
                          End;
                   end;
                 end;
               getlabel(poslabel);
               emit_reg(A_TST,S_L,hreg);
               emitlabeled(A_BGE,poslabel);
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
        is_reg : boolean;
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
        { FPC_BOUNDCHECK needs to be called with
            %d0 - value
            %a1 - pointer to the ranges
        }
        getexplicitregister32(accumulator);
        if fromdef^.deftype=orddef then
          emitloadord2reg(p^.location, porddef(fromdef), accumulator, false)
        else
          emit_load_loc_reg(p^.location, fromdef, s32bitdef, accumulator);
        if doublebound then
          begin
            getlabel(neglabel);
            getlabel(poslabel);
            emit_reg(A_TST,S_L,accumulator);
            emitlabeled(A_BLT,neglabel);
          end;
        getexplicitregister32(R_A1);
        exprasmlist^.concat(new(paicpu,op_sym_ofs_reg(A_LEA,S_L,newasmsymbol(rstr),0,
           R_A1)));
        emitcall('FPC_BOUNDCHECK');
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
                  exprasmlist^.concat(new(paicpu,
                    op_sym_ofs_reg(A_LEA,S_L,newasmsymbol(rstr),
                    8,R_A1)));
                  emitcall('FPC_BOUNDCHECK');
              end;
            emitlab(poslabel);
         end;
        ungetregister(R_A1);
        ungetregister(accumulator);
      end;


    { USES REGISTERS R_A0 AND R_A1 }
    { maximum size of copy is 65535 bytes                                       }
    procedure concatcopy(source,dest : treference;size : longint;delsource : boolean; loadref:boolean);

      var
         ecxpushed : boolean;
         helpsize,storeoffset : longint;
         i : byte;
         reg8,reg32 : tregister;
         swap : boolean;
         hregister : tregister;
         iregister : tregister;
         jregister : tregister;
         hp1 : treference;
         hp2 : treference;
         hl : pasmlabel;
         hl2: pasmlabel;
         popaddress : boolean;

      begin
         popaddress := false;
         { this should never occur }
         if size > 65535 then
           internalerror(0);
         hregister := getregister32;
         if delsource then
           del_reference(source);
         storeoffset:=source.offset;

         { from 12 bytes movs is being used }
         if (not loadref) and ((size<=8) or (not(cs_littlesize in aktglobalswitches) and (size<=12))) then
           begin
              helpsize:=size div 4;
              { move a dword x times }
              for i:=1 to helpsize do
                begin
                   emit_ref_reg(A_MOVE,S_L,newreference(source),hregister);
                   emit_reg_ref(A_MOVE,S_L,hregister,newreference(dest));
                   inc(source.offset,4);
                   inc(dest.offset,4);
                   dec(size,4);
                end;
              { move a word }
              if size>1 then
                begin
                   emit_ref_reg(A_MOVE,S_W,newreference(source),hregister);
                   emit_reg_ref(A_MOVE,S_W,hregister,newreference(dest));
                   inc(source.offset,2);
                   inc(dest.offset,2);
                   dec(size,2);
                end;
              { move a single byte }
              if size>0 then
                begin
                  emit_ref_reg(A_MOVE,S_B,newreference(source),hregister);
                  emit_reg_ref(A_MOVE,S_B,hregister,newreference(dest));
                end

           end
         else
           begin
              if (usableaddress > 1) then
                begin
                    iregister := getaddressreg;
                    jregister := getaddressreg;
                end
              else
                begin
                    { save a0 and a1 }
                    emit_reg_reg(A_MOVE,S_L,R_A0,R_SPPUSH);
                    emit_reg_reg(A_MOVE,S_L,R_A1,R_SPPUSH);
                    popaddress := true;
                    iregister := R_A0;
                    jregister := R_A1;
                end;
              { reference for move (An)+,(An)+ }
              reset_reference(hp1);
              hp1.base := iregister;   { source register }
              hp1.direction := dir_inc;
              reset_reference(hp2);
              hp2.base := jregister;
              hp2.direction := dir_inc;
              { iregister = source }
              { jregister = destination }

              if loadref then
                 emit_ref_reg(A_MOVE,S_L,newreference(source),iregister)
              else
                 emit_ref_reg(A_LEA,S_L,newreference(source),iregister);
              emit_ref_reg(A_LEA,S_L,newreference(dest),jregister);

              { double word move only on 68020+ machines }
              { because of possible alignment problems   }
              { use fast loop mode }
              if (aktoptprocessor=MC68020) then
                begin
                   helpsize := size - size mod 4;
                   size := size mod 4;
                   emit_const_reg(A_MOVE,S_L,helpsize div 4,hregister);
                   getlabel(hl2);
                   emitlabeled(A_BRA,hl2);
                   getlabel(hl);
                   emitlab(hl);
                   emit_ref_ref(A_MOVE,S_L,newreference(hp1),newreference(hp2));
                   emitlab(hl2);
                   exprasmlist^.concat(new(pai_labeled, init_reg(A_DBRA,hl,hregister)));
                   if size > 1 then
                     begin
                        dec(size,2);
                        emit_ref_ref(A_MOVE,S_W,newreference(hp1), newreference(hp2));
                     end;
                   if size = 1 then
                    emit_ref_ref(A_MOVE,S_B,newreference(hp1), newreference(hp2));
                end
              else
                begin
                   { Fast 68010 loop mode with no possible alignment problems }
                   helpsize := size;
                   emit_const_reg(A_MOVE,S_L,helpsize,hregister);
                   getlabel(hl2);
                   emitlabeled(A_BRA,hl2);
                   getlabel(hl);
                   emitlab(hl);
                   emit_ref_ref(A_MOVE,S_B,newreference(hp1),newreference(hp2));
                   emitlab(hl2);
                   exprasmlist^.concat(new(pai_labeled, init_reg(A_DBRA,hl,hregister)));
                end;
              if popaddress then
                begin
                  { restor a0 and a1 }
                  emit_reg_reg(A_MOVE,S_L,R_SPPULL,R_A0);
                  emit_reg_reg(A_MOVE,S_L,R_SPPULL,R_A1);
                end;

       { restore the registers that we have just used olny if they are used! }
              if jregister = R_A1 then
                hp2.base := R_NO;
              if iregister = R_A0 then
                hp1.base := R_NO;
              del_reference(hp1);
              del_reference(hp2);
           end;

           { loading SELF-reference again }
           maybe_loadself;

           if delsource then
             begin
               source.offset:=storeoffset;
               ungetiftemp(source);
             end;

           ungetregister32(hregister);
    end;


    { zero / sign extend to 32-bit register }
    procedure emitloadord2reg(const location:Tlocation;srcdef:Porddef;
                              destreg:Tregister;delloc:boolean);


    var r:Preference;
        srctyp : tbasetype;
    begin
        if not (srcdef^.deftype in [orddef,enumdef]) then
          internalerror(444);
        if (srcdef^.deftype = orddef) then
          srctyp:= srcdef^.typ
        else { enumdef type }
          begin
            case srcdef^.size of
              4 :  srctyp:=u32bit;
              2 :  srctyp:=u16bit;
              1 :  srctyp:=u8bit;
            else
              internalerror(255);
            end;
          end;
        case location.loc of
            LOC_REGISTER,LOC_CREGISTER:
                begin
                    case srctyp of
                        u8bit,uchar,bool8bit: begin
                                 if location.register <> destreg then
                                  begin
                                     emit_reg(A_CLR,S_L,destreg);
                                     emit_reg_reg(A_MOVE,S_B,location.register,destreg);
                                  end
                                 else
                                     emit_const_reg(A_AND,S_B,$FF,destreg);
                               end;
                        s8bit: begin
                                 emit_reg_reg(A_MOVE,S_B,location.register,destreg);
                                 if (aktoptprocessor <> MC68020) then
                                  begin
                                     { byte to word }
                                     emit_reg(A_EXT,S_W,destreg);
                                     { word to long }
                                     emit_reg(A_EXT,S_L,destreg);
                                  end
                                 else { 68020+ and later only }
                                     emit_reg(A_EXTB,S_L,destreg);
                                end;
                        u16bit,uwidechar,bool16bit: begin
                                  if location.register <> destreg then
                                    begin
                                      emit_reg(A_CLR,S_L,destreg);
                                      emit_reg_reg(A_MOVE,S_W,location.register,destreg);
                                    end
                                  else
                                     emit_const_reg(A_AND,S_L,$FFFF,destreg);
                                end;
                        s16bit: begin
                                 emit_reg_reg(A_MOVE,S_W,location.register,destreg);
                                 { word to long }
                                 emit_reg(A_EXT,S_L,destreg);
                                end;
                        u32bit,bool32bit:
                            emit_reg_reg(A_MOVE,S_L,location.register,destreg);
                        s32bit:
                            emit_reg_reg(A_MOVE,S_L,location.register,destreg);
                        else
                          internalerror(330);
                    end;
                    if delloc then
                        ungetregister(location.register);
                end;
            LOC_REFERENCE,LOC_MEM:
                begin
                    if location.reference.is_immediate then
                     emit_const_reg(A_MOVE,S_L,location.reference.offset,destreg)
                    else
                      begin
                        r:=newreference(location.reference);
                        case srctyp of
                        u8bit,uchar,bool8bit: begin
                                 emit_ref_reg(A_MOVE,S_B,r,destreg);
                                 emit_const_reg(A_AND,S_L,$FF,destreg);
                               end;
                        s8bit:  begin
                                 emit_ref_reg(A_MOVE,S_B,r,destreg);
                                 if (aktoptprocessor <> MC68020) then
                                  begin
                                     { byte to word }
                                     emit_reg(A_EXT,S_W,destreg);
                                     { word to long }
                                     emit_reg(A_EXT,S_L,destreg);
                                  end
                                 else { 68020+ and later only }
                                     emit_reg(A_EXTB,S_L,destreg);
                                end;
                        u16bit,uwidechar,bool16bit: begin
                                 emit_ref_reg(A_MOVE,S_W,r,destreg);
                                 emit_const_reg(A_AND,S_L,$FFFF,destreg);
                                end;
                        s16bit: begin
                                 emit_ref_reg(A_MOVE,S_W,r,destreg);
                                 { word to long }
                                 emit_reg(A_EXT,S_L,destreg);
                                end;
                        u32bit,bool32bit:
                            emit_ref_reg(A_MOVE,S_L,r,destreg);
                        s32bit:
                            emit_ref_reg(A_MOVE,S_L,r,destreg);
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
              exprasmlist^.concat(new(pairegalloc,alloc(self_pointer)));
              if lexlevel>normal_function_level then
                begin
                   new(hp);
                   reset_reference(hp^);
                   hp^.offset:=procinfo^.framepointer_offset;
                   hp^.base:=procinfo^.framepointer;
                   emit_ref_reg(A_MOVE,S_L,hp,self_pointer);
                   p:=procinfo^.parent;
                   for i:=3 to lexlevel-1 do
                     begin
                        new(hp);
                        reset_reference(hp^);
                        hp^.offset:=p^.framepointer_offset;
                        hp^.base:=self_pointer;
                        emit_ref_reg(A_MOVE,S_L,hp,self_pointer);
                        p:=p^.parent;
                     end;
                   new(hp);
                   reset_reference(hp^);
                   hp^.offset:=p^.selfpointer_offset;
                   hp^.base:=self_pointer;
                   emit_ref_reg(A_MOVE,S_L,hp,self_pointer);
                end
              else
                begin
                   new(hp);
                   reset_reference(hp^);
                   hp^.offset:=procinfo^.selfpointer_offset;
                   hp^.base:=procinfo^.framepointer;
                   emit_ref_reg(A_MOVE,S_L,hp,self_pointer);
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
                (p^.left^.registers32<p^.right^.registers32))) then
            begin
              hp:=p^.left;
              p^.left:=p^.right;
              p^.right:=hp;
              p^.swaped:=not p^.swaped;
            end;
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
      getlabel(pl);
    end;


    procedure generate_interrupt_stackframe_entry;
      begin
      end;


    procedure generate_interrupt_stackframe_exit;
      begin
      end;


  { generates the code for threadvar initialisation }
  procedure initialize_threadvar(p : pnamedindexobject);{$ifndef FPC}far;{$endif}

    var
       hr : treference;

    begin
       if (psym(p)^.typ=varsym) and
          (vo_is_thread_var in pvarsym(p)^.varoptions) then
         begin
            emit_const_reg(A_MOVE,S_L,pvarsym(p)^.getsize,R_SPPUSH);
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
              emit_ref(A_CLR,S_L,
                newreference(ref));
           end
         else
           begin
              reset_reference(hr);
              hr.symbol:=t^.get_inittable_label;
              emitpushreferenceaddr(hr);
              if is_already_ref then
                emit_ref_reg(A_MOVE,S_L,
                  newreference(ref),R_SPPUSH)
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
                exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,
                  newreference(ref),R_SPPUSH)))
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
      hl,hl2 : pasmlabel; { label helpers }
      src, dst : treference;
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
                highaddress:=pvarsym(p)^.address-target_os.size_of_pointer+procinfo^.para_offset
              else
                highaddress:=pvarsym(p)^.address+target_os.size_of_pointer+procinfo^.para_offset;
              r^.offset:=highaddress;

              getexplicitregister32(R_D0);
              exprasmlist^.concat(new(paicpu,
                op_ref_reg(A_MOVE,S_L,r,R_D0)));

              { why do we increment the pointer? }
              exprasmlist^.concat(new(paicpu,
                op_const_reg(A_ADDQ,S_L,1,R_D0)));

              if (parraydef(pvarsym(p)^.vartype.def)^.elesize<>1) then
               begin
                 if ispowerof2(parraydef(pvarsym(p)^.vartype.def)^.elesize, power) then
                   begin
                     if (power > 0) and (power < 9) then
                        emit_const_reg(A_LSL,S_L,power,R_D0)
                     else
                     if (power <> 0) then
                       begin
                        emit_const_reg(A_MOVE,S_L,power,R_D1);
                        emit_reg_reg(A_LSL,S_L,R_D1,R_D0);
                       end;
                   end
                 else
                   begin
                     len := parraydef(pvarsym(p)^.vartype.def)^.elesize;
                     { use normal MC68000 signed multiply }
                     if (len >= -32768) and (len <= 32767) then
                        emit_const_reg(A_MULS,S_W,len,R_D0)
                     else
                     { use long MC68020 long multiply }
                     if (aktoptprocessor = MC68020) then
                        emit_const_reg(A_MULS,S_L,len,R_D0)
                     else
                     { MC68000 long multiply }
                       begin
                         getexplicitregister32(R_D1);

                         emit_const_reg(A_MOVE,S_L,len,R_D1);
                         emitcall('FPC_MUL_LONGINT');
                         { result in D0 }

                         ungetregister(R_D1);
                       end;
                   end;
               end;
              { this indicates the number of bytes to }
              { reserve on the stack.                 }
                exprasmlist^.concat(new(paicpu,
                  op_reg_reg(A_SUB,S_L,R_D0,stack_pointer)));

              { load destination }
              exprasmlist^.concat(new(paicpu,
                op_reg_reg(A_MOVE,S_L,stack_pointer,R_A0)));

              { load count }
              new(r);
              reset_reference(r^);
              r^.base:=procinfo^.framepointer;
              r^.offset:=highaddress;
              emit_ref_reg(A_MOVE,S_L,r,R_D0);

              { load source }
              new(r);
              reset_reference(r^);
              r^.base:=procinfo^.framepointer;
              r^.offset:=pvarsym(p)^.address+procinfo^.para_offset;
              emit_ref_reg(A_MOVE,S_L,r,R_A1);

              { scheduled .... one more than actual }
              emit_const_reg(A_ADDQ,S_L,1,R_D0);

              { calculate size }
              len:=parraydef(pvarsym(p)^.vartype.def)^.elesize;

              opsize:=S_B;
              { adjust length copy according to element size }
              if ispowerof2(len, power) then
                begin
                  if (power > 0) and (power < 9) then
                    emit_const_reg(A_LSL,S_L,power,R_D0)
                  else
                  { don't shift when shift amount = 0 }
                  if (power <> 0) then
                    begin
                      emit_const_reg(A_MOVE,S_L,power,R_D1);
                      emit_reg_reg(A_LSL,S_L,R_D1,R_D0);
                    end;
                end
              else
                begin
                     { use normal MC68000 signed multiply }
                     if (len >= -32768) and (len <= 32767) then
                        emit_const_reg(A_MULS,S_W,len,R_D0)
                     else
                     { use long MC68020 long multiply }
                     if (aktoptprocessor = MC68020) then
                        emit_const_reg(A_MULS,S_L,len,R_D0)
                     else
                     { MC68000 long multiply }
                       begin
                         getexplicitregister32(R_D1);

                         emit_const_reg(A_MOVE,S_L,len,R_D1);
                         emitcall('FPC_MUL_LONGINT');
                         { result in D0 }

                         ungetregister(R_D1);
                       end;
                end;


              { reference for move (An)+,(An)+ }
              reset_reference(src);
              src.base := R_A1;   { source register }
              src.direction := dir_inc;
              reset_reference(dst);
              dst.base := R_A0;
              dst.direction := dir_inc;

              getlabel(hl2);
              emitlabeled(A_BRA,hl2);
              getlabel(hl);
              emitlab(hl);
              emit_ref_ref(A_MOVE,S_B,newreference(src),newreference(dst));
              emitlab(hl2);
              exprasmlist^.concat(new(pai_labeled, init_reg(A_DBRA,hl,R_D0)));

              { patch the new address }
              new(r);
              reset_reference(r^);
              r^.base:=procinfo^.framepointer;
              r^.offset:=pvarsym(p)^.address+procinfo^.para_offset;
              exprasmlist^.concat(new(paicpu,
                op_reg_ref(A_MOVE,S_L,stack_pointer,r)));

              ungetregister(R_D0);
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
              emit_ref(A_CLR,S_L,r);
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
    end;


  { this returns TRUE if the one of the following registers }
  { is used as the return value of the routine.             }
  { take cares of emulation (SINGLE are always returned in  }
  { general 32-bit register).                               }
  procedure getusedregs(var uses_acc : boolean; var uses_fpu : boolean;
     var uses_scratch : boolean; var uses_a0 : boolean);
    begin
      uses_scratch := false;
      uses_acc := false;
      uses_fpu := false;
      uses_a0 := false;

      if procinfo^.returntype.def<>pdef(voiddef) then
          begin
              if (procinfo^.returntype.def^.deftype in [orddef,enumdef]) then
                begin
                  uses_acc:=true;
                  if procinfo^.returntype.def^.size = 8 then
                        uses_scratch:=true;
                end
              else
                if ret_in_acc(procinfo^.returntype.def,procinfo^.def^.proccalloptions) then
                  begin
                    uses_acc:=true;
                    if (procinfo^.returntype.def^.deftype = pointerdef) and
                    (pocall_cdecl in aktprocsym^.definition^.proccalloptions)
                    then
                      uses_a0:=true;
                  end
              else
                 if (procinfo^.returntype.def^.deftype=floatdef) then
                   begin
                      if cs_fp_emulation in aktmoduleswitches then
                        begin
                          { indicate that the accumulator is being }
                          { used.                                  }
                          uses_acc := true;
                        end
                      else
                        begin
                          case pfloatdef(procinfo^.returntype.def)^.typ of
                          s32real:
                              begin
                                  uses_fpu := true;
                              end;
                          s64real:
                              begin
                                  uses_fpu := true;
                              end;
                          s80real:
                              begin
                                  uses_fpu := true;
                              end;
                          else
                            internalerror(3535);
                          end;
                        end;
                   end;
          end;
    end;



  procedure genentrycode(alist : paasmoutput;const proc_names:Tstringcontainer;make_global:boolean;
                         stackframe:longint;
                         var parasize:longint;var nostackframe:boolean;
                         inlined : boolean;var savedstackoffset : longint);
  {
    Generates the entry code for a procedure

    WARNING : Do not use the emit_x_x macros here, since we do not
    CONCAT instructions but INSERT them.

    ALL INSERTS ARE DONE IN REVERSE ORDER !
  }

    var
      hs : string;
{$ifdef GDB}
      stab_function_name : Pai_stab_function_name;
{$endif GDB}
      hr : preference;
      hreg : tregister;
      p : psymtable;
      r : treference;
      oldlist,
      oldexprasmlist : paasmoutput;
      hlb,classok : pasmlabel;
      i : longint;
      uses_acc : boolean;
      uses_scratch : boolean;
      uses_a0 : boolean;
      uses_fpu : boolean;
      tosave : tregisterset;
      link_op_size : topsize;  { opcode size for link instruction }
    begin
       oldexprasmlist:=exprasmlist;
       exprasmlist:=alist;
       if (not inlined) and (aktprocsym^.definition^.proctypeoption=potype_proginit) then
           begin
              emitinsertcall('FPC_INITIALIZEUNITS');
              if target_info.target=target_M68K_AMIGA then
                begin
                   new(hr);
                   reset_reference(hr^);
                   hr^.symbol:=newasmsymbol(
                   'U_SYSAMIGA_ISCONSOLE');
                   if apptype=at_cui then
                     exprasmlist^.insert(new(paicpu,op_const_ref(A_MOVE,S_B,
                       1,hr)))
                   else
                     exprasmlist^.insert(new(paicpu,op_ref(A_CLR,S_B,hr)));
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
          { constructor returns SELF in accumulator }
          if procinfo^._class^.is_class then
            begin
              procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
              if tf_code_small in target_info.flags then
                exprasmlist^.insert(new(pai_labeled,init(A_BEQ,faillabel)))
              else
                begin
                  getlabel(classok);
                  exprasmlist^.insert(new(pai_label,init(classok)));
                  exprasmlist^.insert(new(pai_labeled,init(A_JMP,faillabel)));
                  exprasmlist^.insert(new(pai_labeled,init(A_BNE,classok)));
                end;
              emitinsertcall('FPC_NEW_CLASS');
            end
          else
            begin
              if tf_code_small in target_info.flags then
                exprasmlist^.insert(new(pai_labeled,init(A_BEQ,faillabel)))
              else
                begin
                  getlabel(classok);
                  exprasmlist^.insert(new(pai_label,init(classok)));
                  exprasmlist^.insert(new(pai_labeled,init(A_JMP,faillabel)));
                  exprasmlist^.insert(new(pai_labeled,init(A_BNE,classok)));
                end;
              exprasmlist^.insert(new(paicpu,op_reg(A_TST,S_L,R_D0)));
              exprasmlist^.insert(new(paicpu,op_reg_reg(A_MOVE,S_L,R_D0,self_pointer)));
              emitinsertcall('FPC_HELP_CONSTRUCTOR');
              { Using the generic version, so we need to push all args on stack }
              new(hr);
              reset_reference(hr^);
              hr^.offset:=procinfo^.selfpointer_offset;
              hr^.base:=procinfo^.framepointer;
              {hreg := getaddressreg;
              exprasmlist^.insert(new(paicpu,op_ref_reg(A_LEA,S_L,hr,hreg))); }
              exprasmlist^.insert(new(paicpu,op_ref(A_PEA,S_L,hr)));
              {exprasmlist^.insert(new(paicpu,op_reg_reg(A_MOVE,S_L,hreg,R_SPPUSH)));
              ungetregister(hreg);}
              { We new to push the VMT that was sent by the caller
                as otherwise we change the vmt field when calling
                constructors inside constructors PM }
              new(hr);
              reset_reference(hr^);
              hr^.offset:=procinfo^.selfpointer_offset-4;
              hr^.base:=procinfo^.framepointer;
              exprasmlist^.insert(new(paicpu,op_ref(A_PEA,S_L,hr)));
              exprasmlist^.insert(new(paicpu,op_const_reg
                 (A_MOVE,S_L,procinfo^._class^.vmt_offset,R_SPPUSH)));
            end;
        end;

      { don't load ESI, does the caller }
      { we must do it for local function }
      { that can be called from a foreach }
      { of another object than self !! PM }

         if assigned(procinfo^._class) and
            (lexlevel>normal_function_level) then
           maybe_loadself;

      { cdecl'ared function have the return buffer address in A1 (or A0), not on stack }
      if (pocall_cdecl in aktprocsym^.definition^.proccalloptions) and
         ret_in_param(procinfo^.returntype.def,procinfo^.def^.proccalloptions) then
  begin
          reset_reference(r);
          r.offset:=procinfo^.return_offset;
          r.base:=procinfo^.framepointer;
          exprasmlist^.insert(new(paicpu,op_reg_ref(A_MOVE,S_L,cdecl_function_return_address_reg,
            newreference(r))));
        end;
      getusedregs(uses_acc,uses_fpu,uses_scratch,uses_a0);
      { When message method contains self as a parameter,
        we must load it into ESI }
      If (po_containsself in aktprocsym^.definition^.procoptions) then
        begin
           new(hr);
           reset_reference(hr^);
           hr^.offset:=procinfo^.selfpointer_offset;
           hr^.base:=procinfo^.framepointer;
           exprasmlist^.insert(new(paicpu,op_ref_reg(A_MOVE,S_L,hr,self_pointer)));
           exprasmlist^.insert(new(pairegalloc,alloc(self_pointer)));
        end;
      if not inlined and (procinfo^.framepointer<>R_SP) and
         ((po_savestdregs in aktprocsym^.definition^.procoptions) or
         (po_saveregisters in aktprocsym^.definition^.procoptions)) then
        begin
          inc(stackframe,4);
          savedstackoffset:=stackframe;
          new(hr);
          reset_reference(hr^);
          hr^.offset:=-savedstackoffset;
          hr^.base:=procinfo^.framepointer;
          exprasmlist^.insert(new(paicpu,op_reg_ref(A_MOVE,S_L,R_SP,hr)));
        end
      else
        savedstackoffset:=-1;
      { should we save edi,esi,ebx like C ? }
      if (po_savestdregs in aktprocsym^.definition^.procoptions) then
       begin
         {!!!!!!!!!!!!!!!!!!!!!!!!!!}
         tosave:=[R_D2..R_D7,R_A2..R_A5];
         { should be intersected with used regs, no ?}
         tosave:=tosave*usedinproc;
         if tosave<>[] then
           exprasmlist^.insert(new(paicpu,op_reglist_reg(A_MOVEM,S_L,tosave,R_SPPUSH)));
       end;

      if (po_saveregisters in aktprocsym^.definition^.procoptions) then
        begin
           { save all registers including floating point registers }
          if not ((cs_fp_emulation) in aktmoduleswitches) then
            begin
              { verify that if this is a function, it returns }
              { a value which requires the FP0 float register }
              { if so then ... don't save that register,      }
              { otherwise save every FPU register.            }
              if uses_fpu then
                  tosave := [R_FP1..R_FP7] { exclude return value }
              else
                  tosave := [R_FP0..R_FP7]; { include FP0 - no return in FP0 }
              exprasmlist^.insert(new(paicpu,op_reglist_reg(A_FMOVEM,S_NO,tosave,R_SPPUSH)));
            end;
          tosave := [R_D0..R_SP];
          { now verify if D1 or D0 are used }
          if uses_acc then
            tosave := tosave - [accumulator];  { don't save accumulator }
          if uses_scratch then
            tosave := tosave - [scratch_reg];  { don't save scratch }
          if uses_a0 then
            tosave := tosave - [R_A0];  { don't save A0 }
          { constructor returns SELF in accumulator }
          { and also uses SELF, so no save of SELF  }
          if (aktprocsym^.definition^.proctypeoption=potype_constructor) then
            tosave := tosave - [accumulator] - [self_pointer];
          exprasmlist^.insert(new(paicpu,op_reglist_reg(A_MOVEM,S_L,tosave,R_SPPUSH)));
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
                  op_const_reg(A_SUB,S_L,stackframe,stack_pointer)));
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
                    if ((stackframe < -32767) or (stackframe > 32768)) then
                      begin
                        { On the 68000 CPU, LINK can only specify a signed
                          16-bit displacement, then in that case give out
                          an error
                        }
                        if (aktoptprocessor = MC68000) then
                           CGMessage(cg_e_stacklimit_in_local_routine);
                        link_op_size := S_L;
                      end
                    else
                        link_op_size := S_W;


                    if cs_profile in aktmoduleswitches then
                       genprofilecode;


                    { with stack checking enalbed, we must do it manually and
                      we cannot use the link instruction
                    }
                    if (cs_check_stack in aktlocalswitches) then
                      begin
                       exprasmlist^.insert(new(paicpu,op_const_reg(A_SUB,S_L,stackframe,stack_pointer)));
                       if not(target_info.target in [target_m68k_linux,target_m68k_netbsd]) and
                          (aktprocsym^.definition^.proctypeoption<>potype_proginit) then
                        begin
                          emitinsertcall('FPC_STACKCHECK');
                          exprasmlist^.insert(new(paicpu,op_const_reg(A_MOVE,S_L,stackframe,R_SPPUSH)));
                        end;
                       exprasmlist^.insert(new(paicpu,op_reg_reg(A_MOVE,S_L,stack_pointer,frame_pointer)));
                       exprasmlist^.insert(new(paicpu,op_reg_reg(A_MOVE,S_L,frame_pointer,R_SPPUSH)));
                      end
                    else
                       exprasmlist^.insert(new(paicpu,op_reg_const(A_LINK,link_op_size,frame_pointer,-stackframe)));


                  end { endif stackframe <> 0 }
              else
                 begin
                   if cs_profile in aktmoduleswitches then
                     genprofilecode;
                   exprasmlist^.insert(new(paicpu,op_reg_reg(A_MOVE,S_L,stack_pointer,frame_pointer)));
                   exprasmlist^.insert(new(paicpu,op_reg_reg(A_MOVE,S_L,frame_pointer,R_SPPUSH)));
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
            usedinproc:=usedinproc + [R_D0];

            { Type of stack-frame must be pushed}
            emit_const_reg(A_MOVE,S_L,1,R_SPPUSH);
            emitcall('FPC_PUSHEXCEPTADDR');
            emit_reg_reg(A_MOVE,S_L,R_D0,R_SPPUSH);
            emitcall('FPC_SETJMP');
            { The move instruction automatically sets up
              the zero flag, when the destination is not
              an address register. - carl
            }
            emit_reg_reg(A_MOVE,S_L,R_D0,R_SPPUSH);
            if tf_code_small in target_info.flags then
              emitlabeled(A_BNE,aktexitlabel)
            else
              begin
                getlabel(hlb);
                emitlabeled(A_BEQ,hlb);
                emitlabeled(A_JMP,aktexitlabel);
                emitlab(hlb);
              end;
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
          exprasmlist^.insert(new(pai_align,init(2)))
         else
          if not(cs_littlesize in aktglobalswitches) then
           exprasmlist^.insert(new(pai_align,init(4)));
       end;
      exprasmlist:=oldexprasmlist;
  end;


  procedure handle_return_value(inlined : boolean;var uses_acc,uses_scratch,uses_a0,
    uses_fpu : boolean);
    var
       hr : preference;
       op : Tasmop;
       s : Topsize;
  begin
      uses_acc:=false;
      uses_scratch:=false;
      uses_fpu := false;
      uses_a0 := false;
      if procinfo^.returntype.def<>pdef(voiddef) then
          begin
              if (procinfo^.funcret_state<>vs_assigned) and not inlined { and
                ((procinfo^.flags and pi_uses_asm)=0)} then
               CGMessage(sym_w_function_result_not_set);
              hr:=new_reference(procinfo^.framepointer,procinfo^.return_offset);
              if (procinfo^.returntype.def^.deftype in [orddef,enumdef]) or
                 ((pocall_cdecl in procinfo^.def^.proccalloptions) and
                  (procinfo^.returntype.def^.deftype=recorddef) and
                  (procinfo^.returntype.def^.size<=4)) then
                begin
                  uses_acc:=true;
                  case procinfo^.returntype.def^.size of
                   8:
                     begin
                        emit_ref_reg(A_MOVE,S_L,hr,R_D0);
                        hr:=new_reference(procinfo^.framepointer,procinfo^.return_offset+4);
                        emit_ref_reg(A_MOVE,S_L,hr,R_D1);
                        uses_scratch:=true;
                     end;

                   4,3:
                     emit_ref_reg(A_MOVE,S_L,hr,R_D0);

                   2:
                     emit_ref_reg(A_MOVE,S_W,hr,R_D0);

                   1:
                     emit_ref_reg(A_MOVE,S_B,hr,R_D0);
                  end;
                end
              else
                if ret_in_acc(procinfo^.returntype.def,procinfo^.def^.proccalloptions) then
                  begin
                    uses_acc:=true;
                    emit_ref_reg(A_MOVE,S_L,hr,R_D0);
                    if  (procinfo^.returntype.def^.deftype=pointerdef) and
                     (pocall_cdecl in aktprocsym^.definition^.proccalloptions)
                    then
                     begin
                       uses_a0:=true;
                       emit_reg_reg(A_MOVE,S_L,R_D0,R_A0);
                     end;
                  end
              else
                 if (procinfo^.returntype.def^.deftype=floatdef) then
                   begin
                      if cs_fp_emulation in aktmoduleswitches then
                        begin
                          { indicate that the accumulator is being }
                          { used.                                  }
                          uses_acc := true;
                          { always s32real type }
                          emit_ref_reg(A_MOVE,S_L,hr,R_D0);
                        end
                      else
                        begin
                          case pfloatdef(procinfo^.returntype.def)^.typ of
                          s32real:
                              begin
                                  uses_fpu := true;
                                  { always use 32-bit register for }
                                  { s32 real value                 }
                                  emit_ref_reg(A_FMOVE,S_FS,hr,R_FP0);
                              end;
                          s64real:
                              begin
                                  emit_ref_reg(A_FMOVE,S_FL,hr,R_FP0);
                                  uses_fpu := true;
                              end;
                          s80real:
                              begin
                                  emit_ref_reg(A_FMOVE,S_FX,hr,R_FP0);
                                  uses_fpu := true;
                              end;
                          else
                            internalerror(3535);
                          end;
                        end;
                   end
              else
                dispose(hr);
          end
  end;


  procedure genexitcode(alist : paasmoutput;parasize:longint;
                        nostackframe,inlined:boolean;savedstackoffset : longint);

    var
{$ifdef GDB}
       mangled_length : longint;
       p : pchar;
       st : string[2];
{$endif GDB}
       nofinal,okexitlabel,noreraiselabel,nodestroycall : pasmlabel;
       hr : treference;
       hr2 : preference;
       uses_eax,uses_edx,uses_esi : boolean;
       oldexprasmlist : paasmoutput;
       ai : paicpu;
       pd : pprocdef;
       uses_acc : boolean;
       uses_scratch : boolean;
       uses_a0 : boolean;
       uses_self : boolean;
       uses_fpu : boolean;
       torestore : tregisterset;
  begin
      { The FPC 1.0.x compiler only supports parameter sizes of 32K
        when in 68000 mode
      }
     if ((parasize < -32767) or (parasize > 32768)) and (aktoptprocessor = MC68000) then
         CGMessage(cg_e_paralimit_in_local_routine);

      oldexprasmlist:=exprasmlist;
      exprasmlist:=alist;

      if aktexitlabel^.is_used then
        exprasmlist^.insert(new(pai_label,init(aktexitlabel)));

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
              { must the object be finalized ? }
              if procinfo^._class^.needs_inittable then
                begin
                   getlabel(nofinal);
                   reset_reference(hr);
                   hr.base:=frame_pointer;
                   hr.offset:=8;
                   emit_ref(A_TST,S_L,newreference(hr));
                   emitlabeled(A_BEQ,nofinal);
                   exprasmlist^.concat(new(paicpu,op_sym(A_PEA,S_L,procinfo^._class^.get_inittable_label)));
                   exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,self_pointer,R_SPPUSH)));
                   emitcall('FPC_FINALIZE');
                   emitlab(nofinal);
                end;
              getexplicitregister32(R_D0);
              exprasmlist^.concat(new(paicpu,op_const_reg(A_MOVE,S_L,procinfo^._class^.vmt_offset,R_D0)));
              emitcall('FPC_HELP_DESTRUCTOR');
              ungetregister32(R_D0);
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
           aktprocsym^.definition^.usedregisters:=ALL_REGISTERS;
           getlabel(noreraiselabel);
           emitcall('FPC_POPADDRSTACK');
           { The move instruction sets the zero flag accordingly,
             when the destination register is not an address
             register
           }
           exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,R_SPPULL,R_D0)));
           emitlabeled(A_BEQ,noreraiselabel);
           if (aktprocsym^.definition^.proctypeoption=potype_constructor) then
             begin
                if assigned(procinfo^._class) then
                  begin
                     pd:=procinfo^._class^.searchdestructor;
                     if assigned(pd) then
                       begin
                          getlabel(nodestroycall);
                          emit_ref(A_TST,S_L,new_reference(procinfo^.framepointer,
                            procinfo^.selfpointer_offset+4));
                          emitlabeled(A_BEQ,nodestroycall);
                          if procinfo^._class^.is_class then
                            begin
                               emit_const_reg(A_MOVE,S_L,1,R_SPPUSH);
                               emit_reg_reg(A_MOVE,S_L,self_pointer,R_SPPUSH);
                            end
                          else
                            begin
                               emit_reg_reg(A_MOVE,S_L,self_pointer,R_SPPUSH);
                               emit_sym(A_PEA,S_L,newasmsymbol(procinfo^._class^.vmt_mangledname));
                            end;
                          if (po_virtualmethod in pd^.procoptions) then
                            begin
                               getexplicitregister32(R_A1);
                               emit_ref_reg(A_MOVE,S_L,new_reference(self_pointer,
                                 procinfo^._class^.vmt_offset),R_A1);
                               { JSR doesn't seem to consider
                                 jsr a2(16) as indirect
                               emit_ref(A_JSR,S_NO,new_reference(R_A1,procinfo^._class^.vmtmethodoffset(pd^.extnumber)));
                                 it jump directly into VMT in that case,
                                 so we load the address of the method into
                                 the base register PM
                                 But that destroyed the self_pointer :(
                                 so we need to use another register for doing this PM }
                               emit_ref_reg(A_MOVE,S_L,new_reference(
                                 R_A1,procinfo^._class^.vmtmethodoffset(pd^.extnumber)),
                                 R_A1);
                               emit_ref(A_JSR,S_NO,new_reference(R_A1,0));
                               ungetregister(R_A1);
                            end
                          else
                            emitcall(pd^.mangledname);
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
         emitcall('FPC_DO_EXIT');
       end;

      { handle return value }
      uses_acc:=false;
      uses_scratch:=false;
      uses_a0 :=false;
      uses_self:=false;
      uses_fpu := false;
      if not(po_assembler in aktprocsym^.definition^.procoptions) then
          if (aktprocsym^.definition^.proctypeoption<>potype_constructor) then
            handle_return_value(inlined,uses_acc,uses_scratch,uses_a0,uses_fpu)
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
                      emit_ref_reg(A_MOVE,S_L,new_reference(procinfo^.framepointer,8),self_pointer);
                      emitcall('FPC_HELP_FAIL_CLASS');
                    end
                  else
                    begin
                      { first push the vmt offset }
                      emit_const_reg(A_MOVE,S_L,procinfo^._class^.vmt_offset,R_SPPUSH);
                      { We new to push the VMT that was sent by the caller
                        as otherwise we change the vmt field when calling
                        constructors inside constructors PM }
                      { as second we push the vmt value
                        which might have been set to -1 to indicate
                        that we should free the memory PM }
                      new(hr2);
                      reset_reference(hr2^);
                      hr2^.offset:=procinfo^.selfpointer_offset-4;
                      hr2^.base:=procinfo^.framepointer;
                      emit_ref(A_PEA,S_L,hr2);
                      { Using the generic version, so we need to push all args on stack }
                      { last push address of self }
                      new(hr2);
                      reset_reference(hr2^);
                      hr2^.offset:=procinfo^.selfpointer_offset;
                      hr2^.base:=procinfo^.framepointer;
                      emit_ref(A_PEA,S_L,hr2);
                      emitcall('FPC_HELP_FAIL');
                      { set the modified value into $a5 PM }
                      emit_ref_reg(A_MOVE,S_L,new_reference(procinfo^.framepointer,procinfo^.selfpointer_offset),self_pointer);
                    end;
                  emitlab(okexitlabel);

                  emit_reg_reg(A_MOVE,S_L,self_pointer,accumulator);
                  uses_acc:=true;
                  uses_self:=true;
              end;

      { stabs uses the label also ! }
      if aktexit2label^.is_used or
         ((cs_debuginfo in aktmoduleswitches) and not inlined) then
        emitlab(aktexit2label);
      { gives problems for long mangled names }
      {list^.concat(new(pai_symbol,init(aktprocsym^.definition^.mangledname+'_end')));}

       if savedstackoffset<>-1 then
        begin
          reset_reference(hr);
          hr.offset:=-savedstackoffset;
          hr.base:=procinfo^.framepointer;
          emit_ref_reg(A_MOVE,S_L,newreference(hr),R_SP);
        end;
      { GCC registers to save? }
      if (po_savestdregs in aktprocsym^.definition^.procoptions) then
        begin
         torestore:=[R_D2..R_D7,R_A2..R_A5];
         { should be intersected with used regs, no ? }
         torestore:=torestore*usedinproc;
         if torestore<>[] then
           exprasmlist^.concat(new(paicpu,op_reg_reglist(A_MOVEM,S_L,R_SPPULL,torestore)));
        end;

      { for the save all registers we can simply use a pusha,popa which
        push edi,esi,ebp,esp(ignored),ebx,edx,ecx,eax }
      if (po_saveregisters in aktprocsym^.definition^.procoptions) then
        begin
          if not ((cs_fp_emulation) in aktmoduleswitches) then
            begin
              torestore := [R_FP0..R_FP7];
              { avoid overwritting FP0 if it is a return value }
              if uses_fpu then
                torestore := torestore - [R_FP0];
              exprasmlist^.concat(new(paicpu,op_reg_reglist(A_FMOVEM,S_NO,R_SPPULL,torestore)));
            end;
          { the 80x86 version is a REAL HACK! :(...}
          torestore := [R_D0..R_SP];
          if uses_self then
            torestore := torestore - [self_pointer];
          if uses_acc then
            torestore := torestore - [accumulator];
          if uses_a0 then
            torestore := torestore - [R_A0];
          if uses_scratch then
            torestore := torestore - [scratch_reg];
          { restore specified registers }
          exprasmlist^.concat(new(paicpu,op_reg_reglist(A_MOVEM,S_L,R_SPPULL,
            torestore)));
        end;
      if not(nostackframe) then
        begin
          if not inlined then
               exprasmlist^.concat(new(paicpu,op_reg(A_UNLK,S_NO,frame_pointer)));
        end
      else
        begin
          if (gettempsize<>0) and not inlined then
            exprasmlist^.insert(new(paicpu,
              op_const_reg(A_ADD,S_L,gettempsize,stack_pointer)));
        end;


      { at last, the return is generated }

      if not inlined then
      if (po_interrupt in aktprocsym^.definition^.procoptions) then
          begin
            (* Is this correct ???? PM
             if uses_self then
               exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOVE,S_L,self_pointer,new_reference(stack_pointer,16))));
             if uses_scratch then
               exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOVE,S_L,scratch_reg,new_reference(stack_pointer,12))));
             if uses_acc then
               exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOVE,S_L,accumulator,new_reference(stack_pointer,0))));
               what should be the offset for A0 then ??
             if uses_a0 then
               exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOVE,S_L,R_A0,new_reference(stack_pointer,XXX)))); *)
             generate_interrupt_stackframe_exit;
          end;
      if not inlined then
       begin
       {Routines with the poclearstack flag set use only a ret.}
       { also routines with parasize=0     }
         if (pocall_clearstack in aktprocsym^.definition^.proccalloptions) then
           begin
{$ifndef OLD_C_STACK}
             { complex return values are removed from stack in C code PM }
             if ret_in_param(aktprocsym^.definition^.rettype.def,procinfo^.def^.proccalloptions) and
             { but cdecl'ared functions use A1 for this, no push }
                not (pocall_cdecl in aktprocsym^.definition^.proccalloptions) then
               begin
                 if (aktoptprocessor = MC68020) then
                     exprasmlist^.concat(new(paicpu,op_const(A_RTD,S_NO,4)))
                 else
                   begin
                      exprasmlist^.concat(new(paicpu,op_reg_reg(
                         A_MOVE,S_L,R_SPPULL,R_A1)));
                       exprasmlist^.concat(new(paicpu,op_const_reg(
                         A_ADDQ,S_L,4,R_SP)));
                       exprasmlist^.concat(new(paicpu,op_reg_reg(
                         A_MOVE,S_L,R_A1,R_SPPUSH)));
                       exprasmlist^.concat(new(paicpu,op_none(
                         A_RTS,S_NO)));
                   end;
               end
             else
{$endif not OLD_C_STACK}
               exprasmlist^.concat(new(paicpu,op_none(A_RTS,S_NO)));
           end
         else if (parasize=0) then
           begin
             exprasmlist^.concat(new(paicpu,op_none(A_RTS,S_NO)))
           end
         else
           begin
            { return with immediate size possible here }
            { signed!                                  }
            if (aktoptprocessor = MC68020) and (parasize < $7FFF) then
                exprasmlist^.concat(new(paicpu,op_const(A_RTD,S_NO,parasize)))
            { manually restore the stack }
            else
              begin
                    { We must pull the PC Counter from the stack, before  }
                    { restoring the stack pointer, otherwise the PC would }
                    { point to nowhere!                                   }

                    { save the PC counter (pop it from the stack)         }
                    exprasmlist^.concat(new(paicpu,op_reg_reg(
                         A_MOVE,S_L,R_SPPULL,R_A1)));
                    { can we do a quick addition ... }
                    if (parasize > 0) and (parasize < 9) then
                       exprasmlist^.concat(new(paicpu,op_const_reg(
                         A_ADDQ,S_L,parasize,R_SP)))
                    else { nope ... }
                       exprasmlist^.concat(new(paicpu,op_const_reg(
                         A_ADD,S_L,parasize,R_SP)));
                    { endif }
                    { restore the PC counter (push it on the stack)       }
                    exprasmlist^.concat(new(paicpu,op_reg_reg(
                         A_MOVE,S_L,R_A1,R_SPPUSH)));
                    exprasmlist^.concat(new(paicpu,op_none(
                      A_RTS,S_NO)))
               end;
           end;
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
                    tostr(N_RSYM)+',0,0,'+tostr(GDB_m68kindex[self_pointer])))));
                  end;
              { define calling EBP as pseudo local var PM }
              { this enables test if the function is a local one !! }
              if  assigned(procinfo^.parent) and (lexlevel>normal_function_level) then
                exprasmlist^.concat(new(pai_stabs,init(strpnew(
                 '"parent_ebp:'+voidpointerdef^.numberstring+'",'+
                 tostr(N_LSYM)+',0,0,'+tostr(procinfo^.framepointer_offset)))));

              if (pdef(aktprocsym^.definition^.rettype.def) <> pdef(voiddef)) then
                begin
                  if ret_in_param(aktprocsym^.definition^.rettype.def,procinfo^.def^.proccalloptions) then
                    exprasmlist^.concat(new(pai_stabs,init(strpnew(
                     '"'+aktprocsym^.name+':X*'+aktprocsym^.definition^.rettype.def^.numberstring+'",'+
                     tostr(N_PSYM)+',0,0,'+tostr(procinfo^.return_offset)))))
                  else
                    exprasmlist^.concat(new(pai_stabs,init(strpnew(
                     '"'+aktprocsym^.name+':X'+aktprocsym^.definition^.rettype.def^.numberstring+'",'+
                     tostr(N_PSYM)+',0,0,'+tostr(procinfo^.return_offset)))));
                  if (m_result in aktmodeswitches) then
                    if ret_in_param(aktprocsym^.definition^.rettype.def,procinfo^.def^.proccalloptions) then
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
              strpcopy(p,'224,0,0,'+aktexit2label^.name);
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
         exprasmlist^.concat(new(paicpu,op_none(A_RTS,S_NO)));
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
         exprasmlist^.concat(new(paicpu,op_none(A_RTS,S_NO)));
         alist^.concatlist(exprasmlist);
      end;


end.
{
  $Log: cga.pas,v $
  Revision 1.1.2.97  2003/06/18 23:19:42  pierre
   * try to fix call to virtual destructor in constructor

  Revision 1.1.2.96  2003/05/06 18:23:01  peter
    * don't call destroy in inherited constructor

  Revision 1.1.2.95  2003/03/24 11:02:09  pierre
   * fix bug 2432, was a problem in concatcopy code

  Revision 1.1.2.94  2003/03/10 14:21:07  pierre
   * fix use of unitialized var in push_value_para

  Revision 1.1.2.93  2003/03/07 12:14:21  pierre
   + new function cdecl_function_return_address_reg

  Revision 1.1.2.92  2003/03/06 00:28:58  pierre
   * try to fix the 64 bit range check code

  Revision 1.1.2.91  2003/03/03 19:47:31  carl
    * partial bugfix for internalerror 10

  Revision 1.1.2.90  2003/02/12 10:42:19  pierre
   * fix faillabel and aktexitlabel for body above 32K in size, fixes bug 2242

  Revision 1.1.2.89  2003/01/16 14:09:19  pierre
   * always use BSR for palmos, also in emitinsertcall

  Revision 1.1.2.88  2003/01/14 21:27:42  peter
    * fixed tw2296

  Revision 1.1.2.87  2002/11/26 11:18:15  pierre
   * fix internalerror in inet.pp compilation

  Revision 1.1.2.86  2002/11/21 10:28:15  pierre
   * use emitlabeled for A_BEQ instruction

  Revision 1.1.2.85  2002/11/20 10:30:07  pierre
   * solve tcalval2 failure

  Revision 1.1.2.84  2002/11/20 08:37:53  pierre
   * try to solve tcalval2 failure

  Revision 1.1.2.83  2002/11/19 23:06:24  pierre
   * try to fix stack for cdecl'ared functions

  Revision 1.1.2.82  2002/11/18 00:38:56  pierre
   * 3 byte records passed by value need special treatment

  Revision 1.1.2.81  2002/11/15 15:55:19  pierre
   * fix remaining m68k record with cdecl problems

  Revision 1.1.2.80  2002/11/15 14:10:16  pierre
    + added a calloptions parameter to ret_inacc and ret_in_param
      functions.
      This allows to handle win32 stdcall and m68k cdecl small records
      case.

  Revision 1.1.2.79  2002/11/15 10:51:45  pierre
   * try to fix passing small records to C functions for m68k

  Revision 1.1.2.78  2002/11/07 16:51:07  pierre
   * several memory leaks removed

  Revision 1.1.2.77  2002/10/21 12:22:29  pierre
   * relative position of high index is different for po_leftright

  Revision 1.1.2.76  2002/10/15 06:16:28  pierre
   * final ? fix for tcalval9

  Revision 1.1.2.75  2002/10/14 20:08:15  pierre
   * fix error in previous commit

  Revision 1.1.2.74  2002/10/14 19:43:22  pierre
   * try to fix the incompatibility between register saving and parameter copies

  Revision 1.1.2.73  2002/10/14 11:10:20  pierre
   * generate extended math functions for m68k if FPC_FPU_INTERNAL is defined

  Revision 1.1.2.72  2002/10/10 19:48:28  carl
    * bugfix of uninitialized variable

  Revision 1.1.2.71  2002/10/09 14:44:02  pierre
   * fix interrupt modifer generated code

  Revision 1.1.2.70  2002/10/09 12:43:03  pierre
   * also use FP0 as return register for 32 bit real type in non emulation mode

  Revision 1.1.2.69  2002/10/07 19:43:51  pierre
   * m68k cdecl'ared function with structured results have address of result in A1

  Revision 1.1.2.68  2002/09/26 20:19:11  carl
    * int_to_bool fixes for both i386 and m68k (direct typecasts
      of var parameters to bool, no longer allowed :()..
    * bugfix with a0 usage
    - now 68000 mode will not fail to compile if data elements are greater than 32K, only
      a warning will be emitted

  Revision 1.1.2.67  2002/09/26 08:03:27  pierre
   * Set return values into A0 also if it is a pointer for m68k cg

  Revision 1.1.2.66  2002/09/22 13:46:52  carl
    * forgot to increment pushedparassize in push_value_para when calling emit_push_mem_size
      (fixes tcalfun3 / tcalfun4 )
    * stack checking cannot be called before system unit is initialized

  Revision 1.1.2.65  2002/09/21 20:46:44  carl
    * stackalign = 4 now pushes value directly
    * emit_push_mem-size now used current alignment value

  Revision 1.1.2.64  2002/09/21 14:20:29  carl
    * default CPU is now 68020+ or higher
    * no longer limited in local stack spacr for 68020+ cpu
    * -m68000 / -m68020 option when assembler is called

  Revision 1.1.2.63  2002/09/20 22:06:26  pierre
   * also fix LOC_FLAGS case for push_value

  Revision 1.1.2.62  2002/09/20 20:58:01  pierre
   * fix push set element for 4 byte alignment

  Revision 1.1.2.61  2002/09/20 20:36:26  pierre
   * fix code generation error for pushing para value of LOC_JUMP with 4 stack alignment

  Revision 1.1.2.60  2002/09/20 13:47:29  pierre
   * avoid a wrong SUBQL 2,sp repetition

  Revision 1.1.2.59  2002/09/15 16:41:48  carl
    * alignment fixes for constant dat
    * tabs now correct for m68k
    * many cg fixes (don't remember all)

  Revision 1.1.2.58  2002/09/14 13:42:41  carl
    * small fix to previous commit
    * indirect call fixes for methods

  Revision 1.1.2.57  2002/09/13 18:54:03  carl
    * calling conventions related fixes
    * endian fixes with references
    * fixes of invalid emitted opcodes

  Revision 1.1.2.56  2002/09/12 19:52:06  carl
    * (m68k): bugfixes for 64-bit arithmetic, 64-bit shifts and 64-bit division
    * (m68k): bugfix for boolean type conversions
    * (m68k): endian problems fixes when src_Size = dst_Size with a different type

  Revision 1.1.2.55  2002/09/10 19:12:47  carl
    * cg int64 bugfixes related to endian (from cg testsuit)

  Revision 1.1.2.54  2002/01/21 21:30:39  pierre
   * fix bug 1658

  Revision 1.1.2.53  2002/01/19 17:05:15  carl
  *  fixed init / final for value parameters

  Revision 1.1.2.52  2002/01/19 16:59:04  carl
  * increment data for local parameters (from i386 version)

  Revision 1.1.2.51  2001/10/11 14:36:33  jonas
    * fixed internalerror(10) due to previous fpu overflow fixes
    * NOTE: from now on, the location of a binary node must now always be set
       before you call calcregisters() for it

  Revision 1.1.2.50  2001/09/17 13:42:31  pierre
   * fixes to remove multiple register ungetregister

  Revision 1.1.2.49  2001/09/14 15:37:39  pierre
   * more int64 fixes

  Revision 1.1.2.48  2001/09/14 11:57:01  pierre
   * try to fix stack pushing if alignment is 4

  Revision 1.1.2.47  2001/09/12 23:55:10  pierre
   * int64 fixes

  Revision 1.1.2.46  2001/09/09 15:14:52  carl
  * patch for loadansistring from i386 version

  Revision 1.1.2.45  2001/08/31 15:12:24  pierre
   * fixes problem with typecast from enum to orddef

  Revision 1.1.2.44  2001/08/29 14:55:26  jonas
    * backported int64 related fixes from main branch (for m68k: I only
      tested whether the compiler can still be compiled, you should still
      compile/run tests/test/tint64*.pp to verify whether I didn't break
      anything

  Revision 1.1.2.43  2001/08/17 16:16:48  florian
    + support for PalmOS added

  Revision 1.1.2.42  2001/08/09 11:39:05  pierre
   * not stack check for netbsd

  Revision 1.1.2.41  2001/08/04 06:12:40  carl
  * corrected shifts when value to shift is zero

  Revision 1.1.2.40  2001/08/03 12:45:43  pierre
   * fix small error in emit_to_mem for float type

  Revision 1.1.2.39  2001/08/03 12:14:25  pierre
   * allow nil as procedure of object in function args

  Revision 1.1.2.38  2001/07/31 23:27:51  pierre
   * avoid to generate wrong assembler instructions if no C saved reg needs to be pushed

  Revision 1.1.2.37  2001/07/31 13:34:43  pierre
   + C std regs saving code added

  Revision 1.1.2.36  2001/07/30 22:33:17  pierre
   * forgot to change a5 after call to fpc_help_fail

  Revision 1.1.2.35  2001/07/30 20:42:51  pierre
   * fix to fix for fail

  Revision 1.1.2.34  2001/07/30 20:27:04  pierre
   * fix fail calling

  Revision 1.1.2.33  2001/07/26 15:18:05  pierre
   * use AND $1 for flag2reg

  Revision 1.1.2.32  2001/07/26 03:06:32  carl
  * bugfix of Scc opcode (must always zero extend result)
  - removed some incorrect comments

  Revision 1.1.2.31  2001/07/25 13:02:21  pierre
   * fix rangecheck code for 8 or 16 bit types

  Revision 1.1.2.30  2001/07/24 15:51:25  pierre
   * fix a fpu problem

  Revision 1.1.2.29  2001/07/24 00:33:09  pierre
   * fix pushing of byte values

  Revision 1.1.2.28  2001/07/23 16:20:50  pierre
   * fix wrong register pushing in copyshortstring

  Revision 1.1.2.27  2001/07/23 13:53:48  pierre
   * use generic fail code

  Revision 1.1.2.26  2001/07/23 13:24:57  pierre
   * fix some object related errors

  Revision 1.1.2.25  2001/07/20 12:09:06  pierre
   * cleanup and fix emitloardord2reg function

  Revision 1.1.2.24  2001/07/19 16:42:47  pierre
   * fix a missed ADDQ use

  Revision 1.1.2.23  2001/07/18 15:30:45  pierre
   * restore FP registers first as they are pushed last for saveregisters

  Revision 1.1.2.22  2001/07/18 11:15:43  pierre
   + support for enumdef in boundcheck for array index

  Revision 1.1.2.21  2001/07/17 16:05:19  pierre
   * correct boundcheck code generation

  Revision 1.1.2.20  2001/07/17 14:00:40  pierre
   * no stackcheck routine for linux

  Revision 1.1.2.19  2001/07/17 07:26:24  pierre
   * remove size suffix for FMOVEM instruction

  Revision 1.1.2.18  2001/06/25 02:25:16  carl
  * bugfix of pushsetelement when a condtant or register is pushed! Alignment was wrong.

  Revision 1.1.2.17  2001/06/20 01:24:04  carl
  * stack corruption bugfix in restore()
  * correct problem with loading a non-byte value from memory and passing it as a byte to a parameter

  Revision 1.1.2.16  2001/06/13 03:06:56  carl
  * fix problems for sign extension with references (CLR cannot be used)

  Revision 1.1.2.15  2001/05/26 20:16:47  carl
  * Corrected problem with explicit register allocation

  Revision 1.1.2.14  2001/05/23 03:32:34  carl
  * Correct problem with pushing word on stack (was pushing a byte instead!)

  Revision 1.1.2.13  2001/05/21 16:46:20  carl
  * fix of mul call

  Revision 1.1.2.12  2001/05/18 17:59:17  carl
  * FPC_SHORTSTR_COPY call now saves used registers as it should
  * saveregisters() now works correctly
  * interrupt keyword now emits RTE keyword
  + maybe_push_float (not really useful... i should remove later)

  Revision 1.1.2.11  2001/05/17 01:30:23  carl
  * zero extended a register should not use CLR when the src reg = dest reg.

  Revision 1.1.2.10  2001/05/09 03:43:36  carl
  * corrected problem with FPC_HELP_FAIL_CLASS test (was using test on address reg)
  + entry code uses LINK opcode
  - removed sone support for 64-bit floating points

  Revision 1.1.2.9  2001/04/24 11:55:20  carl
  * correction of maybe_push() and restore() with LOC_FPU

  Revision 1.1.2.8  2001/04/22 23:34:37  carl
  * corrected problem in emitloardord2reg() (was checking wrong deftype)

  Revision 1.1.2.7  2001/04/22 23:11:41  pierre
   * fix emit_flag2reg function

  Revision 1.1.2.6  2001/04/21 05:03:42  carl
  * stackframe limit verification was inverted!
  * corrected compilation problem under FPC (nested comment)

  Revision 1.1.2.5  2001/04/21 00:33:48  carl
  * m68k updates

  Revision 1.1.2.4  2001/04/19 11:37:34  carl
  * m68k updates (code generator compiles)

  Revision 1.1.2.3  2001/04/17 03:23:14  carl
  + compiles (needs to be tested)

  Revision 1.1.2.7  2001/04/05 03:44:52  carl
  + more porting (entry / exit code)

  Revision 1.1.2.6  2001/03/27 02:54:40  carl
  + reinstated floatloaf and floatstore

  Revision 1.1.2.5  2001/03/24 21:34:18  carl
  + many many fixes

  Revision 1.1.2.4  2001/03/23 02:32:33  carl
  + more porting, now I've reached the routine entry/exit code

  Revision 1.1.2.3  2001/03/22 02:27:34  carl
  + half done!!!!

  Revision 1.1.2.2  2001/03/09 00:50:09  carl
  + more conversion

  Revision 1.1.2.1  2001/03/08 02:48:07  carl
  Started conversion

}
