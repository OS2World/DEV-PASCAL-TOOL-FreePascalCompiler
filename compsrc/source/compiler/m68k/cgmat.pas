{
    $Id: cgmat.pas,v 1.1.2.19 2002/09/29 14:07:28 carl Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate m68k assembler for math nodes

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
unit cgmat;
interface

    uses
      tree;

    procedure secondmoddiv(var p : ptree);
    procedure secondshlshr(var p : ptree);
    procedure secondunaryminus(var p : ptree);
    procedure secondnot(var p : ptree);


implementation

    uses
      globtype,systems,
      cobjects,verbose,globals,
      symconst,symtable,aasm,types,
      hcodegen,temp_gen,pass_2,
      cpubase,cpuasm,cgbase,
{$ifdef dummy}
      end  { this overcomes the annoying highlighting problem in my TP IDE,
             the IDE assumes i386asm start a asm block (FK) }
{$endif}
      cga,tgen;

{*****************************************************************************
                             SecondModDiv
*****************************************************************************}

    procedure secondmoddiv(var p : ptree);
      var
         unusedregisters,
         regstopush       : tregisterset;
         usablecount : byte;
         hreg1 : tregister;
         pushed : boolean;

         power : longint;
         hl,hl1 : pasmlabel;
         pushedreg : tpushed;
         typename,opname : string[6];
         reg : tregister;

      begin
         secondpass(p^.left);
         pushed:=maybe_push(p^.right^.registers32,p^.left,is_64bitint(p^.left^.resulttype));
         secondpass(p^.right);
         if pushed then
           restore(p^.left,is_64bitint(p^.left^.resulttype));
         set_location(p^.location,p^.left^.location);

{         writelocation(p);}
         if is_64bitint(p^.resulttype) then
           begin
              regstopush := ALL_REGISTERS;
              remove_non_regvars_from_loc(p^.location,regstopush);
              remove_non_regvars_from_loc(p^.right^.location,regstopush);
              
              { ugly hack because in *this* case, the pushed register }
              { must not be allocated later on (JM)                   }
              unusedregisters:=unused;
              usablecount:=usablereg32;
              saveusedregisters(pushedreg,regstopush);
              unused:=unusedregisters;
              usablereg32:=usablecount;

              { the left operand is in hloc, because the
                location of left is location but location
                is already destroyed
                
                not anymore... I had to change this because the
                regalloc info was completely wrong otherwise (JM)
              }
              emit_pushq_loc(p^.location);
              release_qword_loc(p^.location);
              clear_location(p^.location);
              emit_pushq_loc(p^.right^.location);
              release_qword_loc(p^.right^.location);

              if porddef(p^.resulttype)^.typ=u64bit then
                typename:='QWORD'
              else
                typename:='INT64';
              if p^.treetype=divn then
                opname:='DIV_'
              else
                opname:='MOD_';
              emitcall('FPC_'+opname+typename);

              p^.location.registerlow:=getregister32;
              p^.location.registerhigh := getregister32;
              p^.location.loc := LOC_REGISTER;
              emit_reg_reg(A_MOVE,S_L,R_D1,p^.location.registerlow);
              emit_reg_reg(A_MOVE,S_L,R_D0,p^.location.registerhigh);
              restoreusedregisters(pushedreg);
           end
         else
           begin
              { put numerator in register }
              if p^.left^.location.loc<>LOC_REGISTER then
                begin
                   if p^.left^.location.loc=LOC_CREGISTER then
                     begin
                       hreg1:=getregister32;
                       emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,hreg1);
                     end
                   else
                     begin
                       del_reference(p^.left^.location.reference);
                       hreg1:=getregister32;
                       emit_ref_reg(A_MOVE,S_L,newreference(p^.left^.location.reference),
                         hreg1);
                     end;
                   clear_location(p^.left^.location);
                   p^.left^.location.loc:=LOC_REGISTER;
                   p^.left^.location.register:=hreg1;
                end
              else hreg1:=p^.left^.location.register;

                if (p^.treetype=divn) and (p^.right^.treetype=ordconstn) and
                    ispowerof2(p^.right^.value,power) then
                  Begin
                    {for signed numbers, the numerator must be adjusted before the
                     shift instruction, but not wih unsigned numbers! Otherwise,
                     "Cardinal($ffffffff) div 16" overflows! (JM)}
                    If is_signed(p^.left^.resulttype) Then
                      Begin
                          begin
                          { a jump, but less operations }
                            emit_reg(A_TST,S_L,hreg1);
                            getlabel(hl);
                            emitlabeled(A_BPL,hl);
                            if power=1 then
                              emit_const_reg(A_ADDQ,S_L,1,hreg1)
                            else
                              emit_const_reg(A_ADD,S_L,p^.right^.value-1,hreg1);
                            emitlab(hl);
                            if (power > 0) and (power < 9) then
                               emit_const_reg(A_ASR, S_L,power, hreg1)
                            else
                              begin
                                emit_const_reg(A_MOVE,S_L,power, R_D0);
                                emit_reg_reg(A_ASR,S_L,R_D0, hreg1);
                              end;
                          end
                      End
                    Else { not signed }
                     Begin
                       if (power > 0) and (power < 9) then
                          emit_const_reg(A_LSR, S_L,power, hreg1)
                       else
                          begin
                            emit_const_reg(A_MOVE,S_L,power, R_D0);
                            emit_reg_reg(A_LSR,S_L,R_D0, hreg1);
                          end;
                     end;
                  End
                else
                  if (p^.treetype=modn) and (p^.right^.treetype=ordconstn) and
                    ispowerof2(p^.right^.value,power) and Not(is_signed(p^.left^.resulttype)) Then
                   {is there a similar trick for MOD'ing signed numbers? (JM)}
                   Begin
                     emit_const_reg(A_AND,S_L,p^.right^.value-1,hreg1);
                   End
                else
                  begin
                   { bring denominator to D1 }
                   { D1 is always free, it's }
                   { only used for temporary  }
                   { purposes                 }
                   if (p^.right^.location.loc<>LOC_REGISTER) and
                      (p^.right^.location.loc<>LOC_CREGISTER) then
                     begin
                        del_reference(p^.right^.location.reference);
                        p^.left^.location.loc:=LOC_REGISTER;
                        emit_ref_reg(A_MOVE,S_L,newreference(p^.right^.location.reference),R_D1);
                     end
                   else
                      begin
                        ungetregister32(p^.right^.location.register);
                        emit_reg_reg(A_MOVE,S_L,p^.right^.location.register,R_D1);
                      end;

                   { on entering this section D1 should contain the divisor }

                   if (aktoptprocessor = MC68020) then
                    begin
                      { Check if divisor is ZERO - if so call HALT_ERROR }
                      { with d0 = 200 (Division by zero!)                }
                      getlabel(hl1);
                      emit_reg(A_TST,S_L,R_D1);
                      { if not zero then simply continue on }
                      emitlabeled(A_BNE,hl1);
                      emit_const_reg(A_MOVE,S_L,200,R_SPPUSH);
                      emitcall('FPC_HANDLEERROR');
                      emitlab(hl1);
                      if (p^.treetype = modn) then
                        Begin
                          reg := getregister32;
                          emit_reg(A_CLR,S_L,reg);
                          getlabel(hl);
                          { here what we do is prepare the high register with the     }
                          { correct sign. i.e we clear it, check if the low dword reg }
                          { which will participate in the division is signed, if so we}
                          { we extend the sign to the high doword register by inverting }
                          { all the bits.                                             }
                          emit_reg(A_TST,S_L,hreg1);
                          emitlabeled(A_BPL,hl);
                          emit_reg(A_NOT,S_L,reg);
                          emitlab(hl);
                          if porddef(p^.right^.resulttype)^.typ=u32bit then
                           exprasmlist^.concat(new(paicpu,op_reg_reg_reg(A_DIVUL,S_L,R_D1,reg,hreg1)))
                          else
                          { reg:hreg1 / d1 }
                           exprasmlist^.concat(new(paicpu,op_reg_reg_reg(A_DIVSL,S_L,R_D1,reg,hreg1)));
                          { hreg1 already contains quotient }
                          { looking for remainder }
                          emit_reg_reg(A_MOVE,S_L,reg,hreg1);
                          ungetregister32(reg);
                        end
                      else
                         { simple division... }
                        Begin
                          { reg:hreg1 / d1 }
                           if porddef(p^.right^.resulttype)^.typ=u32bit then
                              emit_reg_reg(A_DIVU,S_L,R_D1,hreg1)
                           else
                              emit_reg_reg(A_DIVS,S_L,R_D1,hreg1);
                        end;
                    end
                  else { MC68000 operations }
                    begin
                       { u32bitdef (unsigned division) }
                       if porddef(p^.right^.resulttype)^.typ=u32bit then
                         begin
                           { put numerator in d0 }
                           emit_reg_reg(A_MOVE,S_L,hreg1,R_D0);
                           { operation to perform on entry to both }
                           { routines...  d0/d1                    }
                           { return result in d0                   }
                           if p^.treetype = divn then
                               emitcall('FPC_DIV_CARDINAL')
                           else
                               emitcall('FPC_MOD_CARDINAL');
                           emit_reg_reg(A_MOVE,S_L,R_D0,hreg1);
                         end
                       else
                       { s32bitdef }
                         begin
                           { put numerator in d0 }
                           emit_reg_reg(A_MOVE,S_L,hreg1,R_D0);
                           { operation to perform on entry to both }
                           { routines...  d0/d1                    }
                           { return result in d0                   }
                           if p^.treetype = divn then
                               emitcall('FPC_DIV_LONGINT')
                           else
                               emitcall('FPC_MOD_LONGINT');
                           emit_reg_reg(A_MOVE,S_L,R_D0,hreg1);
                         end;
                    end; { endif }
           end;
           usedinproc:=usedinproc + [R_D0];
           usedinproc:=usedinproc + [R_D1];
           clear_location(p^.location);
           p^.location.loc:=LOC_REGISTER;
           p^.location.register:=hreg1;
         end;
      end;


{*****************************************************************************
                             SecondShlShr
*****************************************************************************}

    procedure secondshlshr(var p : ptree);
      var
         hregister1,hregister2,hregister3,
         hregisterhigh,hregisterlow : tregister;
         pushed,popecx : boolean;
         op : tasmop;
         l1,l2,l3 : pasmlabel;
         tmploc : tlocation;
         saved : tpushed;
      begin
         popecx:=false;

         secondpass(p^.left);
         pushed:=maybe_push(p^.right^.registers32,p^.left,is_64bitint(p^.left^.resulttype));
         secondpass(p^.right);
         if pushed then
           restore(p^.left,is_64bitint(p^.left^.resulttype));

         if is_64bitint(p^.left^.resulttype) then
           begin
              { left operand is the value to shift }
              { load it into registers.            }
              if p^.left^.location.loc<>LOC_REGISTER then
                begin
                   if p^.left^.location.loc=LOC_CREGISTER then
                     begin
                        hregisterlow:=getregister32;
                        hregisterhigh:=getregister32;
                        emit_reg_reg(A_MOVE,S_L,p^.left^.location.registerlow,
                          hregisterlow);
                        emit_reg_reg(A_MOVE,S_L,p^.left^.location.registerhigh,
                          hregisterhigh);
                     end
                   else
                     begin
                        del_reference(p^.left^.location.reference);
                        hregisterlow:=getregister32;
                        hregisterhigh:=getregister32;
                        emit_mov_ref_reg64(p^.left^.location.reference,
                          hregisterlow,
                          hregisterhigh);
                     end;
                end
              else
                begin
                   hregisterlow:=p^.left^.location.registerlow;
                   hregisterhigh:=p^.left^.location.registerhigh;
                end;

              { starting here : LEFT node is in registers }

              { shifting by a constant directly coded: }
              { if the value is greater then 31        }
              if (p^.right^.treetype=ordconstn) then
                begin
                   { greater then 31, the lonwer value          }
                   { is equal to zero, and the value is swapped }
                   if p^.right^.value>31 then
                     begin
                        if p^.treetype=shln then
                          begin
                             emit_reg(A_CLR,S_L,hregisterhigh);
                             { constant values > 8 must be laoded into register }
                             if ((p^.right^.value mod 32) > 0) and ((p^.right^.value mod 32) < 9) then
                               emit_const_reg(A_LSL,S_L,p^.right^.value mod 32,
                               hregisterlow)
                             else
                               begin
                                 getexplicitregister32(R_D0);
                                 emit_const_reg(A_MOVE,S_L,p^.right^.value mod 32, R_D0);
                                 emit_reg_reg(A_LSL,S_L,R_D0,hregisterlow);
                                 ungetregister(R_D0);
                               end;
                          end
                        else
                          begin

                             emit_reg(A_CLR,S_L,hregisterlow);
                             { constant values > 8 must be laoded into register }
                             if ((p^.right^.value mod 32) > 0) and ((p^.right^.value mod 32) < 9) then
                               emit_const_reg(A_LSR,S_L,p^.right^.value mod 32,
                               hregisterhigh)
                             else
                              begin
                                getexplicitregister32(R_D0);
                                emit_const_reg(A_MOVE,S_L,p^.right^.value mod 32, R_D0);
                                emit_reg_reg(A_LSR,S_L,R_D0,hregisterhigh);
                                ungetregister(R_D0);
                              end;
                          end;
                        { the values are swapped }
                        p^.location.registerhigh:=hregisterlow;
                        p^.location.registerlow:=hregisterhigh;
                     end
                   else
                     begin
                        { don't save the registers which will receive }
                        { the result!                                 }
                        saveusedregisters(saved,ALL_REGISTERS
                         -[hregisterlow]-[hregisterhigh]-[accumulator]-
                         [scratch_reg]);
                        { push the actual value to shift }
                        { push the high 32-bit value }
                        emit_reg_reg(A_MOVE,S_L,hregisterhigh,R_SPPUSH);
                        { push the low 32-bit value }
                        emit_reg_reg(A_MOVE,S_L,hregisterlow,R_SPPUSH);
                        { push the shift count }
                        emit_const_reg(A_MOVE,S_L,p^.right^.value,R_SPPUSH);
                        { normal double precision shift }
                        if p^.treetype=shln then
                          begin
                            emitcall('FPC_SHL_INT64')
                          end
                        else
                          begin
                            emitcall('FPC_SHR_INT64')
                          end;
                        { restore all used registers }
                        { except those which will    }
                        { receive the result.        }
                        restoreusedregisters(saved);
                        emit_reg_reg(A_MOVE,S_L,R_D1,hregisterlow);
                        emit_reg_reg(A_MOVE,S_L,R_D0,hregisterhigh);
                        p^.location.registerlow:=hregisterlow;
                        p^.location.registerhigh:=hregisterhigh;
                     end;
                   p^.location.loc:=LOC_REGISTER;
                end
              else
                begin
                   { don't save the registers which will receive }
                   { the result!                                 }
                   saveusedregisters(saved,ALL_REGISTERS
                      -[hregisterlow]-[hregisterhigh]-[accumulator]-
                      [scratch_reg]);
                   { push the high 32-bit value }
                   emit_reg_reg(A_MOVE,S_L,hregisterhigh,R_SPPUSH);
                   { push the actual value to shift }
                   { push the low 32-bit value }
                   emit_reg_reg(A_MOVE,S_L,hregisterlow,R_SPPUSH);

                   { load right operators in a register }
                   { the constant value to shift by     }
                   if p^.right^.location.loc<>LOC_REGISTER then
                     begin
                       if p^.right^.location.loc=LOC_CREGISTER then
                          begin
                             emit_reg_reg(A_MOVE,S_L,p^.right^.location.register,
                               R_D0);
                          end
                        else
                          begin
                             del_reference(p^.right^.location.reference);
                             emit_ref_reg(A_MOVE,S_L,newreference(p^.right^.location.reference),
                               R_D0);
                          end;
                     end
                   else
                     emit_reg_reg(A_MOVE,S_L,p^.right^.location.register,R_D0);
                   emit_reg_reg(A_MOVE,S_L,R_D0,R_SPPUSH);

                   if p^.treetype = shln then
                      emitcall('FPC_SHL_INT64')
                   else
                      emitcall('FPC_SHR_INT64');

                   restoreusedregisters(saved);
                   emit_reg_reg(A_MOVE,S_L,R_D1,hregisterlow);
                   emit_reg_reg(A_MOVE,S_L,R_D0,hregisterhigh);
                   p^.location.loc := LOC_REGISTER;
                   p^.location.registerlow := hregisterlow;
                   p^.location.registerhigh := hregisterhigh;
                end;
           end
         else
         { not 64-bit operand }
           begin
              { load left operators in a register }
              if p^.left^.location.loc<>LOC_REGISTER then
                begin
                   if p^.left^.location.loc=LOC_CREGISTER then
                     begin
                        hregister1:=getregister32;
                        emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,
                          hregister1);
                     end
                   else
                     begin
                        del_reference(p^.left^.location.reference);
                        hregister1:=getregister32;
                        emit_ref_reg(A_MOVE,S_L,newreference(p^.left^.location.reference),
                          hregister1);
                     end;
                end
              else
                hregister1:=p^.left^.location.register;

              { determine operator }
              if p^.treetype=shln then
                op:=A_LSL
              else
                op:=A_LSR;

              { shifting by a constant directly coded: }
              if (p^.right^.treetype=ordconstn) then
                begin
                   { l shl 32 should 0 imho, but neither TP nor Delphi do it in this way (FK)
                   if p^.right^.value<=31 then
                   }
                   if ((p^.right^.value and 31) > 0) and ((p^.right^.value and 31) < 9) then
                        emit_const_reg(op,S_L,p^.right^.value and 31,
                               hregister1)
                   else
                      begin
                        { load into register first }
                        emit_const_reg(A_MOVE,S_L,p^.right^.value and 31,R_D0);
                        emit_reg_reg(op,S_L,R_D0,hregister1);
                     end;
                   p^.location.loc:=LOC_REGISTER;
                   p^.location.register:=hregister1;
                end
              else
                begin
                   { load right (shift count) operator in a register }
                   if p^.right^.location.loc<>LOC_REGISTER then
                     begin
                       if p^.right^.location.loc=LOC_CREGISTER then
                          begin
                             hregister2:=R_D0;
                             emit_reg_reg(A_MOVE,S_L,p^.right^.location.register,
                               hregister2);
                          end
                        else
                          begin
                             del_reference(p^.right^.location.reference);
                             hregister2:=R_D0;
                             emit_ref_reg(A_MOVE,S_L,newreference(p^.right^.location.reference),
                               hregister2);
                          end;
                     end
                   else
                     hregister2:=p^.right^.location.register;
                   { the shift count is modulo 32, to make it compatible
                     with the intel version.
                   }
                   emit_const_reg(A_AND,S_L,31,hregister2);
                   emit_reg_reg(op,S_L,hregister2,hregister1);
                   p^.location.register:=hregister1;
                end;
           end;
      end;


{*****************************************************************************
                             SecondUnaryMinus
*****************************************************************************}

    procedure secondunaryminus(var p : ptree);


      begin
         if is_64bitint(p^.left^.resulttype) then
           begin
              secondpass(p^.left);
              clear_location(p^.location);
              p^.location.loc:=LOC_REGISTER;
{              writelocation(p);}
             case p^.left^.location.loc of
                LOC_REGISTER :
                  begin
                     p^.location.registerlow:=p^.left^.location.registerlow;
                     p^.location.registerhigh:=p^.left^.location.registerhigh;
                  end;
                LOC_CREGISTER :
                  begin
                     p^.location.registerlow:=getregister32;
                     p^.location.registerhigh:=getregister32;
                     emit_reg_reg(A_MOVE,S_L,p^.left^.location.registerlow,p^.location.registerlow);
                     emit_reg_reg(A_MOVE,S_L,p^.left^.location.registerhigh,p^.location.registerhigh);
                  end;
                LOC_REFERENCE,LOC_MEM :
                  begin
                     del_reference(p^.left^.location.reference);
                     p^.location.registerlow:=getregister32;
                     p^.location.registerhigh:=getregister32;
                     emit_mov_ref_reg64(p^.left^.location.reference,
                       p^.location.registerlow,
                       p^.location.registerhigh);
                  end;
              end;
            emit_reg(A_NEG,S_L,p^.location.registerlow);
            emit_reg(A_NEGX,S_L,p^.location.registerhigh);
           end
         else
           begin
              secondpass(p^.left);
{              writelocation(p);}
              p^.location.loc:=LOC_REGISTER;
              case p^.left^.location.loc of
                 LOC_REGISTER:
                   begin
                      p^.location.register:=p^.left^.location.register;
                      emit_reg(A_NEG,S_L,p^.location.register);
                   end;
                 LOC_CREGISTER:
                   begin
                      p^.location.register:=getregister32;
                      emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,
                        p^.location.register);
                      emit_reg(A_NEG,S_L,p^.location.register);
                   end;
                 LOC_REFERENCE,LOC_MEM:
                                begin
                                   del_reference(p^.left^.location.reference);
                                   if (p^.left^.resulttype^.deftype=floatdef) and
                                      (pfloatdef(p^.left^.resulttype)^.typ<>f32bit) then
                                     begin
                                        p^.location.loc:=LOC_FPU;
                                        floatload(pfloatdef(p^.left^.resulttype)^.typ,
                                          p^.left^.location.reference,p^.location);
                                       if (cs_fp_emulation) in aktmoduleswitches then
                                        begin
                                         case pfloatdef(p^.left^.resulttype)^.typ of
                                          s32real:
                                            { if in emulation mode change sign manually }
                                            emit_const_reg(A_BCHG,S_L,31,
                                              p^.location.fpuregister);
                                          s64real,s80real:
                                             { if in emulation mode change sign manually }
                                             emit_const_reg(A_BCHG,S_L,31,
                                                 p^.location.fpuregisterhigh);
                                          else
                                            internalerror(1212);
                                         end; { end case }
                                        end
                                       else
                                           emit_reg(A_FNEG,S_FX,p^.location.fpuregister);
                                     end
                                   else
                                     begin
                                        p^.location.register:=getregister32;
                                        emit_ref_reg(A_MOVE,S_L,
                                          newreference(p^.left^.location.reference),
                                          p^.location.register);
                                        emit_reg(A_NEG,S_L,p^.location.register);
                                     end;
                                end;
                 LOC_FPU:
                   begin
                      p^.location.loc:=LOC_FPU;
                      p^.location.fpuregister := p^.left^.location.fpuregister;
                      if (cs_fp_emulation) in aktmoduleswitches then
                         begin
                            case pfloatdef(p^.left^.resulttype)^.typ of
                              s32real:
                                { if in emulation mode change sign manually }
                                emit_const_reg(A_BCHG,S_L,31,p^.location.fpuregister);
                              s64real,s80real:
                                { if in emulation mode change sign manually }
                                emit_const_reg(A_BCHG,S_L,31,p^.location.fpuregisterhigh);
                            else
                               internalerror(1212);
                            end; { end case }
                         end
                      else
                         emit_reg(A_FNEG,S_FX,p^.location.fpuregister);
                   end;
              end;
           end;
      end;


{*****************************************************************************
                               SecondNot
*****************************************************************************}

    procedure secondnot(var p : ptree);
      const
         flagsinvers : array[F_E..F_BE] of tresflags =
            (F_NE,F_E,F_LE,F_GE,F_L,F_G,F_NC,F_C,
             F_A,F_AE,F_B,F_BE);
      var
         hl : pasmlabel;
         opsize : topsize;
      begin
         if is_boolean(p^.resulttype) then
          begin
            opsize:=def_opsize(p^.resulttype);
            { the second pass could change the location of left }
            { if it is a register variable, so we've to do      }
            { this before the case statement                    }
            if p^.left^.location.loc in [LOC_REFERENCE,LOC_MEM,
              LOC_FLAGS,LOC_REGISTER,LOC_CREGISTER] then
              secondpass(p^.left);
{            writelocation(p);}
            case p^.left^.location.loc of
              LOC_JUMP :
                begin
                  hl:=truelabel;
                  truelabel:=falselabel;
                  falselabel:=hl;
                  secondpass(p^.left);
                  maketojumpbool(p^.left);
                  hl:=truelabel;
                  truelabel:=falselabel;
                  falselabel:=hl;
                end;
              LOC_FLAGS :
                p^.location.resflags:=flagsinvers[p^.left^.location.resflags];
              LOC_REGISTER, LOC_CREGISTER :
                begin
                  clear_location(p^.location);
                  emit_reg(A_TST,opsize,p^.left^.location.register);
                  { won't do anything for LOC_CREGISTER (JM) }
                  ungetregister(p^.left^.location.register);
                  p^.location.loc:=LOC_FLAGS;
                  p^.location.resflags:=F_E;
                end;
              LOC_REFERENCE,
              LOC_MEM :
                begin
                  clear_location(p^.location);
                  p^.location.loc:=LOC_REGISTER;
                  del_reference(p^.left^.location.reference);
                  { this was placed before del_ref => internaalerror(10) }
                  p^.location.register:=getregister32;
                  emit_ref_reg(A_MOVE,opsize,
                    newreference(p^.left^.location.reference),p^.location.register);
                  emit_reg(A_TST,opsize,p^.location.register);
                  ungetregister(p^.location.register);
                  p^.location.loc:=LOC_FLAGS;
                  p^.location.resflags:=F_E;
                end;
            end;
          end
         else if is_64bitint(p^.left^.resulttype) then
           begin
              secondpass(p^.left);
{              writelocation(p);}
              clear_location(p^.location);
              p^.location.loc:=LOC_REGISTER;
              case p^.left^.location.loc of
                LOC_REGISTER :
                  begin
                     p^.location.registerlow:=p^.left^.location.registerlow;
                     p^.location.registerhigh:=p^.left^.location.registerhigh;
                     emit_reg(A_NOT,S_L,p^.location.registerlow);
                     emit_reg(A_NOT,S_L,p^.location.registerhigh);
                  end;
                LOC_CREGISTER :
                  begin
                     p^.location.registerlow:=getregister32;
                     p^.location.registerhigh:=getregister32;
                     emit_reg_reg(A_MOVE,S_L,p^.left^.location.registerlow,p^.location.registerlow);
                     emit_reg_reg(A_MOVE,S_L,p^.left^.location.registerhigh,p^.location.registerhigh);
                     emit_reg(A_NOT,S_L,p^.location.registerlow);
                     emit_reg(A_NOT,S_L,p^.location.registerhigh);
                  end;
                LOC_REFERENCE,LOC_MEM :
                  begin
                     del_reference(p^.left^.location.reference);
                     p^.location.registerlow:=getregister32;
                     p^.location.registerhigh:=getregister32;
                     emit_mov_ref_reg64(p^.left^.location.reference,
                       p^.location.registerlow,
                       p^.location.registerhigh);
                     emit_reg(A_NOT,S_L,p^.location.registerlow);
                     emit_reg(A_NOT,S_L,p^.location.registerhigh);
                  end;
              end;
           end
         else
          begin
            secondpass(p^.left);
            clear_location(p^.location);
            p^.location.loc:=LOC_REGISTER;
            case p^.left^.location.loc of
              LOC_REGISTER :
                begin
                  p^.location.register:=p^.left^.location.register;
                  { correct the register }
                  opsize := def_opsize(p^.left^.resulttype);
                  emit_reg(A_NOT,opsize,p^.location.register);
                end;
              LOC_CREGISTER, LOC_REFERENCE, LOC_MEM :
                begin
                  opsize := def_opsize(p^.left^.resulttype);
                  if p^.left^.location.loc <> LOC_CREGISTER then
                     del_reference(p^.left^.location.reference);
                  p^.location.register:=getregister32;
                  if p^.left^.location.loc <> LOC_CREGISTER then
                    emit_ref_reg(A_MOVE,opsize,
                      newreference(p^.left^.location.reference),p^.location.register)
                  else
                    emit_reg_reg(A_MOVE,opsize,
                      p^.left^.location.register,p^.location.register);
                  emit_reg(A_NOT,opsize,p^.location.register);
                end;
            end;
          end;
      end;



end.
{
  $Log: cgmat.pas,v $
  Revision 1.1.2.19  2002/09/29 14:07:28  carl
    * more 64-bit shift problem fixes (there are still problems i think)

  Revision 1.1.2.18  2002/09/21 14:13:47  carl
    * 64-bit calls to shift routines had parameters reversed!

  Revision 1.1.2.17  2002/09/15 16:41:50  carl
    * alignment fixes for constant dat
    * tabs now correct for m68k
    * many cg fixes (don't remember all)

  Revision 1.1.2.16  2002/09/12 19:52:09  carl
    * (m68k): bugfixes for 64-bit arithmetic, 64-bit shifts and 64-bit division
    * (m68k): bugfix for boolean type conversions
    * (m68k): endian problems fixes when src_Size = dst_Size with a different type

  Revision 1.1.2.15  2002/08/18 11:36:03  carl
    * bugfix of secondnot with LOC_CREGISTER as left operand

  Revision 1.1.2.14  2001/08/30 08:09:45  jonas
    * fixed bug in my previous commit

  Revision 1.1.2.13  2001/08/29 14:55:28  jonas
    * backported int64 related fixes from main branch (for m68k: I only
      tested whether the compiler can still be compiled, you should still
      compile/run tests/test/tint64*.pp to verify whether I didn't break
      anything

  Revision 1.1.2.12  2001/07/30 12:11:26  pierre
   * add missing ungetregister

  Revision 1.1.2.11  2001/07/18 23:49:53  pierre
   * fix a bug in shr generation

  Revision 1.1.2.10  2001/05/26 20:23:36  carl
  * correct shifting for 64-bit values

  Revision 1.1.2.9  2001/05/21 16:51:54  carl
  * renamed according to FPC rules

  Revision 1.1.2.8  2001/05/09 03:47:09  carl
  * completely rewrote and corrected buggy shl/shr on 64-bit operands

  Revision 1.1.2.7  2001/04/21 05:23:03  carl
  + updated to reflect i386 changes made by Jonas

  Revision 1.1.2.6  2001/04/19 11:37:36  carl
  * m68k updates (code generator compiles)

  Revision 1.1.2.5  2001/04/05 03:45:30  carl
  * corrected secondnot problem

  Revision 1.1.2.4  2001/04/03 03:04:05  carl
  * cardinal support

  Revision 1.1.2.3  2001/04/02 02:21:09  carl
  + ported

  Revision 1.1.2.2  2001/03/29 02:25:46  carl
  + almost fully ported


}
