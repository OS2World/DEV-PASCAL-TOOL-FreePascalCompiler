{
    $Id: cgmat.pas,v 1.1.2.6 2002/08/18 11:36:02 carl Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate i386 assembler for math nodes

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
      cpubase,cpuasm,
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
         unusedregisters : tregisterset;
         usablecount : byte;
         hreg1 : tregister;
{$ifdef newOptimizations}
         hreg2 : tregister;
{$endif}
         shrdiv, andmod, pushed,popeax,popedx : boolean;

         power : longint;
         hl : pasmlabel;
         pushedreg : tpushed;
         typename,opname : string[6];
         regstopush: byte;

      begin
         shrdiv := false;
         andmod := false;
         secondpass(p^.left);
         pushed:=maybe_push(p^.right^.registers32,p^.left,is_64bitint(p^.left^.resulttype));
         secondpass(p^.right);
         if pushed then
           restore(p^.left,is_64bitint(p^.left^.resulttype));
         set_location(p^.location,p^.left^.location);

         if is_64bitint(p^.resulttype) then
           begin
              regstopush := $ff;
              remove_non_regvars_from_loc(p^.location,regstopush);
              remove_non_regvars_from_loc(p^.right^.location,regstopush);
              
              { ugly hack because in *this* case, the pushed register }
              { must not be allocated later on (JM)                   }
              unusedregisters:=unused;
              usablecount:=usablereg32;
              pushusedregisters(pushedreg,regstopush);
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

              { make sure we don't overwrite any results (JM) }
              if R_EDX in unused then
                begin
                   p^.location.registerhigh:=getexplicitregister32(R_EDX);
                   p^.location.registerlow:=getexplicitregister32(R_EAX);
                end
              else
                begin
                   p^.location.registerlow:=getexplicitregister32(R_EAX);
                   p^.location.registerhigh:=getexplicitregister32(R_EDX);
                end;
              p^.location.loc := LOC_REGISTER;
              emit_reg_reg(A_MOV,S_L,R_EAX,p^.location.registerlow);
              emit_reg_reg(A_MOV,S_L,R_EDX,p^.location.registerhigh);
              popusedregisters(pushedreg);
           end
         else
           begin
              { put numerator in register }
              if p^.left^.location.loc<>LOC_REGISTER then
                begin
                   if p^.left^.location.loc=LOC_CREGISTER then
                     begin
                       hreg1:=getregister32;
                       emit_reg_reg(A_MOV,S_L,p^.left^.location.register,hreg1);
                     end
                   else
                     begin
                       del_reference(p^.left^.location.reference);
                       hreg1:=getregister32;
                       emit_ref_reg(A_MOV,S_L,newreference(p^.left^.location.reference),
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
                    shrdiv := true;
                    {for signed numbers, the numerator must be adjusted before the
                     shift instruction, but not wih unsigned numbers! Otherwise,
                     "Cardinal($ffffffff) div 16" overflows! (JM)}
                    If is_signed(p^.left^.resulttype) Then
                      Begin
{$ifdef newOptimizations}
                        If (aktOptProcessor <> class386) and
                           not(CS_LittleSize in aktglobalswitches) then
                        { use a sequence without jumps, saw this in
                          comp.compilers (JM) }
                          begin
                          { no jumps, but more operations }
                            if (hreg1 = R_EAX) and
                               (R_EDX in unused) then
                              begin
                                hreg2 := getexplicitregister32(R_EDX);
                                emit_none(A_CDQ,S_NO);
                              end
                            else
                              begin
{$ifndef noAllocEdi}
                                getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                hreg2 := R_EDI;
                                emit_reg_reg(A_MOV,S_L,hreg1,R_EDI);
                              { if the left value is signed, R_EDI := $ffffffff,
                                otherwise 0 }
                                emit_const_reg(A_SAR,S_L,31,R_EDI);
                            { if signed, R_EDI := right value-1, otherwise 0 }
                              end;
                            emit_const_reg(A_AND,S_L,p^.right^.value-1,hreg2);
                          { add to the left value }
                            emit_reg_reg(A_ADD,S_L,hreg2,hreg1);
                          { release EDX if we used it }
{$ifndef noAllocEdi}
                          { also releas EDI }
                          ungetregister32(hreg2);
{$else noAllocEdi}
                          if (hreg2 = R_EDX) then
                            ungetregister32(hreg2);
{$endif noAllocEdi}
                          { do the shift }
                            emit_const_reg(A_SAR,S_L,power,hreg1);
                          end
                        else
{$endif newOptimizations}
                          begin
                          { a jump, but less operations }
                            emit_reg_reg(A_TEST,S_L,hreg1,hreg1);
                            getlabel(hl);
                            emitjmp(C_NS,hl);
                            if power=1 then
                              emit_reg(A_INC,S_L,hreg1)
                            else
                              emit_const_reg(A_ADD,S_L,p^.right^.value-1,hreg1);
                            emitlab(hl);
                            emit_const_reg(A_SAR,S_L,power,hreg1);
                          end
                      End
                    Else
                      emit_const_reg(A_SHR,S_L,power,hreg1);
                  End
                else
                  if (p^.treetype=modn) and (p^.right^.treetype=ordconstn) and
                    ispowerof2(p^.right^.value,power) and Not(is_signed(p^.left^.resulttype)) Then
                   {is there a similar trick for MOD'ing signed numbers? (JM)}
                   Begin
                     emit_const_reg(A_AND,S_L,p^.right^.value-1,hreg1);
                     andmod := true;
                   End
                else
                  begin
                      { bring denominator to EDI }
                      { EDI is always free, it's }
                      { only used for temporary  }
                      { purposes              }
{$ifndef noAllocEdi}
                   getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                   if (p^.right^.location.loc<>LOC_REGISTER) and
                      (p^.right^.location.loc<>LOC_CREGISTER) then
                     begin
                       del_reference(p^.right^.location.reference);
                       p^.left^.location.loc:=LOC_REGISTER;
                       emit_ref_reg(A_MOV,S_L,newreference(p^.right^.location.reference),R_EDI);
                     end
                   else
                     begin
                        emit_reg_reg(A_MOV,S_L,p^.right^.location.register,R_EDI);
                        ungetregister32(p^.right^.location.register);
                     end;
                   popedx:=false;
                   popeax:=false;
                   if hreg1=R_EDX then
                     begin
                       if not(R_EAX in unused) then
                          begin
                             emit_reg(A_PUSH,S_L,R_EAX);
                             popeax:=true;
                          end
                       else
                         getexplicitregister32(R_EAX);
                       emit_reg_reg(A_MOV,S_L,R_EDX,R_EAX);
                     end
                   else
                     begin
                        if not(R_EDX in unused) then
                          begin
                             emit_reg(A_PUSH,S_L,R_EDX);
                             popedx:=true;
                          end
                        else
                          getexplicitregister32(R_EDX);
                        if hreg1<>R_EAX then
                          begin
                             if not(R_EAX in unused) then
                               begin
                                  emit_reg(A_PUSH,S_L,R_EAX);
                                  popeax:=true;
                               end
                             else getexplicitregister32(R_EAX);
                             emit_reg_reg(A_MOV,S_L,hreg1,R_EAX);
                          end;
                     end;
                   { sign extension depends on the left type }
                   if porddef(p^.left^.resulttype)^.typ=u32bit then
                      emit_reg_reg(A_XOR,S_L,R_EDX,R_EDX)
                   else
                      emit_none(A_CDQ,S_NO);

                   { division depends on the right type }
                   if porddef(p^.right^.resulttype)^.typ=u32bit then
                     emit_reg(A_DIV,S_L,R_EDI)
                   else
                     emit_reg(A_IDIV,S_L,R_EDI);
{$ifndef noAllocEdi}
                   ungetregister32(R_EDI);
{$endif noAllocEdi}
                   if p^.treetype=divn then
                     begin
                        if not popedx and (hreg1 <> R_EDX) then
                          ungetregister(R_EDX);
                        { if result register is busy then copy }
                        if popeax then
                          begin
                             if hreg1=R_EAX then
                               internalerror(112);
                             emit_reg_reg(A_MOV,S_L,R_EAX,hreg1)
                          end
                        else
                          if hreg1<>R_EAX then
                            Begin
                              ungetregister32(hreg1);
                              { no need to allocate eax, that's already done before }
                              { the div (JM)                                        }
                              hreg1 := R_EAX;
                            end;
                     end
                   else
                     {if we did the mod by an "and", the result is in hreg1 and
                      EDX certainly hasn't been pushed (JM)}
                     if not(andmod) Then
                       begin
                         if not popeax and (hreg1 <> R_EAX) then
                           ungetregister(R_EAX);
                         if popedx then
                          {the mod was done by an (i)div (so the result is now in
                           edx), but edx was occupied prior to the division, so
                           move the result into a safe place (JM)}
                           emit_reg_reg(A_MOV,S_L,R_EDX,hreg1)
                         else
                           if hreg1<>R_EDX then
                             begin
                               ungetregister32(hreg1);
                               hreg1 := R_EDX;
                             end;
                       end;
                   if popeax then
                     emit_reg(A_POP,S_L,R_EAX);
                   if popedx then
                     emit_reg(A_POP,S_L,R_EDX);
                  end;
              If not(andmod or shrdiv) then
               {andmod and shrdiv only use hreg1 (which is already in usedinproc,
                since it was acquired with getregister), the others also use both
                EAX and EDX (JM)}
                Begin
                  usedinproc:=usedinproc or ($80 shr byte(R_EAX));
                  usedinproc:=usedinproc or ($80 shr byte(R_EDX));
                End;
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

      begin
         popecx:=false;

         secondpass(p^.left);
         pushed:=maybe_push(p^.right^.registers32,p^.left,is_64bitint(p^.left^.resulttype));
         secondpass(p^.right);
         if pushed then
           restore(p^.left,is_64bitint(p^.left^.resulttype));

         if is_64bitint(p^.left^.resulttype) then
           begin
              { load left operator in a register }
              if p^.left^.location.loc<>LOC_REGISTER then
                begin
                   if p^.left^.location.loc=LOC_CREGISTER then
                     begin
                        hregisterlow:=getregister32;
                        hregisterhigh:=getregister32;
                        emit_reg_reg(A_MOV,S_L,p^.left^.location.registerlow,
                          hregisterlow);
                        emit_reg_reg(A_MOV,S_L,p^.left^.location.registerhigh,
                          hregisterlow);
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

              { shifting by a constant directly coded: }
              if (p^.right^.treetype=ordconstn) then
                begin
                   { shrd/shl works only for values <=31 !! }
                   if p^.right^.value>31 then
                     begin
                        if p^.treetype=shln then
                          begin
                             emit_reg_reg(A_XOR,S_L,hregisterhigh,
                               hregisterhigh);
                             emit_const_reg(A_SHL,S_L,p^.right^.value and 31,
                               hregisterlow);
                          end
                        else
                          begin
                             emit_reg_reg(A_XOR,S_L,hregisterlow,
                               hregisterlow);
                             emit_const_reg(A_SHR,S_L,p^.right^.value and 31,
                               hregisterhigh);
                          end;
                        p^.location.registerhigh:=hregisterlow;
                        p^.location.registerlow:=hregisterhigh;
                     end
                   else
                     begin
                        if p^.treetype=shln then
                          begin
                             emit_const_reg_reg(A_SHLD,S_L,p^.right^.value and 31,
                               hregisterlow,hregisterhigh);
                             emit_const_reg(A_SHL,S_L,p^.right^.value and 31,
                               hregisterlow);
                          end
                        else
                          begin
                             emit_const_reg_reg(A_SHRD,S_L,p^.right^.value and 31,
                               hregisterhigh,hregisterlow);
                             emit_const_reg(A_SHR,S_L,p^.right^.value and 31,
                               hregisterhigh);
                          end;
                        p^.location.registerlow:=hregisterlow;
                        p^.location.registerhigh:=hregisterhigh;
                     end;
                   p^.location.loc:=LOC_REGISTER;
                end
              else
                begin
                   { load right operators in a register }
                   if p^.right^.location.loc<>LOC_REGISTER then
                     begin
                       if p^.right^.location.loc=LOC_CREGISTER then
                          begin
                             hregister2:=getexplicitregister32(R_ECX);
                             emit_reg_reg(A_MOV,S_L,p^.right^.location.register,
                               hregister2);
                          end
                        else
                          begin
                             del_reference(p^.right^.location.reference);
                             hregister2:=getexplicitregister32(R_ECX);
                             emit_ref_reg(A_MOV,S_L,newreference(p^.right^.location.reference),
                               hregister2);
                          end;
                     end
                   else
                     hregister2:=p^.right^.location.register;

                   { left operator is already in a register }
                   { hence are both in a register }
                   { is it in the case ECX ? }
                   if (hregisterlow=R_ECX) then
                     begin
                        { then only swap }
                        emit_reg_reg(A_XCHG,S_L,hregisterlow,hregister2);
                        hregister3:=hregisterlow;
                        hregisterlow:=hregister2;
                        hregister2:=hregister3;
                     end
                   else if (hregisterhigh=R_ECX) then
                     begin
                        { then only swap }
                        emit_reg_reg(A_XCHG,S_L,hregisterhigh,hregister2);
                        hregister3:=hregisterhigh;
                        hregisterhigh:=hregister2;
                        hregister2:=hregister3;
                     end

                   { if second operator not in ECX ? }
                   else if (hregister2<>R_ECX) then
                     begin
                        { ECX occupied then push it }
                        if not (R_ECX in unused) then
                         begin
                           popecx:=true;
                           emit_reg(A_PUSH,S_L,R_ECX);
                         end;
                        emit_reg_reg(A_MOV,S_L,hregister2,R_ECX);
                     end;

                   if hregister2 <> R_ECX then
                     ungetregister32(hregister2);

                   { the damned shift instructions work only til a count of 32 }
                   { so we've to do some tricks here                           }
                   if p^.treetype=shln then
                     begin
                        getlabel(l1);
                        getlabel(l2);
                        getlabel(l3);
                        emit_const_reg(A_CMP,S_L,64,R_ECX);
                        emitjmp(C_L,l1);
                        emit_reg_reg(A_XOR,S_L,hregisterlow,hregisterlow);
                        emit_reg_reg(A_XOR,S_L,hregisterhigh,hregisterhigh);
                        emitjmp(C_None,l3);
                        emitlab(l1);
                        emit_const_reg(A_CMP,S_L,32,R_ECX);
                        emitjmp(C_L,l2);
                        emit_const_reg(A_SUB,S_L,32,R_ECX);
                        emit_reg_reg(A_SHL,S_L,R_CL,
                          hregisterlow);
                        emit_reg_reg(A_MOV,S_L,hregisterlow,hregisterhigh);
                        emit_reg_reg(A_XOR,S_L,hregisterlow,hregisterlow);
                        emitjmp(C_None,l3);
                        emitlab(l2);
                        emit_reg_reg_reg(A_SHLD,S_L,R_CL,
                          hregisterlow,hregisterhigh);
                        emit_reg_reg(A_SHL,S_L,R_CL,
                          hregisterlow);
                        emitlab(l3);
                     end
                   else
                     begin
                        getlabel(l1);
                        getlabel(l2);
                        getlabel(l3);
                        emit_const_reg(A_CMP,S_L,64,R_ECX);
                        emitjmp(C_L,l1);
                        emit_reg_reg(A_XOR,S_L,hregisterlow,hregisterlow);
                        emit_reg_reg(A_XOR,S_L,hregisterhigh,hregisterhigh);
                        emitjmp(C_None,l3);
                        emitlab(l1);
                        emit_const_reg(A_CMP,S_L,32,R_ECX);
                        emitjmp(C_L,l2);
                        emit_const_reg(A_SUB,S_L,32,R_ECX);
                        emit_reg_reg(A_SHR,S_L,R_CL,
                          hregisterhigh);
                        emit_reg_reg(A_MOV,S_L,hregisterhigh,hregisterlow);
                        emit_reg_reg(A_XOR,S_L,hregisterhigh,hregisterhigh);
                        emitjmp(C_None,l3);
                        emitlab(l2);
                        emit_reg_reg_reg(A_SHRD,S_L,R_CL,
                          hregisterhigh,hregisterlow);
                        emit_reg_reg(A_SHR,S_L,R_CL,
                          hregisterhigh);
                        emitlab(l3);

                     end;

                   { maybe put ECX back }
                   if popecx then
                     emit_reg(A_POP,S_L,R_ECX)
                   else ungetregister32(R_ECX);

                   p^.location.registerlow:=hregisterlow;
                   p^.location.registerhigh:=hregisterhigh;
                end;
           end
         else
           begin
              { load left operators in a register }
              if p^.left^.location.loc<>LOC_REGISTER then
                begin
                   if p^.left^.location.loc=LOC_CREGISTER then
                     begin
                        hregister1:=getregister32;
                        emit_reg_reg(A_MOV,S_L,p^.left^.location.register,
                          hregister1);
                     end
                   else
                     begin
                        del_reference(p^.left^.location.reference);
                        hregister1:=getregister32;
                        emit_ref_reg(A_MOV,S_L,newreference(p^.left^.location.reference),
                          hregister1);
                     end;
                end
              else
                hregister1:=p^.left^.location.register;

              { determine operator }
              if p^.treetype=shln then
                op:=A_SHL
              else
                op:=A_SHR;

              { shifting by a constant directly coded: }
              if (p^.right^.treetype=ordconstn) then
                begin
                   { l shl 32 should 0 imho, but neither TP nor Delphi do it in this way (FK)
                   if p^.right^.value<=31 then
                   }
                     emit_const_reg(op,S_L,p^.right^.value and 31,
                       hregister1);
                   {
                   else
                     emit_reg_reg(A_XOR,S_L,hregister1,
                       hregister1);
                   }
                   p^.location.loc:=LOC_REGISTER;
                   p^.location.register:=hregister1;
                end
              else
                begin
                   { load right operators in a register }
                   if p^.right^.location.loc<>LOC_REGISTER then
                     begin
                       if p^.right^.location.loc=LOC_CREGISTER then
                          begin
                             hregister2:=getexplicitregister32(R_ECX);
                             emit_reg_reg(A_MOV,S_L,p^.right^.location.register,
                               hregister2);
                          end
                        else
                          begin
                             del_reference(p^.right^.location.reference);
                             hregister2:=getexplicitregister32(R_ECX);
                             emit_ref_reg(A_MOV,S_L,newreference(p^.right^.location.reference),
                               hregister2);
                          end;
                     end
                   else
                     hregister2:=p^.right^.location.register;

                   { left operator is already in a register }
                   { hence are both in a register }
                   { is it in the case ECX ? }
                   if (hregister1=R_ECX) then
                     begin
                        { then only swap }
                        emit_reg_reg(A_XCHG,S_L,hregister1,hregister2);
                        hregister3:=hregister1;
                        hregister1:=hregister2;
                        hregister2:=hregister3;
                     end
                   { if second operator not in ECX ? }
                   else if (hregister2<>R_ECX) then
                     begin
                        { ECX occupied then push it }
                        if not (R_ECX in unused) then
                         begin
                           popecx:=true;
                           emit_reg(A_PUSH,S_L,R_ECX);
                         end
                        else
                          getexplicitregister32(R_ECX);
                        emit_reg_reg(A_MOV,S_L,hregister2,R_ECX);
                     end;
                   ungetregister32(hregister2);
                   { right operand is in ECX }
                   emit_reg_reg(op,S_L,R_CL,hregister1);
                   { maybe ECX back }
                   if popecx then
                     emit_reg(A_POP,S_L,R_ECX)
                   else
                     ungetregister32(R_ECX);
                   p^.location.register:=hregister1;
                end;
           end;
      end;


{*****************************************************************************
                             SecondUnaryMinus
*****************************************************************************}

    procedure secondunaryminus(var p : ptree);

{$ifdef SUPPORT_MMX}
      procedure do_mmx_neg;
        var
           op : tasmop;
        begin
           p^.location.loc:=LOC_MMXREGISTER;
           if cs_mmx_saturation in aktlocalswitches then
             case mmx_type(p^.resulttype) of
                mmxs8bit:
                  op:=A_PSUBSB;
                mmxu8bit:
                  op:=A_PSUBUSB;
                mmxs16bit,mmxfixed16:
                  op:=A_PSUBSW;
                mmxu16bit:
                  op:=A_PSUBUSW;
             end
           else
             case mmx_type(p^.resulttype) of
                mmxs8bit,mmxu8bit:
                  op:=A_PSUBB;
                mmxs16bit,mmxu16bit,mmxfixed16:
                  op:=A_PSUBW;
                mmxs32bit,mmxu32bit:
                  op:=A_PSUBD;
             end;
           emit_reg_reg(op,S_NO,p^.location.register,R_MM7);
           emit_reg_reg(A_MOVQ,S_NO,R_MM7,p^.location.register);
        end;
{$endif}

      begin
         if is_64bitint(p^.left^.resulttype) then
           begin
              secondpass(p^.left);
              clear_location(p^.location);
              p^.location.loc:=LOC_REGISTER;
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
                     emit_reg_reg(A_MOV,S_L,p^.left^.location.registerlow,p^.location.registerlow);
                     emit_reg_reg(A_MOV,S_L,p^.left^.location.registerhigh,p^.location.registerhigh);
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
            {
            emit_reg(A_NEG,S_L,p^.location.registerlow);
            emit_const_reg(A_ADC,S_L,0,p^.location.registerhigh);
            emit_reg(A_NEG,S_L,p^.location.registerhigh);
            }
            emit_reg(A_NOT,S_L,p^.location.registerhigh);
            emit_reg(A_NEG,S_L,p^.location.registerlow);
            emit_const_reg(A_SBB,S_L,-1,p^.location.registerhigh);
           end
         else
           begin
              secondpass(p^.left);
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
                      emit_reg_reg(A_MOV,S_L,p^.left^.location.register,
                        p^.location.register);
                      emit_reg(A_NEG,S_L,p^.location.register);
                   end;
{$ifdef SUPPORT_MMX}
                 LOC_MMXREGISTER:
                   begin
                      set_location(p^.location,p^.left^.location);
                      emit_reg_reg(A_PXOR,S_NO,R_MM7,R_MM7);
                      do_mmx_neg;
                   end;
                 LOC_CMMXREGISTER:
                   begin
                      p^.location.register:=getregistermmx;
                      emit_reg_reg(A_PXOR,S_NO,R_MM7,R_MM7);
                      emit_reg_reg(A_MOVQ,S_NO,p^.left^.location.register,
                        p^.location.register);
                      do_mmx_neg;
                   end;
{$endif SUPPORT_MMX}
                 LOC_REFERENCE,LOC_MEM:
                                begin
                                   del_reference(p^.left^.location.reference);
                                   if (p^.left^.resulttype^.deftype=floatdef) and
                                      (pfloatdef(p^.left^.resulttype)^.typ<>f32bit) then
                                     begin
                                        p^.location.loc:=LOC_FPU;
                                        floatload(pfloatdef(p^.left^.resulttype)^.typ,
                                          p^.left^.location.reference);
                                        emit_none(A_FCHS,S_NO);
                                     end
{$ifdef SUPPORT_MMX}
                                   else if (cs_mmx in aktlocalswitches) and is_mmx_able_array(p^.left^.resulttype) then
                                     begin
                                        p^.location.register:=getregistermmx;
                                        emit_reg_reg(A_PXOR,S_NO,R_MM7,R_MM7);
                                        emit_ref_reg(A_MOVQ,S_NO,
                                          newreference(p^.left^.location.reference),
                                          p^.location.register);
                                        do_mmx_neg;
                                     end
{$endif SUPPORT_MMX}
                                   else
                                     begin
                                        p^.location.register:=getregister32;
                                        emit_ref_reg(A_MOV,S_L,
                                          newreference(p^.left^.location.reference),
                                          p^.location.register);
                                        emit_reg(A_NEG,S_L,p^.location.register);
                                     end;
                                end;
                 LOC_FPU:
                   begin
                      p^.location.loc:=LOC_FPU;
                      emit_none(A_FCHS,S_NO);
                   end;
                 LOC_CFPUREGISTER:
                   begin
                      emit_reg(A_FLD,S_NO,
                        correct_fpuregister(p^.left^.location.register,fpuvaroffset));
                      inc(fpuvaroffset);
                      p^.location.loc:=LOC_FPU;
                      emit_none(A_FCHS,S_NO);
                   end;
              end;
           end;
{ Here was a problem...     }
{ Operand to be negated always     }
{ seems to be converted to signed  }
{ 32-bit before doing neg!!     }
{ So this is useless...     }
{ that's not true: -2^31 gives an overflow error if it is negaded (FK) }
{        emitoverflowcheck(p);}
      end;


{*****************************************************************************
                               SecondNot
*****************************************************************************}

    procedure secondnot(var p : ptree);
      const
         flagsinvers : array[F_E..F_BE] of tresflags =
            (F_NE,F_E,F_LE,F_GE,F_L,F_G,F_NC,F_C,
             F_BE,F_B,F_AE,F_A);
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
                  emit_reg_reg(A_TEST,opsize,
                    p^.left^.location.register,p^.left^.location.register);
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
                  p^.location.register:=def_getreg(p^.resulttype);
                  emit_ref_reg(A_MOV,opsize,
                    newreference(p^.left^.location.reference),p^.location.register);
                  emit_reg_reg(A_TEST,opsize,p^.location.register,p^.location.register);
                  ungetregister(p^.location.register);
                  p^.location.loc:=LOC_FLAGS;
                  p^.location.resflags:=F_E;
                end;
            end;
          end
{$ifdef SUPPORT_MMX}
         else
          if (cs_mmx in aktlocalswitches) and is_mmx_able_array(p^.left^.resulttype) then
           begin
             secondpass(p^.left);
             p^.location.loc:=LOC_MMXREGISTER;
             { prepare EDI }
{$ifndef noAllocEdi}
             getexplicitregister32(R_EDI);
{$endif noAllocEdi}
             emit_const_reg(A_MOV,S_L,$ffffffff,R_EDI);
             { load operand }
             case p^.left^.location.loc of
               LOC_MMXREGISTER:
                 set_location(p^.location,p^.left^.location);
               LOC_CMMXREGISTER:
                 begin
                   p^.location.register:=getregistermmx;
                   emit_reg_reg(A_MOVQ,S_NO,p^.left^.location.register,p^.location.register);
                 end;
               LOC_REFERENCE,LOC_MEM:
                 begin
                   del_reference(p^.left^.location.reference);
                   p^.location.register:=getregistermmx;
                   emit_ref_reg(A_MOVQ,S_NO,
                     newreference(p^.left^.location.reference),p^.location.register);
                 end;
             end;
             { load mask }
             emit_reg_reg(A_MOVD,S_NO,R_EDI,R_MM7);
{$ifndef noAllocEdi}
             ungetregister32(R_EDI);
{$endif noAllocEdi}
             { lower 32 bit }
             emit_reg_reg(A_PXOR,S_D,R_MM7,p^.location.register);
             { shift mask }
             emit_const_reg(A_PSLLQ,S_NO,32,R_MM7);
             { higher 32 bit }
             emit_reg_reg(A_PXOR,S_D,R_MM7,p^.location.register);
           end
{$endif SUPPORT_MMX}
         else if is_64bitint(p^.left^.resulttype) then
           begin
              secondpass(p^.left);
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
                     emit_reg_reg(A_MOV,S_L,p^.left^.location.registerlow,p^.location.registerlow);
                     emit_reg_reg(A_MOV,S_L,p^.left^.location.registerhigh,p^.location.registerhigh);
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
                  emit_reg(A_NOT,regsize(p^.location.register),p^.location.register);
                end;
              LOC_CREGISTER, LOC_REFERENCE, LOC_MEM :
                begin
                  opsize := def_opsize(p^.left^.resulttype);
                  if p^.left^.location.loc <> LOC_CREGISTER then
                     del_reference(p^.left^.location.reference);
                  p^.location.register:=getregister32;
                  case opsize of
                    S_B: p^.location.register := reg32toreg8(p^.location.register);
                    S_W: p^.location.register := reg32toreg16(p^.location.register);
                  end;
                  if p^.left^.location.loc <> LOC_CREGISTER then
                    emit_ref_reg(A_MOV,opsize,
                      newreference(p^.left^.location.reference),p^.location.register)
                  else
                    emit_reg_reg(A_MOV,opsize,
                      p^.left^.location.register,p^.location.register);
                  emit_reg(A_NOT,opsize,p^.location.register);
                end;
            end;
          end;
      end;



end.
{
  $Log: cgmat.pas,v $
  Revision 1.1.2.6  2002/08/18 11:36:02  carl
    * bugfix of secondnot with LOC_CREGISTER as left operand

  Revision 1.1.2.5  2001/12/07 13:03:49  jonas
    * fixed web bug 1716

  Revision 1.1.2.4  2001/08/29 14:55:26  jonas
    * backported int64 related fixes from main branch (for m68k: I only
      tested whether the compiler can still be compiled, you should still
      compile/run tests/test/tint64*.pp to verify whether I didn't break
      anything

  Revision 1.1.2.3  2001/04/08 11:03:11  jonas
    * 'not' now always returns a value of the same type + backport of Peter's mainbnranch fixes

  Revision 1.1.2.2  2001/04/04 14:07:15  jonas
    * fixed secondnot (it only supported 4 byte sized operands)

  Revision 1.1.2.1  2001/02/25 03:51:50  carl
  + trying again

  Revision 1.1.2.1  2001/02/25 02:33:34  carl
  - moved to i386 directory

  Revision 1.1.2.6  2000/11/20 14:05:06  jonas
    * fixed bug in my changes to fix the regalloc info for div/mod

  Revision 1.1.2.5  2000/10/19 16:24:32  jonas
    * small fix for previous commit (edx could be released too soon in some
      cases)

  Revision 1.1.2.4  2000/10/19 16:15:34  jonas
    * fixed wrong regalloc info for secondmoddiv

  Revision 1.1.2.3  2000/10/17 15:45:28  jonas
    * fixed allocation of ecx for shl/shr

  Revision 1.1.2.2  2000/09/18 10:07:26  jonas
    * fixed bug in flagsinvers array for unsigned flags (fixed web bug 1135)

  Revision 1.1.2.1  2000/07/28 13:22:48  jonas
    * fixed bug in secondshlshr where ecx was released too soon in some
      cases causing a combination of -Or and -dnewoptimizations to generate
      wrong code

  Revision 1.1  2000/07/13 06:29:45  michael
  + Initial import

  Revision 1.45  2000/03/19 15:20:22  florian
    * not(b) if b is a register variable, didn't work, fixed

  Revision 1.44  2000/02/24 18:41:38  peter
    * removed warnings/notes

  Revision 1.43  2000/02/18 21:25:48  florian
    * fixed a bug in int64/qword handling was a quite ugly one

  Revision 1.42  2000/02/09 13:22:47  peter
    * log truncated

  Revision 1.41  2000/01/27 15:46:00  florian
    * slighly improved code for -<qword> and -<int64>

  Revision 1.40  2000/01/09 12:35:01  jonas
    * changed edi allocation to use getexplicitregister32/ungetregister
      (adapted tgeni386 a bit for this) and enabled it by default
    * fixed very big and stupid bug of mine in cg386mat that broke the
      include() code (and make cycle :( ) if you compiled without
      -dnewoptimizations

  Revision 1.39  2000/01/09 01:44:20  jonas
    + (de)allocation info for EDI to fix reported bug on mailinglist.
      Also some (de)allocation info for ESI added. Between -dallocEDI
      because at this time of the night bugs could easily slip in ;)

  Revision 1.38  2000/01/07 01:14:21  peter
    * updated copyright to 2000

  Revision 1.37  2000/01/07 00:12:10  peter
    * fixed movd isntruction to be A_MOVD instead of A_MOV S_D

  Revision 1.36  1999/11/18 15:34:44  pierre
    * Notes/Hints for local syms changed to
      Set_varstate function

  Revision 1.35  1999/11/06 14:34:18  peter
    * truncated log to 20 revs

  Revision 1.34  1999/09/28 19:43:47  florian
    * the maybe_push fix of Pierre wasn't 100%, the tree parameter
      must contain a valid location (which is saved if necessary)

  Revision 1.33  1999/09/27 23:37:26  peter
    * fixed push/restore bug in div/mod

  Revision 1.32  1999/09/02 17:07:38  florian
    * problems with -Or fixed: tdef.isfpuregable was wrong!

  Revision 1.31  1999/08/19 13:08:50  pierre
   * emit_??? used

  Revision 1.30  1999/08/04 13:45:23  florian
    + floating point register variables !!
    * pairegalloc is now generated for register variables

  Revision 1.29  1999/08/04 00:22:51  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.28  1999/08/03 22:02:45  peter
    * moved bitmask constants to sets
    * some other type/const renamings

}