{
    $Id: cgflw.pas,v 1.1.2.9 2002/10/09 13:09:02 pierre Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate m68k assembler for nodes that influence the flow

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
{$ifdef FPC}
  {$goto on}
{$endif FPC}
unit cgflw;
interface

    uses
      tree;

    procedure secondfor(var p : ptree);
    procedure secondexitn(var p : ptree);
    procedure secondraise(var p : ptree);
    procedure secondtryexcept(var p : ptree);
    procedure secondtryfinally(var p : ptree);
    procedure secondon(var p : ptree);

implementation

    uses
      cobjects,verbose,globtype,globals,systems,
      symconst,symtable,aasm,types,
      hcodegen,temp_gen,pass_2,
      cpubase,cpuasm,cgflwcmn,
      cga,tgen,tcflw,cgbase;





{*****************************************************************************
                              SecondFor
*****************************************************************************}

    procedure secondfor(var p : ptree);
      var
         l3,oldclabel,oldblabel : pasmlabel;
         omitfirstcomp,temptovalue : boolean;
         hs : byte;
         temp1 : treference;
         hop : tasmop;
         cmpreg : tregister;
         opsize : topsize;
         count_var_is_signed : boolean;

      begin
         oldclabel:=aktcontinuelabel;
         oldblabel:=aktbreaklabel;
         getlabel(aktcontinuelabel);
         getlabel(aktbreaklabel);
         getlabel(l3);

         { could we spare the first comparison ? }
         omitfirstcomp:=false;
         if p^.right^.treetype=ordconstn then
           if p^.left^.right^.treetype=ordconstn then
             omitfirstcomp:=(p^.backward and (p^.left^.right^.value>=p^.right^.value))
               or (not(p^.backward) and (p^.left^.right^.value<=p^.right^.value));

         { only calculate reference }
         cleartempgen;
         secondpass(p^.t2);
         { first set the to value
           because the count var can be in the expression !! }
         cleartempgen;
         secondpass(p^.right);
         hs:=p^.t2^.resulttype^.size;
         case hs of
            1 : begin
                   opsize:=S_B;
                end;
            2 : begin
                   opsize:=S_W;
                end;
            4 : begin
                   opsize:=S_L;
                end;
         end;
         { calculate pointer value and check if changeable and if so }
         { load into temporary variable                       }
         if p^.right^.treetype<>ordconstn then
           begin
              temp1.symbol:=nil;
              gettempofsizereference(hs,temp1);
              temptovalue:=true;
              if (p^.right^.location.loc=LOC_REGISTER) or
                 (p^.right^.location.loc=LOC_CREGISTER) then
                begin
                   emit_reg_ref(A_MOVE,opsize,p^.right^.location.register,
                      newreference(temp1));
                 end
              else
                 concatcopy(p^.right^.location.reference,temp1,hs,false,false);
           end
         else
           temptovalue:=false;

         { produce start assignment }
         cleartempgen;
         secondpass(p^.left);
         if p^.t2^.location.loc <> LOC_CREGISTER then
           cmpreg:=getregister32;

         count_var_is_signed:=is_signed(porddef(p^.t2^.resulttype));
         if temptovalue then
             begin
              if p^.t2^.location.loc=LOC_CREGISTER then
                begin
                   emit_ref_reg(A_CMP,opsize,newreference(temp1),
                     p^.t2^.location.register);
                end
              else
                begin
                   emit_ref_reg(A_MOVE,opsize,newreference(p^.t2^.location.reference),
                     cmpreg);
                   emit_ref_reg(A_CMP,opsize,newreference(temp1),
                     cmpreg);
                   { temp register not necessary anymore currently (JM) }
                   ungetregister32(cmpreg);
                end;
           end
         else
             begin
              if not(omitfirstcomp) then
                begin
                   if p^.t2^.location.loc=LOC_CREGISTER then
                     emit_const_reg(A_CMP,opsize,p^.right^.value,
                       p^.t2^.location.register)
                   else
                     emit_const_ref(A_CMP,opsize,p^.right^.value,
                       newreference(p^.t2^.location.reference));
                end;
           end;
         if p^.backward then
           if count_var_is_signed then
              hop:=A_BLT
           else
              hop:=A_BCS
         else
           if count_var_is_signed then
             hop:=A_BGT
           else
         hop:=A_BHI;

         if not(omitfirstcomp) or temptovalue then
            emitlabeled(hop,aktbreaklabel);

         emitlab(l3);

         { help register must not be in instruction block }
         cleartempgen;
         if assigned(p^.t1) then
           secondpass(p^.t1);

         emitlab(aktcontinuelabel);

         { makes no problems there }
         cleartempgen;

         if (p^.t2^.location.loc <> LOC_CREGISTER) then
           begin
             { demand help register again }
             cmpreg:=getregister32;
             case hs of
                1 : opsize := S_B;
                2 : opsize := S_W;
                4 : opsize := S_L;
             end;
           end;

         { produce comparison and the corresponding }
         { jump                              }
         if temptovalue then
           begin
              if p^.t2^.location.loc=LOC_CREGISTER then
                begin
                   emit_ref_reg(A_CMP,opsize,newreference(temp1),
                     p^.t2^.location.register);
                end
              else
                begin
                   emit_ref_reg(A_MOVE,opsize,newreference(p^.t2^.location.reference),
                     cmpreg);
                   emit_ref_reg(A_CMP,opsize,newreference(temp1),
                     cmpreg);
               end;
           end
         else
           begin
              if p^.t2^.location.loc=LOC_CREGISTER then
                emit_const_reg(A_CMP,opsize,p^.right^.value,
                  p^.t2^.location.register)
              else
                 emit_const_ref(A_CMP,opsize,p^.right^.value,
                   newreference(p^.t2^.location.reference));
           end;
         if p^.backward then
           if count_var_is_signed then
             hop:=A_BLE
           else
             hop:=A_BLS
          else
            if count_var_is_signed then
              hop:=A_BGE
            else
              hop:=A_BCC;
         emitlabeled(hop,aktbreaklabel);
         { according to count direction DEC or INC... }
         { must be after the test because of 0to 255 for bytes !! }
         if p^.backward then
           hop:=A_SUBQ
         else
           hop:=A_ADDQ;

         if p^.t2^.location.loc=LOC_CREGISTER then
           emit_const_reg(hop,opsize,1,p^.t2^.location.register)
         else
           emit_const_ref(hop,opsize,1,newreference(p^.t2^.location.reference));
         emitjmp(C_None,l3);

         if (p^.t2^.location.loc <> LOC_CREGISTER) then
           ungetregister32(cmpreg);
         if temptovalue then
           ungetiftemp(temp1);

         { this is the break label: }
         emitlab(aktbreaklabel);

         aktcontinuelabel:=oldclabel;
         aktbreaklabel:=oldblabel;
         { a break/continue in a for block can't be seen outside }
         flowcontrol:=flowcontrol-[fc_break,fc_continue];
      end;


{*****************************************************************************
                              SecondExitN
*****************************************************************************}

    procedure secondexitn(var p : ptree);
      var
         is_mem : boolean;
         {op : tasmop;
         s : topsize;}
         otlabel,oflabel : pasmlabel;
         r : preference;

      label
         do_jmp;
      begin
         include(flowcontrol,fc_exit);
         if assigned(p^.left) then
         if p^.left^.treetype=assignn then
           begin
              { just do a normal assignment followed by exit }
              secondpass(p^.left);
              emitjmp(C_None,aktexitlabel);
           end
         else
           begin
              otlabel:=truelabel;
              oflabel:=falselabel;
              getlabel(truelabel);
              getlabel(falselabel);
              secondpass(p^.left);
              case p^.left^.location.loc of
                 LOC_FPU : goto do_jmp;
                 LOC_MEM,
           LOC_REFERENCE : is_mem:=true;
           LOC_CREGISTER,
            LOC_REGISTER : is_mem:=false;
               LOC_FLAGS : begin
                             emit_flag2reg(p^.left^.location.resflags,R_D0);
                             goto do_jmp;
                           end;
                LOC_JUMP : begin
                             emitlab(truelabel);
                             emit_const_reg(A_MOVEQ,S_L,1,accumulator);
                             emitjmp(C_None,aktexit2label);
                             emitlab(falselabel);
                             emit_reg(A_CLR,S_B,accumulator);
                             goto do_jmp;
                           end;
              else
                internalerror(2001);
              end;
              case procinfo^.returntype.def^.deftype of
           pointerdef,
           procvardef : begin
                          if is_mem then
                            emit_ref_reg(A_MOVE,S_L,
                              newreference(p^.left^.location.reference),accumulator)
                          else
                            emit_reg_reg(A_MOVE,S_L,
                              p^.left^.location.register,accumulator);
                        end;
             floatdef : begin
                        case pfloatdef(procinfo^.returntype.def)^.typ of
                      f32bit : internalerror(499987);
                      s32real :
                             begin
                                if is_mem then
                                  begin
                                    if cs_fp_emulation in aktmoduleswitches then
                                      emit_ref_reg(A_MOVE,S_L,
                                        newreference(p^.left^.location.reference),accumulator)
                                    else
                                      emit_ref_reg(A_FMOVE,S_FS,
                                      newreference(p^.left^.location.reference),R_FP0);
                                  end
                               else
                                 begin
                                   { single values are in the floating point registers }
                                   if cs_fp_emulation in aktmoduleswitches then
                                     emit_reg_reg(A_MOVE,S_L,p^.left^.location.fpuregister,
                                       accumulator)
                                   else
                                     emit_reg_reg(A_FMOVE,S_FS,p^.left^.location.fpuregister,R_FP0);
                                 end;
                            end;
                      s64real :
                        begin
                              if cs_fp_emulation in aktmoduleswitches then
                                begin
                                  if is_mem then
                                    begin
                                      emit_ref_reg(A_MOVE,S_L,
                                        newreference(p^.left^.location.reference),accumulator);
                                      r:=newreference(p^.left^.location.reference);
                                      inc(r^.offset,4);
                                      emit_ref_reg(A_MOVE,S_L,r,scratch_reg);
                                    end
                                  else
                                    begin
                                      emit_reg_reg(A_MOVE,S_L,p^.left^.location.fpuregisterlow,accumulator);
                                      emit_reg_reg(A_MOVE,S_L,p^.left^.location.fpuregisterhigh,scratch_reg);
                                    end;
                                   end
                                 else
                                   begin
                                     if is_mem then
                                      emit_ref_reg(A_FMOVE,S_FL,
                                 newreference(p^.left^.location.reference),R_FP0)
                                     else
                                emit_reg_reg(A_FMOVE,S_FL,p^.left^.location.fpuregister,
                                 R_FP0);
                                   end;
                        end;
                      s64comp :
                               internalerror(1213);
                      s80real :
                        begin
                                 { if emulation mode - extended maps to double }
                                 if cs_fp_emulation in aktmoduleswitches then
                                   begin
                                     if is_mem then
                                       begin
                                         emit_ref_reg(A_MOVE,S_L,
                                            newreference(p^.left^.location.reference),accumulator);
                                         r:=newreference(p^.left^.location.reference);
                                         inc(r^.offset,4);
                                         emit_ref_reg(A_MOVE,S_L,r,scratch_reg);
                                       end
                                     else
                                       begin
                                         emit_reg_reg(A_MOVE,S_L,p^.left^.location.fpuregisterlow,accumulator);
                                         emit_reg_reg(A_MOVE,S_L,p^.left^.location.fpuregisterhigh,scratch_reg);
                                       end;
                                   end
                                 else
                                   begin
                                     if is_mem then
                                      emit_ref_reg(A_FMOVE,S_FX,
                                        newreference(p^.left^.location.reference),R_FP0)
                                     else
                                      emit_reg_reg(A_FMOVE,S_FX,p^.left^.location.fpuregister,
                                        R_FP0);
                                   end;
                        end; { end s80real }
                          end; { end pfloatdef case }
                       end;
              { orddef,
              enumdef : }
              else
                       begin
                          case procinfo^.returntype.def^.size of
                           { it can be a qword/int64 too ... }
                           8 : if is_mem then
                                 begin
                                    { MSB is D0 }
                                    emit_ref_reg(A_MOVE,S_L,
                                      newreference(p^.left^.location.reference),accumulator);
                                    r:=newreference(p^.left^.location.reference);
                                    inc(r^.offset,4);
                                    { LSB in D1 }
                                    emit_ref_reg(A_MOVE,S_L,r,scratch_reg);
                                 end
                               else
                                 begin
                                    emit_reg_reg(A_MOVE,S_L,p^.left^.location.registerlow,scratch_reg);
                                    emit_reg_reg(A_MOVE,S_L,p^.left^.location.registerhigh,accumulator);
                                 end;
                          { this depends on the target alignment!!!!! carl }
                           3: internalerror(12);
                           4 :  if is_mem then
                                 emit_ref_reg(A_MOVE,S_L,
                                   newreference(p^.left^.location.reference),accumulator)
                               else
                                 emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,accumulator);
                           2 : if is_mem then
                                 emit_ref_reg(A_MOVE,S_W,
                                   newreference(p^.left^.location.reference),accumulator)
                               else
                                 emit_reg_reg(A_MOVE,S_W,p^.left^.location.register,accumulator);
                           1 : if is_mem then
                                 emit_ref_reg(A_MOVE,S_B,
                                   newreference(p^.left^.location.reference),accumulator)
                               else
                                 emit_reg_reg(A_MOVE,S_B,p^.left^.location.register,accumulator);
                           else internalerror(605001);
                          end;
                        end;
              end;
do_jmp:
              truelabel:=otlabel;
              falselabel:=oflabel;
              emitjmp(C_None,aktexit2label);
           end
         else
           begin
              emitjmp(C_None,aktexitlabel);
           end;
       end;


{*****************************************************************************
                             SecondRaise
*****************************************************************************}

    procedure secondraise(var p : ptree);

      var
         a : pasmlabel;
      begin
         if assigned(p^.left) then
           begin
              { multiple parameters? }
              if assigned(p^.right) then
                begin
                  { push frame }
                  if assigned(p^.frametree) then
                    begin
                      secondpass(p^.frametree);
                      if codegenerror then
                       exit;
                      emit_push_loc(p^.frametree^.location);
                    end
                  else
           { push 0 on the stack }
                    emit_reg(A_CLR,S_L,R_SPPUSH);
                  { push address }
                  secondpass(p^.right);
                  if codegenerror then
                   exit;
                  emit_push_loc(p^.right^.location);
                end
              else
                begin
                   getlabel(a);
                   emitlab(a);
           { push 0 on the stack }
                   emit_reg(A_CLR,S_L,R_SPPUSH);
                   emit_sym(A_PEA,S_L,a);
                end;
              { push object }
              secondpass(p^.left);
              if codegenerror then
                exit;
              emit_push_loc(p^.left^.location);
              emitcall('FPC_RAISEEXCEPTION');
           end
         else
           begin
              emitcall('FPC_POPADDRSTACK');
              emitcall('FPC_RERAISE');
           end;
       end;


{*****************************************************************************
                             SecondTryExcept
*****************************************************************************}

    var
       endexceptlabel : pasmlabel;

    { does the necessary things to clean up the object stack }
    { in the except block                                    }
    procedure cleanupobjectstack;

      begin
         emitcall('FPC_POPOBJECTSTACK');
         exprasmlist^.concat(new(pairegalloc,alloc(accumulator)));
         emit_reg_reg(A_MOVE,S_L,accumulator,R_SPPUSH);
         emitcall('FPC_DESTROYEXCEPTION');
         exprasmlist^.concat(new(pairegalloc,dealloc(accumulator)));
         maybe_loadself;
      end;

    { pops one element from the exception address stack }
    { and removes the flag                              }
    procedure cleanupaddrstack;

      begin
         emitcall('FPC_POPADDRSTACK');
         { allocate ACC. }
         exprasmlist^.concat(new(pairegalloc,alloc(accumulator)));
         emit_reg_reg(A_MOVE,S_L,R_SPPULL,accumulator);
         { deallocate ACC. }
         exprasmlist^.concat(new(pairegalloc,dealloc(accumulator)));
      end;

    procedure secondtryexcept(var p : ptree);

      var
         exceptlabel,doexceptlabel,oldendexceptlabel,
         lastonlabel,
         exitexceptlabel,
         continueexceptlabel,
         breakexceptlabel,
         exittrylabel,
         continuetrylabel,
         breaktrylabel,
         doobjectdestroy,
         doobjectdestroyandreraise,
         oldaktexitlabel,
         oldaktexit2label,
         oldaktcontinuelabel,
         oldaktbreaklabel : pasmlabel;
         oldexceptblock : ptree;


         oldflowcontrol,tryflowcontrol,
         exceptflowcontrol : tflowcontrol;
      label
         errorexit;
      begin
         oldflowcontrol:=flowcontrol;
         flowcontrol:=[];
         { this can be called recursivly }
         oldendexceptlabel:=endexceptlabel;

         { we modify ACC. }
         usedinproc:=usedinproc + [accumulator];

         { save the old labels for control flow statements }
         oldaktexitlabel:=aktexitlabel;
         oldaktexit2label:=aktexit2label;
         {if assigned(aktbreaklabel) then
           begin this is wrong because
           oldaktbreaklabel was not set to nil
           if aktbreaklevel is nil ! PM }
         oldaktcontinuelabel:=aktcontinuelabel;
         oldaktbreaklabel:=aktbreaklabel;

         { get new labels for the control flow statements }
         getlabel(exittrylabel);
         getlabel(exitexceptlabel);
         if assigned(aktbreaklabel) then
           begin
              getlabel(breaktrylabel);
              getlabel(continuetrylabel);
              getlabel(breakexceptlabel);
              getlabel(continueexceptlabel);
           end;

         getlabel(exceptlabel);
         getlabel(doexceptlabel);
         getlabel(endexceptlabel);
         getlabel(lastonlabel);
         push_int (1); { push type of exceptionframe }
         emitcall('FPC_PUSHEXCEPTADDR');
         { allocate acc. }
         exprasmlist^.concat(new(pairegalloc,alloc(accumulator)));
         emit_reg_reg(A_MOVE,S_L,accumulator,R_SPPUSH);
         emitcall('FPC_SETJMP');
         { the move instruction sets the zero flag
           if the destination is not an address register
           - carl
         }
         emit_reg_reg(A_MOVE,S_L,accumulator,R_SPPUSH);
         { deallocate acc. }
         exprasmlist^.concat(new(pairegalloc,dealloc(accumulator)));
         emitlabeled(A_BNE,exceptlabel);

         { try block }
         { set control flow labels for the try block }
         aktexitlabel:=exittrylabel;
         aktexit2label:=exittrylabel;
         if assigned(oldaktbreaklabel) then
          begin
            aktcontinuelabel:=continuetrylabel;
            aktbreaklabel:=breaktrylabel;
          end;

         oldexceptblock:=aktexceptblock;
         aktexceptblock:=p^.left;
         flowcontrol:=[];
         secondpass(p^.left);
         tryflowcontrol:=flowcontrol;
         aktexceptblock:=oldexceptblock;
         if codegenerror then
           goto errorexit;

         emitlab(exceptlabel);
         emitcall('FPC_POPADDRSTACK');

         exprasmlist^.concat(new(pairegalloc,alloc(accumulator)));
         { the move instruction sets the zero flag
           if the destination is not an address register
           - carl
         }
         emit_reg_reg(A_MOVE,S_L,R_SPPULL,accumulator);
         exprasmlist^.concat(new(pairegalloc,dealloc(accumulator)));

         emitlabeled(A_BEQ,endexceptlabel);
         emitlab(doexceptlabel);

         { set control flow labels for the except block }
         { and the on statements                        }
         aktexitlabel:=exitexceptlabel;
         aktexit2label:=exitexceptlabel;
         if assigned(oldaktbreaklabel) then
          begin
            aktcontinuelabel:=continueexceptlabel;
            aktbreaklabel:=breakexceptlabel;
          end;

         flowcontrol:=[];
         { on statements }
         if assigned(p^.right) then
           begin
              oldexceptblock:=aktexceptblock;
              aktexceptblock:=p^.right;
              secondpass(p^.right);
              aktexceptblock:=oldexceptblock;
           end;

         emitlab(lastonlabel);
         { default handling except handling }
         if assigned(p^.t1) then
           begin
              { FPC_CATCHES must be called with
                'default handler' flag (=-1)
              }
              push_int (-1);
              emitcall('FPC_CATCHES');
              maybe_loadself;

              { the destruction of the exception object must be also }
              { guarded by an exception frame                        }
              getlabel(doobjectdestroy);
              getlabel(doobjectdestroyandreraise);
              emit_const_reg(A_MOVE,S_L,1,R_SPPUSH);
              emitcall('FPC_PUSHEXCEPTADDR');
              exprasmlist^.concat(new(pairegalloc,alloc(accumulator)));
              exprasmlist^.concat(new(paicpu,
                op_reg_reg(A_MOVE,S_L,accumulator,R_SPPUSH)));
              exprasmlist^.concat(new(pairegalloc,dealloc(accumulator)));
              emitcall('FPC_SETJMP');
              exprasmlist^.concat(new(pairegalloc,alloc(accumulator)));
              { the move instruction sets the zero flag
                if the destination is not an address register
                - carl
              }
              emit_reg_reg(A_MOVE,S_L,accumulator,R_SPPUSH);
              exprasmlist^.concat(new(pairegalloc,dealloc(accumulator)));
              emitlabeled(A_BNE,doobjectdestroyandreraise);

              oldexceptblock:=aktexceptblock;
              aktexceptblock:=p^.t1;
              { here we don't have to reset flowcontrol           }
              { the default and on flowcontrols are handled equal }
              secondpass(p^.t1);
              exceptflowcontrol:=flowcontrol;
              aktexceptblock:=oldexceptblock;

              emitlab(doobjectdestroyandreraise);
              emitcall('FPC_POPADDRSTACK');
              exprasmlist^.concat(new(pairegalloc,alloc(accumulator)));
              { the move instruction sets the zero flag
                if the destination is not an address register
                - carl
              }
              emit_reg_reg(A_MOVE,S_L,R_SPPULL,accumulator);
              exprasmlist^.concat(new(pairegalloc,dealloc(accumulator)));
              emitlabeled(A_BEQ,doobjectdestroy);
              emitcall('FPC_POPSECONDOBJECTSTACK');
              exprasmlist^.concat(new(pairegalloc,alloc(accumulator)));
              emit_reg_reg(A_MOVE,S_L,accumulator,R_SPPUSH);
              emitcall('FPC_DESTROYEXCEPTION');
              exprasmlist^.concat(new(pairegalloc,dealloc(accumulator)));
              { we don't need to restore esi here because reraise never }
              { returns                                                 }
              emitcall('FPC_RERAISE');

              emitlab(doobjectdestroy);
              cleanupobjectstack;
              emitjmp(C_None,endexceptlabel);
           end
         else
           begin
              emitcall('FPC_RERAISE');
              exceptflowcontrol:=flowcontrol;
           end;

         if fc_exit in exceptflowcontrol then
           begin
              { do some magic for exit in the try block }
              emitlab(exitexceptlabel);
              { we must also destroy the address frame which guards }
              { exception object                                    }
              cleanupaddrstack;
              cleanupobjectstack;
              emitjmp(C_None,oldaktexitlabel);
           end;

         if fc_break in exceptflowcontrol then
           begin
              emitlab(breakexceptlabel);
              { we must also destroy the address frame which guards }
              { exception object                                    }
              cleanupaddrstack;
              cleanupobjectstack;
              emitjmp(C_None,oldaktbreaklabel);
           end;

         if fc_continue in exceptflowcontrol then
           begin
              emitlab(continueexceptlabel);
              { we must also destroy the address frame which guards }
              { exception object                                    }
              cleanupaddrstack;
              cleanupobjectstack;
              emitjmp(C_None,oldaktcontinuelabel);
           end;

         if fc_exit in tryflowcontrol then
           begin
              { do some magic for exit in the try block }
              emitlab(exittrylabel);
              cleanupaddrstack;
              emitjmp(C_None,oldaktexitlabel);
           end;

         if fc_break in tryflowcontrol then
           begin
              emitlab(breaktrylabel);
              cleanupaddrstack;
              emitjmp(C_None,oldaktbreaklabel);
           end;

         if fc_continue in tryflowcontrol then
           begin
              emitlab(continuetrylabel);
              cleanupaddrstack;
              emitjmp(C_None,oldaktcontinuelabel);
           end;

         emitlab(endexceptlabel);

       errorexit:
         { restore all saved labels }
         endexceptlabel:=oldendexceptlabel;

         { restore the control flow labels }
         aktexitlabel:=oldaktexitlabel;
         aktexit2label:=oldaktexit2label;
         if assigned(oldaktbreaklabel) then
          begin
            aktcontinuelabel:=oldaktcontinuelabel;
            aktbreaklabel:=oldaktbreaklabel;
          end;

         { return all used control flow statements }
         flowcontrol:=oldflowcontrol+exceptflowcontrol+
           tryflowcontrol;
      end;

    procedure secondon(var p : ptree);

      var
         nextonlabel,
         exitonlabel,
         continueonlabel,
         breakonlabel,
         oldaktexitlabel,
         oldaktexit2label,
         oldaktcontinuelabel,
         doobjectdestroyandreraise,
         doobjectdestroy,
         oldaktbreaklabel : pasmlabel;
         ref : treference;
         oldexceptblock : ptree;
         oldflowcontrol : tflowcontrol;

      begin
         oldflowcontrol:=flowcontrol;
         flowcontrol:=[];
         getlabel(nextonlabel);

         { push the vmt }
         emit_sym(A_PEA,S_L,newasmsymbol(p^.excepttype^.vmt_mangledname));
         emitcall('FPC_CATCHES');
         { allocate eax }
         exprasmlist^.concat(new(pairegalloc,alloc(accumulator)));
         emit_reg(A_TST,S_L,accumulator);
         emitlabeled(A_BEQ,nextonlabel);
         ref.symbol:=nil;
     { set some space for exception handler address }
         gettempofsizereference(target_os.size_of_pointer,ref);

         { what a hack ! }
         if assigned(p^.exceptsymtable) then
           pvarsym(p^.exceptsymtable^.symindex^.first)^.address:=ref.offset;

         emit_reg_ref(A_MOVE,S_L,
           accumulator,newreference(ref));
         { deallocate eax }
         exprasmlist^.concat(new(pairegalloc,dealloc(accumulator)));

         { in the case that another exception is risen }
         { we've to destroy the old one                }
         getlabel(doobjectdestroyandreraise);
        emit_const_reg(A_MOVE,S_L,1,R_SPPUSH);
         emitcall('FPC_PUSHEXCEPTADDR');
         exprasmlist^.concat(new(pairegalloc,alloc(accumulator)));
         emit_reg_reg(A_MOVE,S_L,accumulator,R_SPPUSH);
         exprasmlist^.concat(new(pairegalloc,dealloc(accumulator)));
         emitcall('FPC_SETJMP');
         exprasmlist^.concat(new(pairegalloc,alloc(accumulator)));
         { the move sets the zero flag accordingly
           when the destination is not an address register
         }
         emit_reg_reg(A_MOVE,S_L,accumulator,R_SPPUSH);
         exprasmlist^.concat(new(pairegalloc,dealloc(accumulator)));
         emitlabeled(A_BNE,doobjectdestroyandreraise);

         if assigned(p^.right) then
           begin
              oldaktexitlabel:=aktexitlabel;
              oldaktexit2label:=aktexit2label;
              getlabel(exitonlabel);
              aktexitlabel:=exitonlabel;
              aktexit2label:=exitonlabel;
              if assigned(aktbreaklabel) then
               begin
                 oldaktcontinuelabel:=aktcontinuelabel;
                 oldaktbreaklabel:=aktbreaklabel;
                 getlabel(breakonlabel);
                 getlabel(continueonlabel);
                 aktcontinuelabel:=continueonlabel;
                 aktbreaklabel:=breakonlabel;
               end;

              { self register is destroyed by FPC_CATCHES }
              maybe_loadself;
              oldexceptblock:=aktexceptblock;
              aktexceptblock:=p^.right;
              secondpass(p^.right);
              aktexceptblock:=oldexceptblock;
           end;
         getlabel(doobjectdestroy);
         emitlab(doobjectdestroyandreraise);
         emitcall('FPC_POPADDRSTACK');
         exprasmlist^.concat(new(pairegalloc,alloc(accumulator)));
         emit_reg_reg(A_MOVE,S_L,R_SPPULL,accumulator);
         exprasmlist^.concat(new(pairegalloc,dealloc(accumulator)));
         emitlabeled(A_BEQ,doobjectdestroy);
         emitcall('FPC_POPSECONDOBJECTSTACK');
         exprasmlist^.concat(new(pairegalloc,alloc(accumulator)));
         emit_reg_reg(A_MOVE,S_L,accumulator,R_SPPUSH);
         emitcall('FPC_DESTROYEXCEPTION');
         exprasmlist^.concat(new(pairegalloc,dealloc(accumulator)));
         { we don't need to restore esi here because reraise never }
         { returns                                                 }
         emitcall('FPC_RERAISE');

         emitlab(doobjectdestroy);
         cleanupobjectstack;
         { clear some stuff }
         ungetiftemp(ref);
         emitjmp(C_None,endexceptlabel);

         if assigned(p^.right) then
           begin
              { special handling for control flow instructions }
              if fc_exit in flowcontrol then
                begin
                   { the address and object pop does secondtryexcept }
                   emitlab(exitonlabel);
                   emitjmp(C_None,oldaktexitlabel);
                end;

              if fc_break in flowcontrol then
                begin
                   { the address and object pop does secondtryexcept }
                   emitlab(breakonlabel);
                   emitjmp(C_None,oldaktbreaklabel);
                end;

              if fc_continue in flowcontrol then
                begin
                   { the address and object pop does secondtryexcept }
                   emitlab(continueonlabel);
                   emitjmp(C_None,oldaktcontinuelabel);
                end;

              aktexitlabel:=oldaktexitlabel;
              aktexit2label:=oldaktexit2label;
              if assigned(oldaktbreaklabel) then
               begin
                 aktcontinuelabel:=oldaktcontinuelabel;
                 aktbreaklabel:=oldaktbreaklabel;
               end;
           end;

         emitlab(nextonlabel);
         flowcontrol:=oldflowcontrol+flowcontrol;
         { next on node }
         if assigned(p^.left) then
           begin
              cleartempgen;
              secondpass(p^.left);
           end;
      end;

{*****************************************************************************
                             SecondTryFinally
*****************************************************************************}

    procedure secondtryfinally(var p : ptree);

      var
         reraiselabel,
         finallylabel,
         endfinallylabel,
         exitfinallylabel,
         continuefinallylabel,
         breakfinallylabel,
         oldaktexitlabel,
         oldaktexit2label,
         oldaktcontinuelabel,
         oldaktbreaklabel : pasmlabel;
         oldexceptblock : ptree;
         oldflowcontrol,tryflowcontrol : tflowcontrol;
         decconst : longint;

      begin
         { check if child nodes do a break/continue/exit }
         oldflowcontrol:=flowcontrol;
         flowcontrol:=[];
         { we modify accumulator }
         usedinproc:=usedinproc + [accumulator];
         getlabel(finallylabel);
         getlabel(endfinallylabel);
         getlabel(reraiselabel);

         { the finally block must catch break, continue and exit }
         { statements                                            }
         oldaktexitlabel:=aktexitlabel;
         oldaktexit2label:=aktexit2label;
         getlabel(exitfinallylabel);
         aktexitlabel:=exitfinallylabel;
         aktexit2label:=exitfinallylabel;
         if assigned(aktbreaklabel) then
          begin
            oldaktcontinuelabel:=aktcontinuelabel;
            oldaktbreaklabel:=aktbreaklabel;
            getlabel(breakfinallylabel);
            getlabel(continuefinallylabel);
            aktcontinuelabel:=continuefinallylabel;
            aktbreaklabel:=breakfinallylabel;
          end;

         push_int(1); { Type of stack-frame must be pushed}
         emitcall('FPC_PUSHEXCEPTADDR');
         { allocate acc. }
         exprasmlist^.concat(new(pairegalloc,alloc(accumulator)));
         emit_reg_reg(A_MOVE,S_L,accumulator,R_SPPUSH);
         emitcall('FPC_SETJMP');
         emit_reg_reg(A_MOVE,S_L,accumulator,R_SPPUSH);
         emit_reg(A_TST,S_L,accumulator);
         { deallocate acc. }
         exprasmlist^.concat(new(pairegalloc,dealloc(accumulator)));
         emitlabeled(A_BNE,finallylabel);

         { try code }
         if assigned(p^.left) then
           begin
              oldexceptblock:=aktexceptblock;
              aktexceptblock:=p^.left;
              secondpass(p^.left);
              tryflowcontrol:=flowcontrol;
              if codegenerror then
                exit;
              aktexceptblock:=oldexceptblock;
           end;

         emitlab(finallylabel);
         emitcall('FPC_POPADDRSTACK');
         { finally code }
         oldexceptblock:=aktexceptblock;
         aktexceptblock:=p^.right;
         flowcontrol:=[];
         secondpass(p^.right);
         if flowcontrol<>[] then
           CGMessage(cg_e_control_flow_outside_finally);
         aktexceptblock:=oldexceptblock;
         if codegenerror then
           exit;
         { allocate eax }
         exprasmlist^.concat(new(pairegalloc,alloc(accumulator)));
         emit_reg_reg(A_MOVE,S_L,R_SPPULL,accumulator);
{ The following instruction can be optimized out,
  since move to data register sets the zero flag.
  - carl
         emit_reg(A_TST,S_L,accumulator);
}
         emitlabeled(A_BEQ,endfinallylabel);
         emit_const_reg(A_SUBQ,S_L,1,accumulator);
         emitlabeled(A_BEQ,reraiselabel);
         if fc_exit in tryflowcontrol then
           begin
              emit_const_reg(A_SUBQ,S_L,1,accumulator);
              emitlabeled(A_BEQ,oldaktexitlabel);
              decconst:=1;
           end
         else
           decconst:=2;
         if fc_break in tryflowcontrol then
           begin
              emit_const_reg(A_SUBQ,S_L,decconst,accumulator);
              emitlabeled(A_BEQ,oldaktbreaklabel);
              decconst:=1;
           end
         else
           inc(decconst);
         if fc_continue in tryflowcontrol then
           begin
              emit_const_reg(A_SUBQ,S_L,decconst,accumulator);
              emitlabeled(A_BEQ,oldaktcontinuelabel);
           end;
         { deallocate eax }
         exprasmlist^.concat(new(pairegalloc,dealloc(accumulator)));
         emitlab(reraiselabel);
         emitcall('FPC_RERAISE');
         { do some magic for exit,break,continue in the try block }
         if fc_exit in tryflowcontrol then
           begin
              emitlab(exitfinallylabel);
              { allocate eax }
              exprasmlist^.concat(new(pairegalloc,alloc(accumulator)));
              emit_reg_reg(A_MOVE,S_L,R_SPPULL,accumulator);
              exprasmlist^.concat(new(pairegalloc,alloc(accumulator)));
              emit_const_reg(A_MOVE,S_L,2,R_SPPUSH);
              emitjmp(C_NONE,finallylabel);
           end;
         if fc_break in tryflowcontrol then
          begin
             emitlab(breakfinallylabel);
             { allocate eax }
             exprasmlist^.concat(new(pairegalloc,alloc(accumulator)));
             emit_reg_reg(A_MOVE,S_L,R_SPPULL,accumulator);
             { deallocate acc. }
             exprasmlist^.concat(new(pairegalloc,dealloc(accumulator)));
             emit_const_reg(A_MOVE,S_L,3,R_SPPUSH);
             emitjmp(C_NONE,finallylabel);
           end;
         if fc_continue in tryflowcontrol then
           begin
              emitlab(continuefinallylabel);
              exprasmlist^.concat(new(pairegalloc,alloc(accumulator)));
              emit_reg_reg(A_MOVE,S_L,R_SPPULL,accumulator);
              exprasmlist^.concat(new(pairegalloc,alloc(accumulator)));
              emit_const_reg(A_MOVE,S_L,4,R_SPPUSH);
              emitjmp(C_NONE,finallylabel);
           end;

         emitlab(endfinallylabel);

         aktexitlabel:=oldaktexitlabel;
         aktexit2label:=oldaktexit2label;
         if assigned(aktbreaklabel) then
          begin
            aktcontinuelabel:=oldaktcontinuelabel;
            aktbreaklabel:=oldaktbreaklabel;
          end;
         flowcontrol:=oldflowcontrol+tryflowcontrol;
      end;

end.
{
  $Log: cgflw.pas,v $
  Revision 1.1.2.9  2002/10/09 13:09:02  pierre
   * adapt to the change for 32bit float in non emulation mode

  Revision 1.1.2.8  2002/09/15 16:41:50  carl
    * alignment fixes for constant dat
    * tabs now correct for m68k
    * many cg fixes (don't remember all)

  Revision 1.1.2.7  2001/09/17 13:42:34  pierre
   * fixes to remove multiple register ungetregister

  Revision 1.1.2.6  2001/04/19 11:37:35  carl
  * m68k updates (code generator compiles)

  Revision 1.1.2.5  2001/04/11 21:56:15  pierre
   * fix bug with continue inside except block

  Revision 1.1.2.4  2001/04/02 02:20:20  carl
  * Correction of emitflag2reg

  Revision 1.1.2.3  2001/03/24 21:35:05  carl
  + LOC_FPU now uses fpuregister instead of register field

  Revision 1.1.2.2  2001/03/23 01:19:36  carl
  + ported (uncompiled and untested)

}

