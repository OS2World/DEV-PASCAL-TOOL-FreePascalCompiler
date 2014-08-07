 {
    $Id: cgadd.pas,v 1.1.2.38 2002/12/12 10:39:57 pierre Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate m68k assembler for in add node

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
unit cgadd;
interface


    uses
      tree;

    procedure secondadd(var p : ptree);

implementation

    uses
      globtype,systems,
      cobjects,verbose,globals,
      symconst,symtable,aasm,types,
      hcodegen,temp_gen,pass_2,
      cpubase,cpuasm,
      cga,tgen,cgbase;

{*****************************************************************************
                                Helpers
*****************************************************************************}


 procedure processcc(p: ptree);
 const
       { process condition codes bit definitions }
       CARRY_FLAG    = $01;
       OVFL_FLAG     = $02;
       ZERO_FLAG     = $04;
       NEG_FLAG      = $08;
 var
   label1,label2: pasmlabel;
   typ : ttreetyp;
 (*************************************************************************)
 (*  Description: This routine handles the conversion of Floating point   *)
 (*  condition codes to normal cpu condition codes.                       *)
 (*************************************************************************)
 begin
      getlabel(label1);
      getlabel(label2);
      typ:=p^.treetype;
      if p^.swaped then
        case typ of
          equaln    : typ:=unequaln;
          unequaln  : typ:=equaln;
          gtn       : typ:=lten;
          gten       : typ:=ltn;
          ltn       : typ:=gten;
          lten       : typ:=gtn;
           else
             begin
               InternalError(34);
             end;
        end;
      case typ of
        equaln,unequaln: begin
                           { not equal clear zero flag }
                           emitlabeled(A_FBEQ,label1);
                           emit_const_reg(A_AND, S_B, NOT ZERO_FLAG, R_CCR);
                           emitlabeled(A_BRA,label2);
                           emitlab(label1);
                           { equal - set zero flag }
                           emit_const_reg(A_OR,S_B, ZERO_FLAG, R_CCR);
                           emitlab(label2);
                        end;
         ltn:           begin
                           emitlabeled(A_FBLT,label1);
                           { not less than       }
                           { clear N and V flags }
                           emit_const_reg(A_AND, S_B, NOT (NEG_FLAG OR OVFL_FLAG), R_CCR);
                           emitlabeled(A_BRA,label2);
                           emitlab(label1);
                           { less than }
                           emit_const_reg(
                             A_OR,S_B, NEG_FLAG, R_CCR);
                           emit_const_reg(
                             A_AND,S_B, NOT (ZERO_FLAG OR OVFL_FLAG), R_CCR);
                           emitlab(label2);
                        end;
         gtn:           begin
                           emitlabeled(A_FBGT,label1);
                           { not greater than }
                           { set Z flag       }
                           emit_const_reg(
                             A_OR, S_B, ZERO_FLAG, R_CCR);
                           emitlabeled(A_BRA,label2);
                           emitlab(label1);
                           { greater than      }
                           { set N and V flags }
                           { and clear Z flag  }
                           emit_const_reg(
                             A_OR,S_B, NEG_FLAG OR OVFL_FLAG , R_CCR);
                           emit_const_reg(
                             A_AND, S_B, Not ZERO_FLAG, R_CCR);
                           emitlab(label2);
                        end;
         gten:           begin
                           emitlabeled(A_FBGE,label1);
                           { not greater or equal }
                           { set N and clear V and Z }
                           emit_const_reg(
                             A_AND, S_B, NOT (ZERO_FLAG OR OVFL_FLAG), R_CCR);
                           emit_const_reg(
                             A_OR,S_B, NEG_FLAG, R_CCR);
                           emitlabeled(A_BRA,label2);
                           emitlab(label1);
                           { greater or equal    }
                           { clear V and N flags }
                           emit_const_reg(
                             A_AND, S_B, NOT (OVFL_FLAG OR NEG_FLAG), R_CCR);
                           emitlab(label2);
                        end;
         lten:           begin
                           emitlabeled(A_FBLE,label1);
                           { not less or equal }
                           { clear Z, N and V  }
                           emit_const_reg(
                             A_AND, S_B, NOT (ZERO_FLAG OR NEG_FLAG OR OVFL_FLAG), R_CCR);
                           emitlabeled(A_BRA,label2);
                           emitlab(label1);
                           { less or equal     }
                           { set Z and N       }
                           { and clear V       }
                           emit_const_reg(A_OR,S_B, ZERO_FLAG OR NEG_FLAG, R_CCR);
                           emit_const_reg(A_AND,S_B, NOT OVFL_FLAG, R_CCR);
                           emitlab(label2);
                        end;
           else
             begin
               InternalError(34);
             end;
      end; { end case }
 end;

    function getresflags(p : ptree;unsigned : boolean) : tresflags;

      begin
         if not(unsigned) then
           begin
              if p^.swaped then
                case p^.treetype of
                   equaln : getresflags:=F_E;
                   unequaln : getresflags:=F_NE;
                   ltn : getresflags:=F_G;
                   lten : getresflags:=F_GE;
                   gtn : getresflags:=F_L;
                   gten : getresflags:=F_LE;
                end
              else
                case p^.treetype of
                   equaln : getresflags:=F_E;
                   unequaln : getresflags:=F_NE;
                   ltn : getresflags:=F_L;
                   lten : getresflags:=F_LE;
                   gtn : getresflags:=F_G;
                   gten : getresflags:=F_GE;
                end;
           end
         else
           begin
              if p^.swaped then
                case p^.treetype of
                   equaln : getresflags:=F_E;
                   unequaln : getresflags:=F_NE;
                   ltn : getresflags:=F_A;
                   lten : getresflags:=F_AE;
                   gtn : getresflags:=F_B;
                   gten : getresflags:=F_BE;
                end
              else
                case p^.treetype of
                   equaln : getresflags:=F_E;
                   unequaln : getresflags:=F_NE;
                   ltn : getresflags:=F_B;
                   lten : getresflags:=F_BE;
                   gtn : getresflags:=F_A;
                   gten : getresflags:=F_AE;
                end;
           end;
      end;


    procedure SetResultLocation(cmpop,unsigned:boolean;var p :ptree);

      begin
         { remove temporary location if not a set or string }
         { that's a bad hack (FK) who did this ?            }
         if (p^.left^.resulttype^.deftype<>stringdef) and
            ((p^.left^.resulttype^.deftype<>setdef) or (psetdef(p^.left^.resulttype)^.settype=smallset)) and
            (p^.left^.location.loc in [LOC_MEM,LOC_REFERENCE]) then
           ungetiftemp(p^.left^.location.reference);
         if (p^.right^.resulttype^.deftype<>stringdef) and
            ((p^.right^.resulttype^.deftype<>setdef) or (psetdef(p^.right^.resulttype)^.settype=smallset)) and
            (p^.right^.location.loc in [LOC_MEM,LOC_REFERENCE]) then
           ungetiftemp(p^.right^.location.reference);
         { in case of comparison operation the put result in the flags }
         if cmpop then
           begin
              clear_location(p^.location);
              p^.location.loc:=LOC_FLAGS;
              p^.location.resflags:=getresflags(p,unsigned);
           end;
      end;


{*****************************************************************************
                                Addstring
*****************************************************************************}

    procedure addstring(var p : ptree);
      var
        pushedregs : tpushed;
        href       : treference;
        pushed,
        cmpop      : boolean;
        regstopush : tregisterset;
      begin
        { string operations are not commutative }
        if p^.swaped then
          swaptree(p);
        case pstringdef(p^.left^.resulttype)^.string_typ of
           st_ansistring:
             begin
                case p^.treetype of
                   addn:
                     begin
                        cmpop:=false;
                        secondpass(p^.left);
                        { to avoid problem with maybe_push and restore }
                        set_location(p^.location,p^.left^.location);
                        pushed:=maybe_push(p^.right^.registers32,p,false);
                        secondpass(p^.right);
                        if pushed then
                          begin
                             restore(p,false);
                             set_location(p^.left^.location,p^.location);
                          end;
                        { get the temp location, must be done before regs are
                          released/pushed because after the release the regs are
                          still used for the push (PFV) }
                        clear_location(p^.location);
                        p^.location.loc:=LOC_MEM;
                        gettempansistringreference(p^.location.reference);
                        decrstringref(cansistringdef,p^.location.reference);
                        { release used registers }
                        del_location(p^.right^.location);
                        del_location(p^.left^.location);
                        { push the still used registers }
                        saveusedregisters(pushedregs,ALL_REGISTERS);
                        { push data }
                        emitpushreferenceaddr(p^.location.reference);
                        { push data }
                        case p^.right^.location.loc of
                          LOC_REFERENCE,LOC_MEM:
                            emit_push_mem(p^.right^.location.reference);
                          LOC_REGISTER,LOC_CREGISTER:
                            emit_reg_reg(A_MOVE,S_L,p^.right^.location.register,R_SPPUSH);
                        end;
                        case p^.left^.location.loc of
                          LOC_REFERENCE,LOC_MEM:
                            emit_push_mem(p^.left^.location.reference);
                          LOC_REGISTER,LOC_CREGISTER:
                            emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,R_SPPUSH);
                        end;
                        { emit_push_loc(p^.right^.location);
                        emit_push_loc(p^.left^.location);
                        those functions release the registers
                        once more which is wrong PM }
                        emitcall('FPC_ANSISTR_CONCAT');
                        restoreusedregisters(pushedregs);
                        maybe_loadself;
                        ungetiftempansi(p^.left^.location.reference);
                        ungetiftempansi(p^.right^.location.reference);
                     end;
                   ltn,lten,gtn,gten,
                   equaln,unequaln:
                     begin
                        cmpop:=true;
                        if (p^.treetype in [equaln,unequaln]) and
                           (p^.left^.treetype=stringconstn) and
                           (p^.left^.length=0) then
                          begin
                             secondpass(p^.right);
                             { release used registers }
                             del_location(p^.right^.location);
                             del_location(p^.left^.location);
                             case p^.right^.location.loc of
                               LOC_REFERENCE,LOC_MEM:
                                 emit_ref(A_TST,S_L,newreference(p^.right^.location.reference));
                               LOC_REGISTER,LOC_CREGISTER:
                                 emit_reg(A_TST,S_L,p^.right^.location.register);
                             end;
                             ungetiftempansi(p^.left^.location.reference);
                             ungetiftempansi(p^.right^.location.reference);
                          end
                        else if (p^.treetype in [equaln,unequaln]) and
                          (p^.right^.treetype=stringconstn) and
                          (p^.right^.length=0) then
                          begin
                             secondpass(p^.left);
                             { release used registers }
                             del_location(p^.right^.location);
                             del_location(p^.left^.location);
                             case p^.right^.location.loc of
                               LOC_REFERENCE,LOC_MEM:
                                 emit_ref(A_TST,S_L,newreference(p^.left^.location.reference));
                               LOC_REGISTER,LOC_CREGISTER:
                                 emit_reg(A_TST,S_L,p^.left^.location.register);
                             end;
                             ungetiftempansi(p^.left^.location.reference);
                             ungetiftempansi(p^.right^.location.reference);
                          end
                        else
                          begin
                             secondpass(p^.left);
                             pushed:=maybe_push(p^.right^.registers32,p^.left,false);
                             secondpass(p^.right);
                             if pushed then
                               restore(p^.left,false);
                             { release used registers }
                             del_location(p^.right^.location);
                             del_location(p^.left^.location);
                             { push the still used registers }
                             saveusedregisters(pushedregs,ALL_REGISTERS);
                             { push data }
                             case p^.right^.location.loc of
                               LOC_REFERENCE,LOC_MEM:
                                 emit_push_mem(p^.right^.location.reference);
                               LOC_REGISTER,LOC_CREGISTER:
                                 emit_reg_reg(A_MOVE,S_L,p^.right^.location.register,R_SPPUSH);
                             end;
                             case p^.left^.location.loc of
                               LOC_REFERENCE,LOC_MEM:
                                 emit_push_mem(p^.left^.location.reference);
                               LOC_REGISTER,LOC_CREGISTER:
                                 emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,R_SPPUSH);
                             end;
                             emitcall('FPC_ANSISTR_COMPARE');
                             { result in LOC_FLAGS }
                             { watch out here! Restoring the register actually }
                             { changes the conditional flags!                  }
                             { therefore Test the result AFTER restoring the   }
                             { register, and do not restore the D0 register!   }
                             restoreusedregisters(pushedregs);
                             maybe_loadself;
                             emit_reg(A_TST,S_L,R_D0);
                             ungetiftempansi(p^.left^.location.reference);
                             ungetiftempansi(p^.right^.location.reference);
                          end;
                     end;
                end;
               { the result of ansicompare is signed }
               SetResultLocation(cmpop,false,p);
             end;
           st_shortstring:
             begin
                case p^.treetype of
                   addn:
                     begin
                        cmpop:=false;
                        secondpass(p^.left);
                        { if str_concat is set in expr
                          s:=s+ ... no need to create a temp string (PM) }

                        if (p^.left^.treetype<>addn) and not (p^.use_strconcat) then
                          begin

                             { can only reference be }
                             { string in register would be funny    }
                             { therefore produce a temporary string }

                             gettempofsizereference(256,href);
                             copyshortstring(href,p^.left^.location.reference,255,false,true);
                             { release the registers }
{                             done by copyshortstring now (JM)           }
{                             del_reference(p^.left^.location.reference); }
                             ungetiftemp(p^.left^.location.reference);

                             { does not hurt: }
                             clear_location(p^.left^.location);
                             p^.left^.location.loc:=LOC_MEM;
                             p^.left^.location.reference:=href;

                          end;

                        secondpass(p^.right);

                        { on the right we do not need the register anymore too }
                        { Instead of releasing them already, simply do not }
                        { push them (so the release is in the right place, }
                        { because emitpushreferenceaddr doesn't need extra }
                        { registers) (JM)                                  }
                        regstopush := ALL_REGISTERS;
                        remove_non_regvars_from_loc(p^.right^.location,
                              regstopush);
                        saveusedregisters(pushedregs,regstopush);
                        { push the maximum possible length of the result }
                        emitpushreferenceaddr(p^.left^.location.reference);
                        { the optimizer can more easily put the          }
                        { deallocations in the right place if it happens }
                        { too early than when it happens too late (if    }
                        { the pushref needs a "lea (..),edi; push edi")  }
                        del_reference(p^.right^.location.reference);
                        emitpushreferenceaddr(p^.right^.location.reference);
                        emitcall('FPC_SHORTSTR_CONCAT');
                        ungetiftemp(p^.right^.location.reference);
                        maybe_loadself;
                        restoreusedregisters(pushedregs);
                        set_location(p^.location,p^.left^.location);
                     end;
                   ltn,lten,gtn,gten,
                   equaln,unequaln :
                     begin
                        cmpop:=true;
                        { generate better code for s='' and s<>'' }
                        if (p^.treetype in [equaln,unequaln]) and
                           (((p^.left^.treetype=stringconstn) and (str_length(p^.left)=0)) or
                            ((p^.right^.treetype=stringconstn) and (str_length(p^.right)=0))) then
                          begin
                             secondpass(p^.left);
                             { are too few registers free? }
                             pushed:=maybe_push(p^.right^.registers32,p^.left,false);
                             secondpass(p^.right);
                             if pushed then
                               restore(p^.left,false);
                             { only one node can be stringconstn }
                             { else pass 1 would have evaluted   }
                             { this node                         }
                             if p^.left^.treetype=stringconstn then
                               emit_ref(A_TST,S_B,newreference(p^.right^.location.reference))
                             else
                               emit_ref(A_TST,S_B,newreference(p^.left^.location.reference));
                             del_reference(p^.right^.location.reference);
                             del_reference(p^.left^.location.reference);
                          end
                        else
                          begin
                             saveusedregisters(pushedregs,ALL_REGISTERS);
                             secondpass(p^.left);
                             emitpushreferenceaddr(p^.left^.location.reference);
                             del_reference(p^.left^.location.reference);
                             secondpass(p^.right);
                             emitpushreferenceaddr(p^.right^.location.reference);
                             del_reference(p^.right^.location.reference);
                             emitcall('FPC_SHORTSTR_COMPARE');
                             { result in LOC_FLAGS }
                             { watch out here! Restoring the register actually }
                             { changes the conditional flags!                  }
                             { therefore Test the result AFTER restoring the   }
                             { register, and do not restore the D0 register!   }
                             maybe_loadself;
                             restoreusedregisters(pushedregs);
                             emit_reg(A_TST,S_L,R_D0);
                          end;
                        ungetiftemp(p^.left^.location.reference);
                        ungetiftemp(p^.right^.location.reference);
                     end;
                   else CGMessage(type_e_mismatch);
                end;
               { the result of short string compare is also signed PM }
               SetResultLocation(cmpop,false,p);
             end;
          end;
      end;


{*****************************************************************************
                                Addset
*****************************************************************************}

    procedure addset(var p : ptree);
      var
        createset,
        cmpop,
        pushed : boolean;
        href   : treference;
        pushedregs : tpushed;
        regstopush: tregisterset;
      begin
        cmpop:=false;

        { not commutative }
        if p^.swaped then
         swaptree(p);

        { optimize first loading of a set }
        if (p^.right^.treetype=setelementn) and
           not(assigned(p^.right^.right)) and
           is_emptyset(p^.left) then
         createset:=true
        else
        { optimize first loading of a set }
         begin
           createset:=false;
           secondpass(p^.left);
         end;

        { are too few registers free? }
        pushed:=maybe_push(p^.right^.registers32,p^.left,false);
        secondpass(p^.right);
        if codegenerror then
          exit;
        if pushed then
          restore(p^.left,false);

        set_location(p^.location,p^.left^.location);

        { handle operations }

        case p^.treetype of
          equaln,
        unequaln
{$IfNDef NoSetInclusion}
        ,lten, gten
{$EndIf NoSetInclusion}
                  : begin
                     cmpop:=true;
                     del_location(p^.left^.location);
                     del_location(p^.right^.location);
                     { This has to be handled in the entry and     }
                     { exit code of the routine, because restoring }
                     { the registers will corrupt the flags        }
                     {saveusedregisters(pushedregs,ALL_REGISTERS); }
{$IfNDef NoSetInclusion}
                     If (p^.treetype in [equaln, unequaln, lten]) Then
                       Begin
{$EndIf NoSetInclusion}
                         emitpushreferenceaddr(p^.right^.location.reference);
                         emitpushreferenceaddr(p^.left^.location.reference);
{$IfNDef NoSetInclusion}
                       End
                     Else  {gten = lten, if the arguments are reversed}
                       Begin
                         emitpushreferenceaddr(p^.left^.location.reference);
                         emitpushreferenceaddr(p^.right^.location.reference);
                       End;
                     Case p^.treetype of
                       equaln, unequaln:
{$EndIf NoSetInclusion}
                         emitcall('FPC_SET_COMP_SETS');
{$IfNDef NoSetInclusion}
                       lten, gten:
                         Begin
                           emitcall('FPC_SET_CONTAINS_SETS');
                           { we need a jne afterwards, not a jnbe/jnae }
                           p^.treetype := equaln;
                        End;
                     End;
{$EndIf NoSetInclusion}
                     { Result will LOC_FLAGS, and we must check the }
                     maybe_loadself;
                     { we never restore the accumulator }
                     { would corrupt accumulator and flags }
                     {restoreusedregisters(pushedregs);}
                     { verify ACCUMULATOR value }
                     { since this is a boolean value      }
                     { subtract one to set the zero flag  }
                     { if they are equal.                 }
                     emit_const_reg(A_CMP,S_B,1,R_D0);
                     { ZERO FLAG = 1 = The values are equal }
                     ungetiftemp(p^.left^.location.reference);
                     ungetiftemp(p^.right^.location.reference);
                   end;
            addn : begin
                   { add can be an other SET or Range or Element ! }
                     { del_location(p^.right^.location);
                       done in pushsetelement below PM

                     And someone added it again because those registers must
                     not be pushed by the pushusedregisters, however this
                     breaks the optimizer (JM)

                     del_location(p^.right^.location);
                     pushusedregisters(pushedregs,$ffff);}

                     regstopush := ALL_REGISTERS;
                     remove_non_regvars_from_loc(p^.right^.location,regstopush);
                     if (p^.right^.treetype = setelementn) and
                      assigned(p^.right^.right) then
                        remove_non_regvars_from_loc(p^.right^.right^.location,regstopush);

                     remove_non_regvars_from_loc(p^.left^.location,regstopush);
                     saveusedregisters(pushedregs,regstopush);
                     { this is still right before the instruction that uses }
                     { p^.left^.location, but that can be fixed by the      }
                     { optimizer. There must never be an additional         }
                     { between the release and the use, because that is not }
                     { detected/fixed. As Pierre said above, p^.right^.loc  }
                     { will be released in pushsetelement (JM)              }
                     del_location(p^.left^.location);
                     href.symbol:=nil;
                     gettempofsizereference(32,href);
                     if createset then
                      begin
                        pushsetelement(p^.right^.left);
                        emitpushreferenceaddr(href);
                        emitcall('FPC_SET_CREATE_ELEMENT');
                      end
                     else
                      begin
                      { add a range or a single element? }
                        if p^.right^.treetype=setelementn then
                         begin
                           concatcopy(p^.left^.location.reference,href,32,false,false);
                           if assigned(p^.right^.right) then
                            begin
                              pushsetelement(p^.right^.right);
                              pushsetelement(p^.right^.left);
                              emitpushreferenceaddr(href);
                              emitcall('FPC_SET_SET_RANGE');
                            end
                           else
                            begin
                              pushsetelement(p^.right^.left);
                              emitpushreferenceaddr(href);
                              emitcall('FPC_SET_SET_BYTE');
                            end;
                         end
                        else
                         begin
                         { must be an other set }
                           emitpushreferenceaddr(href);
                           emitpushreferenceaddr(p^.right^.location.reference);
                           emitpushreferenceaddr(p^.left^.location.reference);
                           emitcall('FPC_SET_ADD_SETS');
                         end;
                      end;
                     maybe_loadself;
                     restoreusedregisters(pushedregs);
                     ungetiftemp(p^.left^.location.reference);
                     ungetiftemp(p^.right^.location.reference);
                     p^.location.loc:=LOC_MEM;
                     p^.location.reference:=href;
                   end;
            subn,
         symdifn,
            muln : begin
                     { Find out which registers have to pushed (JM) }
                     regstopush := ALL_REGISTERS;
                     remove_non_regvars_from_loc(p^.left^.location,regstopush);
                     remove_non_regvars_from_loc(p^.right^.location,regstopush);
                     { Push them (JM) }
                     saveusedregisters(pushedregs,regstopush);
                     href.symbol:=nil;
                     gettempofsizereference(32,href);
                     emitpushreferenceaddr(href);
                     { Release the registers right before they're used,  }
                     { see explanation in cgai386.pas:loadansistring for }
                     { info why this is done right before the push (JM)  }
                     del_location(p^.right^.location);
                     emitpushreferenceaddr(p^.right^.location.reference);
                     { The same here }
                     del_location(p^.left^.location);
                     emitpushreferenceaddr(p^.left^.location.reference);
                     case p^.treetype of
                      subn : emitcall('FPC_SET_SUB_SETS');
                   symdifn : emitcall('FPC_SET_SYMDIF_SETS');
                      muln : emitcall('FPC_SET_MUL_SETS');
                     end;
                     maybe_loadself;
                     restoreusedregisters(pushedregs);
                     ungetiftemp(p^.left^.location.reference);
                     ungetiftemp(p^.right^.location.reference);
                     p^.location.loc:=LOC_MEM;
                     p^.location.reference:=href;
                   end;
        else
          CGMessage(type_e_mismatch);
        end;
        SetResultLocation(cmpop,true,p);
      end;


{*****************************************************************************
                                SecondAdd
*****************************************************************************}

    procedure secondadd(var p : ptree);
    { is also being used for xor, and "mul", "sub, or and comparative }
    { operators                                                }

      label do_normal;

      var
         unusedregisters : tregisterset;
         usablecount : byte;
         tmpref : preference;
         fpureg,fpureg2,hregister,hregister2 : tregister;
         noswap,popeax,popedx,
         pushed,pushedfpu,
         mboverflow,cmpop : boolean;
         op,op2 : tasmop;
         flags : tresflags;
         otl,ofl : pasmlabel;
         power : longint;
         opsize : topsize;
         hl4: pasmlabel;
         hr : preference;

         { true, if unsigned types are compared }
         unsigned : boolean;
         { true, if a small set is handled with the longint code }
         is_set : boolean;
         { is_in_dest if the result is put directly into }
         { the resulting refernce or varregister }
         is_in_dest : boolean;
         { true, if for sets subtractions the extra not should generated }
         extra_not : boolean;
         { indicates if this is a 64-bit value FPU or integer }
         is64bit : boolean;

         pushedreg : tpushed;
         regstopush: tregisterset;

      procedure firstjmp64bitcmp;

        var
           oldtreetype : ttreetyp;

        begin
           { the jump the sequence is a little bit hairy }
           case p^.treetype of
              ltn,gtn:
                begin
                   emitlabeled(flag_2_jmp[getresflags(p,unsigned)],truelabel);
                   { cheat a little bit for the negative test }
                   p^.swaped:=not(p^.swaped);
                   emitlabeled(flag_2_jmp[getresflags(p,unsigned)],falselabel);
                   p^.swaped:=not(p^.swaped);
                end;
              lten,gten:
                begin
                   oldtreetype:=p^.treetype;
                   if p^.treetype=lten then
                     p^.treetype:=ltn
                   else
                     p^.treetype:=gtn;
                   emitlabeled(flag_2_jmp[getresflags(p,unsigned)],truelabel);
                   { cheat for the negative test }
                   if p^.treetype=ltn then
                     p^.treetype:=gtn
                   else
                     p^.treetype:=ltn;
                   emitlabeled(flag_2_jmp[getresflags(p,unsigned)],falselabel);
                   p^.treetype:=oldtreetype;
                end;
              equaln:
                emitlabeled(A_BNE,falselabel);
              unequaln:
                emitlabeled(A_BNE,truelabel);
           end;
        end;

      procedure secondjmp64bitcmp;

        begin
           { the jump the sequence is a little bit hairy }
           case p^.treetype of
              ltn,gtn,lten,gten:
                begin
                   { the comparisaion of the low dword have to be }
                   {  always unsigned!                            }
                   emitlabeled(flag_2_jmp[getresflags(p,true)],truelabel);
                   emitjmp(C_None,falselabel);
                end;
              equaln:
                begin
                   emitlabeled(A_BNE,falselabel);
                   emitjmp(C_None,truelabel);
                end;
              unequaln:
                begin
                   emitlabeled(A_BNE,truelabel);
                   emitjmp(C_None,falselabel);
                end;
           end;
        end;

      begin
      { to make it more readable, string and set (not smallset!) have their
        own procedures }
         case p^.left^.resulttype^.deftype of
         stringdef : begin
                       addstring(p);
                       exit;
                     end;
            setdef : begin
                     { normalsets are handled separate }
                       if not(psetdef(p^.left^.resulttype)^.settype=smallset) then
                        begin
                          addset(p);
                          exit;
                        end;
                     end;
         end;

         { defaults }
         unsigned:=false;
         is_in_dest:=false;
         extra_not:=false;
         noswap:=false;
         opsize:=S_L;

         { are we a (small)set, must be set here because the side can be
           swapped ! (PFV) }
         is_set:=(p^.left^.resulttype^.deftype=setdef);

         { calculate the operator which is more difficult }
         firstcomplex(p);

         { handling boolean expressions extra: }
         if is_boolean(p^.left^.resulttype) and
            is_boolean(p^.right^.resulttype) then
           begin
             if (porddef(p^.left^.resulttype)^.typ=bool8bit) or
                (porddef(p^.right^.resulttype)^.typ=bool8bit) then
               opsize:=S_B
             else
               if (porddef(p^.left^.resulttype)^.typ=bool16bit) or
                  (porddef(p^.right^.resulttype)^.typ=bool16bit) then
                 opsize:=S_W
             else
               opsize:=S_L;
             { boolean operations }
             case p^.treetype of
              andn,
               orn : begin
                       clear_location(p^.location);
                       p^.location.loc:=LOC_JUMP;
                       cmpop:=false;
                       case p^.treetype of
                        andn : begin
                                  otl:=truelabel;
                                  getlabel(truelabel);
                                  secondpass(p^.left);
                                  maketojumpbool(p^.left);
                                  emitlab(truelabel);
                                  truelabel:=otl;
                               end;
                        orn : begin
                                 ofl:=falselabel;
                                 getlabel(falselabel);
                                 secondpass(p^.left);
                                 maketojumpbool(p^.left);
                                 emitlab(falselabel);
                                 falselabel:=ofl;
                              end;
                       else
                         CGMessage(type_e_mismatch);
                       end;
                       secondpass(p^.right);
                       maketojumpbool(p^.right);
                     end;
          unequaln,ltn,lten,gtn,gten,
       equaln,xorn : begin
                       if (p^.left^.treetype=ordconstn) or (p^.left^.treetype=realconstn) then
                        swaptree(p);
                       if p^.left^.location.loc=LOC_JUMP then
                         begin
                            otl:=truelabel;
                            getlabel(truelabel);
                            ofl:=falselabel;
                            getlabel(falselabel);
                         end;

                       secondpass(p^.left);
                       { if in flags then copy first to register, because the
                         flags can be destroyed }
                       case p^.left^.location.loc of
                          LOC_FLAGS:
                            locflags2reg(p^.left^.location,opsize);
                          LOC_JUMP:
                            locjump2reg(p^.left^.location,opsize, otl, ofl);
                       end;
                       set_location(p^.location,p^.left^.location);
                       pushed:=maybe_push(p^.right^.registers32,p,false);
                       if p^.right^.location.loc=LOC_JUMP then
                         begin
                            otl:=truelabel;
                            getlabel(truelabel);
                            ofl:=falselabel;
                            getlabel(falselabel);
                         end;
                       secondpass(p^.right);
                       if pushed then
                         begin
                            restore(p,false);
                            set_location(p^.left^.location,p^.location);
                         end;
                       case p^.right^.location.loc of
                          LOC_FLAGS:
                            locflags2reg(p^.right^.location,opsize);
                          LOC_JUMP:
                            locjump2reg(p^.right^.location,opsize,otl,ofl);
                       end;
                       goto do_normal;
                    end
             else
               CGMessage(type_e_mismatch);
             end
           end
         else
           begin
              { in case of constant put it to the left }
              if (p^.left^.treetype=ordconstn) or
                 (p^.left^.treetype=realconstn) or
                 ((p^.location.loc=LOC_FPU) and
                  (p^.right^.registersfpu > p^.left^.registersfpu)) or
                 ((p^.location.loc<>LOC_FPU) and
                  (p^.right^.registers32 > p^.left^.registers32)) then
               swaptree(p);
              secondpass(p^.left);
              { this will be complicated as
               a lot of code below assumes that
               p^.location and p^.left^.location are the same }

              set_location(p^.location,p^.left^.location);

              { if we are playing with floating point registers }
              { save them is required.                          }
              if p^.left^.resulttype^.deftype = floatdef then
                begin
                  { HACK!!! TERRIBLE TO OVERCOME PROBLEM WITH FPU }
                  { ALLOCATOR WITH ACCUMULATOR!                   }
                  if (p^.left^.location.loc=LOC_FPU) then
                    pushed := maybe_push_float(p^.right^.registers32,
                      p^.right^.registersfpu,p^.left)
                  else
                    pushed:=false;
                  if (p^.left^.location.loc=LOC_FPU) and
                     (p^.left^.location.fpuregister=R_FP0) and
                     not pushed then
                    begin
                      fpureg:=getfloatreg;
                      emit_reg_reg(A_FMOVE,S_FX,R_FP0,fpureg);
                      p^.left^.location.fpuregister:=fpureg;
                      ungetregister(R_FP0);
                    end;
                end
              else
                begin
                { original non-hacked code }
                 pushed:=maybe_push(p^.right^.registers32,p,is_64bitint(p^.left^.resulttype));
                end;
              secondpass(p^.right);
              if pushed then
                begin
                  { original non-hacked code                }
                  { will also restore floating point values }
                    restore(p,is_64bitint(p^.left^.resulttype));
                  set_location(p^.left^.location,p^.location);
                end;

              if (p^.left^.resulttype^.deftype=pointerdef) or

                 (p^.right^.resulttype^.deftype=pointerdef) or

                 ((p^.right^.resulttype^.deftype=objectdef) and
                  pobjectdef(p^.right^.resulttype)^.is_class and
                 (p^.left^.resulttype^.deftype=objectdef) and
                  pobjectdef(p^.left^.resulttype)^.is_class
                 ) or

                 (p^.left^.resulttype^.deftype=classrefdef) or

                 (p^.left^.resulttype^.deftype=procvardef) or

                 ((p^.left^.resulttype^.deftype=enumdef) and
                  (p^.left^.resulttype^.size=4)) or

                 ((p^.left^.resulttype^.deftype=orddef) and
                 (porddef(p^.left^.resulttype)^.typ=s32bit)) or
                 ((p^.right^.resulttype^.deftype=orddef) and
                 (porddef(p^.right^.resulttype)^.typ=s32bit)) or

                ((p^.left^.resulttype^.deftype=orddef) and
                 (porddef(p^.left^.resulttype)^.typ=u32bit)) or
                 ((p^.right^.resulttype^.deftype=orddef) and
                 (porddef(p^.right^.resulttype)^.typ=u32bit)) or

                { as well as small sets }
              is_set then
                begin
          do_normal:
                   mboverflow:=false;
                   cmpop:=false;
                   unsigned := not(is_signed(p^.left^.resulttype)) or
                               not(is_signed(p^.right^.resulttype));
                   case p^.treetype of
                      addn : begin
                               { this is a really ugly hack!!!!!!!!!! }
                               { this could be done later using EDI   }
                               { as it is done for subn               }
                               { instead of two registers!!!!         }
                               if is_set then
                                begin
                                { adding elements is not commutative }
                                  if p^.swaped and (p^.left^.treetype=setelementn) then
                                   swaptree(p);
                                { are we adding set elements ? }
                                  if p^.right^.treetype=setelementn then
                                   begin
                                   { no range support for smallsets! }
                                     if assigned(p^.right^.right) then
                                      internalerror(43244);
                                   { bts requires both elements to be registers }
                                     if p^.left^.location.loc in [LOC_MEM,LOC_REFERENCE] then
                                      begin
                                        ungetiftemp(p^.left^.location.reference);
                                        del_location(p^.left^.location);
                                        hregister:=getregister32;
                                        emit_ref_reg(A_MOVE,opsize,
                                          newreference(p^.left^.location.reference),hregister);
                                        clear_location(p^.left^.location);
                                        p^.left^.location.loc:=LOC_REGISTER;
                                        p^.left^.location.register:=hregister;
                                        set_location(p^.location,p^.left^.location);
                                      end;
                                     if p^.right^.location.loc in [LOC_MEM,LOC_REFERENCE] then
                                      begin
                                        ungetiftemp(p^.right^.location.reference);
                                        del_location(p^.right^.location);
                                        hregister:=getregister32;
                                        emit_load_loc_reg(p^.right^.location,
                                          p^.right^.resulttype,u32bitdef,hregister);
                                        clear_location(p^.right^.location);
                                        p^.right^.location.loc:=LOC_REGISTER;
                                        p^.right^.location.register:=hregister;
                                      end;
                                     op:=A_BSET;
                                     noswap:=true;
                                   end
                                  else
                                   op:=A_OR;
                                   mboverflow:=false;
                                   unsigned:=false;
                                end
                               else
                                begin
                                  op:=A_ADD;
                                  mboverflow:=true;
                                end;
                             end;
                   symdifn : begin
                               { the symetric diff is only for sets }
                               if is_set then
                                begin
                                  op:=A_EOR;
                                  mboverflow:=false;
                                  unsigned:=false;
                                end
                               else
                                CGMessage(type_e_mismatch);
                             end;
                      muln : begin
                               if is_set then
                                begin
                                  op:=A_AND;
                                  mboverflow:=false;
                                  unsigned:=false;
                                end
                               else
                                begin
                                  if unsigned then
                                   op:=A_MULU
                                  else
                                   op:=A_MULS;
                                  mboverflow:=true;
                                end;
                             end;
                      subn : begin
                               if is_set then
                                begin
                                  op:=A_AND;
                                  mboverflow:=false;
                                  unsigned:=false;
{$IfNDef NoSetConstNot}
                                  If (not p^.swaped) and
                                     (p^.right^.treetype = setconstn) and
                                     (p^.right^.location.reference.is_immediate) then
                                    p^.right^.location.reference.offset := not(p^.right^.location.reference.offset)
                                  Else
                                  If (p^.swaped) and
                                     (p^.left^.treetype = setconstn) and
                                     (p^.left^.location.reference.is_immediate) then
                                    p^.left^.location.reference.offset := not(p^.left^.location.reference.offset)
                                  Else
{$EndIf NoNosetConstNot}
                                    extra_not:=true;
                                end
                               else
                                begin
                                  op:=A_SUB;
                                  mboverflow:=true;
                                end;
                             end;
                  ltn,lten,
                  gtn,gten,
           equaln,unequaln : begin
{$IfNDef NoSetInclusion}
                               If is_set Then
                                 Case p^.treetype of
                                   lten,gten:
                                     Begin
                                      If (not(p^.swaped) and
                                          (p^.treetype = lten)) or
                                         (p^.swaped and
                                          (p^.treetype = gten)) then
                                        swaptree(p);
                                      if p^.left^.location.loc in [LOC_MEM,LOC_REFERENCE] then
                                        begin
                                         ungetiftemp(p^.left^.location.reference);
                                         del_reference(p^.left^.location.reference);
                                         hregister:=getregister32;
                                         emit_ref_reg(A_MOVE,opsize,
                                           newreference(p^.left^.location.reference),hregister);
                                         clear_location(p^.left^.location);
                                         p^.left^.location.loc:=LOC_REGISTER;
                                         p^.left^.location.register:=hregister;
                                         set_location(p^.location,p^.left^.location);
                                       end
                                      else
                                       if p^.left^.location.loc = LOC_CREGISTER Then
                                        {save the register var in a temp register, because
                                          its value is going to be modified}
                                          begin
                                            hregister := getregister32;
                                            emit_reg_reg(A_MOVE,opsize,
                                              p^.left^.location.register,hregister);
                                             clear_location(p^.left^.location);
                                             p^.left^.location.loc:=LOC_REGISTER;
                                             p^.left^.location.register:=hregister;
                                             set_location(p^.location,p^.left^.location);
                                           end;
                                     {here, p^.left^.location should be LOC_REGISTER}
                                      If p^.right^.location.loc in [LOC_MEM,LOC_REFERENCE] Then
                                         emit_ref_reg(A_AND,opsize,
                                           newreference(p^.right^.location.reference),p^.left^.location.register)
                                      Else
                                        emit_reg_reg(A_AND,opsize,
                                          p^.right^.location.register,p^.left^.location.register);
                {warning: ugly hack ahead: we need a "jne" after the cmp, so
                 change the treetype from lten/gten to equaln}
                                      p^.treetype := equaln
                                     End;
                           {no < or > support for sets}
                                   ltn,gtn: CGMessage(type_e_mismatch);
                                 End;
{$EndIf NoSetInclusion}
                               op:=A_CMP;
                               cmpop:=true;
                             end;
                      xorn : op:=A_EOR;
                       orn : op:=A_OR;
                      andn : op:=A_AND;
                   else
                     CGMessage(type_e_mismatch);
                   end;


                   { Convert flags to register first }
                   if (p^.left^.location.loc=LOC_FLAGS) then
                    locflags2reg(p^.left^.location,opsize);
                   if (p^.right^.location.loc=LOC_FLAGS) then
                    locflags2reg(p^.right^.location,opsize);

                   { left and right no register?  }
                   { then one must be demanded    }
                   if (p^.left^.location.loc<>LOC_REGISTER) and
                      (p^.right^.location.loc<>LOC_REGISTER) then
                     begin
                        { register variable ? }
                        if (p^.left^.location.loc=LOC_CREGISTER) then
                          begin
                             { it is OK if this is the destination }
                             if is_in_dest then
                               begin
                                  hregister:=p^.location.register;
                                  emit_reg_reg(A_MOVE,opsize,p^.left^.location.register,
                                    hregister);
                               end
                             else
                             if cmpop then
                               begin
                                  { do not disturb the register }
                                  hregister:=p^.location.register;
                               end
                             else
                               begin
                                  case opsize of
                                     S_L : hregister:=getregister32;
                                     S_B : hregister:=getregister32;
                                  end;
                                  emit_reg_reg(A_MOVE,opsize,p^.left^.location.register,
                                    hregister);
                               end
                          end
                        else
                          begin
                             ungetiftemp(p^.left^.location.reference);
                             del_reference(p^.left^.location.reference);
                             if is_in_dest then
                               begin
                                  hregister:=p^.location.register;
                                  emit_ref_reg(A_MOVE,opsize,
                                    newreference(p^.left^.location.reference),hregister);
                               end
                             else
                               begin
                                  { first give free, then demand new register }
                                  case opsize of
                                     S_L : hregister:=getregister32;
                                     S_W : hregister:=getregister32;
                                     S_B : hregister:=getregister32;
                                  end;
                                  emit_ref_reg(A_MOVE,opsize,
                                    newreference(p^.left^.location.reference),hregister);
                               end;
                          end;
                        clear_location(p^.location);
                        p^.location.loc:=LOC_REGISTER;
                        p^.location.register:=hregister;
                     end
                   else
                     { if on the right the register then swap }
                     if not(noswap) and (p^.right^.location.loc=LOC_REGISTER) then
                       begin
                          swap_location(p^.location,p^.right^.location);

                          { newly swapped also set swapped flag }
                          p^.swaped:=not(p^.swaped);
                       end;
                   { at this point, p^.location.loc should be LOC_REGISTER }
                   { and p^.location.register should be a valid register   }
                   { containing the left result                     }

                    if p^.right^.location.loc<>LOC_REGISTER then
                     begin
                        if (p^.treetype=subn) and p^.swaped then
                          begin
                             if p^.right^.location.loc=LOC_CREGISTER then
                               begin
                                  if extra_not then
                                    emit_reg(A_NOT,opsize,p^.location.register);
                                  getexplicitregister32(R_D1);
                                  emit_reg_reg(A_MOVE,opsize,p^.right^.location.register,R_D1);
                                  emit_reg_reg(op,opsize,p^.location.register,R_D1);
                                  emit_reg_reg(A_MOVE,opsize,R_D1,p^.location.register);
                                  ungetregister32(R_D1);
                               end
                             else
                               begin
                                  if extra_not then
                                    emit_reg(A_NOT,opsize,p^.location.register);
                                  getexplicitregister32(R_D1);
                                  emit_ref_reg(A_MOVE,opsize,newreference(p^.right^.location.reference),R_D1);
                                  emit_reg_reg(op,opsize,p^.location.register,R_D1);
                                  emit_reg_reg(A_MOVE,opsize,R_D1,p^.location.register);
                                  ungetregister32(R_D1);
                                  ungetiftemp(p^.right^.location.reference);
                                  del_reference(p^.right^.location.reference);
                               end;
                          end
                        else
                          begin
                             if (p^.right^.treetype=ordconstn) and
                                (op=A_CMP) and
                                (p^.right^.value=0) then
                               begin
                                  emit_reg(A_TST,opsize,p^.location.register);
                               end
                             else if (p^.right^.treetype=ordconstn) and
                                (op=A_ADD) and
                                (p^.right^.value=1) and
                                not(cs_check_overflow in aktlocalswitches) then
                               begin
                                  emit_const_reg(A_ADD,opsize,1,p^.location.register);
                               end
                             else if (p^.right^.treetype=ordconstn) and
                                (op=A_SUB) and
                                (p^.right^.value=1) and
                                not(cs_check_overflow in aktlocalswitches) then
                               begin
                                  emit_const_reg(A_SUB,opsize,1,p^.location.register);
                               end
                             else if (p^.right^.treetype=ordconstn) and
                                (op=A_MULS) and
                                (ispowerof2(p^.right^.value,power)) and
                                not(cs_check_overflow in aktlocalswitches) then
                               begin
                                    if (power <= 8) then
                                      begin
                                        if power<>0 then
                                          emit_const_reg(A_ASL,opsize,power,p^.location.register)
                                      end
                                    else
                                      begin
                                        getexplicitregister32(R_D1);
                                        emit_const_reg(A_MOVE,S_L,power,R_D1);
                                        emit_reg_reg(A_ASL,opsize,R_D1,p^.location.register);
                                        ungetregister(R_D1);
                                      end;
                               end
                             else
                               begin
                                  if (p^.right^.location.loc=LOC_CREGISTER) then
                                    begin
                                       if extra_not then
                                         begin
                                            getexplicitregister32(R_D1);
                                            emit_reg_reg(A_MOVE,S_L,p^.right^.location.register,R_D1);
                                            emit_reg(A_NOT,S_L,R_D1);
                                            emit_reg_reg(A_AND,S_L,R_D1,
                                              p^.location.register);
                                            ungetregister32(R_D1);
                                         end
                                       else
                                         begin
                                           if (op=A_MULS) and (opsize = S_L) and (aktoptprocessor=MC68000) then
                                           { Emulation for MC68000 }
                                            begin
                                              getexplicitregister32(R_D0);
                                              getexplicitregister32(R_D1);
                                              emit_reg_reg(A_MOVE,S_L,p^.right^.location.register,R_D0);
                                              emit_reg_reg(A_MOVE,S_L,p^.location.register,R_D1);
                                              emitcall('FPC_MUL_LONGINT');
                                              emit_reg_reg(A_MOVE,S_L,R_D0,p^.location.register);
                                              ungetregister(R_D0);
                                              ungetregister(R_D1);
                                            end
                                           else
                                           if (op=A_MULU) and (opsize = S_L) and (aktoptprocessor=MC68000) then
                                            begin
                                              getexplicitregister32(R_D0);
                                              getexplicitregister32(R_D1);
                                              emit_reg_reg(A_MOVE,S_L,p^.right^.location.register,R_D0);
                                              emit_reg_reg(A_MOVE,S_L,p^.location.register,R_D1);
                                              emitcall('FPC_MUL_CARDINAL');
                                              emit_reg_reg(A_MOVE,S_L,R_D0,p^.location.register);
                                              ungetregister(R_D0);
                                              ungetregister(R_D1);
                                            end
                                           else
                                              emit_reg_reg(op,opsize,p^.right^.location.register,
                                                p^.location.register);
                                         end;
                                    end
                                  else
                                    begin
                                       if extra_not then
                                         begin
                                            getexplicitregister32(R_D1);
                                            emit_ref_reg(A_MOVE,S_L,newreference(
                                              p^.right^.location.reference),R_D1);
                                            emit_reg(A_NOT,S_L,R_D1);
                                            emit_reg_reg(A_AND,S_L,R_D1,p^.location.register);
                                            ungetregister32(R_D1);
                                         end
                                       else
                                         begin
                                           if (op=A_MULS) and (opsize = S_L) and (aktoptprocessor=MC68000) then
                                            { Emulation for MC68000 }
                                            begin
                                              getexplicitregister32(R_D0);
                                              getexplicitregister32(R_D1);
                                              emit_ref_reg(A_MOVE, opsize,newreference(p^.right^.location.reference),R_D1);
                                              emit_reg_reg(A_MOVE,S_L,p^.location.register,R_D0);
                                              emitcall('FPC_MUL_LONGINT');
                                              emit_reg_reg(A_MOVE,S_L,R_D0,p^.location.register);
                                              ungetregister(R_D0);
                                              ungetregister(R_D1);
                                            end
                                           else
                                           if (op=A_MULU) and (opsize = S_L) and (aktoptprocessor=MC68000) then
                                            begin
                                              getexplicitregister32(R_D0);
                                              getexplicitregister32(R_D1);
                                              emit_ref_reg(A_MOVE, opsize,newreference(p^.right^.location.reference),R_D1);
                                              emit_reg_reg(A_MOVE,S_L,p^.location.register,R_D0);
                                              emitcall('FPC_MUL_CARDINAL');
                                              emit_reg_reg(A_MOVE,S_L,R_D0,p^.location.register);
                                              ungetregister(R_D0);
                                              ungetregister(R_D1);
                                            end
                                           else
                                            { When one of the source/destination is a memory reference  }
                                            { and the operator is EOR, the we must load it into the     }
                                            { value into a register first since only EOR reg,reg exists }
                                            { on the m68k                                               }
                                            if (op=A_EOR) then
                                              begin
                                                getexplicitregister32(R_D0);
                                                emit_ref_reg(A_MOVE,opsize,newreference(p^.right^.location.reference),R_D0);
                                                emit_reg_reg(op,opsize,R_D0,p^.location.register);
                                                ungetregister(R_D0);
                                              end
                                            else
                                              emit_ref_reg(op,opsize,
                                                newreference(p^.right^.location.reference),p^.location.register);

{                                            emit_ref_reg(op,opsize,newreference(
                                              p^.right^.location.reference),p^.location.register);}
                                         end;
                                       ungetiftemp(p^.right^.location.reference);
                                       del_reference(p^.right^.location.reference);
                                    end;
                               end;
                          end;
                     end
                   else
                     begin
                        { when swapped another result register }
                        if (p^.treetype=subn) and p^.swaped then
                          begin
                             if extra_not then
                               emit_reg(A_NOT,S_L,p^.location.register);

                             emit_reg_reg(op,opsize,
                               p^.location.register,p^.right^.location.register);
                               swap_location(p^.location,p^.right^.location);
                               { newly swapped also set swapped flag }
                               { just to maintain ordering         }
                               p^.swaped:=not(p^.swaped);
                          end
                        else
                          begin
                             if extra_not then
                               emit_reg(A_NOT,S_L,p^.right^.location.register);

                             if (op=A_MULS) and (opsize = S_L) and (aktoptprocessor=MC68000) then
                             { Emulation for MC68000 }
                               begin
                                 getexplicitregister32(R_D0);
                                 getexplicitregister32(R_D1);
                                 emit_reg_reg(A_MOVE, S_L,p^.right^.location.register,R_D1);
                                 emit_reg_reg(A_MOVE,S_L,p^.location.register,R_D0);
                                 emitcall('FPC_MUL_LONGINT');
                                 emit_reg_reg(A_MOVE,S_L,R_D0,p^.location.register);
                                 ungetregister(R_D0);
                                 ungetregister(R_D1);
                               end
                             else
                             if (op=A_MULU) and (opsize = S_L) and (aktoptprocessor=MC68000) then
                               begin
                                 getexplicitregister32(R_D0);
                                 getexplicitregister32(R_D1);
                                 emit_reg_reg(A_MOVE, S_L,p^.right^.location.register,R_D1);
                                 emit_reg_reg(A_MOVE,S_L,p^.location.register,R_D0);
                                 emitcall('FPC_MUL_CARDINAL');
                                 emit_reg_reg(A_MOVE,S_L,R_D0,p^.location.register);
                                 ungetregister(R_D0);
                                 ungetregister(R_D1);
                               end
                             else
                               emit_reg_reg(op,opsize,p^.right^.location.register,p^.location.register);

                          end;
                        case opsize of
                           S_L : ungetregister32(p^.right^.location.register);
                           S_B : ungetregister32(p^.right^.location.register);
                        end;
                     end;

                   if cmpop then
                     case opsize of
                        S_L : ungetregister32(p^.location.register);
                        S_B : ungetregister32(p^.location.register);
                     end;

                   { only in case of overflow operations }
                   { produce overflow code               }
                   { we must put it here directly, because sign of operation }
                   { is in unsigned VAR!!                                    }
                   if mboverflow then
                    begin
                      if cs_check_overflow in aktlocalswitches  then
                       begin
                         getlabel(hl4);
                         if unsigned then
                          emitlabeled(A_BCC,hl4)
                         else
                          emitlabeled(A_BVC,hl4);
                         emitcall('FPC_OVERFLOW');
                         emitlab(hl4);
                       end;
                    end;
                end
              else

              { Char type }
                if ((p^.left^.resulttype^.deftype=orddef) and
                    (porddef(p^.left^.resulttype)^.typ=uchar)) or
              { enumeration type 16 bit }
                   ((p^.left^.resulttype^.deftype=enumdef) and
                    (p^.left^.resulttype^.size=1)) then
                 begin
                   case p^.treetype of
                      ltn,lten,gtn,gten,
                      equaln,unequaln :
                                cmpop:=true;
                      else CGMessage(type_e_mismatch);
                   end;
                   unsigned:=true;
                   { left and right no register? }
                   { the one must be demanded    }
                   if (p^.location.loc<>LOC_REGISTER) and
                     (p^.right^.location.loc<>LOC_REGISTER) then
                     begin
                        if p^.location.loc=LOC_CREGISTER then
                          begin
                             if cmpop then
                               { do not disturb register }
                               hregister:=p^.location.register
                             else
                               begin
                                  hregister:=getregister32;
                                  emit_reg_reg(A_MOVE,S_B,p^.location.register,
                                    hregister);
                               end;
                          end
                        else
                          begin
                             del_reference(p^.location.reference);

                             { first give free then demand new register }
                             hregister:=getregister32;
                             emit_ref_reg(A_MOVE,S_B,newreference(p^.location.reference),
                               hregister);
                          end;
                        clear_location(p^.location);
                        p^.location.loc:=LOC_REGISTER;
                        p^.location.register:=hregister;
                     end;

                   { now p always a register }

                   if (p^.right^.location.loc=LOC_REGISTER) and
                      (p^.location.loc<>LOC_REGISTER) then
                     begin
                       swap_location(p^.location,p^.right^.location);
                       { newly swapped also set swapped flag }
                       p^.swaped:=not(p^.swaped);
                     end;

                   if p^.right^.location.loc<>LOC_REGISTER then
                     begin
                        if p^.right^.location.loc=LOC_CREGISTER then
                          begin
                             emit_reg_reg(A_CMP,S_B,
                                p^.right^.location.register,p^.location.register);
                          end
                        else
                          begin
                             emit_ref_reg(A_CMP,S_B,newreference(
                                p^.right^.location.reference),p^.location.register);
                             del_reference(p^.right^.location.reference);
                          end;
                     end
                   else
                     begin
                        emit_reg_reg(A_CMP,S_B,p^.right^.location.register,
                          p^.location.register);
                        ungetregister32(p^.right^.location.register);
                     end;
                   ungetregister32(p^.location.register);
                end
              else
              { 16 bit enumeration type }
                if ((p^.left^.resulttype^.deftype=enumdef) and
                    (p^.left^.resulttype^.size=2)) then
                 begin
                   case p^.treetype of
                      ltn,lten,gtn,gten,
                      equaln,unequaln :
                                cmpop:=true;
                      else CGMessage(type_e_mismatch);
                   end;
                   unsigned:=true;
                   { left and right no register? }
                   { the one must be demanded    }
                   if (p^.location.loc<>LOC_REGISTER) and
                     (p^.right^.location.loc<>LOC_REGISTER) then
                     begin
                        if p^.location.loc=LOC_CREGISTER then
                          begin
                             if cmpop then
                               { do not disturb register }
                               hregister:=p^.location.register
                             else
                               begin
                                  hregister:=getregister32;
                                  emit_reg_reg(A_MOVE,S_W,p^.location.register,hregister);
                               end;
                          end
                        else
                          begin
                             del_reference(p^.location.reference);

                             { first give free then demand new register }
                             hregister:=getregister32;
                             emit_ref_reg(A_MOVE,S_W,newreference(p^.location.reference),
                               hregister);
                          end;
                        clear_location(p^.location);
                        p^.location.loc:=LOC_REGISTER;
                        p^.location.register:=hregister;
                     end;

                   { now p always a register }

                   if (p^.right^.location.loc=LOC_REGISTER) and
                      (p^.location.loc<>LOC_REGISTER) then
                     begin
                       swap_location(p^.location,p^.right^.location);
                       { newly swapped also set swapped flag }
                       p^.swaped:=not(p^.swaped);
                     end;

                   if p^.right^.location.loc<>LOC_REGISTER then
                     begin
                        if p^.right^.location.loc=LOC_CREGISTER then
                          begin
                             emit_reg_reg(A_CMP,S_W,
                                p^.right^.location.register,p^.location.register);
                          end
                        else
                          begin
                             emit_ref_reg(A_CMP,S_W,newreference(
                                p^.right^.location.reference),p^.location.register);
                             del_reference(p^.right^.location.reference);
                          end;
                     end
                   else
                     begin
                        emit_reg_reg(A_CMP,S_W,p^.right^.location.register,
                          p^.location.register);
                        ungetregister32(p^.right^.location.register);
                     end;
                   ungetregister32(p^.location.register);
                end
              else
              { 64 bit types }
              if is_64bitint(p^.left^.resulttype) then
                begin
                   mboverflow:=false;
                   cmpop:=false;
                   unsigned:=((p^.left^.resulttype^.deftype=orddef) and
                       (porddef(p^.left^.resulttype)^.typ=u64bit)) or
                      ((p^.right^.resulttype^.deftype=orddef) and
                       (porddef(p^.right^.resulttype)^.typ=u64bit));
                   case p^.treetype of
                      addn : begin
                                begin
                                  op:=A_ADD;
                                  op2:=A_ADDX;
                                  mboverflow:=true;
                                end;
                             end;
                      subn : begin
                                op:=A_SUB;
                                op2:=A_SUBX;
                                mboverflow:=true;
                             end;
                      ltn,lten,
                      gtn,gten,
                      equaln,unequaln:
                             begin
                               op:=A_CMP;
                               op2:=A_CMP;
                               cmpop:=true;
                             end;

                      xorn:
                        begin
                           op:=A_EOR;
                           op2:=A_EOR;
                        end;

                      orn:
                        begin
                           op:=A_OR;
                           op2:=A_OR;
                        end;

                      andn:
                        begin
                           op:=A_AND;
                           op2:=A_AND;
                        end;
                      muln:
                        ;
                   else
                     CGMessage(type_e_mismatch);
                   end;

                   if p^.treetype=muln then
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

                        if cs_check_overflow in aktlocalswitches then
                          push_int(1)
                        else
                          push_int(0);
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
                          emitcall('FPC_MUL_QWORD')
                        else
                          emitcall('FPC_MUL_INT64');
                        p^.location.registerlow:=getregister32;
                        p^.location.registerhigh := getregister32;
                        p^.location.loc := LOC_REGISTER;
                        emit_reg_reg(A_MOVE,S_L,R_D1,p^.location.registerlow);
                        emit_reg_reg(A_MOVE,S_L,R_D0,p^.location.registerhigh);
                        restoreusedregisters(pushedreg);
                     end
                   else
                     begin
                        { left and right no register?  }
                        { then one must be demanded    }
                        if (p^.left^.location.loc<>LOC_REGISTER) and
                           (p^.right^.location.loc<>LOC_REGISTER) then
                          begin
                             { register variable ? }
                             if (p^.left^.location.loc=LOC_CREGISTER) then
                               begin
                                  { it is OK if this is the destination }
                                  if is_in_dest then
                                    begin
                                       hregister:=p^.location.registerlow;
                                       hregister2:=p^.location.registerhigh;
                                       emit_reg_reg(A_MOVE,S_L,p^.left^.location.registerlow,
                                         hregister);
                                       emit_reg_reg(A_MOVE,S_L,p^.left^.location.registerlow,
                                         hregister2);
                                    end
                                  else
                                  if cmpop then
                                    begin
                                       { do not disturb the register }
                                       hregister:=p^.location.registerlow;
                                       hregister2:=p^.location.registerhigh;
                                    end
                                  else
                                    begin
                                       hregister:=getregister32;
                                       hregister2:=getregister32;
                                       emit_reg_reg(A_MOVE,S_L,p^.left^.location.registerlow,
                                         hregister);
                                       emit_reg_reg(A_MOVE,S_L,p^.left^.location.registerhigh,
                                         hregister2);
                                    end
                               end
                             else
                               begin
                                  ungetiftemp(p^.left^.location.reference);
                                  del_reference(p^.left^.location.reference);
                                  if is_in_dest then
                                    begin
                                       hregister:=p^.location.registerlow;
                                       hregister2:=p^.location.registerhigh;
                                       emit_mov_ref_reg64(p^.left^.location.reference,hregister,hregister2);
                                    end
                                  else
                                    begin
                                       hregister:=getregister32;
                                       hregister2:=getregister32;
                                       emit_mov_ref_reg64(p^.left^.location.reference,hregister,hregister2);
                                    end;
                               end;
                             clear_location(p^.location);
                             p^.location.loc:=LOC_REGISTER;
                             p^.location.registerlow:=hregister;
                             p^.location.registerhigh:=hregister2;
                          end
                        else
                          { if on the right the register then swap }
                          if not(noswap) and (p^.right^.location.loc=LOC_REGISTER) then
                            begin
                               swap_location(p^.location,p^.right^.location);

                               { newly swapped also set swapped flag }
                               p^.swaped:=not(p^.swaped);
                            end;
                        { at this point, p^.location.loc should be LOC_REGISTER }
                        { and p^.location.register should be a valid register   }
                        { containing the left result                        }

                        if p^.right^.location.loc<>LOC_REGISTER then
                          begin
                             if (p^.treetype=subn) and p^.swaped then
                               begin
                                  if p^.right^.location.loc=LOC_CREGISTER then
                                    begin
                                       { 64-bit / 64-bit operation }
                                       { old code :                }
                                       { MOVE src_reghigh, D0      }
                                       { MIVE src_reglow,  D1      }
                                       { ADD  dest_reglow, D1      }
                                       { ADDX dest_reghigh, D0     }
                                       getexplicitregister32(R_D1);
                                       getexplicitregister32(R_D0);

                                       emit_reg_reg(A_MOVE,opsize,p^.right^.location.register,R_D1);
                                       emit_reg_reg(A_MOVE,opsize,p^.right^.location.registerhigh,R_D0);

                                       { opcodes regrouped to make sure that carry is still valid }
                                       emit_reg_reg(op,opsize,p^.location.register,R_D1);
                                       emit_reg_reg(op2,opsize,p^.location.registerhigh,R_D0);


                                       emit_reg_reg(A_MOVE,opsize,R_D1,p^.location.register);
                                       emit_reg_reg(A_MOVE,opsize,R_D0,p^.location.registerhigh);

                                       ungetregister32(R_D1);
                                       ungetregister32(R_D0);
                                    end
                                  else
                                    begin
                                       { 64-bit / 64-bit operation }
                                       getexplicitregister32(R_D1);
                                       getexplicitregister32(R_D0);

                                       hr:=newreference(p^.right^.location.reference);
                                       inc(hr^.offset,4);

                                       { load both values into registers }
                                       emit_ref_reg(A_MOVE,opsize,
                                         newreference(p^.right^.location.reference),R_D0);
                                       emit_ref_reg(A_MOVE,opsize,hr,R_D1);

                                       { carry is valid between these two calls }
                                       emit_reg_reg(op,opsize,p^.location.registerlow,R_D1);
                                       emit_reg_reg(op2,opsize,p^.location.registerhigh,R_D0);

                                       { store results. }
                                       emit_reg_reg(A_MOVE,opsize,R_D1,p^.location.registerlow);
                                       emit_reg_reg(A_MOVE,opsize,R_D0,p^.location.registerhigh);

                                       ungetiftemp(p^.right^.location.reference);
                                       del_reference(p^.right^.location.reference);

                                       ungetregister32(R_D1);
                                       ungetregister32(R_D0);

                                    end;
                               end
                             else if cmpop then
                               begin
                                  if (p^.right^.location.loc=LOC_CREGISTER) then
                                    begin
                                       emit_reg_reg(A_CMP,S_L,p^.right^.location.registerhigh,
                                          p^.location.registerhigh);
                                       firstjmp64bitcmp;
                                       emit_reg_reg(A_CMP,S_L,p^.right^.location.registerlow,
                                          p^.location.registerlow);
                                       secondjmp64bitcmp;
                                    end
                                  else
                                    begin
                                       hr:=newreference(p^.right^.location.reference);

                                       emit_ref_reg(A_CMP,S_L,hr,p^.location.registerhigh);
                                       firstjmp64bitcmp;

                                       hr:=newreference(p^.right^.location.reference);
                                       inc(hr^.offset,4);
                                       emit_ref_reg(A_CMP,S_L,hr,p^.location.registerlow);
                                       secondjmp64bitcmp;

                                       emitjmp(C_None,falselabel);

                                       ungetiftemp(p^.right^.location.reference);
                                       del_reference(p^.right^.location.reference);
                                    end;
                               end
                             else
                               begin
                                    begin
                                       if (p^.right^.location.loc=LOC_CREGISTER) then
                                         begin
                                            emit_reg_reg(op,S_L,p^.right^.location.registerlow,
                                               p^.location.registerlow);
                                            emit_reg_reg(op2,S_L,p^.right^.location.registerhigh,
                                               p^.location.registerhigh);
                                         end
                                       else
                                         begin
                                            getexplicitregister32(R_D0);
                                            getexplicitregister32(R_D1);
                                            emit_ref_reg(A_MOVE,S_L,newreference(
                                              p^.right^.location.reference),R_D0);
                                            hr:=newreference(p^.right^.location.reference);
                                            inc(hr^.offset,4);
                                            emit_ref_reg(A_MOVE,S_L,hr,R_D1);

                                            emit_reg_reg(op,S_L,R_D1,p^.location.registerlow);
                                            emit_reg_reg(op2, S_L,R_D0,p^.location.registerhigh);
                                            ungetiftemp(p^.right^.location.reference);
                                            del_reference(p^.right^.location.reference);
                                            ungetregister(R_D0);
                                            ungetregister(R_D1);
                                         end;
                                    end;
                               end;
                          end
                        else
                          begin
                             { when swapped another result register }
                             if (p^.treetype=subn) and p^.swaped then
                               begin
                                 emit_reg_reg(op,S_L,
                                    p^.location.registerlow,
                                    p^.right^.location.registerlow);
                                 emit_reg_reg(op2,S_L,
                                    p^.location.registerhigh,
                                    p^.right^.location.registerhigh);
                                  swap_location(p^.location,p^.right^.location);
                                  { newly swapped also set swapped flag }
                                  { just to maintain ordering           }
                                  p^.swaped:=not(p^.swaped);
                               end
                             else if cmpop then
                               begin
                                  emit_reg_reg(A_CMP,S_L,
                                    p^.right^.location.registerhigh,
                                    p^.location.registerhigh);
                                  firstjmp64bitcmp;
                                  emit_reg_reg(A_CMP,S_L,
                                    p^.right^.location.registerlow,
                                    p^.location.registerlow);
                                  secondjmp64bitcmp;
                               end
                             else
                               begin
                                  emit_reg_reg(op,S_L,
                                    p^.right^.location.registerlow,
                                    p^.location.registerlow);
                                  emit_reg_reg(op2,S_L,
                                    p^.right^.location.registerhigh,
                                    p^.location.registerhigh);
                               end;
                             ungetregister32(p^.right^.location.registerlow);
                             ungetregister32(p^.right^.location.registerhigh);
                          end;

                        if cmpop then
                          begin
                             ungetregister32(p^.location.registerlow);
                             ungetregister32(p^.location.registerhigh);
                          end;

                        { only in case of overflow operations }
                        { produce overflow code }
                        { we must put it here directly, because sign of operation }
                        { is in unsigned VAR!!                              }
                        if mboverflow then
                         begin
                           if cs_check_overflow in aktlocalswitches  then
                            begin
                              getlabel(hl4);
                              if unsigned then
                               emitlabeled(A_BCC,hl4)
                              else
                               emitlabeled(A_BVC,hl4);
                              emitcall('FPC_OVERFLOW');
                              emitlab(hl4);
                            end;
                         end;
                        { we have LOC_JUMP as result }
                        if cmpop then
                          begin
                             clear_location(p^.location);
                             p^.location.loc:=LOC_JUMP;
                             cmpop:=false;
                          end;
                     end;
                end
              else
                   { ---------------- FLOATING POINT -------------------- }
              { Floating point }
               if (p^.left^.resulttype^.deftype=floatdef) and
                  (pfloatdef(p^.left^.resulttype)^.typ<>f32bit) then
                 begin
                    { real constants to the right, but only if it
                      isn't on the FPU stack, i.e. 1.0 or 0.0! }
                    if (p^.left^.treetype=realconstn) and
                      (p^.left^.location.loc<>LOC_FPU) then
                      swaptree(p);
                    cmpop:=false;
                    case p^.treetype of
                       addn : op:=A_FADD;
                       muln : op:=A_FMUL;
                       subn : op:=A_FSUB;
                       slashn : op:=A_FDIV;
                       ltn,lten,gtn,gten,
                       equaln,unequaln : begin
                                            op:=A_FCMP;
                                            cmpop:=true;
                                         end;
                       else CGMessage(type_e_mismatch);
                    end;
                   { ---------------- LEFT = LOC_FPUREG -------------------- }
                       if ((p^.treetype =subn) or (p^.treetype = slashn)) and (p^.swaped) then
                          {  fpu_reg =  right(FP1) / fpu_reg }
                          {  fpu_reg = right(FP1) -  fpu_reg  }
                          begin
                             if (cs_fp_emulation in aktmoduleswitches) then
                              begin
                                  Begin
                                    { fpu_reg = right / D1 }
                                    { fpu_reg = right - D1 }
                                    if p^.left^.location.loc = LOC_FPU then
                                       emit_reg_reg(A_MOVE,S_L,p^.left^.location.fpuregister,R_D0)
                                    else
                                       emit_ref_reg(A_MOVE,S_L,newreference(p^.left^.location.reference),
                                         R_D0);


                                    { load value into D1 }
                                    if p^.right^.location.loc <> LOC_FPU then
                                      emit_ref_reg(A_MOVE,S_L,
                                          newreference(p^.right^.location.reference),R_D1)
                                    else
                                     emit_reg_reg(A_MOVE,S_L,p^.right^.location.fpuregister,R_D1);

                                    { probably a faster way to do this but... }
                                    case op of
                                      A_FADD: emitcall('FPC_ADD_SINGLE');
                                      A_FMUL: emitcall('FPC_MUL_SINGLE');
                                      A_FSUB: emitcall('FPC_SUB_SINGLE');
                                      A_FDIV: emitcall('FPC_DIV_SINGLE');
                                      A_FCMP: emitcall('FPC_CMP_SINGLE');
                                   end;
                                   if not cmpop then { only flags are affected with cmpop }
                                    begin
                                      if p^.left^.location.loc = LOC_FPU then
                                        begin
                                         p^.location.loc := LOC_FPU;
                                         p^.location.fpuregister := p^.left^.location.fpuregister;
                                         emit_reg_reg(A_MOVE,S_L,R_D0,p^.left^.location.fpuregister)
                                        end
                                      else
                                         begin
                                           p^.location.loc := LOC_FPU;
                                           p^.location.fpuregister := getregister32;
                                           emit_reg_reg(A_MOVE,S_L,R_D0,p^.location.fpuregister);

                                         end;
                                    end;

                                   { if this was a reference, then delete as it }
                                   { it no longer required.                     }
                                   if p^.right^.location.loc <> LOC_FPU then
                                     del_reference(p^.right^.location.reference);
                                  end;
                                end
                             else { no emulation }
                              begin

                                  if p^.right^.location.loc <> LOC_FPU then
                                    begin
                                      getexplicitregister32(R_FP1);
                                      emit_ref_reg(A_FMOVE,getfloatsize(pfloatdef(p^.left^.resulttype)^.typ),
                                        newreference(p^.right^.location.reference),
                                        R_FP1);
                                      fpureg:=R_FP1;
                                    end
                                  else
                                    { FPm --> FPn must use extended precision }
                                    begin
                                      fpureg:=p^.right^.location.fpuregister;
                                    end;


                                  if p^.left^.location.loc = LOC_FPU then
                                     begin
                                       { arithmetic expression performed in extended mode }
                                       emit_reg_reg(op,S_FX,p^.left^.location.fpuregister,fpureg);
                                     end
                                  else
                                     begin
                                        { always perform operations on EXTENDED values        }
                                        { therefore if operand is in memory, load it          }
                                        { first, which is autoomatically put to extended type }
                                        fpureg2:=getfloatreg;
                                        emit_ref_reg(A_FMOVE,getfloatsize(pfloatdef(p^.left^.resulttype)^.typ),
                                          newreference(p^.left^.location.reference),
                                           fpureg2);
                                        emit_reg_reg(op,S_FX,fpureg2,fpureg);
                                        ungetregister(fpureg2);
                                     end;


                                  { cmpop does not change any floating point register!! }
                                  if not cmpop then
                                    begin
                                       if p^.left^.location.loc = LOC_FPU then
                                         begin
                                           p^.location.loc:=LOC_FPU;
                                           if fpureg<>R_FP1 then
                                             begin
                                               p^.location.fpuregister:=fpureg;
                                               ungetregister(p^.left^.location.fpuregister);
                                             end
                                           else
                                             begin
                                               emit_reg_reg(A_FMOVE,S_FX,fpureg,p^.left^.location.fpuregister);
                                               p^.location.fpuregister:=p^.left^.location.fpuregister;
                                               ungetregister(fpureg);
                                             end;
                                           end
                                       else
                                         begin
                                           p^.location.loc:=LOC_FPU;
                                           if fpureg<>R_FP1 then
                                             p^.location.fpuregister:=fpureg
                                           else
                                             begin
                                               p^.location.fpuregister:=getfloatreg;
                                               emit_reg_reg(A_FMOVE,S_FX,fpureg,p^.location.fpuregister);
                                               ungetregister(fpureg);
                                             end;
                                         end;
                                    end
                                  else
                                  { process comparison, to make it compatible with the rest of the code }
                                      begin
                                        processcc(p);
                                        if p^.left^.location.loc = LOC_FPU then
                                          ungetregister(p^.left^.location.fpuregister);
                                        ungetregister(fpureg);
                                      end;


                                  { if this was a reference, then delete as it }
                                  { it no longer required.                     }
                                  if p^.right^.location.loc <> LOC_FPU then
                                     del_reference(p^.right^.location.reference);
                              end;
                          end
                       else { everything is in the right order }
                         begin
                           {  fpu_reg = fpu_reg / right }
                           {  fpu_reg = fpu_reg - right }
                           { + commutative ops }
                           if cs_fp_emulation in aktmoduleswitches then
                           begin
                               Begin
                                 { load value into accumulator }
                                 if p^.right^.location.loc <> LOC_FPU then
                                   emit_ref_reg(A_MOVE,S_L,
                                     newreference(p^.right^.location.reference),R_D0)
                                 else
                                   emit_reg_reg(A_MOVE,S_L,p^.right^.location.fpuregister,R_D0);

                                 if p^.left^.location.loc = LOC_FPU then
                                    emit_reg_reg(A_MOVE,S_L,p^.left^.location.fpuregister,R_D1)
                                 else
                                    emit_ref_reg(A_MOVE,S_L,
                                       newreference(p^.left^.location.reference),R_D1);
                                 { probably a faster way to do this but... }
                                 case op of
                                   A_FADD: emitcall('FPC_ADD_SINGLE');
                                   A_FMUL: emitcall('FPC_MUL_SINGLE');
                                   A_FSUB: emitcall('FPC_SUB_SINGLE');
                                   A_FDIV: emitcall('FPC_DIV_SINGLE');
                                   A_FCMP: emitcall('FPC_CMP_SINGLE');
                                 end;

                                 if not cmpop then { only flags are affected with cmpop }
                                  begin
                                   if p^.left^.location.loc <> LOC_FPU then
                                    begin
                                      fpureg2:=getregister32;
                                      p^.location.loc:=LOC_FPU;
                                      p^.location.fpuregister:=fpureg2;
                                      emit_reg_reg(A_MOVE,S_L,R_D0,p^.location.fpuregister)
                                    end
                                   else
                                    begin
                                      emit_reg_reg(A_MOVE,S_L,R_D0,p^.left^.location.fpuregister);
                                      p^.location.loc:=LOC_FPU;
                                      p^.location.fpuregister:=p^.left^.location.fpuregister;
                                    end;
                                  end;
                                 { if this was a reference, then delete as it }
                                 { it no longer required.                     }
                                 if p^.right^.location.loc <> LOC_FPU then
                                    del_reference(p^.right^.location.reference);

                               end;
                           end
                           else { no emulation }
                           begin
                             if p^.right^.location.loc <> LOC_FPU then
                               begin
                                 getexplicitregister32(R_FP1);
                                 emit_ref_reg(A_FMOVE,
                                   getfloatsize(pfloatdef(p^.left^.resulttype)^.typ),
                                   newreference(p^.right^.location.reference),R_FP1);
                                 fpureg:=R_FP1;
                               end
                             else
                               begin
                                 {emit_reg_reg(A_FMOVE,S_FX,
                                 p^.right^.location.fpuregister,R_FP1);}
                                 fpureg:=p^.right^.location.fpuregister;
                               end;

                               if p^.left^.location.loc = LOC_FPU then
                                 begin
                                   emit_reg_reg(op,S_FX,fpureg,p^.left^.location.fpuregister);
                                   fpureg2:=p^.left^.location.fpuregister;
                                 end
                               else
                                 begin
                                    { always load a reference from memory  }
                                    { so that it is always converted to an }
                                    { extended type first.                 }
                                    fpureg2:=getfloatreg;
                                    emit_ref_reg(A_FMOVE, getfloatsize(pfloatdef(p^.left^.resulttype)^.typ),
                                      newreference(p^.left^.location.reference),fpureg2);
                                    emit_reg_reg(op,S_FX,fpureg,fpureg2);
                                 end;

                               ungetregister(fpureg);
                                  { cmpop does not change any floating point register!! }
                                  if not cmpop then
                                    begin
                                      p^.location.loc:=LOC_FPU;
                                      p^.location.register:=fpureg2;
                                    end
                                  else
                                  { process comparison, to make it compatible with the rest of the code }
                                      begin
                                        processcc(p);
                                        ungetregister(fpureg2);
                                      end;

                             { if this was a reference, then delete as it }
                             { it no longer required.                     }
                             if p^.right^.location.loc <> LOC_FPU then
                               del_reference(p^.right^.location.reference);

                           end
                         end; { endif treetype = .. }


                         if cmpop then
                          begin
                             { the floating point registers are no longer needed. }
                             if cs_fp_emulation in aktmoduleswitches then
                               ungetregister(p^.left^.location.fpuregister);


                             if p^.swaped then
                               begin
                                 case p^.treetype of
                                     equaln: flags := F_E;
                                     unequaln: flags := F_NE;
                                     ltn : flags := F_G;
                                     lten : flags := F_GE;
                                     gtn : flags := F_L;
                                     gten: flags := F_LE;
                                 end;
                               end
                             else
                               begin
                                 case p^.treetype of
                                     equaln: flags := F_E;
                                     unequaln : flags := F_NE;
                                     ltn: flags := F_L;
                                     lten : flags := F_LE;
                                     gtn : flags := F_G;
                                     gten: flags := F_GE;
                                 end;
                               end;
                              clear_location(p^.location);
                              p^.location.loc:=LOC_FLAGS;
                              p^.location.resflags:=flags;
                              cmpop:=false;
                          end
                         else if cs_fp_emulation in aktmoduleswitches then
                         begin
(*                             clear_location(p^.location);
                             if p^.left^.location.loc = LOC_FPU then
                             begin
                               p^.location.loc := LOC_FPU;
                               p^.location.fpuregister := p^.left^.location.fpuregister
                             end
                             else
                             begin
                                p^.location.loc := p^.left^.location.loc;
                                p^.location.reference := p^.left^.location.reference;
                             end; *)
                         end;
                 end
              else CGMessage(type_e_mismatch);
           end;
       SetResultLocation(cmpop,unsigned,p);
    end;


end.


{
  $Log: cgadd.pas,v $
  Revision 1.1.2.38  2002/12/12 10:39:57  pierre
   * fix bug described in tbs/tb0431.pp

  Revision 1.1.2.37  2002/09/12 19:52:07  carl
    * (m68k): bugfixes for 64-bit arithmetic, 64-bit shifts and 64-bit division
    * (m68k): bugfix for boolean type conversions
    * (m68k): endian problems fixes when src_Size = dst_Size with a different type

  Revision 1.1.2.36  2002/09/10 19:12:48  carl
    * cg int64 bugfixes related to endian (from cg testsuit)

  Revision 1.1.2.35  2002/07/29 18:04:11  carl
    + fixed bug 2031

  Revision 1.1.2.34  2002/04/05 15:01:12  jonas
    * fixed web bug 1915

  Revision 1.1.2.33  2001/09/17 13:42:33  pierre
   * fixes to remove multiple register ungetregister

  Revision 1.1.2.32  2001/09/14 15:37:40  pierre
   * more int64 fixes

  Revision 1.1.2.31  2001/09/13 23:00:59  pierre
   * some more small changes for float pushing

  Revision 1.1.2.30  2001/09/13 13:57:21  pierre
   * fix error in last commit resulting in generation on wrong assembler

  Revision 1.1.2.29  2001/08/30 08:09:44  jonas
    * fixed bug in my previous commit

  Revision 1.1.2.28  2001/08/29 14:55:27  jonas
    * backported int64 related fixes from main branch (for m68k: I only
      tested whether the compiler can still be compiled, you should still
      compile/run tests/test/tint64*.pp to verify whether I didn't break
      anything

  Revision 1.1.2.27  2001/07/31 01:08:09  carl
  * correct problems with emulation code and result

  Revision 1.1.2.26  2001/07/29 20:35:28  pierre
   * improve fpu handling

  Revision 1.1.2.25  2001/07/26 15:19:14  pierre
   * handle swaped fpu comp correctly

  Revision 1.1.2.24  2001/07/26 00:40:54  pierre
   * attempt to fix the fpu to css register pasing

  Revision 1.1.2.23  2001/07/25 22:30:07  pierre
   * one more float fix

  Revision 1.1.2.22  2001/07/25 21:48:39  pierre
   * fix stupid assembler instruction swap

  Revision 1.1.2.21  2001/07/25 13:01:28  pierre
   * fix loading of set elements

  Revision 1.1.2.20  2001/07/25 07:16:59  pierre
   * fix fpu register release for comparison operations

  Revision 1.1.2.19  2001/07/24 15:39:12  pierre
   * optimize and fix fpu code partially

  Revision 1.1.2.18  2001/06/29 02:16:55  carl
  * correct problem with Jonas's commir.

  Revision 1.1.2.17  2001/06/25 14:14:57  jonas
    * fixed set bug discovered by Carl

  Revision 1.1.2.16  2001/06/25 02:24:17  carl
  * bugfix of ??

  Revision 1.1.2.15  2001/06/16 03:44:41  carl
  - useless define removed

  Revision 1.1.2.14  2001/06/14 03:37:47  carl
  * FPC_SET_COMP_SETS returns boolean while cg expects ZF set on equal

  Revision 1.1.2.13  2001/06/03 18:56:07  carl
  * fixed typo when using 64-bit arithmetic

  Revision 1.1.2.12  2001/05/24 03:33:00  carl
  * Corrected big problems when left node for FPU is not in LOC_FPU (load the thing into a register now!)

  Revision 1.1.2.11  2001/05/22 01:32:12  carl
  * bugfix of problem with FPU when left node is LOC_MEM
  - remove s64real , s80real support

  Revision 1.1.2.10  2001/05/21 16:47:16  carl
  * renamed according to FPC rules

  Revision 1.1.2.9  2001/05/18 18:01:30  carl
  * problem with cmp operators with FPU fixed (was not doing it correctly) :(

  Revision 1.1.2.8  2001/05/09 03:45:14  carl
  - removed 64-bit float support
  * bugfix of problem with 64-bit operations (operands were not initialized)

  Revision 1.1.2.7  2001/04/24 12:02:33  carl
  * correction of problems with not enough registers free with LOC_FPU
  * result of secondadd() with FPU is no longer always in LOC_FPU on exit

  Revision 1.1.2.6  2001/04/23 01:13:35  carl
  * corrected problem eith FPU and setting of flags
  * corrected problem with FPU allocation of registers overflow

  Revision 1.1.2.5  2001/04/22 23:38:58  carl
  * problem with s64real type definition

  Revision 1.1.2.4  2001/04/19 11:37:35  carl
  * m68k updates (code generator compiles)

  Revision 1.1.2.3  2001/04/03 03:02:52  carl
  + initial port

}

