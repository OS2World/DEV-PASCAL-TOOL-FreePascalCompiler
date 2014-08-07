{
    $Id: ra386.pas,v 1.6.2.6 2002/11/15 14:10:13 pierre Exp $
    Copyright (c) 1998-2000 by Carl Eric Codere and Peter Vreman

    Handles the common i386 assembler reader routines

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
Unit Ra386;
interface

uses
  aasm,cpubase,cpuasm,RAUtils;

{ Parser helpers }
function is_prefix(t:tasmop):boolean;
function is_override(t:tasmop):boolean;
Function CheckPrefix(prefixop,op:tasmop): Boolean;
Function CheckOverride(overrideop,op:tasmop): Boolean;
Procedure FWaitWarning;

type
  P386Operand=^T386Operand;
  T386Operand=object(TOperand)
    Procedure SetCorrectSize(opcode:tasmop);virtual;
    Function SetupResult : boolean;virtual;
  end;

  P386Instruction=^T386Instruction;
  T386Instruction=object(TInstruction)
    OpOrder : TOperandOrder;
    { Operand sizes }
    procedure AddReferenceSizes;
    procedure SetInstructionOpsize;
    procedure CheckOperandSizes;
    procedure CheckNonCommutativeOpcodes;
    procedure SwapOperands;
    { opcode adding }
    procedure ConcatInstruction(p : paasmoutput);virtual;
  end;


implementation

uses
{$ifdef NEWCG}
  cgbase,
{$else}
  hcodegen,
{$endif}
  globtype,symconst,symtable,systems,types,globals,verbose;

{$define ATTOP}
{$define INTELOP}

{$ifdef NORA386INT}
  {$ifdef NOAG386NSM}
    {$ifdef NOAG386INT}
      {$undef INTELOP}
    {$endif}
  {$endif}
{$endif}

{$ifdef NORA386ATT}
  {$ifdef NOAG386ATT}
    {$undef ATTOP}
  {$endif}
{$endif}
{*****************************************************************************
                              Parser Helpers
*****************************************************************************}

function is_prefix(t:tasmop):boolean;
var
  i : longint;
Begin
  is_prefix:=false;
  for i:=1 to AsmPrefixes do
   if t=AsmPrefix[i-1] then
    begin
      is_prefix:=true;
      exit;
    end;
end;


function is_override(t:tasmop):boolean;
var
  i : longint;
Begin
  is_override:=false;
  for i:=1 to AsmOverrides do
   if t=AsmOverride[i-1] then
    begin
      is_override:=true;
      exit;
    end;
end;


Function CheckPrefix(prefixop,op:tasmop): Boolean;
{ Checks if the prefix is valid with the following opcode }
{ return false if not, otherwise true                          }
Begin
  CheckPrefix := TRUE;
(*  Case prefix of
    A_REP,A_REPNE,A_REPE:
      Case opcode Of
        A_SCASB,A_SCASW,A_SCASD,
        A_INS,A_OUTS,A_MOVS,A_CMPS,A_LODS,A_STOS:;
        Else
          Begin
            CheckPrefix := FALSE;
            exit;
          end;
      end; { case }
    A_LOCK:
      Case opcode Of
        A_BT,A_BTS,A_BTR,A_BTC,A_XCHG,A_ADD,A_OR,A_ADC,A_SBB,A_AND,A_SUB,
        A_XOR,A_NOT,A_NEG,A_INC,A_DEC:;
        Else
          Begin
            CheckPrefix := FALSE;
            Exit;
          end;
      end; { case }
    A_NONE: exit; { no prefix here }
    else
      CheckPrefix := FALSE;
   end; { end case } *)
end;


Function CheckOverride(overrideop,op:tasmop): Boolean;
{ Check if the override is valid, and if so then }
{ update the instr variable accordingly.         }
Begin
  CheckOverride := true;
{     Case instr.getinstruction of
    A_MOVS,A_XLAT,A_CMPS:
      Begin
        CheckOverride := TRUE;
        Message(assem_e_segment_override_not_supported);
      end
  end }
end;


Procedure FWaitWarning;
begin
  if (target_info.target=target_i386_GO32V2) and (cs_fp_emulation in aktmoduleswitches) then
   Message(asmr_w_fwait_emu_prob);
end;

{*****************************************************************************
                              T386Operand
*****************************************************************************}

Procedure T386Operand.SetCorrectSize(opcode:tasmop);
begin
  if att_needsuffix[opcode]=attsufFPU then
    begin
     case size of
      S_L : size:=S_FS;
      S_IQ : size:=S_FL;
     end;
    end
  else if att_needsuffix[opcode]=attsufFPUint then
    begin
      case size of
      S_W : size:=S_IS;
      S_L : size:=S_IL;
      end;
    end;
end;

Function T386Operand.SetupResult:boolean;
var
  Res : boolean;
Begin
  Res:=TOperand.setupResult;
  { replace by ref by register if not place was
    reserved on stack }
  if res and (procinfo^.return_offset=0) then
   begin
     opr.typ:=OPR_REGISTER;
     if is_fpu(procinfo^.returntype.def) then
       begin
         opr.reg:=R_ST0;
         case pfloatdef(procinfo^.returntype.def)^.typ of
           s32real : size:=S_FS;
           s64real : size:=S_FL;
           s80real : size:=S_FX;
           s64comp : size:=S_IQ;
         else
           begin
             Message(asmr_e_cannot_use_RESULT_here);
             res:=false;
           end;
         end;
       end
     else if ret_in_acc(procinfo^.returntype.def,procinfo^.def^.proccalloptions) then
       case procinfo^.returntype.def^.size of
       1 : begin
             opr.reg:=R_AL;
             size:=S_B;
           end;
       2 : begin
             opr.reg:=R_AX;
             size:=S_W;
           end;
       3,4 : begin
               opr.reg:=R_EAX;
               size:=S_L;
             end;
       else
         begin
           Message(asmr_e_cannot_use_RESULT_here);
           res:=false;
         end;
       end;
     Message1(asmr_h_RESULT_is_reg,reg2str(opr.reg));
   end;
  SetupResult:=res;
end;



{*****************************************************************************
                              T386Instruction
*****************************************************************************}

procedure T386Instruction.SwapOperands;
begin
  Inherited SwapOperands;
  { mark the correct order }
  if OpOrder=op_intel then
    OpOrder:=op_att
  else
    OpOrder:=op_intel;
end;

procedure T386Instruction.AddReferenceSizes;
{ this will add the sizes for references like [esi] which do not
  have the size set yet, it will take only the size if the other
  operand is a register }
var
  operand2,i : longint;
  s : pasmsymbol;
  so : longint;
begin
  for i:=1to ops do
   begin
   operands[i]^.SetCorrectSize(opcode);
   if (operands[i]^.size=S_NO) then
    begin
      case operands[i]^.Opr.Typ of
        OPR_REFERENCE :
          begin
            if i=2 then
             operand2:=1
            else
             operand2:=2;
            if operand2<ops then
             begin
               { Only allow register as operand to take the size from }
               if operands[operand2]^.opr.typ=OPR_REGISTER then
                 begin
                   if ((opcode<>A_MOVD) and
                       (opcode<>A_CVTSI2SS)) then
                     operands[i]^.size:=operands[operand2]^.size;
                 end
               else
                begin
                  { if no register then take the opsize (which is available with ATT),
                    if not availble then give an error }
                  if opsize<>S_NO then
                    operands[i]^.size:=opsize
                  else
                   begin
                     Message(asmr_e_unable_to_determine_reference_size);
                     { recovery }
                     operands[i]^.size:=S_L;
                   end;
                end;
             end
            else
             begin
               if opsize<>S_NO then
                 operands[i]^.size:=opsize
             end;
          end;
        OPR_SYMBOL :
          begin
            { Fix lea which need a reference }
            if opcode=A_LEA then
             begin
               s:=operands[i]^.opr.symbol;
               so:=operands[i]^.opr.symofs;
               operands[i]^.opr.typ:=OPR_REFERENCE;
               reset_reference(operands[i]^.opr.ref);
               operands[i]^.opr.ref.symbol:=s;
               operands[i]^.opr.ref.offset:=so;
             end;
            operands[i]^.size:=S_L;
          end;
      end;
    end;
   end;
end;


procedure T386Instruction.SetInstructionOpsize;
begin
  if opsize<>S_NO then
   exit;
  if (OpOrder=op_intel) then
    SwapOperands;
  case ops of
    0 : ;
    1 :
      { "push es" must be stored as a long PM }
      if ((opcode=A_PUSH) or
          (opcode=A_POP)) and
         (operands[1]^.opr.typ=OPR_REGISTER) and
         ((operands[1]^.opr.reg>=firstsreg) and
          (operands[1]^.opr.reg<=lastsreg)) then
        opsize:=S_L
      else
        opsize:=operands[1]^.size;
    2 :
      begin
        case opcode of
          A_MOVZX,A_MOVSX :
            begin
              case operands[1]^.size of
                S_W :
                  case operands[2]^.size of
                    S_L :
                      opsize:=S_WL;
                  end;
                S_B :
                  case operands[2]^.size of
                    S_W :
                      opsize:=S_BW;
                    S_L :
                      opsize:=S_BL;
                  end;
              end;
            end;
          A_MOVD : { movd is a move from a mmx register to a
                     32 bit register or memory, so no opsize is correct here PM }
            exit;
          A_OUT :
            opsize:=operands[1]^.size;
          else
            opsize:=operands[2]^.size;
        end;
      end;
    3 :
      opsize:=operands[3]^.size;
  end;
end;


procedure T386Instruction.CheckOperandSizes;
var
  sizeerr : boolean;
  i : longint;
begin
  if (OpOrder=op_intel) then
    SwapOperands;
  { Check only the most common opcodes here, the others are done in
    the assembler pass }
  case opcode of
    A_PUSH,A_POP,A_DEC,A_INC,A_NOT,A_NEG,
    A_CMP,A_MOV,
    A_ADD,A_SUB,A_ADC,A_SBB,
    A_AND,A_OR,A_TEST,A_XOR: ;
  else
    exit;
  end;
  { Handle the BW,BL,WL separatly }
  sizeerr:=false;
  { special push/pop selector case }
  if ((opcode=A_PUSH) or
      (opcode=A_POP)) and
     (operands[1]^.opr.typ=OPR_REGISTER) and
     ((operands[1]^.opr.reg>=firstsreg) and
      (operands[1]^.opr.reg<=lastsreg)) then
     exit;
  if opsize in [S_BW,S_BL,S_WL] then
   begin
     if ops<>2 then
      sizeerr:=true
     else
      begin
        case opsize of
          S_BW :
            sizeerr:=(operands[1]^.size<>S_B) or (operands[2]^.size<>S_W);
          S_BL :
            sizeerr:=(operands[1]^.size<>S_B) or (operands[2]^.size<>S_L);
          S_WL :
            sizeerr:=(operands[1]^.size<>S_W) or (operands[2]^.size<>S_L);
        end;
      end;
   end
  else
   begin
     for i:=1 to ops do
      begin
        if (operands[i]^.opr.typ<>OPR_CONSTANT) and
           (operands[i]^.size in [S_B,S_W,S_L]) and
           (operands[i]^.size<>opsize) then
         sizeerr:=true;
      end;
   end;
  if sizeerr then
   begin
     { if range checks are on then generate an error }
     if (cs_compilesystem in aktmoduleswitches) or
        not (cs_check_range in aktlocalswitches) then
       Message(asmr_w_size_suffix_and_dest_dont_match)
     else
       Message(asmr_e_size_suffix_and_dest_dont_match);
   end;
end;


{ This check must be done with the operand in ATT order
  i.e.after swapping in the intel reader
  but before swapping in the NASM and TASM writers PM }
procedure T386Instruction.CheckNonCommutativeOpcodes;
begin
  if (OpOrder=op_intel) then
    SwapOperands;
  if ((ops=2) and
     (operands[1]^.opr.typ=OPR_REGISTER) and
     (operands[2]^.opr.typ=OPR_REGISTER) and
     { if the first is ST and the second is also a register
       it is necessarily ST1 .. ST7 }
     (operands[1]^.opr.reg in [R_ST..R_ST0])) or
      (ops=0)  then
      if opcode=A_FSUBR then
        opcode:=A_FSUB
      else if opcode=A_FSUB then
        opcode:=A_FSUBR
      else if opcode=A_FDIVR then
        opcode:=A_FDIV
      else if opcode=A_FDIV then
        opcode:=A_FDIVR
      else if opcode=A_FSUBRP then
        opcode:=A_FSUBP
      else if opcode=A_FSUBP then
        opcode:=A_FSUBRP
      else if opcode=A_FDIVRP then
        opcode:=A_FDIVP
      else if opcode=A_FDIVP then
        opcode:=A_FDIVRP;
  if  ((ops=1) and
      (operands[1]^.opr.typ=OPR_REGISTER) and
      (operands[1]^.opr.reg in [R_ST1..R_ST7])) then
      if opcode=A_FSUBRP then
        opcode:=A_FSUBP
      else if opcode=A_FSUBP then
        opcode:=A_FSUBRP
      else if opcode=A_FDIVRP then
        opcode:=A_FDIVP
      else if opcode=A_FDIVP then
        opcode:=A_FDIVRP;
end;

{*****************************************************************************
                              opcode Adding
*****************************************************************************}

procedure T386Instruction.ConcatInstruction(p : paasmoutput);
var
  siz  : topsize;
  i,asize : longint;
  ai   : paicpu;
begin
  if (OpOrder=op_intel) then
    SwapOperands;

{ Get Opsize }
  if (opsize<>S_NO) or (Ops=0) then
   siz:=opsize
  else
   begin
     if (Ops=2) and (operands[1]^.opr.typ=OPR_REGISTER) then
      siz:=operands[1]^.size
     else
      siz:=operands[Ops]^.size;
     { MOVD should be of size S_LQ or S_QL, but these do not exist PM }
     if (ops=2) and (operands[1]^.size<>S_NO) and
        (operands[2]^.size<>S_NO) and (operands[1]^.size<>operands[2]^.size) then
       siz:=S_NO;
   end;

   if ((opcode=A_MOVD)or
       (opcode=A_CVTSI2SS)) and
      ((operands[1]^.size=S_NO) or
       (operands[2]^.size=S_NO)) then
     siz:=S_NO;
   { NASM does not support FADD without args
     as alias of FADDP
     and GNU AS interprets FADD without operand differently
     for version 2.9.1 and 2.9.5 !! }
   if (ops=0) and
      ((opcode=A_FADD) or
       (opcode=A_FMUL) or
       (opcode=A_FSUB) or
       (opcode=A_FSUBR) or
       (opcode=A_FDIV) or
       (opcode=A_FDIVR)) then
     begin
       if opcode=A_FADD then
         opcode:=A_FADDP
       else if opcode=A_FMUL then
         opcode:=A_FMULP
       else if opcode=A_FSUB then
         opcode:=A_FSUBP
       else if opcode=A_FSUBR then
         opcode:=A_FSUBRP
       else if opcode=A_FDIV then
         opcode:=A_FDIVP
       else if opcode=A_FDIVR then
         opcode:=A_FDIVRP;
{$ifdef ATTOP}
       message1(asmr_w_fadd_to_faddp,att_op2str[opcode]);
{$else}
  {$ifdef INTELOP}
       message1(asmr_w_fadd_to_faddp,int_op2str[opcode]);
  {$else}
       message1(asmr_w_fadd_to_faddp,'fXX');
  {$endif INTELOP}
{$endif ATTOP}
     end;

   { GNU AS interprets FDIV without operand differently
     for version 2.9.1 and 2.10
     we add explicit args to it !! }
  if (ops=0) and
     ((opcode=A_FSUBP) or
      (opcode=A_FSUBRP) or
      (opcode=A_FDIVP) or
      (opcode=A_FDIVRP) or
      (opcode=A_FSUB) or
      (opcode=A_FSUBR) or
      (opcode=A_FDIV) or
      (opcode=A_FDIVR)) then
     begin
{$ifdef ATTOP}
       message1(asmr_w_adding_explicit_args_fXX,att_op2str[opcode]);
{$else}
  {$ifdef INTELOP}
       message1(asmr_w_adding_explicit_args_fXX,int_op2str[opcode]);
  {$else}
       message1(asmr_w_adding_explicit_args_fXX,'fXX');
  {$endif INTELOP}
{$endif ATTOP}
       ops:=2;
       operands[1]^.opr.typ:=OPR_REGISTER;
       operands[2]^.opr.typ:=OPR_REGISTER;
       operands[1]^.opr.reg:=R_ST;
       operands[2]^.opr.reg:=R_ST1;
     end;
  if (ops=1) and
      ((operands[1]^.opr.typ=OPR_REGISTER) and
      (operands[1]^.opr.reg in [R_ST1..R_ST7])) and
      ((opcode=A_FSUBP) or
      (opcode=A_FSUBRP) or
      (opcode=A_FDIVP) or
      (opcode=A_FDIVRP) or
      (opcode=A_FADDP) or
      (opcode=A_FMULP)) then
     begin
{$ifdef ATTOP}
       message1(asmr_w_adding_explicit_first_arg_fXX,att_op2str[opcode]);
{$else}
  {$ifdef INTELOP}
       message1(asmr_w_adding_explicit_first_arg_fXX,int_op2str[opcode]);
  {$else}
       message1(asmr_w_adding_explicit_first_arg_fXX,'fXX');
  {$endif INTELOP}
{$endif ATTOP}
       ops:=2;
       operands[2]^.opr.typ:=OPR_REGISTER;
       operands[2]^.opr.reg:=operands[1]^.opr.reg;
       operands[1]^.opr.reg:=R_ST;
     end;

  if (ops=1) and
      ((operands[1]^.opr.typ=OPR_REGISTER) and
      (operands[1]^.opr.reg in [R_ST1..R_ST7])) and
      ((opcode=A_FSUB) or
      (opcode=A_FSUBR) or
      (opcode=A_FDIV) or
      (opcode=A_FDIVR) or
      (opcode=A_FADD) or
      (opcode=A_FMUL)) then
     begin
{$ifdef ATTOP}
       message1(asmr_w_adding_explicit_second_arg_fXX,att_op2str[opcode]);
{$else}
  {$ifdef INTELOP}
       message1(asmr_w_adding_explicit_second_arg_fXX,int_op2str[opcode]);
  {$else}
       message1(asmr_w_adding_explicit_second_arg_fXX,'fXX');
  {$endif INTELOP}
{$endif ATTOP}
       ops:=2;
       operands[2]^.opr.typ:=OPR_REGISTER;
       operands[2]^.opr.reg:=R_ST;
     end;

   { I tried to convince Linus Torwald to add
     code to support ENTER instruction
     (when raising a stack page fault)
     but he replied that ENTER is a bad instruction and
     Linux does not need to support it
     So I think its at least a good idea to add a warning
     if someone uses this in assembler code
     FPC itself does not use it at all PM }
   if (opcode=A_ENTER) and ((target_info.target=target_i386_linux) or
        (target_info.target=target_i386_FreeBSD)) then
     begin
       message(asmr_w_enter_not_supported_by_linux);
     end;

  ai:=new(paicpu,op_none(opcode,siz));
  ai^.SetOperandOrder(OpOrder);
  ai^.Ops:=Ops;
  for i:=1to Ops do
   begin
     case operands[i]^.opr.typ of
       OPR_CONSTANT :
         ai^.loadconst(i-1,operands[i]^.opr.val);
       OPR_REGISTER:
         ai^.loadreg(i-1,operands[i]^.opr.reg);
       OPR_SYMBOL:
         ai^.loadsymbol(i-1,operands[i]^.opr.symbol,operands[i]^.opr.symofs);
       OPR_REFERENCE:
         begin
           ai^.loadref(i-1,newreference(operands[i]^.opr.ref));
           if operands[i]^.size<>S_NO then
             begin
               asize:=0;
               case operands[i]^.size of
                   S_B :
                     asize:=OT_BITS8;
                   S_W, S_IS :
                     asize:=OT_BITS16;
                   S_L, S_IL, S_FS:
                     asize:=OT_BITS32;
                   S_Q, S_D, S_FL, S_FV :
                     asize:=OT_BITS64;
                   S_FX :
                     asize:=OT_BITS80;
                 end;
               if asize<>0 then
                 ai^.oper[i-1].ot:=(ai^.oper[i-1].ot and not OT_SIZE_MASK) or asize;
             end;
         end;
     end;
   end;

  if (opcode=A_CALL) and (opsize=S_FAR) then
    opcode:=A_LCALL;
  if (opcode=A_JMP) and (opsize=S_FAR) then
    opcode:=A_LJMP;
  if (opcode=A_LCALL) or (opcode=A_LJMP) then
    opsize:=S_FAR;
 { Condition ? }
  if condition<>C_None then
   ai^.SetCondition(condition);

 { Concat the opcode or give an error }
  if assigned(ai) then
   begin
     { Check the instruction if it's valid }
{$ifndef NOAG386BIN}
     ai^.CheckIfValid;
{$endif}
     p^.concat(ai);
   end
  else
   Message(asmr_e_invalid_opcode_and_operand);
end;

end.
{
  $Log: ra386.pas,v $
  Revision 1.6.2.6  2002/11/15 14:10:13  pierre
    + added a calloptions parameter to ret_inacc and ret_in_param
      functions.
      This allows to handle win32 stdcall and m68k cdecl small records
      case.

  Revision 1.6.2.5  2002/10/31 13:18:44  pierre
   * ensure all operand operations are done with correct operand order
   * hopefully complete fix for fsub problem...

  Revision 1.6.2.4  2002/10/30 16:56:18  pierre
   * try to fix bug tw2158

  Revision 1.6.2.3  2001/08/07 15:55:33  pierre
   + new code for NetBSD, behaves like FreeBSD for now

  Revision 1.6.2.2  2001/03/30 21:20:17  pierre
   * fix the movd problem

  Revision 1.6.2.1  2001/03/04 02:22:20  carl
  - renamefest!

  Revision 1.1.2.8  2001/02/24 23:02:53  carl
  make it compile with NOAG386BIN define

  Revision 1.1.2.7  2001/02/20 16:51:31  pierre
   * fix several FPU problems

  Revision 1.1.2.6  2001/01/12 19:18:03  peter
    * check for valid asm instructions

  Revision 1.1.2.5  2000/10/31 07:30:11  pierre
   * fix for bug 1157 second part

  Revision 1.1.2.4  2000/10/07 23:41:06  pierre
   * fix for bug 1157

  Revision 1.1.2.3  2000/09/24 21:40:19  peter
    * error messages updated
    * if messages not available in message file fallback to the internal
      messages
    * message prefixes (like Note:) can now also be set in the msg file

  Revision 1.1.2.2  2000/09/13 13:57:41  marco
   * FreeBSD compiler support

  Revision 1.1.2.1  2000/09/03 11:35:44  peter
    * error for not specified operand size, which is now required for
      newer binutils

  Revision 1.1  2000/07/13 06:29:55  michael
  + Initial import

  Revision 1.19  2000/05/17 11:08:27  pierre
   + add a warning if using ENTER instruction with linux target

  Revision 1.18  2000/05/15 14:08:49  pierre
   * FADD without operand translated into FADDP

  Revision 1.17  2000/05/12 21:26:22  pierre
    * fix the FDIV FDIVR FSUB FSUBR and popping equivalent
      simply by swapping from reverse to normal and vice-versa
      when passing from one syntax to the other !

  Revision 1.16  2000/05/10 08:55:08  pierre
   * no warning nor error for pushl of segment register

  Revision 1.15  2000/05/09 21:44:28  pierre
    * add .byte 066h to force correct pushw %es
    * handle push es as a pushl %es

  Revision 1.14  2000/04/14 12:26:33  pierre
   avoid to reset operand size of opsize is S_NO

  Revision 1.13  2000/04/04 13:48:44  pierre
    + TOperand.SetCorrectSize virtual method added
      to be able to change the suffix according to the instruction
      (FIADD word ptr w need a s as ATT suffix
      wheras FILD word ptr w need a w suffix :( )

  Revision 1.12  2000/02/09 13:23:01  peter
    * log truncated

  Revision 1.11  2000/01/07 01:14:34  peter
    * updated copyright to 2000

  Revision 1.10  1999/12/12 12:59:34  peter
    * only check suffixsize for byte,word,long

  Revision 1.9  1999/08/25 12:00:05  jonas
    * changed pai386, paippc and paiapha (same for tai*) to paicpu (taicpu)

  Revision 1.8  1999/08/04 00:23:23  florian
    * renamed i386asm and i386base to cpuasm and cpubase

}
