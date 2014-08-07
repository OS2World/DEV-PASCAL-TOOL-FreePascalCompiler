{
    $Id: csopt.pas,v 1.1.2.2 2002/04/15 21:04:41 jonas Exp $
    Copyright (c) 1998-2000 by Jonas Maebe, member of the Free Pascal
      development team

    This unit contains the common subexpression elimination procedure.

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
Unit CSOpt;

Interface

Uses aasm;

{Procedure CSOpt386(First, Last: Pai);}
Procedure CSE(AsmL: PAasmOutput; First, Last: Pai);

Implementation

Uses
  CObjects, verbose, hcodegen, globals,cpubase,cpuasm,DAOpt, tgen;

{
Function PaiInSequence(P: Pai; Const Seq: TContent): Boolean;
Var P1: Pai;
    Counter: Byte;
    TmpResult: Boolean;
Begin
  TmpResult := False;
  P1 := Seq.StartMod;
  Counter := 1;
  While Not(TmpResult) And
        (Counter <= Seq.NrOfMods) Do
    Begin
      If (P = P1) Then TmpResult := True;
      Inc(Counter);
      p1 := Pai(p1^.Next);
    End;
  PaiInSequence := TmpResult;
End;
}

Function CheckSequence(p: Pai; Reg: TRegister; Var Found: Longint; Var RegInfo: TRegInfo): Boolean;
{checks whether the current instruction sequence (starting with p) and the
 one between StartMod and EndMod of Reg are the same. If so, the number of
 instructions that match is stored in Found and true is returned, otherwise
 Found holds the number of instructions between StartMod and EndMod and false
 is returned}
Var hp2, hp3{, EndMod}: Pai;
    PrevNonRemovablePai: Pai;
    {Cnt,} OldNrOfMods: Longint;
    OrgRegInfo, HighRegInfo: TRegInfo;
    HighFound, OrgRegFound: Byte;
    RegCounter, regCounter2: TRegister;
    OrgRegResult: Boolean;
    TmpResult: Boolean;
    {TmpState: Byte;}
Begin {CheckSequence}
  Reg := Reg32(Reg);
  TmpResult := False;
  FillChar(OrgRegInfo, SizeOf(OrgRegInfo), 0);
  OrgRegFound := 0;
  HighFound := 0;
  OrgRegResult := False;
  RegCounter := R_EAX;
  GetLastInstruction(p, PrevNonRemovablePai);
  While (RegCounter <= R_EDI) And
        (PPaiProp(PrevNonRemovablePai^.OptInfo)^.Regs[RegCounter].Typ <> Con_Ref) Do
    Inc(RegCounter);
  While (RegCounter <= R_EDI) Do
    Begin
      FillChar(RegInfo, SizeOf(RegInfo), 0);
      RegInfo.NewRegsEncountered := [procinfo^.FramePointer, R_ESP];
      RegInfo.OldRegsEncountered := RegInfo.NewRegsEncountered;
      RegInfo.New2OldReg[procinfo^.FramePointer] := procinfo^.FramePointer;
      RegInfo.New2OldReg[R_ESP] := R_ESP;
      Found := 0;
      hp2 := PPaiProp(PrevNonRemovablePai^.OptInfo)^.Regs[RegCounter].StartMod;
      If (PrevNonRemovablePai <> PPaiProp(PrevNonRemovablePai^.OptInfo)^.Regs[RegCounter].StartMod)
        Then OldNrOfMods := PPaiProp(PrevNonRemovablePai^.OptInfo)^.Regs[RegCounter].NrOfMods
        Else OldNrOfMods := 1;
      hp3 := p;
      While (Found <> OldNrOfMods) And
                                  { old  new }
             InstructionsEquivalent(hp2, hp3, RegInfo) Do
        Begin
          if (hp3^.typ = ait_instruction) and
             ((paicpu(hp3)^.opcode = A_MOV) or
              (paicpu(hp3)^.opcode = A_MOVZX) or
              (paicpu(hp3)^.opcode = A_MOVSX)) and
             (paicpu(hp3)^.oper[0].typ in
               [top_const,top_ref,top_symbol]) and
             (paicpu(hp3)^.oper[1].typ = top_reg) then
            regInfo.lastReload
              [reg32(paicpu(hp3)^.oper[1].reg)] := hp3;
          GetNextInstruction(hp2, hp2);
          GetNextInstruction(hp3, hp3);
          Inc(Found)
        End;
      for regCounter2 := R_EAX to R_EDX do
        if (regInfo.new2OldReg[regCounter2] <> R_NO) and
           (reg in PPaiProp(hp3^.optInfo)^.usedRegs) and
           not regLoadedWithNewValue(reg,false,hp3) then
          include(regInfo.regsStillUsedAfterSeq,regCounter);
      If (Found <> OldNrOfMods) or
 { the following is to avoid problems with rangecheck code (see testcse2) }
         (assigned(hp3) and
          ((reg in regInfo.regsLoadedForRef) and
           (reg in PPaiProp(hp3^.optInfo)^.usedRegs) and
           not regLoadedWithNewValue(reg,false,hp3))) then
        Begin
          TmpResult := False;
          If (found > 0) then
{this is correct because we only need to turn off the CanBeRemoved flag
 when an instruction has already been processed by CheckSequence
 (otherwise CanBeRemoved can't be true and thus can't have to be turned off).
 If it has already been processed by CheckSequence and flagged to be
 removed, it means that it has been checked against a previous sequence
 and that it was equal (otherwise CheckSequence would have returned false
 and the instruction wouldn't have been removed). If this "If found > 0"
 check is left out, incorrect optimizations are performed.}
            Found := PPaiProp(Pai(p)^.OptInfo)^.Regs[Reg].NrOfMods
        End
      Else TmpResult := True;
      If TmpResult And
         (Found > HighFound)
        Then
          Begin
            HighFound := Found;
            HighRegInfo := RegInfo;
          End;
      If (RegCounter = Reg) Then
        Begin
          OrgRegFound := Found;
          OrgRegResult := TmpResult;
          OrgRegInfo := RegInfo
        End;
      Repeat
        Inc(RegCounter);
      Until (RegCounter > R_EDI) or
            ((PPaiProp(PrevNonRemovablePai^.OptInfo)^.Regs[RegCounter].Typ = Con_Ref) {And
             ((Regcounter = Reg) Or
              Not(PaiInSequence(p, PPaiProp(PrevNonRemovablePai^.OptInfo)^.Regs[RegCounter]))) }
            );
    End;
  If (HighFound > 0) And
     (Not(OrgRegResult) Or
      (HighFound > OrgRegFound))
    Then
      Begin
{$ifndef fpc}
        TmpResult := True;
{$else fpc}
        CheckSequence := True;
{$endif fpc}
        RegInfo := HighRegInfo;
        Found := HighFound
      End
    Else
      Begin
{$ifndef fpc}
        TmpResult := OrgRegResult;
{$else fpc}
        CheckSequence := OrgRegResult;
{$endif fpc}
        Found := OrgRegFound;
        RegInfo := OrgRegInfo;
      End;
{$ifndef fpc}
  CheckSequence := TmpResult;
{$endif fpc}
End; {CheckSequence}

Procedure SetAlignReg(p: Pai);
Const alignSearch = 12;
var regsUsable: TRegSet;
    prevInstrCount, nextInstrCount: Longint;
    prevState, nextWState,nextRState: Array[R_EAX..R_EDI] of byte;
    regCounter, lastRemoved: TRegister;
    prev, next: Pai;
{$ifdef alignregdebug}
    temp: Pai;
{$endif alignregdebug}
begin
  regsUsable := [R_EAX,R_ECX,R_EDX,R_EBX,{R_ESP,R_EBP,}R_ESI,R_EDI];
  for regCounter := R_EAX to R_EDI do
    begin
      prevState[regCounter] := PPaiProp(p^.optInfo)^.Regs[regCounter].wState;
      nextWState[regCounter] := PPaiProp(p^.optInfo)^.Regs[regCounter].wState;
      nextRState[regCounter] := PPaiProp(p^.optInfo)^.Regs[regCounter].rState;
    end;
  getLastInstruction(p,prev);
  getNextInstruction(p,next);
  lastRemoved := pai_align(p)^.reg;
  nextInstrCount := 0;
  prevInstrCount := 0;
  while ((assigned(prev) and
          assigned(prev^.optInfo) and
          (prevInstrCount < alignSearch)) or
         (assigned(next) and
          assigned(next^.optInfo) and
          (nextInstrCount < alignSearch))) And
        (regsUsable <> []) do
    begin
{$ifdef alignregdebug}
      if assigned(prev) then
        begin
          temp := new(pai_asm_comment,init(strpnew('got here')));
          temp^.next := prev^.next;
          temp^.previous := prev;
          prev^.next := temp;
          if assigned(temp^.next) then
            temp^.next^.previous := temp;
        end;
{$endif alignregdebug}
      if assigned(prev) and assigned(prev^.optinfo) and
         (prevInstrCount < alignSearch) then
        begin
          if (prev^.typ = ait_instruction) And
             (insProp[PaiCpu(prev)^.opcode].ch[1] <> Ch_ALL) and
             (PaiCpu(prev)^.opcode <> A_JMP) then
            begin
              inc(prevInstrCount);
              for regCounter := R_EAX to R_EDI do
                begin
                  if (regCounter in regsUsable) And
                     (PPaiProp(prev^.optInfo)^.Regs[regCounter].wState <>
                       prevState[regCounter]) then
                    begin
                      lastRemoved := regCounter;
                      exclude(regsUsable,regCounter);
{$ifdef alignregdebug}
                      temp := new(pai_asm_comment,init(strpnew(
                                att_reg2str[regCounter]+' removed')));
                      temp^.next := prev^.next;
                      temp^.previous := prev;
                      prev^.next := temp;
                      if assigned(temp^.next) then
                        temp^.next^.previous := temp;
                      if regsUsable = [] then
                        begin
                          temp := new(pai_asm_comment,init(strpnew(
                                    'regsUsable empty here')));
                          temp^.next := prev^.next;
                          temp^.previous := prev;
                          prev^.next := temp;
                          if assigned(temp^.next) then
                            temp^.next^.previous := temp;
                        end;
{$endif alignregdebug}
                    end;
                  prevState[regCounter] :=
                    PPaiProp(prev^.optInfo)^.Regs[regCounter].wState;
                end;
              getLastInstruction(prev,prev);
            end
          else
            If GetLastInstruction(prev,prev) and
               assigned(prev^.optinfo) then
              for regCounter := R_EAX to R_EDI do
                prevState[regCounter] :=
                  PPaiProp(prev^.optInfo)^.Regs[regCounter].wState
        end;
      if assigned(next) and assigned(next^.optInfo) and
         (nextInstrCount < alignSearch) then
        begin
          if (next^.typ = ait_instruction) and
             (insProp[PaiCpu(next)^.opcode].ch[1] <> Ch_ALL) and
             (PaiCpu(next)^.opcode <> A_JMP) then
            begin
              inc(nextInstrCount);
              for regCounter := R_EAX to R_EDI do
                begin
                  if (regCounter in regsUsable) And
                     ((PPaiProp(next^.optInfo)^.Regs[regCounter].wState <>
                       nextWState[regCounter]) or
                      (PPaiProp(next^.optInfo)^.Regs[regCounter].rState <>
                       nextRState[regCounter])) Then
                    begin
                      lastRemoved := regCounter;
                      exclude(regsUsable,regCounter);
{$ifdef alignregdebug}
                      temp := new(pai_asm_comment,init(strpnew(
                                att_reg2str[regCounter]+' removed')));
                      temp^.next := next^.next;
                      temp^.previous := next;
                      next^.next := temp;
                      if assigned(temp^.next) then
                        temp^.next^.previous := temp;
                      if regsUsable = [] then
                        begin
                          temp := new(pai_asm_comment,init(strpnew(
                                    'regsUsable empty here')));
                          temp^.next := next^.next;
                          temp^.previous := next;
                          next^.next := temp;
                          if assigned(temp^.next) then
                            temp^.next^.previous := temp;
                        end;
{$endif alignregdebug}
                    end;
                  nextWState[regCounter] :=
                    PPaiProp(next^.optInfo)^.Regs[regCounter].wState;
                  nextRState[regCounter] :=
                    PPaiProp(next^.optInfo)^.Regs[regCounter].rState;
                end
            end
          else
            for regCounter := R_EAX to R_EDI do
              begin
                nextWState[regCounter] :=
                  PPaiProp(next^.optInfo)^.Regs[regCounter].wState;
                nextRState[regCounter] :=
                  PPaiProp(next^.optInfo)^.Regs[regCounter].rState;
              end;
          getNextInstruction(next,next);
        end;
    end;
  if regsUsable <> [] then
    for regCounter := R_EAX to R_EDI do
      if regCounter in regsUsable then
        begin
          lastRemoved := regCounter;
          break
        end;
{$ifdef alignregdebug}
  next := new(pai_asm_comment,init(strpnew(att_reg2str[lastRemoved]+
               ' chosen as alignment register')));
  next^.next := p^.next;
  next^.previous := p;
  p^.next := next;
  if assigned(next^.next) then
    next^.next^.previous := next;
{$endif alignregdebug}
  pai_align(p)^.reg := lastRemoved;
End;

Procedure RestoreRegContentsTo(reg: TRegister; const c: TContent; p, endP: pai);
var
{$ifdef replaceregdebug}
    hp: pai;
{$endif replaceregdebug}
    tmpState: byte;
begin
{$ifdef replaceregdebug}
  hp := new(pai_asm_comment,init(strpnew(
          'restored '+att_reg2str[reg]+' with data from here...')));
  hp^.next := p;
  hp^.previous := p^.previous;
  p^.previous := hp;
  if assigned(hp^.previous) then
    hp^.previous^.next := hp;
{$endif replaceregdebug}
{  PPaiProp(p^.optInfo)^.Regs[reg] := c;}
  While (p <> endP) Do
    Begin
      PPaiProp(p^.optInfo)^.Regs[reg] := c;
      getNextInstruction(p,p);
    end;
  tmpState := PPaiProp(p^.optInfo)^.Regs[reg].wState;
  repeat
    PPaiProp(p^.optInfo)^.Regs[reg] := c;
  until not getNextInstruction(p,p) or
        (PPaiProp(p^.optInfo)^.Regs[reg].wState <> tmpState);
{$ifdef replaceregdebug}
  if assigned(p) then
    begin
      hp := new(pai_asm_comment,init(strpnew(
        'restored '+att_reg2str[reg]+' till here...')));
      hp^.next := p;
      hp^.previous := p^.previous;
      p^.previous := hp;
      if assigned(hp^.previous) then
        hp^.previous^.next := hp;
    end;
{$endif replaceregdebug}
end;

Procedure DoCSE(AsmL: PAasmOutput; First, Last: Pai);
{marks the instructions that can be removed by RemoveInstructs. They're not
 removed immediately because sometimes an instruction needs to be checked in
 two different sequences}
var cnt, cnt2, cnt3: longint;
    p, hp1, hp2: Pai;
    hp3, hp4: pai;
    hp5 : pai;
    RegInfo: TRegInfo;
    RegCounter: TRegister;
Begin
  p := First;
  SkipHead(p);
  First := p;
  While (p <> Last) Do
    Begin
      Case p^.typ Of
        ait_align:
          if not(pai_align(p)^.use_op) then
            SetAlignReg(p);
        ait_instruction:
          Begin
            Case Paicpu(p)^.opcode Of
              A_CLD: If GetLastInstruction(p, hp1) And
                        (PPaiProp(hp1^.OptInfo)^.DirFlag = F_NotSet) Then
                       PPaiProp(Pai(p)^.OptInfo)^.CanBeRemoved := True;
              A_MOV, A_MOVZX, A_MOVSX:
                Begin
                  Case Paicpu(p)^.oper[0].typ Of
                    Top_Ref:
                      Begin {destination is always a register in this case}
                        With PPaiProp(p^.OptInfo)^.Regs[Reg32(Paicpu(p)^.oper[1].reg)] Do
                          Begin
                            If (p = StartMod) And
                               GetLastInstruction (p, hp1) And
                               (hp1^.typ <> ait_marker) Then
{so we don't try to check a sequence when p is the first instruction of the block}
                              begin
{$ifdef csdebug}
                               hp5 := new(pai_asm_comment,init(strpnew(
                                 'cse checking '+att_reg2str[Reg32(Paicpu(p)^.oper[1].reg)])));
                               insertLLItem(asml,p,p^.next,hp5);
{$endif csdebug}
                               If CheckSequence(p, Paicpu(p)^.oper[1].reg, Cnt, RegInfo) And
                                  (Cnt > 0) Then
                                 Begin
                                   hp1 := nil;
{ although it's perfectly ok to remove an instruction which doesn't contain }
{ the register that we've just checked (CheckSequence takes care of that),  }
{ the sequence containing this other register should also be completely     }
{ checked and removed, otherwise we may get situations like this:           }
{                                                                           }
{   movl 12(%ebp), %edx                       movl 12(%ebp), %edx           }
{   movl 16(%ebp), %eax                       movl 16(%ebp), %eax           }
{   movl 8(%edx), %edx                        movl 8(%edx), %edx            }
{   movl (%eax), eax                          movl (%eax), eax              }
{   cmpl %eax, %edx                           cmpl %eax, %edx               }
{   jnz  l123           getting converted to  jnz  l123                     }
{   movl 12(%ebp), %edx                       movl 4(%eax), eax             }
{   movl 16(%ebp), %eax                                                     }
{   movl 8(%edx), %edx                                                      }
{   movl 4(%eax), eax                                                       }
                                   hp2 := p;
                                   Cnt2 := 1;
                                  While Cnt2 <= Cnt Do
                                     Begin
                                       If Not(RegInInstruction(Paicpu(hp2)^.oper[1].reg, p)) then
                                         begin
                                           if ((p^.typ = ait_instruction) And
                                               ((paicpu(p)^.OpCode = A_MOV)  or
                                                (paicpu(p)^.opcode = A_MOVZX) or
                                                (paicpu(p)^.opcode = A_MOVSX)) And
                                               (paicpu(p)^.Oper[0].typ in
                                                 [top_const,top_ref,top_symbol])) and
                                               (paicpu(p)^.oper[1].typ = top_reg) then
                                             begin
                                               regCounter := reg32(paicpu(p)^.oper[1].reg);
                                               if (regCounter in reginfo.regsStillUsedAfterSeq) then
                                                 begin
                                                   if (hp1 = nil) then
                                                     hp1 := reginfo.lastReload[regCounter];
                                                 end
{$ifndef noremove}
                                               else
                                                 begin
                                                   hp5 := p;
                                                   for cnt3 := ppaiprop(p^.optinfo)^.regs[regCounter].nrofmods downto 1 do
                                                     begin
                                                       if regModifiedByInstruction(regCounter,hp5) then
                                                         PPaiProp(hp5^.OptInfo)^.CanBeRemoved := True;
                                                       getNextInstruction(hp5,hp5);
                                                     end;
                                                   while (regCounter in PPaiProp(hp5^.optInfo)^.usedRegs) and
                                                         not regLoadedWithNewValue(regCounter,false,hp5) do
                                                     begin
                                                       if regModifiedByInstruction(regCounter,hp5) then
                                                         PPaiProp(hp5^.OptInfo)^.CanBeRemoved := True;
                                                        getNextInstruction(hp5,hp5);
                                                     end;
{$endif noremove}                                end
                                             end
                                         end
{$ifndef noremove}
                                       else
                                         PPaiProp(p^.OptInfo)^.CanBeRemoved := True
{$endif noremove}
                                       ; Inc(Cnt2);
                                       GetNextInstruction(p, p);
                                     End;
                                   hp3 := New(Pai_Marker,Init(NoPropInfoStart));
                                   InsertLLItem(AsmL, Pai(hp2^.Previous), hp2, hp3);
 {hp4 is used to get the contents of the registers before the sequence}
                                   GetLastInstruction(hp2, hp4);
{$IfDef CSDebug}
              For RegCounter := R_EAX To R_EDI Do
                If (RegCounter in RegInfo.RegsLoadedForRef) Then
                  Begin
           hp5 := new(pai_asm_comment,init(strpnew('New: '+att_reg2str[RegCounter]+', Old: '+
                                                  att_reg2str[RegInfo.New2OldReg[RegCounter]])));
           InsertLLItem(AsmL, Pai(hp2^.previous), hp2, hp5);
                  End;
{$EndIf CSDebug}
 { If some registers were different in the old and the new sequence, move }
 { the contents of those old registers to the new ones                    }
                                   For RegCounter := R_EAX To R_EDI Do
                                     If Not(RegCounter in [R_ESP,procinfo^.framepointer]) And
                                        (RegInfo.New2OldReg[RegCounter] <> R_NO) Then
                                       Begin
                                         AllocRegBetween(AsmL,RegInfo.New2OldReg[RegCounter],
                                           PPaiProp(hp4^.OptInfo)^.Regs[RegInfo.New2OldReg[RegCounter]].StartMod,hp2);
                                         If Not(RegCounter In RegInfo.RegsLoadedForRef) And
                                                        {old reg                new reg}
                                            (RegInfo.New2OldReg[RegCounter] <> RegCounter) Then
                                           Begin
                                                 hp3 := New(Paicpu,Op_Reg_Reg(A_MOV, S_L,
                                                                         {old reg          new reg}
                                                       RegInfo.New2OldReg[RegCounter], RegCounter));
                                                 InsertLLItem(AsmL, Pai(hp2^.previous), hp2, hp3);
                                           End
                                         Else
{   imagine the following code:                                            }
{        normal                    wrong optimized                         }
{    movl 8(%ebp), %eax           movl 8(%ebp), %eax                       }
{    movl (%eax), %eax            movl (%eax), %eax                        }
{    cmpl 8(%ebp), %eax           cmpl 8(%ebp), %eax                       }
{    jne l1                       jne l1                                   }
{    movl 8(%ebp), %eax                                                    }
{    movl (%eax), %edi            movl %eax, %edi                          }
{    movl %edi, -4(%ebp)          movl %edi, -4(%ebp)                      }
{    movl 8(%ebp), %eax                                                    }
{    pushl 70(%eax)               pushl 70(%eax)                           }
{                                                                          }
{   The error is that at the moment that the last instruction is executed, }
{   %eax doesn't contain 8(%ebp) anymore. Solution: the contents of        }
{   registers that are completely removed from a sequence (= registers in  }
{   RegLoadedForRef, have to be changed to their contents from before the  }
{   sequence.                                                              }
                                         If RegCounter in RegInfo.RegsLoadedForRef Then
                                           Begin
                                             hp3 := hp2;
                                             { cnt still holds the number of instructions }
                                             { of the sequence, so go to the end of it    }
                                             for cnt2 := 1 to pred(cnt) Do
                                               getNextInstruction(hp3,hp3);
                                             { hp4 = instruction prior to start of sequence }
                                             restoreRegContentsTo(regCounter,
                                               PPaiProp(hp4^.OptInfo)^.Regs[RegCounter],
                                               hp2,hp3);
                                           End;
                                       End;
                                   hp3 := New(Pai_Marker,Init(NoPropInfoEnd));
                                   InsertLLItem(AsmL, Pai(hp2^.Previous), hp2, hp3);
                                   If hp1 <> nil Then
                                     p := hp1;
                                   Continue;
                                 End
                               Else
                                 If (PPaiProp(p^.OptInfo)^.
                                      Regs[Reg32(Paicpu(p)^.oper[1].reg)].Typ = Con_Ref) And
                                    (PPaiProp(p^.OptInfo)^.CanBeRemoved) Then
                                   if (cnt > 0) then
                                     begin
                                       hp2 := p;
                                       Cnt2 := 1;
                                       While Cnt2 <= Cnt Do
                                         Begin
                                           If RegInInstruction(Paicpu(hp2)^.oper[1].reg, p) Then
                                             PPaiProp(p^.OptInfo)^.CanBeRemoved := False;
                                           Inc(Cnt2);
                                           GetNextInstruction(p, p);
                                         End;
                                       Continue;
                                     End
                                   else
                                     begin
                                       { Fix for web bug 972 }
                                       regCounter := Reg32(Paicpu(p)^.oper[1].reg);
                                       cnt := PPaiProp(p^.optInfo)^.Regs[regCounter].nrOfMods;
                                       hp3 := p;
                                       for cnt2 := 1 to cnt do
                                         if not(regModifiedByInstruction(regCounter,hp3) and
                                                not(PPaiProp(hp3^.optInfo)^.canBeRemoved)) then
                                           getNextInstruction(hp3,hp3)
                                         else
                                           break;
                                       getLastInstruction(p,hp4);
                                       RestoreRegContentsTo(regCounter,
                                         PPaiProp(hp4^.optInfo)^.Regs[regCounter],
                                         p,hp3);
                                     end;
                              End;
                          End;
                      End;
                    top_symbol,Top_Const:
                      Begin
                        Case Paicpu(p)^.oper[1].typ Of
                          Top_Reg:
                            Begin
                              regCounter := Reg32(Paicpu(p)^.oper[1].reg);
                              If GetLastInstruction(p, hp1) Then
                                With PPaiProp(hp1^.OptInfo)^.Regs[regCounter] Do
                                  If (Typ = Con_Const) And
                                     (paicpu(startMod)^.opsize >= paicpu(p)^.opsize) and
                                     opsequal(paicpu(StartMod)^.oper[0],paicpu(p)^.oper[0]) Then
                                    begin
                                      PPaiProp(p^.OptInfo)^.CanBeRemoved := True;
                                      allocRegBetween(asmL,regCounter,startMod,p);
                                    end;
                            End;
                        End;
                      End;
                  End;
                End;
              A_STD: If GetLastInstruction(p, hp1) And
                        (PPaiProp(hp1^.OptInfo)^.DirFlag = F_Set) Then
                        PPaiProp(Pai(p)^.OptInfo)^.CanBeRemoved := True;
            End
          End;
      End;
      GetNextInstruction(p, p);
    End;
End;

Procedure RemoveInstructs(AsmL: PAasmOutput; First, Last: Pai);
{ Removes the marked instructions and disposes the PPaiProps of the other }
{ instructions                                                            }
Var p, hp1: Pai;
begin
  p := First;
  While (p <> Last) Do
    Begin
      If (p^.typ = ait_marker) and
         (pai_marker(p)^.kind in [noPropInfoStart,noPropInfoEnd]) then
        begin
          hp1 := pai(p^.next);
          asmL^.remove(p);
          dispose(p,done);
          p := hp1
        end
      else
{$ifndef noinstremove}
        if assigned(p^.optInfo) and
              PPaiProp(p^.optInfo)^.canBeRemoved then
          begin
{$IfDef TP}
            Dispose(PPaiProp(p^.OptInfo));
{$EndIf}
            hp1 := pai(p^.next);
            AsmL^.Remove(p);
            Dispose(p, Done);
            p := hp1;
          End
        Else
{$endif noinstremove}
          Begin
{$IfDef TP}
            if assigned(p^.optInfo) then
              Dispose(PPaiProp(p^.OptInfo));
{$EndIf TP}
            p^.OptInfo := nil;
            p := pai(p^.next);;
          End;
    End;
{$IfNDef TP}
    FreeMem(PaiPropBlock, NrOfPaiObjs*(((SizeOf(TPaiProp)+3)div 4)*4))
{$EndIf TP}
End;

Procedure CSE(AsmL: PAasmOutput; First, Last: Pai);
Begin
  DoCSE(AsmL, First, Last);
  RemoveInstructs(AsmL, First, Last);
End;

End.

{
 $Log: csopt.pas,v $
 Revision 1.1.2.2  2002/04/15 21:04:41  jonas
   * fixed web bug 1932

 Revision 1.1.2.1  2001/03/04 02:22:19  carl
 - renamefest!

 Revision 1.1.2.5  2001/02/25 04:04:10  carl
 + missed updating some unit inclusion

 Revision 1.1.2.4  2000/12/05 09:55:09  jonas
   - removed all newoptimizations stuff since it will never be active under
     1.0.x anyway

 Revision 1.1.2.3  2000/08/25 19:35:08  jonas
   * refined previous fix a bit, some instructions weren't being removed
     while they could

 Revision 1.1.2.2  2000/08/23 12:47:45  jonas
   * fix for web bug 1112 and a bit of clean up in csopt386

 Revision 1.1.2.1  2000/08/04 19:57:45  jonas
   * improved detection of range of instructions which use a register

 Revision 1.1  2000/07/13 06:29:48  michael
 + Initial import

 Revision 1.61  2000/06/18 18:13:12  peter
   * removed unused var

 Revision 1.60  2000/06/03 09:41:37  jonas
   * fixed web bug 972, test for the bug in tests/testopt/testcse3.pp

 Revision 1.59  2000/06/01 11:01:20  peter
   * removed notes

 Revision 1.58  2000/04/29 16:57:44  jonas
   * fixed incompatibility with range chcking code, -O2 and higher
     now work correctly together with -Cr

 Revision 1.57  2000/04/10 12:45:57  jonas
   * fixed a serious bug in the CSE which (I think) only showed with
     -dnewoptimizations when using multi-dimensional arrays with
     elements of a size different from 1, 2 or 4 (especially strings).
   * made the DFA/CSE more robust (much less dependent on specifics of the
     code generator)

 Revision 1.56  2000/03/25 19:05:47  jonas
   * fixed some things for -Or. Make cycle now works with -OG2p3r if
     you use -Aas. There still a bug in popt386.pas that causes a
     problem with the binary writer, but I haven't found it yet

 Revision 1.55  2000/03/24 15:54:49  jonas
   * fix for -dnewoptimizations and -Or (never remove stores to regvars)
     but make cycle with -OG2p3r still fails :(

 Revision 1.54  2000/02/24 18:41:38  peter
   * removed warnings/notes

 Revision 1.53  2000/02/19 13:50:29  jonas
   * fixed bug in -dnewoptizations (showed itself  only if -Or was
     used as well I think)

 Revision 1.52  2000/02/17 07:46:49  jonas
   * -dreplacereg no logner tries to optimize "movl %reg1,%reg1" (which are
     always marked as CanBeRemoved)
   + some comments in -dreplacereg code
   * small fix which could cause crash when optimizer is compiler with -dTP

 Revision 1.51  2000/02/12 19:28:56  jonas
   * fix for imul optimization in popt386 (exclude top_ref as first
     argument)
   * in csopt386: change "mov reg1,reg2; <several operations on reg2>;
     mov reg2,reg1" to "<several operations on reg1>" (-dnewopt...)

 Revision 1.50  2000/02/12 14:10:14  jonas
   + change "mov reg1,reg2;imul x,reg2" to "imul x,reg1,reg2" in popt386
     (-dnewoptimizations)
   * shl(d) and shr(d) are considered to have a hardcoded register if
     they use cl as shift count (since you can't replace them with
     another register) in csopt386 (also for -dnewoptimizations)

 Revision 1.49  2000/02/12 10:54:18  jonas
   * fixed edi allocation in allocRegBetween
   * fixed bug I introduced yesterday, added comment to prevent it from
     happening again in the future

 Revision 1.48  2000/02/11 23:50:03  jonas
   * fixed crashing bug under Dos with -dnewoptimizations (found it,
     John!). Don't understand why it didn't crash under Linux :(

 Revision 1.47  2000/02/10 16:04:43  jonas
   * fixed stupid typo!

 Revision 1.46  2000/02/10 15:07:41  jonas
   * fixed small bug introduced with my previous fix

 Revision 1.45  2000/02/10 14:57:13  jonas
   * fixed bug due to lack of support for top_symbol operands

 Revision 1.44  2000/02/09 13:22:51  peter
   * log truncated

 Revision 1.43  2000/02/04 13:52:17  jonas
   * better support for regvars (still needs a move of the call to the optimize
   procedure to a place where resetusableregisters is not yet called to work)
   * small regallocation fixes for -dnewoptimizations

 Revision 1.42  2000/01/28 15:15:31  jonas
    * moved skipinstr from daopt386 to aasm
    * fixed crashing bug with -dreplacereg in csopt386.pas

 Revision 1.41  2000/01/23 11:11:37  michael
 + Fixes from Jonas.

 Revision 1.40  2000/01/22 16:10:06  jonas
   + all code generator generated "mov reg1,reg2" instructions are now
     attempted to be removed using the replacereg code
     (-dnewoptimizations)
   * small fixes to -dreplacereg code

 Revision 1.39  2000/01/13 13:07:05  jonas
   * released -dalignreg
   * some small fixes to -dnewOptimizations helper procedures

 Revision 1.38  2000/01/07 01:14:23  peter
   * updated copyright to 2000

 Revision 1.37  2000/01/03 17:11:17  jonas
   * fixed bug with -dreplacereg

 Revision 1.36  1999/12/05 16:48:43  jonas
   * CSE of constant loading in regs works properly again
   + if a constant is stored into memory using "mov const, ref" and
     there is a reg that contains this const, it is changed into
     "mov reg, ref"

 Revision 1.35  1999/12/02 11:26:41  peter
   * newoptimizations define added

 Revision 1.34  1999/11/21 13:09:41  jonas
   * fixed some missed optimizations because 8bit regs were not always
     taken into account

 Revision 1.33  1999/11/20 11:37:03  jonas
   * make cycle works with -dreplacereg (register renaming)! I have not
     tested it yet together with -darithopt, but I don't expect problems

 Revision 1.32  1999/11/14 11:26:53  jonas
   + basic register renaming (not yet working completely, between
     -dreplacereg/-dreplaceregdebug)

 Revision 1.31  1999/11/06 16:21:57  jonas
   + search optimial register to use in alignment code (compile with
     -dalignreg, -dalignregdebug to see chosen register in
     assembler code). Still needs support in ag386bin.

 Revision 1.30  1999/11/06 14:34:20  peter
   * truncated log to 20 revs

 Revision 1.29  1999/11/05 16:01:46  jonas
   + first implementation of choosing least used register for alignment code
      (not yet working, between ifdef alignreg)

 Revision 1.28  1999/10/11 11:11:31  jonas
   * fixed bug which sometimes caused a crash when optimizing blocks of code with
     assembler blocks (didn't notice before because of lack of zero page protection
     under Win9x :( )

 Revision 1.27  1999/10/01 13:51:40  jonas
   * CSE now updates the RegAlloc's

 Revision 1.26  1999/09/30 14:43:13  jonas
   * fixed small efficiency which caused some missed optimizations (saves 1
     assembler instruction on the whole compiler/RTL source tree! :)

 Revision 1.25  1999/09/27 23:44:50  peter
   * procinfo is now a pointer
   * support for result setting in sub procedure

 Revision 1.24  1999/08/25 11:59:58  jonas
   * changed pai386, paippc and paiapha (same for tai*) to paicpu (taicpu)

}
