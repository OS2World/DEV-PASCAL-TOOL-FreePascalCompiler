{
    $Id: ag68kmot.pas,v 1.1.2.7 2002/12/02 16:24:08 pierre Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    This unit implements an asmoutput class for MOTOROLA syntax with
    Motorola 68000 (recognized by the Amiga Assembler and Charlie Gibbs's
    A68k)

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
unit ag68kmot;

    interface

    uses aasm,assemble;

    type
      pm68kmotasmlist=^tm68kmotasmlist;
      tm68kmotasmlist = object(tasmlist)
        procedure WriteTree(p:paasmoutput);virtual;
        procedure WriteExternals;virtual;
        procedure WriteAsmList;virtual;
      end;

  implementation

    uses
      globtype,systems,
      dos,globals,cobjects,
      cpubase,cpuasm,
      strings,files,verbose
{$ifdef GDB}
      ,gdb
{$endif GDB}
      ;

    const
      line_length = 70;

    function double2str(d : double) : string;
      var
         hs : string;
      begin
         str(d,hs);
         double2str:=hs;
      end;


(* TO SUPPORT SOONER OR LATER!!!
    function comp2str(d : bestreal) : string;
      type
        pdouble = ^double;
      var
        c  : comp;
        dd : pdouble;
      begin
      {$ifdef TP}
         c:=d;
      {$else}
         c:=comp(d);
      {$endif}
         dd:=pdouble(@c); { this makes a bitwise copy of c into a double }
         comp2str:=double2str(dd^);
      end; *)


    function getreferencestring(const ref : treference) : string;
      var
         s : string;
      begin
         s:='';
         if ref.is_immediate then
             s:='#'+tostr(ref.offset)
         else
           with ref do
             begin
                 if (index=R_NO) and (base=R_NO) and (direction=dir_none) then
                   begin
                     if assigned(symbol) then
                       begin
                         s:=s+symbol^.name;
                         if offset<0 then
                           s:=s+tostr(offset)
                         else
                         if (offset>0) then
                           s:=s+'+'+tostr(offset);
                       end
                     else
                       begin
                       { direct memory addressing }
                         s:=s+'('+tostr(offset)+').l';
                       end;
                   end
                 else
                   begin
                     if assigned(symbol) then
                       s:=s+symbol^.name;
                     if offset<0 then
                       s:=s+tostr(offset)
                     else
                     if (offset>0) then
                       begin
                         if (symbol=nil) then s:=tostr(offset)
                         else s:=s+'+'+tostr(offset);
                       end;
                     if (index<>R_NO) and (base=R_NO) and (direction=dir_none) then
                       begin
                         if (scalefactor = 1) or (scalefactor = 0) then
                           begin
                             if offset = 0 then
                               s:=s+'0(,'+mot_reg2str[index]+'.l)'
                             else
                               s:=s+'(,'+mot_reg2str[index]+'.l)';
                           end
                         else
                           begin
                             if offset = 0 then
                               s:=s+'0(,'+mot_reg2str[index]+'.l*'+tostr(scalefactor)+')'
                             else
                               s:=s+'(,'+mot_reg2str[index]+'.l*'+tostr(scalefactor)+')';
                           end
                       end
                     else
                     if (index=R_NO) and (base<>R_NO) and (direction=dir_inc) then
                       begin
                         if (scalefactor = 1) or (scalefactor = 0) then
                           s:=s+'('+mot_reg2str[base]+')+'
                         else
                           InternalError(10002);
                       end
                     else
                     if (index=R_NO) and (base<>R_NO) and (direction=dir_dec) then
                       begin
                         if (scalefactor = 1) or (scalefactor = 0) then
                           s:=s+'-('+mot_reg2str[base]+')'
                         else
                           InternalError(10003);
                       end
                     else
                     if (index=R_NO) and (base<>R_NO) and (direction=dir_none) then
                       begin
                         s:=s+'('+mot_reg2str[base]+')';
                       end
                     else
                     if (index<>R_NO) and (base<>R_NO) and (direction=dir_none) then
                       begin
                         if (scalefactor = 1) or (scalefactor = 0) then
                           begin
                             if offset = 0 then
                               s:=s+'0('+mot_reg2str[base]+','+mot_reg2str[index]+'.l)'
                             else
                               s:=s+'('+mot_reg2str[base]+','+mot_reg2str[index]+'.l)';
                           end
                         else
                          begin
                            if offset = 0 then
                              s:=s+'0('+mot_reg2str[base]+','+mot_reg2str[index]+'.l*'+tostr(scalefactor)+')'
                            else
                              s:=s+'('+mot_reg2str[base]+','+mot_reg2str[index]+'.l*'+tostr(scalefactor)+')';
                          end
                       end
      { if this is not a symbol, and is not in the above, then there is an error }
                     else
                     if NOT assigned(symbol) then
                       InternalError(10004);
                   end; { endif }
            end; { end with }
         getreferencestring:=s;
      end;


    function getopstr(const o:toper) : string;
    var
      hs : string;
      i : tregister;
    begin
      case o.typ of
            top_reg : getopstr:=mot_reg2str[o.reg];
            top_ref : getopstr:=getreferencestring(o.ref^);
        top_reglist : begin
                      hs:='';
                      for i:=R_NO to R_FPSR do
                      begin
                        if i in o.registerlist^ then
                         hs:=hs+mot_reg2str[i]+'/';
                      end;
                      delete(hs,length(hs),1);
                      getopstr := hs;
                    end;
             top_const : getopstr:='#'+tostr(o.val);
            top_symbol :
                    { compare with i386, where a symbol is considered }
                    { a constant.                                     }
                    begin
                     if assigned(o.sym) then
                       hs:=o.sym^.name
                     else
                       hs:='';
                       if o.symofs>0 then
                        hs:=hs+'+'+tostr(o.symofs)
                       else
                        if o.symofs<0 then
                         hs:=hs+tostr(o.symofs)
                       else
                        if not(assigned(o.sym)) then
                          hs:=hs+'0';
                       getopstr:=hs;
                    end;
            else internalerror(10001);
         end;
      end;



    function getopstr_jmp(const o:toper) : string;
    var
      hs : string;
    begin
      case o.typ of
            top_reg : getopstr_jmp:=mot_reg2str[o.reg];
            top_ref : getopstr_jmp:=getreferencestring(o.ref^);
            top_const : getopstr_jmp:=tostr(o.val);
            top_symbol : begin
                           if assigned(o.sym) then
                             hs:=o.sym^.name
                           else
                             hs:='';
                             if o.symofs>0 then
                              hs:=hs+'+'+tostr(o.symofs)
                             else
                              if o.symofs<0 then
                               hs:=hs+tostr(o.symofs)
                             else
                              if not(assigned(o.sym)) then
                                hs:=hs+'0';
                           getopstr_jmp:=hs;
                         end;
            else internalerror(10001);
         end;
      end;


{****************************************************************************
                              TM68KMOTASMLIST
 ****************************************************************************}

    var
      LastSec : tsection;

    const
      section2str : array[tsection] of string[6]=
       ('','CODE','DATA','BSS','','','','','','','','');
      tempallocstr:array[boolean] of string[16] =
       (' released',' allocated');


    procedure tm68kmotasmlist.WriteTree(p:paasmoutput);
    var
      hp        : pai;
      s         : string;
      counter,
      i,j,lines : longint;
      quoted    : boolean;
    begin
      if not assigned(p) then
       exit;
      hp:=pai(p^.first);
      while assigned(hp) do
       begin
         case hp^.typ of
       ait_comment : Begin
                       AsmWrite(target_asm.comment);
                       AsmWritePChar(pai_asm_comment(hp)^.str);
                       AsmLn;
                     End;
       ait_section : begin
                       if pai_section(hp)^.sec<>sec_none then
                        begin

                          AsmLn;
                          AsmWriteLn('SECTION _'+section2str[pai_section(hp)^.sec]+','+section2str[pai_section(hp)^.sec]);
                        end;
                       LastSec:=pai_section(hp)^.sec;
                     end;
{$ifdef DREGALLOC}
      ait_regalloc : AsmWriteLn(target_asm.comment+'Register '+att_reg2str[pairegalloc(hp)^.reg]+' allocated');
    ait_regdealloc : AsmWriteLn(target_asm.comment+'Register '+att_reg2str[pairegalloc(hp)^.reg]+' released');
{$endif DREGALLOC}
           ait_tempalloc :
             begin
               if (cs_asm_tempalloc in aktglobalswitches) then
                 begin
{$ifdef EXTDEBUG}
                   if assigned(paitempalloc(hp)^.problem) then
                     AsmWriteLn(target_asm.comment+paitempalloc(hp)^.problem^+' ('+tostr(paitempalloc(hp)^.temppos)+','+
                       tostr(paitempalloc(hp)^.tempsize)+')')
                   else
{$endif EXTDEBUG}
                     AsmWriteLn(target_asm.comment+'Temp '+tostr(paitempalloc(hp)^.temppos)+','+
                       tostr(paitempalloc(hp)^.tempsize)+tempallocstr[paitempalloc(hp)^.allocation]);
                 end;
             end;

         ait_align : AsmWriteLn(#9'CNOP 0,'+tostr(pai_align(hp)^.aligntype));
    ait_real_80bit : Message(asmw_e_extended_not_supported);
    ait_comp_64bit : Message(asmw_e_comp_not_supported);
     ait_datablock : begin
                       if pai_datablock(hp)^.is_global then
                        begin
                          AsmWrite(#9'XDEF'#9);
                          AsmWriteln(pai_datablock(hp)^.sym^.name);
                        end;
                       AsmWrite(pai_datablock(hp)^.sym^.name);
                       AsmWriteln(#9#9'DS.B '+tostr(pai_datablock(hp)^.size));
                     end;
   ait_const_32bit : Begin
                       AsmWriteLn(#9#9'DC.L'#9+tostr(pai_const(hp)^.value));
                     end;
   ait_const_16bit : Begin
                       AsmWriteLn(#9#9'DC.W'#9+tostr(pai_const(hp)^.value));
                     end;
    ait_const_8bit : AsmWriteLn(#9#9'DC.B'#9+tostr(pai_const(hp)^.value));
  ait_const_symbol :
                     Begin
                       AsmWrite(#9#9+'DC.L '#9);
                       AsmWrite(pai_const_symbol(hp)^.sym^.name);
                       if pai_const_symbol(hp)^.offset>0 then
                         AsmWrite('+'+tostr(pai_const_symbol(hp)^.offset))
                       else if pai_const_symbol(hp)^.offset<0 then
                         AsmWrite(tostr(pai_const_symbol(hp)^.offset));
                       AsmLn;
                     end;
    ait_real_64bit : Begin
                       AsmWriteLn(#9#9'DC.D'#9+double2str(pai_real_64bit(hp)^.value));
                     end;
    ait_real_32bit : Begin
                       AsmWriteLn(#9#9'DC.S'#9+double2str(pai_real_32bit(hp)^.value));
                     end;
{ TO SUPPORT SOONER OR LATER!!!
          ait_comp : AsmWriteLn(#9#9'DC.D'#9+comp2str(pai_extended(hp)^.value));}
        ait_string : begin
                       counter := 0;
                       lines := pai_string(hp)^.len div line_length;
                       { separate lines in different parts }
                       if pai_string(hp)^.len > 0 then
                       Begin
                         for j := 0 to lines-1 do
                           begin
                              AsmWrite(#9#9'DC.B'#9);
                              quoted:=false;
                              for i:=counter to counter+line_length do
                                 begin
                                   { it is an ascii character. }
                                   if (ord(pai_string(hp)^.str[i])>31) and
                                      (ord(pai_string(hp)^.str[i])<128) and
                                      (pai_string(hp)^.str[i]<>'"') then
                                   begin
                                     if not(quoted) then
                                     begin
                                       if i>counter then
                                         AsmWrite(',');
                                       AsmWrite('"');
                                     end;
                                     AsmWrite(pai_string(hp)^.str[i]);
                                     quoted:=true;
                                   end { if > 31 and < 128 and ord('"') }
                                   else
                                   begin
                                     if quoted then
                                       AsmWrite('"');
                                     if i>counter then
                                       AsmWrite(',');
                                     quoted:=false;
                                     AsmWrite(tostr(ord(pai_string(hp)^.str[i])));
                                   end;
                                end; { end for i:=0 to... }
                                if quoted then AsmWrite('"');
                                AsmLn;
                                counter := counter+line_length;
                               end; { end for j:=0 ... }
                               { do last line of lines }
                               AsmWrite(#9#9'DC.B'#9);
                               quoted:=false;
                               for i:=counter to pai_string(hp)^.len-1 do
                               begin
                                 { it is an ascii character. }
                                 if (ord(pai_string(hp)^.str[i])>31) and
                                    (ord(pai_string(hp)^.str[i])<128) and
                                    (pai_string(hp)^.str[i]<>'"') then
                                 begin
                                   if not(quoted) then
                                   begin
                                     if i>counter then
                                       AsmWrite(',');
                                     AsmWrite('"');
                                   end;
                                 AsmWrite(pai_string(hp)^.str[i]);
                                   quoted:=true;
                                 end { if > 31 and < 128 and " }
                                 else
                                 begin
                                   if quoted then
                                     AsmWrite('"');
                                     if i>counter then
                                       AsmWrite(',');
                                     quoted:=false;
                                     AsmWrite(tostr(ord(pai_string(hp)^.str[i])));
                                 end;
                               end; { end for i:=0 to... }
                             if quoted then AsmWrite('"');
                          end; { endif }
                        AsmLn;
                      end;
          ait_label : begin
                       if assigned(hp^.next) and (pai(hp^.next)^.typ in
                          [ait_const_32bit,ait_const_16bit,ait_const_8bit,
                           ait_const_symbol,ait_real_64bit,
                           ait_real_32bit,ait_string]) then
                        begin
                          if not(cs_littlesize in aktglobalswitches) then
                           AsmWriteLn(#9'CNOP 0,4')
                          else
                           AsmWriteLn(#9'CNOP 0,2');
                        end;
                        AsmWrite(pai_label(hp)^.l^.name);
                        if assigned(hp^.next) and not(pai(hp^.next)^.typ in
                           [ait_const_32bit,ait_const_16bit,
                            ait_const_8bit,ait_const_symbol,
                            ait_real_64bit,ait_string]) then
                         AsmWriteLn(':');
                      end;
         ait_direct : begin
                        AsmWritePChar(pai_direct(hp)^.str);
                        AsmLn;
                      end;
ait_labeled_instruction :
                      Begin
                      { labeled operand }
                        if pai_labeled(hp)^.register = R_NO then
                          begin
                            if pai_labeled(hp)^.lab <> nil then
                              begin
                                AsmWrite(#9+mot_op2str[pai_labeled(hp)^.opcode]+#9);
                                AsmWriteln(pai_labeled(hp)^.lab^.name)
                              end
                            else
                              begin
                                AsmWrite(#9+mot_op2str[pai_labeled(hp)^.opcode]+#9);
                                AsmWriteln(pai_labeled(hp)^.sym^.name);
                              end;
                          end
                        else
                      { labeled operand with register }
                         begin
                            if pai_labeled(hp)^.lab <> nil then
                              begin
                                AsmWrite(#9+mot_op2str[pai_labeled(hp)^.opcode]+#9+
                                  mot_reg2str[pai_labeled(hp)^.register]+',');
                                AsmWriteln(pai_labeled(hp)^.lab^.name);
                              end
                            else
                            { a symbol is the value }
                              begin
                                AsmWrite(#9+mot_op2str[pai_labeled(hp)^.opcode]+#9+
                                  mot_reg2str[pai_labeled(hp)^.register]+',');
                                AsmWriteln(pai_labeled(hp)^.sym^.name);
                              end;
                         end;
                     end;
        ait_symbol : begin
                       { ------------------------------------------------------- }
                       { ----------- ALIGNMENT FOR ANY NON-BYTE VALUE ---------- }
                       { ------------- REQUIREMENT FOR 680x0 ------------------- }
                       { ------------------------------------------------------- }
                       if assigned(hp^.next) and (pai(hp^.next)^.typ in
                          [ait_const_32bit,ait_const_16bit,
                           ait_const_symbol,ait_const_8bit,
                           ait_real_64bit,ait_real_32bit,ait_string]) then
                        begin
                          if not(cs_littlesize in aktglobalswitches) then
                           AsmWriteLn(#9'CNOP 0,4')
                          else
                           AsmWriteLn(#9'CNOP 0,2');
                        end;
                       if pai_symbol(hp)^.is_global then
                         begin
                           AsmWrite(#9'XDEF ');
                           AsmWriteln(pai_symbol(hp)^.sym^.name);
                         end;
                       AsmWrite(pai_symbol(hp)^.sym^.name);
                       if assigned(hp^.next) and not(pai(hp^.next)^.typ in
                          [ait_const_32bit,ait_const_16bit,ait_const_8bit,
                           ait_const_symbol,ait_real_64bit,
                           ait_string,ait_real_32bit]) then
                        AsmWriteLn(':');
                     end;
   ait_instruction : begin
                       s:=#9+mot_op2str[paicpu(hp)^.opcode]+mot_opsize2str[paicpu(hp)^.opsize];
                       if paicpu(hp)^.ops>0 then
                        begin
                        { call and jmp need an extra handling                          }
                        { this code is only called if jmp isn't a labeled instruction }
                          if paicpu(hp)^.opcode in [A_JSR,A_JMP] then
                           s:=s+#9+getopstr_jmp(paicpu(hp)^.oper[0])
                          else
                           begin
                              s:=s+#9+getopstr(paicpu(hp)^.oper[0]);
                             if paicpu(hp)^.ops>1 then
                              begin
                                 s:=s+','+getopstr(paicpu(hp)^.oper[1]);
                             { three operands }
                                if paicpu(hp)^.ops>2 then
                                 begin
                                   if (paicpu(hp)^.opcode = A_DIVSL) or
                                      (paicpu(hp)^.opcode = A_DIVUL) or
                                      (paicpu(hp)^.opcode = A_MULU) or
                                      (paicpu(hp)^.opcode = A_MULS) or
                                      (paicpu(hp)^.opcode = A_DIVS) or
                                      (paicpu(hp)^.opcode = A_DIVU) then
                                    s:=s+':'+getopstr(paicpu(hp)^.oper[2])
                                   else
                                    s:=s+','+getopstr(paicpu(hp)^.oper[2]);
                                 end;
                              end;
                           end;
                        end;
                       AsmWriteLn(s);
                     end;
{$ifdef GDB}
              ait_stabn,
              ait_stabs,
         ait_force_line,
 ait_stab_function_name : ;
{$endif GDB}
        ait_marker : ;
         else
          internalerror(10000);
         end;
         hp:=pai(hp^.next);
       end;
    end;

var
  currentasmlist : pasmlist;

    procedure writeexternal(p:pnamedindexobject);{$ifndef FPC}far;{$endif}
      begin
        if pasmsymbol(p)^.deftyp=AS_EXTERNAL then
          begin
            currentasmlist^.AsmWrite(#9'XREF'#9);
            currentasmlist^.AsmWriteLn(p^.name);
          end;
      end;

    procedure tm68kmotasmlist.WriteExternals;
      begin
        currentasmlist:=@self;
        AsmSymbolList^.foreach({$ifndef TP}@{$endif}writeexternal);
      end;


    procedure tm68kmotasmlist.WriteAsmList;
    begin
{$ifdef EXTDEBUG}
      if assigned(current_module^.mainsource) then
       comment(v_info,'Start writing motorola-styled assembler output for '+current_module^.mainsource^);
{$endif}

      countlabelref:=false;
      WriteExternals;
    { WriteTree(debuglist);}
      WriteTree(codesegment);
      WriteTree(datasegment);
      WriteTree(consts);
      WriteTree(rttilist);
      WriteTree(bsssegment);
      Writetree(importssection);
      Writetree(exportssection);
      Writetree(resourcesection);
      countlabelref:=true;

      AsmLn;
      AsmWriteLn(#9'END');
      AsmLn;

{$ifdef EXTDEBUG}
      if assigned(current_module^.mainsource) then
       comment(v_info,'Done writing motorola-styled assembler output for '+current_module^.mainsource^);
{$endif}
    end;

end.
{
  $Log: ag68kmot.pas,v $
  Revision 1.1.2.7  2002/12/02 16:24:08  pierre
   * avoid problem if label name is 255 chars long

  Revision 1.1.2.6  2002/10/29 15:55:27  pierre
   * cycle works again with -So -dTP options

  Revision 1.1.2.5  2002/10/22 10:13:51  pierre
   * adapt to changes in aasm and temp_gen units

  Revision 1.1.2.4  2002/09/16 21:08:01  pierre
   * get it to compile again

  Revision 1.1.2.3  2001/09/14 03:00:29  carl
  - removed forced alignment of syms (handled by cg and symbol table now)

  Revision 1.1.2.2  2001/04/21 00:33:47  carl
  * m68k updates

  Revision 1.1.2.1  2001/02/25 01:39:37  carl
  - moved into m68k directory

  Revision 1.1.2.1  2001/02/23 10:05:13  pierre
   * first bunch of m68k cpu updates

  Revision 1.1  2000/07/13 06:29:43  michael
  + Initial import

  Revision 1.20  2000/02/09 13:22:44  peter
    * log truncated

  Revision 1.19  2000/01/07 01:14:18  peter
    * updated copyright to 2000

  Revision 1.18  1999/09/16 23:05:51  florian
    * m68k compiler is again compilable (only gas writer, no assembler reader)

}
