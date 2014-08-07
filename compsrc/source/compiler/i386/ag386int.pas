{
    $Id: ag386int.pas,v 1.6.2.10 2002/12/11 13:17:15 pierre Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    This unit implements an asmoutput class for Intel syntax with Intel i386+

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
{$ifdef TP}
  {$N+,E+}
{$endif}
unit ag386int;

    interface

    uses
      aasm,
      assemble,
      cpubase;

    type
      pi386intasmlist=^ti386intasmlist;
      ti386intasmlist = object(tasmlist)
        procedure WriteTree(p:paasmoutput);virtual;
        procedure WriteAsmList;virtual;
        procedure WriteReferenceString(var ref : treference);
        procedure WriteOpStr(const o:toper;s : topsize; opcode: tasmop;dest : boolean);
        procedure WriteOpStrJmp(const o:toper;s : topsize);
        Function  DoAssemble:boolean;virtual;
        procedure WriteExternals;
      end;

  implementation

    uses
      strings,script,
      globtype,globals,systems,cobjects,
      files,verbose,cpuasm
{$ifdef GDB}
      ,gdb
{$endif GDB}
      ;

    const
      line_length = 70;

{$ifdef EXTTYPE}
      extstr : array[EXT_NEAR..EXT_ABS] of String[8] =
             ('NEAR','FAR','PROC','BYTE','WORD','DWORD',
              'CODEPTR','DATAPTR','FWORD','PWORD','QWORD','TBYTE','ABS');
{$endif EXTTYPE}

    function single2str(d : single) : string;
      var
         hs : string;
         p : byte;
      begin
         str(d,hs);
      { nasm expects a lowercase e }
         p:=pos('E',hs);
         if p>0 then
          hs[p]:='e';
         p:=pos('+',hs);
         if p>0 then
          delete(hs,p,1);
         single2str:=lower(hs);
      end;

    function double2str(d : double) : string;
      var
         hs : string;
         p : byte;
      begin
         str(d,hs);
      { nasm expects a lowercase e }
         p:=pos('E',hs);
         if p>0 then
          hs[p]:='e';
         p:=pos('+',hs);
         if p>0 then
          delete(hs,p,1);
         double2str:=lower(hs);
      end;

    function extended2str(e : extended) : string;
      var
         hs : string;
         p : byte;
      begin
         str(e,hs);
      { nasm expects a lowercase e }
         p:=pos('E',hs);
         if p>0 then
          hs[p]:='e';
         p:=pos('+',hs);
         if p>0 then
          delete(hs,p,1);
         extended2str:=lower(hs);
      end;


    function comp2str(d : bestreal) : string;
      type
        pdouble = ^double;
      var
        c  : comp;
        dd : pdouble;
      begin
{$ifdef FPC}
         c:=comp(d);
{$else}
         c:=d;
{$endif}
         dd:=pdouble(@c); { this makes a bitwise copy of c into a double }
         comp2str:=double2str(dd^);
      end;

   function fixline(s:string):string;
   {
     return s with all leading and ending spaces and tabs removed
   }
     var
       i,j,k : longint;
     begin
       i:=length(s);
       while (i>0) and (s[i] in [#9,' ']) do
        dec(i);
       j:=1;
       while (j<i) and (s[j] in [#9,' ']) do
        inc(j);
       for k:=j to i do
        if s[k] in [#0..#31,#127..#255] then
         s[k]:='.';
       fixline:=Copy(s,j,i-j+1);
     end;


{****************************************************************************
                               TI386INTASMLIST
 ****************************************************************************}

    var
      LastSec : tsection;
      lastfileinfo : tfileposinfo;
      infile,
      lastinfile   : pinputfile;

    const
      ait_const2str:array[ait_const_32bit..ait_const_8bit] of string[8]=
        (#9'DD'#9,#9'DW'#9,#9'DB'#9);

    Function PadTabs(const p:string;addch:char):string;
    var
      s : string;
      i : longint;
    begin
      i:=length(p);
      if addch<>#0 then
       begin
         inc(i);
         s:=p+addch;
       end
      else
       s:=p;
      if i<8 then
       PadTabs:=s+#9#9
      else
       PadTabs:=s+#9;
    end;

    procedure ti386intasmlist.WriteReferenceString(var ref : treference);
    var
      s     : string;
      first : boolean;
    begin
      if ref.is_immediate then
       begin
         AsmWrite(tostr(ref.offset));
         exit;
       end
      else
      with ref do
        begin
          first:=true;
          inc(offset,offsetfixup);
          offsetfixup:=0;
          if ref.segment<>R_NO then
           s:=int_reg2str[segment]+':['
          else
           s:='[';
         if assigned(symbol) then
          begin
            if (aktoutputformat = as_i386_tasm) then
              s:=s+'dword ptr ';
            if length(symbol^.name)>200 then
              begin
                AsmWrite(s);
                AsmWrite(symbol^.name);
                s:='';
              end
            else
              s:=s+symbol^.name;
            first:=false;
          end;
         if (base<>R_NO) then
          begin
            if not(first) then
             s:=s+'+'
            else
             first:=false;
             s:=s+int_reg2str[base];
          end;
         if (index<>R_NO) then
           begin
             if not(first) then
               s:=s+'+'
             else
               first:=false;
             s:=s+int_reg2str[index];
             if scalefactor<>0 then
               s:=s+'*'+tostr(scalefactor);
           end;
         if offset<0 then
           s:=s+tostr(offset)
         else if (offset>0) then
           s:=s+'+'+tostr(offset);
         if s[length(s)]='[' then
           s:=s+'0';
         s:=s+']';
        end;
       AsmWrite(s);
     end;


    procedure ti386intasmlist.WriteOpStr(const o:toper;s : topsize; opcode: tasmop;dest : boolean);
    var
      hs : string;
    begin
      case o.typ of
        top_reg :
          hs:=int_reg2str[o.reg];
        top_const :
          hs:=tostr(o.val);
        top_symbol :
          begin
            if assigned(o.sym) then
              begin
                if length(o.sym^.name)>200 then
                  begin
                    AsmWrite('offset ');
                    AsmWrite(o.sym^.name);
                    hs:='';
                  end
                else
                  hs:='offset '+o.sym^.name;
              end
            else
              hs:='offset ';
            if o.symofs>0 then
             hs:=hs+'+'+tostr(o.symofs)
            else
             if o.symofs<0 then
              hs:=hs+tostr(o.symofs)
            else
             if not(assigned(o.sym)) then
               hs:=hs+'0';
          end;
        top_ref :
          begin
            hs:='';
            if ((opcode <> A_LGS) and (opcode <> A_LSS) and
                (opcode <> A_LFS) and (opcode <> A_LDS) and
                (opcode <> A_LES)) then
             Begin
               case s of
                S_B : hs:='byte ptr ';
                S_W : hs:='word ptr ';
                S_L : hs:='dword ptr ';
               S_IS : hs:='word ptr ';
               S_IL : hs:='dword ptr ';
               S_IQ : hs:='qword ptr ';
               S_FS : hs:='dword ptr ';
               S_FL : hs:='qword ptr ';
               S_FX : hs:='tbyte ptr ';
               S_BW : if dest then
                       hs:='word ptr '
                      else
                       hs:='byte ptr ';
               S_BL : if dest then
                       hs:='dword ptr '
                      else
                       hs:='byte ptr ';
               S_WL : if dest then
                       hs:='dword ptr '
                      else
                       hs:='word ptr ';
               end;
             end;
            AsmWrite(hs);
            WriteReferenceString(o.ref^);
            hs:='';
          end;
        else
          internalerror(10001);
      end;
      AsmWrite(hs);
    end;

    procedure ti386intasmlist.WriteOpStrJmp(const o:toper;s : topsize);
    var
      hs : string;
    begin
      case o.typ of
        top_reg :
          hs:=int_reg2str[o.reg];
        top_const :
          hs:=tostr(o.val);
        top_symbol :
          begin
            hs:=o.sym^.name;
            if length(hs)>200 then
              begin
                AsmWrite(hs);
                hs:='';
              end;
            if o.symofs>0 then
             hs:=hs+'+'+tostr(o.symofs)
            else
             if o.symofs<0 then
              hs:=hs+tostr(o.symofs);
          end;
        top_ref :
          { what about lcall or ljmp ??? }
          begin
            if (aktoutputformat = as_i386_tasm) then
              hs:=''
            else
              begin
                if s=S_FAR then
                  hs:='far ptr '
                else
                  hs:='dword ptr ';
              end;
            AsmWrite(hs);
            WriteReferenceString(o.ref^);
            hs:='';
          end;
        else
          internalerror(10001);
      end;
      AsmWrite(hs);
    end;

    procedure ti386intasmlist.WriteTree(p:paasmoutput);
    const
      allocstr : array[boolean] of string[10]=(' released',' allocated');
      nolinetai =[ait_label,
                  ait_regalloc,ait_tempalloc,
                  ait_stabn,ait_stabs,ait_section,
                  ait_cut,ait_marker,ait_align,ait_stab_function_name];
    var
      s,
      prefix   : string;
      hp       : pai;
      counter,
      lines,
      InlineLevel : longint;
      i,j,l    : longint;
      consttyp : tait;
      found,
      do_line,DoNotSplitLine,
      quoted   : boolean;
      sep      : char;
    begin
      if not assigned(p) then
       exit;
      { lineinfo is only needed for codesegment (PFV) }
      do_line:=((cs_asm_source in aktglobalswitches) or
                (cs_lineinfo in aktmoduleswitches))
                 and (p=codesegment);
      InlineLevel:=0;
      DoNotSplitLine:=false;
      hp:=pai(p^.first);
      while assigned(hp) do
       begin
         if do_line and not(hp^.typ in nolinetai) and
            not DoNotSplitLine then
           begin
           { load infile }
             if lastfileinfo.fileindex<>hp^.fileinfo.fileindex then
              begin
                infile:=current_module^.sourcefiles^.get_file(hp^.fileinfo.fileindex);
                if assigned(infile) then
                 begin
                   { open only if needed !! }
                   if (cs_asm_source in aktglobalswitches) then
                    infile^.open;
                 end;
                { avoid unnecessary reopens of the same file !! }
                lastfileinfo.fileindex:=hp^.fileinfo.fileindex;
                { be sure to change line !! }
                lastfileinfo.line:=-1;
              end;
           { write source }
             if (cs_asm_source in aktglobalswitches) and
                assigned(infile) then
              begin
                if (infile<>lastinfile) then
                  begin
                    AsmWriteLn(target_asm.comment+'['+infile^.name^+']');
                    if assigned(lastinfile) then
                      lastinfile^.close;
                  end;
                if (hp^.fileinfo.line<>lastfileinfo.line) and
                   ((hp^.fileinfo.line<infile^.maxlinebuf) or (InlineLevel>0)) then
                  begin
                    if (hp^.fileinfo.line<>0) and
                       ((infile^.linebuf^[hp^.fileinfo.line]>=0) or (InlineLevel>0)) then
                      AsmWriteLn(target_asm.comment+'['+tostr(hp^.fileinfo.line)+'] '+
                        fixline(infile^.GetLineStr(hp^.fileinfo.line)));
                    { set it to a negative value !
                    to make that is has been read already !! PM }
                    if (infile^.linebuf^[hp^.fileinfo.line]>=0) then
                      infile^.linebuf^[hp^.fileinfo.line]:=-infile^.linebuf^[hp^.fileinfo.line]-1;
                  end;
              end;
             lastfileinfo:=hp^.fileinfo;
             lastinfile:=infile;
           end;
         DoNotSplitLine:=false;
         case hp^.typ of
       ait_comment : Begin
                       AsmWrite(target_asm.comment);
                       AsmWritePChar(pai_asm_comment(hp)^.str);
                       AsmLn;
                     End;
           ait_regalloc :
             begin
               if (cs_asm_regalloc in aktglobalswitches) then
                 AsmWriteLn(target_asm.comment+'Register '+int_reg2str[pairegalloc(hp)^.reg]+
                   allocstr[pairegalloc(hp)^.allocation]);
             end;

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
                       tostr(paitempalloc(hp)^.tempsize)+allocstr[paitempalloc(hp)^.allocation]);
                 end;
             end;

       ait_section : begin
                       if LastSec<>sec_none then
                        AsmWriteLn('_'+target_asm.secnames[LastSec]+#9#9'ENDS');
                       if pai_section(hp)^.sec<>sec_none then
                        begin
                          AsmLn;
                          AsmWriteLn('_'+target_asm.secnames[pai_section(hp)^.sec]+#9#9+
                                     'SEGMENT'#9'PARA PUBLIC USE32 '''+
                                     target_asm.secnames[pai_section(hp)^.sec]+'''');
                        end;
                       LastSec:=pai_section(hp)^.sec;
                     end;
         ait_align : begin
                     { CAUSES PROBLEMS WITH THE SEGMENT DEFINITION   }
                     { SEGMENT DEFINITION SHOULD MATCH TYPE OF ALIGN }
                     { HERE UNDER TASM!                              }
                       AsmWriteLn(#9'ALIGN '+tostr(pai_align(hp)^.aligntype));
                     end;
     ait_datablock : begin
                       if pai_datablock(hp)^.is_global then
                         AsmWriteLn(#9'PUBLIC'#9+pai_datablock(hp)^.sym^.name);
                       AsmWriteLn(PadTabs(pai_datablock(hp)^.sym^.name,#0)+'DB'#9+tostr(pai_datablock(hp)^.size)+' DUP(?)');
                     end;
   ait_const_32bit,
    ait_const_8bit,
   ait_const_16bit : begin
                       AsmWrite(ait_const2str[hp^.typ]+tostr(pai_const(hp)^.value));
                       consttyp:=hp^.typ;
                       l:=0;
                       repeat
                         found:=(not (Pai(hp^.next)=nil)) and (Pai(hp^.next)^.typ=consttyp);
                         if found then
                          begin
                            hp:=Pai(hp^.next);
                            s:=','+tostr(pai_const(hp)^.value);
                            AsmWrite(s);
                            inc(l,length(s));
                          end;
                       until (not found) or (l>line_length);
                       AsmLn;
                     end;
  ait_const_symbol : begin
                       AsmWriteLn(#9#9'DD'#9'offset '+pai_const_symbol(hp)^.sym^.name);
                       if pai_const_symbol(hp)^.offset>0 then
                         AsmWrite('+'+tostr(pai_const_symbol(hp)^.offset))
                       else if pai_const_symbol(hp)^.offset<0 then
                         AsmWrite(tostr(pai_const_symbol(hp)^.offset));
                       AsmLn;
                     end;
     ait_const_rva : begin
                       AsmWriteLn(#9#9'RVA'#9+pai_const_symbol(hp)^.sym^.name);
                     end;
        ait_real_32bit : AsmWriteLn(#9#9'DD'#9+single2str(pai_real_32bit(hp)^.value));
        ait_real_64bit : AsmWriteLn(#9#9'DQ'#9+double2str(pai_real_64bit(hp)^.value));
      ait_real_80bit : AsmWriteLn(#9#9'DT'#9+extended2str(pai_real_80bit(hp)^.value));
          ait_comp_64bit : AsmWriteLn(#9#9'DQ'#9+comp2str(pai_real_80bit(hp)^.value));
        ait_string : begin
                       counter := 0;
                       lines := pai_string(hp)^.len div line_length;
                     { separate lines in different parts }
                       if pai_string(hp)^.len > 0 then
                        Begin
                          for j := 0 to lines-1 do
                           begin
                             AsmWrite(#9#9'DB'#9);
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
                               AsmWrite(target_os.newline);
                             counter := counter+line_length;
                          end; { end for j:=0 ... }
                        { do last line of lines }
                        AsmWrite(#9#9'DB'#9);
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
                        if quoted then
                          AsmWrite('"');
                        end;
                       AsmLn;
                     end;
         ait_label : begin
                       if pai_label(hp)^.l^.is_used then
                        begin
                          AsmWrite(pai_label(hp)^.l^.name);
                          if assigned(hp^.next) and not(pai(hp^.next)^.typ in
                             [ait_const_32bit,ait_const_16bit,ait_const_8bit,
                              ait_const_symbol,ait_const_rva,
                              ait_real_32bit,ait_real_64bit,ait_real_80bit,ait_comp_64bit,ait_string]) then
                           AsmWriteLn(':')
                          else
                           DoNotSplitLine:=true;
                        end;
                     end;
        ait_direct : begin
                       AsmWritePChar(pai_direct(hp)^.str);
                       AsmLn;
                     end;
        ait_symbol : begin
                       if pai_symbol(hp)^.is_global then
                         AsmWriteLn(#9'PUBLIC'#9+pai_symbol(hp)^.sym^.name);
                       AsmWrite(pai_symbol(hp)^.sym^.name);
                       if assigned(hp^.next) and not(pai(hp^.next)^.typ in
                          [ait_const_32bit,ait_const_16bit,ait_const_8bit,
                           ait_const_symbol,ait_const_rva,
                           ait_real_32bit,ait_real_64bit,ait_real_80bit,ait_comp_64bit,ait_string]) then
                        AsmWriteLn(':')
                     end;
    ait_symbol_end : begin
                     end;
   ait_instruction : begin
                     { Must be done with args in ATT order
                       paicpu(hp)^.SetOperandOrder(op_att);
                       now done by CheckNonCommutativeOpcodes PM }
                       paicpu(hp)^.CheckNonCommutativeOpcodes;
                     { We need intel order, no At&t }
                       paicpu(hp)^.SetOperandOrder(op_intel);
                     { Reset }
                       prefix:= '';
                       s:='';
                      { We need to explicitely set
                        word prefix to get selectors
                        to be pushed in 2 bytes  PM }
                      if (paicpu(hp)^.opsize=S_W) and
                         ((paicpu(hp)^.opcode=A_PUSH) or
                          (paicpu(hp)^.opcode=A_POP)) and
                          (paicpu(hp)^.oper[0].typ=top_reg) and
                          ((paicpu(hp)^.oper[0].reg>=firstsreg) and
                           (paicpu(hp)^.oper[0].reg<=lastsreg)) then
                        AsmWriteln(#9#9'DB'#9'066h');
                     { added prefix instructions, must be on same line as opcode }
                       if (paicpu(hp)^.ops = 0) and
                          ((paicpu(hp)^.opcode = A_REP) or
                           (paicpu(hp)^.opcode = A_LOCK) or
                           (paicpu(hp)^.opcode =  A_REPE) or
                           (paicpu(hp)^.opcode =  A_REPNZ) or
                           (paicpu(hp)^.opcode =  A_REPZ) or
                           (paicpu(hp)^.opcode = A_REPNE)) then
                        Begin
                          prefix:=int_op2str[paicpu(hp)^.opcode]+#9;
                          hp:=Pai(hp^.next);
                        { this is theorically impossible... }
                          if hp=nil then
                           begin
                             s:=#9#9+prefix;
                             AsmWriteLn(s);
                             break;
                           end;
                          { nasm prefers prefix on a line alone
                          AsmWriteln(#9#9+prefix); but not masm PM
                          prefix:=''; }
                          if (aktoutputformat = as_i386_masm) then
                             begin
                               AsmWriteln(s);
                               prefix:='';
                             end;
                        end
                       else
                        prefix:= '';
                       AsmWrite(#9#9+prefix+int_op2str[paicpu(hp)^.opcode]+cond2str[paicpu(hp)^.condition]);
                       if paicpu(hp)^.ops<>0 then
                        begin
                          if is_calljmp(paicpu(hp)^.opcode) then
                           begin
                             AsmWrite(#9);
                             WriteOpStrJmp(paicpu(hp)^.oper[0],paicpu(hp)^.opsize);
                           end
                          else
                           begin
                             for i:=0to paicpu(hp)^.ops-1 do
                              begin
                                if i=0 then
                                 sep:=#9
                                else
                                 sep:=',';
                                AsmWrite(sep);
                                WriteOpStr(paicpu(hp)^.oper[i],paicpu(hp)^.opsize,paicpu(hp)^.opcode,(i=2));
                              end;
                           end;
                        end;
                        AsmLn;
                     end;
{$ifdef GDB}
             ait_stabn,
             ait_stabs,
        ait_force_line,
ait_stab_function_name : ;
{$endif GDB}
           ait_cut : begin
                     { only reset buffer if nothing has changed }
                       if AsmSize=AsmStartSize then
                        AsmClear
                       else
                        begin
                          if LastSec<>sec_none then
                           AsmWriteLn('_'+target_asm.secnames[LastSec]+#9#9'ENDS');
                          AsmLn;
                          AsmWriteLn(#9'END');
                          AsmClose;
                          DoAssemble;
                          AsmCreate(pai_cut(hp)^.place);
                        end;
                     { avoid empty files }
                       while assigned(hp^.next) and (pai(hp^.next)^.typ in [ait_cut,ait_section,ait_comment]) do
                        begin
                          if pai(hp^.next)^.typ=ait_section then
                           begin
                             lastsec:=pai_section(hp^.next)^.sec;
                           end;
                          hp:=pai(hp^.next);
                        end;
                       AsmWriteLn(#9'.386p');
                       AsmWriteLn('DGROUP'#9'GROUP'#9'_BSS,_DATA');
                       AsmWriteLn(#9'ASSUME'#9'CS:_CODE,ES:DGROUP,DS:DGROUP,SS:DGROUP');
                       { I was told that this isn't necesarry because }
                       { the labels generated by FPC are unique (FK)  }
                       { AsmWriteLn(#9'LOCALS '+target_asm.labelprefix); }
                       if lastsec<>sec_none then
                          AsmWriteLn('_'+target_asm.secnames[lastsec]+#9#9+
                                     'SEGMENT'#9'PARA PUBLIC USE32 '''+
                                     target_asm.secnames[lastsec]+'''');
                       AsmStartSize:=AsmSize;
                     end;
           ait_marker :
             begin
               if pai_marker(hp)^.kind=InlineStart then
                 inc(InlineLevel)
               else if pai_marker(hp)^.kind=InlineEnd then
                 dec(InlineLevel);
             end;
         else
          internalerror(10000);
         end;
         hp:=pai(hp^.next);
       end;
    end;

    var
      currentasmlist : PAsmList;

    procedure writeexternal(p:pnamedindexobject);{$ifndef FPC}far;{$endif}
      begin
        if pasmsymbol(p)^.deftyp=AS_EXTERNAL then
          begin
            if (aktoutputformat = as_i386_masm) then
              currentasmlist^.AsmWriteln(#9'EXTRN'#9+p^.name
                +': NEAR')
            else
              currentasmlist^.AsmWriteln(#9'EXTRN'#9+p^.name);
          end;
      end;

    procedure ti386intasmlist.WriteExternals;
      begin
        currentasmlist:=@self;
        AsmSymbolList^.foreach({$ifndef TP}@{$endif}writeexternal);
      end;


    function ti386intasmlist.DoAssemble : boolean;
    var f : file;
    begin
      DoAssemble:=Inherited DoAssemble;
      { masm does not seem to recognize specific extensions and uses .obj allways PM }
      if (aktoutputformat = as_i386_masm) then
        begin
          if not(cs_asm_extern in aktglobalswitches) then
            begin
              if Not FileExists(objfile) and
                 FileExists(ForceExtension(objfile,'.obj')) then
                begin
                  Assign(F,ForceExtension(objfile,'.obj'));
                  Rename(F,objfile);
                end;
            end
          else
            AsmRes^.AddAsmCommand('mv',ForceExtension(objfile,'.obj')+' '+objfile,objfile);
        end;
    end;

    procedure ti386intasmlist.WriteAsmList;
    begin
{$ifdef EXTDEBUG}
      if assigned(current_module^.mainsource) then
       comment(v_info,'Start writing intel-styled assembler output for '+current_module^.mainsource^);
{$endif}
      LastSec:=sec_none;
      AsmWriteLn(#9'.386p');
      { masm 6.11 does not seem to like LOCALS PM }
      if (aktoutputformat = as_i386_tasm) then
        begin
          AsmWriteLn(#9'LOCALS '+target_asm.labelprefix);
        end;
      AsmWriteLn('DGROUP'#9'GROUP'#9'_BSS,_DATA');
      AsmWriteLn(#9'ASSUME'#9'CS:_CODE,ES:DGROUP,DS:DGROUP,SS:DGROUP');
      AsmLn;

      countlabelref:=false;

      WriteExternals;

    { INTEL ASM doesn't support stabs
      WriteTree(debuglist);}

      WriteTree(codesegment);
      WriteTree(datasegment);
      WriteTree(consts);
      WriteTree(rttilist);
      WriteTree(resourcestringlist);
      WriteTree(bsssegment);
      countlabelref:=true;

      AsmWriteLn(#9'END');
      AsmLn;

{$ifdef EXTDEBUG}
      if assigned(current_module^.mainsource) then
       comment(v_info,'Done writing intel-styled assembler output for '+current_module^.mainsource^);
{$endif EXTDEBUG}
   end;

end.
{
  $Log: ag386int.pas,v $
  Revision 1.6.2.10  2002/12/11 13:17:15  pierre
   * fixed tb241b problem by changing operand strings functions into methods

  Revision 1.6.2.9  2002/10/31 13:18:42  pierre
   * ensure all operand operations are done with correct operand order
   * hopefully complete fix for fsub problem...

  Revision 1.6.2.8  2002/10/29 15:55:27  pierre
   * cycle works again with -So -dTP options

  Revision 1.6.2.7  2002/10/22 10:13:50  pierre
   * adapt to changes in aasm and temp_gen units

  Revision 1.6.2.6  2001/07/23 06:58:56  pierre
   + AsmRes made pointer to support amiga target

  Revision 1.6.2.5  2001/06/15 12:14:05  pierre
   * cope with the fact that masm always writes objects with extension .obj

  Revision 1.6.2.4  2001/04/16 22:59:03  pierre
   * fix for bug 1472

  Revision 1.6.2.3  2001/03/19 12:30:10  pierre
   * avoid troubles with -al option for masm output

  Revision 1.6.2.2  2001/03/19 11:34:25  pierre
    * put prefix onto a separate line for masm
      (this is required for masm 6.11d at least)

  Revision 1.6.2.1  2001/03/04 02:22:19  carl
  - renamefest!

  Revision 1.1.2.3  2001/02/20 16:53:57  pierre
   + better masm support

  Revision 1.1.2.2  2001/01/13 20:23:16  peter
    * fixed operand order that got mixed up for external writers after
      my previous assembler block valid instruction check

  Revision 1.1.2.1  2000/08/20 17:37:11  peter
    * smartlinking fixed for linux

  Revision 1.1  2000/07/13 06:29:43  michael
  + Initial import

  Revision 1.62  2000/05/12 21:26:22  pierre
    * fix the FDIV FDIVR FSUB FSUBR and popping equivalent
      simply by swapping from reverse to normal and vice-versa
      when passing from one syntax to the other !

  Revision 1.61  2000/05/09 21:44:27  pierre
    * add .byte 066h to force correct pushw %es
    * handle push es as a pushl %es

  Revision 1.60  2000/04/06 07:05:57  pierre
   * handle offsetfixup

  Revision 1.59  2000/02/09 13:22:43  peter
    * log truncated

  Revision 1.58  2000/01/07 01:14:18  peter
    * updated copyright to 2000

  Revision 1.57  1999/12/19 17:36:25  florian
    * generation of LOCALS @@ removed

  Revision 1.56  1999/11/06 14:34:16  peter
    * truncated log to 20 revs

  Revision 1.55  1999/11/02 15:06:56  peter
    * import library fixes for win32
    * alignment works again

  Revision 1.54  1999/09/10 15:41:18  peter
    * added symbol_end

  Revision 1.53  1999/09/02 18:47:42  daniel
    * Could not compile with TP, some arrays moved to heap
    * NOAG386BIN default for TP
    * AG386* files were not compatible with TP, fixed.

  Revision 1.52  1999/08/25 11:59:36  jonas
    * changed pai386, paippc and paiapha (same for tai*) to paicpu (taicpu)

  Revision 1.51  1999/08/04 00:22:36  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.50  1999/07/22 09:37:31  florian
    + resourcestring implemented
    + start of longstring support

}
