{
    $Id: aggas.pas,v 1.1.2.14 2003/06/18 12:56:45 pierre Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    This unit implements an asmoutput class for MOTOROLA syntax with
    Motorola 68000 (for GAS v2.52 AND HIGER)

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
{ R- Necessary for the in [] }
{$ifdef TP}
  {$N+,E+,R-}
{$endif}
unit aggas;

    interface

    uses
      cobjects,
      cpubase,
      aasm,
      assemble;

    type
      pm68kgasasmlist=^tm68kgasasmlist;
      tm68kgasasmlist = object(tasmlist)
        procedure WriteTree(p:paasmoutput);virtual;
        procedure WriteAsmList;virtual;
        procedure WriteReferenceString(var ref : treference);
        procedure WriteOpStr(const o:toper);
        procedure WriteOpStrJmp(const o:toper;is_lea : boolean);
{$ifdef GDB}
        procedure WriteFileLineInfo(var fileinfo : tfileposinfo);
        procedure WriteFileEndInfo;
{$endif}
      end;

   implementation

   uses
{$ifdef Delphi}
      dmisc,
{$else Delphi}
      dos,
{$endif Delphi}
      strings,
      globtype,globals,systems,
      files,verbose,cpuasm
{$ifdef GDB}
      ,gdb
{$endif GDB}
      ;

    const
      line_length = 70;

    var
{$ifdef GDB}
      n_line       : byte;     { different types of source lines }
      linecount,
      includecount : longint;
      funcname     : pchar;
      stabslastfileinfo : tfileposinfo;
{$endif}
      lastsec      : tsection; { last section type written }
      lastfileinfo : tfileposinfo;
      infile,
      lastinfile   : pinputfile;
      symendcount  : longint;
    type
      t96bitarray = array[0..11] of byte;
      t64bitarray = array[0..7] of byte;
      t32bitarray = array[0..3] of byte;


    function single2str(d : single) : string;
      var
         hs : string;
      begin
         str(d,hs);
      { replace space with + }
         if hs[1]=' ' then
          hs[1]:='+';
         single2str:='0d'+hs
      end;

    function double2str(d : double) : string;
      var
         hs : string;
      begin
         str(d,hs);
      { replace space with + }
         if hs[1]=' ' then
          hs[1]:='+';
         double2str:='0d'+hs
      end;

    function extended2str(e : extended) : string;
      var
         hs : string;
      begin
         str(e,hs);
      { replace space with + }
         if hs[1]=' ' then
          hs[1]:='+';
         extended2str:='0d'+hs
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

  { convert floating point values }
  { to correct endian             }
  procedure swap64bitarray(var t: t64bitarray);
    var
     b: byte;
    begin
      b:= t[7];
      t[7] := t[0];
      t[0] := b;

      b := t[6];
      t[6] := t[1];
      t[1] := b;

      b:= t[5];
      t[5] := t[2];
      t[2] := b;

      b:= t[4];
      t[4] := t[3];
      t[3] := b;
   end;


   procedure swap32bitarray(var t: t32bitarray);
    var
     b: byte;
    begin
      b:= t[1];
      t[1]:= t[2];
      t[2]:= b;

      b:= t[0];
      t[0]:= t[3];
      t[3]:= b;
    end;

   (* procedure swap80bitarray(var t: t80bitarray);
    begin
     {!!!!!!!!!!!!}
    end; *)

(* TO SUPPORT SOONER OR LATER!!!, why? It's not used anymore on i386 target PM
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


{****************************************************************************
                             T68kGASASMOUTPUT
 ****************************************************************************}

    const
      ait_const2str:array[ait_const_32bit..ait_const_8bit] of string[8]=
        (#9'.long'#9,#9'.short'#9,#9'.byte'#9);
    const
      allocstr:array[alloc_type] of string[16] =
       (' allocated',' released',' to stack',' from stack');
      tempallocstr:array[boolean] of string[16] =
       (' released',' allocated');


    function ait_section2str(s:tsection):string;
    begin
       ait_section2str:=target_asm.secnames[s];
{$ifdef GDB}
       { this is needed for line info in data }
       funcname:=nil;
       case s of
         sec_code : n_line:=n_textline;
         sec_data : n_line:=n_dataline;
         sec_bss  : n_line:=n_bssline;
         else       n_line:=n_dataline;
      end;
{$endif GDB}
      LastSec:=s;
    end;


    procedure tm68kgasasmlist.WriteReferenceString(var ref : treference);
      var
         s,basestr,indexstr : string;

      begin
         s:='';
         if ref.is_immediate then
             s:='#'+tostr(ref.offset)
         else
           with ref do
             begin
                inc(offset,offsetfixup);
                offsetfixup:=0;
                basestr:=gas_reg2str[base];
                indexstr:=gas_reg2str[index];
                if assigned(symbol) then
                  begin
                    if length(symbol^.name)>200 then
                      begin
                        AsmWrite(s);
                        AsmWrite(symbol^.name);
                        s:='';
                      end
                    else
                      s:=s+symbol^.name;
                  end;
                if offset<0 then s:=s+tostr(offset)
                  else if (offset>0) then
                    begin
                       if (symbol=nil) then s:=tostr(offset)
                       else s:=s+'+'+tostr(offset);
                    end
                  else if (index=R_NO) and (base=R_NO) and not assigned(symbol) then
                    s:=s+'0';

                if assigned(symbol) AND (base=R_A5) and
                   (tf_static_a5_based in target_info.flags) then
                  s:=s+'@END';
               if (index<>R_NO) and (base=R_NO) and (direction=dir_none) then
                begin
                  if (scalefactor = 1) or (scalefactor = 0) then
                    s:=s+'(,'+indexstr+'.l)'
                  else
                    s:=s+'(,'+indexstr+'.l*'+tostr(scalefactor)+')'
                end
                else if (index=R_NO) and (base<>R_NO) and (direction=dir_inc) then
                begin
                  if (scalefactor = 1) or (scalefactor = 0) then
                      s:=s+'('+basestr+')+'
                  else
                   InternalError(10002);
                end
                else if (index=R_NO) and (base<>R_NO) and (direction=dir_dec) then
                begin
                  if (scalefactor = 1) or (scalefactor = 0) then
                      s:=s+'-('+basestr+')'
                  else
                   InternalError(10003);
                end
                  else if (index=R_NO) and (base<>R_NO) and (direction=dir_none) then
                begin
                  s:=s+'('+basestr+')'
                end
                  else if (index<>R_NO) and (base<>R_NO) and (direction=dir_none) then
                begin
                  if (scalefactor = 1) or (scalefactor = 0) then
                    s:=s+'('+basestr+','+indexstr+'.l)'
                  else
                    s:=s+'('+basestr+','+indexstr+'.l*'+tostr(scalefactor)+')';
                end;
            end; { end with }
         AsmWrite(s);
      end;


    procedure tm68kgasasmlist.WriteOpStr(const o:toper);
    var
      hs : string;
      i : tregister;
    begin
      case o.typ of
            top_reg :
              hs:=gas_reg2str[o.reg];
            top_ref :
              begin
                WriteReferenceString(o.ref^);
                exit;
              end;
        top_reglist :
              begin
                hs:='';
                for i:=R_NO to R_FPSR do
                begin
                  if i in o.registerlist^ then
                   hs:=hs+gas_reg2str[i]+'/';
                end;
                delete(hs,length(hs),1);
              end;
            top_const :
              hs:='#'+tostr(o.val);
            top_symbol :
              { compare with i386, where a symbol is considered }
              { a constant.                                     }
              begin
                if assigned(o.sym) then
                  begin
                    if length(o.sym^.name)>200 then
                      begin
                        AsmWrite('#');
                        AsmWrite(o.sym^.name);
                        hs:='';
                      end
                    else
                      hs:='#'+o.sym^.name;
                  end
                else
                  hs:='#';
                if o.symofs>0 then
                 hs:=hs+'+'+tostr(o.symofs)
                else if o.symofs<0 then
                  hs:=hs+tostr(o.symofs)
                else if not(assigned(o.sym)) then
                   hs:=hs+'0';
               end;
            else internalerror(10001);
         end;
         AsmWrite(hs);
      end;

    procedure tm68kgasasmlist.WriteOpStrJmp(const o:toper;is_lea : boolean);
    var
      hs : string;
    begin
      case o.typ of
            top_reg :
              hs:=gas_reg2str[o.reg];
            top_ref :
              begin
                WriteReferenceString(o.ref^);
                { all data refs are base on a5 register,
                  so only text refs are left alone...
                  but we need to get them pc relative for palmos PM }
                if (target_info.target=target_m68k_palmos) and is_lea and
                   assigned(o.ref^.symbol) AND (o.ref^.base=R_NO) and
                   (o.ref^.index=R_NO) then
                  hs:='(%pc)'
                else
                  hs:='';
              end;
            top_const :
              hs:=tostr(o.val);
            top_symbol :
              begin
                if assigned(o.sym) then
                  begin
                    if length(o.sym^.name)>200 then
                      begin
                        AsmWrite(o.sym^.name);
                        hs:='';
                      end
                    else
                      hs:=o.sym^.name;
                  end
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
                if (target_info.target=target_m68k_palmos) and
                   is_lea and assigned(o.sym) then
                  hs:=hs+'(%pc)';
              end;
            else internalerror(10001);
         end;
         AsmWrite(hs);
      end;

{$ifdef GDB}
    var
      curr_n    : byte;

      procedure tm68kgasasmlist.WriteFileLineInfo(var fileinfo : tfileposinfo);
        var
          curr_n : byte;
        begin
          if not ((cs_debuginfo in aktmoduleswitches) or
             (cs_gdb_lineinfo in aktglobalswitches)) then
           exit;
        { file changed ? (must be before line info) }
          if (fileinfo.fileindex<>0) and
             (stabslastfileinfo.fileindex<>fileinfo.fileindex) then
           begin
             infile:=current_module^.sourcefiles^.get_file(fileinfo.fileindex);
             if assigned(infile) then
              begin
                if includecount=0 then
                 curr_n:=n_sourcefile
                else
                 curr_n:=n_includefile;
                if (infile^.path^<>'') then
                 begin
                   AsmWriteLn(#9'.stabs "'+lower(BsToSlash(FixPath(infile^.path^,false)))+'",'+
                     tostr(curr_n)+',0,0,'+'Ltext'+ToStr(IncludeCount));
                 end;
                AsmWriteLn(#9'.stabs "'+lower(FixFileName(infile^.name^))+'",'+
                  tostr(curr_n)+',0,0,'+'Ltext'+ToStr(IncludeCount));
                AsmWriteLn('Ltext'+ToStr(IncludeCount)+':');
                inc(includecount);
              end;
           end;
        { line changed ? }
          if (stabslastfileinfo.line<>fileinfo.line) and (fileinfo.line<>0) then
           begin
             if (n_line=n_textline) then
              begin
                if assigned(funcname) and
                   target_os.use_function_relative_addresses then
                  begin
                    AsmWriteLn(target_asm.labelprefix+'l'+tostr(linecount)+':');
                    AsmWrite(#9'.stabn '+tostr(n_line)+',0,'+tostr(fileinfo.line)+','+
                               target_asm.labelprefix+'l'+tostr(linecount)+' - ');
                    AsmWritePChar(FuncName);
                    AsmLn;
                    inc(linecount);
                  end
                { don't set the line number outside functions, because this
                  leads to problem inmain functions PM }
                else if not target_os.use_function_relative_addresses then
                  AsmWriteLn(#9'.stabd'#9+tostr(n_line)+',0,'+tostr(fileinfo.line));
              end
             else
              AsmWriteLn(#9'.stabd'#9+tostr(n_line)+',0,'+tostr(fileinfo.line));
           end;
          stabslastfileinfo:=fileinfo;
        end;

      procedure tm68kgasasmlist.WriteFileEndInfo;

        begin
          if not ((cs_debuginfo in aktmoduleswitches) or
             (cs_gdb_lineinfo in aktglobalswitches)) then
           exit;
          AsmLn;
          AsmWriteLn(ait_section2str(sec_code));
          AsmWriteLn(#9'.stabs "",'+tostr(n_sourcefile)+',0,0,Letext');
          AsmWriteLn('Letext:');
        end;

{$endif GDB}




    procedure tm68kgasasmlist.WriteTree(p:paasmoutput);
    type
      twowords=record
        word1,word2:word;
      end;
      textendedarray = array[0..9] of byte; { last longint will be and $ffff }
    const
      nolinetai =[ait_label,
                  ait_regalloc,ait_tempalloc,
                  ait_stabn,ait_stabs,ait_section,
                  ait_cut,ait_marker,ait_align,ait_stab_function_name];
    var
      ch       : char;
      hp       : pai;
      consttyp : tait;
      s        : string;
      found    : boolean;
      i,pos,l  : longint;
      InlineLevel : longint;
      co       : comp;
      sin      : single;
      d        : double;
      e        : extended;
      op       : tasmop;
      calljmp,
      do_line  : boolean;
      sep      : char;
{      hp        : pai;
      ch        : char;
      consttyp  : tait;
      s         : string;
      pos,l,i   : longint;
      found     : boolean;
      InlineLevel : longint;
      do_line: boolean;}
    begin
      if not assigned(p) then
       exit;
      InlineLevel:=0;
      { lineinfo is only needed for codesegment (PFV) }
      do_line:=(cs_asm_source in aktglobalswitches) or
               ((cs_lineinfo in aktmoduleswitches)
                 and (p=codesegment));
      hp:=pai(p^.first);
      while assigned(hp) do
       begin
         aktfilepos:=hp^.fileinfo;

         if not(hp^.typ in nolinetai) then
          begin
{$ifdef GDB}
             { write stabs }
             if (cs_debuginfo in aktmoduleswitches) or
                (cs_gdb_lineinfo in aktglobalswitches) then
               WriteFileLineInfo(hp^.fileinfo);
{$endif GDB}

             if do_line then
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
{$ifdef LINEINFO}
              { lineinfo }
                if (cs_lineinfo in aktmoduleswitches) then
                 begin
                   if (infile<>lastinfile) then
                    begin
                      lineinfolist^.concat(new(pai_const(init_8bit
                    end
                   else
                    begin
                    end;
                 end;
{$endif LINEINFO}
                lastfileinfo:=hp^.fileinfo;
                lastinfile:=infile;
              end;
          end;

         case hp^.typ of
           ait_comment :
             Begin
               AsmWrite(target_asm.comment);
               AsmWritePChar(pai_asm_comment(hp)^.str);
               AsmLn;
             End;

           ait_regalloc :
             begin
               if (cs_asm_regalloc in aktglobalswitches) then
                 AsmWriteLn(target_asm.comment+'Register '+gas_reg2str[pairegalloc(hp)^.reg]+
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
                       tostr(paitempalloc(hp)^.tempsize)+tempallocstr[paitempalloc(hp)^.allocation]);
                 end;
             end;

           ait_align :
             begin
               AsmWrite(#9'.align '+tostr(pai_align(hp)^.aligntype));
               if pai_align(hp)^.use_op then
                AsmWrite(','+tostr(pai_align(hp)^.fillop));
               AsmLn;
             end;

           ait_section :
             begin
               if pai_section(hp)^.sec<>sec_none then
                begin
                  AsmLn;
                  AsmWriteLn(ait_section2str(pai_section(hp)^.sec));
{$ifdef GDB}
                  lastfileinfo.line:=-1;
{$endif GDB}
                end;
             end;

           ait_datablock :
             begin
               if pai_datablock(hp)^.is_global then
                AsmWrite(#9'.comm'#9)
               else
                AsmWrite(#9'.lcomm'#9);
               AsmWrite(pai_datablock(hp)^.sym^.name);
               AsmWriteLn(','+tostr(pai_datablock(hp)^.size));
             end;

           ait_const_32bit,
           ait_const_16bit,
           ait_const_8bit :
             begin
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

           ait_const_symbol :
             begin
               AsmWrite(#9'.long'#9);
               AsmWrite(pai_const_symbol(hp)^.sym^.name);
               if pai_const_symbol(hp)^.offset>0 then
                 AsmWrite('+'+tostr(pai_const_symbol(hp)^.offset))
               else if pai_const_symbol(hp)^.offset<0 then
                 AsmWrite(tostr(pai_const_symbol(hp)^.offset));
               AsmLn;
             end;

           ait_const_rva :
             begin
               AsmWrite(#9'.rva'#9);
               AsmWriteLn(pai_const_symbol(hp)^.sym^.name);
             end;

           ait_real_80bit :
             begin
               if do_line then
                AsmWriteLn(target_asm.comment+' 96 bit real const: '+extended2str(pai_real_80bit(hp)^.value));
               e:=pai_real_80bit(hp)^.value;
{$ifopt E-}
               if source_os.endian <> target_os.endian then
                 begin
                   AsmWrite(#9'.ldouble'#9);
                   AsmWrite(extended2str(e));
{$ifdef CPU68}
                 end
               else
                 begin
                   { This code only works natively PM }
                   AsmWrite(#9'.byte'#9);
                   for i:=0 to 11 do
                    begin
                      if i<>0 then
                       AsmWrite(',');
                      AsmWrite(tostr(t96bitarray(e)[i]));
                    end;
{$endif CPU68}
                 end;
{$else E-}
               AsmWrite(#9'.ldouble'#9);
               AsmWrite(extended2str(e));
{$endif E-}
               AsmLn;
             end;

           ait_real_64bit :
             begin
               if do_line then
                AsmWriteLn(target_asm.comment+' 64 bit real const: '+double2str(pai_real_64bit(hp)^.value));
{$ifopt E-}
               d:=pai_real_64bit(hp)^.value;
               AsmWrite(#9'.byte'#9);
               { swap the constants to correct endian first }
               if source_os.endian <> target_os.endian then
                  swap64bitarray(t64bitarray(d));
               for i:=0 to 7 do
                begin
                  if i<>0 then
                   AsmWrite(',');
                  AsmWrite(tostr(t64bitarray(d)[i]));
                end;
{$else}
               AsmWrite(#9'.double'#9);
               AsmWrite(double2str(pai_real_64bit(hp)^.value));
{$endif E-}
               AsmLn;
             end;

           ait_real_32bit :
             begin
               if do_line then
                AsmWriteLn(target_asm.comment+' 32 bit real const: '+single2str(pai_real_32bit(hp)^.value));
               sin:=pai_real_32bit(hp)^.value;
               AsmWrite(#9'.byte'#9);
               { swap the constants to correct endian first }
               if source_os.endian <> target_os.endian then
                  swap32bitarray(t32bitarray(sin));
               for i:=0 to 3 do
                begin
                  if i<>0 then
                   AsmWrite(',');
                  AsmWrite(tostr(t32bitarray(sin)[i]));
                end;
               AsmLn;
             end;

           ait_comp_64bit :
             begin
               if do_line then
                AsmWriteLn(target_asm.comment+' 64 bit comp const: '+extended2str(pai_comp_64bit(hp)^.value));
               AsmWrite(#9'.byte'#9);
               { The constant itself must be in the longint range }
               co:=trunc(pai_comp_64bit(hp)^.value+0.1);
               if source_os.endian <> target_os.endian then
                  swap64bitarray(t64bitarray(co));
               for i:=0 to 7 do
                begin
                  if i<>0 then
                   AsmWrite(',');
                  AsmWrite(tostr(t64bitarray(co)[i]));
                end;
               AsmLn;
               (* internalerror(10004); *)
             end;

           ait_direct :
             begin
               AsmWritePChar(pai_direct(hp)^.str);
               AsmLn;
{$IfDef GDB}
               if strpos(pai_direct(hp)^.str,'.data')<>nil then
                 n_line:=n_dataline
               else if strpos(pai_direct(hp)^.str,'.text')<>nil then
                 n_line:=n_textline
               else if strpos(pai_direct(hp)^.str,'.bss')<>nil then
                 n_line:=n_bssline;
{$endif GDB}
             end;

           ait_string :
             begin
               pos:=0;
               for i:=1 to pai_string(hp)^.len do
                begin
                  if pos=0 then
                   begin
                     AsmWrite(#9'.ascii'#9'"');
                     pos:=20;
                   end;
                  ch:=pai_string(hp)^.str[i-1];
                  case ch of
                     #0, {This can't be done by range, because a bug in FPC}
                #1..#31,
             #128..#255 : s:='\'+tostr(ord(ch) shr 6)+tostr((ord(ch) and 63) shr 3)+tostr(ord(ch) and 7);
                    '"' : s:='\"';
                    '\' : s:='\\';
                  else
                   s:=ch;
                  end;
                  AsmWrite(s);
                  inc(pos,length(s));
                  if (pos>line_length) or (i=pai_string(hp)^.len) then
                   begin
                     AsmWriteLn('"');
                     pos:=0;
                   end;
                end;
             end;

           ait_label :
             begin
               if (pai_label(hp)^.l^.is_used) then
                begin
                  if pai_label(hp)^.l^.deftyp=AS_GLOBAL then
                   begin
                     AsmWrite('.globl'#9);
                     AsmWriteLn(pai_label(hp)^.l^.name);
                   end;
                  AsmWrite(pai_label(hp)^.l^.name);
                  AsmWriteLn(':');
                end;
             end;

           ait_symbol :
             begin
               if pai_symbol(hp)^.is_global then
                begin
                  AsmWrite('.globl'#9);
                  AsmWriteLn(pai_symbol(hp)^.sym^.name);
                end;
               if (target_info.target=target_m68k_linux) or
	          (target_info.target=target_m68k_netbsd) then
                begin
                   AsmWrite(#9'.type'#9);
                   AsmWrite(pai_symbol(hp)^.sym^.name);
                   if assigned(pai(hp^.next)) and
                      (pai(hp^.next)^.typ in [ait_const_symbol,ait_const_rva,
                         ait_const_32bit,ait_const_16bit,ait_const_8bit,ait_datablock,
                         ait_real_32bit,ait_real_64bit,ait_real_80bit,ait_comp_64bit]) then
                    AsmWriteLn(',@object')
                   else
                    AsmWriteLn(',@function');
                   if pai_symbol(hp)^.sym^.size>0 then
                    begin
                      AsmWrite(#9'.size'#9);
                      AsmWrite(pai_symbol(hp)^.sym^.name);
                      AsmWrite(', ');
                      AsmWriteLn(tostr(pai_symbol(hp)^.sym^.size));
                    end;
                end;
               AsmWrite(pai_symbol(hp)^.sym^.name);
               AsmWriteLn(':');
             end;
 
           ait_symbol_end :
             begin
               if target_info.target=target_m68k_linux then
                begin
                  s:=target_asm.labelprefix+'e'+tostr(symendcount);
                  inc(symendcount);
                  AsmWriteLn(s+':');
                  AsmWrite(#9'.size'#9);
                  AsmWrite(pai_symbol(hp)^.sym^.name);
                  AsmWrite(', '+s+' - ');
                  AsmWriteLn(pai_symbol(hp)^.sym^.name);
                end;
             end;
ait_labeled_instruction : begin
                     { labeled operand }
                       if pai_labeled(hp)^.register = R_NO then
                         begin
                           if pai_labeled(hp)^.lab <> nil then
                             begin
                               AsmWrite(#9+mot_op2str[pai_labeled(hp)^.opcode]+#9);
                               AsmWriteLn(pai_labeled(hp)^.lab^.name);
                             end
                           else
                             begin
                               AsmWrite(#9+mot_op2str[pai_labeled(hp)^.opcode]+#9);
                               AsmWriteLn(pai_labeled(hp)^.sym^.name);
                             end;
                         end
                       else
                     { labeled operand with register }
                        begin
                           if pai_labeled(hp)^.lab <> nil then
                             begin
                               AsmWrite(#9+mot_op2str[pai_labeled(hp)^.opcode]+#9+
                                 gas_reg2str[pai_labeled(hp)^.register]+',');
                               AsmWriteLn(pai_labeled(hp)^.lab^.name);
                             end
                           else
                           { a symbol is the value }
                             begin
                               AsmWrite(#9+mot_op2str[pai_labeled(hp)^.opcode]+#9+
                                 gas_reg2str[pai_labeled(hp)^.register]+',');
                               AsmWriteLn(pai_labeled(hp)^.sym^.name);
                             end;
                        end;
                     end;
{        ait_symbol : begin
                       if pai_symbol(hp)^.is_global then
                        begin
                          AsmWrite('.globl ');
                          AsmWriteln(pai_symbol(hp)^.sym^.name);
                        end;
                       AsmWrite(pai_symbol(hp)^.sym^.name);
                       AsmWriteln(':');
                     end;}
   ait_instruction : begin
                       { old versions of GAS don't like PEA.L and LEA.L }
                       if (paicpu(hp)^.opcode in [
                            A_LEA,A_PEA,A_ABCD,A_BCHG,A_BCLR,A_BSET,A_BTST,
                            A_EXG,A_NBCD,A_SBCD,A_SWAP,A_TAS,A_SCC,A_SCS,
                            A_SEQ,A_SGE,A_SGT,A_SHI,A_SLE,A_SLS,A_SLT,A_SMI,
                            A_SNE,A_SPL,A_ST,A_SVC,A_SVS,A_SF]) then
                        s:=#9+mot_op2str[paicpu(hp)^.opcode]
                       else
                        s:=#9+mot_op2str[paicpu(hp)^.opcode]+mit_opsize2str[paicpu(hp)^.opsize];

                       if paicpu(hp)^.ops=0 then
                        Asmwrite(s)
                       else
                        begin
                          Asmwrite(s+#9);
                          { s:=''; s is not used after here }
                        { call and jmp need an extra handling                          }
                        { this code is only callded if jmp isn't a labeled instruction }
                          if paicpu(hp)^.opcode in [A_BSR,A_BRA,A_LEA,A_PEA,A_JSR,A_JMP] then
                            begin
                              WriteOpStrJmp(paicpu(hp)^.oper[0],paicpu(hp)^.opcode=A_LEA);
                            end
                          else
                            begin
                              WriteOpStr(paicpu(hp)^.oper[0]);
                            end;
                           if paicpu(hp)^.ops>1 then
                            begin
                              AsmWrite(',');
                              WriteOpStr(paicpu(hp)^.oper[1]);
                            { three operands }
                              if paicpu(hp)^.ops>2 then
                               begin
                                   if (paicpu(hp)^.opcode = A_DIVSL) or
                                      (paicpu(hp)^.opcode = A_DIVUL) or
                                      (paicpu(hp)^.opcode = A_MULU) or
                                      (paicpu(hp)^.opcode = A_MULS) or
                                      (paicpu(hp)^.opcode = A_DIVS) or
                                      (paicpu(hp)^.opcode = A_DIVU) then
                                    Asmwrite(':')
                                   else
                                    AsmWrite(',');
                                   WriteOpStr(paicpu(hp)^.oper[2]);
                               end;
                            end;
                        end;
                       AsmLn;
                     end;
{$ifdef GDB}
           ait_stabs :
             begin
               AsmWrite(#9'.stabs ');
               AsmWritePChar(pai_stabs(hp)^.str);
               AsmLn;
             end;

           ait_stabn :
             begin
               AsmWrite(#9'.stabn ');
               AsmWritePChar(pai_stabn(hp)^.str);
               AsmLn;
             end;

           ait_force_line :
             stabslastfileinfo.line:=0;

           ait_stab_function_name:
             funcname:=pai_stab_function_name(hp)^.str;
{$endif GDB}

           ait_cut :
             begin
               if SmartAsm then
                begin
                { only reset buffer if nothing has changed }
                  if AsmSize=AsmStartSize then
                   AsmClear
                  else
                   begin
                     AsmClose;
                     DoAssemble;
                     AsmCreate(pai_cut(hp)^.place);
                   end;
                { avoid empty files }
                  while assigned(hp^.next) and (pai(hp^.next)^.typ in [ait_cut,ait_section,ait_comment]) do
                   begin
                     if pai(hp^.next)^.typ=ait_section then
                       lastsec:=pai_section(hp^.next)^.sec;
                     hp:=pai(hp^.next);
                   end;
{$ifdef GDB}
                  { force write of filename }
                  FillChar(stabslastfileinfo,sizeof(stabslastfileinfo),0);
                  includecount:=0;
                  funcname:=nil;
                  WriteFileLineInfo(hp^.fileinfo);
{$endif GDB}
                  if lastsec<>sec_none then
                    AsmWriteLn(ait_section2str(lastsec));
                  AsmStartSize:=AsmSize;
                end;
             end;

           ait_marker :
             if pai_marker(hp)^.kind=InlineStart then
               inc(InlineLevel)
             else if pai_marker(hp)^.kind=InlineEnd then
               dec(InlineLevel);
         else
          internalerror(10000);
         end;
         hp:=pai(hp^.next);
       end;
    end;


    procedure tm68kgasasmlist.WriteAsmList;
    var
      p:dirstr;
      n:namestr;
      e:extstr;
{$ifdef GDB}
      fileinfo : tfileposinfo;
{$endif GDB}

    begin
{$ifdef EXTDEBUG}
      if assigned(current_module^.mainsource) then
       Comment(v_info,'Start writing gas-styled assembler output for '+current_module^.mainsource^);
{$endif}

      LastSec:=sec_none;
{$ifdef GDB}
      FillChar(stabslastfileinfo,sizeof(stabslastfileinfo),0);
{$endif GDB}
      FillChar(lastfileinfo,sizeof(lastfileinfo),0);
      LastInfile:=nil;

      if assigned(current_module^.mainsource) then
       fsplit(current_module^.mainsource^,p,n,e)
      else
       begin
         p:=inputdir;
         n:=inputfile;
         e:=inputextension;
       end;
    { to get symify to work }
      AsmWriteLn(#9'.file "'+FixFileName(n+e)+'"');

{$ifdef GDB}
      n_line:=n_bssline;
      funcname:=nil;
      linecount:=1;
      includecount:=0;
      fileinfo.fileindex:=1;
      fileinfo.line:=1;
      { Write main file }
      WriteFileLineInfo(fileinfo);
{$endif GDB}
      AsmStartSize:=AsmSize;
      symendcount:=0;

      countlabelref:=false;
      If (cs_debuginfo in aktmoduleswitches) then
        WriteTree(debuglist);
      WriteTree(codesegment);
      WriteTree(datasegment);
      WriteTree(consts);
      WriteTree(rttilist);
      Writetree(resourcestringlist);
      WriteTree(bsssegment);
      Writetree(importssection);
      { exports are written by DLLTOOL
        if we use it so don't insert it twice (PM) }
      if not UseDeffileForExport and assigned(exportssection) then
        Writetree(exportssection);
      Writetree(resourcesection);
      {$ifdef GDB}
      WriteFileEndInfo;
      {$ENDIF}
      countlabelref:=true;

      AsmLn;
{$ifdef EXTDEBUG}
      if assigned(current_module^.mainsource) then
       comment(v_info,'Done writing gas-styled assembler output for '+current_module^.mainsource^);
{$endif EXTDEBUG}
    end;

end.
{
  $Log: aggas.pas,v $
  Revision 1.1.2.14  2003/06/18 12:56:45  pierre
   + generate type and size info for linux and netbsd

  Revision 1.1.2.13  2003/02/12 11:34:02  pierre
   * try to avoid breakpoint at main to be past the last line

  Revision 1.1.2.12  2003/01/16 14:10:13  pierre
   * add (%pc) for lea on palmos tartget

  Revision 1.1.2.11  2003/01/11 18:31:38  carl
    * fix compilation problems under GDB

  Revision 1.1.2.10  2002/12/11 15:01:43  pierre
   * fix tb0241 problem by change operand string functions into methods

  Revision 1.1.2.9  2002/12/10 23:00:09  pierre
   * palmos specific change for pic code

  Revision 1.1.2.8  2002/12/02 16:24:08  pierre
   * avoid problem if label name is 255 chars long

  Revision 1.1.2.7  2002/10/22 18:49:49  carl
    - FPU_EMULATION removed (was very misleading)

  Revision 1.1.2.6  2002/10/22 10:13:51  pierre
   * adapt to changes in aasm and temp_gen units

  Revision 1.1.2.5  2002/09/29 14:01:42  carl
    * m68k_fpu_emulated -> fpu_emulation switch

  Revision 1.1.2.4  2002/09/16 21:06:59  pierre
   * fix typo bug

  Revision 1.1.2.3  2002/09/15 16:41:48  carl
    * alignment fixes for constant dat
    * tabs now correct for m68k
    * many cg fixes (don't remember all)

  Revision 1.1.2.2  2002/08/18 11:29:17  carl
    * readd in fixes branch

  Revision 1.1.2.23  2001/09/14 11:57:45  pierre
   * replaced ifdef CPUM68K by ifdef CPU68

  Revision 1.1.2.22  2001/09/14 03:00:46  carl
  - removed forced alignment of syms (handled by cg and symbol table now)

  Revision 1.1.2.21  2001/09/12 23:55:58  pierre
   * small typos

  Revision 1.1.2.20  2001/09/06 16:21:40  pierre
   * fixed for cross compilation, extended ins't always 96 bits long

  Revision 1.1.2.19  2001/09/06 15:51:41  pierre
   * remove unused swap80bitarray

  Revision 1.1.2.18  2001/09/06 15:36:55  pierre
   * better realconst support

  Revision 1.1.2.17  2001/09/04 15:50:37  pierre
   * use M68K_FPU_EMULATED to disable 64bit real const writing

  Revision 1.1.2.16  2001/08/08 12:45:10  pierre
   * BSR and BRA need the same handling as JSR

  Revision 1.1.2.15  2001/08/08 12:22:48  pierre
   * fix the constant labels and dupport tai_labeled with sym only

  Revision 1.1.2.14  2001/08/07 15:47:20  pierre
   * generate pea 0 correctly

  Revision 1.1.2.13  2001/07/25 07:16:07  pierre
   * remove comp code as it should never be used

  Revision 1.1.2.12  2001/07/19 16:43:32  pierre
   + improoved register allocation info

  Revision 1.1.2.11  2001/07/18 23:52:25  pierre
   * avoid misinterpretation of comments as line numbers

  Revision 1.1.2.10  2001/07/17 14:18:53  pierre
   * fix error in comp support

  Revision 1.1.2.9  2001/07/17 14:13:28  pierre
   * use section names

  Revision 1.1.2.8  2001/05/23 00:46:28  carl
  * Correct problem with endian and cross-compilation

  Revision 1.1.2.7  2001/05/17 01:29:28  carl
  * offsetfixup now used as it should be

  Revision 1.1.2.6  2001/05/15 21:49:05  carl
  + always add prefixes to registers now

  Revision 1.1.2.5  2001/05/09 03:41:57  carl
  + more beautiful output with tabs.

  Revision 1.1.2.4  2001/04/24 11:53:48  carl
  * correction of reading labeled instructions

  Revision 1.1.2.3  2001/04/23 01:10:36  carl
  + updated to conform to aggas of intel version (same support)

  Revision 1.1.2.2  2001/04/21 00:33:47  carl
  * m68k updates

  Revision 1.1.2.1  2001/02/26 03:03:53  carl
  + renamed from ag68kmot

  Revision 1.1.2.1  2001/02/25 01:39:37  carl
  - moved into m68k directory

  Revision 1.1.2.1  2001/02/23 10:05:13  pierre
   * first bunch of m68k cpu updates

  Revision 1.1  2000/07/13 06:29:43  michael
  + Initial import

  Revision 1.26  2000/04/14 12:49:11  pierre
   * some debug related updates

  Revision 1.25  2000/02/09 13:22:44  peter
    * log truncated

  Revision 1.24  2000/01/07 01:14:18  peter
    * updated copyright to 2000

  Revision 1.23  1999/09/16 23:05:51  florian
    * m68k compiler is again compilable (only gas writer, no assembler reader)

}
