{
    $Id: og386cff.pas,v 1.1.2.3 2002/12/17 12:40:50 pierre Exp $
    Copyright (c) 1998-2000 by Peter Vreman and Pierre Muller

    Contains the 386 binary coff writer

    * This code was inspired by the NASM sources
      The Netwide Assembler is copyright (C) 1996 Simon Tatham and
      Julian Hall. All rights reserved.

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
unit og386cff;

{
  Notes on COFF:

  (0) When I say `standard COFF' below, I mean `COFF as output and
  used by DJGPP'. I assume DJGPP gets it right.

  (1) Win32 appears to interpret the term `relative relocation'
  differently from standard COFF. Standard COFF understands a
  relative relocation to mean that during relocation you add the
  address of the symbol you're referencing, and subtract the base
  address of the section you're in. Win32 COFF, by contrast, seems
  to add the address of the symbol and then subtract the address
  of THE BYTE AFTER THE RELOCATED DWORD. Hence the two formats are
  subtly incompatible.

  (2) Win32 doesn't bother putting any flags in the header flags
  field (at offset 0x12 into the file).

  (3) Win32 uses some extra flags into the section header table:
  it defines flags 0x80000000 (writable), 0x40000000 (readable)
  and 0x20000000 (executable), and uses them in the expected
  combinations. It also defines 0x00100000 through 0x00700000 for
  section alignments of 1 through 64 bytes.

  (4) Both standard COFF and Win32 COFF seem to use the DWORD
  field directly after the section name in the section header
  table for something strange: they store what the address of the
  section start point _would_ be, if you laid all the sections end
  to end starting at zero. Dunno why. Microsoft's documentation
  lists this field as "Virtual Size of Section", which doesn't
  seem to fit at all. In fact, Win32 even includes non-linked
  sections such as .drectve in this calculation.

  (5) Standard COFF does something very strange to common
  variables: the relocation point for a common variable is as far
  _before_ the variable as its size stretches out _after_ it. So
  we must fix up common variable references. Win32 seems to be
  sensible on this one.
}
  interface

    uses
       cobjects,
       systems,cpubase,aasm,og386;

    type
       preloc = ^treloc;
       treloc = packed record
          next     : preloc;
          address  : longint;
          symbol   : pasmsymbol;
          section  : tsection; { only used if symbol=nil }
          relative : relative_type;
       end;

       psymbol = ^tsymbol;
       tsymbol = packed record
         name    : string[8];
         strpos  : longint;
         section : tsection;
         value   : longint;
         typ     : TAsmsymtype;
       end;

       pcoffsection = ^tcoffsection;
       tcoffsection = object
          index  : tsection;
          secidx : longint;
          data   : PDynamicArray;
          size,
          fillsize,
          mempos,
          len,
          datapos,
          relocpos,
          nrelocs,
          align,
          flags     : longint;
          relochead : PReloc;
          reloctail : ^PReloc;
          constructor init(sec:TSection;Aflags:longint);
          destructor  done;
          procedure  write(var d;l:longint);
          procedure  alloc(l:longint);
          procedure  addsymreloc(ofs:longint;p:pasmsymbol;relative:relative_type);
          procedure  addsectionreloc(ofs:longint;sec:tsection;relative:relative_type);
       end;

       pgenericcoffoutput = ^tgenericcoffoutput;
       tgenericcoffoutput = object(tobjectoutput)
         win32   : boolean;
         sects   : array[TSection] of PCoffSection;
         strs,
         syms    : Pdynamicarray;
         initsym : longint;
         constructor init(smart:boolean);
         destructor  done;virtual;
         procedure initwriting(Aplace:tcutplace);virtual;
         procedure donewriting;virtual;
         procedure setsectionsizes(var s:tsecsize);virtual;
         procedure writebytes(var data;len:longint);virtual;
         procedure writealloc(len:longint);virtual;
         procedure writealign(len:longint);virtual;
         procedure writereloc(data,len:longint;p:pasmsymbol;relative:relative_type);virtual;
         procedure writesymbol(p:pasmsymbol);virtual;
         procedure writestabs(section:tsection;offset:longint;p:pchar;nidx,nother,line:longint;reloc:boolean);virtual;
         procedure writesymstabs(section:tsection;offset:longint;p:pchar;ps:pasmsymbol;
           nidx,nother,line:longint;reloc:boolean);virtual;
         function  text_flags : longint;virtual;
         function  data_flags : longint;virtual;
         function  bss_flags : longint;virtual;
         function  idata_flags : longint;virtual;
         function  edata_flags : longint;virtual;
       private
         procedure createsection(sec:tsection);
         procedure write_relocs(s:pcoffsection);
         procedure write_symbol(const name:string;strpos,value,section,typ,aux:longint);
         procedure write_symbols;
         procedure writetodisk;
       end;

       pdjgppcoffoutput = ^tdjgppcoffoutput;
       tdjgppcoffoutput = object(tgenericcoffoutput)
         constructor init(smart:boolean);
         function text_flags : longint;virtual;
         function data_flags : longint;virtual;
         function bss_flags : longint;virtual;
       end;

       pwin32coffoutput = ^twin32coffoutput;
       twin32coffoutput = object(tgenericcoffoutput)
         constructor init(smart:boolean);
         function text_flags : longint;virtual;
         function data_flags : longint;virtual;
         function bss_flags : longint;virtual;
         function idata_flags : longint;virtual;
         function edata_flags : longint;virtual;
       end;

  implementation

      uses
        strings,verbose,
        globtype,globals,files;

    const
{$ifdef TP}
      symbolresize = 20*18;
      strsresize   = 256;
      DataResize   = 1024;
{$else}
      symbolresize = 200*18;
      strsresize   = 8192;
      DataResize   = 8192;
{$endif}


      type
      { Structures which are written directly to the output file }
        coffheader=packed record
          mach   : word;
          nsects : word;
          time   : longint;
          sympos : longint;
          syms   : longint;
          opthdr : word;
          flag   : word;
        end;
        coffsechdr=packed record
          name     : array[0..7] of char;
          vsize    : longint;
          rvaofs   : longint;
          datalen  : longint;
          datapos  : longint;
          relocpos : longint;
          lineno1  : longint;
          nrelocs  : word;
          lineno2  : word;
          flags    : longint;
        end;
        coffsectionrec=packed record
          len     : longint;
          nrelocs : word;
          empty   : array[0..11] of char;
        end;
        coffreloc=packed record
          address  : longint;
          sym      : longint;
          relative : word;
        end;
        coffsymbol=packed record
          name    : array[0..3] of char; { real is [0..7], which overlaps the strpos ! }
          strpos  : longint;
          value   : longint;
          section : integer;
          empty   : integer;
          typ     : byte;
          aux     : byte;
        end;
        pcoffstab=^coffstab;
        coffstab=packed record
          strpos  : longint;
          ntype   : byte;
          nother  : byte;
          ndesc   : word;
          nvalue  : longint;
        end;


{****************************************************************************
                               TSection
****************************************************************************}

    constructor tcoffsection.init(sec:TSection;Aflags:longint);
      begin
        index:=sec;
        secidx:=0;
        flags:=AFlags;
        { alignment after section }
        case sec of
          sec_code,
          sec_data,
          sec_bss :
            align:=16;
          sec_stab:
            align:=4;
          else
            align:=1;
        end;
        { filled after pass 1 }
        size:=0;
        fillsize:=0;
        mempos:=0;
        { pass 2 data }
        relocHead:=nil;
        relocTail:=@relocHead;
        Len:=0;
        NRelocs:=0;
        if sec=sec_bss then
         data:=nil
        else
         new(Data,Init(DataResize));
      end;


    destructor tcoffsection.done;
      begin
        if assigned(Data) then
          dispose(Data,done);
      end;


    procedure  tcoffsection.write(var d;l:longint);
      begin
        if not assigned(Data) then
         Internalerror(3334441);
        Data^.write(d,l);
        inc(len,l);
      end;


    procedure  tcoffsection.alloc(l:longint);
      begin
        if assigned(Data) then
         Internalerror(3334442);
        inc(len,l);
      end;


    procedure tcoffsection.addsymreloc(ofs:longint;p:pasmsymbol;relative:relative_type);
      var
        r : PReloc;
      begin
        new(r);
        reloctail^:=r;
        reloctail:=@r^.next;
        r^.next:=nil;
        r^.address:=ofs+mempos;
        r^.symbol:=p;
        r^.section:=sec_none;
        r^.relative:=relative;
        inc(nrelocs);
      end;


    procedure tcoffsection.addsectionreloc(ofs:longint;sec:tsection;relative:relative_type);
      var
        r : PReloc;
      begin
        new(r);
        reloctail^:=r;
        reloctail:=@r^.next;
        r^.next:=nil;
        r^.address:=ofs+mempos;
        r^.symbol:=nil;
        r^.section:=sec;
        r^.relative:=relative;
        inc(nrelocs);
      end;


{****************************************************************************
                            Genericcoffoutput
****************************************************************************}

    constructor tgenericcoffoutput.init(smart:boolean);
      begin
        inherited init(smart);
      end;


    destructor tgenericcoffoutput.done;
      begin
        inherited done;
      end;


    procedure tgenericcoffoutput.initwriting(Aplace:tcutplace);
      var
        s : string;
      begin
        inherited initwriting(Aplace);
        { reset }
        initsym:=0;
        new(syms,init(symbolresize));
        new(strs,init(strsresize));
        FillChar(Sects,sizeof(Sects),0);
        { we need at least the following 3 sections }
        createsection(sec_code);
        createsection(sec_data);
        createsection(sec_bss);
        if (cs_gdb_lineinfo in aktglobalswitches) or
           (cs_debuginfo in aktmoduleswitches) then
         begin
           createsection(sec_stab);
           createsection(sec_stabstr);
           writestabs(sec_none,0,nil,0,0,0,false);
           { write zero pchar and name together (PM) }
           s:=#0+SplitFileName(current_module^.mainsource^)+#0;
           sects[sec_stabstr]^.write(s[1],length(s));
         end;
      end;


    procedure tgenericcoffoutput.donewriting;
      var
        sec : tsection;
      begin
        { Only write the .o if there are no errors }
        if errorcount=0 then
          writetodisk;
        dispose(syms,done);
        dispose(strs,done);
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) then
          dispose(sects[sec],done);
        inherited donewriting;
      end;


    function tgenericcoffoutput.text_flags : longint;
      begin
        text_flags:=0;
      end;

    function tgenericcoffoutput.data_flags : longint;
      begin
        data_flags:=0;
      end;

    function tgenericcoffoutput.bss_flags : longint;
      begin
        bss_flags:=0;
      end;

    function tgenericcoffoutput.edata_flags : longint;
      begin
        edata_flags:=0;
      end;

    function tgenericcoffoutput.idata_flags : longint;
      begin
        idata_flags:=0;
      end;


    procedure tgenericcoffoutput.createsection(sec:TSection);
      var
        Aflags : longint;
      begin
        Aflags:=0;
        case sec of
          sec_code :
            Aflags:=text_flags;
          sec_data :
            Aflags:=data_flags;
          sec_bss :
            Aflags:=bss_flags;
          sec_idata2,
          sec_idata4,
          sec_idata5,
          sec_idata6,
          sec_idata7 :
            Aflags:=idata_flags;
          sec_edata :
            Aflags:=edata_flags;
          else
            Aflags:=0;
        end;
        sects[sec]:=new(PcoffSection,init(Sec,Aflags));
      end;


    procedure tgenericcoffoutput.writesymbol(p:pasmsymbol);
      var
        pos : longint;
        sym : tsymbol;
        s   : string;
      begin
        { already written ? }
        if p^.idx<>-1 then
         exit;
        { be sure that the section will exists }
        if (p^.section<>sec_none) and not(assigned(sects[p^.section])) then
          createsection(p^.section);
        { symbolname }
        pos:=strs^.size+4;
        s:=p^.name;
        if length(s)>8 then
         begin
           if length(s)<255 then
             s:=s+#0;
           strs^.write(s[1],length(s));
           { if the length is 255 we need to addd the terminal #0
             separately bug report from Florian 20/6/2000 }
           if length(s)=255 then
             begin
               s:=#0;
               strs^.write(s[1],length(s));
             end;
         end
        else
         pos:=-1;
        FillChar(sym,sizeof(sym),0);
        sym.strpos:=pos;
        if pos=-1 then
         sym.name:=s;
        sym.value:=p^.size;
        sym.typ:=p^.typ;
        { if local of global then set the section value to the address
          of the symbol }
        if p^.typ in [AS_LOCAL,AS_GLOBAL] then
         begin
           sym.section:=p^.section;
           sym.value:=p^.address+sects[p^.section]^.mempos;
         end;
        { update the asmsymbol index }
        p^.idx:=syms^.size div sizeof(TSymbol);
        { store the symbol, but not the local ones (PM) }
        if (p^.typ<>AS_LOCAL) or ((copy(s,1,2)<>'.L') and
          ((copy(s,1,1)<>'L') or not win32)) then
          syms^.write(sym,sizeof(tsymbol));
        { make the exported syms known to the objectwriter
          (needed for .a generation) }
        if (p^.typ=AS_GLOBAL) or
           ((p^.typ=AS_EXTERNAL) and (sym.value=p^.size) and (sym.value>0)) then
          writer^.writesym(p^.name);
      end;


    procedure tgenericcoffoutput.writebytes(var data;len:longint);
      begin
        if not assigned(sects[currsec]) then
         createsection(currsec);
        sects[currsec]^.write(data,len);
      end;


    procedure tgenericcoffoutput.writealloc(len:longint);
      begin
        if not assigned(sects[currsec]) then
         createsection(currsec);
        sects[currsec]^.alloc(len);
      end;


    procedure tgenericcoffoutput.writealign(len:longint);
      var
        modulo : longint;
      begin
        if not assigned(sects[currsec]) then
         createsection(currsec);
        modulo:=sects[currsec]^.len mod len;
        if modulo > 0 then
          sects[currsec]^.alloc(len-modulo);
      end;


    procedure tgenericcoffoutput.writereloc(data,len:longint;p:pasmsymbol;relative:relative_type);
      var
        symaddr : longint;
      begin
        if not assigned(sects[currsec]) then
         createsection(currsec);
        if assigned(p) then
         begin
           { real address of the symbol }
           symaddr:=p^.address;
           if p^.section<>sec_none then
            inc(symaddr,sects[p^.section]^.mempos);
           { no symbol relocation need inside a section }
           if p^.section=currsec then
             begin
               case relative of
                 relative_false :
                   begin
                     sects[currsec]^.addsectionreloc(sects[currsec]^.len,currsec,relative_false);
                     inc(data,symaddr);
                   end;
                 relative_true :
                   begin
                     inc(data,symaddr-len-sects[currsec]^.len);
                   end;
                 relative_rva :
                   begin
                     { don't know if this can happens !! }
                     { does this work ?? }
                     sects[currsec]^.addsectionreloc(sects[currsec]^.len,currsec,relative_rva);
                     inc(data,symaddr);
                   end;
               end;
             end
           else
             begin
               writesymbol(p);
               if (p^.section<>sec_none) and (relative<>relative_true) then
                 sects[currsec]^.addsectionreloc(sects[currsec]^.len,p^.section,relative)
               else
                 sects[currsec]^.addsymreloc(sects[currsec]^.len,p,relative);
               if not win32 then {seems wrong to me (PM) }
                inc(data,symaddr)
               else
                if (relative<>relative_true) and (p^.section<>sec_none) then
                 inc(data,symaddr);
               if relative=relative_true then
                begin
                  if win32 then
                    dec(data,len-4)
                  else
                    dec(data,len+sects[currsec]^.len);
                end;
            end;
         end;
        sects[currsec]^.write(data,len);
      end;


    procedure tgenericcoffoutput.writestabs(section:tsection;offset:longint;p:pchar;nidx,nother,line:longint;reloc : boolean);
      var
        stab : coffstab;
        s : tsection;
      begin
        { This is wrong because
          sec_none is used only for external bss
        if section=sec_none then
         s:=currsec
        else }
         s:=section;
        { local var can be at offset -1 !! PM }
        if reloc then
         begin
           if (offset=-1) then
            begin
              if s=sec_none then
               offset:=0
              else
               offset:=sects[s]^.len;
            end;
           if (s<>sec_none) then
            inc(offset,sects[s]^.mempos);
         end;
        if assigned(p) and (p[0]<>#0) then
         begin
           stab.strpos:=sects[sec_stabstr]^.len;
           sects[sec_stabstr]^.write(p^,strlen(p)+1);
         end
        else
         stab.strpos:=0;
        stab.ntype:=nidx;
        stab.ndesc:=line;
        stab.nother:=nother;
        stab.nvalue:=offset;
        sects[sec_stab]^.write(stab,sizeof(stab));
        { when the offset is not 0 then write a relocation, take also the
          hdrstab into account with the offset }
        if reloc then
          if DLLSource and RelocSection then
          { avoid relocation in the .stab section
            because it ends up in the .reloc section instead }
            sects[sec_stab]^.addsectionreloc(sects[sec_stab]^.len-4,s,relative_rva)
          else
            sects[sec_stab]^.addsectionreloc(sects[sec_stab]^.len-4,s,relative_false);
      end;


    procedure tgenericcoffoutput.writesymstabs(section:tsection;offset:longint;p:pchar;ps:pasmsymbol;
                                                 nidx,nother,line:longint;reloc:boolean);
      var
        stab : coffstab;
        s : tsection;
      begin
        { This is wrong because
          sec_none is used only for external bss
        if section=sec_none then
         s:=currsec
        else }
         s:=section;
        { do not use the size stored in offset field
         this is DJGPP specific ! PM }
        if win32 then
          offset:=0;
        { local var can be at offset -1 !! PM }
        if reloc then
         begin
           if (offset=-1) then
            begin
              if s=sec_none then
               offset:=0
              else
               offset:=sects[s]^.len;
            end;
           if (s<>sec_none) then
            inc(offset,sects[s]^.mempos);
         end;
        if assigned(p) and (p[0]<>#0) then
         begin
           stab.strpos:=sects[sec_stabstr]^.len;
           sects[sec_stabstr]^.write(p^,strlen(p)+1);
         end
        else
         stab.strpos:=0;
        stab.ntype:=nidx;
        stab.ndesc:=line;
        stab.nother:=nother;
        stab.nvalue:=offset;
        sects[sec_stab]^.write(stab,sizeof(stab));
        { when the offset is not 0 then write a relocation, take also the
          hdrstab into account with the offset }
        if reloc then
          if DLLSource and RelocSection then
          { avoid relocation in the .stab section
            because it ends up in the .reloc section instead }
            sects[sec_stab]^.addsymreloc(sects[sec_stab]^.len-4,ps,relative_rva)
          else
            sects[sec_stab]^.addsymreloc(sects[sec_stab]^.len-4,ps,relative_false);
      end;


    procedure tgenericcoffoutput.write_relocs(s:pcoffsection);
      var
        rel  : coffreloc;
        hr,r : preloc;
      begin
        r:=s^.relochead;
        while assigned(r) do
         begin
           rel.address:=r^.address;
           if assigned(r^.symbol) then
            begin
              if (r^.symbol^.typ=AS_LOCAL) then
               rel.sym:=2*sects[r^.symbol^.section]^.secidx
              else
               begin
                 if r^.symbol^.idx=-1 then
                   internalerror(4321);
                 rel.sym:=r^.symbol^.idx+initsym;
               end;
            end
           else if r^.section<>sec_none then
            rel.sym:=2*sects[r^.section]^.secidx
           else
            rel.sym:=0;
           case r^.relative of
             relative_true  : rel.relative:=$14;
             relative_false : rel.relative:=$6;
             relative_rva   : rel.relative:=$7;
           end;
           writer^.write(rel,sizeof(rel));
           { goto next and dispose this reloc }
           hr:=r;
           r:=r^.next;
           dispose(hr);
         end;
      end;


    procedure tgenericcoffoutput.write_symbol(const name:string;strpos,value,section,typ,aux:longint);
      var
        sym : coffsymbol;
      begin
        FillChar(sym,sizeof(sym),0);
        if strpos=-1 then
         move(name[1],sym.name,length(name))
        else
         sym.strpos:=strpos;
        sym.value:=value;
        sym.section:=section;
        sym.typ:=typ;
        sym.aux:=aux;
        writer^.write(sym,sizeof(sym));
      end;


    procedure tgenericcoffoutput.write_symbols;
      var
        filename : string[18];
        sec : tsection;
        sectionval,
        i   : longint;
        globalval : byte;
        secrec : coffsectionrec;
        sym : tsymbol;
      begin
        { The `.file' record, and the file name auxiliary record. }
        write_symbol ('.file', -1, 0, -2, $67, 1);
        fillchar(filename,sizeof(filename),0);
        filename:=SplitFileName(current_module^.mainsource^);
        writer^.write(filename[1],sizeof(filename)-1);
        { The section records, with their auxiliaries, also store the
          symbol index }
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) then
          begin
            write_symbol(target_asm.secnames[sec],-1,sects[sec]^.mempos,sects[sec]^.secidx,3,1);
            fillchar(secrec,sizeof(secrec),0);
            secrec.len:=sects[sec]^.len;
            if sects[sec]^.nrelocs<=$ffff then
              secrec.nrelocs:=sects[sec]^.nrelocs
            else
              begin
                { coff section relocation number is limited to a word
                  if we have more, we will produce wrong executables...
                  so its better to emit a fatal error here PM }
                Message(asmw_f_coff_relocation_overflow);
              end;
            writer^.write(secrec,sizeof(secrec));
          end;
        { The real symbols. }
        syms^.seek(0);
        for i:=1 to syms^.size div sizeof(TSymbol) do
         begin
           syms^.read(sym,sizeof(TSymbol));
           if sym.typ=AS_LOCAL then
             globalval:=3
           else
             globalval:=2;
           if assigned(sects[sym.section]) then
             sectionval:=sects[sym.section]^.secidx
           else
             sectionval:=0;
           write_symbol(sym.name,sym.strpos,sym.value,sectionval,globalval,0);
         end;
      end;


    procedure tgenericcoffoutput.setsectionsizes(var s:tsecsize);
      var
        align,
        mempos : longint;
        sec : tsection;
      begin
        { multiply stab with real size }
        s[sec_stab]:=s[sec_stab]*sizeof(coffstab);
        { if debug then also count header stab }
        if (cs_gdb_lineinfo in aktglobalswitches) or
           (cs_debuginfo in aktmoduleswitches) then
         begin
           inc(s[sec_stab],sizeof(coffstab));
           inc(s[sec_stabstr],length(SplitFileName(current_module^.mainsource^))+2);
         end;
        { fix all section }
        mempos:=0;
        for sec:=low(tsection) to high(tsection) do
         begin
           if (s[sec]>0) and (not assigned(sects[sec])) then
             createsection(sec);
           if assigned(sects[sec]) then
            begin
              sects[sec]^.size:=s[sec];
              sects[sec]^.mempos:=mempos;
              { calculate the alignment }
              align:=sects[sec]^.align;
              sects[sec]^.fillsize:=align-(sects[sec]^.size and (align-1));
              if sects[sec]^.fillsize=align then
               sects[sec]^.fillsize:=0;
              { next section position, not for win32 which uses
                relative addresses }
              if not win32 then
                inc(mempos,sects[sec]^.size+sects[sec]^.fillsize);
            end;
         end;
      end;


    procedure tgenericcoffoutput.writetodisk;
      var
        datapos,secidx,
        nsects,sympos,i : longint;
        hstab  : coffstab;
        gotreloc : boolean;
        sec    : tsection;
        header : coffheader;
        sechdr : coffsechdr;
        empty  : array[0..15] of byte;
        hp     : pdynamicblock;
      begin
      { calc amount of sections we have and align sections at 4 bytes }
        fillchar(empty,sizeof(empty),0);
        nsects:=0;
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) then
          begin
{$ifdef EXTDEBUG}
          { check if the section is still the same size }
            if (sects[sec]^.len<>sects[sec]^.size) then
              Comment(V_Warning,'Size of section changed '+
                tostr(sects[sec]^.size)+'->'+tostr(sects[sec]^.len)+
                ' ['+target_asm.secnames[sec]+']');
{$endif EXTDEBUG}
          { fill with zero }
            if sects[sec]^.fillsize>0 then
             begin
               if assigned(sects[sec]^.data) then
                 sects[sec]^.write(empty,sects[sec]^.fillsize)
               else
                 sects[sec]^.alloc(sects[sec]^.fillsize);
             end;
            inc(nsects);
          end;
      { Calculate the filepositions }
        datapos:=sizeof(coffheader)+sizeof(coffsechdr)*nsects;
        initsym:=2; { 2 for the file }
        { sections first }
        secidx:=0;
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) then
          begin
            inc(secidx);
            sects[sec]^.secidx:=secidx;
            sects[sec]^.datapos:=datapos;
            if assigned(sects[sec]^.data) then
              inc(datapos,sects[sec]^.len);
            inc(initsym,2); { 2 for each section }
          end;
        { relocs }
        gotreloc:=false;
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) then
          begin
            sects[sec]^.relocpos:=datapos;
            inc(datapos,10*sects[sec]^.nrelocs);
            if (not gotreloc) and (sects[sec]^.nrelocs>0) then
             gotreloc:=true;
          end;
        { symbols }
        sympos:=datapos;
      { COFF header }
        fillchar(header,sizeof(coffheader),0);
        header.mach:=$14c;
        header.nsects:=nsects;
        header.sympos:=sympos;
        header.syms:=(syms^.size div sizeof(TSymbol))+initsym;
        if gotreloc then
         header.flag:=$104
        else
         header.flag:=$105;
        writer^.write(header,sizeof(header));
      { Section headers }
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) then
          begin
            fillchar(sechdr,sizeof(sechdr),0);
            move(target_asm.secnames[sec][1],sechdr.name,length(target_asm.secnames[sec]));
            if not win32 then
              begin
                sechdr.rvaofs:=sects[sec]^.mempos;
                sechdr.vsize:=sects[sec]^.mempos;
              end
            else
              begin
                if sec=sec_bss then
                  sechdr.vsize:=sects[sec]^.len;
              end;
            sechdr.datalen:=sects[sec]^.len;
            if (sects[sec]^.len>0) and assigned(sects[sec]^.data) then
              sechdr.datapos:=sects[sec]^.datapos;
            sechdr.relocpos:=sects[sec]^.relocpos;
            if sects[sec]^.nrelocs<=$ffff then
              sechdr.nrelocs:=sects[sec]^.nrelocs
            else
              begin
                { coff section relocation number is limited to a word
                  if we have more, we will produce wrong executables...
                  so its better to emit a fatal error here PM }
                Message(asmw_f_coff_relocation_overflow);
              end;
            sechdr.flags:=sects[sec]^.flags;
            writer^.write(sechdr,sizeof(sechdr));
          end;
      { Sections }
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) and
            assigned(sects[sec]^.data) then
          begin
            { For the stab section we need an HdrSym which can now be
              calculated more easily }
            if sec=sec_stab then
             begin
               hstab.ntype:=0{HdrSym};
               hstab.nother:=0;
               hstab.nvalue:=sects[sec_stabstr]^.len;
               hstab.strpos:=1;
               hstab.ndesc:=(sects[sec_stab]^.len div sizeof(coffstab))-1{+1 according to gas output PM};
               sects[sec_stab]^.data^.seek(0);
               sects[sec_stab]^.data^.write(hstab,sizeof(hstab));
             end;
            hp:=sects[sec]^.data^.firstblock;
            while assigned(hp) do
             begin
               writer^.write(hp^.data,hp^.used);
               hp:=hp^.next;
             end;
          end;
      { Relocs }
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) then
          write_relocs(sects[sec]);
      { Symbols }
        write_symbols;
      { Strings }
        i:=strs^.size+4;
        writer^.write(i,4);
        hp:=strs^.firstblock;
        while assigned(hp) do
         begin
           writer^.write(hp^.data,hp^.used);
           hp:=hp^.next;
         end;
      end;


{****************************************************************************
                            DJGppcoffoutput
****************************************************************************}

    constructor tdjgppcoffoutput.init(smart:boolean);
      begin
        inherited init(smart);
        win32:=false;
      end;

    function tdjgppcoffoutput.text_flags : longint;
      begin
        text_flags:=$20;
      end;

    function tdjgppcoffoutput.data_flags : longint;
      begin
        data_flags:=$40;
      end;

    function tdjgppcoffoutput.bss_flags : longint;
      begin
        bss_flags:=$80;
      end;


{****************************************************************************
                            Win32coffoutput
****************************************************************************}

    constructor twin32coffoutput.init(smart:boolean);
      begin
        inherited init(smart);
        win32:=true;
      end;

    function twin32coffoutput.text_flags : longint;
      begin
        text_flags:=$60000020; { same as as 2.9.1 }
      end;

    function twin32coffoutput.data_flags : longint;
      begin
        data_flags:=$c0300040;
      end;

    function twin32coffoutput.bss_flags : longint;
      begin
        bss_flags:=$c0300080;
      end;

    function twin32coffoutput.edata_flags : longint;
      begin
        edata_flags:=$c0300040;
      end;

    function twin32coffoutput.idata_flags : longint;
      begin
        idata_flags:=$40000000;
      end;


end.
{
  $Log: og386cff.pas,v $
  Revision 1.1.2.3  2002/12/17 12:40:50  pierre
   * complain if relocation overflow

  Revision 1.1.2.2  2001/09/06 15:32:25  pierre
   * use the same alignments as DJGPP as does, avoids some random filling on link time

  Revision 1.1.2.1  2001/03/04 02:22:20  carl
  - renamefest!

  Revision 1.1.2.2  2000/10/18 14:52:21  pierre
   * avoid uninitialized fields in coffstab record

  Revision 1.1.2.1  2000/08/19 18:42:27  peter
    * new tdynamicarray implementation using blocks instead of
      reallocmem

  Revision 1.1  2000/07/13 06:29:52  michael
  + Initial import

  Revision 1.24  2000/06/21 20:56:37  pierre
   * fix the problem of long mangledname in internal writer

  Revision 1.23  2000/04/12 12:42:29  pierre
   * fix the -g-l option

  Revision 1.22  2000/03/10 16:05:28  pierre
   * check that symbol is in object

  Revision 1.21  2000/03/10 09:15:54  pierre
   * rva relocation bug fixed

  Revision 1.20  2000/03/09 14:29:47  pierre
   * fix for the stab section size changes with smartlinking

  Revision 1.19  2000/02/09 13:22:54  peter
    * log truncated

  Revision 1.18  2000/01/12 10:38:18  peter
    * smartlinking fixes for binary writer
    * release alignreg code and moved instruction writing align to cpuasm,
      but it doesn't use the specified register yet

  Revision 1.17  2000/01/07 01:14:27  peter
    * updated copyright to 2000

  Revision 1.16  1999/12/20 22:29:26  pierre
    * relocation with debug info in rva (only with internal compiler)

  Revision 1.15  1999/11/30 10:40:43  peter
    + ttype, tsymlist

  Revision 1.14  1999/11/06 14:34:21  peter
    * truncated log to 20 revs

  Revision 1.13  1999/11/02 15:06:57  peter
    * import library fixes for win32
    * alignment works again

  Revision 1.12  1999/08/16 15:35:25  pierre
    * fix for DLL relocation problems
    * external bss vars had wrong stabs for pecoff
    + -WB11000000 to specify default image base, allows to
      load several DLLs with debugging info included
      (relocatable DLL are stripped because the relocation
       of the .Stab section is misplaced by ldw)

  Revision 1.11  1999/08/11 17:17:38  peter
    * fixed rva writting for section relocs
    * fixed section flags for edata and idata

  Revision 1.10  1999/08/04 00:23:05  florian
    * renamed i386asm and i386base to cpuasm and cpubase

}
