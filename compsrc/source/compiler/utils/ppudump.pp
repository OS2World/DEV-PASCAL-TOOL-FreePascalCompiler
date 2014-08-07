{
    $Id: ppudump.pp,v 1.1.2.11 2003/03/29 14:15:04 hajny Exp $
    Copyright (c) 1998-2000 by the FPC Development Team

    Dumps the contents of a FPC unit file (PPU File)

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

 ****************************************************************************}
{$ifdef TP}
  {$N+,E+}
{$endif}
program pppdump;
uses
{$ifdef go32v2}
  dpmiexcp,
{$endif go32v2}
  ppu;

const
  Version   = 'Version 1.01';
  Title     = 'PPU-Analyser';
  Copyright = 'Copyright (c) 1998-2001 by the Free Pascal Development Team';

{ verbosity }
  v_none           = $0;
  v_header         = $1;
  v_defs           = $2;
  v_syms           = $4;
  v_interface      = $8;
  v_implementation = $10;
  v_browser        = $20;
  v_all            = $ff;

var
  ppufile     : pppufile;
  space       : string;
  read_member : boolean;
  verbose     : longint;

type
       { Copied from systems.pas }
       ttargetcpu=
       (
             no_cpu,                   { 0 }
             i386,                     { 1 }
             m68k,                     { 2 }
             alpha,                    { 3 }
             powerpc,                  { 4 }
             sparc,                    { 5 }
             vm                        { 6 }
       );


{****************************************************************************
                          Helper Routines
****************************************************************************}

const has_errors : boolean = false;
Procedure Error(const S : string);
Begin
   Writeln(S);
   has_errors:=true;
End;

Function Target2Str(w:longint):string;
type
       { taken from systems.pas }
       ttarget =
       (
             target_none,               { 0 }
             target_i386_GO32V1,        { 1 }
             target_i386_GO32V2,        { 2 }
             target_i386_linux,         { 3 }
             target_i386_OS2,           { 4 }
             target_i386_Win32,         { 5 }
             target_i386_freebsd,       { 6 }
             target_m68k_Amiga,         { 7 }
             target_m68k_Atari,         { 8 }
             target_m68k_Mac,           { 9 }
             target_m68k_linux,         { 10 }
             target_m68k_PalmOS,        { 11 }
             target_alpha_linux,        { 12 }
             target_powerpc_linux,      { 13 }
             target_powerpc_macos,      { 14 }
             target_i386_sunos,         { 15 }
             target_i386_beos,          { 16 }
             target_i386_netbsd,        { 17 }
             target_m68k_netbsd,        { 18 }
             target_i386_Netware,       { 19 }
             target_i386_qnx,           { 20 }
             target_i386_wdosx,         { 21 }
             target_sparc_sunos,        { 22 }
             target_sparc_linux,        { 23 }
             target_i386_openbsd,       { 24 }
             target_m68k_openbsd,       { 25 }
             system_x86_64_linux,       { 26 }
             system_powerpc_macosx,     { 27 }
             target_i386_EMX            { 28 }
       );
const
  Targets : array[ttarget] of string[16]=(
  { 0 }   'none',
  { 1 }   'GO32V1',
  { 2 }   'GO32V2',
  { 3 }   'Linux-i386',
  { 4 }   'OS/2',
  { 5 }   'Win32',
  { 6 }   'FreeBSD-i386',
  { 7 }   'Amiga',
  { 8 }   'Atari',
  { 9 }   'MacOS-m68k',
  { 10 }  'Linux-m68k',
  { 11 }  'PalmOS-m68k',
  { 12 }  'Linux-alpha',
  { 13 }  'Linux-ppc',
  { 14 }  'MacOS-ppc',
  { 15 }  'Solaris-i386',
  { 16 }  'BeOS-i386',
  { 17 }  'NetBSD-i386',
  { 18 }  'NetBSD-m68k',
  { 19 }  'Netware',
  { 20 }  'Qnx-i386',
  { 21 }  'WDOSX-i386',
  { 22 }  'Solaris-sparc',
  { 23 }  'Linux-sparc',
  { 24 }  'OpenBSD-i386',
  { 25 }  'OpenBSD-m68k',
  { 26 }  'Linux-x86-64',
  { 27 }  'MacOSX-ppc',
  { 28 }  'OS/2 via EMX'
  );
begin
  if w<=ord(high(ttarget)) then
    Target2Str:=Targets[ttarget(w)]
  else
    Target2Str:='<Unknown>';
end;


Function Cpu2Str(w:longint):string;
const
  CpuTxt : array[ttargetcpu] of string[7]=
    ('none','i386','m68k','alpha','powerpc','sparc','vis');
begin
  if w<=ord(high(ttargetcpu)) then
    Cpu2Str:=CpuTxt[ttargetcpu(w)]
  else
    Cpu2Str:='<Unknown>';
end;


function PPUFlags2Str(flags:longint):string;
type
  tflagopt=record
    mask : longint;
    str  : string[30];
  end;
const
  flagopts=14;
  flagopt : array[1..flagopts] of tflagopt=(
    (mask: $1    ;str:'init'),
    (mask: $2    ;str:'final'),
    (mask: $4    ;str:'big_endian'),
    (mask: $8    ;str:'dbx'),
    (mask: $10   ;str:'browser'),
    (mask: $20   ;str:'in_library'),
    (mask: $40   ;str:'smart_linked'),
    (mask: $80   ;str:'static_linked'),
    (mask: $100  ;str:'shared_linked'),
    (mask: $200  ;str:'local_browser'),
    (mask: $400  ;str:'no_link'),
    (mask: $800  ;str:'uf_has_resources'),
    (mask: $1000  ;str:'uf_little_endian'),
    (mask: $2000  ;str:'uf_release')
  );
var
  i : longint;
  first  : boolean;
  s : string;
begin
  s:='';
  if flags<>0 then
   begin
     first:=true;
     for i:=1 to flagopts do
      if (flags and flagopt[i].mask)<>0 then
       begin
         if first then
           first:=false
         else
           s:=s+', ';
         s:=s+flagopt[i].str;
       end;
   end
  else
   s:='none';
  PPUFlags2Str:=s;
end;


const
  HexTbl : array[0..15] of char='0123456789ABCDEF';
function HexB(b:byte):string;
begin
  HexB[0]:=#2;
  HexB[1]:=HexTbl[b shr 4];
  HexB[2]:=HexTbl[b and $f];
end;


{****************************************************************************
                             Read Routines
****************************************************************************}

Procedure ReadLinkContainer(const prefix:string);
{
  Read a serie of strings and write to the screen starting every line
  with prefix
}
  function maskstr(m:longint):string;
  const
    { link options }
    link_none    = $0;
    link_allways = $1;
    link_static  = $2;
    link_smart   = $4;
    link_shared  = $8;
  var
    s : string;
  begin
    s:='';
    if (m and link_allways)<>0 then
     s:=s+'always ';
    if (m and link_static)<>0 then
     s:=s+'static ';
    if (m and link_smart)<>0 then
     s:=s+'smart ';
    if (m and link_shared)<>0 then
     s:=s+'shared ';
    maskstr:=s;
  end;

var
  s : string;
  m : longint;
begin
  while not ppufile^.endofentry do
   begin
     s:=ppufile^.getstring;
     m:=ppufile^.getlongint;
     WriteLn(prefix,s,' (',maskstr(m),')');
   end;
end;


Procedure ReadContainer(const prefix:string);
{
  Read a serie of strings and write to the screen starting every line
  with prefix
}
begin
  while not ppufile^.endofentry do
   WriteLn(prefix,ppufile^.getstring);
end;


Procedure ReadRef;
begin
  if (verbose and v_browser)=0 then
   exit;
  while (not ppufile^.endofentry) and (not ppufile^.error) do
   Writeln(space,'        - Refered : ',ppufile^.getword,', (',ppufile^.getlongint,',',ppufile^.getword,')');
end;


Procedure ReadPosInfo;
begin
  Writeln(ppufile^.getword,' (',ppufile^.getlongint,',',ppufile^.getword,')');
end;


function readderef(const s:string;skipnil:boolean):boolean;
type
  tdereftype = (derefnil,derefaktrecordindex,derefaktstaticindex,
                derefunit,derefrecord,derefindex,
                dereflocal,derefpara,derefaktlocalindex);
var
  b : tdereftype;
begin
  readderef:=true;
  repeat
    b:=tdereftype(ppufile^.getbyte);
    case b of
      derefnil :
        begin
          if not skipnil then
           writeln('nil');
          readderef:=false;
          break;
        end;
      derefaktrecordindex :
        begin
          writeln('AktRecord ',s,' ',ppufile^.getword);
          break;
        end;
      derefaktstaticindex :
        begin
          writeln('AktStatic ',s,' ',ppufile^.getword);
          break;
        end;
      derefaktlocalindex :
        begin
          writeln('AktLocal ',s,' ',ppufile^.getword);
          break;
        end;
      derefunit :
        begin
          writeln('Unit ',ppufile^.getword);
          break;
        end;
      derefrecord :
        begin
          write('RecordDef ',ppufile^.getword,', ');
        end;
      derefpara :
        begin
          write('Parameter of procdef ',ppufile^.getword,', ');
        end;
      dereflocal :
        begin
          write('Local of procdef ',ppufile^.getword,', ');
        end;
      derefindex :
        begin
          write(s,' ',ppufile^.getword,', ');
        end;
      else
        begin
          writeln('!! unsupported dereftyp: ',ord(derefindex));
          break;
        end;
    end;
  until false;
end;


function readdefref:boolean;
begin
  readdefref:=readderef('Definition',false);
end;


function readsymref:boolean;
begin
  readsymref:=readderef('Symbol',false);
end;


procedure readtype;
var
  b1,b2 : boolean;
begin
  b1:=readderef('Definition',true);
  b2:=readderef('Symbol',true);
  if not(b1 or b2) then
   Writeln('nil')
  else
   if (b1 and b2) then
    Writeln('!! Type has both definition and symbol stored');
end;


procedure readsymlist(const s:string);
begin
  readdefref;
  repeat
    write(s);
    if not readsymref then
     break;
  until false;
end;


procedure read_abstract_proc_def;
type
  tproccalloption=(pocall_none,
    pocall_clearstack,    { Use IBM flat calling convention. (Used by GCC.) }
    pocall_leftright,     { Push parameters from left to right }
    pocall_cdecl,         { procedure uses C styled calling }
    pocall_register,      { procedure uses register (fastcall) calling }
    pocall_stdcall,       { procedure uses stdcall call }
    pocall_safecall,      { safe call calling conventions }
    pocall_palmossyscall, { procedure is a PalmOS system call }
    pocall_system,
    pocall_inline,        { Procedure is an assembler macro }
    pocall_internproc,    { Procedure has compiler magic}
    pocall_internconst    { procedure has constant evaluator intern }
  );
  tproccalloptions=set of tproccalloption;
  tproctypeoption=(potype_none,
    potype_proginit,     { Program initialization }
    potype_unitinit,     { unit initialization }
    potype_unitfinalize, { unit finalization }
    potype_constructor,  { Procedure is a constructor }
    potype_destructor,   { Procedure is a destructor }
    potype_operator      { Procedure defines an operator }
  );
  tproctypeoptions=set of tproctypeoption;
  tprocoption=(po_none,
    po_classmethod,       { class method }
    po_virtualmethod,     { Procedure is a virtual method }
    po_abstractmethod,    { Procedure is an abstract method }
    po_staticmethod,      { static method }
    po_overridingmethod,  { method with override directive }
    po_methodpointer,     { method pointer, only in procvardef, also used for 'with object do' }
    po_containsself,      { self is passed explicit to the compiler }
    po_interrupt,         { Procedure is an interrupt handler }
    po_iocheck,           { IO checking should be done after a call to the procedure }
    po_assembler,         { Procedure is written in assembler }
    po_msgstr,            { method for string message handling }
    po_msgint,            { method for int message handling }
    po_exports,           { Procedure has export directive (needed for OS/2) }
    po_external,          { Procedure is external (in other object or lib)}
    po_savestdregs,       { save std regs cdecl and stdcall need that ! }
    po_saveregisters      { save all registers }
  );
  tprocoptions=set of tprocoption;
type
  tproccallopt=record
    mask : tproccalloption;
    str  : string[30];
  end;
  tproctypeopt=record
    mask : tproctypeoption;
    str  : string[30];
  end;
  tprocopt=record
    mask : tprocoption;
    str  : string[30];
  end;
const
  proccallopts=12;
  proccallopt : array[1..proccallopts] of tproccallopt=(
     (mask:pocall_none;         str:''),
     (mask:pocall_clearstack;   str:'ClearStack'),
     (mask:pocall_leftright;    str:'LeftRight'),
     (mask:pocall_cdecl;        str:'Cdecl'),
     (mask:pocall_register;     str:'Register'),
     (mask:pocall_stdcall;      str:'StdCall'),
     (mask:pocall_safecall;     str:'SafeCall'),
     (mask:pocall_palmossyscall;str:'PalmOSSysCall'),
     (mask:pocall_system;       str:'System'),
     (mask:pocall_inline;       str:'Inline'),
     (mask:pocall_internproc;   str:'InternProc'),
     (mask:pocall_internconst;  str:'InternConst')
  );
  proctypeopts=6;
  proctypeopt : array[1..proctypeopts] of tproctypeopt=(
     (mask:potype_proginit;    str:'ProgInit'),
     (mask:potype_unitinit;    str:'UnitInit'),
     (mask:potype_unitfinalize;str:'UnitFinalize'),
     (mask:potype_constructor; str:'Constructor'),
     (mask:potype_destructor;  str:'Destructor'),
     (mask:potype_operator;    str:'Operator')
  );
  procopts=16;
  procopt : array[1..procopts] of tprocopt=(
     (mask:po_classmethod;     str:'ClassMethod'),
     (mask:po_virtualmethod;   str:'VirtualMethod'),
     (mask:po_abstractmethod;  str:'AbstractMethod'),
     (mask:po_staticmethod;    str:'StaticMethod'),
     (mask:po_overridingmethod;str:'OverridingMethod'),
     (mask:po_methodpointer;   str:'MethodPointer'),
     (mask:po_containsself;    str:'ContainsSelf'),
     (mask:po_interrupt;       str:'Interrupt'),
     (mask:po_iocheck;         str:'IOCheck'),
     (mask:po_assembler;       str:'Assembler'),
     (mask:po_msgstr;          str:'MsgStr'),
     (mask:po_msgint;          str:'MsgInt'),
     (mask:po_exports;         str:'Exports'),
     (mask:po_external;        str:'External'),
     (mask:po_savestdregs;     str:'SaveStdRegs'),
     (mask:po_saveregisters;   str:'SaveRegisters')
  );
  tvarspez : array[0..2] of string[5]=('Value','Const','Var  ');
var
  proctypeoption  : tproctypeoption;
  proccalloptions : tproccalloptions;
  procoptions     : tprocoptions;
  i,params : longint;
  first    : boolean;
begin
  write(space,'      Return type : ');
  readtype;
  writeln(space,'         Fpu used : ',ppufile^.getbyte);
  proctypeoption:=tproctypeoption(ppufile^.getlongint);
  if proctypeoption<>potype_none then
   begin
     write(space,'       TypeOption : ');
     first:=true;
     for i:=1to proctypeopts do
      if (proctypeopt[i].mask=proctypeoption) then
       begin
         if first then
           first:=false
         else
           write(', ');
         write(proctypeopt[i].str);
       end;
     writeln;
   end;
  ppufile^.getsmallset(proccalloptions);
  if proccalloptions<>[] then
   begin
     write(space,'      CallOptions : ');
     first:=true;
     for i:=1to proccallopts do
      if (proccallopt[i].mask in proccalloptions) then
       begin
         if first then
           first:=false
         else
           write(', ');
         write(proccallopt[i].str);
       end;
     writeln;
   end;
  ppufile^.getsmallset(procoptions);
  if procoptions<>[] then
   begin
     write(space,'          Options : ');
     first:=true;
     for i:=1to procopts do
      if (procopt[i].mask in procoptions) then
       begin
         if first then
           first:=false
         else
           write(', ');
         write(procopt[i].str);
       end;
     writeln;
   end;
  params:=ppufile^.getword;
  writeln(space,' Nr of parameters : ',params);
  if params>0 then
   begin
     repeat
       write(space,'  - ',tvarspez[ppufile^.getbyte],' : ');
       readtype;
       dec(params);
     until params=0;
   end;
end;


procedure readcommonsym(const s:string);
type
  tsymoption=(sp_none,
    sp_public,
    sp_private,
    sp_published,
    sp_protected,
    sp_forwarddef,
    sp_static,
    sp_primary_typesym    { this is for typesym, to know who is the primary symbol of a def }
  );
  tsymoptions=set of tsymoption;
  tsymopt=record
    mask : tsymoption;
    str  : string[30];
  end;
const
  symopts=7;
  symopt : array[1..symopts] of tsymopt=(
     (mask:sp_public;         str:'Public'),
     (mask:sp_private;        str:'Private'),
     (mask:sp_published;      str:'Published'),
     (mask:sp_protected;      str:'Protected'),
     (mask:sp_forwarddef;     str:'ForwardDef'),
     (mask:sp_static;         str:'Static'),
     (mask:sp_primary_typesym;str:'PrimaryTypeSym')
  );
var
  symoptions : tsymoptions;
  i      : longint;
  first  : boolean;
begin
  writeln(space,'** Symbol Nr. ',ppufile^.getword,' **');
  writeln(space,s,ppufile^.getstring);
  ppufile^.getsmallset(symoptions);
  if symoptions<>[] then
   begin
     write(space,'    File Pos: ');
     readposinfo;
     write(space,'     Options: ');
     first:=true;
     for i:=1to symopts do
      if (symopt[i].mask in symoptions) then
       begin
         if first then
           first:=false
         else
           write(', ');
         write(symopt[i].str);
       end;
     writeln;
   end;
end;


procedure readcommondef(const s:string);
begin
  writeln(space,'** Definition Nr. ',ppufile^.getword,' **');
  writeln(space,s);
  write  (space,'      Type symbol : ');
  readsymref;
end;


{****************************************************************************
                             Read Symbols Part
****************************************************************************}

procedure readsymbols;
Const
  vo_is_C_var = 2;
Type
  absolutetyp = (tovar,toasm,toaddr);
  tconsttyp = (constnone,
    constord,conststring,constreal,constbool,
    constint,constchar,constset,constpointer,constnil,
    constresourcestring
  );
var
  b      : byte;
  pc     : pchar;
  totalsyms,
  symcnt,
  i,j,len : longint;
begin
  symcnt:=1;
  with ppufile^ do
   begin
     if space<>'' then
      Writeln(space,'-----------------------------');
     if readentry=ibstartsyms then
      begin
        totalsyms:=getlongint;
        Writeln(space,'Number of symbols : ',totalsyms);
        Writeln(space,'Symtable datasize : ',getlongint);
        Writeln(space,'Symtable alignment: ',getlongint);
      end
     else
      begin
        totalsyms:=-1;
        Writeln('!! ibstartsym not found');
      end;
     repeat
       b:=readentry;
       if not (b in [iberror,ibendsyms]) then
        inc(symcnt);
       case b of

         ibunitsym :
           readcommonsym('Unit symbol ');

         iblabelsym :
           readcommonsym('Label symbol ');

         ibtypesym :
           begin
             readcommonsym('Type symbol ');
             write(space,' Result Type: ');
             readtype;
           end;

         ibprocsym :
           begin
             readcommonsym('Procedure symbol ');
             write(space,'  Definition: ');
             readdefref;
           end;

         ibconstsym :
           begin
             readcommonsym('Constant symbol ');
             b:=getbyte;
             case tconsttyp(b) of
               constord :
                 begin
                   write (space,' Ordinal Type : ');
                   readtype;
                   writeln (space,'      Value : ',getlongint)
                 end;
               constpointer :
                 begin
                   write (space,' Pointer Type : ');
                   readtype;
                   writeln (space,'      Value : ',getlongint)
                 end;
               conststring,
               constresourcestring :
                 begin
                   len:=getlongint;
                   getmem(pc,len+1);
                   getdata(pc^,len);
                   writeln(space,' Length : ',len);
                   writeln(space,'  Value : "',pc,'"');
                   freemem(pc,len+1);
                   if tconsttyp(b)=constresourcestring then
                    writeln(space,'  Index : ',getlongint);
                 end;
               constreal :
                 writeln(space,'  Value : ',getreal);
               constbool :
                 if getlongint<>0 then
                   writeln (space,'  Value : True')
                 else
                   writeln (space,'  Value : False');
               constint :
                 writeln(space,'  Value : ',getlongint);
               constchar :
                 writeln(space,'  Value : "'+chr(getlongint)+'"');
               constset :
                 begin
                   write (space,'  Set Type : ');
                   readtype;
                   for i:=1to 4 do
                    begin
                      write (space,'  Value : ');
                      for j:=1to 8 do
                       begin
                         if j>1 then
                          write(',');
                         write(hexb(getbyte));
                       end;
                      writeln;
                    end;
                 end;
               else
                 Writeln ('!! Invalid unit format : Invalid const type encountered: ',b);
             end;
           end;

         ibvarsym :
           begin
             readcommonsym('Variable symbol ');
             writeln(space,'        Type: ',getbyte);
             if read_member then
               writeln(space,'     Address: ',getlongint);
             write  (space,'    Var Type: ');
             readtype;
             i:=getlongint;
             writeln(space,'     Options: ',i);
             if (i and vo_is_C_var)<>0 then
               writeln(space,' Mangledname: ',getstring);
           end;

         ibenumsym :
           begin
             readcommonsym('Enumeration symbol ');
             write  (space,'  Definition: ');
             readdefref;
             writeln(space,'       Value: ',getlongint);
           end;

         ibsyssym :
           begin
             readcommonsym('Internal system symbol ');
             writeln(space,' Internal Nr: ',getlongint);
           end;

         ibtypedconstsym :
           begin
             readcommonsym('Typed constant ');
             write  (space,' Constant Type: ');
             readtype;
             writeln(space,'         Label: ',getstring);
             writeln(space,'   ReallyConst: ',(getbyte<>0));
           end;

         ibabsolutesym :
           begin
             readcommonsym('Absolute variable symbol ');
             writeln(space,'          Type: ',getbyte);
             if read_member then
               writeln(space,'       Address: ',getlongint);
             write  (space,'      Var Type: ');
             readtype;
             writeln(space,'       Options: ',getlongint);
             Write (space,'    Relocated to ');
             b:=getbyte;
             case absolutetyp(b) of
               tovar :
                 Writeln('Name : ',getstring);
               toasm :
                 Writeln('Assembler name : ',getstring);
               toaddr :
                 begin
                   Write('Address : ',getlongint);
                   WriteLn(' (Far: ',getbyte<>0,')');
                 end;
               else
                 Writeln ('!! Invalid unit format : Invalid absolute type encountered: ',b);
             end;
           end;

         ibpropertysym :
           begin
             readcommonsym('Property ');
             write  (space,'     Prop Type: ');
             readtype;
             writeln(space,'       Options: ',getlongint);
             writeln(space,'         Index: ',getlongint);
             writeln(space,'       Default: ',getlongint);
             write  (space,'    Index Type: ');
             readtype;
             write  (space,'   Read access: ');
             readsymlist(space+'           Sym: ');
             write  (space,'  Write access: ');
             readsymlist(space+'           Sym: ');
             write  (space,' Stored access: ');
             readsymlist(space+'           Sym: ');
           end;

         ibfuncretsym :
           begin
             readcommonsym('Func return value ');
             write  (space,' Return Type: ');
             readtype;
             writeln(space,'     Address: ',getlongint);
           end;

         iberror :
           begin
             Writeln('!! Error in PPU');
             exit;
           end;

         ibendsyms :
           break;

         else
           WriteLn('!! Skipping unsupported PPU Entry in Symbols: ',b);
       end;
       if not EndOfEntry then
        Writeln('!! Entry has more information stored');
     until false;
     if (totalsyms<>-1) and (symcnt-1<>totalsyms) then
       Writeln('!! Only read ',symcnt-1,' of ',totalsyms,' symbols');
   end;
end;


{****************************************************************************
                         Read defintions Part
****************************************************************************}

procedure readdefinitions(start_read : boolean);
type
  tsettype  = (normset,smallset,varset);
  tbasetype = (uauto,uvoid,uchar,
               u8bit,u16bit,u32bit,
               s8bit,s16bit,s32bit,
               bool8bit,bool16bit,bool32bit,
               u64bit,s64bit);
var
  b : byte;
  oldread_member : boolean;
  totaldefs,
  defcnt : longint;
  setlongintarray : array[0..7] of longint;
begin
  defcnt:=0;
  with ppufile^ do
   begin
     if space<>'' then
      Writeln(space,'-----------------------------');
     if not start_read then
       if readentry=ibstartdefs then
         begin
           totaldefs:=getlongint;
           Writeln(space,'Number of definitions: ',totaldefs);
         end
       else
         begin
           totaldefs:=-1;
           Writeln('!! ibstartdef not found');
         end;
     repeat
       b:=readentry;
       if not (b in [iberror,ibenddefs]) then
        inc(defcnt);
       case b of

         ibpointerdef :
           begin
             readcommondef('Pointer definition');
             write  (space,'    Pointed Type : ');
             readtype;
             writeln(space,'          Is Far : ',(getbyte<>0));
           end;

         iborddef :
           begin
             readcommondef('Ordinal definition');
             write  (space,'        Base type : ');
             b:=getbyte;
             case tbasetype(b) of
               uauto     : writeln('uauto');
               uvoid     : writeln('uvoid');
               uchar     : writeln('uchar');
               u8bit     : writeln('u8bit');
               u16bit    : writeln('u16bit');
               u32bit    : writeln('s32bit');
               s8bit     : writeln('s8bit');
               s16bit    : writeln('s16bit');
               s32bit    : writeln('s32bit');
               bool8bit  : writeln('bool8bit');
               bool16bit : writeln('bool16bit');
               bool32bit : writeln('bool32bit');
               u64bit    : writeln('u64bit');
               s64bit    : writeln('s64bit');
               else        writeln('!! Warning: Invalid base type ',b);
             end;
             writeln(space,'            Range : ',getlongint,' to ',getlongint);
           end;

         ibfloatdef :
           begin
             readcommondef('Float definition');
             writeln(space,'       Float type : ',getbyte);
           end;

         ibarraydef :
           begin
             readcommondef('Array definition');
             write  (space,'     Element type : ');
             readtype;
             write  (space,'       Range Type : ');
             readtype;
             writeln(space,'            Range : ',getlongint,' to ',getlongint);
             writeln(space,'   Is Constructor : ',(getbyte<>0));
           end;

         ibprocdef :
           begin
             readcommondef('Procedure definition');
             read_abstract_proc_def;
             if ttargetcpu(ppufile^.header.cpu) = i386 then
               writeln(space,'    Used Register : ',getbyte)
             else
               begin
                 writeln(space,'    Used Register : ');
                 getnormalset(SetLongintArray);
               end;
             writeln(space,'     Mangled name : ',getstring);
             writeln(space,'           Number : ',getlongint);
             write  (space,'             Next : ');
             readdefref;
             write  (space,'            Class : ');
             readdefref;
             write  (space,'         File Pos : ');
             readposinfo;
           end;

         ibprocvardef :
           begin
             readcommondef('Procedural type (ProcVar) definition');
             read_abstract_proc_def;
           end;

         ibshortstringdef :
           begin
             readcommondef('ShortString definition');
             writeln(space,'           Length : ',getbyte);
           end;

         ibwidestringdef :
           begin
             readcommondef('WideString definition');
             writeln(space,'           Length : ',getlongint);
           end;

         ibansistringdef :
           begin
             readcommondef('AnsiString definition');
             writeln(space,'           Length : ',getlongint);
           end;

         iblongstringdef :
           begin
             readcommondef('Longstring definition');
             writeln(space,'           Length : ',getlongint);
           end;

         ibrecorddef :
           begin
             readcommondef('Record definition');
             writeln(space,'             Size : ',getlongint);
             {read the record definitions and symbols}
             space:='    '+space;
             oldread_member:=read_member;
             read_member:=true;
             readdefinitions(false);
             readsymbols;
             read_member:=oldread_member;
             Delete(space,1,4);
           end;

         ibobjectdef :
           begin
             readcommondef('Object/Class definition');
             writeln(space,'             Size : ',getlongint);
             writeln(space,'       Vmt offset : ',getlongint);
             writeln(space,'    Name of Class : ',getstring);
             write(space,  '   Ancestor Class : ');
             readdefref;
             writeln(space,'          Options : ',getlongint);
           {read the record definitions and symbols}
             space:='    '+space;
             oldread_member:=read_member;
             read_member:=true;
             readdefinitions(false);
             readsymbols;
             read_member:=oldread_member;
             Delete(space,1,4);
           end;

         ibfiledef :
           begin
             ReadCommonDef('File definition');
             write  (space,'             Type : ');
             case getbyte of
              0 : writeln('Text');
              1 : begin
                    writeln('Typed');
                    write  (space,'      File of Type : ');
                    Readtype;
                  end;
              2 : writeln('Untyped');
             end;
           end;

         ibformaldef :
           readcommondef('Generic Definition (void-typ)');

         ibenumdef :
           begin
             readcommondef('Enumeration type definition');
             write(space,'Base enumeration type : ');
             readdefref;
             writeln(space,' Smallest element : ',getlongint);
             writeln(space,'  Largest element : ',getlongint);
             writeln(space,'             Size : ',getlongint);
           end;

         ibclassrefdef :
           begin
             readcommondef('Class reference definition');
             write  (space,'    Pointed Type : ');
             readtype;
           end;

         ibsetdef :
           begin
             readcommondef('Set definition');
             write  (space,'     Element type : ');
             readtype;
             b:=getbyte;
             case tsettype(b) of
               smallset : writeln(space,'  Set with 32 Elements');
               normset  : writeln(space,'  Set with 256 Elements');
               varset   : writeln(space,'  Set with ',getlongint,' Elements');
               else       writeln('!! Warning: Invalid set type ',b);
             end;
           end;

         iberror :
           begin
             Writeln('!! Error in PPU');
             exit;
           end;

         ibenddefs :
           break;

         else
           WriteLn('!! Skipping unsupported PPU Entry in definitions: ',b);
       end;
       if not EndOfEntry then
        Writeln('!! Entry has more information stored');
     until false;
     if (totaldefs<>-1) and (defcnt<>totaldefs) then
      Writeln('!! Only read ',defcnt,' of ',totaldefs,' definitions');
   end;
end;


{****************************************************************************
                           Read General Part
****************************************************************************}

procedure readinterface;
var
  b : byte;
  sourcenumber,
  unitnumber : word;
  ucrc,uintfcrc : longint;
begin
  with ppufile^ do
   begin
     repeat
       b:=readentry;
       case b of

         ibmodulename :
           Writeln('Module Name: ',getstring);

         ibsourcefiles :
           begin
             sourcenumber:=1;
             while not EndOfEntry do
              begin
                Writeln('Source file ',sourcenumber,' : ',getstring);
                inc(sourcenumber);
              end;
           end;

         ibusedmacros :
           begin
             while not EndOfEntry do
              begin
                Write('Conditional ',getstring);
                b:=getbyte;
                if boolean(b)=true then
                  write(' defined at startup')
                else
                  write(' not defined at startup');
                b:=getbyte;
                if boolean(b)=true then
                  writeln(' was used')
                else
                  writeln;
              end;
           end;
         ibloadunit :
           begin
             unitnumber:=1;
             while not EndOfEntry do
              begin
                write('Uses unit: ',getstring,' (Number: ',unitnumber,')');
                ucrc:=getlongint;
                uintfcrc:=getlongint;
                write(' (Crc: ',ucrc,', IntfcCrc: ',uintfcrc,')');
                if getbyte<>0 then
                 writeln(' (interface)')
                else
                 writeln(' (implementation)');
                inc(unitnumber);
              end;
           end;

         iblinkunitofiles :
           ReadLinkContainer('Link unit object file: ');

         iblinkunitstaticlibs :
           ReadLinkContainer('Link unit static lib: ');

         iblinkunitsharedlibs :
           ReadLinkContainer('Link unit shared lib: ');

         iblinkotherofiles :
           ReadLinkContainer('Link other object file: ');

         iblinkotherstaticlibs :
           ReadLinkContainer('Link other static lib: ');

         iblinkothersharedlibs :
           ReadLinkContainer('Link other shared lib: ');

         iberror :
           begin
             Writeln('Error in PPU');
             exit;
           end;

         ibendinterface :
           break;

         else
           WriteLn('!! Skipping unsupported PPU Entry in General Part: ',b);
       end;
     until false;
   end;
end;



{****************************************************************************
                        Read Implementation Part
****************************************************************************}

procedure readimplementation;
var
  b : byte;
begin
  with ppufile^ do
   begin
     repeat
       b:=readentry;
       case b of
         iberror :
           begin
             Writeln('Error in PPU');
             exit;
           end;
         ibendimplementation :
           break;
         else
           WriteLn('!! Skipping unsupported PPU Entry in Implementation: ',b);
       end;
     until false;
   end;
end;


{****************************************************************************
                            Read Browser Part
****************************************************************************}

procedure readbrowser;
var
  b : byte;
const indent : string = '';
begin
  Writeln(indent,'Start of symtable browser');
  indent:=indent+'**';
  with ppufile^ do
   begin
     repeat
       b:=readentry;
       case b of
        ibbeginsymtablebrowser :
                         { here we must read object and record symtables !! }
                    begin
                      indent:=indent+'  ';
                      Writeln(indent,'Record/Object symtable');
                      readbrowser;
                      Indent:=Copy(Indent,1,Length(Indent)-2);
                    end;
            ibsymref : begin
                         readsymref;
                         readref;
                       end;
            ibdefref : begin
                         readdefref;
                         readref;
                         if (ppufile^.header.flags and uf_local_browser)<>0 then
                           begin
                             { parast and localst }
                             indent:=indent+'  ';
                             Writeln(indent,'Parasymtable for function');
                             readdefinitions(false);
                             readsymbols;
                             b:=ppufile^.readentry;
                             if b=ibbeginsymtablebrowser then
                               readbrowser;
                             Writeln(indent,'Localsymtable for function');
                             readdefinitions(false);
                             readsymbols;
                             b:=ppufile^.readentry;
                             if b=ibbeginsymtablebrowser then
                               readbrowser;
                             Indent:=Copy(Indent,1,Length(Indent)-2);
                           end;
                       end;
             iberror : begin
                         Writeln('Error in PPU');
                         exit;
                       end;
        ibendsymtablebrowser : break;
       else
        begin
        WriteLn('!! Skipping unsupported PPU Entry in Browser: ',b);
        Halt;
        end;
       end;
     until false;
   end;
  Indent:=Copy(Indent,1,Length(Indent)-2);
  Writeln(Indent,'End of symtable browser');
end;




procedure dofile (filename : string);
var
  b,unitindex : byte;
begin
{ reset }
  space:='';
{ fix filename }
  if pos('.',filename)=0 then
   filename:=filename+'.ppu';
  ppufile:=new(pppufile,Init(filename));
  if not ppufile^.openfile then
   begin
     writeln ('IO-Error when opening : ',filename,', Skipping');
     exit;
   end;
{ PPU File is open, check for PPU Id }
  if not ppufile^.CheckPPUID then
   begin
     writeln(Filename,' : Not a valid PPU file, Skipping');
     exit;
   end;
{ Check PPU Version }
  Writeln('Analyzing ',filename,' (v',ppufile^.GetPPUVersion,')');
  if ppufile^.GetPPUVersion<16 then
   begin
     writeln(Filename,' : Old PPU Formats (<v16) are not supported, Skipping');
     exit;
   end;
{ Write PPU Header Information }
  if (verbose and v_header)<>0 then
   begin
     Writeln;
     Writeln('Header');
     Writeln('-------');
     with ppufile^.header do
      begin
        Write('Compiler version        : ',ppufile^.header.compiler shr 14);
        if (ppufile^.header.compiler and $3FFF)<>0 then
          begin
            Write('.',(ppufile^.header.compiler shr 7) and $7F);
            if (ppufile^.header.compiler and $7F)<>0 then
              Write('.',ppufile^.header.compiler and $7F);
          end;
        Writeln;
        WriteLn('Target processor        : ',Cpu2Str(cpu));
        WriteLn('Target operating system : ',Target2Str(target));
        Writeln('Unit flags              : ',PPUFlags2Str(flags));
        Writeln('FileSize (w/o header)   : ',size);
        Writeln('Checksum                : ',checksum);
        Writeln('Interface Checksum      : ',interface_checksum);
      end;
   end;
{read the general stuff}
  if (verbose and v_interface)<>0 then
   begin
     Writeln;
     Writeln('Interface section');
     Writeln('------------------');
     readinterface;
   end
  else
   ppufile^.skipuntilentry(ibendinterface);
{read the definitions}
  if (verbose and v_defs)<>0 then
   begin
     Writeln;
     Writeln('Interface definitions');
     Writeln('----------------------');
     readdefinitions(false);
   end
  else
   ppufile^.skipuntilentry(ibenddefs);
{read the symbols}
  if (verbose and v_syms)<>0 then
   begin
     Writeln;
     Writeln('Interface Symbols');
     Writeln('------------------');
     readsymbols;
   end
  else
   ppufile^.skipuntilentry(ibendsyms);
{read the implementation stuff}
{ Not used at the moment (PFV)
  if (verbose and v_implementation)<>0 then
   begin
     Writeln;
     Writeln('Implementation section');
     Writeln('-----------------------');
     readimplementation;
   end
  else}
   ppufile^.skipuntilentry(ibendimplementation);
{read the static browser units stuff}
  if (ppufile^.header.flags and uf_local_browser)<>0 then
   begin
     if (verbose and v_defs)<>0 then
      begin
        Writeln;
        Writeln('Static definitions');
        Writeln('----------------------');
        readdefinitions(false);
      end
     else
      ppufile^.skipuntilentry(ibenddefs);
   {read the symbols}
     if (verbose and v_syms)<>0 then
      begin
        Writeln;
        Writeln('Static Symbols');
        Writeln('------------------');
        readsymbols;
      end;
   end;
{read the browser units stuff}
  if (ppufile^.header.flags and uf_has_browser)<>0 then
   begin
     if (verbose and v_browser)<>0 then
      begin
        Writeln;
        Writeln('Browser section');
        Writeln('---------------');
        UnitIndex:=0;
        repeat
          b:=ppufile^.readentry;
          if b = ibendbrowser then break;
          if b=ibbeginsymtablebrowser then
            begin
               Writeln('Unit ',UnitIndex);
               readbrowser;
               Inc(UnitIndex);
            end
          else
            Writeln('Wrong end browser entry ',b,' should be ',ibendbrowser);
        until false;
      end;
   end;
{read the static browser units stuff}
  if (ppufile^.header.flags and uf_local_browser)<>0 then
   begin
     if (verbose and v_browser)<>0 then
      begin
        Writeln;
        Writeln('Static browser section');
        Writeln('---------------');
        b:=ppufile^.readentry;
        if b=ibbeginsymtablebrowser then
          begin
             Writeln('Unit ',UnitIndex);
             readbrowser;
             Inc(UnitIndex);
          end
        else
          Writeln('Wrong end browser entry ',b,' should be ',ibendbrowser);
      end;
   end;
{shutdown ppufile}
  ppufile^.closefile;
  dispose(ppufile,done);
  Writeln;
end;



procedure help;
begin
  writeln('usage: ppudump [options] <filename1> <filename2>...');
  writeln;
  writeln('[options] can be:');
  writeln('    -V<verbose>  Set verbosity to <verbose>');
  writeln('                   H - Show header info');
  writeln('                   I - Show interface');
  writeln('                   M - Show implementation');
  writeln('                   S - Show interface symbols');
  writeln('                   D - Show interface definitions');
  writeln('                   B - Show browser info');
  writeln('                   A - Show all');
  writeln('    -?           This helpscreen');
  halt;
end;

var
  startpara,
  nrfile,i  : longint;
  para      : string;
begin
  writeln(Title+' '+Version);
  writeln(Copyright);
  writeln;
  if paramcount<1 then
   begin
     writeln('usage: ppudump [options] <filename1> <filename2>...');
     halt(1);
   end;
{ turn verbose on by default }
  verbose:=v_all;
{ read options }
  startpara:=1;
  while copy(paramstr(startpara),1,1)='-' do
   begin
     para:=paramstr(startpara);
     case upcase(para[2]) of
      'V' : begin
              verbose:=0;
              for i:=3to length(para) do
               case upcase(para[i]) of
                'H' : verbose:=verbose or v_header;
                'I' : verbose:=verbose or v_interface;
                'M' : verbose:=verbose or v_implementation;
                'D' : verbose:=verbose or v_defs;
                'S' : verbose:=verbose or v_syms;
                'B' : verbose:=verbose or v_browser;
                'A' : verbose:=verbose or v_all;
               end;
            end;
      '?' : help;
     end;
     inc(startpara);
   end;
{ process files }
  for nrfile:=startpara to paramcount do
   dofile (paramstr(nrfile));
  if has_errors then
    Halt(1);
end.
{
  $Log: ppudump.pp,v $
  Revision 1.1.2.11  2003/03/29 14:15:04  hajny
    + make the new 1.1 EMX target known to ppudump

  Revision 1.1.2.10  2002/11/04 19:09:51  carl
    +m68k support (was reading wrong data!)

  Revision 1.1.2.9  2002/09/27 20:57:35  carl
    * webbug report 2053
    + correct order of tsystems numbering

  Revision 1.1.2.8  2002/04/07 10:27:43  carl
  + added qnx target / sparc / vm targets

  Revision 1.1.2.7  2002/04/04 18:51:39  carl
  + added wdosx support (patch from Pavel)

  Revision 1.1.2.6  2002/03/01 21:01:28  pierre
   * fix compiler version display

  Revision 1.1.2.5  2001/12/15 05:28:19  carl
  + Added QNX target

  Revision 1.1.2.4  2001/09/22 04:51:11  carl
  * updated targets

  Revision 1.1.2.3  2001/06/20 22:01:25  pierre
   + add new flags support

  Revision 1.1.2.2  2001/05/18 22:35:01  peter
    * merged endian ppu fixes from mainbranch
    * endian define

  Revision 1.1.2.1  2000/09/09 20:22:18  peter
    * show dataalignment

  Revision 1.1  2000/07/13 10:16:22  michael
  + Initial import

  Revision 1.15  2000/07/04 19:05:54  peter
    * be optimistic: version 1.00 for some utils

  Revision 1.14  2000/02/09 16:44:14  peter
    * log truncated

  Revision 1.13  2000/01/23 16:34:36  peter
    * updated for new aktlocalindex

  Revision 1.12  2000/01/07 16:46:03  daniel
    * copyright 2000

  Revision 1.11  1999/11/30 10:35:37  peter
    * support new readtype

  Revision 1.10  1999/11/08 14:06:45  florian
    + indexref of propertysym is handle too now

  Revision 1.9  1999/08/31 16:07:37  pierre
   + support for writeusedmacros

  Revision 1.8  1999/08/15 10:47:14  peter
    * updates for new options

  Revision 1.7  1999/08/13 21:25:35  peter
    * updated flags

  Revision 1.6  1999/07/27 23:45:29  peter
    * updated for typesym writing

}
