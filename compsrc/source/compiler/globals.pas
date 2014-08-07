{
    $Id: globals.pas,v 1.1.2.41 2003/04/04 15:33:24 peter Exp $
    Copyright (C) 1998-2000 by Florian Klaempfl

    This unit implements some support functions and global variables

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

{$ifdef tp}
  {$E+,N+}
{$endif}

unit globals;

  interface

    uses
{$ifdef win32}
      windows,
{$endif}
{$ifdef hasunix}
  {$ifdef VER1_0} linux, {$else} unix, {$endif}
{$endif}
{$ifdef os2}
      doscalls,
{$endif}
{$ifdef Delphi}
      sysutils,
      dmisc,
{$else}
      strings,dos,
{$endif}
{$ifdef TP}
      objects,
{$endif}
      globtype,version,tokens,systems,cobjects;

    const
{$ifdef unix}
       DirSep = '/';
{$else}
  {$ifdef amiga}
       DirSep = '/';
  {$else}
       DirSep = '\';
  {$endif}
{$endif}

{$ifdef Splitheap}
       testsplit : boolean = false;
{$endif Splitheap}

       delphimodeswitches : tmodeswitches=
         [m_delphi,m_tp,m_all,m_class,m_objpas,m_result,m_string_pchar,
          m_pointer_2_procedure,m_autoderef,m_tp_procvar,m_initfinal,m_default_ansistring];
       fpcmodeswitches    : tmodeswitches=
         [m_fpc,m_all,m_string_pchar,m_nested_comment,m_repeat_forward,
          m_cvar_support,m_initfinal,m_add_pointer];
       objfpcmodeswitches : tmodeswitches=
         [m_objfpc,m_fpc,m_all,m_class,m_objpas,m_result,m_string_pchar,m_nested_comment,
          m_repeat_forward,m_cvar_support,m_initfinal,m_add_pointer];
       tpmodeswitches     : tmodeswitches=
         [m_tp7,m_tp,m_all,m_tp_procvar];
       gpcmodeswitches    : tmodeswitches=
         [m_gpc,m_all];

    type

    {$ifdef tp}
       smallint = integer;
       cardinal = longint;
    {$endif}
       TSearchPathList = object(TStringQueue)
         procedure AddPath(s:string;addfirst:boolean);
         procedure AddList(list:TSearchPathList;addfirst:boolean);
         function  FindFile(const f : string;var b : boolean) : string;
       end;

    var
       { specified inputfile }
       inputdir       : dirstr;
       inputfile      : namestr;
       inputextension : extstr;
       { specified outputfile with -o parameter }
       outputfile     : namestr;
       { specified with -FE or -FU }
       outputexedir   : dirstr;
       outputunitdir  : dirstr;

       { things specified with parameters }
       paralinkoptions,
       paradynamiclinker : string;
       parapreprocess    : boolean;

       { directory where the utils can be found (options -FD) }
       utilsdirectory : dirstr;

       { some flags for global compiler switches }
       do_build,
       do_release,
       do_make       : boolean;
       not_unit_proc : boolean;
       { path for searching units, different paths can be seperated by ; }
       exepath            : dirstr;  { Path to ppc }
       librarysearchpath,
       unitsearchpath,
       objectsearchpath,
       includesearchpath  : TSearchPathList;

       { deffile }
       usewindowapi  : boolean;
       description   : string;
       dllversion    : string;
       dllmajor,dllminor : word;

       { current position }
       token,                        { current token being parsed }
       idtoken    : ttoken;          { holds the token if the pattern is a known word }
       tokenpos,                     { last postion of the read token }
       aktfilepos : tfileposinfo;    { current position }

       { type of currently parsed block }
       { isn't full implemented (FK)    }
       block_type : tblock_type;

       in_args : boolean;                { arguments must be checked especially }
       parsing_para_level : longint;     { parameter level, used to convert
                                             proc calls to proc loads in firstcalln }
       { Must_be_valid : boolean;           should the variable already have a value
        obsolete replace by set_varstate function }
       compile_level : word;
       make_ref : boolean;
       resolving_forward : boolean;      { used to add forward reference as second ref }
       use_esp_stackframe : boolean;     { to test for call with ESP as stack frame }
       inlining_procedure : boolean;     { are we inlining a procedure }

{$ifdef TP}
       use_big      : boolean;
{$endif}

     { commandline values }
       initdefines        : tlinkedlist;
       initglobalswitches : tglobalswitches;
       initmoduleswitches : tmoduleswitches;
       initlocalswitches  : tlocalswitches;
       initmodeswitches   : tmodeswitches;
       {$IFDEF testvarsets}
        Initsetalloc,                            {0=fixed, 1 =var}
       {$ENDIF}
       initpackenum       : longint;
       initpackrecords    : tpackrecords;
       initoutputformat   : tasm;
       initoptprocessor,
       initspecificoptprocessor : tprocessors;
       initasmmode        : tasmmode;
     { current state values }
       aktglobalswitches : tglobalswitches;
       aktmoduleswitches : tmoduleswitches;
       aktlocalswitches  : tlocalswitches;
       nextaktlocalswitches : tlocalswitches;
       localswitcheschanged : boolean;
       aktmodeswitches   : tmodeswitches;
       {$IFDEF testvarsets}
        aktsetalloc,
       {$ENDIF}
       aktpackenum       : longint;
       aktmaxfpuregisters: longint;
       aktpackrecords    : tpackrecords;
       aktoutputformat   : tasm;
       aktoptprocessor,
       aktspecificoptprocessor : tprocessors;
       aktasmmode        : tasmmode;

     { Memory sizes }
       heapsize,
       maxheapsize,
       stacksize    : longint;

{$Ifdef EXTDEBUG}
       total_of_firstpass,
       firstpass_several : longint;
     { parameter switches }
       debugstop,
       only_one_pass : boolean;
{$EndIf EXTDEBUG}
       { windows / OS/2 application type }
       apptype : tapptype;

    const
       RelocSection : boolean = true;
       RelocSectionSetExplicitly : boolean = false;
       LinkTypeSetExplicitly : boolean = false;
       DLLsource : boolean = false;
       DLLImageBase : pstring = nil;
       UseDeffileForExport : boolean = true;
       ForceDeffileForExport : boolean = false;

       { used to set all registers used for each global function
         this should dramatically decrease the number of
         recompilations needed PM }
       simplify_ppu : boolean = true;

       { should we allow non static members ? }
       allow_only_static : boolean = false;

       Inside_asm_statement : boolean = false;

    { for error info in pp.pas }
    const
       parser_current_file : string = '';

{$ifdef m68k}
       { PalmOS resources }
       palmos_applicationname : string = 'FPC Application';
       palmos_applicationid : string[4] = 'FPCA';
{$endif m68k}

{$ifdef debug}
    { if the pointer don't point to the heap then write an error }
    function assigned(p : pointer) : boolean;
{$endif}
    function min(a,b : longint) : longint;
    function max(a,b : longint) : longint;
    function SwapLong(x : longint): longint;
    function SwapWord(x : word): word;
    function align(i,a:longint):longint;
    function  data_align(length : longint) : longint;
    function align_from_size(datasize:longint;length:longint):longint;
    procedure Replace(var s:string;s1:string;const s2:string);
    procedure ReplaceCase(var s:string;const s1,s2:string);
    function upper(const s : string) : string;
    function lower(const s : string) : string;
    function trimspace(const s:string):string;
    {$ifdef FPC}
    function tostru(i:cardinal) : string;
    {$else}
    function tostru(i:longint) : string;
    {$endif}
    procedure uppervar(var s : string);
    function CompareText(S1, S2: string): longint;
    function hexstr(val : longint;cnt : byte) : string;
    function tostr(i : longint) : string;
    function tostr_with_plus(i : longint) : string;
    procedure valint(S : string;var V : longint;var code : integer);
    function is_number(const s : string) : boolean;
    function ispowerof2(value : longint;var power : longint) : boolean;
    { enable ansistring comparison }
    function compareansistrings(p1,p2 : pchar;length1,length2 : longint) : longint;
    function concatansistrings(p1,p2 : pchar;length1,length2 : longint) : pchar;
    function bstoslash(const s : string) : string;
    procedure abstract;
    function maybequoted(const s:string):string;
    function GetToken(var s:string;endchar:char):string;

    function getdatestr:string;
    function gettimestr:string;
    function filetimestring( t : longint) : string;

    procedure DefaultReplacements(var s:string);
    function  GetCurrentDir:string;
    function  path_absolute(const s : string) : boolean;
    Function  PathExists ( F : String) : Boolean;
    Function  FileExists ( Const F : String) : Boolean;
    Function  RemoveFile(const f:string):boolean;
    Function  RemoveDir(d:string):boolean;
    Function  GetFileTime ( Var F : File) : Longint;
    Function  GetNamedFileTime ( Const F : String) : Longint;
    Function  SplitPath(const s:string):string;
    Function  SplitFileName(const s:string):string;
    Function  SplitName(const s:string):string;
    Function  SplitExtension(Const HStr:String):String;
    Function  AddExtension(Const HStr,ext:String):String;
    Function  ForceExtension(Const HStr,ext:String):String;
    Function  FixPath(s:string;allowdot:boolean):string;
    function  FixFileName(const s:string):string;
    procedure SplitBinCmd(const s:string;var bstr,cstr:string);
    procedure SynchronizeFileTime(const fn1,fn2:string);
    function  FindFile(const f : string;path : string;var b : boolean) : string;
    function  FindExe(bin:string;var found:boolean):string;
    function  GetShortName(const n:string):string;

    Procedure Shell(const command:string);
    function  GetEnvPChar(const envname:string):pchar;
    procedure FreeEnvPChar(p:pchar);

    Function SetCompileMode(const s:string; changeInit: boolean):boolean;

    procedure InitGlobals;
    procedure DoneGlobals;

implementation

    uses
      comphook;

    procedure abstract;
      begin
        do_internalerror(255);
      end;


    procedure WarnNonExistingPath(const path : string);
      begin
        if assigned({$ifdef TP}@{$endif}do_comment) then
          do_comment(V_Hint,'Path "'+path+'" not found');
      end;

    function ngraphsearchvalue(const s1,s2 : string) : double;
      const
         n = 3;
      var
         equals,i,j : longint;
         hs : string;
      begin
         equals:=0;
         { is the string long enough ? }
         if min(length(s1),length(s2))-n+1<1 then
           begin
              ngraphsearchvalue:=0.0;
              exit;
           end;
         for i:=1 to length(s1)-n+1 do
           begin
              hs:=copy(s1,i,n);
              for j:=1 to length(s2)-n+1 do
                if hs=copy(s2,j,n) then
                  inc(equals);
           end;
{$ifdef fpc}
         ngraphsearchvalue:=equals/double(max(length(s1),length(s2))-n+1);
{$else}
         ngraphsearchvalue:=equals/(max(length(s1),length(s2))-n+1);
{$endif}
      end;


    function maybequoted(const s:string):string;
      var
        s1 : string;
        i  : integer;
        quoted : boolean;
      begin
        quoted:=false;
        s1:='"';
        for i:=1 to length(s) do
         begin
           case s[i] of
             '"' :
               begin
                 quoted:=true;
                 s1:=s1+'\"';
               end;
             ' ',
             #128..#255 :
               begin
                 quoted:=true;
                 s1:=s1+s[i];
               end;
             else
               s1:=s1+s[i];
           end;
         end;
        if quoted then
          maybequoted:=s1+'"'
        else
          maybequoted:=s;
      end;


    function GetToken(var s:string;endchar:char):string;
      var
        i : longint;
      begin
        GetToken:='';
        s:=TrimSpace(s);
        if s[1]='''' then
         begin
           i:=1;
           while (i<length(s)) do
            begin
              inc(i);
              if s[i]='''' then
               begin
                 { Remove double quote }
                 if (i<length(s)) and
                    (s[i+1]='''') then
                  begin
                    Delete(s,i,1);
                    inc(i);
                  end
                 else
                  begin
                    GetToken:=Copy(s,2,i-2);
                    Delete(s,1,i);
                    exit;
                  end;
               end;
            end;
           GetToken:=s;
           s:='';
         end
        else
         begin
           i:=pos(EndChar,s);
           if i=0 then
            begin
              GetToken:=s;
              s:='';
              exit;
            end
           else
            begin
              GetToken:=Copy(s,1,i-1);
              Delete(s,1,i);
              exit;
            end;
         end;
      end;


    function bstoslash(const s : string) : string;
    {
      return string s with all \ changed into /
    }
      var
         i : longint;
      begin
        for i:=1to length(s) do
         if s[i]='\' then
          bstoslash[i]:='/'
         else
          bstoslash[i]:=s[i];
         {$ifndef TP}
           {$ifopt H+}
             setlength(bstoslash,length(s));
           {$else}
             bstoslash[0]:=s[0];
           {$endif}
         {$else}
           bstoslash[0]:=s[0];
         {$endif}
      end;

{$ifdef debug}

    function assigned(p : pointer) : boolean;
{$ifndef FPC}
    {$ifndef DPMI}
      type
         ptrrec = record
            ofs,seg : word;
         end;
      var
         lp : longint;
    {$endif DPMI}
{$endif FPC}
      begin
{$ifdef FPC}
          { Assigned is used for procvar and
            stack stored temp records !! PM }
         (* if (p<>nil) {and
            ((p<heaporg) or
            (p>heapptr))} then
           do_internalerror(230); *)
{$else}
    {$ifdef DPMI}
         assigned:=(p<>nil);
         exit;
    {$else DPMI}
         if p=nil then
           lp:=0
         else
           lp:=longint(ptrrec(p).seg)*16+longint(ptrrec(p).ofs);
         if (lp<>0) and
            ((lp<longint(seg(heaporg^))*16+longint(ofs(heaporg^))) or
            (lp>longint(seg(heapptr^))*16+longint(ofs(heapptr^)))) then
           do_internalerror(230);
    {$endif DPMI}
{$endif FPC}
         assigned:=(p<>nil);
      end;
{$endif}


    function min(a,b : longint) : longint;
    {
      return the minimal of a and b
    }
      begin
         if a>b then
           min:=b
         else
           min:=a;
      end;


    function max(a,b : longint) : longint;
    {
      return the maximum of a and b
    }
      begin
         if a<b then
           max:=b
         else
           max:=a;
      end;

{*****************************************************************************
                             Endian Handling
*****************************************************************************}

Function SwapLong(x : longint): longint;
var
  y : word;
  z : word;
Begin
  y := (x shr 16) and $FFFF;
  y := ((y shl 8) and $FFFF) or ((y shr 8) and $ff);
  z := x and $FFFF;
  z := ((z shl 8) and $FFFF) or ((z shr 8) and $ff);
  SwapLong := (longint(z) shl 16) or longint(y);
End;


Function SwapWord(x : word): word;
var
  z : byte;
Begin
  z := (x shr 8) and $ff;
  x := x and $ff;
  x := (x shl 8);
  SwapWord := x or z;
End;


    function  data_align(length : longint) : longint;
      begin
         if (length>2) and (not (cs_littlesize in aktglobalswitches)) then
           data_align:=4
         else
          if length>1 then
           data_align:=2
         else
{$ifdef m68k}
          begin
             { 68000 always requires word alignment, even for byte variables
               on globals at least! }
             if (aktoptprocessor = MC68000) then
               data_align := 2
             else
               data_align := 1;
          end;
{$else}
           data_align:=1;
{$endif}
      end;


    function align_from_size(datasize:longint;length:longint):longint;

    {Increases the datasize with the required alignment; i.e. on pentium
     words should be aligned word; and dwords should be aligned dword.
     So for a word (len=2), datasize is increased to the nearest multiple
     of 2, and for len=4, datasize is increased to the nearest multiple of
     4.}

    var data_align:word;

    begin
        {$IFDEF I386}
        if length>2 then
            data_align:=4
        else if length>1 then
            data_align:=2
        else
            data_align:=1;
        {$ENDIF}
        {$IFDEF M68K}
        data_align:=2;
        {$ENDIF}
        align_from_size:=(datasize+data_align-1) and not(data_align-1);
    end;


    function align(i,a:longint):longint;
    {
      return value <i> aligned <a> boundary
    }
      begin
        align:=(i+a-1) and not(a-1);
      end;


    procedure Replace(var s:string;s1:string;const s2:string);
      var
         last,
         i  : longint;
      begin
        s1:=upper(s1);
        last:=0;
        repeat
          i:=pos(s1,upper(s));
          if i=last then
           i:=0;
          if (i>0) then
           begin
             Delete(s,i,length(s1));
             Insert(s2,s,i);
             last:=i;
           end;
        until (i=0);
      end;


    procedure ReplaceCase(var s:string;const s1,s2:string);
      var
         last,
         i  : longint;
      begin
        last:=0;
        repeat
          i:=pos(s1,s);
          if i=last then
           i:=0;
          if (i>0) then
           begin
             Delete(s,i,length(s1));
             Insert(s2,s,i);
             last:=i;
           end;
        until (i=0);
      end;


    function upper(const s : string) : string;
    {
      return uppercased string of s
    }
      var
         i  : longint;
      begin
         for i:=1 to length(s) do
          if s[i] in ['a'..'z'] then
           upper[i]:=char(byte(s[i])-32)
          else
           upper[i]:=s[i];
        upper[0]:=s[0];
      end;


    function lower(const s : string) : string;
    {
      return lowercased string of s
    }
      var
         i : longint;
      begin
         for i:=1 to length(s) do
          if s[i] in ['A'..'Z'] then
           lower[i]:=char(byte(s[i])+32)
          else
           lower[i]:=s[i];
        lower[0]:=s[0];
      end;


    procedure uppervar(var s : string);
    {
      uppercase string s
    }
      var
         i : longint;
      begin
         for i:=1 to length(s) do
          if s[i] in ['a'..'z'] then
           s[i]:=char(byte(s[i])-32);
      end;

    function CompareText(S1, S2: string): longint;
      begin
        UpperVar(S1);
        UpperVar(S2);
        if S1<S2 then
         CompareText:=-1
        else
         if S1>S2 then
          CompareText:= 1
        else
         CompareText:=0;
      end;

    function hexstr(val : longint;cnt : byte) : string;
      const
        HexTbl : array[0..15] of char='0123456789ABCDEF';
      var
        i : longint;
      begin
        hexstr[0]:=char(cnt);
        for i:=cnt downto 1 do
         begin
           hexstr[i]:=hextbl[val and $f];
           val:=val shr 4;
         end;
      end;

{$ifdef FPC}
   function tostru(i:cardinal):string;
   {
     return string of value i, but for cardinals
   }
      var
        hs : string;
      begin
        str(i,hs);
        tostru:=hs;
      end;
{$else FPC}
    function tostru(i:longint):string;
      begin
        tostru:=tostr(i);
      end;
{$endif FPC}


   function trimspace(const s:string):string;
   {
     return s with all leading and ending spaces and tabs removed
   }
     var
       i,j : longint;
     begin
       i:=length(s);
       while (i>0) and (s[i] in [#9,' ']) do
        dec(i);
       j:=1;
       while (j<i) and (s[j] in [#9,' ']) do
        inc(j);
       trimspace:=Copy(s,j,i-j+1);
     end;


   function tostr(i : longint) : string;
   {
     return string of value i
   }
     var
        hs : string;
     begin
        str(i,hs);
        tostr:=hs;
     end;


   function tostr_with_plus(i : longint) : string;
   {
     return string of value i, but always include a + when i>=0
   }
     var
        hs : string;
     begin
        str(i,hs);
        if i>=0 then
          tostr_with_plus:='+'+hs
        else
          tostr_with_plus:=hs;
     end;


    procedure valint(S : string;var V : longint;var code : integer);
    {
      val() with support for octal, which is not supported under tp7
    }
{$ifndef FPC}
      var
        vs : longint;
        c  : byte;
      begin
        if s[1]='%' then
          begin
             vs:=0;
             longint(v):=0;
             for c:=2 to length(s) do
               begin
                  if s[c]='0' then
                    vs:=vs shl 1
                  else
                  if s[c]='1' then
                    vs:=vs shl 1+1
                  else
                    begin
                      code:=c;
                      exit;
                    end;
               end;
             code:=0;
             longint(v):=vs;
          end
        else
         system.val(S,V,code);
      end;
{$else not FPC}
      begin
         system.val(S,V,code);
      end;
{$endif not FPC}


    function is_number(const s : string) : boolean;
    {
      is string a correct number ?
    }
      var
         w : integer;
         l : longint;
      begin
         valint(s,l,w);
         is_number:=(w=0);
      end;


    function ispowerof2(value : longint;var power : longint) : boolean;
    {
      return if value is a power of 2. And if correct return the power
    }
      var
         hl : longint;
         i : longint;
      begin
         hl:=1;
         ispowerof2:=true;
         for i:=0 to 31 do
           begin
              if hl=value then
                begin
                   power:=i;
                   exit;
                end;
              hl:=hl shl 1;
           end;
         ispowerof2:=false;
      end;


    { enable ansistring comparison }
    { 0 means equal }
    { 1 means p1 > p2 }
    { -1 means p1 < p2 }
    function compareansistrings(p1,p2 : pchar;length1,length2 : longint) : longint;

      var
         i,j : longint;
      begin
         compareansistrings:=0;
         j:=min(length1,length2);
         i:=0;
         while (i<j) do
          begin
            if p1[i]>p2[i] then
             begin
               compareansistrings:=1;
               exit;
             end
            else
             if p1[i]<p2[i] then
              begin
                compareansistrings:=-1;
                exit;
              end;
            inc(i);
          end;
         if length1>length2 then
          compareansistrings:=1
         else
          if length1<length2 then
           compareansistrings:=-1;
      end;


    function concatansistrings(p1,p2 : pchar;length1,length2 : longint) : pchar;
      var
         p : pchar;
      begin
         getmem(p,length1+length2+1);
         move(p1[0],p[0],length1);
         move(p2[0],p[length1],length2+1);
         concatansistrings:=p;
      end;


{****************************************************************************
                               Time Handling
****************************************************************************}

    Function L0(l:longint):string;
    {
      return the string of value l, if l<10 then insert a zero, so
      the string is always at least 2 chars '01','02',etc
    }
      var
        s : string;
      begin
        Str(l,s);
        if l<10 then
         s:='0'+s;
        L0:=s;
      end;


   function gettimestr:string;
   {
     get the current time in a string HH:MM:SS
   }
      var
        hour,min,sec,hsec : word;
      begin
{$ifdef delphi}
        dmisc.gettime(hour,min,sec,hsec);
{$else delphi}
        dos.gettime(hour,min,sec,hsec);
{$endif delphi}
        gettimestr:=L0(Hour)+':'+L0(min)+':'+L0(sec);
      end;


   function getdatestr:string;
   {
     get the current date in a string YY/MM/DD
   }
      var
        Year,Month,Day,Wday : Word;
      begin
{$ifdef delphi}
        dmisc.getdate(year,month,day,wday);
{$else}
        dos.getdate(year,month,day,wday);
{$endif}
        getdatestr:=L0(Year)+'/'+L0(Month)+'/'+L0(Day);
      end;


   function  filetimestring( t : longint) : string;
   {
     convert dos datetime t to a string YY/MM/DD HH:MM:SS
   }
     var
     {$ifndef hasunix}
       DT : DateTime;
     {$endif}
       Year,Month,Day,Hour,Min,Sec : Word;
     begin
       if t=-1 then
        begin
          FileTimeString:='Not Found';
          exit;
        end;
     {$ifndef hasunix}
       unpacktime(t,DT);
       Year:=dT.year;month:=dt.month;day:=dt.day;
       Hour:=dt.hour;min:=dt.min;sec:=dt.sec;
     {$else}
       EpochToLocal (t,year,month,day,hour,min,sec);
     {$endif}
       filetimestring:=L0(Year)+'/'+L0(Month)+'/'+L0(Day)+' '+L0(Hour)+':'+L0(min)+':'+L0(sec);
     end;


{****************************************************************************
                          Default Macro Handling
****************************************************************************}

     procedure DefaultReplacements(var s:string);
       begin
         { Replace some macro's }
         Replace(s,'$FPCVER',version_string);
         Replace(s,'$VERSION',version_string);
         Replace(s,'$FULLVERSION',full_version_string);
         Replace(s,'$FPCDATE',date_string);
         Replace(s,'$FPCTARGET',target_cpu_string);
         Replace(s,'$FPCCPU',target_cpu_string);
         Replace(s,'$TARGET',target_path);
         Replace(s,'$FPCOS',target_path);
       end;


{****************************************************************************
                               File Handling
****************************************************************************}

   function GetCurrentDir:string;
     var
       CurrentDir : string;
     begin
       GetDir(0,CurrentDir);
       GetCurrentDir:=FixPath(CurrentDir,false);
     end;


   function path_absolute(const s : string) : boolean;
   {
     is path s an absolute path?
   }
     begin
        path_absolute:=false;
{$ifdef unix}
        if (length(s)>0) and (s[1]='/') then
          path_absolute:=true;
{$else unix}
  {$ifdef amiga}
        if ((length(s)>0) and ((s[1]='\') or (s[1]='/'))) or (Pos(':',s) = length(s)) then
          path_absolute:=true;
  {$else}
        if ((length(s)>0) and ((s[1]='\') or (s[1]='/'))) or
           ((length(s)>2) and (s[2]=':') and ((s[3]='\') or (s[3]='/'))) then
          path_absolute:=true;
  {$endif amiga}
{$endif unix}
     end;

{$ifndef FPC}
    Procedure FindClose(var Info : SearchRec);
      Begin
      End;
{$endif not FPC}


    Function FileExists ( Const F : String) : Boolean;
{$ifndef delphi}
      Var
         Info : SearchRec;
         res : boolean;
{$endif}
      begin
{$ifdef delphi}
         res:=sysutils.FileExists(f);
{$else}
        findfirst(F,readonly+archive+hidden,info);
        res:=(doserror=0);
        findclose(Info);
{$endif delphi}
         FileExists:=res;
         if assigned({$ifdef TP}@{$endif}do_comment) then
          begin
            if res then
              do_comment(V_Tried,'Searching file '+F+'... found')
            else
              do_comment(V_Tried,'Searching file '+F+'... not found');
          end;
      end;


    Function PathExists ( F : String) : Boolean;
      Var
        Info : SearchRec;
        disk : byte;
      begin
        { these operating systems have dos type drives }
{$ifndef tp}
        { when compiling with the Borland compilers always check }
        if source_os.id in [os_m68k_atari,os_i386_go32v2,os_i386_go32v1,
                            os_i386_win32,os_i386_os2] then
{$endif}
        Begin
          if (Length(f)=3) and (F[2]=':') and (F[3] in ['/','\']) then
            begin
              if F[1] in ['A'..'Z'] then
                disk:=ord(F[1])-ord('A')+1
              else if F[1] in ['a'..'z'] then
                disk:=ord(F[1])-ord('a')+1
              else
                disk:=255;
              if disk=255 then
                PathExists:=false
              else
                PathExists:=(DiskSize(disk)<>-1);
              exit;
            end;
        end;
        if F[Length(f)] in ['/','\'] then
         Delete(f,length(f),1);
        findfirst(F,readonly+archive+hidden+directory,info);
        PathExists:=(doserror=0) and ((info.attr and directory)=directory);
        findclose(Info);
      end;


    Function RemoveFile(const f:string):boolean;
      var
        g : file;
      begin
        assign(g,f);
        {$I-}
         erase(g);
        {$I+}
        RemoveFile:=(ioresult=0);
      end;


    Function RemoveDir(d:string):boolean;
      begin
        if d[length(d)]=DirSep then
         Delete(d,length(d),1);
        {$I-}
         rmdir(d);
        {$I+}
        RemoveDir:=(ioresult=0);
      end;


    Function SplitPath(const s:string):string;
      var
        i : longint;
      begin
        i:=Length(s);
        while (i>0) and not(s[i] in ['/','\']) do
         dec(i);
        SplitPath:=Copy(s,1,i);
      end;


    Function SplitFileName(const s:string):string;
      var
        p : dirstr;
        n : namestr;
        e : extstr;
      begin
        FSplit(s,p,n,e);
        SplitFileName:=n+e;
      end;


    Function SplitName(const s:string):string;
      var
        i,j : longint;
      begin
        i:=Length(s);
        j:=Length(s);
        while (i>0) and not(s[i] in ['/','\']) do
         dec(i);
        while (j>0) and (s[j]<>'.') do
         dec(j);
        if j<=i then
         j:=255;
        SplitName:=Copy(s,i+1,j-(i+1));
      end;


    Function SplitExtension(Const HStr:String):String;
      var
        j : longint;
      begin
        j:=length(Hstr);
        while (j>0) and (Hstr[j]<>'.') do
         begin
           if hstr[j]=DirSep then
            j:=0
           else
            dec(j);
         end;
        if j=0 then
         j:=254;
        SplitExtension:=Copy(Hstr,j,255);
      end;


    Function AddExtension(Const HStr,ext:String):String;
      begin
        if (Ext<>'') and (SplitExtension(HStr)='') then
         AddExtension:=Hstr+Ext
        else
         AddExtension:=Hstr;
      end;


    Function ForceExtension(Const HStr,ext:String):String;
      var
        j : longint;
      begin
        j:=length(Hstr);
        while (j>0) and (Hstr[j]<>'.') do
         dec(j);
        if j=0 then
         j:=255;
        ForceExtension:=Copy(Hstr,1,j-1)+Ext;
      end;


    Function FixPath(s:string;allowdot:boolean):string;
      var
        i : longint;
      begin
        { Fix separator }
        for i:=1 to length(s) do
         if s[i] in ['/','\'] then
          s[i]:=DirSep;
        { Fix ending / }
        if (length(s)>0) and (s[length(s)]<>DirSep) and
           (s[length(s)]<>':') then
         s:=s+DirSep;
        { Remove ./ }
        if (not allowdot) and (s='.'+DirSep) then
         s:='';
        { return }
{$ifdef unix}
        FixPath:=s;
{$else}
        FixPath:=Lower(s);
{$endif}
      end;


   function FixFileName(const s:string):string;
     var
       i      : longint;
      {$ifdef unix}
       NoPath : boolean;
      {$endif unix}
     begin
      {$ifdef unix}
       NoPath:=true;
      {$endif unix}
       for i:=length(s) downto 1 do
        begin
          case s[i] of
      {$ifdef unix}
       '/','\' : begin
                   FixFileName[i]:='/';
                   NoPath:=false; {Skip lowercasing path: 'X11'<>'x11' }
                 end;
      'A'..'Z' : if NoPath then
                  FixFileName[i]:=char(byte(s[i])+32)
                 else
                  FixFileName[i]:=s[i];
      {$else}
        {$ifdef amiga}
          '/','\' : FixFileName[i]:='/';
          'A'..'Z' : FixFileName[i]:=char(byte(s[i])+32);
        {$else}
          '/' : FixFileName[i]:='\';
          'A'..'Z' : FixFileName[i]:=char(byte(s[i])+32);
        {$endif amiga}
      {$endif}
          else
           FixFileName[i]:=s[i];
          end;
        end;
       {$ifndef TP}
         {$ifopt H+}
           SetLength(FixFileName,length(s));
         {$else}
           FixFileName[0]:=s[0];
         {$endif}
       {$else}
         FixFileName[0]:=s[0];
       {$endif}
     end;


   procedure SplitBinCmd(const s:string;var bstr,cstr:string);
     var
       i : longint;
     begin
       i:=pos(' ',s);
       if i>0 then
        begin
          bstr:=Copy(s,1,i-1);
          cstr:=Copy(s,i+1,length(s)-i);
        end
       else
        begin
          bstr:=s;
          cstr:='';
        end;
     end;



   procedure TSearchPathList.AddPath(s:string;addfirst:boolean);
     var
       j        : longint;
       hs,hsd,
       CurrentDir,
       CurrPath : string;
       subdirfound : boolean;
       dir      : searchrec;
   {$IFDEF NEWST}
       hp       : PStringItem;
   {$ELSE}
       hp       : PStringQueueItem;
   {$ENDIF}

       procedure addcurrpath;
       begin
         if addfirst then
          begin
            Delete(currPath);
            Insert(currPath);
          end
         else
          begin
            { Check if already in path, then we don't add it }
            hp:=Find(currPath);
            if not assigned(hp) then
             Concat(currPath);
          end;
       end;

     begin
       if s='' then
        exit;
     { Support default macro's }
       DefaultReplacements(s);
     { get current dir }
       CurrentDir:=GetCurrentDir;
       repeat
         { get currpath }
         if addfirst then
          begin
            j:=length(s);
            while (j>0) and (s[j]<>';') do
             dec(j);
            CurrPath:=FixPath(Copy(s,j+1,length(s)-j),false);
            if j=0 then
             s:=''
            else
             System.Delete(s,j,length(s)-j+1);
          end
         else
          begin
            j:=Pos(';',s);
            if j=0 then
             j:=255;
            CurrPath:=FixPath(Copy(s,1,j-1),false);
            System.Delete(s,1,j);
          end;
         { fix pathname }
         if CurrPath='' then
          CurrPath:='.'+DirSep
         else
          begin
            CurrPath:=FixPath(FExpand(CurrPath),false);
            if (CurrentDir<>'') and (Copy(CurrPath,1,length(CurrentDir))=CurrentDir) then
{$ifndef AMIGA}
             CurrPath:='.'+DirSep+Copy(CurrPath,length(CurrentDir)+1,255);
{$else}
             CurrPath:=CurrentDir+Copy(CurrPath,length(CurrentDir)+1,255);
{$endif}
          end;
         { wildcard adding ? }
         if pos('*',currpath)>0 then
          begin
            if currpath[length(currpath)]=dirsep then
             hs:=Copy(currpath,1,length(CurrPath)-1)
            else
             hs:=currpath;
            hsd:=SplitPath(hs);
            findfirst(hs,directory,dir);
            subdirfound:=false;
            while doserror=0 do
             begin
               if (dir.name<>'.') and
                  (dir.name<>'..') and
                  ((dir.attr and directory)<>0) then
                begin
                  subdirfound:=true;
                  currpath:=hsd+dir.name+dirsep;
                  hp:=Find(currPath);
                  if not assigned(hp) then
                   AddCurrPath;
                end;
               findnext(dir);
             end;
            FindClose(dir);
            if not subdirfound then
              WarnNonExistingPath(currpath);
          end
         else
          begin
            if PathExists(currpath) then
             addcurrpath
            else
              WarnNonExistingPath(currpath);
          end;
       until (s='');
     end;


   procedure TSearchPathList.AddList(list:TSearchPathList;addfirst:boolean);
     var
       s : string;
       hl : TSearchPathList;
     {$IFDEF NEWST}
       hp,hp2 : PStringItem;
     {$ELSE}
       hp,hp2 : PStringQueueItem;
     {$ENDIF}
     begin
       if list.empty then
        exit;
       { create temp and reverse the list }
       if addfirst then
        begin
          hl.Init;
          hp:=list.first;
          while assigned(hp) do
           begin
             hl.insert(hp^.data^);
             hp:=hp^.next;
           end;
          while not hl.empty do
           begin
             s:=hl.Get;
             Delete(s);
             Insert(s);
           end;
          hl.done;
        end
       else
        begin
          hp:=list.first;
          while assigned(hp) do
           begin
             hp2:=Find(hp^.data^);
             { Check if already in path, then we don't add it }
             if not assigned(hp2) then
              Concat(hp^.data^);
             hp:=hp^.next;
           end;
        end;
     end;


   function TSearchPathList.FindFile(const f : string;var b : boolean) : string;
     Var
     {$IFDEF NEWST}
       p : PStringItem;
     {$ELSE}
       p : PStringQueueItem;
     {$ENDIF}
     begin
       FindFile:='';
       b:=false;
       p:=first;
       while assigned(p) do
        begin
          If FileExists(p^.data^+f) then
           begin
             FindFile:=p^.data^;
             b:=true;
             exit;
           end;
          p:=p^.next;
        end;
     end;


   Function GetFileTime ( Var F : File) : Longint;
   Var
   {$ifdef hasunix}
     Info : Stat;
   {$endif}
     L : longint;
   begin
   {$ifdef hasunix}
     FStat (F,Info);
     L:=Info.Mtime;
   {$else}
     GetFTime(f,l);
   {$endif}
     GetFileTime:=L;
   end;


   Function GetNamedFileTime (Const F : String) : Longint;
   begin
     GetNamedFileTime:=do_getnamedfiletime(F);
   end;


   {Touch Assembler and object time to ppu time is there is a ppufilename}
   procedure SynchronizeFileTime(const fn1,fn2:string);
   var
     f : file;
     l : longint;
   begin
     Assign(f,fn1);
     {$I-}
      reset(f,1);
     {$I+}
     if ioresult=0 then
      begin
        getftime(f,l);
        { just to be sure in case there are rounding errors }
        setftime(f,l);
        close(f);
        assign(f,fn2);
        {$I-}
         reset(f,1);
        {$I+}
        if ioresult=0 then
         begin
           setftime(f,l);
           close(f);
         end;
      end;
   end;


   function FindFile(const f : string;path : string;var b : boolean) : string;
      Var
        singlepathstring : string;
        i : longint;
     begin
     {$ifdef unix}
       for i:=1 to length(path) do
        if path[i]=':' then
       path[i]:=';';
     {$endif}
       b:=false;
       FindFile:='';
       repeat
         i:=pos(';',path);
         if i=0 then
           i:=256;
         singlepathstring:=FixPath(copy(path,1,i-1),false);
         delete(path,1,i);
         If FileExists (singlepathstring+f) then
           begin
             FindFile:=singlepathstring;
             b:=true;
             exit;
           end;
       until path='';
     end;

   function FindExe(bin:string;var found:boolean):string;
   begin
     bin:=FixFileName(AddExtension(bin,source_os.exeext));
{$ifdef delphi}
     FindExe:=GetShortName(FindFile(bin,'.;'+exepath+';'+dmisc.getenv('PATH'),found)+bin);
{$else delphi}
     FindExe:=GetShortName(FindFile(bin,'.;'+exepath+';'+dos.getenv('PATH'),found)+bin);
{$endif delphi}
   end;


    function GetShortName(const n:string):string;
{$ifdef win32}
      var
        hs,hs2 : string;
        i : longint;
{$endif}
{$ifdef go32v2}
      var
        hs : string;
{$endif}
      begin
        GetShortName:=n;
{$ifdef win32}
        hs:=n+#0;
        i:=Windows.GetShortPathName(@hs[1],@hs2[1],high(hs2));
        if (i>0) and (i<=high(hs2)) then
          begin
            hs2[0]:=chr(strlen(@hs2[1]));
            GetShortName:=hs2;
          end;
{$endif}
{$ifdef go32v2}
        hs:=n;
        if Dos.GetShortName(hs) then
         GetShortName:=hs;
{$endif}
      end;


 {****************************************************************************
                               OS Dependent things
 ****************************************************************************}

    function GetEnvPChar(const envname:string):pchar;
      {$ifdef win32}
      var
        s     : string;
        i,len : longint;
        hp,p,p2 : pchar;
      {$endif}
      {$ifdef os2}
      var
        P1, P2: PChar;
      {$endif}
      begin
      {$ifdef hasunix}
        GetEnvPchar:=
          {$ifdef VER1_0} linux. {$else} unix. {$endif}  Getenv(envname);
        {$define GETENVOK}
      {$endif}
      {$ifdef win32}
        GetEnvPchar:=nil;
        p:=GetEnvironmentStrings;
        hp:=p;
        while hp^<>#0 do
         begin
           s:=strpas(hp);
           i:=pos('=',s);
           len:=strlen(hp);
           if upper(copy(s,1,i-1))=upper(envname) then
            begin
              GetMem(p2,len-length(envname));
              Move(hp[i],p2^,len-length(envname));
              GetEnvPchar:=p2;
              break;
            end;
           { next string entry}
           hp:=hp+len+1;
         end;
        FreeEnvironmentStrings(p);
        {$define GETENVOK}
      {$endif}
      {$ifdef os2}
        if os_Mode = osOS2 then
        begin
         P1 := StrPNew (EnvName);
         if Assigned (P1) then
         begin
          if DosCalls.DosScanEnv (P1, P2) = 0 then
           GetEnvPChar := P2
          else
           GetEnvPChar := nil;
          StrDispose (P1);
         end else GetEnvPChar := nil;
        end else
         GetEnvPchar := StrPNew (Dos.GetEnv (EnvName));
       {$define GETENVOK}
      {$endif}
      {$ifdef GETENVOK}
        {$undef GETENVOK}
      {$else}
        GetEnvPchar:=StrPNew(Dos.Getenv(envname));
      {$endif}
      end;


    procedure FreeEnvPChar(p:pchar);
      begin
      {$ifndef hasunix}
       {$ifdef os2}
        if os_Mode <> osOS2 then
          StrDispose (P);
       {$else os2}
        StrDispose(p);
       {$endif}
      {$endif}
      end;

    Procedure Shell(const command:string);
      { This is already defined in the linux.ppu for linux, need for the *
        expansion under linux }
      {$ifdef hasunix}
      var       rc      : Integer;
      begin
        rc:=
        {$ifdef VER1_0} linux. {$else} unix. {$endif} Shell(command);
        if (rc<>0) then begin
          DosError:=8;
          WriteLN('Return-Code ',rc,' ',command); // print it, since wie can't give it back
         end else
          DosError:=0;
        // LastDosExitCode can't be written from here (perhaps this funtion shoul be moved to dos
        // This is not exactly like dos.Exec, but better then no error-Check
      end;
      {$else}
         {$ifdef amiga}
          begin
            exec('',command);
          end;
         {$else}
        var
        comspec : string;
      begin
        comspec:=getenv('COMSPEC');
        Exec(comspec,' /C '+command);
      end;
         {$endif amiga}
      {$endif}


    Function SetCompileMode(const s:string; changeInit: boolean):boolean;
      var
        b : boolean;
      begin
        b:=true;
        if s='DEFAULT' then
          aktmodeswitches:=initmodeswitches
        else
         if s='DELPHI' then
          aktmodeswitches:=delphimodeswitches
        else
         if s='TP' then
          aktmodeswitches:=tpmodeswitches
        else
         if s='FPC' then
          aktmodeswitches:=fpcmodeswitches
        else
         if s='OBJFPC' then
          aktmodeswitches:=objfpcmodeswitches
        else
         if s='GPC' then
          aktmodeswitches:=gpcmodeswitches
        else
         b:=false;

        if b and changeInit then
          initmodeswitches := aktmodeswitches;

        if b then
         begin
           { turn ansistrings on by default ? }
           if (m_delphi in aktmodeswitches) then
            begin
              include(aktlocalswitches,cs_ansistrings);
              if changeinit then
               include(initlocalswitches,cs_ansistrings);
            end
           else
            begin
              exclude(aktlocalswitches,cs_ansistrings);
              if changeinit then
               exclude(initlocalswitches,cs_ansistrings);
            end;
           { enum packing }
           if (m_tp7 in aktmodeswitches) then
            aktpackenum:=1
           else
            aktpackenum:=4;
           if changeinit then
            initpackenum:=aktpackenum;
         end;

        SetCompileMode:=b;
      end;


{****************************************************************************
                                    Init
****************************************************************************}

{$ifdef hasunix}
  {$define need_path_search}
{$endif hasunix}
{$ifdef os2}
  {$define need_path_search}
{$endif os2}

   procedure get_exepath;
     var
       hs1 : namestr;
       hs2 : extstr;
{$ifdef need_path_search}
       b: boolean;
{$endif}
     begin
{$ifdef delphi}
       exepath:=dmisc.getenv('PPC_EXEC_PATH');
{$else delphi}
       exepath:=dos.getenv('PPC_EXEC_PATH');
{$endif delphi}
       if exepath='' then
        fsplit(FixFileName(paramstr(0)),exepath,hs1,hs2);
{$ifndef VER0_99_15}
   {$ifdef need_path_search}
       if exepath='' then
        begin
          if pos(source_os.exeext,hs1) <>
               (length(hs1) - length(source_os.exeext)+1) then
            hs1 := hs1 + source_os.exeext;
      {$ifdef delphi}
          exepath := findfile(hs1,dmisc.getenv('PATH'),b);
      {$else delphi}
          exepath := findfile(hs1,dos.getenv('PATH'),b);
      {$endif delphi}
        end;
   {$endif need_path_search}
{$endif}
       exepath:=FixPath(exepath,false);
     end;



   procedure DoneGlobals;
     begin
       initdefines.done;
       if assigned(DLLImageBase) then
         StringDispose(DLLImageBase);
       RelocSection:=true;
       RelocSectionSetExplicitly:=false;
       DLLsource:=false;
       UseDeffileForExport:=true;
       librarysearchpath.Done;
       unitsearchpath.Done;
       objectsearchpath.Done;
       includesearchpath.Done;
     end;

   procedure InitGlobals;
     begin
      { set global switches }
        do_build:=false;
        do_release:=false;
        do_make:=true;
{$ifdef tp}
        use_big:=false;
{$endif tp}
       compile_level:=0;

      { Output }
        OutputFile:='';
        OutputExeDir:='';
        OutputUnitDir:='';

      { Utils directory }
        utilsdirectory:='';

      { Search Paths }
        librarysearchpath.Init;
        unitsearchpath.Init;
        includesearchpath.Init;
        objectsearchpath.Init;

      { Def file }
        usewindowapi:=false;
        description:='Compiled by FPC '+version_string+' - '+target_cpu_string;
        dllversion:='';

      { Init values }
        initmodeswitches:=fpcmodeswitches;
        initlocalswitches:=[cs_check_io];
        initmoduleswitches:=[cs_extsyntax,cs_browser];
        initglobalswitches:=[cs_check_unit_name,cs_link_static];
{$ifdef i386}
        initoptprocessor:=Class386;
        initspecificoptprocessor:=Class386;
        initpackenum:=4;
        {$IFDEF testvarsets}
        initsetalloc:=0;
        {$ENDIF}
        initpackrecords:=packrecord_2;
        initoutputformat:=target_asm.id;
        initasmmode:=asmmode_i386_att;
{$else not i386}
  {$ifdef m68k}
        initoptprocessor:=MC68020;
        include(initmoduleswitches,cs_fp_emulation);
        initpackenum:=4;
        {$IFDEF testvarsets}
         initsetalloc:=0;
        {$ENDIF}
        initpackrecords:=packrecord_2;
        initoutputformat:=target_asm.id;
        initasmmode:=asmmode_m68k_mot;
  {$endif m68k}
{$endif i386}
        initdefines.init;

      { memory sizes, will be overriden by parameter or default for target
        in options or init_parser }
        stacksize:=0;
        heapsize:=0;
        maxheapsize:=0;

      { compile state }
        in_args:=false;
        { must_be_valid:=true; obsolete PM }
        not_unit_proc:=true;

        apptype:=at_cui;
     end;

begin
  get_exepath;
end.
{
  $Log: globals.pas,v $
  Revision 1.1.2.41  2003/04/04 15:33:24  peter
    * also quoted names with hi-ascii values

  Revision 1.1.2.40  2003/01/19 14:55:48  carl
    * more amiga path fixes
    + some small tab fix

  Revision 1.1.2.39  2003/01/11 20:42:24  carl
    * bugfix of PathExists with drives

  Revision 1.1.2.38  2003/01/07 16:17:59  peter
    * gettoken added

  Revision 1.1.2.37  2003/01/05 11:39:46  peter
    * use maybequoted instead of getshortname

  Revision 1.1.2.36  2003/01/04 15:37:06  hajny
    * fix for EMX extender incompatibility

  Revision 1.1.2.35  2002/12/03 19:56:58  carl
     * fix for amiga compilation

  Revision 1.1.2.34  2002/11/27 19:33:26  carl
    * amiga path resolution bugfix (from Nils)

  Revision 1.1.2.33  2002/11/08 13:59:43  pierre
   + CheckMem unit added, to get better memory leak infos with -dEXTDEBUG

  Revision 1.1.2.32  2002/11/02 11:51:08  carl
    + Amiga support in FixFileName

  Revision 1.1.2.31  2002/10/29 15:55:25  pierre
   * cycle works again with -So -dTP options

  Revision 1.1.2.30  2002/10/18 21:55:37  carl
    * alignment of rtti information (m68k only)
    * bytes are also aligned when target = 68000
    + atari target

  Revision 1.1.2.29  2002/09/21 14:20:28  carl
    * default CPU is now 68020+ or higher
    * no longer limited in local stack spacr for 68020+ cpu
    * -m68000 / -m68020 option when assembler is called

  Revision 1.1.2.28  2002/09/04 14:33:42  pierre
   more info for debug report

  Revision 1.1.2.27  2002/07/31 10:49:26  marco
   * hasunix patches

  Revision 1.1.2.26  2002/07/16 14:23:24  pierre
   * Use GetShortName to try to avoid getting spaces in FindExe function

  Revision 1.1.2.25  2002/07/14 14:47:30  carl
    - fix compilation problems (revert back to old files)

  Revision 1.1.2.23  2001/10/04 11:35:20  pierre
   * fix for bug 1594 by using DiskSize

  Revision 1.1.2.22  2001/09/14 12:18:38  pierre
   * use default assembler output corresponding to default target

  Revision 1.1.2.21  2001/09/14 02:59:41  carl
  + added data_align routine from symsym.inc

  Revision 1.1.2.20  2001/09/04 15:45:36  pierre
   * fix SplitBinCmd if without arg, was unused until now

  Revision 1.1.2.19  2001/08/22 02:39:59  carl
  * replaced linux -> unix in some particular cases

  Revision 1.1.2.18  2001/08/17 16:16:40  florian
    + support for PalmOS added

  Revision 1.1.2.17  2001/08/04 11:04:11  peter
    * browcol has no depends on ide/fv

  Revision 1.1.2.16  2001/07/25 13:11:27  pierre
   + SwapLong and SwapWord added

  Revision 1.1.2.15  2001/06/14 16:42:01  pierre
   + -Sr option for releases, include file changes discarded

  Revision 1.1.2.14  2001/05/19 21:37:17  peter
    * simplify_ppu is default, saves recompiles and makes therefor the
      compiler more stable and faster with recompiles

  Revision 1.1.2.13  2001/03/13 21:00:59  peter
    * fixes to get it compiled with 1.1 (linux -> unix)

  Revision 1.1.2.12  2001/02/24 22:56:51  carl
  added cardinal definition for TP

  Revision 1.1.2.11  2001/02/24 03:18:14  carl
  Noag386bin define problem solved

  Revision 1.1.2.10  2001/02/09 23:05:31  peter
    * default packenum=1 for tp7 mode

  Revision 1.1.2.9  2001/02/01 22:31:54  florian
    + Solaris support for the compiler

  Revision 1.1.2.8  2001/01/20 18:35:06  hajny
    + APPTYPE support under OS/2, at_fs, GetEnvPChar for OS/2

  Revision 1.1.2.7  2001/01/13 00:04:29  peter
    * fixed findexe to also support already extension in name

  Revision 1.1.2.6  2000/09/27 21:20:04  peter
    * also set initlocalswitches in setcompilemode

  Revision 1.1.2.5  2000/09/26 10:36:11  jonas
    * initmodeswitches is changed is you change the compiler mode from the
      command line (the -S<x> switches didn't work anymore for changing the
      compiler mode)

  Revision 1.1.2.4  2000/09/24 21:36:26  peter
    + setcompilemode() routine

  Revision 1.1.2.3  2000/09/24 10:39:15  peter
    * fixed ^Z in previous log message

  Revision 1.1.2.2  2000/09/24 10:17:07  jonas
    + searching of exe in path also for OS/2
    * fixed searching of exe in path

  Revision 1.1.2.1  2000/08/12 15:29:52  peter
    * patch from Gabor for IDE to support memory stream reading

  Revision 1.1  2000/07/13 06:29:50  michael
  + Initial import

  Revision 1.67  2000/06/19 19:57:19  pierre
   * smart link is default on win32

  Revision 1.66  2000/06/18 18:05:54  peter
    * no binary value reading with % if not fpc mode
    * extended illegal char message with the char itself (Delphi like)

  Revision 1.65  2000/06/15 18:10:11  peter
    * first look for ppu in cwd and outputpath and after that for source
      in cwd
    * fixpath() for not linux makes path now lowercase so comparing paths
      with different cases (sometimes a drive letter could be
      uppercased) gives the expected results
    * sources_checked flag if there was already a full search for sources
      which aren't found, so another scan isn't done when checking for the
      sources only when recompile is needed

  Revision 1.64  2000/06/11 07:00:21  peter
    * fixed pchar->string conversion for delphi mode

  Revision 1.63  2000/05/12 08:58:51  pierre
   * adapted to Delphi 3

  Revision 1.62  2000/05/12 05:55:04  pierre
   * * get it to compile with Delphi by Kovacs Attila Zoltan

  Revision 1.61  2000/05/11 09:37:25  pierre
   * do not use upcase for strings, reported by Kovacs Attila Zoltan

  Revision 1.60  2000/05/04 20:46:17  peter
    * ansistrings are now default on for delphi mode, as most ppl expect
      this

  Revision 1.59  2000/05/03 14:36:57  pierre
   * fix for tests/test/testrang.pp bug

  Revision 1.58  2000/04/14 12:27:57  pierre
   * setfiletime to both files in synchronize

  Revision 1.57  2000/03/23 15:35:47  peter
    * $VERSION is now version_string
    + $FULLVERSION is now full_version_string

  Revision 1.56  2000/03/20 16:04:05  pierre
   * probably a fix for bug 615

  Revision 1.55  2000/03/08 15:39:45  daniel
    + Added align_from_size function as suggested by Peter.

  Revision 1.54  2000/02/28 17:23:57  daniel
  * Current work of symtable integration committed. The symtable can be
    activated by defining 'newst', but doesn't compile yet. Changes in type
    checking and oop are completed. What is left is to write a new
    symtablestack and adapt the parser to use it.

  Revision 1.53  2000/02/14 20:58:44  marco
   * Basic structures for new sethandling implemented.

  Revision 1.52  2000/02/10 11:45:48  peter
    * addpath fixed with list of paths when inserting at the beginning
    * if exepath=currentdir then it's not inserted in path list
    * searchpaths in ppc386.cfg are now added at the beginning of the
      list instead of at the end. (commandline is not changed)
    * check paths before inserting in list

  Revision 1.51  2000/02/09 13:22:53  peter
    * log truncated

  Revision 1.50  2000/01/26 14:31:03  marco
   * $VERSION is now also substituted in -F paths (that have subst active)

  Revision 1.49  2000/01/23 21:29:14  florian
    * CMOV support in optimizer (in define USECMOV)
    + start of support of exceptions in constructors

  Revision 1.48  2000/01/23 16:36:37  peter
    * better auto RTL dir detection

  Revision 1.47  2000/01/20 00:23:03  pierre
   * fix for GetShortName, now checks results from Win32

  Revision 1.46  2000/01/07 01:14:27  peter
    * updated copyright to 2000

  Revision 1.45  2000/01/07 00:08:09  peter
    * tp7 fix

  Revision 1.44  2000/01/06 15:48:59  peter
    * wildcard support for directory adding, this allows the use of units/*
      in ppc386.cfg

  Revision 1.43  2000/01/04 15:15:50  florian
    + added compiler switch $maxfpuregisters
    + fixed a small problem in secondvecn

  Revision 1.42  1999/12/22 01:01:48  peter
    - removed freelabel()
    * added undefined label detection in internal assembler, this prevents
      a lot of ld crashes and wrong .o files
    * .o files aren't written anymore if errors have occured
    * inlining of assembler labels is now correct

  Revision 1.41  1999/12/20 23:23:28  pierre
   + $description $version

  Revision 1.40  1999/12/20 21:42:34  pierre
    + dllversion global variable
    * FPC_USE_CPREFIX code removed, not necessary anymore
      as we use .edata direct writing by default now.

  Revision 1.39  1999/12/08 10:40:00  pierre
    + allow use of unit var in exports of DLL for win32
      by using direct export writing by default instead of use of DEFFILE
      that does not allow assembler labels that do not
      start with an underscore.
      Use -WD to force use of Deffile for Win32 DLL

  Revision 1.38  1999/12/06 18:21:03  peter
    * support !ENVVAR for long commandlines
    * win32/go32v2 write short pathnames to link.res so c:\Program Files\ is
      finally supported as installdir.

  Revision 1.37  1999/12/02 17:34:34  peter
    * preprocessor support. But it fails on the caret in type blocks

  Revision 1.36  1999/11/18 15:34:45  pierre
    * Notes/Hints for local syms changed to
      Set_varstate function

  Revision 1.35  1999/11/17 17:04:59  pierre
   * Notes/hints changes

  Revision 1.34  1999/11/15 17:42:41  pierre
   * -g disables reloc section for win32

  Revision 1.33  1999/11/12 11:03:50  peter
    * searchpaths changed to stringqueue object

  Revision 1.32  1999/11/09 23:34:46  pierre
   + resolving_forward boolean used for references

  Revision 1.31  1999/11/09 13:00:38  peter
    * define FPC_DELPHI,FPC_OBJFPC,FPC_TP,FPC_GPC
    * initial support for ansistring default with modes

}
