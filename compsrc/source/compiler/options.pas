{
    $Id: options.pas,v 1.1.2.64 2003/02/21 23:40:47 marco Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl and Peter Vreman

    Reads command line options and config files

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
unit options;

interface

uses
  globtype,globals,verbose;

type
  POption=^TOption;
  TOption=object
    FirstPass,
    NoPressEnter,
    DoWriteLogo : boolean;
    FileLevel : longint;
    ParaIncludePath,
    ParaUnitPath,
    ParaObjectPath,
    ParaLibraryPath : TSearchPathList;
    Constructor Init;
    Destructor Done;
    procedure WriteLogo;
    procedure WriteInfo;
    procedure WriteHelpPages;
    procedure QuickInfo(const s:string);
    procedure IllegalPara(const opt:string);
    function  Unsetbool(var opts:string; pos: Longint):boolean;
    procedure interpret_proc_specific_options(const opt:string);virtual;
    procedure interpret_option(const opt :string;ispara:boolean);
    procedure Interpret_envvar(const envname : string);
    procedure Interpret_file(const filename : string);
    procedure Read_Parameters;
    procedure parsecmd(cmd:string);
  end;

procedure read_arguments(cmd:string);


implementation

uses
{$ifdef Delphi}
  dmisc,
{$else Delphi}
  dos,
{$endif Delphi}
{$ifdef EXTDEBUG}
  checkmem,
{$endif EXTDEBUG}
  version,systems,
  cobjects,messages
{$ifdef m68k}
  ,cpubase
{$endif m68k}
{$ifdef BrowserLog}
  ,browlog
{$endif BrowserLog}
  ,cpuswtch
  ;

const
  page_size = 24;

var
  option     : poption;
  read_configfile,        { read config file, set when a cfgfile is found }
  disable_configfile,
  target_is_set : boolean;  { do not allow contradictory target settings }
  asm_is_set  : boolean; { -T also change initoutputformat if not set idrectly }
  fpcdir,
  ppccfg,
  ppcaltcfg,
  param_file    : string;   { file to compile specified on the commandline }

{****************************************************************************
                                 Defines
****************************************************************************}


procedure def_symbol(const s : string);
begin
  if s='' then
   exit;
  initdefines.concat(new(pstring_item,init(upper(s))));
  Message1(option_defining_symbol,s);
end;


procedure undef_symbol(const s : string);
var
  item,next : pstring_item;
begin
  if s='' then
   exit;
  item:=pstring_item(initdefines.first);
  Message1(option_undefining_symbol,s);
  while assigned(item) do
   begin
     if (item^.str^=s) then
      begin
        next:=pstring_item(item^.next);
        initdefines.remove(item);
        dispose(item,done);
        item:=next;
      end
     else
      if item<>pstring_item(item^.next) then
       item:=pstring_item(item^.next)
      else
       break;
   end;
end;


function check_symbol(const s:string):boolean;
var
  hp : pstring_item;
begin
  hp:=pstring_item(initdefines.first);
  while assigned(hp) do
   begin
     if (hp^.str^=s) then
      begin
        check_symbol:=true;
        exit;
      end;
     hp:=pstring_item(hp^.next);
   end;
  check_symbol:=false;
end;

procedure set_default_link_type;
begin
  if (target_os.id=os_i386_win32) then
    begin
      def_symbol('FPC_LINK_SMART');
      undef_symbol('FPC_LINK_STATIC');
      undef_symbol('FPC_LINK_DYNAMIC');
      initglobalswitches:=initglobalswitches+[cs_link_smart];
      initglobalswitches:=initglobalswitches-[cs_link_shared,cs_link_static];
    end
  else
    begin
      undef_symbol('FPC_LINK_SMART');
      def_symbol('FPC_LINK_STATIC');
      undef_symbol('FPC_LINK_DYNAMIC');
      initglobalswitches:=initglobalswitches+[cs_link_static];
      initglobalswitches:=initglobalswitches-[cs_link_shared,cs_link_smart];
    end;
end;



{****************************************************************************
                                 Toption
****************************************************************************}

procedure StopOptions;
begin
  if assigned(Option) then
   begin
     dispose(Option,Done);
     Option:=nil;
   end;
  DoneVerbose;
{$ifdef EXTDEBUG}
  PrintMemLost:=false;
{$endif EXTDEBUG}
  Stop;
end;


procedure Toption.WriteLogo;
var
  p : pchar;
begin
  p:=MessagePchar(option_logo);
  while assigned(p) do
   Comment(V_Normal,GetMsgLine(p));
end;


procedure Toption.WriteInfo;
var
  p : pchar;
begin
  p:=MessagePchar(option_info);
  while assigned(p) do
   Comment(V_Normal,GetMsgLine(p));
  StopOptions;
end;


procedure Toption.WriteHelpPages;

  function PadEnd(s:string;i:longint):string;
  begin
    while (length(s)<i) do
     s:=s+' ';
    PadEnd:=s;
  end;

var
  lastident,
  j,outline,
  ident,
  lines : longint;
  show  : boolean;
  opt   : string[32];
  input,
  s     : string;
  p     : pchar;
begin
  WriteLogo;
  Lines:=4;
  Message1(option_usage,paramstr(0));
  lastident:=0;
  p:=MessagePChar(option_help_pages);
  while assigned(p) do
   begin
   { get a line and reset }
     s:=GetMsgLine(p);
     ident:=0;
     show:=false;
   { parse options }
     case s[1] of
{$ifdef UNITALIASES}
      'a',
{$endif}
{$ifdef EXTDEBUG}
      'e',
{$endif EXTDEBUG}
{$ifdef i386}
      '3',
{$endif}
{$ifdef m68k}
      '6',
{$endif}
      '*' : show:=true;
     end;
     if show then
      begin
        case s[2] of
{$ifdef TP}
         't',
{$endif}
{$ifdef GDB}
         'g',
{$endif}
{$ifdef hasunix}
         'L',
{$endif}
{$ifdef os2}
         'O',
{$endif}
         '*' : show:=true;
        else
         show:=false;
        end;
      end;
   { now we may show the message or not }
     if show then
      begin
        case s[3] of
         '0' : begin
                 ident:=0;
                 outline:=0;
               end;
         '1' : begin
                 ident:=2;
                 outline:=7;
               end;
         '2' : begin
                 ident:=6;
                 outline:=11;
               end;
         '3' : begin
                 ident:=9;
                 outline:=6;
               end;
        end;
        j:=pos('_',s);
        opt:=Copy(s,4,j-4);
        if opt='*' then
         opt:=''
        else
        if opt=' ' then
         opt:=PadEnd(opt,outline)
        else
         opt:=PadEnd('-'+opt,outline);
        if (ident=0) and (lastident<>0) then
         begin
           Comment(V_Normal,'');
           inc(Lines);
         end;
      { page full ? }
        if (lines >= page_size - 1) then
         begin
           if not NoPressEnter then
            begin
              Message(option_help_press_enter);
              readln(input);
              if upper(input)='Q' then
               StopOptions;
            end;
           lines:=0;
         end;
        Comment(V_Normal,PadEnd('',ident)+opt+Copy(s,j+1,255));
        LastIdent:=Ident;
        inc(Lines);
      end;
   end;
  StopOptions;
end;


procedure Toption.QuickInfo(const s:string);
begin
  if source_os.newline=#13#10 then
    Write(s+#10)
  else
    Writeln(s);
  StopOptions;
end;


procedure Toption.IllegalPara(const opt:string);
begin
  Message1(option_illegal_para,opt);
  Message(option_help_pages_para);
  StopOptions;
end;


function Toption.Unsetbool(var opts:string; pos: Longint):boolean;
{ checks if the character after pos in Opts is a + or a - and returns resp.
  false or true. If it is another character (or none), it also returns false }
begin
  UnsetBool := false;
  if Length(Opts)>Pos then
   begin
    inc(Pos);
    UnsetBool := Opts[Pos] = '-';
    if Opts[Pos] in ['-','+']then
     delete(Opts,Pos,1);
   end;
end;


procedure TOption.interpret_proc_specific_options(const opt:string);
begin
end;


procedure TOption.interpret_option(const opt:string;ispara:boolean);
var
  code : integer;
  c    : char;
  more : string;
  major,minor : longint;
  error : integer;
  j,l  : longint;
  d    : DirStr;
  e    : ExtStr;
begin
  if opt='' then
   exit;

  { only parse define,undef,report,target,verbosity and link options the firsttime }
  if firstpass and
     not((opt[1]='-') and (opt[2] in ['i','d','v','V','T','u','n','X'])) then
   exit;

  case opt[1] of
 '-' : begin
         more:=Copy(opt,3,255);
         if firstpass then
           Message1(option_interpreting_firstpass_option,opt)
         else
           Message1(option_interpreting_option,opt);
         case opt[2] of
              '!' : initlocalswitches:=initlocalswitches+[cs_ansistrings];
              '?' : WriteHelpPages;
              'a' : begin
                      initglobalswitches:=initglobalswitches+[cs_asm_leave];
                      j:=1;
                      while j<=length(more) do
                       begin
                         case more[j] of
                          'l' : initglobalswitches:=initglobalswitches+[cs_asm_source];
                          'r' : initglobalswitches:=initglobalswitches+[cs_asm_regalloc];
                          't' : initglobalswitches:=initglobalswitches+[cs_asm_tempalloc];
                          '-' : initglobalswitches:=initglobalswitches-
                                  [cs_asm_leave, cs_asm_source,cs_asm_regalloc, cs_asm_tempalloc];
                         else
                           IllegalPara(opt);
                         end;
                         inc(j);
                       end;
                    end;
              'A' : begin
                      if set_string_asm(More) then
                       begin
                         initoutputformat:=target_asm.id;
                         asm_is_set:=true;
                       end
                      else
                       IllegalPara(opt);
                    end;
              'b' : begin
                      if UnsetBool(More,0) then begin
                        initmoduleswitches:=initmoduleswitches-[cs_browser];
                        initmoduleswitches:=initmoduleswitches-[cs_local_browser];
{$ifdef BrowserLog}
                        initglobalswitches:=initglobalswitches-[cs_browser_log];
{$endif}
                      end else begin
                        initmoduleswitches:=initmoduleswitches+[cs_browser];
{$ifdef BrowserLog}
                        initglobalswitches:=initglobalswitches+[cs_browser_log];
{$endif}
                      end;
                      if More<>'' then
                        if (More='l') or (More='l+') then
                          initmoduleswitches:=initmoduleswitches+[cs_local_browser]
                        else
                        if More='l-' then
                          initmoduleswitches:=initmoduleswitches-[cs_local_browser]
                        else
{$ifdef BrowserLog}
                          browserlog.elements_to_list^.insert(more);
{$else}
                          IllegalPara(opt);
{$endif}
                    end;
              'B' : do_build:=not UnSetBool(more,0);
              'C' : begin
                      j := 1;
                      while j <= length(more) Do
                        Begin
                          case more[j] of
                            'a' : Simplify_ppu:=not UnsetBool(More, j);
                            'h' :
                               begin
                                 val(copy(more,j+1,length(more)-j),heapsize,code);
                                 if (code<>0) or (heapsize>=67107840) or (heapsize<1024) then
                                  IllegalPara(opt);
                                 break;
                               end;
                            'i' : If UnsetBool(More, j) then
                                      initlocalswitches:=initlocalswitches-[cs_check_io]
                                  else initlocalswitches:=initlocalswitches+[cs_check_io];
                            'n' : If UnsetBool(More, j) then
                                      initglobalswitches:=initglobalswitches-[cs_link_extern]
                                  Else initglobalswitches:=initglobalswitches+[cs_link_extern];
                            'o' :
                              If UnsetBool(More, j) then
                                  initlocalswitches:=initlocalswitches-[cs_check_overflow]
                              Else
                                initlocalswitches:=initlocalswitches+[cs_check_overflow];
                            'r' :
                              If UnsetBool(More, j) then
                                  initlocalswitches:=initlocalswitches-[cs_check_range]
                              Else
                                initlocalswitches:=initlocalswitches+[cs_check_range];
                            'R' :
                              If UnsetBool(More, j) then
                                  initlocalswitches:=initlocalswitches-[cs_check_object_ext]
                              Else
                                initlocalswitches:=initlocalswitches+[cs_check_object_ext];
                            's' :
                               begin
                                 val(copy(more,j+1,length(more)-j),stacksize,code);
                                 if (code<>0) or (stacksize>=67107840) or (stacksize<1024) then
                                  IllegalPara(opt);
                                 break;
                               end;
                            't' :
                               If UnsetBool(More, j) then
                                   initlocalswitches:=initlocalswitches-[cs_check_stack]
                               Else
                                 initlocalswitches:=initlocalswitches+[cs_check_stack];
                            'D' :
                               If UnsetBool(More, j) then
                                   initmoduleswitches:=initmoduleswitches-[cs_create_dynamic]
                               Else
                                 initmoduleswitches:=initmoduleswitches+[cs_create_dynamic];
                            'X' :
                               If UnsetBool(More, j) then
                                   initmoduleswitches:=initmoduleswitches-[cs_create_smart]
                               Else
                                 initmoduleswitches:=initmoduleswitches+[cs_create_smart];
                            else
                               IllegalPara(opt);
                          end;
                          inc(j);
                        end;
                    end;
              'd' : def_symbol(more);
              'D' : begin
                      initglobalswitches:=initglobalswitches+[cs_link_deffile];
                      j:=1;
                      while j<=length(more) do
                       begin
                         case more[j] of
                          'd' : begin
                                  description:=Copy(more,j+1,255);
                                  break;
                                end;
                          'v' : begin
                                  dllversion:=Copy(more,j+1,255);
                                  l:=pos('.',dllversion);
                                  dllminor:=0;
                                  error:=0;
                                  if l>0 then
                                    begin
                                      valint(copy(dllversion,l+1,255),minor,error);
                                      if (error=0) and
                                         (minor>=0) and (minor<=$ffff) then
                                        dllminor:=minor
                                      else if error=0 then
                                        error:=1;
                                    end;
                                  if l=0 then l:=256;
                                  dllmajor:=1;
                                  if error=0 then
                                    valint(copy(dllversion,1,l-1),major,error);
                                  if (error=0) and (major>=0) and (major<=$ffff) then
                                    dllmajor:=major
                                  else if error=0 then
                                    error:=1;
                                  if error<>0 then
                                    Message1(scan_w_wrong_version_ignored,dllversion);
                                  break;
                                end;
                          'w' : usewindowapi:=true;
                          '-' : begin
                                  initglobalswitches:=initglobalswitches-
                                    [cs_link_deffile];
                                  usewindowapi:=false;
                                end;
                         else
                           IllegalPara(opt);
                         end;
                         inc(j);
                       end;
                    end;
              'e' : exepath:=FixPath(More,true);
              { Just used by RHIDE }
              'E' : if not UnsetBool(More, 0) then
                      initglobalswitches:=initglobalswitches+[cs_link_extern]
                    else
                      initglobalswitches:=initglobalswitches-[cs_link_extern];
              'F' : begin
                      c:=more[1];
                      Delete(more,1,1);
                      DefaultReplacements(More);
                      case c of
                       'D' : utilsdirectory:=FixPath(More,true);
                       'e' : SetRedirectFile(More);
                       'E' : OutputExeDir:=FixPath(More,true);
                       'i' : if ispara then
                              ParaIncludePath.AddPath(More,false)
                             else
                              includesearchpath.AddPath(More,true);
                       'g' : Message2(option_obsolete_switch_use_new,'-Fg','-Fl');
                       'l' : if ispara then
                              ParaLibraryPath.AddPath(More,false)
                             else
                              LibrarySearchPath.AddPath(More,true);
                       'L' : if More<>'' then
                              ParaDynamicLinker:=More
                             else
                              IllegalPara(opt);
                       'o' : if ispara then
                              ParaObjectPath.AddPath(More,false)
                             else
                              ObjectSearchPath.AddPath(More,true);
                       'r' : Msgfilename:=More;
                       'u' : if ispara then
                              ParaUnitPath.AddPath(More,false)
                             else
                              unitsearchpath.AddPath(More,true);
                       'U' : OutputUnitDir:=FixPath(More,true);
                      else
                        IllegalPara(opt);
                      end;
                    end;
              'g' : begin
                      if UnsetBool(More, 0) then
                        begin
                          initmoduleswitches:=initmoduleswitches-[cs_debuginfo];
                          initglobalswitches:=initglobalswitches-[cs_gdb_dbx];
                          initglobalswitches:=initglobalswitches-[cs_gdb_gsym];
                          initglobalswitches:=initglobalswitches-[cs_gdb_heaptrc];
                          initglobalswitches:=initglobalswitches-[cs_gdb_lineinfo];
                          initglobalswitches:=initglobalswitches-[cs_checkpointer];
{$ifdef EXTDEBUG}
                          only_one_pass:=false;
{$endif EXTDEBUG}
                        end
                      else
                      begin
{$ifdef GDB}
                         initmoduleswitches:=initmoduleswitches+[cs_debuginfo];
                         if not RelocSectionSetExplicitly then
                           RelocSection:=false;
                         j:=1;
                         while j<=length(more) do
                           begin
                             case more[j] of
                               'd' : if UnsetBool(More, j) then
                                       initglobalswitches:=initglobalswitches-[cs_gdb_dbx] else
                                       initglobalswitches:=initglobalswitches+[cs_gdb_dbx];
                               'g' : if UnsetBool(More, j) then
                                       initglobalswitches:=initglobalswitches-[cs_gdb_gsym] else
                                       initglobalswitches:=initglobalswitches+[cs_gdb_gsym];
                               'h' : if UnsetBool(More, j) then
                                       initglobalswitches:=initglobalswitches-[cs_gdb_heaptrc] else
                                       initglobalswitches:=initglobalswitches+[cs_gdb_heaptrc];
                               'l' : if UnsetBool(More, j) then
                                       initglobalswitches:=initglobalswitches-[cs_gdb_lineinfo] else
                                       initglobalswitches:=initglobalswitches+[cs_gdb_lineinfo];
                               'c' : if UnsetBool(More, j) then
                                       initglobalswitches:=initglobalswitches-[cs_checkpointer] else
                                       initglobalswitches:=initglobalswitches+[cs_checkpointer];
{$ifdef EXTDEBUG}
                               'p' : only_one_pass:=not UnsetBool(More, j);
{$endif EXTDEBUG}
                             else
                               IllegalPara(opt);
                             end;
                             inc(j);
                           end;
{$else GDB}
                         Message(option_no_debug_support);
                         Message(option_no_debug_support_recompile_fpc);
{$endif GDB}
                      end;
                    end;
              'h' : begin
                      NoPressEnter:=true;
                      WriteHelpPages;
                    end;
              'i' : if more='' then
                     WriteInfo
                    else
                     begin
                        { Specific info, which can be used in Makefiles }
                        case More[1] of
                          'S' : begin
                                  case More[2] of
                                   'O' : QuickInfo(source_os.shortname);
{$ifdef Delphi !!!!!!!!!}
                                   'P' : QuickInfo('unknown');
{$else}
                                   'P' : QuickInfo(source_cpu_string);
{$endif}
                                  end;
                                end;
                          'T' : begin
                                  case More[2] of
                                   'O' : QuickInfo(target_os.shortname);
                                   'P' : QuickInfo(target_cpu_string);
                                  end;
                                end;
                          'V' : QuickInfo(version_string);
                          'D' : QuickInfo(date_string);
                        else
                          IllegalPara(Opt);
                        end;
                     end;
              'I' : if ispara then
                     ParaIncludePath.AddPath(More,false)
                    else
                     includesearchpath.AddPath(More,false);
              'k' : if more<>'' then
                     ParaLinkOptions:=ParaLinkOptions+' '+More
                    else
                     IllegalPara(opt);
              'l' : DoWriteLogo:=not UnSetBool(more,0);
              'm' : parapreprocess:=not UnSetBool(more,0);
              'n' : if More='' then
                     begin
                       read_configfile:=false;
                       disable_configfile:=true;
                     end
                    else
                     IllegalPara(opt);
              'o' : if More<>'' then
                     Fsplit(More,d,OutputFile,e)
                    else
                     IllegalPara(opt);
              'p' : begin
                      if UnsetBool(More, 0) then
                        begin
                          initmoduleswitches:=initmoduleswitches-[cs_profile];
                          undef_symbol('FPC_PROFILE');
                        end
                      else
                        if Length(More)=0 then
                          IllegalPara(opt)
                        else
                        case more[1] of
                         'g' : if UnsetBool(more, 1) then
                                begin
                                  initmoduleswitches:=initmoduleswitches-[cs_profile];
                                  undef_symbol('FPC_PROFILE');
                                end
                               else
                                begin
                                  initmoduleswitches:=initmoduleswitches+[cs_profile];
                                  def_symbol('FPC_PROFILE');
                               end;
                        else
                          IllegalPara(opt);
                        end;
                    end;
{$ifdef hasunix}
              'P' : if UnsetBool(More, 0) then
                      initglobalswitches:=initglobalswitches-[cs_asm_pipe]
                    else
                      initglobalswitches:=initglobalswitches+[cs_asm_pipe];
{$endif hasunix}
              's' :
                    begin
                      if UnsetBool(More, 0) then
                        begin
                          initglobalswitches:=initglobalswitches-[cs_asm_extern,cs_link_extern];
                          if more<>'' then
                            IllegalPara(opt);
                        end
                      else
                        begin
                          initglobalswitches:=initglobalswitches+[cs_asm_extern,cs_link_extern];
                          if more='h' then
                            initglobalswitches:=initglobalswitches-[cs_link_on_target]
                          else if more='t' then
                            initglobalswitches:=initglobalswitches+[cs_link_on_target]
                          else if more<>'' then
                            IllegalPara(opt);
                        end;
                    end;
              'S' : begin
                      j:=1;
                      while j<=length(more) do
                        begin
                          case more[j] of
                           '2' : SetCompileMode('OBJFPC',true);
                           'a' : initlocalswitches:=InitLocalswitches+[cs_do_assertion];
                           'c' : initmoduleswitches:=initmoduleswitches+[cs_support_c_operators];
                           'd' : SetCompileMode('DELPHI',true);
                           'e' : begin
                                   SetErrorFlags(copy(more,j+1,length(more)));
                                   break;
                                 end;
                           'g' : initmoduleswitches:=initmoduleswitches+[cs_support_goto];
                           'h' : initlocalswitches:=initlocalswitches+[cs_ansistrings];
                           'i' : initmoduleswitches:=initmoduleswitches+[cs_support_inline];
                           'm' : initmoduleswitches:=initmoduleswitches+[cs_support_macro];
                           'o' : SetCompileMode('TP',true);
                           'p' : SetCompileMode('GPC',true);
                           's' : initglobalswitches:=initglobalswitches+[cs_constructor_name];
                           't' : initmoduleswitches:=initmoduleswitches+[cs_static_keyword];
                           'v' : Message1(option_obsolete_switch,'-Sv');
                           '-' : begin
                                   initglobalswitches:=initglobalswitches -
                                     [cs_constructor_name];
                                   initlocalswitches:=InitLocalswitches -
                                     [cs_do_assertion, cs_ansistrings];
                                   initmoduleswitches:=initmoduleswitches -
                                     [cs_support_c_operators, cs_support_goto,
                                      cs_support_inline, cs_support_macro,
                                      cs_static_keyword];
                                 end;
                          else
                           IllegalPara(opt);
                          end;
                          inc(j);
                        end;
                    end;
              'T' : begin
                      more:=Upper(More);
                      defaultreplacements(more);
                      more := upper(more);
                      if not target_is_set then
                       begin
                         {Remove non core targetname extra defines}
                         case target_info.target OF
                           target_i386_freebsd,
                           target_i386_netbsd,
                           target_m68k_netbsd,
         target_i386_openbsd,
         target_m68k_openbsd:
                             begin
			       {$ifdef DOTARGETOPENBSDAOUT}
				 undef_symbol('DOTARGETOPENBSDAOUT');
			       {$endif}	
                               { undef_symbol('LINUX');}
                               undef_symbol('HASUNIX');
                               undef_symbol('BSD');
                               undef_symbol('UNIX');
                             end;
         target_i386_os2: begin
                            undef_symbol('EMX');
                          end;
         target_i386_linux,
         target_m68k_linux:
                            Begin
                               undef_symbol('UNIX');
                              undef_symbol('HASUNIX');
                             End;
                           target_i386_BeOS :
                              undef_symbol('UNIX');
                           target_i386_sunos:
                            begin
                              undef_symbol('UNIX');
                              undef_symbol('SOLARIS');
                            end;
                           target_i386_qnx:
                            begin
                              undef_symbol('UNIX');
                              undef_symbol('QNX');
                            end;
                         end;

                       { remove old target define }
                         undef_symbol(target_info.short_name);
                       { load new target }
                         if not(set_string_target(More)) then
                           IllegalPara(opt);
                       { set new define }
                         def_symbol(target_info.short_name);
                         if not asm_is_set then
                           initoutputformat:=target_asm.id;
                         target_is_set:=true;
                       end
                      else
                       if More<>target_info.short_name then
                        Message1(option_target_is_already_set,target_info.short_name);
                    end;
              'u' : undef_symbol(upper(More));
              'U' : begin
                      j:=1;
                      while j<=length(more) do
                       begin
                         case more[j] of
{$ifdef UNITALIASES}
                           'a' : begin
                                   AddUnitAlias(Copy(More,j+1,255));
                                   break;
                                 end;
{$endif UNITALIASES}
                           'n' : initglobalswitches:=initglobalswitches-[cs_check_unit_name];
                           'p' : begin
                                   Message2(option_obsolete_switch_use_new,'-Up','-Fu');
                                   break;
                                 end;
                           'r' : do_release:=true;
                           's' : initmoduleswitches:=initmoduleswitches+[cs_compilesystem];
                           '-' : begin
                                   initmoduleswitches:=initmoduleswitches
                                     - [cs_compilesystem];
                                   initglobalswitches := initglobalswitches +
                                      [cs_check_unit_name];
                                 end;
                          else
                            IllegalPara(opt);
                          end;
                         inc(j);
                       end;
                    end;

              'v' : if not setverbosity(More) then
                     IllegalPara(opt);

              'V' : PrepareReport;

              'W' : begin
                      j:=0;
                      while j<length(More) do
                      begin
                       inc(J);
                       case More[j] of
                        'B': {bind_win32_dll:=true}
                             begin
                               {  -WB200000 means set prefered base address
                                 to $200000, but does not change relocsection boolean
                                 this way we can create both relocatble and
                                 non relocatable DLL at a specific base address PM }
                               if (length(More)>j) then
                                 begin
                                   if DLLImageBase=nil then
                                     DLLImageBase:=StringDup(Copy(More,j+1,255));
                                 end
                               else
                                 begin
                                   RelocSection:=true;
                                   RelocSectionSetExplicitly:=true;
                                 end;
                               break;
                             end;
                        'C': if UnsetBool(More, j) then
                               apptype:=at_gui else
                               apptype:=at_cui;
                        'D': ForceDeffileForExport:= not UnsetBool(More, j);
                        'F': apptype:=at_fs;
                        'G': if UnsetBool(More, j) then
                               apptype:=at_cui else
                               apptype:=at_gui;
                        'N': begin
                               RelocSection:=UnsetBool(More,j);
                               RelocSectionSetExplicitly:=true;
                             end;

                        'R': begin
                               RelocSection:=not UnsetBool(More,j);
                               RelocSectionSetExplicitly:=true;
                             end;
                       else
                        IllegalPara(opt);
                       end;
                      end; {of while}
                    end;
              'X' : begin
                      j:=1;
                      while j<=length(more) do
                       begin
                          case More[j] of
                           'c' : initglobalswitches:=initglobalswitches+[cs_link_toc];
                           's' : initglobalswitches:=initglobalswitches+[cs_link_strip];
                           't' : initglobalswitches:=initglobalswitches+[cs_link_staticflag];
                           'D' : begin
                                   def_symbol('FPC_LINK_DYNAMIC');
                                   undef_symbol('FPC_LINK_SMART');
                                   undef_symbol('FPC_LINK_STATIC');
                                   initglobalswitches:=initglobalswitches+[cs_link_shared];
                                   initglobalswitches:=initglobalswitches-[cs_link_static,cs_link_smart];
                                   LinkTypeSetExplicitly:=true;
                                 end;
                           'S' : begin
                                   def_symbol('FPC_LINK_STATIC');
                                   undef_symbol('FPC_LINK_SMART');
                                   undef_symbol('FPC_LINK_DYNAMIC');
                                   initglobalswitches:=initglobalswitches+[cs_link_static];
                                   initglobalswitches:=initglobalswitches-[cs_link_shared,cs_link_smart];
                                   LinkTypeSetExplicitly:=true;
                                 end;
                           'X' : begin
                                   def_symbol('FPC_LINK_SMART');
                                   undef_symbol('FPC_LINK_STATIC');
                                   undef_symbol('FPC_LINK_DYNAMIC');
                                   initglobalswitches:=initglobalswitches+[cs_link_smart];
                                   initglobalswitches:=initglobalswitches-[cs_link_shared,cs_link_static];
                                   LinkTypeSetExplicitly:=true;
                                 end;
                           '-' : begin
                                   initglobalswitches:=initglobalswitches-[cs_link_toc, cs_link_strip, cs_link_staticflag];
                                   set_default_link_type;
                                 end;
                          else
                            IllegalPara(opt);
                          end;
                         inc(j);
                       end;
                    end;
       { give processor specific options a chance }
         else
          interpret_proc_specific_options(opt);
         end;
       end;
 '@' : begin
         Message(option_no_nested_response_file);
         StopOptions;
       end;
  else
   begin
     if (length(param_file)<>0) then
       Message(option_only_one_source_support);
     param_file:=opt;
     Message1(option_found_file,opt);
   end;
  end;
end;


procedure Toption.Interpret_file(const filename : string);

  procedure RemoveSep(var fn:string);
  var
    i : longint;
  begin
    i:=0;
    while (i<length(fn)) and (fn[i+1] in [',',' ',#9]) do
      inc(i);
    Delete(fn,1,i);
    i:=length(fn);
    while (i>0) and (fn[i] in [',',' ',#9]) do
      dec(i);
    fn:=copy(fn,1,i);
  end;

  function GetName(var fn:string):string;
  var
    i : longint;
  begin
    i:=0;
    while (i<length(fn)) and (fn[i+1] in ['a'..'z','A'..'Z','0'..'9','_','-']) do
     inc(i);
    GetName:=Copy(fn,1,i);
    Delete(fn,1,i);
  end;

const
  maxlevel=16;
var
  f     : text;
  s,
  opts  : string;
  skip  : array[0..maxlevel-1] of boolean;
  level : longint;
  option_read : boolean;
begin
{ avoid infinite loop }
  Inc(FileLevel);
  Option_read:=false;
  If FileLevel>MaxLevel then
   Message(option_too_many_cfg_files);
{ open file }
  assign(f,filename);
{$ifdef extdebug}
  Comment(V_Info,'trying to open file: '+filename);
{$endif extdebug}
  {$I-}
  reset(f);
  {$I+}
  if ioresult<>0 then
   begin
     Message1(option_unable_open_file,filename);
     exit;
   end
  else
    Message1(option_start_reading_configfile,filename);
  fillchar(skip,sizeof(skip),0);
  level:=0;
  while not eof(f) do
   begin
     readln(f,opts);
     RemoveSep(opts);
     if (opts<>'') and (opts[1]<>';') then
      begin
        if opts[1]='#' then
         begin
           Message1(option_interpreting_file_option,opts);
           Delete(opts,1,1);
           s:=upper(GetName(opts));
           if (s='SECTION') then
            begin
              RemoveSep(opts);
              s:=upper(GetName(opts));
              if level=0 then
               skip[level]:=not (check_symbol(s) or (s='COMMON'));
            end
           else
            if (s='IFDEF') then
             begin
               RemoveSep(opts);
               if Level>=maxlevel then
                begin
                  Message(option_too_many_ifdef);
                  stopOptions;
                end;
               inc(Level);
               skip[level]:=(skip[level-1] or (not check_symbol(upper(GetName(opts)))));
             end
           else
            if (s='IFNDEF') then
             begin
               RemoveSep(opts);
               if Level>=maxlevel then
                begin
                  Message(option_too_many_ifdef);
                  stopOptions;
                end;
               inc(Level);
               skip[level]:=(skip[level-1] or (check_symbol(upper(GetName(opts)))));
             end
           else
            if (s='ELSE') then
             skip[level]:=skip[level-1] or (not skip[level])
           else
            if (s='ENDIF') then
             begin
               skip[level]:=false;
               if Level=0 then
                begin
                  Message(option_too_many_endif);
                  stopOptions;
                end;
               dec(level);
             end
           else
            if (not skip[level]) then
             begin
               if (s='DEFINE') then
                begin
                  RemoveSep(opts);
                  def_symbol(upper(GetName(opts)));
                end
              else
               if (s='UNDEF') then
                begin
                  RemoveSep(opts);
                  undef_symbol(upper(GetName(opts)));
                end
              else
               if (s='WRITE') then
                begin
                  Delete(opts,1,1);
                  WriteLn(opts);
                end
              else
               if (s='INCLUDE') then
                begin
                  Delete(opts,1,1);
                  Interpret_file(opts);
                end;
            end;
         end
        else
         begin
           if (opts[1]='-') or (opts[1]='@') then
            begin
              if (not skip[level]) then
                interpret_option(opts,false);
              Option_read:=true;
            end
           else
             Message1(option_illegal_para,opts);
         end;
      end;
   end;
  if Level>0 then
   Message(option_too_less_endif);
  if Not Option_read then
    Message1(option_no_option_found,filename)
  else
    Message1(option_end_reading_configfile,filename);
  Close(f);
  Dec(FileLevel);
end;


procedure Toption.Interpret_envvar(const envname : string);
var
  argstart,
  env,
  pc     : pchar;
  arglen : longint;
  quote  : set of char;
  hs     : string;
begin
  env:=GetEnvPChar(envname);
  pc:=env;
  if assigned(pc) then
   begin
     repeat
       { skip leading spaces }
       while pc^ in [' ',#9,#13] do
        inc(pc);
       case pc^ of
         #0 :
           break;
         '"' :
           begin
             quote:=['"'];
             inc(pc);
           end;
         '''' :
           begin
              quote:=[''''];
              inc(pc);
           end;
         else
           quote:=[' ',#9,#13];
       end;
     { scan until the end of the argument }
       argstart:=pc;
       while (pc^<>#0) and not(pc^ in quote) do
        inc(pc);
     { create argument }
       arglen:=pc-argstart;
       hs[0]:=chr(arglen);
       move(argstart^,hs[1],arglen);
       interpret_option(hs,true);
     { skip quote }
       if pc^ in quote then
        inc(pc);
     until false;
   end
  else
   Message1(option_no_option_found,'(env) '+envname);
  FreeEnvPChar(env);
end;


procedure toption.read_parameters;
var
  opts       : string;
  paramindex : longint;
begin
  paramindex:=0;
  while paramindex<paramcount do
   begin
     inc(paramindex);
     opts:=paramstr(paramindex);
     case opts[1] of
       '@' :
         if not firstpass then
         begin
           Delete(opts,1,1);
           Message1(option_reading_further_from,opts);
           interpret_file(opts);
         end;
       '!' :
         if not firstpass then
         begin
           Delete(opts,1,1);
           Message1(option_reading_further_from,'(env) '+opts);
           interpret_envvar(opts);
         end;
       else
         interpret_option(opts,true);
     end;
   end;
end;



procedure toption.parsecmd(cmd:string);
var
  i,ps  : longint;
  opts  : string;
begin
  while (cmd<>'') do
   begin
     while cmd[1]=' ' do
      delete(cmd,1,1);
     i:=pos(' ',cmd);
     if i=0 then
      i:=256;
     opts:=Copy(cmd,1,i-1);
     Delete(cmd,1,i);
     case opts[1] of
       '@' :
         if not firstpass then
         begin
           Delete(opts,1,1);
           Message1(option_reading_further_from,opts);
           interpret_file(opts);
         end;
       '!' :
         if not firstpass then
         begin
           Delete(opts,1,1);
           Message1(option_reading_further_from,'(env) '+opts);
           interpret_envvar(opts);
         end;
       '"' :
         begin
           Delete(opts,1,1);
           ps:=pos('"',cmd);
           if (i<>256) and (ps>0) then
             begin
               opts:=opts + ' '+ copy(cmd,1,ps-1);
               cmd:=copy(cmd,ps+1,255);
             end;
           interpret_option(opts,true);
         end;
       else
         interpret_option(opts,true);
     end;
   end;
end;


constructor TOption.Init;
begin
  DoWriteLogo:=false;
  NoPressEnter:=false;
  FirstPass:=false;
  FileLevel:=0;
  ParaIncludePath.Init;
  ParaObjectPath.Init;
  ParaUnitPath.Init;
  ParaLibraryPath.Init;
end;


destructor TOption.Done;
begin
  ParaIncludePath.Done;
  ParaObjectPath.Done;
  ParaUnitPath.Done;
  ParaLibraryPath.Done;
end;


function IfFileExists(Const F : String) : Boolean;
begin
  IfFileExists:=Globals.FileExists(F);
end;

{****************************************************************************
                              Callable Routines
****************************************************************************}

procedure read_arguments(cmd:string);
var
  configpath : pathstr;
  config_file_found : boolean;
begin
{$ifdef Delphi}
  option:=new(poption386,Init);
{$endif Delphi}
{$ifdef i386}
  option:=new(poption386,Init);
{$endif}
{$ifdef m68k}
  option:=new(poption68k,Init);
{$endif}
{$ifdef alpha}
  option:=new(poption,Init);
{$endif}
{$ifdef powerpc}
  option:=new(poption,Init);
{$endif}

  config_file_found:=false;
  read_configfile:=true;
  disable_configfile:=false;

  option^.firstpass:=true;
  if cmd<>'' then
    option^.parsecmd(cmd)
  else
    option^.read_parameters;

{ default defines }
  def_symbol(target_info.short_name);
  def_symbol('FPK');
  def_symbol('FPC');
  def_symbol('VER'+version_nr);
  def_symbol('VER'+version_nr+'_'+release_nr);
  def_symbol('VER'+version_nr+'_'+release_nr+'_'+patch_nr);
{$ifdef newcg}
  def_symbol('WITHNEWCG');
{$endif}




{ Temporary defines, until things settle down }
  def_symbol('INT64');
  def_symbol('HASRESOURCESTRINGS');
  def_symbol('NEWVMTOFFSET');
  def_symbol('SYSTEMTVARREC');
  def_symbol('INCLUDEOK');
  def_symbol('NEWMM');
  def_symbol('HASWIDECHAR');
  def_symbol('INT64FUNCRESOK');


{$ifdef SUPPORT_FIXED}
  def_symbol('HASFIXED');
{$endif SUPPORT_FIXED}
  def_symbol('CORRECTFLDCW');
  def_symbol('ENHANCEDRAISE');
  def_symbol('PACKENUMFIXED');

{ some stuff for TP compatibility }
{$ifdef i386}
  def_symbol('CPU86');
  def_symbol('CPU87');
{$endif}
{$ifdef m68k}
  def_symbol('CPU68');
{$endif}

{ new processor stuff }
{$ifdef i386}
  def_symbol('CPUI386');
{$endif}
{$ifdef m68k}
  def_symbol('CPU68K');
{$endif}
{$ifdef ALPHA}
  def_symbol('CPUALPHA');
{$endif}
{$ifdef powerpc}
  def_symbol('CPUPOWERPC');
{$endif}

{ get default messagefile }
{$ifdef Delphi}
  msgfilename:=dmisc.getenv('PPC_ERROR_FILE');
{$else Delphi}
  msgfilename:=dos.getenv('PPC_ERROR_FILE');
{$endif Delphi}
{ default configfile }
  if (cmd<>'') and (cmd[1]='[') then
   begin
     ppccfg:=Copy(cmd,2,pos(']',cmd)-2);
     Delete(cmd,1,pos(']',cmd));
   end
  else
   begin
     ppcaltcfg:='ppc386.cfg';
     ppccfg:='fpc.cfg';
   end;

{ Order to read configuration file :
  try reading ppc386.cfg in :
   1 - current dir
   2 - configpath
   3 - compiler path
  else try reading ppc.cfg in :
   1 - current dir
   2 - configpath
   3 - compiler path
}
{$ifdef Delphi}
  configpath:=FixPath(dmisc.getenv('PPC_CONFIG_PATH'),false);
{$else Delphi}
  configpath:=FixPath(dos.getenv('PPC_CONFIG_PATH'),false);
{$endif Delphi}
{$ifdef Unix}
  if configpath='' then
   configpath:='/etc/';
{$endif}
  if read_configfile and (ppccfg<>'') then
   begin
     if not IfFileExists(ppcaltcfg) then
      begin
        config_file_found := true;
{$ifdef Unix}
        if (dos.getenv('HOME')<>'') and IfFileExists(FixPath(dos.getenv('HOME'),false)+'.'+ppcaltcfg) then
         ppccfg:=FixPath(dos.getenv('HOME'),false)+'.'+ppcaltcfg
        else
{$endif}
         if IfFileExists(configpath+ppcaltcfg) then
          ppccfg:=configpath+ppcaltcfg
        else
{$ifndef Unix}
         if IfFileExists(exepath+ppcaltcfg) then
          ppccfg:=exepath+ppcaltcfg
        else
{$endif}
         config_file_found := false;
      end
     else
        ppccfg := ppcaltcfg;  { file is found, then set it to ppccfg }


     if read_configfile and not config_file_found then
      begin
        config_file_found := true;
        if not IfFileExists(ppccfg) then
         begin
    {$ifdef Unix}
            if (dos.getenv('HOME')<>'') and IfFileExists(FixPath(dos.getenv('HOME'),false)+'.'+ppccfg) then
             ppccfg:=FixPath(dos.getenv('HOME'),false)+'.'+ppccfg
            else
    {$endif}
             if IfFileExists(configpath+ppccfg) then
              ppccfg:=configpath+ppccfg
            else
    {$ifndef Unix}
             if IfFileExists(exepath+ppccfg) then
              ppccfg:=exepath+ppccfg
            else
    {$endif}
             config_file_found := false;
         end;
      end
   end
  else
    config_file_found := false;


{ Read commandline and configfile }
  target_is_set:=false;
  asm_is_set:=false;

  param_file:='';

  option^.firstpass:=false;

  if read_configfile  and config_file_found then
   begin
     Message1(option_read_config_file,ppccfg);
     option^.interpret_file(ppccfg);
   end;

  if cmd<>'' then
    option^.parsecmd(cmd)
  else
    option^.read_parameters;

{ Write help pages }
  if (cmd='') and (paramcount=0) then
   Option^.WriteHelpPages;

{ Stop if errors in options }
  if ErrorCount>0 then
   StopOptions;

{ write logo if set }
  if option^.DoWriteLogo then
   option^.WriteLogo;

{ Hack: Linux define is also needed for freebsd (MvdV).
          This should be solved now. (MvdV)
  Needs to be here because of -T? }
 if (target_info.target in [target_i386_freebsd,
       target_i386_netbsd,target_m68k_netbsd,target_i386_openbsd,target_m68k_openbsd]) then
  begin
{   def_symbol('LINUX');}
   def_symbol('UNIX');    { generic UNIX }
   def_symbol('HASUNIX'); { Linux + BSD (syscall Unix unit) }
   def_symbol('BSD');     { All BSDs. Makes it easy to kill Linuxisms }

   { Temporary workaround for as long OpenBSD is aout. If this define is 
      set, propagate to next iteration}
   {$ifdef DOTARGETOPENBSDAOUT}
    def_symbol('DOTARGETOPENBSDAOUT');
   {$endif}	

   if (target_info.target=target_i386_freebsd) then
     def_symbol('FREEBSD');
   if (target_info.target in [target_i386_netbsd,target_m68k_netbsd]) then
     def_symbol('NETBSD');
   if (target_info.target in [target_i386_openbsd,target_m68k_openbsd]) then
     def_symbol('OPENBSD');
  end;

 if (target_info.target in [target_i386_linux,target_m68k_linux]) then
  begin
   def_symbol('UNIX');      { generic UNIX }
   def_symbol('HASUNIX');     { Linux + BSD (syscall Unix unit)}
  end;

 if target_info.target=target_i386_sunos then
  begin
   def_symbol('UNIX');
   def_symbol('SOLARIS');
   def_symbol('LIBC');
   def_symbol('SUNOS');
  end;

 if target_info.target in [target_i386_beos,target_i386_qnx] then
  begin
   def_symbol('UNIX');
  end;

 if target_info.target = target_i386_os2 then
  begin
   def_symbol('EMX');
  end;

{$ifdef m68k}
{ Disable fpu emulation for linux and netbsd on m68k machines }
{ FIXME: this overrides possible explicit command line emulation setting,
  but this isn't supported yet anyhow PM }
 if (target_info.target in [target_m68k_openbsd,target_m68k_netbsd,target_m68k_linux]) then
  begin
    initmoduleswitches:=initmoduleswitches-[cs_fp_emulation];
    def_symbol('FPC_FPU_INTERNAL');
  end;
 if (target_info.target = target_m68k_palmos) then
  begin
    { dragonball processors do not support
      68020 instructions PM }
    initoptprocessor:=MC68000;
    { R_A5 is used for GOT PM }
    cpubase.self_pointer:=R_A4;
  end;
 if (initoptprocessor = MC68020) then
     def_symbol('CPUM68020');
{$endif m68k}

  { endian define }
  case target_os.endian of
    endian_little :
      def_symbol('ENDIAN_LITTLE');
    endian_big :
      def_symbol('ENDIAN_BIG');
  end;

{ Check file to compile }
  if param_file='' then
   begin
     Message(option_no_source_found);
     StopOptions;
   end;
{$ifndef unix}
  param_file:=FixFileName(param_file);
{$endif}
  fsplit(param_file,inputdir,inputfile,inputextension);
  if inputextension='' then
   begin
     if IfFileExists(inputdir+inputfile+target_os.sourceext) then
      inputextension:=target_os.sourceext
     else
      if IfFileExists(inputdir+inputfile+target_os.pasext) then
       inputextension:=target_os.pasext;
   end;

{ Add paths specified with parameters to the searchpaths }
  UnitSearchPath.AddList(Option^.ParaUnitPath,true);
  ObjectSearchPath.AddList(Option^.ParaObjectPath,true);
  IncludeSearchPath.AddList(Option^.ParaIncludePath,true);
  LibrarySearchPath.AddList(Option^.ParaLibraryPath,true);

{ add unit environment and exepath to the unit search path }
  if inputdir<>'' then
   Unitsearchpath.AddPath(inputdir,true);
  if not disable_configfile then
{$ifdef Delphi}
  UnitSearchPath.AddPath(dmisc.getenv(target_info.unit_env),false);
{$else}
  UnitSearchPath.AddPath(dos.getenv(target_info.unit_env),false);
{$endif Delphi}
{$ifdef unix}
  fpcdir:=FixPath(getenv('FPCDIR'),false);
  if fpcdir='' then
   begin
     if PathExists('/usr/local/lib/fpc/'+version_string) then
      fpcdir:='/usr/local/lib/fpc/'+version_string+'/'
     else
      fpcdir:='/usr/lib/fpc/'+version_string+'/';
   end;
{$else}
  fpcdir:=FixPath(getenv('FPCDIR'),false);
  if fpcdir='' then
   begin
     fpcdir:=ExePath+'../';
     if not(PathExists(fpcdir+'/units')) and
        not(PathExists(fpcdir+'/rtl')) then
      fpcdir:=fpcdir+'../';
   end;
{$endif}
  { first try development RTL, else use the default installation path }
  if not disable_configfile then
    begin
      if PathExists(FpcDir+'rtl/'+lower(target_info.short_name)) then
       UnitSearchPath.AddPath(FpcDir+'rtl/'+lower(target_info.short_name),false)
      else
       begin
         UnitSearchPath.AddPath(FpcDir+'units/'+lower(target_info.short_name),false);
         UnitSearchPath.AddPath(FpcDir+'units/'+lower(target_info.short_name)+'/rtl',false);
       end;
    end;
  { Add exepath if the exe is not in the current dir, because that is always searched already }
  if ExePath<>GetCurrentDir then
   UnitSearchPath.AddPath(ExePath,false);
  { Add unit dir to the object and library path }
  objectsearchpath.AddList(unitsearchpath,false);
  librarysearchpath.AddList(unitsearchpath,false);

{ switch assembler if it's binary and we got -a on the cmdline }
  if (cs_asm_leave in initglobalswitches) and
     (target_asm.id in binassem) then
   begin
     Message(option_switch_bin_to_src_assembler);
     set_target_asm(target_info.assemsrc);
     initoutputformat:=target_asm.id;
   end;

  if ((target_asm.supported_target <> target_any) and
      (target_asm.supported_target <> target_info.target)) or
    { Never use DJGPP AS for win32 objects PM
      as this leads to wrong relocations into the objects }
     ((target_os.id=os_i386_win32) and
      (target_asm.id=as_i386_as)) then
   begin
     Message2(option_incompatible_asm,target_asm.idtxt,target_os.name);
     { Should we reset to default ??? }
     set_target_asm(target_info.assemsrc);
     Message1(option_asm_forced,target_asm.idtxt);
     initoutputformat:=target_asm.id;
   end;

{ turn off stripping if compiling with debuginfo or profile }
  if (cs_debuginfo in initmoduleswitches) or
     (cs_profile in initmoduleswitches) then
    initglobalswitches:=initglobalswitches-[cs_link_strip];

  if not LinkTypeSetExplicitly then
   set_default_link_type;

{ Set defines depending on the target }
  if (target_info.target in [target_i386_GO32V1,target_i386_GO32V2]) then
   def_symbol('DPMI'); { MSDOS is not defined in BP when target is DPMI }

  dispose(option,Done);
  Option:=nil;
end;


end.
{
  $Log: options.pas,v $
  Revision 1.1.2.64  2003/02/21 23:40:47  marco
   * Added procreation of the dotargetopenbsdaout directive. (which keeps OpenBSD
     to select t_bsdaout as linker module). Removing this procreation and
     recompiling will put OpenBSD in ELF mode.

  Revision 1.1.2.63  2003/02/17 21:29:25  hajny
    * EMX define added for OS/2 for future compatibility

  Revision 1.1.2.62  2003/01/29 00:26:24  pierre
   * avoid checkmem loading if EXTDEBUG is not defined

  Revision 1.1.2.61  2003/01/19 14:55:48  carl
    * more amiga path fixes
    + some small tab fix

  Revision 1.1.2.60  2003/01/12 15:51:45  peter
    * don't print mem lost when exiting from options

  Revision 1.1.2.59  2002/12/18 17:08:56  pierre
   * last patch needs cpubase unit

  Revision 1.1.2.58  2002/12/18 17:06:26  pierre
   * palmos specific requirements

  Revision 1.1.2.57  2002/11/05 11:59:12  pierre
   * forgot to increment j in last patch :(

  Revision 1.1.2.56  2002/11/05 11:37:16  pierre
   * use while statement for More checks as length of More is modified by UnsetBool

  Revision 1.1.2.55  2002/10/22 18:46:18  carl
    - FPU_EMULATION removed (was very misleading)

  Revision 1.1.2.54  2002/10/14 11:10:19  pierre
   * generate extended math functions for m68k if FPC_FPU_INTERNAL is defined

  Revision 1.1.2.53  2002/09/29 14:03:04  carl
    * m68k_fpu_emulated -> fpu_emulation switch

  Revision 1.1.2.52  2002/09/21 14:20:29  carl
    * default CPU is now 68020+ or higher
    * no longer limited in local stack spacr for 68020+ cpu
    * -m68000 / -m68020 option when assembler is called

  Revision 1.1.2.51  2002/09/16 19:30:29  pierre
   * fix -n option that did not work anymore

  Revision 1.1.2.50  2002/09/04 14:32:59  pierre
   + -V option to generate a debug report in fpcdebug.txt

  Revision 1.1.2.49  2002/08/23 08:39:10  pierre
   + added two hints for start and end of config file reading

  Revision 1.1.2.48  2002/08/03 09:16:00  carl
    * fix compilation problems for m68k target
    * fix delphi-style comments (not allowed in fixes!)

  Revision 1.1.2.47  2002/07/31 10:49:27  marco
   * hasunix patches

  Revision 1.1.2.46  2002/07/30 11:21:40  marco
   * Small OpenBSD fixes

  Revision 1.1.2.45  2002/07/14 14:47:30  carl
    - fix compilation problems (revert back to old files)

  Revision 1.1.2.43  2002/04/07 10:24:45  carl
  + added qnx target

  Revision 1.1.2.42  2001/11/24 04:02:45  carl
  * Renamed ppc.cfg -> fpc.cfg

  Revision 1.1.2.41  2001/11/23 02:48:36  carl
  + ppc.cfg is now configuration file for compiler.
    (first tries loading ppc386.cfg for backward compatibility)

  Revision 1.1.2.40  2001/09/04 12:31:32  pierre
    * allow -s option again (error in -sh,-st options addition)

    + add M68K_FPU_EMULATED conditional for m68k targets not using coprocessor

  Revision 1.1.2.39  2001/09/03 16:19:09  pierre
   * implementation of -sh and -st option, for linking at host or target machine

  Revision 1.1.2.38  2001/08/27 14:57:38  pierre
   * disable fp_emulation for m68k linux and netbsd targets

  Revision 1.1.2.37  2001/08/22 02:40:30  carl
  * replaced linux -> unix in some particular cases

  Revision 1.1.2.36  2001/08/14 21:12:50  carl
  - undef for cross-compiling BeOS / SunOS

  Revision 1.1.2.35  2001/08/07 15:55:31  pierre
   + new code for NetBSD, behaves like FreeBSD for now

  Revision 1.1.2.34  2001/07/31 01:14:17  carl
  - removed FPU_IN_RTL (replaced by ifdef linux in RTL)

  Revision 1.1.2.33  2001/07/30 12:15:31  pierre
   + add USE_FPU_IN_RTL conditional for m68k linux

  Revision 1.1.2.32  2001/07/30 09:06:02  pierre
   * also define UNIX for m68k linux

  Revision 1.1.2.31  2001/07/30 00:44:23  carl
  - removed HASINTERNMATH

  Revision 1.1.2.30  2001/07/01 20:17:37  peter
    * better +/- support by Pavel and Sergey

  Revision 1.1.2.29  2001/06/15 11:56:30  pierre
   * Release option changed to -Ur

  Revision 1.1.2.28  2001/06/14 16:42:01  pierre
   + -Sr option for releases, include file changes discarded

  Revision 1.1.2.27  2001/05/18 22:35:01  peter
    * merged endian ppu fixes from mainbranch
    * endian define

  Revision 1.1.2.26  2001/04/22 00:46:33  carl
  - removed HASSAVEREGISTERS symbolic define

  Revision 1.1.2.25  2001/03/16 23:41:32  pierre
   * fix bug 1439

  Revision 1.1.2.24  2001/03/13 20:58:56  peter
    * message loading fixes from Sergey

  Revision 1.1.2.23  2001/03/10 13:22:41  peter
    * don't check messagefile for numbers, this allows the usage of
      1.1 msgfiles with a 1.0.x compiler

  Revision 1.1.2.22  2001/03/08 03:27:26  carl
  - renamed optscpu -> cpuswtch

  Revision 1.1.2.21  2001/03/05 21:47:31  peter
    * press enter moved to errore.msg

  Revision 1.1.2.20  2001/03/04 02:28:19  carl
  +renamed units which removed some defines once again.

  Revision 1.1.2.19  2001/02/26 12:35:13  jonas
    * fixed bug in type checking for compatibility of set elements
    * released fix in options.pas from Carl also for FPC

  Revision 1.1.2.18  2001/02/26 08:06:43  michael
  * option_help_pages:
     allow to omit an option (use one space char insteed an option)
     but to indent a continuation line as if option is present. For lines:
       3*2CX_first line
       3*2 _second line
       3*2*_third line
     we could get:
       -CX        first line
                  second line
       third line

  Revision 1.1.2.17  2001/02/26 07:49:00  michael
  Support replacements for all -F<x> options

  Revision 1.1.2.16  2001/02/24 23:01:25  carl
  added conditional defines for correct compiler

  Revision 1.1.2.15  2001/02/24 03:17:56  carl
  Noag386bin define problem solved

  Revision 1.1.2.14  2001/02/08 22:06:43  florian
    * solaris to sunos changed

  Revision 1.1.2.13  2001/02/05 21:26:04  peter
    * applied patches from Sergey Korshunoff

  Revision 1.1.2.12  2001/02/01 22:31:55  florian
    + Solaris support for the compiler

  Revision 1.1.2.11  2001/01/20 18:37:58  hajny
    + APPTYPE support under OS/2, at_fs, GetEnvPChar for OS/2

  Revision 1.1.2.10  2000/12/16 15:56:17  jonas
    - removed all ifdef cardinalmulfix code

  Revision 1.1.2.9  2000/12/15 13:23:57  jonas
    * added int64funcresok define to signal functions returning int64
      values work fine (necessary when compiling the main branch)

  Revision 1.1.2.8  2000/11/07 15:10:30  marco
   * UNIX defined and checked crosscompile thingy

  Revision 1.1.2.7  2000/09/26 10:36:11  jonas
    * initmodeswitches is changed is you change the compiler mode from the
      command line (the -S<x> switches didn't work anymore for changing the
      compiler mode)

  Revision 1.1.2.6  2000/09/24 21:40:19  peter
    * error messages updated
    * if messages not available in message file fallback to the internal
      messages
    * message prefixes (like Note:) can now also be set in the msg file

  Revision 1.1.2.5  2000/09/18 12:13:08  marco
   * Moved setting extra defines FreeBSD to a later point in the file.
     -T platform change didn't trigger setting those defines. Now crosscompiling
      works.

  Revision 1.1.2.4  2000/09/18 10:59:56  marco
   * Renamed t_freebsd to t_fbsd because of 8.3 convention

  Revision 1.1.2.3  2000/09/13 13:57:41  marco
   * FreeBSD compiler support

  Revision 1.1.2.2  2000/08/07 09:19:33  jonas
    * fixed bug in type conversions between enum subranges (it didn't take
      the packenum directive into account)
    + define PACKENUMFIXED symbol in options.pas

  Revision 1.1.2.1  2000/07/15 07:09:18  pierre
   * refuse AS output for Win32

  Revision 1.1  2000/07/13 06:29:52  michael
  + Initial import

  Revision 1.71  2000/06/30 20:23:38  peter
    * new message files layout with msg numbers (but still no code to
      show the number on the screen)

  Revision 1.70  2000/06/19 19:57:19  pierre
   * smart link is default on win32

  Revision 1.69  2000/05/23 21:28:22  pierre
    + check of compatibility between selected assembler
      output and target OS

  Revision 1.68  2000/05/16 20:19:06  pierre
    + -CR option to enable check for object virtual method

  Revision 1.67  2000/05/10 13:40:19  peter
    * -Se<x> option extended to increase errorcount for
      warning,notes or hints

  Revision 1.66  2000/04/24 13:34:29  peter
    * added enhancedraise define

  Revision 1.65  2000/04/10 11:36:19  pierre
   * get -g-l to work

  Revision 1.64  2000/04/07 14:56:18  peter
    * correctfldcw define added

  Revision 1.63  2000/04/05 21:57:34  pierre
   * no unitdir automatically added if -n option present

  Revision 1.62  2000/03/13 20:06:59  michael
  + Added switch to swicth on assertions.

  Revision 1.61  2000/02/15 14:36:45  florian
    * disable FIXED data type per default

  Revision 1.60  2000/02/10 11:45:48  peter
    * addpath fixed with list of paths when inserting at the beginning
    * if exepath=currentdir then it's not inserted in path list
    * searchpaths in ppc386.cfg are now added at the beginning of the
      list instead of at the end. (commandline is not changed)
    * check paths before inserting in list

  Revision 1.59  2000/02/09 13:22:54  peter
    * log truncated

  Revision 1.58  2000/02/09 10:35:48  peter
    * -Xt option to link staticly against c libs

  Revision 1.57  2000/02/06 17:20:52  peter
    * -gl switch for auto lineinfo including

  Revision 1.56  2000/01/31 15:55:42  peter
    * fixed default unit location for linux when fpcdir was not set

  Revision 1.55  2000/01/23 18:20:50  sg
  * Fixed typo in line 1375 ("fpidr" instead of "fpcdir")

  Revision 1.54  2000/01/23 16:36:37  peter
    * better auto RTL dir detection

  Revision 1.53  2000/01/20 10:36:44  daniel
    * also support ; comments in cfg file

  Revision 1.52  2000/01/17 22:50:28  peter
    * fixed interpret_envvar whcih crashed when the envvar was not set
    * also warn if the envvar is empty (=not set)

  Revision 1.51  2000/01/14 15:33:15  pierre
   + parsecmd supports "filename with spaces" for IDE

  Revision 1.50  2000/01/14 14:33:54  pierre
   + some warnings for wrong lines inside config files

  Revision 1.49  2000/01/10 11:14:19  peter
    * fixed memory leak with options, you must use StopOptions instead of
      Stop
    * fixed memory leak with forward resolving, make_ref is now false

  Revision 1.48  2000/01/07 22:22:02  marco
   * Added $target support for -FD

  Revision 1.47  2000/01/07 01:14:27  peter
    * updated copyright to 2000

  Revision 1.46  2000/01/06 15:48:59  peter
    * wildcard support for directory adding, this allows the use of units/*
      in ppc386.cfg

  Revision 1.45  1999/12/20 23:23:30  pierre
   + $description $version

  Revision 1.44  1999/12/20 21:42:36  pierre
    + dllversion global variable
    * FPC_USE_CPREFIX code removed, not necessary anymore
      as we use .edata direct writing by default now.

  Revision 1.43  1999/12/18 14:55:21  florian
    * very basic widestring support

  Revision 1.42  1999/12/11 18:53:31  jonas
    * fixed type conversions of results of operations with cardinals
      (between -dcardinalmulfix)

  Revision 1.41  1999/12/10 10:03:54  peter
    * fixed parameter orderning

  Revision 1.40  1999/12/08 10:40:01  pierre
    + allow use of unit var in exports of DLL for win32
      by using direct export writing by default instead of use of DEFFILE
      that does not allow assembler labels that do not
      start with an underscore.
      Use -WD to force use of Deffile for Win32 DLL

  Revision 1.39  1999/12/06 18:21:03  peter
    * support !ENVVAR for long commandlines
    * win32/go32v2 write short pathnames to link.res so c:\Program Files\ is
      finally supported as installdir.

}
