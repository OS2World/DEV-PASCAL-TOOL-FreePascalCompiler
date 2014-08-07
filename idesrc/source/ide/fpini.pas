{
    $Id: fpini.pas,v 1.11 2002/12/12 00:03:14 pierre Exp $
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Write/Read Options to INI File

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FPIni;
interface

{$i globdir.inc}

uses
  FPUtils;

procedure InitDirs;
procedure InitINIFile;
procedure CheckINIFile;
function  ReadINIFile: boolean;
function  WriteINIFile(FromSaveAs : boolean) : boolean;


implementation

uses
  Dos,Objects,Drivers,
{$ifdef FVISION}
  FVConsts,
{$else}
  Commands,
{$endif}
  Version,
{$ifdef USE_EXTERNAL_COMPILER}
   fpintf, { superseeds version_string of version unit }
{$endif USE_EXTERNAL_COMPILER}
  WConsts,WUtils,WINI,WViews,{$ifndef EDITORS}WEditor,WCEdit{$else}Editors{$endif},
  {$ifndef NODEBUG}FPDebug,{$endif}FPConst,FPVars,
  FPIntf,FPTools,FPSwitch,FPString;

const
  { INI file sections }
  secFiles       = 'Files';
  secRun         = 'Run';
  secCompile     = 'Compile';
  secColors      = 'Colors';
  secHelp        = 'Help';
  secEditor      = 'Editor';
  secBreakpoint  = 'Breakpoints';
  secWatches     = 'Watches';
  secHighlight   = 'Highlight';
  secMouse       = 'Mouse';
  secSearch      = 'Search';
  secTools       = 'Tools';
  secSourcePath  = 'SourcePath';
  secPreferences = 'Preferences';
  secMisc        = 'Misc';

  { INI file tags }
  ieRecentFile       = 'RecentFile';
(*  ieOpenFile         = 'OpenFile';
  ieOpenFileCount    = 'OpenFileCount'; *)
  ieRunParameters    = 'Parameters';
  ieDebuggeeRedir    = 'DebugRedirection';
  ieRemoteMachine    = 'RemoteMachine';
  ieRemotePort       = 'RemotePort';
  ieRemoteSendCommand = 'RemoteSendCommand';
  ieRemoteConfig     = 'RemoteSendConfig';
  ieRemoteIdent      = 'RemoteSendIdent';
  ieRemoteDirectory  = 'RemoteDirectory';
  iePrimaryFile      = 'PrimaryFile';
  ieCompileMode      = 'CompileMode';
  iePalette          = 'Palette';
  ieHelpFiles        = 'Files';
  ieDefaultTabSize   = 'DefaultTabSize';
  ieDefaultIndentSize = 'DefaultIndentSize';
  ieDefaultEditorFlags='DefaultFlags';
  ieDefaultSaveExt   = 'DefaultSaveExt';
  ieOpenExts         = 'OpenExts';
  ieHighlightExts    = 'Exts';
  ieTabsPattern      = 'NeedsTabs';
  ieDoubleClickDelay = 'DoubleDelay';
  ieReverseButtons   = 'ReverseButtons';
  ieAltClickAction   = 'AltClickAction';
  ieCtrlClickAction  = 'CtrlClickAction';
  ieFindFlags        = 'FindFlags';
  ieToolName         = 'Title';
  ieToolProgram      = 'Program';
  ieToolParams       = 'Params';
  ieToolHotKey       = 'HotKey';
  ieBreakpointTyp    = 'Type';
  ieBreakpointCount  = 'Count';
  ieBreakpointState  = 'State';
  ieBreakpointName   = 'Name';
  ieBreakpointFile   = 'FileName';
  ieBreakpointLine   = 'LineNumber';
  ieBreakpointCond   = 'Condition';
  ieWatchCount       = 'Count';
  ieWatchName        = 'Watch';
  ieSourceList       = 'SourceList';
{  ieVideoMode        = 'VideoMode';}
  ieAutoSave         = 'AutoSaveFlags';
  ieMiscOptions      = 'MiscOptions';
  ieDesktopLocation  = 'DesktopLocation';
  ieDesktopFlags     = 'DesktopFileFlags';
  ieCenterDebuggerRow= 'CenterCurrentLineWhileDebugging';
  ieShowReadme       = 'ShowReadme';


Procedure InitDirs;
begin
  StartupDir:=CompleteDir(FExpand('.'));
{$ifndef unix}
  IDEDir:=CompleteDir(DirOf(system.Paramstr(0)));
{$else}
  SystemIDEDir:='/usr/lib/fpc/'+version_string+'/ide/text';
  IDEdir:=CompleteDir(FExpand('~/.fp'));
  If Not ExistsDir(IDEdir) Then
    begin
      IDEDir:=SystemIDEDir;
      if Not ExistsDir(IDEDir) then
        begin
          if DirOf(system.paramstr(0))<>'' then
            IDEDir:=CompleteDir(DirOf(system.ParamStr(0)))
          else
            IDEDir:=StartupDir;
        end;
    end;
{$endif}
end;

procedure InitINIFile;
var S: string;
begin
  S:=LocateFile(INIFileName);
  if S<>'' then
    IniFileName:=S;
  IniFileName:=FExpand(IniFileName);
end;

procedure CheckINIFile;
var IniDir,CurDir: DirStr;
    INI: PINIFile;
const Btns : array[1..2] of string = (btn_config_copyexisting,btn_config_createnew);
begin
  IniDir:=DirOf(IniFileName); CurDir:=GetCurDir;
  if CompareText(IniDir,CurDir)<>0 then
   if not ExistsFile(CurDir+DirInfoName) then
     if ConfirmBox(FormatStrStr(msg_doyouwanttocreatelocalconfigfile,IniDir),nil,false)=cmYes then
       begin
         if (not ExistsFile(IniFileName)) or
            (ChoiceBox(msg_configcopyexistingorcreatenew,nil,
              Btns,false)=cmUserBtn2) then
           begin
             { create new config here }
             IniFileName:=CurDir+IniName;
             SwitchesPath:=CurDir+SwitchesName;
           end
         else
           begin
             { copy config here }
             if CopyFile(IniFileName,CurDir+IniName)=false then
               ErrorBox(FormatStrStr(msg_errorwritingfile,CurDir+IniName),nil)
             else
               IniFileName:=CurDir+IniName;
             if CopyFile(SwitchesPath,CurDir+SwitchesName)=false then
               ErrorBox(FormatStrStr(msg_errorwritingfile,CurDir+SwitchesName),nil)
             else
               SwitchesPath:=CurDir+SwitchesName;
           end;
       end
     else
       begin
         New(INI, Init(CurDir+DirInfoName));
         INI^.SetEntry(MainSectionName,'Comment','Do NOT delete this file!!!');
         if INI^.Update=false then
           ErrorBox(FormatStrStr(msg_errorwritingfile,INI^.GetFileName),nil);
         Dispose(INI, Done);
       end;
end;

function PaletteToStr(S: string): string;
var C: string;
    I: integer;
begin
  C:='';
  for I:=1 to length(S) do
    begin
      Insert('#$'+IntToHex(ord(S[I]),2),C,Length(C)+1);
    end;
  PaletteToStr:=C;
end;

function StrToPalette(S: string): string;
var I,P,X: integer;
    C: string;
    Hex: boolean;
    OK: boolean;
begin
  C:=''; I:=1;
  OK:=S<>'';
  while OK and (I<=length(S)) and (S[I]='#') do
  begin
    Inc(I); Hex:=false;
    if S[I]='$' then begin Inc(I); Hex:=true; end;
    P:=Pos('#',copy(S,I,High(S))); if P>0 then P:=I+P-1 else P:=length(S)+1;
    if Hex=false then
      begin
        X:=StrToInt(copy(S,I,P-I));
        OK:=(LastStrToIntResult=0) and (0<=X) and (X<=High(S));
      end
    else
      begin
        X:=HexToInt(copy(S,I,P-I));
        OK:=(LastHexToIntResult=0) and (0<=X) and (X<=255);
      end;
    if OK then C:=C+chr(X);
    Inc(I,P-I);
  end;
  StrToPalette:=C;
end;

{$ifndef NODEBUG}
procedure WriteOneWatchEntry(I : Longint;INIFile : PINIFile);
var
  PW : PWatch;
  S  : String;
begin
  Str(I,S);
  PW:=WatchesCollection^.At(I);
  With PW^ do
    begin
      INIFile^.SetEntry(secWatches,ieWatchName+S,GetStr(expr));
    end;
end;

procedure WriteOneBreakPointEntry(I : longint;INIFile : PINIFile);
var PB : PBreakpoint;
    S : String;
begin
  Str(I,S);
  PB:=BreakpointsCollection^.At(I);
  If assigned(PB) then
   With PB^ do
    Begin
      INIFile^.SetEntry(secBreakpoint,ieBreakpointTyp+S,BreakpointTypeStr[typ]);
      INIFile^.SetEntry(secBreakpoint,ieBreakpointState+S,BreakpointStateStr[state]);
      if typ=bt_file_line then
        begin
          INIFile^.SetEntry(secBreakpoint,ieBreakpointFile+S,FileName^);
          INIFile^.SetIntEntry(secBreakpoint,ieBreakpointLine+S,Line);
        end
      else
        INIFile^.SetEntry(secBreakpoint,ieBreakpointName+S,Name^);
      if assigned(Conditions) then
        INIFile^.SetEntry(secBreakpoint,ieBreakpointCond+S,Conditions^)
      else
        INIFile^.SetEntry(secBreakpoint,ieBreakpointCond+S,'');
    end;
end;

procedure ReadOneWatchEntry(I : Longint;INIFile : PINIFile);
var
  PW : PWatch;
  S  : String;
begin
  Str(I,S);
  PW:=new(PWatch,Init(INIFile^.GetEntry(secWatches,ieWatchName+S,'')));
  WatchesCollection^.Insert(PW);
end;

procedure ReadOneBreakPointEntry(i : longint;INIFile : PINIFile);
var PB : PBreakpoint;
    S,S2,SC : string;
    Line : longint;
    typ : BreakpointType;
    state : BreakpointState;

begin
  Str(I,S2);
  typ:=bt_invalid;
  S:=INIFile^.GetEntry(secBreakpoint,ieBreakpointTyp+S2,BreakpointTypeStr[typ]);
  for typ:=low(BreakpointType) to high(BreakpointType) do
    If pos(BreakpointTypeStr[typ],S)>0 then break;
  state:=bs_deleted;
  S:=INIFile^.GetEntry(secBreakpoint,ieBreakpointState+S2,BreakpointStateStr[state]);
  for state:=low(BreakpointState) to high(BreakpointState) do
    If pos(BreakpointStateStr[state],S)>0 then break;
  case typ of
     bt_invalid :;
     bt_file_line :
       begin
         S:=INIFile^.GetEntry(secBreakpoint,ieBreakpointFile+S2,'');
         Line:=INIFile^.GetIntEntry(secBreakpoint,ieBreakpointLine+S2,0);
       end;
     else
       begin
         S:=INIFile^.GetEntry(secBreakpoint,ieBreakpointName+S2,'');
       end;
     end;
   SC:=INIFile^.GetEntry(secBreakpoint,ieBreakpointCond+S2,'');
   if (typ=bt_function) and (S<>'') then
     new(PB,init_function(S))
   else if (typ=bt_file_line) and (S<>'') then
     new(PB,init_file_line(S,Line))
   else
     new(PB,init_type(typ,S));
   If assigned(PB) then
     begin
       PB^.state:=state;
       If SC<>'' then
         PB^.conditions:=NewStr(SC);
       BreakpointsCollection^.Insert(PB);
     end;
end;
{$endif NODEBUG}

function ReadINIFile: boolean;
var INIFile: PINIFile;
    S,PS,S1,S2,S3: string;
    I,P: integer;
    BreakPointCount,WatchesCount:longint;
    OK: boolean;
    ts : TSwitchMode;
    W: word;
begin
  OK:=ExistsFile(IniFileName);
  if OK then
 begin
  New(INIFile, Init(IniFileName));
  { Files }
  OpenExts:=INIFile^.GetEntry(secFiles,ieOpenExts,OpenExts);
  RecentFileCount:=High(RecentFiles);
  for I:=Low(RecentFiles) to High(RecentFiles) do
    begin
      S:=INIFile^.GetEntry(secFiles,ieRecentFile+IntToStr(I),'');
      if (S='') and (RecentFileCount>I-1) then RecentFileCount:=I-1;
      with RecentFiles[I] do
      begin
        P:=Pos(',',S); if P=0 then P:=length(S)+1;
        FileName:=copy(S,1,P-1); Delete(S,1,P);
        P:=Pos(',',S); if P=0 then P:=length(S)+1;
        LastPos.X:=Max(0,StrToInt(copy(S,1,P-1))); Delete(S,1,P);
        P:=Pos(',',S); if P=0 then P:=length(S)+1;
        LastPos.Y:=Max(0,StrToInt(copy(S,1,P-1))); Delete(S,1,P);
      end;
    end;
  { Run }
  { First read the primary file, which can also set the parameters which can
    be overruled with the parameter loading }
  SetPrimaryFile(INIFile^.GetEntry(secCompile,iePrimaryFile,PrimaryFile));
  SetRunParameters(INIFile^.GetEntry(secRun,ieRunParameters,GetRunParameters));
{$ifndef GABOR}
  DebuggeeTTY := INIFile^.GetEntry(secRun,ieDebuggeeRedir,DebuggeeTTY);
{$ifdef SUPPORT_REMOTE}
  RemoteMachine :=INIFile^.GetEntry(secRun,ieRemoteMachine,RemoteMachine);
  RemotePort :=INIFile^.GetEntry(secRun,ieRemotePort,RemotePort);
  RemoteSendCommand :=INIFile^.GetEntry(secRun,ieRemoteSendCommand,RemoteSendCommand);
  RemoteConfig :=INIFile^.GetEntry(secRun,ieRemoteConfig,RemoteConfig);
  RemoteIdent :=INIFile^.GetEntry(secRun,ieRemoteIdent,RemoteIdent);
  RemoteDir :=INIFile^.GetEntry(secRun,ieRemoteDirectory,RemoteDir);
{$endif SUPPORT_REMOTE}
{$endif}
  { Compile }
  S:=INIFile^.GetEntry(secCompile,ieCompileMode,'');
  for ts:=low(TSwitchMode) to high(TSwitchMode) do
    begin
      if SwitchesModeStr[ts]=S then
        SwitchesMode:=ts;
    end;
  { Help }
  S:=INIFile^.GetEntry(secHelp,ieHelpFiles,'');
  repeat
    P:=Pos(';',S); if P=0 then P:=length(S)+1;
    PS:=copy(S,1,P-1);
    if PS<>'' then HelpFiles^.Insert(NewStr(PS));
    Delete(S,1,P);
  until S='';
  { Editor }
{$ifndef EDITORS}
  DefaultTabSize:=INIFile^.GetIntEntry(secEditor,ieDefaultTabSize,DefaultTabSize);
  DefaultIndentSize:=INIFile^.GetIntEntry(secEditor,ieDefaultIndentSize,DefaultIndentSize);
  DefaultCodeEditorFlags:=INIFile^.GetIntEntry(secEditor,ieDefaultEditorFlags,DefaultCodeEditorFlags);
  DefaultSaveExt:=INIFile^.GetEntry(secEditor,ieDefaultSaveExt,DefaultSaveExt);
{$endif}
  { Highlight }
  HighlightExts:=INIFile^.GetEntry(secHighlight,ieHighlightExts,HighlightExts);
  TabsPattern:=INIFile^.GetEntry(secHighlight,ieTabsPattern,TabsPattern);
  { SourcePath }
  SourceDirs:=INIFile^.GetEntry(secSourcePath,ieSourceList,SourceDirs);
  { Mouse }
  DoubleDelay:=INIFile^.GetIntEntry(secMouse,ieDoubleClickDelay,DoubleDelay);
  MouseReverse:=boolean(INIFile^.GetIntEntry(secMouse,ieReverseButtons,byte(MouseReverse)));
  AltMouseAction:=INIFile^.GetIntEntry(secMouse,ieAltClickAction,AltMouseAction);
  CtrlMouseAction:=INIFile^.GetIntEntry(secMouse,ieCtrlClickAction,CtrlMouseAction);
  { Search }
  FindFlags:=INIFile^.GetIntEntry(secSearch,ieFindFlags,FindFlags);
  { Breakpoints }
{$ifndef NODEBUG}
  BreakpointCount:=INIFile^.GetIntEntry(secBreakpoint,ieBreakpointCount,0);
  for i:=1 to BreakpointCount do
    ReadOneBreakPointEntry(i-1,INIFile);
  WatchesCount:=INIFile^.GetIntEntry(secWatches,ieWatchCount,0);
  for i:=1 to WatchesCount do
    ReadOneWatchEntry(i-1,INIFile);
{$endif}
  { Tools }
  for I:=1 to MaxToolCount do
    begin
      S:=IntToStr(I);
      S1:=INIFile^.GetEntry(secTools,ieToolName+S,'');
      if S1='' then Break; { !!! }
      S2:=INIFile^.GetEntry(secTools,ieToolProgram+S,'');
      S3:=INIFile^.GetEntry(secTools,ieToolParams+S,'');
      W:=Max(0,Min(65535,INIFile^.GetIntEntry(secTools,ieToolHotKey+S,0)));
      AddTool(S1,S2,S3,W);
    end;
  { Colors }
  S:=AppPalette;
  PS:=StrToPalette(INIFile^.GetEntry(secColors,iePalette+'_1_40',PaletteToStr(copy(S,1,40))));
  PS:=PS+StrToPalette(INIFile^.GetEntry(secColors,iePalette+'_41_80',PaletteToStr(copy(S,41,40))));
  PS:=PS+StrToPalette(INIFile^.GetEntry(secColors,iePalette+'_81_120',PaletteToStr(copy(S,81,40))));
  PS:=PS+StrToPalette(INIFile^.GetEntry(secColors,iePalette+'_121_160',PaletteToStr(copy(S,121,40))));
  PS:=PS+StrToPalette(INIFile^.GetEntry(secColors,iePalette+'_161_200',PaletteToStr(copy(S,161,40))));
  PS:=PS+StrToPalette(INIFile^.GetEntry(secColors,iePalette+'_201_240',PaletteToStr(copy(S,201,40))));
  if length(PS)<length(CIDEAppColor) then
    PS:=PS+copy(CIDEAppColor,length(PS)+1,255);
  AppPalette:=PS;
(*  { Open files }
  for I:=INIFile^.GetIntEntry(secFiles,ieOpenFileCount,0) downto 1 do
    begin
      S:=INIFile^.GetEntry(secFiles,ieOpenFile+IntToStr(I),'');
      if (S='') then
        break;
      P:=Pos(',',S); if P=0 then P:=length(S)+1;
      S1:=copy(S,1,P-1);
      Delete(S,1,P);
      P:=Pos(',',S);
      if P=0 then P:=length(S)+1;
      X:=Max(0,StrToInt(copy(S,1,P-1)));
      Delete(S,1,P);
      P:=Pos(',',S);
      if P=0 then P:=length(S)+1;
      Y:=Max(0,StrToInt(copy(S,1,P-1)));
      Delete(S,1,P);
      P:=Pos(',',S);
      if P=0 then P:=length(S)+1;
      R.A.X:=Max(0,StrToInt(copy(S,1,P-1)));
      Delete(S,1,P);
      P:=Pos(',',S);
      if P=0 then P:=length(S)+1;
      R.A.Y:=Max(0,StrToInt(copy(S,1,P-1)));
      Delete(S,1,P);
      P:=Pos(',',S);
      if P=0 then P:=length(S)+1;
      R.B.X:=Max(0,StrToInt(copy(S,1,P-1)));
      Delete(S,1,P);
      P:=Pos(',',S);
      if P=0 then P:=length(S)+1;
      R.B.Y:=Max(0,StrToInt(copy(S,1,P-1)));
      if (R.A.X<R.B.X) and (R.A.Y<R.B.Y) then
        TryToOpenFile(@R,S1,X,Y,false)
      else
        TryToOpenFile(nil,S1,X,Y,false);
      { remove it because otherwise we allways keep old files }
      INIFile^.DeleteEntry(secFiles,ieOpenFile+IntToStr(I));
    end;
*)
  { Desktop }
  DesktopFileFlags:=INIFile^.GetIntEntry(secPreferences,ieDesktopFlags,DesktopFileFlags);
  { Debugger }
  IniCenterDebuggerRow:=INIFile^.GetIntEntry(secPreferences,ieCenterDebuggerRow,1)<>0;
  { Preferences }
  AutoSaveOptions:=INIFile^.GetIntEntry(secPreferences,ieAutoSave,AutoSaveOptions);
  MiscOptions:=INIFile^.GetIntEntry(secPreferences,ieMiscOptions,MiscOptions);
  DesktopLocation:=INIFile^.GetIntEntry(secPreferences,ieDesktopLocation,DesktopLocation);
  { Misc }
  ShowReadme:=INIFile^.GetIntEntry(secMisc,ieShowReadme,{integer(ShowReadme)}1)<>0;
  Dispose(INIFile, Done);
 end;
  ReadINIFile:=OK;
end;

function WriteINIFile (FromSaveAs : boolean): boolean;
var INIFile: PINIFile;
    S: string;
    S1,S2,S3: string;
    W: word;
    BreakPointCount,WatchesCount:longint;
    I(*,OpenFileCount*): integer;
    OK: boolean;

procedure ConcatName(P: PString); {$ifndef FPC}far;{$endif}
begin
  if (S<>'') then S:=S+';';
  S:=S+P^;
end;
begin
{$ifdef Unix}
  if not FromSaveAs and (DirOf(IniFileName)=DirOf(SystemIDEDir)) then
    begin
      IniFileName:=FExpand('~/.fp/'+IniName);
      If not ExistsDir(DirOf(IniFileName)) then
        MkDir(FExpand('~/.fp'));
   end;
{$endif Unix}
  New(INIFile, Init(IniFileName));
  { Files }
  { avoid keeping old files }
  INIFile^.DeleteSection(secFiles);
  INIFile^.SetEntry(secFiles,ieOpenExts,'"'+OpenExts+'"');
  for I:=1 to High(RecentFiles) do
    begin
      if I<=RecentFileCount then
         with RecentFiles[I] do S:=FileName+','+IntToStr(LastPos.X)+','+IntToStr(LastPos.Y)
      else
         S:='';
      INIFile^.SetEntry(secFiles,ieRecentFile+IntToStr(I),S);
    end;

(*
    PW:=FirstEditorWindow;
    PPW:=PW;
    I:=1;
    while assigned(PW) do
      begin
        If PW^.HelpCtx=hcSourceWindow then
          begin
            With PW^.editor^ do
              S:=FileName+','+IntToStr(CurPos.X)+','+IntToStr(CurPos.Y);
            PW^.GetBounds(R);
            S:=S+','+IntToStr(R.A.X)+','+IntToStr(R.A.Y)+','+
              IntToStr(R.B.X)+','+IntToStr(R.B.Y);
            INIFile^.SetEntry(secFiles,ieOpenFile+IntToStr(I),S);
            Inc(I);
            OpenFileCount:=I-1;
          end;

        PW:=PSourceWindow(PW^.next);
        While assigned(PW) and (PW<>PPW) and (PW^.HelpCtx<>hcSourceWindow) do
          PW:=PSourceWindow(PW^.next);
        If PW=PPW then
          break;
      end;

  INIFile^.SetIntEntry(secFiles,ieOpenFileCount,OpenFileCount);
*)
  { Run }
  INIFile^.SetEntry(secRun,ieRunParameters,GetRunParameters);
{$ifndef GABOR}
  { If DebuggeeTTY<>'' then }
    INIFile^.SetEntry(secRun,ieDebuggeeRedir,DebuggeeTTY);
{$ifdef SUPPORT_REMOTE}
    INIFile^.SetEntry(secRun,ieRemoteMachine,RemoteMachine);
    INIFile^.SetEntry(secRun,ieRemotePort,RemotePort);
    INIFile^.SetEntry(secRun,ieRemoteSendCommand,RemoteSendCommand);
    INIFile^.SetEntry(secRun,ieRemoteConfig,RemoteConfig);
    INIFile^.SetEntry(secRun,ieRemoteIdent,RemoteIdent);
    INIFile^.SetEntry(secRun,ieRemoteDirectory,RemoteDir);
{$endif SUPPORT_REMOTE}
{$endif}
  { Compile }
  INIFile^.SetEntry(secCompile,iePrimaryFile,PrimaryFile);
  INIFile^.SetEntry(secCompile,ieCompileMode,SwitchesModeStr[SwitchesMode]);
  { Help }
  S:='';
  HelpFiles^.ForEach(@ConcatName);
  INIFile^.SetEntry(secHelp,ieHelpFiles,'"'+S+'"');
  { Editor }
{$ifndef EDITORS}
  INIFile^.SetIntEntry(secEditor,ieDefaultTabSize,DefaultTabSize);
  INIFile^.SetIntEntry(secEditor,ieDefaultIndentSize,DefaultIndentSize);
  INIFile^.SetIntEntry(secEditor,ieDefaultEditorFlags,DefaultCodeEditorFlags);
  INIFile^.SetEntry(secEditor,ieDefaultSaveExt,DefaultSaveExt);
{$endif}
  { Highlight }
  INIFile^.SetEntry(secHighlight,ieHighlightExts,'"'+HighlightExts+'"');
  INIFile^.SetEntry(secHighlight,ieTabsPattern,'"'+TabsPattern+'"');
  { SourcePath }
  INIFile^.SetEntry(secSourcePath,ieSourceList,'"'+SourceDirs+'"');
  { Mouse }
  INIFile^.SetIntEntry(secMouse,ieDoubleClickDelay,DoubleDelay);
  INIFile^.SetIntEntry(secMouse,ieReverseButtons,byte(MouseReverse));
  INIFile^.SetIntEntry(secMouse,ieAltClickAction,AltMouseAction);
  INIFile^.SetIntEntry(secMouse,ieCtrlClickAction,CtrlMouseAction);
  { Search }
  INIFile^.SetIntEntry(secSearch,ieFindFlags,FindFlags);
  { Breakpoints }
{$ifndef NODEBUG}
  BreakPointCount:=BreakpointsCollection^.Count;
  INIFile^.SetIntEntry(secBreakpoint,ieBreakpointCount,BreakpointCount);
  for i:=1 to BreakpointCount do
    WriteOneBreakPointEntry(I-1,INIFile);
  WatchesCount:=WatchesCollection^.Count;
  INIFile^.SetIntEntry(secWatches,ieWatchCount,WatchesCount);
  for i:=1 to WatchesCount do
    WriteOneWatchEntry(I-1,INIFile);
{$endif}
  { Tools }
  INIFile^.DeleteSection(secTools);
  for I:=1 to GetToolCount do
    begin
      S:=IntToStr(I);
      GetToolParams(I-1,S1,S2,S3,W);
      if S1<>'' then S1:='"'+S1+'"';
      if S2<>'' then S2:='"'+S2+'"';
      if S3<>'' then S3:='"'+S3+'"';
      INIFile^.SetEntry(secTools,ieToolName+S,S1);
      INIFile^.SetEntry(secTools,ieToolProgram+S,S2);
      INIFile^.SetEntry(secTools,ieToolParams+S,S3);
      INIFile^.SetIntEntry(secTools,ieToolHotKey+S,W);
    end;
  { Colors }
  if AppPalette<>CIDEAppColor then
  begin
    { this has a bug. if a different palette has been read on startup, and
      then changed back to match the default, this will not update it in the
      ini file, eg. the original (non-default) will be left unmodified... }
    S:=AppPalette;
    INIFile^.SetEntry(secColors,iePalette+'_1_40',PaletteToStr(copy(S,1,40)));
    INIFile^.SetEntry(secColors,iePalette+'_41_80',PaletteToStr(copy(S,41,40)));
    INIFile^.SetEntry(secColors,iePalette+'_81_120',PaletteToStr(copy(S,81,40)));
    INIFile^.SetEntry(secColors,iePalette+'_121_160',PaletteToStr(copy(S,121,40)));
    INIFile^.SetEntry(secColors,iePalette+'_161_200',PaletteToStr(copy(S,161,40)));
    INIFile^.SetEntry(secColors,iePalette+'_201_240',PaletteToStr(copy(S,201,40)));
  end;
  { Desktop }
  INIFile^.SetIntEntry(secPreferences,ieDesktopFlags,DesktopFileFlags);
  INIFile^.SetIntEntry(secPreferences,ieCenterDebuggerRow,byte(IniCenterDebuggerRow));
  { Preferences }
  INIFile^.SetIntEntry(secPreferences,ieAutoSave,AutoSaveOptions);
  INIFile^.SetIntEntry(secPreferences,ieMiscOptions,MiscOptions);
  INIFile^.SetIntEntry(secPreferences,ieDesktopLocation,DesktopLocation);
  { Misc }
  INIFile^.SetIntEntry(secMisc,ieShowReadme,integer(ShowReadme));
  OK:=INIFile^.Update;
  Dispose(INIFile, Done);
  WriteINIFile:=OK;
end;

end.
{
  $Log: fpini.pas,v $
  Revision 1.11  2002/12/12 00:03:14  pierre
   * fix problem with breakpoint conditions that showed up again after exit

  Revision 1.10  2002/11/28 12:55:06  pierre
   + save/retrieve remote support variables

  Revision 1.9  2002/11/21 00:37:56  pierre
   + some cross gdb enhancements

  Revision 1.8  2002/10/23 18:01:50  hajny
    * mistyping fixed

  Revision 1.7  2002/10/18 17:58:48  hajny
    * added missing include

  Revision 1.6  2002/09/07 15:40:43  peter
    * old logs removed and tabs fixed

  Revision 1.5  2002/08/13 07:12:08  pierre
   * use normal strings for ChoiceBox function

  Revision 1.4  2002/04/02 14:06:50  pierre
   * avoid a problem if resetting debuggee redirection

}
