{
    $Id: fpdebug.pas,v 1.46 2003/03/30 12:12:12 armin Exp $
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998-2000 by Pierre Muller

    Debugger call routines for the IDE

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FPDebug;
interface

{$i globdir.inc}

uses
{$ifdef win32}
  Windows,
{$endif win32}
  Objects,Dialogs,Drivers,Views,
  GDBCon,GDBInt,Menus,
  WViews,WEditor,
  FPViews;

type
  PDebugController=^TDebugController;
  TDebugController=object(TGDBController)
     InvalidSourceLine : boolean;

     { if true the current debugger raw will stay in middle of
       editor window when debugging PM }
     CenterDebuggerRow : boolean;
     Disableallinvalidbreakpoints : boolean;
     LastFileName : string;
     LastSource   : PView; {PsourceWindow !! }
     HiddenStepsCount : longint;
     { no need to switch if using another terminal }
     NoSwitch : boolean;
     HasExe   : boolean;
     RunCount : longint;
     WindowWidth : longint;
     FPCBreakErrorNumber : longint;
{$ifdef SUPPORT_REMOTE}
     isRemoteDebugging:boolean;
{$endif SUPPORT_REMOTE}
    constructor Init;
    procedure SetExe(const exefn:string);
    procedure SetWidth(AWidth : longint);
    procedure SetDirectories;
    destructor  Done;
    procedure DoSelectSourceline(const fn:string;line:longint);virtual;
{    procedure DoStartSession;virtual;
    procedure DoBreakSession;virtual;}
    procedure DoEndSession(code:longint);virtual;
    procedure DoUserSignal;virtual;
    procedure AnnotateError;
    procedure InsertBreakpoints;
    procedure RemoveBreakpoints;
    procedure ReadWatches;
    procedure RereadWatches;
    procedure ResetBreakpointsValues;
    procedure DoDebuggerScreen;virtual;
    procedure DoUserScreen;virtual;
    procedure Reset;virtual;
    procedure ResetDebuggerRows;
    procedure Run;virtual;
    procedure Continue;virtual;
    procedure UntilReturn;virtual;
    procedure CommandBegin(const s:string);virtual;
    procedure CommandEnd(const s:string);virtual;
    function  IsRunning : boolean;
    function  AllowQuit : boolean;virtual;
    function  GetValue(Const expr : string) : pchar;
    function  GetFramePointer : CORE_ADDR;
    function  GetLongintAt(addr : CORE_ADDR) : longint;
    function  GetPointerAt(addr : CORE_ADDR) : CORE_ADDR;
  end;

  BreakpointType = (bt_function,bt_file_line,bt_watch,
                    bt_awatch,bt_rwatch,bt_address,bt_invalid);
  BreakpointState = (bs_enabled,bs_disabled,bs_deleted,bs_delete_after);

  PBreakpointCollection=^TBreakpointCollection;

  PBreakpoint=^TBreakpoint;
  TBreakpoint=object(TObject)
     typ  : BreakpointType;
     state : BreakpointState;
     owner : PBreakpointCollection;
     Name : PString;  { either function name or expr to watch }
     FileName : PString;
     OldValue,CurrentValue : Pstring;
     Line : Longint; { only used for bt_file_line type }
     Conditions : PString; { conditions relative to that breakpoint }
     IgnoreCount : Longint; { how many counts should be ignored }
     Commands : pchar; { commands that should be executed on breakpoint }
     GDBIndex : longint;
     GDBState : BreakpointState;
     constructor Init_function(Const AFunc : String);
     constructor Init_Address(Const AAddress : String);
     constructor Init_Empty;
     constructor Init_file_line(AFile : String; ALine : longint);
     constructor Init_type(atyp : BreakpointType;Const AnExpr : String);
     constructor Load(var S: TStream);
     procedure   Store(var S: TStream);
     procedure  Insert;
     procedure  Remove;
     procedure  Enable;
     procedure  Disable;
     procedure  UpdateSource;
     procedure  ResetValues;
     destructor Done;virtual;
  end;

  TBreakpointCollection=object(TCollection)
      function  At(Index: Integer): PBreakpoint;
      function  GetGDB(index : longint) : PBreakpoint;
      function  GetType(typ : BreakpointType;Const s : String) : PBreakpoint;
      function  ToggleFileLine(FileName: String;LineNr : Longint) : boolean;
      procedure Update;
      procedure ShowBreakpoints(W : PFPWindow);
      function  FindBreakpointAt(Editor : PSourceEditor; Line : longint) : PBreakpoint;
      procedure AdaptBreakpoints(Editor : PSourceEditor; Pos, Change : longint);
      procedure ShowAllBreakpoints;
    end;

    PBreakpointItem = ^TBreakpointItem;
    TBreakpointItem = object(TObject)
      Breakpoint : PBreakpoint;
      constructor Init(ABreakpoint : PBreakpoint);
      function    GetText(MaxLen: Sw_integer): string; virtual;
      procedure   Selected; virtual;
      function    GetModuleName: string; virtual;
    end;

    PBreakpointsListBox = ^TBreakpointsListBox;
    TBreakpointsListBox = object(THSListBox)
      Transparent : boolean;
      NoSelection : boolean;
      MaxWidth    : Sw_integer;
      (* ModuleNames : PStoreCollection; *)
      constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar);
      procedure   AddBreakpoint(P: PBreakpointItem); virtual;
      function    GetText(Item,MaxLen: Sw_Integer): String; virtual;
      function    GetLocalMenu: PMenu;virtual;
      procedure   Clear; virtual;
      procedure   TrackSource; virtual;
      procedure   EditNew; virtual;
      procedure   EditCurrent; virtual;
      procedure   DeleteCurrent; virtual;
      procedure   ToggleCurrent;
      procedure   Draw; virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      destructor  Done; virtual;
    end;

    PBreakpointsWindow = ^TBreakpointsWindow;
    TBreakpointsWindow = object(TFPDlgWindow)
      BreakLB : PBreakpointsListBox;
      constructor Init;
      procedure   AddBreakpoint(ABreakpoint : PBreakpoint);
      procedure   ClearBreakpoints;
      procedure   ReloadBreakpoints;
      procedure   Close; virtual;
      procedure   SizeLimits(var Min, Max: TPoint);virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      procedure   Update; virtual;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      destructor  Done; virtual;
    end;

    PBreakpointItemDialog = ^TBreakpointItemDialog;

    TBreakpointItemDialog = object(TCenterDialog)
      constructor Init(ABreakpoint: PBreakpoint);
      function    Execute: Word; virtual;
    private
      Breakpoint : PBreakpoint;
      TypeRB   : PRadioButtons;
      NameIL  : PEditorInputLine;
      ConditionsIL: PEditorInputLine;
      LineIL    : PEditorInputLine;
      IgnoreIL  : PEditorInputLine;
    end;

    PWatch = ^TWatch;
    TWatch =  Object(TObject)
      constructor Init(s : string);
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      procedure rename(s : string);
      procedure Get_new_value;
      procedure Force_new_value;
      destructor done;virtual;
      expr : pstring;
    private
      GDBRunCount : longint;
      last_value,current_value : pchar;
    end;

    PWatchesCollection = ^TWatchesCollection;
    TWatchesCollection = Object(TCollection)
      constructor Init;
      procedure Insert(Item: Pointer); virtual;
      function  At(Index: Integer): PWatch;
      procedure Update;
    private
      MaxW : integer;
    end;

    PWatchesListBox = ^TWatchesListBox;
    TWatchesListBox = object(THSListBox)
      Transparent : boolean;
      MaxWidth    : Sw_integer;
      constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar);
      (* procedure   AddWatch(P: PWatch); virtual; *)
      procedure   Update(AMaxWidth : integer);
      function    GetText (Item: Sw_Integer; MaxLen: Sw_Integer): String; Virtual;
      function    GetIndentedText(Item,Indent,MaxLen: Sw_Integer;var Modified : boolean): String; virtual;
      function    GetLocalMenu: PMenu;virtual;
      (* procedure   Clear; virtual;
      procedure   TrackSource; virtual;*)
      procedure   EditNew; virtual;
      procedure   EditCurrent; virtual;
      procedure   DeleteCurrent; virtual;
      (*procedure   ToggleCurrent; *)
      procedure   Draw; virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      destructor  Done; virtual;
    end;

    PWatchItemDialog = ^TWatchItemDialog;

    TWatchItemDialog = object(TCenterDialog)
      constructor Init(AWatch: PWatch);
      function    Execute: Word; virtual;
    private
      Watch : PWatch;
      NameIL  : PEditorInputLine;
      TextST : PAdvancedStaticText;
    end;

    PWatchesWindow = ^TWatchesWindow;
    TWatchesWindow = Object(TFPDlgWindow)
      WLB : PWatchesListBox;
      Constructor Init;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      procedure   Update; virtual;
      destructor  Done; virtual;
    end;

    PFramesListBox = ^TFramesListBox;
    TFramesListBox = object(TMessageListBox)
      constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar);
      procedure   Update;
      function    GetLocalMenu: PMenu;virtual;
      procedure   GotoSource; virtual;
      procedure   GotoAssembly; virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      destructor  Done; virtual;
    end;

    PStackWindow = ^TStackWindow;
    TStackWindow = Object(TFPDlgWindow)
      FLB : PFramesListBox;
      Constructor Init;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      procedure   Update; virtual;
      destructor  Done; virtual;
    end;

  procedure InitStackWindow;
  procedure DoneStackWindow;

  function  ActiveBreakpoints : boolean;
  function  GDBFileName(st : string) : string;
  function  OSFileName(st : string) : string;


const
     BreakpointTypeStr : Array[BreakpointType] of String[9]
       = ( 'function','file-line','watch','awatch','rwatch','address','invalid');
     BreakpointStateStr : Array[BreakpointState] of String[8]
       = ( 'enabled','disabled','invalid',''{'to be deleted' should never be used});

var
  Debugger             : PDebugController;
  BreakpointsCollection : PBreakpointCollection;
  WatchesCollection    : PwatchesCollection;

procedure InitDebugger;
procedure DoneDebugger;
procedure InitGDBWindow;
procedure DoneGDBWindow;
procedure InitDisassemblyWindow;
procedure DoneDisassemblyWindow;
procedure InitBreakpoints;
procedure DoneBreakpoints;
procedure InitWatches;
procedure DoneWatches;

procedure RegisterFPDebugViews;

procedure UpdateDebugViews;

implementation

uses
  Dos,
{$ifdef fpc}
  Video,
{$endif fpc}
{$ifdef DOS}
  fpusrscr,
{$endif DOS}
  App,Strings,
{$ifdef FVISION}
  FVConsts,
{$else}
  Commands,HelpCtx,
{$endif}
{$ifdef win32}
  Windebug,
{$endif win32}
{$ifdef Unix}
  {$ifdef VER1_0}
    Linux,
  {$else}
    Unix,
  {$endif}
{$endif Unix}
  Systems,Globals,
  FPRegs,
  FPString,FPVars,FPUtils,FPConst,FPSwitch,
  FPIntf,FPCompil,FPIde,FPHelp,
  Validate,WUtils,Wconsts;

const
  RBreakpointsWindow: TStreamRec = (
     ObjType: 1701;
     VmtLink: Ofs(TypeOf(TBreakpointsWindow)^);
     Load:    @TBreakpointsWindow.Load;
     Store:   @TBreakpointsWindow.Store
  );

  RBreakpointsListBox : TStreamRec = (
     ObjType: 1702;
     VmtLink: Ofs(TypeOf(TBreakpointsListBox)^);
     Load:    @TBreakpointsListBox.Load;
     Store:   @TBreakpointsListBox.Store
  );

  RWatchesWindow: TStreamRec = (
     ObjType: 1703;
     VmtLink: Ofs(TypeOf(TWatchesWindow)^);
     Load:    @TWatchesWindow.Load;
     Store:   @TWatchesWindow.Store
  );

  RWatchesListBox: TStreamRec = (
     ObjType: 1704;
     VmtLink: Ofs(TypeOf(TWatchesListBox)^);
     Load:    @TWatchesListBox.Load;
     Store:   @TWatchesListBox.Store
  );

  RStackWindow: TStreamRec = (
     ObjType: 1705;
     VmtLink: Ofs(TypeOf(TStackWindow)^);
     Load:    @TStackWindow.Load;
     Store:   @TStackWindow.Store
  );

  RFramesListBox: TStreamRec = (
     ObjType: 1706;
     VmtLink: Ofs(TypeOf(TFramesListBox)^);
     Load:    @TFramesListBox.Load;
     Store:   @TFramesListBox.Store
  );

  RBreakpoint: TStreamRec = (
     ObjType: 1707;
     VmtLink: Ofs(TypeOf(TBreakpoint)^);
     Load:    @TBreakpoint.Load;
     Store:   @TBreakpoint.Store
  );

  RWatch: TStreamRec = (
     ObjType: 1708;
     VmtLink: Ofs(TypeOf(TWatch)^);
     Load:    @TWatch.Load;
     Store:   @TWatch.Store
  );

  RBreakpointCollection: TStreamRec = (
     ObjType: 1709;
     VmtLink: Ofs(TypeOf(TBreakpointCollection)^);
     Load:    @TBreakpointCollection.Load;
     Store:   @TBreakpointCollection.Store
  );

  RWatchesCollection: TStreamRec = (
     ObjType: 1710;
     VmtLink: Ofs(TypeOf(TWatchesCollection)^);
     Load:    @TWatchesCollection.Load;
     Store:   @TWatchesCollection.Store
  );

{$ifdef I386}
const
  FrameName = '$ebp';
{$define FrameNameKnown}
{$endif i386}
{$ifdef m68k}
const
  FrameName = '$fp';
{$define FrameNameKnown}
{$endif m68k}
{$ifdef powerpc}
  { stack and frame registers are the same on powerpc,
    so I am not sure that this will work PM }
const
  FrameName = '$r1';
{$define FrameNameKnown}
{$endif powerpc}

{$ifdef TP}
function HexStr(Value: longint; Len: byte): string;
begin
  HexStr:=IntToHex(Value,Len);
end;
{$endif}


function  GDBFileName(st : string) : string;
{$ifndef Unix}
var i : longint;
{$endif Unix}
begin
{$ifdef Unix}
  GDBFileName:=st;
{$else}
{ should we also use / chars ? }
  for i:=1 to Length(st) do
    if st[i]='\' then
{$ifdef win32}
  { Don't touch at '\ ' used to escapes spaces in windows file names PM }
     if (i=length(st)) or (st[i+1]<>' ') then
{$endif win32}
      st[i]:='/';
{$ifdef win32}
{ for win32 we should convert e:\ into //e/ PM }
  if (length(st)>2) and (st[2]=':') and (st[3]='/') then
    st:=CygDrivePrefix+'/'+st[1]+copy(st,3,length(st));
{ support spaces in the name by escaping them but without changing '\ ' into '\\ ' }
  for i:=Length(st) downto 1 do
    if (st[i]=' ') and ((i=1) or (st[i-1]<>'\')) then
      st:=copy(st,1,i-1)+'\'+copy(st,i,length(st));
{$endif win32}
{$ifdef go32v2}
{ for go32v2 we should convert //e/ back into e:/  PM }
  if (length(st)>3) and (st[1]='/') and (st[2]='/') and (st[4]='/') then
    st:=st[3]+':/'+copy(st,5,length(st));
{$endif go32v2}
  GDBFileName:=LowerCaseStr(st);
{$endif}
end;

function  OSFileName(st : string) : string;
{$ifndef Unix}
var i : longint;
{$endif Unix}
begin
{$ifdef Unix}
  OSFileName:=st;
{$else}
{$ifdef win32}
{ for win32 we should convert /cygdrive/e/ into e:\ PM }
  if pos(CygDrivePrefix+'/',st)=1 then
    st:=st[Length(CygdrivePrefix)+2]+':\'+copy(st,length(CygdrivePrefix)+4,length(st));
{$endif win32}
{ support spaces in the name by escaping them but without changing '\ ' into '\\ ' }
  for i:=Length(st) downto 2 do
    if (st[i]=' ') and (st[i-1]='\') then
      st:=copy(st,1,i-2)+copy(st,i,length(st));
{$ifdef go32v2}
{ for go32v2 we should convert //e/ back into e:/  PM }
  if (length(st)>3) and (st[1]='/') and (st[2]='/') and (st[4]='/') then
    st:=st[3]+':\'+copy(st,5,length(st));
{$endif go32v2}
{ should we also use / chars ? }
  for i:=1 to Length(st) do
    if st[i]='/' then
      st[i]:='\';
  OSFileName:=LowerCaseStr(st);
{$endif}
end;

{****************************************************************************
                            TDebugController
****************************************************************************}

procedure UpdateDebugViews;

  begin
{$ifdef SUPPORT_REMOTE}
     PushStatus(msg_getting_info_on+RemoteMachine);
{$endif SUPPORT_REMOTE}
     DeskTop^.Lock;
     If assigned(StackWindow) then
       StackWindow^.Update;
     If assigned(RegistersWindow) then
       RegistersWindow^.Update;
     If assigned(Debugger) then
       Debugger^.ReadWatches;
     If assigned(FPUWindow) then
       FPUWindow^.Update;
     DeskTop^.UnLock;
{$ifdef SUPPORT_REMOTE}
     PopStatus;
{$endif SUPPORT_REMOTE}
  end;

constructor TDebugController.Init;
begin
  inherited Init;
  CenterDebuggerRow:=IniCenterDebuggerRow;
  Disableallinvalidbreakpoints:=false;
  NoSwitch:=False;
  HasExe:=false;
  Debugger:=@self;
  WindowWidth:=-1;
{$ifndef GABOR}
  switch_to_user:=true;
{$endif}
  Command('set print object off');
end;

procedure TDebugController.SetExe(const exefn:string);
  var f : string;
begin
  f := GDBFileName(GetShortName(exefn));
  if (f<>'') and ExistsFile(exefn) then
    begin
      LoadFile(f);
      HasExe:=true;
      Command('b FPC_BREAK_ERROR');
      FPCBreakErrorNumber:=last_breakpoint_number;
{$ifdef FrameNameKnown}
      { this fails in GDB 5.1 because
        GDB replies that there is an attempt to dereference
        a generic pointer...
        test delayed in DoSourceLine... PM
      Command('cond '+IntToStr(FPCBreakErrorNumber)+
        ' (('+FrameName+' + 8)^ <> 0) or'+
        ' (('+FrameName+' + 12)^ <> 0)');  }
{$endif FrameNameKnown}
      SetArgs(GetRunParameters);
      SetDirectories;
      InsertBreakpoints;
      ReadWatches;
    end
  else
    begin
      HasExe:=false;
      Command('file');
    end;
end;

procedure TDebugController.SetWidth(AWidth : longint);
begin
  WindowWidth:=AWidth;
  Command('set width '+inttostr(WindowWidth));
end;

procedure TDebugController.SetDirectories;
  var f,s: string;
      i : longint;
      Dir : SearchRec;
begin
  f:=GetSourceDirectories;
  repeat
    i:=pos(';',f);
    if i=0 then
        s:=f
    else
      begin
        s:=copy(f,1,i-1);
        system.delete(f,1,i);
      end;
    DefaultReplacements(s);
    if (pos('*',s)=0) and ExistsDir(s) then
      Command('dir '+GDBFileName(GetShortName(s)))
    { we should also handle the /* cases of -Fu option }
    else if pos('*',s)>0 then
      begin
        Dos.FindFirst(s,Directory,Dir);
        { the '*' can only be in the last dir level }
        s:=DirOf(s);
        while Dos.DosError=0 do
          begin
            if ((Dir.attr and Directory) <> 0) and ExistsDir(s+Dir.Name) then
              Command('dir '+GDBFileName(GetShortName(s+Dir.Name)));
            Dos.FindNext(Dir);
          end;
{$ifdef FPC}
        Dos.FindClose(Dir);
{$endif def FPC}
      end;
  until i=0;
end;

procedure TDebugController.InsertBreakpoints;
  procedure DoInsert(PB : PBreakpoint);
  begin
    PB^.Insert;
  end;

begin
  BreakpointsCollection^.ForEach(@DoInsert);
  Disableallinvalidbreakpoints:=false;
end;

procedure TDebugController.ReadWatches;

  procedure DoRead(PB : PWatch);
  begin
    PB^.Get_new_value;
  end;

begin
  WatchesCollection^.ForEach(@DoRead);
  If Assigned(WatchesWindow) then
    WatchesWindow^.Update;
end;

procedure TDebugController.RereadWatches;

  procedure DoRead(PB : PWatch);
  begin
    PB^.Force_new_value;
  end;

begin
  WatchesCollection^.ForEach(@DoRead);
  If Assigned(WatchesWindow) then
    WatchesWindow^.Update;
end;


procedure TDebugController.RemoveBreakpoints;
  procedure DoDelete(PB : PBreakpoint);
    begin
      PB^.Remove;
    end;
begin
   BreakpointsCollection^.ForEach(@DoDelete);
end;

procedure TDebugController.ResetBreakpointsValues;
  procedure DoResetVal(PB : PBreakpoint);
    begin
      PB^.ResetValues;
    end;
begin
   BreakpointsCollection^.ForEach(@DoResetVal);
end;

function  ActiveBreakpoints : boolean;
  var
    IsActive : boolean;

  procedure TestActive(PB : PBreakpoint);
    begin
        If PB^.state=bs_enabled then
          IsActive:=true;
    end;
begin
   IsActive:=false;
   If assigned(BreakpointsCollection) then
     BreakpointsCollection^.ForEach(@TestActive);
   ActiveBreakpoints:=IsActive;
end;


destructor TDebugController.Done;
begin
  { kill the program if running }
  Reset;
  RemoveBreakpoints;
  inherited Done;
end;


procedure TDebugController.Run;
{$ifdef Unix}
var
  Debuggeefile : text;
  ResetOK, TTYUsed  : boolean;
{$endif Unix}
{$ifdef PALMOSGDB}
const
  TargetProtocol = 'palmos';
{$else}
const
  TargetProtocol = 'remote';
{$endif PALMOSGDB}

{$ifdef SUPPORT_REMOTE}
var
  S,ErrorStr : string;
{$endif SUPPORT_REMOTE}
begin
  ResetBreakpointsValues;
{$ifdef SUPPORT_REMOTE}
  NoSwitch:=true;
  isRemoteDebugging:=false;
{$ifndef CROSSGDB}
  If (RemoteMachine<>'') and (RemotePort<>'') then
{$else CROSSGDB}
  if true then
{$endif CROSSGDB}
    begin
      isRemoteDebugging:=true;
      S:=RemoteMachine;
      If pos('@',S)>0 then
        S:=copy(S,pos('@',S)+1,High(S));
      If RemotePort<>'' then
        S:=S+':'+RemotePort;
{$ifdef PALMOSGDB}
      { set the default value for PalmOS }
      If S='' then
        S:='localhost:2000';
{$endif PALMOSGDB}
      PushStatus(msg_connectingto+S);
      Command('target '+TargetProtocol+' '+S);
      if Error then
        begin
           ErrorStr:=strpas(GetError);
           ErrorBox(#3'Error in "target '+TargetProtocol+'"'#13#3+ErrorStr,nil);
           PopStatus;
           exit;
        end;
      PopStatus;
    end
  else
    begin
{$endif SUPPORT_REMOTE}
{$ifdef win32}
  { Run the debugge in another console }
  if DebuggeeTTY<>'' then
    Command('set new-console on')
  else
    Command('set new-console off');
  NoSwitch:=DebuggeeTTY<>'';
{$endif win32}
{$ifdef Unix}
  { Run the debuggee in another tty }
  if DebuggeeTTY <> '' then
    begin
{$I-}
      Assign(Debuggeefile,DebuggeeTTY);
      system.Reset(Debuggeefile);
      ResetOK:=IOResult=0;
      If ResetOK and IsATTY(textrec(Debuggeefile).handle) then
        begin
          Command('tty '+DebuggeeTTY);
          TTYUsed:=true;
        end
      else
        begin
          Command('tty ');
          TTYUsed:=false;
        end;
      if ResetOK then
        close(Debuggeefile);
      if TTYUsed and (DebuggeeTTY<>TTYName(stdout)) then
        NoSwitch:= true
      else
        NoSwitch:=false;
    end
  else
    begin
      if TTYName(input)<>'' then
        Command('tty '+TTYName(input));
      NoSwitch := false;
    end;
{$endif Unix}
{$ifdef SUPPORT_REMOTE}
    end;
{$endif SUPPORT_REMOTE}
  { Switch to user screen to get correct handles }
  UserScreen;
  { Don't try to print GDB messages while in User Screen mode }
  If assigned(GDBWindow) then
    GDBWindow^.Editor^.Lock;
{$ifdef SUPPORT_REMOTE}
  if isRemoteDebugging then
  begin
    inc(init_count);
    { pass the stop in start code }
    Command('continue');
  end else
{$endif SUPPORT_REMOTE}
  inherited Run;
  DebuggerScreen;
  If assigned(GDBWindow) then
    GDBWindow^.Editor^.UnLock;
  IDEApp.SetCmdState([cmResetDebugger,cmUntilReturn],true);
  IDEApp.UpdateRunMenu(true);
  UpdateDebugViews;
end;


function TDebugController.IsRunning : boolean;
begin
  IsRunning:=debuggee_started;
end;

procedure TDebugController.Continue;
begin
{$ifdef NODEBUG}
  NoDebugger;
{$else}
  if not debuggee_started then
    Run
  else
    inherited Continue;
  UpdateDebugViews;
{$endif NODEBUG}
end;

procedure TDebugController.UntilReturn;
begin
  Command('finish');
  UpdateDebugViews;
  { We could try to get the return value !
    Not done yet }
end;


procedure TDebugController.CommandBegin(const s:string);
begin
  if assigned(GDBWindow) and (in_command>1) then
    begin
      { We should do something special for errors !! }
      If StrLen(GetError)>0 then
        GDBWindow^.WriteErrorText(GetError);
      GDBWindow^.WriteOutputText(GetOutput);
    end;
  if assigned(GDBWindow) then
    GDBWindow^.WriteString(S);
end;

procedure TDebugController.CommandEnd(const s:string);
begin
  if assigned(GDBWindow) and (in_command<=1) then
    begin
      { We should do something special for errors !! }
      If StrLen(GetError)>0 then
        GDBWindow^.WriteErrorText(GetError);
      GDBWindow^.WriteOutputText(GetOutput);
      GDBWindow^.Editor^.TextEnd;
    end;
end;

function  TDebugController.AllowQuit : boolean;
begin
  if IsRunning then
    begin
      if ConfirmBox('Really quit GDB window'#13+
         'and kill running program?',nil,true)=cmYes then
        begin
           Reset;
           DoneGDBWindow;
           {AllowQuit:=true;}
           AllowQuit:=false;
        end
      else
        AllowQuit:=false;
    end
  else if ConfirmBox('Really quit GDB window?',nil,true)=cmYes then
    begin
      DoneGDBWindow;
      {AllowQuit:=true;}
      AllowQuit:=false;
    end
  else
    AllowQuit:=false;
end;

procedure TDebugController.ResetDebuggerRows;
  procedure ResetDebuggerRow(P: PView); {$ifndef FPC}far;{$endif}
  begin
    if assigned(P) and
       (TypeOf(P^)=TypeOf(TSourceWindow)) then
       PSourceWindow(P)^.Editor^.SetLineFlagExclusive(lfDebuggerRow,-1);
  end;

begin
  Desktop^.ForEach(@ResetDebuggerRow);
end;

procedure TDebugController.Reset;
begin
  inherited Reset;
  { we need to free the executable
    if we want to recompile it }
  SetExe('');
  NoSwitch:=false;
  { In case we have something that the compiler touched }
  If IDEApp.IsRunning then
    begin
      IDEApp.SetCmdState([cmResetDebugger,cmUntilReturn],false);
      IDEApp.UpdateRunMenu(false);
      AskToReloadAllModifiedFiles;
      ResetDebuggerRows;
    end;
end;

procedure TDebugController.AnnotateError;
var errornb : longint;
begin
  if error then
    begin
       errornb:=error_num;
       UpdateDebugViews;
       ErrorBox(#3'Error within GDB'#13#3'Error code = %d',@errornb);
    end;
end;

function TDebugController.GetValue(Const expr : string) : pchar;
var
  p,p2,p3 : pchar;
begin
  if WindowWidth<>-1 then
    Command('set width 0xffffffff');
  Command('p '+expr);
  p:=GetOutput;
  p3:=nil;
  if assigned(p) and (p[strlen(p)-1]=#10) then
   begin
     p3:=p+strlen(p)-1;
     p3^:=#0;
   end;
  if assigned(p) then
    p2:=strpos(p,'=')
  else
    p2:=nil;
  if assigned(p2) then
    p:=p2+1;
  while p^ in [' ',TAB] do
    inc(p);
  { get rid of type }
  if p^ = '(' then
    p:=strpos(p,')')+1;
  while p^ in [' ',TAB] do
    inc(p);
  if assigned(p) then
    GetValue:=StrNew(p)
  else
    GetValue:=StrNew(GetError);
  if assigned(p3) then
    p3^:=#10;
  got_error:=false;
  if WindowWidth<>-1 then
    Command('set width '+IntToStr(WindowWidth));
end;

function TDebugController.GetFramePointer : CORE_ADDR;
var
  st : string;
  p : longint;
begin
{$ifdef FrameNameKnown}
  Command('p /d '+FrameName);
  st:=strpas(GetOutput);
  p:=pos('=',st);
  while (p<length(st)) and (st[p+1] in [' ',#9]) do
    inc(p);
  Delete(st,1,p);
  p:=1;
  while (st[p] in ['0'..'9']) do
    inc(p);
  Delete(st,p,High(st));
  GetFramePointer:=StrToCard(st);
{$else not FrameNameKnown}
  GetFramePointer:=0;
{$endif not FrameNameKnown}
end;

function TDebugController.GetLongintAt(addr : CORE_ADDR) : longint;
var
  st : string;
  p : longint;
begin
  Command('x /wd 0x'+hexstr(longint(addr),8));
  st:=strpas(GetOutput);
  p:=pos(':',st);
  while (p<length(st)) and (st[p+1] in [' ',#9]) do
    inc(p);
  Delete(st,1,p);
  p:=1;
  while (st[p] in ['0'..'9']) do
    inc(p);
  Delete(st,p,High(st));
  GetLongintAt:=StrToInt(st);
end;

function TDebugController.GetPointerAt(addr : CORE_ADDR) : CORE_ADDR;
var
  val : CORE_ADDR;
  st : string;
  p : longint;
begin
  Command('x /wx 0x'+hexstr(longint(addr),8));
  st:=strpas(GetOutput);
  p:=pos(':',st);
  while (p<length(st)) and (st[p+1] in [' ',#9]) do
    inc(p);
  if (p<length(st)) and (st[p+1]='$') then
    inc(p);
  Delete(st,1,p);
  p:=1;
  while (st[p] in ['0'..'9','A'..'F','a'..'f']) do
    inc(p);
  Delete(st,p,High(st));
  GetPointerAt:=HexToCard(st);
end;

procedure TDebugController.DoSelectSourceLine(const fn:string;line:longint);
var
  W: PSourceWindow;
  Found : boolean;
  PB : PBreakpoint;
  S : String;
  BreakIndex : longint;
  ebp,stop_addr : CORE_ADDR;
  i,ExitCode : longint;
  ExitAddr,ExitFrame : CORE_ADDR;
const
  FirstArgOffset = 2 * sizeof(CORE_ADDR);
  SecondArgOffset = 3 * sizeof(CORE_ADDR);
  ThirdArgOffset = 4 * sizeof(CORE_ADDR);

begin
  BreakIndex:=stop_breakpoint_number;
  Desktop^.Lock;
  { 0 based line count in Editor }
  if Line>0 then
    dec(Line);

  S:=fn;
  stop_addr:=current_pc;

  if (BreakIndex=FPCBreakErrorNumber) then
    begin
      { Procedure HandleErrorAddrFrame
         (Errno : longint;addr,frame : longint);
         [public,alias:'FPC_BREAK_ERROR']; }
{$ifdef FrameNameKnown}
      ExitCode:=GetLongintAt(GetFramePointer+FirstArgOffset);
      ExitAddr:=GetPointerAt(GetFramePointer+SecondArgOffset);
      ExitFrame:=GetPointerAt(GetFramePointer+ThirdArgOffset);
      if (ExitCode=0) and (ExitAddr=0) then
        begin
          Desktop^.Unlock;
          Command('continue');
          exit;
        end;
      { forget all old frames }
      clear_frames;
      { record new frames }
      Command('backtrace');
      for i:=0 to frame_count-1 do
        begin
          with frames[i]^ do
            begin
              if ExitAddr=address then
                begin
                  Command('f '+IntToStr(i));
                  if assigned(file_name) then
                    begin
                      s:=strpas(file_name);
                      line:=line_number;
                      stop_addr:=address;
                    end;
                  break;
                end;
            end;
        end;
{$endif FrameNameKnown}
    end;
  { Update Disassembly position }
  if Assigned(DisassemblyWindow) then
    DisassemblyWindow^.SetCurAddress(stop_addr);

  if (fn=LastFileName) then
    begin
      W:=PSourceWindow(LastSource);
      if assigned(W) then
        begin
          W^.Editor^.SetCurPtr(0,Line);
          W^.Editor^.TrackCursor(CenterDebuggerRow);
          W^.Editor^.SetLineFlagExclusive(lfDebuggerRow,Line);
          UpdateDebugViews;

          {if Not assigned(GDBWindow) or not GDBWindow^.GetState(sfActive) then
            handled by SelectInDebugSession}
          W^.SelectInDebugSession;
          InvalidSourceLine:=false;
        end
      else
        InvalidSourceLine:=true;
    end
  else
    begin
      if s='' then
        W:=nil
      else
        W:=TryToOpenFile(nil,s,0,Line,false);
      if assigned(W) then
        begin
          W^.Editor^.SetLineFlagExclusive(lfDebuggerRow,Line);
          W^.Editor^.TrackCursor(CenterDebuggerRow);
          UpdateDebugViews;
          {if Not assigned(GDBWindow) or not GDBWindow^.GetState(sfActive) then
            handled by SelectInDebugSession}
          W^.SelectInDebugSession;
          LastSource:=W;
          InvalidSourceLine:=false;
        end
        { only search a file once }
      else
       begin
         Desktop^.UnLock;
         if s='' then
           Found:=false
         else
         { it is easier to handle with a * at the end }
           Found:=IDEApp.OpenSearch(s+'*');
         Desktop^.Lock;
         if not Found then
           begin
             InvalidSourceLine:=true;
             LastSource:=Nil;
             { Show the stack in that case }
             InitStackWindow;
             UpdateDebugViews;
             StackWindow^.MakeFirst;
           end
         else
           begin
             { should now be open }
              W:=TryToOpenFile(nil,s,0,Line,true);
              W^.Editor^.SetLineFlagExclusive(lfDebuggerRow,Line);
              W^.Editor^.TrackCursor(CenterDebuggerRow);
              UpdateDebugViews;
              {if Not assigned(GDBWindow) or not GDBWindow^.GetState(sfActive) then
                handled by SelectInDebugSession}
              W^.SelectInDebugSession;
              LastSource:=W;
              InvalidSourceLine:=false;
           end;
       end;
    end;
  LastFileName:=s;
  Desktop^.UnLock;
  if BreakIndex>0 then
    begin
      PB:=BreakpointsCollection^.GetGDB(BreakIndex);
      if (BreakIndex=FPCBreakErrorNumber) then
       begin
          if (ExitCode<>0) or (ExitAddr<>0) then
            WarningBox(#3'Run Time Error '+IntToStr(ExitCode)+#13+
                     #3'Error address $'+IntToHex(ExitAddr,8),nil)
          else
            WarningBox(#3'Run Time Error',nil);
       end
      else if not assigned(PB) then
        begin
          WarningBox(#3'Stopped by breakpoint '+IntToStr(BreakIndex),nil);
        end
      { For watch we should get old and new value !! }
      else if (Not assigned(GDBWindow) or not GDBWindow^.GetState(sfActive)) and
         (PB^.typ<>bt_file_line) and (PB^.typ<>bt_function) and
         (PB^.typ<>bt_address) then
        begin
           Command('p '+GetStr(PB^.Name));
           S:=GetPChar(GetOutput);
           got_error:=false;
           If Pos('=',S)>0 then
             S:=Copy(S,Pos('=',S)+1,255);
           If S[Length(S)]=#10 then
             Delete(S,Length(S),1);
           if Assigned(PB^.OldValue) then
             DisposeStr(PB^.OldValue);
           PB^.OldValue:=PB^.CurrentValue;
           PB^.CurrentValue:=NewStr(S);
           If PB^.typ=bt_function then
             WarningBox(#3'GDB stopped due to'#13+
               #3+BreakpointTypeStr[PB^.typ]+' '+GetStr(PB^.Name),nil)
           else if (GetStr(PB^.OldValue)<>S) then
             WarningBox(#3'GDB stopped due to'#13+
               #3+BreakpointTypeStr[PB^.typ]+' '+GetStr(PB^.Name)+#13+
               #3+'Old value = '+GetStr(PB^.OldValue)+#13+
               #3+'New value = '+GetStr(PB^.CurrentValue),nil)
           else
             WarningBox(#3'GDB stopped due to'#13+
               #3+BreakpointTypeStr[PB^.typ]+' '+GetStr(PB^.Name)+#13+
               #3+' value = '+GetStr(PB^.CurrentValue),nil);
        end;
    end;
end;

procedure TDebugController.DoUserSignal;
var P :Array[1..2] of pstring;
    S1, S2 : string;
begin
  S1:=strpas(signal_name);
  S2:=strpas(signal_string);
  P[1]:=@S1;
  P[2]:=@S2;
  WarningBox(msg_programsignal,@P);
end;

procedure TDebugController.DoEndSession(code:longint);
var P :Array[1..2] of longint;
begin
   IDEApp.SetCmdState([cmUntilReturn,cmResetDebugger],false);
   IDEApp.UpdateRunMenu(false);
   ResetDebuggerRows;
   LastExitCode:=Code;
   If HiddenStepsCount=0 then
     InformationBox(msg_programexitedwithexitcode,@code)
   else
     begin
        P[1]:=code;
        P[2]:=HiddenStepsCount;
        WarningBox(msg_programexitedwithcodeandsteps,@P);
     end;
  { In case we have something that the compiler touched }
  AskToReloadAllModifiedFiles;
{$ifdef win32}
  main_pid_valid:=false;
{$endif win32}
end;


procedure TDebugController.DoDebuggerScreen;
{$ifdef win32}
  var
   IdeMode : DWord;
{$endif win32}
begin
  if NoSwitch then
    begin
      PopStatus;
    end
  else
    begin
      IDEApp.ShowIDEScreen;
      Message(Application,evBroadcast,cmDebuggerStopped,pointer(RunCount));
      PopStatus;
    end;
{$ifdef win32}
   if NoSwitch then
     begin
       { Ctrl-C as normal char }
       GetConsoleMode(GetStdHandle(cardinal(Std_Input_Handle)), @IdeMode);
       IdeMode:=(IdeMode or ENABLE_MOUSE_INPUT or ENABLE_WINDOW_INPUT) and not ENABLE_PROCESSED_INPUT;
       SetConsoleMode(GetStdHandle(cardinal(Std_Input_Handle)), IdeMode);
     end;
   ChangeDebuggeeWindowTitleTo(Stopped_State);
{$endif win32}
end;


procedure TDebugController.DoUserScreen;

{$ifdef win32}
  var
   IdeMode : DWord;
{$endif win32}
begin
  Inc(RunCount);
  if NoSwitch then
    begin
{$ifdef SUPPORT_REMOTE}
      PushStatus(msg_runningremotely+RemoteMachine);
{$else not SUPPORT_REMOTE}
{$ifdef Unix}
      PushStatus(msg_runninginanotherwindow+DebuggeeTTY);
{$else not Unix}
      PushStatus(msg_runninginanotherwindow);
{$endif Unix}
{$endif not SUPPORT_REMOTE}
    end
  else
    begin
      PushStatus(msg_runningprogram);
      IDEApp.ShowUserScreen;
    end;
{$ifdef win32}
   if NoSwitch then
     begin
       { Ctrl-C as interrupt }
       GetConsoleMode(GetStdHandle(cardinal(Std_Input_Handle)), @IdeMode);
       IdeMode:=(IdeMode or ENABLE_MOUSE_INPUT or ENABLE_PROCESSED_INPUT or ENABLE_WINDOW_INPUT);
       SetConsoleMode(GetStdHandle(cardinal(Std_Input_Handle)), IdeMode);
     end;
   ChangeDebuggeeWindowTitleTo(Running_State);
{$endif win32}
end;

{****************************************************************************
                                 TBreakpoint
****************************************************************************}

constructor TBreakpoint.Init_function(Const AFunc : String);
begin
  typ:=bt_function;
  state:=bs_enabled;
  GDBState:=bs_deleted;
  Name:=NewStr(AFunc);
  FileName:=nil;
  Line:=0;
  IgnoreCount:=0;
  Commands:=nil;
  Conditions:=nil;
  OldValue:=nil;
  CurrentValue:=nil;
end;

constructor TBreakpoint.Init_Address(Const AAddress : String);
begin
  typ:=bt_address;
  state:=bs_enabled;
  GDBState:=bs_deleted;
  Name:=NewStr(AAddress);
  FileName:=nil;
  Line:=0;
  IgnoreCount:=0;
  Commands:=nil;
  Conditions:=nil;
  OldValue:=nil;
  CurrentValue:=nil;
end;

constructor TBreakpoint.Init_Empty;
begin
  typ:=bt_function;
  state:=bs_enabled;
  GDBState:=bs_deleted;
  Name:=Nil;
  FileName:=nil;
  Line:=0;
  IgnoreCount:=0;
  Commands:=nil;
  Conditions:=nil;
  OldValue:=nil;
  CurrentValue:=nil;
end;

constructor TBreakpoint.Init_type(atyp : BreakpointType;Const AnExpr : String);
begin
  typ:=atyp;
  state:=bs_enabled;
  GDBState:=bs_deleted;
  Name:=NewStr(AnExpr);
  IgnoreCount:=0;
  Commands:=nil;
  Conditions:=nil;
  OldValue:=nil;
  CurrentValue:=nil;
end;

constructor TBreakpoint.Init_file_line(AFile : String; ALine : longint);
var
  CurDir : String;
begin
  typ:=bt_file_line;
  state:=bs_enabled;
  GDBState:=bs_deleted;
  { d:test.pas:12 does not work !! }
  { I do not know how to solve this if
  if (Length(AFile)>1) and (AFile[2]=':') then
    AFile:=Copy(AFile,3,255);        }
{$ifdef Unix}
  CurDir:=GetCurDir;
{$else}
  CurDir:=LowerCaseStr(GetCurDir);
{$endif Unix}
  if Pos(CurDir,OSFileName(FEXpand(AFile)))=1 then
    FileName:=NewStr(Copy(OSFileName(FExpand(AFile)),length(CurDir)+1,255))
  else
    FileName:=NewStr(OSFileName(FExpand(AFile)));
  Name:=nil;
  Line:=ALine;
  IgnoreCount:=0;
  Commands:=nil;
  Conditions:=nil;
  OldValue:=nil;
  CurrentValue:=nil;
end;

constructor TBreakpoint.Load(var S: TStream);
var
  FName : PString;
begin
  S.Read(typ,SizeOf(BreakpointType));
  S.Read(state,SizeOf(BreakpointState));
  GDBState:=bs_deleted;
  case typ of
    bt_file_line :
      begin
        { convert to current target }
        FName:=S.ReadStr;
        FileName:=NewStr(OSFileName(GetStr(FName)));
        If Assigned(FName) then
          DisposeStr(FName);
        S.Read(Line,SizeOf(Line));
        Name:=nil;
      end;
  else
    begin
        Name:=S.ReadStr;
        Line:=0;
        FileName:=nil;
    end;
  end;
  S.Read(IgnoreCount,SizeOf(IgnoreCount));
  Commands:=S.StrRead;
  Conditions:=S.ReadStr;
  OldValue:=nil;
  CurrentValue:=nil;
end;

procedure TBreakpoint.Store(var S: TStream);
var
  St : String;
begin
  S.Write(typ,SizeOf(BreakpointType));
  S.Write(state,SizeOf(BreakpointState));
  case typ of
    bt_file_line :
      begin
        st:=OSFileName(GetStr(FileName));
        S.WriteStr(@St);
        S.Write(Line,SizeOf(Line));
      end;
  else
    begin
        S.WriteStr(Name);
    end;
  end;
  S.Write(IgnoreCount,SizeOf(IgnoreCount));
  S.StrWrite(Commands);
  S.WriteStr(Conditions);
end;

procedure TBreakpoint.Insert;
  var
    p,p2 : pchar;
    st : string;
begin
  If not assigned(Debugger) then Exit;
  Remove;
  Debugger^.last_breakpoint_number:=0;
  if (GDBState=bs_deleted) and (state=bs_enabled) then
    begin
      if (typ=bt_file_line) and assigned(FileName) then
        Debugger^.Command('break '+GDBFileName(NameAndExtOf(GetStr(FileName)))+':'+IntToStr(Line))
      else if (typ=bt_function) and assigned(name) then
        Debugger^.Command('break '+name^)
      else if (typ=bt_address) and assigned(name) then
        Debugger^.Command('break *0x'+name^)
      else if (typ=bt_watch) and assigned(name) then
        Debugger^.Command('watch '+name^)
      else if (typ=bt_awatch) and assigned(name) then
        Debugger^.Command('awatch '+name^)
      else if (typ=bt_rwatch) and assigned(name) then
        Debugger^.Command('rwatch '+name^);
      if Debugger^.last_breakpoint_number<>0 then
        begin
          GDBIndex:=Debugger^.last_breakpoint_number;
          GDBState:=bs_enabled;
          Debugger^.Command('cond '+IntToStr(GDBIndex)+' '+GetStr(Conditions));
          If IgnoreCount>0 then
            Debugger^.Command('ignore '+IntToStr(GDBIndex)+' '+IntToStr(IgnoreCount));
          If Assigned(Commands) then
            begin
              {Commands are not handled yet }
              Debugger^.Command('command '+IntToStr(GDBIndex));
              p:=commands;
              while assigned(p) do
                begin
                  p2:=strscan(p,#10);
                  if assigned(p2) then
                      p2^:=#0;
                  st:=strpas(p);
                  Debugger^.command(st);
                  if assigned(p2) then
                      p2^:=#10;
                  p:=p2;
                  if assigned(p) then
                    inc(p);
                end;
              Debugger^.Command('end');
            end;
        end
      else
      { Here there was a problem !! }
        begin
          GDBIndex:=0;
          if not Debugger^.Disableallinvalidbreakpoints then
            begin
              if (typ=bt_file_line) and assigned(FileName) then
                begin
                  ClearFormatParams;
                  AddFormatParamStr(NameAndExtOf(FileName^));
                  AddFormatParamInt(Line);
                  if ChoiceBox(msg_couldnotsetbreakpointat,@FormatParams,[btn_ok,button_DisableAllBreakpoints],false)=cmUserBtn2 then
                    Debugger^.Disableallinvalidbreakpoints:=true;
                end
              else
                begin
                  ClearFormatParams;
                  AddFormatParamStr(BreakpointTypeStr[typ]);
                  AddFormatParamStr(GetStr(Name));
                  if ChoiceBox(msg_couldnotsetbreakpointtype,@FormatParams,[btn_ok,button_DisableAllBreakpoints],false)=cmUserBtn2 then
                    Debugger^.Disableallinvalidbreakpoints:=true;
                end;
            end;
          state:=bs_disabled;
          UpdateSource;
        end;
    end
  else if (GDBState=bs_disabled) and (state=bs_enabled) then
    Enable
  else if (GDBState=bs_enabled) and (state=bs_disabled) then
    Disable;
end;

procedure TBreakpoint.Remove;
begin
  If not assigned(Debugger) then Exit;
  if GDBIndex>0 then
    Debugger^.Command('delete '+IntToStr(GDBIndex));
  GDBIndex:=0;
  GDBState:=bs_deleted;
end;

procedure TBreakpoint.Enable;
begin
  If not assigned(Debugger) then Exit;
  if GDBIndex>0 then
    Debugger^.Command('enable '+IntToStr(GDBIndex))
  else
    Insert;
  GDBState:=bs_enabled;
end;

procedure TBreakpoint.Disable;
begin
  If not assigned(Debugger) then Exit;
  if GDBIndex>0 then
    Debugger^.Command('disable '+IntToStr(GDBIndex));
  GDBState:=bs_disabled;
end;

procedure TBreakpoint.ResetValues;
begin
  if assigned(OldValue) then
    DisposeStr(OldValue);
  OldValue:=nil;
  if assigned(CurrentValue) then
    DisposeStr(CurrentValue);
  CurrentValue:=nil;
end;

procedure  TBreakpoint.UpdateSource;
var W: PSourceWindow;
    b : boolean;
begin
  if typ=bt_file_line then
    begin
      W:=SearchOnDesktop(FExpand(OSFileName(GetStr(FileName))),false);
      If assigned(W) then
        begin
          if state=bs_enabled then
            b:=true
          else
            b:=false;
          W^.Editor^.SetLineFlagState(Line-1,lfBreakpoint,b);
        end;
    end;
end;

destructor TBreakpoint.Done;
begin
  Remove;
  ResetValues;
  if assigned(Name) then
    DisposeStr(Name);
  if assigned(FileName) then
    DisposeStr(FileName);
  if assigned(Conditions) then
    DisposeStr(Conditions);
  if assigned(Commands) then
    StrDispose(Commands);
  inherited Done;
end;

{****************************************************************************
                        TBreakpointCollection
****************************************************************************}

function TBreakpointCollection.At(Index: Integer): PBreakpoint;
begin
  At:=inherited At(Index);
end;

procedure TBreakpointCollection.Update;
begin
  if assigned(Debugger) then
    begin
      Debugger^.RemoveBreakpoints;
      Debugger^.InsertBreakpoints;
    end;
  if assigned(BreakpointsWindow) then
    BreakpointsWindow^.Update;
end;

function  TBreakpointCollection.GetGDB(index : longint) : PBreakpoint;

  function IsNum(P : PBreakpoint) : boolean;{$ifndef FPC}far;{$endif}
  begin
    IsNum:=P^.GDBIndex=index;
  end;

begin
  if index=0 then
    GetGDB:=nil
  else
    GetGDB:=FirstThat(@IsNum);
end;

procedure TBreakpointCollection.ShowBreakpoints(W : PFPWindow);

  procedure SetInSource(P : PBreakpoint);{$ifndef FPC}far;{$endif}
  begin
    If assigned(P^.FileName) and
      (OSFileName(FExpand(P^.FileName^))=OSFileName(FExpand(PSourceWindow(W)^.Editor^.FileName))) then
      PSourceWindow(W)^.Editor^.SetLineFlagState(P^.Line-1,lfBreakpoint,P^.state=bs_enabled);
  end;

  procedure SetInDisassembly(P : PBreakpoint);{$ifndef FPC}far;{$endif}
    var
      PDL : PDisasLine;
      S : string;
      ps,qs,i : longint;
  begin
    for i:=0 to PDisassemblyWindow(W)^.Editor^.GetLineCount-1 do
      begin
        PDL:=PDisasLine(PDisassemblyWindow(W)^.Editor^.GetLine(i));
        if PDL^.Address=0 then
          begin
            if (P^.typ=bt_file_line) then
              begin
                S:=PDisassemblyWindow(W)^.Editor^.GetDisplayText(i);
                ps:=pos(':',S);
                qs:=pos(' ',copy(S,ps+1,High(S)));
                if (GDBFileName(FExpand(P^.FileName^))=GDBFileName(FExpand(Copy(S,1,ps-1)))) and
                   (StrToInt(copy(S,ps+1,qs-1))=P^.line) then
                  PDisassemblyWindow(W)^.Editor^.SetLineFlagState(i,lfBreakpoint,P^.state=bs_enabled);
              end;
          end
        else
          begin
            If (P^.typ=bt_address) and (PDL^.Address=HexToCard(P^.Name^)) then
              PDisassemblyWindow(W)^.Editor^.SetLineFlagState(i,lfBreakpoint,P^.state=bs_enabled);
          end;
      end;
  end;

begin
  if W=PFPWindow(DisassemblyWindow) then
    ForEach(@SetInDisassembly)
  else
    ForEach(@SetInSource);
end;


procedure TBreakpointCollection.AdaptBreakpoints(Editor : PSourceEditor; Pos, Change : longint);

  procedure AdaptInSource(P : PBreakpoint);{$ifndef FPC}far;{$endif}
  begin
    If assigned(P^.FileName) and
      (OSFileName(FExpand(P^.FileName^))=OSFileName(FExpand(Editor^.FileName))) then
        begin
          if P^.state=bs_enabled then
            Editor^.SetLineFlagState(P^.Line-1,lfBreakpoint,false);
          if P^.Line-1>=Pos then
            begin
              if (Change>0) or (P^.Line-1>=Pos-Change) then
                P^.line:=P^.Line+Change
              else
                begin
                  { removing inside a ForEach call leads to problems }
                  { so we do that after PM }
                  P^.state:=bs_delete_after;
                end;
            end;
          if P^.state=bs_enabled then
            Editor^.SetLineFlagState(P^.Line-1,lfBreakpoint,true);
        end;
  end;

  var
    I : longint;
begin
  ForEach(@AdaptInSource);
  I:=Count-1;
  While (I>=0) do
    begin
      if At(I)^.state=bs_delete_after then
        AtFree(I);
      Dec(I);
    end;
end;

function TBreakpointCollection.FindBreakpointAt(Editor : PSourceEditor; Line : longint) : PBreakpoint;

  function IsAtLine(P : PBreakpoint) : boolean;{$ifndef FPC}far;{$endif}
  begin
    If assigned(P^.FileName) and
       (OSFileName(FExpand(P^.FileName^))=OSFileName(FExpand(Editor^.FileName))) and
       (Line=P^.Line) then
      IsAtLine:=true
    else
      IsAtLine:=false;
  end;

begin
  FindBreakpointAt:=FirstThat(@IsAtLine);
end;

procedure TBreakpointCollection.ShowAllBreakpoints;

  procedure SetInSource(P : PBreakpoint);{$ifndef FPC}far;{$endif}
    var
      W : PSourceWindow;
  begin
    If assigned(P^.FileName) then
      begin
        W:=SearchOnDesktop(P^.FileName^,false);
        if assigned(W) then
          W^.Editor^.SetLineFlagState(P^.Line-1,lfBreakpoint,P^.state=bs_enabled);
      end;
  end;

begin
  ForEach(@SetInSource);
end;

function TBreakpointCollection.GetType(typ : BreakpointType;Const s : String) : PBreakpoint;

  function IsThis(P : PBreakpoint) : boolean;{$ifndef FPC}far;{$endif}
  begin
    IsThis:=(P^.typ=typ) and (GetStr(P^.Name)=S);
  end;

begin
  GetType:=FirstThat(@IsThis);
end;

function TBreakpointCollection.ToggleFileLine(FileName: String;LineNr : Longint) : boolean;

var PB : PBreakpoint;

  function IsThere(P : PBreakpoint) : boolean;{$ifndef FPC}far;{$endif}
  begin
    IsThere:=(P^.typ=bt_file_line) and assigned(P^.FileName) and
      (OSFileName(FExpand(P^.FileName^))=FileName) and (P^.Line=LineNr);
  end;
begin
    FileName:=OSFileName(FileName);
    PB:=FirstThat(@IsThere);
    ToggleFileLine:=false;
    If Assigned(PB) then
      if PB^.state=bs_disabled then
        begin
          PB^.state:=bs_enabled;
          ToggleFileLine:=true;
        end
      else if PB^.state=bs_enabled then
        PB^.state:=bs_disabled;
    If not assigned(PB) then
      begin
        PB:= New(PBreakpoint,Init_file_line(FileName,LineNr));
        if assigned(PB) then
          Begin
            Insert(PB);
            ToggleFileLine:=true;
          End;
      end;
    if assigned(PB) then
      PB^.UpdateSource;
    Update;
end;



{****************************************************************************
                         TBreakpointItem
****************************************************************************}

constructor TBreakpointItem.Init(ABreakpoint : PBreakpoint);
begin
  inherited Init;
  Breakpoint:=ABreakpoint;
end;

function TBreakpointItem.GetText(MaxLen: Sw_integer): string;
var S: string;
begin
 with Breakpoint^ do
   begin
     S:=BreakpointTypeStr[typ];
     While Length(S)<10 do
       S:=S+' ';
     S:=S+'|';
     S:=S+BreakpointStateStr[state]+' ';
     While Length(S)<20 do
       S:=S+' ';
     S:=S+'|';
     if (typ=bt_file_line) then
       S:=S+NameAndExtOf(GetStr(FileName))+':'+IntToStr(Line)
         else
       S:=S+GetStr(name);
     While Length(S)<40 do
       S:=S+' ';
     S:=S+'|';
     if IgnoreCount>0 then
       S:=S+IntToStr(IgnoreCount);
     While Length(S)<49 do
       S:=S+' ';
     S:=S+'|';
     if assigned(Conditions) then
       S:=S+' '+GetStr(Conditions);
     if length(S)>MaxLen then S:=copy(S,1,MaxLen-2)+'..';
     GetText:=S;
   end;
end;

procedure TBreakpointItem.Selected;
begin
end;

function TBreakpointItem.GetModuleName: string;
begin
  if breakpoint^.typ=bt_file_line then
    GetModuleName:=GetStr(breakpoint^.FileName)
  else
    GetModuleName:='';
end;

{****************************************************************************
                         TBreakpointsListBox
****************************************************************************}

constructor TBreakpointsListBox.Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar);
begin
  inherited Init(Bounds,1,AHScrollBar, AVScrollBar);
  GrowMode:=gfGrowLoX+gfGrowHiX+gfGrowHiY;
  NoSelection:=true;
end;

function TBreakpointsListBox.GetLocalMenu: PMenu;
var M: PMenu;
begin
  if (Owner<>nil) and (Owner^.GetState(sfModal)) then M:=nil else
  M:=NewMenu(
    NewItem(menu_bplocal_gotosource,'',kbNoKey,cmMsgGotoSource,hcMsgGotoSource,
    NewItem(menu_bplocal_editbreakpoint,'',kbNoKey,cmEditBreakpoint,hcEditBreakpoint,
    NewItem(menu_bplocal_newbreakpoint,'',kbNoKey,cmNewBreakpoint,hcNewBreakpoint,
    NewItem(menu_bplocal_deletebreakpoint,'',kbNoKey,cmDeleteBreakpoint,hcDeleteBreakpoint,
    NewItem(menu_bplocal_togglestate,'',kbNoKey,cmToggleBreakpoint,hcToggleBreakpoint,
    nil))))));
  GetLocalMenu:=M;
end;

procedure TBreakpointsListBox.HandleEvent(var Event: TEvent);
var DontClear: boolean;
begin
  case Event.What of
    evKeyDown :
      begin
        DontClear:=false;
        case Event.KeyCode of
          kbEnter :
            Message(@Self,evCommand,cmMsgGotoSource,nil);
          kbIns :
            Message(@Self,evCommand,cmNewBreakpoint,nil);
          kbDel :
            Message(@Self,evCommand,cmDeleteBreakpoint,nil);
        else
          DontClear:=true;
        end;
        if not DontClear then
          ClearEvent(Event);
      end;
    evBroadcast :
      case Event.Command of
        cmListItemSelected :
          if Event.InfoPtr=@Self then
            Message(@Self,evCommand,cmEditBreakpoint,nil);
      end;
    evCommand :
      begin
        DontClear:=false;
        case Event.Command of
          cmMsgTrackSource :
            if Range>0 then
              TrackSource;
          cmEditBreakpoint :
              EditCurrent;
          cmToggleBreakpoint :
              ToggleCurrent;
          cmDeleteBreakpoint :
              DeleteCurrent;
          cmNewBreakpoint :
              EditNew;
          cmMsgClear :
            Clear;
          else
            DontClear:=true;
        end;
        if not DontClear then
          ClearEvent(Event);
      end;
  end;
  inherited HandleEvent(Event);
end;

procedure TBreakpointsListBox.AddBreakpoint(P: PBreakpointItem);
var W : integer;
begin
  if List=nil then New(List, Init(20,20));
  W:=length(P^.GetText(255));
  if W>MaxWidth then
  begin
    MaxWidth:=W;
    if HScrollBar<>nil then
       HScrollBar^.SetRange(0,MaxWidth);
  end;
  List^.Insert(P);
  SetRange(List^.Count);
  if Focused=List^.Count-1-1 then
     FocusItem(List^.Count-1);
  P^.Breakpoint^.UpdateSource;
  DrawView;
end;

(* function TBreakpointsListBox.AddModuleName(const Name: string): PString;
var P: PString;
begin
  if ModuleNames<>nil then
    P:=ModuleNames^.Add(Name)
  else
    P:=nil;
  AddModuleName:=P;
end;  *)

function TBreakpointsListBox.GetText(Item,MaxLen: Sw_Integer): String;
var P: PBreakpointItem;
    S: string;
begin
  P:=List^.At(Item);
  S:=P^.GetText(MaxLen);
  GetText:=copy(S,1,MaxLen);
end;

procedure TBreakpointsListBox.Clear;
begin
  if assigned(List) then
    Dispose(List, Done);
  List:=nil;
  MaxWidth:=0;
  (* if assigned(ModuleNames) then
    ModuleNames^.FreeAll; *)
  SetRange(0); DrawView;
  Message(Application,evBroadcast,cmClearLineHighlights,@Self);
end;

procedure TBreakpointsListBox.TrackSource;
var W: PSourceWindow;
    P: PBreakpointItem;
    R: TRect;
    (* Row,Col: sw_integer; *)
begin
  (*Message(Application,evBroadcast,cmClearLineHighlights,@Self);
  if Range=0 then Exit;*)
  P:=List^.At(Focused);
  if P^.GetModuleName='' then Exit;
  Desktop^.Lock;
  GetNextEditorBounds(R);
  R.B.Y:=Owner^.Origin.Y;
  W:=EditorWindowFile(P^.GetModuleName);
  if assigned(W) then
    begin
      W^.GetExtent(R);
      R.B.Y:=Owner^.Origin.Y;
      W^.ChangeBounds(R);
      W^.Editor^.SetCurPtr(1,P^.Breakpoint^.Line);
    end
  else
    W:=TryToOpenFile(@R,P^.GetModuleName,1,P^.Breakpoint^.Line,true);
  if W<>nil then
    begin
      W^.Select;
      W^.Editor^.TrackCursor(true);
      W^.Editor^.SetLineFlagExclusive(lfHighlightRow,P^.Breakpoint^.Line);
    end;
  if Assigned(Owner) then
    Owner^.Select;
  Desktop^.UnLock;
end;

procedure TBreakpointsListBox.ToggleCurrent;
var
  P: PBreakpointItem;
begin
  if Range=0 then Exit;
  P:=List^.At(Focused);
  if P=nil then Exit;
  if P^.Breakpoint^.state=bs_enabled then
    P^.Breakpoint^.state:=bs_disabled
  else if P^.Breakpoint^.state=bs_disabled then
    P^.Breakpoint^.state:=bs_enabled;
  P^.Breakpoint^.UpdateSource;
  BreakpointsCollection^.Update;
end;

procedure TBreakpointsListBox.EditCurrent;
var
  P: PBreakpointItem;
begin
  if Range=0 then Exit;
  P:=List^.At(Focused);
  if P=nil then Exit;
  Application^.ExecuteDialog(New(PBreakpointItemDialog,Init(P^.Breakpoint)),nil);
  P^.Breakpoint^.UpdateSource;
  BreakpointsCollection^.Update;
end;

procedure TBreakpointsListBox.DeleteCurrent;
var
  P: PBreakpointItem;
begin
  if Range=0 then Exit;
  P:=List^.At(Focused);
  if P=nil then Exit;
  { delete it form source window }
  P^.Breakpoint^.state:=bs_disabled;
  P^.Breakpoint^.UpdateSource;
  BreakpointsCollection^.free(P^.Breakpoint);
  List^.free(P);
  BreakpointsCollection^.Update;
end;

procedure TBreakpointsListBox.EditNew;
var
  P: PBreakpoint;
begin
  P:=New(PBreakpoint,Init_Empty);
  if Application^.ExecuteDialog(New(PBreakpointItemDialog,Init(P)),nil)<>cmCancel then
    begin
      P^.UpdateSource;
      BreakpointsCollection^.Insert(P);
      BreakpointsCollection^.Update;
    end
  else
    dispose(P,Done);
end;

procedure TBreakpointsListBox.Draw;
var
  I, J, Item: Sw_Integer;
  NormalColor, SelectedColor, FocusedColor, Color: Word;
  ColWidth, CurCol, Indent: Integer;
  B: TDrawBuffer;
  Text: String;
  SCOff: Byte;
  TC: byte;
procedure MT(var C: word); begin if TC<>0 then C:=(C and $ff0f) or (TC and $f0); end;
begin
  if (Owner<>nil) then TC:=ord(Owner^.GetColor(6)) else TC:=0;
  if State and (sfSelected + sfActive) = (sfSelected + sfActive) then
  begin
    NormalColor := GetColor(1);
    FocusedColor := GetColor(3);
    SelectedColor := GetColor(4);
  end else
  begin
    NormalColor := GetColor(2);
    SelectedColor := GetColor(4);
  end;
  if Transparent then
    begin MT(NormalColor); MT(SelectedColor); end;
  if NoSelection then
     SelectedColor:=NormalColor;
  if HScrollBar <> nil then Indent := HScrollBar^.Value
  else Indent := 0;
  ColWidth := Size.X div NumCols + 1;
  for I := 0 to Size.Y - 1 do
  begin
    for J := 0 to NumCols-1 do
    begin
      Item := J*Size.Y + I + TopItem;
      CurCol := J*ColWidth;
      if (State and (sfSelected + sfActive) = (sfSelected + sfActive)) and
        (Focused = Item) and (Range > 0) then
      begin
        Color := FocusedColor;
        SetCursor(CurCol+1,I);
        SCOff := 0;
      end
      else if (Item < Range) and IsSelected(Item) then
      begin
        Color := SelectedColor;
        SCOff := 2;
      end
      else
      begin
        Color := NormalColor;
        SCOff := 4;
      end;
      MoveChar(B[CurCol], ' ', Color, ColWidth);
      if Item < Range then
      begin
        Text := GetText(Item, ColWidth + Indent);
        Text := Copy(Text,Indent,ColWidth);
        MoveStr(B[CurCol+1], Text, Color);
        if ShowMarkers then
        begin
          WordRec(B[CurCol]).Lo := Byte(SpecialChars[SCOff]);
          WordRec(B[CurCol+ColWidth-2]).Lo := Byte(SpecialChars[SCOff+1]);
        end;
      end;
      MoveChar(B[CurCol+ColWidth-1], #179, GetColor(5), 1);
    end;
    WriteLine(0, I, Size.X, 1, B);
  end;
end;

constructor TBreakpointsListBox.Load(var S: TStream);
begin
  inherited Load(S);
end;

procedure TBreakpointsListBox.Store(var S: TStream);
var OL: PCollection;
    OldR : integer;
begin
  OL:=List;
  OldR:=Range;
  Range:=0;
  New(List, Init(1,1));

  inherited Store(S);

  Dispose(List, Done);
  Range:=OldR;
  List:=OL;
  { ^^^ nasty trick - has anyone a better idea how to avoid storing the
    collection? Pasting here a modified version of TListBox.Store+
    TAdvancedListBox.Store isn't a better solution, since by eventually
    changing the obj-hierarchy you'll always have to modify this, too - BG }
end;

destructor TBreakpointsListBox.Done;
begin
  inherited Done;
  if List<>nil then Dispose(List, Done);
  (* if ModuleNames<>nil then Dispose(ModuleNames, Done);*)
end;

{****************************************************************************
                         TBreakpointsWindow
****************************************************************************}

constructor TBreakpointsWindow.Init;
var R,R2: TRect;
    HSB,VSB: PScrollBar;
    ST: PStaticText;
    S: String;
    X,X1 : Sw_integer;
    Btn: PButton;
const
  NumButtons = 5;
begin
  Desktop^.GetExtent(R); R.A.Y:=R.B.Y-18;
  inherited Init(R, dialog_breakpointlist, wnNoNumber);

  HelpCtx:=hcBreakpointListWindow;

  GetExtent(R); R.Grow(-1,-1); R.B.Y:=R.A.Y+1;
  S:=label_breakpointpropheader;
  New(ST, Init(R,S));
  ST^.GrowMode:=gfGrowHiX;
  Insert(ST);
  GetExtent(R); R.Grow(-1,-1); Inc(R.A.Y,1); R.B.Y:=R.A.Y+1;
  New(ST, Init(R, CharStr('�', MaxViewWidth)));
  ST^.GrowMode:=gfGrowHiX;
  Insert(ST);
  GetExtent(R); R.Grow(-1,-1); Inc(R.A.Y,2);Dec(R.B.Y,5);
  R2.Copy(R); Inc(R2.B.Y); R2.A.Y:=R2.B.Y-1;
  New(HSB, Init(R2)); HSB^.GrowMode:=gfGrowLoY+gfGrowHiY+gfGrowHiX; Insert(HSB);
  HSB^.SetStep(R.B.X-R.A.X-2,1);
  R2.Copy(R); Inc(R2.B.X); R2.A.X:=R2.B.X-1;
  New(VSB, Init(R2)); VSB^.GrowMode:=gfGrowLoX+gfGrowHiX+gfGrowHiY; Insert(VSB);
  VSB^.SetStep(R.B.Y-R.A.Y-2,1);
  New(BreakLB, Init(R,HSB,VSB));
  BreakLB^.GrowMode:=gfGrowHiX+gfGrowHiY;
  BreakLB^.Transparent:=true;
  Insert(BreakLB);
  GetExtent(R);R.Grow(-1,-1);
  Dec(R.B.Y);
  R.A.Y:=R.B.Y-2;
  X:=(R.B.X-R.A.X) div NumButtons;
  X1:=R.A.X+(X div 2);
  R.A.X:=X1-3;R.B.X:=X1+7;
  New(Btn, Init(R, button_Close, cmClose, bfDefault));
  Btn^.GrowMode:=gfGrowLoY+gfGrowHiY;
  Insert(Btn);
  X1:=X1+X;
  R.A.X:=X1-3;R.B.X:=X1+7;
  New(Btn, Init(R, button_New, cmNewBreakpoint, bfNormal));
  Btn^.GrowMode:=gfGrowLoY+gfGrowHiY;
  Insert(Btn);
  X1:=X1+X;
  R.A.X:=X1-3;R.B.X:=X1+7;
  New(Btn, Init(R, button_Edit, cmEditBreakpoint, bfNormal));
  Btn^.GrowMode:=gfGrowLoY+gfGrowHiY;
  Insert(Btn);
  X1:=X1+X;
  R.A.X:=X1-3;R.B.X:=X1+7;
  New(Btn, Init(R, button_ToggleButton, cmToggleBreakInList, bfNormal));
  Btn^.GrowMode:=gfGrowLoY+gfGrowHiY;
  Insert(Btn);
  X1:=X1+X;
  R.A.X:=X1-3;R.B.X:=X1+7;
  New(Btn, Init(R, button_Delete, cmDeleteBreakpoint, bfNormal));
  Btn^.GrowMode:=gfGrowLoY+gfGrowHiY;
  Insert(Btn);
  BreakLB^.Select;
  Update;
  BreakpointsWindow:=@self;
end;

constructor TBreakpointsWindow.Load(var S: TStream);
begin
  inherited Load(S);
  GetSubViewPtr(S,BreakLB);
end;

procedure TBreakpointsWindow.Store(var S: TStream);
begin
  inherited Store(S);
  PutSubViewPtr(S,BreakLB);
end;

procedure TBreakpointsWindow.AddBreakpoint(ABreakpoint : PBreakpoint);
begin
  BreakLB^.AddBreakpoint(New(PBreakpointItem, Init(ABreakpoint)));
end;

procedure TBreakpointsWindow.ClearBreakpoints;
begin
  BreakLB^.Clear;
  ReDraw;
end;

procedure TBreakpointsWindow.ReloadBreakpoints;
  procedure InsertInBreakLB(P : PBreakpoint);
  begin
    BreakLB^.AddBreakpoint(New(PBreakpointItem, Init(P)));
  end;
begin
  If not assigned(BreakpointsCollection) then
    exit;
  BreakpointsCollection^.ForEach(@InsertInBreakLB);
  ReDraw;
end;

procedure TBreakpointsWindow.SizeLimits(var Min, Max: TPoint);
begin
  inherited SizeLimits(Min,Max);
  Min.X:=40; Min.Y:=18;
end;

procedure TBreakpointsWindow.Close;
begin
  Hide;
end;

procedure TBreakpointsWindow.HandleEvent(var Event: TEvent);
var DontClear : boolean;
begin
  case Event.What of
    evKeyDown :
      begin
        if (Event.KeyCode=kbEnter) or (Event.KeyCode=kbEsc) then
          begin
            ClearEvent(Event);
            Hide;
          end;
      end;
    evCommand :
      begin
       DontClear:=False;
       case Event.Command of
         cmNewBreakpoint :
           BreakLB^.EditNew;
         cmEditBreakpoint :
           BreakLB^.EditCurrent;
         cmDeleteBreakpoint :
           BreakLB^.DeleteCurrent;
         cmToggleBreakInList :
           BreakLB^.ToggleCurrent;
         cmClose :
           Hide;
          else
            DontClear:=true;
        end;
        if not DontClear then
          ClearEvent(Event);
      end;
    evBroadcast :
      case Event.Command of
        cmUpdate :
          Update;
      end;
  end;
  inherited HandleEvent(Event);
end;

procedure TBreakpointsWindow.Update;
var
  StoreFocus : longint;
begin
  StoreFocus:=BreakLB^.Focused;
  ClearBreakpoints;
  ReloadBreakpoints;
  If StoreFocus<BreakLB^.Range then
    BreakLB^.FocusItem(StoreFocus);
end;

destructor TBreakpointsWindow.Done;
begin
  inherited Done;
  BreakpointsWindow:=nil;
end;

{****************************************************************************
                         TBreakpointItemDialog
****************************************************************************}

constructor TBreakpointItemDialog.Init(ABreakpoint: PBreakpoint);
var R,R2,R3: TRect;
    Items: PSItem;
    I : BreakpointType;
    KeyCount: sw_integer;
begin
  KeyCount:=longint(high(BreakpointType));

  R.Assign(0,0,60,Max(9+KeyCount,18));
  inherited Init(R,dialog_modifynewbreakpoint);
  Breakpoint:=ABreakpoint;

  GetExtent(R); R.Grow(-3,-2); R3.Copy(R);
  Inc(R.A.Y); R.B.Y:=R.A.Y+1; R.B.X:=R.B.X-3;
  New(NameIL, Init(R, 255)); Insert(NameIL);
  R2.Copy(R); R2.A.X:=R2.B.X; R2.B.X:=R2.A.X+3;
  Insert(New(PHistory, Init(R2, NameIL, hidWatchDialog)));
  R.Copy(R3); Inc(R.A.Y); R.B.Y:=R.A.Y+1;
  R2.Copy(R); R2.Move(-1,-1);
  Insert(New(PLabel, Init(R2, label_breakpoint_name, NameIL)));
  R.Move(0,3);
  New(ConditionsIL, Init(R, 255)); Insert(ConditionsIL);
  R2.Copy(R); R2.Move(-1,-1); Insert(New(PLabel, Init(R2, label_breakpoint_conditions, ConditionsIL)));
  R.Move(0,3); R.B.X:=R.A.X+36;
  New(LineIL, Init(R, 128)); Insert(LineIL);
  LineIL^.SetValidator(New(PRangeValidator, Init(0,MaxInt)));
  R2.Copy(R); R2.Move(-1,-1); Insert(New(PLabel, Init(R2, label_breakpoint_line, LineIL)));
  R.Move(0,3);
  New(IgnoreIL, Init(R, 128)); Insert(IgnoreIL);
  IgnoreIL^.SetValidator(New(PRangeValidator, Init(0,MaxInt)));
  R2.Copy(R); R2.Move(-1,-1); Insert(New(PLabel, Init(R2, label_breakpoint_ignorecount, IgnoreIL)));

  R.Copy(R3); Inc(R.A.X,38); Inc(R.A.Y,7); R.B.Y:=R.A.Y+KeyCount;
  Items:=nil;
  { don't use invalid type }
  for I:=pred(high(BreakpointType)) downto low(BreakpointType) do
    Items:=NewSItem(BreakpointTypeStr[I], Items);
  New(TypeRB, Init(R, Items));

  R2.Copy(R); R2.Move(-1,-1); R2.B.Y:=R2.A.Y+1;
  Insert(New(PLabel, Init(R2, label_breakpoint_type, TypeRB)));

  Insert(TypeRB);

  InsertButtons(@Self);

  NameIL^.Select;
end;

function TBreakpointItemDialog.Execute: Word;
var R: word;
    S1: string;
    err: word;
    L: longint;
begin
  R:=longint(Breakpoint^.typ);
  TypeRB^.SetData(R);

  If Breakpoint^.typ=bt_file_line then
    S1:=GetStr(Breakpoint^.FileName)
  else
    S1:=GetStr(Breakpoint^.name);
  NameIL^.SetData(S1);

  If Breakpoint^.typ=bt_file_line then
    S1:=IntToStr(Breakpoint^.Line)
  else
    S1:='0';
  LineIL^.SetData(S1);

  S1:=IntToStr(Breakpoint^.IgnoreCount);
  IgnoreIL^.SetData(S1);
  S1:=GetStr(Breakpoint^.Conditions);
  ConditionsIL^.SetData(S1);

  if assigned(FirstEditorWindow) then
    FindReplaceEditor:=FirstEditorWindow^.Editor;

  R:=inherited Execute;

  FindReplaceEditor:=nil;
  if R=cmOK then
  begin
    TypeRB^.GetData(R);
    L:=R;
    Breakpoint^.typ:=BreakpointType(L);

    NameIL^.GetData(S1);
    If Breakpoint^.typ=bt_file_line then
      begin
        If assigned(Breakpoint^.FileName) then
          DisposeStr(Breakpoint^.FileName);
        Breakpoint^.FileName:=NewStr(S1);
      end
    else
      begin
        If assigned(Breakpoint^.Name) then
          DisposeStr(Breakpoint^.Name);
        Breakpoint^.name:=NewStr(S1);
      end;
    If Breakpoint^.typ=bt_file_line then
      begin
        LineIL^.GetData(S1);
        Val(S1,L,err);
        Breakpoint^.Line:=L;
      end;
    IgnoreIL^.GetData(S1);
    Val(S1,L,err);
    Breakpoint^.IgnoreCount:=L;

    ConditionsIL^.GetData(S1);
    If assigned(Breakpoint^.Conditions) then
      DisposeStr(Breakpoint^.Conditions);
    Breakpoint^.Conditions:=NewStr(S1);
  end;
  Execute:=R;
end;

{****************************************************************************
                         TWatch
****************************************************************************}

constructor TWatch.Init(s : string);
  begin
    expr:=NewStr(s);
    last_value:=nil;
    current_value:=nil;
    Get_new_value;
    GDBRunCount:=-1;
  end;

constructor TWatch.Load(var S: TStream);
  begin
    expr:=S.ReadStr;
    last_value:=nil;
    current_value:=nil;
    Get_new_value;
    GDBRunCount:=-1;
  end;

procedure TWatch.Store(var S: TStream);
  begin
    S.WriteStr(expr);
  end;

procedure TWatch.rename(s : string);
  begin
    if assigned(expr) then
      begin
        if GetStr(expr)=S then
          exit;
        DisposeStr(expr);
      end;
    expr:=NewStr(s);
    if assigned(last_value) then
      StrDispose(last_value);
    last_value:=nil;
    if assigned(current_value) then
      StrDispose(current_value);
    current_value:=nil;
    GDBRunCount:=-1;
    Get_new_value;
  end;

procedure TWatch.Get_new_value;
  var p, q : pchar;
      i, j, curframe, startframe : longint;
      s,s2 : string;
      loop_higher, found : boolean;
      last_removed : char;

    function GetValue(var s : string) : boolean;
      begin
        Debugger^.command('p '+s);
        if not Debugger^.Error then
          begin
            s:=StrPas(Debugger^.GetOutput);
            GetValue:=true;
          end
        else
          begin
            s:=StrPas(Debugger^.GetError);
            GetValue:=false;
            { do not open a messagebox for such errors }
            Debugger^.got_error:=false;
          end;
      end;

  begin
    If not assigned(Debugger) or Not Debugger^.HasExe or
       (GDBRunCount=Debugger^.RunCount) then
      exit;
    GDBRunCount:=Debugger^.RunCount;
    if assigned(last_value) then
      strdispose(last_value);
    last_value:=current_value;
    s:=GetStr(expr);
    found:=GetValue(s);
    Debugger^.got_error:=false;
    loop_higher:=not found;
    if not found then
      begin
        curframe:=Debugger^.get_current_frame;
        startframe:=curframe;
      end
    else
      begin
        curframe:=0;
        startframe:=0;
      end;
    while loop_higher do
      begin
         s:='parent_ebp';
         if GetValue(s) then
           begin
             repeat
               inc(curframe);
               if not Debugger^.set_current_frame(curframe) then
                 loop_higher:=false;
{$ifdef FrameNameKnown}
               s2:='/x '+FrameName;
{$else not  FrameNameKnown}
               s2:='/x $ebp';
{$endif FrameNameKnown}
               getValue(s2);
               j:=pos('=',s2);
               if j>0 then
                 s2:=copy(s2,j+1,length(s2));
               while s2[1] in [' ',TAB] do
                 delete(s2,1,1);
               if pos(s2,s)>0 then
                 loop_higher :=false;
             until not loop_higher;
             { try again at that level }
             s:=GetStr(expr);
             found:=GetValue(s);
             loop_higher:=not found;
           end
         else
           loop_higher:=false;
      end;
    if found then
      p:=StrNew(Debugger^.GetOutput)
    else
      begin
        { get a reasonable output at least }
        s:=GetStr(expr);
        GetValue(s);
        p:=StrNew(Debugger^.GetError);
      end;
    Debugger^.got_error:=false;
    { We should try here to find the expr in parent
      procedure if there are
      I will implement this as I added a
      parent_ebp pseudo local var to local procedure
      in stabs debug info PM }
    { But there are some pitfalls like
      locals redefined in other sublocals that call the function }
    if curframe<>startframe then
      Debugger^.set_current_frame(startframe);

    q:=nil;
    if assigned(p) and (p[0]='$') then
      q:=StrPos(p,'=');
    if not assigned(q) then
      q:=p;
    if assigned(q) then
      i:=strlen(q)
    else
      i:=0;
    if (i>0) and (q[i-1]=#10) then
      begin
        while (i>1) and ((q[i-2]=' ') or (q[i-2]=#9)) do
          dec(i);
        last_removed:=q[i-1];
        q[i-1]:=#0;
      end
    else
      last_removed:=#0;
    if assigned(q) then
      current_value:=strnew(q)
    else
      current_value:=strnew('');
    if last_removed<>#0 then
      q[i-1]:=last_removed;
    strdispose(p);
    GDBRunCount:=Debugger^.RunCount;
  end;

procedure TWatch.Force_new_value;
  begin
    GDBRunCount:=-1;
    Get_new_value;
  end;

destructor TWatch.Done;
  begin
    if assigned(expr) then
      disposestr(expr);
    if assigned(last_value) then
      strdispose(last_value);
    if assigned(current_value) then
      strdispose(current_value);
    inherited done;
  end;

{****************************************************************************
                         TWatchesCollection
****************************************************************************}

      constructor TWatchesCollection.Init;
        begin
          inherited Init(10,10);
        end;

      procedure TWatchesCollection.Insert(Item: Pointer);
       begin
         PWatch(Item)^.Get_new_value;
         Inherited Insert(Item);
         Update;
       end;

      procedure TWatchesCollection.Update;
        var
         W,W1 : integer;

         procedure GetMax(P : PWatch);
           begin
              if assigned(P^.Current_value) then
                W1:=StrLen(P^.Current_value)+3+Length(GetStr(P^.expr))
              else
                W1:=2+Length(GetStr(P^.expr));
              if W1>W then
                W:=W1;
           end;

         begin
          W:=0;
          ForEach(@GetMax);
          MaxW:=W;
          If assigned(WatchesWindow) then
            WatchesWindow^.WLB^.Update(MaxW);
        end;

      function  TWatchesCollection.At(Index: Integer): PWatch;
        begin
          At:=Inherited At(Index);
        end;

{****************************************************************************
                         TWatchesListBox
****************************************************************************}

constructor TWatchesListBox.Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar);
  begin
    inherited Init(Bounds,1,AHScrollBar,AVScrollBar);
    If assigned(List) then
      dispose(list,done);
    List:=WatchesCollection;
  end;

procedure TWatchesListBox.Update(AMaxWidth : integer);
var R : TRect;
begin
  GetExtent(R);
  MaxWidth:=AMaxWidth;
  if (HScrollBar<>nil) and (R.B.X-R.A.X<MaxWidth) then
    HScrollBar^.SetRange(0,MaxWidth-(R.B.X-R.A.X))
  else
    HScrollBar^.SetRange(0,0);
  if R.B.X-R.A.X>MaxWidth then
    HScrollBar^.Hide
  else
    HScrollBar^.Show;
  SetRange(List^.Count+1);
  if R.B.Y-R.A.Y>Range then
    VScrollBar^.Hide
  else
    VScrollBar^.Show;

  {if Focused=List^.Count-1-1 then
     FocusItem(List^.Count-1);
     What was that for ?? PM }
  DrawView;
end;

function    TWatchesListBox.GetIndentedText(Item,Indent,MaxLen: Sw_Integer;var Modified : boolean): String;
var
  PW : PWatch;
  ValOffset : Sw_integer;
  S : String;
begin
  Modified:=false;
  if Item>=WatchesCollection^.Count then
    begin
      GetIndentedText:='';
      exit;
    end;

  PW:=WatchesCollection^.At(Item);
  ValOffset:=Length(GetStr(PW^.Expr))+2;
  if not assigned(PW^.expr) then
    GetIndentedText:=''
  else if Indent<ValOffset then
    begin
      S:=GetStr(PW^.Expr);
      if Indent=0 then
        S:=' '+S
      else
        S:=Copy(S,Indent,High(S));
      if not assigned(PW^.current_value) then
        S:=S+' <Unknown value>'
      else
        S:=S+' '+GetPChar(PW^.Current_value);
      GetIndentedText:=Copy(S,1,MaxLen);
    end
  else
   begin
      if not assigned(PW^.Current_value) or
         (StrLen(PW^.Current_value)<Indent-Valoffset) then
        S:=''
      else
        S:=GetPchar(@(PW^.Current_Value[Indent-Valoffset]));
      GetIndentedText:=Copy(S,1,MaxLen);
   end;
   if assigned(PW^.current_value) and
      assigned(PW^.last_value) and
      (strcomp(PW^.Last_value,PW^.Current_value)<>0) then
     Modified:=true;
end;

procedure TWatchesListBox.EditCurrent;
var
  P: PWatch;
begin
  if Range=0 then Exit;
  if Focused<WatchesCollection^.Count then
    P:=WatchesCollection^.At(Focused)
  else
    P:=New(PWatch,Init(''));
  Application^.ExecuteDialog(New(PWatchItemDialog,Init(P)),nil);
  WatchesCollection^.Update;
end;

function    TWatchesListBox.GetText (Item: Sw_Integer; MaxLen: Sw_Integer): String;
var
  Dummy_Modified : boolean;
begin
  GetText:=GetIndentedText(Item, 0, MaxLen, Dummy_Modified);
end;

procedure TWatchesListBox.DeleteCurrent;
var
  P: PWatch;
begin
  if (Range=0) or
     (Focused>=WatchesCollection^.Count) then
    exit;
  P:=WatchesCollection^.At(Focused);
  WatchesCollection^.free(P);
  WatchesCollection^.Update;
end;

procedure TWatchesListBox.EditNew;
var
  P: PWatch;
  S : string;
begin
  if Focused<WatchesCollection^.Count then
    begin
      P:=WatchesCollection^.At(Focused);
      S:=GetStr(P^.expr);
    end
  else
    S:='';
  P:=New(PWatch,Init(S));
  if Application^.ExecuteDialog(New(PWatchItemDialog,Init(P)),nil)<>cmCancel then
    begin
      WatchesCollection^.AtInsert(Focused,P);
      WatchesCollection^.Update;
    end
  else
    dispose(P,Done);
end;

procedure   TWatchesListBox.Draw;
var
  I, J, Item: Sw_Integer;
  NormalColor, SelectedColor, FocusedColor, Color: Word;
  ColWidth, CurCol, Indent: Integer;
  B: TDrawBuffer;
  Modified : boolean;
  Text: String;
  SCOff: Byte;
  TC: byte;
  procedure MT(var C: word);
    begin
      if TC<>0 then C:=(C and $ff0f) or (TC and $f0);
    end;
begin
  if (Owner<>nil) then TC:=ord(Owner^.GetColor(6)) else TC:=0;
  if State and (sfSelected + sfActive) = (sfSelected + sfActive) then
  begin
    NormalColor := GetColor(1);
    FocusedColor := GetColor(3);
    SelectedColor := GetColor(4);
  end else
  begin
    NormalColor := GetColor(2);
    SelectedColor := GetColor(4);
  end;
  if Transparent then
    begin MT(NormalColor); MT(SelectedColor); end;
  (* if NoSelection then
     SelectedColor:=NormalColor;*)
  if HScrollBar <> nil then Indent := HScrollBar^.Value
  else Indent := 0;
  ColWidth := Size.X div NumCols + 1;
  for I := 0 to Size.Y - 1 do
  begin
    for J := 0 to NumCols-1 do
    begin
      Item := J*Size.Y + I + TopItem;
      CurCol := J*ColWidth;
      if (State and (sfSelected + sfActive) = (sfSelected + sfActive)) and
        (Focused = Item) and (Range > 0) then
      begin
        Color := FocusedColor;
        SetCursor(CurCol+1,I);
        SCOff := 0;
      end
      else if (Item < Range) and IsSelected(Item) then
      begin
        Color := SelectedColor;
        SCOff := 2;
      end
      else
      begin
        Color := NormalColor;
        SCOff := 4;
      end;
      MoveChar(B[CurCol], ' ', Color, ColWidth);
      if Item < Range then
      begin
        (* Text := GetText(Item, ColWidth + Indent);
        Text := Copy(Text,Indent,ColWidth); *)
        Text:=GetIndentedText(Item,Indent,ColWidth,Modified);
        if modified then
          begin
            SCOff:=0;
            Color:=(Color and $fff0) or Red;
          end;
        MoveStr(B[CurCol], Text, Color);
        if {ShowMarkers or } Modified then
        begin
          WordRec(B[CurCol]).Lo := Byte(SpecialChars[SCOff]);
          WordRec(B[CurCol+ColWidth-2]).Lo := Byte(SpecialChars[SCOff+1]);
          WordRec(B[CurCol+ColWidth-2]).Hi := Color and $ff;
        end;
      end;
      MoveChar(B[CurCol+ColWidth-1], #179, GetColor(5), 1);
    end;
    WriteLine(0, I, Size.X, 1, B);
  end;
end;

function TWatchesListBox.GetLocalMenu: PMenu;
var M: PMenu;
begin
  if (Owner<>nil) and (Owner^.GetState(sfModal)) then M:=nil else
  M:=NewMenu(
    NewItem(menu_watchlocal_edit,'',kbNoKey,cmEdit,hcNoContext,
    NewItem(menu_watchlocal_new,'',kbNoKey,cmNew,hcNoContext,
    NewItem(menu_watchlocal_delete,'',kbNoKey,cmDelete,hcNoContext,
    NewLine(
    NewItem(menu_msglocal_saveas,'',kbNoKey,cmSaveAs,hcSaveAs,
    nil))))));
  GetLocalMenu:=M;
end;

procedure   TWatchesListBox.HandleEvent(var Event: TEvent);
var DontClear: boolean;
begin
  case Event.What of
    evKeyDown :
      begin
        DontClear:=false;
        case Event.KeyCode of
          kbEnter :
            Message(@Self,evCommand,cmEdit,nil);
          kbIns :
            Message(@Self,evCommand,cmNew,nil);
          kbDel :
            Message(@Self,evCommand,cmDelete,nil);
        else
          DontClear:=true;
        end;
        if not DontClear then
          ClearEvent(Event);
      end;
    evBroadcast :
      case Event.Command of
        cmListItemSelected :
          if Event.InfoPtr=@Self then
            Message(@Self,evCommand,cmEdit,nil);
      end;
    evCommand :
      begin
        DontClear:=false;
        case Event.Command of
          cmEdit :
              EditCurrent;
          cmDelete :
              DeleteCurrent;
          cmNew :
              EditNew;
          else
            DontClear:=true;
        end;
        if not DontClear then
          ClearEvent(Event);
      end;
  end;
  inherited HandleEvent(Event);
end;

      constructor TWatchesListBox.Load(var S: TStream);
        begin
          inherited Load(S);
          If assigned(List) then
            dispose(list,done);
          List:=WatchesCollection;
          { we must set Range PM }
          SetRange(List^.count+1);
        end;

      procedure   TWatchesListBox.Store(var S: TStream);
        var OL: PCollection;
            OldRange : Sw_integer;
        begin
          OL:=List;
          OldRange:=Range;
          Range:=0;
          New(List, Init(1,1));
          inherited Store(S);
          Dispose(List, Done);
          List:=OL;
          { ^^^ nasty trick - has anyone a better idea how to avoid storing the
            collection? Pasting here a modified version of TListBox.Store+
            TAdvancedListBox.Store isn't a better solution, since by eventually
            changing the obj-hierarchy you'll always have to modify this, too - BG }
          SetRange(OldRange);
        end;

      destructor  TWatchesListBox.Done;
        begin
          List:=nil;
          inherited Done;
        end;

{****************************************************************************
                         TWatchesWindow
****************************************************************************}

  Constructor TWatchesWindow.Init;
    var
      HSB,VSB: PScrollBar;
      R,R2 : trect;
    begin
      Desktop^.GetExtent(R);
      R.A.Y:=R.B.Y-7;
      inherited Init(R, dialog_watches,SearchFreeWindowNo);
      Palette:=wpCyanWindow;
      GetExtent(R);
      HelpCtx:=hcWatchesWindow;
      R.Grow(-1,-1);
      R2.Copy(R);
      Inc(R2.B.Y);
      R2.A.Y:=R2.B.Y-1;
      New(HSB, Init(R2));
      HSB^.GrowMode:=gfGrowLoY+gfGrowHiY+gfGrowHiX;
      HSB^.SetStep(R.B.X-R.A.X,1);
      Insert(HSB);
      R2.Copy(R);
      Inc(R2.B.X);
      R2.A.X:=R2.B.X-1;
      New(VSB, Init(R2));
      VSB^.GrowMode:=gfGrowLoX+gfGrowHiX+gfGrowHiY;
      Insert(VSB);
      New(WLB,Init(R,HSB,VSB));
      WLB^.GrowMode:=gfGrowHiX+gfGrowHiY;
      WLB^.Transparent:=true;
      Insert(WLB);
      If assigned(WatchesWindow) then
        dispose(WatchesWindow,done);
      WatchesWindow:=@Self;
      Update;
    end;

  procedure TWatchesWindow.Update;
    begin
      WatchesCollection^.Update;
      Draw;
    end;

  constructor TWatchesWindow.Load(var S: TStream);
    begin
      inherited Load(S);
      GetSubViewPtr(S,WLB);
      If assigned(WatchesWindow) then
        dispose(WatchesWindow,done);
      WatchesWindow:=@Self;
    end;

  procedure TWatchesWindow.Store(var S: TStream);
    begin
      inherited Store(S);
      PutSubViewPtr(S,WLB);
    end;

  Destructor TWatchesWindow.Done;
    begin
      WatchesWindow:=nil;
      Dispose(WLB,done);
      inherited done;
    end;


{****************************************************************************
                         TWatchItemDialog
****************************************************************************}

constructor TWatchItemDialog.Init(AWatch: PWatch);
var R,R2: TRect;
begin
  R.Assign(0,0,50,10);
  inherited Init(R,'Edit Watch');
  Watch:=AWatch;

  GetExtent(R); R.Grow(-3,-2);
  Inc(R.A.Y); R.B.Y:=R.A.Y+1; R.B.X:=R.A.X+36;
  New(NameIL, Init(R, 255)); Insert(NameIL);
  R2.Copy(R); R2.A.X:=R2.B.X; R2.B.X:=R2.A.X+3;
  Insert(New(PHistory, Init(R2, NameIL, hidWatchDialog)));
  R2.Copy(R); R2.Move(-1,-1);
  Insert(New(PLabel, Init(R2, label_watch_expressiontowatch, NameIL)));
  GetExtent(R);
  R.Grow(-3,-1);
  R.A.Y:=R.A.Y+3;
  TextST:=New(PAdvancedStaticText, Init(R, label_watch_values));
  Insert(TextST);

  InsertButtons(@Self);

  NameIL^.Select;
end;

function TWatchItemDialog.Execute: Word;
var R: word;
    S1,S2: string;
begin
  S1:=GetStr(Watch^.expr);
  NameIL^.SetData(S1);

  S1:=GetPChar(Watch^.Current_value);
  S2:=GetPChar(Watch^.Last_value);

  ClearFormatParams;
  AddFormatParamStr(S1);
  AddFormatParamStr(S2);
  if assigned(Watch^.Last_value) and
     assigned(Watch^.Current_value) and
     (strcomp(Watch^.Last_value,Watch^.Current_value)=0) then
    S1:=FormatStrF(msg_watch_currentvalue,FormatParams)
  else
    S1:=FormatStrF(msg_watch_currentandpreviousvalue,FormatParams);

  TextST^.SetText(S1);

  if assigned(FirstEditorWindow) then
    FindReplaceEditor:=FirstEditorWindow^.Editor;

  R:=inherited Execute;

  FindReplaceEditor:=nil;

  if R=cmOK then
  begin
    NameIL^.GetData(S1);
    Watch^.Rename(S1);
    If assigned(Debugger) then
       Debugger^.ReadWatches;
  end;
  Execute:=R;
end;

{****************************************************************************
                         TStackWindow
****************************************************************************}

  constructor TFramesListBox.Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar);
    begin
      Inherited Init(Bounds,AHScrollBar,AVScrollBar);
    end;

  procedure TFramesListBox.Update;

    var i : longint;
        W : PSourceWindow;

    begin
      { call backtrace command }
      If not assigned(Debugger) then
        exit;
    {$ifndef NODEBUG}
      DeskTop^.Lock;
      Clear;
      { forget all old frames }
      Debugger^.clear_frames;

      if Debugger^.WindowWidth<>-1 then
        Debugger^.Command('set width 0xffffffff');
      Debugger^.Command('backtrace');
      { generate list }
      { all is in tframeentry }
      for i:=0 to Debugger^.frame_count-1 do
        begin
          with Debugger^.frames[i]^ do
            begin
              if assigned(file_name) then
                AddItem(new(PMessageItem,init(0,GetPChar(function_name)+GetPChar(args),
                  AddModuleName(GetPChar(file_name)),line_number,1)))
              else
                AddItem(new(PMessageItem,init(0,HexStr(address,8)+' '+GetPChar(function_name)+GetPChar(args),
                  AddModuleName(''),line_number,1)));
              W:=SearchOnDesktop(GetPChar(file_name),false);
              { First reset all Debugger rows }
              If assigned(W) then
                begin
                  W^.Editor^.SetLineFlagExclusive(lfDebuggerRow,-1);
                  W^.Editor^.DebuggerRow:=-1;
                end;
            end;
        end;
      { Now set all Debugger rows }
      for i:=0 to Debugger^.frame_count-1 do
        begin
          with Debugger^.frames[i]^ do
            begin
              W:=SearchOnDesktop(GetPChar(file_name),false);
              If assigned(W) then
                begin
                  If W^.Editor^.DebuggerRow=-1 then
                    begin
                      W^.Editor^.SetLineFlagState(line_number-1,lfDebuggerRow,true);
                      W^.Editor^.DebuggerRow:=line_number-1;
                    end;
                end;
            end;
        end;
      if Assigned(list) and (List^.Count > 0) then
        FocusItem(0);
      if Debugger^.WindowWidth<>-1 then
        Debugger^.Command('set width '+IntToStr(Debugger^.WindowWidth));
      DeskTop^.Unlock;
     {$endif}
    end;

  function TFramesListBox.GetLocalMenu: PMenu;
    begin
      GetLocalMenu:=Inherited GetLocalMenu;
    end;

  procedure TFramesListBox.GotoSource;
    begin
      { select frame for watches }
      If not assigned(Debugger) then
        exit;
    {$ifndef NODEBUG}
      Debugger^.Command('f '+IntToStr(Focused));
      { for local vars }
      Debugger^.RereadWatches;
   {$endif}
      { goto source }
      inherited GotoSource;
    end;

  procedure   TFramesListBox.GotoAssembly;
    begin
      { select frame for watches }
      If not assigned(Debugger) then
        exit;
    {$ifndef NODEBUG}
      Debugger^.Command('f '+IntToStr(Focused));
      { for local vars }
      Debugger^.RereadWatches;
   {$endif}
      { goto source/assembly mixture }
      InitDisassemblyWindow;
      DisassemblyWindow^.LoadFunction('');
      DisassemblyWindow^.SetCurAddress(Debugger^.frames[Focused]^.address);
      DisassemblyWindow^.SelectInDebugSession;
    end;


  procedure   TFramesListBox.HandleEvent(var Event: TEvent);
    begin
      if ((Event.What=EvKeyDown) and (Event.CharCode='i')) or
         ((Event.What=EvCommand) and (Event.Command=cmDisassemble)) then
        GotoAssembly;
      inherited HandleEvent(Event);
    end;

  destructor  TFramesListBox.Done;
    begin
      Inherited Done;
    end;

  Constructor TStackWindow.Init;
    var
      HSB,VSB: PScrollBar;
      R,R2 : trect;
    begin
      Desktop^.GetExtent(R);
      R.A.Y:=R.B.Y-5;
      inherited Init(R, dialog_callstack, wnNoNumber);
      Palette:=wpCyanWindow;
      GetExtent(R);
      HelpCtx:=hcStackWindow;
      R.Grow(-1,-1);
      R2.Copy(R);
      Inc(R2.B.Y);
      R2.A.Y:=R2.B.Y-1;
      New(HSB, Init(R2));
      HSB^.GrowMode:=gfGrowLoY+gfGrowHiY+gfGrowHiX;
      Insert(HSB);
      R2.Copy(R);
      Inc(R2.B.X);
      R2.A.X:=R2.B.X-1;
      New(VSB, Init(R2));
      VSB^.GrowMode:=gfGrowLoX+gfGrowHiX+gfGrowHiY;
      Insert(VSB);
      New(FLB,Init(R,HSB,VSB));
      FLB^.GrowMode:=gfGrowHiX+gfGrowHiY;
      Insert(FLB);
      If assigned(StackWindow) then
        dispose(StackWindow,done);
      StackWindow:=@Self;
      Update;
    end;

  procedure TStackWindow.Update;
    begin
      FLB^.Update;
      DrawView;
    end;

  constructor TStackWindow.Load(var S: TStream);
    begin
      inherited Load(S);
      GetSubViewPtr(S,FLB);
      If assigned(StackWindow) then
        dispose(StackWindow,done);
      StackWindow:=@Self;
    end;

  procedure TStackWindow.Store(var S: TStream);
    begin
      inherited Store(S);
      PutSubViewPtr(S,FLB);
    end;

  Destructor TStackWindow.Done;
    begin
      StackWindow:=nil;
      Dispose(FLB,done);
      inherited done;
    end;

{****************************************************************************
                         Init/Final
****************************************************************************}


function GetGDBTargetShortName : string;
begin
{$ifdef SUPPORT_REMOTE}
{$ifdef PALMOSGDB}
GetGDBTargetShortName:='palmos';
{$else}
GetGDBTargetShortName:='linux';
{$endif PALMOSGDB}
{$else not SUPPORT_REMOTE}
{$ifdef COMPILER_1_0}
GetGDBTargetShortName:=source_os.shortname
{$else}
GetGDBTargetShortName:=source_info.shortname
{$endif}
{$endif not SUPPORT_REMOTE}
end;

procedure InitDebugger;
{$ifdef DEBUG}
var s : string;
    i,p : longint;
{$endif DEBUG}
var
   NeedRecompileExe : boolean;
   cm : longint;
begin
{$ifdef DEBUG}
  if not use_gdb_file then
    begin
      Assign(gdb_file,GDBOutFileName);
      {$I-}
      Rewrite(gdb_file);
      if InOutRes<>0 then
        begin
          s:=GDBOutFileName;
          p:=pos('.',s);
          if p>1 then
           for i:=0 to 9 do
             begin
               s:=copy(s,1,p-2)+chr(i+ord('0'))+copy(s,p,length(s));
               InOutRes:=0;
               Assign(gdb_file,s);
               rewrite(gdb_file);
               if InOutRes=0 then
                 break;
             end;
        end;
      if IOResult=0 then
        Use_gdb_file:=true;
    end;
  {$I+}
{$endif}

  NeedRecompileExe:=false;
  if UpCaseStr(TargetSwitches^.GetCurrSelParam)<>UpCaseStr(GetGDBTargetShortName) then
    begin
     ClearFormatParams;
     AddFormatParamStr(TargetSwitches^.GetCurrSelParam);
     AddFormatParamStr(GetGDBTargetShortName);
     cm:=ConfirmBox(msg_cantdebugchangetargetto,@FormatParams,true);
     if cm=cmCancel then
       Exit;
     if cm=cmYes then
       begin
         { force recompilation }
         PrevMainFile:='';
         NeedRecompileExe:=true;
         TargetSwitches^.SetCurrSelParam(GetGDBTargetShortName);
         If DebugInfoSwitches^.GetCurrSelParam='-' then
           DebugInfoSwitches^.SetCurrSelParam('l');
         IDEApp.UpdateTarget;
       end;
    end;
  if not NeedRecompileExe then
    NeedRecompileExe:=(not ExistsFile(ExeFile)) or (CompilationPhase<>cpDone) or
     (PrevMainFile<>MainFile) or NeedRecompile(cRun,false);
  if Not NeedRecompileExe and Not MainHasDebugInfo then
    begin
     ClearFormatParams;
     cm:=ConfirmBox(msg_compiledwithoutdebuginforecompile,nil,true);
     if cm=cmCancel then
       Exit;
     if cm=cmYes then
       begin
         { force recompilation }
         PrevMainFile:='';
         NeedRecompileExe:=true;
         DebugInfoSwitches^.SetCurrSelParam('l');
       end;
    end;
  if NeedRecompileExe then
    DoCompile(cRun);
  if CompilationPhase<>cpDone then
    Exit;
  if (EXEFile='') then
   begin
     ErrorBox(msg_nothingtodebug,nil);
     Exit;
   end;
{ init debugcontroller }
  if not assigned(Debugger) then
    begin
      PushStatus(msg_startingdebugger);
      new(Debugger,Init);
      PopStatus;
    end;
  Debugger^.SetExe(ExeFile);
{$ifdef GDBWINDOW}
  InitGDBWindow;
{$endif def GDBWINDOW}
end;


procedure DoneDebugger;
begin
{$ifdef DEBUG}
  If IDEApp.IsRunning then
    PushStatus('Closing debugger');
{$endif}
  if assigned(Debugger) then
   dispose(Debugger,Done);
  Debugger:=nil;
{$ifdef DOS}
  If assigned(UserScreen) then
    PDosScreen(UserScreen)^.FreeGraphBuffer;
{$endif DOS}
{$ifdef DEBUG}
  If Use_gdb_file then
    begin
      Use_gdb_file:=false;
      Close(GDB_file);
    end;
  If IDEApp.IsRunning then
    PopStatus;
{$endif DEBUG}
end;

procedure InitGDBWindow;
var
  R : TRect;
begin
  if GDBWindow=nil then
    begin
      DeskTop^.GetExtent(R);
      new(GDBWindow,init(R));
      DeskTop^.Insert(GDBWindow);
    end;
end;

procedure DoneGDBWindow;
begin
  If IDEApp.IsRunning and
     assigned(GDBWindow) then
    begin
      DeskTop^.Delete(GDBWindow);
    end;
  GDBWindow:=nil;
end;

procedure InitDisassemblyWindow;
var
  R : TRect;
begin
  if DisassemblyWindow=nil then
    begin
      DeskTop^.GetExtent(R);
      new(DisassemblyWindow,init(R));
      DeskTop^.Insert(DisassemblyWindow);
    end;
end;

procedure DoneDisassemblyWindow;
begin
  if assigned(DisassemblyWindow) then
    begin
      DeskTop^.Delete(DisassemblyWindow);
      Dispose(DisassemblyWindow,Done);
      DisassemblyWindow:=nil;
    end;
end;

procedure InitStackWindow;
begin
  if StackWindow=nil then
    begin
      new(StackWindow,init);
      DeskTop^.Insert(StackWindow);
    end;
end;

procedure DoneStackWindow;
begin
  if assigned(StackWindow) then
    begin
      DeskTop^.Delete(StackWindow);
      StackWindow:=nil;
    end;
end;

procedure InitBreakpoints;
begin
  New(BreakpointsCollection,init(10,10));
end;

procedure DoneBreakpoints;
begin
  Dispose(BreakpointsCollection,Done);
  BreakpointsCollection:=nil;
end;

procedure InitWatches;
begin
  New(WatchesCollection,init);
end;

procedure DoneWatches;
begin
  Dispose(WatchesCollection,Done);
  WatchesCollection:=nil;
end;

procedure RegisterFPDebugViews;
begin
  RegisterType(RWatchesWindow);
  RegisterType(RBreakpointsWindow);
  RegisterType(RWatchesListBox);
  RegisterType(RBreakpointsListBox);
  RegisterType(RStackWindow);
  RegisterType(RFramesListBox);
  RegisterType(RBreakpoint);
  RegisterType(RWatch);
  RegisterType(RBreakpointCollection);
  RegisterType(RWatchesCollection);
end;

end.

{
  $Log: fpdebug.pas,v $
  Revision 1.46  2003/03/30 12:12:12  armin
  * allow local and remote debugging if SUPPORT_REMOTE is given

  Revision 1.45  2003/03/27 14:10:55  pierre
   * fix problem with mixed case target names as suggested by Armin Diehl

  Revision 1.44  2003/01/14 16:25:23  pierre
   + small palmos specific additions

  Revision 1.43  2002/12/18 01:20:12  pierre
   + Use TEditorInputLine instead of TInputLine

  Revision 1.42  2002/12/16 15:15:40  pierre
   * Added TBreakpointCollection.FindBreakpointAt method

  Revision 1.41  2002/12/16 09:05:28  pierre
   * sanity ceck in ToggleFileLine method

  Revision 1.40  2002/02/09 02:04:46  pierre
   * fix problem with disable all invalid breakpoints

  Revision 1.39  2002/12/12 00:05:57  pierre
   * add code for breakpoint moves + registers in fprags.pas unit

  Revision 1.38  2002/11/30 01:56:52  pierre
   + powerpc cpu support started

  Revision 1.37  2002/11/28 13:00:25  pierre
   + remote support

  Revision 1.36  2002/11/21 17:52:28  pierre
   * some crossgdb infos added

  Revision 1.35  2002/11/21 15:48:39  pierre
   * fix several problems related to remote cross debugging

  Revision 1.34  2002/11/21 00:37:56  pierre
   + some cross gdb enhancements

  Revision 1.33  2002/09/21 22:23:49  pierre
   * restore text mode on reset for Dos apps

  Revision 1.32  2002/09/17 21:58:45  pierre
   * correct last fpu patch so 'info all' is called only once

  Revision 1.31  2002/09/17 21:48:41  pierre
   * allow fpu window to be resized

  Revision 1.30  2002/09/17 21:20:07  pierre
   * fix infinite recursion if GDB window and register window open

  Revision 1.29  2002/09/13 22:30:50  pierre
   * only fpc uses video unit

  Revision 1.28  2002/09/13 08:13:07  pierre
   * avoid RTE 201 in hexstr calls

  Revision 1.27  2002/09/07 21:04:41  carl
    * fix range check errors for version 1.1 compilation

  Revision 1.26  2002/09/07 15:40:42  peter
    * old logs removed and tabs fixed

  Revision 1.25  2002/09/03 13:59:47  pierre
   + added history for watches and breakpoints

  Revision 1.24  2002/09/02 10:18:09  pierre
   * fix problems with breakpoint lists

  Revision 1.23  2002/08/13 08:59:12  pierre
   + Run menu changes depending on wether the debuggee is running or not

  Revision 1.22  2002/08/13 07:15:02  pierre
   + Disable all invalid breakpoints feature added

  Revision 1.21  2002/06/10 19:26:48  pierre
   * check if DebuggeTTY is a valid terminal

  Revision 1.20  2002/06/06 14:11:25  pierre
   * handle win32 Ctrl-C change for graphic version

  Revision 1.19  2002/06/06 08:16:18  pierre
   * avoid crashes if quitting while debuggee is running

  Revision 1.18  2002/04/25 13:33:31  pierre
   * fix the problem with dirs containing asterisks

  Revision 1.17  2002/04/17 11:11:54  pierre
    * avoid problems for ClassVariable in Watches window

  Revision 1.16  2002/04/11 06:41:13  pierre
   * fix problem of TWatchesListBox with fvision

  Revision 1.15  2002/04/03 06:18:30  pierre
   * fix some win32 GDB filename problems

  Revision 1.14  2002/04/02 15:09:38  pierre
   * fixed wrong exit without unlock

  Revision 1.13  2002/04/02 13:23:54  pierre
   * Use StrToCard and HexToCard functions to avoid signed/unsigned overflows

  Revision 1.12  2002/04/02 12:20:58  pierre
   * fix problem with breakpoints in subdirs

  Revision 1.11  2002/04/02 11:10:29  pierre
   * fix FPC_BREAK_ERROR problem and avoid blinking J

  Revision 1.10  2002/03/27 11:24:09  pierre
   * fix several problems related to long file nmze support for win32 exes

  Revision 1.9  2002/02/06 14:45:00  pierre
   + handle signals

}
