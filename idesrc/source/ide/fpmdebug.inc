{
    $Id: fpmdebug.inc,v 1.6 2002/11/28 12:57:00 pierre Exp $
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Debug menu entries

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifdef SUPPORT_REMOTE}
function GetRemoteString : string;
var
  St : string;
begin
  St:=RemoteSendCommand;
  If RemoteConfig<>'' then
    ReplaceStrI(St,'$CONFIG','-F '+RemoteConfig)
  else
    ReplaceStrI(St,'$CONFIG','');
  If RemoteIdent<>'' then
    ReplaceStrI(St,'$IDENT','-i '+RemoteIdent)
  else
    ReplaceStrI(St,'$IDENT','');
  ReplaceStrI(St,'$LOCALFILE',GDBFileName(ExeFile));
  ReplaceStrI(St,'$REMOTEDIR',RemoteDir);
  ReplaceStrI(St,'$REMOTEMACHINE',RemoteMachine);
  GetRemoteString:=st;
end;

procedure TIDEApp.TransferRemote;
var
  S,SendCommand : string;
  Executed : boolean;
begin
  SendCommand:=GetRemoteString;
  if SendCommand<>'' then
    begin
      s:='scp'+exeext;
      if LocateExeFile(s) then
        Executed:=DoExecute(s,SendCommand,'','','',exNormal)
      else
        Executed:=DoExecute('scp',SendCommand,'','','',exNormal);
      if Executed then
        begin
         if (DosError<>0) or (DosExitCode<>0) then
          ErrorBox(#3'Execution of'#13#3+s+' '+SendCommand+#13#3+
            'returned ('+inttostr(DosError)+','+inttostr(DosExitCode)+')',nil);
        end
      else
        ErrorBox(#3'Unable to execute'#13#3+s+' '+SendCommand,nil);

    end
  else
    ErrorBox(#3'Unable to transfer executable',nil);
end;
{$endif SUPPORT_REMOTE}

procedure TIDEApp.DoUserScreenWindow;
begin
  if UserScreenWindow=nil then
    begin
     New(UserScreenWindow, Init(UserScreen, SearchFreeWindowNo));
     Desktop^.Insert(UserScreenWindow);
    end;
  UserScreenWindow^.MakeFirst;
end;

procedure TIDEApp.DoCloseUserScreenWindow;
begin
  if Assigned(UserScreenWindow) then
    Message(UserScreenWindow,evCommand,cmClose,nil);
end;

procedure TIDEApp.DoUserScreen;
var Event : TEvent;
    Clear : Boolean;
begin
  if UserScreen=nil then
   begin
     ErrorBox(msg_userscreennotavailable,nil);
     Exit;
   end;

  ShowUserScreen;

  InitKeyBoard;
  repeat
    repeat
      Drivers.GetKeyEvent(Event);
      if Event.What=evNothing then
        GiveUpTimeSlice;
    until Event.What=evKeyboard;
    Clear:=true;
    if not UserScreen^.CanScroll then
      Clear:=false
    else
      case Event.keycode of
        kbPgUp : UserScreen^.Scroll(-20);
        kbPgDn : UserScreen^.Scroll(20);
        kbUp : UserScreen^.Scroll(-1);
        kbDown : UserScreen^.Scroll(1);
        kbHome : UserScreen^.Scroll(-1024);
        kbEnd : UserScreen^.Scroll(+1024);
      else
         Clear:=false;
      end;
    if Clear then
      ClearEvent(Event);
  until Event.what=evKeyboard;
  while (Keyboard.PollKeyEvent<>0) do
   Keyboard.GetKeyEvent;
  DoneKeyboard;

  ShowIDEScreen;
end;

procedure TIDEApp.DoShowCallStack;
begin
{$ifdef NODEBUG}
  NoDebugger;
{$else}
  If not assigned(StackWindow) then
    InitStackWindow
  else
    StackWindow^.MakeFirst;
{$endif NODEBUG}
end;

procedure TIDEApp.DoShowRegisters;
begin
{$ifdef NODEBUG}
  NoDebugger;
{$else}
  If not assigned(RegistersWindow) then
    InitRegistersWindow
  else
    RegistersWindow^.MakeFirst;
{$endif NODEBUG}
end;

procedure TIDEApp.DoShowFPU;
begin
{$ifdef NODEBUG}
  NoDebugger;
{$else}
  If not assigned(FPUWindow) then
    InitFPUWindow
  else
    FPUWindow^.MakeFirst;
{$endif NODEBUG}
end;

procedure TIDEApp.DoShowBreakpointList;
begin
{$ifdef NODEBUG}
  NoDebugger;
{$else}
  If assigned(BreakpointsWindow) then
    begin
      BreakpointsWindow^.Update;
      BreakpointsWindow^.Show;
      BreakpointsWindow^.MakeFirst;
    end
  else
    begin
      New(BreakpointsWindow,Init);
      Desktop^.Insert(BreakpointsWindow);
    end;
{$endif NODEBUG}
end;

procedure TIDEApp.DoShowWatches;
begin
{$ifdef NODEBUG}
  NoDebugger;
{$else}
  If assigned(WatchesWindow) then
    begin
      WatchesWindow^.Update;
      WatchesWindow^.MakeFirst;
    end
  else
    begin
      New(WatchesWindow,Init);
      Desktop^.Insert(WatchesWindow);
    end;
{$endif NODEBUG}
end;

procedure TIDEApp.DoAddWatch;
{$ifdef NODEBUG}
begin
  NoDebugger;
end;
{$else}
var
  P: PWatch;
  EditorWindow : PSourceWindow;
  EditorWasFirst : boolean;
  S : string;
begin
  EditorWindow:=FirstEditorWindow;
  { Leave the editor first, but only if there was already an WatchesWindow }
  EditorWasFirst:=(PWindow(Desktop^.First)=PWindow(EditorWindow)) and
                  assigned(WatchesWindow);
  If assigned(EditorWindow) then
    S:={LowerCaseStr(}EditorWindow^.Editor^.GetCurrentWord
  else
    S:='';
  P:=New(PWatch,Init(S));
  if ExecuteDialog(New(PWatchItemDialog,Init(P)),nil)<>cmCancel then
    begin
      WatchesCollection^.Insert(P);
      WatchesCollection^.Update;
      DoShowWatches;
      if EditorWasFirst then
       EditorWindow^.MakeFirst;
    end
  else
    dispose(P,Done);
end;
{$endif NODEBUG}

{
  $Log: fpmdebug.inc,v $
  Revision 1.6  2002/11/28 12:57:00  pierre
   + TransferRemote method added

  Revision 1.5  2002/10/30 22:07:11  pierre
   * only handle direction keys specially if buffer is bigger than window

  Revision 1.4  2002/09/07 15:40:43  peter
    * old logs removed and tabs fixed

  Revision 1.3  2002/09/03 13:59:09  pierre
   * don't use LowerCaseStr for AddWatch as it confuses line completion

  Revision 1.2  2002/09/02 09:27:35  pierre
   * avoid 100 CPU usage when AltF5 is used

}
