{
    $Id: fpmrun.inc,v 1.8 2002/12/18 01:21:42 pierre Exp $
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Run menu entries

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$define MODIFIEDINDEBUG}

function TIDEApp.AskRecompileIfModified:boolean;
var
  PW,PPW : PSourceWindow;
  Checkmodifiededitor : boolean;
begin
  AskRecompileIfModified:=false;
{$ifdef MODIFIEDINDEBUG}
  if not AskRecompileIfModifiedFlag then
   exit;
  Checkmodifiededitor:=false;
  PW:=FirstEditorWindow;
  PPW:=PW;
  while assigned(PW) do
   begin
     If PW^.HelpCtx=hcSourceWindow then
      begin
        if pw^.editor^.getmodified then
         if pw^.editor^.core^.getmodifytime > LastCompileTime then
           begin
             Checkmodifiededitor:=true;
             break;
           end;
      end;
     PW:=PSourceWindow(PW^.next);
     While assigned(PW) and (PW<>PPW) and (PW^.HelpCtx<>hcSourceWindow) do
      PW:=PSourceWindow(PW^.next);
     If PW=PPW then
      break;
   end;
  if Checkmodifiededitor then
   begin
     if (MessageBox(#3+'You''ve edited a file, recompile the project?',nil,mfinformation+mfyesbutton+mfnobutton)=cmYes) then
      begin
        {$IFNDEF NODEBUG}
        if Assigned(Debugger) then
          begin
            if Debugger^.IsRunning then
              RestartingDebugger:=true;
          end;
        DoResetDebugger;
        {$ENDIF}
        DoRun;
        AskRecompileIfModified:=true;
      end
     else
      AskRecompileIfModifiedFlag:=false;
   end;
{$endif MODIFIEDINDEBUG}
end;


procedure TIDEApp.DoStepOver;
begin
{$ifndef NODEBUG}
  if not assigned(Debugger) or Not Debugger^.HasExe then
   begin
     InitDebugger;
     if not assigned(Debugger) then
      begin
        NoDebugger;
        exit;
      end;
   end;
  if not Debugger^.IsRunning then
    Debugger^.StartTrace
  else
   begin
     if AskRecompileIfModified then
      exit;
     if InDisassemblyWindow then
       Debugger^.TraceNextI
     else
       Debugger^.TraceNext;
   end;
  {While (Debugger^.InvalidSourceLine and
         Debugger^.IsRunning and
         not Debugger^.error) do
    begin
      Inc(Debugger^.HiddenStepsCount);
     if InDisassemblyWindow then
       Debugger^.TraceNextI
     else
      Debugger^.TraceNext;
    end;}
  Debugger^.AnnotateError;
{$else NODEBUG}
  NoDebugger;
{$endif NODEBUG}
end;


procedure TIDEApp.DoTraceInto;
begin
{$ifndef NODEBUG}
  if not assigned(Debugger) or Not Debugger^.HasExe then
   begin
     InitDebugger;
     if not assigned(Debugger) then
      begin
        NoDebugger;
        exit;
      end;
   end;
  if not debugger^.IsRunning then
    Debugger^.StartTrace
  else
   begin
     if AskRecompileIfModified then
      exit;
     if InDisassemblyWindow then
       Debugger^.TraceStepI
     else
       Debugger^.TraceStep;
   end;
  { I think we should not try to go deeper !
    if the source is not found PM }
  While (Debugger^.InvalidSourceLine and
         Debugger^.IsRunning and
         not Debugger^.error) do
    begin
      Inc(Debugger^.HiddenStepsCount);
      if InDisassemblyWindow then
        Debugger^.TraceNextI
      else
        Debugger^.TraceNext;
    end;
  Debugger^.AnnotateError;
{$else NODEBUG}
  NoDebugger;
{$endif NODEBUG}
end;

procedure TIDEApp.DoContUntilReturn;
begin
{$ifndef NODEBUG}
  if not assigned(Debugger) or Not Debugger^.HasExe then
   begin
     InitDebugger;
     if not assigned(Debugger) then
      begin
        NoDebugger;
        exit;
      end;
   end;
  if not debugger^.IsRunning then
    Debugger^.Run
  else
   begin
     if AskRecompileIfModified then
      exit;
     Debugger^.UntilReturn;
   end;
  Debugger^.AnnotateError;
{$else NODEBUG}
  NoDebugger;
{$endif NODEBUG}
end;

procedure TIDEApp.DoRun;
var
  RunDirect : boolean;
begin

{$ifndef NODEBUG}
  if not assigned(Debugger) or not Debugger^.HasExe or not Debugger^.IsRunning then
{$endif}
    begin
      if (not ExistsFile(ExeFile)) or (CompilationPhase<>cpDone) or
         NeedRecompile(cRun,false) then
        begin
          DoCompile(cRun);
          if CompilationPhase<>cpDone then
            Exit;
          if not Status.IsExe then
            begin
              ErrorBox(msg_cannotrununit,nil);
              Exit;
            end;
          if IsLibrary then
            begin
              ErrorBox(msg_cannotrunlibrary,nil);
              Exit;
            end;
        end;
      if (EXEFile='') then
       begin
         ErrorBox(msg_nothingtorun,nil);
         Exit;
       end;
      if not ExistsFile(ExeFile) then
        begin
          MsgParms[1].Ptr:=@EXEFile;
          ErrorBox(msg_invalidfilename,@MsgParms);
          Exit;
        end;
      RunDirect:=true;
{$ifndef NODEBUG}
      { we use debugger if and only if there are active breakpoints
        AND the target is correct for debugging !! PM }
      if (ActiveBreakpoints or RestartingDebugger) and
{$ifdef COMPILER_1_0}
        (target_os.shortname=source_os.shortname)
{$else COMPILER_1_0}
        (target_info.shortname=source_info.shortname)
{$endif COMPILER_1_0}
        then
        begin
          if not assigned(Debugger) or Not Debugger^.HasExe then
            InitDebugger;
          if assigned(Debugger) then
            begin
              if RestartingDebugger then
                begin
                  RestartingDebugger:=false;
                  Debugger^.StartTrace;
                end
              else
                Debugger^.Run;
              RunDirect:=false;
            end;
        end;
{$endif ndef NODEBUG}
      if Not RunDirect then
        exit;
{$ifdef Unix}
      if (DebuggeeTTY<>'') then
        DoExecute(ExeFile,GetRunParameters,DebuggeeTTY,DebuggeeTTY,DebuggeeTTY,exNormal)
      else
{$endif Unix}
      DoExecute(ExeFile,GetRunParameters,'','','',exNormal);
      { In case we have something that the compiler touched }
      AskToReloadAllModifiedFiles;
      LastExitCode:=ExecuteResult;
      If IOStatus<>0 then
        begin
          MsgParms[1].Ptr:=@EXEFile;
          MsgParms[2].long:=IOStatus;
          InformationBox(msg_programnotrundoserroris,@MsgParms);
        end
      else If LastExitCode<>0 then
        begin
          MsgParms[1].Ptr:=@EXEFile;
          MsgParms[2].long:=LastExitCode;
          InformationBox(msg_programfileexitedwithexitcode,@MsgParms);
        end;
    end
{$ifndef NODEBUG}
  else
    Debugger^.Continue
{$endif}
  ;
end;

procedure TIDEApp.UpdateRunMenu(DebuggeeRunning : boolean);
var MenuItem : PMenuItem;
begin
  MenuItem:=PAdvancedMenuBar(MenuBar)^.GetMenuItem(cmRun);
  if assigned(MenuItem) then
    begin
      If assigned(MenuItem^.Name) then
        DisposeStr(MenuItem^.Name);
      if DebuggeeRunning then
        MenuItem^.Name:=NewStr(menu_run_continue)
      else
        MenuItem^.Name:=NewStr(menu_run_run);
    end;
  MenuItem:=PAdvancedMenuBar(MenuBar)^.GetMenuItem(cmResetDebugger);
  if assigned(MenuItem) then
    MenuItem^.Disabled:=not DebuggeeRunning;
  MenuItem:=PAdvancedMenuBar(MenuBar)^.GetMenuItem(cmUntilReturn);
  if assigned(MenuItem) then
    MenuItem^.Disabled:=not DebuggeeRunning;
end;


procedure TIDEApp.Parameters;
var R,R2: TRect;
    D: PCenterDialog;
    IL: PEditorInputLine;
begin
  R.Assign(0,0,round(ScreenWidth*54/80),4);
  New(D, Init(R, dialog_programparameters));
  with D^ do
  begin
    GetExtent(R); R.Grow(-2,-1); Inc(R.A.Y); R.B.Y:=R.A.Y+1;
    R2.Copy(R); R2.A.X:=16; Dec(R2.B.X,4);
    New(IL, Init(R2, 255));
    IL^.Data^:=GetRunParameters;
    Insert(IL);
    R2.Copy(R); R2.A.X:=R2.B.X-3; R2.B.X:=R2.A.X+3;
    Insert(New(PHistory, Init(R2, IL, hidRunParameters)));
    R2.Copy(R); R2.B.X:=16;
    Insert(New(PLabel, Init(R2, label_parameters_parameter, IL)));
  end;
  InsertButtons(D);
  IL^.Select;
  if Desktop^.ExecView(D)=cmOK then
  begin
    SetRunParameters(IL^.Data^);
  end;
  Dispose(D, Done);
end;

procedure TIDEApp.DoResetDebugger;
begin
{$ifndef NODEBUG}
  if assigned(Debugger) then
     DoneDebugger;
  UpdateScreen(true);
{$else NODEBUG}
  NoDebugger;
{$endif NODEBUG}
end;

procedure TIDEApp.DoContToCursor;
{$ifndef NODEBUG}
var
  W : PFPWindow;
  PDL : PDisasLine;
  S,FileName : string;
  P,CurY,LineNr : longint;
{$endif}
begin
{$ifndef NODEBUG}
  if (DeskTop^.Current=nil) or
     ((TypeOf(DeskTop^.Current^)<>TypeOf(TSourceWindow)) and
      (TypeOf(DeskTop^.Current^)<>TypeOf(TDisassemblyWindow))) then
    Begin
       ErrorBox(msg_impossibletoreachcursor,nil);
       Exit;
    End;

  If not assigned(Debugger) or Not Debugger^.HasExe then
    begin
      InitDebugger;
      if not assigned(Debugger) then
        begin
          NoDebugger;
          exit;
        end;
    end;

  W:=PFPWindow(DeskTop^.Current);
  If assigned(W) then
    begin
      If TypeOf(W^)=TypeOf(TSourceWindow) then
        begin
          FileName:=PSourceWindow(W)^.Editor^.FileName;
          LineNr:=PSourceWindow(W)^.Editor^.CurPos.Y+1;
          Debugger^.Command('tbreak '+GDBFileName(NameAndExtOf(FileName))+':'+IntToStr(LineNr));
          Debugger^.Continue;
        end
      else
        begin
          CurY:=PDisassemblyWindow(W)^.Editor^.CurPos.Y;
          if CurY<PDisassemblyWindow(W)^.Editor^.GetLineCount then
            PDL:=PDisasLine(PDisassemblyWindow(W)^.Editor^.GetLine(CurY))
          else
            PDL:=nil;
          if assigned(PDL) then
            begin
              if PDL^.Address<>0 then
                begin
                  Debugger^.Command('tbreak *0x'+IntToHex(PDL^.Address,8));
                end
              else
                begin
                  S:=PDisassemblyWindow(W)^.Editor^.GetDisplayText(PDisassemblyWindow(W)^.Editor^.CurPos.Y);
                  p:=pos(':',S);
                  FileName:=Copy(S,1,p-1);
                  S:=Copy(S,p+1,high(S));
                  p:=pos(' ',S);
                  S:=Copy(S,1,p-1);
                  LineNr:=StrToInt(S);
                  Debugger^.Command('tbreak '+GDBFileName(NameAndExtOf(FileName))+':'+IntToStr(LineNr));
                end;
              Debugger^.Continue;
            end;
        end;
    end;
{$else NODEBUG}
  NoDebugger;
{$endif NODEBUG}
end;

procedure TIDEApp.DoOpenGDBWindow;
begin
{$ifndef NODEBUG}
  InitGDBWindow;
  if not assigned(Debugger) then
    begin
      new(Debugger,Init);
      if assigned(Debugger) then
        Debugger^.SetExe(ExeFile);
    end;
  If assigned(GDBWindow) then
    GDBWindow^.MakeFirst;
{$else NODEBUG}
  NoDebugger;
{$endif NODEBUG}
end;

procedure TIDEApp.DoToggleBreak;
{$ifndef NODEBUG}
var
  W : PSourceWindow;
  WD : PDisassemblyWindow;
  PDL : PDisasLine;
  PB : PBreakpoint;
  S,FileName : string;
  b : boolean;
  CurY,P,LineNr : longint;
{$endif}
begin
{$ifndef NODEBUG}
  if (DeskTop^.Current=nil) or
     (TypeOf(DeskTop^.Current^)<>TypeOf(TSourceWindow)) and
     (TypeOf(DeskTop^.Current^)<>TypeOf(TDisassemblyWindow)) then
    Begin
       ErrorBox(msg_impossibletosetbreakpoint,nil);
       Exit;
    End;

  if assigned (DeskTop^.Current) and
     (TypeOf(DeskTop^.Current^)=TypeOf(TSourceWindow)) then
    begin
      W:=PSourceWindow(DeskTop^.Current);
      FileName:=W^.Editor^.FileName;
      If FileName='' then
        begin
          W^.Editor^.SaveAs;
          FileName:=W^.Editor^.FileName;
          If FileName='' then
            Begin
              ErrorBox(msg_impossibletosetbreakpoint,nil);
              Exit;
            End;
        end;
      LineNr:=W^.Editor^.CurPos.Y+1;
      BreakpointsCollection^.ToggleFileLine(FileName,LineNr);
    end
  else if assigned (DeskTop^.Current) and
     (TypeOf(DeskTop^.Current^)=TypeOf(TDisassemblyWindow)) then
    begin
      WD:=PDisassemblyWindow(DeskTop^.Current);
      CurY:=WD^.Editor^.CurPos.Y;
      if CurY<WD^.Editor^.GetLineCount then
        PDL:=PDisasLine(WD^.Editor^.GetLine(CurY))
      else
        PDL:=nil;
      if assigned(PDL) then
        begin
          if PDL^.Address<>0 then
            begin
              PB:=New(PBreakpoint,init_address(IntToHex(PDL^.Address,8)));
              BreakpointsCollection^.Insert(PB);
              WD^.Editor^.SetLineFlagState(CurY,lfBreakpoint,true);
            end
          else
            begin
              S:=WD^.Editor^.GetDisplayText(WD^.Editor^.CurPos.Y);
              p:=pos(':',S);
              FileName:=Copy(S,1,p-1);
              S:=Copy(S,p+1,high(S));
              p:=pos(' ',S);
              S:=Copy(S,1,p-1);
              LineNr:=StrToInt(S);
              b:=BreakpointsCollection^.ToggleFileLine(FileName,LineNr);
              WD^.Editor^.SetLineFlagState(CurY,lfBreakpoint,b);
            end;
        end;
    end;
{$else NODEBUG}
  NoDebugger;
{$endif NODEBUG}
end;

{
  $Log: fpmrun.inc,v $
  Revision 1.8  2002/12/18 01:21:42  pierre
   + Use TEditorInputLine instead of TInputLine

  Revision 1.7  2002/12/16 09:06:08  pierre
   * don't insert breakpoints in unsaved sources

  Revision 1.6  2002/10/12 19:43:07  hajny
    * missing HasSignal conditionals added (needed for FPC/2)

  Revision 1.5  2002/09/07 15:40:44  peter
    * old logs removed and tabs fixed

  Revision 1.4  2002/09/05 08:45:59  pierre
   * try to fix recompilation on changes problems

  Revision 1.3  2002/08/13 08:59:12  pierre
   + Run menu changes depending on wether the debuggee is running or not

}
