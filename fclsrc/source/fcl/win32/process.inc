uses windows;

Function TProcess.GetRunning : Boolean;

begin
  IF FRunning then
    Frunning:=GetExitStatus=Still_Active;
  Result:=FRunning;
end;

Procedure TProcess.Execute;

Var PName,PDir : PChar;
    FStartupInfo : TStartupInfo;
    FProcessAttributes,
    FTHreadAttributes : TSecurityAttributes;    
    FProcessInformation : TProcessInformation;
    status : longbool;
    
begin
  FillChar(FProcessAttributes,SizeOf(FProcessAttributes),#0);
  FillChar(FThreadAttributes,SizeOf(FThreadAttributes),#0);
  FillChar(FStartupInfo,SizeOf(FStartupInfo),#0);
  if poNoConsole in FCReateOptions then
    FCreationFlags:=FCreationFlags or Detached_Process;
  If poRunSuspended in FCreateOptions Then
    FCreationFlags:=FCreationFlags or Create_Suspended;
  If poUsePipes in FCreateOptions then
    begin
    FreeStreams;
    CreatePipeStreams (FChildInputSTream,FParentOutPutStream);
    CreatePipeStreams (FParentInputStream,FChildOutPutStream);
    if poStdErrToOutPut in FCreateOptions then
      CreatePipeStreams (FParentErrorStream,FChildErrorStream)
    else
      begin
      FChildErrorStream:=FChildOutPutStream;
      FParentErrorStream:=FParentInputStream;
      end;
    end;
  With FStartupInfo do
    begin
    if poUsePipes in FCreateOptions then
      begin
      dwFlags:=dwFlags or Startf_UseStdHandles;
      hStdInput:=FChildInputStream.Handle;
      hStdOutput:=FChildOutPutStream.Handle;
      hStdError:=FChildErrorStream.Handle;
      end;
    If (FFillAttribute<>-1) then
      begin
      dwFlags:=dwFlags or Startf_UseFillAttribute;
      dwFillAttribute:=FFIllAttribute;
      end;
    If FShowWindow then
      begin
      dwFlags:=dwFlags or Startf_UseShowWindow;
      // ?? dwXCountChars:=Value;
      end;
    if FWindowWidth<>-1 then
      begin
      dwFlags:=dwFlags or Startf_UseCountChars;
      dwXCountChars:=FWindowWidth;
      end;
    if FWindowRows<>-1 then
      begin
      dwFlags:=dwFlags or Startf_UseCountChars;
      dwYCountChars:=FWindowRows;
      end;
    if FWindowHeight<>-1 then
      begin
      dwFlags:=dwFlags or Startf_UsePosition;
      dwYsize:=FWindowHeight;
      end;
    If FWindowWidth<>-1 then
      begin
      dwFlags:=dwFlags or Startf_UsePosition;
      dwxsize:=FWindowWidth;
      end;
    IF FWindowLeft<>-1 then
      begin
      dwFlags:=dwFlags or Startf_UseSize;
      dwx:=FWindowLeft;
      end;
    If FWindowTop<>-1 then
      begin
      dwFlags:=dwFlags or Startf_UseSize;
      dwy:=FWindowTop;
      end;
    end;
  Writeln ('About to start');  
  If FApplicationName<>'' then PName:=Pchar(FApplicationName) else PName:=Nil;
  If FCurrentDirectory<>'' then PName:=Pchar(FCurrentDirectory) else PDir:=Nil;
  Status:=CreateProcess (Pname,PChar(FCommandLine),@FProcessAttributes,@FThreadAttributes,
                 FInheritHandles,FCreationFlags,FEnvironment,PDir,@FStartupInfo,
                 @fProcessInformation);
  Writeln ('Created ',Status);               
  FTHreadHandle:=fProcessInformation.hthread;
  Writeln ('ThreadHandle :',FThreadHandle);
  FHandle:=fProcessInformation.hProcess;
  Writeln ('Process Handle :',FHandle);
  FPID:=fProcessInformation.dwProcessID;
  Writeln ('Process Handle :',FPID);
  FRunning:=True;
  if (poWaitOnExit in FCreateOptions) and
      not (poRunSuspended in FCreateOptions) then
    WaitOnExit;
end;

Function TProcess.WaitOnExit : Dword;

begin
  Result:=WaitForSingleObject (FHandle,Infinite);
  If Result<>Wait_Failed then
    GetExitStatus;
  FRunning:=False;
end;

Function TProcess.Suspend : Longint;

begin
  Result:=SuspendThread(ThreadHandle);
end;

Function TProcess.Resume : LongInt;

begin
  Result:=ResumeThread(ThreadHandle);
end;

Function TProcess.Terminate(AExitCode : Integer) : Boolean;

begin
  Result:=False;
  If ExitStatus=Still_active then
    Result:=TerminateProcess(Handle,AexitCode);
end;
{
  $Log: process.inc,v $
  Revision 1.3  2000/07/25 11:27:35  jonas
    * fixed missing comment openers for log section

  Revision 1.2  2000/07/13 11:33:07  michael
  + removed logs
 
}
