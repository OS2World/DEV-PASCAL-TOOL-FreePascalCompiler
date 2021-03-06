{
    $Id: eventlog.inc,v 1.3 2003/04/01 15:56:58 peter Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Win32 implementation part of event logging facility.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

uses windows,registry;

Function TEventLog.DefaultFileName : String;

begin
  Result:=ChangeFileExt(Paramstr(0),'.log');
end;

Resourcestring
  SErrNoSysLog = 'Could not open system log (error %d)';
  SErrLogFailed = 'Failed to log entry (error %d)';

Procedure TEventLog.ActivateSystemLog;

begin
  CheckIdentification;
  FLogHandle := Pointer(OpenEventLog(Nil,Pchar(Identification)));
  If FLogHandle=Nil then
    Raise ELogError.CreateFmt(SErrNoSysLog,[GetLastError]);
end;

Procedure TEventLog.DeActivateSystemLog;

begin
  CloseEventLog(Cardinal(FLogHandle));
end;

{
function ReportEvent(hEventLog: THandle; wType, wCategory: Word;
  dwEventID: DWORD; lpUserSid: Pointer; wNumStrings: Word;
  dwDataSize: DWORD; lpStrings, lpRawData: Pointer): BOOL; stdcall;
}

procedure TEventLog.WriteSystemLog(EventType : TEventType; Msg : String);

Var
  P : PChar;
  I : Integer;
  FCategory : Word;
  FEventID : DWord;
  FEventType : Word;

begin
  FCategory:=MapTypeToCategory(EventType);
  FEventID:=MapTypeToEventID(EventType);
  FEventType:=MapTypeToEvent(EventType);
  P:=PChar(Msg);
  If Not ReportEvent(Cardinal(FLogHandle),FEventType,FCategory,FEventID,Nil,1,0,@P,Nil) then
    begin
    I:=GetLastError;
    Raise ELogError.CreateFmt(SErrLogFailed,[I]);
    end;
end;

Function TEventLog.RegisterMessageFile(AFileName : String) : Boolean;

Const
  SKeyEventLog = '\SYSTEM\CurrentControlSet\Services\EventLog\Application\%s';
  SKeyCategoryCount       = 'CategoryCount';
  SKeyEventMessageFile    = 'EventMessageFile';
  SKeyCategoryMessageFile = 'CategoryMessageFile';
  SKeyTypesSupported      = 'TypesSupported';

Var
  ELKey : String;
  R : TRegistry;

begin
  CheckIdentification;
  If AFileName='' then
    AFileName:=ParamStr(0);
  R:=TRegistry.Create;
  Try
    R.RootKey:=HKEY_LOCAL_MACHINE;
    ELKey:=Format(SKeyEventLog,[IDentification]);
    Result:=R.OpenKey(ELKey,True);
    If Result then
      try
        R.WriteInteger(SKeyCategoryCount,4);
        R.WriteString(SKeyCategoryMessageFile,AFileName);
        R.WriteString(SKeyEventMessageFile,AFileName);
        R.WriteInteger(SKeyTypesSupported,7);
      except
        Result:=False;
      end
  Finally
   R.Free;
  end;
end;

function TEventLog.MapTypeToCategory(EventType: TEventType): Word;
begin
  If (EventType=ETCustom) then
    DoGetCustomEventCategory(Result)
  else
    Result:=Ord(EventType);
  If Result=0 then
    Result:=1;
end;

function TEventLog.MapTypeToEventID(EventType: TEventType): DWord;

begin
  If (EventType=ETCustom) then
    DoGetCustomEventID(Result)
  else
    begin
    If (FEventIDOffset=0) then
      FEventIDOffset:=1000;
    Result:=FEventIDOffset+Ord(EventType);
    end;
end;

function TEventLog.MapTypeToEvent(EventType: TEventType): DWord;



Const
  EVENTLOG_SUCCESS=0;
  WinET : Array[TEventType] of word = (EVENTLOG_SUCCESS,
     EVENTLOG_INFORMATION_TYPE,
     EVENTLOG_WARNING_TYPE,EVENTLOG_ERROR_TYPE,
     EVENTLOG_AUDIT_SUCCESS);

begin
  If EventType=etCustom Then
    begin
    If CustomLogType=0 then
      CustomLogType:=EVENTLOG_SUCCESS;
    Result:=CustomLogType;
    DoGetCustomEvent(Result);
    end
  else
    Result:=WinET[EventType];
end;
