{
    $Id: syncobjs.pp,v 1.6 2003/06/17 07:35:58 michael Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1998 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc} 
{$h+}
unit syncobjs;

  interface

    uses
       windows,sysutils;

    type
      PSecurityAttributes = Windows.PSecurityAttributes;
      TSecurityAttributes = Windows.TSecurityAttributes;
      TEventHandle = THandle;

{$I syncobh.inc}

implementation

{$I syncob.inc}

procedure TCriticalSection.Acquire;

begin
   EnterCriticalSection(CriticalSection);
end;

procedure TCriticalSection.Release;

begin
   LeaveCriticalSection(CriticalSection);
end;

constructor TCriticalSection.Create;

begin
  inherited Create;
  InitializeCriticalSection(CriticalSection);
end;

destructor TCriticalSection.Destroy;

begin
  DeleteCriticalSection(CriticalSection);
  inherited Destroy;
end;

destructor THandleObject.destroy;

begin
  CloseHandle(FHandle);
  inherited Destroy;
end;

constructor TEventObject.Create(EventAttributes : PSecurityAttributes;
  AManualReset,InitialState : Boolean;const Name : string);

begin
  FHandle := CreateEvent(EventAttributes, AManualReset, InitialState, PChar(Name));
end;

destructor TEventObject.destroy;

begin
  inherited;
end;

procedure TEventObject.ResetEvent;

begin
  Windows.ResetEvent(FHandle)
end;

procedure TEventObject.SetEvent;

begin
  Windows.SetEvent(FHandle);
end;

function TEventObject.WaitFor(Timeout : Cardinal) : TWaitResult;

begin
  case WaitForSingleObject(Handle, Timeout) of
    WAIT_ABANDONED: Result := wrAbandoned;
    WAIT_OBJECT_0: Result := wrSignaled;
    WAIT_TIMEOUT: Result := wrTimeout;
    WAIT_FAILED:
        begin
        Result := wrError;
        FLastError := GetLastError;
       end;
  else
    Result := wrError;    
  end;
end;

constructor TSimpleEvent.Create;

begin
  FHandle := CreateEvent(nil, True, False, nil);
end;

end.

{
  $Log: syncobjs.pp,v $
  Revision 1.6  2003/06/17 07:35:58  michael
  + Renamed ManualReset parameter to TEventObject constructor

  Revision 1.5  2003/06/14 19:16:50  michael
  + Some improvements for the Linux version

  Revision 1.4  2003/06/11 11:59:52  michael
  + Implemented Win32 of syncobjs

  Revision 1.3  2002/09/07 15:15:29  peter
    * old logs removed and tabs fixed

}