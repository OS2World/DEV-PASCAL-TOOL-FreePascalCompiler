{
    $Id: video.inc,v 1.1.2.10 2001/10/11 23:43:11 pierre Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Const
  LockUpdateScreen : Integer = 0;

Procedure LockScreenUpdate;

begin
  Inc(LockUpdateScreen);
end;

Procedure UnLockScreenUpdate;

begin
  If LockUpdateScreen>0 then
    Dec(LockUpdateScreen);
end;

Function GetLockScreenCount : integer;
begin
  GetLockScreenCount:=LockUpdateScreen;
end;

Var
  CurrentVideoDriver : TVideoDriver;
  NextVideoMode      : TVideoMode;

Const
  VideoInitialized  : Boolean = False;
  DriverInitialized : Boolean = False;
  NextVideoModeSet  : Boolean = False;

Function SetVideoDriver (Const Driver : TVideoDriver) : Boolean;
{ Sets the videodriver to be used }
begin
  If Not VideoInitialized then
    Begin
      CurrentVideoDriver:=Driver;
      DriverInitialized:=true;
      NextVideoModeSet:=false;
    End;
  SetVideoDriver:=Not VideoInitialized;
end;

Procedure GetVideoDriver (Var Driver : TVideoDriver);
{ Retrieves the current videodriver }
begin
  Driver:=CurrentVideoDriver;
end;

{ ---------------------------------------------------------------------
  External functions that use the video driver.
  ---------------------------------------------------------------------}

Procedure FreeVideoBuf;

begin
  if (VideoBuf<>Nil) then
    begin
    FreeMem(VideoBuf);
    FreeMem(OldVideoBuf);
    VideoBuf:=Nil;
    OldVideoBuf:=Nil;
    VideoBufSize:=0;
    end;
end;

Procedure AssignVideoBuf (OldCols, OldRows : Word);

Var NewVideoBuf,NewOldVideoBuf : PVideoBuf;
    S,I,C,R,NewVideoBufSize : Integer;

begin
  S:=SizeOf(TVideoCell);
  NewVideoBufSize:=ScreenWidth*ScreenHeight*S;
  GetMem(NewVideoBuf,NewVideoBufSize);
  GetMem(NewOldVideoBuf,NewVideoBufSize);
  // Move contents of old videobuffers to new if there are any.
  if (VideoBuf<>Nil) then
    begin
    If (ScreenWidth<OldCols) then
      C:=ScreenWidth
    else
      C:=OldCols;
    If (ScreenHeight<OldRows) then
      R:=ScreenHeight
    else
      R:=OldRows;
    For I:=0 to R-1 do
      begin
      Move(VideoBuf^[I*OldCols],NewVideoBuf^[I*ScreenWidth],S*C);
      Move(OldVideoBuf^[I*OldCols],NewOldVideoBuf^[I*ScreenWidth],S*C);
      end;
    end;
  FreeVideoBuf;
  VideoBufSize:=NewVideoBufSize;
  VideoBuf:=NewVideoBuf;
  OldVideoBuf:=NewOldVideoBuf;
end;

Procedure InitVideo;

begin
  If Not VideoInitialized then
    begin
    If Assigned(CurrentVideoDriver.InitDriver) then
      CurrentVideoDriver.InitDriver;
    VideoInitialized:=True;
    if NextVideoModeSet then
      SetVideoMode(NextVideoMode)
    else
      AssignVideoBuf(0,0);
    ClearScreen;
    end;
end;


Procedure DoneVideo;

begin
  If VideoInitialized then
    begin
    If Assigned(CurrentVideoDriver.DoneDriver) then
      CurrentVideoDriver.DoneDriver;
    FreeVideoBuf;
    VideoInitialized:=False;
    end;
end;

Procedure UpdateScreen (Force : Boolean);

begin
  If (LockUpdateScreen<=0) and
     Assigned(CurrentVideoDriver.UpdateScreen) then
      CurrentVideoDriver.UpdateScreen(Force);
end;

Procedure ClearScreen;

begin
  // Should this not be the current color ?
  FillWord(VideoBuf^,VideoBufSize shr 1,$0720);
  If Assigned(CurrentVideoDriver.ClearScreen) then
    CurrentVideoDriver.ClearScreen
  else
    UpdateScreen(True);
  FillWord(OldVideoBuf^,VideoBufSize shr 1,$0720);
end;

Procedure SetCursorType (NewType : Word);

begin
  if Assigned(CurrentVideoDriver.SetCursorType) then
    CurrentVideoDriver.SetCursorType(NewType)
end;

Function GetCursorType : Word;

begin
  if Assigned(CurrentVideoDriver.GetCursorType) then
    GetCursorType:=CurrentVideoDriver.GetCursorType()
  else
    GetCursorType:=0;
end;

procedure SetCursorPos(NewCursorX, NewCursorY: Word);

begin
  If Assigned(CurrentVideoDriver.SetCursorPos) then
    CurrentVideoDriver.SetCursorPos(NewCursorX, NewCursorY)
end;

function GetCapabilities: Word;
begin
  If Assigned(CurrentVideoDriver.GetCapabilities) then
    GetCapabilities:=CurrentVideoDriver.GetCapabilities()
  else
    GetCapabilities:=0;
end;


{ ---------------------------------------------------------------------
    General functions
  ---------------------------------------------------------------------}


procedure GetVideoMode(var Mode: TVideoMode);
begin
  Mode.Col := ScreenWidth;
  Mode.Row := ScreenHeight;
  Mode.Color := ScreenColor;
end;

Function SetVideoMode(Const Mode: TVideoMode) : Boolean;

Var
  OldR,OldC: Word;

begin
  SetVideoMode:=DriverInitialized;
  if not DriverInitialized then
    exit;
  If VideoInitialized then
    begin
    OldC:=ScreenWidth;
    OldR:=ScreenHeight;
    If Assigned(CurrentVideoDriver.SetVideoMode) then
      SetVideoMode:=CurrentVideoDriver.SetVideoMode(Mode)
    else
      SetVideoMode:=False;
    // Assign buffer
    If SetVideoMode then
      AssignVideoBuf(OldC,Oldr);
    end
  else
    begin
    NextVideoMode:=Mode;
    NextVideoModeSet:=true;
    end;
end;


Function GetVideoModeCount : Word;

begin
  If Assigned(CurrentVideoDriver.GetVideoModeCount) then
    GetVideoModeCount:=CurrentVideoDriver.GetVideoModeCount()
  else
    GetVideoModeCount:=1;
end;

Function GetVideoModeData(Index : Word; Var Data: TVideoMode) : Boolean;

begin
  If Assigned(CurrentVideoDriver.GetVideoModeData) then
    GetVideoModeData:=CurrentVideoDriver.GetVideoModeData(Index,Data)
  else
    begin
    GetVideoModeData:=(Index=0);
    If GetVideoModeData then
      GetVideoMode(Data);
    end
end;

function DefaultErrorHandler(AErrorCode: Longint; AErrorInfo: Pointer): TErrorHandlerReturnValue;
begin
  ErrorCode := AErrorCode;
  ErrorInfo := AErrorInfo;
  DefaultErrorHandler := errAbort; { return error code }
end;



{
  $Log: video.inc,v $
  Revision 1.1.2.10  2001/10/11 23:43:11  pierre
   * fix problem in buffer copying

  Revision 1.1.2.9  2001/10/11 16:27:11  pierre
   + support setvideomode also if Video is not initialized

  Revision 1.1.2.8  2001/10/11 11:31:16  pierre
   * fix problems with VideoInitialized and buffer copies

  Revision 1.1.2.7  2001/10/10 21:03:24  michael
  + Clearscreen now also clears oldvideobuf after screenupdate

  Revision 1.1.2.6  2001/10/08 23:20:21  pierre
   * fix a small but crashing typo error

  Revision 1.1.2.5  2001/10/06 22:23:40  michael
  + Better video mode selection/setting system

  Revision 1.1.2.4  2001/10/04 18:59:11  michael
  + Made setVideoDriver a function

  Revision 1.1.2.3  2001/10/02 15:10:45  pierre
   + GetLockScreenCount function added

  Revision 1.1.2.2  2001/09/21 18:42:08  michael
  + Implemented support for custom video drivers.

  Revision 1.1.2.1  2001/01/30 22:21:22  peter
    * move api to rtl

  Revision 1.1  2001/01/13 11:13:12  peter
    * API 2 RTL

}

