{
    $Id: mouse.inc,v 1.1.2.5 2002/09/17 11:35:28 pierre Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Var
  CurrentMouseDriver : TMouseDriver;
  MouseInitialized : Boolean;

  // Mouse queue event mechanism
  PendingMouseEvent  : array[0..MouseEventBufSize-1] of TMouseEvent;
  PendingMouseHead,
  PendingMouseTail   : PMouseEvent;
  PendingMouseEvents : byte;
  LastMouseEvent : TMouseEvent;

Procedure ClearMouseEventQueue;

begin
  PendingMouseHead:=@PendingMouseEvent;
  PendingMouseTail:=@PendingMouseEvent;
  PendingMouseEvents:=0;
  FillChar(LastMouseEvent,sizeof(TMouseEvent),0);
end;


procedure InitMouse;

begin
 If Not MouseInitialized then
   begin
   If Assigned(CurrentMouseDriver.InitDriver) Then
     CurrentMouseDriver.InitDriver();
   ClearMouseEventQueue;
   MouseInitialized:=True;
   end;
end;

procedure DoneMouse;

begin
 If MouseInitialized then
   begin
   If Assigned(CurrentMouseDriver.DoneDriver) Then
     CurrentMouseDriver.DoneDriver();
   ClearMouseEventQueue;
   MouseInitialized:=False;
   end;
end;

function DetectMouse:byte;

begin
  If Assigned(CurrentMouseDriver.DetectMouse) Then
    DetectMouse:=CurrentMouseDriver.DetectMouse()
  else
    DetectMouse:=0;
end;

procedure ShowMouse;

begin
  If Assigned(CurrentMouseDriver.ShowMouse) Then
    CurrentMouseDriver.ShowMouse();
end;

procedure HideMouse;

begin
  If Assigned(CurrentMouseDriver.HideMouse) Then
    CurrentMouseDriver.HideMouse();
end;

function GetMouseX:word;

begin
  If Assigned(CurrentMouseDriver.GetMouseX) Then
    GetMouseX:=CurrentMouseDriver.GetMouseX()
  else
    GetMouseX:=0;
end;

function GetMouseY:word;

begin
  If Assigned(CurrentMouseDriver.GetMouseY) Then
    GetMouseY:=CurrentMouseDriver.GetMouseY()
  else
    GetMouseY:=0;
end;

function GetMouseButtons:word;

begin
  If Assigned(CurrentMouseDriver.GetMouseButtons) Then
    GetMouseButtons:=CurrentMouseDriver.GetMouseButtons()
  else
    GetMouseButtons:=0;
end;

procedure SetMouseXY(x,y:word);

begin
  If Assigned(CurrentMouseDriver.SetMouseXY) Then
    CurrentMouseDriver.SetMouseXY(X,Y)
end;

Procedure GetPendingEvent(Var MouseEvent:TMouseEvent);

begin
  MouseEvent:=PendingMouseHead^;
  inc(PendingMouseHead);
  if longint(PendingMouseHead)=longint(@PendingMouseEvent)+sizeof(PendingMouseEvent) then
   PendingMouseHead:=@PendingMouseEvent;
  dec(PendingMouseEvents);
  if ((LastMouseEvent.x<>MouseEvent.x) or
     (LastMouseEvent.y<>MouseEvent.y)) and
     (LastMouseEvent.Buttons=MouseEvent.Buttons) then
    MouseEvent.Action:=MouseActionMove;
  if (LastMouseEvent.Buttons<>MouseEvent.Buttons) and
     (MouseEvent.Action<>MouseActionDown) and
     (MouseEvent.Action<>MouseActionUp) then
     begin
       if (LastMouseEvent.Buttons<MouseEvent.Buttons) then
         MouseEvent.Action:=MouseActionDown
       else
        MouseEvent.Action:=MouseActionUp;
     end;
  LastMouseEvent:=MouseEvent;
end;

procedure GetMouseEvent(var MouseEvent:TMouseEvent);

begin
  if CurrentMouseDriver.UseDefaultQueue then
    begin
    if (PendingMouseEvents>0) then
      GetPendingEvent(MouseEvent)
    else
      FillChar(MouseEvent,sizeof(MouseEvent),0);
    end
  else
    If Assigned(CurrentMouseDriver.GetMouseEvent) Then
      begin
      CurrentMouseDriver.GetMouseEvent(MouseEvent);
      LastMouseEvent:=MouseEvent;
      end
    else
      FillChar(MouseEvent,sizeof(TMouseEvent),0);
end;

procedure PutMouseEvent(const MouseEvent: TMouseEvent);
begin
  if CurrentMouseDriver.UseDefaultQueue then
    begin
    PendingMouseTail^:=MouseEvent;
    inc(PendingMouseTail);
    if longint(PendingMouseTail)=longint(@PendingMouseEvent)+sizeof(PendingMouseEvent) then
      PendingMouseTail:=@PendingMouseEvent;
    inc(PendingMouseEvents);
    end
  else
    If Assigned(CurrentMouseDriver.PutMouseEvent) then
      CurrentMouseDriver.PutMouseEvent(MouseEvent);
end;

function PollMouseEvent(var MouseEvent: TMouseEvent):boolean;

begin
  if (CurrentMouseDriver.UseDefaultQueue) and
     (PendingMouseEvents>0) then
    begin
    MouseEvent:=PendingMouseHead^;
    PollMouseEvent:=true;
    end
  else
    If Assigned(CurrentMouseDriver.PollMouseEvent) Then
      begin
      PollMouseEvent:=CurrentMouseDriver.PollMouseEvent(MouseEvent);
      // Put it in queue, so next poll/get will be faster.
      // Only if an event was found PM
      // If PollMouseEvent then
      //  PutMouseEvent(MouseEvent);
      // This is all wrong, because the Event might already
      // have been pushed in the Event Array.
      end
    else
      PollMouseEvent:=false;
end;

Procedure SetMouseDriver(Const Driver : TMouseDriver);

begin
  If Not MouseInitialized then
    CurrentMouseDriver:=Driver;
end;


Procedure GetMouseDriver(Var Driver : TMouseDriver);

begin
  Driver:=CurrentMouseDriver;
end;

{
  $Log: mouse.inc,v $
  Revision 1.1.2.5  2002/09/17 11:35:28  pierre
    * In GetMouseEvent, only modify Action field if not already set.

  Revision 1.1.2.4  2001/09/23 23:32:25  pierre
   * avoid to call PutMouseEvent on any event in PollMouseEvent as it might already be in the Event Array

  Revision 1.1.2.3  2001/09/23 23:26:38  pierre
   * avoid to call PutMouseEvent on empty events

  Revision 1.1.2.2  2001/09/23 23:21:32  pierre
   * avoid some cases with functions returning in undefined state

  Revision 1.1.2.1  2001/09/21 23:53:48  michael
  + Added mouse driver support.

}
