{
    $Id: video.pp,v 1.1.2.9 2002/12/16 22:16:12 peter Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    Video unit for Win32

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Video;
interface

{$i videoh.inc}

implementation

uses
  windows,dos;

{$i video.inc}

var
  ConsoleInfo : TConsoleScreenBufferInfo;
  ConsoleCursorInfo : TConsoleCursorInfo;

procedure SysInitVideo;
begin
  ScreenColor:=true;
  GetConsoleScreenBufferInfo(TextRec(Output).Handle, ConsoleInfo);
  GetConsoleCursorInfo(TextRec(Output).Handle, ConsoleCursorInfo);
  {
    About the ConsoleCursorInfo record: There are 3 possible
    structures in it that can be regarded as the 'screen':
    - dwsize   : contains the cols & row in current screen buffer.
    - srwindow : Coordinates (relative to buffer) of upper left
                 & lower right corners of visible console.
    - dmMaximumWindowSize : Maximal size of Screen buffer.
    The first implementation of video used srWindow. After some
    bug-reports, this was switched to dwMaximumWindowSize.
  }
  with ConsoleInfo.dwMaximumWindowSize do
    begin
    ScreenWidth:=X;
    ScreenHeight:=Y;
    end;
  { TDrawBuffer only has FVMaxWidth elements
    larger values lead to crashes }
  if ScreenWidth> FVMaxWidth then
    ScreenWidth:=FVMaxWidth;
  CursorX:=ConsoleInfo.dwCursorPosition.x;
  CursorY:=ConsoleInfo.dwCursorPosition.y;
  if not ConsoleCursorInfo.bvisible then
    CursorLines:=0
  else
    CursorLines:=ConsoleCursorInfo.dwSize;
end;

Function SysSetVideoMode (Const Mode : TVideoMode) : Boolean;
var
  ConsoleScreenBufferInfo : TConsoleScreenBufferInfo;
  res : boolean;
  error : longint;
  WindowPos : Small_rect;
begin
  { Mode.color field is ignored }
  ScreenColor:=true;

  GetConsoleScreenBufferInfo(TextRec(Output).Handle, ConsoleScreenBufferInfo);
  {
    About the ConsoleCursorInfo record: There are 3 possible
    structures in it that can be regarded as the 'screen':
    - dwsize   : contains the cols & row in current screen buffer.
    - srwindow : Coordinates (relative to buffer) of upper left
                 & lower right corners of visible console.
    - dmMaximumWindowSize : Maximal size of Screen buffer.
    The first implementation of video used srWindow. After some
    bug-reports, this was switched to dwMaximumWindowSize.
  }
  with ConsoleScreenBufferInfo do
    begin
       If mode.col<dwMaximumWindowSize.x then
         ScreenWidth:=Mode.col
       else
         ScreenWidth:=dwMaximumWindowSize.x;
       If mode.row<dwMaximumWindowSize.y then
         ScreenHeight:=Mode.row
       else
         ScreenHeight:=dwMaximumWindowSize.y;
       { TDrawBuffer only has FVMaxWidth elements
         larger values lead to crashes }
       if ScreenWidth> FVMaxWidth then
         ScreenWidth:=FVMaxWidth;
       WindowPos.left:=0;
       WindowPos.right:=ConsoleScreenBufferInfo.srWindow.right
                        -ConsoleScreenBufferInfo.srWindow.left;
       WindowPos.top:=0;
       WindowPos.bottom:=ConsoleScreenBufferInfo.srWindow.bottom
                        -ConsoleScreenBufferInfo.srWindow.top;
       if WindowPos.Right<ScreenWidth-1 then
         WindowPos.right:=ScreenWidth-1;
       if WindowPos.Bottom<ScreenHeight-1 then
         WindowPos.Bottom:=ScreenHeight-1;
       res:=SetConsoleWindowInfo(TextRec(Output).Handle,true,WindowPos);
       if not res then
         begin
           error:=GetLastError;
           Errorcode:=errVioNoSuchMode;
         end;
       SysSetVideoMode:=res;
    end;
end;


procedure SysDoneVideo;
begin
  SetCursorType(crUnderLine);
end;


function SysGetCapabilities: Word;
begin
  SysGetCapabilities:=cpColor or cpChangeCursor;
end;


procedure SysSetCursorPos(NewCursorX, NewCursorY: Word);
var
  pos : COORD;
begin
   pos.x:=NewCursorX;
   pos.y:=NewCursorY;
   SetConsoleCursorPosition(TextRec(Output).Handle,pos);
   CursorX:=pos.x;
   CursorY:=pos.y;
end;


function SysGetCursorType: Word;
begin
   GetConsoleCursorInfo(TextRec(Output).Handle,ConsoleCursorInfo);
   if not ConsoleCursorInfo.bvisible then
     SysGetCursorType:=crHidden
   else
     case ConsoleCursorInfo.dwSize of
        1..30:
          SysGetCursorType:=crUnderline;
        31..70:
          SysGetCursorType:=crHalfBlock;
        71..100:
          SysGetCursorType:=crBlock;
     end;
end;


procedure SysSetCursorType(NewType: Word);
begin
   GetConsoleCursorInfo(TextRec(Output).Handle,ConsoleCursorInfo);
   if newType=crHidden then
     ConsoleCursorInfo.bvisible:=false
   else
     begin
        ConsoleCursorInfo.bvisible:=true;
        case NewType of
           crUnderline:
             ConsoleCursorInfo.dwSize:=10;

           crHalfBlock:
             ConsoleCursorInfo.dwSize:=50;

           crBlock:
             ConsoleCursorInfo.dwSize:=99;
        end
     end;
   SetConsoleCursorInfo(TextRec(Output).Handle,ConsoleCursorInfo);
end;

procedure SysClearScreen;
begin
  UpdateScreen(true);
end;


{$IFDEF FPC}
function WriteConsoleOutput(hConsoleOutput:HANDLE; lpBuffer:pointer; dwBufferSize:COORD; dwBufferCoord:COORD;
   var lpWriteRegion:SMALL_RECT):WINBOOL; external 'kernel32' name 'WriteConsoleOutputA';
{$ENDIF}

procedure SysUpdateScreen(Force: Boolean);
type TmpRec = Array[0..(1024*32) - 1] of TCharInfo;

type WordRec = record
                  One, Two: Byte;
               end; { wordrec }

var
   BufSize,
   BufCoord    : COORD;
   WriteRegion : SMALL_RECT;
   LineBuf     : ^TmpRec;
   BufCounter  : Longint;
   LineCounter,
   ColCounter  : Longint;
   smallforce  : boolean;
(*
begin
  if not force then
   begin
     asm
        movl    VideoBuf,%esi
        movl    OldVideoBuf,%edi
        movl    VideoBufSize,%ecx
        shrl    $2,%ecx
        repe
        cmpsl
        setne   force
     end;
   end;
  if Force then
   begin
      BufSize.X := ScreenWidth;
      BufSize.Y := ScreenHeight;

      BufCoord.X := 0;
      BufCoord.Y := 0;
      with WriteRegion do
        begin
           Top :=0;
           Left :=0;
           Bottom := ScreenHeight-1;
           Right := ScreenWidth-1;
        end;
      New(LineBuf);
      BufCounter := 0;

      for LineCounter := 1 to ScreenHeight do
        begin
           for ColCounter := 1 to ScreenWidth do
             begin
               LineBuf^[BufCounter].UniCodeChar := WordRec(VideoBuf^[BufCounter]).One;
               LineBuf^[BufCounter].Attributes := WordRec(VideoBuf^[BufCounter]).Two;

               Inc(BufCounter);
             end; { for }
        end; { for }

      WriteConsoleOutput(TextRec(Output).Handle, LineBuf, BufSize, BufCoord, WriteRegion);
      Dispose(LineBuf);

      move(VideoBuf^,OldVideoBuf^,VideoBufSize);
   end;
end;
*)
var
   x1,y1,x2,y2 : longint;

begin
  if force then
   smallforce:=true
  else
   begin
     asm
        movl    VideoBuf,%esi
        movl    OldVideoBuf,%edi
        movl    VideoBufSize,%ecx
        shrl    $2,%ecx
        repe
        cmpsl
        setne   smallforce
     end;
   end;
  if SmallForce then
   begin
      BufSize.X := ScreenWidth;
      BufSize.Y := ScreenHeight;

      BufCoord.X := 0;
      BufCoord.Y := 0;
      with WriteRegion do
        begin
           Top :=0;
           Left :=0;
           Bottom := ScreenHeight-1;
           Right := ScreenWidth-1;
        end;
      New(LineBuf);
      BufCounter := 0;
      x1:=ScreenWidth+1;
      x2:=-1;
      y1:=ScreenHeight+1;
      y2:=-1;
      for LineCounter := 1 to ScreenHeight do
        begin
           for ColCounter := 1 to ScreenWidth do
             begin
               if (WordRec(VideoBuf^[BufCounter]).One<>WordRec(OldVideoBuf^[BufCounter]).One) or
                 (WordRec(VideoBuf^[BufCounter]).Two<>WordRec(OldVideoBuf^[BufCounter]).Two) then
                 begin
                    if ColCounter<x1 then
                      x1:=ColCounter;
                    if ColCounter>x2 then
                      x2:=ColCounter;
                    if LineCounter<y1 then
                      y1:=LineCounter;
                    if LineCounter>y2 then
                      y2:=LineCounter;
                 end;
               LineBuf^[BufCounter].UniCodeChar := WordRec(VideoBuf^[BufCounter]).One;
               { If (WordRec(VideoBuf^[BufCounter]).Two and $80)<>0 then
                 LineBuf^[BufCounter].Attributes := $100+WordRec(VideoBuf^[BufCounter]).Two
               else }
                 LineBuf^[BufCounter].Attributes := WordRec(VideoBuf^[BufCounter]).Two;

               Inc(BufCounter);
             end; { for }
        end; { for }
      BufSize.X := ScreenWidth;
      BufSize.Y := ScreenHeight;

      with WriteRegion do
        begin
           if force then
             begin
               Top := 0;
               Left :=0;
               Bottom := ScreenHeight-1;
               Right := ScreenWidth-1;
               BufCoord.X := 0;
               BufCoord.Y := 0;
             end
           else
             begin
               Top := y1-1;
               Left :=x1-1;
               Bottom := y2-1;
               Right := x2-1;
               BufCoord.X := x1-1;
               BufCoord.Y := y1-1;
             end;
        end;
      {
      writeln('X1: ',x1);
      writeln('Y1: ',y1);
      writeln('X2: ',x2);
      writeln('Y2: ',y2);
      }
      WriteConsoleOutput(TextRec(Output).Handle, LineBuf, BufSize, BufCoord, WriteRegion);
      Dispose(LineBuf);

      move(VideoBuf^,OldVideoBuf^,VideoBufSize);
   end;
end;

Const
  SysVideoDriver : TVideoDriver = (
    InitDriver : @SysInitVideo;
    DoneDriver : @SysDoneVideo;
    UpdateScreen : @SysUpdateScreen;
    ClearScreen : @SysClearScreen;
    SetVideoMode : @SysSetVideoMode;
    GetVideoModeCount : Nil;
    GetVideoModeData : Nil;
    SetCursorPos : @SysSetCursorPos;
    GetCursorType : @SysGetCursorType;
    SetCursorType : @SysSetCursorType;
    GetCapabilities : @SysGetCapabilities

  );

initialization
  SetVideoDriver(SysVideoDriver);
end.
{
  $Log: video.pp,v $
  Revision 1.1.2.9  2002/12/16 22:16:12  peter
    * Fixed missing difference in last char

  Revision 1.1.2.8  2002/09/09 09:24:11  pierre
   * set return value of SysSetVideoMode

  Revision 1.1.2.7  2002/07/16 14:06:33  pierre
   + SysSetVideoMode

  Revision 1.1.2.6  2001/10/06 22:23:41  michael
  + Better video mode selection/setting system

  Revision 1.1.2.5  2001/09/21 18:42:09  michael
  + Implemented support for custom video drivers.

  Revision 1.1.2.4  2001/06/12 22:34:20  pierre
   * avoid crash at exit of IDE

  Revision 1.1.2.3  2001/04/10 20:33:04  peter
    * remove some warnings

  Revision 1.1.2.2  2001/04/02 13:29:41  pierre
   * avoid crash if DoneVideo called twice

  Revision 1.1.2.1  2001/01/30 21:52:03  peter
    * moved api utils to rtl

  Revision 1.1  2001/01/13 11:03:59  peter
    * API 2 RTL commit

}

