{
    $Id: crt.pp,v 1.1.2.16 2002/08/28 14:22:10 pierre Exp $

    Borland Pascal 7 Compatible CRT Unit for win32

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit crt;

{$mode objfpc}

interface

const
{ CRT modes }
  BW40          = 0;            { 40x25 B/W on Color Adapter }
  CO40          = 1;            { 40x25 Color on Color Adapter }
  BW80          = 2;            { 80x25 B/W on Color Adapter }
  CO80          = 3;            { 80x25 Color on Color Adapter }
  Mono          = 7;            { 80x25 on Monochrome Adapter }
  Font8x8       = 256;          { Add-in for ROM font }

{ Mode constants for 3.0 compatibility }
  C40           = CO40;
  C80           = CO80;

{ Foreground and background color constants }
  Black         = 0;
  Blue          = 1;
  Green         = 2;
  Cyan          = 3;
  Red           = 4;
  Magenta       = 5;
  Brown         = 6;
  LightGray     = 7;

{ Foreground color constants }
  DarkGray      = 8;
  LightBlue     = 9;
  LightGreen    = 10;
  LightCyan     = 11;
  LightRed      = 12;
  LightMagenta  = 13;
  Yellow        = 14;
  White         = 15;

{ Add-in for blinking }
  Blink         = 128;

var

{ Interface variables }
  CheckBreak: Boolean;    { Enable Ctrl-Break }
  CheckEOF: Boolean;      { Enable Ctrl-Z }
  DirectVideo: Boolean;   { Enable direct video addressing }
  CheckSnow: Boolean;     { Enable snow filtering }
  LastMode: Word;         { Current text mode }
  TextAttr: Byte;         { Current text attribute }
  WindMin: Word;          { Window upper left coordinates }
  WindMax: Word;          { Window lower right coordinates }
  { FPC Specific for large screen support }
  WinMinX,
  WinMinY,
  WinMaxX,
  WinMaxY  : Longint;

{ Interface procedures }
procedure AssignCrt(var F: Text);
function KeyPressed: Boolean;
function ReadKey: Char;
procedure TextMode(Mode: Integer);
procedure Window(X1,Y1,X2,Y2: Byte);
procedure GotoXY(X,Y: Byte);
function WhereX: Byte;
function WhereY: Byte;
procedure ClrScr;
procedure ClrEol;
procedure InsLine;
procedure DelLine;
procedure TextColor(Color: Byte);
procedure TextBackground(Color: Byte);
procedure LowVideo;
procedure HighVideo;
procedure NormVideo;
procedure Delay(MS: Word);
procedure Sound(Hz: Word);
procedure NoSound;

{Extra Functions}
procedure cursoron;
procedure cursoroff;
procedure cursorbig;


implementation

uses
  dos,
  windows;


var
    OutHandle     : THandle;
    InputHandle   : THandle;

    CursorSaveX   : Longint;
    CursorSaveY   : Longint;

    ScreenWidth   : Longint;
    ScreenHeight  : Longint;
    IsWindowsNT   : Boolean;

    SaveCursorSize: Longint;


{
  definition of textrec is in textrec.inc
}
{$i textrec.inc}

{****************************************************************************
                           Low level Routines
****************************************************************************}

function GetPlatformID: Longint;
var OsVersion: TOSVersionInfo;
begin
  OsVersion.dwOsVersionInfoSize := SizeOf(OsVersion);

  GetVersionEx(OsVersion);

  Result := OsVersion.dwPlatformID;
end; { func. GetPlatformID }


procedure TurnMouseOff;
var Mode: DWORD;
begin
  if GetConsoleMode(InputHandle, @Mode) then      { Turn the mouse-cursor off }
    begin
      Mode := Mode AND cardinal(NOT enable_processed_input)
                   AND cardinal(NOT enable_mouse_input);

      SetConsoleMode(InputHandle, Mode);
    end; { if }
end; { proc. TurnMouseOff }


function GetScreenHeight : longint;
var
  ConsoleInfo: TConsoleScreenBufferinfo;
begin
  if not GetConsoleScreenBufferInfo(OutHandle, ConsoleInfo) then
    begin
{$ifdef SYSTEMDEBUG}
      Writeln(stderr,'GetScreenHeight failed GetLastError returns ',GetLastError);
      Halt(1);
{$endif SYSTEMDEBUG}
      Result:=25;
    end
  else
    Result := ConsoleInfo.dwSize.Y;
end; { func. GetScreenHeight }


function GetScreenWidth : longint;
var
  ConsoleInfo: TConsoleScreenBufferInfo;
begin
  if not GetConsoleScreenBufferInfo(OutHandle, ConsoleInfo) then
    begin
{$ifdef SYSTEMDEBUG}
      Writeln(stderr,'GetScreenWidth failed GetLastError returns ',GetLastError);
      Halt(1);
{$endif SYSTEMDEBUG}
      Result:=80;
    end
  else
    Result := ConsoleInfo.dwSize.X;
end; { func. GetScreenWidth }


procedure SetScreenCursor(x,y : longint);
var
  CurInfo: TCoord;
begin
  FillChar(Curinfo, SizeOf(Curinfo), 0);
  CurInfo.X := X - 1;
  CurInfo.Y := Y - 1;

  SetConsoleCursorPosition(OutHandle, CurInfo);

  CursorSaveX := X - 1;
  CursorSaveY := Y - 1;
end;


procedure GetScreenCursor(var x,y : longint);
begin
  X := CursorSaveX + 1;
  Y := CursorSaveY + 1;
end;


{****************************************************************************
                              Helper Routines
****************************************************************************}


Function FullWin:boolean;
{
  Full Screen 80x25? Window(1,1,80,25) is used, allows faster routines
}
begin
  FullWin:=(WinMinX=1) and (WinMinY=1) and
           (WinMaxX=ScreenWidth) and (WinMaxY=ScreenHeight);
end;


{****************************************************************************
                             Public Crt Functions
****************************************************************************}


procedure textmode(mode : integer);
begin
  {!!! Not done yet !!! }
end;


Procedure TextColor(Color: Byte);
{
  Switch foregroundcolor
}
Begin
  TextAttr:=(Color and $8f) or (TextAttr and $70);
End;



Procedure TextBackground(Color: Byte);
{
  Switch backgroundcolor
}
Begin
  TextAttr:=((Color shl 4) and ($f0 and not Blink)) or (TextAttr and ($0f OR Blink) );
End;



Procedure HighVideo;
{
  Set highlighted output.
}
Begin
  TextColor(TextAttr Or $08);
End;



Procedure LowVideo;
{
  Set normal output
}
Begin
  TextColor(TextAttr And $77);
End;



Procedure NormVideo;
{
  Set normal back and foregroundcolors.
}
Begin
  TextColor(7);
  TextBackGround(0);
End;


Procedure GotoXy(X: Byte; Y: Byte);
{
  Go to coordinates X,Y in the current window.
}
Begin
  If (X>0) and (X<=WinMaxX- WinMinX+1) and
     (Y>0) and (Y<=WinMaxY-WinMinY+1) Then
   Begin
     Inc(X,WinMinX-1);
     Inc(Y,WinMinY-1);
     SetScreenCursor(x,y);
   End;
End;


Procedure Window(X1, Y1, X2, Y2: Byte);
{
  Set screen window to the specified coordinates.
}
Begin
  if (X1>X2) or (X2>ScreenWidth) or
     (Y1>Y2) or (Y2>ScreenHeight) then
   exit;
  WinMinX:=X1;
  WinMaxX:=X2;
  WinMinY:=Y1;
  WinMaxY:=Y2;
  WindMin:=((Y1-1) Shl 8)+(X1-1);
  WindMax:=((Y2-1) Shl 8)+(X2-1);
  GoToXY(1,1);
End;


procedure ClrScr;
var
  ClipRect: TSmallRect;
  SrcRect: TSmallRect;
  DestCoor: TCoord;
  CharInfo: TCharInfo;
  CharsWritten : DWORD;
begin
  CharInfo.UnicodeChar := 32;
  CharInfo.Attributes := TextAttr;

  SrcRect.Left := WinMinX - 1;
  SrcRect.Top := WinMinY - 1;
  SrcRect.Right := WinMaxX - 1;
  SrcRect.Bottom := WinMaxY - 1;
  ClipRect := SrcRect;

  if IsWindowsNT then
    begin
      DestCoor.X := -WinMaxX;
      DestCoor.Y := -WinMaxY;

      ScrollConsoleScreenBuffer(OutHandle, SrcRect, ClipRect,
                                DestCoor, CharInfo);
    end
      else begin      { Win95 seems to have a bug in scrolling, unfortunately }
             { This routine 3 times copies the bottom 12 lines to the }
             { top part of the screen. This eventually will clear the }
             { screen. }

             if WinMaxY<>WinMinY then
               begin
                 DestCoor.X := WinMinX - 1;
                 DestCoor.Y := WinMinY - ((WinMaxY - WinMinY) div 2);
               end
             else if WinMaxX<>WinMinX then
               begin
                 DestCoor.X := WinMinX - ((WinMaxX - WinMinX) div 2);
                 DestCoor.Y := WinMinY - 1;
               end
             else
               begin
                 DestCoor.X := WinMinX - 1;
                 DestCoor.Y := WinMinY - 1;
                 FillConsoleOutputCharacter(OutHandle, CharInfo.AsciiChar, 1,
                   DestCoor,CharsWritten);
                 FillConsoleOutputAttribute(OutHandle, CharInfo.Attributes, 1,
                   DestCoor,CharsWritten);
               end;


             {-------- Scroll 1st part }
             ScrollConsoleScreenBuffer(OutHandle, SrcRect, ClipRect,
                                       DestCoor, CharInfo);


             {-------- Scroll 2nd part }
             ScrollConsoleScreenBuffer(OutHandle, SrcRect, ClipRect,
                                       DestCoor, CharInfo);

             {-------- Scroll 3rd part (last line) }
             ScrollConsoleScreenBuffer(OutHandle, SrcRect, ClipRect,
                                       DestCoor, CharInfo);
           end; { if in Windows95 }

  GotoXY(1,1);
end; { proc. ClrScr }


procedure ClrEol;
{
  Clear from current position to end of line.
}
var Temp: Dword;
    CharInfo: Char;
    Coord: TCoord;
    X,Y: Longint;
begin
  GetScreenCursor(x,y);

  CharInfo := #32;

  Coord.X := X - 1;
  Coord.Y := Y - 1;

  FillConsoleOutputCharacter(OutHandle, CharInfo, WinMaxX - X + 1, Coord, @Temp);
  FillConsoleOutputAttribute(OutHandle, TextAttr, WinMaxX - X + 1, Coord, @Temp);
end;



Function WhereX: Byte;
{
  Return current X-position of cursor.
}
var
  x,y : longint;
Begin
  GetScreenCursor(x,y);
  WhereX:=x-WinMinX+1;
End;



Function WhereY: Byte;
{
  Return current Y-position of cursor.
}
var
  x,y : longint;
Begin
  GetScreenCursor(x,y);
  WhereY:=y-WinMinY+1;
End;


{*************************************************************************
                            KeyBoard
*************************************************************************}

var
   ScanCode : char;
   SpecialKey : boolean;
   DoingNumChars: Boolean;
   DoingNumCode: Byte;

Function RemapScanCode (ScanCode: word; CtrlKeyState: dword; keycode:word): byte;
  { Several remappings of scancodes are necessary to comply with what
    we get with MSDOS. Special Windows keys, as Alt-Tab, Ctrl-Esc etc.
    are excluded }
var
  AltKey, CtrlKey, ShiftKey: boolean;
const
  {
    Keypad key scancodes:

      Ctrl Norm

      $77  $47 - Home
      $8D  $48 - Up arrow
      $84  $49 - PgUp
      $8E  $4A - -
      $73  $4B - Left Arrow
      $8F  $4C - 5
      $74  $4D - Right arrow
      $4E  $4E - +
      $75  $4F - End
      $91  $50 - Down arrow
      $76  $51 - PgDn
      $92  $52 - Ins
      $93  $53 - Del
  }
  CtrlKeypadKeys: array[$47..$53] of byte =
    ($77, $8D, $84, $8E, $73, $8F, $74, $4E, $75, $91, $76, $92, $93);

begin
  AltKey := ((CtrlKeyState AND
            (RIGHT_ALT_PRESSED OR LEFT_ALT_PRESSED)) > 0);
  CtrlKey := ((CtrlKeyState AND
            (RIGHT_CTRL_PRESSED OR LEFT_CTRL_PRESSED)) > 0);
  ShiftKey := ((CtrlKeyState AND SHIFT_PRESSED) > 0);
  if AltKey then begin
    Case KeyCode of
      VK_NUMPAD0 ..
      VK_NUMPAD9    : begin
                       DoingNumChars := true;
                       DoingNumCode := Byte((DoingNumCode * 10) + (KeyCode - VK_NUMPAD0));
                      end;
    end; { case }


    case ScanCode of
    // Digits, -, =
    $02..$0D: inc(ScanCode, $76);
    // Function keys
    $3B..$44: inc(Scancode, $2D);
    $57..$58: inc(Scancode, $34);
    // Extended cursor block keys
    $47..$49, $4B, $4D, $4F..$53:
              inc(Scancode, $50);
    // Other keys
    $1C:      Scancode := $A6;   // Enter
    $35:      Scancode := $A4;   // / (keypad and normal!)
    end
   end
  else if CtrlKey then
    case Scancode of
    // Tab key
    $0F:      Scancode := $94;
    // Function keys
    $3B..$44: inc(Scancode, $23);
    $57..$58: inc(Scancode, $32);
    // Keypad keys
    $35:      Scancode := $95;   // \
    $37:      Scancode := $96;   // *
    $47..$53: Scancode := CtrlKeypadKeys[Scancode];
    end
  else if ShiftKey then
    case Scancode of
    // Function keys
    $3B..$44: inc(Scancode, $19);
    $57..$58: inc(Scancode, $30);
    end
  else
    case Scancode of
    // Function keys
    $57..$58: inc(Scancode, $2E); // F11 and F12
  end;
  Result := ScanCode;
end;


function KeyPressed : boolean;
var
  nevents, nread: dword;
  buf : TINPUTRECORD;
  AltKey: Boolean;
begin
  KeyPressed := FALSE;
  if ScanCode <> #0 then
    KeyPressed := TRUE
  else
   begin
     GetNumberOfConsoleInputEvents(TextRec(input).Handle,nevents);
     while nevents>0 do
       begin
          ReadConsoleInputA(TextRec(input).Handle,buf,1,nread);
          if buf.EventType = KEY_EVENT then
            if buf.Event.KeyEvent.bKeyDown then
              begin
                 { Alt key is VK_MENU }
                 { Capslock key is VK_CAPITAL }

                 AltKey := ((Buf.Event.KeyEvent.dwControlKeyState AND
                            (RIGHT_ALT_PRESSED OR LEFT_ALT_PRESSED)) > 0);
                 if not(Buf.Event.KeyEvent.wVirtualKeyCode in [VK_SHIFT, VK_MENU, VK_CONTROL,
                                                      VK_CAPITAL, VK_NUMLOCK,
                                                      VK_SCROLL]) then
                   begin
                      keypressed:=true;

                      if (ord(buf.Event.KeyEvent.AsciiChar) = 0) or                         (buf.Event.KeyEvent.dwControlKeyState and
                                     (LEFT_ALT_PRESSED or ENHANCED_KEY) > 0) then
                        begin
                           if (ord(buf.Event.KeyEvent.AsciiChar) = 13) and
                              (buf.Event.KeyEvent.wVirtualKeyCode = VK_RETURN) then
                             begin
                               SpecialKey:=false;
                               ScanCode:=Chr(13);
                             end
                           else
                             begin
                               SpecialKey := TRUE;
                               ScanCode := Chr(RemapScanCode(Buf.Event.KeyEvent.wVirtualScanCode, Buf.Event.KeyEvent.dwControlKeyState,
                                           Buf.Event.KeyEvent.wVirtualKeyCode));
                             end;
                        end
                      else
                        begin
                           SpecialKey := FALSE;
                           ScanCode := Chr(Ord(buf.Event.KeyEvent.AsciiChar));
                        end;

                      if Buf.Event.KeyEvent.wVirtualKeyCode in [VK_NUMPAD0..VK_NUMPAD9] then
                        if AltKey then
                          begin
                             Keypressed := false;
                             Specialkey := false;
                             ScanCode := #0;
                          end
                        else break;
                   end;
              end
             else if (Buf.Event.KeyEvent.wVirtualKeyCode in [VK_MENU]) then
               if DoingNumChars then
                 if DoingNumCode > 0 then
                   begin
                      ScanCode := Chr(DoingNumCode);
                      Keypressed := true;

                      DoingNumChars := false;
                      DoingNumCode := 0;
                      break
                   end; { if }
          { if we got a key then we can exit }
          if keypressed then
            exit;
          GetNumberOfConsoleInputEvents(TextRec(input).Handle,nevents);
       end;
   end;
end;


function ReadKey: char;
begin
  repeat
    WaitForSingleObject(TextRec(input).Handle,1000);
  until KeyPressed;

  if SpecialKey then begin
    ReadKey := #0;
    SpecialKey := FALSE;
  end
  else begin
    ReadKey := ScanCode;
    ScanCode := #0;
  end;
end;


{*************************************************************************
                                   Delay
*************************************************************************}

procedure Delay(MS: Word);
begin
  Sleep(ms);
end; { proc. Delay }


procedure sound(hz : word);
begin
  MessageBeep(0); { lame ;-) }
end;


procedure nosound;
begin
end;


{****************************************************************************
                          HighLevel Crt Functions
****************************************************************************}

procedure removeline(y : longint);
var
  ClipRect: TSmallRect;
  SrcRect: TSmallRect;
  DestCoor: TCoord;
  CharInfo: TCharInfo;
begin
  CharInfo.UnicodeChar := 32;
  CharInfo.Attributes := TextAttr;

  Y := WinMinY + Y-1;

  SrcRect.Top := Y - 01;
  SrcRect.Left := WinMinX - 1;
  SrcRect.Right := WinMaxX - 1;
  SrcRect.Bottom := WinMaxY - 1;

  DestCoor.X := WinMinX - 1;
  DestCoor.Y := Y - 2;
  ClipRect := SrcRect;

  ScrollConsoleScreenBuffer(OutHandle, SrcRect, ClipRect, DestCoor, CharInfo);
end; { proc. RemoveLine }


procedure delline;
begin
  removeline(wherey);
end; { proc. DelLine }


procedure insline;
var
  ClipRect: TSmallRect;
  SrcRect: TSmallRect;
  DestCoor: TCoord;
  CharInfo: TCharInfo;
  X,Y: Longint;
begin
  GetScreenCursor(X, Y);

  CharInfo.UnicodeChar := 32;
  CharInfo.Attributes := TextAttr;

  SrcRect.Top := Y - 1;
  SrcRect.Left := WinMinX - 1;
  SrcRect.Right := WinMaxX - 1;
  SrcRect.Bottom := WinMaxY - 1;

  DestCoor.X := WinMinX - 1;
  DestCoor.Y := Y;
  ClipRect := SrcRect;

  ScrollConsoleScreenBuffer(OutHandle, SrcRect, ClipRect, DestCoor, CharInfo);
end; { proc. InsLine }




{****************************************************************************
                             Extra Crt Functions
****************************************************************************}

procedure cursoron;
var CursorInfo: TConsoleCursorInfo;
begin
  GetConsoleCursorInfo(OutHandle, CursorInfo);
  CursorInfo.dwSize := SaveCursorSize;
  CursorInfo.bVisible := true;
  SetConsoleCursorInfo(OutHandle, CursorInfo);
end;


procedure cursoroff;
var CursorInfo: TConsoleCursorInfo;
begin
  GetConsoleCursorInfo(OutHandle, CursorInfo);
  CursorInfo.bVisible := false;
  SetConsoleCursorInfo(OutHandle, CursorInfo);
end;


procedure cursorbig;
var CursorInfo: TConsoleCursorInfo;
begin
  GetConsoleCursorInfo(OutHandle, CursorInfo);
  CursorInfo.dwSize := 99{100};
  CursorInfo.bVisible := true;
  SetConsoleCursorInfo(OutHandle, CursorInfo);
end;


{*****************************************************************************
                          Read and Write routines
*****************************************************************************}

var
  CurrX, CurrY : longint;

procedure WriteChar(c:char);
var
    Cell    : TCharInfo;
    BufSize : Coord;                    { Column-row size of source buffer }
    WritePos: TCoord;                       { Upper-left cell to write from }
    DestRect: TSmallRect;
begin
  Case C of
   #10 : begin
           Inc(CurrY);
         end;
   #13 : begin
           CurrX := WinMinX;
         end; { if }
   #08 : begin
           if CurrX > WinMinX then Dec(CurrX);
         end; { ^H }
   #07 : begin
           // MessagBeep(0);
         end; { ^G }
     else begin
            BufSize.X := 01;
            BufSize.Y := 01;

            WritePos.X := 0;
            WritePos.Y := 0;

            Cell.UniCodeChar := Ord(c);
            Cell.Attributes := TextAttr;

            DestRect.Left := (CurrX - 01);
            DestRect.Top := (CurrY - 01);
            DestRect.Right := (CurrX - 01);
            DestRect.Bottom := (CurrY - 01);

            WriteConsoleOutput(OutHandle, @Cell, BufSize, WritePos, DestRect);

            Inc(CurrX);
          end; { else }
  end; { case }
  if CurrX > WinMaxX then
    begin
      CurrX := WinMinX;
      Inc(CurrY);
    end; { if }
  While CurrY > WinMaxY do
   begin
     RemoveLine(1);
     Dec(CurrY);
   end; { while }
end;


Function CrtWrite(var f : textrec):integer;
var
  i : longint;
begin
  GetScreenCursor(CurrX,CurrY);
  for i:=0 to f.bufpos-1 do
   WriteChar(f.buffer[i]);
  SetScreenCursor(CurrX,CurrY);
  f.bufpos:=0;
  CrtWrite:=0;
end;


Function CrtRead(Var F: TextRec): Integer;

  procedure BackSpace;
  begin
    if (f.bufpos>0) and (f.bufpos=f.bufend) then
     begin
       WriteChar(#8);
       WriteChar(' ');
       WriteChar(#8);
       dec(f.bufpos);
       dec(f.bufend);
     end;
  end;

var
  ch : Char;
Begin
  GetScreenCursor(CurrX,CurrY);
  f.bufpos:=0;
  f.bufend:=0;
  repeat
    if f.bufpos>f.bufend then
     f.bufend:=f.bufpos;
    SetScreenCursor(CurrX,CurrY);
    ch:=readkey;
    case ch of
    #0 : case readkey of
          #71 : while f.bufpos>0 do
                 begin
                   dec(f.bufpos);
                   WriteChar(#8);
                 end;
          #75 : if f.bufpos>0 then
                 begin
                   dec(f.bufpos);
                   WriteChar(#8);
                 end;
          #77 : if f.bufpos<f.bufend then
                 begin
                   WriteChar(f.bufptr^[f.bufpos]);
                   inc(f.bufpos);
                 end;
          #79 : while f.bufpos<f.bufend do
                 begin
                   WriteChar(f.bufptr^[f.bufpos]);
                   inc(f.bufpos);
                 end;
         end;
    ^S,
    #8 : BackSpace;
    ^Y,
   #27 : begin
           f.bufpos:=f.bufend;
           while f.bufend>0 do
            BackSpace;
         end;
   #13 : begin
           WriteChar(#13);
           WriteChar(#10);
           f.bufptr^[f.bufend]:=#13;
           f.bufptr^[f.bufend+1]:=#10;
           inc(f.bufend,2);
           break;
         end;
   #26 : if CheckEOF then
          begin
            f.bufptr^[f.bufend]:=#26;
            inc(f.bufend);
            break;
          end;
    else
     begin
       if f.bufpos<f.bufsize-2 then
        begin
          f.buffer[f.bufpos]:=ch;
          inc(f.bufpos);
          WriteChar(ch);
        end;
     end;
    end;
  until false;
  f.bufpos:=0;
  SetScreenCursor(CurrX,CurrY);
  CrtRead:=0;
End;


Function CrtReturn(Var F:TextRec):Integer;
Begin
  CrtReturn:=0;
end;


Function CrtClose(Var F: TextRec): Integer;
Begin
  F.Mode:=fmClosed;
  CrtClose:=0;
End;


Function CrtOpen(Var F: TextRec): Integer;
Begin
  If F.Mode=fmOutput Then
   begin
     TextRec(F).InOutFunc:=@CrtWrite;
     TextRec(F).FlushFunc:=@CrtWrite;
   end
  Else
   begin
     F.Mode:=fmInput;
     TextRec(F).InOutFunc:=@CrtRead;
     TextRec(F).FlushFunc:=@CrtReturn;
   end;
  TextRec(F).CloseFunc:=@CrtClose;
  CrtOpen:=0;
End;


procedure AssignCrt(var F: Text);
begin
  Assign(F,'');
  TextRec(F).OpenFunc:=@CrtOpen;
end;


const
  conout : pchar = 'CONOUT$';

var
  CursorInfo  : TConsoleCursorInfo;
  ConsoleInfo : TConsoleScreenBufferinfo;

begin
  { Initialize the output handles }
  OutHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  InputHandle := GetStdHandle(STD_INPUT_HANDLE);
  LastMode := 3;

  {--------------------- Get the cursor size and such -----------------------}
  FillChar(CursorInfo, SizeOf(CursorInfo), 00);
  GetConsoleCursorInfo(OutHandle, CursorInfo);
  SaveCursorSize := CursorInfo.dwSize;

  {------------------ Get the current cursor position and attr --------------}
  FillChar(ConsoleInfo, SizeOf(ConsoleInfo), 0);
  if not GetConsoleScreenBufferInfo(OutHandle, ConsoleInfo) then
    begin
      OutHandle:=CreateFile(ConOut, generic_read or generic_write,
        file_share_read or file_share_write,nil,
        open_existing,0,0);
      If (OutHandle=Invalid_handle_value) then
        begin
          Writeln(stderr,'No way to get the console handle');
          Halt(1);
        end;
      if not GetConsoleScreenBufferInfo(OutHandle, ConsoleInfo) then
        begin
          Writeln(stderr,'No way to get console screen buffer info');
          Halt(1);
        end;
    end;
  CursorSaveX := ConsoleInfo.dwCursorPosition.X;
  CursorSaveY := ConsoleInfo.dwCursorPosition.Y;
  TextAttr := ConsoleInfo.wAttributes;

  { Load startup values }
  ScreenWidth := GetScreenWidth;
  ScreenHeight := GetScreenHeight;
  IsWindowsNT := (GetPlatformID = VER_PLATFORM_WIN32_NT);

  { Not required, the dos crt does also not touch the mouse }
  {TurnMouseOff;}

  WinMinX:=1;
  WinMinY:=1;
  WinMaxX:=ScreenWidth;
  WinMaxY:=ScreenHeight;
  if ScreenHeight<=256 then
    WindMax := (ScreenWidth - 1) OR ((ScreenHeight - 1) SHL 8)
  else
    WindMax:=(ScreenWidth -1) or (255 shl 8);
  DoingNumChars := false;
  DoingNumCode := 0;

  { Redirect the standard output }
  AssignCrt(Output);
  Rewrite(Output);
  TextRec(Output).Handle:= OutHandle;

  AssignCrt(Input);
  Reset(Input);
  TextRec(Input).Handle:= InputHandle;
end. { unit Crt }

{
  $Log: crt.pp,v $
  Revision 1.1.2.16  2002/08/28 14:22:10  pierre
   * Use WaitForSingleObject in readkey function

  Revision 1.1.2.15  2002/07/16 14:08:22  pierre
   * better handling if more than 255 lines

  Revision 1.1.2.14  2002/03/01 22:28:58  pierre
   * fix bug 1838

  Revision 1.1.2.13  2002/01/19 11:46:51  peter
    * no turnoffmouse
    * fixed clrscr with small windows

  Revision 1.1.2.12  2001/11/08 23:29:44  pierre
   * fix bug 1675

  Revision 1.1.2.11  2001/09/09 18:29:53  carl
  * correct the tabs

  Revision 1.1.2.10  2001/09/09 14:47:09  carl
  * patch for Windows 98 bug with enhanced keys.

  Revision 1.1.2.9  2001/08/07 23:21:58  pierre
   * use 99 instead of 100 for dwSize in cursorbig

  Revision 1.1.2.8  2001/08/05 12:24:36  peter
    * fixed for new input_record

  Revision 1.1.2.7  2001/06/29 19:45:22  peter
    * fixed clreol

  Revision 1.1.2.6  2001/06/27 20:21:47  peter
    * support large screens

  Revision 1.1.2.5  2001/04/16 10:56:13  peter
    * fixes for stricter compiler

  Revision 1.1.2.4  2001/04/10 20:33:04  peter
    * remove some warnings

  Revision 1.1.2.3  2001/01/03 21:03:31  florian
    * fixed the repeat key bug introduced by my last bug fix

  Revision 1.1.2.2  2000/12/09 13:29:33  florian
    * fixed web bug 1228: keypressed ate too much keys

  Revision 1.1.2.1  2000/09/10 20:19:09  peter
    * fixed alt-<key>

  Revision 1.1  2000/07/13 06:31:19  michael
  + Initial import

  Revision 1.16  2000/06/11 07:04:58  peter
    * Windows unit has now more Delphi compatibile functions
    * placed the external functions only in the interface

  Revision 1.15  2000/04/14 12:14:39  pierre
   * try to get it to work if output is redirected

  Revision 1.14  2000/02/26 14:57:17  florian
    * writing at coloumn <screenwidth> wasn't possible in some cases,
      fixed

  Revision 1.13  2000/02/09 16:59:34  peter
    * truncated log

  Revision 1.12  1999/10/22 14:36:20  peter
    * crtreturn also needs f:textrec as parameter

  Revision 1.11  1999/08/28 09:30:39  peter
    * fixes from Maarten Bekers

  Revision 1.10  1999/08/24 13:15:44  peter
    * Removeline fixed
}
