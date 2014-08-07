{
    $Id: video.pp,v 1.2.2.20 2002/09/10 13:46:23 pierre Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    Video unit for linux

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
  Linux, Strings, TermInfo;

{$i video.inc}


Type TConsoleType = (ttyNetwork,ttyLinux,ttyFreeBSD,ttyNetBSD);

var
  LastCursorType : byte;
  TtyFd: Longint;
  Console: TConsoleType;
{$ifdef logging}
  f: file;

const
  logstart: string = '';
  nl: char = #10;
  logend: string = #10#10;
{$endif logging}

{$ifdef I386}
{$ASMMODE ATT}
{$endif I386}

const

  can_delete_term : boolean = false;
  ACSIn : string = '';
  ACSOut : string = '';
  InACS : boolean =false;

function IsACS(var ch,ACSchar : char): boolean;
begin
  IsACS:=false;
  case ch of
    #24, #30: {}
      ch:='^';
    #25, #31: {}
      ch:='v';
    #26, #16: {Never introduce a ctrl-Z ... }
      ch:='>';
    {#27,needed in Escape sequences} #17: {}
      ch:='<';
    #176, #177, #178: {°±²}
      begin
        IsACS:=true;
        ACSChar:='a';
      end;
    #180, #181, #182, #185: {´µ¶¹}
      begin
        IsACS:=true;
        ACSChar:='u';
      end;
    #183, #184, #187, #191: {·¸»¿}
      begin
        IsACS:=true;
        ACSChar:='k';
      end;
    #188, #189, #190, #217: {¼½¾Ù}
      begin
        IsACS:=true;
        ACSChar:='j';
      end;
    #192, #200, #211, #212: {ÀÈÓÔ}
      begin
        IsACS:=true;
        ACSChar:='m';
      end;
    #193, #202, #207, #208: {ÁÊÏÐ}
      begin
        IsACS:=true;
        ACSChar:='v';
      end;
    #194, #203, #209, #210: {ÂËÑÒ}
      begin
        IsACS:=true;
        ACSChar:='w';
      end;
    #195, #198, #199, #204: {ÃÆÇÌ}
      begin
        IsACS:=true;
        ACSChar:='t';
      end;
    #196, #205: {ÄÍ}
      begin
        IsACS:=true;
        ACSChar:='q';
      end;
    #179, #186: {³º}
      begin
        IsACS:=true;
        ACSChar:='x';
      end;
    #197, #206, #215, #216: {ÅÎ×Ø}
      begin
        IsACS:=true;
        ACSChar:='n';
      end;
    #201, #213, #214, #218: {ÉÕÖÚ}
      begin
        IsACS:=true;
        ACSChar:='l';
      end;
    #254: { þ }
      begin
        ch:='*';
      end;
    { Shadows for Buttons }
    #220: { Ü }
      begin
        IsACS:=true;
        ACSChar:='a';
      end;
    #223: { ß }
      begin
        IsACS:=true;
        ACSChar:='a';
      end;
  end;
end;


function SendEscapeSeqNdx(Ndx: Word) : boolean;
var
  P,pdelay: PChar;
begin
  SendEscapeSeqNdx:=false;
  if not assigned(cur_term_Strings) then
    exit{RunError(219)};
  P:=cur_term_Strings^[Ndx];
  if assigned(p) then
   begin { Do not transmit the delays }
     pdelay:=strpos(p,'$<');
     if assigned(pdelay) then
       pdelay^:=#0;
     fdWrite(stdoutputhandle, P^, StrLen(P));
     SendEscapeSeqNdx:=true;
     if assigned(pdelay) then
       pdelay^:='$';
   end;
end;


procedure SendEscapeSeq(const S: String);
begin
  fdWrite(stdoutputhandle, S[1], Length(S));
end;


Function IntStr(l:longint):string;
var
  s : string;
begin
  Str(l,s);
  IntStr:=s;
end;


Function XY2Ansi(x,y,ox,oy:longint):String;
{
  Returns a string with the escape sequences to go to X,Y on the screen
}
Begin
  if y=oy then
   begin
     if x=ox then
      begin
        XY2Ansi:='';
        exit;
      end;
     if x=1 then
      begin
        XY2Ansi:=#13;
        exit;
      end;
     if x>ox then
      begin
        XY2Ansi:=#27'['+IntStr(x-ox)+'C';
        exit;
      end
     else
      begin
        XY2Ansi:=#27'['+IntStr(ox-x)+'D';
        exit;
      end;
   end;
  if x=ox then
   begin
     if y>oy then
      begin
        XY2Ansi:=#27'['+IntStr(y-oy)+'B';
        exit;
      end
     else
      begin
        XY2Ansi:=#27'['+IntStr(oy-y)+'A';
        exit;
      end;
   end;
  if ((x=1) and (oy+1=y)) and (Console<>ttyFreeBSD) then
   XY2Ansi:=#13#10
  else
   XY2Ansi:=#27'['+IntStr(y)+';'+IntStr(x)+'H';
End;



const
  AnsiTbl : string[8]='04261537';
Function Attr2Ansi(Attr,OAttr:longint):string;
{
  Convert Attr to an Ansi String, the Optimal code is calculate
  with use of the old OAttr
}
var
  hstr : string[16];
  OFg,OBg,Fg,Bg : longint;

  procedure AddSep(ch:char);
  begin
    if length(hstr)>0 then
     hstr:=hstr+';';
    hstr:=hstr+ch;
  end;

begin
  if Attr=OAttr then
   begin
     Attr2Ansi:='';
     exit;
   end;
  Hstr:='';
  Fg:=Attr and $f;
  Bg:=Attr shr 4;
  OFg:=OAttr and $f;
  OBg:=OAttr shr 4;
  if (OFg<>7) or (Fg=7) or ((OFg>7) and (Fg<8)) or ((OBg>7) and (Bg<8)) then
   begin
     hstr:='0';
     OFg:=7;
     OBg:=0;
   end;
  if (Fg>7) and (OFg<8) then
   begin
     AddSep('1');
     OFg:=OFg or 8;
   end;
  if (Bg and 8)<>(OBg and 8) then
   begin
     AddSep('5');
     OBg:=OBg or 8;
   end;
  if (Fg<>OFg) then
   begin
     AddSep('3');
     hstr:=hstr+AnsiTbl[(Fg and 7)+1];
   end;
  if (Bg<>OBg) then
   begin
     AddSep('4');
     hstr:=hstr+AnsiTbl[(Bg and 7)+1];
   end;
  if hstr='0' then
   hstr:='';
  Attr2Ansi:=#27'['+hstr+'m';
end;

procedure UpdateTTY(Force:boolean);
type
  tchattr=packed record
{$ifdef ENDIAN_LITTLE}
    ch : char;
    attr : byte;
{$else}
    attr : byte;
    ch : char;
{$endif}
  end;
var
  outbuf   : array[0..1023+255] of char;
  chattr   : tchattr;
  skipped  : boolean;
  outptr,
  spaces,
  eol,
  x,y,
  LastX,LastY,
  SpaceAttr,
  LastAttr : longint;
  p,pold   : pvideocell;
  LastLineWidth : Longint;

procedure TransformUsingACS(var st : string);
var
  res : string;
  i : longint;
  ch,ACSch : char;
begin
  res:='';
  for i:=1 to length(st) do
    begin
      ch:=st[i];
      if IsACS(ch,ACSch) then
        begin
          if not InACS then
            begin
              res:=res+ACSIn;
              InACS:=true;
            end;
          res:=res+ACSch;
        end
      else
        begin
          if InACS then
            begin
              res:=res+ACSOut+Attr2Ansi(LastAttr,0);
              InACS:=false;
            end;
          res:=res+ch;
        end;
    end;
  st:=res;
end;


  procedure outdata(hstr:string);
  begin
   If Length(HStr)>0 Then
   Begin
    while (eol>0) do
     begin
       hstr:=#13#10+hstr;
       dec(eol);
     end;
    if NoExtendedFrame and (ACSIn<>'') and (ACSOut<>'') then
      TransformUsingACS(Hstr);
    move(hstr[1],outbuf[outptr],length(hstr));
    inc(outptr,length(hstr));
    if outptr>=1024 then
     begin
{$ifdef logging}
       blockwrite(f,logstart[1],length(logstart));
       blockwrite(f,nl,1);
       blockwrite(f,outptr,sizeof(outptr));
       blockwrite(f,nl,1);
       blockwrite(f,outbuf,outptr);
       blockwrite(f,nl,1);
{$endif logging}
       fdWrite(stdoutputhandle,outbuf,outptr);
       outptr:=0;
     end;
    end;
  end;

  procedure OutClr(c:byte);
  begin
    if c=LastAttr then
     exit;
    OutData(Attr2Ansi(c,LastAttr));
    LastAttr:=c;
  end;

  procedure OutSpaces;
  begin
    if (Spaces=0) then
     exit;
    OutClr(SpaceAttr);
    OutData(Space(Spaces));
    LastX:=x;
    LastY:=y;
    Spaces:=0;
  end;

  function GetTermString(ndx:word):String;
var
   P,pdelay: PChar;
begin
  GetTermString:='';
  if not assigned(cur_term_Strings) then
    exit{RunError(219)};
  P:=cur_term_Strings^[Ndx];
  if assigned(p) then
   begin { Do not transmit the delays }
     pdelay:=strpos(p,'$<');
     if assigned(pdelay) then
       pdelay^:=#0;
     GetTermString:=StrPas(p);
     if assigned(pdelay) then
       pdelay^:='$';
   end;
end;

begin
  OutPtr:=0;
  Eol:=0;
  skipped:=true;
  p:=PVideoCell(VideoBuf);
  pold:=PVideoCell(OldVideoBuf);
{ init Attr and X,Y }
  SendEscapeSeq(#27'[m'{#27'[H'});
  LastAttr:=7;
  LastX:=-1;
  LastY:=-1;
  for y:=1 to ScreenHeight do
   begin
     SpaceAttr:=0;
     Spaces:=0;
     LastLineWidth:=ScreenWidth;
     If (y=ScreenHeight) And (Console=ttyFreeBSD) {And :am: is on} Then
      LastLineWidth:=ScreenWidth-2;
     for x:=1 to LastLineWidth do
      begin
        if (not force) and (p^=pold^) then
         begin
           if (Spaces>0) then
            OutSpaces;
           skipped:=true;
         end
        else
         begin
           if skipped then
            begin
              OutData(XY2Ansi(x,y,LastX,LastY));
              LastX:=x;
              LastY:=y;
              skipped:=false;
            end;
           chattr:=tchattr(p^);
           if chattr.ch in [#0,#255] then
            chattr.ch:=' ';
           if chattr.ch=' ' then
            begin
              if Spaces=0 then
               SpaceAttr:=chattr.Attr;
              if (chattr.attr and $f0)=(spaceattr and $f0) then
               chattr.Attr:=SpaceAttr
              else
               begin
                 OutSpaces;
                 SpaceAttr:=chattr.Attr;
               end;
              inc(Spaces);
            end
           else
            begin
              if (Spaces>0) then
               OutSpaces;
              if ord(chattr.ch)<32 then
                begin
                  Chattr.Attr:= $ff xor Chattr.Attr;
                  ChAttr.ch:= chr(ord(chattr.ch)+ord('A')-1);
                end;
              if LastAttr<>chattr.Attr then
               OutClr(chattr.Attr);
              OutData(chattr.ch);
              LastX:=x+1;
              LastY:=y;
            end;
           p^:=tvideocell(chattr);
         end;
        inc(p);
        inc(pold);
      end;
     if (Spaces>0) then
      OutSpaces;
     if force then
      inc(eol)
     else
      skipped:=true;
   end;
  eol:=0;
  {if am in capabilities? Then}
  If (Console=ttyFreeBSD) and (Plongint(p)^<>plongint(pold)^) Then
   Begin
    OutData(XY2Ansi(ScreenWidth,ScreenHeight,LastX,LastY));
    OutData(#8);
    {Output last char}
    chattr:=tchattr(p[1]);
    if LastAttr<>chattr.Attr then
     OutClr(chattr.Attr);
    OutData(chattr.ch);
    inc(LastX);
//    OutData(XY2Ansi(ScreenWidth-1,ScreenHeight,LastX,LastY));
//   OutData(GetTermString(Insert_character));
    OutData(#8+#27+'[1@');

    chattr:=tchattr(p^);
    if LastAttr<>chattr.Attr then
     OutClr(chattr.Attr);
    OutData(chattr.ch);
    inc(LastX);
   end;
  OutData(XY2Ansi(CursorX,CursorY,LastX,LastY));
{$ifdef logging}
  blockwrite(f,logstart[1],length(logstart));
  blockwrite(f,nl,1);
  blockwrite(f,outptr,sizeof(outptr));
  blockwrite(f,nl,1);
  blockwrite(f,outbuf,outptr);
  blockwrite(f,nl,1);
{$endif logging}
  fdWrite(stdoutputhandle,outbuf,outptr);
  if InACS then
    SendEscapeSeqNdx(exit_alt_charset_mode);
end;

var
  InitialVideoTio, preInitVideoTio, postInitVideoTio: Linux.termios;
  inputRaw, outputRaw: boolean;

procedure saveRawSettings(const tio: Linux.termios);
Begin
  with tio do
   begin
     inputRaw :=
       ((c_iflag and (IGNBRK or BRKINT or PARMRK or ISTRIP or
                                INLCR or IGNCR or ICRNL or IXON)) = 0) and
       ((c_lflag and (ECHO or ECHONL or ICANON or ISIG or IEXTEN)) = 0);
     outPutRaw :=
       ((c_oflag and OPOST) = 0) and
       ((c_cflag and (CSIZE or PARENB)) = 0) and
       ((c_cflag and CS8) <> 0);
   end;
end;

procedure restoreRawSettings(tio: Linux.termios);
begin
  with tio do
    begin
      if inputRaw then
        begin
          c_iflag := c_iflag and cardinal(not (IGNBRK or BRKINT or PARMRK or ISTRIP or
            INLCR or IGNCR or ICRNL or IXON));
          c_lflag := c_lflag and
            cardinal(not (ECHO or ECHONL or ICANON or ISIG or IEXTEN));
       end;
     if outPutRaw then
       begin
         c_oflag := c_oflag and cardinal (not(OPOST));
         c_cflag := (c_cflag and cardinal(not(CSIZE or PARENB))) or CS8;
       end;
   end;
  TCSetAttr(1,TCSANOW,tio);
end;

procedure TargetEntry;
begin
  TCGetAttr(1,InitialVideoTio);
end;

procedure TargetExit;
begin
  TCSetAttr(1,TCSANOW,InitialVideoTio);
end;

procedure prepareInitVideo;
begin
  TCGetAttr(1,preInitVideoTio);
  saveRawSettings(preInitVideoTio);
end;

procedure videoInitDone;
begin
  TCGetAttr(1,postInitVideoTio);
  restoreRawSettings(postInitVideoTio);
end;

procedure prepareDoneVideo;
var
  tio: Linux.termios;
begin
  TCGetAttr(1,tio);
  saveRawSettings(tio);
  TCSetAttr(1,TCSANOW,postInitVideoTio);
end;

procedure doneVideoDone;
begin
  restoreRawSettings(preInitVideoTio);
end;

procedure SysInitVideo;
const
  fontstr : string[3]=#27'(K';
var
  ThisTTY: String[30];
  FName: String;
  WS: packed record
    ws_row, ws_col, ws_xpixel, ws_ypixel: Word;
  end;
  Err: Longint;
  prev_term : TerminalCommon_ptr1;
begin
{$ifndef CPUI386}
  LowAscii:=false;
{$endif CPUI386}
  { check for tty }

  ThisTTY:=TTYName(stdinputhandle);
  if IsATTY(stdinputhandle) then
   begin
     { save current terminal characteristics and remove rawness }
     prepareInitVideo;
     { write code to set a correct font }
     fdWrite(stdoutputhandle,fontstr[1],length(fontstr));
     { running on a tty, find out whether locally or remotely }
     TTYFd:=-1;
     Console:=TTyNetwork;  {Default: Network or other vtxxx tty}
     if (Copy(ThisTTY, 1, 8) = '/dev/tty') and (ThisTTY[9]<>'p') Then
      begin
  	Case ThisTTY[9] of
	 '0'..'9' : begin { running Linux on native console or native-emulation }
           	 FName:='/dev/vcsa' + ThisTTY[9];
           	 TTYFd:=fdOpen(FName, Octal(666), Open_RdWr); { open console }
		 IF TTYFd <>-1 Then
 		  Console:=ttyLinux;
  	        end;
	 'v'  :  { check for (Free?)BSD native}
		If (ThisTTY[10]>='0') and (ThisTTY[10]<='9') Then
 		 Console:=ttyFreeBSD;	{TTYFd ?}
	 end;
      end;
    If Copy(GetEnv('TERM'),1,6)='cons25' Then
     Console:=ttyFreeBSD;
     If Console<>ttylinux Then
      begin
        { running on a remote terminal, no error with /dev/vcsa }
        LowAscii:=false;
        //TTYFd:=stdoutputhandle;
      end;
     ioctl(stdinputhandle, TIOCGWINSZ, @WS);
     if WS.ws_Col=0 then
      WS.ws_Col:=80;
     if WS.ws_Row=0 then
      WS.ws_Row:=25;
     ScreenWidth:=WS.ws_Col;
     { TDrawBuffer only has FVMaxWidth elements
       larger values lead to crashes }
     if ScreenWidth> FVMaxWidth then
       ScreenWidth:=FVMaxWidth;
     ScreenHeight:=WS.ws_Row;
     CursorX:=1;
     CursorY:=1;
     LastCursorType:=$ff;
     ScreenColor:=True;
     { Start with a clear screen }
     if Console<>ttyLinux then
      begin
        prev_term:=cur_term;
        setupterm(nil, stdoutputhandle, err);
        can_delete_term:=assigned(prev_term) and (prev_term<>cur_term);
        SendEscapeSeqNdx(cursor_home);
        SendEscapeSeqNdx(cursor_normal);
        SendEscapeSeqNdx(cursor_visible);
        SendEscapeSeqNdx(enter_ca_mode);
        SetCursorType(crUnderLine);
	If Console=ttyFreeBSD Then
	 SendEscapeSeqNdx(exit_am_mode);
      end
     else if not assigned(cur_term) then
       begin
         setupterm(nil, stdoutputhandle, err);
         can_delete_term:=false;
       end;
     if assigned(cur_term_Strings) then
       begin
         ACSIn:=StrPas(cur_term_Strings^[enter_alt_charset_mode]);
         ACSOut:=StrPas(cur_term_Strings^[exit_alt_charset_mode]);
         if (ACSIn<>'') and (ACSOut<>'') then
           SendEscapeSeqNdx(ena_acs);
         if pos('$<',ACSIn)>0 then
           ACSIn:=Copy(ACSIn,1,Pos('$<',ACSIn)-1);
         if pos('$<',ACSOut)>0 then
           ACSOut:=Copy(ACSOut,1,Pos('$<',ACSOut)-1);
       end
     else
       begin
         ACSIn:='';
         ACSOut:='';
       end;
{$ifdef logging}
     assign(f,'video.log');
     rewrite(f,1);
{$endif logging}
     { save new terminal characteristics and possible restore rawness }
     videoInitDone;
   end
  else
   ErrorCode:=errVioInit; { not a TTY }
end;

procedure SysDoneVideo;
begin
  prepareDoneVideo;
  if Console=ttyLinux then
   SetCursorPos(1,1)
  else
   begin
     SendEscapeSeqNdx(exit_ca_mode);
     SendEscapeSeqNdx(cursor_home);
     SendEscapeSeqNdx(cursor_normal);
     SendEscapeSeqNdx(cursor_visible);
     SetCursorType(crUnderLine);
     SendEscapeSeq(#27'[H');
   end;
  ACSIn:='';
  ACSOut:='';
  doneVideoDone;
  {FreeBSD gives an error here.
   According to Pierre this could be more a NCurses version thing that
   a FreeBSD one. FreeBSD 4.4 has ncurses 5}
  {$ifndef BSD}
  if can_delete_term then
    begin
      del_curterm(cur_term);
      can_delete_term:=false;
    end;
  {$endif}
{$ifdef logging}
  close(f);
{$endif logging}
 {$ifdef BSD}
      SendEscapeSeq(#27'[m');
 {$endif}
end;


procedure SysClearScreen;
begin
  if Console=ttylinux then
    UpdateScreen(true)
  else
    begin
    SendEscapeSeq(#27'[0m');
    SendEscapeSeqNdx(clear_screen);
    end;
end;


procedure SysUpdateScreen(Force: Boolean);
var
  DoUpdate : boolean;
  i : longint;
  p1,p2 : plongint;
begin
  if not force then
   begin
{$ifdef i386}
     asm
          movl    VideoBuf,%esi
          movl    OldVideoBuf,%edi
          movl    VideoBufSize,%ecx
          shrl    $2,%ecx
          repe
          cmpsl
          setne   DoUpdate
     end;
{$else not i386}
{$ifdef m68k}
     { Note this code uses a word counter,
       thus the maximum VideoBufferSize is
       $ffff*2 i.e. approx. 128000 cells, but I  don't think
       that this is a real limitation PM }
     asm
          move.l    VideoBuf,a0
          move.l    OldVideoBuf,a1
          move.l    VideoBufSize,d0
          lsr.l     #2,d0
        @LCmpLoop:
          cmpm.l    (a0)+,(a1)+
          dbne.w    d0,@LCmpLoop
          sne.b     d0
          neg.b     d0
          move.b    d0,DoUpdate
     end;
{$else not m68k}
     p1:=plongint(VideoBuf);
     p2:=plongint(OldVideoBuf);
     for i:=0 to VideoBufSize div 2 do
       if (p1^<>p2^) then
         begin
           DoUpdate:=true;
           break;
         end
       else
         begin
           { Inc does add sizeof(longint) to both pointer values }
           inc(p1);
           inc(p2);
         end;
{$endif not m68k}
{$endif not i386}
   end
  else
   DoUpdate:=true;
  if not DoUpdate then
   exit;
  if Console=ttylinux then
   begin
     fdSeek(TTYFd, 4, Seek_Set);
     fdWrite(TTYFd, VideoBuf^,VideoBufSize);
   end
  else
   begin
     UpdateTTY(force);
   end;
  Move(VideoBuf^, OldVideoBuf^, VideoBufSize);
end;


function SysGetCapabilities: Word;
begin
{ about cpColor... we should check the terminfo database... }
  SysGetCapabilities:=cpUnderLine + cpBlink + cpColor;
end;


procedure SysSetCursorPos(NewCursorX, NewCursorY: Word);
var
  Pos : array [1..2] of Byte;
begin
  if (CursorX=NewCursorX+1) and (CursorY=NewCursorY+1) then
    exit;
  if Console=ttylinux then
   begin
     fdSeek(TTYFd, 2, Seek_Set);
     Pos[1]:=NewCursorX;
     Pos[2]:=NewCursorY;
     fdWrite(TTYFd, Pos, 2);
   end
  else
   begin
     { newcursorx,y is 0 based ! }
     SendEscapeSeq(XY2Ansi(NewCursorX+1,NewCursorY+1,0,0));
   end;
  CursorX:=NewCursorX+1;
  CursorY:=NewCursorY+1;
end;


function SysGetCursorType: Word;
begin
  SysGetCursorType:=LastCursorType;
end;


procedure SysSetCursorType(NewType: Word);
begin
  if LastCursorType=NewType then
    exit;
  LastCursorType:=NewType;
  case NewType of
   crBlock :
     Begin
       If not SendEscapeSeqNdx(cursor_visible) then
	If Console<>ttyFreeBSD Then
         SendEscapeSeq(#27'[?17;0;64c');
     End;
   crHidden :
     Begin
       If not SendEscapeSeqNdx(cursor_invisible) then
	If Console<>ttyFreeBSD Then
         SendEscapeSeq(#27'[?1c');
     End;
  else
    begin
      If not SendEscapeSeqNdx(cursor_normal) then
	If Console<>ttyFreeBSD Then
        SendEscapeSeq(#27'[?2c');
    end;
  end;
end;

Const
  SysVideoDriver : TVideoDriver = (
    InitDriver : @SysInitVideo;
    DoneDriver : @SysDoneVideo;
    UpdateScreen : @SysUpdateScreen;
    ClearScreen : @SysClearScreen;
    SetVideoMode : Nil;
    GetVideoModeCount : Nil;
    GetVideoModeData : Nil;
    SetCursorPos : @SysSetCursorPos;
    GetCursorType : @SysGetCursorType;
    SetCursorType : @SysSetCursorType;
    GetCapabilities : @SysGetCapabilities;
  );

initialization
  SetVideoDriver(SysVideoDriver);
end.
{
  $Log: video.pp,v $
  Revision 1.2.2.20  2002/09/10 13:46:23  pierre
   * make sure LastCursorType is set in InitVideo

  Revision 1.2.2.19  2002/09/10 13:34:02  pierre
   * never write escape sequences to vcsa buffers

  Revision 1.2.2.18  2002/09/10 09:17:09  pierre
    * fix a bug preventing correct recognition of linux tty
    * added several explicit typecast to remove warnings

  Revision 1.2.2.17  2002/07/16 14:02:46  pierre
   + use assembly code for m6_k

  Revision 1.2.2.16  2002/07/08 15:05:55  marco
   * Fixed problem with last char on cons25 screens. ttyFreeBSD is now set
  	also if copy(S,1,6)='cons25'. Should be wscons for netbsd?
     Fix is a hack, needs to be cleaned up still.

  Revision 1.2.2.15  2002/07/06 16:43:10  marco
   * Forgot one part of the merge of last fix. Fixed.

  Revision 1.2.2.14  2002/07/06 16:08:21  marco
   * Patch from Pierre to fix problem with corrupted attributes after ACS
  	change (TransformUsingACS). (during Strassbourg 2002 FPC meeting)

  Revision 1.2.2.13  2002/01/07 09:30:57  marco
   * Some things changed from console=ttyfreebsd to ifdef BSD, since the
      over network it also has a problem with it.

  Revision 1.2.2.12  2002/01/07 08:11:26  marco
   * More small FreeBSD console fixes that shouldn't change anything else

  Revision 1.2.2.11  2002/01/04 21:58:46  marco
   * 1st stage FreeBSD fixes.

  Revision 1.2.2.10  2001/10/13 12:58:48  michael
  + Removed defaultmode field from driver

  Revision 1.2.2.9  2001/10/06 22:23:41  michael
  + Better video mode selection/setting system

  Revision 1.2.2.8  2001/09/21 18:42:09  michael
  + Implemented support for custom video drivers.

  Revision 1.2.2.7  2001/08/28 12:23:15  pierre
   * set skipped to true if changing line and force is false to avoid problems if terminal reports less columns as available

  Revision 1.2.2.6  2001/08/01 10:50:59  pierre
   * avoid warning for m68k cpu

  Revision 1.2.2.5  2001/07/30 23:34:51  pierre
   * make tchattr record endian dependant

  Revision 1.2.2.4  2001/07/29 20:25:18  pierre
   * fix wrong deref in generic compare code

  Revision 1.2.2.3  2001/07/13 14:49:08  pierre
   + implement videobuf comparaison for non i386 cpus

  Revision 1.2.2.2  2001/01/30 22:23:44  peter
    * unix back to linux

  Revision 1.2.2.1  2001/01/30 21:52:02  peter
    * moved api utils to rtl

  Revision 1.2  2001/01/21 20:21:41  marco
   * Rename fest II. Rtl OK

  Revision 1.1  2001/01/13 11:03:58  peter
    * API 2 RTL commit

}

