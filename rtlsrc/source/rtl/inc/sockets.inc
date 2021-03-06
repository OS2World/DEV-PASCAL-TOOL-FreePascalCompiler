{
    $Id: sockets.inc,v 1.1 2000/07/13 06:30:48 michael Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{******************************************************************************
                     Text File Writeln/ReadLn Support
******************************************************************************}


Procedure OpenSock(var F:Text);
begin
  if textrec(f).handle=UnusedHandle then
   textrec(f).mode:=fmclosed
  else
   case textrec(f).userdata[1] of
     S_OUT : textrec(f).mode:=fmoutput;
     S_IN : textrec(f).mode:=fminput;
   else
    textrec(f).mode:=fmclosed;
   end;
end;



Procedure IOSock(var F:text);
begin
  case textrec(f).mode of
   fmoutput : fdWrite(textrec(f).handle,textrec(f).bufptr^,textrec(f).bufpos);
    fminput : textrec(f).BufEnd:=fdRead(textrec(f).handle,textrec(f).bufptr^,textrec(f).bufsize);
  end;
  textrec(f).bufpos:=0;
end;



Procedure FlushSock(var F:Text);
begin
  if (textrec(f).mode=fmoutput) and (textrec(f).bufpos<>0) then
   begin
     IOSock(f);
     textrec(f).bufpos:=0;
   end;
end;



Procedure CloseSock(var F:text);
begin
  { Nothing special has to be done here }
end;



Procedure Sock2Text(Sock:Longint;Var SockIn,SockOut:Text);
{
 Set up two Pascal Text file descriptors for reading and writing)
}
begin
{ First the reading part.}
  Assign(SockIn,'.');
  Textrec(SockIn).Handle:=Sock;
  Textrec(Sockin).userdata[1]:=S_IN;
  TextRec(SockIn).OpenFunc:=@OpenSock;
  TextRec(SockIn).InOutFunc:=@IOSock;
  TextRec(SockIn).FlushFunc:=@FlushSock;
  TextRec(SockIn).CloseFunc:=@CloseSock;
{ Now the writing part. }
  Assign(SockOut,'.');
  Textrec(SockOut).Handle:=Sock;
  Textrec(SockOut).userdata[1]:=S_OUT;
  TextRec(SockOut).OpenFunc:=@OpenSock;
  TextRec(SockOut).InOutFunc:=@IOSock;
  TextRec(SockOut).FlushFunc:=@FlushSock;
  TextRec(SockOut).CloseFunc:=@CloseSock;
end;


{******************************************************************************
                                Untyped File
******************************************************************************}

Procedure Sock2File(Sock:Longint;Var SockIn,SockOut:File);
begin
{Input}
  Assign(SockIn,'.');
  FileRec(SockIn).Handle:=Sock;
  FileRec(SockIn).RecSize:=1;
  FileRec(Sockin).userdata[1]:=S_IN;
{Output}
  Assign(SockOut,'.');
  FileRec(SockOut).Handle:=Sock;
  FileRec(SockOut).RecSize:=1;
  FileRec(SockOut).userdata[1]:=S_OUT;
end;

{******************************************************************************
                               InetSock
******************************************************************************}

Function DoAccept(Sock:longint;Var addr:TInetSockAddr):longint;

Var AddrLen : Longint;

begin
  AddrLEn:=SizeOf(Addr);
  DoAccept:=Accept(Sock,Addr,AddrLen);
end;

Function DoConnect(Sock:longint;const addr: TInetSockAddr): Boolean;

begin
  DoConnect:=Connect(Sock,Addr,SizeOF(TInetSockAddr));
end;

Function Connect(Sock:longint;const addr: TInetSockAddr;var SockIn,SockOut:text):Boolean;

begin
  Connect:=DoConnect(Sock,addr);
  If Connect then
     Sock2Text(Sock,SockIn,SockOut);
end;

Function Connect(Sock:longint;const addr:TInetSockAddr;var SockIn,SockOut:file):Boolean;

begin
  Connect:=DoConnect(Sock,addr);
  If Connect then
     Sock2File(Sock,SockIn,SockOut);
end;

Function Accept(Sock:longint;var addr:TInetSockAddr;var SockIn,SockOut:text):Boolean;
var
  s : longint;
begin
  S:=DoAccept(Sock,addr);
  if S>0 then
   begin
     Sock2Text(S,SockIn,SockOut);
     Accept:=true;
   end
  else
   Accept:=false;
end;

Function Accept(Sock:longint;var addr:TInetSockAddr;var SockIn,SockOut:File):Boolean;
var
  s : longint;
begin
  S:=DoAccept(Sock,addr);
  if S>0 then
   begin
     Sock2File(S,SockIn,SockOut);
     Accept:=true;
   end
  else
   Accept:=false;
end;
{
  $Log: sockets.inc,v $
  Revision 1.1  2000/07/13 06:30:48  michael
  + Initial import

  Revision 1.7  2000/06/21 22:27:50  pierre
   * no bufpos reset on flush for fminput

  Revision 1.6  2000/05/22 12:38:51  jonas
    * CloseSock is now a dummy (it called system.close, but this doesn't
      make any sense since itself can only be called by system.close) This
      caused an endless loop with my last changes where the mode of a
      textrec is set to fmClosed only after the closeFunc() has been called.

  Revision 1.5  2000/02/09 16:59:31  peter
    * truncated log

  Revision 1.4  2000/01/07 16:41:36  daniel
    * copyright 2000

  Revision 1.3  2000/01/07 16:32:25  daniel
    * copyright 2000 added

}