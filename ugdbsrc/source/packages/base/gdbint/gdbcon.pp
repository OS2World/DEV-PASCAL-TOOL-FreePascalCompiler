{
    $Id: gdbcon.pp,v 1.5 2002/09/17 20:57:04 pierre Exp $
    Copyright (c) 1998 by Peter Vreman

    Lowlevel GDB interface which communicates directly with libgdb

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit GDBCon;
interface

uses
  GDBInt;

type
  PGDBController=^TGDBController;
  TGDBController=object(TGDBInterface)
    progname   : pchar;
    progargs   : pchar;
    in_command,
    init_count : longint;
    constructor Init;
    destructor  Done;
    procedure CommandBegin(const s:string);virtual;
    procedure Command(const s:string);
    procedure CommandEnd(const s:string);virtual;
    procedure Reset;virtual;
    { tracing }
    procedure StartTrace;
    procedure Run;virtual;
    procedure TraceStep;virtual;
    procedure TraceNext;virtual;
    procedure TraceStepI;virtual;
    procedure TraceNextI;virtual;
    procedure Continue;virtual;
    { needed for dos because newlines are only #10 (PM) }
    procedure WriteErrorBuf;
    procedure WriteOutputBuf;
    function  GetOutput : Pchar;
    function  GetError : Pchar;
    function  LoadFile(var fn:string):boolean;
    procedure SetArgs(const s : string);
    procedure ClearSymbols;
  end;

procedure UnixDir(var s : string);

implementation

uses
{$ifdef win32}
  windows,
{$endif win32}
  strings;

{$ifdef win32}
const
  CygDrivePrefixKey1 = 'Software';
  CygDrivePrefixKey2 = 'Cygnus Solutions';
  CygDrivePrefixKey3 = 'Cygwin';
  CygDrivePrefixKey4 = 'mounts v2';
  CygDrivePrefixKey = 'cygdrive prefix';

function CygDrivePrefix : string;
var
  i : longint;
  length : dword;
  Value : pchar;
  _type : dword;
  Key,NKey : HKey;
begin
  Length:=0;
  Key:=HKEY_CURRENT_USER;
  i := RegOpenKeyEx(Key, CygDrivePrefixKey1, 0, KEY_ENUMERATE_SUB_KEYS, @NKey);
  if i=ERROR_SUCCESS then
    begin
      Key:=NKey;
      i := RegOpenKeyEx(Key, CygDrivePrefixKey2, 0, KEY_ENUMERATE_SUB_KEYS, @NKey);
    end;
  if i=ERROR_SUCCESS then
    begin
      RegCloseKey(Key);
      Key:=NKey;
      i := RegOpenKeyEx(Key, CygDrivePrefixKey3, 0, KEY_ENUMERATE_SUB_KEYS, @NKey);
    end;
  if i=ERROR_SUCCESS then
    begin
      RegCloseKey(Key);
      Key:=NKey;
      i := RegOpenKeyEx(Key, CygDrivePrefixKey4, 0, KEY_ENUMERATE_SUB_KEYS, @NKey);
    end;
  if i=ERROR_SUCCESS then
    begin
      RegCloseKey(Key);
      Key:=NKey;
      i := RegQueryValueEx( Key, CygDrivePrefixKey, nil, @_type, nil, @length);
    end;
  if i<>ERROR_SUCCESS then
    CygDrivePrefix:='/cygdrive'
  else
    Begin
      GetMem(Value,Length);
      i := RegQueryValueEx( Key, CygDrivePrefixKey, nil, @_type, LPByte(Value), @length);
      if i<>ERROR_SUCCESS then
        CygDrivePrefix:='/cygdrive'
      else
        CygDrivePrefix:=StrPas(Value);
      FreeMem(Value,Length);
    End;
  if Key<>HKEY_CURRENT_USER then
    RegCloseKey(Key);
end;
{$endif win32}

procedure UnixDir(var s : string);
var i : longint;
begin
  for i:=1 to length(s) do
    if s[i]='\' then
{$ifdef win32}
  { Don't touch at '\ ' used to escapes spaces in windows file names PM }
     if (i=length(s)) or (s[i+1]<>' ') then
{$endif win32}
      s[i]:='/';
{$ifdef win32}
{ for win32 we should convert e:\ into //e/ PM }
  if (length(s)>2) and (s[2]=':') and (s[3]='/') then
    s:=CygDrivePrefix+'/'+s[1]+copy(s,3,length(s));
{$endif win32}
end;

constructor TGDBController.Init;
begin
  inherited init;
end;


destructor TGDBController.Done;
begin
  if assigned(progname) then
    strdispose(progname);
  if assigned(progargs) then
    strdispose(progargs);
  inherited done;
end;


procedure TGDBController.Command(const s:string);
begin
  inc(in_command);
  CommandBegin(s);
  gdboutputbuf.reset;
  gdberrorbuf.reset;
  gdb_command(s);
  {
    What is that for ?? PM
    I had to comment it because
    it resets the debuggere after each command !!
    Maybe it can happen on errors ??
  if in_command<0 then
   begin
     in_command:=0;
     inc(in_command);
     Reset;
     dec(in_command);
   end; }
  CommandEnd(s);
  dec(in_command);
end;

procedure TGDBController.CommandBegin(const s:string);
begin
end;

procedure TGDBController.CommandEnd(const s:string);
begin
end;

function TGDBController.LoadFile(var fn:string):boolean;
var
  cmd : string;
begin
  getdir(0,cmd);
  UnixDir(cmd);
  cmd:='cd '+cmd;
  Command(cmd);
  GDB__Init;
  UnixDir(fn);
  if assigned(progname) then
    strdispose(progname);
  getmem(progname,length(fn)+1);
  strpcopy(progname,fn);
  if fn<>'' then
    Command('file '+fn);
  LoadFile:=true;
end;

procedure TGDBController.SetArgs(const s : string);
begin
  if assigned(progargs) then
    strdispose(progargs);
  getmem(progargs,length(s)+1);
  strpcopy(progargs,s);
  command('set args '+s);
end;

procedure TGDBController.Reset;
begin
  call_reset:=false;
{ DeleteBreakPoints(); }
  if debuggee_started then
   begin
     reset_command:=true;
     BreakSession;
     Command('kill');
     reset_command:=false;
     debuggee_started:=false;
   end;
end;

procedure TGDBController.StartTrace;
begin
  Command('tbreak PASCALMAIN');
  Run;
end;

procedure TGDBController.Run;
begin
  Command('run');
  inc(init_count);
end;


procedure TGDBController.TraceStep;
begin
  Command('step');
end;


procedure TGDBController.TraceNext;
begin
  Command('next');
end;


procedure TGDBController.TraceStepI;
begin
  Command('stepi');
end;


procedure TGDBController.TraceNextI;
begin
  Command('nexti');
end;


procedure TGDBController.Continue;
begin
  Command('continue');
end;


procedure TGDBController.ClearSymbols;
begin
  if debuggee_started then
   Reset;
  if init_count>0 then
   Command('file');
end;


procedure BufWrite(Buf : pchar);
  var p,pe : pchar;
begin
  p:=buf;
  While assigned(p) do
    begin
       pe:=strscan(p,#10);
       if pe<>nil then
         pe^:=#0;
       Writeln(p);
       { restore for dispose }
       if pe<>nil then
         pe^:=#10;
       if pe=nil then
         p:=nil
       else
         begin
           p:=pe;
           inc(p);
         end;
    end;
end;


function  TGDBController.GetOutput : Pchar;
begin
  GetOutput:=gdboutputbuf.buf;
end;

function  TGDBController.GetError : Pchar;
var p : pchar;
begin
  p:=gdberrorbuf.buf;
  if (p^=#0) and got_error then
    GetError:=pchar(longint(gdboutputbuf.buf)+gdboutputbuf.idx)
  else
    GetError:=p;
end;

procedure TGDBController.WriteErrorBuf;
begin
   BufWrite(gdberrorbuf.buf);
end;


procedure TGDBController.WriteOutputBuf;
begin
   BufWrite(gdboutputbuf.buf);
end;


end.
{
  $Log: gdbcon.pp,v $
  Revision 1.5  2002/09/17 20:57:04  pierre
    * increment in_command before calling CommandBegin
      and after calling CommandEnd to be able to count
      command levels inside these methods.

  Revision 1.4  2002/09/07 15:42:51  peter
    * old logs removed and tabs fixed

  Revision 1.3  2002/04/03 09:09:22  pierre
   * fix UniwDir for win32 gdbcon.pp

  Revision 1.2  2002/03/26 16:23:14  pierre
   * get IDE to work with dirs containing spaces for win32

  Revision 1.1  2002/01/29 17:54:49  peter
    * splitted to base and extra

}
