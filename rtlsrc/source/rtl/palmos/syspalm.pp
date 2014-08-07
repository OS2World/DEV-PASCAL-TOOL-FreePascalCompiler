{
    $Id: syspalm.pp,v 1.1.2.3 2003/01/10 15:43:29 pierre Exp $

    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$define PALMOS}

Unit SysPalm;

{$I os.inc}

  Interface

    Type
       { type and constant declartions doesn't hurt }
       LongInt  = $80000000..$7fffffff;
       Integer  = -32768..32767;
       ShortInt = -128..127;
       Byte     = 0..255;
       Word     = 0..65535;

       { !!!!
       DWord    = Cardinal;
       LongWord = Cardinal;
       }

       { The Cardinal data type isn't currently implemented for the m68k }
       DWord    = LongInt;
       LongWord = LongInt;

       { Zero - terminated strings }
       PChar    = ^Char;
       PPChar   = ^PChar;

       { procedure type }
       TProcedure = Procedure;

    const
       { max. values for longint and int }
       MaxLongint = High(LongInt);
       MaxInt = High(Integer);

       { Must be determined at startup for both }
       Test68000 : byte = 0;
       Test68881 : byte = 0;

{Platform specific information}
const
 LineEnding = #10;
 LFNSupport = false;
 DirectorySeparator = '/';
 DriveSeparator = ':';
 PathSeparator = ';';
 FileNameCaseSensitive = false;

    { Palm specific data types }
    type
       Ptr    = ^Char;

    var
       ExitCode : DWord;
       ExitProc : pointer;
       inoutres : word;
       { this variables are passed to PilotMain by the PalmOS }
       cmd : Word;
       cmdPBP : Ptr;
       launchFlags : Word;

  implementation

    { mimic the C start code }

    function PilotMain(_cmd : Word;_cmdPBP : Ptr;_launchFlags : Word) : DWord;cdecl;public;[alias : 'PilotMain'];

      begin
         asm
             trap #8
         end;
         cmd:=_cmd;
         cmdPBP:=_cmdPBP;
         launchFlags:=_launchFlags;
         asm
            bsr PASCALMAIN
         end;
         PilotMain:=ExitCode;
      end;


{*****************************************************************************
                        Initialization / Finalization
*****************************************************************************}

const
  maxunits=1024; { See also files.pas of the compiler source }
type
  TInitFinalRec=record
    InitProc,
    FinalProc : TProcedure;
  end;
  TInitFinalTable=record
    TableCount,
    InitCount  : longint;
    Procs      : array[1..maxunits] of TInitFinalRec;
  end;

var
  InitFinalTable : TInitFinalTable;external name 'INITFINAL';

procedure InitializeUnits;[public,alias:'FPC_INITIALIZEUNITS'];
var
  i : longint;
begin
(*
  with InitFinalTable do
   begin
     for i:=1 to TableCount do
      begin
        if assigned(Procs[i].InitProc) then
         Procs[i].InitProc();
        InitCount:=i;
      end;
   end;
*)
end;


procedure FinalizeUnits;[public,alias:'FPC_FINALIZEUNITS'];
begin
{
  with InitFinalTable do
   begin
     while (InitCount>0) do
      begin
        // we've to decrement the cound before calling the final. code
        // else a halt in the final. code leads to a endless loop
        dec(InitCount);
        if assigned(Procs[InitCount+1].FinalProc) then
         Procs[InitCount+1].FinalProc();
      end;
   end;
}
end;

{*****************************************************************************
                         System Dependent Exit code
*****************************************************************************}
Procedure system_exit;
begin
end;

Procedure InternalExit;
var
  current_exit : Procedure;
Begin
(*
  while exitProc<>nil Do
   Begin
     InOutRes:=0;
     current_exit:=tProcedure(exitProc);
     exitProc:=nil;
     current_exit();
   End;
  { Finalize units }
  FinalizeUnits;

  Should be a message box
  { Show runtime error and exit }
  If erroraddr<>nil Then
   Begin
     Writeln(stdout,'Runtime error ',Errorcode,' at 0x',hexstr(Longint(Erroraddr),8));
     { to get a nice symify }
     Writeln(stdout,BackTraceStrFunc(Longint(Erroraddr)));
     dump_stack(stdout,ErrorBase);
     Writeln(stdout,'');
   End;
*)
End;


Procedure do_exit;[Public,Alias:'FPC_DO_EXIT'];
begin
  InternalExit;
  System_exit;
end;

{
begin
   ExitProc:=nil;
   ExitCode:=0;
}
end.

{
  $Log: syspalm.pp,v $
  Revision 1.1.2.3  2003/01/10 15:43:29  pierre
   * small comment changes

  Revision 1.1.2.2  2001/11/07 15:22:36  michael
  + Added OS describing constants

  Revision 1.1.2.1  2001/08/16 19:13:57  florian
    + fixed a lot stuff to get it running again

  Revision 1.1  2000/07/13 06:31:10  michael
    + Initial import

  Revision 1.8  2000/01/07 16:41:51  daniel
    * copyright 2000

  Revision 1.7  2000/01/07 16:32:34  daniel
    * copyright 2000 added

  Revision 1.6  1999/09/17 10:00:40  florian
    * now using direct assembler mode

  Revision 1.5  1999/05/17 21:52:46  florian
    * most of the Object Pascal stuff moved to the system unit

  Revision 1.4  1999/01/18 10:05:56  pierre
   + system_exit procedure added

  Revision 1.3  1998/08/31 12:18:37  peter
    * export changed to public which is allowed in implementation

  Revision 1.2  1998/08/22 10:23:59  florian
    + PilotMain implemented

  Revision 1.1  1998/08/05 17:19:07  florian
    + first few things for PalmOS support
}
