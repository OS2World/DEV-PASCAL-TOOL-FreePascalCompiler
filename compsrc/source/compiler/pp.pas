 {
    $Id: pp.pas,v 1.1.2.9 2002/11/08 13:59:43 pierre Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    Commandline compiler for Free Pascal

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************}

{
  possible compiler switches (* marks a currently required switch):
  -----------------------------------------------------------------
  TP                  to compile the compiler with Turbo or Borland Pascal
  GDB*                support of the GNU Debugger
  I386                generate a compiler for the Intel i386+
  M68K                generate a compiler for the M68000
  SPARC               generate a compiler for SPARC
  POWERPC             generate a compiler for the PowerPC
  USEOVERLAY          compiles a TP version which uses overlays
  DEBUG               version with debug code is generated
  EXTDEBUG            some extra debug code is executed
  SUPPORT_MMX         only i386: releases the compiler switch
                      MMX which allows the compiler to generate
                      MMX instructions
  EXTERN_MSG          Don't compile the msgfiles in the compiler, always
                      use external messagefiles, default for TP
  NOAG386INT          no Intel Assembler output
  NOAG386NSM          no NASM output
  NOAG386BIN          leaves out the binary writer, default for TP
  NORA386DIR          No direct i386 assembler reader
  -----------------------------------------------------------------

  Required switches for a i386 compiler be compiled by Free Pascal Compiler:
  GDB;I386

  Required switches for a i386 compiler be compiled by Turbo Pascal:
  I386;TP;NOOPT;NOAG386BIN;NOAG386NSM;NOAG386INT;NORA386DIR

  Required switches for a 68000 compiler be compiled by Turbo Pascal:
  GDB;M68k;TP
}

{$ifdef FPC}
   {$ifndef GDB}
      { people can try to compile without GDB }
      { $error The compiler switch GDB must be defined}
   {$endif GDB}
   { but I386 or M68K must be defined }
   { and only one of the two }
   {$ifndef I386}
      {$ifndef M68K}
        {$fatal One of the switches I386 or M68K must be defined}
      {$endif M68K}
   {$endif I386}
   {$ifdef I386}
      {$ifdef M68K}
        {$fatal ONLY one of the switches I386 or M68K must be defined}
      {$endif M68K}
   {$endif I386}
   {$ifdef support_mmx}
     {$ifndef i386}
       {$fatal I386 switch must be on for MMX support}
     {$endif i386}
   {$endif support_mmx}
{$endif}

{$ifdef TP}
  {$IFNDEF DPMI}
    {$M 24000,0,655360}
  {$ELSE}
    {$M 65000}
  {$ENDIF DPMI}
  {$E+,N+,F+,S-,R-}
{$endif TP}


program pp;

{$IFDEF TP}
  {$UNDEF PROFILE}
  {$IFDEF DPMI}
    {$UNDEF USEOVERLAY}
  {$ENDIF}
  {$DEFINE NOAG386BIN}
{$ENDIF}
{$ifdef FPC}
  {$UNDEF USEOVERLAY}
{$ENDIF}

uses
{$ifdef useoverlay}
  {$ifopt o+}
    Overlay,ppovin,
  {$else}
    {$error You must compile with the $O+ switch}
  {$endif}
{$endif useoverlay}
{$ifdef profile}
  profile,
{$endif profile}
{$ifdef FPC}
{$ifdef heaptrc}
  heapbase,
  ppheap,
{$endif heaptrc}
{$ifdef EXTDEBUG}
  checkmem,
{$endif EXTDEBUG}
{$ifdef unix}
  catch,
{$endif}
{$ifdef go32v2}
  {$ifdef DEBUG}
    {$define NOCATCH}
  {$endif DEBUG}
  catch,
{$endif}
{$endif FPC}
  globals,compiler
{$ifdef logmemblocks}
{$ifdef fpc}
  ,memlog
{$endif fpc}
{$endif logmemblocks}
{  ,windows}
  ;


var
  oldexit : pointer;
{  l_before, l_after : large_integer;}
procedure myexit;{$ifndef FPC}far;{$endif}
begin
  exitproc:=oldexit;
{ Show Runtime error if there was an error }
  if (erroraddr<>nil) then
   begin

     case exitcode of
      100:
        begin
           erroraddr:=nil;
           writeln('Error while reading file');
        end;
      101:
        begin
           erroraddr:=nil;
           writeln('Error while writing file');
        end;
      202:
        begin
           erroraddr:=nil;
           writeln('Error: Stack Overflow');
        end;
      203:
        begin
           erroraddr:=nil;
           writeln('Error: Out of memory');
        end;
     end;
     { we cannot use aktfilepos.file because all memory might have been
       freed already !
       But we can use global parser_current_file var }
     Writeln('Compilation aborted ',parser_current_file,':',aktfilepos.line);
   end;
{   QueryPerformanceCounter(@l_after);}
{   WriteLn(l_after.Quadpart - l_before.Quadpart);}
end;

begin
{  QueryPerformanceCounter(@l_before);}
  oldexit:=exitproc;
  exitproc:=@myexit;
{$ifdef UseOverlay}
  InitOverlay;
{$endif}

{ Call the compiler with empty command, so it will take the parameters }
  Halt(compiler.Compile(''));
end.
{
  $Log: pp.pas,v $
  Revision 1.1.2.9  2002/11/08 13:59:43  pierre
   + CheckMem unit added, to get better memory leak infos with -dEXTDEBUG

  Revision 1.1.2.8  2002/11/08 09:05:37  pierre
   * heapbase added to avoid previous memory allocation on linux

  Revision 1.1.2.7  2002/10/18 21:55:39  carl
    * alignment of rtti information (m68k only)
    * bytes are also aligned when target = 68000
    + atari target

  Revision 1.1.2.6  2002/05/31 11:18:19  marco
   * Rename fest for 1.0.x step one. Compiler and RTL

  Revision 1.1.2.5  2001/03/04 02:28:19  carl
  +renamed units which removed some defines once again.

  Revision 1.1.2.4  2001/03/02 02:16:43  carl
  * corrected mistake in previous commit GDB does not work
   with BP

  Revision 1.1.2.3  2001/03/02 02:16:00  carl
  + added comments for compiling under BP

  Revision 1.1.2.2  2001/02/25 04:08:59  carl
  - removed some unused defines
  + added some used defines

  Revision 1.1.2.1  2000/09/27 22:35:02  pierre
   * suppress lineinfo explicit in _uses

  Revision 1.1  2000/07/13 06:29:54  michael
  + Initial import

  Revision 1.60  2000/04/02 15:22:19  florian
    * fixed bug 903: the compiler gives now a nice message if it can't create
      the .o file, (same for future .ar)

  Revision 1.59  2000/03/20 09:36:23  florian
    * using the directive DEBUG when compiling the compiler will include now
      the lineinfo unit on all targets

  Revision 1.58  2000/03/16 10:29:06  florian
    * disk full runerror writes now a nice message

  Revision 1.57  2000/03/14 16:30:14  pierre
   + lineinfo for win32 with debug

  Revision 1.56  2000/02/18 12:34:43  pierre
   DEBUG implies NOCATCH for go32v2

  Revision 1.55  2000/02/10 23:44:43  florian
    * big update for exception handling code generation: possible mem holes
      fixed, break/continue/exit should work always now as expected

  Revision 1.54  2000/02/09 13:22:59  peter
    * log truncated

  Revision 1.53  2000/01/07 01:14:30  peter
    * updated copyright to 2000

  Revision 1.52  1999/11/06 14:34:23  peter
    * truncated log to 20 revs

  Revision 1.51  1999/11/05 13:15:00  florian
    * some fixes to get the new cg compiling again

  Revision 1.50  1999/09/17 17:14:10  peter
    * @procvar fixes for tp mode
    * @<id>:= gives now an error

  Revision 1.49  1999/09/16 23:05:54  florian
    * m68k compiler is again compilable (only gas writer, no assembler reader)

  Revision 1.48  1999/09/10 18:48:08  florian
    * some bug fixes (e.g. must_be_valid and procinfo.funcret_is_valid)
    * most things for stored properties fixed

  Revision 1.47  1999/09/02 18:47:45  daniel
    * Could not compile with TP, some arrays moved to heap
    * NOAG386BIN default for TP
    * AG386* files were not compatible with TP, fixed.

  Revision 1.46  1999/08/28 15:34:20  florian
    * bug 519 fixed

  Revision 1.45  1999/08/04 00:23:18  florian
    * renamed i386asm and i386base to cpuasm and cpubase
}
