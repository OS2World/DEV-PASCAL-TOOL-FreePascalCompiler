{
    $Id: sysatari.pas,v 1.1.2.7 2002/10/19 14:47:47 carl Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Carl Eric Codere
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit sysatari;

{--------------------------------------------------------------------}
{ LEFT TO DO:                                                        }
{--------------------------------------------------------------------}
{ o Implement truncate                                               }
{ o Implement paramstr(0)                                            }
{--------------------------------------------------------------------}

  interface

    { used for single computations }
    const BIAS4 = $7f-1;

    {$I systemh.inc}
var
  argc : longint;
  argv : ppchar;
  envp : ppchar;
    
  { command line information passed from startup }
  arglength : byte;
  args : pchar;
  _env : pchar;

    {$I heaph.inc}

{Platform specific information}
const
 LineEnding = #13#10;
 LFNSupport = false;
 DirectorySeparator = '\';
 DriveSeparator = ':';
 PathSeparator = ';';
 FileNameCaseSensitive = false;

 sLineBreak: string [2] = LineEnding;


const
  UnusedHandle    = -1;
  StdInputHandle  = 0;
  StdOutputHandle = 1;
  StdErrorHandle  : longint = 1;




  implementation
  


    {$I system.inc}

var
  _HEAP : longint;external name 'HEAP';
  _HEAPSIZE : longint;external name 'HEAPSIZE';



    var
      errno : smallint;

{$S-}
    procedure Stack_Check; assembler;
    { Check for local variable allocation }
    { On Entry -> d0 : size of local stack we are trying to allocate }
         asm
          XDEF STACKCHECK
           move.l  sp,d1            { get value of stack pointer            }
           sub.l   d0,d1            {  sp - stack_size                      }
           sub.l   #2048,d1
           cmp.l   __BREAK,d1
           bgt     @st1nosweat
           move.l  #202,-(sp)
           jsr     HandleError      { stack overflow    }
         @st1nosweat:
         end;


    Procedure Error2InOut;
    Begin
     if (errno <= -2) and (errno >= -11) then
       InOutRes:=150-errno  { 150+errno }
     else
      Begin
        case errno of
          -32 : InOutRes:=1;
          -33 : InOutRes:=2;
          -34 : InOutRes:=3;
          -35 : InOutRes:=4;
          -36 : InOutRes:=5;
          -37 : InOutRes:=8;
          -39 : InOutRes:=8;
          -40 : InOutRes:=9;
          -46 : InOutRes:=15;
          -67..-64 : InOutRes:=153;
          -15 : InOutRes:=151;
          -13 : InOutRes:=150;
        else
           InOutres := word(errno);
         end;
     end;
     errno:=0;
    end;


    function paramstr(l : longint) : string;
      var
       s1 : string;
      begin
         if l = 0 then
         Begin
           s1 := '';
         end
         else
         if (l>0) and (l<=paramcount) then
           begin
             paramstr:=strpas(argv[l]);
           end
         else paramstr:='';
      end;

      function paramcount : longint;
      Begin
        paramcount := argc - 1;
      end;




    procedure randomize;

      var
         hl : longint;

      begin
         asm
           movem.l d2/d3/a2/a3, -(sp)     { save OS registers }
           move.w #17,-(sp)
           trap   #14         { call xbios - random number }
           add.l  #2,sp
           movem.l (sp)+,d2/d3/a2/a3
           move.l d0,hl       { result in d0 }
         end;
         randseed:=hl;
      end;

  { This routine is used to grow the heap.  }
  { But here we do a trick, we say that the }
  { heap cannot be regrown!                 }
  function sbrk( size: longint): longint;
  var memblock : pointer;
  Begin
   if size = 0 then
     begin
       sbrk := -1;
       exit;
     end;
   asm
     movem.l d2/d3/a2/a3, -(sp)     { save OS registers }
     move.l size,-(sp)
     move.w #$48,-(sp)
     trap   #1
     addq.l #6,sp
     movem.l (sp)+,d2/d3/a2/a3
     move.l d0,memblock
   end;
   if memblock = nil then
      sbrk:=-1
   else
      sbrk := longint(memblock);
  end;
  
  function getheapstart:pointer;
    begin
      getheapstart := @_HEAP;
    end;


  function getheapsize:longint;
    begin
      getheapsize := _HEAPSIZE;
    end;
  

{$I heap.inc}


{****************************************************************************
                          Low Level File Routines
 ****************************************************************************}

procedure AllowSlash(p:pchar);
var
  i : longint;
begin
{ allow slash as backslash }
  for i:=0 to strlen(p) do
   if p[i]='/' then p[i]:='\';
end;


procedure do_close(h : longint);
begin
  asm
        movem.l d2/d3/a2/a3,-(sp)
        move.l  h,d0
        move.w  d0,-(sp)
        move.w  #$3e,-(sp)
        trap    #1
        add.l   #4,sp      { restore stack ... }
        movem.l (sp)+,d2/d3/a2/a3
        tst.l   d0
        beq     @closeok
        move.w  d0,errno
    @closeok:
  end;
  if errno <> 0 then
     Error2InOut;
end;


procedure do_erase(p : pchar);
begin
  AllowSlash(p);
  asm
        movem.l d2/d3/a2/a3,-(sp)   { save regs }
        move.l  p,-(sp)
        move.w #$41,-(sp)
        trap   #1
        add.l  #6,sp
        move.l d6,d2       { restore d2 }
        movem.l (sp)+,d2/d3/a2/a3
        tst.w  d0
        beq    @doserend
        move.w d0,errno
        @doserend:
  end;
  if errno <> 0 then
     Error2InOut;
end;


procedure do_rename(p1,p2 : pchar);
begin
  AllowSlash(p1);
  AllowSlash(p2);
  asm
            movem.l d2/d3/a2/a3,-(sp)
            move.l  p2,-(sp)
            move.l  p1,-(sp)
            move.w  #0,-(sp)
            move.w  #$56,-(sp)
            trap    #1
            lea     12(sp),sp
            movem.l (sp)+,d2/d3/a2/a3
            tst.l   d0
            beq     @dosreend
            move.w  d0,errno    { error ... }
         @dosreend:
  end;
  WriteLn(errno);
  if errno <> 0 then
     Error2InOut;
end;

function do_isdevice(handle:longint):boolean;
begin
  if (handle=stdoutputhandle) or (handle=stdinputhandle) or
  (handle=stderrorhandle) then
    do_isdevice:=FALSE
  else
    do_isdevice:=TRUE;
end;


function do_write(h,addr,len : longint) : longint;
begin
  asm
            movem.l d2/d3/a2/a3,-(sp)
            move.l  addr,-(sp)
            move.l  len,-(sp)
            move.l  h,d0
            move.w  d0,-(sp)
            move.w  #$40,-(sp)
            trap    #1
            lea     12(sp),sp
            movem.l (sp)+,d2/d3/a2/a3
            tst.l   d0
            bpl     @doswrend
            move.w  d0,errno    { error ... }
          @doswrend:
            move.l  d0,@RESULT
  end;
  if errno <> 0 then
     Error2InOut;
end;


function do_read(h,addr,len : longint) : longint;
begin
  asm
            move.l h,d0
            movem.l d2/d3/a2/a3,-(sp)
            move.l  addr,-(sp)
            move.l len,-(sp)
            move.w d0,-(sp)
            move.w #$3f,-(sp)
            trap   #1
            lea    12(sp),sp
            movem.l (sp)+,d2/d3/a2/a3
            tst.l   d0
            bpl     @dosrdend
            move.w  d0,errno    { error ... }
          @dosrdend:
            move.l  d0,@Result
  end;
  if errno <> 0 then
     Error2InOut;
end;


function do_filepos(handle : longint) : longint;
begin
  asm
            movem.l d2/d3/a2/a3,-(sp)
            move.w #1,-(sp)     { seek from current position }
            move.l handle,d0
            move.w d0,-(sp)
            move.l #0,-(sp)     { with a seek offset of zero }
            move.w #$42,-(sp)
            trap   #1
            lea    10(sp),sp
            movem.l (sp)+,d2/d3/a2/a3
            move.l d0,@Result
  end;
end;


procedure do_seek(handle,pos : longint);
begin
  asm
            movem.l d2/d3/a2/a3,-(sp)
            move.w #0,-(sp)     { seek from start of file    }
            move.l handle,d0
            move.w d0,-(sp)
            move.l pos,-(sp)
            move.w #$42,-(sp)
            trap   #1
            lea    10(sp),sp
            movem.l (sp)+,d2/d3/a2/a3
            tst.l   d0
            bpl     @dosskend
            move.w  d0,errno    { error ... }
          @dosskend:
  end;
  if errno <> 0 then
     Error2InOut;
end;


function do_seekend(handle:longint):longint;
var
 t: longint;
begin
  asm
            movem.l d2/d3/a2/a3,-(sp)
            move.w #2,-(sp)     { seek from end of file        }
            move.l handle,d0
            move.w d0,-(sp)
            move.l #0,-(sp)     { with an offset of 0 from end }
            move.w #$42,-(sp)
            trap   #1
            lea    10(sp),sp
            movem.l (sp)+,d2/d3/a2/a3
            tst.l   d0
            bpl     @dosskend
            move.w  d0,errno    { error ... }
          @dosskend:
            move.l d0,t
  end;
  do_seekend:=t;
  if errno <> 0 then
     Error2InOut;
end;


function do_filesize(handle : longint) : longint;
var
   aktfilepos : longint;
begin
   aktfilepos:=do_filepos(handle);
   do_filesize:=do_seekend(handle);
   do_seek(handle,aktfilepos);
end;


procedure do_truncate (handle,pos:longint);
begin
  do_seek(handle,pos);
  {!!!!!!!!!!!!}
end;


procedure do_open(var f;p:pchar;flags:longint);
{
  filerec and textrec have both handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $100)   the file will be append
  when (flags and $1000)  the file will be truncate/rewritten
  when (flags and $10000) there is no check for close (needed for textfiles)
}
var
  i : word;
  oflags: longint;
begin
  AllowSlash(p);
 { close first if opened }
  if ((flags and $10000)=0) then
   begin
     case filerec(f).mode of
      fminput,fmoutput,fminout : Do_Close(filerec(f).handle);
      fmclosed : ;
     else
      begin
        inoutres:=102; {not assigned}
        exit;
      end;
     end;
   end;
{ reset file handle }
  filerec(f).handle:=UnusedHandle;
  oflags:=$02; { read/write mode }
{ convert filemode to filerec modes }
  case (flags and 3) of
   0 : begin
         filerec(f).mode:=fminput;
         oflags:=$00; { read mode only }
       end;
   1 : filerec(f).mode:=fmoutput;
   2 : filerec(f).mode:=fminout;
  end;
  { if the file is created, then 
    open it in read/write mode. 
  }  
  if (flags and $1000)<>0 then
     oflags:=$02;   { read/write with create }
{ empty name is special }
  if p[0]=#0 then
   begin
     case filerec(f).mode of
       fminput : filerec(f).handle:=StdInputHandle;
      fmappend,
      fmoutput : begin
                   filerec(f).handle:=StdOutputHandle;
                   filerec(f).mode:=fmoutput; {fool fmappend}
                 end;
     end;
     exit;
   end;
  { if the file must first be truncated / created
    we must call first the routine to create the 
    file, close this file, and re-open it. 
    (Reading from a created file fails on tos 2.06)
  }
  if (flags and $1000)<>0 then
   begin
    asm
      movem.l d2/d3/a2/a3,-(sp)    { save used registers }
      { rewrite mode - create new file }
      move.w  #0,-(sp)
      move.l  p,-(sp)
      move.w  #$3c,-(sp)
      trap    #1
      add.l   #8,sp       { restore stack of os call }
      movem.l (sp)+,d2/d3/a2/a3
      tst.w   d0
      bpl     @opennoerr  { if positive return values then ok }
      cmp.w   #-1,d0      { if handle is -1 CON:              }
      beq     @opennoerr
      cmp.w   #-2,d0      { if handle is -2 AUX:              }
      beq     @opennoerr
      cmp.w   #-3,d0      { if handle is -3 PRN:              }
      beq     @opennoerr
      move.w  d0,errno    { otherwise normal error            }
    @opennoerr:
      move.w  d0,i        { get handle as SIGNED VALUE...     }
    end;
    { now close the file, to reopen it after }
    do_close(i);
   end; 
   asm
      movem.l d2/d3/a2/a3,-(sp)    { save used registers }
      { reset - open existing files     }
      move.l  oflags,d0    { use flag as source  ...    }
      move.w  d0,-(sp)
      move.l  p,-(sp)
      move.w  #$3d,-(sp)
      trap    #1
      add.l   #8,sp       { restore stack of os call }
      movem.l (sp)+,d2/d3/a2/a3

      tst.w   d0
      bpl     @opennoerr  { if positive return values then ok }
      cmp.w   #-1,d0      { if handle is -1 CON:              }
      beq     @opennoerr
      cmp.w   #-2,d0      { if handle is -2 AUX:              }
      beq     @opennoerr
      cmp.w   #-3,d0      { if handle is -3 PRN:              }
      beq     @opennoerr
      move.w  d0,errno    { otherwise normal error            }
    @opennoerr:
      move.w  d0,i        { get handle as SIGNED VALUE...     }
    end;
  if errno <> 0 then
     Error2InOut;
  filerec(f).handle:=longint(i);
  { append mode }
  if (flags and $100)<>0 then
   begin
      do_seekend(filerec(f).handle);
      filerec(f).mode:=fmoutput; {fool fmappend}
   end;
end;

{*****************************************************************************
                           UnTyped File Handling
*****************************************************************************}

{$i file.inc}

{*****************************************************************************
                           Typed File Handling
*****************************************************************************}

{$i typefile.inc}

{*****************************************************************************
                           Text File Handling
*****************************************************************************}

{$i text.inc}

{*****************************************************************************
                           Directory Handling
*****************************************************************************}

procedure DosDir(func:byte;const s:string);
var
  buffer : array[0..255] of char;
  c : word;
begin
  move(s[1],buffer,length(s));
  buffer[length(s)]:=#0;
  AllowSlash(pchar(@buffer));
  c:=word(func);
  asm
        movem.l d2/d3/a2/a3,-(sp)
        pea     buffer
        move.w  c,-(sp)
        trap    #1
        add.l   #6,sp
        movem.l (sp)+,d2/d3/a2/a3
        tst.w   d0
        beq     @dosdirend
        move.w  d0,errno
     @dosdirend:
  end;
  if errno <> 0 then
     Error2InOut;
end;


procedure mkdir(const s : string);[IOCheck];
begin
  If (s='') or (InOutRes <> 0) then
   exit;
  DosDir($39,s);
end;


procedure rmdir(const s : string);[IOCheck];
begin
  if (s = '.' ) then
    InOutRes := 16;
  If (s='') or (InOutRes <> 0) then
   exit;
  DosDir($3a,s);
end;


procedure chdir(const s : string);[IOCheck];
begin
  If (s='') or (InOutRes <> 0) then
   exit;
  DosDir($3b,s);
end;


procedure getdir(drivenr : byte;var dir : string);
var
  buffer : array[0..255] of char;
  i    : longint;
  j: byte;
  drv: word;
begin
  drv:=word(drivenr);
  { if drivenr = 0, then we need some magic to get the
    current active path.
  }
  if (drivenr = 0) then
   begin
     asm
        movem.l d2/d3/a2/a3,-(sp)
        move.w #$19,-(sp)
        trap   #1
        add.l  #2,sp
        move.w d0,drv
        movem.l (sp)+,d2/d3/a2/a3
     end;
     { the next call starts from 1 as drives }
     { so adjust value accordingly.          }
     drv:=drv + 1;
   end;
  asm
            movem.l d2/d3/a2/a3,-(sp)
            { Get dir from drivenr : 0=default, 1=A etc... }
            move.w drv,-(sp)
            pea    buffer
            { call attos function 47H : Get dir }
            move.w #$47,-(sp)
            { make the call }
            trap   #1
            add.l  #8,sp
            movem.l (sp)+,d2/d3/a2/a3
  end;
  { conversion to pascal string }
  i:=0;
  while (buffer[i]<>#0) do
   begin
     if buffer[i]='/' then
      buffer[i]:='\';
     dir[i+3]:=buffer[i];
     inc(i);
   end;
  dir[2]:=':';
  dir[3]:='\';
  dir[0]:=char(i+2);
  if drivenr<>0 then   { Drive was supplied. We know it }
   dir[1]:=chr(65+drivenr-1)
  else
   dir[1]:=chr(byte(drv-1)+ord('A'));
end;


{*****************************************************************************
                         System Dependent Exit code
*****************************************************************************}
Procedure system_exit;
begin
 asm
  clr.l   d0
  move.w  exitcode,d0
  move.w  d0,-(sp)
  move.w  #$4c,-(sp)
  trap    #1
 end;
end;

{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}
procedure setup_environment;
var env_selector : word;
    env_count : longint;
    dos_env,cp : pchar;
begin
   env_count:=0;
   cp:=_env;
   while cp ^ <> #0 do
    begin
      inc(env_count);
      while (cp^ <> #0) do inc(longint(cp)); { skip to NUL }
      inc(longint(cp)); { skip to next character }
    end;
  envp := sysgetmem((env_count+1) * sizeof(pchar));
  if (envp = nil) then exit;
  cp:=_env;
  env_count:=0;
  while cp^ <> #0 do
   begin
     envp[env_count] := sysgetmem(strlen(cp)+1);
     move(cp, envp[env_count], strlen(cp)+1);
     inc(env_count);
     while (cp^ <> #0) do
      inc(longint(cp)); { skip to NUL }
     inc(longint(cp)); { skip to next character }
   end;
  envp[env_count]:=nil;
end;



   Procedure GenerateArgs;

   var
     ArgvLen : longint;

     procedure allocarg(idx,len:longint);
     var
        i,oldargvlen : longint;
     begin
       if idx>=argvlen then
        begin
          oldargvlen:=argvlen;
          argvlen:=(idx+8) and (not 7);
          sysreallocmem(argv,argvlen*sizeof(pointer));
          for i:=oldargvlen to argvlen-1 do
            argv[i]:=nil;
        end;
       { use realloc to reuse already existing memory }
       sysreallocmem(argv[idx],len+1);
     end;

   var
    count: word;
    start,_end : word;
    localindex: word;
    p : pchar;
    temp : string;

   Begin
        count:=0;
        argv:=nil;
        argvlen:=0;
        p:=args;
        argvlen:=0;
        { Set argv[0] }
        temp:='';
        allocarg(0,length(temp));
        move(temp[1],argv[0]^,length(temp));
        argv[0][length(temp)]:=#0;
        { Handle the other args }
        count := 0;
        { first index is one }
        localindex := 1;
        While p[count] <> #0 do
            Begin
              while (p[count] = ' ') or (p[count] = #9) do
                  inc(count);
              start:=count;
              while (p[count] <> #0) and (p[count] <> ' ') and (p[count] <> #9) do
                 inc(count);
              if (count-start>0) then
                begin
                  allocarg(localindex,count-start);
                  move(p[start],argv[localindex]^,count-start);
                  argv[localindex][count-start]:=#0;
                end;
              localindex:=localindex+1;
              if p[count] = #0 then break;
              inc(count);
            end;
        argc:=localindex;
   end;


begin
  argv:=nil;
  argc:=0;
{ Initialize ExitProc }
  ExitProc:=Nil;
{ Setup heap }
  InitHeap;
{ Setup stdin, stdout and stderr }
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);
  { The Amiga does not seem to have a StdError }
  { handle, therefore make the StdError handle }
  { equal to the StdOutputHandle.              }
  StdErrorHandle := StdOutputHandle;
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
  GenerateArgs;
  Setup_Environment;
{ Reset IO Error }
  InOutRes:=0;
  errno := 0;
end.

{
  $Log: sysatari.pas,v $
  Revision 1.1.2.7  2002/10/19 14:47:47  carl
    * use correct register conventions
    * read bugfix
    * open file bugfix
    + environment / argv / argc setup

  Revision 1.1.2.6  2002/10/18 22:01:25  carl
     * more major updates to make atari work again

  Revision 1.1.2.5  2002/09/29 14:00:54  carl
    * updates for compilation (still does not work on steem)

  Revision 1.1.2.4  2002/03/10 11:41:51  carl
  * updated with other RTL's
  * InOutRes := 16 with rmdir()

  Revision 1.1.2.3  2001/11/08 04:19:25  carl
  * fix constant problems

  Revision 1.1.2.2  2001/11/07 15:27:44  michael
  + Added OS describing constants

  Revision 1.1.2.1  2001/07/21 19:18:23  carl
  - removed unused variable

  Revision 1.1  2000/07/13 06:30:30  michael
  + Initial import

  Revision 1.14  2000/01/07 16:41:29  daniel
    * copyright 2000

  Revision 1.13  2000/01/07 16:32:23  daniel
    * copyright 2000 added

  Revision 1.12  1999/09/10 15:40:33  peter
    * fixed do_open flags to be > $100, becuase filemode can be upto 255

  Revision 1.11  1999/01/18 10:05:48  pierre
   + system_exit procedure added

  Revision 1.10  1998/12/28 15:50:43  peter
    + stdout, which is needed when you write something in the system unit
      to the screen. Like the runtime error

  Revision 1.9  1998/09/14 10:48:02  peter
    * FPC_ names
    * Heap manager is now system independent

  Revision 1.8  1998/07/15 12:11:59  carl
    * hmmm... can't remember! :(...

  Revision 1.5  1998/07/13 12:34:13  carl
    + Error2InoutRes implemented
    * do_read was doing a wrong os call!
    * do_open was not pushing the right values
    * DosDir was pushing the wrong params on the stack
    * do_close would never works, was pushing a longint instead of word

  Revision 1.4  1998/07/02 12:39:27  carl
    * IOCheck for mkdir,chdir and rmdir, just like in TP

  Revision 1.3  1998/07/01 14:40:20  carl
    + new stack checking implemented
    + IOCheck for chdir , getdir , mkdir and rmdir

  Revision 1.1.1.1  1998/03/25 11:18:47  root
  * Restored version

  Revision 1.8  1998/02/23 02:27:39  carl
    * make it link correctly

  Revision 1.7  1998/02/06 16:33:02  carl
    * oops... commited wrong file
    + do_open is now standard with other platforms

  Revision 1.5  1998/01/31 19:32:51  carl
    - removed incorrect $define

  Revision 1.4  1998/01/27 10:55:45  peter
    * Word Handles from -1 -> $ffff

  Revision 1.3  1998/01/25 22:44:14  peter
    * Using uniform layout

}
