{
    $Id: system.pp,v 1.1 2000/07/13 06:31:17 michael Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    This is a prototype file to show all function that need to be implemented 
    for a new operating system (provided the processor specific
    function are already implemented !)
 
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ no stack check in system }
{$S-}
unit system;

interface

{ include system-independent routine headers }

{$I systemh.inc}

{ include heap support headers }

{$I heaph.inc}

implementation

{ include system independent routines }

{$I system.inc}

procedure setup_arguments;
begin
end;

procedure setup_environment;
begin
end;

{*****************************************************************************
                         System Dependent Exit code
*****************************************************************************}
Procedure system_exit;
begin
end;

{*****************************************************************************
                         Stack check code
*****************************************************************************}
procedure int_stackcheck(stack_size:longint);[public,alias:'FPC_STACKCHECK'];
{
  called when trying to get local stack if the compiler directive $S
  is set this function must preserve esi !!!! because esi is set by
  the calling proc for methods it must preserve all registers !!

  With a 2048 byte safe area used to write to StdIo without crossing
  the stack boundary
}
begin
end;

{*****************************************************************************
                              ParamStr/Randomize
*****************************************************************************}

{ number of args }
function paramcount : longint;
begin
  {paramcount := argc - 1;}
  paramcount:=0; 
end;

{ argument number l }
function paramstr(l : longint) : string;
begin
  {if (l>=0) and (l+1<=argc) then
   paramstr:=strpas(argv[l])
  else}
   paramstr:='';
end;

{ set randseed to a new pseudo random value }
procedure randomize;
begin
  {regs.realeax:=$2c00;
  sysrealintr($21,regs);
  hl:=regs.realedx and $ffff;
  randseed:=hl*$10000+ (regs.realecx and $ffff);}
  randseed:=0;
end;

{*****************************************************************************
                              Heap Management
*****************************************************************************}

{ first address of heap }
function getheapstart:pointer;{assembler;
asm
        leal    HEAP,%eax
end ['EAX'];}
begin
   getheapstart:=0;
end;

{ current length of heap }
function getheapsize:longint;{assembler;
asm
        movl    HEAPSIZE,%eax
end ['EAX'];}
begin
   getheapsize:=0;
end;

{ function to allocate size bytes more for the program }
{ must return the first address of new data space or -1 if fail }
function Sbrk(size : longint):longint;{assembler;
asm
        movl    size,%eax
        pushl   %eax
        call    ___sbrk
        addl    $4,%esp
end;}
begin
  Sbrk:=-1;
end;


{ include standard heap management }
{$I heap.inc}


{****************************************************************************
                        Low level File Routines
       All these functions can set InOutRes on errors 
 ****************************************************************************}

{ close a file from the handle value }
procedure do_close(handle : longint);
begin
  InOutRes:=1;
end;

procedure do_erase(p : pchar);
begin
  InOutRes:=1;
end;

procedure do_rename(p1,p2 : pchar);
begin
  InOutRes:=1;
end;

function do_write(h,addr,len : longint) : longint;
begin
  InOutRes:=1;
end;

function do_read(h,addr,len : longint) : longint;
begin
  InOutRes:=1;
end;

function do_filepos(handle : longint) : longint;
begin
  InOutRes:=1;
end;

procedure do_seek(handle,pos : longint);
begin
  InOutRes:=1;
end;

function do_seekend(handle:longint):longint;
begin
  InOutRes:=1;
end;

function do_filesize(handle : longint) : longint;
begin
  InOutRes:=1;
end;

{ truncate at a given position }
procedure do_truncate (handle,pos:longint);
begin
  InOutRes:=1;
end;

procedure do_open(var f;p:pchar;flags:longint);
{
  filerec and textrec have both handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $10)   the file will be append
  when (flags and $100)  the file will be truncate/rewritten
  when (flags and $1000) there is no check for close (needed for textfiles)
}
begin
  InOutRes:=1;
end;

function do_isdevice(handle:longint):boolean;
begin
  do_isdevice:=false;
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

{ should we consider #26 as the  end of a file ? }
{?? $DEFINE EOF_CTRLZ}

{$i text.inc}

{*****************************************************************************
                           Directory Handling
*****************************************************************************}
procedure mkdir(const s : string);[IOCheck];
begin
  InOutRes:=1;
end;

procedure rmdir(const s : string);[IOCheck];
begin
  InOutRes:=1;
end;

procedure chdir(const s : string);[IOCheck];
begin
  InOutRes:=1;
end;

procedure getdir(drivenr : byte;var dir : shortstring);
begin
  InOutRes:=1;
end;

{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}

Begin
{ Setup heap }
  InitHeap;
{ Setup stdin, stdout and stderr }
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
{ Setup environment and arguments }
  Setup_Environment;
  Setup_Arguments; 
{ Reset IO Error }
  InOutRes:=0;
End.
{
  $Log: system.pp,v $
  Revision 1.1  2000/07/13 06:31:17  michael
  + Initial import

  Revision 1.4  2000/01/07 16:41:51  daniel
    * copyright 2000

  Revision 1.3  2000/01/07 16:32:34  daniel
    * copyright 2000 added

  Revision 1.2  1999/01/18 10:11:10  pierre
   * sbrk must return -1 on fail

  Revision 1.1  1999/01/18 10:07:41  pierre
   Skeleton for new system unit

}
