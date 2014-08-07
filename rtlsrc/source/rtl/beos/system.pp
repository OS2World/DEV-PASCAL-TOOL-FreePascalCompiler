{
    $Id: system.pp,v 1.3.2.2 2002/02/15 18:18:37 carl Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    BeOS system unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ no stack check in system }

{$S-}
unit System;

interface

{ include system-independent routine headers }

{$I systemh.inc}

{ include heap support headers }

{$I heaph.inc}

var
  argc : longint;
  argv : ppchar;
  envp : ppchar;
  errno : longint;   { should be the same length a cint }

var
  UnusedHandle:longint;
  StdInputHandle:longint;
  StdOutputHandle:longint;
  StdErrorHandle:longint;
  
{Platform specific information}
const
 LineEnding = #10;
 LFNSupport = true;
 DirectorySeparator = '/';
 DriveSeparator = '';
 PathSeparator = ':';
 FileNameCaseSensitive  = True;
  

implementation


{$I system.inc}
{$i errno.inc}          { Error numbers                   }
{$I osposixh.inc}       { include POSIX types / constants }
{$I osposix.inc}        { include POSIX system calls      }
{$I beos.inc}           { Include BeOS system definitions }

{ define that we use our own heap routines }
{$define SYSTEM_HAS_GETHEAPSTART}
{$define SYSTEM_HAS_GETHEAPSIZE}
{$i sysposix.inc}


{*****************************************************************************
                              Executable filename
*****************************************************************************}

{ this routine sets up the paramstr(0) string at startup }
procedure setupexecname;
var
 cookie: cint;
 image : image_info;
 index : byte;
begin
  cookie:=0;
  fillchar(image, sizeof(image_info), 0);
  get_next_image_info(0,cookie,image);
  execpathstr := strpas(image.name);
  { problem with Be 4.5 noted... path contains . character }
  { if file is directly executed in CWD                    }
  index:=pos('/./',execpathstr);
  if index <> 0 then
    begin
      { remove the /. characters }
      Delete(execpathstr,index, 2);
    end;
end;




{*****************************************************************************
                              Heap Management
*****************************************************************************}

var myheapstart:longint;
    myheapsize:longint;
    myheaprealsize:longint;
    heap_handle:area_id;
    zero:longint;

{ first address of heap }
function getheapstart:pointer;
begin
   getheapstart:=pointer(myheapstart);
end;

{ current length of heap }
function getheapsize:longint;
begin
   getheapsize:=myheapsize;
end;



{ function to allocate size bytes more for the program }
{ must return the first address of new data space or -1 if fail }
function Sbrk(size : longint):longint;
var newsize,newrealsize:longint;
begin
  if (myheapsize+size)<=myheaprealsize then
  begin
    Sbrk:=myheapstart+myheapsize;
    myheapsize:=myheapsize+size;
    exit;
  end;
  newsize:=myheapsize+size;
  newrealsize:=(newsize and $FFFFF000)+$1000;
  { = 0 then ok }
  if resize_area(heap_handle,newrealsize)=0 then
  begin
    Sbrk:=myheapstart+myheapsize;
    myheapsize:=newsize;
    myheaprealsize:=newrealsize;
    exit;
  end;
  Sbrk:=-1;
end;


{ include standard heap management }
{$I heap.inc}


{****************************************************************************
                        Low level File Routines
       All these functions can set InOutRes on errors
 ****************************************************************************}



function do_isdevice(handle:longint):boolean;
begin
  do_isdevice:= (handle=StdInputHandle) or
                (handle=StdOutputHandle) or
                (handle=StdErrorHandle);
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
{$DEFINE SHORT_LINEBREAK}
{ DEFINE EXTENDED_EOF}

{$i text.inc}

{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}
var
  stacklength : longint;external name '__stklen';
begin
  { setup lowest value of stack pointer }
  StackBottom := SPtr - StackLength;
  zero:=0;
  myheapsize:=$2000;
  myheaprealsize:=$2000;
  myheapstart:=0;
  { According to BeOS doc. - the allocated memory    }
  { will be automatically freed when the application }
  { will terminate (CEC)                             }
  heap_handle:=create_area('fpcheap',myheapstart,B_ANY_ADDRESS,myheaprealsize,B_NO_LOCK,
    B_READ_AREA OR B_WRITE_AREA);
  if heap_handle>0 then
  begin
    InitHeap;
  end
  else
    system_exit;
{ Set up signals handlers }
  InstallSignals;
{ Setup heap }
  InitExceptions;
{ Arguments }
  SetupCmdLine;
{ Setup IO }
  StdInputHandle:=0;
  StdOutputHandle:=1;
  StdErrorHandle:=2;

  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
  
{ Reset IO Error }
  InOutRes:=0;
{ Get filename executable }  
  setupexecname;
end.


{
 $Log: system.pp,v $
 Revision 1.3.2.2  2002/02/15 18:18:37  carl
 * paramstr(0) bugfix

 Revision 1.3.2.1  2001/12/17 02:13:19  carl
 * sysbeos -> system unit

}
