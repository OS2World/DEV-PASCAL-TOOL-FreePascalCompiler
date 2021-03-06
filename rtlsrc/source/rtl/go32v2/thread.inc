{
    $Id: thread.inc,v 1.1 2000/07/13 06:30:40 michael Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Dummy multithreading support for DOS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
const
   threadvarblocksize : dword = 0;

type
   pd = ^dword;

var
   mainprogramthreadblock : pointer;

procedure init_threadvar(offset : pdword;size : dword);[public,alias: 'FPC_INIT_THREADVAR'];

  begin
     offset^:=threadvarblocksize;
     inc(threadblocksize,size);
  end;


function relocate_threadvar(offset : dword) : pointer;[public,alias: 'FPC_RELOCATE_THREADVAR'];

  asm
     movl mainprogramthreadblock,%eax
     addl offset,%eax
  end;

{
  $Log: thread.inc,v $
  Revision 1.1  2000/07/13 06:30:40  michael
  + Initial import

  Revision 1.4  2000/02/09 16:59:29  peter
    * truncated log

  Revision 1.3  2000/01/07 16:41:32  daniel
    * copyright 2000

}
