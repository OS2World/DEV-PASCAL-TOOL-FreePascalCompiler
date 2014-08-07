{
    $Id: initc.pp,v 1.1.2.1 2002/09/27 20:59:51 carl Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Pierre Muller

    Code to generate execution of all c functions
    with constructors attributes

    Based on .ctor and .dtor sections of DJGPP gcc compiler

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit InitC;

interface

implementation

  { we need to include dpmiexcp unit
    to avoid getting troubles with _exit found both
    in libc and in v2prt0.as PM }
  uses
    dpmiexcp;

  type
     simple_proc = procedure;
  var
     first_ctor : longint;external name 'djgpp_first_ctor';
     ctor       : array [0..maxlongint div sizeof(simple_proc)] of simple_proc;external name 'djgpp_first_ctor';
     last_ctor  : longint;external name 'djgpp_last_ctor';
     first_dtor : longint;external name 'djgpp_first_dtor';
     dtor   : array [0..maxlongint div sizeof(simple_proc)] of simple_proc;external name 'djgpp_first_dtor';
     last_dtor  : longint;external name 'djgpp_last_dtor';
     bss_count : longint;external name '___bss_count';
  const
     save_exit : pointer = nil;

procedure run_c_constructors;

  const
     already_done : longint = -1;
  var
     f : simple_proc;
     i,nb : longint;
  begin
     if already_done=bss_count then
       exit;
     already_done:=bss_count;
     f:=ctor[0];
     nb:=((cardinal(@last_ctor)-cardinal(@first_ctor)) div sizeof(pointer));
     for i:=1 to nb do
       begin
          f();
          f:=ctor[i];
       end;
  end;
  
procedure run_c_destructors;
  const
     already_done : longint = -1;
  var
     f : simple_proc;
     i,nb : longint;
  begin
     exitproc:=save_exit;
     if already_done=bss_count then
       exit;
     already_done:=bss_count;
     f:=dtor[0];
     nb:=((cardinal(last_dtor)-cardinal(first_dtor)) div sizeof(pointer));
     for i:=1 to nb do
       begin
          f();
          f:=dtor[i];
       end;
  end;
  
begin
   run_c_constructors;
   If cardinal(@first_dtor)<>cardinal(@last_dtor) then
     begin
        { can exitproc be allready non nil here ?
          you have to make really weird things to achieve
          that be lets suppose it is possible !! (PM) }
        save_exit:=exitproc;
        exitproc:=@run_c_destructors;
     end;
end.

{
  $Log: initc.pp,v $
  Revision 1.1.2.1  2002/09/27 20:59:51  carl
    * fix problems with 2GB limit

  Revision 1.1  2000/07/13 06:30:38  michael
  + Initial import

  Revision 1.6  2000/02/09 16:59:29  peter
    * truncated log

  Revision 1.5  2000/01/09 00:35:17  pierre
   * initc now loads dpmiexcp unit to avoid linker problems

  Revision 1.4  2000/01/07 16:41:32  daniel
    * copyright 2000

  Revision 1.3  2000/01/07 16:32:23  daniel
    * copyright 2000 added

}
