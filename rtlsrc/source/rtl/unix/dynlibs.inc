{
    $Id: dynlibs.inc,v 1.2 2002/09/07 16:01:27 peter Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Implement OS-dependent part of dynamic library loading.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifdef readinterface}

{ ---------------------------------------------------------------------
    Interface declarations
  ---------------------------------------------------------------------}

Type
  TLibHandle = Pointer;

Const
  NilHandle = Nil;

{$else}

{ ---------------------------------------------------------------------
    Implementation section
  ---------------------------------------------------------------------}

uses dl;

Function LoadLibrary(Name : AnsiString) : TLibHandle;

begin
  Result:=dlopen(Pchar(Name),RTLD_LAZY);
end;

Function GetProcedureAddress(Lib : TLibHandle; ProcName : AnsiString) : Pointer;

begin
  Result:=dlsym(lib,pchar(ProcName));
end;

Function UnloadLibrary(Lib : TLibHandle) : Boolean;

begin
  Result:=dlClose(Lib)=0;
end;

{$endif}

{
  $Log: dynlibs.inc,v $
  Revision 1.2  2002/09/07 16:01:27  peter
    * old logs removed and tabs fixed

}
