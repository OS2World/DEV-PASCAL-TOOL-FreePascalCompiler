{
    $Id: ole2.pp,v 1.1 2000/07/13 06:31:21 michael Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Implemtents some stuff of OLE2, tries to be Delphi compatible

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit ole2;

{$Mode ObjFpc}

  interface

    uses
       windows;

    type
       IUnknown = class
         public
           function QueryInterface(const iid: TIID; var obj): HResult; virtual; {$ifndef VER0_99_10}stdcall;{$endif} abstract;
           function AddRef: Longint; virtual; {$ifndef VER0_99_10}stdcall;{$endif} abstract;
           function Release: Longint; virtual; {$ifndef VER0_99_10}stdcall;{$endif} abstract;
       end;

  implementation

end.
{
  $Log: ole2.pp,v $
  Revision 1.1  2000/07/13 06:31:21  michael
  + Initial import

  Revision 1.5  2000/02/09 16:59:34  peter
    * truncated log

  Revision 1.4  2000/01/07 16:41:52  daniel
    * copyright 2000

}

