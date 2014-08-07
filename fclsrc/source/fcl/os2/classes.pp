{
    $Id: classes.pp,v 1.6 2002/09/07 15:15:27 peter Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2002 by the Free Pascal development team

    Classes unit for OS/2

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}

{ determine the type of the resource/form file }
{$define Win16Res}

unit Classes;

interface

uses
  strings,
  sysutils;

{$i classesh.inc}


implementation

uses
  typinfo;

{ OS - independent class implementations are in /inc directory. }
{$i classes.inc}


initialization
  CommonInit;

finalization
  CommonCleanup;

end.
{
  $Log: classes.pp,v $
  Revision 1.6  2002/09/07 15:15:27  peter
    * old logs removed and tabs fixed

  Revision 1.5  2002/02/10 13:38:14  hajny
    * DosCalls dependency removed to avoid type redefinitions

}
