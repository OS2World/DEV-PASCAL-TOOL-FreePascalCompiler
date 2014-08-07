{
    $Id: fpstring.pas,v 1.2 2002/09/07 15:40:44 peter Exp $
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 2000 by Florian Klaempfl

    Strings for menus, dialogs etc

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$i globdir.inc}
{$ifdef USERESSTRINGS}{$mode objfpc}{$endif}
unit fpstring;

  interface

    uses
       fpconst;

{$ifdef LANG_HUN}
{$i fpstrh.inc}    { Hungarian language file }
{$else}
 {$ifdef LANG_GER}
 {$i fpstrg.inc}    { German language file }
 {$else}
   {$i fpstre.inc}  { English language file }
 {$endif}
{$endif}

  implementation

end.
{
  $Log: fpstring.pas,v $
  Revision 1.2  2002/09/07 15:40:44  peter
    * old logs removed and tabs fixed

}
