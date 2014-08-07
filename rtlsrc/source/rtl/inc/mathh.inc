{
    $Id: mathh.inc,v 1.1.2.9 2003/02/05 20:02:53 carl Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl,
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

   { declarations of the math routines }

{$ifdef DEFAULT_EXTENDED}
    function abs(d : extended) : extended;
    function arctan(d : extended) : extended;
    function cos(d : extended) : extended;
    function exp(d : extended) : extended;
    function frac(d : extended) : extended;
    function int(d : extended) : extended;
    function ln(d : extended) : extended;
    function pi : extended;
    function sin(d : extended) : extended;
    function sqr(d : extended) : extended;
    function sqrt(d : extended) : extended;
    function trunc(d : extended) : int64;
    function round(d : extended) : longint;
    function power(bas,expo : extended) : extended;
{$else DEFAULT_EXTENDED}
    function abs(d : real) : real;
    function arctan(d : real) : real;
    function cos(d : real) : real;
    function exp(d : real) : real;
    function frac(d : real) : real;
    function int(d : real) : real;
    function ln(d : real) : real;
    function round(d : real) : longint;
    function sin(d : real) : real;
    function sqr(d : real) : real;
    function sqrt(d : real) : real;
    function trunc(d : real) : longint;
    function power(bas,expo : real) : real;
    function pi : real;
{$endif DEFAULT_EXTENDED}

{$ifdef INT64}
    function power(bas,expo : int64) : int64;
{$else not INT64}
    function power(bas,expo : longint) : longint;
{$endif not INT64}

{$ifdef HASFIXED}
    function sqrt(d : fixed) : fixed;
    function Round(x: fixed): longint;
    function sqr(d : fixed) : fixed;
    function abs(d : fixed) : fixed;
    function frac(d : fixed) : fixed;
    function trunc(d : fixed) : longint;
    function int(d : fixed) : fixed;
{$endif HASFIXED}

{$ifdef SUPPORT_DOUBLE}
    type
       real48 = array[0..5] of byte;

    function Real2Double(r : real48) : double;
    operator := (b:real48) d:double;
{$ifdef SUPPORT_EXTENDED}
    operator := (b:real48) e:extended;
{$endif}
{$endif}

{
  $Log: mathh.inc,v $
  Revision 1.1.2.9  2003/02/05 20:02:53  carl
    * round returns longint again for compatibility with 1.0.6

  Revision 1.1.2.8  2003/01/15 22:33:38  pierre
   * fix problem if INT64 is not defined

  Revision 1.1.2.7  2003/01/15 00:48:29  peter
    * power returns int64

  Revision 1.1.2.6  2003/01/14 21:15:23  peter
    * regenerated

  Revision 1.1.2.5  2002/10/09 08:57:34  pierre
   * operator := from real48 to extended only if SUPPORT_EXTENDED is defined

  Revision 1.1.2.4  2001/12/14 03:39:11  carl
  + added ifdfef support_double, since we don't support double under m68k!

  Revision 1.1.2.3  2001/12/13 20:22:05  michael
  + Added double2real function from main branch

  Revision 1.1.2.2  2001/04/22 00:40:29  carl
  - trunc() for non-extended version is now as before.
   (the extended version should also be fixed!)

  Revision 1.1.2.1  2001/04/03 20:33:01  marco
   * Quickfixed trunc to int64 for Sebastian.

  Revision 1.1  2000/07/13 06:30:47  michael
  + Initial import

  Revision 1.9  2000/02/15 14:36:57  florian
    * disabled FIXED data type per default

  Revision 1.8  2000/02/09 16:59:30  peter
    * truncated log

  Revision 1.7  2000/01/07 16:41:34  daniel
    * copyright 2000

  Revision 1.6  2000/01/07 16:32:24  daniel
    * copyright 2000 added

}
