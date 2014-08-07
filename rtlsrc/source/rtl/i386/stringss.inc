{
    $Id: stringss.inc,v 1.1.2.3 2001/03/05 16:56:01 jonas Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Processor dependent part of strings.pp, not shared with
    sysutils unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

function strpas(p : pchar) : string;
{$i strpas.inc}

function strpcopy(d : pchar;const s : string) : pchar;assembler;
asm
        pushl   %esi            // Save ESI
        cld
        movl    s,%esi          // Load Source adress
        movl    d,%edi          // load destination address
        movzbl  (%esi),%ecx        // load length in ECX
        incl    %esi
        rep
        movsb
        movb    $0,(%edi)
        movl    d,%eax       // return value to EAX
        popl    %esi
end ['EDI','EAX','ECX'];

{
  $Log: stringss.inc,v $
  Revision 1.1.2.3  2001/03/05 16:56:01  jonas
    * moved implementations of strlen and strpas to separate include files
      (they were duplicated in i386.inc and strings.inc/stringss.inc)
    * strpas supports 'nil' pchars again (returns an empty string)

  Revision 1.1.2.2  2001/03/04 17:31:34  jonas
    * fixed all implementations of strpas

  Revision 1.1.2.1  2001/03/04 12:42:18  jonas
    * fixed strpas (was limited to 254 chars) and made it overall slightly faster

  Revision 1.1  2000/07/13 06:30:43  michael
  + Initial import

  Revision 1.15  2000/07/01 10:52:12  jonas
    * fixed reading past end-of-heap again (correctly this time I hope)

  Revision 1.14  2000/06/30 12:20:20  jonas
    * strpas is again slightly slower, but won't crash anymore if a pchar
      is passed to it that starts less than 4 bbytes from the heap end

  Revision 1.13  2000/06/12 19:53:32  peter
    * change .align to .balign

  Revision 1.12  2000/06/12 13:17:56  jonas
    * fixed typo :(

  Revision 1.11  2000/06/12 08:33:26  jonas
    * new fixed and faster strpas (previous version only returned the first
      254 chars when the pchar was aligned on a 4 byte boundary and was >=
      255 chars)

  Revision 1.10  2000/03/28 11:14:33  jonas
    * added missing register that is destroyed by strecopy
    + some destroyed register lists for procedures that didn't have one yet

  Revision 1.9  2000/02/09 16:59:29  peter
    * truncated log

  Revision 1.8  2000/01/11 22:56:57  pierre
    * wrong change for StrPas function corrected

  Revision 1.7  2000/01/11 21:12:15  marco
   * direct params to internal asm.

  Revision 1.6  2000/01/07 16:41:33  daniel
    * copyright 2000

}
