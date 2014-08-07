{
    $Id: strings.pp,v 1.1.2.1 2001/11/19 00:30:23 carl Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Strings unit for PChar (asciiz/C compatible strings) handling

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit strings;

{$S-}
interface

    { Returns the length of a string }
    function strlen(p : pchar) : longint;

    { Converts a Pascal string to a null-terminated string }
    function strpcopy(d : pchar;const s : string) : pchar;

    { Converts a null-terminated string to a Pascal string }
    function strpas(p : pchar) : string;

    { Copies source to dest, returns a pointer to dest }
    function strcopy(dest,source : pchar) : pchar;

    { Copies at most maxlen bytes from source to dest. }
    { Returns a pointer to dest }
    function strlcopy(dest,source : pchar;maxlen : longint) : pchar;

    { Copies source to dest and returns a pointer to the terminating }
    { null character.    }
    function strecopy(dest,source : pchar) : pchar;

    { Returns a pointer tro the terminating null character of p }
    function strend(p : pchar) : pchar;

    { Appends source to dest, returns a pointer do dest}
    function strcat(dest,source : pchar) : pchar;

    { Compares str1 und str2, returns }
    { a value <0 if str1<str2;        }
    {  0 when str1=str2               }
    { and a value >0 if str1>str2     }
    function strcomp(str1,str2 : pchar) : longint;

    { The same as strcomp, but at most l characters are compared  }
    function strlcomp(str1,str2 : pchar;l : longint) : longint;

    { The same as strcomp but case insensitive       }
    function stricomp(str1,str2 : pchar) : longint;

    { Copies l characters from source to dest, returns dest. }
    function strmove(dest,source : pchar;l : longint) : pchar;

    { Appends at most l characters from source to dest }
    function strlcat(dest,source : pchar;l : longint) : pchar;

    { Returns a pointer to the first occurrence of c in p }
    { If c doesn't occur, nil is returned }
    function strscan(p : pchar;c : char) : pchar;

    { Returns a pointer to the last occurrence of c in p }
    { If c doesn't occur, nil is returned }
    function strrscan(p : pchar;c : char) : pchar;

    { converts p to all-lowercase, returns p   }
    function strlower(p : pchar) : pchar;

    { converts p to all-uppercase, returns p  }
    function strupper(p : pchar) : pchar;

    { The same al stricomp, but at most l characters are compared }
    function strlicomp(str1,str2 : pchar;l : longint) : longint;

    { Returns a pointer to the first occurrence of str2 in    }
    { str2 Otherwise returns nil                          }
    function strpos(str1,str2 : pchar) : pchar;

    { Makes a copy of p on the heap, and returns a pointer to this copy  }
    function strnew(p : pchar) : pchar;

    { Allocates L bytes on the heap, returns a pchar pointer to it }
    function stralloc(L : longint) : pchar;

    { Releases a null-terminated string from the heap  }
    procedure strdispose(p : pchar);

implementation

{  Read Processor dependent part, shared with sysutils unit }
{$i strings.inc }

{ Read processor denpendent part, NOT shared with sysutils unit }
{$i stringss.inc }

{ Functions not in assembler, but shared with sysutils unit  }
{$i stringsi.inc}

{ Functions, different from the one in sysutils }

    function stralloc(L : longint) : pchar;

      begin
         StrAlloc:=Nil;
         GetMem (Stralloc,l);
      end;

    function strnew(p : pchar) : pchar;

      var
         len : longint;

      begin
         strnew:=nil;
         if (p=nil) or (p^=#0) then
           exit;
         len:=strlen(p)+1;
         getmem(strnew,len);
         if strnew<>nil then
           strmove(strnew,p,len);
      end;

    procedure strdispose(p : pchar);

      begin
         if p<>nil then
          begin
            freemem(p);
            p:=nil;
          end;
      end;

end.

{
  $Log: strings.pp,v $
  Revision 1.1.2.1  2001/11/19 00:30:23  carl
  - no stack checking possible because used by lineinfo which
    might get infinite recursion if stack limit reached

  Revision 1.1  2000/07/13 06:30:48  michael
  + Initial import

  Revision 1.7  2000/03/19 08:40:14  peter
    * strdispose uses freemem(pointer) and resets pointer to nil

  Revision 1.6  2000/03/18 15:43:05  jonas
    * strdispose now uses dispose instead of freemem(strlen()+1)

  Revision 1.5  2000/02/09 16:59:31  peter
    * truncated log

  Revision 1.4  2000/01/07 16:41:36  daniel
    * copyright 2000

  Revision 1.3  2000/01/07 16:32:25  daniel
    * copyright 2000 added

  Revision 1.2  1999/12/10 15:02:12  peter
    * strnew is ofcourse also different between sysutils and strings, just
      like stralloc/strdispose.

}

