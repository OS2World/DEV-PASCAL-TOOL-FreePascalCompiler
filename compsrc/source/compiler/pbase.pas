{
    $Id: pbase.pas,v 1.1 2000/07/13 06:29:53 michael Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    Contains some helper routines for the parser

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit pbase;

  interface

    uses
       cobjects,tokens,globals,symtable
{$ifdef fixLeaksOnError}
       ,comphook
{$endif fixLeaksOnError}
{$IFDEF NEWST}
       ,symbols,defs
{$ENDIF NEWST}
       ;

    const
       { true, if we are after an assignement }
       afterassignment : boolean = false;

       { sspecial for handling procedure vars }
       getprocvar : boolean = false;
       getprocvardef : pprocvardef = nil;


    var
       { size of data segment, set by proc_unit or proc_program }
       datasize : longint;

       { for operators }
       optoken : ttoken;
       opsym : pvarsym;

       { symtable were unit references are stored }
       refsymtable : psymtable;

       { true, if only routine headers should be parsed }
       parse_only : boolean;

       { true, if we should ignore an equal in const x : 1..2=2 }
       ignore_equal : boolean;

{$ifdef fixLeaksOnError}
    { not worth it to make a pstack, there's only one data field (a pointer). }
    { in the interface, because pmodules and psub also use it for their names }
    var strContStack: TStack;
        pbase_old_do_stop: tstopprocedure;
{$endif fixLeaksOnError}

    function tokenstring(i : ttoken):string;

    { consumes token i, if the current token is unequal i }
    { a syntax error is written                           }
    procedure consume(i : ttoken);

    {Tries to consume the token i, and returns true if it was consumed:
     if token=i.}
    function try_to_consume(i:Ttoken):boolean;

    { consumes all tokens til atoken (for error recovering }
    procedure consume_all_until(atoken : ttoken);

    { consumes tokens while they are semicolons }
    procedure emptystats;

    { reads a list of identifiers into a string container }
    function idlist : pstringcontainer;

    { just for an accurate position of the end of a procedure (PM) }
    var
       last_endtoken_filepos: tfileposinfo;


  implementation

    uses
       files,scanner,systems,verbose;

    function tokenstring(i : ttoken):string;
      begin
        tokenstring:=tokeninfo^[i].str;
      end;

    { consumes token i, write error if token is different }
    procedure consume(i : ttoken);
      begin
        if (token<>i) and (idtoken<>i) then
          if token=_id then
            Message2(scan_f_syn_expected,tokeninfo^[i].str,'identifier '+pattern)
          else
            Message2(scan_f_syn_expected,tokeninfo^[i].str,tokeninfo^[token].str)
        else
          begin
            if token=_END then
              last_endtoken_filepos:=tokenpos;
            current_scanner^.readtoken;
          end;
      end;

    function try_to_consume(i:Ttoken):boolean;


    begin
        try_to_consume:=false;
        if (token=i) or (idtoken=i) then
            begin
                try_to_consume:=true;
                if token=_END then
                    last_endtoken_filepos:=tokenpos;
                current_scanner^.readtoken;
            end;
    end;

    procedure consume_all_until(atoken : ttoken);
      begin
         while (token<>atoken) and (idtoken<>atoken) do
          begin
            Consume(token);
            if token=_EOF then
             begin
               Consume(atoken);
               Message(scan_f_end_of_file);
               exit;
             end;
          end;
      end;


    procedure emptystats;
      begin
         repeat
         until not try_to_consume(_SEMICOLON);
      end;


    { reads a list of identifiers into a string container }
    function idlist : pstringcontainer;
      var
        sc : pstringcontainer;
      begin
         sc:=new(pstringcontainer,init);
         repeat
           sc^.insert_with_tokeninfo(pattern,
             tokenpos);
           consume(_id);
           if token=_COMMA then consume(_COMMA)
             else break
         until false;
         idlist:=sc;
      end;

{$ifdef fixLeaksOnError}
procedure pbase_do_stop; {$ifdef tp} far; {$endif tp}
var names: PStringContainer;
begin
  names := PStringContainer(strContStack.pop);
  while names <> nil do
    begin
      dispose(names,done);
      names := PStringContainer(strContStack.pop);
    end;
  strContStack.done;
  do_stop := pbase_old_do_stop;
{$ifdef tp}
  do_stop;
{$else tp}
  do_stop();
{$endif tp}
end;

begin
  strContStack.init;
  pbase_old_do_stop := do_stop;
  do_stop := {$ifndef tp}@{$endif}pbase_do_stop;
{$endif fixLeaksOnError}
end.

{
  $Log: pbase.pas,v $
  Revision 1.1  2000/07/13 06:29:53  michael
  + Initial import

  Revision 1.31  2000/03/11 21:11:24  daniel
    * Ported hcgdata to new symtable.
    * Alignment code changed as suggested by Peter
    + Usage of my is operator replacement, is_object

  Revision 1.30  2000/02/09 13:22:56  peter
    * log truncated

  Revision 1.29  2000/01/11 17:16:04  jonas
    * removed a lot of memory leaks when an error is encountered (caused by
      procinfo and pstringcontainers). There are still plenty left though :)

  Revision 1.28  2000/01/07 01:14:28  peter
    * updated copyright to 2000

  Revision 1.27  1999/11/06 14:34:21  peter
    * truncated log to 20 revs

  Revision 1.26  1999/10/01 08:02:46  peter
    * forward type declaration rewritten

  Revision 1.25  1999/09/02 18:47:44  daniel
    * Could not compile with TP, some arrays moved to heap
    * NOAG386BIN default for TP
    * AG386* files were not compatible with TP, fixed.

  Revision 1.24  1999/08/04 13:02:50  jonas
    * all tokens now start with an underscore
    * PowerPC compiles!!

  Revision 1.23  1999/07/27 23:42:10  peter
    * indirect type referencing is now allowed

  Revision 1.22  1999/07/26 09:42:10  florian
    * bugs 494-496 fixed

}
