{
    $Id: scanner.pas,v 1.1.2.21 2003/01/14 23:56:20 peter Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    This unit implements the scanner part and handling of the switches

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
{$ifdef tp}
  {$F+,N+,E+,R-}
{$endif}
unit scanner;
{$ifdef FPC}
  {$goto on}
{$endif FPC}

  interface

    uses
{$ifdef Delphi}
       dmisc,
{$endif Delphi}
       globtype,version,tokens,
       cobjects,globals,verbose,comphook,files;

    const
{$ifdef TP}
       maxmacrolen=1024;
       preprocbufsize=1024;
{$else}
       maxmacrolen=16*1024;
       preprocbufsize=32*1024;
{$endif}
       Newline = #10;
       ErrorMacroDepth = 16;

    type
       tcommentstyle = (comment_none,comment_tp,comment_oldtp,comment_delphi,comment_c);

       pmacrobuffer = ^tmacrobuffer;
       tmacrobuffer = array[0..maxmacrolen-1] of char;

       preproctyp = (pp_ifdef,pp_ifndef,pp_if,pp_ifopt,pp_else);
       ppreprocstack = ^tpreprocstack;
       tpreprocstack = object
          typ     : preproctyp;
          accept  : boolean;
          next    : ppreprocstack;
          name    : stringid;
          line_nb : longint;
          constructor init(atyp:preproctyp;a:boolean;n:ppreprocstack);
          destructor done;
       end;

       pscannerfile = ^tscannerfile;
       tscannerfile = object
          inputfile    : pinputfile;  { current inputfile list }

          inputbuffer,                { input buffer }
          inputpointer : pchar;
          inputstart   : longint;

          line_no,                    { line }
          lastlinepos  : longint;

          lasttokenpos : longint;     { token }
          lasttoken,
          nexttoken    : ttoken;

          comment_level,
          yylexcount     : longint;
          lastasmgetchar : char;
          ignoredirectives : tstringcontainer; { ignore directives, used to give warnings only once }
          preprocstack   : ppreprocstack;
          invalid        : boolean; { flag if sourcefiles have been destroyed ! }
          in_asm_string  : boolean;
          constructor init(const fn:string);
          destructor done;
        { File buffer things }
          function  openinputfile:boolean;
          procedure closeinputfile;
          function  tempopeninputfile:boolean;
          procedure tempcloseinputfile;
          procedure saveinputfile;
          procedure restoreinputfile;
          procedure nextfile;
          procedure addfile(hp:pinputfile);
          procedure reload;
          procedure insertmacro(const macname:string;p:pchar;len,line,refindex:longint);
        { Scanner things }
          procedure gettokenpos;
          procedure inc_comment_level;
          procedure dec_comment_level;
          procedure illegal_char(c:char);
          procedure end_of_file;
          procedure checkpreprocstack;
          procedure poppreprocstack;
          procedure addpreprocstack(atyp : preproctyp;a:boolean;const s:string;w:longint);
          procedure elsepreprocstack;
          procedure linebreak;
          procedure readchar;
          procedure readstring;
          procedure readnumber;
          function  readid:string;
          function  readval:longint;
          function  readcomment:string;
          function  readstate:char;
          procedure skipspace;
          procedure skipuntildirective;
          procedure skipcomment;
          procedure skipdelphicomment;
          procedure skipoldtpcomment;
          procedure readtoken;
          function  readpreproc:ttoken;
          function  asmgetchar:char;
       end;

       ppreprocfile=^tpreprocfile;
       tpreprocfile=object
         f   : text;
         buf : pointer;
         spacefound,
         eolfound : boolean;
         constructor init(const fn:string);
         destructor  done;
         procedure Add(const s:string);
         procedure AddSpace;
       end;


    var
        c              : char;
        orgpattern,
        pattern        : string;
        current_scanner : pscannerfile;
        aktcommentstyle : tcommentstyle; { needed to use read_comment from directives }

        preprocfile : ppreprocfile; { used with only preprocessing }


implementation

    uses
{$ifndef delphi}
      dos,
{$endif delphi}
      systems,symtable,switches
{$IFDEF NEWST}
      ,symbols
{$ENDIF NEWST};

{*****************************************************************************
                              Helper routines
*****************************************************************************}

    const
      { use any special name that is an invalid file name to avoid problems }
      preprocstring : array [preproctyp] of string[7]
        = ('$IFDEF','$IFNDEF','$IF','$IFOPT','$ELSE');


    function is_keyword(const s:string):boolean;
      var
        low,high,mid : longint;
      begin
        if not (length(s) in [2..tokenidlen]) then
         begin
           is_keyword:=false;
           exit;
         end;
        low:=ord(tokenidx^[length(s),s[1]].first);
        high:=ord(tokenidx^[length(s),s[1]].last);
        while low<high do
         begin
           mid:=(high+low+1) shr 1;
           if pattern<tokeninfo^[ttoken(mid)].str then
            high:=mid-1
           else
            low:=mid;
         end;
        is_keyword:=(pattern=tokeninfo^[ttoken(high)].str) and
                    (tokeninfo^[ttoken(high)].keyword in aktmodeswitches);
      end;


{*****************************************************************************
                            Preprocessor writting
*****************************************************************************}

    constructor tpreprocfile.init(const fn:string);
      begin
      { open outputfile }
        assign(f,fn);
        {$I-}
         rewrite(f);
        {$I+}
        if ioresult<>0 then
         Comment(V_Fatal,'can''t create file '+fn);
        getmem(buf,preprocbufsize);
        settextbuf(f,buf^,preprocbufsize);
      { reset }
        eolfound:=false;
        spacefound:=false;
      end;


    destructor tpreprocfile.done;
      begin
        close(f);
        freemem(buf,preprocbufsize);
      end;


    procedure tpreprocfile.add(const s:string);
      begin
        write(f,s);
      end;

    procedure tpreprocfile.addspace;
      begin
        if eolfound then
         begin
           writeln(f,'');
           eolfound:=false;
           spacefound:=false;
         end
        else
         if spacefound then
          begin
            write(f,' ');
            spacefound:=false;
          end;
      end;


{*****************************************************************************
                              TPreProcStack
*****************************************************************************}

    constructor tpreprocstack.init(atyp : preproctyp;a:boolean;n:ppreprocstack);
      begin
        accept:=a;
        typ:=atyp;
        next:=n;
      end;


    destructor tpreprocstack.done;
      begin
      end;


{****************************************************************************
                                TSCANNERFILE
 ****************************************************************************}

    constructor tscannerfile.init(const fn:string);
      begin
        inputfile:=do_openinputfile(fn);
        if assigned(current_module) then
          current_module^.sourcefiles^.register_file(inputfile);
      { reset localinput }
        inputbuffer:=nil;
        inputpointer:=nil;
        inputstart:=0;
      { reset scanner }
        preprocstack:=nil;
        comment_level:=0;
        yylexcount:=0;
        block_type:=bt_general;
        line_no:=0;
        lastlinepos:=0;
        lasttokenpos:=0;
        lasttoken:=NOTOKEN;
        nexttoken:=NOTOKEN;
        lastasmgetchar:=#0;
        ignoredirectives.init;
        invalid:=false;
        in_asm_string:=false;
      { load block }
        if not openinputfile then
         Message1(scan_f_cannot_open_input,fn);
        reload;
      { process first read char }
        case c of
         #26 : reload;
         #10,
         #13 : linebreak;
        end;
      end;


    destructor tscannerfile.done;
      begin
        resetscanners(@self);
        if not invalid then
          begin
             if status.errorcount=0 then
              checkpreprocstack
             else
              begin
                while assigned(preprocstack) do
                 poppreprocstack;
              end;
           { close file, but only if we are the first compile }
           { probably not necessary anymore with invalid flag PM }
             if not current_module^.in_second_compile then
              begin
                if not inputfile^.closed then
                 closeinputfile;
              end;
          end
        else
          while assigned(preprocstack) do
            poppreprocstack;
         ignoredirectives.done;
       end;


    function tscannerfile.openinputfile:boolean;
      begin
        openinputfile:=inputfile^.open;
      { load buffer }
        inputbuffer:=inputfile^.buf;
        inputpointer:=inputfile^.buf;
        inputstart:=inputfile^.bufstart;
      { line }
        line_no:=0;
        lastlinepos:=0;
        lasttokenpos:=0;
      end;


    procedure tscannerfile.closeinputfile;
      begin
        inputfile^.close;
      { reset buffer }
        inputbuffer:=nil;
        inputpointer:=nil;
        inputstart:=0;
      { reset line }
        line_no:=0;
        lastlinepos:=0;
        lasttokenpos:=0;
      end;


    function tscannerfile.tempopeninputfile:boolean;
      begin
        tempopeninputfile:=inputfile^.tempopen;
      { reload buffer }
        inputbuffer:=inputfile^.buf;
        inputpointer:=inputfile^.buf;
        inputstart:=inputfile^.bufstart;
      end;


    procedure tscannerfile.tempcloseinputfile;
      begin
        inputfile^.setpos(inputstart+(inputpointer-inputbuffer));
        inputfile^.tempclose;
      { reset buffer }
        inputbuffer:=nil;
        inputpointer:=nil;
        inputstart:=0;
      end;


    procedure tscannerfile.saveinputfile;
      begin
        inputfile^.saveinputpointer:=inputpointer;
        inputfile^.savelastlinepos:=lastlinepos;
        inputfile^.saveline_no:=line_no;
      end;


    procedure tscannerfile.restoreinputfile;
      begin
        inputpointer:=inputfile^.saveinputpointer;
        lastlinepos:=inputfile^.savelastlinepos;
        line_no:=inputfile^.saveline_no;
        if not inputfile^.is_macro then
          parser_current_file:=inputfile^.name^;
      end;


    procedure tscannerfile.nextfile;
      var
        to_dispose : pinputfile;
      begin
        if assigned(inputfile^.next) then
         begin
           if inputfile^.is_macro then
             to_dispose:=inputfile
           else
             to_dispose:=nil;
           { we can allways close the file, no ? }
           inputfile^.close;
           inputfile:=inputfile^.next;
           if assigned(to_dispose) then
             begin
               dispose(to_dispose,done);
               { we are one level lower in macro nesting now }
               dec(yylexcount);
             end;
           restoreinputfile;
         end;
      end;


    procedure tscannerfile.addfile(hp:pinputfile);
      begin
        saveinputfile;
      { add to list }
        hp^.next:=inputfile;
        inputfile:=hp;
      { load new inputfile }
        restoreinputfile;
      end;


    procedure tscannerfile.reload;
      begin
        with inputfile^ do
         begin
           { when nothing more to read then leave immediatly, so we
             don't change the aktfilepos and leave it point to the last
             char }
           if (c=#26) and (not assigned(next)) then
            exit;
           repeat
           { still more to read?, then change the #0 to a space so its seen
             as a seperator, this can't be used for macro's which can change
             the place of the #0 in the buffer with tempopen }
             if (c=#0) and (bufsize>0) and
                not(inputfile^.is_macro) and
                (inputpointer-inputbuffer<bufsize) then
              begin
                c:=' ';
                inc(longint(inputpointer));
                exit;
              end;
           { can we read more from this file ? }
             if (c<>#26) and (not endoffile) then
              begin
                readbuf;
                inputpointer:=buf;
                inputbuffer:=buf;
                inputstart:=bufstart;
              { first line? }
                if line_no=0 then
                 begin
                   line_no:=1;
                   if cs_asm_source in aktglobalswitches then
                     inputfile^.setline(line_no,bufstart);
                 end;
              end
             else
              begin
              { load eof position in tokenpos/aktfilepos }
                gettokenpos;
              { close file }
                closeinputfile;
              { no next module, than EOF }
                if not assigned(inputfile^.next) then
                 begin
                   c:=#26;
                   exit;
                 end;
              { load next file and reopen it }
                nextfile;
                tempopeninputfile;
              { status }
                Message1(scan_t_back_in,inputfile^.name^);
              end;
           { load next char }
             c:=inputpointer^;
             inc(longint(inputpointer));
           until c<>#0; { if also end, then reload again }
         end;
      end;


    procedure tscannerfile.insertmacro(const macname:string;p:pchar;len,line,refindex:longint);
      var
        hp : pinputfile;
      begin
      { save old postion and decrease linebreak }
        if c=newline then
         dec(line_no);
        dec(longint(inputpointer));
        tempcloseinputfile;
      { create macro 'file' }
        { use special name to dispose after !! }
        hp:=do_openinputfile('_Macro_.'+macname);
        addfile(hp);
        with inputfile^ do
         begin
           setmacro(p,len);
         { local buffer }
           inputbuffer:=buf;
           inputpointer:=buf;
           inputstart:=bufstart;
           ref_index:=refindex
         end;
      { reset line }
        line_no:=line;
        lastlinepos:=0;
        lasttokenpos:=0;
      { load new c }
        c:=inputpointer^;
        inc(longint(inputpointer));
      end;


    procedure tscannerfile.gettokenpos;
    { load the values of tokenpos and lasttokenpos }
      begin
        lasttokenpos:=inputstart+(inputpointer-inputbuffer);
        tokenpos.line:=line_no;
        tokenpos.column:=lasttokenpos-lastlinepos;
        tokenpos.fileindex:=inputfile^.ref_index;
        aktfilepos:=tokenpos;
      end;


    procedure tscannerfile.inc_comment_level;
      var
         oldaktfilepos : tfileposinfo;
      begin
         if (m_nested_comment in aktmodeswitches) then
           inc(comment_level)
         else
           comment_level:=1;
         if (comment_level>1) then
          begin
             oldaktfilepos:=aktfilepos;
             gettokenpos; { update for warning }
             Message1(scan_w_comment_level,tostr(comment_level));
             aktfilepos:=oldaktfilepos;
          end;
      end;


    procedure tscannerfile.dec_comment_level;
      begin
         if (m_nested_comment in aktmodeswitches) then
           dec(comment_level)
         else
           comment_level:=0;
      end;


    procedure tscannerfile.linebreak;
      var
         cur : char;
         oldtokenpos,
         oldaktfilepos : tfileposinfo;
      begin
        with inputfile^ do
         begin
           if (byte(inputpointer^)=0) and not(endoffile) then
            begin
              cur:=c;
              reload;
              if byte(cur)+byte(c)<>23 then
                dec(longint(inputpointer));
            end
           else
            begin
            { Fix linebreak to be only newline (=#10) for all types of linebreaks }
              if (byte(inputpointer^)+byte(c)=23) then
                inc(longint(inputpointer));
            end;
           c:=newline;
         { increase line counters }
           lastlinepos:=bufstart+(inputpointer-inputbuffer);
           inc(line_no);
         { update linebuffer }
           if cs_asm_source in aktglobalswitches then
             inputfile^.setline(line_no,lastlinepos);
         { update for status and call the show status routine,
           but don't touch aktfilepos ! }
           oldaktfilepos:=aktfilepos;
           oldtokenpos:=tokenpos;
           gettokenpos; { update for v_status }
           inc(status.compiledlines);
           ShowStatus;
           aktfilepos:=oldaktfilepos;
           tokenpos:=oldtokenpos;
         end;
      end;


    procedure tscannerfile.illegal_char(c:char);
      var
        s : string;
      begin
        if c in [#32..#255] then
         s:=''''+c+''''
        else
         s:='#'+tostr(ord(c));
        Message2(scan_f_illegal_char,s,'$'+hexstr(ord(c),2));
      end;


    procedure tscannerfile.end_of_file;
      begin
        checkpreprocstack;
        Message(scan_f_end_of_file);
      end;


    procedure tscannerfile.checkpreprocstack;
      begin
      { check for missing ifdefs }
        while assigned(preprocstack) do
         begin
           Message3(scan_e_endif_expected,preprocstring[preprocstack^.typ],preprocstack^.name,tostr(preprocstack^.line_nb));
           poppreprocstack;
         end;
      end;


    procedure tscannerfile.poppreprocstack;
      var
        hp : ppreprocstack;
      begin
        if assigned(preprocstack) then
         begin
           Message1(scan_c_endif_found,preprocstack^.name);
           hp:=preprocstack^.next;
           dispose(preprocstack,done);
           preprocstack:=hp;
         end
        else
         Message(scan_e_endif_without_if);
      end;


    procedure tscannerfile.addpreprocstack(atyp : preproctyp;a:boolean;const s:string;w:longint);
      begin
        preprocstack:=new(ppreprocstack,init(atyp,((preprocstack=nil) or preprocstack^.accept) and a,preprocstack));
        preprocstack^.name:=s;
        preprocstack^.line_nb:=line_no;
        if preprocstack^.accept then
         Message2(w,preprocstack^.name,'accepted')
        else
         Message2(w,preprocstack^.name,'rejected');
      end;


    procedure tscannerfile.elsepreprocstack;
      begin
        if assigned(preprocstack) then
         begin
           preprocstack^.typ:=pp_else;
           preprocstack^.line_nb:=line_no;
           if not(assigned(preprocstack^.next)) or (preprocstack^.next^.accept) then
            preprocstack^.accept:=not preprocstack^.accept;
           if preprocstack^.accept then
            Message2(scan_c_else_found,preprocstack^.name,'accepted')
           else
            Message2(scan_c_else_found,preprocstack^.name,'rejected');
         end
        else
         Message(scan_e_endif_without_if);
      end;


    procedure tscannerfile.readchar;
      begin
        c:=inputpointer^;
        if c=#0 then
         reload
        else
         inc(longint(inputpointer));
        case c of
         #26 : reload;
         #10,
         #13 : linebreak;
        end;
      end;


    procedure tscannerfile.readstring;
      var
        i : longint;
      begin
        i:=0;
        repeat
          case c of
                 '_',
            '0'..'9',
            'A'..'Z' : begin
                         if i<255 then
                          begin
                            inc(i);
                            orgpattern[i]:=c;
                            pattern[i]:=c;
                          end;
                         c:=inputpointer^;
                         inc(longint(inputpointer));
                       end;
            'a'..'z' : begin
                         if i<255 then
                          begin
                            inc(i);
                            orgpattern[i]:=c;
                            pattern[i]:=chr(ord(c)-32)
                          end;
                         c:=inputpointer^;
                         inc(longint(inputpointer));
                       end;
              #0 : reload;
              #26 : begin
                      reload;
                      if c=#26 then
                        break;
                    end;
             #13,#10 : begin
                         linebreak;
                         break;
                       end;
          else
           break;
          end;
        until false;
        {$ifndef TP}
          {$ifopt H+}
            setlength(orgpattern,i);
            setlength(pattern,i);
          {$else}
            orgpattern[0]:=chr(i);
            pattern[0]:=chr(i);
          {$endif}
        {$else}
          orgpattern[0]:=chr(i);
          pattern[0]:=chr(i);
        {$endif}
      end;


    procedure tscannerfile.readnumber;
      var
        base,
        i  : longint;
      begin
        case c of
         '&' : begin
                 readchar;
                 base:=8;
                 pattern[1]:='&';
                 i:=1;
               end;
         '%' : begin
                 readchar;
                 base:=2;
                 pattern[1]:='%';
                 i:=1;
               end;
         '$' : begin
                 readchar;
                 base:=16;
                 pattern[1]:='$';
                 i:=1;
               end;
        else
         begin
           base:=10;
           i:=0;
         end;
        end;
        while ((base>=10) and (c in ['0'..'9'])) or
              ((base=16) and (c in ['A'..'F','a'..'f'])) or
              ((base=8) and (c in ['0'..'7'])) or
              ((base=2) and (c in ['0'..'1'])) do
         begin
           if i<255 then
            begin
              inc(i);
              pattern[i]:=c;
            end;
        { get next char }
           c:=inputpointer^;
           if c=#0 then
            reload
           else
            inc(longint(inputpointer));
         end;
      { was the next char a linebreak ? }
        case c of
         #26 : reload;
         #10,
         #13 : linebreak;
        end;
        {$ifndef TP}
          {$ifopt H+}
            setlength(pattern,i);
          {$else}
            pattern[0]:=chr(i);
          {$endif}
        {$else}
          pattern[0]:=chr(i);
        {$endif}
      end;


    function tscannerfile.readid:string;
      begin
        readstring;
        readid:=pattern;
      end;


    function tscannerfile.readval:longint;
      var
        l : longint;
        w : integer;
      begin
        readnumber;
        valint(pattern,l,w);
        readval:=l;
      end;


    function tscannerfile.readcomment:string;
      var
        i : longint;
      begin
        i:=0;
        repeat
          case c of
           '{' :
             if aktcommentstyle=comment_tp then
              inc_comment_level;
           '}' :
             if aktcommentstyle=comment_tp then
              begin
                readchar;
                dec_comment_level;
                if comment_level=0 then
                 break
                else
                 continue;
              end;
           '*' :
             if aktcommentstyle=comment_oldtp then
              begin
                readchar;
                if c=')' then
                 begin
                   readchar;
                   dec_comment_level;
                   break;
                 end
                else
                 { Add both characters !!}
                 if (i<255) then
                   begin
                   inc(i);
                   readcomment[i]:='*';
                   if (i<255) then
                     begin
                     inc(i);
                     readcomment[i]:='*';
                     end;
                   end;
              end
             else
              { Not old TP comment, so add...}
              begin
              if (i<255) then
               begin
               inc(i);
               readcomment[i]:='*';
               end;
              end;
           #26 :
              end_of_file;
          else
            begin
              if (i<255) then
               begin
                 inc(i);
                 readcomment[i]:=c;
               end;
            end;
          end;
          c:=inputpointer^;
          if c=#0 then
           reload
          else
           inc(longint(inputpointer));
          if c in [#10,#13] then
           linebreak;
        until false;
        {$ifndef TP}
          {$ifopt H+}
            setlength(readcomment,i);
          {$else}
            readcomment[0]:=chr(i);
          {$endif}
        {$else}
          readcomment[0]:=chr(i);
        {$endif}
      end;


    function tscannerfile.readstate:char;
      var
        state : char;
      begin
        state:=' ';
        if c=' ' then
         begin
           current_scanner^.skipspace;
           current_scanner^.readid;
           if pattern='ON' then
            state:='+'
           else
            if pattern='OFF' then
             state:='-';
         end
        else
         state:=c;
        if not (state in ['+','-']) then
         Message(scan_e_wrong_switch_toggle);
        readstate:=state;
      end;


    procedure tscannerfile.skipspace;
      begin
        while c in [' ',#9..#13] do
         begin
           c:=inputpointer^;
           if c=#0 then
            reload
           else
            inc(longint(inputpointer));
           case c of
            #26 :
              reload;
            #10,
            #13 :
              linebreak;
           end;
         end;
      end;


    procedure tscannerfile.skipuntildirective;
      var
        incomment : boolean;
        found : longint;
        next_char_loaded : boolean;
        oldcommentstyle : tcommentstyle;
      begin
         found:=0;
         next_char_loaded:=false;
         incomment:=true;
         oldcommentstyle:=aktcommentstyle;
         repeat
           case c of
             #26 :
               end_of_file;
             '{' :
               begin
                 if not(m_nested_comment in aktmodeswitches) or
                    (comment_level=0) then
                  begin
                    found:=1;
                    aktcommentstyle:=comment_tp;
                  end;
                 inc_comment_level;
                 incomment:=true;
               end;
             '}' :
               begin
                 dec_comment_level;
                 found:=0;
                 incomment:=false;
               end;
             '*' :
               begin
                 if incomment then
                   begin
                     readchar;
                     if c=')' then
                       begin
                         dec_comment_level;
                         found:=0;
                         incomment:=false;
                       end
                     else
                       next_char_loaded:=true;
                   end
                 else
                   found := 0;
               end;
             '$' :
               begin
                 if found=1 then
                  found:=2;
               end;
             '''' :
               if not incomment then
                begin
                  repeat
                    readchar;
                    case c of
                      #26 :
                        end_of_file;
                      newline :
                        break;
                      '''' :
                        begin
                          readchar;
                          if c<>'''' then
                           begin
                             next_char_loaded:=true;
                             break;
                           end;
                        end;
                    end;
                  until false;
                end;
             '(' :
               begin
                 if not incomment then
                  begin
                    readchar;
                    if c='*' then
                     begin
                       readchar;
                       if c='$' then
                        begin
                          found:=2;
                          inc_comment_level;
                          aktcommentstyle:=comment_oldtp;
                        end
                       else
                        begin
                          skipoldtpcomment;
                          aktcommentstyle:=oldcommentstyle;
                        end;
                     end
                    else
                     next_char_loaded:=true;
                  end
                 else
                  found:=0;
               end;
             '/' :
               begin
                 if not incomment then
                  begin
                    readchar;
                    if c='/' then
                     begin
                       skipdelphicomment;
                       aktcommentstyle:=oldcommentstyle;
                     end
                    else
                     next_char_loaded:=true;
                  end
                 else
                  found:=0;
               end;
             else
               found:=0;
           end;
           if next_char_loaded then
             next_char_loaded:=false
           else
             begin
                c:=inputpointer^;
                if c=#0 then
                  reload
                else
                  inc(longint(inputpointer));
                case c of
                  #26 : reload;
                  #10,
                  #13 : linebreak;
                end;
             end;
         until (found=2);
      end;


{****************************************************************************
                      Include directive scanning/parsing
****************************************************************************}

{$i scandir.inc}


{****************************************************************************
                             Comment Handling
****************************************************************************}

    procedure tscannerfile.skipcomment;
      begin
        aktcommentstyle:=comment_tp;
        readchar;
        inc_comment_level;
      { handle compiler switches }
        if (c='$') then
         handledirectives;
      { handle_switches can dec comment_level,  }
        while (comment_level>0) do
         begin
           case c of
            '{' : inc_comment_level;
            '}' : dec_comment_level;
            #26 : end_of_file;
           end;
           c:=inputpointer^;
           if c=#0 then
            reload
           else
            inc(longint(inputpointer));
           case c of
            #26 : reload;
            #10,
            #13 : linebreak;
           end;
         end;
        aktcommentstyle:=comment_none;
      end;


    procedure tscannerfile.skipdelphicomment;
      begin
        aktcommentstyle:=comment_delphi;
        inc_comment_level;
        readchar;
      { this is currently not supported }
        if c='$' then
          Message(scan_e_wrong_styled_switch);
      { skip comment }
        while not (c in [newline,#26]) do
           readchar;
        dec_comment_level;
        aktcommentstyle:=comment_none;
      end;


    procedure tscannerfile.skipoldtpcomment;
      var
        found : longint;
      begin
        aktcommentstyle:=comment_oldtp;
        inc_comment_level;
        { only load a char if last already processed,
          was cause of bug1634 PM }
        if c=#0 then
          readchar;
      { this is now supported }
        if (c='$') then
         handledirectives;
      { skip comment }
        while (comment_level>0) do
         begin
           found:=0;
           repeat
             case c of
               #26 :
                 end_of_file;
               '*' :
                 begin
                   if found=3 then
                    found:=4
                   else
                    found:=1;
                 end;
               ')' :
                 begin
                   if found in [1,4] then
                    begin
                      dec_comment_level;
                      if comment_level=0 then
                       found:=2
                      else
                       found:=0;
                    end;
                 end;
               '(' :
                 begin
                   if found=4 then
                    inc_comment_level;
                   found:=3;
                 end;
               else
                 begin
                   if found=4 then
                    inc_comment_level;
                   found:=0;
                 end;
             end;
             c:=inputpointer^;
             if c=#0 then
              reload
             else
              inc(longint(inputpointer));
             case c of
              #26 : reload;
              #10,
              #13 : linebreak;
             end;
           until (found=2);
         end;
        aktcommentstyle:=comment_none;
      end;



{****************************************************************************
                               Token Scanner
****************************************************************************}

    procedure tscannerfile.readtoken;
      var
        code    : integer;
        len,
        low,high,mid : longint;
        m       : longint;
        mac     : pmacrosym;
        msgwritten : boolean;
        asciinr : string[6];
      label
         exit_label;
      begin
        if localswitcheschanged then
          begin
            aktlocalswitches:=nextaktlocalswitches;
            localswitcheschanged:=false;
          end;
      { was there already a token read, then return that token }
        if nexttoken<>NOTOKEN then
         begin
           token:=nexttoken;
           nexttoken:=NOTOKEN;
           goto exit_label;
         end;

      { Skip all spaces and comments }
        repeat
          case c of
            '{' :
              skipcomment;
            ' ',#9..#13 :
              begin
                if parapreprocess then
                 begin
                   if c=#10 then
                    preprocfile^.eolfound:=true
                   else
                    preprocfile^.spacefound:=true;
                 end;
                skipspace;
              end
            else
              break;
          end;
        until false;

      { Save current token position, for EOF its already loaded }
        if c<>#26 then
         gettokenpos;

      { Check first for a identifier/keyword, this is 20+% faster (PFV) }
        if c in ['A'..'Z','a'..'z','_'] then
         begin
           readstring;
           token:=_ID;
           idtoken:=_ID;
         { keyword or any other known token,
           pattern is always uppercased }
           if (pattern[1]<>'_') and (length(pattern) in [2..tokenidlen]) then
            begin
              low:=ord(tokenidx^[length(pattern),pattern[1]].first);
              high:=ord(tokenidx^[length(pattern),pattern[1]].last);
              while low<high do
               begin
                 mid:=(high+low+1) shr 1;
                 if pattern<tokeninfo^[ttoken(mid)].str then
                  high:=mid-1
                 else
                  low:=mid;
               end;
              if pattern=tokeninfo^[ttoken(high)].str then
               begin
                 if tokeninfo^[ttoken(high)].keyword in aktmodeswitches then
                  if tokeninfo^[ttoken(high)].op=NOTOKEN then
                    token:=ttoken(high)
                  else
                    token:=tokeninfo^[ttoken(high)].op;
                 idtoken:=ttoken(high);
               end;
            end;
         { Only process identifiers and not keywords }
           if token=_ID then
            begin
            { this takes some time ... }
              if (cs_support_macro in aktmoduleswitches) then
               begin
                 mac:=pmacrosym(macros^.search(pattern));
                 if assigned(mac) and (assigned(mac^.buftext)) then
                  begin
                    if yylexcount>16 then
                      Message(scan_e_macro_deep_ten)
                    else
                      begin
                        insertmacro(pattern,mac^.buftext,mac^.buflen,mac^.fileinfo.line,
                          mac^.fileinfo.fileindex);
                      { handle empty macros }
                        if c=#0 then
                         begin
                           reload;
                           case c of
                            #26 : reload;
                            #10,
                            #13 : linebreak;
                           end;
                         end;
                      { play it again ... }
                        inc(yylexcount);
                        readtoken;
                      end;
                    exit;
                  end;
               end;
            end;
         { return token }
           goto exit_label;
         end
        else
         begin
           idtoken:=_NOID;
           case c of

             '$' :
               begin
                 readnumber;
                 token:=_INTCONST;
                 goto exit_label;
               end;

             '&' :
               begin
                 if not(m_fpc in aktmodeswitches) then
                  Illegal_Char(c)
                 else
                  begin
                    readnumber;
                    token:=_INTCONST;
                    goto exit_label;
                  end;
               end;

             '%' :
               begin
                 if (m_tp in aktmodeswitches) then
                  Illegal_Char(c)
                 else
                  begin
                    readnumber;
                    token:=_INTCONST;
                    goto exit_label;
                  end;
               end;

             '0'..'9' :
               begin
                 readnumber;
                 if (c in ['.','e','E']) then
                  begin
                  { first check for a . }
                    if c='.' then
                     begin
                       readchar;
                       { is it a .. from a range? }
                       case c of
                         '.' :
                           begin
                             readchar;
                             token:=_INTCONST;
                             nexttoken:=_POINTPOINT;
                             goto exit_label;
                           end;
                         ')' :
                           begin
                             readchar;
                             token:=_INTCONST;
                             nexttoken:=_RECKKLAMMER;
                             goto exit_label;
                           end;
                       end;
                       { insert the number after the . }
                       pattern:=pattern+'.';
                       while c in ['0'..'9'] do
                        begin
                          pattern:=pattern+c;
                          readchar;
                        end;
                      end;
                  { E can also follow after a point is scanned }
                    if c in ['e','E'] then
                     begin
                       pattern:=pattern+'E';
                       readchar;
                       if c in ['-','+'] then
                        begin
                          pattern:=pattern+c;
                          readchar;
                        end;
                       if not(c in ['0'..'9']) then
                        Illegal_Char(c);
                       while c in ['0'..'9'] do
                        begin
                          pattern:=pattern+c;
                          readchar;
                        end;
                     end;
                    token:=_REALNUMBER;
                    goto exit_label;
                  end;
                 token:=_INTCONST;
                 goto exit_label;
               end;

             ';' :
               begin
                 readchar;
                 token:=_SEMICOLON;
                 goto exit_label;
               end;

             '[' :
               begin
                 readchar;
                 token:=_LECKKLAMMER;
                 goto exit_label;
               end;

             ']' :
               begin
                 readchar;
                 token:=_RECKKLAMMER;
                 goto exit_label;
               end;

             '(' :
               begin
                 readchar;
                 case c of
                   '*' :
                     begin
                       c:=#0;{Signal skipoldtpcomment to reload a char }
                       skipoldtpcomment;
                       readtoken;
                       exit;
                     end;
                   '.' :
                     begin
                       readchar;
                       token:=_LECKKLAMMER;
                       goto exit_label;
                     end;
                 end;
                 token:=_LKLAMMER;
                 goto exit_label;
               end;

             ')' :
               begin
                 readchar;
                 token:=_RKLAMMER;
                 goto exit_label;
               end;

             '+' :
               begin
                 readchar;
                 if (c='=') and (cs_support_c_operators in aktmoduleswitches) then
                  begin
                    readchar;
                    token:=_PLUSASN;
                    goto exit_label;
                  end;
                 token:=_PLUS;
                 goto exit_label;
               end;

             '-' :
               begin
                 readchar;
                 if (c='=') and (cs_support_c_operators in aktmoduleswitches) then
                  begin
                    readchar;
                    token:=_MINUSASN;
                    goto exit_label;
                  end;
                 token:=_MINUS;
                 goto exit_label;
               end;

             ':' :
               begin
                 readchar;
                 if c='=' then
                  begin
                    readchar;
                    token:=_ASSIGNMENT;
                    goto exit_label;
                  end;
                 token:=_COLON;
                 goto exit_label;
               end;

             '*' :
               begin
                 readchar;
                 if (c='=') and (cs_support_c_operators in aktmoduleswitches) then
                  begin
                    readchar;
                    token:=_STARASN;
                  end
                 else
                  if c='*' then
                   begin
                     readchar;
                     token:=_STARSTAR;
                   end
                 else
                  token:=_STAR;
                 goto exit_label;
               end;

             '/' :
               begin
                 readchar;
                 case c of
                   '=' :
                     begin
                       if (cs_support_c_operators in aktmoduleswitches) then
                        begin
                          readchar;
                          token:=_SLASHASN;
                          goto exit_label;
                        end;
                     end;
                   '/' :
                     begin
                       skipdelphicomment;
                       readtoken;
                       exit;
                     end;
                 end;
                 token:=_SLASH;
                 goto exit_label;
               end;

             '=' :
               begin
                 readchar;
                 token:=_EQUAL;
                 goto exit_label;
               end;

             '.' :
               begin
                 readchar;
                 case c of
                   '.' :
                     begin
                       readchar;
                       token:=_POINTPOINT;
                       goto exit_label;
                     end;
                   ')' :
                     begin
                       readchar;
                       token:=_RECKKLAMMER;
                       goto exit_label;
                     end;
                 end;
                 token:=_POINT;
                 goto exit_label;
               end;

             '@' :
               begin
                 readchar;
                 if c='@' then
                  begin
                    readchar;
                    token:=_DOUBLEADDR;
                  end
                 else
                  token:=_KLAMMERAFFE;
                 goto exit_label;
               end;

             ',' :
               begin
                 readchar;
                 token:=_COMMA;
                 goto exit_label;
               end;

             '''','#','^' :
               begin
                 len:=0;
                 msgwritten:=false;
                 pattern:='';
                 if c='^' then
                  begin
                    readchar;
                    c:=upcase(c);
                    if (block_type=bt_type) or
                       (lasttoken=_ID) or (lasttoken=_NIL) or
                       (lasttoken=_RKLAMMER) or (lasttoken=_RECKKLAMMER) or (lasttoken=_CARET) then
                     begin
                       token:=_CARET;
                       goto exit_label;
                     end
                    else
                     begin
                       inc(len);
                       if c<#64 then
                        pattern[len]:=chr(ord(c)+64)
                       else
                        pattern[len]:=chr(ord(c)-64);
                       readchar;
                     end;
                  end;
                 repeat
                   case c of
                     '#' :
                       begin
                         readchar; { read # }
                         if c='$' then
                           begin
                              readchar; { read leading $ }
                              asciinr:='$';
                              while (upcase(c) in ['A'..'F','0'..'9']) and (length(asciinr)<6) do
                               begin
                                 asciinr:=asciinr+c;
                                 readchar;
                               end;
                           end
                         else
                           begin
                              asciinr:='';
                              while (c in ['0'..'9']) and (length(asciinr)<6) do
                               begin
                                 asciinr:=asciinr+c;
                                 readchar;
                               end;
                           end;
                         valint(asciinr,m,code);
                         if (asciinr='') or (code<>0) or
                            (m<0) or (m>255) then
                          Message(scan_e_illegal_char_const);
                         if len<255 then
                          begin
                            inc(len);
                            pattern[len]:=chr(m);
                          end
                         else
                          begin
                            if not msgwritten then
                             begin
                               Message(scan_e_string_exceeds_255_chars);
                               msgwritten:=true;
                             end;
                          end;
                       end;
                     '''' :
                       begin
                         repeat
                           readchar;
                           case c of
                             #26 :
                               end_of_file;
                             newline :
                               Message(scan_f_string_exceeds_line);
                             '''' :
                               begin
                                 readchar;
                                 if c<>'''' then
                                  break;
                               end;
                           end;
                           if len<255 then
                            begin
                              inc(len);
                              pattern[len]:=c;
                            end
                           else
                            begin
                              if not msgwritten then
                               begin
                                 Message(scan_e_string_exceeds_255_chars);
                                 msgwritten:=true;
                               end;
                            end;
                         until false;
                       end;
                     '^' :
                       begin
                         readchar;
                         c:=upcase(c);
                         if c<#64 then
                          c:=chr(ord(c)+64)
                         else
                          c:=chr(ord(c)-64);
                         if len<255 then
                          begin
                            inc(len);
                            pattern[len]:=c;
                          end
                         else
                          begin
                            if not msgwritten then
                             begin
                               Message(scan_e_string_exceeds_255_chars);
                               msgwritten:=true;
                             end;
                          end;
                         readchar;
                       end;
                     else
                      break;
                   end;
                 until false;
                 pattern[0]:=chr(len);
               { strings with length 1 become const chars }
                 if len=1 then
                  token:=_CCHAR
                 else
                  token:=_CSTRING;
                 goto exit_label;
               end;

             '>' :
               begin
                 readchar;
                 case c of
                   '=' :
                     begin
                       readchar;
                       token:=_GTE;
                       goto exit_label;
                     end;
                   '>' :
                     begin
                       readchar;
                       token:=_OP_SHR;
                       goto exit_label;
                     end;
                   '<' :
                     begin { >< is for a symetric diff for sets }
                       readchar;
                       token:=_SYMDIF;
                       goto exit_label;
                     end;
                 end;
                 token:=_GT;
                 goto exit_label;
               end;

             '<' :
               begin
                 readchar;
                 case c of
                   '>' :
                     begin
                       readchar;
                       token:=_UNEQUAL;
                       goto exit_label;
                     end;
                   '=' :
                     begin
                       readchar;
                       token:=_LTE;
                       goto exit_label;
                     end;
                   '<' :
                     begin
                       readchar;
                       token:=_OP_SHL;
                       goto exit_label;
                     end;
                 end;
                 token:=_LT;
                 goto exit_label;
               end;

             #26 :
               begin
                 token:=_EOF;
                 checkpreprocstack;
                 goto exit_label;
               end;
             else
               Illegal_Char(c);
           end;
        end;
exit_label:
        lasttoken:=token;
      end;


    function tscannerfile.readpreproc:ttoken;
      begin
         skipspace;
         case c of
        'A'..'Z',
        'a'..'z',
    '_','0'..'9' : begin
                     preprocpat:=readid;
                     readpreproc:=_ID;
                   end;
             '}' : begin
                     readpreproc:=_END;
                   end;
             '(' : begin
                     readchar;
                     readpreproc:=_LKLAMMER;
                   end;
             ')' : begin
                     readchar;
                     readpreproc:=_RKLAMMER;
                   end;
             '+' : begin
                     readchar;
                     readpreproc:=_PLUS;
                   end;
             '-' : begin
                     readchar;
                     readpreproc:=_MINUS;
                   end;
             '*' : begin
                     readchar;
                     readpreproc:=_STAR;
                   end;
             '/' : begin
                     readchar;
                     readpreproc:=_SLASH;
                   end;
             '=' : begin
                     readchar;
                     readpreproc:=_EQUAL;
                   end;
             '>' : begin
                     readchar;
                     if c='=' then
                      begin
                        readchar;
                        readpreproc:=_GTE;
                      end
                     else
                      readpreproc:=_GT;
                   end;
             '<' : begin
                     readchar;
                     case c of
                      '>' : begin
                              readchar;
                              readpreproc:=_UNEQUAL;
                            end;
                      '=' : begin
                              readchar;
                              readpreproc:=_LTE;
                            end;
                     else   readpreproc:=_LT;
                     end;
                   end;
             #26 :
               end_of_file;
         else
          begin
            readpreproc:=_EOF;
            checkpreprocstack;
          end;
         end;
      end;


    function tscannerfile.asmgetchar : char;
      begin
         if lastasmgetchar<>#0 then
          begin
            c:=lastasmgetchar;
            lastasmgetchar:=#0;
          end
         else
          readchar;
         if in_asm_string then
           begin
             asmgetchar:=c;
             exit;
           end;
         repeat
           case c of
             '{' :
               skipcomment;
             '/' :
               begin
                  readchar;
                  if c='/' then
                   skipdelphicomment
                  else
                   begin
                     asmgetchar:='/';
                     lastasmgetchar:=c;
                     exit;
                   end;
               end;
             '(' :
               begin
                  readchar;
                  if c='*' then
                   begin
                     c:=#0;{Signal skipoldtpcomment to reload a char }
                     skipoldtpcomment;
                   end
                  else
                   begin
                     asmgetchar:='(';
                     lastasmgetchar:=c;
                     exit;
                   end;
               end;
             else
               begin
                 asmgetchar:=c;
                 exit;
               end;
           end;
         until false;
      end;

end.
{
  $Log: scanner.pas,v $
  Revision 1.1.2.21  2003/01/14 23:56:20  peter
    * fixed tw1950

  Revision 1.1.2.20  2003/01/05 16:27:58  peter
    * fixed delphi comment parsing in skipuntildirective

  Revision 1.1.2.19  2002/11/07 16:51:01  pierre
   * several memory leaks removed

  Revision 1.1.2.18  2002/11/07 12:30:02  pierre
   * dispose preprocstack even if invalid to avoid memory leaks

  Revision 1.1.2.17  2002/09/05 16:41:57  carl
    * bugfix 2072 (also fixed in main)
    * fixes for bug 1938 for m68k version

  Revision 1.1.2.16  2002/09/05 14:01:17  pierre
   * fix bug 2004 relative to macro debugging

  Revision 1.1.2.15  2002/08/06 21:23:36  florian
    + support for octal constants, they are specified by a leading &

  Revision 1.1.2.14  2002/03/01 14:39:44  peter
    * fixed // and (* parsing to not be done when already parsing a
      tp comment in skipuntildirective

  Revision 1.1.2.13  2002/03/01 12:38:55  peter
    * support // parsing in skipuntildirective

  Revision 1.1.2.12  2002/02/18 22:30:50  pierre
   * change scan_w_macro_deep_ten to an error

  Revision 1.1.2.11  2002/02/15 08:25:02  pierre
   * partial fix for bug report 1816, should become an error later

  Revision 1.1.2.10  2001/10/22 19:55:47  peter
    * give error with string constants longer than 255 chars, this is
      compatible with kylix

  Revision 1.1.2.9  2001/10/10 22:27:43  pierre
   * fix for bug 1634

  Revision 1.1.2.8  2001/09/25 16:14:38  pierre
   * remove error if last line contains delphi comment

  Revision 1.1.2.7  2001/04/13 22:15:34  peter
    * fixed comment after comment parsing in assembler blocks

  Revision 1.1.2.5  2000/12/18 18:00:54  peter
    * skipuntildirective fix

  Revision 1.1.2.4  2000/12/16 15:30:12  peter
    * fixed parsing of comments in string with skipuntildirective

  Revision 1.1.2.3  2000/11/30 00:33:34  pierre
   * fix for web bug 1229

  Revision 1.1.2.2  2000/08/12 15:29:52  peter
    * patch from Gabor for IDE to support memory stream reading

  Revision 1.1.2.1  2000/08/08 19:19:11  peter
    * only report illegal directives once

  Revision 1.1  2000/07/13 06:29:56  michael
  + Initial import

  Revision 1.116  2000/07/08 18:03:11  peter
    * undid my previous commit, because it breaks some code

  Revision 1.115  2000/07/08 16:22:30  peter
    * also support string parsing in skipuntildirective for fpc modes

  Revision 1.114  2000/06/30 20:23:38  peter
    * new message files layout with msg numbers (but still no code to
      show the number on the screen)

  Revision 1.113  2000/06/18 18:05:54  peter
    * no binary value reading with % if not fpc mode
    * extended illegal char message with the char itself (Delphi like)

  Revision 1.112  2000/06/09 21:35:37  peter
    * fixed parsing of $if preproc function

  Revision 1.111  2000/05/03 14:36:58  pierre
   * fix for tests/test/testrang.pp bug

  Revision 1.110  2000/04/08 20:18:53  michael
  * Fixed bug in readcomment that was dropping * characters

  Revision 1.109  2000/03/13 21:21:57  peter
    * ^m support also after a string

  Revision 1.108  2000/03/12 17:53:16  florian
    * very small change to scanner ...

  Revision 1.107  2000/02/29 23:59:47  pierre
   Use $GOTO ON

  Revision 1.106  2000/02/28 17:23:57  daniel
  * Current work of symtable integration committed. The symtable can be
    activated by defining 'newst', but doesn't compile yet. Changes in type
    checking and oop are completed. What is left is to write a new
    symtablestack and adapt the parser to use it.

  Revision 1.105  2000/02/09 13:23:03  peter
    * log truncated

  Revision 1.104  2000/01/30 19:28:25  peter
    * fixed filepos when eof is read, it'll now stay on the eof position

  Revision 1.103  2000/01/07 01:14:38  peter
    * updated copyright to 2000

  Revision 1.102  1999/12/02 17:34:34  peter
    * preprocessor support. But it fails on the caret in type blocks

  Revision 1.101  1999/11/15 17:52:59  pierre
    + one field added for ttoken record for operator
      linking the id to the corresponding operator token that
      can now now all be overloaded
    * overloaded operators are resetted to nil in InitSymtable
      (bug when trying to compile a uint that overloads operators twice)

  Revision 1.100  1999/11/06 14:34:26  peter
    * truncated log to 20 revs

  Revision 1.99  1999/11/03 23:44:28  peter
    * fixed comment level counting after directive

  Revision 1.98  1999/11/02 15:05:08  peter
    * fixed oldtp comment parsing

  Revision 1.97  1999/10/30 12:32:30  peter
    * fixed line counter when the first line had #10 only. This was buggy
      for both the main file as for include files

  Revision 1.96  1999/09/27 23:40:10  peter
    * fixed macro within macro endless-loop

  Revision 1.95  1999/09/03 10:02:48  peter
    * $IFNDEF is 7 chars and not 6 chars

  Revision 1.94  1999/09/02 18:47:47  daniel
    * Could not compile with TP, some arrays moved to heap
    * NOAG386BIN default for TP
    * AG386* files were not compatible with TP, fixed.

  Revision 1.93  1999/08/30 10:17:58  peter
    * fixed crash in psub
    * ansistringcompare fixed
    * support for #$0b8

  Revision 1.92  1999/08/06 13:11:44  michael
  * Removed C style comments.

  Revision 1.91  1999/08/05 16:53:11  peter
    * V_Fatal=1, all other V_ are also increased
    * Check for local procedure when assigning procvar
    * fixed comment parsing because directives
    * oldtp mode directives better supported
    * added some messages to errore.msg

  Revision 1.90  1999/08/04 13:03:05  jonas
    * all tokens now start with an underscore
    * PowerPC compiles!!

  Revision 1.89  1999/07/29 11:43:22  peter
    * always output preprocstack when unexpected eof is found
    * fixed tp7/delphi skipuntildirective parsing

  Revision 1.88  1999/07/24 11:20:59  peter
    * directives are allowed in (* *)
    * fixed parsing of (* between conditional code

}

