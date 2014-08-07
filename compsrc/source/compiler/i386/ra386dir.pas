{
    $Id: ra386dir.pas,v 1.4.2.2 2002/11/15 14:10:15 pierre Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    Reads inline assembler and writes the lines direct to the output

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
unit Ra386dir;

  interface

    uses
      tree;

     function assemble : ptree;

  implementation

     uses
        files,globals,scanner,aasm,cpubase,cpuasm,
        cobjects,symconst,symtable,types,verbose,
{$ifdef NEWCG}
        cgbase,
{$else}
        hcodegen,
{$endif}
        rautils,ra386;

    function assemble : ptree;

      var
         retstr,s,hs : string;
         c : char;
         ende : boolean;
         sym : psym;
         code : paasmoutput;
         i,l : longint;

       procedure writeasmline;
         var
           i : longint;
         begin
           i:=length(s);
           while (i>0) and (s[i] in [' ',#9]) do
            dec(i);
           {$ifndef TP}
             {$ifopt H+}
               setlength(s,i);
             {$else}
               s[0]:=chr(i);
             {$endif}
           {$else}
             s[0]:=chr(i);
           {$endif}
           if s<>'' then
            code^.concat(new(pai_direct,init(strpnew(s))));
            { consider it set function set if the offset was loaded }
           if assigned(procinfo^.returntype.def) and
              (pos(retstr,upper(s))>0) then
              procinfo^.funcret_state:=vs_assigned;
           s:='';
         end;

     begin
       ende:=false;
       s:='';
       if assigned(procinfo^.returntype.def) and
          is_fpu(procinfo^.returntype.def) then
         procinfo^.funcret_state:=vs_assigned;
       if assigned(procinfo^.returntype.def) and
          (procinfo^.returntype.def<>pdef(voiddef)) then
         retstr:=upper(tostr(procinfo^.return_offset)+'('+att_reg2str[procinfo^.framepointer]+')')
       else
         retstr:='';
         c:=current_scanner^.asmgetchar;
         code:=new(paasmoutput,init);
         while not(ende) do
           begin
              { wrong placement
              current_scanner^.gettokenpos; }
              case c of
                 'A'..'Z','a'..'z','_' : begin
                      current_scanner^.gettokenpos;
                      i:=0;
                      hs:='';
                      while ((ord(c)>=ord('A')) and (ord(c)<=ord('Z')))
                         or ((ord(c)>=ord('a')) and (ord(c)<=ord('z')))
                         or ((ord(c)>=ord('0')) and (ord(c)<=ord('9')))
                         or (c='_') do
                        begin
                           inc(i);
                           hs[i]:=c;
                           c:=current_scanner^.asmgetchar;
                        end;
                      {$ifndef TP}
                        {$ifopt H+}
                          setlength(hs,i);
                        {$else}
                          hs[0]:=chr(i);
                        {$endif}
                      {$else}
                         hs[0]:=chr(i);
                      {$endif}
                      if upper(hs)='END' then
                         ende:=true
                      else
                         begin
                            if c=':' then
                              begin
                                getsym(upper(hs),false);
                                if srsym<>nil then
                                  if (srsym^.typ = labelsym) then
                                    Begin
                                       hs:=plabelsym(srsym)^.lab^.name;
                                       plabelsym(srsym)^.lab^.is_set:=true;
                                    end
                                  else
                                    Message(asmr_w_using_defined_as_local);
                              end
                            else if upper(hs)='FWAIT' then
                             FwaitWarning
                            else
                            { access to local variables }
                            if assigned(aktprocsym) then
                              begin
                                 { is the last written character an special }
                                 { char ?                                   }
                                 if (s[length(s)]='%') and
                                    ret_in_acc(procinfo^.returntype.def,procinfo^.def^.proccalloptions) and
                                    ((pos('AX',upper(hs))>0) or
                                    (pos('AL',upper(hs))>0)) then
                                   procinfo^.funcret_state:=vs_assigned;
                                 if (s[length(s)]<>'%') and
                                   (s[length(s)]<>'$') and
                                   ((s[length(s)]<>'0') or (hs[1]<>'x')) then
                                   begin
                                      if assigned(aktprocsym^.definition^.localst) and
                                         (lexlevel >= normal_function_level) then
                                        sym:=aktprocsym^.definition^.localst^.search(upper(hs))
                                      else
                                        sym:=nil;
                                      if assigned(sym) then
                                        begin
                                           if (sym^.typ = labelsym) then
                                             Begin
                                                hs:=plabelsym(sym)^.lab^.name;
                                             end
                                           else if sym^.typ=varsym then
                                             begin
                                             {variables set are after a comma }
                                             {like in movl %eax,I }
                                             if pos(',',s) > 0 then
                                               pvarsym(sym)^.varstate:=vs_used
                                             else
                                             if (pos('MOV',upper(s)) > 0) and (pvarsym(sym)^.varstate=vs_declared) then
                                              Message1(sym_n_uninitialized_local_variable,hs);
                                             if (vo_is_external in pvarsym(sym)^.varoptions) then
                                               hs:=pvarsym(sym)^.mangledname
                                             else
                                               hs:='-'+tostr(pvarsym(sym)^.address)+
                                                   '('+att_reg2str[procinfo^.framepointer]+')';
                                             end
                                           else
                                           { call to local function }
                                           if (sym^.typ=procsym) and ((pos('CALL',upper(s))>0) or
                                              (pos('LEA',upper(s))>0)) then
                                             begin
                                                hs:=pprocsym(sym)^.definition^.mangledname;
                                             end;
                                        end
                                      else
                                        begin
                                           if assigned(aktprocsym^.definition^.parast) then
                                             sym:=aktprocsym^.definition^.parast^.search(upper(hs))
                                           else
                                             sym:=nil;
                                           if assigned(sym) then
                                             begin
                                                if sym^.typ=varsym then
                                                  begin
                                                     l:=pvarsym(sym)^.address;
                                                     { set offset }
                                                     inc(l,aktprocsym^.definition^.parast^.address_fixup);
                                                     hs:=tostr(l)+'('+att_reg2str[procinfo^.framepointer]+')';
                                                     if pos(',',s) > 0 then
                                                       pvarsym(sym)^.varstate:=vs_used;
                                                  end;
                                             end
                                      { I added that but it creates a problem in line.ppi
                                      because there is a local label wbuffer and
                                      a static variable WBUFFER ...
                                      what would you decide, florian ?}
                                      else

                                        begin
{$ifndef IGNOREGLOBALVAR}
                                           getsym(upper(hs),false);
                                           sym:=srsym;
                                           if assigned(sym) and (sym^.owner^.symtabletype in [unitsymtable,
                                             globalsymtable,staticsymtable]) then
                                             begin
                                                if (sym^.typ = varsym) or (sym^.typ = typedconstsym) then
                                                  begin
                                                     Message2(asmr_h_direct_global_to_mangled,hs,sym^.mangledname);
                                                     hs:=sym^.mangledname;
                                                     if sym^.typ=varsym then
                                                       inc(pvarsym(sym)^.refs);
                                                  end;
                                                { procs can be called or the address can be loaded }
                                                if (sym^.typ=procsym) and
                                                   ((pos('CALL',upper(s))>0) or (pos('LEA',upper(s))>0)) then
                                                  begin
                                                     if assigned(pprocsym(sym)^.definition^.nextoverloaded) then
                                                       Message1(asmr_w_direct_global_is_overloaded_func,hs);
                                                     Message2(asmr_h_direct_global_to_mangled,hs,sym^.mangledname);
                                                     hs:=sym^.mangledname;
                                                  end;
                                             end
                                           else
{$endif TESTGLOBALVAR}
                                           if upper(hs)='__SELF' then
                                             begin
                                                if assigned(procinfo^._class) then
                                                  hs:=tostr(procinfo^.selfpointer_offset)+
                                                      '('+att_reg2str[procinfo^.framepointer]+')'
                                                else
                                                 Message(asmr_e_cannot_use_SELF_outside_a_method);
                                             end
                                           else if upper(hs)='__RESULT' then
                                             begin
                                                if assigned(procinfo^.returntype.def) and
                                                  (procinfo^.returntype.def<>pdef(voiddef)) then
                                                  hs:=retstr
                                                else
                                                  Message(asmr_e_void_function);
                                             end
                                           else if upper(hs)='__OLDEBP' then
                                             begin
                                                { complicate to check there }
                                                { we do it: }
                                                if lexlevel>normal_function_level then
                                                  hs:=tostr(procinfo^.framepointer_offset)+
                                                    '('+att_reg2str[procinfo^.framepointer]+')'
                                                else
                                                  Message(asmr_e_cannot_use_OLDEBP_outside_nested_procedure);
                                             end;
                                           end;
                                        end;
                                   end;
                              end;
                            s:=s+hs;
                         end;
                   end;
 '{',';',#10,#13 : begin
                      if pos(retstr,s) > 0 then
                        procinfo^.funcret_state:=vs_assigned;
                     writeasmline;
                     c:=current_scanner^.asmgetchar;
                   end;
             #26 : Message(scan_f_end_of_file);
             else
               begin
                 current_scanner^.gettokenpos;
                 {$ifndef TP}
                   {$ifopt H+}
                     setlength(s,length(s)+1);
                   {$else}
                     inc(byte(s[0]));
                   {$endif}
                 {$else}
                    inc(byte(s[0]));
                 {$endif}
                 s[length(s)]:=c;
                 c:=current_scanner^.asmgetchar;
               end;
           end;
         end;
       writeasmline;
       assemble:=genasmnode(code);
     end;

end.
{
  $Log: ra386dir.pas,v $
  Revision 1.4.2.2  2002/11/15 14:10:15  pierre
    + added a calloptions parameter to ret_inacc and ret_in_param
      functions.
      This allows to handle win32 stdcall and m68k cdecl small records
      case.

  Revision 1.4.2.1  2001/03/04 02:22:21  carl
  - renamefest!

  Revision 1.1  2000/07/13 06:29:55  michael
  + Initial import

  Revision 1.30  2000/02/09 13:23:02  peter
    * log truncated

  Revision 1.29  2000/01/07 01:14:36  peter
    * updated copyright to 2000

  Revision 1.28  1999/11/30 10:40:53  peter
    + ttype, tsymlist

  Revision 1.27  1999/11/17 17:05:03  pierre
   * Notes/hints changes

  Revision 1.26  1999/11/09 23:06:46  peter
    * esi_offset -> selfpointer_offset to be newcg compatible
    * hcogegen -> cgbase fixes for newcg

  Revision 1.25  1999/11/06 14:34:24  peter
    * truncated log to 20 revs

  Revision 1.24  1999/09/27 23:44:58  peter
    * procinfo is now a pointer
    * support for result setting in sub procedure

  Revision 1.23  1999/08/04 00:23:26  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.22  1999/08/03 22:03:11  peter
    * moved bitmask constants to sets
    * some other type/const renamings

}
