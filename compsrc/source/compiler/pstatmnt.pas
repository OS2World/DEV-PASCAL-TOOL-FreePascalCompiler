{
    $Id: pstatmnt.pas,v 1.1.2.23 2003/03/17 18:10:14 peter Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    Does the parsing of the statements

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
{$ifdef FPC}
  {$goto on}
{$endif FPC}
{$ifdef DEBUG}
  {$define TEST_EXTENDED_GETCOPY}
{$endif DEBUG}

unit pstatmnt;

  interface

    uses tree;

    { reads a block }
    function block(islibrary : boolean) : ptree;

    { reads an assembler block }
    function assembler_block : ptree;

  implementation

    uses
       globtype,systems,tokens,
       strings,cobjects,globals,files,verbose,
       symconst,symtable,aasm,pass_1,types,scanner,
{$ifdef newcg}
       cgbase,
{$else}
       hcodegen,
{$endif}
       ppu
       ,pbase,pexpr,pdecl,cpubase,cpuasm
{$ifdef i386}
       ,tgen
  {$ifndef NoRa386Int}
       ,ra386int
  {$endif NoRa386Int}
  {$ifndef NoRa386Att}
       ,ra386att
  {$endif NoRa386Att}
  {$ifndef NoRa386Dir}
       ,ra386dir
  {$endif NoRa386Dir}
{$endif i386}
{$ifdef m68k}
       ,tgen
       ,rasm
{$endif m68k}
{$ifdef alpha}
       ,tgeni386  { this is a dummy!! }
{$endif alpha}
{$ifdef powerpc}
       ,tgeni386  { this is a dummy!! }
{$endif powerpc}
       ;


    const
      statement_level : longint = 0;

    function statement : ptree;forward;


    function if_statement : ptree;
      var
         ex,if_a,else_a : ptree;
      begin
         consume(_IF);
         ex:=comp_expr(true);
         consume(_THEN);
         if token<>_ELSE then
           if_a:=statement
         else
           if_a:=nil;

         if try_to_consume(_ELSE) then
            else_a:=statement
         else
           else_a:=nil;
         if_statement:=genloopnode(ifn,ex,if_a,else_a,false);
      end;

    { creates a block (list) of statements, til the next END token }
    function statements_til_end : ptree;

      var
         first,last : ptree;

      begin
         first:=nil;
         while token<>_END do
           begin
              if first=nil then
                begin
                   last:=gennode(statementn,nil,statement);
                   first:=last;
                end
              else
                begin
                   last^.left:=gennode(statementn,nil,statement);
                   last:=last^.left;
                end;
              if not try_to_consume(_SEMICOLON) then
                break;
              emptystats;
           end;
         consume(_END);
         statements_til_end:=gensinglenode(blockn,first);
      end;

    function case_statement : ptree;

      var
         { contains the label number of currently parsed case block }
         aktcaselabel : pasmlabel;
         firstlabel : boolean;
         root : pcaserecord;

         { the typ of the case expression }
         casedef : pdef;

      procedure newcaselabel(l,h : longint;first:boolean);

        var
           hcaselabel : pcaserecord;

        procedure insertlabel(var p : pcaserecord);

          begin
             if p=nil then p:=hcaselabel
             else
                if (p^._low>hcaselabel^._low) and
                   (p^._low>hcaselabel^._high) then
                  if (hcaselabel^.statement = p^.statement) and
                     (p^._low = hcaselabel^._high + 1) then
                    begin
                      p^._low := hcaselabel^._low;
                      dispose(hcaselabel);
                    end
                  else
                    insertlabel(p^.less)
                else
                  if (p^._high<hcaselabel^._low) and
                     (p^._high<hcaselabel^._high) then
                    if (hcaselabel^.statement = p^.statement) and
                       (p^._high+1 = hcaselabel^._low) then
                      begin
                        p^._high := hcaselabel^._high;
                        dispose(hcaselabel);
                      end
                    else
                      insertlabel(p^.greater)
                  else Message(parser_e_double_caselabel);
          end;

        begin
           new(hcaselabel);
           hcaselabel^.less:=nil;
           hcaselabel^.greater:=nil;
           hcaselabel^.statement:=aktcaselabel;
           hcaselabel^.firstlabel:=first;
           getlabel(hcaselabel^._at);
           hcaselabel^._low:=l;
           hcaselabel^._high:=h;
           insertlabel(root);
        end;

      var
         code,caseexpr,p,instruc,elseblock : ptree;
         hl1,hl2 : longint;
         casedeferror : boolean;
      begin
         consume(_CASE);
{$ifndef TEST_EXTENDED_GETCOPY}
         Not_supported_for_inline(_CASE);
{$endif TEST_EXTENDED_GETCOPY}
         caseexpr:=comp_expr(true);
       { determines result type }
         cleartempgen;
         do_firstpass(caseexpr);
         casedeferror:=false;
         casedef:=caseexpr^.resulttype;
         if (not assigned(casedef)) or
            not(is_ordinal(casedef) or is_64bitint(casedef)) then
          begin
            CGMessage(type_e_ordinal_expr_expected);
            { create a correct tree }
            disposetree(caseexpr);
            caseexpr:=genordinalconstnode(0,u32bitdef,false);
            { set error flag so no rangechecks are done }
            casedeferror:=true;
          end;

         consume(_OF);
         inc(statement_level);
         root:=nil;
         instruc:=nil;
         repeat
           getlabel(aktcaselabel);
           firstlabel:=true;

           { may be an instruction has more case labels }
           repeat
             p:=expr;
             cleartempgen;
             do_firstpass(p);
             hl1:=0;
             hl2:=0;
             if (p^.treetype=rangen) then
               begin
                  { type checking for case statements }
                  if is_subequal(casedef, p^.left^.resulttype) and
                     is_subequal(casedef, p^.right^.resulttype) then
                    begin
                      hl1:=get_ordinal_value(p^.left);
                      hl2:=get_ordinal_value(p^.right);
                      if hl1>hl2 then
                        CGMessage(parser_e_case_lower_less_than_upper_bound);
                      if not casedeferror then
                       begin
                         testrange(casedef,hl1);
                         testrange(casedef,hl2);
                       end;
                    end
                  else
                    CGMessage(parser_e_case_mismatch);
                  newcaselabel(hl1,hl2,firstlabel);
               end
             else
               begin
                  { type checking for case statements }
                  if not is_subequal(casedef, p^.resulttype) then
                    CGMessage(parser_e_case_mismatch);
                  hl1:=get_ordinal_value(p);
                  if not casedeferror then
                    testrange(casedef,hl1);
                  newcaselabel(hl1,hl1,firstlabel);
               end;
             disposetree(p);
             if token=_COMMA then
               consume(_COMMA)
             else
               break;
             firstlabel:=false;
           until false;
           consume(_COLON);

           { handles instruction block }
           p:=gensinglenode(labeln,statement);
           p^.labelnr:=aktcaselabel;

           { concats instruction }
           instruc:=gennode(statementn,instruc,p);

           if not((token=_ELSE) or (token=_OTHERWISE) or (token=_END)) then
             consume(_SEMICOLON);
         until (token=_ELSE) or (token=_OTHERWISE) or (token=_END);

         if (token=_ELSE) or (token=_OTHERWISE) then
           begin
              if not try_to_consume(_ELSE) then
                consume(_OTHERWISE);
              elseblock:=statements_til_end;
           end
         else
           begin
              elseblock:=nil;
              consume(_END);
           end;
         dec(statement_level);

         code:=gencasenode(caseexpr,instruc,root);

         code^.elseblock:=elseblock;

         case_statement:=code;
      end;


    function repeat_statement : ptree;

      var
         first,last,p_e : ptree;

      begin
         consume(_REPEAT);
         first:=nil;
         inc(statement_level);

         while token<>_UNTIL do
           begin
              if first=nil then
                begin
                   last:=gennode(statementn,nil,statement);
                   first:=last;
                end
              else
                begin
                   last^.left:=gennode(statementn,nil,statement);
                   last:=last^.left;
                end;
              if not try_to_consume(_SEMICOLON) then
                break;
              emptystats;
           end;
         consume(_UNTIL);
         dec(statement_level);

         first:=gensinglenode(blockn,first);
         p_e:=comp_expr(true);
         repeat_statement:=genloopnode(repeatn,p_e,first,nil,false);
      end;


    function while_statement : ptree;

      var
         p_e,p_a : ptree;

      begin
         consume(_WHILE);
         p_e:=comp_expr(true);
         consume(_DO);
         p_a:=statement;
         while_statement:=genloopnode(whilen,p_e,p_a,nil,false);
      end;


    function for_statement : ptree;

      var
         p_e,tovalue,p_a : ptree;
         backward : boolean;

      begin
         { parse loop header }
         consume(_FOR);
         p_e:=expr;
         if token=_DOWNTO then
           begin
              consume(_DOWNTO);
              backward:=true;
           end
         else
           begin
              consume(_TO);
              backward:=false;
           end;
         tovalue:=comp_expr(true);
         consume(_DO);

         { ... now the instruction }
         p_a:=statement;
         for_statement:=genloopnode(forn,p_e,tovalue,p_a,backward);
      end;


    function _with_statement : ptree;

      var
         right,p : ptree;
         i,levelcount : longint;
         withsymtable,symtab : psymtable;
         obj : pobjectdef;
{$ifdef tp}
         hp : ptree;
{$endif}
      begin
         p:=comp_expr(true);
         do_firstpass(p);
         set_varstate(p,false);
         right:=nil;
         if (not codegenerror) and
            (p^.resulttype^.deftype in [objectdef,recorddef]) then
          begin
            case p^.resulttype^.deftype of
             objectdef : begin
                           obj:=pobjectdef(p^.resulttype);
                           withsymtable:=new(pwithsymtable,init);
                           withsymtable^.symsearch:=obj^.symtable^.symsearch;
                           withsymtable^.defowner:=obj;
                           symtab:=withsymtable;
                           if (p^.treetype=loadn) and
                              (p^.symtable=aktprocsym^.definition^.localst) then
                             pwithsymtable(symtab)^.direct_with:=true;
                           {symtab^.withnode:=p; not yet allocated !! }
                           pwithsymtable(symtab)^.withrefnode:=p;
                           levelcount:=1;
                           obj:=obj^.childof;
                           while assigned(obj) do
                            begin
                              symtab^.next:=new(pwithsymtable,init);
                              symtab:=symtab^.next;
                              symtab^.symsearch:=obj^.symtable^.symsearch;
                              if (p^.treetype=loadn) and
                                 (p^.symtable=aktprocsym^.definition^.localst) then
                                pwithsymtable(symtab)^.direct_with:=true;
                              {symtab^.withnode:=p; not yet allocated !! }
                              pwithsymtable(symtab)^.withrefnode:=p;
                              symtab^.defowner:=obj;
                              obj:=obj^.childof;
                              inc(levelcount);
                            end;
                           symtab^.next:=symtablestack;
                           symtablestack:=withsymtable;
                         end;
             recorddef : begin
                           symtab:=precorddef(p^.resulttype)^.symtable;
                           levelcount:=1;
                           withsymtable:=new(pwithsymtable,init);
                           withsymtable^.symsearch:=symtab^.symsearch;
                           withsymtable^.next:=symtablestack;
                              if (p^.treetype=loadn) and
                                 (p^.symtable=aktprocsym^.definition^.localst) then
                                pwithsymtable(withsymtable)^.direct_with:=true;
                              {symtab^.withnode:=p; not yet allocated !! }
                              pwithsymtable(withsymtable)^.withrefnode:=p;
                           withsymtable^.defowner:=precorddef(p^.resulttype);
                           symtablestack:=withsymtable;
                        end;
            end;
            if token=_COMMA then
             begin
               consume(_COMMA);
               right:=_with_statement{$ifndef tp}(){$endif};
             end
            else
             begin
               consume(_DO);
               if token<>_SEMICOLON then
                right:=statement
               else
                right:=genzeronode(errorn);
             end;
            for i:=1 to levelcount do
             symtablestack:=symtablestack^.next;
            _with_statement:=genwithnode(pwithsymtable(withsymtable),p,right,levelcount);
          end
         else
          begin
            Message(parser_e_false_with_expr);
            { try to recover from error }
            if token=_COMMA then
             begin
               consume(_COMMA);
{$ifdef tp}
               hp:=_with_statement;
{$else}
               _with_statement();
{$endif}
             end
            else
             begin
               consume(_DO);
               { ignore all }
               if token<>_SEMICOLON then
                statement;
             end;
            _with_statement:=nil;
          end;
      end;


    function with_statement : ptree;
      begin
         consume(_WITH);
{$ifndef TEST_EXTENDED_GETCOPY}
         Not_supported_for_inline(_WITH);
{$endif TEST_EXTENDED_GETCOPY}
         with_statement:=_with_statement;
      end;


    function raise_statement : ptree;

      var
         p,pobj,paddr,pframe : ptree;

      begin
         pobj:=nil;
         paddr:=nil;
         pframe:=nil;
         consume(_RAISE);
         if not(token in [_ELSE,_UNTIL,_SEMICOLON,_END]) then
           begin
              { object }
              pobj:=comp_expr(true);
              if try_to_consume(_AT) then
                begin
                   paddr:=comp_expr(true);
                   if try_to_consume(_COMMA) then
                     pframe:=comp_expr(true);
                end;
           end
         else
           begin
              if (block_type<>bt_except) then
                Message(parser_e_no_reraise_possible);
           end;
         p:=genraisenode(pobj,paddr,pframe);
         raise_statement:=p;
      end;


    function try_statement : ptree;

      var
         p_try_block,p_finally_block,first,last,
         p_default,p_specific,hp : ptree;
         ot : pobjectdef;
         sym : pvarsym;
         old_block_type : tblock_type;
         exceptsymtable : psymtable;
         objname : stringid;

      begin
         procinfo^.flags:=procinfo^.flags or
           pi_uses_exceptions;

         p_default:=nil;
         p_specific:=nil;

         { read statements to try }
         consume(_TRY);
{$ifndef TEST_EXTENDED_GETCOPY}
         Not_supported_for_inline(_TRY);
{$endif TEST_EXTENDED_GETCOPY}
         first:=nil;
         inc(statement_level);

         while (token<>_FINALLY) and (token<>_EXCEPT) do
           begin
              if first=nil then
                begin
                   last:=gennode(statementn,nil,statement);
                   first:=last;
                end
              else
                begin
                   last^.left:=gennode(statementn,nil,statement);
                   last:=last^.left;
                end;
              if not try_to_consume(_SEMICOLON) then
                break;
              emptystats;
           end;
         p_try_block:=gensinglenode(blockn,first);

         if try_to_consume(_FINALLY) then
           begin
              p_finally_block:=statements_til_end;
              try_statement:=gennode(tryfinallyn,p_try_block,p_finally_block);
              dec(statement_level);

           end
         else
           begin
              consume(_EXCEPT);
              old_block_type:=block_type;
              block_type:=bt_except;
              ot:=pobjectdef(generrordef);
              p_specific:=nil;
              if token=_ON then
                { catch specific exceptions }
                begin
                   repeat
                     consume(_ON);
                     if token=_ID then
                       begin
                          objname:=pattern;
                          getsym(objname,false);
                          consume(_ID);
                          { is a explicit name for the exception given ? }
                          if try_to_consume(_COLON) then
                            begin
                               getsym(pattern,true);
                               consume(_ID);
                               if srsym^.typ=unitsym then
                                 begin
                                    consume(_POINT);
                                    getsymonlyin(punitsym(srsym)^.unitsymtable,pattern);
                                    consume(_ID);
                                 end;
                               if (srsym^.typ=typesym) and
                                 (ptypesym(srsym)^.restype.def^.deftype=objectdef) and
                                 pobjectdef(ptypesym(srsym)^.restype.def)^.is_class then
                                 begin
                                    ot:=pobjectdef(ptypesym(srsym)^.restype.def);
                                    sym:=new(pvarsym,initdef(objname,ot));
                                 end
                               else
                                 begin
                                    sym:=new(pvarsym,initdef(objname,new(perrordef,init)));
                                    if (srsym^.typ=typesym) then
                                      Message1(type_e_class_type_expected,ptypesym(srsym)^.restype.def^.typename)
                                    else
                                      Message1(type_e_class_type_expected,ot^.typename);
                                 end;
                               exceptsymtable:=new(psymtable,init(stt_exceptsymtable));
                               exceptsymtable^.insert(sym);
                               { insert the exception symtable stack }
                               exceptsymtable^.next:=symtablestack;
                               symtablestack:=exceptsymtable;
                            end
                          else
                            begin
                               { check if type is valid, must be done here because
                                 with "e: Exception" the e is not necessary }
                               if srsym=nil then
                                begin
                                  Message1(sym_e_id_not_found,objname);
                                  srsym:=generrorsym;
                                end;
                               { only exception type }
                               if srsym^.typ=unitsym then
                                 begin
                                    consume(_POINT);
                                    getsymonlyin(punitsym(srsym)^.unitsymtable,pattern);
                                    consume(_ID);
                                 end;
                               if (srsym^.typ=typesym) and
                                 (ptypesym(srsym)^.restype.def^.deftype=objectdef) and
                                 pobjectdef(ptypesym(srsym)^.restype.def)^.is_class then
                                 ot:=pobjectdef(ptypesym(srsym)^.restype.def)
                               else
                                 begin
                                    ot:=pobjectdef(generrordef);
                                    if (srsym^.typ=typesym) then
                                      Message1(type_e_class_type_expected,ptypesym(srsym)^.restype.def^.typename)
                                    else
                                      Message1(type_e_class_type_expected,ot^.typename);
                                 end;
                               exceptsymtable:=nil;
                            end;
                       end
                     else
                       consume(_ID);
                     consume(_DO);
                     hp:=gennode(onn,nil,statement);
                     if ot^.deftype=errordef then
                       begin
                          disposetree(hp);
                          hp:=genzeronode(errorn);
                       end;
                     if p_specific=nil then
                       begin
                          last:=hp;
                          p_specific:=last;
                       end
                     else
                       begin
                          last^.left:=hp;
                          last:=last^.left;
                       end;
                     { set the informations }
                     if last^.treetype = onn then
                       begin
                         last^.excepttype:=ot;
                         last^.exceptsymtable:=exceptsymtable;
                         last^.disposetyp:=dt_onn;
                       end;
                     { remove exception symtable }
                     if assigned(exceptsymtable) then
                       begin
                         dellexlevel;
                         if last^.treetype <> onn then
                           dispose(exceptsymtable,done);
                       end;
                     if not try_to_consume(_SEMICOLON) then
                        break;
                     emptystats;
                   until (token=_END) or(token=_ELSE);
                   if token=_ELSE then
                     { catch the other exceptions }
                     begin
                        consume(_ELSE);
                        p_default:=statements_til_end;
                     end
                   else
                     consume(_END);
                end
              else
                { catch all exceptions }
                begin
                   p_default:=statements_til_end;
                end;
              dec(statement_level);

              block_type:=old_block_type;
              try_statement:=genloopnode(tryexceptn,p_try_block,p_specific,p_default,false);
           end;
      end;


    function exit_statement : ptree;

      var
         p : ptree;

      begin
         consume(_EXIT);
         if try_to_consume(_LKLAMMER) then
           begin
              p:=comp_expr(true);
              consume(_RKLAMMER);
              if (block_type=bt_except) then
                Message(parser_e_exit_with_argument_not__possible);
              if procinfo^.returntype.def=pdef(voiddef) then
                Message(parser_e_void_function);
           end
         else
           p:=nil;
         p:=gensinglenode(exitn,p);
         { p^.resulttype:=procinfo^.returntype.def; }
         p^.resulttype:=voiddef;
         exit_statement:=p;
      end;


    function _asm_statement : ptree;
      var
        asmstat : ptree;
        Marker : Pai;
      begin
         Inside_asm_statement:=true;
         case aktasmmode of
           asmmode_none : ; { just be there to allow to a compile without
                              any assembler readers }
{$ifdef i386}
  {$ifndef NoRA386Att}
           asmmode_i386_att:
             asmstat:=ra386att.assemble;
  {$endif NoRA386Att}
  {$ifndef NoRA386Int}
           asmmode_i386_intel:
             asmstat:=ra386int.assemble;
  {$endif NoRA386Int}
  {$ifndef NoRA386Dir}
           asmmode_i386_direct:
             begin
               if not target_asm.allowdirect then
                 Message(parser_f_direct_assembler_not_allowed);
               if (pocall_inline in aktprocsym^.definition^.proccalloptions) then
                 Begin
                    Message1(parser_w_not_supported_for_inline,'direct asm');
                    Message(parser_w_inlining_disabled);
                    exclude(aktprocsym^.definition^.proccalloptions,pocall_inline);
                 End;
               asmstat:=ra386dir.assemble;
             end;
  {$endif NoRA386Dir}
{$endif}
{$ifdef m68k}
           asmmode_m68k_mot:
             asmstat:=rasm.assemble;
{$endif}
         else
           Message(parser_f_assembler_reader_not_supported);
         end;

         { Read first the _ASM statement }
         consume(_ASM);

{$ifndef newcg}
         { END is read }
         if try_to_consume(_LECKKLAMMER) then
           begin
              { it's possible to specify the modified registers }
              asmstat^.object_preserved:=true;
              if token<>_RECKKLAMMER then
                repeat
                { uppercase, because it's a CSTRING }
                  uppervar(pattern);
{$ifdef i386}
                  if pattern='EAX' then
                    usedinproc:=usedinproc or ($80 shr byte(R_EAX))
                  else if pattern='EBX' then
                    usedinproc:=usedinproc or ($80 shr byte(R_EBX))
                  else if pattern='ECX' then
                    usedinproc:=usedinproc or ($80 shr byte(R_ECX))
                  else if pattern='EDX' then
                    usedinproc:=usedinproc or ($80 shr byte(R_EDX))
                  else if pattern='ESI' then
                    begin
                       usedinproc:=usedinproc or ($80 shr byte(R_ESI));
                       asmstat^.object_preserved:=false;
                    end
                  else if pattern='EDI' then
                    usedinproc:=usedinproc or ($80 shr byte(R_EDI))
{$endif i386}
{$ifdef m68k}
                  if pattern='D0' then
                    usedinproc:=usedinproc +[R_D0]
                  else if pattern='D1' then
                    usedinproc:=usedinproc + [R_D1]
                  else if pattern='D2' then
                    usedinproc:=usedinproc + [R_D2]
                  else if pattern='D3' then
                    usedinproc:=usedinproc + [R_D3]
                  else if pattern='D4' then
                    usedinproc:=usedinproc + [R_D4]
                  else if pattern='D5' then
                    usedinproc:=usedinproc + [R_D5]
                  else if pattern='D6' then
                    usedinproc:=usedinproc + [R_D6]
                  else if pattern='D7' then
                    usedinproc:=usedinproc + [R_D7]
                  else if pattern='A0' then
                    usedinproc:=usedinproc + [R_A0]
                  else if pattern='A1' then
                    usedinproc:=usedinproc + [R_A1]
                  else if pattern='A2' then
                    usedinproc:=usedinproc + [R_A2]
                  else if pattern='A3' then
                    usedinproc:=usedinproc + [R_A3]
                  else if pattern='A4' then
                    usedinproc:=usedinproc + [R_A4]
                  else if pattern='A5' then
                    usedinproc:=usedinproc + [R_A5]
{$endif m68k}
                  else consume(_RECKKLAMMER);
                  consume(_CSTRING);
                  if not try_to_consume(_COMMA) then
                    break;
                until false;
              consume(_RECKKLAMMER);
           end
{$ifdef i386}
         else usedinproc:=$ff;
{$else}
         else usedinproc := ALL_REGISTERS;
{$endif i386}
{$endif newcg}

{ mark the start and the end of the assembler block for the optimizer }

         If Assigned(AsmStat^.p_asm) Then
           Begin
             Marker := New(Pai_Marker, Init(AsmBlockStart));
             AsmStat^.p_asm^.Insert(Marker);
             Marker := New(Pai_Marker, Init(AsmBlockEnd));
             AsmStat^.p_asm^.Concat(Marker);
           End;
         Inside_asm_statement:=false;

         _asm_statement:=asmstat;
      end;


        function new_dispose_statement : ptree;
        var
          p,p2 : ptree;
          ht : ttoken;
          again : boolean; { dummy for do_proc_call }
          destructorname : stringid;
          sym : psym;
          classh : pobjectdef;
          pd,pd2 : pdef;
          destructorpos,storepos : tfileposinfo;
          tt : ttreetyp;
        begin
          ht:=token;
          if try_to_consume(_NEW) then
            tt:=hnewn
          else
            begin
                consume(_DISPOSE);
                tt:=hdisposen;
            end;
          consume(_LKLAMMER);


          p:=comp_expr(true);

          { calc return type }
          cleartempgen;
          do_firstpass(p);
          set_varstate(p,tt=hdisposen);

  {var o:Pobject;
           begin
               new(o,init);     (*Also a valid new statement*)
           end;}

          if try_to_consume(_COMMA) then
            begin
                   { extended syntax of new and dispose }
                   { function styled new is handled in factor }
                   { destructors have no parameters }
                   destructorname:=pattern;
                   destructorpos:=tokenpos;
                   consume(_ID);

                   pd:=p^.resulttype;
                   if pd=nil then
                    pd:=generrordef;
                   pd2:=pd;
                   if (pd^.deftype<>pointerdef) then
                     begin
                        Message1(type_e_pointer_type_expected,pd^.typename);
                        p:=factor(false);
                        consume(_RKLAMMER);
                        new_dispose_statement:=genzeronode(errorn);
                        exit;
                     end;
                   { first parameter must be an object or class }
                   if ppointerdef(pd)^.pointertype.def^.deftype<>objectdef then
                     begin
                        Message(parser_e_pointer_to_class_expected);
                        new_dispose_statement:=factor(false);
                        consume_all_until(_RKLAMMER);
                        consume(_RKLAMMER);
                        exit;
                     end;
                   { check, if the first parameter is a pointer to a _class_ }
                   classh:=pobjectdef(ppointerdef(pd)^.pointertype.def);
                   if classh^.is_class then
                     begin
                        Message(parser_e_no_new_or_dispose_for_classes);
                        new_dispose_statement:=factor(false);
                        consume_all_until(_RKLAMMER);
                        consume(_RKLAMMER);
                        exit;
                     end;
                   { search cons-/destructor, also in parent classes }
                   storepos:=tokenpos;
                   tokenpos:=destructorpos;
                   sym:=search_class_member(classh,destructorname);
                   tokenpos:=storepos;

                   { the second parameter of new/dispose must be a call }
                   { to a cons-/destructor                              }
                   if (not assigned(sym)) or (sym^.typ<>procsym) then
                         begin
                            if tt=hnewn then
                             Message(parser_e_expr_have_to_be_constructor_call)
                            else
                             Message(parser_e_expr_have_to_be_destructor_call);
                            new_dispose_statement:=genzeronode(errorn);
                         end
                   else
                         begin
                           p2:=gensinglenode(tt,p);
                           if ht=_NEW then
                             begin
                               { Constructors can take parameters.}
                               p2^.resulttype:=ppointerdef(pd)^.pointertype.def;
                               do_member_read(false,sym,p2,pd,again);
                             end
                           else
                             begin
                               if (m_tp in aktmodeswitches) then
                                 begin
                                   { Constructors can take parameters.}
                                   p2^.resulttype:=ppointerdef(pd)^.pointertype.def;
                                   do_member_read(false,sym,p2,pd,again);
                                 end
                               else
                                 begin
                                   p2:=genmethodcallnode(pprocsym(sym),srsymtable,p2);
                                   { support dispose(p,done()); }
                                   if try_to_consume(_LKLAMMER) then
                                     begin
                                       if not try_to_consume(_RKLAMMER) then
                                         begin
                                           Message(parser_e_no_paras_for_destructor);
                                           consume_all_until(_RKLAMMER);
                                           consume(_RKLAMMER);
                                         end;
                                     end;
                                 end;
                             end;

                           { we need the real called method }
                           cleartempgen;
                           do_firstpass(p2);
                           if p2^.treetype<>calln then
                              begin
                                if tt=hnewn then
                                  CGMessage(parser_e_expr_have_to_be_constructor_call)
                                else
                                  CGMessage(parser_e_expr_have_to_be_destructor_call);
                              end;


                           if not codegenerror then
                            begin
                              if (ht=_NEW) and (p2^.procdefinition^.proctypeoption<>potype_constructor) then
                                Message(parser_e_expr_have_to_be_constructor_call);
                              if (ht=_DISPOSE) and (p2^.procdefinition^.proctypeoption<>potype_destructor) then
                                Message(parser_e_expr_have_to_be_destructor_call);

                              if ht=_NEW then
                               begin
                                 p2:=gennode(assignn,getcopy(p),gensinglenode(newn,p2));
                                 p2^.right^.resulttype:=pd2;
                               end;
                            end;
                           new_dispose_statement:=p2;
                         end;
            end
          else
            begin
               if p^.resulttype=nil then
                p^.resulttype:=generrordef;
               if (p^.resulttype^.deftype<>pointerdef) then
                 Begin
                    Message1(type_e_pointer_type_expected,p^.resulttype^.typename);
                    new_dispose_statement:=genzeronode(errorn);
                 end
               else
                 begin
                    if (ppointerdef(p^.resulttype)^.pointertype.def^.deftype=objectdef) and
                       (oo_has_vmt in pobjectdef(ppointerdef(p^.resulttype)^.pointertype.def)^.objectoptions) then
                      Message(parser_w_use_extended_syntax_for_objects);
                    if (ppointerdef(p^.resulttype)^.pointertype.def^.deftype=orddef) and
                       (porddef(ppointerdef(p^.resulttype)^.pointertype.def)^.typ=uvoid) then
                      if (m_tp in aktmodeswitches) or
                         (m_delphi in aktmodeswitches) then
                       Message(parser_w_no_new_dispose_on_void_pointers)
                      else
                       Message(parser_e_no_new_dispose_on_void_pointers);

                     case ht of
                        _NEW : new_dispose_statement:=gensinglenode(simplenewn,p);
                        _DISPOSE : new_dispose_statement:=gensinglenode(simpledisposen,p);
                     end;
                 end;
            end;
          consume(_RKLAMMER);
      end;


    function statement_block(starttoken : ttoken) : ptree;

      var
         first,last : ptree;
         filepos : tfileposinfo;

      begin
         first:=nil;
         filepos:=tokenpos;
         consume(starttoken);
         inc(statement_level);

         while not(token in [_END,_FINALIZATION]) do
           begin
              if first=nil then
                begin
                   last:=gennode(statementn,nil,statement);
                   first:=last;
                end
              else
                begin
                   last^.left:=gennode(statementn,nil,statement);
                   last:=last^.left;
                end;
              if (token in [_END,_FINALIZATION]) then
                break
              else
                begin
                   { if no semicolon, then error and go on }
                   if token<>_SEMICOLON then
                     begin
                        consume(_SEMICOLON);
                        consume_all_until(_SEMICOLON);
                     end;
                   consume(_SEMICOLON);
                end;
              emptystats;
           end;

         { don't consume the finalization token, it is consumed when
           reading the finalization block, but allow it only after
           an initalization ! }
         if (starttoken<>_INITIALIZATION) or (token<>_FINALIZATION) then
           consume(_END);

         dec(statement_level);

         last:=gensinglenode(blockn,first);
         set_tree_filepos(last,filepos);
         statement_block:=last;
      end;


    function statement : ptree;

      var
         p : ptree;
         code : ptree;
         labelnr : pasmlabel;
         filepos : tfileposinfo;
         sr : plabelsym;

      label
         ready;

      begin
         filepos:=tokenpos;
         case token of
            _GOTO : begin
                       if not(cs_support_goto in aktmoduleswitches)then
                        Message(sym_e_goto_and_label_not_supported);
                       consume(_GOTO);
                       if (token<>_INTCONST) and (token<>_ID) then
                         begin
                            Message(sym_e_label_not_found);
                            code:=genzeronode(errorn);
                         end
                       else
                         begin
                            getsym(pattern,true);
                            consume(token);
                            if srsym^.typ<>labelsym then
                              begin
                                 Message(sym_e_id_is_no_label_id);
                                 code:=genzeronode(errorn);
                              end
                            else
                              begin
                                code:=genlabelnode(goton,plabelsym(srsym)^.lab);
                                code^.labsym:=plabelsym(srsym);
                                { set flag that this label is used }
                                plabelsym(srsym)^.used:=true;
                              end;
                         end;
                    end;
            _BEGIN : code:=statement_block(_BEGIN);
            _IF    : code:=if_statement;
            _CASE  : code:=case_statement;
            _REPEAT : code:=repeat_statement;
            _WHILE : code:=while_statement;
            _FOR : code:=for_statement;
            _NEW,_DISPOSE : code:=new_dispose_statement;

            _WITH : code:=with_statement;
            _TRY : code:=try_statement;
            _RAISE : code:=raise_statement;
            { semicolons,else until and end are ignored }
            _SEMICOLON,
            _ELSE,
            _UNTIL,
            _END:
              code:=genzeronode(niln);
            _FAIL : begin
                       { internalerror(100); }
                       if (aktprocsym^.definition^.proctypeoption<>potype_constructor) then
                        Message(parser_e_fail_only_in_constructor);
                       consume(_FAIL);
                       code:=genzeronode(failn);
                    end;
            _EXIT : code:=exit_statement;
            _ASM : begin
                      code:=_asm_statement;
                   end;
            _EOF : begin
                     Message(scan_f_end_of_file);
                   end;
         else
           begin
              if (token in [_INTCONST,_ID]) then
                begin
                   getsym(pattern,true);
                   lastsymknown:=true;
                   lastsrsym:=srsym;
                   { it is NOT necessarily the owner
                     it can be a withsymtable !!! }
                   lastsrsymtable:=srsymtable;
                   if assigned(srsym) and (srsym^.typ=labelsym) then
                     begin
                        consume(token);
                        consume(_COLON);
                        { we must preserve srsym to set code later }
                        sr:=plabelsym(srsym);
                        if sr^.defined then
                          Message(sym_e_label_already_defined);
                        sr^.defined:=true;

                        { statement modifies srsym }
                        labelnr:=sr^.lab;
                        lastsymknown:=false;
                        { the pointer to the following instruction }
                        { isn't a very clean way                   }
                        code:=gensinglenode(labeln,statement{$ifndef tp}(){$endif});
                        code^.labelnr:=labelnr;
                        sr^.code:=code;
                        { sorry, but there is a jump the easiest way }
                        goto ready;
                     end;
                end;
              p:=expr;
              if not(p^.treetype in [calln,assignn,breakn,inlinen,
                continuen, nothingn]) then
                Message(cg_e_illegal_expression);
              { specify that we don't use the value returned by the call }
              { Question : can this be also improtant
                for inlinen ??
                it is used for :
                 - dispose of temp stack space
                 - dispose on FPU stack }
              if p^.treetype=calln then
                p^.return_value_used:=false;
              code:=p;
           end;
         end;
         ready:
         if assigned(code) then
          set_tree_filepos(code,filepos);
         statement:=code;
      end;

    function block(islibrary : boolean) : ptree;

      var
         funcretsym : pfuncretsym;
         storepos : tfileposinfo;

      begin
         { do we have an assembler block without the po_assembler?
           we should allow this for Delphi compatibility (PFV) }
         if (token=_ASM) and (m_delphi in aktmodeswitches) then
           begin
             include(aktprocsym^.definition^.procoptions,po_assembler);
             block:=assembler_block;
             exit;
           end;
         if procinfo^.returntype.def<>pdef(voiddef) then
           begin
              { if the current is a function aktprocsym is non nil }
              { and there is a local symtable set }
              storepos:=tokenpos;
              tokenpos:=aktprocsym^.fileinfo;
              funcretsym:=new(pfuncretsym,init(aktprocsym^.name,procinfo));
              { insert in local symtable }
              symtablestack^.insert(funcretsym);
              tokenpos:=storepos;
              if ret_in_acc(procinfo^.returntype.def,procinfo^.def^.proccalloptions) or
{$ifdef m68k}
                 (pocall_cdecl in aktprocsym^.definition^.proccalloptions) or
{$endif m68k}
                 (procinfo^.returntype.def^.deftype=floatdef) then
                procinfo^.return_offset:=-funcretsym^.address;
              procinfo^.funcretsym:=funcretsym;
              { insert result also if support is on }
              if (m_result in aktmodeswitches) then
               begin
                 procinfo^.resultfuncretsym:=new(pfuncretsym,init('RESULT',procinfo));
                 symtablestack^.insert(procinfo^.resultfuncretsym);
               end;
           end;
         read_declarations(islibrary);

         { temporary space is set, while the BEGIN of the procedure }
         if (symtablestack^.symtabletype=localsymtable) then
           procinfo^.firsttemp_offset := -symtablestack^.datasize
         else
           procinfo^.firsttemp_offset := 0;

         { space for the return value }
         { !!!!!   this means that we can not set the return value
         in a subfunction !!!!! }
         { because we don't know yet where the address is }
         if procinfo^.returntype.def<>pdef(voiddef) then
           begin
              if ret_in_acc(procinfo^.returntype.def,procinfo^.def^.proccalloptions) or
{$ifdef m68k}
                 (pocall_cdecl in aktprocsym^.definition^.proccalloptions) or
{$endif m68k}
                 (procinfo^.returntype.def^.deftype=floatdef) then
              { if (procinfo^.retdef^.deftype=orddef) or
                 (procinfo^.retdef^.deftype=pointerdef) or
                 (procinfo^.retdef^.deftype=enumdef) or
                 (procinfo^.retdef^.deftype=procvardef) or
                 (procinfo^.retdef^.deftype=floatdef) or
                 (
                   (procinfo^.retdef^.deftype=setdef) and
                   (psetdef(procinfo^.retdef)^.settype=smallset)
                 ) then  }
                begin
                   { the space has been set in the local symtable }
                   procinfo^.return_offset:=-funcretsym^.address;
                   if ((procinfo^.flags and pi_operator)<>0) and
                     assigned(opsym) then
                     {opsym^.address:=procinfo^.para_offset; is wrong PM }
                     opsym^.address:=-procinfo^.return_offset;
                   { eax is modified by a function }
{$ifndef newcg}
{$ifdef i386}
                   usedinproc:=usedinproc or ($80 shr byte(R_EAX));

                   if is_64bitint(procinfo^.returntype.def) then
                     usedinproc:=usedinproc or ($80 shr byte(R_EDX))
{$else}
                   usedinproc:=usedinproc + [accumulator];
{$ifdef m68k}
                   if (procinfo^.returntype.def^.deftype=pointerdef) and
                      (pocall_cdecl in aktprocsym^.definition^.proccalloptions)
                   then
                     usedinproc:=usedinproc + [R_A0];
                   if ret_in_param(procinfo^.returntype.def,procinfo^.def^.proccalloptions) and
                      (pocall_cdecl in aktprocsym^.definition^.proccalloptions)
                   then
                     usedinproc:=usedinproc + [R_A1];
{$endif m68k}

                   if is_64bitint(procinfo^.returntype.def) then
                     usedinproc:=usedinproc  + [scratch_reg];
{$endif i386}
{$endif newcg}
                end;
           end;

         {Unit initialization?.}
         if (lexlevel=unit_init_level) and (current_module^.is_unit)
            or islibrary then
           begin
             if (token=_END) then
                begin
                   consume(_END);
                   { We need at least a node, else the entry/exit code is not
                     generated and thus no PASCALMAIN symbol which we need (PFV) }
                   if islibrary then
                    block:=genzeronode(nothingn)
                   else
                    block:=nil;
                end
              else
                begin
                   if token=_INITIALIZATION then
                     begin
                        current_module^.flags:=current_module^.flags or uf_init;
                        block:=statement_block(_INITIALIZATION);
                     end
                   else if (token=_FINALIZATION) then
                     begin
                        if (current_module^.flags and uf_finalize)<>0 then
                          block:=statement_block(_FINALIZATION)
                        else
                          begin
                          { can we allow no INITIALIZATION for DLL ??
                            I think it should work PM }
                             block:=nil;
                             exit;
                          end;
                     end
                   else
                     begin
                        current_module^.flags:=current_module^.flags or uf_init;
                        block:=statement_block(_BEGIN);
                     end;
                end;
            end
         else
            block:=statement_block(_BEGIN);
      end;

    function assembler_block : ptree;

      begin
         read_declarations(false);
         { temporary space is set, while the BEGIN of the procedure }
         if symtablestack^.symtabletype=localsymtable then
           procinfo^.firsttemp_offset := -symtablestack^.datasize
         else
           procinfo^.firsttemp_offset := 0;

         { assembler code does not allocate }
         { space for the return value       }
          if procinfo^.returntype.def<>pdef(voiddef) then
           begin
              if ret_in_acc(procinfo^.returntype.def,procinfo^.def^.proccalloptions) then
                begin
                   { in assembler code the result should be directly in %eax
                   procinfo^.retoffset:=procinfo^.firsttemp-procinfo^.retdef^.size;
                   procinfo^.firsttemp:=procinfo^.retoffset;                 }

{$ifndef newcg}
{$ifdef i386}
                   usedinproc:=usedinproc or ($80 shr byte(R_EAX))
{$else}
                   usedinproc:=usedinproc + [accumulator];
{$ifdef m68k}
                   if procinfo^.returntype.def^.deftype=pointerdef then
                     usedinproc:=usedinproc + [R_A0];
{$endif m68k}
{$endif i386}
{$endif newcg}
                end
              {
              else if not is_fpu(procinfo^.retdef) then
               should we allow assembler functions of big elements ?
                YES (FK)!!
               Message(parser_e_asm_incomp_with_function_return);
              }
            end;
           { set the framepointer to esp for assembler functions }
           { but only if the are no local variables           }
           { added no parameter also (PM)                       }
           { disable for methods, because self pointer is expected }
           { at -8(%ebp) (JM)                                      }
           { why if se use %esp then self is still at the correct address PM }
           if {not(assigned(procinfo^._class)) and}
              (po_assembler in aktprocsym^.definition^.procoptions) and
              (aktprocsym^.definition^.localst^.datasize=0) and
              (aktprocsym^.definition^.parast^.datasize=0) and
              not(ret_in_param(aktprocsym^.definition^.rettype.def,
                  aktprocsym^.definition^.proccalloptions)) then
             begin
               procinfo^.framepointer:=stack_pointer;
               { set the right value for parameters }
               dec(aktprocsym^.definition^.parast^.address_fixup,target_os.size_of_pointer);
               dec(procinfo^.para_offset,target_os.size_of_pointer);
             end;
          { force the asm statement }
            if token<>_ASM then
             consume(_ASM);
            procinfo^.Flags := procinfo^.Flags Or pi_is_assembler;
            assembler_block:=_asm_statement;
          { becuase the END is already read we need to get the
            last_endtoken_filepos here (PFV) }
            last_endtoken_filepos:=tokenpos;
          end;

end.
{
  $Log: pstatmnt.pas,v $
  Revision 1.1.2.23  2003/03/17 18:10:14  peter
    * allow more end tokens instead of only semicolon after inherited

  Revision 1.1.2.22  2003/01/06 21:23:49  peter
    * allow else after raise

  Revision 1.1.2.21  2002/12/27 14:38:54  jonas
    * allow nothingn as statement to fix assert() problem

  Revision 1.1.2.20  2002/11/15 14:10:07  pierre
    + added a calloptions parameter to ret_inacc and ret_in_param
      functions.
      This allows to handle win32 stdcall and m68k cdecl small records
      case.

  Revision 1.1.2.19  2002/10/29 09:48:37  pierre
   * fix crash report as webbug 2174

  Revision 1.1.2.18  2002/10/17 20:41:16  pierre
   * case with and try statements are not ready for inline yet

  Revision 1.1.2.17  2002/10/07 19:43:50  pierre
   * m68k cdecl'ared function with structured results have address of result in A1

  Revision 1.1.2.16  2002/09/26 20:19:09  carl
    * int_to_bool fixes for both i386 and m68k (direct typecasts
      of var parameters to bool, no longer allowed :()..
    * bugfix with a0 usage
    - now 68000 mode will not fail to compile if data elements are greater than 32K, only
      a warning will be emitted

  Revision 1.1.2.15  2002/09/26 08:03:25  pierre
   * Set return values into A0 also if it is a pointer for m68k cg

  Revision 1.1.2.14  2002/09/07 11:04:31  carl
    * 2nd part of tw1996 bugfix (genordconstnode now has option to indicate if
      range must be verified), this also optimizes a bit.

  Revision 1.1.2.13  2002/03/07 17:35:28  carl
  * 1850 bugfix

  Revision 1.1.2.12  2001/09/17 23:45:05  pierre
   + support more registers at asm end['xx']

  Revision 1.1.2.11  2001/05/15 18:42:47  carl
  - removed unused ifdef

  Revision 1.1.2.10  2001/04/21 00:28:09  carl
  * m68k updates

  Revision 1.1.2.9  2001/03/08 03:24:28  carl
  - renamed back to ra386int usage

  Revision 1.1.2.8  2001/03/04 02:28:19  carl
  +renamed units which removed some defines once again.

  Revision 1.1.2.7  2001/02/25 02:35:30  carl
  - removed some ifdef cpu

  Revision 1.1.2.6  2001/02/24 23:03:15  carl
  make it compile under TP

  Revision 1.1.2.5  2001/02/23 10:05:17  pierre
   * first bunch of m68k cpu updates

  Revision 1.1.2.4  2000/11/27 16:05:06  jonas
    * fix for web bug 1251 (example 1)

  Revision 1.1.2.3  2000/11/22 22:42:42  peter
    * fixed crash with exception without sysutils

  Revision 1.1.2.2  2000/09/26 08:49:03  pierre
   * avoid memory leak with raisen

  Revision 1.1.2.1  2000/08/11 15:15:43  florian
    * fixed bug 1096 (problem with exit in $X- mode)

  Revision 1.1  2000/07/13 06:29:54  michael
  + Initial import

  Revision 1.131  2000/06/30 22:15:39  peter
    * fixed internalerror 2002 when case expr is not correct, by creating
      a temp correct case expression

  Revision 1.130  2000/05/04 12:59:10  pierre
   * bug found by Kovacs Attila Zoltan corrected

  Revision 1.129  2000/04/29 12:50:14  peter
    * support asm block without assembler directive for -Sd

  Revision 1.128  2000/04/24 11:11:50  peter
    * backtraces for exceptions are now only generated from the place of the
      exception
    * frame is also pushed for exceptions
    * raise statement enhanced with [,<frame>]

  Revision 1.127  2000/03/19 14:17:05  florian
    * crash when using exception classes without sysutils unit fixed

  Revision 1.126  2000/03/19 11:16:44  peter
    * check for unknown id in on exception

  Revision 1.125  2000/03/16 15:12:06  pierre
   assembler method code does not need ebp framepointer

  Revision 1.124  2000/03/14 16:37:25  pierre
   * destructor can have args in TP mode only (bug825 and 839)

  Revision 1.123  2000/02/29 23:59:47  pierre
   Use $GOTO ON

  Revision 1.122  2000/02/09 13:22:59  peter
    * log truncated

  Revision 1.121  2000/01/23 16:33:49  peter
    * fixed destructor parsing with preprocessor things
    * support dipsoe(p,done())
    * fixed constructor message with dispose(p,<nonexist>)

  Revision 1.120  2000/01/16 22:17:12  peter
    * renamed call_offset to para_offset

  Revision 1.119  2000/01/12 10:30:50  peter
    * fixed library with only end.

  Revision 1.118  2000/01/07 01:14:31  peter
    * updated copyright to 2000

  Revision 1.117  1999/12/22 01:01:52  peter
    - removed freelabel()
    * added undefined label detection in internal assembler, this prevents
      a lot of ld crashes and wrong .o files
    * .o files aren't written anymore if errors have occured
    * inlining of assembler labels is now correct

  Revision 1.116  1999/12/14 09:58:42  florian
    + compiler checks now if a goto leaves an exception block

  Revision 1.115  1999/12/01 22:43:17  peter
    * fixed sigsegv with casedef=nil

  Revision 1.114  1999/12/01 12:42:32  peter
    * fixed bug 698
    * removed some notes about unused vars

  Revision 1.113  1999/11/30 10:40:45  peter
    + ttype, tsymlist

  Revision 1.112  1999/11/20 01:19:10  pierre
    * DLL index used for win32 target with DEF file
    + DLL initialization/finalization support

  Revision 1.111  1999/11/18 15:34:48  pierre
    * Notes/Hints for local syms changed to
      Set_varstate function

  Revision 1.110  1999/11/17 17:05:02  pierre
   * Notes/hints changes

  Revision 1.109  1999/11/15 22:00:48  peter
    * labels used but not defined give error instead of warning, the warning
      is now only with declared but not defined and not used.

  Revision 1.108  1999/11/10 00:24:02  pierre
   * more browser details

  Revision 1.107  1999/11/09 13:02:46  peter
    * fixed 'raise end;'

  Revision 1.106  1999/11/06 14:34:23  peter
    * truncated log to 20 revs

  Revision 1.105  1999/10/22 10:39:35  peter
    * split type reading from pdecl to ptype unit
    * parameter_dec routine is now used for procedure and procvars

  Revision 1.104  1999/10/14 14:57:54  florian
    - removed the hcodegen use in the new cg, use cgbase instead

  Revision 1.103  1999/09/27 23:44:56  peter
    * procinfo is now a pointer
    * support for result setting in sub procedure

  Revision 1.102  1999/09/16 23:05:54  florian
    * m68k compiler is again compilable (only gas writer, no assembler reader)

}
