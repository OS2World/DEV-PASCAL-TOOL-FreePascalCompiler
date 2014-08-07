{
    $Id: tcflw.pas,v 1.1.2.14 2003/03/17 18:09:38 peter Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    Type checking and register allocation for nodes that influence
    the flow

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
unit tcflw;
interface

    uses
      tree;

    procedure first_while_repeat(var p : ptree);
    procedure firstif(var p : ptree);
    procedure firstfor(var p : ptree);
    procedure firstexit(var p : ptree);
    procedure firstgoto(var p : ptree);
    procedure firstlabel(var p : ptree);
    procedure firstraise(var p : ptree);
    procedure firsttryexcept(var p : ptree);
    procedure firsttryfinally(var p : ptree);
    procedure firston(var p : ptree);

var
   { the block node of the current exception block to check gotos }
   aktexceptblock : ptree;


implementation

    uses
      globtype,systems,
      cobjects,verbose,globals,
      symconst,symtable,aasm,types,htypechk,pass_1,cpubase
      ,hcodegen
      ,temp_gen
      ,tgen
      ;

{*****************************************************************************
                         First_While_RepeatN
*****************************************************************************}

    procedure first_while_repeat(var p : ptree);
      var
         old_t_times : longint;
      begin
         old_t_times:=t_times;

         { calc register weight }
         if not(cs_littlesize in aktglobalswitches ) then
           t_times:=t_times*8;
         cleartempgen;

         firstpass(p^.left);
         set_varstate(p^.left,true);
         if codegenerror then
           exit;
         if not is_boolean(p^.left^.resulttype) then
           begin
             CGMessage(type_e_mismatch);
             exit;
           end;

         p^.registers32:=p^.left^.registers32;
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}

         { loop instruction }
         if assigned(p^.right) then
           begin
              cleartempgen;
              firstpass(p^.right);
              if codegenerror then
                exit;

              if p^.registers32<p^.right^.registers32 then
                p^.registers32:=p^.right^.registers32;
              if p^.registersfpu<p^.right^.registersfpu then
                p^.registersfpu:=p^.right^.registersfpu;
{$ifdef SUPPORT_MMX}
              if p^.registersmmx<p^.right^.registersmmx then
                p^.registersmmx:=p^.right^.registersmmx;
{$endif SUPPORT_MMX}
           end;

         t_times:=old_t_times;
      end;


{*****************************************************************************
                               FirstIfN
*****************************************************************************}

    procedure firstif(var p : ptree);
      var
         old_t_times : longint;
         hp : ptree;
      begin
         old_t_times:=t_times;
         cleartempgen;
         firstpass(p^.left);
         set_varstate(p^.left,true);

         { Only check type if no error, we can't leave here because
           the p^.right also needs to be firstpassed }
         if not codegenerror then
          begin
            if not is_boolean(p^.left^.resulttype) then
              Message1(type_e_boolean_expr_expected,p^.left^.resulttype^.typename);
          end;

         p^.registers32:=p^.left^.registers32;
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}

         { determines registers weigths }
         if not(cs_littlesize in aktglobalswitches) then
           t_times:=t_times div 2;
         if t_times=0 then
           t_times:=1;

         { if path }
         if assigned(p^.right) then
           begin
              cleartempgen;
              firstpass(p^.right);

              if p^.registers32<p^.right^.registers32 then
                p^.registers32:=p^.right^.registers32;
              if p^.registersfpu<p^.right^.registersfpu then
                p^.registersfpu:=p^.right^.registersfpu;
{$ifdef SUPPORT_MMX}
              if p^.registersmmx<p^.right^.registersmmx then
                p^.registersmmx:=p^.right^.registersmmx;
{$endif SUPPORT_MMX}
           end;

         { else path }
         if assigned(p^.t1) then
           begin
              cleartempgen;
              firstpass(p^.t1);

              if p^.registers32<p^.t1^.registers32 then
                p^.registers32:=p^.t1^.registers32;
              if p^.registersfpu<p^.t1^.registersfpu then
                p^.registersfpu:=p^.t1^.registersfpu;
{$ifdef SUPPORT_MMX}
              if p^.registersmmx<p^.t1^.registersmmx then
                p^.registersmmx:=p^.t1^.registersmmx;
{$endif SUPPORT_MMX}
           end;

         { leave if we've got an error in one of the paths }

         if codegenerror then
           exit;

         if p^.left^.treetype=ordconstn then
           begin
              { optimize }
              if p^.left^.value=1 then
                begin
                   disposetree(p^.left);
                   hp:=p^.right;
                   disposetree(p^.t1);
                   { we cannot set p to nil !!! }
                   if assigned(hp) then
                     begin
                        putnode(p);
                        p:=hp;
                     end
                   else
                     begin
                        p^.left:=nil;
                        p^.t1:=nil;
                        p^.treetype:=nothingn;
                     end;
                end
              else
                begin
                   disposetree(p^.left);
                   hp:=p^.t1;
                   disposetree(p^.right);
                   { we cannot set p to nil !!! }
                   if assigned(hp) then
                     begin
                        putnode(p);
                        p:=hp;
                     end
                   else
                     begin
                        p^.left:=nil;
                        p^.right:=nil;
                        p^.treetype:=nothingn;
                     end;
                end;
           end;

         t_times:=old_t_times;
      end;


{*****************************************************************************
                               FirstFor
*****************************************************************************}

    procedure firstfor(var p : ptree);

      var
         old_t_times : longint;
         hp : ptree;
      begin
         { Calc register weight }
         old_t_times:=t_times;
         if not(cs_littlesize in aktglobalswitches) then
           t_times:=t_times*8;
         { save counter var }
         p^.t2:=getcopy(p^.left^.left);

         if p^.left^.treetype<>assignn then
           CGMessage(cg_e_illegal_expression);

         cleartempgen;
         firstpass(p^.left);
         set_varstate(p^.left,false);

         cleartempgen;
         if assigned(p^.t1) then
          begin
            firstpass(p^.t1);
            if codegenerror then
             exit;
          end;

         p^.registers32:=p^.t1^.registers32;
         p^.registersfpu:=p^.t1^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
         if p^.left^.registers32>p^.registers32 then
           p^.registers32:=p^.left^.registers32;
         if p^.left^.registersfpu>p^.registersfpu then
           p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         if p^.left^.registersmmx>p^.registersmmx then
           p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}

         { process count var }
         cleartempgen;
         firstpass(p^.t2);
         set_varstate(p^.t2,true);
         if codegenerror then
          exit;

         { Check count var, record fields are also allowed in tp7 }
         hp:=p^.t2;
         while (hp^.treetype=subscriptn) or
               ((hp^.treetype=vecn) and
                is_constintnode(hp^.right)) do
          hp:=hp^.left;
         { we need a simple loadn, but the load must be in a global symtable or
           in the same lexlevel }
         if (hp^.treetype=funcretn) or
            ((hp^.treetype=loadn) and
             (
              (hp^.symtable^.symtablelevel<=1) or
              (hp^.symtable^.symtablelevel=lexlevel)
             ) and
             not(
                 (hp^.symtableentry^.typ=varsym) and
                 (pvarsym(hp^.symtableentry)^.varspez=vs_var)
                )
            ) then
          begin
            if (hp^.treetype=loadn) and
               (hp^.symtableentry^.typ=varsym) then
              pvarsym(hp^.symtableentry)^.varstate:=vs_used;
            if (not(is_ordinal(p^.t2^.resulttype)) or is_64bitint(p^.t2^.resulttype)) then
              CGMessagePos(hp^.fileinfo,type_e_ordinal_expr_expected);
          end
         else
          CGMessagePos(hp^.fileinfo,cg_e_illegal_count_var);

         if p^.t2^.registers32>p^.registers32 then
           p^.registers32:=p^.t2^.registers32;
         if p^.t2^.registersfpu>p^.registersfpu then
           p^.registersfpu:=p^.t2^.registersfpu;
{$ifdef SUPPORT_MMX}
         if p^.t2^.registersmmx>p^.registersmmx then
           p^.registersmmx:=p^.t2^.registersmmx;
{$endif SUPPORT_MMX}

         cleartempgen;
         firstpass(p^.right);
         set_varstate(p^.right,true);
         p^.right:=gentypeconvnode(p^.right,p^.t2^.resulttype);
         cleartempgen;
         firstpass(p^.right);

         if p^.right^.registers32>p^.registers32 then
           p^.registers32:=p^.right^.registers32;
         if p^.right^.registersfpu>p^.registersfpu then
           p^.registersfpu:=p^.right^.registersfpu;
{$ifdef SUPPORT_MMX}
         if p^.right^.registersmmx>p^.registersmmx then
           p^.registersmmx:=p^.right^.registersmmx;
{$endif SUPPORT_MMX}
         { we need at least one register for comparisons PM }
         if p^.registers32=0 then
           inc(p^.registers32);
         t_times:=old_t_times;
      end;


{*****************************************************************************
                              FirstExit
*****************************************************************************}

    procedure firstexit(var p : ptree);
      var
         pt : ptree;
      begin
         p^.resulttype:=voiddef;
         if assigned(p^.left) then
           begin
              firstpass(p^.left);
              procinfo^.funcret_state:=vs_assigned;
              set_varstate(p^.left,true);
              if codegenerror then
               exit;
              { Check the 2 types }
              p^.left:=gentypeconvnode(p^.left,procinfo^.returntype.def);
              firstpass(p^.left);
              if ret_in_param(procinfo^.returntype.def,procinfo^.def^.proccalloptions) or
                 procinfo^.no_fast_exit or
                 ((procinfo^.flags and pi_uses_exceptions)<>0) then
                begin
                  pt:=genzeronode(funcretn);
                  pt^.rettype.setdef(procinfo^.returntype.def);
                  pt^.funcretprocinfo:=procinfo;
                  p^.left:=gennode(assignn,pt,p^.left);
                  firstpass(p^.left);
                end;
              p^.registers32:=p^.left^.registers32;
              p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
           end;
      end;


{*****************************************************************************
                             FirstGoto
*****************************************************************************}

    procedure firstgoto(var p : ptree);
      begin
         p^.resulttype:=voiddef;
      end;


{*****************************************************************************
                             FirstLabel
*****************************************************************************}

    procedure firstlabel(var p : ptree);
      begin
         cleartempgen;
         p^.exceptionblock:=aktexceptblock;
         firstpass(p^.left);
         p^.registers32:=p^.left^.registers32;
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
         p^.resulttype:=voiddef;
      end;


{*****************************************************************************
                             FirstRaise
*****************************************************************************}

    procedure firstraise(var p : ptree);
      begin
         p^.resulttype:=voiddef;
         if assigned(p^.left) then
           begin
              { first para must be a _class_ }
              firstpass(p^.left);
              if assigned(p^.left^.resulttype) and
                 ((p^.left^.resulttype^.deftype<>objectdef) or
                  not(pobjectdef(p^.left^.resulttype)^.is_class)) then
                CGMessage(type_e_mismatch);
              set_varstate(p^.left,true);
              if codegenerror then
               exit;
              { insert needed typeconvs for addr,frame }
              if assigned(p^.right) then
               begin
                 { addr }
                 firstpass(p^.right);
                 p^.right:=gentypeconvnode(p^.right,s32bitdef);
                 firstpass(p^.right);
                 if codegenerror then
                  exit;
                 { frame }
                 if assigned(p^.frametree) then
                  begin
                    firstpass(p^.frametree);
                    p^.frametree:=gentypeconvnode(p^.frametree,s32bitdef);
                    firstpass(p^.frametree);
                    if codegenerror then
                     exit;
                  end;
               end;
              left_right_max(p);
           end;
      end;


{*****************************************************************************
                             FirstTryExcept
*****************************************************************************}

    procedure firsttryexcept(var p : ptree);

      var
         oldexceptblock : ptree;

      begin
         cleartempgen;
         oldexceptblock:=aktexceptblock;
         aktexceptblock:=p^.left;
         firstpass(p^.left);
         aktexceptblock:=oldexceptblock;
         { on statements }
         if assigned(p^.right) then
           begin
              cleartempgen;
              oldexceptblock:=aktexceptblock;
              aktexceptblock:=p^.right;
              firstpass(p^.right);
              aktexceptblock:=oldexceptblock;
              p^.registers32:=max(p^.registers32,p^.right^.registers32);
              p^.registersfpu:=max(p^.registersfpu,p^.right^.registersfpu);
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=max(p^.registersmmx,p^.right^.registersmmx);
{$endif SUPPORT_MMX}
           end;
         { else block }
         if assigned(p^.t1) then
           begin
              oldexceptblock:=aktexceptblock;
              aktexceptblock:=p^.t1;
              firstpass(p^.t1);
              aktexceptblock:=oldexceptblock;
              p^.registers32:=max(p^.registers32,p^.t1^.registers32);
              p^.registersfpu:=max(p^.registersfpu,p^.t1^.registersfpu);
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=max(p^.registersmmx,p^.t1^.registersmmx);
{$endif SUPPORT_MMX}
           end;
      end;


{*****************************************************************************
                             FirstTryFinally
*****************************************************************************}

    procedure firsttryfinally(var p : ptree);

      var
         oldexceptblock : ptree;

      begin
         p^.resulttype:=voiddef;
{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}
         oldexceptblock:=aktexceptblock;
         aktexceptblock:=p^.left;
         firstpass(p^.left);
         aktexceptblock:=oldexceptblock;
         set_varstate(p^.left,true);
         cleartempgen;
         oldexceptblock:=aktexceptblock;
         aktexceptblock:=p^.right;
         firstpass(p^.right);
         aktexceptblock:=oldexceptblock;
         set_varstate(p^.right,true);
         if codegenerror then
           exit;
         left_right_max(p);
      end;


{*****************************************************************************
                                 FirstOn
*****************************************************************************}

    procedure firston(var p : ptree);

      var
         oldexceptblock : ptree;

      begin
         { that's really an example procedure for a firstpass :) }
         if (p^.excepttype^.deftype<>objectdef) or
           not(pobjectdef(p^.excepttype)^.is_class) then
           CGMessage(type_e_mismatch);
         cleartempgen;
         p^.resulttype:=voiddef;
         p^.registers32:=0;
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         if assigned(p^.left) then
           begin
              firstpass(p^.left);
              p^.registers32:=p^.left^.registers32;
              p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
           end;

         cleartempgen;
         if assigned(p^.right) then
           begin
              oldexceptblock:=aktexceptblock;
              aktexceptblock:=p^.right;
              firstpass(p^.right);
              aktexceptblock:=oldexceptblock;
              p^.registers32:=max(p^.registers32,p^.right^.registers32);
              p^.registersfpu:=max(p^.registersfpu,p^.right^.registersfpu);
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=max(p^.registersmmx,p^.right^.registersmmx);
{$endif SUPPORT_MMX}
           end;
      end;


end.
{
  $Log: tcflw.pas,v $
  Revision 1.1.2.14  2003/03/17 18:09:38  peter
    * don't allow var parameters as for loop counters

  Revision 1.1.2.13  2002/11/15 14:10:10  pierre
    + added a calloptions parameter to ret_inacc and ret_in_param
      functions.
      This allows to handle win32 stdcall and m68k cdecl small records
      case.

  Revision 1.1.2.12  2001/11/19 14:11:02  jonas
    * upper constant limits for "for" loops are now also converted to the
      type of the counter var

  Revision 1.1.2.11  2001/08/27 23:16:54  pierre
   * fix for bug 1485

  Revision 1.1.2.10  2001/04/22 21:27:50  pierre
   * fix a typo miss

  Revision 1.1.2.9  2001/04/21 15:33:42  peter
    * check for loadn before typecasting in fornode

  Revision 1.1.2.8  2001/03/15 00:27:02  pierre
   * mark exit arg as used: fixes bug 1438

  Revision 1.1.2.7  2001/02/25 02:35:30  carl
  - removed some ifdef cpu

  Revision 1.1.2.6  2001/02/21 21:52:10  pierre
   * fix bug 1407

  Revision 1.1.2.5  2000/09/10 21:19:04  peter
    * fixed for counter variable check again

  Revision 1.1.2.4  2000/09/03 11:31:14  peter
    * fixed previous commit

  Revision 1.1.2.2  2000/08/11 15:13:11  florian
    * fixed bug 1096 (problem with exit in $X- mode)

  Revision 1.1.2.1  2000/08/02 06:49:26  jonas
    * fixed crash when an undeclared identifier is used in a raise statement

  Revision 1.1  2000/07/13 06:29:59  michael
  + Initial import

  Revision 1.38  2000/06/02 21:14:34  pierre
   * fix for tbs/tbs0318.pp bug

  Revision 1.37  2000/04/24 11:11:50  peter
    * backtraces for exceptions are now only generated from the place of the
      exception
    * frame is also pushed for exceptions
    * raise statement enhanced with [,<frame>]

  Revision 1.36  2000/03/19 14:17:05  florian
    * crash when using exception classes without sysutils unit fixed

  Revision 1.35  2000/02/17 14:53:43  florian
    * some updates for the newcg

  Revision 1.34  2000/02/09 13:23:07  peter
    * log truncated

  Revision 1.33  2000/02/01 09:43:22  peter
    * allow funcret also as counter variable

  Revision 1.32  2000/01/07 01:14:45  peter
    * updated copyright to 2000

  Revision 1.31  1999/12/14 09:58:42  florian
    + compiler checks now if a goto leaves an exception block

  Revision 1.30  1999/12/13 11:21:24  peter
    * better position for for counter errors

  Revision 1.29  1999/12/09 23:18:05  pierre
   * no_fast_exit if procedure contains implicit termination code

  Revision 1.28  1999/12/02 17:27:56  peter
    * give error when for counter is in other lexlevel

  Revision 1.27  1999/11/30 10:40:58  peter
    + ttype, tsymlist

  Revision 1.26  1999/11/18 15:34:49  pierre
    * Notes/Hints for local syms changed to
      Set_varstate function

  Revision 1.25  1999/11/17 17:05:07  pierre
   * Notes/hints changes

  Revision 1.24  1999/11/06 14:34:30  peter
    * truncated log to 20 revs

  Revision 1.23  1999/10/05 22:01:53  pierre
   * bug exit('test') + fail for classes

  Revision 1.22  1999/10/04 20:27:41  peter
    * fixed first pass for if branches if the expression got an error

  Revision 1.20  1999/09/27 23:45:01  peter
    * procinfo is now a pointer
    * support for result setting in sub procedure

  Revision 1.19  1999/09/16 23:05:56  florian
    * m68k compiler is again compilable (only gas writer, no assembler reader)

  Revision 1.18  1999/09/16 10:44:30  pierre
   * firstexit must now set procinfo^.funcret_is_valid

  Revision 1.17  1999/08/23 23:41:45  pierre
   * for reg allocation corrected

  Revision 1.16  1999/08/05 16:53:20  peter
    * V_Fatal=1, all other V_ are also increased
    * Check for local procedure when assigning procvar
    * fixed comment parsing because directives
    * oldtp mode directives better supported
    * added some messages to errore.msg

  Revision 1.15  1999/08/04 00:23:39  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.14  1999/08/03 22:03:30  peter
    * moved bitmask constants to sets
    * some other type/const renamings

  Revision 1.13  1999/08/01 18:28:15  florian
    * modifications for the new code generator

}
