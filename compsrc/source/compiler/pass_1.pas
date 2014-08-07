{
    $Id: pass_1.pas,v 1.1.2.6 2002/11/15 14:10:06 pierre Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    This unit implements the first pass of the code generator

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
  {$F+}
{$endif tp}
unit pass_1;
interface

    uses
       tree;

    procedure firstpass(var p : ptree);
    function  do_firstpass(var p : ptree) : boolean;


implementation

    uses
      globtype,systems,
      cobjects,verbose,globals,
      aasm,symconst,symtable,types,
      htypechk,
      tcadd,tccal,tccnv,tccon,tcflw,
      tcinl,tcld,tcmat,tcmem,tcset,cpubase,cpuasm
{$ifdef newcg}
      ,cgbase
      ,tgcpu
{$else newcg}
      ,hcodegen
      ,tgen
{$endif}
      ;

{*****************************************************************************
                              FirstPass
*****************************************************************************}

    type
       firstpassproc = procedure(var p : ptree);

    procedure firstnothing(var p : ptree);
      begin
         if not assigned(p^.resulttype) then
           p^.resulttype:=voiddef;
      end;


    procedure firsterror(var p : ptree);
      begin
         p^.error:=true;
         codegenerror:=true;
         p^.resulttype:=generrordef;
      end;


    procedure firststatement(var p : ptree);
      begin
         { left is the next statement in the list }
         p^.resulttype:=voiddef;
         { no temps over several statements }
{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}
         { right is the statement itself calln assignn or a complex one }
         {must_be_valid:=true; obsolete PM }
         firstpass(p^.right);
         if (not (cs_extsyntax in aktmoduleswitches)) and
            assigned(p^.right^.resulttype) and
            not((p^.right^.treetype=calln) and
                (p^.right^.procdefinition^.proctypeoption=potype_constructor)) and
            (p^.right^.resulttype<>pdef(voiddef)) then
           CGMessage(cg_e_illegal_expression);
         if codegenerror then
           exit;
         p^.registers32:=p^.right^.registers32;
         p^.registersfpu:=p^.right^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.right^.registersmmx;
{$endif SUPPORT_MMX}
         { left is the next in the list }
         firstpass(p^.left);
         if codegenerror then
           exit;
         if p^.right^.registers32>p^.registers32 then
           p^.registers32:=p^.right^.registers32;
         if p^.right^.registersfpu>p^.registersfpu then
           p^.registersfpu:=p^.right^.registersfpu;
{$ifdef SUPPORT_MMX}
         if p^.right^.registersmmx>p^.registersmmx then
           p^.registersmmx:=p^.right^.registersmmx;
{$endif}
      end;


    procedure firstblock(var p : ptree);
      var
         hp : ptree;
         count : longint;
      begin
         count:=0;
         hp:=p^.left;
         while assigned(hp) do
           begin
              if cs_regalloc in aktglobalswitches then
                begin
                   { Codeumstellungen }

                   { Funktionsresultate an exit anh�ngen }
                   { this is wrong for string or other complex
                     result types !!! }
                     if ret_in_acc(procinfo^.returntype.def,
                       procinfo^.def^.proccalloptions) and
                      assigned(hp^.left) and
                      assigned(hp^.left^.right) and
                      (hp^.left^.right^.treetype=exitn) and
                      (hp^.right^.treetype=assignn) and
                      (hp^.right^.left^.treetype=funcretn) then
                      begin
                         if assigned(hp^.left^.right^.left) then
                           CGMessage(cg_n_inefficient_code)
                         else
                           begin
                              hp^.left^.right^.left:=hp^.right^.right;
                              hp^.right^.right:=nil;
                              disposetree(hp^.right);
                              hp^.right:=nil;
                           end;
                      end
                   { warning if unreachable code occurs and elimate this }
                   else if (hp^.right^.treetype in
                     [exitn,breakn,continuen,goton]) and
                     { statement node (JM) }
                     assigned(hp^.left) and
                     { kind of statement! (JM) }
                     assigned(hp^.left^.right) and
                     (hp^.left^.right^.treetype<>labeln) then
                     begin
                        { use correct line number }
                        aktfilepos:=hp^.left^.fileinfo;
                        disposetree(hp^.left);
                        hp^.left:=nil;
                        CGMessage(cg_w_unreachable_code);
                        { old lines }
                        aktfilepos:=hp^.right^.fileinfo;
                     end;
                end;
              if assigned(hp^.right) then
                begin
{$ifdef newcg}
                   tg.cleartempgen;
{$else newcg}
                   cleartempgen;
{$endif newcg}
                   codegenerror:=false;
                   firstpass(hp^.right);
                   if (not (cs_extsyntax in aktmoduleswitches)) and
                      assigned(hp^.right^.resulttype) and
                      not((hp^.right^.treetype=calln) and
                          (hp^.right^.procdefinition^.proctypeoption=potype_constructor)) and
                      (hp^.right^.resulttype<>pdef(voiddef)) then
                     CGMessage(cg_e_illegal_expression);
                   {if codegenerror then
                     exit;}
                   hp^.registers32:=hp^.right^.registers32;
                   hp^.registersfpu:=hp^.right^.registersfpu;
{$ifdef SUPPORT_MMX}
                   hp^.registersmmx:=hp^.right^.registersmmx;
{$endif SUPPORT_MMX}
                end
              else
                hp^.registers32:=0;

              if hp^.registers32>p^.registers32 then
                p^.registers32:=hp^.registers32;
              if hp^.registersfpu>p^.registersfpu then
                p^.registersfpu:=hp^.registersfpu;
{$ifdef SUPPORT_MMX}
              if hp^.registersmmx>p^.registersmmx then
                p^.registersmmx:=hp^.registersmmx;
{$endif}
              inc(count);
              hp:=hp^.left;
           end;
      end;



    procedure firstasm(var p : ptree);
      begin
        procinfo^.flags:=procinfo^.flags or pi_uses_asm;
      end;



    procedure firstpass(var p : ptree);
      const
         procedures : array[ttreetyp] of firstpassproc =
            ({$ifndef tp}@{$endif}firstadd,   {addn}
             {$ifndef tp}@{$endif}firstadd,   {muln}
             {$ifndef tp}@{$endif}firstadd,   {subn}
             {$ifndef tp}@{$endif}firstmoddiv,      {divn}
             {$ifndef tp}@{$endif}firstadd,   {symdifn}
             {$ifndef tp}@{$endif}firstmoddiv,      {modn}
             {$ifndef tp}@{$endif}firstassignment,  {assignn}
             {$ifndef tp}@{$endif}firstload, {loadn}
             {$ifndef tp}@{$endif}firstrange,       {range}
             {$ifndef tp}@{$endif}firstadd,   {ltn}
             {$ifndef tp}@{$endif}firstadd,   {lten}
             {$ifndef tp}@{$endif}firstadd,   {gtn}
             {$ifndef tp}@{$endif}firstadd,   {gten}
             {$ifndef tp}@{$endif}firstadd,   {equaln}
             {$ifndef tp}@{$endif}firstadd,   {unequaln}
             {$ifndef tp}@{$endif}firstin,     {inn}
             {$ifndef tp}@{$endif}firstadd,   {orn}
             {$ifndef tp}@{$endif}firstadd,   {xorn}
             {$ifndef tp}@{$endif}firstshlshr,      {shrn}
             {$ifndef tp}@{$endif}firstshlshr,      {shln}
             {$ifndef tp}@{$endif}firstadd,   {slashn}
             {$ifndef tp}@{$endif}firstadd,   {andn}
             {$ifndef tp}@{$endif}firstsubscript,   {subscriptn}
             {$ifndef tp}@{$endif}firstderef,       {derefn}
             {$ifndef tp}@{$endif}firstaddr, {addrn}
             {$ifndef tp}@{$endif}firstdoubleaddr,  {doubleaddrn}
             {$ifndef tp}@{$endif}firstordconst,    {ordconstn}
             {$ifndef tp}@{$endif}firsttypeconv,    {typeconvn}
             {$ifndef tp}@{$endif}firstcalln,       {calln}
             {$ifndef tp}@{$endif}firstnothing,     {callparan}
             {$ifndef tp}@{$endif}firstrealconst,   {realconstn}
             {$ifndef tp}@{$endif}firstfixconst,    {fixconstn}
             {$ifndef tp}@{$endif}firstunaryminus,  {unaryminusn}
             {$ifndef tp}@{$endif}firstasm,         {asmn}
             {$ifndef tp}@{$endif}firstvec,         {vecn}
             {$ifndef tp}@{$endif}firstpointerconst,{pointerconstn}
             {$ifndef tp}@{$endif}firststringconst, {stringconstn}
             {$ifndef tp}@{$endif}firstfuncret,     {funcretn}
             {$ifndef tp}@{$endif}firstself, {selfn}
             {$ifndef tp}@{$endif}firstnot,   {notn}
             {$ifndef tp}@{$endif}firstinline,      {inlinen}
             {$ifndef tp}@{$endif}firstniln, {niln}
             {$ifndef tp}@{$endif}firsterror,       {errorn}
             {$ifndef tp}@{$endif}firsttype, {typen}
             {$ifndef tp}@{$endif}firsthnew, {hnewn}
             {$ifndef tp}@{$endif}firsthdispose,    {hdisposen}
             {$ifndef tp}@{$endif}firstnew,   {newn}
             {$ifndef tp}@{$endif}firstsimplenewdispose, {simpledisposen}
             {$ifndef tp}@{$endif}firstsetelement,  {setelementn}
             {$ifndef tp}@{$endif}firstsetconst,    {setconstn}
             {$ifndef tp}@{$endif}firstblock,       {blockn}
             {$ifndef tp}@{$endif}firststatement,   {statementn}
             {$ifndef tp}@{$endif}firstnothing,     {loopn}
             {$ifndef tp}@{$endif}firstif,     {ifn}
             {$ifndef tp}@{$endif}firstnothing,     {breakn}
             {$ifndef tp}@{$endif}firstnothing,     {continuen}
             {$ifndef tp}@{$endif}first_while_repeat, {repeatn}
             {$ifndef tp}@{$endif}first_while_repeat, {whilen}
             {$ifndef tp}@{$endif}firstfor,   {forn}
             {$ifndef tp}@{$endif}firstexit, {exitn}
             {$ifndef tp}@{$endif}firstwith, {withn}
             {$ifndef tp}@{$endif}firstcase, {casen}
             {$ifndef tp}@{$endif}firstlabel,       {labeln}
             {$ifndef tp}@{$endif}firstgoto, {goton}
             {$ifndef tp}@{$endif}firstsimplenewdispose, {simplenewn}
             {$ifndef tp}@{$endif}firsttryexcept,   {tryexceptn}
             {$ifndef tp}@{$endif}firstraise,       {raisen}
             {$ifndef tp}@{$endif}firstnothing,     {switchesn}
             {$ifndef tp}@{$endif}firsttryfinally,  {tryfinallyn}
             {$ifndef tp}@{$endif}firston,     {onn}
             {$ifndef tp}@{$endif}firstis,     {isn}
             {$ifndef tp}@{$endif}firstas,     {asn}
             {$ifndef tp}@{$endif}firsterror,       {caretn}
             {$ifndef tp}@{$endif}firstnothing,     {failn}
             {$ifndef tp}@{$endif}firstadd,   {starstarn}
             {$ifndef tp}@{$endif}firstprocinline,  {procinlinen}
             {$ifndef tp}@{$endif}firstarrayconstruct, {arrayconstructn}
             {$ifndef tp}@{$endif}firstarrayconstructrange, {arrayconstructrangen}
             {$ifndef tp}@{$endif}firstnothing,     {nothingn}
             {$ifndef tp}@{$endif}firstloadvmt      {loadvmtn}
             );
      var
         oldcodegenerror  : boolean;
         oldlocalswitches : tlocalswitches;
         oldpos    : tfileposinfo;
{$ifdef extdebug}
         str1,str2 : string;
         oldp      : ptree;
         not_first : boolean;
{$endif extdebug}
      begin
{$ifdef extdebug}
         inc(total_of_firstpass);
         if (p^.firstpasscount>0) and only_one_pass then
           exit;
{$endif extdebug}
         oldcodegenerror:=codegenerror;
         oldpos:=aktfilepos;
         oldlocalswitches:=aktlocalswitches;
{$ifdef extdebug}
         if p^.firstpasscount>0 then
           begin
              move(p^,str1[1],sizeof(ttree));
       {$ifndef TP}
         {$ifopt H+}
           SetLength(str1,sizeof(ttree));
         {$else}
              str1[0]:=char(sizeof(ttree));
         {$endif}
       {$else}
              str1[0]:=char(sizeof(ttree));
       {$endif}
              new(oldp);
              oldp^:=p^;
              not_first:=true;
              inc(firstpass_several);
           end
         else
           not_first:=false;
{$endif extdebug}

         if not p^.error then
           begin
              codegenerror:=false;
              aktfilepos:=p^.fileinfo;
              aktlocalswitches:=p^.localswitches;
              procedures[p^.treetype](p);
              aktlocalswitches:=oldlocalswitches;
              aktfilepos:=oldpos;
              p^.error:=codegenerror;
              if codegenerror and
                 (p^.resulttype=nil) then
               p^.resulttype:=generrordef;
              codegenerror:=codegenerror or oldcodegenerror;
           end
         else
           codegenerror:=true;
{$ifdef extdebug}
         if not_first then
           begin
              { dirty trick to compare two ttree's (PM) }
              move(p^,str2[1],sizeof(ttree));
       {$ifndef TP}
         {$ifopt H+}
           SetLength(str2,sizeof(ttree));
         {$else}
              str2[0]:=char(sizeof(ttree));
         {$endif}
       {$else}
              str2[0]:=char(sizeof(ttree));
       {$endif}
              if str1<>str2 then
                begin
                   comment(v_debug,'tree changed after first counting pass '
                     +tostr(longint(p^.treetype)));
                   compare_trees(oldp,p);
                end;
              dispose(oldp);
           end;
         if count_ref then
           inc(p^.firstpasscount);
{$endif extdebug}
      end;


    function do_firstpass(var p : ptree) : boolean;
      begin
         aktexceptblock:=nil;
         codegenerror:=false;
         firstpass(p);
         do_firstpass:=codegenerror;
      end;


end.
{
  $Log: pass_1.pas,v $
  Revision 1.1.2.6  2002/11/15 14:10:06  pierre
    + added a calloptions parameter to ret_inacc and ret_in_param
      functions.
      This allows to handle win32 stdcall and m68k cdecl small records
      case.

  Revision 1.1.2.5  2002/11/15 10:50:59  pierre
   * add char array support in str, fixes bug tbs/tb0392

  Revision 1.1.2.4  2001/10/28 17:16:39  peter
    * set resulttype for nodes with errors

  Revision 1.1.2.3  2001/06/06 14:27:15  jonas
    * fixed wrong typed constant procvars in preparation of my fix which will
      disallow them in FPC mode

  Revision 1.1.2.2  2001/02/25 02:35:29  carl
  - removed some ifdef cpu

  Revision 1.1.2.1  2001/02/05 20:46:33  peter
    * fixed bug 1364

  Revision 1.1  2000/07/13 06:29:53  michael
  + Initial import

  Revision 1.115  2000/05/25 12:00:14  jonas
    * fixed unreachable code detection

  Revision 1.114  2000/02/17 14:53:42  florian
    * some updates for the newcg

  Revision 1.113  2000/02/09 13:22:55  peter
    * log truncated

  Revision 1.112  2000/01/07 01:14:28  peter
    * updated copyright to 2000

  Revision 1.111  1999/12/14 09:58:42  florian
    + compiler checks now if a goto leaves an exception block

  Revision 1.110  1999/11/30 10:40:44  peter
    + ttype, tsymlist

  Revision 1.109  1999/11/18 15:34:47  pierre
    * Notes/Hints for local syms changed to
      Set_varstate function

  Revision 1.108  1999/11/17 17:05:01  pierre
   * Notes/hints changes

  Revision 1.107  1999/10/26 12:30:43  peter
    * const parameter is now checked
    * better and generic check if a node can be used for assigning
    * export fixes
    * procvar equal works now (it never had worked at least from 0.99.8)
    * defcoll changed to linkedlist with pparaitem so it can easily be
      walked both directions

  Revision 1.106  1999/09/27 23:44:51  peter
    * procinfo is now a pointer
    * support for result setting in sub procedure

  Revision 1.105  1999/09/26 21:30:16  peter
    + constant pointer support which can happend with typecasting like
      const p=pointer(1)
    * better procvar parsing in typed consts

  Revision 1.104  1999/09/11 09:08:31  florian
    * fixed bug 596
    * fixed some problems with procedure variables and procedures of object,
      especially in TP mode. Procedure of object doesn't apply only to classes,
      it is also allowed for objects !!

  Revision 1.103  1999/08/04 00:23:09  florian
    * renamed i386asm and i386base to cpuasm and cpubase

}
