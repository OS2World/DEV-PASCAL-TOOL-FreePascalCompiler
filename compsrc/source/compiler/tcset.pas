{
    $Id: tcset.pas,v 1.1.2.7 2003/01/05 18:49:48 peter Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    Type checking and register allocation for set/case nodes

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
unit tcset;
interface

    uses
      tree;

    procedure firstsetelement(var p : ptree);
    procedure firstin(var p : ptree);
    procedure firstrange(var p : ptree);
    procedure firstcase(var p : ptree);


implementation

    uses
      globtype,systems,
      cobjects,verbose,globals,
      symconst,symtable,aasm,types,
      htypechk,pass_1,
      tccnv,cpubase
{$ifdef newcg}
      ,cgbase
      ,tgcpu
{$else newcg}
      ,hcodegen
      ,tgen
{$endif newcg}
      ;

{*****************************************************************************
                           FirstSetElement
*****************************************************************************}

    procedure firstsetelement(var p : ptree);
      begin
         firstpass(p^.left);
         set_varstate(p^.left,true);
         if codegenerror then
          exit;

         if assigned(p^.right) then
          begin
            firstpass(p^.right);
            set_varstate(p^.right,true);
            if codegenerror then
             exit;
          end;

         set_location(p^.location,p^.left^.location);
         calcregisters(p,0,0,0);
         p^.resulttype:=p^.left^.resulttype;
      end;


{*****************************************************************************
                              FirstIn
*****************************************************************************}

    procedure firstin(var p : ptree);
      type
        byteset = set of byte;
      var
        t : ptree;
        pst : pconstset;

    function createsetconst(psd : psetdef) : pconstset;
      var
        pcs : pconstset;
        pes : penumsym;
        i : longint;
      begin
        new(pcs);
        case psd^.elementtype.def^.deftype of
          enumdef :
            begin
              pes:=penumdef(psd^.elementtype.def)^.firstenum;
              while assigned(pes) do
                begin
                  pcs^[pes^.value div 8]:=pcs^[pes^.value div 8] or (1 shl (pes^.value mod 8));
                  pes:=pes^.nextenum;
                end;
            end;
          orddef :
            begin
              for i:=porddef(psd^.elementtype.def)^.low to porddef(psd^.elementtype.def)^.high do
                begin
                  pcs^[i div 8]:=pcs^[i div 8] or (1 shl (i mod 8));
                end;
            end;
        end;
       createsetconst:=pcs;
      end;

      begin
         p^.location.loc:=LOC_FLAGS;
         p^.resulttype:=booldef;

         firstpass(p^.right);
         set_varstate(p^.right,true);
         if codegenerror then
          exit;

         { Convert array constructor first to set }
         if is_array_constructor(p^.right^.resulttype) then
          begin
            arrayconstructor_to_set(p^.right);
            firstpass(p^.right);
            if codegenerror then
             exit;
          end;

         { if p^.right is a typen then the def
         is in typenodetype PM }
         if p^.right^.treetype=typen then
           p^.right^.resulttype:=p^.right^.typenodetype;

         if p^.right^.resulttype^.deftype<>setdef then
           CGMessage(sym_e_set_expected);
         if codegenerror then
           exit;

         if (p^.right^.treetype=typen) then
           begin
             { we need to create a setconstn }
             pst:=createsetconst(psetdef(p^.right^.typenodetype));
             t:=gensetconstnode(pst,psetdef(p^.right^.typenodetype));
             dispose(pst);
             putnode(p^.right);
             p^.right:=t;
           end;

         firstpass(p^.left);
         set_varstate(p^.left,true);
         if codegenerror then
           exit;

         { empty set then return false }
         if not assigned(psetdef(p^.right^.resulttype)^.elementtype.def) then
          begin
            t:=genordinalconstnode(0,booldef,false);
            disposetree(p);
            firstpass(t);
            p:=t;
            exit;
          end;

         { type conversion/check }
         p^.left:=gentypeconvnode(p^.left,psetdef(p^.right^.resulttype)^.elementtype.def);
         firstpass(p^.left);
         if codegenerror then
           exit;

         { constant evaulation }
         if (p^.left^.treetype=ordconstn) and (p^.right^.treetype=setconstn) then
          begin
            t:=genordinalconstnode(byte(p^.left^.value in byteset(p^.right^.value_set^)),booldef,true);
            disposetree(p);
            firstpass(t);
            p:=t;
            exit;
          end;

         left_right_max(p);
         { this is not allways true due to optimization }
         { but if we don't set this we get problems with optimizing self code }
         if psetdef(p^.right^.resulttype)^.settype<>smallset then
           procinfo^.flags:=procinfo^.flags or pi_do_call
         else
           begin
              { a smallset needs maybe an misc. register }
              if (p^.left^.treetype<>ordconstn) and
                not(p^.right^.location.loc in [LOC_CREGISTER,LOC_REGISTER]) and
                (p^.right^.registers32<1) then
                inc(p^.registers32);
           end;
      end;


{*****************************************************************************
                              FirstRange
*****************************************************************************}

    procedure firstrange(var p : ptree);
      var
         ct : tconverttype;
      begin
         firstpass(p^.left);
         set_varstate(p^.left,true);
         firstpass(p^.right);
         set_varstate(p^.right,true);
         if codegenerror then
           exit;
         { both types must be compatible }
         if not(is_equal(p^.left^.resulttype,p^.right^.resulttype)) and
            (isconvertable(p^.left^.resulttype,p^.right^.resulttype,ct,ordconstn,false,true,nil)=0) then
           CGMessage(type_e_mismatch);
         { Check if only when its a constant set }
         if (p^.left^.treetype=ordconstn) and (p^.right^.treetype=ordconstn) then
          begin
          { upper limit must be greater or equal than lower limit }
          { not if u32bit }
            if (p^.left^.value>p^.right^.value) and
               (( p^.left^.value<0) or (p^.right^.value>=0)) then
              CGMessage(cg_e_upper_lower_than_lower);
          end;
        left_right_max(p);
        p^.resulttype:=p^.left^.resulttype;
        set_location(p^.location,p^.left^.location);
      end;


{*****************************************************************************
                              FirstCase
*****************************************************************************}

    procedure firstcase(var p : ptree);
      var
         old_t_times : longint;
         hp : ptree;
      begin
         { evalutes the case expression }
{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}
         firstpass(p^.left);
         set_varstate(p^.left,true);
         if codegenerror then
           exit;
         p^.registers32:=p^.left^.registers32;
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}

         { walk through all instructions }

         {   estimates the repeat of each instruction }
         old_t_times:=t_times;
         if not(cs_littlesize in aktglobalswitches) then
           begin
              t_times:=t_times div case_count_labels(p^.nodes);
              if t_times<1 then
                t_times:=1;
           end;
         {   first case }
         hp:=p^.right;
         while assigned(hp) do
           begin
{$ifdef newcg}
              tg.cleartempgen;
{$else newcg}
              cleartempgen;
{$endif newcg}
              firstpass(hp^.right);

              { searchs max registers }
              if hp^.right^.registers32>p^.registers32 then
                p^.registers32:=hp^.right^.registers32;
              if hp^.right^.registersfpu>p^.registersfpu then
                p^.registersfpu:=hp^.right^.registersfpu;
{$ifdef SUPPORT_MMX}
              if hp^.right^.registersmmx>p^.registersmmx then
                p^.registersmmx:=hp^.right^.registersmmx;
{$endif SUPPORT_MMX}

              hp:=hp^.left;
           end;

         { may be handle else tree }
         if assigned(p^.elseblock) then
           begin
{$ifdef newcg}
              tg.cleartempgen;
{$else newcg}
              cleartempgen;
{$endif newcg}
              firstpass(p^.elseblock);
              if codegenerror then
                exit;
              if p^.registers32<p^.elseblock^.registers32 then
                p^.registers32:=p^.elseblock^.registers32;
              if p^.registersfpu<p^.elseblock^.registersfpu then
                p^.registersfpu:=p^.elseblock^.registersfpu;
{$ifdef SUPPORT_MMX}
              if p^.registersmmx<p^.elseblock^.registersmmx then
                p^.registersmmx:=p^.elseblock^.registersmmx;
{$endif SUPPORT_MMX}
           end;
         t_times:=old_t_times;

         { there is one register required for the case expression }
         if p^.registers32<1 then p^.registers32:=1;
      end;


end.
{
  $Log: tcset.pas,v $
  Revision 1.1.2.7  2003/01/05 18:49:48  peter
    * isconvertable has extra para to check for operator or not

  Revision 1.1.2.6  2002/09/08 08:51:15  carl
    * bugfix for report 2109

  Revision 1.1.2.5  2002/09/07 11:04:29  carl
    * 2nd part of tw1996 bugfix (genordconstnode now has option to indicate if
      range must be verified), this also optimizes a bit.

  Revision 1.1.2.4  2002/04/22 10:52:56  pierre
   * fix for bug 1786

  Revision 1.1.2.3  2001/10/11 16:09:55  jonas
    * fixed small bug in previous commit

  Revision 1.1.2.2  2001/10/11 14:36:31  jonas
    * fixed internalerror(10) due to previous fpu overflow fixes
    * NOTE: from now on, the location of a binary node must now always be set
       before you call calcregisters() for it

  Revision 1.1.2.1  2001/02/25 02:35:31  carl
  - removed some ifdef cpu

  Revision 1.1  2000/07/13 06:30:00  michael
  + Initial import

  Revision 1.19  2000/02/17 14:53:43  florian
    * some updates for the newcg

  Revision 1.18  2000/02/09 13:23:08  peter
    * log truncated

  Revision 1.17  2000/01/07 01:14:47  peter
    * updated copyright to 2000

  Revision 1.16  1999/11/30 10:40:59  peter
    + ttype, tsymlist

  Revision 1.15  1999/11/18 15:34:51  pierre
    * Notes/Hints for local syms changed to
      Set_varstate function

  Revision 1.14  1999/09/27 23:45:02  peter
    * procinfo is now a pointer
    * support for result setting in sub procedure

  Revision 1.13  1999/09/07 15:01:33  pierre
   * elem in set_type did not work yet

  Revision 1.12  1999/08/04 00:23:45  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.11  1999/08/03 22:03:38  peter
    * moved bitmask constants to sets
    * some other type/const renamings

}
