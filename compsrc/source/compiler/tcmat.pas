{
    $Id: tcmat.pas,v 1.1.2.11 2002/09/07 11:04:29 carl Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    Type checking and register allocation for math nodes

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
unit tcmat;
interface

    uses
      tree;

    procedure firstmoddiv(var p : ptree);
    procedure firstshlshr(var p : ptree);
    procedure firstunaryminus(var p : ptree);
    procedure firstnot(var p : ptree);


implementation

    uses
      globtype,systems,tokens,
      cobjects,verbose,globals,
      symconst,symtable,aasm,types,
      htypechk,pass_1,cpubase,
{$ifdef newcg}
      cgbase,
{$else newcg}
      hcodegen,
{$endif newcg}
      { for isbinaryoverloaded function }
      tcadd;

{*****************************************************************************
                             FirstModDiv
*****************************************************************************}

    procedure firstmoddiv(var p : ptree);
      var
         t : ptree;
         rv,lv : longint;
         rd,ld : pdef;

      begin
         firstpass(p^.left);
         set_varstate(p^.left,true);
         firstpass(p^.right);
         set_varstate(p^.right,true);
         if codegenerror then
           exit;

         if isbinaryoverloaded(p) then
           exit;

         { check for division by zero }
         rv:=p^.right^.value;
         lv:=p^.left^.value;
         if is_constintnode(p^.right) and (rv=0) then
          begin
            Message(parser_e_division_by_zero);
            { recover }
            rv:=1;
          end;

         if is_constintnode(p^.left) and is_constintnode(p^.right) then
           begin
              case p^.treetype of
                modn : t:=genordinalconstnode(lv mod rv,s32bitdef,true);
                divn : t:=genordinalconstnode(lv div rv,s32bitdef,true);
              end;
              disposetree(p);
              firstpass(t);
              p:=t;
              exit;
           end;
         p^.location.loc:=LOC_REGISTER;
         { if one operand is a cardinal and the other is a positive constant, convert the }
         { constant to a cardinal as well so we don't have to do a 64bit division (JM)    }

         { Do the same for qwords and positive constants as well, otherwise things like   }
         { "qword mod 10" are evaluated with int64 as result, which is wrong if the       }
         { "qword" was > high(int64) (JM)                                                 }
         if (p^.left^.resulttype^.deftype=orddef) and (p^.right^.resulttype^.deftype=orddef) then
           if (porddef(p^.right^.resulttype)^.typ in [u32bit,u64bit]) and
              is_constintnode(p^.left) and
              (p^.left^.value >= 0) then
             begin
               p^.left := gentypeconvnode(p^.left,p^.right^.resulttype);
               firstpass(p^.left);
             end
           else if (porddef(p^.left^.resulttype)^.typ in [u32bit,u64bit]) and
              is_constintnode(p^.right) and
              (p^.right^.value >= 0) then
             begin
               p^.right := gentypeconvnode(p^.right,p^.left^.resulttype);
               firstpass(p^.right);
             end;

         if (p^.left^.resulttype^.deftype=orddef) and (p^.right^.resulttype^.deftype=orddef) and
            (is_64bitint(p^.left^.resulttype) or is_64bitint(p^.right^.resulttype) or
             { when mixing cardinals and signed numbers, convert everythign to 64bit (JM) }
             ((porddef(p^.right^.resulttype)^.typ = u32bit) and
              is_signed(p^.left^.resulttype)) or
             ((porddef(p^.left^.resulttype)^.typ = u32bit) and
              is_signed(p^.right^.resulttype))) then
           begin
              rd:=p^.right^.resulttype;
              ld:=p^.left^.resulttype;
              { issue warning if necessary }
              if not (is_64bitint(p^.left^.resulttype) or is_64bitint(p^.right^.resulttype)) then
                CGMessage(type_w_mixed_signed_unsigned);
              if is_signed(rd) or is_signed(ld) then
                begin
                   if (porddef(ld)^.typ<>s64bit) then
                     begin
                       p^.left:=gentypeconvnode(p^.left,cs64bitdef);
                       firstpass(p^.left);
                     end;
                   if (porddef(rd)^.typ<>s64bit) then
                     begin
                        p^.right:=gentypeconvnode(p^.right,cs64bitdef);
                        firstpass(p^.right);
                     end;
                   calcregisters(p,2,0,0);
                end
              else
                begin
                   if (porddef(ld)^.typ<>u64bit) then
                     begin
                       p^.left:=gentypeconvnode(p^.left,cu64bitdef);
                       firstpass(p^.left);
                     end;
                   if (porddef(rd)^.typ<>u64bit) then
                     begin
                        p^.right:=gentypeconvnode(p^.right,cu64bitdef);
                        firstpass(p^.right);
                     end;
                   calcregisters(p,2,0,0);
                end;
              p^.resulttype:=p^.left^.resulttype;
           end
         else
           begin
              if not(p^.right^.resulttype^.deftype=orddef) or
                not(porddef(p^.right^.resulttype)^.typ in [s32bit,u32bit]) then
                p^.right:=gentypeconvnode(p^.right,s32bitdef);

              if not(p^.left^.resulttype^.deftype=orddef) or
                not(porddef(p^.left^.resulttype)^.typ in [s32bit,u32bit]) then
                p^.left:=gentypeconvnode(p^.left,s32bitdef);

              firstpass(p^.left);
              firstpass(p^.right);

              { the resulttype depends on the right side, because the left becomes }
              { always 64 bit                                                      }
              p^.resulttype:=p^.right^.resulttype;

              if codegenerror then
                exit;

              left_right_max(p);
              if p^.left^.registers32<=p^.right^.registers32 then
                inc(p^.registers32);
           end;
      end;


{*****************************************************************************
                             FirstShlShr
*****************************************************************************}

    procedure firstshlshr(var p : ptree);
      var
         t : ptree;
         regs : longint;
      begin
         firstpass(p^.left);
         set_varstate(p^.left,true);
         firstpass(p^.right);
         set_varstate(p^.right,true);
         if codegenerror then
           exit;

         if isbinaryoverloaded(p) then
           exit;

         if is_constintnode(p^.left) and is_constintnode(p^.right) then
           begin
              case p^.treetype of
                 shrn : t:=genordinalconstnode(p^.left^.value shr p^.right^.value,s32bitdef,true);
                 shln : t:=genordinalconstnode(p^.left^.value shl p^.right^.value,s32bitdef,true);
              end;
              disposetree(p);
              firstpass(t);
              p:=t;
              exit;
           end;
         p^.location.loc:=LOC_REGISTER;
         { 64 bit ints have their own shift handling }
         if not(is_64bitint(p^.left^.resulttype)) then
           begin
              if porddef(p^.left^.resulttype)^.typ <> u32bit then
                p^.left:=gentypeconvnode(p^.left,s32bitdef);
              firstpass(p^.left);
              regs:=1;
              p^.resulttype:=p^.left^.resulttype;
           end
         else
           begin
              p^.resulttype:=p^.left^.resulttype;
              regs:=2;
           end;

         p^.right:=gentypeconvnode(p^.right,s32bitdef);
         firstpass(p^.right);

         if codegenerror then
           exit;

         if (p^.right^.treetype<>ordconstn) then
          inc(regs);
         calcregisters(p,regs,0,0);

      end;


{*****************************************************************************
                             FirstUnaryMinus
*****************************************************************************}

    procedure firstunaryminus(var p : ptree);
      var
         t : ptree;
         minusdef : pprocdef;
      begin
         firstpass(p^.left);
         set_varstate(p^.left,true);
         p^.registers32:=p^.left^.registers32;
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
         p^.resulttype:=p^.left^.resulttype;
         if codegenerror then
           exit;
         if is_constintnode(p^.left) then
           begin
              t:=genordinalconstnode(-p^.left^.value,s32bitdef,true);
              disposetree(p);
              firstpass(t);
              p:=t;
              exit;
           end;
           { nasm can not cope with negativ reals !! }
         if is_constrealnode(p^.left)
{$ifdef old_nasm}
           and not(aktoutputformat in [as_i386_nasmcoff,as_i386_nasmelf,as_i386_nasmobj])
{$endif old_nasm}
             then
           begin
              t:=genrealconstnode(-p^.left^.value_real,bestrealdef^);
              disposetree(p);
              firstpass(t);
              p:=t;
              exit;
           end;
         if (p^.left^.resulttype^.deftype=floatdef) then
           begin
              if pfloatdef(p^.left^.resulttype)^.typ=f32bit then
                begin
                   if (p^.left^.location.loc<>LOC_REGISTER) and
                     (p^.registers32<1) then
                     p^.registers32:=1;
                   p^.location.loc:=LOC_REGISTER;
                end
              else
                begin
                  if (p^.left^.location.loc<>LOC_FPU) and
                     (p^.registersfpu<1) then
                     p^.registersfpu:=1;
                  p^.location.loc:=LOC_FPU;
                end;
           end
{$ifdef SUPPORT_MMX}
         else if (cs_mmx in aktlocalswitches) and
           is_mmx_able_array(p^.left^.resulttype) then
             begin
               if (p^.left^.location.loc<>LOC_MMXREGISTER) and
                 (p^.registersmmx<1) then
                 p^.registersmmx:=1;
               { if saturation is on, p^.left^.resulttype isn't
                 "mmx able" (FK)
               if (cs_mmx_saturation in aktlocalswitches^) and
                 (porddef(parraydef(p^.resulttype)^.definition)^.typ in
                 [s32bit,u32bit]) then
                 CGMessage(type_e_mismatch);
               }
             end
{$endif SUPPORT_MMX}
         else if is_64bitint(p^.left^.resulttype) then
           begin
              firstpass(p^.left);
              p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
              p^.registers32:=p^.left^.registers32;
              if codegenerror then
                exit;
              if (p^.left^.location.loc<>LOC_REGISTER) and
                (p^.registers32<2) then
              p^.registers32:=2;
              p^.location.loc:=LOC_REGISTER;
              p^.resulttype:=p^.left^.resulttype;
           end
         else if (p^.left^.resulttype^.deftype=orddef) then
           begin
              p^.left:=gentypeconvnode(p^.left,s32bitdef);
              firstpass(p^.left);
              p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
              p^.registers32:=p^.left^.registers32;
              if codegenerror then
                exit;
              if (p^.left^.location.loc<>LOC_REGISTER) and
                (p^.registers32<1) then
              p^.registers32:=1;
              p^.location.loc:=LOC_REGISTER;
              p^.resulttype:=p^.left^.resulttype;
           end
         else
           begin
              if assigned(overloaded_operators[_minus]) then
                minusdef:=overloaded_operators[_minus]^.definition
              else
                minusdef:=nil;
              while assigned(minusdef) do
                begin
                   if is_equal(pparaitem(minusdef^.para^.first)^.paratype.def,p^.left^.resulttype) and
                      (pparaitem(minusdef^.para^.first)^.next=nil) then
                     begin
                        t:=gencallnode(overloaded_operators[_minus],nil);
                        t^.left:=gencallparanode(p^.left,nil);
                        putnode(p);
                        p:=t;
                        firstpass(p);
                        exit;
                     end;
                   minusdef:=minusdef^.nextoverloaded;
                end;
              CGMessage(type_e_mismatch);
           end;
      end;


{*****************************************************************************
                               FirstNot
*****************************************************************************}

    procedure firstnot(var p : ptree);
      var
         t : ptree;
         v, ressize: longint;
         notdef : pprocdef;
      begin
         firstpass(p^.left);
         set_varstate(p^.left,true);
         if codegenerror then
           exit;

         if (p^.left^.treetype=ordconstn) then
           begin
              v := p^.left^.value;
              case porddef(p^.left^.resulttype)^.typ of
                bool8bit,
                bool16bit,
                bool32bit:
                  v:=byte(not boolean(v));
                uchar,
                u8bit :
                  v:=byte(not byte(v));
                s8bit :
                  v:=shortint(not shortint(v));
                u16bit :
                  v:=word(not word(v));
                s16bit :
                  v:=smallint(not smallint(v));
                u32bit, s32bit :
                  v:=longint(not longint(v));
                else
                  CGMessage(type_e_mismatch);
              end;
              t:=genordinalconstnode(v,p^.left^.resulttype,true);
              disposetree(p);
              firstpass(t);
              p:=t;
              exit;
           end;
         p^.resulttype:=p^.left^.resulttype;
         p^.location.loc:=p^.left^.location.loc;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
         if is_boolean(p^.resulttype) then
           begin
             p^.registers32:=p^.left^.registers32;
             if (p^.location.loc in [LOC_REFERENCE,LOC_MEM,LOC_CREGISTER]) then
              begin
                p^.location.loc:=LOC_REGISTER;
                if (p^.registers32<1) then
                 p^.registers32:=1;
              end;
            { before loading it into flags we need to load it into
              a register thus 1 register is need PM }
{$ifdef i386}
             if p^.left^.location.loc<>LOC_JUMP then
               p^.location.loc:=LOC_FLAGS;
{$endif def i386}
           end
         else
{$ifdef SUPPORT_MMX}
           if (cs_mmx in aktlocalswitches) and
             is_mmx_able_array(p^.left^.resulttype) then
             begin
               if (p^.left^.location.loc<>LOC_MMXREGISTER) and
                 (p^.registersmmx<1) then
                 p^.registersmmx:=1;
             end
         else
{$endif SUPPORT_MMX}
           if is_64bitint(p^.left^.resulttype) then
             begin
                p^.registers32:=p^.left^.registers32;
                if (p^.location.loc in [LOC_REFERENCE,LOC_MEM,LOC_CREGISTER]) then
                 begin
                   p^.location.loc:=LOC_REGISTER;
                   if (p^.registers32<2) then
                    p^.registers32:=2;
                 end;
             end
         else if is_integer(p^.left^.resulttype) then
           begin
              p^.registers32:=p^.left^.registers32;
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}

              if (p^.left^.location.loc<>LOC_REGISTER) and
                 (p^.registers32<1) then
                p^.registers32:=1;
              p^.location.loc:=LOC_REGISTER;
           end
         else
           begin
              if assigned(overloaded_operators[_op_not]) then
                notdef:=overloaded_operators[_op_not]^.definition
              else
                notdef:=nil;
              while assigned(notdef) do
                begin
                   if is_equal(pparaitem(notdef^.para^.first)^.paratype.def,p^.left^.resulttype) and
                      (pparaitem(notdef^.para^.first)^.next=nil) then
                     begin
                        t:=gencallnode(overloaded_operators[_op_not],nil);
                        t^.left:=gencallparanode(p^.left,nil);
                        putnode(p);
                        p:=t;
                        firstpass(p);
                        exit;
                     end;
                   notdef:=notdef^.nextoverloaded;
                end;
              CGMessage(type_e_mismatch);
           end;

         p^.registersfpu:=p^.left^.registersfpu;
      end;



end.
{
  $Log: tcmat.pas,v $
  Revision 1.1.2.11  2002/09/07 11:04:29  carl
    * 2nd part of tw1996 bugfix (genordconstnode now has option to indicate if
      range must be verified), this also optimizes a bit.

  Revision 1.1.2.10  2001/12/27 15:33:27  jonas
    * fixed fpuregister counting errors

  Revision 1.1.2.9  2001/10/11 14:36:31  jonas
    * fixed internalerror(10) due to previous fpu overflow fixes
    * NOTE: from now on, the location of a binary node must now always be set
       before you call calcregisters() for it

  Revision 1.1.2.8  2001/04/08 11:03:11  jonas
    * 'not' now always returns a value of the same type + backport of Peter's mainbnranch fixes

  Revision 1.1.2.7  2001/03/21 15:24:43  jonas
    * result of not(constant) now has the same resulttype as the constant and
      the resulting constant is truncated so it lies within this type's domain

  Revision 1.1.2.6  2001/03/20 17:55:10  jonas
    * not (cardinal) now has cardinal instead of longint result (bug reported
      in mailinglist)

  Revision 1.1.2.5  2001/03/04 10:38:55  jonas
    * fixed 'qword mod/div pos_const' to have qword result

  Revision 1.1.2.4  2001/02/20 15:59:34  pierre
   * allow negative real constants for nasm

  Revision 1.1.2.3  2000/12/16 15:54:00  jonas
    * 'resulttype of cardinal shl/shr x' is cardinal instead of longint

  Revision 1.1.2.2  2000/12/15 13:55:46  jonas
    * fixed bug in division of qwords (introduced by previous patch)

  Revision 1.1.2.1  2000/12/13 12:25:40  jonas
    + also added 64bit conversion when using cardinals and signed
      expressions for div (in tcmat this time :)
    * removed automatic type conversion back to dword of 64bit results

  Revision 1.1  2000/07/13 06:29:59  michael
  + Initial import

  Revision 1.31  2000/06/05 20:41:18  pierre
    + support for NOT overloading
    + unsupported overloaded operators generate errors

  Revision 1.30  2000/06/02 21:13:56  pierre
   * use is_equal instead of direct def equality in unary minus overload

  Revision 1.29  2000/02/17 14:53:43  florian
    * some updates for the newcg

  Revision 1.28  2000/02/09 13:23:08  peter
    * log truncated

  Revision 1.27  2000/01/07 01:14:46  peter
    * updated copyright to 2000

  Revision 1.26  1999/12/11 18:53:31  jonas
    * fixed type conversions of results of operations with cardinals
      (between -dcardinalmulfix)

  Revision 1.25  1999/11/30 10:40:58  peter
    + ttype, tsymlist

  Revision 1.24  1999/11/26 13:51:29  pierre
   * fix for overloading of shr shl mod and div

  Revision 1.23  1999/11/18 15:34:50  pierre
    * Notes/Hints for local syms changed to
      Set_varstate function

  Revision 1.22  1999/11/06 14:34:30  peter
    * truncated log to 20 revs

  Revision 1.21  1999/10/26 12:30:46  peter
    * const parameter is now checked
    * better and generic check if a node can be used for assigning
    * export fixes
    * procvar equal works now (it never had worked at least from 0.99.8)
    * defcoll changed to linkedlist with pparaitem so it can easily be
      walked both directions

  Revision 1.20  1999/08/23 23:37:01  pierre
   * firstnot register counting error corrected

  Revision 1.19  1999/08/04 13:03:15  jonas
    * all tokens now start with an underscore
    * PowerPC compiles!!

  Revision 1.18  1999/08/04 00:23:43  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.17  1999/08/03 22:03:34  peter
    * moved bitmask constants to sets
    * some other type/const renamings

}
