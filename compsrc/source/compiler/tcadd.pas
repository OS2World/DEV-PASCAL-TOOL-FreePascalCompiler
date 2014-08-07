{
    $Id: tcadd.pas,v 1.1.2.19 2003/03/12 00:19:29 pierre Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    Type checking and register allocation for add node

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
unit tcadd;
interface

    uses
      tree;

    procedure firstadd(var p : ptree);
    function isbinaryoverloaded(var p : ptree) : boolean;


implementation

    uses
      globtype,systems,tokens,
      cobjects,verbose,globals,
      symconst,symtable,aasm,types,
{$ifdef newcg}
      cgbase,
{$else newcg}
      hcodegen,
{$endif newcg}
      htypechk,pass_1,
      cpubase,tccnv
      ;

    function isbinaryoverloaded(var p : ptree) : boolean;

     var
         rd,ld   : pdef;
         t : ptree;
         optoken : ttoken;

      begin
        isbinaryoverloaded:=false;
        { overloaded operator ? }
        { load easier access variables }
        rd:=p^.right^.resulttype;
        ld:=p^.left^.resulttype;
        if isbinaryoperatoroverloadable(ld,p^.left^.treetype,rd,p^.right^.treetype,p^.treetype) then
          begin
             isbinaryoverloaded:=true;
             {!!!!!!!!! handle paras }
             case p^.treetype of
                { the nil as symtable signs firstcalln that this is
                  an overloaded operator }
                addn:
                  optoken:=_PLUS;
                subn:
                  optoken:=_MINUS;
                muln:
                  optoken:=_STAR;
                starstarn:
                  optoken:=_STARSTAR;
                slashn:
                  optoken:=_SLASH;
                ltn:
                  optoken:=tokens._lt;
                gtn:
                  optoken:=tokens._gt;
                lten:
                  optoken:=_lte;
                gten:
                  optoken:=_gte;
                equaln,unequaln :
                  optoken:=_EQUAL;
                symdifn :
                  optoken:=_SYMDIF;
                modn :
                  optoken:=_OP_MOD;
                orn :
                  optoken:=_OP_OR;
                xorn :
                  optoken:=_OP_XOR;
                andn :
                  optoken:=_OP_AND;
                divn :
                  optoken:=_OP_DIV;
                shln :
                  optoken:=_OP_SHL;
                shrn :
                  optoken:=_OP_SHR;
                else
                  exit;
             end;
             t:=gencallnode(overloaded_operators[optoken],nil);
             { we have to convert p^.left and p^.right into
              callparanodes }
             if t^.symtableprocentry=nil then
               begin
                  CGMessage(parser_e_operator_not_overloaded);
                  putnode(t);
               end
             else
               begin
                  inc(t^.symtableprocentry^.refs);
                  t^.left:=gencallparanode(p^.left,nil);
                  t^.left:=gencallparanode(p^.right,t^.left);
                  if p^.treetype=unequaln then
                   t:=gensinglenode(notn,t);
                  firstpass(t);
                  putnode(p);
                  p:=t;
               end;
          end;
      end;

{*****************************************************************************
                                FirstAdd
*****************************************************************************}

{$ifdef fpc}
{$maxfpuregisters 0}
{$endif fpc}

    procedure firstadd(var p : ptree);

      procedure make_bool_equal_size(var p:ptree);
      begin
        if porddef(p^.left^.resulttype)^.typ>porddef(p^.right^.resulttype)^.typ then
         begin
           p^.right:=gentypeconvnode(p^.right,porddef(p^.left^.resulttype));
           p^.right^.convtyp:=tc_bool_2_int;
           p^.right^.explizit:=true;
           firstpass(p^.right);
         end
        else
         if porddef(p^.left^.resulttype)^.typ<porddef(p^.right^.resulttype)^.typ then
          begin
            p^.left:=gentypeconvnode(p^.left,porddef(p^.right^.resulttype));
            p^.left^.convtyp:=tc_bool_2_int;
            p^.left^.explizit:=true;
            firstpass(p^.left);
          end;
      end;

      var
         t,hp    : ptree;
         ot,
         lt,rt,pt: ttreetyp;
         rv,lv   : longint;
         rvd,lvd : bestreal;
         resdef,
         rd,ld   : pdef;
         tempdef : pdef;
         postconvdef : pdef;
         concatstrings : boolean;

         { to evalute const sets }
         resultset : pconstset;
         i : longint;
         b : boolean;
         convdone : boolean;
         s1,s2 : pchar;
         l1,l2 : longint;

      begin
         { first do the two subtrees }
         firstpass(p^.left);
         firstpass(p^.right);
         if codegenerror then
           exit;

         { convert array constructors to sets, because there is no other operator
           possible for array constructors }
         if is_array_constructor(p^.left^.resulttype) then
           begin
           arrayconstructor_to_set(p^.left);
           firstpass(p^.left);
           end;
         if is_array_constructor(p^.right^.resulttype) then
           begin
           arrayconstructor_to_set(p^.right);
           firstpass(p^.right);
           end;
         { both left and right need to be valid }
         set_varstate(p^.left,true);
         set_varstate(p^.right,true);

         { load easier access variables }
         lt:=p^.left^.treetype;
         rt:=p^.right^.treetype;
         { always use pt instead of p^.treetype, becayse p may be replaced }
         { with a typeconversion node! (JM)                                }
         pt:=p^.treetype;
         rd:=p^.right^.resulttype;
         ld:=p^.left^.resulttype;
         convdone:=false;
         postconvdef:=nil;

         if isbinaryoverloaded(p) then
           exit;
         { compact consts }

         { convert int consts to real consts, if the }
         { other operand is a real const             }
         if (rt=realconstn) and is_constintnode(p^.left) then
           begin
              t:=genrealconstnode(p^.left^.value,p^.right^.resulttype);
              disposetree(p^.left);
              p^.left:=t;
              lt:=realconstn;
           end;
         if (lt=realconstn) and is_constintnode(p^.right) then
           begin
              t:=genrealconstnode(p^.right^.value,p^.left^.resulttype);
              disposetree(p^.right);
              p^.right:=t;
              rt:=realconstn;
           end;

       { check for division by zero }
       if (pt = slashn) and
          (((rt = ordconstn) and
            (p^.right^.value = 0)) or
           ((rt = realconstn) and
            (p^.right^.value_real = 0.0))) then
         begin
           Message(parser_e_division_by_zero);
           case rt of
             ordconstn:
                p^.right^.value := 1;
             realconstn:
                p^.right^.value_real := 1.0;
           end;
         end;

       { both are int constants, also allow operations on two equal enums
         in fpc mode (Needed for conversion of C code) }
         if (((is_constintnode(p^.left) and is_constintnode(p^.right)) or
              (is_constboolnode(p^.left) and is_constboolnode(p^.right) and
               (pt in [ltn,lten,gtn,gten,equaln,unequaln,andn,xorn,orn,slashn])))) or
            { support pointer arithmetics on constants (JM) }
            ((lt = pointerconstn) and is_constintnode(p^.right) and
             (pt in [addn,subn])) or
            ((lt = pointerconstn) and (rt = pointerconstn) and
             (pt in [ltn,lten,gtn,gten,equaln,unequaln,subn])) then
           begin
              { when comparing/substracting  pointers, make sure they are }
              { of the same  type (JM)                                    }
              if (lt = pointerconstn) and (rt = pointerconstn) then
                if not(cs_extsyntax in aktmoduleswitches) and
                   not(pt in [equaln,unequaln]) then
                  CGMessage(type_e_mismatch)
                else
                  if (pt <> subn) and
                     is_equal(p^.right^.resulttype,voidpointerdef) then
                    begin
                       p^.right:=gentypeconvnode(p^.right,ld);
                       firstpass(p^.right);
                       rd := p^.right^.resulttype;
                    end
                  else if (pt <> subn) and
                          is_equal(p^.left^.resulttype,voidpointerdef) then
                    begin
                       p^.left:=gentypeconvnode(p^.left,rd);
                       firstpass(p^.left);
                       ld := p^.left^.resulttype;
                    end
                  else if not(is_equal(ld,rd)) then
                    CGMessage(type_e_mismatch);
              { return a boolean for boolean operations (and,xor,or) }
              if is_constboolnode(p^.left) then
                resdef:=booldef
              else if (lt = pointerconstn) then
                if (rt = pointerconstn) then
                  if (pt <> subn) then
                  { pointer comparison }
                    resdef := booldef
                  { pointer substraction }
                  else resdef := s32bitdef
                else
                  { pointer + integer }
                  resdef:=p^.left^.resulttype
              else
               if pt <> slashn then
                 resdef:=s32bitdef
               else
                 resdef:=bestrealdef^;
              lv:=p^.left^.value;
              rv:=p^.right^.value;
              if (lt = pointerconstn) and
                 (rt <> pointerconstn) then
                rv := rv * ppointerdef(p^.left^.resulttype)^.pointertype.def^.size;
              case pt of
                addn :
                  if (lt <> pointerconstn) then
                    t:=genordinalconstnode(lv+rv,resdef,true)
                  else t := genpointerconstnode(lv+rv,resdef);
                subn :
                  { pointer - pointer = ordinal }
                  if (lt <> pointerconstn) or (rt = pointerconstn) then
                    t:=genordinalconstnode(lv-rv,resdef,true)
                  else t := genpointerconstnode(lv-rv,resdef);
                muln : t:=genordinalconstnode(lv*rv,resdef,true);
                xorn : t:=genordinalconstnode(lv xor rv,resdef,true);
                 orn : t:=genordinalconstnode(lv or rv,resdef,true);
                andn : t:=genordinalconstnode(lv and rv,resdef,true);
                 ltn : t:=genordinalconstnode(ord(lv<rv),booldef,true);
                lten : t:=genordinalconstnode(ord(lv<=rv),booldef,true);
                 gtn : t:=genordinalconstnode(ord(lv>rv),booldef,true);
                gten : t:=genordinalconstnode(ord(lv>=rv),booldef,true);
              equaln : t:=genordinalconstnode(ord(lv=rv),booldef,true);
            unequaln : t:=genordinalconstnode(ord(lv<>rv),booldef,true);
              slashn : begin
                       { int/int becomes a real }
                         t:=genrealconstnode(int(lv)/int(rv),resdef);
                         firstpass(t);
                       end;
              else
                CGMessage(type_e_mismatch);
              end;
              disposetree(p);
              firstpass(t);
              p:=t;
              exit;
           end;

       { both real constants ? }
         if (lt=realconstn) and (rt=realconstn) then
           begin
              lvd:=p^.left^.value_real;
              rvd:=p^.right^.value_real;
              case pt of
                 addn : t:=genrealconstnode(lvd+rvd,bestrealdef^);
                 subn : t:=genrealconstnode(lvd-rvd,bestrealdef^);
                 muln : t:=genrealconstnode(lvd*rvd,bestrealdef^);
               starstarn,
               caretn : begin
                          if lvd<0 then
                           begin
                             Message(parser_e_invalid_float_operation);
                             t:=genrealconstnode(0,bestrealdef^);
                           end
                          else if lvd=0 then
                            t:=genrealconstnode(1.0,bestrealdef^)
                          else
                            t:=genrealconstnode(exp(ln(lvd)*rvd),bestrealdef^);
                        end;
               slashn :
                        begin
                          t:=genrealconstnode(lvd/rvd,bestrealdef^);
                        end;
                  ltn : t:=genordinalconstnode(ord(lvd<rvd),booldef,true);
                 lten : t:=genordinalconstnode(ord(lvd<=rvd),booldef,true);
                  gtn : t:=genordinalconstnode(ord(lvd>rvd),booldef,true);
                 gten : t:=genordinalconstnode(ord(lvd>=rvd),booldef,true);
               equaln : t:=genordinalconstnode(ord(lvd=rvd),booldef,true);
             unequaln : t:=genordinalconstnode(ord(lvd<>rvd),booldef,true);
              else
                begin
                  CGMessage(type_e_mismatch);
                  t := genzeronode(errorn);
                end;
              end;
              disposetree(p);
              p:=t;
              firstpass(p);
              exit;
           end;

       { concating strings ? }
         concatstrings:=false;
         s1:=nil;
         s2:=nil;
         if (lt=ordconstn) and (rt=ordconstn) and
            is_char(ld) and is_char(rd) then
           begin
              s1:=strpnew(char(byte(p^.left^.value)));
              s2:=strpnew(char(byte(p^.right^.value)));
              l1:=1;
              l2:=1;
              concatstrings:=true;
           end
         else
           if (lt=stringconstn) and (rt=ordconstn) and is_char(rd) then
           begin
              s1:=getpcharcopy(p^.left);
              l1:=p^.left^.length;
              s2:=strpnew(char(byte(p^.right^.value)));
              l2:=1;
              concatstrings:=true;
           end
         else
           if (lt=ordconstn) and (rt=stringconstn) and is_char(ld) then
           begin
              s1:=strpnew(char(byte(p^.left^.value)));
              l1:=1;
              s2:=getpcharcopy(p^.right);
              l2:=p^.right^.length;
              concatstrings:=true;
           end
         else if (lt=stringconstn) and (rt=stringconstn) then
           begin
              s1:=getpcharcopy(p^.left);
              l1:=p^.left^.length;
              s2:=getpcharcopy(p^.right);
              l2:=p^.right^.length;
              concatstrings:=true;
           end;

         { I will need to translate all this to ansistrings !!! }
         if concatstrings then
           begin
              case pt of
                 addn :
                   t:=genpcharconstnode(concatansistrings(s1,s2,l1,l2),l1+l2);
                 ltn :
                   t:=genordinalconstnode(byte(compareansistrings(s1,s2,l1,l2)<0),booldef,true);
                 lten :
                   t:=genordinalconstnode(byte(compareansistrings(s1,s2,l1,l2)<=0),booldef,true);
                 gtn :
                   t:=genordinalconstnode(byte(compareansistrings(s1,s2,l1,l2)>0),booldef,true);
                 gten :
                   t:=genordinalconstnode(byte(compareansistrings(s1,s2,l1,l2)>=0),booldef,true);
                 equaln :
                   t:=genordinalconstnode(byte(compareansistrings(s1,s2,l1,l2)=0),booldef,true);
                 unequaln :
                   t:=genordinalconstnode(byte(compareansistrings(s1,s2,l1,l2)<>0),booldef,true);
              end;
              ansistringdispose(s1,l1);
              ansistringdispose(s2,l2);
              disposetree(p);
              firstpass(t);
              p:=t;
              exit;
           end;

       { if both are orddefs then check sub types }
         if (ld^.deftype=orddef) and (rd^.deftype=orddef) then
           begin
           { 2 booleans ? }
             if is_boolean(ld) and is_boolean(rd) then
              begin
                case pt of
                  andn,
                  orn:
                    begin
                      make_bool_equal_size(p);
                      p^.location.loc:=LOC_JUMP;
                      calcregisters(p,0,0,0);
                    end;
                  xorn,ltn,lten,gtn,gten:
                    begin
                      if pt <> xorn then
                        p^.location.loc := LOC_FLAGS
                      else
                        p^.location.loc := LOC_REGISTER;
                      make_bool_equal_size(p);
                      if (p^.left^.location.loc in [LOC_JUMP,LOC_FLAGS]) and
                        (p^.left^.location.loc in [LOC_JUMP,LOC_FLAGS]) then
                        calcregisters(p,2,0,0)
                      else
                        calcregisters(p,1,0,0);
                    end;
                  unequaln,
                  equaln:
                    begin
                      make_bool_equal_size(p);
                      { Remove any compares with constants }
                      if (p^.left^.treetype=ordconstn) then
                       begin
                         hp:=p^.right;
                         b:=(p^.left^.value<>0);
                         ot:=pt;
                         disposetree(p^.left);
                         putnode(p);
                         p:=hp;
                         if (not(b) and (ot=equaln)) or
                            (b and (ot=unequaln)) then
                          begin
                            p:=gensinglenode(notn,p);
                            firstpass(p);
                          end;
                         exit;
                       end;
                      if (p^.right^.treetype=ordconstn) then
                       begin
                         hp:=p^.left;
                         b:=(p^.right^.value<>0);
                         ot:=pt;
                         disposetree(p^.right);
                         putnode(p);
                         p:=hp;
                         if (not(b) and (ot=equaln)) or
                            (b and (ot=unequaln)) then
                          begin
                            p:=gensinglenode(notn,p);
                            firstpass(p);
                          end;
                         exit;
                       end;
                      p^.location.loc := LOC_FLAGS;
                      if (p^.left^.location.loc in [LOC_JUMP,LOC_FLAGS]) and
                        (p^.left^.location.loc in [LOC_JUMP,LOC_FLAGS]) then
                        calcregisters(p,2,0,0)
                      else
                        calcregisters(p,1,0,0);
                    end;
                else
                  CGMessage(type_e_mismatch);
                end;
(*
                { these one can't be in flags! }

                Yes they can, secondadd converts the loc_flags to a register.
                The typeconversions below are simply removed by firsttypeconv()
                because the resulttype of p^.left = p^.left^.resulttype
                (surprise! :) (JM)

                if pt in [xorn,unequaln,equaln] then
                  begin
                     if p^.left^.location.loc=LOC_FLAGS then
                       begin
                          p^.left:=gentypeconvnode(p^.left,porddef(p^.left^.resulttype));
                          p^.left^.convtyp:=tc_bool_2_int;
                          p^.left^.explizit:=true;
                          firstpass(p^.left);
                       end;
                     if p^.right^.location.loc=LOC_FLAGS then
                       begin
                          p^.right:=gentypeconvnode(p^.right,porddef(p^.right^.resulttype));
                          p^.right^.convtyp:=tc_bool_2_int;
                          p^.right^.explizit:=true;
                          firstpass(p^.right);
                       end;
                     { readjust registers }
                     calcregisters(p,1,0,0);
                  end;
*)
                convdone:=true;
              end
             else
             { Both are chars? only convert to shortstrings for addn }
              if is_char(rd) and is_char(ld) then
               begin
                 if pt=addn then
                   begin
                     p^.left:=gentypeconvnode(p^.left,cshortstringdef);
                     p^.right:=gentypeconvnode(p^.right,cshortstringdef);
                     firstpass(p^.left);
                     firstpass(p^.right);
                     { here we call STRCOPY }
                     { if we are actually parsing a subroutine! }
                     { parsing interface section : procinfo = nil }
                     if assigned(procinfo) then
                        procinfo^.flags:=procinfo^.flags or pi_do_call;
                     p^.location.loc:=LOC_MEM;
                     calcregisters(p,0,0,0);
                   end
                 else
                   begin
                     p^.location.loc := LOC_FLAGS;
                     calcregisters(p,1,0,0);
                   end;
                 convdone:=true;
               end
              { is there a 64 bit type ? }
             else if ((porddef(rd)^.typ=s64bit) or (porddef(ld)^.typ=s64bit)) and
               { the / operator is handled later }
               (pt<>slashn) then
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
                  if pt in [addn,subn,muln,andn,orn,xorn] then
                    p^.location.loc := LOC_REGISTER
                  else
                    p^.location.loc := LOC_JUMP;
                  calcregisters(p,2,0,0);
                  convdone:=true;
               end
             else if ((porddef(rd)^.typ=u64bit) or (porddef(ld)^.typ=u64bit)) and
               { the / operator is handled later }
               (pt<>slashn) then
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
                  if pt in [addn,subn,muln,andn,orn,xorn] then
                    p^.location.loc := LOC_REGISTER
                  else
                    p^.location.loc := LOC_JUMP;
                  calcregisters(p,2,0,0);
                  convdone:=true;
               end
             else
              { is there a cardinal? }
              if ((porddef(rd)^.typ=u32bit) or (porddef(ld)^.typ=u32bit)) and
               { the / operator is handled later }
               (pt<>slashn) then
               begin
                 if is_signed(ld) and
                    { then rd = u32bit }
                    { convert positive constants to u32bit }
                    not(is_constintnode(p^.left) and
                        (p^.left^.value >= 0)) and
                    { range/overflow checking on mixed signed/cardinal expressions }
                    { is only possible if you convert everything to 64bit (JM)     }
                    ((aktlocalswitches * [cs_check_overflow,cs_check_range] <> []) and
                     (pt in [addn,subn,muln])) then
                   begin
                     { perform the operation with 64bit but post typecast back to u32bitdef }
                     CGMessage(type_n_mixed_signed_unsigned3);
                     p^.left := gentypeconvnode(p^.left,cs64bitdef);
                     firstpass(p^.left);
                     p^.right := gentypeconvnode(p^.right,cs64bitdef);
                     firstpass(p^.right);
                     postconvdef:=u32bitdef;
                   end
                 else
                   begin
                     if is_signed(ld) and
                        not(is_constintnode(p^.left) and
                            (p^.left^.value >= 0)) and
                        (cs_check_range in aktlocalswitches) then
                       CGMessage(type_w_mixed_signed_unsigned2);
                     p^.left := gentypeconvnode(p^.left,u32bitdef);
                     firstpass(p^.left);

                     if is_signed(rd) and
                        { then ld = u32bit }
                        { convert positive constants to u32bit }
                        not(is_constintnode(p^.right) and
                            (p^.right^.value >= 0)) and
                        ((aktlocalswitches * [cs_check_overflow,cs_check_range] <> []) and
                         (pt in [addn,subn,muln])) then
                       begin
                         { perform the operation with 64bit but
                           post typecast back to u32bitdef }
                         CGMessage(type_n_mixed_signed_unsigned3);
                         p^.left := gentypeconvnode(p^.left,cs64bitdef);
                         firstpass(p^.left);
                         p^.right := gentypeconvnode(p^.right,cs64bitdef);
                         firstpass(p^.right);
                         postconvdef:=u32bitdef;
                       end
                     else
                       begin
                         if is_signed(rd) and
                            not(is_constintnode(p^.right) and
                                (p^.right^.value >= 0)) and
                            (cs_check_range in aktlocalswitches) then
                           CGMessage(type_w_mixed_signed_unsigned2);
                         p^.right := gentypeconvnode(p^.right,u32bitdef);
                         firstpass(p^.right);
                       end;
                   end;
                  if pt in [addn,subn,muln,andn,orn,xorn] then
                    p^.location.loc := LOC_REGISTER
                  else if porddef(p^.left^.resulttype)^.typ <> s64bit then
                    p^.location.loc := LOC_FLAGS
                  else
                    p^.location.loc := LOC_JUMP;
                 { did we convert things to 64bit? }
                 if porddef(p^.left^.resulttype)^.typ = s64bit then
                   calcregisters(p,2,0,0)
                 else
                   begin
                     calcregisters(p,1,0,0);
                 { for unsigned mul we need an extra register }
                     if pt=muln then
                       inc(p^.registers32);
                   end;
                 convdone:=true;
               end;
           end
         else

         { left side a setdef, must be before string processing,
           else array constructor can be seen as array of char (PFV) }
           if (ld^.deftype=setdef) {or is_array_constructor(ld)} then
             begin
             { trying to add a set element? }
                if (pt=addn) and (rd^.deftype<>setdef) then
                 begin
                   if (rt=setelementn) then
                    begin
                      if not(is_equal(psetdef(ld)^.elementtype.def,rd)) then
                       CGMessage(type_e_set_element_are_not_comp);
                    end
                   else
                    CGMessage(type_e_mismatch)
                 end
                else
                 begin
                   if not(pt in [addn,subn,symdifn,muln,equaln,unequaln
{$IfNDef NoSetInclusion}
                                          ,lten,gten
{$EndIf NoSetInclusion}
                   ]) then
                    CGMessage(type_e_set_operation_unknown);
                 { right def must be a also be set }
                   if (rd^.deftype<>setdef) or not(is_equal(rd,ld)) then
                    CGMessage(type_e_set_element_are_not_comp);
                 end;

                { ranges require normsets }
                if (psetdef(ld)^.settype=smallset) and
                   (rt=setelementn) and
                   assigned(p^.right^.right) then
                 begin
                   { generate a temporary normset def, it'll be destroyed
                     when the symtable is unloaded }
                   tempdef:=new(psetdef,init(psetdef(ld)^.elementtype.def,255));
                   p^.left:=gentypeconvnode(p^.left,tempdef);
                   firstpass(p^.left);
                   ld:=p^.left^.resulttype;
                 end;

                { if the destination is not a smallset then insert a typeconv
                  which loads a smallset into a normal set }
                if (psetdef(ld)^.settype<>smallset) and
                   (psetdef(rd)^.settype=smallset) then
                 begin
                   if (p^.right^.treetype=setconstn) then
                     begin
                        t:=gensetconstnode(p^.right^.value_set,psetdef(p^.left^.resulttype));
                        t^.left:=p^.right^.left;
                        putnode(p^.right);
                        p^.right:=t;
                     end
                   else
                     p^.right:=gentypeconvnode(p^.right,psetdef(p^.left^.resulttype));
                   firstpass(p^.right);
                 end;

                { we also need to do it in the other case,
                  but right can be an element, so we need to check
                  that its also a set PM }

                if (rd^.deftype=setdef) and
                   (psetdef(rd)^.settype<>smallset) and
                   (psetdef(ld)^.settype=smallset) then
                 begin
                   if (p^.left^.treetype=setconstn) then
                     begin
                        t:=gensetconstnode(p^.left^.value_set,psetdef(p^.right^.resulttype));
                        t^.left:=p^.left^.left;
                        putnode(p^.left);
                        p^.left:=t;
                     end
                   else
                     p^.left:=gentypeconvnode(p^.left,psetdef(p^.right^.resulttype));
                   firstpass(p^.left);
                 end;

                { do constant evaluation }
                if (p^.right^.treetype=setconstn) and
                   not assigned(p^.right^.left) and
                   (p^.left^.treetype=setconstn) and
                   not assigned(p^.left^.left) then
                  begin
                     new(resultset);
                     case pt of
                        addn : begin
                                  for i:=0 to 31 do
                                    resultset^[i]:=
                                      p^.right^.value_set^[i] or p^.left^.value_set^[i];
                                  t:=gensetconstnode(resultset,psetdef(ld));
                               end;
                        muln : begin
                                  for i:=0 to 31 do
                                    resultset^[i]:=
                                      p^.right^.value_set^[i] and p^.left^.value_set^[i];
                                  t:=gensetconstnode(resultset,psetdef(ld));
                               end;
                        subn : begin
                                  for i:=0 to 31 do
                                    resultset^[i]:=
                                      p^.left^.value_set^[i] and not(p^.right^.value_set^[i]);
                                  t:=gensetconstnode(resultset,psetdef(ld));
                               end;
                     symdifn : begin
                                  for i:=0 to 31 do
                                    resultset^[i]:=
                                      p^.left^.value_set^[i] xor p^.right^.value_set^[i];
                                  t:=gensetconstnode(resultset,psetdef(ld));
                               end;
                    unequaln : begin
                                 b:=true;
                                 for i:=0 to 31 do
                                  if p^.right^.value_set^[i]=p^.left^.value_set^[i] then
                                   begin
                                     b:=false;
                                     break;
                                   end;
                                 t:=genordinalconstnode(ord(b),booldef,true);
                               end;
                      equaln : begin
                                 b:=true;
                                 for i:=0 to 31 do
                                  if p^.right^.value_set^[i]<>p^.left^.value_set^[i] then
                                   begin
                                     b:=false;
                                     break;
                                   end;
                                 t:=genordinalconstnode(ord(b),booldef,true);
                               end;
{$IfNDef NoSetInclusion}
                       lten : Begin
                                b := true;
                                For i := 0 to 31 Do
                                  If (p^.right^.value_set^[i] And p^.left^.value_set^[i]) <>
                                      p^.left^.value_set^[i] Then
                                    Begin
                                      b := false;
                                      Break
                                    End;
                                t := genordinalconstnode(ord(b),booldef,true);
                              End;
                       gten : Begin
                                b := true;
                                For i := 0 to 31 Do
                                  If (p^.left^.value_set^[i] And p^.right^.value_set^[i]) <>
                                      p^.right^.value_set^[i] Then
                                    Begin
                                      b := false;
                                      Break
                                    End;
                                t := genordinalconstnode(ord(b),booldef,true);
                              End;
{$EndIf NoSetInclusion}
                     end;
                     dispose(resultset);
                     disposetree(p);
                     p:=t;
                     firstpass(p);
                     exit;
                  end
                else
                 if psetdef(ld)^.settype=smallset then
                  begin
                     p^.location.loc:=LOC_REGISTER;
                     { are we adding set elements ? }
                     if p^.right^.treetype=setelementn then
                       calcregisters(p,2,0,0)
                     else
                       calcregisters(p,1,0,0);
                  end
                 else
                  begin
                     p^.location.loc:=LOC_MEM;
                     calcregisters(p,0,0,0);
                     { here we call SET... }
                     { if it exists - we might be parsing constant }
                     { section still!                              }
                     if assigned(procinfo) then
                        procinfo^.flags:=procinfo^.flags or pi_do_call;
                  end;
              convdone:=true;
            end
         else

         { pointer comperation and subtraction }
           if (rd^.deftype=pointerdef) and (ld^.deftype=pointerdef) then
            begin
              p^.location.loc:=LOC_REGISTER;
              { p^.right:=gentypeconvnode(p^.right,ld); }
              { firstpass(p^.right); }
              calcregisters(p,1,0,0);
              case pt of
                 equaln,unequaln :
                   begin
                      if is_equal(p^.right^.resulttype,voidpointerdef) then
                        begin
                           p^.right:=gentypeconvnode(p^.right,ld);
                           firstpass(p^.right);
                        end
                      else if is_equal(p^.left^.resulttype,voidpointerdef) then
                        begin
                           p^.left:=gentypeconvnode(p^.left,rd);
                           firstpass(p^.left);
                        end
                      else if not(is_equal(ld,rd)) then
                        CGMessage(type_e_mismatch);
                   end;
                 ltn,lten,gtn,gten:
                   begin
                      if is_equal(p^.right^.resulttype,voidpointerdef) then
                        begin
                           p^.right:=gentypeconvnode(p^.right,ld);
                           firstpass(p^.right);
                        end
                      else if is_equal(p^.left^.resulttype,voidpointerdef) then
                        begin
                           p^.left:=gentypeconvnode(p^.left,rd);
                           firstpass(p^.left);
                        end
                      else if not(is_equal(ld,rd)) then
                        CGMessage(type_e_mismatch);
                      if not(cs_extsyntax in aktmoduleswitches) then
                        CGMessage(type_e_mismatch);
                   end;
                 subn:
                   begin
                      if not(is_equal(ld,rd)) then
                        CGMessage(type_e_mismatch);
                      if not(cs_extsyntax in aktmoduleswitches) then
                        CGMessage(type_e_mismatch);
                      p^.resulttype:=s32bitdef;
                      exit;
                   end;
                 else CGMessage(type_e_mismatch);
              end;
              convdone:=true;
           end
         else

           { compare pchar to char arrays by addresses
             like BP/Delphi }
           if ((is_pchar(ld) or (lt=niln)) and is_chararray(rd)) or
              ((is_pchar(rd) or (rt=niln)) and is_chararray(ld)) then
             begin
               if is_chararray(rd) then
                 begin
                   p^.right:=gentypeconvnode(p^.right,charpointerdef);
                   firstpass(p^.right);
                 end
               else
                 begin
                   p^.left:=gentypeconvnode(p^.left,charpointerdef);
                   firstpass(p^.left);
                 end;
               p^.location.loc:=LOC_REGISTER;
               calcregisters(p,1,0,0);
               convdone:=true;
             end
         else
           { is one of the operands a string?,
             chararrays are also handled as strings (after conversion).
             Note: This must be done after the check for pointerdef (PFV) }
           if (rd^.deftype=stringdef) or (ld^.deftype=stringdef) or
              ((is_pchar(rd) or is_chararray(rd) or is_char(rd)) and
               (is_pchar(ld) or is_chararray(ld) or is_char(ld))) then
            begin
              if is_widestring(rd) or is_widestring(ld) then
                begin
                   if not(is_widestring(rd)) then
                     p^.right:=gentypeconvnode(p^.right,cwidestringdef);
                   if not(is_widestring(ld)) then
                     p^.left:=gentypeconvnode(p^.left,cwidestringdef);
                   p^.resulttype:=cwidestringdef;
                   { this is only for add, the comparisaion is handled later }
                   p^.location.loc:=LOC_REGISTER;
                end
              else if is_ansistring(rd) or is_ansistring(ld) then
                begin
                   if not(is_ansistring(rd)) then
                     p^.right:=gentypeconvnode(p^.right,cansistringdef);
                   if not(is_ansistring(ld)) then
                     p^.left:=gentypeconvnode(p^.left,cansistringdef);
                   { we use ansistrings so no fast exit here }
                   { if we are actually parsing a subroutine! }
                   { parsing interface section : procinfo = nil }
                   if assigned(procinfo) then
                      procinfo^.no_fast_exit:=true;
                   p^.resulttype:=cansistringdef;
                   { this is only for add, the comparisaion is handled later }
                   p^.location.loc:=LOC_REGISTER;
                end
              else if is_longstring(rd) or is_longstring(ld) then
                begin
                   if not(is_longstring(rd)) then
                     p^.right:=gentypeconvnode(p^.right,clongstringdef);
                   if not(is_longstring(ld)) then
                     p^.left:=gentypeconvnode(p^.left,clongstringdef);
                   p^.resulttype:=clongstringdef;
                   { this is only for add, the comparisaion is handled later }
                   p^.location.loc:=LOC_MEM;
                end
              else
                begin
                   if not(is_shortstring(rd))
{$ifdef newoptimizations2}
{$ifdef i386}
                      { shortstring + char handled seperately  (JM) }
                      and (not(cs_optimize in aktglobalswitches) or
                           (pt <> addn) or not(is_char(rd)))
{$endif i386}
{$endif newoptimizations2}
                    then
                      p^.right:=gentypeconvnode(p^.right,cshortstringdef);
                   if not(is_shortstring(ld)) then
                     p^.left:=gentypeconvnode(p^.left,cshortstringdef);
                   p^.resulttype:=cshortstringdef;
                   { this is only for add, the comparisaion is handled later }
                   p^.location.loc:=LOC_MEM;
                end;
              { only if there is a type cast we need to do again }
              { the first pass                             }
              if p^.left^.treetype=typeconvn then
                firstpass(p^.left);
              if p^.right^.treetype=typeconvn then
                firstpass(p^.right);
              { here we call STRCONCAT or STRCMP or STRCOPY }
              { if we are actually parsing a subroutine! }
              { parsing interface section : procinfo = nil }
              if assigned(procinfo) then
                 procinfo^.flags:=procinfo^.flags or pi_do_call;
              if p^.location.loc=LOC_MEM then
                calcregisters(p,0,0,0)
              else
                calcregisters(p,1,0,0);
{$ifdef newoptimizations}
{$ifdef i386}
              { not always necessary, only if it is not a constant char and }
              { not a regvar, but don't know how to check this here (JM)    }
              if is_char(rd) then
                inc(p^.registers32);
{$endif i386}
{$endif newoptimizations}
              convdone:=true;
           end
         else

         { is one a real float ? }
           if (rd^.deftype=floatdef) or (ld^.deftype=floatdef) then
            begin
            { if one is a fixed, then convert to f32bit }
              if ((rd^.deftype=floatdef) and (pfloatdef(rd)^.typ=f32bit)) or
                 ((ld^.deftype=floatdef) and (pfloatdef(ld)^.typ=f32bit)) then
               begin
                 if not is_integer(rd) or (pt<>muln) then
                   p^.right:=gentypeconvnode(p^.right,s32fixeddef);
                 if not is_integer(ld) or (pt<>muln) then
                   p^.left:=gentypeconvnode(p^.left,s32fixeddef);
                 firstpass(p^.left);
                 firstpass(p^.right);
                 p^.location.loc:=LOC_REGISTER;
                 calcregisters(p,1,0,0);
               end
              else
              { convert both to bestreal }
                begin
                  p^.right:=gentypeconvnode(p^.right,bestrealdef^);
                  p^.left:=gentypeconvnode(p^.left,bestrealdef^);
                  firstpass(p^.left);
                  firstpass(p^.right);
                  p^.location.loc:=LOC_FPU;
                  calcregisters(p,0,1,0);
                  { an add node always first loads both the left and the    }
                  { right in the fpu before doing the calculation. However, }
                  { calcregisters(0,2,0) will overestimate the number of    }
                  { necessary registers (it will make it 3 in case one of   }
                  { the operands is already in the fpu) (JM)                }
                  if ((p^.left^.location.loc <> LOC_FPU) or
                      (p^.right^.location.loc <> LOC_FPU)) and
                     (p^.registersfpu < 2) then
                    inc(p^.registersfpu);
                end;
              convdone:=true;
            end
         else

           if (rd^.deftype=objectdef) and (ld^.deftype=objectdef) and
              pobjectdef(rd)^.is_class and pobjectdef(ld)^.is_class then
            begin
              p^.location.loc:=LOC_REGISTER;
              if pobjectdef(rd)^.is_related(pobjectdef(ld)) then
                p^.right:=gentypeconvnode(p^.right,ld)
              else
                p^.left:=gentypeconvnode(p^.left,rd);
              firstpass(p^.right);
              firstpass(p^.left);
              calcregisters(p,1,0,0);
              case pt of
                 equaln,unequaln : ;
                 else CGMessage(type_e_mismatch);
              end;
              convdone:=true;
            end
         else

           if (rd^.deftype=classrefdef) and (ld^.deftype=classrefdef) then
            begin
              p^.location.loc:=LOC_REGISTER;
              if pobjectdef(pclassrefdef(rd)^.pointertype.def)^.is_related(pobjectdef(
                pclassrefdef(ld)^.pointertype.def)) then
                p^.right:=gentypeconvnode(p^.right,ld)
              else
                p^.left:=gentypeconvnode(p^.left,rd);
              firstpass(p^.right);
              firstpass(p^.left);
              calcregisters(p,1,0,0);
              case pt of
                 equaln,unequaln : ;
                 else CGMessage(type_e_mismatch);
              end;
              convdone:=true;
           end
         else

         { allows comperasion with nil pointer }
           if (rd^.deftype=objectdef) and
              pobjectdef(rd)^.is_class then
            begin
              p^.location.loc:=LOC_REGISTER;
              p^.left:=gentypeconvnode(p^.left,rd);
              firstpass(p^.left);
              calcregisters(p,1,0,0);
              case pt of
                 equaln,unequaln : ;
                 else CGMessage(type_e_mismatch);
              end;
              convdone:=true;
            end
         else

           if (ld^.deftype=objectdef) and
              pobjectdef(ld)^.is_class then
            begin
              p^.location.loc:=LOC_REGISTER;
              p^.right:=gentypeconvnode(p^.right,ld);
              firstpass(p^.right);
              calcregisters(p,1,0,0);
              case pt of
                 equaln,unequaln : ;
                 else CGMessage(type_e_mismatch);
              end;
              convdone:=true;
            end
         else

           if (rd^.deftype=classrefdef) then
            begin
              p^.left:=gentypeconvnode(p^.left,rd);
              firstpass(p^.left);
              p^.location.loc := LOC_FLAGS;
              calcregisters(p,1,0,0);
              case pt of
                 equaln,unequaln : ;
                 else CGMessage(type_e_mismatch);
              end;
              convdone:=true;
            end
         else

           if (ld^.deftype=classrefdef) then
            begin
              p^.right:=gentypeconvnode(p^.right,ld);
              firstpass(p^.right);
              p^.location.loc := LOC_FLAGS;
              calcregisters(p,1,0,0);
              case pt of
                equaln,unequaln : ;
              else
                CGMessage(type_e_mismatch);
              end;
              convdone:=true;
           end
         else

         { support procvar=nil,procvar<>nil }
           if ((ld^.deftype=procvardef) and (rt=niln)) or
              ((rd^.deftype=procvardef) and (lt=niln)) then
            begin
              p^.location.loc:=LOC_REGISTER;
              calcregisters(p,1,0,0);
              case pt of
                 equaln,unequaln : ;
              else
                CGMessage(type_e_mismatch);
              end;
              convdone:=true;
            end
         else

{$ifdef SUPPORT_MMX}
           if (cs_mmx in aktlocalswitches) and is_mmx_able_array(ld) and
             is_mmx_able_array(rd) and is_equal(ld,rd) then
            begin
              firstpass(p^.right);
              firstpass(p^.left);
              case pt of
                addn,subn,xorn,orn,andn:
                  ;
                { mul is a little bit restricted }
                muln:
                  if not(mmx_type(p^.left^.resulttype) in
                    [mmxu16bit,mmxs16bit,mmxfixed16]) then
                    CGMessage(type_e_mismatch);
                else
                  CGMessage(type_e_mismatch);
              end;
              p^.location.loc:=LOC_MMXREGISTER;
              calcregisters(p,0,0,1);
              convdone:=true;
            end
          else
{$endif SUPPORT_MMX}

           { this is a little bit dangerous, also the left type }
           { should be checked! This broke the mmx support      }
           if (rd^.deftype=pointerdef) or
             is_zero_based_array(rd) then
            begin
              if is_zero_based_array(rd) then
                begin
                   p^.resulttype:=new(ppointerdef,init(parraydef(rd)^.elementtype));
                   p^.right:=gentypeconvnode(p^.right,p^.resulttype);
                   firstpass(p^.right);
                   rd := p^.right^.resulttype;
                end;
              p^.location.loc:=LOC_REGISTER;
              p^.left:=gentypeconvnode(p^.left,s32bitdef);
              firstpass(p^.left);
              calcregisters(p,1,0,0);
              if pt=addn then
                begin
                  if not(cs_extsyntax in aktmoduleswitches) or
                    (not(is_pchar(ld)) and not(m_add_pointer in aktmodeswitches)) then
                    CGMessage(type_e_mismatch);
                  { Dirty hack, to support multiple firstpasses (PFV) }
                  if (p^.resulttype=nil) and
                     (rd^.deftype=pointerdef) and
                     (ppointerdef(rd)^.pointertype.def^.size>1) then
                   begin
                     p^.left:=gennode(muln,p^.left,genordinalconstnode(ppointerdef(rd)^.pointertype.def^.size,s32bitdef,true));
                     firstpass(p^.left);
                   end;
                end
              else
                CGMessage(type_e_mismatch);
              convdone:=true;
            end
         else

           if (ld^.deftype=pointerdef) or
             is_zero_based_array(ld) then
            begin
              if is_zero_based_array(ld) then
                begin
                   p^.resulttype:=new(ppointerdef,init(parraydef(ld)^.elementtype));
                   p^.left:=gentypeconvnode(p^.left,p^.resulttype);
                   firstpass(p^.left);
                   ld := p^.left^.resulttype;
                end;
              p^.location.loc:=LOC_REGISTER;
              p^.right:=gentypeconvnode(p^.right,s32bitdef);
              firstpass(p^.right);
              calcregisters(p,1,0,0);
              case pt of
                addn,subn : begin
                              if not(cs_extsyntax in aktmoduleswitches) or
                                 (not(is_pchar(ld)) and not(m_add_pointer in aktmodeswitches)) then
                               CGMessage(type_e_mismatch);
                              { Dirty hack, to support multiple firstpasses (PFV) }
                              if (p^.resulttype=nil) and
                                 (ld^.deftype=pointerdef) and
                                 (ppointerdef(ld)^.pointertype.def^.size>1) then
                               begin
                                 p^.right:=gennode(muln,p^.right,
                                   genordinalconstnode(ppointerdef(ld)^.pointertype.def^.size,s32bitdef,true));
                                 firstpass(p^.right);
                               end;
                            end;
              else
                CGMessage(type_e_mismatch);
              end;
              convdone:=true;
           end
         else

           if (rd^.deftype=procvardef) and (ld^.deftype=procvardef) and is_equal(rd,ld) then
            begin
              p^.location.loc:=LOC_REGISTER;
              calcregisters(p,1,0,0);
              case pt of
                 equaln,unequaln : ;
              else
                CGMessage(type_e_mismatch);
              end;
              convdone:=true;
            end
         else

           if (ld^.deftype=enumdef) and (rd^.deftype=enumdef) then
            begin
              if not(is_equal(ld,rd)) then
                begin
                   p^.right:=gentypeconvnode(p^.right,ld);
                   firstpass(p^.right);
                end;
              p^.location.loc := LOC_FLAGS;
              calcregisters(p,1,0,0);
              case pt of
                 equaln,unequaln,
                 ltn,lten,gtn,gten : ;
                 else CGMessage(type_e_mismatch);
              end;
              convdone:=true;
            end;

         { the general solution is to convert to 32 bit int }
         if not convdone then
           begin
              { but an int/int gives real/real! }
              if pt=slashn then
                begin
                   CGMessage(type_h_use_div_for_int);
                   p^.right:=gentypeconvnode(p^.right,bestrealdef^);
                   p^.left:=gentypeconvnode(p^.left,bestrealdef^);
                   firstpass(p^.left);
                   firstpass(p^.right);
                   p^.location.loc:=LOC_FPU;
                   { maybe we need an integer register to save }
                   { a reference                               }
                   if ((p^.left^.location.loc<>LOC_FPU) or
                       (p^.right^.location.loc<>LOC_FPU)) and
                       (p^.left^.registers32=p^.right^.registers32) then
                     calcregisters(p,1,1,0)
                   else
                     calcregisters(p,0,1,0);
                  { an add node always first loads both the left and the    }
                  { right in the fpu before doing the calculation. However, }
                  { calcregisters(0,2,0) will overestimate the number of    }
                  { necessary registers (it will make it 3 in case one of   }
                  { the operands is already in the fpu) (JM)                }
                  if ((p^.left^.location.loc <> LOC_FPU) or
                      (p^.right^.location.loc <> LOC_FPU)) and
                     (p^.registersfpu < 2) then
                    inc(p^.registersfpu);
                end
              else
                begin
                   p^.right:=gentypeconvnode(p^.right,s32bitdef);
                   p^.left:=gentypeconvnode(p^.left,s32bitdef);
                   firstpass(p^.left);
                   firstpass(p^.right);
                   p^.location.loc:=LOC_REGISTER;
                   calcregisters(p,1,0,0);
                end;
           end;

         if codegenerror then
           exit;

         { determines result type for comparions }
         { here the is a problem with multiple passes }
         { example length(s)+1 gets internal 'longint' type first }
         { if it is a arg it is converted to 'LONGINT' }
         { but a second first pass will reset this to 'longint' }
         case pt of
            ltn,lten,gtn,gten,equaln,unequaln:
              begin
                 if (not assigned(p^.resulttype)) or
                   (p^.resulttype^.deftype=stringdef) then
                   p^.resulttype:=booldef;
                 if is_64bitint(p^.left^.resulttype) then
                   p^.location.loc:=LOC_JUMP
                 else
                   p^.location.loc:=LOC_FLAGS;
              end;
            xorn:
              begin
                if not assigned(p^.resulttype) then
                  p^.resulttype:=p^.left^.resulttype;
                 p^.location.loc:=LOC_REGISTER;
              end;
            addn:
              begin
                if not assigned(p^.resulttype) then
                 begin
                 { for strings, return is always a 255 char string }
                   if is_shortstring(p^.left^.resulttype) then
                     p^.resulttype:=cshortstringdef
                   else
                    p^.resulttype:=p^.left^.resulttype;
                 end;
              end;
            else
              if not assigned(p^.resulttype) then
                p^.resulttype:=p^.left^.resulttype;
         end;
        if assigned(postconvdef) then
          begin
            p:=gentypeconvnode(p,postconvdef);
            firstpass(p);
          end;
      end;



end.
{
  $Log: tcadd.pas,v $
  Revision 1.1.2.19  2003/03/12 00:19:29  pierre
   * fix tuintint bug

  Revision 1.1.2.18  2003/01/07 19:19:43  peter
    * allow pchar=constantchar
    * support chararray=nil

  Revision 1.1.2.17  2002/11/14 17:31:13  pierre
   * fix problem with adding smallset to normset. Fixes tb0417.pp

  Revision 1.1.2.16  2002/10/08 17:08:18  jonas
    * fixed crsh of bugreport 2037

  Revision 1.1.2.15  2002/10/04 20:48:07  jonas
    * fixed web bug 2139: checking for division by zero fixed

  Revision 1.1.2.14  2002/09/07 11:04:30  carl
    * 2nd part of tw1996 bugfix (genordconstnode now has option to indicate if
      range must be verified), this also optimizes a bit.

  Revision 1.1.2.13  2002/03/07 21:38:21  carl
  * bug fix 1858 (parsing a constant with a typed constant -
    procinfo might not be valid!)

  Revision 1.1.2.12  2001/12/27 15:33:27  jonas
    * fixed fpuregister counting errors

  Revision 1.1.2.11  2001/10/11 14:36:30  jonas
    * fixed internalerror(10) due to previous fpu overflow fixes
    * NOTE: from now on, the location of a binary node must now always be set
       before you call calcregisters() for it

  Revision 1.1.2.10  2001/02/04 11:22:41  jonas
    * fixed web bug 1377

  Revision 1.1.2.9  2001/02/04 11:11:26  jonas
    * support for evaluation of constant pointer expressions

  Revision 1.1.2.8  2000/12/16 15:54:51  jonas
    + warning when there is a chance to get a range check error because of
      automatic type conversion to u32bit

  Revision 1.1.2.7  2000/12/13 12:25:40  jonas
    + also added 64bit conversion when using cardinals and signed
      expressions for div (in tcmat this time :)
    * removed automatic type conversion back to dword of 64bit results

  Revision 1.1.2.6  2000/12/11 14:14:10  jonas
    * no longer use left type for conversion mentioned in previous commit,
      since it may not be 32bit. Always use u32bitdef instead.

  Revision 1.1.2.5  2000/12/11 11:47:41  jonas
    * automatically typecast result of 64bit evaluations of 32bit operations
      (when using range checking or when dividing) back to 32bits.

  Revision 1.1.2.4  2000/12/08 17:03:23  jonas
    + added full range checking for 64bit types
    * fixed web bug 1144

  Revision 1.1.2.3  2000/09/10 20:19:03  peter
    * fixed crash with smallset -> normalset conversion

  Revision 1.1.2.2  2000/07/27 09:17:38  jonas
    * removed obsolete typeconversion (it got removed by the compiler in
      firsttypeconv anyway)

  Revision 1.1.2.1  2000/07/19 05:55:33  michael
  + Applied patch from Pierre

  Revision 1.1  2000/07/13 06:29:58  michael
  + Initial import

  Revision 1.79  2000/06/02 21:24:48  pierre
    * operator overloading now uses isbinaryoperatoracceptable
      and is unaryoperatoracceptable

  Revision 1.78  2000/05/31 06:58:41  florian
    * forgot to commit a fix for the enumeration subrange problem, yesterday

  Revision 1.77  2000/05/11 17:53:40  peter
    * small fix for previous commit

  Revision 1.76  2000/05/11 16:47:37  peter
    * fixed check for overloaded operator with array and chararray check

  Revision 1.75  2000/04/25 14:43:36  jonas
    - disabled "string_var := string_var + ... " and "string_var + char_var"
      optimizations (were only active with -dnewoptimizations) because of
      several internal issues

  Revision 1.74  2000/04/21 12:35:05  jonas
    + special code for string + char, between -dnewoptimizations

  Revision 1.73  2000/03/28 21:14:18  pierre
   * fix for bug 891

  Revision 1.72  2000/03/20 10:16:51  florian
   * fixed <dword>/<dword>, <int64>/<int64> and <qword>/<qword>

  Revision 1.71  2000/03/18 15:01:19  jonas
    * moved a $maxfpuregisters directive a bit up because it was being
      ignored

  Revision 1.70  2000/02/19 10:12:48  florian
    * fixed one more internalerror 10

  Revision 1.69  2000/02/17 14:53:42  florian
    * some updates for the newcg

  Revision 1.68  2000/02/14 22:34:28  florian
    * fixed another internalerror

  Revision 1.67  2000/02/13 22:46:28  florian
    * fixed an internalerror with writeln
    * fixed arrayconstructor_to_set to force the generation of better code
      and added a more strict type checking

  Revision 1.66  2000/02/13 14:21:51  jonas
    * modifications to make the compiler functional when compiled with
      -Or

  Revision 1.65  2000/02/09 13:23:06  peter
    * log truncated

  Revision 1.64  2000/02/04 08:47:10  florian
    * better register variable allocation in -Or mode

  Revision 1.63  2000/01/07 01:14:43  peter
    * updated copyright to 2000

  Revision 1.62  2000/01/04 20:10:20  florian
    * mmx support fixed

  Revision 1.61  1999/12/11 18:53:31  jonas
    * fixed type conversions of results of operations with cardinals
      (between -dcardinalmulfix)

  Revision 1.60  1999/12/09 23:18:04  pierre
   * no_fast_exit if procedure contains implicit termination code

  Revision 1.59  1999/12/01 12:42:33  peter
    * fixed bug 698
    * removed some notes about unused vars

  Revision 1.58  1999/11/30 10:40:56  peter
    + ttype, tsymlist

  Revision 1.57  1999/11/26 13:51:29  pierre
   * fix for overloading of shr shl mod and div

  Revision 1.56  1999/11/18 15:34:48  pierre
    * Notes/Hints for local syms changed to
      Set_varstate function

  Revision 1.55  1999/11/17 17:05:06  pierre
   * Notes/hints changes

  Revision 1.54  1999/11/16 23:45:28  pierre
   * global var token was changed by overload code (form bug 707)

  Revision 1.53  1999/11/15 21:53:42  peter
    * fixed constant eval for bool xor/or/and bool

  Revision 1.52  1999/11/15 17:53:00  pierre
    + one field added for ttoken record for operator
      linking the id to the corresponding operator token that
      can now now all be overloaded
    * overloaded operators are resetted to nil in InitSymtable
      (bug when trying to compile a uint that overloads operators twice)

  Revision 1.51  1999/11/06 14:34:29  peter
    * truncated log to 20 revs

  Revision 1.50  1999/09/27 23:45:00  peter
    * procinfo is now a pointer
    * support for result setting in sub procedure

  Revision 1.49  1999/09/16 13:39:14  peter
    * arrayconstructor 2 set conversion is now called always in the
      beginning of firstadd

  Revision 1.48  1999/09/15 20:35:45  florian
    * small fix to operator overloading when in MMX mode
    + the compiler uses now fldz and fld1 if possible
    + some fixes to floating point registers
    + some math. functions (arctan, ln, sin, cos, sqrt, sqr, pi) are now inlined
    * .... ???

  Revision 1.47  1999/09/13 16:28:05  peter
    * typo in previous commit open_array -> chararray :(

  Revision 1.46  1999/09/10 15:40:46  peter
    * fixed array check for operators, becuase array can also be a set

  Revision 1.45  1999/09/08 16:05:29  peter
    * pointer add/sub is now as expected and the same results as inc/dec

}
