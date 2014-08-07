{
    $Id: tccnv.pas,v 1.1.2.29 2003/03/03 15:54:09 carl Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    Type checking and register allocation for type converting nodes

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
{$ifdef TP}
  {$E+,F+,N+,D+,L+,Y+}
{$endif}
unit tccnv;
interface

    uses
      tree;

    procedure arrayconstructor_to_set(var p:ptree);

    procedure firsttypeconv(var p : ptree);
    procedure firstas(var p : ptree);
    procedure firstis(var p : ptree);


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
      htypechk,pass_1,cpubase;


{*****************************************************************************
                    Array constructor to Set Conversion
*****************************************************************************}

    procedure arrayconstructor_to_set(var p:ptree);
      var
        constp,
        buildp,
        p2,p3,p4    : ptree;
        pd        : pdef;
        constset    : pconstset;
        constsetlo,
        constsethi  : longint;

        procedure update_constsethi(p:pdef);
        begin
          if ((p^.deftype=orddef) and
             (porddef(p)^.high>=constsethi)) then
            begin
               constsethi:=porddef(p)^.high;
               if pd=nil then
                 begin
                    if (constsethi>255) or
                      (porddef(p)^.low<0) then
                      pd:=u8bitdef
                    else
                      pd:=p;
                 end;
               if constsethi>255 then
                 constsethi:=255;
            end
          else if ((p^.deftype=enumdef) and
            (penumdef(p)^.max>=constsethi)) then
            begin
               if pd=nil then
                 pd:=p;
               constsethi:=penumdef(p)^.max;
            end;
        end;

        procedure do_set(pos : longint);
        var
          mask,l : longint;
        begin
          if (pos>255) or (pos<0) then
           Message(parser_e_illegal_set_expr);
          if pos>constsethi then
           constsethi:=pos;
          if pos<constsetlo then
           constsetlo:=pos;
          { to do this correctly we use the 32bit array }
          l:=pos shr 5;
          mask:=1 shl (pos mod 32);
          { do we allow the same twice }
          if (pconst32bitset(constset)^[l] and mask)<>0 then
           Message(parser_e_illegal_set_expr);
          pconst32bitset(constset)^[l]:=pconst32bitset(constset)^[l] or mask;
        end;

      var
        l : longint;
        lr,hr : longint;

      begin
        new(constset);
        FillChar(constset^,sizeof(constset^),0);
        pd:=nil;
        constsetlo:=0;
        constsethi:=0;
        constp:=gensinglenode(setconstn,nil);
        constp^.value_set:=constset;
        buildp:=constp;
        if assigned(p^.left) then
         begin
           while assigned(p) do
            begin
              p4:=nil; { will contain the tree to create the set }
            { split a range into p2 and p3 }
              if p^.left^.treetype=arrayconstructrangen then
               begin
                 p2:=p^.left^.left;
                 p3:=p^.left^.right;
               { node is not used anymore }
                 putnode(p^.left);
               end
              else
               begin
                 p2:=p^.left;
                 p3:=nil;
               end;
              firstpass(p2);
              if assigned(p3) then
               firstpass(p3);
              if codegenerror then
               break;
              case p2^.resulttype^.deftype of
                 enumdef,
                 orddef:
                   begin
                      getrange(p2^.resulttype,lr,hr);
                      if assigned(p3) then
                       begin
                         { this isn't good, you'll get problems with
                           type t010 = 0..10;
                                ts = set of t010;
                           var  s : ts;b : t010
                           begin  s:=[1,2,b]; end.
                         if is_integer(p3^.resulttype) then
                          begin
                            p3:=gentypeconvnode(p3,u8bitdef);
                            firstpass(p3);
                          end;
                         }

                         if assigned(pd) and not(is_equal(pd,p3^.resulttype)) then
                           begin
                              aktfilepos:=p3^.fileinfo;
                              CGMessage(type_e_typeconflict_in_set);
                           end
                         else
                           begin
                             if (p2^.treetype=ordconstn) and (p3^.treetype=ordconstn) then
                              begin
                                 if not(is_integer(p3^.resulttype)) then
                                   pd:=p3^.resulttype
                                 else
                                   begin
                                      p3:=gentypeconvnode(p3,u8bitdef);
                                      p2:=gentypeconvnode(p2,u8bitdef);
                                      firstpass(p2);
                                      firstpass(p3);
                                   end;

                                for l:=p2^.value to p3^.value do
                                  do_set(l);
                                disposetree(p3);
                                disposetree(p2);
                              end
                             else
                              begin
                                update_constsethi(p2^.resulttype);
                                p2:=gentypeconvnode(p2,pd);
                                firstpass(p2);

                                update_constsethi(p3^.resulttype);
                                p3:=gentypeconvnode(p3,pd);
                                firstpass(p3);


                                if assigned(pd) then
                                  p3:=gentypeconvnode(p3,pd)
                                else
                                  p3:=gentypeconvnode(p3,u8bitdef);
                                firstpass(p3);
                                p4:=gennode(setelementn,p2,p3);
                              end;
                           end;
                       end
                      else
                       begin
                      { Single value }
                         if p2^.treetype=ordconstn then
                          begin
                            if not(is_integer(p2^.resulttype)) then
                              update_constsethi(p2^.resulttype)
                            else
                              begin
                                 p2:=gentypeconvnode(p2,u8bitdef);
                                 firstpass(p2);
                              end;

                            do_set(p2^.value);
                            disposetree(p2);
                          end
                         else
                          begin
                            update_constsethi(p2^.resulttype);

                            if assigned(pd) then
                              p2:=gentypeconvnode(p2,pd)
                            else
                              p2:=gentypeconvnode(p2,u8bitdef);
                            firstpass(p2);

                            p4:=gennode(setelementn,p2,nil);
                          end;
                       end;
                    end;
          stringdef : begin
                        { if we've already set elements which are constants }
                        { throw an error                                    }
                        if ((pd=nil) and assigned(buildp)) or
                          not(is_equal(pd,cchardef)) then
                          CGMessage(type_e_typeconflict_in_set)
                        else
                         for l:=1 to length(pstring(p2^.value_str)^) do
                          do_set(ord(pstring(p2^.value_str)^[l]));
                        if pd=nil then
                         pd:=cchardef;
                        disposetree(p2);
                      end;
              else
               CGMessage(type_e_ordinal_expr_expected);
              end;
            { insert the set creation tree }
              if assigned(p4) then
               buildp:=gennode(addn,buildp,p4);
            { load next and dispose current node }
              p2:=p;
              p:=p^.right;
              putnode(p2);
            end;
          if (pd=nil) then
            begin
               pd:=u8bitdef;
               constsethi:=255;
            end;
         end
        else
         begin
         { empty set [], only remove node }
           putnode(p);
         end;
      { set the initial set type }
        constp^.resulttype:=new(psetdef,init(pd,constsethi));
        firstpass(buildp);
      { set the new tree }
        p:=buildp;
      end;


{*****************************************************************************
                             FirstTypeConv
*****************************************************************************}

    type
       tfirstconvproc = procedure(var p : ptree);

    procedure first_int_to_int(var p : ptree);
      begin
        if (p^.left^.location.loc<>LOC_REGISTER) and
           (p^.resulttype^.size>p^.left^.resulttype^.size) then
           p^.location.loc:=LOC_REGISTER;
        if is_64bitint(p^.resulttype) then
          p^.registers32:=max(p^.registers32,2)
        else
          p^.registers32:=max(p^.registers32,1);
      end;


    procedure first_cstring_to_pchar(var p : ptree);
      begin
         p^.registers32:=1;
         p^.location.loc:=LOC_REGISTER;
      end;


    procedure first_string_to_chararray(var p : ptree);
      begin
         p^.registers32:=1;
         p^.location.loc:=LOC_REGISTER;
      end;


    procedure first_string_to_string(var p : ptree);
      var
        hp : ptree;
      begin
         if pstringdef(p^.resulttype)^.string_typ<>
            pstringdef(p^.left^.resulttype)^.string_typ then
           begin
              if (p^.left^.treetype=stringconstn) and
                 not (pstringdef(p^.resulttype)^.string_typ in [st_ansistring,st_widestring]) then
                begin
                   p^.left^.stringtype:=pstringdef(p^.resulttype)^.string_typ;
                   p^.left^.resulttype:=p^.resulttype;
                   { remove typeconv node }
                   hp:=p;
                   p:=p^.left;
                   putnode(hp);
                   exit;
                end
              else
                procinfo^.flags:=procinfo^.flags or pi_do_call;
           end;
         { for simplicity lets first keep all ansistrings
           as LOC_MEM, could also become LOC_REGISTER }
         if pstringdef(p^.resulttype)^.string_typ in [st_ansistring,st_widestring] then
           { we may use ansistrings so no fast exit here }
           procinfo^.no_fast_exit:=true;
         p^.location.loc:=LOC_MEM;
      end;


    procedure first_char_to_string(var p : ptree);
      var
         hp : ptree;
      begin
         if p^.left^.treetype=ordconstn then
           begin
              hp:=genstringconstnode(chr(p^.left^.value),st_default);
              hp^.stringtype:=pstringdef(p^.resulttype)^.string_typ;
              firstpass(hp);
              disposetree(p);
              p:=hp;
           end
         else
           p^.location.loc:=LOC_MEM;
      end;


    procedure first_char_to_chararray(var p: ptree);
      var
        hp : ptree;
      begin
        if (p^.resulttype^.size <> 1) then
          begin
            { convert first to string, then to chararray }
             hp := gentypeconvnode(gentypeconvnode(p^.left,cshortstringdef),
              p^.resulttype);
            { do an explicit firstpass of p^.left, it won't be done implicitely }
            { because gentypeconvnode() already sets the resulttype (JM)        }
            firstpass(hp^.left);
            firstpass(hp);
            p^.left := nil;
            disposetree(p);
            p := hp;
          end
        else
          { a chararray with 1 element is the same as a char }
          set_location(p^.location,p^.left^.location);
      end;

    procedure first_nothing(var p : ptree);
      begin
         p^.location.loc:=LOC_MEM;
      end;


    procedure first_array_to_pointer(var p : ptree);
      begin
         if p^.registers32<1 then
           p^.registers32:=1;
         p^.location.loc:=LOC_REGISTER;
      end;


    procedure first_int_to_real(var p : ptree);
      var
        t : ptree;
      begin
        if p^.left^.treetype=ordconstn then
         begin
           t:=genrealconstnode(p^.left^.value,pfloatdef(p^.resulttype));
           firstpass(t);
           disposetree(p);
           p:=t;
           exit;
         end;
{$ifdef m68k}
         if (cs_fp_emulation in aktmoduleswitches) or
            (pfloatdef(p^.resulttype)^.typ=s32real) or
            (pfloatdef(p^.resulttype)^.typ=f32bit) then
           begin
             if p^.registers32<1 then
               p^.registers32:=1;
           end
         else
           if p^.registersfpu<1 then
             p^.registersfpu:=1;
{$else not m68k}
         if p^.registersfpu<1 then
           p^.registersfpu:=1;
{$endif not m68k}
        p^.location.loc:=LOC_FPU;
      end;


    procedure first_int_to_fix(var p : ptree);
      var
        t : ptree;
      begin
        if p^.left^.treetype=ordconstn then
         begin
           t:=genfixconstnode(p^.left^.value shl 16,p^.resulttype);
           firstpass(t);
           disposetree(p);
           p:=t;
           exit;
         end;
        if p^.registers32<1 then
         p^.registers32:=1;
        p^.location.loc:=LOC_REGISTER;
      end;


    procedure first_real_to_fix(var p : ptree);
      var
        t : ptree;
      begin
        if p^.left^.treetype=fixconstn then
         begin
           t:=genfixconstnode(round(p^.left^.value_real*65536),p^.resulttype);
           firstpass(t);
           disposetree(p);
           p:=t;
           exit;
         end;
        { at least one fpu and int register needed }
        if p^.registers32<1 then
          p^.registers32:=1;
        if p^.registersfpu<1 then
          p^.registersfpu:=1;
        p^.location.loc:=LOC_REGISTER;
      end;


    procedure first_fix_to_real(var p : ptree);
      var
        t : ptree;
      begin
        if p^.left^.treetype=fixconstn then
          begin
            t:=genrealconstnode(round(p^.left^.value_fix/65536.0),p^.resulttype);
            firstpass(t);
            disposetree(p);
            p:=t;
            exit;
          end;
{$ifdef m68k}
         if (cs_fp_emulation in aktmoduleswitches) or
            (pfloatdef(p^.resulttype)^.typ=s32real) or
            (pfloatdef(p^.resulttype)^.typ=f32bit) then
           begin
             if p^.registers32<1 then
               p^.registers32:=1;
           end
         else
           if p^.registersfpu<1 then
             p^.registersfpu:=1;
{$else not m68k}
         if p^.registersfpu<1 then
           p^.registersfpu:=1;
{$endif not m68k}
        p^.location.loc:=LOC_FPU;
      end;


    procedure first_real_to_real(var p : ptree);
      var
        t : ptree;
      begin
         if p^.left^.treetype=realconstn then
           begin
             t:=genrealconstnode(p^.left^.value_real,p^.resulttype);
             firstpass(t);
             disposetree(p);
             p:=t;
             exit;
           end;
        { comp isn't a floating type }
{$ifdef i386}
         if (pfloatdef(p^.resulttype)^.typ=s64comp) and
            (pfloatdef(p^.left^.resulttype)^.typ<>s64comp) and
            not (p^.explizit) then
           CGMessage(type_w_convert_real_2_comp);
{$endif}
{$ifdef m68k}
         if (cs_fp_emulation in aktmoduleswitches) or
            (pfloatdef(p^.resulttype)^.typ=s32real) or
            (pfloatdef(p^.resulttype)^.typ=f32bit) then
           begin
             if p^.registers32<1 then
               p^.registers32:=1;
           end
         else
           if p^.registersfpu<1 then
             p^.registersfpu:=1;
{$else not m68k}
         if p^.registersfpu<1 then
           p^.registersfpu:=1;
{$endif not m68k}
         p^.location.loc:=LOC_FPU;
      end;


    procedure first_pointer_to_array(var p : ptree);
      begin
         if p^.registers32<1 then
           p^.registers32:=1;
         p^.location.loc:=LOC_REFERENCE;
      end;


    procedure first_chararray_to_string(var p : ptree);
      begin
         { the only important information is the location of the }
         { result                                               }
         { other stuff is done by firsttypeconv           }
         p^.location.loc:=LOC_MEM;
      end;


    procedure first_cchar_to_pchar(var p : ptree);
      begin
         p^.left:=gentypeconvnode(p^.left,cshortstringdef);
         { convert constant char to constant string }
         firstpass(p^.left);
         { evalute tree }
         firstpass(p);
      end;


    procedure first_bool_to_int(var p : ptree);
      begin
         { byte(boolean) or word(wordbool) or longint(longbool) must
         be accepted for var parameters }
         if (p^.explizit) and
            (p^.left^.resulttype^.size=p^.resulttype^.size) and
            (p^.left^.location.loc in [LOC_REFERENCE,LOC_MEM,LOC_CREGISTER]) then
           exit;
         p^.location.loc:=LOC_REGISTER;
         if p^.registers32<1 then
           p^.registers32:=1;
      end;


    procedure first_int_to_bool(var p : ptree);
      begin
         { byte(boolean) or word(wordbool) or longint(longbool) must
         be accepted for var parameters }
         if (p^.explizit) and
            (p^.left^.resulttype^.size=p^.resulttype^.size) and
            (p^.left^.location.loc in [LOC_REFERENCE,LOC_MEM,LOC_CREGISTER]) then
           exit;
         p^.location.loc:=LOC_REGISTER;
         { need if bool to bool !!
           not very nice !!
         p^.left:=gentypeconvnode(p^.left,s32bitdef);
         p^.left^.explizit:=true;
         firstpass(p^.left);  }
         if p^.registers32<1 then
           p^.registers32:=1;
      end;


    procedure first_bool_to_bool(var p : ptree);
      begin
         p^.location.loc:=LOC_REGISTER;
         if p^.registers32<1 then
           p^.registers32:=1;
      end;


    procedure first_proc_to_procvar(var p : ptree);
      var
        pd : pprocdef;
        found : boolean;
      begin
         { hmmm, I'am not sure if that is necessary (FK) }
         firstpass(p^.left);
         if codegenerror then
           exit;

         if (p^.left^.location.loc<>LOC_REFERENCE) then
           CGMessage(cg_e_illegal_expression);

         p^.registers32:=p^.left^.registers32;
         if p^.registers32<1 then
           p^.registers32:=1;
         p^.location.loc:=LOC_REGISTER;

         if (p^.left^.resulttype^.deftype=procdef) and
            (p^.resulttype^.deftype=procvardef) and
            assigned(pprocdef(p^.left^.resulttype)^.nextoverloaded) then
           begin
             pd:=pprocdef(p^.left^.resulttype);
             found:=false;
             while assigned(pd) do
               begin
                 if proc_to_procvar_equal(pd,pprocvardef(p^.resulttype),not p^.explizit,true) then
                   begin
                     if found then
                       CGMessage(cg_e_cant_choose_overload_function)
                     else
                       begin
                         found:=true;
                         p^.left^.resulttype:=pd;
                       end;
                   end;
                 pd:=pd^.nextoverloaded;
               end;
           end;
      end;


    procedure first_load_smallset(var p : ptree);
      begin
      end;


    procedure first_cord_to_pointer(var p : ptree);
      var
        t : ptree;
      begin
        if p^.left^.treetype=ordconstn then
          begin
            t:=genpointerconstnode(p^.left^.value,p^.resulttype);
            firstpass(t);
            disposetree(p);
            p:=t;
            exit;
          end
        else
          internalerror(432472389);
      end;


    procedure first_pchar_to_string(var p : ptree);
      begin
         p^.location.loc:=LOC_REFERENCE;
      end;


    procedure first_ansistring_to_pchar(var p : ptree);
      begin
         p^.location.loc:=LOC_REGISTER;
         if p^.registers32<1 then
           p^.registers32:=1;
      end;


    procedure first_arrayconstructor_to_set(var p:ptree);
      var
        hp : ptree;
      begin
        if p^.left^.treetype<>arrayconstructn then
         internalerror(5546);
      { remove typeconv node }
        hp:=p;
        p:=p^.left;
        putnode(hp);
      { create a set constructor tree }
        arrayconstructor_to_set(p);
      { now firstpass the set }
        firstpass(p);
      end;


  procedure firsttypeconv(var p : ptree);
    var
      hp : ptree;
      currprocdef : pprocdef;
      aprocdef : pprocdef;
      enable_range_check: boolean;
    const
       firstconvert : array[tconverttype] of tfirstconvproc = (
         {$ifndef tp}@{$endif}first_nothing, {equal}
         {$ifndef tp}@{$endif}first_nothing, {not_possible}
         {$ifndef tp}@{$endif}first_string_to_string,
         {$ifndef tp}@{$endif}first_char_to_string,
         {$ifndef tp}@{$endif}first_char_to_chararray,
         {$ifndef tp}@{$endif}first_pchar_to_string,
         {$ifndef tp}@{$endif}first_cchar_to_pchar,
         {$ifndef tp}@{$endif}first_cstring_to_pchar,
         {$ifndef tp}@{$endif}first_ansistring_to_pchar,
         {$ifndef tp}@{$endif}first_string_to_chararray,
         {$ifndef tp}@{$endif}first_chararray_to_string,
         {$ifndef tp}@{$endif}first_array_to_pointer,
         {$ifndef tp}@{$endif}first_pointer_to_array,
         {$ifndef tp}@{$endif}first_int_to_int,
         {$ifndef tp}@{$endif}first_int_to_bool,
         {$ifndef tp}@{$endif}first_bool_to_bool,
         {$ifndef tp}@{$endif}first_bool_to_int,
         {$ifndef tp}@{$endif}first_real_to_real,
         {$ifndef tp}@{$endif}first_int_to_real,
         {$ifndef tp}@{$endif}first_int_to_fix,
         {$ifndef tp}@{$endif}first_real_to_fix,
         {$ifndef tp}@{$endif}first_fix_to_real,
         {$ifndef tp}@{$endif}first_proc_to_procvar,
         {$ifndef tp}@{$endif}first_arrayconstructor_to_set,
         {$ifndef tp}@{$endif}first_load_smallset,
         {$ifndef tp}@{$endif}first_cord_to_pointer
       );
     begin
       aprocdef:=nil;
       { if explicite type cast, then run firstpass }
       if (p^.explizit) or not assigned(p^.left^.resulttype) then
         firstpass(p^.left);
       if (p^.left^.treetype=typen) and (p^.left^.resulttype=generrordef) then
         begin
            codegenerror:=true;
            Message(parser_e_no_type_not_allowed_here);
         end;
       if codegenerror then
         begin
           p^.resulttype:=generrordef;
           exit;
         end;

       if not assigned(p^.left^.resulttype) then
        begin
          codegenerror:=true;
          internalerror(52349);
          exit;
        end;

       { load the value_str from the left part }
       p^.registers32:=p^.left^.registers32;
       p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
       p^.registersmmx:=p^.left^.registersmmx;
{$endif}
       set_location(p^.location,p^.left^.location);

       { remove obsolete type conversions }
       if is_equal(p^.left^.resulttype,p^.resulttype) then
         begin
         { becuase is_equal only checks the basetype for sets we need to
           check here if we are loading a smallset into a normalset }
           if (p^.resulttype^.deftype=setdef) and
              (p^.left^.resulttype^.deftype=setdef) and
              (psetdef(p^.resulttype)^.settype<>smallset) and
              (psetdef(p^.left^.resulttype)^.settype=smallset) then
            begin
            { try to define the set as a normalset if it's a constant set }
              if p^.left^.treetype=setconstn then
               begin
                 p^.resulttype:=p^.left^.resulttype;
                 psetdef(p^.resulttype)^.settype:=normset
               end
              else
               p^.convtyp:=tc_load_smallset;
              exit;
            end
           else
            begin
              hp:=p;
              p:=p^.left;
              p^.resulttype:=hp^.resulttype;
              putnode(hp);
              exit;
            end;
         end;
       aprocdef:=assignment_overloaded(p^.left^.resulttype,p^.resulttype);
       if assigned(aprocdef) then
         begin
            procinfo^.flags:=procinfo^.flags or pi_do_call;
            hp:=gencallnode(overloaded_operators[_assignment],nil);
            { tell explicitly which def we must use !! (PM) }
            hp^.procdefinition:=aprocdef;
            hp^.left:=gencallparanode(p^.left,nil);
            putnode(p);
            p:=hp;
            firstpass(p);
            exit;
         end;

       if isconvertable(p^.left^.resulttype,p^.resulttype,p^.convtyp,p^.left^.treetype,p^.explizit,true,nil)=0 then
         begin
           {Procedures have a resulttype of voiddef and functions of their
           own resulttype. They will therefore always be incompatible with
           a procvar. Because isconvertable cannot check for procedures we
           use an extra check for them.}
           if (m_tp_procvar in aktmodeswitches) then
            begin
              if (p^.resulttype^.deftype=procvardef) and
                 (is_procsym_load(p^.left) or is_procsym_call(p^.left)) then
               begin
                 if is_procsym_call(p^.left) then
                  begin
                    if assigned(p^.left^.right) then
                     begin
                       hp:=getcopy(p^.left^.right);
                       disposetree(p^.left);
                       firstpass(hp);
                       p^.left:=hp;
                       aprocdef:=pprocdef(p^.left^.resulttype);
                     end
                    else
                     begin
                       currprocdef:=get_proc_2_procvar_def(p^.left^.symtableprocentry,pprocvardef(p^.resulttype));
                       if assigned(currprocdef) then
                        begin
                          if (p^.left^.symtableprocentry^.owner^.symtabletype=objectsymtable) and
                             assigned(p^.left^.methodpointer) and
                             (p^.left^.methodpointer^.treetype<>typen) then
                           hp:=genloadmethodcallnode(pprocsym(p^.left^.symtableprocentry),currprocdef,p^.left^.symtableproc,
                                 getcopy(p^.left^.methodpointer))
                          else
                           hp:=genloadcallnode(pprocsym(p^.left^.symtableprocentry),currprocdef,p^.left^.symtableproc);
                          disposetree(p^.left);
                          firstpass(hp);
                          p^.left:=hp;
                          aprocdef:=pprocdef(p^.left^.resulttype);
                        end;
                     end;
                  end
                 else
                  begin
                    if (p^.left^.treetype<>addrn) then
                      aprocdef:=pprocsym(p^.left^.symtableentry)^.definition;
                  end;
                 p^.convtyp:=tc_proc_2_procvar;
                 { Now check if the procedure we are going to assign to
                   the procvar,  is compatible with the procvar's type }
                 if assigned(aprocdef) then
                  begin
                    firstconvert[p^.convtyp](p);
                  end
                 else
                  CGMessage2(type_e_incompatible_types,p^.left^.resulttype^.typename,p^.resulttype^.typename);
                 exit;
               end;
            end;
           if p^.explizit then
            begin
              { check if the result could be in a register }
              if not(p^.resulttype^.is_intregable) and
                not(p^.resulttype^.is_fpuregable) then
                make_not_regable(p^.left);
              { boolean to byte are special because the
                location can be different }

              if is_integer(p^.resulttype) and
                 is_boolean(p^.left^.resulttype) then
               begin
                  p^.convtyp:=tc_bool_2_int;
                  firstconvert[p^.convtyp](p);
                  exit;
               end;
              { ansistring to pchar }
              if is_pchar(p^.resulttype) and
                 is_ansistring(p^.left^.resulttype) then
               begin
                 p^.convtyp:=tc_ansistring_2_pchar;
                 firstconvert[p^.convtyp](p);
                 exit;
               end;
              { do common tc_equal cast }
              p^.convtyp:=tc_equal;

              { enum to ordinal will always be s32bit }
              if (p^.left^.resulttype^.deftype=enumdef) and
                 is_ordinal(p^.resulttype) then
               begin
                 if p^.left^.treetype=ordconstn then
                  begin
                    hp:=genordinalconstnode(p^.left^.value,p^.resulttype,true);
                    disposetree(p);
                    firstpass(hp);
                    p:=hp;
                    exit;
                  end
                 else
                  begin
                    if isconvertable(s32bitdef,p^.resulttype,p^.convtyp,ordconstn,false,true,nil)=0 then
                      CGMessage2(type_e_incompatible_types,p^.left^.resulttype^.typename,p^.resulttype^.typename);
                  end;
               end

              { ordinal to enumeration }
              else
               if (p^.resulttype^.deftype=enumdef) and
                  is_ordinal(p^.left^.resulttype) then
                begin
                  if p^.left^.treetype=ordconstn then
                   begin
                     hp:=genordinalconstnode(p^.left^.value,p^.resulttype,true);
                     disposetree(p);
                     firstpass(hp);
                     p:=hp;
                     exit;
                   end
                  else
                   begin
                     if IsConvertable(p^.left^.resulttype,s32bitdef,p^.convtyp,ordconstn,false,true,nil)=0 then
                       CGMessage2(type_e_incompatible_types,p^.left^.resulttype^.typename,p^.resulttype^.typename);
                   end;
                end

              { nil to ordinal node }
              else if is_ordinal(p^.resulttype) and
                (p^.left^.treetype=niln) then
                begin
                   hp:=genordinalconstnode(0,p^.resulttype,true);
                   firstpass(hp);
                   disposetree(p);
                   p:=hp;
                   exit;
                end
              { pointerconstnode to ordinal }
              else if is_ordinal(p^.resulttype) and
                (p^.left^.treetype = pointerconstn) then
                begin
                  hp:=genordinalconstnode(p^.left^.value,p^.resulttype,true);
                  firstpass(hp);
                  disposetree(p);
                  p:=hp;
                  exit;
                end

              {Are we typecasting an ordconst to a char?}
              else
                if is_char(p^.resulttype) and
                   is_ordinal(p^.left^.resulttype) then
                 begin
                   if p^.left^.treetype=ordconstn then
                    begin
                      hp:=genordinalconstnode(p^.left^.value,p^.resulttype,true);
                      firstpass(hp);
                      disposetree(p);
                      p:=hp;
                      exit;
                    end
                   else
                    begin
                      if IsConvertable(p^.left^.resulttype,u8bitdef,p^.convtyp,ordconstn,false,true,nil)=0 then
                        CGMessage2(type_e_incompatible_types,p^.left^.resulttype^.typename,p^.resulttype^.typename);
                    end;
                 end

              { Are we char to ordinal }
              else
                if is_char(p^.left^.resulttype) and
                   is_ordinal(p^.resulttype) then
                 begin
                   if p^.left^.treetype=ordconstn then
                    begin
                      hp:=genordinalconstnode(p^.left^.value,p^.resulttype,true);
                      firstpass(hp);
                      disposetree(p);
                      p:=hp;
                      exit;
                    end
                   else
                    begin
                      if IsConvertable(u8bitdef,p^.resulttype,p^.convtyp,ordconstn,false,true,nil)=0 then
                        CGMessage2(type_e_incompatible_types,p^.left^.resulttype^.typename,p^.resulttype^.typename);
                    end;
                 end

               { only if the same size or formal def }
               { why do we allow typecasting of voiddef ?? (PM) }
               else
                begin
                  if not(
                       (p^.left^.resulttype^.deftype=formaldef) or
                       (p^.left^.resulttype^.size=p^.resulttype^.size) or
                       (is_equal(p^.left^.resulttype,voiddef)  and
                       (p^.left^.treetype=derefn))
                     ) or
                     (p^.left^.resulttype^.deftype=classrefdef) then
                    CGMessage(cg_e_illegal_type_conversion);
                  if ((p^.left^.resulttype^.deftype=orddef) and
                      (p^.resulttype^.deftype=pointerdef)) or
                      ((p^.resulttype^.deftype=orddef) and
                       (p^.left^.resulttype^.deftype=pointerdef))
                       {$ifdef extdebug}and (p^.firstpasscount=0){$endif} then
                    CGMessage(cg_d_pointer_to_longint_conv_not_portable);
                end;

               { the conversion into a strutured type is only }
               { possible, if the source is no register    }
               if ((p^.resulttype^.deftype in [recorddef,stringdef,arraydef]) or
                   ((p^.resulttype^.deftype=objectdef) and not(pobjectdef(p^.resulttype)^.is_class))
                  ) and (p^.left^.location.loc in [LOC_REGISTER,LOC_CREGISTER]) { and
                   it also works if the assignment is overloaded
                   YES but this code is not executed if assignment is overloaded (PM)
                  not assigned(assignment_overloaded(p^.left^.resulttype,p^.resulttype))} then
                 CGMessage(cg_e_illegal_type_conversion);
            end
           else
            CGMessage2(type_e_incompatible_types,p^.left^.resulttype^.typename,p^.resulttype^.typename);
         end;

       { tp7 procvar support, when right is not a procvardef and we got a
         loadn of a procvar then convert to a calln, the check for the
         result is already done in is_convertible, also no conflict with
         @procvar is here because that has an extra addrn }
         if (m_tp_procvar in aktmodeswitches) and
            (p^.resulttype^.deftype<>procvardef) and
            (p^.left^.treetype=loadn) and
            (p^.left^.resulttype^.deftype=procvardef) and
            (pprocvardef(p^.left^.resulttype)^.para^.count=0) then
          begin
            hp:=gencallnode(nil,nil);
            hp^.right:=p^.left;
            firstpass(hp);
            p^.left:=hp;
          end;

        { remove typeconv after niln }
        if (p^.left^.treetype=niln) then
          begin
            if p^.resulttype^.deftype=orddef then
             begin
               hp:=genordinalconstnode(0,p^.resulttype,false);
             end
            else
             begin
               hp:=p^.left;
               hp^.resulttype:=p^.resulttype;
               p^.left:=nil;
             end;
            disposetree(p);
            firstpass(hp);
            p:=hp;
            exit;
          end;

        { ordinal contants can be directly converted }
        { but not int64/qword                        }
        if (p^.left^.treetype=ordconstn) and is_ordinal(p^.resulttype) and
          not(is_64bitint(p^.resulttype)) then
          begin
             { range checking is done in genordinalconstnode (PFV) }
             { disable for explicit type casts (JM) }
             if p^.explizit and
                (cs_check_range in aktlocalswitches) then
               begin
                 exclude(aktlocalswitches,cs_check_range);
                 enable_range_check := true;
               end
             else
               enable_range_check := false;

             hp:=genordinalconstnode(p^.left^.value,p^.resulttype,true);

             { do sign extension if necessary (JM) }
             if not (cs_check_range in aktlocalswitches) and
                is_signed(p^.resulttype) then
               with hp^ do
                 case p^.resulttype^.size of
                   1: value := shortint(value);
                   2: value := smallint(value);
                   4: value := longint(value);
                 end;
             if enable_range_check then
               include(aktlocalswitches,cs_check_range);

             disposetree(p);
             firstpass(hp);
             p:=hp;
             exit;
          end;
        if p^.convtyp<>tc_equal then
          firstconvert[p^.convtyp](p);
      end;


{*****************************************************************************
                                FirstIs
*****************************************************************************}

    procedure firstis(var p : ptree);
       var
        error : boolean;
      begin
         error := false;
         firstpass(p^.left);
         set_varstate(p^.left,true);
         firstpass(p^.right);
         set_varstate(p^.right,true);
         if codegenerror then
           exit;

         if (p^.right^.resulttype^.deftype<>classrefdef) then
           CGMessage(type_e_mismatch);

         left_right_max(p);

         { left must be a class }
         if (p^.left^.resulttype^.deftype<>objectdef) or
            not(pobjectdef(p^.left^.resulttype)^.is_class) then
           CGMessage(type_e_mismatch);

        
         if codegenerror then
           exit;
         { the operands must be related }
         if (not(pobjectdef(p^.left^.resulttype)^.is_related(
           pobjectdef(pclassrefdef(p^.right^.resulttype)^.pointertype.def)))) and
           (not(pobjectdef(pclassrefdef(p^.right^.resulttype)^.pointertype.def)^.is_related(
           pobjectdef(p^.left^.resulttype)))) then
           CGMessage(type_e_mismatch);

         p^.location.loc:=LOC_FLAGS;
         p^.resulttype:=booldef;
      end;


{*****************************************************************************
                                FirstAs
*****************************************************************************}

    procedure firstas(var p : ptree);
      begin
         firstpass(p^.right);
         set_varstate(p^.right,true);
         firstpass(p^.left);
         set_varstate(p^.left,true);
         if codegenerror then
           exit;

         if (p^.right^.resulttype^.deftype<>classrefdef) then
           CGMessage(type_e_mismatch);

         left_right_max(p);

         { left must be a class }
         if (p^.left^.resulttype^.deftype=objectdef) and
            (pobjectdef(p^.left^.resulttype)^.is_class) then
          begin
            { the operands must be related }
            if (not(pobjectdef(p^.left^.resulttype)^.is_related(
               pobjectdef(pclassrefdef(p^.right^.resulttype)^.pointertype.def)))) and
               (not(pobjectdef(pclassrefdef(p^.right^.resulttype)^.pointertype.def)^.is_related(
               pobjectdef(p^.left^.resulttype)))) then
              CGMessage(type_e_mismatch);
          end
         else
          CGMessage(type_e_mismatch);

         set_location(p^.location,p^.left^.location);
         p^.resulttype:=pclassrefdef(p^.right^.resulttype)^.pointertype.def;
      end;


end.
{
  $Log: tccnv.pas,v $
  Revision 1.1.2.29  2003/03/03 15:54:09  carl
    + 2403 bugfix

  Revision 1.1.2.28  2003/01/14 22:34:47  peter
    * only convert procvars to callnodes when they don't require
      parameters

  Revision 1.1.2.27  2003/01/06 21:20:31  peter
    * proc_to_procvar_equal gets parameter if an error should be
      written for method-procvar mismatch

  Revision 1.1.2.26  2003/01/05 23:06:03  peter
    * fix nil to orddef conv

  Revision 1.1.2.25  2003/01/05 18:49:48  peter
    * isconvertable has extra para to check for operator or not

  Revision 1.1.2.24  2003/01/05 18:06:29  peter
    * don't allow conversion from classrefdef to class

  Revision 1.1.2.23  2003/01/05 16:27:01  peter
    * fixed crash in as when the type was not a class

  Revision 1.1.2.22  2002/12/22 16:31:14  peter
    * fix crash with exit(procvar)

  Revision 1.1.2.21  2002/11/18 13:49:32  pierre
   * fix tbs/tb0251.pp failure

  Revision 1.1.2.20  2002/10/28 23:04:27  pierre
   * fix bug in test/units/system/tsetstr.pp

  Revision 1.1.2.19  2002/09/08 08:51:14  carl
    * bugfix for report 2109

  Revision 1.1.2.18  2002/09/07 11:04:29  carl
    * 2nd part of tw1996 bugfix (genordconstnode now has option to indicate if
      range must be verified), this also optimizes a bit.

  Revision 1.1.2.17  2001/11/02 23:19:10  jonas
    * fixed web bug 1665 (allow char to chararray type conversion)

  Revision 1.1.2.16  2001/10/28 17:18:12  peter
    * allow assignment of overloaded procedures to procvars when we know
      which procedure to take

  Revision 1.1.2.15  2001/08/05 13:19:27  peter
    * fix for proc of obj=nil

  Revision 1.1.2.14  2001/07/25 13:08:35  pierre
   * use tconst32bitset type for sets

  Revision 1.1.2.13  2001/07/17 14:10:11  pierre
   * m68k in emulation mode uses 1 normal register for conversion to float

  Revision 1.1.2.12  2001/06/06 14:27:16  jonas
    * fixed wrong typed constant procvars in preparation of my fix which will
      disallow them in FPC mode

  Revision 1.1.2.11  2001/03/08 17:43:35  jonas
    * fixed web bug 1430 (made a typo)

  Revision 1.1.2.10  2001/03/08 17:42:48  jonas
    * fixed web bug 1430

  Revision 1.1.2.9  2001/02/21 11:41:15  jonas
    * evaluate typecasts of const pointers to ordinals inline

  Revision 1.1.2.8  2001/02/20 21:38:03  peter
    * removed unused code

  Revision 1.1.2.7  2000/12/08 12:34:10  jonas
    * really fixed sign extension (previous fix simply restored the original
      behaviour)

  Revision 1.1.2.6  2000/12/08 12:12:44  jonas
    * fixed stupid bug in sign extension fix from yesterday

  Revision 1.1.2.5  2000/12/07 16:42:22  jonas
    * sign extension for explicit typecasts on constants

  Revision 1.1.2.4  2000/08/26 19:41:44  peter
    * integer(char) explicit typecast support (tp7,delphi compatible)

  Revision 1.1.2.3  2000/08/02 07:14:28  jonas
    - undid my changes from the previous two commits because it was a bug
      in cg386cnv which I've now fixed (previous changes only masked it in
      some cases)

  Revision 1.1.2.2  2000/08/01 10:38:10  jonas
    * refined my previous IE(10) fix (in some cases, too many registers could
      be reserved)

  Revision 1.1.2.1  2000/07/19 18:15:04  jonas
    * fixed IE(10) when using -Or and shortstring->ansistring conversions

  Revision 1.1  2000/07/13 06:29:59  michael
  + Initial import

  Revision 1.62  2000/03/14 15:05:18  pierre
   * fix for bug 866

  Revision 1.61  2000/02/14 18:12:50  florian
    * fixed set problem s:=[<word>];

  Revision 1.60  2000/02/13 22:46:28  florian
    * fixed an internalerror with writeln
    * fixed arrayconstructor_to_set to force the generation of better code
      and added a more strict type checking

  Revision 1.59  2000/02/09 13:23:07  peter
    * log truncated

  Revision 1.58  2000/01/09 23:16:07  peter
    * added st_default stringtype
    * genstringconstnode extended with stringtype parameter using st_default
      will do the old behaviour

  Revision 1.57  2000/01/07 01:14:44  peter
    * updated copyright to 2000

  Revision 1.56  1999/12/19 12:08:27  florian
    * bug reported by Alex S. fixed: it wasn't possible to type cast nil in const
      declarations: const l = longint(nil);

  Revision 1.55  1999/12/09 23:18:04  pierre
   * no_fast_exit if procedure contains implicit termination code

  Revision 1.54  1999/11/30 10:40:57  peter
    + ttype, tsymlist

  Revision 1.53  1999/11/18 15:34:49  pierre
    * Notes/Hints for local syms changed to
      Set_varstate function

  Revision 1.52  1999/11/06 14:34:29  peter
    * truncated log to 20 revs

  Revision 1.51  1999/11/05 13:15:00  florian
    * some fixes to get the new cg compiling again

  Revision 1.50  1999/09/27 23:45:00  peter
    * procinfo is now a pointer
    * support for result setting in sub procedure

  Revision 1.49  1999/09/26 21:30:22  peter
    + constant pointer support which can happend with typecasting like
      const p=pointer(1)
    * better procvar parsing in typed consts

  Revision 1.48  1999/09/17 17:14:12  peter
    * @procvar fixes for tp mode
    * @<id>:= gives now an error

  Revision 1.47  1999/09/11 09:08:34  florian
    * fixed bug 596
    * fixed some problems with procedure variables and procedures of object,
      especially in TP mode. Procedure of object doesn't apply only to classes,
      it is also allowed for objects !!

  Revision 1.46  1999/08/13 15:43:59  peter
    * fixed proc->procvar conversion for tp_procvar mode, it now uses
      also the genload(method)call() function

  Revision 1.45  1999/08/07 14:21:04  florian
    * some small problems fixed

  Revision 1.44  1999/08/04 13:03:14  jonas
    * all tokens now start with an underscore
    * PowerPC compiles!!

  Revision 1.43  1999/08/04 00:23:36  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.42  1999/08/03 22:03:28  peter
    * moved bitmask constants to sets
    * some other type/const renamings

}
