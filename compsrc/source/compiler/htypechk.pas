{
    $Id: htypechk.pas,v 1.1.2.38 2003/03/10 10:54:39 pierre Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    This unit exports some help routines for the type checking

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
unit htypechk;
interface

    uses
      tokens,tree,symtable;

    type
      Ttok2nodeRec=record
        tok : ttoken;
        nod : ttreetyp;
        op_overloading_supported : boolean;
      end;

    const
      tok2nodes=25;
      tok2node:array[1..tok2nodes] of ttok2noderec=(
        (tok:_PLUS    ;nod:addn;op_overloading_supported:true),      { binary overloading supported }
        (tok:_MINUS   ;nod:subn;op_overloading_supported:true),      { binary and unary overloading supported }
        (tok:_STAR    ;nod:muln;op_overloading_supported:true),      { binary overloading supported }
        (tok:_SLASH   ;nod:slashn;op_overloading_supported:true),    { binary overloading supported }
        (tok:_EQUAL   ;nod:equaln;op_overloading_supported:true),    { binary overloading supported }
        (tok:_GT      ;nod:gtn;op_overloading_supported:true),       { binary overloading supported }
        (tok:_LT      ;nod:ltn;op_overloading_supported:true),       { binary overloading supported }
        (tok:_GTE     ;nod:gten;op_overloading_supported:true),      { binary overloading supported }
        (tok:_LTE     ;nod:lten;op_overloading_supported:true),      { binary overloading supported }
        (tok:_SYMDIF  ;nod:symdifn;op_overloading_supported:true),   { binary overloading supported }
        (tok:_STARSTAR;nod:starstarn;op_overloading_supported:true), { binary overloading supported }
        (tok:_OP_AS     ;nod:asn;op_overloading_supported:false),     { binary overloading NOT supported }
        (tok:_OP_IN     ;nod:inn;op_overloading_supported:false),     { binary overloading NOT supported }
        (tok:_OP_IS     ;nod:isn;op_overloading_supported:false),     { binary overloading NOT supported }
        (tok:_OP_OR     ;nod:orn;op_overloading_supported:true),     { binary overloading supported }
        (tok:_OP_AND    ;nod:andn;op_overloading_supported:true),    { binary overloading supported }
        (tok:_OP_DIV    ;nod:divn;op_overloading_supported:true),    { binary overloading supported }
        (tok:_OP_NOT    ;nod:notn;op_overloading_supported:true),    { unary overloading supported }
        (tok:_OP_MOD    ;nod:modn;op_overloading_supported:true),    { binary overloading supported }
        (tok:_OP_SHL    ;nod:shln;op_overloading_supported:true),    { binary overloading supported }
        (tok:_OP_SHR    ;nod:shrn;op_overloading_supported:true),    { binary overloading supported }
        (tok:_OP_XOR    ;nod:xorn;op_overloading_supported:true),    { binary overloading supported }
        (tok:_ASSIGNMENT;nod:assignn;op_overloading_supported:true), { unary overloading supported }
        (tok:_CARET   ;nod:caretn;op_overloading_supported:false),    { binary overloading NOT supported }
        (tok:_UNEQUAL ;nod:unequaln;op_overloading_supported:false)   { binary overloading NOT supported  overload = instead }
      );
    const
    { firstcallparan without varspez we don't count the ref }
{$ifdef extdebug}
       count_ref : boolean = true;
{$endif def extdebug}
       get_para_resulttype : boolean = false;
       allow_array_constructor : boolean = false;


    { Conversion }
    function isconvertable(def_from,def_to : pdef;
             var doconv : tconverttype;fromtreetype : ttreetyp;
             explicit,checkoperator : boolean; overload_procs : pprocdef) : byte;
    { is overloading of this operator allowed for this
      binary operator }
    function isbinaryoperatoroverloadable(ld:pdef;lt:ttreetyp;rd:pdef;rt:ttreetyp;treetyp : ttreetyp) : boolean;

    { is overloading of this operator allowed for this
      unary operator }
    function isunaryoperatoroverloadable(rd,dd : pdef;
             treetyp : ttreetyp) : boolean;

    { check operator args and result type }
    function isoperatoracceptable(pf : pprocdef; optoken : ttoken) : boolean;

    { Register Allocation }
    procedure make_not_regable(p : ptree);
    procedure left_right_max(p : ptree);
    procedure calcregisters(p : ptree;r32,fpu,mmx : word);

    { subroutine handling }
    procedure test_protected_sym(sym : psym);
    procedure test_protected(p : ptree);
    function  valid_for_formal_var(p : ptree) : boolean;
    function  valid_for_formal_const(p : ptree) : boolean;
    function  is_procsym_load(p:Ptree):boolean;
    function  is_procsym_call(p:Ptree):boolean;
    function  assignment_overloaded(from_def,to_def : pdef) : pprocdef;
    procedure test_local_to_procvar(from_def:pprocvardef;to_def:pdef);
    function  valid_for_var(p:ptree):boolean;
    function  valid_for_assignment(p:ptree):boolean;

    function internal_assignment_overloaded(from_def,to_def : pdef; var passproc: pprocdef) : pprocdef;

implementation

    uses
       globtype,systems,
       cobjects,verbose,globals,
       symconst,
       types,pass_1,cpubase,
{$ifdef newcg}
       cgbase
{$else}
       hcodegen
{$endif}
       ;

    type
      TValidAssign=(Valid_Property,Valid_Void);
      TValidAssigns=set of TValidAssign;

{****************************************************************************
                             Convert
****************************************************************************}

    { Returns:
       0 - Not convertable
       1 - Convertable
       2 - Convertable, but not first choice }
    { overload_procs : list of overloaded procedures. Should be non
      nil, if this routine is called from assignment_overloaded(),
      otherwise, the value should be nil
    }
    function isconvertable(def_from,def_to : pdef;
             var doconv : tconverttype;fromtreetype : ttreetyp;
             explicit,checkoperator : boolean; overload_procs : pprocdef) : byte;

      { Tbasetype:  uauto,uvoid,uchar,
                    u8bit,u16bit,u32bit,
                    s8bit,s16bit,s32,
                    bool8bit,bool16bit,bool32bit,
                    u64bit,s64bitint }
      type
        tbasedef=(bvoid,bchar,bint,bbool);
      const
        basedeftbl:array[tbasetype] of tbasedef =
          (bvoid,bvoid,bchar,
           bint,bint,bint,
           bint,bint,bint,
           bbool,bbool,bbool,bint,bint,bchar);

        basedefconverts : array[tbasedef,tbasedef] of tconverttype =
         ((tc_not_possible,tc_not_possible,tc_not_possible,tc_not_possible),
          (tc_not_possible,tc_equal,tc_not_possible,tc_not_possible),
          (tc_not_possible,tc_not_possible,tc_int_2_int,tc_int_2_bool),
          (tc_not_possible,tc_not_possible,tc_bool_2_int,tc_bool_2_bool));

      var
         b : byte;
         hd1,hd2 : pdef;
         hct : tconverttype;
      begin
        isconvertable := 0;
       { safety check }
         if not(assigned(def_from) and assigned(def_to)) then
          begin
            isconvertable:=0;
            exit;
          end;

       { tp7 procvar def support, in tp7 a procvar is always called, if the
         procvar is passed explicit a addrn would be there }
         if (m_tp_procvar in aktmodeswitches) and
            (def_from^.deftype=procvardef) and
            (fromtreetype=loadn) and
            { only if the procvar doesn't require any paramters }
            not assigned(pprocvardef(def_from)^.para) then
          begin
            def_from:=pprocvardef(def_from)^.rettype.def;
          end;

       { we walk the wanted (def_to) types and check then the def_from
         types if there is a conversion possible }
         b:=0;
         case def_to^.deftype of
           orddef :
             begin
               case def_from^.deftype of
                 orddef :
                   begin
                     doconv:=basedefconverts[basedeftbl[porddef(def_from)^.typ],basedeftbl[porddef(def_to)^.typ]];
                     b:=1;
                     if (doconv=tc_not_possible) or
                        ((doconv=tc_int_2_bool) and
                         (not explicit) and
                         (not is_boolean(def_from))) or
                        ((doconv=tc_bool_2_int) and
                         (not explicit) and
                         (not is_boolean(def_to))) then
                       b:=0;
                   end;
                 enumdef :
                   begin
                     { needed for char(enum) }
                     if explicit then
                      begin
                        doconv:=tc_int_2_int;
                        b:=1;
                      end;
                   end;
                 procvardef,
                 pointerdef,
                 classrefdef :
                   begin
                     if explicit then
                      begin
                        doconv:=tc_int_2_int;
                        b:=1;
                      end;
                   end;
               end;
             end;

          stringdef :
             begin
               case def_from^.deftype of
                 stringdef :
                   begin
                     doconv:=tc_string_2_string;
                     b:=1;
                   end;
                 orddef :
                   begin
                   { char to string}
                     if is_char(def_from) then
                      begin
                        doconv:=tc_char_2_string;
                        b:=1;
                      end;
                   end;
                 arraydef :
                   begin
                   { array of char to string, the length check is done by the firstpass of this node }
                     if is_chararray(def_from) then
                      begin
                        doconv:=tc_chararray_2_string;
                        if (is_shortstring(def_to) and
                            (def_from^.size <= 255)) or
                           (is_ansistring(def_to) and
                            (def_from^.size > 255)) then
                         b:=1
                        else
                         b:=2;
                      end;
                   end;
                 pointerdef :
                   begin
                   { pchar can be assigned to short/ansistrings,
                     but not in tp7 compatible mode }
                     if is_pchar(def_from) and not(m_tp7 in aktmodeswitches) then
                      begin
                        doconv:=tc_pchar_2_string;
                        { prefer ansistrings because pchars can overflow shortstrings, }
                        { but only if ansistrings are the default (JM)                 }
                        if (is_shortstring(def_to) and
                            not(cs_ansistrings in aktlocalswitches)) or
                           (is_ansistring(def_to) and
                            (cs_ansistrings in aktlocalswitches)) then
                          b:=1
                        else
                          b:=2;
                      end;
                   end;
               end;
             end;

           floatdef :
             begin
               case def_from^.deftype of
                 orddef :
                   begin { ordinal to real }
                     if is_integer(def_from) then
                       begin
                          if pfloatdef(def_to)^.typ=f32bit then
                            doconv:=tc_int_2_fix
                          else
                            doconv:=tc_int_2_real;
                          b:=1;
                       end;
                   end;
                 floatdef :
                   begin { 2 float types ? }
                     if pfloatdef(def_from)^.typ=pfloatdef(def_to)^.typ then
                       doconv:=tc_equal
                     else
                       begin
                          if pfloatdef(def_from)^.typ=f32bit then
                            doconv:=tc_fix_2_real
                          else
                            if pfloatdef(def_to)^.typ=f32bit then
                              doconv:=tc_real_2_fix
                            else
                              doconv:=tc_real_2_real;
                       end;
                     b:=1;
                   end;
               end;
             end;

           enumdef :
             begin
               if (def_from^.deftype=enumdef) then
                begin
                  if explicit then
                   begin
                     b:=1;
                     doconv:=tc_int_2_int;
                   end
                  else
                   begin
                     hd1:=def_from;
                     while assigned(penumdef(hd1)^.basedef) do
                      hd1:=penumdef(hd1)^.basedef;
                     hd2:=def_to;
                     while assigned(penumdef(hd2)^.basedef) do
                      hd2:=penumdef(hd2)^.basedef;
                     if (hd1=hd2) then
                      begin
                        b:=1;
                        { because of packenum they can have different sizes! (JM) }
                        doconv:=tc_int_2_int;
                      end;
                   end;
                end
               else
                if (def_from^.deftype=orddef) and
                   explicit then
                 begin
                   b:=1;
                   doconv:=tc_int_2_int;
                 end;
             end;

           arraydef :
             begin
             { open array is also compatible with a single element of its base type }
               if is_open_array(def_to) and
                  is_equal(parraydef(def_to)^.elementtype.def,def_from) then
                begin
                  doconv:=tc_equal;
                  b:=1;
                end
               else
                begin
                  case def_from^.deftype of
                    arraydef :
                      begin
                        { array constructor -> open array }
                        if is_open_array(def_to) and
                           is_array_constructor(def_from) then
                         begin
                           if is_void(parraydef(def_from)^.elementtype.def) or
                              is_equal(parraydef(def_to)^.elementtype.def,parraydef(def_from)^.elementtype.def) then
                            begin
                              doconv:=tc_equal;
                              b:=1;
                            end
                           else
                            if isconvertable(parraydef(def_from)^.elementtype.def,
                                             parraydef(def_to)^.elementtype.def,hct,arrayconstructn,false,true,nil)<>0 then
                             begin
                               doconv:=hct;
                               b:=2;
                             end;
                         end
                        else
                        { array of tvarrec -> array of const }
                         if is_array_of_const(def_to) and
                            is_equal(parraydef(def_to)^.elementtype.def,parraydef(def_from)^.elementtype.def) then
                          begin
                            doconv:=tc_equal;
                            b:=1;
                          end;
                      end;
                    pointerdef :
                      begin
                        if is_zero_based_array(def_to) and
                           is_equal(ppointerdef(def_from)^.pointertype.def,parraydef(def_to)^.elementtype.def) then
                         begin
                           doconv:=tc_pointer_2_array;
                           b:=1;
                         end;
                      end;
                    stringdef :
                      begin
                        { string to char array }
                        if (not is_special_array(def_to)) and
                           is_char(parraydef(def_to)^.elementtype.def) then
                         begin
                           doconv:=tc_string_2_chararray;
                           b:=1;
                         end;
                      end;
                    orddef :
                      begin
                        { char to chararray }
                        if is_chararray(def_to) and
                           is_char(def_from) then
                          begin
                            doconv:=tc_char_2_chararray;
                            b:=2;
                          end;
                      end;
                    recorddef :
                      begin
                        { tvarrec -> array of constconst }
                         if is_array_of_const(def_to) and
                            is_equal(def_from,parraydef(def_to)^.elementtype.def) then
                          begin
                            doconv:=tc_equal;
                            b:=1;
                          end;
                      end;
                  end;
                end;
             end;

           pointerdef :
             begin
               case def_from^.deftype of
                 stringdef :
                   begin
                     { string constant (which can be part of array constructor)
                       to zero terminated string constant }
                     if (fromtreetype in [arrayconstructn,stringconstn]) and
                        is_pchar(def_to) then
                      begin
                        doconv:=tc_cstring_2_pchar;
                        b:=1;
                      end;
                   end;
                 orddef :
                   begin
                     { char constant to zero terminated string constant }
                     if (fromtreetype=ordconstn) then
                      begin
                        if is_equal(def_from,cchardef) and
                           is_pchar(def_to) then
                         begin
                           doconv:=tc_cchar_2_pchar;
                           b:=1;
                         end
                        else
                         if is_integer(def_from) then
                          begin
                            doconv:=tc_cord_2_pointer;
                            b:=1;
                          end;
                      end;
                   end;
                 arraydef :
                   begin
                     { chararray to pointer }
                     if is_zero_based_array(def_from) and
                        is_equal(parraydef(def_from)^.elementtype.def,ppointerdef(def_to)^.pointertype.def) then
                      begin
                        doconv:=tc_array_2_pointer;
                        b:=1;
                      end;
                   end;
                 pointerdef :
                   begin
                     { child class pointer can be assigned to anchestor pointers }
                     if (
                         (ppointerdef(def_from)^.pointertype.def^.deftype=objectdef) and
                         (ppointerdef(def_to)^.pointertype.def^.deftype=objectdef) and
                         pobjectdef(ppointerdef(def_from)^.pointertype.def)^.is_related(
                           pobjectdef(ppointerdef(def_to)^.pointertype.def))
                        ) or
                        { all pointers can be assigned to void-pointer }
                        is_equal(ppointerdef(def_to)^.pointertype.def,voiddef) or
                        { in my opnion, is this not clean pascal }
                        { well, but it's handy to use, it isn't ? (FK) }
                        is_equal(ppointerdef(def_from)^.pointertype.def,voiddef) then
                       begin
                         doconv:=tc_equal;
                         b:=1;
                       end;
                   end;
                 procvardef :
                   begin
                     { procedure variable can be assigned to an void pointer }
                     { Not anymore. Use the @ operator now.}
                     if not(m_tp_procvar in aktmodeswitches) and
                        (ppointerdef(def_to)^.pointertype.def^.deftype=orddef) and
                        (porddef(ppointerdef(def_to)^.pointertype.def)^.typ=uvoid) then
                      begin
                        doconv:=tc_equal;
                        b:=1;
                      end;
                   end;
                 classrefdef,
                 objectdef :
                   begin
                     { class types and class reference type
                       can be assigned to void pointers      }
                     if (
                         ((def_from^.deftype=objectdef) and pobjectdef(def_from)^.is_class) or
                         (def_from^.deftype=classrefdef)
                        ) and
                        (ppointerdef(def_to)^.pointertype.def^.deftype=orddef) and
                        (porddef(ppointerdef(def_to)^.pointertype.def)^.typ=uvoid) then
                       begin
                         doconv:=tc_equal;
                         b:=1;
                       end;
                   end;
               end;
             end;

           setdef :
             begin
               { automatic arrayconstructor -> set conversion }
               if is_array_constructor(def_from) then
                begin
                  doconv:=tc_arrayconstructor_2_set;
                  b:=1;
                end;
             end;

           procvardef :
             begin
               { proc -> procvar }
               if (def_from^.deftype=procdef) and
                  (m_tp_procvar in aktmodeswitches) then
                begin
                  doconv:=tc_proc_2_procvar;
                  if proc_to_procvar_equal(pprocdef(def_from),pprocvardef(def_to),false,true) then
                   b:=1;
                end
               { procvar -> procvar }
               else
                 if (def_from^.deftype=procvardef) and
                    (proc_to_procvar_equal(pprocvardef(def_from),pprocvardef(def_to),false,true)) then
                   begin
                     doconv:=tc_equal;
                     b := 2;
                   end
               else
                { for example delphi allows the assignement from pointers }
                { to procedure variables                                  }
                if (m_pointer_2_procedure in aktmodeswitches) and
                  (def_from^.deftype=pointerdef) and
                  (ppointerdef(def_from)^.pointertype.def^.deftype=orddef) and
                  (porddef(ppointerdef(def_from)^.pointertype.def)^.typ=uvoid) then
                begin
                   doconv:=tc_equal;
                   b:=1;
                end
               else
               { nil is compatible with procvars }
                if (fromtreetype=niln) then
                 begin
                   doconv:=tc_equal;
                   b:=1;
                 end;
             end;

           objectdef :
             begin
               { object pascal objects }
               if (def_from^.deftype=objectdef) {and
                  pobjectdef(def_from)^.isclass and pobjectdef(def_to)^.isclass }then
                begin
                  doconv:=tc_equal;
                  if pobjectdef(def_from)^.is_related(pobjectdef(def_to)) then
                   b:=1;
                end
               else
               { Class specific }
                if (pobjectdef(def_to)^.is_class) then
                 begin
                   { void pointer also for delphi mode }
                   if (m_delphi in aktmodeswitches) and
                      is_voidpointer(def_from) then
                    begin
                      doconv:=tc_equal;
                      b:=1;
                    end
                   else
                   { nil is compatible with class instances }
                    if (fromtreetype=niln) and (pobjectdef(def_to)^.is_class) then
                     begin
                       doconv:=tc_equal;
                       b:=1;
                     end;
                 end;
             end;

           classrefdef :
             begin
               { class reference types }
               if (def_from^.deftype=classrefdef) then
                begin
                  doconv:=tc_equal;
                  if explicit or
                     (pobjectdef(pclassrefdef(def_from)^.pointertype.def)^.is_related(
                        pobjectdef(pclassrefdef(def_to)^.pointertype.def))) then
                   b:=1;
                end
               else
                { nil is compatible with class references }
                if (fromtreetype=niln) then
                 begin
                   doconv:=tc_equal;
                   b:=1;
                 end;
             end;

           filedef :
             begin
               { typed files are all equal to the abstract file type
               name TYPEDFILE in system.pp in is_equal in types.pas
               the problem is that it sholud be also compatible to FILE
               but this would leed to a problem for ASSIGN RESET and REWRITE
               when trying to find the good overloaded function !!
               so all file function are doubled in system.pp
               this is not very beautiful !!}
               if (def_from^.deftype=filedef) and
                  (
                   (
                    (pfiledef(def_from)^.filetyp = ft_typed) and
                    (pfiledef(def_to)^.filetyp = ft_typed) and
                    (
                     (pfiledef(def_from)^.typedfiletype.def = pdef(voiddef)) or
                     (pfiledef(def_to)^.typedfiletype.def = pdef(voiddef))
                    )
                   ) or
                   (
                    (
                     (pfiledef(def_from)^.filetyp = ft_untyped) and
                     (pfiledef(def_to)^.filetyp = ft_typed)
                    ) or
                    (
                     (pfiledef(def_from)^.filetyp = ft_typed) and
                     (pfiledef(def_to)^.filetyp = ft_untyped)
                    )
                   )
                  ) then
                 begin
                    doconv:=tc_equal;
                    b:=1;
                 end
             end;

           else
             begin
             { assignment overwritten ?? }
             { load the value directly to avoid infinite recusrsivity }
               if not assigned(overload_procs) then
                 begin
                   if assigned(overloaded_operators[_assignment]) then
                      overload_procs:=overloaded_operators[_assignment]^.definition
                   else
                    exit;
                 end;
               if checkoperator and
                  (internal_assignment_overloaded(def_from,def_to,overload_procs)<>nil) then
                b:=2;
             end;
         end;
        isconvertable:=b;
      end;

    { ld is the left type definition
      rd the right type definition
      dd the result type definition  or voiddef if unkown }
    function isbinaryoperatoroverloadable(ld:pdef;lt:ttreetyp;rd:pdef;rt:ttreetyp;treetyp : ttreetyp) : boolean;
      begin
        isbinaryoperatoroverloadable:=
           (treetyp=starstarn) or
           (ld^.deftype=recorddef) or
           (rd^.deftype=recorddef) or
           ((rd^.deftype=pointerdef) and
            not(is_pchar(rd) and
                (is_chararray(ld) or
                 is_char(ld) or
                 (ld^.deftype=stringdef) or
                 (treetyp=addn))) and
            (not(ld^.deftype in [pointerdef,objectdef,classrefdef,procvardef,arraydef]) or
             not (treetyp in [equaln,unequaln,gtn,gten,ltn,lten,subn])
            ) and
            (not is_integer(ld) or not (treetyp in [addn,subn]))
           ) or
           ((ld^.deftype=pointerdef) and
            not(is_pchar(ld) and
                (is_chararray(rd) or
                 is_char(rd) or
                 (rd^.deftype=stringdef) or
                 (treetyp=addn))) and
            (not(rd^.deftype in [stringdef,pointerdef,objectdef,classrefdef,procvardef,arraydef]) and
             ((not is_integer(rd) and (rd^.deftype<>objectdef)
               and (rd^.deftype<>classrefdef)) or
              not (treetyp in [equaln,unequaln,gtn,gten,ltn,lten,addn,subn])
             )
            )
           ) or
           { array def, but not mmx or chararray+[char,string,chararray] }
           ((ld^.deftype=arraydef) and
            not((cs_mmx in aktlocalswitches) and
                is_mmx_able_array(ld)) and
            not(is_chararray(ld) and
                (is_char(rd) or
                 is_pchar(rd) or
                 { char array + int = pchar + int, fix for web bug 1377 (JM) }
                 is_integer(rd) or
                 (rd^.deftype=stringdef) or
                 is_chararray(rd) or
                 (rt=niln)))
           ) or
           ((rd^.deftype=arraydef) and
            not((cs_mmx in aktlocalswitches) and
                is_mmx_able_array(rd)) and
            not(is_chararray(rd) and
                (is_char(ld) or
                 is_pchar(ld) or
                 (ld^.deftype=stringdef) or
                 is_chararray(ld) or
                 (lt=niln)))
           ) or
           { <> and = are defined for classes }
           ((ld^.deftype=objectdef) and
            (not(pobjectdef(ld)^.is_class) or
             not(treetyp in [equaln,unequaln])
            )
           ) or
           ((rd^.deftype=objectdef) and
            (not(pobjectdef(rd)^.is_class) or
             not(treetyp in [equaln,unequaln])
            )
             or
           { allow other operators that + on strings }
           (
            (is_char(rd) or
             is_pchar(rd) or
             (rd^.deftype=stringdef) or
             is_chararray(rd) or
             is_char(ld) or
             is_pchar(ld) or
             (ld^.deftype=stringdef) or
             is_chararray(ld)
             ) and
             not(treetyp in [addn,equaln,unequaln,gtn,gten,ltn,lten]) and
             not(is_pchar(ld) and
                 (is_integer(rd) or (rd^.deftype=pointerdef)) and
                 (treetyp=subn)
                )
            )
           );
      end;


    function isunaryoperatoroverloadable(rd,dd : pdef;
             treetyp : ttreetyp) : boolean;
      var
        conv : tconverttype;
      begin
        isunaryoperatoroverloadable:=false;
        { what assignment overloading should be allowed ?? }
        if (treetyp=assignn) then
          begin
            isunaryoperatoroverloadable:=not(is_equal(rd,dd) or
                                             (isconvertable(rd,dd,conv,nothingn,true,false,nil)<>0));
          end
        { should we force that rd and dd are equal ?? }
        else if (treetyp=subn { unaryminusn }) then
          begin
            isunaryoperatoroverloadable:=
              not is_integer(rd) and not (rd^.deftype=floatdef)
{$ifdef SUPPORT_MMX}
              and not ((cs_mmx in aktlocalswitches) and
              is_mmx_able_array(rd))
{$endif SUPPORT_MMX}
              ;
          end
        else if (treetyp=notn) then
          begin
            isunaryoperatoroverloadable:=not is_integer(rd) and not is_boolean(rd)
{$ifdef SUPPORT_MMX}
              and not ((cs_mmx in aktlocalswitches) and
              is_mmx_able_array(rd))
{$endif SUPPORT_MMX}
              ;
          end;
      end;

    function isoperatoracceptable(pf : pprocdef; optoken : ttoken) : boolean;
      var
        ld,rd,dd : pdef;
        i : longint;
      begin
        case pf^.parast^.symindex^.count of
          2 : begin
                isoperatoracceptable:=false;
                for i:=1 to tok2nodes do
                  if tok2node[i].tok=optoken then
                    begin
                      ld:=pvarsym(pf^.parast^.symindex^.first)^.vartype.def;
                      rd:=pvarsym(pf^.parast^.symindex^.first^.indexnext)^.vartype.def;
                      dd:=pf^.rettype.def;
                      isoperatoracceptable:=
                        tok2node[i].op_overloading_supported and
                        isbinaryoperatoroverloadable(ld,nothingn,rd,nothingn,tok2node[i].nod);
                      break;
                    end;
              end;
          1 : begin
                rd:=pvarsym(pf^.parast^.symindex^.first)^.vartype.def;
                dd:=pf^.rettype.def;
                for i:=1 to tok2nodes do
                  if tok2node[i].tok=optoken then
                    begin
                      isoperatoracceptable:=
                        tok2node[i].op_overloading_supported and
                        isunaryoperatoroverloadable(rd,dd,tok2node[i].nod);
                      break;
                    end;
              end;
          else
            isoperatoracceptable:=false;
          end;
      end;

{****************************************************************************
                          Register Calculation
****************************************************************************}

    { marks an lvalue as "unregable" }
    procedure make_not_regable(p : ptree);
      begin
         case p^.treetype of
            typeconvn :
              make_not_regable(p^.left);
            loadn :
              if p^.symtableentry^.typ=varsym then
                pvarsym(p^.symtableentry)^.varoptions:=pvarsym(p^.symtableentry)^.varoptions-[vo_regable,vo_fpuregable];
         end;
      end;


    procedure left_right_max(p : ptree);
      begin
        if assigned(p^.left) then
         begin
           if assigned(p^.right) then
            begin
              p^.registers32:=max(p^.left^.registers32,p^.right^.registers32);
              p^.registersfpu:=max(p^.left^.registersfpu,p^.right^.registersfpu);
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=max(p^.left^.registersmmx,p^.right^.registersmmx);
{$endif SUPPORT_MMX}
            end
           else
            begin
              p^.registers32:=p^.left^.registers32;
              p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
            end;
         end;
      end;

    { calculates the needed registers for a binary operator }
    procedure calcregisters(p : ptree;r32,fpu,mmx : word);

      begin
         left_right_max(p);

      { Only when the difference between the left and right registers < the
        wanted registers allocate the amount of registers }

        if assigned(p^.left) then
         begin
           if assigned(p^.right) then
            begin
              { the location must be already filled in because we need it to }
              { calculate the necessary number of registers (JM)             }
              if p^.location.loc = LOC_INVALID then
                internalerror(200110101);

              if (abs(p^.left^.registers32-p^.right^.registers32)<r32) or
                 ((p^.location.loc = LOC_FPU) and
                  (p^.right^.registersfpu <= p^.left^.registersfpu) and
                  ((p^.right^.registersfpu <> 0) or (p^.left^.registersfpu <> 0)) and
                  (p^.left^.registers32   < p^.right^.registers32)) then
                inc(p^.registers32,r32);
              if (abs(p^.left^.registersfpu-p^.right^.registersfpu)<fpu) then
               inc(p^.registersfpu,fpu);
              { constants in FPU generate one more need !! PM
              if (p^.left^.treetype=realconstn) and (p^.left^.location.loc=LOC_FPU) then
                if p^.registersfpu<p^.right^.registersfpu+1 then
                  p^.registersfpu:=p^.right^.registersfpu+1;
              solved by swaped if left is realconstn }

{$ifdef SUPPORT_MMX}
              if (abs(p^.left^.registersmmx-p^.right^.registersmmx)<mmx) then
               inc(p^.registersmmx,mmx);
{$endif SUPPORT_MMX}
              { the following is a little bit guessing but I think }
              { it's the only way to solve same internalerrors:    }
              { if the left and right node both uses registers     }
              { and return a mem location, but the current node    }
              { doesn't use an integer register we get probably    }
              { trouble when restoring a node                      }
              if (p^.left^.registers32=p^.right^.registers32) and
                 (p^.registers32=p^.left^.registers32) and
                 (p^.registers32>0) and
                (p^.left^.location.loc in [LOC_REFERENCE,LOC_MEM]) and
                (p^.right^.location.loc in [LOC_REFERENCE,LOC_MEM]) then
                inc(p^.registers32);
            end
           else
            begin
              if (p^.left^.registers32<r32) then
               inc(p^.registers32,r32);
              if (p^.left^.registersfpu<fpu) then
               inc(p^.registersfpu,fpu);
{$ifdef SUPPORT_MMX}
              if (p^.left^.registersmmx<mmx) then
               inc(p^.registersmmx,mmx);
{$endif SUPPORT_MMX}
            end;
         end;

         { error CGMessage, if more than 8 floating point }
         { registers are needed                         }
         {if p^.registersfpu>maxfpuregs then
          CGMessage(cg_e_too_complex_expr); now pushed if needed PM }
      end;

{****************************************************************************
                          Subroutine Handling
****************************************************************************}

{ protected field handling
  protected field can not appear in
  var parameters of function !!
  this can only be done after we have determined the
  overloaded function
  this is the reason why it is not in the parser, PM }

    procedure test_protected_sym(sym : psym);
      begin
         if (sp_protected in sym^.symoptions) and
            ((sym^.owner^.symtabletype=unitsymtable) or
             ((sym^.owner^.symtabletype=objectsymtable) and
             (pobjectdef(sym^.owner^.defowner)^.owner^.symtabletype=unitsymtable))
            ) then
          CGMessage(parser_e_cant_access_protected_member);
      end;


    procedure test_protected(p : ptree);
      begin
        case p^.treetype of
         loadn : test_protected_sym(p^.symtableentry);
     typeconvn : test_protected(p^.left);
        derefn : test_protected(p^.left);
    subscriptn : begin
                 { test_protected(p^.left);
                   Is a field of a protected var
                   also protected ???  PM }
                   test_protected_sym(p^.vs);
                 end;
        end;
      end;

   function  valid_for_formal_const(p : ptree) : boolean;
     var
        v : boolean;
     begin
        { p must have been firstpass'd before }
        { accept about anything but not a statement ! }
        case p^.treetype of
          calln,
          statementn,
          addrn :
           begin
             { addrn is not allowed as this generate a constant value,
               but a tp procvar are allowed (PFV) }
             if p^.procvarload then
              v:=true
             else
              v:=false;
           end;
          else
            v:=true;
        end;
        valid_for_formal_const:=v;
     end;

    function is_procsym_load(p:Ptree):boolean;
      begin
         is_procsym_load:=((p^.treetype=loadn) and (p^.symtableentry^.typ=procsym)) or
                          ((p^.treetype=addrn) and (p^.left^.treetype=loadn)
                          and (p^.left^.symtableentry^.typ=procsym)) ;
      end;

   { change a proc call to a procload for assignment to a procvar }
   { this can only happen for proc/function without arguments }
    function is_procsym_call(p:Ptree):boolean;
      begin
        is_procsym_call:=
             (p^.treetype=calln) and
             (p^.left=nil) and
             (
              ((p^.right=nil) and (p^.symtableprocentry^.typ=procsym)) or
              ((p^.right<>nil) and (p^.right^.symtableprocentry^.typ=varsym))
             );
      end;


    function internal_assignment_overloaded(from_def,to_def : pdef; var passproc: pprocdef) : pprocdef;
    var
          convtyp : tconverttype;
     begin
       internal_assignment_overloaded := nil;
       while passproc<>nil do
         begin
           if is_equal(passproc^.rettype.def,to_def) and
              (is_equal(pparaitem(passproc^.para^.first)^.paratype.def,from_def) or
              (isconvertable(from_def,pparaitem(passproc^.para^.first)^.paratype.def,
                convtyp,ordconstn,false,true,passproc)=1)) then
             begin
               internal_assignment_overloaded:=passproc;
               break;
             end;
            passproc:=passproc^.nextoverloaded;
        end;
     end;

    function assignment_overloaded(from_def,to_def : pdef) : pprocdef;
       var
          passproc : pprocdef;
       begin
          assignment_overloaded:=nil;
          if assigned(overloaded_operators[_assignment]) then
            passproc:=overloaded_operators[_assignment]^.definition
          else
            exit;
          assignment_overloaded:=internal_assignment_overloaded(from_def, to_def, passproc);
       end;


    { local routines can't be assigned to procvars }
    procedure test_local_to_procvar(from_def:pprocvardef;to_def:pdef);
      begin
         if (from_def^.symtablelevel>1) and (to_def^.deftype=procvardef) then
           CGMessage(type_e_cannot_local_proc_to_procvar);
      end;


    function  valid_for_assign(p:ptree;opts:TValidAssigns):boolean;
      var
        hp : ptree;
        gotwith,
        gotsubscript,
        gotpointer,
        gotclass,
        gotderef : boolean;
        fromdef,
        todef    : pdef;
      begin
        valid_for_assign:=false;
        gotsubscript:=false;
        gotderef:=false;
        gotclass:=false;
        gotpointer:=false;
        gotwith:=false;
        hp:=p;
        if not(valid_void in opts) and
           is_void(hp^.resulttype) then
         begin
           CGMessagePos(hp^.fileinfo,type_e_argument_cant_be_assigned);
           exit;
         end;
        while assigned(hp) do
         begin
           { property allowed? calln has a property check itself }
           if hp^.isproperty then
            begin
              if (valid_property in opts) then
               valid_for_assign:=true
              else
               begin
                 { check return type }
                 case hp^.resulttype^.deftype of
                   pointerdef :
                     gotpointer:=true;
                   objectdef :
                     gotclass:=pobjectdef(hp^.resulttype)^.is_class;
                   recorddef, { handle record like class it needs a subscription }
                   classrefdef :
                     gotclass:=true;
                 end;
                 { 1. if it returns a pointer and we've found a deref,
                   2. if it returns a class or record and a subscription or with is found }
                 if (gotpointer and gotderef) or
                    (gotclass and (gotsubscript or gotwith)) then
                   valid_for_assign:=true
                 else
                   CGMessagePos(hp^.fileinfo,type_e_argument_cant_be_assigned);
               end;
              exit;
            end;
           case hp^.treetype of
             derefn :
               begin
                 gotderef:=true;
                 hp:=hp^.left;
               end;
             typeconvn :
               begin
                 { typecast sizes must match, exceptions:
                   - from formaldef
                   - from void
                   - typecast from pointer to array
                   - BP style objects }
                 fromdef:=hp^.left^.resulttype;
                 todef:=hp^.resulttype;
                 if not((fromdef^.deftype=formaldef) or
                        is_void(fromdef) or
                        ((fromdef^.deftype=objectdef) and (todef^.deftype=objectdef) and
                         pobjectdef(fromdef)^.is_related(pobjectdef(todef))) or
                        ((fromdef^.deftype=pointerdef) and (todef^.deftype=arraydef))) and
                    (fromdef^.size<>todef^.size) then
                  begin
                    { in TP it is allowed to typecast to smaller types }
                    if not(m_tp7 in aktmodeswitches) or
                       (todef^.size>fromdef^.size) or
                       (target_os.endian=endian_big) then
                     CGMessagePos2(hp^.fileinfo,type_e_typecast_wrong_size_for_assignment,tostr(fromdef^.size),
                      tostr(todef^.size));
                  end;
                 case hp^.resulttype^.deftype of
                   pointerdef :
                     gotpointer:=true;
                   objectdef :
                     gotclass:=pobjectdef(hp^.resulttype)^.is_class;
                   classrefdef :
                     gotclass:=true;
                   arraydef :
                     begin
                       { pointer -> array conversion is done then we need to see it
                         as a deref, because a ^ is then not required anymore }
                       if (hp^.left^.resulttype^.deftype=pointerdef) then
                        gotderef:=true;
                     end;
                 end;
                 hp:=hp^.left;
               end;
             vecn,
             asn :
               hp:=hp^.left;
             subscriptn :
               begin
                 gotsubscript:=true;
                 hp:=hp^.left;
                 { a class/interface access is an implicit }
                 { dereferencing                           }
                 if (hp^.resulttype^.deftype=objectdef) and
                   pobjectdef(hp^.resulttype)^.is_class then
                   gotderef:=true;
               end;
             subn,
             addn :
               begin
                 { Allow add/sub operators on a pointer, or an integer
                   and a pointer typecast and deref has been found }
                 if ((hp^.resulttype^.deftype=pointerdef) or
                     (is_integer(hp^.resulttype) and gotpointer)) and
                    gotderef then
                  valid_for_assign:=true
                 else
                  CGMessagePos(hp^.fileinfo,type_e_variable_id_expected);
                 exit;
               end;
             addrn :
               begin
                 if gotderef or
                    hp^.procvarload then
                  valid_for_assign:=true
                 else
                  CGMessagePos(hp^.fileinfo,type_e_no_assign_to_addr);
                 exit;
               end;
             selfn,
             funcretn :
               begin
                 valid_for_assign:=true;
                 exit;
               end;
             calln :
               begin
                 { check return type }
                 case hp^.resulttype^.deftype of
                   pointerdef :
                     gotpointer:=true;
                   objectdef :
                     gotclass:=pobjectdef(hp^.resulttype)^.is_class;
                   recorddef, { handle record like class it needs a subscription }
                   classrefdef :
                     gotclass:=true;
                 end;
                 { 1. if it returns a pointer and we've found a deref,
                   2. if it returns a class or record and a subscription or with is found }
                 if (gotpointer and gotderef) or
                    (gotclass and (gotsubscript or gotwith)) then
                  valid_for_assign:=true
                 else
                  CGMessagePos(hp^.fileinfo,type_e_argument_cant_be_assigned);
                 exit;
               end;
             loadn :
               begin
                 case hp^.symtableentry^.typ of
                   absolutesym,
                   varsym :
                     begin
                       if (pvarsym(hp^.symtableentry)^.varspez=vs_const) then
                        begin
                          { allow p^:= constructions with p is const parameter }
                          if gotderef then
                           valid_for_assign:=true
                          else
                           CGMessagePos(hp^.fileinfo,type_e_no_assign_to_const);
                          exit;
                        end;
                       { Are we at a with symtable, then we need to process the
                         withrefnode also to check for maybe a const load }
                       if (hp^.symtable^.symtabletype=withsymtable) then
                        begin
                          { continue with processing the withref node }
                          hp:=ptree(pwithsymtable(hp^.symtable)^.withrefnode);
                          gotwith:=true;
                        end
                       else
                        begin
                          { set the assigned flag for varsyms }
                          if (pvarsym(hp^.symtableentry)^.varstate=vs_declared) then
                           pvarsym(hp^.symtableentry)^.varstate:=vs_assigned;
                          valid_for_assign:=true;
                          exit;
                        end;
                     end;
                   funcretsym,
                   procsym, { fix bug 2374 PM }
                   typedconstsym :
                     begin
                       valid_for_assign:=true;
                       exit;
                     end;
                   else
                     begin
                       valid_for_assign:=false;
                       exit;
                     end;
                 end;
               end;
             else
               begin
                 CGMessagePos(hp^.fileinfo,type_e_variable_id_expected);
                 exit;
               end;
            end;
         end;
      end;


    function  valid_for_var(p:ptree):boolean;
      begin
        valid_for_var:=valid_for_assign(p,[]);
      end;


   function  valid_for_formal_var(p : ptree) : boolean;
     begin
        valid_for_formal_var:=valid_for_assign(p,[valid_void]);
     end;


    function  valid_for_assignment(p:ptree):boolean;
      begin
        valid_for_assignment:=valid_for_assign(p,[valid_property]);
      end;

end.
{
  $Log: htypechk.pas,v $
  Revision 1.1.2.38  2003/03/10 10:54:39  pierre
   * better error message for tbs/tb0377 on m68k

  Revision 1.1.2.37  2003/02/12 00:25:01  pierre
   * fix for bug 2374

  Revision 1.1.2.36  2003/01/11 18:13:55  carl
    * compilation bugfixes under BP

  Revision 1.1.2.35  2003/01/07 19:19:43  peter
    * allow pchar=constantchar
    * support chararray=nil

  Revision 1.1.2.34  2003/01/07 16:18:28  peter
    * allow explicit conversion from orddef->enumdef

  Revision 1.1.2.33  2003/01/06 21:20:31  peter
    * proc_to_procvar_equal gets parameter if an error should be
      written for method-procvar mismatch

  Revision 1.1.2.32  2003/01/05 23:05:25  peter
    * add explicit conversion from pointer/procvar/classref to int

  Revision 1.1.2.31  2003/01/05 18:49:48  peter
    * isconvertable has extra para to check for operator or not

  Revision 1.1.2.30  2003/01/05 18:03:02  peter
    * use isequal and isconvertable to check if assignment overload
      is allowed

  Revision 1.1.2.29  2002/12/22 16:31:14  peter
    * fix crash with exit(procvar)

  Revision 1.1.2.28  2002/09/08 13:59:26  carl
    * forgot to initialize return value

  Revision 1.1.2.27  2002/09/08 08:51:13  carl
    * bugfix for report 2109

  Revision 1.1.2.26  2002/04/01 20:21:13  jonas
    * fixed web bug 1907
    * fixed some other procvar related bugs (all related to accepting procvar
      constructs with either too many or too little parameters)

  Revision 1.1.2.25  2002/01/16 09:33:46  jonas
    * no longer allow assignments to pointer expressions (unless there's a
      deref), reported by John Lee

  Revision 1.1.2.24  2001/12/17 12:42:08  jonas
    * added type conversion from procvar to procvar (if their arguments are
      convertable, two procvars are convertable too)

  Revision 1.1.2.23  2001/11/02 23:19:09  jonas
    * fixed web bug 1665 (allow char to chararray type conversion)

  Revision 1.1.2.22  2001/10/28 17:18:11  peter
    * allow assignment of overloaded procedures to procvars when we know
      which procedure to take

  Revision 1.1.2.20  2001/10/11 14:36:29  jonas
    * fixed internalerror(10) due to previous fpu overflow fixes
    * NOTE: from now on, the location of a binary node must now always be set
       before you call calcregisters() for it

  Revision 1.1.2.19  2001/09/10 22:48:29  pierre
   * adapt to fpu pushing if fpu overflow

  Revision 1.1.2.18  2001/09/07 07:49:40  jonas
    * only allow typecasting for assignment from oneobject type to another
      with a different size if the two object types are related

  Revision 1.1.2.17  2001/09/03 19:15:27  carl
  * make it compile under BP

  Revision 1.1.2.16  2001/09/03 15:21:01  pierre
   * fix problem with objects as var parametrers

  Revision 1.1.2.15  2001/09/02 21:43:02  peter
    * check typecast sizes for values in assignments

  Revision 1.1.2.14  2001/06/06 17:23:36  jonas
    * fixed inconsistent handling of procvars in FPC mode (sometimes @ was
      required to assign the address of a procedure to a procvar,, sometimes
      not. Now it is always required)

  Revision 1.1.2.13  2001/06/04 18:03:57  peter
    * fixes to valid_for_assign for properties

  Revision 1.1.2.12  2001/06/04 11:47:10  peter
    * better const to var checking

  Revision 1.1.2.11  2001/05/18 18:08:48  carl
  * replace by portable constant a numeric constant value

  Revision 1.1.2.10  2001/02/20 21:26:41  peter
    * tvarrec -> array of const

  Revision 1.1.2.9  2001/02/12 21:58:14  pierre
   * fix for bug 1395

  Revision 1.1.2.8  2001/02/04 11:22:42  jonas
    * fixed web bug 1377

  Revision 1.1.2.7  2001/01/08 21:42:57  peter
    * string isn't compatible with array of char

  Revision 1.1.2.6  2000/12/22 22:43:15  peter
    * fixed bug 1286

  Revision 1.1.2.5  2000/12/09 13:03:47  florian
    * web bug 1207 fixed: field and properties of const classes can be
      changed

  Revision 1.1.2.4  2000/12/08 14:07:13  jonas
    * removed curly braces from previous log comment

  Revision 1.1.2.3  2000/12/08 10:41:06  jonas
    * fix for web bug 1245: arrays of char with size >255 are now passed to
      overloaded procedures which expect ansistrings instead of shortstrings
      if possible
    * pointer to array of chars (when using $t+) are now also considered
      pchars

  Revision 1.1.2.2  2000/08/16 18:26:00  peter
    * splitted namedobjectitem.next into indexnext and listnext so it
      can be used in both lists
    * don't allow "word = word" type definitions

  Revision 1.1.2.1  2000/08/07 09:19:33  jonas
    * fixed bug in type conversions between enum subranges (it didn't take
      the packenum directive into account)
    + define PACKENUMFIXED symbol in options.pas

  Revision 1.1  2000/07/13 06:29:51  michael
  + Initial import

  Revision 1.71  2000/07/06 18:56:58  peter
    * fixed function returning record type and assigning to the result

  Revision 1.70  2000/06/18 19:41:19  peter
    * fixed pchar<->[string,chararray] operations

  Revision 1.69  2000/06/11 07:00:21  peter
    * fixed pchar->string conversion for delphi mode

  Revision 1.68  2000/06/06 20:25:43  pierre
    * unary minus operator overloading was broken
    + accept pointer args in binary operator

  Revision 1.67  2000/06/05 20:41:17  pierre
    + support for NOT overloading
    + unsupported overloaded operators generate errors

  Revision 1.66  2000/06/04 09:04:30  peter
    * check for procvar in valid_for_formal

  Revision 1.65  2000/06/02 21:22:04  pierre
    + isbinaryoperatoracceptable and isunaryoperatoracceptable
      for a more coherent operator overloading implementation
      tok2node moved from pexpr unit to htypechk

  Revision 1.64  2000/06/01 19:13:02  peter
    * fixed long line for tp7

  Revision 1.63  2000/06/01 11:00:52  peter
    * fixed string->pchar conversion for array constructors

  Revision 1.62  2000/05/30 18:38:45  florian
    * fixed assignments of subrange enumeration types

  Revision 1.61  2000/05/26 18:21:41  peter
    * give error for @ with formal const,var parameter. Because @ generates
      a constant value and not a reference

  Revision 1.60  2000/05/16 16:01:03  florian
    * fixed type conversion test for open arrays: the to and from fields where
      exchanged which leads under certain circumstances to problems when
      passing arrays of classes/class references as open array parameters

  Revision 1.59  2000/02/18 16:13:29  florian
    * optimized ansistring compare with ''
    * fixed 852

  Revision 1.58  2000/02/09 13:22:53  peter
    * log truncated

  Revision 1.57  2000/02/05 12:11:50  peter
    * property check for assigning fixed for calln

  Revision 1.56  2000/02/01 09:41:27  peter
    * allow class -> voidpointer for delphi mode

  Revision 1.55  2000/01/07 01:14:27  peter
    * updated copyright to 2000

  Revision 1.54  1999/12/31 14:26:27  peter
    * fixed crash with empty array constructors

  Revision 1.53  1999/12/18 14:55:21  florian
    * very basic widestring support

  Revision 1.52  1999/12/16 19:12:04  peter
    * allow constant pointer^ also for assignment

  Revision 1.51  1999/12/09 09:35:54  peter
    * allow assigning to self

  Revision 1.50  1999/11/30 10:40:43  peter
    + ttype, tsymlist

  Revision 1.49  1999/11/18 15:34:45  pierre
    * Notes/Hints for local syms changed to
      Set_varstate function

  Revision 1.48  1999/11/09 14:47:03  peter
    * pointer->array is allowed for all pointer types in FPC, fixed assign
      check for it.

  Revision 1.47  1999/11/09 13:29:33  peter
    * valid_for_assign allow properties with calln

  Revision 1.46  1999/11/08 22:45:33  peter
    * allow typecasting to integer within pointer typecast+deref

  Revision 1.45  1999/11/06 14:34:21  peter
    * truncated log to 20 revs

  Revision 1.44  1999/11/04 23:11:21  peter
    * fixed pchar and deref detection for assigning

  Revision 1.43  1999/10/27 16:04:45  peter
    * valid_for_assign support for calln,asn

  Revision 1.42  1999/10/26 12:30:41  peter
    * const parameter is now checked
    * better and generic check if a node can be used for assigning
    * export fixes
    * procvar equal works now (it never had worked at least from 0.99.8)
    * defcoll changed to linkedlist with pparaitem so it can easily be
      walked both directions

  Revision 1.41  1999/10/14 14:57:52  florian
    - removed the hcodegen use in the new cg, use cgbase instead

  Revision 1.40  1999/09/26 21:30:15  peter
    + constant pointer support which can happend with typecasting like
      const p=pointer(1)
    * better procvar parsing in typed consts

  Revision 1.39  1999/09/17 17:14:04  peter
    * @procvar fixes for tp mode
    * @<id>:= gives now an error

  Revision 1.38  1999/08/17 13:26:07  peter
    * arrayconstructor -> arrayofconst fixed when arraycosntructor was not
      variant.

}
