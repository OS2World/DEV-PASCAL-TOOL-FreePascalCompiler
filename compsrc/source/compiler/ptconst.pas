{
    $Id: ptconst.pas,v 1.1.2.25 2003/02/02 13:29:24 peter Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    Reads typed constants

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
unit ptconst;

  interface

   uses symtable;

    { this procedure reads typed constants }
    { sym is only needed for ansi strings  }
    { the assembler label is in the middle (PM) }
    procedure readtypedconst(def : pdef;sym : ptypedconstsym;no_change_allowed : boolean);

  implementation

    uses
{$ifdef Delphi}
       sysutils,
{$else}
       strings,
{$endif Delphi}
       globtype,systems,tokens,
       cobjects,globals,scanner,
       symconst,aasm,types,verbose,
       tree,pass_1,
       { parser specific stuff }
       pbase,pexpr,
       { processor specific stuff }
       cpubase,
       { codegen }
{$ifdef newcg}
       cgbase,
{$else}
       hcodegen,
{$endif}
       hcgdata;


{$ifdef fpc}
{$maxfpuregisters 0}
{$endif fpc}
    { this procedure reads typed constants }
    procedure readtypedconst(def : pdef;sym : ptypedconstsym;no_change_allowed : boolean);

      var
         len,base  : longint;
         p,hp,hpstart : ptree;
         i,l,offset,
         strlength : longint;
         curconstsegment : paasmoutput;
         ll        : pasmlabel;
         s         : string;
         c         : char;
         ca        : pchar;
     ali       : longint;
         aktpos    : longint;
         obj       : pobjectdef;
         pointertypedef : pdef;
         symt      : psymtable;
         rvalue     : bestreal;
         strval    : pchar;
         error     : boolean;
         recsym1,
         recsym2  : psym;
         j :        integer;

      procedure check_range;
        begin
           if ((get_int64_ordinal_value(p)>porddef(def)^.get_high) or
               (get_int64_ordinal_value(p)<porddef(def)^.get_low)) then
             begin
                if (cs_check_range in aktlocalswitches) then
                  Message(parser_e_range_check_error)
                else
                  Message(parser_w_range_check_error);
             end;
        end;

(*      function is_po_equal(o1,o2:longint):boolean;
        begin
        { assembler does not affect }
          is_po_equal:=(o1 and not(poassembler))=
                       (o2 and not(poassembler));
        end; *)

{$R-}  {Range check creates problem with init_8bit(-1) !!}
      begin
         if no_change_allowed then
           curconstsegment:=consts
         else
           curconstsegment:=datasegment;
         case def^.deftype of
            orddef:
              begin
                 p:=comp_expr(true);
                 do_firstpass(p);
                 case porddef(def)^.typ of
                    s8bit,
                    u8bit : begin
                               if not is_constintnode(p) then
                               { is't an int expected }
                                 Message(cg_e_illegal_expression)
                               else
                                 begin
                                    curconstsegment^.concat(new(pai_const,init_8bit(p^.value)));
                                    check_range;
                                 end;
                            end;
                    s32bit : begin
                                if not is_constintnode(p) then
                                  Message(cg_e_illegal_expression)
                                else
                                  begin
                                     curconstsegment^.concat(new(pai_const,init_32bit(p^.value)));
                                     check_range;
                                  end;
                            end;
                    u32bit : begin
                                if not is_constintnode(p) then
                                  Message(cg_e_illegal_expression)
                                else
                                   curconstsegment^.concat(new(pai_const,init_32bit(p^.value)));
                             end;
                    bool8bit : begin
                                  if not is_constboolnode(p) then
                                    Message(cg_e_illegal_expression);
                                  curconstsegment^.concat(new(pai_const,init_8bit(p^.value)));
                               end;
                    bool16bit : begin
                                  if not is_constboolnode(p) then
                                    Message(cg_e_illegal_expression);
                                  curconstsegment^.concat(new(pai_const,init_16bit(p^.value)));
                               end;
                    bool32bit : begin
                                  if not is_constboolnode(p) then
                                    Message(cg_e_illegal_expression);
                                  curconstsegment^.concat(new(pai_const,init_32bit(p^.value)));
                               end;
                    uchar : begin
                                if not is_constcharnode(p) then
                                  Message(cg_e_illegal_expression);
                                curconstsegment^.concat(new(pai_const,init_8bit(p^.value)));
                            end;
                    uwidechar : begin
                                if not is_constcharnode(p) then
                                  Message(cg_e_illegal_expression);
                                curconstsegment^.concat(new(pai_const,init_16bit(p^.value)));
                            end;
                    u16bit,
                    s16bit : begin
                                if not is_constintnode(p) then
                                  Message(cg_e_illegal_expression);
                                curconstsegment^.concat(new(pai_const,init_16bit(p^.value)));
                                check_range;
                            end;
                    s64bit,
                    u64bit:
                      begin
                         if not is_constintnode(p) then
                           Message(cg_e_illegal_expression)
                         else
                           begin
                              {!!!!! hmmm, we can write yet only consts til 2^32-1 :( (FK) }
                              if target_os.endian = endian_little then
                                begin
                                  curconstsegment^.concat(new(pai_const,init_32bit(p^.value)));
                                  if (p^.value<0) and (porddef(def)^.typ = s64bit) then
                                    curconstsegment^.concat(new(pai_const,init_32bit(-1)))
                                  else
                                    curconstsegment^.concat(new(pai_const,init_32bit(0)));
                                end
                              else
                                begin
                                  if (p^.value<0) and (porddef(def)^.typ = s64bit) then
                                    curconstsegment^.concat(new(pai_const,init_32bit(-1)))
                                  else
                                    curconstsegment^.concat(new(pai_const,init_32bit(0)));
                                  curconstsegment^.concat(new(pai_const,init_32bit(p^.value)));
                                end;
                           end;
                      end;
                    else
                      internalerror(3799);
                 end;
                 disposetree(p);
              end;
         floatdef:
           begin
              p:=comp_expr(true);
              do_firstpass(p);
              if is_constrealnode(p) then
                rvalue:=p^.value_real
              else if is_constintnode(p) then
                rvalue:=get_ordinal_value(p)
              else
                Message(cg_e_illegal_expression);

              case pfloatdef(def)^.typ of
                 s32real : curconstsegment^.concat(new(pai_real_32bit,init(rvalue)));
                 s64real : curconstsegment^.concat(new(pai_real_64bit,init(rvalue)));
                 s80real : curconstsegment^.concat(new(pai_real_80bit,init(rvalue)));
                 s64comp  : curconstsegment^.concat(new(pai_comp_64bit,init(rvalue)));
                 f32bit : curconstsegment^.concat(new(pai_const,init_32bit(trunc(rvalue*65536))));
              else internalerror(18);
              end;
              disposetree(p);
           end;
         classrefdef:
           begin
              p:=comp_expr(true);
              do_firstpass(p);
              case p^.treetype of
                 loadvmtn:
                   begin
                      if not(pobjectdef(pclassrefdef(p^.resulttype)^.pointertype.def)^.is_related(
                        pobjectdef(pclassrefdef(def)^.pointertype.def))) then
                        Message(cg_e_illegal_expression);
                      curconstsegment^.concat(new(pai_const_symbol,init(newasmsymbol(pobjectdef(
                        pclassrefdef(p^.resulttype)^.pointertype.def)^.vmt_mangledname))));
                   end;
                 niln:
                   curconstsegment^.concat(new(pai_const,init_32bit(0)));
                 else Message(cg_e_illegal_expression);
              end;
              disposetree(p);
           end;
         pointerdef:
           begin
              p:=comp_expr(true);
              do_firstpass(p);
              if (p^.treetype=typeconvn) and
                 ((p^.left^.treetype=addrn) or (p^.left^.treetype=niln)) and
                 is_equal(def,p^.resulttype) then
                begin
                   hp:=p^.left;
                   putnode(p);
                   p:=hp;
                end;
              { allows horrible ofs(typeof(TButton)^) code !! }
              if (p^.treetype=addrn) and (p^.left^.treetype=derefn) then
                begin
                   hp:=p^.left^.left;
                   p^.left^.left:=nil;
                   disposetree(p);
                   p:=hp;
                end;
              { const pointer ? }
              if (p^.treetype = pointerconstn) then
                curconstsegment^.concat(new(pai_const,init_32bit(p^.value)))
              { nil pointer ? }
              else if p^.treetype=niln then
                curconstsegment^.concat(new(pai_const,init_32bit(0)))
              { maybe pchar ? }
              else
                if is_char(ppointerdef(def)^.pointertype.def) and
                   (p^.treetype<>addrn) then
                  begin
                    getdatalabel(ll);
                    curconstsegment^.concat(new(pai_const_symbol,init(ll)));
{$ifndef i386}
                  { personally, normally i would prefer that constants }
                  { be handled specifically in the symtable instead of }
                  { directly writing it to the constant segment here   }
                  if (p^.treetype = stringconstn) then
                    begin
                       ali:=data_align(p^.length+1);
                    end
                  else
                     ali := 0;
                  if ali>1 then
                   begin
                    consts^.concat(new(pai_align,init(ali)));
                   end;
{$endif}
                    consts^.concat(new(pai_label,init(ll)));
                    if p^.treetype=stringconstn then
                      begin
                        len:=p^.length;
                        { For tp7 the maximum lentgh can be 255 }
                        if (m_tp in aktmodeswitches) and
                           (len>255) then
                         len:=255;
                        getmem(ca,len+2);
                        move(p^.value_str^,ca^,len+1);
                        consts^.concat(new(pai_string,init_length_pchar(ca,len+1)));
                      end
                    else
                      if is_constcharnode(p) then
                        consts^.concat(new(pai_string,init(char(byte(p^.value))+#0)))
                    else
                      Message(cg_e_illegal_expression);
                end
              else
                if p^.treetype=addrn then
                  begin
                    p:=gentypeconvnode(p,def);
                    do_firstpass(p);
                    { if a typeconv node was inserted then check if it was an tc_equal. If
                      true then we remove the node. If not tc_equal then we leave the typeconvn
                      and the nodetype=loadn will always be false and generate the error (PFV) }
                    if (p^.treetype=typeconvn) then
                     begin
                       if (p^.convtyp=tc_equal) then
                        hpstart:=p^.left^.left
                       else
                        hpstart:=p;
                     end
                    else
                     hpstart:=p^.left;
                    hp:=hpstart;
                    while assigned(hp) and (hp^.treetype in [subscriptn,vecn]) do
                      hp:=hp^.left;
                    if (hp^.treetype=loadn) then
                      begin
                        do_firstpass(hpstart);
                        hp:=hpstart;
                        offset:=0;
                        while assigned(hp) and (hp^.treetype<>loadn) do
                          begin
                             case hp^.treetype of
                               vecn       :
                                 begin
                                    if (hp^.left^.resulttype^.deftype=stringdef) then
                                      begin
                                         { this seems OK for shortstring and ansistrings PM }
                                         { it is wrong for widestrings !! }
                                         len:=1;
                                         base:=0;
                                      end
                                    else if (hp^.left^.resulttype^.deftype=arraydef) then
                                      begin
                                         len:=parraydef(hp^.left^.resulttype)^.elesize;
                                         base:=parraydef(hp^.left^.resulttype)^.lowrange;
                                      end
                                    else
                                      Message(cg_e_illegal_expression);
                                    if is_constintnode(hp^.right) then
                                      inc(offset,len*(get_ordinal_value(hp^.right)-base))
                                    else
                                      Message(cg_e_illegal_expression);
                                      {internalerror(9779);}
                                 end;

                               subscriptn : inc(offset,hp^.vs^.address)
                             else
                               Message(cg_e_illegal_expression);
                             end;
                             hp:=hp^.left;
                          end;
                        if hp^.symtableentry^.typ=constsym then
                          Message(type_e_variable_id_expected);
                        curconstsegment^.concat(new(pai_const_symbol,initname_offset(hp^.symtableentry^.mangledname,offset)));
                        (*if token=POINT then
                          begin
                             offset:=0;
                             while token=_POINT do
                               begin
                                  consume(_POINT);
                                  lsym:=pvarsym(precdef(
                                        ppointerdef(p^.resulttype)^.pointertype.def)^.symtable^.search(pattern));
                                  if assigned(sym) then
                                    offset:=offset+lsym^.address
                                  else
                                    begin
                                       Message1(sym_e_illegal_field,pattern);
                                    end;
                                  consume(_ID);
                               end;
                             curconstsegment^.concat(new(pai_const_symbol_offset,init(
                               strpnew(p^.left^.symtableentry^.mangledname),offset)));
                          end
                        else
                          begin
                             curconstsegment^.concat(new(pai_const,init_symbol(
                               strpnew(p^.left^.symtableentry^.mangledname))));
                          end;   *)
                      end
                    else
                      Message(cg_e_illegal_expression);
                  end
              else
              { allow typeof(Object type)}
                if (p^.treetype=inlinen) and
                   (p^.inlinenumber=in_typeof_x) then
                  begin
                    if (p^.left^.treetype=typen) then
                      begin
                        curconstsegment^.concat(new(pai_const_symbol,
                          initname(pobjectdef(p^.left^.resulttype)^.vmt_mangledname)));
                      end
                    else
                      Message(cg_e_illegal_expression);
                  end
              else
                Message(cg_e_illegal_expression);
              disposetree(p);
           end;
         setdef:
           begin
              p:=comp_expr(true);
              do_firstpass(p);
              if p^.treetype=setconstn then
                begin
                   { we only allow const sets }
                   if assigned(p^.left) then
                     Message(cg_e_illegal_expression)
                   else
                     begin
                        { this writing is endian independant   }
                        { untrue - because they are considered }
                        { arrays of 32-bit values CEC          }

                        if source_os.endian = target_os.endian then
                          begin
                            for l:= 0 to def^.size-1 do
                               curconstsegment^.concat(new(pai_const,init_8bit(p^.value_set^[l])));
                          end
                        else
                          begin
                            { store as longint values in swaped format }
                            j:=0;
                            for l:=0 to ((def^.size-1) div 4) do
                              Begin
                               curconstsegment^.concat(new(pai_const,init_8bit(p^.value_set^[j+3])));
                               curconstsegment^.concat(new(pai_const,init_8bit(p^.value_set^[j+2])));
                               curconstsegment^.concat(new(pai_const,init_8bit(p^.value_set^[j+1])));
                               curconstsegment^.concat(new(pai_const,init_8bit(p^.value_set^[j])));
                               Inc(j,4);
                             end;
                          end;
                     end;
                end
              else
                Message(cg_e_illegal_expression);
              disposetree(p);
           end;
         enumdef:
           begin
              p:=comp_expr(true);
              do_firstpass(p);
              if p^.treetype=ordconstn then
                begin
                  if is_equal(p^.resulttype,def) or
                     is_subequal(p^.resulttype,def) then
                   begin
                     case p^.resulttype^.size of
                       1 : curconstsegment^.concat(new(pai_const,init_8bit(p^.value)));
                       2 : curconstsegment^.concat(new(pai_const,init_16bit(p^.value)));
                       4 : curconstsegment^.concat(new(pai_const,init_32bit(p^.value)));
                     end;
                   end
                  else
                   Message2(type_e_incompatible_types,def^.typename,p^.resulttype^.typename);
                end
              else
                Message(cg_e_illegal_expression);
              disposetree(p);
           end;
         stringdef:
           begin
              p:=comp_expr(true);
              do_firstpass(p);
              { load strval and strlength of the constant tree }
              if p^.treetype=stringconstn then
                begin
                  strlength:=p^.length;
                  strval:=p^.value_str;
                end
              else if is_constcharnode(p) then
                begin
                  { strval:=pchar(@p^.value);
                    THIS FAIL on BIG_ENDIAN MACHINES PM }
                  c:=chr(p^.value and $ff);
                  strval:=@c;
                  strlength:=1
                end
              else if is_constresourcestringnode(p) then
                begin
                  strval:=pchar(pconstsym(p^.symtableentry)^.value);
                  strlength:=pconstsym(p^.symtableentry)^.len;
                end
              else
                begin
                  Message(cg_e_illegal_expression);
                  strlength:=-1;
                end;
              if strlength>=0 then
               begin
                 case pstringdef(def)^.string_typ of
                   st_shortstring:
                     begin
                       if strlength>=def^.size then
                        begin
                          message2(parser_w_string_too_long,strpas(strval),tostr(def^.size-1));
                          strlength:=def^.size-1;
                        end;
                       curconstsegment^.concat(new(pai_const,init_8bit(strlength)));
                       { this can also handle longer strings }
                       getmem(ca,strlength+1);
                       move(strval^,ca^,strlength);
                       ca[strlength]:=#0;
                       curconstsegment^.concat(new(pai_string,init_length_pchar(ca,strlength)));
                       { fillup with spaces if size is shorter }
                       if def^.size>strlength then
                        begin
                          getmem(ca,def^.size-strlength);
                          { def^.size contains also the leading length, so we }
                          { we have to subtract one                       }
                          fillchar(ca[0],def^.size-strlength-1,' ');
                          ca[def^.size-strlength-1]:=#0;
                          { this can also handle longer strings }
                          curconstsegment^.concat(new(pai_string,init_length_pchar(ca,def^.size-strlength-1)));
                        end;
                     end;
{$ifdef UseLongString}
                   st_longstring:
                     begin
                       { first write the maximum size }
                       curconstsegment^.concat(new(pai_const,init_32bit(strlength)))));
                       { fill byte }
                       curconstsegment^.concat(new(pai_const,init_8bit(0)));
                       getmem(ca,strlength+1);
                       move(strval^,ca^,strlength);
                       ca[strlength]:=#0;
                       generate_pascii(consts,ca,strlength);
                       curconstsegment^.concat(new(pai_const,init_8bit(0)));
                     end;
{$endif UseLongString}
                   st_ansistring:
                     begin
                        { an empty ansi string is nil! }
                        if (strlength=0) then
                          curconstsegment^.concat(new(pai_const,init_32bit(0)))
                        else
                          begin
                            getdatalabel(ll);
                            curconstsegment^.concat(new(pai_const_symbol,init(ll)));
                            { first write the maximum size }
                            consts^.concat(new(pai_const,init_32bit(strlength)));
                            { second write the real length }
                            consts^.concat(new(pai_const,init_32bit(strlength)));
                            { redondent with maxlength but who knows ... (PM) }
                            { third write use count (set to -1 for safety ) }
                            consts^.concat(new(pai_const,init_32bit(-1)));
                            consts^.concat(new(pai_label,init(ll)));
                            getmem(ca,strlength+2);
                            move(strval^,ca^,strlength);
                            { The terminating #0 to be stored in the .data section (JM) }
                            ca[strlength]:=#0;
                            { End of the PChar. The memory has to be allocated because in }
                            { tai_string.done, there is a freemem(len+1) (JM)             }
                            ca[strlength+1]:=#0;
                            consts^.concat(new(pai_string,init_length_pchar(ca,strlength+1)));
                          end;
                     end;
                 end;
               end;
              disposetree(p);
           end;
         arraydef:
           begin
              if token=_LKLAMMER then
                begin
                    consume(_LKLAMMER);
                    for l:=parraydef(def)^.lowrange to parraydef(def)^.highrange-1 do
                      begin
                         readtypedconst(parraydef(def)^.elementtype.def,nil,no_change_allowed);
                         consume(_COMMA);
                      end;
                    readtypedconst(parraydef(def)^.elementtype.def,nil,no_change_allowed);
                    consume(_RKLAMMER);
                 end
              else
              { if array of char then we allow also a string }
               if is_char(parraydef(def)^.elementtype.def) then
                begin
                   p:=comp_expr(true);
                   do_firstpass(p);
                   if p^.treetype=stringconstn then
                    begin
                      len:=p^.length;
                      { For tp7 the maximum lentgh can be 255 }
                      if (m_tp in aktmodeswitches) and
                         (len>255) then
                       len:=255;
                      ca:=p^.value_str;
                    end
                   else
                     if is_constcharnode(p) then
                      begin
                        c:=chr(p^.value and $ff);
                        ca:=@c;
                        len:=1;
                      end
                   else
                     begin
                       Message(cg_e_illegal_expression);
                       len:=0;
                     end;
                   if len>(Parraydef(def)^.highrange-Parraydef(def)^.lowrange+1) then
                     Message(parser_e_string_larger_array);
                   for i:=Parraydef(def)^.lowrange to Parraydef(def)^.highrange do
                     begin
                        if i+1-Parraydef(def)^.lowrange<=len then
                          begin
                             curconstsegment^.concat(new(pai_const,init_8bit(byte(ca^))));
                             inc(ca);
                          end
                        else
                          {Fill the remaining positions with #0.}
                          curconstsegment^.concat(new(pai_const,init_8bit(0)));
                     end;
                   disposetree(p);
                end
              else
                begin
                  { we want the ( }
                  consume(_LKLAMMER);
                end;
           end;
         procvardef:
           begin
              { Procvars and pointers are no longer compatible.  }
              { under tp:  =nil or =var under fpc: =nil or =@var }
              if token=_NIL then
                begin
                   curconstsegment^.concat(new(pai_const,init_32bit(0)));
                   if (po_methodpointer in pprocvardef(def)^.procoptions) then
                     curconstsegment^.concat(new(pai_const,init_32bit(0)));
                   consume(_NIL);
                   exit;
                end;
              { you can't assign a value other than NIL to a typed constant  }
              { which is a "procedure of object", because this also requires }
              { address of an object/class instance, which is not known at   }
              { compile time (JM)                                            }
              if (po_methodpointer in pprocvardef(def)^.procoptions) then
                Message(parser_e_no_procvarobj_const);
                { parse the rest too, so we can continue with error checking }
              getprocvar:=true;
              getprocvardef:=pprocvardef(def);
              p:=comp_expr(true);
              getprocvar:=false;
              do_firstpass(p);
              if codegenerror then
               begin
                 disposetree(p);
                 exit;
               end;
             { let type conversion check everything needed }
              p:=gentypeconvnode(p,def);
              do_firstpass(p);
              if codegenerror then
               begin
                 disposetree(p);
                 exit;
               end;
              { remove typeconvn, that will normally insert a lea
                instruction which is not necessary for us }
              if p^.treetype=typeconvn then
               begin
                 hp:=p^.left;
                 putnode(p);
                 p:=hp;
               end;
              { remove addrn which we also don't need here }
              if p^.treetype=addrn then
               begin
                 hp:=p^.left;
                 putnode(p);
                 p:=hp;
               end;
              { we now need to have a loadn with a procsym }
              if (p^.treetype=loadn) and
                 (p^.symtableentry^.typ=procsym) then
               begin
                 curconstsegment^.concat(new(pai_const_symbol,
                   initname(pprocsym(p^.symtableentry)^.definition^.mangledname)));
               end
              else
               Message(cg_e_illegal_expression);
              disposetree(p);
           end;
         { reads a typed constant record }
         recorddef:
           begin
              consume(_LKLAMMER);
              s:='<record start>';
              aktpos:=0;
              { can't use srsym here, because that one can be modified }
              { by the call to readtypedconst below (JM)               }
              recsym1 := psym(precorddef(def)^.symtable^.symindex^.first);
              recsym2:=nil;
              { assume first field is variant start for now }
              while token<>_RKLAMMER do
                begin
                   s:=pattern;
                   consume(_ID);
                   consume(_COLON);
                   error := false;
                   recsym2 := psym(precorddef(def)^.symtable^.search(s));
                   if not assigned(recsym2) then
                     begin
                       Message1(sym_e_illegal_field,s);
                       error := true;
                     end;
                   if (not error) and
                      (not assigned(recsym1) or
                       (s <> recsym1^.name)) then
                     { possible variant record (JM) }
                     begin
                       { All parts of a variant start at the same offset      }
                       { Also allow jumping from one variant part to another, }
                       { as long as the offsets match                         }
                       if (assigned(recsym1) and
                           (pvarsym(recsym2)^.address = pvarsym(recsym1)^.address)) or
                          { recsym1 is not assigned after parsing w2 in the    }
                          { typed const in the next example:                   }
                          {   type tr = record case byte of                    }
                          {          1: (l1,l2: dword);                        }
                          {          2: (w1,w2: word);                         }
                          {        end;                                        }
                          {   const r: tr = (w1:1;w2:1;l2:5);                  }
                          (pvarsym(recsym2)^.address = aktpos) then
                         recsym1 := recsym2
                       { going backwards isn't allowed in any mode }
                       else if (pvarsym(recsym2)^.address<aktpos) then
                         begin
                           Message(parser_e_invalid_record_const);
                           error := true;
                         end
                       { Delphi allows you to skip fields }
                       else if (m_delphi in aktmodeswitches) then
                         begin
                           Message1(parser_w_skipped_fields_before,s);
                           recsym1 := recsym2;
                         end
                       { FPC and TP don't }
                       else
                         begin
                           Message1(parser_e_skipped_fields_before,s);
                           error := true;
                         end;
                     end;
                   if error then
                     consume_all_until(_SEMICOLON)
                   else
                     begin
                        { if needed fill (alignment) }
                        if pvarsym(recsym1)^.address>aktpos then
                          for i:=1 to pvarsym(recsym1)^.address-aktpos do
                            curconstsegment^.concat(new(pai_const,init_8bit(0)));

                        { new position }
                        aktpos:=pvarsym(recsym1)^.address+pvarsym(recsym1)^.vartype.def^.size;

                        { read the data }
                        readtypedconst(pvarsym(recsym1)^.vartype.def,nil,no_change_allowed);

                        { keep previous field for checking whether whole }
                        { record was initialized (JM)                    }
                        recsym2 := recsym1;
                        { goto next field }
                        recsym1 := psym(recsym1^.indexnext);
                        if token=_SEMICOLON then
                          consume(_SEMICOLON)
                        else break;
                     end;
                end;

              { are there any fields left?                            }
              if assigned(recsym1) and
                 { don't complain if there only come other variant parts }
                 { after the last initialized field                      }
                 ((recsym2=nil) or
                  (pvarsym(recsym1)^.address > pvarsym(recsym2)^.address)) then
                Message1(parser_h_skipped_fields_after,s);

              for i:=1 to def^.size-aktpos do
                curconstsegment^.concat(new(pai_const,init_8bit(0)));
              consume(_RKLAMMER);
           end;
         { reads a typed object }
         objectdef:
           begin
              if (([oo_has_vmt,oo_is_class]*pobjectdef(def)^.objectoptions)<>[])
                 and not(m_tp in aktmodeswitches) then
                begin
                   { support nil assignment for classes }
                   if pobjectdef(def)^.is_class and
                      try_to_consume(_NIL) then
                    begin
                      curconstsegment^.concat(new(pai_const,init_32bit(0)));
                    end
                   else
                    begin
                      Message(parser_e_type_const_not_possible);
                      consume_all_until(_RKLAMMER);
                    end;
                end
              else
                begin
                   consume(_LKLAMMER);
                   aktpos:=0;
                   while token<>_RKLAMMER do
                     begin
                        s:=pattern;
                        consume(_ID);
                        consume(_COLON);
                        srsym:=nil;
                        obj:=pobjectdef(def);
                        symt:=obj^.symtable;
                        while (srsym=nil) and assigned(symt) do
                          begin
                             srsym:=symt^.search(s);
                             if assigned(obj) then
                               obj:=obj^.childof;
                             if assigned(obj) then
                               symt:=obj^.symtable
                             else
                               symt:=nil;
                          end;

                        if srsym=nil then
                          begin
                             Message1(sym_e_id_not_found,s);
                             consume_all_until(_SEMICOLON);
                          end
                        else
                          begin
                             { check position }
                             if pvarsym(srsym)^.address<aktpos then
                               Message(parser_e_invalid_record_const);

                             { check in VMT needs to be added for TP mode }
                             if (m_tp in aktmodeswitches) and
                                (oo_has_vmt in pobjectdef(def)^.objectoptions) and
                                (pobjectdef(def)^.vmt_offset<pvarsym(srsym)^.address) then
                               begin
                                 for i:=1 to pobjectdef(def)^.vmt_offset-aktpos do
                                   curconstsegment^.concat(new(pai_const,init_8bit(0)));
                                 curconstsegment^.concat(new(pai_const_symbol,initname(pobjectdef(def)^.vmt_mangledname)));
                                 { this is more general }
                                 aktpos:=pobjectdef(def)^.vmt_offset + target_os.size_of_pointer;
                               end;
                             { if needed fill }
                             if pvarsym(srsym)^.address>aktpos then
                               for i:=1 to pvarsym(srsym)^.address-aktpos do
                                 curconstsegment^.concat(new(pai_const,init_8bit(0)));

                             { new position }
                             aktpos:=pvarsym(srsym)^.address+pvarsym(srsym)^.vartype.def^.size;

                             { read the data }
                             readtypedconst(pvarsym(srsym)^.vartype.def,nil,no_change_allowed);

                             if token=_SEMICOLON then
                               consume(_SEMICOLON)
                             else break;
                          end;
                     end;
                   if (m_tp in aktmodeswitches) and
                      (oo_has_vmt in pobjectdef(def)^.objectoptions) and
                      (pobjectdef(def)^.vmt_offset>=aktpos) then
                     begin
                       for i:=1 to pobjectdef(def)^.vmt_offset-aktpos do
                         curconstsegment^.concat(new(pai_const,init_8bit(0)));
                       curconstsegment^.concat(new(pai_const_symbol,initname(pobjectdef(def)^.vmt_mangledname)));
                       { this is more general }
                       aktpos:=pobjectdef(def)^.vmt_offset + target_os.size_of_pointer;
                     end;
                   for i:=1 to def^.size-aktpos do
                     curconstsegment^.concat(new(pai_const,init_8bit(0)));
                   consume(_RKLAMMER);
                end;
           end;
         errordef:
           begin
              { try to consume something useful }
              if token=_LKLAMMER then
                consume_all_until(_RKLAMMER)
              else
                consume_all_until(_SEMICOLON);
           end;
         else Message(parser_e_type_const_not_possible);
         end;
      end;
{$ifdef fpc}
{$maxfpuregisters default}
{$endif fpc}

end.
{
  $Log: ptconst.pas,v $
  Revision 1.1.2.25  2003/02/02 13:29:24  peter
    * fix uninited var when first record field is skipped

  Revision 1.1.2.24  2003/01/05 16:29:43  peter
    * fixed crash when defining a non-existing record element

  Revision 1.1.2.23  2002/12/17 12:42:48  pierre
   * use get_low and get_high

  Revision 1.1.2.22  2002/09/15 16:38:57  carl
    * alignment fixes for m68k only (ifdef m68k)

  Revision 1.1.2.21  2001/10/29 13:44:46  jonas
    * typed constants that are "procedure of object" and which are assigned
      nil require 8 bytes of "0" (not 4)
    * fixed web bug 1655 (reject the code)

  Revision 1.1.2.20  2001/09/15 04:18:37  carl
  * alignment fixes for non-intel 80x86 targets.

  Revision 1.1.2.19  2001/09/07 07:23:51  pierre
   * handle negative constants for int64 type correctly

  Revision 1.1.2.18  2001/09/07 06:51:28  pierre
   * write 64int constants correctly according to target endianess

  Revision 1.1.2.17  2001/09/02 21:42:06  peter
    * better fix for procvardef to pointerdef conversion in typed const

  Revision 1.1.2.16  2001/08/31 16:44:01  pierre
   * first try to fix the wrong typecast to ppointerdef for a procvardef

  Revision 1.1.2.15  2001/07/30 13:16:28  pierre
   * set recsym2 to nil at start of const record parsing

  Revision 1.1.2.14  2001/07/25 15:12:23  pierre
   * second char problem fixed

  Revision 1.1.2.13  2001/07/25 15:04:16  pierre
   * fix char handling for string constants

  Revision 1.1.2.12  2001/07/25 13:40:28  pierre
   * store set constants correctly for different endianess

  Revision 1.1.2.11  2001/06/25 02:39:25  carl
  * big_endian fixes for constant sets (they are considered arrays of longint now)

  Revision 1.1.2.10  2001/06/14 03:36:42  carl
  * A long long time ago in a far away kingdom someone made sure that constant set array are saved are bytes, therefore now platform independant.

  Revision 1.1.2.9  2001/06/07 21:38:07  pierre
   * change warning about unset fields into hint

  Revision 1.1.2.8  2001/06/06 17:23:38  jonas
    * fixed inconsistent handling of procvars in FPC mode (sometimes @ was
      required to assign the address of a procedure to a procvar,, sometimes
      not. Now it is always required)

  Revision 1.1.2.7  2001/05/06 17:14:15  jonas
    + detect incomplete typed constant records

  Revision 1.1.2.6  2001/02/04 11:11:27  jonas
    * support for evaluation of constant pointer expressions

  Revision 1.1.2.5  2001/02/01 23:23:51  pierre
   * fix for bug 1365

  Revision 1.1.2.4  2000/12/10 20:15:59  peter
    * also check for subtypes for enumerations

  Revision 1.1.2.3  2000/09/30 13:12:32  peter
    * const array of char and pchar length fixed

  Revision 1.1.2.2  2000/08/24 19:10:51  peter
    * allow nil for class typed consts

  Revision 1.1.2.1  2000/08/05 13:21:52  peter
    * fixed enumwriting with enumsize <> 4

  Revision 1.1  2000/07/13 06:29:55  michael
  + Initial import

  Revision 1.68  2000/06/06 13:06:17  jonas
    * ansistring constants now also get a trailing #0 (bug reported by
      Thomas Schatzl)

  Revision 1.67  2000/05/17 17:10:06  peter
    * add support for loading of typed const strings with resourcestrings,
      made the loading also a bit more generic

  Revision 1.66  2000/05/12 06:02:01  pierre
   * * get it to compile with Delphi by Kovacs Attila Zoltan

  Revision 1.65  2000/05/11 09:15:15  pierre
    + add a warning if a const string is longer than the
      length of the string type

  Revision 1.64  2000/04/02 09:12:51  florian
    + constant procedure variables can have a @ in front:
         const p : procedure = @p;
      til now only
         const p : procedure = p;
      was allowed

  Revision 1.63  2000/02/13 14:21:51  jonas
    * modifications to make the compiler functional when compiled with
      -Or

  Revision 1.62  2000/02/09 13:23:01  peter
    * log truncated

  Revision 1.61  2000/01/07 01:14:33  peter
    * updated copyright to 2000

  Revision 1.60  1999/12/18 14:55:21  florian
    * very basic widestring support

  Revision 1.59  1999/11/30 10:40:51  peter
    + ttype, tsymlist

  Revision 1.58  1999/11/08 18:50:11  florian
    * disposetree for classrefdef added

  Revision 1.57  1999/11/08 16:24:28  pierre
   * missing disposetree added to avoid memory loss

  Revision 1.56  1999/11/08 14:02:16  florian
    * problem with "index X"-properties solved
    * typed constants of class references are now allowed

  Revision 1.55  1999/11/06 14:34:23  peter
    * truncated log to 20 revs

  Revision 1.54  1999/10/14 14:57:54  florian
    - removed the hcodegen use in the new cg, use cgbase instead

  Revision 1.53  1999/09/26 21:30:20  peter
    + constant pointer support which can happend with typecasting like
      const p=pointer(1)
    * better procvar parsing in typed consts

  Revision 1.52  1999/08/10 12:30:02  pierre
   * avoid unused locals

  Revision 1.51  1999/08/04 13:03:02  jonas
    * all tokens now start with an underscore
    * PowerPC compiles!!

  Revision 1.50  1999/08/04 00:23:21  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.49  1999/08/03 22:03:08  peter
    * moved bitmask constants to sets
    * some other type/const renamings

  Revision 1.48  1999/07/23 16:05:26  peter
    * alignment is now saved in the symtable
    * C alignment added for records
    * PPU version increased to solve .12 <-> .13 probs

}
