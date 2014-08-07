{
    $Id: cginl.pas,v 1.1.2.25 2003/01/20 15:13:54 pierre Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate m68k inline nodes

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
unit cginl;
interface

    uses
      tree;

    procedure secondinline(var p : ptree);


implementation

    uses
      globtype,systems,
      cobjects,verbose,globals,files,
      symconst,symtable,aasm,types,
      hcodegen,temp_gen,pass_1,pass_2,
      cpubase,cpuasm,
      cga,tgen,cgcal,cgbase;


{*****************************************************************************
                                Helpers
*****************************************************************************}

    { reverts the parameter list }
    var nb_para : integer;

    function reversparameter(p : ptree) : ptree;

       var
         hp1,hp2 : ptree;

      begin
         hp1:=nil;
         nb_para := 0;
         while assigned(p) do
           begin
              { pull out }
              hp2:=p;
              p:=p^.right;
              inc(nb_para);
              { pull in }
              hp2^.right:=hp1;
              hp1:=hp2;
           end;
         reversparameter:=hp1;
       end;


{*****************************************************************************
                             SecondInLine
*****************************************************************************}

    procedure StoreDirectFuncResult(var dest:ptree);
      var
        hp : ptree;
        hdef : porddef;
        hreg : tregister;
        hregister : tregister;
        oldregisterdef : boolean;
        op : tasmop;
        opsize : topsize;
        floatloc : tlocation;

      begin
        { Get the accumulator first so it can't be used in the dest }
        if (dest^.resulttype^.deftype=orddef) and
          not(is_64bitint(dest^.resulttype)) then
          hregister:=getexplicitregister32(accumulator);
        { process dest }
        SecondPass(dest);
        if Codegenerror then
         exit;
        { store the value }
        Case dest^.resulttype^.deftype of
          floatdef:
              begin
                floatloc.loc:=LOC_FPU;
                if ((cs_fp_emulation) in aktmoduleswitches) then
                  floatloc.fpuregister:=R_D0
                else
                  floatloc.fpuregister:=R_FP0;
                floatstore(PFloatDef(dest^.resulttype)^.typ,floatloc, dest^.location.reference);
              end;
          orddef:
            begin
              if is_64bitint(dest^.resulttype) then
                begin
                   emit_movq_reg_loc(R_D0,R_D1,dest^.location);
                end
              else
               begin
                 opsize := def_opsize(dest^.resulttype);
                 Case dest^.resulttype^.size of
                  1 : emit_const_reg(A_AND,S_L,$FF,hregister);
                  2 : emit_const_reg(A_AND,S_L,$FFFF,hregister);
                 End;
                 hreg := hregister;
                 case dest^.location.loc of
                  LOC_REFERENCE,
                  LOC_MEM:
                       begin
                         emit_reg_ref(A_MOVE,opsize,hreg,
                           newreference(dest^.location.reference));
                       end;
                  LOC_REGISTER,
                  LOC_CREGISTER:
                       begin
                         emit_reg_reg(A_MOVE,opsize,hreg,
                           dest^.location.register);
                       end;
                 else
                   internalerror(330);
                 end;
                 If (cs_check_range in aktlocalswitches) and
                    {no need to rangecheck longints or cardinals on 32bit processors}
                    not((porddef(dest^.resulttype)^.typ = s32bit) and
                        (porddef(dest^.resulttype)^.low = longint($80000000)) and
                        (porddef(dest^.resulttype)^.high = $7fffffff)) and
                    not((porddef(dest^.resulttype)^.typ = u32bit) and
                        (porddef(dest^.resulttype)^.low = 0) and
                        (porddef(dest^.resulttype)^.high = longint($ffffffff))) then
                  Begin
                    {do not register this temporary def}
                    OldRegisterDef := RegisterDef;
                    RegisterDef := False;
                    hdef:=nil;
                    Case PordDef(dest^.resulttype)^.typ of
                      u8bit,u16bit,u32bit:
                        begin
                          new(hdef,init(u32bit,0,$ffffffff));
                          hreg:=hregister;
                        end;
                      s8bit,s16bit,s32bit:
                        begin
                          new(hdef,init(s32bit,$80000000,$7fffffff));
                          hreg:=hregister;
                        end;
                    end;
                    { create a fake node }
                    hp := genzeronode(nothingn);
                    hp^.location.loc := LOC_REGISTER;
                    hp^.location.register := hreg;
                    if assigned(hdef) then
                      hp^.resulttype:=hdef
                    else
                      hp^.resulttype:=dest^.resulttype;
                    { emit the range check }
                    emitrangecheck(hp,dest^.resulttype);
                    hp^.right := nil;
                    if assigned(hdef) then
                      Dispose(hdef, Done);
                    RegisterDef := OldRegisterDef;
                    disposetree(hp);
                  End;
                 ungetregister(hregister);
               end;
            End;
          else
            internalerror(66766766);
        end;
        { free used registers }
        del_locref(dest^.location);
      end;


    procedure secondinline(var p : ptree);
       const
         {tfloattype = (s32real,s64real,s80real,s64bit,f16bit,f32bit);}
{        float_name: array[tfloattype] of string[8]=
           ('S32REAL','S64REAL','S80REAL','S64BIT','F16BIT','F32BIT'); }
         addsubop:array[in_inc_x..in_dec_x] of tasmop=(A_ADD,A_SUB);
         addsubopx:array[in_inc_x..in_dec_x] of tasmop=(A_ADDX,A_SUBX);
       var
         aktfile : treference;
         ft : tfiletyp;
         opsize : topsize;
         op,
         asmop : tasmop;
         pushed : tpushed;
         {inc/dec}
         addconstant : boolean;
         addvalue : longint;
         savedregs : tpushed;


      procedure handlereadwrite(doread,doln : boolean);
      { produces code for READ(LN) and WRITE(LN) }

        procedure loadstream;
          const
            io:array[boolean] of string[7]=('_OUTPUT','_INPUT');
          var
            r : preference;
          begin
            new(r);
            reset_reference(r^);
            r^.symbol:=newasmsymbol(
            'U_'+upper(target_info.system_unit)+io[doread]);
            emit_ref_reg(A_LEA,S_L,r,R_A0)
          end;

        const
           rdwrprefix:array[boolean] of string[15]=('FPC_WRITE_TEXT_','FPC_READ_TEXT_');
        var
           node,hp    : ptree;
           typedtyp,
           pararesult : pdef;
           orgfloattype : tfloattype;
           dummycoll  : tparaitem;
           iolabel    : pasmlabel;
           npara      : longint;
           esireloaded : boolean;

        begin
           { here we don't use register calling conventions }
           dummycoll.init;
           dummycoll.register:=R_NO;
           { I/O check }
           if (cs_check_io in aktlocalswitches) and
              not(po_iocheck in aktprocsym^.definition^.procoptions) then
             begin
                getlabel(iolabel);
                emitlab(iolabel);
             end
           else
             iolabel:=nil;
           { for write of real with the length specified }
           hp:=nil;
           { reserve temporary pointer to data variable }
           aktfile.symbol:=nil;
           gettempofsizereference(4,aktfile);
           { first state text data }
           ft:=ft_text;
           { and state a parameter ? }
           if p^.left=nil then
             begin
                { the following instructions are for "writeln;" }
                loadstream;
                { save @aktfile in temporary variable }
                emit_reg_ref(A_MOVE,S_L,R_A0,newreference(aktfile));
             end
           else
             begin
                { revers paramters }
                node:=reversparameter(p^.left);

                p^.left := node;
                npara := nb_para;
                { calculate data variable }
                { is first parameter a file type ? }
                if node^.left^.resulttype^.deftype=filedef then
                  begin
                     ft:=pfiledef(node^.left^.resulttype)^.filetyp;
                     if ft=ft_typed then
                       typedtyp:=pfiledef(node^.left^.resulttype)^.typedfiletype.def;
                     secondpass(node^.left);
                     if codegenerror then
                       exit;

                     { save reference in temporary variables }
                     if node^.left^.location.loc<>LOC_REFERENCE then
                       begin
                          CGMessage(cg_e_illegal_expression);
                          exit;
                       end;

                     emit_ref_reg(A_LEA,S_L,newreference(node^.left^.location.reference),R_A0);
                     del_reference(node^.left^.location.reference);
                     { skip to the next parameter }
                     node:=node^.right;
                  end
                else
                  begin
                  { load stdin/stdout stream }
                     loadstream;
                  end;

                { save @aktfile in temporary variable }
                emit_reg_ref(A_MOVE,S_L,R_A0,newreference(aktfile));
                if doread or
		   (ft=ft_typed) then
                { parameter by READ gives call by reference }
                  dummycoll.paratyp:=vs_var
                { an WRITE Call by "Const" }
                else
                  dummycoll.paratyp:=vs_const;

                { because of secondcallparan, which otherwise attaches }
                if ft=ft_typed then
                  { this is to avoid copy of simple const parameters }
                  {dummycoll.data:=new(pformaldef,init)}
                  dummycoll.paratype.setdef(cformaldef)
                else
                  { I think, this isn't a good solution (FK) }
                  dummycoll.paratype.reset;

                while assigned(node) do
                  begin
                     esireloaded:=false;
                     saveusedregisters(pushed,ALL_REGISTERS);
                     hp:=node;
                     node:=node^.right;
                     hp^.right:=nil;
                     if hp^.is_colon_para then
                       CGMessage(parser_e_illegal_colon_qualifier);
                     { when float is written then we need bestreal to be pushed
                       convert here else we loose the old float type }
                     if (not doread) and
                        (ft<>ft_typed) and
                        (hp^.left^.resulttype^.deftype=floatdef) then
                      begin
                        orgfloattype:=pfloatdef(hp^.left^.resulttype)^.typ;
                        hp^.left:=gentypeconvnode(hp^.left,bestrealdef^);
                        firstpass(hp^.left);
                      end;
                     { when read ord,floats are functions, so they need this
                       parameter as their destination instead of being pushed }
                     if doread and
                        (ft<>ft_typed) and
                        (hp^.resulttype^.deftype in [orddef,floatdef]) then
                      begin
                      end
                     else
                      begin
                        { reset data type }
                        dummycoll.paratype.reset;
                        { create temporary defs for high tree generation }
                        if doread and (is_shortstring(hp^.resulttype)) then
                          dummycoll.paratype.setdef(openshortstringdef)
                        else
                          if (is_chararray(hp^.resulttype)) then
                            dummycoll.paratype.setdef(openchararraydef);
                        secondcallparan(hp,@dummycoll,false,false,false,0,0);
                      end;
                     hp^.right:=node;
                     if codegenerror then
                       exit;

                     emit_push_mem(aktfile);
                     if (ft=ft_typed) then
                       begin
                          { OK let's try this }
                          { first we must only allow the right type }
                          { we have to call blockread or blockwrite }
                          { but the real problem is that            }
                          { reset and rewrite should have set       }
                          { the type size                          }
                          { as recordsize for that file !!!!    }
                          { how can we make that                    }
                          { I think that is only possible by adding }
                          { reset and rewrite to the inline list a call }
                          { allways read only one record by element }
                            push_int(typedtyp^.size);
                            if doread then
                              emitcall('FPC_TYPED_READ')
                            else
                              emitcall('FPC_TYPED_WRITE');
                       end
                     else
                       begin
                          { save current position }
                          pararesult:=hp^.left^.resulttype;
                          { handle possible field width  }
                          { of course only for write(ln) }
                          if not doread then
                            begin
                               { handle total width parameter }
                              if assigned(node) and node^.is_colon_para then
                                begin
                                   hp:=node;
                                   node:=node^.right;
                                   hp^.right:=nil;
                                   dummycoll.paratype.setdef(hp^.resulttype);
                                   dummycoll.paratyp:=vs_value;
                                   secondcallparan(hp,@dummycoll,false,false,false,0,0);
                                   hp^.right:=node;
                                   if codegenerror then
                                     exit;
                                end
                              else
                                if pararesult^.deftype<>floatdef then
                                  push_int(0)
                                else
                                  push_int(-32767);
                            { a second colon para for a float ? }
                              if assigned(node) and node^.is_colon_para then
                                begin
                                   hp:=node;
                                   node:=node^.right;
                                   hp^.right:=nil;
                                   dummycoll.paratype.setdef(hp^.resulttype);
                                   dummycoll.paratyp:=vs_value;
                                   secondcallparan(hp,@dummycoll,false,false,false,0,0);
                                   hp^.right:=node;
                                   if pararesult^.deftype<>floatdef then
                                     CGMessage(parser_e_illegal_colon_qualifier);
                                   if codegenerror then
                                     exit;
                                end
                              else
                                begin
                                  if pararesult^.deftype=floatdef then
                                    push_int(-1);
                                end;
                             { push also the real type for floats }
                              if pararesult^.deftype=floatdef then
                                push_int(ord(orgfloattype));
                            end;
                          case pararesult^.deftype of
                            stringdef :
                              begin
                                emitcall(rdwrprefix[doread]+pstringdef(pararesult)^.stringtypname);
                              end;
                            pointerdef :
                              begin
                                if is_pchar(pararesult) then
                                  emitcall(rdwrprefix[doread]+'PCHAR_AS_POINTER')
                              end;
                            arraydef :
                              begin
                                if is_chararray(pararesult) then
                                  emitcall(rdwrprefix[doread]+'PCHAR_AS_ARRAY')
                              end;
                            floatdef :
                              begin
                                emitcall(rdwrprefix[doread]+'FLOAT');
                                {
                                if pfloatdef(p^.resulttype)^.typ<>f32bit then
                                  dec(fpuvaroffset);
                                }
                                if doread then
                                  begin
                                     maybe_loadself;
                                     esireloaded:=true;
                                     StoreDirectFuncResult(hp^.left);
                                  end;
                              end;
                            orddef :
                              begin
                                case porddef(pararesult)^.typ of
                                  s8bit,s16bit,s32bit :
                                    emitcall(rdwrprefix[doread]+'SINT');
                                  u8bit,u16bit,u32bit :
                                    emitcall(rdwrprefix[doread]+'UINT');
                                  uchar :
                                    emitcall(rdwrprefix[doread]+'CHAR');
                                  s64bit :
                                    emitcall(rdwrprefix[doread]+'INT64');
                                  u64bit :
                                    emitcall(rdwrprefix[doread]+'QWORD');
                                  bool8bit,
                                  bool16bit,
                                  bool32bit :
                                    emitcall(rdwrprefix[doread]+'BOOLEAN');
                                end;
                                if doread then
                                  begin
                                     maybe_loadself;
                                     esireloaded:=true;
                                     StoreDirectFuncResult(hp^.left);
                                  end;
                              end;
                          end;
                       end;
                   { load ESI in methods again }
                     restoreusedregisters(pushed);
                     if not(esireloaded) then
                       maybe_loadself;
                  end;
             end;
         { Insert end of writing for textfiles }
           if ft=ft_text then
             begin
               saveusedregisters(pushed,ALL_REGISTERS);
               emit_push_mem(aktfile);
               if doread then
                begin
                  if doln then
                    emitcall('FPC_READLN_END')
                  else
                    emitcall('FPC_READ_END');
                end
               else
                begin
                  if doln then
                    emitcall('FPC_WRITELN_END')
                  else
                    emitcall('FPC_WRITE_END');
                end;
               restoreusedregisters(pushed);
               maybe_loadself;
             end;
         { Insert IOCheck if set }
           if assigned(iolabel) then
             begin
                { registers are saved in the procedure }
                emit_sym(A_PEA,S_L,iolabel);
                emitcall('FPC_IOCHECK');
             end;
         { Freeup all used temps }
           ungetiftemp(aktfile);
           if assigned(p^.left) then
             begin
                p^.left:=reversparameter(p^.left);
                if npara<>nb_para then
                  CGMessage(cg_f_internal_error_in_secondinline);
                hp:=p^.left;
                while assigned(hp) do
                  begin
                     if assigned(hp^.left) then
                       if (hp^.left^.location.loc in [LOC_MEM,LOC_REFERENCE]) then
                         ungetiftemp(hp^.left^.location.reference);
                     hp:=hp^.right;
                  end;
             end;
        end;

      procedure handle_str;

        var
           hp,node : ptree;
           dummycoll : tparaitem;
           is_real : boolean;
           realtype : tfloattype;
           procedureprefix : string;

          begin
           dummycoll.init;
           dummycoll.register:=R_NO;
           saveusedregisters(pushed,ALL_REGISTERS);
           node:=p^.left;
           is_real:=false;
           while assigned(node^.right) do node:=node^.right;
           { if a real parameter somewhere then call REALSTR }
           if (node^.left^.resulttype^.deftype=floatdef) then
            begin
              is_real:=true;
              realtype:=pfloatdef(node^.left^.resulttype)^.typ;
            end;

           node:=p^.left;
           { we have at least two args }
           { with at max 2 colon_para in between }

           { string arg }
           hp:=node;
           node:=node^.right;
           hp^.right:=nil;
           dummycoll.paratyp:=vs_var;
           if (hp^.left^.treetype=nothingn) then
             begin
               { we use a temp string }
               dummycoll.paratype.setdef(openshortstringdef);
               hp^.left^.location.loc:=LOC_REFERENCE;
               gettempofsizereference(256,hp^.left^.location.reference);
               p^.location.loc:=LOC_REFERENCE;
               p^.location.reference:=hp^.left^.location.reference;
             end
           else if is_shortstring(hp^.resulttype) then
             dummycoll.paratype.setdef(openshortstringdef)
           else
             dummycoll.paratype.setdef(hp^.resulttype);
           procedureprefix:='FPC_'+pstringdef(hp^.resulttype)^.stringtypname+'_';
           secondcallparan(hp,@dummycoll,false,false,false,0,0);
           if codegenerror then
             exit;

           dummycoll.paratyp:=vs_const;
           disposetree(p^.left);
           p^.left:=nil;
           { second arg }
           hp:=node;
           node:=node^.right;
           hp^.right:=nil;

           { if real push real type }
           if is_real then
             push_int(ord(realtype));

           { frac  para }
           if hp^.is_colon_para and assigned(node) and
              node^.is_colon_para then
             begin
                dummycoll.paratype.setdef(hp^.resulttype);
                dummycoll.paratyp:=vs_value;
                secondcallparan(hp,@dummycoll,false,false,false,0,0);
                if codegenerror then
                  exit;
                disposetree(hp);
                hp:=node;
                node:=node^.right;
                hp^.right:=nil;
             end
           else
             if is_real then
             push_int(-1);

           { third arg, length only if is_real }
           if hp^.is_colon_para then
             begin
                dummycoll.paratype.setdef(hp^.resulttype);
                dummycoll.paratyp:=vs_value;
                secondcallparan(hp,@dummycoll,false,false,false,0,0);
                if codegenerror then
                  exit;
                disposetree(hp);
                hp:=node;
                node:=node^.right;
                hp^.right:=nil;
             end
           else
             if is_real then
               push_int(-32767)
             else
               push_int(-1);

           { Convert float to bestreal }
           if is_real then
            begin
              hp^.left:=gentypeconvnode(hp^.left,bestrealdef^);
              firstpass(hp^.left);
            end;

           { last arg longint or real }
           dummycoll.paratype.setdef(hp^.resulttype);
           dummycoll.paratyp:=vs_value;
           secondcallparan(hp,@dummycoll,false,false,false,0,0);
           if codegenerror then
             exit;

           if is_real then
             emitcall(procedureprefix+'FLOAT')
           else
             case porddef(hp^.resulttype)^.typ of
                u32bit:
                  emitcall(procedureprefix+'CARDINAL');

                u64bit:
                  emitcall(procedureprefix+'QWORD');

                s64bit:
                  emitcall(procedureprefix+'INT64');

                else
                  emitcall(procedureprefix+'LONGINT');
             end;
           disposetree(hp);

           restoreusedregisters(pushed);
        end;


        Procedure Handle_Val;
        var
           hp,node, code_para, dest_para : ptree;
           hreg,hreg2,hregc : TRegister;
           hdef: POrdDef;
           procedureprefix : string;
           hr, hr2: TReference;
           dummycoll : tparaitem;
           has_code, has_32bit_code, oldregisterdef: boolean;
           r : preference;
           reslocation : tlocation;

          begin
           dummycoll.init;
           dummycoll.register:=R_NO;
           node:=p^.left;
           hp:=node;
           node:=node^.right;
           hp^.right:=nil;
          {if we have 3 parameters, we have a code parameter}
           has_code := Assigned(node^.right);
           has_32bit_code := false;
           reset_reference(hr);
           hreg := R_NO;

           If has_code then
             Begin
               {code is an orddef, that's checked in tcinl}
               code_para := hp;
               hp := node;
               node := node^.right;
               hp^.right := nil;
               has_32bit_code := (porddef(code_para^.left^.resulttype)^.typ in [u32bit,s32bit]);
             End;

          {hp = destination now, save for later use}
           dest_para := hp;

          {if EAX is already in use, it's a register variable. Since we don't
           need another register besides EAX, release the one we got}
           If hreg <> accumulator Then ungetregister32(hreg);

          {load and push the address of the destination}
           dummycoll.paratyp:=vs_var;
           dummycoll.paratype.setdef(dest_para^.resulttype);
           secondcallparan(dest_para,@dummycoll,false,false,false,0,0);
           if codegenerror then
             exit;

          {save the regvars}
           saveusedregisters(pushed,ALL_REGISTERS);

          {now that we've already pushed the addres of dest_para^.left on the
           stack, we can put the real parameters on the stack}

           If has_32bit_code Then
             Begin
               dummycoll.paratyp:=vs_var;
               dummycoll.paratype.setdef(code_para^.resulttype);
               secondcallparan(code_para,@dummycoll,false,false,false,0,0);
               if codegenerror then
                 exit;
               Disposetree(code_para);
             End
           Else
             Begin
           {only 32bit code parameter is supported, so fake one}
               GetTempOfSizeReference(4,hr);
               emitpushreferenceaddr(hr);
             End;

          {node = first parameter = string}
           dummycoll.paratyp:=vs_const;
           dummycoll.paratype.setdef(node^.resulttype);
           secondcallparan(node,@dummycoll,false,false,false,0,0);
           if codegenerror then
             exit;

           Case dest_para^.resulttype^.deftype of
             floatdef:
               begin
                  procedureprefix := 'FPC_VAL_REAL_';
               end;
             orddef:
               if is_64bitint(dest_para^.resulttype) then
                 begin
                    if is_signed(dest_para^.resulttype) then
                      procedureprefix := 'FPC_VAL_INT64_'
                    else
                      procedureprefix := 'FPC_VAL_QWORD_';
                 end
               else
                 begin
                    if is_signed(dest_para^.resulttype) then
                      begin
                        {if we are converting to a signed number, we have to include the
                         size of the destination, so the Val function can extend the sign
                         of the result to allow proper range checking}
                        emit_const_reg(A_MOVE,S_L,dest_para^.resulttype^.size,R_SPPUSH);
                        procedureprefix := 'FPC_VAL_SINT_'
                      end
                    else
                      procedureprefix := 'FPC_VAL_UINT_';
                 end;
           End;
           emitcall(procedureprefix+pstringdef(node^.resulttype)^.stringtypname);
           { before disposing node we need to ungettemp !! PM }
           if node^.left^.location.loc in [LOC_REFERENCE,LOC_MEM] then
             ungetiftemp(node^.left^.location.reference);
           disposetree(node);
           p^.left := nil;

          {reload self register in case the dest_para/code_para is a class variable or so}
           maybe_loadself;

           If (dest_para^.resulttype^.deftype = orddef) Then
             Begin
              {store the result in a safe place, because EAX may be used by a
               register variable}
               hreg := getexplicitregister32(R_D0);
               emit_reg_reg(A_MOVE,S_L,R_D0,hreg);
               if is_64bitint(dest_para^.resulttype) then
                 begin
                    hreg2:=getexplicitregister32(R_D1);
                    emit_reg_reg(A_MOVE,S_L,R_D1,hreg2);
                 end;
              {as of now, hreg now holds the location of the result, if it was
               integer}
             End;

           { restore the register vars}

           restoreusedregisters(pushed);

           If has_code and Not(has_32bit_code) Then
             {only 16bit code is possible}
             Begin
              {load the address of the code parameter}
               secondpass(code_para^.left);
               hregc:=getregister32;
               {emit_reg(A_CLR,S_L,hregc); unneeded
                but we need to load as a long PM }
              {move the error code to its destination}
               emit_ref_reg(A_MOVE,S_L,NewReference(hr),hregc);
{               emit_mov_reg_loc(S_W,accumulator,code_para^.left^.location);}
               case code_para^.left^.location.loc of
                 LOC_REGISTER,
                 LOC_CREGISTER :
                         begin
                           emit_reg_reg(A_MOVE,S_W,
                             hregc,code_para^.left^.location.register);
                         end;
                 LOC_MEM,
                 LOC_REFERENCE :
                         begin
                           if code_para^.left^.location.reference.is_immediate then
                             internalerror(334)
                           else
                             begin
                               emit_reg_ref(A_MOVE,S_W,
                                 hregc,newreference(
                                 code_para^.left^.location.reference));
                             end;
                         end;
                else
                 internalerror(330);
               end;
               Disposetree(code_para);
               ungetregister32(hregc);
             End;

          {restore the address of the result}
           emit_reg_reg(A_MOVE,S_L,R_SPPULL,R_A0);

          {set up hr2 to a refernce with EDI as base register}
           reset_reference(hr2);
           hr2.base := R_A0;

          {save the function result in the destination variable}
           Case dest_para^.left^.resulttype^.deftype of
             floatdef:
               begin
                 reslocation.loc:=LOC_FPU;
                 if (cs_fp_emulation in aktmoduleswitches) then
                    begin
                       getexplicitregister32(R_D0);
                       reslocation.fpuregister:=R_D0;
                    end
                 else
                    reslocation.fpuregister:=R_FP0;
                 floatstore(PFloatDef(dest_para^.left^.resulttype)^.typ, reslocation,hr2);
               end;
             orddef:
               Begin
                 Case PordDef(dest_para^.left^.resulttype)^.typ of
                   u8bit,s8bit:
                     emit_reg_ref(A_MOVE,S_B,hreg,newreference(hr2));
                   u16bit,s16bit:
                     emit_reg_ref(A_MOVE,S_W,hreg,newreference(hr2));
                   u32bit,s32bit:
                     emit_reg_ref(A_MOVE, S_L,hreg,newreference(hr2));
                   u64bit,s64bit:
                     begin
                        emit_reg_ref(A_MOVE,S_L,hreg,newreference(hr2));
                        r:=newreference(hr2);
                        inc(r^.offset,4);
                        emit_reg_ref(A_MOVE,S_L,hreg2,r);
                        Ungetregister32(hreg2);
                     end;
                 End;
                 Ungetregister32(hreg);
               End;
           End;
           If (cs_check_range in aktlocalswitches) and
              (dest_para^.left^.resulttype^.deftype = orddef) and
              (not(is_64bitint(dest_para^.left^.resulttype))) and
            {the following has to be changed to 64bit checking, once Val
             returns 64 bit values (unless a special Val function is created
             for that)}
            {no need to rangecheck longints or cardinals on 32bit processors}
               not((porddef(dest_para^.left^.resulttype)^.typ = s32bit) and
                   (porddef(dest_para^.left^.resulttype)^.low = longint($80000000)) and
                   (porddef(dest_para^.left^.resulttype)^.high = $7fffffff)) and
               not((porddef(dest_para^.left^.resulttype)^.typ = u32bit) and
                   (porddef(dest_para^.left^.resulttype)^.low = 0) and
                   (porddef(dest_para^.left^.resulttype)^.high = longint($ffffffff))) then
             Begin
               hp := getcopy(dest_para^.left);
               hp^.location.loc := LOC_REGISTER;
               hp^.location.register := hreg;
              {do not register this temporary def}
               OldRegisterDef := RegisterDef;
               RegisterDef := False;
               Case PordDef(dest_para^.left^.resulttype)^.typ of
                 u8bit,u16bit,u32bit: new(hdef,init(u32bit,0,$ffffffff));
                 s8bit,s16bit,s32bit: new(hdef,init(s32bit,$80000000,$7fffffff));
               end;
               hp^.resulttype := hdef;
               emitrangecheck(hp,dest_para^.left^.resulttype);
               hp^.right := nil;
               Dispose(hp^.resulttype, Done);
               RegisterDef := OldRegisterDef;
               disposetree(hp);
             End;
          {dest_para^.right is already nil}
           disposetree(dest_para);
           UnGetIfTemp(hr);
        end;

      var
         r : preference;
         hp : ptree;
         l : longint;
         ispushed : boolean;
         hregister : tregister;
         otlabel,oflabel{,l1}   : pasmlabel;
         oldpushedparasize : longint;

      begin
      { save & reset pushedparasize }
         oldpushedparasize:=pushedparasize;
         pushedparasize:=0;
         case p^.inlinenumber of
            in_assert_x_y:
              begin
                 { the node should be removed in the firstpass }
                 if not (cs_do_assertion in aktlocalswitches) then
                  internalerror(7123458);
                 otlabel:=truelabel;
                 oflabel:=falselabel;
                 getlabel(truelabel);
                 getlabel(falselabel);
                 secondpass(p^.left^.left);
                 maketojumpbool(p^.left^.left);
                 emitlab(falselabel);
                 { erroraddr }
                 emit_reg_reg(A_MOVE,S_L,frame_pointer,R_SPPUSH);
         { lineno }
                 emit_const_reg(A_MOVE,S_L,aktfilepos.line,R_SPPUSH);
                 { filename string }
                 hp:=genstringconstnode(current_module^.sourcefiles^.get_file_name(aktfilepos.fileindex),st_shortstring);
                 secondpass(hp);
                 if codegenerror then
                  exit;
                 emitpushreferenceaddr(hp^.location.reference);
                 disposetree(hp);
                 { push msg }
                 secondpass(p^.left^.right^.left);
                 emitpushreferenceaddr(p^.left^.right^.left^.location.reference);
                 { call }
                 emitcall('FPC_ASSERT');
                 emitlab(truelabel);
                 truelabel:=otlabel;
                 falselabel:=oflabel;
              end;
            in_lo_word,
            in_hi_word :
              begin
                 secondpass(p^.left);
                 p^.location.loc:=LOC_REGISTER;
                 if p^.left^.location.loc<>LOC_REGISTER then
                   begin
                     if p^.left^.location.loc=LOC_CREGISTER then
                       begin
                          p^.location.register:=getregister32;
                          emit_reg_reg(A_MOVE,S_W,p^.left^.location.register,
                            p^.location.register);
                       end
                     else
                       begin
                          del_reference(p^.left^.location.reference);
                          p^.location.register:=getregister32;
                          emit_ref_reg(A_MOVE,S_W,newreference(p^.left^.location.reference),
                            p^.location.register);
                       end;
                   end
                 else p^.location.register:=p^.left^.location.register;
                 if p^.inlinenumber=in_hi_word then
                   emit_const_reg(A_LSR,S_W,8,p^.location.register);
                 p^.location.register:=p^.location.register;
              end;
            in_sizeof_x,
            in_typeof_x :
              begin
                 { for both cases load vmt }
                 if p^.left^.treetype=typen then
                   begin
                      if p^.inlinenumber=in_typeof_x then
                        p^.location.register:=getaddressreg
                      else
                        p^.location.register:=getregister32;
                      if p^.inlinenumber=in_sizeof_x then
                        emit_sym_ofs_reg(A_MOVE,S_L,
                          newasmsymbol(pobjectdef(p^.left^.resulttype)^.vmt_mangledname),0,
                          p^.location.register)
                      else
                        emit_sym_ofs_reg(A_LEA,S_L,
                          newasmsymbol(pobjectdef(p^.left^.resulttype)^.vmt_mangledname),0,
                          p^.location.register);
                   end
                 else
                   begin
                      secondpass(p^.left);
                      del_reference(p^.left^.location.reference);
                      p^.location.loc:=LOC_REGISTER;
                      if p^.inlinenumber=in_sizeof_x then
                        p^.location.register:=getaddressreg
                      else
                        p^.location.register:=getregister32;
                      { handle self inside a method of a class }
                      if p^.left^.location.loc in [LOC_CREGISTER,LOC_REGISTER] then
                        begin
                          if (p^.left^.resulttype^.deftype=classrefdef) or
                             (po_staticmethod in aktprocsym^.definition^.procoptions)
                             then
                            emit_reg_reg(A_MOVE,S_L,
                              p^.left^.location.register,
                              p^.location.register)
                          else
                          { load VMT pointer }
                          emit_ref_reg(A_MOVE,S_L,
                          new_reference(p^.left^.location.register,pobjectdef(p^.left^.resulttype)^.vmt_offset),
                            p^.location.register);
                        end
                      else if (p^.left^.resulttype^.deftype=objectdef) and
                         pobjectdef(p^.left^.resulttype)^.is_class then
                        begin
                          { deref class }
                          emit_ref_reg(A_MOVE,S_L,
                            newreference(p^.left^.location.reference),
                            p^.location.register);

                          { load VMT pointer }
                          emit_ref_reg(A_MOVE,S_L,
                            new_reference(p^.location.register,pobjectdef(p^.left^.resulttype)^.vmt_offset),
                            p^.location.register);
                        end
                      else
                        begin
                          { load VMT pointer }
                          { not for classrefdefs }
                          if (p^.left^.resulttype^.deftype=objectdef) then
                            inc(p^.left^.location.reference.offset,
                              pobjectdef(p^.left^.resulttype)^.vmt_offset);
                          emit_ref_reg(A_MOVE,S_L,
                          newreference(p^.left^.location.reference),
                            p^.location.register);
                        end;
                      {  in sizeof load size }
                      if p^.inlinenumber=in_sizeof_x then
                        begin
                           new(r);
                           reset_reference(r^);
                           r^.base:=p^.location.register;
                           emit_ref_reg(A_MOVE,S_L,r,
                             p^.location.register);
                        end;
                   end;
              end;
            in_lo_long,
            in_hi_long :
              begin
                 secondpass(p^.left);
                 p^.location.loc:=LOC_REGISTER;
                 if p^.left^.location.loc<>LOC_REGISTER then
                   begin
                      if p^.left^.location.loc=LOC_CREGISTER then
                        begin
                           p^.location.register:=getregister32;
                           emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,
                             p^.location.register);
                        end
                      else
                        begin
                           del_reference(p^.left^.location.reference);
                           p^.location.register:=getregister32;
                           emit_ref_reg(A_MOVE,S_L,newreference(p^.left^.location.reference),
                             p^.location.register);
                        end;
                   end
                 else p^.location.register:=p^.left^.location.register;
                 if p^.inlinenumber=in_hi_long then
                   begin
                     getexplicitregister32(R_D0);
                     emit_const_reg(A_MOVE,S_L,16,R_D0);
                     emit_reg_reg(A_LSR,S_L,R_D0,p^.location.register);
                     ungetregister(R_D0);
                   end;
                 p^.location.register:=p^.location.register;
              end;
            in_lo_qword,
            in_hi_qword:
              begin
                 secondpass(p^.left);
                 p^.location.loc:=LOC_REGISTER;
                 case p^.left^.location.loc of
                    LOC_CREGISTER:
                      begin
                         p^.location.register:=getregister32;
                         if p^.inlinenumber=in_hi_qword then
                           emit_reg_reg(A_MOVE,S_L,p^.left^.location.registerhigh,
                             p^.location.register)
                         else
                           emit_reg_reg(A_MOVE,S_L,p^.left^.location.registerlow,
                             p^.location.register)
                      end;
                    LOC_MEM,LOC_REFERENCE:
                      begin
                         del_reference(p^.left^.location.reference);
                         p^.location.register:=getregister32;
                         r:=newreference(p^.left^.location.reference);
                         if p^.inlinenumber=in_lo_qword then
                           inc(r^.offset,4);
                         emit_ref_reg(A_MOVE,S_L,
                           r,p^.location.register);
                      end;
                    LOC_REGISTER:
                      begin
                         if p^.inlinenumber=in_hi_qword then
                           begin
                              p^.location.register:=p^.left^.location.registerhigh;
                              ungetregister32(p^.left^.location.registerlow);
                           end
                         else
                           begin
                              p^.location.register:=p^.left^.location.registerlow;
                              ungetregister32(p^.left^.location.registerhigh);
                           end;
                      end;
                 end;
              end;
            in_length_string :
              begin
                 secondpass(p^.left);
                 set_location(p^.location,p^.left^.location);
                 { length in ansi strings is at offset -8 }
                 if is_ansistring(p^.left^.resulttype) then
                   dec(p^.location.reference.offset,8)
                 { char is always 1, so make it a constant value }
                 else if is_char(p^.left^.resulttype) then
                   begin
                     clear_location(p^.location);
                     p^.location.loc:=LOC_MEM;
                     p^.location.reference.is_immediate:=true;
                     p^.location.reference.offset:=1;
                   end;
              end;
            in_pred_x,
            in_succ_x:
              begin
                 secondpass(p^.left);
                 if p^.inlinenumber=in_pred_x then
                     asmop:=A_SUB
                 else
                     asmop:=A_ADD;
                 case p^.resulttype^.size of
                   8 : opsize:=S_L;
                   4 : opsize:=S_L;
                   2 : opsize:=S_W;
                   1 : opsize:=S_B;
                 else
                   internalerror(10080);
                 end;
                 p^.location.loc:=LOC_REGISTER;
                 if p^.resulttype^.size=8 then
                   begin
                      if p^.left^.location.loc<>LOC_REGISTER then
                        begin
                           if p^.left^.location.loc=LOC_CREGISTER then
                             begin
                                p^.location.registerlow:=getregister32;
                                p^.location.registerhigh:=getregister32;
                                emit_reg_reg(A_MOVE,opsize,p^.left^.location.registerlow,
                                  p^.location.registerlow);
                                emit_reg_reg(A_MOVE,opsize,p^.left^.location.registerhigh,
                                  p^.location.registerhigh);
                             end
                           else
                             begin
                                del_reference(p^.left^.location.reference);
                                p^.location.registerlow:=getregister32;
                                p^.location.registerhigh:=getregister32;
                                emit_ref_reg(A_MOVE,opsize,newreference(p^.left^.location.reference),
                                  p^.location.registerhigh);
                                r:=newreference(p^.left^.location.reference);
                                inc(r^.offset,4);
                                emit_ref_reg(A_MOVE,opsize,r,
                                  p^.location.registerlow);
                             end;
                        end
                      else
                        begin
                           p^.location.registerhigh:=p^.left^.location.registerhigh;
                           p^.location.registerlow:=p^.left^.location.registerlow;
                        end;
                      getexplicitregister32(R_D0);
                      if p^.inlinenumber=in_succ_x then
                        begin
                           emit_const_reg(A_ADDQ,opsize,1,
                             p^.location.registerlow);
                           emit_reg(A_CLR,S_L,R_D0);
                           emit_reg_reg(A_ADDX,opsize,R_D0,
                             p^.location.registerhigh);
                        end
                      else
                        begin
                           emit_const_reg(A_SUBQ,opsize,1,
                             p^.location.registerlow);
                           emit_reg(A_CLR,S_L,R_D0);
                           emit_reg_reg(A_SUBX,opsize,R_D0,
                             p^.location.registerhigh);
                        end;
                       ungetregister(R_D0);
                   end
                 else
                   begin
                      if p^.left^.location.loc<>LOC_REGISTER then
                        begin
                           { first, we've to release the source location ... }
                           if p^.left^.location.loc in [LOC_MEM,LOC_REFERENCE] then
                             del_reference(p^.left^.location.reference);

                           p^.location.register:=getregister32;
                           if p^.left^.location.loc=LOC_CREGISTER then
                             emit_reg_reg(A_MOVE,opsize,p^.left^.location.register,
                               p^.location.register)
                           else
                           if p^.left^.location.loc=LOC_FLAGS then
                             emit_flag2reg(p^.left^.location.resflags,p^.location.register)
                           else
                             emit_ref_reg(A_MOVE,opsize,newreference(p^.left^.location.reference),
                               p^.location.register);
                        end
                      else p^.location.register:=p^.left^.location.register;
                      emit_const_reg(asmop,opsize,1,p^.location.register);
                   end;
                 emitoverflowcheck(p);
                 emitrangecheck(p,p^.resulttype);
              end;
            in_dec_x,
            in_inc_x :
              begin
              { set defaults }
                addvalue:=1;
                addconstant:=true;
              { load first parameter, must be a reference }
                secondpass(p^.left^.left);
                case p^.left^.left^.resulttype^.deftype of
                  orddef,
                 enumdef : begin
                             case p^.left^.left^.resulttype^.size of
                              1 : opsize:=S_B;
                              2 : opsize:=S_W;
                              4 : opsize:=S_L;
                              8 : opsize:=S_L;
                             end;
                           end;
              pointerdef : begin
                             opsize:=S_L;
                             if porddef(ppointerdef(p^.left^.left^.resulttype)^.pointertype.def)=voiddef then
                              addvalue:=1
                             else
                              addvalue:=ppointerdef(p^.left^.left^.resulttype)^.pointertype.def^.size;
                           end;
                else
                 internalerror(10081);
                end;
              { second argument specified?, must be a s32bit in register }
                if assigned(p^.left^.right) then
                 begin
                   ispushed:=maybe_push(p^.left^.right^.left^.registers32,p^.left^.left,false);
                   secondpass(p^.left^.right^.left);
                   if ispushed then
                     restore(p^.left^.left,false);
                 { when constant, just multiply the addvalue }
                   if is_constintnode(p^.left^.right^.left) then
                    addvalue:=addvalue*get_ordinal_value(p^.left^.right^.left)
                   else
                    begin
                      case p^.left^.right^.left^.location.loc of
                   LOC_REGISTER,
                  LOC_CREGISTER : hregister:=p^.left^.right^.left^.location.register;
                        LOC_MEM,
                  LOC_REFERENCE : begin
                                    del_reference(p^.left^.right^.left^.location.reference);
                                    hregister:=getregister32;
                                    emit_ref_reg(A_MOVE,S_L,
                                      newreference(p^.left^.right^.left^.location.reference),hregister);
                                  end;
                       else
                        internalerror(10082);
                       end;
                    { insert multiply with addvalue if its >1 }
                      if addvalue>1 then
                        emit_const_reg(A_MULS,opsize,addvalue,hregister);
                      addconstant:=false;
                    end;
                 end;
              { write the add instruction }
                if addconstant then
                 begin
                    if p^.left^.left^.location.loc=LOC_CREGISTER then
                       emit_const_reg(addsubop[p^.inlinenumber],opsize,
                         addvalue,p^.left^.left^.location.register)
                     else
                       emit_const_ref(addsubop[p^.inlinenumber],opsize,
                         addvalue,newreference(p^.left^.left^.location.reference));
                 end
                else
                 begin
                    if p^.left^.left^.location.loc=LOC_CREGISTER then
                      emit_reg_reg(addsubop[p^.inlinenumber],opsize,
                        hregister,p^.left^.left^.location.register)
                    else
                      emit_reg_ref(addsubop[p^.inlinenumber],opsize,
                        hregister,newreference(p^.left^.left^.location.reference));
                   ungetregister32(hregister);
                 end;
                if p^.left^.left^.resulttype^.size=8 then
                  begin
                    getexplicitregister32(R_D0);
                    emit_reg(A_CLR,S_L,R_D0);
                    if p^.left^.left^.location.loc=LOC_CREGISTER then
                      emit_reg_reg(addsubop[p^.inlinenumber],S_L,R_D0,
                        p^.left^.left^.location.registerhigh)
                    else
                      begin
                        getexplicitregister32(R_D1);
                        r:=newreference(p^.left^.left^.location.reference);
                        inc(r^.offset,4);
                        emit_ref_reg(A_MOVE,S_L,r,R_D1);
                        emit_reg_reg(addsubop[p^.inlinenumber],S_L,R_D0,R_D1);
                        r:=newreference(p^.left^.left^.location.reference);
                        inc(r^.offset,4);
                        emit_reg_ref(A_MOVE,S_L,R_D1,r);
                        ungetregister(R_D1);
                      end;
                    ungetregister(R_D0);
                  end;
                emitoverflowcheck(p^.left^.left);
                emitrangecheck(p^.left^.left,p^.left^.left^.resulttype);
              end;
            in_assigned_x :
              begin
                 secondpass(p^.left^.left);
                 p^.location.loc:=LOC_FLAGS;
                 if (p^.left^.left^.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                   begin
                      emit_reg(A_TST,S_L,p^.left^.left^.location.register);
                      ungetregister32(p^.left^.left^.location.register);
                   end
                 else
                   begin
                      emit_ref(A_TST,S_L,newreference(p^.left^.left^.location.reference));
                      del_reference(p^.left^.left^.location.reference);
                   end;
                 p^.location.resflags:=F_NE;
              end;
             in_reset_typedfile,in_rewrite_typedfile :
               begin
                  saveusedregisters(pushed,ALL_REGISTERS);
                  emit_const_reg(A_MOVE,S_L,pfiledef(p^.left^.resulttype)^.typedfiletype.def^.size,R_SPPUSH);
                  secondpass(p^.left);
                  emitpushreferenceaddr(p^.left^.location.reference);
                  if p^.inlinenumber=in_reset_typedfile then
                    emitcall('FPC_RESET_TYPED')
                  else
                    emitcall('FPC_REWRITE_TYPED');
                  restoreusedregisters(pushed);
               end;
            in_write_x :
              handlereadwrite(false,false);
            in_writeln_x :
              handlereadwrite(false,true);
            in_read_x :
              handlereadwrite(true,false);
            in_readln_x :
              handlereadwrite(true,true);
            in_str_x_string :
              begin
                 handle_str;
                 maybe_loadself;
              end;
            in_val_x :
              Begin
                handle_val;
              End;
            in_include_x_y,
            in_exclude_x_y:
              begin
                 secondpass(p^.left^.left);
                 if p^.left^.right^.left^.treetype=ordconstn then
                   begin
                      { calculate bit position }
                      l:=1 shl (p^.left^.right^.left^.value mod 32);

                      { determine operator }
                      if p^.inlinenumber=in_include_x_y then
                        asmop:=A_OR
                      else
                        begin
                           asmop:=A_AND;
                           l:=not(l);
                        end;
                      if (p^.left^.left^.location.loc=LOC_REFERENCE) then
                        begin
                           inc(p^.left^.left^.location.reference.offset,(p^.left^.right^.left^.value div 32)*4);
                           emit_const_ref(asmop,S_L,
                             l,newreference(p^.left^.left^.location.reference));
                           del_reference(p^.left^.left^.location.reference);
                        end
                      else
                        { LOC_CREGISTER }
                        emit_const_reg(asmop,S_L,
                          l,p^.left^.left^.location.register);
                   end
                 else
                   begin
                      { generate code for the element to set }
                      ispushed:=maybe_push(p^.left^.right^.left^.registers32,p^.left^.left,false);
                      secondpass(p^.left^.right^.left);
                      if ispushed then
                        restore(p^.left^.left,false);
                      { determine asm operator }
                      if p^.inlinenumber=in_include_x_y then
                        asmop:=A_BSET
                      else
                        asmop:=A_BCLR;
                      if psetdef(p^.left^.resulttype)^.settype=smallset then
                        begin
                           if p^.left^.right^.left^.location.loc in [LOC_CREGISTER,LOC_REGISTER] then
                             { we don't need a mod 32 because this is done automatically  }
                             { by the bts instruction. For proper checking we would       }
                             { need a cmp and jmp, but this should be done by the         }
                             { type cast code which does range checking if necessary (FK) }
                             hregister:=p^.left^.right^.left^.location.register
                           else
                             begin
                                hregister:=getexplicitregister32(R_D0);
                                emit_load_loc_reg(p^.left^.right^.left^.location,
                                  p^.left^.right^.left^.resulttype, u32bitdef,hregister);
                             end;

                          if (p^.left^.left^.location.loc=LOC_REFERENCE) then
                            begin
                               { BCLR and BSET only works on 32-bits when destination
                                 is a register.
                               }
                              getexplicitregister32(R_D1);
                              emit_ref_reg(A_MOVE,S_L,newreference(p^.left^.left^.location.reference),R_D1);
                              emit_reg_reg(asmop,S_L,hregister,R_D1);
                              emit_reg_ref(A_MOVE,S_L,R_D1,newreference(p^.left^.left^.location.reference));
                              ungetregister32(R_D1);
                            end
                          else
                            emit_reg_reg(asmop,S_L,hregister,
                              p^.left^.left^.location.register);
                        if hregister = R_D0 then
                          ungetregister32(R_D0);
                        end
                      else
                        begin
                           saveusedregisters(savedregs,ALL_REGISTERS);

                           pushsetelement(p^.left^.right^.left);
                           { normset is allways a ref }
                           emitpushreferenceaddr(p^.left^.left^.location.reference);
                           if p^.inlinenumber=in_include_x_y then
                             emitcall('FPC_SET_SET_BYTE')
                           else
                             emitcall('FPC_SET_UNSET_BYTE');

                           restoreusedregisters(savedregs);
                        end;
                   end;
              end;
            in_typeinfo_x:
               begin
                  p^.left^.left^.typenodetype^.generate_rtti;
                  p^.location.register:=getregister32;
                  new(r);
                  reset_reference(r^);
                  r^.symbol:=p^.left^.left^.typenodetype^.rtti_label;
                  emit_ref_reg(A_LEA,S_L,r,R_A0);
          emit_reg_reg(A_MOVE,S_L,R_A0,p^.location.register);
               end;
      { all the following routines are currently NOT internal }
      { routines.                                             }
            in_pi:
              begin
          internalerror(9);
              end;
            in_sin_extended,
            in_arctan_extended,
            in_abs_extended,
            in_sqr_extended,
            in_sqrt_extended,
            in_ln_extended,
            in_cos_extended:
              begin
                 if (cs_fp_emulation in aktmoduleswitches) then
                   internalerror(9);
                 secondpass(p^.left);
                 case p^.left^.location.loc of
                    LOC_FPU:
                      p^.location:=p^.left^.location;
                    LOC_REFERENCE,LOC_MEM:
                      begin
                         floatload(pfloatdef(p^.left^.resulttype)^.typ,
                           p^.left^.location.reference,p^.location);
                         del_reference(p^.left^.location.reference);
                      end
                    else
                      internalerror(309991);
                 end;
                 if not(p^.location.fpuregister in [R_FP0..R_FP7]) then
                   internalerror(20021014);
                 case p^.inlinenumber of
                    in_sin_extended:
                      op:=A_FSIN;
                    in_cos_extended:
                      op:=A_FCOS;
                    in_arctan_extended:
                      op:=A_FATAN;
                    in_abs_extended:
                      op:=A_FABS;
                    in_sqr_extended:
                      op:=A_FMUL;
                    in_sqrt_extended:
                      op:=A_FSQRT;
                    in_ln_extended:
                      op:=A_FLOGN;
                    end;
                    emit_reg_reg(op,S_FX,
                      p^.location.fpuregister,p^.location.fpuregister);
                 end;
            else internalerror(9);
         end;
         { reset pushedparasize }
         pushedparasize:=oldpushedparasize;
      end;

end.

{
  $Log: cginl.pas,v $
  Revision 1.1.2.25  2003/01/20 15:13:54  pierre
   * correct self loadnig inside static object methods

  Revision 1.1.2.24  2003/01/20 13:35:59  pierre
   * fix sizeof/typeof in class methods and with class types

  Revision 1.1.2.23  2003/01/14 21:27:42  peter
    * fixed tw2296

  Revision 1.1.2.22  2002/11/15 10:51:00  pierre
   * add char array support in str, fixes bug tbs/tb0392

  Revision 1.1.2.21  2002/11/14 13:45:01  pierre
   * StoreDirectFuncResult was wrong for floats

  Revision 1.1.2.20  2002/10/14 11:10:22  pierre
   * generate extended math functions for m68k if FPC_FPU_INTERNAL is defined

  Revision 1.1.2.19  2002/10/12 10:20:29  carl
  *  comment out Pierres code to avoid crashes!

  Revision 1.1.2.18  2002/10/11 20:41:09  pierre
   * first part of internal fpu functions support

  Revision 1.1.2.17  2002/09/29 14:06:16  carl
    * illegal memory access with wrong size fixed in read call

  Revision 1.1.2.16  2002/09/16 20:04:30  pierre
   * added a missing getexplicitregister(R_D0) for val

  Revision 1.1.2.15  2002/09/13 18:54:05  carl
    * calling conventions related fixes
    * endian fixes with references
    * fixes of invalid emitted opcodes

  Revision 1.1.2.14  2001/09/14 15:37:41  pierre
   * more int64 fixes

  Revision 1.1.2.13  2001/08/04 00:27:43  pierre
   * ADDX and SUBX only support reg;reg args, used for 64bit inc/dec and succ/pred

  Revision 1.1.2.12  2001/08/02 13:58:20  pierre
   * convert all move.l symbol into lea sym, probably just bug in aggas...

  Revision 1.1.2.11  2001/07/30 12:36:43  pierre
   * LSR 16,reg is not allowed, use D0 reg instead for hi_word

  Revision 1.1.2.10  2001/07/25 07:18:48  pierre
   * fix val implementation for 16bit code var

  Revision 1.1.2.9  2001/07/25 02:34:38  carl
  + support emulation mode for VAL

  Revision 1.1.2.8  2001/07/19 07:12:14  pierre
   * fix val code

  Revision 1.1.2.7  2001/07/13 23:52:41  pierre
   * an addressreg is needed for in_sizeof_x internal

  Revision 1.1.2.6  2001/05/26 20:21:24  carl
  - removed some useless construct

  Revision 1.1.2.5  2001/05/09 03:46:37  carl
  + reinstanted floatiing point inlines (incomplete!)
  * bugfix of problem when calling internal set handling routines 0
    registers were not saved

  Revision 1.1.2.4  2001/04/19 11:37:36  carl
  * m68k updates (code generator compiles)

  Revision 1.1.2.3  2001/04/02 02:20:38  carl
  + ported


}

