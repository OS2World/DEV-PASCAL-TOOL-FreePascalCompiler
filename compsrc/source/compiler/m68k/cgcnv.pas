{
    $Id: cgcnv.pas,v 1.1.2.34 2003/01/12 16:11:23 peter Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate m68k assembler for type converting nodes

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
unit cgcnv;
interface

    uses
      tree;

    procedure loadshortstring(p:ptree);
    procedure loadlongstring(p:ptree);
    procedure loadansi2short(source,dest : ptree);

    procedure secondtypeconv(var p : ptree);
    procedure secondas(var p : ptree);
    procedure secondis(var p : ptree);


implementation

   uses
      cobjects,verbose,globtype,globals,systems,
      symconst,symtable,aasm,types,
      hcodegen,temp_gen,pass_2,pass_1,
      cpubase,cpuasm,
      cga,tgen,cgbase;



    procedure push_shortstring_length(p:ptree);
      var
        hightree : ptree;
      begin
        if is_open_string(p^.resulttype) then
         begin
           getsymonlyin(p^.symtable,'high'+pvarsym(p^.symtableentry)^.name);
           hightree:=genloadnode(pvarsym(srsym),p^.symtable);
           firstpass(hightree);
           secondpass(hightree);
           push_value_para(hightree,false,false,0,4);
           disposetree(hightree);
         end
        else
         begin
           push_int(pstringdef(p^.resulttype)^.len);
         end;
      end;


    procedure loadshortstring(p:ptree);
    {
      Load a string, handles stringdef and orddef (char) types
    }
      var
       regs_to_push: tregisterset;
       saved : tpushed;
      begin
         case p^.right^.resulttype^.deftype of
            stringdef:
              begin
                 if (p^.right^.treetype=stringconstn) and
                   (str_length(p^.right)=0) then
                   emit_ref(A_CLR,S_B,newreference(p^.left^.location.reference))
                 else
                   begin
                     regs_to_push := ALL_REGISTERS;
                     remove_non_regvars_from_loc(p^.left^.location,regs_to_push);
                     remove_non_regvars_from_loc(p^.right^.location,regs_to_push);
                     saveusedregisters(saved, regs_to_push);
                     emitpushreferenceaddr(p^.left^.location.reference);
                     del_reference(p^.left^.location.reference);
                     emitpushreferenceaddr(p^.right^.location.reference);
                     del_reference(p^.right^.location.reference);
                     push_shortstring_length(p^.left);
                     emitcall('FPC_SHORTSTR_COPY');
                     maybe_loadself;
                     restoreusedregisters(saved);
                   end;
              end;
              { 1 character string }
            orddef:
              begin
                  { offset 0: length of string }
                  { offset 1: character        }
                 if p^.right^.treetype=ordconstn then
                   begin
                                  emit_const_ref(A_MOVE,S_W,1*256+p^.right^.value,
                              newreference(p^.left^.location.reference));
                   end
                 else
                   begin
                      { not so elegant (goes better with extra register }
                       if (p^.right^.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                         begin
                            emit_mov_reg_reg(S_B,p^.right^.location.register,accumulator);
                            ungetregister32(p^.right^.location.register);
                        end
                      else
                        begin
                           emit_mov_ref_reg(S_B,newreference(p^.right^.location.reference),accumulator);
                           del_reference(p^.right^.location.reference);
                        end;
                      { alignment can cause problems }
                      { add length of string to ref }
                      emit_const_ref(A_MOVE,S_B,1,newreference(p^.left^.location.reference));
                      { temporarily increase offset }
                      Inc(p^.left^.location.reference.offset);
                                          emit_reg_ref(A_MOVE,S_B,accumulator,newreference(p^.left^.location.reference));
                      Dec(p^.left^.location.reference.offset);
                   end;
              end;
         else
           CGMessage(type_e_mismatch);
         end;
      end;

    procedure loadlongstring(p:ptree);
    {
      Load a string, handles stringdef and orddef (char) types
    }
      var
         r : preference;

      begin
         case p^.right^.resulttype^.deftype of
            stringdef:
              begin
                 if (p^.right^.treetype=stringconstn) and
                   (str_length(p^.right)=0) then
                   emit_ref(A_CLR,S_L,newreference(p^.left^.location.reference))
                 else
                   begin
                     emitpushreferenceaddr(p^.left^.location.reference);
                     emitpushreferenceaddr(p^.right^.location.reference);
                     push_shortstring_length(p^.left);
                     emitcall('FPC_LONGSTR_COPY');
                     maybe_loadself;
                   end;
              end;
            orddef:
              begin
                 { offset 0-3: length of string }
                 { offset 4: character        }
                 emit_const_ref(A_MOVE,S_L,1,newreference(p^.left^.location.reference));

                 r:=newreference(p^.left^.location.reference);
                 inc(r^.offset,4);

                 if p^.right^.treetype=ordconstn then
                   emit_const_ref(A_MOVE,S_B,p^.right^.value,r)
                 else
                   begin
                      case p^.right^.location.loc of
                         LOC_REGISTER,LOC_CREGISTER:
                           begin
                              emit_reg_ref(A_MOVE,S_B,p^.right^.location.register,r);
                              ungetregister(p^.right^.location.register);
                           end;
                         LOC_MEM,LOC_REFERENCE:
                           begin
                              emit_mov_ref_reg(S_B,newreference(p^.right^.location.reference),accumulator);
                              emit_reg_ref(A_MOVE,S_B,accumulator,r);
                              del_reference(p^.right^.location.reference);
                           end
                         else
                           internalerror(20799);
                        end;
                   end;
              end;
         else
           CGMessage(type_e_mismatch);
         end;
      end;


    procedure loadansi2short(source,dest : ptree);
      var
         pushed : tpushed;
         regs_to_push: tregisterset;
      begin
         { offset 0-3: length of string }
         { offset 4: character        }
         { Find out which registers have to be pushed (JM) }
         regs_to_push := ALL_REGISTERS;
         remove_non_regvars_from_loc(source^.location,regs_to_push);
         { Push them (JM) }
         saveusedregisters(pushed,regs_to_push);
         case source^.location.loc of
           LOC_REFERENCE,LOC_MEM:
             begin
                { Now release the location and registers (see cgai386.pas: }
                { loadansistring for more info on the order) (JM)          }
                ungetiftemp(source^.location.reference);
                del_reference(source^.location.reference);
                emit_push_mem(source^.location.reference);
             end;
           LOC_REGISTER,LOC_CREGISTER:
             begin
                emit_mov_reg_reg(S_L,source^.location.register,R_SPPUSH);
                { Now release the register (JM) }
                ungetregister32(source^.location.register);
             end;
         end;
         push_shortstring_length(dest);
         emitpushreferenceaddr(dest^.location.reference);
         emitcall('FPC_ANSISTR_TO_SHORTSTR');
         restoreusedregisters(pushed);
         maybe_loadself;
      end;



{*****************************************************************************
                             SecondTypeConv
*****************************************************************************}

    type
      tsecondconvproc = procedure(var pto,pfrom : ptree;convtyp : tconverttype);

    procedure second_int_to_int(var pto,pfrom : ptree;convtyp : tconverttype);
      var
        op      : tasmop;
        opsize    : topsize;
        hregister,
        hregister2 : tregister;
        hlabel, l : pasmlabel;
        isconstant : boolean;

      begin
        isconstant := false;
        { insert range check if not explicit conversion }
        if not(pto^.explizit) then
          emitrangecheck(pfrom,pto^.resulttype);

        { typecasts from void don't require any code }
        if is_void(pfrom^.resulttype) then
         begin
           pto^.location:=pfrom^.location;
           exit;
         end;

        { move loc flags and jump to register }
        case pfrom^.location.loc of
          LOC_FLAGS : begin
                        hregister:=getregister32;
                        emit_flag2reg(pfrom^.location.resflags,hregister);
                        pfrom^.location.loc:=LOC_REGISTER;
                        pfrom^.location.register:=hregister;
                      end;
           LOC_JUMP : begin
                        hregister:=getregister32;
                        getlabel(hlabel);
                        emitlab(truelabel);
                        emit_const_reg(A_MOVE,S_L,1,hregister);
                        emitjmp(C_None,hlabel);
                        emitlab(falselabel);
                        emit_reg(A_CLR,S_L,hregister);
                        emitlab(hlabel);
                        pfrom^.location.loc:=LOC_REGISTER;
                        pfrom^.location.register:=hregister;
                      end;
        end;


        { is the result size smaller ? }
        if pto^.resulttype^.size<pfrom^.resulttype^.size then
          begin
            { only need to set the new size of a register }
            if (pfrom^.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
             begin
               pto^.location.register:=pfrom^.location.register;
               { we can release the upper register }
               if is_64bitint(pfrom^.resulttype) then
                 ungetregister32(pfrom^.location.registerhigh);
             end;
            { On big endian machines, we must ALWAYS load into a }
            { register, from the full size, since reading the    }
            { memory partially will not work at all.             }
            if (pfrom^.location.loc in [LOC_MEM,LOC_REFERENCE]) then
              begin
                { allocate a general purpose register }
                hregister := getregister32;

                if is_64bitint(pfrom^.resulttype) then
                  begin
                    hregister2 := getregister32;
                    { simply load without sign extension }
                    { we really don't need it this time. }
                    emit_ref_reg(A_MOVE,S_L,
                     newreference(pfrom^.location.reference),
                           hregister2);
                    inc(pfrom^.location.reference.offset,4);
                    emit_ref_reg(A_MOVE,S_L,
                      newreference(pfrom^.location.reference),
                           hregister);
                    dec(pfrom^.location.reference.offset,4);
                  end
                else
                 begin
                   { sign extension is not really necessary }
                   { but let's do it anyways.               }
                   emitloadord2reg(pfrom^.location,porddef(pfrom^.resulttype),
                       hregister,false);
                 end;
                del_reference(pfrom^.location.reference);
                { we can do this here as we need no temp inside }
                ungetiftemp(pfrom^.location.reference);
                { result is now in register }
                pto^.location.loc := LOC_REGISTER;
                pto^.location.register := hregister;
                if is_64bitint(pfrom^.resulttype) then
                  begin
                    pto^.location.registerhigh := hregister2;
                  end;
              end;
          end

        { is the result size bigger ? }
        else if pto^.resulttype^.size>pfrom^.resulttype^.size then
          begin
            { remove reference }
            if (pfrom^.location.loc in [LOC_MEM,LOC_REFERENCE]) then
              begin
                del_reference(pfrom^.location.reference);
                { we can do this here as we need no temp inside }
                ungetiftemp(pfrom^.location.reference);
              end;

            { get op and opsize, handle separate for constants, because
              movz doesn't support constant values }
            if (pfrom^.location.loc=LOC_MEM) and (pfrom^.location.reference.is_immediate) then
             begin
               isconstant := true;
               if is_64bitint(pto^.resulttype) then
                 opsize:=S_L
               else
                 opsize:=def_opsize(pto^.resulttype);
             end;
            { load the register we need }
            if pfrom^.location.loc<>LOC_REGISTER then
              hregister:=getregister32
            else
              hregister:=pfrom^.location.register;

            { set the correct register size and location }
            clear_location(pto^.location);
            pto^.location.register:=hregister;
            pto^.location.loc:=LOC_REGISTER;
            { do we need a second register for a 64 bit type ? }
            if is_64bitint(pto^.resulttype) then
              begin
                 hregister2:=getregister32;
                 pto^.location.registerhigh:=hregister2;
              end;
            { insert the assembler code }


            { is this an immediate value? }
            if isconstant then
               emit_const_reg(A_MOVE,opsize,pfrom^.location.reference.offset,
                 pto^.location.register)
            else
            { load into register with possible sign extension }
              begin
                emit_load_loc_reg(pfrom^.location,pfrom^.resulttype,
                  pto^.resulttype,pto^.location.register);
              end;
            { do we need a sign extension for int64? }
            if is_64bitint(pto^.resulttype) then
              begin
                 emit_reg(A_CLR,S_L,hregister2);
                 if (porddef(pto^.resulttype)^.typ=s64bit) and
                   is_signed(pfrom^.resulttype) then
                   begin
                      getlabel(l);
                      emit_const_reg(A_BTST,S_L,31,hregister);
                      emitlabeled(A_BEQ,l);
                      emit_reg(A_NOT,S_L,
                        hregister2);
                      emitlab(l);
                   end;
              end;
          end;
      end;

    procedure second_string_to_string(var pto,pfrom : ptree;convtyp : tconverttype);

      var
         pushed : tpushed;
         regs_to_push: tregisterset;

      begin
         { does anybody know a better solution than this big case statement ? }
         { ok, a proc table would do the job                              }
         case pstringdef(pto^.resulttype)^.string_typ of

            st_shortstring:
              case pstringdef(pfrom^.resulttype)^.string_typ of
                 st_shortstring:
                   begin
                      gettempofsizereference(pto^.resulttype^.size,pto^.location.reference);
                      copyshortstring(pto^.location.reference,pfrom^.location.reference,
                        pstringdef(pto^.resulttype)^.len,false,true);
                      ungetiftemp(pfrom^.location.reference);
                   end;
                 st_longstring:
                   begin
                      {!!!!!!!}
                      internalerror(8888);
                   end;
                 st_ansistring:
                   begin
                      gettempofsizereference(pto^.resulttype^.size,pto^.location.reference);
                      loadansi2short(pfrom,pto);
                   end;
                 st_widestring:
                   begin
                      {!!!!!!!}
                      internalerror(8888);
                   end;
              end;

            st_longstring:
              case pstringdef(pfrom^.resulttype)^.string_typ of
                 st_shortstring:
                   begin
                      {!!!!!!!}
                      internalerror(8888);
                   end;
                 st_ansistring:
                   begin
                      {!!!!!!!}
                      internalerror(8888);
                   end;
                 st_widestring:
                   begin
                      {!!!!!!!}
                      internalerror(8888);
                   end;
              end;

            st_ansistring:
              case pstringdef(pfrom^.resulttype)^.string_typ of
                 st_shortstring:
                   begin
                      clear_location(pto^.location);
                      pto^.location.loc:=LOC_REFERENCE;
                      gettempansistringreference(pto^.location.reference);
                      decrstringref(cansistringdef,pto^.location.reference);
                      { We don't need the source regs anymore (JM) }
                      regs_to_push := ALL_REGISTERS;
                      remove_non_regvars_from_loc(pfrom^.location,regs_to_push);
                      saveusedregisters(pushed,regs_to_push);
                      release_loc(pfrom^.location);
                      emit_push_lea_loc(pfrom^.location,true);
                      emit_push_lea_loc(pto^.location,false);
                      emitcall('FPC_SHORTSTR_TO_ANSISTR');
                      restoreusedregisters(pushed);
                      maybe_loadself;
                   end;
                 st_longstring:
                   begin
                      {!!!!!!!}
                      internalerror(8888);
                   end;
                 st_widestring:
                   begin
                      {!!!!!!!}
                      internalerror(8888);
                   end;
              end;

            st_widestring:
              case pstringdef(pfrom^.resulttype)^.string_typ of
                 st_shortstring:
                   begin
                      {!!!!!!!}
                      internalerror(8888);
                   end;
                 st_longstring:
                   begin
                      {!!!!!!!}
                      internalerror(8888);
                   end;
                 st_ansistring:
                   begin
                      {!!!!!!!}
                      internalerror(8888);
                   end;
                 st_widestring:
                   begin
                      {!!!!!!!}
                      internalerror(8888);
                   end;
              end;
         end;
      end;


    procedure second_cstring_to_pchar(var pto,pfrom : ptree;convtyp : tconverttype);
      var
        hr : preference;
        hreg : tregister;
      begin
         clear_location(pto^.location);
         pto^.location.loc:=LOC_REGISTER;
         case pstringdef(pfrom^.resulttype)^.string_typ of
           st_shortstring :
             begin
               inc(pfrom^.location.reference.offset);
               del_reference(pfrom^.location.reference);
               pto^.location.register:=getaddressreg;
               emit_ref_reg(A_LEA,S_L,newreference(pfrom^.location.reference),
                 pto^.location.register);
             end;
           st_ansistring :
             begin
               if (pfrom^.treetype=stringconstn) and
                  (str_length(pfrom)=0) then
                begin
                  new(hr);
                  reset_reference(hr^);
                  hr^.symbol:=newasmsymbol('FPC_EMPTYCHAR');
                  pto^.location.register:=getaddressreg;
                  emit_ref_reg(A_LEA,S_L,hr,pto^.location.register);
                end
               else
                begin
                  del_reference(pfrom^.location.reference);
                  pto^.location.register:=getaddressreg;
                  emit_ref_reg(A_MOVE,S_L,newreference(pfrom^.location.reference),
                    pto^.location.register);
                end;
             end;
           st_longstring:
             begin
               {!!!!!!!}
               internalerror(8888);
             end;
           st_widestring:
             begin
               {!!!!!!!}
               internalerror(8888);
             end;
         end;
      end;


    procedure second_string_to_chararray(var pto,pfrom : ptree;convtyp : tconverttype);
      var
         pushedregs: tpushed;
         arrsize, strtype: longint;
         regstopush: tregisterset;
      begin
         with parraydef(pto^.resulttype)^ do
          begin
            if highrange<lowrange then
             internalerror(75432653);
            arrsize := highrange-lowrange+1;
          end;

         if (pfrom^.treetype = stringconstn) and
            { pfrom^.length+1 since there's always a terminating #0 character (JM) }
            (pfrom^.length+1 >= arrsize) and
            (pstringdef(pfrom^.resulttype)^.string_typ=st_shortstring) then
           begin
             inc(pto^.location.reference.offset);
             exit;
           end;
         clear_location(pto^.location);
         pto^.location.loc := LOC_REFERENCE;
         gettempofsizereference(arrsize,pto^.location.reference);

         regstopush := ALL_REGISTERS;
         remove_non_regvars_from_loc(pfrom^.location,regstopush);
         saveusedregisters(pushedregs,regstopush);

         emit_push_lea_loc(pto^.location,false);

         case pstringdef(pfrom^.resulttype)^.string_typ of
           st_shortstring :
             begin
               { 0 means shortstring }
               strtype := 0;
               del_reference(pfrom^.location.reference);
               emit_push_lea_loc(pfrom^.location,true);
               ungetiftemp(pfrom^.location.reference);
             end;
           st_ansistring :
             begin
               { 1 means ansistring }
               strtype := 1;
               case pfrom^.location.loc of
                  LOC_CREGISTER,LOC_REGISTER:
                    begin
                      { ungetregister(pfrom^.location.register);
                        done in emit_push_loc }
                      emit_push_loc(pfrom^.location);
                    end;
                  LOC_MEM,LOC_REFERENCE:
                    begin
                      {del_reference(pfrom^.location.reference);
                        done in emit_push_loc }
                      emit_push_loc(pfrom^.location);
                      {ungetiftemp(pfrom^.location.reference);
                        done in emit_push_loc }
                    end;
               end;
             end;
           st_longstring:
             begin
               {!!!!!!!}
               { 2 means longstring, but still needs support in FPC_STR_TO_CHARARRAY,
                 which is in i386.inc and/or generic.inc (JM) }
               strtype := 2;

               internalerror(8888);
             end;
           st_widestring:
             begin
               {!!!!!!!}
               { 3 means widestring, but still needs support in FPC_STR_TO_CHARARRAY,
                 which is in i386.inc and/or generic.inc (JM) }
               strtype := 3;
               internalerror(8888);
             end;
         end;
         push_int(arrsize);
         push_int(strtype);
         emitcall('FPC_STR_TO_CHARARRAY');
         restoreusedregisters(pushedregs);
         maybe_loadself;
      end;


    procedure second_array_to_pointer(var pto,pfrom : ptree;convtyp : tconverttype);
      begin
         del_reference(pfrom^.location.reference);
         clear_location(pto^.location);
         pto^.location.loc:=LOC_REGISTER;
         pto^.location.register:=getaddressreg;
         emit_ref_reg(A_LEA,S_L,newreference(pfrom^.location.reference),pto^.location.register);
      end;


    procedure second_pointer_to_array(var pto,pfrom : ptree;convtyp : tconverttype);
          var
           hreg : tregister;
      begin
        clear_location(pto^.location);
        pto^.location.loc:=LOC_REFERENCE;
        reset_reference(pto^.location.reference);
        case pfrom^.location.loc of
          LOC_REGISTER :
                    begin
                          hreg := getaddressreg;
              { move the pointer in a data register back into }
              { an address register.                          }
              emit_reg_reg(A_MOVE, S_L, pfrom^.left^.location.register,hreg);
              pto^.location.reference.base:=hreg;
              ungetregister32(pfrom^.left^.location.register);
                        end;
          LOC_CREGISTER :
            begin
              pto^.location.reference.base:=getaddressreg;
              emit_reg_reg(A_MOVE,S_L,pfrom^.location.register,pto^.location.reference.base);
            end
         else
            begin
              del_reference(pfrom^.location.reference);
              pto^.location.reference.base:=getaddressreg;
              emit_ref_reg(A_MOVE,S_L,newreference(pfrom^.location.reference),
                pto^.location.reference.base);
            end;
        end;
      end;


    { generates the code for the type conversion from an array of char }
    { to a string                                                       }
    procedure second_chararray_to_string(var pto,pfrom : ptree;convtyp : tconverttype);
      var
         pushed : tpushed;
         regstopush: tregisterset;
         l : longint;
      begin
         { calc the length of the array }
         l:=parraydef(pfrom^.resulttype)^.highrange-parraydef(pfrom^.resulttype)^.lowrange+1;
         { this is a type conversion which copies the data, so we can't }
         { return a reference                                        }
         clear_location(pto^.location);
         pto^.location.loc:=LOC_MEM;
         case pstringdef(pto^.resulttype)^.string_typ of
           st_shortstring :
             begin
               if l>255 then
                begin
                  CGMessage(type_e_mismatch);
                  l:=255;
                end;
               gettempofsizereference(pto^.resulttype^.size,pto^.location.reference);
               { we've also to release the registers ... }
               { Yes, but before pushusedregisters since that one resets unused! }
               { This caused web bug 1073 (JM)                                   }
               regstopush := ALL_REGISTERS;
               remove_non_regvars_from_loc(pfrom^.location,regstopush);
               saveusedregisters(pushed,regstopush);
               if l>=pto^.resulttype^.size then
                 push_int(pto^.resulttype^.size-1)
               else
                 push_int(l);
               { ... here only the temp. location is released }
               emit_push_lea_loc(pfrom^.location,true);
               del_reference(pfrom^.location.reference);
               emitpushreferenceaddr(pto^.location.reference);
               emitcall('FPC_CHARARRAY_TO_SHORTSTR');
               maybe_loadself;
               restoreusedregisters(pushed);
             end;
           st_ansistring :
             begin
               gettempansistringreference(pto^.location.reference);
               decrstringref(cansistringdef,pto^.location.reference);
               regstopush := ALL_REGISTERS;
               remove_non_regvars_from_loc(pfrom^.location,regstopush);
               saveusedregisters(pushed,regstopush);
               push_int(l);
               emitpushreferenceaddr(pfrom^.location.reference);
               release_loc(pfrom^.location);
               emitpushreferenceaddr(pto^.location.reference);
               emitcall('FPC_CHARARRAY_TO_ANSISTR');
               restoreusedregisters(pushed);
               maybe_loadself;
             end;
           st_longstring:
             begin
               {!!!!!!!}
               internalerror(8888);
             end;
           st_widestring:
             begin
               {!!!!!!!}
               internalerror(8888);
             end;
        end;
      end;


    procedure second_char_to_string(var pto,pfrom : ptree;convtyp : tconverttype);
      var
        pushed : tpushed;
      begin
         clear_location(pto^.location);
         pto^.location.loc:=LOC_MEM;
         case pstringdef(pto^.resulttype)^.string_typ of
           st_shortstring :
             begin
               gettempofsizereference(256,pto^.location.reference);
               { call loadstring with correct left and right }
               pto^.right:=pfrom;
               pto^.left:=pto;
               loadshortstring(pto);
               pto^.left:=nil; { reset left tree, which is empty }
               { pto^.right is not disposed for typeconv !! PM }
               disposetree(pto^.right);
               pto^.right:=nil;
             end;
           st_ansistring :
             begin
               gettempansistringreference(pto^.location.reference);
               decrstringref(cansistringdef,pto^.location.reference);
               release_loc(pfrom^.location);
               saveusedregisters(pushed,ALL_REGISTERS);
               {emit_pushw_loc(pfrom^.location);}
               case pfrom^.location.loc of
                 LOC_REGISTER,LOC_CREGISTER :
                   begin
                     if (target_os.stackalignment = 4) then
                       emit_const_reg(A_SUBQ,S_L,2,R_SP);
                     if pfrom^.resulttype^.size=1 then
                       emit_mov_reg_reg(S_B,pfrom^.location.register,R_SPPUSH)
                     else
                       emit_mov_reg_reg(S_W,pfrom^.location.register,R_SPPUSH);
                   end;
                 LOC_MEM,LOC_REFERENCE :
                   emit_push_mem_size(pfrom^.location.reference,pfrom^.resulttype^.size,
                     target_os.stackalignment);
               else
                 internalerror(8889);
               end;
               emitpushreferenceaddr(pto^.location.reference);
               emitcall('FPC_CHAR_TO_ANSISTR');
               restoreusedregisters(pushed);
               maybe_loadself;
             end;
           else
            internalerror(4179);
        end;
      end;

    procedure second_int_to_real(var pto,pfrom : ptree;convtyp : tconverttype);

      var
         r : preference;
         hregister : tregister;
         l1,l2 : pasmlabel;
         is_64bit  : boolean;
         href : treference;

      begin
         getexplicitregister32(accumulator);
         is_64bit:=porddef(pfrom^.resulttype)^.typ in [u64bit,s64bit];
         if not is_64bit then
              { load from register / memory to a 32-bit register }
              { by keeping the sign information, then free the   }
              { register or reference.                           }
              emitloadord2reg(pfrom^.location, porddef(pfrom^.resulttype),
                accumulator, true);
         clear_location(pto^.location);
         pto^.location.loc:=LOC_FPU;
         { now we've got everything in a register or registers if its not
           a 64-bit int operand
         }
         if (cs_fp_emulation in aktmoduleswitches) then
          begin
            pto^.location.fpuregister := getregister32;
            if is_64bit then
              begin
                  emit_pushq_loc(pfrom^.location);
                  emitcall('FPC_INT64_TO_SINGLE');
                  emit_reg_reg(A_MOVE,S_L,accumulator,pto^.location.fpuregister);
              end
            else
            if (pfloatdef(pto^.resulttype)^.typ = s32real) and (not is_64bit) then
              begin
                 emitcall('FPC_LONG_TO_SINGLE');
                 emit_reg_reg(A_MOVE,S_L,accumulator,pto^.location.fpuregister);
              end
            else
                 internalerror(1212);
          end
          else
          begin
             pto^.location.fpuregister := getfloatreg;
             { Is this supported by m68k CPUs ? PM }
             { This is probably wrong for uint64 ... }
             if is_64bit then
               begin
                 if pfloatdef(pto^.resulttype)^.typ = s32real then
                   begin
                     emit_pushq_loc(pfrom^.location);
                     emitcall('FPC_INT64_TO_SINGLE');
                     emit_reg_reg(A_MOVE,S_L,accumulator,pto^.location.fpuregister);
                   end
                 else
                 if pfloatdef(pto^.resulttype)^.typ in [s64real,s80real] then
                   begin
                      { we pass the memory area as a var parameter }
                      { for a DOUBLE                               }
                      gettempofsizereference(8,href);
                      { 2nd paramater : reference of float64 }
                      emitpushreferenceaddr(href);
                      { 1st parameter : a : int64 value }
                      emit_pushq_loc(pfrom^.location);
                      emitcall('FPC_INT64_TO_DOUBLE');
                      emit_ref_reg(A_FMOVE,S_FL,newreference(href),pto^.location.fpuregister);
                      ungetiftemp(href);
                   end
                 else
                    internalerror(1212);
               end
             else { not 64-bit operand }
               emit_reg_reg(A_FMOVE, S_L, accumulator, pto^.location.fpuregister);
          end;
        ungetregister(accumulator);
        { free the temporary space if 64-bit operand was in memory }
        if (pfrom^.location.loc in [LOC_MEM,LOC_REFERENCE]) and (is_64bit) then
           ungetiftemp(pfrom^.location.reference)
        else if (pfrom^.location.loc in [LOC_CREGISTER,LOC_REGISTER]) and (is_64bit) then
          begin
            ungetregister(pfrom^.location.registerhigh);
            ungetregister(pfrom^.location.registerlow);
          end;
      end;


    { fixed data type is no longer supported }
    procedure second_real_to_fix(var pto,pfrom : ptree;convtyp : tconverttype);
      begin
{$ifndef DEBUG}
        CgMessage(parser_f_unsupported_feature);
{$else DEBUG}
        CgMessage(parser_w_unsupported_feature);
        emitcall('FPC_ABSTRACTERROR');
{$endif DEBUG}
      end;

    procedure second_real_to_real(var pto,pfrom : ptree;convtyp : tconverttype);
      begin
         case pfrom^.location.loc of
            LOC_FPU :
              begin
                pto^.location:=pfrom^.location;
                pto^.location.fpuregister := pfrom^.location.fpuregister;
              end;
            LOC_MEM,
            LOC_REFERENCE:
              begin
                             { LOAD FPU value reference into register }
                 floatload(pfloatdef(pfrom^.resulttype)^.typ,
                   pfrom^.location.reference,pto^.location);
                 { we have to free the reference }
                 del_reference(pfrom^.location.reference);
              end;
         end;
         clear_location(pto^.location);
         pto^.location.loc:=LOC_FPU;
      end;


    { fixed is not officialy supported in FPC 1.0.4 }
    procedure second_fix_to_real(var pto,pfrom : ptree;convtyp : tconverttype);
      begin
{$ifndef DEBUG}
        CgMessage(parser_f_unsupported_feature);
{$else DEBUG}
        CgMessage(parser_w_unsupported_feature);
        emitcall('FPC_ABSTRACTERROR');
{$endif DEBUG}
      end;


    procedure second_cord_to_pointer(var pto,pfrom : ptree;convtyp : tconverttype);
      begin
        { this can't happend, because constants are already processed in
          pass 1 }
        internalerror(47423985);
      end;



    { FIXED is no longer supported }
    procedure second_int_to_fix(var pto,pfrom : ptree;convtyp : tconverttype);
      begin
{$ifndef DEBUG}
        CgMessage(parser_f_unsupported_feature);
{$else DEBUG}
        CgMessage(parser_w_unsupported_feature);
        emitcall('FPC_ABSTRACTERROR');
{$endif DEBUG}
      end;


    procedure second_proc_to_procvar(var pto,pfrom : ptree;convtyp : tconverttype);
      begin
        { method pointer ? }
        if assigned(pfrom^.left) then
          begin
             set_location(pto^.location,pfrom^.location);
          end
        else
          begin
             clear_location(pto^.location);
             pto^.location.loc:=LOC_REGISTER;
             del_reference(pfrom^.location.reference);
             pto^.location.register:=getaddressreg;
             emit_ref_reg(A_LEA,S_L,
               newreference(pfrom^.location.reference),pto^.location.register);
          end;
      end;



    procedure second_bool_to_int(var pto,pfrom : ptree;convtyp : tconverttype);
      var
         oldtruelabel,oldfalselabel,hlabel : pasmlabel;
         hregister : tregister;
         newsize,
         opsize : topsize;
         op     : tasmop;
      begin
         oldtruelabel:=truelabel;
         oldfalselabel:=falselabel;
         getlabel(truelabel);
         getlabel(falselabel);
         secondpass(pfrom);
         { byte(boolean) or word(wordbool) or longint(longbool) must
         be accepted for var parameters }
         if (pto^.explizit) and
            (pfrom^.resulttype^.size=pto^.resulttype^.size) and
            (pfrom^.location.loc in [LOC_REFERENCE,LOC_MEM,LOC_CREGISTER]) then
           begin
              set_location(pto^.location,pfrom^.location);
              truelabel:=oldtruelabel;
              falselabel:=oldfalselabel;
              exit;
           end;
         clear_location(pto^.location);
         { node is in register }
         pto^.location.loc:=LOC_REGISTER;
         if pfrom^.location.loc in [LOC_MEM,LOC_REFERENCE] then
           del_reference(pfrom^.location.reference);
         pto^.location.register:=getregister32;
         { this is used for the jump statement }
         case pto^.resulttype^.size of
          1 : begin
                newsize:=S_B;
              end;
          2 : begin
                newsize:=S_W;
              end;
          4 : begin
                newsize:=S_L;
              end;
          8:  begin
                newsize:=S_L;
                pto^.location.registerhigh := getregister32;
                { we cannot clear the high register before the code }
                { is emitted. Lets do it after...                   }
                { except for LOC_JUMP, where it may change the      }
                { instruction flow (CEC)                            }
              end;
         else
          internalerror(10060);
         end;

         case pfrom^.location.loc of
            LOC_MEM,
      LOC_REFERENCE,
       LOC_REGISTER,
      LOC_CREGISTER : begin
                        { load the value, with possible sign extension }
                        emit_load_loc_reg(pfrom^.location,pfrom^.resulttype,
                              pto^.resulttype,pto^.location.register);
                      end;
          LOC_FLAGS : begin
                        emit_flag2reg(pfrom^.location.resflags,pto^.location.register);
                      end;
           LOC_JUMP : begin
                        getlabel(hlabel);
                        emitlab(truelabel);
                        { clear the high register in case of a 64-bit operand }
                        if pto^.resulttype^.size = 8 then
                         begin
                           emit_reg(A_CLR,S_L,pto^.location.registerhigh);
                         end;
                        emit_const_reg(A_MOVE,newsize,1,pto^.location.register);
                        emitjmp(C_None,hlabel);
                        emitlab(falselabel);
                        { clear the high register in case of a 64-bit operand }
                        if pto^.resulttype^.size = 8 then
                         begin
                           emit_reg(A_CLR,S_L,pto^.location.registerhigh);
                         end;
                        emit_reg(A_CLR,newsize,pto^.location.register);
                        emitlab(hlabel);
                      end;
         else
           internalerror(10061);
         end;
         { clear the high register in case of a 64-bit operand }
         { special LOC_JUMP case handled above.                }
         if (pto^.resulttype^.size = 8) and (pfrom^.location.loc <> LOC_JUMP) then
            emit_reg(A_CLR,S_L,pto^.location.registerhigh);
         truelabel:=oldtruelabel;
         falselabel:=oldfalselabel;
      end;


    procedure second_int_to_bool(var pto,pfrom : ptree;convtyp : tconverttype);
      var
        hregister : tregister;
        flags     : tresflags;
        opsize    : topsize;
        pref      : preference;
      begin
         clear_location(pto^.location);
         { byte(boolean) or word(wordbool) or longint(longbool) must
         be accepted for var parameters }
         { i just broke the type conversion above, except for bytes,
           otherwise, the code generator would emit wrong code!
           I prefer having a parser error, over invalid generated code
           - carl
         }
         if (pto^.explizit) and
            (pfrom^.resulttype^.size=pto^.resulttype^.size) and
            (pfrom^.resulttype^.size=1) and
            (pfrom^.location.loc in [LOC_REFERENCE,LOC_MEM,LOC_CREGISTER]) then
           begin
              set_location(pto^.location,pfrom^.location);
              exit;
           end;
         pto^.location.loc:=LOC_REGISTER;
         opsize:=def_opsize(pfrom^.resulttype);
         case pfrom^.location.loc of
            LOC_MEM,LOC_REFERENCE :
              begin
                if is_64bitint(pfrom^.resulttype) then
                 begin
                   hregister:=getregister32;
                   emit_ref_reg(A_MOVE,opsize,
                     newreference(pfrom^.location.reference),hregister);
                   pref:=newreference(pfrom^.location.reference);
                   inc(pref^.offset,4);
                   emit_ref_reg(A_OR,opsize,pref,hregister);
                 end
                else
                 begin
                   hregister:=getregister32;
                   emit_ref_reg(A_MOVE,opsize,
                     newreference(pfrom^.location.reference),hregister);
                   emit_reg(A_TST,opsize,hregister);
                 end;
                flags:=F_NE;
              end;
            LOC_FLAGS :
              begin
                hregister:=getregister32;
                flags:=pfrom^.location.resflags;
              end;
            LOC_REGISTER,LOC_CREGISTER :
              begin
                hregister:=pfrom^.location.register;
                if is_64bitint(pfrom^.resulttype) then
                  emit_reg_reg(A_OR,opsize,pfrom^.location.registerhigh,hregister)
                else
                  emit_reg(A_TST,opsize,hregister);
                flags:=F_NE;
              end;
            else
              internalerror(10062);
         end;
         pto^.location.register := hregister;
         emit_flag2reg(flags,pto^.location.register);
      end;

    procedure second_load_smallset(var pto,pfrom : ptree;convtyp : tconverttype);
      var
        href : treference;
        pushedregs : tpushed;
      begin
        href.symbol:=nil;
        saveusedregisters(pushedregs,ALL_REGISTERS);
        gettempofsizereference(32,href);
        emit_push_mem_size(pfrom^.location.reference,4,target_os.stackalignment);
        emitpushreferenceaddr(href);
        emitcall('FPC_SET_LOAD_SMALL');
        maybe_loadself;
        restoreusedregisters(pushedregs);
        clear_location(pto^.location);
        pto^.location.loc:=LOC_MEM;
        pto^.location.reference:=href;
      end;


    procedure second_ansistring_to_pchar(var pto,pfrom : ptree;convtyp : tconverttype);
      var
         l1 : pasmlabel;
         hr : preference;
      begin
         clear_location(pto^.location);
         pto^.location.loc:=LOC_REGISTER;
         getlabel(l1);
         case pfrom^.location.loc of
            LOC_CREGISTER,LOC_REGISTER:
              pto^.location.register:=pfrom^.location.register;
            LOC_MEM,LOC_REFERENCE:
              begin
                del_reference(pfrom^.location.reference);
                pto^.location.register:=getregister32;
                emit_ref_reg(A_MOVE,S_L,newreference(pfrom^.location.reference),
                  pto^.location.register);
              end;
         end;
         { TST cannot be used on address registers }
         if isaddressregister(pto^.location.register) then
            emit_const_reg(A_CMP,S_L,0,pto^.location.register)
         else
            emit_reg(A_TST,S_L,pto^.location.register);
         emitlabeled(A_BNE,l1);
         new(hr);
         reset_reference(hr^);
         hr^.symbol:=newasmsymbol('FPC_EMPTYCHAR');
         emit_ref_reg(A_LEA,S_L,hr,R_A0);
         emit_reg_reg(A_MOVE,S_L,R_A0,pto^.location.register);
         emitlab(l1);
      end;


    procedure second_pchar_to_string(var pto,pfrom : ptree;convtyp : tconverttype);
      var
        pushed : tpushed;
        regs_to_push: tregisterset;
      begin
         case pstringdef(pto^.resulttype)^.string_typ of
           st_shortstring:
             begin
                pto^.location.loc:=LOC_REFERENCE;
                gettempofsizereference(pto^.resulttype^.size,pto^.location.reference);
                regs_to_push := ALL_REGISTERS;
                remove_non_regvars_from_loc(pfrom^.location,regs_to_push);
                saveusedregisters(pushed,regs_to_push);

                case pfrom^.location.loc of
                   LOC_REGISTER,LOC_CREGISTER:
                     begin
                        emit_reg_reg(A_MOVE,S_L,pfrom^.location.register,R_SPPUSH);
                        ungetregister32(pfrom^.location.register);
                     end;
                   LOC_REFERENCE,LOC_MEM:
                     begin
                       { Now release the registers (see cgai386.pas:     }
                       { loadansistring for more info on the order) (JM) }
                        del_reference(pfrom^.location.reference);
                        emit_push_mem(pfrom^.location.reference);
                     end;
                end;
                emitpushreferenceaddr(pto^.location.reference);
                emitcall('FPC_PCHAR_TO_SHORTSTR');
                maybe_loadself;
                restoreusedregisters(pushed);
             end;
           st_ansistring:
             begin
                pto^.location.loc:=LOC_REFERENCE;
                gettempansistringreference(pto^.location.reference);
                decrstringref(cansistringdef,pto^.location.reference);
                { Find out which regs have to be pushed (JM) }
                regs_to_push := ALL_REGISTERS;
                remove_non_regvars_from_loc(pfrom^.location,regs_to_push);
                saveusedregisters(pushed,regs_to_push);
                case pfrom^.location.loc of
                  LOC_REFERENCE,LOC_MEM:
                    begin
                      { Now release the registers (see cgai386.pas:     }
                      { loadansistring for more info on the order) (JM) }
                      del_reference(pfrom^.location.reference);
                      emit_push_mem(pfrom^.location.reference);
                    end;
                  LOC_REGISTER,LOC_CREGISTER:
                    begin
                       { Now release the registers (see cgai386.pas:     }
                       { loadansistring for more info on the order) (JM) }
                      emit_reg_reg(A_MOVE,S_L,pfrom^.location.register,R_SPPUSH);
                      ungetregister32(pfrom^.location.register);
                   end;
                end;
                emitpushreferenceaddr(pto^.location.reference);
                emitcall('FPC_PCHAR_TO_ANSISTR');
                maybe_loadself;
                restoreusedregisters(pushed);
             end;
         else
          begin
            internalerror(12121);
          end;
         end;
      end;


    procedure second_nothing(var pto,pfrom : ptree;convtyp : tconverttype);
      begin
      end;


{****************************************************************************
                             SecondTypeConv
****************************************************************************}

    procedure secondtypeconv(var p : ptree);
      const
         secondconvert : array[tconverttype] of tsecondconvproc = (
           {$ifndef tp}@{$endif}second_nothing, {equal}
           {$ifndef tp}@{$endif}second_nothing, {not_possible}
           {$ifndef tp}@{$endif}second_string_to_string,
           {$ifndef tp}@{$endif}second_char_to_string,
           {$ifndef tp}@{$endif}second_nothing, {char_to_chararray}
           {$ifndef tp}@{$endif}second_pchar_to_string,
           {$ifndef tp}@{$endif}second_nothing, {cchar_to_pchar}
           {$ifndef tp}@{$endif}second_cstring_to_pchar,
           {$ifndef tp}@{$endif}second_ansistring_to_pchar,
           {$ifndef tp}@{$endif}second_string_to_chararray,
           {$ifndef tp}@{$endif}second_chararray_to_string,
           {$ifndef tp}@{$endif}second_array_to_pointer,
           {$ifndef tp}@{$endif}second_pointer_to_array,
           {$ifndef tp}@{$endif}second_int_to_int,
           {$ifndef tp}@{$endif}second_int_to_bool,
           {$ifndef tp}@{$endif}second_bool_to_int, { bool_to_bool }
           {$ifndef tp}@{$endif}second_bool_to_int,
           {$ifndef tp}@{$endif}second_real_to_real,
           {$ifndef tp}@{$endif}second_int_to_real,
           {$ifndef tp}@{$endif}second_int_to_fix,
           {$ifndef tp}@{$endif}second_real_to_fix,
           {$ifndef tp}@{$endif}second_fix_to_real,
           {$ifndef tp}@{$endif}second_proc_to_procvar,
           {$ifndef tp}@{$endif}second_nothing, {arrayconstructor_to_set}
           {$ifndef tp}@{$endif}second_load_smallset,
           {$ifndef tp}@{$endif}second_cord_to_pointer
         );
      begin

         { this isn't good coding, I think tc_bool_2_int, shouldn't be }
         { type conversion (FK)                                 }

         if not(p^.convtyp in [tc_bool_2_int,tc_bool_2_bool]) then
           begin
              secondpass(p^.left);
              set_location(p^.location,p^.left^.location);
              if codegenerror then
               exit;
           end;
         { the second argument only is for maybe_range_checking !}
         secondconvert[p^.convtyp](p,p^.left,p^.convtyp);

      end;


{*****************************************************************************
                             SecondIs
*****************************************************************************}

    procedure secondis(var p : ptree);
      var
         pushed : tpushed;

      begin
         { save all used registers }
         saveusedregisters(pushed,ALL_REGISTERS);
         secondpass(p^.left);
         clear_location(p^.location);
         p^.location.loc:=LOC_FLAGS;
         p^.location.resflags:=F_NE;

         { push instance to check: }
         case p^.left^.location.loc of
            LOC_REGISTER,LOC_CREGISTER:
              begin
                 emit_reg_reg(A_MOVE,
                   S_L,p^.left^.location.register,R_SPPUSH);
                 ungetregister32(p^.left^.location.register);
              end;
            LOC_MEM,LOC_REFERENCE:
              begin
                 emit_ref_reg(A_MOVE,
                   S_L,newreference(p^.left^.location.reference),R_SPPUSH);
                 del_reference(p^.left^.location.reference);
              end;
            else internalerror(100);
         end;

         { generate type checking }
         secondpass(p^.right);
         case p^.right^.location.loc of
            LOC_REGISTER,LOC_CREGISTER:
              begin
                 emit_reg_reg(A_MOVE,
                   S_L,p^.right^.location.register,R_SPPUSH);
                 ungetregister32(p^.right^.location.register);
              end;
            LOC_MEM,LOC_REFERENCE:
              begin
                 emit_ref_reg(A_MOVE,
                   S_L,newreference(p^.right^.location.reference),R_SPPUSH);
                 del_reference(p^.right^.location.reference);
              end;
            else internalerror(100);
         end;
         emitcall('FPC_DO_IS');
         { set flags accordingly }
         { we must pop after, since the result of this node is in }
         { flags, and restoring registers corrupts the ccr on the }
         { m68k.                                                  }
         restoreusedregisters(pushed);
         maybe_loadself;
         emit_reg(A_TST,S_B,accumulator);
      end;


{*****************************************************************************
                             SecondAs
*****************************************************************************}

    procedure secondas(var p : ptree);
      var
         pushed : tpushed;
      begin
         secondpass(p^.left);
         { save all used registers }
         saveusedregisters(pushed,ALL_REGISTERS);

         { push instance to check: }
         case p^.left^.location.loc of
            LOC_REGISTER,LOC_CREGISTER:
              emit_reg_reg(A_MOVE,
                S_L,p^.left^.location.register,R_SPPUSH);
            LOC_MEM,LOC_REFERENCE:
              emit_ref_reg(A_MOVE,
                S_L,newreference(p^.left^.location.reference),R_SPPUSH);
            else internalerror(100);
         end;

         { we doesn't modifiy the left side, we check only the type }
         set_location(p^.location,p^.left^.location);

         { generate type checking }
         secondpass(p^.right);
         case p^.right^.location.loc of
            LOC_REGISTER,LOC_CREGISTER:
              begin
                 emit_reg_reg(A_MOVE,
                   S_L,p^.right^.location.register,R_SPPUSH);
                 ungetregister32(p^.right^.location.register);
              end;
            LOC_MEM,LOC_REFERENCE:
              begin
                 emit_ref_reg(A_MOVE,
                   S_L,newreference(p^.right^.location.reference),R_SPPUSH);
                 del_reference(p^.right^.location.reference);
              end;
            else internalerror(100);
         end;
         emitcall('FPC_DO_AS');
         { restore register, this restores automatically the }
         { result                                           }
         restoreusedregisters(pushed);
         maybe_loadself;
      end;


end.
{
  $Log: cgcnv.pas,v $
  Revision 1.1.2.34  2003/01/12 16:11:23  peter
    * fix typecast from void

  Revision 1.1.2.33  2003/01/11 18:28:16  carl
    * LOC_JUMP and LOC_FLAGS implementation for int_to_int conversion

  Revision 1.1.2.32  2002/10/14 11:10:22  pierre
   * generate extended math functions for m68k if FPC_FPU_INTERNAL is defined

  Revision 1.1.2.31  2002/10/13 15:12:21  carl
     + add support for int64_to_float

  Revision 1.1.2.30  2002/09/26 20:19:12  carl
    * int_to_bool fixes for both i386 and m68k (direct typecasts
      of var parameters to bool, no longer allowed :()..
    * bugfix with a0 usage
    - now 68000 mode will not fail to compile if data elements are greater than 32K, only
      a warning will be emitted

  Revision 1.1.2.29  2002/09/21 20:46:45  carl
    * stackalign = 4 now pushes value directly
    * emit_push_mem-size now used current alignment value

  Revision 1.1.2.28  2002/09/20 06:58:20  pierre
   * unsupported feature changed into warning in debug mode

  Revision 1.1.2.27  2002/09/15 16:41:49  carl
    * alignment fixes for constant dat
    * tabs now correct for m68k
    * many cg fixes (don't remember all)

  Revision 1.1.2.26  2002/09/12 19:52:08  carl
    * (m68k): bugfixes for 64-bit arithmetic, 64-bit shifts and 64-bit division
    * (m68k): bugfix for boolean type conversions
    * (m68k): endian problems fixes when src_Size = dst_Size with a different type

  Revision 1.1.2.25  2002/09/10 19:12:49  carl
    * cg int64 bugfixes related to endian (from cg testsuit)

  Revision 1.1.2.24  2001/11/09 09:56:19  jonas
    * fixed bug where self was not reloaded after converting from a string
      to a chararray

  Revision 1.1.2.23  2001/11/02 23:19:11  jonas
    * fixed web bug 1665 (allow char to chararray type conversion)

  Revision 1.1.2.22  2001/09/30 09:18:03  jonas
    * backported register allocation fixes from main branch

  Revision 1.1.2.21  2001/09/17 13:42:34  pierre
   * fixes to remove multiple register ungetregister

  Revision 1.1.2.20  2001/09/01 01:28:13  carl
  * bugfix of int64 support (BTST is from 0 to 31, not 1..32)

  Revision 1.1.2.19  2001/08/03 11:43:09  pierre
   * changed some pointer problematic conversions

  Revision 1.1.2.18  2001/08/02 23:35:58  pierre
   * remove wrong code for int64 to real

  Revision 1.1.2.17  2001/07/30 12:48:10  pierre
   * use BTST for int64 sign extension

  Revision 1.1.2.16  2001/07/29 21:51:43  carl
  * bugfix for int64 support of boolean conversion

  Revision 1.1.2.15  2001/07/26 03:07:30  carl
  * ansistring empty conversion was never allocating a register

  Revision 1.1.2.14  2001/07/19 09:49:45  pierre
   * fix wrong pushes for loadshortstring function

  Revision 1.1.2.13  2001/07/18 15:34:08  pierre
   * fake support for int64 to real conversion

  Revision 1.1.2.12  2001/07/16 13:28:12  jonas
    * fixed allocation of register before release in second_cstring_to_pchar
      (backported from 1.1 branch)

  Revision 1.1.2.11  2001/07/14 01:59:54  carl
  + explicitly allocate accumulator in second_int_real

  Revision 1.1.2.10  2001/06/07 00:15:09  carl
  * corrected problem with sign verfiication with 64-bit values
  * diff from i386 version

  Revision 1.1.2.9  2001/05/26 20:20:43  carl
  * Corrected problem type conversion of int64 ! (again)

  Revision 1.1.2.8  2001/05/22 01:33:39  carl
  * corrected problem with sign verification of int64 values (and, not cmp should be used!)

  Revision 1.1.2.7  2001/05/18 18:04:52  carl
  * fixed problem when conversion from int to real (wrong register allocated)

  Revision 1.1.2.6  2001/05/17 01:31:10  carl
  * when type conversion is done on ordinal always load into register first (endian problem)

  Revision 1.1.2.5  2001/04/19 11:37:35  carl
  * m68k updates (code generator compiles)

  Revision 1.1.2.4  2001/04/03 03:03:28  carl
  + fixed problems with A_EOR and flags on exit of boolean result routines

  Revision 1.1.2.3  2001/03/27 02:55:36  carl
  + ported!


}

