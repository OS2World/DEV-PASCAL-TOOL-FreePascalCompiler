{
    $Id: t_win32.pas,v 1.1.2.18 2003/01/15 19:35:00 carl Exp $
    Copyright (c) 1998-2000 by Peter Vreman

    This unit implements support import,export,link routines
    for the (i386) Win32 target

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
unit t_win32;

  interface

{$define GenerateStabs}

  uses
{$ifdef GenerateStabs}
    symtable,
{$endif GenerateStabs}
    import,export,link;

  const
     winstackpagesize = 4096;

  type

    pwin32imported_item = ^twin32imported_item;
    twin32imported_item = object(timported_item)
{$ifdef GenerateStabs}
      procsym : pprocsym;
{$endif GenerateStabs}
    end;

    pimportlibwin32=^timportlibwin32;
    timportlibwin32=object(timportlib)
      procedure preparelib(const s:string);virtual;
{$ifdef GenerateStabs}
      procedure procsymimportprocedure(aprocsym : pprocsym; const func,module:string;index:longint;const name:string);virtual;
{$endif GenerateStabs}
      procedure importprocedure(const func,module:string;index:longint;const name:string);virtual;
      procedure importvariable(const varname,module:string;const name:string);virtual;
      procedure generatelib;virtual;
      procedure generatenasmlib;virtual;
      procedure generatesmartlib;virtual;
    end;

    pexportlibwin32=^texportlibwin32;
    texportlibwin32=object(texportlib)
      st : string;
      last_index : longint;
      procedure preparelib(const s:string);virtual;
      procedure exportprocedure(hp : pexported_item);virtual;
      procedure exportvar(hp : pexported_item);virtual;
      procedure generatelib;virtual;
      procedure generatenasmlib;virtual;
    end;

    plinkerwin32=^tlinkerwin32;
    tlinkerwin32=object(tlinker)
    private
       Function  WriteResponseFile(isdll:boolean) : Boolean;
       Function  PostProcessExecutable(const fn:string;isdll:boolean) : Boolean;
    public
       Constructor Init;
       Procedure SetDefaultInfo;virtual;
       function  MakeExecutable:boolean;virtual;
       function  MakeSharedLibrary:boolean;virtual;
    end;


  implementation

    uses
{$ifdef PAVEL_LINKLIB}
{$ifdef Delphi}
      dmisc,
{$else Delphi}
      dos,
{$endif Delphi}
       impdef,
{$endif PAVEL_LINKLIB}
       aasm,files,globtype,globals,cobjects,systems,verbose,
       script,gendef,
       cpubase,cpuasm
{$ifdef GDB}
       ,gdb
{$endif}
       ;

    function DllName(Const Name : string) : string;
      var n : string;
      begin
         n:=Upper(SplitExtension(Name));
         if (n='.DLL') or (n='.DRV') or (n='.EXE') then
           DllName:=Name
         else
           DllName:=Name+target_os.sharedlibext;
      end;


{*****************************************************************************
                             TIMPORTLIBWIN32
*****************************************************************************}

    procedure timportlibwin32.preparelib(const s : string);
      begin
         if not(assigned(importssection)) then
           importssection:=new(paasmoutput,init);
      end;


    procedure timportlibwin32.importprocedure(const func,module : string;index : longint;const name : string);
      var
         hp1 : pimportlist;
         hp2 : pwin32imported_item;
         hs  : string;
      begin
         hs:=DllName(module);
         { search for the module }
         hp1:=pimportlist(current_module^.imports^.first);
         while assigned(hp1) do
           begin
              if hs=hp1^.dllname^ then
                break;
              hp1:=pimportlist(hp1^.next);
           end;
         { generate a new item ? }
         if not(assigned(hp1)) then
           begin
              hp1:=new(pimportlist,init(hs));
              current_module^.imports^.concat(hp1);
           end;
         { search for reuse of old import item }
         hp2:=pwin32imported_item(hp1^.imported_items^.first);
         while assigned(hp2) do
          begin
            if hp2^.func^=func then
             break;
            hp2:=pwin32imported_item(hp2^.next);
          end;
         if not assigned(hp2) then
          begin
            hp2:=new(pwin32imported_item,init(func,name,index));
{$ifdef GenerateStabs}
            hp2^.procsym:=nil;
{$endif GenerateStabs}
            hp1^.imported_items^.concat(hp2);
          end;
      end;

{$ifdef GenerateStabs}
    procedure timportlibwin32.procsymimportprocedure(aprocsym : pprocsym; const func,
       module:string;index:longint;const name:string);
      var
         hp1 : pimportlist;
         hp2 : pwin32imported_item;
         hs  : string;
      begin
         hs:=DllName(module);
         { search for the module }
         hp1:=pimportlist(current_module^.imports^.first);
         while assigned(hp1) do
           begin
              if hs=hp1^.dllname^ then
                break;
              hp1:=pimportlist(hp1^.next);
           end;
         { generate a new item ? }
         if not(assigned(hp1)) then
           begin
              hp1:=new(pimportlist,init(hs));
              current_module^.imports^.concat(hp1);
           end;
         { search for reuse of old import item }
         hp2:=pwin32imported_item(hp1^.imported_items^.first);
         while assigned(hp2) do
          begin
            if hp2^.func^=func then
             break;
            hp2:=pwin32imported_item(hp2^.next);
          end;
         if not assigned(hp2) then
          begin
            hp2:=new(pwin32imported_item,init(func,name,index));
            hp2^.procsym:=aprocsym;
            hp1^.imported_items^.concat(hp2);
          end;
      end;

{$endif GenerateStabs}

    procedure timportlibwin32.importvariable(const varname,module:string;const name:string);
      var
         hp1 : pimportlist;
         hp2 : pwin32imported_item;
         hs  : string;
      begin
         hs:=DllName(module);
         { search for the module }
         hp1:=pimportlist(current_module^.imports^.first);
         while assigned(hp1) do
           begin
              if hs=hp1^.dllname^ then
                break;
              hp1:=pimportlist(hp1^.next);
           end;
         { generate a new item ? }
         if not(assigned(hp1)) then
           begin
              hp1:=new(pimportlist,init(hs));
              current_module^.imports^.concat(hp1);
           end;
         hp2:=new(pwin32imported_item,init_var(varname,name));
         hp1^.imported_items^.concat(hp2);
      end;

    procedure timportlibwin32.generatenasmlib;
      var
         hp1 : pimportlist;
         hp2 : pwin32imported_item;
         p : pchar;
      begin
         importssection^.concat(new(pai_section,init(sec_code)));
         hp1:=pimportlist(current_module^.imports^.first);
         while assigned(hp1) do
           begin
             hp2:=pwin32imported_item(hp1^.imported_items^.first);
             while assigned(hp2) do
               begin
                 if (aktoutputformat=as_i386_tasm) or
                    (aktoutputformat=as_i386_masm) then
                   p:=strpnew(#9+'EXTRN '+hp2^.func^)
                 else
                   p:=strpnew(#9+'EXTERN '+hp2^.func^);
                 importssection^.concat(new(pai_direct,init(p)));
                 p:=strpnew(#9+'import '+hp2^.func^+' '+
                   hp1^.dllname^+' '+hp2^.name^);
                 importssection^.concat(new(pai_direct,init(p)));
                 hp2:=pwin32imported_item(hp2^.next);
               end;
             hp1:=pimportlist(hp1^.next);
           end;
      end;


    procedure timportlibwin32.generatesmartlib;
      var
         hp1 : pimportlist;
         importname : string;
         mangledstring : string;
         suffix : longint;
         hp2 : pwin32imported_item;
         lhead,lname,lcode,
         lidata4,lidata5 : pasmlabel;
         r : preference;
      begin
         if (aktoutputformat<>as_i386_asw) and
            (aktoutputformat<>as_i386_pecoff) then
           begin
             generatenasmlib;
             exit;
           end;
         hp1:=pimportlist(current_module^.imports^.first);
         while assigned(hp1) do
           begin
           { Get labels for the sections }
             getdatalabel(lhead);
             getdatalabel(lname);
             getlabel(lidata4);
             getlabel(lidata5);
           { create header for this importmodule }
             importssection^.concat(new(pai_cut,init_begin));
             importssection^.concat(new(pai_section,init(sec_idata2)));
             importssection^.concat(new(pai_label,init(lhead)));
             { pointer to procedure names }
             importssection^.concat(new(pai_const_symbol,init_rva(lidata4)));
             { two empty entries follow }
             importssection^.concat(new(pai_const,init_32bit(0)));
             importssection^.concat(new(pai_const,init_32bit(0)));
             { pointer to dll name }
             importssection^.concat(new(pai_const_symbol,init_rva(lname)));
             { pointer to fixups }
             importssection^.concat(new(pai_const_symbol,init_rva(lidata5)));
             { first write the name references }
             importssection^.concat(new(pai_section,init(sec_idata4)));
             importssection^.concat(new(pai_const,init_32bit(0)));
             importssection^.concat(new(pai_label,init(lidata4)));
             { then the addresses and create also the indirect jump }
             importssection^.concat(new(pai_section,init(sec_idata5)));
             importssection^.concat(new(pai_const,init_32bit(0)));
             importssection^.concat(new(pai_label,init(lidata5)));

             { create procedures }
             hp2:=pwin32imported_item(hp1^.imported_items^.first);
             while assigned(hp2) do
               begin
                 { insert cuts }
                 importssection^.concat(new(pai_cut,init));
                 { create indirect jump }
                 if not hp2^.is_var then
                  begin
                    getlabel(lcode);
                    new(r);
                    reset_reference(r^);
                    r^.symbol:=lcode;
                    { place jump in codesegment, insert a code section in the
                      importsection to reduce the amount of .s files (PFV) }
                    importssection^.concat(new(pai_section,init(sec_code)));
{$IfDef GDB}
                    if (cs_debuginfo in aktmoduleswitches) then
                     importssection^.concat(new(pai_stab_function_name,init(nil)));
{$EndIf GDB}
                    importssection^.concat(new(pai_symbol,initname_global(hp2^.func^,0)));
                    importssection^.concat(new(paicpu,op_ref(A_JMP,S_NO,r)));
                    importssection^.concat(new(pai_align,init_op(4,$90)));
{$IfDef GDB}
{$ifdef GenerateStabs}
                    if (cs_debuginfo in aktmoduleswitches) and assigned(hp2^.procsym) then
                      begin
                        mangledstring:=hp2^.procsym^.definition^.mangledname;
                        hp2^.procsym^.definition^.setmangledname(hp2^.func^);
                        hp2^.procsym^.concatstabto(importssection);
                        hp2^.procsym^.definition^.setmangledname(mangledstring);
                      end;
{$endif GenerateStabs}
{$EndIf GDB}
                  end;
                 { create head link }
                 importssection^.concat(new(pai_section,init(sec_idata7)));
                 importssection^.concat(new(pai_const_symbol,init_rva(lhead)));
                 { fixup }
                 getlabel(pasmlabel(hp2^.lab));
                 importssection^.concat(new(pai_section,init(sec_idata4)));
                 importssection^.concat(new(pai_const_symbol,init_rva(hp2^.lab)));
                 { add jump field to importsection }
                 importssection^.concat(new(pai_section,init(sec_idata5)));
                 if hp2^.is_var then
                  importssection^.concat(new(pai_symbol,initname_global(hp2^.func^,0)))
                 else
                  importssection^.concat(new(pai_label,init(lcode)));
                 if (cs_debuginfo in aktmoduleswitches) then
                   begin
                     if assigned(hp2^.name) then
                       begin
                         suffix:=0;
                         importname:='__imp_'+hp2^.name^;
                         while getasmsymbol(importname)<>nil do
                           begin
                             inc(suffix);
                             importname:='__imp_'+hp2^.name^+'_'+tostr(suffix);
                           end;
                         importssection^.concat(new(pai_symbol,initname(importname,4)));
                       end
                     else
                       begin
                         importname:='__imp_by_ordinal'+tostr(hp2^.ordnr);
                         suffix:=0;
                         while getasmsymbol(importname)<>nil do
                           begin
                             inc(suffix);
                             importname:='__imp_by_ordinal'+tostr(hp2^.ordnr)+'_'+tostr(suffix);
                           end;
                         importssection^.concat(new(pai_symbol,initname(importname,4)));
                       end;
                   end;
                  if hp2^.name^<>'' then
                    importssection^.concat(new(pai_const_symbol,init_rva(hp2^.lab)))
                  else
                    importssection^.concat(new(pai_const,init_32bit($80000000 or hp2^.ordnr)));
                 { finally the import information }
                 importssection^.concat(new(pai_section,init(sec_idata6)));
                 importssection^.concat(new(pai_label,init(hp2^.lab)));
                 importssection^.concat(new(pai_const,init_16bit(hp2^.ordnr)));
                 importssection^.concat(new(pai_string,init(hp2^.name^+#0)));
                 importssection^.concat(new(pai_align,init_op(2,0)));
                 hp2:=pwin32imported_item(hp2^.next);
               end;

              { write final section }
              importssection^.concat(new(pai_cut,init_end));
              { end of name references }
              importssection^.concat(new(pai_section,init(sec_idata4)));
              importssection^.concat(new(pai_const,init_32bit(0)));
              { end if addresses }
              importssection^.concat(new(pai_section,init(sec_idata5)));
              importssection^.concat(new(pai_const,init_32bit(0)));
              { dllname }
              importssection^.concat(new(pai_section,init(sec_idata7)));
              importssection^.concat(new(pai_label,init(lname)));
              importssection^.concat(new(pai_string,init(hp1^.dllname^+#0)));

              hp1:=pimportlist(hp1^.next);
           end;
       end;


    procedure timportlibwin32.generatelib;
      var
         hp1 : pimportlist;
         hp2 : pwin32imported_item;
         l1,l2,l3,l4 : pasmlabel;
         mangledstring : string;
         importname : string;
         suffix : longint;
         r : preference;
      begin
         if (aktoutputformat<>as_i386_asw) and
            (aktoutputformat<>as_i386_pecoff) then
           begin
             generatenasmlib;
             exit;
           end;
         hp1:=pimportlist(current_module^.imports^.first);
         while assigned(hp1) do
           begin
              { align codesegment for the jumps }
              importssection^.concat(new(pai_section,init(sec_code)));
              importssection^.concat(new(pai_align,init_op(4,$90)));
              { Get labels for the sections }
              getlabel(l1);
              getlabel(l2);
              getlabel(l3);
              importssection^.concat(new(pai_section,init(sec_idata2)));
              { pointer to procedure names }
              importssection^.concat(new(pai_const_symbol,init_rva(l2)));
              { two empty entries follow }
              importssection^.concat(new(pai_const,init_32bit(0)));
              importssection^.concat(new(pai_const,init_32bit(0)));
              { pointer to dll name }
              importssection^.concat(new(pai_const_symbol,init_rva(l1)));
              { pointer to fixups }
              importssection^.concat(new(pai_const_symbol,init_rva(l3)));

              { only create one section for each else it will
                create a lot of idata* }

              { first write the name references }
              importssection^.concat(new(pai_section,init(sec_idata4)));
              importssection^.concat(new(pai_label,init(l2)));

              hp2:=pwin32imported_item(hp1^.imported_items^.first);
              while assigned(hp2) do
                begin
                   getlabel(pasmlabel(hp2^.lab));
                   if hp2^.name^<>'' then
                     importssection^.concat(new(pai_const_symbol,init_rva(hp2^.lab)))
                   else
                     importssection^.concat(new(pai_const,init_32bit($80000000 or hp2^.ordnr)));
                   hp2:=pwin32imported_item(hp2^.next);
                end;
              { finalize the names ... }
              importssection^.concat(new(pai_const,init_32bit(0)));

              { then the addresses and create also the indirect jump }
              importssection^.concat(new(pai_section,init(sec_idata5)));
              importssection^.concat(new(pai_label,init(l3)));
              hp2:=pwin32imported_item(hp1^.imported_items^.first);
              while assigned(hp2) do
                begin
                   if not hp2^.is_var then
                    begin
                      getlabel(l4);
                      { create indirect jump }
                      new(r);
                      reset_reference(r^);
                      r^.symbol:=l4;
                      { place jump in codesegment }
                      importssection^.concat(new(pai_section,init(sec_code)));
{$IfDef GDB}
                      if (cs_debuginfo in aktmoduleswitches) then
                        importssection^.concat(new(pai_stab_function_name,init(nil)));
{$EndIf GDB}
                      importssection^.concat(new(pai_symbol,initname_global(hp2^.func^,0)));
                      importssection^.concat(new(paicpu,op_ref(A_JMP,S_NO,r)));
                      importssection^.concat(new(pai_align,init_op(4,$90)));
{$IfDef GDB}
{$ifdef GenerateStabs}
                    if (cs_debuginfo in aktmoduleswitches) and assigned(hp2^.procsym) then
                      begin
                        mangledstring:=hp2^.procsym^.definition^.mangledname;
                        hp2^.procsym^.definition^.setmangledname(hp2^.func^);
                        hp2^.procsym^.concatstabto(importssection);
                        hp2^.procsym^.definition^.setmangledname(mangledstring);
                      end;
{$endif GenerateStabs}
{$EndIf GDB}
                      { add jump field to importsection }
                      importssection^.concat(new(pai_section,init(sec_idata5)));
                      if (cs_debuginfo in aktmoduleswitches) then
                        begin
                          if assigned(hp2^.name) then
                            begin
                              importname:='__imp_'+hp2^.name^;
                              suffix:=0;
                              while getasmsymbol(importname)<>nil do
                                begin
                                  inc(suffix);
                                  importname:='__imp_'+tostr(hp2^.ordnr)+'_'+tostr(suffix);
                                end;
                              importssection^.concat(new(pai_symbol,initname(importname,4)));
                            end
                          else
                            begin
                              importname:='__imp_by_ordinal'+tostr(hp2^.ordnr);
                              suffix:=0;
                              while getasmsymbol(importname)<>nil do
                                begin
                                  inc(suffix);
                                  importname:='__imp_by_ordinal'+tostr(hp2^.ordnr)+'_'+tostr(suffix);
                                end;
                              importssection^.concat(new(pai_symbol,initname(importname,4)));
                            end;
                        end;
                      importssection^.concat(new(pai_label,init(l4)));
                    end
                   else
                    begin
                      importssection^.concat(new(pai_symbol,initname_global(hp2^.func^,0)));
                    end;
                   importssection^.concat(new(pai_const_symbol,init_rva(hp2^.lab)));
                   hp2:=pwin32imported_item(hp2^.next);
                end;
              { finalize the addresses }
              importssection^.concat(new(pai_const,init_32bit(0)));

              { finally the import information }
              importssection^.concat(new(pai_section,init(sec_idata6)));
              hp2:=pwin32imported_item(hp1^.imported_items^.first);
              while assigned(hp2) do
                begin
                   importssection^.concat(new(pai_label,init(hp2^.lab)));
                   { the ordinal number }
                   importssection^.concat(new(pai_const,init_16bit(hp2^.ordnr)));
                   importssection^.concat(new(pai_string,init(hp2^.name^+#0)));
                   importssection^.concat(new(pai_align,init_op(2,0)));
                   hp2:=pwin32imported_item(hp2^.next);
                end;
              { create import dll name }
              importssection^.concat(new(pai_section,init(sec_idata7)));
              importssection^.concat(new(pai_label,init(l1)));
              importssection^.concat(new(pai_string,init(hp1^.dllname^+#0)));

              hp1:=pimportlist(hp1^.next);
           end;
      end;


{*****************************************************************************
                             TEXPORTLIBWIN32
*****************************************************************************}


    procedure texportlibwin32.preparelib(const s:string);
      begin
         if not(assigned(exportssection)) then
           exportssection:=new(paasmoutput,init);
         last_index:=0;
         getdatalabel(edatalabel);
      end;

    procedure texportlibwin32.exportvar(hp : pexported_item);
      begin
         { same code used !! PM }
         exportprocedure(hp);
      end;


    procedure texportlibwin32.exportprocedure(hp : pexported_item);
      { must be ordered at least for win32 !! }
      var
        hp2 : pexported_item;
      begin
        { first test the index value }
        if (hp^.options and eo_index)<>0 then
          begin
             if (hp^.index<=0) or (hp^.index>$ffff) then
               begin
                 message1(parser_e_export_invalid_index,tostr(hp^.index));
                 exit;
               end;
             if (hp^.index<=last_index) then
               begin
                 message1(parser_e_export_ordinal_double,tostr(hp^.index));
                 { disregard index value }
                 inc(last_index);
                 hp^.index:=last_index;
                 exit;
               end
             else
               begin
                 last_index:=hp^.index;
               end;
          end
        else
          begin
             inc(last_index);
             hp^.index:=last_index;
          end;
        { now place in correct order }
        hp2:=pexported_item(current_module^._exports^.first);
        while assigned(hp2) and
           (hp^.name^>hp2^.name^) do
          hp2:=pexported_item(hp2^.next);
        { insert hp there !! }
        if assigned(hp2) and (hp2^.name^=hp^.name^) then
          begin
             { this is not allowed !! }
             message1(parser_e_export_name_double,hp^.name^);
             exit;
          end;
        if hp2=pexported_item(current_module^._exports^.first) then
          current_module^._exports^.insert(hp)
        else if assigned(hp2) then
          begin
             hp^.next:=hp2;
             hp^.previous:=hp2^.previous;
             if assigned(hp2^.previous) then
               hp2^.previous^.next:=hp;
             hp2^.previous:=hp;
          end
        else
          current_module^._exports^.concat(hp);
      end;


    procedure texportlibwin32.generatelib;
      var
         ordinal_base,ordinal_max,ordinal_min : longint;
         current_index : longint;
         entries,named_entries : longint;
         name_label,dll_name_label,export_address_table : pasmlabel;
         export_name_table_pointers,export_ordinal_table : pasmlabel;
         hp,hp2 : pexported_item;
         tempexport : plinkedlist;
         address_table,name_table_pointers,
         name_table,ordinal_table : paasmoutput;
      begin

         if (aktoutputformat<>as_i386_asw) and
            (aktoutputformat<>as_i386_pecoff) then
           begin
             generatenasmlib;
             exit;
           end;
         hp:=pexported_item(current_module^._exports^.first);
         if not assigned(hp) then
           exit;

         ordinal_max:=0;
         ordinal_min:=$7FFFFFFF;
         entries:=0;
         named_entries:=0;
         getlabel(dll_name_label);
         getlabel(export_address_table);
         getlabel(export_name_table_pointers);
         getlabel(export_ordinal_table);

         { count entries }
         while assigned(hp) do
           begin
              inc(entries);
              if (hp^.index>ordinal_max) then
                ordinal_max:=hp^.index;
              if (hp^.index>0) and (hp^.index<ordinal_min) then
                ordinal_min:=hp^.index;
              if assigned(hp^.name) then
                inc(named_entries);
              hp:=pexported_item(hp^.next);
           end;

         { no support for higher ordinal base yet !! }
         ordinal_base:=1;
         current_index:=ordinal_base;
         { we must also count the holes !! }
         entries:=ordinal_max-ordinal_base+1;

         exportssection^.concat(new(pai_section,init(sec_edata)));
         { create label to reference from main so smartlink will include
           the .edata section }
         exportssection^.concat(new(pai_symbol,init(edatalabel,0)));
         { export flags }
         exportssection^.concat(new(pai_const,init_32bit(0)));
         { date/time stamp }
         exportssection^.concat(new(pai_const,init_32bit(0)));
         { major version }
         exportssection^.concat(new(pai_const,init_16bit(0)));
         { minor version }
         exportssection^.concat(new(pai_const,init_16bit(0)));
         { pointer to dll name }
         exportssection^.concat(new(pai_const_symbol,init_rva(dll_name_label)));
         { ordinal base normally set to 1 }
         exportssection^.concat(new(pai_const,init_32bit(ordinal_base)));
         { number of entries }
         exportssection^.concat(new(pai_const,init_32bit(entries)));
         { number of named entries }
         exportssection^.concat(new(pai_const,init_32bit(named_entries)));
         { address of export address table }
         exportssection^.concat(new(pai_const_symbol,init_rva(export_address_table)));
         { address of name pointer pointers }
         exportssection^.concat(new(pai_const_symbol,init_rva(export_name_table_pointers)));
         { address of ordinal number pointers }
         exportssection^.concat(new(pai_const_symbol,init_rva(export_ordinal_table)));
         { the name }
         exportssection^.concat(new(pai_label,init(dll_name_label)));
         if st='' then
           exportssection^.concat(new(pai_string,init(current_module^.modulename^+target_os.sharedlibext+#0)))
         else
           exportssection^.concat(new(pai_string,init(st+target_os.sharedlibext+#0)));

         {  export address table }
         address_table:=new(paasmoutput,init);
         address_table^.concat(new(pai_align,init_op(4,0)));
         address_table^.concat(new(pai_label,init(export_address_table)));
         name_table_pointers:=new(paasmoutput,init);
         name_table_pointers^.concat(new(pai_align,init_op(4,0)));
         name_table_pointers^.concat(new(pai_label,init(export_name_table_pointers)));
         ordinal_table:=new(paasmoutput,init);
         ordinal_table^.concat(new(pai_align,init_op(4,0)));
         ordinal_table^.concat(new(pai_label,init(export_ordinal_table)));
         name_table:=new(paasmoutput,init);
         name_table^.concat(new(pai_align,init_op(4,0)));
         { write each address }
         hp:=pexported_item(current_module^._exports^.first);
         while assigned(hp) do
           begin
              if (hp^.options and eo_name)<>0 then
                begin
                   getlabel(name_label);
                   name_table_pointers^.concat(new(pai_const_symbol,init_rva(name_label)));
                   ordinal_table^.concat(new(pai_const,init_16bit(hp^.index-ordinal_base)));
                   name_table^.concat(new(pai_align,init_op(2,0)));
                   name_table^.concat(new(pai_label,init(name_label)));
                   name_table^.concat(new(pai_string,init(hp^.name^+#0)));
                end;
              hp:=pexported_item(hp^.next);
           end;
         { order in increasing ordinal values }
         { into tempexport list }
         tempexport:=new(plinkedlist,init);
         hp:=pexported_item(current_module^._exports^.first);
         while assigned(hp) do
           begin
              current_module^._exports^.remove(hp);
              hp2:=pexported_item(tempexport^.first);
              while assigned(hp2) and (hp^.index>hp2^.index) do
                begin
                   hp2:=pexported_item(hp2^.next);
                end;
              if hp2=pexported_item(tempexport^.first) then
                 tempexport^.insert(hp)
              else
                begin
                   if assigned(hp2) then
                     begin
                        hp^.next:=hp2;
                        hp^.previous:=hp2^.previous;
                        hp2^.previous:=hp;
                        if assigned(hp^.previous) then
                          hp^.previous^.next:=hp;
                      end
                    else
                      tempexport^.concat(hp);
                end;
              hp:=pexported_item(current_module^._exports^.first);;
           end;

         { write the export adress table }
         current_index:=ordinal_base;
         hp:=pexported_item(tempexport^.first);
         while assigned(hp) do
           begin
              { fill missing values }
              while current_index<hp^.index do
                begin
                   address_table^.concat(new(pai_const,init_32bit(0)));
                   inc(current_index);
                end;
              address_table^.concat(new(pai_const_symbol,initname_rva(hp^.sym^.mangledname)));
              inc(current_index);
              hp:=pexported_item(hp^.next);
           end;

         exportssection^.concatlist(address_table);
         exportssection^.concatlist(name_table_pointers);
         exportssection^.concatlist(ordinal_table);
         exportssection^.concatlist(name_table);
         dispose(address_table,done);
         dispose(name_table_pointers,done);
         dispose(ordinal_table,done);
         dispose(name_table,done);
         dispose(tempexport,done);
      end;

    procedure texportlibwin32.generatenasmlib;
      var
         hp : pexported_item;
         p : pchar;
      begin
         exportssection^.concat(new(pai_section,init(sec_code)));
         hp:=pexported_item(current_module^._exports^.first);
         while assigned(hp) do
           begin
             p:=strpnew(#9+'export '+hp^.sym^.mangledname+' '+
               hp^.name^+' '+tostr(hp^.index));
             exportssection^.concat(new(pai_direct,init(p)));
             hp:=pexported_item(hp^.next);
           end;
      end;


{****************************************************************************
                              TLINKERWIN32
****************************************************************************}


Constructor TLinkerWin32.Init;
begin
  Inherited Init;
  { allow duplicated libs (PM) }
  SharedLibFiles.doubles:=true;
  StaticLibFiles.doubles:=true;
  If not ForceDeffileForExport then
    UseDeffileForExport:=false;
end;

Procedure TLinkerWin32.SetDefaultInfo;
begin
  with Info do
   begin
     ExeCmd[1]:='ldw $OPT $STRIP $APPTYPE $IMAGEBASE $RELOC -o $EXE $RES';
     DllCmd[1]:='ldw $OPT $STRIP --dll $APPTYPE $IMAGEBASE $RELOC -o $EXE $RES';
     if RelocSection or UseDeffileForExport then
       begin
          { ExeCmd[2]:='dlltool --as $ASBIN --dllname $EXE --output-exp exp.$$$ $RELOC $DEF';
            use short forms to avoid 128 char limitation problem }
          ExeCmd[2]:='dlltool -S $ASBIN -D $EXE -e exp.$$$ $RELOC $DEF';
          ExeCmd[3]:='ldw $OPT $STRIP $APPTYPE $IMAGEBASE -o $EXE $RES exp.$$$';
          { DllCmd[2]:='dlltool --as $ASBIN --dllname $EXE --output-exp exp.$$$ $RELOC $DEF'; }
          DllCmd[2]:='dlltool -S $ASBIN -D $EXE -e exp.$$$ $RELOC $DEF';
          DllCmd[3]:='ldw $OPT $STRIP --dll $APPTYPE $IMAGEBASE -o $EXE $RES exp.$$$';
       end;
   end;
end;

{$ifndef PAVEL_LINKLIB}
Function TLinkerWin32.WriteResponseFile(isdll:boolean) : Boolean;
Var
  linkres  : TLinkRes;
  i        : longint;
{$IFDEF NEWST}
  HPath    : PStringItem;
{$ELSE}
  HPath    : PStringQueueItem;
{$ENDIF NEWST}
  s,s2        : string;
  found,linklibc : boolean;
begin
  WriteResponseFile:=False;

  { Open link.res file }
  LinkRes.Init(outputexedir+Info.ResName);

  { Write path to search libraries }
  HPath:=current_module^.locallibrarysearchpath.First;
  while assigned(HPath) do
   begin
     LinkRes.Add('SEARCH_DIR('+MaybeQuoted(HPath^.Data^)+')');
     HPath:=HPath^.Next;
   end;
  HPath:=LibrarySearchPath.First;
  while assigned(HPath) do
   begin
     LinkRes.Add('SEARCH_DIR('+MaybeQuoted(HPath^.Data^)+')');
     HPath:=HPath^.Next;
   end;

  { add objectfiles, start with prt0 always }
  LinkRes.Add('INPUT(');
  if isdll then
   LinkRes.AddFileName(MaybeQuoted(FindObjectFile('wdllprt0','')))
  else
  if (cs_profile in aktmoduleswitches) then
   LinkRes.AddFileName(MaybeQuoted(FindObjectFile('gprt0','')))
  else
   LinkRes.AddFileName(MaybeQuoted(FindObjectFile('wprt0','')));

  while not ObjectFiles.Empty do
   begin
     s:=ObjectFiles.Get;
     if s<>'' then
      LinkRes.AddFileName(MaybeQuoted(s));
   end;
  LinkRes.Add(')');

  { Write staticlibraries }
  if (not StaticLibFiles.Empty) or (cs_profile in aktmoduleswitches) then
   begin
     LinkRes.Add('GROUP(');
     if (cs_profile in aktmoduleswitches) then
       begin
         LinkRes.Add('-lgcc');
         LinkRes.Add('-lmoldname');
         LinkRes.Add('-lmsvcrt');
         LinkRes.Add('-lgmon');
         LinkRes.Add('-lkernel32');
       end;
     While not StaticLibFiles.Empty do
      begin
        S:=StaticLibFiles.Get;
        LinkRes.AddFileName(MaybeQuoted(s));
      end;
     LinkRes.Add(')');
   end;

  { Write sharedlibraries like -l<lib>, also add the needed dynamic linker
    here to be sure that it gets linked this is needed for glibc2 systems (PFV) }
  if not SharedLibFiles.Empty then
   begin
     linklibc:=false;
     LinkRes.Add('INPUT(');
     While not SharedLibFiles.Empty do
      begin
        S:=SharedLibFiles.Get;
        if pos('.',s)=0 then
          { we never directly link a DLL
            its allways through an import library PM }
          { libraries created by C compilers have .a extensions }
          s2:=s+'.a'{ target_os.sharedlibext }
        else
          s2:=s;
        s2:=FindLibraryFile(s2,'',found);
        if found then
          begin
            LinkRes.Add(s2);
            continue;
          end;
        if pos(target_os.sharedlibprefix,s)=1 then
          s:=copy(s,length(target_os.sharedlibprefix)+1,255);
        if s<>'c' then
         begin
           i:=Pos(target_os.sharedlibext,S);
           if i>0 then
            Delete(S,i,255);
           LinkRes.Add('-l'+s);
         end
        else
         begin
           LinkRes.Add('-l'+s);
           linklibc:=true;
         end;
      end;
     { be sure that libc is the last lib }
     if linklibc then
      LinkRes.Add('-lc');
     LinkRes.Add(')');
   end;
{ Write and Close response }
  linkres.writetodisk;
  linkres.done;

  WriteResponseFile:=True;
end;
{$else PAVEL_LINKLIB}
Function TLinkerWin32.WriteResponseFile(isdll:boolean) : Boolean;
Var
  linkres  : TLinkRes;
  HPath    : {$ifdef NEWST} PStringItem {$else} PStringQueueItem {$endif};
  s,s2        : string;
  success : boolean;
function ExpandName(const s:string):string;
var
  sysdir:string;
procedure GetSysDir;
  begin
   sysdir:=GetEnv('windir');
   if sysdir<>''then
    begin
     if not(sysdir[length(sysdir)]in['\','/'])then
      sysdir:=sysdir+dirsep;
    end;
  end;
function IsFile(d:string;var PathToDll:string):longbool;
  var
   f:file;
   attr:word;
  begin
   PathToDll:='';
   if d<>''then
    if d[length(d)]<>dirsep then
     d:=d+dirsep;
   d:=d+s;
   assign(f,d);
   GetFattr(f,Attr);
   if DOSerror<>0 then
    IsFile:=false
   else
    begin
     if(attr and directory)=0 then
      begin
       IsFile:=true;
       PathToDll:=MaybeQuoted(d);
      end
     else
      IsFile:=false;
    end;
  end;
var
  PathToDll:string;
begin
  if not isFile('',PathToDll)then
   begin
    HPath:=LibrarySearchPath.First;
     while assigned(HPath) do
      begin
       if isFile(MaybeQuoted(HPath^.Data^),PathToDll)then
        break;
       HPath:=HPath^.Next;
      end;
    if PathToDll='' then
     begin
      GetSysDir;
      if not isFile(sysdir,PathToDll)then
       if not isFile(sysdir+'system32',PathToDll)then
        if not isFile(sysdir+'system',PathToDll)then
         begin
          message1(exec_w_libfile_not_found,S2);
          PathToDll:=S2;
         end;
     end;
   end;
  ExpandName:=PathToDll;
end;
function DotPos(const s:string):longint;
var
  i:longint;
begin
  DotPos:=0;
  for i:=length(s)downto 1 do
   begin
    if s[i]in['/','\',':']then
     exit
    else if s[i]='.'then
     begin
      DotPos:=i;
      exit;
     end;
   end;
end;
procedure strip(var s:string);
  var
   d:dirstr;
   n:namestr;
   e:extstr;
  begin
   fsplit(s,d,n,e);
   s:=n;
  end;
function do_makedef(const s:string):longbool;
  begin
   if cs_link_extern in aktglobalswitches then
    do_makedef:=DoExec(FindUtil('fpimpdef'),'-o deffile.$$$ -i '+s,false,false)
   else
    do_makedef:=makedef(s,'deffile.$$$');
  end;
begin
  WriteResponseFile:=False;
  While not SharedLibFiles.Empty do
   begin
     S:=SharedLibFiles.Get;
     if DotPos(s)=0 then
      s2:=s+target_os.sharedlibext
     else
      s2:=s;
     strip(s);
     if not do_makedef(ExpandName(s2))then
      begin
       Message(exec_w_error_while_linking);
       aktglobalswitches:=aktglobalswitches+[cs_link_extern];
      end
     else
      begin
       s:=target_os.staticlibprefix+s+target_os.staticlibext;
       success:=DoExec(FindUtil('dlltool'),'-l '+s+' -D '+s2+' -d deffile.$$$',false,false);
       ObjectFiles.insert(s);
       if not success then
        break;
      end;
   end;

  { Open link.res file }
  LinkRes.Init(outputexedir+Info.ResName);

  { Write path to search libraries }
  HPath:=current_module^.locallibrarysearchpath.First;
  while assigned(HPath) do
   begin
     LinkRes.Add('SEARCH_DIR('+MaybeQuoted(HPath^.Data^)+')');
     HPath:=HPath^.Next;
   end;
  HPath:=LibrarySearchPath.First;
  while assigned(HPath) do
   begin
     LinkRes.Add('SEARCH_DIR('+MaybeQuoted(HPath^.Data^)+')');
     HPath:=HPath^.Next;
   end;

  { add objectfiles, start with prt0 always }
  LinkRes.Add('INPUT(');
  if isdll then
   LinkRes.AddFileName(MaybeQuoted(FindObjectFile('wdllprt0','')))
  else
   LinkRes.AddFileName(MaybeQuoted(FindObjectFile('wprt0','')));
  while not ObjectFiles.Empty do
   begin
     s:=ObjectFiles.Get;
     if s<>'' then
      LinkRes.AddFileName(MaybeQuoted(s));
   end;
  LinkRes.Add(')');

  { Write staticlibraries }
  if not StaticLibFiles.Empty then
   begin
     LinkRes.Add('GROUP(');
     While not StaticLibFiles.Empty do
      begin
        S:=StaticLibFiles.Get;
        LinkRes.AddFileName(MaybeQuoted(s));
      end;
     LinkRes.Add(')');
   end;

{ Write and Close response }
  linkres.writetodisk;
  linkres.done;

  WriteResponseFile:=True;
end;
{$endif PAVEL_LINKLIB}


function TLinkerWin32.MakeExecutable:boolean;
var
  binstr,
  cmdstr  : string;
  success : boolean;
  i       : longint;
  AsBinStr     : string[80];
  StripStr,
  RelocStr,
  AppTypeStr,
  ImageBaseStr : string[40];
begin
  if not(cs_link_extern in aktglobalswitches) then
   Message1(exec_i_linking,current_module^.exefilename^);

{ Create some replacements }
  RelocStr:='';
  AppTypeStr:='';
  ImageBaseStr:='';
  StripStr:='';
  AsBinStr:=FindUtil('asw');
  if RelocSection then
   { Using short form to avoid problems with 128 char limitation under Dos. }
   RelocStr:='-b base.$$$';
  if apptype=at_gui then
   AppTypeStr:='--subsystem windows';
  if assigned(DLLImageBase) then
   ImageBaseStr:='--image-base=0x'+DLLImageBase^;
  if (cs_link_strip in aktglobalswitches) then
   StripStr:='-s';

{ Write used files and libraries }
  WriteResponseFile(false);

{ Call linker }
  success:=false;
  for i:=1 to 3 do
   begin
     SplitBinCmd(Info.ExeCmd[i],binstr,cmdstr);
     if binstr<>'' then
      begin
        Replace(cmdstr,'$EXE',MaybeQuoted(current_module^.exefilename^));
        Replace(cmdstr,'$OPT',Info.ExtraOptions);
        Replace(cmdstr,'$RES',outputexedir+Info.ResName);
        Replace(cmdstr,'$APPTYPE',AppTypeStr);
        Replace(cmdstr,'$ASBIN',MaybeQuoted(AsbinStr));
        Replace(cmdstr,'$RELOC',RelocStr);
        Replace(cmdstr,'$IMAGEBASE',ImageBaseStr);
        Replace(cmdstr,'$STRIP',StripStr);
        if not DefFile.Empty {and UseDefFileForExport} then
          begin
            DefFile.WriteFile;
            Replace(cmdstr,'$DEF','-d '+MaybeQuoted(deffile.fname));
          end
        else
          Replace(cmdstr,'$DEF','');
        success:=DoExec(FindUtil(binstr),cmdstr,(i=1),false);
        if not success then
         break;
      end;
   end;

{ Post process }
  if success then
   success:=PostProcessExecutable(current_module^.exefilename^,false);

{ Remove ReponseFile }
  if (success) and not(cs_link_extern in aktglobalswitches) then
   begin
     RemoveFile(outputexedir+Info.ResName);
     RemoveFile('base.$$$');
     RemoveFile('exp.$$$');
     RemoveFile('deffile.$$$');
   end;

  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;


Function TLinkerWin32.MakeSharedLibrary:boolean;
var
  binstr,
  cmdstr  : string;
  found,
  success : boolean;
  i       : longint;
  AsBinStr     : string[80];
  StripStr,
  RelocStr,
  AppTypeStr,
  ImageBaseStr : string[40];
begin
  MakeSharedLibrary:=false;
  if not(cs_link_extern in aktglobalswitches) then
   Message1(exec_i_linking,current_module^.sharedlibfilename^);

{ Create some replacements }
  RelocStr:='';
  AppTypeStr:='';
  ImageBaseStr:='';
  StripStr:='';
  AsBinStr:=FindExe('asw',found);
  if RelocSection then
   { Using short form to avoid problems with 128 char limitation under Dos. }
   RelocStr:='-b base.$$$';
  if apptype=at_gui then
   AppTypeStr:='--subsystem windows';
  if assigned(DLLImageBase) then
   ImageBaseStr:='--image-base=0x'+DLLImageBase^;
  if (cs_link_strip in aktglobalswitches) then
   StripStr:='-s';

{ Write used files and libraries }
  WriteResponseFile(true);

{ Call linker }
  success:=false;
  for i:=1 to 3 do
   begin
     SplitBinCmd(Info.DllCmd[i],binstr,cmdstr);
     if binstr<>'' then
      begin
        Replace(cmdstr,'$EXE',MaybeQuoted(current_module^.sharedlibfilename^));
        Replace(cmdstr,'$OPT',Info.ExtraOptions);
        Replace(cmdstr,'$RES',outputexedir+Info.ResName);
        Replace(cmdstr,'$APPTYPE',AppTypeStr);
        Replace(cmdstr,'$ASBIN',MaybeQuoted(AsbinStr));
        Replace(cmdstr,'$RELOC',RelocStr);
        Replace(cmdstr,'$IMAGEBASE',ImageBaseStr);
        Replace(cmdstr,'$STRIP',StripStr);
        if not DefFile.Empty {and UseDefFileForExport} then
          begin
            DefFile.WriteFile;
            Replace(cmdstr,'$DEF','-d '+MaybeQuoted(deffile.fname));
          end
        else
          Replace(cmdstr,'$DEF','');
        success:=DoExec(FindUtil(binstr),cmdstr,(i=1),false);
        if not success then
         break;
      end;
   end;

{ Post process }
  if success then
   success:=PostProcessExecutable(current_module^.sharedlibfilename^,true);

{ Remove ReponseFile }
  if (success) and not(cs_link_extern in aktglobalswitches) then
   begin
     RemoveFile(outputexedir+Info.ResName);
     RemoveFile('base.$$$');
     RemoveFile('exp.$$$');
   end;
  MakeSharedLibrary:=success;   { otherwise a recursive call to link method }
end;


function tlinkerwin32.postprocessexecutable(const fn : string;isdll:boolean):boolean;
type
  tdosheader = packed record
     e_magic : word;
     e_cblp : word;
     e_cp : word;
     e_crlc : word;
     e_cparhdr : word;
     e_minalloc : word;
     e_maxalloc : word;
     e_ss : word;
     e_sp : word;
     e_csum : word;
     e_ip : word;
     e_cs : word;
     e_lfarlc : word;
     e_ovno : word;
     e_res : array[0..3] of word;
     e_oemid : word;
     e_oeminfo : word;
     e_res2 : array[0..9] of word;
     e_lfanew : longint;
  end;
  tpeheader = packed record
     PEMagic : array[0..3] of char;
     Machine : word;
     NumberOfSections : word;
     TimeDateStamp : longint;
     PointerToSymbolTable : longint;
     NumberOfSymbols : longint;
     SizeOfOptionalHeader : word;
     Characteristics : word;
     Magic : word;
     MajorLinkerVersion : byte;
     MinorLinkerVersion : byte;
     SizeOfCode : longint;
     SizeOfInitializedData : longint;
     SizeOfUninitializedData : longint;
     AddressOfEntryPoint : longint;
     BaseOfCode : longint;
     BaseOfData : longint;
     ImageBase : longint;
     SectionAlignment : longint;
     FileAlignment : longint;
     MajorOperatingSystemVersion : word;
     MinorOperatingSystemVersion : word;
     MajorImageVersion : word;
     MinorImageVersion : word;
     MajorSubsystemVersion : word;
     MinorSubsystemVersion : word;
     Reserved1 : longint;
     SizeOfImage : longint;
     SizeOfHeaders : longint;
     CheckSum : longint;
     Subsystem : word;
     DllCharacteristics : word;
     SizeOfStackReserve : longint;
     SizeOfStackCommit : longint;
     SizeOfHeapReserve : longint;
     SizeOfHeapCommit : longint;
     LoaderFlags : longint;
     NumberOfRvaAndSizes : longint;
     DataDirectory : array[1..$80] of byte;
  end;
  tcoffsechdr=packed record
    name     : array[0..7] of char;
    vsize    : longint;
    rvaofs   : longint;
    datalen  : longint;
    datapos  : longint;
    relocpos : longint;
    lineno1  : longint;
    nrelocs  : word;
    lineno2  : word;
    flags    : longint;
  end;
  psecfill=^tsecfill;
  tsecfill=record
    fillpos,
    fillsize : longint;
    next : psecfill;
  end;
var
  f : file;
  cmdstr : string;
  dosheader : tdosheader;
  peheader : tpeheader;
  firstsecpos,
  maxfillsize,
  l,peheaderpos : longint;
  coffsec : tcoffsechdr;
  secroot,hsecroot : psecfill;
  zerobuf : pointer;
begin
  postprocessexecutable:=false;
  { when -s is used or it's a dll then quit }
  if (cs_link_extern in aktglobalswitches) then
   begin
     if apptype=at_gui then
       cmdstr:='--subsystem gui'
     else if apptype=at_cui then
       cmdstr:='--subsystem console';
     if dllversion<>'' then
       cmdstr:=cmdstr+' --version '+dllversion;
     cmdstr:=cmdstr+' --input '+MaybeQuoted(fn);
     cmdstr:=cmdstr+' --stack '+tostr(stacksize);
     DoExec(FindUtil('postw32'),cmdstr,false,false);
     postprocessexecutable:=true;
     exit;
   end;
  { open file }
  assign(f,fn);
  {$I-}
   reset(f,1);
  if ioresult<>0 then
    Message1(execinfo_f_cant_open_executable,fn);
  { read headers }
  blockread(f,dosheader,sizeof(tdosheader));
  peheaderpos:=dosheader.e_lfanew;
  seek(f,peheaderpos);
  blockread(f,peheader,sizeof(tpeheader));
  { write info }
  Message1(execinfo_x_codesize,tostr(peheader.SizeOfCode));
  Message1(execinfo_x_initdatasize,tostr(peheader.SizeOfInitializedData));
  Message1(execinfo_x_uninitdatasize,tostr(peheader.SizeOfUninitializedData));
  { change stack size (PM) }
  { I am not sure that the default value is adequate !! }
  peheader.SizeOfStackReserve:=stacksize;
  { change the header }
  { sub system }
  { gui=2 }
  { cui=3 }
  if apptype=at_gui then
    peheader.Subsystem:=2
  else if apptype=at_cui then
    peheader.Subsystem:=3;
  if dllversion<>'' then
    begin
     peheader.MajorImageVersion:=dllmajor;
     peheader.MinorImageVersion:=dllminor;
    end;
  { reset timestamp }
  peheader.TimeDateStamp:=0;
  { write header back }
  seek(f,peheaderpos);
  blockwrite(f,peheader,sizeof(tpeheader));
  if ioresult<>0 then
    Message1(execinfo_f_cant_process_executable,fn);
  seek(f,peheaderpos);
  blockread(f,peheader,sizeof(tpeheader));
  { write the value after the change }
  Message1(execinfo_x_stackreserve,tostr(peheader.SizeOfStackReserve));
  Message1(execinfo_x_stackcommit,tostr(peheader.SizeOfStackCommit));
  { read section info }
  maxfillsize:=0;
  firstsecpos:=0;
  secroot:=nil;
  for l:=1 to peheader.NumberOfSections do
   begin
     blockread(f,coffsec,sizeof(tcoffsechdr));
     if coffsec.datapos>0 then
      begin
        if secroot=nil then
         firstsecpos:=coffsec.datapos;
        new(hsecroot);
        hsecroot^.fillpos:=coffsec.datapos+coffsec.vsize;
        hsecroot^.fillsize:=coffsec.datalen-coffsec.vsize;
        hsecroot^.next:=secroot;
        secroot:=hsecroot;
        if secroot^.fillsize>maxfillsize then
         maxfillsize:=secroot^.fillsize;
      end;
   end;
  if firstsecpos>0 then
   begin
     l:=firstsecpos-filepos(f);
     if l>maxfillsize then
      maxfillsize:=l;
   end
  else
   l:=0;
  { get zero buffer }
  getmem(zerobuf,maxfillsize);
  fillchar(zerobuf^,maxfillsize,0);
  { zero from sectioninfo until first section }
  blockwrite(f,zerobuf^,l);
  { zero section alignments }
  while assigned(secroot) do
   begin
     seek(f,secroot^.fillpos);
     blockwrite(f,zerobuf^,secroot^.fillsize);
     hsecroot:=secroot;
     secroot:=secroot^.next;
     dispose(hsecroot);
   end;
  freemem(zerobuf,maxfillsize);
  close(f);
  {$I+}
  if ioresult<>0 then;
  postprocessexecutable:=true;
end;

end.
{
  $Log: t_win32.pas,v $
  Revision 1.1.2.18  2003/01/15 19:35:00  carl
    * fix small compilation problems

  Revision 1.1.2.17  2003/01/06 11:46:27  pierre
   * Use FindUtil to look for asw to first check in -FD option directory

  Revision 1.1.2.16  2003/01/06 11:26:57  pierre
   * use MaybeQuoted in PostProcessing

  Revision 1.1.2.15  2003/01/05 11:39:46  peter
    * use maybequoted instead of getshortname

  Revision 1.1.2.14  2002/12/04 19:29:58  carl
    + correct profiling support for Win32

  Revision 1.1.2.13  2002/11/19 00:47:03  pierre
   * GenerateStabs default now

  Revision 1.1.2.12  2002/11/08 10:13:52  pierre
  * remove interface crc changes bug in debug mode

  Revision 1.1.2.11  2002/09/20 06:59:41  pierre
   * try to get cygwin profiling to work

  Revision 1.1.2.10  2002/07/16 14:32:29  pierre
   + try to generate stabs info for external DLL entries

  Revision 1.1.2.9  2001/10/12 16:05:03  peter
    * fixed typos

  Revision 1.1.2.8  2001/10/04 21:47:57  pierre
   * fix problem with multiple import of same procedure

  Revision 1.1.2.7  2001/09/26 16:30:59  pierre
   + add assembler label so that GDB skips trampoline code

  Revision 1.1.2.6  2001/08/07 15:49:12  pierre
   * code for profiling

  Revision 1.1.2.5  2001/06/15 12:38:51  pierre
   * allow generation of executables or DLL with spaces in name for go32v2 and win32 targets

  Revision 1.1.2.4  2001/06/12 21:47:14  pierre
   * fix the smartlinking problem for DLL

  Revision 1.1.2.3  2001/06/06 22:00:34  peter
    * Win32 fixes

  Revision 1.1.2.2  2001/06/03 15:18:40  peter
    * linux sharedlib fixes

  Revision 1.1.2.1  2001/02/26 19:47:40  peter
    * moved target units to targets/ subdir

  Revision 1.1.2.3  2001/02/20 16:52:26  pierre
   + nasm obj type import/export

  Revision 1.1.2.2  2001/01/12 19:20:37  peter
    * fix in pavel_linklib

  Revision 1.1.2.1  2000/12/30 22:57:26  peter
    * export with case provided in exports section

  Revision 1.1  2000/07/13 06:29:57  michael
  + Initial import

  Revision 1.25  2000/07/08 20:43:38  peter
    * findobjectfile gets extra arg with directory where the unit is found
      and the .o should be looked first

  Revision 1.24  2000/06/20 12:44:30  pierre
   * do not create an empty export section

  Revision 1.23  2000/05/23 20:18:25  pierre
    + pavel's code integrated, but onyl inside
      ifdef pavel_linklib !

  Revision 1.22  2000/04/14 11:16:10  pierre
    * partial linklib change
      I could not use Pavel's code because it broke the current way
      linklib is used, which is messy :(
    + add postw32 call if external linking on win32

  Revision 1.21  2000/03/10 09:14:40  pierre
   * dlltool is also needed if we use DefFile

  Revision 1.20  2000/02/28 17:23:57  daniel
  * Current work of symtable integration committed. The symtable can be
    activated by defining 'newst', but doesn't compile yet. Changes in type
    checking and oop are completed. What is left is to write a new
    symtablestack and adapt the parser to use it.

  Revision 1.19  2000/02/24 18:41:39  peter
    * removed warnings/notes

  Revision 1.18  2000/01/12 10:31:45  peter
    * fixed group() writing

  Revision 1.17  2000/01/11 09:52:07  peter
    * fixed placing of .sl directories
    * use -b again for base-file selection
    * fixed group writing for linux with smartlinking

  Revision 1.16  2000/01/09 00:55:51  pierre
    * GROUP of smartlink units put before the C libraries
      to allow for smartlinking code that uses C code.

  Revision 1.15  2000/01/07 01:14:43  peter
    * updated copyright to 2000

  Revision 1.14  2000/01/07 00:10:26  peter
    * --base-file instead of -b as dlltool 2.9.1 doesn't understand it
    * clear timestamp in pe header

  Revision 1.13  1999/12/20 23:23:30  pierre
   + $description $version

  Revision 1.12  1999/12/08 10:40:01  pierre
    + allow use of unit var in exports of DLL for win32
      by using direct export writing by default instead of use of DEFFILE
      that does not allow assembler labels that do not
      start with an underscore.
      Use -WD to force use of Deffile for Win32 DLL

  Revision 1.11  1999/12/06 18:21:04  peter
    * support !ENVVAR for long commandlines
    * win32/go32v2 write short pathnames to link.res so c:\Program Files\ is
      finally supported as installdir.

  Revision 1.10  1999/11/24 11:45:36  pierre
   * $STRIP was missign in DllCmd[1]

  Revision 1.9  1999/11/22 22:20:43  pierre
    * Def file syntax for win32 with index corrected
    * direct output of .edata leads to same indexes
      (index 5 leads to next export being 6 unless otherwise
      specified like for enums)

  Revision 1.8  1999/11/16 23:39:04  peter
    * use outputexedir for link.res location

  Revision 1.7  1999/11/15 15:01:56  pierre
   + Pavel's changes to support reloc section in exes

  Revision 1.6  1999/11/12 11:03:50  peter
    * searchpaths changed to stringqueue object

  Revision 1.5  1999/11/04 10:55:31  peter
    * TSearchPathString for the string type of the searchpaths, which is
      ansistring under FPC/Delphi

  Revision 1.4  1999/11/02 15:06:58  peter
    * import library fixes for win32
    * alignment works again

  Revision 1.3  1999/10/28 10:33:06  pierre
   * Libs can be link serveral times

  Revision 1.2  1999/10/22 14:42:40  peter
    * reset linklibc

  Revision 1.1  1999/10/21 14:29:38  peter
    * redesigned linker object
    + library support for linux (only procedures can be exported)

}
