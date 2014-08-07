{
    $Id: parser.pas,v 1.1.2.7 2002/12/18 16:50:38 pierre Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    This unit does the parsing process

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
  {$E+,N+,D+,F+}
{$endif}
unit parser;

{ Use exception catching so the compiler goes futher after a Stop }
{$ifndef NOUSEEXCEPT}
{$ifdef i386}
  {$define USEEXCEPT}
{$endif}

{$ifdef TP}
  {$ifdef DPMI}
    {$undef USEEXCEPT}
  {$endif}
{$endif}
{$endif ndef NOUSEEXCEPT}

  interface

    procedure preprocess(const filename:string);
    procedure compile(const filename:string;compile_system:boolean);
    procedure initparser;
    procedure doneparser;

  implementation

    uses
      globtype,version,tokens,systems,
      cobjects,globals,verbose,
      symtable,files,aasm,
{$ifndef newcg}
      hcodegen,
{$endif newcg}
      assemble,link,script,gendef,
{$ifdef BrowserLog}
      browlog,
{$endif BrowserLog}
{$ifdef UseExcept}
      tpexcept,compiler,
{$endif UseExcept}
{$ifdef newcg}
      cgobj,
      cgcpu,
      { cgbase must be after hcodegen to use the correct procinfo !!! }
      cgbase,
{$endif newcg}
{$ifdef GDB}
       gdb,
{$endif GDB}
      comphook,tree,scanner,pbase,ptype,psystem,pmodules,cresstr;


    procedure initparser;
      begin
         { ^M means a string or a char, because we don't parse a }
         { type declaration                                      }
         ignore_equal:=false;

         { we didn't parse a object or class declaration }
         { and no function header                        }
         testcurobject:=0;

         { a long time, this was forgotten }
         aktprocsym:=nil;

         current_module:=nil;
         compiled_module:=nil;
         procinfo:=nil;

         loaded_units.init;

         usedunits.init;

         { global switches }
         aktglobalswitches:=initglobalswitches;

         { scanner }
         c:=#0;
         pattern:='';
         orgpattern:='';
         current_scanner:=nil;

         { memory sizes }
         if heapsize=0 then
          heapsize:=target_info.heapsize;
         if maxheapsize=0 then
          maxheapsize:=target_info.maxheapsize;
         if stacksize=0 then
          stacksize:=target_info.stacksize;

         { open assembler response }
         GenerateAsmRes(outputexedir+'ppas');

         { open deffile }
         DefFile.Init(outputexedir+inputfile+target_os.defext);

         { list of generated .o files, so the linker can remove them }
         SmartLinkOFiles.init;
      end;


    procedure doneparser;
      begin

         { if there was an error in the scanner, the scanner is
           still assigned }
         if assigned(current_scanner) then
          begin
            dispose(current_scanner,done);
            current_scanner:=nil;
          end;

         { unload units }
         { this resets current_module to nil }
         loaded_units.done;
         usedunits.done;

         { close ppas,deffile }
         Dispose(asmres,done);
         deffile.done;

         { free list of .o files }
         SmartLinkOFiles.done;
      end;


    procedure default_macros;
      var
        hp : pstring_item;
      begin
      { commandline }
        hp:=pstring_item(initdefines.first);
        while assigned(hp) do
         begin
           def_macro(hp^.str^);
           hp:=pstring_item(hp^.next);
         end;
      { set macros for version checking }
        set_macro('FPC_VERSION',version_nr);
        set_macro('FPC_RELEASE',release_nr);
        set_macro('FPC_PATCH',patch_nr);
      end;


    procedure preprocess(const filename:string);
      var
        i : longint;
      begin
         new(preprocfile,init('pre'));
       { default macros }
         macros:=new(psymtable,init(macrosymtable));
         macros^.name:=stringdup('Conditionals for '+filename);
         default_macros;
       { initialize a module }
         current_module:=new(pmodule,init(filename,false));
         main_module:=current_module;
       { startup scanner, and save in current_module }
         current_scanner:=new(pscannerfile,Init(filename));
         current_module^.scanner:=current_scanner;
       { loop until EOF is found }
         repeat
           current_scanner^.readtoken;
           preprocfile^.AddSpace;
           case token of
             _ID :
               begin
                 preprocfile^.Add(orgpattern);
               end;
             _REALNUMBER,
             _INTCONST :
               preprocfile^.Add(pattern);
             _CSTRING :
               begin
                 i:=0;
                 while (i<length(pattern)) do
                  begin
                    inc(i);
                    if pattern[i]='''' then
                     begin
                       insert('''',pattern,i);
                       inc(i);
                     end;
                  end;
                 preprocfile^.Add(''''+pattern+'''');
               end;
             _CCHAR :
               begin
                 case pattern[1] of
                   #39 :
                     pattern:='''''''';
                   #0..#31,
                   #128..#255 :
                     begin
                       str(ord(pattern[1]),pattern);
                       pattern:='#'+pattern;
                     end;
                   else
                     pattern:=''''+pattern[1]+'''';
                 end;
                 preprocfile^.Add(pattern);
               end;
             _EOF :
               break;
             else
               preprocfile^.Add(tokeninfo^[token].str)
           end;
         until false;
       { free scanner }
         dispose(current_scanner,done);
         current_scanner:=nil;
       { close }
         dispose(preprocfile,done);
      end;


    procedure compile(const filename:string;compile_system:boolean);
      var
       { scanner }
         oldidtoken,
         oldtoken       : ttoken;
         oldtokenpos    : tfileposinfo;
         oldc           : char;
         oldpattern,
         oldorgpattern  : string;
         old_block_type : tblock_type;
         oldcurrent_scanner,prev_scanner,
         scanner : pscannerfile;
       { symtable }
         oldmacros,
         oldrefsymtable,
         olddefaultsymtablestack,
         oldsymtablestack : psymtable;
         oldprocprefix    : string;
         oldaktprocsym    : pprocsym;
         oldoverloaded_operators : toverloaded_operators;
       { cg }
         oldnextlabelnr : longint;
         oldparse_only  : boolean;
       { asmlists }
         oldimports,
         oldexports,
         oldresource,
         oldrttilist,
         oldresourcestringlist,
         oldbsssegment,
         olddatasegment,
         oldcodesegment,
         oldexprasmlist,
         olddebuglist,
         oldwithdebuglist,
         oldconsts     : paasmoutput;
         oldasmsymbollist : pdictionary;
       { resourcestrings }
         OldResourceStrings : PResourceStrings;
       { akt.. things }
         oldaktlocalswitches  : tlocalswitches;
         oldaktmoduleswitches : tmoduleswitches;
         oldaktfilepos      : tfileposinfo;
         oldaktpackenum,oldaktmaxfpuregisters : longint;
         oldaktpackrecords  : tpackrecords;
         oldaktoutputformat : tasm;
         oldaktspecificoptprocessor,
         oldaktoptprocessor : tprocessors;
         oldaktasmmode      : tasmmode;
         oldaktmodeswitches : tmodeswitches;
         old_compiled_module : pmodule;
         prev_name          : pstring;
{$ifdef USEEXCEPT}
         recoverpos    : jmp_buf;
         oldrecoverpos : pjmp_buf;
{$endif useexcept}
{$ifdef newcg}
         oldcg         : pcg;
{$endif newcg}
{$ifdef GDB}
         store_dbx : plongint;
{$endif GDB}

      begin
         inc(compile_level);
         prev_name:=stringdup(parser_current_file);
         parser_current_file:=filename;
         old_compiled_module:=compiled_module;
       { save symtable state }
         oldsymtablestack:=symtablestack;
         olddefaultsymtablestack:=defaultsymtablestack;
         oldrefsymtable:=refsymtable;
         oldmacros:=macros;
         oldprocprefix:=procprefix;
         oldaktprocsym:=aktprocsym;
         move(overloaded_operators,oldoverloaded_operators,sizeof(toverloaded_operators));
       { save scanner state }
         oldc:=c;
         oldpattern:=pattern;
         oldorgpattern:=orgpattern;
         oldtoken:=token;
         oldidtoken:=idtoken;
         old_block_type:=block_type;
         oldtokenpos:=tokenpos;
         oldcurrent_scanner:=current_scanner;
       { save cg }
         oldnextlabelnr:=nextlabelnr;
         oldparse_only:=parse_only;
       { save assembler lists }
         olddatasegment:=datasegment;
         oldbsssegment:=bsssegment;
         oldcodesegment:=codesegment;
         olddebuglist:=debuglist;
         oldwithdebuglist:=withdebuglist;
         oldconsts:=consts;
         oldrttilist:=rttilist;
         oldexprasmlist:=exprasmlist;
         oldimports:=importssection;
         oldexports:=exportssection;
         oldresource:=resourcesection;
         oldresourcestringlist:=resourcestringlist;
         oldasmsymbollist:=asmsymbollist;
         OldResourceStrings:=ResourceStrings;
       { save akt... state }
       { handle the postponed case first }
        if localswitcheschanged then
          begin
            aktlocalswitches:=nextaktlocalswitches;
            localswitcheschanged:=false;
          end;
         oldaktlocalswitches:=aktlocalswitches;
         oldaktmoduleswitches:=aktmoduleswitches;
         oldaktpackrecords:=aktpackrecords;
         oldaktpackenum:=aktpackenum;
         oldaktmaxfpuregisters:=aktmaxfpuregisters;
         oldaktoutputformat:=aktoutputformat;
         oldaktoptprocessor:=aktoptprocessor;
         oldaktspecificoptprocessor:=aktspecificoptprocessor;
         oldaktasmmode:=aktasmmode;
         oldaktfilepos:=aktfilepos;
         oldaktmodeswitches:=aktmodeswitches;
{$ifdef newcg}
         oldcg:=cg;
{$endif newcg}
{$ifdef GDB}
         store_dbx:=dbx_counter;
         dbx_counter:=nil;
{$endif GDB}
       { show info }
         Message1(parser_i_compiling,filename);

       { reset symtable }
         symtablestack:=nil;
         defaultsymtablestack:=nil;
         systemunit:=nil;
         refsymtable:=nil;
         aktprocsym:=nil;
         procprefix:='';
         registerdef:=true;
         aktmaxfpuregisters:=-1;
         fillchar(overloaded_operators,sizeof(toverloaded_operators),0);
         { macros }
         macros:=new(psymtable,init(macrosymtable));
         macros^.name:=stringdup('Conditionals for '+filename);
         default_macros;

       { reset the unit or create a new program }
         if assigned(current_module) then
           begin
              {current_module^.reset this is wrong !! }
               scanner:=current_module^.scanner;
               current_module^.reset;
               current_module^.scanner:=scanner;
           end
         else
          begin
            current_module:=new(pmodule,init(filename,false));
            main_module:=current_module;
          end;

         { a unit compiled at command line must be inside the loaded_unit list }
         if (compile_level=1) then
           loaded_units.insert(current_module);

         compiled_module:=current_module;
         current_module^.in_compile:=true;
       { Load current state from the init values }
         aktlocalswitches:=initlocalswitches;
         aktmoduleswitches:=initmoduleswitches;
         aktmodeswitches:=initmodeswitches;
         {$IFDEF Testvarsets}
         aktsetalloc:=initsetalloc;
         {$ENDIF}
         aktpackrecords:=initpackrecords;
         aktpackenum:=initpackenum;
         aktoutputformat:=initoutputformat;
         set_target_asm(aktoutputformat);
         aktoptprocessor:=initoptprocessor;
         aktspecificoptprocessor:=initspecificoptprocessor;
         aktasmmode:=initasmmode;
         { we need this to make the system unit }
         if compile_system then
          aktmoduleswitches:=aktmoduleswitches+[cs_compilesystem];

       { startup scanner, and save in current_module }
         current_scanner:=new(pscannerfile,Init(filename));
         current_scanner^.readtoken;
         prev_scanner:=current_module^.scanner;
         current_module^.scanner:=current_scanner;
         UpdateStatus;
       { init code generator for a new module }
         codegen_newmodule;
{$ifdef newcg}
{$ifdef i386}
         cg:=new(pcg386,init);
{$endif i386}
{$ifdef alpha}
         cg:=new(pcgalpha,init);
{$endif alpha}
{$ifdef powerpc}
         cg:=new(pcgppc,init);
{$endif powerpc}
{$endif newcg}

         { If the compile level > 1 we get a nice "unit expected" error
           message if we are trying to use a program as unit.}
{$ifdef USEEXCEPT}
         if setjmp(recoverpos)=0 then
          begin
            oldrecoverpos:=recoverpospointer;
            recoverpospointer:=@recoverpos;
{$endif USEEXCEPT}

            if (token=_UNIT) or (compile_level>1) then
              begin
                current_module^.is_unit:=true;
                proc_unit;
              end
            else
              proc_program(token=_LIBRARY);
{$ifdef USEEXCEPT}
            recoverpospointer:=oldrecoverpos;
          end
         else
          begin
            recoverpospointer:=oldrecoverpos;
            longjump_used:=true;
          end;
{$endif USEEXCEPT}

       { clear memory }
{$ifdef Splitheap}
         if testsplit then
           begin
           { temp heap should be empty after that !!!}
             codegen_donemodule;
             Releasetempheap;
           end;
{$endif Splitheap}

       { restore old state, close trees, > 0.99.5 has heapblocks, so
         it's the default to release the trees }
         codegen_donemodule;

{$ifdef newcg}
         dispose(cg,done);
{$endif newcg}

       { free ppu }
         if assigned(current_module^.ppufile) then
          begin
            dispose(current_module^.ppufile,done);
            current_module^.ppufile:=nil;
          end;
       { free scanner }
         dispose(current_scanner,done);
         current_scanner:=nil;
       { restore previous scanner !! }
         current_module^.scanner:=prev_scanner;
         if assigned(prev_scanner) then
           prev_scanner^.invalid:=true;

       { free macros }
         {!!! No check for unused macros yet !!! }
         dispose(macros,done);

         if (compile_level>1) then
           begin
{$ifdef newcg}
              cg:=oldcg;
{$endif newcg}
{$ifdef GDB}
              dbx_counter:=store_dbx;
{$endif GDB}
              { restore scanner }
              c:=oldc;
              pattern:=oldpattern;
              orgpattern:=oldorgpattern;
              token:=oldtoken;
              idtoken:=oldidtoken;
              tokenpos:=oldtokenpos;
              block_type:=old_block_type;
              current_scanner:=oldcurrent_scanner;
              { restore cg }
              nextlabelnr:=oldnextlabelnr;
              parse_only:=oldparse_only;
              { restore asmlists }
              exprasmlist:=oldexprasmlist;
              datasegment:=olddatasegment;
              bsssegment:=oldbsssegment;
              codesegment:=oldcodesegment;
              consts:=oldconsts;
              debuglist:=olddebuglist;
              withdebuglist:=oldwithdebuglist;
              importssection:=oldimports;
              exportssection:=oldexports;
              resourcesection:=oldresource;
              rttilist:=oldrttilist;
              resourcestringlist:=oldresourcestringlist;
              asmsymbollist:=oldasmsymbollist;
              ResourceStrings:=OldResourceStrings;
              { restore symtable state }
              refsymtable:=oldrefsymtable;
              symtablestack:=oldsymtablestack;
              defaultsymtablestack:=olddefaultsymtablestack;
              macros:=oldmacros;
              aktprocsym:=oldaktprocsym;
              procprefix:=oldprocprefix;
              move(oldoverloaded_operators,overloaded_operators,sizeof(toverloaded_operators));
              aktlocalswitches:=oldaktlocalswitches;
              aktmoduleswitches:=oldaktmoduleswitches;
              aktpackrecords:=oldaktpackrecords;
              aktpackenum:=oldaktpackenum;
              aktmaxfpuregisters:=oldaktmaxfpuregisters;
              aktoutputformat:=oldaktoutputformat;
              set_target_asm(aktoutputformat);
              aktoptprocessor:=oldaktoptprocessor;
              aktspecificoptprocessor:=oldaktspecificoptprocessor;
              aktasmmode:=oldaktasmmode;
              aktfilepos:=oldaktfilepos;
              aktmodeswitches:=oldaktmodeswitches;
           end;
       { Shut down things when the last file is compiled }
         if (compile_level=1) then
          begin
          { Close script }
            if (not AsmRes^.Empty) then
             begin
               Message1(exec_i_closing_script,AsmRes^.Fn);
               AsmRes^.WriteToDisk;
             end;

{$ifdef USEEXCEPT}
            if not longjump_used then
{$endif USEEXCEPT}
            { do not create browsers on errors !! }
            if status.errorcount=0 then
              begin
{$ifdef BrowserLog}
              { Write Browser Log }
              if (cs_browser_log in aktglobalswitches) and
                 (cs_browser in aktmoduleswitches) then
                 begin
                 if browserlog.elements_to_list^.empty then
                   begin
                   Message1(parser_i_writing_browser_log,browserlog.Fname);
                   WriteBrowserLog;
                   end
                 else
                  browserlog.list_elements;
                 end;
{$endif BrowserLog}

                 { Write Browser Collections }
                 do_extractsymbolinfo;
              end;

         if current_module^.in_second_compile then
           begin
             current_module^.in_second_compile:=false;
             current_module^.in_compile:=true;
           end
         else
           current_module^.in_compile:=false;

          (* Obsolete code aktprocsym
             is disposed by the localsymtable disposal (PM)
          { Free last aktprocsym }
            if assigned(aktprocsym) and (aktprocsym^.owner=nil) then
             begin
               { init parts are not needed in units !! }
               if current_module^.is_unit then
                 aktprocsym^.definition^.forwarddef:=false;
               dispose(aktprocsym,done);
             end; *)
          end;

         dec(compile_level);
         parser_current_file:=prev_name^;
         stringdispose(prev_name);
         compiled_module:=old_compiled_module;
{$ifdef USEEXCEPT}
         if longjump_used then
           longjmp(recoverpospointer^,1);
{$endif USEEXCEPT}
      end;

end.
{
  $Log: parser.pas,v $
  Revision 1.1.2.7  2002/12/18 16:50:38  pierre
   * avoid using current_module after its disposal

  Revision 1.1.2.6  2002/01/29 19:43:56  peter
    * update target_asm according to outputformat

  Revision 1.1.2.5  2001/07/23 06:58:56  pierre
   + AsmRes made pointer to support amiga target

  Revision 1.1.2.4  2001/04/12 17:59:23  peter
    * memleak fixed when include file not found

  Revision 1.1.2.3  2000/11/23 12:51:11  pierre
   + added compiling_current to status record

  Revision 1.1.2.2  2000/10/01 19:46:30  peter
    * reset procinfo that is not reset to nil when an error is encountered

  Revision 1.1.2.1  2000/08/12 15:33:37  peter
    + usedasmsymbollist to check and reset only the used symbols

  Revision 1.1  2000/07/13 06:29:53  michael
  + Initial import

  Revision 1.105  2000/06/01 19:09:57  peter
    * made resourcestrings OOP so it's easier to handle it per module

  Revision 1.104  2000/05/29 10:04:40  pierre
    * New bunch of Gabor changes

  Revision 1.103  2000/05/11 06:52:37  pierre
   * fix localswitch problem if compiling objpas

  Revision 1.102  2000/04/24 12:45:44  peter
    * made overloaded_operators local per unit, but it still doesn't work
      correct

  Revision 1.101  2000/02/18 20:53:15  pierre
    * fixes a stabs problem for functions
    + includes a stabs local var for with statements
      the name is with in lowercase followed by an index
      for nested with.
    + Withdebuglist added because the stabs declarations of local
      var are postponed to end of function.

  Revision 1.100  2000/02/14 20:58:44  marco
   * Basic structures for new sethandling implemented.

  Revision 1.99  2000/02/09 13:22:55  peter
    * log truncated

  Revision 1.98  2000/01/23 21:29:17  florian
    * CMOV support in optimizer (in define USECMOV)
    + start of support of exceptions in constructors

  Revision 1.97  2000/01/11 09:52:06  peter
    * fixed placing of .sl directories
    * use -b again for base-file selection
    * fixed group writing for linux with smartlinking

  Revision 1.96  2000/01/07 01:14:28  peter
    * updated copyright to 2000

  Revision 1.95  2000/01/04 15:15:52  florian
    + added compiler switch $maxfpuregisters
    + fixed a small problem in secondvecn

  Revision 1.94  1999/12/02 17:34:34  peter
    * preprocessor support. But it fails on the caret in type blocks

  Revision 1.93  1999/11/24 11:41:03  pierre
   * defaultsymtablestack is now restored after parser.compile

  Revision 1.92  1999/11/18 15:34:46  pierre
    * Notes/Hints for local syms changed to
      Set_varstate function

  Revision 1.91  1999/11/09 23:48:47  pierre
   * some DBX work, still does not work

  Revision 1.90  1999/11/06 14:34:21  peter
    * truncated log to 20 revs

  Revision 1.89  1999/10/22 10:39:34  peter
    * split type reading from pdecl to ptype unit
    * parameter_dec routine is now used for procedure and procvars

  Revision 1.88  1999/10/12 21:20:45  florian
    * new codegenerator compiles again

  Revision 1.87  1999/10/03 19:44:41  peter
    * removed objpasunit reference, tvarrec is now searched in systemunit
      where it already was located

  Revision 1.86  1999/10/01 08:02:45  peter
    * forward type declaration rewritten

  Revision 1.85  1999/09/16 08:02:39  pierre
   + old_compiled_module to avoid wrong file info when load PPU files

  Revision 1.84  1999/09/15 22:09:23  florian
    + rtti is now automatically generated for published classes, i.e.
      they are handled like an implicit property

  Revision 1.83  1999/08/31 15:51:11  pierre
   * in_second_compile cleaned up, in_compile and in_second_load added

  Revision 1.82  1999/08/26 20:24:41  michael
  + Hopefuly last fixes for resourcestrings

  Revision 1.81  1999/08/04 13:02:48  jonas
    * all tokens now start with an underscore
    * PowerPC compiles!!

  Revision 1.80  1999/08/03 17:09:37  florian
    * the alpha compiler can be compiled now

  Revision 1.79  1999/08/01 23:36:40  florian
    * some changes to compile the new code generator

}
