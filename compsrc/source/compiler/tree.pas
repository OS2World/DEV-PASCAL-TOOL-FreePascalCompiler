{
    $Id: tree.pas,v 1.1.2.21 2003/04/23 15:16:27 pierre Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    This units exports some routines to manage the parse tree

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
  {$E+,N+}
{$endif}
unit tree;

{$ifdef DEBUG}
  {$define TEST_EXTENDED_GETCOPY}
{$endif DEBUG}
  interface

    uses
       globtype,cobjects
       {$IFDEF NEWST}
       ,objects,symtable,symbols,defs
       {$ELSE}
       ,symconst,symtable
       {$ENDIF NEWST}
       ,aasm,cpubase;

    type
       pconstset = ^tconstset;
       tconstset = array[0..31] of byte;
       pconst32bitset = ^tconst32bitset;
       tconst32bitset = array[0..7] of longint;

       ttreetyp = (
          addn,     {Represents the + operator.}
          muln,     {Represents the * operator.}
          subn,     {Represents the - operator.}
          divn,     {Represents the div operator.}
          symdifn,       {Represents the >< operator.}
          modn,     {Represents the mod operator.}
          assignn,       {Represents an assignment.}
          loadn,           {Represents the use of a variabele.}
          rangen,         {Represents a range (i.e. 0..9).}
          ltn,       {Represents the < operator.}
          lten,     {Represents the <= operator.}
          gtn,       {Represents the > operator.}
          gten,     {Represents the >= operator.}
          equaln,         {Represents the = operator.}
          unequaln,     {Represents the <> operator.}
          inn,       {Represents the in operator.}
          orn,       {Represents the or operator.}
          xorn,     {Represents the xor operator.}
          shrn,     {Represents the shr operator.}
          shln,     {Represents the shl operator.}
          slashn,         {Represents the / operator.}
          andn,     {Represents the and operator.}
          subscriptn,      {??? Field in a record/object?}
          derefn,         {Dereferences a pointer.}
          addrn,           {Represents the @ operator.}
          doubleaddrn,     {Represents the @@ operator.}
          ordconstn,       {Represents an ordinal value.}
          typeconvn,       {Represents type-conversion/typecast.}
          calln,           {Represents a call node.}
          callparan,       {Represents a parameter.}
          realconstn,      {Represents a real value.}
          fixconstn,       {Represents a fixed value.}
          unaryminusn,     {Represents a sign change (i.e. -2).}
          asmn,     {Represents an assembler node }
          vecn,     {Represents array indexing.}
          pointerconstn,
          stringconstn,    {Represents a string constant.}
          funcretn,     {Represents the function result var.}
          selfn,           {Represents the self parameter.}
          notn,     {Represents the not operator.}
          inlinen,       {Internal procedures (i.e. writeln).}
          niln,     {Represents the nil pointer.}
          errorn,         {This part of the tree could not be
                            parsed because of a compiler error.}
          typen,           {A type name. Used for i.e. typeof(obj).}
          hnewn,           {The new operation, constructor call.}
          hdisposen,       {The dispose operation with destructor call.}
          newn,     {The new operation, constructor call.}
          simpledisposen,  {The dispose operation.}
          setelementn,     {A set element(s) (i.e. [a,b] and also [a..b]).}
          setconstn,       {A set constant (i.e. [1,2]).}
          blockn,         {A block of statements.}
          statementn,      {One statement in a block of nodes.}
          loopn,           { used in genloopnode, must be converted }
          ifn,       {An if statement.}
          breakn,         {A break statement.}
          continuen,       {A continue statement.}
          repeatn,       {A repeat until block.}
          whilen,         {A while do statement.}
          forn,     {A for loop.}
          exitn,           {An exit statement.}
          withn,           {A with statement.}
          casen,           {A case statement.}
          labeln,         {A label.}
          goton,           {A goto statement.}
          simplenewn,      {The new operation.}
          tryexceptn,      {A try except block.}
          raisen,         {A raise statement.}
          switchesn,       {??? Currently unused...}
          tryfinallyn,     {A try finally statement.}
          onn,       { for an on statement in exception code }
          isn,       {Represents the is operator.}
          asn,       {Represents the as typecast.}
          caretn,         {Represents the ^ operator.}
          failn,           {Represents the fail statement.}
          starstarn,       {Represents the ** operator exponentiation }
          procinlinen,     {Procedures that can be inlined }
          arrayconstructn, {Construction node for [...] parsing}
          arrayconstructrangen, {Range element to allow sets in array construction tree}
          { added for optimizations where we cannot suppress }
          nothingn,
          loadvmtn
       );

       tconverttype = (
          tc_equal,
          tc_not_possible,
          tc_string_2_string,
          tc_char_2_string,
          tc_char_2_chararray,
          tc_pchar_2_string,
          tc_cchar_2_pchar,
          tc_cstring_2_pchar,
          tc_ansistring_2_pchar,
          tc_string_2_chararray,
          tc_chararray_2_string,
          tc_array_2_pointer,
          tc_pointer_2_array,
          tc_int_2_int,
          tc_int_2_bool,
          tc_bool_2_bool,
          tc_bool_2_int,
          tc_real_2_real,
          tc_int_2_real,
          tc_int_2_fix,
          tc_real_2_fix,
          tc_fix_2_real,
          tc_proc_2_procvar,
          tc_arrayconstructor_2_set,
          tc_load_smallset,
          tc_cord_2_pointer
       );

       { allows to determine which elementes are to be replaced }
       tdisposetyp = (dt_nothing,dt_leftright,dt_left,dt_leftrighthigh,
                      dt_mbleft,dt_typeconv,dt_inlinen,dt_leftrightmethod,
                      dt_mbleft_and_method,dt_loop,dt_case,dt_with,dt_onn,
                      dt_leftrightframe);

      { different assignment types }

      tassigntyp = (at_normal,at_plus,at_minus,at_star,at_slash);

      pcaserecord = ^tcaserecord;
      tcaserecord = record

          { range }
          _low,_high : longint;

          { only used by gentreejmp }
          _at : pasmlabel;

          { label of instruction }
          statement : pasmlabel;

          { is this the first of an case entry, needed to release statement
            label (PFV) }
          firstlabel : boolean;

          { left and right tree node }
          less,greater : pcaserecord;
       end;

       ptree = ^ttree;
       ttree = record
          error : boolean;
          disposetyp : tdisposetyp;
          { is true, if the right and left operand are swaped }
          swaped : boolean;
          { do we need to parse childs to set var state }
          varstateset : boolean;
          { the location of the result of this node }
          location : tlocation;

          { the number of registers needed to evalute the node }
          registers32,registersfpu : longint;  { must be longint !!!! }
{$ifdef SUPPORT_MMX}
          registersmmx : longint;
{$endif SUPPORT_MMX}
          left,right : ptree;
          resulttype : pdef;
          fileinfo : tfileposinfo;
          localswitches : tlocalswitches;
          isproperty : boolean;
{$ifdef extdebug}
          firstpasscount : longint;
{$endif extdebug}
{$ifdef TEMPREGDEBUG}
          usableregs : longint;
{$endif TEMPREGDEBUG}
{$ifdef EXTTEMPREGDEBUG}
          reallyusedregs : longint;
{$endif EXTTEMPREGDEBUG}
{$ifdef TEMPS_NOT_PUSH}
          temp_offset : longint;
{$endif TEMPS_NOT_PUSH}
          case treetype : ttreetyp of
             addn : (use_strconcat : boolean;string_typ : tstringtype);
             callparan : (is_colon_para : boolean;exact_match_found,
                          convlevel1found,convlevel2found:boolean;hightree:ptree);
             assignn : (assigntyp : tassigntyp;concat_string : boolean);
             loadn : (symtableentry : psym;symtable : psymtable;
                      is_absolute,is_first : boolean);
             calln : (symtableprocentry : pprocsym;
                      symtableproc : psymtable;procdefinition : pabstractprocdef;
                      methodpointer : ptree;
                      no_check,unit_specific,
                      return_value_used,static_call : boolean);
             addrn : (procvarload:boolean;procvarloaddef:pprocvardef);
             ordconstn : (value : longint);
             realconstn : (value_real : bestreal;lab_real : pasmlabel);
             fixconstn : (value_fix: longint);
             funcretn : (funcretprocinfo : pointer;
                       retsym:Pfuncretsym;
                       rettype : ttype;
                       is_first_funcret : boolean);
             subscriptn : (vs : pvarsym);
             raisen : (frametree : ptree);
             vecn : (memindex,memseg:boolean;callunique : boolean);
             stringconstn : (value_str : pchar;length : longint; lab_str : pasmlabel;stringtype : tstringtype);
             typeconvn : (convtyp : tconverttype;explizit : boolean);
             typen : (typenodetype : pdef;typenodesym:ptypesym);
             inlinen : (inlinenumber : byte;inlineconst:boolean);
             procinlinen : (inlinetree:ptree;inlineprocsym:pprocsym;retoffset,para_offset,para_size : longint);
             setconstn : (value_set : pconstset;lab_set:pasmlabel);
             loopn : (t1,t2 : ptree;backward : boolean);
             asmn : (p_asm : paasmoutput;object_preserved : boolean);
             casen : (nodes : pcaserecord;elseblock : ptree);
             labeln,goton : (labelnr : pasmlabel;exceptionblock : ptree;labsym : plabelsym);
        {$IFDEF NEWST}
             withn : (withsymtables:Pcollection;
                      withreference:preference;
                      islocal:boolean);
        {$ELSE}
             withn : (withsymtable : pwithsymtable;
                      tablecount : longint;
                      withreference:preference;
                      islocal:boolean);
        {$ENDIF NEWST}
             onn : (exceptsymtable : psymtable;excepttype : pobjectdef);
             arrayconstructn : (cargs,cargswap,forcevaria,novariaallowed: boolean;constructdef:pdef);
           end;

    function gennode(t : ttreetyp;l,r : ptree) : ptree;
    function genlabelnode(t : ttreetyp;nr : pasmlabel) : ptree;
    function genloadnode(v : pvarsym;st : psymtable) : ptree;
    function genloadcallnode(v: pprocsym;def:pprocdef;st: psymtable): ptree;
    function genloadmethodcallnode(v: pprocsym;def:pprocdef;st: psymtable; mp:ptree): ptree;
    function gensinglenode(t : ttreetyp;l : ptree) : ptree;
    function gensubscriptnode(varsym : pvarsym;l : ptree) : ptree;
    function genordinalconstnode(v : longint;def : pdef; rangecheck : boolean) : ptree;
    function genpointerconstnode(v : longint;def : pdef) : ptree;
    function genfixconstnode(v : longint;def : pdef) : ptree;
    function gentypeconvnode(node : ptree;t : pdef) : ptree;
    function gentypenode(t : pdef;sym:ptypesym) : ptree;
    function gencallparanode(expr,next : ptree) : ptree;
    function genrealconstnode(v : bestreal;def : pdef) : ptree;
    function gencallnode(v : pprocsym;st : psymtable) : ptree;
    function genmethodcallnode(v : pprocsym;st : psymtable;mp : ptree) : ptree;

    { allow pchar or string for defining a pchar node }
    function genstringconstnode(const s : string;st:tstringtype) : ptree;
    { length is required for ansistrings }
    function genpcharconstnode(s : pchar;length : longint) : ptree;
    { helper routine for conststring node }
    function getpcharcopy(p : ptree) : pchar;

    function genzeronode(t : ttreetyp) : ptree;
    function geninlinenode(number : byte;is_const:boolean;l : ptree) : ptree;
    function genprocinlinenode(callp,code : ptree) : ptree;
    function gentypedconstloadnode(sym : ptypedconstsym;st : psymtable) : ptree;
    function genenumnode(v : penumsym) : ptree;
    function genraisenode(l,r,frame : ptree) : ptree;
    function genselfnode(_class : pdef) : ptree;
    function gensetconstnode(s : pconstset;settype : psetdef) : ptree;
    function genloopnode(t : ttreetyp;l,r,n1: ptree;back : boolean) : ptree;
    function genasmnode(p_asm : paasmoutput) : ptree;
    function gencasenode(l,r : ptree;nodes : pcaserecord) : ptree;
{$IFDEF NEWST}
    function genwithnode(symtables:Pcollection;l,r : ptree) : ptree;
{$ELSE}
    function genwithnode(symtable:pwithsymtable;l,r : ptree;count : longint) : ptree;
{$ENDIF NEWST}

    function getcopy(p : ptree) : ptree;

    function equal_trees(t1,t2 : ptree) : boolean;
{$ifdef newoptimizations2}
    { checks if t1 is loaded more than once in t2 and its sub-trees }
    function multiple_uses(t1,t2: ptree): boolean;
{$endif newoptimizations2}

    procedure swaptree(p:Ptree);
    procedure disposetree(p : ptree);
    procedure putnode(p : ptree);
    { function getnode : ptree;
      this function should not be
      in interface as it may generates
      memory losses if used }
    procedure clear_location(var loc : tlocation);
    procedure set_location(var destloc,sourceloc : tlocation);
    procedure swap_location(var destloc,sourceloc : tlocation);
    procedure set_file_line(from,_to : ptree);
    procedure set_tree_filepos(p : ptree;const filepos : tfileposinfo);
{$ifdef extdebug}
    procedure compare_trees(oldp,p : ptree);
    const
       maxfirstpasscount : longint = 0;
{$endif extdebug}

    { sets the callunique flag, if the node is a vecn, }
    { takes care of type casts etc.                 }
    procedure set_unique(p : ptree);

    { sets funcret_is_valid to true, if p contains a funcref node }
    procedure set_funcret_is_valid(p : ptree);

    {
    type
    tvarstaterequire = (vsr_can_be_undefined,vsr_must_be_valid,
      vsr_is_used_after,vsr_must_be_valid_and_is_used_after); }

    { sets varsym varstate field correctly }
    procedure unset_varstate(p : ptree);
    procedure set_varstate(p : ptree;must_be_valid : boolean);

    { gibt den ordinalen Werten der Node zurueck oder falls sie }
    { keinen ordinalen Wert hat, wird ein Fehler erzeugt        }
    function get_ordinal_value(p : ptree) : longint;
    function get_int64_ordinal_value(p : ptree) : int64;

    function is_constnode(p : ptree) : boolean;
    { true, if p is a pointer to a const int value }
    function is_constintnode(p : ptree) : boolean;
    function is_constboolnode(p : ptree) : boolean;
    function is_constrealnode(p : ptree) : boolean;
    function is_constcharnode(p : ptree) : boolean;
    function is_constresourcestringnode(p : ptree) : boolean;

    function str_length(p : ptree) : longint;
    function is_emptyset(p : ptree):boolean;

    { counts the labels }
    function case_count_labels(root : pcaserecord) : longint;
    { searches the highest label }
    function case_get_max(root : pcaserecord) : longint;
    { searches the lowest label }
    function case_get_min(root : pcaserecord) : longint;

    type
      pptree = ^ptree;

{$ifdef TEMPREGDEBUG}
    const
      curptree : pptree = nil;
{$endif TEMPREGDEBUG}

{$I innrcomp.inc}

{$ifdef newcg}
{$I nodeh.inc}
{$endif newcg}
  implementation

    uses
       systems,
       globals,verbose,files,types,
{$ifdef newcg}
       cgbase
{$else newcg}
       hcodegen
{$endif newcg}
{$IFDEF NEWST}
       ,symtablt
{$ENDIF}
       ;

    function getnode : ptree;

      var
         hp : ptree;

      begin
         new(hp);
         { makes error tracking easier }
         fillchar(hp^,sizeof(ttree),0);
         { reset }
         hp^.location.loc:=LOC_INVALID;
         { save local info }
         hp^.fileinfo:=aktfilepos;
         hp^.localswitches:=aktlocalswitches;
         getnode:=hp;
      end;


    procedure putnode(p : ptree);
      begin
         { clean up the contents of a node }
         case p^.treetype of
          asmn : if assigned(p^.p_asm) then
                  dispose(p^.p_asm,done);
  stringconstn : begin
                   ansistringdispose(p^.value_str,p^.length);
                 end;
     setconstn : begin
                   if assigned(p^.value_set) then
                     dispose(p^.value_set);
                 end;
   procinlinen : begin
                   disposetree(p^.inlinetree);
                 end;
         end;
{$ifdef extdebug}
         if p^.firstpasscount>maxfirstpasscount then
            maxfirstpasscount:=p^.firstpasscount;
{$endif extdebug}
         dispose(p);
      end;

{$ifdef TEST_EXTENDED_GETCOPY}

{$IFDEF NEWST}
    function getwithcolcopy(pwc : Pcollection) : Pcollection;
      begin
      {$warning getwithcolcopy function not implemented }
        internalerror(11);
      end;
{$ENDIF}


    function duplicatecaselabels(p : pcaserecord;statements : ptree) : pcaserecord;
      var
        hp : pcaserecord;
        cp : ptree;
      begin
         new(hp);
         move(p^,hp^,sizeof(tcaserecord));
         getlabel(hp^._at);
         getlabel(hp^.statement);
         { we need to change the pasmlabel of the labeln
           which still point to the old labels }
         cp:=statements;
         while assigned(cp) do
           begin
             if (cp^.right^.treetype=labeln) then
               begin
                if (cp^.right^.labelnr=p^.statement) then
                   begin
                     cp^.right^.labelnr:=hp^.statement;
                     break;
                   end;
               end
             else
               internalerror(20021018);
             cp:=cp^.left;
           end;
         if assigned(p^.greater) then
           hp^.greater:=duplicatecaselabels(p^.greater,statements);
         if assigned(p^.less) then
           hp^.less:=duplicatecaselabels(p^.less,statements);
         duplicatecaselabels:=hp;
      end;
{$endif TEST_EXTENDED_GETCOPY}

    function getcopy(p : ptree) : ptree;

      var
{$ifdef TEST_EXTENDED_GETCOPY}
         symt,nsymt : pwithsymtable;
         i : longint;
{$endif TEST_EXTENDED_GETCOPY}
         hp : ptree;

      begin
         if not assigned(p) then
          begin
            getcopy:=nil;
            exit;
          end;
         hp:=getnode;
         hp^:=p^;
{$ifdef extdebug}
         hp^.firstpasscount:=0;
{$endif extdebug}
         {
         *0: dt_nothing,
         *1: dt_leftright,
         *2: dt_left,
         *3: dt_leftrighthigh,
         *4: dt_mbleft,
         *5: dt_typeconv,
         *6: dt_inlinen,
         *7: dt_leftrightmethod,
         *8: dt_mbleft_and_method,
         *9: dt_loop,
         *10: dt_case,
         11: dt_with,
         12: dt_onn,
         *13: dt_leftrightframe
         }
         case p^.disposetyp of
            {1} dt_leftright :
              begin
                 if assigned(p^.left) then
                   hp^.left:=getcopy(p^.left);
                 if assigned(p^.right) then
                   hp^.right:=getcopy(p^.right);
              end;
            {3} dt_leftrighthigh :
              begin
                 if assigned(p^.left) then
                   hp^.left:=getcopy(p^.left);
                 if assigned(p^.right) then
                   hp^.right:=getcopy(p^.right);
                 if assigned(p^.hightree) then
                   hp^.hightree:=getcopy(p^.hightree);
              end;
            {13} dt_leftrightframe :
              begin
                 if assigned(p^.left) then
                   hp^.left:=getcopy(p^.left);
                 if assigned(p^.right) then
                   hp^.right:=getcopy(p^.right);
                 if assigned(p^.frametree) then
                   hp^.frametree:=getcopy(p^.frametree);
              end;
            {7} dt_leftrightmethod :
              begin
                 if assigned(p^.left) then
                   hp^.left:=getcopy(p^.left);
                 if assigned(p^.right) then
                   hp^.right:=getcopy(p^.right);
                 if assigned(p^.methodpointer) then
                   hp^.methodpointer:=getcopy(p^.methodpointer);
              end;
            {0} dt_nothing : ;
            {2} dt_left    :
              if assigned(p^.left) then
                hp^.left:=getcopy(p^.left);
            {4} dt_mbleft :
              if assigned(p^.left) then
                hp^.left:=getcopy(p^.left);
            {8} dt_mbleft_and_method :
              begin
                 if assigned(p^.left) then
                   hp^.left:=getcopy(p^.left);
                 hp^.methodpointer:=getcopy(p^.methodpointer);
              end;
            {9} dt_loop :
              begin
                 if assigned(p^.left) then
                   hp^.left:=getcopy(p^.left);
                 if assigned(p^.right) then
                   hp^.right:=getcopy(p^.right);
                 if assigned(p^.t1) then
                   hp^.t1:=getcopy(p^.t1);
                 if assigned(p^.t2) then
                   hp^.t2:=getcopy(p^.t2);
              end;
            {5} dt_typeconv : hp^.left:=getcopy(p^.left);
            {6} dt_inlinen :
              if assigned(p^.left) then
                hp^.left:=getcopy(p^.left);
{$ifdef TEST_EXTENDED_GETCOPY}
            {10} dt_case :
              begin
                 if assigned(p^.left) then
                   hp^.left:=getcopy(p^.left);
                 if assigned(p^.right) then
                   hp^.right:=getcopy(p^.right);
                 if assigned(p^.nodes) then
                   hp^.nodes:=duplicatecaselabels(p^.nodes,hp^.right);
                 if assigned(p^.elseblock) then
                   hp^.elseblock:=getcopy(p^.elseblock);
              end;
            {11} dt_with :
              begin
                 if assigned(p^.left) then
                   hp^.left:=getcopy(p^.left);
                 if assigned(p^.right) then
                   hp^.right:=getcopy(p^.right);
              {$IFDEF NEWST}
                 hp^.withsymtables:=getwithcolcopy(p^.withsymtables);
              {$ELSE}
                 symt:=p^.withsymtable;
                 nsymt:=nil;
                 for i:=1 to p^.tablecount do
                   begin
                      if assigned(symt) then
                        begin
                          inc(symt^.use_count);
                          symt^.withrefnode:=hp^.left;
                        end;
                      symt:=pwithsymtable(symt^.next);
                   end;
              {$ENDIF NEWST}
              end;
{$endif TEST_EXTENDED_GETCOPY}
            else internalerror(11);
         end;
       { now check treetype }
         case p^.treetype of
  stringconstn : begin
                   hp^.value_str:=getpcharcopy(p);
                   hp^.length:=p^.length;
                 end;
     setconstn : begin
                   new(hp^.value_set);
                   hp^.value_set^:=p^.value_set^;
                 end;
   procinlinen : begin
                   hp^.inlinetree := getcopy(p^.inlinetree)
                 end;
          asmn : begin
                   new(hp^.p_asm,init);
                   hp^.p_asm^.concatlistcopy(p^.p_asm);
                 end;
            end;
         getcopy:=hp;
      end;

    procedure deletecaselabels(p : pcaserecord);

      begin
         if assigned(p^.greater) then
           deletecaselabels(p^.greater);
         if assigned(p^.less) then
           deletecaselabels(p^.less);
         dispose(p);
      end;

    procedure swaptree(p:Ptree);

    var swapp:Ptree;

    begin
        swapp:=p^.right;
        p^.right:=p^.left;
        p^.left:=swapp;
        p^.swaped:=not(p^.swaped);
    end;


    procedure disposetree(p : ptree);

      var
         nsymt, symt : pwithsymtable;
         i : longint;
         np : ptree;

      begin
         if not(assigned(p)) then
           exit;
         repeat
           np:=nil;
           if not(p^.treetype in [addn..loadvmtn]) then
             internalerror(26219);
           case p^.disposetyp of
              dt_leftright :
                begin
                   { avoid deep recursions
                     for procedure with lots of statements PM }
                   if p^.treetype=statementn then
                     np:=p^.left
                   else if assigned(p^.left) then
                     disposetree(p^.left);
                   if assigned(p^.right) then
                     disposetree(p^.right);
                end;
              dt_leftrighthigh :
                begin
                   if assigned(p^.left) then
                     disposetree(p^.left);
                   if assigned(p^.right) then
                     disposetree(p^.right);
                   if assigned(p^.hightree) then
                     disposetree(p^.hightree);
                end;
              dt_leftrightframe :
                begin
                   if assigned(p^.left) then
                     disposetree(p^.left);
                   if assigned(p^.right) then
                     disposetree(p^.right);
                   if assigned(p^.frametree) then
                     disposetree(p^.frametree);
                end;
              dt_leftrightmethod :
                begin
                   if assigned(p^.left) then
                     disposetree(p^.left);
                   if assigned(p^.right) then
                     disposetree(p^.right);
                   if assigned(p^.methodpointer) then
                     disposetree(p^.methodpointer);
                end;
              dt_case :
                begin
                   if assigned(p^.left) then
                     disposetree(p^.left);
                   if assigned(p^.right) then
                     disposetree(p^.right);
                   if assigned(p^.nodes) then
                     deletecaselabels(p^.nodes);
                   if assigned(p^.elseblock) then
                     disposetree(p^.elseblock);
                end;
              dt_nothing : ;
              dt_left    :
                if assigned(p^.left) then
                  disposetree(p^.left);
              dt_mbleft :
                if assigned(p^.left) then
                  disposetree(p^.left);
              dt_mbleft_and_method :
                begin
                   if assigned(p^.left) then disposetree(p^.left);
                   disposetree(p^.methodpointer);
                end;
              dt_typeconv : disposetree(p^.left);
              dt_inlinen :
                if assigned(p^.left) then
                  disposetree(p^.left);
              dt_loop :
                begin
                   if assigned(p^.left) then
                     disposetree(p^.left);
                   if assigned(p^.right) then
                     disposetree(p^.right);
                   if assigned(p^.t1) then
                     disposetree(p^.t1);
                   if assigned(p^.t2) then
                     disposetree(p^.t2);
                end;
              dt_onn:
                begin
                   if assigned(p^.left) then
                     disposetree(p^.left);
                   if assigned(p^.right) then
                     disposetree(p^.right);
                   if assigned(p^.exceptsymtable) then
                     dispose(p^.exceptsymtable,done);
                end;
              dt_with :
                begin
                   if assigned(p^.left) then
                     disposetree(p^.left);
                   if assigned(p^.right) then
                     disposetree(p^.right);
                {$IFDEF NEWST}
                   dispose(p^.withsymtables,done);
                {$ELSE}
                   symt:=p^.withsymtable;
                   for i:=1 to p^.tablecount do
                     begin
                        if assigned(symt) then
                          begin
                             nsymt:=pwithsymtable(symt^.next);
                             dec(symt^.use_count);
                             if  symt^.use_count=0 then
                               dispose(symt,done);
                          end;
                        symt:=nsymt;
                     end;
                {$ENDIF NEWST}
                end;
              else internalerror(12);
           end;
           putnode(p);
           p:=np;
         until p=nil;
      end;

    procedure set_file_line(from,_to : ptree);

      begin
         if assigned(from) then
           _to^.fileinfo:=from^.fileinfo;
      end;

   procedure set_tree_filepos(p : ptree;const filepos : tfileposinfo);
     begin
        p^.fileinfo:=filepos;
     end;

{$IFDEF NEWST}
   function genwithnode(symtables:Pcollection;l,r : ptree) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_with;
         p^.treetype:=withn;
         p^.left:=l;
         p^.right:=r;
         p^.registers32:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=nil;
         p^.withsymtables:=symtables;
         p^.withreference:=nil;
         p^.islocal:=false;
         set_file_line(l,p);
         genwithnode:=p;
      end;
{$ELSE}
   function genwithnode(symtable : pwithsymtable;l,r : ptree;count : longint) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_with;
         p^.treetype:=withn;
         p^.left:=l;
         p^.right:=r;
         p^.registers32:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=nil;
         p^.withsymtable:=symtable;
         p^.tablecount:=count;
         p^.withreference:=nil;
         p^.islocal:=false;
         set_file_line(l,p);
         genwithnode:=p;
      end;
{$ENDIF NEWST}

    function genfixconstnode(v : longint;def : pdef) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_nothing;
         p^.treetype:=fixconstn;
         p^.registers32:=0;
         { p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=def;
         p^.value:=v;
         genfixconstnode:=p;
      end;

    function gencallparanode(expr,next : ptree) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_leftrighthigh;
         p^.treetype:=callparan;
         p^.left:=expr;
         p^.right:=next;
         p^.registers32:=0;
         { p^.registers16:=0;
         p^.registers8:=0; }
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.registersfpu:=0;
         p^.resulttype:=nil;
         p^.exact_match_found:=false;
         p^.convlevel1found:=false;
         p^.convlevel2found:=false;
         p^.is_colon_para:=false;
         p^.hightree:=nil;
         set_file_line(expr,p);
         gencallparanode:=p;
      end;

    function gennode(t : ttreetyp;l,r : ptree) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_leftright;
         p^.treetype:=t;
         p^.left:=l;
         p^.right:=r;
         p^.registers32:=0;
         { p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=nil;
         gennode:=p;
      end;

    function gencasenode(l,r : ptree;nodes : pcaserecord) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_case;
         p^.treetype:=casen;
         p^.left:=l;
         p^.right:=r;
         p^.nodes:=nodes;
         p^.registers32:=0;
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=nil;
         set_file_line(l,p);
         gencasenode:=p;
      end;

    function genloopnode(t : ttreetyp;l,r,n1 : ptree;back : boolean) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_loop;
         p^.treetype:=t;
         p^.left:=l;
         p^.right:=r;
         p^.t1:=n1;
         p^.t2:=nil;
         p^.registers32:=0;
         p^.backward:=back;
         { p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=nil;
         set_file_line(l,p);
         genloopnode:=p;
      end;

    function genordinalconstnode(v : longint;def : pdef; rangecheck : boolean) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_nothing;
         p^.treetype:=ordconstn;
         p^.registers32:=0;
         { p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=def;
         p^.value:=v;
         if rangecheck then
            testrange(p^.resulttype,p^.value);
         genordinalconstnode:=p;
      end;

    function genpointerconstnode(v : longint;def : pdef) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_nothing;
         p^.treetype:=pointerconstn;
         p^.registers32:=0;
         { p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=def;
         p^.value:=v;
         genpointerconstnode:=p;
      end;

    function genenumnode(v : penumsym) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_nothing;
         p^.treetype:=ordconstn;
         p^.registers32:=0;
{        p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=v^.definition;
         p^.value:=v^.value;
         testrange(p^.resulttype,p^.value);
         genenumnode:=p;
      end;


    function genrealconstnode(v : bestreal;def : pdef) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_nothing;
         p^.treetype:=realconstn;
         p^.registers32:=0;
{        p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=1;
         { even if the const is in memory, it must be loaded to be used PM }
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=def;
         p^.value_real:=v;
         p^.lab_real:=nil;
         genrealconstnode:=p;
      end;


    function genstringconstnode(const s : string;st:tstringtype) : ptree;

      var
         p : ptree;
         l : longint;
      begin
         p:=getnode;
         p^.disposetyp:=dt_nothing;
         p^.treetype:=stringconstn;
         p^.registers32:=0;
{        p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         l:=length(s);
         p^.length:=l;
         { stringdup write even past a #0 }
         getmem(p^.value_str,l+1);
         move(s[1],p^.value_str^,l);
         p^.value_str[l]:=#0;
         p^.lab_str:=nil;
         if st=st_default then
          begin
            if cs_ansistrings in aktlocalswitches then
             p^.stringtype:=st_ansistring
            else
             p^.stringtype:=st_shortstring;
          end
         else
          p^.stringtype:=st;
         case p^.stringtype of
           st_shortstring :
             p^.resulttype:=cshortstringdef;
           st_ansistring :
            p^.resulttype:=cansistringdef;
           else
             internalerror(44990099);
         end;
         genstringconstnode:=p;
      end;

    function getpcharcopy(p : ptree) : pchar;
      var
         pc : pchar;
      begin
         pc:=nil;
         getmem(pc,p^.length+1);
         if pc=nil then
           Message(general_f_no_memory_left);
         move(p^.value_str^,pc^,p^.length+1);
         getpcharcopy:=pc;
      end;


    function genpcharconstnode(s : pchar;length : longint) : ptree;
      var
         p : ptree;
      begin
         p:=getnode;
         p^.disposetyp:=dt_nothing;
         p^.treetype:=stringconstn;
         p^.registers32:=0;
{        p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.length:=length;
         if (cs_ansistrings in aktlocalswitches) or
            (length>255) then
          begin
            p^.stringtype:=st_ansistring;
            p^.resulttype:=cansistringdef;
          end
         else
          begin
            p^.stringtype:=st_shortstring;
            p^.resulttype:=cshortstringdef;
          end;
         p^.value_str:=s;
         p^.lab_str:=nil;
         genpcharconstnode:=p;
      end;


    function gensinglenode(t : ttreetyp;l : ptree) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_left;
         p^.treetype:=t;
         p^.left:=l;
         p^.registers32:=0;
{        p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=nil;
         gensinglenode:=p;
      end;

    function genasmnode(p_asm : paasmoutput) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_nothing;
         p^.treetype:=asmn;
         p^.registers32:=maxintregs;
         p^.p_asm:=p_asm;
         p^.object_preserved:=false;
{        p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=maxfpuregs;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=8;
{$endif SUPPORT_MMX}
         p^.resulttype:=nil;
         genasmnode:=p;
      end;

    function genloadnode(v : pvarsym;st : psymtable) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.registers32:=0;
{        p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.treetype:=loadn;
      {$IFDEF NEWST}
         p^.resulttype:=v^.definition;
      {$ELSE}
         p^.resulttype:=v^.vartype.def;
      {$ENDIF NEWST}
         p^.symtableentry:=v;
         p^.symtable:=st;
         p^.is_first := False;
         { method pointer load nodes can use the left subtree }
         p^.disposetyp:=dt_left;
         p^.left:=nil;
         genloadnode:=p;
      end;

    function genloadcallnode(v: pprocsym;def:pprocdef;st: psymtable): ptree;
      var
         p : ptree;

      begin
         p:=getnode;
         p^.registers32:=0;
{        p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.treetype:=loadn;
         p^.left:=nil;
         p^.resulttype:=def;
         p^.symtableentry:=v;
         p^.symtable:=st;
         p^.is_first := False;
         p^.disposetyp:=dt_nothing;
         genloadcallnode:=p;
      end;

    function genloadmethodcallnode(v: pprocsym;def:pprocdef;st: psymtable; mp:ptree): ptree;
      var
         p : ptree;

      begin
         p:=getnode;
         p^.registers32:=0;
{        p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.treetype:=loadn;
         p^.left:=nil;
         p^.resulttype:=def;
         p^.symtableentry:=v;
         p^.symtable:=st;
         p^.is_first := False;
         p^.disposetyp:=dt_left;
         if mp^.treetype=typen then
           internalerror(200301041);
         p^.left:=mp;
         genloadmethodcallnode:=p;
      end;


    function gentypedconstloadnode(sym : ptypedconstsym;st : psymtable) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.registers32:=0;
{        p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.treetype:=loadn;
         p^.left:=nil;
      {$IFDEF NEWST}
         p^.resulttype:=sym^.definition;
      {$ELSE}
         p^.resulttype:=sym^.typedconsttype.def;
      {$ENDIF NEWST}
         p^.symtableentry:=sym;
         p^.symtable:=st;
         p^.disposetyp:=dt_nothing;
         gentypedconstloadnode:=p;
      end;

    function gentypeconvnode(node : ptree;t : pdef) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_typeconv;
         p^.treetype:=typeconvn;
         p^.left:=node;
         p^.registers32:=0;
{        p^.registers16:=0;
         p^.registers8:=0; }
         p^.convtyp:=tc_equal;
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=t;
         p^.explizit:=false;
         set_file_line(node,p);
         gentypeconvnode:=p;
      end;

    function gentypenode(t : pdef;sym:ptypesym) : ptree;
      var
         p : ptree;
      begin
         p:=getnode;
         p^.disposetyp:=dt_nothing;
         p^.treetype:=typen;
         p^.registers32:=0;
{        p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=generrordef;
         p^.typenodetype:=t;
         p^.typenodesym:=sym;
         gentypenode:=p;
      end;

    function gencallnode(v : pprocsym;st : psymtable) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.registers32:=0;
{        p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.treetype:=calln;
         p^.symtableprocentry:=v;
         p^.symtableproc:=st;
         p^.unit_specific:=false;
         p^.no_check:=false;
         p^.return_value_used:=true;
         p^.disposetyp := dt_leftrightmethod;
         p^.methodpointer:=nil;
         p^.left:=nil;
         p^.right:=nil;
         p^.procdefinition:=nil;
         gencallnode:=p;
      end;

    function genmethodcallnode(v : pprocsym;st : psymtable;mp : ptree) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.registers32:=0;
{        p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.treetype:=calln;
         p^.return_value_used:=true;
         p^.symtableprocentry:=v;
         p^.symtableproc:=st;
         p^.disposetyp:=dt_leftrightmethod;
         p^.left:=nil;
         p^.right:=nil;
         p^.methodpointer:=mp;
         p^.procdefinition:=nil;
         genmethodcallnode:=p;
      end;

    function gensubscriptnode(varsym : pvarsym;l : ptree) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_left;
         p^.treetype:=subscriptn;
         p^.left:=l;
         p^.registers32:=0;
         p^.vs:=varsym;
{        p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=nil;
         gensubscriptnode:=p;
      end;

   function genzeronode(t : ttreetyp) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_nothing;
         p^.treetype:=t;
         p^.registers32:=0;
{        p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=nil;
         genzeronode:=p;
      end;

   function genlabelnode(t : ttreetyp;nr : pasmlabel) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_nothing;
         p^.treetype:=t;
         p^.registers32:=0;
{        p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=nil;
         { for security }
         { nr^.is_used:=true;}
         p^.labelnr:=nr;
         p^.exceptionblock:=nil;
         genlabelnode:=p;
      end;

    function genselfnode(_class : pdef) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_nothing;
         p^.treetype:=selfn;
         p^.registers32:=0;
{        p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=_class;
         genselfnode:=p;
      end;

    function genraisenode(l,r,frame : ptree) : ptree;

      var
         p : ptree;

      begin
         p:=gennode(raisen,l,r);
         p^.disposetyp:=dt_leftrightframe;
         p^.frametree:=frame;
         genraisenode:=p;
      end;

   function geninlinenode(number : byte;is_const:boolean;l : ptree) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_inlinen;
         p^.treetype:=inlinen;
         p^.left:=l;
         p^.inlinenumber:=number;
         p^.inlineconst:=is_const;
         p^.registers32:=0;
{        p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=nil;
         geninlinenode:=p;
      end;


      { uses the callnode to create the new procinline node }
    function genprocinlinenode(callp,code : ptree) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_nothing;
         p^.treetype:=procinlinen;
         p^.inlineprocsym:=callp^.symtableprocentry;
         p^.retoffset:=-target_os.size_of_pointer; { less dangerous as zero (PM) }
         p^.para_offset:=0;
      {$IFDEF NEWST}
         {Fixme!!}
         internalerror($00022801);
      {$ELSE}
         p^.para_size:=p^.inlineprocsym^.definition^.para_size(target_os.stackalignment);
         if ret_in_param(p^.inlineprocsym^.definition^.rettype.def,
              p^.inlineprocsym^.definition^.proccalloptions) then
           p^.para_size:=p^.para_size+target_os.size_of_pointer;
      {$ENDIF NEWST}
         { copy args }
         p^.inlinetree:=getcopy(code);
         p^.registers32:=code^.registers32;
         p^.registersfpu:=code^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
      {$IFDEF NEWST}
         {Fixme!!}
      {$ELSE}
         p^.resulttype:=p^.inlineprocsym^.definition^.rettype.def;
      {$ENDIF NEWST}
         genprocinlinenode:=p;
      end;

   function gensetconstnode(s : pconstset;settype : psetdef) : ptree;

     var
        p : ptree;

     begin
        p:=getnode;
        p^.disposetyp:=dt_nothing;
        p^.treetype:=setconstn;
        p^.registers32:=0;
        p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=settype;
         p^.left:=nil;
         new(p^.value_set);
         p^.value_set^:=s^;
         gensetconstnode:=p;
      end;

{$ifdef extdebug}
    procedure compare_trees(oldp,p : ptree);

      var
         error_found : boolean;

      begin
          if oldp^.resulttype<>p^.resulttype then
            begin
               error_found:=true;
               if is_equal(oldp^.resulttype,p^.resulttype) then
                 comment(v_debug,'resulttype fields are different but equal')
               else
                 comment(v_warning,'resulttype fields are really different');
            end;
         if oldp^.treetype<>p^.treetype then
           begin
              comment(v_warning,'treetype field different');
              error_found:=true;
           end
         else
           comment(v_debug,' treetype '+tostr(longint(oldp^.treetype)));
         if oldp^.error<>p^.error then
           begin
              comment(v_warning,'error field different');
              error_found:=true;
           end;
         if oldp^.disposetyp<>p^.disposetyp then
           begin
              comment(v_warning,'disposetyp field different');
              error_found:=true;
           end;
         { is true, if the right and left operand are swaped }
         if oldp^.swaped<>p^.swaped then
           begin
              comment(v_warning,'swaped field different');
              error_found:=true;
           end;

         { the location of the result of this node }
         if oldp^.location.loc<>p^.location.loc then
           begin
              comment(v_warning,'location.loc field different');
              error_found:=true;
           end;

          { the number of registers needed to evalute the node }
          if oldp^.registers32<>p^.registers32 then
           begin
              comment(v_warning,'registers32 field different');
              comment(v_warning,' old '+tostr(oldp^.registers32)+'<> new '+tostr(p^.registers32));
              error_found:=true;
           end;
          if oldp^.registersfpu<>p^.registersfpu then
           begin
              comment(v_warning,'registersfpu field different');
              error_found:=true;
           end;
{$ifdef SUPPORT_MMX}
          if oldp^.registersmmx<>p^.registersmmx then
           begin
              comment(v_warning,'registersmmx field different');
              error_found:=true;
           end;
{$endif SUPPORT_MMX}
          if oldp^.left<>p^.left then
           begin
              comment(v_warning,'left field different');
              error_found:=true;
           end;
          if oldp^.right<>p^.right then
           begin
              comment(v_warning,'right field different');
              error_found:=true;
           end;
          if oldp^.fileinfo.line<>p^.fileinfo.line then
            begin
               comment(v_warning,'fileinfo.line field different');
               error_found:=true;
            end;
          if oldp^.fileinfo.column<>p^.fileinfo.column then
            begin
               comment(v_warning,'fileinfo.column field different');
               error_found:=true;
            end;
          if oldp^.fileinfo.fileindex<>p^.fileinfo.fileindex then
            begin
               comment(v_warning,'fileinfo.fileindex field different');
               error_found:=true;
            end;
          if oldp^.localswitches<>p^.localswitches then
            begin
               comment(v_warning,'localswitches field different');
               error_found:=true;
            end;
{$ifdef extdebug}
          if oldp^.firstpasscount<>p^.firstpasscount then
            begin
               comment(v_warning,'firstpasscount field different');
               error_found:=true;
            end;
{$endif extdebug}
          if oldp^.treetype=p^.treetype then
          case oldp^.treetype of
             addn :
             begin
                if oldp^.use_strconcat<>p^.use_strconcat then
                  begin
                     comment(v_warning,'use_strconcat field different');
                     error_found:=true;
                  end;
                if oldp^.string_typ<>p^.string_typ then
                  begin
                     comment(v_warning,'stringtyp field different');
                     error_found:=true;
                  end;
             end;
             callparan :
             {(is_colon_para : boolean;exact_match_found : boolean);}
             begin
                if oldp^.is_colon_para<>p^.is_colon_para then
                  begin
                     comment(v_warning,'use_strconcat field different');
                     error_found:=true;
                  end;
                if oldp^.exact_match_found<>p^.exact_match_found then
                  begin
                     comment(v_warning,'exact_match_found field different');
                     error_found:=true;
                  end;
             end;
             assignn :
             {(assigntyp : tassigntyp;concat_string : boolean);}
             begin
                if oldp^.assigntyp<>p^.assigntyp then
                  begin
                     comment(v_warning,'assigntyp field different');
                     error_found:=true;
                  end;
                if oldp^.concat_string<>p^.concat_string then
                  begin
                     comment(v_warning,'concat_string field different');
                     error_found:=true;
                  end;
             end;
             loadn :
             {(symtableentry : psym;symtable : psymtable;
                      is_absolute,is_first : boolean);}
             begin
                if oldp^.symtableentry<>p^.symtableentry then
                  begin
                     comment(v_warning,'symtableentry field different');
                     error_found:=true;
                  end;
                if oldp^.symtable<>p^.symtable then
                  begin
                     comment(v_warning,'symtable field different');
                     error_found:=true;
                  end;
                if oldp^.is_absolute<>p^.is_absolute then
                  begin
                     comment(v_warning,'is_absolute field different');
                     error_found:=true;
                  end;
                if oldp^.is_first<>p^.is_first then
                  begin
                     comment(v_warning,'is_first field different');
                     error_found:=true;
                  end;
             end;
             calln :
             {(symtableprocentry : pprocsym;
                      symtableproc : psymtable;procdefinition : pprocdef;
                      methodpointer : ptree;
                      no_check,unit_specific : boolean);}
             begin
                if oldp^.symtableprocentry<>p^.symtableprocentry then
                  begin
                     comment(v_warning,'symtableprocentry field different');
                     error_found:=true;
                  end;
                if oldp^.symtableproc<>p^.symtableproc then
                  begin
                     comment(v_warning,'symtableproc field different');
                     error_found:=true;
                  end;
                if oldp^.procdefinition<>p^.procdefinition then
                  begin
                     comment(v_warning,'procdefinition field different');
                     error_found:=true;
                  end;
                if oldp^.methodpointer<>p^.methodpointer then
                  begin
                     comment(v_warning,'methodpointer field different');
                     error_found:=true;
                  end;
                if oldp^.no_check<>p^.no_check then
                  begin
                     comment(v_warning,'no_check field different');
                     error_found:=true;
                  end;
                if oldp^.unit_specific<>p^.unit_specific then
                  begin
                     error_found:=true;
                     comment(v_warning,'unit_specific field different');
                  end;
             end;
             ordconstn :
               begin
                  if oldp^.value<>p^.value then
                  begin
                     comment(v_warning,'value field different');
                     error_found:=true;
                  end;
               end;
             realconstn :
               begin
                  if oldp^.value_real<>p^.value_real then
                  begin
                     comment(v_warning,'valued field different');
                     error_found:=true;
                  end;
                  if oldp^.lab_real<>p^.lab_real then
                  begin
                     comment(v_warning,'labnumber field different');
                     error_found:=true;
                  end;
                  { if oldp^.realtyp<>p^.realtyp then
                  begin
                     comment(v_warning,'realtyp field different');
                     error_found:=true;
                  end; }
               end;
           end;
         if not error_found then
           comment(v_warning,'did not find difference in trees');

      end;
{$endif extdebug}

    function equal_trees(t1,t2 : ptree) : boolean;

      begin
         if t1^.treetype=t2^.treetype then
           begin
              case t1^.treetype of
                 addn,
                 muln,
                 equaln,
                 orn,
                 xorn,
                 andn,
                 unequaln:
                   begin
                      equal_trees:=(equal_trees(t1^.left,t2^.left) and
                                    equal_trees(t1^.right,t2^.right)) or
                                   (equal_trees(t1^.right,t2^.left) and
                                    equal_trees(t1^.left,t2^.right));
                   end;
                 subn,
                 divn,
                 modn,
                 assignn,
                 ltn,
                 lten,
                 gtn,
                 gten,
                 inn,
                 shrn,
                 shln,
                 slashn,
                 rangen:
                   begin
                      equal_trees:=(equal_trees(t1^.left,t2^.left) and
                                    equal_trees(t1^.right,t2^.right));
                   end;
                 unaryminusn,
                 notn,
                 derefn,
                 addrn:
                   begin
                      equal_trees:=(equal_trees(t1^.left,t2^.left));
                   end;
                loadn:
                   begin
                      equal_trees:=(t1^.symtableentry=t2^.symtableentry)
                        { not necessary
                                     and (t1^.symtable=t2^.symtable)};
                   end;
                {

                   subscriptn,
                   ordconstn,typeconvn,calln,callparan,
                   realconstn,asmn,vecn,
                   stringconstn,funcretn,selfn,
                   inlinen,niln,errorn,
                   typen,hnewn,hdisposen,newn,
                   disposen,setelen,setconstrn
                }
                else equal_trees:=false;
             end;
          end
        else
          equal_trees:=false;
     end;

{$ifdef newoptimizations2}
    function multiple_uses(t1,t2: ptree): boolean;
    var nr: longint;

      procedure check_tree(t: ptree);
      begin
        inc(nr,ord(equal_trees(t1,t)));
        if (nr < 2) and assigned(t^.left) then
          check_tree(t^.left);
        if (nr < 2) and assigned(t^.right) then
          check_tree(t^.right);
      end;

    begin
       nr := 0;
       check_tree(t2);
       multiple_uses := nr > 1;
    end;
{$endif newoptimizations2}

    procedure set_unique(p : ptree);

      begin
         if assigned(p) then
           begin
              case p^.treetype of
                 vecn:
                    p^.callunique:=true;
                 typeconvn,subscriptn,derefn:
                    set_unique(p^.left);
              end;
           end;
      end;

    procedure set_funcret_is_valid(p : ptree);

      begin
         if assigned(p) then
           begin
              case p^.treetype of
                 funcretn:
                    begin
                      if p^.is_first_funcret or
                         (pprocinfo(p^.funcretprocinfo)^.funcret_state=vs_declared_and_first_found) then
                        pprocinfo(p^.funcretprocinfo)^.funcret_state:=vs_assigned;
                    end;
                 vecn,typeconvn,subscriptn{,derefn}:
                    set_funcret_is_valid(p^.left);
              end;
           end;
      end;


    procedure unset_varstate(p : ptree);
      begin
        while assigned(p) do
         begin
           p^.varstateset:=false;
           case p^.treetype of
             typeconvn,
             subscriptn,
             vecn :
               p:=p^.left;
             else
               break;
           end;
         end;
      end;


    procedure set_varstate(p : ptree;must_be_valid : boolean);

      begin
         if not assigned(p) then
           exit
         else
           begin
             if p^.varstateset then
               exit;
              case p^.treetype of
           typeconvn :
             if p^.convtyp in
               [
                tc_cchar_2_pchar,
                tc_cstring_2_pchar,
                tc_array_2_pointer
               ] then
               set_varstate(p^.left,false)
             else if p^.convtyp in
               [
                tc_pchar_2_string,
                tc_pointer_2_array
               ] then
               set_varstate(p^.left,true)
             else
               set_varstate(p^.left,must_be_valid);
           subscriptn :
             set_varstate(p^.left,must_be_valid);
           vecn:
             begin
               if (p^.left^.resulttype^.deftype in [stringdef,arraydef]) then
                 set_varstate(p^.left,must_be_valid)
               else
                 set_varstate(p^.left,true);
               set_varstate(p^.right,true);
             end;
           { do not parse calln }
           calln : ;
           callparan:
             begin
               set_varstate(p^.left,must_be_valid);
               set_varstate(p^.right,must_be_valid);
             end;
           loadn :
         if (p^.symtableentry^.typ=varsym) then
          begin
            if must_be_valid and p^.is_first then
              begin
                if (pvarsym(p^.symtableentry)^.varstate=vs_declared_and_first_found) or
                   (pvarsym(p^.symtableentry)^.varstate=vs_set_but_first_not_passed) then
                 if (assigned(pvarsym(p^.symtableentry)^.owner) and
                    assigned(aktprocsym) and
                    (pvarsym(p^.symtableentry)^.owner = aktprocsym^.definition^.localst)) then
                  begin
                    if p^.symtable^.symtabletype=localsymtable then
                     CGMessage1(sym_n_uninitialized_local_variable,pvarsym(p^.symtableentry)^.name)
                    else
                     CGMessage1(sym_n_uninitialized_variable,pvarsym(p^.symtableentry)^.name);
                  end;
              end;
          if (p^.is_first) then
           begin
             if pvarsym(p^.symtableentry)^.varstate=vs_declared_and_first_found then
             { this can only happen at left of an assignment, no ? PM }
              if (parsing_para_level=0) and not must_be_valid then
               pvarsym(p^.symtableentry)^.varstate:=vs_assigned
              else
               pvarsym(p^.symtableentry)^.varstate:=vs_used;
             if pvarsym(p^.symtableentry)^.varstate=vs_set_but_first_not_passed then
               pvarsym(p^.symtableentry)^.varstate:=vs_used;
             p^.is_first:=false;
           end
         else
           begin
             if (pvarsym(p^.symtableentry)^.varstate=vs_assigned) and
                (must_be_valid or (parsing_para_level>0) or
                 (p^.resulttype^.deftype=procvardef)) then
               pvarsym(p^.symtableentry)^.varstate:=vs_used;
             if (pvarsym(p^.symtableentry)^.varstate=vs_declared_and_first_found) and
                (must_be_valid or (parsing_para_level>0) or
                (p^.resulttype^.deftype=procvardef)) then
               pvarsym(p^.symtableentry)^.varstate:=vs_set_but_first_not_passed;
           end;
         end;
         funcretn:
         begin
         { no claim if setting higher return value_str }
         if must_be_valid and
            (procinfo=pprocinfo(p^.funcretprocinfo)) and
            ((procinfo^.funcret_state=vs_declared) or
            ((p^.is_first_funcret) and
             (procinfo^.funcret_state=vs_declared_and_first_found))) then
           begin
             CGMessage(sym_w_function_result_not_set);
             { avoid multiple warnings }
             procinfo^.funcret_state:=vs_assigned;
           end;
         if p^.is_first_funcret and not must_be_valid then
           pprocinfo(p^.funcretprocinfo)^.funcret_state:=vs_assigned;
         end;
         else
           begin
             {internalerror(565656);}
           end;
         end;{case }
         p^.varstateset:=true;
      end;
    end;

    procedure clear_location(var loc : tlocation);

      begin
        loc.loc:=LOC_INVALID;
      end;

    {This is needed if you want to be able to delete the string with the nodes !!}
    procedure set_location(var destloc,sourceloc : tlocation);

      begin
        destloc:= sourceloc;
      end;

    procedure swap_location(var destloc,sourceloc : tlocation);

      var
         swapl : tlocation;

      begin
         swapl := destloc;
         destloc := sourceloc;
         sourceloc := swapl;
      end;


    function get_ordinal_value(p : ptree) : longint;
      begin
         if p^.treetype=ordconstn then
           get_ordinal_value:=p^.value
         else
           begin
             Message(type_e_ordinal_expr_expected);
             get_ordinal_value:=0;
           end;
      end;

    function get_int64_ordinal_value(p : ptree) : int64;
      begin
         if p^.treetype=ordconstn then
           begin
             if assigned(p^.resulttype) and
                is_integer(p^.resulttype) then
               case porddef(p^.resulttype)^.typ of
                 u32bit,u64bit :
                   get_int64_ordinal_value:=cardinal(p^.value);
                 else
                   get_int64_ordinal_value:=p^.value;
               end
             else
               get_int64_ordinal_value:=p^.value;
           end
         else
           begin
             Message(type_e_ordinal_expr_expected);
             get_int64_ordinal_value:=0;
           end;
      end;


    function is_constnode(p : ptree) : boolean;
      begin
        is_constnode:=(p^.treetype in [ordconstn,realconstn,stringconstn,fixconstn,setconstn]);
      end;


    function is_constintnode(p : ptree) : boolean;
      begin
         is_constintnode:=(p^.treetype=ordconstn) and is_integer(p^.resulttype);
      end;


    function is_constcharnode(p : ptree) : boolean;

      begin
         is_constcharnode:=(p^.treetype=ordconstn) and is_char(p^.resulttype);
      end;

    function is_constrealnode(p : ptree) : boolean;

      begin
         is_constrealnode:=(p^.treetype=realconstn);
      end;

    function is_constboolnode(p : ptree) : boolean;

      begin
         is_constboolnode:=(p^.treetype=ordconstn) and is_boolean(p^.resulttype);
      end;


    function is_constresourcestringnode(p : ptree) : boolean;
      begin
        is_constresourcestringnode:=(p^.treetype=loadn) and
                                    (p^.symtableentry^.typ=constsym) and
                                    (pconstsym(p^.symtableentry)^.consttyp=constresourcestring);
      end;


    function str_length(p : ptree) : longint;

      begin
         str_length:=p^.length;
      end;


    function is_emptyset(p : ptree):boolean;
    {
      return true if set s is empty
    }
      var
        i : longint;
      begin
        i:=0;
        if p^.treetype=setconstn then
         begin
           while (i<32) and (p^.value_set^[i]=0) do
            inc(i);
         end;
        is_emptyset:=(i=32);
      end;


{*****************************************************************************
                              Case Helpers
*****************************************************************************}

    function case_count_labels(root : pcaserecord) : longint;
      var
         _l : longint;

      procedure count(p : pcaserecord);
        begin
           inc(_l);
           if assigned(p^.less) then
             count(p^.less);
           if assigned(p^.greater) then
             count(p^.greater);
        end;

      begin
         _l:=0;
         count(root);
         case_count_labels:=_l;
      end;


    function case_get_max(root : pcaserecord) : longint;
      var
         hp : pcaserecord;
      begin
         hp:=root;
         while assigned(hp^.greater) do
           hp:=hp^.greater;
         case_get_max:=hp^._high;
      end;


    function case_get_min(root : pcaserecord) : longint;
      var
         hp : pcaserecord;
      begin
         hp:=root;
         while assigned(hp^.less) do
           hp:=hp^.less;
         case_get_min:=hp^._low;
      end;

{$ifdef newcg}
{$I node.inc}
{$endif newcg}
end.
{
  $Log: tree.pas,v $
  Revision 1.1.2.21  2003/04/23 15:16:27  pierre
   * avoid stack overflow on long procedures in disposetree

  Revision 1.1.2.20  2003/01/14 23:26:12  peter
    * fix for funcret use in nested proc, fixes 1851

  Revision 1.1.2.19  2003/01/05 23:06:33  peter
    * methodpointer=typen fixes

  Revision 1.1.2.18  2002/11/27 17:06:38  pierre
   + get_int64_ordinal_value added

  Revision 1.1.2.17  2002/11/15 14:10:10  pierre
    + added a calloptions parameter to ret_inacc and ret_in_param
      functions.
      This allows to handle win32 stdcall and m68k cdecl small records
      case.

  Revision 1.1.2.16  2002/10/22 10:12:41  pierre
   * try to improove with inlining (still only in debug mode)

  Revision 1.1.2.15  2002/10/18 10:45:14  pierre
   * fix label regeneration in duplicatecaselabels

  Revision 1.1.2.14  2002/10/17 20:42:26  pierre
   * test code for with and case statements inlined

  Revision 1.1.2.13  2002/10/15 18:34:40  pierre
   * remove getnode from interface

  Revision 1.1.2.12  2002/09/07 11:04:29  carl
    * 2nd part of tw1996 bugfix (genordconstnode now has option to indicate if
      range must be verified), this also optimizes a bit.

  Revision 1.1.2.11  2001/11/02 23:19:09  jonas
    * fixed web bug 1665 (allow char to chararray type conversion)

  Revision 1.1.2.10  2001/10/28 17:18:13  peter
    * allow assignment of overloaded procedures to procvars when we know
      which procedure to take

  Revision 1.1.2.9  2001/09/10 22:49:32  pierre
   * set registersfpu field correctly for realconstn

  Revision 1.1.2.8  2001/07/25 13:07:50  pierre
   + tconst32bitset type defined

  Revision 1.1.2.7  2001/06/02 19:27:41  peter
    * testranges for all ordinal constants, also enums

  Revision 1.1.2.6  2001/05/18 18:10:11  carl
  * replace by portable constant a numeric constant value

  Revision 1.1.2.5  2001/02/25 02:16:26  carl
  + cpu clean

  Revision 1.1.2.4  2001/01/09 10:04:07  florian
    * innr.inc to innrcomp.inc renamed

  Revision 1.1.2.3  2000/11/22 14:05:03  jonas
    * fixed several errors in getcopy()
    * genprocinlinenode() now makes a copy of the code tree (and it's copied
      in getcopy and disposed in putnode)

  Revision 1.1.2.2  2000/09/26 08:49:04  pierre
   * avoid memory leak with raisen

  Revision 1.1.2.1  2000/08/02 19:37:52  peter
    * unset_varstate function to reset varstateset variable, fixes bug 1034

  Revision 1.1  2000/07/13 06:30:02  michael
  + Initial import

  Revision 1.122  2000/06/29 13:49:39  jonas
    * fixed copy/paste bugs in getcopy

  Revision 1.121  2000/06/06 20:24:07  pierre
   Generate a warning if function result is a pointer and result^:=x; is used

  Revision 1.120  2000/05/17 17:10:06  peter
    * add support for loading of typed const strings with resourcestrings,
      made the loading also a bit more generic

  Revision 1.119  2000/04/25 14:43:37  jonas
    - disabled "string_var := string_var + ... " and "string_var + char_var"
      optimizations (were only active with -dnewoptimizations) because of
      several internal issues

  Revision 1.118  2000/04/24 11:11:50  peter
    * backtraces for exceptions are now only generated from the place of the
      exception
    * frame is also pushed for exceptions
    * raise statement enhanced with [,<frame>]

  Revision 1.117  2000/04/08 16:22:11  jonas
    * fixed concat_string optimization and enabled it when
      -dnewoptimizations is used

  Revision 1.116  2000/03/01 15:36:12  florian
    * some new stuff for the new cg

  Revision 1.115  2000/03/01 11:43:55  daniel
  * Some more work on the new symtable.
  + Symtable stack unit 'symstack' added.

  Revision 1.114  2000/02/28 17:23:57  daniel
  * Current work of symtable integration committed. The symtable can be
    activated by defining 'newst', but doesn't compile yet. Changes in type
    checking and oop are completed. What is left is to write a new
    symtablestack and adapt the parser to use it.

  Revision 1.113  2000/02/20 20:49:46  florian
    * newcg is compiling
    * fixed the dup id problem reported by Paul Y.

  Revision 1.112  2000/02/17 14:53:43  florian
    * some updates for the newcg

  Revision 1.111  2000/02/09 13:23:09  peter
    * log truncated

  Revision 1.110  2000/01/26 12:02:30  peter
    * abstractprocdef.para_size needs alignment parameter
    * secondcallparan gets para_alignment size instead of dword_align

  Revision 1.109  2000/01/09 23:16:07  peter
    * added st_default stringtype
    * genstringconstnode extended with stringtype parameter using st_default
      will do the old behaviour

  Revision 1.108  2000/01/07 01:14:48  peter
    * updated copyright to 2000

  Revision 1.107  2000/01/06 01:10:33  pierre
   * fixes for set_varstate on conversions

  Revision 1.106  1999/12/22 01:01:52  peter
    - removed freelabel()
    * added undefined label detection in internal assembler, this prevents
      a lot of ld crashes and wrong .o files
    * .o files aren't written anymore if errors have occured
    * inlining of assembler labels is now correct

  Revision 1.105  1999/12/14 09:58:42  florian
    + compiler checks now if a goto leaves an exception block

  Revision 1.104  1999/11/30 10:40:59  peter
    + ttype, tsymlist

  Revision 1.103  1999/11/18 15:34:51  pierre
    * Notes/Hints for local syms changed to
      Set_varstate function

  Revision 1.102  1999/11/17 17:05:07  pierre
   * Notes/hints changes

  Revision 1.101  1999/11/06 14:34:31  peter
    * truncated log to 20 revs

  Revision 1.100  1999/10/22 14:37:31  peter
    * error when properties are passed to var parameters

  Revision 1.99  1999/09/27 23:45:03  peter
    * procinfo is now a pointer
    * support for result setting in sub procedure

  Revision 1.98  1999/09/26 21:30:22  peter
    + constant pointer support which can happend with typecasting like
      const p=pointer(1)
    * better procvar parsing in typed consts

  Revision 1.97  1999/09/17 17:14:13  peter
    * @procvar fixes for tp mode
    * @<id>:= gives now an error

  Revision 1.96  1999/09/16 11:34:59  pierre
   * typo correction

  Revision 1.95  1999/09/10 18:48:11  florian
    * some bug fixes (e.g. must_be_valid and procinfo^.funcret_is_valid)
    * most things for stored properties fixed

  Revision 1.94  1999/09/07 07:52:20  peter
    * > < >= <= support for boolean
    * boolean constants are now calculated like integer constants

  Revision 1.93  1999/08/27 10:38:31  pierre
   + EXTTEMPREGDEBUG code added

  Revision 1.92  1999/08/26 21:10:08  peter
    * better error recovery for case

  Revision 1.91  1999/08/23 23:26:00  pierre
    + TEMPREGDEBUG code, test of register allocation
      if a tree uses more than registers32 regs then
      internalerror(10) is issued
    + EXTTEMPREGDEBUG will also give internalerror(10) if
      a same register is freed twice (happens in several part
      of current compiler like addn for strings and sets)

}
