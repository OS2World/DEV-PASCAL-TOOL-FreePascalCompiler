{
    $Id: symdefh.inc,v 1.1.2.13 2003/01/15 01:42:27 peter Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl, Pierre Muller

    Interface for the definition types of the symtable

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

{************************************************
                    TDef
************************************************}



       tdef = object(tsymtableentry)
          typesym    : ptypesym;  { which type the definition was generated this def }

{$ifdef EXTDEBUG}
          fileinfo   : tfileposinfo;
{$endif}

          { adress of init informations }
          inittable_label : pasmlabel;

          { address of rtti }
          rtti_label : pasmlabel;

          nextglobal,
          previousglobal : pdef;
       private
          savesize  : longint;
       public
          deftype    : tdeftype;

          has_inittable : boolean;
          has_rtti   : boolean;
{$ifdef GDB}
          is_def_stab_written : tdefstabstatus;
          globalnb       : longint;
{$endif GDB}
          { two bytes left until next multiple of 4 (JM) }

          constructor init;
          constructor load;
          destructor  done;virtual;
          procedure deref;virtual;
          function  typename:string;
          procedure write;virtual;
          function  size:longint;virtual;
          function  alignment:longint;virtual;
          function  gettypename:string;virtual;
          function  is_publishable : boolean;virtual;
          function  is_in_current : boolean;
          procedure correct_owner_symtable; { registers enumdef inside objects or
                                              record directly in the owner symtable !! }
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
          function  NumberString:string;
          procedure set_globalnb;virtual;
          function  allstabstring : pchar;virtual;
{$endif GDB}
          { init. tables }
          function  needs_inittable : boolean;virtual;
          procedure generate_inittable;
          function  get_inittable_label : pasmlabel;
          { the default implemenation calls write_rtti_data     }
          { if init and rtti data is different these procedures }
          { must be overloaded                                  }
          procedure write_init_data;virtual;
          procedure write_child_init_data;virtual;
          { rtti }
          procedure write_rtti_name;
          function  get_rtti_label : string;virtual;
          procedure generate_rtti;virtual;
          procedure write_rtti_data;virtual;
          procedure write_child_rtti_data;virtual;
          function is_intregable : boolean;
          function is_fpuregable : boolean;
       end;

       targconvtyp = (act_convertable,act_equal,act_exact);

       tvarspez = (vs_value,vs_const,vs_var);

       pparaitem = ^tparaitem;
       tparaitem = object(tlinkedlist_item)
          convertlevel : byte;
          argconvtyp   : targconvtyp;
          paratype     : ttype;
          paratyp      : tvarspez;
          register     : tregister;
          parasym      : psym;
       end;

       tfiletyp = (ft_text,ft_typed,ft_untyped);

       pfiledef = ^tfiledef;
       tfiledef = object(tdef)
          filetyp : tfiletyp;
          typedfiletype : ttype;
          constructor inittext;
          constructor inituntyped;
          constructor inittyped(const tt : ttype);
          constructor inittypeddef(p : pdef);
          constructor load;
          procedure write;virtual;
          procedure deref;virtual;
          function  gettypename:string;virtual;
          procedure setsize;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

       pformaldef = ^tformaldef;
       tformaldef = object(tdef)
          constructor init;
          constructor load;
          procedure write;virtual;
          function  gettypename:string;virtual;
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

       pforwarddef = ^tforwarddef;
       tforwarddef = object(tdef)
          tosymname : string;
          forwardpos : tfileposinfo;
          constructor init(const s:string;const pos : tfileposinfo);
          function  gettypename:string;virtual;
       end;

       perrordef = ^terrordef;
       terrordef = object(tdef)
          constructor init;
          function  gettypename:string;virtual;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;virtual;
{$endif GDB}
       end;

       { tpointerdef and tclassrefdef should get a common
         base class, but I derived tclassrefdef from tpointerdef
         to avoid problems with bugs (FK)
       }

       ppointerdef = ^tpointerdef;
       tpointerdef = object(tdef)
          is_far : boolean;
          pointertype : ttype;
          constructor init(const tt : ttype);
          constructor initfar(const tt : ttype);
          constructor initdef(p : pdef);
          constructor initfardef(p : pdef);
          constructor load;
          procedure write;virtual;
          procedure deref;virtual;
          function  gettypename:string;virtual;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

       pprocdef = ^tprocdef;

       pobjectdef = ^tobjectdef;
       tobjectdef = object(tdef)
          childof  : pobjectdef;
          objname  : pstring;
          symtable : psymtable;
          objectoptions : tobjectoptions;
          { to be able to have a variable vmt position }
          { and no vmt field for objects without virtuals }
          vmt_offset : longint;
{$ifdef GDB}
          writing_class_record_stab : boolean;
{$endif GDB}
          constructor init(const n : string;c : pobjectdef);
          constructor load;
          destructor  done;virtual;
          procedure write;virtual;
          procedure deref;virtual;
          function  size : longint;virtual;
          function  alignment:longint;virtual;
          function  vmtmethodoffset(index:longint):longint;
          function  is_publishable : boolean;virtual;
          function  vmt_mangledname : string;
          function  rtti_name : string;
          procedure check_forwards;
          function  is_related(d : pobjectdef) : boolean;
          function  is_class : boolean;
          function  next_free_name_index : longint;
          procedure insertvmt;
          procedure set_parent(c : pobjectdef);
          function searchdestructor : pprocdef;
          { debug }
{$ifdef GDB}
          function stabstring : pchar;virtual;
          procedure set_globalnb;virtual;
          function  classnumberstring : string;
          procedure concatstabto(asmlist : paasmoutput);virtual;
          function  allstabstring : pchar;virtual;
{$endif GDB}
          { init/final }
          function  needs_inittable : boolean;virtual;
          procedure write_init_data;virtual;
          procedure write_child_init_data;virtual;
          { rtti }
          function  get_rtti_label : string;virtual;
          procedure generate_rtti;virtual;
          procedure write_rtti_data;virtual;
          procedure write_child_rtti_data;virtual;
          function generate_field_table : pasmlabel;
       end;

       pclassrefdef = ^tclassrefdef;
       tclassrefdef = object(tpointerdef)
          constructor init(def : pdef);
          constructor load;
          procedure write;virtual;
          function gettypename:string;virtual;
          { debug }
{$ifdef GDB}
          function stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

       parraydef = ^tarraydef;
       tarraydef = object(tdef)
          IsVariant,
          IsConstructor: boolean;
       private
          rangenr    : longint;
       public
          lowrange,
          highrange  : longint;
          elementtype,
          rangetype  : ttype;
          IsArrayOfConst : boolean;
          function gettypename:string;virtual;
          function elesize : longint;
          constructor init(l,h : longint;rd : pdef);
          constructor load;
          procedure write;virtual;
{$ifdef GDB}
          function stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
          procedure deref;virtual;
          function size : longint;virtual;
          function alignment : longint;virtual;
          procedure setelementtype(t:ttype);
          { generates the ranges needed by the asm instruction BOUND (i386)
            or CMP2 (Motorola) }
          procedure genrangecheck;

          { returns the label of the range check string }
          function getrangecheckstring : string;
          function needs_inittable : boolean;virtual;
          procedure write_rtti_data;virtual;
          procedure write_child_rtti_data;virtual;
       end;

       precorddef = ^trecorddef;
       trecorddef = object(tdef)
          symtable : psymtable;
          constructor init(p : psymtable);
          constructor load;
          destructor done;virtual;
          procedure write;virtual;
          procedure deref;virtual;
          function  size:longint;virtual;
          function  alignment : longint;virtual;
          function  gettypename:string;virtual;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
          { init/final }
          procedure write_init_data;virtual;
          procedure write_child_init_data;virtual;
          function  needs_inittable : boolean;virtual;
          { rtti }
          procedure write_rtti_data;virtual;
          procedure write_child_rtti_data;virtual;
       end;

       porddef = ^torddef;
       torddef = object(tdef)
          typ      : tbasetype;
        private
          rangenr  : longint;
        public
          low,high : longint;
          constructor init(t : tbasetype;v,b : longint);
          constructor load;
          procedure write;virtual;
          function  is_publishable : boolean;virtual;
          function  gettypename:string;virtual;
          procedure setsize;
          function get_low : int64;
          function get_high : int64;

          { generates the ranges needed by the asm instruction BOUND }
          { or CMP2 (Motorola)                                       }
          procedure genrangecheck;
          function  getrangecheckstring : string;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;virtual;
{$endif GDB}
          { rtti }
          procedure write_rtti_data;virtual;
       end;

       pfloatdef = ^tfloatdef;
       tfloatdef = object(tdef)
          typ : tfloattype;
          constructor init(t : tfloattype);
          constructor load;
          procedure write;virtual;
          function  gettypename:string;virtual;
          function  is_publishable : boolean;virtual;
          procedure setsize;
          { debug }
{$ifdef GDB}
          function stabstring : pchar;virtual;
{$endif GDB}
          { rtti }
          procedure write_rtti_data;virtual;
       end;

       pabstractprocdef = ^tabstractprocdef;
       tabstractprocdef = object(tdef)
          { saves a definition to the return type }
          symtablelevel   : byte;
          fpu_used        : byte;    { how many stack fpu must be empty }
          rettype         : ttype;
          proctypeoption  : tproctypeoption;
          proccalloptions : tproccalloptions;
          procoptions     : tprocoptions;
          para            : plinkedlist;
          selfpara        : pparaitem;
          constructor init;
          constructor load;
          destructor done;virtual;
          procedure  write;virtual;
          procedure deref;virtual;
          function  concatpara(vs:psym;tt:ttype;vsp : tvarspez):pparaitem;
          function  para_size(alignsize:longint) : longint;
          function  demangled_paras : string;
          function  proccalloption2str : string;
          function  is_methodpointer:boolean;virtual;
          function  is_addressonly:boolean;virtual;
          procedure test_if_fpu_result;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

       pprocvardef = ^tprocvardef;
       tprocvardef = object(tabstractprocdef)
          constructor init;
          constructor load;
          procedure write;virtual;
          function  size : longint;virtual;
          function gettypename:string;virtual;
          function is_publishable : boolean;virtual;
          function  is_methodpointer:boolean;virtual;
          function  is_addressonly:boolean;virtual;
          { debug }
{$ifdef GDB}
          function stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput); virtual;
{$endif GDB}
          { rtti }
          procedure write_child_rtti_data;virtual;
          procedure write_rtti_data;virtual;
       end;

       tmessageinf = record
         case integer of
           0 : (str : pchar);
           1 : (i : longint);
       end;

       tprocdef = object(tabstractprocdef)
       private
          _mangledname : pchar;
       public
          { (forward procedure) }
          forwarddef,
          { true if the procedure is declared in the interface }
          interfacedef : boolean;

          extnumber  : longint;
          messageinf : tmessageinf;
          nextoverloaded : pprocdef;
{$ifndef EXTDEBUG}
          { where is this function defined, needed here because there
            is only one symbol for all overloaded functions
            EXTDEBUG has fileinfo in tdef (PFV) }
          fileinfo : tfileposinfo;
{$endif}
          { pointer to the local symbol table }
          localst : psymtable;
          { pointer to the parameter symbol table }
          parast : psymtable;
          { symbol owning this definition }
          procsym : pprocsym;
          { browser info }
          lastref,
          defref,
          crossref,
          lastwritten : pref;
          refcount : longint;
          _class : pobjectdef;
          { it's a tree, but this not easy to handle }
          { used for inlined procs                   }
          code : pointer;
{$ifdef newcg}
          usedregisters : tregisterset;
{$else newcg}
{$ifdef i386}
          usedregisters : longint;
{$else}
          usedregisters : tregisterset;
{$endif}
{$endif newcg}
          { true, if the procedure is only declared }
          { true if the procedure has a forward declaration }
          hasforward : boolean;
          { check the problems of manglednames }
          count      : boolean;
          is_used    : boolean;
          { small set which contains the modified registers }
          constructor init;
          constructor load;
          destructor  done;virtual;
          procedure write;virtual;
          procedure deref;virtual;
          function  haspara:boolean;
          function  mangledname : string;
          procedure setmangledname(const s : string);
          procedure load_references;
          function  write_references : boolean;
          function  procname: string;
          function  is_methodpointer:boolean;virtual;
          function  is_addressonly:boolean;virtual;
          { debug }
{$ifdef GDB}
          function  cplusplusmangledname : string;
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
          { browser }
{$ifdef BrowserLog}
          procedure add_to_browserlog;
{$endif BrowserLog}
       end;

       pstringdef = ^tstringdef;
       tstringdef = object(tdef)
          string_typ : tstringtype;
          len        : longint;
          constructor shortinit(l : byte);
          constructor shortload;
          constructor longinit(l : longint);
          constructor longload;
          constructor ansiinit(l : longint);
          constructor ansiload;
          constructor wideinit(l : longint);
          constructor wideload;
          function  stringtypname:string;
          function  size : longint;virtual;
          procedure write;virtual;
          function  gettypename:string;virtual;
          function  is_publishable : boolean;virtual;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
          { init/final }
          function  needs_inittable : boolean;virtual;
          { rtti }
          procedure write_rtti_data;virtual;
       end;

       penumdef = ^tenumdef;
       tenumdef = object(tdef)
          has_jumps : boolean;
          rangenr,
          minval,
          maxval    : longint;
          firstenum : penumsym;
          basedef   : penumdef;
          constructor init;
          constructor init_subrange(_basedef:penumdef;_min,_max:longint);
          constructor load;
          destructor done;virtual;
          procedure write;virtual;
          procedure deref;virtual;
          function  gettypename:string;virtual;
          function  is_publishable : boolean;virtual;
          procedure calcsavesize;
          procedure setmax(_max:longint);
          procedure setmin(_min:longint);
          function  min:longint;
          function  max:longint;
          function  getrangecheckstring:string;
          procedure genrangecheck;
          { debug }
{$ifdef GDB}
          function stabstring : pchar;virtual;
{$endif GDB}
          { rtti }
          procedure write_child_rtti_data;virtual;
          procedure write_rtti_data;virtual;
       end;

       psetdef = ^tsetdef;
       tsetdef = object(tdef)
          settype : tsettype;
          elementtype : ttype;
          constructor init(s : pdef;high : longint);
          constructor load;
          destructor  done;virtual;
          procedure write;virtual;
          procedure deref;virtual;
          function  gettypename:string;virtual;
          function  is_publishable : boolean;virtual;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
          { rtti }
          procedure write_rtti_data;virtual;
          procedure write_child_rtti_data;virtual;
       end;

{
  $Log: symdefh.inc,v $
  Revision 1.1.2.13  2003/01/15 01:42:27  peter
    * fix po_addressonly check

  Revision 1.1.2.12  2003/01/15 00:35:42  peter
    * methodpointer check is simpler
    * also check po_addressonly when comparing methodpointers
    * use address of instead of procedure variable of for po_addresonly

  Revision 1.1.2.11  2003/01/07 19:21:53  peter
    * allow self as parameter for non method functions/procvars
    * aktprocsym creation simplified and support hidding of unitname

  Revision 1.1.2.10  2003/01/05 16:25:27  peter
    * check for array size when defining an array type

  Revision 1.1.2.9  2002/11/27 17:04:31  pierre
   + two new method tordef.get_low and get_high returning int64 values

  Revision 1.1.2.8  2001/04/19 11:35:25  carl
  * m68k updates

  Revision 1.1.2.7  2001/04/13 01:23:32  peter
    * include fileinfo in definition when extdebug is set

  Revision 1.1.2.6  2000/10/16 19:43:03  pierre
   * trying to correct class stabss once more

  Revision 1.1.2.5  2000/10/13 12:08:43  peter
    * class stabsnumbering fixed partly

  Revision 1.1.2.4  2000/09/19 22:26:28  pierre
   * fix for the local class debugging problem

  Revision 1.1.2.3  2000/08/18 12:56:32  pierre
   * avoid infinite recursion in stabs generation

  Revision 1.1.2.2  2000/08/07 12:32:46  jonas
    * reordered severam object fields to make optimal use of memory (avoid
      alignment padding)

  Revision 1.1.2.1  2000/08/06 14:14:09  peter
    * hasforward added to check if a proc had a forward decl

  Revision 1.1  2000/07/13 06:29:57  michael
  + Initial import

  Revision 1.54  2000/06/02 18:48:48  florian
    + fieldtable support for classes

  Revision 1.53  2000/02/09 13:23:04  peter
    * log truncated

  Revision 1.52  2000/02/04 20:00:22  florian
    * an exception in a construcor calls now the destructor (this applies only
      to classes)

  Revision 1.51  2000/01/26 12:02:30  peter
    * abstractprocdef.para_size needs alignment parameter
    * secondcallparan gets para_alignment size instead of dword_align

  Revision 1.50  2000/01/07 01:14:40  peter
    * updated copyright to 2000

  Revision 1.49  2000/01/03 19:26:04  peter
    * fixed resolving of ttypesym which are reference from object/record
      fields.

  Revision 1.48  1999/11/30 10:40:55  peter
    + ttype, tsymlist

  Revision 1.47  1999/11/17 17:05:04  pierre
   * Notes/hints changes

  Revision 1.46  1999/11/09 23:35:50  pierre
   + better reference pos for forward defs

  Revision 1.45  1999/11/06 14:34:27  peter
    * truncated log to 20 revs

  Revision 1.44  1999/10/26 12:30:45  peter
    * const parameter is now checked
    * better and generic check if a node can be used for assigning
    * export fixes
    * procvar equal works now (it never had worked at least from 0.99.8)
    * defcoll changed to linkedlist with pparaitem so it can easily be
      walked both directions

  Revision 1.43  1999/10/01 10:05:44  peter
    + procedure directive support in const declarations, fixes bug 232

  Revision 1.42  1999/10/01 08:02:48  peter
    * forward type declaration rewritten

  Revision 1.41  1999/08/10 12:34:49  pierre
   + procsym field in tprocdef to allow correct gdb info generation

  Revision 1.40  1999/08/09 22:19:57  peter
    * classes vmt changed to only positive addresses
    * sharedlib creation is working

  Revision 1.39  1999/08/07 14:21:02  florian
    * some small problems fixed

  Revision 1.38  1999/08/05 16:53:15  peter
    * V_Fatal=1, all other V_ are also increased
    * Check for local procedure when assigning procvar
    * fixed comment parsing because directives
    * oldtp mode directives better supported
    * added some messages to errore.msg

  Revision 1.37  1999/08/03 22:03:16  peter
    * moved bitmask constants to sets
    * some other type/const renamings

  Revision 1.36  1999/08/02 21:29:04  florian
    * the main branch psub.pas is now used for
      newcg compiler

  Revision 1.35  1999/07/27 23:42:20  peter
    * indirect type referencing is now allowed

  Revision 1.34  1999/07/23 16:05:30  peter
    * alignment is now saved in the symtable
    * C alignment added for records
    * PPU version increased to solve .12 <-> .13 probs

}
