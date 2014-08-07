{
    $Id: symconst.pas,v 1.1.2.5 2003/01/06 21:19:08 peter Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl, Pierre Muller

    Symbol table constants

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
unit symconst;
interface

{$ifdef FPC}
  {$ifdef PACKENUMFIXED}
    {$PACKENUM 1}
  {$endif}
{$endif}

const
  def_alignment = 4;

type
  { symbol options }
  tsymoption=(sp_none,
    sp_public,
    sp_private,
    sp_published,
    sp_protected,
    sp_static,
    sp_primary_typesym    { this is for typesym, to know who is the primary symbol of a def }
    ,sp_7
    ,sp_8
    ,sp_9
    ,sp_10
    ,sp_11
    ,sp_12
    ,sp_13
    ,sp_14
    ,sp_15
    ,sp_16
    ,sp_17
    ,sp_18
    ,sp_19
    ,sp_20
    ,sp_21
    ,sp_22
    ,sp_23
    ,sp_24
  );
  tsymoptions=set of tsymoption;

  { flags for a definition }
  tdefoption=(df_none,
    df_need_rtti,          { the definitions needs rtti }
    df_has_rtti            { the rtti is generated      }
    ,df_3
    ,df_4
    ,df_5
    ,df_6
    ,df_7
    ,df_8
    ,df_9
    ,df_10
    ,df_11
    ,df_12
    ,df_13
    ,df_14
    ,df_15
    ,df_16
    ,df_17
    ,df_18
    ,df_19
    ,df_20
    ,df_21
    ,df_22
    ,df_23
    ,df_24
  );
  tdefoptions=set of tdefoption;

  { base types for orddef }
  tbasetype = (
    uauto,uvoid,uchar,
    u8bit,u16bit,u32bit,
    s8bit,s16bit,s32bit,
    bool8bit,bool16bit,bool32bit,
    u64bit,s64bit,uwidechar
  );

  { float types }
  tfloattype = (
    s32real,s64real,s80real,
    s64comp,
    f16bit,f32bit
  );

  { string types }
  tstringtype = (st_default,
    st_shortstring, st_longstring, st_ansistring, st_widestring
  );

  { set types }
  tsettype = (
    normset,smallset,varset
  );

  { calling convention for tprocdef and tprocvardef }
  tproccalloption=(pocall_none,
    pocall_clearstack,    { Use IBM flat calling convention. (Used by GCC.) }
    pocall_leftright,     { Push parameters from left to right }
    pocall_cdecl,         { procedure uses C styled calling }
    pocall_register,      { procedure uses register (fastcall) calling }
    pocall_stdcall,       { procedure uses stdcall call }
    pocall_safecall,      { safe call calling conventions }
    pocall_palmossyscall, { procedure is a PalmOS system call }
    pocall_system,
    pocall_inline,        { Procedure is an assembler macro }
    pocall_internproc,    { Procedure has compiler magic}
    pocall_internconst    { procedure has constant evaluator intern }
    ,pocall_12
    ,pocall_13
    ,pocall_14
    ,pocall_15
    ,pocall_16
    ,pocall_17
    ,pocall_18
    ,pocall_19
    ,pocall_20
    ,pocall_21
    ,pocall_22
    ,pocall_23
    ,pocall_24
  );
  tproccalloptions=set of tproccalloption;

  { basic type for tprocdef and tprocvardef }
  tproctypeoption=(potype_none,
    potype_proginit,     { Program initialization }
    potype_unitinit,     { unit initialization }
    potype_unitfinalize, { unit finalization }
    potype_constructor,  { Procedure is a constructor }
    potype_destructor,   { Procedure is a destructor }
    potype_operator      { Procedure defines an operator }
    ,potype_7
    ,potype_8
    ,potype_9
    ,potype_10
    ,potype_11
    ,potype_12
    ,potype_13
    ,potype_14
    ,potype_15
    ,potype_16
    ,potype_17
    ,potype_18
    ,potype_19
    ,potype_20
    ,potype_21
    ,potype_22
    ,potype_23
    ,potype_24
  );
  tproctypeoptions=set of tproctypeoption;

  { other options for tprocdef and tprocvardef }
  tprocoption=(po_none,
    po_classmethod,       { class method }
    po_virtualmethod,     { Procedure is a virtual method }
    po_abstractmethod,    { Procedure is an abstract method }
    po_staticmethod,      { static method }
    po_overridingmethod,  { method with override directive }
    po_methodpointer,     { method pointer, only in procvardef, also used for 'with object do' }
    po_containsself,      { self is passed explicit to the compiler }
    po_interrupt,         { Procedure is an interrupt handler }
    po_iocheck,           { IO checking should be done after a call to the procedure }
    po_assembler,         { Procedure is written in assembler }
    po_msgstr,            { method for string message handling }
    po_msgint,            { method for int message handling }
    po_exports,           { Procedure has export directive (needed for OS/2) }
    po_external,          { Procedure is external (in other object or lib)}
    po_savestdregs,       { save std regs cdecl and stdcall need that ! }
    po_saveregisters,     { save all registers }
    po_overload,          { procedure is declared with overload directive }
    po_addressonly        { flag that only the address of a method is returned and not a full methodpointer }
    ,po_19
    ,po_20
    ,po_21
    ,po_22
    ,po_23
    ,po_24
  );
  tprocoptions=set of tprocoption;

  { options for objects and classes }
  tobjectoption=(oo_none,
    oo_is_class,
    oo_is_forward,         { the class is only a forward declared yet }
    oo_has_virtual,        { the object/class has virtual methods }
    oo_has_private,
    oo_has_protected,
    oo_has_constructor,    { the object/class has a constructor }
    oo_has_destructor,     { the object/class has a destructor }
    oo_has_vmt,            { the object/class has a vmt }
    oo_has_msgstr,
    oo_has_msgint,
    oo_has_abstract,       { the object/class has an abstract method => no instances can be created }
    oo_can_have_published, { the class has rtti, i.e. you can publish properties }
    oo_cpp_class,          { the object/class uses an C++ compatible }
                           { class layout }
    oo_interface           { delphi styled interface }
    ,oo_15
    ,oo_16
    ,oo_17
    ,oo_18
    ,oo_19
    ,oo_20
    ,oo_21
    ,oo_22
    ,oo_23
    ,oo_24
  );

  tobjectoptions=set of tobjectoption;

  { options for properties }
  tpropertyoption=(ppo_none,
    ppo_indexed,
    ppo_defaultproperty,
    ppo_stored,
    ppo_hasparameters,
    ppo_is_override
    ,ppo_6
    ,ppo_7
    ,ppo_8
    ,ppo_9
    ,ppo_10
    ,ppo_11
    ,ppo_12
    ,ppo_13
    ,ppo_14
    ,ppo_15
    ,ppo_16
    ,ppo_17
    ,ppo_18
    ,ppo_19
    ,ppo_20
    ,ppo_21
    ,ppo_22
    ,ppo_23
    ,ppo_24
  );

  tpropertyoptions=set of tpropertyoption;

  { options for variables }
  tvaroption=(vo_none,
    vo_regable,
    vo_is_C_var,
    vo_is_external,
    vo_is_dll_var,
    vo_is_thread_var,
    vo_fpuregable,
    vo_is_local_copy,
    vo_is_const,  { variable is declared as const (parameter) and can't be written to }
    vo_is_exported
    ,vo_10
    ,vo_11
    ,vo_12
    ,vo_13
    ,vo_14
    ,vo_15
    ,vo_16
    ,vo_17
    ,vo_18
    ,vo_19
    ,vo_20
    ,vo_21
    ,vo_22
    ,vo_23
    ,vo_24
  );
  tvaroptions=set of tvaroption;

  { definition contains the informations about a type }
  tdeftype = (abstractdef,arraydef,recorddef,pointerdef,orddef,
              stringdef,enumdef,procdef,objectdef,errordef,
              filedef,formaldef,setdef,procvardef,floatdef,
              classrefdef,forwarddef);

  { possible types for symtable entries }
  tsymtyp = (abstractsym,varsym,typesym,procsym,unitsym,programsym,
             constsym,enumsym,typedconstsym,errorsym,syssym,
             labelsym,absolutesym,propertysym,funcretsym,
             macrosym);

  { State of the variable, if it's declared, assigned or used }
  tvarstate=(vs_none,
    vs_declared,vs_declared_and_first_found,
    vs_set_but_first_not_passed,vs_assigned,vs_used
  );

  absolutetyp = (tovar,toasm,toaddr);

  tconsttyp = (constnone,
    constord,conststring,constreal,constbool,
    constint,constchar,constset,constpointer,constnil,
    constresourcestring
  );

{$ifdef GDB}
  tdefstabstatus = (
    not_written,
    being_written,
    written);
{$endif GDB}

const
  { relevant options for assigning a proc or a procvar to a procvar }
  po_compatibility_options = [
    po_classmethod,
    po_staticmethod,
    po_methodpointer,
    po_containsself,
    po_interrupt,
    po_iocheck,
    po_exports
  ];

const
     SymTypeName : array[tsymtyp] of string[12] =
     ('abstractsym','variable','type','proc','unit','program',
      'const','enum','typed const','errorsym','system sym',
      'label','absolute','property','funcret',
      'macrosym');

implementation

end.
{
  $Log: symconst.pas,v $
  Revision 1.1.2.5  2003/01/06 21:19:08  peter
    * po_address to get the address of a method without selfpointer

  Revision 1.1.2.4  2001/02/26 19:46:24  peter
    * removed $ifdef tp. So the sets are equal for all compilers

  Revision 1.1.2.3  2001/02/25 01:27:53  carl
  problems with set sizes in tp mode fixed

  Revision 1.1.2.2  2000/08/18 12:56:31  pierre
   * avoid infinite recursion in stabs generation

  Revision 1.1.2.1  2000/08/05 13:22:30  peter
    * packenum 1 when packenumfixed is defined

  Revision 1.1  2000/07/13 06:29:56  michael
  + Initial import

  Revision 1.13  2000/06/18 18:11:32  peter
    * C record packing fixed to also check first entry of the record
      if bigger than the recordalignment itself
    * variant record alignment uses alignment per variant and saves the
      highest alignment value

  Revision 1.12  2000/06/02 21:15:49  pierre
   + vo_is_exported for bug0317 fix

  Revision 1.11  2000/03/19 14:56:38  florian
    * bug 873 fixed
    * some cleanup in objectdec

  Revision 1.10  2000/01/09 23:16:06  peter
    * added st_default stringtype
    * genstringconstnode extended with stringtype parameter using st_default
      will do the old behaviour

  Revision 1.9  2000/01/07 01:14:39  peter
    * updated copyright to 2000

  Revision 1.8  1999/12/18 14:55:21  florian
    * very basic widestring support

  Revision 1.7  1999/11/30 10:40:54  peter
    + ttype, tsymlist

  Revision 1.6  1999/11/17 17:05:04  pierre
   * Notes/hints changes

  Revision 1.5  1999/11/07 23:16:49  florian
    * finally bug 517 solved ...

  Revision 1.4  1999/10/26 12:30:45  peter
    * const parameter is now checked
    * better and generic check if a node can be used for assigning
    * export fixes
    * procvar equal works now (it never had worked at least from 0.99.8)
    * defcoll changed to linkedlist with pparaitem so it can easily be
      walked both directions

  Revision 1.3  1999/10/01 08:02:48  peter
    * forward type declaration rewritten

  Revision 1.2  1999/08/04 13:45:29  florian
    + floating point register variables !!
    * pairegalloc is now generated for register variables

  Revision 1.1  1999/08/03 22:03:14  peter
    * moved bitmask constants to sets
    * some other type/const renamings

}
