{
    $Id: cpuasm.pas,v 1.1.2.12 2002/11/21 10:27:26 pierre Exp $
    Copyright (c) 1998-2001 by Florian Klaempfl and Pierre Muller

    m68k family assembler instructions

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
unit cpuasm;
interface

uses
  cobjects,
  aasm,globals,verbose,
  cpubase;

const
  MaxPrefixes=4;

type

  Alloc_type  = (at_alloc,at_free,at_savetostack,at_retrievefromstack);

  pairegalloc = ^tairegalloc;
  tairegalloc = object(tai)
     allocation : Alloc_type;
     reg        : tregister;
     constructor alloc(r : tregister);
     constructor dealloc(r : tregister);
     constructor tostack(r : tregister);
     constructor fromstack(r : tregister);
  end;

  paicpu = ^taicpu;
  taicpu = object(tai)
     opsize    : topsize;
     is_jmp    : boolean; { is this instruction a jump? (needed for optimizer) }
     ops       : longint;
     oper      : array[0..2] of toper;
     opcode    : tasmop;
     {condition : TAsmCond;}
     constructor op_none(op : tasmop;_size : topsize);

     constructor op_reg(op : tasmop;_size : topsize;_op1 : tregister);
     constructor op_const(op : tasmop;_size : topsize;_op1 : longint);
     constructor op_ref(op : tasmop;_size : topsize;_op1 : preference);

     constructor op_reg_reg(op : tasmop;_size : topsize;_op1,_op2 : tregister);
     constructor op_reg_ref(op : tasmop;_size : topsize;_op1 : tregister;_op2 : preference);
     constructor op_reg_const(op:tasmop; _size: topsize; _op1: tregister; _op2: longint);

     constructor op_const_reg(op : tasmop;_size : topsize;_op1 : longint;_op2 : tregister);
     constructor op_const_const(op : tasmop;_size : topsize;_op1,_op2 : longint);
     constructor op_const_ref(op : tasmop;_size : topsize;_op1 : longint;_op2 : preference);

     constructor op_ref_reg(op : tasmop;_size : topsize;_op1 : preference;_op2 : tregister);
     { this is only allowed if _op1 is an int value (_op1^.isintvalue=true) }
     constructor op_ref_ref(op : tasmop;_size : topsize;_op1,_op2 : preference);

     constructor op_reg_reg_reg(op : tasmop;_size : topsize;_op1,_op2,_op3 : tregister);
     constructor op_const_reg_reg(op : tasmop;_size : topsize;_op1 : longint;_op2 : tregister;_op3 : tregister);
     constructor op_const_ref_reg(op : tasmop;_size : topsize;_op1 : longint;_op2 : preference;_op3 : tregister);
     constructor op_reg_reg_ref(op : tasmop;_size : topsize;_op1,_op2 : tregister; _op3 : preference);
     constructor op_const_reg_ref(op : tasmop;_size : topsize;_op1 : longint;_op2 : tregister;_op3 : preference);

     constructor op_reg_reglist(op: tasmop; _size : topsize; _op1: tregister;_op2: tregisterlist);
     constructor op_reglist_reg(op: tasmop; _size : topsize; _op1: tregisterlist; _op2: tregister);

     constructor op_ref_reglist(op: tasmop; _size : topsize; _op1: preference;_op2: tregisterlist);
     constructor op_reglist_ref(op: tasmop; _size : topsize; _op1: tregisterlist; _op2: preference);

     { this is for Jmp instructions }
     {constructor op_cond_sym(op : tasmop;cond:TAsmCond;_size : topsize;_op1 : pasmsymbol);}

     constructor op_sym(op : tasmop;_size : topsize;_op1 : pasmsymbol);
     constructor op_sym_ofs(op : tasmop;_size : topsize;_op1 : pasmsymbol;_op1ofs:longint);
     constructor op_sym_ofs_reg(op : tasmop;_size : topsize;_op1 : pasmsymbol;_op1ofs:longint;_op2 : tregister);
     constructor op_sym_ofs_ref(op : tasmop;_size : topsize;_op1 : pasmsymbol;_op1ofs:longint;_op2 : preference);

     procedure loadconst(opidx:longint;l:longint);
     procedure loadsymbol(opidx:longint;s:pasmsymbol;sofs:longint);
     procedure loadref(opidx:longint;p:preference);
     procedure loadreg(opidx:longint;r:tregister);
    procedure loadreglist(opidx:longint;r:pregisterlist);
     procedure loadoper(opidx:longint;o:toper);

     destructor done;virtual;
     function  getcopy:plinkedlist_item;virtual;
     function  GetString:string;
  private
     segprefix : tregister;
     procedure init(op : tasmop;_size : topsize); { this need to be called by all constructor }
  end;


{*****************************************************************************
                                Labeled instruction
*****************************************************************************}

    pai_labeled = ^tai_labeled;
    tai_labeled = object(tai)
      opcode : tasmop;
      register : tregister;
      lab : pasmlabel;
      sym : pasmsymbol;
      constructor init(op : tasmop; l : pasmlabel);
      constructor init_sym(op : tasmop; asym : pasmsymbol);
      constructor init_reg(op: tasmop; l : pasmlabel; reg: tregister);
      constructor init_reg_sym(op : tasmop; asym: pasmsymbol; reg :tregister);
      destructor done;virtual;
    end;


implementation

{*****************************************************************************
                                 TaiRegAlloc
*****************************************************************************}

    constructor tairegalloc.alloc(r : tregister);
      begin
        inherited init;
        typ:=ait_regalloc;
        allocation:=at_alloc;
        reg:=r;
      end;

    constructor tairegalloc.tostack(r : tregister);
      begin
        inherited init;
        typ:=ait_regalloc;
        allocation:=at_savetostack;
        reg:=r;
      end;

    constructor tairegalloc.fromstack(r : tregister);
      begin
        inherited init;
        typ:=ait_regalloc;
        allocation:=at_retrievefromstack;
        reg:=r;
      end;

    constructor tairegalloc.dealloc(r : tregister);
      begin
        inherited init;
        typ:=ait_regalloc;
        allocation:=at_free;
        reg:=r;
      end;


{*****************************************************************************
                                 Taicpu Constructors
*****************************************************************************}

    procedure taicpu.loadconst(opidx:longint;l:longint);
      begin
        if opidx>=ops then
         ops:=opidx+1;
        with oper[opidx] do
         begin
           if typ=top_ref then
            disposereference(ref);
           val:=l;
           typ:=top_const;
         end;
      end;


    procedure taicpu.loadsymbol(opidx:longint;s:pasmsymbol;sofs:longint);
      begin
        if opidx>=ops then
         ops:=opidx+1;
        with oper[opidx] do
         begin
           if typ=top_ref then
            disposereference(ref);
           sym:=s;
           symofs:=sofs;
           typ:=top_symbol;
         end;
        { Mark the symbol as used }
        if assigned(s) then
         s^.increfs;
      end;


    procedure taicpu.loadref(opidx:longint;p:preference);
      begin
        if opidx>=ops then
         ops:=opidx+1;
        with oper[opidx] do
         begin
           if typ=top_ref then
            disposereference(ref);
           if p^.is_immediate then
             begin
{$ifdef ASMDEBUG1}
               Comment(V_Warning,'Reference immediate');
{$endif}
               val:=p^.offset;
               disposereference(p);
               typ:=top_const;
             end
           else
             begin
               ref:=p;
               if not(ref^.segment in [R_NO]) then
                 segprefix:=ref^.segment;
               typ:=top_ref;
               { mark symbol as used }
               if assigned(ref^.symbol) then
                 ref^.symbol^.increfs;
             end;
         end;
      end;


    procedure taicpu.loadreg(opidx:longint;r:tregister);
      begin
        if opidx>=ops then
         ops:=opidx+1;
        with oper[opidx] do
         begin
           if typ=top_ref then
            disposereference(ref);
           reg:=r;
           typ:=top_reg;
         end;
      end;

   procedure taicpu.loadreglist(opidx:longint;r:pregisterlist);
      begin
        if opidx>=ops then
         ops:=opidx+1;
        with oper[opidx] do
         begin
           if typ=top_ref then
            disposereference(ref);
           registerlist:=r;
           typ:=top_reglist;
         end;
      end;

    procedure taicpu.loadoper(opidx:longint;o:toper);
      begin
        if opidx>=ops then
         ops:=opidx+1;
        if oper[opidx].typ=top_ref then
          disposereference(oper[opidx].ref);
        oper[opidx]:=o;
        { copy also the reference }
        if oper[opidx].typ=top_ref then
         oper[opidx].ref:=newreference(o.ref^);
      end;




    procedure taicpu.init(op : tasmop;_size : topsize);
      begin
         typ:=ait_instruction;
         is_jmp:=false;
         segprefix:=R_NO;
         opcode:=op;
         opsize:=_size;
         ops:=0;
         fillchar(oper,sizeof(oper),0);
      end;


    constructor taicpu.op_none(op : tasmop;_size : topsize);
      begin
         inherited init;
         init(op,_size);
      end;


    constructor taicpu.op_reg(op : tasmop;_size : topsize;_op1 : tregister);
      begin
         inherited init;
         init(op,_size);
         ops:=1;
         loadreg(0,_op1);
      end;


    constructor taicpu.op_const(op : tasmop;_size : topsize;_op1 : longint);
      begin
         inherited init;
         init(op,_size);
         ops:=1;
         loadconst(0,_op1);
      end;


    constructor taicpu.op_ref(op : tasmop;_size : topsize;_op1 : preference);
      begin
         inherited init;
         init(op,_size);
         ops:=1;
         loadref(0,_op1);
      end;


    constructor taicpu.op_reg_reg(op : tasmop;_size : topsize;_op1,_op2 : tregister);
      begin
         inherited init;
         init(op,_size);
         ops:=2;
         loadreg(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_reg_const(op:tasmop; _size: topsize; _op1: tregister; _op2: longint);
      begin
         inherited init;
         init(op,_size);
         ops:=2;
         loadreg(0,_op1);
         loadconst(1,_op2);
      end;


    constructor taicpu.op_reg_ref(op : tasmop;_size : topsize;_op1 : tregister;_op2 : preference);
      begin
         inherited init;
         init(op,_size);
         ops:=2;
         loadreg(0,_op1);
         loadref(1,_op2);
      end;


    constructor taicpu.op_const_reg(op : tasmop;_size : topsize;_op1 : longint;_op2 : tregister);
      begin
         inherited init;
         init(op,_size);
         ops:=2;
         loadconst(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_const_const(op : tasmop;_size : topsize;_op1,_op2 : longint);
      begin
         inherited init;
         init(op,_size);
         ops:=2;
         loadconst(0,_op1);
         loadconst(1,_op2);
      end;


    constructor taicpu.op_const_ref(op : tasmop;_size : topsize;_op1 : longint;_op2 : preference);
      begin
         inherited init;
         init(op,_size);
         ops:=2;
         loadconst(0,_op1);
         loadref(1,_op2);
      end;


    constructor taicpu.op_ref_reg(op : tasmop;_size : topsize;_op1 : preference;_op2 : tregister);
      begin
         inherited init;
         init(op,_size);
         ops:=2;
         loadref(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_ref_ref(op : tasmop;_size : topsize;_op1,_op2 : preference);
      begin
         inherited init;
         init(op,_size);
         ops:=2;
         loadref(0,_op1);
         loadref(1,_op2);
      end;


    constructor taicpu.op_reg_reg_reg(op : tasmop;_size : topsize;_op1,_op2,_op3 : tregister);
      begin
         inherited init;
         init(op,_size);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
      end;

    constructor taicpu.op_const_reg_reg(op : tasmop;_size : topsize;_op1 : longint;_op2 : tregister;_op3 : tregister);
      begin
         inherited init;
         init(op,_size);
         ops:=3;
         loadconst(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
      end;

    constructor taicpu.op_reg_reg_ref(op : tasmop;_size : topsize;_op1,_op2 : tregister;_op3 : preference);
      begin
         inherited init;
         init(op,_size);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadref(2,_op3);
      end;


    constructor taicpu.op_const_ref_reg(op : tasmop;_size : topsize;_op1 : longint;_op2 : preference;_op3 : tregister);
      begin
         inherited init;
         init(op,_size);
         ops:=3;
         loadconst(0,_op1);
         loadref(1,_op2);
         loadreg(2,_op3);
      end;


    constructor taicpu.op_const_reg_ref(op : tasmop;_size : topsize;_op1 : longint;_op2 : tregister;_op3 : preference);
      begin
         inherited init;
         init(op,_size);
         ops:=3;
         loadconst(0,_op1);
         loadreg(1,_op2);
         loadref(2,_op3);
      end;


   constructor taicpu.op_ref_reglist(op: tasmop; _size : topsize; _op1: preference;_op2: tregisterlist);
     Begin
        inherited init;
        init(op,_size);
        ops:=2;
        loadref(0,_op1);
        loadreglist(1,newreglist(_op2));
     end;

   constructor taicpu.op_reglist_ref(op: tasmop; _size : topsize; _op1: tregisterlist; _op2: preference);
     Begin
        inherited init;
        init(op,_size);
        ops:=2;
        loadreglist(0,newreglist(_op1));
        loadref(1,_op2);
     End;



   constructor taicpu.op_reg_reglist(op: tasmop; _size : topsize; _op1: tregister;_op2: tregisterlist);
     Begin
        inherited init;
        init(op,_size);
        ops:=2;
        loadreg(0,_op1);
        loadreglist(1,newreglist(_op2));
     end;


   constructor taicpu.op_reglist_reg(op: tasmop; _size : topsize; _op1: tregisterlist; _op2: tregister);
     Begin
        inherited init;
        init(op,_size);
        ops:=2;
        loadreglist(0,newreglist(_op1));
        loadreg(1,_op2);
     End;


(*    constructor taicpu.op_cond_sym(op : tasmop;cond:TAsmCond;_size : topsize;_op1 : pasmsymbol);
      begin
         inherited init;
         init(op,_size);
         condition:=cond;
         ops:=1;
         loadsymbol(0,_op1,0);
      end; *)


    constructor taicpu.op_sym(op : tasmop;_size : topsize;_op1 : pasmsymbol);
      begin
         inherited init;
         init(op,_size);
         ops:=1;
         loadsymbol(0,_op1,0);
      end;


    constructor taicpu.op_sym_ofs(op : tasmop;_size : topsize;_op1 : pasmsymbol;_op1ofs:longint);
      begin
         inherited init;
         init(op,_size);
         ops:=1;
         loadsymbol(0,_op1,_op1ofs);
      end;


    constructor taicpu.op_sym_ofs_reg(op : tasmop;_size : topsize;_op1 : pasmsymbol;_op1ofs:longint;_op2 : tregister);
      begin
         inherited init;
         init(op,_size);
         ops:=2;
         if ((op >= A_DBCC) and (op <= A_DBF))
          or ((op >= A_FDBEQ) and (op <= A_FDBNGLE)) then
           begin
             loadreg(0,_op2);
             loadsymbol(1,_op1,_op1ofs);
           end
          else
           begin
             loadsymbol(0,_op1,_op1ofs);
             loadreg(1,_op2);
           end;
      end;


    constructor taicpu.op_sym_ofs_ref(op : tasmop;_size : topsize;_op1 : pasmsymbol;_op1ofs:longint;_op2 : preference);
      begin
         inherited init;
         init(op,_size);
         ops:=2;
         loadsymbol(0,_op1,_op1ofs);
         loadref(1,_op2);
      end;


    destructor taicpu.done;
      var
        i : longint;
      begin
        case oper[0].typ of
          top_ref:
           begin
            if assigned(oper[0].ref) then
               dispose(oper[0].ref);
           end;
          top_symbol:
           begin
             { if assigned(oper[0].sym) and
                assigned(asmsymbollist) then
               Pasmsymbol(oper[0].sym)^.decrefs; }
           end;
        end;
        for i:=2 to ops do
          if (oper[i-1].typ=top_ref) then
            dispose(oper[i-1].ref);
        inherited done;
      end;


    function taicpu.getcopy:plinkedlist_item;
      var
        i : longint;
        p : plinkedlist_item;
      begin
        p:=inherited getcopy;
        { make a copy of the references }
        for i:=1 to ops do
         if (paicpu(p)^.oper[i-1].typ=top_ref) then
          begin
            new(paicpu(p)^.oper[i-1].ref);
            paicpu(p)^.oper[i-1].ref^:=oper[i-1].ref^;
          end;
        getcopy:=p;
      end;




    function taicpu.GetString:string;
{$ifdef ASMDEBUG}
      var
        i : longint;
        s : string;
        addsize : boolean;
{$endif}
      begin
{$ifdef ASMDEBUG}
        s:='['+int_op2str[opcode];
        for i:=1to ops do
         begin
           if i=1 then
            s:=s+' '
           else
            s:=s+',';
           { type }
           addsize:=false;
           if (oper[i-1].ot and OT_XMMREG)=OT_XMMREG then
            s:=s+'xmmreg'
           else
             if (oper[i-1].ot and OT_MMXREG)=OT_MMXREG then
              s:=s+'mmxreg'
           else
             if (oper[i-1].ot and OT_FPUREG)=OT_FPUREG then
              s:=s+'fpureg'
           else
            if (oper[i-1].ot and OT_REGISTER)=OT_REGISTER then
             begin
               s:=s+'reg';
               addsize:=true;
             end
           else
            if (oper[i-1].ot and OT_IMMEDIATE)=OT_IMMEDIATE then
             begin
               s:=s+'imm';
               addsize:=true;
             end
           else
            if (oper[i-1].ot and OT_MEMORY)=OT_MEMORY then
             begin
               s:=s+'mem';
               addsize:=true;
             end
           else
             s:=s+'???';
           { size }
           if addsize then
            begin
              if (oper[i-1].ot and OT_BITS8)<>0 then
                s:=s+'8'
              else
               if (oper[i-1].ot and OT_BITS16)<>0 then
                s:=s+'16'
              else
               if (oper[i-1].ot and OT_BITS32)<>0 then
                s:=s+'32'
              else
                s:=s+'??';
              { signed }
              if (oper[i-1].ot and OT_SIGNED)<>0 then
               s:=s+'s';
            end;
         end;
        GetString:=s+']';
{$else}
        GetString:='';
{$endif ASMDEBUG}
      end;


{****************************************************************************
                              TAI_LABELED
 ****************************************************************************}

    constructor tai_labeled.init(op : tasmop; l : pasmlabel);

      begin
         inherited init;
         sym := nil;
         opcode := op;
         lab := l;
         register := R_NO;
         typ:=ait_labeled_instruction;
         lab^.increfs;
      end;


    constructor tai_labeled.init_sym(op : tasmop; asym: pasmsymbol);
      begin
         inherited init;
         sym:= asym;
         lab := nil;
         opcode := op;
         register := R_NO;
         typ:=ait_labeled_instruction;
{         lab^.increfs;}
      end;

    constructor tai_labeled.init_reg_sym(op : tasmop; asym: pasmsymbol; reg :tregister);
      begin
         inherited init;
         sym:= asym;
         lab := nil;
         opcode := op;
         register := reg;
         typ:=ait_labeled_instruction;
{         lab^.increfs;}
      end;

    constructor tai_labeled.init_reg(op : tasmop; l : pasmlabel; reg: tregister);

      begin
         inherited init;
         sym := nil;
         lab := l;
         opcode := op;
         register := reg;
         typ:=ait_labeled_instruction;
         lab^.increfs;
      end;

    destructor tai_labeled.done;

      begin
         { if assigned(lab) and
            assigned(asmsymbollist) then
           lab^.decrefs; }
         inherited done;
      end;


end.
{
  $Log: cpuasm.pas,v $
  Revision 1.1.2.12  2002/11/21 10:27:26  pierre
   * use A_FDBNGLE

  Revision 1.1.2.11  2002/11/12 11:38:57  pierre
   + tasmsymbol refs member made private.
   + increfs and decrefs methods added.
   * decrefs only called if the label is not used, not on disposal.
   + added check that refs does not become < 0.

  Revision 1.1.2.10  2002/11/07 16:51:09  pierre
   * several memory leaks removed

  Revision 1.1.2.9  2001/08/09 21:46:01  pierre
   * avoid deref of nil lab in tai_labeled destructor

  Revision 1.1.2.8  2001/08/08 12:18:29  pierre
   + tai_labeled.init_sym constructor added, needed by rasm

  Revision 1.1.2.7  2001/07/19 16:43:32  pierre
   + improoved register allocation info

  Revision 1.1.2.6  2001/05/18 18:03:23  carl
  + new templates for saving/restoring multiple registers

  Revision 1.1.2.5  2001/04/24 12:01:08  carl
  * correction of problems with pai_labeled instructions

  Revision 1.1.2.4  2001/04/23 01:14:42  carl
  * bugfix of not initializing the labeled instructions

  Revision 1.1.2.3  2001/04/21 05:04:51  carl
  * nil pointer checking, otherwise a crash would occur (taicpu.done)

  Revision 1.1.2.2  2001/04/17 03:24:20  carl
  + some bug fixes

  Revision 1.1.2.1  2001/02/23 10:05:20  pierre
   * first bunch of m68k cpu updates

  Revision 1.1  2000/07/13 06:30:05  michael
  + Initial import

  Revision 1.2  2000/01/07 01:14:50  peter
    * updated copyright to 2000

  Revision 1.1  1999/09/16 23:05:57  florian
    * m68k compiler is again compilable (only gas writer, no assembler reader)

}
