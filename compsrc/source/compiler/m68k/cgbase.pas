{
    $Id: cgbase.pas,v 1.1.2.5 2001/04/23 01:14:08 carl Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    This unit implements simplified routines for emitting
    assembler instructions.

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
unit cgbase;

interface
uses
 aasm,cpubase,cpuasm;

    procedure emit_none(i : tasmop;s : topsize);

    procedure emit_const(i : tasmop;s : topsize;c : longint);
    procedure emit_reg(i : tasmop;s : topsize;reg : tregister);
    procedure emit_ref(i : tasmop;s : topsize;ref : preference);

    procedure emit_const_reg(i : tasmop;s : topsize;c : longint;reg : tregister);
    procedure emit_const_ref(i : tasmop;s : topsize;c : longint;ref : preference);
    procedure emit_ref_reg(i : tasmop;s : topsize;ref : preference;reg : tregister);
    procedure emit_reg_ref(i : tasmop;s : topsize;reg : tregister;ref : preference);
    procedure emit_reg_reg(i : tasmop;s : topsize;reg1,reg2 : tregister);
    procedure emit_const_reg_reg(i : tasmop;s : topsize;c : longint;reg1,reg2 : tregister);
    procedure emit_reg_reg_reg(i : tasmop;s : topsize;reg1,reg2,reg3 : tregister);
    procedure emit_sym(i : tasmop;s : topsize;op : pasmsymbol);
    procedure emit_sym_ofs(i : tasmop;s : topsize;op : pasmsymbol;ofs : longint);
    procedure emit_sym_ofs_reg(i : tasmop;s : topsize;op : pasmsymbol;ofs:longint;reg : tregister);
    procedure emit_sym_ofs_ref(i : tasmop;s : topsize;op : pasmsymbol;ofs:longint;ref : preference);
    procedure emit_ref_ref(i: tasmop; s: topsize; ref1: preference; ref2: preference);


implementation

{    procedure emit_ref(i: tasmop; s: topsize; ref : preference);
      begin
         exprasmlist^.concat(new(paicpu,op_ref(i,s,ref)));
      end;}

    procedure emit_ref_ref(i: tasmop; s: topsize; ref1: preference; ref2: preference);
      begin
         exprasmlist^.concat(new(paicpu,op_ref_ref(i,s,ref1,ref2)));
      end;

    procedure emit_none(i : tasmop;s : topsize);
      begin
         exprasmlist^.concat(new(paicpu,op_none(i,s)));
      end;

    procedure emit_reg(i : tasmop;s : topsize;reg : tregister);
      begin
         exprasmlist^.concat(new(paicpu,op_reg(i,s,reg)));
      end;

    procedure emit_ref(i : tasmop;s : topsize;ref : preference);
      begin
         exprasmlist^.concat(new(paicpu,op_ref(i,s,ref)));
      end;

    procedure emit_const(i : tasmop;s : topsize;c : longint);
      begin
         exprasmlist^.concat(new(paicpu,op_const(i,s,c)));
      end;

    procedure emit_const_reg(i : tasmop;s : topsize;c : longint;reg : tregister);
      begin
         exprasmlist^.concat(new(paicpu,op_const_reg(i,s,c,reg)));
      end;

    procedure emit_const_ref(i : tasmop;s : topsize;c : longint;ref : preference);
      begin
         exprasmlist^.concat(new(paicpu,op_const_ref(i,s,c,ref)));
      end;

    procedure emit_ref_reg(i : tasmop;s : topsize;ref : preference;reg : tregister);
      begin
         exprasmlist^.concat(new(paicpu,op_ref_reg(i,s,ref,reg)));
      end;

    procedure emit_reg_ref(i : tasmop;s : topsize;reg : tregister;ref : preference);
      begin
         exprasmlist^.concat(new(paicpu,op_reg_ref(i,s,reg,ref)));
      end;

    procedure emit_reg_reg(i : tasmop;s : topsize;reg1,reg2 : tregister);
      begin
         if (reg1=reg2) and (i = A_MOVE) then
            exit;
         exprasmlist^.concat(new(paicpu,op_reg_reg(i,s,reg1,reg2)));
      end;

    procedure emit_const_reg_reg(i : tasmop;s : topsize;c : longint;reg1,reg2 : tregister);
      begin
         exprasmlist^.concat(new(paicpu,op_const_reg_reg(i,s,c,reg1,reg2)));
      end;

    procedure emit_reg_reg_reg(i : tasmop;s : topsize;reg1,reg2,reg3 : tregister);
      begin
         exprasmlist^.concat(new(paicpu,op_reg_reg_reg(i,s,reg1,reg2,reg3)));
      end;

    procedure emit_sym(i : tasmop;s : topsize;op : pasmsymbol);
      begin
        exprasmlist^.concat(new(paicpu,op_sym(i,s,op)));
      end;

    procedure emit_sym_ofs(i : tasmop;s : topsize;op : pasmsymbol;ofs : longint);
      begin
        exprasmlist^.concat(new(paicpu,op_sym_ofs(i,s,op,ofs)));
      end;

    procedure emit_sym_ofs_reg(i : tasmop;s : topsize;op : pasmsymbol;ofs:longint;reg : tregister);
      begin
        exprasmlist^.concat(new(paicpu,op_sym_ofs_reg(i,s,op,ofs,reg)));
      end;

    procedure emit_sym_ofs_ref(i : tasmop;s : topsize;op : pasmsymbol;ofs:longint;ref : preference);
      begin
        exprasmlist^.concat(new(paicpu,op_sym_ofs_ref(i,s,op,ofs,ref)));
      end;

end.