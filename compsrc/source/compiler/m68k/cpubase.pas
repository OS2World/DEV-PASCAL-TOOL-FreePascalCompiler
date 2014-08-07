{
    $Id: cpubase.pas,v 1.1.2.16 2002/12/11 13:18:12 pierre Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl, Carl Eric Codere

    This unit implements an types and classes specific for the
    MC68000/MC68020

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
unit cpubase;

  interface

    uses
       cobjects,aasm,globtype;


    type
    {  warning: CPU32 opcodes are not fully compatible with the MC68020. }
       { 68000 only opcodes }
       tasmop = (A_ABCD,
         A_ADD,A_ADDA,A_ADDI,A_ADDQ,A_ADDX,A_AND,A_ANDI,
         A_ASL,A_ASR,A_BCC,A_BCS,A_BEQ,A_BGE,A_BGT,A_BHI,
         A_BLE,A_BLO,A_BLS,A_BLT,A_BMI,A_BNE,A_BPL,A_BVC,A_BVS,
         A_BCHG,A_BCLR,A_BRA,A_BSET,A_BSR,A_BTST,A_CHK,
         A_CLR,A_CMP,A_CMPA,A_CMPI,A_CMPM,A_DBCC,A_DBCS,A_DBEQ,A_DBGE,
         A_DBGT,A_DBHI,A_DBLE,A_DBLO,A_DBLS,A_DBLT,A_DBMI,A_DBNE,A_DBRA,
         A_DBPL,A_DBT,A_DBVC,A_DBVS,A_DBF,A_DIVS,A_DIVU,
         A_EOR,A_EORI,A_EXG,A_ILLEGAL,A_EXT,A_JMP,A_JSR,
         A_LEA,A_LINK,A_LSL,A_LSR,A_MOVE,A_MOVEA,A_MOVEI,A_MOVEQ,
         A_MOVEM,A_MOVEP,A_MULS,A_MULU,A_NBCD,A_NEG,A_NEGX,
         A_NOP,A_NOT,A_OR,A_ORI,A_PEA,A_ROL,A_ROR,A_ROXL,
         A_ROXR,A_RTR,A_RTS,A_SBCD,A_SCC,A_SCS,A_SEQ,A_SGE,
         A_SGT,A_SHI,A_SLE,A_SLS,A_SLT,A_SMI,A_SNE,
         A_SPL,A_ST,A_SVC,A_SVS,A_SF,A_SUB,A_SUBA,A_SUBI,A_SUBQ,
         A_SUBX,A_SWAP,A_TAS,A_TRAP,A_TRAPV,A_TST,A_UNLK,
         A_RTE,A_RESET,A_STOP,
         { MC68010 instructions }
         A_BKPT,A_MOVEC,A_MOVES,A_RTD,
         { MC68020 instructions }
         A_BFCHG,A_BFCLR,A_BFEXTS,A_BFEXTU,A_BFFFO,
         A_BFINS,A_BFSET,A_BFTST,A_CALLM,A_CAS,A_CAS2,
         A_CHK2,A_CMP2,A_DIVSL,A_DIVUL,A_EXTB,A_PACK,A_RTM,
         A_TRAPCC,A_TRACS,A_TRAPEQ,A_TRAPF,A_TRAPGE,A_TRAPGT,
         A_TRAPHI,A_TRAPLE,A_TRAPLS,A_TRAPLT,A_TRAPMI,A_TRAPNE,
         A_TRAPPL,A_TRAPT,A_TRAPVC,A_TRAPVS,A_UNPK,
         { FPU Processor instructions - directly supported only. }
         { IEEE aware and misc. condition codes not supported   }
         A_FABS,A_FADD,
         A_FBEQ,A_FBNE,A_FBNGT,A_FBGT,A_FBGE,A_FBNGE,
         A_FBLT,A_FBNLT,A_FBLE,A_FBGL,A_FBNGL,A_FBGLE,A_FBNGLE,
         A_FDBEQ,A_FDBNE,A_FDBGT,A_FDBNGT,A_FDBGE,A_FDBNGE,
         A_FDBLT,A_FDBNLT,A_FDBLE,A_FDBGL,A_FDBNGL,A_FDBGLE,A_FDBNGLE,
         A_FSEQ,A_FSNE,A_FSGT,A_FSNGT,A_FSGE,A_FSNGE,
         A_FSLT,A_FSNLT,A_FSLE,A_FSGL,A_FSNGL,A_FSGLE,A_FSNGLE,
         A_FCMP,A_FDIV,A_FMOVE,A_FMOVEM,
         A_FMUL,A_FNEG,A_FNOP,A_FSQRT,A_FSUB,A_FSGLDIV,
         A_FSFLMUL,A_FTST,
         { These two were missing PM }
         A_FINT,A_FINTRZ,
	 A_FSIN,A_FCOS,A_FATAN,A_FLOGN,A_FETOX,
         A_FTRAPEQ,A_FTRAPNE,A_FTRAPGT,A_FTRAPNGT,A_FTRAPGE,A_FTRAPNGE,
         A_FTRAPLT,A_FTRAPNLT,A_FTRAPLE,A_FTRAPGL,A_FTRAPNGL,A_FTRAPGLE,A_FTRAPNGLE,
         { Protected instructions }
         A_CPRESTORE,A_CPSAVE,
         { FPU Unit protected instructions                    }
         { and 68030/68851 common MMU instructions            }
         { (this may include 68040 MMU instructions)          }
         A_FRESTORE,A_FSAVE,A_PFLUSH,A_PFLUSHA,A_PLOAD,A_PMOVE,A_PTEST,
         { Useful for assembly langage output }
         A_LABEL,A_NONE);

       { enumeration for registers, don't change the }
       { order of this table                         }
       { Registers which can and will be used by the compiler }
       tregister = (
         R_NO,R_D0,R_D1,R_D2,R_D3,R_D4,R_D5,R_D6,R_D7,
         R_A0,R_A1,R_A2,R_A3,R_A4,R_A5,R_A6,R_SP,
         { PUSH/PULL- quick and dirty hack }
         R_SPPUSH,R_SPPULL,
         { misc. }
         R_CCR,R_FP0,R_FP1,R_FP2,R_FP3,R_FP4,R_FP5,R_FP6,
         R_FP7,R_FPCR,R_SR,R_SSP,R_DFC,R_SFC,R_VBR,R_FPSR,
         { other - not used in reg2str }
         R_DEFAULT_SEG);



       { S_NO = No Size of operand }
       { S_B  = Byte size operand  }
       { S_W  = Word size operand  }
       { S_L  = DWord size operand }
       { USED FOR conversions in x86}
       { S_BW = Byte to word       }
       { S_BL = Byte to long       }
       { S_WL = Word to long       }
       { Floating point types      }
       { S_FS  = single type (32 bit) }
       { S_FL  = double/64bit integer }
       { S_FX  = Extended type      }
       { S_IS  = integer on 16 bits   }
       { S_IL  = integer on 32 bits   }
       { S_IQ  = integer on 64 bits   }
       topsize = (S_NO,S_B,S_W,S_L,S_BW,S_BL,S_WL,
                  S_FS,S_FL,S_FX,S_IS,S_IL,S_IQ);

       plocation = ^tlocation;

       { information about the location of an operand }
       { LOC_FPU         FPU registers = Dn if emulation }
       { LOC_REGISTER    in a processor register }
       { LOC_MEM         in the memory }
       { LOC_REFERENCE   like LOC_MEM, but lvalue }
       { LOC_JUMP        nur bool'sche Resultate, Sprung zu false- oder }
       {                 truelabel }
       { LOC_FLAGS       nur bool'sche Rsultate, Flags sind gesetzt }
       { LOC_CREGISTER   register which shouldn't be modified }
       { LOC_INVALID     added for tracking problems}

       tloc = (LOC_INVALID,LOC_FPU,LOC_REGISTER,LOC_MEM,LOC_REFERENCE,LOC_JUMP,
           LOC_FLAGS,LOC_CREGISTER);

       tregisterlist = set of tregister;
       pregisterlist = ^tregisterlist;

 { F_E = Equal
   F_NE = Not Equal
   F_G = Greater then
   F_L = Less then
   F_GE = Greater or equal then
   F_LE = Less or equal then
   F_C = Carry
   F_NC = Not Carry
   F_A = Above
   F_AE = Above or Equal
   F_B = Below
   F_BE = Below or Equal
   other flags:
   FL_xxx = floating type flags .

 }
       tresflags = (F_E,F_NE,F_G,F_L,F_GE,F_LE,F_C,F_NC,
          F_A,F_AE,F_B,F_BE);

       trefoptions=(ref_none,ref_parafixup,ref_localfixup,ref_selffixup);


      { direction of address register : }
      {              (An)     (An)+   -(An)  }
      tdirection = (dir_none,dir_inc,dir_dec);

      preference = ^treference;
      treference = record
        base,segment,index : tregister;
        offset : longint;
        symbol      : pasmsymbol;
        offsetfixup : longint;
        options     : trefoptions;
        { indexed increment and decrement mode }
        { (An)+ and -(An)                      }
        direction : tdirection;
        { a constant is also a treference, this makes the code generator }
        { easier                                                         }
        is_immediate : boolean;
        scalefactor : byte;
      end;

      tlocation = record
      case loc : tloc of
         { segment in reference at the same place as in loc_register }
        { segment in reference at the same place as in loc_register }
        LOC_REGISTER,LOC_CREGISTER :(
        case longint of
          1 : (register,segment,registerhigh : tregister);
          2 : (registerlow : tregister);
         );
         LOC_MEM,LOC_REFERENCE : (reference : treference);
         LOC_FPU : (
         case longint of
           1 : (fpuregister:tregister);
           { this is for emulation purposes - these are general }
           { purpose registers which can hold a double on a     }
           { register pair.                                     }
           2 : (fpuregisterlow, fpuregisterhigh : tregister);
         );
         LOC_JUMP : ();
         LOC_FLAGS : (resflags : tresflags);
         LOC_INVALID : ();
       end;


    type
      tregisterset = set of tregister;

{*****************************************************************************
                                 Constants
*****************************************************************************}

    const
       ALL_REGISTERS = [R_D0..R_A6,R_FP0..R_FP7];
       firstreg = R_D0;
       lastreg = R_FP7;
       { needed so that FPU registers also get pushed
         in calling a subroutine PM }



       {This constant is an alias for the stack pointer }
       stack_pointer = R_SP;
       frame_pointer = R_A6;
       self_pointer : tregister = R_A5;
       {This constant is an alias for the accumulator    }
       accumulator = R_D0;
       scratch_reg = R_D1;



      registers_saved_on_cdecl = [];

{
      scratch_regs : array[1..1] of tregister = (R_EDI);

      max_scratch_regs = 1;}

     { sizes }
     pointersize   = 4;
     extended_size = 12;
     sizepostfix_pointer = S_L;



     { obsolete constant names }
     maxvarregs = 6;
     maxfpuvarregs = 7;
     { new constant names }
     maxintregs = maxvarregs;
     maxfpuregs = maxfpuvarregs;
     maxaddrregs = 8;

     varregs : array[1..maxintregs] of tregister =
       (R_D2,R_D3,R_D4,R_D5,R_D6,R_D7);
     fpuvarregs : array[1..maxfpuregs] of tregister =
      (R_FP1,R_FP2,R_FP3,R_FP4,R_FP5,R_FP6,R_FP7);
     { address registers - this is excactly as other registers }
     { in most processors.                                     }
     addrregs : array[1..maxaddrregs] of tregister =
       (R_A0,R_A1,R_A2,R_A3,R_A4,R_A5,R_A6,R_SP);



 {----------------------------------------------------------------------}
 { F_E = Equal                                                          }
 { F_NE = Not Equal                                                     }
 { F_G = Greater then                                                   }
 { F_L = Less then                                                      }
 { F_GE = Greater or equal then                                         }
 { F_LE = Less or equal then                                            }
 { F_C = Carry                            = C                           }
 { F_NC = Not Carry                       = not C                       }
 { F_A = Above                            = not C and not Z             }
 { F_AE = Above or Equal                  = not C                       }
 { F_B = Below                            = C                           }
 { F_BE = Below or Equal                  = C or Z                      }
 { FL_E = Floating point equal            = Z                           }
 { FL_NE = Floating point Not equal       = not Z                       }
 { FL_A  = Floating point above           =                             }
 { FL_AE = Floating point above or equal  =                             }
 { FL_B  = Floating point below           =                             }
 { FL_BE = Floating point below or equal  =                             }

 { THE ORDER OF THIS TABLE SHOULD NOT BE CHANGED! }
 {  F_E,F_NE,F_G,F_L,F_GE,F_LE,F_C,F_NC,
          F_A,F_AE,F_B,F_BE }
 flag_2_jmp: array[F_E..F_BE] of tasmop =
 (A_BEQ,A_BNE,A_BGT,A_BLT,A_BGE,A_BLE,A_BCS,A_BCC,
  A_BHI,A_BCC,A_BCS,A_BLS);
  { floating point jumps - CURRENTLY NOT USED }
{  A_FBEQ,A_FBNE,A_FBGT,A_FBGE,A_FBLT,A_FBLE); }

 { don't change the order of this table, it is related to }
 { the flags table.                                       }

 flag_2_set: array[F_E..F_BE] of tasmop =
 (A_SEQ,A_SNE,A_SGT,A_SLT,A_SGE,A_SLE,A_SCS,A_SCC,
  A_SHI,A_SCC,A_SCS,A_SLS);


type
  TAsmCond=(C_None,
    (* C_A,C_AE,C_B,C_BE,C_C,C_E,C_G,C_GE,C_L,C_LE,C_NA,C_NAE,
    C_NB,C_NBE,C_NC,C_NE,C_NG,C_NGE,C_NL,C_NLE,C_NO,C_NP,
    C_NS,C_NZ,C_O,C_P,C_PE,C_PO,C_S,C_Z *)
    C_EQ,C_NE,C_GT,C_LT,C_GE,C_LE,C_CS,C_CC,
    C_HI,C_LS);

const
  cond2str:array[TAsmCond] of string[3]=('',
    'eq','ne','gt','lt','ge','le','cs','cc',
    'hi','ls'
    );
  inverse_cond:array[TAsmCond] of TAsmCond=(C_None,
    {C_EQ}C_NE,{C_NE}C_EQ,
    {C_GT}C_LE,{C_LT}C_GE,
    {C_GE}C_LT,{C_LE}C_GT,
    {C_CS}C_CC,{C_CC}C_CS,
    {C_HI}C_LS,{C_LS}C_HI
  );
const
  { arrays for boolean location conversions }
 {  F_E,F_NE,F_G,F_L,F_GE,F_LE,F_C,F_NC,
          F_A,F_AE,F_B,F_BE }
  flag_2_cond : array[TResFlags] of TAsmCond =
     (C_EQ,C_NE,C_GT,C_LT,C_GE,C_LE,C_CS,C_CC,
      C_HI,C_CC,C_CS,C_LS);



const
  CondAsmOps=2;
  CondAsmOp:array[0..CondAsmOps-1] of TasmOp=(
    A_Bcc, A_Scc
  );
  CondAsmOpStr:array[0..CondAsmOps-1] of string[4]=(
    'B','S'
  );



TYPE
{*****************************************************************************
                                Operands
*****************************************************************************}

       { Types of operand }
        toptype=(top_none,top_reg,top_ref,top_const,top_symbol,top_reglist);

        toper=record
          ot  : longint;
          case typ : toptype of
           top_none   : ();
           top_reg    : (reg:tregister);
           top_ref    : (ref:preference);
           top_const  : (val:longint);
           top_symbol : (sym:pasmsymbol;symofs:longint);
           top_reglist: (registerlist : pregisterlist);
        end;




    { resets all values of ref to defaults }
    procedure reset_reference(var ref : treference);

    { set mostly used values of a new reference }
    function new_reference(base : tregister;offset : longint) : preference;

    procedure disposereference(var r : preference);

    function newreference(const r : treference) : preference;

    function newreglist(const r : tregisterlist) : pregisterlist;

    function reg2str(r : tregister) : string;



{****************************************************************************
                            Assembler Mnemoics
****************************************************************************}

   const
     firstop = A_ABCD;
     lastop = A_LABEL;

     mot_op2str : array[firstop..lastop] of string[10] =
       { 68000 only instructions }
       ('abcd','add', 'adda','addi','addq','addx','and','andi',
       'asl','asr','bcc','bcs','beq','bge','bgt','bhi',
       'ble','blo','bls','blt','bmi','bne','bpl','bvc','bvs',
       'bchg','bclr','bra','bset','bsr','btst','chk',
       'clr','cmp','cmpa','cmpi','cmpm','dbcc','dbcs','dbeq','dbge',
       'dbgt','dbhi','dble','dblo','dbls','dblt','dbmi','dbne','dbra',
       'dbpl','dbt','dbvc','dbvs','dbf','divs','divu',
       'eor','eori','exg','illegal','ext','jmp','jsr',
       'lea','link','lsl','lsr','move','movea','movei','moveq',
       'movem','movep','muls','mulu','nbcd','neg','negx',
       'nop','not','or','ori','pea','rol','ror','roxl',
       'roxr','rtr','rts','sbcd','scc','scs','seq','sge',
       'sgt','shi','sle','sls','slt','smi','sne',
       'spl','st','svc','svs','sf','sub','suba','subi','subq',
       'subx','swap','tas','trap','trapv','tst','unlk',
       'rte','reset','stop',
       { MC68010 instructions }
       'bkpt','movec','moves','rtd',
       { MC68020 instructions }
       'bfchg','bfclr','bfexts','bfextu','bfffo',
       'bfins','bfset','bftst','callm','cas','cas2',
       'chk2','cmp2','divsl','divul','extb','pack','rtm',
       'trapcc','tracs','trapeq','trapf','trapge','trapgt',
       'traphi','traple','trapls','traplt','trapmi','trapne',
       'trappl','trapt','trapvc','trapvs','unpk',
       { FPU Processor instructions - directly supported only. }
       { IEEE aware and misc. condition codes not supported   }
       'fabs','fadd',
       'fbeq','fbne','fbngt','fbgt','fbge','fbnge',
       'fblt','fbnlt','fble','fbgl','fbngl','fbgle','fbngle',
       'fdbeq','fdbne','fdbgt','fdbngt','fdbge','fdnbge',
       'fdblt','fdbnlt','fdble','fdbgl','fdbngl','fdbgle','fdbngle',
       'fseq','fsne','fsgt','fsngt','fsge','fsnge',
       'fslt','fsnlt','fsle','fsgl','fsngl','fsgle','fsngle',
       'fcmp','fdiv','fmove','fmovem',
       'fmul','fneg','fnop','fsqrt','fsub','fsgldiv',
       'fsflmul','ftst',
       'fint','fintrz',
       'fsin','fcos','fatan','flogn','fetox',
       'ftrapeq','ftrapne','ftrapgt','ftrapngt','ftrapge','ftrapnge',
       'ftraplt','ftrapnlt','ftraple','ftrapgl','ftrapngl','ftrapgle',
       'ftrapngle',
       { Useful for assembly langage output }
       { Protected instructions }
       'cprestore','cpsave',
       { FPU Unit protected instructions                    }
       { and 68030/68851 common MMU instructions            }
       { (this may include 68040 MMU instructions)          }
       'frestore','fsave','pflush','pflusha','pload','pmove','ptest',
       { Useful for assembly langage output }
       '');

     mot_opsize2str : array[topsize] of string[2] =
      ('','.b','.w','.l','.b','.b','.w',
       '.s','.d','.x','.s','.l','.q');
       { I don't know about S_IS, S_IL and S_IQ for m68k
         so I guessed, I am not even sure it can happen !!
         (PM) }

     mot_reg2str : array[R_NO..R_FPSR] of string[6] =
      ('', 'd0','d1','d2','d3','d4','d5','d6','d7',
       'a0','a1','a2','a3','a4','a5','a6','sp',
       '-(sp)','(sp)+',
       'ccr','fp0','fp1','fp2','fp3','fp4','fp5',
       'fp6','fp7','fpcr','sr','ssp','dfc',
       'sfc','vbr','fpsr');

     gas_opsize2str : array[topsize] of string[2] =
      ('','.b','.w','.l','.b','.b','.w',
       '.s','.d','.x','.s','.l','.q');

     gas_reg2str : array[R_NO..R_FPSR] of string[6] =
      ('', '%d0','%d1','%d2','%d3','%d4','%d5','%d6','%d7',
       '%a0','%a1','%a2','%a3','%a4','%a5','%a6','%sp',
       '-(%sp)','(%sp)+',
       '%ccr','%fp0','%fp1','%fp2','%fp3','%fp4','%fp5',
       '%fp6','%fp7','%fpcr','%sr','%ssp','%dfc',
       '%sfc','%vbr','%fpsr');

     mit_opsize2str : array[topsize] of string[2] =
      ('','b','w','l','b','b','w',
       's','d','x','s','l','q');

     mit_reg2str : array[R_NO..R_FPSR] of string[6] =
      ('', 'd0','d1','d2','d3','d4','d5','d6','d7',
       'a0','a1','a2','a3','a4','a5','a6','sp',
       'sp@-','sp@+',
       'ccr','fp0','fp1','fp2','fp3','fp4','fp5',
       'fp6','fp7','fpcr','sr','ssp','dfc',
       'sfc','vbr','fpsr');



{*****************************************************************************
                                  Init/Done
*****************************************************************************}

  procedure InitCpu;
  procedure DoneCpu;

  implementation

    uses
      strings,globals,verbose;

    function reg2str(r : tregister) : string;

      const
     a : array[R_NO..R_FPSR] of string[5] =
      ('','D0','D1','D2','D3','D4','D5','D6','D7',
       'A0','A1','A2','A3','A4','A5','A6','A7',
      '-(SP)','(SP)+',
       'CCR','FP0','FP1','FP2',
       'FP3','FP4','FP5','FP6','FP7','FPCR','SR',
       'SSP','DFC','SFC','VBR','FPSR');

      begin
         reg2str:=a[r];
      end;

    procedure disposereference(var r : preference);

      begin
         dispose(r);
         r:=nil;
      end;

    function newreference(const r : treference) : preference;

      var
     p : preference;

      begin
         new(p);
         p^:=r;
         newreference:=p;
      end;

    function newreglist(const r : tregisterlist) : pregisterlist;
      var
        p : pregisterlist;
      begin
         new(p);
         p^:=r;
         newreglist:=p;
      end;

    procedure reset_reference(var ref : treference);

      begin
        FillChar(ref,sizeof(treference),0);
      end;

      function new_reference(base : tregister;offset : longint) : preference;

        var
           r : preference;
        begin
           new(r);
           FillChar(r^,sizeof(treference),0);
           r^.base:=base;
           r^.offset:=offset;
           new_reference:=r;
        end;


{*****************************************************************************
                                  Init/Done
*****************************************************************************}

  procedure InitCpu;
    begin
    end;

  procedure DoneCpu;
    begin
    end;

end.
{
  $Log: cpubase.pas,v $
  Revision 1.1.2.16  2002/12/11 13:18:12  pierre
   * change self_pointer into a tpyed const for palm os

  Revision 1.1.2.15  2002/11/21 10:25:57  pierre
   * fix fdbngle that was wrongly written fbdngle

  Revision 1.1.2.14  2002/11/21 09:11:20  pierre
   * A_BLO and A_DBLO aliases added

  Revision 1.1.2.13  2002/10/14 11:10:22  pierre
   * generate extended math functions for m68k if FPC_FPU_INTERNAL is defined

  Revision 1.1.2.12  2002/10/11 20:41:10  pierre
   * first part of internal fpu functions support

  Revision 1.1.2.11  2001/07/26 12:32:47  pierre
   + fint and fintrz instructions added

  Revision 1.1.2.10  2001/07/24 23:46:16  pierre
   * lastreg set to R_FP7 as fpu registers must also be saved

  Revision 1.1.2.9  2001/05/18 18:03:58  carl
  + moved constant section together

  Revision 1.1.2.8  2001/05/15 21:49:59  carl
  - rename gasPalm_reg2str -> gas_reg2str

  Revision 1.1.2.7  2001/04/24 11:59:56  carl
  * correction of problems with pai_labeled instructions

  Revision 1.1.2.6  2001/04/21 00:33:48  carl
  * m68k updates

  Revision 1.1.2.5  2001/04/19 11:37:37  carl
  * m68k updates (code generator compiles)

  Revision 1.1.2.4  2001/04/17 03:24:48  carl
  + make it compile correctly and clean up of useless stuff

  Revision 1.1.2.3  2001/03/27 03:15:14  carl
  + LOC_FPU is finalized correctly now (I hope!)

  Revision 1.1.2.2  2001/03/09 00:50:48  carl
  + added LOC_FPU registerhigh, registerlow

  Revision 1.1.2.1  2001/02/23 10:05:20  pierre
   * first bunch of m68k cpu updates

  Revision 1.1  2000/07/13 06:30:05  michael
  + Initial import

  Revision 1.2  2000/01/07 01:14:50  peter
    * updated copyright to 2000

  Revision 1.1  1999/09/16 23:05:57  florian
    * m68k compiler is again compilable (only gas writer, no assembler reader)

  Revision 1.12  1999/08/19 13:02:08  pierre
    + label faillabel added for _FAIL support

  Revision 1.11  1999/06/22 16:24:42  pierre
   * local browser stuff corrected

  Revision 1.10  1998/10/29 11:35:45  florian
    * some dll support for win32
    * fixed assembler writing for PalmOS

  Revision 1.9  1998/10/14 08:47:18  pierre
    * bugs in secondfuncret for result in subprocedures removed

  Revision 1.8  1998/10/13 16:50:15  pierre
    * undid some changes of Peter that made the compiler wrong
      for m68k (I had to reinsert some ifdefs)
    * removed several memory leaks under m68k
    * removed the meory leaks for assembler readers
    * cross compiling shoud work again better
      ( crosscompiling sysamiga works
       but as68k still complain about some code !)

  Revision 1.7  1998/08/31 12:26:27  peter
    * m68k and palmos updates from surebugfixes

  Revision 1.6  1998/08/21 14:08:44  pierre
    + TEST_FUNCRET now default (old code removed)
      works also for m68k (at least compiles)

  Revision 1.5  1998/06/04 23:51:45  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

  Revision 1.4  1998/05/23 01:21:10  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

  Revision 1.3  1998/05/11 13:07:54  peter
    + $ifdef NEWPPU for the new ppuformat
    + $define GDB not longer required
    * removed all warnings and stripped some log comments
    * no findfirst/findnext anymore to remove smartlink *.o files

  Revision 1.2  1998/04/29 10:33:54  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions
}
