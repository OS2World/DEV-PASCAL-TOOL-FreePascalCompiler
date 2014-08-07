{
    $Id: lowmath.inc,v 1.1.2.12 2002/09/29 14:00:04 carl Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2001 by Carl Eric Codere,
    member of the Free Pascal development team

    This include implements low-level mathematical
    routines for the 680x0 family of processors.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{*************************************************************************}
{ single floating point routines taken from GCC 2.5.2 Atari compiler      }
{ library source.                                                         }
{  Original credits:                                                      }
{    written by Kai-Uwe Bloem (I5110401@dbstu1.bitnet).                   }
{      Based on a 80x86 floating point packet from comp.os.minix,         }
{        written by P.Housel                                              }
{    Patched by Olaf Flebbe (flebbe@tat.physik.uni-tuebingen.de)          }
{    Revision by michal 05-93 (ntomczak@vm.ucs.ualberta.ca)               }
{*************************************************************************}
{ Following licences apply to Trunc() :                                   }
{                                                                         }
{  This C source fragment is part of the SoftFloat IEC/IEEE               }
{  Floating-point Arithmetic Package, Release 2a.                         }
{                                                                         }
{  Written by John R. Hauser.  This work was made possible in part by     }
{  the International Computer Science Institute, located at Suite 600, 1947 }
{  Center Street, Berkeley, California 94704.  Funding was partially      }
{  provided by the National Science Foundation under grant MIP-9311980.   }
{  The original version of this code was written as part of a project to  }
{  build a fixed-point vector processor in collaboration with the University }
{  of California at Berkeley, overseen by Profs. Nelson Morgan and John   }
{  Wawrzynek.  More information is available through the Web page         }
{  `http://HTTP.CS.Berkeley.EDU/~jhauser/arithmetic/SoftFloat.html'.      }
{                                                                         }
{  THIS SOFTWARE IS DISTRIBUTED AS IS, FOR FREE.  Although reasonable     }
{  effort has been made to avoid it, THIS SOFTWARE MAY CONTAIN FAULTS THAT}
{  WILL AT TIMES RESULT IN INCORRECT BEHAVIOR.  USE OF THIS SOFTWARE IS   }
{  RESTRICTED TO PERSONS AND ORGANIZATIONS WHO CAN AND WILL TAKE FULL     }
{  RESPONSIBILITY FOR ANY AND ALL LOSSES, COSTS, OR OTHER PROBLEMS ARISING}
{  FROM ITS USE.                                                          }
{                                                                         }
{  Derivative works are acceptable, even for commercial purposes, so long }
{  as (1) they include prominent notice that the work is derivative, and  }
{  (2) they include prominent notice akin to these four paragraphs for    }
{  those parts of this code that are retained.                            }
{-------------------------------------------------------------------------}
{   Conversion of the above C code was done by Carl Eric Codere - 2001    }
{-------------------------------------------------------------------------}

{--------------------------------------------------------------------}
{ LEFT TO DO:                                                        }
{ o Add support for FPU if present.                                  }
{--------------------------------------------------------------------}


{********************************************************************}
{*                   IEEE Single FPU arithmetic                     *}
{********************************************************************}

Procedure Single_Norm;[public,alias : 'FPC_NORM_SINGLE'];Assembler;
{--------------------------------------------}
{ Low-level routine to normalize single  e   }
{  IEEE floating point values. Never called  }
{  directly.                                 }
{ On Exit:                                   }
{      d0 = result.                          }
{  Registers destroyed: d0,d1                }
{--------------------------------------------}
Asm
   tst.l    d4             { rounding and u.mant == 0 ?    }
   bne      @normlab1
   tst.b    d1
   beq      @retzok
@normlab1:
   clr.b    d2             { "sticky byte"                 }
@normlab3:
   move.l   #$ff000000,d5
@normlab4:
   tst.w    d0             { divide (shift)                }
   ble      @normlab2      {  denormalized number          }
   move.l   d4,d3
   and.l    d5,d3          {  or until no bits above 23    }
   beq      @normlab5
@normlab2:
   addq.w   #1,d0          { increment exponent            }
   lsr.l    #1,d4
   or.b     d1,d2          { set "sticky"                  }
   roxr.b   #1,d1          { shift into rounding bits      }
   bra      @normlab4
@normlab5:
   and.b    #1,d2
   or.b     d2,d1          { make least sig bit "sticky"   }
   asr.l    #1,d5          { #0xff800000 -> d5             }
@normlab6:
   move.l   d4,d3          { multiply (shift) until        }
   and.l    d5,d3          { one in "implied" position     }
   bne      @normlab7
   subq.w   #1,d0          { decrement exponent            }
   beq      @normlab7      {  too small. store as denormalized number }
   add.b    d1,d1          { some doubt about this one *              }
   addx.l   d4,d4
   bra      @normlab6
@normlab7:
   tst.b    d1             { check rounding bits          }
   bge      @normlab9      { round down - no action neccessary }
   neg.b    d1
   bvc      @normlab8      { round up                     }
   move.w   d4,d1          { tie case - round to even     }
                           {   dont need rounding bits any more }
   and.w    #1,d1          { check if even                }
   beq      @normlab9      {   mantissa is even - no action necessary  }
                           { fall through                 }
@normlab8:
   clr.w    d1             { zero rounding bits            }
   add.l    #1,d4
   tst.w    d0
   bne      @normlab10     { renormalize if number was denormalized   }
   add.w    #1,d0          { correct exponent for denormalized numbers }
   bra      @normlab3
@normlab10:
   move.l   d4,d3          { check for rounding overflow              }
   asl.l    #1,d5          { #0xff000000 -> d5                        }
   and.l    d5,d3
   bne      @normlab4      { go back and renormalize                  }
@normlab9:
   tst.l    d4             { check if normalization caused an underflow }
   beq      @retz
   tst.w    d0             { check for exponent overflow or underflow  }
   blt      @retz
   cmp.w    #255,d0
   bge      @oflow

   lsl.w    #8,d0          { re-position exponent - one bit too high }
   lsl.w    #1,d2          { get X bit                               }
   roxr.w   #1,d0          { shift it into sign position             }
   swap     d0             { map to upper word                       }
   clr.w    d0
   and.l    #$7fffff,d4    { top mantissa bits                       }
   or.l     d4,d0          { insert exponent and sign                }
   movem.l  (sp)+,d2-d5
   rts

@retz:
   { handling underflow should be done here... }
   { by default simply return 0 as retzok...   }
@retzok:
   moveq.l   #0,d0
   lsl.w     #1,d2
   roxr.l    #1,d0          { sign of 0 is the same as of d2          }
   movem.l   (sp)+,d2-d5
   rts

@oflow:
   move.l    #$7f800000,d0  { +infinity as proposed by IEEE         }

   tst.w     d2             { transfer sign                         }
   bge       @ofl_clear     { (mjr++)                               }
   bset      #31,d0         {                                       }
@ofl_clear:
   or.b      #2,ccr         { set overflow flag.                    }
   movem.l   (sp)+,d2-d5
   rts
end;


Procedure Single_AddSub; Assembler;
{--------------------------------------------}
{ Low-level routine to add/subtract single   }
{  IEEE floating point values. Never called  }
{  directly.                                 }
{ On Exit:                                   }
{      d0 = result -- from normalize routine }
{      Flags : V set if overflow.            }
{              on underflow d0 = 0           }
{  Registers destroyed: d0,d1                }
{--------------------------------------------}
Asm
{--------------------------------------------}
{ On Entry:                                  }
{      d1-d0 = single values to subtract.    }
{--------------------------------------------}
XDEF FPC_SUB_SINGLE
   eor.l      #$80000000,d0         {   reverse sign of v }
{--------------------------------------------}
{ On Entry:                                  }
{      d0, d1 = single values to add.        }
{--------------------------------------------}
XDEF FPC_ADD_SINGLE
   movem.l d2-d5,-(sp)              { save registers      }
   move.l   d0,d4                   { d4 = d0 = v         }
   move.l   d1,d5                   { d5 = d1 = u         }

   move.l  #$7fffff,d3
   move.l  d5,d0                    { d0 = u.exp          }
   move.l  d5,d2                    { d2.h = u.sign       }
   swap       d0
   move.w  d0,d2                    { d2 = u.sign         }
   and.l   d3,d5                    { remove exponent from u.mantissa }

   move.l  d4,d1                    { d1 = v.exp          }
   and.l   d3,d4                    { remove exponent from v.mantissa }
   swap    d1
   eor.w   d1,d2                    { d2 = u.sign ^ v.sign (in bit 15)}
   clr.b   d2                       { we will use the lowest byte as a flag }
   moveq.l #15,d3
   bclr    d3,d1                    { kill sign bit u.exp             }
   bclr    d3,d0                    { kill sign bit u.exp             }
   btst    d3,d2                    { same sign for u and v?          }
   beq     @slabel1
   cmp.l   d0,d1                    { different signs - maybe x - x ? }
   seq     d2                       { set 'cancellation' flag         }
@slabel1:
   lsr.w   #7,d0                    { keep here exponents only        }
   lsr.w   #7,d1
{--------------------------------------------------------------------}
{          Now perform testing of NaN and infinities                 }
{--------------------------------------------------------------------}
   moveq.l #-1,d3
   cmp.b   d3,d0
   beq     @alabel1
   cmp.b   d3,d1
   bne     @nospec
   bra     @alabel2
{--------------------------------------------------------------------}
{                        u is special.                               }
{--------------------------------------------------------------------}
@alabel1:
   tst.b      d2
   bne        @retnan      { cancellation of specials -> NaN }
   tst.l      d5
   bne        @retnan      { arith with Nan gives always NaN }

   addq.w     #4,a0        { here is an infinity             }
   cmp.b      d3,d1
   bne        @alabel3     { skip check for NaN if v not special }
{--------------------------------------------------------------------}
{                        v is special.                               }
{--------------------------------------------------------------------}
@alabel2:
   tst.l           d4
   bne        @retnan
@alabel3:
   move.l     (a0),d0
   bra        @return
{--------------------------------------------------------------------}
{                     Return a quiet nan                             }
{--------------------------------------------------------------------}
@retnan:
   moveq.l    #-1,d0
   lsr.l      #1,d0                { 0x7fffffff -> d0        }
   bra        @return
{ Ok, no inifinty or NaN involved.. }
@nospec:
   tst.b           d2
   beq        @alabel4
   moveq.l    #0,d0                { x - x hence we always return +0 }
@return:
   movem.l    (sp)+,d2-d5
   rts

@alabel4:
   moveq.l    #23,d3
   bset       d3,d5           { restore implied leading "1"   }
   tst.w      d0              { check for zero exponent - no leading "1" }
   bne        @alabel5
   bclr       d3,d5           { remove it                     }
   addq.w     #1,d0           { "normalize" exponent          }
@alabel5:
   bset       d3,d4           { restore implied leading "1"   }
   tst.w      d1              { check for zero exponent - no leading "1"  }
   bne        @alabel6
   bclr       d3,d4           { remove it                     }
   addq.w  #1,d1              { "normalize" exponent          }
@alabel6:
   moveq.l    #0,d3           { (put initial zero rounding bits in d3) }
   neg.w      d1              { d1 = u.exp - v.exp            }
   add.w      d0,d1
   beq        @alabel8      { exponents are equal - no shifting neccessary }
   bgt        @alabel7      { not equal but no exchange neccessary         }
   exg        d4,d5                { exchange u and v }
   sub.w      d1,d0                { d0 = u.exp - (u.exp - v.exp) = v.exp }
   neg.w      d1
   tst.w      d2              { d2.h = u.sign ^ (u.sign ^ v.sign) = v.sign }
   bpl        @alabel7
   bchg       #31,d2
@alabel7:
   cmp.w      #26,d1       { is u so much bigger that v is not }
   bge        @alabel9      { significant ?                     }
{--------------------------------------------------------------------}
{ shift mantissa left two digits, to allow cancellation of           }
{ most significant digit, while gaining an additional digit for      }
{ rounding.                                                          }
{--------------------------------------------------------------------}
   moveq.l #1,d3
@alabel10:
   add.l           d5,d5
   subq.w  #1,d0              { decrement exponent            }
   subq.w  #1,d1              { done shifting altogether ?    }
   dbeq    d3,@alabel10        { loop if still can shift u.mant more }
   moveq.l    #0,d3

   cmp.w      #16,d1          { see if fast rotate possible    }
   blt        @alabel11
   or.w       d4,d3           { set rounding bits              }
   clr.w      d4
   swap       d4
   subq.w  #8,d1
   subq.w  #8,d1
   bra      @alabel11

@alabel12:
   move.b    d4,d2
   and.b      #1,d2
   or.b       d2,d3
   lsr.l      #1,d4               { shift v.mant right the rest of the way }
@alabel11:
   dbra    d1,@alabel12           { loop                                   }

@alabel8:
   tst.w      d2                  { are the signs equal ?                  }
   bpl        @alabel13           { yes, no negate necessary               }


   tst.w      d3                  { negate rounding bits and v.mant        }
   beq        @alabel14
   addq.l  #1,d4
@alabel14:
   neg.l           d4

@alabel13:
   add.l      d4,d5                { u.mant = u.mant + v.mant              }
   bcs        @alabel9             { needn not negate                      }
   tst.w      d2                   { opposite signs ?                      }
   bpl        @alabel9             { do not need to negate result          }

   neg.l      d5
   not.l      d2                   { switch sign                           }
@alabel9:
   move.l  d5,d4                   { move result for normalization         }
   clr.l      d1
   tst.l      d3
   beq        @alabel15
   moveq.l #-1,d1
@alabel15:
   swap    d2                      { put sign into d2 (exponent is in d0)  }
   jmp     FPC_NORM_SINGLE         { leave registers on stack for norm_sf  }
end;


Procedure Single_Mul;Assembler;
{--------------------------------------------}
{ Low-level routine to multiply two single   }
{  IEEE floating point values. Never called  }
{  directly.                                 }
{ Om Entry:                                  }
{      d0,d1 = values to multiply            }
{ On Exit:                                   }
{      d0 = result.                          }
{  Registers destroyed: d0,d1                }
{ stack space used (and restored): 8 bytes.  }
{--------------------------------------------}
Asm
XDEF FPC_MUL_SINGLE
   movem.l  d2-d5,-(sp)
   move.l   d0,d4       { d4 = v                   }
   move.l   d1,d5       { d5 = u                   }

   move.l   #$7fffff,d3
   move.l   d5,d0       { d0 = u.exp               }
   and.l    d3,d5       { remove exponent from u.mantissa }
   swap     d0
   move.w   d0,d2       { d2 = u.sign              }

   move.l   d4,d1       { d1 = v.exp               }
   and.l    d3,d4       { remove exponent from v.mantissa }
   swap     d1
   eor.w    d1,d2       { d2 = u.sign ^ v.sign (in bit 15)}

   moveq.l  #15,d3
   bclr     d3,d0       { kill sign bit                   }
   bclr     d3,d1       { kill sign bit                   }
   tst.l    d0          { test if one of factors is 0     }
   beq      @mlabel1
   tst.l    d1
@mlabel1:
   seq      d2         { 'one of factors is 0' flag in the lowest byte }
   lsr.w    #7,d0      { keep here exponents only         }
   lsr.w    #7,d1

{--------------------------------------------------------------------}
{          Now perform testing of NaN and infinities                 }
{--------------------------------------------------------------------}
   moveq.l  #-1,d3
   cmp.b    d3,d0
   beq      @mlabel2
   cmp.b    d3,d1
   bne      @mnospec
   bra      @mlabel3
{--------------------------------------------------------------------}
{                   first operand is special                         }
{--------------------------------------------------------------------}
@mlabel2:
   tst.l    d5         { is it NaN?                       }
   bne      @mretnan
@mlabel3:
   tst.b    d2         { 0 times special or special times 0 ? }
   bne      @mretnan   { yes -> NaN                       }
   cmp.b    d3,d1      { is the other special ?           }
   beq      @mlabel4   { maybe it is NaN                  }
{--------------------------------------------------------------------}
{                   Return infiny with correct sign                  }
{--------------------------------------------------------------------}
@mretinf:
   move.l   #$ff000000,d0  { we will return #0xff800000 or #0x7f800000 }
   lsl.w    #1,d2
   roxr.l   #1,d0      { shift in high bit as given by d2 }
@mreturn:
   movem.l  (sp)+,d2-d5
   rts

{--------------------------------------------------------------------}
{                        v is special.                               }
{--------------------------------------------------------------------}
@mlabel4:
   tst.l    d4          { is this NaN?                    }
   beq      @mretinf    { we know that the other is not zero }
@mretnan:
   moveq.l  #-1,d0
   lsr.l    #1,d0       { 0x7fffffff -> d0                }
   bra      @mreturn
{--------------------------------------------------------------------}
{                    End of NaN and Inf                              }
{--------------------------------------------------------------------}
@mnospec:
   tst.b    d2          { not needed - but we can waste two instr. }
   bne      @mretzz     { return signed 0 if one of factors is 0   }
   moveq.l  #23,d3
   bset     d3,d5       { restore implied leading "1"              }
   subq.w   #8,sp       { multiplication accumulator               }
   tst.w    d0          { check for zero exponent - no leading "1" }
   bne      @mlabel5
   bclr     d3,d5       { remove it                                }
   addq.w   #1,d0       { "normalize" exponent                     }
@mlabel5:
   tst.l   d5
   beq     @mretz       { multiplying zero                         }

   moveq.l #23,d3
   bset    d3,d4        { restore implied leading "1"              }
   tst.w   d1           { check for zero exponent - no leading "1" }
   bne     @mlabel6
   bclr    d3,d4        { remove it                                }
   addq.w  #1,d1        { "normalize" exponent                     }
@mlabel6:
   tst.l   d4
   beq     @mretz       { multiply by zero                         }

   add.w   d1,d0        { add exponents,                           }
   sub.w   #BIAS4+16-8,d0 { remove excess bias, acnt for repositioning }

   clr.l   (sp)         { initialize 64-bit product to zero        }
   clr.l   4(sp)
{--------------------------------------------------------------------}
{ see Knuth, Seminumerical Algorithms, section 4.3. algorithm M      }
{--------------------------------------------------------------------}
   move.w  d4,d3
   mulu.w  d5,d3        { mulitply with bigit from multiplier      }
   move.l  d3,4(sp)     { store into result                        }

   move.l  d4,d3
   swap    d3
   mulu.w  d5,d3
   add.l   d3,2(sp)     { add to result                            }

   swap    d5           { [TOP 8 BITS SHOULD BE ZERO !]            }

   move.w  d4,d3
   mulu.w  d5,d3        { mulitply with bigit from multiplier      }
   add.l   d3,2(sp)     { store into result (no carry can occur here) }

   move.l  d4,d3
   swap    d3
   mulu.w  d5,d3
   add.l   d3,(sp)      { add to result                            }
            { [TOP 16 BITS SHOULD BE ZERO !] }
   movem.l 2(sp),d4-d5  { get the 48 valid mantissa bits           }
   clr.w   d5           { (pad to 64)                              }

   move.l  #$0000ffff,d3
@mlabel7:
   cmp.l   d3,d4        { multiply (shift) until                  }
   bhi     @mlabel8     {  1 in upper 16 result bits              }
   cmp.w   #9,d0        { give up for denormalized numbers        }
   ble     @mlabel8
   swap    d4         { (we''re getting here only when multiplying }
   swap    d5         {  with a denormalized number; there''s an   }
   move.w  d5,d4      {  eventual loss of 4 bits in the rounding   }
   clr.w   d5         {  byte -- what a pity 8-)                   }
   subq.w  #8,d0      { decrement exponent                         }
   subq.w  #8,d0
   bra     @mlabel7
@mlabel8:
   move.l  d5,d1      { get rounding bits                          }
   rol.l   #8,d1
   move.l  d1,d3      { see if sticky bit should be set            }
   and.l   #$ffffff00,d3
   beq     @mlabel9
   or.b    #1,d1      { set "sticky bit" if any low-order set      }
@mlabel9:
   addq.w  #8,sp      { remove accumulator from stack              }
   jmp     FPC_NORM_SINGLE { (result in d4)                        }

@mretz:
   addq.w  #8,sp      { release accumulator space                  }
@mretzz:
   moveq.l #0,d0      { save zero as result                        }
   lsl.w   #1,d2      { and set it sign as for d2                  }
   roxr.l  #1,d0
   movem.l (sp)+,d2-d5
   rts                { no normalizing neccessary                  }
end;


Procedure Single_Div;Assembler;
{--------------------------------------------}
{ Low-level routine to dividr two single     }
{  IEEE floating point values. Never called  }
{  directly.                                 }
{ Om Entry:                                  }
{      d1/d0 = u/v = operation to perform.   }
{ On Exit:                                   }
{      d0 = result.                          }
{  Registers destroyed: d0,d1                }
{ stack space used (and restored): 8 bytes.  }
{--------------------------------------------}
ASM
XDEF FPC_DIV_SINGLE
   { u = d1 = dividend }
   { v = d0 = divisor  }
   tst.l  d0              { check if divisor is 0                  }
   bne    @dno_exception

   move.l #$7f800000,d0
   btst   #31,d1          { transfer sign of dividend              }
   beq    @dclear
   bset   #31,d0
@dclear:
   rts

@dno_exception:
   move.l  d1,d4          { d4 = u, d5 = v                         }
   move.l  d0,d5
   movem.l d2-d5,-(sp)    { save registers                         }

   move.l  #$7fffff,d3
   move.l  d4,d0          { d0 = u.exp                             }
   and.l   d3,d4          { remove exponent from u.mantissa        }
   swap    d0
   move.w  d0,d2          { d2 = u.sign                            }

   move.l  d5,d1          { d1 = v.exp                             }
   and.l   d3,d5          { remove exponent from v.mantissa        }
   swap    d1
   eor.w   d1,d2          { d2 = u.sign ^ v.sign (in bit 15)       }

   moveq.l #15,d3
   bclr    d3,d0          { kill sign bit                          }
   bclr    d3,d1          { kill sign bit                          }
   lsr.w   #7,d0
   lsr.w   #7,d1

   moveq.l #-1,d3
   cmp.b   d3,d0          { comparison with #0xff                  }
   beq     @dlabel1       { u == NaN ;; u== Inf                    }
   cmp.b   d3,d1
   beq     @dlabel2       { v == NaN ;; v == Inf                   }
   tst.b   d0
   bne     @dlabel4       { u not zero nor denorm                  }
   tst.l   d4
   beq     @dlabel3       { 0/ ?                                   }

@dlabel4:
   tst.w   d1
   bne     @dnospec

   tst.l   d5
   bne     @dnospec
   bra     @dretinf       { x/0 -> +/- Inf                         }

@dlabel1:
   tst.l   d4             { u == NaN ?                             }
   bne     @dretnan       { NaN/ x                                 }
   cmp.b   d3,d1
   beq     @dretnan       { Inf/Inf or Inf/NaN                     }
{  bra     dretinf      ; Inf/x ; x != Inf && x != NaN             }
{--------------------------------------------------------------------}
{                  Return infinity with correct sign.                }
{--------------------------------------------------------------------}
@dretinf:
   move.l  #$ff000000,d0
   lsl.w   #1,d2
   roxr.l  #1,d0          { shift in high bit as given by d2       }
@dreturn:
   movem.l (sp)+,d2-d5
   rts

@dlabel2:
   tst.l   d5
   bne     @dretnan       { x/NaN                                  }
{   bra    dretzero   ; x/Inf -> +/- 0                             }
{--------------------------------------------------------------------}
{                  Return correct signed zero.                       }
{--------------------------------------------------------------------}
@dretzero:
   moveq.l #0,d0          { zero destination                       }
   lsl.w   #1,d2          { set X bit accordingly                  }
   roxr.l  #1,d0
   bra     @dreturn

@dlabel3:
   tst.w   d1
   bne     @dretzero      { 0/x ->+/- 0                            }
   tst.l   d4
   bne     @dretzero      { 0/x                                    }
{   bra    dretnan         0/0                                     }
{--------------------------------------------------------------------}
{                       Return NotANumber                            }
{--------------------------------------------------------------------}
@dretnan:
   move.l  d3,d0          { d3 contains 0xffffffff                 }
   lsr.l   #1,d0
   bra     @dreturn
{--------------------------------------------------------------------}
{                      End of Special Handling                       }
{--------------------------------------------------------------------}
@dnospec:
   moveq.l #23,d3
   bset    d3,d4          { restore implied leading "1"            }
   tst.w   d0             { check for zero exponent - no leading "1" }
   bne     @dlabel5
   bclr    d3,d4          { remove it                              }
   add.w   #1,d0          { "normalize" exponent                   }
@dlabel5:
   tst.l   d4
   beq     @dretzero      { dividing zero                          }

   bset    d3,d5          { restore implied leading "1"            }
   tst.w   d1             { check for zero exponent - no leading "1"}
   bne     @dlabel6
   bclr    d3,d5          { remove it                              }
   add.w   #1,d1          { "normalize" exponent                   }
@dlabel6:

   sub.w   d1,d0          { subtract exponents,                    }
   add.w   #BIAS4-8+1,d0  { add bias back in, account for shift    }
   add.w   #34,d0     { add loop offset, +2 for extra rounding bits}
                      { for denormalized numbers (2 implied by dbra)}
   move.w  #27,d1     { bit number for "implied" pos (+4 for rounding)}
   moveq.l #-1,d3     { zero quotient (for speed a one''s complement) }
   sub.l   d5,d4      { initial subtraction, u = u - v                }
@dlabel7:
   btst    d1,d3      { divide until 1 in implied position            }
   beq     @dlabel9

   add.l   d4,d4
   bcs     @dlabel8   { if carry is set, add, else subtract           }

   addx.l  d3,d3      { shift quotient and set bit zero               }
   sub.l   d5,d4      { subtract, u = u - v                           }
   dbra    d0,@dlabel7      { give up if result is denormalized        }
   bra     @dlabel9
@dlabel8:
   addx.l  d3,d3      { shift quotient and clear bit zero             }
   add.l   d5,d4      { add (restore), u = u + v                      }
   dbra    d0,@dlabel7      { give up if result is denormalized        }
@dlabel9:
   subq.w  #2,d0      { remove rounding offset for denormalized nums  }
   not.l   d3         { invert quotient to get it right               }

   clr.l   d1         { zero rounding bits                            }
   tst.l   d4         { check for exact result                        }
   beq     @dlabel10
   moveq.l #-1,d1     { prevent tie case                              }
@dlabel10:
   move.l  d3,d4      { save quotient mantissa                        }
   jmp     FPC_NORM_SINGLE { (registers on stack removed by norm_sf)  }
end;


Procedure Single_Cmp; Assembler;
{--------------------------------------------}
{ Low-level routine to compare single two    }
{  single point values..                     }
{  Never called directly.                    }
{ On Entry:                                  }
{      d1 and d0 Values to compare           }
{      d0 = first operand                    }
{ On Exit:                                   }
{      Flags according to result             }
{  Registers destroyed: d0,d1                }
{--------------------------------------------}
Asm
XDEF  FPC_CMP_SINGLE
   tst.l     d0               { check sign bit             }
   bpl       @cmplab1
   neg.l     d0               { negate                     }
   bchg      #31,d0           { toggle sign bit            }
@cmplab1:
   tst.l     d1               { check sign bit             }
   bpl       @cmplab2
   neg.l     d1               { negate                     }
   bchg      #31,d1           { toggle sign bit            }
@cmplab2:
   cmp.l     d0,d1            { compare...                 }
   rts
end;


Procedure Long2Single;Assembler;
{--------------------------------------------}
{ Low-level routine to convert a longint     }
{  to a single floating point value.         }
{ On entry: d0 = longint value to convert.   }
{ On Exit:                                   }
{      d0 = single IEEE value                }
{  Registers destroyed: d0,d1                }
{  stack space used and restored:  8 bytes   }
{--------------------------------------------}
Asm
XDEF FPC_LONG_TO_SINGLE
   movem.l  d2-d5,-(sp)  { save registers to make norm_sf happy}

   move.l   d0,d4        { prepare result mantissa          }
   move.w   #BIAS4+32-8,d0   { radix point after 32 bits    }
   move.l   d4,d2        { set sign flag                    }
   bge      @l2slabel1   { nonnegative                      }
   neg.l    d4           { take absolute value              }
@l2slabel1:
   swap     d2           { follow SINGLE_NORM conventions   }
   clr.w    d1           { set rounding = 0                 }
   jmp      FPC_NORM_SINGLE
end;


{********************************************************************}
{*                   32-bit signed arithmetic                       *}
{********************************************************************}

Procedure LongMul;Assembler;
{--------------------------------------------}
{ Low-level routine to multiply two signed   }
{  32-bit values. Never called directly.     }
{ On entry: d1,d0 = 32-bit signed values to  }
{           multiply.                        }
{ On Exit:                                   }
{      d0 = result.                          }
{  Registers destroyed: d0,d1                }
{  stack space used and restored: 10 bytes   }
{--------------------------------------------}
Asm
XDEF FPC_MUL_LONGINT
{$ifdef CPUM68020}
             cmp.b   #2,Test68000    { Are we on a 68020+ cpu                  }
             blt     @Lmulcontinue
             muls.l  d1,d0           { yes, then directly mul...               }
             rts                     { return... result in d0                  }
{$endif}             
@Lmulcontinue:
             move.l    d2,a0         { save registers                          }
             move.l    d3,a1

             move.l    d0,-(sp)
             move.l    d1,-(sp)

             movem.w   (sp)+,d0-d3   { u = d0-d1, v = d2-d3                    }

             move.w    d0,-(sp)      { sign flag                               }
             bpl       @LMul1        { is u negative ?                         }
             neg.w     d1            { yes, force it positive                  }
             negx.w    d0
@LMul1:
             tst.w     d2            { is v negative ?                         }
             bpl       @LMul2
             neg.w     d3            { yes, force it positive ...              }
             negx.w    d2
             not.w     (sp)          {  ... and modify flag word               }
@LMul2:
             ext.l     d0            { u.h <> 0 ?                              }
             beq       @LMul3
             mulu.w    d3,d0         { r  = v.l * u.h                          }
@LMul3:
             tst.w     d2            { v.h <> 0 ?                              }
             beq       @LMul4
             mulu.w    d1,d2         { r += v.h * u.l                          }
             add.w     d2,d0
@LMul4:
             swap      d0
             clr.w     d0
             mulu.w    d3,d1        { r += v.l * u.l                           }
             add.l     d1,d0
             move.l    a1,d3
             move.l    a0,d2
             tst.w     (sp)+        { should the result be negated ?           }
             bpl       @LMul5       { no, just return                          }
             neg.l     d0           { else r = -r                              }
@LMul5:
             rts
end;





Procedure LongDiv; [public,alias : 'FPC_DIV_LONGINT'];Assembler;
{--------------------------------------------}
{ Low-level routine to do signed long        }
{ division.                                  }
{ On entry: d0/d1 operation to perform       }
{ On Exit:                                   }
{      d0 = quotient                         }
{      d1 = remainder                        }
{  Registers destroyed: d0,d1                }
{  stack space used and restored: 14 bytes   }
{--------------------------------------------}
asm
{$ifdef CPUM68020}
   cmp.b       #2,Test68000  { can we use divs ?                  }
   blt         @continue
   tst.l       d1
   beq         @zerodiv2
   move.l      d6,-(sp)     { save d6 register                    }
   move.l      d1,d6
   clr.l       d1           { clr                                 }
   tst.l       d0           { check sign of d0                    }
   bpl         @posdiv
   move.l      #$ffffffff,d1{ sign extend into  d1                }
@posdiv:
   divsl.l     d6,d1:d0
   move.l      (sp)+,d6      { restore d6 register                }
   rts
{$endif}   
@continue:

   move.l      d2,a0      { save registers                        }
   move.l      d3,a1
   move.l      d4,-(sp)   { divisor = d1 = d4                     }
   move.l      d5,-(sp)   { divident = d0 = d5                    }
   move.l      d6,-(sp)   { save d6                               }

   move.l      d1,d4      { save divisor                          }
   move.l      d0,d5      { save dividend                         }

   clr.w       -(sp)      { sign flag                             }

   clr.l       d0         { prepare result                        }
   move.l      d4,d2      { get divisor                           }
   beq         @zerodiv   { divisor = 0 ?                         }
   bpl         @LDiv1     { divisor < 0 ?                         }
   neg.l       d2         { negate it                             }
   not.w       (sp)       { remember sign                         }
@LDiv1:
   move.l      d5,d1      { get dividend                          }
   bpl         @LDiv2     { dividend < 0 ?                        }
   neg.l       d1         { negate it                             }
   not.w       (sp)       { remember sign                         }
@LDiv2:
{;== case 1) divident < divisor}
   cmp.l       d2,d1      { is divident smaller then divisor ?    }
   bcs         @LDiv7     { yes, return immediately               }
{;== case 2) divisor has <= 16 significant bits}
   move.l      d4,d6      { put divisor in d6 register            }
   lsr.l       #8,d6      { rotate into low word                  }
   lsr.l       #8,d6
   tst.l       d6
   bne         @LDiv3     { divisor has only 16 bits              }
   move.w      d1,d3      { save dividend                         }
   clr.w       d1         { divide dvd.h by dvs                   }
   swap        d1
   beq         @LDiv4     { (no division necessary if dividend zero)}
   divu        d2,d1
@LDiv4:
   move.w      d1,d0      { save quotient.h                       }
   swap        d0
   move.w      d3,d1      { (d0.h = remainder of prev divu)       }
   divu        d2,d1      { divide dvd.l by dvs                   }
   move.w      d1,d0      { save quotient.l                       }
   clr.w       d1         { get remainder                         }
   swap        d1
   bra         @LDiv7     { and return                            }
{;== case 3) divisor > 16 bits (corollary is dividend > 16 bits, see case 1)}
@LDiv3:
   moveq.l     #31,d3     { loop count                            }
@LDiv5:
   add.l       d1,d1      { shift divident ...                    }
   addx.l      d0,d0      {  ... into d0                          }
   cmp.l       d2,d0      { compare with divisor                  }
   bcs         @LDiv6
   sub.l       d2,d0      { big enough, subtract                  }
   addq.w      #1,d1      { and note bit into result              }
@LDiv6:
   dbra        d3,@LDiv5
   exg         d0,d1      { put quotient and remainder in their registers}
@LDiv7:
   tst.l       d5         { must the remainder be corrected ?     }
   bpl         @LDiv8
   neg.l       d1         { yes, apply sign                       }
{ the following line would be correct if modulus is defined as in algebra}
{;   add.l   sp@(6),d1   ; algebraic correction: modulus can only be >= 0}
@LDiv8:
   tst.w       (sp)+      { result should be negative ?          }
   bpl         @LDiv9
   neg.l       d0         { yes, negate it                      }
@LDiv9:
   move.l      a1,d3
   move.l      a0,d2
   move.l      (sp)+,d6   { restore registers                   }
   move.l      (sp)+,d5
   move.l      (sp)+,d4
   rts                    { en exit : remainder = d1, quotient = d0 }
@zerodiv:
   move.l      a1,d3      { restore stack...                        }
   move.l      a0,d2
   move.w      (sp)+,d1   { remove sign word                        }
   move.l      (sp)+,d6   { restore registers.                      }
   move.l      (sp)+,d5
   move.l      (sp)+,d4
@zerodiv2:
   move.l      #200,-(sp)
   jsr         FPC_HANDLEERROR { RunError(200)                     }
   rts                    { this should never occur...             }
end;


Procedure LongMod;[public,alias : 'FPC_MOD_LONGINT'];Assembler;
{ see longdiv for info on calling convention }
asm
   jsr     FPC_DIV_LONGINT
   move.l  d1,d0         { return the remainder in d0 }
   rts
end;


{********************************************************************}
{*                  32-bit unsigned arithmetic                      *}
{********************************************************************}

{--------------------------------------------}
{ Low-level routine to do unsigned long      }
{ multiplication.                            }
{ On entry: d0,d1 operands to multiply       }
{ On Exit:                                   }
{      d0 = result                           }
{  Registers destroyed: none                 }
{  stack space used and restored:  6 bytes   }
{--------------------------------------------}
procedure mulcardinal;assembler;[public,alias : 'FPC_MUL_CARDINAL'];
asm
   movem.l D2-D3,-(A7)  { Save the registers }
   move.l  d0,d2        { get value to multiply }
   swap    d2
   tst.w   D2           { See if high word is zero }
   beq     @LMUTstSecond
   move.l  d1,d3
   swap    d3
   tst.w   D3           { Possible overflow if high word is set }
   bne     @LMUOverflow { Uhoh; go deal with it }
@LMUTough:              { A mixed bag (tougher case) }
   move.l  D0,D2        { Cache u1.l1 in D2     }
   move.l  D1,D3        { Cache u2.l2 in D3     }
   clr.l   D0           { Prepare to collect result in D0    }
   move.w  D2,D0        { Multiply l1 (in D2) and l2 (in D3) }
   mulu    D3,D0
   move.w  D2,D1        { l1 -> D1 and u1 -> D2              }
   swap    D2
   mulu    D3,D2        { (u1 * l2) -> D2                    }
   swap    D3           { u2 -> D3                           }
   mulu    D3,D1        { (u2 * l1) -> D1                    }
   add.l   D2,D1        { Add the partial results            }
   swap    D1           { Swap into high word where it belongs }
   tst.w   D1           { Test the low word, H(u2 * l1)      }
   bne     @LMUOverflow
   add.l   D1,D0        { Add the partial results to get ans. }
   bcc     @LMUDone
@LMUOverflow:
   move.l  #$FFFFFFFF,D0 { The emulation does not support overflow checking }
   bra     @LMUDone
@LMUTstSecond:
   move.l  D1,D3
   swap    D3
   tst.w   D3            { Check for simple case if high word 0  }
   bne     @LMUTough
   mulu    D1,D0         { Do a half multiply and get outta town }
@LMUDone:
                         { Result is in D0                    }
   movem.l (A7)+,D2-D3   { Restore the registers              }
end;


{--------------------------------------------}
{ Low-level routine to do unsigned long      }
{ division.                                  }
{ On entry: d0/d1 operation to perform       }
{          d0 = dividend                     }
{          d1 = divisor                      }
{ On Exit:                                   }
{      d0 = quotient                         }
{      d1 = remainder                        }
{  Registers destroyed: d0,d1                }
{  stack space used and restored: 14 bytes   }
{--------------------------------------------}
procedure divcardinal;assembler;[public,alias : 'FPC_DIV_CARDINAL'];
asm
{$ifdef CPUM68020}
    cmp.b       #2,Test68000  { Is it a 68020 or higher CPU?      }
    blt         @continue
    tst.l       d1
    beq         @zerodiv2
    move.l      d6,-(sp)     { save d6 register                   }
    move.l      d1,d6
    clr.l       d1           { clr                                }
    divul.l     d6,d1:d0
    move.l      (sp)+,d6     { restore d6 register                }
    rts                      { result in d0                       }
@zerodiv2:
    move.l      #200,-(sp)
    jsr         FPC_HANDLEERROR { RunError(200)                     }
    rts                    { this should never occur...             }
{$endif}    
@continue:
  move.l  d2,a0     { save registers                         }
  movem.l d2/d3/d4,-(sp)
  move.l  d1,d2      { get divisor                             }
  move.l  d0,d1      { get dividend                            }
  clr.l d0         { prepare result                          }
{== case 1) divident < divisor }
  cmp.l d2,d1    { is divident smaller then divisor ?      }
  bcs     @end         { yes, return immediately with zero       }
{== case 2) divisor has <= 16 significant bits }
    move.l  d1,d4        { test high word of divisor               }
    swap    d4
  tst.w d4
  bne     @2           { divisor has only 16 bits                }
  move.w  d1,d3    { save dividend                           }
  clr.w d1         { divide dvd.h by dvs                     }
  swap  d1
  beq     @01          { (no division necessary if dividend zero) }
  divu  d2,d1
@01:
    move.w  d1,d0   { save quotient.h                           }
  swap  d0
  move.w  d3,d1   { (d1.h = remainder of prev divu)           }
  divu  d2,d1   { divide dvd.l by dvs                       }
  move.w  d1,d0   { save quotient.l                           }
  clr.w d1        { get remainder                             }
  swap  d1
  bra     @end      { and return                                }
{== case 3) divisor > 16 bits (corollary is dividend > 16 bits, see case 1) }
@2:
  moveq #31,d3    { loop count                                }
@3:
  add.l d1,d1   { shift divident ...                        }
  addx.l  d0,d0   {  ... into d0                              }
  cmp.l d2,d0   { compare with divisor                      }
  bcs     @4
  sub.l d2,d0   { big enough, subtract                      }
  add.w #1,d1   { and note bit in result                    }
@4:
  dbra  d3,@3
  exg.l d0,d1   { put quotient and remainder in their registers }
@end:
  movem.l (sp)+,d2/d3/d4
end;

{--------------------------------------------}
{ Low-level routine to do unsigned long      }
{ modulo.                                    }
{    d0 := d0 mod d1                         }
{--------------------------------------------}
procedure modcardinal;assembler;[public,alias : 'FPC_MOD_CARDINAL'];
asm
   jsr     FPC_DIV_CARDINAL
   move.l  d1,d0         { return the remainder in d0 }
   rts
end;


{********************************************************************}
{*                  64-bit signed arithmetic                        *}
{********************************************************************}
function shlint64(cnt : longint; low,high: longint):int64;assembler;[public,alias : 'FPC_SHL_INT64'];
asm
    move.l     high,d0
    move.l     low,d1
    move.l     cnt,d2
    { if the count value is negative do a mod 32 on it,
      if it is not, then simply do a mod 64 on it    }
    tst.l      d2
    bmi        @NegativeCnt
    and.l      #63,d2
    bra        @Continue
@NegativeCnt
    and.l      #31,d2   
@Continue:
    move.l     #32,d3
    sub.l      d2,d3
    bgt        @Complex
    neg.l      d3
    move.l     d1,d0
    asl.l      d3,d0
    clr.l      d1
    bra        @End
@Complex:     
    asl.l      d2,d0
    move.l     d1,d4
    lsr.l      d3,d4
    or.l       d4,d0
    asl.l      d2,d1
@End:
                        { d1 = low / d0 = high            }
end;

function shrint64(cnt : longint; low,high: longint):int64;assembler;[public,alias : 'FPC_SHR_INT64'];
{ used registers are saved by the code generator }
asm
        move.l    high,d0
        move.l    low,d1
        move.l    cnt,d2
        { if the count value is negative do a mod 32 on it,
          if it is not, then simply do a mod 64 on it    }
        tst.l     d2
        bmi       @NegativeCnt
        and.l     #63,d2
        bra       @Continue
@NegativeCnt
        and.l     #31,d2   
@Continue:
        move.l    #32,d3
        sub.l     d2,d3
        bgt       @Complex
        neg.l     d3
        move.l    d0,d1
        lsr.l     d3,d1
        clr.l     d0
        bra       @End
@Complex:     
        lsr.l     d2,d1
        move.l    d0,d4
        asl.l     d3,d4
        or.l      d4,d1
        lsr.l     d2,d0
@End:
                            { d1 = low / d0 = high            }
end;



{
  $Log: lowmath.inc,v $
  Revision 1.1.2.12  2002/09/29 14:00:04  carl
    * more compatibility fixes for shifts

  Revision 1.1.2.11  2002/09/21 14:03:18  carl
     * fix for 68000/68020 support

  Revision 1.1.2.10  2002/09/12 19:48:09  carl
    * use correct calling conventions for 64-bit results

  Revision 1.1.2.9  2001/07/30 00:01:28  carl
  - removed trunc code (moved to genmath.inc)
  * bugfix of Trunc() for big-endian machines

  Revision 1.1.2.8  2001/07/27 01:50:00  carl
  + TRUNC() now works in emulation and non-emulation mode

  Revision 1.1.2.7  2001/06/13 03:03:15  carl
  + cardinal support fully implemented and tested

  Revision 1.1.2.6  2001/06/12 02:06:42  carl
  + cardinal multiplication implemented

  Revision 1.1.2.5  2001/05/26 20:24:52  carl
  + implemented 64-bit shifts

  Revision 1.1.2.4  2001/05/21 16:44:44  carl
  * renamed according to FPC rules

  Revision 1.1.2.3  2001/05/18 17:50:19  carl
  * made the internal routines public (required for compiling)

  Revision 1.1.2.2  2001/05/17 01:34:31  carl
  + clean up
  + added missing routines
  * D6 is now saved as it is supposed to

  Revision 1.1.2.1  2001/05/09 03:32:17  carl
  * renamed internal routines according to FPC rules

  Revision 1.1  2000/07/13 06:30:55  michael
  + Initial import

  Revision 1.6  2000/01/07 16:41:42  daniel
    * copyright 2000

  Revision 1.5  2000/01/07 16:32:28  daniel
    * copyright 2000 added

  Revision 1.4  1998/10/13 08:00:04  pierre
    * some bugs related to FPC_ prefix fixed
    * problems with pbyte sometimes defined and sometimes not for rttip.inc solved

  Revision 1.3  1998/07/01 14:28:32  carl
    * LONGDIV bugfixed with signed and modulo
    * LONGDIV bugfix when divisor is less then 16 bits

  Revision 1.1.1.1  1998/03/25 11:18:44  root
  * Restored version

  Revision 1.5  1998/01/26 12:01:47  michael
  + Added log at the end

}
