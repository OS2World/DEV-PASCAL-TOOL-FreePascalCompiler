{
    $Id: cpu.pp,v 1.1 2000/07/13 06:30:41 michael Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl

    This unit contains some routines to get informations about the
    processor

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit cpu;
  interface

    { returns true, if the processor supports the cpuid instruction }
    function cpuid_support : boolean;

    { returns true, if floating point is done by an emulator }
    function floating_point_emulation : boolean;

    { returns the contents of the cr0 register }
    function cr0 : longint;


  implementation

{$ASMMODE INTEL}


    function cpuid_support : boolean;assembler;
      {
        Check if the ID-flag can be changed, if changed then CpuID is supported.
        Tested under go32v1 and Linux on c6x86 with CpuID enabled and disabled (PFV)
      }
      asm
         pushf
         pushf
         pop     eax
         mov     ebx,eax
         xor     eax,200000h
         push    eax
         popf
         pushf
         pop     eax
         popf
         and     eax,200000h
         and     ebx,200000h
         cmp     eax,ebx
         setnz   al
      end;


    function cr0 : longint;assembler;
      asm
         DB 0Fh,20h,0C0h
         { mov eax,cr0
           special registers are not allowed in the assembler
                parsers }
      end;


    function floating_point_emulation : boolean;
      begin
         {!!!! I don't know currently the position of the EM flag }
         { $4 after Ralf Brown's list }
         floating_point_emulation:=(cr0 and $4)<>0;
      end;

end.

{
  $Log: cpu.pp,v $
  Revision 1.1  2000/07/13 06:30:41  michael
  + Initial import

  Revision 1.7  2000/02/09 16:59:29  peter
    * truncated log

  Revision 1.6  2000/01/07 16:41:32  daniel
    * copyright 2000

}
