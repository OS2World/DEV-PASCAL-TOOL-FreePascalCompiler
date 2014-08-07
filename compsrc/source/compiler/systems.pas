{
    $Id: systems.pas,v 1.1.2.63 2003/03/24 11:02:53 pierre Exp $
    Copyright (C) 1998-2000 by Florian Klaempfl

    This unit contains information about the target systems supported
    (these are not processor specific)

    This progsam is free software; you can redistribute it and/or modify
    iu under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge- MA 02139, USA.

 ****************************************************************************
}
unit systems;

{$ifdef FPC}
  {$ifdef PACKENUMFIXED}
    {$PACKENUM 1}
  {$endif}
{$endif}

  interface

   type
       tendian = (endian_little,endian_big);

     {
       IMPORTANT NOTE:
       The value of this enumeration is stored in PPU files.
       Therefore adding new CPU targets should not change the
       values of the pre-existing targets. (CEC)
       FURTHERMORE : Make sure that this branch values, are
       consistant with the main branch version always.
     }
       ttargetcpu=
       (
             no_cpu,                   { 0 }
             i386,                     { 1 }
             m68k,                     { 2 }
             alpha,                    { 3 }
             powerpc,                  { 4 }
             sparc,                    { 5 }
             vm                        { 6 }
       );

       tprocessors = (no_processor
            ,Class386,ClassP5,ClassP6
            ,MC68000,MC68100,MC68020
       );

       tsection=(sec_none,
         sec_code,sec_data,sec_bss,
         sec_idata2,sec_idata4,sec_idata5,sec_idata6,sec_idata7,sec_edata,
         sec_stab,sec_stabstr
       );

     type
       tasmmode= (asmmode_none
            ,asmmode_i386_direct,asmmode_i386_att,asmmode_i386_intel
            ,asmmode_m68k_mot,asmmode_alpha_direct,asmmode_powerpc_direct
       );
     const
       {$ifdef i386}  i386asmmodecnt=3;  {$else} i386asmmodecnt=0; {$endif}
       {$ifdef m68k}  m68kasmmodecnt=1;  {$else} m68kasmmodecnt=0; {$endif}
       {$ifdef alpha} alphaasmmodecnt=1; {$else} alphaasmmodecnt=0; {$endif}
       {$ifdef powerpc} powerpcasmmodecnt=1; {$else} powerpcasmmodecnt=0; {$endif}
       asmmodecnt=i386asmmodecnt+m68kasmmodecnt+Alphaasmmodecnt+powerpcasmmodecnt+1;

     { IMPORTANT NOTE:
       the integer value of this enum is stored in PPU
       files to recognize the target, so if you add new targets
       allways add them at end PM
       FURTHERMORE : Make sure that this branch values, are
       consistant with the main branch version always. (CEC)
       }
     type
       ttarget =
       (
             target_none,               { 0 }
             target_i386_GO32V1,        { 1 }
             target_i386_GO32V2,        { 2 }
             target_i386_linux,         { 3 }
             target_i386_OS2,           { 4 }
             target_i386_Win32,         { 5 }
             target_i386_freebsd,       { 6 }
             target_m68k_Amiga,         { 7 }
             target_m68k_Atari,         { 8 }
             target_m68k_Mac,           { 9 }
             target_m68k_linux,         { 10 }
             target_m68k_PalmOS,        { 11 }
             target_alpha_linux,        { 12 }
             target_powerpc_linux,      { 13 }
             target_powerpc_macos,      { 14 }
             target_i386_sunos,         { 15 }
             target_i386_beos,          { 16 }
             target_i386_netbsd,        { 17 }
             target_m68k_netbsd,        { 18 }
             target_i386_Netware,       { 19 }
             target_i386_qnx,           { 20 }
             target_i386_wdosx,         { 21 }
             target_sparc_sunos,        { 22 }
             target_sparc_linux,        { 23 }
             target_i386_openbsd,       { 24 }
             target_m68k_openbsd,       { 25 }
             system_x86_64_linux,       { 26 }
             system_powerpc_macosx      { 27 }
       );

       ttargetflags = (tf_none,
            tf_supports_stack_checking,tf_need_export,tf_needs_isconsole
{$ifdef m68k}
            ,tf_code_small,tf_static_a5_based
{$endif m68k}
       );

     const
       { alias for supported_target field in tasminfo }
       target_any = target_none;

       {$ifdef i386} i386targetcnt=13; {$else} i386targetcnt=0; {$endif}
       {$ifdef m68k} m68ktargetcnt=9; {$else} m68ktargetcnt=0; {$endif}
       {$ifdef alpha} alphatargetcnt=1; {$else} alphatargetcnt=0; {$endif}
       {$ifdef powerpc} powerpctargetcnt=2; {$else} powerpctargetcnt=0; {$endif}
       targetcnt=i386targetcnt+m68ktargetcnt+alphatargetcnt+powerpctargetcnt-1;

     type
       tasm = (as_none
            ,as_i386_as,as_i386_as_aout,as_i386_asw,
            as_i386_nasmcoff,as_i386_nasmwin32,
            as_i386_nasmelf,as_i386_nasmobj,
            as_i386_tasm,as_i386_masm,
            as_i386_dbg,as_i386_coff,as_i386_pecoff,
            as_m68k_as,as_m68k_asbsd,as_m68k_gas,
            as_m68k_mit,as_m68k_mot,as_m68k_mpw,
            as_alpha_as,as_powerpc_as,as_powerpc_mpw,
            as_m68k_palm,as_i386_openbsd_aout
       );
       { binary assembler writers, needed to test for -a }
     const
       {$ifdef i386} i386asmcnt=13; {$else} i386asmcnt=0; {$endif}
       {$ifdef m68k} m68kasmcnt=7; {$else} m68kasmcnt=0; {$endif}
       {$ifdef alpha} alphaasmcnt=1; {$else} alphaasmcnt=0; {$endif}
       {$ifdef powerpc} powerpcasmcnt=2; {$else} powerpcasmcnt=0; {$endif}
       asmcnt=i386asmcnt+m68kasmcnt+alphaasmcnt+powerpcasmcnt+1;

       binassem : set of tasm = [
         as_i386_dbg,as_i386_coff,as_i386_pecoff
       ];

     type
       tar = (ar_none
            ,ar_i386_ar,ar_i386_arw
            ,ar_m68k_ar,ar_alpha_ar,ar_powerpc_ar
       );
     const
       {$ifdef i386} i386arcnt=2; {$else} i386arcnt=0; {$endif}
       {$ifdef m68k} m68karcnt=1; {$else} m68karcnt=0; {$endif}
       {$ifdef alpha} alphaarcnt=1; {$else} alphaarcnt=0; {$endif}
       {$ifdef powerpc} powerpcarcnt=1; {$else} powerpcarcnt=0; {$endif}
       arcnt=i386arcnt+m68karcnt+alphaarcnt+powerpcarcnt+1;

     type
       tres = (res_none
            ,res_i386_windres,res_m68k_mpw,res_powerpc_mpw, res_i386_emx,res_m68k_palmos
       );
     const
       {$ifdef i386} i386rescnt=2; {$else} i386rescnt=0; {$endif}
       {$ifdef m68k} m68krescnt=2; {$else} m68krescnt=0; {$endif}
       {$ifdef alpha} alpharescnt=0; {$else} alpharescnt=0; {$endif}
       {$ifdef powerpc} powerpcrescnt=1; {$else} powerpcrescnt=0; {$endif}
       rescnt=i386rescnt+m68krescnt+alpharescnt+powerpcrescnt+1;

     type
       tos = ( os_none,
               os_i386_GO32V1,
               os_i386_GO32V2,
               os_i386_Linux,
               os_i386_OS2,
               os_i386_Win32,
               os_i386_freeBSD,
               os_m68k_Amiga,
               os_m68k_Atari,
               os_m68k_Mac,
               os_m68k_Linux,
               os_m68k_PalmOS,
               os_alpha_linux,
               os_powerpc_linux,
               os_powerpc_macos,
               os_i386_sunos,
               os_i386_beos,
               os_i386_netBSD,
               os_m68k_netBSD,
               os_i386_qnx,
{            os_i386_wdosx,
            os_sparc_sunos,
            os_sparc_linux,}
               os_i386_openbsd,
               os_m68k_openbsd
       );
     const
       i386oscnt=11;
       m68koscnt=7;
       alphaoscnt=1;
       powerpcoscnt=2;
       oscnt=i386oscnt+m68koscnt+alphaoscnt+powerpcoscnt+1;

   type
       tosinfo = packed record
          id           : tos;
          name         : string[30];
          shortname    : string[9];
          sharedlibext : string[10];
          staticlibext,
          sourceext,
          pasext,
          exeext,
          defext,
          scriptext : string[4];
          staticlibprefix : string[4];
          sharedlibprefix : string[4];
          Cprefix   : string[2];
          newline   : string[2];
          { Char use to separate pathes }
          dirsep    : char;
          { Does case import for file search }
          files_case_relevent : boolean;
          endian    : tendian;
          {longint this is a little overkill no ?? but 256 is possible one day }
          stackalignment : word;
          maxCrecordalignment : word;
          size_of_pointer : byte;
          size_of_longint : byte;
          use_bound_instruction : boolean;
          use_function_relative_addresses : boolean;
       end;

       tasminfo = packed record
          id          : tasm;
          idtxt       : string[10];
          asmbin      : string[8];
          asmcmd      : string[50];
          supported_target : ttarget;
          allowdirect,
          externals,
          needar,
          labelprefix_only_inside_procedure : boolean;
          labelprefix : string[3];
          comment     : string[2];
          secnames    : array[tsection] of string[20];
       end;

       tarinfo = packed record
          id      : tar;
          arcmd   : string[50];
       end;

       tresinfo = packed record
          id      : tres;
          resbin  : string[8];
          rescmd  : string[50];
       end;

       ttargetinfo = packed record
          target      : ttarget;
          flags       : set of ttargetflags;
          cpu         : ttargetcpu;
          short_name  : string[8];
          unit_env    : string[12];
          system_unit : string[8];
          smartext,
          unitext,
          unitlibext,
          asmext,
          objext,
          resext,
          resobjext,
          exeext      : string[4];
          os          : tos;
          assem       : tasm;
          assemsrc    : tasm; { default source writing assembler }
          ar          : tar;
          res         : tres;
          heapsize,
          maxheapsize,
          stacksize   : longint;
       end;

       tasmmodeinfo=packed record
          id    : tasmmode;
          idtxt : string[8];
       end;

    var
       target_cpu  : ttargetcpu;
       target_info : ttargetinfo;
       target_os   : tosinfo;
       target_asm  : tasminfo;
       target_ar   : tarinfo;
       target_res  : tresinfo;
       target_path : string[12]; { for rtl/<X>/,fcl/<X>/, etc. }
       source_os   : tosinfo;

    function set_target_os(t:tos):boolean;
    function set_target_asm(t:tasm):boolean;
    function set_target_ar(t:tar):boolean;
    function set_target_res(t:tres):boolean;
    function set_target_info(t:ttarget):boolean;

    function set_string_target(s : string) : boolean;
    function set_string_asm(s : string) : boolean;
    function set_string_asmmode(s:string;var t:tasmmode):boolean;

    procedure InitSystems;


implementation

    const

{****************************************************************************
                                 OS Info
****************************************************************************}
       os_infos : array[1..oscnt] of tosinfo = (
          (
            id     : os_none;
            name         : 'No operating system';
            shortname    : 'none'
          ),
          (
            id     : os_i386_go32v1;
            name         : 'GO32 V1 DOS extender';
            shortname    : 'go32v1';
            sharedlibext : '.dll';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';      { No .exe, the linker only output a.out ! }
            defext       : '.def';
            scriptext    : '.bat';
            staticlibprefix : '';
            sharedlibprefix : '';
            Cprefix      : '_';
            newline      : #13#10;
            dirsep       : '\';
            files_case_relevent : false;
            endian       : endian_little;
            stackalignment : 2;
            maxCrecordalignment : 4;
            size_of_pointer : 4;
            size_of_longint : 4;
            use_bound_instruction : false;
            use_function_relative_addresses : true
          ),
          (
            id     : os_i386_go32v2;
            name         : 'GO32 V2 DOS extender';
            shortname    : 'go32v2';
            sharedlibext : '.dll';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '.exe';
            defext       : '.def';
            scriptext    : '.bat';
            staticlibprefix : '';
            sharedlibprefix : '';
            Cprefix      : '_';
            newline      : #13#10;
            dirsep       : '\';
            files_case_relevent : false;
            endian       : endian_little;
            stackalignment : 2;
            maxCrecordalignment : 4;
            size_of_pointer : 4;
            size_of_longint : 4;
            use_bound_instruction : false;
            use_function_relative_addresses : true
          ),
          (
            id     : os_i386_linux;
            name         : 'Linux for i386';
            shortname    : 'linux';
            sharedlibext : '.so';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';
            defext       : '.def';
            scriptext    : '.sh';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            files_case_relevent : true;
            endian       : endian_little;
            stackalignment : 4;
            maxCrecordalignment : 4;
            size_of_pointer : 4;
            size_of_longint : 4;
            use_bound_instruction : false;
            use_function_relative_addresses : true
          ),
          (
            id     : os_i386_FreeBSD;
            name         : 'FreeBSD/ELF for i386';
            shortname    : 'freebsd';
            sharedlibext : '.so';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';
            defext       : '.def';
            scriptext    : '.sh';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            files_case_relevent : true;
            endian       : endian_little;
            stackalignment : 4;
            maxCrecordalignment : 4;
            size_of_pointer : 4;
            size_of_longint : 4;
            use_bound_instruction : false;
            use_function_relative_addresses : true
          ),
          (
            id     : os_i386_os2;
            name         : 'OS/2 via EMX';
            shortname    : 'os2';
            sharedlibext : '.ao2';
            staticlibext : '.ao2';
            sourceext    : '.pas';
            pasext       : '.pp';
            exeext       : '.exe';
            defext       : '.def';
            scriptext    : '.cmd';
            staticlibprefix : '';
            sharedlibprefix : '';
            Cprefix      : '_';
            newline      : #13#10;
            dirsep       : '\';
            files_case_relevent : false;
            endian       : endian_little;
            stackalignment : 4;
            maxCrecordalignment : 4;
            size_of_pointer : 4;
            size_of_longint : 4;
            use_bound_instruction : false;
            use_function_relative_addresses : false
          ),
          (
            id     : os_i386_win32;
            name         : 'Win32 for i386';
            shortname    : 'win32';
            sharedlibext : '.dll';
            staticlibext : '.aw';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '.exe';
            defext       : '.def';
            scriptext    : '.bat';
            staticlibprefix : 'libp';
            sharedlibprefix : '';
            Cprefix      : '_';
            newline      : #13#10;
            dirsep       : '\';
            files_case_relevent : false;
            endian       : endian_little;
            stackalignment : 4;
            maxCrecordalignment : 16;
            size_of_pointer : 4;
            size_of_longint : 4;
            use_bound_instruction : false;
            use_function_relative_addresses : true
          ),
          (
            id     : os_m68k_amiga;
            name         : 'Commodore Amiga';
            shortname    : 'amiga';
            sharedlibext : '.library';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';
            defext       : '';
            scriptext    : '';
            staticlibprefix : 'lib';
            sharedlibprefix : '';
            Cprefix      : '_';
            newline      : #10;
            dirsep       : '/';
            files_case_relevent : false;
            endian       : endian_big;
            stackalignment : 2;
            maxCrecordalignment : 2;
            size_of_pointer : 4;
            size_of_longint : 4;
            use_bound_instruction : false;
            use_function_relative_addresses : false
          ),
          (
            id     : os_m68k_atari;
            name         : 'Atari ST/STE';
            shortname    : 'atari';
            sharedlibext : '';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '.ttp';
            defext       : '';
            scriptext    : '';
            staticlibprefix : '';
            sharedlibprefix : '';
            Cprefix      : '_';
            newline      : #10;
            dirsep       : '\';
            files_case_relevent : false;
            endian       : endian_big;
            stackalignment : 2;
            maxCrecordalignment : 2;
            size_of_pointer : 4;
            size_of_longint : 4;
            use_bound_instruction : false;
            use_function_relative_addresses : false
          ),
          (
            id     : os_m68k_mac;
            name         : 'Macintosh m68k';
            shortname    : 'mac';
            sharedlibext : 'Lib';
            staticlibext : 'Lib';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';
            defext       : '';
            scriptext    : '';
            staticlibprefix : '';
            sharedlibprefix : '';
            Cprefix      : '_';
            newline      : #13;
            dirsep       : ':'; { Is this correct ?? PM }
            files_case_relevent : true;
            endian       : endian_big;
            stackalignment : 2;
            maxCrecordalignment : 4; { not checked }
            size_of_pointer : 4;
            size_of_longint : 4;
            use_bound_instruction : false;
            use_function_relative_addresses : false
          ),
          (
            id     : os_m68k_linux;
            name         : 'Linux for m68k';
            shortname    : 'linux';
            sharedlibext : '.so';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';
            defext       : '';
            scriptext    : '.sh';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            files_case_relevent : true;
            endian       : endian_big;
            stackalignment : 4;
            maxCrecordalignment : 2;
            size_of_pointer : 4;
            size_of_longint : 4;
            use_bound_instruction : false;
            use_function_relative_addresses : true
          ),
          (
            id     : os_m68k_palmos;
            name         : 'PalmOS';
            shortname    : 'palmos';
            sharedlibext : '.so';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';
            defext       : '';
            scriptext    : '.sh';
            staticlibprefix : 'libp';
            sharedlibprefix : '';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/'; { Does this notion even exist on PalmOS ?? PM }
            files_case_relevent : true;
            endian       : endian_big;
            stackalignment : 2;
            maxCrecordalignment : 2;
            size_of_pointer : 4;
            size_of_longint : 4;
            use_bound_instruction : false;
            use_function_relative_addresses : true
          ),
          (
            id     : os_alpha_linux;
            name         : 'Linux for Alpha';
            shortname    : 'axplinux';
            sharedlibext : '.so';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';
            defext       : '.def';
            scriptext    : '.sh';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            files_case_relevent : true;
            endian       : endian_little;
            stackalignment : 8;
            maxCrecordalignment : 32;
            size_of_pointer : 8;
            size_of_longint : 4;
            use_bound_instruction : false;
            use_function_relative_addresses : true
          ),
          (
            id     : os_powerpc_linux;
            name         : 'Linux for PowerPC';
            shortname    : 'linuxppc';
            sharedlibext : '.so';
            staticlibext : '.s';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';
            defext       : '.def';
            scriptext    : '.sh';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            files_case_relevent : true;
            endian       : endian_big;
            stackalignment : 8;
            maxCrecordalignment : 32;
            size_of_pointer : 4;
            size_of_longint : 4;
            use_bound_instruction : false;
            use_function_relative_addresses : true
          ),
          (
            id     : os_powerpc_macos;
            name         : 'MacOs (PowerPC)';
            shortname    : 'MacOs/PPC';
            sharedlibext : 'Lib';
            staticlibext : 'Lib';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';
            defext       : '';
            scriptext    : '';
            staticlibprefix : '';
            sharedlibprefix : '';
            Cprefix      : '';
            newline      : #13;
            dirsep       : ':';
            files_case_relevent : true;
            endian       : endian_big;
            stackalignment : 8;
            maxCrecordalignment : 32;
            size_of_pointer : 4;
            size_of_longint : 4;
            use_bound_instruction : false;
            use_function_relative_addresses : true
          ),
          (
            id     : os_i386_sunos;
            name         : 'SunOS/ELF for i386';
            shortname    : 'sunos';
            sharedlibext : '.so';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';
            defext       : '.def';
            scriptext    : '.sh';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            files_case_relevent : true;
            endian       : endian_little;
            stackalignment : 4;
            maxCrecordalignment : 4;
            size_of_pointer : 4;
            size_of_longint : 4;
            use_bound_instruction : false;
            use_function_relative_addresses : true
          ),
          (
            id     : os_i386_beos;
            name         : 'BeOS for i386';
            shortname    : 'beos';
            sharedlibext : '.so';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';
            defext       : '.def';
            scriptext    : '.sh';
            staticlibprefix : 'lib';
            sharedlibprefix : 'so';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            files_case_relevent : true;
            endian       : endian_little;
            stackalignment : 4;
            maxCrecordalignment : 4;
            size_of_pointer : 4;
            size_of_longint : 4;
            use_bound_instruction : false;
            use_function_relative_addresses : true
          ),
          (
            id     : os_i386_NetBSD;
            name         : 'NetBSD for i386';
            shortname    : 'netbsd';
            sharedlibext : '.so';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';
            defext       : '.def';
            scriptext    : '.sh';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            files_case_relevent : true;
            endian       : endian_little;
            stackalignment : 4;
            maxCrecordalignment : 4;
            size_of_pointer : 4;
            size_of_longint : 4;
            use_bound_instruction : false;
            use_function_relative_addresses : true
          ),
          (
            id     : os_m68k_NetBSD;
            name         : 'NetBSD/ELF for m68k';
            shortname    : 'netbsd';
            sharedlibext : '.so';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';
            defext       : '';
            scriptext    : '.sh';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            Cprefix      : ''; { m68k netbsd still uses aout format PM }
              { Since 1.5.3  ELF, MvdV}
            newline      : #10;
            dirsep       : '/';
            files_case_relevent : true;
            endian       : endian_big;
            stackalignment : 4;
            maxCrecordalignment : 8;
            size_of_pointer : 4;
            size_of_longint : 4;
            use_bound_instruction : false;
            use_function_relative_addresses : true
          ),
          (
            id     : os_i386_qnx;
            name         : 'QNX/ELF for i386';
            shortname    : 'qnx';
            sharedlibext : '.so';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';
            defext       : '.def';
            scriptext    : '.sh';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            files_case_relevent : true;
            endian       : endian_little;
            stackalignment : 4;
            maxCrecordalignment : 4;
            size_of_pointer : 4;
            size_of_longint : 4;
            use_bound_instruction : false;
            use_function_relative_addresses : true
          ),
          (
            id     : os_i386_OpenBSD;
            name         : 'OpenBSD for i386';
            shortname    : 'openbsd';
            sharedlibext : '.so';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';
            defext       : '.def';
            scriptext    : '.sh';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            Cprefix      : '_';     { OpenBSD 3.1 is still a.out}
            newline      : #10;
            dirsep       : '/';
            files_case_relevent : true;
            endian       : endian_little;
            stackalignment : 4;
            maxCrecordalignment : 4;
            size_of_pointer : 4;
            size_of_longint : 4;
            use_bound_instruction : false;
            use_function_relative_addresses : false
          ),
          (
            id     : os_m68k_OpenBSD;
            name         : 'OpenBSD for m68k';
            shortname    : 'openbsd';
            sharedlibext : '.so';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';
            defext       : '';
            scriptext    : '.sh';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            Cprefix      : '_';
            newline      : #10;
            dirsep       : '/';
            files_case_relevent : true;
            endian       : endian_big;
            stackalignment : 4;
            maxCrecordalignment : 2;
            size_of_pointer : 4;
            size_of_longint : 4;
            use_bound_instruction : false;
            use_function_relative_addresses : false
          )
          );

{****************************************************************************
                             Assembler Info
****************************************************************************}

       as_infos : array[1..asmcnt] of tasminfo = (
          (
            id     : as_none;
            idtxt  : 'no'
          )
{$ifdef i386}
          ,(
            id     : as_i386_as;
            idtxt  : 'AS';
            asmbin : 'as';
            asmcmd : '-o $OBJ $ASM';
            supported_target : target_any;
            allowdirect : true;
            externals : false;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : '.L';
            comment : '# ';
            secnames : ('',
              '.text','.data','.bss',
              '','','','','','',
              '.stab','.stabstr')
          )
          ,(
            id     : as_i386_as_aout;
            idtxt  : 'AS_AOUT';
            asmbin : 'as';
            asmcmd : '-o $OBJ $ASM';
            supported_target : target_i386_os2;
            allowdirect : true;
            externals : false;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : 'L';
            comment : '# ';
            secnames : ('',
              '.text','.data','.bss',
              '','','','','','',
              '.stab','.stabstr')
          )
          ,(
            id     : as_i386_asw;
            idtxt  : 'ASW';
            asmbin : 'asw';
            asmcmd : '-o $OBJ $ASM';
            supported_target : target_i386_win32;
            allowdirect : true;
            externals : false;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : '.L';
            comment : '# ';
            secnames : ('',
              '.text','.data','.section .bss',
              '.section .idata$2','.section .idata$4','.section .idata$5',
                '.section .idata$6','.section .idata$7','.section .edata',
              '.stab','.stabstr')
          )
          ,(
            id     : as_i386_nasmcoff;
            idtxt  : 'NASMCOFF';
            asmbin : 'nasm';
            asmcmd : '-f coff -o $OBJ $ASM';
            supported_target : target_i386_go32v2;
            allowdirect : true;
            externals : true;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : '..@';
            comment : '; ';
            secnames : ('',
              '.text','.data','.bss',
              '.idata2','.idata4','.idata5','.idata6','.idata7','.edata',
              '.stab','.stabstr')
          )
          ,(
            id     : as_i386_nasmwin32;
            idtxt  : 'NASMWIN32';
            asmbin : 'nasm';
            asmcmd : '-f win32 -o $OBJ $ASM';
            supported_target : target_i386_win32;
            allowdirect : true;
            externals : true;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : '..@';
            comment : '; ';
            secnames : ('',
              '.text','.data','.bss',
              '.idata2','.idata4','.idata5','.idata6','.idata7','.edata',
              '.stab','.stabstr')
          )
          ,(
            id     : as_i386_nasmelf;
            idtxt  : 'NASMELF';
            asmbin : 'nasm';
            asmcmd : '-f elf -o $OBJ $ASM';
            supported_target : target_i386_linux;
            allowdirect : true;
            externals : true;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : '..@';
            comment : '; ';
            secnames : ('',
              '.text','.data','.bss',
              '.idata2','.idata4','.idata5','.idata6','.idata7','.edata',
              '.stab','.stabstr')
          )
          ,(
            id     : as_i386_nasmobj;
            idtxt  : 'NASMOBJ';
            asmbin : 'nasm';
            asmcmd : '-f obj -o $OBJ $ASM';
            supported_target : target_any; { what should I write here ?? }
            allowdirect : true;
            externals : true;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : '..@';
            comment : '; ';
            secnames : ('',
              '.text','.data','.bss',
              '.idata2','.idata4','.idata5','.idata6','.idata7','.edata',
              '.stab','.stabstr')
          )
          ,(
            id     : as_i386_tasm;
            idtxt  : 'TASM';
            asmbin : 'tasm';
            asmcmd : '/m2 /ml $ASM $OBJ';
            supported_target : target_any; { what should I write here ?? }
            allowdirect : true;
            externals : true;
            needar : true;
            labelprefix_only_inside_procedure : true;
            labelprefix : '@@';
            comment : '; ';
            secnames : ('',
              'CODE','DATA','BSS',
              '','','','','','',
              '','')
          )
          ,(
            id     : as_i386_masm;
            idtxt  : 'MASM';
            asmbin : 'masm';
            asmcmd : '/c /Cp $ASM /Fo$OBJ';
            supported_target : target_any; { what should I write here ?? }
            allowdirect : true;
            externals : true;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : '@@';
            comment : '; ';
            secnames : ('',
              'CODE','DATA','BSS',
              '','','','','','',
              '','')
          )
          ,(
            id     : as_i386_dbg;
            idtxt  : 'DBG';
            asmbin : '';
            asmcmd : '';
            supported_target : target_any;
            allowdirect : false;
            externals : true;
            needar : false;
            labelprefix_only_inside_procedure : false;
            labelprefix : 'L';
            comment : '';
            secnames : ('',
              '.text','.data','.bss',
              '.idata$2','.idata$4','.idata$5','.idata$6','.idata$7','.edata',
              '.stab','.stabstr')
          )
          ,(
            id     : as_i386_coff;
            idtxt  : 'COFF';
            asmbin : '';
            asmcmd : '';
            supported_target : target_i386_go32v2;
            allowdirect : false;
            externals : true;
            needar : false;
            labelprefix_only_inside_procedure : false;
            labelprefix : '.L';
            comment : '';
            secnames : ('',
              '.text','.data','.bss',
              '.idata$2','.idata$4','.idata$5','.idata$6','.idata$7','.edata',
              '.stab','.stabstr')
          )
          ,(
            id     : as_i386_pecoff;
            idtxt  : 'PECOFF';
            asmbin : '';
            asmcmd : '';
            supported_target : target_i386_win32;
            allowdirect : false;
            externals : true;
            needar : false;
            labelprefix_only_inside_procedure : false;
            labelprefix : '.L';
            comment : '';
            secnames : ('',
              '.text','.data','.bss',
              '.idata$2','.idata$4','.idata$5','.idata$6','.idata$7','.edata',
              '.stab','.stabstr')
          )
          ,(
            id     : as_i386_openbsd_aout;
            idtxt  : 'AS_OPENBSD';
            asmbin : 'as';
            asmcmd : '-o $OBJ $ASM';
            supported_target : target_i386_openbsd;
            allowdirect : true;
            externals : false;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : 'L';
            comment : '# ';
            secnames : ('',
              '.text','.data','.bss',
              '','','','','','',
              '.stab','.stabstr')
          )
{$endif i386}
{$ifdef m68k}
          ,(
            id     : as_m68k_as;
            idtxt  : 'AS';
            asmbin : 'as';
            asmcmd : '--register-prefix-optional -o $OBJ $ASM';
            supported_target : target_any;
            allowdirect : true;
            externals : false;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : '.L';
            comment : '# ';
            secnames : ('',
              '.text','.data','.bss',
              '.idata$2','.idata$4','.idata$5','.idata$6','.idata$7','.edata',
              '.stab','.stabstr')
          )
          ,(
            id     : as_m68k_asbsd;
            idtxt  : 'ASBSD';
            asmbin : 'as';
            asmcmd : '-o $OBJ $ASM';
            supported_target : target_any;
            allowdirect : true;
            externals : false;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : 'L';
            comment : '# ';
            secnames : ('',
              '.text','.data','.bss',
              '.idata$2','.idata$4','.idata$5','.idata$6','.idata$7','.edata',
              '.stab','.stabstr')
          )
          ,(
            id     : as_m68k_gas;
            idtxt  : 'GAS';
            asmbin : 'as'; { Gas for the Amiga}
            asmcmd : '--register-prefix-optional -o $OBJ $ASM';
            supported_target : target_any;
            allowdirect : true;
            externals : false;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : 'L';
            comment : '| ';
            secnames : ('',
              '.text','.data','.bss',
              '.idata$2','.idata$4','.idata$5','.idata$6','.idata$7','.edata',
              '.stab','.stabstr')
          )
          ,(
            id     : as_m68k_mit;
            idtxt  : 'MIT';
            asmbin : '';
            asmcmd : '-o $OBJ $ASM';
            supported_target : target_any;
            allowdirect : true;
            externals : false;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : '.L';
            comment : '| ';
            secnames : ('',
              '.text','.data','.bss',
              '.idata$2','.idata$4','.idata$5','.idata$6','.idata$7','.edata',
              '.stab','.stabstr')
          )
          ,(
            id     : as_m68k_mot;
            idtxt  : 'MOT';
            asmbin : '';
            asmcmd : '-o $OBJ $ASM';
            supported_target : target_any;
            allowdirect : true;
            externals : false;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : '__L';
            comment : '| ';
            secnames : ('',
              '.text','.data','.bss',
              '.idata$2','.idata$4','.idata$5','.idata$6','.idata$7','.edata',
              '.stab','.stabstr')
          )
          ,(
            id     : as_m68k_mpw;
            idtxt  : 'MPW';
            asmbin : '';
            asmcmd : '-model far -o $OBJ $ASM';
            supported_target : target_any;
            allowdirect : true;
            externals : false;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : '__L';
            comment : '* ';
            secnames : ('',
              '.text','.data','.bss',
              '.idata$2','.idata$4','.idata$5','.idata$6','.idata$7','.edata',
              '.stab','.stabstr')
          )
          ,(
            id     : as_m68k_palm;
            idtxt  : 'ASPALM';
            asmbin : 'aspalm';
            asmcmd : '-mno-68881 -m68000 -l -o $OBJ $ASM';
            supported_target : target_m68k_PalmOS;
            allowdirect : true;
            externals : false;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : '.L';
            comment : '# ';
            secnames : ('',
              '.text','.data','.bss',
              '.idata$2','.idata$4','.idata$5','.idata$6','.idata$7','.edata',
              '.stab','.stabstr')
          )
{$endif m68k}
{$ifdef alpha}
          ,(
            id     : as_alpha_as;
            idtxt  : 'AS';
            asmbin : 'as';
            asmcmd : '-o $OBJ $ASM';
            supported_target : target_any;
            allowdirect : true;
            externals : false;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : '.L';
            comment : '# ';
            secnames : ('',
              '.text','.data','.bss',
              '','','','','','',
              '.stab','.stabstr')
          )
{$endif}
{$ifdef powerpc}
          ,(
            id     : as_powerpc_as;
            idtxt  : 'AS';
            asmbin : 'as';
            asmcmd : '-o $OBJ $ASM';
            supported_target : target_any;
            allowdirect : true;
            externals : false;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : '.L';
            comment : '# ';
            secnames : ('',
              '.text','.data','.bss',
              '','','','','','',
              '.stab','.stabstr')
          )
          ,(
            id     : as_powerpc_mpw;
            idtxt  : 'PPCAsm';
            asmbin : 'PPCAsm';
            asmcmd : '-o $OBJ $ASM';
            supported_target : target_any;
            allowdirect : true;
            externals : false;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : '.L';
            comment : '; ';
            secnames : ('',
              '.text','.data','.bss',
              '','','','','','',
              '.stab','.stabstr')
          )
{$endif}
          );


{****************************************************************************
                                 Ar Info
****************************************************************************}
       ar_infos : array[1..arcnt] of tarinfo = (
          (
            id    : ar_none
          )
{$ifdef i386}
          ,(
            id    : ar_i386_ar;
            arcmd : 'ar rs $LIB $FILES'
          ),
          (
            id    : ar_i386_arw;
            arcmd : 'arw rs $LIB $FILES'
          )
{$endif i386}
{$ifdef m68k}
          ,(
            id    : ar_m68k_ar;
            arcmd : 'ar rs $LIB $FILES'
          )
{$endif m68k}
{$ifdef alpha}
          ,(
            id    : ar_alpha_ar;
            arcmd : 'ar rs $LIB $FILES'
          )
{$endif}
{$ifdef powerpc}
          ,(
            id    : ar_powerpc_ar;
            arcmd : 'ar rs $LIB $FILES'
          )
{$endif}
          );


{****************************************************************************
                                 Res Info
****************************************************************************}
       res_infos : array[1..rescnt] of tresinfo = (
          (
            id     : res_none
          )
{$ifdef i386}
          ,(
            id     : res_i386_windres;
            resbin : 'windres';
            rescmd : '--include $INC -O coff -o $OBJ $RES'
          )
          ,(
            id     : res_i386_emx;
            resbin : 'emxbind';
            rescmd : '-b -r $RES $OBJ'
(* Not really used - see TLinkeros2.SetDefaultInfo in t_os2.pas. *)
          )
{$endif i386}
{$ifdef m68k}
          ,(
            id     : res_m68k_mpw;
            resbin : 'rez';
            rescmd : '-i $INC -o $OBJ $RES'
          )
          ,(
            id     : res_m68k_palmos;
            resbin : 'pilrc';
            rescmd : '-I $INC $RES'
          )
{$endif m68}
{$ifdef powerpc}
          ,(
            id     : res_powerpc_mpw;
            resbin : 'rez';
            rescmd : '-i $INC -o $OBJ $RES'
          )
{$endif powerpc}
          );


{****************************************************************************
                            Targets Info
****************************************************************************}
       target_infos : array[1..targetcnt] of ttargetinfo = (
          (
            target      : target_none;
            flags       : [];
            cpu  : no_cpu;
            short_name  : 'notarget'
          )
{$ifdef i386}
          ,(
            target      : target_i386_GO32V1;
            flags       : [];
            cpu         : i386;
            short_name  : 'GO32V1';
            unit_env    : 'GO32V1UNITS';
            system_unit : 'SYSTEM';
            smartext    : '.sl';
            unitext     : '.pp1';
            unitlibext  : '.ppl';
            asmext      : '.s1';
            objext      : '.o1';
            resext      : '.res';
            resobjext   : '.o1r';
            exeext      : ''; { The linker produces a.out }
            os          : os_i386_GO32V1;
            assem       : as_i386_as;
            assemsrc    : as_i386_as;
            ar          : ar_i386_ar;
            res         : res_none;
            heapsize    : 2048*1024;
            maxheapsize : 32768*1024;
            stacksize   : 16384
          ),
          (
            target      : target_i386_GO32V2;
            flags       : [];
            cpu         : i386;
            short_name  : 'GO32V2';
            unit_env    : 'GO32V2UNITS';
            system_unit : 'SYSTEM';
            smartext    : '.sl';
            unitext     : '.ppu';
            unitlibext  : '.ppl';
            asmext      : '.s';
            objext      : '.o';
            resext      : '.res';
            resobjext   : '.or';
            exeext      : '.exe';
            os          : os_i386_GO32V2;
            assem       : as_i386_coff;
            assemsrc    : as_i386_as;
            ar          : ar_i386_ar;
            res         : res_none;
            heapsize    : 2048*1024;
            maxheapsize : 32768*1024;
            stacksize   : 16384
          ),
          (
            target      : target_i386_LINUX;
            flags       : [];
            cpu         : i386;
            short_name  : 'LINUX';
            unit_env    : 'LINUXUNITS';
            system_unit : 'syslinux';
            smartext    : '.sl';
            unitext     : '.ppu';
            unitlibext  : '.ppl';
            asmext      : '.s';
            objext      : '.o';
            resext      : '.res';
            resobjext   : '.or';
            exeext      : '';
            os          : os_i386_Linux;
            assem       : as_i386_as;
            assemsrc    : as_i386_as;
            ar          : ar_i386_ar;
            res         : res_none;
            heapsize    : 256*1024;
            maxheapsize : 32768*1024;
            stacksize   : 8192
          ),
          (
            target      : target_i386_FreeBSD;
            flags       : [];
            cpu         : i386;
            short_name  : 'FREEBSD';
            unit_env    : 'BSDUNITS';
            system_unit : 'sysbsd';
            smartext    : '.sl';
            unitext     : '.ppu';
            unitlibext  : '.ppl';
            asmext      : '.s';
            objext      : '.o';
            resext      : '.res';
            resobjext   : '.or';
            exeext      : '';
            os          : os_i386_Freebsd;
            assem       : as_i386_as;
            assemsrc    : as_i386_as;
            ar          : ar_i386_ar;
            res         : res_none;
            heapsize    : 256*1024;
            maxheapsize : 32768*1024;
            stacksize   : 8192
          ),
          (
            target      : target_i386_OS2;
            flags       : [tf_need_export];
            cpu  : i386;
            short_name  : 'OS2';
            unit_env    : 'OS2UNITS';
            system_unit : 'SYSOS2';
            smartext    : '.sl';
            unitext     : '.ppo';
            unitlibext  : '.ppl';
            asmext      : '.so2';
            objext      : '.oo2';
            resext      : '.res';
            resobjext   : '.oor';
            exeext      : ''; { The linker produces a.out }
            os          : os_i386_OS2;
            assem       : as_i386_as_aout;
            assemsrc    : as_i386_as_aout;
            ar          : ar_i386_ar;
            res         : res_i386_emx;
            heapsize    : 256*1024;
            maxheapsize : 256*1024*1024;
            stacksize   : 256*1024
          ),
          (
            target      : target_i386_WIN32;
            flags       : [];
            cpu         : i386;
            short_name  : 'WIN32';
            unit_env    : 'WIN32UNITS';
            system_unit : 'SYSWIN32';
            smartext    : '.slw';
            unitext     : '.ppw';
            unitlibext  : '.ppl';
            asmext      : '.sw';
            objext      : '.ow';
            resext      : '.rc';
            resobjext   : '.owr';
            exeext      : '.exe';
            os          : os_i386_Win32;
            assem       : as_i386_pecoff;
            assemsrc    : as_i386_asw;
            ar          : ar_i386_arw;
            res         : res_i386_windres;
            heapsize    : 256*1024;
            maxheapsize : 32*1024*1024;
            stacksize   : 32*1024*1024
          ),
          (
            target      : target_i386_sunos;
            flags       : [];
            cpu         : i386;
            short_name  : 'SUNOS';
            unit_env    : 'SUNOSUNITS';
            system_unit : 'system';
            smartext    : '.sl';
            unitext     : '.ppu';
            unitlibext  : '.ppl';
            asmext      : '.s';
            objext      : '.o';
            resext      : '.res';
            resobjext   : '.or';
            exeext      : '';
            os          : os_i386_sunos;
            assem       : as_i386_as;
            assemsrc    : as_i386_as;
            ar          : ar_i386_ar;
            res         : res_none;
            heapsize    : 256*1024;
            maxheapsize : 32768*1024;
            stacksize   : 262144
          ),
          (
            target      : target_i386_BEOS;
            flags       : [];
            cpu         : i386;
            short_name  : 'BEOS';
            unit_env    : 'BEOSUNITS';
            system_unit : 'system';
            smartext    : '.sl';
            unitext     : '.ppu';
            unitlibext  : '.ppl';
            asmext      : '.s';
            objext      : '.o';
            resext      : '.res';
            resobjext   : '.or';
            exeext      : '';
            os          : os_i386_BeOS;
            assem       : as_i386_as;
            assemsrc    : as_i386_as;
            ar          : ar_i386_ar;
            res         : res_none;
            heapsize    : 256*1024;
            maxheapsize : 32768*1024;
            stacksize   : 262144
          ),
          (
            target      : target_i386_NetBSD;
            flags       : [];
            cpu         : i386;
            short_name  : 'NETBSD';
            unit_env    : 'BSDUNITS';
            system_unit : 'sysbsd';
            smartext    : '.sl';
            unitext     : '.ppu';
            unitlibext  : '.ppl';
            asmext      : '.s';
            objext      : '.o';
            resext      : '.res';
            resobjext   : '.or';
            exeext      : '';
            os          : os_i386_netbsd;
            assem       : as_i386_as;
            assemsrc    : as_i386_as;
            ar          : ar_i386_ar;
            res         : res_none;
            heapsize    : 256*1024;
            maxheapsize : 32768*1024;
            stacksize   : 8192
          ),
          (
            target      : target_i386_QNX;
            flags       : [];
            cpu         : i386;
            short_name  : 'QNX';
            unit_env    : 'QNXUNITS';
            system_unit : 'system';
            smartext    : '.sl';
            unitext     : '.ppu';
            unitlibext  : '.ppl';
            asmext      : '.s';
            objext      : '.o';
            resext      : '.res';
            resobjext   : '.or';
            exeext      : '';
            os          : os_i386_qnx;
            assem       : as_i386_as;
            assemsrc    : as_i386_as;
            ar          : ar_i386_ar;
            res         : res_none;
            heapsize    : 256*1024;
            maxheapsize : 32768*1024;
            stacksize   : 262144
          ),
          (
            target      : target_i386_OpenBSD;
            flags       : [];
            cpu         : i386;
            short_name  : 'OPENBSD';
            unit_env    : 'BSDUNITS';
            system_unit : 'sysbsd';
            smartext    : '.sl';
            unitext     : '.ppu';
            unitlibext  : '.ppl';
            asmext      : '.s';
            objext      : '.o';
            resext      : '.res';
            resobjext   : '.or';
            exeext      : '';
            os          : os_i386_openbsd;
            assem       : as_i386_as;
            assemsrc    : as_i386_as;
            ar          : ar_i386_ar;
            res         : res_none;
            heapsize    : 256*1024;
            maxheapsize : 32768*1024;
            stacksize   : 8192
          )
{$endif i386}
{$ifdef m68k}
          ,(
            target      : target_m68k_Amiga;
            flags       : [];
            cpu         : m68k;
            short_name  : 'AMIGA';
            unit_env    : '';
            system_unit : 'sysamiga';
            smartext    : '.sl';
            unitext     : '.ppu';
            unitlibext  : '.ppl';
            asmext      : '.asm';
            objext      : '.o';
            resext      : '.res';
            resobjext   : '.or';
            exeext      : '';
            os          : os_m68k_Amiga;
            assem       : as_m68k_as;
            assemsrc    : as_m68k_as;
            ar          : ar_m68k_ar;
            res         : res_none;
            heapsize    : 128*1024;
            maxheapsize : 32768*1024;
            stacksize   : 8192
          ),
          (
            target      : target_m68k_Atari;
            flags       : [];
            cpu         : m68k;
            short_name  : 'ATARI';
            unit_env    : '';
            system_unit : 'SYSATARI';
            smartext    : '.sl';
            unitext     : '.ppu';
            unitlibext  : '.ppl';
            asmext      : '.s';
            objext      : '.o';
            resext      : '.res';
            resobjext   : '.or';
            exeext      : '.ttp';
            os          : os_m68k_Atari;
            assem       : as_m68k_as;
            assemsrc    : as_m68k_as;
            ar          : ar_m68k_ar;
            res         : res_none;
            heapsize    : 128*1024;
            maxheapsize : 32768*1024;
            stacksize   : 16384
          ),
          (
            target      : target_m68k_Mac;
            flags       : [];
            cpu         : m68k;
            short_name  : 'MACOS';
            unit_env    : '';
            system_unit : 'sysmac';
            smartext    : '.sl';
            unitext     : '.ppt';
            unitlibext  : '.ppl';
            asmext      : '.a';
            objext      : '.o';
            resext      : '.res';
            resobjext   : '.or';
            exeext      : '';
            os          : os_m68k_Mac;
            assem       : as_m68k_mpw;
            assemsrc    : as_m68k_mpw;
            ar          : ar_m68k_ar;
            res         : res_none;
            heapsize    : 128*1024;
            maxheapsize : 32768*1024;
            stacksize   : 8192
          ),
          (
            target      : target_m68k_linux;
            flags       : [];
            cpu         : m68k;
            short_name  : 'LINUX';
            unit_env    : 'LINUXUNITS';
            system_unit : 'syslinux';
            smartext    : '.sl';
            unitext     : '.ppu';
            unitlibext  : '.ppl';
            asmext      : '.s';
            objext      : '.o';
            resext      : '.res';
            resobjext   : '.or';
            exeext      : '';
            os          : os_m68k_Linux;
            assem       : as_m68k_as;
            assemsrc    : as_m68k_as;
            ar          : ar_m68k_ar;
            res         : res_none;
            heapsize    : 128*1024;
            maxheapsize : 32768*1024;
            stacksize   : 8192
          ),
          (
            target      : target_m68k_PalmOS;
            flags       : [tf_code_small,tf_static_a5_based];
            cpu         : m68k;
            short_name  : 'PALMOS';
            unit_env    : 'PALMUNITS';
            system_unit : 'SYSPALM';
            smartext    : '.sl';
            unitext     : '.ppu';
            unitlibext  : '.ppl';
            asmext      : '.s';
            objext      : '.o';
            resext      : '.res';
            resobjext   : '.or';
            exeext      : '';
            os          : os_m68k_PalmOS;
            assem       : as_m68k_palm;
            assemsrc    : as_m68k_palm;
            ar          : ar_m68k_ar;
            res         : res_m68k_palmos;
            heapsize    : 128*1024;
            maxheapsize : 32768*1024;
            stacksize   : 8192
          ),
          (
            target      : target_m68k_netbsd;
            flags       : [];
            cpu         : m68k;
            short_name  : 'NETBSD';
            unit_env    : 'BSDUNITS';
            system_unit : 'sysbsd';
            smartext    : '.sl';
            unitext     : '.ppu';
            unitlibext  : '.ppl';
            asmext      : '.s';
            objext      : '.o';
            resext      : '.res';
            resobjext   : '.or';
            exeext      : '';
            os          : os_m68k_netbsd;
            assem       : as_m68k_as;
            assemsrc    : as_m68k_as;
            ar          : ar_m68k_ar;
            res         : res_none;
            heapsize    : 128*1024;
            maxheapsize : 32768*1024;
            stacksize   : 8192
          ),
          (
            target      : target_m68k_openbsd;
            flags       : [];
            cpu         : m68k;
            short_name  : 'OPENBSD';
            unit_env    : 'BSDUNITS';
            system_unit : 'sysbsd';
            smartext    : '.sl';
            unitext     : '.ppu';
            unitlibext  : '.ppl';
            asmext      : '.s';
            objext      : '.o';
            resext      : '.res';
            resobjext   : '.or';
            exeext      : '';
            os          : os_m68k_openbsd;
            assem       : as_m68k_asbsd;
            assemsrc    : as_m68k_asbsd;
            ar          : ar_m68k_ar;
            res         : res_none;
            heapsize    : 128*1024;
            maxheapsize : 32768*1024;
            stacksize   : 8192
          )
{$endif m68k}
{$ifdef alpha}
          ,(
            target      : target_alpha_LINUX;
            flags       : [];
            cpu         : alpha;
            short_name  : 'LINUX';
            unit_env    : 'LINUXUNITS';
            system_unit : 'syslinux';
            smartext    : '.sl';
            unitext     : '.ppu';
            unitlibext  : '.ppl';
            asmext      : '.s';
            objext      : '.o';
            resext      : '.res';
            resobjext   : '.or';
            exeext      : '';
            os          : os_alpha_Linux;
            assem       : as_alpha_as;
            assemsrc    : as_alpha_as;
            ar          : ar_alpha_ar;
            res         : res_none;
            heapsize    : 256*1024;
            maxheapsize : 32768*1024;
            stacksize   : 8192
          )
{$endif}
{$ifdef powerpc}
          ,(
            target      : target_powerpc_LINUX;
            flags       : [];
            cpu         : powerpc;
            short_name  : 'LINUX';
            unit_env    : '';
            system_unit : 'syslinux';
            smartext    : '.sl';
            unitext     : '.ppu';
            unitlibext  : '.ppl';
            asmext      : '.s';
            objext      : '.o';
            resext      : '.res';
            resobjext   : '.or';
            exeext      : '';
            os          : os_powerpc_Linux;
            assem       : as_powerpc_as;
            assemsrc    : as_powerpc_as;
            ar          : ar_powerpc_ar;
            res         : res_none;
            heapsize    : 256*1024;
            maxheapsize : 32768*1024;
            stacksize   : 8192
          ),
          (
            target      : target_powerpc_MACOS;
            flags       : [];
            cpu         : powerpc;
            short_name  : 'MACOS';
            unit_env    : '';
            system_unit : 'sysmac';
            smartext    : '.sl';
            unitext     : '.ppt';
            unitlibext  : '.ppl';
            asmext      : '.a';
            objext      : '.o';
            resext      : '.res';
            resobjext   : '.or';
            exeext      : '';
            os          : os_powerpc_macos;
            assem       : as_powerpc_mpw;
            assemsrc    : as_powerpc_mpw;
            ar          : ar_powerpc_ar;
            res         : res_powerpc_mpw;
            heapsize    : 256*1024;
            maxheapsize : 32768*1024;
            stacksize   : 8192
          )
{$endif}
          );

{****************************************************************************
                             AsmModeInfo
****************************************************************************}
       asmmodeinfos : array[1..asmmodecnt] of tasmmodeinfo = (
          (
            id    : asmmode_none;
            idtxt : 'none'
          )
{$ifdef i386}
          ,(
            id    : asmmode_i386_direct;
            idtxt : 'DIRECT'
          ),
          (
            id    : asmmode_i386_att;
            idtxt : 'ATT'
          ),
          (
            id    : asmmode_i386_intel;
            idtxt : 'INTEL'
          )
{$endif i386}
{$ifdef m68k}
          ,(
            id    : asmmode_m68k_mot;
            idtxt : 'MOT'
          )
{$endif m68k}
{$ifdef alpha}
          ,(
            id    : asmmode_alpha_direct;
            idtxt : 'DIRECT'
          )
{$endif}
{$ifdef powerpc}
          ,(
            id    : asmmode_powerpc_direct;
            idtxt : 'DIRECT'
          )
{$endif}
          );

{****************************************************************************
                                Helpers
****************************************************************************}

function upper(const s : string) : string;
var
  i  : longint;
begin
  for i:=1 to length(s) do
   if s[i] in ['a'..'z'] then
    upper[i]:=char(byte(s[i])-32)
   else
    upper[i]:=s[i];
{$ifndef TP}
  {$ifopt H+}
    SetLength(upper,length(s));
  {$else}
    upper[0]:=s[0];
  {$endif}
{$else}
  upper[0]:=s[0];
{$endif}
end;

function lower(const s : string) : string;
var
  i : longint;
begin
  for i:=1 to length(s) do
   if s[i] in ['A'..'Z'] then
    lower[i]:=char(byte(s[i])+32)
   else
    lower[i]:=s[i];
  {$ifndef TP}
    {$ifopt H+}
      setlength(lower,length(s));
    {$else}
      lower[0]:=s[0];
    {$endif}
  {$else}
    lower[0]:=s[0];
  {$endif}
end;


function set_target_os(t:tos):boolean;
var
  i : longint;
begin
  set_target_os:=false;
  { target 1 is none }
  for i:=2 to oscnt do
   if os_infos[i].id=t then
    begin
      target_os:=os_infos[i];
      set_target_os:=true;
      exit;
    end;
end;


function set_target_asm(t:tasm):boolean;
var
  i : longint;
begin
  set_target_asm:=false;
  for i:=1 to asmcnt do
   if as_infos[i].id=t then
    begin
      target_asm:=as_infos[i];
      set_target_asm:=true;
      exit;
    end;
end;


function set_target_ar(t:tar):boolean;
var
  i : longint;
begin
  set_target_ar:=false;
  for i:=1 to arcnt do
   if ar_infos[i].id=t then
    begin
      target_ar:=ar_infos[i];
      set_target_ar:=true;
      exit;
    end;
end;


function set_target_res(t:tres):boolean;
var
  i : longint;
begin
  set_target_res:=false;
  for i:=1 to rescnt do
   if res_infos[i].id=t then
    begin
      target_res:=res_infos[i];
      set_target_res:=true;
      exit;
    end;
end;


function set_target_info(t:ttarget):boolean;
var
  i : longint;
begin
  set_target_info:=false;
  for i:=1 to targetcnt do
   if target_infos[i].target=t then
    begin
      target_info:=target_infos[i];
      set_target_os(target_info.os);
      set_target_asm(target_info.assem);
      set_target_ar(target_info.ar);
      set_target_res(target_info.res);
      target_path:=lower(target_info.short_name);
      target_cpu:=target_info.cpu;
      set_target_info:=true;
      exit;
    end;
end;


{****************************************************************************
                             Load from string
****************************************************************************}

function set_string_target(s : string) : boolean;
var
  i : longint;
begin
  set_string_target:=false;
  { this should be case insensitive !! PM }
  s:=upper(s);
  for i:=1 to targetcnt do
   if target_infos[i].short_name=s then
    begin
      set_target_info(target_infos[i].target);
      set_string_target:=true;
      exit;
    end;
end;


function set_string_asm(s : string) : boolean;
var
  i : longint;
begin
  set_string_asm:=false;
  { this should be case insensitive !! PM }
  s:=upper(s);
  for i:=1 to asmcnt do
   if as_infos[i].idtxt=s then
    begin
      target_asm:=as_infos[i];
      set_string_asm:=true;
    end;
end;


function set_string_asmmode(s:string;var t:tasmmode):boolean;
var
  i : longint;
begin
  set_string_asmmode:=false;
  { this should be case insensitive !! PM }
  s:=upper(s);
  for i:=1 to asmmodecnt do
   if asmmodeinfos[i].idtxt=s then
    begin
      t:=asmmodeinfos[i].id;
      set_string_asmmode:=true;
    end;
end;


{****************************************************************************
                      Initialization of default target
****************************************************************************}

procedure default_os(t:ttarget);
begin
  set_target_info(t);
  if source_os.name='' then
    source_os:=target_os;
end;


procedure set_source_os(t:tos);
var
  i : longint;
begin
{ can't use message() here (PFV) }
  if source_os.name<>'' then
    Writeln('Warning: Source OS Redefined!');
  for i:=1 to oscnt do
   if os_infos[i].id=t then
    begin
      source_os:=os_infos[i];
      exit;
    end;
end;


procedure InitSystems;
begin
{ first get source OS }
  source_os.name:='';
{ please note then we use cpu86 and cpu68 here on purpose !! }
{$ifdef cpu86}
 {$ifndef FPC}
  {$ifdef tp}
    set_source_os(os_i386_GO32V2);
  {$endif}
 {$endif ndef FPC}
  {$ifdef GO32V1}
    set_source_os(os_i386_GO32V1);
  {$else}
    {$ifdef GO32V2}
      set_source_os(os_i386_GO32V2);
    {$else}
      {$ifdef OS2}
        set_source_os(os_i386_OS2);
        if (OS_Mode = osDOS) or (OS_Mode = osDPMI) then
          source_os.scriptext := '.bat';
{OS/2 via EMX can be run under DOS as well}
      {$else}
        {$ifdef unix}
           {$Ifdef BSD}
             {$ifdef FreeBSD}
              set_source_os(os_i386_FreeBSD);
             {$else}
              {$ifdef netbsd}
                set_source_os(os_i386_NetBSD);
              {$else}
    set_source_os(os_i386_OpenBSD);
              {$endif}
            {$endif}
           {$endif bsd}
          {$ifdef linux}
            {$ifndef BSD}   { not necessary anymore if compiler is post 2000-05-31 }
              set_source_os(os_i386_LINUX);
            {$endif}
          {$endif linux}
          {$ifdef sunos}
           set_source_os(os_i386_sunos);
          {$endif}
          {$ifdef beos}
           set_source_os(os_i386_beos);
          {$endif}
        {$else}
          {$ifdef WIN32}
            set_source_os(os_i386_WIN32);
          {$else}
          {$endif win32}
        {$endif unix}
      {$endif os2}
    {$endif go32v2}
  {$endif go32v1}
{$endif cpu86}
{$ifdef cpu68}
  {$ifdef AMIGA}
    set_source_os(os_m68k_Amiga);
  {$else}
    {$ifdef ATARI}
      set_source_os(os_m68k_Atari);
    {$else}
      {$ifdef MACOS}
        set_source_os(os_m68k_MAC);
      {$else}
        {$ifdef BSD}
          {$ifdef NetBSD}
            set_source_os(os_m68k_NetBSD);
          {$else}
      {$ifdef OpenBSD}
        set_source_os(os_m68k_OpenBSD);
       {$endif}
    {$endif else_NetBSD}
        {$else BSD}
          {$ifdef LINUX}
             set_source_os(os_m68k_linux);
          {$endif linux}
        {$endif BSD}
      {$endif macos}
    {$endif atari}
  {$endif amiga}
{$endif cpu68}

{ Now default target !! }
{$ifdef i386}
  {$ifdef GO32V1}
     default_os(target_i386_GO32V1);
  {$else}
    {$ifdef GO32V2}
      default_os(target_i386_GO32V2);
    {$else}
      {$ifdef OS2}
        default_os(target_i386_OS2);
      {$else}
        {$ifdef unix}
           {$ifdef BSD}
          {$ifdef FreeBSD}
               default_os(target_i386_FreeBSD);
              {$else}
            {$ifdef NetBSD}
           default_os(target_i386_NetBSD);
        {$else}
           default_os(target_i386_OpenBSD);
            {$endif}
          {$endif}
            {$endif bsd}
            {$ifdef linux}
              {$ifndef BSD}
                default_os(target_i386_LINUX);
              {$endif}
            {$endif}
            {$ifdef sunos}
                default_os(target_i386_sunos);
            {$endif}
            {$ifdef beos}
                default_os(target_i386_beos);
            {$endif}
            {$ifdef qnx}
                default_os(target_i386_qnx);
            {$endif}
        {$else unix}
           {$ifdef WIN32}
             default_os(target_i386_WIN32);
           {$else}
             default_os(target_i386_GO32V2);
           {$endif win32}
        {$endif unix}
      {$endif os2}
    {$endif go32v2}
  {$endif go32v1}
{$endif i386}
{$ifdef m68k}
  {$ifdef AMIGA}
    default_os(target_m68k_Amiga);
  {$else}
    {$ifdef ATARI}
      default_os(target_m68k_Atari);
    {$else}
      {$ifdef MACOS}
        default_os(target_m68k_Mac);
      {$else}
  {$ifdef BSD}
          {$ifdef NetBSD}
            default_os(target_m68k_NetBSD);
          {$else}
        default_os(target_m68k_OpenBSD);
    {$endif else NetBSD}
  {$else BSD}
          {$ifdef LINUX}
             default_os(target_m68k_linux);
          {$else}
             default_os(target_m68k_Amiga);
          {$endif linux}
        {$endif BSD}
      {$endif macos}
    {$endif atari}
  {$endif amiga}
{$endif m68k}
{$ifdef alpha}
  default_os(target_alpha_linux);
{$endif alpha}
{$ifdef powerpc}
  default_os(target_powerpc_linux);
{$endif powerpc}
end;


begin
  InitSystems;
end.
{
  $Log: systems.pas,v $
  Revision 1.1.2.63  2003/03/24 11:02:53  pierre
   * fix a string length overflow

  Revision 1.1.2.62  2003/03/06 23:24:37  pierre
   * Crecords seem to be aligned up to 8 bytes for m68k netbsd

  Revision 1.1.2.61  2003/02/20 22:25:42  marco
   * OpenBSD works with old linker but needs DOTARGETOPENBSDAOUT defined.

  Revision 1.1.2.60  2003/02/10 01:01:44  pierre
   * try to enhance openbsd ports

  Revision 1.1.2.59  2003/01/16 14:06:52  pierre
   * gcc for palmos does not add leading underscore

  Revision 1.1.2.58  2003/01/13 10:41:00  pierre
   * fix some palmos osinfo values

  Revision 1.1.2.57  2003/01/11 20:45:49  carl
    * some removing of illegal tabs

  Revision 1.1.2.56  2003/01/09 09:29:05  pierre
   * use pilrc for palmos resources

  Revision 1.1.2.55  2002/12/18 16:53:19  pierre
   * source assembler for palm corrected

  Revision 1.1.2.54  2002/10/29 15:55:26  pierre
   * cycle works again with -So -dTP options

  Revision 1.1.2.53  2002/10/20 16:29:13  carl
    * fix ppu names for amiga/atari targets

  Revision 1.1.2.52  2002/10/18 21:55:41  carl
    * alignment of rtti information (m68k only)
    * bytes are also aligned when target = 68000
    + atari target

  Revision 1.1.2.51  2002/10/08 20:11:47  carl
    * C record alignment for GCC on amiga is also 2 max. (tested with GCC 2.95.1)

  Revision 1.1.2.50  2002/10/08 13:51:24  pierre
   * gcc for m68k only aligns up to 2 for linux (tested with gcc 2.95.4 and 3.0.4)

  Revision 1.1.2.49  2002/10/04 14:39:44  pierre
   * set stack alignment to 4 for m68k linux

  Revision 1.1.2.48  2002/09/27 20:57:34  carl
    * webbug report 2053
    + correct order of tsystems numbering

  Revision 1.1.2.47  2002/09/26 18:49:24  hajny
    * maxheapsize for OS/2 increased to allow 1.1 cycling

  Revision 1.1.2.46  2002/09/20 13:48:57  pierre
   * switch m68k netbsd to ELF

  Revision 1.1.2.45  2002/09/19 21:49:01  pierre
   * fix problem with -A switch on m68k

  Revision 1.1.2.44  2002/09/15 16:34:00  carl
    * libprefix for amiga target (weird all tabs are messed up)

  Revision 1.1.2.43  2002/08/03 09:16:00  carl
    * fix compilation problems for m68k target
    * fix delphi-style comments (not allowed in fixes!)

  Revision 1.1.2.42  2002/07/30 12:53:39  marco
   * Initial OpenBSD hacks. Some newer platforms (netware and wdosx) don't seem to have target
      records

  Revision 1.1.2.41  2002/07/14 13:17:43  carl
    - revert to old version because it no longer compiles on Be/QNX/Sunos

  Revision 1.1.2.40  2002/05/31 11:18:19  marco
   * Rename fest for 1.0.x step one. Compiler and RTL

  Revision 1.1.2.39  2002/04/17 18:59:52  carl
  + added qnx target

  Revision 1.1.2.38  2002/04/07 10:26:04  carl
  + added qnx target

  Revision 1.1.2.37  2002/04/04 19:10:03  carl
  + added wdosx support (patch from Pavel)

  Revision 1.1.2.36  2002/01/23 22:09:19  marco
   * Fix from Pierre which fixed debugging on NetBSD

  Revision 1.1.2.35  2001/12/31 23:35:19  marco
   * Carls fix didn't work, commited my tested version instead.

  Revision 1.1.2.33  2001/12/15 05:42:00  carl
  + complete cleanup of OS selection
  + QNX target

  Revision 1.1.2.32  2001/12/09 03:35:49  carl
  * Updated SunOS stack size and system unit name

  Revision 1.1.2.31  2001/09/28 01:10:18  carl
  * small typofix in BeOS target

  Revision 1.1.2.30  2001/09/22 00:08:48  carl
  * target definitions common for both FIXES branch and main branch are the same

  Revision 1.1.2.29  2001/09/15 04:19:24  carl
  * fix for amiga assembler name.

  Revision 1.1.2.28  2001/09/14 15:31:07  pierre
   * m68k netbsd OS uses underscore as C prefix

  Revision 1.1.2.27  2001/09/14 12:19:21  pierre
   * fix typo error about setting beos source os

  Revision 1.1.2.26  2001/09/14 11:15:37  pierre
   * restore m68k netbsd alignment to 2 as there are errors for alignment = 4

  Revision 1.1.2.25  2001/09/14 07:46:18  pierre
   * don't unse function relative address for m68k netbsd

  Revision 1.1.2.24  2001/09/13 23:03:22  pierre
   + m68k mit assembler output updated and used for netbsd target

  Revision 1.1.2.23  2001/09/05 10:32:02  pierre
     * i386 netbsd uses normal as_i386_as, error was due to

     wrong configuration in cross compilation of as and ld

     as_i386_asbsd removed

  Revision 1.1.2.22  2001/09/05 08:49:36  marco
   * NetBSD target definition fixed.

  Revision 1.1.2.21  2001/09/03 16:19:10  pierre
   * implementation of -sh and -st option, for linking at host or target machine

  Revision 1.1.2.20  2001/08/29 11:51:28  pierre
   + as_i386_asbsd for Netbsd that needs 'L' local prefix

  Revision 1.1.2.19  2001/08/22 02:41:12  carl
  * default_os for BeOS is not LINUX!

  Revision 1.1.2.18  2001/08/17 16:16:47  florian
    + support for PalmOS added

  Revision 1.1.2.17  2001/08/09 16:28:56  pierre
   + as_m68k_asbsd added, same as as_m68k_as, but uses L as local prefix, needed for NetBSD
   * NetBSD source_os and default_target correctly set

  Revision 1.1.2.16  2001/08/07 15:51:14  pierre
   + i386 and m68k netbsd targets added

  Revision 1.1.2.15  2001/08/03 02:12:00  carl
  * modify stack size of BeOS target

  Revision 1.1.2.14  2001/07/13 03:19:40  carl
  + BeOS support

  Revision 1.1.2.13  2001/06/15 12:43:16  pierre
   * masm fix

  Revision 1.1.2.12  2001/06/03 15:18:40  peter
    * linux sharedlib fixes

  Revision 1.1.2.11  2001/05/21 03:38:10  carl
  * fix for tp source_os

  Revision 1.1.2.10  2001/02/25 04:20:12  carl
  * corrected problem with unkwon fields again, this is a compiler bug!!

  Revision 1.1.2.9  2001/02/24 03:14:49  carl
  Make it compile - the ordering of fields was not correct, would this actually compile???

  Revision 1.1.2.8  2001/02/20 16:47:48  pierre
   better tasm/masm support

  Revision 1.1.2.7  2001/02/08 22:06:44  florian
    * solaris to sunos changed

  Revision 1.1.2.6  2001/02/01 22:31:55  florian
    + Solaris support for the compiler

  Revision 1.1.2.5  2001/01/01 22:26:04  marco
   * Now has libp as prefix for static linking

  Revision 1.1.2.4  2000/09/24 21:13:37  hajny
    * OS/2 default stack size increased

  Revision 1.1.2.3  2000/09/24 19:55:30  hajny
    * OS/2 stack alignment corrected (4)

  Revision 1.1.2.2  2000/09/13 13:57:41  marco
   * FreeBSD compiler support

  Revision 1.1.2.1  2000/08/07 12:32:47  jonas
    * reordered severam object fields to make optimal use of memory (avoid
      alignment padding)

  Revision 1.1  2000/07/13 06:29:57  michael
  + Initial import

  Revision 1.106  2000/06/25 19:08:28  hajny
    + $R support for OS/2 (EMX) added

  Revision 1.105  2000/06/23 21:31:18  pierre
   + new target_os field: maxCstructalignment

  Revision 1.104  2000/05/23 21:26:52  pierre
    + added supported_target fiedl to tasminfo record
      to disregard wrong assembler settings

  Revision 1.103  2000/05/11 09:07:45  pierre
   * change tosinfo.shortname length, reported by Kovacs Attila Zoltan

  Revision 1.102  2000/04/22 14:25:03  jonas
    * aasm.pas: pai_align instead of pai_align_abstract if cpu <> i386
    + systems.pas: info for macos/ppc
    * new/cgobj.pas: compiles again without newst define
    * new/powerpc/cgcpu: generate different entry/exit code depending on
      whether target_os is MacOs or Linux

  Revision 1.101  2000/04/04 14:18:15  pierre
   * nasmwin32 is 9 chars long, idtxt changed accordingly

  Revision 1.100  2000/04/04 13:54:58  pierre
   + nasmwin32 for win32 object output with nasm assembler

  Revision 1.99  2000/02/09 13:23:06  peter
    * log truncated

  Revision 1.98  2000/01/07 01:14:42  peter
    * updated copyright to 2000

  Revision 1.97  1999/11/06 14:34:28  peter
    * truncated log to 20 revs

  Revision 1.96  1999/11/03 23:43:45  peter
    * fixed ar commands

  Revision 1.95  1999/10/21 14:29:37  peter
    * redesigned linker object
    + library support for linux (only procedures can be exported)

  Revision 1.94  1999/09/15 22:09:27  florian
    + rtti is now automatically generated for published classes, i.e.
      they are handled like an implicit property

  Revision 1.93  1999/09/15 20:24:56  daniel
  + Dw switch now does something.

  Revision 1.92  1999/09/07 15:02:41  pierre
   * powerpc default was alpha !!

  Revision 1.91  1999/08/16 15:35:29  pierre
    * fix for DLL relocation problems
    * external bss vars had wrong stabs for pecoff
    + -WB11000000 to specify default image base, allows to
      load several DLLs with debugging info included
      (relocatable DLL are stripped because the relocation
       of the .Stab section is misplaced by ldw)

  Revision 1.90  1999/08/04 13:03:11  jonas
    * all tokens now start with an underscore
    * PowerPC compiles!!

  Revision 1.89  1999/08/04 00:23:32  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.88  1999/08/03 22:03:23  peter
    * moved bitmask constants to sets
    * some other type/const renamings

  Revision 1.87  1999/08/03 17:09:43  florian
    * the alpha compiler can be compiled now

  Revision 1.86  1999/08/03 15:52:00  michael
  * changed shortname for linux alpha

  Revision 1.85  1999/08/03 13:50:19  michael
  + Changes for alpha

  Revision 1.84  1999/08/02 23:56:51  michael
  + Added alpha cpu and linux for alpha os

}
