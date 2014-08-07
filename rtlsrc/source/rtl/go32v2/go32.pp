{
    $Id: go32.pp,v 1.1.2.5 2002/09/07 12:30:38 carl Exp $
    This file is part of the Free Pascal run time library.
    and implements some stuff for protected mode programming
    Copyright (c) 1999-2000 by the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit go32;

{$S-,R-,I-,Q-} {no stack check, used by DPMIEXCP !! }

interface

    const
    { contants for the run modes returned by get_run_mode }
       rm_unknown = 0;
       rm_raw     = 1;     { raw (without HIMEM) }
       rm_xms     = 2;     { XMS (for example with HIMEM, without EMM386) }
       rm_vcpi    = 3;     { VCPI (for example HIMEM and EMM386) }
       rm_dpmi    = 4;     { DPMI (for example DOS box or 386Max) }

    { flags }
       carryflag     = $001;
       parityflag    = $004;
       auxcarryflag  = $010;
       zeroflag      = $040;
       signflag      = $080;
       trapflag      = $100;
       interruptflag = $200;
       directionflag = $400;
       overflowflag  = $800;

    type
       tmeminfo = record
          available_memory,
          available_pages,
          available_lockable_pages,
          linear_space,
          unlocked_pages,
          available_physical_pages,
          total_physical_pages,
          free_linear_space,
          max_pages_in_paging_file,
          reserved0,
          reserved1,
          reserved2 : longint;
       end;

       tseginfo = record
          offset  : pointer;
          segment : word;
       end;

       trealregs = record
         case integer of
          1: { 32-bit } (EDI, ESI, EBP, Res, EBX, EDX, ECX, EAX: longint;
                         Flags, ES, DS, FS, GS, IP, CS, SP, SS: word);
          2: { 16-bit } (DI, DI2, SI, SI2, BP, BP2, R1, R2: word;
                         BX, BX2, DX, DX2, CX, CX2, AX, AX2: word);
          3: { 8-bit }  (stuff: array[1..4] of longint;
                         BL, BH, BL2, BH2, DL, DH, DL2, DH2,
                         CL, CH, CL2, CH2, AL, AH, AL2, AH2: byte);
          4: { Compat } (RealEDI, RealESI, RealEBP, RealRES,
                         RealEBX, RealEDX, RealECX, RealEAX: longint;
                         RealFlags,
                         RealES, RealDS, RealFS, RealGS,
                         RealIP, RealCS, RealSP, RealSS: word);
       end;

      registers = trealregs;

    { this works only with real DPMI }
    function allocate_ldt_descriptors(count : word) : word;
    function free_ldt_descriptor(d : word) : boolean;
    function segment_to_descriptor(seg : word) : word;
    function get_next_selector_increment_value : word;
    function get_segment_base_address(d : word) : longint;
    function set_segment_base_address(d : word;s : longint) : boolean;
    function set_segment_limit(d : word;s : longint) : boolean;
    function set_descriptor_access_right(d : word;w : word) : longint;
    function create_code_segment_alias_descriptor(seg : word) : word;
    function get_linear_addr(phys_addr : longint;size : longint) : longint;
    function get_segment_limit(d : word) : longint;
    function get_descriptor_access_right(d : word) : longint;
    function get_page_size:longint;
    function map_device_in_memory_block(handle,offset,pagecount,device:longint):boolean;
    function realintr(intnr : word;var regs : trealregs) : boolean;

    { is needed for functions which need a real mode buffer }
    function global_dos_alloc(bytes : longint) : longint;
    function global_dos_free(selector : word) : boolean;

    var
       { selector for the DOS memory (only usable if in DPMI mode) }
       dosmemselector : word;
       { result of dpmi call }
       int31error : word;

    { this procedure copies data where the source and destination }
    { are specified by 48 bit pointers                            }
    { Note: the procedure checks only for overlapping if          }
    { source selector=destination selector                        }
    procedure seg_move(sseg : word;source : longint;dseg : word;dest : longint;count : longint);

    { fills a memory area specified by a 48 bit pointer with c }
    procedure seg_fillchar(seg : word;ofs : longint;count : longint;c : char);
    procedure seg_fillword(seg : word;ofs : longint;count : longint;w : word);

    {************************************}
    { this works with all PM interfaces: }
    {************************************}

    function get_meminfo(var meminfo : tmeminfo) : boolean;
    function get_pm_interrupt(vector : byte;var intaddr : tseginfo) : boolean;
    function set_pm_interrupt(vector : byte;const intaddr : tseginfo) : boolean;
    function get_rm_interrupt(vector : byte;var intaddr : tseginfo) : boolean;
    function set_rm_interrupt(vector : byte;const intaddr : tseginfo) : boolean;
    function get_exception_handler(e : byte;var intaddr : tseginfo) : boolean;
    function set_exception_handler(e : byte;const intaddr : tseginfo) : boolean;
    function get_pm_exception_handler(e : byte;var intaddr : tseginfo) : boolean;
    function set_pm_exception_handler(e : byte;const intaddr : tseginfo) : boolean;
    function free_rm_callback(var intaddr : tseginfo) : boolean;
    function get_rm_callback(pm_func : pointer;const reg : trealregs;var rmcb : tseginfo) : boolean;
    function get_cs : word;
    function get_ds : word;
    function get_ss : word;

    { locking functions }
    function allocate_memory_block(size:longint):longint;
    function free_memory_block(blockhandle : longint) : boolean;
    function request_linear_region(linearaddr, size : longint;
                                   var blockhandle : longint) : boolean;
    function lock_linear_region(linearaddr, size : longint) : boolean;
    function lock_data(var data;size : longint) : boolean;
    function lock_code(functionaddr : pointer;size : longint) : boolean;
    function unlock_linear_region(linearaddr, size : longint) : boolean;
    function unlock_data(var data;size : longint) : boolean;
    function unlock_code(functionaddr : pointer;size : longint) : boolean;

    { disables and enables interrupts }
    procedure disable;
    procedure enable;

    function inportb(port : word) : byte;
    function inportw(port : word) : word;
    function inportl(port : word) : longint;

    procedure outportb(port : word;data : byte);
    procedure outportw(port : word;data : word);
    procedure outportl(port : word;data : longint);
    function get_run_mode : word;

    function transfer_buffer : longint;
    function tb_segment : longint;
    function tb_offset : longint;
    function tb_size : longint;
    procedure copytodos(var addr; len : longint);
    procedure copyfromdos(var addr; len : longint);

    procedure dpmi_dosmemput(seg : word;ofs : word;var data;count : longint);
    procedure dpmi_dosmemget(seg : word;ofs : word;var data;count : longint);
    procedure dpmi_dosmemmove(sseg,sofs,dseg,dofs : word;count : longint);
    procedure dpmi_dosmemfillchar(seg,ofs : word;count : longint;c : char);
    procedure dpmi_dosmemfillword(seg,ofs : word;count : longint;w : word);



    const
       { this procedures are assigned to the procedure which are needed }
       { for the current mode to access DOS memory                      }
       { It's strongly recommended to use this procedures!              }
       dosmemput      : procedure(seg : word;ofs : word;var data;count : longint)=@dpmi_dosmemput;
       dosmemget      : procedure(seg : word;ofs : word;var data;count : longint)=@dpmi_dosmemget;
       dosmemmove     : procedure(sseg,sofs,dseg,dofs : word;count : longint)=@dpmi_dosmemmove;
       dosmemfillchar : procedure(seg,ofs : word;count : longint;c : char)=@dpmi_dosmemfillchar;
       dosmemfillword : procedure(seg,ofs : word;count : longint;w : word)=@dpmi_dosmemfillword;

  implementation

{$asmmode ATT}


    { the following procedures copy from and to DOS memory using DPMI }
    procedure dpmi_dosmemput(seg : word;ofs : word;var data;count : longint);

      begin
         seg_move(get_ds,longint(@data),dosmemselector,seg*16+ofs,count);
      end;

    procedure dpmi_dosmemget(seg : word;ofs : word;var data;count : longint);

      begin
         seg_move(dosmemselector,seg*16+ofs,get_ds,longint(@data),count);
      end;

    procedure dpmi_dosmemmove(sseg,sofs,dseg,dofs : word;count : longint);

      begin
         seg_move(dosmemselector,sseg*16+sofs,dosmemselector,dseg*16+dofs,count);
      end;

    procedure dpmi_dosmemfillchar(seg,ofs : word;count : longint;c : char);

      begin
         seg_fillchar(dosmemselector,seg*16+ofs,count,c);
      end;

    procedure dpmi_dosmemfillword(seg,ofs : word;count : longint;w : word);

      begin
         seg_fillword(dosmemselector,seg*16+ofs,count,w);
      end;


    procedure test_int31(flag : longint);
      begin
         asm
            pushl %ebx
            movw  $0,INT31ERROR
            movl  flag,%ebx
            testb $1,%bl
            jz    .Lti31_1
            movw  %ax,INT31ERROR
            xorl  %eax,%eax
            jmp   .Lti31_2
            .Lti31_1:
            movl  $1,%eax
            .Lti31_2:
            popl  %ebx
         end;
      end;

    function global_dos_alloc(bytes : longint) : longint;

      begin
         asm
            movl bytes,%ebx
            addl $0xf,%ebx              // round up
            shrl $0x4,%ebx              // convert to Paragraphs
            movl $0x100,%eax            // function 0x100
            int  $0x31
            jnc  .LDos_OK
            movw %ax,INT31ERROR
            xorl %eax,%eax
            jmp  .LDos_end
          .LDos_OK:
            shll $0x10,%eax             // return Segment in hi(Result)
            movw %dx,%ax                // return Selector in lo(Result)
          .LDos_end:
            movl %eax,__result
         end;
      end;

    function  global_dos_free(selector : word) : boolean;

      begin
         asm
            movw Selector,%dx
            movl $0x101,%eax
            int  $0x31
            setnc %al
            movb %al,__RESULT
         end;
      end;

    function realintr(intnr : word;var regs : trealregs) : boolean;

      begin
         regs.realsp:=0;
         regs.realss:=0;
         asm
            { save all used registers to avoid crash under NTVDM }
            { when spawning a 32-bit DPMI application            }
            pushw %fs
            movw  intnr,%bx
            xorl  %ecx,%ecx
            movl  regs,%edi
            { es is always equal ds }
            movl  $0x300,%eax
            int   $0x31
            popw  %fs
            setnc %al
            movb  %al,__RESULT
         end;
      end;

    procedure seg_fillchar(seg : word;ofs : longint;count : longint;c : char);

      begin
         asm
            movl ofs,%edi
            movl count,%ecx
            movb c,%dl
            { load es with selector }
            pushw %es
            movw seg,%ax
            movw %ax,%es
            { fill eax with duplicated c }
            { so we can use stosl        }
            movb %dl,%dh
            movw %dx,%ax
            shll $16,%eax
            movw %dx,%ax
            movl %ecx,%edx
            shrl $2,%ecx
            cld
            rep
            stosl
            movl %edx,%ecx
            andl $3,%ecx
            rep
            stosb
            popw %es
         end ['EAX','ECX','EDX','EDI'];
      end;

    procedure seg_fillword(seg : word;ofs : longint;count : longint;w : word);

      begin
         asm
            movl ofs,%edi
            movl count,%ecx
            movw w,%dx
            { load segment }
            pushw %es
            movw seg,%ax
            movw %ax,%es
            { fill eax }
            movw %dx,%ax
            shll $16,%eax
            movw %dx,%ax
            movl %ecx,%edx
            shrl $1,%ecx
            cld
            rep
            stosl
            movl %edx,%ecx
            andl $1,%ecx
            rep
            stosw
            popw %es
         end ['EAX','ECX','EDX','EDI'];
      end;

    procedure seg_move(sseg : word;source : longint;dseg : word;dest : longint;count : longint);

      begin
         if count=0 then
           exit;
         if (sseg<>dseg) or ((sseg=dseg) and (source>dest)) then
           asm
              pushw %es
              pushw %ds
              cld
              movl count,%ecx
              movl source,%esi
              movl dest,%edi
              movw dseg,%ax
              movw %ax,%es
              movw sseg,%ax
              movw %ax,%ds
              movl %ecx,%eax
              shrl $2,%ecx
              rep
              movsl
              movl %eax,%ecx
              andl $3,%ecx
              rep
              movsb
              popw %ds
              popw %es
           end ['ESI','EDI','ECX','EAX']
         else if (source<dest) then
           { copy backward for overlapping }
           asm
              pushw %es
              pushw %ds
              std
              movl count,%ecx
              movl source,%esi
              movl dest,%edi
              movw dseg,%ax
              movw %ax,%es
              movw sseg,%ax
              movw %ax,%ds
              addl %ecx,%esi
              addl %ecx,%edi
              movl %ecx,%eax
              andl $3,%ecx
              orl %ecx,%ecx
              jz .LSEG_MOVE1

              { calculate esi and edi}
              decl %esi
              decl %edi
              rep
              movsb
              incl %esi
              incl %edi
           .LSEG_MOVE1:
              subl $4,%esi
              subl $4,%edi
              movl %eax,%ecx
              shrl $2,%ecx
              rep
              movsl
              cld
              popw %ds
              popw %es
           end ['ESI','EDI','ECX'];
      end;

    procedure outportb(port : word;data : byte);

      begin
         asm
            movw port,%dx
            movb data,%al
            outb %al,%dx
         end ['EAX','EDX'];
      end;

    procedure outportw(port : word;data : word);

      begin
         asm
            movw port,%dx
            movw data,%ax
            outw %ax,%dx
         end ['EAX','EDX'];
      end;

    procedure outportl(port : word;data : longint);

      begin
         asm
            movw port,%dx
            movl data,%eax
            outl %eax,%dx
         end ['EAX','EDX'];
      end;

    function inportb(port : word) : byte;

      begin
         asm
            movw port,%dx
            inb %dx,%al
            movb %al,__RESULT
         end ['EAX','EDX'];
      end;

    function inportw(port : word) : word;

      begin
         asm
            movw port,%dx
            inw %dx,%ax
            movw %ax,__RESULT
         end ['EAX','EDX'];
      end;

    function inportl(port : word) : longint;

      begin
         asm
            movw port,%dx
            inl %dx,%eax
            movl %eax,__RESULT
         end ['EAX','EDX'];
      end;



    function get_cs : word;assembler;
      asm
            movw %cs,%ax
      end;


    function get_ss : word;assembler;
      asm
            movw %ss,%ax
      end;


    function get_ds : word;assembler;
      asm
            movw %ds,%ax
      end;


    function set_pm_interrupt(vector : byte;const intaddr : tseginfo) : boolean;

      begin
         asm
            movl intaddr,%eax
            movl (%eax),%edx
            movw 4(%eax),%cx
            movl $0x205,%eax
            movb vector,%bl
            int $0x31
            pushf
            call test_int31
            movb %al,__RESULT
         end;
      end;

    function set_rm_interrupt(vector : byte;const intaddr : tseginfo) : boolean;

      begin
         asm
            movl intaddr,%eax
            movw (%eax),%dx
            movw 4(%eax),%cx
            movl $0x201,%eax
            movb vector,%bl
            int $0x31
            pushf
            call test_int31
            movb %al,__RESULT
         end;
      end;

    function set_pm_exception_handler(e : byte;const intaddr : tseginfo) : boolean;

      begin
         asm
            movl intaddr,%eax
            movl (%eax),%edx
            movw 4(%eax),%cx
            movl $0x212,%eax
            movb e,%bl
            int $0x31
            pushf
            call test_int31
            movb %al,__RESULT
         end;
      end;

    function set_exception_handler(e : byte;const intaddr : tseginfo) : boolean;

      begin
         asm
            movl intaddr,%eax
            movl (%eax),%edx
            movw 4(%eax),%cx
            movl $0x203,%eax
            movb e,%bl
            int $0x31
            pushf
            call test_int31
            movb %al,__RESULT
         end;
      end;

    function get_pm_exception_handler(e : byte;var intaddr : tseginfo) : boolean;

      begin
         asm
            movl $0x210,%eax
            movb e,%bl
            int $0x31
            pushf
            call test_int31
            movb %al,__RESULT
            movl intaddr,%eax
            movl %edx,(%eax)
            movw %cx,4(%eax)
         end;
      end;

    function get_exception_handler(e : byte;var intaddr : tseginfo) : boolean;

      begin
         asm
            movl $0x202,%eax
            movb e,%bl
            int $0x31
            pushf
            call test_int31
            movb %al,__RESULT
            movl intaddr,%eax
            movl %edx,(%eax)
            movw %cx,4(%eax)
         end;
      end;

    function get_pm_interrupt(vector : byte;var intaddr : tseginfo) : boolean;

      begin
         asm
            movb vector,%bl
            movl $0x204,%eax
            int $0x31
            pushf
            call test_int31
            movb %al,__RESULT
            movl intaddr,%eax
            movl %edx,(%eax)
            movw %cx,4(%eax)
         end;
      end;

    function get_rm_interrupt(vector : byte;var intaddr : tseginfo) : boolean;

      begin
         asm
            movb vector,%bl
            movl $0x200,%eax
            int $0x31
            pushf
            call test_int31
            movb %al,__RESULT
            movl intaddr,%eax
            movzwl %dx,%edx
            movl %edx,(%eax)
            movw %cx,4(%eax)
         end;
      end;

    function free_rm_callback(var intaddr : tseginfo) : boolean;
      begin
         asm
            movl intaddr,%eax
            movw (%eax),%dx
            movw 4(%eax),%cx
            movl $0x304,%eax
            int $0x31
            pushf
            call test_int31
            movb %al,__RESULT
         end;
      end;

    { here we must use ___v2prt0_ds_alias instead of from v2prt0.s
    because the exception processor sets the ds limit to $fff
    at hardware exceptions }

    var
       ___v2prt0_ds_alias : word; external name '___v2prt0_ds_alias';

    function get_rm_callback(pm_func : pointer;const reg : trealregs;var rmcb : tseginfo) : boolean;
      begin
         asm
            movl  pm_func,%esi
            movl  reg,%edi
            pushw %es
            movw  ___v2prt0_ds_alias,%ax
            movw  %ax,%es
            pushw %ds
            movw  %cs,%ax
            movw  %ax,%ds
            movl  $0x303,%eax
            int   $0x31
            popw  %ds
            popw  %es
            pushf
            call test_int31
            movb %al,__RESULT
            movl  rmcb,%eax
            movzwl %dx,%edx
            movl  %edx,(%eax)
            movw  %cx,4(%eax)
         end;
      end;

    function allocate_ldt_descriptors(count : word) : word;

      begin
         asm
            movw count,%cx
            xorl %eax,%eax
            int $0x31
            movw %ax,__RESULT
         end;
      end;

    function free_ldt_descriptor(d : word) : boolean;

      begin
         asm
            movw d,%bx
            movl $1,%eax
            int $0x31
            pushf
            call test_int31
            movb %al,__RESULT
         end;
      end;

    function segment_to_descriptor(seg : word) : word;

      begin
         asm
            movw seg,%bx
            movl $2,%eax
            int $0x31
            movw %ax,__RESULT
         end;
      end;

    function get_next_selector_increment_value : word;

      begin
         asm
            movl $3,%eax
            int $0x31
            movw %ax,__RESULT
         end;
      end;

    function get_segment_base_address(d : word) : longint;

      begin
         asm
            movw d,%bx
            movl $6,%eax
            int $0x31
            xorl %eax,%eax
            movw %dx,%ax
            shll $16,%ecx
            orl %ecx,%eax
            movl %eax,__RESULT
         end;
      end;

    function get_page_size:longint;
      begin
        asm
           movl $0x604,%eax
           int $0x31
           shll $16,%ebx
           movw %cx,%bx
           movl %ebx,__RESULT
        end;
      end;

    function request_linear_region(linearaddr, size : longint;
                                   var blockhandle : longint) : boolean;
      var
         pageofs : longint;

      begin
         pageofs:=linearaddr and $3ff;
         linearaddr:=linearaddr-pageofs;
         size:=size+pageofs;
         asm
            movl $0x504,%eax
            movl linearaddr,%ebx
            movl size,%ecx
            movl $1,%edx
            xorl %esi,%esi
            int $0x31
            pushf
            call test_int31
            movb %al,__RESULT
            movl blockhandle,%eax
            movl %esi,(%eax)
            movl %ebx,pageofs
         end;
         if pageofs<>linearaddr then
           request_linear_region:=false;
      end;

    function allocate_memory_block(size:longint):longint;
      begin
        asm
          movl  $0x501,%eax
          movl  size,%ecx
          movl  %ecx,%ebx
          shrl  $16,%ebx
          andl  $65535,%ecx
          int   $0x31
          jnc   .Lallocate_mem_block_err
          xorl  %ebx,%ebx
          xorl  %ecx,%ecx
       .Lallocate_mem_block_err:
          shll  $16,%ebx
          movw  %cx,%bx
          shll  $16,%esi
          movw  %di,%si
          movl  %ebx,__RESULT
        end;
     end;

    function free_memory_block(blockhandle : longint) : boolean;
      begin
         asm
            movl blockhandle,%esi
            movl %esi,%edi
            shll $16,%esi
            movl $0x502,%eax
            int  $0x31
            pushf
            call test_int31
            movb %al,__RESULT
         end;
      end;

    function lock_linear_region(linearaddr, size : longint) : boolean;

      begin
          asm
            movl  $0x600,%eax
            movl  linearaddr,%ecx
            movl  %ecx,%ebx
            shrl  $16,%ebx
            movl  size,%esi
            movl  %esi,%edi
            shrl  $16,%esi
            int   $0x31
            pushf
            call test_int31
            movb %al,__RESULT
          end;
      end;

    function lock_data(var data;size : longint) : boolean;

      var
         linearaddr : longint;

      begin
         if get_run_mode<>rm_dpmi then
           exit;
         linearaddr:=longint(@data)+get_segment_base_address(get_ds);
         lock_data:=lock_linear_region(linearaddr,size);
      end;

    function lock_code(functionaddr : pointer;size : longint) : boolean;

      var
         linearaddr : longint;

      begin
         if get_run_mode<>rm_dpmi then
           exit;
         linearaddr:=longint(functionaddr)+get_segment_base_address(get_cs);
         lock_code:=lock_linear_region(linearaddr,size);
      end;

    function unlock_linear_region(linearaddr,size : longint) : boolean;

      begin
         asm
            movl  $0x601,%eax
            movl  linearaddr,%ecx
            movl  %ecx,%ebx
            shrl  $16,%ebx
            movl  size,%esi
            movl  %esi,%edi
            shrl  $16,%esi
            int   $0x31
            pushf
            call  test_int31
            movb  %al,__RESULT
         end;
      end;

    function unlock_data(var data;size : longint) : boolean;

      var
         linearaddr : longint;
      begin
         if get_run_mode<>rm_dpmi then
           exit;
         linearaddr:=longint(@data)+get_segment_base_address(get_ds);
         unlock_data:=unlock_linear_region(linearaddr,size);
      end;

    function unlock_code(functionaddr : pointer;size : longint) : boolean;

      var
         linearaddr : longint;
      begin
         if get_run_mode<>rm_dpmi then
           exit;
         linearaddr:=longint(functionaddr)+get_segment_base_address(get_cs);
         unlock_code:=unlock_linear_region(linearaddr,size);
      end;

    function set_segment_base_address(d : word;s : longint) : boolean;

      begin
         asm
            movw d,%bx
            leal s,%eax
            movw (%eax),%dx
            movw 2(%eax),%cx
            movl $7,%eax
            int $0x31
            pushf
            call test_int31
            movb %al,__RESULT
         end;
      end;

    function set_descriptor_access_right(d : word;w : word) : longint;

      begin
         asm
            movw d,%bx
            movw w,%cx
            movl $9,%eax
            int $0x31
            pushf
            call test_int31
            movw %ax,__RESULT
         end;
      end;

    function set_segment_limit(d : word;s : longint) : boolean;

      begin
         asm
            movw d,%bx
            leal s,%eax
            movw (%eax),%dx
            movw 2(%eax),%cx
            movl $8,%eax
            int $0x31
            pushf
            call test_int31
            movb %al,__RESULT
         end;
      end;

    function get_descriptor_access_right(d : word) : longint;

      begin
         asm
            movzwl d,%eax
            lar %eax,%eax
            jz .L_ok
            xorl %eax,%eax
         .L_ok:
            movl %eax,__RESULT
         end;
      end;
    function get_segment_limit(d : word) : longint;

      begin
         asm
            movzwl d,%eax
            lsl %eax,%eax
            jz .L_ok2
            xorl %eax,%eax
         .L_ok2:
            movl %eax,__RESULT
         end;
      end;

    function create_code_segment_alias_descriptor(seg : word) : word;

      begin
         asm
            movw seg,%bx
            movl $0xa,%eax
            int $0x31
            pushf
            call test_int31
            movw %ax,__RESULT
         end;
      end;

    function get_meminfo(var meminfo : tmeminfo) : boolean;

      begin
         asm
            movl meminfo,%edi
            movl $0x500,%eax
            int $0x31
            pushf
            movb %al,__RESULT
            call test_int31
         end;
      end;

    function get_linear_addr(phys_addr : longint;size : longint) : longint;

      begin
         asm
            movl phys_addr,%ebx
            movl %ebx,%ecx
            shrl $16,%ebx
            movl size,%esi
            movl %esi,%edi
            shrl $16,%esi
            movl $0x800,%eax
            int $0x31
            pushf
            call test_int31
            shll $16,%ebx
            movw %cx,%bx
            movl %ebx,__RESULT
         end;
      end;

    procedure disable;assembler;

      asm
         cli
      end;

    procedure enable;assembler;

      asm
         sti
      end;


    var
      _run_mode : word;external name '_run_mode';

    function get_run_mode : word;

      begin
         get_run_mode:=_run_mode;
      end;

    function map_device_in_memory_block(handle,offset,pagecount,device:longint):boolean;
      begin
         asm
           movl device,%edx
           movl handle,%esi
           movl offset,%ebx
           movl pagecount,%ecx
           movl $0x0508,%eax
           int $0x31
           pushf
           setnc %al
           movb %al,__RESULT
           call test_int31
         end;
      end;

{*****************************************************************************
                              Transfer Buffer
*****************************************************************************}

    function transfer_buffer : longint;
      begin
         transfer_buffer := go32_info_block.linear_address_of_transfer_buffer;
      end;


    function tb_segment : longint;
      begin
        tb_segment:=go32_info_block.linear_address_of_transfer_buffer shr 4;
      end;


    function tb_offset : longint;
      begin
        tb_offset:=go32_info_block.linear_address_of_transfer_buffer and $f;
      end;


    function tb_size : longint;
      begin
         tb_size := go32_info_block.size_of_transfer_buffer;
      end;


    procedure copytodos(var addr; len : longint);
       begin
          if len>tb_size then
            runerror(217);
          seg_move(get_ds,longint(@addr),dosmemselector,transfer_buffer,len);
       end;


    procedure copyfromdos(var addr; len : longint);
       begin
          if len>tb_size then
            runerror(217);
          seg_move(dosmemselector,transfer_buffer,get_ds,longint(@addr),len);
       end;


    var
      _core_selector : word;external name '_core_selector';

begin
   int31error:=0;
   dosmemselector:=_core_selector;
end.

{
  $Log: go32.pp,v $
  Revision 1.1.2.5  2002/09/07 12:30:38  carl
    - removed support_ports option (cleanup)

  Revision 1.1.2.4  2001/06/06 14:27:17  jonas
    * fixed wrong typed constant procvars in preparation of my fix which will
      disallow them in FPC mode

  Revision 1.1.2.3  2001/05/07 18:11:42  carl
  - removed some useless push/pop from my previous fix.

  Revision 1.1.2.2  2001/05/06 22:41:59  carl
  * correct bug of realintr() under NT (NT corrupts some registers)

  Revision 1.1.2.1  2000/12/30 22:41:24  peter
    * fixed map_device_in_memory (from bug report)

  Revision 1.1  2000/07/13 06:30:37  michael
  + Initial import

  Revision 1.8  2000/02/09 16:59:28  peter
    * truncated log

  Revision 1.7  2000/01/07 16:41:31  daniel
    * copyright 2000

  Revision 1.6  2000/01/07 16:32:23  daniel
    * copyright 2000 added

  Revision 1.5  1999/09/09 07:13:29  pierre
    - Port[] moved to ports.pp unit
    * global_dos_alloc returns zero and set int31error
      if DPMI call fails

}
