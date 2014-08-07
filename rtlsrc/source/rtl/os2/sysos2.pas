{
 $Id: sysos2.pas,v 1.1.2.22 2003/02/20 17:11:13 hajny Exp $
 ****************************************************************************

                     Free Pascal -- OS/2 runtime library

         Copyright (c) 1999-2002 by Free Pascal development team

 Free Pascal is distributed under the GNU Public License v2. So is this unit.
 The GNU Public License requires you to distribute the source code of this
 unit with any product that uses it. We grant you an exception to this, and
 that is, when you compile a program with the Free Pascal Compiler, you do not
 need to ship source code with that program, AS LONG AS YOU ARE USING
 UNMODIFIED CODE! If you modify this code, you MUST change the next line:

 <This an official, unmodified Free Pascal source code file.>

 Send us your modified files, we can work together if you want!

 Free Pascal is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 Library GNU General Public License for more details.

 You should have received a copy of the Library GNU General Public License
 along with Free Pascal; see the file COPYING.LIB.  If not, write to
 the Free Software Foundation, 59 Temple Place - Suite 330,
 Boston, MA 02111-1307, USA.

****************************************************************************}

unit sysos2;

{Changelog:

    People:

        DM - Daniel Mantione

    Date:           Description of change:              Changed by:

     -              First released version 0.1.         DM

Coding style:

    My coding style is a bit unusual for Pascal. Nevertheless I friendly ask
    you to try to make your changes not look all to different. In general,
    set your IDE to use a tabsize of 4.}

interface

{Link the startup code.}
{$l prt1.oo2}

{$I SYSTEMH.INC}
{$I heaph.inc}

type    Tos=(osDOS,osOS2,osDPMI);

var     os_mode:Tos;
        first_meg:pointer;

type    Psysthreadib=^Tsysthreadib;
        Pthreadinfoblock=^Tthreadinfoblock;
        PPThreadInfoBlock=^PThreadInfoBlock;
        Pprocessinfoblock=^Tprocessinfoblock;
        PPProcessInfoBlock=^PProcessInfoBlock;

        Tbytearray=array[0..$ffff] of byte;
        Pbytearray=^Tbytearray;

        Tsysthreadib=record
            tid,
            priority,
            version:longint;
            MCcount,
            MCforceflag:word;
        end;

        Tthreadinfoblock=record
            pexchain,
            stack,
            stacklimit:pointer;
            tib2:Psysthreadib;
            version,
            ordinal:longint;
        end;

        Tprocessinfoblock=record
            pid,
            parentpid,
            hmte:longint;
            cmd,
            env:Pbytearray;
            flstatus,
            ttype:longint;
        end;

{Platform specific information}
const
 LineEnding = #13#10;
{ LFNSupport is defined separately below!!! }
 DirectorySeparator = '\';
 DriveSeparator = ':';
 PathSeparator = ';';
{ FileNameCaseSensitive is defined separately below!!! }


const   UnusedHandle=$ffff;
        StdInputHandle=0;
        StdOutputHandle=1;
        StdErrorHandle=2;

        FileNameCaseSensitive : boolean = false;
        LFNSupport: boolean = true;

var
{ C-compatible arguments and environment }
  argc  : longint;external name '_argc';
  argv  : ppchar;external name '_argv';
  envp  : ppchar;external name '_environ';


implementation

{$I SYSTEM.INC}

var
    heap_base: pointer; external name '__heap_base';
    heap_brk: pointer; external name '__heap_brk';
{$IFDEF CONTHEAP}
    BrkLimit: cardinal;
{$ENDIF CONTHEAP}

procedure DosGetInfoBlocks (PATIB: PPThreadInfoBlock;
                            PAPIB: PPProcessInfoBlock); cdecl;
                            external 'DOSCALLS' index 312;

function DosSetRelMaxFH (var ReqCount, CurMaxFH: longint): longint; cdecl;
external 'DOSCALLS' index 382;

function DosSetCurrentDir (Name:PChar): longint; cdecl;
external 'DOSCALLS' index 255;

function DosSetDefaultDisk (DiskNum:longint): longint; cdecl;
external 'DOSCALLS' index 220;

{ This is not real prototype, but its close enough }
{ for us (the 2nd parameter is acutally a pointer  }
{ to a structure).                                 }
function DosCreateDir( Name : pchar; p : pointer): longint; cdecl;
external 'DOSCALLS' index 270;

function DosDeleteDir( Name : pchar) : longint; cdecl;
external 'DOSCALLS' index 226;

{This is the correct way to call external assembler procedures.}
procedure syscall; external name '___SYSCALL';



   { converts an OS/2 error code to a TP compatible error }
   { code. Same thing exists under most other supported   }
   { systems.                                             }
   { Only call for OS/2 DLL imported routines             }
   Procedure Errno2InOutRes;
   Begin
     { errors 1..18 are the same as in DOS }
     case InOutRes of
      { simple offset to convert these error codes }
      { exactly like the error codes in Win32      }
      19..31 : InOutRes := InOutRes + 131;
      { gets a bit more complicated ... }
      32..33 : InOutRes := 5;
      38 : InOutRes := 100;
      39 : InOutRes := 101;
      112 : InOutRes := 101;
      110 : InOutRes := 5;
      114 : InOutRes := 6;
      290 : InOutRes := 290;
     end;
     { all other cases ... we keep the same error code }
   end;

{***************************************************************************

                Runtime error checking related routines.

***************************************************************************}

{$S-}
procedure st1(stack_size : longint); [public,alias : 'FPC_STACKCHECK'];
var
 c: cardinal;
begin
 c := cardinal(Sptr) - cardinal(stack_size) - STACK_MARGIN;
 if os_mode = osos2 then
   begin
     if (c <= cardinal(StackBottom)) then
        HandleError(202);
   end
 else
   begin
     if (c <= cardinal(heap_brk)) then
        HandleError(202);
   end;
end;

{****************************************************************************

                    Miscellaneous related routines.

****************************************************************************}

{$asmmode intel}
procedure system_exit; assembler;
asm
    mov  ah, 04ch
    mov  al, byte ptr exitcode
    call syscall
end ['EAX'];

{$ASMMODE ATT}

function paramcount:longint;assembler;

asm
    movl argc,%eax
    decl %eax
end ['EAX'];

    function args:pointer;assembler;

    asm
        movl argv,%eax
    end ['EAX'];


function paramstr(l:longint):string;

var p:^Pchar;

begin
    { There seems to be a problem with EMX for DOS when trying to }
    { access paramstr(0), and to avoid problems between DOS and   }
    { OS/2 they have been separated.                              }
    if os_Mode = OsOs2 then
    begin
      if L = 0 then
        begin
            GetMem (P, 260);
            p[0] := #0;  { in case of error, initialize to empty string }
{$ASMMODE INTEL}
            asm
                mov edx, P
                mov ecx, 260
                mov eax, 7F33h
                call syscall    { error handle already with empty string }
            end;
            ParamStr := StrPas (PChar (P));
            FreeMem (P, 260);
        end
      else
        if (l>0) and (l<=paramcount) then
            begin
                p:=args;
                paramstr:=strpas(p[l]);
            end
        else paramstr:='';
    end
   else
    begin
      p:=args;
      paramstr:=strpas(p[l]);
    end;
end;


procedure randomize; assembler;
asm
    mov ah, 2Ch
    call syscall
    mov word ptr [randseed], cx
    mov word ptr [randseed + 2], dx
end;

{$ASMMODE ATT}

{****************************************************************************

                    Heap management releated routines.

****************************************************************************}


{ this function allows to extend the heap by calling
syscall $7f00 resizes the brk area}

function sbrk(size:longint):longint;
{$IFDEF DUMPGROW}
var
  L: longint;
begin
  WriteLn ('Trying to grow heap by ', Size, ' to ', HeapSize + Size);
{$IFDEF CONTHEAP}
  WriteLn ('BrkLimit is ', BrkLimit);
{$ENDIF CONTHEAP}
  asm
    movl size,%edx
    movw $0x7f00,%ax
    call syscall     { result directly in EAX }
    mov  %eax,L
  end;
  WriteLn ('New heap at ', L);
  Sbrk := L;
end;
{$ELSE DUMPGROW}
                                     assembler;
asm
    movl size,%edx
    movw $0x7f00,%ax
    call syscall     { result directly in EAX }
end;
{$ENDIF DUMPGROW}

function getheapstart:pointer;assembler;

asm
    movl heap_base,%eax
end ['EAX'];

function getheapsize:longint;assembler;
asm
    movl heap_brk,%eax
end ['EAX'];

{$i heap.inc}

{****************************************************************************

                          Low Level File Routines

****************************************************************************}

procedure allowslash(p:Pchar);

{Allow slash as backslash.}

var i:longint;

begin
    for i:=0 to strlen(p) do
        if p[i]='/' then p[i]:='\';
end;

procedure do_close(h:longint);

begin
{ Only three standard handles under real OS/2 }
  if (h > 4) or
     ((os_MODE = osOS2) and (h > 2)) then
   begin
     asm
        movb $0x3e,%ah
        movl h,%ebx
        call syscall
        jnc  .Lnoerror           { error code?            }
        movw  %ax, InOutRes       { yes, then set InOutRes }
     .Lnoerror:
     end;
   end;
end;

procedure do_erase(p:Pchar);

begin
    allowslash(p);
    asm
        movl P,%edx
        movb $0x41,%ah
        call syscall
        jnc .LERASE1
        movw %ax,inoutres;
    .LERASE1:
    end;
end;

procedure do_rename(p1,p2:Pchar);

begin
    allowslash(p1);
    allowslash(p2);
    asm
        movl P1, %edx
        movl P2, %edi
        movb $0x56,%ah
        call syscall
        jnc .LRENAME1
        movw %ax,inoutres;
    .LRENAME1:
    end;
end;

function do_read(h,addr,len:longint):longint; assembler;
asm
    movl len,%ecx
    movl addr,%edx
    movl h,%ebx
    movb $0x3f,%ah
    call syscall
    jnc .LDOSREAD1
    movw %ax,inoutres;
    xorl %eax,%eax
.LDOSREAD1:
end;

function do_write(h,addr,len:longint) : longint; assembler;
asm
    xorl %eax,%eax
    cmpl $0,len    { 0 bytes to write is undefined behavior }
    jz   .LDOSWRITE1
    movl len,%ecx
    movl addr,%edx
    movl h,%ebx
    movb $0x40,%ah
    call syscall
    jnc .LDOSWRITE1
    movw %ax,inoutres;
.LDOSWRITE1:
end;

function do_filepos(handle:longint): longint; assembler;
asm
    movw $0x4201,%ax
    movl handle,%ebx
    xorl %edx,%edx
    call syscall
    jnc .LDOSFILEPOS
    movw %ax,inoutres;
    xorl %eax,%eax
.LDOSFILEPOS:
end;

procedure do_seek(handle,pos:longint); assembler;
asm
    movw $0x4200,%ax
    movl handle,%ebx
    movl pos,%edx
    call syscall
    jnc .LDOSSEEK1
    movw %ax,inoutres;
.LDOSSEEK1:
end;

function do_seekend(handle:longint):longint; assembler;
asm
    movw $0x4202,%ax
    movl handle,%ebx
    xorl %edx,%edx
    call syscall
    jnc .Lset_at_end1
    movw %ax,inoutres;
    xorl %eax,%eax
.Lset_at_end1:
end;

function do_filesize(handle:longint):longint;

var aktfilepos:longint;

begin
    aktfilepos:=do_filepos(handle);
    do_filesize:=do_seekend(handle);
    do_seek(handle,aktfilepos);
end;

procedure do_truncate(handle,pos:longint); assembler;
asm
(* DOS function 40h isn't safe for this according to EMX documentation *)
    movl $0x7F25,%eax
    movl Handle,%ebx
    movl Pos,%edx
    call syscall
    incl %eax
    movl %ecx, %eax
    jnz .LTruncate1      { compare the value of EAX to verify error }
(* File position is undefined after truncation, move to the end. *)
    movl $0x4202,%eax
    movl Handle,%ebx
    movl $0,%edx
    call syscall
    jnc .LTruncate2
.LTruncate1:
    movw %ax,inoutres;
.LTruncate2:
end;

const
    FileHandleCount: longint = 20;

function Increase_File_Handle_Count: boolean;
var Err: word;
    L1, L2: longint;
begin
    if os_mode = osOS2 then
        begin
            L1 := 10;
            if DosSetRelMaxFH (L1, L2) <> 0 then
                Increase_File_Handle_Count := false
            else
                if L2 > FileHandleCount then
                    begin
                        FileHandleCount := L2;
                        Increase_File_Handle_Count := true;
                    end
                else
                    Increase_File_Handle_Count := false;
        end
    else
        begin
            Inc (FileHandleCount, 10);
            Err := 0;
            asm
                movl $0x6700, %eax
                movl FileHandleCount, %ebx
                call syscall
                jnc .LIncFHandles
                movw %ax, Err
.LIncFHandles:
            end;
            if Err <> 0 then
                begin
                    Increase_File_Handle_Count := false;
                    Dec (FileHandleCount, 10);
                end
            else
                Increase_File_Handle_Count := true;
        end;
end;

procedure do_open(var f;p:pchar;flags:longint);

{
  filerec and textrec have both handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $100)   the file will be append
  when (flags and $1000)  the file will be truncate/rewritten
  when (flags and $10000) there is no check for close (needed for textfiles)
}

var Action: longint;

begin
    allowslash(p);
    { close first if opened }
    if ((flags and $10000)=0) then
        begin
            case filerec(f).mode of
                fminput,fmoutput,fminout : Do_Close(filerec(f).handle);
                fmclosed:;
            else
                begin
                    inoutres:=102; {not assigned}
                    exit;
                end;
            end;
       end;
    { reset file handle }
    filerec(f).handle := UnusedHandle;
    Action := 0;
    { convert filemode to filerec modes }
    case (flags and 3) of
        0 : filerec(f).mode:=fminput;
        1 : filerec(f).mode:=fmoutput;
        2 : filerec(f).mode:=fminout;
    end;
    if (flags and $1000)<>0 then
        Action := $50000; (* Create / replace *)
    { empty name is special }
    if p[0]=#0 then
        begin
          case FileRec(f).mode of
            fminput :
              FileRec(f).Handle:=StdInputHandle;
            fminout, { this is set by rewrite }
            fmoutput :
              FileRec(f).Handle:=StdOutputHandle;
            fmappend :
              begin
                FileRec(f).Handle:=StdOutputHandle;
                FileRec(f).mode:=fmoutput; {fool fmappend}
              end;
            end;
            exit;
        end;
    Action := Action or (Flags and $FF);
    (* DenyNone if sharing not specified. *)
    if Flags and 112 = 0 then
        Action := Action or 64;
    asm
        movl $0x7f2b, %eax
        movl Action, %ecx
        movl p, %edx
        call syscall
        cmpl $0xffffffff, %eax
        jnz .LOPEN1
        movw %cx, InOutRes
        movw UnusedHandle, %ax
.LOPEN1:
        movl f,%edx         { Warning : This assumes Handle is first }
        movw %ax,(%edx)     { field of FileRec                       }
    end;
    if (InOutRes = 4) and Increase_File_Handle_Count then
(* Trying again after increasing amount of file handles *)
        asm
            movl $0x7f2b, %eax
            movl Action, %ecx
            movl p, %edx
            call syscall
            cmpl $0xffffffff, %eax
            jnz .LOPEN2
            movw %cx, InOutRes
            movw UnusedHandle, %ax
.LOPEN2:
            movl f,%edx
            movw %ax,(%edx)
        end;
      { for systems that have more handles }
    if FileRec (F).Handle > FileHandleCount then
        FileHandleCount := FileRec (F).Handle;
    if (flags and $100)<>0 then
        begin
            do_seekend(filerec(f).handle);
            FileRec (F).Mode := fmOutput; {fool fmappend}
        end;
end;

{$ASMMODE INTEL}
function do_isdevice (Handle: longint): boolean; assembler;
asm
    mov ebx, Handle
    mov eax, 4400h
    call syscall
    jnc @noerror
    mov [InOutRes], ax
    mov eax, 0              { indicate a false value }
    jmp @IsDevEnd
@noerror:
    mov eax, 1
    test edx, 80h           { verify if it is a file  }
    jnz @IsDevEnd
    dec eax                 { nope, so result is zero }
@IsDevEnd:
end;
{$ASMMODE ATT}


{*****************************************************************************
                           UnTyped File Handling
*****************************************************************************}

{$i file.inc}

{*****************************************************************************
                           Typed File Handling
*****************************************************************************}

{$i typefile.inc}

{*****************************************************************************
                           Text File Handling
*****************************************************************************}

{$DEFINE EOF_CTRLZ}

{$i text.inc}

{****************************************************************************

                          Directory related routines.

****************************************************************************}

{*****************************************************************************
                           Directory Handling
*****************************************************************************}


procedure dosdir(func:byte;const s:string);

var buffer:array[0..255] of char;

begin
    move(s[1],buffer,length(s));
    buffer[length(s)]:=#0;
    allowslash(Pchar(@buffer));
    asm
        leal buffer,%edx
        movb func,%ah
        call syscall
        jnc  .LDOS_DIRS1
        movw %ax,inoutres
    .LDOS_DIRS1:
    end;
end;


procedure MkDir (const S: string);[IOCHECK];

var buffer:array[0..255] of char;
    Rc : word;

begin
  If (s='') or (InOutRes <> 0) then
   exit;
 if os_mode = osOs2 then
    begin
      move(s[1],buffer,length(s));
      buffer[length(s)]:=#0;
      allowslash(Pchar(@buffer));
      Rc := DosCreateDir(buffer,nil);
      if Rc <> 0 then
       begin
         InOutRes := Rc;
         Errno2Inoutres;
       end;
    end
  else
   begin
     { Under EMX 0.9d DOS this routine call may sometimes fail   }
     { The syscall documentation indicates clearly that this     }
     { routine was NOT tested.                                   }
     DosDir ($39, S);
   end;
end;


procedure rmdir(const s : string);[IOCHECK];
var buffer:array[0..255] of char;
    Rc : word;
begin
  if (s = '.' ) then
    InOutRes := 16;
  If (s='') or (InOutRes <> 0) then
   exit;
  if os_mode = osOs2 then
    begin
      move(s[1],buffer,length(s));
      buffer[length(s)]:=#0;
      allowslash(Pchar(@buffer));
      Rc := DosDeleteDir(buffer);
      if Rc <> 0 then
       begin
         InOutRes := Rc;
         Errno2Inoutres;
       end;
    end
  else
   begin
     { Under EMX 0.9d DOS this routine call may sometimes fail   }
     { The syscall documentation indicates clearly that this     }
     { routine was NOT tested.                                   }
     DosDir ($3A, S);
   end;
end;

{$ASMMODE INTEL}

procedure ChDir (const S: string);[IOCheck];

var RC: longint;
    Buffer: array [0..255] of char;

begin
  If (s='') or (InOutRes <> 0) then
   exit;
(* According to EMX documentation, EMX has only one current directory
   for all processes, so we'll use native calls under OS/2. *)
            if os_Mode = osOS2 then
                begin
                    if (Length (S) >= 2) and (S [2] = ':') then
                        begin
                            RC := DosSetDefaultDisk ((Ord (S [1]) and
                                    not ($20)) - $40);
                            if RC <> 0 then
                                InOutRes := RC
                            else
                                if Length (S) > 2 then
                                    begin
                                        Move (S [1], Buffer, Length (S));
                                        Buffer [Length (S)] := #0;
                                        AllowSlash (PChar (@Buffer));
                                        RC := DosSetCurrentDir (@Buffer);
                                        if RC <> 0 then
                                         begin
                                            InOutRes := RC;
                                            Errno2InOutRes;
                                         end;
                                    end;
                        end
                    else
                        begin
                            Move (S [1], Buffer, Length (S));
                            Buffer [Length (S)] := #0;
                            AllowSlash (PChar (@Buffer));
                            RC := DosSetCurrentDir (@Buffer);
                            if RC <> 0 then
                             begin
                                  InOutRes:= RC;
                                  Errno2InOutRes;
                             end;
                        end;
                end
            else
                if (Length (S) >= 2) and (S [2] = ':') then
                    begin
                        asm
                            mov esi, S
                            mov al, [esi + 1]
                            and al, not (20h)
                            sub al, 41h
                            mov edx, eax
                            mov ah, 0Eh
                            call syscall
                            mov ah, 19h
                            call syscall
                            cmp al, dl
                            jz @LCHDIR
                            mov InOutRes, 15
@LCHDIR:
                        end;
                        if (Length (S) > 2) and (InOutRes <> 0) then
                            { Under EMX 0.9d DOS this routine may sometime }
                            { fail or crash the system.                    }
                            DosDir ($3B, S);
                    end
                else
                    { Under EMX 0.9d DOS this routine may sometime }
                    { fail or crash the system.                    }
                    DosDir ($3B, S);
end;

{$ASMMODE ATT}

procedure GetDir (DriveNr: byte; var Dir: ShortString);

{Written by Michael Van Canneyt.}

var sof:Pchar;
    i:byte;

begin
    Dir [4] := #0;
    { Used in case the specified drive isn't available }
    sof:=pchar(@dir[4]);
    { dir[1..3] will contain '[drivenr]:\', but is not }
    { supplied by DOS, so we let dos string start at   }
    { dir[4]                                           }
    { Get dir from drivenr : 0=default, 1=A etc... }
    asm
        movb drivenr,%dl
        movl sof,%esi
        mov  $0x47,%ah
        call syscall
        jnc .LGetDir
        movw %ax, InOutRes
.LGetDir:
    end;
    { Now Dir should be filled with directory in ASCIIZ, }
    { starting from dir[4]                               }
    dir[0]:=#3;
    dir[2]:=':';
    dir[3]:='\';
    i:=4;
    {Conversion to Pascal string }
    while (dir[i]<>#0) do
        begin
            { convert path name to DOS }
            if dir[i]='/' then
            dir[i]:='\';
            dir[0]:=char(i);
            inc(i);
        end;
    { upcase the string (FPC function) }
    if drivenr<>0 then   { Drive was supplied. We know it }
        dir[1]:=chr(64+drivenr)
    else
        begin
            { We need to get the current drive from DOS function 19H  }
            { because the drive was the default, which can be unknown }
            asm
                movb $0x19,%ah
                call syscall
                addb $65,%al
                movb %al,i
            end;
            dir[1]:=char(i);
        end;
    if not (FileNameCaseSensitive) then dir:=upcase(dir);
end;


{****************************************************************************

                        System unit initialization.

****************************************************************************}

function GetFileHandleCount: longint;
var L1, L2: longint;
begin
    L1 := 0; (* Don't change the amount, just check. *)
    if DosSetRelMaxFH (L1, L2) <> 0 then
      GetFileHandleCount := 50
    else GetFileHandleCount := L2;
end;

var TIB:PThreadInfoBlock;
    PIB:PProcessInfoBlock;

begin
    {Determine the operating system we are running on.}
{$ASMMODE INTEL}
    asm
        mov os_mode, 0
        mov eax, 7F0Ah
        call syscall
        test bx, 512         {Bit 9 is OS/2 flag.}
        setne byte ptr os_mode
        test bx, 4096
{        jz @noRSX
 ... Heap initialization skipped under RSX. TH
}
        jz @heapok

        mov os_mode, 2
    @noRSX:

    {Enable the brk area by initializing it with the initial heap size.}

        mov eax, 7F01h
        mov edx, heap_brk
        add edx, heap_base
        call syscall
        cmp eax, -1
        jnz @heapok
        push dword 204
        call HandleError
    @heapok:
{$IFDEF CONTHEAP}
{ Find out brk limit }
        mov eax, 7F02h
        mov ecx, 3
        call syscall
        jcxz @heaplimitknown
        mov eax, 0
    @heaplimitknown:
        mov BrkLimit, eax
{$ELSE CONTHEAP}
{ Change sbrk behaviour to allocate arbitrary (non-contiguous) memory blocks }
        mov eax, 7F0Fh
        mov ecx, 0Ch
        mov edx, 8
        call syscall
{$ENDIF CONTHEAP}
    end;

    { in OS/2 this will always be nil, but in DOS mode }
    { this can be changed.                             }
    first_meg := nil;
    {Now request, if we are running under DOS,
     read-access to the first meg. of memory.}
    if os_mode in [osDOS,osDPMI] then
        asm
            mov ax, 7F13h
            xor ebx, ebx
            mov ecx, 0FFFh
            xor edx, edx
            call syscall
            jc  @endmem
            mov first_meg, eax
         @endmem:
        end
    else
        begin
          (* Initialize the amount of file handles *)
            FileHandleCount := GetFileHandleCount;
        end;
    {At 0.9.2, case for enumeration does not work.}
    case os_mode of
        osDOS:
            stackbottom:=0;     {In DOS mode, heap_brk is also the
                                 stack bottom.}
        osOS2:
            begin
                DosGetInfoBlocks(@TIB,@PIB);
                stackbottom:=cardinal(TIB^.Stack);
            end;
        osDPMI:
            stackbottom:=0;     {Not sure how to get it, but seems to be
                                 always zero.}
    end;
    exitproc:=nil;

    {Initialize the heap.}
    initheap;

    { ... and exceptions }
    InitExceptions;


    OpenStdIO(Input,fmInput,StdInputHandle);
    OpenStdIO(Output,fmOutput,StdOutputHandle);
    OpenStdIO(StdOut,fmOutput,StdOutputHandle);
    OpenStdIO(StdErr,fmOutput,StdErrorHandle);

    { no I/O-Error }
    inoutres:=0;
{$IFDEF DUMPGROW}
{$IFDEF CONTHEAP}
    WriteLn ('Initial brk size is ', GetHeapSize);
    WriteLn ('Brk limit is ', BrkLimit);
{$ENDIF CONTHEAP}
{$ENDIF DUMPGROW}
end.

{
  $Log: sysos2.pas,v $
  Revision 1.1.2.22  2003/02/20 17:11:13  hajny
    * fixes for OS/2 v2.1 incompatibility

  Revision 1.1.2.21  2003/01/15 22:16:26  hajny
    * default sharing mode changed to DenyNone

  Revision 1.1.2.20  2003/01/12 19:00:40  hajny
    * mistyping again

  Revision 1.1.2.19  2003/01/12 17:34:59  hajny
    * Skipping heap initialization under RSX

  Revision 1.1.2.18  2002/12/15 22:47:09  hajny
    * First_Meg fix merged

  Revision 1.1.2.17  2002/10/27 14:18:18  hajny
    * heap management (hopefully) fixed

  Revision 1.1.2.16  2002/03/10 11:43:53  carl
  * InOutRes := 16 with rmdir()

  Revision 1.1.2.15  2002/02/10 20:00:46  hajny
    * LFNSupport corrected

  Revision 1.1.2.14  2002/02/10 13:47:07  hajny
    * heap management corrected (heap_brk)

  Revision 1.1.2.13  2001/11/08 04:20:43  carl
  + LFNSupport constant

  Revision 1.1.2.12  2001/11/07 15:19:06  michael
  + Added OS describing constants

  Revision 1.1.2.11  2001/07/21 19:20:21  carl
  - removed unused code

  Revision 1.1.2.10  2001/05/21 20:52:46  hajny
    * system_exit back to pure assembler

  Revision 1.1.2.9  2001/05/12 03:20:19  carl
  + Correct IOResult error mapping like TP
  + Missing InOutRes checking added for I/O calls
  * IOCheck calls for directory services missing
  * paramstr(0) bugfix under DOS

  Revision 1.1.2.8  2001/04/20 19:28:39  hajny
    * setne operand size fixed

  Revision 1.1.2.7  2001/03/11 19:07:14  hajny
    * merging FExpand and Find* fixes

  Revision 1.1.2.6  2001/02/20 21:31:42  peter
    * chdir,mkdir,rmdir with empty string fixed

  Revision 1.1.2.5  2001/02/04 02:01:29  hajny
    * direct asm removing, DosGetInfoBlocks change merged

  Revision 1.1.2.4  2000/11/13 21:24:12  hajny
    * ParamStr (0) fixed

  Revision 1.1.2.3  2000/11/11 23:14:10  hajny
    * stackcheck alias corrected

  Revision 1.1.2.2  2000/10/15 20:45:16  hajny
    * ChDir correction, unit name changed

  Revision 1.1  2000/07/13 06:31:07  michael
  + Initial import

  Revision 1.34  2000/07/09 17:09:47  hajny
    * little mistyping

  Revision 1.33  2000/07/09 17:05:24  hajny
    * default sharing mode changed to deny all (compatibility)

  Revision 1.32  2000/06/11 09:47:57  hajny
    * error handling and sharing corrected

  Revision 1.31  2000/06/05 18:53:30  hajny
    * FileHandleCount handling for OS/2 added

  Revision 1.30  2000/06/04 14:14:01  hajny
    * do_truncate corrected, do_open might work under W9x now

  Revision 1.29  2000/05/28 18:17:39  hajny
    do_isdevice corrected

  Revision 1.28  2000/05/21 15:58:50  hajny
    + FileNameCaseSensitive added

  Revision 1.27  2000/04/07 17:47:34  hajny
    * got rid of os.inc

  Revision 1.26  2000/02/09 16:59:34  peter
    * truncated log

  Revision 1.25  2000/02/09 12:39:11  peter
    * halt moved to system.inc

  Revision 1.24  2000/01/20 23:38:02  peter
    * support fm_inout as stdoutput for assign(f,'');rewrite(f,1); becuase
      rewrite opens always with filemode 2

  Revision 1.23  2000/01/16 23:10:15  peter
    * handle check fixed

  Revision 1.22  2000/01/16 22:25:38  peter
    * check handle for file closing

  Revision 1.21  2000/01/09 20:45:58  hajny
    * FPK changed to FPC

  Revision 1.20  2000/01/07 16:41:50  daniel
    * copyright 2000

  Revision 1.19  2000/01/07 16:32:33  daniel
    * copyright 2000 added

  Revision 1.18  2000/01/02 17:45:25  hajny
    * cdecl added for doscalls routines

  Revision 1.17  1999/09/10 15:40:35  peter
    * fixed do_open flags to be > $100, becuase filemode can be upto 255

}
