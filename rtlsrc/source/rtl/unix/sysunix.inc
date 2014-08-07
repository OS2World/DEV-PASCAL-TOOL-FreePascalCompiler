{
    $Id: sysunix.inc,v 1.1.2.33 2003/04/24 09:32:18 pierre Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This is the core of the system unit *nix systems (now FreeBSD
     and Unix).

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{$ifdef FreeBSD}
 {$define UseSigInfo}
{$endif}

{$I system.inc}

{ used in syscall to report errors.}
var
  Errno : longint;

{ Include constant and type definitions }
{$i errno.inc    }  { Error numbers                 }
{$i sysnr.inc    }  { System call numbers           }
{$i sysconst.inc }  { Miscellaneous constants       }
{$i systypes.inc }  { Types needed for system calls }

{ Read actual system call definitions. }
{$i signal.inc}
{$i syscalls.inc }
{$ifdef FPC_USE_SIGPROCMASK}
{$i signalf.inc}
{$endif}

{*****************************************************************************
                       Misc. System Dependent Functions
*****************************************************************************}

procedure prthaltproc;external name '_haltproc';

Procedure System_exit;
Begin
  prthaltproc;
End;


Function ParamCount: Longint;
Begin
  Paramcount:=argc-1;
End;


Function ParamStr(l: Longint): String;
var
  link,
  hs : string;
  i : longint;
begin
  if l=0 then
   begin
     str(sys_getpid,hs);
     hs:='/proc/'+hs+'/exe'#0;
     i:=Sys_readlink(@hs[1],@link[1],high(link));
     { it must also be an absolute filename, linux 2.0 points to a memory
       location so this will skip that }
     if (i>0) and (link[1]='/') then
      begin
        link[0]:=chr(i);
        paramstr:=link;
      end
     else
      paramstr:=strpas(argv[0]);
   end
  else
   if (l>0) and (l<argc) then
    paramstr:=strpas(argv[l])
  else
    paramstr:='';
end;


Procedure Randomize;
Begin
  randseed:=sys_time;
End;


{*****************************************************************************
                              Heap Management
*****************************************************************************}

var
  _HEAP : pointer;external name 'HEAP';
  _HEAPSIZE : longint;external name 'HEAPSIZE';

function getheapstart:pointer;assembler;
{$undef fpc_getheapstart_ok}
{$ifdef i386}
{$define fpc_getheapstart_ok}
asm
        leal    _HEAP,%eax
end ['EAX'];
{$endif i386}
{$ifdef m68k}
{$define fpc_getheapstart_ok}
asm
        lea.l   _HEAP,a0
        move.l  a0,d0
end['A0','D0'];
{$endif m68k}
{$ifndef fpc_getheapstart_ok}
{$error Getheapstart code is not implemented }
{$endif not fpc_getheapstart_ok}


function getheapsize:longint;assembler;
{$undef fpc_getheapsize_ok}
{$ifdef i386}
{$define fpc_getheapsize_ok}
asm
        movl    _HEAPSIZE,%eax
end ['EAX'];
{$endif i386}
{$ifdef m68k}
{$define fpc_getheapsize_ok}
asm
       move.l   _HEAPSIZE,d0
end ['D0'];
{$endif m68k}
{$ifndef fpc_getheapsize_ok}
{$error Getheapsize code is not implemented }
{$endif not fpc_getheapsize_ok}



Function sbrk(size : longint) : Longint;
begin
 sbrk:=Sys_mmap(0,Size,3,MAP_PRIVATE+MAP_ANONYMOUS,-1,0);
 if sbrk<>-1 then
  errno:=0;
    {! It must be -1, not 0 as before, see heap.inc. Should be in sysmmap?}
end;

{ include standard heap management }
{$I heap.inc}


{*****************************************************************************
                          Low Level File Routines
*****************************************************************************}

{
  The lowlevel file functions should take care of setting the InOutRes to the
  correct value if an error has occured, else leave it untouched
}

Procedure Errno2Inoutres;
{
  Convert ErrNo error to the correct Inoutres value
}

begin
  if ErrNo=0 then { Else it will go through all the cases }
   exit;
  If errno<0 then Errno:=-errno;
  case ErrNo of
   Sys_ENFILE,
   Sys_EMFILE : Inoutres:=4;
   Sys_ENOENT : Inoutres:=2;
    Sys_EBADF : Inoutres:=6;
   Sys_ENOMEM,
   Sys_EFAULT : Inoutres:=217;
   Sys_EINVAL : Inoutres:=218;
    Sys_EPIPE,
    Sys_EINTR,
      Sys_EIO,
   Sys_EAGAIN,
   Sys_ENOSPC : Inoutres:=101;
 Sys_ENAMETOOLONG,
    Sys_ELOOP,
  Sys_ENOTDIR : Inoutres:=3;
    Sys_EROFS,
   Sys_EEXIST,
   Sys_EISDIR,
   Sys_ENOTEMPTY,
   Sys_EACCES : Inoutres:=5;
  Sys_ETXTBSY : Inoutres:=162;
  else
    InOutRes := Integer(Errno);
  end;
end;

Procedure Do_Close(Handle:Longint);
Begin
  sys_close(Handle);
End;

Procedure Do_Erase(p:pchar);
{$ifdef BSD}
 var FileInfo : Stat;
{$endif}

Begin
  {$ifdef BSD} {or POSIX}
  { verify if the filename is actually a directory }
  { if so return error and do nothing, as defined  }
  { by POSIX                                       }
  if sys_stat(p,fileinfo)<0 then
   begin
     Errno2Inoutres;
     exit;
   end;
  {$ifdef BSD}
   if (fileinfo.mode and STAT_IFMT)=STAT_IFDIR then
  {$else}
   if s_ISDIR(fileinfo.st_mode) then
  {$endif}
   begin
     InOutRes := 2;
     exit;
   end;
  {$endif}
  sys_unlink(p);
  Errno2Inoutres;
  {$ifdef Unix}
  { tp compatible result }
  if (Errno=Sys_EISDIR) then
   InOutRes:=2;
  {$endif}
End;


Procedure Do_Rename(p1,p2:pchar);
Begin
  sys_rename(p1,p2);
  Errno2Inoutres;
End;


Function Do_Write(Handle,Addr,Len:Longint):longint;
Begin
  repeat
    Do_Write:=sys_write(Handle,pchar(addr),len);
  until ErrNo<>Sys_EINTR;
  Errno2Inoutres;
  if Do_Write<0 then
   Do_Write:=0;
End;


Function Do_Read(Handle,Addr,Len:Longint):Longint;
Begin
  repeat
    Do_Read:=sys_read(Handle,pchar(addr),len);
  until ErrNo<>Sys_EINTR;
  Errno2Inoutres;
  if Do_Read<0 then
   Do_Read:=0;
End;

function Do_FilePos(Handle: Longint):longint;
Begin
  do_FilePos:=sys_lseek(Handle, 0, Seek_Cur);
  Errno2Inoutres;
End;

Procedure Do_Seek(Handle,Pos:Longint);
Begin
  sys_lseek(Handle, pos, Seek_set);
  errno2inoutres;
End;

Function Do_SeekEnd(Handle:Longint): Longint;
begin
  Do_SeekEnd:=sys_lseek(Handle,0,Seek_End);
  errno2inoutres;
end;

Function Do_FileSize(Handle:Longint): Longint;
var
  Info : Stat;
Begin
  if sys_fstat(handle,info)=0 then
   Do_FileSize:=Info.Size
  else
   Do_FileSize:=0;
  Errno2Inoutres;
End;

Procedure Do_Truncate(Handle,fPos:longint);

begin
  sys_ftruncate(handle,fpos);
  Errno2Inoutres;
end;


Procedure Do_Open(var f;p:pchar;flags:longint);
{
  FileRec and textrec have both Handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $100)   the file will be append
  when (flags and $1000)  the file will be truncate/rewritten
  when (flags and $10000) there is no check for close (needed for textfiles)
}
var
  oflags : longint;
Begin
{ close first if opened }
  if ((flags and $10000)=0) then
   begin
     case FileRec(f).mode of
      fminput,fmoutput,fminout : Do_Close(FileRec(f).Handle);
      fmclosed : ;
     else
      begin
        inoutres:=102; {not assigned}
        exit;
      end;
     end;
   end;
{ reset file Handle }
  FileRec(f).Handle:=UnusedHandle;
{ We do the conversion of filemodes here, concentrated on 1 place }
  case (flags and 3) of
   0 : begin
         oflags :=Open_RDONLY;
         FileRec(f).mode:=fminput;
       end;
   1 : begin
         oflags :=Open_WRONLY;
         FileRec(f).mode:=fmoutput;
       end;
   2 : begin
         oflags :=Open_RDWR;
         FileRec(f).mode:=fminout;
       end;
  end;
  if (flags and $1000)=$1000 then
   oflags:=oflags or (Open_CREAT or Open_TRUNC)
  else
   if (flags and $100)=$100 then
    oflags:=oflags or (Open_APPEND);
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
{ real open call }
  FileRec(f).Handle:=sys_open(p,oflags,438);
  if (ErrNo=Sys_EROFS) and ((OFlags and Open_RDWR)<>0) then
   begin
     Oflags:=Oflags and not(Open_RDWR);
     FileRec(f).Handle:=sys_open(p,oflags,438);
   end;
  Errno2Inoutres;
End;


Function Do_IsDevice(Handle:Longint):boolean;
{
  Interface to Unix ioctl call.
  Performs various operations on the filedescriptor Handle.
  Ndx describes the operation to perform.
  Data points to data needed for the Ndx function. The structure of this
  data is function-dependent.
}
var
  Data : array[0..255] of byte; {Large enough for termios info}
begin
  Do_IsDevice:=(sys_ioctl(handle,IOCTL_TCGETS,@data)<>-1);
  Errno:=0;
end;


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

{$DEFINE SHORT_LINEBREAK}
{$DEFINE EXTENDED_EOF}

{$i text.inc}

{*****************************************************************************
                           Directory Handling
*****************************************************************************}

Procedure MkDir(Const s: String);[IOCheck];
Var
  Buffer: Array[0..255] of Char;
Begin
  If (s='') or (InOutRes <> 0) then
   exit;
  Move(s[1], Buffer, Length(s));
  Buffer[Length(s)] := #0;
  sys_mkdir(@buffer, 511);
  Errno2Inoutres;
End;


Procedure RmDir(Const s: String);[IOCheck];
Var
  Buffer: Array[0..255] of Char;
Begin
  if (s ='.') then
    InOutRes := 16;
  If (s='') or (InOutRes <> 0) then
   exit;
  Move(s[1], Buffer, Length(s));
  Buffer[Length(s)] := #0;
  sys_rmdir(@buffer);
  {$ifdef BSD}
    if (Errno=Sys_EINVAL) Then
     InOutRes:=5
    Else
   {$endif}
  Errno2Inoutres;
End;


Procedure ChDir(Const s: String);[IOCheck];
Var
  Buffer: Array[0..255] of Char;
Begin
  If (s='') or (InOutRes <> 0) then
   exit;
  Move(s[1], Buffer, Length(s));
  Buffer[Length(s)] := #0;
  sys_chdir(@buffer);
  Errno2Inoutres;
  { file not exists is path not found under tp7 }
  if InOutRes=2 then
   InOutRes:=3;
End;


procedure getdir(drivenr : byte;var dir : shortstring);
var
  thisdir      : stat;
  rootino,
  thisino,
  dotdotino    : longint;
  rootdev,
  thisdev,
  dotdotdev    : dev_t;
  thedir,dummy : string[255];
  dirstream    : pdir;
  d            : pdirent;
  mountpoint,validdir : boolean;
  predot       : string[255];
begin
  drivenr:=0;
  dir:='';
  thedir:='/'#0;
  if sys_stat(@thedir[1],thisdir)<0 then
   exit;
  rootino:=thisdir.ino;
  rootdev:=thisdir.dev;
  thedir:='.'#0;
  if sys_stat(@thedir[1],thisdir)<0 then
   exit;
  thisino:=thisdir.ino;
  thisdev:=thisdir.dev;
  { Now we can uniquely identify the current and root dir }
  thedir:='';
  predot:='';
  while not ((thisino=rootino) and (thisdev=rootdev)) do
   begin
   { Are we on a mount point ? }
     dummy:=predot+'..'#0;
     if sys_stat(@dummy[1],thisdir)<0 then
      exit;
     dotdotino:=thisdir.ino;
     dotdotdev:=thisdir.dev;
     mountpoint:=(thisdev<>dotdotdev);
   { Now, Try to find the name of this dir in the previous one }
     dirstream:=opendir (@dummy[1]);
     if dirstream=nil then
      exit;
     repeat
       d:=sys_readdir (dirstream);
       validdir:=false;
       if (d<>nil) and
          (not ((d^.name[0]='.') and ((d^.name[1]=#0) or ((d^.name[1]='.')
                                 and (d^.name[2]=#0))))) and
                                 (mountpoint or (d^.ino=thisino)) then
        begin
          dummy:=predot+'../'+strpas(@(d^.name[0]))+#0;
          validdir:=not (sys_stat (@(dummy[1]),thisdir)<0);
        end
       else
        validdir:=false;
     until (d=nil) or
           ((validdir) and (thisdir.dev=thisdev) and (thisdir.ino=thisino) );
     { At this point, d.name contains the name of the current dir}
     if (d<>nil) then
      thedir:='/'+strpas(@(d^.name[0]))+thedir;
     { closedir also makes d invalid }
     if (closedir(dirstream)<0) or (d=nil) then
      exit;
     thisdev:=dotdotdev;
     thisino:=dotdotino;
     predot:=predot+'../';
   end;
{ Now rootino=thisino and rootdev=thisdev so we've reached / }
  dir:=thedir
end;


{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}


{$ifdef I386}
{ this should be defined in i386 directory !! PM }
const
  fpucw : word = $1332;
  FPU_Invalid = 1;
  FPU_Denormal = 2;
  FPU_DivisionByZero = 4;
  FPU_Overflow = 8;
  FPU_Underflow = $10;
  FPU_StackUnderflow = $20;
  FPU_StackOverflow = $40;
  FPU_All = $7f;

Procedure ResetFPU;
begin
{$ifndef CORRECTFLDCW}
  {$asmmode direct}
{$endif}
  asm
    fninit
    fldcw   fpucw
  end;
{$ifndef CORRECTFLDCW}
  {$asmmode att}
{$endif}
end;

function GetFPUState(const SigContext : SigContextRec) : longint;
begin
{$ifndef BSD}
  if assigned(SigContext.fpstate) then
    GetfpuState:=SigContext.fpstate^.sw;
{$else}
{$if NetBSD or OpenBSD}
  { net/openbsd has no floating point info in sigcontext :( PM }
  GetFPUState:=0;
{$endif}
{$ifdef FreeBSD}
  Getfpustate:=SigContext.en_sw;	{Freebsd context appears empty}
{$endif}
{$endif}
{$ifdef SYSTEM_DEBUG}
  writeln('xx:',sigcontext.en_tw,' ',sigcontext.en_cw);
{$endif SYSTEM_DEBUG}
{$ifdef SYSTEM_DEBUG}
  Writeln(stderr,'FpuState = ',GetFpuState);
{$endif SYSTEM_DEBUG}
end;

{$define FPC_HAS_FPU_EXCEPTION_BITS}

{$endif I386}

{$ifdef M68K}
const
{ Bits in control register }
  RoundingMode = $30;
  RoundingPrecision = $c0;
  InexactDecimal = $100;
  InexactOperation = $200;
  DivideByZero = $400;
  UnderFlow = $800;
  OverFlow = $1000;
  OperandError = $2000;
  SignalingNaN = $4000;
  BranchOnUnordered = $800;

  fpucw : longint = {InexactOperation or }DivideByZero or
    OverFlow or OperandError or
    SignalingNaN or BranchOnUnordered;
  fpust : longint = 0;
{ Bits in status register }
  FPU_Invalid = $80;
  FPU_Denormal = $8;
  FPU_DivisionByZero = $10;
  FPU_Overflow = $40;
  FPU_Underflow = $20;
  { m68k is not stack based }
  FPU_StackUnderflow = $0;
  FPU_StackOverflow = $0;
  FPU_All = $f8;

Procedure ResetFPU;
begin
  asm
    fmove.l fpucw,fpcr
    fmove.l fpust,fpsr
  end;
end;

function GetFPUState(const SigContext : SigContextRec) : longint;
begin
{$ifndef BSD}
  GetfpuState:=SigContext.sc_fpst;
{$else}
  { BSD has no infos yet in signal.inc }
  Getfpustate:=0{SigContext.en_sw};
{$endif}
{$ifdef SYSTEM_DEBUG}
  Writeln(stderr,'FpuState = ',GetFpuState);
{$endif SYSTEM_DEBUG}
end;

{$define FPC_HAS_FPU_EXCEPTION_BITS}
{$endif M68K}

{$ifndef FPC_HAS_FPU_EXCEPTION_BITS}
const
  FPU_Invalid = 0;
  FPU_Denormal = 0;
  FPU_DivisionByZero = 0;
  FPU_Overflow = 0;
  FPU_Underflow = 0;
  FPU_StackUnderflow = 0;
  FPU_StackOverflow = 0;

Procedure ResetFPU;
begin
end;

function GetFPUState(const SigContext : SigContextRec) : longint;
begin
  GetFpuState:=0;
end;
{$endif FPC_HAS_FPU_EXCEPTION_BITS}

{don't use ifndef Linux here. In 1.0.x the other platforms still define
Linux}

{$ifdef UseSigInfo}
{$ifdef FreeBSD}
procedure SignalToRunerror(Sig: longint; var info : tsiginfo_t;Var SigContext:SigContextRec); cdecl;
{$else}
 Not tested yet
{$endif}

var
  res : word;

begin
  res:=0;
  case sig of
    SIGFPE :
          begin
	    Case Info.si_code Of
		FPE_INTDIV : Res:=200;  {integer divide fault. Div0?}
		FPE_FLTOVF : Res:=205;  {Overflow trap}
		FPE_FLTUND : Res:=206;  {Stack over/underflow}
		FPE_FLTRES : Res:=216;  {Device not available}
		FPE_FLTINV : Res:=216;  {Invalid floating point operation}
	       Else
  	        Res:=208; {coprocessor error}
		End;
	     ResetFPU;
	  End;
    SIGILL,
    SIGBUS,
    SIGSEGV :
        res:=216;
  end;
  {$ifdef FPC_USE_SIGPROCMASK}
   reenable_signal(sig);
  {$endif }
{ give runtime error at the position where the signal was raised }
  if res<>0 then
   begin
{$ifdef I386}
   HandleErrorAddrFrame(res,SigContext.sc_eip,SigContext.sc_ebp);
{$else}
     HandleError(res);
{$endif}
   end;
end;

{$Else}

{$ifdef BSD}
{$if NetBSD or OpenBSD}
procedure SignalToRunerror(Sig: longint;code:longint; var SigContext:  SigContextRec); cdecl;
{$else} {FreeBSD tested, OpenBSD still unknown}
procedure SignalToRunerror(Sig: longint;var info:tsiginfo_t; var SigContext:  SigContextRec); cdecl;
{$endif}
{$else}
{$ifdef Solaris}
procedure SignalToRunerror(Sig: longint; SigContext: SigContextRec;someptr:pointer); cdecl;
{$else}
{$ifdef m68k}
procedure SignalToRunerror(Sig: longint; Info : pointer; var SigContext: SigContextRec); cdecl;
{$else not m68k}
procedure SignalToRunerror(Sig: longint; SigContext: SigContextRec); cdecl;
{$endif not m68k}
{$ENDIF}
{$endif}
var
  res,fpustate : word;
begin
  res:=0;
{$ifdef BSD}
{$ifdef DEBUG_FPU}
  Writeln('Sig=',sig,' Code=',code,' TrapNo=',SigContext.sc_trapno,
    ' Err=',SigContext.sc_err);
{$ifdef I386}
  fpustate:=0;
  asm
    fnstsw fpustate
  end;
  Writeln('Direct fpu state =',fpustate,' eip=',hexstr(SigContext.sc_eip,8));
{$endif I386}
{$endif DEBUG_FPU}
{$endif BSD}
  case sig of
    SIGFPE :
          begin
    { this is not allways necessary but I don't know yet
      how to tell if it is or not PM }
          res:=200;
          fpustate:=GetFPUState(SigContext);

{$ifdef FPC_HAS_FPU_EXCEPTION_BITS}
          if (FpuState and FPU_All) <> 0 then
            begin
              { first check the more precise options }
              if (FpuState and FPU_DivisionByZero)<>0 then
                res:=200
              else if (FpuState and FPU_Overflow)<>0 then
                res:=205
              else if (FpuState and FPU_Underflow)<>0 then
                res:=206
              else if (FpuState and FPU_Denormal)<>0 then
                res:=216
              else if (FpuState and (FPU_StackOverflow or FPU_StackUnderflow))<>0 then
                res:=207
              else if (FpuState and FPU_Invalid)<>0 then
                res:=216
              else
                res:=207;  {'Coprocessor Error'}
            end;
{$endif FPC_HAS_FPU_EXCEPTION_BITS}
          ResetFPU;
        end;
    SIGILL,
    SIGBUS,
    SIGSEGV :
        res:=216;
  end;
{$ifdef FPC_USE_SIGPROCMASK}
  reenable_signal(sig);
{$endif }
{ give runtime error at the position where the signal was raised }
  if res<>0 then
   begin
{$ifdef I386}
     {$ifdef BSD}
      HandleErrorAddrFrame(res,SigContext.sc_eip,SigContext.sc_ebp);
     {$else}
      HandleErrorAddrFrame(res,SigContext.eip,SigContext.ebp);
     {$endif}
{$else}
{$ifdef M68K}
     {$ifdef BSD}
      HandleErrorAddrFrame(res,SigContext.sc_pc,SigContext.sc_fp);
      {HandleError(res);}
     {$else}
      { HandleErrorAddrFrame(res,SigContext.sc_pc,SigContext.sc_fp);}
      { fp is not saved in context record :( }
      HandleError(res);
     {$endif}
     HandleError(res);
{$endif M68K}
{$endif I386}
   end;
end;
{$endif not UseSigInfo}

Procedure InstallSignals;
const
{$Ifndef BSD}
 {$ifdef solaris}
  act: SigActionRec =(sa_flags:SA_SIGINFO;Handler:(sa:@signaltorunerror;sa_mask:0);
 {$else}
  act: SigActionRec = (handler:(Sa:TSigAction(@SignalToRunError));sa_mask:0;sa_flags:0;
                       Sa_restorer: NIL);
 {$endif}
{$ELSE}
{$ifdef OpenBSD}
   act: SigActionRec = (handler:(Sa:TSigAction(@SignalToRunError));sa_flags:SA_SIGINFO;
    sa_mask:0);
{$endif}
{$ifdef FreeBSD}
   act: SigActionRec = (handler:(Sa:TSigAction(@SignalToRunError));sa_flags:SA_SIGINFO;
    sa_mask:(0,0,0,0));
{$endif}
{$ifdef NetBSD}
   act: SigActionRec = (handler:(Sa:TSigAction(@SignalToRunError));
    sa_mask : (0,0,0,0);
    sa_flags:SA_SIGINFO);
{$endif NetBSD}
{$endif BSD}

  oldact: PSigActionRec = Nil;          {Probably not necessary anymore, now
                                         VAR is removed}
begin
  ResetFPU;
  SigAction(SIGFPE,@act,oldact);
{$ifndef Solaris}
  SigAction(SIGBUS,@act,oldact);
  SigAction(SIGILL,@act,oldact);
{$ifdef FPC_USE_SIGALTSTACK}
  { we need to use alternate stack for SIGSEGV
    as this is needed for stack overflow handling PM }
  act.sa_flags:=act.sa_flags or SA_ONSTACK;
{$endif FPC_USE_SIGALTSTACK}
  SigAction(SIGSEGV,@act,oldact);
{$endif}
end;


procedure SetupCmdLine;
var
  bufsize,
  len,j,
  size,i : longint;
  found  : boolean;
  buf    : array[0..1026] of char;

  procedure AddBuf;
  begin
    reallocmem(cmdline,size+bufsize);
    move(buf,cmdline[size],bufsize);
    inc(size,bufsize);
    bufsize:=0;
  end;

begin
  size:=0;
  bufsize:=0;
  i:=0;
  while (i<argc) do
   begin
     len:=strlen(argv[i]);
     if len>sizeof(buf)-2 then
      len:=sizeof(buf)-2;
     found:=false;
     for j:=1 to len do
      if argv[i][j]=' ' then
       begin
         found:=true;
         break;
       end;
     if bufsize+len>=sizeof(buf)-2 then
      AddBuf;
     if found then
      begin
        buf[bufsize]:='"';
        inc(bufsize);
      end;
     move(argv[i]^,buf[bufsize],len);
     inc(bufsize,len);
     if found then
      begin
        buf[bufsize]:='"';
        inc(bufsize);
      end;
     if i<argc then
      buf[bufsize]:=' '
     else
      buf[bufsize]:=#0;
     inc(bufsize);
     inc(i);
   end;
  AddBuf;
end;

{$ifdef FPC_USE_SIGALTSTACK}
procedure InstallAlternateSignalStack;
var
  SS : SigAltStack;
begin
  GetMem(SS.ss_sp,SIGSTKSZ);
  SS.ss_flags:=0;
  SS.ss_size:=SIGSTKSZ;
  Sys_SigAltStack(@SS,nil);
end;
{$endif FPC_USE_SIGALTSTACK}

Begin
{ Set up signals handlers }
  InstallSignals;
{ Setup heap }
  InitHeap;
{$ifdef FPC_USE_SIGALTSTACK}
{ Setup signal handler specific stack }
  InstallAlternateSignalStack;
{$endif FPC_USE_SIGALTSTACK}
  InitExceptions;
{ Arguments }
  SetupCmdLine;
{ Setup stdin, stdout and stderr }
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
{ Reset IO Error }
  InOutRes:=0;
End.

{
  $Log: sysunix.inc,v $
  Revision 1.1.2.33  2003/04/24 09:32:18  pierre
   * reset ERrNo after call to sys_ioctl in Do_IsDevice

  Revision 1.1.2.32  2003/03/14 08:55:28  pierre
   + add SA_ONSTACK for SIGSEGV handling if SigAltStack is used

  Revision 1.1.2.31  2003/03/13 23:54:58  pierre
   * Use SigContext for netbsd m68k in SignalToRunError

  Revision 1.1.2.30  2003/03/13 22:37:16  pierre
   + FPC_USE_SIGALTSTACK code added

  Revision 1.1.2.29  2003/03/03 19:21:45  marco
   * New version with a siginfo fix

  Revision 1.1.2.28  2003/02/07 21:57:18  marco
   * small fixes

  Revision 1.1.2.27  2003/02/02 21:22:22  marco
   * Fix for tw1152, both FreeBSD and NetBSD

  Revision 1.1.2.26  2003/01/31 22:02:11  pierre
   * allow several exceptions to be honored

  Revision 1.1.2.25  2003/01/31 14:08:30  pierre
   * fix sigaction for netbsd

  Revision 1.1.2.24  2003/01/31 12:12:49  pierre
   * fix compilation failure for netbsd

  Revision 1.1.2.23  2002/11/17 21:07:56  marco
   * sigset 16-byte fix.

  Revision 1.1.2.22  2002/10/11 12:05:03  pierre
   * fix linux m68k fpu exceptions

  Revision 1.1.2.21  2002/10/10 19:44:06  pierre
   * better fpu exception support for m68k cpu

  Revision 1.1.2.20  2002/05/31 11:18:21  marco
   * Rename fest for 1.0.x step one. Compiler and RTL

  Revision 1.1.2.19  2002/03/10 11:46:05  carl
  * InOutRes := 16 with rmdir()
  * InOutRes := 5 more checking

  Revision 1.1.2.18  2002/03/09 19:38:14  marco
   * Fixed for FreeBSD

  Revision 1.1.2.17  2002/03/04 13:55:51  marco
   * Fix from Carl for sys_EISDIR for non Linux systems. FreeBSD works.
  	might need minor changes for other OSes.

  Revision 1.1.2.16  2002/01/29 20:58:59  marco
   * Added a more POSIX signal handler.

  Revision 1.1.2.15  2001/09/30 21:13:48  peter
    * erase(directory) returns now 2 to be tp compatible

  Revision 1.1.2.14  2001/08/04 15:23:45  carl
  * bugfix for Sys_EISDIR checking (bug 1565)

  Revision 1.1.2.13  2001/07/13 22:24:12  pierre
   * fix an error in previous commit

  Revision 1.1.2.12  2001/07/13 14:50:19  pierre
   clean up assembler parts

  Revision 1.1.2.11  2001/06/02 19:35:39  peter
    * chdir rte 2 to 3

  Revision 1.1.2.10  2001/06/02 00:25:30  peter
    * moved some unix files to target dependent dirs

  Revision 1.1.2.9  2001/04/12 17:59:05  peter
    * fixed usage of already release memory in getdir

  Revision 1.1.2.8  2001/03/13 10:31:50  marco
   * Small fixes + moving of linsyscall and bsdsyscall

  Revision 1.1.2.7  2001/03/12 20:37:50  marco
   * [Solaris] Now cycles for FreeBSD (wrong version Linux unit commited)

  Revision 1.1.2.6  2001/03/12 15:13:17  marco
   * [Solaris] Solaris patches. No dramatic changes, only small stuff.

  Revision 1.1.2.5  2001/02/20 21:31:42  peter
    * chdir,mkdir,rmdir with empty string fixed

  Revision 1.1.2.4  2001/01/31 10:50:05  michael
  + do_Seek should set inoutres

  Revision 1.1.2.3  2000/12/17 13:59:05  peter
    * removed debug writelns

  Revision 1.1.2.2  2000/12/08 21:57:34  michael
  - Removed something happened string

  Revision 1.1.2.1  2000/09/16 11:01:17  marco
   *  Split syslinux into sysunix.inc and sysunixh.inc

  Revision 1.1.2.1  2000/09/14 13:38:26  marco
   * Moved from Linux dir. now start of generic unix dir, from which the
      really exotic features should be moved to the target specific dirs.

  Revision 1.1.2.3  2000/09/10 16:12:40  marco
  The rearrangement to linux for

  Revision 1.1.2.2  2000/08/05 18:33:29  peter
    * paramstr(0) fix for linux 2.0 kernels

  Revision 1.1.2.1  2000/07/14 07:33:15  michael
  + Fixed do_open call. Directory checking must not be performed

  Revision 1.1  2000/07/13 06:30:54  michael
  + Initial import

  Revision 1.49  2000/07/08 18:02:39  peter
    * do_open checks for directory, if directory then ioerror 2

  Revision 1.48  2000/06/30 22:14:03  peter
    * removed obsolete crtlib code
    * support EINTR for read/write to restart the syscall

  Revision 1.47  2000/05/11 17:55:13  peter
    * changed order of fpustate checking to first check the more
      specific states

  Revision 1.46  2000/05/08 14:27:36  peter
    * released newsignal
    * newsignal gives now better backtraces using the sigcontext eip/ebp
      fields

  Revision 1.45  2000/04/16 16:07:58  marco
   * BSD fixes

  Revision 1.44  2000/04/14 13:04:53  marco
   * Merged bsd/syslinux.pp and 1.43 linux/syslinux.pp to this file with ifdefs

  Revision 1.43  2000/04/07 14:56:36  peter
    * switch to direct asm if not correctfldcw defined

  Revision 1.42  2000/03/31 23:26:32  pierre
   * FPU needs reset for all SIGFPE even from integer division by zero

  Revision 1.41  2000/03/31 23:21:19  pierre
    * multiple exception handling works
      (for linux only if syslinux is compiled with -dnewsignal)

  Revision 1.40  2000/03/31 13:24:28  jonas
    * signal handling using sigaction when compiled with -dnewsignal
      (allows multiple signals to be received in one run)

  Revision 1.39  2000/03/25 12:28:37  peter
    * patch for getdir from Pierre

  Revision 1.38  2000/03/23 15:24:18  peter
    * remove handle check for do_close

  Revision 1.37  2000/02/09 16:59:32  peter
    * truncated log

  Revision 1.36  2000/02/09 12:17:51  peter
    * moved halt to system.inc
    * syslinux doesn't use direct asm anymore

  Revision 1.35  2000/02/08 11:47:09  peter
    * paramstr(0) support

  Revision 1.34  2000/01/20 23:38:02  peter
    * support fm_inout as stdoutput for assign(f,'');rewrite(f,1); becuase
      rewrite opens always with filemode 2

  Revision 1.33  2000/01/16 22:25:38  peter
    * check handle for file closing

  Revision 1.32  2000/01/07 16:41:41  daniel
    * copyright 2000

  Revision 1.31  2000/01/07 16:32:28  daniel
    * copyright 2000 added

  Revision 1.30  1999/12/01 22:57:31  peter
    * cmdline support

  Revision 1.29  1999/11/06 14:39:12  peter
    * truncated log

  Revision 1.28  1999/10/28 09:50:06  peter
    * use mmap instead of brk

  Revision 1.27  1999/09/10 15:40:35  peter
    * fixed do_open flags to be > $100, becuase filemode can be upto 255

  Revision 1.26  1999/09/08 16:14:43  peter
    * pointer fixes

  Revision 1.25  1999/07/28 23:18:36  peter
    * closedir fixes, which now disposes the pdir itself

}
