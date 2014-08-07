{actualsyscall:
  _actualsyscall : int $0x80
                   jb someerror
                   ret
      someerror:   storeerrorsomewhere
                   ret
}

{
procedure actualsyscall; cdecl; EXTERNAL NAME '_actualsyscall';
}

procedure actualsyscall; assembler;
    asm
         int $0x80
         jb .LErrorcode
         xor %ebx,%ebx
         ret
.LErrorcode:
	 mov %eax,%ebx
         mov $-1,%eax
   end;


function Do_SysCall(sysnr:LONGINT):longint; assembler;

asm
  movl  sysnr,%eax
  call  actualsyscall
  movl  %ebx,Errno
end;

function Do_SysCall(sysnr,param1:longint):longint; assembler;

 asm
  movl  sysnr,%eax
  pushl Param1
  call  actualsyscall
  addl  $4,%esp
  movl  %ebx,Errno
 end;

function Do_SysCall(sysnr,param1:integer):longint; assembler;

 asm
  movzwl sysnr,%eax
  pushw Param1
  call  actualsyscall
  addl  $2,%esp
  movl  %ebx,Errno
 end;


function Do_SysCall(sysnr,param1,param2:LONGINT):longint; assembler;

 asm
   movl  sysnr,%eax
   pushl param2
   pushl Param1
   call  actualsyscall
   addl  $8,%esp
   movl  %ebx,Errno
 end;

function Do_SysCall(sysnr,param1,param2,param3:LONGINT):longint; assembler;

 asm
   movl  sysnr,%eax
   pushl param3
   pushl param2
   pushl Param1
   call  actualsyscall
   addl  $12,%esp
   movl  %ebx,Errno
 end;

function Do_SysCall(sysnr,param1,param2,param3,param4:LONGINT):longint; assembler;

asm
   movl  sysnr,%eax
   pushl param4
   pushl param3
   pushl param2
   pushl Param1
   call  actualsyscall
   addl  $16,%esp
   movl  %ebx,Errno
end;


function Do_SysCall(sysnr,param1,param2,param3,param4,param5:LONGINT):longint;  assembler;

 asm
   movl  sysnr,%eax
   pushl param5
   pushl param4
   pushl param3
   pushl param2
   pushl Param1
   call  actualsyscall
   addl  $20,%esp
   movl  %ebx,Errno
 end;

function Do_SysCall(sysnr,param1,param2,param3,param4,param5,param6:LONGINT):longint;  assembler;

asm
   movl  sysnr,%eax
   pushl param6
   pushl param5
   pushl param4
   pushl param3
   pushl param2
   pushl Param1
   call  actualsyscall
   addl  $24,%esp
   movl  %ebx,Errno
end;


function Do_SysCall(sysnr,param1,param2,param3,param4,param5,param6,param7:LONGINT):longint;  assembler;

asm
   movl  sysnr,%eax
   pushl param7
   pushl param6
   pushl param5
   pushl param4
   pushl param3
   pushl param2
   pushl Param1
   call  actualsyscall
   addl  $28,%esp
   movl  %ebx,Errno
end;

