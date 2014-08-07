	.file	"prt0.s"
gcc2_compiled.:
___gnu_compiled_c:
.data
	.align 5,0x90
	.type	 _rcsid , @object
	.size	_rcsid , 57
_rcsid:
	.ascii "$OpenBSD: crt0.c,v 1.6 2002/02/16 21:27:20 millert Exp $\0"
.globl ___progname
	.align 2,0x90
	.type	 ___progname , @object
	.size	___progname , 4
___progname:
	.long _empty
___fpucw:
        .long   0x1332
.text
	.align 2,0x90
.globl start
	.type	start , @function
start:
	pushl %ebp
	movl %esp,%ebp
	subl $12,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
#APP
	lea 4(%ebp), %esi
#NO_APP
	leal 4(%esi),%edi
	movl (%esi),%eax
	movl %eax,U_SYSBSD_ARGC
	sall $2,%eax
	leal 4(%eax,%edi),%eax
	movl %eax,_environ
	movl %eax,U_SYSBSD_ENVP
        movl %edi,U_SYSBSD_ARGV
	movl 4(%esi),%ebx
	testl %ebx,%ebx
	je L3
	addl $-8,%esp
	pushl $47
	pushl %ebx
	call __strrchr
	movl %eax,___progname
	addl $16,%esp
	testl %eax,%eax
	jne L4
	movl %ebx,___progname
	jmp L3
	.align 2,0x90
L4:
	incl %eax
	movl %eax,___progname
L3:
#	movl $__DYNAMIC,-4(%ebp)
#	movl -4(%ebp),%eax
#	movl -4(%ebp),%eax
#	testl %eax,%eax
#	je L6
#	addl $-12,%esp
#	pushl $__DYNAMIC
#	call ___load_rtld
#	addl $16,%esp
L6:
#APP
	eprol:
	__callmain:
#NO_APP
	subl $16,%esp
        finit                          
        fwait
        fldcw   ___fpucw

#	pushl _environ
#	pushl %edi
#	pushl (%esi)
	xorl %ebp,%ebp
	call PASCALMAIN
	pushl %eax
 	jmp _haltproc


.p2align 2,0x90
.globl _haltproc
.type _haltproc,@function

_haltproc:
           mov $1,%eax
           movzwl U_SYSBSD_EXITCODE,%ebx
           pushl %ebx
           call _actualsyscall
           addl  $4,%esp
           jmp   _haltproc

_actualsyscall:
         int $0x80
         jb .LErrorcode
         xor %ebx,%ebx
         ret
.LErrorcode:
         mov %eax,%ebx
         mov $-1,%eax
         ret
        .p2align 2,0x90

	.size	start , . - start
#APP
		___syscall:
			popl %ecx
			popl %eax
			pushl %ecx
			int $0x80
			pushl %ecx
			jc 1f
			ret
		1:
			movl $-1,%eax
			ret
#NO_APP
.lcomm _crt.6,36

	.align 2,0x90
	.type	__strrchr , @function
__strrchr:
	pushl %ebp
	movl %esp,%ebp
	pushl %ebx
	movl 8(%ebp),%eax
	movb 12(%ebp),%bl
	xorl %ecx,%ecx
	.align 2,0x90
L104:
	movb (%eax),%dl
	cmpb %bl,%dl
	jne L107
	movl %eax,%ecx
L107:
	testb %dl,%dl
	je L106
	incl %eax
	jmp L104
	.align 2,0x90
L106:
	movl %ecx,%eax
	popl %ebx
	leave
	ret
	.size	__strrchr , . - __strrchr


.comm _environ,4
.comm _errno,4
.lcomm _empty,4
.lcomm _ld_entry,4
