	.file	"cprt0.as"
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
	movl $__DYNAMIC,-4(%ebp)
	movl -4(%ebp),%eax
	movl -4(%ebp),%eax
	testl %eax,%eax
	je L6
	addl $-12,%esp
	pushl $__DYNAMIC
	call ___load_rtld
	addl $16,%esp
L6:
#APP
	eprol:
	__callmain:
#NO_APP
	subl $16,%esp
	pushl _environ
	pushl %edi
	pushl (%esi)
        finit                          
        fwait
        fldcw   ___fpucw

	xorl  %ebp,%ebp
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
LC0:
	.ascii "/usr/libexec/ld.so\0"
LC1:
	.ascii "No ld.so\12\0"
LC2:
	.ascii "Failure reading ld.so\12\0"
LC3:
	.ascii "Bad magic: ld.so\12\0"
LC4:
	.ascii "Cannot map ld.so\12\0"
	.align 5,0x90
LC5:
	.ascii "crt0: update /usr/libexec/ld.so\12\0"
LC6:
	.ascii "ld.so failed\12\0"
	.align 2,0x90
	.type	___load_rtld , @function
___load_rtld:
	pushl %ebp
	movl %esp,%ebp
	subl $44,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 8(%ebp),%edi
	movl $LC0,_crt.6+28
	pushl $0
	pushl $0
	pushl $LC0
	pushl $5
	call ___syscall
	movl %eax,_crt.6+8
	addl $16,%esp
	cmpl $-1,%eax
	jne L8
	movl 8(%edi),%eax
	cmpl $0,4(%eax)
	jne L9
	movl $0,_ld_entry
	jmp L7
	.align 2,0x90
L9:
	pushl $10
	pushl $LC1
	pushl $2
	pushl $4
	call ___syscall
	addl $-8,%esp
	pushl $1
	pushl $1
	call ___syscall
	addl $32,%esp
L8:
	pushl $32
	leal -32(%ebp),%eax
	pushl %eax
	pushl _crt.6+8
	pushl $3
	call ___syscall
	addl $16,%esp
	cmpl $31,%eax
	ja L10
	pushl $23
	pushl $LC2
	pushl $2
	pushl $4
	call ___syscall
	addl $-8,%esp
	pushl $1
	pushl $1
	call ___syscall
	addl $32,%esp
L10:
	movl -32(%ebp),%eax
	testl $-65536,%eax
	je L16
#APP
	rorw $8, %ax; rorl $16, %eax; rorw $8, %ax
#NO_APP
	cmpw $267,%ax
	jne L17
	jmp L15
	.align 2,0x90
L16:
	cmpl $267,%eax
	je L15
L17:
	movl -32(%ebp),%eax
	testl $-65536,%eax
	je L18
#APP
	rorw $8, %ax; rorl $16, %eax; rorw $8, %ax
#NO_APP
	cmpw $204,%ax
	jne L19
	jmp L15
	.align 2,0x90
L18:
	cmpl $204,%eax
	je L15
L19:
	pushl $18
	pushl $LC3
	pushl $2
	pushl $4
	call ___syscall
	addl $-8,%esp
	pushl $1
	pushl $1
	call ___syscall
	addl $32,%esp
L15:
	movl $-1,_crt.6+4
	addl $-4,%esp
	movl -32(%ebp),%eax
	testl $-65536,%eax
	je L28
#APP
	rorw $8, %ax; rorl $16, %eax; rorw $8, %ax
#NO_APP
	cmpw $267,%ax
	jne L29
	jmp L26
	.align 2,0x90
L28:
	orl $65536,%eax
	cmpl $267,%eax
	je L26
L29:
	movl -32(%ebp),%eax
	testl $-65536,%eax
	je L30
#APP
	rorw $8, %ax; rorl $16, %eax; rorw $8, %ax
#NO_APP
	andl $65535,%eax
	cmpl $65740,%eax
	jne L31
	jmp L26
	.align 2,0x90
L30:
	orl $65536,%eax
	cmpl $65740,%eax
	je L26
L31:
	movl -32(%ebp),%eax
	testl $-65536,%eax
	je L34
#APP
	rorw $8, %ax; rorl $16, %eax; rorw $8, %ax
#NO_APP
	andl $65535,%eax
	cmpl $65803,%eax
	je L35
	jmp L32
	.align 2,0x90
L34:
	orl $65536,%eax
	cmpl $65803,%eax
	jne L32
L35:
	movl $4096,%eax
	jmp L84
	.align 2,0x90
L32:
	movl $32,%eax
	jmp L84
	.align 2,0x90
L26:
	xorl %eax,%eax
L84:
	xorl %edx,%edx
	pushl %edx
	pushl %eax
	pushl $0
	pushl _crt.6+8
	pushl $2
	pushl $5
	movl -24(%ebp),%eax
	addl -28(%ebp),%eax
	addl -20(%ebp),%eax
	pushl %eax
	pushl $0
	pushl $0
	pushl $197
	pushl $198
	call ___syscall
	movl %eax,_crt.6
	addl $48,%esp
	cmpl $-1,%eax
	jne L36
	pushl $18
	pushl $LC4
	pushl $2
	pushl $4
	call ___syscall
	addl $-8,%esp
	pushl $1
	pushl $1
	call ___syscall
	addl $32,%esp
L36:
	addl $-4,%esp
	movl -32(%ebp),%eax
	movl %eax,%edx
	testl $-65536,%edx
	je L57
#APP
	rorw $8, %ax; rorl $16, %eax; rorw $8, %ax
#NO_APP
	cmpw $267,%ax
	je L56
	jmp L58
	.align 2,0x90
L57:
	cmpl $267,%edx
	je L56
L58:
	testl $-65536,%edx
	je L59
	movl %edx,%eax
#APP
	rorw $8, %ax; rorl $16, %eax; rorw $8, %ax
#NO_APP
	cmpw $204,%ax
	je L56
	jmp L54
	.align 2,0x90
L59:
	cmpl $204,%edx
	jne L54
L56:
	movl -28(%ebp),%ecx
	movl %ecx,%ebx
	testl $-65536,%edx
	je L63
	movl %edx,%eax
#APP
	rorw $8, %ax; rorl $16, %eax; rorw $8, %ax
#NO_APP
	cmpw $267,%ax
	jne L64
	jmp L61
	.align 2,0x90
L63:
	movl %edx,%eax
	orl $65536,%eax
	cmpl $267,%eax
	je L61
L64:
	testl $-65536,%edx
	je L65
	movl %edx,%eax
#APP
	rorw $8, %ax; rorl $16, %eax; rorw $8, %ax
#NO_APP
	andl $65535,%eax
	cmpl $65740,%eax
	jne L66
	jmp L61
	.align 2,0x90
L65:
	movl %edx,%eax
	orl $65536,%eax
	cmpl $65740,%eax
	je L61
L66:
	testl $-65536,%edx
	je L69
#APP
	rorw $8, %dx; rorl $16, %edx; rorw $8, %dx
#NO_APP
	movzwl %dx,%edx
	cmpl $65803,%edx
	je L70
	jmp L67
	.align 2,0x90
L69:
	orl $65536,%edx
	cmpl $65803,%edx
	jne L67
L70:
	leal 8191(%ecx),%eax
	jmp L85
	.align 2,0x90
L67:
	leal 4127(%ecx),%eax
	jmp L85
	.align 2,0x90
L61:
	leal 4095(%ecx),%eax
L85:
	andl $-4096,%eax
	jmp L72
	.align 2,0x90
L54:
	movl -28(%ebp),%ecx
	movl %ecx,%ebx
	testl $-65536,%edx
	je L73
	movl %edx,%eax
#APP
	rorw $8, %ax; rorl $16, %eax; rorw $8, %ax
#NO_APP
	cmpw $267,%ax
	jne L74
	jmp L71
	.align 2,0x90
L73:
	movl %edx,%eax
	orl $65536,%eax
	cmpl $267,%eax
	je L71
L74:
	testl $-65536,%edx
	je L75
	movl %edx,%eax
#APP
	rorw $8, %ax; rorl $16, %eax; rorw $8, %ax
#NO_APP
	andl $65535,%eax
	cmpl $65740,%eax
	jne L76
	jmp L71
	.align 2,0x90
L75:
	movl %edx,%eax
	orl $65536,%eax
	cmpl $65740,%eax
	je L71
L76:
	testl $-65536,%edx
	je L79
#APP
	rorw $8, %dx; rorl $16, %edx; rorw $8, %dx
#NO_APP
	movzwl %dx,%edx
	cmpl $65803,%edx
	je L80
	jmp L77
	.align 2,0x90
L79:
	orl $65536,%edx
	cmpl $65803,%edx
	jne L77
L80:
	leal 4096(%ecx),%eax
	jmp L72
	.align 2,0x90
L77:
	leal 32(%ecx),%eax
	jmp L72
	.align 2,0x90
L71:
	movl %ecx,%eax
L72:
	xorl %edx,%edx
	pushl %edx
	pushl %eax
	pushl $0
	pushl _crt.6+8
	pushl $18
	pushl $3
	pushl -24(%ebp)
	addl _crt.6,%ebx
	pushl %ebx
	pushl $0
	pushl $197
	pushl $198
	call ___syscall
	addl $48,%esp
	cmpl $-1,%eax
	jne L53
	pushl $18
	pushl $LC4
	pushl $2
	pushl $4
	call ___syscall
	addl $-8,%esp
	pushl $1
	pushl $1
	call ___syscall
	addl $32,%esp
L53:
	movl -20(%ebp),%eax
	testl %eax,%eax
	je L81
	addl $-4,%esp
	pushl $0
	pushl $0
	pushl $0
	pushl _crt.6+4
	pushl $4114
	pushl $3
	pushl %eax
	movl -24(%ebp),%eax
	addl -28(%ebp),%eax
	addl _crt.6,%eax
	pushl %eax
	pushl $0
	pushl $197
	pushl $198
	call ___syscall
	addl $48,%esp
	cmpl $-1,%eax
	jne L81
	pushl $18
	pushl $LC4
	pushl $2
	pushl $4
	call ___syscall
	addl $-8,%esp
	pushl $1
	pushl $1
	call ___syscall
	addl $32,%esp
L81:
	movl %edi,_crt.6+12
	movl _environ,%eax
	movl %eax,_crt.6+16
	movl $__callmain,_crt.6+20
	movl ___progname,%eax
	movl %eax,_crt.6+24
	movl $_crt.6,%esi
	movl _crt.6,%ebx
	addl $32,%ebx
	addl $-8,%esp
	pushl %esi
	pushl $4
	call *%ebx
	addl $16,%esp
	cmpl $-1,%eax
	jne L82
	pushl $33
	pushl $LC5
	pushl $2
	pushl $4
	call ___syscall
	addl $-8,%esp
	pushl %esi
	pushl $3
	call *%ebx
	addl $32,%esp
	cmpl $-1,%eax
	jne L83
	pushl $14
	pushl $LC6
	pushl $2
	pushl $4
	call ___syscall
	addl $-8,%esp
	pushl $1
	pushl $1
	call ___syscall
L83:
	movl 12(%edi),%eax
	movl %eax,_ld_entry
	jmp L7
	.align 2,0x90
L82:
	movl _crt.6+32,%eax
	movl %eax,_ld_entry
	addl $-12,%esp
	pushl 16(%eax)
	call _atexit
L7:
	leal -56(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	leave
	ret
	.size	___load_rtld , . - ___load_rtld
	.align 2,0x90
.globl _dlopen
	.type	_dlopen , @function
_dlopen:
	pushl %ebp
	movl %esp,%ebp
	subl $8,%esp
	movl _ld_entry,%eax
	testl %eax,%eax
	je L87
	addl $-8,%esp
	pushl 12(%ebp)
	pushl 8(%ebp)
	movl (%eax),%eax
	call *%eax
	jmp L88
	.align 2,0x90
L87:
	xorl %eax,%eax
L88:
	leave
	ret
	.size	_dlopen , . - _dlopen
	.align 2,0x90
.globl _dlclose
	.type	_dlclose , @function
_dlclose:
	pushl %ebp
	movl %esp,%ebp
	subl $8,%esp
	movl _ld_entry,%eax
	testl %eax,%eax
	je L90
	addl $-12,%esp
	pushl 8(%ebp)
	movl 4(%eax),%eax
	call *%eax
	jmp L91
	.align 2,0x90
L90:
	movl $-1,%eax
L91:
	leave
	ret
	.size	_dlclose , . - _dlclose
	.align 2,0x90
.globl _dlsym
	.type	_dlsym , @function
_dlsym:
	pushl %ebp
	movl %esp,%ebp
	subl $8,%esp
	movl _ld_entry,%eax
	testl %eax,%eax
	je L93
	addl $-8,%esp
	pushl 12(%ebp)
	pushl 8(%ebp)
	movl 8(%eax),%eax
	call *%eax
	jmp L94
	.align 2,0x90
L93:
	xorl %eax,%eax
L94:
	leave
	ret
	.size	_dlsym , . - _dlsym
	.align 2,0x90
.globl _dlctl
	.type	_dlctl , @function
_dlctl:
	pushl %ebp
	movl %esp,%ebp
	subl $8,%esp
	movl _ld_entry,%eax
	testl %eax,%eax
	je L96
	addl $-4,%esp
	pushl 16(%ebp)
	pushl 12(%ebp)
	pushl 8(%ebp)
	movl 12(%eax),%eax
	call *%eax
	jmp L97
	.align 2,0x90
L96:
	movl $-1,%eax
L97:
	leave
	ret
	.size	_dlctl , . - _dlctl
LC7:
	.ascii "Service unavailable\0"
	.align 2,0x90
.globl _dlerror
	.type	_dlerror , @function
_dlerror:
	pushl %ebp
	movl %esp,%ebp
	subl $24,%esp
	movl _ld_entry,%edx
	testl %edx,%edx
	je L100
	addl $-4,%esp
	leal -4(%ebp),%eax
	pushl %eax
	pushl $1
	pushl $0
	movl 12(%edx),%eax
	call *%eax
	addl $16,%esp
	cmpl $-1,%eax
	jne L99
L100:
	movl $LC7,%eax
	jmp L102
	.align 2,0x90
L99:
	movl -4(%ebp),%eax
	testl %eax,%eax
	je L101
	addl $-12,%esp
	pushl %eax
	call _strerror
	jmp L102
	.align 2,0x90
L101:
	xorl %eax,%eax
L102:
	leave
	ret
	.size	_dlerror , . - _dlerror
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
