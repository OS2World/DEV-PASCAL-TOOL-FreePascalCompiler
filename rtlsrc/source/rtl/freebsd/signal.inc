{
    $Id: signal.inc,v 1.1.2.4 2003/03/13 22:24:49 marco Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Marco van de Voort,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Type        Size_t=cardinal;

Const   { For sending a signal }

  SA_NOCLDSTOP = 8;
  SA_ONSTACK   = $001; { take signal on signal stack }
  SA_RESTART   = $002; { restart system call on signal return }
  SA_RESETHAND = $004; { reset to SIG_DFL when taking signal }
  SA_NODEFER   = $010; { don't mask the signal we're delivering }
  SA_NOCLDWAIT = $020; { don't keep zombies around }
  SA_SIGINFO   = $040; { signal handler with SA_SIGINFO args }
  SA_USERTRAMP = $100; { SUNOS compat: Do not bounce off kernel's sigtramp }

  SIG_BLOCK     = 1;
  SIG_UNBLOCK   = 2;
  SIG_SETMASK =   3;

{BSD Checked}
  SIG_DFL    = 0 ;
  SIG_IGN    = 1 ;
  SIG_ERR    = -1 ;

  SIGHUP     = 1;
  SIGINT     = 2;
  SIGQUIT    = 3;
  SIGILL     = 4;
  SIGTRAP    = 5;
  SIGABRT    = 6;
  SIGIOT     = 6;
  SIGEMT     = 7;
  SIGFPE     = 8;
  SIGKILL    = 9;
  SIGBUS     = 10;
  SIGSEGV    = 11;
  SIGSYS     = 12;
  SIGPIPE    = 13;
  SIGALRM    = 14;
  SIGTERM    = 15;
  SIGURG     = 16;
  SIGSTOP    = 17;
  SIGTSTP    = 18;
  SIGCONT    = 19;
  SIGCHLD    = 20;
  SIGTTIN    = 21;
  SIGTTOU    = 22;
  SIGIO      = 23;
  SIGXCPU    = 24;
  SIGXFSZ    = 25;
  SIGVTALRM  = 26;
  SIGPROF    = 27;
  SIGWINCH   = 28;
  SIGINFO    = 29;
  SIGUSR1    = 30;
  SIGUSR2    = 31;


Type

   Sigval = Record
   	    Case Boolean OF
	{ Members as suggested by Annex C of POSIX 1003.1b. }
		false : (sigval_int : Longint);
		True  : (sigval_ptr : Pointer);
	    End;

  TSigInfo_t = record
		si_signo,			{ signal number }
		si_errno,			{ errno association }
	{
	 * Cause of signal, one of the SI_ macros or signal-specific
	 * values, i.e. one of the FPE_... values for SIGFPE. This
	 * value is equivalent to the second argument to an old-style
	 * FreeBSD signal handler.
	 }
		si_code,			{ signal code }
		si_pid 		: Longint; 	{ sending process }
		si_uid 		: Cardinal;	{ sender's ruid }
		si_status	: Longint;	{ exit value }
		si_addr 	: Pointer;	{ faulting instruction }
  	        si_value 	: SigVal;	{ signal value }
		si_band 	: Cardinal;	{ band event for SIGPOLL }
		__spare 	: array[0..6] of Longint; { gimme some slack }
		end;


CONST
{machine/trap.h}

{
 * Trap type values
 * also known in trap.c for name strings
 }

        T_PRIVINFLT	=1;	{ privileged instruction }
        T_BPTFLT	=3;	{ breakpoint instruction }
        T_ARITHTRAP	=6;	{ arithmetic trap }
        T_ASTFLT	=7;	{ system forced exception }
        T_PROTFLT	=9;	{ protection fault }
        T_TRCTRAP	=10;	{ debug exception (sic) }
        T_PAGEFLT	=12;	{ page fault }
        T_ALIGNFLT	=14;	{ alignment fault }
    
        T_DIVIDE	=18;	{ integer divide fault }
        T_NMI		=19;	{ non-maskable trap }
        T_OFLOW		=20;	{ overflow trap }
        T_BOUND		=21;	{ bound instruction fault }
        T_DNA		=22;	{ device not available fault }
        T_DOUBLEFLT	=23;	{ double fault }
        T_FPOPFLT	=24;	{ fp coprocessor operand fetch fault }
        T_TSSFLT	=25;	{ invalid tss fault }
        T_SEGNPFLT	=26;	{ segment not present fault }
        T_STKFLT	=27;	{ stack fault }
        T_MCHK		=28;	{ machine check trap }
        T_XMMFLT	=29;	{ SIMD floating-point exception }
        T_RESERVED	=30;	{ reserved (unknown) }

{ XXX most of the following codes aren't used, but could be. }

{ definitions for <sys/signal.h> }
//        ILL_RESAD_FAULT	=T_RESADFLT;
        ILL_PRIVIN_FAULT=T_PRIVINFLT;
//        ILL_RESOP_FAULT	=T_RESOPFLT;
        ILL_ALIGN_FAULT	=T_ALIGNFLT;
        ILL_FPOP_FAULT	=T_FPOPFLT;	{ coprocessor operand fault }

{ portable macros for SIGFPE/ARITHTRAP }
        FPE_INTOVF	=1;	{ integer overflow }
        FPE_INTDIV	=2;	{ integer divide by zero }
        FPE_FLTDIV	=3;	{ floating point divide by zero }
        FPE_FLTOVF	=4;	{ floating point overflow }
        FPE_FLTUND	=5;	{ floating point underflow }
        FPE_FLTRES	=6;	{ floating point inexact result }
        FPE_FLTINV	=7;	{ invalid floating point operation }
        FPE_FLTSUB	=8;	{ subscript out of range }

{ codes for SIGBUS }
        BUS_PAGE_FAULT	=T_PAGEFLT;	{ page fault protection base }
        BUS_SEGNP_FAULT	=T_SEGNPFLT;	{ segment not present }
        BUS_STK_FAULT	=T_STKFLT;	{ stack segment }
        BUS_SEGM_FAULT	=T_RESERVED;	{ segment protection base }

{ Trap's coming from user mode }
        	T_USER	=$100;




{$packrecords C}
const
  SI_PAD_SIZE   = ((128/sizeof(longint)) - 3);

{
 * The sequence of the fields/registers in struct sigcontext should match
 * those in mcontext_t.
 }

type sigset_t = array[0..3] of longint;

// typedef struct __mcontext {

{
 * Structure used in sigaltstack call. }

Type  sigaltstack = Record
	ss_sp    : Pchar;			{ signal stack base }
	ss_size  : size_t;		{ signal stack length }
	ss_flags : Longint;		{ SS_DISABLE and/or SS_ONSTACK }
	end;
  stack_t = sigaltstack;
  psigaltstack=^sigaltstack;
  pstack_t=^stack_t;


Type mcontext_t=Record
	{
	 * The first 20 fields must match the definition of
	 * sigcontext. So that we can support sigcontext
	 * and ucontext_t at the same time.
	 }
	 
	mc_onstack,		{ XXX - sigcontext compat. }
	mc_gs,
	mc_fs,
	mc_es,
	mc_ds,
	mc_edi,
	mc_esi,
	mc_ebp,
	mc_isp,
	mc_ebx,
	mc_edx,
	mc_ecx,
	mc_eax,
	mc_trapno,
	mc_err,
	mc_eip,
	mc_cs,
	mc_eflags,
	mc_esp,			{ machine state }
	mc_ss : Longint;

	imc_fpregs : array[0..27] Of Longint;		{ env87 + fpacc87 + u_long }
	spare : array[0..16] Of Longint;
	end;
	
	pucontext=^ucontext;
	ucontext=record
	{
	 * Keep the order of the first two fields. Also,
	 * keep them the first two fields in the structure.
	 * This way we can have a union with struct
	 * sigcontext and ucontext_t. This allows us to
	 * support them both at the same time.
	 * note: the union is not defined, though.
	 }
	uc_sigmask   : sigset_t;
	muc_mcontext : mcontext_t;
	uc_link	     : pucontext;
	uc_stack     : stack_t;
	spares	     : array[0..7] Of Longint;
       end;

    PSigContextRec = ^SigContextRec;
    SigContextRec = record
       sc_mask      : sigset_t;          { signal mask to restore }
       sc_onstack   : longint;              { sigstack state to restore }

       sc_gs        : longint;              { machine state (struct trapframe): }
       sc_fs        : longint;
       sc_es        : longint;
       sc_ds        : longint;
       sc_edi       : longint;
       sc_esi       : longint;
       sc_ebp       : longint;
       sc_isp       : longint;
       sc_ebx       : longint;
       sc_edx       : longint;
       sc_ecx       : longint;
       sc_eax       : longint;
       sc_trapno    : longint;
       sc_err       : longint;
       sc_eip       : longint;
       sc_cs        : longint;
       sc_efl       : longint;
       sc_esp       : longint;
       sc_ss        : longint;
        {
         * XXX FPU state is 27 * 4 bytes h/w, 1 * 4 bytes s/w (probably not
         * needed here), or that + 16 * 4 bytes for emulators (probably all
         * needed here).  The "spare" bytes are mostly not spare.
         }
       en_cw        : cardinal;     { control word (16bits used) }
       en_sw        : cardinal;     { status word (16bits) }
       en_tw        : cardinal;     { tag word (16bits) }
       en_fip       : cardinal;     { floating point instruction pointer }
       en_fcs       : word;         { floating code segment selector }
       en_opcode    : word;         { opcode last executed (11 bits ) }
       en_foo       : cardinal;     { floating operand offset }
       en_fos       : cardinal;     { floating operand segment selector }
       fpr_acc      : array[0..79] of char;
       fpr_ex_sw    : cardinal;
       fpr_pad      : array[0..63] of char;
       end;

  SignalHandler   = Procedure(Sig : Longint);cdecl;
  PSignalHandler  = ^SignalHandler;
  SignalRestorer  = Procedure;cdecl;
  PSignalRestorer = ^SignalRestorer;
  TSigAction = procedure(Sig: Longint; SigContext: SigContextRec;someptr:pointer);cdecl;

  Sigset=sigset_t;
  PSigSet = ^SigSet;

  SigActionRec = packed record
    Handler  : record
      case byte of
        0: (Sh: SignalHandler);
        1: (Sa: TSigAction);
      end;
    Sa_Flags    : Longint;
    Sa_Mask     : SigSet;
  end;
  PSigActionRec = ^SigActionRec;

{
  Change action of process upon receipt of a signal.
  Signum specifies the signal (all except SigKill and SigStop).
  If Act is non-nil, it is used to specify the new action.
  If OldAct is non-nil the previous action is saved there.
}

{
  $Log: signal.inc,v $
  Revision 1.1.2.4  2003/03/13 22:24:49  marco
   * sigaltstack changed to pointer

  Revision 1.1.2.3  2002/11/17 20:54:28  marco
   * back on 16 bytes sigset. This is driving me nuts.

  Revision 1.1.2.2  2002/01/29 20:59:18  marco
   * AdditionsCVS: ----------------------------------------------------------------------

  Revision 1.1.2.1  2000/09/16 11:19:08  marco
   * Moved files from BSD to FreeBSD directory, with some small changes

  Revision 1.1.2.1  2000/09/10 16:18:41  marco
  initial version

  Revision 1.2  2000/03/31 23:11:23  pierre
   * TSigAction Context param is the full record not a pointer

  Revision 1.1  2000/03/31 13:24:28  jonas
    * signal handling using sigaction when compiled with -dnewsignal
      (allows multiple signals to be received in one run)

}
