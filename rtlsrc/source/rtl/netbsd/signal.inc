{
    $Id: signal.inc,v 1.1.2.5 2003/03/13 23:53:41 pierre Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Jonas Maebe,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


Const   { For sending a signal }

  SA_NOCLDSTOP = 8;
  SA_ONSTACK   = $001; { take signal on signal stack }
  SA_RESTART   = $002; { restart system call on signal return }
  SA_RESETHAND = $004; { reset to SIG_DFL when taking signal }
  SA_NODEFER   = $010; { don't mask the signal we're delivering }
  SA_NOCLDWAIT = $020; { don't keep zombies around }
  SA_SIGINFO   = $040; { signal handler with SA_SIGINFO args }
  SA_USERTRAMP = $100; { SUNOS compat: Do not bounce off kernel's sigtramp }

  SIG_BLOCK   = 1;
  SIG_UNBLOCK = 2;
  SIG_SETMASK = 3;

{BSD Checked}
  SIG_DFL = 0 ;
  SIG_IGN = 1 ;
  SIG_ERR = -1 ;

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
  SIGCHLD   = 20;
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


{$packrecords C}
const
  SI_PAD_SIZE   = ((128/sizeof(longint)) - 3);

{
 * The sequence of the fields/registers in struct sigcontext should match
 * those in mcontext_t.
 }

type sigset_t = array [0..3] of cardinal;

    PSigContextRec = ^SigContextRec;
    SigContextRec = record
{$ifdef i386}
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
       //sc_trapno    : longint;
       //sc_err       : longint;
       sc_eip       : longint;
       sc_cs        : longint;
       sc_efl       : longint;
       sc_esp       : longint;
       sc_ss        : longint;
       (* {
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
       fpr_pad      : array[0..63] of char; *)
       sc_onstack   : longint;              { sigstack state to restore }
       __sc_mask13    : longint;
       sc_trapno    : longint;
       sc_err       : longint;
       sc_mask      : sigset_t;          { signal mask to restore }
{$endif i386}
{$ifdef m68k}
       sc_onstack : longint;            { sigstack state to restore } 
       __sc_mask13 : longint;           { signal mask to restore (old style) }
       sc_sp : longint;                 { sp to restore }
       sc_fp : longint;                 { fp to restore }
       sc_ap : longint;                 { ap to restore }
       sc_pc : longint;                 { pc to restore }
       sc_psi : longint;                { psl to restore }
       sc_mask : sigset_t;              { signal mask to restore (new style) }
{$endif m68k}
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
    Sa_Mask     : SigSet;
    Sa_Flags    : Longint;
  end;
  PSigActionRec = ^SigActionRec;

{
 * Structure used in sigaltstack call. }

Const
  SS_ONSTACK = 1;
  SS_DISABLE = 2;
  MINSIGSTKSZ = 2048;
  SIGSTKSZ    = 8192;

Type
     size_t=cardinal;
     sigaltstack = Record
	ss_sp    : Pchar;			{ signal stack base }
	ss_size  : size_t;		{ signal stack length }
	ss_flags : Longint;		{ SS_DISABLE and/or SS_ONSTACK }
	end;
  stack_t = sigaltstack;
  psigaltstack=^sigaltstack;
  pstack_t=^stack_t;



{
  Change action of process upon receipt of a signal.
  Signum specifies the signal (all except SigKill and SigStop).
  If Act is non-nil, it is used to specify the new action.
  If OldAct is non-nil the previous action is saved there.
}


{
  $Log: signal.inc,v $
  Revision 1.1.2.5  2003/03/13 23:53:41  pierre
   * use the newer sigcontext version for m68k

  Revision 1.1.2.4  2003/03/13 23:44:12  marco
   * added m68k contextrec

  Revision 1.1.2.3  2003/03/13 22:41:37  marco
   * sigalstack for netbsd

  Revision 1.1.2.2  2003/01/31 14:08:30  pierre
   * fix sigaction for netbsd

  Revision 1.1.2.1  2001/08/10 11:07:17  pierre
   New NetBSD files taken and adapted from FreeBSD

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
