{
    $Id: signal.inc,v 1.1.2.7 2003/03/14 09:32:53 pierre Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Jonas Maebe,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$packrecords C}

{********************
      Signal
********************}
type
  SigSet  = Longint;
  PSigSet = ^SigSet;

Const
  { For sending a signal }
  SA_NOCLDSTOP = 1;
  SA_SHIRQ     = $04000000;
  SA_STACK     = $08000000;
  SA_RESTART   = $10000000;
  SA_INTERRUPT = $20000000;
  SA_NOMASK    = $40000000;
  SA_ONESHOT   = $80000000;
  SA_ONSTACK   = SA_STACK;

  SIG_BLOCK   = 0;
  SIG_UNBLOCK = 1;
  SIG_SETMASK = 2;

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
  SIGBUS     = 7;
  SIGFPE     = 8;
  SIGKILL    = 9;
  SIGUSR1    = 10;
  SIGSEGV    = 11;
  SIGUSR2    = 12;
  SIGPIPE    = 13;
  SIGALRM    = 14;
  SIGTerm    = 15;
  SIGSTKFLT  = 16;
  SIGCHLD    = 17;
  SIGCONT    = 18;
  SIGSTOP    = 19;
  SIGTSTP    = 20;
  SIGTTIN    = 21;
  SIGTTOU    = 22;
  SIGURG     = 23;
  SIGXCPU    = 24;
  SIGXFSZ    = 25;
  SIGVTALRM  = 26;
  SIGPROF    = 27;
  SIGWINCH   = 28;
  SIGIO      = 29;
  SIGPOLL    = SIGIO;
  SIGPWR     = 30;
  SIGUNUSED  = 31;


const
  SI_PAD_SIZE	= ((128/sizeof(longint)) - 3);

type
  Size_T = cardinal;

{$Ifdef i386}
  tfpreg = record
	  significand: array[0..3] of word;
	  exponent: word;
  end;

  pfpstate = ^tfpstate;
  tfpstate = record
	   cw, sw, tag, ipoff, cssel, dataoff, datasel: cardinal;
	   st: array[0..7] of tfpreg;
	   status: cardinal;
  end;


  PSigContextRec = ^SigContextRec;
  SigContextRec = record
    gs, __gsh: word;
    fs, __fsh: word;
    es, __esh: word;
    ds, __dsh: word;
    edi: cardinal;
    esi: cardinal;
    ebp: cardinal;
    esp: cardinal;
    ebx: cardinal;
    edx: cardinal;
    ecx: cardinal;
    eax: cardinal;
    trapno: cardinal;
    err: cardinal;
    eip: cardinal;
    cs, __csh: word;
    eflags: cardinal;
    esp_at_signal: cardinal;
    ss, __ssh: word;
    fpstate: pfpstate;
    oldmask: cardinal;
    cr2: cardinal;
  end;
{$ENDIF}

{$Ifdef m68k}

  { REMARK: floating point regs are defined as arrays of
    3 longints; I don't know if C does align this to
    16 byte boundaries for each element of the array PM }
  { If C does we might need to define this as
    array from 0 to 3 }

  tfpreg = array[0..2] of longint;

  pfpstate = ^tfpstate;
  tfpstate = record
           pcr,psr,fpiaddr : longint;
	   fpreg : array [0..7] of tfpreg;
  end;

  PSigContextRec = ^SigContextRec;
  SigContextRec = record
 	sc_mask  : dword; { old sigmask }
	sc_usp   : dword; { old user stack pointer }
	sc_d0,sc_d1    : dword; {}
	sc_a0, sc_a1 : dword;
	sc_sr : word;
	sc_pc : dword;
	sc_formatvec : word;
	sc_fpregs : array [0..1] of tfpreg; { room for two fp registers }
	sc_fpcr, sc_fpst, sc_fpaddr : dword;
	sc_fpstate : array [0..216-1] of byte;
    end;
{$ENDIF}

(*
  PSigInfoRec = ^SigInfoRec;
  SigInfoRec = record
    si_signo: longint;
    si_errno: longint;
    si_code: longint;

    case longint of
      0:
        (pad: array[SI_PAD_SIZE] of longint);
      1: { kill }
        ( kill: record
            pid: longint;  { sender's pid }
            uid : longint; { sender's uid }
          end );
      2: { POSIX.1b timers }
        ( timer : record
            timer1 : cardinal;
            timer2 : cardinal;
           end );
      3: { POSIX.1b signals }
        ( rt : record
            pid : longint;    { sender's pid }
            uid : longint;    { sender's uid }
            sigval : longint;
         end );
      4: { SIGCHLD }
        ( sigchld : record
          pid : longint;    { which child }
          uid : longint;    { sender's uid }
          status : longint; { exit code }
          utime : timeval;
          stime : timeval;
         end );
      5: { SIGILL, SIGFPE, SIGSEGV, SIGBUS }
        ( sigfault : record
            addr : pointer;{ faulting insn/memory ref. }
          end );
      6:
        ( sigpoll : record
            band : longint; { POLL_IN, POLL_OUT, POLL_MSG }
            fd : longint;
          end );
  end;
*)

  SignalHandler   = Procedure(Sig : Longint);cdecl;
  PSignalHandler  = ^SignalHandler;
  SignalRestorer  = Procedure;cdecl;
  PSignalRestorer = ^SignalRestorer;
  TSigAction = procedure(Sig: Longint; SigContext: SigContextRec);cdecl;

  SigActionRec = packed record
    Handler  : record
      case byte of
        0: (Sh: SignalHandler);
        1: (Sa: TSigAction);
      end;
    Sa_Mask     : SigSet;
    Sa_Flags    : Longint;
    Sa_restorer : SignalRestorer; { Obsolete - Don't use }
  end;
  PSigActionRec = ^SigActionRec;

const
  SS_ONSTACK = 1;
  SS_DISABLE = 2;
  MINSIGSTKSZ = 2048;
  SIGSTKSZ    = 8192;

type
  SigAltStack = record
    ss_sp : pointer;
    ss_flags : longint;
    ss_size : size_t;
  end;

  stack_t = sigaltstack;

  PSigAltStack = ^SigAltStack;

  pstack_t = ^stack_t;

{
  $Log: signal.inc,v $
  Revision 1.1.2.7  2003/03/14 09:32:53  pierre
   * fix m68k failure due to last commit

  Revision 1.1.2.6  2003/03/14 08:55:28  pierre
   + add SA_ONSTACK for SIGSEGV handling if SigAltStack is used

  Revision 1.1.2.5  2003/03/13 23:01:56  pierre
   + Use SigAltStack

  Revision 1.1.2.4  2002/10/11 12:05:03  pierre
   * fix linux m68k fpu exceptions

  Revision 1.1.2.3  2002/10/11 09:05:41  pierre
   + m68k SigContext record

  Revision 1.1.2.2  2001/06/18 22:22:42  pierre
   + fake SigContextRec structure for m68k processor

  Revision 1.1.2.1  2000/09/10 16:17:25  marco
   * added (moved from sockets.pp and i386/signal.inc)

  Revision 1.1  2000/07/13 06:30:55  michael
  + Initial import

  Revision 1.2  2000/03/31 23:11:23  pierre
   * TSigAction Context param is the full record not a pointer

  Revision 1.1  2000/03/31 13:24:28  jonas
    * signal handling using sigaction when compiled with -dnewsignal
      (allows multiple signals to be received in one run)

}
