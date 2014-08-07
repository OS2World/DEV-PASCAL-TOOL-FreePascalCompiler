{
    $Id: errno.inc,v 1.1.2.4 2001/12/09 03:25:36 carl Exp $
    This file is part of the Free Pascal run time library.

    Solaris error codes (solaris 7)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{***********************************************************************}
{                       POSIX ERROR DEFINITIONS                         }
{***********************************************************************}
const
    { The following constants are system dependent but must all exist }
    Sys_E2BIG       =  7;
    Sys_EACCES      = 13;
    Sys_EAGAIN      = 11;
    Sys_EBADF       =  9;
    Sys_EBUSY       = 16;
    Sys_ECANCELED   = 47;
    Sys_ECHILD      = 10;
    Sys_EDEADLK     = 45;
    Sys_EDOM        = 33;
    Sys_EEXIST      = 17;
    Sys_EFAULT      = 14;
    Sys_EFBIG       = 27;
    Sys_EINPROGRESS =150;
    Sys_EINTR       =  4;
    Sys_EINVAL      = 22;
    Sys_EIO         =  5;
    Sys_EISDIR      = 21; 
    Sys_EMFILE      = 24;
    Sys_EMLINK      = 31;
    Sys_EMSGSIZE    = 97;
    Sys_ENAMETOOLONG= 78;
    Sys_ENFILE      = 23;
    Sys_ENODEV      = 19;
    Sys_ENOENT      =  2;
    Sys_ENOEXEC     =  8;
    Sys_ENOLCK      = 46;
    Sys_ENOMEM      = 12;
    Sys_ENOSPC      = 28;
    Sys_ENOSYS      = 89;
    Sys_ENOTDIR     = 20;
    Sys_ENOTEMPTY   = 93;
    Sys_ENOTTY      = 25;
    Sys_ENXIO       =  6;
    Sys_EPERM       =  1;
    Sys_EPIPE       = 32;
    Sys_ERANGE      = 34;
    Sys_EROFS       = 30;
    Sys_ESPIPE      = 29;
    Sys_ESRCH       =  3;
    Sys_ETIMEDOUT   =145; 
    Sys_EXDEV       = 18;
    Sys_ENOTSUP     = 48;
    { These next errors are POSIX, but only defined when    }
    { certain types of POSIX extensions are defined:        }
    Sys_EBADMSG     = 77;


    { Solars specific error codes }
    Sys_ENOTBLK	     = 15;        { block device required       }
    Sys_ETXTBSY      = 26;        { text file busy        	}
    Sys_ENOMSH	     = 35;        { no message of desired type  }	    
    Sys_EIDRM	     = 36;        { identifier removed    	}
    Sys_ECHRNG       = 37;        { channel number out of range }	
    Sys_EL2NSYNC     = 38;        { Level 2 not synchronized    }
    Sys_EL3HLT	     = 39;	  { Level 3 halted              }
    Sys_EL3RST	     = 40;        { Level 3 reset               }
    Sys_ELNRNG	     = 41;        { Link number out of range    }	
    Sys_EUNATCH      = 42;        { Protocol driver not attached }
    Sys_ENOCSI	     = 43;        { No CSI structure available   }
    Sys_EL2HLT	     = 44;        { Level 2 halted               }
    Sys_EDQUOT       = 49;        { Disk quota exceeded          }
    Sys_ENOSTR       = 60;        { Device not a stream          }    	
    Sys_ENODATA      = 61;        { No data available            }
    Sys_ETIME        = 62;        { Timer expired                }
    Sys_ENOSR        = 63;        { Out of stream resources      }
    Sys_ENONET       = 64;        { Machine not on network       }
    Sys_ENOPKG	     = 65;        { Package not installed        }
    Sys_EROMOTE      = 66;        { Object is remote             }
    Sys_ENOLINK      = 67;        { Link has been severed        }
    Sys_EADV         = 68;        { Advertise error              }
    Sys_ESRMNT       = 69;        { Srmount error                }
    Sys_ECOMM        = 70;        { Comm. error on send          }
    Sys_EPROTO       = 71;        { Protocol error               }
    Sys_EDOTDOT      = 76;        
    Sys_EOVERFLOW    = 79;
    Sys_ENOTUNIQ     = 80;        { Name not unique on network   }   
    Sys_EBADFD       = 81;        { File descriptor in bad state }
    Sys_EREMCHG      = 82;        { Remote address changed       }
    Sys_ELIBACC      = 83;        { Cannot access shared library }
    Sys_ELIBBAD      = 84;        { Accessing corrupt shared lib }
    Sys_ELIBSCN      = 85;        { .lib section corrupted       }
    Sys_ELIBMAX      = 86;        { Link in more shared libs than limit }
    Sys_ELIBEXEC     = 87;        { Cannot exec a shared lib. directly  }
    Sys_EILSEQ       = 88;        { Illegal byte sequence               }
    Sys_ELOOP        = 90;       
    Sys_ESTART       = 91;        { Restartable syscall                 }
    Sys_ESTRPIPE     = 92;
    Sys_EUSERS       = 94;        { Too many users                      }
    Sys_ENOTSOCK     = 95;        { Socket operation on non-socket      }
    Sys_EDESTADDRREQ = 96;        { Destination address required        }   
    Sys_EPROTOTYPE   = 98;        { Protocol wrong type for socket      }    
    Sys_ENOPROTOPT   = 99;        { Protocol not available              }
    Sys_EPROTONOSUPPORT = 120;    { Protocol not supported              }
    Sys_ESOCKTNOTSUPPORT = 121;   { Socket type not supported           }
    Sys_EOPNOTSUPP   =122;        
    Sys_EPFNOSUPPORT =123;        { Protocol family not supported       }
    Sys_EAFNOSUPPORT =124;        
    Sys_EADDRINUSE   =125;        { Address already in use              }
    Sys_EADDRNOTAVAIL=126;        { Cannot assign requested address     }
    Sys_ENETDOWN     =127;        { Network is down                     }
    Sys_ENETUNREACH  =128;        { Network is unreachable              }
    Sys_ENETRESET    =129;        { Network dropped connection because of reset } 
    Sys_ECONNABORTED =130;	  { Software caused connection abort	}
    Sys_ECONNRESET   =131;        { Connection reset by peer            }
    Sys_ENOBUFS      =132;        { No buffer space available           }
    Sys_EISCONN      =133;        { Transport endpoint is already connected }
    Sys_ENOTCONN     =134;        { Transport endpoint is not connected }
    Sys_ESHUTDOWN    =143;        { Cannot send after transport endpoint shutdown }
    Sys_ETOOMANYREFS =144;        { Too many references: cannot splice  }
    Sys_ECONNREFUSED =146;        { Connection refused                  }
    Sys_EHOSTDOWN    =147;        { Host is down                        }    
    Sys_EHOSTUNREACH =148;        { No route to host                    }
    Sys_EALREADY     =149;        { operation already in progress       }
    Sys_ESTALE       =151;        { Stale NFS file handle               }


{
  $Log: errno.inc,v $
  Revision 1.1.2.4  2001/12/09 03:25:36  carl
  + reinstated

  Revision 1.1.2.1  2001/12/04 00:46:25  carl
  + initial import

  Revision 1.1.2.2  2001/11/30 03:52:11  carl
  * renamed spelling mistake for Sys_EACCES

  Revision 1.1.2.1  2001/11/26 03:00:37  carl
  + continued solaris port


}