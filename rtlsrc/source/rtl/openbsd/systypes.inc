{
    $Id: systypes.inc,v 1.1.2.1 2002/07/29 17:15:59 marco Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
type

{ The following are records for system calls BSD updated }
  dirent = packed record
    ino    : cardinal;		{ This is not inode number, but "a number
				  unique for each file on a filesystem"}
    reclen : word;
    d_type,
    namlen : byte;
    name   : array [0..255] of char;
  end;

  pdirent =^dirent;
  TDirEnt = dirent;

    TDir= packed record {BSD libc record.}
            fd    : longint;    { file descriptor associated with directory }
            loc,                { offset in current buffer }
            size  : cardinal;   { amount of data returned by getdirentries}
            buf   : pdirent;    { data buffer, actually a pchar}
            len   : longint;    { size of data buffer 	}
            seek,               { magic cookie returned by getdirentries}
            rewind: cardinal;   { magic cookie for rewinding}
            flags : longint;    { flags for readdir }
            end;

  PDir =^TDir;

{$packrecords C}
 dev_t	= Longint; // taken from sysunix.getdir to avoid ifdef's.
 ino_t  = Cardinal;
 mode_t = word;
 nlink_t= word; 
 uid_t  = cardinal;
 gid_t  = cardinal;
 time_t = cardinal;
 off_t  = int64;     // probably actually qword, but this is 1.0.x
 fsid_t = array[0..1] of longint;

 Stat =record {BSD version}
        dev        :  dev_t;            { inode's device }
        ino        :  ino_t;    	{ inode's number }
        mode       :  mode_t; 		{ inode protection mode }
        nlink	   :  nlink_t;          { number of hard links }
        uid	   :  uid_t;		{ user ID of the file's owner }
        gid	   :  gid_t;		{ group ID of the file's group }
        dev_t	   :  dev_t;		{ device type }
	atime      :  time_t;  		{ time of last access }
	atime_nsec :  cardinal;         { nsec of last access }
	mtime	   :  time_t;         	{ time of last data modification }
	mtime_nsec :  cardinal;         { nsec of last data modification }
	ctime      :  time_t;       	{ time of last file status change }
	ctime_nsec :  cardinal;      	{ nsec of last file status change }
	size       :  off_t;     	{ file size, in bytes }
	blocks     :  Int64;		{ blocks allocated for file }
	blksze     :  cardinal;     	{ optimal blocksize for I/O }
	flags      :  cardinal;     	{ user defined flags for file }
	filegen    :  cardinal;		{ file generation number }
 	lspare	   : longint;
	qspare     : array[0..1] of int64;
       end;

  PStat=^Stat;
  TStat=Stat;

  Statfs = packed record
    f_flags     : Cardinal;  		// copy of mount flags
    bsize       : Longint;	      	{ fundamental block size}
    iosize      : Cardinal;           	{ optimal block size }
    blocks      : Cardinal;           	{  total blocks}
    bfree       : Cardinal;    		{ blocks free}
    bavail      : Longint;    		{ block available for mortal users}
    files       : Cardinal;	  	{ Total file nodes}
    ffree       : Cardinal; 	   	{ file nodes free}
    fsid        : fsid_t;		// file system id
    fowner      : uid_t; 	        {mounter uid}
    fsyncwrites : Cardinal;		// count of sync writes since mount
    fasyncwrites: Cardinal;		// count of async writes since mount
    f_ctime     : Cardinal;		// last mount time
    spare	: array[0..2] of Cardinal;
    fstypename  : array[0..MFSNAMELEN-1] OF Char;    	// fs name
    mntonname   : array[0..MNAMELEN-1] OF Char;	  	// mount point
    mnfromnname : array[0..MNAMELEN-1] OF Char;		// mounted fs
    Mountoptalign: array[0..159] of char;		// Mountoptions union
  end;

  PStatFS=^StatFS;
  TStatFS=StatFS;

  fdSet=array[0..7] of longint;  {=256 bits. OpenBSD, check}
  pfdset=^fdset;
  TFDSet=fdset;

  timeval = packed record
    sec,
    usec	:longint;
  end;
  ptimeval=^timeval;
  TTimeVal=timeval;

  timespec = packed record
    tv_sec  : time_t;
    tv_nsec : longint;
  end;

  timezone = packed record
    minuteswest,
    dsttime			: longint;
  end;
  ptimezone =^timezone;
  TTimeZone = timezone;

  utsname = packed record
    sysname,
    nodename,
    release,
    version,
    machine,
    domainname : Array[0..SYSNMLN-1] of char;
  end;
  PUTSName=^UTSName;
  TUTSName=UTSName;

{
  $Log: systypes.inc,v $
  Revision 1.1.2.1  2002/07/29 17:15:59  marco
   * Initial OpenBSD port. One days work. Still problems with initcode

  Revision 1.1.2.2  2001/08/30 23:00:18  marco
   * Fix for stat record.

  Revision 1.1.2.1  2001/08/10 11:07:17  pierre
   New NetBSD files taken and adapted from FreeBSD

  Revision 1.1.2.5  2001/03/12 15:01:39  marco
   * [Solaris] introduced dev_t

  Revision 1.1.2.4  2001/01/20 16:54:51  marco
   * Fixed timeval. Must have thought that long=int64 when I did initial port?

  Revision 1.1.2.3  2000/10/30 14:36:07  marco
   * timespec from int64 to longint.

  Revision 1.1.2.2  2000/10/24 12:14:37  pierre
   + timespec type for syscall_nr_nanosleep

  Revision 1.1.2.1  2000/09/16 11:19:08  marco
   * Moved files from BSD to FreeBSD directory, with some small changes

  Revision 1.1.2.1  2000/09/10 16:12:14  marco
  Initial signals, sockets and clone

  Revision 1.1  2000/07/13 06:30:33  michael
  + Initial import

  Revision 1.8  2000/04/16 16:08:30  marco
   * Updated PDir to BSD libc layout. (which is totally different from Linux)

  Revision 1.7  2000/04/10 15:46:52  marco
   * worked all day. probably a lot changed

  Revision 1.5  2000/03/17 12:58:57  marco
   * some changes to ftruncate based procs. Added a "0" as extra parameter

  Revision 1.4  2000/02/04 16:55:47  marco
   * Fixed tdir, some params need to be 32-bit

  Revision 1.3  2000/02/03 17:05:55  marco

   * Some types fixed/ported.

  Revision 1.2  2000/02/02 16:45:38  marco
   * Typo in STAT record

  Revision 1.1  2000/02/02 16:36:09  marco
   * Initial version. Copy of linux version, with BSD stat.

}
