{
    $Id: errors.pp,v 1.1.2.1 2000/09/14 13:38:25 marco Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY;without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


Unit errors;

Interface

uses strings;

const
  sys_errn=125;
  sys_errlist:array[0..sys_errn-1] of pchar = (
        'Success',                              { 0 }
        'Operation not permitted',              { EPERM }
        'No such file or directory',            { ENOENT }
        'No such process',                      { ESRCH }
        'Interrupted system call',              { EINTR }
        'I/O error',                            { EIO }
        'No such device or address',            { ENXIO }
        'Arg list too long',                    { E2BIG }
        'Exec format error',                    { ENOEXEC }
        'Bad file number',                      { EBADF }
        'No child processes',                   { ECHILD }
        'Try again',                            { EAGAIN }
        'Out of memory',                        { ENOMEM }
        'Permission denied',                    { EACCES }
        'Bad address',                          { EFAULT }
        'Block device required',                { ENOTBLK }
        'Device or resource busy',              { EBUSY }
        'File exists',                          { EEXIST }
        'Cross-device link',                    { EXDEV }
        'No such device',                       { ENODEV }
        'Not a directory',                      { ENOTDIR }
        'Is a directory',                       { EISDIR }
        'Invalid argument',                     { EINVAL }
        'File table overflow',                  { ENFILE }
        'Too many open files',                  { EMFILE }
        'Not a typewriter',                     { ENOTTY }
        'Text file busy',                       { ETXTBSY }
        'File too large',                       { EFBIG }
        'No space left on device',              { ENOSPC }
        'Illegal seek',                         { ESPIPE }
        'Read-only file system',                { EROFS }
        'Too many links',                       { EMLINK }
        'Broken pipe',                          { EPIPE }
        'Math argument out of domain of func',  { EDOM }
        'Math result not representable',        { ERANGE }
        'Resource deadlock would occur',        { EDEADLK }
        'File name too long',                   { ENAMETOOLONG }
        'No record locks available',            { ENOLCK }
        'Function not implemented',             { ENOSYS }
        'Directory not empty',                  { ENOTEMPTY }
        'Too many symbolic links encountered',  { ELOOP }
        'Operation would block',                { EWOULDBLOCK }
        'No message of desired type',           { ENOMSG }
        'Identifier removed',                   { EIDRM }
        'Channel number out of range',          { ECHRNG }
        'Level 2 not synchronized',             { EL2NSYNC }
        'Level 3 halted',                       { EL3HLT }
        'Level 3 reset',                        { EL3RST }
        'Link number out of range',             { ELNRNG }
        'Protocol driver not attached',         { EUNATCH }
        'No CSI structure available',           { ENOCSI }
        'Level 2 halted',                       { EL2HLT }
        'Invalid exchange',                     { EBADE }
        'Invalid request descriptor',           { EBADR }
        'Exchange full',                        { EXFULL }
        'No anode',                             { ENOANO }
        'Invalid request code',                 { EBADRQC }
        'Invalid slot',                         { EBADSLT }
        'File locking deadlock error',          { EDEADLOCK }
        'Bad font file format',                 { EBFONT }
        'Device not a stream',                  { ENOSTR }
        'No data available',                    { ENODATA }
        'Timer expired',                        { ETIME }
        'Out of streams resources',             { ENOSR }
        'Machine is not on the network',        { ENONET }
        'Package not installed',                { ENOPKG }
        'Object is remote',                     { EREMOTE }
        'Link has been severed',                { ENOLINK }
        'Advertise error',                      { EADV }
        'Srmount error',                        { ESRMNT }
        'Communication error on send',          { ECOMM }
        'Protocol error',                       { EPROTO }
        'Multihop attempted',                   { EMULTIHOP }
        'RFS specific error',                   { EDOTDOT }
        'Not a data message',                   { EBADMSG }
        'Value too large for defined data type',        { EOVERFLOW }
        'Name not unique on network',           { ENOTUNIQ }
        'File descriptor in bad state',         { EBADFD }
        'Remote address changed',               { EREMCHG }
        'Can not access a needed shared library',       { ELIBACC }
        'Accessing a corrupted shared library',         { ELIBBAD }
        '.lib section in a.out corrupted',      { ELIBSCN }
        'Attempting to link in too many shared libraries',      { ELIBMAX }
        'Cannot exec a shared library directly',        { ELIBEXEC }
        'Illegal byte sequence',                { EILSEQ }
        'Interrupted system call should be restarted',  { ERESTART }
        'Streams pipe error',                   { ESTRPIPE }
        'Too many users',                       { EUSERS }
        'Socket operation on non-socket',       { ENOTSOCK }
        'Destination address required',         { EDESTADDRREQ }
        'Message too long',                     { EMSGSIZE }
        'Protocol wrong type for socket',       { EPROTOTYPE }
        'Protocol not available',               { ENOPROTOOPT }
        'Protocol not supported',               { EPROTONOSUPPORT }
        'Socket type not supported',            { ESOCKTNOSUPPORT }
        'Operation not supported on transport endpoint',        { EOPNOTSUPP }
        'Protocol family not supported',        { EPFNOSUPPORT }
        'Address family not supported by protocol',     { EAFNOSUPPORT }
        'Address already in use',               { EADDRINUSE }
        'Cannot assign requested address',      { EADDRNOTAVAIL }
        'Network is down',                      { ENETDOWN }
        'Network is unreachable',               { ENETUNREACH }
        'Network dropped connection because of reset',  { ENETRESET }
        'Software caused connection abort',     { ECONNABORTED }
        'Connection reset by peer',             { ECONNRESET }
        'No buffer space available',            { ENOBUFS }
        'Transport endpoint is already connected',      { EISCONN }
        'Transport endpoint is not connected',  { ENOTCONN }
        'Cannot send after transport endpoint shutdown',        { ESHUTDOWN }
        'Too many references: cannot splice',   { ETOOMANYREFS }
        'Connection timed out',                 { ETIMEDOUT }
        'Connection refused',                   { ECONNREFUSED }
        'Host is down',                         { EHOSTDOWN }
        'No route to host',                     { EHOSTUNREACH }
        'Operation already in progress',        { EALREADY }
        'Operation now in progress',            { EINPROGRESS }
        'Stale NFS file handle',                { ESTALE }
        'Structure needs cleaning',             { EUCLEAN }
        'Not a XENIX named type file',          { ENOTNAM }
        'No XENIX semaphores available',        { ENAVAIL }
        'Is a named type file',                 { EISNAM }
        'Remote I/O error',                     { EREMOTEIO }
        'Quota exceeded',                       { EDQUOT }
        'No medium found',                      { ENOMEDIUM }
        'Wrong medium type');                   { EMEDIUMTYPE }

Function  StrError(err:longint):string;
Procedure PError(const s:string; Errno : longint);

Implementation

Function StrError(err:longint):string;
var s : string[12];
begin
  if (err<0) or (err>=sys_errn) then
   begin
     str(err,s);
     StrError:='Unknown Error ('+s+')';
   end
  else
   StrError:=StrPas(Sys_ErrList[err]);
end;


procedure PError(const s:string; Errno : longint);
begin
  WriteLn(stderr,s,': ',StrError(ErrNo));
end;

end.

{
  $Log: errors.pp,v $
  Revision 1.1.2.1  2000/09/14 13:38:25  marco
   * Moved from Linux dir. now start of generic unix dir, from which the
      really exotic features should be moved to the target specific dirs.

  Revision 1.1.2.1  2000/08/22 19:21:48  michael
  + Implemented syserrormessage. Made dummies for go32v2 and OS/2
  * Changed linux/errors.pp so it uses pchars for storage.

  Revision 1.1  2000/07/13 06:30:53  michael
  + Initial import

  Revision 1.7  2000/05/17 17:11:16  peter
    * fixed string length

  Revision 1.6  2000/02/09 16:59:31  peter
    * truncated log

  Revision 1.5  2000/01/07 16:41:40  daniel
    * copyright 2000

  Revision 1.4  2000/01/07 16:32:26  daniel
    * copyright 2000 added

}
