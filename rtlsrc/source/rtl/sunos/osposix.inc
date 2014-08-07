{
    $Id: osposix.inc,v 1.1.2.6 2001/12/20 03:04:07 carl Exp $
    Copyright (c) 2001 by Carl Eric Codere

    Implements POSIX 1003.1 conforming interface

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}

  {$Linklib c}




    function int_fork : pid_t; cdecl; external name 'fork';
    function int_execve(const path : pchar; const argv : ppchar; const envp: ppchar): cint; cdecl; external name 'execve';
    function int_waitpid(pid : pid_t; var stat_loc : cint; options: cint): pid_t; cdecl; external name 'waitpid';
    procedure sys_exit(status : cint); cdecl; external name '_exit';
    function int_uname(var name: utsname): cint; cdecl; external name '_nuname';
    function int_opendir(const dirname : pchar): pdir; cdecl; external name 'opendir';
    function int_readdir(dirp : pdir) : pdirent;cdecl; external name 'readdir';
    function int_closedir(dirp : pdir): cint; cdecl; external name 'closedir';
    function int_chdir(const path : pchar): cint; cdecl; external name 'chdir';
    function int_open(const path: pchar; flags : cint; mode: mode_t):cint; cdecl; external name 'open';
    function int_mkdir(const path : pchar; mode: mode_t):cint; cdecl; external name 'mkdir';
    function int_unlink(const path: pchar): cint; cdecl; external name 'unlink';
    function int_rmdir(const path : pchar): cint; cdecl; external name 'rmdir';
    function int_rename(const old : pchar; const newpath: pchar): cint; cdecl;external name 'rename';
    function int_access(const pathname : pchar; amode : cint): cint; cdecl; external name 'access';
    function int_close(fd : cint): cint; cdecl; external name 'close';
    function int_read(fd: cint; buf: pchar; nbytes : size_t): ssize_t; cdecl; external name 'read';
    function int_write(fd: cint;const buf:pchar; nbytes : size_t): ssize_t; cdecl; external name 'write';
    function int_lseek(fd : cint; offset : off_t; whence : cint): off_t; cdecl; external name 'lseek';
    function int_time(var tloc:time_t): time_t; cdecl; external name 'time';
    function int_ftruncate(fd : cint; flength : off_t): cint; cdecl; external name 'ftruncate';
    function int_sigaction(sig: cint; var act : sigactionrec; var oact : sigactionrec): cint; cdecl; external name 'sigaction';


    function sys_fork : pid_t; 
     begin
       sys_fork := int_fork;
       if sys_fork <> - 1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
     end;
     
     
    function sys_execve(const path : pchar; const argv : ppchar; const envp: ppchar): cint;
    begin
       sys_execve := int_execve(path, argv, envp);
       if sys_execve <> - 1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    
    function sys_waitpid(pid : pid_t; var stat_loc : cint; options: cint): pid_t;
    begin
       sys_waitpid := int_waitpid(pid, stat_loc, options);
       if sys_waitpid <> - 1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;


    function sys_uname(var name: utsname): cint; 
     begin
       sys_uname := int_uname(name);
       if sys_uname <> - 1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
     end;
     
    function sys_opendir(const dirname : pchar): pdir; 
    begin
       sys_opendir := int_opendir(dirname);
       if sys_opendir <> nil then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    
    
    function sys_readdir(dirp : pdir) : pdirent;
    begin
       sys_readdir := int_readdir(dirp);
       if sys_readdir <> nil then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    

    function sys_closedir(dirp : pdir): cint; 
    begin
       sys_closedir := int_closedir(dirp);
       if sys_closedir <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    
    function sys_chdir(const path : pchar): cint; 
    begin
       sys_chdir := int_chdir(path);
       if sys_chdir <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    
    
    function sys_open(const path: pchar; flags : cint; mode: mode_t):cint; 
    begin
       sys_open:= int_open(path, flags, mode);
       if sys_open <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    
    
    function sys_mkdir(const path : pchar; mode: mode_t):cint; 
    begin
       sys_mkdir:= int_mkdir(path, mode);
       if sys_mkdir <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    
    function sys_unlink(const path: pchar): cint; 
    begin
       sys_unlink := int_unlink(path);
       if sys_unlink <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    
    
    function sys_rmdir(const path : pchar): cint; 
    begin
       sys_rmdir := int_rmdir(path);
       if sys_rmdir <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    
    function sys_rename(const old : pchar; const newpath: pchar): cint; 
    begin
       sys_rename := int_rename(old, newpath);
       if sys_rename <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    
    function sys_access(const pathname : pchar; amode : cint): cint; 
    begin
       sys_access := int_access(pathname, amode);
       if sys_access <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    
    
    function sys_close(fd : cint): cint; 
    begin
       sys_close := int_close(fd);
       if sys_close <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    
    function sys_read(fd: cint; buf: pchar; nbytes : size_t): ssize_t; 
    begin
       sys_read := int_read(fd, buf, nbytes);
       if sys_read <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    
    
    function sys_write(fd: cint;const buf:pchar; nbytes : size_t): ssize_t; 
    begin
       sys_write := int_write(fd, buf, nbytes);
       if sys_write <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    
    
    function sys_lseek(fd : cint; offset : off_t; whence : cint): off_t; 
    begin
       sys_lseek := int_lseek(fd, offset, whence);
       if sys_lseek <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    
    function sys_time(var tloc:time_t): time_t;
    begin
      sys_time := int_time(tloc);
      if sys_time <> -1 then
        begin
          errno := 0;         { reset errno when the call succeeds, contrary to libc }
        end;
    end;
    
    function sys_ftruncate(fd : cint; flength : off_t): cint;
    begin
       sys_ftruncate := int_ftruncate(fd, flength);
       if sys_ftruncate <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    
    function sys_sigaction(sig: cint; var act : sigactionrec; var oact : sigactionrec): cint; 
    begin
       sys_sigaction := int_sigaction(sig, act, oact);
       if sys_sigaction <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;

    { internal routines used SVR4.0                    }
    { therefore these calls will not work on SVR3.0    }
    { operating systems.                               }
    const 
       _STAT_VER = 2;        { stat version }
       
    function _xstat(const version : cint;const path: pchar; var buf : stat): cint; cdecl; external name '_xstat';
    function _fxstat(const version : cint; fd : cint; var sb : stat): cint; cdecl; external name '_fxstat';

   
    function sys_fstat(fd : cint; var sb : stat): cint; 
      begin
        sys_fstat := _fxstat(_STAT_VER, fd, sb);
        if sys_fstat <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
      end;
      
    function sys_stat(const path: pchar; var buf : stat): cint; 
      begin
        sys_stat := _xstat(_STAT_VER, path, buf);
        if sys_stat <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
      end;

    function S_ISDIR(m : mode_t): boolean;
      begin
        if (m and $F000) = $4000 then
          S_ISDIR := true
        else
          S_ISDIR := false;
      end;

    function S_ISCHR(m : mode_t): boolean;
      begin
        if (m and $F000) = $2000 then
          S_ISCHR := true
        else
          S_ISCHR := false;
      end;

    function S_ISBLK(m : mode_t): boolean;
      begin
        if (m and $F000) = $6000 then
          S_ISBLK := true
        else
          S_ISBLK := false;
      end;

    function S_ISREG(m : mode_t): boolean;
      begin
        if (m and $F000) = $8000 then
          S_ISREG := true
        else
          S_ISREG := false;
      end;

    function S_ISFIFO(m : mode_t): boolean;
      begin
        if (m and $F000) = $1000 then
          S_ISFIFO := true
        else
          S_ISFIFO := false;
      end;

    function wifexited(status : cint): cint;
      begin
          wifexited := longint((status and $FF) = 0);
      end;

    function wexitstatus(status : cint): cint;
     begin
       wexitstatus := (((status) shr 8) and $FF);
     end;

    function wstopsig(status : cint): cint;
     begin
       wstopsig := (((status) shr 8) and $FF);
     end;

    function wifsignaled(status : cint): cint;
     begin
       if ((status and $FF) > 0) and ((status and $FF00)=0) then
         wifsignaled := 1
       else  
         wifsignaled := 0;
     end;


{

 $Log: osposix.inc,v $
 Revision 1.1.2.6  2001/12/20 03:04:07  carl
 * S_ISFIFO evaaluation was wrong

 Revision 1.1.2.5  2001/12/09 03:25:17  carl
 + reinstated

 Revision 1.1.2.2  2001/12/08 03:37:32  carl
 * errno must be reset to zero after the call

 Revision 1.1.2.1  2001/12/04 00:45:56  carl
 + initial import

}