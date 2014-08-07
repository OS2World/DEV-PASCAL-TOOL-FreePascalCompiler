{
    $Id: solaris.inc,v 1.1.2.1 2001/12/09 03:25:17 carl Exp $
    Copyright (c) 2001 by Carl Eric Codere


    Implements Solaris system calls and types


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
const
  _FSTYPSZ = 16;


type
  fsblkcnt_t = cardinal;
  fsfilcnt_t = cardinal;
  
  statvfs_t = packed record
    f_bsize : cardinal;                {* fundamental file system block size *}
    f_frsize : cardinal;               {* fragment size *}
    f_blocks : fsblkcnt_t;             {* total blocks of f_frsize on fs *} 
    f_bfree : fsblkcnt_t;              {* total free blocks of f_frsize *}   
    f_bavail : fsblkcnt_t;	       {* free blocks avail to non-superuser *}
    f_files : fsfilcnt_t;              {* total file nodes (inodes) *}
    f_free : fsfilcnt_t;               {* total free file nodes *}
    f_favail : fsfilcnt_t;             {* free nodes avail to non-superuser *}
    f_fsid :  cardinal;                {* file system id (dev for now) *}
    f_basetype : array[0.._FSTYPSZ-1] of char; {* target fs type name null terminated *}
    f_flag : cardinal;                 {* bit-mask of flags *} 
    f_namemax : cardinal;              {* maximum file name length *}
    f_fstr : array[0..31] of char;     {* filesystem-specific string *}
    f_filler : array[1..16] of cardinal; {* reserved for future expansion *} 
  end;  
  
  
function sys_statvfs(const path: pchar; var buf : statvfs_t): cint; cdecl; external name 'statvfs';  


  {**********************************************************************}
  {                      Memory Management routines                      }
  {**********************************************************************}
  { mmap cannot be used with anonymous on Solaris since it started from Solaris 8 }
  



{ 
 $Log: solaris.inc,v $
 Revision 1.1.2.1  2001/12/09 03:25:17  carl
 + reinstated

 Revision 1.1.2.1  2001/12/04 02:47:50  carl
 + Solaris specific types and syscalls

} 