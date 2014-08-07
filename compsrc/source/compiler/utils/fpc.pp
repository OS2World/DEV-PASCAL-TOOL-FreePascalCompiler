{
    $Id: fpc.pp,v 1.1.2.6 2003/01/27 19:50:28 carl Exp $
    Copyright (c) 2000 by Florian Klaempfl

    This file is the "loader" for the Free Pascal compiler

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

 ****************************************************************************}
program fpc;

  uses
     dos;

  const
{$ifdef UNIX}
    exeext='';
{$else UNIX}
  {$ifdef AMIGA}
    exeext='';
  {$else}
    exeext='.exe';
  {$endif}    
{$endif UNIX}


  procedure error(const s : string);
    begin
       writeln('Error: ',s);
       halt(1);
    end;


  function SplitPath(Const HStr:ShortString):ShortString;
    var
      i : longint;
    begin
      i:=Length(Hstr);
      while (i>0) and not(Hstr[i] in ['\','/']) do
       dec(i);
      SplitPath:=Copy(Hstr,1,i);
    end;


  function FileExists ( Const F : ShortString) : Boolean;
    var
      Info : SearchRec;
    begin
      findfirst(F,readonly+archive+hidden,info);
      FileExists:=(doserror=0);
      findclose(Info);
    end;


  procedure findexe(var ppcbin:string);
    var
      path : string;
    begin
      { add .exe extension }
      ppcbin:=ppcbin+exeext;

      { get path of fpc.exe }
      path:=splitpath(paramstr(0));
      if FileExists(path+ppcbin) then
       ppcbin:=path+ppcbin
      else
       begin
         path:=FSearch(ppcbin,getenv('PATH'));
         if path<>'' then
          ppcbin:=path;
       end;
    end;


  var
     s,
     processorname,
     ppcbin,
     processorstr   : shortstring;
     ppccommandline : ansistring;
     i : longint;
  begin
     ppccommandline:='';
{$ifdef i386}
     ppcbin:='ppc386';
     processorname:='i386';
{$endif i386}
{$ifdef m68k}
     ppcbin:='ppc68k';
     processorname:='m68k';
{$endif m68k}
{$ifdef alpha}
     ppcbin:='ppcapx';
     processorname:='alpha';
{$endif alpha}
{$ifdef powerpc}
     ppcbin:='ppcppc';
     processorname:='powerpc';
{$endif powerpc}
     for i:=1 to paramcount do
       begin
          s:=paramstr(i);
          if pos('-P',s)=1 then
            begin
               processorstr:=copy(s,3,length(s)-2);
               { -PB is a special code that will show the
                 default compiler and exit immediatly. It's
                 main usage is for Makefile }
               if processorstr='B' then
                begin
                  { report the full name of the ppcbin }
                  findexe(ppcbin);
                  writeln(ppcbin);
                  halt(0);
                end
               { -PP is a special code that will show the
                 processor and exit immediatly. It's
                 main usage is for Makefile }
               else if processorstr='P' then
                begin
                  { report the processor }
                  writeln(processorname);
                  halt(0);
                end
               else if processorstr='i386' then
                 ppcbin:='ppc386'
               else if processorstr='m68k' then
                 ppcbin:='ppc68k'
               else if processorstr='alpha' then
                 ppcbin:='ppcapx'
               else if processorstr='powerpc' then
                 ppcbin:='ppcppc'
               else error('Illegal processor type "'+processorstr+'"');
            end
          else
            ppccommandline:=ppccommandline+s+' ';
       end;

     { find the full path to the specified exe }
     findexe(ppcbin);

     { call ppcXXX }
     swapvectors;
     exec(ppcbin,ppccommandline);
     swapvectors;
     if doserror<>0 then
       error(ppcbin+' can''t be executed');
     halt(dosexitcode);
  end.
{
  $Log: fpc.pp,v $
  Revision 1.1.2.6  2003/01/27 19:50:28  carl
    * More amiga fixes

  Revision 1.1.2.5  2002/10/28 23:26:24  pierre
   * remove last wrong commit

  Revision 1.1.2.3  2001/10/16 20:52:28  peter
    * fpc -PB instead of -P?
    * check if fpc exists if not found fallback to ppc386

  Revision 1.2  2001/09/22 11:11:43  peter
    * "fpc -P?" command to query for used ppcXXX compiler

  Revision 1.1.2.1  2001/04/25 22:43:24  peter
    * compiler dependent utils in utils/ subdir

  Revision 1.1.2.2  2000/12/12 19:47:40  peter
    * fixed for go32v2 and win32

  Revision 1.1.2.1  2000/11/18 14:16:12  peter
    * really working now

  Revision 1.1  2000/07/13 06:29:50  michael
  + Initial import

  Revision 1.1  2000/07/07 17:07:20  florian
    + initial revision

}
