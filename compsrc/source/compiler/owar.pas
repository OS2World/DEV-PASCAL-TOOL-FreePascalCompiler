{
    $Id: owar.pas,v 1.1.2.3 2001/02/24 03:17:40 carl Exp $
    Copyright (c) 1998-2000 by Peter Vreman

    Contains the stuff for writing .a files directly

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
unit owar;
interface

uses
  cobjects,owbase;

type
  tarhdr=packed record
    name : array[0..15] of char;
    date : array[0..11] of char;
    uid  : array[0..5] of char;
    gid  : array[0..5] of char;
    mode : array[0..7] of char;
    size : array[0..9] of char;
    fmag : array[0..1] of char;
  end;

  parobjectwriter=^tarobjectwriter;
  tarobjectwriter=object(tobjectwriter)
    constructor Init(const Aarfn:string);
    destructor  Done;virtual;
    procedure create(const fn:string);virtual;
    procedure close;virtual;
    procedure writesym(const sym:string);virtual;
    procedure write(var b;len:longint);virtual;
  private
    arfn        : string;
    arhdr       : tarhdr;
    symreloc,
    symstr,
    lfnstr,
    ardata      : PDynamicArray;
    objpos      : longint;
    objfn       : string;
    timestamp   : string[12];
    procedure createarhdr(fn:string;size:longint;const gid,uid,mode:string);
    procedure writear;
  end;


implementation

uses
   verbose,
{$ifdef Delphi}
   dmisc;
{$else Delphi}
   dos;
{$endif Delphi}

const
{$ifdef TP}
  symrelocbufsize = 256;
  symstrbufsize = 256;
  lfnstrbufsize = 256;
  arbufsize  = 256;
  objbufsize = 256;
{$else}
  symrelocbufsize = 4096;
  symstrbufsize = 8192;
  lfnstrbufsize = 4096;
  arbufsize  = 65536;
  objbufsize = 16384;
{$endif}

{*****************************************************************************
                                   Helpers
*****************************************************************************}

const
  C1970=2440588;
  D0=1461;
  D1=146097;
  D2=1721119;
Function Gregorian2Julian(DT:DateTime):LongInt;
Var
  Century,XYear,Month : LongInt;
Begin
  Month:=DT.Month;
  If Month<=2 Then
   Begin
     Dec(DT.Year);
     Inc(Month,12);
   End;
  Dec(Month,3);
  Century:=(longint(DT.Year Div 100)*D1) shr 2;
  XYear:=(longint(DT.Year Mod 100)*D0) shr 2;
  Gregorian2Julian:=((((Month*153)+2) div 5)+DT.Day)+D2+XYear+Century;
End;

function DT2Unix(DT:DateTime):LongInt;
Begin
  DT2Unix:=(Gregorian2Julian(DT)-C1970)*86400+(LongInt(DT.Hour)*3600)+(DT.Min*60)+DT.Sec;
end;


{*****************************************************************************
                                TArObjectWriter
*****************************************************************************}

constructor tarobjectwriter.init(const Aarfn:string);
var
  time  : datetime;
  dummy : word;
begin
  arfn:=Aarfn;
  new(arData,init(arbufsize));
  new(symreloc,init(symrelocbufsize));
  new(symstr,init(symstrbufsize));
  new(lfnstr,init(lfnstrbufsize));
{ create timestamp }
  getdate(time.year,time.month,time.day,dummy);
  gettime(time.hour,time.min,time.sec,dummy);
  Str(DT2Unix(time),timestamp);
end;


destructor tarobjectwriter.done;
begin
  if Errorcount=0 then
   writear;
  dispose(arData,done);
  dispose(symreloc,done);
  dispose(symstr,done);
  dispose(lfnstr,done);
end;


procedure tarobjectwriter.createarhdr(fn:string;size:longint;const gid,uid,mode:string);
var
  tmp : string[9];
begin
  fillchar(arhdr,sizeof(tarhdr),' ');
{ create ar header }
  fn:=fn+'/';
  if length(fn)>16 then
   begin
     arhdr.name[0]:='/';
     str(lfnstr^.size,tmp);
     move(tmp[1],arhdr.name[1],length(tmp));
     fn:=fn+#10;
     lfnstr^.write(fn[1],length(fn));
   end
  else
   move(fn[1],arhdr.name,length(fn));
  { don't write a date if also no gid/uid/mode is specified }
  if gid<>'' then
    move(timestamp[1],arhdr.date,sizeof(timestamp));
  str(size,tmp);
  move(tmp[1],arhdr.size,length(tmp));
  move(gid[1],arhdr.gid,length(gid));
  move(uid[1],arhdr.uid,length(uid));
  move(mode[1],arhdr.mode,length(mode));
  arhdr.fmag:='`'#10;
end;


procedure tarobjectwriter.create(const fn:string);
begin
  objfn:=fn;
  objpos:=ardata^.size;
  ardata^.seek(objpos + sizeof(tarhdr));
end;


procedure tarobjectwriter.close;
begin
  ardata^.align(2);
{ fix the size in the header }
  createarhdr(objfn,ardata^.size-objpos-sizeof(tarhdr),'42','42','644');
{ write the header }
  ardata^.seek(objpos);
  ardata^.write(arhdr,sizeof(tarhdr));
end;


procedure tarobjectwriter.writesym(const sym:string);
var
  c : char;
{$ifdef tp}
 s : string;
{$endif}
begin
  c:=#0;
  symreloc^.write(objpos,4);

{$ifdef tp}
  s := sym;
  symstr^.write(s[1],length(sym));
{$else}
  symstr^.write(sym[1],length(sym));
{$endif}
  symstr^.write(c,1);
end;


procedure tarobjectwriter.write(var b;len:longint);
begin
  ardata^.write(b,len);
end;


procedure tarobjectwriter.writear;

  function lsb2msb(l:longint):longint;
  type
    bytearr=array[0..3] of byte;
  var
    l1 : longint;
  begin
    bytearr(l1)[0]:=bytearr(l)[3];
    bytearr(l1)[1]:=bytearr(l)[2];
    bytearr(l1)[2]:=bytearr(l)[1];
    bytearr(l1)[3]:=bytearr(l)[0];
    lsb2msb:=l1;
  end;

const
  armagic:array[1..8] of char='!<arch>'#10;
type
  plongint=^longint;
var
  arf : file;
  fixup,l,
  relocs,i : longint;
begin
  assign(arf,arfn);
  {$I-}
   rewrite(arf,1);
  {$I+}
  if ioresult<>0 then
    begin
       Message1(exec_e_cant_create_archivefile,arfn);
       exit;
    end;
  blockwrite(arf,armagic,sizeof(armagic));
  { align first, because we need the size for the fixups of the symbol reloc }
  if lfnstr^.size>0 then
   lfnstr^.align(2);
  if symreloc^.size>0 then
   begin
     symstr^.align(2);
     fixup:=12+sizeof(tarhdr)+symreloc^.size+symstr^.size;
     if lfnstr^.size>0 then
      inc(fixup,lfnstr^.size+sizeof(tarhdr));
     relocs:=symreloc^.size div 4;
     { fixup relocs }
     for i:=0to relocs-1 do
      begin
        symreloc^.seek(i*4);
        symreloc^.read(l,4);
        symreloc^.seek(i*4);
        l:=lsb2msb(l+fixup);
        symreloc^.write(l,4);
      end;
     createarhdr('',4+symreloc^.size+symstr^.size,'0','0','0');
     blockwrite(arf,arhdr,sizeof(tarhdr));
     relocs:=lsb2msb(relocs);
     blockwrite(arf,relocs,4);
     symreloc^.blockwrite(arf);
     symstr^.blockwrite(arf);
   end;
  if lfnstr^.size>0 then
   begin
     createarhdr('/',lfnstr^.size,'','','');
     blockwrite(arf,arhdr,sizeof(tarhdr));
     lfnstr^.blockwrite(arf);
   end;
  ardata^.blockwrite(arf);
  system.close(arf);
end;


end.
{
  $Log: owar.pas,v $
  Revision 1.1.2.3  2001/02/24 03:17:40  carl
  Noag386bin define problem solved

  Revision 1.1.2.2  2000/08/19 18:42:27  peter
    * new tdynamicarray implementation using blocks instead of
      reallocmem

  Revision 1.1.2.1  2000/08/08 19:09:35  peter
    * patch from Jonas to not use objdata

  Revision 1.1  2000/07/13 06:29:52  michael
  + Initial import

  Revision 1.7  2000/04/02 15:22:19  florian
    * fixed bug 903: the compiler gives now a nice message if it can't create
      the .o file, (same for future .ar)

  Revision 1.6  2000/02/09 13:22:55  peter
    * log truncated

  Revision 1.5  2000/01/07 01:14:28  peter
    * updated copyright to 2000

}
