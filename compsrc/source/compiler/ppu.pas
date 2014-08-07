{
    $Id: ppu.pas,v 1.1.2.8 2002/11/08 13:58:58 pierre Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    Routines to read/write ppu files

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
{$ifdef TP}
  {$N+,E+}
{$endif}
unit ppu;


interface

{ Also write the ppu if only crc if done, this can be used with ppudump to
  see the differences between the intf and implementation }
{ define INTFPPU}

{$ifdef Test_Double_checksum}
var
  CRCFile : text;
const
  CRC_array_Size = 200000;
type
  tcrc_array = array[0..crc_array_size] of longint;
  pcrc_array = ^tcrc_array;
{$endif Test_Double_checksum}

const
  CurrentPPUVersion=21;

{ buffer sizes }
  maxentrysize = 1024;
{$ifdef TP}
  ppubufsize   = 1024;
{$else}
  ppubufsize   = 16384;
{$endif}

{ppu entries}
  mainentryid         = 1;
  subentryid          = 2;
  {special}
  iberror             = 0;
  ibstartdefs         = 248;
  ibenddefs           = 249;
  ibstartsyms         = 250;
  ibendsyms           = 251;
  ibendinterface      = 252;
  ibendimplementation = 253;
  ibendbrowser        = 254;
  ibend               = 255;
  {general}
  ibmodulename           = 1;
  ibsourcefiles          = 2;
  ibloadunit             = 3;
  ibinitunit             = 4;
  iblinkunitofiles       = 5;
  iblinkunitstaticlibs   = 6;
  iblinkunitsharedlibs   = 7;
  iblinkotherofiles      = 8;
  iblinkotherstaticlibs  = 9;
  iblinkothersharedlibs  = 10;
  ibdbxcount             = 11;
  ibsymref               = 12;
  ibdefref               = 13;
  ibendsymtablebrowser   = 14;
  ibbeginsymtablebrowser = 15;
  ibusedmacros           = 16;
  {syms}
  ibtypesym       = 20;
  ibprocsym       = 21;
  ibvarsym        = 22;
  ibconstsym      = 23;
  ibenumsym       = 24;
  ibtypedconstsym = 25;
  ibabsolutesym   = 26;
  ibpropertysym   = 27;
  ibvarsym_C      = 28;
  ibunitsym       = 29;  { needed for browser }
  iblabelsym      = 30;
  ibfuncretsym    = 31;
  ibsyssym        = 32;
  {definitions}
  iborddef         = 40;
  ibpointerdef     = 41;
  ibarraydef       = 42;
  ibprocdef        = 43;
  ibshortstringdef = 44;
  ibrecorddef      = 45;
  ibfiledef        = 46;
  ibformaldef      = 47;
  ibobjectdef      = 48;
  ibenumdef        = 49;
  ibsetdef         = 50;
  ibprocvardef     = 51;
  ibfloatdef       = 52;
  ibclassrefdef    = 53;
  iblongstringdef  = 54;
  ibansistringdef  = 55;
  ibwidestringdef  = 56;
  ibvariantdef     = 57;

{ unit flags }
  uf_init          = $1;
  uf_finalize      = $2;
  uf_big_endian    = $4;
  uf_has_dbx       = $8;
  uf_has_browser   = $10;
  uf_in_library    = $20;  { is the file in another file than <ppufile>.* ? }
  uf_smart_linked  = $40;  { the ppu can be smartlinked }
  uf_static_linked = $80;  { the ppu can be linked static }
  uf_shared_linked = $100; { the ppu can be linked shared }
  uf_local_browser = $200;
  uf_no_link       = $400; { unit has no .o generated, but can still have
                             external linking! }
  uf_has_resources = $800; { unit has resource section }
  uf_little_endian = $1000;
  uf_release       = $2000;{ unit was compiled with -Sr option }

type
  ppureal=extended;

  tppuerror=(ppuentrytoobig,ppuentryerror);

  tppuheader=packed record { 36 bytes }
    id       : array[1..3] of char; { = 'PPU' }
    ver      : array[1..3] of char;
    compiler : word;
    cpu      : word;
    target   : word;
    flags    : longint;
    size     : longint; { size of the ppufile without header }
    checksum : longint; { checksum for this ppufile }
    interface_checksum : longint;
    future   : array[0..2] of longint;
  end;

  tppuentry=packed record
    id   : byte;
    nr   : byte;
    size : longint;
  end;

   pppufile=^tppufile;
   tppufile=object
     f        : file;
     mode     : byte; {0 - Closed, 1 - Reading, 2 - Writing}
     fname    : string;
     fsize    : longint;
{$ifdef Test_Double_checksum}
     crcindex,
     crc_index,
     crcindex2,
     crc_index2 : longint;
     crc_test,
     crc_test2  : pcrc_array;
{$endif def Test_Double_checksum}
     change_endian : boolean;
     buf      : pchar;
     bufstart,
     bufsize,
     bufidx   : longint;
     entrybufstart,
     entrystart,
     entryidx : longint;
     entry    : tppuentry;
     closed,
     tempclosed : boolean;
     closepos : longint;
   public
     entrytyp : byte;
     header           : tppuheader;
     size             : longint;
     crc,
     interface_crc    : longint;
     error,
     do_crc,
     do_interface_crc : boolean;
     crc_only         : boolean;    { used to calculate interface_crc before implementation }
     constructor Init(const fn:string);
     destructor  Done;
     procedure flush;
     procedure closefile;
     function  CheckPPUId:boolean;
     function  GetPPUVersion:longint;
     procedure NewHeader;
     procedure NewEntry;
   {read}
     function  openfile:boolean;
     procedure reloadbuf;
     procedure readdata(var b;len:longint);
     procedure skipdata(len:longint);
     function  readentry:byte;
     function  EndOfEntry:boolean;
     procedure getdatabuf(var b;len:longint;var res:longint);
     procedure getdata(var b;len:longint);
     function  getbyte:byte;
     function  getword:word;
     function  getlongint:longint;
     function  getreal:ppureal;
     function  getstring:string;
     procedure getnormalset(var b);
     procedure getsmallset(var b);
     function  skipuntilentry(untilb:byte):boolean;
  {write}
     function  createfile:boolean;
     procedure writeheader;
     procedure writebuf;
     procedure writedata(const b;len:longint);
     procedure writeentry(ibnr:byte);
     procedure putdata(var b;len:longint);
     procedure putbyte(b:byte);
     procedure putword(w:word);
     procedure putlongint(l:longint);
     procedure putreal(d:ppureal);
     procedure putstring(s:string);
     procedure putnormalset(var b);
     procedure putsmallset(var b);
     procedure tempclose;
     function  tempopen:boolean;
  end;

implementation

  uses
{$ifdef Test_Double_checksum}
    comphook,
{$endif def Test_Double_checksum}
    crc;

{*****************************************************************************
                             Endian Handling
*****************************************************************************}

Function SwapLong(x : longint): longint;
var
  y : word;
  z : word;
Begin
  y := (x shr 16) and $FFFF;
  y := ((y shl 8) and $FFFF) or ((y shr 8) and $ff);
  z := x and $FFFF;
  z := ((z shl 8) and $FFFF) or ((z shr 8) and $ff);
  SwapLong := (longint(z) shl 16) or longint(y);
End;


Function SwapWord(x : word): word;
var
  z : byte;
Begin
  z := (x shr 8) and $ff;
  x := x and $ff;
  x := (x shl 8);
  SwapWord := x or z;
End;


{*****************************************************************************
                                  TPPUFile
*****************************************************************************}

constructor tppufile.Init(const fn:string);
begin
  fname:=fn;
  change_endian:=false;
  crc_only:=false;
  Mode:=0;
  NewHeader;
  Error:=false;
  closed:=true;
  tempclosed:=false;
  getmem(buf,ppubufsize);
end;


destructor tppufile.Done;
begin
  closefile;
  if assigned(buf) then
    freemem(buf,ppubufsize);
end;


procedure tppufile.flush;
begin
  if Mode=2 then
   writebuf;
end;


procedure tppufile.closefile;
begin
{$ifdef Test_Double_checksum}
  if mode=2 then
   begin
     if assigned(crc_test) then
      dispose(crc_test);
     if assigned(crc_test2) then
      dispose(crc_test2);
   end;
{$endif Test_Double_checksum}
  if Mode<>0 then
   begin
     Flush;
     {$I-}
      system.close(f);
     {$I+}
     if ioresult<>0 then;
     Mode:=0;
     closed:=true;
   end;
end;


function tppufile.CheckPPUId:boolean;
begin
  CheckPPUId:=((Header.Id[1]='P') and (Header.Id[2]='P') and (Header.Id[3]='U'));
end;


function tppufile.GetPPUVersion:longint;
var
  l    : longint;
  code : integer;
begin
  Val(header.ver[1]+header.ver[2]+header.ver[3],l,code);
  if code=0 then
   GetPPUVersion:=l
  else
   GetPPUVersion:=0;
end;


procedure tppufile.NewHeader;
var
  s : string;
begin
  fillchar(header,sizeof(tppuheader),0);
  str(currentppuversion,s);
  while length(s)<3 do
   s:='0'+s;
  with header do
   begin
     Id[1]:='P';
     Id[2]:='P';
     Id[3]:='U';
     Ver[1]:=s[1];
     Ver[2]:=s[2];
     Ver[3]:=s[3];
   end;
end;


{*****************************************************************************
                                TPPUFile Reading
*****************************************************************************}

function tppufile.openfile:boolean;
var
  ofmode : byte;
  i      : word;
begin
  openfile:=false;
  assign(f,fname);
  ofmode:=filemode;
  filemode:=$0;
  {$I-}
   reset(f,1);
  {$I+}
  filemode:=ofmode;
  if ioresult<>0 then
   exit;
  closed:=false;
{read ppuheader}
  fsize:=filesize(f);
  if fsize<sizeof(tppuheader) then
   exit;
  blockread(f,header,sizeof(tppuheader),i);
  { The header is always stored in little endian order }
  { therefore swap if on a big endian machine          }
{$IFDEF ENDIAN_BIG}
  header.compiler := SwapWord(header.compiler);
  header.cpu := SwapWord(header.cpu);
  header.target := SwapWord(header.target);
  header.flags := SwapLong(header.flags);
  header.size := SwapLong(header.size);
  header.checksum := SwapLong(header.checksum);
  header.interface_checksum := SwapLong(header.interface_checksum);
{$ENDIF}
  { the PPU DATA is stored in native order }
  if (header.flags and uf_big_endian) = uf_big_endian then
   Begin
{$ifdef ENDIAN_BIG}
      change_endian := FALSE;
{$ELSE}
      change_endian := TRUE;
{$ENDIF}
   End
  else if (header.flags and uf_little_endian) = uf_little_endian then
   Begin
{$ifdef ENDIAN_BIG}
      change_endian := TRUE;
{$else}
      change_endian := FALSE;
{$endif}
   End;
  {reset buffer}
  bufstart:=i;
  bufsize:=0;
  bufidx:=0;
  Mode:=1;
  FillChar(entry,sizeof(tppuentry),0);
  entryidx:=0;
  entrystart:=0;
  entrybufstart:=0;
  Error:=false;
  openfile:=true;
end;


procedure tppufile.reloadbuf;
{$ifdef tp}
var
 i: word;
 {$endif}
begin
  inc(bufstart,bufsize);
{$ifdef tp}
  blockread(f,buf^,ppubufsize,i);
  bufsize := i;
{$else}
  blockread(f,buf^,ppubufsize,bufsize);
{$endif}
  bufidx:=0;
end;


procedure tppufile.readdata(var b;len:longint);
var
  p   : pchar;
  left,
  idx : longint;
begin
  p:=pchar(@b);
  idx:=0;
  while len>0 do
   begin
     left:=bufsize-bufidx;
     if len>left then
      begin
        move(buf[bufidx],p[idx],left);
        dec(len,left);
        inc(idx,left);
        reloadbuf;
        if bufsize=0 then
         exit;
      end
     else
      begin
        move(buf[bufidx],p[idx],len);
        inc(bufidx,len);
        exit;
      end;
   end;
end;


procedure tppufile.skipdata(len:longint);
var
  left : longint;
begin
  while len>0 do
   begin
     left:=bufsize-bufidx;
     if len>left then
      begin
        dec(len,left);
        reloadbuf;
        if bufsize=0 then
         exit;
      end
     else
      begin
        inc(bufidx,len);
        exit;
      end;
   end;
end;


function tppufile.readentry:byte;
begin
  if entryidx<entry.size then
   skipdata(entry.size-entryidx);
  readdata(entry,sizeof(tppuentry));
  if change_endian then
     entry.size := swaplong(entry.size);
  entrystart:=bufstart+bufidx;
  entryidx:=0;
  if not(entry.id in [mainentryid,subentryid]) then
   begin
     readentry:=iberror;
     error:=true;
     exit;
   end;
  readentry:=entry.nr;
end;


function tppufile.endofentry:boolean;
begin
  endofentry:=(entryidx>=entry.size);
end;


procedure tppufile.getdatabuf(var b;len:longint;var res:longint);
begin
  if entryidx+len>entry.size then
   res:=entry.size-entryidx
  else
   res:=len;
  readdata(b,res);
  inc(entryidx,res);
end;


procedure tppufile.getdata(var b;len:longint);
begin
  if entryidx+len>entry.size then
   begin
     error:=true;
     exit;
   end;
  readdata(b,len);
  inc(entryidx,len);
end;


function tppufile.getbyte:byte;
var
  b : byte;
begin
  if entryidx+1>entry.size then
   begin
     error:=true;
     getbyte:=0;
     exit;
   end;
  readdata(b,1);
  getbyte:=b;
  inc(entryidx);
end;


function tppufile.getword:word;
type
  pword = ^word;
var
  w : word;
begin
  if entryidx+2>entry.size then
   begin
     error:=true;
     getword:=0;
     exit;
   end;
  readdata(w,2);
  if change_endian then
   getword:=swapword(w)
  else
   getword:=w;
  inc(entryidx,2);
end;


function tppufile.getlongint:longint;
type
  plongint = ^longint;
var
  l : longint;
begin
  if entryidx+4>entry.size then
   begin
     error:=true;
     getlongint:=0;
     exit;
   end;
  readdata(l,4);
  if change_endian then
   getlongint:=swaplong(l)
  else
   getlongint:=l;
  inc(entryidx,4);
end;


function tppufile.getreal:ppureal;
type
  pppureal = ^ppureal;
var
  d : ppureal;
begin
  if entryidx+sizeof(ppureal)>entry.size then
   begin
     error:=true;
     getreal:=0;
     exit;
   end;
  readdata(d,sizeof(ppureal));
  getreal:=d;
  inc(entryidx,sizeof(ppureal));
end;


function tppufile.getstring:string;
var
  s : string;
begin
  s[0]:=chr(getbyte);
  if entryidx+length(s)>entry.size then
   begin
     error:=true;
     exit;
   end;
  ReadData(s[1],length(s));
  getstring:=s;
  inc(entryidx,length(s));
end;


procedure tppufile.getsmallset(var b);
var
  l : longint;
begin
  l:=getlongint;
  longint(b):=l;
end;


procedure tppufile.getnormalset(var b);
type
  SetLongintArray = Array [0..7] of longint;
var
  i : longint;
begin
  if change_endian then
    begin
      for i:=0 to 7 do
        SetLongintArray(b)[i]:=getlongint;
    end
  else
    getdata(b,32);
end;


function tppufile.skipuntilentry(untilb:byte):boolean;
var
  b : byte;
begin
  repeat
    b:=readentry;
  until (b in [ibend,iberror]) or ((b=untilb) and (entry.id=mainentryid));
  skipuntilentry:=(b=untilb);
end;


{*****************************************************************************
                                TPPUFile Writing
*****************************************************************************}

function tppufile.createfile:boolean;
begin
  createfile:=false;
{$ifdef INTFPPU}
  if crc_only then
   begin
     fname:=fname+'.intf';
     crc_only:=false;
   end;
{$endif}
  if not crc_only then
    begin
      assign(f,fname);
      {$I-}
       rewrite(f,1);
      {$I+}
      if ioresult<>0 then
       exit;
      Mode:=2;
    {write header for sure}
      blockwrite(f,header,sizeof(tppuheader));
    end;
  bufsize:=ppubufsize;
  bufstart:=sizeof(tppuheader);
  bufidx:=0;
{reset}
  crc:=$ffffffff;
  interface_crc:=$ffffffff;
  do_interface_crc:=true;
  Error:=false;
  do_crc:=true;
  size:=0;
  entrytyp:=mainentryid;
{start}
{endian}
  NewEntry;
  createfile:=true;
end;


procedure tppufile.writeheader;
var
  opos : longint;
begin
  { flush buffer }
  writebuf;
  { update size (w/o header!) in the header }
  header.size:=bufstart-sizeof(tppuheader);
{$ifdef ENDIAN_BIG}
  header.compiler := SwapWord(header.compiler);
  header.cpu := SwapWord(header.cpu);
  header.target := SwapWord(header.target);
  header.flags := SwapLong(header.flags);
  header.size := SwapLong(header.size);
  header.checksum := SwapLong(header.checksum);
  header.interface_checksum := SwapLong(header.interface_checksum);
{$endif}
  { write header and restore filepos after it }
  opos:=filepos(f);
  seek(f,0);
  blockwrite(f,header,sizeof(tppuheader));
  seek(f,opos);
end;


procedure tppufile.writebuf;
begin
  if not crc_only then
    blockwrite(f,buf^,bufidx);
  inc(bufstart,bufidx);
  bufidx:=0;
end;


procedure tppufile.writedata(const b;len:longint);
var
  p   : pchar;
  left,
  idx : longint;
begin
  if crc_only then
    exit;
  p:=pchar(@b);
  idx:=0;
  while len>0 do
   begin
     left:=bufsize-bufidx;
     if len>left then
      begin
        move(p[idx],buf[bufidx],left);
        dec(len,left);
        inc(idx,left);
        inc(bufidx,left);
        writebuf;
      end
     else
      begin
        move(p[idx],buf[bufidx],len);
        inc(bufidx,len);
        exit;
      end;
   end;
end;


procedure tppufile.NewEntry;
begin
  with entry do
   begin
     id:=entrytyp;
     nr:=ibend;
     size:=0;
   end;
{Reset Entry State}
  entryidx:=0;
  entrybufstart:=bufstart;
  entrystart:=bufstart+bufidx;
{Alloc in buffer}
  writedata(entry,sizeof(tppuentry));
end;


procedure tppufile.writeentry(ibnr:byte);
var
  opos : longint;
begin
{create entry}
  entry.id:=entrytyp;
  entry.nr:=ibnr;
  entry.size:=entryidx;
  if change_endian then
    entry.size := SwapLong(entry.size);
{it's already been sent to disk ?}
  if entrybufstart<>bufstart then
   begin
    if not crc_only then
      begin
      {flush to be sure}
        WriteBuf;
      {write entry}
        opos:=filepos(f);
        seek(f,entrystart);
        blockwrite(f,entry,sizeof(tppuentry));
        seek(f,opos);
      end;
     entrybufstart:=bufstart;
   end
  else
   move(entry,buf[entrystart-bufstart],sizeof(entry));
{Add New Entry, which is ibend by default}
  entrystart:=bufstart+bufidx; {next entry position}
  NewEntry;
end;


procedure tppufile.putdata(var b;len:longint);
begin
  if do_crc then
   begin
     crc:=UpdateCrc32(crc,b,len);
{$ifdef Test_Double_checksum}
     if crc_only then
       begin
         crc_test2^[crc_index2]:=crc;
{$ifdef Test_Double_checksum_write}
         Writeln(CRCFile,crc);
{$endif Test_Double_checksum_write}
         if crc_index2<crc_array_size then
          inc(crc_index2);
       end
     else
       begin
         if (crcindex2<crc_array_size) and (crcindex2<crc_index2) and
            (crc_test2^[crcindex2]<>crc) then
           Do_comment(V_Note,'impl CRC changed');
{$ifdef Test_Double_checksum_write}
         Writeln(CRCFile,crc);
{$endif Test_Double_checksum_write}
         inc(crcindex2);
       end;
{$endif def Test_Double_checksum}
     if do_interface_crc then
       begin
         interface_crc:=UpdateCrc32(interface_crc,b,len);
{$ifdef Test_Double_checksum}
        if crc_only then
          begin
            crc_test^[crc_index]:=interface_crc;
{$ifdef Test_Double_checksum_write}
            Writeln(CRCFile,interface_crc);
{$endif Test_Double_checksum_write}
            if crc_index<crc_array_size then
             inc(crc_index);
          end
        else
          begin
            if (crcindex<crc_array_size) and (crcindex<crc_index) and
               (crc_test^[crcindex]<>interface_crc) then
              Do_comment(V_Warning,'CRC changed');
{$ifdef Test_Double_checksum_write}
            Writeln(CRCFile,interface_crc);
{$endif Test_Double_checksum_write}
            inc(crcindex);
          end;
{$endif def Test_Double_checksum}
       end;
    end;
  if not crc_only then
    writedata(b,len);
  inc(entryidx,len);
end;


procedure tppufile.putbyte(b:byte);
begin
  putdata(b,1);
end;


procedure tppufile.putword(w:word);
begin
  if change_endian then
    w:=swapword(w);
  putdata(w,2);
end;


procedure tppufile.putlongint(l:longint);
begin
  if change_endian then
    l:=swaplong(l);
  putdata(l,4);
end;


procedure tppufile.putreal(d:ppureal);
begin
  putdata(d,sizeof(ppureal));
end;


procedure tppufile.putstring(s:string);
begin
  putdata(s,length(s)+1);
end;


{ Note: this relies on the fact that a smallset is 4 byte long PM }
procedure tppufile.putsmallset(var b);
var
  l : longint;
begin
  l:=longint(b);
  putlongint(l);
end;


procedure tppufile.putnormalset(var b);
type
  SetLongintArray = Array [0..7] of longint;
var
  i : longint;
begin
  if change_endian then
    begin
      for i:=0 to 7 do
        SetLongintArray(b)[i]:=SwapLong(SetLongintArray(b)[i]);
    end;
  putdata(b,32);
  { don't forget to restore the correct value }
  if change_endian then
    begin
      for i:=0 to 7 do
        SetLongintArray(b)[i]:=SwapLong(SetLongintArray(b)[i]);
    end;
end;


    procedure tppufile.tempclose;
      begin
        if not closed then
         begin
           closepos:=filepos(f);
           {$I-}
            system.close(f);
           {$I+}
           if ioresult<>0 then;
           closed:=true;
           tempclosed:=true;
         end;
      end;


    function tppufile.tempopen:boolean;
      var
        ofm : byte;
      begin
        tempopen:=false;
        if not closed or not tempclosed then
         exit;
        ofm:=filemode;
        filemode:=0;
        {$I-}
         reset(f,1);
        {$I+}
        filemode:=ofm;
        if ioresult<>0 then
         exit;
        closed:=false;
        tempclosed:=false;

      { restore state }
        seek(f,closepos);
        tempopen:=true;
      end;

end.
{
  $Log: ppu.pas,v $
  Revision 1.1.2.8  2002/11/08 13:58:58  pierre
   * implementation CRC change generates only notes

  Revision 1.1.2.7  2001/09/22 04:49:25  carl
  * updated wrong comment

  Revision 1.1.2.6  2001/07/18 11:14:14  pierre
   * restore normal set if change_endian is true

  Revision 1.1.2.5  2001/06/22 13:27:13  pierre
   * forgot to swap sets on reading ppu

  Revision 1.1.2.4  2001/06/22 12:30:49  pierre
   * handle normal sets as arrays of longint

  Revision 1.1.2.3  2001/06/14 16:42:02  pierre
   + -Sr option for releases, include file changes discarded

  Revision 1.1.2.2  2001/05/21 03:35:08  carl
  * range check error fixes with CRC updating (interface is not same as v1.1)
  + now endian stuff should work ok

  Revision 1.1.2.1  2001/05/18 22:35:01  peter
    * merged endian ppu fixes from mainbranch
    * endian define

  Revision 1.1  2000/07/13 06:29:54  michael
  + Initial import

  Revision 1.59  2000/05/15 13:19:04  pierre
   CRC stuff moved to CRC unit

  Revision 1.58  2000/05/12 08:58:51  pierre
   * adapted to Delphi 3

  Revision 1.57  2000/05/11 06:54:29  florian
    * fixed some vmt problems, especially related to overloaded methods
      in objects/classes

  Revision 1.56  2000/02/29 21:58:31  pierre
   * ORDERSOURCES released

  Revision 1.55  2000/02/09 13:22:59  peter
    * log truncated

  Revision 1.54  2000/01/07 01:14:30  peter
    * updated copyright to 2000

  Revision 1.53  1999/12/02 11:29:07  peter
    * INFTPPU define to write the ppu of the interface to .ppu.intf

  Revision 1.52  1999/11/30 10:40:45  peter
    + ttype, tsymlist

  Revision 1.51  1999/11/23 09:42:38  peter
    * makefile updates to work with new fpcmake

  Revision 1.50  1999/11/21 01:42:37  pierre
   * Nextoverloading ordering fix

  Revision 1.49  1999/11/18 15:34:48  pierre
    * Notes/Hints for local syms changed to
      Set_varstate function

  Revision 1.48  1999/11/17 17:05:02  pierre
   * Notes/hints changes

  Revision 1.47  1999/11/06 14:34:23  peter
    * truncated log to 20 revs

  Revision 1.46  1999/09/17 09:14:56  peter
    * ppu header writting now uses currentppuversion

  Revision 1.45  1999/09/16 13:27:08  pierre
    + error if PPU modulename is different from what is searched
      (8+3 limitations!)
    + cond ORDERSOURCES to allow recompilation of FP
      if symppu.inc is changed (need PPUversion change!)

  Revision 1.44  1999/09/16 11:34:58  pierre
   * typo correction

  Revision 1.43  1999/09/10 18:48:09  florian
    * some bug fixes (e.g. must_be_valid and procinfo.funcret_is_valid)
    * most things for stored properties fixed

  Revision 1.42  1999/08/31 15:47:56  pierre
   + startup conditionals stored in PPU file for debug info

  Revision 1.41  1999/08/30 16:21:40  pierre
   * tempclosing of ppufiles under dos was wrong

  Revision 1.40  1999/08/27 10:48:40  pierre
    + tppufile.tempclose and tempopen added
    * some changes so that nothing is writtedn to disk while
      calculating CRC only

  Revision 1.39  1999/08/24 12:01:36  michael
  + changes for resourcestrings

  Revision 1.38  1999/08/15 10:47:48  peter
    + normalset,smallset writing

  Revision 1.37  1999/08/02 23:13:20  florian
    * more changes to compile for the Alpha

  Revision 1.36  1999/07/23 16:05:25  peter
    * alignment is now saved in the symtable
    * C alignment added for records
    * PPU version increased to solve .12 <-> .13 probs

}
