{
    $Id: generic.inc,v 1.1.2.31 2003/02/18 17:39:08 jonas Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Processor independent implementation for the system unit
    (adapted for intel i386.inc file)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{****************************************************************************
                               Primitives
****************************************************************************}
type
  pstring = ^shortstring;

{$ifndef FPC_SYSTEM_HAS_MOVE}
procedure Move(const source;var dest;count:longint);
type
  bytearray    = array [0..maxlongint-1] of byte;
var
  i,size : longint;
begin
  if count <= 0 then exit;
  Dec(count);
  for i:=0 to count do
         bytearray(dest)[i]:=bytearray(source)[i];
end;
{$endif ndef FPC_SYSTEM_HAS_MOVE}


{$ifndef FPC_SYSTEM_HAS_FILLCHAR}
Procedure FillChar(var x;count:longint;value:byte);
type
  longintarray = array [0..maxlongint div 4] of longint;
  bytearray    = array [0..maxlongint-1] of byte;
var
  i,v : longint;
begin
  if count <= 0 then exit;
  v := 0;
  v:=(value shl 8) or (value and $FF);
  v:=(v shl 16) or (v and $ffff);
  for i:=0 to (count div 4) -1 do
    longintarray(x)[i]:=v;
  for i:=(count div 4)*4 to count-1 do
    bytearray(x)[i]:=value;
end;
{$endif ndef FPC_SYSTEM_HAS_FILLCHAR}


{$ifndef RTLLITE}

{$ifndef FPC_SYSTEM_HAS_FILLBYTE}
procedure FillByte (var x;count : longint;value : byte );
begin
  FillChar (X,Count,CHR(VALUE));
end;
{$endif ndef FPC_SYSTEM_HAS_FILLBYTE}


{$ifndef FPC_SYSTEM_HAS_FILLWORD}
procedure fillword(var x;count : longint;value : word);
type
  longintarray = array [0..maxlongint div 4] of longint;
  wordarray    = array [0..maxlongint div 2] of word;
var
  i,v : longint;
begin
  if count <= 0 then exit;
  v:=value*$10000+value;
  for i:=0 to (count div 2) -1 do
    longintarray(x)[i]:=v;
  for i:=(count div 2)*2 to count-1 do
    wordarray(x)[i]:=value;
end;
{$endif ndef FPC_SYSTEM_HAS_FILLWORD}


{$ifndef FPC_SYSTEM_HAS_FILLDWORD}
procedure FillDWord(var x;count : longint;value : DWord);
type
  longintarray = array [0..maxlongint div 4] of longint;
begin
  if Count <= 0 then exit;
  while Count<>0 do
   begin
     longintarray(X)[Count-1]:=longint(Value);
     Dec(Count);
   end;
end;
{$endif ndef FPC_SYSTEM_HAS_FILLDWORD}


{$ifndef FPC_SYSTEM_HAS_INDEXCHAR}
function IndexChar(var buf;len:longint;b:char):longint;
begin
  IndexChar:=IndexByte(Buf,Len,byte(B));
end;
{$endif ndef FPC_SYSTEM_HAS_INDEXCHAR}

{$endif ndef RTLLITE}

{$ifndef FPC_SYSTEM_HAS_INDEXBYTE}
function IndexByte(var buf;len:longint;b:byte):longint;
type
  bytearray    = array [0..maxlongint-1] of byte;
var
  I : longint;
begin
  I:=0;
  while (I<Len) and (bytearray(buf)[I]<>b) do
   inc(I);
  if (i=Len) then
   i:=-1;                                {Can't use 0, since it is a possible value}
  IndexByte:=I;
end;
{$endif ndef FPC_SYSTEM_HAS_INDEXBYTE}


{$ifndef RTLLITE}

{$ifndef FPC_SYSTEM_HAS_INDEXWORD}
function Indexword(var buf;len:longint;b:word):longint;
type
  wordarray    = array [0..maxlongint div 2] of word;
var
  I : longint;
begin
  I:=0;
  while (I<Len) and (wordarray(buf)[I]<>b) do
   inc(I);
  if (i=Len) then
   i:=-1;           {Can't use 0, since it is a possible value for index}
  Indexword:=I;
end;
{$endif ndef FPC_SYSTEM_HAS_INDEXWORD}


{$ifndef FPC_SYSTEM_HAS_INDEXDWORD}
function IndexDWord(var buf;len:longint;b:DWord):longint;
type
  longintarray = array [0..maxlongint div 4] of longint;
var
  I : longint;
begin
  I:=0;
  while (I<Len) and (longintarray(buf)[I]<>b) do inc(I);
  if (i=Len) then
   i:=-1;           {Can't use 0, since it is a possible value for index}
  IndexDWord:=I;
end;
{$endif ndef FPC_SYSTEM_HAS_INDEXDWORD}


{$ifndef FPC_SYSTEM_HAS_COMPARECHAR}
function CompareChar(var buf1,buf2;len:longint):longint;
begin
  CompareChar:=CompareByte(buf1,buf2,len);
end;
{$endif ndef FPC_SYSTEM_HAS_COMPARECHAR}


{$ifndef FPC_SYSTEM_HAS_COMPAREBYTE}
function CompareByte(var buf1,buf2;len:longint):longint;
type
  bytearray    = array [0..maxlongint-1] of byte;
var
  I : longint;
begin
  I:=0;
  if (Len<>0) and (@Buf1<>@Buf2) then
   begin
     while (bytearray(Buf1)[I]=bytearray(Buf2)[I]) and (I<Len) do
      inc(I);
     if I=Len then  {No difference}
      I:=0
     else
      begin
        I:=bytearray(Buf1)[I]-bytearray(Buf2)[I];
        if I>0 then
         I:=1
        else
         if I<0 then
          I:=-1;
      end;
   end;
  CompareByte:=I;
end;
{$endif ndef FPC_SYSTEM_HAS_COMPAREBYTE}


{$ifndef FPC_SYSTEM_HAS_COMPAREWORD}
function CompareWord(var buf1,buf2;len:longint):longint;
type
  wordarray    = array [0..maxlongint div 2] of word;
var
  I : longint;
begin
  I:=0;
  if (Len<>0) and (@Buf1<>@Buf2) then
   begin
     while (wordarray(Buf1)[I]=wordarray(Buf2)[I]) and (I<Len) do
      inc(I);
     if I=Len then  {No difference}
      I:=0
     else
      begin
        I:=wordarray(Buf1)[I]-wordarray(Buf2)[I];
        if I>0 then
         I:=1
        else
         if I<0 then
          I:=-1;
      end;
   end;
  CompareWord:=I;
end;
{$endif ndef FPC_SYSTEM_HAS_COMPAREWORD}


{$ifndef FPC_SYSTEM_HAS_COMPAREDWORD}
function CompareDWord(var buf1,buf2;len:longint):longint;
type
  longintarray    = array [0..maxlongint div 4] of longint;
var
  I : longint;
begin
  I:=0;
  if (Len<>0) and (@Buf1<>@Buf2) then
   begin
     while (longintarray(Buf1)[I]=longintarray(Buf2)[I]) and (I<Len) do
      inc(I);
     if I=Len then  {No difference}
      I:=0
     else
      begin
        I:=longintarray(Buf1)[I]-longintarray(Buf2)[I];
        if I>0 then
         I:=1
        else
         if I<0 then
          I:=-1;
      end;
   end;
  CompareDWord:=I;
end;
{$endif ndef FPC_SYSTEM_HAS_COMPAREDWORD}


{$ifndef FPC_SYSTEM_HAS_MOVECHAR0}
procedure MoveChar0(var buf1,buf2;len:longint);
var
  I : longint;
begin
  if Len = 0 then exit;
  I:=IndexByte(Buf1,Len,0);
  if I<>-1 then
    Move(Buf1,Buf2,I)
  else
    Move(Buf1,Buf2,len);
end;
{$endif ndef FPC_SYSTEM_HAS_MOVECHAR0}


{$ifndef FPC_SYSTEM_HAS_INDEXCHAR0}
function IndexChar0(var buf;len:longint;b:Char):longint;
var
  I : longint;
begin
  if Len<>0 then
   begin
     I:=IndexByte(Buf,Len,0);
     IndexChar0:=IndexByte(Buf,I,0);
   end
  else
   IndexChar0:=0;
end;
{$endif ndef FPC_SYSTEM_HAS_INDEXCHAR0}


{$ifndef FPC_SYSTEM_HAS_COMPARECHAR0}
function CompareChar0(var buf1,buf2;len:longint):longint;
type
  bytearray    = array [0..maxlongint-1] of byte;

Var i : longint;

begin
  I:=0;
  if (Len<>0) and (@Buf1<>@Buf2) then
   begin
     while (I<Len) And
           ((bytearray(Buf1)[i]<>0) and (Bytearray(buf2)[i]<>0)) and
           (bytearray(Buf1)[I]=bytearray(Buf2)[I])  do
      inc(I);
     if (I=Len) or
        (Bytearray(Buf1)[i]=0) or
        (Bytearray(buf2)[I]=0) then  {No difference or 0 reached }
      I:=0
     else
      begin
        I:=bytearray(Buf1)[I]-bytearray(Buf2)[I];
        if I>0 then
         I:=1
        else
         if I<0 then
          I:=-1;
      end;
   end;
  CompareChar0:=I;
end;
{$endif ndef FPC_SYSTEM_HAS_COMPARECHAR0}

{$endif ndef RTLLITE}


{****************************************************************************
                              Object Helpers
****************************************************************************}

{$ifndef FPC_SYSTEM_HAS_FPC_HELP_CONSTRUCTOR}

{ Generic code does not set the register used for self !
  So this needs to be done by the compiler after calling
  FPC_HELP_CONSTRUCTOR : generic allways means aa little less efficient (PM) }
{ I don't think we really need to save any registers here      }
{ since this is called at the start of the constructor (CEC)   }
function int_help_constructor(var _self : pointer; var vmt : pointer; vmt_pos : cardinal) : pointer; [public,alias:'FPC_HELP_CONSTRUCTOR'];
   type
     ppointer = ^pointer;
     pvmt = ^tvmt;
     tvmt = packed record
        size,msize : longint;
        parent : pointer;
        end;
   var
      objectsize : longint;
      vmtcopy : pointer;
begin
   if vmt=nil then
     begin
       int_help_constructor:=_self;
       exit;
     end;
   vmtcopy:=vmt;
   objectsize:=pvmt(vmtcopy)^.size;
   if _self=nil then
     begin
       getmem(_self,objectsize);
       longint(vmt):=-1; { needed for fail }
     end;
   if _self<>nil then
     begin
       fillchar(_self^,objectsize,#0);
       ppointer(_self+vmt_pos)^:=vmtcopy;
     end;
   int_help_constructor:=_self;
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_HELP_CONSTRUCTOR}

{$ifndef FPC_SYSTEM_HAS_FPC_HELP_DESTRUCTOR}

procedure int_help_destructor(var _self : pointer; vmt : pointer; vmt_pos : cardinal);saveregisters;[public,alias:'FPC_HELP_DESTRUCTOR'];
   type
     ppointer = ^pointer;
     pvmt = ^tvmt;
     tvmt = packed record
        size,msize : longint;
        parent : pointer;
        end;
   var
      objectsize : longint;
begin
   if (_self=nil) then
     exit;
   if (pvmt(ppointer(_self+vmt_pos)^)^.size=0) or
      (pvmt(ppointer(_self+vmt_pos)^)^.size+pvmt(ppointer(_self+vmt_pos)^)^.msize<>0) then
     HandleError(210);
   objectsize:=pvmt(vmt)^.size;
   { reset vmt to nil for protection }
   ppointer(_self+vmt_pos)^:=nil;
   freemem(_self,objectsize);
   _self:=nil;
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_HELP_DESTRUCTOR}

{$ifndef FPC_SYSTEM_HAS_FPC_HELP_FAIL}
{error No pascal version of Int_help_fail; there is one now PM }
procedure int_help_fail(var _self : pointer; var vmt : pointer; vmt_pos : cardinal); [public,alias:'FPC_HELP_FAIL'];
   type
     ppointer = ^pointer;
     pvmt = ^tvmt;
     tvmt = packed record
        size,msize : longint;
        parent : pointer;
        end;
begin
   if vmt=nil then
     exit;
   if longint(vmt)=-1 then
     begin
       if (_self=nil) or (ppointer(_self+vmt_pos)^=nil) then
         HandleError(210)
       else
         begin
           ppointer(_self+vmt_pos)^:=nil;
           freemem(_self);
           _self:=nil;
           vmt:=nil;
         end;
     end
   else
     ppointer(_self+vmt_pos)^:=nil;
   _self := nil;
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_HELP_FAIL}

{$ifndef FPC_SYSTEM_HAS_FPC_NEW_CLASS}
{$error No pascal version of Int_new_class}
{$endif ndef FPC_SYSTEM_HAS_FPC_NEW_CLASS}

{$ifndef FPC_SYSTEM_HAS_FPC_DISPOSE_CLASS}
{$error No pascal version of Int_dispose_class}
{$endif ndef FPC_SYSTEM_HAS_FPC_DISPOSE_CLASS}

{$ifndef FPC_SYSTEM_HAS_FPC_CHECK_OBJECT}

procedure int_check_object(vmt : pointer);saveregisters;[public,alias:'FPC_CHECK_OBJECT'];
   type
     pvmt = ^tvmt;
     tvmt = packed record
        size,msize : longint;
        parent : pointer;
        end;
begin
   if (vmt=nil) or
      (pvmt(vmt)^.size=0) or
      (pvmt(vmt)^.size+pvmt(vmt)^.msize<>0) then
        HandleError(210);
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_CHECK_OBJECT}

{ checks for a correct vmt pointer }
{ deeper check to see if the current object is }
{ really related to the true }

{$ifndef FPC_SYSTEM_HAS_FPC_CHECK_OBJECT_EXT}

procedure int_check_object_ext(vmt, expvmt : pointer);saveregisters;[public,alias:'FPC_CHECK_OBJECT_EXT'];
   type
     pvmt = ^tvmt;
     tvmt = packed record
        size,msize : longint;
        parent : pointer;
        end;
begin
   if (vmt=nil) or
      (pvmt(vmt)^.size=0) or
      (pvmt(vmt)^.size+pvmt(vmt)^.msize<>0) then
        HandleError(210);
   while assigned(vmt) do
     if vmt=expvmt then
       exit
     else
       vmt:=pvmt(vmt)^.parent;
   HandleError(219);
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_CHECK_OBJECT_EXT}


{****************************************************************************
                                 String
****************************************************************************}

{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_COPY}

procedure int_strcopy(len:longint;sstr,dstr:pointer);[public,alias:'FPC_SHORTSTR_COPY'];
var
  slen : byte;
type
  pstring = ^string;
begin
  if dstr=nil then
    exit;
  if sstr=nil then
    begin
      if dstr<>nil then
        pstring(dstr)^[0]:=#0;
      exit;
    end;
  slen:=length(pstring(sstr)^);
  if slen<len then
    len:=slen;
  { don't forget the length character }
  if len <> 0 then
      move(sstr^,dstr^,len+1);
  pstring(dstr)^[0]:=chr(len);
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_SHORTSTR_COPY}

{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_CONCAT}

procedure int_strconcat(s1,s2:pointer);[public,alias:'FPC_SHORTSTR_CONCAT'];
var
  s1l, s2l : byte;
type
  pstring = ^string;
begin
  if (s1=nil) or (s2=nil) then
    exit;
  s1l:=length(pstring(s1)^);
  s2l:=length(pstring(s2)^);
  if s1l+s2l>255 then
    s1l:=255-s2l;
  move(pstring(s1)^[1],pstring(s2)^[s2l+1],s1l);
  pstring(s2)^[0]:=chr(s1l+s2l);
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_SHORTSTR_CONCAT}

{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_COMPARE}

function int_strcmp(rightstr,leftstr:pointer) : longint;[public,alias:'FPC_SHORTSTR_COMPARE'];
var
   leftlen,rightlen,max,i : byte;
   d : longint;
type
  pstring = ^string;
begin
  rightlen:=length(pstring(rightstr)^);
  leftlen:=length(pstring(leftstr)^);
  if leftlen<rightlen then
    max:=leftlen
  else
    max:=rightlen;
  for i:=1 to max do
    begin
     d:=byte(pstring(leftstr)^[i])-byte(pstring(rightstr)^[i]);
     if d>0 then
       exit(1)
     else if d<0 then
       exit(-1);
    end;
  if leftlen>rightlen then
    exit(1)
  else if leftlen<rightlen then
    exit(-1)
  else
    exit(0);
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_SHORTSTR_COMPARE}

{$ifndef FPC_SYSTEM_HAS_FPC_PCHAR_TO_SHORTSTR}

function strpas(p:pchar):shortstring;[public,alias:'FPC_PCHAR_TO_SHORTSTR'];
var
  l : longint;
  s: shortstring;
begin
  if p=nil then
    l:=0
  else
    l:=strlen(p);
  if l>255 then
    l:=255;
  if l>0 then
    move(p^,s[1],l);
  s[0]:=chr(l);
  strpas := s;
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_PCHAR_TO_SHORTSTR}

{$ifndef FPC_SYSTEM_HAS_FPC_CHARARRAY_TO_SHORTSTR}

function strchararray(p:pchar; l : longint):shortstring;[public,alias:'FPC_CHARARRAY_TO_SHORTSTR'];
var
 s: shortstring;
 index: longint;
 len: byte;
begin
  if l>=256 then
    l:=255
  else if l<0 then
    l:=0;
  move(p^,s[1],l);
  index:=IndexByte(p^,l,0);
  if (index < 0) then
    len := l
  else
    len := index;
  s[0] := chr(len);
  strchararray := s;
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_CHARARRAY_TO_SHORTSTR}

{$ifopt r+}
{$define rangeon}
{$r-}
{$endif}

{$ifndef FPC_SYSTEM_HAS_FPC_STR_TO_CHARARRAY}
procedure str_to_chararray(strtyp, arraysize: longint; src,dest: pchar);[public,alias:'FPC_STR_TO_CHARARRAY'];
var
  len: longint;
begin
  case strtyp of
    { shortstring }
    0:
      begin
        len := byte(src[0]);
        inc(src);
      end;
{$ifdef SUPPORT_ANSISTRING}
    { ansistring}
    1: len := length(ansistring(pointer(src)));
{$endif SUPPORT_ANSISTRING}
    { longstring }
    2:;
    { widestring }
    3:;
  end;
  if len > arraysize then
    len := arraysize;
  { make sure we don't dereference src if it can be nil (JM) }
  if len > 0 then
    move(src^,dest^,len);
  fillchar(dest[len],arraysize-len,0);
end;
{$endif FPC_SYSTEM_HAS_FPC_STR_TO_CHARARRAY}

{$ifdef rangeon}
{$r+}
{undef rangeon}
{$endif rangeon}

{$ifndef FPC_SYSTEM_HAS_STRLEN}

function strlen(p:pchar):longint;
var i : longint;
begin
  i:=0;
  while p[i]<>#0 do inc(i);
  exit(i);
end;

{$endif ndef FPC_SYSTEM_HAS_STRLEN}

{****************************************************************************
                       Caller/StackFrame Helpers
****************************************************************************}

{$ifndef FPC_SYSTEM_HAS_GET_FRAME}
{$error Get_frame must be defined for each processor }
{$endif ndef FPC_SYSTEM_HAS_GET_FRAME}

{$ifndef FPC_SYSTEM_HAS_GET_CALLER_ADDR}
{$error Get_caller_addr must be defined for each processor }
{$endif ndef FPC_SYSTEM_HAS_GET_CALLER_ADDR}

{$ifndef FPC_SYSTEM_HAS_GET_CALLER_FRAME}
{$error Get_caller_frame must be defined for each processor }
{$endif ndef FPC_SYSTEM_HAS_GET_CALLER_FRAME}

{****************************************************************************
                                 Math
****************************************************************************}

{$ifndef FPC_SYSTEM_HAS_ABS_LONGINT}
function abs(l:longint):longint;[internconst:in_const_abs];
begin
   if l<0 then
     abs:=-l
   else
     abs:=l;
end;

{$endif ndef FPC_SYSTEM_HAS_ABS_LONGINT}

{$ifndef FPC_SYSTEM_HAS_ODD_LONGINT}

function odd(l:longint):boolean; [internconst:in_const_odd];
begin
   odd:=boolean(l and 1);
end;

{$endif ndef FPC_SYSTEM_HAS_ODD_LONGINT}

{$ifndef FPC_SYSTEM_HAS_ODD_CARDINAL}

function odd(l:cardinal):boolean;
begin
   odd:=boolean(l and 1);
end;

{$endif ndef FPC_SYSTEM_HAS_ODD_CARDINAL}


{$ifdef int64}

{$ifndef FPC_SYSTEM_HAS_ODD_INT64}

function odd(l:int64):boolean;[internconst:in_const_odd];
begin
   odd:=boolean(longint(l) and 1);
end;

{$endif ndef FPC_SYSTEM_HAS_ODD_INT64}

{$ifndef FPC_SYSTEM_HAS_ODD_QWORD}

function odd(l:qword):boolean;
begin
   odd:=boolean(longint(l) and 1);
end;

{$endif ndef FPC_SYSTEM_HAS_ODD_QWORD}

{$endif int64}


{$ifndef FPC_SYSTEM_HAS_SQR_LONGINT}

function sqr(l:longint):longint;[internconst:in_const_sqr];
begin
   sqr:=l*l;
end;

{$endif ndef FPC_SYSTEM_HAS_SQR_LONGINT}


{$ifdef int64}

{$ifndef FPC_SYSTEM_HAS_ABS_INT64}

function abs(l: Int64): Int64;
begin
  if l < 0 then
    abs := -l
  else
    abs := l;
end;

{$endif ndef FPC_SYSTEM_HAS_ABS_INT64}


{$ifndef FPC_SYSTEM_HAS_SQR_INT64}

function sqr(l: Int64): Int64;
begin
  sqr := l*l;
end;

{$endif ndef FPC_SYSTEM_HAS_SQR_INT64}


{$ifndef FPC_SYSTEM_HAS_SQR_QWORD}

function sqr(l: QWord): QWord;
begin
  sqr := l*l;
end;

{$endif ndef FPC_SYSTEM_HAS_SQR_INT64}

{$endif int64}

{$ifndef FPC_SYSTEM_HAS_SPTR}
{$error Sptr must be defined for each processor }
{$endif ndef FPC_SYSTEM_HAS_SPTR}

{****************************************************************************
                                 Str()
****************************************************************************}

{$ifndef FPC_SYSTEM_HAS_INT_STR_LONGINT}

procedure int_str(l : longint;var s : string);
var
  value: longint;
  negative: boolean;

  begin
     negative := false;
     s:='';
     { Workaround: }
     if l=$80000000 then
       begin
         s:='-2147483648';
         exit;
       end;
     { handle case where l = 0 }
     if l = 0 then
       begin
         s:='0';
         exit;
       end;
     If l < 0 then
       begin
         negative := true;
         value:=abs(l);
       end
     else
         value:=l;
     { handle non-zero case }
     while value>0 do
       begin
         s:=char((value mod 10)+ord('0'))+s;
         value := value div 10;
       end;
     if negative then
       s := '-' + s;
  end;

{$endif ndef FPC_SYSTEM_HAS_INT_STR_LONGINT}

{$ifndef FPC_SYSTEM_HAS_INT_STR_CARDINAL}

procedure int_str(l : cardinal;var s : string);
begin
  s:='';
  if l = 0 then
  begin
    s := '0';
    exit;
  end;
  while l>0 do
    begin
       s:=char(ord('0')+(l mod 10))+s;
       l:=l div 10;
    end;
end;

{$endif ndef FPC_SYSTEM_HAS_INT_STR_CARDINAL}

{****************************************************************************
                               Bounds Check
****************************************************************************}

{$ifndef FPC_SYSTEM_HAS_FPC_BOUNDCHECK}

procedure int_boundcheck(l : longint; range : pointer);[public,alias: 'FPC_BOUNDCHECK'];
  type
    prange = ^trange;
    trange = packed record
               min,max : longint;
             end;
begin
   if (l < prange(range)^.min) or
      (l > prange(range)^.max) then
     HandleError(201);
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_BOUNDCHECK}



{
  $Log: generic.inc,v $
  Revision 1.1.2.31  2003/02/18 17:39:08  jonas
    - removed buggy i386-specific FPC_CHARARRAY_TO_SHORTSTR
    * fixed generic FPC_CHARARRAY_TO_SHORTSTR (web bug 2382)
    * fixed some potential range errors in indexchar/word/dword

  Revision 1.1.2.30  2003/01/15 22:35:12  pierre
   * remove some unused vars

  Revision 1.1.2.29  2002/10/20 12:04:16  carl
    * avoid crashes with negative values for fill / move

  Revision 1.1.2.28  2002/10/15 15:40:55  carl
  HandleError 220 -> HandleError 219

  Revision 1.1.2.27  2002/10/14 19:32:39  pierre
   * fix tclass4 test failure

  Revision 1.1.2.26  2002/10/12 13:20:51  carl
    * use correct defines

  Revision 1.1.2.25  2002/10/11 11:44:31  pierre
   * Replace RunError by HandleError in system unit include files

  Revision 1.1.2.24  2002/09/27 20:59:51  carl
    * fix problems with 2GB limit

  Revision 1.1.2.23  2002/09/13 18:52:04  carl
    * self must be set to nil on failure

  Revision 1.1.2.22  2002/03/30 13:08:10  carl
  * memory corruption bugfix in FPC_HELP_CONSTRUCTOR if object cannot be allocated

  Revision 1.1.2.21  2002/01/21 12:33:49  jonas
    + abs(int64), sqr(int64), sqr(qword)

  Revision 1.1.2.20  2001/09/17 23:47:02  pierre
   * leave in_const_odd for longint version as this is required for version 1.0, don't merge to 1.1

  Revision 1.1.2.19  2001/08/01 10:49:22  pierre
   * fix string compare error for 't'>'ta'

  Revision 1.1.2.18  2001/07/31 01:39:55  carl
  - removed some commented code (cleanup)

  Revision 1.1.2.17  2001/07/23 14:59:22  pierre
   * fix genric shortstr_compare function

  Revision 1.1.2.16  2001/07/23 14:04:26  pierre
   * fix error in previous commit for help_constructor

  Revision 1.1.2.15  2001/07/23 13:46:12  pierre
   * fix help_constructor and implement help_fail

  Revision 1.1.2.14  2001/07/20 13:03:07  pierre
   * only allocate memory if _self is nil in help_constructor

  Revision 1.1.2.13  2001/07/18 15:32:50  pierre
   generic FPC_HELP_CONSTRUCTOR changed to a function returning self address

  Revision 1.1.2.12  2001/07/18 07:22:31  pierre
   + generate error if no FPC_HELP_FAIL is present

  Revision 1.1.2.11  2001/05/21 16:44:05  carl
  + more saveregisters required for some routines (compare with 386 asm version)

  Revision 1.1.2.10  2001/05/18 17:48:33  carl
  * bugfix of conversion for longint and cardinal (case = zero was not taken care of)

  Revision 1.1.2.9  2001/05/16 15:45:15  jonas
    + odd() for cardinal, int64 and qword

  Revision 1.1.2.8  2001/05/09 03:27:41  carl
  * another packed record for FPC_BOUNDCHECK

  Revision 1.1.2.7  2001/04/22 00:38:29  carl
  - removed FPC_IOCHECK (no longer required, it is in system.inc)

  Revision 1.1.2.6  2001/04/21 05:11:07  carl
  * corrected problem with int_str() with cardinal parameter

  Revision 1.1.2.5  2001/04/14 13:32:31  carl
  * Now compiles and works (test with i386 compiler)

  Revision 1.1.2.4  2001/04/12 02:55:22  carl
  * Corrected string -> shortstring
  * Corrected VMT fields -> packed record

  Revision 1.1.2.3  2001/03/05 17:09:29  jonas
    * changed typecast in FPC_STR_TO_CHARARRAY so that no temp ansistring is
      generated anymore

  Revision 1.1.2.2  2000/10/01 13:14:50  michael
  + Corrected and (hopefully) improved compare0

  Revision 1.1.2.1  2000/08/09 11:21:32  jonas
    + FPC_STR_TO_CHARARRAY routine necessary for string -> chararray
      conversions fix

  Revision 1.1  2000/07/13 06:30:45  michael
  + Initial import

  Revision 1.9  2000/07/07 18:23:41  marco
   * Changed move (var source;var dest) to move (const source;var dest)

  Revision 1.8  2000/03/10 13:45:31  pierre
   * small fixes

  Revision 1.7  2000/02/09 16:59:29  peter
    * truncated log

  Revision 1.6  2000/01/10 09:54:30  peter
    * primitives added

  Revision 1.5  2000/01/07 16:41:34  daniel
    * copyright 2000

  Revision 1.4  2000/01/07 16:32:24  daniel
    * copyright 2000 added

  Revision 1.3  1999/12/21 11:12:16  pierre
     * some assembler functions translated to pascal
       WARNING these are not yet TESTED !!!
     + FPC_CHARARRAY_TO_SHORTSTRING added

}
