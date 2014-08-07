{
    $Id: dos.pp,v 1.1.2.2 2002/10/19 14:48:53 carl Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Dos unit for BP7 compatible RTL

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit dos;
interface


Const
  {Bitmasks for CPU Flags}
  fcarry     = $0001;
  fparity    = $0004;
  fauxiliary = $0010;
  fzero      = $0040;
  fsign      = $0080;
  foverflow  = $0800;

  {Bitmasks for file attribute}
  readonly  = $01;
  hidden    = $02;
  sysfile   = $04;
  volumeid  = $08;
  directory = $10;
  archive   = $20;
  anyfile   = $3F;

  {File Status}
  fmclosed = $D7B0;
  fminput  = $D7B1;
  fmoutput = $D7B2;
  fminout  = $D7B3;


Type
{ Needed for LFN Support }
  ComStr  = String[255];
  PathStr = String[255];
  DirStr  = String[255];
  NameStr = String[255];
  ExtStr  = String[255];

{
  filerec.inc contains the definition of the filerec.
  textrec.inc contains the definition of the textrec.
  It is in a separate file to make it available in other units without
  having to use the DOS unit for it.
}
{$i filerec.inc}
{$i textrec.inc}

  DateTime = packed record
    Year,
    Month,
    Day,
    Hour,
    Min,
    Sec   : word;
  End;

  searchrec = packed record
     fill : array[1..21] of byte;
     attr : byte;
     time : longint;
     { reserved : word; not in DJGPP V2 }
     olddta : pointer;
     size : longint;
     name : string[255]; { LFN Name, DJGPP uses only [12] but more can't hurt (PFV) }
  end;
  
  Registers = record
  end;

Var
  DosError : integer;

{Interrupt}
Procedure Intr(intno: byte; var regs: registers);
Procedure MSDos(var regs: registers);

{Info/Date/Time}
Function  DosVersion: Word;
Procedure GetDate(var year, month, mday, wday: word);
Procedure GetTime(var hour, minute, second, sec100: word);
procedure SetDate(year,month,day: word);
Procedure SetTime(hour,minute,second,sec100: word);
Procedure UnpackTime(p: longint; var t: datetime);
Procedure PackTime(var t: datetime; var p: longint);

{Exec}
Procedure Exec(const path: pathstr; const comline: comstr);
Function  DosExitCode: word;

{Disk}
Function  DiskFree(drive: byte) : int64;
Function  DiskSize(drive: byte) : int64;
Procedure FindFirst(const path: pathstr; attr: word; var f: searchRec);
Procedure FindNext(var f: searchRec);
Procedure FindClose(Var f: SearchRec);

{File}
Procedure GetFAttr(var f; var attr: word);
Procedure GetFTime(var f; var time: longint);
Function  FSearch(path: pathstr; dirlist: string): pathstr;
Function  FExpand(const path: pathstr): pathstr;
Procedure FSplit(path: pathstr; var dir: dirstr; var name: namestr; var ext: extstr);

{Environment}
Function  EnvCount: longint;
Function  EnvStr(index: integer): string;
Function  GetEnv(envvar: string): string;

{Misc}
Procedure SetFAttr(var f; attr: word);
Procedure SetFTime(var f; time: longint);
Procedure GetCBreak(var breakvalue: boolean);
Procedure SetCBreak(breakvalue: boolean);
Procedure GetVerify(var verify: boolean);
Procedure SetVerify(verify: boolean);

{Do Nothing Functions}
Procedure SwapVectors;
Procedure GetIntVec(intno: byte; var vector: pointer);
Procedure SetIntVec(intno: byte; vector: pointer);
Procedure Keep(exitcode: word);


implementation

const
   { BIOS Error codes }
   E_OK = 0;    { No error                  }
   ERROR = -1;  { Generic error             }
   EDRVNR =  -2;{ Drive not ready           }
   EUNCMD = -3; { Unknown command           }
   E_CRC = -4;  { CRC error                 }
   EBADRQ = -5; { Bad request               }
   E_SEEK = -6; { Seek error                }
   EMEDIA = -7; { Unknown media             }
   ESECNF = -8; { Sector not found          }
   EPAPER = -9; { Out of paper              }
   EWRITF =-10; { Write fault               }
   EREADF =-11; { Read fault                }
   EWRPRO =-12; { Device is write protected }
   E_CHNG =-14; { Media change detected     }
   EUNDEV =-15; { Unknown device            }
   EBADSF =-16; { Bad sectors on format     }
   EOTHER =-17; { Insert other disk (request) }
   { GemDOS errors codes }
   EINVFN =-32; { Invalid function          }
   EFILNF =-33; { File not found            }
   EPTHNF =-34; { Path not found            }
   ENHNDL =-35; { No more handles           }
   EACCDN =-36; { Access denied             }
   EIHNDL =-37; { Invalid handle            }
   ENSMEM =-39; { Insufficient memory       }
   EIMBA =-40;  { Invalid memory block address  }
   EDRIVE =-46; { Invalid drive specification   }
   ENSAME =-48; { Cross device rename           }
   ENMFIL =-49; { No more files                 }
   ELOCKED =-58; { Record is already locked     }
   ENSLOCK =-59; { Invalid lock removal request }
   ENAMETOOLONG =-64; { Range error             }
   EINTRN =-65; { Internal error                }
   EPLFMT =-66; { Invalid program load format   }
   EGSBF =-67;  { Memory block growth failure   }
   ELOOP =-80;  { Too many symbolic links       }
   EMOUNT =-200;{  Mount point crossed (indicator) }
   
   { Some parameters to GemDOS functions }
   FA_INQUIRE = 0;   { fattrib call/ get attribs }
   FA_SET = 1;       { fattrib call/ set attribs }
   FD_INQUIRE = 0;   { fdatime get               }
   FD_SET  = 1;      { fdatime set               }


{******************************************************************************
                           --- Dos Interrupt ---
******************************************************************************}


procedure LoadDosError(trapres : longint);
var
  r : registers;
  SimpleDosError : word;
begin
    if trapres <> E_OK then
      doserror := -trapres
    else
      doserror:=0;
end;


procedure intr(intno : byte;var regs : registers);
begin
end;


procedure msdos(var regs : registers);
begin
end;


{******************************************************************************
                        --- Info / Date / Time ---
******************************************************************************}

function dosversion : word; assembler;
asm
    movem.l d2/d3/a2/a3,-(sp)
    move.w #$30,-(sp)
    trap   #1
    addq.l #2,sp         { restore stack pointer       }
    movem.l (sp)+,d2/d3/a2/a3
                         { return value in d0          }
end;


function WeekDay (y,m,d:longint):longint;
{
  Calculates th day of the week. returns -1 on error
}
var
  u,v : longint;
begin
  if (m<1) or (m>12) or (y<1600) or (y>4000) or
     (d<1) or (d>30+((m+ord(m>7)) and 1)-ord(m=2)) or
     ((m*d=58) and (((y mod 4>0) or (y mod 100=0)) and (y mod 400>0))) then
   WeekDay:=-1
  else
   begin
     u:=m;
     v:=y;
     if m<3 then
      begin
        inc(u,12);
        dec(v);
      end;
     WeekDay:=(d+2*u+((3*(u+1)) div 5)+v+(v div 4)-(v div 100)+(v div 400)+1) mod 7;
   end;
end;



procedure getdate(var year,month,mday,wday : word);
var
  datevalue : word;
begin
  asm
    movem.l d2/d3/a2/a3,-(sp)
    move.w #$2A,-(sp)
    trap   #1
    addq.l #2,sp
    movem.l (sp)+,d2/d3/a2/a3
    move.w d0,datevalue
  end;
  mday:=datevalue and $1F;
  month := (datevalue shr 5) and $0F;
  year := (((datevalue) shr 9) and $3F)+1980;
  Wday:=weekday(Year,Month,MDay);
end;


procedure setdate(year,month,day : word);
var
  datevalue : word;
begin
  datevalue := day and $1F;
  datevalue := datevalue or word(month shl 5) or word((year - 1980) shl 9);
  asm
    movem.l d2/d3/a2/a3,-(sp)
    move.w datevalue,-(sp)
    move.w #$2B,-(sp)
    trap   #1
    addq.l #4,sp
    movem.l (sp)+,d2/d3/a2/a3
  end;
end;


procedure gettime(var hour,minute,second,sec100 : word);
var
 timevalue : word;
begin
  asm
    movem.l d2/d3/a2/a3,-(sp)
    move.w #$2C,-(sp)
    trap   #1
    addq.l #2,sp
    movem.l (sp)+,d2/d3/a2/a3
    move.w d0,timevalue
  end;
  hour := (timevalue shr 11) and $1F;
  minute := (timevalue shr 5) and $3f;
  { the field returned in seconds from GemDOS is divided by two }
  second := (timevalue and $1F) * 2;
  sec100:=0;
end;


procedure settime(hour,minute,second,sec100 : word);
var
 timevalue : word;
begin
  timevalue := (hour and $1F) shl 11;
  timevalue := timevalue or ((minute and $3f) shl 5);
  timevalue := timevalue or (second div 2) and $1F;
  { sec100 is ignored on atari }
  asm
    movem.l d2/d3/a2/a3,-(sp)
    move.w timevalue,-(sp)
    move.w #$2D,-(sp)
    trap   #1
    addq.l #4,sp
    movem.l (sp)+,d2/d3/a2/a3
  end;
end;


Procedure packtime(var t : datetime;var p : longint);
Begin
  p:=(t.hour)+(t.min shl 6)+(t.sec shl 11)+(t.year shl 16)+(t.month shl 22)
  +  (t.day shl 26);
End;


Procedure unpacktime(p : longint;var t : datetime);
Begin
  with t do
   begin
     hour:=p and 31;
     min:=(p shr 5) and 63;
     sec:=(p shr 10) and 31;
     year:=((p shr 15) and $7F)+1980;
     month:=(p shr 22) and 15;
     day:=(p shr 26) and 31;
   end;
End;


{******************************************************************************
                               --- Exec ---
******************************************************************************}

var
  lastdosexitcode : word;

procedure exec(const path : pathstr;const comline : comstr);
begin
end;

function dosexitcode : word;
begin
  dosexitcode:=lastdosexitcode;
end;


procedure getcbreak(var breakvalue : boolean);
begin
end;


procedure setcbreak(breakvalue : boolean);
begin
end;


procedure getverify(var verify : boolean);
begin
end;


procedure setverify(verify : boolean);
begin
end;


{******************************************************************************
                               --- Disk ---
******************************************************************************}
type
  { disk free            }
  tdiskinfo = packed record
   b_free : longword;      { number of free clusters }
   b_total : longword;     { number of used clusters }
   b_secsize : longword;   { bytes per sector        }
   b_clsize : longword;    { sectors per cluster     }
  end;
  
function diskfree(drive : byte) : int64;
var
 diskinfo : tdiskinfo;
 trapres : longint;
 drivew: word;
begin 
   drivew := drive;
   asm
    movem.l d2/d3/a2/a3,-(sp)
    move.w drivew,-(sp)
    pea    diskinfo
    move.w #$36,-(sp)
    trap   #1
    addq.l #8,sp
    movem.l (sp)+,d2/d3/a2/a3
    move.l d0,trapres
   end;
   LoadDosError(trapres);
   diskfree:=int64(int64(diskinfo.b_free) * int64(diskinfo.b_secsize) * int64(diskinfo.b_clsize));
end;


function disksize(drive : byte) : int64;
var
 diskinfo : tdiskinfo;
 trapres : longint;
 drivew: word;
begin 
   drivew := drive;
   asm
    movem.l d2/d3/a2/a3,-(sp)
    move.w drivew,-(sp)
    pea    diskinfo
    move.w #$36,-(sp)
    trap   #1
    addq.l #8,sp
    movem.l (sp)+,d2/d3/a2/a3
    move.l d0,trapres
   end;
   LoadDosError(trapres);
   disksize:=int64(int64(diskinfo.b_total) * int64(diskinfo.b_secsize) * int64(diskinfo.b_clsize));
end;



{******************************************************************************
                     --- DosFindfirst DosFindNext ---
******************************************************************************}

procedure dossearchrec2searchrec(var f : searchrec);
var
  len : longint;
begin
(*
  { Check is necessary!! OS/2's VDM doesn't clear the name with #0 if the }
  { file doesn't exist! (JM)                                              }
  if dosError = 0 then
    len:=StrLen(@f.Name)
  else len := 0;
  Move(f.Name[0],f.Name[1],Len);
  f.Name[0]:=chr(len);*)
end;


procedure DosFindfirst(path : pchar;attr : word;var f : searchrec);
var
   i : longint;
begin
(*  { allow slash as backslash }
  for i:=0 to strlen(path) do
    if path[i]='/' then path[i]:='\';
  copytodos(f,sizeof(searchrec));
  dosregs.edx:=tb_offset;
  dosregs.ds:=tb_segment;
  dosregs.ah:=$1a;
  msdos(dosregs);
  dosregs.ecx:=attr;
  dosregs.edx:=tb_offset+Sizeof(searchrec)+1;
  dosmemput(tb_segment,tb_offset+Sizeof(searchrec)+1,path^,strlen(path)+1);
  dosregs.ds:=tb_segment;
  dosregs.ah:=$4e;
  msdos(dosregs);
  copyfromdos(f,sizeof(searchrec));
  LoadDosError;
  dossearchrec2searchrec(f);*)
end;


procedure Dosfindnext(var f : searchrec);
begin
(*  copytodos(f,sizeof(searchrec));
  dosregs.edx:=tb_offset;
  dosregs.ds:=tb_segment;
  dosregs.ah:=$1a;
  msdos(dosregs);
  dosregs.ah:=$4f;
  msdos(dosregs);
  copyfromdos(f,sizeof(searchrec));
  LoadDosError;
  dossearchrec2searchrec(f);*)
end;


{******************************************************************************
                     --- Findfirst FindNext ---
******************************************************************************}
procedure findfirst(const path : pathstr;attr : word;var f : searchRec);
var
  path0 : array[0..256] of char;
begin
  doserror:=0;
  {strpcopy(path0,path);
  Dosfindfirst(path0,attr,f);}
end;


procedure findnext(var f : searchRec);
begin
  doserror:=0;
  Dosfindnext(f);
end;


Procedure FindClose(Var f: SearchRec);
begin
  DosError:=0;
end;


procedure swapvectors;
begin
end;


{******************************************************************************
                               --- File ---
******************************************************************************}

procedure fsplit(path : pathstr;var dir : dirstr;var name : namestr;var ext : extstr);
var
   dotpos,p1,i : longint;
begin
  { allow slash as backslash }
  for i:=1 to length(path) do
   if path[i]='/' then path[i]:='\';
  { get drive name }
  p1:=pos(':',path);
  if p1>0 then
    begin
       dir:=path[1]+':';
       delete(path,1,p1);
    end
  else
    dir:='';
  { split the path and the name, there are no more path informtions }
  { if path contains no backslashes                                 }
  while true do
    begin
       p1:=pos('\',path);
       if p1=0 then
         break;
       dir:=dir+copy(path,1,p1);
       delete(path,1,p1);
    end;
    p1:=pos('.',path);
    if p1>0 then
      begin
         ext:=copy(path,p1,4);
         delete(path,p1,length(path)-p1+1);
      end
    else
      ext:='';
    name:=path;
end;



{$DEFINE FPC_FEXPAND_DRIVES} (* Full paths begin with drive specification *)

{$I fexpand.inc}

{$UNDEF FPC_FEXPAND_DRIVES}
{$UNDEF FPC_FEXPAND_UNC}


Function FSearch(path: pathstr; dirlist: string): pathstr;
var
  i,p1   : longint;
  s      : searchrec;
  newdir : pathstr;
begin
{ check if the file specified exists }
  findfirst(path,anyfile,s);
  if doserror=0 then
   begin
     findclose(s);
     fsearch:=path;
     exit;
   end;
{ No wildcards allowed in these things }
  if (pos('?',path)<>0) or (pos('*',path)<>0) then
    fsearch:=''
  else
    begin
       { allow slash as backslash }
       for i:=1 to length(dirlist) do
         if dirlist[i]='/' then dirlist[i]:='\';
       repeat
         p1:=pos(';',dirlist);
         if p1<>0 then
          begin
            newdir:=copy(dirlist,1,p1-1);
            delete(dirlist,1,p1);
          end
         else
          begin
            newdir:=dirlist;
            dirlist:='';
          end;
         if (newdir<>'') and (not (newdir[length(newdir)] in ['\',':'])) then
          newdir:=newdir+'\';
         findfirst(newdir+path,anyfile,s);
         if doserror=0 then
          newdir:=newdir+path
         else
          newdir:='';
       until (dirlist='') or (newdir<>'');
       fsearch:=newdir;
    end;
  findclose(s);
end;



{******************************************************************************
                       --- Get/Set File Time,Attr ---
******************************************************************************}

procedure getftime(var f;var time : longint);
var
 trapres : longint;
 handle : word;
begin
  handle:= filerec(f).handle and $ffff;
  asm
    movem.l d2/d3/a2/a3,-(sp)
    move.w #FD_INQUIRE,-(sp)
    move.w handle,-(sp)
    pea    time
    move.w #$57,-(sp)    { call gemdos function }
    trap   #1
    lea    10(sp),sp     { restore stack pointer }
    movem.l (sp)+,d2/d3/a2/a3
    move.l d0,trapres
  end;
  LoadDosError(trapres);
end;


procedure setftime(var f;time : longint);
var
 trapres : longint;
 handle : word;
 newtime : longint;
begin
  handle:= filerec(f).handle and $ffff;
  newtime := time;
  asm
    movem.l d2/d3/a2/a3,-(sp)
    move.w #FD_SET,-(sp)
    move.w handle,-(sp)
    pea    newtime
    move.w #$57,-(sp)    { call gemdos function }
    trap   #1
    lea    10(sp),sp     { restore stack pointer }
    movem.l (sp)+,d2/d3/a2/a3
    move.l d0,trapres
  end;
  LoadDosError(trapres);
end;


procedure getfattr(var f;var attr : word);
var
 fname : pchar;
 trapres : longint;
begin
  fname:=@(filerec(f).name);
  if fname = nil then
    exit;
  asm
    movem.l d2/d3/a2/a3,-(sp)
    move.w attr,-(sp)
    move.w #FA_INQUIRE,-(sp)
    pea    fname
    move.w #$43,-(sp)    { call fattrib function }
    trap   #1
    lea    10(sp),sp     { restore stack pointer }
    movem.l (sp)+,d2/d3/a2/a3
    move.l d0,trapres
  end;
  { only if the value is negative it is an error }
  if trapres < 0 then
    LoadDosError(trapres)
  else
   begin
    attr := trapres and $FFFF; 
    DosError := 0;
   end;
end;


procedure setfattr(var f;attr : word);
var
 fname : pchar;
 trapres : longint;
begin
  fname:=@(filerec(f).name);
  if fname = nil then
    exit;
  asm
    movem.l d2/d3/a2/a3,-(sp)
    move.w attr,-(sp)
    move.w #FA_SET,-(sp)
    pea    fname
    move.w #$43,-(sp)    { call fattrib function }
    trap   #1
    lea    10(sp),sp     { restore stack pointer }
    movem.l (sp)+,d2/d3/a2/a3
    move.l d0,trapres
  end;
  { only if the value is negative it is an error }
  if trapres < 0 then
    LoadDosError(trapres)
  else
    DosError := 0;
end;


{******************************************************************************
                             --- Environment ---
******************************************************************************}

function envcount : longint;
var
  hp : ppchar;
begin
  hp:=envp;
  envcount:=0;
  while assigned(hp^) do
   begin
     inc(envcount);
     inc(hp);
   end;
end;


function envstr(index : integer) : string;
begin
  if (index<=0) or (index>envcount) then
   begin
     envstr:='';
     exit;
   end;
  envstr:=strpas(envp[index]);
end;


Function  GetEnv(envvar: string): string;
var
  hp      : ppchar;
  hs    : string;
  eqpos : longint;
begin
  envvar:=upcase(envvar);
  hp:=envp;
  getenv:='';
  while assigned(hp^) do
   begin
     hs:=strpas(hp^);
     eqpos:=pos('=',hs);
     if upcase(copy(hs,1,eqpos-1))=envvar then
      begin
        getenv:=copy(hs,eqpos+1,255);
        exit;
      end;
     inc(hp);
   end;
end;


{******************************************************************************
                             --- Not Supported ---
******************************************************************************}

Procedure keep(exitcode : word);
Begin
End;

Procedure getintvec(intno : byte;var vector : pointer);
Begin
End;

Procedure setintvec(intno : byte;vector : pointer);
Begin
End;

var
 version: word;
Begin
 version := DosVersion;
 { Check if GemDOS version is 0.15 or higher }
 if ((version and $ff) = 0) and ((version shl 8)<15) then
   begin
     WriteLn('This program requires GemDOS 0.15 or higher...');
     Halt(1);
   end;
end.

{
  $Log: dos.pp,v $
  Revision 1.1.2.2  2002/10/19 14:48:53  carl
     * register conventions fixes
     + envinorment routines updated

  Revision 1.1.2.1  2002/10/18 22:01:52  carl
     + atari dos unit

}
