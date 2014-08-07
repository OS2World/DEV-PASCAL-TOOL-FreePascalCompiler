{ Straight translation of libc readdir related code.

Thanks to Sebastian G. for his considerable help.
}

// Open a directory.

const

	DTF_HIDEW	=1;	{ hide whiteout entries }
	DTF_NODUP	=2;	{ don't return duplicate names }
	DTF_REWIND	=4;	{ rewind after reading union stack }
	__DTF_READALL	=8;	{ everything has been read }
	DT_WHT		=14;

  { For File control mechanism }
  FD_cloexec=1;
  F_GetFd  = 1;
  F_SetFd  = 2;
  F_GetFl  = 3;
  F_SetFl  = 4;
  F_GetLk  = 5;
  F_SetLk  = 6;
  F_SetLkW = 7;
  F_GetOwn = 8;
  F_SetOwn = 9;

type pcardinal=^cardinal;
     ppbyte=^pbyte;







function sys_getdirentries(fd:longint;buf:pchar;nbytes:longint;basep:pcardinal):longint;
forward;


{ Hybrid exponential search/linear search merge sort with hybrid
  natural/pairwise first pass.  Requires about .3% more comparisons
  for random data than LSMS with pairwise first pass alone.
  It works for objects as small as two bytes. }

{$define NATURAL}

const
  THRESHOLD = 16;		// Best choice for natural merge cut-off.

{ $define NATURAL to get hybrid natural merge.
  (The default is pairwise merging.) 

#include <sys/types.h>

#include <errno.h>
#include <stdlib.h>
#include <string.h>
}
const
  ISIZE = SizeOf(Longint);
  PSIZE = SizeOf(PByte);

type

  TCompareFunction = function(p1, p2: Pointer): Longint;


procedure setup(list1, list2: PByte; n, size: Longint; cmp: TCompareFunction); forward;
procedure insertionsort(a: PByte; n, size: Longint; cmp: TCompareFunction); forward;



{ Find the next possible pointer head.  (Trickery for forcing an array
  to do double duty as a linked list when objects do not align with word
  boundaries. }
// Assumption: PSIZE is a power of 2.

{#define EVAL(p) (u_char **)						\
	((u_char *)0 +							\
	    (((u_char *)p + PSIZE - 1 - (u_char *) 0) & ~(PSIZE - 1)))}

function EVAL(p: PByte): PPByte;
begin
  EVAL := PPByte((LongInt(p) + PSIZE - 1) and not (PSIZE - 1));
end;

// Arguments are as for qsort.



function mergesort(base: Pointer; nmemb, size: Longint; cmp: TCompareFunction): Longint;

  procedure ICOPY_LIST(src, dst, last: PByte);
  begin
    Move(src, dst, longint(last - src));
  end;

  procedure ICOPY_ELT(src, dst: PByte; i: Longint);
  begin
    Move(src, dst, i);
  end;

  procedure CCOPY_LIST(src, dst, last: PByte);
  begin
    Move(src, dst, longint(last - src));
  end;

  procedure CCOPY_ELT(src, dst: PByte; i: Longint);
  begin
    Move(src, dst, i);
  end;

var
  i, sense, big, iflag: Longint;
  f1, f2, t, b, tp2, q, l1, l2, list2, list1, p2, p, last: PByte;
  p1: PPByte;

Label exponential,fastcase,xcopy;

begin
  if size < PSIZE div 2 then		// Pointers must fit into 2 * size.
  begin
    errno := SYS_EINVAL;
    exit(-1);
  end;
  if nmemb = 0 then
    exit(0);

  iflag := 0;
  if ((size mod ISIZE) = 0) and ((LongInt(base) mod ISIZE) = 0) then
    iflag := 1;

  GetMem(list2, nmemb * size + PSIZE);
  if not Assigned(list2) then
    exit(-1);

  list1 := base;
  setup(list1, list2, nmemb, size, cmp);
  last := list2 + nmemb * size;
  i := 0;
  big := 0;
  while EVAL(list2)^ <> last do
  begin
    l2 := list1;
    p1 := EVAL(list1);
    tp2 := list2;
    p2 := list2;
    while p2 <> last do
    begin
      p2 := EVAL(p2)^;
      f1 := l2;
      f2 := list1 + (p2 - list2);
      l1 := f2;
      if p2 <> last then
        p2 := EVAL(p2)^;
      l2 := list1 + (p2 - list2);
      while (f1 < l1) and (f2 < l2) do
      begin
        if cmp(f1, f2) <= 0 then
	begin
	  q := f2;
	  b := f1;
	  t := l1;
	  sense := -1;
	end else
	begin
	  q := f1;
	  b := f2;
	  t := l2;
	  sense := 0;
	end;
	if big=0 then
	begin
	  while True do
	  begin
	    Inc(b, Size);
	    if (b >= t) or (cmp(q, b) <= sense) then
	      break;
	    Inc(i);
	    begin
	      big := 1;
	      goto EXPONENTIAL;
	    end;
	  end;
	end else
	begin
EXPONENTIAL:
	  i := size;
	  while True do
	  begin
	    p := b + i;
	    if p >= t then
	    begin
	      p := t - size;
	      if (p > b) and (cmp(q, p) <= sense) then
	        t := p
	      else
	        b := p;
	      break;
	    end else
	    if cmp(q, p) <= sense then
	    begin
	      t := p;
	      if i = size then
	        big := 0;
	      goto FASTCASE;
	    end else
	      b := p;
	    i := i shl 1;
	  end;

	  while t > b + size do
	  begin
	    i := (((t - b) div size) shr 1) * size;
	    p := b + i;
	    if cmp(q, p) <= sense then
	      t := p
	    else
	      b := p;
	  end;
	  goto xCOPY;
FASTCASE:
	  while i > size do
	  begin
	    i := i shr 1;
	    p := b + i;
	    if longint(p) <= sense then
	      t := p
	    else
	      b := p;
	  end;
xCOPY:	  b := t;
	end;

	i := size;
	if q = f1 then
	  if iflag<>0 then
	  begin
	    ICOPY_LIST(f2, tp2, b);
	    ICOPY_ELT(f1, tp2, i);
	  end else
	  begin
	    CCOPY_LIST(f2, tp2, b);
	    CCOPY_ELT(f1, tp2, i);
	  end
	else
	  if iflag<>0 then
	  begin
	    ICOPY_LIST(f1, tp2, b);
	    ICOPY_ELT(f2, tp2, i);
	  end else
	  begin
	    CCOPY_LIST(f1, tp2, b);
	    CCOPY_ELT(f2, tp2, i);
	  end;
      end;
      if f2 < l2 then
        if iflag<>0  then
	  ICOPY_LIST(f2, tp2, l2)
	else
	  CCOPY_LIST(f2, tp2, l2)
      else if (f1 < l1) then
        if iflag<>0 then
	  ICOPY_LIST(f1, tp2, l1)
	else
	  CCOPY_LIST(f1, tp2, l1);
      p1^ := l2;

      p1 := EVAL(l2);
    end;

    tp2 := list1;		// swap list1, list2
    list1 := list2;
    list2 := tp2;
    last := list2 + nmemb * size;
  end;
  if base = list2 then
  begin
    Move(list1, list2, nmemb * size);
    list2 := list1;
  end;
  FreeMem(list2);
  MergeSort := 0;
end;


{
#define	swap(a, b) {					\
		s = b;					\
		i = size;				\
		do {					\
			tmp = *a; *a++ = *s; *s++ = tmp; \
		} while (--i);				\
		a -= size;				\
	}
#define reverse(bot, top) {				\
	s = top;					\
	do {						\
		i = size;				\
		do {					\
			tmp = *bot; *bot++ = *s; *s++ = tmp; \
		} while (--i);				\
		s -= size2;				\
	} while(bot < s);				\
}
}

{ Optional hybrid natural/pairwise first pass.  Eats up list1 in runs of
  increasing order, list2 in a corresponding linked list.  Checks for runs
  when THRESHOLD/2 pairs compare with same sense.  (Only used when NATURAL
   is defined.  Otherwise simple pairwise merging is used.) }

procedure setup(list1, list2: PByte; n, size: Longint; cmp: TCompareFunction);



var
  i, len, size2, tmp: Longint;
  sense: Boolean;
  f1, f2, s, l2, last, p2: PByte;

procedure swap(var a : pbyte; b : Pbyte); inline;
begin
 s:=b;
 i:=size;
 repeat 
  tmp:=a^;
  a^:=s^;  inc(a);
  s^:=tmp; inc(s);
  dec(i);
 until i=0;
 dec(a,size);
end;

procedure reverse(var bot : pbyte; top:pbyte); inline;
 
begin
 s:=top;
 repeat
   i:=size;
   repeat
     tmp:=bot^;
     bot^:=s^;  inc(bot);
     s^  :=tmp; inc(s);
     dec(i);
   until i=0;
  dec(s,size2);
 until bot>=s;
end;

begin
  size2 := size * 2;
  if n <= 5 then
  begin
    insertionsort(list1, n, size, cmp);
    EVAL(list2)^ := PByte(longint(list2) + n * size);
    exit;
  end;

  { Avoid running pointers out of bounds; limit n to evens
    for simplicity. }
  i := 4 + (n and 1);
  insertionsort(list1 + (n - i) * size, i, size, cmp);
  last := list1 + size * (n - i);
  EVAL(pbyte(longint(list2) + longint(last) - longint(list1)))^ := pbyte(longint(list2) + n *size);

{$ifdef NATURAL}
  p2 := list2;
  f1 := list1;
  sense := cmp(f1, f1 + size) > 0;
  while f1 < last do
  begin
    len := 2;
    // Find pairs with same sense.
    f2 := f1 + size2;
    while f2 < last do
    begin
      if (cmp(f2, f2 + size) > 0) <> sense then
        break;
      Inc(len, 2);
      Inc(f2, size2);
    end;

    sense := not sense;
  end;

  if len < THRESHOLD then
    repeat						// Pairwise merge
      EVAL(p2)^ := pbyte(f1 + size2 - list1 + list2);
      p2 := EVAL(p2)^;
      if sense then
        swap(f1, pbyte(longint(f1) + size));
      Inc(f1, size2);
    until f1 >= f2
  else
  begin							// Natural merge
    l2 := f2;
    f2 := f1 + size2;
    while f2 < l2 do
    begin
      if (cmp(f2 - size, f2) > 0) <> sense then
      begin
        EVAL(p2)^ := pbyte(f2 - list1 + list2);
	p2 := EVAL(p2)^;
	if sense then {!!}
	  reverse(f1, f2 - size);
	f1 := f2;
      end;
      Inc(f2, size2);
    end;
    if sense then
      reverse(f1, f2 - size);
    f1 := f2;
    if (f2 < last) or (cmp(f2 - size, f2) > 0) then
    begin
      EVAL(p2)^ := pbyte(f2 - list1 + list2);
      p2 := EVAL(p2)^;
    end else
    begin
      EVAL(p2)^ := list2 + n * size;
      p2 := EVAL(p2)^;
    end;
  end;

{$ELSE}		// pairwise merge only.
  f1 := list1;
  p2 := list2;
  while f1 < last do
  begin
    EVAL(p2)^ := p2 + size2;
    p2 := EVAL(p2)^;
    if cmp(f1, f1 + size) > 0 then
      swap(f1, f1 + size);

    Inc(f1, size2);
  end;
{$endif  NATURAL }
end;


{ This is to avoid out-of-bounds addresses in sorting the
  last 4 elements. }

procedure insertionsort(a: PByte; n, size: Longint; cmp: TCompareFunction);


var s : pbyte;

var
  ai, t, u: PByte;
  tmp,i: Longint;

procedure swap(var a : pbyte; b : Pbyte); inline;

begin
 s:=b;
 i:=size;
 repeat 
  tmp:=a^;
  a^:=s^;  inc(a);
  s^:=tmp; inc(s);
  dec(i);
 until i=0;
 dec(a,size);
end;

begin
  ai := a + size;
  Dec(n);
  while n >= 1 do
  begin
    t := ai;
    while t > a do
    begin
      u := t - size;
      if cmp(u, t) <= 0 then
        break;
      swap(u, t);

      Dec(t, size);
    end;

    Dec(n);
    Inc(ai, size);
  end;
end;

// get next entry in a directory.

function sys_readdir(dirp: PDir): PDirEnt;
var
  dp: PDirEnt;
begin
  while True do
  begin
    if dirp^.loc >= dirp^.size then
    begin
      if (dirp^.flags and __DTF_READALL) <> 0 then
	exit(nil);
      dirp^.loc := 0;
    end;
    if (dirp^.loc = 0) and  ((dirp^.flags and __DTF_READALL) = 0) then
    begin
      dirp^.size := sys_getdirentries(dirp^.fd,
        pchar(dirp^.buf), dirp^.len, pcardinal(@dirp^.seek));
      if dirp^.size <= 0 then
        exit(nil);
    end;
    dp := PDirEnt(dirp^.buf + dirp^.loc);
    if (LongInt(dp) and 3) <> 0 then	// bogus pointer check
      exit(nil);
    if (dp^.reclen <= 0) or (dp^.reclen > dirp^.len + 1 - dirp^.loc) then
      exit(nil);
    Inc(dirp^.loc, dp^.reclen);
    if dp^.ino = 0 then
      continue;
    if (dp^.d_type = DT_WHT) and ((dirp^.flags and DTF_HIDEW) <> 0) then
      continue;
    exit(dp);
  end;
end;



{ The option SINGLEUSE may be defined to say that a telldir
  cookie may be used only once before it is freed. This option
  is used to avoid having memory usage grow without bound. }

{$define SINGLEUSE}


{ One of these structures is malloced to describe the current directory
  position each time telldir is called. It records the current magic
  cookie returned by getdirentries and the offset within the buffer
  associated with that return value. }

type
  PDDLoc = ^TDDLoc;
  TDDLoc = record
    loc_next: PDDLoc;		// next structure in list
    loc_index: LongInt;		// key associated with structure
    loc_seek: LongInt;		// magic cookie returned by getdirentries
    loc_loc: LongInt;		// offset of entry in buffer
    loc_dirp: PDir;		// directory which used this entry
  end;

const
  NDIRHASH = 32;		// Num of hash lists, must be a power of 2


function LOCHASH(i: Integer): Integer; inline;
begin
  LOCHASH := i and (NDIRHASH - 1);
end;

var
  loccnt: LongInt;		// Index of entry for sequential readdir's
  hash: array[0..NDIRHASH-1] of PDDLoc;	// Hash list heads for ddlocs


// return a pointer into a directory

function telldir(dirp: PDir): LongInt;
var
  index: Integer;
  lp: PDDLoc;
begin
  GetMem(lp, SizeOf(TDDLoc));
  if lp = nil then
    exit(-1);
  index := loccnt;
  Inc(loccnt);
  lp^.loc_index := index;
  lp^.loc_seek := dirp^.seek;
  lp^.loc_loc := dirp^.loc;
  lp^.loc_dirp := dirp;
  lp^.loc_next := hash[LOCHASH(index)];
  hash[LOCHASH(index)] := lp;
  telldir := index;
end;


{ seek to an entry in a directory.
  Only values returned by "telldir" should be passed to seekdir. }

procedure seekdir(dirp: PDir; loc: LongInt);
var
  lp: PDDLoc;
  prevlp: ^PDDLoc;
  dp: PDirEnt;

label found;

begin
  prevlp := @hash[LOCHASH(loc)];
  lp := prevlp^;
  while Assigned(lp) do
  begin
    if lp^.loc_index = loc then
      break;
    prevlp := @lp^.loc_next;
    lp := lp^.loc_next;
  end;
  if not Assigned(lp) then
    exit;
  if (lp^.loc_loc = dirp^.loc) and (lp^.loc_seek = dirp^.seek) then
    goto found;
  sys_lseek(dirp^.fd, lp^.loc_seek, SEEK_SET);
  dirp^.seek := lp^.loc_seek;
  dirp^.loc := 0;
  while dirp^.loc < lp^.loc_loc do
  begin
    dp := sys_readdir(dirp);
    if not Assigned(dp) then
      break;
  end;

found:
{$ifdef SINGLEUSE}
  prevlp^ := lp^.loc_next;
  FreeMem(lp);
{$endif}
end;


// Reclaim memory for telldir cookies which weren't used.

procedure _reclaim_telldir(dirp: PDir);
var
  lp: PDDLoc;
  prevlp: ^PDDLoc;
  i: Integer;
begin
  for i := 0 to NDIRHASH - 1 do
  begin
    prevlp := @hash[i];
    lp := prevlp^;
    while Assigned(lp) do
    begin
      if lp^.loc_dirp = dirp then
      begin
        prevlp^ := lp^.loc_next;
	FreeMem(lp);
	lp := prevlp^;
	continue;
      end;
      prevlp := @lp^.loc_next;
      lp := lp^.loc_next;
    end;
  end;
end;



const
  DIRBLKSIZ = 1024;

function sys_fcntl(Fd:longint;Cmd:longint;Arg:Longint):longint; inline;

begin
 sys_fcntl:=do_syscall(syscall_nr_fcntl,fd,cmd,arg);
end;

function sys_getdirentries(fd:longint;buf:pchar;nbytes:longint;basep:pcardinal):longint;

begin
 sys_getdirentries:=do_syscall(syscall_nr_getdirentries,fd,longint(buf),nbytes,longint(basep));
end;


function strcomp(str1,str2 : pchar) : longint;assembler;
asm
        movl    str2,%edi
        movl    $0xffffffff,%ecx
        cld
        xorl    %eax,%eax
        repne
        scasb
        not     %ecx
        movl    str2,%edi
        movl    str1,%esi
        repe
        cmpsb
        movb    -1(%esi),%al
        movzbl  -1(%edi),%ecx
        subl    %ecx,%eax
end ['EAX','ECX','ESI','EDI'];


{/*
 * Alphabetic order comparison routine for those who want it.
 */}
function alphasort (d1,d2:pointer):longint;
begin
 alphasort:=strcomp(pdirent(d1)^.name,pdirent(d2)^.name);
end;

// strcomp( pdirent(ppdirent(d1)^)^.name, pdirent(ppdirent(d2)^)^.name);


{
	return(strcmp((*(struct dirent **)d1)->d_name,
	    (*(struct dirent **)d2)->d_name));}


function OpenDir2(name: PChar; flags: LongInt): PDir;
var
  dirp        : pdir;
  fd          : longint;
  incr        : longint;
  saved_errno : longint;
  unionstack  : longint;
  statb       : stat;
  bad         : boolean;
  len, space, 
  n	      : longint;
  buf, ddptr, 
  ddeptr      : PChar;
  dpv	      : ^PDirEnt;
  dp, xp      : PDirEnt;

{
	 * stat() before open() because opening of special files may be
	 * harmful.  fstat() after open because the file may have changed.
}	

label fail;

begin
  if sys_stat(name,statb)<>0 THEN
    exit(NIL);
  if not((statb.mode and $f000)=$4000)then
  begin
    errno:=sys_enotdir;
    exit(NIL);
  end;
  fd:=sys_open(name,OPEN_NONBLOCK OR OPEN_RDONLY,438);
  if fd<0 THEN
    exit(NIL);
  dirp:= NIL;
  bad :=false;
  if sys_fstat(fd,statb)<0 then
    bad:=true
   else
   begin
     if not((statb.mode and $f000)=$4000)then {should use linux.isdir if 
						crossinlining works}
     begin
       errno:=sys_enotdir;
       bad:=true;
     end else
     begin
       if (sys_fcntl(fd,F_SETFD,FD_CLOEXEC) = -1) OR (maxavail<sizeof(dirent)) then
       bad:=true;
     end;
   end;
  Incr:=DirBlkSiz;
  If Not Bad then
   begin
    len := 0;
    space := 0;
    buf := nil;
    ddptr := nil;

    { The strategy here is to read all the directory
      entries into a buffer, sort the buffer, and
      remove duplicate entries by setting the inode
      number to zero. }
    repeat
      { Always make at least DIRBLKSIZ bytes
	available to getdirentries }
      if space < DIRBLKSIZ then
      begin
        Inc(space, incr);
        Inc(len, incr);
	ReallocMem(buf, len);
	if not Assigned(buf) then
	  goto fail;
	ddptr := buf + len - space;
      end;
      n := sys_getdirentries(fd, ddptr, space, @dirp^.seek);
      if n > 0 then
      begin
        Inc(ddptr, n);
        Dec(space, n);
      end;
    until n <= 0;

    ddeptr := ddptr;
    flags := flags or __DTF_READALL;

    { Re-open the directory.
      This has the effect of rewinding back to the
      top of the union stack and is needed by
      programs which plan to fchdir to a descriptor
      which has also been read -- see fts.c. }
    if (flags and DTF_REWIND) <> 0 then
    begin
      sys_close(fd);
      fd := sys_open(name, OPEN_RDONLY,438);
      if fd = -1 then
      begin
        saved_errno := errno;
        FreeMem(buf);
	FreeMem(dirp);
	errno := saved_errno;
	exit(nil);
      end;
    end;

    { There is now a buffer full of (possibly) duplicate
      names. }
    dirp^.buf := pdirent(buf);

    { Go round this loop twice...

      Scan through the buffer, counting entries.
      On the second pass, save pointers to each one.
      Then sort the pointers and remove duplicate names. }
    dpv := 0;
    while True do
    begin
      n := 0;
      ddptr := buf;
      while ddptr < ddeptr do
      begin
        dp := PDirEnt(ddptr);
        if (LongInt(dp) and 3) <> 0 then
          break;
        if (dp^.reclen <= 0) or (dp^.reclen > (ddeptr + 1 - ddptr)) then
          break;
        Inc(ddptr, dp^.reclen);
        if dp^.ino <> 0 then
        begin
	  if Assigned(dpv) then
	    dpv[n] := dp;	// !!!: might not compile; objfpc mode can help (?)
	  Inc(n);
	end;
      end;
      if Assigned(dpv) then
      begin
        // This sort must be stable.
        mergesort(dpv, n, sizeof(dpv^), @alphasort);

        dpv[n] := nil;
        xp := nil;

        { Scan through the buffer in sort order,
          zapping the inode number of any
          duplicate names. }
        n := 0;
        while Assigned(dpv[n]) do
        begin
          dp := dpv[n];
          if (not Assigned(xp)) or (strcomp(dp^.name, xp^.name) <> 0) then
            xp := dp
          else
            dp^.ino := 0;
          if (dp^.d_type = DT_WHT) and ((flags and DTF_HIDEW) <> 0) then
            dp^.ino := 0;
          Inc(n);
        end;
        FreeMem(dpv);
        break;
      end else
      begin
        GetMem(dpv, (n + 1) * SizeOf(PDirEnt));
        if not Assigned(dpv) then
	  break;
      end;
    end;

    dirp^.len := len;
    dirp^.size := pchar(ddptr) - pchar(dirp^.buf);
  end else
  begin
    dirp^.len := incr;
    GetMem(dirp^.buf, dirp^.len);
    if not Assigned(dirp^.buf) then
      goto fail;
    dirp^.seek := 0;
    flags := flags and not DTF_REWIND;
  end;

  dirp^.loc := 0;
  dirp^.fd := fd;
  dirp^.flags := flags;

  // Set up seek point for rewinddir.
  dirp^.rewind := telldir(dirp);

  exit(dirp);

fail:
  saved_errno := errno;
  FreeMem(dirp);
  sys_close(fd);
  errno := saved_errno;
  OpenDir2 := nil;
end;

function OpenDir(Name: Pchar): PDir; 
begin
  OpenDir := OpenDir2(Name, DTF_HIDEW or DTF_NODUP);
end;

function closedir(dirp:PDir): Integer;
var
  fd: Longint;
begin
  seekdir(dirp, dirp^.rewind);	// free seekdir storage
  fd := dirp^.fd;
  dirp^.fd := -1;
  dirp^.loc := 0;
  FreeMem(dirp^.buf);
  FreeMem(dirp);
  _reclaim_telldir(dirp);
  closedir :=sys_close(fd);
end;
