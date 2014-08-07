{
    $Id: temp_gen.pas,v 1.1.2.5 2003/03/24 15:29:19 pierre Exp $
    Copyright (C) 1998-2000 by Florian Klaempfl

    This unit handles the temporary variables stuff for i386

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
unit temp_gen;

  interface

    uses
      cpubase,cpuinfo,cobjects,globals,tree,hcodegen,verbose,files,aasm;

    type
      ttemptype = (tt_none,tt_free,tt_normal,tt_persistant,tt_ansistring,tt_freeansistring,tt_widestring,tt_freewidestring);
      ttemptypeset = set of ttemptype;

      ptemprecord = ^ttemprecord;
      ttemprecord = record
         temptype   : ttemptype;
         pos    : longint;
         size       : longint;
         next       : ptemprecord;
         nextfree   : ptemprecord; { for faster freeblock checking }
{$ifdef EXTDEBUG}
         posinfo,
         releaseposinfo : tfileposinfo;
{$endif}
      end;

    var
      { contains all temps }
      templist      : ptemprecord;
      { contains all free temps using nextfree links }
      tempfreelist  : ptemprecord;
      { Offsets of the first/last temp }
      firsttemp,
      lasttemp      : longint;

    { generates temporary variables }
    procedure resettempgen;
    procedure setfirsttemp(l : longint);
    function gettempsize : longint;
    function newtempofsize(size : longint) : longint;
    function gettempofsize(size : longint) : longint;
    { special call for inlined procedures }
    function gettempofsizepersistant(size : longint) : longint;
    { for parameter func returns }
    function normaltemptopersistant(pos : longint) : boolean;
    procedure persistanttemptonormal(pos : longint);
    {procedure ungettemp(pos : longint;size : longint);}
    procedure ungetpersistanttemp(pos : longint);
    procedure gettempofsizereference(l : longint;var ref : treference);
    function istemp(const ref : treference) : boolean;
    procedure ungetiftemp(const ref : treference);
    function ungetiftempansi(const ref : treference) : boolean;
    procedure gettempansistringreference(var ref : treference);


  implementation

    uses
       scanner,systems,cga;


{$ifdef EXTDEBUG}
    procedure ExtDebugComment(l:longint;s:string);
      begin
        Comment(l,s);
      end;

{$endif EXTDEBUG}

    procedure resettempgen;
      var
         hp : ptemprecord;
      begin
        { Clear the old templist }
        while assigned(templist) do
         begin
{$ifdef EXTDEBUG}
           case templist^.temptype of
             tt_normal :
               ExtDebugComment(V_Warning,'normal temporary assignment of size '+
                       tostr(templist^.size)+' from pos '+tostr(templist^.posinfo.line)+
                       ':'+tostr(templist^.posinfo.column)+
                       ' at pos '+tostr(templist^.pos)+
                       ' not freed at the end of the procedure');
             tt_persistant :
               ExtDebugComment(V_Warning,'persistant temporary assignment of size '+
                       tostr(templist^.size)+' from pos '+tostr(templist^.posinfo.line)+
                       ':'+tostr(templist^.posinfo.column)+
                       ' at pos '+tostr(templist^.pos)+
                       ' not freed at the end of the procedure');
             tt_ansistring :
               ExtDebugComment(V_Warning,'temporary ANSI assignment of size '+
                       tostr(templist^.size)+' from pos '+tostr(templist^.posinfo.line)+
                       ':'+tostr(templist^.posinfo.column)+
                       ' at pos '+tostr(templist^.pos)+
                     ' not freed at the end of the procedure');
           end;
{$endif}
           hp:=templist;
           templist:=hp^.next;
           dispose(hp);
         end;
        templist:=nil;
        tempfreelist:=nil;
        firsttemp:=0;
        lasttemp:=0;
      end;


    procedure setfirsttemp(l : longint);
      begin
         { this is a negative value normally }
         if l < 0 then
          Begin
            if odd(l) then
             Dec(l);
          end
         else
          Begin
            if odd(l) then
             Inc(l);
          end;
         firsttemp:=l;
         lasttemp:=l;
      end;


    function newtempofsize(size : longint) : longint;
      var
        tl : ptemprecord;
      begin
        { we need to allocate at least a minimum of 4 bytes, else
          we get two temps at the same position resulting in problems
          when finding the corresponding temprecord }
        if size=0 then
         size:=4;
        { Just extend the temp, everything below has been use
          already }
        dec(lasttemp,size);
        { now we can create the templist entry }
        new(tl);
        tl^.temptype:=tt_normal;
        tl^.pos:=lasttemp;
        tl^.size:=size;
        tl^.next:=templist;
        tl^.nextfree:=nil;
        templist:=tl;
        newtempofsize:=tl^.pos;
      end;

const
  lasttempofsize : ptemprecord = nil;

    function gettempofsize(size : longint) : longint;
      var
         tl,
         bestslot,bestprev,
         hprev,hp : ptemprecord;
         bestsize,ofs : longint;
      begin
         bestprev:=nil;
         bestslot:=nil;
         tl:=nil;
         bestsize:=0;
{$ifdef EXTDEBUG}
         if size=0 then
          ExtDebugComment(V_Warning,'Temp of size 0 requested');
{$endif}
         { Align needed size on 4 bytes }
         if (size mod 4)<>0 then
           size:=size+(4-(size mod 4));
         { First check the tmpfreelist }
         if assigned(tempfreelist) then
          begin
            { Check for a slot with the same size first }
            hprev:=nil;
            hp:=tempfreelist;
            while assigned(hp) do
             begin
{$ifdef EXTDEBUG}
               if hp^.temptype<>tt_free then
                 ExtDebugComment(V_Warning,'Temp in freelist is not set to tt_free');
{$endif}
               if hp^.size>=size then
                begin
                  { Slot is the same size, then leave immediatly }
                  if hp^.size=size then
                   begin
                     bestprev:=hprev;
                     bestslot:=hp;
                     bestsize:=size;
                     break;
                   end
                  else
                   begin
                     if (bestsize=0) or (hp^.size<bestsize) then
                      begin
                        bestprev:=hprev;
                        bestslot:=hp;
                        bestsize:=hp^.size;
                      end;
                   end;
                end;
               hprev:=hp;
               hp:=hp^.nextfree;
             end;
          end;
         { Reuse an old temp ? }
         if assigned(bestslot) then
          begin
            if bestsize=size then
             begin
               bestslot^.temptype:=tt_normal;
               ofs:=bestslot^.pos;
               tl:=bestslot;
               { Remove from the tempfreelist }
               if assigned(bestprev) then
                 bestprev^.nextfree:=bestslot^.nextfree
               else
                 tempfreelist:=bestslot^.nextfree;
             end
            else
             begin
               { Resize the old block }
               dec(bestslot^.size,size);
               { Create new block and link after bestslot }
               new(tl);
               tl^.temptype:=tt_normal;
               tl^.pos:=bestslot^.pos+bestslot^.size;
               ofs:=tl^.pos;
               tl^.size:=size;
               tl^.nextfree:=nil;
               { link the new block }
               tl^.next:=bestslot^.next;
               bestslot^.next:=tl;
             end;
          end
         else
          begin
             ofs:=newtempofsize(size);
             tl:=templist;
          end;
         lasttempofsize:=tl;
{$ifdef EXTDEBUG}
         tl^.posinfo:=aktfilepos;
{$endif}
         exprasmlist^.concat(new(paitempalloc,alloc(ofs,size)));
         gettempofsize:=ofs;
      end;


    function gettempofsizepersistant(size : longint) : longint;
      var
         l : longint;
      begin
         l:=gettempofsize(size);
         lasttempofsize^.temptype:=tt_persistant;
{$ifdef EXTDEBUG}
         ExtDebugComment(V_Debug,'temp managment  : call to gettempofsizepersistant()'+
                     ' with size '+tostr(size)+' returned '+tostr(l));
         exprasmlist^.concat(new(paitempalloc,allocinfo(lasttempofsize^.pos,lasttempofsize^.size,
           'temp managment  : call to gettempofsizepersistant()'+
           ' with size '+tostr(size)+' returned '+tostr(l))));
{$endif}
         gettempofsizepersistant:=l;
      end;


    function gettempsize : longint;
      var
        _align : longint;
      begin
        { align to 4 bytes at least
          otherwise all those subl $2,%esp are meaningless PM }
        _align:=target_os.stackalignment;
        if _align<4 then
          _align:=4;
        gettempsize:=Align(-lasttemp,_align);
      end;


    procedure gettempofsizereference(l : longint;var ref : treference);
      begin
         { do a reset, because the reference isn't used }
         reset_reference(ref);
         ref.offset:=gettempofsize(l);
         ref.base:=procinfo^.framepointer;
      end;


    procedure gettempansistringreference(var ref : treference);
      var
         foundslot,tl : ptemprecord;
      begin
         { do a reset, because the reference isn't used }
         reset_reference(ref);
         ref.base:=procinfo^.framepointer;
         { Reuse old ansi slot ? }
         foundslot:=nil;
         tl:=templist;
         while assigned(tl) do
          begin
            if tl^.temptype=tt_freeansistring then
             begin
               foundslot:=tl;
{$ifdef EXTDEBUG}
               tl^.posinfo:=aktfilepos;
{$endif}
               break;
             end;
            tl:=tl^.next;
          end;
         if assigned(foundslot) then
          begin
            foundslot^.temptype:=tt_ansistring;
            ref.offset:=foundslot^.pos;
            { we're reusing an old slot then set the function result to true
              so that we can call a decr_ansistr }

            { we never know if a slot was used previously:
              imagine a loop: in the first run the slot wasn't used
              while in later runs it is reused (FK)
            gettempansistringreference:=true;
            }
          end
         else
          begin
            ref.offset:=newtempofsize(target_os.size_of_pointer);
{$ifdef EXTDEBUG}
            templist^.posinfo:=aktfilepos;
{$endif}
            templist^.temptype:=tt_ansistring;
            { set result to false, we don't need an decr_ansistr
              gettempansistringreference:=true;
              Not necessary, the above (FK)
            }
          end;
         exprasmlist^.concat(new(paitempalloc,alloc(ref.offset,target_os.size_of_pointer)));
      end;


    function ungetiftempansi(const ref : treference) : boolean;
      var
         tl : ptemprecord;
      begin
        ungetiftempansi:=false;
        if not istemp(ref) then
          exit;
        tl:=templist;
        while assigned(tl) do
         begin
           if tl^.pos=ref.offset then
            begin
              if tl^.temptype=tt_ansistring then
               begin
                 tl^.temptype:=tt_freeansistring;
                 ungetiftempansi:=true;
                 exprasmlist^.concat(new(paitempalloc,dealloc(tl^.pos,tl^.size)));
                 exit;
{$ifdef EXTDEBUG}
               end
              else if (tl^.temptype=tt_freeansistring) then
               begin
                 ExtDebugComment(V_Debug,'temp ansi managment problem : ungetiftempansi()'+
                     ' at pos '+tostr(ref.offset)+ ' already free !');
                 exprasmlist^.concat(new(paitempalloc,allocinfo(tl^.pos,tl^.size,
                   'temp ansi managment problem : ungetiftempansi()'+
                   ' at pos '+tostr(ref.offset)+ ' already free !')));
{$endif}
               end;
            end;
           tl:=tl^.next;
         end;
      end;

    function istemp(const ref : treference) : boolean;

      begin
         { ref.index = R_NO was missing
           led to problems with local arrays
           with lower bound > 0 (PM) }
         istemp:=((ref.base=procinfo^.framepointer) and
{$ifndef alpha}
                  (ref.index=R_NO) and
{$endif}
                  (ref.offset<firsttemp));
      end;


    procedure persistanttemptonormal(pos : longint);
      var
        hp : ptemprecord;
      begin
         hp:=templist;
         while assigned(hp) do
           if (hp^.pos=pos) and (hp^.temptype=tt_persistant) then
             begin
{$ifdef EXTDEBUG}
               ExtDebugComment(V_Debug,'temp managment : persistanttemptonormal()'+
                  ' at pos '+tostr(pos)+ ' found !');
               exprasmlist^.concat(new(paitempalloc,allocinfo(hp^.pos,hp^.size,
                 'Temp reset from persistant to normal ')));
{$endif}
                hp^.temptype:=tt_normal;
                exit;
             end
           else
             hp:=hp^.next;
{$ifdef EXTDEBUG}
         ExtDebugComment(V_Debug,'temp managment problem : persistanttemptonormal()'+
            ' at pos '+tostr(pos)+ ' not found !');
         exprasmlist^.concat(new(paitempalloc,allocinfo(pos,-1,
           'temp managment problem : persistanttemptonormal()'+
           ' at pos '+tostr(pos)+ ' not found !')));
{$endif}
      end;


    function normaltemptopersistant(pos : longint) : boolean;
      var
        hp : ptemprecord;
      begin
        normaltemptopersistant:=false;
         hp:=templist;
         while assigned(hp) do
           if (hp^.pos=pos) then
             begin
               if (hp^.temptype=tt_normal) then
                 begin
{$ifdef EXTDEBUG}
                   ExtDebugComment(V_Debug,'temp managment : normaltemptopersistant()'+
                     ' at pos '+tostr(pos)+ ' found !');
                    exprasmlist^.concat(new(paitempalloc,allocinfo(hp^.pos,hp^.size,
                     'Temp made persistant ')));
{$endif}
                   hp^.temptype:=tt_persistant;
                   normaltemptopersistant:=true;
                   exit;
                 end
               else
                 begin
{$ifdef EXTDEBUG}
                   ExtDebugComment(V_Debug,'temp managment : normaltemptopersistant()'+
                     ' at pos '+tostr(pos)+ ' already special');
                   exprasmlist^.concat(new(paitempalloc,allocinfo(hp^.pos,hp^.size,
                     'Temp aready persistant in normaltemptopersistant() ')));
{$endif}
                   exit;
                 end;
             end
           else
             hp:=hp^.next;
{$ifdef EXTDEBUG}
         ExtDebugComment(V_Debug,'temp managment problem : normaltemptopersistant()'+
            ' at pos '+tostr(pos)+ ' not found !');
         exprasmlist^.concat(new(paitempalloc,allocinfo(pos,-1,
           'temp managment problem : normaltemptopersistant()'+
           ' at pos '+tostr(pos)+ ' not found !')));
{$endif}
      end;


    function ungettemp(pos:longint;allowtype:ttemptype):ttemptype;
      var
         hp,hnext,hprev,hprevfree : ptemprecord;
      begin
         ungettemp:=tt_none;
         hp:=templist;
         hprev:=nil;
         hprevfree:=nil;
         while assigned(hp) do
          begin
            if (hp^.pos=pos) then
             begin
               { check type }
               ungettemp:=hp^.temptype;
               if hp^.temptype<>allowtype then
                begin
                  exit;
                end;
               exprasmlist^.concat(new(paitempalloc,dealloc(hp^.pos,hp^.size)));
               { set this block to free }
               hp^.temptype:=tt_free;
               { Update tempfreelist }
               if assigned(hprevfree) then
                begin
                  { Connect with previous? }
                  if assigned(hprev) and (hprev^.temptype=tt_free) then
                   begin
                     inc(hprev^.size,hp^.size);
                     hprev^.next:=hp^.next;
                     dispose(hp);
                     hp:=hprev;
                   end
                  else
                   hprevfree^.nextfree:=hp;
                end
               else
                begin
                  hp^.nextfree:=tempfreelist;
                  tempfreelist:=hp;
                end;
               { Next block free ? Yes, then concat }
               hnext:=hp^.next;
               if assigned(hnext) and (hnext^.temptype=tt_free) then
                begin
                  inc(hp^.size,hnext^.size);
                  hp^.nextfree:=hnext^.nextfree;
                  hp^.next:=hnext^.next;
                  dispose(hnext);
                end;
               exit;
             end;
            if (hp^.temptype=tt_free) then
             hprevfree:=hp;
            hprev:=hp;
            hp:=hp^.next;
          end;
         { Check if in persistant area }
         hp:=templist;
         while assigned(hp) do
          begin
            if (hp^.temptype=tt_persistant) and
               (pos>=hp^.pos) and (pos<hp^.pos+hp^.size) then
              begin
                ungettemp:=tt_persistant;
                exit;
              end;
            hp:=hp^.next;
          end;
        ungettemp:=tt_none;
      end;


    procedure ungetpersistanttemp(pos : longint);
      begin
{$ifdef EXTDEBUG}
        if ungettemp(pos,tt_persistant)<>tt_persistant then
          begin
            ExtDebugComment(V_Warning,'temp managment problem : ungetpersistanttemp()'+
                  ' at pos '+tostr(pos)+ ' not found !');
            exprasmlist^.concat(new(paitempalloc,allocinfo(pos,-1,
              'temp managment problem : persistant temp'+
              ' at pos '+tostr(pos)+ ' not found !')));
          end
        else
          exprasmlist^.concat(new(paitempalloc,allocinfo(pos,-1,
            'Persistant temp'+
            ' at pos '+tostr(pos)+ ' released!')));
{$else}
        ungettemp(pos,tt_persistant);
{$endif}
      end;


    procedure ungetiftemp(const ref : treference);
{$ifdef EXTDEBUG}
      var
         tt : ttemptype;
{$endif}
      begin
         if istemp(ref) then
           begin
              { first check if ansistring }
              if ungetiftempansi(ref) then
                exit;
{$ifndef EXTDEBUG}
              ungettemp(ref.offset,tt_normal);
{$else}
              tt:=ungettemp(ref.offset,tt_normal);
              if tt=tt_persistant then
                begin
                  ExtDebugComment(V_Debug,'temp at pos '+tostr(ref.offset)+' not released because persistant!');
                  exprasmlist^.concat(new(paitempalloc,allocinfo(ref.offset,-1,
                    'temp at pos '+tostr(ref.offset)+' not released because persistant!')));
                end;
              if tt=tt_none then
                begin
                  ExtDebugComment(V_Warning,'temp not found for release at offset '+tostr(ref.offset));
                  exprasmlist^.concat(new(paitempalloc,allocinfo(ref.offset,-1,
                    'temp managment problem : temp'+
                    ' at pos '+tostr(ref.offset)+ ' not found !')));
                end;
{$endif}
           end;
      end;


   procedure inittemps;
     begin
       tempfreelist:=nil;
       templist:=nil;
     end;

begin
  InitTemps;
end.
{
  $Log: temp_gen.pas,v $
  Revision 1.1.2.5  2003/03/24 15:29:19  pierre
   + Use ExtDebugComment function to ease debugging of ExtDebug info

  Revision 1.1.2.4  2003/01/13 21:57:46  peter
    * check istemp() in ungetiftempansi

  Revision 1.1.2.3  2002/10/22 10:11:51  pierre
   + generate better temp allocation debug info using taittempalloc.allocinfo

  Revision 1.1.2.2  2001/06/02 19:26:40  peter
    * don't allow a temp of size 0, always allocate atleast 4 bytes

  Revision 1.1.2.1  2001/02/25 02:35:31  carl
  - removed some ifdef cpu

  Revision 1.1  2000/07/13 06:30:00  michael
  + Initial import

  Revision 1.43  2000/05/23 13:55:27  pierre
   Use a multiple of 4 to substract from stack pointer for locals and temps

  Revision 1.42  2000/02/09 13:23:08  peter
    * log truncated

  Revision 1.41  2000/01/07 01:14:47  peter
    * updated copyright to 2000

  Revision 1.40  1999/12/19 23:53:14  pierre
   * problem with persistant temp fixed

  Revision 1.39  1999/12/01 12:42:33  peter
    * fixed bug 698
    * removed some notes about unused vars

  Revision 1.38  1999/11/06 14:34:31  peter
    * truncated log to 20 revs

  Revision 1.37  1999/09/27 23:45:02  peter
    * procinfo is now a pointer
    * support for result setting in sub procedure

  Revision 1.36  1999/09/26 13:26:08  florian
    * exception patch of Romio nevertheless the excpetion handling
      needs some corections regarding register saving
    * gettempansistring is again a procedure

  Revision 1.35  1999/09/16 11:34:59  pierre
   * typo correction

  Revision 1.34  1999/08/04 00:23:46  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.33  1999/08/02 00:34:06  michael
  * alpha has no index

}
