{
    $Id: tokens.pas,v 1.1.2.4 2001/07/23 08:53:05 pierre Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl, Pierre Muller

    Tokens used by the compiler

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
unit tokens;
interface

uses
  globtype;

const
  tokenidlen=14;
  tokheader=#8'Free Pascal Compiler -- Token data'#13#10#26;

type
  ttoken=(NOTOKEN,
    { operators, which can also be overloaded }
    _PLUS,
    _MINUS,
    _STAR,
    _SLASH,
    _EQUAL,
    _GT,
    _LT,
    _GTE,
    _LTE,
    _SYMDIF,
    _STARSTAR,
    _OP_AS,
    _OP_IN,
    _OP_IS,
    _OP_OR,
    _OP_AND,
    _OP_DIV,
    _OP_MOD,
    _OP_NOT,
    _OP_SHL,
    _OP_SHR,
    _OP_XOR,
    _ASSIGNMENT,
    { special chars }
    _CARET,
    _UNEQUAL,
    _LECKKLAMMER,
    _RECKKLAMMER,
    _POINT,
    _COMMA,
    _LKLAMMER,
    _RKLAMMER,
    _COLON,
    _SEMICOLON,
    _KLAMMERAFFE,
    _POINTPOINT,
    _DOUBLEADDR,
    _EOF,
    _ID,
    _NOID,
    _REALNUMBER,
    _INTCONST,
    _CSTRING,
    _CCHAR,
    { C like operators }
    _PLUSASN,
    _MINUSASN,
    _ANDASN,
    _ORASN,
    _STARASN,
    _SLASHASN,
    _MODASN,
    _DIVASN,
    _NOTASN,
    _XORASN,
    { Normal words }
    _AS,
    _AT,
    _DO,
    _IF,
    _IN,
    _IS,
    _OF,
    _ON,
    _OR,
    _TO,
    _AND,
    _ASM,
    _DIV,
    _END,
    _FAR,
    _FOR,
    _MOD,
    _NEW,
    _NIL,
    _NOT,
    _SET,
    _SHL,
    _SHR,
    _TRY,
    _VAR,
    _XOR,
    _CASE,
    _CVAR,
    _ELSE,
    _EXIT,
    _FAIL,
    _FILE,
    _GOTO,
    _NAME,
    _NEAR,
    _READ,
    _SELF,
    _THEN,
    _TRUE,
    _TYPE,
    _UNIT,
    _USES,
    _WITH,
    _ALIAS,
    _ARRAY,
    _BEGIN,
    _BREAK,
    _CDECL,
    _CLASS,
    _CONST,
    _FALSE,
    _INDEX,
    _LABEL,
    _RAISE,
    _UNTIL,
    _WHILE,
    _WRITE,
    _DOWNTO,
    _EXCEPT,
    _EXPORT,
    _INLINE,
    _OBJECT,
    _PACKED,
    _PASCAL,
    _PUBLIC,
    _RECORD,
    _REPEAT,
    _RESULT,
    _STATIC,
    _STORED,
    _STRING,
    _SYSTEM,
    _ASMNAME,
    _DEFAULT,
    _DISPOSE,
    _DYNAMIC,
    _EXPORTS,
    _FINALLY,
    _FORWARD,
    _IOCHECK,
    _LIBRARY,
    _MESSAGE,
    _PRIVATE,
    _PROGRAM,
    _STDCALL,
    _SYSCALL,
    _VIRTUAL,
    _ABSOLUTE,
    _ABSTRACT,
    _CONTINUE,
    _CPPCLASS,
    _EXTERNAL,
    _FUNCTION,
    _OPERATOR,
    _OVERLOAD,
    _OVERRIDE,
    _POPSTACK,
    _PROPERTY,
    _REGISTER,
    _RESIDENT,
    _SAFECALL,
    _ASSEMBLER,
    _INHERITED,
    _INTERFACE,
    _INTERRUPT,
    _NODEFAULT,
    _OTHERWISE,
    _PROCEDURE,
    _PROTECTED,
    _PUBLISHED,
    _THREADVAR,
    _DESTRUCTOR,
    _INTERNPROC,
    _OPENSTRING,
    _CONSTRUCTOR,
    _INTERNCONST,
    _REINTRODUCE,
    _SHORTSTRING,
    _FINALIZATION,
    _SAVEREGISTERS,
    _IMPLEMENTATION,
    _INITIALIZATION,
    _RESOURCESTRING
  );

  tokenrec=record
    str     : string[tokenidlen];
    special : boolean;
    keyword : tmodeswitch;
    op      : ttoken;
  end;

  ttokenarray=array[ttoken] of tokenrec;
  ptokenarray=^ttokenarray;

  tokenidxrec=record
    first,last : ttoken;
  end;

  ptokenidx=^ttokenidx;
  ttokenidx=array[2..tokenidlen,'A'..'Z'] of tokenidxrec;

const
  arraytokeninfo : ttokenarray =(
      (str:''              ;special:true ;keyword:m_none;op:NOTOKEN),
    { Operators which can be overloaded }
      (str:'+'             ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'-'             ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'*'             ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'/'             ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'='             ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'>'             ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'<'             ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'>='            ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'<='            ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'><'            ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'**'            ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'as'            ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'in'            ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'is'            ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'or'            ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'and'           ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'div'           ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'mod'           ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'not'           ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'shl'           ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'shr'           ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'xor'           ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:':='            ;special:true ;keyword:m_none;op:NOTOKEN),
    { Special chars }
      (str:'^'             ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'<>'            ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'['             ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:']'             ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'.'             ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:','             ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'('             ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:')'             ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:':'             ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:';'             ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'@'             ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'..'            ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'@@'            ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'end of file'   ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'identifier'    ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'non identifier';special:true ;keyword:m_none;op:NOTOKEN),
      (str:'const real'    ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'ordinal const' ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'const string'  ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'const char'    ;special:true ;keyword:m_none;op:NOTOKEN),
    { C like operators }
      (str:'+='            ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'-='            ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'&='            ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'|='            ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'*='            ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:'/='            ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:''              ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:''              ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:''              ;special:true ;keyword:m_none;op:NOTOKEN),
      (str:''              ;special:true ;keyword:m_none;op:NOTOKEN),
    { Normal words }
      (str:'AS'            ;special:false;keyword:m_class;op:_OP_AS),
      (str:'AT'            ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'DO'            ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'IF'            ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'IN'            ;special:false;keyword:m_all;op:_OP_IN),
      (str:'IS'            ;special:false;keyword:m_class;op:_OP_IS),
      (str:'OF'            ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'ON'            ;special:false;keyword:m_class;op:NOTOKEN),
      (str:'OR'            ;special:false;keyword:m_all;op:_OP_OR),
      (str:'TO'            ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'AND'           ;special:false;keyword:m_all;op:_OP_AND),
      (str:'ASM'           ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'DIV'           ;special:false;keyword:m_all;op:_OP_DIV),
      (str:'END'           ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'FAR'           ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'FOR'           ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'MOD'           ;special:false;keyword:m_all;op:_OP_MOD),
      (str:'NEW'           ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'NIL'           ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'NOT'           ;special:false;keyword:m_all;op:_OP_NOT),
      (str:'SET'           ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'SHL'           ;special:false;keyword:m_all;op:_OP_SHL),
      (str:'SHR'           ;special:false;keyword:m_all;op:_OP_SHR),
      (str:'TRY'           ;special:false;keyword:m_class;op:NOTOKEN),
      (str:'VAR'           ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'XOR'           ;special:false;keyword:m_all;op:_OP_XOR),
      (str:'CASE'          ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'CVAR'          ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'ELSE'          ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'EXIT'          ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'FAIL'          ;special:false;keyword:m_none;op:NOTOKEN), { only set within constructors PM }
      (str:'FILE'          ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'GOTO'          ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'NAME'          ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'NEAR'          ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'READ'          ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'SELF'          ;special:false;keyword:m_none;op:NOTOKEN), {set inside methods only PM }
      (str:'THEN'          ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'TRUE'          ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'TYPE'          ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'UNIT'          ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'USES'          ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'WITH'          ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'ALIAS'         ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'ARRAY'         ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'BEGIN'         ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'BREAK'         ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'CDECL'         ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'CLASS'         ;special:false;keyword:m_class;op:NOTOKEN),
      (str:'CONST'         ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'FALSE'         ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'INDEX'         ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'LABEL'         ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'RAISE'         ;special:false;keyword:m_class;op:NOTOKEN),
      (str:'UNTIL'         ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'WHILE'         ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'WRITE'         ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'DOWNTO'        ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'EXCEPT'        ;special:false;keyword:m_class;op:NOTOKEN),
      (str:'EXPORT'        ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'INLINE'        ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'OBJECT'        ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'PACKED'        ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'PASCAL'        ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'PUBLIC'        ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'RECORD'        ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'REPEAT'        ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'RESULT'        ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'STATIC'        ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'STORED'        ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'STRING'        ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'SYSTEM'        ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'ASMNAME'       ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'DEFAULT'       ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'DISPOSE'       ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'DYNAMIC'       ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'EXPORTS'       ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'FINALLY'       ;special:false;keyword:m_class;op:NOTOKEN),
      (str:'FORWARD'       ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'IOCHECK'       ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'LIBRARY'       ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'MESSAGE'       ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'PRIVATE'       ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'PROGRAM'       ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'STDCALL'       ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'SYSCALL'       ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'VIRTUAL'       ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'ABSOLUTE'      ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'ABSTRACT'      ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'CONTINUE'      ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'CPPCLASS'      ;special:false;keyword:m_fpc;op:NOTOKEN),
      (str:'EXTERNAL'      ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'FUNCTION'      ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'OPERATOR'      ;special:false;keyword:m_fpc;op:NOTOKEN),
      (str:'OVERLOAD'      ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'OVERRIDE'      ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'POPSTACK'      ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'PROPERTY'      ;special:false;keyword:m_class;op:NOTOKEN),
      (str:'REGISTER'      ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'RESIDENT'      ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'SAFECALL'      ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'ASSEMBLER'     ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'INHERITED'     ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'INTERFACE'     ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'INTERRUPT'     ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'NODEFAULT'     ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'OTHERWISE'     ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'PROCEDURE'     ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'PROTECTED'     ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'PUBLISHED'     ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'THREADVAR'     ;special:false;keyword:m_class;op:NOTOKEN),
      (str:'DESTRUCTOR'    ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'INTERNPROC'    ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'OPENSTRING'    ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'CONSTRUCTOR'   ;special:false;keyword:m_all;op:NOTOKEN),
      (str:'INTERNCONST'   ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'REINTRODUCE'   ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'SHORTSTRING'   ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'FINALIZATION'  ;special:false;keyword:m_initfinal;op:NOTOKEN),
      (str:'SAVEREGISTERS' ;special:false;keyword:m_none;op:NOTOKEN),
      (str:'IMPLEMENTATION';special:false;keyword:m_all;op:NOTOKEN),
      (str:'INITIALIZATION';special:false;keyword:m_initfinal;op:NOTOKEN),
      (str:'RESOURCESTRING';special:false;keyword:m_class;op:NOTOKEN)
  );

var
  tokeninfo:ptokenarray;
  tokenidx:ptokenidx;

procedure inittokens;
procedure donetokens;
procedure create_tokenidx;

implementation

{$ifdef TP}
uses
  dos;
{$endif}

procedure create_tokenidx;
{ create an index with the first and last token for every possible token
  length, so a search only will be done in that small part }
var
  t : ttoken;
  i : longint;
  c : char;
begin
  fillchar(tokenidx^,sizeof(tokenidx^),0);
  for t:=low(ttoken) to high(ttoken) do
   begin
     if not arraytokeninfo[t].special then
      begin
        i:=length(arraytokeninfo[t].str);
        c:=arraytokeninfo[t].str[1];
        if ord(tokenidx^[i,c].first)=0 then
         tokenidx^[i,c].first:=t;
        tokenidx^[i,c].last:=t;
      end;
   end;
end;

procedure inittokens;
{$ifdef TP}
var
  f:file;
  n : namestr;
  d : dirstr;
  e : extstr;
  header:string;
  a:longint;
{$endif TP}
begin
{$ifdef TP}
    fsplit(paramstr(0),d,n,e);
    { when debugging d=''!!!! FK }
    if d='' then
      assign(f,'tokens.dat')
    else
      assign(f,d+'tokens.dat');
    {$I-}
    reset(f,1);
    {We are not sure that the msg file is loaded!}
    if ioresult<>0 then
        begin
            { Very nice indeed !!! PM }
            writeln('Fatal: File tokens.dat not found.');
            close(f);
            halt(3);
        end;
    blockread(f,header,1);
    blockread(f,header[1],length(header));
    blockread(f,a,sizeof(a));
    if (ioresult<>0) or
       (header<>tokheader) or (a<>sizeof(ttokenarray)) then
     begin
       writeln('Fatal: File tokens.dat corrupt.');
       close(f);
       halt(3);
     end;
    new(tokeninfo);
    blockread(f,tokeninfo^,sizeof(ttokenarray));
    new(tokenidx);
    blockread(f,tokenidx^,sizeof(tokenidx^));
    close(f);
{$I+}
    if (ioresult<>0) then
     begin
       writeln('Fatal: File tokens.dat corrupt.');
       halt(3);
     end;
{$else not TP}
  tokeninfo:=@arraytokeninfo;
  new(tokenidx);
  create_tokenidx;
{$endif not TP}
end;


procedure donetokens;
begin
{$ifdef TP}
    dispose(tokeninfo);
{$else TP}
    tokeninfo:=nil;
{$endif TP}
    dispose(tokenidx);
end;

end.
{
  $Log: tokens.pas,v $
  Revision 1.1.2.4  2001/07/23 08:53:05  pierre
   * speed up create_tokenidx

  Revision 1.1.2.3  2001/06/02 19:31:01  peter
    * removed unused field, tokens.dat need still to be regenerated

  Revision 1.1.2.2  2001/05/08 14:40:53  carl
  + reinstated encoded fields, otherwise would crash tp

  Revision 1.1.2.1  2001/05/06 17:12:43  jonas
    - commented out an unused field in tokenrec

  Revision 1.1  2000/07/13 06:30:02  michael
  + Initial import

  Revision 1.24  2000/06/18 18:12:40  peter
    * support overload keyword

  Revision 1.23  2000/06/05 20:41:18  pierre
    + support for NOT overloading
    + unsupported overloaded operators generate errors

  Revision 1.22  2000/03/19 14:56:39  florian
    * bug 873 fixed
    * some cleanup in objectdec

  Revision 1.21  2000/02/12 23:53:18  carl
    * bugfixes in tokens using TP conditional

  Revision 1.20  2000/02/09 13:23:08  peter
    * log truncated

  Revision 1.19  2000/01/07 01:14:48  peter
    * updated copyright to 2000

  Revision 1.18  1999/11/15 17:53:00  pierre
    + one field added for ttoken record for operator
      linking the id to the corresponding operator token that
      can now now all be overloaded
    * overloaded operators are resetted to nil in InitSymtable
      (bug when trying to compile a uint that overloads operators twice)

  Revision 1.17  1999/09/21 20:53:23  florian
    * fixed 1/s problem from mailing list

  Revision 1.16  1999/09/17 09:17:49  peter
    * removed uses globals

  Revision 1.15  1999/09/16 13:41:37  peter
    * better error checking

  Revision 1.14  1999/09/08 16:02:04  peter
    * tokendat compiles for tp
    * tokens.dat supplied by default

  Revision 1.13  1999/09/03 08:37:34  pierre
    *  tokens.dat only used for TP, and also removed from
       compiler dependencies

  Revision 1.12  1999/09/02 18:47:49  daniel
    * Could not compile with TP, some arrays moved to heap
    * NOAG386BIN default for TP
    * AG386* files were not compatible with TP, fixed.

  Revision 1.11  1999/08/04 13:03:17  jonas
    * all tokens now start with an underscore
    * PowerPC compiles!!

  Revision 1.10  1999/08/03 22:03:39  peter
    * moved bitmask constants to sets
    * some other type/const renamings

  Revision 1.9  1999/07/22 09:38:01  florian
    + resourcestring implemented
    + start of longstring support

}
