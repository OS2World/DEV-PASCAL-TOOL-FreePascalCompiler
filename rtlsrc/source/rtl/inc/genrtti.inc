{
    $Id: genrtti.inc,v 1.1.2.5 2002/11/13 11:58:39 pierre Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by xxxx
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ Run-Time type information routines - processor dependent part }


{$ifndef FPC_SYSTEM_HAS_FPC_INITIALIZE}
Procedure Initialize (Data,TypeInfo : pointer);saveregisters;[Public,Alias : 'FPC_INITIALIZE'];

{ this definition is sometimes (depending on switches)
  already defined or not so define it locally to avoid problems PM }
Type
    Pbyte = ^Byte;
Var Temp       : PByte;
    I          : longint;
    Size,Count : longint;
    TInfo : Pointer;

begin
  Temp:=PByte(TypeInfo);
  case temp^ of
    tkAstring,tkWstring : PPchar(Data)^:=Nil;
    tkArray :
      begin
      temp:=Temp+1;
      I:=temp^;
      temp:=temp+(I+1);               // skip name string;
      Size:=PArrayRec(Temp)^.Size;     // get element size
      Count:=PArrayRec(Temp)^.Count;  // get element Count
      TInfo:=PArrayRec(Temp)^.Info;   // Get element info
      For I:=0 to Count-1 do
        Initialize (Data+(I*size),TInfo);
      end;
    tkrecord,
    tkObject,
    tkClass :
      begin
      Temp:=Temp+1;
      I:=Temp^;
      temp:=temp+(I+1);             // skip name string;
      Size:=PRecRec(Temp)^.Size;    // get record size; not needed.
      Count:=PRecRec(Temp)^.Count;  // get element Count
      For I:=1 to count Do
        With PRecRec(Temp)^.elements[I] do
          Initialize (Data+Offset,Info);
      end;
  end;
end;
{$endif}

{$ifndef FPC_SYSTEM_HAS_FPC_FINALIZE}
Procedure Finalize (Data,TypeInfo: Pointer);saveregisters;[Public,Alias : 'FPC_FINALIZE'];

{ this definition is sometimes (depending on switches)
  already defined or not so define it locally to avoid problems PM }
Type
    Pbyte = ^Byte;
Var Temp       : PByte;
    I          : longint;
    Size,Count : longint;
    TInfo : Pointer;

begin
  Temp:=PByte(TypeInfo);
  case temp^ of
    tkAstring,tkWstring : AnsiStr_Decr_Ref(PPointer(Data)^);
    tkArray :
      begin
      Temp:=Temp+1;
      I:=temp^;
      temp:=temp+(I+1);               // skip name string;
      Size:=PArrayRec(Temp)^.Size;     // get element size
      Count:=PArrayRec(Temp)^.Count;  // get element Count
      TInfo:=PArrayRec(Temp)^.Info;   // Get element info
      For I:=0 to Count-1 do
        Finalize (Data+(I*size),TInfo);
      end;
    tkrecord,
    tkObject,
    tkClass :
      begin
      Temp:=Temp+1;
      I:=Temp^;
      temp:=temp+(I+1);             // skip name string;
      Size:=PRecRec(Temp)^.Size;    // get record size; not needed.
      Count:=PRecRec(Temp)^.Count;  // get element Count
      For I:=1 to count do
        With PRecRec(Temp)^.elements[I] do
          Finalize (Data+Offset,Info);
      end;
  end;
end;
{$endif}

{$ifndef FPC_SYSTEM_HAS_FPC_ADDREF}
Procedure Addref (Data,TypeInfo : Pointer);saveregisters; [Public,alias : 'FPC_ADDREF'];

{ this definition is sometimes (depending on switches)
  already defined or not so define it locally to avoid problems PM }
Type
    Pbyte = ^Byte;
    PPointer = ^Pointer;
Var Temp       : PByte;
    I          : longint;
    Size,Count : longint;
    TInfo : Pointer;

begin
  Temp:=PByte(TypeInfo);
  case temp^ of
    { In case of an ansistring, data is pushed as a var parameter.   }
    { This means that if you look at data as a value parameter, it   }
    { containst the address of the ansistring. AnsiStr_Incr_Ref also }
    { expects a var parameter, so to pass the address of the         }
    { ansistring and not that of the data parameter on the stack,    }
    { you have to dereference data (JM)                              }
    tkAstring,tkWstring : AnsiStr_Incr_Ref(PPointer(Data)^);
    tkArray :
      begin
      Temp:=Temp+1;
      I:=temp^;
      temp:=temp+(I+1);               // skip name string;
      Size:=PArrayRec(Temp)^.Size;     // get element size
      Count:=PArrayRec(Temp)^.Count;  // get element Count
      TInfo:=PArrayRec(Temp)^.Info;   // Get element info
      For I:=0 to Count-1 do
        AddRef (Data+(I*size),TInfo);
      end;
    tkrecord,
    tkObject,
    tkClass :
      begin
      Temp:=Temp+1;
      I:=Temp^;
      temp:=temp+(I+1);             // skip name string;
      Size:=PRecRec(Temp)^.Size;    // get record size; not needed.
      Count:=PRecRec(Temp)^.Count;  // get element Count
      For I:=1 to count do
        With PRecRec(Temp)^.elements[I] do
          AddRef (Data+Offset,Info);
      end;
  end;
end;
{$endif}

{$ifndef FPC_SYSTEM_HAS_FPC_DECREF}
Procedure DecRef (Data, TypeInfo : Pointer);saveregisters;[Public,alias : 'FPC_DECREF'];
{ this definition is sometimes (depending on switches)
  already defined or not so define it locally to avoid problems PM }
Type
    Pbyte = ^Byte;
    PPointer = ^Pointer;
Var Temp       : PByte;
    I          : longint;
    Size,Count : longint;
    TInfo : Pointer;

begin
  Temp:=PByte(TypeInfo);
  case temp^ of
    { see AddRef for comment about below construct (JM) }
    tkAstring,tkWstring : AnsiStr_Decr_Ref(PPointer(Data)^);
    tkArray :
      begin
      Temp:=Temp+1;
      I:=temp^;
      temp:=temp+(I+1);               // skip name string;
      Size:=PArrayRec(Temp)^.Size;     // get element size
      Count:=PArrayRec(Temp)^.Count;  // get element Count
      TInfo:=PArrayRec(Temp)^.Info;   // Get element info
      For I:=0 to Count-1 do
        DecRef (Data+(I*size),TInfo);
      end;
    tkrecord,
    tkObject,
    tkClass :
      begin
      Temp:=Temp+1;
      I:=Temp^;
      temp:=temp+(I+1);             // skip name string;
      Size:=PRecRec(Temp)^.Size;    // get record size; not needed.
      Count:=PRecRec(Temp)^.Count;  // get element Count
      For I:=1 to count do
        With PRecRec(Temp)^.elements[I] do
          DecRef (Data+Offset,Info);
      end;
  end;
end;
{$endif}

{
 $Log: genrtti.inc,v $
 Revision 1.1.2.5  2002/11/13 11:58:39  pierre
  * fix a bug that caused memory holes in exceptions

 Revision 1.1.2.4  2002/11/13 10:36:35  pierre
  + add tkClass and tkObject handling

 Revision 1.1.2.3  2001/06/28 08:00:10  jonas
   * fixed ansistr_incr_ref() and the generic addref()

 Revision 1.1.2.2  2001/05/21 16:44:14  carl
 + more saveregisters required for some routines (compare with 386 asm version)

 Revision 1.1.2.1  2001/04/22 00:43:32  carl
 + generic rtti type information (a bit like generic.inc)


}
