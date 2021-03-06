{
    $Id: streams.inc,v 1.12 2002/04/25 19:14:13 sg Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{****************************************************************************}
{*                             TStream                                      *}
{****************************************************************************}

{$ifdef seek64bit}
  function TStream.GetPosition: Int64;

    begin
       Result:=Seek(0,soCurrent);
    end;

  procedure TStream.SetPosition(Pos: Int64);

    begin
       Seek(pos,soBeginning);
    end;

  procedure TStream.SetSize64(NewSize: Int64);

    begin
      // Required because can't use overloaded functions in properties
      SetSize(NewSize);
    end;

  function TStream.GetSize: Int64;

    var
       p : longint;

    begin
       p:=GetPosition;
       GetSize:=Seek(0,soEnd);
       Seek(p,soBeginning);
    end;

  procedure TStream.SetSize(NewSize: Longint);

    begin
    // We do nothing. Pipe streams don't support this
    // As wel as possible read-ony streams !!
    end;

  procedure TStream.SetSize(NewSize: Int64);

    begin
      // Backwards compatibility that calls the longint SetSize
      if (NewSize<Low(longint)) or
         (NewSize>High(longint)) then
        raise ERangeError.Create(SRangeError);
      SetSize(longint(NewSize));
    end;

  function TStream.Seek(Offset: Longint; Origin: Word): Longint;

    type
      TSeek64 = function(offset:Int64;Origin:TSeekorigin):Int64 of object;
    var
      CurrSeek,
      TStreamSeek : TSeek64;
      CurrClass   : TClass;
    begin
      // Redirect calls to 64bit Seek, but we can't call the 64bit Seek
      // from TStream, because then we end up in an infinite loop
      CurrSeek:=nil;
      CurrClass:=Classtype;
      while (CurrClass<>nil) and
            (CurrClass<>TStream) do
       CurrClass:=CurrClass.Classparent;
      if CurrClass<>nil then
       begin
         CurrSeek:=@Self.Seek;
         TStreamSeek:=@TStream(@CurrClass).Seek;
         if TMethod(TStreamSeek).Code=TMethod(CurrSeek).Code then
          CurrSeek:=nil;
       end;
      if CurrSeek<>nil then
       Result:=Seek(Int64(offset),TSeekOrigin(origin))
      else
       raise EStreamError.CreateFmt(SSeekNotImplemented,[ClassName]);
    end;

  function TStream.Seek(Offset: Int64; Origin: TSeekorigin): Int64;

    begin
      // Backwards compatibility that calls the longint Seek
      if (Offset<Low(longint)) or
         (Offset>High(longint)) then
        raise ERangeError.Create(SRangeError);
      Result:=Seek(longint(Offset),ord(Origin));
    end;

{$else seek64bit}

  function TStream.GetPosition: Longint;

    begin
       Result:=Seek(0,soFromCurrent);
    end;

  procedure TStream.SetPosition(Pos: Longint);

    begin
       Seek(pos,soFromBeginning);
    end;

  function TStream.GetSize: Longint;

    var
       p : longint;

    begin
       p:=GetPosition;
       GetSize:=Seek(0,soFromEnd);
       Seek(p,soFromBeginning);
    end;

  procedure TStream.SetSize(NewSize: Longint);

    begin
    // We do nothing. Pipe streams don't support this
    // As wel as possible read-ony streams !!
    end;

{$endif seek64bit}

  procedure TStream.ReadBuffer(var Buffer; Count: Longint);

    begin
       if Read(Buffer,Count)<Count then
         Raise EReadError.Create(SReadError);
    end;

  procedure TStream.WriteBuffer(const Buffer; Count: Longint);

    begin
       if Write(Buffer,Count)<Count then
         Raise EWriteError.Create(SWriteError);
    end;

  function TStream.CopyFrom(Source: TStream; Count: Int64): Int64;

    var
       i : Int64;
       buffer : array[0..1023] of byte;

    begin
       CopyFrom:=0;
       while Count>0 do
         begin
            if (Count>sizeof(buffer)) then
              i:=sizeof(Buffer)
            else
              i:=Count;
            i:=Source.Read(buffer,i);
            i:=Write(buffer,i);
            dec(count,i);
            CopyFrom:=CopyFrom+i;
            if i=0 then
              exit;
         end;
    end;

  function TStream.ReadComponent(Instance: TComponent): TComponent;

    var
      Reader: TReader;

    begin

      Reader := TReader.Create(Self, 4096);
      try
        Result := Reader.ReadRootComponent(Instance);
      finally
        Reader.Free;
      end;

    end;

  function TStream.ReadComponentRes(Instance: TComponent): TComponent;

    begin

      ReadResHeader;
      Result := ReadComponent(Instance);

    end;

  procedure TStream.WriteComponent(Instance: TComponent);

    begin

      WriteDescendent(Instance, nil);

    end;

  procedure TStream.WriteComponentRes(const ResName: string; Instance: TComponent);

    begin

      WriteDescendentRes(ResName, Instance, nil);

    end;

  procedure TStream.WriteDescendent(Instance, Ancestor: TComponent);

    var
       Driver : TAbstractObjectWriter;
       Writer : TWriter;

    begin

       Driver := TBinaryObjectWriter.Create(Self, 4096);
       Try
         Writer := TWriter.Create(Driver);
         Try
           Writer.WriteDescendent(Instance, Ancestor);
         Finally
           Writer.Destroy;
         end;
       Finally
         Driver.Free;
       end;

    end;

  procedure TStream.WriteDescendentRes(const ResName: string; Instance, Ancestor: TComponent);

    var
      FixupInfo: Integer;

    begin

      { Write a resource header }
      WriteResourceHeader(ResName, FixupInfo);
      { Write the instance itself }
      WriteDescendent(Instance, Ancestor);
      { Insert the correct resource size into the resource header }
      FixupResourceHeader(FixupInfo);

    end;

  procedure TStream.WriteResourceHeader(const ResName: string; {!!!: out} var FixupInfo: Integer);

    begin
       { Numeric resource type }
       WriteByte($ff);
       { Application defined data }
       WriteWord($0a);
       { write the name as asciiz }
       WriteBuffer(ResName[1],length(ResName));
       WriteByte(0);
       { Movable, Pure and Discardable }
       WriteWord($1030);
       { Placeholder for the resource size }
       WriteDWord(0);
       { Return current stream position so that the resource size can be
         inserted later }
       FixupInfo := Position;
    end;

  procedure TStream.FixupResourceHeader(FixupInfo: Integer);

    var
       ResSize : Integer;

    begin

      ResSize := Position - FixupInfo;

      { Insert the correct resource size into the placeholder written by
        WriteResourceHeader }
      Position := FixupInfo - 4;
      WriteDWord(ResSize);
      { Seek back to the end of the resource }
      Position := FixupInfo + ResSize;

    end;

  procedure TStream.ReadResHeader;

    begin
       try
         { application specific resource ? }
         if ReadByte<>$ff then
           raise EInvalidImage.Create(SInvalidImage);
         if ReadWord<>$000a then
           raise EInvalidImage.Create(SInvalidImage);
         { read name }
         while ReadByte<>0 do
           ;
         { check the access specifier }
         if ReadWord<>$1030 then
           raise EInvalidImage.Create(SInvalidImage);
         { ignore the size }
         ReadDWord;
       except
         on EInvalidImage do
           raise;
         else
           raise EInvalidImage.create(SInvalidImage);
       end;
    end;

  function TStream.ReadByte : Byte;

    var
       b : Byte;

    begin
       ReadBuffer(b,1);
       ReadByte:=b;
    end;

  function TStream.ReadWord : Word;

    var
       w : Word;

    begin
       ReadBuffer(w,2);
       ReadWord:=w;
    end;

  function TStream.ReadDWord : Cardinal;

    var
       d : Cardinal;

    begin
       ReadBuffer(d,4);
       ReadDWord:=d;
    end;

  Function TStream.ReadAnsiString : String;
  Type
    PByte = ^Byte;
  Var
    TheSize : Longint;
    P : PByte ;
  begin
    ReadBuffer (TheSize,SizeOf(TheSize));
    SetLength(Result,TheSize);
    // Illegal typecast if no AnsiStrings defined.
    if TheSize>0 then
     begin
       ReadBuffer (Pointer(Result)^,TheSize);
       P:=Pointer(Result)+TheSize;
       p^:=0;
     end;
   end;

  Procedure TStream.WriteAnsiString (S : String);

  Var L : Longint;

  begin
    L:=Length(S);
    WriteBuffer (L,SizeOf(L));
    WriteBuffer (Pointer(S)^,L);
  end;

  procedure TStream.WriteByte(b : Byte);

    begin
       WriteBuffer(b,1);
    end;

  procedure TStream.WriteWord(w : Word);

    begin
       WriteBuffer(w,2);
    end;

  procedure TStream.WriteDWord(d : Cardinal);

    begin
       WriteBuffer(d,4);
    end;


{****************************************************************************}
{*                             THandleStream                                *}
{****************************************************************************}

Constructor THandleStream.Create(AHandle: Integer);

begin
  FHandle:=AHandle;
end;


function THandleStream.Read(var Buffer; Count: Longint): Longint;

begin
  Result:=FileRead(FHandle,Buffer,Count);
  If Result=-1 then Result:=0;
end;


function THandleStream.Write(const Buffer; Count: Longint): Longint;

begin
  Result:=FileWrite (FHandle,Buffer,Count);
  If Result=-1 then Result:=0;
end;

{$ifdef seek64bit}

Procedure THandleStream.SetSize(NewSize: Longint);

begin
  SetSize(Int64(NewSize));
end;


Procedure THandleStream.SetSize(NewSize: Int64);

begin
  FileTruncate(FHandle,NewSize);
end;


function THandleStream.Seek(Offset: Int64; Origin: TSeekOrigin): Int64;

begin
  Result:=FileSeek(FHandle,Offset,ord(Origin));
end;

{$else seek64bit}

Procedure THandleStream.SetSize(NewSize: Longint);
begin
  FileTruncate(FHandle,NewSize);
end;


function THandleStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result:=FileSeek(FHandle,Offset,Origin);
end;

{$endif seek64bit}


{****************************************************************************}
{*                             TFileStream                                  *}
{****************************************************************************}

constructor TFileStream.Create(const AFileName: string; Mode: Word);

begin
  FFileName:=AFileName;
  If Mode=fmcreate then
    FHandle:=FileCreate(AFileName)
  else
    FHAndle:=FileOpen(AFileName,Mode);
  If FHandle<0 then
    If Mode=fmcreate then
      raise EFCreateError.createfmt(SFCreateError,[AFileName])
    else
      raise EFOpenError.Createfmt(SFOpenError,[AFilename]);
end;


constructor TFileStream.Create(const AFileName: string; Mode: Word; Rights: Cardinal);

begin
  FFileName:=AFileName;
  If Mode=fmcreate then
    FHandle:=FileCreate(AFileName)
  else
    FHAndle:=FileOpen(AFileName,Mode);
  If FHandle<0 then
    If Mode=fmcreate then
      raise EFCreateError.createfmt(SFCreateError,[AFileName])
    else
      raise EFOpenError.Createfmt(SFOpenError,[AFilename]);
end;


destructor TFileStream.Destroy;

begin
  FileClose(FHandle);
end;

{****************************************************************************}
{*                             TCustomMemoryStream                          *}
{****************************************************************************}

procedure TCustomMemoryStream.SetPointer(Ptr: Pointer; ASize: Longint);

begin
  FMemory:=Ptr;
  FSize:=ASize;
end;


function TCustomMemoryStream.Read(var Buffer; Count: Longint): Longint;

begin
  Result:=0;
  If (FSize>0) and (FPosition<Fsize) then
    begin
    Result:=FSize-FPosition;
    If Result>Count then Result:=Count;
    Move ((FMemory+FPosition)^,Buffer,Result);
    FPosition:=Fposition+Result;
    end;
end;


function TCustomMemoryStream.Seek(Offset: Longint; Origin: Word): Longint;

begin
  Case Origin of
    soFromBeginning : FPosition:=Offset;
    soFromEnd       : FPosition:=FSize+Offset;
    soFromCurrent   : FpoSition:=FPosition+Offset;
  end;
  Result:=FPosition;
end;


procedure TCustomMemoryStream.SaveToStream(Stream: TStream);

begin
  if FSize>0 then Stream.WriteBuffer (FMemory^,FSize);
end;


procedure TCustomMemoryStream.SaveToFile(const FileName: string);

Var S : TFileStream;

begin
  Try
    S:=TFileStream.Create (FileName,fmCreate);
    SaveToStream(S);
  finally
    S.free;
  end;
end;


{****************************************************************************}
{*                             TMemoryStream                                *}
{****************************************************************************}


Const TMSGrow = 4096; { Use 4k blocks. }

procedure TMemoryStream.SetCapacity(NewCapacity: Longint);

begin
  SetPointer (Realloc(NewCapacity),Fsize);
  FCapacity:=NewCapacity;
end;


function TMemoryStream.Realloc(var NewCapacity: Longint): Pointer;

Var MoveSize : Longint;

begin
  If NewCapacity>0 Then // round off to block size.
    NewCapacity := (NewCapacity + (TMSGrow-1)) and not (TMSGROW-1);
  // Only now check !
  If NewCapacity=FCapacity then
    Result:=FMemory
  else
    If NewCapacity=0 then
      FreeMem (FMemory,Fcapacity)
    else
      begin
      GetMem (Result,NewCapacity);
      If Result=Nil then
        Raise EStreamError.Create(SMemoryStreamError);
      If FCapacity>0 then
        begin
        MoveSize:=FSize;
        If MoveSize>NewCapacity then MoveSize:=NewCapacity;
        Move (Fmemory^,Result^,MoveSize);
        FreeMem (FMemory,FCapacity);
        end;
      end;
end;


destructor TMemoryStream.Destroy;

begin
  Clear;
  Inherited Destroy;
end;


procedure TMemoryStream.Clear;

begin
  FSize:=0;
  FPosition:=0;
  SetCapacity (0);
end;


procedure TMemoryStream.LoadFromStream(Stream: TStream);

begin
  Stream.Position:=0;
  SetSize(Stream.Size);
  If FSize>0 then Stream.ReadBuffer(FMemory^,FSize);
end;


procedure TMemoryStream.LoadFromFile(const FileName: string);

Var S : TFileStream;

begin
  S:=TFileStream.Create (FileName,fmOpenRead);
  Try
    LoadFromStream(S);
  finally
    S.free;
  end;
end;


procedure TMemoryStream.SetSize(NewSize: Longint);

begin
  SetCapacity (NewSize);
  FSize:=NewSize;
  IF FPosition>FSize then
    FPosition:=FSize;
end;


function TMemoryStream.Write(const Buffer; Count: Longint): Longint;

Var NewPos : Longint;

begin
  If Count=0 then
    exit(0);
  NewPos:=FPosition+Count;
  If NewPos>Fsize then
    begin
    IF NewPos>FCapacity then
      SetCapacity (NewPos);
    FSize:=Newpos;
    end;
  System.Move (Buffer,(FMemory+FPosition)^,Count);
  FPosition:=NewPos;
  Result:=Count;
end;


{****************************************************************************}
{*                             TStringStream                                *}
{****************************************************************************}

procedure TStringStream.SetSize(NewSize: Longint);

begin
 Setlength(FDataString,NewSize);
 If FPosition>NewSize then FPosition:=NewSize;
end;


constructor TStringStream.Create(const AString: string);

begin
  Inherited create;
  FDataString:=AString;
end;


function TStringStream.Read(var Buffer; Count: Longint): Longint;

begin
  Result:=Length(FDataString)-FPosition;
  If Result>Count then Result:=Count;
  // This supposes FDataString to be of type AnsiString !
  Move (Pchar(FDataString)[FPosition],Buffer,Result);
  FPosition:=FPosition+Result;
end;


function TStringStream.ReadString(Count: Longint): string;

Var NewLen : Longint;

begin
  NewLen:=Length(FDataString)-FPosition;
  If NewLen>Count then NewLen:=Count;
  SetLength(Result,NewLen);
  Read (Pointer(Result)^,NewLen);
  ReadString:='';
end;


function TStringStream.Seek(Offset: Longint; Origin: Word): Longint;

begin
  Case Origin of
    soFromBeginning : FPosition:=Offset;
    soFromEnd       : FPosition:=Length(FDataString)+Offset;
    soFromCurrent   : FpoSition:=FPosition+Offset;
  end;
  If FPosition>Length(FDataString) then FPosition:=Length(FDataString);
  If FPosition<0 then FPosition:=0;
  Result:=FPosition;
end;


function TStringStream.Write(const Buffer; Count: Longint): Longint;

begin
  Result:=Count;
  SetSize(FPosition+Count);
  // This supposes that FDataString is of type AnsiString)
  Move (Buffer,PCHar(FDataString)[Fposition],Count);
  FPosition:=FPosition+Count;
end;


procedure TStringStream.WriteString(const AString: string);

begin
  Write (PChar(Astring)[0],Length(AString));
end;



{****************************************************************************}
{*                             TResourceStream                              *}
{****************************************************************************}

procedure TResourceStream.Initialize(Instance: THandle; Name, ResType: PChar);

begin
end;


constructor TResourceStream.Create(Instance: THandle; const ResName: string; ResType: PChar);

begin
end;


constructor TResourceStream.CreateFromID(Instance: THandle; ResID: Integer; ResType: PChar);

begin
end;


destructor TResourceStream.Destroy;

begin
end;


function TResourceStream.Write(const Buffer; Count: Longint): Longint;

begin
  Write:=0;
end;


{
  $Log: streams.inc,v $
  Revision 1.12  2002/04/25 19:14:13  sg
  * Fixed TStringStream.ReadString

  Revision 1.11  2002/12/18 16:45:33  peter
    * set function result in TStream.Seek(int64) found by Mattias Gaertner

  Revision 1.10  2002/12/18 16:35:59  peter
    * fix crash in Seek()

  Revision 1.9  2002/12/18 15:51:52  michael
  + Hopefully fixed some issues with int64 seek

  Revision 1.8  2002/10/22 09:38:39  michael
  + Fixed TmemoryStream.LoadFromStream, reported by Mattias Gaertner

  Revision 1.7  2002/09/07 15:15:25  peter
    * old logs removed and tabs fixed

 }
