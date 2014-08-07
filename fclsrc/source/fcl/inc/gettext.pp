{
    $Id: gettext.pp,v 1.7 2003/01/05 15:55:06 florian Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2001 by the Free Pascal development team

    Gettext interface to resourcestrings.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit gettext;

interface

uses SysUtils, Classes;

const
  MOFileHeaderMagic = $950412de;

type

  TMOFileHeader = packed record
    magic: LongWord;             // MOFileHeaderMagic
    revision: LongWord;          // 0
    nstrings: LongWord;          // Number of string pairs
    OrigTabOffset: LongWord;     // Offset of original string offset table
    TransTabOffset: LongWord;    // Offset of translated string offset table
    HashTabSize: LongWord;       // Size of hashing table
    HashTabOffset: LongWord;     // Offset of first hashing table entry
  end;

  TMOStringInfo = packed record
    length: LongWord;
    offset: LongWord;
  end;

  TMOStringTable = array[0..(1 shl 30) div SizeOf(TMOStringInfo)] of TMOStringInfo;
  PMOStringTable = ^TMOStringTable;


  TLongWordArray = array[0..(1 shl 30) div SizeOf(LongWord)] of LongWord;
  PLongWordArray = ^TLongWordArray;

  TPCharArray = array[0..(1 shl 30) div SizeOf(PChar)] of PChar;
  PPCharArray = ^TPCharArray;

  TMOFile = class
  protected
    StringCount, HashTableSize: LongWord;
    HashTable: PLongWordArray;
    OrigTable, TranslTable: PMOStringTable;
    OrigStrings, TranslStrings: PPCharArray;
  public
    constructor Create(const AFilename: String);
    constructor Create(AStream: TStream);
    destructor Destroy; override;
    function Translate(AOrig: PChar; ALen: Integer; AHash: LongWord): String;
    function Translate(AOrig: String; AHash: LongWord): String;
    function Translate(AOrig: String): String;
  end;

  EMOFileError = class(Exception);


  procedure TranslateResourceStrings(AFile: TMOFile);
  procedure TranslateResourceStrings(const AFilename: String);


implementation

uses dos;

var
  GettextUsed: Boolean;


constructor TMOFile.Create(AStream: TStream);
var
  header: TMOFileHeader;
  i: Integer;
  s: String;
begin
  inherited Create;

  AStream.Read(header, Sizeof(header));

  if header.magic <> MOFileHeaderMagic then
    raise EMOFileError.Create('Invalid magic - not a MO file?');

  GetMem(OrigTable, header.nstrings * SizeOf(TMOStringInfo));
  GetMem(TranslTable, header.nstrings * SizeOf(TMOStringInfo));
  GetMem(OrigStrings, header.nstrings * SizeOf(PChar));
  GetMem(TranslStrings, header.nstrings * SizeOf(PChar));


  AStream.Position := header.OrigTabOffset;
  AStream.Read(OrigTable^, header.nstrings * SizeOf(TMOStringInfo));

  AStream.Position := header.TransTabOffset;
  AStream.Read(TranslTable^, header.nstrings * SizeOf(TMOStringInfo));

  StringCount := header.nstrings;

  // Read strings
  for i := 0 to StringCount - 1 do
  begin
    AStream.Position := OrigTable^[i].offset;
{    SetLength(s, OrigTable^[i].length);
    AStream.Read(s[1], OrigTable^[i].length);
    OrigStrings^[i] := StrNew(PChar(s));}
    GetMem(OrigStrings^[i], OrigTable^[i].length + 1);
    AStream.Read(OrigStrings^[i]^, OrigTable^[i].length);
    OrigStrings^[i][OrigTable^[i].length] := #0;
  end;

  for i := 0 to StringCount - 1 do
  begin
    AStream.Position := TranslTable^[i].offset;
{    SetLength(s, TranslTable^[i].length);
    AStream.Read(s[1], TranslTable^[i].length);
    TranslStrings^[i] := StrNew(PChar(s));}
    GetMem(TranslStrings^[i], TranslTable^[i].length+1);
    AStream.Read(TranslStrings^[i]^, TranslTable^[i].length);
    TranslStrings^[i][TranslTable^[i].length] := #0;
  end;

  // Read hashing table
  HashTableSize := header.HashTabSize;
  GetMem(HashTable, 4 * HashTableSize);
  AStream.Position := header.HashTabOffset;
  AStream.Read(HashTable^, 4 * HashTableSize);
end;

constructor TMOFile.Create(const AFilename: String);
var
  f: TStream;
begin
  f := TFileStream.Create(AFilename, fmOpenRead);
  try
    Self.Create(f);
  finally
    f.Free;
  end;
end;

destructor TMOFile.Destroy;
var
  i: Integer;
begin
  for i := 0 to StringCount - 1 do
  begin
    FreeMem(OrigStrings^[i]);
    FreeMem(TranslStrings^[i]);
  end;
  FreeMem(OrigTable);
  FreeMem(TranslTable);
  FreeMem(OrigStrings);
  FreeMem(TranslStrings);
  FreeMem(HashTable);
  inherited Destroy;
end;

function TMOFile.Translate(AOrig: PChar; ALen: Integer; AHash: LongWord): String;
var
  idx, incr, nstr: LongWord;
begin
  idx := AHash mod HashTableSize;
  incr := 1 + (AHash mod (HashTableSize - 2));
  while True do
  begin
    nstr := HashTable^[idx];
    if nstr = 0 then
    begin
      Result := '';
      exit;
    end;
    if (OrigTable^[nstr - 1].length = ALen) and
       (StrComp(OrigStrings^[nstr - 1], AOrig) = 0) then
    begin
      Result := TranslStrings^[nstr - 1];
      exit;
    end;
    if idx >= HashTableSize - incr then
      Dec(idx, HashTableSize - incr)
    else
      Inc(idx, incr);
  end;
end;

function TMOFile.Translate(AOrig: String; AHash: LongWord): String;
begin
  Result := Translate(PChar(AOrig), Length(AOrig), AHash);
end;

function TMOFile.Translate(AOrig: String): String;
begin
  Result := Translate(AOrig, Hash(AOrig));
end;


// -------------------------------------------------------
//   Resourcestring translation procedures
// -------------------------------------------------------

{
  Define USEITERATOR if you want to translate the strings using
  the SetResourceStrings call. This is not recommended for this
  particular iplementation, since we must pass through a global
  variable TheFile : TMOFile. However that works too.
}

{$ifdef USEITERATOR}
var
  Thefile : TMOFile;

function Translate (Name,Value : AnsiString; Hash : Longint) : AnsiString;

begin
  Result:=TheFile.Translate(Value,Hash);
end;

procedure TranslateResourceStrings(AFile: TMOFile);
var
  i,j : Integer;
  s : String;
begin
  TheFile:=AFile;
  SetResourceStrings(@Translate);
end;
{$else}

procedure TranslateResourceStrings(AFile: TMOFile);
var
  i, j, count: Integer;
  s: String;
begin
  for i:=0 to ResourceStringTableCount - 1 do
  begin
    count := ResourceStringCount(I);
    for j := 0 to count - 1 do
    begin
      s := AFile.Translate(GetResourceStringDefaultValue(i, j),
        GetResourceStringHash(i, j));
      if Length(s) > 0 then
      begin
        SetResourceStringValue(i, j, s);
        GettextUsed := True;
      end;
    end;
  end;
end;
{$endif}

procedure TranslateResourceStrings(const AFilename: String);
var
  mo: TMOFile;
  lang, FallbackLanguage: String;
begin
  lang := GetEnv('LC_ALL');
  if Length(lang) = 0 then
  begin
    lang := GetEnv('LC_MESSAGES');
    if Length(lang) = 0 then
    begin
      lang := GetEnv('LANG');
      if Length(lang) = 0 then
        exit;   // no language defined via environment variables
    end;
  end;

  FallbackLanguage := Copy(lang, 1, 2);
  try
    mo := TMOFile.Create(Format(AFilename, [FallbackLanguage]));
    try
      TranslateResourceStrings(mo);
    finally
      mo.Free;
    end;
  except
    on e: Exception do;
  end;

  lang := Copy(lang, 1, 5);
  try
    mo := TMOFile.Create(Format(AFilename, [lang]));
    try
      TranslateResourceStrings(mo);
    finally
      mo.Free;
    end;
  except
    on e: Exception do;
  end;
end;

finalization
  if GettextUsed then
    ResetResourceTables;
end.


{
  $Log: gettext.pp,v $
  Revision 1.7  2003/01/05 15:55:06  florian
    * fixed type = type ...; statement for 1.1

  Revision 1.6  2002/09/07 15:15:24  peter
    * old logs removed and tabs fixed

  Revision 1.5  2002/01/19 11:54:52  peter
    * fixed wrong getmem

}
