{
    $Id: wini.pas,v 1.3 2002/09/07 15:40:50 peter Exp $
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by B�rczi G�bor

    Reading and writing .INI files

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit WINI;

interface

uses Objects;

type

    PINIEntry = ^TINIEntry;
    TINIEntry = object(TObject)
      constructor Init(const ALine: string);
      function    GetText: string;
      function    GetTag: string;
      function    GetComment: string;
      function    GetValue: string;
      procedure   SetValue(const S: string);
      destructor  Done; virtual;
    private
      Tag      : PString;
      Value    : PString;
      Comment  : PString;
      Text     : PString;
      Modified : boolean;
      procedure Split;
    end;

    PINISection = ^TINISection;
    TINISection = object(TObject)
      constructor Init(const AName: string);
      function    GetName: string;
      function    AddEntry(const S: string): PINIEntry;
      function    SearchEntry(Tag: string): PINIEntry; virtual;
      procedure   DeleteEntry(Tag: string);
      procedure   ForEachEntry(EnumProc: pointer); virtual;
      destructor  Done; virtual;
    private
      Name     : PString;
      Entries  : PCollection;
    end;

    PINIFile = ^TINIFile;
    TINIFile = object(TObject)
      MakeNullEntries: boolean;
      constructor Init(const AFileName: string);
      function    GetFileName: string;
      function    Read: boolean; virtual;
      function    Update: boolean; virtual;
      function    IsModified: boolean; virtual;
      function    SearchSection(Section: string): PINISection; virtual;
      function    SearchEntry(const Section, Tag: string): PINIEntry; virtual;
      procedure   ForEachSection(EnumProc: pointer); virtual;
      procedure   ForEachEntry(const Section: string; EnumProc: pointer); virtual;
      function    GetEntry(const Section, Tag, Default: string): string; virtual;
      procedure   SetEntry(const Section, Tag, Value: string); virtual;
      function    GetIntEntry(const Section, Tag: string; Default: longint): longint; virtual;
      procedure   SetIntEntry(const Section, Tag: string; Value: longint); virtual;
      procedure   DeleteSection(const Section: string); virtual;
      procedure   DeleteEntry(const Section, Tag: string);
      destructor  Done; virtual;
    private
{      ReadOnly: boolean;}
      Sections: PCollection;
      FileName: PString;
    end;

const MainSectionName : string[40] = 'MainSection';
      CommentChar     : char = ';';
      ValidStrDelimiters: set of char = ['''','"'];

implementation

uses
  CallSpec,
  WUtils;

constructor TINIEntry.Init(const ALine: string);
begin
  inherited Init;
  Text:=NewStr(ALine);
  Split;
end;


function TINIEntry.GetText: string;
var S,CoS: string;
begin
  if Text=nil then
    begin
      CoS:=GetComment;
      S:=GetTag+'='+GetValue;
      if Trim(S)='=' then S:=CoS else
        if CoS<>'' then S:=S+' '+CommentChar+' '+CoS;
    end
    else S:=Text^;
  GetText:=S;
end;


function TINIEntry.GetTag: string;
begin
  GetTag:=GetStr(Tag);
end;


function TINIEntry.GetComment: string;
begin
  GetComment:=GetStr(Comment);
end;


function TINIEntry.GetValue: string;
begin
  GetValue:=GetStr(Value);
end;


procedure TINIEntry.SetValue(const S: string);
begin
  if GetValue<>S then
  begin
    if Text<>nil then DisposeStr(Text); Text:=nil;
    if Value<>nil then DisposeStr(Value);
    Value:=NewStr(S);
    Modified:=true;
  end;
end;


procedure TINIEntry.Split;
var S,ValueS: string;
    P,P2,StartP: longint;
    { using byte for P2 lead to infinite loops PM }
    C: char;
    InString: boolean;
    Delimiter: char;
begin
  S:=GetText; Delimiter:=#0;
  P:=Pos('=',S); P2:=Pos(CommentChar,S);
  if (P2<>0) and (P2<P) then P:=0;
  if P<>0 then
    begin
      Tag:=NewStr(copy(S,1,P-1));
      P2:=P+1; InString:=false; ValueS:='';
      StartP:=P2;
      while (P2<=length(S)) do
        begin
          C:=S[P2];
          if (P2=StartP) and (C in ValidStrDelimiters) then begin Delimiter:=C; InString:=true; end else
          if C=Delimiter then InString:=not InString else
          if (C=CommentChar) and (InString=false) then Break else
          ValueS:=ValueS+C;
          Inc(P2);
        end;
      Value:=NewStr(Trim(ValueS));
      Comment:=NewStr(copy(S,P2+1,High(S)));
    end else
    begin
      Tag:=nil;
      Value:=nil;
      Comment:=NewStr(S);
    end;
end;


destructor TINIEntry.Done;
begin
  inherited Done;
  if Text<>nil then DisposeStr(Text);
  if Tag<>nil then DisposeStr(Tag);
  if Value<>nil then DisposeStr(Value);
  if Comment<>nil then DisposeStr(Comment);
end;


constructor TINISection.Init(const AName: string);
begin
  inherited Init;
  Name:=NewStr(AName);
  New(Entries, Init(50,500));
end;


function TINISection.GetName: string;
begin
  GetName:=GetStr(Name);
end;

function TINISection.AddEntry(const S: string): PINIEntry;
var E: PINIEntry;
begin
  New(E, Init(S));
  Entries^.Insert(E);
  AddEntry:=E;
end;

procedure TINIFile.ForEachSection(EnumProc: pointer);
var I: Sw_integer;
   S: PINISection;
begin
  for I:=0 to Sections^.Count-1 do
    begin
      S:=Sections^.At(I);
      CallPointerLocal(EnumProc,PreviousFramePointer,S);
    end;
end;

procedure TINISection.ForEachEntry(EnumProc: pointer);
var I: integer;
    E: PINIEntry;
begin
  for I:=0 to Entries^.Count-1 do
    begin
      E:=Entries^.At(I);
      CallPointerLocal(EnumProc,PreviousFramePointer,E);
    end;
end;

function TINISection.SearchEntry(Tag: string): PINIEntry;
function MatchingEntry(E: PINIEntry): boolean; {$ifndef FPC}far;{$endif}
begin
  MatchingEntry:=UpcaseStr(E^.GetTag)=Tag;
end;
begin
  Tag:=UpcaseStr(Tag);
  SearchEntry:=Entries^.FirstThat(@MatchingEntry);
end;

procedure TINISection.DeleteEntry(Tag: string);
var
  P : PIniEntry;
begin
  P:=SearchEntry(Tag);
  if assigned(P) then
    Entries^.Free(P);
end;

destructor TINISection.Done;
begin
  inherited Done;
  if Name<>nil then DisposeStr(Name);
  Dispose(Entries, Done);
end;


constructor TINIFile.Init(const AFileName: string);
begin
  inherited Init;
  FileName:=NewStr(AFileName);
  New(Sections, Init(50,50));
  Read;
end;

function TINIFile.GetFileName: string;
begin
  GetFileName:=GetStr(FileName);
end;

function TINIFile.Read: boolean;
var f: text;
    OK: boolean;
    S,TS: string;
    P: PINISection;
    I: integer;
begin
  New(P, Init(MainSectionName));
  Sections^.Insert(P);
  Assign(f,FileName^);
{$I-}
  Reset(f);
  OK:=EatIO=0;
  while OK and (Eof(f)=false) do
    begin
      readln(f,S);
      TS:=Trim(S);
      OK:=EatIO=0;
      if OK then
      if TS<>'' then
      if copy(TS,1,1)='[' then
      begin
        I:=Pos(']',TS); if I=0 then I:=length(TS)+1;
        New(P, Init(copy(TS,2,I-2)));
        Sections^.Insert(P);
      end else
      begin
        P^.AddEntry(S);
      end;
    end;
  Close(f);
  EatIO;
{$I+}
  Read:=true;
end;

function TINIFile.IsModified: boolean;

  function SectionModified(P: PINISection): boolean; {$ifndef FPC}far;{$endif}

    function EntryModified(E: PINIEntry): boolean; {$ifndef FPC}far;{$endif}
    begin
      EntryModified:=E^.Modified;
    end;

  begin
    SectionModified:=(P^.Entries^.FirstThat(@EntryModified)<>nil);
  end;

begin
  IsModified:=(Sections^.FirstThat(@SectionModified)<>nil);
end;


function TINIFile.Update: boolean;
var f: text;
    OK: boolean;
    P: PINISection;
    E: PINIEntry;
    I,J: integer;
begin
  Assign(f,FileName^);
{$I-}
  Rewrite(f);
  OK:=EatIO=0;
  if OK then
  for I:=0 to Sections^.Count-1 do
    begin
      P:=Sections^.At(I);
      if I<>0 then writeln(f,'['+P^.GetName+']');
      for J:=0 to P^.Entries^.Count-1 do
        begin
          E:=P^.Entries^.At(J);
          writeln(f,E^.GetText);
          OK:=EatIO=0;
          if OK=false then Break;
        end;
      if OK and ((I>0) or (P^.Entries^.Count>0)) and (I<Sections^.Count-1) then
        writeln(f,'');
      OK:=OK and (EatIO=0);
      if OK=false then Break;
    end;
  Close(f);
  EatIO;
{$I+}
  if OK then
    for I:=0 to Sections^.Count-1 do
      begin
        P:=Sections^.At(I);
        for J:=0 to P^.Entries^.Count-1 do
          begin
            E:=P^.Entries^.At(J);
            E^.Modified:=false;
          end;
      end;
  Update:=OK;
end;

function TINIFile.SearchSection(Section: string): PINISection;
function MatchingSection(P: PINISection): boolean; {$ifndef FPC}far;{$endif}
var SN: string;
    M: boolean;
begin
  SN:=UpcaseStr(P^.GetName);
  M:=SN=Section;
  MatchingSection:=M;
end;
begin
  Section:=UpcaseStr(Section);
  SearchSection:=Sections^.FirstThat(@MatchingSection);
end;

function TINIFile.SearchEntry(const Section, Tag: string): PINIEntry;
var P: PINISection;
    E: PINIEntry;
begin
  P:=SearchSection(Section);
  if P=nil then E:=nil else
    E:=P^.SearchEntry(Tag);
  SearchEntry:=E;
end;

procedure TINIFile.ForEachEntry(const Section: string; EnumProc: pointer);
var P: PINISection;
    E: PINIEntry;
    I: integer;
begin
  P:=SearchSection(Section);
  if P<>nil then
    for I:=0 to P^.Entries^.Count-1 do
      begin
        E:=P^.Entries^.At(I);
      {$ifdef FPC}
        CallPointerMethodLocal(EnumProc,CurrentFramePointer,@Self,E);
      {$else}
        asm
          push E.word[2]
          push E.word[0]
          push word ptr [bp]
          call EnumProc
        end;
      {$endif}
      end;
end;

function TINIFile.GetEntry(const Section, Tag, Default: string): string;
var E: PINIEntry;
    S: string;
begin
  E:=SearchEntry(Section,Tag);
  if E=nil then S:=Default else
    S:=E^.GetValue;
  GetEntry:=S;
end;

procedure TINIFile.SetEntry(const Section, Tag, Value: string);
var E: PINIEntry;
    P: PINISection;
begin
  E:=SearchEntry(Section,Tag);
  if E=nil then
   if (MakeNullEntries=true) or (Value<>'') then
    begin
      P:=SearchSection(Section);
      if P=nil then
        begin
          New(P, Init(Section));
          Sections^.Insert(P);
        end;
      E:=P^.AddEntry(Tag+'='+Value);
      E^.Modified:=true;
    end;
  if E<>nil then
    E^.SetValue(Value);
end;

function TINIFile.GetIntEntry(const Section, Tag: string; Default: longint): longint;
var L: longint;
begin
  L:=StrToInt(GetEntry(Section,Tag,IntToStr(Default)));
  if LastStrToIntResult<>0 then L:=Default;
  GetIntEntry:=L;
end;

procedure TINIFile.SetIntEntry(const Section, Tag: string; Value: longint);
begin
  SetEntry(Section,Tag,IntToStr(Value));
end;

procedure TINIFile.DeleteSection(const Section: string);
var P: PINISection;
begin
  P:=SearchSection(Section);
  if P<>nil then
    Sections^.Free(P);
end;

procedure TINIFile.DeleteEntry(const Section, Tag: string);
var P: PINISection;
begin
  P:=SearchSection(Section);
  if P<>nil then
    P^.DeleteEntry(Tag);
end;

destructor TINIFile.Done;
begin
  if IsModified then
    Update;
  inherited Done;
  if FileName<>nil then
    DisposeStr(FileName);
  Dispose(Sections, Done);
end;


END.
{
  $Log: wini.pas,v $
  Revision 1.3  2002/09/07 15:40:50  peter
    * old logs removed and tabs fixed

}
