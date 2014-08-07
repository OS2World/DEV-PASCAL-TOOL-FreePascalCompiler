{
    $Id: browcol.pas,v 1.1.2.7 2001/08/04 11:04:10 peter Exp $
    Copyright (c) 1998-2000 by Berczi Gabor
    Modifications Copyright (c) 1999-2000 Florian Klaempfl and Pierre Muller

    Support routines for getting browser info in collections

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
unit browcol;
interface
uses
  cobjects,objects,symconst,symtable;

{$ifndef FPC}
  type
    sw_integer = integer;
{$endif FPC}

const
  SymbolTypLen : integer = 6;

  RecordTypes : set of tsymtyp =
    ([typesym,unitsym,programsym]);

    sfRecord        = $00000001;
    sfObject        = $00000002;
    sfClass         = $00000004;
    sfPointer       = $00000008;
    sfHasMemInfo    = $80000000;

type
    TStoreCollection = object(TStringCollection)
      function Add(const S: string): PString;
    end;

    PModuleNameCollection = ^TModuleNameCollection;
    TModuleNameCollection = object(TStoreCollection)
    end;

    PTypeNameCollection = ^TTypeNameCollection;
    TTypeNameCollection = object(TStoreCollection)
    end;

    PSymbolCollection       = ^TSymbolCollection;
    PSortedSymbolCollection = ^TSortedSymbolCollection;
    PReferenceCollection    = ^TReferenceCollection;

    PReference = ^TReference;
    TReference = object(TObject)
      FileName  : PString;
      Position  : TPoint;
      constructor Init(AFileName: PString; ALine, AColumn: Sw_integer);
      function    GetFileName: string;
      destructor  Done; virtual;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
    end;

    PSymbolMemInfo = ^TSymbolMemInfo;
    TSymbolMemInfo = record
      Addr      : longint;
      LocalAddr : longint;
      Size      : longint;
      PushSize  : longint;
    end;

    PSymbol = ^TSymbol;
    TSymbol = object(TObject)
      Name       : PString;
      Typ        : tsymtyp;
      Params     : PString;
      References : PReferenceCollection;
      Items      : PSymbolCollection;
      DType      : PString;
      VType      : PString;
      TypeID     : longint;
      RelatedTypeID : longint;
      DebuggerCount : longint;
      Ancestor   : PSymbol;
      Flags      : longint;
      MemInfo    : PSymbolMemInfo;
      constructor Init(const AName: string; ATyp: tsymtyp; AParams: string; AMemInfo: PSymbolMemInfo);
      procedure   SetMemInfo(const AMemInfo: TSymbolMemInfo);
      function    GetReferenceCount: Sw_integer;
      function    GetReference(Index: Sw_integer): PReference;
      function    GetItemCount: Sw_integer;
      function    GetItem(Index: Sw_integer): PSymbol;
      function    GetName: string;
      function    GetText: string;
      function    GetTypeName: string;
      destructor  Done; virtual;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
    end;

    PExport = ^TExport;
    TExport = object(TObject)
      constructor Init(const AName: string; AIndex: longint; ASymbol: PSymbol);
      function    GetDisplayText: string;
      destructor  Done; virtual;
    private
      Name: PString;
      Index: longint;
      Symbol: PSymbol;
    end;

    PExportCollection = ^TExportCollection;
    TExportCollection = object(TSortedCollection)
      function At(Index: sw_Integer): PExport;
      function Compare(Key1, Key2: Pointer): sw_Integer; virtual;
    end;

    PImport = ^TImport;
    TImport = object(TObject)
      constructor Init(const ALibName, AFuncName,ARealName: string; AIndex: longint);
      function    GetDisplayText: string;
      destructor  Done; virtual;
    private
      LibName: PString;
      FuncName: PString;
      RealName: PString;
      Index: longint;
    end;

    PImportCollection = ^TImportCollection;
    TImportCollection = object(TSortedCollection)
      function At(Index: sw_Integer): PImport;
      function Compare(Key1, Key2: Pointer): sw_Integer; virtual;
    end;

    PObjectSymbolCollection = ^TObjectSymbolCollection;

    PObjectSymbol = ^TObjectSymbol;
    TObjectSymbol = object(TObject)
      Parent     : PObjectSymbol;
      Symbol     : PSymbol;
      Expanded   : boolean;
      constructor Init(AParent: PObjectSymbol; ASymbol: PSymbol);
      constructor InitName(const AName: string);
      function    GetName: string;
      function    GetDescendantCount: sw_integer;
      function    GetDescendant(Index: sw_integer): PObjectSymbol;
      procedure   AddDescendant(P: PObjectSymbol);
      destructor  Done; virtual;
      constructor Load(var S: TStream);
      procedure   Store(S: TStream);
    private
      Name: PString;
      Descendants: PObjectSymbolCollection;
    end;

    TSymbolCollection = object(TSortedCollection)
       constructor Init(ALimit, ADelta: Integer);
       function  At(Index: Sw_Integer): PSymbol;
       procedure Insert(Item: Pointer); virtual;
       function  LookUp(const S: string; var Idx: sw_integer): string; virtual;
    end;

    TSortedSymbolCollection = object(TSymbolCollection)
      function  Compare(Key1, Key2: Pointer): Sw_Integer; virtual;
      procedure Insert(Item: Pointer); virtual;
      function  LookUp(const S: string; var Idx: sw_integer): string; virtual;
    end;

    PIDSortedSymbolCollection = ^TIDSortedSymbolCollection;
    TIDSortedSymbolCollection = object(TSymbolCollection)
      function  Compare(Key1, Key2: Pointer): Sw_Integer; virtual;
      procedure Insert(Item: Pointer); virtual;
      function  SearchSymbolByID(AID: longint): PSymbol;
    end;

    TObjectSymbolCollection = object(TSortedCollection)
      constructor Init(ALimit, ADelta: Integer);
      function  Compare(Key1, Key2: Pointer): Sw_Integer; virtual;
      function  LookUp(const S: string; var Idx: sw_integer): string; virtual;
       function At(Index: Sw_Integer): PObjectSymbol;
    end;

    TReferenceCollection = object(TCollection)
       function At(Index: Sw_Integer): PReference;
    end;

    PSourceFile = ^TSourceFile;
    TSourceFile = object(TObject)
      SourceFileName: PString;
      ObjFileName: PString;
      PPUFileName: PString;
      constructor Init(ASourceFileName, AObjFileName, APPUFileName: string);
      destructor  Done; virtual;
      function    GetSourceFilename: string;
      function    GetObjFileName: string;
      function    GetPPUFileName: string;
    end;

    PSourceFileCollection = ^TSourceFileCollection;
    TSourceFileCollection = object(TCollection)
      function At(Index: sw_Integer): PSourceFile;
    end;

    PModuleSymbol = ^TModuleSymbol;
    TModuleSymbol = object(TSymbol)
      Exports_   : PExportCollection;
      Imports    : PImportCollection;
      LoadedFrom : PString;
      UsedUnits  : PSymbolCollection;
      DependentUnits: PSymbolCollection;
      MainSource: PString;
      SourceFiles: PStringCollection;
      constructor Init(const AName, AMainSource: string);
      procedure   SetLoadedFrom(const AModuleName: string);
      procedure   AddUsedUnit(P: PSymbol);
      procedure   AddDependentUnit(P: PSymbol);
      procedure   AddSourceFile(const Path: string);
      destructor  Done; virtual;
    end;

const
  Modules     : PSymbolCollection = nil;
  ModuleNames : PModuleNameCollection = nil;
  TypeNames   : PTypeNameCollection = nil;
  ObjectTree  : PObjectSymbol = nil;
  SourceFiles : PSourceFileCollection = nil;

procedure DisposeBrowserCol;
procedure NewBrowserCol;
procedure CreateBrowserCol;
procedure InitBrowserCol;
procedure DoneBrowserCol;

function  LoadBrowserCol(S: PStream): boolean;
function  StoreBrowserCol(S: PStream) : boolean;

procedure BuildObjectInfo;

procedure BuildSourceList;

function SearchObjectForSymbol(O: PSymbol): PObjectSymbol;

procedure RegisterSymbols;

implementation

uses
  Dos,{$ifndef FPC}strings,{$endif}
{$ifdef DEBUG}
  verbose,
{$endif DEBUG}
  aasm,globtype,globals,files,comphook;

const
  RModuleNameCollection: TStreamRec = (
     ObjType: 3001;
     VmtLink: Ofs(TypeOf(TModuleNameCollection)^);
     Load:    @TModuleNameCollection.Load;
     Store:   @TModuleNameCollection.Store
  );
  RTypeNameCollection: TStreamRec = (
     ObjType: 3002;
     VmtLink: Ofs(TypeOf(TTypeNameCollection)^);
     Load:    @TTypeNameCollection.Load;
     Store:   @TTypeNameCollection.Store
  );
  RReference: TStreamRec = (
     ObjType: 3003;
     VmtLink: Ofs(TypeOf(TReference)^);
     Load:    @TReference.Load;
     Store:   @TReference.Store
  );
  RSymbol: TStreamRec = (
     ObjType: 3004;
     VmtLink: Ofs(TypeOf(TSymbol)^);
     Load:    @TSymbol.Load;
     Store:   @TSymbol.Store
  );
  RObjectSymbol: TStreamRec = (
     ObjType: 3005;
     VmtLink: Ofs(TypeOf(TObjectSymbol)^);
     Load:    @TObjectSymbol.Load;
     Store:   @TObjectSymbol.Store
  );
  RSymbolCollection: TStreamRec = (
     ObjType: 3006;
     VmtLink: Ofs(TypeOf(TSymbolCollection)^);
     Load:    @TSymbolCollection.Load;
     Store:   @TSymbolCollection.Store
  );
  RSortedSymbolCollection: TStreamRec = (
     ObjType: 3007;
     VmtLink: Ofs(TypeOf(TSortedSymbolCollection)^);
     Load:    @TSortedSymbolCollection.Load;
     Store:   @TSortedSymbolCollection.Store
  );
  RIDSortedSymbolCollection: TStreamRec = (
     ObjType: 3008;
     VmtLink: Ofs(TypeOf(TIDSortedSymbolCollection)^);
     Load:    @TIDSortedSymbolCollection.Load;
     Store:   @TIDSortedSymbolCollection.Store
  );
  RObjectSymbolCollection: TStreamRec = (
     ObjType: 3009;
     VmtLink: Ofs(TypeOf(TObjectSymbolCollection)^);
     Load:    @TObjectSymbolCollection.Load;
     Store:   @TObjectSymbolCollection.Store
  );
  RReferenceCollection: TStreamRec = (
     ObjType: 3010;
     VmtLink: Ofs(TypeOf(TReferenceCollection)^);
     Load:    @TReferenceCollection.Load;
     Store:   @TReferenceCollection.Store
  );
  RModuleSymbol: TStreamRec = (
     ObjType: 3011;
     VmtLink: Ofs(TypeOf(TModuleSymbol)^);
     Load:    @TModuleSymbol.Load;
     Store:   @TModuleSymbol.Store
  );

{****************************************************************************
                                   Helpers
****************************************************************************}

function GetStr(P: PString): string;
begin
  if P=nil then
    GetStr:=''
  else
    GetStr:=P^;
end;

function IntToStr(L: longint): string;
var S: string;
begin
  Str(L,S);
  IntToStr:=S;
end;

function UpcaseStr(S: string): string;
var I: integer;
begin
  for I:=1 to length(S) do
      S[I]:=Upcase(S[I]);
  UpcaseStr:=S;
end;

function FloatToStr(E: extended): string;
var S: string;
begin
  Str(E:0:24,S);
  if Pos('.',S)>0 then
    begin
      while (length(S)>0) and (S[length(S)]='0') do
        Delete(S,length(S),1);
      if (length(S)>0) and (S[length(S)]='.') then
        Delete(S,length(S),1);
    end;
  if S='' then S:='0';
  FloatToStr:=S;
end;

{****************************************************************************
                                TStoreCollection
****************************************************************************}

function TStoreCollection.Add(const S: string): PString;
var P: PString;
    Index: Sw_integer;
begin
  if S='' then P:=nil else
  if Search(@S,Index) then P:=At(Index) else
    begin
      P:=NewStr(S);
      Insert(P);
    end;
  Add:=P;
end;


{****************************************************************************
                                TSymbolCollection
****************************************************************************}

constructor TSymbolCollection.Init(ALimit, ADelta: Integer);
begin
  inherited Init(ALimit,ADelta);
{  Duplicates:=true;}
end;

function TSymbolCollection.At(Index: Sw_Integer): PSymbol;
begin
  At:=inherited At(Index);
end;

procedure TSymbolCollection.Insert(Item: Pointer);
begin

  TCollection.Insert(Item);
end;

function TSymbolCollection.LookUp(const S: string; var Idx: sw_integer): string;
begin
  Idx:=-1;
  LookUp:='';
end;

{****************************************************************************
                               TReferenceCollection
****************************************************************************}

function TReferenceCollection.At(Index: Sw_Integer): PReference;
begin
  At:=inherited At(Index);
end;


{****************************************************************************
                            TSortedSymbolCollection
****************************************************************************}

function TSortedSymbolCollection.Compare(Key1, Key2: Pointer): Sw_Integer;
var K1: PSymbol absolute Key1;
    K2: PSymbol absolute Key2;
    R: Sw_integer;
    S1,S2: string;
begin
  S1:=Upper(K1^.GetName);
  S2:=Upper(K2^.GetName);
  if S1<S2 then R:=-1 else
  if S1>S2 then R:=1 else
   if K1^.TypeID=K2^.TypeID then R:=0 else
    begin
      S1:=K1^.GetName;
      S2:=K2^.GetName;
      if S1<S2 then R:=-1 else
      if S1>S2 then R:=1 else
       if K1^.TypeID<K2^.TypeID then R:=-1 else
       if K1^.TypeID>K2^.TypeID then R:= 1 else
        R:=0;
    end;
  Compare:=R;
end;

procedure TSortedSymbolCollection.Insert(Item: Pointer);
begin
  TSortedCollection.Insert(Item);
end;

function TSortedSymbolCollection.LookUp(const S: string; var Idx: sw_integer): string;
var OLI,ORI,Left,Right,Mid: integer;
    LeftP,RightP,MidP: PSymbol;
    RL: integer;
    LeftS,MidS,RightS: string;
    FoundS: string;
    UpS : string;
begin
  Idx:=-1; FoundS:='';
  Left:=0; Right:=Count-1;
  UpS:=Upper(S);
  if Left<Right then
  begin
    while (Left<Right) do
    begin
      OLI:=Left; ORI:=Right;
      Mid:=Left+(Right-Left) div 2;
      MidP:=At(Mid);
{$ifdef DEBUG}
      LeftP:=At(Left); RightP:=At(Right);
      LeftS:=Upper(LeftP^.GetName);
      RightS:=Upper(RightP^.GetName);
{$endif DEBUG}
      MidS:=Upper(MidP^.GetName);
      if copy(MidS,1,length(UpS))=UpS then
        begin
          Idx:=Mid;
          FoundS:=MidS;
        end;
{      else}
        if UpS<MidS then
          Right:=Mid
        else
          Left:=Mid;
      if (OLI=Left) and (ORI=Right) then
        begin
          if idX<>-1 then
            break;
          if Mid=Left then
            begin
              RightP:=At(Right);
              RightS:=Upper(RightP^.GetName);
              if copy(RightS,1,length(UpS))=UpS then
                begin
                  Idx:=Right;
                  FoundS:=RightS;
                end;
            end;
          if Mid=Right then
            begin
              LeftP:=At(Left);
              LeftS:=Upper(LeftP^.GetName);
              if copy(LeftS,1,length(UpS))=UpS then
                begin
                  Idx:=Left;
                  FoundS:=LeftS;
                end;
            end;
          Break;
        end;
    end;
  end;
  LookUp:=FoundS;
end;

{****************************************************************************
                           TIDSortedSymbolCollection
****************************************************************************}

function TIDSortedSymbolCollection.Compare(Key1, Key2: Pointer): Sw_Integer;
var K1: PSymbol absolute Key1;
    K2: PSymbol absolute Key2;
    R: Sw_integer;
begin
  if K1^.TypeID<K2^.TypeID then R:=-1 else
  if K1^.TypeID>K2^.TypeID then R:= 1 else
  R:=0;
  Compare:=R;
end;

procedure TIDSortedSymbolCollection.Insert(Item: Pointer);
begin
  TSortedCollection.Insert(Item);
end;

function TIDSortedSymbolCollection.SearchSymbolByID(AID: longint): PSymbol;
var S: TSymbol;
    Index: sw_integer;
    P: PSymbol;
begin
  S.TypeID:=AID;
  if Search(@S,Index)=false then P:=nil else
    P:=At(Index);
  SearchSymbolByID:=P;
end;

{****************************************************************************
                           TObjectSymbolCollection
****************************************************************************}

function TObjectSymbolCollection.At(Index: Sw_Integer): PObjectSymbol;
begin
  At:=inherited At(Index);
end;

constructor TObjectSymbolCollection.Init(ALimit, ADelta: Integer);
begin
  inherited Init(ALimit,ADelta);
end;

function TObjectSymbolCollection.Compare(Key1, Key2: Pointer): Sw_Integer;
var K1: PObjectSymbol absolute Key1;
    K2: PObjectSymbol absolute Key2;
    R: Sw_integer;
    S1,S2: string;
begin
  S1:=Upper(K1^.GetName);
  S2:=Upper(K2^.GetName);
  if S1<S2 then R:=-1 else
  if S1>S2 then R:=1 else
  { make sure that we distinguish between different objects with the same name }
  if longint(K1^.Symbol)<longint(K2^.Symbol) then R:=-1 else
  if longint(K1^.Symbol)>longint(K2^.Symbol) then R:= 1 else
  R:=0;
  Compare:=R;
end;

function TObjectSymbolCollection.LookUp(const S: string; var Idx: sw_integer): string;
var OLI,ORI,Left,Right,Mid: integer;
    LeftP,RightP,MidP: PObjectSymbol;
    RL: integer;
    LeftS,MidS,RightS: string;
    FoundS: string;
    UpS : string;
begin
  Idx:=-1; FoundS:='';
  Left:=0; Right:=Count-1;
  UpS:=Upper(S);
  if Left<Right then
  begin
    while (Left<Right) do
    begin
      OLI:=Left; ORI:=Right;
      Mid:=Left+(Right-Left) div 2;
      LeftP:=At(Left); RightP:=At(Right); MidP:=At(Mid);
      LeftS:=Upper(LeftP^.GetName); MidS:=Upper(MidP^.GetName);
      RightS:=Upper(RightP^.GetName);
      if copy(MidS,1,length(UpS))=UpS then
        begin
          Idx:=Mid;
          FoundS:=MidS;
        end;
{      else}
        if UpS<MidS then
          Right:=Mid
        else
          Left:=Mid;
      if (OLI=Left) and (ORI=Right) then
        Break;
    end;
  end;
  LookUp:=FoundS;
end;

{****************************************************************************
                                TReference
****************************************************************************}

constructor TReference.Init(AFileName: PString; ALine, AColumn: Sw_integer);
begin
  inherited Init;
  FileName:=AFileName;
  Position.X:=AColumn;
  Position.Y:=ALine;
end;

function TReference.GetFileName: string;
begin
  GetFileName:=GetStr(FileName);
end;

destructor TReference.Done;
begin
  inherited Done;
end;

constructor TReference.Load(var S: TStream);
begin
  S.Read(Position, SizeOf(Position));

  { --- items needing fixup --- }
  S.Read(FileName, SizeOf(FileName)); { ->ModulesNames^.Item }
end;

procedure TReference.Store(var S: TStream);
begin
  S.Write(Position, SizeOf(Position));

  { --- items needing fixup --- }
  S.Write(FileName, SizeOf(FileName));
end;

{****************************************************************************
                                   TSymbol
****************************************************************************}

constructor TSymbol.Init(const AName: string; ATyp: tsymtyp; AParams: string; AMemInfo: PSymbolMemInfo);
begin
  inherited Init;
  Name:=NewStr(AName); Typ:=ATyp;
  if AMemInfo<>nil then
    SetMemInfo(AMemInfo^);
  New(References, Init(20,50));
  if ATyp in RecordTypes then
    begin
      Items:=New(PSortedSymbolCollection, Init(50,100));
    end;
end;

procedure TSymbol.SetMemInfo(const AMemInfo: TSymbolMemInfo);
begin
  if MemInfo=nil then New(MemInfo);
  Move(AMemInfo,MemInfo^,SizeOf(MemInfo^));
  Flags:=Flags or sfHasMemInfo;
end;

function TSymbol.GetReferenceCount: Sw_integer;
var Count: Sw_integer;
begin
  if References=nil then Count:=0 else
    Count:=References^.Count;
  GetReferenceCount:=Count;
end;

function TSymbol.GetReference(Index: Sw_integer): PReference;
begin
  GetReference:=References^.At(Index);
end;

function TSymbol.GetItemCount: Sw_integer;
var Count: Sw_integer;
begin
  if Items=nil then Count:=0 else
    Count:=Items^.Count;
  GetItemCount:=Count;
end;

function TSymbol.GetItem(Index: Sw_integer): PSymbol;
begin
  GetItem:=Items^.At(Index);
end;

function TSymbol.GetName: string;
begin
  GetName:=GetStr(Name);
end;

function TSymbol.GetText: string;
var S: string;
    I: Sw_integer;
begin
  S:=GetTypeName;
  if length(S)>SymbolTypLen then
   S:=Copy(S,1,SymbolTypLen)
  else
   begin
     while length(S)<SymbolTypLen do
      S:=S+' ';
   end;
  S:=S+' '+GetName;
  if (Flags and sfRecord)<>0 then
    S:=S+' = record'
  else
  if (Flags and sfObject)<>0 then
    begin
      S:=S+' = ';
      if (Flags and sfClass)<>0 then
        S:=S+'class'
      else
        S:=S+'object';
      if Ancestor<>nil then
        S:=S+'('+Ancestor^.GetName+')';
    end
  else
    begin
      if Assigned(DType) then
        S:=S+' = '+DType^;
      if Assigned(Params) then
        S:=S+'('+Params^+')';
      if Assigned(VType) then
        S:=S+': '+VType^;
    end;
  GetText:=S;
end;

function TSymbol.GetTypeName: string;
var S: string;
begin
  case Typ of
    abstractsym  : S:='abst';
    varsym       : S:='var';
    typesym      : S:='type';
    procsym      : if VType=nil then
                     S:='proc'
                   else
                     S:='func';
    unitsym      : S:='unit';
    programsym   : S:='prog';
    constsym     : S:='const';
    enumsym      : S:='enum';
    typedconstsym: S:='const';
    errorsym     : S:='error';
    syssym       : S:='sys';
    labelsym     : S:='label';
    absolutesym  : S:='abs';
    propertysym  : S:='prop';
    funcretsym   : S:='res';
    macrosym     : S:='macro';
  else S:='';
  end;
  GetTypeName:=S;
end;

destructor TSymbol.Done;
begin
  inherited Done;
  if assigned(MemInfo) then
    Dispose(MemInfo);
  if assigned(References) then
    Dispose(References, Done);
  if assigned(Items) then
    Dispose(Items, Done);
  if assigned(Name) then
    DisposeStr(Name);
{  if assigned(Params) then
    DisposeStr(Params); in TypeNames
  if assigned(VType) then
    DisposeStr(VType);
  if assigned(DType) then
    DisposeStr(DType);
  if assigned(Ancestor) then
    DisposeStr(Ancestor);}
end;

constructor TSymbol.Load(var S: TStream);
var MI: TSymbolMemInfo;
    W: word;
begin
  TObject.Init;

  S.Read(Typ,SizeOf(Typ));
  S.Read(TypeID, SizeOf(TypeID));
  S.Read(RelatedTypeID, SizeOf(RelatedTypeID));
  S.Read(Flags, SizeOf(Flags));
  Name:=S.ReadStr;
  if (Flags and sfHasMemInfo)<>0 then
    begin
      S.Read(MI,SizeOf(MI));
      SetMemInfo(MI);
    end;

  W:=0;
  S.Read(W,SizeOf(W));
  if (W and 1)<>0 then
    New(References, Load(S));
  if (W and 2)<>0 then
    New(Items, Load(S));

  { --- items needing fixup --- }
  S.Read(DType, SizeOf(DType));
  S.Read(VType, SizeOf(VType));
  S.Read(Params, SizeOf(Params));
end;

procedure TSymbol.Store(var S: TStream);
var W: word;
begin
  S.Write(Typ,SizeOf(Typ));
  S.Write(TypeID, SizeOf(TypeID));
  S.Write(RelatedTypeID, SizeOf(RelatedTypeID));
  S.Write(Flags, SizeOf(Flags));
  S.WriteStr(Name);

  if (Flags and sfHasMemInfo)<>0 then
    S.Write(MemInfo^,SizeOf(MemInfo^));

  W:=0;
  if Assigned(References) then W:=W or 1;
  if Assigned(Items) then W:=W or 2;
  S.Write(W,SizeOf(W));
  if Assigned(References) then References^.Store(S);
  if Assigned(Items) then Items^.Store(S);

  { --- items needing fixup --- }
  S.Write(DType, SizeOf(DType));
  S.Write(VType, SizeOf(VType));
  S.Write(Params, SizeOf(Params));
end;

constructor TExport.Init(const AName: string; AIndex: longint; ASymbol: PSymbol);
begin
  inherited Init;
  Name:=NewStr(AName); Index:=AIndex;
  Symbol:=ASymbol;
end;

function TExport.GetDisplayText: string;
var S: string;
begin
  S:=GetStr(Name)+' '+IntToStr(Index);
  if Assigned(Symbol) and (UpcaseStr(Symbol^.GetName)<>UpcaseStr(GetStr(Name))) then
    S:=S+' ('+Symbol^.GetName+')';
  GetDisplayText:=S;
end;

destructor TExport.Done;
begin
  if Assigned(Name) then DisposeStr(Name);
  inherited Done;
end;

constructor TImport.Init(const ALibName, AFuncName,ARealName: string; AIndex: longint);
begin
  inherited Init;
  LibName:=NewStr(ALibName);
  FuncName:=NewStr(AFuncName); RealName:=NewStr(ARealName);
  Index:=AIndex;
end;

function TImport.GetDisplayText: string;
var S: string;
begin
  S:=GetStr(RealName);
  if Assigned(FuncName) then S:=GetStr(FuncName)+' ('+S+')';
  if S='' then S:=IntToStr(Index);
  S:=GetStr(LibName)+' '+S;
  GetDisplayText:=S;
end;

destructor TImport.Done;
begin
  if Assigned(LibName) then DisposeStr(LibName);
  if Assigned(FuncName) then DisposeStr(FuncName);
  if Assigned(RealName) then DisposeStr(RealName);
  inherited Done;
end;

function TImportCollection.At(Index: sw_Integer): PImport;
begin
  At:=inherited At(Index);
end;

function TImportCollection.Compare(Key1, Key2: Pointer): sw_Integer;
var K1: PImport absolute Key1;
    K2: PImport absolute Key2;
    S1: string;
    S2: string;
    R: sw_integer;
begin
  if (K1^.RealName=nil) and (K2^.RealName<>nil) then R:= 1 else
  if (K1^.RealName<>nil) and (K2^.RealName=nil) then R:=-1 else
  if (K1^.RealName=nil) and (K2^.RealName=nil) then
    begin
      if K1^.Index<K2^.Index then R:=-1 else
      if K1^.Index>K2^.Index then R:= 1 else
      R:=0;
    end
  else
    begin
      if K1^.FuncName=nil then S1:=GetStr(K1^.RealName) else S1:=GetStr(K1^.FuncName);
      if K2^.FuncName=nil then S2:=GetStr(K2^.RealName) else S2:=GetStr(K2^.FuncName);
      S1:=UpcaseStr(S1); S2:=UpcaseStr(S2);
      if S1<S2 then R:=-1 else
      if S1>S2 then R:= 1 else
      R:=0;
    end;
  Compare:=R;
end;

function TExportCollection.At(Index: sw_Integer): PExport;
begin
  At:=inherited At(Index);
end;

function TExportCollection.Compare(Key1, Key2: Pointer): sw_Integer;
var K1: PExport absolute Key1;
    K2: PExport absolute Key2;
    S1: string;
    S2: string;
    R: sw_integer;
begin
  S1:=UpcaseStr(GetStr(K1^.Name)); S2:=UpcaseStr(GetStr(K2^.Name));
  if S1<S2 then R:=-1 else
  if S1>S2 then R:= 1 else
  R:=0;
  Compare:=R;
end;

constructor TModuleSymbol.Init(const AName, AMainSource: string);
begin
  inherited Init(AName,unitsym,'',nil);
  MainSource:=NewStr(AMainSource);
end;

procedure TModuleSymbol.SetLoadedFrom(const AModuleName: string);
begin
  SetStr(LoadedFrom,AModuleName);
end;

procedure TModuleSymbol.AddUsedUnit(P: PSymbol);
begin
  if Assigned(UsedUnits)=false then
    New(UsedUnits, Init(10,10));
  UsedUnits^.Insert(P);
end;

procedure TModuleSymbol.AddDependentUnit(P: PSymbol);
begin
  if Assigned(DependentUnits)=false then
    New(DependentUnits, Init(10,10));
  DependentUnits^.Insert(P);
end;

procedure TModuleSymbol.AddSourceFile(const Path: string);
begin
  if Assigned(SourceFiles)=false then
    New(SourceFiles, Init(10,10));
  SourceFiles^.Insert(NewStr(Path));
end;

destructor TModuleSymbol.Done;
begin
  inherited Done;
  if Assigned(MainSource) then DisposeStr(MainSource);
  if assigned(Exports_) then
    Dispose(Exports_, Done);
  if Assigned(Imports) then
    Dispose(Imports, Done);
  if Assigned(LoadedFrom) then
    DisposeStr(LoadedFrom);
  if Assigned(UsedUnits) then
  begin
    UsedUnits^.DeleteAll;
    Dispose(UsedUnits, Done);
  end;
  if Assigned(DependentUnits) then
  begin
    DependentUnits^.DeleteAll;
    Dispose(DependentUnits, Done);
  end;
  if Assigned(SourceFiles) then Dispose(SourceFiles, Done);
end;


constructor TObjectSymbol.Init(AParent: PObjectSymbol; ASymbol: PSymbol);
begin
  inherited Init;
  Parent:=AParent;
  Symbol:=ASymbol;
end;

constructor TObjectSymbol.InitName(const AName: string);
begin
  inherited Init;
  Name:=NewStr(AName);
end;

function TObjectSymbol.GetName: string;
begin
  if Name<>nil then
    GetName:=Name^
  else
    GetName:=Symbol^.GetName;
end;

function TObjectSymbol.GetDescendantCount: sw_integer;
var Count: sw_integer;
begin
  if Descendants=nil then Count:=0 else
    Count:=Descendants^.Count;
  GetDescendantCount:=Count;
end;

function TObjectSymbol.GetDescendant(Index: sw_integer): PObjectSymbol;
begin
  GetDescendant:=Descendants^.At(Index);
end;

procedure TObjectSymbol.AddDescendant(P: PObjectSymbol);
begin
  if Descendants=nil then
    New(Descendants, Init(50,10));
  Descendants^.Insert(P);
end;

destructor TObjectSymbol.Done;
begin
  if Assigned(Name) then DisposeStr(Name); Name:=nil;
  if Assigned(Descendants) then Dispose(Descendants, Done); Descendants:=nil;
  inherited Done;
end;

constructor TObjectSymbol.Load(var S: TStream);
begin
end;

procedure TObjectSymbol.Store(S: TStream);
begin
end;

{****************************************************************************
                                TSourceFile
****************************************************************************}

constructor TSourceFile.Init(ASourceFileName, AObjFileName, APPUFileName: string);
begin
  inherited Init;
  SourceFileName:=NewStr(ASourceFileName);
  ObjFileName:=NewStr(AObjFileName);
  PPUFileName:=NewStr(APPUFileName);
end;

destructor TSourceFile.Done;
begin
  if assigned(SourceFileName) then DisposeStr(SourceFileName);
  if assigned(ObjFileName) then DisposeStr(ObjFileName);
  if assigned(PPUFileName) then DisposeStr(PPUFileName);
  inherited Done;
end;

function TSourceFile.GetSourceFilename: string;
begin
  GetSourceFilename:=GetStr(SourceFileName);
end;

function TSourceFile.GetObjFileName: string;
begin
  GetObjFilename:=GetStr(ObjFileName);
end;

function TSourceFile.GetPPUFileName: string;
begin
  GetPPUFilename:=GetStr(PPUFileName);
end;

function TSourceFileCollection.At(Index: sw_Integer): PSourceFile;
begin
  At:=inherited At(Index);
end;

{*****************************************************************************
                              Main Routines
*****************************************************************************}

procedure DisposeBrowserCol;
begin
  if assigned(Modules) then
   begin
     dispose(Modules,Done);
     Modules:=nil;
   end;
  if assigned(ModuleNames) then
   begin
     dispose(ModuleNames,Done);
     ModuleNames:=nil;
   end;
  if assigned(TypeNames) then
   begin
     dispose(TypeNames,Done);
     TypeNames:=nil;
   end;
  if assigned(ObjectTree) then
    begin
      Dispose(ObjectTree, Done);
      ObjectTree:=nil;
    end;
end;


procedure NewBrowserCol;
begin
  New(Modules, Init(50,50));
  New(ModuleNames, Init(50,50));
  New(TypeNames, Init(1000,5000));
end;


  procedure ProcessSymTable(OwnerSym: PSymbol; var Owner: PSymbolCollection; Table: PSymTable);
  var I,J,defcount,symcount: longint;
      Ref: PRef;
      Sym,ParSym: PSym;
      Symbol: PSymbol;
      Reference: PReference;
      ParamCount: Sw_integer;
      Params: array[0..20] of PString;
      inputfile : pinputfile;
      Idx: sw_integer;
      S: string;
  procedure SetVType(Symbol: PSymbol; VType: string);
  begin
    Symbol^.VType:=TypeNames^.Add(VType);
  end;
  procedure SetDType(Symbol: PSymbol; DType: string);
  begin
    Symbol^.DType:=TypeNames^.Add(DType);
  end;
  function GetDefinitionStr(def: pdef): string; forward;
  function GetEnumDefStr(def: penumdef): string;
  var Name: string;
      esym: penumsym;
      Count: integer;
  begin
    Name:='(';
    esym:=def^.Firstenum; Count:=0;
    while (esym<>nil) do
      begin
        if Count>0 then
          Name:=Name+', ';
        Name:=Name+esym^.name;
        esym:=esym^.nextenum;
        Inc(Count);
      end;
    Name:=Name+')';
    GetEnumDefStr:=Name;
  end;
  function GetArrayDefStr(def: parraydef): string;
  var Name: string;
  begin
    Name:='array ['+IntToStr(def^.lowrange)+'..'+IntToStr(def^.highrange)+'] of ';
    if assigned(def^.elementtype.def) then
      Name:=Name+GetDefinitionStr(def^.elementtype.def);
    GetArrayDefStr:=Name;
  end;
  function GetFileDefStr(def: pfiledef): string;
  var Name: string;
  begin
    Name:='';
    case def^.filetyp of
      ft_text    : Name:='text';
      ft_untyped : Name:='file';
      ft_typed   : Name:='file of '+GetDefinitionStr(def^.typedfiletype.def);
    end;
    GetFileDefStr:=Name;
  end;
  function GetStringDefStr(def: pstringdef): string;
  var Name: string;
  begin
    Name:='';
    case def^.string_typ of
      st_shortstring :
        if def^.len=255 then
          Name:='shortstring'
        else
          Name:='string['+IntToStr(def^.len)+']';
      st_longstring :
        Name:='longstring';
      st_ansistring :
        Name:='ansistring';
      st_widestring :
        Name:='widestring';
    else ;
    end;
    GetStringDefStr:=Name;
  end;
  function retdefassigned(def: pabstractprocdef): boolean;
  var OK: boolean;
  begin
    OK:=false;
    if assigned(def^.rettype.def) then
      if UpcaseStr(GetDefinitionStr(def^.rettype.def))<>'VOID' then
        OK:=true;
    retdefassigned:=OK;
  end;
  function GetAbsProcParmDefStr(def: pabstractprocdef): string;
  var Name: string;
      dc: pparaitem;
      Count: integer;
      CurName: string;
  begin
    Name:='';
    dc:=pparaitem(def^.para^.first);
    Count:=0;
    while assigned(dc) do
     begin
       CurName:='';
       case dc^.paratyp of
         vs_Value : ;
         vs_Const : CurName:=CurName+'const ';
         vs_Var   : CurName:=CurName+'var ';
       end;
       if assigned(dc^.paratype.def) then
         CurName:=CurName+GetDefinitionStr(dc^.paratype.def);
       if dc^.next<>nil then
         CurName:=', '+CurName;
       Name:=CurName+Name;
       dc:=pparaitem(dc^.next);
       Inc(Count);
     end;
    GetAbsProcParmDefStr:=Name;
  end;
  function GetAbsProcDefStr(def: pabstractprocdef): string;
  var Name: string;
  begin
    Name:=GetAbsProcParmDefStr(def);
    if Name<>'' then Name:='('+Name+')';
    if retdefassigned(def) then
      Name:='function'+Name+': '+GetDefinitionStr(def^.rettype.def)
    else
      Name:='procedure'+Name;
    GetAbsProcDefStr:=Name;
  end;
  function GetProcDefStr(def: pprocdef): string;
  var DName: string;
      J: integer;
  begin
{    DName:='';
    if assigned(def) then
    begin
      if assigned(def^.parast) then
        begin
          with def^.parast^ do
          for J:=1 to number_symbols do
            begin
              if J<>1 then DName:=DName+', ';
              ParSym:=GetsymNr(J);
              if ParSym=nil then Break;
              DName:=DName+ParSym^.Name;
            end;
        end
    end;}
    DName:=GetAbsProcDefStr(def);
    GetProcDefStr:=DName;
  end;
  function GetProcVarDefStr(def: pprocvardef): string;
  begin
    GetProcVarDefStr:=GetAbsProcDefStr(def);
  end;
  function GetSetDefStr(def: psetdef): string;
  var Name: string;
  begin
    Name:='';
    case def^.settype of
      normset  : Name:='set';
      smallset : Name:='set';
      varset   : Name:='varset';
    end;
    Name:=Name+' of ';
    Name:=Name+GetDefinitionStr(def^.elementtype.def);
    GetSetDefStr:=Name;
  end;
  function GetPointerDefStr(def: ppointerdef): string;
  begin
    GetPointerDefStr:='^'+GetDefinitionStr(def^.pointertype.def);
  end;
  function GetDefinitionStr(def: pdef): string;
  var Name: string;
      sym: psym;
  begin
    Name:='';
    if def<>nil then
    begin
      if assigned(def^.typesym) then
        Name:=def^.typesym^.name;
      if Name='' then
      case def^.deftype of
        arraydef :
          Name:=GetArrayDefStr(parraydef(def));
        stringdef :
          Name:=GetStringDefStr(pstringdef(def));
        enumdef :
          Name:=GetEnumDefStr(penumdef(def));
        procdef :
          Name:=GetProcDefStr(pprocdef(def));
        procvardef :
          Name:=GetProcVarDefStr(pprocvardef(def));
        filedef :
          Name:=GetFileDefStr(pfiledef(def));
        setdef :
          Name:=GetSetDefStr(psetdef(def));
      end;
    end;
    GetDefinitionStr:=Name;
  end;
  function GetEnumItemName(Sym: penumsym): string;
  var Name: string;
      ES: penumsym;
  begin
    Name:='';
    if assigned(sym) and assigned(sym^.definition) then
      if assigned(sym^.definition^.typesym) then
      begin
{        ES:=sym^.definition^.First;
        while (ES<>nil) and (ES^.Value<>sym^.Value) do
          ES:=ES^.next;
        if assigned(es) and (es^.value=sym^.value) then
          Name:=}
        Name:=sym^.definition^.typesym^.name;
        if Name<>'' then
          Name:=Name+'('+IntToStr(sym^.value)+')';
      end;
    GetEnumItemName:=Name;
  end;
  function GetConstValueName(sym: pconstsym): string;
  var Name: string;
  begin
    Name:='';
{    if assigned(sym^.definition) then
     if assigned(sym^.definition^.sym) then
       Name:=sym^.definition^.sym^.name;}
    if Name='' then
    case sym^.consttyp of
      constord :
        Name:=sym^.consttype.def^.typesym^.name+'('+IntToStr(sym^.value)+')';
      constresourcestring,
      conststring :
{        Name:=''''+GetStr(PString(sym^.Value))+'''';}
        Name:=''''+StrPas(pointer(sym^.Value))+'''';
      constreal:
        Name:=FloatToStr(PBestReal(sym^.Value)^);
      constbool:
{        if boolean(sym^.Value)=true then
          Name:='TRUE'
        else
          Name:='FALSE';}
        Name:='Longbool('+IntToStr(sym^.Value)+')';
      constint:
        Name:=IntToStr(sym^.value);
      constchar:
        Name:=''''+chr(sym^.Value)+'''';
      constset:
{        Name:=SetToStr(pnormalset(sym^.Value))};
      constnil: ;
    end;
    GetConstValueName:=Name;
  end;
  procedure ProcessDefIfStruct(definition: pdef);
  begin
    { still led to infinite recursions
      only usefull for unamed types PM }
    if assigned(definition) and not assigned(definition^.typesym) then
    begin
      case definition^.deftype of
        recorddef :
          if precorddef(definition)^.symtable<>Table then
            ProcessSymTable(Symbol,Symbol^.Items,precorddef(definition)^.symtable);
        objectdef :
          if pobjectdef(definition)^.symtable<>Table then
            ProcessSymTable(Symbol,Symbol^.Items,pobjectdef(definition)^.symtable);
        { leads to infinite loops !!
        pointerdef :
          with ppointerdef(definition)^ do
            if assigned(definition) then
              if assigned(definition^.sym) then
                ProcessDefIfStruct(definition^.sym^.definition);}
      end;
    end;
  end;
  var MemInfo: TSymbolMemInfo;
      ObjDef: pobjectdef;
  begin
    if not Assigned(Table) then
     Exit;
    if Owner=nil then
     Owner:=New(PSortedSymbolCollection, Init(10,50));
    sym:=psym(Table^.symindex^.first);
    while assigned(sym) do
      begin
        ParamCount:=0;
        New(Symbol, Init(Sym^.Name,Sym^.Typ,'',nil));
        case Sym^.Typ of
          varsym :
             with pvarsym(sym)^ do
             begin
               if assigned(vartype.def) then
                 if assigned(vartype.def^.typesym) then
                   SetVType(Symbol,vartype.def^.typesym^.name)
                 else
                   SetVType(Symbol,GetDefinitionStr(vartype.def));
               ProcessDefIfStruct(vartype.def);
               if assigned(vartype.def) then
                 if (vartype.def^.deftype=pointerdef) and
                    assigned(ppointerdef(vartype.def)^.pointertype.def) then
                 begin
                   Symbol^.Flags:=(Symbol^.Flags or sfPointer);
                   Symbol^.RelatedTypeID:=longint(ppointerdef(vartype.def)^.pointertype.def);
                 end;
               MemInfo.Addr:=address;
               if assigned(localvarsym) then
                 MemInfo.LocalAddr:=localvarsym^.address
               else
                 MemInfo.LocalAddr:=0;
               if assigned(vartype.def) and (vartype.def^.deftype=arraydef) then
                 begin
                   if parraydef(vartype.def)^.highrange<parraydef(vartype.def)^.lowrange then
                     MemInfo.Size:=-1
                   else
                     MemInfo.Size:=getsize;
                 end
               else
                 MemInfo.Size:=getsize;
               MemInfo.PushSize:=getpushsize;
               Symbol^.SetMemInfo(MemInfo);
             end;
          constsym :
             SetDType(Symbol,GetConstValueName(pconstsym(sym)));
          enumsym :
            if assigned(penumsym(sym)^.definition) then
             SetDType(Symbol,GetEnumItemName(penumsym(sym)));
          unitsym :
            begin
  {            ProcessSymTable(Symbol^.Items,punitsym(sym)^.unitsymtable);}
            end;
          syssym :
{            if assigned(Table^.Name) then
            if Table^.Name^='SYSTEM' then}
              begin
                Symbol^.Params:=TypeNames^.Add('...');
              end;
          funcretsym :
            if Assigned(OwnerSym) then
            with pfuncretsym(sym)^ do
              if assigned(rettype.def) then
                if assigned(rettype.def^.typesym) then
                   SetVType(OwnerSym,rettype.def^.typesym^.name);
          procsym :
            begin
              with pprocsym(sym)^ do
              if assigned(definition) then
              begin
                if cs_local_browser in aktmoduleswitches then
                  ProcessSymTable(Symbol,Symbol^.Items,definition^.parast);
                if assigned(definition^.parast) then
                  begin
                    Symbol^.Params:=TypeNames^.Add(GetAbsProcParmDefStr(definition));
                  end
                else { param-definition is NOT assigned }
                  if assigned(Table^.Name) then
                  if Table^.Name^='SYSTEM' then
                  begin
                    Symbol^.Params:=TypeNames^.Add('...');
                  end;
                if cs_local_browser in aktmoduleswitches then
                 begin
                   if assigned(definition^.localst) and
                     (definition^.localst^.symtabletype<>staticsymtable) then
                    ProcessSymTable(Symbol,Symbol^.Items,definition^.localst);
                 end;
              end;
            end;
          typesym :
            begin
            with ptypesym(sym)^ do
              if assigned(restype.def) then
               begin
                Symbol^.TypeID:=longint(restype.def);
                case restype.def^.deftype of
                  arraydef :
                    SetDType(Symbol,GetArrayDefStr(parraydef(restype.def)));
                  enumdef :
                    SetDType(Symbol,GetEnumDefStr(penumdef(restype.def)));
                  procdef :
                    SetDType(Symbol,GetProcDefStr(pprocdef(restype.def)));
                  procvardef :
                    SetDType(Symbol,GetProcVarDefStr(pprocvardef(restype.def)));
                  objectdef :
                    with pobjectdef(restype.def)^ do
                    begin
                      ObjDef:=childof;
                      if ObjDef<>nil then
                        Symbol^.RelatedTypeID:=longint(ObjDef);{TypeNames^.Add(S);}
                      Symbol^.Flags:=(Symbol^.Flags or sfObject);
                      if is_class then
                        Symbol^.Flags:=(Symbol^.Flags or sfClass);
                      ProcessSymTable(Symbol,Symbol^.Items,pobjectdef(restype.def)^.symtable);
                    end;
                  recorddef :
                    begin
                      Symbol^.Flags:=(Symbol^.Flags or sfRecord);
                      ProcessSymTable(Symbol,Symbol^.Items,precorddef(restype.def)^.symtable);
                    end;
                  pointerdef :
                    begin
                      Symbol^.Flags:=(Symbol^.Flags or sfPointer);
                      Symbol^.RelatedTypeID:=longint(ppointerdef(restype.def)^.pointertype.def);{TypeNames^.Add(S);}
                      SetDType(Symbol,GetPointerDefStr(ppointerdef(restype.def)));
                    end;

                  filedef :
                    SetDType(Symbol,GetFileDefStr(pfiledef(restype.def)));
                  setdef :
                    SetDType(Symbol,GetSetDefStr(psetdef(restype.def)));
                end;
               end;
            end;
        end;
        Ref:=Sym^.defref;
        while Assigned(Symbol) and assigned(Ref) do
          begin
            inputfile:=get_source_file(ref^.moduleindex,ref^.posinfo.fileindex);
            if Assigned(inputfile) and Assigned(inputfile^.name) then
              begin
                New(Reference, Init(ModuleNames^.Add(inputfile^.name^),
                  ref^.posinfo.line,ref^.posinfo.column));
                Symbol^.References^.Insert(Reference);
              end;
            Ref:=Ref^.nextref;
          end;
        if Assigned(Symbol) then
          begin
            if not Owner^.Search(Symbol,J) then
              Owner^.Insert(Symbol)
            else
              begin
                Dispose(Symbol,done);
                Symbol:=nil;
              end;
          end;
        sym:=psym(sym^.indexnext);
      end;
  end;

function SearchModule(const Name: string): PModuleSymbol;
function Match(P: PModuleSymbol): boolean; {$ifndef FPC}far;{$endif}
begin
  Match:=CompareText(P^.GetName,Name)=0;
end;
var P: PModuleSymbol;
begin
  P:=nil;
  if Assigned(Modules) then
    P:=Modules^.FirstThat(@Match);
  SearchModule:=P;
end;

procedure CreateBrowserCol;
var
  T: PSymTable;
  UnitS,PM: PModuleSymbol;
  hp : pmodule;
  puu: pused_unit;
  pdu: pdependent_unit;
  pif: pinputfile;
begin
  DisposeBrowserCol;
  if (cs_browser in aktmoduleswitches) then
    NewBrowserCol;
  hp:=pmodule(loaded_units.first);
  if (cs_browser in aktmoduleswitches) then
   while assigned(hp) do
    begin
       t:=psymtable(hp^.globalsymtable);
       if assigned(t) then
         begin
           New(UnitS, Init(T^.Name^,hp^.mainsource^));
           if Assigned(hp^.loaded_from) then
             if assigned(hp^.loaded_from^.globalsymtable) then
               UnitS^.SetLoadedFrom(psymtable(hp^.loaded_from^.globalsymtable)^.name^);
{           pimportlist(current_module^.imports^.first);}

           if assigned(hp^.sourcefiles) then
           begin
             pif:=hp^.sourcefiles^.files;
             while (pif<>nil) do
             begin
               UnitS^.AddSourceFile(pif^.path^+pif^.name^);
               pif:=pif^.next;
             end;
           end;

           Modules^.Insert(UnitS);
           ProcessSymTable(UnitS,UnitS^.Items,T);
           if cs_local_browser in aktmoduleswitches then
             begin
                t:=psymtable(hp^.localsymtable);
                if assigned(t) then
                  ProcessSymTable(UnitS,UnitS^.Items,T);
             end;
         end;
       hp:=pmodule(hp^.next);
    end;

  hp:=pmodule(loaded_units.first);
  if (cs_browser in aktmoduleswitches) then
   while assigned(hp) do
    begin
       t:=psymtable(hp^.globalsymtable);
       if assigned(t) then
         begin
           UnitS:=SearchModule(T^.Name^);
           puu:=pused_unit(hp^.used_units.first);
           while (puu<>nil) do
           begin
             PM:=SearchModule(puu^.name^);
             if Assigned(PM) then
               UnitS^.AddUsedUnit(PM);
             puu:=pused_unit(puu^.next);
           end;
           pdu:=pdependent_unit(hp^.dependent_units.first);
           while (pdu<>nil) do
           begin
             PM:=SearchModule(psymtable(pdu^.u^.globalsymtable)^.name^);
             if Assigned(PM) then
               UnitS^.AddDependentUnit(PM);
             pdu:=pdependent_unit(pdu^.next);
           end;
         end;
       hp:=pmodule(hp^.next);
    end;

  if (cs_browser in aktmoduleswitches) then
    BuildObjectInfo;
  { can allways be done
    needed to know when recompilation of sources is necessary }
  BuildSourceList;
end;

procedure BuildObjectInfo;
var C,D: PIDSortedSymbolCollection;
    E : PCollection;
    ObjectC: PObjectSymbolCollection;
    ObjectsSymbol: PObjectSymbol;
procedure InsertSymbolCollection(Symbols: PSymbolCollection);
var I: sw_integer;
    P: PSymbol;
begin
  for I:=0 to Symbols^.Count-1 do
    begin
      P:=Symbols^.At(I);
      if (P^.Flags and sfObject)<>0 then
        C^.Insert(P);
      if (P^.typ=typesym) then
        D^.Insert(P);
      if (P^.typ=varsym) and ((P^.flags and sfPointer)<>0) then
        E^.Insert(P);
      if P^.Items<>nil then
        InsertSymbolCollection(P^.Items);
    end;
end;
function SearchObjectForSym(O: PSymbol): PObjectSymbol;
var I,Idx: sw_integer;
    OS,P: PObjectSymbol;
begin
  P:=nil;
  for I:=0 to ObjectC^.Count-1 do
    begin
      OS:=ObjectC^.At(I);
      if OS^.Symbol=O then
        begin P:=OS; Break; end;
    end;
  SearchObjectForSym:=P;
end;
procedure BuildTree;
var I: sw_integer;
    Symbol: PSymbol;
    Parent,OS: PObjectSymbol;
begin
  I:=0;
  while (I<C^.Count) do
    begin
      Symbol:=C^.At(I);
      if Symbol^.Ancestor=nil then
        Parent:=ObjectsSymbol
      else
        Parent:=SearchObjectForSym(Symbol^.Ancestor);
      if Parent<>nil then
        begin
          New(OS, Init(Parent, Symbol));
          Parent^.AddDescendant(OS);
          ObjectC^.Insert(OS);
          C^.AtDelete(I);
        end
      else
        Inc(I);
    end;
end;
var Pass: integer;
    I: sw_integer;
    P: PSymbol;
begin
  New(C, Init(1000,5000));
  New(D, Init(1000,5000));
  New(E, Init(1000,5000));
  InsertSymbolCollection(Modules);

  { --- Resolve ancestor<->descendant references --- }
  for I:=0 to C^.Count-1 do
    begin
      P:=C^.At(I);
      if P^.RelatedTypeID<>0 then
        P^.Ancestor:=C^.SearchSymbolByID(P^.RelatedTypeID);
    end;

  { --- Resolve pointer definition references --- }
  for I:=0 to D^.Count-1 do
    begin
      P:=D^.At(I);
      if P^.RelatedTypeID<>0 then
        P^.Ancestor:=D^.SearchSymbolByID(P^.RelatedTypeID);
    end;

  { --- Resolve  pointer var definition references --- }
  for I:=0 to E^.Count-1 do
    begin
      P:=PSymbol(E^.At(I));
      if P^.RelatedTypeID<>0 then
        P^.Ancestor:=D^.SearchSymbolByID(P^.RelatedTypeID);
    end;

  { E is not needed anymore }
  E^.DeleteAll;
  Dispose(E,Done);

  { D is not needed anymore }
  D^.DeleteAll;
  Dispose(D,Done);

  { --- Build object tree --- }
  if assigned(ObjectTree) then
    Dispose(ObjectTree, Done);
  New(ObjectsSymbol, InitName('Objects'));
  ObjectTree:=ObjectsSymbol;

  New(ObjectC, Init(C^.Count,100));

  Pass:=0;
  if C^.Count>0 then
  repeat
    BuildTree;
    Inc(Pass);
  until (C^.Count=0) or (Pass>20); { more than 20 levels ? - then there must be a bug }

  ObjectC^.DeleteAll; Dispose(ObjectC, Done);
  C^.DeleteAll; Dispose(C, Done);
end;

function SearchObjectForSymbol(O: PSymbol): PObjectSymbol;
function ScanObjectCollection(Parent: PObjectSymbol): PObjectSymbol;
var I: sw_integer;
    OS,P: PObjectSymbol;
    ObjectC: PObjectSymbolCollection;
begin
  P:=nil;
  if Parent<>nil then
  if Parent^.Descendants<>nil then
  begin
    ObjectC:=Parent^.Descendants;
    for I:=0 to ObjectC^.Count-1 do
      begin
        OS:=ObjectC^.At(I);
        if OS^.Symbol=O then
          begin P:=OS; Break; end;
        if OS^.Descendants<>nil then
          begin
            P:=ScanObjectCollection(OS);
            if P<>nil then Break;
          end;
      end;
  end;
  ScanObjectCollection:=P;
end;
begin
  SearchObjectForSymbol:=ScanObjectCollection(ObjectTree);
end;

procedure BuildSourceList;
var m: pmodule;
    s: pinputfile;
    p: cobjects.pstring;
    ppu,obj: string;
    source: string;
begin
  if Assigned(SourceFiles) then
    begin
      Dispose(SourceFiles, Done);
      SourceFiles:=nil;
    end;
  if assigned(loaded_units.first) then
  begin
    New(SourceFiles, Init(50,10));
    m:=pmodule(loaded_units.first);
    while assigned(m) do
    begin
      obj:=fexpand(m^.objfilename^);
      ppu:=''; source:='';
      if m^.is_unit then
        ppu:=fexpand(m^.ppufilename^);
      if (m^.is_unit=false) and (m^.islibrary=false) then
        ppu:=fexpand(m^.exefilename^);
      if assigned(m^.sourcefiles) then
        begin
          s:=m^.sourcefiles^.files;
          while assigned(s) do
          begin
            source:='';
            p:=s^.path;
            if assigned(p) then
              source:=source+p^;
            p:=s^.name;
            if assigned(p) then
              source:=source+p^;
            source:=fexpand(source);

            SourceFiles^.Insert(New(PSourceFile, Init(source,obj,ppu)));
            s:=s^.ref_next;
          end;
        end;
      m:=pmodule(m^.next);
    end;
  end;
end;

{*****************************************************************************
                                 Initialize
*****************************************************************************}



var
  oldexit : pointer;

procedure browcol_exit;{$ifndef FPC}far;{$endif}
begin
  exitproc:=oldexit;
  DisposeBrowserCol;
  if Assigned(SourceFiles) then
    begin
      Dispose(SourceFiles, Done);
      SourceFiles:=nil;
    end;
  if assigned(ObjectTree) then
    begin
      Dispose(ObjectTree, Done);
      ObjectTree:=nil;
    end;
end;


procedure InitBrowserCol;
begin
end;


procedure DoneBrowserCol;
begin
  { nothing, the collections are freed in the exitproc - ??? }
  { nothing? then why do we've this routine for ? IMHO, either we should
    remove this, or it should destroy the browser info when it's called. - BG }
end;

type
     PPointerXRef = ^TPointerXRef;
     TPointerXRef = record
       PtrValue : pointer;
       DataPtr  : pointer;
     end;

     PPointerDictionary = ^TPointerDictionary;
     TPointerDictionary = object(TSortedCollection)
       function  At(Index: sw_Integer): PPointerXRef;
       function  Compare(Key1, Key2: Pointer): sw_Integer; virtual;
       procedure FreeItem(Item: Pointer); virtual;
       function  SearchXRef(PtrValue: pointer): PPointerXRef;
       function  AddPtr(PtrValue, DataPtr: pointer): PPointerXRef;
       procedure Resolve(var P);
     end;

function NewPointerXRef(APtrValue, ADataPtr: pointer): PPointerXRef;
var P: PPointerXRef;
begin
  New(P); FillChar(P^,SizeOf(P^),0);
  with P^ do begin PtrValue:=APtrValue; DataPtr:=ADataPtr; end;
  NewPointerXRef:=P;
end;

procedure DisposePointerXRef(P: PPointerXRef);
begin
  if Assigned(P) then Dispose(P);
end;

function TPointerDictionary.At(Index: sw_Integer): PPointerXRef;
begin
  At:=inherited At(Index);
end;

function TPointerDictionary.Compare(Key1, Key2: Pointer): sw_Integer;
var K1: PPointerXRef absolute Key1;
    K2: PPointerXRef absolute Key2;
    R: integer;
begin
  if longint(K1^.PtrValue)<longint(K2^.PtrValue) then R:=-1 else
  if longint(K1^.PtrValue)>longint(K2^.PtrValue) then R:= 1 else
  R:=0;
  Compare:=R;
end;

procedure TPointerDictionary.FreeItem(Item: Pointer);
begin
  if Assigned(Item) then DisposePointerXRef(Item);
end;

function TPointerDictionary.SearchXRef(PtrValue: pointer): PPointerXRef;
var P: PPointerXRef;
    T: TPointerXRef;
    Index: sw_integer;
begin
  T.PtrValue:=PtrValue;
  if Search(@T,Index)=false then P:=nil else
    P:=At(Index);
  SearchXRef:=P;
end;

function TPointerDictionary.AddPtr(PtrValue, DataPtr: pointer): PPointerXRef;
var P: PPointerXRef;
begin
  P:=SearchXRef(PtrValue);
  if P=nil then
    begin
      P:=NewPointerXRef(PtrValue,DataPtr);
      Insert(P);
{$ifdef DEBUG}
    end
  else
    begin
      if P^.DataPtr<>DataPtr then
        InternalError(987654);
{$endif DEBUG}
    end;
  AddPtr:=P;
end;

procedure TPointerDictionary.Resolve(var P);
var X: PPointerXRef;
    V: pointer;
begin
  Move(P,V,SizeOf(V));
  X:=SearchXRef(V);
  if X=nil then V:=nil else
    V:=X^.DataPtr;
  Move(V,P,SizeOf(V));
end;

procedure ReadPointers(S: PStream; C: PCollection; D: PPointerDictionary);
var W,I: sw_integer;
    P: pointer;
begin
  S^.Read(W,SizeOf(W));
  for I:=0 to W-1 do
  begin
    S^.Read(P,SizeOf(P));
    D^.AddPtr(P,C^.At(I));
  end;
end;

function LoadBrowserCol(S: PStream): boolean;
var PD: PPointerDictionary;
procedure FixupPointers;
procedure FixupReference(P: PReference); {$ifndef FPC}far;{$endif}
begin
  PD^.Resolve(P^.FileName);
end;
procedure FixupSymbol(P: PSymbol); {$ifndef FPC}far;{$endif}
var I: sw_integer;
begin
  PD^.Resolve(P^.DType);
  PD^.Resolve(P^.VType);
  PD^.Resolve(P^.Params);
  if Assigned(P^.References) then
    with P^.References^ do
     for I:=0 to Count-1 do
       FixupReference(At(I));
  if Assigned(P^.Items) then
    with P^.Items^ do
     for I:=0 to Count-1 do
       FixupSymbol(At(I));
end;
begin
  Modules^.ForEach(@FixupSymbol);
end;
procedure ReadSymbolPointers(P: PSymbol); {$ifndef FPC}far;{$endif}
var I: sw_integer;
    PV: pointer;
begin
  S^.Read(PV, SizeOf(PV));
  PD^.AddPtr(PV,P);
  if Assigned(P^.Items) then
    with P^.Items^ do
     for I:=0 to Count-1 do
       ReadSymbolPointers(At(I));
end;
begin
  DisposeBrowserCol;

  New(ModuleNames, Load(S^));
  New(TypeNames, Load(S^));
  New(Modules, Load(S^));

  New(PD, Init(4000,1000));
  ReadPointers(S,ModuleNames,PD);
  ReadPointers(S,TypeNames,PD);
  ReadPointers(S,Modules,PD);
  Modules^.ForEach(@ReadSymbolPointers);
  FixupPointers;
  Dispose(PD, Done);

  BuildObjectInfo;
  LoadBrowserCol:=(S^.Status=stOK);
end;

procedure StorePointers(S: PStream; C: PCollection);
var W,I: sw_integer;
    P: pointer;
begin
  W:=C^.Count;
  S^.Write(W,SizeOf(W));
  for I:=0 to W-1 do
  begin
    P:=C^.At(I);
    S^.Write(P,SizeOf(P));
  end;
end;

function StoreBrowserCol(S: PStream) : boolean;
procedure WriteSymbolPointers(P: PSymbol); {$ifndef FPC}far;{$endif}
var I: sw_integer;
begin
  S^.Write(P, SizeOf(P));
  if Assigned(P^.Items) then
    with P^.Items^ do
     for I:=0 to Count-1 do
       WriteSymbolPointers(At(I));
end;
var W: sw_integer;
begin
  ModuleNames^.Store(S^);
  TypeNames^.Store(S^);
  Modules^.Store(S^);

  StorePointers(S,ModuleNames);
  StorePointers(S,TypeNames);
  StorePointers(S,Modules);
  Modules^.ForEach(@WriteSymbolPointers);
  StoreBrowserCol:=(S^.Status=stOK);
end;

procedure RegisterSymbols;
begin
  RegisterType(RModuleNameCollection);
  RegisterType(RTypeNameCollection);
  RegisterType(RReference);
  RegisterType(RSymbol);
  RegisterType(RObjectSymbol);
  RegisterType(RSymbolCollection);
  RegisterType(RSortedSymbolCollection);
  RegisterType(RIDSortedSymbolCollection);
  RegisterType(RObjectSymbolCollection);
  RegisterType(RReferenceCollection);
  RegisterType(RModuleSymbol);
end;

begin
  oldexit:=exitproc;
  exitproc:=@browcol_exit;
end.
{
  $Log: browcol.pas,v $
  Revision 1.1.2.7  2001/08/04 11:04:10  peter
    * browcol has no depends on ide/fv

  Revision 1.1.2.6  2001/03/22 17:30:11  pierre
   * fix an error introduced in last change

  Revision 1.1.2.5  2001/03/17 23:13:01  pierre
   * fix several memory leaks

  Revision 1.1.2.4  2001/03/16 17:48:19  pierre
   * try to remove memory leaks

  Revision 1.1.2.3  2000/11/06 16:56:46  pierre
   * fix source file list

  Revision 1.1.2.2  2000/08/18 13:18:52  pierre
   * restore next instead of indexnext field for dc local var in GetAbsProcParmDefStr

  Revision 1.1.2.1  2000/08/16 18:25:59  peter
    * splitted namedobjectitem.next into indexnext and listnext so it
      can be used in both lists
    * don't allow "word = word" type definitions

  Revision 1.1  2000/07/13 06:29:44  michael
  + Initial import

  Revision 1.43  2000/07/05 21:20:48  pierre
   + Register TModuleSymbol

  Revision 1.42  2000/07/05 10:17:38  pierre
   * avoid internalerror on open arrays

  Revision 1.41  2000/06/19 19:56:43  pierre
   * small error fix

  Revision 1.40  2000/06/16 06:08:44  pierre
   *Gabor's changes

  Revision 1.39  2000/05/29 10:04:40  pierre
    * New bunch of Gabor changes

  Revision 1.38  2000/04/20 08:52:01  pierre
   * allow to view objects having the same name

  Revision 1.37  2000/03/14 15:04:19  pierre
   * DebuggerValue moved to fpsymbol unit

  Revision 1.36  2000/03/13 20:28:12  pierre
   * X was not found in TSortedSymbolCollection.LookUp

  Revision 1.35  2000/03/08 12:25:29  pierre
   * more fixes for TSymbol

  Revision 1.34  2000/03/07 21:55:59  pierre
    * Tsymbol and Ancestor fixes

  Revision 1.33  2000/02/09 13:22:45  peter
    * log truncated

  Revision 1.32  2000/01/20 00:24:06  pierre
   * StoreBrowserCol changed to boolean function

  Revision 1.31  2000/01/07 01:14:19  peter
    * updated copyright to 2000

  Revision 1.30  1999/12/01 11:11:19  pierre
   * don't redefine sw_integer for FPC : corrected version

  Revision 1.29  1999/12/01 11:05:47  pierre
   * don't redefine sw_integer for FPC

  Revision 1.28  1999/11/30 10:40:42  peter
    + ttype, tsymlist

  Revision 1.27  1999/11/10 00:42:42  pierre
    * LookUp function now returns the complete name in browcol
      and fpsymbol only yakes a part of LoopUpStr

  Revision 1.26  1999/11/06 14:34:17  peter
    * truncated log to 20 revs

  Revision 1.25  1999/10/26 12:30:40  peter
    * const parameter is now checked
    * better and generic check if a node can be used for assigning
    * export fixes
    * procvar equal works now (it never had worked at least from 0.99.8)
    * defcoll changed to linkedlist with pparaitem so it can easily be
      walked both directions

  Revision 1.24  1999/09/16 07:54:48  pierre
   * BuildSourceList allways called for dependency in FP

  Revision 1.23  1999/09/07 15:07:49  pierre
   * avoid some infinite recursions

  Revision 1.22  1999/08/16 18:25:49  peter
    * fixes from gabor

  Revision 1.21  1999/08/09 14:09:04  peter
    * updated for symtable updates

  Revision 1.20  1999/08/03 22:02:29  peter
    * moved bitmask constants to sets
    * some other type/const renamings

}
