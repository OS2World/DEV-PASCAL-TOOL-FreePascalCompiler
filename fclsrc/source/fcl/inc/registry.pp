Unit registry;

{$mode objfpc} 
{$H+}

interface

{$ifndef win32}
{$define XMLREG}
{$endif}

Uses
  {$ifndef XMLREG}
    Windows,
  {$endif XMLREG}
    Classes, 
    SysUtils;

  {$I regdef.inc}


type
  ERegistryException = class(Exception);

  TRegKeyInfo = record
    NumSubKeys: Integer;
    MaxSubKeyLen: Integer;
    NumValues: Integer;
    MaxValueLen: Integer;
    MaxDataLen: Integer;
    FileTime: TDateTime;
  end;

  TRegDataType = (rdUnknown, rdString, rdExpandString, rdInteger, rdBinary);

  TRegDataInfo = record
    RegData: TRegDataType;
    DataSize: Integer;
  end;

{ ---------------------------------------------------------------------
    TRegistry
  ---------------------------------------------------------------------}

  TRegistry = class(TObject)
  private
    FStringSizeIncludesNull : Boolean;
    FSysData : Pointer;
    fAccess: LongWord;
    fCurrentKey: HKEY;
    fRootKey: HKEY;
    fLazyWrite: Boolean;
    fCurrentPath: string;
    procedure SetRootKey(Value: HKEY);
    Procedure SysRegCreate;
    Procedure SysRegFree;
    Function  SysGetData(const Name: String; Buffer: Pointer; BufSize: Integer; var RegData: TRegDataType): Integer;
    Function  SysPutData(const Name: string; Buffer: Pointer; BufSize: Integer; RegData: TRegDataType) : Boolean;
    Function  SysCreateKey(const Key: String): Boolean;
  protected
    function GetBaseKey(Relative: Boolean): HKey;
    function GetData(const Name: string; Buffer: Pointer;
                  BufSize: Integer; var RegData: TRegDataType): Integer;
    function GetKey(const Key: string): HKEY;
    procedure ChangeKey(Value: HKey; const Path: string);
    procedure PutData(const Name: string; Buffer: Pointer;
                  BufSize: Integer; RegData: TRegDataType);
    procedure SetCurrentKey(Value: HKEY);
  public
    constructor Create;
    destructor Destroy; override;

    function CreateKey(const Key: string): Boolean;
    function DeleteKey(const Key: string): Boolean;
    function DeleteValue(const Name: string): Boolean;
    function GetDataInfo(const ValueName: string; var Value: TRegDataInfo): Boolean;
    function GetDataSize(const ValueName: string): Integer;
    function GetDataType(const ValueName: string): TRegDataType;
    function GetKeyInfo(var Value: TRegKeyInfo): Boolean;
    function HasSubKeys: Boolean;
    function KeyExists(const Key: string): Boolean;
    function LoadKey(const Key, FileName: string): Boolean;
    function OpenKey(const Key: string; CanCreate: Boolean): Boolean;
    function OpenKeyReadOnly(const Key: string): Boolean;
    function ReadCurrency(const Name: string): Currency;
    function ReadBinaryData(const Name: string; var Buffer; BufSize: Integer): Integer;
    function ReadBool(const Name: string): Boolean;
    function ReadDate(const Name: string): TDateTime;
    function ReadDateTime(const Name: string): TDateTime;
    function ReadFloat(const Name: string): Double;
    function ReadInteger(const Name: string): Integer;
    function ReadString(const Name: string): string;
    function ReadTime(const Name: string): TDateTime;
    function RegistryConnect(const UNCName: string): Boolean;
    function ReplaceKey(const Key, FileName, BackUpFileName: string): Boolean;
    function RestoreKey(const Key, FileName: string): Boolean;
    function SaveKey(const Key, FileName: string): Boolean;
    function UnLoadKey(const Key: string): Boolean;
    function ValueExists(const Name: string): Boolean;

    procedure CloseKey;
    procedure GetKeyNames(Strings: TStrings);
    procedure GetValueNames(Strings: TStrings);
    procedure MoveKey(const OldName, NewName: string; Delete: Boolean);
    procedure RenameValue(const OldName, NewName: string);
    procedure WriteCurrency(const Name: string; Value: Currency);
    procedure WriteBinaryData(const Name: string; var Buffer; BufSize: Integer);
    procedure WriteBool(const Name: string; Value: Boolean);
    procedure WriteDate(const Name: string; Value: TDateTime);
    procedure WriteDateTime(const Name: string; Value: TDateTime);
    procedure WriteFloat(const Name: string; Value: Double);
    procedure WriteInteger(const Name: string; Value: Integer);
    procedure WriteString(const Name, Value: string);
    procedure WriteExpandString(const Name, Value: string);
    procedure WriteTime(const Name: string; Value: TDateTime);

    property Access: LongWord read fAccess write fAccess;
    property CurrentKey: HKEY read fCurrentKey;
    property CurrentPath: string read fCurrentPath;
    property LazyWrite: Boolean read fLazyWrite write fLazyWrite;
    property RootKey: HKEY read fRootKey write SetRootKey;
    Property StringSizeIncludesNull : Boolean read FStringSizeIncludesNull;
  end;

{ ---------------------------------------------------------------------
    TRegIniFile
  ---------------------------------------------------------------------}
  TRegIniFile = class(TRegistry)
  private
    fFileName: String;
  public
    constructor Create(const FN: string);
    function ReadString(const Section, Ident, Default: string): string;
    function ReadInteger(const Section, Ident: string;
                Default: Longint): Longint;
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean;

    procedure WriteString(const Section, Ident, Value: String);
    procedure WriteInteger(const Section, Ident: string; Value: Longint);
    procedure WriteBool(const Section, Ident: string; Value: Boolean);
    procedure ReadSection(const Section: string; Strings: TStrings);
    procedure ReadSections(Strings: TStrings);
    procedure ReadSectionValues(const Section: string; Strings: TStrings);
    procedure EraseSection(const Section: string);
    procedure DeleteKey(const Section, Ident: String);

    property FileName: String read fFileName;
  end;

ResourceString
  SInvalidRegType   = 'Invalid registry data type: "%s"';
  SRegCreateFailed  = 'Failed to create key: "%s"';
  SRegSetDataFailed = 'Failed to set data for value "%s"';
  SRegGetDataFailed = 'Failed to get data for value "%s"';

implementation

{ ---------------------------------------------------------------------
    Include implementation-dependent code
  ---------------------------------------------------------------------}
  

{$ifdef XMLREG}
{$i xregreg.inc}
{$else}
{$i winreg.inc}
{$endif}

{ ---------------------------------------------------------------------
    Generic, implementation-independent code.
  ---------------------------------------------------------------------}
 

Constructor TRegistry.Create;

begin
  inherited Create;
  FAccess     := KEY_ALL_ACCESS;
  FRootKey    := HKEY_CURRENT_USER;
  FLazyWrite  := True;
  FCurrentKey := 0;
  SysRegCreate;
end;

Destructor TRegistry.Destroy;
begin
  CloseKey;
  SysRegFree;
  inherited Destroy;
end;

function TRegistry.CreateKey(const Key: String): Boolean;

begin
  Result:=SysCreateKey(Key);
  If Not Result Then 
    Raise ERegistryException.CreateFmt(SRegCreateFailed, [Key]);
end;

function TRegistry.GetBaseKey(Relative: Boolean): HKey;
begin
  If Relative Then
    Result := CurrentKey 
  else
    Result := RootKey;
end;

function TRegistry.GetData(const Name: String; Buffer: Pointer;
          BufSize: Integer; var RegData: TRegDataType): Integer;
begin
  Result:=SysGetData(Name,Buffer,BufSize,RegData);
  If (Result=-1) then
    Raise ERegistryException.CreateFmt(SRegGetDataFailed, [Name]);
end;

procedure TRegistry.PutData(const Name: string; Buffer: Pointer;
  BufSize: Integer; RegData: TRegDataType);

begin
  If Not SysPutData(Name,Buffer,BufSize,RegData) then
    Raise ERegistryException.CreateFmt(SRegSetDataFailed, [Name]);
end;


function TRegistry.GetDataSize(const ValueName: String): Integer;

Var
  Info: TRegDataInfo;

begin
  If GetDataInfo(ValueName,Info) Then
    Result := Info.DataSize 
  else
    Result := -1;
end;

function TRegistry.GetDataType(const ValueName: string): TRegDataType;

Var
  Info: TRegDataInfo;

begin
  GetDataInfo(ValueName, Info);
  Result:=Info.RegData;
end;

Function TRegistry.HasSubKeys: Boolean;

Var
  Info : TRegKeyInfo;
  
begin
  Result:=GetKeyInfo(Info);
  If Result then
    Result:=(Info.NumSubKeys>0);
end;

function TRegistry.ReadBinaryData(const Name: string; var Buffer; BufSize: Integer): Integer;

Var
  RegDataType: TRegDataType;

begin
  Result := GetData(Name, @Buffer, BufSize, RegDataType);
  If (RegDataType<>rdBinary) Then
    Raise ERegistryException.CreateFmt(SInvalidRegType, [Name]);
end;

function TRegistry.ReadInteger(const Name: string): Integer;

Var
  RegDataType: TRegDataType;

begin
  GetData(Name, @Result, SizeOf(Integer), RegDataType);
  If RegDataType<>rdInteger Then
    Raise ERegistryException.CreateFmt(SInvalidRegType, [Name]);
end;

function TRegistry.ReadBool(const Name: string): Boolean;

begin
  Result:=ReadInteger(Name)<>0;
end;

function TRegistry.ReadCurrency(const Name: string): Currency;

Var
  RegDataType: TRegDataType;

begin
  ReadBinaryData(Name, Result, SizeOf(Currency));
end;

function TRegistry.ReadDate(const Name: string): TDateTime;

begin
  ReadBinaryData(Name, Result, SizeOf(TDateTime));
  Result:=Round(Result);
end;

function TRegistry.ReadDateTime(const Name: string): TDateTime;
Var
  RegDataType: TRegDataType;

begin
  ReadBinaryData(Name, Result, SizeOf(TDateTime));
end;

function TRegistry.ReadFloat(const Name: string): Double;

begin
  ReadBinaryData(Name,Result,SizeOf(Double));
end;

function TRegistry.ReadString(const Name: string): string;

Var
  Info : TRegDataInfo;

begin
  GetDataInfo(Name,Info);
  If Not (Info.RegData in [rdString,rdExpandString]) then
    Raise ERegistryException.CreateFmt(SInvalidRegType, [Name]);
  SetLength(Result,Info.DataSize);
  If Info.DataSize>0 then
    begin
    If StringSizeIncludesNull then
      SetLength(Result, Info.DataSize-1)
    else  
      SetLength(Result, Info.DataSize);
    GetData(Name,@Result[1],Info.DataSize,Info.RegData);
    end;
end;

function TRegistry.ReadTime(const Name: string): TDateTime;

begin
  ReadBinaryData(Name, Result, SizeOf(TDateTime));
  Result:=Frac(Result);
end;

procedure TRegistry.WriteBinaryData(const Name: string; var Buffer; BufSize: Integer);
begin
  PutData(Name, @Buffer, BufSize, rdBinary);
end;

procedure TRegistry.WriteBool(const Name: string; Value: Boolean);

begin
  WriteInteger(Name,Ord(Value));
end;

procedure TRegistry.WriteCurrency(const Name: string; Value: Currency);
begin
  WriteBinaryData(Name, Value, SizeOf(Currency));
end;

procedure TRegistry.WriteDate(const Name: string; Value: TDateTime);
begin
  WriteBinarydata(Name, Value, SizeOf(TDateTime));
end;

procedure TRegistry.WriteTime(const Name: string; Value: TDateTime);
begin
  WriteBinaryData(Name, Value, SizeOf(TDateTime));
end;

procedure TRegistry.WriteDateTime(const Name: string; Value: TDateTime);
begin
  WriteBinaryData(Name, Value, SizeOf(TDateTime));
end;

procedure TRegistry.WriteExpandString(const Name, Value: string);

begin
  PutData(Name, @Value[1], Length(Value),rdExpandString);
end;

procedure TRegistry.WriteFloat(const Name: string; Value: Double);
begin
  WriteBinaryData(Name, Value, SizeOf(Double));
end;

procedure TRegistry.WriteInteger(const Name: string; Value: Integer);
begin
  PutData(Name, @Value, SizeOf(Integer), rdInteger);
end;

procedure TRegistry.WriteString(const Name, Value: string);

begin
  PutData(Name, @Value[1], Length(Value), rdString);
end;

procedure TRegistry.MoveKey(const OldName, NewName: string; Delete: Boolean);
begin

end;


{ ---------------------------------------------------------------------
    Include TRegIniFile implementation
  ---------------------------------------------------------------------}
  

{$i regini.inc}
  
end.
