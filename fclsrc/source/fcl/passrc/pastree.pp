{
    $Id: pastree.pp,v 1.1 2003/03/13 21:47:42 sg Exp $
    This file is part of the Free Component Library

    Pascal parse tree classes
    Copyright (c) 2000-2003 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


unit PasTree;

interface

uses Classes;

resourcestring
  // Parse tree node type names
  SPasTreeElement = 'generic element';
  SPasTreeSection = 'unit section';
  SPasTreeModule = 'module';
  SPasTreePackage = 'package';
  SPasTreeResString = 'resource string';
  SPasTreeType = 'generic type';
  SPasTreePointerType = 'pointer type';
  SPasTreeAliasType = 'alias type';
  SPasTreeTypeAliasType = '"type" alias type';
  SPasTreeClassOfType = '"class of" type';
  SPasTreeRangeType = 'range type';
  SPasTreeArrayType = 'array type';
  SPasTreeEnumValue = 'enumeration value';
  SPasTreeEnumType = 'enumeration type';
  SPasTreeSetType = 'set type';
  SPasTreeRecordType = 'record type';
  SPasTreeObjectType = 'object';
  SPasTreeClassType = 'class';
  SPasTreeInterfaceType = 'interface';
  SPasTreeArgument = 'argument';
  SPasTreeProcedureType = 'procedure type';
  SPasTreeResultElement = 'function result';
  SPasTreeFunctionType = 'function type';
  SPasTreeUnresolvedTypeRef = 'unresolved type reference';
  SPasTreeVariable = 'variable';
  SPasTreeConst = 'constant';
  SPasTreeProperty = 'property';
  SPasTreeOverloadedProcedure = 'overloaded procedure';
  SPasTreeProcedure = 'procedure';
  SPasTreeFunction = 'function';
  SPasTreeConstructor = 'constructor';
  SPasTreeDestructor = 'destructor';
  SPasTreeProcedureImpl = 'procedure/function implementation';
  SPasTreeConstructorImpl = 'constructor implementation';
  SPasTreeDestructorImpl = 'destructor implementation';

type

  TPasModule = class;

  TPasMemberVisibility = (visDefault, visPrivate, visProtected, visPublic,
    visPublished, visAutomated);
    
  TPasMemberVisibilities = set of TPasMemberVisibility;


  TPTreeElement = class of TPasElement;

  TPasElement = class
  private
    FRefCount: LongWord;
    FName: String;
    FParent: TPasElement;
  public
    constructor Create(const AName: String; AParent: TPasElement); virtual;
    procedure AddRef;
    procedure Release;
    function FullName: String;		// Name including parent's names
    function PathName: String;		// = Module.Name + FullName
    function GetModule: TPasModule;
    function ElementTypeName: String; virtual;
    function GetDeclaration(full : Boolean) : String; virtual;
    Visibility: TPasMemberVisibility;
    property RefCount: LongWord read FRefCount;
    property Name: String read FName;
    property Parent: TPasElement read FParent;
  end;

  TPasSection = class(TPasElement)
  public
    constructor Create(const AName: String; AParent: TPasElement); override;
    destructor Destroy; override;
    function ElementTypeName: String; override;
    procedure AddUnitToUsesList(const AUnitName: String);
    UsesList: TList;		// TPasUnresolvedTypeRef or TPasModule elements
    Declarations, ResStrings, Types, Consts, Classes,
      Functions, Variables: TList;
  end;

  TPasModule = class(TPasElement)
  public
    destructor Destroy; override;
    function ElementTypeName: String; override;
    function GetDeclaration(full : boolean) : String; override;
    InterfaceSection, ImplementationSection: TPasSection;
    PackageName: String;
  end;

  TPasPackage = class(TPasElement)
  public
    constructor Create(const AName: String; AParent: TPasElement); override;
    destructor Destroy; override;
    function ElementTypeName: String; override;
    Modules: TList;	// List of TPasModule objects
  end;

  TPasResString = class(TPasElement)
  public
    function ElementTypeName: String; override;
    function GetDeclaration(full : Boolean) : String; Override;
    Value: String;
  end;

  TPasType = class(TPasElement)
  public
    function ElementTypeName: String; override;
  end;

  TPasPointerType = class(TPasType)
  public
    destructor Destroy; override;
    function ElementTypeName: String; override;
    function GetDeclaration(full : Boolean): String; override;
    DestType: TPasType;
  end;

  TPasAliasType = class(TPasType)
  public
    destructor Destroy; override;
    function ElementTypeName: String; override;
    function GetDeclaration(full : Boolean): String; override;
    DestType: TPasType;
  end;

  TPasTypeAliasType = class(TPasAliasType)
  public
    function ElementTypeName: String; override;
  end;

  TPasClassOfType = class(TPasAliasType)
  public
    function ElementTypeName: String; override;
    function GetDeclaration(full: boolean) : String; override;
  end;


  TPasRangeType = class(TPasType)
  public
    function ElementTypeName: String; override;
    function GetDeclaration(full : boolean) : String; override;
    RangeStart, RangeEnd: String;
  end;

  TPasArrayType = class(TPasType)
  public
    destructor Destroy; override;
    function ElementTypeName: String; override;
    function GetDeclaration(full : boolean) : String; override;
    IndexRange : String;
    ElType: TPasType;
  end;

  TPasEnumValue = class(TPasElement)
  public
    function ElementTypeName: String; override;
    IsValueUsed: Boolean;
    Value: Integer;
  end;

  TPasEnumType = class(TPasType)
  public
    constructor Create(const AName: String; AParent: TPasElement); override;
    destructor Destroy; override;
     function ElementTypeName: String; override;
    function GetDeclaration(full : boolean) : String; override;
    Procedure GetEnumNames(Names : TStrings);
    Values: TList;	// List of TPasEnumValue objects
  end;

  TPasSetType = class(TPasType)
  public
    destructor Destroy; override;
    function ElementTypeName: String; override;
    function GetDeclaration(full : boolean) : String; override;
    EnumType: TPasType;
  end;

  TPasRecordType = class(TPasType)
  public
    constructor Create(const AName: String; AParent: TPasElement); override;
    destructor Destroy; override;
    function ElementTypeName: String; override;
    function GetDeclaration(full : boolean) : String; override;
    IsPacked: Boolean;
    Members: TList;	// array of TPasVariable elements
  end;


  TPasObjKind = (okObject, okClass, okInterface);

  TPasClassType = class(TPasType)
  public
    constructor Create(const AName: String; AParent: TPasElement); override;
    destructor Destroy; override;
    function ElementTypeName: String; override;
    ObjKind: TPasObjKind;
    AncestorType: TPasType;	// TPasClassType or TPasUnresolvedTypeRef
    Members: TList;	// array of TPasElement objects
  end;

  TArgumentAccess = (argDefault, argConst, argVar, argOut);

  TPasArgument = class(TPasElement)
  public
    destructor Destroy; override;
    function ElementTypeName: String; override;
    function GetDeclaration(full : boolean) : String; override;
    Access: TArgumentAccess;
    ArgType: TPasType;
    Value: String;
  end;

  TPasProcedureType = class(TPasType)
  public
    constructor Create(const AName: String; AParent: TPasElement); override;
    destructor Destroy; override;
    class function TypeName: String; virtual;
    function ElementTypeName: String; override;
    IsOfObject: Boolean;
    function GetDeclaration(full : boolean) : String; override;
    procedure GetArguments(List : TStrings);
    function CreateArgument(const AName, AUnresolvedTypeName: String):
      TPasArgument;
    Args: TList;	// List of TPasArgument objects
  end;

  TPasResultElement = class(TPasElement)
  public
    destructor Destroy; override;
    function ElementTypeName : String; override;
    ResultType: TPasType;
  end;

  TPasFunctionType = class(TPasProcedureType)
  public
    destructor Destroy; override;
    class function TypeName: String; override;
    function ElementTypeName: String; override;
    function GetDeclaration(Full : boolean) : String; override;
    ResultEl: TPasResultElement;
  end;

  TPasUnresolvedTypeRef = class(TPasType)
  public
    // Typerefs cannot be parented! -> AParent _must_ be NIL
    constructor Create(const AName: String; AParent: TPasElement); override;
    function ElementTypeName: String; override;
  end;

  TPasTypeRef = class(TPasUnresolvedTypeRef)
  public
    // function GetDeclaration(full : Boolean): String; override;
    RefType: TPasType;
  end;

  TPasVariable = class(TPasElement)
  public
    destructor Destroy; override;
    function ElementTypeName: String; override;
    function GetDeclaration(full : boolean) : String; override;
    VarType: TPasType;
    Value: String;
    Modifiers : string;
  end;

  TPasConst = class(TPasVariable)
  public
    function ElementTypeName: String; override;
  end;

  TPasProperty = class(TPasVariable)
  public
    constructor Create(const AName: String; AParent: TPasElement); override;
    destructor Destroy; override;
    function ElementTypeName: String; override;
    function GetDeclaration(full : boolean) : String; override;
    Args: TList;	// List of TPasArgument objects
    IndexValue, ReadAccessorName, WriteAccessorName,
      StoredAccessorName, DefaultValue: String;
    IsDefault, IsNodefault: Boolean;
  end;

  TPasProcedureBase = class(TPasElement)
  public
    function TypeName: String; virtual; abstract;
  end;

  TPasOverloadedProc = class(TPasProcedureBase)
  public
    constructor Create(const AName: String; AParent: TPasElement); override;
    destructor Destroy; override;
    function ElementTypeName: String; override;
    function TypeName: String; override;
    Overloads: TList;		// List of TPasProcedure nodes
  end;

  TPasProcedure = class(TPasProcedureBase)
  public
    destructor Destroy; override;
    function ElementTypeName: String; override;
    function TypeName: String; override;
    ProcType: TPasProcedureType;
    function GetDeclaration(full: Boolean): String; override;
    procedure GetModifiers(List: TStrings);
    IsVirtual, IsDynamic, IsAbstract, IsOverride,
      IsOverload, IsMessage: Boolean;
  end;

  TPasFunction = class(TPasProcedure)
  public
    function ElementTypeName: String; override;
    function GetDeclaration (full : boolean) : String; override;
  end;

  TPasConstructor = class(TPasProcedure)
  public
    function ElementTypeName: String; override;
    function TypeName: String; override;
  end;

  TPasDestructor = class(TPasProcedure)
  public
    function ElementTypeName: String; override;
    function TypeName: String; override;
  end;


  TPasImplBlock = class;

  TPasProcedureImpl = class(TPasElement)
  public
    constructor Create(const AName: String; AParent: TPasElement); override;
    destructor Destroy; override;
    function ElementTypeName: String; override;
    function TypeName: String; virtual;
    ProcType: TPasProcedureType;
    Locals: TList;
    Body: TPasImplBlock;
  end;

  TPasConstructorImpl = class(TPasProcedureImpl)
  public
    function ElementTypeName: String; override;
    function TypeName: String; override;
  end;

  TPasDestructorImpl = class(TPasProcedureImpl)
  public
    function ElementTypeName: String; override;
    function TypeName: String; override;
  end;

  TPasImplElement = class(TPasElement)
  end;

  TPasImplCommand = class(TPasImplElement)
  public
    Command: String;
  end;

  TPasImplCommands = class(TPasImplElement)
  public
    constructor Create(const AName: String; AParent: TPasElement); override;
    destructor Destroy; override;
    Commands: TStrings;
  end;

  TPasImplIfElse = class(TPasImplElement)
  public
    destructor Destroy; override;
    Condition: String;
    IfBranch, ElseBranch: TPasImplElement;
  end;

  TPasImplForLoop = class(TPasImplElement)
  public
    destructor Destroy; override;
    Variable: TPasVariable;
    StartValue, EndValue: String;
    Body: TPasImplElement;
  end;

  TPasImplBlock = class(TPasImplElement)
  public
    constructor Create(const AName: String; AParent: TPasElement); override;
    destructor Destroy; override;
    function AddCommand(const ACommand: String): TPasImplCommand;
    function AddCommands: TPasImplCommands;
    function AddIfElse(const ACondition: String): TPasImplIfElse;
    function AddForLoop(AVar: TPasVariable;
      const AStartValue, AEndValue: String): TPasImplForLoop;
    Elements: TList;	// TPasImplElement objects
  end;


const
  AccessNames: array[TArgumentAccess] of String[6] = ('', 'const ', 'var ', 'out ');
  AllVisibilities: TPasMemberVisibilities = 
     [visDefault, visPrivate, visProtected, visPublic,
      visPublished, visAutomated];

  VisibilityNames: array[TPasMemberVisibility] of String = (
    'default', 'private', 'protected', 'public', 'published', 'automated');

  ObjKindNames: array[TPasObjKind] of String = (
    'object', 'class', 'interface');
  

implementation

uses SysUtils;


{ Parse tree element type name functions }

function TPasElement.ElementTypeName: String; begin Result := SPasTreeElement end;
function TPasSection.ElementTypeName: String; begin Result := SPasTreeSection end;
function TPasModule.ElementTypeName: String; begin Result := SPasTreeModule end;
function TPasPackage.ElementTypeName: String; begin Result := SPasTreePackage end;
function TPasResString.ElementTypeName: String; begin Result := SPasTreeResString end;
function TPasType.ElementTypeName: String; begin Result := SPasTreeType end;
function TPasPointerType.ElementTypeName: String; begin Result := SPasTreePointerType end;
function TPasAliasType.ElementTypeName: String; begin Result := SPasTreeAliasType end;
function TPasTypeAliasType.ElementTypeName: String; begin Result := SPasTreeTypeAliasType end;
function TPasClassOfType.ElementTypeName: String; begin Result := SPasTreeClassOfType end;
function TPasRangeType.ElementTypeName: String; begin Result := SPasTreeRangeType end;
function TPasArrayType.ElementTypeName: String; begin Result := SPasTreeArrayType end;
function TPasEnumValue.ElementTypeName: String; begin Result := SPasTreeEnumValue end;
function TPasEnumType.ElementTypeName: String; begin Result := SPasTreeEnumType end;
function TPasSetType.ElementTypeName: String; begin Result := SPasTreeSetType end;
function TPasRecordType.ElementTypeName: String; begin Result := SPasTreeRecordType end;
function TPasArgument.ElementTypeName: String; begin Result := SPasTreeArgument end;
function TPasProcedureType.ElementTypeName: String; begin Result := SPasTreeProcedureType end;
function TPasResultElement.ElementTypeName: String; begin Result := SPasTreeResultElement end;
function TPasFunctionType.ElementTypeName: String; begin Result := SPasTreeFunctionType end;
function TPasUnresolvedTypeRef.ElementTypeName: String; begin Result := SPasTreeUnresolvedTypeRef end;
function TPasVariable.ElementTypeName: String; begin Result := SPasTreeVariable end;
function TPasConst.ElementTypeName: String; begin Result := SPasTreeConst end;
function TPasProperty.ElementTypeName: String; begin Result := SPasTreeProperty end;
function TPasOverloadedProc.ElementTypeName: String; begin Result := SPasTreeOverloadedProcedure end;
function TPasProcedure.ElementTypeName: String; begin Result := SPasTreeProcedure end;
function TPasFunction.ElementTypeName: String; begin Result := SPasTreeFunction end;
function TPasConstructor.ElementTypeName: String; begin Result := SPasTreeConstructor end;
function TPasDestructor.ElementTypeName: String; begin Result := SPasTreeDestructor end;
function TPasProcedureImpl.ElementTypeName: String; begin Result := SPasTreeProcedureImpl end;
function TPasConstructorImpl.ElementTypeName: String; begin Result := SPasTreeConstructorImpl end;
function TPasDestructorImpl.ElementTypeName: String; begin Result := SPasTreeDestructorImpl end;

function TPasClassType.ElementTypeName: String;
begin
  case ObjKind of
    okObject: Result := SPasTreeObjectType;
    okClass: Result := SPasTreeClassType;
    okInterface: Result := SPasTreeInterfaceType;
  end;
end;


{ All other stuff: }


constructor TPasElement.Create(const AName: String; AParent: TPasElement);
begin
  inherited Create;
  FName := AName;
  FParent := AParent;
end;

procedure TPasElement.AddRef;
begin
  Inc(FRefCount);
end;

procedure TPasElement.Release;
begin
  if FRefCount = 0 then
    Free
  else
    Dec(FRefCount);
end;

function TPasElement.FullName: String;
var
  p: TPasElement;
begin
  Result := Name;
  p := Parent;
  while Assigned(p) and not p.InheritsFrom(TPasSection) do
  begin
    if (p.ClassType <> TPasOverloadedProc) and (Length(p.Name) > 0) then
      if Length(Result) > 0 then
        Result := p.Name + '.' + Result
      else
        Result := p.Name;
    p := p.Parent;
  end;
end;

function TPasElement.PathName: String;
var
  p: TPasElement;
begin
  Result := Name;
  p := Parent;
  while Assigned(p) do
  begin
    if (p.ClassType <> TPasOverloadedProc) and (Length(p.Name) > 0) then
      if Length(Result) > 0 then
        Result := p.Name + '.' + Result
      else
        Result := p.Name;
    p := p.Parent;
  end;
end;

function TPasElement.GetModule: TPasModule;
begin
  if ClassType = TPasPackage then
    Result := nil
  else
  begin
    Result := TPasModule(Self);
    while Assigned(Result) and not (Result.ClassType = TPasModule) do
      Result := TPasModule(Result.Parent);
  end;
end;

function TPasElement.GetDeclaration (full : boolean): String;

begin
  if Full then
    Result := Name
  else  
    Result := '';
end;

constructor TPasSection.Create(const AName: String; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  UsesList := TList.Create;
  Declarations := TList.Create;
  ResStrings := TList.Create;
  Types := TList.Create;
  Consts := TList.Create;
  Classes := TList.Create;
  Functions := TList.Create;
  Variables := TList.Create;
end;

destructor TPasSection.Destroy;
var
  i: Integer;
begin
  Variables.Free;
  Functions.Free;
  Classes.Free;
  Consts.Free;
  Types.Free;
  ResStrings.Free;

  for i := 0 to Declarations.Count - 1 do
    TPasElement(Declarations[i]).Release;
  Declarations.Free;

  for i := 0 to UsesList.Count - 1 do
    TPasType(UsesList[i]).Release;
  UsesList.Free;

  inherited Destroy;
end;

procedure TPasSection.AddUnitToUsesList(const AUnitName: String);
begin
  UsesList.Add(TPasUnresolvedTypeRef.Create(AUnitName, Self));
end;


destructor TPasModule.Destroy;
begin
  if Assigned(InterfaceSection) then
    InterfaceSection.Release;
  if Assigned(ImplementationSection) then
    ImplementationSection.Release;
  inherited Destroy;
end;


constructor TPasPackage.Create(const AName: String; AParent: TPasElement);
begin
  if (Length(AName) > 0) and (AName[1] <> '#') then
    inherited Create('#' + AName, AParent)
  else
    inherited Create(AName, AParent);
  Modules := TList.Create;
end;

destructor TPasPackage.Destroy;
var
  i: Integer;
begin
  for i := 0 to Modules.Count - 1 do
    TPasModule(Modules[i]).Release;
  Modules.Free;
  inherited Destroy;
end;


destructor TPasPointerType.Destroy;
begin
  if Assigned(DestType) then
    DestType.Release;
  inherited Destroy;
end;


destructor TPasAliasType.Destroy;
begin
  if Assigned(DestType) then
    DestType.Release;
  inherited Destroy;
end;


destructor TPasArrayType.Destroy;
begin
  if Assigned(ElType) then
    ElType.Release;
  inherited Destroy;
end;


constructor TPasEnumType.Create(const AName: String; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Values := TList.Create;
end;

destructor TPasEnumType.Destroy;
var
  i: Integer;
begin
  for i := 0 to Values.Count - 1 do
    TPasEnumValue(Values[i]).Release;
  Values.Free;
  inherited Destroy;
end;

procedure TPasEnumType.GetEnumNames(Names: TStrings);
var
  i: Integer;
begin
  with Values do
  begin
    for i := 0 to Count - 2 do
      Names.Add(TPasEnumValue(Items[i]).Name + ',');
    if Count > 0 then
      Names.Add(TPasEnumValue(Items[Count - 1]).Name);
  end;  
end;


destructor TPasSetType.Destroy;
begin
  if Assigned(EnumType) then
    EnumType.Release;
  inherited Destroy;
end;


constructor TPasRecordType.Create(const AName: String; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Members := TList.Create;
end;

destructor TPasRecordType.Destroy;
var
  i: Integer;
begin
  for i := 0 to Members.Count - 1 do
    TPasVariable(Members[i]).Release;
  Members.Free;
  inherited Destroy;
end;


constructor TPasClassType.Create(const AName: String; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Members := TList.Create;
end;

destructor TPasClassType.Destroy;
var
  i: Integer;
begin
  for i := 0 to Members.Count - 1 do
    TPasElement(Members[i]).Release;
  Members.Free;
  if Assigned(AncestorType) then
    AncestorType.Release;
  inherited Destroy;
end;


destructor TPasArgument.Destroy;
begin
  if Assigned(ArgType) then
    ArgType.Release;
  inherited Destroy;
end;


constructor TPasProcedureType.Create(const AName: String; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Args := TList.Create;
end;

destructor TPasProcedureType.Destroy;
var
  i: Integer;
begin
  for i := 0 to Args.Count - 1 do
    TPasArgument(Args[i]).Release;
  Args.Free;
  inherited Destroy;
end;

function TPasProcedureType.TypeName: String;
begin
  Result := 'procedure';
end;

function TPasProcedureType.CreateArgument(const AName,
  AUnresolvedTypeName: String): TPasArgument;
begin
  Result := TPasArgument.Create(AName, Self);
  Args.Add(Result);
  Result.ArgType := TPasUnresolvedTypeRef.Create(AUnresolvedTypeName, Result);
end;


destructor TPasResultElement.Destroy;
begin
  if Assigned(ResultType) then
    ResultType.Release;
  inherited Destroy;
end;


destructor TPasFunctionType.Destroy;
begin
  if Assigned(ResultEl) then
    ResultEl.Release;
  inherited Destroy;
end;

function TPasFunctionType.TypeName: String;
begin
  Result := 'function';
end;


constructor TPasUnresolvedTypeRef.Create(const AName: String; AParent: TPasElement);
begin
  inherited Create(AName, nil);
end;


destructor TPasVariable.Destroy;
begin
  { Attention, in derived classes, VarType isn't necessarily set!
    (e.g. in Constants) }
  if Assigned(VarType) then
    VarType.Release;
  inherited Destroy;
end;


constructor TPasProperty.Create(const AName: String; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Args := TList.Create;
end;

destructor TPasProperty.Destroy;
var
  i: Integer;
begin
  for i := 0 to Args.Count - 1 do
    TPasArgument(Args[i]).Release;
  Args.Free;
  inherited Destroy;
end;


constructor TPasOverloadedProc.Create(const AName: String; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Overloads := TList.Create;
end;

destructor TPasOverloadedProc.Destroy;
var
  i: Integer;
begin
  for i := 0 to Overloads.Count - 1 do
    TPasProcedure(Overloads[i]).Release;
  Overloads.Free;
  inherited Destroy;
end;

function TPasOverloadedProc.TypeName: String;
begin
  if Assigned(TPasProcedure(Overloads[0]).ProcType) then
    Result := TPasProcedure(Overloads[0]).ProcType.TypeName
  else
    SetLength(Result, 0);
end;


destructor TPasProcedure.Destroy;
begin
  if Assigned(ProcType) then
    ProcType.Release;
  inherited Destroy;
end;

function TPasProcedure.TypeName: String;
begin
  Result := ProcType.TypeName;
end;


function TPasConstructor.TypeName: String;
begin
  Result := 'constructor';
end;


function TPasDestructor.TypeName: String;
begin
  Result := 'destructor';
end;


constructor TPasProcedureImpl.Create(const AName: String; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Locals := TList.Create;
end;

destructor TPasProcedureImpl.Destroy;
var
  i: Integer;
begin
  if Assigned(Body) then
    Body.Release;

  for i := 0 to Locals.Count - 1 do
    TPasElement(Locals[i]).Release;
  Locals.Free;

  if Assigned(ProcType) then
    ProcType.Release;

  inherited Destroy;
end;

function TPasProcedureImpl.TypeName: String;
begin
  Result := ProcType.TypeName;
end;


function TPasConstructorImpl.TypeName: String;
begin
  Result := 'constructor';
end;

function TPasDestructorImpl.TypeName: String;
begin
  Result := 'destructor';
end;


constructor TPasImplCommands.Create(const AName: String; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Commands := TStringList.Create;
end;

destructor TPasImplCommands.Destroy;
begin
  Commands.Free;
  inherited Destroy;
end;


destructor TPasImplIfElse.Destroy;
begin
  if Assigned(IfBranch) then
    IfBranch.Release;
  if Assigned(ElseBranch) then
    ElseBranch.Release;
  inherited Destroy;
end;


destructor TPasImplForLoop.Destroy;
begin
  if Assigned(Variable) then
    Variable.Release;
  if Assigned(Body) then
    Body.Release;
  inherited Destroy;
end;


constructor TPasImplBlock.Create(const AName: String; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Elements := TList.Create;
end;

destructor TPasImplBlock.Destroy;
var
  i: Integer;
begin
  for i := 0 to Elements.Count - 1 do
    TPasImplElement(Elements[i]).Release;
  Elements.Free;
  inherited Destroy;
end;

function TPasImplBlock.AddCommand(const ACommand: String): TPasImplCommand;
begin
  Result := TPasImplCommand.Create('', Self);
  Elements.Add(Result);
  Result.Command := ACommand;
end;

function TPasImplBlock.AddCommands: TPasImplCommands;
begin
  Result := TPasImplCommands.Create('', Self);
  Elements.Add(Result);
end;

function TPasImplBlock.AddIfElse(const ACondition: String): TPasImplIfElse;
begin
  Result := TPasImplIfElse.Create('', Self);
  Elements.Add(Result);
  Result.Condition := ACondition;
end;

function TPasImplBlock.AddForLoop(AVar: TPasVariable; const AStartValue,
  AEndValue: String): TPasImplForLoop;
begin
  Result := TPasImplForLoop.Create('', Self);
  Elements.Add(Result);
  Result.Variable := AVar;
  Result.StartValue := AStartValue;
  Result.EndValue := AEndValue;
end;



{ ---------------------------------------------------------------------

  ---------------------------------------------------------------------}

function TPasModule.GetDeclaration(full : boolean): String;
begin
  Result := 'Unit ' + Name;
end;

{
function TPas.GetDeclaration : string;
begin
  Result:=Name;
end;
}

function TPasResString.GetDeclaration (full : boolean) : string;
begin
  Result:=Value;
  If Full Then
    Result:=Name+' = '+Result;
end;

function TPasPointerType.GetDeclaration (full : boolean) : string;
begin
  Result:='^'+DestType.Name;
  If Full then
    Result:=Name+' = '+Result;
end;

function TPasAliasType.GetDeclaration (full : boolean) : string;
begin
  Result:=DestType.Name;
  If Full then
    Result:=Name+' = '+Result;
end;

function TPasClassOfType.GetDeclaration (full : boolean) : string;
begin
  Result:='Class of '+DestType.Name;
  If Full then
    Result:=Name+' = '+Result;
end;

function TPasRangeType.GetDeclaration (full : boolean) : string;
begin
  Result:=RangeStart+'..'+RangeEnd;
  If Full then
    Result:=Name+' = '+Result;  
end;

function TPasArrayType.GetDeclaration (full : boolean) : string;
begin
  Result:='Array['+IndexRange+'] of ';
  If Assigned(Eltype) then
    Result:=Result+ElType.Name
  else
    Result:=Result+'const';
    If Assigned(ELtype) then
  If Full Then
    Result:=Name+' = '+Result;
end;

Function IndentStrings(S : TStrings; indent : Integer) : String;

Var
  I,CurrLen,CurrPos : Integer;
  

begin
  Result:='';
  CurrLen:=0;
  CurrPos:=0;
  For I:=0 to S.Count-1 do
    begin
    CurrLen:=Length(S[i]);
    If (CurrLen+CurrPos)>72 then
      begin
      Result:=Result+LineEnding+StringOfChar(' ',Indent);
      CurrPos:=Indent;
      end;
    Result:=Result+S[i];
    CurrPos:=CurrPos+CurrLen;
    end;
end;

function TPasEnumType.GetDeclaration (full : boolean) : string;

Var
  S : TStringList;
  i : integer;
    
begin
  S:=TStringList.Create;
  Try
    If Full then
      S.Add(Name+' = (')
    else  
      S.Add('(');
    GetEnumNames(S);  
    S[S.Count-1]:=S[S.Count-1]+')';
    If Full then      
      Result:=IndentStrings(S,Length(Name)+4)
    else
      Result:=IndentStrings(S,1);  
  finally
    S.Free;
  end;
end;

function TPasSetType.GetDeclaration (full : boolean) : string;

Var
  S : TStringList;
  i : Integer;
  
begin
  If EnumType is TPasEnumType then
    begin
    S:=TStringList.Create;
    Try
      If Full then
        S.Add(Name+'= Set of (')
      else  
        S.Add('Set of (');
      TPasEnumType(EnumType).GetEnumNames(S);
      S[S.Count-1]:=S[S.Count-1]+')';
      I:=Pos('(',S[0]);  
      Result:=IndentStrings(S,i);
    finally
      S.Free;
    end;
    end
  else
    begin
    Result:='Set of '+EnumType.Name;
    If Full then
      Result:=Name+' = '+Result;
    end;
end;

function TPasRecordType.GetDeclaration (full : boolean) : string;

Var
  S,T : TStringList;
  temp : String;
  I : integer;
    
begin
  S:=TStringList.Create;
  T:=TstringList.Create;
  Try
    Temp:='record';
    If IsPacked then
      Temp:='packed '+Temp;
    If Full then
      Temp:=Name+' = '+Temp;
    S.Add(Temp);
    For I:=0 to Members.Count-1 do
      begin
      Temp:=TPasVariable(Members[i]).GetDeclaration(True);
      If Pos(LineEnding,Temp)>0 then
        begin
        T.Text:=Temp;
        For I:=0 to T.Count-1 do
          S.Add('  '+T[i]+';');
        end
      else
        S.Add('  '+Temp+';');
      end;  
    S.Add('end');
    Result:=S.Text;  
  finally
    S.free;
    T.free;
  end;  
end;

procedure TPasProcedureType.GetArguments(List : TStrings);

Var
  T : String;
  I : Integer;
  
begin
  For I:=0 to Args.Count-1 do
    begin
    T:=AccessNames[TPasArgument(Args[i]).Access];
    T:=T+TPasArgument(Args[i]).GetDeclaration(True);
    If I=0 then
      T:='('+T;
    If I<Args.Count-1 then
      List.Add(T+';')
    else
      List.Add(T+')');  
    end;  
end;    

function TPasProcedureType.GetDeclaration (full : boolean) : string;

Var
  S : TStringList;
    
begin
  S:=TStringList.Create;
  Try
    If Full then 
      S.Add(Format('%s = ',[Name]));
    S.Add(TypeName);
    GetArguments(S);
    If IsOfObject then 
      S.Add(' of object');
    If Full then  
      Result:=IndentStrings(S,Length(S[0])+Length(S[1])+1)
    else
      Result:=IndentStrings(S,Length(S[0])+1);  
  finally
    S.Free;
  end;
end;

function TPasFunctionType.GetDeclaration (full : boolean) : string;

Var
  S : TStringList;
  T : String;
    
begin
  S:=TStringList.Create;
  Try
    If Full then 
      S.Add(Format('%s = ',[Name]));
    S.Add(TypeName);
    GetArguments(S);
    If Assigned(ResultEl) then
      begin
      T:=' : ';
      If (ResultEl.ResultType.Name<>'') then
        T:=T+ResultEl.ResultType.Name
      else
        T:=T+ResultEl.ResultType.GetDeclaration(False);
      S.Add(T);  
      end;  
    If IsOfObject then 
      S.Add(' of object');
    If Full then  
      Result:=IndentStrings(S,Length(S[0])+Length(S[1])+1)
    else
      Result:=IndentStrings(S,Length(S[0])+1);  
  finally
    S.Free;
  end;
end;

function TPasVariable.GetDeclaration (full : boolean) : string;

Const
 Seps : Array[Boolean] of String = (' = ',' : ');

begin
  If Assigned(VarType) then
    begin
    If VarType.Name='' then
      Result:=VarType.GetDeclaration(False)
    else
      Result:=VarType.Name;
    Result:=Result+Modifiers;
    end
  else
    Result:=Value;
  If Full then 
    Result:=Name+Seps[Assigned(VarType)]+Result;
end;

function TPasProperty.GetDeclaration (full : boolean) : string;

Var
  S : String;
  I : Integer;
  
begin
  If Assigned(VarType) then
    begin
    If VarType.Name='' then
      Result:=VarType.GetDeclaration(False)
    else
      Result:=VarType.Name;
    end
  else
    Result:=Value;
  S:='';
  If Assigned(Args) and (Args.Count>0) then
    begin
    For I:=0 to Args.Count-1 do
      begin
      If (S<>'') then
        S:=S+';';
      S:=S+TPasElement(Args[i]).GetDeclaration(true);
      end;
    end;
  If S<>'' then
    S:='['+S+']'
  else  
    S:=' ';
  If Full then 
    Result:=Name+S+': '+Result;
  If IsDefault then 
    Result:=Result+'; default'
end;

Procedure TPasProcedure.GetModifiers(List : TStrings);
 
  Procedure DoAdd(B : Boolean; S : String);
  
  begin
    if B then
      List.add('; '+S);
  end;

begin
  Doadd(IsVirtual,' Virtual');
  DoAdd(IsDynamic,' Dynamic');
  DoAdd(IsOverride,' Override');    
  DoAdd(IsAbstract,' Abstract');
  DoAdd(IsOverload,' Overload');
  DoAdd(IsMessage,' Message');
end;

function TPasProcedure.GetDeclaration (full : boolean) : string;

Var
  S : TStringList;
  Index : integer;
begin
  S:=TStringList.Create;
  try
    If Full then
      S.Add(TypeName+' '+Name);
    ProcType.GetArguments(S);
    GetModifiers(S);
    Result:=IndentStrings(S,Length(S[0]));
  finally
    S.Free;
  end;
end;

function TPasFunction.GetDeclaration (full : boolean) : string;

Var
  S : TStringList;
  T : String;
  
begin
  S:=TStringList.Create;
  try
    If Full then
      S.Add(TypeName+' '+Name);
    ProcType.GetArguments(S);
    If Assigned((Proctype as TPasFunctionType).ResultEl) then
      With TPasFunctionType(ProcType).ResultEl.ResultType do
        begin
        T:=' : ';
        If (Name<>'') then
          T:=T+Name
        else
          T:=T+GetDeclaration(False);
        S.Add(T);  
        end;  
    GetModifiers(S);
    Result:=IndentStrings(S,Length(S[0]));
  finally
    S.Free;
  end;
end;


function TPasArgument.GetDeclaration (full : boolean) : string;
begin
  If Assigned(ArgType) then
    begin
    If ArgType.Name<>'' then
      Result:=ArgType.Name
    else   
      Result:=ArgType.GetDeclaration(False);
    If Full then
      Result:=Name+': '+Result;
    end
  else If Full then
    Result:=Name
  else    
    Result:='';
end;

end.


{
  $Log: pastree.pp,v $
  Revision 1.1  2003/03/13 21:47:42  sg
  * First version as part of FCL

}
