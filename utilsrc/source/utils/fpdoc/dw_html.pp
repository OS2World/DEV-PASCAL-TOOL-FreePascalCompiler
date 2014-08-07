{
    $Id: dw_html.pp,v 1.4 2003/04/22 00:00:05 sg Exp $

    FPDoc  -  Free Pascal Documentation Tool
    Copyright (C) 2000 - 2003 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    * HTML/XHTML output generator

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


unit dw_HTML;

interface

uses Classes, DOM, DOM_HTML, dGlobals, PasTree, dWriter;

const
  // Subpage indices for modules
  ResstrSubindex = 1;
  ConstsSubindex = 2;
  TypesSubindex = 3;
  ClassesSubindex = 4;
  ProcsSubindex = 5;
  VarsSubindex = 6;

  // Subpage indices for classes
  PropertiesByInheritanceSubindex = 1;
  PropertiesByNameSubindex = 2;
  MethodsByInheritanceSubindex = 3;
  MethodsByNameSubindex = 4;
  EventsByInheritanceSubindex = 5;
  EventsByNameSubindex = 6;

type

  TFileAllocator = class
  public
    procedure AllocFilename(AElement: TPasElement; ASubindex: Integer); virtual;
    function GetFilename(AElement: TPasElement;
      ASubindex: Integer): String; virtual; abstract;
    function GetRelativePathToTop(AElement: TPasElement): String; virtual;
    function GetCSSFilename(ARelativeTo: TPasElement): DOMString; virtual;
  end;

  TShortNameFileAllocator = class(TFileAllocator)
  private
    FExtension: String;
  public
    constructor Create(const AExtension: String);
    procedure AllocFilename(AElement: TPasElement; ASubindex: Integer); override;
    property Extension: String read FExtension;
  end;

  TLongNameFileAllocator = class(TFileAllocator)
  private
    FExtension: String;
  public
    constructor Create(const AExtension: String);
    function GetFilename(AElement: TPasElement;
      ASubindex: Integer): String; override;
    function GetRelativePathToTop(AElement: TPasElement): String; override;
    property Extension: String read FExtension;
  end;


  TPageInfo = class
    Element: TPasElement;
    SubpageIndex: Integer;
  end;


  THTMLWriter = class(TFPDocWriter)
  private
    FAllocator: TFileAllocator;
    FPackage: TPasPackage;
    function GetPageCount: Integer;
  protected
    CurDirectory: String;	// relative to curdir of process
    BaseDirectory: String;	// relative path to package base directory
    PageInfos: TObjectList;	// list of TPageInfo objects

    Doc: THTMLDocument;
    BodyElement, TitleElement: TDOMElement;

    Module: TPasModule;

    OutputNodeStack: TList;
    CurOutputNode: TDOMNode;
    InsideHeadRow, DoPasHighlighting: Boolean;
    HighlighterFlags: Byte;

    function ResolveLinkID(const Name: String): DOMString;
    function ResolveLinkWithinPackage(AElement: TPasElement;
      ASubpageIndex: Integer): String;

    // Helper functions for creating DOM elements
    function CreateEl(Parent: TDOMNode; const AName: DOMString): THTMLElement;
    function CreatePara(Parent: TDOMNode): THTMLElement;
    function CreateH1(Parent: TDOMNode): THTMLElement;
    function CreateH2(Parent: TDOMNode): THTMLElement;
    function CreateH3(Parent: TDOMNode): THTMLElement;
    function CreateTable(Parent: TDOMNode): THTMLElement;
    function CreateContentTable(Parent: TDOMNode): THTMLElement;
    function CreateTR(Parent: TDOMNode): THTMLElement;
    function CreateTD(Parent: TDOMNode): THTMLElement;
    function CreateTD_vtop(Parent: TDOMNode): THTMLElement;
    function CreateLink(Parent: TDOMNode; const AHRef: DOMString): THTMLElement;
    function CreateAnchor(Parent: TDOMNode; const AName: DOMString): THTMLElement;
    function CreateCode(Parent: TDOMNode): THTMLElement;
    function CreateWarning(Parent: TDOMNode): THTMLElement;

    // Description node conversion
    procedure PushOutputNode(ANode: TDOMNode);
    procedure PopOutputNode;
    procedure DescrWriteText(const AText: DOMString); override;
    procedure DescrBeginBold; override;
    procedure DescrEndBold; override;
    procedure DescrBeginItalic; override;
    procedure DescrEndItalic; override;
    procedure DescrBeginEmph; override;
    procedure DescrEndEmph; override;
    procedure DescrWriteFileEl(const AText: DOMString); override;
    procedure DescrWriteKeywordEl(const AText: DOMString); override;
    procedure DescrWriteVarEl(const AText: DOMString); override;
    procedure DescrBeginLink(const AId: DOMString); override;
    procedure DescrEndLink; override;
    procedure DescrWriteLinebreak; override;
    procedure DescrBeginParagraph; override;
    procedure DescrEndParagraph; override;
    procedure DescrBeginCode(HasBorder: Boolean; const AHighlighterName: String); override;
    procedure DescrWriteCodeLine(const ALine: String); override;
    procedure DescrEndCode; override;
    procedure DescrBeginOrderedList; override;
    procedure DescrEndOrderedList; override;
    procedure DescrBeginUnorderedList; override;
    procedure DescrEndUnorderedList; override;
    procedure DescrBeginDefinitionList; override;
    procedure DescrEndDefinitionList; override;
    procedure DescrBeginListItem; override;
    procedure DescrEndListItem; override;
    procedure DescrBeginDefinitionTerm; override;
    procedure DescrEndDefinitionTerm; override;
    procedure DescrBeginDefinitionEntry; override;
    procedure DescrEndDefinitionEntry; override;
    procedure DescrBeginSectionTitle; override;
    procedure DescrBeginSectionBody; override;
    procedure DescrEndSection; override;
    procedure DescrBeginRemark; override;
    procedure DescrEndRemark; override;
    procedure DescrBeginTable(ColCount: Integer; HasBorder: Boolean); override;
    procedure DescrEndTable; override;
    procedure DescrBeginTableCaption; override;
    procedure DescrEndTableCaption; override;
    procedure DescrBeginTableHeadRow; override;
    procedure DescrEndTableHeadRow; override;
    procedure DescrBeginTableRow; override;
    procedure DescrEndTableRow; override;
    procedure DescrBeginTableCell; override;
    procedure DescrEndTableCell; override;


    procedure AppendText(Parent: TDOMNode; const AText: DOMString);
    procedure AppendNbSp(Parent: TDOMNode; ACount: Integer);
    procedure AppendSym(Parent: TDOMNode; const AText: DOMString);
    procedure AppendKw(Parent: TDOMNode; const AText: DOMString);
    function AppendPasSHFragment(Parent: TDOMNode; const AText: String;
      AShFlags: Byte): Byte;
    procedure AppendShortDescr(Parent: TDOMNode; Element: TPasElement);
    procedure AppendDescr(AContext: TPasElement; Parent: TDOMNode;
      DescrNode: TDOMElement; AutoInsertBlock: Boolean);
    procedure AppendDescrSection(AContext: TPasElement; Parent: TDOMNode;
      DescrNode: TDOMElement; const ATitle: DOMString);
    procedure AppendShortDescrCell(Parent: TDOMNode; Element: TPasElement);
    function AppendHyperlink(Parent: TDOMNode; Element: TPasElement): TDOMElement;
    function AppendType(CodeEl, TableEl: TDOMElement;
      Element: TPasType; Expanded: Boolean): TDOMElement;
    function AppendProcType(CodeEl, TableEl: TDOMElement;
      Element: TPasProcedureType; Indent: Integer): TDOMElement;
    procedure AppendProcExt(CodeEl: TDOMElement; Element: TPasProcedure);
    procedure AppendProcDecl(CodeEl, TableEl: TDOMElement;
      Element: TPasProcedureBase);
    procedure AppendProcArgsSection(Parent: TDOMNode;
      Element: TPasProcedureType);

    procedure AppendTitle(const AText: DOMString);
    procedure AppendMenuBar(ASubpageIndex: Integer);
    procedure FinishElementPage(AElement: TPasElement);

    procedure CreatePageBody(AElement: TPasElement; ASubpageIndex: Integer); virtual;
    procedure CreatePackagePageBody;
    procedure CreateModulePageBody(AModule: TPasModule; ASubpageIndex: Integer);
    procedure CreateConstPageBody(AConst: TPasConst);
    procedure CreateTypePageBody(AType: TPasType);
    procedure CreateClassPageBody(AClass: TPasClassType; ASubpageIndex: Integer);
    procedure CreateClassMemberPageBody(AElement: TPasElement);
    procedure CreateVarPageBody(AVar: TPasVariable);
    procedure CreateProcPageBody(AProc: TPasProcedureBase);
  public
    constructor Create(AEngine: TFPDocEngine; AAllocator: TFileAllocator;
      APackage: TPasPackage);
    destructor Destroy; override;

    // Single-page generation
    function CreateHTMLPage(AElement: TPasElement;
      ASubpageIndex: Integer): TXMLDocument;
    function CreateXHTMLPage(AElement: TPasElement;
      ASubpageIndex: Integer): TXMLDocument;

    // For producing complete package documentation
    procedure WriteHTMLPages;
    procedure WriteXHTMLPages;

    SearchPage: String;
    property Allocator: TFileAllocator read FAllocator;
    property Package: TPasPackage read FPackage;
    property PageCount: Integer read GetPageCount;

    property OnTest: TNotifyEvent;
  end;



implementation

uses SysUtils, XHTML, XMLWrite, HTMWrite, sh_pas;



procedure TFileAllocator.AllocFilename(AElement: TPasElement;
  ASubindex: Integer);
begin
end;

function TFileAllocator.GetRelativePathToTop(AElement: TPasElement): String;
begin
  SetLength(Result, 0);
end;

function TFileAllocator.GetCSSFilename(ARelativeTo: TPasElement): DOMString;
begin
  Result := GetRelativePathToTop(ARelativeTo) + 'fpdoc.css';
end;


constructor TShortNameFileAllocator.Create(const AExtension: String);
begin
  inherited Create;
  FExtension := AExtension;
end;

procedure TShortNameFileAllocator.AllocFilename(AElement: TPasElement;
  ASubindex: Integer);
begin
  // !!!: Add element to file list
end;


constructor TLongNameFileAllocator.Create(const AExtension: String);
begin
  inherited Create;
  FExtension := AExtension;
end;

function TLongNameFileAllocator.GetFilename(AElement: TPasElement;
  ASubindex: Integer): String;
var
  i: Integer;
begin
  if AElement.ClassType = TPasPackage then
    Result := 'index'
  else if AElement.ClassType = TPasModule then
    Result := LowerCase(AElement.Name) + PathDelim + 'index'
  else
  begin
    Result := LowerCase(AElement.PathName);
    i := 1;
    if Result[1] = '#' then
    begin
      while Result[i] <> '.' do
        Inc(i);
      Result := Copy(Result, i + 1, Length(Result));
    end;
    i := 1;
    while Result[i] <> '.' do
      Inc(i);
    Result[i] := PathDelim;
  end;

  if ASubindex > 0 then
    Result := Result + '-' + IntToStr(ASubindex);
  Result := Result + Extension;
end;

function TLongNameFileAllocator.GetRelativePathToTop(AElement: TPasElement): String;
begin
  if AElement.ClassType = TPasPackage then
    Result := ''
  else
    Result := '../';
end;



constructor THTMLWriter.Create(AEngine: TFPDocEngine; AAllocator: TFileAllocator;
  APackage: TPasPackage);

  procedure AddPage(AElement: TPasElement; ASubpageIndex: Integer);
  var
    PageInfo: TPageInfo;
  begin
    PageInfo := TPageInfo.Create;
    PageInfo.Element := AElement;
    PageInfo.SubpageIndex := ASubpageIndex;
    PageInfos.Add(PageInfo);
    Allocator.AllocFilename(AElement, ASubpageIndex);
    if ASubpageIndex = 0 then
      Engine.AddLink(AElement.PathName,
        Allocator.GetFilename(AElement, ASubpageIndex));
  end;

  procedure AddPages(AElement: TPasElement; ASubpageIndex: Integer;
    AList: TList);
  var
    i: Integer;
  begin
    if AList.Count > 0 then
    begin
      AddPage(AElement, ASubpageIndex);
      for i := 0 to AList.Count - 1 do
        AddPage(TPasElement(AList[i]), 0);
    end;
  end;

  procedure ScanModule(AModule: TPasModule);
  var
    i, j, k: Integer;
    s: String;
    ClassEl: TPasClassType;
    FPEl, AncestorMemberEl: TPasElement;
    DocNode: TDocNode;
    DidAutolink: Boolean;
  begin
    AddPage(AModule, 0);
    with AModule do
    begin
      if InterfaceSection.ResStrings.Count > 0 then
      begin
        AddPage(AModule, ResstrSubindex);
	s := Allocator.GetFilename(AModule, ResstrSubindex);
	for i := 0 to InterfaceSection.ResStrings.Count - 1 do
	  with TPasResString(InterfaceSection.ResStrings[i]) do
            Engine.AddLink(PathName, s + '#' + LowerCase(Name));
      end;
      AddPages(AModule, ConstsSubindex, InterfaceSection.Consts);
      AddPages(AModule, TypesSubindex, InterfaceSection.Types);
      if InterfaceSection.Classes.Count > 0 then
      begin
        AddPage(AModule, ClassesSubindex);
        for i := 0 to InterfaceSection.Classes.Count - 1 do
	begin
	  ClassEl := TPasClassType(InterfaceSection.Classes[i]);
          AddPage(ClassEl, 0);
	  // !!!: Only add when there are items
	  AddPage(ClassEl, PropertiesByInheritanceSubindex);
	  AddPage(ClassEl, PropertiesByNameSubindex);
	  AddPage(ClassEl, MethodsByInheritanceSubindex);
	  AddPage(ClassEl, MethodsByNameSubindex);
	  AddPage(ClassEl, EventsByInheritanceSubindex);
	  AddPage(ClassEl, EventsByNameSubindex);

          for j := 0 to ClassEl.Members.Count - 1 do
          begin
            FPEl := TPasElement(ClassEl.Members[j]);
            if ((FPEl.Visibility = visPrivate) and Engine.HidePrivate) or
	      ((FPEl.Visibility = visProtected) and Engine.HideProtected) then
	      continue;

            DocNode := Engine.FindDocNode(FPEl);
            if not Assigned(DocNode) then
            begin
              DidAutolink := False;
	      if Assigned(ClassEl.AncestorType) and
	        (ClassEl.AncestorType.ClassType = TPasClassType) then
	      begin
	        for k := 0 to TPasClassType(ClassEl.AncestorType).Members.Count - 1 do
	        begin
	          AncestorMemberEl :=
	            TPasElement(TPasClassType(ClassEl.AncestorType).Members[k]);
	          if AncestorMemberEl.Name = FPEl.Name then
	          begin
	            DocNode := Engine.FindDocNode(AncestorMemberEl);
	            if Assigned(DocNode) then
	            begin
	              DidAutolink := True;
		      Engine.AddLink(FPEl.PathName,
	    		Engine.FindAbsoluteLink(AncestorMemberEl.PathName));
	              break;
	            end;
	          end;
	        end;
	      end;
	      if not DidAutolink then
	        AddPage(FPEl, 0);
	    end else
    	      AddPage(FPEl, 0);
    	  end;
	end;
      end;
      AddPages(AModule, ProcsSubindex, InterfaceSection.Functions);
      AddPages(AModule, VarsSubindex, InterfaceSection.Variables);
    end;
  end;

var
  i: Integer;
begin
  inherited Create(AEngine);
  FAllocator := AAllocator;
  FPackage := APackage;
  OutputNodeStack := TList.Create;

  PageInfos := TObjectList.Create;

  // Allocate page for the package itself, if a name is given (i.e. <> '#')
  if Length(Package.Name) > 1 then
    AddPage(Package, 0);

  for i := 0 to Package.Modules.Count - 1 do
    ScanModule(TPasModule(Package.Modules[i]));
end;

destructor THTMLWriter.Destroy;
begin
  PageInfos.Free;
  OutputNodeStack.Free;
  inherited Destroy;
end;

function THTMLWriter.CreateHTMLPage(AElement: TPasElement;
  ASubpageIndex: Integer): TXMLDocument;
var
  HTMLEl: THTMLHtmlElement;
  HeadEl: THTMLHeadElement;
  El: TDOMElement;
begin
  Doc := THTMLDocument.Create;
  Result := Doc;
  Doc.AppendChild(Doc.CreateProcessingInstruction(
    'DOCTYPE', 'HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN"'));

  HTMLEl := Doc.CreateHtmlElement;
  Doc.AppendChild(HTMLEl);

  HeadEl := Doc.CreateHeadElement;
  HTMLEl.AppendChild(HeadEl);
  El := Doc.CreateElement('meta');
  HeadEl.AppendChild(El);
  El['http-equiv'] := 'Content-Type';
  El['content'] := 'text/html; charset=iso8859-1';
  TitleElement := Doc.CreateElement('title');
  HeadEl.AppendChild(TitleElement);
  El := Doc.CreateElement('link');

  BodyElement := Doc.CreateElement('body');
  HTMLEl.AppendChild(BodyElement);

  CreatePageBody(AElement, ASubpageIndex);

  HeadEl.AppendChild(El);
  El['rel'] := 'stylesheet';
  El['type'] := 'text/css';
  El['href'] := Allocator.GetCSSFilename(AElement);
end;

function THTMLWriter.CreateXHTMLPage(AElement: TPasElement;
  ASubpageIndex: Integer): TXMLDocument;
begin
  Result := nil;
end;

procedure CreatePath(const AFilename: String);
var
  EndIndex: Integer;
  Path: String;
begin
  EndIndex := Length(AFilename);
  while not (AFilename[EndIndex] in DirSeparators) do
  begin
    Dec(EndIndex);
    if EndIndex = 0 then
      exit;
  end;

  Path := Copy(AFilename, 1, EndIndex - 1);
  if not FileExists(Path) then
  begin
    CreatePath(Path);
    MkDir(Path);
  end;
end;

procedure THTMLWriter.WriteHTMLPages;
var
  i: Integer;
  PageDoc: TXMLDocument;
  Filename: String;
begin
  Engine.Output := IncludeTrailingBackSlash(Engine.Output);
  for i := 0 to PageInfos.Count - 1 do
    with TPageInfo(PageInfos[i]) do
    begin
      PageDoc := CreateHTMLPage(Element, SubpageIndex);
      try
        Filename := Engine.Output + Allocator.GetFilename(Element, SubpageIndex);
	CreatePath(Filename);
        WriteHTMLFile(PageDoc, Filename);
      finally
        PageDoc.Free;
      end;
    end;
end;

procedure THTMLWriter.WriteXHTMLPages;
begin
end;

{procedure THTMLWriter.CreateDoc(const ATitle: DOMString;
  AElement: TPasElement; const AFilename: String);
var
  El: TDOMElement;
  DocInfo: TDocInfo;
  CSSName: String;
begin
  Doc := TXHTMLDocument.Create;
  with TXHTMLDocument(Doc) do
  begin
    Encoding := 'ISO8859-1';
    CSSName := 'fpdoc.css';
    if Assigned(Module) then
      CSSName := '../' + CSSName;
$IFNDEF ver1_0
    StylesheetType := 'text/css';
    StylesheetHRef := CSSName;
$ENDIF
    CreateRoot(xhtmlStrict);
    with RequestHeadElement do
    begin
      AppendText(RequestTitleElement, ATitle);
      El := CreateElement('link');
      AppendChild(El);
      El['rel'] := 'stylesheet';
      El['type'] := 'text/css';
      El['href'] := CSSName;
    end;
    Self.BodyElement := RequestBodyElement('en');
  end;

  if Length(AFilename) > 0 then
  begin
    DocInfo := TDocInfo.Create;
    DocInfos.Add(DocInfo);
    DocInfo.Element := AElement;
    DocInfo.Filename := AFilename;
  end;
end;
}


{ Used for:
  - <link> elements in descriptions
  - "see also" entries
  - AppendHyperlink (for unresolved parse tree element links)
}

function THTMLWriter.ResolveLinkID(const Name: String): DOMString;
var
  i: Integer;
  ThisPackage: TLinkNode;
begin
  if Length(Name) = 0 then
  begin
    SetLength(Result, 0);
    exit;
  end;

  if Name[1] = '#' then
    Result := Engine.FindAbsoluteLink(Name)
  else
  begin
    SetLength(Result, 0);
    { Try all packages }
    ThisPackage := Engine.RootLinkNode.FirstChild;
    while Assigned(ThisPackage) do
    begin
      Result := Engine.FindAbsoluteLink(ThisPackage.Name + '.' + Name);
      if Length(Result) = 0 then
      begin
        Result := Engine.FindAbsoluteLink(Module.PathName + '.' + Name);
	// WriteLn('Searching for ', Module.PathName + '.' + Name, ' => ', Result);
        if Length(Result) = 0 then
          for i := Length(Name) downto 1 do
            if Name[i] = '.' then
	    begin
	      Result := ResolveLinkID(Copy(Name, 1, i - 1));
              exit;
	    end;
      end;
      ThisPackage := ThisPackage.NextSibling;
    end;
  end;

  if Length(Result) > 0 then
    if Copy(Result, 1, Length(CurDirectory) + 1) = CurDirectory + '/' then
      Result := Copy(Result, Length(CurDirectory) + 2, Length(Result))
    else
      Result := BaseDirectory + Result;
end;

function THTMLWriter.ResolveLinkWithinPackage(AElement: TPasElement;
  ASubpageIndex: Integer): String;
var
  ParentEl: TPasElement;
begin
  ParentEl := AElement;
  while Assigned(ParentEl) and not (ParentEl.ClassType = TPasPackage) do
    ParentEl := ParentEl.Parent;
  if Assigned(ParentEl) and (TPasPackage(ParentEl) = Engine.Package) then
  begin
    Result := Allocator.GetFilename(AElement, ASubpageIndex);
    if Copy(Result, 1, Length(CurDirectory) + 1) = CurDirectory + '/' then
      Result := Copy(Result, Length(CurDirectory) + 2, Length(Result))
    else
      Result := BaseDirectory + Result;
  end else
    SetLength(Result, 0);
end;

function THTMLWriter.CreateEl(Parent: TDOMNode;
  const AName: DOMString): THTMLElement;
begin
  Result := Doc.CreateElement(AName);
  Parent.AppendChild(Result);
end;

function THTMLWriter.CreatePara(Parent: TDOMNode): THTMLElement;
begin
  Result := CreateEl(Parent, 'p');
end;

function THTMLWriter.CreateH1(Parent: TDOMNode): THTMLElement;
begin
  Result := CreateEl(Parent, 'h1');
end;

function THTMLWriter.CreateH2(Parent: TDOMNode): THTMLElement;
begin
  Result := CreateEl(Parent, 'h2');
end;

function THTMLWriter.CreateH3(Parent: TDOMNode): THTMLElement;
begin
  Result := CreateEl(Parent, 'h3');
end;

function THTMLWriter.CreateTable(Parent: TDOMNode): THTMLElement;
begin
  Result := CreateEl(Parent, 'table');
  Result['cellspacing'] := '0';
  Result['cellpadding'] := '0';
end;

function THTMLWriter.CreateContentTable(Parent: TDOMNode): THTMLElement;
begin
  Result := CreateEl(Parent, 'table');
end;

function THTMLWriter.CreateTR(Parent: TDOMNode): THTMLElement;
begin
  Result := CreateEl(Parent, 'tr');
end;

function THTMLWriter.CreateTD(Parent: TDOMNode): THTMLElement;
begin
  Result := CreateEl(Parent, 'td');
end;

function THTMLWriter.CreateTD_vtop(Parent: TDOMNode): THTMLElement;
begin
  Result := CreateEl(Parent, 'td');
  Result['valign'] := 'top';
end;

function THTMLWriter.CreateLink(Parent: TDOMNode;
  const AHRef: DOMString): THTMLElement;
begin
  Result := CreateEl(Parent, 'a');
  Result['href'] := AHRef;
end;

function THTMLWriter.CreateAnchor(Parent: TDOMNode;
  const AName: DOMString): THTMLElement;
begin
  Result := CreateEl(Parent, 'a');
  Result['name'] := AName;
end;

function THTMLWriter.CreateCode(Parent: TDOMNode): THTMLElement;
begin
  Result := CreateEl(CreateEl(Parent, 'tt'), 'nobr');
end;

function THTMLWriter.CreateWarning(Parent: TDOMNode): THTMLElement;
begin
  Result := CreateEl(Parent, 'span');
  Result['class'] := 'warning';
end;


procedure THTMLWriter.PushOutputNode(ANode: TDOMNode);
begin
  OutputNodeStack.Add(CurOutputNode);
  CurOutputNode := ANode;
end;

procedure THTMLWriter.PopOutputNode;
begin
  CurOutputNode := TDOMNode(OutputNodeStack[OutputNodeStack.Count - 1]);
  OutputNodeStack.Delete(OutputNodeStack.Count - 1);
end;

procedure THTMLWriter.DescrWriteText(const AText: DOMString);
begin
  AppendText(CurOutputNode, AText);
end;

procedure THTMLWriter.DescrBeginBold;
begin
  PushOutputNode(CreateEl(CurOutputNode, 'b'));
end;

procedure THTMLWriter.DescrEndBold;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrBeginItalic;
begin
  PushOutputNode(CreateEl(CurOutputNode, 'i'));
end;

procedure THTMLWriter.DescrEndItalic;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrBeginEmph;
begin
  PushOutputNode(CreateEl(CurOutputNode, 'em'));
end;

procedure THTMLWriter.DescrEndEmph;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrWriteFileEl(const AText: DOMString);
var
  NewEl: TDOMElement;
begin
  NewEl := CreateEl(CurOutputNode, 'span');
  NewEl['class'] := 'file';
  AppendText(NewEl, AText);
end;

procedure THTMLWriter.DescrWriteKeywordEl(const AText: DOMString);
var
  NewEl: TDOMElement;
begin
  NewEl := CreateEl(CurOutputNode, 'span');
  NewEl['class'] := 'kw';
  AppendText(NewEl, AText);
end;

procedure THTMLWriter.DescrWriteVarEl(const AText: DOMString);
begin
  AppendText(CreateEl(CurOutputNode, 'var'), AText);
end;

procedure THTMLWriter.DescrBeginLink(const AId: DOMString);
var
  s: DOMString;
begin
  s := ResolveLinkID(AId);
  if Length(s) = 0 then
  begin
    WriteLn(Format(SErrUnknownLinkID, [AId]));
    PushOutputNode(CreateEl(CurOutputNode, 'b'));
  end else
    PushOutputNode(CreateLink(CurOutputNode, s));
end;

procedure THTMLWriter.DescrEndLink;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrWriteLinebreak;
begin
  CreateEl(CurOutputNode, 'br');
end;

procedure THTMLWriter.DescrBeginParagraph;
begin
  PushOutputNode(CreatePara(CurOutputNode));
end;

procedure THTMLWriter.DescrEndParagraph;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrBeginCode(HasBorder: Boolean; const AHighlighterName: String);
begin
  DoPasHighlighting := (AHighlighterName = '') or (AHighlighterName = 'Pascal');
  HighlighterFlags := 0;
  PushOutputNode(CreateEl(CurOutputNode, 'pre'));
end;

procedure THTMLWriter.DescrWriteCodeLine(const ALine: String);
begin
  if DoPasHighlighting then
  begin
    HighlighterFlags := AppendPasSHFragment(CurOutputNode, ALine,
      HighlighterFlags);
    AppendText(CurOutputNode, #10);
  end else
    AppendText(CurOutputNode, ALine + #10);
end;

procedure THTMLWriter.DescrEndCode;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrBeginOrderedList;
begin
  PushOutputNode(CreateEl(CurOutputNode, 'ol'));
end;

procedure THTMLWriter.DescrEndOrderedList;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrBeginUnorderedList;
begin
  PushOutputNode(CreateEl(CurOutputNode, 'ul'));
end;

procedure THTMLWriter.DescrEndUnorderedList;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrBeginDefinitionList;
begin
  PushOutputNode(CreateEl(CurOutputNode, 'dl'));
end;

procedure THTMLWriter.DescrEndDefinitionList;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrBeginListItem;
begin
  PushOutputNode(CreateEl(CurOutputNode, 'li'));
end;

procedure THTMLWriter.DescrEndListItem;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrBeginDefinitionTerm;
begin
  PushOutputNode(CreateEl(CurOutputNode, 'dt'));
end;

procedure THTMLWriter.DescrEndDefinitionTerm;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrBeginDefinitionEntry;
begin
  PushOutputNode(CreateEl(CurOutputNode, 'dd'));
end;

procedure THTMLWriter.DescrEndDefinitionEntry;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrBeginSectionTitle;
begin
  PushOutputNode(CreateEl(CurOutputNode, 'h3'));
end;

procedure THTMLWriter.DescrBeginSectionBody;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrEndSection;
begin
end;

procedure THTMLWriter.DescrBeginRemark;
var
  NewEl, TDEl: TDOMElement;
begin
  NewEl := CreateEl(CurOutputNode, 'table');
  NewEl['width'] := '100%';
  NewEl['border'] := '0';
  NewEl['CellSpacing'] := '0';
  NewEl['class'] := 'remark';
  NewEl := CreateTR(NewEl);
  TDEl := CreateTD(NewEl);
  TDEl['valign'] := 'top';
  TDEl['class'] := 'pre';
  AppendText(CreateEl(TDEl, 'b'), SDocRemark);
  PushOutputNode(CreateTD(NewEl));
end;

procedure THTMLWriter.DescrEndRemark;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrBeginTable(ColCount: Integer; HasBorder: Boolean);
var
  Table: TDOMElement;
begin
  Table := CreateEl(CurOutputNode, 'table');
  Table['border'] := IntToStr(Ord(HasBorder));
  PushOutputNode(Table);
end;

procedure THTMLWriter.DescrEndTable;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrBeginTableCaption;
begin
  PushOutputNode(CreateEl(CurOutputNode, 'caption'));
end;

procedure THTMLWriter.DescrEndTableCaption;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrBeginTableHeadRow;
begin
  PushOutputNode(CreateTr(CurOutputNode));
  InsideHeadRow := True;
end;

procedure THTMLWriter.DescrEndTableHeadRow;
begin
  InsideHeadRow := False;
  PopOutputNode;
end;

procedure THTMLWriter.DescrBeginTableRow;
begin
  PushOutputNode(CreateTR(CurOutputNode));
end;

procedure THTMLWriter.DescrEndTableRow;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrBeginTableCell;
begin
  if InsideHeadRow then
    PushOutputNode(CreateEl(CurOutputNode, 'th'))
  else
    PushOutputNode(CreateTD(CurOutputNode));
end;

procedure THTMLWriter.DescrEndTableCell;
begin
  PopOutputNode;
end;




procedure THTMLWriter.AppendText(Parent: TDOMNode; const AText: DOMString);
begin
  Parent.AppendChild(Doc.CreateTextNode(AText));
end;

procedure THTMLWriter.AppendNbSp(Parent: TDOMNode; ACount: Integer);
begin
  while ACount > 0 do
  begin
    Parent.AppendChild(Doc.CreateEntityReference('nbsp'));
    Dec(ACount);
  end;
end;

procedure THTMLWriter.AppendSym(Parent: TDOMNode; const AText: DOMString);
var
  El: TDOMElement;
begin
  El := CreateEl(Parent, 'span');
  El['class'] := 'sym';
  AppendText(El, AText);
end;

procedure THTMLWriter.AppendKw(Parent: TDOMNode; const AText: DOMString);
var
  El: TDOMElement;
begin
  El := CreateEl(Parent, 'span');
  El['class'] := 'kw';
  AppendText(El, AText);
end;

function THTMLWriter.AppendPasSHFragment(Parent: TDOMNode;
  const AText: String; AShFlags: Byte): Byte;
var
  CurParent: TDOMNode;
  Line, Last, p: PChar;
  IsInSpecial: Boolean;
  El: TDOMElement;
begin
  GetMem(Line, Length(AText) * 3 + 4);
  DoPascalHighlighting(AShFlags, PChar(AText), Line);
  Result := AShFlags;

  CurParent := Parent;
  IsInSpecial := False;
  Last := Line;
  p := Line;
  while p[0] <> #0 do
  begin
    if p[0] = LF_ESCAPE then
    begin
      p[0] := #0;
      AppendText(CurParent, Last);

      if IsInSpecial then
        CurParent := Parent;
      case Ord(p[1]) of
	shDefault:
	  IsInSpecial := False;
	shInvalid:
	  begin
	    El := CreateEl(CurParent, 'font');
	    El['color'] := 'red';
	    CurParent := El;
	    IsInSpecial := True;
	  end;
	shSymbol:
	  begin
	    El := CreateEl(CurParent, 'span');
	    El['class'] := 'sym';
	    CurParent := El;
	    IsInSpecial := True;
	  end;
	shKeyword:
	  begin
	    El := CreateEl(CurParent, 'span');
	    El['class'] := 'kw';
	    CurParent := El;
	    IsInSpecial := True;
	  end;
	shComment:
	  begin
	    El := CreateEl(CurParent, 'span');
	    El['class'] := 'cmt';
	    CurParent := El;
	    IsInSpecial := True;
	  end;
	shDirective:
	  begin
	    El := CreateEl(CurParent, 'span');
	    El['class'] := 'dir';
	    CurParent := El;
	    IsInSpecial := True;
	  end;
	shNumbers:
	  begin
	    El := CreateEl(CurParent, 'span');
	    El['class'] := 'num';
	    CurParent := El;
	    IsInSpecial := True;
	  end;
	shCharacters:
	  begin
	    El := CreateEl(CurParent, 'span');
	    El['class'] := 'chr';
	    CurParent := El;
	    IsInSpecial := True;
	  end;
	shStrings:
	  begin
	    El := CreateEl(CurParent, 'span');
	    El['class'] := 'str';
	    CurParent := El;
	    IsInSpecial := True;
	  end;
	shAssembler:
	  begin
	    El := CreateEl(CurParent, 'span');
	    El['class'] := 'asm';
	    CurParent := El;
	    IsInSpecial := True;
	  end;
      end;
      Last := p + 2;
    end;
    Inc(p);
  end;
  if Last <> p then
    AppendText(CurParent, Last);
  FreeMem(Line);
end;

procedure THTMLWriter.AppendShortDescr(Parent: TDOMNode; Element: TPasElement);
var
  DocNode: TDocNode;
begin
  DocNode := Engine.FindDocNode(Element);
  if Assigned(DocNode) and Assigned(DocNode.ShortDescr) then
  begin
    PushOutputNode(Parent);
    try
      if not ConvertShort(Element, DocNode.ShortDescr) then
        WriteLn(SErrInvalidShortDescr);
    finally
      PopOutputNode;
    end;
  end;
end;

procedure THTMLWriter.AppendDescr(AContext: TPasElement; Parent: TDOMNode;
  DescrNode: TDOMElement; AutoInsertBlock: Boolean);
begin
  if Assigned(DescrNode) then
  begin
    PushOutputNode(Parent);
    try
      ConvertDescr(AContext, DescrNode, AutoInsertBlock);
    finally
      PopOutputNode;
    end;
  end;
end;

procedure THTMLWriter.AppendDescrSection(AContext: TPasElement;
  Parent: TDOMNode; DescrNode: TDOMElement; const ATitle: DOMString);
begin
  if not IsDescrNodeEmpty(DescrNode) then
  begin
    AppendText(CreateH2(Parent), ATitle);
    AppendDescr(AContext, Parent, DescrNode, True);
  end;
end;


procedure THTMLWriter.AppendShortDescrCell(Parent: TDOMNode;
  Element: TPasElement);
var
  ParaEl: TDOMElement;
begin
  if Assigned(Engine.FindShortDescr(Element)) then
  begin
    AppendNbSp(CreatePara(CreateTD(Parent)), 2);
    ParaEl := CreatePara(CreateTD(Parent));
    ParaEl['class'] := 'cmt';
    AppendShortDescr(ParaEl, Element);
  end;
end;

function THTMLWriter.AppendHyperlink(Parent: TDOMNode;
  Element: TPasElement): TDOMElement;
var
  s: String;
  UnitList: TList;
  i: Integer;
  ThisPackage: TLinkNode;
begin
  if Assigned(Element) then
  begin
    if Element.InheritsFrom(TPasUnresolvedTypeRef) then
    begin
      s := ResolveLinkID(Element.Name);
      if Length(s) = 0 then
      begin
	{ Try all packages }
	ThisPackage := Engine.RootLinkNode.FirstChild;
	while Assigned(ThisPackage) do
	begin
	  s := ResolveLinkID(ThisPackage.Name + '.' + Element.Name);
	  if Length(s) > 0 then
	    break;
	  ThisPackage := ThisPackage.NextSibling;
	end;
        if Length(s) = 0 then
        begin
          { Okay, then we have to try all imported units of the current module }
          UnitList := Module.InterfaceSection.UsesList;
          for i := UnitList.Count - 1 downto 0 do
          begin
	    { Try all packages }
	    ThisPackage := Engine.RootLinkNode.FirstChild;
	    while Assigned(ThisPackage) do
	    begin
	      s := ResolveLinkID(ThisPackage.Name + '.' +
	        TPasType(UnitList[i]).Name + '.' + Element.Name);
	      if Length(s) > 0 then
	        break;
	      ThisPackage := ThisPackage.NextSibling;
	    end;
	    if Length(s) > 0 then
	      break;
	  end;
	end;
      end;
    end else
      s := ResolveLinkID(Element.PathName);

    if Length(s) > 0 then
    begin
      Result := CreateLink(Parent, s);
      AppendText(Result, Element.Name);
    end else
    begin
      Result := nil;
      AppendText(Parent, Element.Name);
    end;
  end else
  begin
    Result := nil;
    AppendText(CreateWarning(Parent), '<NIL>');
  end;
end;


{ Returns the new CodeEl, which will be the old CodeEl in most cases }
function THTMLWriter.AppendType(CodeEl, TableEl: TDOMElement;
  Element: TPasType; Expanded: Boolean): TDOMElement;
begin
  Result := CodeEl;

  if not Assigned(Element) then
    AppendText(CreateWarning(CodeEl), '<NIL>')
  else if (not Expanded) and (Length(Element.Name) > 0) then
    AppendHyperlink(CodeEl, Element)
  else
  // Array
  if Element.ClassType = TPasArrayType then
  begin
    AppendPasSHFragment(CodeEl,
      'array [' + TPasArrayType(Element).IndexRange + '] of ', 0);
    Result := AppendType(CodeEl, TableEl, TPasArrayType(Element).ElType, False);
  end else
  // Procedure or funtion type
  if Element.InheritsFrom(TPasProcedureType) then
  begin
    AppendKw(CodeEl, TPasProcedureType(Element).TypeName);
    Result := AppendProcType(CodeEl, TableEl, TPasProcedureType(Element), 0)
  end else
  // Range type
  if Element.InheritsFrom(TPasRangeType) then
    AppendPasSHFragment(CodeEl, TPasRangeType(Element).RangeStart + '..' +
      TPasRangeType(Element).RangeEnd, 0)
  else
  // Other types
    AppendHyperlink(CodeEl, Element);
end;

function THTMLWriter.AppendProcType(CodeEl, TableEl: TDOMElement;
  Element: TPasProcedureType; Indent: Integer): TDOMElement;

  function CreateIndentedCodeEl(Indent: Integer): TDOMElement;
  begin
    Result := CreateCode(CreatePara(CreateTD(CreateTR(TableEl))));
    AppendNbSp(Result, Indent);
  end;

var
  i: Integer;
  Arg: TPasArgument;
begin
  if Element.Args.Count > 0 then
  begin
    AppendSym(CodeEl, '(');

    for i := 0 to Element.Args.Count - 1 do
    begin
      Arg := TPasArgument(Element.Args[i]);
      CodeEl := CreateIndentedCodeEl(Indent + 2);

      case Arg.Access of
	argConst: AppendKw(CodeEl, 'const ');
	argVar: AppendKw(CodeEl, 'var ');
	argOut: AppendKw(CodeEl, 'out ');
      end;
      AppendText(CodeEl, Arg.Name);
      if Assigned(Arg.ArgType) then
      begin
	AppendSym(CodeEl, ': ');
	CodeEl := AppendType(CodeEl, TableEl, Arg.ArgType, False);
      end;
      if Length(Arg.Value) > 0 then
        AppendPasSHFragment(CodeEl, ' = ' + Arg.Value, 0);
      if i < Element.Args.Count - 1 then
	AppendSym(CodeEl, ';');
    end;

    if Element.InheritsFrom(TPasFunctionType) or Element.IsOfObject then
    begin
      CodeEl := CreateIndentedCodeEl(Indent);
      if Element.InheritsFrom(TPasFunctionType) then
      begin
	AppendSym(CodeEl, '):');
	AppendHyperlink(CodeEl, TPasFunctionType(Element).ResultEl.ResultType);
      end else
	AppendSym(CodeEl, ')');
      if Element.IsOfObject then
      begin
	AppendText(CodeEl, ' ');	// Don't remove
	AppendKw(CodeEl, 'of object');
      end;
    end else
      if Indent > 0 then
	AppendSym(CodeEl, ')')
      else
      begin
        CodeEl := CreateCode(CreatePara(CreateTD(CreateTR(TableEl))));
        AppendSym(CodeEl, ')');
      end;
  end else
  begin
    { Procedure or function without arguments }
    if Element.InheritsFrom(TPasFunctionType) then
    begin
      AppendSym(CodeEl, ': ');
      AppendHyperlink(CodeEl, TPasFunctionType(Element).ResultEl.ResultType);
    end;
    if Element.IsOfObject then
      AppendKw(CodeEl, ' of object');
  end;
  Result := CodeEl;
end;

procedure THTMLWriter.AppendProcExt(CodeEl: TDOMElement;
  Element: TPasProcedure);

  procedure AppendExt(const Ext: String);
  begin
    AppendKw(CodeEl, ' ' + Ext);
    AppendSym(CodeEl, ';');
  end;

begin
  if Element.IsVirtual then
    AppendExt('virtual');
  if Element.IsDynamic then
    AppendExt('dynamic');
  if Element.IsAbstract then
    AppendExt('abstract');
  if Element.IsOverride then
    AppendExt('override');
  if Element.IsOverload then
    AppendExt('overload');
  if Element.IsMessage then
    AppendExt('message');
end;


{ Used in two places:
  - Page for the method of a class
  - Page for a tandalone procedure or function. }

procedure THTMLWriter.AppendProcDecl(CodeEl, TableEl: TDOMElement;
  Element: TPasProcedureBase);

  procedure WriteVariant(AProc: TPasProcedure);
  begin
    AppendProcArgsSection(TableEl.ParentNode, AProc.ProcType);

    AppendKw(CodeEl, AProc.TypeName);
    if Element.Parent.ClassType = TPasClassType then
    begin
      AppendText(CodeEl, ' ');
      AppendHyperlink(CodeEl, Element.Parent);
      AppendSym(CodeEl, '.');
      AppendText(CodeEl, AProc.Name);
    end else
      AppendText(CodeEl, ' ' + AProc.FullName);
    CodeEl := AppendProcType(CodeEl, TableEl, AProc.ProcType, 0);
    AppendSym(CodeEl, ';');
    AppendProcExt(CodeEl, AProc);
  end;

var
  i: Integer;
begin
  if Element.ClassType = TPasOverloadedProc then
    for i := 0 to TPasOverloadedProc(Element).Overloads.Count - 1 do
    begin
      if i > 0 then
      begin
        CreateEl(CodeEl, 'br');
        CreateEl(CodeEl, 'br');
      end;
      WriteVariant(TPasProcedure(TPasOverloadedProc(Element).Overloads[i]));
    end
  else
    WriteVariant(TPasProcedure(Element));
end;

procedure THTMLWriter.AppendProcArgsSection(Parent: TDOMNode;
  Element: TPasProcedureType);
var
  HasFullDescr, IsFirst: Boolean;
  ResultEl: TPasResultElement;
  ArgTableEl, TREl: TDOMElement;
  DocNode: TDocNode;
  i: Integer;
  Arg: TPasArgument;
begin
  IsFirst := True;
  for i := 0 to Element.Args.Count - 1 do
  begin
    Arg := TPasArgument(Element.Args[i]);
    if IsDescrNodeEmpty(Engine.FindShortDescr(Arg)) then
      continue;
    if IsFirst then
    begin
      IsFirst := False;
      AppendText(CreateH2(Parent), SDocArguments);
      ArgTableEl := CreateTable(Parent);
    end;
    TREl := CreateTR(ArgTableEl);
    AppendText(CreateCode(CreatePara(CreateTD_vtop(TREl))), Arg.Name);
    AppendShortDescrCell(TREl, Arg);
  end;

  if Element.ClassType = TPasFunctionType then
  begin
    ResultEl := TPasFunctionType(Element).ResultEl;
    DocNode := Engine.FindDocNode(ResultEl);
    HasFullDescr := Assigned(DocNode) and not IsDescrNodeEmpty(DocNode.Descr);
    if HasFullDescr or
      (Assigned(DocNode) and not IsDescrNodeEmpty(DocNode.ShortDescr)) then
    begin
      AppendText(CreateH2(Parent), SDocFunctionResult);
      if HasFullDescr then
	AppendDescr(ResultEl, Parent, DocNode.Descr, True)
      else
	AppendDescr(ResultEl, CreatePara(Parent), DocNode.ShortDescr, False);
    end;
  end;
end;

procedure THTMLWriter.AppendTitle(const AText: DOMString);
begin
  AppendText(TitleElement, AText);
  AppendText(CreateH1(BodyElement), AText);
end;

procedure THTMLWriter.AppendMenuBar(ASubpageIndex: Integer);
var
  TableEl, TREl, ParaEl, TitleEl: TDOMElement;

  procedure AddLink(ALinkSubpageIndex: Integer; const AName: String);
  begin
    AppendText(ParaEl, '[');
    if ALinkSubpageIndex = ASubpageIndex then
      AppendText(ParaEl, AName)
    else
      AppendText(
        CreateLink(ParaEl, ResolveLinkWithinPackage(Module, ALinkSubpageIndex)),
	AName);
    AppendText(ParaEl, ']');
  end;

begin
  TableEl := CreateEl(BodyElement, 'table');
  TableEl['cellpadding'] := '4';
  TableEl['cellspacing'] := '0';
  TableEl['border'] := '0';
  TableEl['width'] := '100%';
  TableEl['class'] := 'bar';
  TREl := CreateTR(TableEl);
  ParaEl := CreateEl(CreateTD(TREl), 'b');

  if Assigned(Module) then
  begin
    AddLink(0, SDocOverview);
    if Module.InterfaceSection.ResStrings.Count > 0 then
      AddLink(ResstrSubindex, SDocResStrings);
    if Module.InterfaceSection.Consts.Count > 0 then
      AddLink(ConstsSubindex, SDocConstants);
    if Module.InterfaceSection.Types.Count > 0 then
      AddLink(TypesSubindex, SDocTypes);
    if Module.InterfaceSection.Classes.Count > 0 then
      AddLink(ClassesSubindex, SDocClasses);
    if Module.InterfaceSection.Functions.Count > 0 then
      AddLink(ProcsSubindex, SDocProceduresAndFunctions);
    if Module.InterfaceSection.Variables.Count > 0 then
      AddLink(VarsSubindex, SDocVariables);
  end;

  if Length(SearchPage) > 0 then
  begin
    AppendText(ParaEl, '[');
    AppendText(CreateLink(ParaEl, SearchPage), SDocSearch);
    AppendText(ParaEl, ']');
  end;
  ParaEl := CreateTD(TREl);
  ParaEl['align'] := 'right';
  TitleEl := CreateEl(ParaEl, 'span');
  TitleEl['class'] := 'bartitle';
  if Assigned(Module) then
    AppendText(TitleEl, Format(SDocUnitTitle, [Module.Name]));
  if Assigned(Package) then
  begin
    AppendText(TitleEl, ' (');
    AppendHyperlink(TitleEl, Package);
    AppendText(TitleEl, ')');
  end;
end;

procedure THTMLWriter.FinishElementPage(AElement: TPasElement);
var
  DocNode: TDocNode;
  IsFirstSeeAlso: Boolean;
  Node: TDOMNode;
  TableEl, El, TREl, TDEl, ParaEl, NewEl, DescrEl: TDOMElement;
  s: String;
  f: Text;
begin
  DocNode := Engine.FindDocNode(AElement);
  if Assigned(DocNode) and Assigned(DocNode.Descr) then
    AppendDescrSection(AElement, BodyElement, DocNode.Descr, SDocDescription);
  // ###

  // Append "Errors" section
  if Assigned(DocNode) and Assigned(DocNode.ErrorsDoc) then
    AppendDescrSection(AElement, BodyElement, DocNode.ErrorsDoc, SDocErrors);

      // Append "See also" section
      if Assigned(DocNode) and Assigned(DocNode.SeeAlso) then
      begin
        IsFirstSeeAlso := True;
	Node := DocNode.SeeAlso.FirstChild;
	while Assigned(Node) do
	begin
	  if (Node.NodeType = ELEMENT_NODE) and (Node.NodeName = 'link') then
	  begin
	    if IsFirstSeeAlso then
	    begin
	      IsFirstSeeAlso := False;
	      AppendText(CreateH2(BodyElement), SDocSeeAlso);
	      TableEl := CreateTable(BodyElement);
	    end;

	    El := TDOMElement(Node);
	    TREl := CreateTR(TableEl);
	    ParaEl := CreatePara(CreateTD_vtop(TREl));
	    s := ResolveLinkID(El['id']);
	    if Length(s) = 0 then
	    begin
	      WriteLn(Format(SErrUnknownLinkID, [El['id']]));
	      NewEl := CreateEl(ParaEl, 'b')
	    end else
	      NewEl := CreateLink(ParaEl, s);
	    AppendText(NewEl, El['id']);

	    DescrEl := Engine.FindShortDescr(AElement.GetModule, El['id']);
	    if Assigned(DescrEl) then
	    begin
	      AppendNbSp(CreatePara(CreateTD(TREl)), 2);
	      ParaEl := CreatePara(CreateTD(TREl));
	      ParaEl['class'] := 'cmt';

	      PushOutputNode(ParaEl);
	      try
		ConvertShort(AElement, DescrEl);
	      finally
		PopOutputNode;
	      end;
	    end;
	  end;
	  Node := Node.NextSibling;
	end;
      end;

      // Append examples, if present
      if Assigned(DocNode) and Assigned(DocNode.FirstExample) then
      begin
        Node := DocNode.FirstExample;
	while Assigned(Node) do
	begin
	  if (Node.NodeType = ELEMENT_NODE) and (Node.NodeName = 'example') then
	  begin
	    AppendText(CreateH2(BodyElement), SDocExample);
	    try
	      Assign(f, Engine.GetExampleFilename(TDOMElement(Node)));
	      Reset(f);
	      try
	        PushOutputNode(BodyElement);
		DescrBeginCode(False, TDOMElement(Node)['highlighter']);
	        while not EOF(f) do
	        begin
	          ReadLn(f, s);
		  DescrWriteCodeLine(s);
	        end;
		DescrEndCode;
	        PopOutputNode;
	      finally
	        Close(f);
	      end;
	    except
	      on e: Exception do
	      begin
	        e.Message := '[example] ' + e.Message;
	        raise;
	      end;
	    end;
	  end;
	  Node := Node.NextSibling;
	end;
      end;
end;

procedure THTMLWriter.CreatePageBody(AElement: TPasElement;
  ASubpageIndex: Integer);
var
  i: Integer;
  Element: TPasElement;
begin
  CurDirectory := Allocator.GetFilename(AElement, ASubpageIndex);
  i := Length(CurDirectory);
  while (i > 0) and not (CurDirectory[i] in DirSeparators) do
    Dec(i);
  CurDirectory := Copy(CurDirectory, 1, i);
  BaseDirectory := Allocator.GetRelativePathToTop(AElement);

  if AElement.ClassType = TPasPackage then
    CreatePackagePageBody
  else
  begin
    Element := AElement;
    while Element.ClassType <> TPasModule do
      Element := Element.Parent;
    Module := TPasModule(Element);

    if AElement.ClassType = TPasModule then
      CreateModulePageBody(TPasModule(AElement), ASubpageIndex)
    else if AElement.Parent.InheritsFrom(TPasClassType) then
      CreateClassMemberPageBody(AElement)
    else if AElement.ClassType = TPasConst then
      CreateConstPageBody(TPasConst(AElement))
    else if AElement.InheritsFrom(TPasClassType) then
      CreateClassPageBody(TPasClassType(AElement), ASubpageIndex)
    else if AElement.InheritsFrom(TPasType) then
      CreateTypePageBody(TPasType(AElement))
    else if AElement.ClassType = TPasVariable then
      CreateVarPageBody(TPasVariable(AElement))
    else if AElement.InheritsFrom(TPasProcedureBase) then
      CreateProcPageBody(TPasProcedure(AElement));
  end;
end;

procedure THTMLWriter.CreatePackagePageBody;
var
  DocNode: TDocNode;
  TableEl, TREl: TDOMElement;
  i: Integer;
  ThisModule: TPasModule;
begin
  AppendMenuBar(0);
  AppendTitle(Format(SDocPackageTitle, [Copy(Package.Name, 2, 256)]));
  AppendShortDescr(CreatePara(BodyElement), Package);

  AppendText(CreateH2(BodyElement), SDocUnits);
  TableEl := CreateTable(BodyElement);
  for i := 0 to Package.Modules.Count - 1 do
  begin
    ThisModule := TPasModule(Package.Modules[i]);
    TREl := CreateTR(TableEl);
    AppendHyperlink(CreateCode(CreatePara(CreateTD_vtop(TREl))), ThisModule);
    AppendShortDescrCell(TREl, ThisModule);
  end;

  DocNode := Engine.FindDocNode(Package);
  if Assigned(DocNode) and Assigned(DocNode.Descr) then
    AppendDescrSection(nil, BodyElement, DocNode.Descr, SDocDescription);
end;

procedure THTMLWriter.CreateModulePageBody(AModule: TPasModule;
  ASubpageIndex: Integer);

  procedure CreateMainPage;
  var
    TableEl, TREl, TDEl, CodeEl: TDOMElement;
    i: Integer;
    UnitRef: TPasType;
    DocNode: TDocNode;
  begin
    AppendMenuBar(0);
    AppendTitle(Format(SDocUnitTitle, [AModule.Name]));
    AppendShortDescr(CreatePara(BodyElement), AModule);

    if AModule.InterfaceSection.UsesList.Count > 0 then
    begin
      TableEl := CreateTable(BodyElement);
      AppendKw(CreateCode(CreatePara(CreateTD(CreateTR(TableEl)))), 'uses');
      for i := 0 to AModule.InterfaceSection.UsesList.Count - 1 do
      begin
        UnitRef := TPasType(AModule.InterfaceSection.UsesList[i]);
        DocNode := Engine.FindDocNode(UnitRef);
        if Assigned(DocNode) and DocNode.IsSkipped then
          continue;
        TREl := CreateTR(TableEl);
        TDEl := CreateTD_vtop(TREl);
        CodeEl := CreateCode(CreatePara(TDEl));
        AppendNbSp(CodeEl, 2);
        AppendHyperlink(CodeEl, UnitRef);
        if i < AModule.InterfaceSection.UsesList.Count - 1 then
          AppendSym(CodeEl, ',')
        else
          AppendSym(CodeEl, ';');
        AppendText(CodeEl, '  ');		// Space for descriptions
        AppendShortDescrCell(TREl, UnitRef);
      end;
    end;

    DocNode := Engine.FindDocNode(AModule);
    if Assigned(DocNode) and Assigned(DocNode.Descr) then
      AppendDescrSection(AModule, BodyElement, DocNode.Descr, SDocOverview);
  end;

  procedure CreateSimpleSubpage(const ATitle: DOMString; AList: TList);
  var
    TableEl, TREl, TDEl, CodeEl: TDOMElement;
    i, j: Integer;
    Decl: TPasElement;
    SortedList: TList;
    DocNode: TDocNode;
  begin
    AppendMenuBar(ASubpageIndex);
    AppendTitle(Format(SDocUnitTitle + ': %s', [AModule.Name, ATitle]));
    SortedList := TList.Create;
    try
      for i := 0 to AList.Count - 1 do
      begin
        Decl := TPasElement(AList[i]);
        DocNode := Engine.FindDocNode(Decl);
        if (not Assigned(DocNode)) or (not DocNode.IsSkipped) then
        begin
          j := 0;
          while (j < SortedList.Count) and (CompareText(
	    TPasElement(SortedList[j]).PathName, Decl.PathName) < 0) do
            Inc(j);
          SortedList.Insert(j, Decl);
        end;
      end;

      TableEl := CreateTable(BodyElement);
      for i := 0 to SortedList.Count - 1 do
      begin
        Decl := TPasElement(SortedList[i]);
        TREl := CreateTR(TableEl);
        CodeEl := CreateCode(CreatePara(CreateTD_vtop(TREl)));
        AppendHyperlink(CodeEl, Decl);
        AppendShortDescrCell(TREl, Decl);
      end;
    finally
      SortedList.Free;
    end;
  end;

  procedure CreateResStringsPage;
  var
    ParaEl: TDOMElement;
    i, j: Integer;
    Decl: TPasResString;
    DocNode: TDocNode;
  begin
    AppendMenuBar(ResstrSubindex);
    AppendTitle(Format(SDocUnitTitle + ': %s', [AModule.Name, SDocResStrings]));
    for i := 0 to AModule.InterfaceSection.ResStrings.Count - 1 do
    begin
      Decl := TPasResString(AModule.InterfaceSection.ResStrings[i]);
      CreateEl(BodyElement, 'a')['name'] := LowerCase(Decl.Name);
      ParaEl := CreatePara(BodyElement);
      AppendText(CreateCode(ParaEl), Decl.Name);
      CreateEl(ParaEl, 'br');
      AppendText(ParaEl, Decl.Value);
    end;
  end;

begin
  case ASubpageIndex of
    0:
      CreateMainPage;
    ResstrSubindex:
      CreateResStringsPage;
    ConstsSubindex:
      CreateSimpleSubpage(SDocConstants, AModule.InterfaceSection.Consts);
    TypesSubindex:
      CreateSimpleSubpage(SDocTypes, AModule.InterfaceSection.Types);
    ClassesSubindex:
      CreateSimpleSubpage(SDocClasses, AModule.InterfaceSection.Classes);
    ProcsSubindex:
      CreateSimpleSubpage(SDocProceduresAndFunctions, AModule.InterfaceSection.Functions);
    VarsSubindex:
      CreateSimpleSubpage(SDocVariables, AModule.InterfaceSection.Variables);
  end;
end;

procedure THTMLWriter.CreateConstPageBody(AConst: TPasConst);
var
  TableEl, CodeEl: TDOMElement;
begin
  AppendMenuBar(-1);
  AppendTitle(AConst.Name);
  AppendShortDescr(CreatePara(BodyElement), AConst);
  AppendText(CreateH2(BodyElement), SDocDeclaration);

  TableEl := CreateTable(BodyElement);
  CodeEl := CreateCode(CreatePara(CreateTD(CreateTR(TableEl))));

  AppendKw(CodeEl, 'const');
  AppendText(CodeEl, ' ' + AConst.Name);
  if Assigned(AConst.VarType) then
  begin
    AppendSym(CodeEl, ': ');
    AppendType(CodeEl, TableEl, AConst.VarType, False);
  end;
  AppendPasSHFragment(CodeEl, ' = ' + AConst.Value + ';', 0);

  FinishElementPage(AConst);
end;

procedure THTMLWriter.CreateTypePageBody(AType: TPasType);
var
  TableEl, TREl, TDEl, CodeEl: TDOMElement;
  DocNode: TDocNode;
  i: Integer;
  s: String;
  EnumType: TPasEnumType;
  EnumValue: TPasEnumValue;
  Variable: TPasVariable;
begin
  AppendMenuBar(-1);
  AppendTitle(AType.Name);
  AppendShortDescr(CreatePara(BodyElement), AType);
  AppendText(CreateH2(BodyElement), SDocDeclaration);

  TableEl := CreateTable(BodyElement);
  TREl := CreateTR(TableEl);
  TDEl := CreateTD(TREl);
  CodeEl := CreateCode(CreatePara(TDEl));
  AppendKw(CodeEl, 'type ');
  AppendText(CodeEl, AType.Name);
  AppendSym(CodeEl, ' = ');

  // Alias
  if AType.ClassType = TPasAliasType then
  begin
    if Assigned(TPasAliasType(AType).DestType) then
      AppendHyperlink(CodeEl, TPasAliasType(AType).DestType)
    else
      AppendText(CreateWarning(CodeEl), '<Destination type is NIL>');
    AppendSym(CodeEl, ';');
  end else
  // Class of
  if AType.ClassType = TPasClassOfType then
  begin
    AppendKw(CodeEl, 'class of ');
    AppendHyperlink(CodeEl, TPasClassOfType(AType).DestType);
    AppendSym(CodeEl, ';');
  end else
  // Enumeration
  if AType.ClassType = TPasEnumType then
  begin
    AppendSym(CodeEl, '(');
    for i := 0 to TPasEnumType(AType).Values.Count - 1 do
    begin
      EnumValue := TPasEnumValue(TPasEnumType(AType).Values[i]);
      TREl := CreateTR(TableEl);
      CodeEl := CreateCode(CreatePara(CreateTD_vtop(TREl)));
      AppendShortDescrCell(TREl, EnumValue);
      AppendNbSp(CodeEl, 2);
      s := EnumValue.Name;
      if EnumValue.IsValueUsed then
        s := s + ' = ' + IntToStr(EnumValue.Value);
      if i < TPasEnumType(AType).Values.Count - 1 then
        s := s + ',';
      AppendPasSHFragment(CodeEl, s, 0);
    end;
    AppendSym(CreateCode(CreatePara(CreateTD(CreateTR(TableEl)))), ');');
  end else
  // Pointer type
  if AType.ClassType = TPasPointerType then
  begin
    AppendSym(CodeEl, '^');
    if Assigned(TPasPointerType(AType).DestType) then
      AppendHyperlink(CodeEl, TPasPointerType(AType).DestType)
    else
      AppendText(CreateWarning(CodeEl), '<Destination type is NIL>');
    AppendSym(CodeEl, ';');
  end else
  if AType.InheritsFrom(TPasProcedureType) then
  begin
    AppendSym(AppendType(CodeEl, TableEl, TPasType(AType), True), ';');
    AppendProcArgsSection(BodyElement, TPasProcedureType(AType));
  end else
  // Record
  if AType.ClassType = TPasRecordType then
  begin
    if TPasRecordType(AType).IsPacked then
      AppendKw(CodeEl, 'packed record')
    else
      AppendKw(CodeEl, 'record');

    for i := 0 to TPasRecordType(AType).Members.Count - 1 do
    begin
      Variable := TPasVariable(TPasRecordType(AType).Members[i]);
      TREl := CreateTR(TableEl);
      CodeEl := CreateCode(CreatePara(CreateTD_vtop(TREl)));
      AppendShortDescrCell(TREl, Variable);
      AppendNbSp(CodeEl, 2);
      AppendText(CodeEl, Variable.Name);
      AppendSym(CodeEl, ': ');
      AppendType(CodeEl, TableEl, Variable.VarType, False);
      AppendSym(CodeEl, ';');
    end;

    CodeEl := CreateCode(CreatePara(CreateTD(CreateTR(TableEl))));
    AppendText(CodeEl, ' '); // !!!: Dirty trick, necessary for current XML writer
    AppendKw(CodeEl, 'end');
    AppendSym(CodeEl, ';');
  end else
  // Set
  if AType.ClassType = TPasSetType then
  begin
    AppendKw(CodeEl, 'set of ');
    if TPasSetType(AType).EnumType.ClassType = TPasEnumType then
    begin
      AppendSym(CodeEl, '(');
      EnumType := TPasEnumType(TPasSetType(AType).EnumType);
      for i := 0 to EnumType.Values.Count - 1 do
      begin
        EnumValue := TPasEnumValue(EnumType.Values[i]);
        TREl := CreateTR(TableEl);
        CodeEl := CreateCode(CreatePara(CreateTD_vtop(TREl)));
        AppendShortDescrCell(TREl, EnumValue);
	AppendNbSp(CodeEl, 2);
	s := EnumValue.Name;
        if EnumValue.IsValueUsed then
	  s := s + ' = ' + IntToStr(EnumValue.Value);
        if i < EnumType.Values.Count - 1 then
	  s := s + ',';
        AppendPasSHFragment(CodeEl, s, 0);
      end;
      AppendSym(CreateCode(CreatePara(CreateTD(CreateTR(TableEl)))), ');');
    end else
    begin
      AppendHyperlink(CodeEl, TPasSetType(AType).EnumType);
      AppendSym(CodeEl, ';');
    end;
  end else
  // Type alias
  if AType.ClassType = TPasTypeAliasType then
  begin
    AppendKw(CodeEl, 'type ');
    AppendHyperlink(CodeEl, TPasTypeAliasType(AType).DestType);
    AppendSym(CodeEl, ';');
  end else
  // Probably one of the simple types, which allowed in other places as wel...
    AppendSym(AppendType(CodeEl, TableEl, TPasType(AType), True), ';');

  FinishElementPage(AType);
end;


function PropertyFilter(AMember: TPasElement): Boolean;
begin
  Result := (AMember.ClassType = TPasProperty) and
    (Copy(AMember.Name, 1, 2) <> 'On');
end;

function MethodFilter(AMember: TPasElement): Boolean;
begin
  Result := AMember.InheritsFrom(TPasProcedureBase);
end;

function EventFilter(AMember: TPasElement): Boolean;
begin
  Result := (AMember.ClassType = TPasProperty) and
    (Copy(AMember.Name, 1, 2) = 'On');
end;

procedure THTMLWriter.CreateClassPageBody(AClass: TPasClassType;
  ASubpageIndex: Integer);
type
  TMemberFilter = function(AMember: TPasElement): Boolean;
var
  ParaEl: TDOMElement;

  procedure AppendMemberListLink(AListSubpageIndex: Integer;
    const AText: DOMString);
  var
    LinkEl: TDOMElement;
  begin
    AppendText(ParaEl, '[');
    LinkEl := CreateEl(ParaEl, 'a');
    LinkEl['href'] :=
      ResolveLinkWithinPackage(AClass, AListSubpageIndex);
    LinkEl['onClick'] := 'window.open(''' + LinkEl['href'] + ''', ''list'', ' +
     '''dependent=yes,resizable=yes,scrollbars=yes,height=400,width=300''); return false;';
    AppendText(LinkEl, AText);
    AppendText(ParaEl, ' (');
    LinkEl := CreateEl(ParaEl, 'a');
    LinkEl['href'] :=
      ResolveLinkWithinPackage(AClass, AListSubpageIndex + 1);
    LinkEl['onClick'] := 'window.open(''' + LinkEl['href'] + ''', ''list'', ' +
     '''dependent=yes,resizable=yes,scrollbars=yes,height=400,width=300''); return false;';
    AppendText(LinkEl, SDocByName);
    AppendText(ParaEl, ')');
    AppendText(ParaEl, '] ');
  end;

  procedure CreateMainPage;
  var
    TableEl, TREl, TDEl, CodeEl: TDOMElement;
    DocNode: TDocNode;
    Member: TPasElement;
    CurVisibility: TPasMemberVisibility;
    i: Integer;
    s: String;
    ThisClass: TPasClassType;
    HaveSeenTObject: Boolean;
  begin
    AppendMenuBar(-1);
    AppendTitle(AClass.Name);

    ParaEl := CreatePara(BodyElement);
    AppendMemberListLink(PropertiesByInheritanceSubindex, SDocProperties);
    AppendMemberListLink(MethodsByInheritanceSubindex, SDocMethods);
    AppendMemberListLink(EventsByInheritanceSubindex, SDocEvents);

    AppendShortDescr(CreatePara(BodyElement), AClass);
    AppendText(CreateH2(BodyElement), SDocDeclaration);

    TableEl := CreateTable(BodyElement);

    TREl := CreateTR(TableEl);
    TDEl := CreateTD(TREl);
    CodeEl := CreateCode(CreatePara(TDEl));
    AppendKw(CodeEl, 'type');
    AppendText(CodeEl, ' ' + AClass.Name + ' ');
    AppendSym(CodeEl, '=');
    AppendText(CodeEl, ' ');
    AppendKw(CodeEl, ObjKindNames[AClass.ObjKind]);

    if Assigned(AClass.AncestorType) then
    begin
      AppendSym(CodeEl, '(');
      AppendHyperlink(CodeEl, AClass.AncestorType);
      AppendSym(CodeEl, ')');
    end;

    if AClass.Members.Count > 0 then
    begin
      CurVisibility := visDefault;
      for i := 0 to AClass.Members.Count - 1 do
      begin
        Member := TPasElement(AClass.Members[i]);
        if CurVisibility <> Member.Visibility then
        begin
          CurVisibility := Member.Visibility;
  	  if ((CurVisibility = visPrivate) and Engine.HidePrivate) or
	    ((CurVisibility = visProtected) and Engine.HideProtected) then
	    continue;
	  case CurVisibility of
	    visPrivate: s := 'private';
	    visProtected: s := 'protected';
	    visPublic: s := 'public';
	    visPublished: s := 'published';
	    visAutomated: s := 'automated';
	  end;
	  AppendKw(CreateCode(CreatePara(CreateTD(CreateTR(TableEl)))), s);
        end else
	  if ((CurVisibility = visPrivate) and Engine.HidePrivate) or
	    ((CurVisibility = visProtected) and Engine.HideProtected) then
	    continue;

        TREl := CreateTR(TableEl);
        CodeEl := CreateCode(CreatePara(CreateTD_vtop(TREl)));
        AppendNbSp(CodeEl, 2);
        AppendShortDescrCell(TREl, Member);

        if Member.InheritsFrom(TPasProcedureBase) then
        begin
	  AppendKw(CodeEl, TPasProcedureBase(Member).TypeName + ' ');
	  AppendHyperlink(CodeEl, Member);
	  if (Member.ClassType = TPasOverloadedProc) or
	    (TPasProcedure(Member).ProcType.Args.Count > 0) then
	    AppendSym(CodeEl, '();')
	  else
	    AppendSym(CodeEl, ';');
	  if Member.ClassType <> TPasOverloadedProc then
	    AppendProcExt(CodeEl, TPasProcedure(Member));
        end else
        if Member.ClassType = TPasVariable then
        begin
	  AppendHyperlink(CodeEl, Member);
	  AppendSym(CodeEl, ': ');
	  AppendHyperlink(CodeEl, TPasVariable(Member).VarType);
	  AppendSym(CodeEl, ';');
        end else
        if Member.ClassType = TPasProperty then
        begin
          AppendKw(CodeEl, 'property ');
	  AppendHyperlink(CodeEl, Member);
	  if Assigned(TPasProperty(Member).VarType) then
	  begin
	    AppendSym(CodeEl, ': ');
	    AppendHyperlink(CodeEl, TPasProperty(Member).VarType);
	  end;
	  AppendSym(CodeEl, ';');
	  if TPasProperty(Member).IsDefault then
	  begin
	    AppendKw(CodeEl, ' default');
	    AppendSym(CodeEl, ';');
	  end;
	  SetLength(s, 0);
	  if Length(TPasProperty(Member).ReadAccessorName) > 0 then
	    s := s + 'r';
	  if Length(TPasProperty(Member).WriteAccessorName) > 0 then
	    s := s + 'w';
	  if Length(TPasProperty(Member).StoredAccessorName) > 0 then
	    s := s + 's';
	  if Length(s) > 0 then
	    AppendText(CodeEl, '  [' + s + ']');
        end else
          AppendText(CreateWarning(CodeEl), '<' + Member.ClassName + '>');
      end;

      CodeEl := CreateCode(CreatePara(CreateTD(CreateTR(TableEl))));
    end;

    AppendText(CodeEl, ' '); // !!!: Dirty trick, necessary for current XML writer
    AppendKw(CodeEl, 'end');
    AppendSym(CodeEl, ';');


    AppendText(CreateH2(BodyElement), SDocInheritance);
    TableEl := CreateTable(BodyElement);
    HaveSeenTObject := AClass.ObjKind <> okClass;
    ThisClass := AClass;
    while True do
    begin
      TREl := CreateTR(TableEl);
      TDEl := CreateTD_vtop(TREl);
      TDEl['align'] := 'center';
      CodeEl := CreateCode(CreatePara(TDEl));
      AppendHyperlink(CodeEl, ThisClass);
      AppendShortDescrCell(TREl, ThisClass);
      if HaveSeenTObject or (CompareText(ThisClass.Name, 'TObject') = 0) then
        HaveSeenTObject := True
      else
      begin
        TDEl := CreateTD(CreateTR(TableEl));
        TDEl['align'] := 'center';
        AppendText(TDEl, '|');
      end;

      if Assigned(ThisClass.AncestorType) then
      begin
        if ThisClass.AncestorType.InheritsFrom(TPasClassType) then
          ThisClass := TPasClassType(ThisClass.AncestorType)
        else
        begin
          TDEl := CreateTD(CreateTR(TableEl));
	  TDEl['align'] := 'center';
          AppendText(CreateCode(CreatePara(TDEl)), ThisClass.AncestorType.Name);
	  if CompareText(ThisClass.AncestorType.Name, 'TObject') = 0 then
	    HaveSeenTObject := True
	  else
	  begin
            TDEl := CreateTD(CreateTR(TableEl));
	    TDEl['align'] := 'center';
	    AppendText(TDEl, '?');
	  end;
          break;
        end
      end else
        break;
    end;

    if not HaveSeenTObject then
    begin
      TDEl := CreateTD(CreateTR(TableEl));
      TDEl['align'] := 'center';
      AppendText(CreateCode(CreatePara(TDEl)), 'TObject');
    end;

    FinishElementPage(AClass);
  end;

  procedure CreateInheritanceSubpage(AFilter: TMemberFilter);
  var
    ThisClass: TPasClassType;
    i: Integer;
    Member: TPasElement;
    TableEl, TREl, TDEl, ParaEl, LinkEl: TDOMElement;
  begin
    TableEl := CreateTable(BodyElement);
    ThisClass := AClass;
    while True do
    begin
      TREl := CreateTR(TableEl);
      TDEl := CreateTD(TREl);
      TDEl['colspan'] := '3';
      CreateTD(TREl);
      LinkEl := AppendHyperlink(CreateEl(CreateCode(CreatePara(TDEl)), 'b'), ThisClass);
      if Assigned(LinkEl) then
	LinkEl['onClick'] := 'opener.location.href = ''' + LinkEl['href'] +
	  '''; return false;';
      for i := 0 to ThisClass.Members.Count - 1 do
      begin
        Member := TPasElement(ThisClass.Members[i]);
        if ((Member.Visibility = visPrivate) and Engine.HidePrivate) or
	  ((Member.Visibility = visProtected) and Engine.HideProtected) or
	  not AFilter(Member) then
	  continue;
	TREl := CreateTR(TableEl);
	ParaEl := CreatePara(CreateTD(TREl));
	case Member.Visibility of
	  visPrivate:
	    AppendText(ParaEl, 'pv');
	  visProtected:
	    AppendText(ParaEl, 'pt');
	  visPublished:
	    AppendText(ParaEl, 'pl');
	end;
	AppendNbSp(ParaEl, 1);

	ParaEl := CreateTD(TREl);
	if (Member.ClassType = TPasProperty) and
	  (Length(TPasProperty(Member).WriteAccessorName) = 0) then
	begin
	  AppendText(ParaEl, 'ro');
	  AppendNbSp(ParaEl, 1);
	end;

        LinkEl := AppendHyperlink(CreatePara(CreateTD(TREl)), Member);
	if Assigned(LinkEl) then
	  LinkEl['onClick'] := 'opener.location.href = ''' + LinkEl['href'] +
	    '''; return false;';
      end;
      if (not Assigned(ThisClass.AncestorType)) or
        (not (ThisClass.AncestorType.ClassType = TPasClassType)) then
	break;
      ThisClass := TPasClassType(ThisClass.AncestorType);
      AppendNbSp(CreatePara(CreateTD(CreateTR(TableEl))), 1);
    end;
  end;

  procedure CreateSortedSubpage(AFilter: TMemberFilter);
  var
    List: TList;
    ThisClass: TPasClassType;
    i, j: Integer;
    Member: TPasElement;
    TableEl, TREl, TDEl, ParaEl, LinkEl: TDOMElement;
  begin
    List := TList.Create;
    try
      ThisClass := AClass;
      while True do
      begin
        for i := 0 to ThisClass.Members.Count - 1 do
        begin
          Member := TPasElement(ThisClass.Members[i]);
          if (not (((Member.Visibility = visPrivate) and Engine.HidePrivate) or
	    ((Member.Visibility = visProtected) and Engine.HideProtected))) and
	    AFilter(Member) then
	  begin
    	    j := 0;
	    while (j < List.Count) and
              (CompareText(TPasElement(List[j]).Name, Member.Name) < 0) do
	      Inc(j);
	    List.Insert(j, Member);
	  end;
	end;
        if (not Assigned(ThisClass.AncestorType)) or
          (not (ThisClass.AncestorType.ClassType = TPasClassType)) then
	  break;
        ThisClass := TPasClassType(ThisClass.AncestorType);
      end;

      TableEl := CreateTable(BodyElement);
      for i := 0 to List.Count - 1 do
      begin
        Member := TPasElement(List[i]);
        TREl := CreateTR(TableEl);
        ParaEl := CreatePara(CreateTD(TREl));
        case Member.Visibility of
	  visPrivate:
	    AppendText(ParaEl, 'pv');
	  visProtected:
	    AppendText(ParaEl, 'pt');
	  visPublished:
	    AppendText(ParaEl, 'pl');
        end;
        AppendNbSp(ParaEl, 1);

        ParaEl := CreatePara(CreateTD(TREl));
        if (Member.ClassType = TPasProperty) and
	  (Length(TPasProperty(Member).WriteAccessorName) = 0) then
	begin
	  AppendText(ParaEl, 'ro');
	  AppendNbSp(ParaEl, 1);
	end;

	TDEl := CreateTD(TREl);
	TDEl['nowrap'] := 'nowrap';
	ParaEl := CreatePara(TDEl);
        LinkEl := AppendHyperlink(ParaEl, Member);
	if Assigned(LinkEl) then
	  LinkEl['onClick'] := 'opener.location.href = ''' + LinkEl['href'] +
	    '''; return false;';
	AppendText(ParaEl, ' (');
        LinkEl := AppendHyperlink(ParaEl, Member.Parent);
	if Assigned(LinkEl) then
	  LinkEl['onClick'] := 'opener.location.href = ''' + LinkEl['href'] +
	    '''; return false;';
	AppendText(ParaEl, ')');
      end;
    finally
      List.Free;
    end;
  end;

begin
  case ASubpageIndex of
    0:
      CreateMainPage;
    PropertiesByInheritanceSubindex:
      CreateInheritanceSubpage(@PropertyFilter);
    PropertiesByNameSubindex:
      CreateSortedSubpage(@PropertyFilter);
    MethodsByInheritanceSubindex:
      CreateInheritanceSubpage(@MethodFilter);
    MethodsByNameSubindex:
      CreateSortedSubpage(@MethodFilter);
    EventsByInheritanceSubindex:
      CreateInheritanceSubpage(@EventFilter);
    EventsByNameSubindex:
      CreateSortedSubpage(@EventFilter);
  end;
end;

procedure THTMLWriter.CreateClassMemberPageBody(AElement: TPasElement);
var
  TableEl, TREl, CodeEl: TDOMElement;

  procedure CreateVarPage(Element: TPasVariable);
  begin
    AppendHyperlink(CodeEl, Element.Parent);
    AppendSym(CodeEl, '.');
    AppendText(CodeEl, Element.Name);
    if Assigned(Element.VarType) then
    begin
      AppendSym(CodeEl, ': ');
      AppendSym(AppendType(CodeEl, TableEl, Element.VarType, False), ';');
    end;
  end;

  procedure CreatePropertyPage(Element: TPasProperty);
  var
    NeedBreak: Boolean;
  begin
    AppendKw(CodeEl, 'property ');
    AppendHyperlink(CodeEl, Element.Parent);
    AppendSym(CodeEl, '.');
    AppendText(CodeEl, Element.Name);
    if Assigned(Element.VarType) then
    begin
      AppendSym(CodeEl, ': ');
      AppendType(CodeEl, TableEl, Element.VarType, False);
    end;

    NeedBreak := False;
    if Length(TPasProperty(Element).IndexValue) <> 0 then
    begin
      CreateEl(CodeEl, 'br');
      AppendNbsp(CodeEl, 2);
      AppendKw(CodeEl, 'index ');
      AppendPasSHFragment(CodeEl, TPasProperty(Element).IndexValue, 0);
      NeedBreak := True;
    end;
    if Length(TPasProperty(Element).ReadAccessorName) <> 0 then
    begin
      CreateEl(CodeEl, 'br');
      AppendNbsp(CodeEl, 2);
      AppendKw(CodeEl, 'read ');
      AppendText(CodeEl, TPasProperty(Element).ReadAccessorName);
      NeedBreak := True;
    end;
    if Length(TPasProperty(Element).WriteAccessorName) <> 0 then
    begin
      CreateEl(CodeEl, 'br');
      AppendNbsp(CodeEl, 2);
      AppendKw(CodeEl, 'write ');
      AppendText(CodeEl, TPasProperty(Element).WriteAccessorName);
      NeedBreak := True;
    end;
    if Length(TPasProperty(Element).StoredAccessorName) <> 0 then
    begin
      CreateEl(CodeEl, 'br');
      AppendNbsp(CodeEl, 2);
      AppendKw(CodeEl, 'stored ');
      AppendText(CodeEl, TPasProperty(Element).StoredAccessorName);
      NeedBreak := True;
    end;
    if Length(TPasProperty(Element).DefaultValue) <> 0 then
    begin
      CreateEl(CodeEl, 'br');
      AppendNbsp(CodeEl, 2);
      AppendKw(CodeEl, 'default ');
      AppendPasSHFragment(CodeEl, TPasProperty(Element).DefaultValue, 0);
      NeedBreak := True;
    end;

    AppendSym(CodeEl, ';');

    if TPasProperty(Element).IsDefault or TPasProperty(Element).IsNodefault then
    begin
      if NeedBreak then
      begin
        CreateEl(CodeEl, 'br');
        AppendNbsp(CodeEl, 2);
      end;
      if TPasProperty(Element).IsDefault then
        AppendKw(CodeEl, 'default')
      else
        AppendKw(CodeEl, 'nodefault');
      AppendSym(CodeEl, ';');
    end;
  end;

var
  s: String;
  DocNode: TDocNode;
begin
  AppendMenuBar(-1);
  AppendTitle(AElement.FullName);
  AppendShortDescr(CreatePara(BodyElement), AElement);
  AppendText(CreateH2(BodyElement), SDocDeclaration);

  TableEl := CreateTable(BodyElement);
  TREl := CreateTR(TableEl);
  CodeEl := CreateCode(CreatePara(CreateTD(TREl)));
  AppendText(CodeEl, ' ');	// !!!: Workaround for current HTML writer

  case AElement.Visibility of
    visPrivate: s := 'private';
    visProtected: s := 'protected';
    visPublic: s := 'public';
    visPublished: s := 'published';
    visAutomated: s := 'automated';
    else s := '';
  end;
  if Length(s) > 0 then
    AppendKw(CodeEl, s);
  AppendText(CodeEl, ' ');

  if AElement.ClassType = TPasVariable then
    CreateVarPage(TPasVariable(AElement))
  else if AElement.InheritsFrom(TPasProcedureBase) then
    AppendProcDecl(CodeEl, TableEl, TPasProcedureBase(AElement))
  else if AElement.ClassType = TPasProperty then
    CreatePropertyPage(TPasProperty(AElement))
  else
    AppendText(CreateWarning(BodyElement), '<' + AElement.ClassName + '>');

  FinishElementPage(AElement);
end;

procedure THTMLWriter.CreateVarPageBody(AVar: TPasVariable);
var
  TableEl, TREl, TDEl, CodeEl, El: TDOMElement;
  DocNode: TDocNode;
begin
  AppendMenuBar(-1);
  AppendTitle(AVar.FullName);
  AppendShortDescr(CreatePara(BodyElement), AVar);
  AppendText(CreateH2(BodyElement), SDocDeclaration);

  TableEl := CreateTable(BodyElement);
  TREl := CreateTR(TableEl);
  TDEl := CreateTD(TREl);
  CodeEl := CreateCode(CreatePara(TDEl));

  AppendKw(CodeEl, 'var');
  AppendText(CodeEl, ' ' + AVar.Name);
  if Assigned(AVar.VarType) then
  begin
    AppendSym(CodeEl, ': ');
    El := AppendType(CodeEl, TableEl, AVar.VarType, False);
  end else
    El := CodeEl;
  if Length(AVar.Value) > 0 then
    AppendPasSHFragment(El, ' = ' + AVar.Value + ';', 0)
  else
    AppendSym(El, ';');

  FinishElementPage(AVar);
end;

procedure THTMLWriter.CreateProcPageBody(AProc: TPasProcedureBase);
var
  TableEl, TREl, TDEl, CodeEl: TDOMElement;
begin
  AppendMenuBar(-1);
  AppendTitle(AProc.Name);
  AppendShortDescr(CreatePara(BodyElement), AProc);
  AppendText(CreateH2(BodyElement), SDocDeclaration);

  TableEl := CreateTable(BodyElement);
  TREl := CreateTR(TableEl);
  TDEl := CreateTD(TREl);
  CodeEl := CreateCode(CreatePara(TDEl));

  AppendProcDecl(CodeEl, TableEl, AProc);

  FinishElementPage(AProc);
end;


// private methods

function THTMLWriter.GetPageCount: Integer;
begin
  Result := PageInfos.Count;
end;

end.


{
  $Log: dw_html.pp,v $
  Revision 1.4  2003/04/22 00:00:05  sg
  * Fixed bug in path building for links to elements which don't have their
    own page, but their parent element has

  Revision 1.3  2003/04/17 14:15:24  sg
  * Added writing of array ranges

  Revision 1.2  2003/03/18 19:28:44  michael
  + Some changes to output handling, more suitable for tex output

  Revision 1.1  2003/03/17 23:03:20  michael
  + Initial import in CVS

  Revision 1.15  2003/03/13 22:02:13  sg
  * New version with many bugfixes and our own parser (now independent of the
    compiler source)

  Revision 1.14  2002/11/15 19:46:32  sg
  * Added support for classes and objects (formerly all objects have been
    written als classes)

  Revision 1.13  2002/05/24 00:13:22  sg
  * much improved new version, including many linking and output fixes

  Revision 1.12  2002/03/12 10:58:36  sg
  * reworked linking engine and internal structure

  Revision 1.11  2002/01/20 11:19:55  michael
  + Added link attribute and property to TFPElement

  Revision 1.10  2001/12/21 11:25:13  sg
  * The parser can now unget two tokens from the scanner
  * Added parsing and HTML output of range types
  * Procedure/function variable bugfixes

  Revision 1.9  2001/12/17 22:34:04  sg
  * Fixed typo in output for menu bar

  Revision 1.8  2001/12/17 13:41:17  jonas
    * OsPathSeparator -> PathDelim
}
