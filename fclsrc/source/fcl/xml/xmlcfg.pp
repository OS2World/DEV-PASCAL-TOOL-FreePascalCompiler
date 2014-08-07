{
    $Id: xmlcfg.pp,v 1.5 2002/09/07 15:15:29 peter Exp $
    This file is part of the Free Component Library

    Implementation of TXMLConfig class
    Copyright (c) 1999 - 2001 by Sebastian Guenther, sg@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  TXMLConfig enables applications to use XML files for storing their
  configuration data
}

{$MODE objfpc}
{$H+}

unit XMLCfg;

interface
uses Classes, DOM, XMLRead, XMLWrite;

type

  {"APath" is the path and name of a value: A XML configuration file is
   hierarchical. "/" is the path delimiter, the part after the last "/"
   is the name of the value. The path components will be mapped to XML
   elements, the name will be an element attribute.}

  TXMLConfig = class(TComponent)
  private
    FFilename: String;
    procedure SetFilename(const AFilename: String);
  protected
    doc: TXMLDocument;
    FModified: Boolean;
    procedure Loaded; override;
  public
    constructor Create(const AFilename: String);
    destructor Destroy; override;
    procedure Flush;    // Writes the XML file
    function  GetValue(const APath, ADefault: String): String;
    function  GetValue(const APath: String; ADefault: Integer): Integer;
    function  GetValue(const APath: String; ADefault: Boolean): Boolean;
    procedure SetValue(const APath, AValue: String);
    procedure SetValue(const APath: String; AValue: Integer);
    procedure SetValue(const APath: String; AValue: Boolean);
    property Modified: Boolean read FModified;
  published
    property Filename: String read FFilename write SetFilename;
  end;


// ===================================================================

implementation

uses SysUtils;


constructor TXMLConfig.Create(const AFilename: String);
begin
  inherited Create(nil);
  SetFilename(AFilename);
end;

destructor TXMLConfig.Destroy;
begin
  if Assigned(doc) then
  begin
    Flush;
    doc.Free;
  end;
  inherited Destroy;
end;

procedure TXMLConfig.Flush;
var
  f: Text;
begin
  if Modified then
  begin
    AssignFile(f, Filename);
    Rewrite(f);
    try
      WriteXMLFile(doc, f);
    finally
      CloseFile(f);
    end;
    FModified := False;
  end;
end;

function TXMLConfig.GetValue(const APath, ADefault: String): String;
var
  Node, Child, Attr: TDOMNode;
  i: Integer;
  NodePath: String;
begin
  Node := doc.DocumentElement;
  NodePath := APath;
  while True do
  begin
    i := Pos('/', NodePath);
    if i = 0 then
      break;
    Child := Node.FindNode(Copy(NodePath, 1, i - 1));
    NodePath := Copy(NodePath, i + 1, Length(NodePath));
    if not Assigned(Child) then
    begin
      Result := ADefault;
      exit;
    end;
    Node := Child;
  end;
  Attr := Node.Attributes.GetNamedItem(NodePath);
  if Assigned(Attr) then
    Result := Attr.NodeValue
  else
    Result := ADefault;
end;

function TXMLConfig.GetValue(const APath: String; ADefault: Integer): Integer;
begin
  Result := StrToInt(GetValue(APath, IntToStr(ADefault)));
end;

function TXMLConfig.GetValue(const APath: String; ADefault: Boolean): Boolean;
var
  s: String;
begin
  if ADefault then
    s := 'True'
  else
    s := 'False';

  s := GetValue(APath, s);

  if UpperCase(s) = 'TRUE' then
    Result := True
  else if UpperCase(s) = 'FALSE' then
    Result := False
  else
    Result := ADefault;
end;

procedure TXMLConfig.SetValue(const APath, AValue: String);
var
  Node, Child, Attr: TDOMNode;
  i: Integer;
  NodeName, NodePath: String;
begin
  Node := Doc.DocumentElement;
  NodePath := APath;
  while True do
  begin
    i := Pos('/', NodePath);
    if i = 0 then
      break;
    NodeName := Copy(NodePath, 1, i - 1);
    NodePath := Copy(NodePath, i + 1, Length(NodePath));
    Child := Node.FindNode(NodeName);
    if not Assigned(Child) then
    begin
      Child := Doc.CreateElement(NodeName);
      Node.AppendChild(Child);
    end;
    Node := Child;
  end;

  if (not Assigned(TDOMElement(Node).GetAttributeNode(NodePath))) or
    (TDOMElement(Node)[NodePath] <> AValue) then
  begin
    TDOMElement(Node)[NodePath] := AValue;
    FModified := True;
  end;
end;

procedure TXMLConfig.SetValue(const APath: String; AValue: Integer);
begin
  SetValue(APath, IntToStr(AValue));
end;

procedure TXMLConfig.SetValue(const APath: String; AValue: Boolean);
begin
  if AValue then
    SetValue(APath, 'True')
  else
    SetValue(APath, 'False');
end;

procedure TXMLConfig.Loaded;
begin
  inherited Loaded;
  if Length(Filename) > 0 then
    SetFilename(Filename);              // Load the XML config file
end;

procedure TXMLConfig.SetFilename(const AFilename: String);
var
  f: File;
  cfg: TDOMElement;
begin
  FFilename := AFilename;

  if csLoading in ComponentState then
    exit;

  if Assigned(doc) then
  begin
    Flush;
    doc.Free;
  end;

  AssignFile(f, AFileName);
  {$I-}
  Reset(f, 1);
  {$I+}
  if IOResult = 0 then
    try
      ReadXMLFile(doc, f);
    finally
      CloseFile(f);
    end;

  if not Assigned(doc) then
    doc := TXMLDocument.Create;

  cfg :=TDOMElement(doc.FindNode('CONFIG'));
  if not Assigned(cfg) then begin
    cfg := doc.CreateElement('CONFIG');
    doc.AppendChild(cfg);
  end;
end;


end.
{
  $Log: xmlcfg.pp,v $
  Revision 1.5  2002/09/07 15:15:29  peter
    * old logs removed and tabs fixed

}
