{
    $Id: sax_html.pp,v 1.5 2003/03/16 22:38:09 sg Exp $
    This file is part of the Free Component Library

    HTML parser with SAX-like interface
    Copyright (c) 2000-2002 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{
  Known problems:
  * The whitespace handling does only work for processing the DOM tree.
    Storing the DOM tree to a XML file will result in a quite ugly file.
    (This probably has got much better with recent versions, which do
    decent whitespace converting, but it's not tested really good.)
  * Entity references in attribute values don't get parsed.
}


unit SAX_HTML;

interface

uses SysUtils, Classes, SAX, DOM, DOM_HTML;

type

{ THTMLReader: The HTML reader class }

  THTMLScannerContext = (
    scUnknown,
    scWhitespace,	// within whitespace
    scText,		// within text
    scEntityReference,	// within entity reference ("&...;")
    scTag);		// within a start tag or end tag

  THTMLReader = class(TSAXReader)
  private
    FStarted: Boolean;
    FEndOfStream: Boolean;
    FScannerContext: THTMLScannerContext;
    FTokenText: SAXString;
    FCurStringValueDelimiter: Char;
    FAttrNameRead: Boolean;
  protected
    procedure EnterNewScannerContext(NewContext: THTMLScannerContext);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Parse(AInput: TSAXInputSource); override; overload;

    property EndOfStream: Boolean read FEndOfStream;
    property ScannerContext: THTMLScannerContext read FScannerContext;
    property TokenText: SAXString read FTokenText;
  end;


{ THTMLToDOMConverter }

  THTMLNodeType = (ntWhitespace, ntText, ntEntityReference, ntTag);

  THTMLNodeInfo = class
    NodeType: THTMLNodeType;
    DOMNode: TDOMNode;
  end;

  THTMLToDOMConverter = class
  private
    FReader: THTMLReader;
    FDocument: TDOMDocument;
    FElementStack: TList;
    FNodeBuffer: TList;
    IsFragmentMode, FragmentRootSet: Boolean;
    FragmentRoot: TDOMNode;

    procedure ReaderCharacters(Sender: TObject; const ch: PSAXChar;
      Start, Count: Integer);
    procedure ReaderIgnorableWhitespace(Sender: TObject; const ch: PSAXChar;
      Start, Count: Integer);
    procedure ReaderSkippedEntity(Sender: TObject; const Name: SAXString);
    procedure ReaderStartElement(Sender: TObject;
      const NamespaceURI, LocalName, RawName: SAXString; Attr: TSAXAttributes);
    procedure ReaderEndElement(Sender: TObject;
      const NamespaceURI, LocalName, RawName: SAXString);

  public
    constructor Create(AReader: THTMLReader; ADocument: TDOMDocument);
    constructor CreateFragment(AReader: THTMLReader; AFragmentRoot: TDOMNode);
    destructor Destroy; override;
  end;


// Helper functions; these ones are HTML equivalents of ReadXML[File|Fragment]

procedure ReadHTMLFile(var ADoc: THTMLDocument; const AFilename: String);
procedure ReadHTMLFile(var ADoc: THTMLDocument; var f: TStream);

procedure ReadHTMLFragment(AParentNode: TDOMNode; const AFilename: String);
procedure ReadHTMLFragment(AParentNode: TDOMNode; var f: TStream);



implementation

uses HTMLDefs;

const
  WhitespaceChars = [#9, #10, #13, ' '];


constructor THTMLReader.Create;
begin
  inherited Create;
  FScannerContext := scUnknown;
end;

destructor THTMLReader.Destroy;
begin
  if FStarted then
    DoEndDocument;
  inherited Destroy;
end;

procedure THTMLReader.Parse(AInput: TSAXInputSource);
const
  MaxBufferSize = 1024;
var
  Buffer: array[0..MaxBufferSize - 1] of Char;
  BufferSize, BufferPos: Integer;
begin
  if not FStarted then
  begin
    FStarted := True;
    DoStartDocument;
  end;

  FEndOfStream := False;
  while True do
  begin
    // Read data into the input buffer
    BufferSize := AInput.Stream.Read(Buffer, MaxBufferSize);
    if BufferSize = 0 then
    begin
      FEndOfStream := True;
      break;
    end;

    BufferPos := 0;
    while BufferPos < BufferSize do
      case ScannerContext of
        scUnknown:
	  case Buffer[BufferPos] of
	    #9, #10, #13, ' ':
	      EnterNewScannerContext(scWhitespace);
	    '&':
	      begin
	        Inc(BufferPos);
	        EnterNewScannerContext(scEntityReference);
	      end;
	    '<':
	      begin
	        Inc(BufferPos);
	        EnterNewScannerContext(scTag);
	      end;
	    else
	      EnterNewScannerContext(scText);
	  end;
	scWhitespace:
	  case Buffer[BufferPos] of
	    #9, #10, #13, ' ':
	      begin
		FTokenText := FTokenText + Buffer[BufferPos];
	        Inc(BufferPos);
	      end;
	    '&':
	      begin
	        Inc(BufferPos);
	        EnterNewScannerContext(scEntityReference);
	      end;
	    '<':
	      begin
	        Inc(BufferPos);
		EnterNewScannerContext(scTag);
	      end;
	    else
	      EnterNewScannerContext(scText);
	  end;
        scText:
	  case Buffer[BufferPos] of
	    #9, #10, #13, ' ':
	      EnterNewScannerContext(scWhitespace);
	    '&':
	      begin
	        Inc(BufferPos);
	        EnterNewScannerContext(scEntityReference);
	      end;
	    '<':
	      begin
	        Inc(BufferPos);
		EnterNewScannerContext(scTag);
	      end;
	    else
	    begin
	      FTokenText := FTokenText + Buffer[BufferPos];
	      Inc(BufferPos);
	    end;
	  end;
	scEntityReference:
	  if Buffer[BufferPos] = ';' then
	  begin
	    Inc(BufferPos);
	    EnterNewScannerContext(scUnknown);
	  end else if not (Buffer[BufferPos] in
	    ['a'..'z', 'A'..'Z', '0'..'9', '#']) then
	    EnterNewScannerContext(scUnknown)
	  else
	  begin
	    FTokenText := FTokenText + Buffer[BufferPos];
	    Inc(BufferPos);
	  end;
	scTag:
	  case Buffer[BufferPos] of
	    '''', '"':
	      begin
	        if FAttrNameRead then
		begin
	          if FCurStringValueDelimiter = #0 then
		    FCurStringValueDelimiter := Buffer[BufferPos]
		  else if FCurStringValueDelimiter = Buffer[BufferPos] then
		  begin
		    FCurStringValueDelimiter := #0;
		    FAttrNameRead := False;
		  end;
		end;
		FTokenText := FTokenText + Buffer[BufferPos];
		Inc(BufferPos);
	      end;
	    '=':
	      begin
	        FAttrNameRead := True;
		FTokenText := FTokenText + Buffer[BufferPos];
		Inc(BufferPos);
	      end;
	    '>':
	      begin
	        Inc(BufferPos);
		if FCurStringValueDelimiter = #0 then
		  EnterNewScannerContext(scUnknown);
	      end;
	    else
	    begin
	      FTokenText := FTokenText + Buffer[BufferPos];
	      Inc(BufferPos);
	    end;
	  end;
      end;
  end;
end;

procedure THTMLReader.EnterNewScannerContext(NewContext: THTMLScannerContext);

  function SplitTagString(const s: String; var Attr: TSAXAttributes): String;
  var
    i, j: Integer;
    AttrName: String;
    ValueDelimiter: Char;
    DoIncJ: Boolean;
  begin
    Attr := nil;
    i := Pos(' ', s);
    if i <= 0 then
      Result := LowerCase(s)
    else
    begin
      Result := LowerCase(Copy(s, 1, i - 1));
      Attr := TSAXAttributes.Create;

      Inc(i);

      while (i <= Length(s)) and (s[i] in WhitespaceChars) do
        Inc(i);

      SetLength(AttrName, 0);
      j := i;

      while j <= Length(s) do
        if s[j] = '=' then
	begin
	  AttrName := LowerCase(Copy(s, i, j - i));
	  Inc(j);
	  if (j < Length(s)) and ((s[j] = '''') or (s[j] = '"')) then
	  begin
  	    ValueDelimiter := s[j];
	    Inc(j);
	  end else
	    ValueDelimiter := #0;
	  i := j;
	  DoIncJ := False;
	  while j <= Length(s) do
	    if ValueDelimiter = #0 then
	      if s[j] in WhitespaceChars then
	        break
	      else
	        Inc(j)
	    else if s[j] = ValueDelimiter then
	    begin
	      DoIncJ := True;
	      break
	    end else
	      Inc(j);

          Attr.AddAttribute('', AttrName, '', '', Copy(s, i, j - i));

	  if DoIncJ then
	    Inc(j);

          while (j <= Length(s)) and (s[j] in WhitespaceChars) do
	    Inc(j);
	  i := j;
	end
	else if s[j] in WhitespaceChars then
	begin
	  Attr.AddAttribute('', Copy(s, i, j - i), '', '', '');
	  Inc(j);
          while (j <= Length(s)) and (s[j] in WhitespaceChars) do
	    Inc(j);
	  i := j;
        end else
	  Inc(j);
    end;
  end;

var
  Attr: TSAXAttributes;
  EntString, TagName: String;
  Found: Boolean;
  Ent: Char;
  i: Integer;
begin
  case ScannerContext of
    scWhitespace:
      DoIgnorableWhitespace(PSAXChar(TokenText), 1, Length(TokenText));
    scText:
      DoCharacters(PSAXChar(TokenText), 0, Length(TokenText));
    scEntityReference:
      begin
        if ResolveHTMLEntityReference(TokenText, Ent) then
	begin
	  EntString := Ent;
	  DoCharacters(PSAXChar(EntString), 0, 1);
	end else
	begin
	  { Is this a predefined Unicode character entity? We must check this,
	    as undefined entities must be handled as text, for compatiblity
	    to popular browsers... }
	  Found := False;
	  for i := Low(UnicodeHTMLEntities) to High(UnicodeHTMLEntities) do
	    if UnicodeHTMLEntities[i] = TokenText then
	    begin
	      Found := True;
	      break;
	    end;
	  if Found then
	    DoSkippedEntity(TokenText)
	  else
            DoCharacters(PSAXChar('&' + TokenText), 0, Length(TokenText) + 1);
	end;
      end;
    scTag:
      if Length(TokenText) > 0 then
      begin
        Attr := nil;
        if TokenText[1] = '/' then
	begin
	  DoEndElement('',
	    SplitTagString(Copy(TokenText, 2, Length(TokenText)), Attr), '');
	end else if TokenText[1] <> '!' then
	begin
	  // Do NOT combine to a single line, as Attr is an output value!
	  TagName := SplitTagString(TokenText, Attr);
	  DoStartElement('', TagName, '', Attr);
	end;
	if Assigned(Attr) then
  	  Attr.Free;
      end;
  end;
  FScannerContext := NewContext;
  SetLength(FTokenText, 0);
  FCurStringValueDelimiter := #0;
  FAttrNameRead := False;
end;


{ THTMLToDOMConverter }

constructor THTMLToDOMConverter.Create(AReader: THTMLReader;
  ADocument: TDOMDocument);
begin
  inherited Create;
  FReader := AReader;
  FReader.OnCharacters := @ReaderCharacters;
  FReader.OnIgnorableWhitespace := @ReaderIgnorableWhitespace;
  FReader.OnSkippedEntity := @ReaderSkippedEntity;
  FReader.OnStartElement := @ReaderStartElement;
  FReader.OnEndElement := @ReaderEndElement;
  FDocument := ADocument;
  FElementStack := TList.Create;
  FNodeBuffer := TList.Create;
end;

constructor THTMLToDOMConverter.CreateFragment(AReader: THTMLReader;
  AFragmentRoot: TDOMNode);
begin
  inherited Create;
  FReader := AReader;
  FReader.OnCharacters := @ReaderCharacters;
  FReader.OnIgnorableWhitespace := @ReaderIgnorableWhitespace;
  FReader.OnSkippedEntity := @ReaderSkippedEntity;
  FReader.OnStartElement := @ReaderStartElement;
  FReader.OnEndElement := @ReaderEndElement;
  FDocument := AFragmentRoot.OwnerDocument;
  FElementStack := TList.Create;
  FNodeBuffer := TList.Create;
  FragmentRoot := AFragmentRoot;
  IsFragmentMode := True;
end;

destructor THTMLToDOMConverter.Destroy;
var
  i: Integer;
begin
  // Theoretically, always exactly one item will remain - the root element:
  for i := 0 to FNodeBuffer.Count - 1 do
    THTMLNodeInfo(FNodeBuffer[i]).Free;
  FNodeBuffer.Free;

  FElementStack.Free;
  inherited Destroy;
end;

procedure THTMLToDOMConverter.ReaderCharacters(Sender: TObject;
  const ch: PSAXChar; Start, Count: Integer);
var
  s: SAXString;
  NodeInfo: THTMLNodeInfo;
begin
  SetLength(s, Count);
  Move(ch^, s[1], Count * SizeOf(SAXChar));

  NodeInfo := THTMLNodeInfo.Create;
  NodeInfo.NodeType := ntText;
  NodeInfo.DOMNode := FDocument.CreateTextNode(s);
  FNodeBuffer.Add(NodeInfo);
end;

procedure THTMLToDOMConverter.ReaderIgnorableWhitespace(Sender: TObject;
  const ch: PSAXChar; Start, Count: Integer);
var
  s: SAXString;
  NodeInfo: THTMLNodeInfo;
begin
  SetLength(s, Count);
  Move(ch^, s[1], Count * SizeOf(SAXChar));

  NodeInfo := THTMLNodeInfo.Create;
  NodeInfo.NodeType := ntWhitespace;
  NodeInfo.DOMNode := FDocument.CreateTextNode(s);
  FNodeBuffer.Add(NodeInfo);
end;

procedure THTMLToDOMConverter.ReaderSkippedEntity(Sender: TObject;
  const Name: SAXString);
var
  NodeInfo: THTMLNodeInfo;
begin
  NodeInfo := THTMLNodeInfo.Create;
  NodeInfo.NodeType := ntEntityReference;
  NodeInfo.DOMNode := FDocument.CreateEntityReference(Name);
  FNodeBuffer.Add(NodeInfo);
end;

procedure THTMLToDOMConverter.ReaderStartElement(Sender: TObject;
  const NamespaceURI, LocalName, RawName: SAXString; Attr: TSAXAttributes);
var
  NodeInfo: THTMLNodeInfo;
  Element: TDOMElement;
  i: Integer;
begin
  // WriteLn('Start: ', LocalName, '. Node buffer before: ', FNodeBuffer.Count, ' elements');
  Element := FDocument.CreateElement(LocalName);
  if Assigned(Attr) then
  begin
    // WriteLn('Attribute: ', Attr.GetLength);
    for i := 0 to Attr.GetLength - 1 do
    begin
      // WriteLn('#', i, ': LocalName = ', Attr.GetLocalName(i), ', Value = ', Attr.GetValue(i));
      Element[Attr.GetLocalName(i)] := Attr.GetValue(i);
    end;
  end;

  NodeInfo := THTMLNodeInfo.Create;
  NodeInfo.NodeType := ntTag;
  NodeInfo.DOMNode := Element;
  if IsFragmentMode then
  begin
    if not FragmentRootSet then
    begin
      FragmentRoot.AppendChild(Element);
      FragmentRootSet := True;
    end;
  end else
    if not Assigned(FDocument.DocumentElement) then
      FDocument.AppendChild(Element);
  FNodeBuffer.Add(NodeInfo);
  // WriteLn('Start: ', LocalName, '. Node buffer after: ', FNodeBuffer.Count, ' elements');
end;

procedure THTMLToDOMConverter.ReaderEndElement(Sender: TObject;
  const NamespaceURI, LocalName, RawName: SAXString);
var
  NodeInfo, NodeInfo2: THTMLNodeInfo;
  i, j: Integer;
  TagInfo: PHTMLElementProps;
begin
  // WriteLn('End: ', LocalName, '. Node buffer: ', FNodeBuffer.Count, ' elements');
  // Find the matching start tag
  i := FNodeBuffer.Count - 1;
  while i >= 0 do
  begin
    NodeInfo := THTMLNodeInfo(FNodeBuffer.Items[i]);
    if (NodeInfo.NodeType = ntTag) and
      (CompareText(NodeInfo.DOMNode.NodeName, LocalName) = 0) then
    begin
      // We found the matching start tag

      TagInfo := nil;
      for j := Low(HTMLElProps) to High(HTMLElProps) do
        if CompareText(HTMLElProps[j].Name, LocalName) = 0 then
	begin
	  TagInfo := @HTMLElProps[j];
	  break;
	end;

      Inc(i);
      while i < FNodeBuffer.Count do
      begin
        NodeInfo2 := THTMLNodeInfo(FNodeBuffer.Items[i]);

	if (NodeInfo2.NodeType = ntWhitespace) and Assigned(TagInfo) and
	  (not (efPreserveWhitespace in TagInfo^.Flags)) then
	  // Handle whitespace, which doesn't need to get preserved...
	  if not (efPCDATAContent in TagInfo^.Flags) then
	    // No character data allowed within the current element
	    NodeInfo2.DOMNode.Free
	  else
	  begin
	    // Character data allowed, so normalize it
	    NodeInfo2.DOMNode.NodeValue := ' ';
            NodeInfo.DOMNode.AppendChild(NodeInfo2.DOMNode)
	  end
	else
	  NodeInfo.DOMNode.AppendChild(NodeInfo2.DOMNode);

	NodeInfo2.Free;
	FNodeBuffer.Delete(i);
      end;
      break;
    end;
    Dec(i);
  end;
end;


procedure ReadHTMLFile(var ADoc: THTMLDocument; const AFilename: String);
var
  f: TStream;
begin
  ADoc := nil;
  f := TFileStream.Create(AFilename, fmOpenRead);
  try
    ReadHTMLFile(ADoc, f);
  finally
    f.Free;
  end;
end;

procedure ReadHTMLFile(var ADoc: THTMLDocument; var f: TStream);
var
  Reader: THTMLReader;
  Converter: THTMLToDOMConverter;
begin
  ADoc := THTMLDocument.Create;
  Reader := THTMLReader.Create;
  try
    Converter := THTMLToDOMConverter.Create(Reader, ADoc);
    try
      Reader.ParseStream(f);
    finally
      Converter.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure ReadHTMLFragment(AParentNode: TDOMNode; const AFilename: String);
var
  f: TStream;
begin
  f := TFileStream.Create(AFilename, fmOpenRead);
  try
    ReadHTMLFragment(AParentNode, f);
  finally
    f.Free;
  end;
end;

procedure ReadHTMLFragment(AParentNode: TDOMNode; var f: TStream);
var
  Reader: THTMLReader;
  Converter: THTMLToDOMConverter;
begin
  Reader := THTMLReader.Create;
  try
    Converter := THTMLToDOMConverter.CreateFragment(Reader, AParentNode);
    try
      Reader.ParseStream(f);
    finally
      Converter.Free;
    end;
  finally
    Reader.Free;
  end;
end;


end.


{
  $Log: sax_html.pp,v $
  Revision 1.5  2003/03/16 22:38:09  sg
  * Added fragment parsing functions

  Revision 1.4  2002/12/14 19:18:21  sg
  * Improved whitespace handling (although it's still not perfect in all
    cases)

  Revision 1.3  2002/12/12 20:17:32  sg
  * More WideString fixes

  Revision 1.2  2002/12/12 13:43:38  michael
  + Patches from peter to fix 1.1 compile

  Revision 1.1  2002/12/11 21:06:07  sg
  * Small cleanups
  * Replaced htmldoc unit with dom_html unit
  * Added SAX parser framework and SAX HTML parser

}
