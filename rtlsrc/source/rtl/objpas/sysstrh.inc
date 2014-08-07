{
    *********************************************************************
    $Id: sysstrh.inc,v 1.1.2.7 2003/06/26 12:22:33 michael Exp $
    Copyright (C) 1997, 1998 Gertjan Schouten

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
    *********************************************************************

    System Utilities For Free Pascal
}

{==============================================================================}
{   standard Functions                                                         }
{==============================================================================}

type
   PString = ^String;

   { For FloatToText }
   TFloatFormat = (ffGeneral, ffExponent, ffFixed, ffNumber, ffCurrency);
   TReplaceFlags = set of (rfReplaceAll, rfIgnoreCase);
   
   TFloatRec = Record
      Exponent: Integer;
      Negative: Boolean;
      Digits: Array[0..18] Of Char;
   End;
   
const
  { For floattodatetime }
  MinDateTime: TDateTime = -657434.0;      { 01/01/0100 12:00:00.000 AM }
  MaxDateTime: TDateTime =  2958465.99999; { 12/31/9999 11:59:59.999 PM }

Type
  TTextLineBreakStyle = (tlbsLF, tlbsCRLF);   // Must move to system unit, and add Mac tlbsCR too ?

Const
  DefaultTextLineBreakStyle: TTextLineBreakStyle = {$ifdef unix} tlbsLF {$else} tlbsCRLF {$endif};

               
Const
  LeadBytes: set of Char = [];
  EmptyStr : string = '';
  NullStr : PString = @EmptyStr;
      
  EmptyWideStr : WideString = '';
  NullWideStr : PWideString = @EmptyWideStr;

Function NewStr(const S: string): PString;
procedure DisposeStr(S: PString);
procedure AssignStr(var P: PString; const S: string);
procedure AppendStr(var Dest: String; const S: string);
Function UpperCase(const s: string): string;
Function LowerCase(const s: string): string;
Function CompareStr(const S1, S2: string): Integer;
Function CompareMemRange(P1, P2: Pointer; Length: cardinal): integer;
Function CompareMem(P1, P2: Pointer; Length: cardinal): Boolean;
Function CompareText(const S1, S2: string): integer;

Function AnsiUpperCase(const s: string): string;
Function AnsiLowerCase(const s: string): string;
Function AnsiCompareStr(const S1, S2: string): integer;
Function AnsiCompareText(const S1, S2: string): integer;
Function AnsiStrComp(S1, S2: PChar): integer;
Function AnsiStrIComp(S1, S2: PChar): integer;
Function AnsiStrLComp(S1, S2: PChar; MaxLen: cardinal): integer;
Function AnsiStrLIComp(S1, S2: PChar; MaxLen: cardinal): integer;
Function AnsiStrLower(Str: PChar): PChar;
Function AnsiStrUpper(Str: PChar): PChar;
Function AnsiLastChar(const S: string): PChar;
Function AnsiStrLastChar(Str: PChar): PChar;

Function Trim(const S: string): string;
Function TrimLeft(const S: string): string;
Function TrimRight(const S: string): string;
Function QuotedStr(const S: string): string;
Function AnsiQuotedStr(const S: string; Quote: char): string;
Function AnsiExtractQuotedStr(Const Src: PChar; Quote: Char): string;
Function AdjustLineBreaks(const S: string): string;
Function AdjustLineBreaks(const S: string;Style: TTextLineBreakStyle): string;
Function IsValidIdent(const Ident: string): boolean;
Function IntToStr(Value: integer): string;
Function IntToStr(Value: Int64): string;
Function IntToHex(Value: integer; Digits: integer): string;
Function IntToHex(Value: Int64; Digits: integer): string;
Function StrToInt(const s: string): integer;
Function StrToInt64(const s: string): int64;
Function StrToIntDef(const S: string; Default: integer): integer;
Function StrToInt64Def(const S: string; Default: int64): int64;
Function LoadStr(Ident: integer): string;
// Function FmtLoadStr(Ident: integer; const Args: array of const): string;
Function Format (Const Fmt : String; const Args : Array of const) : String;
Function FormatBuf (Var Buffer; BufLen : Cardinal; Const Fmt; fmtLen : Cardinal; Const Args : Array of const) : Cardinal;
Function StrFmt(Buffer,Fmt : PChar; Const args: Array of const) : Pchar;
Function StrLFmt(Buffer : PCHar; Maxlen : Cardinal;Fmt : PChar; Const args: Array of const) : Pchar;
Procedure FmtStr(Var Res: String; Const Fmt : String; Const args: Array of const);
Function FloatToStrF(Value: Extended; format: TFloatFormat; Precision, Digits: Integer): String;
Function FloatToStr(Value: Extended): String;
Function StrToFloat(Const S : String) : Extended;
Function TextToFloat(Buffer: PChar; Var Value: Extended): Boolean;
Function FloatToText(Buffer: PChar; Value: Extended; format: TFloatFormat; Precision, Digits: Integer): Longint;
Function FloatToTextFmt(Buffer: PChar; Value: Extended; format: PChar): Integer;
Procedure FloatToDecimal(Var Result: TFloatRec; Value: Extended; Precision, Decimals : integer);
Function FormatFloat(Const Format : String; Value : Extended) : String;
Function LastDelimiter(const Delimiters, S: string): Integer;
Function StringReplace(const S, OldPattern, NewPattern: string;  Flags: TReplaceFlags): string;
Function IsDelimiter(const Delimiters, S: string; Index: Integer): Boolean;
Function FloatToDateTime (Const Value : Extended) : TDateTime;
Function FloatToCurr (Const Value : Extended) : Currency;
Function CurrToStr(Value: Currency): string;
Function StrToCurr(const S: string): Currency;
Function StrToBool(const S: string): Boolean;
Function BoolToStr(B: Boolean): string;

// MBCS Functions. No MBCS yet, so mostly these are calls to the regular counterparts.
Type
  TMbcsByteType = (mbSingleByte, mbLeadByte, mbTrailByte);
  
Function AnsiCompareFileName(const S1, S2: string): Integer;
Function SameFileName(const S1, S2: string): Boolean;
Function AnsiLowerCaseFileName(const S: string): string;
Function AnsiUpperCaseFileName(const S: string): string;
Function AnsiPos(const Substr, S: string): Integer;
Function AnsiStrPos(Str, SubStr: PChar): PChar;
Function AnsiStrRScan(Str: PChar; Chr: Char): PChar;
Function AnsiStrScan(Str: PChar; Chr: Char): PChar;
Function ByteType(const S: string; Index: Integer): TMbcsByteType;
Function StrByteType(Str: PChar; Index: Cardinal): TMbcsByteType;
Function ByteToCharLen(const S: string; MaxLen: Integer): Integer;
Function CharToByteLen(const S: string; MaxLen: Integer): Integer;
Function ByteToCharIndex(const S: string; Index: Integer): Integer;

const
{$ifndef unix}
  SwitchChars = ['/','-']; 
{$else}
  SwitchChars = ['-'];
{$endif}

Function FindCmdLineSwitch(const Switch: string; const Chars: TSysCharSet;IgnoreCase: Boolean): Boolean;
Function FindCmdLineSwitch(const Switch: string; IgnoreCase: Boolean): Boolean;
Function FindCmdLineSwitch(const Switch: string): Boolean;
                          
                          

{==============================================================================}
{   extra Functions                                                            }
{==============================================================================}

Function LeftStr(const S: string; Count: integer): string;
Function RightStr(const S: string; Count: integer): string;
Function BCDToInt(Value: integer): integer;

{
  $Log: sysstrh.inc,v $
  Revision 1.1.2.7  2003/06/26 12:22:33  michael
  + Fixed AdjustLineBreaks to be Kylix/Delphi compatible

  Revision 1.1.2.6  2002/11/28 22:25:01  michael
  + Fixed float<>string conversion routines

  Revision 1.1.2.5  2002/11/28 20:24:12  michael
  + merged some fixes from mainbranch

  Revision 1.1.2.4  2002/10/29 23:41:06  michael
  + Added lots of D4 functions

  Revision 1.1.2.3  2001/09/20 14:35:34  michael
   Implemented missing StringReplace Function

  Revision 1.1.2.2  2000/12/07 21:48:58  michael
  + Added LastDelimiter Function

  Revision 1.1.2.1  2000/08/09 19:31:03  peter
    * int64 updates from Marco

  Revision 1.1  2000/07/13 06:31:01  michael
  + Initial import

  Revision 1.10  2000/05/08 17:03:02  sg
  * Changed CompareMem to CompareMemRange and added new (Delphi compatible)
    CompareMem. (CompareMem needs a Boolean as result type, not Integer)

  Revision 1.9  2000/02/09 16:59:33  peter
    * truncated log

}

