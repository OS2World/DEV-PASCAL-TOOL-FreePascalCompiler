{$MODE OBJFPC}
{$H+}
Unit resolve;

{$ifndef win32}
// Here till BSD supports the netbsd unit.
{$ifdef linux}
// Undefine this to use the C library resolve routines. 
// Don't use under win32, netdb does not work on Win32 (yet) !!
{$define usenetdb}
{$endif linux}
{$endif}
{ --------------------------------------------------------------------
  Unit for internet domain calls.
  Copyright (C) 2003  Michael Van Canneyt

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 1, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

  ------------------------------------------------------------------- }
  
interface

uses 
  Classes,UriParser;

Type
  THostAddr = array[1..4] of byte;
  PHostAddr = ^THostAddr;
  TNetAddr = THostAddr;
  PNetAddr = ^TNetAddr;

Const
  NoAddress : THostAddr = (0,0,0,0);
  NoNet : TNetAddr = (0,0,0,0);

{ ---------------------------------------------------------------------
  Axuliary routines
  ---------------------------------------------------------------------}

function HostAddrToStr (Entry : THostAddr) : String;
function StrToHostAddr (IP : String) : THostAddr;
function NetAddrToStr (Entry : TNetAddr) : String;
function StrToNetAddr (IP : String) : TNetAddr;
Function HostToNet (Host : ThostAddr) : ThostAddr;
Function HostToNet (Host : Longint) : Longint;
Function NetToHost (Net : Longint) : Longint;
Function NetToHost (Net : TNetAddr) : TNetAddr;
Function ShortHostToNet (Host : Word) : Word;
Function ShortNetToHost (Net : Word) : Word;

Type

{ ---------------------------------------------------------------------
    TResolver
  ---------------------------------------------------------------------}
  
  TResolver = Class (TComponent)
  Private
    FName : String;
    FAliases : TStringList;
    FRaiseOnError : Boolean;
    FLastError: Integer;
    Function GetAlias(Index : Integer) : STring;
    Function GetAliasCount : Integer;
    Function GetAliasSorted : Boolean;
    Procedure SetAliasSorted (Value : Boolean);
  Protected
    Procedure CheckOperation(Msg : String);
    Function NameLookup(Const S : String) : Boolean; virtual;
    Procedure SaveAliases(P : PPChar);
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure ClearData; virtual;
    Property ResolvedName : String Read FName;
    Property Aliases [Index : integer ] : string Read GetAlias;
    Property AliasCount : Integer read GetAliasCount;
    Property SortAliases : Boolean Read GetAliasSorted Write SetAliasSorted;
    Property RaiseOnError : Boolean Read FRaiseOnError Write FRAiseOnError;
    Property LastError : Integer Read FlastError;
  end;

{ ---------------------------------------------------------------------
    THostResolver
  ---------------------------------------------------------------------}

  THostResolver = Class(TResolver)
  Private
    FHostAddress : THostAddr;
    FAddressCount : Integer;
    FAddresses : PHostAddr;
    Function GetAddress (Index : Integer) : THostAddr;
    Function GetNetAddress (Index : Integer) : THostAddr;
    Function GetNetHostAddress : THostAddr;
    Function GetAsString : String;
    Procedure SaveHostEntry (Entry : Pointer);
  Public
    Procedure ClearData; Override;
    Function NameLookup(Const S : String) : Boolean; override;
    Function AddressLookup(Const S : String) : Boolean; virtual;
    Function AddressLookup(Const Address : THostAddr) : Boolean; virtual;
    Property HostAddress : THostAddr Read FHostAddress;
    Property NetHostAddress : THostAddr Read GetNetHostAddress;
    Property AddressAsString : String Read GetAsString;
    Property AddressCount : Integer Read FAddressCount ;
    Property Addresses [Index : Integer] : ThostAddr Read GetAddress;
    Property NetAddresses [Index : Integer] : ThostAddr Read GetNetAddress;
  end;

{ ---------------------------------------------------------------------
    TNetResolver
  ---------------------------------------------------------------------}
  
  TNetResolver = Class(TResolver)
  Private
    FNetAddress : TNetAddr;
    FAddrType : Integer;
    Function  GetAsString : String;
    Procedure SaveNetEntry(Entry : Pointer);
    Function GetNetAddress : TNetAddr;
  Public
    Procedure ClearData; override;
    Function NameLookup(Const S : String) : boolean; override;
    Function AddressLookup(Const S : String) : Boolean; virtual;
    Function AddressLookup(Const Address : TNetAddr) : Boolean; virtual;
    Property NetAddress : TNetAddr Read FNetAddress;
    Property NetNetAddress : TNetAddr Read GetNetAddress;
    Property AddressAsString : String Read GetAsString;
    Property AddressType : Integer Read FAddrType;
  end;

{ ---------------------------------------------------------------------
    TServiceResolver
  ---------------------------------------------------------------------}

  TServiceResolver = Class(TResolver)
  private
    FProtocol : String;
    FPort : Integer;
    Procedure SaveServiceEntry(Entry : Pointer);
    Function GetNetPort : Integer ;
  public
    Procedure ClearData; override;
    Function NameLookup (Const S : String) : boolean; override;
    Function NameLookup (Const S,Proto : String) : Boolean;
    Function PortLookup (APort : Longint; Proto: string) : Boolean;
    Property Protocol : String Read FProtocol;
    Property Port : Integer Read FPort;
    Property NetPort : Integer Read GetNetPort;
  end;

  TURIParser = Class(TComponent)
  Private
    FActive : Boolean;
    FProtocol: String;
    FUsername: String;
    FPassword: String;
    FHost: String;
    FPort: Word;
    FPath: String;
    FDocument: String;
    FParams: String;
    FBookmark: String;
    FURI : String;
  Protected  
    Procedure SetElement (Index : Integer; Value : String);Virtual;
    Function GetElement(Index : Integer) : String;
    Procedure SetPort(Value : Word);
    Procedure SetURI(Value : String);
  Public  
    Procedure Clear;   
    Procedure ParseUri(AURI : String);
    Function ComposeURI : String;
  Published
    Property Port: Word  Read FPort Write SetPort;
    Property Protocol: String Index 0 Read GetElement Write SetElement;
    Property Username: String Index 1 Read GetElement Write SetElement;
    Property Password: String Index 2 Read GetElement Write SetElement;
    Property Host: String     Index 3 Read GetElement Write SetElement;
    Property Path: String     index 4 Read GetElement Write SetElement;
    Property Document: String index 5 read GetElement Write SetElement;
    Property Params: String   Index 6 read GetElement Write SetElement;
    Property Bookmark: String Index 7 Read GetElement Write SetElement;
    Property URI : String Read FURI write SetURI; 
    Property Active : Boolean Read FActive Write FActive;
  end;


Resourcestring
  SErrHostByName = 'Host by name';
  SErrHostByAddr = 'Host by address';
  SErrNetByName  = 'Net by name';
  SErrServByName = 'Service by name';
  SErrServByPort = 'Service by port';
  
Implementation

{ ---------------------------------------------------------------------
    Include system dependent stuff.
  ---------------------------------------------------------------------}

{$ifdef usenetdb}
uses netdb;
{$else}  
{$i resolve.inc}
{$endif}

function HostAddrToStr (Entry : THostAddr) : String;

Var Dummy : String[4];
    I : Longint;

begin
  HostAddrToStr:='';
  For I:=1 to 4 do
   begin
   Str(Entry[I],Dummy);
   HostAddrToStr:=HostAddrToStr+Dummy;
   If I<4 Then 
     HostAddrToStr:=HostAddrToStr+'.';
   end;
end;

function StrToHostAddr(IP : String) : THostAddr ;

Var 
    Dummy : String;
    I     : Longint;
    J     : Integer;
    Temp : THostAddr;

begin
  Result:=NoAddress;
  For I:=1 to 4 do
   begin
   If I<4 Then
     begin
     J:=Pos('.',IP);
     If J=0 then 
       exit;
     Dummy:=Copy(IP,1,J-1);
     Delete (IP,1,J);
     end
   else
     Dummy:=IP;
   Val (Dummy,Temp[I],J);
   If J<>0 then Exit;
   end;
 Result:=Temp;
end;

function NetAddrToStr (Entry : TNetAddr) : String;

Var Dummy : String[4];
    I : Longint;

begin
  NetAddrToStr:='';
  For I:=4 downto 1 do
   begin
   Str(Entry[I],Dummy);
   NetAddrToStr:=NetAddrToStr+Dummy;
   If I>1 Then 
     NetAddrToStr:=NetAddrToStr+'.';
   end;
end;

function StrToNetAddr(IP : String) : TNetAddr;

begin
  StrToNetAddr:=TNetAddr(StrToHostAddr(IP));
end;

Function HostToNet (Host : ThostAddr) : THostAddr;

begin
  Result[1]:=Host[4];
  Result[2]:=Host[3];
  Result[3]:=Host[2];
  Result[4]:=Host[1];
end;

Function NetToHost (Net : TNetAddr) : TNetAddr;

begin
  Result[1]:=Net[4];
  Result[2]:=Net[3];
  Result[3]:=Net[2];
  Result[4]:=Net[1];
end;

Function HostToNet (Host : Longint) : Longint;

begin
  Result:=Longint(HostToNet(THostAddr(host)));
end;

Function NetToHost (Net : Longint) : Longint;

begin
  Result:=Longint(NetToHost(TNetAddr(Net)));
end;

Function ShortHostToNet (Host : Word) : Word;

begin
  ShortHostToNet:=lo(host)*256+Hi(Host);
end;

Function ShortNetToHost (Net : Word) : Word;

begin
  ShortNetToHost:=lo(Net)*256+Hi(Net);
end;

{ ---------------------------------------------------------------------
  TResolver
  ---------------------------------------------------------------------}

Constructor TResolver.Create(AOwner : TComponent);

begin
  Inherited;
  FAliases:=TstringList.Create;
end;

Destructor TResolver.Destroy;

begin
  ClearData;
  FAliases.Free;
end;

Procedure TResolver.ClearData;

begin
  FName:='';
  FAliases.Clear;
end;

Function TResolver.GetAlias(Index : Integer) : STring;

begin
  Result:=FAliases[Index];
end;

Function TResolver.GetAliasCount : Integer;

begin
  Result:=FAliases.Count;
end;

Function TResolver.GetAliasSorted : Boolean;

begin
  Result:=FAliases.Sorted;
end;

Procedure TResolver.SetAliasSorted (Value : Boolean);

begin
  FAliases.Sorted:=Value;
end;

Procedure TResolver.CheckOperation(Msg : String);

begin
end;

Function TResolver.NameLookup(Const S : String) : Boolean;

begin
  ClearData;
  FName:=S;
  Result:=True;
end;

Procedure TResolver.SaveAliases(P : PPChar);

Var
  I : Integer;

begin
  If (P<>Nil) then
    begin
    I:=0;
    While P[I]<>Nil do
      begin
      FAliases.Add(StrPas(P[I]));
      Inc(I);
      end;
    end;
end;


{ ---------------------------------------------------------------------
  THostResolver
  ---------------------------------------------------------------------}

Function THostResolver.GetAddress (Index : Integer) : THostAddr;

begin
  If (Index>=0) and (Index<FAddressCount) then
    Result:=FAddresses[Index];
end;

Function THostResolver.GetAsString : String;

begin
  Result:=HostAddrToStr(FHostAddress);
end;

Procedure THostResolver.ClearData;

begin
  Inherited;
  FHostAddress:=NoAddress;
  If FAddressCount<>0 Then
    FreeMem(FAddresses);
  FAddressCount:=0;
  FAddresses:=Nil;
end;

Function THostResolver.AddressLookup(Const S : String) : Boolean;

begin
  Result:=AddressLookup(StrToHostAddr(S));
end;

{$ifdef usenetdb}
Function THostResolver.NameLookup (Const S : String) : Boolean;

Var
  H : THostEntry;

begin
  Result:=Inherited NameLookup(S);
  If Result then
    begin
    Result:=GetHostByName(S,H);
    If Result then
      SaveHostEntry(@H);
    end;
end;

Function THostResolver.AddressLookup (Const Address: THostAddr) : Boolean;

Var
  H : THostEntry;

begin
  ClearData;
  Result:=GetHostByAddr(Address,H);
  If Result then
    SaveHostEntry(@H);
end;

Procedure THostResolver.SaveHostEntry(Entry : Pointer);

Var
  PH : ^THostEntry;
  I : Integer;
  
begin
  PH:=ENtry;
  FName:=PH^.Name;
  FHostAddress:=PH^.Addr;
  FAddressCount:=1;
  GetMem(FAddresses,SizeOf(THostAddr));
  FAddresses[0]:=PH^.Addr;
  FAliases.CommaText:=PH^.Aliases;
end;

{$else}

Function THostResolver.NameLookup (Const S : String) : Boolean;

Var
  FHostEntry : PHostEntry;

begin
  Result:=Inherited NameLookup(S);
  If Result then
    begin
    FHostEntry:=GetHostByName(pchar(FName));
    Result:=FHostEntry<>Nil;
    If Result then
      SaveHostEntry(FHostEntry)
    else
      begin
      FLastError:=GetDNSError;
      CheckOperation(SErrHostByName);
      end;
    end;
end;

Procedure THostResolver.SaveHostEntry(Entry : Pointer);

Var
  P : Pointer;
  I,Count : Integer;

begin
  With PHostEntry(Entry)^ do
    begin
    FName:=StrPas(H_Name);
    FAddressCount:=0;
    While H_Addr[FAddressCount]<>Nil do
      Inc(FAddressCount);
    If FAddressCount>0 then
      begin  
      GetMem(FAddresses,FAddressCount*SizeOf(THostAddr));
      For I:=0 to FAddressCount-1 do
        FAddresses[I]:=PHostAddr(H_Addr[I])^;
      FHostAddress:=FAddresses[0];
      end;
    SaveAliases(H_Aliases);  
    end;
end;

Function THostResolver.AddressLookup (Const Address: THostAddr) : Boolean;

Var
  FHostEntry : PHostEntry;

begin
  ClearData;
  FHostEntry:=GetHostByAddr(Pchar(@Address),SizeOf(Address),AF_INET);
  Result:=FHostEntry<>Nil;
  If Result then
    SaveHostEntry(FHostEntry)
  else
    begin
    FLastError:=GetDNSError;
    CheckOperation(SErrHostByAddr);
    end;
end;
{$endif}

Function THostResolver.GetNetAddress (Index : Integer) : THostAddr;

begin
  Result:=HostToNet(Addresses[Index]);
end;

Function THostResolver.GetNetHostAddress : THostAddr;

begin
  Result:=HostToNet(FHostAddress);
end;


{ ---------------------------------------------------------------------
    TNetResolver
  ---------------------------------------------------------------------}

{$ifdef usenetdb}
Function TNetResolver.AddressLookup (Const Address: TNetAddr) : boolean;

Var
  N : TNetworkEntry;

begin
  ClearData;
  Result:=GetNetworkByAddr(Address,N);
  If Result then
    SaveNetEntry(@N);
end;

Function TNetResolver.NameLookup (Const S : String) : Boolean;

Var
  N : TNetworkEntry;

begin
  Result:=Inherited NameLookup(S);
  If Result then
    begin
    Result:=GetNetworkByName(S,N);
    If Result then
      SaveNetEntry(@N);
    end;
end;

Procedure TNetResolver.SaveNetEntry(Entry : Pointer);

Var
  PN : ^TNetworkEntry;

begin
  PN:=ENtry;
  FName:=PN^.Name;
  FNetAddress:=PN^.Addr;
  FAliases.CommaText:=PN^.Aliases;
end;

{$else}
Function TNetResolver.NameLookup (Const S : String) : Boolean;

Var
  FNetEntry : PNetEntry;

begin
  Result:=Inherited NameLookup(S);
  If Result then
    begin
    FNetEntry:=GetNetByName(pchar(S));
    Result:=FNetEntry<>Nil;
    If Result then
      SaveNetEntry(FNetEntry)
    else
      begin
      FLastError:=GetDNSError;
      Checkoperation(SErrNetByName);
      end;
    end;
end;

Procedure TNetResolver.SaveNetEntry(Entry : Pointer);

begin
  With PNetEntry(Entry)^ do
    begin
    FName:=StrPas(N_Name);
    FAddrType:=N_addrtype;
    FNetAddress:=NetToHost(TNetAddr(N_net));
    SaveAliases(N_Aliases);
    end;
end;

Function TNetResolver.AddressLookup (Const Address: TNetAddr) : boolean;

Var
  FNetEntry : PNetEntry;

begin
  ClearData;
{$ifndef win32}
  FNetEntry:=GetNetByAddr(Longint(HostToNet(Address)),AF_INET);
{$else}  
  FNetEntry:=Nil;
{$endif}
  Result:=FNetEntry<>Nil;
  If Result then
    SaveNetEntry(FNetEntry)
  else
    begin
    FLastError:=GetDNSError;
    CheckOperation(SErrNetByName);
    end;
end;

{$endif}

Function TNetResolver.AddressLookup(Const S : String) : Boolean;

begin
  Result:=AddressLookup(StrToNetAddr(S));
end;


Function TNetResolver.GetAsString : String;

begin
  Result:=HostAddrToStr(FNetAddress);
end;

Function TNetResolver.GetNetAddress : TNetAddr;

begin
  Result:=HostToNet(FNetAddress);
end;


Procedure TNetResolver.ClearData;

begin
  Inherited;
  FNetAddress:=NoAddress;
  FAddrType:=0;
end;

{ ---------------------------------------------------------------------
    TServiceResolver
  ---------------------------------------------------------------------}
  
Function TServiceResolver.NameLookup (Const S : String) : Boolean;

begin
  Result:=NameLookup(S,'');
end;

{$ifdef usenetdb}

Function TServiceResolver.NameLookup (Const S,Proto : String) : Boolean;

Var
  E : TServiceEntry;

begin
  ClearData;
  Result:=GetServiceByName(S,Proto,E);
  If Result then
    SaveServiceEntry(@E);
end;

Function TServiceResolver.PortLookup (APort: Longint; Proto : String) : Boolean;

Var
  E : TServiceEntry;

begin
  ClearData;
  Result:=GetServiceByPort(APort,Proto,E);
  If Result then
    SaveServiceEntry(@E);
end;

Procedure TServiceResolver.SaveServiceEntry(Entry : Pointer);

Var
  PE : ^TServiceEntry;

begin
  PE:=Entry;
  FName:=PE^.Name;
  FPort:=PE^.Port;
  FProtocol:=PE^.Protocol;
  FAliases.CommaText:=PE^.Aliases;
end;

{$else}

Function TServiceResolver.NameLookup (Const S,Proto : String) : Boolean;

Var
  FServiceEntry : PServEntry;

begin
  ClearData;
  FName:=S;
  FProtocol:=Proto;
  If (proto='') then
    FServiceEntry:=GetServByName(pchar(S),Nil)
  else
    FServiceEntry:=GetServByName(pchar(S),PChar(FProtocol));
  Result:=FServiceEntry<>Nil;
  If Result then
    SaveServiceEntry(FServiceEntry)
  else  
    begin
    FLastError:=GetDNSError;
    CheckOperation(SErrServByName);
    end;
end;

Function TServiceResolver.PortLookup (APort: Longint; Proto : String) : Boolean;

Var
  FServiceEntry : PServEntry;

begin
  ClearData;
  APort:=ShortHostToNet(APort);
  FProtoCol:=Proto;
  If (Proto='') then
    FServiceEntry:=GetServByPort(APort,Nil)
  else  
    FServiceEntry:=GetServByPort(APort,pchar(Proto));
  Result:=FServiceEntry<>Nil;
  If Result then
    SaveServiceEntry(FServiceEntry)
  else
    begin
    FLastError:=GetDNSError;
    CheckOperation(SErrServByPort);
    end;
end;

Procedure TServiceResolver.SaveServiceEntry(Entry : Pointer);

begin
  With PServEntry(Entry)^ do
   begin
   FName:=strpas(s_name);
   FPort:=ShortHostToNet(S_port);
   FProtocol:=strpas(s_proto);
   SaveAliases(S_aliases);
   end;    
end;
{$endif}

Procedure TServiceResolver.ClearData;

begin
  Inherited;
  FProtocol:='';
  FPort:=0;
end;

Function TServiceResolver.GetNetPort : Integer;

begin
  Result:=ShortHostToNet(FPort);
end;

{ ---------------------------------------------------------------------
    TURIParser
  ---------------------------------------------------------------------}
  

Procedure TURIParser.SetElement (Index : Integer; Value : String);

begin
 Case index of
   0  : FProtocol := Value;
   1  : FUsername := Value;
   2  : FPassword := Value;
   3  : FHost     := Value;
   4  : FPath     := Value;
   5  : FDocument := Value;
   6  : FParams   := Value;
   7  : FBookmark := Value;
  else  
  end;
  If FActive and not (csLoading in ComponentState) then
    FURI:=ComposeURI;
end;

Function  TURIParser.GetElement(Index : Integer) : String;

begin
  Case Index of
  0  : Result := FProtocol;
  1  : Result := FUsername;
  2  : Result := FPassword;
  3  : Result := FHost    ;
  4  : Result := FPath    ;
  5  : Result := FDocument;
  6  : Result := FParams  ;
  7  : Result := FBookmark;
  else  
    Result:='';
  end;  
end;

Procedure TURIParser.SetPort(Value : Word);

begin
  FPort:=Value;
  If FActive and not (csLoading in ComponentState) then
    FURI:=ComposeURI;
end;

Procedure TURIParser.SetURI(Value : String);

begin
  If Active and not (csLoading in ComponentState) then
    begin
    Clear;
    ParseUri(Value);
    end;
  FURI:=Value;  
end;

Procedure TURIParser.Clear;   

begin
   FProtocol :='';
   FUsername :='';
   FPassword :='';
   FHost     :='';
   FPort     :=0;
   FPath     :='';
   FDocument :='';
   FParams   :='';
   FBookmark :='';
   FURI      :='';
end;

Procedure TURIParser.ParseUri(AURI : String);

Var
  U : TURI;
  
begin
  U:=UriParser.ParseURI(AUri);
  FProtocol := u.Protocol;
  FUsername := u.Username;
  FPassword := u.Password;
  FHost     := u.Host    ;
  FPort     := u.Port    ;
  FPath     := u.Path    ;
  FDocument := u.Document;
  FParams   := u.Params  ;
  FBookmark := u.Bookmark;
end;


Function  TURIParser.ComposeURI : String;

var
  U : TURI;

begin
  U.Protocol := FProtocol;
  U.Username := FUsername;
  U.Password := FPassword;
  U.Host     := FHost    ;
  U.Port     := FPort    ;
  U.Path     := FPath    ;
  U.Document := FDocument;
  U.Params   := FParams  ;
  U.Bookmark := FBookmark;
  Result:=EncodeUri(U);
end;


{$ifdef usenetdb}
Procedure InitResolve;

begin
end; 

Procedure FinalResolve;

begin
end; 
{$endif}

Initialization
  InitResolve;
  
Finalization
  FinalResolve;

end.
{
   $Log: resolve.pp,v $
   Revision 1.4  2003/05/17 21:52:37  michael
   + Added TURIParser class

   Revision 1.3  2003/03/07 20:33:33  michael
   Use native pascal netdb on Linux

   Revision 1.2  2003/02/03 10:14:12  michael
   + Added init/final routines to initialize winsock library

   Revision 1.1  2003/02/01 16:50:38  michael
   + Added resolve unit for WIndows/unix

}
