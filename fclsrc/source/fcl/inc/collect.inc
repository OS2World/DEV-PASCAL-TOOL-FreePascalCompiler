{
    $Id: collect.inc,v 1.3 2002/09/07 15:15:24 peter Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{****************************************************************************}
{*                             TCollectionItem                              *}
{****************************************************************************}


function TCollectionItem.GetIndex: Integer;

begin
  if FCollection<>nil then
    Result:=FCollection.FItems.IndexOf(Pointer(Self))
  else
    Result:=-1;
end;



procedure TCollectionItem.SetCollection(Value: TCollection);

begin
  IF Value<>FCollection then
    begin
    If FCollection<>Nil then FCollection.RemoveItem(Self);
    if Value<>Nil then Value.InsertItem(Self);
    FCollection:=Value;
    end;
end;



procedure TCollectionItem.Changed(AllItems: Boolean);

begin
 If (FCollection<>Nil) then
  begin
  If AllItems then
    FCollection.Update(Nil)
  else
    FCollection.Update(Self);
  end;
end;



function TCollectionItem.GetNamePath: string;

begin
  If FCollection<>Nil then
    Result:=FCollection.GetNamePath+'['+IntToStr(Index)+']'
  else
    Result:=ClassName;
end;


function TCollectionItem.GetOwner: TPersistent;

begin
  Result:=FCollection;
end;



function TCollectionItem.GetDisplayName: string;

begin
  Result:=ClassName;
end;



procedure TCollectionItem.SetIndex(Value: Integer);

Var Temp : Longint;

begin
  Temp:=GetIndex;
  If (Temp>-1) and (Temp<>Value) then
    begin
    FCollection.FItems.Move(Temp,Value);
    Changed(True);
    end;
end;


procedure TCollectionItem.SetDisplayName(const Value: string);

begin
  Changed(False);
end;



constructor TCollectionItem.Create(ACollection: TCollection);

begin
  Inherited Create;
  SetCollection(ACollection);
end;



destructor TCollectionItem.Destroy;

begin
  SetCollection(Nil);
  Inherited Destroy;
end;

{****************************************************************************}
{*                             TCollection                                  *}
{****************************************************************************}



function TCollection.GetCount: Integer;

begin
  If Assigned(FItems) Then
    Result:=FItems.Count
  else
    Result:=0;
end;


Procedure TCollection.SetPropName;

begin
  //!! Should be replaced by the proper routines.
  FPropName:='';
end;


function TCollection.GetPropName: string;

Var TheOWner : TPersistent;

begin
  Result:=FPropNAme;
  TheOWner:=GetOwner;
  If (Result<>'') or (TheOwner=Nil) Or (TheOwner.Classinfo=Nil) then exit;
  SetPropName;
  Result:=FPropName;
end;



procedure TCollection.InsertItem(Item: TCollectionItem);

begin
  If Not(Item Is FitemClass) then
    exit;
  FItems.add(Pointer(Item));
  Item.FID:=FNextID;
  inc(FNextID);
  SetItemName(Item);
  Changed;
end;



procedure TCollection.RemoveItem(Item: TCollectionItem);

begin
  FItems.Remove(Pointer(Item));
  Item.FCollection:=Nil;
  Changed;
end;


function TCollection.GetAttrCount: Integer;

begin
  Result:=0;
end;


function TCollection.GetAttr(Index: Integer): string;

begin
  Result:='';
end;



function TCollection.GetItemAttr(Index, ItemIndex: Integer): string;


begin
   Result:=TCollectionItem(FItems.Items[ItemIndex]).DisplayName;
end;



function TCollection.GetNamePath: string;

Var OwnerName,ThePropName : String;

begin
  Result:=ClassName;
  If GetOwner=Nil then Exit;
  OwnerName:=GetOwner.GetNamePath;
  If OwnerName='' then Exit;
  ThePropName:=PropName;
  if ThePropName='' then exit;
  Result:=OwnerName+'.'+PropName;
end;



procedure TCollection.Changed;

begin
  Update(Nil);
end;



function TCollection.GetItem(Index: Integer): TCollectionItem;

begin
   Result:=TCollectionItem(FItems.Items[Index]);
end;



procedure TCollection.SetItem(Index: Integer; Value: TCollectionItem);

begin
  TCollectionItem(FItems.items[Index]).Assign(Value);
end;



procedure TCollection.SetItemName(Item: TCollectionItem);

begin
end;



procedure TCollection.Update(Item: TCollectionItem);

begin
end;



constructor TCollection.Create(AItemClass: TCollectionItemClass);

begin
  inherited create;
  FItemClass:=AItemClass;
  FItems:=TList.Create;
end;



destructor TCollection.Destroy;

begin
  If Assigned(FItems) Then Clear;
  FItems.Free;
  Inherited Destroy;
end;



function TCollection.Add: TCollectionItem;

begin
  Result:=FItemClass.Create(Self);
end;



procedure TCollection.Assign(Source: TPersistent);

Var I : Longint;

begin
  If Source is TCollection then
    begin
    Clear;
    For I:=0 To TCollection(Source).Count-1 do
     Add.Assign(TCollection(Source).Items[I]);
    exit;
    end
  else
    Inherited Assign(Source);
end;



procedure TCollection.BeginUpdate;

begin
end;



procedure TCollection.Clear;

begin
  If Assigned(FItems) then
    While FItems.Count>0 do TCollectionItem(FItems.Last).Free;
end;



procedure TCollection.EndUpdate;

begin
end;



function TCollection.FindItemID(ID: Integer): TCollectionItem;

Var I : Longint;

begin
  Result:=Nil;
  For I:=0 to Fitems.Count-1 do
   begin
   Result:=TCollectionItem(FItems.items[I]);
   If Result.Id=Id then exit;
   end;
end;

{
  $Log: collect.inc,v $
  Revision 1.3  2002/09/07 15:15:24  peter
    * old logs removed and tabs fixed

}
