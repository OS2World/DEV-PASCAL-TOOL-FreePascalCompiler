{   $Id: testib.pp,v 1.4 2002/09/07 15:15:23 peter Exp $

    Copyright (c) 2000 by Pavel Stingl

    Interbase testing program

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

program TestIB;

{$linklib dl}
{$linklib crypt}

uses Interbase, SysUtils;

var
  Database : TIBDatabase;
  Trans    : TIBTransaction;
  Query    : TIBQuery;
  x        : integer;

begin
  Database := TIBDatabase.Create(nil);
  Trans    := TIBTransaction.Create(nil);
  Query    := TIBQuery.Create(nil);

  Database.DatabaseName := 'test.gdb';
  Database.UserName     := 'sysdba';
  Database.Password     := 'masterkey';
  Database.Transaction  := Trans;
  Trans.Action          := caRollback;
  Trans.Active          := True;


  Write('Opening database... Database.Connected = ');
  Database.Open;
  WriteLn(Database.Connected);

  // Assigning database to dataset
  Query.Database := Database;

  Query.SQL.Add('select * from fpdev');
  Query.Open;

  WriteLn;

  while not Query.EOF do
  begin
    for x := 0 to Query.FieldCount - 2 do
      Write(Query.Fields[x].AsString,',');
    WriteLn(Query.Fields[Query.FieldCount - 1].AsString);
    Query.Next;
  end;

  WriteLn;


  try
    WriteLn('Trying to insert new record to table fpdev');
    Query.Close;
    Query.SQL.Clear;
    Query.SQL.Add('insert into fpdev values (''9'',''John Doe'',''jd@unknown.net'')');
    Query.ExecSQL;
    Trans.CommitRetaining;
    WriteLn('Insert succeeded.');
  except
    on E:Exception do
    begin
      WriteLn(E.Message);
      WriteLn('Error when inserting record. Transaction rollback.');
      Trans.RollbackRetaining;
    end;
  end;

  WriteLn;

  Trans.Commit;

  Write('Closing database... Database.Connected = ');
  Database.Close;
  WriteLn(Database.Connected);
end.

{
  $Log: testib.pp,v $
  Revision 1.4  2002/09/07 15:15:23  peter
    * old logs removed and tabs fixed

}
