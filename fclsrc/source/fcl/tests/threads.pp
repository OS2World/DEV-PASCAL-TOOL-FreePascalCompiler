program testthreads;

{$mode objfpc}

uses
  sysutils,
  classes;

type
  TMyThread=class(TThread)
  private
    ch : char;
  protected
    procedure Execute; override;
  public
    constructor Create(c:char);
  end;

procedure TMyThread.Execute;
begin
  repeat
    write(ch);
  until Terminated;
end;


constructor TMyThread.Create(c:char);
begin
  ch:=c;
  inherited Create(false);
end;

var
  t1,t2 : TMyThread;
begin
  t1:=TMyThread.Create('a');
  t2:=TMyThread.Create('b');
  readln;
  t2.Terminate;
  readln;
  t1.Terminate;
  readln;
  t2.Destroy;
  t1.Destroy;
end.
{
  $Log: threads.pp,v $
  Revision 1.4  2002/09/07 21:03:37  carl
    * correct termination of thread

  Revision 1.3  2002/09/07 15:15:28  peter
    * old logs removed and tabs fixed

}
