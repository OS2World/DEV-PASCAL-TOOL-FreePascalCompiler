Program Example48;

{ Program to demonstrate the BaseName function. }

Uses linux;

Var S : String;

begin
  S:=FExpand(Paramstr(0));
  Writeln ('This program is called : ',Basename(S,''));
end.
