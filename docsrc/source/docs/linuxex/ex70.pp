Program Example70;

{ Program to demonstrate the StringToPPchar function. }

Uses linux;

Var S : String;
    P : PPChar;
    I : longint;
    
begin
  // remark whitespace at end.
  S:='This is a string with words. ';
  P:=StringToPPChar(S);
  I:=0;
  While P[i]<>Nil do
    begin
    Writeln('Word ',i,' : ',P[i]);
    Inc(I);
    end;
  FreeMem(P,i*SizeOf(Pchar));  
end.
