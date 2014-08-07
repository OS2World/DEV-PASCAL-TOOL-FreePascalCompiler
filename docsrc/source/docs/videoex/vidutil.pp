unit vidutil;

Interface

uses
  video;

{$ifndef cpu86}
{$error This example only works on intel 80x86 machines}
{$endif}
  

Procedure TextOut(X,Y : Word;Const S : String);

Implementation
  
Procedure TextOut(X,Y : Word;Const S : String);

Var
  W,P,I,M : Word;

begin
  P:=((X-1)+(Y-1)*ScreenWidth);
  M:=Length(S);
  If P+M>ScreenWidth*ScreenHeight then
    M:=ScreenWidth*ScreenHeight-P;
  For I:=1 to M do
    VideoBuf^[P+I-1]:=Ord(S[i])+($07 shl 8);
end;
  
end.  