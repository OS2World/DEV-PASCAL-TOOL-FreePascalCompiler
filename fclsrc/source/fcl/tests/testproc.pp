program testproc;

uses classes,process;

Const BufSize = 1024;

{$ifdef unix}
      TheProgram = 'doecho';
{$else}
      TheProgram = 'doecho.exe';
{$endif}


Var S : TProcess;
    Buf : Array[1..BUFSIZE] of char;
    I,Count : longint;

begin
  S:=TProcess.Create(Nil);
  S.Commandline:=theprogram;
  S.Options:=[poUsePipes,poNoConsole];
  S.execute;
  Repeat
    Count:=s.output.read(buf,BufSize);
    // reverse print for fun.
    For I:=Count downto 1 do
      write(buf[i]);
  until Count=0;
  writeln;
  S.Free;
end.

{
   $Log: testproc.pp,v $
   Revision 1.5  2002/09/07 15:15:28  peter
     * old logs removed and tabs fixed

   Revision 1.4  2002/05/31 11:33:49  marco
    * 1.0.x renamefest

     Revision 1.3  2002/05/18 13:38:30  michael
     + Fixed test program to new interface

}
