ppc386 -O3p3 -Ch8000000 -dI386 -dGDB -dBROWSERLOG -Sg -Fii386 -Futargets;i386 pp.pas %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 0 goto success
goto failed
:success
copy pp.exe ppc386.exe
:failed
