@ECHO OFF
SET _opts=
if "%1" NEQ "" SET _opts=-D%1
ECHO ON
zmac --zmac %_opts% -o boot.cim -o boot.lst boot.asm
copy boot.cim boot.bin
