@REM Build plusBASIC ROM
@ECHO OFF
SET _opts=
if "%1" NEQ "" SET _opts=-D%1
zmac --zmac -c -n %_opts% aqplusbas.asm -I auxrom -I basic -I gfxrom --oo cim,lst
