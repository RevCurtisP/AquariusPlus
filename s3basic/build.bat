@REM Build Aquarius S3 ROM
@ECHO OFF
SET _opts=
if "%1" NEQ "" SET _opts=-D%1
if "%2" NEQ "" SET _opts=%_opts% -D%2
if "%3" NEQ "" SET _opts=%_opts% -D%3
if "%4" NEQ "" SET _opts=%_opts% -D%4

SET _tail=
if "%1" EQU "aqplus" SET _tail="-%1"
if "%1" EQU "sdbasic" SET _tail="-%1"

ECHO ON
zmac -o s3basic%_tail%.cim -o s3basic.lst %_opts% s3basic.asm
