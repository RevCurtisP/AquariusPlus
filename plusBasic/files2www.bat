@REM Copy assembled ROM and documentation to local www folder
@SET _wwwdir=..\www
@SET _reldir=%_wwwdir%\release
@SET _devdir=%_wwwdir%\dev
copy zout\sysrom.bin %_reldir%
copy pt3player\zout\main.cim %_reldir%\ptplay.bin
copy README.html %_reldir%\quickref.html
copy README.md %_reldir%\quickref.md
copy releases.html %_reldir%
copy releases.md %_reldir%
copy ..\util\bas2baq.py %_reldir%
copy ..\util\baq2bas.py %_reldir%
copy ..\pbutil\getver.bas %_reldir%
copy ..\pbutil\myip.bas %_reldir%
copy ..\dev\aqplus.inc %_devdir%
