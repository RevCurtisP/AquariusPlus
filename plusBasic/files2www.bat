@REM Copy assembled ROM and documentation to local www folder
@SET _dest=..\www\release
copy zout\sysrom.bin %_dest%
copy pt3player\zout\main.cim %_dest%\ptplay.bin
copy README.html %_dest%\quickref.html
copy README.md %_dest%\quickref.md
copy releases.html %_dest%
copy releases.md %_dest%
copy ..\util\bas2baq.py %_dest%
copy ..\util\baq2bas.py %_dest%
copy ..\pbutil\getver.bas %_dest%
copy ..\pbutil\myip.bas %_dest%
