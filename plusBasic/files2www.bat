@REM Copy assembled ROM and documentation to local www folder
@SET _dest=..\www\release
copy zout\sysrom.bin %_dest%
copy quickref.html %_dest%\quickref.html
copy releases.html %_dest%\releases.html
copy quickref.md %_dest%\quickref.md
copy ..\util\bas2baq.py %_dest%
copy ..\util\baq2bas.py %_dest%
copy ..\pbutil\getver.bas %_dest%
copy ..\pbutil\myip.bas %_dest%
