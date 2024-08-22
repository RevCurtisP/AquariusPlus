@echo off
echo Converting %1.bas + aqpunit.bas to %1.baq
copy /B %1.bas+aqpunit.bas %1.$$$ > nul
python ..\util\bas2baq.py %1.$$$ %1.baq
if %errorlevel% neq 0 goto DONE
del %1.$$$
echo Complete
:DONE
