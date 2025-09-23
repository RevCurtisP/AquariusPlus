@echo off
echo Building unit tests
for %%f in (??.bas) do (
  echo %%~nf
  if %%f equ au.bas (
    copy /B %%f %%~nf.$$$ > nul
  ) else (
    copy /B %%f+aqpunit.bas %%~nf.$$$ > nul
  )
  python ..\util\bas2baq.py %1 %%~nf.$$$ %%~nf.baq
  if "%errorlevel%" neq "0" (goto:eof)
  timeout 1 > nul
  move %%~nf.baq %aquaplus%\sdcard\au\ >nul
  del %%~nf.$$$
)

echo Building list of tests
findstr /C:".baq  |" README.md >tests.txt
copy tests.txt %aquaplus%\sdcard\au\

echo Build complete
