@echo off
echo Building unit tests
for %%f in (??.bas) do (
  echo %%~nf
  if %%f equ au.bas (
    copy /B %%f %%~nf.$$$ > nul
  ) else (
    copy /B %%f+aqpunit.bas %%~nf.$$$ > nul
  )
  python ..\util\bas2baq.py %%~nf.$$$ %%~nf.baq
  if "%errorlevel%" neq "0" (goto:eof)
  move %%~nf.baq %aquaplus%\sdcard\au\ >nul
  del %%~nf.$$$
)
echo Build complete
