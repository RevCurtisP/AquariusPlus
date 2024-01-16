@REM Build variable dump program
zmac --zmac -c -n -o dump.cim -o dump.lst dump.asm
copy dump.cim %aquaplus%\sdcard\au\dump.bin