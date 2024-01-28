@REM Move Assembled ROM to AquaLite ROMs Directory
@REM Requires environment variable "AquaPlus" SET to 
@REM   AquaLite base directory
copy zout\sysrom.bin %AquaPlus%\sdcard
