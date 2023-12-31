# plusBASIC Release History

 - v0.20k
   - Implemented `SET TILEMAP (x,y)` and `tilemap_set_tile`
 - v0.20j2 
   - Allow both `,` and `;` as delimiters in `DEF INTLIST`
 - v0.20j1
   - Fixed `ATTR attrs` and `PALETTE palette#` causing syntax error after `FILL TILEMAP tile#`
 - v0.20j 
   - '.' treated as '0' in hex literal strings and ASC$ functiion.
 - v0.20i
   - Added BASIC functions `FILEEXT$()`, `FILEDIR$()`, `TRIMDIR$()`, `TRIMEXT$()`
   - Added kernel routines `file_get_ext`, `file_trim_dir`, `file_get_dir`, `file_trim_ext`
   - Allow unquoted filename operand for `RUN` in direct mode
   - Added direct mode syntax `DEL file file file...`
   - Added syntax `LOAD filename$,@page` and `SAVE filename$,@page`
   - Added cartridge diagnostic mode assembly option
 - v0.20h
   - Fixed 8k carts not working
 - v0.20g 
   - `FILL SCREEN`, `GET SCREEN`, `PUT SCREEN` now work in both 40 column and 80 column mode
 - v0.20f 
   - Added optional comma between end-coordinate and fill character in `FILL SCREEN`
 - v0.20e 
   - Added `KEY()` function
 - v0.20d
   - Added `DEC()` function, fixed `esp_get_version`/`VER(0)`/`VER$(0)`

