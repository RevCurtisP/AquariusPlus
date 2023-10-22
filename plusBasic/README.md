# plusBASIC for the Aquarius+

Requires S3 BASIC assembled with "aqplus" switch

## Breaking changes
  - `SET COLOR` statement changed to `SET PALETTE`
  - `DEF COLORLIST` statement changed to `DEF PALETTELIST`
  - `GETCOLOR` function changed to `GETPALETTE`
  - `COLOR` option changed to `PALETTE` in `SPRITE` and `TILE` commands
  - `CHRSET` statement changed to `USE CHRSET`
  - `POKE!` and `PEEK!` changed to `POKEINT` and `PEEKINT`
  - `COPY!` changed to `COPY ... FAST`
  - `SET TILE` syntax `TO color_index, ...` removed

## BASIC Language Enhancements 

### Experimental
  - `SET SPRITE sprite$ TO proplist$` - set sprite properties
  - `SET TILEMAP OFFSET x , y` - set tilemap offset
  - `TILEMAPX` pseudo-variable returns tilemap x-offset
  - `TILEMAPY` pseudo-variable returns tilemap y-offset
    
### Live
  - Hexadecimal integer and string literals.
  - All statements and functions that use an address accept -65535 to 65535
  - Optional line labels
    - Useable with `GOTO`, `GOSUB`, an `RESTORE`
  - BASIC now displays long error messages
    - e.g. `Syntax error` instead of `?SN error`
    - Function Call error now displays `Illegal quantity`
  - `CD$` pseudo-variable returns current path
  - `CLS fgcolor, bgcolor` - Clear screen to specified colors
  - `COLOR #palette, entrylist` - set a palette
  - `COPY @source TO @dest` - copy entire page
  - `COPY [@page], src, len TO [@page], dst` - copy memory from one location to another
  - `DATE$` pseudo-variable returns current date "YYYYMMDD"
  - `DATETIME$` pseudo-variable returns date and time "YYYYMMDD"
  - `DEF ATTRLIST var$ = attr, attr, ...` - create attribute list
  - `DEF COLORLIST var$ = palette#, palette#, ...` - create color palette list
  - `DEF INTLIST var$ = int, int, ...` - create integer list
  - `DEF SPRITE sprite$ = spritle#, x-offset, y-offset;...` - define sprite
  - `DEF TILELIST var$ = tile#, tile#, ...` - crete tile index list
  - `ERR` pseudo-variable returns last error number
  - `ERR#` pseudo-variable returns last error message
  - `ERRLINE` pseudo-variable returns line last error occured in
  - `EVAL(string$)` - evaluates expression in string
  - `GETARGS varlist` - get arguments passed to subroutine
  - `GETCOLOR$(palette#)` - returns color palette RGB values
  - `GETKEY` - like `INKEY` but waits for keypress
  - `GETKEY$` - like `INKEY$` but waits for keypress
  - `GETSPRITE$(SpriteDef$)` - returns sprite properties
  - `GETTILE$(tile#)` - returns tile pixel data
  - `GOSUB line : ARGS arglist` - pass arguments to subroutine
  - `GOSUB line : ARGS arglist RETURN varlist` - get values returned by subroutine
  - `INKEY` - like `INKEY$` but returns ASCII code of key pressed
  - `LOAD "FILE",@page,address` - load file into page
  - `MOUSEB, MOUSEX, MOUSEY` - returns mouse position and buttons
  - `ON ERROR GOTO` - Error trapping
  - `POKE!` and `PEEK!` always return a positive integer
  - `POKE @page,address,byte` - write byte to address in page
  - `POKE! address,word` - write word to address
  - `POKE! @,page,address,word` - write word to address in page
  - `PEEK (@page,address)` - read byte from address in page
  - `PEEK! (address)` - read word from address
  - `PEEK! (@page,address)` - read word from address in page
  - `RENAME oldname$ TO newname$` - rename file
  - `RESUME` - clear error condition and continue
  - `RETURN vallist` - return values from subroutine
  - `SAVE "FILE",@page,address,length` - save from page to file
  - `SCREEN mode` - select screen mode (text, tile, bitmap, sprites)
  - `SET COLOR palette , index TO rgb_list` - set color palette
  - `SET SPRITE sprite$ ...` - set sprite properties
  - `SET TILE tile# TO tile_data` - set tile pixel data
  - `STRING$ (length, byte/string$)` - returns string of repeating characters
  - `TIME$` pseudo-variable returns current time "HHmmss"

## Internal BASIC Code Changes:
  - Fixed plusBASIC version display on welcome screen
    - Prints right justified on same line as System version if there is room
    - Prints on next line if not enough room
  - Extricated DOS and ESP machine language routines from BASIC statements and functions
    - routines can now be passed BASIC string descriptors, eliminating filename buffer
  - Optimized Hook, Statement, and Function Dispatch routines
  - Added Extended Keyboard decode tables used by S3 BASIC
  - Calls STRPRI instead of STROUT for null terminated strings
  - Added standalone callable assembly routines
  - Added 1500 ms timeout to esp_read_byte
    - Timeout appears to be too short on harware, disabled
  - Added ESP error -9, "ESP times out"
  - Moved all ST_ and FN_ routines out of main assembly flie

## Assembly Language Code Changes
  - New callable routines
    - dos_change_dir
    - dos_delete_file
    - dos_create_dir
    - dos_get_cwd

### Assembly Routines

## ToDo:
  - Make dos_create_dir and MKDIR use buffer in page 35
  - Move common code in esp__bytes and esp__paged into callable routines
  - Move esp_open, esp_create, esp_close, esp_closeall and rename to dos_
  - Kernal/BIOS jump table. Jumps start at $2100
  - Update txt2bas.py and bas2txt.py
  - Create USB-BASIC/MX-BASIC to plusBASIC conversion utility
  
## Proposed Additions and Enhancements

### Direct Mode Enhancements
  - Command history (Page Up and Down)
  - Enhanced Line Editor
    - Cursor Left, Right, Up, DOwn
    - Home and End
    - Insert On/Off
    - Delete
    - 
### New Statements and Functions

Some statements will use compound tokens, for example INSTR$ would
be the token for IN followed by the token for STR$.

### Error Handling

  - Make ESP and DOS errors generate BASIC errors
    - Start at 50 like CP/M BASIC does with disk erros

#### File I/O

  - fd = OPEN(filename$ [, mode$])
  - fd = OPEN("tcp://address[,port]")
  - fd = OPENDIR(path$) 
  - CLOSE(fd) 
    - CLOSE(\*) to close all files
  - PRINT# fd, ...
  - INPUT# fd, var$, ...
  - LINE INPUT# fd, var$
  - GET# fd, var$ [, len]
  - PUT# fd, var$, len
    - will pad or truncate contents of var$ to len
  - GOTO# fd, position
    - SEEK but reusing an existing token
  - Possibly READ# and OUTPUT# 

#### Graphics

  - LINE 
  - CIRCLE 
  - DRAW
  
#### Strings

  - INSTRING$(str$, str$ [,pos])
  - MIDS$(var$, pos [,len]) = string expression

#### Miscellaneous
  
  - VARPTR(variable)

### Enhancements to existing Statements and Functions
 
  - COPY srcfile$ TO dstfile$
