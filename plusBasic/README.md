# plusBASIC for the Aquarius+

Requires S3 BASIC assembled with "aqplus" switch

Based on and replaces Aquarius+ System ROM

## Internal Code Changes:
  - Optimized Hook, Statement, and Function Dispatch routines
  - Added Extended Keyboard decode tables used by S3 BASIC
  - Calls STRPRI instead of STROUT for null terminated strings
  - Added standalone callable assembly routines
  - Added 1500 ms timeout to esp_read_byte
    - Timeout appears to be too short on harware, disabled
  - Added ESP error -9, "ESP times out"
  - Moved all ST_ and FN_ routines out of main assembly flie
 
## BASIC Language Enhancements 
  - CD$ pseudo-variable returns current path
  - DATE$ pseudo-variable returns current date "YYYYMMDD"
  - TIME$ pseudo-variable returns current time "HHmmss"
  - DATETIME$ pseudo-variable returns date and time "YYYYMMDD"

## Assembly Language Code Changes
  - New callable routines
    - dos_change_dir
    - dos_delete_file
    - dos_create_dir
    - dos_get_cwd



  
### Assembly Ruutines

## ToDo:
  - Make dos_create_dir and MKDIR use buffer in page 35
  - Extricate dos code from statement code and make dos routines callable from assembly.
  - Move BASIC statement and function code to ROM bank 1.
  - Kernal/BIOS jump table. Jumps start at $FFED and go down from there.
  
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
  - ON ERROR GOTO line
  - ERR$ pseudo-variable
  - ERRLINE$ pseudo-variable
  - RESUME

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

  - SCREEN mode
  - LINE 
  - CIRCLE 
  - DRAW
  
#### Strings

  - STRING$(char$, count)
  - INSTR$(str$, str$ [,pos])
  - MIDS$(var$, pos [,len]) = string expression

#### Miscellaneous
  
  - ARGS and ARGS$()
  - DATE$ pseudo-variable
  - TIME$ pseudo-variable
  - VARPTR(variable)

### Enhancements to existing Statements and Functions
 
  - CLS [fgcolor [, bgcolor]]
  - PEEK([@page,] addr)
  - PEEK!([@page,] addr) returns 16-bit word
  - PEEK$([@page], addr, length)
  - POKE [@page,] addr, byte
  - POKE! [@page,] addr, word
  - POKE [@page,] addr, string$
  - COPY [@page], addr, length TO [@page], addr
  - COPY srcfile$ TO dstfile$
  - SAVE file$, [@page,] addr, length
  - LOAD file$, [@page,] addr
