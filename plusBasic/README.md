# plusBASIC for the Aquarius+

Requires S3 BASIC assembled with "aqplus" switch

Based on and replaces Aquarius+ System ROM

## Internal Code Changes:

  - Optimized Hook, Statement, and Function Dispatch routines
  - Added Extended Keyboard decode tables used by S3 BASIC
  - Calls STRPRI instead of STROUT for null terminated strings

## Additions

### BASIC

  - CD$ pseudo-variable returns current path

### Assembly

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
