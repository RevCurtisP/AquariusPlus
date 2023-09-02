# Integer, Floating-Point and String Constants

Constants are the data values that you put in your BASIC statements. BASIC uses these values to represent data during statement execution. Aquarius BASIC can recognize and manipulate three types of constants:

 1. _Integer Numbers_
 2. _Floating-Point Numbers_
 3. _Strings_

## Integer Constants

Integer constants are whole numbers (numbers without decimal points). Integer constants must be between -65535  and +65535. Integer constants
do not have decimal points or commas between digits. If the plus (+) sign is left out, the constant is assumed to be a positive number. Zeros
coming before a constant are ignored and shouldn't be used since they waste memory and slow down your program. However, they won't cause an
error. Integers are stored in memory as two-byte binary numbers. 

Some examples of integer constants are:
 + `   -12`
 + `  8765`
 + `-32768`
 + `   +44`
 + `     0`
 + `-32767`
  
**_NOTE: Do NOT put commas inside any number. For example, always type 32,000 as 32000. If you put a comma in the middle of a number you will get the BASIC error message ?SYNTAX ERROR._**

**_Advanced:_** Integers are stored in memory as floating-point numbers
(see below).

***
## _plusBASIC enhancements_

### Hexadecimal Constants
Hexadecimal constants evaluate to an integer between 0 and 65525. The syntax is a dollar sign followed a series of hexadecimal digits. The hexadecimal number must be between $0 and $FFFF. Any number of zero is allowed directly after the dollar sign, but they will slow down your program.

Some examples of Hexadecimal integer constants are:
 + `  $42`
 + ` $00A`
 + `$8123`
 + `$C002`
 + `$FFFF`

### ASCII Constants
ASCII constants evaluate to the ASCII value of a character. The syntax is a single quote, a single character, and another single quote. Both the start and end quote must be there, and must be one and only one character between them. Any printable character is allowed, even a single quote.

Some examples of ASCII constants are:
 + `' '`
 + `'0'`
 + `'''`
 + `'@'`
 + `'z'`

**Note:** '_x_' is functionally identical to ASC("_x_").

***
