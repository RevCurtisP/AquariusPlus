;-----------------------------------------------------------------------------
; regs.inc
;
; This file contains locations of hardware registers and ROM/RAM locations.
;
; Locations to the original Aquarius ROM are taken from the dissassembly by
; Curtis F Kaylor, which can be found at:
; https://github.com/RevCurtisP/Aquarius/blob/main/disassembly/aquarius-rom.lst
;
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; System variables
;-----------------------------------------------------------------------------
SCREEN:     equ $3000   ; Screen Character Matrix
COLOR:      equ $3400   ; Screen Color Matrix
TTYPOS:     equ $3800   ; [M80] STORE TERMINAL POSITION HERE
CURRAM:     equ $3801   ; Position in CHARACTER RAM of cursor
USRPOK:     equ $3803   ; JP instruction for USR() routine
USRADD:     equ $3804   ; Address of USR routine
HOOK:       equ $3806   ; Extended ROM Hook Vector
CNTOFL:     equ $3808   ; Line Counter. If not 0,
LASTFF:     equ $3809   ; Last value last output to port #FF
CHARC:      equ $380A   ; [M80] ISCNTC STORES EATEN CHAR HERE WHEN NOT A ^C
RESPTR:     equ $380B   ; Pointer to Ctrl-Key Reserved Word
CURCHR:     equ $380D   ; Character under Cursor
LSTX:       equ $380E   ; [M64] Matrix Coordinate of Last Key Pressed
KCOUNT:     equ $380F   ; Keyboard debounce counter
FDIVC:      equ $3810   ;
FDIVB:      equ $3814   ;
FDIVA:      equ $3818   ;
FDIVG:      equ $381B   ;
;               $381E   ; {M80} RND FUNCTION PERTUBATION COUNT
RNDCNT:     equ $381F   ;
;               $3820   ;
                        ; (S3BASIC) Deprecated RND Permutation Table
;        $3821 - $3837  ; Unused
                        ; Extended BASIC Graphics Routine Variables

BANK0PAGE   equ $3830   ; Page that was banked into RAM
BANK1PAGE   equ $3831   ; Page that was banked into RAM
BANK2PAGE   equ $3832   ; Page that was banked into RAM
BANK3PAGE   equ $3833   ; Page that was banked into RAM
;               $3834   ; unused
;               $3835   ; unused
FILEDATE    equ $3836
FILETIME    equ $3838
FILEATTR    equ $383A
BINPAGE     equ $383B   ; Binary file load/save page
BINSTART:   equ $383C   ; Binary file load/save address
BINLEN:     equ $383E   ; 16-bit binary file length
FILESIZE    equ $383B   ; File Size (32-bits $383C-$383F)
ESP_FDESC   equ $3840   ; ESP file descriptor to close on Error or Ctrl-C

RNDX:       equ $3841   ; [M80] LAST RANDOM NUMBER GENERATED, BETWEEN 0 AND 1
LPTPOS:     equ $3846   ; [M80] POSITION OF LPT PRINT HEAD
PRTFLG:     equ $3847   ; [M80] WHETHER OUTPUT GOES TO LPT
LINLEN:     equ $3848   ; Length of a Screen Line
CLMLST:     equ $3849   ;
RUBSW:      equ $384A   ; [M80] RUBOUT SWITCH =1 INSIDE THE PROCESSING OF A RUBOUT (INLIN)
TOPMEM:     equ $384B   ; [M80] TOP LOCATION TO USE FOR THE STACK INITIALLY SET UP BY INIT
                        ; [M80] ACCORDING TO MEMORY SIZE TO ALLOW FOR 50 BYTES OF STRING SPACE.
                        ; [M80] CHANGED BY A CLEAR COMMAND WITH AN ARGUMENT.
CURLIN:     equ $384D   ; [M80] CURRENT LINE #
TXTTAB:     equ $384F   ; [M80] POINTER TO BEGINNING OF TEXT
                        ; [M80] DOESN'T CHANGE AFTER BEING SETUP BY INIT.
FILNAM:     equ $3851   ; File Name for CSAVE and CLOAD
FILNAF:     equ $3857   ; File Name Read from Cassette
INSYNC:     equ $385D   ; Contains $FF between read of SYNC and data from tape
CLFLAG:     equ $385E   ; Flags whether doing CLOAD (0) or CLOAD? ($FF)
BUFMIN:     equ $385F   ; {M80} START OF BUFFER
BUF:        equ $3860   ; [M80] TYPE IN STORED HERE. DIRECT STATEMENTS EXECUTE OUT OF HERE.
                        ; [M80] REMEMBER "INPUT" SMASHES BUF. MUST BE AT A LOWER ADDRESS THAN DSCTMP
                        ; [M80] OR ASSIGNMENT OF STRING VALUES IN DIRECT STATEMENTS WON'T COPY INTO
                        ; [M80] STRING SPACE -- WHICH IT MUST ALLOW FOR SINGLE QUOTE IN BIG LINE
ENDBUF:     equ $38A9   ; [M80] PLACE TO STOP BIG LINES
DIMFLG:     equ $38AA   ; [M80] IN GETTING A POINTER TO A VARIABLE IT IS IMPORTANT TO REMEMBER
                        ; [M80] WHETHER IT IS BEING DONE FOR "DIM" OR NOT
VALTYP:     equ $38AB   ; [M80] THE TYPE INDICATOR 0=NUMERIC 1=STRING
DORES:      equ $38AC   ; [M80] WHETHER CAN OR CAN'T CRUNCH RES'D WORDS TURNED ON WHEN "DATA"
                        ; [M80] BEING SCANNED BY CRUNCH SO UNQUOTED STRINGS WON'T BE CRUNCHED.
MEMSIZ:     equ $38AD   ; [M80] HIGHEST LOCATION IN MEMORY
TEMPPT:     equ $38AF   ; [M80] POINTER AT FIRST FREE TEMP DESCRIPTOR. INITIALIZED TO POINT TO TEMPST
TEMPST:     equ $38B1   ; [M80] STORAGE FOR NUMTMP TEMP DESCRIPTORS
DSCTMP:     equ $38BD   ; [M80] STRING FUNCTIONS BUILD ANSWER DESCRIPTOR HERE
FRETOP:     equ $38C1   ; [M80] TOP OF STRING FREE SPACE
TENP3:      equ $38C3   ; [M80] USED MOMENTARILY BY FRMEVL. USED IN EXTENDED BY FOUT AND
                        ; [M80] USER DEFINED FUNCTIONS ARRAY VARIABLE HANDLING TEMPORARY
TEMP8:      equ $38C5   ; [M80] USED TO STORE THE ADDRESS OF THE END OF STRING ARRAYS IN GARBAGE COLLECTION
ENDFOR:     equ $38C7   ; [M80] SAVED TEXT POINTER AT END OF "FOR" STATEMENT
DATLIN:     equ $38C9   ; [M80] DATA LINE # -- REMEMBER FOR ERRORS
SUBFLG:     equ $38CB   ; [M80] FLAG WHETHER SUBSCRIPTED VARIABLE ALLOWED "FOR" AND USER-DEFINED
                        ; [M80] FUNCTION POINTER FETCHING TURN THIS ON BEFORE CALLING PTRGET SO
                        ; [M80] ARRAYS WON'T BE DETECTED. STKINI AND PTRGET CLEAR IT.
USFLG:      equ $38CC   ; Direct Mode Flag
FLGINP:     equ $38CD   ; [M80] FLAGS WHETHER WE ARE DOING "INPUT" OR A READ
SAVTXT:     equ $38CE   ; [M80] PLACE WHERE NEWSTT SAVES TEXT POINTER
TENP2:      equ $38D0   ; [M80] FORMULA EVALUATOR TEMP MUST BE PRESERVED BY OPERATORS
                        ; [M80] USED IN EXTENDED BY FOUT AND USER-DEFINED FUNCTIONS
OLDLIN:     equ $38D2   ; [M80] OLD LINE NUMBER (SETUP BY ^C,"STOP" OR "END" IN A PROGRAM)
OLDTXT:     equ $38D4   ; [M80] OLD TEXT POINTER
VARTAB:     equ $38D6   ; [M80] POINTER TO START OF SIMPLE VARIABLE SPACE. UPDATED WHENEVER
                        ; [M80] THE SIZE OF THE PROGRAM CHANGES, SET TO [TXTTAB] BY SCRATCH ("NEW").
ARYTAB:     equ $38D8   ; [M80] POINTER TO BEGINNING OF ARRAY TABLE. INCREMENTED BY 6 WHENEVER
                        ; [M80] A NEW SIMPLE VARIABLE IS FOUND, AND SET TO [VARTAB] BY CLEARC.
STREND:     equ $38DA   ; [M80] END OF STORAGE IN USE. INCREASED WHENEVER A NEW ARRAY
                        ; [M80] OR SIMPLE VARIABLE IS ENCOUNTERED SET TO [VARTAB] BY CLEARC.
DATPTR:     equ $38DC   ; [M80] POINTER TO DATA. INITIALIZED TO POINT AT THE ZERO IN FRONT OF [TXTTAB]
                        ; [M80] BY "DATPTR" WHICH IS CALLED BY CLEARC. UPDATED BY EXECUTION OF A "READ"
VARNAM:     equ $38DE
VARPNT:     equ $38E0
;        $38E2:-$38E3   ; Unused
FACLO:      equ $38E4   ; [M80] LOW ORDER OF MANTISSA
FACMO:      equ $38E5   ; [M80] MIDDLE ORDER OF MANTISSA
FACHO:      equ $38E6   ; [M80] HIGH ORDER OF MANTISSA
FAC:        equ $38E7   ; [M80] EXPONENT
FBUFFR:     equ $38E8   ; [M80[ BUFFER FOR FOUT
RESHO:      equ $38F6   ; [M65] RESULT OF MULTIPLIER AND DIVIDER
RESMO:      equ $38F7   ; RESMO and RESLO are loaded into and stored from HL
RESLO:      equ $38F8   ;
SAVSTK:     equ $38F9   ; [M80] NEWSTT SAVES STACK HERE BEFORE SO THAT ERROR REVERY CAN
                        ; [M80] CAN DATPTR THE STACK WHEN AN ERROR OCCURS
INTJMP  equ     $38FB   ; RST 7 Interrupt JMP (3 bytes)
KEYADR  equ     $38FE   ;;Extended Key Tables Base Address minus 1
;               $3900   ; This is always 0
BASTXT:     equ $3901   ; Start of Basic Program


plus_page   equ 1       ; plusBASIC ROM BANK3 page

;-----------------------------------------------------------------------------
; BASIC Extended System Constants, Variables and Buffers
; Page 36, swapped into Bank 3
;-----------------------------------------------------------------------------
gfx_sysvars equ $8000   ; Extended BASIC Graphics System Variables
                        ; defined in gfx/gfx.asm
buff_strlen equ $C0FF   ; Length of String in Buffer
string_buff equ $C100   ; String Buffer  - up to 255 chars plus null terminator 
                        ; Must be on 256 byte boundary
screen_buff equ $F800   ; For saving screen memory or double-buffering


;-----------------------------------------------------------------------------
; Variables
;-----------------------------------------------------------------------------
BASIC_RAM_END: equ $BFFF    ; End of BASIC RAM, $BF00-$BFFF is reserved for our extensions

;-----------------------------------------------------------------------------
; System ROM functions
;-----------------------------------------------------------------------------
;RST
START       equ $00     ; 0 Start/Reboot
SYNCHR      equ $08     ; 1 SN Error if next character does not match
CHRGET      equ $10     ; 2 Get Next Character
OUTCHR      equ $18     ; 3 Output Character
COMPAR      equ $20     ; 4 Compare HL with DE
FSIGN       equ $28     ; 5 Get sign of Floating Point Argument
HOOKDO      equ $30     ; 6 Extended BASIC Hook Dispatch
INTRPT      equ $38     ; 7 (S3BASIC) Execute Interrupt Routine

;CALL
MAKUPR      equ $0112   ; (S3BASIC) Convert A to Upper Case
STRPRZ      equ $011B   ; (S3BASIC) Print null terminated string - Faster than STROUT
STRPRI      equ $0122   ; (S3BASIC) Print inline null terminated string 
FREADL      equ $0127   ; (S3BASIC) Free String, Return Address in DE, Length in BC
STRADL      equ $012A   ; (S3BASIC) Return String Address in DE, Length in BC from Descriptor in HL
BYTPRT      equ $0135   ; (S3BASIC) Print A as Decimal Number
UPRCMP      equ $013B   ; (S3BASIC) Compare B byets of uppercased String [HL] to String [DE] 
INITFF:     equ $0153   ; Initialize I/O Port 255
TIMSTR      equ $0E2F   ; Create tempory string from null-terminated string
REDDY       equ $036E   ; Text "Ok"
SNERR:      equ $03C4   ; Syntax error
DV0ERR:     equ $03C7   ; Division by zero error
NFERR:      equ $03CA   ; Next without for error
DDERR:      equ $03CD   ; redimensioned array error
UFERR:      equ $03D0   ; Undefined function error
OVERR:      equ $03D3   ; Overflow Error
MOERR:      equ $03D6   ; Missing Operand error
TMERR:      equ $03D9   ; Type mismatch error
ERROR:      equ $03DB   ; Process error code, E = code (offset to 2 char error name)
ERRFN1:     equ $03F4
READY:      equ $0402
MAIN        equ $0414   ; Immediate Mode loop     
CHEAD       equ $0489   ; Link Lines
CRUNCX      equ $04F9   ; Tokenize text at [HL]
RESSRC:     equ $05A8
NEWSTT:     equ $062C
CHRGT2      equ $066C   ; Get Current Character and Set Flags
FRCINT:     equ $0682   ; Convert fp number to 16 bit signed integer in DE
FCERR:      equ $0697   ; Function call error
IQERR       equ FCERR   ; Illegal quantity error (future expansion)
RUNC2:      equ $06DB
FRMNUM:     equ $0972   ; Evaluate numeric expression
CHKNUM:     equ $0975   ; Error if evaluated expression not a number
CHKSTR:     equ $0976   ; Error if evaluated expression not string
FRMEVL:     equ $0985   ; Evaluate formula
QDOT:       equ $0A14   ; Continue EVAL routine
PARCHK:     equ $0A37   ; Evaluate expression in brackets
LABBCK:     equ $0A49
ISVAR       equ $0A4E   ; Get variable value or string pointer
GIVINT      equ $0B21   ; Float Integer MSB=[A], LSB=[C]
SNGFLT:     equ $0B36   ; Store variable 8 bit (out: B = value)
GETBYT:     equ $0B54   ; Evaluate numeric expression (integer 0-255)
OMERR:      equ $0BB7   ; Out of memory error
SCRTCH:     equ $0BBE
RUNC:       equ $0BCB
CLEARC:     equ $0BCF
STKINI:     equ $0BE5
;FRMINT         $0B7F   ; Deprecated in S3BASIC, use GETINT instead
ISLETC:     equ $0CC6   ; Test for a letter
STRINI      equ $0E50   ; Reserve space for new string and create descriptor
STRLIT      equ $0E5F   ; Build descriptor for string litersl
STRLTI      equ $0E60   ; Do STRLIT, without decrementing HL
PUTNEW      equ $0E7E   ; Set Pointer to Temporary String
STROUT:     equ $0E9D   ; Print null-terminated string in HL
FRESTR      equ $0FC6   ; Free tempory string, return string descriptor address in HL
FRETM2      equ $0FCC   ; Free up tem in FACLO
LEN1:       equ $0FF7   ; Get string length (in: (FPREG) = string block, out: HL = string block, A = length)
FINBCK      equ $101D   ; Return a String
PTRGET:     equ $10D1   ; Get variable (out: BC = addr, DE = len)
FIN         equ $15E5   ; Parse floating point number
LINPRT      equ $1675   ; Print Two Byte Number in HL as Decimal - Alters all Registers
OUTDO       equ $198A   ; Output a Character
CRDO:       equ $19EA   ; Print CR+LF
WRMCON:     equ $1A40
BITTAB      equ $1ACA   ; Semigraphic Pixel Index to Bit Mask Table
GETINT      equ $1AD0   ; Parse an Integer
TTYCHR:     equ $1D72   ; Print character in A with pause/break at end of page
TTYOUT:     equ $1D94   ; Print character in A
TTYFIS:     equ $1DE7   ; Save Column and Position
FLOATR      equ $14FB   ; Float signed number in B,A,D,E
JMPINI:     equ $1FE1   ; Power Up/Reset Routine: Jumped to from RST 0
PRNTIT:     equ $1FF2   ; Print copyright message and return

; Extra variables / functions
INT2STR:    equ $1679   ; Convert 16 bit integer in HL to text at FPSTR (starts with ' ')
FPSTR:      equ $38E9   ; Floating point string

XINIT:      equ $E010   ; Cartridge entry point
RESET:      equ $0089

; Standard BASIC Tokens
PLUSTK      equ $A8

;Adddress of Byte following Hook RST
HOOK0       = $03DF
HOOK1       = $03E7
HOOK2       = $0403
HOOK3       = $0430
HOOK4       = $0481
HOOK5       = $0486
HOOK6       = $07BD
HOOK7       = $0867
HOOK8       = $0881
HOOK9       = $09FE
HOOK10      = $0537
HOOK11      = $0CCE
HOOK12      = $0BBF
HOOK13      = $198B
HOOK14      = $1986
HOOK15      = $0B3C
HOOK16      = $0B41
HOOK17      = $1AE9
HOOK18      = $1E7F
HOOK19      = $1D73
HOOK20      = $1C2D
HOOK21      = $1C09
HOOK22      = $05A1
HOOK23      = $0659
HOOK24      = $06BF
HOOK25      = $0781
HOOK26      = $0894
HOOK27      = $0A60
HOOK28      = $08F1
HOOK29      = $09A3

;-----------------------------------------------------------------------------
; IO registers
;-----------------------------------------------------------------------------
IO_VCTRL:       equ $E0
IO_VSCRX_L:     equ $E1
IO_VSCRX_H:     equ $E2
IO_VSCRY:       equ $E3
IO_VSPRSEL:     equ $E4
IO_VSPRX_L:     equ $E5
IO_VSPRX_H:     equ $E6
IO_VSPRY:       equ $E7
IO_VSPRIDX:     equ $E8
IO_VSPRATTR:    equ $E9
IO_VPALSEL:     equ $EA
IO_VPALDATA:    equ $EB
IO_VLINE:       equ $EC
IO_VIRQLINE:    equ $ED
IO_IRQMASK:     equ $EE
IO_IRQSTAT:     equ $EF
IO_BANK0:       equ $F0
IO_BANK1:       equ $F1
IO_BANK2:       equ $F2
IO_BANK3:       equ $F3
IO_ESPCTRL:     equ $F4
IO_ESPDATA:     equ $F5
IO_PSG1DATA:    equ $F6
IO_PSG1ADDR:    equ $F7
IO_PSG2DATA:    equ $F8
IO_PSG2ADDR:    equ $F9
IO_SYSCTRL:     equ $FB
IO_CASSETTE:    equ $FC
IO_CPM:         equ $FD     ; Write-only
IO_VSYNC:       equ $FD     ; Read-only
IO_PRINTER:     equ $FE
IO_SCRAMBLE:    equ $FF     ; Write-only
IO_KEYBOARD:    equ $FF     ; Read-only

VCTRL_TEXT_EN:   equ (1<<0)
VCTRL_MODE_OFF:  equ (0<<1)
VCTRL_MODE_TILE: equ (1<<1)
VCTRL_MODE_BM:   equ (2<<1)
VCTRL_SPR_EN:    equ (1<<3)
VCTRL_TEXT_PRIO: equ (1<<4)

;-----------------------------------------------------------------------------
; Other definitions
;-----------------------------------------------------------------------------
BANK0_BASE: equ $0000
BANK1_BASE: equ $4000
BANK2_BASE: equ $8000
BANK3_BASE: equ $C000

BANK_READONLY:  equ $80     ; Bank readonly
BANK_OVERLAY:   equ $40     ; RAM overlay

;-----------------------------------------------------------------------------
; Other definitions
;-----------------------------------------------------------------------------
SYNCHK macro value
    rst     $08
    db      'value'
endm

;-----------------------------------------------------------------------------
; ESP32 commands
;-----------------------------------------------------------------------------
ESPCMD_RESET:      equ $01     ; Reset ESP
ESPCMD_VERSION:    equ $02     ; Get version string
ESPCMD_DATETIME    equ $03     ; Get current date and time
ESPCMD_OPEN:       equ $10     ; Open / create file
ESPCMD_CLOSE:      equ $11     ; Close open file
ESPCMD_READ:       equ $12     ; Read from file
ESPCMD_WRITE:      equ $13     ; Write to file
ESPCMD_SEEK:       equ $14     ; Move read/write pointer
ESPCMD_TELL:       equ $15     ; Get current read/write
ESPCMD_OPENDIR:    equ $16     ; Open directory
ESPCMD_CLOSEDIR:   equ $17     ; Close open directory
ESPCMD_READDIR:    equ $18     ; Read from directory
ESPCMD_DELETE:     equ $19     ; Remove file or directory
ESPCMD_RENAME:     equ $1A     ; Rename / move file or directory
ESPCMD_MKDIR:      equ $1B     ; Create directory
ESPCMD_CHDIR:      equ $1C     ; Change directory
ESPCMD_STAT:       equ $1D     ; Get file status
ESPCMD_GETCWD:     equ $1E     ; Get current working directory
ESPCMD_CLOSEALL:   equ $1F     ; Close any open file/directory descriptor

ERR_NOT_FOUND:     equ -1   ; File / directory not found
ERR_TOO_MANY_OPEN: equ -2   ; Too many open files / directories
ERR_PARAM:         equ -3   ; Invalid parameter
ERR_EOF:           equ -4   ; End of file / directory
ERR_EXISTS:        equ -5   ; File already exists
ERR_OTHER:         equ -6   ; Other error
ERR_NO_DISK:       equ -7   ; No disk
ERR_NOT_EMPTY:     equ -8   ; Not empty

FO_RDONLY:      equ 0x00    ; Open for reading only
FO_WRONLY:      equ 0x01    ; Open for writing only
FO_RDWR:        equ 0x02    ; Open for reading and writing
FO_ACCMODE:     equ 0x03    ; Mask for above modes
FO_APPEND:      equ 0x04    ; Append mode
FO_CREATE:      equ 0x08    ; Create if non-existant
FO_TRUNC:       equ 0x10    ; Truncate to zero length
FO_EXCL:        equ 0x20    ; Error if already exists
