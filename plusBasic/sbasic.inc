;-----------------------------------------------------------------------------
; sbasic.inc
;
; This file contains locations of SBASIC3 systen variables and ROM calls
;
; Locations to the original Aquarius ROM are taken from the dissassembly by
; Curtis F Kaylor, which can be found at:
; https://github.com/RevCurtisP/Aquarius/blob/main/disassembly/aquarius-rom.lst
;-----------------------------------------------------------------------------
; sbasic.inc

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
RNDTAB      equ $3821   ; RND Permutation Table - Deprecated in S3BASIC
                        ; Used for plusBASIC system variables (see plus.inc)
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
TEMP3:      equ $38C3   ; [M80] USED MOMENTARILY BY FRMEVL. USED IN EXTENDED BY FOUT AND
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
STREND      equ $38DA   ; [M80] END OF STORAGE IN USE. INCREASED WHENEVER A NEW ARRAY/VAR IS ADDED
TEMP2:      equ $38D0   ; [M80] FORMULA EVALUATOR TEMP MUST BE PRESERVED BY OPERATORS
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
FPSTR:      equ $38E9   ; Floating point string
RESHO:      equ $38F6   ; [M65] RESULT OF MULTIPLIER AND DIVIDER
RESMO:      equ $38F7   ; RESMO and RESLO are loaded into and stored from HL
RESLO:      equ $38F8   ;
SAVSTK:     equ $38F9   ; [M80] NEWSTT SAVES STACK HERE BEFORE SO THAT ERROR REVERY CAN
                        ; [M80] CAN DATPTR THE STACK WHEN AN ERROR OCCURS
INTJMP  equ     $38FB   ; RST 7 Interrupt JMP (3 bytes)
KEYADR  equ     $38FE   ;;Extended Key Tables Base Address minus 1
;               $3900   ; This is always 0
BASTXT:     equ $3901   ; Start of Basic Program

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

RESET:      equ $0089
INITFF:     equ $0153   ; Initialize I/O Port 255
REDDY       equ $036E   ; Text "Ok"
FNDFOR      equ $039F   ; Find FOR/GOSUB entry on stack
SNERR:      equ $03C4   ; Syntax error
DV0ERR:     equ $03C7   ; Division by zero error
NFERR:      equ $03CA   ; Next without for error
DDERR:      equ $03CD   ; redimensioned array error
UFERR:      equ $03D0   ; Undefined function error
OVERR:      equ $03D3   ; Overflow Error
MOERR:      equ $03D6   ; Missing Operand error
TMERR:      equ $03D9   ; Type mismatch error
ERROR:      equ $03DB   ; Process error code, E = code (offset to 2 char error name)
ERRCRD      equ $03E0   ; Print Error Message and Return to Immediate Mode
ERRPRT      equ $03EA   ; Print Error pointed to by HL and Return to Immediate Mode
ERRFN1:     equ $03F4   ; Print String in HL + " in " + line number, Return to Immediate mode 
STPRDY      equ $0401   ; For LIST command stopping
READY:      equ $0402
MAIN        equ $0414   ; Immediate Mode loop     
CHEAD       equ $0489   ; Link Lines
FNDLIN      equ $049F   ; Find BASIC line wirh line number in [DE]
KLOOP       equ $04C5   ; Keyword to token loop
CRUNCX      equ $04F9   ; Tokenize text at [HL]
RESSRC:     equ $05A8
NEWSTT:     equ $062C
CHRGTR      equ $066B   ; CALLable CHRGET
CHRGT2      equ $066C   ; Get Current Character and Set Flags
CHRGT3      equ $066D   ; Test character in A and set flags
GONE4       equ $063C   ; Execute next line
FRCINT:     equ $0682   ; Convert fp number to 16 bit signed integer in DE
RETURN      equ $06F8   ; Execute RETURN statement
FCERR:      equ $0697   ; Function call error
SCNLIN      equ $069C   ; Get line number from text at (HL)
IQERR       equ FCERR   ; Illegal quantity error (future expansion)
RUNC2:      equ $06DB
GOTO        equ $06DC   ; Execute GOTO Statement
GOTOLN      equ $06DF   ; GOTO line# in DE
USERR       equ $06F3   ; Undefined line error
LETDO       equ $0736   ; Execute LET, bypassing PTRGET and SYNCHK '='
INBUFC      equ $0768   ; Copy temporary string to string variable
NTOERR      equ $0782   ; Execute ON ... GOTO
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
ERRDIR      equ $0B45   ; Error if in direct mode
GETBYT:     equ $0B54   ; Evaluate numeric expression (integer 0-255)
CONINT      equ $0B57   ; Convert FAC to byte
OMERR:      equ $0BB7   ; Out of memory error
SCRTCH:     equ $0BBE
RUNC:       equ $0BCB
CLEARC:     equ $0BCF
STKINI:     equ $0BE5
;FRMINT         $0B7F   ; Deprecated in S3BASIC, use GETINT instead
BGNRST      equ $0C19   ; Finish up RESTORE
ISLETC:     equ $0CC6   ; Test for a letter
TIMSTR      equ $0E2F   ; Create tempory string from null-terminated string
STRINI      equ $0E50   ; Reserve space for new string and create descriptor
STRLIT      equ $0E5F   ; Build descriptor for string litersl
STRLTI      equ $0E60   ; Do STRLIT, without decrementing HL
PUTNEW      equ $0E7E   ; Set Pointer to Temporary String
STROUT:     equ $0E9D   ; Print null-terminated string in HL
FRESTR      equ $0FC6   ; Free tempory string, return string descriptor address in HL
FREFAC      equ $0FC9   ; Free tempory string with descriptor in FAC
FRETM2      equ $0FCC   ; Free up temporary in FACLO
FRETMP      equ $0FCD   ; Free up temporary
FRETMS      equ $0FE4   ; Free up temporary descriptor
LEN1:       equ $0FF7   ; Get string length (in: (FPREG) = string block, out: HL = string block, A = length)
ASC2        equ $1006   ; Return DE = Pointer to String Text, A=First Character 
CHR         equ $1013   ; CHR$ Function
FINBCK      equ $101D   ; Return a String
PTRGET:     equ $10D1   ; Get variable (out: BC = addr, DE = len)
FIN         equ $15E5   ; Parse floating point number
FIN0        equ $15EE   ; Parse floating point number wihout leading + or -
LINPRT      equ $1675   ; Print Two Byte Number in HL as Decimal - Alters all Registers
INT2STR:    equ $1679   ; Convert 16 bit integer in HL to text at FPSTR (starts with ' ')
FOUT        equ $1680   ; Convert float in FAC to text
OUTDO       equ $198A   ; Output a Character
CRDO:       equ $19EA   ; Print CR+LF
BUFCIN      equ $1A02   ; Return A as string from function 
NULRT       equ $1A0B   ; Return null string
CHARCG      equ $1A18   ; Get keypress, check for Ctrl-C
WRMCON:     equ $1A40
BITTAB      equ $1ACA   ; Semigraphic Pixel Index to Bit Mask Table
GETINT      equ $1AD0   ; Parse an Integer
COPY        equ $1B15   ; Execute COPY statement
TTYCHR:     equ $1D72   ; Print character in A with pause/break at end of page
TTYOUT:     equ $1D94   ; Print character in A
TTYFIS:     equ $1DE7   ; Save Column and Position
FLOATR      equ $14FB   ; Float signed number in B,A,D,E
JMPINI:     equ $1FE1   ; Power Up/Reset Routine: Jumped to from RST 0
PRNTIT:     equ $1FF2   ; Print copyright message and return

; Extra variables / functions

; plusBASIC system jump points
SCNLBL  equ     $200F   ;; | Line label hook for GOTO, GOSUB, and RESTORE

XINIT:      equ $E010   ; Cartridge entry point

; Standard BASIC Tokens
GOTOTK      equ $88     ; GOTO
GOSUTK      equ $8C     ; GOSUB
ONTK        equ $90     ; ON
DEFTK       equ $93     ; DEF
LISTTK      equ $97     ; LIST
PLUSTK      equ $A8     ; +
RETTK       equ $8D     ; RETURN
TOTK        equ $A1     ; TO
ORTK        equ $AE     ; OR
EQUATK      equ $B0     ; = 
INTTK       equ $B3     ; INT
POSTK       equ $B8     ; POS

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

