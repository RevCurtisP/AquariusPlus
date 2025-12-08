;===========================================================================
; Error messages, lookup table, and lookup routines
;===========================================================================

; S3BASIC errors
ERRNF   equ     $00   ;  1 NF NEXT without FOR
ERRSN   equ     $02   ;  2 SN Syntax error
ERRRG   equ     $04   ;  3 RG RETURN without GOSUB
ERROD   equ     $06   ;  4 OD Out of DATA
ERRFC   equ     $08   ;  5 FC Function Call error
ERROV   equ     $0A   ;  6 OV Overflow
ERROM   equ     $0C   ;  7 OM Out of Memory
ERRUS   equ     $0E   ;  8 UL Undefined Line number (Undefined Statement)
ERRBS   equ     $10   ;  9 BS Bad Subscript
ERRDD   equ     $12   ; 10 DD Re-DIMensioned array (Duplicate Definition)
ERRDZ   equ     $14   ; 11 /0 Division by Zero 
ERRID   equ     $16   ; 12 ID Illegal direct
ERRTM   equ     $18   ; 13 TM Type mismatch
ERROS   equ     $1A   ; 14 OS Out of String space
ERRLS   equ     $1C   ; 15 LS String too Long
ERRST   equ     $1E   ; 16 WT String formula too complex
ERRCN   equ     $20   ; 17 CN Cant CONTinue
ERRUF   equ     $22   ; 18 UF UnDEFined FN function
ERRMO   equ     $24   ; 19 MO Missing operand
; plusBASIC errors        
ERRNR   equ     $26   ; 20 No RESUME             
ERRRE   equ     $28   ; 21 RESUME without error
ERRUE   equ     $2A   ; 22 Unprintable error
ERRLBO  equ     $2C   ; 23 Line buffer overflow
ERRGS   equ     $2E   ; 24 Statement not implemented
ERRUL   equ     $30   ; 25 Undefined label
ERRAG   equ     $32   ; 26 ARG without GOSUB
ERRUD   equ     $34   ; 27 Undimensioned array
ERRTO   equ     $36   ; 28 Too many operands
ERRIM   equ     $38   ; 29 Invalid mode
ERRBR   equ     $3A   ; 30 Bad range
ERRES   equ     $3C   ; 31 Empty string
ERRSL   equ     $3E   ; 32 String length
NONDSK  equ     $40   ; 33 Last non disk error
; Disk errors
DSKERRS equ     $60   ; Start of Disk Errors
ERRBDF  equ     $60   ; 49 Bad File
ERRFNF  equ     $62   ; 50 File / directory not found         
ERRTMF  equ     $64   ; 51 Too many open files / directories  
ERRIPR  equ     $66   ; 52 Invalid parameter                
ERRRPE  equ     $68   ; 53 End of file / directory            
ERRFAE  equ     $6A   ; 54 File already exists                
ERRIOE  equ     $6C   ; 55 Other error                        
ERRNOD  equ     $6E   ; 56 No disk                            
ERRNEM  equ     $70   ; 57 Not empty                          
LSTERR  equ     $72   ; 58 Last error used for range checks

;===========================================================================
; The error routines fit in the space between the end of the dispatch 
; routines and the beginning of the error lookup table
;===========================================================================

;----------------------------------------------------------------------------
; Print error message and return to direct mode
;----------------------------------------------------------------------------
force_error:
    call    CRDONZ                ; Print CR/LF if TTYPOS is not Zero
    ld      a,7                   ; Ring the bell
    rst     OUTCHR
    call    get_errmsg_ptr        ; Get Pointer into Error Table
    call    STROUT
    ld      hl,ERRTXT
    jp      ERRFN1

; -------------------------------------------------------------------------------
;  Error Message Lookup Routines`
; ------------------------------------------------------------------------------
get_errno_ptr:
    or      a
    jr      z,ret_null
    dec     a                     ; Convert to Error# to offset
    sla     a     
    ld      e,a                   ; Put in E
get_errcode_ptr:
    ld      a,e                   ; Get Error Table Offset into A
    cp      NONDSK                ; If Offset < NONDSK
    jr      c,.load_ptr           ;   Display it
    cp      DSKERRS               ; Else if Offset < DSKERRS
    jr      c,.ue_err             ;   Display Unprintable
    cp      LSTERR                ; Else if Offser < LSTERR
    jr      c,.dos_err            ;   Display DOS error
.ue_err
    ld      a,ERRUE               ; Display "UE" - Unprintable Error
    byte    $01                   ; LD BC, over SUB
.dos_err
    sub     DSKERRS-NONDSK        ; Else Adjust
.load_ptr
    ld      l,a                   ; Table Starts at page boundary
    ld      h,high(err_codes)     ; Put address in HL
    ret
ret_null:
    ld      hl,REDDY-1
    ret

get_errno_msg:
    call    get_errno_ptr
    jr      _errmag_ptr

; Get Pointer to Long Error Message
; E = Offset into Error Table - 0=NF, 2=SN, etc.
get_errmsg_ptr:
    call    get_errcode_ptr       ; Get Pointer to Error Code for E
_errmag_ptr:
    ld      a,(hl)                ; Read Address from Error Message Table
    inc     hl
    ld      h,(hl)
    ld      l,a
    ret


; The word error
ERRTXT: byte    " error",0

; Long Error Descriptions
err_messages:
MSGNE:  byte    "",0                            
MSGNF:  byte    "NEXT without FOR",0            ; 1
MSGSN:  byte    "Syntax",0                      ; 2
MSGRG:  byte    "RETURN without GOSUB",0        ; 3
MSGOD:  byte    "Out of DATA",0                 ; 4
MSGFC:  byte    "Illegal Quantity",0            ; 5
MSGOV:  byte    "Overflow",0                    ; 6
MSGOM:  byte    "Out of memory",0               ; 7
MSGUS:  byte    "Undefined line number",0       ; 8
MSGBS:  byte    "Subscript out of range",0      ; 9
MSGDD:  byte    "Duplicate Definition",0        ; 10
MSGDV0: byte    "Division by zero",0            ; 11
MSGID:  byte    "Illegal direct",0              ; 12
MSGTM:  byte    "Type mismatch",0               ; 13
MSGSO:  byte    "Out of string space",0         ; 14
MSGLS:  byte    "String too long",0             ; 15
MSGST:  byte    "String formula too complex",0  ; 16
MSGCN:  byte    "Can't continue",0              ; 17
MSGUF:  byte    "Undefined user function",0     ; 18
MSGMO:  byte    "Missing operand",0             ; 19
; Errors from CP/M BASIC                        
MSGNR:  byte    "No RESUME",0                   ; 20
MSGRE:  byte    "RESUME without GOSUB",0        ; 21
MSGUE:  byte    "Unprintable",0                 ; 22
MSGLBO: byte    "Line buffer overflow",0        ; 23      
MSGGS:  byte    "Statement not implemented",0   ; 24
MSGUL:  byte    "Undefined line label",0        ; 25
MSGAG:  byte    "ARGS without GOSUB",0          ; 26
; plusBASIC errors
MSGUD:  byte    "Undimensioned Array",0         ; 27 
MSGTO:  byte    "Too many operands",0           ; 28
MSGIM:  byte    "Invalid mode",0                ; 29
MSGBR:  byte    "Bad range",0                   ; 30
MSGES:  byte    "Empty string",0                ; 31
MSGSL:  byte    "String length",0               ; 32
        byte    0                               ; 33  Last non disk error                             

; File System Errors                            ;     ESP32 Error
doserr_messages:
MSGBDF: byte    "Bad file",0                    ; 49  Bad File
MSGFNF: byte    "File not found",0              ; 50  -1: File / directory not found         
MSGTMF: byte    "Too many files",0              ; 51  -2: Too many open files / directories  
MSGIPR: byte    "Invalid parameter",0           ; 52  -3: Invalid parameter                
MSGRPE: byte    "Input past end",0              ; 53  -4: End of file / directory            
MSGFAE: byte    "File already exists",0         ; 54  -5: File already exists                
MSGIOE: byte    "I/O",0                         ; 55  -6: Other error
MSGNOD: byte    "No disk",0                     ; 56  -7: No disk                             
MSGNEM: byte    "Not empty",0                   ; 57  -8: Not empty                           
        byte    0                               ; 58  Last error used for range checks

