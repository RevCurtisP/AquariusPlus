;-----------------------------------------------------------------------------
; plus.asm - plusBASIC specific statements and functions
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; DATE$ - Get Current Date
; DATETIME$ - Get Current Date & Time
;-----------------------------------------------------------------------------
FN_DATE:
    rst     CHRGET                ; Skip Token
    ld      c,0                   ; Return Just Date
    cp      TIMETK                ; If followed by TIME token
    jr      nz,.notime
    rst     CHRGET                ;   Skip TIME token
    dec     c                     ;   Return Date and Time
.notime    
    SYNCHK  '$'                   ; Require Dollar Sign
    push    hl
    ld      de,LABBCK
    push    de
    ld      ix,esp_get_datetime   ; Read Date and Time into buffer
    call    bas_read_to_buff      ; Set buffer address and call routine
    inc     c                     ; 0 = DATETIME$, 1=DATE$
    jr      z,.done
    ld      de,8
    add     hl,de                 ; Start of Time substring
    xor     a
    ld      (hl),a                ; Terminate Date substring
    sbc     hl,de                 ; Set HL back to Buffer address 
.done
    jp      TIMSTR                ; Create and return temporary string

;-----------------------------------------------------------------------------
; GET functions stub
;-----------------------------------------------------------------------------
FN_GET:
    rst     CHRGET                ; Skip GET Token
    rst     SYNCHR                ; Only GETKEY for now
    byte    KEYTK

;-----------------------------------------------------------------------------
; GETKEY - Wait for key an return ASCII code
; GETKEY$  - Wait for key and return as string
;-----------------------------------------------------------------------------
FN_GETKEY:
    call    CHARCG                ; Wait for keypress
    jr      z,FN_GETKEY
    ld      e,a                   ; Save ASCII Code
    ld      a,(hl)                ; Get character after KEY
    cp      '$'                   ; Check for GETKEY$
    push    af                    ; Save Flags
    call    z,CHRGTR              ; If GETKEY$, skip $
    pop     af                    ; Restore Flags
    push    hl                    ; Do the function stuff
    ld      bc,LABBCK
    push    bc
    ld      a,e                   ; Get ASCII back, Flags are from CP '$'
    jp      nz,SNGFLT             ; If not GETKEY$, Float it
    pop     bc                    ; Get rid of dummy return address
    jp      BUFCIN                ; Else Return String
    
;-----------------------------------------------------------------------------
; GET statements stub
;-----------------------------------------------------------------------------
ST_GET:
    cp      ARGSTK
    rst     SYNCHR
    byte    ARGSTK
    jp      ST_GETARGS

;-----------------------------------------------------------------------------
; INKEY - Return ASCII code of currently pressed key
;-----------------------------------------------------------------------------
FN_INKEY:
    rst     CHRGET                ; Skip KEY Token
    push    hl                    ; Do the function stuff
    ld      bc,LABBCK
    push    bc
    call    CHARCG                ; Get Keypress
    jp      SNGFLT                ; and Float it

;-----------------------------------------------------------------------------
; JOY() function
; syntax: var = JOY(stick)
;    stick - 0 will read left or right
;          - 1 will read left joystick only
;          - 2 will read right joystick only
;
; |        | Data bus |  Binary  | Hex  | Decimal |    
; | Switch | grounded | 76543210 | code |  code   |          
; | ------ | -------- | -------- |      | ------- |    
; |  K1    |  D6      | .1...... |  BF  |   191   |    
; |  K2    |  D7,2    | 1.....1. |  7B  |   123   |    +------------------------+
; |  K3    |  D7,5    | 1..1.... |  5F  |    95   |    |                        |
; |  K4    |  D5      | ..1..... |  DF  |   223   |    |    [K1]  [K2]  [K3]    |
; |  K5    |  D7,1    | 1.....1. |  7D  |   125   |    |                        |
; |  K6    |  D7,0    | 1......1 |  7E  |   126   |    |    [K4]  [K5]  [K6]    |                      
; |  p1    |  D1      | ......1. |  FD  |   253   |    |                        |                      
; |  P2    |  D1,4    | ...1..1. |  ED  |   237   |    |                        |        
; |  P3    |  D1,0,4  | ...1..11 |  EC  |   236   |    |      P12 P13 P14       |     
; |  P4    |  D1,0    | ......11 |  FC  |   252   |    |   P11      |     P15   |  
; |  P5    |  D0      | .......1 |  FE  |   254   |    |       \    |   /       |     
; |  P6    |  D0,4    | ...1...1 |  EE  |   238   |    |   P10  \   |  /  P16   |  
; |  P7    |  D3,0,4  | ...11..1 |  E6  |   230   |    |         \  | /         |       
; |  P8    |  D3,0    | ....1..1 |  F6  |   246   |    |   P9 ------*------ P1  |      
; |  P9    |  D3      | ....1... |  F7  |   247   |    |          / |  \        |      
; |  P10   |  D3,4    | ...11... |  E7  |   231   |    |    P8   /  |   \  P2   |      
; |  P11   |  D3,2,4  | ...111.. |  E3  |   227   |    |        /   |    \      |
; |  P12   |  D3,2    | ....11.. |  F3  |   243   |    |     P7     |      P3   |      
; |  P13   |  D2      | .....1.. |  FB  |   251   |    |        P6  P5  P4      |                       
; |  P14   |  D2,4    | ...1.1.. |  EB  |   235   |    |                        |                       
; |  P15   |  D1,2,4  | ...1.11. |  E9  |   233   |    +------------------------+                       
; |  P16   |  D1,2    | .....11. |  F9  |   249   |                           



;-----------------------------------------------------------------------------
FN_JOY:
    rst     CHRGET            ; Skip Token and Eat Spaces
    call    PARCHK
    push    hl
    ld      bc,LABBCK
    push    bc
    call    FRCINT         ; FRCINT - evalute formula pointed by HL result in DE

    ld      a, e  
    or      a
    jr      nz, .joy01
    ld      a, $03

.joy01:
    ld      e, a
    ld      bc, $00F7
    ld      a, $FF
    bit     0, e
    jr      z, .joy03
    ld      a, $0e
    out     (c), a
    dec     c
    ld      b, $FF

.joy02:
    in      a,(c)
    djnz    .joy02
    cp      $FF
    jr      nz, .joy05

.joy03:
    bit     1,e
    jr      z, .joy05
    ld      bc, $00F7
    ld      a, $0F
    out     (c), a
    dec     c
    ld      b, $FF

.joy04:
    in      a, (c)
    djnz    .joy04

.joy05:
    cpl
    jp      SNGFLT

;-----------------------------------------------------------------------------
; PSG statement
; syntax: PSG register, value [, ... ]
;-----------------------------------------------------------------------------
ST_PSG:
    cp      $00
    jp      z, MOERR         ; MO error if no args

.psgloop:
    ; Get PSG register to write to
    call    GETBYT           ; Get/evaluate register
    cp      16
    jr      nc, .psg2

    out     (IO_PSG1ADDR), a ; Set the PSG register

    ; Expect comma
    SYNCHK  ','

    ; Get value to write to PSG register
    call    GETBYT           ; Get/evaluate value
    out     (IO_PSG1DATA), a ; Send data to the selected PSG register

.check_comma:
    ; Check for a comma
    ld      a, (hl)          ; Get next character on command line
    cp      ','              ; Compare with ','
    ret     nz               ; No comma = no more parameters -> return

    inc     hl               ; next character on command line
    jr      .psgloop         ; parse next register & value

.psg2:
    sub     16
    out     (IO_PSG2ADDR), a ; Set the PSG register

    ; Expect comma
    SYNCHK  ','

    ; Get value to write to PSG register
    call    GETBYT           ; Get/evaluate value
    out     (IO_PSG2DATA), a ; Send data to the selected PSG register

    jr      .check_comma

;-----------------------------------------------------------------------------
; TIME$ - Get Current Time
;-----------------------------------------------------------------------------
FN_TIME:
    rst     CHRGET                ; Skip Token
    SYNCHK  '$'                   ; Require Dollar Sign
    push    hl
    ld      bc,LABBCK
    push    bc
    ld      ix,esp_get_datetime   ; Read Date and Time into buffer
    call    bas_read_to_buff      ; Set buffer address and call routine
    ld      bc,8
    add     hl,bc                 ; Start of Date String
    jp      TIMSTR

;----------------------------------------------------------------------------
; EVAL - Evaluate string
;----------------------------------------------------------------------------
FN_EVAL:
    call    ERRDIR            ; Issue Error if in Direct Mode
    rst     CHRGET            ; Skip Token
    call    PARCHK            ; Get Argument
    push    hl                ; Save Text Pointer
    call    FRESTR            ; Free up temporary and get descriptor address
    call    string_addr_len   ; Get Argument String Length in BC, Address in HL
    ld      a,ENDBUF-BUF      ;
    cp      c
    jr      c,LSERR           ; Error if longer than 127 bytes
    ex      de,hl             ; Text address into HL
    ld      de,BUF            ;
    ldir                      ; Copy String to Buffer
    xor     a
    ld      (de),a            ; Terminate String
    ld      hl,BUF            ; Reading Line Buffer
    ld      d,h               ; Writing Line Buffers
    ld      e,l 
    xor     a                 ; Tokenize String
    ld      (DORES),a         ; 
    ld      c,5               ; 
    call    KLOOP             ; 
    ld      hl,BUF            ; Point to Line Buffer
    call    FRMEVL            ; Evaluate Formula
    pop     hl                ; Restore Text Pointer
    ret

LSERR:
    ld      e,ERRLS
    jp      ERROR
    
   
;-----------------------------------------------------------------------------
; SET Statement stub
;-----------------------------------------------------------------------------
ST_SET:
    cp      TILETK
    jp      z,SET_TILE
    cp      COLTK
    jr      nz,.notcol
    rst     CHRGET                ; Skip COL token            
    call    SYNCHR                ; Require OR after COL
    byte    ORTK                  
    jp      st_set_palette
.notcol
    jp      SNERR

