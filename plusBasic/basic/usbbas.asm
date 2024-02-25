;====================================================================
; Statements and Functions from USB BASIC
;====================================================================


;-----------------------------------------------------------------------------
; ST_CALL
;
; syntax: CALL address
; address is signed integer, 0 to 32767   = $0000-$7FFF
;                            -32768 to -1 = $8000-$FFFF
;
; on entry to user code, HL = text after address
; on exit from user code, HL should point to end of statement
;-----------------------------------------------------------------------------
ST_CALL:
    call    GETINT                ; Convert to 16 bit integer
    push    de                    ; Stack = CalAdr, RtnAdr
    ld      a,(hl)      
    cp      XTOKEN                ; If no Extended token
    ret     nz                    ;   Jump to user code, HL = BASIC text pointer
    inc     hl
    ld      a,(hl)                ; Get extended token
    cp      ARGSTK                
    jr      z,.call_args          ; If not ARGS
    dec     hl                    ;   Back up pointer
    ret     z                     ;   and Jump to user code
    
.call_args
    call    .get_arg              ; Stack = ArgHL, CalAdr, RtnAdr
    jr      nz,.arg_hl
    call    .get_arg              ; Stack = ArgDE, ArgHL, CalAdr, RtnAdr
    jr      nz,.arg_de
    call    .get_arg              ; Stack = ArgBC, ArgDE, ArgHL, CalAdr, RtnAdr
    jr      nz,.arg_bc
    call    .get_arg              ; Stack = ArgAF, ArgBC, ArgDE, ArgHL, CalAdr, RtnAdr
.arg_af:
    pop     de                    ; DE = ArgAF; Stack = ArgAF, ArgHL, ArgHL, CalAdr, RtnAdr
    ld      a,e                   ; A = ArgA
.arg_bc:                                        
    pop     bc                    ; BC = ArgBC; Stack = ArgDE, ArgHL, CalAdr, RtnAdr
.arg_de:                                        
    pop     de                    ; DE = ArgDE; Stack = ArgHL, CalAdr, RtnAdr
.arg_hl                              
    exx
    pop     hl                    ; HL' = ArgHL; Stack = CalAdr, RtnAdr
    pop     ix                    ; IX = CalAdr; Stack = RtnAdr
    push    hl                    ; Stack = ArgHL, RtnAdr
    exx
    ex      (sp),hl               ; HL = ArgHL; Stack = TxtPtr, RtnAdr
    call    jumpix                ; Call the routine
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret

.get_arg
    rst     CHRGET                ; Skip comma
    call    GETINT                ; Get Arg
    pop     ix                    ; IX = RtnAdr
    push    de                    ; Arg onto staclk
    ld      a,(hl)
    cp      ','                   ; Check for comma
    jp      (ix)                  ; and Return
    

;-----------------------------------------------------------------------------
; HEX$() function
; eg. A$=HEX$(B)
;-----------------------------------------------------------------------------
FN_HEX:
    rst     CHRGET            ; Skip Token and Eat Spaces
    call    PARCHK
    call    GETYPE          ; Get Type of Argument
    jr      z,HEX_STRING    ; If String, Convert It and Return
    push    hl              ; Stack = TxtPtr
    push    bc              ; Stack = DummyRtn, TxtPtr
    call    FRCINT          ; Evaluate formula @HL, result in DE
    ld      hl, FPSTR       ; HL = temp string
    ld      a, d
    or      a               ; > zero ?
    jr      z, .lower_byte
    ld      a, d
    call    byte_to_hex     ; Yes, convert byte in D to hex string
.lower_byte:
    ld      a, e
    call    byte_to_hex     ; Convert byte in E to hex string
    ld      (hl), 0         ; Null-terminate string
    ld      hl, FPSTR
.create_string:
    jp      TIMSTR                ; Create BASIC string

HEX_STRING:
    push    hl              ; Stack = TxtPtr
    push    bc              ; Stack = DummyRtn, TxtPtr
    call    free_addr_len   ; BC = StrLen, DE = StrAdr, HL = StrDsc
    ex      de,hl           ; HL = StrAdr
    ld      a,c             ; A = ArgLen
    or      a               ; If Length is 0
    jr      z,_null_string  ;   Return Null String
    push    hl              ; Stack=Arg Text Address
    push    af              ; Stack=Arg Length, Arg Text Address
    add     a,a             ; New String will be Twice as long
    jp      c,LSERR         ; LS Error if greater than 255
    call    STRINI          ; Create Result String returning HL=Descriptor, DE=Text Address
    pop     af              ; A=Arg Length, Stack=Arg Text Address
    pop     hl              ; HL=Arg Text Address
    ex      de,hl           ; DE=Arg Text Address, HL=Result Text Address
    ld      b,a             ; Loop through Arg String Text 
.hexloop:
    ld      a,(de)          ; Get Arg String Character
    inc     de              ; and Bump Pointer
    call    _hexbyte        ; Convert to Hex in Result String
    djnz    .hexloop        ; Loop until B=0
    jp      FINBCK          ; Return Result String

_hexbyte:
    ld      c,a
    rra
    rra
    rra
    rra
    call    .hex
    ld      a,c
.hex:
    and     $0f
    cp      10
    jr      c,.chr
    add     7
.chr:
    add     '0'
    ld      (hl),a
    inc     hl
    ret

_null_string:
    ld      hl,REDDY-1    ; Point at null terminator
    jp      TIMSTR        ; and return null string


;-----------------------------------------------------------------------------
; IN() function
; syntax: var = IN(port)
;-----------------------------------------------------------------------------
FN_IN:
    rst     CHRGET                ; Skip IN and Eat Spaces
    cp      XTOKEN                ; If followed by extended token
    jr      z,.extended           ;   Handle it
    call    PARCHK
    push    hl
    ld      bc,LABBCK
    push    bc
    call    FRCINT           ; Evaluate formula pointed by HL, result in DE
    ld      b, d
    ld      c, e             ; BC = port

    ; Read from port
    in      a, (c)           ; A = in(port)
    jp      SNGFLT           ; Return with 8 bit input value in variable var


.extended
    rst     CHRGET                ; Skip XTOKEN
    sub     KEYTK                 ; $86 KEY
    jp      z,FN_INKEY            ; 
    dec     a                     ; $87 DEX
    jp      z,FN_INDEX
    sub     STRTK-DEXTK           ; $9C STR
    jp      z,FN_INSTR            ;
    jp      SNERR

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
; |  P1    |  D1      | ......1. |  FD  |   253   |    |                        |                      
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
; LOCATE statement
; Syntax: LOCATE col, row
;-----------------------------------------------------------------------------
ST_LOCATE:
    call    GETBYT              ; Read number from command line (column). Stored in A and E
    push    af                  ; Column store on stack for later use
    dec     e
    ld      a,(LINLEN)
    dec     a
    dec     a                   ; A = 38 or 78
    cp      e                   ; If less than screen position
    jp      c, FCERR            ; If w, Illegal Quantity

    ; Expect comma
    SYNCHK  ','

    call    GETBYT              ; Read number from command line (row). Stored in A and E
    cp      24                  ; Compare with 24 decimal (max rows on screen)
    jp      nc,FCERR            ; If higher then 24 goto FC error

    inc     e
    pop     af                  ; Restore column from store
    ld      d, a                ; Column in register D, row in register E
    ex      de, hl              ; Switch DE with HL
    call    move_cursor         ; Cursor to screen location HL (H=col, L=row)
    ex      de, hl
    ret

;-----------------------------------------------------------------------------
; OUT statement
; syntax: OUT port, data
;-----------------------------------------------------------------------------
ST_OUT:
    call    FRMNUM              ; Get/evaluate port
    call    FRCINT              ; Convert number to 16 bit integer (result in DE)
    push    de                  ; Stored to be used in BC

    ; Expect comma
    SYNCHK  ","

    call    GETBYT              ; Get/evaluate data
    pop     bc                  ; BC = port
    out     (c), a              ; Out data to port
    ret

;-----------------------------------------------------------------------------
; PSG statement
; syntax: PSG register, value [, ... ]
;-----------------------------------------------------------------------------
;;ToDo:
;; DEF PSGLIST P$ = register,value;register,value...
;; PSG P$
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


