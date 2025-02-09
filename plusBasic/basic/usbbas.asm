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
    push    hl                    ; Stack = TxtPtr
    push    bc                    ; Stack = DmyRtn, TxtPtr
    call    faclo_addr_len        ; BC = ArgLen, DE = ArgAdr, HL = ArgDsc
    ex      de,hl                 ; DE = ArgDsc, HL = ArgAdr
    ld      a,c                   ; A = ArgLen
    or      a                     ; If Length is 0
    jr      z,_null_string        ;   Return Null String
    push    de                    ; Stack = ArgDsc, DmyRtn, TxtPtr
    push    hl                    ; Stack = ArgAdr, ArgDsc, DmyRtn, TxtPtr
    push    af                    ; Stack = ArgLen, ArgAdr, ArgDsc, DmyRtn, TxtPtr
    add     a,a                   ; NewLen = ArgLen * 2
    jp      c,LSERR               ; LS Error if greater than 255
    call    STRINI                ; DE = NewAdr
    pop     af                    ; A = ArgLen; Stack = ArgAdr, ArgDsc, DmyRtn, TxtPtr
    pop     hl                    ; HL = ArgAdr; Stack = ArgDsc, DmyRtn, TxtPtr
    ex      de,hl                 ; DE = ArgAdr, HL = NewAdr
    ld      b,a                   ; Loop through Arg String Text
.hexloop:
    ld      a,(de)                ; Get Arg String Character
    inc     de                    ; and Bump Pointer
    call    _hexbyte              ; Convert to Hex in Result String
    djnz    .hexloop              ; Loop until B=0
    pop     de                    ; DE = ArgDsc; Stack = DmyRtn, TxtPtr
    call    FRETMP                ; Free ArgStr
    jp      FINBCK                ; Return Result String

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
;         var$ = IN$(port, len)
;-----------------------------------------------------------------------------
FN_IN:
    rst     CHRGET                ; Skip IN and Eat Spaces
    cp      '$'                   ; If followed by dollar sign
    jr      z,.instring          ;   Do IN$()
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

.instring
    rst     CHRGET                ; Skip $
    call    FRMPRN                ; Evaluate formula following (
    call    force_integer         ; DE = IOPort
    push    de                    ; Stack = IOPort, RtnAdr
    call    get_comma_byte        ; DE = StrLen
    SYNCHK  ')'                   ; Require )
    ex      (sp),hl               ; HL = IOPort; Stack = TxtPtr, RtnAdr
    push    hl                    ; Stack = IOPort, TxtPtr, RtnAdr
    ld      a,e
    or      a                     ; If SrtLen = 0
    jp      z,FCERR               ;   Illegal quantity error
    call    STRINI                ; HL = StrDsc, DE = StrAdr, A = StrLen
    pop     bc                    ; BC = IOPort; Stack = TxtPtr, RtnAdr
    push    hl                    ; Stack = StrDsc, TxtPtr, RtnAdr
    ld      l,a                   ; L = SrtLen
.loop
    in      a,(c)
    ld      (de),a
    inc     de
    dec     l
    jr      nz,.loop
    pop     hl                    ; HL = StrDsc; Stack = TxtPtr, RtnAdr
    jp      PUTNEW

.extended
    rst     CHRGET                ; Skip XTOKEN
    sub     KEYTK                 ; $86 KEY
    jp      z,FN_INKEY            ;
    dec     a                     ; $87 DEX
    jp      z,FN_INDEX
    sub     STRTK-DEXTK           ; $9C STR
    jp      z,FN_INSTR            ;
    sub     MEMTK-STRTK           ; $A7 MEM
    jp      z,FN_INMEM            ;
    jp      SNERR

;-----------------------------------------------------------------------------
; JOY() function
; syntax: var = JOY(stick)
;    stick - 0 will read left or right
;          - 1 will read left joystick only
;          - 2 will read right joystick only
;
; |        | Data bus |  Binary  | Hex  | Decimal | JOY  |  JOY()  |
; | Switch | grounded | 76543210 | code |  code   | Hex  | Decimal |
; | ------ | -------- | -------- | ---- | ------- | ---- | ------- |
; |  K1    |  D6      | .1...... |  BF  |   191   |  40  |    64   |
; |  K2    |  D7,2    | 1....1.. |  7B  |   123   |  84  |   132   |    +------------------------+   +-----------------------+
; |  K3    |  D7,5    | 1.1..... |  5F  |    95   |  A0  |   160   |    |                        |   |                       |
; |  K4    |  D5      | ..1..... |  DF  |   223   |  20  |    32   |    |    [K1]  [K2]  [K3]    |   |  [ 64]  [132]  [160]  |
; |  K5    |  D7,1    | 1.....1. |  7D  |   125   |  82  |   130   |    |                        |   |                       |
; |  K6    |  D7,0    | 1......1 |  7E  |   126   |  81  |   129   |    |    [K4]  [K5]  [K6]    |   |  [ 32]  [130]  [129]  |
; |  P1    |  D1      | ......1. |  FD  |   253   |  02  |     2   |    |                        |   |                       |
; |  P2    |  D1,4    | ...1..1. |  ED  |   237   |  12  |    18   |    |                        |   |                       |
; |  P3    |  D1,0,4  | ...1..11 |  EC  |   236   |  13  |    19   |    |      P12 P13 P14       |   |       12  4  20       |
; |  P4    |  D1,0    | ......11 |  FC  |   252   |  03  |     3   |    |   P11      |     P15   |   |    28     |     22    |
; |  P5    |  D0      | .......1 |  FE  |   254   |  01  |     1   |    |       \    |   /       |   |       \   |   /       |
; |  P6    |  D0,4    | ...1...1 |  EE  |   238   |  11  |    17   |    |   P10  \   |  /  P16   |   |  24    \  |  /    6   |
; |  P7    |  D3,0,4  | ...11..1 |  E6  |   230   |  19  |    25   |    |         \  | /         |   |         \ | /         |
; |  P8    |  D3,0    | ....1..1 |  F6  |   246   |  09  |     9   |    |   P9 ------*------ P1  |   |  8 -------*------- 2  |
; |  P9    |  D3      | ....1... |  F7  |   247   |  08  |     8   |    |          / |  \        |   |         / | \         |
; |  P10   |  D3,4    | ...11... |  E7  |   231   |  18  |    24   |    |    P8   /  |   \  P2   |   |   9    /  |  \   18   |
; |  P11   |  D3,2,4  | ...111.. |  E3  |   227   |  1C  |    28   |    |        /   |    \      |   |       /   |   \       |
; |  P12   |  D3,2    | ....11.. |  F3  |   243   |  0C  |    12   |    |     P7     |      P3   |   |     25    |    19     |
; |  P13   |  D2      | .....1.. |  FB  |   251   |  04  |     4   |    |        P6  P5  P4      |   |       17  1  3        |
; |  P14   |  D2,4    | ...1.1.. |  EB  |   235   |  14  |    20   |    |                        |   |                       |
; |  P15   |  D1,2,4  | ...1.11. |  E9  |   233   |  16  |    22   |    +------------------------+   +-----------------------+
; |  P16   |  D1,2    | .....11. |  F9  |   249   |  06  |     6   |
;-----------------------------------------------------------------------------
FN_JOY:
;; ToDo: Move controller read code to routine in aux_rom 
    rst     CHRGET                ; Skip Token and Eat Spaces
    cp      '$'                   
    push    af                    ; Stack = FnSfx, RtnAdr
    call    z,CHRGTR              ; If JOY$, skip $
    call    PARCHK
    pop     af                    ; A = FnSfx; Stack = RtnAdr
    push    hl                    ; Stack = TxtPtr, RtnAdr
    ld      bc,LABBCK
    push    bc                    ; Stack = LABBCK, TxtPtr, RtnAdr
    push    af                    ; Stack = FnSfx, LABBCK, TxtPtr, RtnAdr
    call    CONINT                ; E = FnParm
    pop     af                    ; A = FnSfx; Stack = LABBCK, TxtPtr, RtnAdr
    ld      a, e                  ; A = FnParm
    jr      z,_joy_string
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
; JOY$(stick) function
; stick = controller index
; Returns: 8 byte string
;    1  ( Signed )  Left stick X 
;    2  ( Signed )  Left stick Y 
;    3  ( Signed )  Right stick X
;    4  ( Signed )  Right stick Y
;    5  (Unsigned)  Left trigger 
;    6  (Unsigned)  Right trigger
;    7  Bit 0  A
;           1  B
;           2  X
;           3  Y
;           4  View
;           5  Guide (Xbox button)
;           6  Menu
;           7  LS (Button in left stick)
;    8      0  RS (Button in right stick)
;           1  LB (Left shoulder button)
;           2  RB (Right shoulder button)
;           3  D-pad up
;           4  D-pad down
;           5  D-pad left
;           6  D-pad right
;           7  Share (Xbox Series S/X controller only)
;-----------------------------------------------------------------------------
_joy_string:
    push    af                    ; Stack = CtlIdx, LABBCK, TxtPtr, RtnAdr
    ld      a,8                   ; A = BufLen
    call    STRINI                ; HL = StrDsc, DE = StrAdr
    pop     af                    ; C = CtlIdx
    ld      iy,espx_get_gamectrl
    call    aux_call              ; Read game controller status into buffer
    jp      p,FINBCK              ; Pop LABBCK and do PUTNEW
    xor     a
    ld      (FACLO),a             ; Return null string
    pop     hl                    ; Stack = TxtPtr, RtnAdr
    jp      NULRT
    
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
; Syntax: OUT port, data
; Ports $E0, $EE - $F3, $FB
;-----------------------------------------------------------------------------
ST_OUT:
    call    check_bang          ; Stack = CurChr+Flag
    call    GETINT              ; Convert number to 16 bit integer (result in DE)
    pop     af                  ; F = BngFlg
    ld      a,e                 ; A = Port
    jr      z,.okay
    cp      IO_VCTRL             
    jr      c,.okay             ; A < VCTRL, Okay
    jp      z,FCERR             ; A = VCTRL, Error
    cp      IO_IRQMASK          
    jr      c,.okay
    cp      IO_ESPCTRL
    jp      c,FCERR
    cp      IO_SYSCTRL      
    jp      z,FCERR
.okay
    push    de                  ; Stored to be used in BC
    SYNCHK  ","                 ; Require comma
.dloop
    call    FRMEVL              ; Parse data portion
    call    GETYPE
    jr      z,.string           ; If number
    call    CONINT              ;   Convert to byte
    pop     bc                  ;   BC = IOPort
    out     (c), a              ;   Write byte to port
    jr      .next               ; Else
.string
    ex      (sp),hl             ;   HL = IOPort; Stack = TxtPtr
    push    hl                  ;   Stack = IOPort, TxtPtr
    call    free_addr_len       ;   DE = StrAdr, BC = StrLen
    ld      h,b
    ld      l,c                 ;   HL = StrLen
    pop     bc                  ;   BC = IOPort
    jr      z,.pophl            ;   If not ""
.sloop
    ld      a,(de)              ;     A = StrChr
    out     (c),a               ;     Write to IOPort
    inc     de                  ;     Bump StrPtr
    dec     hl                  ;     Count down
    ld      a,h
    or      l                   ;     If HL = 0
    jr      nz,.sloop           ;       OUT next character
.pophl
    pop     hl                  ;   HL = TxtPtr; Stack = RtnAdr
.next
    call    CHRGT2              ; Reget NxtChr
    ret     z                   ; If terminator, return
    ld      e,a                 ; Save NxtChr
    rst     CHRGET              ; Skip it
    ld      a,e                 ; Get it back
    cp      ';'                 ; If semicolon
    jr      z,ST_OUT            ;   Start over with next IOPort
    cp      ','                 ; If not comma
    jp      nz,SNERR            ;   Syntax error
    push    bc                  ;   Stack = IOPort, RtnAdr
    jr      .dloop              ;   Get next byte or string

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


