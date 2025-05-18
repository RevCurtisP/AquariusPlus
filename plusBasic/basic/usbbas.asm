;====================================================================
; Statements and Functions from USB BASIC
;====================================================================


;-----------------------------------------------------------------------------
; Syntax: CALL address
; on entry to user code, HL = text after address
; on exit from user code, HL should point to end of statement
;-----------------------------------------------------------------------------
; CALL $245D
; CALL $210F ARGS $20C5

ST_CALL:
    call    get_page_addr         ; A = Page, DE = Addr
    push    de                    ; Stack = Addr, RtnAdr
    push    af                    ; Stack =Page, Addr, RtnAdr 
    ld      a,(hl)
    cp      XTOKEN                ; If Extended token
    jr      nz,.call
.xtoken
    inc     hl                    ;   Skip XTOKEN
    ld      a,(hl)                ;   Get extended token
    cp      ARGSTK
    dec     hl                    ;   TxtPtr to XTOKEN
    jr      nz,.call              ;   If ARGS
    inc     hl                    ;     TxtPtr to ARGS
.args
    call    skip_get_int
    ld      (SAVEHL),de           ;     SAVEHL = Arg1
    call    .comma_arg
    ld      (SAVEDE),de           ;     SAVEDE = Arg2
    call    .comma_arg
    ld      (SAVEBC),de           ;     SAVEBC = Arg3
    pop     af                    ;     A = Page; Stack = Addr, RtnAdr
    pop     iy                    ;     IY = Addr; Stack = RtnAdr
    push    hl                    ;     Stack = TxtPtr. RtnAdr
    ld      hl,POPHRT
    push    hl                    ;     Stack = POPHRT, TxtPtr, RtnAdr
    push    iy                    ;     Stack = Addr, POPHRT, TxtPtr, RtnAdr
    push    af                    ;     Stack = Page, Addr, POPHRT, TxtPtr, RtnAdr
    ld      hl,(SAVEHL)           ;     HL = Arg1
    ld      de,(SAVEDE)           ;     DE = Arg2
    ld      bc,(SAVEBC)           ;     BC = Arg3
.call
    pop     af                    ; If no page specified
    ret     nc                    ;   Jump to user code, HL = BASIC text pointer
    pop     iy                    ; Else
    jp      page_call             ;   Jump to address in page 

.comma_arg
    ld      de,0
    ld      a,(hl)
    cp      ','                   ; Check for comma
    ret     nz
    jp      get_comma_int

;-----------------------------------------------------------------------------
; HEX$() function
; eg. A$=HEX$(B)
;-----------------------------------------------------------------------------
FN_HEX:
    rst     CHRGET                ; Skip Token and Eat Spaces
    call    PARTYP                ; FACC = Arg, A = ArgTyo
    jr      z,HEX_STRING          ; If String, Convert It and Return
    push    hl                    ; Stack = TxtPtr
    push    bc                    ; Stack = DummyRtn, TxtPtr
    call    FRC_LONG              ; CDE = Arg
    ld      hl,FBUFFR+1           ; HL = temp string
    ld      a,c
    or      a                     ; If high byte <> 0
    jr      z,.middle_byte
    call    byte_to_hex           ;   Convert to hex string
    ld      a,d
    jr      .force_middle
.middle_byte
    ld      a,d
    or      a                     ; If middle byte <> 0
    jr      z,.lower_byte
.force_middle
    call    byte_to_hex           ;   Convert to hex string
.lower_byte
    ld      a,e
    call    byte_to_hex           ; Convert low byte to hex string
    ld      (hl),0                ; Null-terminate string
    ld      hl,FBUFFR+1
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
    call    CHKNUM
    call    FRCINT                ; DE = IOPort
    push    de                    ; Stack = IOPort, RtnAdr
    call    get_comma_byte        ; DE = StrLen
    SYNCHKC ')'                   ; Require )
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

;--------------------------------------------------------------------------------------------------------------------------------
; JOY() function
; syntax: var = JOY(stick)
;    stick - 0 will read left or right
;          - 1 will read left joystick only
;          - 2 will read right joystick only
;
; |        | Data bus |  Binary   | Hex  | Decimal | JOY  |  JOY()  |
; | Switch | grounded | 765 43210 | code |  code   | Hex  | Decimal |
; | ------ | -------- | --------- | ---- | ------- | ---- | ------- |
; |  K1    |  D6      | .1. ..... |  BF  |   191   |  40  |    64   |
; |  K2    |  D7,2    | 1.. ..1.. |  7B  |   123   |  84  |   132   |    +------------------------+   +-----------------------+
; |  K3    |  D7,5    | 1.1 ..... |  5F  |    95   |  A0  |   160   |    |                        |   |                       |
; |  K4    |  D5      | ..1 ..... |  DF  |   223   |  20  |    32   |    |    [K1]  [K2]  [K3]    |   |  [ 64]  [132]  [160]  |
; |  K5    |  D7,1    | 1.. ...1. |  7D  |   125   |  82  |   130   |    |                        |   |                       |
; |  K6    |  D7,0    | 1.. ....1 |  7E  |   126   |  81  |   129   |    |    [K4]  [K5]  [K6]    |   |  [ 32]  [130]  [129]  |
; | ------ | -------- | --------- | ---- | ------- | ---- | ------- |    |                        |   |                       |
; |  P1    |  D1      | ... ...1. |  FD  |   253   |  02  |     2   |    |                        |   |                       |
; |  P2    |  D1,4    | ... 1..1. |  ED  |   237   |  12  |    18   |    |      P12 P13 P14       |   |       12  4  20       |
; |  P3    |  D1,0,4  | ... 1..11 |  EC  |   236   |  13  |    19   |    |   P11      |     P15   |   |    28     |     22    |
; |  P4    |  D1,0    | ... ...11 |  FC  |   252   |  03  |     3   |    |       \    |   /       |   |       \   |   /       |
; |  P5    |  D0      | ... ....1 |  FE  |   254   |  01  |     1   |    |   P10  \   |  /  P16   |   |  24    \  |  /    6   |
; |  P6    |  D0,4    | ... 1...1 |  EE  |   238   |  11  |    17   |    |         \  | /         |   |         \ | /         |
; |  P7    |  D3,0,4  | ... 11..1 |  E6  |   230   |  19  |    25   |    |   P9 ------*------ P1  |   |  8 -------*------- 2  |
; |  P8    |  D3,0    | ... .1..1 |  F6  |   246   |  09  |     9   |    |          / |  \        |   |         / | \         |
; |  P9    |  D3      | ... .1... |  F7  |   247   |  08  |     8   |    |    P8   /  |   \  P2   |   |   9    /  |  \   18   |
; |  P10   |  D3,4    | ... 11... |  E7  |   231   |  18  |    24   |    |        /   |    \      |   |       /   |   \       |
; |  P11   |  D3,2,4  | ... 111.. |  E3  |   227   |  1C  |    28   |    |     P7     |      P3   |   |     25    |    19     |
; |  P12   |  D3,2    | ... .11.. |  F3  |   243   |  0C  |    12   |    |        P6  P5  P4      |   |       17  1  3        |
; |  P13   |  D2      | ... ..1.. |  FB  |   251   |  04  |     4   |    |                        |   |                       |
; |  P14   |  D2,4    | ... 1.1.. |  EB  |   235   |  14  |    20   |    +------------------------+   +-----------------------+
; |  P15   |  D1,2,4  | ... 1.11. |  E9  |   233   |  16  |    22   |    
; |  P16   |  D1,2    | ... ..11. |  F9  |   249   |  06  |     6   |
;--------------------------------------------------------------------------------------------------------------------------------
FN_JOY:
    ld      bc,0                  ; FnSfx1, FnSfx2 = None
    call    .getsfx
    jr      z,.arg                
    ld      b,a                   ; B = FnSfx1
    call    .getsfx
    jr      z,.arg                
    ld      c,a                   ; C = FnSfx2
    inc     hl                    ; Skip FnSfx2
.arg
    push    bc                    ; Stack = FnSfxs, RtnAdr
    call    PARCHK                ; Parse Argument
    pop     bc                    ; A = FnSfx; Stack = RtnAdr
    call    push_hl_labbck
    call    aux_call_inline       ; A = PortVal 
    word    bas_joy            ;
    jr      c,_joy_string         ; If JOY$, go do it
    jp      m,FLOAT
    jp      SNGFLT                ; Float it

.getsfx:
    inc     hl                     
    ld      a,(hl)                
    cp      ' '   
    ret     z
    cp      '('
    ret


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
    call    aux_call_inline       ; Read controller status into temp string
    word    bas_joy_string
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
    call    parse_screen_coord
    ex      de, hl              ; Switch DE with HL
    call    aux_call_inline
    word    move_cursor         ; Cursor to screen location HL (H=col, L=row)
    ex      de, hl
    ret

parse_screen_coord:
    call    GETBYT              ; Read number from command line (column). Stored in A and E
    push    af                  ; Column store on stack for later use
    dec     e
    ld      a,(LINLEN)
    dec     a
    dec     a                   ; A = 38 or 78
    cp      e                   ; If less than screen position
    jp      c, FCERR            ; If w, Illegal Quantity

    ; Expect comma
    SYNCHKC ','

    call    GETBYT              ; Read number from command line (row). Stored in A and E
    cp      24                  ; Compare with 24 decimal (max rows on screen)
    jp      nc,FCERR            ; If higher then 24 goto FC error

    inc     e
    pop     af                  ; Restore column from store
    ld      d, a                ; Column in register D, row in register E
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
    SYNCHKC ','                 ; Require comma
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
    SYNCHKC   ','

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
    SYNCHKC   ','

    ; Get value to write to PSG register
    call    GETBYT           ; Get/evaluate value
    out     (IO_PSG2DATA), a ; Send data to the selected PSG register

    jr      .check_comma


