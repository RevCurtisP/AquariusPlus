;-----------------------------------------------------------------------------
; plus.asm - plusBASIC specific statements and functions
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; ATTR(fgcolor,bgcolor)
;-----------------------------------------------------------------------------
; ? ATTR(1,3);ATTR(2,1)
; ? ATTR$(1,3,2),ATTR$(2,1,4)
FN_ATTR:
    rst     CHRGET                ; Skip ATTR
    cp      '$'                   
    jr      nz,_attr_byte         ; If ATTR$
    call    skip_paren_colors     ;   DE = AtrByt
    push    af                    ;   Stack = AtrByt, RtnAdr
    call    get_comma_byte        ;   DE = Count
    SYNCHK  ')'
    pop     af                    ;   A = AtrByt; Stack = RtnAdr
    call    push_hl_labbck
    jp      SPACE2                ;   Do STRING$

_attr_byte:
    call    get_paren_colors      ;   DE = AtrByt
    SYNCHK  ')'
    jp      push_labbck_floatde

;-----------------------------------------------------------------------------
; BIT(expr,bit#)
;-----------------------------------------------------------------------------
; ToDo: Allow expr to be a string
; ? BIT(1,0)
; ? BIT("@",4)
FN_BIT:
    call    skip_frmprn_getype    ; A = Type
    jr      z,_bit_string         ; If string, Type mismatch error (for now)
    call    FRC_LONG              ; BCDE = LngInt
    push    bc
    push    de                    ; Stack = LngInt, RtnAdr
    call    get_comma_byte        ; E = BitNo
    SYNCHK  ')'
    ld      a,e                   ; A = BitNo
    pop     de
    pop     bc                    ; BCDE = LngInt, Stack = RtnAdr
    call    push_hl_labbck        ; Stack = LABBCK, TxtPtr, RtnAdr
    ld      iy,bool_checkbit_long
auxcall_floatsbyte:
    call    aux_call              ; Get bit value
    jp      c,FCERR               ; If invalid bit#, Illegal quantity error
    jp      float_signed_byte     ; Else return it
_bit_string:
    jp      GSERR

;-----------------------------------------------------------------------------
; BYTE(string)
; BYTE(string,offset)
;-----------------------------------------------------------------------------
FN_BYTE:
    rst     CHRGET                ; Skip BYTE
    ld      iy,float_signed_byte  ; Returning signed byte
    jp      sng_chr               ; Execute ASC() code
    
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
; Convert hexadecimal string to integer
; DEC(hexstring$)
;-----------------------------------------------------------------------------
; ?DEC("FFFF")
FN_DEC:
    rst     CHRGET            ; Skip DEC
    call    PARCHK
    call    push_hl_labbck
    call    free_addr_len   ; Free string and get text 
    dec     de              ; Back up Text Pointer
    xor     a               ; Set A to 0
    inc     c               ; Bump Length for DEC C
    ex      de,hl
    jp      eval_hex        ; Convert the Text
    
;-----------------------------------------------------------------------------
; GET functions stub
;-----------------------------------------------------------------------------
FN_GET:
    rst     CHRGET                ; Skip GET Token
    cp      COLTK
    jp      z,FN_GETCOLOR
    cp      TILETK
    jp      z,FN_GETTILE
    rst     SYNCHR
    byte    XTOKEN                ; Check Extended Tokens
    cp      SPRITK
    jp      z,FN_GETSPRITE
    cp      PALETK                
    jp      z,FN_GETPALETTE
    cp      CHRTK
    jp      z,FN_GETCHR
    cp      ATTRTK
    jp      z,FN_GETATTR
    cp      KEYTK
    jr      z,FN_GETKEY
    cp      SPEEDTK
    jr      z,FN_GETSPEED
    jp      SNERR

;-----------------------------------------------------------------------------
; GETKEY - Wait for key an return ASCII code
; GETKEY$  - Wait for key and return as string
;-----------------------------------------------------------------------------
FN_GETKEY:
    rst     CHRGET                ; Skip KEY
    push    hl
.loop
    call    CHARCG                ; Wait for keypress
    jr      z,.loop
    pop     hl
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

FN_GETSPEED:
    rst     CHRGET                ; Skip SPEED
    call    get_turbo_mode
push_hl_labbck_floata:
    call    push_hl_labbck
    jp      SNGFLT

;----------------------------------------------------------------------------
; Search for string in memory
; INMEM(address,string$)
; INMEM(@page,address,string$)
;----------------------------------------------------------------------------
; ToDo: INMEM(!extaddr,string)
;       Have to mod to return long
; PRINT INMEM(0,"Copyright")
; PRINT INMEM($3900,"Copyright")
; PRINT HEX$(INMEM(0,"Copy"+"wrong"))
; PRINT HEX$(INMEM(@61,0,"Out of DATA"))
; PRINT INMEM(@61,0,"WontFindMe")
FN_INMEM:
    rst     CHRGET                ; Skip MEM
    call    paren_page_arg        ; AF = PgFlag
    push    af                    ; Stack = PgFlag, RtnAdr
    call    GETINT                ; DE = MemAdr
    pop     af                    ; AF = PgFlag; Stack = RtnAdr
    push    de                    ; Stack = MemAdr, RtnAdr
    push    af                    ; Stack = PgFlag, MemAdr, RtnAdr
    call    get_comma             ; Require comma
    call    FRMEVL                ; Parse argument
    call    CHKSTR                ; Error if not string
    SYNCHK  ')'                   ; Require )
    pop     af                    ; AF = PgFlag; Stack = MemAdr, RtnAdr
    ex      (sp),hl               ; HL = MemAdr; Stack = TxtPtr, RtnAdr
    ld      bc,LABBCK             
    push    bc                    ; Stack = LABBCK, TxtPtr, RtnAdr
    push    hl                    ; Stack = MemAdr, TxtPtr, RtnAdr
    ex      af,af'                ; AF' = PgFlag
    call    free_addr_len         ; DE = TxtAdr, BC = TxtLen
    jp      z,pop_float_minus_one ; Return -1 if null string
    pop     hl                    ; HL = MemAdr; Stack = TxtAdr, RtnAdr
    ex      af,af'                ; AF = PgFlag
    call    .search
    ex      de,hl                 ; DE = Address in page
    ld      a,d
    and     e
    inc     a                     ; If address = $FFFF
    jp      z,float_signed_int    ;   Return -1
    jp      FLOAT_DE              ; Else return unsigned address 
.search
    jp      nc,string_find        ; If no page, search main memory and return
    call    page_find_string      ; Else search for page in string
    jp      z,FCERR               ; Illegal quantity error if invalid address
    ret

;----------------------------------------------------------------------------
; Search for string in string array
; INDEX(*array,string$)
; ToDo: INDEX(*array,string${,start})
;----------------------------------------------------------------------------
; A$(0)="foo":A$(9)="bar"
; ? INDEX(*A$,"foo")
; ? INDEX(*A$,"bar")
; ? INDEX(*A$,"baz")
FN_INDEX:
    rst     CHRGET                ; Skip DEX
    SYNCHK  '('                   ; Require (
    call    get_star_array        ; DE = AryAdr, BC = AryLen
    call    CHKSTR                ; Error if nor string
    srl     b
    rr      c
    srl     b
    rr      c                     ; ArySiz = AryLen / 4
    push    de                    ; Stack = AryAdr, RtnAdr
    push    bc                    ; Stack = ArySiz, AryAdr, RtnAdr
    SYNCHK  ','                   ; Require ,
    call    FRMEVL                ; Evaluate search arg
    SYNCHK  ')'                   ; Require (
    push    hl                    ; Stack = TxtPtr, ArySiz, AryAdr, RtnAdr
    call    free_addr_len         ; DE = StrAdr, A = StrLen
    pop     hl                    ; HL = TxtPtr; Stack = ArySiz, AryAdr, RtnAdr
    pop     bc                    ; BC = ArySiz; Stack = AryAdr, RtnAdr
    ex      (sp),hl               ; HL = AryAdr; Stack = TxtPtr, RtnAdr
    ex      de,hl                 ; HL = StrAdr, DE = AryAdr
    call    .array
    ld      de,LABBCK
    push    de                    ; Stack = LABBCK, TxtPtr, RtnAdr 
    ld      a,b                   ; 
    jp      GIVINT                ; Float AB signed and return

.array
    ld      iy,string_search_array
    jp      aux_call

;-----------------------------------------------------------------------------
; LOOP statements stub
;-----------------------------------------------------------------------------
ST_LOOP:
    rst     CHRGET                ; Skip LOOP token
    rst     SYNCHR
    byte    XTOKEN
    cp      PT3TK                 ;   If token is PT3
    jp      z,ST_LOOP_PT3         ;     Do PAUSE PT3
    jp      SNERR

;-----------------------------------------------------------------------------
; UPR(), UPR$(), LWR(), LWR$
;-----------------------------------------------------------------------------
FN_UPR:

FN_LWR:
    jp      GSERR


;-----------------------------------------------------------------------------
; MOUSEB - Returns mouse buttons
; MOUSEW - Returns mouse wheel delta
; MOUSEX - Returns mouse x-position
; MOUSEY - Returns mouse y-position
;-----------------------------------------------------------------------------
; To do: move core code to AuxROM
; Add MOUSE, possibly change esp_get_mouse to populate buffer
FN_MOUSE:
    rst     CHRGET                ; Skip MOUSE token
    jr      z,.snerr              ;   Error
    push    af                    ; Stack = SfxChr, RtnAdr
    rst     CHRGET                ; Skip Character after MOUSE
    ex      (sp),hl               ; HL = SfxChr, Stack = TxtPtr, RtnAdr
    ld      bc,LABBCK
    push    bc                    ; Stack = LABBCK, TxtPtr, RtnAdr
    push    hl                    ; Stack = SfxChr, LABBCK, TxtPtr, RtnAdr
    call    esp_get_mouse         ; BC = xpos, D = buttons, E = ypos, L = wheel
    jr      nz,.not_found
    ld      a,(MOUSEWDLT)
    add     l                     ; Accumulate mouse wheel delta
    ld      (MOUSEWDLT),a
    pop     af                    ; AF = SfxChr; Stack = LABBCK, TxtPtr, RtnAdr
    cp      'X'
    jr      z,.xpos
    cp      'Y'
    jr      z,.ypos
    cp      'B'
    jr      z,.buttons
    cp      'W'
    jr      z,.wheel
.snerr:
    jp      SNERR
.buttons
    ld      a,d
    byte    $06                   ; LD B, over next instruction
.ypos:
    ld      a,e
    jp      SNGFLT
.not_found:
    pop     af                    ; Stack = LABBCK, TxtPtr, RtnAdr
    ld      a,-1                  ; Return Not found
    jr      .signed_byte
.xpos:
    jp      FLOAT_BC
.wheel
    ld      a,(MOUSEWDLT)
    ex      af,af'
    xor     a
    ld      (MOUSEWDLT),a
    ex      af,af'
.signed_byte
    jp      float_signed_byte

;-----------------------------------------------------------------------------
; INKEY - Return ASCII code of currently pressed key
;-----------------------------------------------------------------------------
FN_INKEY:
    rst     CHRGET                ; Skip KEY Token
    push    hl                    ; Do the function stuff
    ld  bc,LABBCK
    push    bc
    call    CHARCG                ; Get Keypress
    jp      SNGFLT                ; and Float it

;-----------------------------------------------------------------------------
; KEY - Check 
; KEY(keycode) - Check if key is pressed
;-----------------------------------------------------------------------------
; RUN progs/keymove.bas
FN_KEY:
    rst     CHRGET                ; Skip KEY Token
    call    PARCHK                ; Evaluate argument
    push    hl                    ; Do the function stuff
    ld      bc,LABBCK
    push    bc
    call    GETYPE                ; Get argument type
    jr      z,.string             ; If numeric
    ld      a,-1
    rst     FSIGN
    call    p,CONINT              ;   A = Matrix code
    call    key_pressed           ;   A = -1 if pressed
    jp      c,FCERR               ;   Error if invalid keycode
    jp      float_signed_byte     ;   Return result
.string
    call    free_addr_len         ; DE = Address, BC - StrLen
    jp      z,FCERR               ; Error if NULL string
    ld      b,c
.loop
    ld      a,(de)
    push    bc
    push    de
    call    keycode
    ld      a,c
    call    key_pressed
    pop     de
    pop     bc
    jp      nz,float_signed_byte
    inc     de
    djnz    .loop
    xor     a
    jp      float_signed_byte

; Returns C = Keycode
keycode:
    ld      hl,keytable
    ld      b,keytablen
    ld      c,0
.loop
    cp      (hl)
    ret     z
    inc     hl
    inc     c
    djnz    .loop
    jp      FCERR

keytable:
    byte    '=',$08,':',$0D,';','.',$9D,$7F
    byte    '-','/','0','p','l',',',$8F,$8E
    byte    '9','o','k','m','n','j',$9E,$9F
    byte    '8','i','7','u','h','b',$9B,$9A
    byte    '6','y','g','v','c','f',$8A,$8B
    byte    '5','t','4','r','d','x',$89,$88
    byte    '3','e','s','z',' ','a',$9C,$8C
    byte    '2','w','1','q', 0 , 0 , 0 , 0 
keytablen   equ   $-keytable

;-----------------------------------------------------------------------------
; JOIN Statement
; Syntax: JOIN *array$ INTO string$ DEL delimiter
;-----------------------------------------------------------------------------
; A$(1)="a":JOIN *A$ INTO S$ DEL "|":? S$
; FOR I=0 TO 8:B$(I)=CHR$(I+64):NEXT:JOIN *B$ INTO S$ DEL ",":? S$
ST_JOIN:
    rst     CHRGET                ; Skip SPLIT
    call    get_star_array        ; DE = AryAdr, BC = AryLen
    call    _skipfirst            ; ARRAYPTR = AryPtr, ARRAYLEN = AryLen
    jp      z,BSERR               ; If only one element, Bad subscript error
    rst     SYNCHR
    byte    INTTK
    SYNCHK  'O'                   ; Require INTO   
    ld      de,(ARYTAB)           
    push    de                    ; Stack = AryTab, RtnAdr
    call    get_stringvar         ; DE = VarPtr
    ld      (SPLITDSC),de         ; SPLITDSC = VarPtr      
    pop     de                    ; DE = OldTab; Stack = RtnAdr
    push    hl                    ; Stack = TxtPtr, RtnAdr
    ld      hl,(ARYTAB)           
    sbc     hl,de                 ;  
    ex      de,hl                 ; DE = AryTab-OldTab
    ld      hl,(ARRAYPTR)         ; If arrays moved
    add     hl,de                 ; Adjust array pointer
    ld      (ARRAYPTR),hl         
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    rst     SYNCHR                ; Require DEL
    byte    DELTK
    call    get_char              ; A = DelChr
    ld      (SPLITDEL),a          ; SPLITDEL = DelChr
    push    hl                    ; Stack = TxtPtr, RtnAdr
    call    get_strbuf_addr       ; HL = StrBuf
    ld      (BUFADR),hl           ; BufAdr = StrBuf
    ld      (BUFPTR),hl           ; BufPtr = StrBuf
    ld      bc,0
    ld      (BUFLEN),bc           ; BufLen = 0
.loop
    ld      hl,(ARRAYPTR)         ; HL = AryPtr
    call    string_addr_len       ; DE = StrAdr, BC = StrLen
    ex      de,hl                 ; HL = StrAdr
    ld      de,(BUFPTR)           ; DE = BufPtr
    ld      a,(BUFLEN)            ; A = Buflen
    jr      z,.empty              ; If StrLen > 0
    add     c                     ;   A = BufLen + StrLen + 1
    inc     a                     ;   If A > 255
    jp      c,LSERR               ;     String to long error
    ldir                          ;   DE = NewPtr
    dec     a
.empty
    inc     a
    ld      (BUFLEN),a            ;   BufLen = A 
    ld      a,(SPLITDEL)          ; A = DelChr
    ld      (de),a                ; (NewPtr) = DelChr
    inc     de                    ; Bump NewPtr
    ld      (BUFPTR),de           ; DE = NewPtr
    call    _skipone              ; If not last element
    jr      nz,.loop              ;   Loop
    ld      hl,(BUFPTR)
    dec     hl                    ; Back up to last delimiter
    ld      (hl),0                ; Terminate string
    ld      a,(BUFLEN)            ; A = StrLen
    dec     a                     ; Cut last delimiter
    call    STRINI                ; HL = DscTmp, DE = DstAdr
    push    hl                    ; Stack = DscTmp, TxtAdr, RtnAdr
    ld      hl,(BUFADR)           ; HL = StrBuf
    ld      bc,(BUFLEN)           ; BC = BufLen
    ldir                          ; Copy Buffer to String Space
    pop     hl                    ; HL = DscTmp; Stack = TxtAdr, RtnAdr
    ld      de,(SPLITDSC)         ; DE = VarPtr
    call    MOVVFM                ; Copy StrDsc to Variable
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret

;-----------------------------------------------------------------------------
; SPLIT Statement
; Syntax: SPLIT string$ INTO *array$ DEL delimiter
;-----------------------------------------------------------------------------
ST_SPLIT:
    rst     CHRGET                ; Skip SPLIT
    call    GET_STRING            ; Parse SrcStr
    ld      de,(FACLO)            ; DE = SrcDsc
    ld      (SPLITDSC),de         ; SPLITDSC = SrcDsc
    rst     SYNCHR
    byte    INTTK
    SYNCHK  'O'                   ; Require INTO   
    call    get_star_array        ; DE = AryAdr, BC = AryLen
    ld      (ARRAYADR),de         ; ARRAYADR = array$(0)
    xor     a
    ld      (de),a                ; array$(0) = ""
    call    _clearfirst           ; ARRAYPTR = AryPtr, ARRAYLEN = AryLen
    ret     z                     ; If array size = 1, return
    rst     SYNCHR                ; Require DEL
    byte    DELTK
;get delimiter
    call    FRMEVL                ; Evaluate delimiter
    push    hl                    ; Stack = TxtPtr, RtnAdr
    call    GETYPE                ; A = Type
    jr      z,.string_del         ; If numeric
    call    CONINT                ;   Convert to Byte
    jr      .save_del             ; Else
.string_del
    call    FREFAC                ;   HL = DelDsc
    call    string_addr_len       ;   DE = DelAdr, A,BC = DelLen
    jr      z,.save_del           ;   If not null string
    ld      a,(de)                ;     A = DelChr
.save_del
    ld      (SPLITDEL),a          ; SPLITDEL = DelChr
    ld      hl,(SPLITDSC)         ; HL = SrcDsc
    call    FRETM2                ; HL = SrcDsc
    call    string_addr_len       ; DE = SrcAdr, BC = SrcLen
    jp      z,.abort              ; If null string, pop TxtPtr and return
    push    bc                    ; Stack = SrcLen, TxtPtr, RtnAdr
    call    get_strbuf_addr       ; HL = StrBuf
    pop     bc                    ; BC = SrcLen; Stack = TxtPtr, RtnAdr
    push    hl                    ; Stack = StrBuf, TxtPtr, RtnAdr
    ex      de,hl                 ; DE = StrBuf, HL = SrcAdr
    ldir                          ; Copy Source String to String Buffer
    xor     a
    ld      (de),a                ; Null terminate string
    ld      (SPLITSEG),a          ; SPLITSEG = SegCnt
    pop     hl                    ; HL = BufPtr; Stack = TxtPtr, RtnAdr
.loop
    push    hl                    ; Stack = SegPtr, TxtPtr, RtnAdr
    ld      a,(SPLITDEL)          ; A = DelChr
    ld      b,a                   ; B = DelChr
    ld      c,0
.parse
    ld      a,(hl)
    or      a
    jr      z,.store              ; If not NUL
    cp      b                     
    jr      z,.store              ; Or delimiter
    inc     hl
    inc     c                     ;   Check next character
    jr      .parse
.store
    push    hl                    ; Stack = BufPtr, SegPtr, TxtPtr, RtnAdr
    xor     a
    ld      d,a
    ld      e,a                   ; DE = 0
    ld      b,a                   ; BC = SegLen                 
    or      c                     ; A = SegLen
    jr      z,.null               ; If SegLen <> 0
    push    bc                    ;   Stack = SegLen, BufPtr, SegPtr, TxtPtr, RtnAdr
    call    GETSPA                ;   DE = StrAdr
    push    de                    ;   Stack = StrAdr, SegLen, BufPtr, TxtPtr, RtnAdr
    call    FRETMS                ;   Free temporary but not string space
    pop     de                    ;   DE = StrAdr; Stack = SegLen, BufPtr, SegPtr, TxtPtr, RtnAdr
    pop     bc                    ;   BC = SegLen; Stack = BufPtr, SegPtr, TxtPtr, RtnAdr
.null
    ld      hl,(ARRAYPTR)         ; HL = AryPtr
    call    write_bcde2hl         ; Write string descriptor to array
    ld      (ARRAYPTR),hl         ; ARRAYPTR = AryPtr
    ld      a,b
    or      c
    pop     hl                    ; HL = BufPtr; Stack = SegPtr, TxtPtr, RtnAdr
    ex      (sp),hl               ; HL = SegPtr, Stack = BufPtr, TxtPtr, RtnAdr
    jr      z,.nocopy             ; If SegLen <> 0
    ldir                          ;   Copy Segment to String Text
.nocopy
    ld      hl,SPLITSEG           ; Bump SegCnt
    inc     (hl)
    pop     hl                    ; HL = BufPtr; Stack = TxtPtr, RtnAdr
    ld      a,(hl)                ; A = SegDel
    inc     hl                    ; Skip segment delimiter
    or      a                      
    jr      z,.done               ; If not null (end of SrcStr)  
    ld      bc,(ARRAYLEN)         ;   BC = AryLen
    call    _countdown            ;   If not last element
    jr      nz,.loop              ;     Get next segment
.done
    ld      a,(SPLITSEG)          ; A = SegCnt
    call    SNGFLT                ; Float Segment Count
    call    FOUT                  ; Convert to ASCII
    ld      de,FBUFFR+1           ; DE = FltAdr
    call    str_length            ; A, BC = FltLen
    push    bc                    ; Stack = FltLen, TxtPtr, RtnAdr
    call    GETSPA                ; DE = StrAdr
    push    de                    ; Stack = StrAdr, FltLen, TxtPtr, RtnAdr 
    call    FRETMS                ; Free temporary but not string space
    pop     de                    ; DE = StrAdr; Stack = FltLen, TxtPtr, RtnAdr
    pop     bc                    ; BC = FltLen; Stack = TxtPtr, RtnAdr
    dec     bc                    ; Adjust for skip leading space
    push    de                    ; Stack = StrAdr, TxtPtr, RtnAdr
    push    bc                    ; Stack = FltLen, StrAdr, TxtPtr, RtnAdr
    ld      hl,FBUFFR+2           ; Skip leading space
    ldir                          ; Copy FBUFFR to string space
    pop     bc                    ; BC = FltLen; Stack = StrAdr, TxtPtr, RtnAdr
    pop     de                    ; DE = StrAdr; Stack = TxtPtr, RtnAdr
    ld      hl,(ARRAYADR)         ; HL = ARRAYADR
    call    write_bcde2hl         ; Write string descriptor to VAR$(0)
.abort
    ld      bc,(ARRAYLEN)
    ld      hl,(ARRAYPTR)
    call    fill_array_hl
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret

;Input HL: Pointer into Array, BC: Remaining Array Length
fill_array_hl:
    ld      a,b                   
    or      c                     ; If BC = 0
    ret     z                     ;   Return
    jp      sys_fill_zero         ; Fill to end with 0 bytes

_skipone
    ld    de,(ARRAYPTR)
    ld    bc,(ARRAYLEN)
_skipfirst
    inc   de
    inc   de
    inc   de
    inc   de
    jr    _ARRAYPTR
_clearfirst
    xor     a
    ld      b,4
.skiploop
    ld      (de),a
    inc     de
    djnz    .skiploop
_ARRAYPTR
    ld      (ARRAYPTR),de         ; ARRAYPTR = AryPtr
_countdown
    dec     bc
    dec     bc
    dec     bc
    dec     bc                    ; AryLen = AryLen - 4
    ld      (ARRAYLEN),bc         ; ARRAYLEN = AryLen
_array_len
    ld      a,b
    or      c                     ;   If not last element
    ret

;-----------------------------------------------------------------------------
; STASH Statement Stub
;-----------------------------------------------------------------------------
ST_STASH:
    rst     CHRGET                ; Skip STASH
    cp      SCRNTK                
    jp      z,ST_STASH_SCREEN
    jp      SNERR

;-----------------------------------------------------------------------------
; SWAP Statement Stub
;-----------------------------------------------------------------------------
ST_SWAP:
    cp      SCRNTK                
    jp      z,ST_SWAP_SCREEN
    rst     SYNCHR
    byte    XTOKEN
    cp      VARTK
    jp      z,ST_SWAP_VARS
    jp      SNERR

;-----------------------------------------------------------------------------
; TIME$ - Get Current Time
;-----------------------------------------------------------------------------
FN_TIME:
    rst     CHRGET                ; Skip Token
    cp      'R'                   ; If TIMER
    jr      z,FN_TIMER            ;   Read Countdown Timer
    SYNCHK  '$'                   ; Require dollar sign
    push    hl
    ld      bc,LABBCK
    push    bc
    ld      ix,esp_get_datetime   ; Read Date and Time into buffer
    call    bas_read_to_buff      ; Set buffer address and call routine
    ld      bc,8
    add     hl,bc                 ; Start of Date String
    jp      TIMSTR

;-----------------------------------------------------------------------------
; TIMER - Get Timer Count
;-----------------------------------------------------------------------------
FN_TIMER:
    rst     CHRGET                ; Skip 'R'
    push    hl                    ; Text Pointer on Stack
    ld      hl,LABBCK
    push    hl                    ; Return to LABBCK
    call    timer_read            ; Read timer
    jp      FLOAT_CDE             ; Return as 23 bit integer

;-----------------------------------------------------------------------------
; TIMER - Set Timer Count
; Syntax: TIMER = Expression
;-----------------------------------------------------------------------------
ST_TIMER:
    SYNCHK  'R'                   ; Require 'R'
    rst     SYNCHR
    byte    EQUATK                ; Require '='
    call    GET_LONG              ; Get count into C,D,E
    jp      timer_write           ; Set the timer

;----------------------------------------------------------------------------
; Compare blocks of memory
; Syntax: COMPARE( !extaddr|{@page,}address , !extaddr|{@page,}address , length)
;----------------------------------------------------------------------------
FN_COMPARE:
    rst     CHRGET                ; Skip COMPARE
    SYNCHK  '('
    cp      MULTK                 ; If *
    jp      z,.compare_arrays     ;   Compare Arrays
    call    get_page_addr         ; AF = PgFlg1, DE = Addr1
    push    de                    ; Stack = Addr1, RtnAdr
    push    af                    ; Stack = PgFlg1, Addr1, RtnAdr
    call    get_comma_page_addr   ; AF = PgFlg2, DE = Addr2
    push    af                    ; Stack = PgFlg2, PgFlg1, Addr1, RtnAdr
    push    de                    ; Stack = Addr2, PgFlg2, PgFlg1, Addr1, RtnAdr
    call    get_comma_int         ; DE = Length
    call    close_paren
    ld      b,d
    ld      c,e                   ; BC = Length
    pop     de                    ; DE = Addr2 ; Stack = PgFlg2, PgFlg1, Addr1, RtnAdr
    pop     af                    ; AF = PgFlg2; Stack = PgFlg1, Addr1, RtnAdr
    ex      af,af'                ; AF' = PgFlg2
    pop     af                    ; AF = PgFlg1; Stack = Addr1, RtnAdr
    ex      (sp),hl               ; HL = Addr1 ; Stack = TxtPtr, RtnAdr
    push    hl                    ; Stack = Addr1, TxtPtr, RtnAdr
    ld      hl,LABBCK             
    ex      (sp),HL               ; HL = Addr1; Stack = LABBCK, TxtPtr, RtnAdr
    jr      c,.page1              ; If Addr1 not paged
    ex      af,af'                ;   AF = PgFlg2, AF' = PgFlg1
    jr      c,.mem2page           ;   If Addr2 not paged
.mem2mem
    call    sys_mem_compare       ;     Do Memory to Memory Compare
    jr      .float_it             ;     Return Result
.mem2page                         ;   Else
    ex      de,hl                 ;     AF = PgFlg2, HL = Addr2, DE = Addr1
    jr      .page_mem             ;     Compare Paged to Memory and return result
.page1                            ; Else
    ex      af,af'                ;   AF = PgFlg2, AF' = PgFlg1
    jr      c,.page2page          ;   If Addr2 not paged
    ex      af,af'                ;     AF = PgFlg1, HL = Addr1, DE = Addr2
.page_mem
    call    page_mem_compare      ;      Compare Paged to Memory`
    jr      .float_it             ;      and return result
.page2page:                       ;   Else
    call    page_page_compare     ;     GS error for now
.float_it
    jp      float_signed_int

.compare_arrays
    call    skip_star_array       ; DE = Addr1, BC = Len1
    push    de                    ; Stack = Addr1, RtnAdr
    push    bc                    ; Stack = Len1, Addr1, RtnAdr
    call    get_comma             ; MO Error if no Comma
    call    get_star_array        ; DE = Addr2, BC = Len2
    call    close_paren           ; Require ')', TOERR if ','
    ex      (sp),hl               ; HL = Len1; Stack = TxtPtr, Addr1, RtnAdr
    sbc     hl,bc                 ; If Len1 - Len2 <> 0
    jr      nz,.badlen            ;   Return 0 (not equal)
    pop     hl                    ; HL = TxtPtr; Stack = Addr1, RtnAdr
    ex      (sp),hl               ; HL = Addr1, Stack = TxtPtr, RtnAdr
    call    sys_mem_compare       ;      Compare Paged to Memory`
    ld      hl,LABBCK             
    push    hl                    ; Stack = LABBCK, TxtPtr, RtnAdr
    jp      float_signed_int      ; Return result
.badlen
    pop     hl                    ; HL = TxtPtr; Stack = Addr1, RtnAdr
    pop     de                    ; Stack = RtnAdr
    push    hl                    ; Stack = TxtPtr, RtnAdr
    ld      hl,LABBCK             
    push    hl                    ; Stack = LABBCK, TxtPtr, RtnAdr
    xor     a
    jp      SNGFLT                ; Return 0
    
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
    ld      bc,-1
    ld      (TEMP3),bc
    ld      c,5               ;
    call    tokenize          ; Call KLOOP, restore Extended ROM
    ld      hl,BUF            ; Point to Line Buffer
    call    FRMEVL            ; Evaluate Formula
    pop     hl                ; Restore Text Pointer
    ret

LSERR:
    ld      e,ERRLS
    jp      ERROR


;-----------------------------------------------------------------------------
; FILL Statement stub
; FILL BYTE [@page], startaddr, oount, byte
; FILL WORD [@page], startaddr, count, word
;-----------------------------------------------------------------------------
; ToDo: Chain memory fills with semicolon
; Allow chaining of all the following 
; FILL BITMAP BYTE becomes FILL BITMAP (x,y)-(x,y) ...
; FILL BITMAP COLOR becomes FILL COLORMAP
; FILL SCREEN CHR becomes FILL SCREEN
; FILL SCREEN ATTR becomes FILL COLOR
;
; FILL BYTES $4003,29,$5A
; FILL WORDS $5000,13,$1234
; FILL BYTES @33,35,27,$A5
; FILL WORDS @33,126,7,$CDEF
; FILL BYTES !8888,27,$A5
; FILL WORDS !9999,7,$CDEF
ST_FILL:
    rst     CHRGET                ; Skip FILL
    cp      SCRNTK
    jp      z,ST_FILL_SCREEN
    cp      TILETK
    jp      z,ST_FILL_TILE
    cp      BITTK
    jp      z,ST_FILL_BITMAP
    cp      COLTK
    jp      z,ST_FILL_COLORMAP
    rst     SYNCHR
    byte    XTOKEN
    cp      BYTETK
    jr      z,.fillmem
    cp      WORDTK
    jp      nz,SNERR
.fillmem
    push    af                    ; Stack = BytWrd, RtnAdr
    rst     CHRGET                ; Skip BYTE/WORD
    SYNCHK  'S'                   ; Require S
    call    get_page_addr         ; Check for Page Arg
    pop     bc                    ; BC = BytWrd; Stack = RtnAdr
    push    de                    ; Stack = BgnAdr, RtnAdr
    push    bc                    ; Stack = BytWrd, BgnAdr, RtnAdr
    push    af                    ; Stack = PgFlg, BytWrd, BgnAdr, RtnAdr
    call    get_comma_int         ; DE = Count
    push    de                    ; Stack = Count, PgFlag, BytWrd, BgnAdr, RtnAdr
    call    get_comma_int         ; DE = FilVal
    pop     bc                    ; BC = Count; Stack = PgFlag, BytWrd, BgnAdr, RtnAdr
    pop     af                    ; AF = PgFlag; Stack = BytWrd, BgnAdr, RtnAdr
    ex      af,af'                ; AF' = PgFlag
    pop     af                    ; AF = BytWrd; Stack = BgnAdr, RtnAdr
    ex      (sp),hl               ; HL = BgnAdr; Stack = TxtPtr, RtnAdr
    push    hl                    ; Stack = BgnAdr, TxtPtr, RtnAdr
    ld      hl,POPHRT             ; After fill, POP HL and RET
    ex      (sp),hl               ; HL = BgnAdr; Stack = POPHRT, TxtPtr, RtnAdr
    ex      de,hl                 ; DE = BgnAdr, HL = FilVal
    cp      WORDTK
    jr      z,.isword             ; If not fill WORDS
    ld      a,h
    or      a                     ;   If FilVal > 255
    jp      nz,FCERR              ;     Blow Up
    ex      af,af'                ;   AF = PgFlag
    jp      c,page_fill_byte
    ld      a,l                   ;   A = FilByt
    ex      de,hl                 ;   HL = BgnAdr
    jp      sys_fill_mem          ;   
.isword
    ex      af,af'                ; AF = PgFlag
    jp      c,page_fill_word
    ex      de,hl                 ; HL = BgnAdr, DE = FilVal
    jp      sys_fill_word          
    
;----------------------------------------------------------------------------
; GET Statement stub
;----------------------------------------------------------------------------
ST_GET:
    cp      SCRNTK                 
    jp      z,ST_GET_SCREEN
    cp      TILETK                
    jp      z,ST_GET_TILEMAP      
    rst     SYNCHR
    byte    XTOKEN
    cp      ARGSTK
    jp      z,ST_GETARGS
    jp      SNERR

;----------------------------------------------------------------------------
; LINE Statement stub
;----------------------------------------------------------------------------
ST_LINE:
    cp      INPUTK
    jp      z,ST_LINE_INPUT
    jp      SNERR

;----------------------------------------------------------------------------
; PUT Statement stub
;----------------------------------------------------------------------------
ST_PUT:
    cp      SCRNTK
    jp      z,ST_PUT_SCREEN
    cp      TILETK                ; If GET TILEMAP
    jp      z,ST_PUT_TILEMAP      ;   Go do it
    jp      SNERR

;-----------------------------------------------------------------------------
; SET Statement stub
;-----------------------------------------------------------------------------
; ToDo: SET BIT var,bit#
; ToDo: SET MOUSE ON/OFF/TILE (mouse.asm)
ST_SET:
    cp      BITTK                 ; $EB
    jr      z,ST_SET_BIT
    cp      TILETK                ; $F0
    jp      z,ST_SET_TILE
    cp      COLTK                 ; $F5
    jp      z,ST_SETCOLOR
    cp      FNTK                  ; $A2
    jp      z,ST_SET_FNKEY
    cp      SAVETK                ; $DC
    jp      z,ST_SET_SAVE
    cp      FILETK                ; $F3
    jp      z,ST_SET_FILE
    cp      USRTK                 ; $B5
    jp      z,ST_SET_USR

    rst     SYNCHR                ; Must be extended Token
    byte    XTOKEN                ; $FE
    cp      PALETK                ; $81
    jp      z,ST_SETPALETTE
    cp      SPRITK                ; $84
    jp      z,ST_SET_SPRITE
    cp      CHRTK                 ; $85
    jr      z,ST_SET_CHR
    cp      KEYTK                 ; $86
    jr      z,ST_SET_KEY
    cp      FASTK                 ; $88
    jr      z,ST_SET_FAST
    cp      PT3TK                 ; $8C
    jp      z,ST_SET_PT3
    cp      BRKTK                 ; $9A
    jr      z,ST_SET_BREAK
    cp      SPEEDTK               ; $B1
    jr      z,ST_SET_SPEED
    jp      SNERR

;-----------------------------------------------------------------------------
; Set specified bit in variable to 1
; Syntax: SET BIT VAR,bit#
;-----------------------------------------------------------------------------
ST_SET_BIT:
    jp      GSERR

;-----------------------------------------------------------------------------
; Toggle control-c checking
; Syntax: SET BREAK ON/OFF
;-----------------------------------------------------------------------------
ST_SET_BREAK:
    call    get_on_off            ; A = $FF if ON, $00 if OFF
    xor     $FF                   ; A = $00 if enable, $FF if disabled
    jp      sys_break_mode

;-----------------------------------------------------------------------------
; Redifine character in Character RAM
; Syntax: SET CHRDEF ascii_code TO string$
; ToDo: SET CHRDEF ascii_code {IN char_set} TO string$
;-----------------------------------------------------------------------------
; SET CHRDEF 127 TO $"AA55AA55AA55AA55":PRINT CHR$(127)
; SET CHRDEF "@" TO $"FF00FF00FF00FF00":PRINT "@"
ST_SET_CHR:
    rst     CHRGET                ; Skip CHR
    rst     SYNCHR
    byte    DEFTK                 ; Require DEF
    call    get_char              ; A = ChrASC
    push    af                    ; Stack = ChrASC, RtnAdr
    rst     SYNCHR
    byte    TOTK                  ; Require TO
    call    get_string_arg        ; DE = StrAdr, BC = StrLen, Stack = TxtPtr, ChrASC, RtnAdr
    ld      a,c
    cp      8                     ; If StrLen <> 8
    jp      nz,FCERR              ;   Illegal quantity error
    pop     hl                    ; HL = TxtPtr; Stack = ChrASC, RtnAdr
    pop     af                    ; A = ChrASC
    push    hl                    ; Stack = TxtAdr, RtnAdr
    ld      l,b                   ; Destination = 0 - ChrRAM
    ld      iy,gfx_redefine_char        
    call    aux_call
    jp      c,LSERR
    ld      a,(BASYSCTL)          ; Set character set modified flag
    or      a,BASCHRMOD        
    ld      (BASYSCTL),a
    pop     hl                    ; HL = TxtAdr; Stack = RtnAdr
    ret

;-----------------------------------------------------------------------------
; Set keybuffer mode
; Syntax: SET KEY mode
;-----------------------------------------------------------------------------
ST_SET_KEY:
    rst     CHRGET                ; Skip KEY
    call    GETBYT                ; Get key mode
    push    hl
    call    key_set_keymode       ; Set the mode
    jp      m,FCERR
    pop     hl
    ret

;-----------------------------------------------------------------------------
; Set Z80 Clock Speed
; Syntax: SET SPEED speed
;-----------------------------------------------------------------------------
ST_SET_SPEED:
    call    skip_get_byte4         ; 
    jr      _turbo_mode
;-----------------------------------------------------------------------------
; Set Fast Mode
; Syntax: SET FAST ON/OFF
;-----------------------------------------------------------------------------
ST_SET_FAST:
    call    get_on_off            ; A = $FF if ON, $00 if OFF
    and     1                     ; A = 1 if ON, 0 if OFF
_turbo_mode:
    call    set_turbo_mode
    jp      c,FCERR               ; If illegal mode, Illlegal quantity
    ret

;-----------------------------------------------------------------------------
; Set Fast Mode
; Syntax: SET FILE ERROR ON/OFF
;-----------------------------------------------------------------------------
; SET FILE ERROR OFF:? PEEK($3830)
; SET FILE ERROR OFF:CD "XXX":PRINT ERR,ERR$,ERRLINE
; SET FILE ERROR OFF:LOAD "XXX":PRINT ERR,ERR$,ERRLINE
ST_SET_FILE:
    rst     CHRGET
    rst     SYNCHR
    byte    ERRTK
    rst     SYNCHR
    byte    ORTK
    call    check_on_off          ; A = $FF if ON, $00 if OFF
set_ferr_flag:
    and     FERR_FLAG
    ld      b,a
    ld      a,(EXT_FLAGS)
    and     $FF-FERR_FLAG
    or      b 
    ld      (EXT_FLAGS),a
    ret

;-----------------------------------------------------------------------------
; Write string to FNKEY buffer
; Syntax: SET FNKEY TO string$
;-----------------------------------------------------------------------------
; SET FNKEY 3 TO "CD"+CHR$(13)
ST_SET_FNKEY:
    rst     CHRGET                ; Skip FN
    rst     SYNCHR
    byte    XTOKEN                ; Require KEY
    rst     SYNCHR
    byte    KEYTK
    call    GETBYT                ; Get Function Key Number
    dec     a                     ; Make it 0 to 15
    cp      16                    ; If > 15
    jp      nc,FCERR              ;   Illegal quantity
    push    af                    ; Stack = FnkNum, RtnAdr
    rst     SYNCHR                
    byte    TOTK                  ; Require TO
    call    get_string_arg        ; BC = StrLen, DE = StrAdr; Stack = TxtPtr, FnkNum, RtnAdr
    ld      a,c
    cp      32                    ; If longer than 31
    jp      nc,ERRLS              ;   String too long error
    pop     hl                    ; HL = TxtPtr; Stack = FnkNum, RtnAdr
    ex      (sp),hl               ; H = FnkNum; Stack = TxtPtr, RtnAdr
    ld      a,h                   ; A = FnkNum
    ld      iy,fnkey_write_buffer ; Write to the buffer
    call    aux_call
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret

;-----------------------------------------------------------------------------
; Enable/Disable User Interrupt
; Syntax: SET USRINT ON/OFF
;-----------------------------------------------------------------------------
ST_SET_USR:
    inc     hl                    ; Skip USR
    rst     SYNCHR
    byte    INTTK                 ; Require INT
    call    check_on_off          ; A = $FF if ON, $00 if OFF
    push    hl                    ; Stack = TxtPtr, RtnAdr
    ld      hl,userint_enable
    jr      nz,.set_userint
    ld      hl,userint_disable
.set_userint
    call    jump_hl
    pop     hl
    ret

;-----------------------------------------------------------------------------
; PAUSE Statement 
;-----------------------------------------------------------------------------
ST_PAUSE:
    jp      z,.tryin              ; If no argument, wait for key and return
    jp      p,.nottoken           ; If followed by token
    rst     SYNCHR
    byte    XTOKEN
    cp      PT3TK                 ;   If token is PT3
    jp      z,ST_PAUSE_PT3        ;     Do PAUSE PT3
.nottoken
    call    FRMEVL                ; Get Operand
    push    hl                    ; Stack = TxtPtr, RtnAdr
    call    GETYPE                ; 
    jp      z,.string             ; If Numeric
    call    FRCINT                ; DE = Jiffies
    ld      a,(BASYSCTL)
    and     BASBRKOFF
    ld      iy,pause_jiffies
    call    aux_call
    pop     hl
    jp      key_clear_fifo
.string
    call    STRPRT
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
.tryin
    jp      TRYIN                 ; Wait for key and return
    

;-----------------------------------------------------------------------------
; RESET Statement stub
;-----------------------------------------------------------------------------
;; ToDo: RESET BIT var,bit#
;;       RESET CHRSET set#
;;       RESET CHRDEF ascii_code
ST_RESET:
    rst     CHRGET                ; Skip RESET
    cp      MULTK
    jr      z,_reset_array
    cp      SCRNTK                
    jp      z,ST_RESET_SCREEN
    rst     SYNCHR
    byte    XTOKEN
    cp      PALETK
    jp      z,ST_RESET_PALETTE
    jp      SNERR

;-----------------------------------------------------------------------------
; Set all array elements to 0 or empty string
; RESET *array
;-----------------------------------------------------------------------------
_reset_array:
    call    skip_star_array       ; DE = AryAdr, BC = AryLen
    push    hl                    ; Stack = TxtPtr, RtnAdr
    call    GETYPE                ; A = 0 and Z set if string
    push    af                    ; Stack = TypFlg, TxtPtr, RtnAdr
    ex      de,hl                 ; HL = AryAdr
    call    sys_fill_zero         ; Fill array data with zeros
    pop     af                    ; A = TypFlg
    ld      bc,POPHRT
    push    bc                    ; Return to POPHRT
    ret     nz                    ; If numeric, pop TxtPtr and return
    jp      GARBA2                ; Else collect garbage, pop TxtPtr and return
    
;-----------------------------------------------------------------------------
; USE Statement stub
;-----------------------------------------------------------------------------
ST_USE:
    cp      SCRNTK
    jp      z,ST_USE_SCREEN
    call    SYNCHR
    byte    XTOKEN
    cp      CHRTK
    jp      z,ST_USECHR           ; USE CHRSET
    jp      SNERR

;-----------------------------------------------------------------------------
; VER Function
; Syntax: VER$(x)
; x = 0 for System, 1 for plusBASIC, 2 for S3 BASIC
;-----------------------------------------------------------------------------
FN_VER:
    inc     hl                    ; Skip VER 
    ld      a,(hl)                ; Get following character
    cp      '$'                   ; 
    push    af                    ; Stack = StrFlg, RtnAdr
    jr      nz,.notstring         ; If '$'
    inc     hl                    ;   Skip it
.notstring
    SYNCHK  '('
    call    FRMEVL                ; Evaluate argument
    SYNCHK  ')'
    call    GETYPE
    jr      nz,.getver            ; If it's a string
    ex      (sp),hl               ; HL = StrFlg, Stack = TxtPtr, RtnAdr
    push    hl                    ; Stack = StrFlg, TxtPtr, RtnAdr
    call    free_addr_len         ; BC = StrLen, DE = StrAdr, HL = StrDsc
    ex      de,hl                 ; HL = StrAdr
    jr      .return_ver
.getver    
    call    CONINT                ; Convert argument to byte
    ld      ix,esp_get_version
    or      a
    jr      z,.zero
    ld      ix,sys_ver_plusbasic
    cp      1
    jr      z,.zero
    ld      ix,sys_ver_s3basic
.zero
    ex      (sp),hl               ; HL = StrFlg, Stack = TxtPtr, RtnAdr
    push    hl                    ; Stack = StrFlg, TxtPtr, RtnAdr
    ld      hl,FBUFFR
    ld      bc,14
    call    jump_ix               ; Get version string
.return_ver
    pop     af                    ; F = StrFlg, 
    ld      bc,LABBCK
    push    bc                    ; Stack = LABBCK, TxtPtr, RtnAdr
    jp      z,TIMSTR              ; If VER$(), return version string
    call    sys_num_ver           ; Else convert to integer
    jp      FLOAT_CDE             ; and return it

;-----------------------------------------------------------------------------
; WORD(string)
; WORD(string,offset)
; WORD$(integer)
;-----------------------------------------------------------------------------
; ? WORD($"FFFF")
; ? WORD($"0000")
; ? WORD("ABCD",2)
; ? WORD$($3141)
FN_WORD:
    inc     hl                    ; Check character directly after ASC Token
    ld      a,(hl)                ; (don't skip spaces)
    cp      '$'                   
    jr       z,str_word           

    call    frmprn_getype
    jp      nz,word_int
    ld      iy,FLOAT_DE
word_str:
    ld      de,(FACLO)
    push    de                    ; Stack = StrDsc, RtnAdr
    push    iy                    ; Stack = FltJmp, RtnAdr
    ld      de,1                  ; WrdPos = 1
    ld      a,(hl)
    cp      ','
    jr      nz,_first_word
    rst     CHRGET                ; Skip Comma
    call    GETBYT                ; DE = WrdPos
    or      a
    jp      z,FCERR
_first_word:    
    SYNCHK  ')'
    pop     iy                    ; IY = FltJmp; Stack = StrDsc, RtnAdr
    ex      (sp),hl               ; HL = StrDsc; Stack = TxtPtr, RtnAdr                
    ld      bc,LABBCK
    push    bc                    ; Stack = LABCK, TxtPtr, RtnAdr
    push    de                    ; Stack = WrdPos, LABBCK, TxtPtr, RtnAdr
    call    free_hl_addr_len      ; DE = StrAdr, BC = StrLen
    pop     hl                    ; HL = WrdPos; Stack = LABBCK, TxtPtr. RtnAdr
    ld      a,c
    dec     a                     ; A = StrLen - 1
    cp      l                     ; If WrdPos > StrLen-1
    jp      c,FCERR               ;   Illegal quantity
    dec     hl                    ; Adjust WrdPos for add
    add     hl,de                 ; HL = WrdAdr
    ld      e,(hl)                
    inc     hl
    ld      d,(hl)                ; DE = WrdVal
    jp      (iy)                  ; Float it

word_int:
    SYNCHK  ')'                   ; Require )
    call    push_hl_labbck        ; Stack = LABBCK, TxtPtr, RtnAdr
    call    FRCINT                ; DE = ArgVal
    jp      FLOAT_DE

str_word:
    rst     CHRGET                ; Skip $
    call    PARCHK                
    push    hl                    ; Stack = TxtPtr. RtnAdr
    call    FRCINT                ; DE = ArgVal
    push    de                    ; Stack = ArgVal, TxtPtr, RtnAdr
    ld      a,2
    call    STRINI                ; Allocate 2 byte string
    pop     de                    ; DE = ArgVal; Stack = TxtPtr. RtnAdr
    ld      hl,(DSCTMP+2)         ; HL = StrAdr
    ld      (hl),e                ; Write ArgVal to string
    inc     hl
    ld      (hl),d                
    jp      PUTNEW                ; and return it

;-----------------------------------------------------------------------------
; WRITE Statement stub
;-----------------------------------------------------------------------------
ST_WRITE:
   cp       XTOKEN
   jp       nz,GSERR
   rst      CHRGET                ; Skip XTOKEN
   cp       KEYTK
   jr       z,ST_WRITE_KEYS
   jp       SNERR
   
;-----------------------------------------------------------------------------
; WRITE KEYS Statement
; Syntax: WRITE KEYS string$
;-----------------------------------------------------------------------------
; WRITE KEYS \" ? 123\r"
ST_WRITE_KEYS:
   rst      CHRGET                ; Skip KEY
   SYNCHK   'S'                   ; Require S
   call     get_free_string       ; DE = StrAdr, BC = StrLen
   ld       a,' '                 ; Will be gobbled by Ctrl-C check
   ld       iy,autokey_write_buffer
   jp       aux_call_popret       ; Write to auto-key buffer