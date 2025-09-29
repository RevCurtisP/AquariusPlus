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
    SYNCHKC ')'
    pop     af                    ;   A = AtrByt; Stack = RtnAdr
    call    push_hl_labbck
    jp      SPACE2                ;   Do STRING$

_attr_byte:
    call    get_paren_colors      ;   DE = AtrByt
    SYNCHKC ')'
    jp      push_labbck_floatde

;-----------------------------------------------------------------------------
; BIN$(long)
;-----------------------------------------------------------------------------
FN_BIN:
    jp      GSERR
    inc     hl                    ; Skip BIN
    SYNCHKC '$'                   ; Require $
    call    PARTYP                ; FACC = Arg
    jp      z,TMERR               ; Type mismatch error if string
    ld      a,(FAC)               ; A = Exponent
    sub     128                   ; A = BinLen
    jr      z,.zero_string
    jr      c,.zero_string

.zero_string

;-----------------------------------------------------------------------------
; BIT(expr,bit#)
;-----------------------------------------------------------------------------
; ? BIT(1,0)
; ? BIT("@",6)
FN_BIT:
    call    FRMPRS                ; A = Type
    jr      z,_bit_string
    call    FRC_LONG              ; BCDE = LngInt
    push    bc
    push    de                    ; Stack = LngInt, RtnAdr
    call    get_comma_byte        ; E = BitNo
    SYNCHKC ')'
    ld      a,e                   ; A = BitNo
    pop     de
    pop     bc                    ; BCDE = LngInt, Stack = RtnAdr
    ld      iy,bool_checkbit_long
    call    push_hl_labbck        ; Stack = LABBCK, TxtPtr, RtnAdr
auxcall_floatsbyte:
    call    aux_call              ; Get bit value
    jp      c,FCERR               ; If invalid bit#, Illegal quantity error
    jp      FLOAT                 ; Else return it
_bit_string:
    ld      de,(FACLO)            ; DE = StrDsc
    push    de                    ; Stack = StrDsc, RtnAdr
    call    get_comma_int         ; DE = BitNo
    SYNCHKC ')'
    pop     bc                    ; BC = StrDsc; Stack = RtnAdr
    call    push_hl_labbck        ; Stack = LABBCK, TxtPtr, RtnAdr
    push    de                    ; Stack = BitNo, LABBCK, TxtPtr, RtnAdr
    ld      h,b
    ld      l,c                   ; HL = StrDsc
    call    free_hl_addr_len      ; BC = StrLen, DE = StrAdr
    pop     hl                    ; HL = BitNo; Stack = LABBCK, TxtPtr, RtnAdr
    ld      iy,bool_checkbit_string
    jr      auxcall_floatsbyte


;-----------------------------------------------------------------------------
; BYTE(string)
; BYTE(string,offset)
;-----------------------------------------------------------------------------
FN_BYTE:
    rst     CHRGET                ; Skip BYTE
    ld      iy,FLOAT              ; Returning signed byte
    jp      sng_chr               ; Execute ASC() code


;-----------------------------------------------------------------------------
; CHECK functions stub
;-----------------------------------------------------------------------------
FN_CHECK:
    inc     hl
    ld      a,(hl)
    cp      DIRTK
    jp      z,FN_CHECKDIR
    jp      SNERR

;-----------------------------------------------------------------------------
; CHECK statements stub
;-----------------------------------------------------------------------------
ST_CHECK:
    rst     CHRGET                ; Skip CHECK
    SYNCHKT XTOKEN
    SYNCHKT VERTK
    
ST_CHECK_VER:
    call    get_free_string       ; BC = ArgLen, DE = ArgAdr, HL = ArgDsc; Stack = TxtPtr
    ld      iy,aux_check_ver
    call    aux_call              ; HL = BufAdr
    jp      nc,POPHRT             ; If version okay, pop TxtPtr and return
    call    print_c_string        ; Print error message
    jp      ENDPRG                ; and end program
  
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
    SYNCHKC '$'                   ; Require Dollar Sign
    call    push_hl_labbck
    call    aux_call_inline
    word    bas_date
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
; DUMP Variable and Array Table to File
;-----------------------------------------------------------------------------
ST_DUMP:
    rst     CHRGET                ; Skip DUMP
    SYNCHKT XTOKEN                ; Require VARS
    SYNCHKT VARTK
    SYNCHKC 'S'
    jp      z,MOERR
    call    str_literal
    push    hl                    ; Stack = TxtPtr, RtnAdr
    call    get_strbuf_addr
    ex      de,hl                 ; DE = StrBuf
    ld      hl,FACLO              ; HL = FilNam
    ld      iy,dump_vars
    call    aux_call              ; Do the dump
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret

;-----------------------------------------------------------------------------
; GET functions stub
;-----------------------------------------------------------------------------
FN_GET:
    rst     CHRGET                ; Skip GET Token
    cp      COLTK
    jp      z,FN_GETCOLOR
    cp      TILETK
    jp      z,FN_GETTILE
    cp      SOUNDTK
    jr      Z,FN_GETSOUND
    SYNCHKT XTOKEN                ; Check Extended Tokens
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
    cp      CURTK
    jr      z,FN_GETCURSOR
    cp      BORDTK
    jp      z,FN_GETBORDER
    cp      LWRTK
    jp      z,FN_GETLWR
    cp      UPRTK
    jp      z,FN_GETUPR
    jp      SNERR

;-----------------------------------------------------------------------------
; GETKEY - Wait for key an return ASCII code
; GETKEY$  - Wait for key and return as string
;-----------------------------------------------------------------------------
FN_GETKEY:
    rst     CHRGET                ; Skip KEY
    push    hl
    call    get_key               ; Wait for keypress
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

FN_GETSOUND:
    rst     CHRGET                ; Skip SOUND
    SYNCHKT XTOKEN
    SYNCHKT FASTK                 ; Require FAST
    call    push_hl_labbck        ; Stack = LABBCK, TxtPtr, RtnAdr
    ld      iy,get_soundfast
    jp      aux_call_float

FN_GETSPEED:
    rst     CHRGET                ; Skip SPEED
    call    get_turbo_mode
push_hl_labbck_floata:
    call    push_hl_labbck
    jp      SNGFLT

FN_GETCURSOR:
    call    require_sor
    call    get_cursor_mode
push_hl_labbck_float_sbyte:
    call    push_hl_labbck
    jp      FLOAT

;----------------------------------------------------------------------------
; Decrement variable
; INC A
;----------------------------------------------------------------------------
ST_DEC:
    ld      iy,bas_dec
    jr      _inc_dec

;----------------------------------------------------------------------------
; Increment variable
; INC A
;----------------------------------------------------------------------------
ST_INC:
    SYNCHKC 'C'                   ; Must be IN + C
    ld      iy,bas_inc
_inc_dec
    CALL    PTRGET
    jp      aux_call

;----------------------------------------------------------------------------
; Search for string in memory
; INMEM(address,string$)
; INMEM(@page,address,string$)
;----------------------------------------------------------------------------
;; ToDo: INMEM(!extaddr,string)
;       Have to mod to return long
; PRINT INMEM(0,"Copyright")
; PRINT INMEM($3900,"Copyright")
; PRINT HEX$(INMEM(0,"Copy"+"wrong"))
; PRINT HEX$(INMEM(@61,0,"Out of DATA"))
; PRINT INMEM(@61,0,"WontFindMe")
FN_INMEM:
    rst     CHRGET                ; Skip MEM
    push    af                    ; Stack = PgFlag, RtnAdr
    call    GETINT                ; DE = MemAdr
    pop     af                    ; AF = PgFlag; Stack = RtnAdr
    push    de                    ; Stack = MemAdr, RtnAdr
    push    af                    ; Stack = PgFlag, MemAdr, RtnAdr
    call    get_comma             ; Require comma
    call    FRMSTR                ; Parse string argument
    SYNCHKC ')'                   ; Require )
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
    SYNCHKC '('                   ; Require (
    call    get_star_array        ; DE = AryAdr, BC = AryLen
    call    CHKSTR                ; Error if nor string
    srl     b
    rr      c
    srl     b
    rr      c                     ; ArySiz = AryLen / 4
    push    de                    ; Stack = AryAdr, RtnAdr
    push    bc                    ; Stack = ArySiz, AryAdr, RtnAdr
    SYNCHKC ','                   ; Require ,
    call    FRMEVL                ; Evaluate search arg
    SYNCHKC ')'                   ; Require (
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
; FLOAT(string)
; FLOAT(string,offset)
; FLOAT$(long)
;-----------------------------------------------------------------------------
FN_FLOAT:
    inc     hl                    ; Check character directly after ASC Token
    ld      a,(hl)                ; (don't skip spaces)
    cp      '$'
    ld      bc,MOVRF
    jp      z,str_float           ;   Go do it
    ld      bc,return_float
    jp      float_str

;-----------------------------------------------------------------------------
; LONG(string)
; LONG(string,offset)
; LONG$(long)
;-----------------------------------------------------------------------------
FN_LONG:
    inc     hl                    ; Check character directly after ASC Token
    ld      a,(hl)                ; (don't skip spaces)
    cp      '$'
    jr      z,str_long            ;   Go do it
    ld      bc,FLOAT_CDE
float_str:
    push    bc
    call    FRMPRN
    call    CHKSTR
    pop     bc
    ld      de,(FACLO)
    push    de                    ; Stack = StrDsc, RtnAdr
    push    bc                    ; Stack = FltJmp, RtnAdr
    ld      de,1                  ; BinPos = 1
    ld      a,(hl)
    cp      ','
    jr      nz,_first_long
    rst     CHRGET                ; Skip Comma
    call    GETBYT                ; DE = BinPos
    or      a
    jp      z,FCERR
_first_long:
    SYNCHKC ')'
    pop     iy                    ; IY = FltJmp; Stack = StrDsc, RtnAdr
    ex      (sp),hl               ; HL = StrDsc; Stack = TxtPtr, RtnAdr
    ld      bc,LABBCK
    push    bc                    ; Stack = LABCK, TxtPtr, RtnAdr
    push    de                    ; Stack = BinPos, LABBCK, TxtPtr, RtnAdr
    call    free_hl_addr_len      ; DE = StrAdr, BC = StrLen
    pop     hl                    ; HL = BinPos; Stack = LABBCK, TxtPtr. RtnAdr
    ld      a,c
    sub     3                     ; A = StrLen - 3
    cp      l                     ; If BinPos > StrLen-3
    jp      c,FCERR               ;   Illegal quantity
    dec     hl                    ; Adjust BinPos for add
    add     hl,de                 ; HL = WrdAdr
    ld      e,(hl)
    inc     hl
    ld      d,(hl)                ; DE = WrdVal
    inc     hl
    ld      c,(hl)                ; DE = WrdVal
    inc     hl
    ld      b,(hl)                ; DE = WrdVal
    jp      (iy)                  ; Float it

str_long:
    ld      bc,FRC_LONG
str_float:
    rst     CHRGET                ; Skip $
    push    bc
    call    PARCHK
    pop     iy
    push    hl                    ; Stack = TxtPtr. RtnAdr
    call    jump_iy               ; BCDE = ArgVal
    push    de
    push    bc                    ; Stack = ArgVal, TxtPtr, RtnAdr
    ld      a,4
    call    STRINI                ; Allocate 2 byte string
    pop     bc
    pop     de                    ; DE = ArgVal; Stack = TxtPtr. RtnAdr
    ld      hl,(DSCTMP+2)         ; HL = StrAdr
    ld      (hl),e                ; Write ArgVal to string
    inc     hl
    ld      (hl),d
    inc     hl
    ld      (hl),c
    inc     hl
    ld      (hl),b
    jp      PUTNEW                ; and return it

;-----------------------------------------------------------------------------
; LOOP statements stub
;-----------------------------------------------------------------------------
ST_LOOP:
    rst     CHRGET                ; Skip LOOP token
    SYNCHKT XTOKEN
    cp      TRKTK                 ;   If token is TRACK
    jp      z,ST_LOOP_TRACK       ;     Do PAUSE TRACK
    jp      SNERR

;-----------------------------------------------------------------------------
; UPR(), UPR$(), LWR(), LWR$
; GETUPR, GETUPR$, UPRKEY, UPRKEY$
;-----------------------------------------------------------------------------
FN_GETUPR:
FN_GETLWR:
    jp      GSERR
    cp      UPRTK                 ; Set Z it UPRTK, NZ if LWRTK
    push    af                    ; Stack = UprFlg, RtnAdr
    inc     hl                    ; Skip UPR/LWR
    ld      ix,get_key
FN_UPR:
FN_LWR:
    ld      a,(hl)                ; Reget Token
    cp      UPRTK                 ; Set Z it UPRTK, NZ if LWRTK
    push    af                    ; Stack = UprFlg, RtnAdr
    inc     hl                    ; Skip UPR/LWR
    ld      a,(hl)                ; A = NxtChr
    cp      (XTOKEN)              ; If Extended Token
    ld      iy,in_key
    jr      z,.extended           ;   Do UPRKEY, LWRKEY
    cp      '$'                   ;
    jr      z,.do_string          ; If UPR() or LWR()
    call    get_char_parens       ;   C = Character
    pop     af                    ;   F = UprFlg; Stack = RtnAdr
    call    push_hl_labbck        ;   Stack = LABBCK, TxtPtr, RtnAdr
.ret_char
    ld      a,c                   ;   A = Operand
    call    uprlwr_char           ;   Convert character
    jp      float_byte            ;   and return it
.do_string
    inc     hl                    ; Skip $
    call    PARTYP                ; F = ValTyp
    pop     bc                    ; B = UprType; Stack = RtnAdr
    push    hl                    ; Stack = TxtPtr
    push    hl                    ; Stack = DmyRtn, TxtPtr
    push    bc                    ; Stack = UprFlg, DmyRtn, TxtPtr, RtnAdr
    jp      z,.string
    call    STRIN1
    call    CONINT                ; E     = Character
.ret_string
    pop     af                    ; F = UprFlg; Stack = DmyRtn, TxtPtr, RtnAdr
    ld      a,e                   ; A = Character
    call    uprlwr_char           ;   Convert character
    ld      e,a
    jp      SETSTR
.string
    call    faclo_addr_len        ; BC = ArgLen, DE = ArgAdr, HL = ArgDsc
    jp      z,pop_null_string     ; If StrLen = 0  Return Null String
    pop     af                    ; F = UprFlg; Stack = DmyRtn, TxtPtr, RtnAdr
    push    hl                    ; Stack = ArgDsc, DmyRtn, TxtPtr
    push    de                    ; Stack = ArgAdr, ArgDsc, DmyRtn, TxtPtr
    push    bc                    ; Stack = ArgLen, ArgAdr, ArgDsc, DmyRtn, TxtPtr
    push    af                    ; Stack = UprFlg, ArgLen, ArgAdr, ArgDsc, DmyRtn, TxtPtr
    ld      a,c                   ; A = StrLen
    call    STRINI                ; DE = NewAdr
    pop     af                    ; F = UprFlg; Stack = ArgLen, ArgAdr, ArgDsc, DmyRtn, TxtPtr
    pop     bc                    ; BC = ArgLen; Stack = ArgAdr, ArgDsc, DmyRtn, TxtPtr
    pop     hl                    ; HL = ArgAdr; Stack = ArgDsc, DmyRtn, TxtPtr
    ld      iy,uprlwr_string
    call    aux_call               ; Convert the string
    pop     de                    ; DE = ArgDsc; Stack = DmyRtn, TxtPtr
    call    FRETMP                ; Free ArgStr
    jp      FINBCK                ; Return Result String

; 10 IF K=UPRKEY:ON -(K=0) GOTO 10:? K: GOTO 10

.extended
    inc     hl
    SYNCHKT KEYTK                 ; Require KEY
    call    check_dollar          ; Stack = StrFlg, UprFlg, RtnAdr
    pop     bc                    ; C = StrFlg; Stack = UprFlg, RtnAdr
    pop     af                    ; F = UprFlg; Stack = RtnAdr
    call    push_hl_labbck        ; Stack = LABBCK, TxtPtr, RtnAdr
    push    af                    ; Stack = UprFlg, LABBCK, TxtPtr, RtnAdr
    push    bc                    ; Stack = StrFlg, UprFlg, LABBCK, TxtPtr, RtnAdr
    call    jump_iy               ; Get Keypress
    ld      c,a
    pop     af                    ; A = StrFlg; Stack = UprFlg, LABBCK, TxtPtr, RtnAdr
    jr      z,.key_str            ; If returning number
    pop     af                    ;   A = UprFlg; Stack = LABBCK, TxtPtr, RtnAdr
    jr      .ret_char             ;   Convert C, float and return
.key_str
    ld      a,c                   ; A = KeyChr
    or      a                     ; If no key
    jp      z,pop_null_string     ;   Pop UprFlg and return ""
    push    bc                    ; Stack = KeyChr, UprFlg, LABBCK, TxtPtr, RtnAdr
    call    STRIN1
    pop     de                    ; E = KeyChr; Stack = UprFlg, LABBCK, TxtPtr, RtnAdr
    jr      .ret_string

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
    jp      z,SNERR               ;   Error
    push    af                    ; Stack = SfxChr, RtnAdr
    rst     CHRGET                ; Skip Character after MOUSE
    ex      (sp),hl               ; HL = SfxChr, Stack = TxtPtr, RtnAdr
    ld      bc,LABBCK
    push    bc                    ; Stack = LABBCK, TxtPtr, RtnAdr
    call    aux_call_inline
    word    bas_mouse             ; A or BC = Result
    jp      c,FLOAT_BC
    jp      m,FLOAT               
    jp      SNGFLT

;-----------------------------------------------------------------------------
; INKEY - Return ASCII code of currently pressed key
;-----------------------------------------------------------------------------
FN_INKEY:
    rst     CHRGET                ; Skip KEY Token
    push    hl                    ; Do the function stuff
    ld      bc,LABBCK
    push    bc
    call    in_key                ; Get Keypress
    jp      SNGFLT                ; and Float it

;-----------------------------------------------------------------------------
; KEY - Check for Matrix Keypress
; KEY(keycode) - Check if pecified key is pressed
; KEY(-1) - Check if any key is pressed
; KEY(string$) - Check for keypress in list
;-----------------------------------------------------------------------------
; RUN progs/keymove.bas
FN_KEY:
    call    key_read              ; Drain key buffer
    rst     CHRGET                ; Skip KEY Token
    call    PARTYP                ; Evaluate argument in parens
    call    push_hl_labbck
    call    aux_call_inline
    word    bas_key
    jp      (hl)

;-----------------------------------------------------------------------------
; JOIN Statement
; Syntax: JOIN *array$ INTO string$ DEL delimiter
;-----------------------------------------------------------------------------
; A$(1)="a":JOIN *A$ INTO S$ DEL "|":? S$
; FOR I=0 TO 8:B$(I)=CHR$(I+64):NEXT:JOIN *B$ INTO S$ DEL ",":? S$
ST_JOIN:
    rst     CHRGET                ; Skip JOIN
    call    get_star_array        ; DE = AryAdr, BC = AryLen
    call    aux_call_inline
    word    bas_skipfirst         ; ARRAYPTR = AryPtr, ARRAYLEN = AryLen
    jp      z,BSERR               ; If only one element, Bad subscript error
    SYNCHKT INTTK
    SYNCHKC 'O'                   ; Require INTO
    ld      de,(ARYTAB)
    push    de                    ; Stack = AryTab, RtnAdr
    call    get_stringvar         ; DE = VarPtr
    ld      (SPLITDSC),de         ; SPLITDSC = VarPtr
    pop     de                    ; DE = OldTab; Stack = RtnAdr
    push    hl                    ; Stack = TxtPtr, RtnAdr
    ld      hl,(ARYTAB)
    sbc     hl,de                 ; DE = AryTab-OldTab
    ex      de,hl                 ; If arrays moved because get_stringvar
    ld      hl,(ARRAYPTR)         ;   created a new string variable
    add     hl,de                 ;   Adjust array pointer
    ld      (ARRAYPTR),hl
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    call    parse_delimiter       ; A = DelChr
    ld      (SPLITDEL),a          ; SPLITDEL = DelChr
    push    hl                    ; Stack = TxtPtr, RtnAdr
    call    aux_call_inline
    word    bas_join
    pop     hl
    ret

;-----------------------------------------------------------------------------
; OFFSET Function
; OFFSET ( row, column)
;-----------------------------------------------------------------------------
FN_OFF:
    inc     hl                    ; Skip OFF
    SYNCHKT SETTK                 ; Require SET
    call    SCAND                 ; C = Column, E = Row
    call    push_hl_labbck        ; Stack = LABBCK, TxtPtr, RtnAdr
    call    aux_call_inline
    word    bas_offset            ; DE = Offset
    jp      FLOAT_DE

;-----------------------------------------------------------------------------
; SPLIT Statement
; Syntax: SPLIT string$ INTO *array$ DEL delimiter
;-----------------------------------------------------------------------------
ST_SPLIT:
    rst     CHRGET                ; Skip SPLIT
    call    FRMSTR                ; Parse SrcStr
    ld      de,(FACLO)            ; DE = SrcDsc
    ld      (SPLITDSC),de         ; SPLITDSC = SrcDsc
    SYNCHKT INTTK
    SYNCHKC 'O'                   ; Require INTO
    call    get_star_array        ; DE = AryAdr, BC = AryLen
    ld      (ARRAYADR),de         ; ARRAYADR = AryAdr
    xor     a
    ld      (de),a                ; array$(0) = ""
    ld      (ARRAYPTR),de         ; ARRAYADR = AryAdr
    ld      (ARRAYLEN),bc         ; ARRAYLEN = AryLen
    call    parse_delimiter       ; A = DelChr
    ld      (SPLITDEL),a          ; SPLITDEL = DelChr
    push    hl
    call    aux_call_inline
    word    bas_split
    pop     hl
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
    SYNCHKT XTOKEN
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
    SYNCHKC '$'                   ; Require dollar sign
    call    push_hl_labbck
aux_call_timstr:
    call    aux_call_inline
    word    bas_time
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
    SYNCHKC 'R'                   ; Require 'R'
    SYNCHKT EQUATK                ; Require '='
    call    GET_LONG              ; Get count into C,D,E
    jp      timer_write           ; Set the timer

;----------------------------------------------------------------------------
; Compare blocks of memory
; Syntax: COMPARE( !extaddr|{@page,}address , !extaddr|{@page,}address , length)
;----------------------------------------------------------------------------
FN_COMPARE:
    rst     CHRGET                ; Skip COMPARE
    SYNCHKC '('
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
    SYNCHKT XTOKEN
    cp      BYTETK
    jr      z,.fillmem
    cp      WORDTK
    jp      nz,SNERR
.fillmem
    push    af                    ; Stack = BytWrd, RtnAdr
    rst     CHRGET                ; Skip BYTE/WORD
    SYNCHKC 'S'                   ; Require S
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
    SYNCHKT XTOKEN
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
    SYNCHKT XTOKEN
    cp      CHRTK
    jp      z,ST_PUT_CHR
    jp      SNERR

;-----------------------------------------------------------------------------
; SET Statement stub
;-----------------------------------------------------------------------------
; ToDo: SET BIT var,bit#
; ToDo: SET BORDER CHR char COLOR fg,bg
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
    cp      SOUNDTK               ; $B5
    jp      z,ST_SET_SOUND

    SYNCHKT XTOKEN                ; Must be extended Token
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
    cp      TRKTK                 ; $8C
    jp      z,ST_SET_TRACK
    cp      BRKTK                 ; $9A
    jr      z,ST_SET_BREAK
    cp      CURTK                 ; $AA
    jr      z,ST_SET_CURSOR
    cp      SPEEDTK               ; $B1
    jr      z,ST_SET_SPEED
    cp      BORDTK                ; $B6
    jp      z,ST_SET_BORDER
    jp      SNERR

;-----------------------------------------------------------------------------
; Set specified bit in variable to 1
; Syntax: SET BIT VAR,bit#
;-----------------------------------------------------------------------------
ST_SET_BIT:
    jp      GSERR
    ld      bc,bas_set_bit
_set_reset_bit:
    rst     CHRGET                ; Skip BIT
    push    bc                    ; Stack = AuxRtn, RtnAdr
    call    PTRTYP                ; DE = VarPtr, AF = VarTyp
    push    af                    ; Stack = VarTyp, AuxRtn, RtnAdr
    push    de                    ; Stack = VarPtr, VarTyp, AuxRtn, RtnAdr
    call    get_comma_int         ; DE = BitNo
    pop     bc                    ; BC = VarPtr; Stack = VarTyp, AuxRtn, RtnAdr
    pop     af                    ; AF = VarTyp; Stack = AuxRtn, RtnAdr
    pop     iy                    ; IY = AuxRtn; Stack = RtnAdr
    jp      aux_call_preserve_hl  ; Execute core code and return


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
    SYNCHKT DEFTK                 ; Require DEF
    call    get_char              ; A = ChrASC
    push    af                    ; Stack = ChrASC, RtnAdr
    call    get_to_string_arg     ; DE = StrAdr, BC = StrLen, Stack = TxtPtr, ChrASC, RtnAdr
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
    ld      a,(SCREENCTL)         ; Set character set modified flag
    or      a,SCRCHRMOD
    ld      (SCREENCTL),a
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
; Set Cursor Display Mode
;-----------------------------------------------------------------------------
ST_SET_CURSOR
    call    require_sor
    call    check_on_off          ; A = $FF if ON, $00 if OFF
    jp      set_cursor_mode

require_cursor:
    SYNCHKT CURTK
    byte    $3E                   ; LD A, over INC HL
require_sor:
    inc     hl                    ; Skip CUR
    SYNCHKC 'S'
    SYNCHKT ORTK                  ; Require SOR
    ret

;-----------------------------------------------------------------------------
; Set File Error generation
; Syntax: SET FILE ERROR ON/OFF
;-----------------------------------------------------------------------------
; SET FILE ERROR OFF:? PEEK($3830)
; SET FILE ERROR OFF:CD "XXX":PRINT ERR,ERR$,ERRLINE
; SET FILE ERROR OFF:LOAD "XXX":PRINT ERR,ERR$,ERRLINE
ST_SET_FILE:
    rst     CHRGET
    cp      '#'
    jp      z,set_filepos
    SYNCHKT ERRTK
    SYNCHKT ORTK
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
    SYNCHKT XTOKEN                ; Require KEY
    SYNCHKT KEYTK
    call    GETBYT                ; Get Function Key Number
    dec     a                     ; Make it 0 to 15
    cp      16                    ; If > 15
    jp      nc,FCERR              ;   Illegal quantity
    push    af                    ; Stack = FnkNum, RtnAdr
    call    get_to_string_arg     ; BC = StrLen, DE = StrAdr; Stack = TxtPtr, FnkNum, RtnAdr
    ld      a,c
    cp      32                    ; If longer than 31
    jp      nc,LSERR              ;   String too long error
    pop     hl                    ; HL = TxtPtr; Stack = FnkNum, RtnAdr
    ex      (sp),hl               ; H = FnkNum; Stack = TxtPtr, RtnAdr
    ld      a,h                   ; A = FnkNum
    ld      iy,fnkey_write_buffer ; Write to the buffer
    call    aux_call
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret

;-----------------------------------------------------------------------------
; Set SOUND turbo mode (defaults to ON)
; Syntax: SET SOUND FAST ON/OFF
;-----------------------------------------------------------------------------
ST_SET_SOUND:
    rst     CHRGET                ; Skip SOUND
    SYNCHKT XTOKEN
    SYNCHKT FASTK                 ; Require FAST
    call    check_on_off          ; 0 = Off, $FF = On
    ld      iy,set_soundfast
    jp      aux_call              ; Set flag and return

;-----------------------------------------------------------------------------
; Enable/Disable User Interrupt
; Syntax: SET USRINT ON/OFF
;-----------------------------------------------------------------------------
ST_SET_USR:
    inc     hl                    ; Skip USR
    SYNCHKT INTTK                 ; Require INT
    call    check_on_off          ; A = $FF if ON, $00 if OFF
    push    hl                    ; Stack = TxtPtr, RtnAdr
    ld      hl,userint_enable
    jr      nz,.set_userint
    ld      hl,userint_disable
.set_userint
    call    JUMPHL
    pop     hl
    ret

;-----------------------------------------------------------------------------
; PAUSE Statement
;-----------------------------------------------------------------------------
;; ToDo: make pause_jiffies return carry set if ctrl-c was pressed
;;       and clear keybuffer if it was
ST_PAUSE:
    jp      z,.tryin              ; If no argument, wait for key and return
    jp      p,.notxtoken          ; If followed by token
    cp      XTOKEN
    jr      nz,.notxtoken
    rst     CHRGET
    cp      UNTILTK
    jr      z,ST_PAUSE_UNTIL
    cp      TRKTK                 ;   If token is TRACK
    jp      z,ST_PAUSE_TRACK      ;     Do PAUSE TRACK
.notxtoken
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


; PAUSE UNTIL INKEY
ST_PAUSE_UNTIL
    rst     CHRGET                ; Skip UNTIL
.loop
    push    hl                    ; Stack = CndPtr, RtnAdr
    call    INCNTC                ; Check for Ctrl-C
    call    FRMNUM                ; Evaluate Conditional
    pop     de                    ; DE = CndPtr
    rst     FSIGN                 ; If A <> 0
    ret     nz                    ;   Pause is done
    ex      de,hl                 ; HL = CndPtr
    jr      .loop                 ; and try again

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
    SYNCHKT XTOKEN
    cp      PALETK
    jp      z,ST_RESET_PALETTE
    cp      BORDTK
    jp      z,ST_RESET_BORDER
    cp      SPRITK
    jp      z,ST_RESET_SPRITE
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
    SYNCHKT XTOKEN
    cp      CHRTK
    jp      z,ST_USECHR           ; USE CHRSET
    jp      SNERR


;-----------------------------------------------------------------------------
; VARDEF Function
; Syntax: VARDEF(*arrayname)
; Returns: -1 if array defined, else false
;-----------------------------------------------------------------------------
;; PRINT VARDEF(*A)
;;; ToDo: Add support for simple variables
FN_VARDEF:
    rst     CHRGET                ; Skip DEF
    SYNCHKC '('                   ; Require (
    SYNCHKT MULTK                 ; Require *
    ld      a,(BASYSCTL)
    or      BASVARDEF             ; Set Called by PTRGET flag
    ld      (BASYSCTL),a
    call    DIM1                  ; Search for array
    ld      b,a                   ; B = Found
    SYNCHKC ')'                   ; Require )
    ld      a,(BASYSCTL)
    and     $FF-BASVARDEF         ; Clear Called by PTRGET flag
    ld      (BASYSCTL),a
    ld      a,0
    ld      (VALTYP),a
push_hl_labbck_floats_b:
    ld      a,b                   ; A = Found
    jp      push_hl_labbck_float_sbyte

isary_hook:
    jp      z,ISARY
    ld      a,(BASYSCTL)
    and     BASVARDEF             ; If called from VARDEF
    jp      nz,ERSFIN             ;    Search for arry
    jp      NOARYS                ; Else continue on

dderr_hook:
    jp      z,ISARY
    ld      a,(BASYSCTL)
    and     BASVARDEF             ; If not called from VARDEF
    jp      z,DDERR               ;   Redimensioned Variable Error
    pop     hl                    ; Discard # of Dimensions
    pop     hl                    ; Restore TxtPtr  
    or      $FF                   ; Return $FF
    ret

notfdd_hook:
    ld      a,(BASYSCTL)
    and     BASVARDEF             ; f not called from VARDEF
    ld      de,4                  ;   Replaced with JP notfdd_hook
    jp      z,NOTFD1              ;   Dimension the variable
    pop     hl                    ; Discard # of Dimensions
    pop     hl                    ; Restore TxtPtr      
    xor     a                     ; Return 0                   
    ret      

;-----------------------------------------------------------------------------
; VER Function
; Syntax: VER$(x)
; x = 0 for System, 1 for plusBASIC, 2 for S3 BASIC
;-----------------------------------------------------------------------------
FN_VER:
    inc     hl                    ; Skip VER
    ld      a,(hl)                ; Get following character
    cp      '$'                   ; StrFlg = 0 if StrFn, else StrNum
    push    af                    ; Stack = StrFlg, RtnAdr
    call    z,CHRGTR              ; If String, Skip $
    call    PARTYP                ; FACC = Arg, A = ArgTyp
    pop     af                    ; AF = StrFlg
    call    push_hl_labbck        ; Stack = LABBCK, TxtPtr, RtnAdr
    ld      iy,aux_ver
    call    aux_call              ; A = StrFlg, HL = StrAdr or CDE = SrtHsh
    jp      z,TIMSTR              ; If VER$(), return version string
    jp      FLOAT_CDE             ; Else return SrtHsh

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
    call    FRMPRT                ; A = Type
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
    SYNCHKC ')'
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
    SYNCHKC ')'                   ; Require )
    call    push_hl_labbck        ; Stack = LABBCK, TxtPtr, RtnAdr
    call    FRCINT                ; DE = ArgVal
    jp      FLOAT_DE

str_word:
    rst    CHRGET                ; Skip $
    call   PARCHK
    push   hl                    ; Stack = TxtPtr. RtnAdr
    call   FRCINT                ; DE = ArgVal
; Enter with DE = word, HL on stack
ret_str_word:
    push   de                    ; Stack = ArgVal, TxtPtr, RtnAdr
    ld     a,2
    call   STRINI                ; Allocate 2 byte string
    pop    de                    ; DE = ArgVal; Stack = TxtPtr. RtnAdr
    ld     hl,(DSCTMP+2)         ; HL = StrAdr
    ld     (hl),e                ; Write ArgVal to string
    inc    hl
    ld     (hl),d
    jp     PUTNEW                ; and return it

;-----------------------------------------------------------------------------
; WRITE Statement stub
;-----------------------------------------------------------------------------
ST_WRITE:
    cp      '#'
    jp      z,write_file
    SYNCHKT XTOKEN
    rst     CHRGET               ; Skip XTOKEN
    cp      KEYTK
    jr      z,ST_WRITE_KEYS
    jp      SNERR

;-----------------------------------------------------------------------------
; WRITE KEYS Statement
; Syntax: WRITE KEYS string$
;-----------------------------------------------------------------------------
; WRITE KEYS \" ? 123\r"
;; ToDo: Find out why first character gets eaten
ST_WRITE_KEYS:
    rst      CHRGET                ; Skip KEY
    SYNCHKC  'S'                   ; Require S
    call     get_free_string       ; DE = StrAdr, BC = StrLen
    ld       iy,autokey_write_buffer
    call     aux_call
    jp       aux_call_popret       ; Write to auto-key buffer
