;====================================================================
; BASIC Graphics Statement and Function Code in GFX ROM Bank
;====================================================================


; Called from FN_CURSOR
; On Entry B = CURSOR suffix
bas_cursor:
    push    bc                    ; Stack = Suffix, RtnAdr
    call    cursor_location       ; B = Xpos, C = Ypos, DE = Offset
    pop     af                    ; A = Suffix; Stack = RtnAdr
    cp      XTOKEN                ; If Suffix = OFFSET
    ret     z                     ;   Return Offset
    ld      d,0                   ; ResMSB = 0
    cp      'X'                   ; If Suffix = X
    ld      e,b
    ret     z                     ;   Return XPOS
    cp      'Y'                   ; If Suffix = Y
    ld      e,c
    ret     z                     ;   Return YPOS
    jp      SNERR                 ; Else Syntax error

; Called from ST_RESET_SPRITE
; On Entry A = ValTyp, DE: SptNum or VarPtr, HL = TxtPtr
bas_reset_sprite:
    jr      nz,.notbyte           ; If Numeric
    ld      a,e                   ;   A = SptNum
    cp      64                    ;   If > 63
    jp      nc,FCERR              ;     Illegal Quantity error
    jp      spritle_reset         ;   Reset spritle
.notbyte
    cp      MULTK                 ; If *
    jp      z,spritle_reset_all   ;   Reset all spritles
    push    hl                    ; Stack = TxtPtr, RtnAdr
    ex      de,hl                 ; HL = VarPtr
    call    string_addr_len       ; C = StrLen, DE = StrAdr
    jp      z,ESERR               ; If StrLen = 0, Empty String Error
    call    sprite_reset
    jp      c,SLERR
    pop     hl
    ret

; Called from _get_rgb
; Input: A: Green, B: Red, E:Blue
bas_make_rgb:
    and     $0F                   ; Shift Green to high nybble
    rla
    rla
    rla
    rla
    or      e                     ; A = Green + Blue
    ld      e,a                   ; E = Green + Blue
    ld      d,b                   ; D = Red
    ret
bas_rgb_string:
    push    hl                    ; Stack = TxtPtr, RtnAdr
    call    free_addr_len         ; DE = StrAdr, A = StrLen
    pop     hl                    ; HL = TxtPtr, Stack = RtnAdr
    jp      z,ESERR               ; If StrLen = 0, Empty string error
    cp      3                     ; If StrLen < 3
    jp      c,SLERR               ;   String length error
    jr      nz,.rgb_string        ; If
    call    .get_nybble           ;   A = Red
    push    af                    ;   Stack = Red, RtnAdr
    call    .get_nybble           ;   A = Green
    push    af                    ;   Stack = Green, Red, RtnAdr
    call    .get_nybble           ;   A = Blue
    ld      e,a                   ;   E = Blue
    pop     af                    ;   A = Green
    pop     bc                    ;   B = Red
    jr      bas_make_rgb          ;   Return RGB in DE
.rgb_string
    push    hl                    ; Stack = TxtPtr, RtnAdr
    call    bytes_to_fbuffr       ; Copy String to FBUFFR
    jp      c,SLERR               ; Error if string too long
    call    dec_to_rgb            ; BC = RGB
    pop     hl                    ; HL = TxtPtr, Stack = RtnAdr
    jp      c,FCERR               ; Error if not "rrr ggg bbb"
    ld      d,b
    ld      e,c                   ; Return RGB in DE
    ret

.get_nybble
    ld      a,(de)                ; A = Color Component
    inc     de
    call    aux_cvt_hex           ; Convert Hex digit to nybble and return
    jp      c,FCERR
    ret

; Called from _rgbdec:
; On entry: C = Delmtr, DE = RgbDsc
; On exit: HL = BufAdr, A = StrLen
bas_rgbdec:
    ld      a,c                   ; A = Delmtr
    push    af                    ; Stack = Delmtr, RtnAdr
    ex      de,hl
    call    free_hl_addr_len      ; DE = StrAdr, A = StrLen
    cp      2                     ; If StrLen <> 2
    jp      nz,SLERR              ;   Strnig length error
    ld      hl,FBUFFR             ; HL = BufAdr
    pop     bc                    ; B = Delmtr; Stack = RtnAdr
    push    hl
    call    rgb_dec
    pop     hl
    ret

;Input: BC: StrLen, DE: StrAdr
;Output: HL: FBUFFR
;Clobbers: A, BC, DE
bytes_to_fbuffr:
    ld      a,c
    cp      14
    ccf                           ;If StrLen >= 14
    ret     c                     ;  Return Carry set
    ld      hl,FBUFFR
    push    hl                    ; Stack = FBUFFR, RtnAdr
    ex      de,hl                 ; DE = FBUFFR, HL = StrBuf
    ldir                             ; Copy string
    xor     a                     ; Clear carry
    ld      (de),a                ; Write delimiter
    pop     hl                    ; HL = FBUFFR, Stack = RtnAdr
    ret

; Called from _def_sprite_string
; On Entry HL = TxtPtr; Stack = DatLen, BufPtr, VarPtr, RtnAdr
bas_strsprite:
    pop     ix                    ; IX = RtnAdr; Stack = DatLen, BufPtr, VarPtr, RtnAdr
    call    skip_get_stringvar    ; DE = ArgPtr
    pop     bc                    ; Stack = BufPtr, VarPtr, RtnAdr
    ex      (sp),hl               ; HL = BufPtr, Stack = TxtPtr, VarPtr, RtnAdr
    push    hl                    ; Stack = BufPtr, TxtPtr, VarPtr, RtnAdr
    ex      de,hl                 ; HL = ArgPtr
    call    string_addr_len       ; DE = ArgAdr, BC = ArgLen
    pop     hl                    ; HL = BufPtr, Stack = TxtPtr, VarPtr, RtnAdr
    call    sprite_define         ; HL = BufPtr, BC = DatLen
    jp      c,FCERR
    ex      (sp),hl               ; HL = TxtPtr, Stack = BufPtr, VarPtr, RtnAdr
    push    bc                    ; Stack = DatLen, BufPtr, VarPtr, RtnAdr
    jp      (ix)                  ; Return


; Called from FN_GETSPRITE
; Input: FACLO = ArgDsc
; Output: HL = ResDsc
bas_getsprite:
    call    CHKSTR
    call    faclo_addr_len        ; HL = SprPtr. BC = SprLen, DE = SprAdr
    push    de                    ; Stack = SprAdr, DummyAdr, TxtPtr, RtnAdr
    ld      a,(de)                ; A = SptlCnt
    ld      d,a
    add     a                     ; x 2
    add     a                     ; x 4
    add     d                     ; x 5
    jp      c,LSERR               ; Error if too long
    ld      ix,sprite_get_attrs
    call    STRINI                ; Create BufStr; HL = BufDsc, DE = BufAdr
    call    string_addr_len       ; BC = BufLen, DE = BufAdr
    ex      (sp),hl               ; HL = SprAdr; Stack = BufDsc, DummyAdr, TxtPtr, RtnAdr
    call    jump_ix
    jp      nz,OVERR              ; Sprite and Buffer Size Mismtch
    ld      a,1
    ld      (VALTYP),a            ; Set Type to String
    call    FRETM2
    pop     hl                    ; HL = BufDsc; Stack = DummyAdr, TxtPtr, RtnAdr
    ret

; Called from ST_PUT_CHR
; On entry, A = ChrASC, BC = Col, DE = Row
bas_put_chr:
    push    bc                    ; Stack = Col, RtnAdr
    ex      de,hl                 ; HL = Row
    add     hl,hl                 ; HL = Row * 2
    add     hl,hl                 ; HL = Row * 4
    add     hl,hl                 ; HL = Row * 6
    push    hl                    ; Stack = Line, Col, RtnAdr
    ld      de,FBUFFR             ; DE = BufAdr
    push    de                    ; Stack = BufAdr, Row, Col, RtnAdr
    call    _getchrdef            ; Copy ChrData to buffer
    pop     hl                    ; HL = BufAdr, Stack = Row, Col, RtnAdr
    pop     de                    ; DE = Line; Stack = Col, RtnAdr
    pop     bc                    ; BC = Col; Stack = RtnAdr
    jp      bitmap_put_char       ; Put char to bitmap and return

; Called from FN_GETCHRDEF
; Input: B: ChrASCl
bas_getchrdef:
    push    bc
    ld      a,8                   ; A = BufLen
    call    STRINI                ; DE = BufAdr
    pop     af
_getchrdef
    ld      bc,8                  ; BC = BufLen
    ld      l,0                   ; L = ChrSet (CharRAM)
    jp      gfx_get_char_def

; Called from FN_GETCHRSET
; Output: ChrSet
bas_get_chrset:
    ld      a,(SCREENCTL)
    ld      b,a
    and     SCRCHRMOD
    ld      a,-1
    ret     nz
    ld      a,b
    and     SCRCHRSET             ; A = 0 if default character Set
    ret     z
    ld      a,1                   ;   A = 1
    ret

; Called from SCREEN
bas_do_gfx_mode:
    cp      4
    jp      nc,FCERR              ; Error if > 3
    add     a,a                   ; Shift 1 bit left
    ld      b,a                   ; B = Gfx Mode
    ld      a,~6                  ; A = Mask
    jr      .do_bit
.update_bit
    ld      a,b                   ; A = Option bit
    jr      nz,.not_zero          ; If operand is zero
    ld      b,0                   ;   Clear Option bit
.not_zero
    xor     $FF                   ; A = Bit Mask
.do_bit
    and     c                     ; A = Masked VCTRL
    or      b                     ; A = New VCTRL
    ld      c,a                   ; C = New VCTRL
    ret

bas_defgotvar:
    pop     bc                    ; BC = RtnAdr
    push    de                    ; VarAdr
    push    hl                    ; Stack = TxtPtr, VarAdr
    push    bc                    ; Stack = RtnAdr, TxtPtr, VarAdr
    call    get_strbuf_addr       ; HL = BufAdr
    pop     bc                    ; BC = RtnAdr; Stack = TxtPtr, VarAdr
    ex      (sp),hl               ; HL = TxtPtr; Stack = BufAdr, TxtPtr, VarAdr
    xor     a                     ; DatLen = 0
    push    af                    ; Stack = DatLen, BufAdr, VarPtr
    push    bc                    ; Stack = RtnAdr, DatLen, BufAdr, VarPtr
    SYNCHKT EQUATK                ; Require '='
    ret

bas_parse_attr:
    call    GETYPE
    jr      z,.string             ; If numeric
    call    CONINT                ;   Convert to byte in A
    and     $7E                   ; Strip Index MSB
    ret
.string
    push    hl
    xor     a
    ex      af,af'                ; AF' = Properties
    call    free_addr_len         ; DE = StrAdr, C = StrLen
.loop
    jr      z,.done               ; Pop HL and return if end of string
    ld      a,(de)                ; A = PropChar
    inc     de                    ; Bump StrPtr
    and     $5F                   ; Convert to upper case
    ld      b,SPR_HFLIP
    cp      'H'
    jr      z,.set
    ld      b,SPR_VFLIP
    cp      'V'
    jr      z,.set
    ld      b,SPR_HEIGHT
    cp      'D'
    jr      z,.set
    ld      b,SPR_PRIORITY
    cp      'P'
    jr      z,.set
    jp      FCERR
.set
    ex      af,af'
    or      a,b
    ex      af,af'
.next
    dec     c
    jr      .loop
.done
    ex      af,af'
    ld      e,a
    pop     hl
    ret

; Called from ST_SET_TILE
; Input: BC: StrLen, DE: StrAdr, HL: TilIdx
bas_set_tile_str:
    ld      a,b
    cp      0
    ret     nz
    ld      a,c
    cp      32
    jr      z,.settile
    cp      64
    ret     nz
    push    hl                    ; Stack = TilIdx, RtnAdr
    push    bc                    ; Stack = StrLen, TilIdx, RtnAdr
    call    get_strbuf_addr       ; HL = BinAdr
    pop     bc                    ; BC = StrLen, Stack = TilIdx, RtnAdr
    ld      iy,aux_hex_to_asc
    call    aux_call              ; BC = BinLen, HL = BinAdr
    ex      de,hl                 ; DE = BinAdr
    pop     hl                    ; HL = TilIdx; Stack = RtnAdr
.settile
    call    tile_set
    ret     c                     ; Return Carry set if overflow
    xor     a                     ; Clear C, set Z flags
    ret

; Called from ST_SET_TILE
; Input: A: AryTyp, DE: DatAdr, BC: DatLen
bas_set_tile_ary:
    jp      nz,TMERR              ; Error if not string array
    ex      de,hl                 ; HL = DatAdr
.loop
    ld      a,b
    or      c
    ret     z                     ; End of array, return Z
    ld      a,(hl)                ; A = StrLen
    inc     hl
    inc     hl                    ; Skip unused byte
    ld      e,(hl)
    inc     hl
    ld      d,(hl)                ; DE = StrAdr
    inc     hl
    or      a
    jr      z,.next               ; If StrLen <> 0
    cp      34                    ;   If StrLen <> 34
    ret     nz                    ;     Return NZ
    push    bc                    ;   Stack = AryCnt, RtnAdr
    push    hl                    ;   Stack = AryPtr, AryCnt, RtnAdr
    ld      bc,32                 ;   BC = TilLen
    ex      de,hl                 ;   HL = TilAdr
    ld      e,(hl)
    inc     hl
    ld      d,(hl)                ;   DE = TilIdx
    inc     hl
    ex      de,hl                 ;   DE = TilAdr, HL = TileIdx
    call    tile_set              ;   Write Tile Data
    pop     hl                    ;   HL = AryPtr; Stack = AryCnt, RtnAdr
    pop     bc                    ;   BC = AryCnt; Stack = RtnAdr
.next
    dec     bc
    dec     bc
    dec     bc
    dec     bc
    jr      .loop

