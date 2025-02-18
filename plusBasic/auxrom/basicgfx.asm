;====================================================================
; BASIC Graphics Statement and Function Code in GFX ROM Bank
;====================================================================

; Called from _def_sprite_rect
; On Entry HL = TxtPtr; Stack = DatLen, BufPtr, VarPtr, RtnAdr
bas_rectsprite:
    
    

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

; Called from FN_GETCHRDEF
; Input: B: ChrASCl
bas_getchrdef:
    push    bc
    ld      a,8                   ; A = BufLen
    call    STRINI                ; DE = BufAdr
    pop     af
    ld      bc,8                  ; BC = BufLen
    ld      l,0                   ; L = ChrSet
    jp      gfx_get_char_def

; Called from FN_GETCHRSET
; Output: ChrSet
bas_get_chrset:
    ld      a,(BASYSCTL)
    ld      b,a
    and     BASCHRMOD
    ld      a,-1
    ret     nz
    ld      a,b
    and     BASCHRSET             ; A = 0 if default character Set
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
    rst     SYNCHR
    byte    EQUATK                ; Require '='
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
   