;====================================================================
; BASIC Graphics Statements and Functions Core Code in GfxROM
;====================================================================

bas_bmp_fill_color:
    call    bitmap_get_mode       ; A = BmpMode
    ld      iy,bitmap_fill_color
    call    gfxrom_call           ; Do the fill
    ret     nc
    jp      TOERR
    

bas_check_bmp_mode:
    call    bitmap_get_mode       ; A = BmpMode
    cp      2                     ; If BmpMode = 2 (1bpp)
    ret     z                     ;   Return
    jp      IMERR                 ; Else Invalid mode error

; On entry, A = ValTyp, FACC = attribute argument
; Returns A = Attributes byte
bas_convert_attr:
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

; Called from FN_GETCHRDEF
bas_getcolor:
    ld      a,(SCREENCTL)         ; 
    rla                           ; Carry = SCRCOLOR
    ld      ix,float_minus_one
    ret     nc
    ld      a,(SCOLOR)
    ld      ix,SNGFLT
    ret

; Called from FN_GETCHRDEF
; Input: B: ChrASCl
bas_getchrdef:
    push    bc                    ; Stack = ChrASC, RtnAdr
    push    hl                    ; Stack = ChrSet, ChrASC, RtnAdr
    ld      a,8                   ; A = BufLen
    call    STRINI                ; DE = BufAdr
    pop     hl                    ; L = ChrSet; Stack = ChrASC, RtnAdr
    pop     af                    ; A = ChrASC; Stack = RtnAdr
_getchrdef:
    ld      bc,8                  ; BC = BufLen
    jp      gfx_get_chrdef

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

; Called from ST_RESET_CHR
; On entry: A = ChrASC
bas_reset_chrdef:
    push    hl                    ;  Stack = TxtPtr
    ld      bc,8
    ld      de,FBUFFR
    call    gfx_reset_chrdef
    jp      c,LSERR
    pop     hl
    ret

; Called from ST_SET_CHR
; On entry: A = ChrASC, E = ChrSet, HL = TxtPtr
bas_set_chr:
    push    hl
    call    gfx_set_chrdef
    jp      c,LSERR
    ld      a,(SCREENCTL)         ; Set character set modified flag
    or      a,SCRCHRMOD
    ld      (SCREENCTL),a
    pop     hl
    ret

; On entry A =  le 1, E = Spritle 2
bas_spritlecol:
  cp        64                    ; If Spritle# > 63
  jp        nc,FCERR              ;   Illegal quantity error
  ld        b,a                   ; B = Spritle 1
  ld        a,e                   ; A = Spritle 2
  cp        64                    ; If Spritle# > 63
  jp        nc,FCERR              ;   Illegal quantity error
  ld        c,a                   ; C = Spritle 2
  call      spritle_collision     ; Carry Set if collision
  ld        a,0                   
  sbc       0                     ; If Carry Set return -1
  ret

; On entry DE = SprDsc1, FACC = SprDsc2
bas_spritecol:
  push      de                    ; Stack = SprDsc1
  ex        de,hl                 ; HL = SprDsc1
  call      string_addr_len       ; DE = SprDef1
  jp        z,ESERR               ; Empty string error if StrLen = 0
  push      de                    ; Stack = SprDef1, SprDsc1, RtnAdr
  call      free_addr_len         ; DE = SprDef2
  jp        z,ESERR               ; Empty string error if StrLen = 0
  ex        de,hl                 ; HL = SprDef2
  pop       de                    ; DE = SprDef1; Stack = SprDsc1, RtnAdr
  call      sprite_collision      ; Set Carry if collision
  ld        a,0
  sbc       0
  pop       hl                    ; HL = SprDsc1; Stack = RtnAdr
  push      af                    ; Stack = Result, RtnAdr
  call      FRETM2                ; Free SprDsc1
  xor       a
  ld        (VALTYP),a            ; Set return type to numeric
  pop       af                    ; F = Result; Stack = RtnAdr
  ret

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
; Input: A = GETSPRITE suffix, FACLO = ArgDsc
; Output: HL = ResDsc
bas_getsprite:
    cp      '$'
    jr      nz,.getspritle        ; If GETSPRITE$
    call    free_addr_len         ;   HL = SprPtr. BC = SprLen, DE = SprAdr
    push    de                    ;   Stack = SprAdr, DummyAdr, TxtPtr, RtnAdr
    ld      a,(de)                ;   A = SptlCnt
    ld      d,a
    add     a                     ;   x 2
    add     a                     ;   x 4
    add     d                     ;   x 5
    jp      c,LSERR               ;   Error if too long
    call    STRINI                ;   Create BufStr; HL = BufDsc, DE = BufAdr
    call    string_addr_len       ;   BC = BufLen, DE = BufAdr
    ex      (sp),hl               ;   HL = SprAdr; Stack = BufDsc, DummyAdr, TxtPtr, RtnAdr
    call    sprite_get_attrs
    jp      nz,OVERR              ;   Sprite and Buffer Size Mismtch
    call    VALSTR                ;   Set Type to String
    call    FRETM2
    pop     hl                    ;   HL = BufDsc; Stack = DummyAdr, TxtPtr, RtnAdr
    ld      ix,FINBCK             ;   Return String
    ret                           ; Else
.getspritle
    cp      '('                   ;   If GETSPRITE()
    jr      nz,.notparen
    call    .dospritle
    ld      ix,FLOAT_HL           ;     Return Attrs+Tile#
    ret                           ;   Else
.notparen
    push    af                    ;     Stack = FncSfx, RtnAdr
    call    .getpos               ;     BC = Xpos, DE = Ypos
    pop     af                    ;     A = FncSfx; Stack = RtnAdr
    ld      ix,FLOAT_BC
    cp      'X'                   ;     If GETSPRITEX()
    ret     z                     ;       Return X-Position
    ld      ix,FLOAT_DE
    cp      'Y'                   ;     Else If GETSPRITEY()
    ret     z                     ;       Return Y-Position
    jp      SNERR                 ;     Else Syntax error

; Returns BC = Xpos, DE = Ypos
; Clobbers: A, HL
.getpos
    call    GETYPE
    jr      nz,.dospritle         ; If argument is string
    call    free_addr_len         ;   DE = SprAdr, A, BC = SprLen
    cp      6                     ;   If SprLen < 6
    jp      c,slERR               ;     String length error
    ex      de,hl                 ;   HL = SprAdr
    jp      sprite_get_pos        ;   Return BC = Xpos, DE = Ypos
; Returns BC = Xpos, DE = Ypos; HL = Attrs
; Clobbers: A
.dospritle
    call    CONINT                ; A, E = SptNum
    cp      64                    ; If > 63
    jp      nc,FCERR              ;   Illegal quantity error
    jp      spritle_get_attrs     ; Return BC = Xpos, DE = Ypos, HL = Attrs+Tile#


; Will be called from FN_PALLETE
; On entry DE = Palette#, 
bas_getpalette:
    ld      bc,palette_get
    jr      _get_gfx_string
; Called from FN_GETTILE
; On entry DE = Tile#, 
bas_gettile:
    ld      bc,tile_get
; Builds 32 byte temporary string and populates with 
_get_gfx_string:
    push    bc                    ; Stack = GfxCall
    push    de                    ; Stack = Tile/Palt, GfxCall, RtnAdr
    ld      a,32                  ; Reading 32 bytes
    call    STRINI                ; Create TmpStr; HL = StrDsc, DE = StrAdr
    ld      bc,32                 ; BC = StrLen
    pop     hl                    ; HL = Tile/Palt; Stack = GfxCall, RtnAdr
    call    VALSTR                ; Set return type to string
    ld      a,l                   ; A = Palette# (for palette_get)
    ret                           ; Jump to GfxCall and return

; Called from ST_SET_TILE
; Input: A: AryTyp, DE: DatAdr, BC: DatLen
bas_set_tile_ary:
    jp      nz,TMERR              ; Error if not string array
    ex      de,hl                 ; HL = DatAdr
.loop
    ld      a,  b
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

; Called from ST_SET_TILE
; Input: BC: StrLen, DE: StrAdr, HL: TilIdx
bas_set_tile_str:
    push    hl                    ; Stack = TileIdx, RtnAdr
    call    free_addr_len         ; DE = DataAddr, BC = Count
    pop     hl                    ; HL = TileIdx; Stack = RtnAdr
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

; Called from LOAD TILESET
; Input: HL = Text Pointer
; Output: BC = Base Index
;         DE = 0
bas_index_offset:
    ld      de,0                  ; Offset = 0
    ld      bc,0                  ; BaseIdx = 0
    push    hl                    ; Stack = TxtPtr, RtnAdr
    call    .index                ; If not INDEX
    call    nz,.offset            ;   BC = BaseIdx if OFFSET
    jp      nz,POPHRT             ; Return NZ if neither INDEX or OFFSET
    pop     de                    ; Stack = RtnAdr
    ret
.index:
    cp      INTK                  ; If not INDEX
    ret     nz                    ;   Return Zero Clear
    rst     CHRGET
    cp      XTOKEN
    ret     nz    
    rst     CHRGET
    cp      DEXTK 
    ret     nz    
    jr      .done                 ; Else Return Zero Set
.offset:
    cp      XTOKEN                ; If not OFFSET 
    ret     nz                    ;   Return Zero Clear
    rst     CHRGET
    cp      OFFTK
    ret     nz
    rst     CHRGET
    cp      SETTK
    ret     nz                    ; Else
    ld      a,$FF                 ;   A = UseCurMode
    call    tile_offset           ;   BC = BaseIdx
.done
    rst     CHRGET
_xor_a_ret:
    xor     a
    ret

; On entry: A = ChrASC, BC = Colors, HL = Tile#
bas_set_tile_to_chr:
    push    hl                    ; Stack = Tile#, RtnAdr
    call    get_strbuf_addr_no_bc ; HL = StrBuf
    ex      de,hl                 ; DE = StrBuf, HL = Colors
    ld      iy,tile_from_chrrom   ; Build tile in String Buffer
    call    gfxrom_call           ; DE = StrBuf, BC = DatLen
    ld      iy,tile_set
    pop     hl                    ; HL = Tile#; Stack = RtnAdr
    jp      tile_set              ; Write the tile

; Called from FN_POS
; Input: A = SfxChr; Output: DE = Result
bas_pos:
    push    af                    ; Stack = SfxChr, RtnAdr
    call    bitmap_read_sysvars   ; B = BmpClr, C = BmpY, DE = BmpX
    pop     af                    ; A = SfxChr; Stack = RtnAdr
    cp      'X'                   ; If POSX
    ret     z                     ;   Return X
    cp      'Y'
    jp      nz,SNERR
    ld      d,0
    ld      e,c
    ret

; Called from FN_INRECT
; Input: BC:X, DE: Y, HL: StrDsc
; Output: A: Index
bas_inrect:
    push    bc                    ; Stack = X, RtnAdr
    push    de                    ; Stack = Y, X, RtnAdr
    call    free_hl_addr_len      ; DE = StrAdr, BC = Strlen
    jp      z,ESERR               ; If StrLen = 0, Empty string error
    and     $07                   ; If StrLen not multiple of 8
    jp      nz,SLERR              ;   String length error
    ld      a,c                   ; A = StrLen
    rra
    rra
    rra                           ; A = RectCnt
    ex      de,hl                 ; HL = StrAdr
    pop     de                    ; DE = Y; Stack = X, RtnAdr
    pop     bc                    ; BC = Y; Stack = RtnAdr
    jp      gfx_in_rectlist

; Called from ST_RECT
bas_rect:
    ld      hl,(STRDSC2)          ; HL = BoxClrDsc
    call    .color_string
    push    de                    ; Stack = BoxClrAdr, RtnAdrs
    ld      hl,(STRDSC1)          ; DE = BoxChrDsc
    ld      de,boxdraw_text
    inc     h
    dec     h
    call    nz,.free_desc         ; DE = BoxChrAdr
    ex      de,hl                 ; HL = BoxChrAdr
    exx                           ; HL' = BoxChrAdr
    ld      bc,(RECT_X2)          ; BC = X2
    ld      de,(RECT_Y2)          ; DE = Y2
    pop     hl                    ; HL = BoxClrAdr
    exx                           ; HL = BoxChrAdr, BC' = X2, DE' = Y2, HL' = BoxClrAdr
    ld      bc,(RECT_X1)          ; BC = X1
    ld      de,(RECT_Y1)          ; DE = Y1
    xor     a                     ; A = Default Screen
    call    screen_rect
    jp      m,IMERR
    jp      c,FCERR
    ret
.free_desc
    call    free_hl_addr_len      ; DE = StrAdr, A = StrLen
    cp      9                     ; If StrLen <> 9
    jp      nz,SLERR              ;   String Length Error
    ret
.color_string
    inc     h
    dec     h                     ; IF HL > 255
    jr      nz,.free_desc
    ld      a,e                   ; A = Colors
    or      a                     ; If Colors = 0,0
    call    z,screen__colors      ;   A = Default colors
    ld      de,FBUFFR
    push    de
    ld      b,9
.loop    
    ld      (de),a
    inc     de
    djnz    .loop
    pop     de
    ret

; Core code for RGBR(), RGBG(), RGB(B)
; On entry: A = Function Suffix, D = Red, E = Green+Blue
; On Exit: DE = Result
bas_rgbx:
    cp      '('                   ; If no suffix
    ret     z                     ;   Return RGB value
    ex      de,hl                 ; H = Red, L = Green+Blue
    ld      d,0                   ; Clear result MSB
    cp      'R'                   ; If RGBR()
    jr      z,.red                ;   Return Red component
    cp      'G'                   ; If RGBG()
    jr      z,.green              ;   Return Green component
    cp      'B'                   ; If RGBB()
    jr      z,.blue               ;   Return Red component
    jp      SNERR                 ; Else Syntax Error
.red
    ld      a,h
    and     $0F
    ld      e,a
    ret
.green
    ld      a,l
    and     $F0
    rra
    rra
    rra
    rra
    ld      e,a
    ret
.blue
    ld        a,l
    and       $0F
    ld        e,a
    ret


bas_fill_colormap:
    ex      de,hl                 ; H = 0, L = Colors
    call    _read_rect;           ; BC = X2, DE = Y2, BC' = X2, DE' = Y2
    xor     a
    jp      colormap_fill_rect

_read_rect:
    exx
    ld      bc,(RECT_X2)          ; BC = X2
    ld      de,(RECT_Y2)          ; DE = Y2
    exx                           ; BC' = X2, DE' = Y2
    ld      bc,(RECT_X1)          ; BC = X1
    ld      de,(RECT_Y1)          ; DE = Y1
    ret

; ST_FILL_SCREEN core code
; BC: ScrFlg+ScrChr, DE: ClrFlg+ScrClr
; RECT_X1, RECT_Y1, RECT_X2, RECT_Y2 = Coordinates
bas_fill_screen:
    ld      a,b                   ; If no CHR
    or      d                     ;   or COLOR
    jp      z,MOERR               ;   Missing operand error
    push    de                    ; Stack = ClrFlg+ScrClr, RtnAdr
    inc     b                     ; If ScrFlg
    call    z,.fill_chr           ;   Fill Screen with CHR
    jp      c,FCERR
    pop     de                    ; BC = ClrFlg+ScrClr; Stack = RtnAdr
    inc     d                     ; If ClrFlg
    call    z,.fill_color         ;   Fill Screen with COLOR
    jp      m,IMERR
    jp      c,FCERR
    ret
.fill_chr
    ld      l,c                   ; L = ScrChr
    call    _rect_coords          ; A = Screen#, C = X1, D = Y1, C' = X
    jp      screen_fill           ; Fill Screen
.fill_color
    ld      l,e                   ; L = ScrClr
    call    _rect_coords          ; A = Screen#, C = X1, D = Y1, C' = X
    jp      color_fill            ; Fill Screen Color RAM

_rect_coords
    xor     a                     ; Use Current Screen  
    ld      bc,(RECT_X1)
    ld      de,(RECT_Y1)
    exx
    ld      bc,(RECT_X2)
    ld      de,(RECT_Y2)
    exx
    ret

bas_frect:
    ld      a,(PSETOPT)
    or      a                     ; If LinOpt < 128
    call    p,.color_frect        ;   Fill ColorMap Rectangle
    call    _rect_coords          ; A = Screen#, C = X1, D = Y1, C' = X1, D' = Y1, Carry Clear       
    ld      a,(PSETOPT)
    ld      h,a
    ld      l,0
    call    bitmap_frect
    jr      .done
.color_frect
    cp      16
    jp      nc,FCERR
    call    _rect_coords          ; A = Screen#, C = X1, D = Y1, C' = X1, D' = Y1, Carry Clear
    ld      hl,(PSETOPT)          ; L = Color
    ld      h,0
    rl      l                     ; A = Color * 2
    rl      l                     ; A = Color * 4
    rl      l                     ; A = Color * 8
    rl      l                     ; A = Color * 16
    ld      h,$0F                 ; AndMsk = FgColor Only
    call    colormap_fill_rect    ; Fill Colormap Rectangle
.done
    jp      m,IMERR
    jp      c,FCERR
    ret
    
; Called from ST_PUT_CHR
; On entry, A = ChrASC, BC = X, DE = Y
bas_put_chr:
    push    bc                    ; Stack = X, RtnAdr
    push    de                    ; Stack = Y, X, RtnAdr
    ld      de,FBUFFR             ; DE = BufAdr
    push    de                    ; Stack = BufAdr, Y, X, RtnAdr
    ld      l,0                   ; L = ChrSet
    call    _getchrdef            ; Copy ChrData to buffer
    pop     hl                    ; HL = BufAdr, Stack = Y, X, RtnAdr
    pop     de                    ; DE = X; Stack = Y, RtnAdr
    pop     bc                    ; BC = Y; Stack = RtnAdr
    jp      bitmap_drawchr        ; Put char to bitmap and return
