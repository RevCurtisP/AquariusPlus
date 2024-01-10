;====================================================================
; Graphics Statements and Functions
;====================================================================


;-----------------------------------------------------------------------------
; USE CHRSET - Change Character Set
; Syntax:
;-----------------------------------------------------------------------------
ST_USECHR:
    rst     CHRGET                ; Skip CHR
    rst     SYNCHR                ; Require SET
    byte    SETTK
    call    GETBYT                ; Evaluate Argument
    cp      3
    jp      nc,FCERR
    push    hl                    ; Stack = TxtPtr, RtnAdr
    call    select_chrset         ;
    pop     hl                    ; HL - TxtPtr; Stack = RtnAdr
    ret

;-----------------------------------------------------------------------------
; SET COLOR statement
; syntax: SET COLOR foreground, background
;-----------------------------------------------------------------------------
ST_SETCOLOR:
    rst     CHRGET                ; Skip CHR
    rst     SYNCHR                ; Require OR
    byte    ORTK
    cp      XTOKEN
    jr      z,.extended
    call    get_color_args
    ld      (SCOLOR),a
    ld      a,(BASYSCTL)
    or      a,BASCRNCLR
    jr      .done
.extended
    rst     CHRGET                ; Skip XTOKEN
    rst     SYNCHR                ; Require OFF
    byte    OFFTK
    ld      a,(BASYSCTL)
    and     low(~BASCRNCLR)
.done
    ld      (BASYSCTL),a
    ret

;-----------------------------------------------------------------------------
; RESET PALETTE to default colors
; syntax: RESET PALETTE palette#
;-----------------------------------------------------------------------------
ST_RESET_PALETTE:
    rst     CHRGET                ; Skip PALETTE
    call    get_byte4             ; A = palette#
    push    hl                    ; Stack = TxtPtr, RtnAdr
    ld      iy,palette_reset
    call    aux_call
    pop     hl
    ret

;-----------------------------------------------------------------------------
; SET PALETTE statement
; syntax: SET PALETTE palette# [, index] TO rgblist$
;-----------------------------------------------------------------------------
; SET PALETTE 0 TO $"111122223333444455556666777788889999"
ST_SETPALETTE:
    rst     CHRGET                ; Skip PALETTE
    call    get_byte4             ; E = Palette#
    ld      c,0                   ; Default Entry# to 0
    ld      a,(hl)
    cp      INTK                  ; If followed by IN
    jr      nz,.not_index         ;
    rst     CHRGET                ;   Eat It
    rst     SYNCHR
    byte    XTOKEN
    rst     SYNCHR
    byte    DEXTK                 ;   Require DEX
    push    de                    ;   Stack = Palette#
    call    get_byte16            ;   Parse Byte between 0 and 15
    ld      c,e                   ;   C = Entry #
    pop     de                    ;   DE = Palette#
.not_index
    rst     SYNCHR
    byte    TOTK                  ; Require TO
    ld      a,e                   ; Get palette#
    cp      4                     ; If greater than 3
    jp      nc,FCERR              ;   Error
    ld      iy,mult_a_32          ;
    call    aux_call
    ld      b,a                   ; B = Shifted palette#
    push    bc                    ; Stack = PltOfs+Entry
    call    FRMEVL                ; Evaluate RGB list
    ex      (sp),hl               ; HL = PltOfs+Entry; Stack = TxtPtr
    push    hl                    ; Stack = PltOfs+Entry, TxtPtr
    call    free_addr_len         ; DE = DataAddr, BC = Count
    pop     hl                    ; HL = PltOfs+Entry; Stack = TxtPtr
    ld      a,l                   ; A = Entry#
    ld      l,h                   ; L = PltOfs
    scf                           ; Palette already shifted
    ld      iy,palette_set        ; Set the entry (increments C)
    call    aux_call
    jp      c,OVERR               ; Error if Overflow
    pop     hl                    ; HL = TxtPtr
    ret


;-----------------------------------------------------------------------------
; Proposed new syntax
; SCREEN SAVE|RESTORE|SWAP|RESET
; SCREEN [text],[graphics],[sprites],[priority],[remap]
;-----------------------------------------------------------------------------
ST_SCREEN:
    or      a                     ; If token
    jp      m,.is_token           ;   Process it

    push    hl
    ld      a,BUFSCRN40           ; Save current text screen to buffer
    ld      hl,SCRN40BUF
    ld      ix,screen_save
    call    aux_rom_call
    pop     hl

    in      a,(IO_VCTRL)
    ld      c,a                   ; C = Current VCTRL

    call    get_byte_optional     ; Do Text
    call    nc,.do_text_mode

    call    get_byte_optional     ; Do Graphics
    call    nc,.do_gfx_mode

    call    get_byte_optional     ; Do Sprites
    ld      b,VCTRL_SPR_EN
    call    nc,.update_bit

    call    get_byte_optional     ; Do Priority
    ld      b,VCTRL_TEXT_PRIO
    call    nc,.update_bit

    call    get_byte_optional     ; Do Border Remap
    ld      b,VCTRL_REMAP_BC
    call    nc,.update_bit

    ld      a,c
    out     (IO_VCTRL),a          ; Write back out

    push    hl
    ld      a,BUFSCRN40           ; Restore current text screen from buffer
    ld      hl,SCRN40BUF
    ld      ix,screen_restore        ;
    call    aux_rom_call
    pop     hl

    jp      set_linlen            ; Set TTYWID to screen columns and return

.is_token
    cp      SAVETK                ; If SAVE
    jr      z,ST_SCREEN_SAVE      ;   Do SCREEN SAVE
    cp      RESTK                 ; If RESTORE
    jr      z,ST_SCREEN_RESTORE   ;   Do SCREEN RESTORE
    cp      SWAPTK                ; If RESTORE
    jr      z,ST_SCREEN_SWAP      ;   Do SCREEN SWAP
    rst     SYNCHR
    byte    XTOKEN                ; Else
    rst     SYNCHR                ;  Require RESET
    byte    RESETK
    jr      _reset_screen

.do_text_mode:
    cp      4                     ; If > 3
    jp      nc,FCERR              ;   Illegal Quantity error
    ld      de,.text_mode_table
    call    table_lookup          ; Look up Text Mode bits
    ld      b,a                   ; B = Mode Bits
    ld      a,$3E                 ; A = Text Mode mask
    jr      .do_bit               ; Mask, combine, and return
.do_gfx_mode
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

.text_mode_table:
    byte    VCTRL_TEXT_OFF                  ; 0 = Text Off
    byte    VCTRL_TEXT_EN                   ; 1 = 40 Column Primary
    byte    VCTRL_TEXT_EN+VCTRL_TEXT_PAGE   ; 2 = 40 Column Secondary
    byte    VCTRL_TEXT_EN+VCRTL_80COL_EN    ; 3 = 80 Column

ST_RESET_SCREEN:
    rst     CHRGET                ; Skip SCREEN
_reset_screen:
    push    hl
    ld      iy,screen_reset
    call    aux_call
    pop     hl
    ret

;-----------------------------------------------------------------------------
; SCREEN RESTORE - Copy Screen Buffer to Text Screen
;-----------------------------------------------------------------------------
ST_SCREEN_RESTORE:
    rst     CHRGET                ; Skip RESTORE
    push    hl                    ; Stack = TxtPtr, RtnAdr
    ld      a,SWPSCRN40
    ld      hl,SCRN40SWP
    ld      ix,screen_restore
    call    aux_rom_call
    pop     hl                    ; HL = TxtPtr; HL = RtnAdr
    ret

;-----------------------------------------------------------------------------
; SCREEN SAVE - Copy Text Screen to Screen Buffer
;-----------------------------------------------------------------------------
ST_SCREEN_SAVE:
    rst     CHRGET                ; Skip SAVE
    push    hl                    ; Stack = TxtPtr, RtnAdr
    ld      a,SWPSCRN40
    ld      hl,SCRN40SWP
    ld      ix,screen_save
    call    aux_rom_call
    pop     hl                    ; HL = TxtPtr; HL = RtnAdr
    ret

;-----------------------------------------------------------------------------
; SCREEN SWAP - Swap Text Screen with Screen Buffer
;-----------------------------------------------------------------------------
ST_SCREEN_SWAP:
    rst     CHRGET                ; Skip SWAP
    push    hl                    ; Stack = TxtPtr, RtnAdr
    ld      hl,SCRN40SWP
    ld      ix,screen_swap
    call    aux_rom_call          ; Do the Copy
    pop     hl                    ; HL = TxtPtr; HL = RtnAdr
    ret

;-----------------------------------------------------------------------------
; SET TILE Statement
; SET TILE tile# TO tiledata$
;-----------------------------------------------------------------------------
ST_SET_TILE:
    rst     CHRGET                ; Skip TILE Token
    cp      MAPTK                 ; If TILEMAP
    jp      z,ST_SET_TILEMAP      ;   Go do that
    call    get_int512            ; Get Tile#
    push    de                    ; Stack = Tile#, RtnAdr
    rst     SYNCHR                ; Require TO
    byte    TOTK
    call    FRMEVL                ; Get DataAddr or String
    ex      (sp),hl               ; HL = Tile#; Stack = TxtPtr, RtnAdr
    push    hl                    ; Stack = Tile#, TxtPtr, RtnAdr
    call    free_addr_len         ; DE = DataAddr, BC = Count
    pop     hl                    ; HL = Tile#; Stack = TxtPtr, RtnAdr
    ld      iy,tile_set
    call    aux_call
    jp      c,OVERR               ; Error if Overflow
    pop     hl                    ; HL = TxtPtr
    ret

;-----------------------------------------------------------------------------
; FILL SCREEN [(col,row)-(col,row)] [CHR chr|chr$] [COLOR fgcolor, bgcolor]
;-----------------------------------------------------------------------------
ST_FILL_SCREEN:
    ld      iy,screen_size
    call    aux_call              ; Default to entire screen
    rst     CHRGET                ; Skip SCREEN
    cp      '('                   ; If open paren
    call    z,scan_rect           ;   B = BgnCol, C = EndCol, D = BgnRow, E= EndRow
.params
    push    de                    ; Stack = Rows, RtnAdr
    push    bc                    ; Stack = Cols, Rows, RtnAdr
    ld      a,(hl)                ; Reget next character
    cp      XTOKEN                ;
    jr      nz,.notx
    inc     hl
    rst     SYNCHR
    byte    CHRTK                 ; If CHR
    call    get_char              ;   Parse fill character
    pop     bc                    ;   BC = Cols; Stack = Rows, RtnAdr
    pop     de                    ;   DE = Rows; Stack = RtnAdr
    push    de                    ;   Stack = Rows, RtnAdr
    push    bc                    ;   Stack = Cols, Rows, RtnAdr
    push    hl                    ;   Stack = TxtPtr, Cols, Rows, RtnAdr
    ld      h,0                   ;   H = Fill Text
    ld      l,a                   ;   L = Fill Byte
    ld      iy,screen_fill
    call    aux_call              ;   Do the fill
    pop     hl                    ;   HL = TxtPtr; Stack = Cols, Rows, RtnAdr
    call    CHRGT2                ;   Reget next character
    jr      nz,.notx              ;   If terminator
    pop     bc                    ;      Stack = Rows, RtnAdr
    pop     de                    ;      Stack = RtnAdr
    ret                           ;      Return
.notx
    rst     SYNCHR                ; Else
    byte    COLTK
    rst     SYNCHR                ;   Require color
    byte    ORTK
    call    get_screen_colors     ;   A = Colors
    pop     bc                    ;   BC = Cols; Stack = Rows, RtnAdr
    pop     de                    ;   DE = Rows; Stack = RtnAdr
    push    hl                    ;   Stack = TxtPtr, Cols, Rows, RtnAdr
    ld      h,-1                  ;   H = Fill Text
    ld      l,a                   ;   L = Fill Byte
    ld      iy,screen_fill
    call    aux_call              ;   Do the fill
    pop     hl                    ;   HL = TxtPtr; Stack = Cols, Rows, RtnAdr
    ret

;-----------------------------------------------------------------------------
; FILL TILEMAP TILE tile# ATTR attrs PALETTE palette#
; FILL TILEMAP (col,row)-(col,row) TILE tile# ATTR attrs PALETTE palette#
;-----------------------------------------------------------------------------
;FILL TILEMAP (5,4) - (12,11) TILE 128
;FILL TILEMAP TILE 511
ST_FILL_TILE:
    rst     CHRGET                ; Skip TILE
    rst     SYNCHR                ; Require MAP
    byte    MAPTK
    cp      '('
    jr      nz,.fill_tile_all
    call    scan_rect             ; B = BgnCol, C = EndCol, D = BgnRow, E= EndRow
    jr      .get_props
.fill_tile_all
    ld      bc,63                 ; BgnRow = 0, EndRow = 31
    ld      de,31                 ; BgnCol = 0, EndCol = 63
.get_props
    push    de                    ; Stack = Rows, RtnAdr
    push    bc                    ; Stack = Cols, Rows, RtnAdr
    call    _get_tile_props       ; BC = Props, DE = Tile #
    ld      iy,tile_combine_props
    call    aux_call              ; DE = TilPrp
    pop     bc                    ; BC = Cols; Stack = Rows, RtnAdr
    ex      (sp),hl               ; HL = Rows; Stack = TxtPtr, RtnAdr
    ex      de,hl                 ; DE = Rows, HL = TilPrp
    ld      iy,tilemap_fill
    call    aux_call              ; In: B=BgnCol, C=EndCol, D=BgnRow, E=EndRow, HL: TilPrp
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret

;-----------------------------------------------------------------------------
; GET SCREEN (col,row)-(col,row) *arrayvar
; binary = column-count, row-count, char, color, char color, ...
;-----------------------------------------------------------------------------
;DIM A(40)
;GET SCREEN (2,2) - (10,10),*A
ST_GET_SCREEN:
    rst     CHRGET                ; Skip SCREEN
    call    _screen_suffix        ; Check for CHR and ATTR
    push    bc                    ; Stack = Mode, RtnAdr
    call    scan_rect             ; B = BgnCol, C = EndCol, D = BgnRow, E = EndRow
    SYNCHK  ','                   ; Require comma
    rst     SYNCHR
    byte    MULTK                 ; Require * - for now
    pop     af                    ; A = Mode; Stack = RtnAdr
    push    de                    ; Stack = Rows, RtnAdr
    push    bc                    ; Stack = Cols, Rows, RtnAdr
    push    af                    ; Stack = Mode, Cols, Rows, RtnAdr
    call    get_array             ; DE = Array Data Address
    pop     af                    ; A = Mode, Stack = Cols, Rows, RtnAdr
    pop     bc                    ; B = BgnCol, C = EndCol; Stack = Rows, RtnAdr
    ex      (sp),hl               ; HL = Rows; Stack = TxtPtr, RtnAdr
    ex      de,hl                 ; D = BgnRow, E = EndRow, HL = AryAdr
    ld      iy,screen_get
    call    aux_call              ; In: B=BgnCol, C=EndCol, D=BgnRow, E=EndRow, HL: AryAdr
    jp      c,FCERR
    jp      z,FCERR
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret

;-----------------------------------------------------------------------------
; GET TILEMAP (col,row)-(col,row),*arrayvar
; binary = column-count, row-count, cells
;-----------------------------------------------------------------------------
;DIM A(40)
;GET TILEMAP (2,2) - (10,10),*A
ST_GET_TILEMAP:
    jp      GSERR

    rst     CHRGET                ; Skip SCREEN
    rst     SYNCHR                ; Require MAP
    byte    MAPTK
    call    scan_rect             ; B = BgnCol, C = EndCol, D = BgnRow, E = EndRow
    SYNCHK  ','                   ; Require comma
    rst     SYNCHR
    byte    MULTK                 ; Require * - for now
    push    de                    ; Stack = Rows, RtnAdr
    push    bc                    ; Stack = Cols, Rows, RtnAdr
    call    get_array             ; DE = Array Data Address
    pop     bc                    ; B = BgnCol, C = EndCol; Stack = Rows, RtnAdr
    ex      (sp),hl               ; HL = Rows; Stack = TxtPtr, RtnAdr
    ex      de,hl                 ; D = BgnRow, E = EndRow, HL = AryAdr
    call    tilemap_get            ; In: B=BgnCol, C=EndCol, D=BgnRow, E=EndRow, HL: AryAdr
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret

;----------------------------------------------------------------------------
; PUT SCREEN Statement
; Syntax: PUT SCREEN (x1,y1),*arrayvar
;----------------------------------------------------------------------------
;PUT SCREEN (4,4),*A
ST_PUT_SCREEN:
    rst     CHRGET                ; Skip SCREEN
    call    _screen_suffix        ; Check for CHR and ATTR
    push    bc                    ; Stack = Mode, RtnAdr
    call    SCAND                 ; C = Col, E = Row
    SYNCHK  ','                   ; Require comma
    rst     SYNCHR
    byte    MULTK                 ; Require * - for now
    pop     af                    ; A = Mode; Stack = RtnAdr
    push    de                    ; Stack = Row, RtnAdr
    push    bc                    ; Stack = Col, Row, RtnAdr
    push    af                    ; Stack = Mode, Col, Row, RtnAdr
    call    get_array             ; DE = Array Data Address
    pop     af                    ; A = Mode, Stack = Col, Row, RtnAdr
    pop     bc                    ; C = Col; Stack = Row, RtnAdr
    ex      (sp),hl               ; HL = Row; Stack = TxtPtr, RtnAdr
    ex      de,hl                 ; E = Row, HL = AryAdr
    ld      iy,screen_put
    call    aux_call              ; In: C=Col, E=End, HL: AryAdr
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret

_screen_suffix:
    ld      b,3
    cp      XTOKEN                ; If Not Extended Token
    ret     nz                    ;   Return 3 (SCREEN+COLOR)
    rst     CHRGET                ; Else Eat Token
    ld      b,1
    cp      CHRTK                 ; If CHR
    jp      z,CHRGTR              ;   Eat it and return 1 (SCREEN)
    ld      b,2
    rst     SYNCHR                ; Else
    byte    ATTRTK                ;   Require ATTRS
    ret

;-----------------------------------------------------------------------------
; PUT TILEMAP (col,row),*arrayvar
;-----------------------------------------------------------------------------
;PUT TILEMAP (5,5),*A
ST_PUT_TILEMAP:
    jp      GSERR

    rst     CHRGET                ; Skip SCREEN
    rst     SYNCHR                ; Require MAP
    byte    MAPTK
    call    SCAND                 ; C = Col, E = Row
    SYNCHK  ','                   ; Require comma
    rst     SYNCHR
    byte    MULTK                 ; Require * - for now
    push    de                    ; Stack = Row, RtnAdr
    push    bc                    ; Stack = Col, Row, RtnAdr
    call    get_array             ; DE = Array Data Address
    pop     bc                    ; C = Col; Stack = Row, RtnAdr
    ex      (sp),hl               ; HL = Row; Stack = TxtPtr, RtnAdr
    ex      de,hl                 ; E = Row, HL = AryAdr
    call    tilemap_put           ; In: C=Col, E=End, HL: AryAdr
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret

;-----------------------------------------------------------------------------
; Copy section of virtual tilemap into actual tilemap
; COPY TILEMAP @page,width,height,x,y
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; SET TILEMAP
; SET TILEMAP (x,y) TO TILE tile# ATTR attrs PALETTE palette#
; SET TILEMAP (x,y) TO integer
; SET TILEMAP OFFSET x,y
;-----------------------------------------------------------------------------
ST_SET_TILEMAP:
    rst     CHRGET                ; Skip MAP
    cp      XTOKEN
    jr      nz,.set_xy            ; If XTOKEN
    rst     CHRGET                ;   Skip it
    cp      OFFTK                 ;   If OFF
    jr      z,_tilemap_offset     ;   Do SET TILEMAP OFFSET
    jp      SNERR
.set_xy                           ; Else
    call    SCAND                 ;   C = Column, E = Row
    push    de                    ;   Stack = Row, RtnAdr
    push    bc                    ;   Stack = Column, Row, RtnAdr
    rst     SYNCHR
    byte    TOTK                  ;   Require TO
    cp      TILETK
    jr      nz,.set_int           ;   If TILE 
    call    _get_tile_props       ;     BC = Props, DE = Tile#
    ld      iy,tile_combine_props
    call    aux_call              ; DE = TilPrp
    jr      .set_tile             ;   Else
.set_int
    call    get_int512            ;     DE = TilDef
.set_tile
    pop     bc                    ;   C = Column; Stack = Row, RtnAdr
    ex      (sp),hl               ;   L = Row; Stack = TxtPtr, RtnAdr
    ex      de,hl                 ;   E = Row, HL = TilDef
    ld      iy,tilemap_set_tile   
    call    aux_call              ;   Write tile to tilemap
    call    tilemap_set_tile
    jp      c,FCERR               ;   Error if invalid coordinates
    pop     hl                    ;   HL = TxtPtr; Stack = RtnAdr
    ret

; Parse TILE tile# [ATTR attrs] [PALETTE palette#]
; Output: BC = Props, DE = Tile#
; Note: TILE is not an extended token, the rest are
_get_tile_props:
    rst     SYNCHR                ; Require TILE
    byte    TILETK
    call    get_int512            ; DE = Tile#
_get_attr_palette:
    push    de                    ; Stack = Tile# , RtnAdr
    ld      bc,0                  ; Default to Attrs 0, Palette# 0
.loop
    call    CHRGT2                ; Reget character
    jp      z,pop_de_ret          ; Return if terminator
    push    bc                    ; Stack = Props, Tile #, RtnAdr
    rst     SYNCHR
    byte    XTOKEN                ; Must be extended token
    cp      ATTRTK                ; If ATTR
    jr      nz,.notattrs
    call    GTBYTC                ;   Get attributes
    pop     bc                    ;   BC = Props; Stack = Tile#, RtnAdr
    ld      b,a                   ;   B = attributes
    jr      .loop
.notattrs
    rst     SYNCHR
    byte    PALETK
    call    get_byte4             ;   Get palette#
    pop     bc                    ;   BC = Props; Stack = Tile#, RtnAdr
    ld      c,a                   ;   C = palette#
    jr      .loop

_tilemap_offset:
    rst     CHRGET                ; Skip OFF
    rst     SYNCHR                ; Require SET
    byte    SETTK
    call    get_int512            ; DE = X-position
    push    de                    ; Stack = X-position, RtnAdr
    SYNCHK  ','                   ; Require comma
    call    GETBYT                ; E = Y-position
    pop     bc                    ; BC = X-position, Stack = RtnAdr
    ld      iy,tilemap_set_offset ; Set Offset and return
    jp      aux_call

;-----------------------------------------------------------------------------
; TILEMAPX, TILEMAPY
;-----------------------------------------------------------------------------
FN_TILE:
    rst     CHRGET                ; Skip TILE
    rst     SYNCHR
    byte    MAPTK                 ; Require MAP
    cp      '('                   ; If (
    jr      z,FN_TILEMAP          ; Go do TILEMAP(x,y)
    push    AF                    ; Stack = 'X'/'Y', RtnAdr
    ld      iy,tilemap_get_offset    
    call    aux_call              ; BC = X-Offset, DE = Y-Offset
    pop     AF                    ; A = 'X'/'Y', Stack = RtnAdr
    cp      'Y'                   ; If not TILEMAPY
    jr      z,push_labbck_floatde
    cp      'X'                   ;   If not TILEMAPX
    jp      nz,SNERR              ;     Return Error
    ld      d,b                   ;   DE = X-Offset
    ld      e,c
push_labbck_floatde:
    call    push_hlinc_labbck     ; Stack = LABBCK, TxtPtr, RtnAdr
    jp      FLOAT_DE              ;   Return Y-Offset

;-----------------------------------------------------------------------------
; TILEMAP(col,row) - Return tile# and properties
;-----------------------------------------------------------------------------
FN_TILEMAP
    call    SCAND                 ; Parse column and row
    call    push_hl_labbck        ; Stack = LABBCK, TxtPtr, RtnAdr
    ld      iy,tilemap_get_tile
    call    aux_call              ; BC = tile# + properties
    jp      c,FCERR               ; Carry set = bad args
    jp      FLOAT_BC              ; Return BC

;-----------------------------------------------------------------------------
; DEF ATTRLIST A$ = attr#, attr#, ...
; attr# is an even integer between 0 and 127
;   Bit 6 = Priority
; Bit 4-5 = Palette#
;   Bit 3 = Double Height (sprite only)
;   Bit 2 = Vertical Flip
;   Bit 1 = Horizontal Flip
;-----------------------------------------------------------------------------
ST_DEFATTR:
    rst     CHRGET                ; Skip COL
    call    _setupdef
.loop
    call    GETBYT                ; Get Attribute
    and     $7E                   ; Strip Index MSB
    call    sbuff_write_byte      ; Write it to string buffer
    call    CHRGT2                ; Reget next character
    jr      z,_finish_def         ; If not end of statement
    SYNCHK  ','                   ;   Require comma
    jr      .loop                 ;   and get next tile#

;-----------------------------------------------------------------------------
; DEF PALETTELIST P$ = palette#, palette#, ...
; palette# is an integer between 0 and 3
;-----------------------------------------------------------------------------
ST_DEFPALETTE:
    rst     CHRGET                ; Skip PALETTE
    call    _setupdef
.loop
    call    get_byte4             ; Get palette#
    call    sbuff_write_byte      ; Write it to string buffer
    call    CHRGT2                ; Reget next character
    jr      z,_finish_def         ; If not end of statement
    SYNCHK  ','                   ;   Require comma
    jr      .loop                 ;   and get next color

;-----------------------------------------------------------------------------
; DEF INTLIST I$ = int, int, ...
; int is a integer between 0 and 65535
;-----------------------------------------------------------------------------
ST_DEFINT:
    rst     CHRGET                ; Skip INT
    call    _setupdef             ; Stack = VarPtr, RtnAdr
.loop
    call    GETINT                ; Get Integer
    call    sbuff_write_de        ; Write it to string buffer
    call    CHRGT2                ; Reget next character
    jr      z,_finish_def         ; If not end of statement
    cp      ';'                   ;   
    jr      nz,.not_sc            ;   If semicolon
    rst     CHRGET                ;     Skip it
    jr      .loop                 ;     and get next integer
.not_sc
    SYNCHK  ','                   ;   Else require comma
    jr      .loop                 ;     and get next integer

;-----------------------------------------------------------------------------
; DEF RGBLIST R$ = r,g,b; r,g,b; ...
; int is a integer between 0 and 65535
;-----------------------------------------------------------------------------
ST_DEFRGB:
    rst     CHRGET                ; Skip INT
    call    _setupdef             ; Stack = VarPtr, RtnAdr
.loop
    call    _get_rgb              ; DE = RGB value
    call    sbuff_write_de        ; Write RGB to String Buffer
    call    CHRGT2                ; Reget next character
    jr      z,_finish_def         ; If not end of statement
    SYNCHK  ';'                   ;   Require semicolon
    jr      .loop                 ;   and get next RGB

_finish_def:
    ex      (sp),hl               ; HL = VarPtr; Stack = TxtPtr
    push    hl                    ; Stack = VarPtr, TxtPtr
    call    sbuff_create_string   ; Copy Buffer to Temporary. Return HL = StrDsc
    push    hl                    ; Stack = StrDsc, VarPtr, TxtPtr
    jp      INBUFC                ; Copy Temporary to Variable and return

_setupdef:
    rst     SYNCHR
    byte    LISTTK                ; Require LIST
_defnolist:
    call    get_stringvar         ; Get string variable address
    pop     bc                    ; BC = ThisRet; Stack = RtnAdr
    push    de                    ; Stack = VarPtr, RtnAdr
    push    bc                    ; Stack = ThisRet, VarPtr, RtnAdr
    rst     SYNCHR
    byte    EQUATK                ; Require '='
    jp      sbuff_init            ; Init string buffer and return

;-----------------------------------------------------------------------------
; DEF TILELIST T$ = tile#, tile#, ...
; tile# is a integer between 0 and 511
;-----------------------------------------------------------------------------
ST_DEF_TILELIST:
    rst     CHRGET                ; Skip TILE
    call    _setupdef
.loop
    call    get_int512            ; Get tile#
    call    sbuff_write_de        ; Write it to string buffer
    call    CHRGT2                ; Reget next character
    jr      z,_finish_def         ; If not end of statement
    SYNCHK  ','                   ;   Require comma
    jr      .loop                 ;   and get next tile#

;-----------------------------------------------------------------------------
; GETTILE Function
; GETTILE$(tile#)
; tile# is a integer between 0 and 511
;-----------------------------------------------------------------------------
FN_GETTILE:
    rst     CHRGET                ; Skip Tile token
    SYNCHK  '$'
    SYNCHK  '('
    call    get_int512            ; DE = Tile#
    SYNCHK  ')'
    push    hl                    ; Stack = TxtPtr, RtnAdr
    push    hl                    ; Stack = DummyAdr, TxtPtr, RtnAdr
    push    de                    ; Stack = Tile#, DummyAdr, TxtPtr, RtnAdr
    ld      a,32                  ; Reading 32 bytes

    call    STRINI                ; Create TmpStr; HL = StrDsc, DE = StrAdr
    ld      bc,32                 ; BC = StrLen
    pop     hl                    ; HL = Tile#; Stack = DummyAdr, TxtPtr, RtnAdr
    ld      iy,tile_get
    ld      a,1
    ld      (VALTYP),a            ; Set Type to String
    call    FRESTR                ; Free Temporary
    jp      FINBCK                ; Return String

;-----------------------------------------------------------------------------
; GETPALETTE Function
; Syntax: GETPALETTE$(palette#)
; palette# is a integer between 0 and 3
;-----------------------------------------------------------------------------
FN_GETPALETTE:
    rst     CHRGET                ; Skip PALETTE token
    SYNCHK  '$'
    SYNCHK  '('
    call    GETBYT                ; E = Palette#
    SYNCHK  ')'
    push    hl                    ; Stack = TxtPtr
    push    hl                    ; Stack = DummyAdr, TxtPtr
    push    de                    ; Stack = Palette#, DummyAdr, TxtPtr
    ld      a,32                  ; Reading 32 bytes

    call    STRINI                ; Create TmpStr; HL = StrDsc, DE = StrAdr
    ld      bc,32                 ; BC = StrLen
    pop     hl                    ; HL = Palette#; Stack = DummyAdr, TxtPtr
    ld      a,l
    ld      iy,palette_get
    call    aux_call
    ld      a,1
    ld      (VALTYP),a            ; Set Type to String
    call    FRESTR                ; Free Temporary
    jp      FINBCK                ; Return String

;-----------------------------------------------------------------------------
; DEF SPRITE sprite$ = spritle#, x-offset, y-offset; spritle#, x-offset, y-offset
;-----------------------------------------------------------------------------
ST_DEF_SPRITE:
    rst     CHRGET                ; Skip SPRITE
    call    _defnolist            ; Stack = VarPtr, RtnAdr
    xor     a                     ; A = SptlCnt (0)
    push    af                    ; Stack = SptlCnt, VarPtr, RtnAdr
    call    sbuff_write_byte      ; Write it
    ld      b,a                   ; B = MaxYoffset (0)
    ld      c,a                   ; C = MaxXoffset (0)
    call    sbuff_write_bc        ; Write Them
.loop
    call    _get_byte             ; Get Spritle#
    cp      64
    jp      nc,FCERR
    call    sbuff_write_byte      ; Write Spritle#
    SYNCHK  ','
    call    _get_byte             ; Get Xoffset
    cp      c
    jr      c,.skipx              ; If Xoffset > MaxXoffset
    ld      c,a                   ;   MaxXoffset = Xoffset
.skipx
    call    sbuff_write_byte      ; Write Xoffset
    SYNCHK  ','
    call    _get_byte             ; Get Yoffset
    cp      b
    jr      c,.skipy              ; If Xoffset > MaxXoffset
    ld      b,a                   ;   MaxXoffset = Xoffset
.skipy
    call    sbuff_write_byte      ; Write Xoffset
    pop     af                    ; A = SptlCnt; Stack = VarPtr, RtnAdr
    inc     a                     ; SptlCnt += 1
    push    af                    ; Stack = SptlCnt, VarPtr, RtnAdr
    call    CHRGT2
    jr      z,.done
    SYNCHK  ';'
    jr      .loop
.done
    ld      de,0                  ; DE = BuffOffset (0)
    pop     af                    ; A = SptlCnt; Stack = VarPtr, RtnAdr
    call    sbuff_write_byte_ofs  ; Write it
    ld      a,c                   ;
    add     a,8                   ; A = MaxXoffset + Spritle Width
    call    sbuff_write_byte_ofs  ; Write it
    ld      a,b                   ;
    add     a,8                   ; A = MaxYoffset + Spritle Width
    call    sbuff_write_byte_ofs  ; Write it
    jp      _finish_def           ; A = SptlCnt; Stack = VarPtr, RtnAdr

_get_byte:
    push    bc                ; Stack = MaxOffsets
    call    GETBYT            ; Get spritle#
    pop     bc                ; BC = MaxOffsets
    ret

;-----------------------------------------------------------------------------
; SET SPRITE sprite$ [ON|OFF] [POS x,y] [TILE tilelist$] [PALETTE palettelist$] [ATTR attrlist$]
; SET SPRITE sprite$ TO proplist$
; SET SPRITE * OFF|CLEAR
; Attributes: Priority (64), Double-Height (8), Vertical Flip (4), Horizontal Flip (2)
; ToDo: SET SPRITE spritle# ...
;-----------------------------------------------------------------------------
ST_SET_SPRITE:
    rst     CHRGET                ; Skip SPRITE
    cp      MULTK                 ; If *
    jp      z,.all                ;   Set all Sprites
    call    get_stringvar         ; DE = SprPtr
    jp      z,MOERR               ; Missing Operand Error
    push    hl                    ; Stack = TxtPtr, RtnAdr
    ex      de,hl                 ; HL = SprPtr
    call    string_addr_len       ; BC = SprLen, DE = SprAdr
    xor     a
    cp      c                     ; If Sprite not defined
    jp      z,FCERR               ;   FC Error
    ex      de,hl                 ; DE = SprPtr, HL = SprAdr
    ex      (sp),hl               ; HL = TxtPtr; Stack = SprAdr, RtnAdr
    ld      a,(hl)                ; Get current character
    cp      TOTK                  ; If TO
    jp      z,.props              ;   Set all properties
.loop
    cp      ';'
    jr      z,.nextsprite
    cp      POSTK
    jr      z,.pos
    cp      ONTK
    jr      z,.on
    cp      TILETK
    jr      z,.tiles
    rst     SYNCHR
    byte    XTOKEN
    cp      ATTRTK
    jr      z,.attrs
    cp      OFFTK
    jr      z,.off
    cp      PALETK
    jr      z,.palette
    jp      SNERR

.nextsprite
    pop     de                    ; HL = TxtPtr; Stack = RtnAdr
    jr      ST_SET_SPRITE

.palette
    ld      ix,sprite_set_palettes; IX = jump address
    jr      .string_arg

.tiles
    rst     CHRGET                ; Skip ATTR
    ld      ix,sprite_set_tiles   ; IX = jump address
    jr      .string_arg

.attrs
    rst     CHRGET                ; Skip ATTR
    ld      ix,sprite_set_attrs   ; IX = jump address

.string_arg
    push    ix                    ; Stack = JmpOfs, SprAdr, RtnAdr
    call    get_string_arg        ; BC = StrLen, DE = StrAdr, HL = StrDsc; Stack = TxtPtr, JmpOfs, SprAdr, RtnAdr

    pop     hl                    ; HL = TxtPtr; Stack = JmpOfs, SprAdr, RtnAdr
    pop     ix                    ; IX = JmpOfs; Stack = SprAdr, RtnAdr
.do_gfx:
    ex      (sp),hl               ; HL = SprAdr; Stack = TxtPtr, RtnAdr
    call    jump_ix               ; HL = SprAdr; Stack = TxtPtr, RtnAdr
    jp      nz,FCERR
    ex      (sp),hl               ; HL = TxtPtr; Stack = SprAdr, RtnAdr
    call    CHRGT2                ; If not Terminator
    jr      nz,.loop              ; Get Next Argument
    pop     bc                    ;   Stack = RtnAdr
    ret

.on
    byte    $3E                   ; LD A,$AF, Non-Zero = On
.off
    xor     a                     ; 0 = Off
    ex      af,af'
    rst     CHRGET                ; Skip ON/OFF
    ex      af,af'
    ld      ix,sprite_toggle      ; IX = jump address
    jr      .do_gfx

.pos
    rst     CHRGET                ; Skip POS
    call    GETINT                ; DE = X-pos
    push    de                    ; Stack = X-pos, SprAdr, RtnAdr
    SYNCHK  ','                   ; Require Comma
    call    GETINT                ; DE = Y-pos
    pop     bc                    ; BC = X-pos; Stack = SprAdr, RtnAdr
    ld      ix,sprite_set_pos     ; IX = jump address
    jr      .do_gfx

.all
    rst     CHRGET                ; Skip *
    cp      CLRTK                 ; If CLEAR
    jr      z,.allreset           ;   Reset all spritles
    rst     SYNCHR                ; Else
    byte    XTOKEN
    rst     SYNCHR
    byte    OFFTK                 ;   SNERR if not OFF
.alloff
    ld      c,0
    jp      spritle_toggle_all    ; Disable all off and return
.allreset
    rst     CHRGET                ; Skip CLEAR
    jp      spritle_clear_all

.props                            ; HL = TxtPtr; Stack = SprAdr, RtnAdr
    rst     CHRGET                ; Skip TO
    call    get_string_arg        ; BC = StrLen, DE = StrAdr, HL = StrDsc; Stack = TxtPtr, SprAdr, RtnAdr
    pop     hl                    ; BC = StrLen, DE = StrAdr, HL = TxtPtr; Stack = SprAdr, RtnAdr
    ex      (sp),hl               ; BC = StrLen, DE = StrAdr, HL = SprAdr; Stack = TxtPtr, RtnAdr
    call    sprite_set_props      ; Set properties
    jp      nz,FCERR
    pop     hl
    ret

;-----------------------------------------------------------------------------
; GETSPRITE Attributes
; GETSPRITE$(SpriteDef$)
; Data Format: 5 characters for each spritle
;          X-position - 1-2
;          Y-position -  3
;          Attrs+Tile - 4-5
;-----------------------------------------------------------------------------
FN_GETSPRITE:
    rst     CHRGET                ; Skip SPRITE token
    SYNCHK  '$'
    call    PARCHK                ; Get Arg in Parentheses
    call    CHKSTR                ; Error if not a sting
    push    hl                    ; Stack = TxtPtr, RtnAdr
    push    hl                    ; Stack = DummyAdr, TxtPtr, RtnAdr
    ld      hl,(FACLO)            ; HL = SprPtr
    call    string_addr_len       ; BC = SprLen, DE = SprAdr
    push    de                    ; Stack = SprAdr, DummyAdr, TxtPtr, RtnAdr
    ld      a,(de)                ; A = SptlCnt
    ld      d,a
    add     a                     ; x 2
    add     a                     ; x 4
    add     d                     ; x 5
    jp      c,LSERR               ; Error if too long
    call    STRINI                ; Create BufStr; HL = BufDsc, DE = BufAdr
    call    string_addr_len       ; BC = BufLen, DE = BufAdr
    ex      (sp),hl               ; HL = SprAdr; Stack = BufDsc, DummyAdr, TxtPtr, RtnAdr
    call    sprite_get_attrs
    jp      nz,OVERR              ; Sprite and Buffer Size Mismtch
    ld      a,1
    ld      (VALTYP),a            ; Set Type to String
    call    FRETM2
    pop     hl                    ; HL = BufDsc; Stack = DummyAdr, TxtPtr, RtnAdr
    jp      FINBCK                ; Return String


;-----------------------------------------------------------------------------
; RGB$(r,g,b)
;-----------------------------------------------------------------------------
FN_RGB:
    inc     hl                    ; Skip RGB
    SYNCHK  '$'                   ; Require $
    SYNCHK  '('                   ; Require Open Paren
    call    _get_rgb              ; DE = RGB value
    SYNCHK  ')'                   ; Require Close Paren
    push    hl                    ; Stack = TxtPtr, RtnAdr
    push    de                    ; Stack = RGB, TxtPtr, RtnAdr
    ld      a,2
    call    STRINI                ; Allocate two character string
    ex      de,hl                 ; HL = String Text Address
    pop     de                    ; DE = RGB; Stack = TxtPtr, RtnAdr
    ld      (hl),e                ; Store LSB
    inc     hl
    ld      (hl),d                ; Store MSB
    jp      PUTNEW                ; Set Pointer to StringDesc and return

; Parse RGB triplet, returns DE = RGB integer
_get_rgb:
    call    get_byte16            ; Get Red
    push    af                    ; Stack = Red, RtnAdr
    SYNCHK  ','
    call    get_byte16            ; Get Green
    push    af                    ; Stack = Green, Red, RtnAdr
    SYNCHK  ','
    call    get_byte16            ; E = Blue
    pop     af                    ; A = Green; Stack = Red, RtnAdr
    and     $0F                   ; Shift Green to high nybble
    rla
    rla
    rla
    rla
    or      e                     ; A = Green + Blue
    pop     de                    ; D = Red; Stack = RtnAdr
    ld      e,a                   ; E = Green + Blue
    ret

