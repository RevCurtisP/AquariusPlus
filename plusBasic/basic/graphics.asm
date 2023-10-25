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
; SET PALETTE statement
; syntax: SET PALETTE palette# [, index] TO rgblist$
;-----------------------------------------------------------------------------
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
    call    palette_shift_num     ;
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
    call    palette_set           ; Set the entry (increments C)
    jp      c,OVERR               ; Error if Overflow
    pop     hl                    ; HL = TxtPtr
    ret


;-----------------------------------------------------------------------------
; Proposed new syntax
; SCREEN SAVE|RESTORE|SWAP|RESET
; SCREEN [text],[textpage],[graphics],[sprites],[wide],[priority],[remap]
;-----------------------------------------------------------------------------
ST_SCREEN:
    jp      m,.is_token           ; If a token, go process it
    in      a,(IO_VCTRL)
    ld      c,a                   ; C = Current VCTRL

    call    get_byte_optional     ; Do Text Enabled
    ld      b,VCTRL_TEXT_EN
    call    nc,.update_bit

    call    get_byte_optional     ; Do Text Page
    ld      b,VCTRL_TEXT_PAGE
    call    nc,.update_bit

    call    get_byte_optional     ; Do graphics
    call    nc,.do_gfx_mode

    call    get_byte_optional     ; Do Sprites
    ld      b,VCTRL_SPR_EN
    call    nc,.update_bit
    
    call    get_byte_optional     ; Do Wide
    ld      b,VCRTL_80COL_EN
    call    nc,.update_bit
    
    call    get_byte_optional     ; Do Priority
    ld      b,VCTRL_TEXT_PRIO
    call    nc,.update_bit
    
    call    get_byte_optional     ; Do Border Remap
    ld      b,VCTRL_REMAP_BC
    call    nc,.update_bit
    
    ld      a,c
    out     (IO_VCTRL),a          ; Write back out
    ret

.is_token
    cp      SAVETK                ; If SAVE
    jr      z,ST_SCREEN_SAVE      ;   Do SCREEN SAVE
    cp      RESTK                 ; If RESTORE
    jr      z,ST_SCREEN_RESTORE   ;   Do SCREEN RESTORE
    cp      SWAPTK                ; If RESTORE
    jr      z,ST_SCREEN_SWAP      ;   Do SCREEN RESTORE
    rst     SYNCHR
    byte    XTOKEN                ; Else
    rst     SYNCHR                ;  Require RESET
    byte    RESETK
    jp      reset_screen

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
 
;-----------------------------------------------------------------------------
; SCREEN RESTORE - Copy Screen Buffer to Text Screen
;-----------------------------------------------------------------------------
ST_SCREEN_RESTORE:
    rst     CHRGET                ; Skip RESTORE
    push    hl                    ; Stack = TxtPtr, RtnAdr
    call    screen_restore        ; Do the Copy  
    pop     hl                    ; HL = TxtPtr; HL = RtnAdr
    ret

;-----------------------------------------------------------------------------
; SCREEN SAVE - Copy Text Screen to Screen Buffer
;-----------------------------------------------------------------------------
ST_SCREEN_SAVE:
    rst     CHRGET                ; Skip SAVE
    push    hl                    ; Stack = TxtPtr, RtnAdr
    call    screen_save           ; Do the Copy  
    pop     hl                    ; HL = TxtPtr; HL = RtnAdr
    ret

;-----------------------------------------------------------------------------
; SCREEN SWAP - Swap Text Screen with Screen Buffer
;-----------------------------------------------------------------------------
ST_SCREEN_SWAP:
    rst     CHRGET                ; Skip SWAP
    push    hl                    ; Stack = TxtPtr, RtnAdr
    call    screen_swap           ; Do the Copy  
    pop     hl                    ; HL = TxtPtr; HL = RtnAdr
    ret

;-----------------------------------------------------------------------------
; SET TILE Statement
; SET TILE tile# TO color_index, ...
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
      call    tile_set
    jp      c,OVERR               ; Error if Overflow
    pop     hl                    ; HL = TxtPtr
    ret

;-----------------------------------------------------------------------------
; FILL SCREEN (col,row)-(col,row) character$, fgcolor, bgcolor
; FILL SCREEN (col,row)-(col,row) character, fgcolor, bgcolor
;-----------------------------------------------------------------------------
;FILL SCREEN (5,10) - (25,15) "X",7,0
;FILL SCREEN (20,8) - (38,20) $86,5,0
ST_FILL_SCREEN:
    rst     CHRGET                ; Skip SCREEN
    call    scan_rect             ; B = BgnCol, C = EndCol, D = BgnRow, E= EndRow  
    push    de                    ; Stack = Rows, RtnAdr
    push    bc                    ; Stack = Cols, Rows, RtnAdr
    call    FRMEVL                ; Evaluate Character
    call    GETYPE                
    jr      z,.string             ; If numeric
    call    CONINT                ;   Convert to byte
    jr      .gotit                ; Else
.string
    push    hl                    ; Stack = TxtPtr, Cols, Rows, RtnAdr
    call    free_addr_len         ; BC = StrLen, DE = StrAdr
    pop     hl                    ; HL = TxtPtr; Stack = Cols, Rows, RtnAdr
    xor     a 
    or      c                     
    jp      z,FCERR               ; Error if LEN = 0
    ld      a,(de)                ;   Get first character
.gotit    
    push    af                    ; Stack = Char, Cols, Rows, RtnAdr
    SYNCHK  ','                   ; Require comma
    call    get_byte16            ; A = FColor
    sll     a                     ; 
    sll     a                     ; 
    sll     a                     ; 
    sll     a                     ; 
    push    af                    ; Stack = FColor, Char, Cols, Rows, RtnAdr
    SYNCHK  ','                   ; Require comma
    call    get_byte16            ; A = BColor
    pop     bc                    ; D = FColor; Stack = Char, Cols, Rows, RtnAdr
    or      b                     ; A = Colors
    pop     de                    ; D = Char; Stack = Cols, Rows, RtnAdr
    ld      e,a                   ; DE = ChrClr
    pop     bc                    ; BC = Cols; Stack = Rows, RtnAdr
    ex      (sp),hl               ; HL = Rows; Stack = TxtPtr, RtnAdr
    ex      de,hl                 ; DE = Rows, HL = ChrClr
    call    screen_fill           ; In: B=BgnCol, C=EndCol, D=BgnRow, E=EndRow, HL: ChrClr
    jp      c,FCERR
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
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
    jr      nz,.fill_all
    call    scan_rect             ; B = BgnCol, C = EndCol, D = BgnRow, E= EndRow  
    jr      .get_props
.fill_all
    ld      bc,63                 ; BgnRow = 0, EndRow = 31
    ld      de,31                 ; BgnCol = 0, EndCol = 63
.get_props
    push    de                    ; Stack = Rows, RtnAdr
    push    bc                    ; Stack = Cols, Rows, RtnAdr
    call    _get_tile_props       ; BC = Props, DE = Tile #
    call    tile_combine_props    ; DE = TilPrp
    pop     bc                    ; BC = Cols; Stack = Rows, RtnAdr
    ex      (sp),hl               ; HL = Rows; Stack = TxtPtr, RtnAdr
    ex      de,hl                 ; DE = Rows, HL = TilPrp
    call    tilemap_fill          ; In: B=BgnCol, C=EndCol, D=BgnRow, E=EndRow, HL: TilPrp
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
    call    screen_get            ; In: B=BgnCol, C=EndCol, D=BgnRow, E=EndRow, HL: AryAdr
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
    call    screen_put            ; In: C=Col, E=End, HL: AryAdr
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
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
    rst     SYNCHR                ;
    byte    XTOKEN                ; Only Extended Tokeb


    cp      OFFTK                 ; If OFF
    jr      z,_tilemap_offset     ;   Do SET TILEMAP OFFSET
    jp      SNERR

;ToDo: get TileMap (X,Y) working 
;Note: TILE is not an extended token, the rest are
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
    jp      tilemap_set_offset    ; Set Offset and return

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
    call    tilemap_get_offset    ; BC = X-Offset, DE = Y-Offset
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
    call    tilemap_get_tile      ; BC = tile# + properties
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
    SYNCHK  ','                   ;   Require comma
    jr      .loop                 ;   and get next tile#

;-----------------------------------------------------------------------------
; DEF RGBLIST R$ = r,b,g; r,b,g; ...
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
    call    tile_get
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
    call    palette_get           ;
    ld      a,1
    ld      (VALTYP),a            ; Set Type to String
    call    FRESTR                ; Free Temporary
    jp      FINBCK                ; Return String

;-----------------------------------------------------------------------------
; DEF SPRITE sprite$ = spritle#, x-offset, y-offset; spritle#, x-offset, y-offset
;-----------------------------------------------------------------------------
ST_DEFSPRITE:
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

