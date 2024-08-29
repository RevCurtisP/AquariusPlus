;====================================================================
; Graphics Statements and Functions
;====================================================================

;; ToDo: Normalize SCREEN CHR and SCREEN ATTR
;; LOAD SCREEN, SAVE SCREEN

;-----------------------------------------------------------------------------
; USE CHRSET - Change Character Set
; Syntax: USE CHRSET [0|1|filename$]
;-----------------------------------------------------------------------------
; USE CHRSET "latin1d.chr"
ST_USECHR:
.andmask = 255-BASCHRSET
.ormask = BASCHRSET * 256
    rst     CHRGET                ; Skip CHR
    rst     SYNCHR                ; Require SET
    byte    SETTK
    call    FRMEVL                ; Evaluate operand
    call    GETYPE
    jr      nz,.switch
    push    hl
    call    FRESTR                ; Free TEMP, get StrDsc in HL
    call    load_chrset           ; Load the character set
    inc     a                     ; A = 1
    jr      .select
.switch
    call    CONINT                ; A = Operand
    cp      2
    jp      nc,FCERR
    push    hl                    ; Stack = TxtPtr, RtnAdr
.select
    or      a                     ; Set Flags
    push    af                    ; Stack = ChrSet, TxtPtr, RtnAdr
    ld      a,(BASYSCTL)
    jr      nz,.default
    and     $FF-BASCHRSET
    byte    $01                   ; LD BC over OR
.default    
    or      BASCHRSET
    and     $FF-BASCHRMOD         ; Clear CharRAM modified bit
    ld      (BASYSCTL),a
    pop     af                    ; A = ChrSet; Stack = TxtPtr, RtnAdr
    call    select_chrset         ;
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret

;-----------------------------------------------------------------------------
; USE SCREEN - Specify bitmap mode for drawing
; Syntax: USE SCREEN text,graphics
;-----------------------------------------------------------------------------
ST_USE_SCREEN:
    jp      GSERR                 ; Nor implemented error
    rst     CHRGET                ; Skip SCREEN
    call    get_byte4             ; A = text mode
    push    af
    call    get_comma             ; MO Error if no comma
    call    get_byte4             ; A = graphics mode
    pop     bc                    ; B = text mode
    ret

;-----------------------------------------------------------------------------
; GETATTR(X,Y) - Change Character Set
;-----------------------------------------------------------------------------
FN_GETATTR
    rst     CHRGET                ; Skip ATTR
    jp      GSERR                 ; Not implemented error

;-----------------------------------------------------------------------------
; GETCHR(X,Y) - Get Character at (X,Y)
;-----------------------------------------------------------------------------
FN_GETCHR:
    rst     CHRGET                ; Skip CHR
    cp      DEFTK                 ;  If GETCHRDEF
    jr      z,FN_GETCHRDEF        ;   Go do it
    cp      SETTK                 ; If GETCHRSET
    jr      z,FN_GETCHRSET        ;   Go do it
    jp      GSERR                 ; Not implemented error

;-----------------------------------------------------------------------------
; GETCHRDEF - Return Corrent Character Set
; GETCHRDEF$(ascii_code)
;-----------------------------------------------------------------------------
; ToDo: GETCHRDEF(ascii_code, chrset)
; PRINT HEX$(GETCHRDEF$(127))
; PRINT HEX$(GETCHRDEF$('@'))
FN_GETCHRDEF:
    rst     CHRGET                ; Skip DEF
    SYNCHK  '$'                   ; Require '$'
    SYNCHK  '('                   ; Require (
    call    get_char              ; C = ChrASC
    ex      af,af'                ; A' = ChrASC
    SYNCHK  ')'                   ; Require )
    push    hl                    ; Stack = TxtPtr, RtnAdr  
    ld      a,8                   ; A = BufLen
    call    STRINI                ; DE = BufAdr
    ex      af,af'                ; A = ChrASC
    ld      bc,8                  ; BC = BufLen
    ld      l,0                   ; L = ChrSet
    ld      iy,gfx_get_char_def
    call    aux_call
    jp      PUTNEW

;-----------------------------------------------------------------------------
; GETCHRSET - Return Corrent Character Set
;-----------------------------------------------------------------------------
; PRINT GETCHRSET
FN_GETCHRSET:
    rst     CHRGET                ; Skip SET
    push    hl                    ; Stack = TxtPtr, RtnAdr
    ld      hl,LABBCK           
    push    hl                    ; Stack = LABBCK, TxtPtr, RtnAdr
    call    ext_get_chrset
    jp      FLOAT                 ; Float an return result
; Return current character set in A
ext_get_chrset:
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

;-----------------------------------------------------------------------------
; SET COLOR statements
; SET COLOR foreground, background
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
    ld      iy,palette_reset
    jp      aux_call_preserve_hl

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
; SCREEN function
; Returns: Bit   76    5      4        3       21     0   
;               Text Remap Priority Sprites Graphics  -
;-----------------------------------------------------------------------------
FN_SCREEN:
    rst     CHRGET                ; 
    push    hl                    ; Stack = TxtPtr, RtnAdr
    ld      bc,LABBCK
    push    bc                    ; Stack = LABBCK, TxtPtr, RtnAdr
    ld      iy,screen_status
    call    aux_call              ; A = Screen status    
    jp      SNGFLT                ; Float it and return

;-----------------------------------------------------------------------------
; SCREEN [text],[graphics],[sprites],[priority],[remap]
;-----------------------------------------------------------------------------
; SCREEN 0:PRINT HEX$(PEEK($3830))
; SCREEN 3:PRINT HEX$(PEEK($3830))
; SCREEN 0,2:PRINT HEX$(PEEK($3830))
; SCREEN 0,3:PRINT HEX$(PEEK($3830))
ST_SCREEN:
    call    get_byte_optional     ; A = [text]
    jr      c,.no_text            ; If specified
    ld      iy,screen_switch      
    call    aux_call_preserve_hl
.no_text
    in      a,(IO_VCTRL)
    ld      c,a                   ; C = Current VCTRL

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

    ld      iy,bitmap_set_mode_nobuff
    jp      aux_call              ; Set EXT_FLAGS and return

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
; COPY SCREEN - Copy Screen RAM to paged memory
; COPY SCREEN TO @page,address
;-----------------------------------------------------------------------------
ST_COPY_SCREEN:
    rst     CHRGET                ; Skip SCREEN
    rst     SYNCHR
    byte    TOTK
    call    get_page_addr         ; A = Page, DE = Address
    jp      nc,FCERR              ; Error if page not specified
    ld      iy,screen_write_paged
    jr      aux_call_preserve_hl

;-----------------------------------------------------------------------------
; COPY TO SCREEN - Copy paged memory to Screen RAM 
; COPY @page,address TO SCREEN
;-----------------------------------------------------------------------------
; On entry, A = Page, DE = Address
copy_to_screen:
    ld      iy,screen_read_paged
    jr      aux_call_preserve_hl

;-----------------------------------------------------------------------------
; COPY TO SCREEN - Copy Screen RAM to paged memory
; COPY @page,address TO SCREEN
;-----------------------------------------------------------------------------
;; ToDo: Implement this (will need to mod ST_COPY in enhanced.asm)
; COPY @32,0 TO SCREEN
    ret

;-----------------------------------------------------------------------------
; RESET SCREEN - Reset current text screen to default settings
;-----------------------------------------------------------------------------
ST_RESET_SCREEN:
    rst     CHRGET                ; Skip SCREEN
_reset_screen:
    ld      iy,screen_reset
aux_call_preserve_hl:
    push    hl
aux_call_popret:
    call    aux_call
    pop     hl
    ret

;-----------------------------------------------------------------------------
; RESTORE SCREEN - Copy Screen Buffer to Text Screen
;-----------------------------------------------------------------------------
ST_RESTORE_SCREEN:
    ld      iy,screen_restore
    jr      _swapcall_pophrt

;-----------------------------------------------------------------------------
; STASH SCREEN - Copy Text Screen to Screen Buffer
;-----------------------------------------------------------------------------
ST_STASH_SCREEN:
    ld      iy,screen_stash
    jr      _swapcall_pophrt

;-----------------------------------------------------------------------------
; SWAP SCREEN - Swap Text Screen with Screen Buffer
;-----------------------------------------------------------------------------
ST_SWAP_SCREEN:
    ld      iy,screen_swap
_swapcall_pophrt:
    rst     CHRGET                ; Skip SCREEN
    push    hl                    ; Stack = TxtPtr, RtnAdr
    ld      hl,SWPSCRN40          ; Stash to Swap Buffers
    jp      aux_call_popret              

;-----------------------------------------------------------------------------
; SET TILE Statement
; SET TILE tile# TO tiledata$
; SET TILE tile# TO CHR ascii_code, fg_color, bg_color
;-----------------------------------------------------------------------------
ST_SET_TILE:
    rst     CHRGET                ; Skip TILE Token
    cp      MAPTK                 ; If TILEMAP
    jp      z,ST_SET_TILEMAP      ;   Go do that
    call    get_int512            ; Get Tile#
    push    de                    ; Stack = Tile#, RtnAdr
    rst     SYNCHR                ; Require TO
    byte    TOTK
    cp      XTOKEN                ; If extended token
    jr      z,_set_tile_ext       ;   Do SET TILE ... TO CHR ...
    call    FRMEVL                ; Get DataAddr or String
    ex      (sp),hl               ; HL = Tile#; Stack = TxtPtr, RtnAdr
    push    hl                    ; Stack = Tile#, TxtPtr, RtnAdr
    call    free_addr_len         ; DE = DataAddr, BC = Count
_set_tile:
    pop     hl                    ; HL = Tile#; Stack = TxtPtr, RtnAdr
    ld      iy,tile_set
    call    aux_call
    jp      c,OVERR               ; Error if Overflow
    pop     hl                    ; HL = TxtPtr
    ret

; SET TILE 1 TO CHR 64,1,0
; SET TILE 2 TO CHR 32,0,9:? HEX$(T$)
; SET TILE 3 TO CHR '.',$F,0:? HEX$(T$)
; SET TILE 4 TO CHR '"',$A,1:? HEX$(T$)
_set_tile_ext:
    rst     CHRGET                ; Skip XTOKEN
    rst     SYNCHR
    byte    CHRTK                 ; Require CHR
    call    GETBYT                ; A = AscVal
    push    af                    ; Stack = AscVal, Tile#, RtnAdr
    call    get_comma_byte        ; A = FgColr
    push    af                    ; Stack = FgColr, AscVal, Tile#, RtnAdr
    call    get_comma_byte        ; E = BgColr
    pop     af                    ; A = FgColr; Stack = AscVal, Tile#, RtnAdr
    ld      d,a                   ; D = FgColr
    call    no_more               ; Error if another comma
    pop     af                    ; A = AscVal; Stack = Tile#, RtnAdr
    ex      (sp),hl               ; HL = Tile#; Stack = TxtPtr, RtnAdr
    push    hl                    ; Stack = Tile#, TxtPtr, RtnAdr
    call    get_strbuf_addr       ; HL = StrBuf
    ld      b,d
    ld      c,e                   ; BC = Colors
    ex      de,hl                 ; DE = StrBuf, HL = Colors
    ld      iy,tile_from_chrrom   ; Build tile in String Buffer
    call    aux_call              ; DE = StrBuf, BC = DatLen
    jr      _set_tile             ; Write the tile

;-----------------------------------------------------------------------------
; FILL COLORMAP (col,row)-(col,row) COLOR fgcolor, bgcolor
;-----------------------------------------------------------------------------
; FILL COLORMAP (0,0)-(4,5) COLOR 7,1
; FILL COLORMAP (10,5)-(20,15) COLOR 7,1
ST_FILL_COLORMAP:
    rst     CHRGET                ; Skip COL
    rst     SYNCHR
    byte    ORTK
    rst     SYNCHR                ; Require ORMAP
    byte    MAPTK                 
    call    scan_rect             ; B = BgnCol, C = EndCol, D = BgnRow, E= EndRow
    push    bc                    ; Stack = Cols, RtnAdr
    push    de                    ; Stack = Rows, Cols, RtnAdr
    call    parse_colors          ; A = Colors
    pop     de                    ; DE = Rows; Stack = Cols, RtnAdr
    pop     bc                    ; BC = Cols; Stack = RtnAdr
    push    hl                    ; Stack = TxtPtr, RtnAdr
    ld      l,a                   ; L = Colors
    ld      iy,colormap_fill
    call    aux_call
    jp      c,FCERR               ; Illegal quantity error if out of bounds
    pop     hl
    ret

;-----------------------------------------------------------------------------
; FILL SCREEN [(col,row)-(col,row)] [CHR chr|chr$] [COLOR fgcolor, bgcolor]
;-----------------------------------------------------------------------------
; Will not change to FILL SCREEN CHR/ATTR because this is easier to parse
; FILL SCREEN (10,5)-(20,15) CHR '*' COLOR 7,0
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
    jr      nz,.notx              ; If extended token
    inc     hl                    ;   Skip XTOKEN
    rst     SYNCHR
    byte    CHRTK                 ;   Require CHR
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
    call    parse_colors          ;   A = Colors
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
    call    screen_suffix         ; Check for CHR and ATTR
    ld      (XTEMP0),bc           ; XTEMP1 = Mode
    call    scan_rect             ; B = BgnCol, C = EndCol, D = BgnRow, E = EndRow
    ld      iy,screen_get
    jr      _do_get

;-----------------------------------------------------------------------------
; GET TILEMAP (col,row)-(col,row),*arrayvar
; binary = column-count, row-count, cells
;-----------------------------------------------------------------------------
;DIM A(40):FILL TILEMAP TILE 511
;GET TILEMAP (2,2) - (10,10),*A
;GET TILEMAP (1,1)-(2,2),^A$
ST_GET_TILEMAP:
    rst     CHRGET                ; Skip TILE
    rst     SYNCHR                ; Require MAP
    byte    MAPTK
    call    scan_rect             ; B = BgnCol, C = EndCol, D = BgnRow, E = EndRow
    ld      iy,tilemap_get
_do_get:
    SYNCHK  ','                   ; Require comma
    push    de                    ; Stack = Rows, RtnAdr
    push    bc                    ; Stack = Cols, Rows, RtnAdr
    cp      MULTK                 ; If *
    jp      z,_get_put_array      ;   Get into array
_get_to_string:
    call    _parse_get_string     ; A = StrLen, DE = VarPtr
    push    hl                    ; Stack = TxtPtr, Cols, Rows, RtnAdr
    push    de                    ; Stack = VarPtr, TxtPtr, Cols, Rows, RtnAdr
    call    STRINI                ; DE = StrAdr, HL = StrDsc
    ex      de,hl                 ; DE = StrDsc, HL = StrAdr
    ex      (sp),hl               ; HL = VarPtr; Stack = StrAdr, TxtPtr, Cols, Rows, RtnAdr
    push    hl                    ; Stack = VarPtr, StrAdr, TxtPtr, Cols, Rows, RtnAdr
    call    FRETMS                ; Free temp but not string space
    pop     hl                    ; HL = VarPtr; Stack = StrAdr, TxtPtr, Cols, Rows, RtnAdr
    call    MOVE                  ; Copy StrDsc to VarPtr
    pop     de                    ; DE = StrAdr; Stack = TxtPtr, Cols, Rows, RtnAdr
    pop     hl                    ; HL = TxtPtr; Stack = Cols, Rows, RtnAdr
    jp      _get_put

_parse_get_string:
    push    hl                    ; Stack = TxtPtr, RtnAdr
    push    iy
    ld      iy,gfx_rect_size
    call    aux_call              ; HL = Rectangle size
    pop     iy
    inc     hl
    add     hl,hl                 ; HL = HL * 2 + 2
    ld      a,h
    or      a                     ; If > 255
    jp      nz,LSERR              ;   String too long error
    ld      a,l                   ; A = StrLen
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    push    af                    ; Stack = StrLen, RtnAdr
    rst     SYNCHR
    byte    EXPTK                 ; Require ^
    call    PTRGET                ; Return DE = VarPtr
    pop     af                    ; A = StrLen; Stack = RtnAdr
    ret



;------------------------------------------------------------------------
; PUT SCREEN Statement
; Syntax: PUT SCREEN (x1,y1),*arrayvar
;----------------------------------------------------------------------------
;PUT SCREEN (4,4),*A
ST_PUT_SCREEN:
    rst     CHRGET                ; Skip SCREEN
    call    screen_suffix         ; B: 1 = CHR, 2 = ATTR, 3 = Neither
    ld      (XTEMP0),bc           ; XTEMP1 = Mode
    call    SCAND                 ; C = Col, E = Row
    ld      iy,screen_put
    jr      _do_put

screen_suffix:
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
;DIM A(40)
;GET TILEMAP (2,2) - (10,10),*A
;FILL TILEMAP TILE 128 PALETTE 2
;PUT TILEMAP (2,2),*A
;GET TILEMAP (1,1)-(2,2),^A$
;PUT TILEMAP (2,2),^A$
ST_PUT_TILEMAP:
    rst     CHRGET                ; Skip TILE
    rst     SYNCHR                ; Require MAP
    byte    MAPTK
    call    SCAND                 ; C = Col, E = Row
    ld      iy,tilemap_put
_do_put:
    SYNCHK  ','                   ; Require comma
    push    de                    ; Stack = Row, RtnAdr
    push    bc                    ; Stack = Col, Row, RtnAdr
    cp      MULTK                 ; If *
    jr      z,_get_put_array      ;   Get into array
_put_from_string:
    rst     SYNCHR
    byte    EXPTK                 ; Require ^
    call    PTRGET                ; Return DE = VarPtr
    push    hl                    ; Stack = TxtPtr, Cols, Rows, RtnAdr
    ex      de,hl                 ; HL = VarPtr
    call    string_addr_len       ; DE = StrAdr, BC = StrLen
    ld      a,b
    or      c                     ; If StrLen = 0 
    jr      z,ESERR               ;   Illegal quantity error
    pop     hl                    ; HL = TxtPtr; Stack = Cols, Rows, RtnAdr
    jr      _get_put              ; Put it

ESERR:
    ld      e,ERRES
    jp      ERROR

_get_put_array:
    rst     SYNCHR
    byte    MULTK                 ; Require * - for now
    call    get_array             ; DE = Array Data Address
_get_put:
    pop     bc                    ; C = Col; Stack = Row, RtnAdr
    ex      (sp),hl               ; HL = Row; Stack = TxtPtr, RtnAdr
    ex      de,hl                 ; E = Row, HL = AryAdr
    ld      a,(XTEMP0+1)          ; A = Mode (GET/PUT SCREEN)
    call    aux_call
    jp      c,FCERR
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
; TILE functions stub
;-----------------------------------------------------------------------------
FN_TILE:
    rst     CHRGET                ; Skip TILE
    cp      MAPTK                 ; If TILEMAP
    jr      z,FN_TILEMAP          ;   Go Do It
    rst     SYNCHR                ; Else
    byte    XTOKEN                ;   
    rst     SYNCHR                ;   Require OFFSET
    byte    OFFTK                 ;   
    rst     SYNCHR                
    byte    SETTK                 ;   
;-----------------------------------------------------------------------------
; TILEOFFSET
;-----------------------------------------------------------------------------
FN_TILEOFFSET:
    call    tile_offset           ; BC = Offset
    jr      push_labbck_floatbc

;-----------------------------------------------------------------------------
; TILEMAPX, TILEMAPY, TILEMAP()
;-----------------------------------------------------------------------------
FN_TILEMAP:
    rst     CHRGET                ; Skip MAP
    cp      '('                   ; If (
    jr      z,_tilemap_xy         ; Go do TILEMAP(x,y)
    push    af                    ; Stack = 'X'/'Y', RtnAdr
    rst     CHRGET                ; Skip X/Y
    ld      iy,tilemap_get_offset
    call    aux_call              ; BC = X-Offset, DE = Y-Offset
    pop     af                    ; A = 'X'/'Y', Stack = RtnAdr
    cp      'Y'                   ; If not TILEMAPY
    jr      z,push_labbck_floatde
    cp      'X'                   ;   If not TILEMAPX
    jp      nz,SNERR              ;     Return Error
push_labbck_floatbc:
    ld      d,b                   ;   DE = X-Offset
    ld      e,c
push_labbck_floatde:
    call    push_hl_labbck        ; Stack = LABBCK, TxtPtr, RtnAdr
    jp      FLOAT_DE              ;   Return Y-Offset

;-----------------------------------------------------------------------------
; TILEMAP(col,row) - Return tile# and properties
;-----------------------------------------------------------------------------
_tilemap_xy:
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
ST_DEF_ATTR:
    rst     CHRGET                ; Skip ATTR/BYTE
    call    _setupdef             ; DatLen, BufAdr, VarPtr
.loop
    call    GETBYT                ; E = Attribute
    and     $7E                   ; Strip Index MSB
    call    _write_byte_strbuf    ; Write it to string buffer
    call    CHRGT2                ; Reget next character
    jr      z,_finish_def         ; If not end of statement
    SYNCHK  ','                   ;   Require comma
    jr      .loop                 ;   and get next tile#

;-----------------------------------------------------------------------------
; DEF PALETTELIST P$ = palette#, palette#, ...
; palette# is an integer between 0 and 3
;-----------------------------------------------------------------------------
;DEF PALETTELIST P$ = 0,1,2,3
ST_DEF_PALETTE:
    rst     CHRGET                ; Skip PALETTE
    call    _setupdef            ; Stack = DatLen, BufPtr, VarPtr, RtnAdr
.loop
    call    get_byte4             ; Get palette#
    call    _write_byte_strbuf    ; Write to buffer
    call    CHRGT2                ; Reget next character
    jr      z,_finish_def        ; If not end of statement
    SYNCHK  ','                   ;   Require comma
    jr      .loop                 ;   and get next color

;-----------------------------------------------------------------------------
; DEF INTLIST I$ = int, int, ...
; int is a integer between 0 and 65535
;-----------------------------------------------------------------------------
; DEF INTLIST I$ = 1234,$ABBA;0,$FFFF
ST_DEF_INT:
    rst     CHRGET                ; Skip INT
    call    _setupdef            ; Stack = DatLen, BufPtr, VarPtr, RtnAdr
.loop
    call    GETINT                ; DE = Integer
    call    _write_word_strbuf    ; Write DE to strbuf
    call    CHRGT2                ; Reget next character
    jr      z,_finish_def        ; Finish up if end of statement
    cp      ';'                   ;
    jr      nz,.not_sc            ; If semicolon
    rst     CHRGET                ;   Skip it
    jr      .loop                 ;   and get next integer
.not_sc
    SYNCHK  ','                   ; Else require comma
    jr      .loop                 ;   and get next integer

;-----------------------------------------------------------------------------
; DEF RGBLIST R$ = r,g,b; r,g,b; ...
; int is a integer between 0 and 65535
;-----------------------------------------------------------------------------
ST_DEF_RGB:
    rst     CHRGET                ; Skip RGB
    call    _setupdef             ; Stack = DatLen, BufPtr, VarPtr
.loop
    call    _get_rgb              ; DE = RGB value
    call    _write_word_strbuf    ; Write RGB to String Buffer
    call    CHRGT2                ; Reget next character
    jr      z,_finish_def         ; If not end of statement
    SYNCHK  ';'                   ;   Require semicolon
    jr      .loop                 ;   and get next RGB

; On entry HL = TxtPtr; Stack = DatLen, BufPtr, VarPtr, RtnAdr
_finish_def:
    pop     af                    ; A = DatLen; Stack = BufPtr, VarPtr, RtnAdr
    pop     de                    ; Stack = VarPtr, RtnAdr
    ex      (sp),hl               ; HL = VarPtr; Stack = TxtPtr, RtnAdr
    push    hl                    ; Stack = VarPtr, TxtPtr, RtnAdr
_assign_def_var:
    call    strbuf_temp_str       ; Copy Buffer to Temporary. Return HL = StrDsc
    push    hl                    ; Stack = StrDsc, VarPtr, TxtPtr
    jp      INBUFC                ; Copy Temporary to Variable and return

_setupdef:
    rst     SYNCHR
    byte    LISTTK                ; Require LIST
_defnolist:
    call    get_stringvar         ; DE = VarAdr
_defgotvar:
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


; These routines must not change BC or HL
; On entry, Stack = RtnAdr, DatLen, BufPtr, VarPtr
_write_bc_strbuf:
    ld      d,b                   ; DE = BC
    ld      e,c
_write_word_strbuf:
    byte    $3E                   ; LD A,$AF
_write_byte_strbuf:
    xor     a                     ; A = 0
    or      a                     ; Z = Byte, NZ = Word
    ex      af,af'                ; AF' = IntFlg
    pop     ix                    ; IX = RtnAdr; Stack = DatLen, BufPtr, VarPtr
    pop     af                    ; AF = DatLen; Stack = BufPtr, VarPtr
    ex      (sp),hl               ; HL = BufPtr; Stack = TxtPtr, VarPtr
    inc     a                     ; Bump length
    jr      z,.lserr              ; Error if >255
    ld      (hl),e                ; Write LSB
    inc     hl                    ; Bump BufPtr
    ex      af,af'                ; AF = IntFlg, AF' = DatLen
    jr      z,.done               ; If writing word
    ex      af,af'                ;   AF = DatLen, AF' = IntFlg
    inc     a                     ;   Bump DatLen
.lserr:
    jp      z,LSERR               ;   Error if >255
    ex      af,af'                ;   AF = IntFlg, AF' = DatLen
    ld      (hl),d                ;   Write LSB
    inc     hl                    ;   Bump BufPtr
.done
    ex      (sp),hl               ;   HL = TxtPtr; Stack = BufPtr, VarPtr
    ex      af,af'                ;   A = DatLen
    push    af                    ; Stack = DatLen, BufPtr, VarPtr
    jp      (ix)                  ; Return


;-----------------------------------------------------------------------------
; DEF TILELIST T$ = tile#, tile#, ...
; tile# is a integer between 0 and 511
;-----------------------------------------------------------------------------
ST_DEF_TILELIST:
    rst     CHRGET                ; Skip TILE
    call    _setupdef             ; Stack = DatLen, BufPtr, VarPtr
.loop
    call    get_int512            ; Get tile#
    call    _write_word_strbuf    ; Write it to string buffer
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
    ld      iy,tile_get
    jr      _get_aux

;-----------------------------------------------------------------------------
; GETPALETTE Function
; Syntax: GETPALETTE$(palette#)
; palette# is a integer between 0 and 3
;-----------------------------------------------------------------------------
FN_GETPALETTE:
    rst     CHRGET                ; Skip PALETTE token
    SYNCHK  '$'
    SYNCHK  '('
    ld      iy,palette_get
    call    GETBYT                ; E = Palette#
_get_aux
    SYNCHK  ')'
    push    hl                    ; Stack = TxtPtr
    push    hl                    ; Stack = DummyAdr, TxtPtr
    push    de                    ; Stack = Palette#, DummyAdr, TxtPtr
    ld      a,32                  ; Reading 32 bytes
    call    STRINI                ; Create TmpStr; HL = StrDsc, DE = StrAdr
    ld      bc,32                 ; BC = StrLen
    pop     hl                    ; HL = Palette#; Stack = DummyAdr, TxtPtr
    ld      a,l
aux_call_finbck:
    call    aux_call
    ld      a,1
    ld      (VALTYP),a            ; Set Type to String
    jp      FINBCK                ; Return String

;-----------------------------------------------------------------------------
; DEF SPRITE sprite$ = spritle#, x-offset, y-offset; spritle#, x-offset, y-offset
; DEF SPRITE sprite$ = [ spritle#, ... ] {; ...}
; DEF SPRITE sprite$ = spritle_list$
; String Format: SprCount,TotWidth,TotHeigth,(SprNum,Xoffset,Yoffset)...
;-----------------------------------------------------------------------------
ST_DEF_SPRITE:
    rst     CHRGET                ; Skip SPRITE
    call    _defnolist            ; Stack = DatLen, BufPtr, VarPtr
    xor     a                     ; A = SptlCnt (0)
    ld      b,a                   ; B = MaxYoffset (0)
    ld      c,a                   ; C = MaxXoffset (0)
    ld      (XTEMP0),a            ; XTEMP0 = SptlCnt
    call    _write_byte_strbuf    ; Write E to string buffer
    call    _write_bc_strbuf      ; Write BC to string buffer
    ld      a,(hl)
    cp      '['                   ; If [ after =
    jr      z,_def_sprite_rect    ;   Do rectangular definition
.loop
    call    _get_byte             ; A,E = Spritle#
    jp      nc,FCERR
    call    _write_byte_strbuf    ; Write E to string buffer
    SYNCHK  ','
    call    _get_byte             ; A,E = Xoffset
    cp      c
    jr      c,.skipx              ; If Xoffset > MaxXoffset
    ld      c,a                   ;   MaxXoffset = Xoffset
.skipx
    call    _write_byte_strbuf    ; Write E to string buffer
    SYNCHK  ','
    call    _get_byte             ; A,E Yoffset
    cp      b
    jr      c,.skipy              ; If Xoffset > MaxXoffset
    ld      b,a                   ;   MaxXoffset = Xoffset
.skipy
    call    _write_byte_strbuf    ; Write E to string buffer
    call    inc_xtemp0            ; Increment SptlCnt
    call    CHRGT2
    jr      z,_sprite_done
    SYNCHK  ';'
    jr      .loop
_sprite_done
    push    hl                    ; Stack = TxtPtr, Datlen, BufPtr, VarPtr, RtnAdr
    push    bc                    ; Stack = MaxOfs, TxtPtr, Datlen, BufPtr, VarPtr, RtnAdr
    call    get_strbuf_addr       ; HL = BufAdr
    pop     bc                    ; BC = MaxOfs; Stack = TxtPtr, Datlen, BufPtr, VarPtr, RtnAdr
    ld      a,(XTEMP0)
    ld      (hl),a                ; Write SptlCnt to string buffer
    inc     hl
    ld      a,c                   ;
    add     a,8                   ; A = MaxXoffset + Spritle Width
    ld      (hl),a                ; Write to string buffer
    inc     hl
    ld      a,b                   ;
    add     a,8                   ; A = MaxYoffset + Spritle Height
    ld      (hl),a                ; Write to string buffer
    pop     hl                    ; HL = TxtPtr; Stack = Datlen, BufPtr, VarPtr, RtnAdr
    jp      _finish_def          ;

_get_byte:
    push    bc                    ; Stack = MaxOfs
    call    get_byte64            ; Get spritle#
    pop     bc                    ; BC = MaxOfs
    ret

; DEF SPRITE S$ = [0,1,2,3],[5,6,4,7],[3,2,1,0]
; Here B and C are the current Y and X offset
_def_sprite_rect:
    xor     a 
    ld      (XTEMP1),a            ; MaxXoffset = 0
.gloop
    rst     CHRGET                ; Skip [ or ,
.loop
    call    _get_byte             ; A,E = sprite#
    cp      64
    jp      nc,FCERR
    call    _write_byte_strbuf    ; Write E to string buffer
    call    _write_bc_strbuf      ; Write X,Y to string buffer
    ld      a,c
    add     8                     ; Add 8 to Xoffset
    jp      c,FCERR               ; Error if > 255
    ld      c,a
    ld      a,(XTEMP1)            ; A = MaxXoffset
    cp      c                     
    jr      nc,.skipx             ; If Xoffset > MaxXoffset
    ld      a,c
    ld      (XTEMP1),a            ;   MaxXoffset = Xoffset
.skipx
    call    inc_xtemp0            ; Increment SptlCnt
    ld      a,(hl)
    cp      ','                   ; If comma
    jr      z,.gloop              ;   Get next sprite in row
    SYNCHK  ']'                   ; Else require close brace
    jr      z,.done               ; If not end of statement
    SYNCHK  ','                   ;   Require comma
    SYNCHK  '['                   ;   And open brace
    ld      a,b
    add     8                     ;   Add 8 to Yoffset
    jp      c,FCERR               ;   Error if > 255
    ld      b,a
    ld      c,0                   ;   Reset Xoffset to 0
    jr      .loop                 ;     and get next sprite
.done
    ld      a,(XTEMP1)            
    ld      c,a                   ; C = MaxXoffset
    jr      _sprite_done

    
;-----------------------------------------------------------------------------
; SET SPRITE sprite$ [ON|OFF] [POS x,y] [TILE tilelist$] [PALETTE palettelist$] [ATTR attrlist$]
; SET SPRITE sprite$ TILECLIP tileclip$
; SET SPRITE sprite$ TO proplist$
; SET SPRITE * OFF|CLEAR
; Attributes: Priority (64), Double-Height (8), Vertical Flip (4), Horizontal Flip (2)
; ToDo: SET SPRITE spritle# ...
;-----------------------------------------------------------------------------
ST_SET_SPRITE:
    rst     CHRGET                ; Skip SPRITES
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
    rst     CHRGET                ; Skip PALETTE
    ld      ix,sprite_set_palettes; IX = jump address
    jr      .string_arg

.tiles
    rst     CHRGET                ; Skip TILE
    cp      XTOKEN                ; If followed by extended token
    jr      z,.tilex              ;   Go handle it
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

; On entry: HL = TxtPtr; Stack = SprAdr, RtnAdr
.tilex   
    rst     CHRGET                ; Skip XTOKEN
    rst     SYNCHR
    byte    CLIPTK                ; Require CLIP
    dec     hl                    ; Back up for CHRGET
    byte    $F6                   ; OR A,$AF over XOR A
.props                            
    xor     a                     ; ClpFlg = False
    push    af                    ; Stack = ClpFlg, SprAdr, RtnAdr
    rst     CHRGET                ; Skip TO
    call    get_string_arg        ; BC = StrLen, DE = StrAdr, HL = StrDsc; Stack = TxtPtr, ClpFlg,SprAdr, RtnAdr
    pop     hl                    ; HL = TxtPtr; Stack = ClpFlg, SprAdr, RtnAdr
    pop     af                    ; AF = ClpFlg; Stack = SprAdr, RtnAdr
    jr      z,.notclip            ; If TILECLIP
    inc     de
    inc     de                    ;   Skip clip dimensions
    dec     bc
    dec     bc                    ;   Adjust string length
.notclip    
    ex      (sp),hl               ; HL = SprAdr; Stack = TxtPtr, RtnAdr
    call    sprite_set_props   
    jp      nz,FCERR
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
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
    ld      a,(hl)
    cp      '$'                   ; 
    push    af                    ; Stack = '$', RtnAdr
    call    z,CHRGTR              ; If $, skip it
    SYNCHK  '('                   ; Require Open Paren
    call    _get_rgb              ; DE = RGB value
    SYNCHK  ')'                   ; Require Close Paren
    pop     af                    ; A = '$'; Stack = RrnAdr
    push    hl                    ; Stack = TxtPtr, RtnAdr
    jr      z,.string             ; If not RGB$()
    ld      bc,LABBCK             
    push    bc                    ;   Push return address for FLOAT_DE
    jp      FLOAT_DE              ;   Float RGB and return.
.string    
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

test_gs_init:
    push    hl
    ld      iy,get_set_init
    call    aux_call
    ld      a,0
    rla                           ; A = Carry
    ld      (BMPMODE),a           ; BMPMODE = Result
    pop     hl
    ret

