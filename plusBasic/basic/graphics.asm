;====================================================================
; Graphics Statements and Functions
;====================================================================

; Graphics module jump table offsets
    _startup            equ $00
    _init               equ $03
    _set_screen_mode    equ $06 
    _shift_palette_num  equ $09 
    _set_palette_entry  equ $0C 
    _set_palette        equ $0F
    _get_palette        equ $12
    _set_tile_pixel     equ $15
    _set_tile           equ $18
    _get_tile           equ $1B

;-----------------------------------------------------------------------------
; COLOR statement 
; syntax: [SET] COLOR [#] palette [, index] [TO | ;] rgb, ...
;         [SET] COLOR [#] palette [, index] [TO | ;] rgblist$
;-----------------------------------------------------------------------------
ST_COLOR:
    call    SYNCHR                ; Require OR after COL
    byte    ORTK                  
    cp      '#'                   ; If followed with '#'
    jr      z,st_set_palette      ;   Set palette
    jp      SNERR                 ; No other syntax supported yet
    rst     CHRGET                ; Skip '#'
st_set_palette:
    call    GETBYT                ; E = Palette#
    ld      c,0                   ; Default Entry# to 0
    ld      a,(hl)
    cp      ','                   ; If followed by comma
    jr      nz,.not_comma         ;
    rst     CHRGET                ; Eat It
    push    de                    ; Stack = Palette#
    call    GETBYT                ; E = Entry#
    cp      16                    ; If greater than 15
    jp      nc,FCERR              ;   Error out
    ld      c,e                   ; C = Entry #
    pop     de                    ; DE = Palette#
.not_comma:
    ld      a,(hl)                ; Get next character
    cp      TOTK                  ; Must be followed by
    jr      nz,.notto             ; either TO
    rst     CHRGET
    jr      .wasto
.notto
    SYNCHK  ';'                   ; or semicolon
.wasto
    ld      a,e                   ; Get palette#
    cp      4                     ; If greater than 3
    jp      nc,FCERR              ;   Error
    ld      ix,_shift_palette_num
    call    do_gfx_routine        ; 
    ld      b,a                   ; B = Shifted palette#
    push    bc                    ; Stack = PltOfs+Entry
    call    FRMEVL                ; Get DataAddr or String
    call    GETYPE                ; If a String
    jr      z,.string             ;   Process and set Tile
    call    FRCINT
    jr      .skip
.loop    
    call    GETINT                ; DE = RGB palette entry
.skip
    pop     bc                    ; Get back palette select
    scf
    ld      ix,_set_palette_entry
    call    do_gfx_routine        ; Set the entry (increments C)
    push    bc                    ; Save palette select again
    call    CHRGT2                ; Reget current character
    jr      z,.done               ; Finish up if terminator
    SYNCHK  ','                   ; Require comma
    jr      .loop
.done
    pop     bc                    ; Take palette select off stack
    ret

.string:
    ex      (sp),hl               ; HL = PltOfs+Entry; Stack = TxtPtr
    push    hl                    ; Stack = PltOfs+Entry, TxtPtr
    call    free_addr_len         ; DE = DataAddr, BC = Count
    pop     hl                    ; HL = PltOfs+Entry; Stack = TxtPtr
    ld      a,l                   ; A = Entry#
    ld      l,h                   ; L = PltOfs
    scf                           ; Palette already shifted
    ld      ix,_set_palette
    call    do_gfx_routine        ; Set the entry (increments C)
    jp      c,OVERR               ; Error if Overflow
    pop     hl                    ; HL = TxtPtr
    ret
    
    
;-----------------------------------------------------------------------------
; SCREEN statement 
; syntax: SCREEN mode
;             p s gm t
;   mode:  0 (0 0 00 1) Text 
;          1 (0 0 01 0) 64 x 32 Color Tilemap
;          2 (0 0 10 0) 320 x 200 Bitmap mode ON
;          3 (0 0 11 0) Multicolor Bitmap Mode
;         +4 (x 1 xx x) Sprites On
;         +8 (0 x xx 1) Text Behind Graphics
;        +16 (1 x xx 1) Text in Front of Graphics
;-----------------------------------------------------------------------------


ST_SCREEN:
    call    GETBYT                ; Get Mode
    push    hl
    cp      24                    ; If greater than 23
    jp      nc,FCERR              ;   Illegal quantity error
    ld      ix,_set_screen_mode   ; 
    call    do_gfx_routine        ; Set Screen Mode
    pop     hl
    ret

;-----------------------------------------------------------------------------
; SET TILE Statement
; SETTILE tile# TO color_index, ...
; SETTILE tile# TO tiledata$
;-----------------------------------------------------------------------------
SET_TILE:
    rst     CHRGET                ; Skip TILE Token
    call    GETINT                ; Get tile index
    push    DE                    ; Stack = Tile#
    rst     SYNCHR                ; Require TO
    byte    TOTK
    call    FRMEVL                ; Get DataAddr or String
    call    GETYPE                ; If a String
    jr      z,.string             ;   Process and set Tile

.string:
    ex      (sp),hl               ; HL = Tile#; Stack = TxtPtr
    push    hl                    ; Stack = Tile#, TxtPtr
    call    free_addr_len         ; DE = DataAddr, BC = Count
    pop     hl                    ; HL = Tile#; Stack = TxtPtr
.set_it:
    ld      ix,_set_tile      
    call    do_gfx_routine        ; Set Tile Data
    jp      c,OVERR               ; Error if Overflow
    pop     hl                    ; HL = TxtPtr
    ret

;-----------------------------------------------------------------------------
; DEF ATTRLIST T$ = attr#, attr#, ...
; attr# is an even integet between 0 and 127
;   Bit 6 = Priority
; Bit 4-5 = Palette# 
;   Bit 4 = Double Height (sprite only)
;   Bit 3 = Vertical Flip
;   Bit 2 = Horizontal Flip
;-----------------------------------------------------------------------------
ST_DEFATTR:
    rst     CHRGET                ; Skip COL
    rst     SYNCHR
    byte    ORTK                  ; Require OR
    call    _setupdef
.loop
    call    GETBYT                ; Get Attribute
    and     $7E                   ; Strip Index MSB
    call    sbuff_write_de        ; Write it to string buffer
    call    CHRGT2                ; Reget next character
    jr      z,_finish_def         ; If not end of statement
    SYNCHK  ','                   ;   Require comma
    jr      .loop                 ;   and get next tile#
    
;-----------------------------------------------------------------------------
; DEF COLORLIST T$ = tile#, tile#, ...
; tile# is a integet between 0 and 511
;-----------------------------------------------------------------------------
ST_DEFCOLOR:
    rst     CHRGET                ; Skip COL
    rst     SYNCHR
    byte    ORTK                  ; Require OR
    call    _setupdef
.loop
    call    get_byte16            ; Get color
    call    sbuff_write_de        ; Write it to string buffer
    call    CHRGT2                ; Reget next character
    jr      z,_finish_def         ; If not end of statement
    SYNCHK  ','                   ;   Require comma
    jr      .loop                 ;   and get next color    
;-----------------------------------------------------------------------------
; DEF INTLIST I$ = int, int, ...
; int is a integet between 0 and 65535
;-----------------------------------------------------------------------------
ST_DEFINT:
    rst     CHRGET                ; Skip INT
    call    _setupdef             ; Stack = RetAdr, VarPtr
.loop
    call    GETINT                ; Get Integer
    call    sbuff_write_de        ; Write it to string buffer
    call    CHRGT2                ; Reget next character
    jr      z,_finish_def         ; If not end of statement
    SYNCHK  ','                   ;   Require comma
    jr      .loop                 ;   and get next tile#

_finish_def:
    ex      (sp),hl               ; HL = VarPtr; Stack = TxtPtr  
    push    hl                    ; Stack = VarPtr, TxtPtr
    call    sbuff_create_string   ; Copy Buffer to Temporary. Return HL = StrDsc
    push    hl                    ; Stack = StrDsc, VarPtr, TxtPtr
    jp      INBUFC                ; Copy Temporary to Variable and hwx:

_setupdef:
    rst     SYNCHR
    byte    LISTTK                ; Require LIST
    call    get_stringvar         ; Get string variable address
    pop     bc                    ; BC = RetAdr
    push    de                    ; Stack = VarPtr
    push    bc                    ; Stack = RetAdr, VarPtr
    rst     SYNCHR            
    byte    EQUATK                ; Require '='
    jp      sbuff_init            ; Init string buffer and return

;-----------------------------------------------------------------------------
; DEF TILELIST T$ = tile#, tile#, ...
; tile# is a integet between 0 and 511
;-----------------------------------------------------------------------------
ST_DEFTILE:
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
; TILEGET Function 
; TILEGET$(tile#)
; tile# is a integet between 0 and 511
;-----------------------------------------------------------------------------
FN_TILE:
    rst     CHRGET                ; Skip Tile token
    rst     SYNCHR
    byte    GETTK
    SYNCHK  '$'
    SYNCHK  '('
    call    get_int512            ; DE = Tile#
    SYNCHK  ')'
    push    hl                    ; Stack = TxtPtr
    push    hl                    ; Stack = DummyAdr, TxtPtr
    push    de                    ; Stack = Tile#, DummyAdr, TxtPtr
    ld      a,32                  ; Reading 32 bytes

    call    STRINI                ; Create TmpStr; HL = StrDsc, DE = StrAdr
    ld      bc,32                 ; BC = StrLen
    pop     hl                    ; HL = Tile#; Stack = DummyAdr, TxtPtr
    ld      ix,_get_tile
    call    _get_tile             ; Read Tile data
    ld      a,1
    ld      (VALTYP),a            ; Set Type to String
    call    FRESTR                ; Free Temporary
    jp      FINBCK                ; Return String

;-----------------------------------------------------------------------------
; COLORGET Function 
; COLORGET$(palette#)
; palette# is a integet between 0 and 3
;-----------------------------------------------------------------------------
FN_COLOR:
    rst     CHRGET                ; Skip Tile token
    rst     SYNCHR
    byte    ORTK
    rst     SYNCHR
    byte    GETTK
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
    ld      ix,_get_palette
    call    do_gfx_routine        ; Set the entry (increments C)
    ld      a,1
    ld      (VALTYP),a            ; Set Type to String
    call    FRESTR                ; Free Temporary
    jp      FINBCK                ; Return String

