;====================================================================
; Graphics Statements and Functions
;====================================================================

;-----------------------------------------------------------------------------
; COLOR statement 
; syntax: [SET] COLOR [#] palette [, index] [TO | ;] rgb, ...
;         [SET] COLOR [#] palette [, index] [TO | ;] rgblist$
;-----------------------------------------------------------------------------
ST_COLOR:
    call    SYNCHR                ; Require OR after COL
    byte    ORTK                  
    cp      '#'                   ; If followed with '#'
    jr      z,ST_SETCOLOR         ;   Set palette
    jp      SNERR                 ; No other syntax supported yet
    rst     CHRGET                ; Skip '#'
ST_SETCOLOR:
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
    call    palette_shift_num     ; 
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
    call    palette_set_entry     ; Set the entry (increments C)
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
    call    palette_set           ; Set the entry (increments C)
    jp      c,OVERR               ; Error if Overflow
    pop     hl                    ; HL = TxtPtr
    ret
    
    
;-----------------------------------------------------------------------------
; SCREEN statement 
; syntax: SCREEN mode
;              b p s gm t
;   mode:  0 ( 0 0 0 00 1) Text 
;          1 ( 0 0 0 01 0) 64 x 32 Color Tilemap
;          2 ( 0 0 0 10 0) 320 x 200 Bitmap mode ON
;          3 ( 0 0 0 11 0) Multicolor Bitmap Mode
;         +4 ( 0 x 1 xx x) Sprites On
;         +8 ( 0 0 x xx 1) Text Behind Graphics
;        +16 ( 0 1 x xx 1) Text in Front of Graphics
;        +24 ( 1 x x xx x) Remap Border Character
;-----------------------------------------------------------------------------


ST_SCREEN:
    call    GETBYT                ; Get Mode
    push    hl                    ; Stack = TxtPtr, RtnAdr
    cp      24                    ; If greater than 23
    jp      nc,FCERR              ;   Illegal quantity error
    call    screen_set_mode       ; 
    pop     hl                    ; HL - TxtPtr; Stack = RtnAdr
    ret

;-----------------------------------------------------------------------------
; SET TILE Statement
; SETTILE tile# TO color_index, ...
; SETTILE tile# TO tiledata$
;-----------------------------------------------------------------------------
ST_SETTILE:
    rst     CHRGET                ; Skip TILE Token
    call    GETINT                ; Get Tile#
    push    de                    ; Stack = Tile#, RtnAdr
    rst     SYNCHR                ; Require TO
    byte    TOTK
    call    FRMEVL                ; Get DataAddr or String
    call    GETYPE                ; If a String
    jr      z,.string             ;   Process and set Tile
    ld      e,0                   ; Pixel# = 0
    push    de                    ; Stack = Pixel#, Tile#, RtnAdr
    call    CONINT
    jr      .skip
.loop    
    call    GETBYT                ; A = Color Index
.skip
    cp      16
    jp      nc,FCERR              ; Error if > 15
    ld      c,a                   ; C = Color
    pop     de                    ; DE = Pixel#; Stack = Tile#, RtnAdr
    ex      (sp),hl               ; HL = Tile#, Stack = TxtPtr, RtnAdr
    call    tile_set_pixel
    inc     e                     ; Increment Pixel#
    ld      a,16
    cp      e
    jp      c,OVERR               ; Error if > 15
    ex      (sp),hl               ; HL = TxtPtr, Stack = Tile#, RtnAdr
    push    de                    ; Stack = Pixel#, Tile#, RtnAdr
    call    CHRGT2                ; Reget current character
    jr      z,.done               ; Finish up if terminator
    SYNCHK  ','                   ; Require comma
    jr      .loop
.done
    pop     bc                    ; Stack = Tile#, RtnAdr
    pop     bc                    ; Stack = RtnAdr
    ret
.string:
    ex      (sp),hl               ; HL = Tile#; Stack = TxtPtr, RtnAdr
    push    hl                    ; Stack = Tile#, TxtPtr, RtnAdr
    call    free_addr_len         ; DE = DataAddr, BC = Count
    pop     hl                    ; HL = Tile#; Stack = TxtPtr, RtnAdr
.set_it:
    call    tile_set      
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
; DEF COLORLIST T$ = palette#, palette#, ...
; tile# is a integet between 0 and 511
;-----------------------------------------------------------------------------
ST_DEFCOLOR:
    rst     CHRGET                ; Skip COL
    rst     SYNCHR
    byte    ORTK                  ; Require OR
    call    _setupdef
.loop
    call    GETBYT                ; Get palette#
    cp      4                     ; If >=4
    jp      nc,FCERR              ;   Error
    call    sbuff_write_byte      ; Write it to string buffer
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
    call    _setupdef             ; Stack = VarPtr, RetAdr
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
_defnolist:
    call    get_stringvar         ; Get string variable address
    pop     bc                    ; BC = ThisRet; Stack = RetAdr
    push    de                    ; Stack = VarPtr, RetAdr
    push    bc                    ; Stack = ThisRet, VarPtr, RetAdr
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
; GETCOLOR Function 
; GETCOLOR$(palette#)
; palette# is a integet between 0 and 3
;-----------------------------------------------------------------------------
FN_GETCOL:
    rst     CHRGET                ; Skip COL token
    rst     SYNCHR
    byte    ORTK
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
; DEF SPRITE sprite$ = spritle#, x-offset, y-offset; spritle#, x-offset, y-offset;
;-----------------------------------------------------------------------------
ST_DEFSPRITE:
    rst     CHRGET                ; Skip SPRITE
    call    _defnolist            ; Stack = VarPtr, RetAdr
    xor     a                     ; A = SptlCnt (0)
    push    af                    ; Stack = SptlCnt, VarPtr, RetAdr
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
    pop     af                    ; A = SptlCnt; Stack = VarPtr, RetAdr
    inc     a                     ; SptlCnt += 1
    push    af                    ; Stack = SptlCnt, VarPtr, RetAdr
    call    CHRGT2
    jr      z,.done
    SYNCHK  ';'
    jr      .loop
.done
    ld      de,0                  ; DE = BuffOffset (0)
    pop     af                    ; A = SptlCnt; Stack = VarPtr, RetAdr
    call    sbuff_write_byte_ofs  ; Write it
    ld      a,c                   ; 
    add     a,8                   ; A = MaxXoffset + Spritle Width
    call    sbuff_write_byte_ofs  ; Write it
    ld      a,b                   ; 
    add     a,8                   ; A = MaxYoffset + Spritle Width
    call    sbuff_write_byte_ofs  ; Write it
    jp      _finish_def           ; A = SptlCnt; Stack = VarPtr, RetAdr

_get_byte:
    push    bc                ; Stack = MaxOffsets
    call    GETBYT            ; Get spritle#
    pop     bc                ; BC = MaxOffsets
    ret

;-----------------------------------------------------------------------------
; SET SPRITE sprite$ [ON/OFF] [POS x,y] [TILE tilelst$] COLOR [colorlist$] [ATTR attrlist$]
; Attributes: Priority (64), Double-Height (8), Vertical Flip (4), Horizontal Flip (2)
;-----------------------------------------------------------------------------
ST_SETSPRITE:
    rst     CHRGET                ; Skip SPRITE
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
.loop
    cp      ';'
    jr      z,.nextsprite
    cp      POSTK
    jr      z,.pos
    cp      ATTRTK                
    jr      z,.attrs
    cp      COLTK 
    jr      z,.color
    cp      TILETK 
    jr      z,.tiles
    cp      ONTK
    jr      z,.on
    cp      OFFTK
    jr      z,.off
    jp      SNERR
    
.nextsprite    
    pop     de                    ; HL = TxtPtr; Stack = RtnAdr
    jr      ST_SETSPRITE
    
.color
    rst     CHRGET                ; Skip COL
    rst     SYNCHR                ; Require OR
    byte    ORTK
    ld      ix,sprite_set_colors  ; IX = jump address
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
