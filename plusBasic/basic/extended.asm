;====================================================================
; Statements and Functions from Aquarius Extended BASIC
;====================================================================

;-----------------------------------------------------------------------------
; CLS statement 
; syntax: CLS [fgcolor, bgcolor]
;-----------------------------------------------------------------------------

ST_CLS:
    jp      z,do_cls              ; No parameters, use default
    call    get_color_args        ; Pares foreground and background colors
    jp      clear_home            ; Clear screen and homecursor

;-----------------------------------------------------------------------------
; DEF statement stub
;-----------------------------------------------------------------------------
ST_DEF:
    cp      INTTK         
    jp      z,ST_DEFINT             ; DEF INTLIST
    cp      TILETK
    jp      z,ST_DEF_TILELIST       ; DEF TILELIST
    cp      RGBTK                   
    jp      z,ST_DEFRGB             ; DEF RGBLIST
    cp      XTOKEN          
    jp      nz,SNERR                ; Extended Token Prefix
    inc     hl
    ld      a,(hl)                  ; Get Extended Token
    cp      SPRITK
    jp      z,ST_DEFSPRITE          ; DEF TILELIST
    cp      ATTRTK
    jp      z,ST_DEFATTR            ; DEF ATTRLIST
    cp      PALETK
    jp      z,ST_DEFPALETTE         ; DEF PALETTELIST
    jp      SNERR
