;====================================================================
; Statements and Functions from Aquarius Extended BASIC
;====================================================================

;-----------------------------------------------------------------------------
; CLS statement 
; syntax: CLS [fgcolor, bgcolor]
;-----------------------------------------------------------------------------

ST_CLS:
    jp      z,do_cls_default      ; no parameters, use default
    call    get_byte16             ; get foreground color
    push    af                    ; save it
    SYNCHK  ','                   ; require commae
    call    get_byte16             ; get background color
    pop     af                    ; get back foreground color
    or      a                     ; clear carry
    rla       
    rla       
    rla       
    rla                           ; shift to high nybble
    or      e                     ; combine background color
    jp      do_cls                ; Clear screen and homecursor

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
