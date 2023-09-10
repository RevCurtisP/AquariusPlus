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
    cp      ATTRTK
    jp      z,ST_DEFATTR            ; DEF ATTRLIST
    cp      COLTK
    jp      z,ST_DEFCOLOR           ; DEF COLORLIST
    cp      INTTK         
    jp      z,ST_DEFINT             ; DEF INTLIST
    cp      TILETK
    jp      z,ST_DEFTILE          ; DEF TILELIST
    jp      SNERR
