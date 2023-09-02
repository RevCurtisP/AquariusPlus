;====================================================================
; Statements and Functions from Aquarius Extended BASIC
;====================================================================

;-----------------------------------------------------------------------------
; CLS statement 
; syntax: CLS [fgcolor, bgcolor]
;-----------------------------------------------------------------------------

ST_CLS:
    jp      z,do_cls_default      ; no parameters, use default
    call    get_color             ; get foreground color
    push    af                    ; save it
    SYNCHK  ','                   ; require commae
    call    get_color             ; get background color
    pop     af                    ; get back foreground color
    or      a                     ; clear carry
    rla       
    rla       
    rla       
    rla                           ; shift to high nybble
    or      e                     ; combine background color
    jp      do_cls                ; Clear screen and homecursor


get_color:
    call    GETBYT        ; get foreground color in e
    cp      16            ; if > 15
    jp      nc,FCERR      ;   FC Error
    ret

