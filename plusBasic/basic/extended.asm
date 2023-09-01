;====================================================================
; Statements and Functions from Aquarius Extended BASIC
;====================================================================

;-----------------------------------------------------------------------------
; CLS statement 
; syntax: CLS [fgcolor, [bgcolor]]
;-----------------------------------------------------------------------------

ST_CLS:
    ld      a,6                   ; default to black on cyan
    jr      z,do_cls              ; no parameters, use default
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
; Clear screen and homecursor
do_cls:
    call    clearscreen
    ld      (CURCHR),a            ; SPACE under cursor
    ld      de,$3000+41           ; Point Address for (0,0) 
    ld      (CURRAM),de       
    xor     a
    ld      (TTYPOS),a            ; column 0
    ret

clearscreen:
    push    hl
    ld      hl,$3000
    ld      c,25
.line:
    ld      b,40
.char:
    ld      (hl),' '
    set     2,h
    ld      (hl),a
    res     2,h
    inc     hl
    djnz    .char
    dec     c
    jr      nz,.line
    pop     hl
    ret

get_color:
    call    GETBYT        ; get foreground color in e
    cp      16            ; if > 15
    jp      nc,FCERR      ;   FC Error
    ret

