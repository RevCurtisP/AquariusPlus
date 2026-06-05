;====================================================================
; Screen Statements and Functions
;====================================================================


;-----------------------------------------------------------------------------
; ? GETBORDERCHR
; ? GETBORDERCOLOR
; ? GETBORDERCHR$
; ? GETBORDERCOLOR$
;-----------------------------------------------------------------------------
FN_GETBORDER:
    rst     CHRGET                ; Skip BORDER
    ld      iy,border_get_chr
    cp      CHRSTK
    jr      z,skip_aux_call_bufin
    cp      COLTK
    jr      z,.color              ; If Not COL
    SYNCHKT XTOKEN
    SYNCHKT CHRTK
    jr      .getborder
.color
    rst     CHRGET                ; Skip COL
    SYNCHKT ORTK                  ; Require OR
    ld      iy,border_get_color
.getborder
    cp      '$'
    jp      nz,gfxrom_call_sngflt
skip_aux_call_bufin:
    rst     CHRGET                ; Skip '$'
aux_call_bufcin:
    push    hl
    call    aux_call
    jp      BUFCIN



;-----------------------------------------------------------------------------
; Set border to character to space and colors to default
;-----------------------------------------------------------------------------
ST_RESET_BORDER:
    rst     CHRGET                ; Skip BORDER
    push    hl
    ld      iy,border_reset
    jp      gfxrom_call_popret

;-----------------------------------------------------------------------------
; SET BORDER {CHR byte} {COLOR fg,bg}
; SET BORDER CHR '@'
; SET BORDER COLOR 7,0
; SET BORDER CHR '*' COLOR 0,7
;-----------------------------------------------------------------------------
ST_SET_BORDER:
    rst     CHRGET                ; Skip BORDER
    cp      COLTK
    jr      z,.color              ; If Not COL
    call    parse_char            ;   A = Character
    ld      iy,border_set_chr     ;   Set border color and return
    call    gfxrom_call_preserve_hl
    ld      a,(hl)
    or      a                     ;   Return if end of statement
    ret     z
.color
    call    parse_colors          ; A = Colors
    ld      iy,border_set_color   ; Set border color and return
    jp      gfxrom_call_preserve_hl

