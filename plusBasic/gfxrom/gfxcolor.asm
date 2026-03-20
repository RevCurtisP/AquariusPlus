;=============================================================================
; Color and Palette Assembly Routines
;=============================================================================


;-----------------------------------------------------------------------------
; Reset all palettes to default colors
; Input: A: Palette# (* = All)
; Clobbers: A,BC,DE,HL
;-----------------------------------------------------------------------------
palette_reset_all:
    ld      b,0
    ld      e,4
.alloop
    call    _do_palette_reset
    dec     e
    jr      nz,.alloop
    ret

;-----------------------------------------------------------------------------
; Reset palette to default colors
; Input: A: Palette# (* = All)
; Clobbers: A,BC,D,HL
;-----------------------------------------------------------------------------
palette_reset:
    call    mult_a_32
    ld      b,a
_do_palette_reset:
    ld      c,IO_VPALSEL
    ld      d,32
    ld      hl,default__palette
.palloop:
    out     (c),b
    ld      a,(hl)
    out     (IO_VPALDATA), a
    inc     hl
    inc     b
    dec     d
    jr      nz, .palloop
    ret

default__palette:
    dw $111, $F11, $1F1, $FF1, $22E, $F1F, $3CC, $FFF
    dw $CCC, $3BB, $C2C, $419, $FF7, $2D4, $B22, $333
