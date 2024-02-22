;=============================================================================
; Color and Palette Assembly Routines
;=============================================================================

;-----------------------------------------------------------------------------
; Reset palette to default colors
; Input: A: Palette#
; Clobbers: A,BC,D,HL
;-----------------------------------------------------------------------------
palette_reset:
    call    mult_a_32
    ld      b,a
    ld      c,IO_VPALSEL
    ld      d,32
    ld      hl, default_palette
.palloop:
    out     (c), b
    ld      a, (hl)
    out     (IO_VPALDATA), a
    inc     hl
    inc     b
    dec     d
    jr      nz, .palloop
    ret

default_palette:
    dw $111, $F11, $1F1, $FF1, $22E, $F1F, $3CC, $FFF
    dw $CCC, $3BB, $C2C, $419, $FF7, $2D4, $B22, $333


;-----------------------------------------------------------------------------
; Set palette
; Input: A: Entry#               VPALSEL   ~PPEEEEE : P = Palette#, E = Entry#
;       BC: Data Length          VPALDATA  GGGGBBBB : G = Green, B = Blue 
;       DE: Data Address                   ~~~~RRRR : R = Red
;        L: Palette# (shifted if Carry Set)
;           
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
palette_set:
    push    af
    jr      c,.shifted
    ld      a,l
    call    mult_a_32             ; B = Shifted palette #
    ld      l,a                   ; Save It
.shifted:
    pop     af
    and     $0F                   ; Mask it
    rla                           ; Multiply by 2
    or      l                     ; Combine with palette #
    ld      l,a
.loop
    ld      a,b
    or      c
    ret     z
    ld      a,l
    out     (IO_VPALSEL),a        ; Select Palette and Entry
    ld      a,(de)
    out     (IO_VPALDATA),a       ; Write Green and Blue
    inc     l
    inc     de
    dec     bc
    jr      .loop

;-----------------------------------------------------------------------------
; Get palette
; Input: A: Palette#
;       BC: Data Length      
;       DE: Data Address
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
palette_get:
    call    mult_a_32             ; A = Shifted palette #
    ld      l,a                   ; Save It
.loop
    ld      a,b
    or      c
    ret     z
    ld      a,l
    out     (IO_VPALSEL),a        ; Select Next Register
    in      a,(IO_VPALDATA)       ; Read Palette byte
    ld      (de),a
    inc     l
    inc     de
    dec     bc
    jr      .loop
