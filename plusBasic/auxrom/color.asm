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
    ld      hl, .default_palette
.palloop:
    out     (c), b
    ld      a, (hl)
    out     (IO_VPALDATA), a
    inc     hl
    inc     b
    dec     d
    jr      nz, .palloop
    ret

.default_palette:
    dw $111, $F11, $1F1, $FF1, $22E, $F1F, $3CC, $FFF
    dw $CCC, $3BB, $C2C, $419, $FF7, $2D4, $B22, $333
;    dw $111                       ;  0 BLACK    
;    dw $F11                       ;  1 RED      
;    dw $1F1                       ;  2 GREEN    
;    dw $FF1                       ;  3 YELLOW   
;    dw $22E                       ;  4 BLUE     
;    dw $F1F                       ;  5 MAGENTA  
;    dw $3CC                       ;  6 CYAN     
;    dw $FFF                       ;  7 WHITE    
;    dw $CCC                       ;  8 GREY     
;    dw $3BB                       ;  9 DKCYAN   
;    dw $C2C                       ; 10 DKMAGENTA
;    dw $419                       ; 11 DKBLUE   
;    dw $FF7                       ; 12 LTYELLOW
;    dw $2D4                       ; 13 DKGREEN
;    dw $B22                       ; 14 DKRED
;    dw $333                       ; 15 DKGREY


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
