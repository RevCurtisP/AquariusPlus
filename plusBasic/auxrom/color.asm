;=============================================================================
; Color and Palette Assembly Routines
;=============================================================================

;-----------------------------------------------------------------------------
; Set palette
; Input: A: Entry#      
;       BC: Data Length      
;       DE: Data Address
;        L: Palette# (shifted if Carry Set)
;           
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
palette_set:
    push    af
    jr      c,.shifted
    ld      a,l
    call    palette_shift_num ; B = Shifted palette #
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
    call    palette_shift_num ; A = Shifted palette #
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

;-----------------------------------------------------------------------------
; Set palette entry                        76543210
; Input: A: Palette#            VPALSEL   ~PPEEEEE : P = Palette#, E = Entry#
;        B: Shifted palette#    VPALDATA  GGGGBBBB : G = Green, B = Blue
;        C: Entry#                        ~~~~RRRR : R = Red
;       DE: Entry               
;       Use A if Carry is Clear, B if Carry is Set
; Output: B: Shifted palette#
;         C: Next entry#
;-----------------------------------------------------------------------------
    palette_set_entry:
    jr      c,.use_b
    call    palette_shift_num     ; B = Shifted palette #
    ld      b,a                   ; Save It
.use_b:
    push    a
    ld      a,c                   ; Get Entry #
    and     $0F                   ; Mask it
    rla                           ; Multiply by 2
    or      b                     ; Combine with palette #
    out     (IO_VPALSEL),a        ; Select Palette and Entry
    ex      af,af'
    ld      a,e
    out     (IO_VPALDATA),a       ; Write Green and Blue
    ex      af,af'
    inc     a
    out     (IO_VPALSEL),a        ; Select Palette and Entry
    ld      a,d                   ; 
    out     (IO_VPALDATA),a       ; Write Red
    inc     c
    ex      af,af'
    pop     a
    ret

;-----------------------------------------------------------------------------
; Shift Palette Number for ORing with palette index address
; Input:  A: Palette#            
; Output: A: Shifted palette#    
;-----------------------------------------------------------------------------
palette_shift_num:
    and     $03                   ; Remove extraneous b
    rla                           ; Shift palette # to bits 5 and 6
    rla
    rla
    rla
    rla
    ret
