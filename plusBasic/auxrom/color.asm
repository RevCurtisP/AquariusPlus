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

;-----------------------------------------------------------------------------
; Convert Binary RGB list to ASCII
; Input: C: Entry Count
;       DE: Binary Data Address
;       HL: ASCII Buffer Address
; Output: A: ASCII data length
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
rgb_to_asc:
    xor     a                     ; AscLen = 0
    inc     c                     ; Bump for countdown
.loop
    dec     c
    ret     z
    ex      af,af'                ; A' = AscLen
    ld      a,'#'
    ld      (hl),a                ; Write #
    inc     hl    
    ld      a,(de)                ; A = GB
    inc     de                    ; 
    push    af                    ; Stack = GB, RtnAdr
    ld      a,(de)                ; A = R
    inc     de
    call    .low_nybble           ; Write RR
    pop     af                    ; A = GB; Stack = RtnAdr
    push    af                    ; Stack = GB, RtnAdr
    call    .high_nybble          ; Write GG
    pop     af                    ; A = GB; Stack = RtnAdr
    call    .low_nybble
    call    _crlf                 ; Write CR+LF
    ex      af,af'                ; A = AscLen
    add     9                     ; AscLen += LinLen
    jr      .loop
.low_nybble
    and     $0F
    ld      b,a
    rla
    rla
    rla
    rla
    jr      .to_hex
.high_nybble
    and     $F0
    ld      b,a
    rra
    rra
    rra
    rra
.to_hex
    or      b
    jp      byte_to_hex
    
_crlf:
    ld      a,13
    ld      (hl),a                ; Write CR
    inc     hl    
    ld      a,10
    ld      (hl),a                ; Write LF
    inc     hl    
    ret
    
;-----------------------------------------------------------------------------
; Convert ASCII RRGGBB to binary GB0R
; Input: DE: Binary Data Pointer
;        HL: ASCII Buffer Address
; Output: DE: Updated Binary Data Pointer
; Flags: Carry set if invalid line
; Clobbered: BC, HL
;-----------------------------------------------------------------------------
asc_to_rgb:
    call    read_nybbles          ; B,C = hiRed,lowRed
    ret     c
    push    bc                    ; Stack = Red, RtnAdr
    call    read_nybbles          ; B,C = hiGreen, lowGreen
    jp      c,discard_ret
    ld      a,b                   ; A = hiGreen
    rla
    rla
    rla
    rla                           
    ld      c,a                   ; C = G0
    call    read_nybbles          ; B,C = hiBlue, lowBlue
    jp      c,discard_ret
    ld      a,b                   ; A = hiBlue
    or      c                     ; A = GB
    ld      (de),a
    inc     de
    pop     bc                    ; BC = Red; Stack = RtnAdr
    ld      a,b                   ; A = hiRed
    ld      (de),a
    inc     de
    ret

; HL = AscPtr; Output: B: First nybble, A: Second nybble    
read_nybbles
    call    read_hex_nybble
    ld      b,a                   
    call    nc,read_hex_nybble
    ret

read_hex_nybble:    
    ld      a,(hl)                ; A = First hex digit
    inc     hl
hex_to_nybble:
    call    uppercase_char
    sub    '0'                    ; Convert digit to bytes
    ret     c                     ; Return carry set if < 0
    cp      10                    ; If <= 9
    ccf
    ret     nc                    ;   Return it
    sub    'A'-'0'                ; Convert A-F to 0-5
    ret     c                     ; Return carry set if < 0
    add     10                    ; Convert A-F to 10-15
    cp      16                    ; Return carry if > 15
    ccf
    ret
