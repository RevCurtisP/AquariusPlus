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
; Input: B: (ToDo) Prefix
;        C: Entry Count
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
    call    rgb_hex
    ex      af,af'                ; A = AscLen
    call    _crlf                 ; Write CR+LF
    ex      af,af'                ; A = AscLen
    add     a,2
    jr      .loop

; Convert Palette entry to "RRGGBB"
; Input: B: Prefix (ToDo)
;       DE: BinPtr, HL: BufPtr
; Preserve A, BC
rgb_hex:
    ex      af,af'                ; A' = AscLen
    ld      a,b                   ; A = PfxChr
    or      a
    jr      z,.nopfx              ; If PfxChr <> NUL
    ld      (hl),a                ;   Write it
    inc     hl
    ex      af,af'                ;   A = AscLen
    inc     a
    ex      af,af'                ;   A' = AscLen
.nopfx    
    push    bc
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
    ex      af,af'                ; A = AscLen
    add     9                     ; AscLen += LinLen
    ld      (hl),0
    pop     bc
    ret
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
; Convert Binary RGB value to Decimal triplet
; Input: B: Delimiter
;        C: Entry Count
;       DE: Binary Data Address
;       HL: ASCII Buffer Address
; Output: A: ASCII String length
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
rgb_to_dec:
    push    hl                    ; Stack = BufAdr
    inc     c                     ; Bump for countdown
.loop
    dec     c
    jr      z,.done
    call    rgb_dec               ; Write red,grn,blu
    call    _crlf                 ; Write CR/LF
    jr      .loop
.done
    or      a                     ; Clear carry
    pop     de                    ; DE = BufAdr
    sbc     hl,de                 ; HL = StrLen
    ld      a,l                   ; A = StrLen
    ret

; Convert $GB0R to "red,green,blue"
; Input: B: Delmtr, DE: BinPtr, HL: BufPtr
rgb_dec:
    ld      a,(de)                ; A = GrnBlu
    inc     de                    ; Bump BinPtr
    push    af                    ; Stack = GrnBlu, RtnAdr
    ld      a,(de)                ; A = Red
    inc     de                    ; Bump BinPtr
    call    .to_dec               ; Convert and write to buffer
    pop     af                    ; A = GrnBlu; Stack = RtnAdr
    push    af                    ; Stack = GrnBlu, RtnAdr
    rra
    rra
    rra
    rra                           ; Fast A = A >> 4 
    call    .del_to_dec           ; Write delimiter and Green to buffer
    pop     af                    ; A = GrnBlu; Stack = RtnAdr
    call    .del_to_dec           ; Write delimiter and Blue to buffer
    xor     a                     ; A = 0, Clear carry
    ld      (hl),a                ; Null terminate string
    ret
    
.del_to_dec:
    ld      (hl),b
    inc     hl
.to_dec:
    push    bc                    ; Stack = Del+Cnt, RtnAdr
    push    de                    ; Stack = BinPtr, Del+Cnt, RtnAdr
    and     $0F                   ; Strip high nybble and clear carry
    ld      e,a                   ; E = Color
    rla
    rla
    rla
    rla                           ; A = A << 4
    add     e                     ; Made $x into $xx
    call    byte_to_dec_fast
    pop     de                    ; DE = BinPtr; Stack = Del+Cnt, RtnAdr
    pop     bc                    ; BC = Del+Cnt; Stack = RtnAdr
    ret

;; Input: A: Byte, HL: TxtPtr; Clobbered: AF, BC, DE
;; Does not generate leading spaces
;; ToDo: (after release) move to auxrom/misc.asm
byte_to_dec_fast:
    ld      e,'0'
    ld      d,100
    call    .digit
    ld      d,10
    call    .digit
    dec     e
    ld      d,1
.digit
    ld      b,-1
.loop
    inc     b
    sub     a,d
    jr      nc,.loop
    add     a,d
    ld      c,a
    ld      a,'0'
    add     b
    cp      e
    jr      z,.ret
    ld      (hl),a
    inc     hl
    ex      af,af'
    dec     e
.ret
    ld      a,c
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
read_nybbles:
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

;-----------------------------------------------------------------------------
; Convert decimal "rrr ggg bbb" to binary GB0R
; Input: A: Delimiter character
;        C: String Length
;       DE: String address
; Output: BC: RGB value
;         HL: String Pointer
; Flags: Carry set if invalid line
; Clobbered: A, DE
;-----------------------------------------------------------------------------
dec_to_rgb:
    ld      d,a                   ; D = Delimiter
    ld      e,c                   ; E = StrLen
    ld      bc,0                  ; RGB = $00
    push    bc                    ; Stack = RGB, RtnAdr
    call    dec_to_byte           ; A = Red
    pop     bc                    ; BC = RGB, Stack = RtnAdr
    ret     c                     ; Return Error if no red
    call    div_a_16              ; Red = Red / 16
    ld      b,a                   ; B = Red 
    call    _check_delimiter      ; If no Delimiter
    ret     c                     ;   Return Error
    push    bc                    ; Stack = RGB, RtnAdr
    call    dec_to_byte           ; A = Green
    pop     bc                    ; BC = RGB, Stack = RtnAdr
    ret     c                     ; Return if no blue
    and     $F0                   ; Green = Green / 16 (MSB)
    ld      c,a                   ; C = Green
    call    _check_delimiter      ; If no Delimiter
    ret     c                     ;   Return Error
    push    bc                    ; Stack = RGB, RtnAdr
    call    dec_to_byte           ; A = Blue
    pop     bc                    ; BC = RGB, Stack = RtnAdr
    ret     c                     ; Return if no green
    call    div_a_16              ; Blue = Blue / 16
    or      c                     ; A = Green + Blue
    ld      c,a                   ; C = Green + Blue
    ret     

; Input: D = DelChr
;        E = StrCnt
;       HL = StrPtr
; Output: HL = StrPtr
; Flags: Carry set if not delimiter
_check_delimiter:
    ld      a,e                   ; A = StrCnt
    or      a                     ; If End of String
    jr      z,_scf                ;   Return Error
    ld      a,(hl)                ; A = CurChr
    inc     hl                    ; Bump StrPtr (even if not DelChr)
    dec     e                     ; Decrement StrCnt
    cp      d                     ; If CurChr = DelChr
    ret     z                     ;   Return Carry Clear (from CP D)
_scf
    scf                           ; Return Carry Set
    ret     

; Input: E: StrCnt
;        HL: StrPtr
; Output: A, C: Byte
;         E:StrCnt
;        HL: StrPtr
; Flags: Carry set if not number
;   else Zero set if end of line
; Clobbers: BC
dec_to_byte:
    ld      a,e                   ; A = StrCnt
    or      a                     ; If StrCnt = 0
    jr      z,_scf                ;   Return error
    ld      c,0
    call    get_dec_digit         ; A = Digit
    ret     c                     ; Return if not digit
    ld      c,a                   ; Byte = 0
.loop
    call    get_dec_digit         ; A = Digit
    jr      c,.done               ; If not digit
    push    af                    ;   Stack = EolFlg, RtnAdr
    call    mult_c_10
    add     a,c                   ; Byte = Byte * 10 + Digit
    ret     c                     ; Return if overflow
    ld      c,a
    pop     af                    ; F = EolFlg; Stack = RtnAdr
    jr      nz,.loop              ; If not EOL, check next character
.done
    ld      a,c                   ; Return Byte
    or      a                     ; Carry Clear = Valid Byte
    ret

; Input: HL: TxtPtr
; Output: A: Byte
; Flags: Carry set if not digit;
;        Zero set if end of string
get_dec_digit:
    ld      a,(hl)                ; A = TxtChr
    sub     '0'                   ; A = Digit
    ret     c                     ; If Digit < 0 return not digit
    cp      10
    ccf                           ; If Digit > 9
    ret     c                     ;   Return not digit
    inc     hl                    ; Bump Pointer
    dec     e
    ret
div_a_16:
    sra     a                     ; A = A / 2
div_a_8:
    sra     a                     ; A = A / 4
div_a_4:
    sra     a                     ; A = A / 8
    sra     a                     ; A = A / 16
    ret

; Clobbers B
mult_c_10:
    ld      b,a                   ; Save A
    ld      a,c                   ; A = Num
    add     a                     ; A = Num * 2
    ret     c                     ; Return if overflow
    add     a                     ; A = Num * 4
    ret     c                     ; Return if overflow
    add     c                     ; A = Num * 5
    ret     c                     ; Return if overflow
    add     a,a                   ; A = Num * 10 (Carry set if overflow)
    ld      c,a                   ; C = Num * 10
    ld      a,b                   ; Restore A
    ret
    