\;=============================================================================
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
; Write one or more entries to palette
; Input: A: Entry#               VPALSEL   ~PPEEEEE : P = Palette#, E = Entry#
;       BC: Data Length          VPALDATA  GGGGBBBB : G = Green, B = Blue 
;       DE: Data Address                   ~~~~RRRR : R = Red
;        L: Palette# (shifted if Carry Set)
;           
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
;;; ToDo: Add SET PALETTE and GETPALETTE$ tests to gr,bas (rename to rg.bas?)
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
; Read entire palette
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
; Read single palette entry
; Input: A: Palette#
;        E: Ebtry Index
; Output: DE: Palette Entry
; Clobbered: A,BC
;-----------------------------------------------------------------------------
palette_get_entry:
    call    mult_a_32             ; A = Shifted palette #
    sla     e                     ; E = Entry Offset
    add     e
    ld      b,a                   ; B = Register Index
    ld      c,IO_VPALSEL
    out     (c),b                 ; Select Index
    in      a,(IO_VPALDATA)       ; Read Green+Blue byte
    ld      e,a                   ; E = GB
    inc     b                     ; 
    out     (c),b                 ; Select Next Index
    in      a,(IO_VPALDATA)       ; Read Red byte
    ld      d,a                   ; DE = RGB
    ret

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
