;====================================================================
; Miscellaneous Assembly Calls used by BASIC
;====================================================================

;-----------------------------------------------------------------------------
; Return bit status in 23 bit integer
; Input: A: Bit# (0 - 23)
;      CDE: 24 bit Long
; Output: A: -1 if set, 0 if not set
; Flags: Carry set if A > 23
; Clobbered: BC, DE
;-----------------------------------------------------------------------------
bool_checkbit_long:
    ld      b,a                   ; B = BitNo
    ld      a,23
    cp      b                     ; If BitNo > 23
    ret     c                     ;   Return Carry Set
    inc     b                     ; Bump BitNo for Loop
    xor     a                     ; Result = 0
.loop
    srl     c
    rr      d
    rr      e                     ; Carry = Next Bit
    djnz    .loop                 ; Countdown and loop
    ccf                           ; Carry = ! Bit
    adc     $FF                   ; Convert bit to 0/$FF
    or      a                     ; Clear carry
    ret

;-----------------------------------------------------------------------------
; Return bit status in string
; Input: BC: string length (0 to 255)
;        DE: string address
;        HL: bit# (0 - 2039)
; Output: A: -1 if set, 0 if not set
; Flags: Carry set if HL > 2039
; Clobbered: AF',BC,DE,HL
;-----------------------------------------------------------------------------
bool_checkbit_string:
    push    de                    ; Stack = StrAdr, RtnAdr
    ld      de,2040
    rst     COMPAR
    ccf
    pop     de                    ; DE = StrAdr; Stack = RtnAdr
    ret     c                     ; If BitNo > 2039, return Carry Set
    ld      a,l
    and     $07                   ; A = ChrBit
    push    af                    ; Stack = ChrBit, RtnAdr
    ld      b,5
    call    shift_hl_left         ; H = ChrPos
    ld      a,h
    cp      c                     ; If ChrPos > StrLen
    ret     c                     ;   Return Carry Set
    ld      l,a
    ld      h,0
    add     hl,de                 ; HL = ChrAdr
    pop     bc                    ; B = ChrBit
    inc     b                     ; Bump BitNo for Loop
    ld      a,(hl)                ; A = StrChr
.loop
    rra
    djnz    .loop
    ccf                           ; Carry = ! Bit
    ld      a,0
    adc     $FF                   ; Convert bit to 0/$FF
    or      a                     ; Clear carry
    ret
