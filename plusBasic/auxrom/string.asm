;-----------------------------------------------------------------------------
; string.asm - string manipulation routines
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; Trim whitespace characters from both ends of string
; Input: A: trim character, 0 = whitespace
;       BC: string length
;       DE: string address
; Output: C: trimmed length
;        DE: trimmed address
; Clobbered: HL
;-----------------------------------------------------------------------------
string_trim:
    push    af
    call    string_trim_right
    pop     af
;-----------------------------------------------------------------------------
; Trim whitespace characters from left end of string
; Input: B: trim chars length
;        C: string length
;       DE: trim chars address
;       HL: string address
; Output: C: trimmed length
;        HL: trimmed address
;-----------------------------------------------------------------------------
string_trim_left:
    ld      a,c
    or      a                     ; If null string
    ret     z                     ;   Return
    ld      a,b
    or      a
    jr      z,.whitespace          ; If trim chars
.trim   
    push    de                    ; Stack = TrmLst, RtnAdr      
    push    bc                    ; Stack = TrmLen, TrmLst, RtnAdr
.loop    
    ld      a,(de)
    cp      (hl)
    jr      z,.next
    inc     de
    djnz    .loop
    pop     bc                    ; BC = TrmLen; Stack = TrmLst, RtnAdr
    pop     de                    ; DE = TrmLst; Stack = RtnAdr
    ret
.next
    inc     hl
    pop     bc                    ; BC = TrmLen; Stack = TrmLst, RtnAdr
    pop     de                    ; DE = TrmLst; Stack = RtnAdr
    dec     c
    ret     z
    jp      .trim
.whitespace
    ld      a,(hl)
    cp      ' '+1
    ret     nc
    inc     hl
    dec     c
    jr      nz,.whitespace
    ret

;-----------------------------------------------------------------------------
; Trim whitespace characters from right end of string
; Input: B: trim chars length
;        C: string length
;       DE: trim chars address
;       HL: string address
; Output: C: trimmed length
;        HL: trimmed address
;-----------------------------------------------------------------------------
string_trim_right:
    ld      a,c
    or      a                     ; If null string
    ret     z                     ;   Return
    push    hl                    ; Stack = StrAdr, RetAdr
    push    hl                    ; Stack = StrAdr, StrAdr, RetAdr
    ld      hl,POPHRT             
    ex      (sp),hl               ; HL = StrAdr; Stack = POPHRT, StrAdr, RetAdr
    push    bc                    ; Stack = StrTrmLen, POPHRT, StrAdr, RetAdr
    ld      b,0
    add     hl,bc                 ; HL = StrPtr (byte after end)
    pop     bc                    ; Stack = POPHRT, StrAdr, RetAdr
    ld      a,b
    or      a
    jr      z,.whitespace         ; If trim chars
.trim   
    push    de                    ; Stack = TrmLst, POPHRT, StrAdr, RtnAdr      
    push    bc                    ; Stack = TrmLen, TrmLst, POPHRT, StrAdr, RtnAdr
    dec     hl                    ; Back up StrPtr
.loop    
    ld      a,(de)
    cp      (hl)
    jr      z,.next
    inc     de
    djnz    .loop
    pop     bc                    ; BC = TrmLen; Stack = TrmLst, POPHRT, StrAdr, RtnAdr
    pop     de                    ; DE = TrmLst; Stack = POPHRT, StrAdr, RtnAdr
    ret
.next
    pop     bc                    ; BC = TrmLen; Stack = TrmLst, POPHRT, StrAdr, RtnAdr
    pop     de                    ; DE = TrmLst; Stack = POPHRT, StrAdr, RtnAdr
    dec     c
    ret     z
    jp      .trim
.whitespace
    dec     hl                      ; Back up to previous character
    ld      a,(hl)
    cp      ' '+1
    ret     nc
    dec     c
    jr      nz,.whitespace
    ret
