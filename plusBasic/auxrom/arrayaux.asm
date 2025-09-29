;=====================================================================================
; Array Auxillary ROM Routines
;=====================================================================================

; ------------------------------------------------------------------------------
; Print String Array contents to Screen with LIST style pausing
; Input: A = Array Type
;       BC = Array Data Address
;       DE = Array Data Length
; ------------------------------------------------------------------------------
list_string_array:
    jp      nz,TMERR              ; If Not string array, Typoe Mismatch
    ld      a,23                  ; Set line count to 23
    ld      (CNTOFL),a            
    push    hl                    ; Stack = TxtPtr, RtnAdr
    ex      de,hl                 ; HL = DatAdr
.loop
    ld      a,b
    or      c                     ; If DatLen = 9
    jp      z,POPHRT              ;   Restore Text Pointer and Return
    push    bc                    ; Stack = DatLen, TxtPtr, RtnAdr
    ld      e,(hl)                ; E = StrLen
    inc     hl
    inc     hl
    ld      c,(hl)
    inc     hl
    ld      b,(hl)                ; BC = StrAdr
    inc     hl
    call    STRPRD                ; Print the String
    call    CRDO
    pop     bc                    ; BC = DatLen; Stack = TxtPtr, RtnAdr
    dec     bc                    ; Count down
    dec     bc
    dec     bc
    dec     bc
    jr      .loop                 ; Do next element
