;=====================================================================================
; Array Auxillary ROM Routines
;=====================================================================================

; Input: A: Type, DE: Array Start, BC = Array Length
clear_array:
    call    GETYPE                ; A = AryTyp
    push    hl                    ; Stack = TxrPtr, RtnAdr
    push    de                    ; Stack = AryAdr, TxtPtr, RtnAdr
    push    bc                    ; Stack = AryLen, AryAdr, TxtPtr, RtnAdr
    push    af                    ; Stack = AryTyp, AryLen, AryAdr, TxtPtr, RtnAdr
    ex      de,hl                 ; HL = AryAdr
    call    sys_fill_zero         ; Fill array data with 0
    pop     af                    ; AF = AryTyp; Stack = AryLen, AryAdr, TxtPtr, RtnAdr
    call    z,GARBA2              ; If string, do garbage collection
    pop     bc                    ; BC = AryLen; Stack = AryLen, AryAdr, TxtPtr, RtnAdr
    pop     de                    ; DE = AryAdr; Stack = AryAdr, TxtPtr, RtnAdr
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret

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
