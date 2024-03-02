;====================================================================
; Alternate Keyboard port routines
;====================================================================

;-----------------------------------------------------------------------------
; Clear alternate keyboard port FIFO buffer
; Cobbered: A
;-----------------------------------------------------------------------------
key_clear_fifo:
    xor       a
    out       (IO_KEYBUFF),a
    ret
    
;-----------------------------------------------------------------------------
; Read ASCII key from alternate keyboard port
; Output: A: Key ASCII value (0 = no key pressed)
;-----------------------------------------------------------------------------
key_read_ascii:
    in        a,(IO_KEYBUFF)
    or        a
    ret
    
;-----------------------------------------------------------------------------
; Read scan code from alternate keyboard port
; Output: A: Key ASCII value (0 = no key pressed)
;-----------------------------------------------------------------------------
key_read_scancode:
    ret
    
;-----------------------------------------------------------------------------
; Set keyboard mode
;  Input: A: Mode (KB_ENABLE | KB_SCANCODE | KB_REPEAT) 
; Output: A: 0 if succesful, else error code
;-----------------------------------------------------------------------------
key_set_keymode:
    jp      esp_set_keymode

;-----------------------------------------------------------------------------
; Check if key is pressed
;  Input: A: Key matrix code
; Output: A: -1 if pressed, else 0
;-----------------------------------------------------------------------------
key_pressed:
    cp      64
    ccf                           ; If KeyCode > 63
    ret     c                     ;   Return Carry set
    push    af                    ; Stack = KeyCode, RtnAdr
    and     $07                   ; Isolate row number
    call    _bitmask              ; Get row bitmask
    ld      e,a                   ; E = RowMsk
    pop     af                    ; A = KeyCode; Stack = RtnAdr
    and     $38                   
    rra                           ; Isolate column number
    rra
    rra       
    call    _bitmask              ; Get column bitmask
    cpl                           ; Invert it
    ld      b,a                   ; B = ColMsk
    ld      c,IO_KEYBOARD
    in      a,(c)                 ; Read key matrix
    cpl                           ; Invert result
    and     e                     ; Isolate row bit
    ret     z                     ; Return 0 if not set
    or      -1                    ; Else return -1 with flags set
    ret
_bitmask
    ld      b,a
    inc     b
    xor     a
    ccf
.loop
    rla
    djnz    .loop
    ret

