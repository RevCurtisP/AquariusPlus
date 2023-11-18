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

