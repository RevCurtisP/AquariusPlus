;-----------------------------------------------------------------------------
; esp.asm - ESP32 routines
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; Issue command to ESP
;-----------------------------------------------------------------------------
esp_cmd:
    push    a

    ; Drain RX FIFO
.drain:
    in      a, (IO_ESPCTRL)
    and     a, 1
    jr      z, .done
    in      a, (IO_ESPDATA)
    jr      .drain
.done:

    ; Issue start of command
    ld      a, $80
    out     (IO_ESPCTRL), a

    ; Issue command
    pop     a
    jp      esp_send_byte


;-----------------------------------------------------------------------------
; Issue command to ESP and Send string with Descriptor in HL
; Input:   A: Command
;         HL: String Descriptor
; Output:  A: Result
;         DE: Address of Byte after String
;         BC: String Length
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------

esp_cmd_strdesc:
    call    esp_cmd
    call    esp_send_strdesc

;-----------------------------------------------------------------------------
; Open file to String Descriptor in HL
;
; Clobbered registers: A, HL, DE
;-----------------------------------------------------------------------------
esp_open:
    ld      a, ESPCMD_OPEN
    call    esp_cmd
    ld      a, FO_RDONLY
    call    esp_send_byte
    call    esp_send_strdesc
    jp      esp_get_result

;-----------------------------------------------------------------------------
; Get first result byte, and jump to error handler if it was an error
;-----------------------------------------------------------------------------
esp_get_result:
    call    esp_get_byte
    or      a
    jp      m, esp_error
    ret

;-----------------------------------------------------------------------------
; Read 32-bit long from ESP32 into BC,DE
;-----------------------------------------------------------------------------

esp_get_long:
    call    esp_get_word
    ld      c,e
    ld      b,d

;-----------------------------------------------------------------------------
; Read 16-bit word from ESP32 into DE
; Returns with MSB in A
;-----------------------------------------------------------------------------

esp_get_word:
    call    esp_get_byte       ; Read LSB
    ld      e,a                   ; into C
    call    esp_get_byte       ; Read MSB
    ld      d,a                   ; into B
    ret

;-----------------------------------------------------------------------------
; Wait for data from ESP
;-----------------------------------------------------------------------------
esp_get_byte:
.wait:
    in      a, (IO_ESPCTRL)
    and     a, 1
    jr      z, .wait
    in      a, (IO_ESPDATA)
    ret

;-----------------------------------------------------------------------------
; Write data to ESP
;------------b-----------------------------------------------------------------
esp_send_byte:
    push    a

.wait:
    in      a, (IO_ESPCTRL)
    and     a, 2
    jr      nz, .wait

    pop     a
    out     (IO_ESPDATA), a
    ret


;-----------------------------------------------------------------------------
; Send String from String Descriptor
; Input:    HL: String Descriptor Address
; Output:   DE: Address of Byte after String
;           BC: String Length
; Destroys: HL
;-----------------------------------------------------------------------------

esp_send_strdesc: 
    ld      a,h                   ; If HL is 0
    or      l
    jp      z,esp_send_byte       ; Send Null Terminator
    call    STRADL                ; Get Text Address in DE and Length in BC

;-----------------------------------------------------------------------------
; Send String
; Input:  DE: String Address
;         BC: String Length
; Output: DE: Address of Byte after String
;         BC: String Length
;-----------------------------------------------------------------------------

esp_send_string: 
    call    new_send_bytes        ; Send Command String  
    xor     a 
    jp      esp_send_byte         ; Send String Terminator

;-----------------------------------------------------------------------------
; Send bytes
; Input:  DE: source address
;         BC: number of bytes to write
; Output: DE: next address
;         BC: number of bytes actually written
;-----------------------------------------------------------------------------
new_send_bytes:
    push    bc

.loop:
    ; Done sending? (DE=0)
    ld      a, b
    or      a, c
    jr      z, .done

    ld      a, (de)
    call    esp_send_byte
    inc     de
    dec     bc
    jr      .loop

.done:
    pop     bc
    ret
    
