;=====================================================================================
; esp_aux.asm - ESP32 routines in Aux ROM
;=====================================================================================

;-----------------------------------------------------------------------------
; Read CR, LF, CR/LF, or null terminated string
;  Input: A: File descriptor
;        BC: Maximum line length
;        HL: String buffer address
; Output: BC: String length
; Clobbered: A
;-----------------------------------------------------------------------------
esp_aux_read_line:
    push    af
    ld      a,ESPCMD_READLINE     
    call    esp_cmd
    pop     af
    call    esp_send_byte         ; Send file descriptor
    call    esp_send_bc           ; Send maximum line length
    call    esp_get_result 
    ret     m                     ; Return if error
    push    hl                    ; Stack = BufAdr, RtnAdr
    ld      bc,0                  ; Initialize length
.loop
    call    esp_get_byte          ; Read byte
    ld      (hl),a
    or      a
    jr      z,.done               ; If not null terminator
    inc     hl                    ;   Bump address
    inc     bc                    ;   Update length
    jr      .loop                 ;   Read next byte
.done
    pop     hl                    ; HL = BufAdr; Stack = RtnAdr
    ret
