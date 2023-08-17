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

esp_cmd_string:
    call    esp_cmd
    jp      esp_send_string

;-----------------------------------------------------------------------------
; Issue command to ESP and Send string with Descriptor in HL
; Input:   A: Command
;         HL: String Descriptor
; Output:  A: Result
;         DE: Address of Byte after String
;         BC: String Length
;-----------------------------------------------------------------------------

esp_cmd_strdesc:
    call    esp_cmd
    call    esp_send_strdesc

;-----------------------------------------------------------------------------
; Get first result byte, and jump to error handler if it was an error
;-----------------------------------------------------------------------------
esp_get_result:
    call    esp_get_byte
    or      a
    jp      m, esp_error
    ret

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
; Close any open file/directory descriptor
;
; Clobbered registers: A
;-----------------------------------------------------------------------------
esp_close:
    ld      a, ESPCMD_CLOSEALL
    call    esp_cmd
    jp      esp_get_result


;-----------------------------------------------------------------------------
; Close any open file/directory descriptor
;
; Clobbered registers: A
;-----------------------------------------------------------------------------
esp_close_all:
    ld      a, ESPCMD_CLOSEALL
    call    esp_cmd
    jp      esp_get_result

;-----------------------------------------------------------------------------
; Create file from string descriptor in HL
;-----------------------------------------------------------------------------
esp_create:
    ld      a, ESPCMD_OPEN
    call    esp_cmd
    ld      a, FO_WRONLY | FO_CREATE | FO_TRUNC
    call    esp_send_byte
    call    esp_send_strdesc
    jp      esp_get_result


;-----------------------------------------------------------------------------
; Read 32-bit long from ESP32 into BC,DE
;-----------------------------------------------------------------------------

esp_get_long:
    call    esp_get_word
    ld      c,e
    ld      b,d

;-----------------------------------------------------------------------------t
; Read 16-bit word from ESP32 into DE
; Returns with MSB in A
;-----------------------------------------------------------------------------

esp_get_word:
    call    esp_get_byte       ; Read LSB
    ld      e,a                   ; into E
    call    esp_get_byte       ; Read MSB
    ld      d,a                   ; into D
    ret

;-----------------------------------------------------------------------------
; Wait for data from ESP
; Time Out = 1500 milliseconds - (43 + 82 * 65536) / 3.579545 microseconds
;-----------------------------------------------------------------------------
esp_get_byte:
    push    bc                    ;+11	
                                  	
.wait:                            
    dec     bc                    ;+6	
    ld      a,b                   ;+4	
    or      c                     ;+4	
    jr      z,.timeout            ;+7+5
    in      a, (IO_ESPCTRL)       ;+11	
    and     a, 1                  ;+7	
    jr      z, .wait              ;+7+5
    in      a, (IO_ESPDATA)       ;+11	
    pop     bc                    ;+10	
    ret                           ;+10	
.timeout                          
    ld      a,-9                  ;+7	
    pop     bc                    ;+10	
    ret                           ;+10	
;-----------------------------------------------------------------------------
; Read bytes
; Input:  HL: destination address
;         DE: number of bytes to read
; Output: HL: next address (start address if no bytes read)
;         DE: number of bytes actually read
;
; Clobbered registers: A, HL, DE
;-----------------------------------------------------------------------------
esp_read_bytes:
    ld      a, ESPCMD_READ
    call    esp_cmd

    ; Send file descriptor
    xor     a
    call    esp_send_byte

    ; Send read size
    call    esp_send_word
    
    ; Get result
    call    esp_get_result

    ; Get number of bytes actual read
    call    esp_get_word

    push    de

.loop:
    ; Done reading? (DE=0)
    ld      a, d
    or      a, e
    jr      z, .done

    call    esp_get_byte
    ld      (hl), a
    inc     hl
    dec     de
    jr      .loop

.done:
    pop     de
    ret

;-----------------------------------------------------------------------------
; esp_get_datetime - Read Date and Time String BASIC string buffer
; Assumes string_buff is on a 256 byte boundary
; Sets: string_buff: Date and Time in format YYYYMMDDHHmmss
; Output:  E: String Length, DE = End of String, HL = Buffer Address
;-----------------------------------------------------------------------------
esp_get_datetime:
    ld      a,ESPCMD_DATETIME     ; Issue CWD command
    call    esp_cmd
    call    esp_get_result
    ret     m                     ; Return if Error
    xor     a     
    call    esp_send_byte         ; Response Type ($00)
    ret     m                     ; Fall into esp_read_to_buff

;-----------------------------------------------------------------------------
; esp_read_to_buff - Read String from ESP to BASIC string buffer
; Assumes string_buff is on a 256 byte boundary
; Sets: string_buff: Current Directory
; Output:  E: String Length, DE = End of String, HL = Buffer Address
;-----------------------------------------------------------------------------
esp_read_to_buff:
    push    af                    ; Save A
    ld      b,255                 ; Maximum Length, Length Counter
    ld      hl,string_buff        ; BASIC String BUFFER
    ld      d,h
    ld      e,l
.loop
    call    esp_get_byte          ; Get character
    jp      m,.error              ; If Error, terminate string and return
    ld      (de),a                ; Store in Buffer
    inc     de
    or      a
    jr      z,.done               ; Return if end of String
    djnz    .loop     
.error
    xor     a
    ld      (de),a                ; Add Null Terminator
.done
    ld      a,e
    ld      (buff_strlen),a
    pop     af                    ; Restore Result
    ret

;-----------------------------------------------------------------------------
; Write 32-bit long in BC,DE to ESP32
; Returns with MSB in A
;-----------------------------------------------------------------------------
esp_send_long:
    ld      a,c
    call    esp_send_byte
    ld      a,b
    call    esp_send_byte

;-----------------------------------------------------------------------------
; Write 16-bit word in DE to ESP32
; Returns with MSB in A
;-----------------------------------------------------------------------------
esp_send_word:
    ld      a,e
    call    esp_send_byte
    ld      a,d

;-----------------------------------------------------------------------------
; Write data to ESP
;-----------------------------------------------------------------------------
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
; Output: HL: Address of Byte after String
;         DE: Bytes Written
;         BC: String Length
; Destroys: HL
;-----------------------------------------------------------------------------
esp_send_string: 
    ex      de,hl                 ; Address into HL
    ld      d,b                   ; Length into DE
    ld      e,c
    call    esp_send_bytes        ; Send Command String  
    xor     a 
    jp      esp_send_byte         ; Send String Terminator

;-----------------------------------------------------------------------------
; Send bytes
; Input:  HL: source address
;         DE: number of bytes to write
; Output: HL: next address
;         DE: number of bytes actually written
;-----------------------------------------------------------------------------
esp_send_bytes:
    push    de

.loop:
    ; Done sending? (DE=0)
    ld      a, d
    or      a, e
    jr      z, .done

    ld      a, (hl)
    call    esp_send_byte
    inc     hl
    dec     de
    jr      .loop

.done:
    pop     de
    ret

;-----------------------------------------------------------------------------
; Write bytes
; Input:  HL: source address
;         DE: number of bytes to write
; Output: HL: next address
;         DE: number of bytes actually written
;
; Clobbered registers: A, HL, DE
;-----------------------------------------------------------------------------
esp_write_bytes:
    ld      a, ESPCMD_WRITE
    call    esp_cmd

    ; Send file descriptor
    xor     a
    call    esp_send_byte

    ; Send write size
    call    esp_send_word

    ; Send bytes
    call    esp_send_bytes

    ; Get result
    call    esp_get_result

    ; Get number of bytes actual written
    call    esp_get_word
    ld      e, a

    ret

;-----------------------------------------------------------------------------
; Seek
; Input:  BC = Offset low 16 bits
;         DE = Offset high 16 bits
; Clobbered registers: A, DE
;-----------------------------------------------------------------------------
esp_seek:
    ld      a, ESPCMD_SEEK
    call    esp_cmd

    ; Send file descriptor
    xor     a
    call    esp_send_byte

    ; Send offset
    call    esp_send_long

    ; Get result
    call    esp_get_result
    ret

;-----------------------------------------------------------------------------
; esp_error
;-----------------------------------------------------------------------------
esp_error:
    neg
    dec     a
    cp      -ERR_TIMEOUT
    jr      c, .ok
    ld      a, -ERR_OTHER - 1

.ok:
    ld      hl, .error_msgs
    add     a,a
    add     l
    ld      l, a
    ld      a, h
    adc     a, 0
    ld      h, a
    ld      a, (hl)
    inc     hl
    ld      h, (hl)
    ld      l, a

    ; Print error message
    ld      a, '?'
    rst     OUTCHR
    jp      ERRFN1

.error_msgs:
    dw .msg_err_not_found     ; -1: File / directory not found
    dw .msg_err_too_many_open ; -2: Too many open files / directories
    dw .msg_err_param         ; -3: Invalid parameter
    dw .msg_err_eof           ; -4: End of file / directory
    dw .msg_err_exists        ; -5: File already exists
    dw .msg_err_other         ; -6: Other error
    dw .msg_err_no_disk       ; -7: No disk
    dw .msg_err_not_empty     ; -8: Not empty
    dw .msg_err_timeout       ; -9: ESP32 timed out

.msg_err_not_found:     db "Not found",0
.msg_err_too_many_open: db "Too many open",0
.msg_err_param:         db "Invalid param",0
.msg_err_eof:           db "EOF",0
.msg_err_exists:        db "Already exists",0
.msg_err_other:         db "Unknown error",0
.msg_err_no_disk:       db "No disk",0
.msg_err_not_empty:     db "Not empty",0
.msg_err_timeout:       db "ESP32 timed out",0

;-----------------------------------------------------------------------------
; Bad file error
;-----------------------------------------------------------------------------
err_bad_file:
    ld      hl, .msg_bad_file

    ; Print error message
    ld      a, '?'
    rst     OUTCHR
    jp      ERRFN1

.msg_bad_file:       db "Bad file",0

