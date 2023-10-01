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
; Issue ESP command with string argument
; Input:  A: Command
;        DE: String Address
;        BC: String Length
; Output: A: Result
;        DE: Address of Byte after String
;        BC: String Length
;-----------------------------------------------------------------------------

esp_cmd_string:
    call    esp_cmd
    call    esp_send_string

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
; Close file or directory
; Clobbered registers: A
;-----------------------------------------------------------------------------
esp_close:
    ld      a, ESPCMD_CLOSE
    call    esp_cmd
    jp      esp_get_result

;-----------------------------------------------------------------------------
; Close all files and directories
; Input: A: File Descriptor
; Output: A: Result 
;-----------------------------------------------------------------------------
esp_close_all:
    ld      a, ESPCMD_CLOSEALL
    call    esp_cmd
    jp      esp_get_result

;-----------------------------------------------------------------------------
; esp_read_to_buff - Read bytes from ESP to string buffer
; Input: HL: Address of String Buffer
; Output: E: String Length, 
;        DE: Address of Terminator
;        HL: Buffer Address
; Clobbers: B
;-----------------------------------------------------------------------------
esp_read_to_buff:
    push    af                    ; Save A
    ld      b,255                 ; Maximum Length, Length Counter
    ld      d,h
    ld      e,l
.loop
    call    esp_get_byte          ; Get character
    jp      m,.error              ; If Error, terminate string and return
    ld      (de),a                ; Store in Buffer
    or      a
    jr      z,.done               ; Return if end of String
    inc     de
    djnz    .loop     
.error
    xor     a
    ld      (de),a                ; Add Null Terminator
.done
    ex      de,hl                 ; HL = Terminator Address, DE = Buffer Address
    sbc     hl,de                 ; HL = Length
    ex      de,hl                 ; DE = Length, HL = Buffer Address
    pop     af                    ; Restore Result
    ret

;-----------------------------------------------------------------------------
; Read bytes from ESP to main memory
; Input:  BC: number of bytes to read
;         DE: destination address
; Output: BC: number of bytes actually read
;      DE,HL: next address (start address if no bytes read)
;         
; Clobbered registers: A
;-----------------------------------------------------------------------------
esp_read_bytes:
    ld      a, ESPCMD_READ
    call    esp_cmd

    ; Send file descriptor
    xor     a
    call    esp_send_byte

    ; Send read size
    call    esp_send_bc
    
    ; Get result
    call    esp_get_result

    ; Get number of bytes actual read
    call    esp_get_bc

    push    bc

.loop:
    ; Done reading? (BC=0)
    ld      a, b
    or      a, c
    jr      z, .done

    call    esp_get_byte
    ld      (de), a
    inc     de
    dec     bc
    jr      .loop

.done:
    ld      h,d                   ; Return end address in DE and HL
    ld      l,e
    pop     bc
    ret


;-----------------------------------------------------------------------------
; Read bytes from ESP to paged memory
; Input:   A: page
;         BC: number of bytes to read
;         DE: destination address
; Output: BC: number of bytes actually read
;      DE,HL: next address (start address if no bytes read)
;         
; Clobbered registers: A
;-----------------------------------------------------------------------------
esp_read_paged:
    call    page_set4write_coerce
    jp      z,page_restore_plus  ; Return if illegal page

    ld      a, ESPCMD_READ
    call    esp_cmd

    ; Send file descriptor
    xor     a
    call    esp_send_byte

    ; Send read size
    call    esp_send_bc
    
    ; Get result
    call    esp_get_result

    ; Get number of bytes actual read
    call    esp_get_bc

    push    bc

    dec     de
.loop:
    ; Done reading? (BC=0)
    ld      a, b
    or      a, c
    jr      z, .done

    inc     de
    ld      a,d
    or      a,e
    jr      nz,.read_byte
    call    page_next_address
    jr      c,.error              ; Return if overflow
.read_byte
    call    esp_get_byte
    ld      (de), a
    dec     bc
    jr      .loop

.done:
    
    ld      h,a
    or      $FF                   ; Clear zero and carry flags
    ld      a,h
    ld      h,d                   ; Return end address in DE and HL
    ld      l,e
.error
    pop     bc
    jp      page_restore_plus     ; Restore original page


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
; Returns with MSB (D) in A
;-----------------------------------------------------------------------------

esp_get_long:
    call    esp_get_bc

;-----------------------------------------------------------------------------t
; Read word from ESP32 into DE
; Returns with MSB in A
;-----------------------------------------------------------------------------

esp_get_de:
    call    esp_get_byte       ; Read LSB
    ld      e,a                   ; into E
    call    esp_get_byte       ; Read MSB
    ld      d,a                ; into D
    ret

;-----------------------------------------------------------------------------t
; Read word from ESP32 into BC
; Returns with MSB in A
;-----------------------------------------------------------------------------

esp_get_bc:
    call    esp_get_byte          ; Read LSB
    ld      c,a                   ; into C
    call    esp_get_byte          ; Read MSB
    ld      b,a                   ; into B
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
    call    string_addr_len       ; Get Text Address in DE and Length in BC

;-----------------------------------------------------------------------------
; Send String
; Input:  DE: String Address
;         BC: String Length
; Output: DE: Address of Byte after String
;         BC: Bytes Written
; Destroys: HL
;-----------------------------------------------------------------------------

esp_send_string: 
    call    esp_send_bytes        ; Send Command String  
    xor     a 
    jp      esp_send_byte         ; Send String Terminator

;-----------------------------------------------------------------------------
; Write bytes from main memory
; Input:  DE: source address
;         BC: number of bytes to write
; Output: DE: next address
;         BC: number of bytes actually written
; Clobbered registers: A, HL
;-----------------------------------------------------------------------------
esp_write_bytes:
    ld      a, ESPCMD_WRITE
    call    esp_cmd

    ; Send file descriptor
    xor     a
    call    esp_send_byte

    ; Send write size
    call    esp_send_bc

    ; Send bytes
    call    esp_send_bytes

    ; Get result
    call    esp_get_result

    ; Get number of bytes actual written
    call    esp_get_bc

    ret

;-----------------------------------------------------------------------------
; Write byte repeatedly
; Input:  E: byte to write
;         BC: number of times to write write it
; Output: BC: number of bytes actually written
; Clobbered registers: A, HL
;-----------------------------------------------------------------------------
esp_write_repbyte:
    ld      a, ESPCMD_WRITE
    call    esp_cmd

    ; Send file descriptor
    xor     a
    call    esp_send_byte

    ; Send write size
    call    esp_send_bc

    ; Send bytes
    
.loop:
    ; Done sending? (BC=0)
    ld      a, b
    or      a, c
    jr      z, .done

    ld      a, e
    call    esp_send_byte
    dec     bc
    jr      .loop

.done:
    ; Get result
    call    esp_get_result

    ; Get number of bytes actual written
    call    esp_get_bc

    ret

;-----------------------------------------------------------------------------
; Send bytes from main memory
; Input:  DE: source address
;         BC: number of bytes to write
; Output: DE: next address
;         BC: number of bytes actually written
;-----------------------------------------------------------------------------
esp_send_bytes:
    push    bc

.loop:
    ; Done sending? (BC=0)
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

;-----------------------------------------------------------------------------
; Write bytes from paged memory
; Input:   A: page
;         DE: source address
;         BC: number of bytes to write
; Output: DE: next address
;         BC: number of bytes actually written
;
; Clobbered registers: A, HL, DE
;-----------------------------------------------------------------------------
esp_write_paged:
    call    page_set4read_coerce
    jp      z,page_restore_plus  ; Return if illegal page
    ld      a, ESPCMD_WRITE
    call    esp_cmd

    ; Send file descriptor
    xor     a
    call    esp_send_byte

    ; Send write size
    call    esp_send_bc

    ; Send bytes

.loop:
    ; Done sending? (BC=0)
    ld      a, b
    or      a, c
    jr      z, .done

    ld      a, (de)
    call    esp_send_byte
    inc     de
    ld      a,d
    or      a,e
    jr      nz,.not_end
    call    page_next_address
    jr      c,.error              ; Return if overflow
.not_end
    dec     bc
    jr      .loop

.done:

    ; Get result
    call    esp_get_result

    ; Get number of bytes actual written
    call    esp_get_bc

    ld      h,a
    or      $FF                   ; Clear zero and carry flags
    ld      a,h
.error
    jp      page_restore_plus     ; Restore original page

;-----------------------------------------------------------------------------
; Write 16-bit word in BC to ESP32
; Returns with MSB in A
;-----------------------------------------------------------------------------
esp_send_bc:
    ld      a,c
    call    esp_send_byte
    ld      a,b
    jr      esp_send_byte

;-----------------------------------------------------------------------------
; Write 32-bit long in BC,DE to ESP32
; Returns with MSB in A
;-----------------------------------------------------------------------------
esp_send_long:
    call    esp_send_bc
;-----------------------------------------------------------------------------
; Write 16-bit word in DE to ESP32
; Returns with MSB in A
;-----------------------------------------------------------------------------
esp_send_de:
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
; Move to position in open file
; Input:  BC = Offset low 16 bits
;         DE = Offset high 16 bits
; Clobbered registers: A
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
; Get current position in open file
; Output: BC = Offset low 16 bits
;         DE = Offset high 16 bits
; Clobbered registers: A
;-----------------------------------------------------------------------------
esp_tell:
    ret:

;-----------------------------------------------------------------------------
; esp_get_datetime - Read date and time into string buffer
; Input: HL = Buffer Address
; Sets: string_buff: Date and Time in format YYYYMMDDHHmmss
; Output:  E: String Length, DE = End of String, HL = Buffer Address
;-----------------------------------------------------------------------------
esp_get_datetime:
    ld      a,ESPCMD_DATETIME     ; Issue DATETIME command
    call    esp_cmd
    xor     a     
    call    esp_send_byte         ; Response Type ($00)
    call    esp_get_result
    ret     m                     
    jp      esp_read_to_buff      

;-----------------------------------------------------------------------------
; esp_get_mouse - Read date and time into string buffer
; Output:  A: 0 if succesful, else error code
;         BC: X-position
;          E: Button State      
;          D: Y-position
;-----------------------------------------------------------------------------
esp_get_mouse:
    ld      a,ESPCMD_GETMOUSE     ; Issue MOUSE command
    call    esp_cmd
    call    esp_get_byte
    or      a
    ret     m                     
    call    esp_get_long
    xor     a                     ; Return success
    ret
    
;-----------------------------------------------------------------------------
; esp_error
;-----------------------------------------------------------------------------
esp_error:
    push    af
    call    page_restore_plus     ; In case of paged LOAD or SAVE
    pop     af
    
    neg
    dec     a
    cp      -ERR_WRITE_PROTECT
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
    dw .msg_err_write_protect ; -9: Write protected

.msg_err_not_found:     db "Not found",0
.msg_err_too_many_open: db "Too many open",0
.msg_err_param:         db "Invalid param",0
.msg_err_eof:           db "EOF",0
.msg_err_exists:        db "Already exists",0
.msg_err_other:         db "Unknown error",0
.msg_err_no_disk:       db "No disk",0
.msg_err_not_empty:     db "Not empty",0
.msg_err_write_protect  db "Write protected",0

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

